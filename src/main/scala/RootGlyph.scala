package org.sufrin.glyph
import GlyphTypes.Scalar


/**
 * A top-level glyph that keeps track of the details of the host window of
 * the GUIrootGUIroot; its geometry, etc. Wrapping a glyph in one of these
 * makes it possible to provide ``global'' services (such a popups)
 * from reactive glyphs without them needing to refer to a global object.
 *
 * These glyphs are inserted automatically when an `Interaction` is constructed
 * from a `Window` and the root of an application-specific glyph tree.
 *
 * //TODO: can we now dispense with the Envelope, since we have to implement most Glyph features here now anyway.
 */
class RootGlyph(var GUIroot: Glyph) extends Glyph { thisRoot =>
  import io.github.humbleui.jwm.{Event, EventKey, Screen, Window}
  import io.github.humbleui.jwm.App.runOnUIThread
  def copy(fg: Brush=this.fg, bg: Brush=this.bg) : Glyph = new RootGlyph(GUIroot)

  var ignoreResizes: Int = 0
  /**
   * Regenerate the `GUIroot` to be of size (no more than) `(newW, newH)`, if
   * it is `resizeable`. Otherwise the current Interaction's software scale
   * is changed uniformly to accomodate the new window size.
   *
   * The next mouse motion following the resize causes the window to take on
   * the dimensions of the new `GUIroot` -- this gets round the problem of
   * a resize leaving new content in the wrong-shaped window. It would have been
   * better if Skija told us the state of the mouse when it invokes the resize event,
   * for then we could delay the recomputation of the content until the window size
   * became stable.
   *
   * @param newW
   * @param newH
   */
  def resizeRoot(newW: Scalar, newH: Scalar): Unit = {
     if (GUIroot.resizeable) {
       ignoreResizes -= 1
       if (ignoreResizes<0) {
         io.github.humbleui.jwm.App.runOnUIThread { ()=>
           val newRoot = GUIroot.atSize(Vec(newW - 21f, newH - 21f)) // I HATE MAGIC: where do the deltas come from?
           if (newRoot ne GUIroot) {
             newRoot.parent = this
             GUIroot.parent = this
             diagonal = newRoot.diagonal
             onNextMotion { fixContentSize() }
           }
         }
       }
     }
     else {
       // Just change the global scale
       eventHandler.softwareScale = newW / w min newH / h
     }
  }

  def fixContentSize(): Unit = {
    ignoreResizes = 1
    rootWindow.setContentSize(diagonal.x.toInt, diagonal.y.toInt)
  }

  def setContentSize(diagonal: Vec): Unit = {
    println(s"contentSize:=$diagonal")
    rootWindow.setContentSize(diagonal.x.toInt, diagonal.y.toInt)
  }

  val fg: Brush = DefaultBrushes.invisible
  val bg: Brush = DefaultBrushes.invisible
  var diagonal: Vec = GUIroot.diagonal
  locally { GUIroot.parent = this }
  override def toString: String = s"RootGlyph\n\tdelegate = $GUIroot"
  /** The root Window of this interaction */
  var  rootWindow: Window = null

  /** The actual width of the window; not the proposed width of the glyph  */
  def W: Scalar = rootWindow.getContentRectAbsolute.getWidth.toFloat
  /** The actual height of the window; not the proposed height of the glyph  */
  def H: Scalar = rootWindow.getContentRectAbsolute.getHeight.toFloat


  /**
   * Redraw the main window associated with this (running) GUI.
   */
  override def reDraw(): Unit = rootWindow.requestFrame()

    //////////////////////////// Overlay considerations


  /**
   *  Overlay management.
   *
   *  Overlays are drawn after (over) the main GUI. There are two forms of overlay, both
   *  represented as `RootLayer` values and having associated (possibly reactive) glyphs.
   *
   *  1. Layers: these are managed as a stack of `RootLayer`, with higher layers drawn over lower layers.
   *
   *  2. Annotations: these are managed as a mapping from names to `RootLayer`s.
   *
   *
   */
  object Overlay {

    import scala.collection.mutable

    val annotations:  collection.mutable.Map[String, RootLayer] = new mutable.TreeMap[String, RootLayer]
    val layers:       collection.mutable.Stack[RootLayer] = new collection.mutable.Stack[RootLayer]
    def overlay:      Glyph    = layers.top.glyph
    def isModal:      Boolean  = layers.top.isModal
    def strictHiding: Boolean  = layers.top.strictHiding
    def visual: Boolean        = !layers.top.active

    /**  Push a new layer onto the overlay stack. */
    def pushLayer(glyph: Glyph, isModal: Boolean=false, isMenu: Boolean = false, strictHiding: Boolean=true, visible: Boolean=true, offMenuClick: () => Unit = { () => }): Unit = {
      layers.push(RootLayer(glyph, isModal, isMenu = isMenu, strictHiding=strictHiding, visible=visible, offMenuClick=offMenuClick))
    }

    /**  Add a new annotation (with identifier `id`) to the annotation mapping; and return it. */
    def newAnnotation(id: String, glyph: Glyph, isModal: Boolean = false, strictHiding: Boolean = true, visible: Boolean=true, active: Boolean=true): RootLayer = {
      val layer = RootLayer(glyph, isModal, strictHiding, visible=visible, active=active)
      glyph.parent = thisRoot
      annotations(id) = layer
      layer
    }

    /**  Replace the topmost overlay on the stack. */
    def set(glyph: Glyph, isModal: Boolean = false, strictHiding: Boolean = true): Unit = {
      if (layers.nonEmpty) layers.pop()
      layers.push(RootLayer(glyph, isModal, strictHiding))
    }

    /**
     * Remove the topmost, if any, overlay from the stack, then
     * if there are no more layers, give up keyboard and mouse focus
     * completely.
     */
    def pop(): Unit = {
      if (layers.nonEmpty) {
        layers.pop()
        reDraw()
      }
      if (layers.isEmpty) {
         giveupFocus()
         giveupFocus() // to clear recentFocus
      }
    }

    /** If the overlay stack is empty then `None`; else {{{Some(its topmost element)}}} */
    def top: Option[RootLayer] = { if (layers.nonEmpty) Some(layers.top) else None  }

    /**
     * Does the topmost overlay's bounding box overlap / completely overlap `underneath`s
     * bounding box? Overlapping is tested when the overlay's `strictHiding` is true; else
     * completely overlapping is tested.
     */
    @inline final def hides(underneath: Glyph): Boolean =  {
      val overLoc:  Vec  = overlay.location
      val overDiag: Vec  = overlay.diagonal
      val underDist: Vec = underneath.rootDistance
      if (strictHiding)
           (underDist.inside(overLoc, overDiag)) || ((underDist + underneath.diagonal).inside(overLoc, overDiag))
      else
           (underDist.inside(overLoc, overDiag)) && ((underDist + underneath.diagonal).inside(overLoc, overDiag))
    }
  }


  /*
   *  Locate `p` within a visible reactive glyph in a decoration, in the topmost overlay, or else within a
   *  *visible* glyph in the underlying `GUIroot`.
   *
   *  The intention is that
   * (a) A direct hover over a reactive glyph in an overlaydialogues will be picked up
   *     during a mouse-focus transfer -- whether or not there
   *     are reactive glyphs "hidden" by it.  //TODO: reconsider this?
   *
   * (b) `GUIroot` glyphs that are *completely covered* by the top overlay will not be selected during a mouse-focus
   *  transfer. Thus as far as a button is concerned,
   *  if you can't see it at all then you can't press it; but if you can see some of it
   *  then you can (unless strict hiding is enabled in the topmost overlay).
   *
   *  TODO: generalize from "top overlay" to "any overlay".
   *
   *  TODO: (see notebooks) a click ANYWHERE outside a running isModal/menu overlay
   *        should perhaps close that overlay. One would be to present a
   *        "dummy" hit with a dummy reactive if the search at ** finds `None`. That
   *        reactive would interpret (only) a button up and ignore everything else.
   *
   */

  override def glyphContaining(p: Vec): Option[Hit] = {
    decorationContaining(p) match {
      case None =>
        // Dialogue logic
        Overlay.layers.nonEmpty match {
          // There's no overlay
          case false => GUIroot.glyphContaining(p)

          // There are overlays
          case true =>
            import Overlay.{isModal, overlay}
            if (isModal)
              overlay.glyphContaining(p - overlay.location) //**
            else
              overlay.glyphContaining(p - overlay.location) match {
                // There's no hit in the overlay: so look underneath
                case None =>
                  GUIroot.glyphContaining(p) match {
                    // No hit underneath either
                    case None => None
                    // A hit underneath: should it be masked by the overlay
                    case aHit@Some(Hit(underneath, _)) =>
                      if (Overlay.hides(underneath)) None else aHit
                  }
                // There's a hit in the overlay: accept it
                case other => other
              }
        }
      case other => other
    }
  }

  /**
   *  Yield a hit, if possible, on a visible `Reactive` glyph that contains `p` within  a decoration layer
   */
def decorationContaining(p: Vec): Option[Hit] = {
  // select the visible, active decoration layers.
  val visibleActive = (Overlay.annotations.filter { case (id: String, layer: RootLayer) => layer.visible && layer.active })
  // get the hits from the visible, active decoration layers containing hits
  val visibleActiveHits = visibleActive.map { case (id: String, layer: RootLayer) => (layer.glyph.glyphContaining(p - layer.glyph.location)) }.filter(_.nonEmpty).map(_.get)
  // now thin these hits to those that are hits on reactives
  val visibleReactiveHits = visibleActiveHits.filter{ hit => hit.glyph.isReactive }
  // and if there is at least one, then choose the first.
  // TODO: Deal with ambiguities by some sort of priority -- it surely won't happen often
  //       because annotations are not really supposed to offer reactions.
  if (visibleReactiveHits.nonEmpty) Some(visibleReactiveHits.head) else None
}

  /**
   *  If there's a visible decoration containing a reactive glyph that contains `p` then
   *  return a hit on that glyph; else seek a hit in the `GUIroot`.
   */
override def reactiveContaining(p: Vec): Option[ReactiveGlyph] =
  decorationContaining(p) match {
    case None      => GUIroot.reactiveContaining(p)
    case Some(hit) => hit.glyph.reactiveContaining(p)
  }

override def reactiveParent: Option[ReactiveGlyph] = GUIroot.reactiveParent

/**
 * Draw the main glyph overlaid with the visible annotations and then
 * the overlay glyphs in last-pushed order.
 */
override def draw(surface: Surface): Unit = {
  GUIroot.drawBackground(surface)
  GUIroot.draw(surface)

  for {(id, layer)  <- Overlay.annotations if layer.visible} {
      surface.withOrigin(layer.glyph.location) {
        layer.glyph.draw(surface)
      }
    }

  for { layer <- Overlay.layers.reverseIterator if layer.visible } {
    surface.withOrigin(layer.glyph.location) {
      layer.glyph.draw(surface)
    }
  }
}
  /**
   *  Invoked by the eventHandler when there has been an off-glyph click.
   *  Informs the topmost (if any) overlay, if it is a menu
   */
  def offGlyphClick(): Unit = {
    Overlay.top match {
      case None =>
      case Some(rootLayer) =>
        if (rootLayer.isMenu) rootLayer.offMenuClick()
    }
  }

//////////////////////////////////////////////////////////////////////////////////


def windowOrigin: (Int, Int) = {
  val rect = rootWindow.getWindowRect
  val crect = rootWindow.getContentRect
  val (x, y) = (rect.getLeft, rect.getTop)
  //println(s"RootGlyph.windowOrigin=($x,$y) [$crect]")
  (x,y)
}

def onScreenSize(g: Glyph): (Int, Int) = eventHandler.onScreenSize(g)

def contentLocation: (Int, Int) = eventHandler.contentLocation

def logicalLocation(glyph: Glyph, offset: Vec): (Int, Int) = eventHandler.logicalLocation(glyph.rootDistance+offset)

def logicalLocation(offset: Vec): (Int, Int)               = eventHandler.logicalLocation(offset)

/** The EventHandler (if any) managing this Interaction */
var  eventHandler: EventHandler = null

def softwareScale:  Scalar = eventHandler.softwareScale
def currentScreen: Screen  = rootWindow.getScreen
def hardwareScale: Scalar  = rootWindow.getScreen.getScale

/** Extra arguments -- usually provided from the command line via `Interaction` */
val args: List[String] = Nil

override def isRoot = true

/**
 *  Invoke this if the cursor or the focus leaves the root window.
 *  Help for implementing "auto-popdown" of menu windows
 */

def onRootLeave(): Unit = {}
def onRootEnter(): Unit = {}

protected var _onCloseRequest: Option[Window => Unit] = None

def onCloseRequest(action: Window => Unit): Unit = _onCloseRequest = Some(action)

def windowCloseRequest(w: Window): Unit = {
    _onCloseRequest match {
      case None         => w.close()
      case Some(action) => action(w)
    }
}

var mouseInside: Option[Boolean] = None

var _onMotion: Option[()=>Unit] = None

def onNextMotion(action: => Unit): Unit = {
    _onMotion = Some {()=>action}
}

def onMotion(mouseLoc: Vec): Unit = {
  _onMotion match {
    case None => ()
    case Some(action) =>
      _onMotion = None
      action()
  }
}

def acceptRootGlyphEvent(event: RootGlyphEvent, window: Window, handler: EventHandler): Unit = {
  if (handler.logEvents) println(event)
  event match {
    case RootEnterEvent(window) =>
      onRootEnter()
    case RootLeaveEvent(window) =>
      onRootLeave()
  }
}

def acceptWindowEvent(event: Event, window: Window, handler: EventHandler): Unit = {
  import io.github.humbleui.jwm.{EventWindowFocusIn, EventWindowFocusOut, EventWindowMove, EventWindowResize, EventWindowRestore}
  rootWindow   = window
  eventHandler = handler
  if (handler.logEvents) println(f"$event%s 0x${window._ptr}%12x")
  event match {
    case _: EventWindowFocusOut =>
      import io.github.humbleui.jwm.App
      App.runOnUIThread(() => onRootLeave())
    case _: EventWindowFocusIn =>
      rootWindow.requestFrame()
    case ev: EventWindowResize =>
      resizeRoot(ev.getContentWidth.toFloat, ev.getContentHeight.toFloat)
      rootWindow.requestFrame()
    case _: EventWindowMove =>
      // also when resized by moving a corner or an edge
      rootWindow.requestFrame()
    case _: EventWindowRestore =>
      rootWindow.requestFrame()
    case _ =>
      rootWindow.requestFrame()
  }
}

def close(): Unit = {
  runOnUIThread(()=>rootWindow.close())
}

def isWindowClosed: Boolean = rootWindow.isClosed

def grabKeyboard(component: ReactiveGlyph): Unit = {
  if (eventHandler ne null) eventHandler.keyboardFocus = Some(component)
}

def freeKeyboard(): Unit = {
  if (eventHandler ne null) {
    eventHandler.recentKeyboardFocus = eventHandler.keyboardFocus
    eventHandler.keyboardFocus = None
  }
}

/** True iff the next keystroke will be offered to `component`.  */
def hasKeyboardFocus(component: ReactiveGlyph): Boolean = {
  if (eventHandler ne null) eventHandler.keyboardFocus match {
    case None =>
         eventHandler.recentKeyboardFocus match {
           case None => false
           case Some(focussed) => focussed eq component
         }
    case Some(focussed) => focussed eq component
  } else false
}

def giveupFocus(): Unit = eventHandler.giveupFocus()

def whenUnfocussed(event: EventKey): Unit = {}

}

case class RootLayer(glyph: Glyph, isModal: Boolean, var strictHiding: Boolean, isMenu: Boolean = false, offMenuClick: () => Unit = { ()=> }, var visible: Boolean = true, active: Boolean=false)
