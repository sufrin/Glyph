package org.sufrin.glyph

import GlyphTypes.Scalar

import io.github.humbleui.jwm.{App, Platform, Window}

/**
 * When started, an `Interaction` hosts the given GUI root glyph
 * in the given window. It uses a `EventHandler` to determine
 * reactions to user- or system-generated events that take
 * place in the window.
 *
 * Various (overrideable) defaults define the size and position of the window, its title, its initial position
 * on the screen, etc.
 *
 * @see EventHandler
 *
 * @param window the (main) interaction window
 * @param guiRoot the root glyph of the GUI for this app/window
 *
 * Here's an example main programthat creates a window, associates it with
 * a root GUI glyph, and starts the GUI
 *
 * {{{
 * object GlyphExample {
 *  def main(args: Array[String]): Unit = {
 *    App.start(() => {
 *     new Interaction(App.makeWindow(), LargeTestScenes.root) {
 *         override val iconPath = Some("PNG/eg1g.png")
 *     }.start()
 *   })
 *  }
 * }
 * }}}
 */
class Interaction(val window: Window, guiRoot: Glyph, initialScaleFactor: Scalar = 1.0f) {
  thisInteraction =>

  import GlyphTypes.{Scalar, Pixels}

  import io.github.humbleui.jwm.{EventKey, EventTextInput, EventTextInputMarked, Screen}

  /**
   * Command line arguments, if any, passed from outside the application
   */
  def args: List[String] = Nil

  def screen: Screen = window.getScreen

  val platform: Platform  = Platform.CURRENT
  /** X coordinate of screen on the plane that consists of all screens */
  private def screenLeft: Int      = screen.getWorkArea.getLeft
  /** Y coordinate of screen on the plane that consists of all screens */
  private def screenTop:  Int      = screen.getWorkArea.getTop

  /**
   *   Normalizing hardwareScale factor, mapping logical pixel coordinates to physical screen coordinates.
   */
  private def hardwareScale: Scalar   = handler.hardwareScale

  def title:       String         = "Glyph Interaction"
  def iconPath:    Option[String] = None
  val logEvents:   Boolean        = false

  /** Invoke this when the focus leaves the window */
  def onFocusLeave(): Unit = { }
  /** Reset the handler's focus */
  def giveupFocus(): Unit = {
    handler.giveupFocus()
  }

  /** Invoked for an `EventKey` when there is no `keyboardFocus`  */
  def onKeyboardUnfocussed(key: EventKey): Unit = {}
  /** Invoked for `EventTextInput` when there is no `keyboardFocus` */
  def onKeyboardUnfocussed(key: EventTextInput): Unit = {}
  /** Invoked for an `EventTextInputMarked` when there is no keyboardFocus */
  def onKeyboardUnfocussed(key: EventTextInputMarked): Unit = {}

  /** `(w, h)` of the root window, scaled (as usual) by the screen-dependent hardwareScale factor */
  def size: Pixels  = Vec.scaleToPixels(initialScaleFactor * hardwareScale, handler.root.w, handler.root.h)
  /**  how much width/height to add to the natural size of the GUI root when sizing the window */
  def inset: Pixels = (1, 1)
  /** Standard place for the window depends on the number of open windows */
  def position(window: Window): Pixels = (screenLeft + 20*App._windows.size, screenTop + 20*App._windows.size)

  val handler = new EventHandler { thisHandler =>

    /** Delegates `onKeyboardUnfocussed` to this `Interaction`  */
    override def onKeyboardUnfocussed(key: EventKey): Unit = thisInteraction.onKeyboardUnfocussed(key)
    /** Delegates `onKeyboardUnfocussed` to this `Interaction`  */
    override def onKeyboardUnfocussed(key: EventTextInput): Unit = thisInteraction.onKeyboardUnfocussed(key)
    /** Delegates `onKeyboardUnfocussed` to this `Interaction`  */
    override def onKeyboardUnfocussed(key: EventTextInputMarked): Unit = thisInteraction.onKeyboardUnfocussed(key)

    import io.github.humbleui.jwm.Screen

    val window: Window = thisInteraction.window

    /** The handler draws `root` at scale when needed */
    val root: Glyph    =
      new RootGlyph(guiRoot) {
        override val args: List[String] = thisInteraction.args
      }

    val screen: Screen = thisInteraction.screen
    locally {
      thisHandler.logEvents = thisInteraction.logEvents
      thisHandler.softwareScale = initialScaleFactor
    }

  }

  locally { handler.softwareScale = initialScaleFactor }

  /**
   * Set the size, screen position, and title of the window; construct and install an appropriate `EventHandler`, then
   * make the window visible.
   */
  def start(): Unit = {
    import io.github.humbleui.jwm.skija.LayerGLSkija
    import GlyphTypes.Pixels
    window.setTitle(title)
    val (px, py): Pixels = position(window)
    //println(s"Window at: ($px, $py)")
    // randomly placed if no such position
    // TODO: Refine window placement
    try { window.setWindowPosition(px, py) } catch {
      case ex: IllegalArgumentException => ex.printStackTrace()
    }
    window.setLayer(new LayerGLSkija)

    platform match {
      case Platform.MACOS | Platform.X11 =>
        import java.io.File
        if (iconPath.isDefined) window.setIcon(new File(iconPath.get))
      case _ =>
    }

    // Set the window size
    { val (w, h): Pixels   = size
      val (dx, dy): Pixels = inset
      window.setContentSize(w+dx, h+dy)
    }

    // Set the event Listener, and make the window visible
    window.setEventListener(handler)
    window.setVisible(true)
  }

}


