package org.sufrin.glyph
package unstyled
package dynamic

import unstyled.{static, Text}

import io.github.humbleui.jwm.App

/**
 * A collection of dynamic glyphs: glyphs some aspects of which can vary dynamically
 */


  import GlyphTypes.{Font, Scale}

  /**
   * A `Glyph` with the same diagonal as `glyph`, which is drawn at the
   * current `value` of `hardwareScale`, within the original bounding box.
   *
   * `hardwareScale` is the variable `theScale`, unless `theScale` is `null`, in which case a  new
   * `Variable` (with value 1.0f is allocated.
   */
  class DynamicallyScaled(theScale: Variable[Scale], glyph: Glyph) extends Glyph {
    override val fg = glyph.fg
    override val bg = glyph.bg
    val scale = if (theScale eq null) Variable(1.0f) else theScale
    def draw(surface: Surface): Unit = {
      surface.withClip(diagonal) {
        surface.withScale(scale.value) {
          glyph.draw(surface)
        }
      }
    }

    locally { glyph.parent = this }

    /**
     * The diagonal size of the glyph
     */
    def diagonal: Vec = glyph.diagonal

    def copy(fg: Brush=fg, bg: Brush = bg): DynamicallyScaled = new DynamicallyScaled(theScale, glyph.copy(fg, bg))

  }

  object DynamicallyScaled {
    def apply(theScale: Variable[Scale]=null)(glyph: Glyph): DynamicallyScaled = new DynamicallyScaled(theScale, glyph)
  }

  /**
   *  A `Glyph` with the same diagonal as `glyph`, which is drawn with origin at `(panFactor, tiltFactor)`
   */
  class DynamicallyScrolled(tiltFactor: Variable[Float], panFactor: Variable[Float], glyph: Glyph) extends Glyph {
    override val fg = glyph.fg
    override val bg = glyph.bg
    def draw(surface: Surface): Unit = {
      surface.withOrigin(panFactor.value, tiltFactor.value) {
        glyph.draw(surface)
      }
    }

    locally { glyph.parent = this }

    def diagonal: Vec = glyph.diagonal

    def copy(fg: Brush=fg, bg:Brush=bg): DynamicallyScrolled = new DynamicallyScrolled(tiltFactor, panFactor, glyph.copy(fg, bg))

  }

  object DynamicallyScrolled {
    def apply(tiltFactor: Variable[Float], panFactor: Variable[Float])(glyph: Glyph): Glyph =
      new DynamicallyScrolled(panFactor, tiltFactor, glyph)
  }


  /**
   * A reactive glyph, based on `glyph` that responds to mousewheel and other scrolling
   * events, by tilting, panning (if the wheel was ctrl-shifted) and scaling (if the wheel
   * was shift-shifted). When the mouse is clicked in the glyph its scale and location are
   * reset. "Dragging" can also be enabled in the glyph.
   *
   * The `glyph` may be dynamically sized (for example if it is constructed by `styled.Resizeable`) and
   * the port behaves appropriately when this is the case.
   *
   * Unless it is set explicitly by `initialPortDiagonal` the port size is the size of the glyph
   * when the `ViewPort` is first constructed.
   *
   * The buck stops here when it comes to locating the active glyph containing a given point.
   * In short, one of these glyphs CANNOT USEFULLY contain any reactives.
   *
   *
   * TODO: the above would require a new focus within the top-level dispatcher; namely
   *       one to which all events are forwarded from the top level. This amounts to an
   *       internal virtual window. Perhaps straightforward to deliver eventually.....
   */
  class ViewPort(glyph: Glyph, override val fg: Brush, override val bg: Brush, initialPortDiagonal: Vec = null) extends ReactiveGlyph {

    import GlyphTypes.Scalar

    import io.github.humbleui.jwm.{EventKey, EventMouseScroll}

    val scale     = Variable(1f)
    val pan, tilt = Variable(0f)

    val portDiagonal: Vec = if (initialPortDiagonal eq null) glyph.diagonal else initialPortDiagonal

    override def kind: String = s"ViewPort($scale, $pan, $tilt)"

    val delegate = DynamicallyScrolled(pan, tilt) {
      DynamicallyScaled(scale) {
        glyph
      }
    }

    locally { delegate.parent = this }

    def scrollBy(amount: Scalar): Unit = {
      val newTilt = tilt.value+amount
      if (newTilt.abs < 0.95f*glyph.h) tilt.value = newTilt
    }

    def panBy(amount: Scalar): Unit = {
      val newPan = pan.value+amount
      if (newPan.abs < 0.95f*glyph.h) pan.value = newPan
    }

    def scaleBy(amount: Scalar): Unit =
      if (_enableScale) scale.value *= amount

    def reset(): Unit = {
      scale.value = 1f
      pan.value = 0f
      tilt.value = 0f
      reDraw()
    }

    private var _enableDrag: Boolean = false
    def enableDrag(enable: Boolean): this.type = { _enableDrag = enable; this }

    private var _enableScale: Boolean = false
    def enableScale(enable: Boolean): this.type = { _enableScale = enable; this }

    import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, KeyModifier, Window}

    /**
     * The last known coordinates of the mouse
     */
    object MousePosition {
      var lastX, lastY = 0
    }

    override def accept(mouse: EventMouseMove, location: Vec, window: Window): Unit = {
      var dy: Int = mouse.getMovementY
      var dx: Int = mouse.getMovementX
      if (Modifiers.toBitmap(mouse).any) {
        // Hack because Linux doesn't report movement magnitudes
        if (dy == 0 && dx == 0) {
          val thisX: Int = mouse.getX
          val thisY: Int = mouse.getY
          dx = (thisX - MousePosition.lastX)
          dy = (thisY - MousePosition.lastY)
          MousePosition.lastX = thisX
          MousePosition.lastY = thisY
        }
        if (_enableDrag) {
          panBy(dx.toFloat)
          scrollBy(dy.toFloat)
          reDraw() // window.requestFrame()
        }
      }
    }


    override def accept(mouse: EventMouseButton, location: Vec, window: Window): Unit = {
      if (_enableDrag) {
        if (mouse.isPressed) {
          MousePosition.lastX = mouse.getX
          MousePosition.lastY = mouse.getY
        }
        if (mouse.isPressed && mouse.isModifierDown(KeyModifier.SHIFT) && mouse.isModifierDown(KeyModifier.CONTROL)) {
          reset()
        }
      } else {
        reset()
      }
    }

    override def accept(scroll: EventMouseScroll, location: Vec, window: Window): Unit = {
      // OS/X encodes horizontal scroll on a uniwheel by a shift. Do others?
      if (scroll.isModifierDown(KeyModifier.SHIFT)) { // magnification
        val delta = if (scroll._deltaY + scroll._deltaX > 0) 1.2f else 1 / 1.2f
        scaleBy(delta)
      }
      else if (scroll.isModifierDown(KeyModifier.CONTROL)) { // panning
        panBy(scroll._deltaY)
      } else { // tilting
        scrollBy(scroll._deltaY)
      }
      reDraw() // window.requestFrame()
    }

    override def accept(key: EventKey, location: Vec, window: Window): Unit = {
      import Modifiers._

      import io.github.humbleui.jwm.Key._
      val mods: Bitmap = toBitmap(key)

      if (mods.includeSome(Pressed)) key._key match {
        case END        =>
          tilt.value    = -glyph.h*0.9f
          pan.value     = -glyph.w*0.9f
        case HOME       => reset()
        case UP         => scrollBy(glyph.h*0.12f)
        case DOWN       => scrollBy(-glyph.h*0.12f)
        case PAGE_UP    => scrollBy(portDiagonal.y*0.9f)
        case PAGE_DOWN  => scrollBy(-portDiagonal.y*0.9f)
        case RIGHT      => panBy(-glyph.w*0.12f)
        case LEFT       => panBy(glyph.w*0.12f)
        case other      =>
      }
      reDraw()
    }

    var selected: Boolean = false

    override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {
      event match {
        case _: GlyphEnter =>
          import io.github.humbleui.jwm.MouseCursor
          guiRoot.grabKeyboard(this)
          window.setMouseCursor(MouseCursor.CROSSHAIR)
          selected = true
          reDraw() // window.requestFrame()
        case _: GlyphLeave =>
          guiRoot.freeKeyboard()
          selected = false
          reDraw() // window.requestFrame()
      }
    }


    val offset = Vec(fg.strokeWidth/2, fg.strokeWidth/2)

    val selectedColor   = fg(cap=Brushes.ROUND)
    val unselectedColor = fg(width=fg.strokeWidth, cap=Brushes.ROUND, color=0XFF666666)

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withClip(diagonal) {
        surface.withOrigin(offset) {
          delegate.draw(surface)
        }
      }
      surface.drawRect(if (selected) selectedColor else unselectedColor, Vec.Origin, diagonal)
      // Because this is a reactive glyph
      surface.declareCurrentTransform(this)
    }

    val diagonal: Vec = portDiagonal + (offset * 2f)

    def copy(fg: Brush=fg, bg:Brush=bg): ViewPort = new ViewPort(glyph.copy(fg, bg), fg, bg, initialPortDiagonal)

  }

  object ViewPort {
    def apply(glyph: Glyph, fg: Brush=Brushes.black, bg: Brush = Brushes.white, initialPortDiagonal: Vec=null): ViewPort = new ViewPort(glyph, fg, bg, initialPortDiagonal)
  }

  /**
   * TODO: Idea: a control region for an actively viewed glyph
   */



  /**
   * Dynamically chooseable glyph from among the sequence of `glyphs`, that behaves exactly like
   * the currently-selected of the glyphs, whose origin is at the centre of the bounding box.
   *
   * Its bounding box is the union of the bounding boxes of `glyphs` enlarged by
   * a perimeter of half the foreground strokewidth.
   *
   * Its `bg` is set to `BG`, if that appears, else to the `bg` of the largest (in area) of the glyphs.
   */
  class OneOf(val glyphs: Seq[Glyph], align: Alignment, valign: VAlignment, override val fg: Brush, BG: Brush=null, val enableBG: Boolean = true) extends Composite(glyphs) {
    override val bg = if (BG eq null) OneOf.largestBG(glyphs) else BG
    override val kind = "OneOf"
    override def toString: String = s"""OneOf(fg=$fg, bg=$bg\n glyphs=\n  ${glyphs.map(_.toString).mkString(",\n  ")})"""

    var _selection: Int = 0
    /** Select the `selection%length`th glyph. */
    def select(selection: Int): Unit = _selection = selection%glyphs.length
    /** How many glyphs are presemnt. */
    def length: Int = glyphs.length

    def selection: Int = _selection
    @inline def selectedGlyph: Glyph = glyphs(_selection)

    def next(): Unit = { select((_selection+1)%glyphs.length) }
    def prev(): Unit = { _selection = if (_selection==0) glyphs.length-1 else _selection-1 }

    /**
     * Space for all
     */
    val inset = fg.getStrokeWidth

    val boundingRect = Vec(glyphs.map(_.w).max, glyphs.map(_.h).max)+(inset, inset)
    val diagonal = boundingRect+(inset, inset)

    // Locate the subglyphs.
    // Link subglyphs into the glyph tree
    // TODO: (why didn't this happen in Composite?)
    locally {
      for {glyph <- glyphs} {
          glyph @@ ((diagonal.x-glyph.w)*align.proportion, (diagonal.y-glyph.h)*valign.proportion)
          glyph.parent = this
      }
    }

    override def draw(surface: Surface): Unit = {
      if (enableBG) drawBackground(surface)
      val glyph = selectedGlyph
      //surface.withOrigin(inset/2, inset/2) { surface.drawRect(fg, boundingRect) }
      surface.withOrigin(glyph.location) { glyph.draw(surface) }
    }

    override def glyphContaining(p: Vec): Option[Hit] =
      selectedGlyph.glyphContaining(p-selectedGlyph.location)

    override def contains(p: Vec): Boolean =
      selectedGlyph.contains(p)

    /**
     * A  copy of this glyph: made with the same constructor
     */
    def copy(fg: Brush=fg, bg:Brush=bg): Glyph = new OneOf(glyphs, align, valign, fg, bg, enableBG)
  }

  object OneOf {
    val defaultBG: Brush = Brush("transparent")
    val defaultFG: Brush = defaultBG

    def largestBG(glyphs: Seq[Glyph]): Brush = {
      val g = glyphs.foldLeft(glyphs.head){ (l, r) => if (l.h*l.w > r.h*r.w) l else r }
      g.bg
    }

    def apply(fg: Brush=defaultFG, bg: Brush=null, align: Alignment=Center, valign: VAlignment=Mid, enableBG: Boolean = true)(glyphs: Glyph*): OneOf =
        new OneOf(glyphs, align, valign, fg=fg, BG=bg, enableBG)


    def seq(fg: Brush=defaultFG, bg: Brush=defaultFG, align: Alignment=Center, valign: VAlignment=Mid)(glyphs: Seq[Glyph]): OneOf =
        new OneOf(glyphs, align, valign, fg=fg, BG=bg)

  }

  /**
   * A (laterally) split screen that (initially) juxtaposes `left` to `right`. If `dynamic`
   * its bounding box is large enough to accomodate each of them separately; else it is large
   * enough to accomodate them side-by-side. The boundary between the current left and right
   * is drawn as a vertical line with the brush `fg`.
   *
   *
   * @param left
   * @param right
   * @param dynamic
   * @param fg
   * @param bg
   */
  class SplitScreen(left: Glyph, right: Glyph, dynamic: Boolean, override val fg: Brush, override val bg: Brush)
        extends  Glyph {
    import GlyphTypes.Scalar

    locally { left.parent = this; right.parent = this }

    var theLeft  = left
    var theRight = right

    var _boundary:   Scalar = left.w
    var _proportion: Scalar = left.w/w

    def boundary: Scalar   = _boundary
    def proportion: Scalar = _proportion

    /**
     * Set the boundary at `proportion` of the width of the bounding box.
     * @param proportion
     */
    def setBoundary(proportion: Scalar): Unit = {
      _proportion = proportion
      _boundary   = proportion * w
      reDraw()
    }

    /**
     * Exchange the current `left` and `right`, and re-establish the boundary if necessary:
     * if `dynamic` is true, then it is where it was before the exchange;
     * otherwise it is between the current left and the current right.
     */
    def exchange(): Unit = {
      val t = theLeft
      theLeft = theRight
      theRight = t
      if (dynamic) setBoundary(_proportion) else setBoundary(theLeft.w/w)
    }

    val sepWidth = fg.strokeWidth
    val offset   = sepWidth/2f

    def diagonal: Vec =
        if (dynamic)
          Vec((left.w max right.w) + sepWidth, left.h max right.h)
        else
          Vec(left.w + right.w + sepWidth, left.h max right.h)

    override def draw(surface: Surface): Unit = {
      val leftScope    = Vec(_boundary, h)
      val rightScope   = Vec((w-_boundary-sepWidth) max 0f, h)
      val ambientScale = guiRoot.ambientScale

      /* The left pane */
      surface.withScope(ambientScale, leftScope) {
        surface.withClip(leftScope) {
            theLeft.draw(surface)
        }
      }

      /* The divider */
      surface.drawLines$(fg, _boundary+offset, 0, _boundary+offset, h)

      /* The right pane */
      surface.withOrigin(_boundary + sepWidth, 0) {
          surface.withScope(ambientScale, rightScope) {
            surface.withClip(rightScope) {
              theRight.draw(surface)
          }
        }
      }
    }

    /**
     * In the appropriate sub-glyph, find the glyph containing `p` . The
     * boundary between subglyphs is the (vertical) divider.
     */
    override def glyphContaining(p: Vec): Option[Hit] = {
      if (0f <= p.x && p.x < _boundary)
          theLeft.glyphContaining(p)
      else if(_boundary+fg.strokeWidth<=p.x && p.x<w)
          theRight.glyphContaining(p-(_boundary+fg.strokeWidth, 0))
      else None
    }

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new SplitScreen(left, right, dynamic, fg, bg)

  }

  object SplitScreen {
    def apply(left: Glyph, right: Glyph, dynamic: Boolean, fg: Brush=Brushes.black, bg: Brush = Brushes.transparent): SplitScreen =
      new SplitScreen(left, right, dynamic, fg, bg)
  }


  trait Steppable {
    def start(): Unit = {}
    def step(): Unit  = {}
    def stop(): Unit  = {}
  }

  trait Animateable[T] extends Settable[T] with Steppable

  /**
   * An active glyph whose bounding box is that of its initial glyph: which is `background` unless that is `null`, otherwise
   * `toGlyph(initial)`
   *
   * Thereafter it always shows `toGlyph(current)` (clipped to the bounding box of `background`)
   * after the first and subsequent assignments  to `current`. Although this is unenforceable,
   * it makes sense for `background` to be `toGlyph(initial)`.
   */
  abstract class ActiveGlyph[T](initial: T, val background: Glyph=null) extends Glyph with Animateable [T]  {
    def toGlyph(t: T): Glyph

    val forcedSet: Boolean = false

    val initialGlyph = if (background eq null) toGlyph(initial) else background

    protected var currentGlyph: Glyph = initialGlyph
    protected var current: T          = initial

    /**
     * Set the current state, and show its `toGlyph`.
     */
    def set(state: T): Unit =  {
      if (forcedSet || state != current) currentGlyph = toGlyph(state)
      current = state
      // if (hasGuiRoot)
      reDraw()
    }

    /** Get the current state. */
    def get: T = current

    override def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.withClip(diagonal) {
          surface.withOrigin(currentGlyph.location) {
            currentGlyph.draw(surface)
          }
        }
      }

    override def diagonal: Vec = initialGlyph.diagonal

    override val fg: Brush = initialGlyph.fg
    override val bg: Brush = initialGlyph.bg

    override def glyphContaining(p: Vec): Option[Hit] = currentGlyph.glyphContaining(p-currentGlyph.location)
  }

  class ActiveString(font: Font, fg: Brush, bg: Brush, initial: String) extends ActiveGlyph[String](initial, Text(initial, font, fg, bg)) {
    def toGlyph(t: String): Glyph = Text(t, font, fg, bg)
    override def copy(fg: Brush, bg: Brush): Glyph = new ActiveString(font, fg, bg, initial)
  }

  object ActiveString {
    def apply(font: Font, fg: Brush, bg: Brush)(initial: String): ActiveString =
        new ActiveString(font, fg, bg, initial)
  }


  trait Transform extends Function1[Glyph,Glyph]

  /**
   * An `ActiveGlyph[Int]` constructed from an original `glyph` by applying a sequence of transforms to it.
   * The resulting active glyph shows `transforms(i%transforms.length)(glyph)` when in state `i`.
   * Its bounding box is the union of the bounding boxes of the transformed glyphs; and each glyph
   * is effectively centered in it when shown.
   * @param glyph
   * @param transforms
   * @param stretch
   *
   * @see OneOf
   */
  class Transformable(glyph: Glyph, transforms: Seq[Transform]) extends ActiveGlyph[Int](0, static.INVISIBLE()){
    val transformed: Seq[Glyph] = transforms.map{ transform => transform(glyph) }
    val maxW = Measure.maxHeight(transformed)
    val maxH = Measure.maxWidth(transformed)

    override def diagonal: Vec = Vec(maxW, maxH)
    override val background: Glyph = transformed(0)

    locally {
      // place the transformed glyphs, and link them into the tree
      for {g <- transformed} {
        val x = (diagonal.x - g.w) / 2f
        val y = (diagonal.y - g.h) / 2f
        g.parent = this
        g @@ (x, y)
      }
    }

    def toGlyph(frame: Int): Glyph = transformed(frame)

    override def step(): Unit = set((current + 1)%transforms.length)


    locally { currentGlyph = toGlyph(current) }

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = static.INVISIBLE()
  }

  object Transformable {
    def apply(glyph: Glyph, transforms: Seq[Transform]): Transformable = {
        new Transformable(glyph, transforms)
    }
  }

  object Periodic {
    def apply[T](glyph: Animateable[T], msPerFrame:    Long=40L): Periodic[T] = new Periodic[T](glyph, msPerFrame)
    def apply[T](glyph: Animateable[T], fps: Double): Periodic[T]             = new Periodic[T](glyph, (1000.0/fps).toLong)
  }

  class Periodic[T](glyph: Animateable[T], private var _msPerFrame: Long) {
    val schedule = Schedule(_msPerFrame){App.runOnUIThread(() => glyph.step())}

    def running = schedule.running

    def start(): Unit = if (!schedule.running) {
      schedule.periodically()
    }

    def stop(): Unit =
      if (schedule.running) {
        schedule.cancel()
      }

    def msPerFrame_=(msPerFrame: Long): Unit = {
       _msPerFrame = msPerFrame
       schedule.period = msPerFrame
    }

    def msPerFrame: Long = _msPerFrame

    def fps_=(fps: Double): Unit = {
      _msPerFrame = if (fps>0.0) (1000.0/fps).toLong else 1000L
      schedule.period = _msPerFrame
    }

    def fps: Double = 1000.0/_msPerFrame
  }

