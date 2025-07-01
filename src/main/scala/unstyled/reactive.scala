package org.sufrin.glyph
package unstyled

import io.github.humbleui.jwm.{EventMouseScroll, Window}
import org.sufrin.glyph.unstyled.static.{FilledRect, INVISIBLE}

/**
 *  A collection of `Reactive` glyph types.
 */
object reactive {

  type Reaction = Modifiers.Bitmap => Unit

  import Brushes._
  import GlyphTypes.Scalar

  /**
   *  A mixin for reactive glyphs that can make nonstandard responses to
   *  the cursor entering or leaving them. `HintManager`s can be
   *  declared for such glyphs; and
   */
  trait Enterable {
    def guiRoot:  RootGlyph
    def reDraw(): Unit

    protected var _onGlyphEvent: Option[(RootGlyph, Boolean, Vec)=>Unit] = None

    /**
     * Set or clear the action to be invoked when the cursor enters or leaves a reactive glyph.
     *
     * The action will be applied to `(root, hovering, location)`, where
     * `root` is the root glyph of the enterable in which the event takes place;
     * `hovering` is true iff the cursor is hovering over the enterable;
     * `location` is the location (relative to the enterable) of the event.
     */
    def onGlyphEvent(action: ((RootGlyph, Boolean, Vec) => Unit)=null): Unit = {
      _onGlyphEvent = if (action eq null) None else Some(action)
    }
  }

  /**
   * A `GenericButton` keeps track of whether the cursor is hovering over it, and
   * whether it has been pressed but not released. If the mouse is pressed and then released
   * it invokes `react()`. If the mouse is pressed over it, but released elsewhere,
   * then there is no reaction.
   */
  abstract class GenericButton extends ReactiveGlyph with Enterable { thisButton =>

    import io.github.humbleui.jwm.{EventMouseButton, Window}

    val react: Reaction // reaction when the button is released while hovering

    /**
     * Reaction to invoke on the UI thread after the main reaction has run.
     * Used in the decoration of menu buttons
     */
    private var _afterReact: Option[Reaction] = None
    /** Specify a reaction to invoke after the main reaction  */
    def afterReact(afterReact: Reaction): this.type = {
      _afterReact = Some(afterReact)
      this
    }

    /**
     * This button's reaction pops up a menu.
     * The `popupMenu` it is on will not pop down automatically when it is pressed.
     *
     * @see Popupmenu
     */
    private  var _isMenuButton: Boolean = false
    override def asMenuButton: this.type = { _isMenuButton = true; this }
    override def isMenuButton: Boolean   = _isMenuButton

    def setHover(state: Boolean): Unit = {
      hovered = state
      reDraw()
    }

    var pressed, hovered: Boolean = false
    var inactive: Boolean = false
    var enabled:  Boolean = true
    /**
     * The most recent state of the modifiers (shift keys, button keys).
     * This is intended to support hinting: maintained but as-yet unused.
     */
    var modifiers: Int    = 0


    @inline def disabled: Boolean = !enabled

    /**
     * Return the current enabled state of the glyph; set the new state
     */
    override def enabled(state: Boolean): Boolean = {
      val oldState = enabled
      enabled = state
      oldState
    }

    def S(caption: String)(bool: Boolean): String = if (bool) caption else ""
    def A: String = toString.take(30)
    def showState: String = s"""${S("P")(pressed)}${S("H")(hovered)} $A... """

    /**
     * Invoke the button's reaction independently of its pressed/hovered state,
     * then reset its state.
     *
     * TODO: for shortcuts
     */
    def invokeReaction(): Unit = {
      pressed = false
      hovered = false
      react(modifiers)
    }

    override def accept(mouse: EventMouseButton, location: Vec, window: Window): Unit = {
      import Modifiers._
      modifiers = Modifiers(mouse)
      //println(s"${modifiers.toLongString}")
      if (!disabled) {
        if (mouse.isPressed) {
          pressed = true
        } else {
          if (pressed) {
            import io.github.humbleui.jwm.App
            // transition from pressed to non-pressed
            pressed = false
            react(modifiers)
            if (_afterReact.isDefined) App.runOnUIThread {
              () => {
                _afterReact.get(Modifiers(mouse))
                hovered = false //** Menu buttons need this to avoid phantom hover post popdown
              }
            }
          }
        }
        //println(s"""$showState ${mouse.isPressed}@$location""")
        markStateChanged
      }
    }



    /**
     * Set or clear a glyph that could be shown on entry to the reactive.
     */
    protected var _hint: Option[Glyph] = None
    def setHint(hint: Glyph = null): Unit =
      _hint = if (hint eq null) None else Some(hint)

    override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {
      modifiers = Modifiers(event)
      //println(s"$event ${modifiers.toLongString}")
      if (!disabled) {
        event match {
          case _: GlyphEnter =>
            import io.github.humbleui.jwm.MouseCursor
            hovered = true
            window.setMouseCursor(MouseCursor.POINTING_HAND)

          case _: GlyphLeave =>
            hovered = false
            pressed = false //println(s"""$showState Leave@$location""")
        }

        try {
          _onGlyphEvent match {
            case None =>
            case Some(action) => action(thisButton.guiRoot, hovered, location + rootDistance)
          }
        } catch { case e: AssertionError => println(s"${e.toString}")}
        markStateChanged
      }
    }
  }

  object ColourButton {
    val up:    Brush = fallback.buttonForeground()
    val down:  Brush = fallback.buttonDown()
    val hover: Brush = fallback.buttonHover()
    val bg:    Brush = fallback.buttonBackground

    /** The simplest form of text button. Its state is indicated by the brush used to paint the foreground (background) of the text.  */
    def apply(text: String, up: Brush=up, down: Brush=down, hover: Brush=hover, bg: Brush = bg, background: Boolean = true, hint: Hint=NoHint)(react: Reaction): ReactiveGlyph = {
        val glyph: Glyph = fallback.buttonText(text, up, bg)
        val button = new ColourButton(glyph, down, hover, background, react)
        hint(button)
        button
    }

    /** The simplest form of glyph button. Its state is indicated by the brush used to paint the foreground (background) of the glyph.  */
    def apply(glyph: Glyph, down: Brush, hover: Brush, background: Boolean, hint: Hint)(react: Reaction): ReactiveGlyph = {
      val button = new ColourButton(glyph, down, hover, background, react)
      hint(button)
      button
    }
  }

  class ColourButton(appearance: Glyph, down: Brush, hover: Brush, val background: Boolean, val react: Reaction) extends GenericButton {
    override def toString: String =
      s"ColourButton($up, $down, $hover, $background)"

    /** The brush  */
    val up: Brush           = if (background) appearance.bg else appearance.fg

    val currentBrush: Brush = up.copy()

    val glyph: Glyph        = if (background) appearance(bg=currentBrush) else appearance(fg=currentBrush)

    def setCurrentBrush(b: Brush): Unit = {
      currentBrush.color(b.color).width(b.strokeWidth).mode(b.mode).pathEffect(b.pathEffect).mode(b.mode)
    }

    locally { glyph.parent = this }

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p)
    override def contains(p: Vec): Boolean = glyph.contains(p)

    override val fg: Brush = glyph.fg
    override val bg: Brush = glyph.bg

    def alphaDown:  Int = 0xFF
    def alphaUp:    Int = 0xFF
    def alphaHover: Int = 0xF0
    def alphaDisabled: Int = 0x70

    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = {
        val (brush, alpha) = {
          if (disabled) (up, alphaDisabled)
          if (inactive) (up, alphaUp) else
          (pressed, hovered) match {
            case (true, true)  => (down,   alphaDown)
            case (false, true) => (hover,  alphaHover)
            case (_, _)        => (up,     alphaUp)
          }
        }
        surface.withAlpha(diagonal, alpha) {
          setCurrentBrush(brush)
          glyph.draw(surface)
          surface.declareCurrentTransform(this)
        }
        // draw the hint, if there is one
        _hint match {
          case None        =>
          case Some(hint) => surface.withOrigin(w-10, h-10) { hint.draw(surface) }
        }
    }

    /**
     * The diagonal size of the glyph
     */
    override def diagonal: Vec = glyph.diagonal

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush=this.fg, bg: Brush=this.bg): Glyph =
      new ColourButton(appearance(fg.copy(), bg.copy()), down, hover, background, react)

  }

  /**
   * A button that normally shows `up`; shows `down` when a mouse button is down
   * within it, and (when hovering) shows `up` displaced/dimmed as specified by its `hover`
   * property.
   */
  class RawButton(up: Glyph, down: Glyph, hover: Glyph, override val fg: Brush, override val bg: Brush,
                  val react: Reaction) extends GenericButton {
    override def toString: String = s"RawButton($up, $down, $hover)"

    locally{
      up.parent = this
      down.parent = this
      hover.parent = this
    }

    /**
     *  When true, it is assumed that the button area is the union of the bounding
     *  boxes of the three glyphs, and whether the mouse cursor is within the button.
     *
     *  When false, the up, down, and hover glyph shapes are used, individually, depending on
     *  the state of the button, to determine whether the cursor is within the button.
     */
    def asBounded: Boolean = !withDetailedShape
    val withDetailedShape: Boolean = false

    override def glyphContaining(p: Vec): Option[Hit] =
          if (asBounded)
            super.glyphContaining(p)
          else
          if (up.glyphContaining(p).isDefined)
            Some(Hit(this, Vec.Origin))
          else
            None

    override def contains(p: Vec): Boolean =
      if (asBounded)
        super.contains(p)
      else
      if (pressed && hovered)
        super.glyphContains(down, p)
      else
        if(hovered) super.glyphContains(hover, p)
      else
        super.glyphContains(up, p)

    override def diagonal: Vec = up.diagonal union down.diagonal union hover.diagonal + extra
    def extra:      Vec = Vec(5, 5)
    def alphaDown:  Int = 0xFF
    def alphaUp:    Int = 0xFF
    def alphaHover: Int = 0x9F
    def alphaDisabled: Int = 0x70

    val downOffset  = diagonal - down.diagonal - (extra * 0.5f)
    val upOffset    = diagonal - up.diagonal
    val hoverOffset = diagonal - hover.diagonal

    def copy(fg: Brush=fg, bg: Brush = bg): RawButton = new RawButton(up(), down(), hover(), fg, bg, react)

    private val alwaysPrescale = true

    override def draw(surface: Surface): Unit = {
      //println(s"drawing $this")
      //drawBackground(surface)
      // an inactive button just shows its `up` glyph
      if (inactive || disabled) {
        surface.withAlpha(diagonal, if (disabled) alphaDisabled else alphaUp) {
          surface.withOrigin(upOffset.x * 0.5f, upOffset.y * 0.5f) {
            up.draw(surface)
            surface.declareCurrentTransform(this)
          }
        }
      } else
      if (pressed && hovered)
        surface.withAlpha(diagonal, alphaDown) {
          surface.withOrigin(downOffset.x * 0.5f, downOffset.y * 0.5f) {
            down.draw(surface)
            surface.declareCurrentTransform(this)
          }
        }
      else if (hovered)
        surface.withAlpha(diagonal, alphaHover) {
          surface.withOrigin(hoverOffset.x * 0.5f, hoverOffset.y * 0.5f) {
            hover.draw(surface)
            surface.declareCurrentTransform(this)
          }
        }
      else
        surface.withAlpha(diagonal, alphaUp) {
          surface.withOrigin(upOffset.x * 0.5f, upOffset.y * 0.5f) {
            up.draw(surface)
            surface.declareCurrentTransform(this)
          }
        }
    }
  }

  object RawButton {
    /**
     *  A button with up/down/hover glyphs, and offsets/alphas
     *  that reflect its state
     */
    def apply(up: Glyph, down: Glyph, hover: Glyph)(reaction: Reaction): RawButton =
      new RawButton(up, down, hover, up.fg, up.bg, { reaction } )

    /**
     * A button with up/down/hover glyphs specified by `up`, and down and hover
     * colours as specified. Also offsets/alphas that reflect its state.
     */
    def apply(up: Glyph, down: Brush, hover: Brush)(reaction: Reaction): RawButton =
      new RawButton(up, up(fg=down), up(fg=hover), up.fg, up.bg, { reaction })

    /**
     * A button with up/down/hover glyphs, but no offsets/alphas
     * that reflect its state. The exact shape of the glyphs are used to
     * decide whether the mouse cursor is within the button.
     */
    def exact(up: Glyph, down: Glyph, hover: Glyph)(reaction: Reaction): RawButton =
      new RawButton(up, down, hover, up.fg, up.bg, { reaction }) {
        override val withDetailedShape: Boolean = true
        override def extra: Vec  = Vec.Zero
        override val downOffset: Vec = Vec.Zero
        override val upOffset: Vec = Vec.Zero
        override val hoverOffset: Vec = Vec.Zero
      }

    /**
     * A button with up/down/hover glyphs specified by `up` but no offsets/alphas
     * that reflect its state. The down and hover colours are specified by the given paints.
     * The exact shape of the glyph  is used to
     * decide whether the mouse cursor is within the button.
     */
    def exact(up: Glyph, down: Brush, hover: Brush)(reaction: Reaction): RawButton =
      new RawButton(up, up(fg = down), up(fg = hover), up.fg, up.bg, { reaction }) {
        override val withDetailedShape: Boolean = true
        override def extra: Vec = Vec.Zero
        override val downOffset: Vec = Vec.Zero
        override val upOffset: Vec = Vec.Zero
        override val hoverOffset: Vec = Vec.Zero
      }
  }

  /** Simple buttons with intrinsic (default) styling.*/
  object TextButton  {

    def apply(text: String, fg: Brush = fallback.buttonForeground, bg: Brush = fallback.buttonBackground, background: Boolean = true)
             (reaction: Reaction): ColourButton = {
         val up = fallback.buttonText(text, fg, bg).enlarged(fallback.upFrame.strokeWidth * 4)
         //new RawButton(up, up(fg = red), up(fg = green), up.fg, up.bg, reaction)
         new ColourButton(up, fg, bg, background, reaction)
       }


    }

  object UniformlyColouredButton {
    /**
     * A uniformly coloured button, with a hint
     */
    def apply(size: Vec, colour: Brush, hint: Hint=NoHint)
             (reaction: Reaction): RawButton = {
      val glyph = FilledRect(size.x, size.y, colour)
      val but = new RawButton(glyph, glyph(), glyph(), colour, colour, reaction)
      hint(but)
      but
    }
  }

  object FramedButton  {

    /** A framed button whose up, down, and hover glyphs look the same */
    def apply(up: Glyph)(reaction: Reaction): RawButton =
      new RawButton(
        up.framed(fallback.upFrame),
        up().framed(fallback.downFrame),
        up().framed(fallback.hoverFrame), up.fg, up.bg, reaction)

    /** A framed button whose up, down, and hover glyphs are all `text` */
    def apply(text: String, fg: Brush=fallback.buttonForeground, bg: Brush=fallback.buttonBackground)(reaction: Reaction): RawButton = {
      val up = fallback.buttonText(text, fg, bg).enlarged(fallback.upFrame.strokeWidth*4)
      new RawButton(
        up.framed(fallback.upFrame),
        up().framed(fallback.downFrame),
        up().framed(fallback.hoverFrame), up.fg, up.bg, reaction)
    }

    /**
     *   A framed button whose up, down, and hover glyphs are all `up`, and whose action is
     *   independent of the state of the keyboard modifiers or the mouse button that was pressed.
     */
    def apply(up: Glyph)(action: => Unit): RawButton = new RawButton(
      up.framed(fallback.upFrame),
      up().framed(fallback.downFrame),
      up().framed(fallback.hoverFrame), up.fg, up.bg, { _ => action })

  }

  object ShadedButton  {

    import org.sufrin.glyph.unstyled.static.Shaded

    /** A button with a shaded presentation, and `text` as its caption */
    def apply(text: String, fg: Brush = fallback.buttonForeground, bg: Brush = fallback.buttonBackground, delta: GlyphTypes.Scalar=6f)(reaction: Reaction): RawButton = {
      val up    = Shaded.Static(fg, bg, delta, false)(fallback.buttonText(text, fg, bg))
      val down  = Shaded.Static(fg, bg, delta, true)(fallback.buttonText(text, fg, bg))
      val hover = up.copy()
      RawButton(up, down, hover) (reaction)
    }

    /** A button with a shaded presentation, and `glyph` as its caption */
    def apply(glyph: Glyph, fg: Brush, bg: Brush, delta: GlyphTypes.Scalar)(reaction: Reaction): RawButton = {
      val up = Shaded.Static(fg, bg, delta, false)(glyph)
      val down = Shaded.Static(fg, bg, delta, true)(glyph)
      val hover = up.copy()
      RawButton(up, down, hover)(reaction)
    }

  }

  /**
   * A generic slider
   *
   * TODO: Turned sliders attract focus curiously.
   */
  abstract class Slider extends ReactiveGlyph with Enterable { thisSlider =>

    import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, MouseCursor, Window}
    /** Invoked when the cursor drags to `loc` */
    def dragTo(loc: Vec): Unit
    def dragTo(proportion: Scalar): Unit

    val verticalCursor   = MouseCursor.RESIZE_NS
    val horizontalCursor = MouseCursor.RESIZE_WE
    val cursor           = MouseCursor.CROSSHAIR

    var pressed, hovered: Boolean = false
    var inactive: Boolean = false
    var enabled:  Boolean = true
    /**
     * The most recent state of the modifiers (shift keys, button keys).
     * This is intended to support hinting: maintained but as-yet unused.
     */
    var modifiers: Int    = 0
    @inline def disabled: Boolean = !enabled

    /**
     * Return the current enabled state of the glyph; set the new state
     */
    override def enabled(state: Boolean): Boolean = {
      val oldState = enabled
      enabled = state
      oldState
    }

    def S(caption: String)(bool: Boolean): String = if (bool) caption else ""
    def A: String = toString.take(30)
    def showState: String = s"""${S("P")(pressed)}${S("H")(hovered)} $A... """

    override def accept(mouse: EventMouseMove, location: Vec, window: Window): Unit = {
        if (pressed) {
          dragTo(location)
          reDraw()
        }
    }

    override def accept(mouse: EventMouseButton, location: Vec, window: Window): Unit = {
      modifiers = Modifiers(mouse)
      //println(s"${modifiers.toLongString}")
      if (!disabled) {
        if (mouse.isPressed) {
          pressed = true
          dragTo(location)
        } else {
          pressed = false
        }
        //println(s"""$showState ${mouse.isPressed}@$location""")
        markStateChanged
      }
    }


    override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {
      modifiers = Modifiers(event)
      //println(s"$event ${modifiers.toLongString}")
      if (!disabled) {
        event match {
          case _: GlyphEnter =>
            hovered = true
            window.setMouseCursor(cursor)

          case _: GlyphLeave =>
            hovered = false
            pressed = false //println(s"""$showState Leave@$location""")
        }

        _onGlyphEvent match {
          case None =>
          case Some(action) => action(thisSlider.guiRoot, hovered, location + rootDistance)
        }
        markStateChanged
      }
    }

  }

  class HorizontalSlider(track: Glyph, image: Glyph, initialProportion: Scalar, override val fg: Brush, override val bg: Brush, reaction: Scalar => Unit) extends Slider {
    val diagonal: Vec = Vec(track.w, image.h max track.h)
    override def toString: String = s"Slider.H(${(track.w,track.h)} [${(image.w, image.h)}])"
    var x: Scalar = 0
    var y: Scalar = 0
    @inline private def onTrack(cx: Scalar): Boolean  =
      0 <= cx && cx+image.w<w

    override val cursor = horizontalCursor

    val trackLength: Scalar = w - image.w

    locally {
      val newX = (initialProportion*trackLength)
      if (onTrack(newX)) this.x=newX
    }

    def dragTo(proportion: Scalar): Unit = {
      val newX = (proportion*trackLength)
      if (onTrack(newX)) {
        this.x=newX
        reDraw()
      }
    }

    /** Invoked when the cursor drags to `loc` */
    def dragTo(loc: Vec): Unit = {
      val newX = (loc.x - image.w/2) max 0
      if (onTrack(newX)) {
        this.x = newX
        this.y = 0
        reaction(x/trackLength)
      }
    }

    override def accept(wheel: EventMouseScroll, origin: Vec, window: Window): Unit = {
        val dx = wheel.getDeltaY min 50f max -50f
        if (onTrack(x+dx)) {
          this.x = x+dx
          reaction(x/trackLength)
          reDraw()
        }
    }

    val trackOffset: Scalar = (h - track.h)/2 max 0
    val imageOffset: Scalar = (track.h-image.h)/2 max 0
    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.declareCurrentTransform(this)
        surface.withOrigin(0, trackOffset) { track.draw(surface) }
        surface.withOrigin(x, imageOffset) { image.draw(surface) }
    }

    locally { track.parent = this }

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = new HorizontalSlider(track, image, initialProportion, fg, bg, reaction)
  }


  class VerticalSlider(track: Glyph, image: Glyph, initialProportion: Scalar, override val fg: Brush, override val bg: Brush, reaction: Scalar => Unit) extends Slider {
    val diagonal: Vec = Vec(image.w max track.w, image.h max track.h)
    override def toString: String = s"Slider.V(${(track.w,track.h)} [${(image.w, image.h)}])"
    var x: Scalar = 0
    var y: Scalar = 0
    @inline private def onTrack(cy: Scalar): Boolean  =
      0 <= cy && cy+image.h<h

    val trackLength: Scalar = h - image.h

    override val cursor = verticalCursor

    locally {
      val newY = (initialProportion*trackLength).toFloat
      if (onTrack(newY)) {
        this.y=newY
      }
    }

    def dragTo(proportion: Scalar): Unit = {
      val newY = (proportion*trackLength).toFloat
      if (onTrack(newY)) {
        this.y=newY
        reDraw()
      }
    }

    /** Invoked when the cursor drags to `loc` */
    def dragTo(loc: Vec): Unit = {
      val newY = (loc.y - image.h/2) max 0
      if (onTrack(newY)) {
        this.x = 0
        this.y = newY
        reaction(y/trackLength)
      }
    }

    override def accept(wheel: EventMouseScroll, origin: Vec, window: Window): Unit = {
      val dy = wheel.getDeltaY min 50f max -50f
      if (onTrack(y+dy)) {
        this.y = y+dy
        reaction(y/trackLength)
        reDraw()
      }
    }

    val trackOffset: Scalar = (w - track.w)/2 max 0
    val imageOffset: Scalar = (track.w-image.w)/2 max 0
    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.declareCurrentTransform(this)
      surface.withOrigin(trackOffset, 0) { track.draw(surface) }
      surface.withOrigin(imageOffset, y) { image.draw(surface) }
    }

    locally { track.parent = this }

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = new VerticalSlider(track, image, initialProportion, fg, bg, reaction)
  }

  object Slider {
    def Horizontal(track: Glyph, image: Glyph, initialProportion: Scalar = 0f, fg: Brush = transparent, bg: Brush = transparent)(reaction: Scalar => Unit): HorizontalSlider =
        new HorizontalSlider(track, image, initialProportion, fg, bg, reaction)

    def Vertical(track: Glyph, image: Glyph, initialProportion: Scalar = 0f, fg: Brush = transparent, bg: Brush = transparent)(reaction: Scalar => Unit): VerticalSlider =
      new VerticalSlider(track, image, initialProportion, fg, bg, reaction)
  }


  /**
   *
   *   A reactive glyph that delegates all accepts to `glyph`.
   *   Usually one or more of the accept methods will be overridden.
   */
  class Delegate(glyph: ReactiveGlyph) extends ReactiveGlyph {

    import io.github.humbleui.jwm._

    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = glyph.draw(surface)

    /**
     * The diagonal size of the glyph
     */
    def diagonal: Vec = glyph.diagonal

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = new Delegate(glyph)

    override val fg: Brush = glyph.fg
    override val bg: Brush = glyph.bg

    override def isReactive: Boolean = true

    /** If the glyph wants to react to a mouse click */
    override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = glyph.accept(event, location, window)

    /** If the glyph wants to react to a mouse movement */
    override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = glyph.accept(event, location, window)

    /** If the glyph wants to react to the mousewheel */
    override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = glyph.accept(event, location, window)

    /** If the glyph wants to react to a keystroke */
    override def accept(key: EventKey, location: Vec, window: Window): Unit = glyph.accept(key, location, window)

    override def accept(key: EventTextInput, location: Vec, window: Window): Unit = glyph.accept(key, location, window)

    override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = glyph.accept(key, location, window)
    // ... etc

    // Synthetic events delivered by the standard event handler
    override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = glyph.accept(event, location, window)

    // Synthetic events delivered by the standard event handler on focusin/focusout
    override def accept(event: RootGlyphEvent, location: Vec, window: Window): Unit = glyph.accept(event, location, window)

  }

}
