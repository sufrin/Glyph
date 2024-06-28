package org.sufrin.glyph

/**
 *  A collection of `Reactive` glyph types.
 */
object ReactiveGlyphs extends Brushes {

  type Reaction = Modifiers.Bitmap => Unit

  import GlyphTypes.Scalar


  /**
   *  Experimental: derive the window to redraw from the glyph
   *  hierarchy, not from the event being accepted. This is
   *  intended to let us tidy up the signatures of the plethora of
   *  `accept` methods of `ReactiveGlyph`s.
   */
  val hierarchical: Boolean = false

  /**
   * TODO: Deliver buttons via a systematic API.
   *
   * TODO: Redraw requests made only at state changes on motion over hovered reactives
   *       Plan: interaction between the hovered reactive and EventHandler
   */

  /**
   * A `GenericButton` keeps track of whether the cursor is hovering over it, and
   * whether it has been pressed but not released. If the mouse is pressed and then released
   * it invokes `react()`. If the mouse is pressed over it, but released elsewhere,
   * then there is no reaction.
   */
  abstract class GenericButton extends ReactiveGlyph {

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
        markStateChanged // if (hierarchical) reDraw() else window.requestFrame()
      }
    }

    private var _onGlyphEvent: Option[(Boolean, Vec)=>Unit] = None

    /**
     * Set or clear the action to be invoked when the cursor enters or leaves a reactive glyph.
     */
    def onGlyphEvent(action: ((Boolean, Vec) => Unit)=null): Unit = {
      _onGlyphEvent = if (action eq null) None else Some(action)
    }

    /**
     * Set or clear a glyph that could be shown on entry to the reactive.
     */
    protected var _hint: Option[Glyph] = None
    def setHint(hint: Glyph = null): Unit =
      _hint = if (hint eq null) None else Some(hint)

    override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {
      import Modifiers._
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

        _onGlyphEvent match {
          case None =>
          case Some(action) => action(hovered, location)
        }
        markStateChanged
      }
      //if (hierarchical) reDraw() else window.requestFrame()
    }
  }

  object ColourButton {
    val up:    Brush = Brushes.buttonForeground()
    val down:  Brush = Brushes.buttonDown()
    val hover: Brush = Brushes.buttonHover()
    val bg:    Brush = Brushes.buttonBackground
    def apply(text: String, up: Brush=up, down: Brush=down, hover: Brush=hover, bg: Brush = bg, background: Boolean = true)(react: Reaction): ReactiveGlyph = {
        val glyph: Glyph = Brushes.buttonText(text).asGlyph(up, bg)
        new ColourButton(glyph, down, hover, background, react)
    }
    def apply(glyph: Glyph, down: Brush, hover: Brush, background: Boolean)(react: Reaction): ReactiveGlyph = {
      new ColourButton(glyph, down, hover, background, react)
    }
  }

  class ColourButton(appearance: Glyph, down: Brush, hover: Brush, val background: Boolean, val react: Reaction) extends GenericButton {
    override def toString: String =
      s"ColourButton($up, $down, $hover, $background)"

    val up: Brush           = if (background) appearance.bg else appearance.fg
    val currentBrush: Brush = up.copy()
    val glyph: Glyph        = if (background) appearance(bg=currentBrush) else appearance(fg=currentBrush)
    def setCurrentBrush(b: Brush): Unit = {
      currentBrush.color(b.color).width(b.strokeWidth)
    }

    locally { glyph.parent = this }

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p)

    val fg: Brush = glyph.fg
    val bg: Brush = glyph.bg

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
            case (false, true) => (hover, alphaHover)
            case (_, _)        => (up,    alphaUp)
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
    def copy(fg: Brush=this.fg, bg: Brush=this.bg): Glyph = new ColourButton(appearance(fg.copy(), bg.copy()), down, hover, background, react)

  }

  /**
   * A button that normally shows `up`; shows `down` when a mouse button is down
   * within it, and (when hovering) shows `up` displaced/dimmed as specified by its `hover`
   * property.
   */
  class RawButton(up: Glyph, down: Glyph, hover: Glyph, val fg: Brush, val bg: Brush,
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
    def alphaHover: Int = 0xFF
    def alphaDisabled: Int = 0x70

    val downOffset  = diagonal - down.diagonal - (extra scaled 0.5f)
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
     * The exact shape of the glyph  used to
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

    // The remaining button constructors are being phased out
    //

    object TextButton extends DefaultPaints {

      def apply(text: String, fg: Brush = Brushes.buttonForeground, bg: Brush = Brushes.buttonBackground, background: Boolean = true)(reaction: Reaction): ColourButton = {
        val up = Brushes.buttonText(text).asGlyph(fg, bg).enlarged(Brushes.upFrame.strokeWidth*4)
        // new RawButton(up, up(fg=red), up(fg=green), up.fg, up.bg, reaction)
        new ColourButton(up, red, green, background, reaction)
      }
    }

    object FramedButton extends DefaultPaints {

    import GlyphTransforms.Framed

    /** A framed button whose up, down, and hover glyphs look the same */
    def apply(up: Glyph)(reaction: Reaction): RawButton =
      new RawButton(
        Framed(Brushes.upFrame)(up),
        Framed(Brushes.downFrame)(up()),
        Framed(Brushes.hoverFrame)(up()), up.fg, up.bg, reaction)

    /** A framed button whose up, down, and hover glyphs are all `textlayout` */
    def apply(text: String, fg: Brush=Brushes.buttonForeground, bg: Brush=Brushes.buttonBackground)(reaction: Reaction): RawButton = {
      val up = Brushes.buttonText(text).asGlyph(fg, bg).enlarged(Brushes.upFrame.strokeWidth*4)
      new RawButton(
        Framed(Brushes.upFrame)(up),
        Framed(Brushes.downFrame)(up()),
        Framed(Brushes.hoverFrame)(up()), up.fg, up.bg, reaction)
    }

    /**
     *   A framed button whose up, down, and hover glyphs are all `up`, and whose action is
     *   independent of the state of the keyboard modifiers or the mouse button that was pressed.
     */
    def apply(up: Glyph)(action: => Unit): RawButton = new RawButton(
      Framed(Brushes.upFrame)(up),
      Framed(Brushes.downFrame)(up()),
      Framed(Brushes.hoverFrame)(up()), up.fg, up.bg, { _ => action })

  }

  object ShadedButton extends DefaultPaints {

    import Glyphs.Shaded._

    /** A button with a shaded presentation, and `textlayout` as its caption */
    def ofString(text: String, fg: Brush = Brushes.buttonForeground, bg: Brush = Brushes.buttonBackground, delta: GlyphTypes.Scalar=4f)(reaction: Reaction): RawButton = {
      val up    = Static(fg, bg, delta, false)(Brushes.buttonText(text).asGlyph(fg, bg))
      val down  = Static(fg, bg, delta, true)(Brushes.buttonText(text).asGlyph(fg, bg))
      val hover = up.copy()
      RawButton(up, down, hover) (reaction)
    }

    /** A button with a shaded presentation, and `glyph` as its caption */
    def ofGlyph(glyph: Glyph, fg: Brush = Brushes.buttonForeground, bg: Brush = Brushes.buttonBackground, delta: GlyphTypes.Scalar = 4f)(reaction: Reaction): RawButton = {
      val up = Static(fg, bg, delta, false)(glyph)
      val down = Static(fg, bg, delta, true)(glyph)
      val hover = up.copy()
      RawButton(up, down, hover)(reaction)
    }

    /** A textlayout-labelled shaded button whose reaction is independent of modifiers.  */
    def apply(text: String, fg: Brush = Brushes.buttonForeground, bg: Brush = Brushes.buttonBackground, delta: GlyphTypes.Scalar = 4f)(reaction: Reaction): RawButton = {
      val up = Static(fg, bg, delta, false)(Brushes.buttonText(text).asGlyph(fg, bg).enlarged(delta))
      val down = Static(fg, bg, delta, true)(Brushes.buttonText(text).asGlyph(fg, bg).enlarged(delta))
      val hover = up.copy()
      RawButton(up, down, hover) { reaction }
    }

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

    val fg: Brush = glyph.fg
    val bg: Brush = glyph.bg

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
