package org.sufrin.glyph
package styles

/**
 * styles make it straightforward to provide stylistic uniformity for
 * GUIs whose components take implicit context parameters.
 *
 * This "high level" notion is an alternative to the "low level"
 * provision of default foreground, background, and font parameters.
 *
 * The StyleSheet class incorporates many of these individual
 * style components.
 *
 * @see StyleSheet
 * @see styled
 */


  import GlyphTypes.{Font, Scalar}

  /** How to style a toggle: on and off colours */
  case class ToggleStyle
  (on: GlyphColours,
   off: GlyphColours
  )

  case class CheckboxStyle
  (tick: String,
   cross: String,
   on: GlyphColours,
   off: GlyphColours
  )

  trait Spaces {
    val emWidth: Scalar
    val exHeight: Scalar
    val fill: Glyph
    val em: Glyph
    val ex: Glyph
  }

  /** Universal style for a glyph that has a foreground and a background (and may have a font) */
  case class GlyphStyle(font: Font, fg: Brush, bg: Brush) extends Spaces {
    def toGlyph(string: String, fg: Brush = fg, bg: Brush = bg): Glyph = Text(string, font).asGlyph(fg, bg)

    lazy val emWidth: Scalar = font.measureTextWidth("M")
    /** height of an X  in the button font */
    lazy val exHeight: Scalar = font.measureText("X").getHeight

    /** Space of dimension `emWidth * exHeight` */
    lazy val strut: Glyph = new FixedSize.Space(emWidth, exHeight, 0f, 0f)

    /** An `emWidth/2` stretchable space */
    lazy val fill: Glyph = new FixedSize.Space(emWidth / 2, 0f, 1f, 0f)

    /** An `emWidth` space (height 1) */
    lazy val em: Glyph = new FixedSize.Space(emWidth, 1f, 0f, 0f)

    /** An `exHeight` space (width 1) */
    lazy val ex: Glyph = new FixedSize.Space(1f, exHeight, 0f, 0f)
  }

  /**
   * Decorations normally applied to button-like `styled` glyphs and
   * other components as they are being constructed.
   */
  package decoration {
    /** The decoration applied to a glyph */
    trait Decoration {
      def decorate(glyph: Glyph): Glyph
    }

    /**
     * Decorate the glyph with a (possibly-curved) frame
     *
     * @param fg           foreground of the frame
     * @param bg           background of the frame
     * @param enlarge      (if `<1`) multiple of the smaller of the two glyph dimensions to enlarge the glyph by; otherwise absolute value to enlarge the glyph by.
     * @param radius (if `==0`) a rectangular frame; otherwise the `radius` of the corner curves
     * @see GlyphTransforms.Framed
     */
    case class Framed(fg: Brush=DefaultBrushes.black, bg: Brush=DefaultBrushes.nothing, enlarge: Scalar = 0.15f, radius: Scalar = 0f) extends Decoration {
      def decorate(glyph: Glyph): Glyph =
        glyph.enlarged(if (enlarge < 1f) enlarge * (glyph.w min glyph.h) else enlarge).framed(fg, bg, radius)
    }

    /** Decorate the glyph with `.enlarge(enlargement).edged(fg, bg)`. The `enlargement` is enlarge` itself if `enlarge>1`,
     * otherwise `enlarge * (thisGlyph.w min thisGlyph.h)`
     */
    case class Edged(fg: Brush=DefaultBrushes.black, bg: Brush=DefaultBrushes.nothing, enlarge: Scalar = 0.15f, radiusFactor: Scalar = 0f) extends Decoration {
      def decorate(glyph: Glyph): Glyph =
        glyph.enlarged(if (enlarge < 1f) enlarge * (glyph.w min glyph.h) else enlarge).edged(fg, bg)
    }

    /**
     * Decorate the glyph with `.shaded(fg, bg, enlarge, delta, down)`. The pre-framing enlargement is `enlarge` itself if `enlarge>1`,
     * otherwise `enlarge * (thisGlyph.w min thisGlyph.h)`
     *
     * @param fg      foreground of the shading
     * @param bg      background of the shading
     * @param enlarge (if `<1`) multiple of the smaller of the two glyph dimensions to enlarge the glyph by; otherwise absolute value to enlarge the glyph by.
     * @param delta   width of the shading
     * @param down    if true the frame is around the bottom right, else it is around the top left
     * @see Glyph.shaded
     * @see Glyphs.Shaded
     */
    case class Shaded(fg: Brush=DefaultBrushes.black, bg: Brush=DefaultBrushes.nothing, enlarge: Scalar = 0.15f, delta: Scalar = 8f, down: Boolean = false) extends Decoration {
      def decorate(glyph: Glyph): Glyph = glyph.shaded(fg, bg, enlarge, delta, down)
    }

    /** Decorated the `.enlarged(enlargement)` glyph with
     *
     * {{{ BlurredFrame(blur, spread, fg, bg, dx, dy) }}}
     *
     * The pre-decoration `enlargement` is enlarge` itself if
     * {{{ enlarge>1 }}}
     * otherwise it is {{{enlarge * (the glyph's width min the glyph's height)}}}
     */
    case class Blurred(fg: Brush=DefaultBrushes.black, bg: Brush = DefaultBrushes.nothing, blur: Scalar, spread: Scalar, delta: Scalar = 0f, enlarge: Scalar=0f) extends Decoration {

      def decorate(glyph: Glyph): Glyph = {
        val enlargement = if (enlarge < 1f) enlarge * (glyph.w min glyph.h) else enlarge
        Glyphs.BlurredFrame(blur, spread, fg, bg, dx = delta, dy = delta)(glyph enlarged enlargement)
      }
    }

    /**
     *  Decorated by a glyph transform
     */
    case class Decorated(transform: Glyph=>Glyph) extends Decoration {
      def decorate(glyph: Glyph): Glyph = transform(glyph)
    }

    /**
     * Leave the glyph undecorated.
     */
    case object unDecorated extends Decoration {
      def decorate(glyph: Glyph): Glyph = glyph
    }
  }

  case class ButtonStyle
  (  up: GlyphStyle
   , down: GlyphStyle
   , hover: GlyphStyle
   , toggle: ToggleStyle
   , checkbox: CheckboxStyle
   , frame: decoration.Decoration
   , border: Float
  ) {
    lazy val nested: ButtonStyle = this.copy(frame = decoration.unDecorated)
  }

  /**
   * Styling for a complete menu
   *
   * @param button       style for the menu's button
   * @param nestedButton style for the menu's button if the menu is nested
   * @param reactive     style for the menu's reactive entries
   * @param inactive     style for the menu's inactive entries
   */
  case class MenuStyle
  (  button: ButtonStyle
   , nestedButton: ButtonStyle
   , reactive: ButtonStyle
   , inactive: decoration.Decoration
   , bg: Brush
   , fg: Brush
  )

  case class GlyphButtonStyle
  (frame: decoration.Decoration,
   //border: Scalar
  )

