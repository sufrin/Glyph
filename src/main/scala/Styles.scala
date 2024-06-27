package org.sufrin.glyph

/**
 * Styles make it straightforward to provide stylistic uniformity for
 * GUIs whose components take implicit context parameters.
 *
 * This "high level" notion is an alternative to the "low level"
 * provision of default foreground, background, and font parameters.
 *
 * TODO: this feature is in flux.
 *
 * TODO: change all `Style` traits to case classes, thereby making
 *       copying-with-changes less tedious to implement for new styles.
 *
 * TODO: this feature seems to be heading inexorably towards something
 *       along the lines of "cascading". We'd like it to be convenient
 *       to specify the "collection of styles" to be used in instantiating
 *       a styled component. Perhaps the implicit parameter to a styled
 *       component should simply refer to a (named) collecton of styles; and
 *       there should be a global mapping.
 *
 * @see Styled
 */
object Styles {

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

    /** An `emWidth/2` stretchable space */
    lazy val fill: Glyph = new FixedSize.Space(emWidth / 2, 0f, 1f, 0f)

    /** An `emWidth` space (height 1) */
    lazy val em: Glyph = new FixedSize.Space(emWidth, 1f, 0f, 0f)

    /** An `exHeight` space (width 1) */
    lazy val ex: Glyph = new FixedSize.Space(1f, exHeight, 0f, 0f)
  }

  /**
   * Decorations applied to button-like glyphs
   */
  object Decoration {
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
     * @param radiusFactor (if `==0`) a rectangular frame; otherwise the `radiusFactor` of the corner curves
     * @see GlyphTransforms.Framed
     */
    case class Framed(fg: Brush, bg: Brush, enlarge: Scalar = 0.25f, radiusFactor: Scalar = 0f) extends Decoration {
      def decorate(glyph: Glyph): Glyph =
        glyph.enlarged(if (enlarge < 1f) enlarge * (glyph.w min glyph.h) else enlarge).framed(fg, bg, radiusFactor)
    }

    /**
     * Decorate the frame with shading
     * else the top left corner. The enlargement is `enlarge` itself if `enlarge>1`, otherwise `enlarge * (thisGlyph.w min thisGlyph.h)`
     *
     * @param fg      foreground of the shading
     * @param bg      background of the shading
     * @param enlarge (if `<1`) multiple of the smaller of the two glyph dimensions to enlarge the glyph by; otherwise absolute value to enlarge the glyph by.
     * @param delta   width of the shading
     * @param down    if true the frame is around the bottom right, else it is around the top left
     * @see Glyph.shaded
     * @see Glyphs.Shaded
     */
    case class Shaded(fg: Brush, bg: Brush, enlarge: Scalar = 0.25f, delta: Scalar = 8f, down: Boolean = false) extends Decoration {
      def decorate(glyph: Glyph): Glyph = glyph.shaded(fg, bg, enlarge, delta, down)
    }

    case class Blurred(fg: Brush, bg: Brush = Glyphs.nothing, blur: Scalar, spread: Scalar, delta: Scalar = 0f) extends Decoration {
      def decorate(glyph: Glyph): Glyph = Glyphs.BlurredFrame(blur, spread, fg, bg, dx = delta, dy = delta)(glyph)
    }

    /**
     * Leave the glyph undecorated.
     */
    case object Unframed extends Decoration {
      def decorate(glyph: Glyph): Glyph = glyph
    }
  }

  case class ButtonStyle
  (  up: GlyphStyle
   , down: GlyphStyle
   , hover: GlyphStyle
   , toggle: ToggleStyle
   , checkbox: CheckboxStyle
   , frame: Decoration.Decoration
   , border: Float
  ) {
    lazy val nested: ButtonStyle = this.copy(frame = Decoration.Unframed)
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
   , inactive: Decoration.Decoration
   , bg: Brush
   , fg: Brush
  )

  case class GlyphButtonStyle
  (frame: Decoration.Decoration,
   border: Scalar
  )

  /**
   * Stylesheet with default values for all its features.
   */
  trait DefaultSheet extends StyleSheet {

    import GlyphTypes.{FontManager, FontStyle, Typeface}

    object Brushes extends Brushes {}
    import Brushes._
    import Decoration._

    /** Default: can be overridden */
    def face: Typeface = FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL)
    /** Default: can be overridden */
    def buttonFontSize: Scalar = 22
    /** Default: can be overridden */
    def buttonFont: Font = new Font(face, buttonFontSize)

    def labelFontSize: Scalar = 20
    /** Default: can be overridden */
    def labelFont: Font = new Font(face, labelFontSize)

    /** Default: can be overridden */
    def buttonBorderWidth: Scalar = 5f
    /** Default: can be overridden */
    def buttonBorderColor: Int = 0xFF777777
    def buttonBackgroundColor: Int = 0xFFAAAAAA
    /** Default: can be overridden */
    def buttonBorderBrush: Brush = Brush("buttonBorder")(color = buttonBorderColor, width = buttonBorderWidth, cap = SQUARE)
    def buttonBackgroundBrush: Brush = Brush("buttonBackground")(color = buttonBackgroundColor)

    lazy val buttonStyle: ButtonStyle = {
      val colours = new GlyphColours {
        val fg: Brush = black
        val bg: Brush = nothing
      }
      val up: GlyphStyle = GlyphStyle(font = buttonFont, fg = blue, bg = nothing)
      val down: GlyphStyle = GlyphStyle(font = buttonFont, fg = red, bg = nothing)
      val hover: GlyphStyle = GlyphStyle(font = buttonFont, fg = green, bg = nothing)
      val frame: Decoration =
          Framed(fg = buttonBorderBrush, bg = buttonBackgroundBrush, radiusFactor = 0.5f)
      val border: Scalar = 6f
      val toggle: ToggleStyle = ToggleStyle(
        on = new GlyphColours {
          val fg: Brush = red;
          val bg: Brush = nothing
        },
        off = new GlyphColours {
          val fg: Brush = blue;
          val bg: Brush = nothing
        }
      )
      val checkbox: CheckboxStyle = CheckboxStyle(tick = "✔", cross = "✖", on = toggle.on, off = toggle.off)
      ButtonStyle(up = up, down = down, hover = hover, frame = frame, border = border, toggle = toggle, checkbox = checkbox)
    }

    def unFramed: StyleSheet = new Derived {
      override def buttonStyle: ButtonStyle = delegate.buttonStyle.nested
    }

    def menuStyle: MenuStyle = MenuStyle(
      button = buttonStyle,
      nestedButton = buttonStyle.copy(frame = Framed(fg = black(width = 0), bg = buttonBackgroundBrush)),
      reactive = buttonStyle.copy(frame = Framed(fg = black(width = 0), bg = buttonBackgroundBrush)),
      inactive = Unframed,
      bg = lightGrey,
      fg = lightGrey
    )

    def labelStyle: GlyphStyle = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)

    lazy val Spaces: Spaces = labelStyle
  }

  object Default extends DefaultSheet {}

}
