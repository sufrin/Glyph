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
    def toGlyph(string: String, fg: Brush = fg, bg: Brush = bg): Glyph =
      if (true) unstyled.Label(string, font, fg, bg, Center) else unstyled.Text(string, font, fg, bg)

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

