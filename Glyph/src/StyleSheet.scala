package org.sufrin
package glyph

import org.sufrin.glyph.GlyphTypes.{Font, FontStyle, Scalar}
import org.sufrin.glyph.styles._

case class StyleSheet
(fontScale: Scalar = 1.0f,
 textFontFamily: FontFamily  = FontFamily(),
 textFontStyle: FontStyle = FontStyle.NORMAL,
 textFontSize: Scalar  = 22f,
 labelFontFamily: FontFamily  = FontFamily(),
 labelFontStyle: FontStyle = FontStyle.NORMAL,
 labelFontSize: Scalar  = 22f,
 cdataFontFamily: FontFamily  = FontFamily("Courier"),
 cdataFontStyle: FontStyle = FontStyle.NORMAL,
 cdataFontSize: Scalar  = 22f,
 buttonFontFamily: FontFamily  = FontFamily(),
 buttonFontStyle: FontStyle = FontStyle.NORMAL,
 buttonFontSize: Scalar  = 22f,
 buttonBorderBrush: Brush = Brushes("red.5.round.stroke"),
 buttonBackgroundBrush: Brush = Brushes("transparent"),
 buttonForegroundBrush: Brush = Brushes("blue"),
 buttonHoverBrush: Brush = Brushes("green"),
 buttonDownBrush: Brush = Brushes("red"),
 toggleBackgroundBrush: Brush = Brushes("transparent"),
 toggleOnBrush: Brush = Brushes("blue"),
 toggleOffBrush: Brush = Brushes("blue"),
 labelBackgroundBrush: Brush = Brushes("transparent"),
 labelForegroundBrush: Brush = Brushes("blue"),
 cdataBackgroundBrush: Brush = Brushes("transparent"),
 cdataForegroundBrush: Brush = Brushes("black"),
 textBackgroundBrush: Brush = Brushes("transparent"),
 textForegroundBrush: Brush = Brushes("blue"),
 popupBackgroundBrush: Brush = Brushes("white"),
 popupForegroundBrush: Brush = Brushes("transparent"),
 backgroundBrush: Brush = Brushes("lightGrey"),
 foregroundBrush: Brush = Brushes("darkGrey"),
 // Paragraph layout properties
 parAlign:    Alignment = Left,
 parSkip:     Scalar    = 5f,
 parWidth:    Scalar    = 200f,
 leftMargin:  Scalar=0f,
 rightMargin: Scalar=0f,
 parIndent:   Scalar=0f,
 padX: Scalar = 0f,
 padY: Scalar = 0f,
 buttonDecoration: styles.decoration.Decoration = styles.decoration.unDecorated,

 // Dimensions that are sometimes provided by parent/root glyphs
 containerDiagonal: Vec = Vec.Zero,
 windowDiagonal:    Vec = Vec.Zero,
 screenDiagonal:    Vec = Vec.Zero,

 //
 discretionaryWordBreak: String = "_"
) {

  val containerWidth:   Scalar  = containerDiagonal.x
  val containerHeight:  Scalar  = containerDiagonal.y
  val windowWidth:      Scalar  = windowDiagonal.x
  val windowHeight:     Scalar  = windowDiagonal.y
  val screenWidth:      Scalar  = screenDiagonal.x
  val screenHeight:     Scalar  = screenDiagonal.y

  val toggleOn = new GlyphColours {
    val fg: Brush = toggleOnBrush;
    val bg: Brush = toggleBackgroundBrush
  }

  val toggleOff = new GlyphColours {
    val fg: Brush = toggleOffBrush;
    val bg: Brush = toggleBackgroundBrush
  }

  def labelFont: Font = labelFontFamily.makeFont(labelFontStyle, labelFontSize*fontScale)
  def cdataFont: Font = cdataFontFamily.makeFont(cdataFontStyle, cdataFontSize*fontScale)
  def textFont: Font = textFontFamily.makeFont(textFontStyle, textFontSize*fontScale)
  def buttonFont: Font = buttonFontFamily.makeFont(buttonFontStyle, buttonFontSize*fontScale)
  def buttonBorderWidth: Scalar = buttonBorderBrush.strokeWidth

  def parNarrow(left: Scalar, right: Scalar): StyleSheet = {
    val lm = leftMargin
    val rm = rightMargin
    logging.Default.info(s"parNarrow($left,$right) => ${(lm + left, rm+right)}")
    copy(leftMargin = lm + left, rightMargin = rm + right)
  }

  // derived
  lazy val labelStyle: GlyphStyle = GlyphStyle(labelFont, labelForegroundBrush, labelBackgroundBrush)

  lazy val buttonStyle: ButtonStyle = ButtonStyle(
    up     = GlyphStyle(font = buttonFont, fg = buttonForegroundBrush, bg = buttonBackgroundBrush),
    down   = GlyphStyle(font = buttonFont, fg = buttonDownBrush, bg = buttonBackgroundBrush),
    hover  = GlyphStyle(font = buttonFont, fg = buttonHoverBrush, bg = buttonBackgroundBrush),
    frame  = buttonDecoration,
    border = 10f,
    toggle = ToggleStyle(on = toggleOn, off = toggleOff),
    checkbox = CheckboxStyle(tick = "✔", cross = "✖", on = toggleOn, off = toggleOff)
  )

  lazy val menuStyle: MenuStyle = MenuStyle(
    button = buttonStyle,
    nestedButton = buttonStyle.copy(frame = decoration.Edged(fg = Brushes.black(width = 0))),
    reactive = buttonStyle.copy(frame = decoration.Edged(fg = Brushes.black(width = 0))),
    inactive = decoration.Edged(fg = Brushes.black(width = 1)),
    bg = Brushes.lightGrey,
    fg = Brushes.lightGrey,
  )

  lazy val emWidth: Scalar = textFont.measureTextWidth("m") // textFont.getMetrics.getMaxCharWidth//
  lazy val exHeight: Scalar = textFont.getMetrics.getXHeight //textFont.measureText("X").getHeight
  lazy val baseLine: Scalar = {
    val m = textFont.getMetrics
    m.getHeight+m.getDescent
  }
  def em: Glyph = new FixedSize.Space(emWidth, 1f, 0, 0)
  def ex: Glyph = new FixedSize.Space(1f, exHeight, 0, 0)

  def  hFill(ems: Int=1, stretch: Scalar=1): Glyph = new FixedSize.Space(ems*emWidth, exHeight, stretch, 0)
  def  vFill(exs: Int=1, stretch: Scalar=1): Glyph = new FixedSize.Space(emWidth, exs*exHeight, 0, stretch)
  def  hSpace(ems: Int=1): Glyph = new FixedSize.Space(ems*emWidth, exHeight, 0, 0)
  def  vSpace(exs: Int=1): Glyph = new FixedSize.Space(emWidth, exs*exHeight, 0, 0)

  lazy val interWordWidth: Scalar = emWidth*0.4f
  def  textWordFill(stretch: Scalar): Glyph = new FixedSize.Space(interWordWidth, exHeight, stretch, 0)

  @inline private def styled(fontStyle: FontStyle): StyleSheet =
    copy(
      textFontStyle = fontStyle,
      labelFontStyle  = fontStyle,
      buttonFontStyle  = fontStyle
    )

  /** This `StyleSheet`, with text, label, and button font styles italic */
  def italicStyle: StyleSheet = styled(FontStyle.ITALIC)
  /** This `StyleSheet`, with text, label, and button font styles bold */
  def boldStyle: StyleSheet = styled(FontStyle.BOLD)
  /** This `StyleSheet`, with text, label, and button font styles bold italic */
  def boldItalicStyle: StyleSheet = styled(FontStyle.BOLD_ITALIC)
  /** This `StyleSheet`, with text, label, and button font styles "normal" */
  def normalStyle: StyleSheet = styled(FontStyle.NORMAL)

  /**
   * Set all font sizes to `size`
   */
  def fontSizes(size: Scalar): StyleSheet =
    copy(
      textFontSize    = size,
      labelFontSize   = size,
      buttonFontSize  = size
  )

}



