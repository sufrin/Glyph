package org.sufrin.glyph

import GlyphTypes.{Font, FontStyle, Scalar}
import styles.decoration.{Framed, Unframed}
import styles.{ButtonStyle, CheckboxStyle, decoration, GlyphStyle, MenuStyle, ToggleStyle}

case class StyleSheet
(fontScale: Scalar = 1.0f,
 textFontFamily: FontFamily  = FontFamily(),
 textFontStyle: FontStyle = FontStyle.NORMAL,
 textFontSize: Scalar  = 22f,
 labelFontFamily: FontFamily  = FontFamily(),
 labelFontStyle: FontStyle = FontStyle.NORMAL,
 labelFontSize: Scalar  = 22f,
 buttonFontFamily: FontFamily  = FontFamily(),
 buttonFontStyle: FontStyle = FontStyle.NORMAL,
 buttonFontSize: Scalar  = 22f,
 buttonBorderBrush: Brush = Brush("buttonBorder")(color=0XFF777777, width=5f),
 buttonBackgroundBrush: Brush = Brush("transparent")(color=0X00FFFFFF), // transparent
 buttonForegroundBrush: Brush = Brush("blue")(color=0xFF0000FF), // blue
 buttonHoverBrush: Brush = Brush("green")(color=0xFF00FF00), // green
 buttonDownBrush: Brush = Brush("buttonDown")(color=0xFFFF0000), // red
 toggleBackgroundBrush: Brush = Brush("red")(color=0X00FFFFFF), // transparent
 toggleOnBrush: Brush = Brush("red")(color=0xFFFF0000), // red
 toggleOffBrush: Brush = Brush("blue")(color=0xFF0000FF), // blue
 labelBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF), // transparent
 labelForegroundBrush: Brush = Brush("blue")(color=0xFF0000FF), // blue
 textBackgroundBrush: Brush = Brush("transparent")(color=0X00FFFFFF), // transparent
 textForegroundBrush: Brush = Brush("blue")(color=0xFF0000FF), // blue
 backgroundBrush: Brush = Brush("background")(color=0XFFBBBBBB),
 foregroundBrush: Brush = Brush("foreground")(color=0XFFBBBBBB),
 // Paragraph layout properties
 parAlign:    Alignment = Left,
 parSkip:     Scalar    = 5f,
 parWidth:    Scalar    = 200f,
 leftMargin:  Scalar=0f,
 rightMargin: Scalar=0f,
 parIndent:   Scalar=0f,
 padX: Scalar = 0f,
 padY: Scalar = 0f,
 buttonFrame: styles.decoration.Decoration = styles.decoration.Unframed,
 // Container constraints
 containerDimension: Vec = Vec.Zero,
 //
 discretionaryWordBreak: String = "_"
) {

  val toggleOn = new GlyphColours {
    val fg: Brush = toggleOnBrush;
    val bg: Brush = toggleBackgroundBrush
  }

  val toggleOff = new GlyphColours {
    val fg: Brush = toggleOffBrush;
    val bg: Brush = toggleBackgroundBrush
  }

  def labelFont: Font = labelFontFamily.makeFont(labelFontStyle, labelFontSize*fontScale)
  def textFont: Font = textFontFamily.makeFont(textFontStyle, textFontSize*fontScale)
  def buttonFont: Font = buttonFontFamily.makeFont(buttonFontStyle, buttonFontSize*fontScale)
  def buttonBorderWidth: Scalar = buttonBorderBrush.strokeWidth

  def parNarrow(left: Scalar, right: Scalar): StyleSheet = {
    val lm = leftMargin
    val rm = rightMargin
    org.sufrin.logging.Default.info(s"parNarrow($left,$right) => ${(lm + left, rm+right)}")
    copy(leftMargin = lm + left, rightMargin = rm + right)
  }

  // derived
  lazy val labelStyle: GlyphStyle = GlyphStyle(labelFont, labelForegroundBrush, labelBackgroundBrush)

  lazy val buttonStyle: ButtonStyle = ButtonStyle(
    up     = GlyphStyle(font = buttonFont, fg = buttonForegroundBrush, bg = buttonBackgroundBrush),
    down   = GlyphStyle(font = buttonFont, fg = buttonDownBrush, bg = buttonBackgroundBrush),
    hover  = GlyphStyle(font = buttonFont, fg = buttonHoverBrush, bg = buttonBackgroundBrush),
    frame  = buttonFrame,
    border = 6f,
    toggle = ToggleStyle(on = toggleOn, off = toggleOff),
    checkbox = CheckboxStyle(tick = "✔", cross = "✖", on = toggleOn, off = toggleOff)
  )

  /** A stylesheet derived from this one, but with button framing specified by `border`, and decor`. */
  def withButtonFrame(frame: styles.decoration.Decoration = decoration.Unframed): StyleSheet = copy(buttonFrame=frame)

  lazy val menuStyle: MenuStyle = MenuStyle(
    button = buttonStyle,
    nestedButton = buttonStyle.copy(frame = Framed(fg = DefaultBrushes.black(width = 0), bg = buttonBackgroundBrush)),
    reactive = buttonStyle.copy(frame = Framed(fg = DefaultBrushes.black(width = 0), bg = buttonBackgroundBrush)),
    inactive = Unframed,
    bg = DefaultBrushes.lightGrey,
    fg = DefaultBrushes.lightGrey,
  )

  lazy val emWidth: Scalar = textFont.measureTextWidth("m") // textFont.getMetrics.getMaxCharWidth//
  lazy val exHeight: Scalar = textFont.getMetrics.getXHeight //textFont.measureText("X").getHeight
  lazy val interWordWidth: Scalar = emWidth*0.4f
  lazy val baseLine: Scalar = {
    val m = textFont.getMetrics
    m.getHeight+m.getDescent
  }
  lazy val em: Glyph = new FixedSize.Space(emWidth, 1f, 0, 0)
  lazy val ex: Glyph = new FixedSize.Space(1f, exHeight, 0, 0)

  @inline private def styled(fontStyle: FontStyle): StyleSheet = copy(textFontStyle = fontStyle)
  def italicStyle: StyleSheet = styled(FontStyle.ITALIC)
  def boldStyle: StyleSheet = styled(FontStyle.BOLD)
  def boldItalicStyle: StyleSheet = styled(FontStyle.BOLD_ITALIC)
  def normalStyle: StyleSheet = styled(FontStyle.NORMAL)

}



