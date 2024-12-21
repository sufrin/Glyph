package org.sufrin.glyph

import GlyphTypes.{Font, FontStyle, Scalar}
import Styles.Decoration.{Framed, Unframed}
import Styles.{ButtonStyle, CheckboxStyle, Decoration, GlyphStyle, MenuStyle, ToggleStyle}

case class Sheet
( fontScale: Scalar = 1.0f,
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
  buttonBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF), // transparent
  buttonForegroundBrush: Brush = Brush("buttonForeground")(color=0xFF0000FF), // blue
  buttonHoverBrush: Brush = Brush("buttonHover")(color=0xFF00FF00),           // green
  buttonDownBrush: Brush = Brush("buttonDown")(color=0xFFFF0000),             // red
  toggleBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF), // transparent
  toggleOnBrush: Brush = Brush("toggleOn")(color=0xFFFF0000),                 // red
  toggleOffBrush: Brush = Brush("toggleOn")(color=0xFF0000FF),                // blue
  labelBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF),  // transparent
  labelForegroundBrush: Brush = Brush("buttonForeground")(color=0xFF0000FF),  // blue
  textBackgroundBrush: Brush = Brush("textBackground")(color=0X00FFFFFF),     // transparent
  textForegroundBrush: Brush = Brush("textForeground")(color=0xFF0000FF),     // blue
  backgroundBrush: Brush = Brush("background")(color=0XFFBBBBBB),
  // Paragraph layout properties
  parAlign:    Alignment = Left,
  parSkip:     Scalar    = 5f,
  parWidth:    Scalar    = 200f,
  leftMargin:  Scalar=0f,
  rightMargin: Scalar=0f,
  parIndent:   Scalar=0f,
  padX: Scalar = 0f,
  padY: Scalar = 0f,
  buttonFrame: Styles.Decoration.Decoration = Styles.Decoration.Unframed,
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

  def parNarrow(left: Scalar, right: Scalar): Sheet = {
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
  def withButtonFrame(frame: Styles.Decoration.Decoration = Decoration.Unframed): Sheet = copy(buttonFrame=frame)

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
  lazy val baseLine: Scalar = {
    val m = textFont.getMetrics
    m.getHeight+m.getDescent
  }

  @inline private def styled(fontStyle: FontStyle): Sheet = copy(textFontStyle = fontStyle)
  def italicStyle: Sheet = styled(FontStyle.ITALIC)
  def boldStyle: Sheet = styled(FontStyle.BOLD)
  def boldItalicStyle: Sheet = styled(FontStyle.BOLD_ITALIC)
  def normalStyle: Sheet = styled(FontStyle.NORMAL)

}



