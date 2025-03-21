package org.sufrin
package glyph

import GlyphTypes.{Font, FontStyle, Scalar}
import styles.decoration.{Framed, unDecorated}
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
 buttonBorderBrush: Brush = Brush("buttonBorder")(color=0XFF777777, width=5f, cap=Brushes.ROUND),
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
 popupBackgroundBrush: Brush = Brush("popupbackground") color 0xffeeeeee,
 popupForegroundBrush: Brush = Brush("popupforeground") color 0xff000000 strokeWidth(1),
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
    border = 6f,
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
  lazy val interWordWidth: Scalar = emWidth*0.4f
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

  @inline private def styled(fontStyle: FontStyle): StyleSheet = copy(textFontStyle = fontStyle)
  def italicStyle: StyleSheet = styled(FontStyle.ITALIC)
  def boldStyle: StyleSheet = styled(FontStyle.BOLD)
  def boldItalicStyle: StyleSheet = styled(FontStyle.BOLD_ITALIC)
  def normalStyle: StyleSheet = styled(FontStyle.NORMAL)

}



