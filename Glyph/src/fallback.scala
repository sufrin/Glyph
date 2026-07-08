package org.sufrin.glyph

/**
 * Default attributes for some unstyled glyphs. These are
 * all derived from the current `StyleSheet`, which is initially the "stock"
 * stylesheet with default attributes. Changes can be "wholesale" or
 * differential.
 *
 * {{{
 *   fallback.StyleSheet =
 *      ... // another sheet
 *
 *   fallback.StyleSheet =
 *      fallback.StyleSheet(
 *           buttonDownBrush=Brushes.yellow,
 *           buttonBackgroundBrush=Brushes.lightGrey
 *      )
 * }}}
 *
 */
object fallback {


  private var sheet: StyleSheet = org.sufrin.glyph.StyleSheet()
  def StyleSheet_(sheet: StyleSheet): Unit = this.sheet=sheet
  def StyleSheet: StyleSheet = this.sheet

  def upFrame: Brush = sheet.buttonForegroundBrush(width=2)
  def downFrame: Brush = sheet.buttonDownBrush(width=2)
  def hoverFrame: Brush = sheet.buttonHoverBrush(width=2)

  def buttonFamily: FontFamily = sheet.buttonFontFamily         // was FontFamily("Menlo")
  def buttonPointSize: Float = sheet.buttonFontSize             // was 22.0f
  def buttonFont = sheet.buttonFont            // was buttonFamily.makeFont(GlyphTypes.FontStyle.NORMAL, buttonPointSize)
  def buttonText(s: String, fg: Brush=buttonForeground, bg: Brush=buttonBackground): unstyled.Text = unstyled.Text(s, buttonFont, fg, bg)

  def buttonForeground: Brush = sheet.buttonForegroundBrush
  def buttonBackground: Brush = sheet.buttonBackgroundBrush
  def buttonDown:       Brush = sheet.buttonDownBrush
  def buttonHover:      Brush = sheet.buttonHoverBrush

  def textFamily: FontFamily = sheet.textFontFamily // was FontFamily("Menlo")
  def textPointSize: Float = sheet.textFontSize     // was 22.0f
  def textFont = sheet.textFont    // was textFamily.makeFont(GlyphTypes.FontStyle.NORMAL, textPointSize)
  def textForeground: Brush = sheet.textForegroundBrush
  def textBackground: Brush = sheet.textBackgroundBrush


}
