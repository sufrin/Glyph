package org.sufrin.glyph

import unstyled.Text

object fallback {

  //
  // The following are used to set the default attributes of some unstyled glyphs
  //
  private var sheet: StyleSheet = StyleSheet()
  def StyleSheet_(sheet: StyleSheet): Unit = this.sheet=sheet

  def upFrame: Brush = sheet.buttonForegroundBrush              // was Brush("black.2.round.fill")
  def downFrame: Brush = sheet.buttonDownBrush                  // was ("red.2.round.fill")
  def hoverFrame: Brush = sheet.buttonHoverBrush                // was ("green.2.round.fill")

  def buttonFamily: FontFamily = sheet.buttonFontFamily         // was FontFamily("Menlo")
  def buttonPointSize: Float = sheet.buttonFontSize             // was 22.0f
  def buttonFont = sheet.buttonFont            // was buttonFamily.makeFont(GlyphTypes.FontStyle.NORMAL, buttonPointSize)
  def buttonText(s: String, fg: Brush=buttonForeground, bg: Brush=buttonBackground): Text = Text(s, buttonFont, fg, bg)

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
