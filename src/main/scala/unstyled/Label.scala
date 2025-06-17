package org.sufrin.glyph
package unstyled
import GlyphTypes._

object Label {
  /** A (possibly multi-line) label in the given font.
   *
   * @param text
   * @param font
   * @param fg colour of the text
   * @param bg colour of the background
   * @param align alignment of the lines (if >1) of the text.
   * @return
   */
  def apply(text: String, font: Font=fallback.textFont, fg: Brush=fallback.textForeground, bg: Brush=Brushes.transparent, align: Alignment=Center): Glyph = {
    import NaturalSize.Col
    val lines = text.split('\n').toList
    lines.length match {
      case 1 => unstyled.Text(text, font, fg, bg)
      case _ => {
        val texts = lines.map { line => unstyled.Text(line, font, fg, bg) }
        Col(align=align, bg = bg)(texts)
      }
    }
  }
}
