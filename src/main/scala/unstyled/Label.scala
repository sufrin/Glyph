package org.sufrin.glyph
package unstyled
import GlyphTypes._

object Label {
  def apply(text: String, font: Font=Brushes.buttonFont, fg: Brush=Brushes.black, bg: Brush=Brushes.nothing, align: Alignment=Center): Glyph = {
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
