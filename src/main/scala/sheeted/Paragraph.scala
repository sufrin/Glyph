package org.sufrin.glyph
package sheeted

import GlyphTypes.{Font, Scalar}
import DynamicGlyphs.ActiveGlyph

import org.sufrin.glyph.DefaultBrushes.{blue, nothing}
import org.sufrin.glyph.NaturalSize.Col

import scala.collection.mutable.ListBuffer


/**
 * A paragraph of width `ems` ems, formed by `text` using `style` to determine font. The words of the paragraph
 * are separated by sequences of space or newline.
 */
object Paragraph {
  def apply(ems: Scalar, align: Alignment)(text: String)(implicit style: Sheet): Glyph = {
    val localSheet = style.copy(parWidth = ems*style.emWidth, parAlign = align)
    val glyphs = text.split("[\n ]+").filter(_.nonEmpty).toList.flatMap{
      w => List(Text(w, style.textFont, style.textForegroundBrush, style.textBackgroundBrush), new FixedSize.Space(style.interWordWidth, 1f, 1f, 0f))
    }
    glyphXML.Paragraph.fromGlyphs(localSheet, glyphs, None)
  }

}


/**
 * An active paragraph of width `ems` ems, initially formatted from by `text` using `style` to determine font.
 * It is re-formatted whenever its `set(text)` method is invoked.
 */
class ActiveParagraph(ems: Scalar, align: Alignment, text: String)(implicit style: Sheet)
  extends  ActiveGlyph[String](text, Paragraph(ems, align)(text: String)(style)) {
  def toGlyph(text: String): Glyph = Paragraph(ems: Scalar, align: Alignment)(text)(style)
  override def copy(fg: Brush, bg: Brush): ActiveGlyph[String] = new ActiveParagraph(ems, align, text)(style)
}

/**
 * An active paragraph of width `ems` ems, initially formatted from by `text` using `style` to determine font.
 * It is re-formatted whenever its `set(text)` method is invoked.
 */
object ActiveParagraph {
  def apply(ems: Scalar, align: Alignment)(text: String)(implicit style: Sheet): ActiveParagraph = new ActiveParagraph(ems, align, text)(style)
}
