package org.sufrin.glyph
package styled

import GlyphTypes.Scalar
import unstyled.dynamic.ActiveGlyph


/**
 * A paragraph of width `ems` ems, formed by `text` using `style` to determine font. The words of the paragraph
 * are separated by sequences of space or newline.
 *
 * @see glyphML.Paragraph.fromGlyphs
 */
object Paragraph {
  def apply(ems: Scalar, align: Alignment, parHang: Glyph=null)(text: String)(implicit style: StyleSheet): Glyph = {
    val localSheet = style.copy(parWidth = ems*style.emWidth, parAlign = align)
    val glyphs = text.split("[\n ]+").filter(_.nonEmpty).toList.flatMap{
      w => List(unstyled.Text(w, style.textFont, style.textForegroundBrush, style.textBackgroundBrush), new FixedSize.Space(style.interWordWidth, 1f, 1f, 0f))
    }
    glyphML.Paragraph.fromGlyphs(localSheet, glyphs, if (parHang==null) None else Some(parHang))
  }

}


/**
 * An active paragraph of width `ems` ems, initially formatted from by `text` using `style` to determine font.
 * It is re-formatted whenever its `set(text)` method is invoked. If `parHang` is provided then it is "hung" to
 * * the top-left of the paragraph.
 */
class ActiveParagraph(ems: Scalar, align: Alignment, parHang: Glyph, text: String)(implicit style: StyleSheet)
  extends  ActiveGlyph[String](text, Paragraph(ems, align)(text: String)(style)) {
  def toGlyph(text: String): Glyph = Paragraph(ems: Scalar, align: Alignment)(text)(style)
  override def copy(fg: Brush, bg: Brush): ActiveGlyph[String] = new ActiveParagraph(ems, align, parHang, text)(style)
}

/**
 * An active paragraph of width `ems` ems, initially formatted from by `text` using `style` to determine font.
 * It is re-formatted whenever its `set(text)` method is invoked. If `parHang` is provided then it is "hung" to
 * the top-left of the paragraph.
 */
object ActiveParagraph {
  def apply(ems: Scalar, align: Alignment, parHang: Glyph=null)(text: String)(implicit style: StyleSheet): ActiveParagraph = new ActiveParagraph(ems, align, parHang, text)(style)
}
