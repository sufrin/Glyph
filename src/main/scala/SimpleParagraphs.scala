package org.sufrin.glyph

import DefaultBrushes.{blue, nothing}
import GlyphTypes.Font
import NaturalSize.Col

import scala.collection.mutable.ListBuffer

object SimpleParagraphs {
  /**
   * A glyph formed of the paragraphs of `text`. Paragraphs are separated by blank lines; lines within each paragraph have no more than
   * `ems` characters on them. Each line has the alignment specified by `align`. If the first word of a paragraph starts with
   * [C] then the rest of that paragraph is centered, whatever the specified `align`. Likewise, if it starts with [R] the rest of the
   * paragraph is right-aligned.
   */
  def apply(ems: Int, font: Font = DefaultBrushes.buttonFont, fg: Brush = blue, align: Alignment=Left)(text: String): Glyph = {
    val emWidth = Text("n", font).w

    def paragraphLines(text: String): Seq[Glyph] = {
      val words = new StreamIterator[String](text.split("""[\n ]+""").iterator)
      val result = ListBuffer[Glyph]()
      val line = new StringBuilder()
      var alignment = align
      while (words.hasElement) {
        // first word of the paragraph may specify centered alignment
        if (words.hasElement && (words.element.startsWith("[C]") || words.element.startsWith("[R]"))) {
          alignment=words.element.substring(0, 3) match {
            case "[C]" => Center
            case "[R]" => Right
          }
          words.update(words.element.substring(3))
        }
        while (words.hasElement && line.length + words.element.length + 1 < ems) {
          line.append(words.element); line.append(' ')
          words.nextElement()
        }
        alignment match {
          case Center =>
              result.addOne(FixedSize.Row(width=ems*emWidth)(FixedSize.Space(1,1,1), Text(line.toString, font, fg), FixedSize.Space(1,1,1)))
          case Right =>
              result.addOne(FixedSize.Row(width=ems*emWidth)(FixedSize.Space(1,1,1), Text(line.toString, font, fg)))
          case  _ =>
              result.addOne(Text(line.toString, font, fg))
        }

        line.clear()
      }
      result.addOne(Text(" ", font, fg))
      result.toSeq
    }

    val paras = for { par <- text.split("\n\n+").toList } yield Col(align=align)(paragraphLines(par))

    Col(align=Left, bg=nothing)(paras)
  }
}
