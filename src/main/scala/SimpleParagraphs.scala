package org.sufrin.glyph

import DefaultBrushes.{blue, nothing}
import GlyphTypes.Font
import NaturalSize.Col

import scala.collection.mutable.ListBuffer

object SimpleParagraphs {
  /**
   * A glyph formed of the paragraphs of `text` with blank lines between them.
   * No paragraph is wider than `ems` characters.
   *
   * The `text` is split into paragraphs at boundaries where more than a single newline appears.
   * The words of each paragraph are then used to fill lines of length no more than `ems`, and the
   * glyph that results is the Col composition of the Text formed of these lines.
   *
   * Normally each line has the alignment specified by `align`; but if the first word of a paragraph starts with
   * [C] then the rest of that paragraph is centered, whatever the specified `align`. Likewise, if it starts with [R] the rest of the
   * paragraph is right-aligned. The Justify alignment is equivalent to Left.
   */
  def apply(ems: Int, font: Font = DefaultBrushes.buttonFont, fg: Brush = blue, align: Alignment=Left)(text: String): Glyph = {
    val emWidth = Text("n", font).w

    def paragraphLines(text: String): Seq[Glyph] = {
      val words = Stream[String](text.split("""[\n ]+"""))
      val result = ListBuffer[Glyph]()
      val line = new StringBuilder()
      var alignment = if (align==Justify) Left else align
      while (words.hasElement) {
        // first word of the paragraph may specify centered or right alignment
        if (words.hasElement && (words.element.startsWith("[C]") || words.element.startsWith("[R]"))) {
          alignment=words.element.substring(0, 3) match {
            case "[C]" => Center
            case "[R]" => Right
          }
          words.setElement(words.element.substring(3))
        }
        while (words.hasElement && line.length + words.element.length + 1 < ems) {
          line.append(words.element); line.append(' ')
          words.nextElement()
        }
        val theText = Text(line.toString, font, fg)
        alignment match {
          case Center =>
              result.addOne(FixedSize.Row(width=ems*emWidth)(FixedSize.Space(1,1,1), theText, FixedSize.Space(1,1,1)))
          case Right =>
              result.addOne(FixedSize.Row(width=ems*emWidth)(FixedSize.Space(1,1,1), theText))
          case  _ =>
              result.addOne(theText)
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
