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
   * The `text` is split into paragraphs at boundaries where an empty or empty-looking line appears, or
   * where a line starts with "[".
   *
   * The words of each paragraph are then used to fill lines of length no more than `ems`, and the
   * glyph that results is the Col composition of the Text formed of these lines.
   *
   * Normally each line has the alignment specified by `align`; but if the first word of a paragraph starts with
   * [C] then the alignment  of the paragraph is Center. Likewise, if it starts with [R] the rest of the
   * paragraph is right-aligned. [I] indents the rest of the paragrap. [*] indents and bulletpoints
   * the rest of the paragraph.
   */
  def apply(ems: Int, font: Font = DefaultBrushes.buttonFont, fg: Brush = blue, align: Alignment=Left)(text: String): Glyph = {
    val emWidth = Text("n", font).w

    def paragraphLines(ems: Int, text: String, align: Alignment): Seq[Glyph] = {
      val words = Stream[String](text.trim.split("""[ ]+"""))
      val result = ListBuffer[Glyph]()
      val line = new StringBuilder()
      val alignment = if (align==Justify) Left else align
      while (words.hasElement) {
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
      result.toSeq
    }

    val result = ListBuffer[Glyph]()

    for { par <- text.split("\n\n+[ ]*|\n\\[").toList } {
        var content = par
        var indent = 0
        var alignment = align
        var formatting = true
        var skip = true

        while (formatting) {
          content match {
            case s"[*]$rest" => content = "* "+rest; indent = 5
            case s"[C]$rest" => content = rest; alignment = Center
            case s"[R]$rest" => content = rest; alignment = Right
            case s"*]$rest" => content = "* "+rest; indent = 5; skip=false
            case s"C]$rest" => content = rest; alignment = Center; skip=false
            case s"R]$rest" => content = rest; alignment = Right; skip=false
            case s"I]$rest" => content = rest; indent = 5; skip=false
            case _ => formatting = false
          }
        }

        val justified   = Col(align=align)(paragraphLines(ems-indent, content, alignment))
        if (skip) result.addOne(Text(" ", font, fg))

        if (indent!=0) {
           result.addOne(Text(" "*indent, font, fg) beside justified)
        } else {
          result.addOne(justified)
        }

    }

    Col(align=Left, bg=nothing)(result.toSeq)
  }
}
