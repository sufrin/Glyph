package org.sufrin.glyph
import unstyled.Text

import Brushes.{blue, nothing}
import GlyphTypes.{Font, Scalar}
import NaturalSize.{Col, Row}

import scala.collection.mutable.ListBuffer

/**
 *  Implements a much simpler notation than `glyphXML` to describe texts that need
 *  justification, centering, etc; but need no colour or font  changes.
 */

object SimpleParagraphs {
  /**
   * A glyph formed of the paragraphs of `text` with blank lines between them.
   * No paragraph is wider than `ems` * the width of an `n` in `font`.
   *
   * The `text` is split into paragraphs at boundaries where an empty or empty-looking line appears, or
   * where a line starts with "[".
   *
   * The words of each paragraph are then used to fill lines of length no more than the limit, and the
   * glyph that results is the Col composition of the text formed of these lines.
   *
   * Normally each line has the alignment specified by `align`; but this may be changed on
   * a per-paragraph basis, depending on whether the line starts with a directive:
   * {{{
   *  [C] each filled line is centered.
   *  [R] each filled line is right-aligned
   *  [L] each filled line is left-aligned
   *  [J] each filled line is "inflated" to fill the width available
   *  [I] each filled line is indented
   *  [*] as above, but the first starts with a bulletpoint
   * }}}
   */
  def apply(ems: Int, font: Font = Brushes.buttonFont, fg: Brush = blue, bg: Brush = nothing, align: Alignment=Justify)(text: String): Glyph = {
    val emWidth = unstyled.Text("n", font).w

    def paragraphLines(ems: Int, text: String, align: Alignment): Seq[Glyph] = {
      val words = Stream[String](text.trim.split("""[ ]+"""))
      val result = ListBuffer[Glyph]()
      val line = new ListBuffer[Glyph]
      def lineLength: Scalar = line.map(_.w).sum
      val lineWidth = ems*emWidth
      val alignment = align
      while (words.hasElement) {
        var text: Glyph = Text(words.element, font, fg)
        while (words.hasElement && lineLength + text.w + emWidth < lineWidth) {
          line.append(text)
          line.append(FixedSize.Space(emWidth, 1, 1))
          words.nextElement()
          if (words.hasElement) text = Text(words.element, font, fg)
        }
        //val theText = Text(line.toString, font, fg, bg)
        alignment match {
          case Center =>
              line.remove(line.length-1)
              line.append(FixedSize.Space(1,1,200))
              result.addOne(FixedSize.Row(width=ems*emWidth)(FixedSize.Space(1,1,200)::line.toList))
          case Right =>
            line.remove(line.length-1)
            result.addOne(FixedSize.Row(width=ems*emWidth)(FixedSize.Space(1,1,200)::line.toList))
          case Left =>
            line.append(FixedSize.Space(1,1,200))
            result.addOne(FixedSize.Row(width=ems*emWidth)(line.toList))
          case Justify =>
              result.addOne(FixedSize.Row(width=ems*emWidth)(line.toList))
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
        var count = 0

        while (formatting) {
          content match {
            case s"[*]$rest" => content = "* "+rest; indent = 5
            case s"[C]$rest" => content = rest; alignment = Center
            case s"[R]$rest" => content = rest; alignment = Right
            case s"[J]$rest" => content = rest; alignment = Justify
            case s"*]$rest" => content = "* "+rest; indent = 5; skip=false
            case s"C]$rest" => content = rest; alignment = Center; skip=false
            case s"R]$rest" => content = rest; alignment = Right; skip=false
            case s"J]$rest" => content = rest; alignment = Justify; skip=false
            case s"I]$rest" => content = rest; indent = 5; skip=false
            case _ => formatting = false
          }
        }

        val justified   = Col(align=align)(paragraphLines(ems-indent, content, alignment))
        if (skip&&count>0) result.addOne(Text(" ", font, fg))
        count += 1

        result.addOne(FixedSize.Space(1, 10, 0))

        if (indent!=0) {
           result.addOne(Row(Text(" "*indent, font, fg), justified))
        } else {
          result.addOne(justified)
        }

    }

    Col(align=Left, bg=bg)(result.toSeq)
  }
}
