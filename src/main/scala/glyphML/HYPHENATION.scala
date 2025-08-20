package org.sufrin.glyph
package glyphML

import unstyled.static.BreakableGlyph
import GlyphTypes.{Font, Scalar}

import org.sufrin.logging.SourceLoggable

object HYPHENATION extends SourceLoggable {
  implicit class SemanticStyleSheet(val style: StyleSheet) extends AnyVal {
    def makeHyphenatableText(text: String, parts: Seq[String]): Glyph = new HyphenatableText(text, parts, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush, transient=false)
    def makeText(text: String): Glyph = unstyled.Text(text, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush, transient=false)
  }

  trait Hyphenation
  case class  Hyphenated(left: Glyph, right: Glyph) extends Hyphenation
  case class  Unbroken(glyph: Glyph) extends Hyphenation
  case object Unbreakable extends Hyphenation

  implicit class ExtendedSeq[T](val s: Seq[T]) extends AnyVal {
    def properTails:  Seq[Seq[T]] = s.tails.drop(1).toSeq
    def properFronts: Seq[Seq[T]] = s.reverse.properTails.map(_.reverse)
    def front: Seq[T] = s.dropRight(1) // s.reverse.tail.reverse
  }

  class HyphenatableText(string: String, val parts: Seq[String], font: Font, fg: Brush, bg: Brush, transient: Boolean) extends unstyled.Text(string, font, fg, bg, transient) {
    val hyphen = unstyled.Text("\u2010", font, fg, bg, false)

    /**
     * Yields the best feasible hyphenationfor the space left,  for this string as `Hyphenated(front, rest)` (where `rest` may itself be hyphenatable), or `Unbreakable`
     * if the string can't be broken to fit in the space left. If the string fits in the space without needing to be broken, then return `Unbroken(string)`. The
     * latter may happen when parts of texts are too wide for their column.
     *
     * @param spaceLeft
     * @return
     */
    def hyphenate(spaceLeft: Scalar): Hyphenation = {
      def width(parts: Seq[String]): Scalar = parts.map(font.measureTextWidth(_,fg)).sum

      val fronts = parts.properFronts.front

      val annotated = fronts.map { s=> (s, width(s))}

      val solutions = annotated.find{ case (s, w) => w<spaceLeft}

      if (HYPHENATION.level<=org.sufrin.logging.FINEST) {
        finest(s"Space: $spaceLeft")
        println(s"  Fronts:    ${fronts.toList.mkString(", ")}")
        println(s"  Annotated: ${annotated.toList.mkString(", ")}")
        println(s"  Solution:  ${solutions.toList.mkString(", ")}")
      }

      if (solutions.isEmpty)
        Unbreakable
      else
      {
        val (front, _) = solutions.get
        if (front.length==parts.length) {
          warn(s"Unbroken $string")
          Unbroken(unstyled.Text(string, font, fg, bg, transient))
        } else {
          val rest = parts.drop(front.length)
          Hyphenated(unstyled.Text(front.mkString(""), font, fg, bg, transient),
                     new HyphenatableText(rest.mkString(""), rest, font, fg, bg, transient))
        }
      }
    }
  }


  /**
   * Maps the given `text` in the given `style` to a (potentially) hyphenatable text.
   * If the text (after stripping of punctuation) appears in the hyphenation dictionary, then a
   * suitable `HyphenatableText(text,parts)` is returned. Likewise, if the text has discretionary hyphenation
   * points, then `HyphenatableText(text.,parts)`
   *
   * @param text
   * @param style
   * @return
   */
  def forText(text: String, style: StyleSheet): Glyph = {
    val discretionaryWordBreak = style.discretionaryWordBreak

    def discretionary: Option[Seq[String]] =
      if (text.contains(discretionaryWordBreak)) {
        val parts = text.split(discretionaryWordBreak).toSeq
        Some(parts)
      }
      else None

    val quoted = """(["]+)(.+)([":,.;!]+)""".r
    val punctuated = """(.+)([:,.;!]+)""".r

    def unpunctuate: (String, String, String) = {
      quoted.findPrefixMatchOf(text) match {
        case Some(matched) =>
          (matched.group(1), matched.group(2), matched.group(3))
        case None =>
          punctuated.findPrefixMatchOf(text) match {
            case Some(matched) =>
              //println(s"PUNCT: ${(matched.group(1), matched.group(2))}")
              ("", matched.group(1), matched.group(2))
            case None =>
              ("", text, "")
          }
      }
    }

    import org.sufrin.SourceLocation.sourceLocation

    def repunctuate(pre: String, parts: Seq[String], post: String): Seq[String] = {
      if (pre.isEmpty && post.isEmpty) parts else {
        val newHead = pre+parts.head
        val newLast = parts.last+post
        HYPHENATION.finest(s"$pre/$post $newHead ... $newLast")
        parts.tail.take(parts.length-2).prepended(newHead).appended(newLast)
      }
    }

    discretionary match {
      case Some(parts) =>
        if (false) {
          val hyphen = style.makeText("-")
          val glyphs = parts.toSeq.map(style.makeText(_))
          (new BreakableGlyph(hyphen, glyphs))
        } else
          style.makeHyphenatableText(parts.mkString(""), parts)

      case None =>
        val (pre, word, post) = unpunctuate
        hyphenation.get(word) match {
          case None =>
            if (logging) finest(s"No hyphenation for: $word")
            (style.makeText(text))
          case Some(parts) =>
            val redecorated = repunctuate(pre, parts, post)
            if (logging) finer(s"$text -> ${redecorated.toList.mkString(", ")}")
            (style.makeHyphenatableText(text, redecorated))
        }
    }
  }

  val hyphenation: collection.mutable.Map[String, Seq[String]] = collection.mutable.Map[String, Seq[String]]()

  def apply(word: String)(implicit punct: String="-"): Unit =
    hyphenation.update(word.replaceAll(punct,""), word.split(punct).toSeq)

}
