package org.sufrin.glyph
package styled

import GlyphTypes.Scalar
import NaturalSize.{Col, Row}

import scala.collection.mutable.ArrayBuffer


object TextLayout {
  /**
   * A column of one or more lines that appear as they are presented in `text`, aligned `Left`, `Right`, or `Center`.
   * For filled/justified/centered layout within a specific width use `TextParagraphs`.
   *
   * @param text    the text of the label
   * @param splitAt (=newline) character used to split the textlayout into lines
   * @param align   the alignment of lines in the column (`Justify==Left`)
   * @param detail  implicit context specifying fonts, foregrounds, etc
   * @return yields the `align`ed  `Col` of default `TextLabel`s generated from `textlayout.split(splitAt)`.
   *
   * @see TextParagraph
   * @see TextParagraphs
   */
  def TextLabel(text: String, align: Alignment = Center)(implicit sheet: Styles.Sheet): Glyph =
    DetailedTextLabel(text, align, sheet.labelStyle)

  def DetailedTextLabel(text: String, align: Alignment, detail: Styles.GlyphStyle): Glyph = {
    val lines = text.split('\n').toList
    lines.length match {
      case 1 => Text(text, detail.font).asGlyph(detail.fg, detail.bg)
      case _ => {
        val texts = lines.map { line => Text(line, detail.font).asGlyph(detail.fg, detail.bg) }
        align match {
          case Right => Col.atRight$(texts)
          case Left | Justify => Col.atLeft$(texts)
          case Center => Col.centered$(texts)
        }
      }
    }
  }


  private def formatTextParagraphs(width: Scalar, align: Alignment, text: String)(implicit sheet: Styles.Sheet): Glyph = {
    val style = sheet.labelStyle
    val paras = text.split("\n([ ]+|<)").filter(_.nonEmpty).map { TextParagraph(width, align) }
    val space = style.Spaces.em
    def ex = style.Spaces.ex
    val page = ArrayBuffer[Glyph]()
    var lastSingleton = false
    for { (para, singleton) <- paras  } {
      if (singleton && lastSingleton) { page.takeInPlace(page.size-1) }
      page += para
      page += ex
      lastSingleton=singleton
    }
    if (page.last==ex) page.takeInPlace(page.size-1) // drop the final ex
    Col.centered$(page.toSeq)
  }

  /**
   * A column of one or more lines derived from `text`, aligned as specified by `Left`, `Right`, `Center`, or `Justify`.
   *
   * @param text  the text of the paragraphs to be aligned; paragraph boundaries are at lines that start with one or more spaces, or with a '`<`'.
   *
   * The column is indented (see below) if it starts with a "chunk" consisting solely of the character `'<'`; and its width
   *i s diminished if it ends with a "chunk" consisting solely of the character `'>'`. The indentation/diminution is proportional to the
   * number of `'<'` /  `'>'` characters.
   *
   * NB: the present implementation is unsophisticated. First the line is split into paragraphs -- delimited by lines that
   * begin with one or more spaces, or a '`<`'. Next each paragraph is split into
   * "chunks" delimited by spaces/newlines, which are normally set between the current margins according to
   * the mode of alignment. If the paragraph starts with a  chunk consisting solely of two or more '`<`' then its left margin is increased
   * by an amount proportional to their number; if it ENDS with a chunk consisting solely of two or more '`>`', then
   * its right margin is decreased by an amount proportional to their number.
   *
   * If a chunk's size exceeds the maximum width of a line, then it is "forced" into a new line; but will appear framed within a
   * thin red box, and clipped at its rightmost end.
   *
   * @param size  (Scalar) the width of the column to be occupied by the paragraphs: in logical units (independent of font size)
   * @param ems   (Int) the width of the column expressed in "ems" in the font determined by the implicit style.
   * @param align the alignment of the lines within each paragraph.
   * @param style the styling of the glyphs that form the paragraphs.
   * @return a column in which the paragraphs appear (separated by `style.ex`)
   *
   */
  def TextParagraphs(width: Scalar, align: Alignment)(text: String)(implicit sheet: Styles.Sheet): Glyph =
    formatTextParagraphs(width, align, text)(sheet)

  def TextParagraphs(ems: Int, align: Alignment)(text: String)(implicit sheet: Styles.Sheet): Glyph =
    formatTextParagraphs(ems*sheet.Spaces.em.w, align, text)(sheet)

   /**
    *  Workhorse method that splits a text into chunks (at newline and space boundaries), then assembles
    *  a galley from them that is no wider than the overall width. The galley is left aligned, right aligned, or
    *  justified, depending on `align`. If the first chunk of the text starts with one or more '<', then
    *  the left margin is indented, if the last chunk starts with one or more '>', then
    *  the right margin is indented.
    *
    *  The result is `(g, singleton)` where `g` is the (properly-indented) glyph, and `singleton` holds only if the
    *  galley from which it was constructed held a singleton line.
    *
    *  If a chunk's size exceeds the maximum width of a line, then it is "forced" into a new line; but will appear framed within a
    *  thin red box, and clipped at its rightmost end.
    *
    * @see TextParagraphs
    */
  private def TextParagraph(overallWidth: Scalar, align: Alignment)(text: String)(implicit sheet: Styles.Sheet): (Glyph, Boolean) = {
    var indent, undent: Float = 0f
    var body: List[String] = text.split("[\n ]").filter(_.nonEmpty).toList
    val space = sheet.Spaces.em
    val ex = sheet.Spaces.ex
    val interWord = FixedSize.Space(space.w/3f, 100f)                // very stretchy
    def horizontalSpacing(dist: Scalar): Glyph = FixedSize.Space(dist, 0f, 0f) // width, but no height or stretch
    val POINTW = TextLabel("<").w

    // Decide on the margins (indent and undent) of the paragraph.
    // TODO: add to the formatting gamut
    body match {
      case Nil =>
        // when para starts with "<<"
      case s"<$spec" :: rest if spec.matches("<*") =>
        indent = POINTW * (1f+spec.length.toFloat)
        body = rest
        // when this para ends with ">>"
        rest.reverse match {
          case Nil =>
          case s">$spec" :: rest if spec.matches(">*") =>
            undent = POINTW * (spec.length.toFloat)
            body = rest.reverse
          case _ =>
        }
      case _ =>
    }

    // maximum width of this paragraph: invariant
    val maxWidth   = overallWidth - indent - undent
    //
    val maxWidthfloor = maxWidth.floor


    // As each line of the paragraph is assembled it is added to the galley
    val galley = ArrayBuffer[Glyph]()

    // make glyphs of the chunks
    val style = sheet.labelStyle
    val glyphs: Seq[Glyph] = body.map { word => Text(word, style.font).asGlyph(style.fg, style.bg) }

    /**
     * `Iterator`-like structure supporting inspection of  the "current" `element` of
     * a sequence, providing `hasElement()` is true. Unlike an `Iterator`,
     * the current element can be inspected without being "consumed".
     * The methods `nextElement` and `prevElement` change the current element position,
     * which starts at `0`.
     *
     * @param seq
     * @tparam T
     */
    class Stream[T](seq: Seq[T]) {
      private var pos: Int = 0
      /** Is `element` defined */
      def hasElement: Boolean = 0 <= pos && pos < seq.length
      def element: T = seq(pos)
      def nextElement(): Unit = pos += 1
      def prevElement(): Unit = pos -= 1
    }

    def setLineFrom(words: Stream[Glyph]): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      line += align.leftFill()
      while (words.hasElement && (lineWidth + words.element.w + interWord.w < maxWidthfloor)) {
        line += words.element
        line += interWord()
        lineWidth += words.element.w + interWord.w
        words.nextElement()
      }
      // line is full, erase the interword or leftfill
      line.update(line.length - 1, align.rightFill())
      // if this is the very last line, words will be empty
      if (!words.hasElement) {
        line += align.lastFill()
      }
      (lineWidth, line.toSeq)
    }

    val words = new Stream[Glyph](glyphs.toSeq)
    var setting = true
    while (setting && words.hasElement) {
      // Maybe we have an overlong word
      if (words.hasElement && words.element.w.ceil >= maxWidthfloor) {
        // it's a candidate for hyphenation (if it's text): but we'll just illuminate it
        // println(s"Oversize ${words.element} ${words.element.w} $maxWidth")
        galley += words.element.framed(fg=Brush() col (0XFFFF0000))
        words.nextElement()
      } else {
        val (width, glyphs) = setLineFrom(words)
        if (glyphs.length == 1 && width == 0) {
          setting = false
          println("Belt and braces deployed")
        } else
          galley += FixedSize.Row(maxWidth).atTop$(glyphs)
      }
    }

    val column   = Col.atLeft$(galley.toList)
    val indented = if (indent > 0f) Row(horizontalSpacing(indent), column) else column
    (indented, galley.size==1)
  }

  import DynamicGlyphs.ActiveGlyph

  /** An `ActiveString` glyph whose initial appearance is computed as
   * {{{ TextParagraphs(width, align)(text)}}}
   *
   * Whenever its associated string is set by `set(text)`, the associated glyph is recomputed as
   * {{{ TextParagraphs(width, align)(text': String) }}}
   * and it is redrawn.
   */
  class ActiveParagraphs(width: Scalar, align: Alignment, text: String)(implicit sheet: Styles.Sheet)
    extends  ActiveGlyph[String](text, TextParagraphs(width, align)(text: String)(sheet))
  {
    def toGlyph(text: String): Glyph = TextParagraphs(width: Scalar, align: Alignment)(text)(sheet)
    override def copy(fg: Brush, bg: Brush): ActiveGlyph[String] = new ActiveParagraphs(width, align, text)(sheet)
  }

  object ActiveParagraphs {
    def apply(ems: Int, align: Alignment=Left)(text: String)(implicit sheet: Styles.Sheet): ActiveGlyph[String] =
      new ActiveParagraphs(ems*sheet.Spaces.em.w, align, text)(sheet)
  }

  object ActiveString {
    def apply(initial: String)(implicit sheet: Styles.Sheet): DynamicGlyphs.ActiveString = {
        val detail = sheet.labelStyle
        DynamicGlyphs.ActiveString(detail.font, detail.fg, detail.bg)(initial)
    }
  }

}