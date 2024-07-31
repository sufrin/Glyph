package org.sufrin.glyph
package styled

/**
 *  A collection of styled text glyph constructors.
 */

object text {
  import GlyphTypes._
  import collection.mutable.ArrayBuffer
  import NaturalSize._
  /**
   * A column of one or more lines that appear as they are presented in `text`, aligned `Left`, `Right`, or `Center`.
   * For filled/justified/centered layout within a specific width use `Paragraphs`.
   *
   * @param text    the text of the label
   * @param splitAt (=newline) character used to split the textlayout into lines
   * @param align   the alignment of lines in the column (`Justify==Left`)
   * @param detail  implicit context specifying fonts, foregrounds, etc
   * @return yields the `align`ed  `Col` of default `Label`s generated from `textlayout.split(splitAt)`.
   *
   * @see Paragraphs
   */
  def Label(text: String, align: Alignment = Center)(implicit sheet: StyleSheet): Glyph =
    Label(text, align, sheet.labelStyle)

  /**
   *  As `Label` above but with an explicit `GlyphStyle` parameter
   */
  def Label(text: String, align: Alignment, detail: Styles.GlyphStyle): Glyph = {
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


  private def formatParagraphs(width: Scalar, align: Alignment, text: String)(implicit sheet: StyleSheet): Glyph = {
    val style = sheet.labelStyle
    import style.{em, ex}
    val paras = text.split("\n([ ]+|<)").filter(_.nonEmpty).map { textParagraph(width, align) }
    val space = em
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
   * @param width (Scalar) the width of the column to be occupied by the paragraphs: in logical units (independent of font size)
   * @param ems   (Int) the width of the column expressed in "ems" in the font determined by the implicit style.
   * @param align the alignment of the lines within each paragraph.
   * @param style the styling of the glyphs that form the paragraphs.
   * @return a column in which the paragraphs appear (separated by `style.ex`)
   *
   */
  def Paragraphs(width: Scalar, align: Alignment)(text: String)(implicit sheet: StyleSheet): Glyph =
    formatParagraphs(width, align, text)(sheet)

  def Paragraphs(ems: Int, align: Alignment)(text: String)(implicit sheet: StyleSheet): Glyph =
    formatParagraphs(ems*sheet.Spaces.em.w, align, text)(sheet)

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
    * @see Paragraphs
    */
  private def textParagraph(overallWidth: Scalar, align: Alignment)(text: String)(implicit sheet: StyleSheet): (Glyph, Boolean) = {
    var leftMargin, rightMargin: Degrees = 0f
    var body: List[String] = text.split("[\n ]").filter(_.nonEmpty).toList
    val space = sheet.Spaces.em
    val ex    = sheet.Spaces.ex
    val interWord =                               // very stretchy near-em space
        FixedSize.Space(space.w/1.9f, 100f)
    def horizontalSpacing(dist: Scalar): Glyph =
        FixedSize.Space(dist, 0f, 0f)             // width, but no height or stretch
    val POINTW = Label("<").w

    // Decide on the margins (leftMargin and rightMargin) of the paragraph.
    // TODO: add to the formatting gamut
    body match {
      case Nil =>
        // when para starts with "<<"
      case s"<$spec" :: rest if spec.matches("<*") =>
        leftMargin = POINTW * (1f+spec.length.toFloat)
        body = rest
        // when this para ends with ">>"
        rest.reverse match {
          case Nil =>
          case s">$spec" :: rest if spec.matches(">*") =>
            rightMargin = POINTW * (spec.length.toFloat)
            body   = rest.reverse
          case _ =>
        }
      case _ =>
    }

    // make glyphs of the chunks
    val style = sheet.labelStyle
    val glyphs: Seq[Glyph] = body.map { word => Text(word, style.font).asGlyph(style.fg, style.bg) }

    val galley: ArrayBuffer[Glyph] =
        glyphParagraph(overallWidth, align, leftMargin, rightMargin, interWord, glyphs)

    val column   = Col.atLeft$(galley.toList)
    // The leftMargin is applied to the whole column, if necessary
    val indented = if (leftMargin > 0f) Row(horizontalSpacing(leftMargin), column) else column
    (indented, galley.size==1)
  }

  def glyphParagraph(overallWidth:  Scalar,
                     align:         Alignment,
                     leftMargin:    Scalar,
                     rightMargin:   Scalar,
                     interWord:     Glyph,
                     glyphs:        Seq[Glyph]) = {
    // As each line of the paragraph is assembled it is added to the galley
    val galley = ArrayBuffer[Glyph]()
    // maximum width of this paragraph: invariant
    val maxWidth       = overallWidth - (leftMargin + rightMargin)
    // avoid rounding
    val maxWidthfloor  = maxWidth.floor
    //println(s"[ov=$overallWidth,lm=$leftMargin,maxw=$maxWidthfloor]")
    val interWordWidth = interWord.w
    val words = Stream[Glyph](glyphs)

    @inline def setLine(): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      line += align.leftFill()
      while (words.hasElement && (lineWidth + words.element.w + interWordWidth < maxWidthfloor)) {
        line += words.element
        line += interWord()
        lineWidth += words.element.w + interWordWidth
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

    var setting = true
    while (setting && words.hasElement) {
      // Maybe we have an overlong word
      if (words.hasElement && words.element.w.ceil >= maxWidthfloor) {
        // it's a candidate for hyphenation (if it's text): but we'll just illuminate it
        // println(s"Oversize ${words.element} ${words.element.w} $maxWidth")
        galley += words.element.framed(fg = Brush() col (0XFFFF0000))
        words.nextElement()
      } else {
        val (width, glyphs) = setLine()
        // the line had only its starting alignment glyph; nothing else to do
        if (glyphs.length == 1 && width == 0) {
          setting = false
        } else
          galley += FixedSize.Row(maxWidth).atTop$(glyphs)
      }
    }
    galley
  }

  /**
   *  UNUSED
   *
   *  An iterator for the galley glyphs representing
   *  lines made from an iterator of simple glyphs.
   *
   *  When false, `untilEnd` terminates the result iterator at
   *  the first empty "line"; otherwise it terminates only
   *  when the input iterator terminates.
   */
  def glyphParagraphGalleys(overallWidth:  Scalar,
                     align:         Alignment,
                     leftMargin:    Scalar,
                     rightMargin:   Scalar,
                     interWord:     Glyph,
                     iterator:      Iterator[Glyph],
                     untilEnd:      Boolean = false): Iterator[Glyph] = {
    // maximum width of this paragraph: invariant
    val maxWidth       = overallWidth - (leftMargin + rightMargin)
    // avoid rounding
    val maxWidthfloor  = maxWidth.floor
    val interWordWidth = interWord.w
    val words          = Stream(iterator)

    @inline def setLine(): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      line += align.leftFill()
      while (words.hasElement && (lineWidth + words.element.w + interWordWidth < maxWidthfloor)) {
        line += words.element
        line += interWord()
        lineWidth += words.element.w + interWordWidth
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

    var setting = true

    new Iterator[Glyph] {
      def hasNext: Boolean = (setting||untilEnd) && words.hasElement
      def next(): Glyph = {
        var result: Glyph = null
        while (hasNext) {
          // Maybe we have an overlong word
          if (words.hasElement && words.element.w.ceil >= maxWidthfloor) {
            // it's a candidate for hyphenation (if it's text): but we'll just illuminate it
            // println(s"Oversize ${words.element} ${words.element.w} $maxWidth")
            result = words.element.framed(fg = Brush() col (0XFFFF0000))
            words.nextElement()
          } else {
            val (width, glyphs) = setLine()
            // the line had only its starting alignment glyph
            if (glyphs.length == 1 && width == 0) {
              result = glyphs(0) // a zero-width glyph
              setting = untilEnd
            } else
              result = FixedSize.Row(maxWidth).atTop$(glyphs)
          }
        }
        result
      }
    }
  }


  import DynamicGlyphs.ActiveGlyph

  /** An `ActiveString` glyph whose initial appearance is computed as
   * {{{ Paragraphs(width, align)(text)}}}
   *
   * Whenever its associated string is set by `set(text)`, the associated glyph is recomputed as
   * {{{ Paragraphs(width, align)(text': String) }}}
   * and it is redrawn.
   */
  class ActiveParagraphs(width: Scalar, align: Alignment, text: String)(implicit sheet: StyleSheet)
    extends  ActiveGlyph[String](text, Paragraphs(width, align)(text: String)(sheet))
  {
    def toGlyph(text: String): Glyph = Paragraphs(width: Scalar, align: Alignment)(text)(sheet)
    override def copy(fg: Brush, bg: Brush): ActiveGlyph[String] = new ActiveParagraphs(width, align, text)(sheet)
  }

  object ActiveParagraphs {
    def apply(ems: Int, align: Alignment=Left)(text: String)(implicit sheet: StyleSheet): ActiveGlyph[String] =
      new ActiveParagraphs(ems*sheet.Spaces.em.w, align, text)(sheet)
  }

  object ActiveString {
    def apply(initial: String)(implicit sheet: StyleSheet): DynamicGlyphs.ActiveString = {
        val detail = sheet.labelStyle
        DynamicGlyphs.ActiveString(detail.font, detail.fg, detail.bg)(initial)
    }
  }

}