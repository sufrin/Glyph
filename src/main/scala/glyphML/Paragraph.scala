package org.sufrin.glyph
package glyphML

import GlyphTypes.Scalar
import glyphML.HYPHENATION.{HyphenatableText, Hyphenated, Unbreakable, Unbroken}
import unstyled.static

import org.sufrin.glyph.glyphML.AbstractSyntax.Scope
import org.sufrin.logging.{SourceDefault, SourceLoggable}

import scala.collection.mutable.ArrayBuffer

object Paragraph extends SourceLoggable {

  def fromGlyphs(sheet: StyleSheet, glyphs: Seq[Glyph], parHang: Option[Glyph], scope: Scope=Scope(Nil)): Glyph = {
    val glyphs$   =
      (if (sheet.parIndent>0) List(static.Rect(sheet.parIndent, 1f, fg=Brushes.transparent)) else Nil) ++ glyphs

    val (hangGlyph, hangWidth) = parHang match {
      case None    => (None, 0f)
      case Some(h) => (Some(h), h.w)
    }

    val leftMargin = sheet.leftMargin max hangWidth


    // The overall width is determined by the context
    // If the bounding box is unspecified, then use the column width
    val galley =
      formatParagraph(
        overallWidth   = sheet.parWidth - hangWidth,
        align          = sheet.parAlign,
        leftMargin     = sheet.leftMargin,
        rightMargin    = sheet.rightMargin,
        interWordWidth = sheet.emWidth,
        glyphs$,
        scope
        )

    val column = NaturalSize.Col(bg = sheet.textBackgroundBrush, align=Left)(galley.toSeq)


    hangGlyph match {
      case None =>
        if (true || leftMargin > 0f)
          NaturalSize.Row(Mid, bg = sheet.textBackgroundBrush)(
            FixedSize.Space(w =  leftMargin, h = 0f, stretch = 0f),
            column,
            FixedSize.Space(w = sheet.rightMargin, h = 0f, stretch = 0f))
        else
          column

      case Some(theGlyph) =>
        val space = FixedSize.Space(leftMargin-theGlyph.w,theGlyph.h, 0f)
        NaturalSize.Row(Mid, bg = sheet.textBackgroundBrush)(
          NaturalSize.Row(Top, bg = sheet.textBackgroundBrush)(theGlyph, space, column),
          FixedSize.Space(w = sheet.rightMargin, h = 0f, stretch = 0f))
    }
  }

  /**
   * Build a sequence of galleys representing the lines of a paragraph
   * formed from `glyphs`.
   */
  def formatParagraph(overallWidth:   Scalar,
                      align:          Alignment,
                      leftMargin:     Scalar,
                      rightMargin:    Scalar,
                      interWordWidth: Scalar,
                      glyphs:         Seq[Glyph],
                      scope: Scope) = {
    // As each line of the paragraph is assembled it is added to the galley
    val galley = ArrayBuffer[Glyph]()
    // maximum width of this paragraph: invariant
    val maxWidth       = overallWidth - (leftMargin + rightMargin)
    // avoid rounding
    val maxWidthfloor  = maxWidth.floor
    //println(s"[ov=$overallWidth,lm=$leftMargin,maxw=$maxWidthfloor]")
    val words = new PushbackIteratorOfIterator[Glyph](glyphs.iterator)
    var setting = true

    @inline def composeLineGlyphs(): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      // Skip any leading interwordspaces left over from a previous line.
      while (words.hasElement && words.element.isInstanceOf[FixedSize.Space]) words.nextElement()

      // start the line
      line += align.leftFill()
      // add words and interword spaces while there is room
      while (words.hasElement && (lineWidth + words.element.w < maxWidthfloor)) {
        words.element match {
//          case BreakableGlyph(_, glyphs) =>
//            for { glyph <- glyphs } line += glyph
//            lineWidth += words.element.w
//            words.nextElement()

          case other =>
            line += other
            lineWidth += words.element.w
            words.nextElement()
        }
      }

      // squeeze an extra chunk on by splitting a breakable, if possible?
      if (words.hasElement) words.element match {
//        case breakable: BreakableGlyph =>
//          val breakPoint: Int = breakable.maximal(maxWidthfloor-lineWidth-interWordWidth-breakable.hyphen.w) //??
//          if (breakPoint!=0) {
//            val glyphs = breakable.glyphs
//            for { i <- 0 until breakPoint } {
//              lineWidth += glyphs(i).w
//              line += glyphs(i)
//            }
//            line += breakable.hyphen()
//            //line += interWord()// (from the earlier implementation
//            lineWidth += breakable.hyphen.w
//            words.pushBack(new BreakableGlyph(breakable.hyphen, glyphs.drop(breakPoint)))
//          } else {
//            //SourceDefault.finest(s"Infeasible fit at EOL: $breakable")
//          }

        case breakable: HyphenatableText =>
          breakable.hyphenate(maxWidthfloor-lineWidth-interWordWidth-breakable.hyphen.w) match {
            case Unbreakable =>
              Paragraph.finest(s"Infeasible fit at EOL: ${breakable.string}")

            case Hyphenated(left, right) =>
              line += left
              line += breakable.hyphen.copy()
              words.pushBack(right)

            case Unbroken(glyph)  =>
              line += glyph
              words.nextElement()
          }

        case _ =>
      }


      // line is full
      val endLine = if (words.hasElement) align.rightFill() else align.lastFill()
      // terminate the made-up line with a line-ending stretchy space
      if (line.last.isInstanceOf[FixedSize.Space])
        line.update(line.length - 1, endLine)
      else
        line+=endLine

      (lineWidth, line.toSeq)
    }

    @inline def composeLine(): Unit = {
      val (width, glyphs) = composeLineGlyphs()
      if (glyphs.length == 1 && width == 0) {
        // A line with exactly its leftFill() present
        setting = false
      } else {
        galley += FixedSize.Row(maxWidth, align=Baseline)(glyphs)
      }
    }

    while (setting && words.hasElement) {
      // At the start of a line: perhaps we have an overlong but splittable element
      if (words.hasElement && words.element.w.ceil >= maxWidthfloor) {
        words.element match {
          case breakable: HyphenatableText =>
            breakable.hyphenate(maxWidthfloor-interWordWidth-breakable.hyphen.w) match {
              case Unbreakable | Unbroken(_) =>
                SourceDefault.warn(s"Clipped unbreakable: ${breakable.string} [at $scope]")
                galley += CLIPWIDTH(maxWidthfloor)(breakable)
                words.nextElement()


              case _ : Hyphenated =>
                // element splitting is feasible: just compose the line
                // with this starting on it
                composeLine()
            }

//          case breakable: BreakableGlyph =>
//            val breakPoint: Int = breakable.maximal(maxWidthfloor-interWordWidth-breakable.hyphen.w)
//            if (breakPoint==0)  {
//              // element splitting is infeasible: just clip
//              SourceDefault.fine(s"Clipped at infeasible split: $breakable")
//              galley += CLIPWIDTH(maxWidthfloor)(NaturalSize.Row(align=Mid, bg=Brushes.pink)(breakable.glyphs))
//              words.nextElement()
//            }
//            else {
//              // element splitting is feasible: just compose the line starting with it
//              composeLine()
//            }

          case other =>
            // element is unfittable: just clip it
            other match {
              case other: HyphenatableText => Paragraph.fine(s"Unfittable Hyphenatable: ${other.parts.mkString("-")}")
              case other: unstyled.Text    => Paragraph.fine(s"Unfittable Text: ${other.string}")
              case _                       => Paragraph.fine(s"Unfittable: ${other.getClass}")
            }
            galley += CLIPWIDTH(maxWidthfloor)(other).framed(Brushes.red, bg=Brushes.pink)
            words.nextElement()
        }
      } else {
        composeLine()
      }
    }
    galley
  }

  def CLIPWIDTH(width: Scalar)(g: Glyph): Glyph = new GlyphShape {

    def draw(surface: Surface): Unit =
      surface.withClip(Vec(width, g.h)) {
        g.draw(surface)
        surface.drawLines$(Brushes.black(width=2), 0,g.h/2, width, g.h/2)
      }

    def diagonal: Vec = Vec(width, g.h)

    def withBrushes(fg: Brush, bg: Brush): GlyphShape = ???
  }.asGlyph

}
