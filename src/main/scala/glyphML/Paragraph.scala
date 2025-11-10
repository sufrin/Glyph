package org.sufrin.glyph
package glyphML

import GlyphTypes.Scalar
import glyphML.HYPHENATION.{HyphenatableText, Hyphenated, Unbreakable, Unbroken}
import unstyled.static

import org.sufrin.glyph.glyphML.AbstractSyntax.Scope
import org.sufrin.logging.{SourceDefault, SourceLoggable}

import scala.collection.mutable.ArrayBuffer

object Paragraph extends SourceLoggable {

  case class Shape(lineStartGlyph: ()=>Glyph, lineEndGlyph: ()=>Glyph, galleyWidth: Scalar)

  trait Shaper {
    def nextShape(lineNo: Int): Shape
  }

  case class RectangularShaper(align: Alignment, galleyWidth: Scalar) extends Shaper {
    def nextShape(lineNo: Int): Shape = Shape(align.leftFill, align.rightFill, galleyWidth)
  }

  case class DiminishingShaper(align: Alignment, galleyWidth: Scalar, lastLine: Int, proportion: Scalar,  minWidth: Scalar) extends Shaper {
    var currentWidth: Scalar = galleyWidth
    def nextShape(lineNo: Int): Shape = if (lineNo<=lastLine  && currentWidth>=minWidth) {
        var currentLeft = galleyWidth*proportion*lineNo
        currentWidth = galleyWidth-currentLeft max minWidth
        val left = FixedSize.Space(currentLeft, 1, 0)
        Shape(()=>left, align.rightFill, currentWidth)
    } else nextShape(0)
  }

  case class PyramidShaper(align: Alignment, galleyWidth: Scalar, lastLine: Int, leftProportion: Scalar,  rightProportion: Scalar, minWidth: Scalar) extends Shaper {
    var currentWidth: Scalar = minWidth
    def nextShape(lineNo: Int): Shape = if (lineNo<=lastLine && currentWidth<=galleyWidth) {
      var currentLeft = galleyWidth*leftProportion*(lastLine-lineNo)
      var currentRight = galleyWidth*rightProportion*(lastLine-lineNo)
      currentWidth = galleyWidth-(currentLeft+currentRight) max minWidth
      val left = FixedSize.Space(currentLeft, 1, 0)
      val right = FixedSize.Space(currentRight, 1, 0)
      Shape(()=>left, ()=>right, currentWidth)
    } else
      Shape(align.leftFill, align.rightFill, galleyWidth)
  }

  case class PrefixIndentShaper(align: Alignment, galleyWidth: Scalar, prefixIndent: Scalar, prefixLines: Int) extends Shaper  {
    def nextShape(lineNo: Int): Shape =
      if (lineNo<prefixLines)
        Shape(()=>FixedSize.Space(prefixIndent, 1, 0), align.rightFill, galleyWidth-prefixIndent)
      else
        Shape(align.leftFill, align.rightFill, galleyWidth-prefixIndent)

  }

  def fromGlyphs(sheet: StyleSheet, glyphs: Seq[Glyph], parHang: Option[Glyph], scope: Scope=Scope(Nil)): Glyph = {
    val glyphs$   =
      (if (sheet.parIndent>0) List(static.Rect(sheet.parIndent, 1f, fg=Brushes.transparent)) else Nil) ++ glyphs

    val (hangGlyph, hangWidth) = parHang match {
      case None    => (None, 0f)
      case Some(h) => (Some(h), h.w)
    }

    val leftMargin = sheet.leftMargin max hangWidth

    def buildShaper(galleyWidth: Scalar): Shaper = RectangularShaper(sheet.parAlign, galleyWidth)
      // TODO: move the choice of shaper outside the invocation
      //       probably not as a sheet feature: perhaps only settable from glyphML
      // TODO: an "Illuminated" first letter feature? Probably uncalled-for in a GUI
      // PyramidShaper(sheet.parAlign, galleyWidth, lastLine = 6, leftProportion = .05f, rightProportion = .05f,  galleyWidth/2)
      // PrefixIndentShaper(sheet.parAlign, galleyWidth, prefixIndent = 2*sheet.textFont.getSpacing, prefixLines = 2)
      // DiminishingShaper(sheet.parAlign, galleyWidth, 10, 0.05f, galleyWidth/6) //

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
        scope,
        buildShaper
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
                      scope:          Scope,
                      buildShaper:    Scalar=>Shaper) = {
    // As each line of the paragraph is assembled it is added to the galley
    val galley = ArrayBuffer[Glyph]()
    // maximum width of this paragraph: invariant
    val maxWidth       = overallWidth - (leftMargin + rightMargin)
    // avoid rounding
    var galleyWidth  = maxWidth.floor
    //println(s"[ov=$overallWidth,lm=$leftMargin,maxw=$maxWidthfloor]")
    val words = new PushbackIteratorOfIterator[Glyph](glyphs.iterator)
    var setting = true

    val shaper = buildShaper(galleyWidth)


    @inline def composeLineGlyphs(): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      val thisShape = shaper.nextShape(galley.length)
      galleyWidth = thisShape.galleyWidth

      // Skip any leading interwordspaces left over from a previous line.
      while (words.hasElement && words.element.isInstanceOf[FixedSize.Space]) words.nextElement()

      // start the line
      line += thisShape.lineStartGlyph()

      // add words and interword spaces while there is room
      while (words.hasElement && (lineWidth + words.element.w < galleyWidth)) {
            line += words.element
            lineWidth += words.element.w
            words.nextElement()
      }
      // end of words, or line about to overflow

      // squeeze an extra chunk on by splitting a breakable, if possible
      if (words.hasElement) words.element match {

        case breakable: HyphenatableText =>
          breakable.hyphenate(galleyWidth-lineWidth-interWordWidth-breakable.hyphen.w) match {
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
      val endLine = if (words.hasElement) thisShape.lineEndGlyph() else align.lastFill()
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
      if (words.hasElement && words.element.w.ceil >= galleyWidth) {
        words.element match {
          case breakable: HyphenatableText =>
            breakable.hyphenate(galleyWidth-interWordWidth-breakable.hyphen.w) match {
              case Unbreakable | Unbroken(_) =>
                SourceDefault.warn(s"Clipped unbreakable: ${breakable.string} [at $scope]")
                galley += CLIPWIDTH(galleyWidth)(breakable)
                words.nextElement()


              case _ : Hyphenated =>
                // element splitting is feasible: just compose the line
                // with this starting on it
                composeLine()
            }


          case other =>
            // element is unfittable: just clip it
            other match {
              case other: HyphenatableText => SourceDefault.warn(s"Unfittable Hyphenatable: ${other.parts.mkString("-")}")
              case other: unstyled.Text    => SourceDefault.warn(s"Unfittable Text: ${other.string}")
              case _                       => SourceDefault.warn(s"Unfittable: ${other.getClass}")
            }
            galley += CLIPWIDTH(galleyWidth)(other).framed(Brushes.red, bg=Brushes.pink)
            words.nextElement()
        }
      } else {
        composeLine()
      }
    }
    galley
  }

  import org.sufrin.glyph.{Shape=>GlyphShape}
  def CLIPWIDTH(width: Scalar)(g: Glyph): Glyph = new GlyphShape {

    def draw(surface: Surface): Unit =
      surface.withClip(Vec(width, g.h)) {
        surface.withAlpha(g.diagonal, 0.7f) {
          surface.fillRect(Brushes.darkGrey, g.diagonal)
          g.draw(surface)
        }
      }

    def diagonal: Vec = Vec(width, g.h)

    def withBrushes(fg: Brush, bg: Brush): GlyphShape = null
  }.asGlyph

}
