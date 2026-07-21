package tests

/**
 * This is an experiment in working with paragraph builders.
 * @param builder
 * @param width
 * @param height
 */

import io.github.humbleui.skija.FontMgr
import io.github.humbleui.skija.paragraph._
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph._
import org.sufrin.glyph.NaturalSize._


class Para(builder: ParagraphBuilder, width: Scalar, height: Scalar) extends Glyph {
  def addText(s: String): Unit = {
    val words = s.split("""[\n\s]""")
    for { w <- words} { builder.addText(w); builder.addText(" ") }
  }

  /** A copy of this glyph; perhaps with different foreground/background */
  def copy(fg: Brush, bg: Brush): Glyph = this

  lazy val para: Paragraph = builder.build().layout(width)

  override def diagonal: Vec = {
    Vec(width, para.getHeight)
  }

  def draw(surface: Surface): Unit = {
    surface.drawParagraph(para)
  }


  override def toString: String = {
    s"Para(${para._text})\n${para.getLineMetrics.toList.mkString("\n")}"
  }
}

class TestParagraph(sheet: StyleSheet) {
  implicit val style: StyleSheet = sheet
  val fc: FontCollection = new FontCollection()
  fc.setDefaultFontManager(FontMgr.getDefault)
  val black24 = new TextStyle().setFontSize(24f).setWordSpacing(3f).setColor(Brushes.black.color)
  val red30   = new TextStyle().setWordSpacing(3f).setFontSize(20f).setColor(Brushes.red.color)
  val centred: ParagraphStyle = new ParagraphStyle().setAlignment(Alignment.JUSTIFY).setEllipsis("...").setMaxLinesCount(40)
  val para = new Para(new ParagraphBuilder(centred, fc).pushStyle(black24), 400f, 400f)

  para.addText("The curfew tolls the knell of closing day, the lowing herd wind some-thing o'er the way; the blah di ba")
  lazy val GUI: Glyph = Col()(para)
}

object TestParagraph extends Application {
  val sheet: StyleSheet = StyleSheet()
  val GUI: Glyph = new TestParagraph(sheet).GUI
  println(GUI)
  override def title: String = "TextParagraph"
}
