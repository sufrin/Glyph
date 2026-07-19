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
  val black24 = new TextStyle().setWordSpacing(3f).setFontSize(24f).setColor(Brushes.black.color)
  val red30   = new TextStyle().setWordSpacing(3f).setFontSize(20f).setColor(Brushes.red.color)
  val centred: ParagraphStyle = new ParagraphStyle().setAlignment(Alignment.CENTER).setEllipsis("...").setMaxLinesCount(40)
  val para = new ParagraphBuilder(centred, fc).pushStyle(black24).
    addText("Glyph is ").
    pushStyle(red30).addText("constructive in ").
    //addPlaceholder(new PlaceholderStyle(40f, 50f, PlaceholderAlignment.MIDDLE, BaselineMode.ALPHABETIC, 10f)).
    addText("spirit\tand\temphasis").popStyle().popStyle()

  lazy val GUI: Glyph = Col()(new Para(para, 180f, 300f))
}

object TestParagraph extends Application {
  val sheet: StyleSheet = StyleSheet()
  val GUI: Glyph = new TestParagraph(sheet).GUI
  println(GUI)
  override def title: String = "TextParagraph"
}
