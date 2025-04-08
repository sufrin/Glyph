package org.sufrin.glyph
package textshape
import io.github.humbleui.skija.paragraph._
import io.github.humbleui.skija.FontMgr
import org.sufrin.glyph.FontID
import org.sufrin.glyph.GlyphTypes.{Font, FontManager, FontStyle, Scalar}
import org.sufrin.glyph.textshape.TextShape.{paragraphStyle, textStyle}

/**
 * Quick-and-dirty experiment with Skija text paragraphs
 */
@deprecated("Bugs in the underlying SKija/Skia implementation of ParagraphBuilder") object TextShape {

  def fontCollection(familyName: String): FontCollection = {
    val coll = new FontCollection()
    coll.setDefaultFontManager(FontManager.default, familyName)
    coll.setEnableFallback(true)
    coll
  }

  def fontID(font: Font): FontID =
    FontFamily.fontID(font)  match {
      case Some(id) => id
      case None =>
        println("Error: font $font not made by FontFamily")
        FontID("Courier", FontStyle.NORMAL, 32)
    }


  def textStyle(font: Font, fg: Brush, bg: Brush): TextStyle = {
    val style = new TextStyle()
    style.setForeground(fg)
    style.setBackground(bg)
    style.setColor(bg.color)
    style.setDecorationStyle(DecorationStyle.NONE) // overline, etc.
    //style.setHeight(0.8f)
    val FontID(family, fontStyle, size) = fontID(font)
    println(fontID(font))
    style.setFontFamily(family).setFontSize(size).setFontStyle(fontStyle)
    style
  }

  def strutStyle(font: Font): StrutStyle = {
    val FontID(family, fontStyle, size) = fontID(font)
    val style=new StrutStyle()
    style.setFontSize(size).setFontFamilies(Array(family)).setFontStyle(fontStyle).setEnabled(true).setLeading(3)
    style
  }

  def paragraphStyle(font: Font, lines: Int, fg: Brush, bg: Brush): ParagraphStyle = {
    val style = new ParagraphStyle()
    style.setDirection(Direction.LTR)
    style.setAlignment(Alignment.JUSTIFY)
    style.setTextStyle(textStyle(font, fg, bg))
    style.setMaxLinesCount(lines)
    style.setStrutStyle(strutStyle(font))
    //style.setHeightMode(HeightMode.ALL)
    //style.setHeight(1f)
    val metrics = font.getMetrics();
    System.out.println("Ascent: " + metrics.getAscent());
    System.out.println("Descent: " + metrics.getDescent());
    System.out.println("Leading: " + metrics.getLeading());
    style
  }
}

@deprecated("Bugs in the underlying SKija/Skia implementation of ParagraphBuilder") class TextShape(font: Font, lines: Int, fg: Brush, bg: Brush) { thisShape =>
  val Some(FontID(family, fontstyle, size)) = FontFamily.fontID(font)
  val collection = new FontCollection().setDefaultFontManager(FontMgr.getDefault()) // TextShape.fontCollection(family)
  val style = paragraphStyle(font, lines, fg, bg)
  val builder = new ParagraphBuilder(style, collection)
  builder.pushStyle(textStyle(font, fg, bg))

  def addText(text: String): Unit = builder.addText(text.replaceAll("""[\n]+""", " "))

  def layout(pts: Scalar): Paragraph = {
    builder.popStyle()
    val r = builder.build()
    r.layout(pts)
    println((r.getMaxWidth, r.getMaxIntrinsicWidth, r.getLongestLine, r.getHeight, r.getLineNumber))
    r
  }

  def toGlyph(pts: Scalar): Glyph = new Glyph() {

    val para = layout(pts)
    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = {
      surface.drawParagraph(para)
    }

    /**
     * The diagonal size of the glyph
     */
    def diagonal: Vec = Vec(para.getMaxWidth, para.getHeight)

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = toGlyph(pts)

    val fg: Brush = thisShape.fg
    val bg: Brush = thisShape.bg
  }
}
