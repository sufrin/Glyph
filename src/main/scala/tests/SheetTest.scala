package org.sufrin.glyph
package tests

import org.sufrin.SourceLocation.SourceLocation

class SheetTest {
  import scala.xml._
  import org.sufrin.glyph.GlyphXML.source
  import org.sufrin.glyph.GlyphXML
  import org.sufrin.glyph.Sheet
  import org.sufrin.glyph.sheeted._

  object Uniform {
    import sheeted.UniformSize
    def but(caption: String) = UniformSize(caption){ _ => println(caption) }
    val buttons: Seq [UniformSize.ButtonSpecification] = List("The", "Rain", "in", "Spain") map but
  }

  val X = new GlyphXML{}

  /**
   * Applied when an (outermost) xml `Elem`ent is destined to denote a `Glyph`. This
   * translates the element in a context that records its source location in scala text.
   */
  implicit def XMLtoGlyph(elem: Elem)(implicit source: SourceLocation): Glyph = {
    val within = List("")
    NaturalSize.Col().atLeft$(X.translate(List(s"$source"))(within)(elem)(Map.empty)(new Sheet()))
  }

  // Build stylesheets from scratch

  val sheet0: Sheet = Sheet()

  val sheet1: Sheet = sheet0.copy(
    buttonBackgroundBrush = DefaultBrushes.lightGrey,
    buttonBorderBrush = DefaultBrushes.blue(width=15f),
    toggleBackgroundBrush = DefaultBrushes.red,
    labelBackgroundBrush = DefaultBrushes.lightGrey
  )


  implicit val sheet: Sheet =
    sheet1
      .withButtonFrame(Styles.Decoration.Framed(fg=sheet1.buttonBorderBrush, bg=sheet1.buttonBackgroundBrush, radiusFactor = 0.63f))
      //.withButtonFrame(Styles.Decoration.Shaded(fg=sheet1.buttonForegroundBrush, bg=DefaultBrushes.nothing))
      //.withButtonFrame(Styles.Decoration.Unframed)

  // Glyphs used from the XML
  X("toggle1") = sheeted.TextToggle(whenTrue="Checking", whenFalse="Not checking", initially=true){ _ => }
  X("button1") = sheeted.TextButton("Button 1"){ _ => }
  X("checkbox1") = sheeted.CheckBox(initially=true){ _ => }
  X("buttons")   = NaturalSize.Grid.rows(width=1)(sheeted.UniformSize.constrained(Uniform.buttons))
  X("boxes")     = RadioCheckBoxes(List("One", "Two", "Three"), "One"){ n => println(n) }(sheet.withButtonFrame()).arrangedVertically()

  val rootWidth = "50em"

  val root: Glyph =
    <body width={rootWidth}
          fontFamily="Courier" fontScale="1" fontSize="24" align="justify" parSkip="0.4ex"
          framed="red"  padX="2ex" padY="2ex" background="grey4" textBackground="grey4">
      <p>
        Welcome to the world of style sheets and of glyphs that are styled by such sheets.
        This very small app tests a few of these.
      </p>

      <row>
        <glyph ref="checkbox1"/>
        <glyph ref="button1"/>
        <glyph ref="toggle1"/>
        <glyph ref="buttons"/>
        <glyph ref="boxes"/>
      </row>

    </body>

}

object SheetTest extends Application {
  override val defaultIconPath: Option[String] = Some("./flag.png")
  val title = "SheetTest"
  val GUI: Glyph = new SheetTest {}.root
}

