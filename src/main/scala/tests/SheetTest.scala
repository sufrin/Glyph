package org.sufrin.glyph
package tests

import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.Application
import org.sufrin.glyph.DynamicGlyphs.ActiveString
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.GlyphXML.source
import org.sufrin.glyph.ReactiveGlyphs.Enterable

import scala.collection.immutable.ListMap
import scala.xml.Elem

 /* class SheetTest {
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
  implicit def XMLtoGlyph(elem: Elem)(implicit sheet: Sheet): Glyph = {
    val within = List("")
    NaturalSize.Col().atLeft$(X.translate(List(s"$source"))(within)(elem)(Map.empty)(sheet))
  }


  // Build stylesheets from scratch

  val sheet0: Sheet = new Sheet()

  val sheet1: Sheet = sheet0.copy(
    buttonBackgroundBrush = DefaultBrushes.lightGrey,
    buttonBorderBrush = DefaultBrushes.blue(width=8f),
    toggleBackgroundBrush = DefaultBrushes.red,
    labelBackgroundBrush = DefaultBrushes.lightGrey,
    textFontFamily=FontFamily("Menlo"),
    textFontSize=14,
    labelFontFamily=FontFamily("Menlo"),
    labelFontSize=14,
    buttonFontFamily=FontFamily("Menlo"),
    buttonFontSize=14
  )


  implicit val sheet: Sheet =
    sheet1
     // .withButtonFrame(Styles.Decoration.Framed(fg=sheet1.buttonBorderBrush, bg=sheet1.buttonBackgroundBrush, radiusFactor = 0.2f))
     .withButtonFrame(Styles.Decoration.Shaded(fg=sheet1.buttonForegroundBrush, bg=DefaultBrushes.nothing))
      .withButtonFrame(Styles.Decoration.Blurred(fg=sheet1.buttonForegroundBrush, blur=4, spread=4))
  .withButtonFrame(Styles.Decoration.Unframed)

  val radioSheet = sheet.withButtonFrame()

  // Glyphs named in the XML
  X("toggle1")   = sheeted.TextToggle(whenTrue="Checking", whenFalse="Not checking", initially=true){ _ => }
  X("button1")   = sheeted.TextButton("Button 1"){ _ => }
  X("button2")   = sheeted.TextButton("Button 2"){ _ => }
  X("checkbox1") = sheeted.CheckBox(initially=true){ _ => }
  X("buttons")   = NaturalSize.Grid.table()(sheeted.UniformSize.constrained(Uniform.buttons))
  X("boxes")     = RadioCheckBoxes(List("One", "Two", "Three"), "One"){ n => println(n) }(radioSheet).arrangedVertically()

  val emWidth = 60

  def rootWidth = s"${emWidth}em"

  val root: Glyph =
    <body align="justify" parSkip="0.4ex"
          frame="red"  framed="false" padX="2ex" padY="2ex" background="grey4" textBackground="grey4">

      <p width={s"${emWidth}em"} source="p1">
        Welcome to the world of style sheets and of glyphs that are styled by such sheets.
        This very small app tests a few of these.
      </p>

      <row width={s"${emWidth}em"}>
        <fill/>
            <p width={s"${emWidth/2}em"} >
            Here are some reactive glyphs to be getting on with. They are packed in <i>this</i>
            left-aligned
            narrow paragraph that is centred in the interface:
            $checkbox1 $button1 $toggle1 $buttons $boxes
           </p>
        <fill/>
          $boxes
        <fill/>
      </row>

      <row width={rootWidth} frame="yellow/2">
        <!--p width={s"${emWidth/5}em"} fontScale="0.65" frame="darkGrey/2" framed="true">
          The glyphs to the right are the same as those above, except that they are
          packed in a fixed-size row. WTF!
        </p-->
        <glyph ref="checkbox1"/>
        <glyph ref="button1"/>
        <glyph ref="toggle1"/>
        <glyph ref="buttons"/>
        <fill/>
        <glyph ref="boxes" framed="true" frame="red/3" turned="35"/>
      </row>

      <p width={rootWidth} source="p2">
        Here is $button2$ -- notice that it is (almost) centered vertically around the baseline.
      </p>


      <p width={rootWidth} source="p3">
        Here is $button1 and the vertical column of $buttons -- the latter will also be centered vertically about
        the text baseline, and that the line separation is thereby altered.
      </p>



    </body>

}
*/



trait SheetTestInterface {
  import sheeted._
  import org.sufrin.glyph.Styles.Decoration._
  val fontSize = 18f
  val pageWidthEms = 60f
  val background = DefaultBrushes.lightGrey
  val dimension = Vec(700f, 500f)


  def EMS(ems: Scalar): String = s"${ems}em"


  val rootStyle = Sheet(
    textFontSize = fontSize,
    buttonFontSize = fontSize,
    labelFontSize = fontSize,
    backgroundBrush = background,
    labelBackgroundBrush = background,
    textBackgroundBrush = background,
    buttonBackgroundBrush = background,
    containerDimension = dimension
  )

  val buttonStyle = rootStyle.withButtonFrame(Blurred(fg=rootStyle.buttonForegroundBrush, blur=6, spread=6))
  implicit val pageStyle: Sheet = rootStyle.withButtonFrame(Framed(fg=rootStyle.buttonForegroundBrush, bg=rootStyle.buttonBackgroundBrush))
  implicit val bookStyle: BookStyle = BookStyle(buttonStyle, pageStyle)

  // The book container
  val book = Book()
  val Page = book.DefinePage

  // XML details
  val xml = new GlyphXML{}

  /**
   * Applied when an (outermost) xml `Elem`ent is destined to denote a `Glyph`. This
   * translates the element in a context that records its source location in scala text.
   */
  implicit def XMLtoGlyph(elem: Elem)(implicit sheet: Sheet): Glyph = {
    val within = List("")
    NaturalSize.Col().atLeft$(xml.translate(List(s"$source"))(within)(elem)(Map.empty)(sheet))
  }


  xml("#p")    = ListMap("align"->"justify")
  xml("#body") = ListMap("align"->"center", "padX"->"2em", "padY"->"2ex", "width"->EMS(pageWidthEms))
  xml("#glyph") = ListMap("framed"->"false")


  //******************
  Page("Welcome"){
      <body>
        <p align="center"><b>Welcome</b></p>
        <p>This little app​lic​ation is a testbed for Glyphs styled imp​licitly with style​sheets.</p>
        <p>The text <nobreak>"&amp;"</nobreak> without spurious spaces, is denoted as follows:</p>
        <![CDATA[<nobreak>"&amp;"</nobreak> ]]>
      </body>
  }

  //******************
  Page("Reactive Glyphs 1"){
    // Declare a local "active" glyph
    val active = ActiveString(font=pageStyle.textFont, fg=pageStyle.textForegroundBrush, bg=pageStyle.textBackgroundBrush)("  Unticked  ")
    xml("active") = active

    // Declare two reactive glyphs
    xml("boxesL") = RadioCheckBoxes(List("0", "1", "2"), "0"){
      case Some(n) => active.set(s"L ticked $n"); case None => active.set("L Unticked")
    }.arrangedVertically()
    xml("boxesR") = RadioCheckBoxes(List("0", "1", "2"), "0"){
      case Some(n) => active.set(s"R ticked $n"); case None => active.set("R Unticked")
    }.arrangedVertically()

    val anchor = sheeted.TextButton("Hover here for an explanation"){ _ => }
    val theHint: Glyph = <p>This is how it was done</p>
    new HintManager(anchor.asInstanceOf[Enterable], theHint, 10f)
    xml("anchor") = anchor

    <body>
      <p align="center"><b>Reactive Glyphs embedded in XML</b></p>

      <p align="centre">A fixed-width row with 2 reactive glyphs &amp; an active glyph</p>
      <row width={EMS(pageWidthEms)}>
        <fill/>
        <row width={EMS(pageWidthEms*2/3)} frame="yellow/4">
          <glyph ref="boxesL" turned="35"/> <fill/> <col frame="green">$active </col><fill/> $boxesR
        </row>
        <fill/>
      </row>
      <glyph ref="anchor"/>
    </body>
  }

  val GUI: Glyph = book.Layout.leftButtons()
}

object SheetTest  extends Application {
  override val defaultIconPath: Option[String] = Some("./flag.png")
  val title = "SheetTest"
  val GUI: Glyph = new SheetTestInterface {}.GUI
}

