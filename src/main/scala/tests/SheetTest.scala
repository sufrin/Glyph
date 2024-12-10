package org.sufrin.glyph
package tests

import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.Application
import org.sufrin.glyph.DynamicGlyphs.ActiveString
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.GlyphXML.source
import org.sufrin.glyph.Location.Location
import org.sufrin.glyph.ReactiveGlyphs.Enterable
import org.sufrin.glyph.windowdialogues.Dialogue

import scala.collection.immutable.ListMap
import scala.xml.{Elem, Node}


trait SheetTestInterface {
  import sheeted._
  import org.sufrin.glyph.Styles.Decoration._
  val fontSize = 18f
  val pageWidthEms = 60f
  val background = DefaultBrushes.lightGrey
  val dimension = Vec(700f, 500f)


  def EMS(ems: Scalar): String = s"${ems}em"
  def BR(word: String, at: String="_"): String = word.replace(at, "\u200B")


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
  val explainStyle: Sheet = rootStyle.copy(backgroundBrush = DefaultBrushes.lightGrey, fontScale=0.7f, textFontFamily=FontFamily("Courier"))

  def OK(blurb: Glyph, position: Location=null, title: String="")(implicit sheet: Sheet): Dialogue[Unit] = {
    // Mutual references ok<->popup
    lazy val ok: Glyph = TextButton("OK") {
      _ => popup.close()
    }(sheet)
    lazy val popup: Dialogue[Unit] = new Dialogue[Unit](blurb, List(ok), position, title, bg=sheet.backgroundBrush)
    popup
  }


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

  /**
   * A button that pops up a dialogue
   * @param caption caption for the dialogue window
   * @param explanation glyphXML for the dialogue
   * @return the button
   */
  def explainButton(caption: String)(explanation: scala.xml.Elem): Glyph = {
    lazy val button: Glyph = TextButton("Source"){
      _ => OK(XMLtoGlyph(explanation)(explainStyle), null, caption)(pageStyle).OnRootOf(button).start()
    }
    button
  }

  xml("#p")     = ListMap("align"->"justify")
  xml("#body")  = ListMap("align"->"center", "padX"->"2em", "padY"->"2ex", "width"->EMS(pageWidthEms))
  xml("#glyph") = ListMap("framed"->"false")
  xml("wide")   = ListMap("width"->EMS(pageWidthEms))


  //******************
  Page("Welcome"){
      <body parSkip="1.5ex">
        <p align="center"><b>Welcome</b></p>
        <p>
            This notebook is a testbed for Glyphs styled imp_licitly with style_sheets.
            Its pages have for the most part been specified directly in <b>GlyphXML,</b> which bears
            a <i>passing</i> resemblance to <b>xhtml.</b>
        </p>
        <p>
            The <b>Glyph</b> API has convenient facilities for the mutual em_bedding
            of <b>Scala</b> glyph objects and <b>GlyphXML.</b>
        </p>
      </body>
  }

  //******************
  Page("Reactive Glyphs"){
    // Declare a local "active" glyph
    val active = ActiveString(font=pageStyle.textFont, fg=pageStyle.textForegroundBrush, bg=pageStyle.textBackgroundBrush)("  Unticked  ")
    xml("active") = active

    // Declare two reactive glyphs
    xml("boxesL") = RadioCheckBoxes(List("0", "1", "2"), ""){
      case Some(n) => active.set(s"L ticked $n"); case None => active.set("L Unticked")
    }.arrangedVertically()
    xml("boxesR") = RadioCheckBoxes(List("0", "1", "2"), ""){
      case Some(n) => active.set(s"R ticked $n"); case None => active.set("R Unticked")
    }.arrangedVertically()

    xml("explain1") = explainButton("Source of Reactive Glyphs")(<body>
      <![CDATA[
    val active = ActiveString(font=pageStyle.textFont, fg=pageStyle.textForegroundBrush, bg=pageStyle.textBackgroundBrush)("  Unticked  ")
    xml("boxesL") = RadioCheckBoxes(List("0", "1", "2"), ""){
      case Some(n) => active.set(s"L ticked $n"); case None => active.set("L Unticked")
    }.arrangedVertically()
    xml("boxesR") = RadioCheckBoxes(List("0", "1", "2"), ""){
      case Some(n) => active.set(s"R ticked $n"); case None => active.set("R Unticked")
    }.arrangedVertically()
    <body>
        <p align="center"><b>Reactive Glyphs</b></p>
        <p align="centre">A fixed-width row with 2 reactive glyphs &amp; an active glyph</p>
        <row class="#wide">
          <fill/>
          <row width={EMS(pageWidthEms*2/3)} frame="yellow/4">
               <glyph ref="boxesL" turned="35"/>
               <fill/> <col frame="green">$active </col><fill/>
               $boxesR
          </row>
          <fill/>
        </row>
        <s/>
    </body>
      ]]>
    </body>)

    <body>
      <p align="center"><splice><b>Reactive Glyphs</b></splice></p>

      <p align="centre">A fixed-width row with 2 reactive glyphs &amp; an active glyph</p>
      <row class="wide">
        <fill/>
        <row width={EMS(pageWidthEms*2/3)} frame="yellow/4">
          <glyph ref="boxesL" turned="35"/> <fill/> <col frame="green">$active </col><fill/> $boxesR
        </row>
        <fill/>
      </row>
      <s/>
      <row class="wide"><fill/><glyph ref="explain1"/><fill/></row>
    </body>
  }

  //******************
  Page("Hyphenation"){
    xml("explain2") = explainButton("Source of Hyphenation")(<body>
    <![CDATA[

    xml("flocci")     = "Flo_cci_nau_ci_nihil_ipil_if_icat_ion"
    xml("hyphenated") = (<splice>
      Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
      The  word &flocci; has many places at which it can be broken.
    </splice>)

    <body parSkip="1.7ex">
      <p align="center"><b>Hyphenation</b></p>
      <p align="centre">Paragraphs of different widths</p>
      <div align="center">
        <div frame="yellow/2">
          <p width={EMS(55)}><use ref="hyphenated"/></p>
          <p width={EMS(50)}><use ref="hyphenated"/></p>
          <p width={EMS(45)}><use ref="hyphenated"/></p>
          <p width={EMS(40)}><use ref="hyphenated"/></p>
          <p width={EMS(35)}><use ref="hyphenated"/></p>
          <p width={EMS(30)}><use ref="hyphenated"/></p>
        </div>
        <s/>
        <row class="wide"><fill/><glyph ref="explain2"/><fill/></row>
      </div>
    </body>
    ]]>
    </body>)

    xml("flocci")     = "Flo_cci_nau_ci_nihil_ipil_if_icat_ion"
    xml("hyphenated") = (<splice>
      Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
      The  word &flocci; has many places at which it can be broken.
    </splice>)

    <body parSkip="1.7ex">
      <p align="center"><b>Hyphenation</b></p>
      <p align="centre">Paragraphs of different widths</p>

      <div align="center">
        <div frame="yellow/2">
          <p width={EMS(55)}><use ref="hyphenated"/></p>
          <p width={EMS(50)}><use ref="hyphenated"/></p>
          <p width={EMS(45)}><use ref="hyphenated"/></p>
          <p width={EMS(40)}><use ref="hyphenated"/></p>
          <p width={EMS(35)}><use ref="hyphenated"/></p>
          <p width={EMS(30)}><use ref="hyphenated"/></p>
        </div>
        <s/>
        <row class="wide"><fill/><glyph ref="explain2"/><fill/></row>
      </div>
    </body>
  }

  Page("Etc"){
    <body>
      <p align="center"><b>Etc</b></p>
      <p align="center">Miscellaneous notes</p>

      <p>The text <nobreak>"&amp;"</nobreak> is denoted by:</p>
      <row class="wide"><fill/><![CDATA[<nobreak>"&amp;"</nobreak>]]> &nbsp;<verb> rather than </verb><![CDATA["&amp;"]]><fill/></row>
      <s/>
      <p>Without the <nobreak>&ls;nobreak&gt;</nobreak> embedding the three
        elements appear sep_arated by spaces: "&amp;".
      </p>


    </body>
  }

  val GUI: Glyph = book.Layout.leftButtons()
}

object SheetTest  extends Application {
  override val defaultIconPath: Option[String] = Some("./flag.png")
  val title = "SheetTest"
  val GUI: Glyph = new SheetTestInterface {}.GUI
}

