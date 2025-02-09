package org.sufrin.glyph
package tests

import io.github.humbleui.skija.PaintStrokeCap
import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.Application
import org.sufrin.glyph.DynamicGlyphs.ActiveString
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.GlyphXMLOld.{glyphsToParagraph, withBaseline, Abstraction, AttributeMap}
import org.sufrin.glyph.Location.Location
import org.sufrin.glyph.ReactiveGlyphs.Enterable
import org.sufrin.glyph.windowdialogues.Dialogue
import org.sufrin.glyph.DefaultBrushes.blue

import scala.::
import scala.collection.immutable.ListMap
import scala.xml.{Elem, Node}

/**
 * A straightforward generator for consecutively-numbered paragraphs.
 */
class ListItem(format: String, var listItem: Int=0) extends ElementGenerator {
  private var current: Text = null

  import NumberUtils._

  def translate(xml: GlyphXMLOld)(sources: List[String])(within: List[String])(child: Seq[Node])(localAttributes: AttributeMap)(sheet: Sheet): Seq[Glyph] = {
    import GlyphXMLOld.TypedAttributeMap
    listItem += 1
    listItem = localAttributes.Int("count", listItem)
    val label = format.replace("%i", roman(listItem))
                      .replace("%I", Roman(listItem))
                      .replace("%a", alpha(listItem))
                      .replace("%A", Alpha(listItem))
                      .replace("%d", s"$listItem")
    current = Text(label, sheet.labelFont, sheet.labelForegroundBrush, sheet.labelBackgroundBrush)
    val skip = sheet.parSkip
    val lead = current.asGlyph()
    val thePara = GlyphXMLOld.glyphsToParagraph(child.flatMap { node => xml.translate(sources)("p"::within)(node)(localAttributes)(sheet) },
                                             Some(lead))(sheet)
    if (skip == 0.0f)
       List(thePara)
    else
       List(thePara above FixedSize.Space(1f, sheet.parSkip, 0f))
  }
}

trait SheetTestInterface {
  import sheeted._
  import org.sufrin.glyph.Styles.Decoration._
  val fontSize = 24f
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
  implicit val pageStyle: Sheet = rootStyle.withButtonFrame(Framed(fg=rootStyle.buttonForegroundBrush(width=2, cap=PaintStrokeCap.SQUARE), bg=rootStyle.buttonBackgroundBrush))
  implicit val bookStyle: BookSheet = BookSheet(buttonStyle, pageStyle)
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
  val xml = new GlyphXMLOld{}

  /**
   * Applied when an (outermost) xml `Elem`ent is destined to denote a `Glyph`. This
   * translates the element in a context that records its source location in scala text.
   */
  implicit def XMLtoGlyph(elem: Elem)(implicit sheet: Sheet): Glyph = {
    val within = List("")
    NaturalSize.Col().atLeft$(xml.translate(List(s"source"))(within)(elem)(Map.empty)(sheet))
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


  xml("#p")      = <ATTRIBUTES align="justify"/>
  xml("#body")   = <ATTRIBUTES align="center" padX="2em" padY="2ex" width={EMS(pageWidthEms)}/> //ListMap("align"->"center", "padX"->"2em", "padY"->"2ex", "width"->EMS(pageWidthEms))
  xml("#glyph")  = <ATTRIBUTES framed="false"/>
  xml("wide")    = <ATTRIBUTES width={EMS(pageWidthEms)}/>
  xml("caption")  = new Abstraction(<p align="center" width={EMS(pageWidthEms)}> &BODY; </p>)
  xml("centered") = new Abstraction(<row width={EMS(pageWidthEms)}><fill/> &BODY; <fill/></row>)
  xml("BREAK")    = new Abstraction(<fill stretch="100000"/>)

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
  Page("TextToggles&CheckBoxes"){
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

    xml("toggle") = TextToggle(whenTrue="This toggle is set: click to clear it",
                               whenFalse="This toggle is clear: click to set", initially = true) { state => }


    xml("explain1") = explainButton("TextToggles & CheckBoxes")(<body textForeground="black">
      <![CDATA[
      xml("caption") = new Abstraction(<p align="center" width={EMS(pageWidthEms)}>&BODY;</p>)

      <caption><b>TextToggles &amp; CheckBoxes</b></caption>
      <caption>(notice the slight dimming when hovering over one)</caption>
      <caption>A fixed-width row with radio-like checkboxes</caption>
      <centered turnLeft="45" turnRight="-45">
        <row width={EMS(pageWidthEms*2/3)} frame="yellow/4">
          <glyph ref="boxesL" turned="$turnLeft(0)"/> <fill/> <col frame="green">$active </col><fill/> <glyph ref="boxesR" turned="$turnRight(0)"/>
        </row>
      </centered>
      <s/><s/><s/>
      <caption>A fixed-width row with a single text toggle</caption>
      <centered>$toggle</centered>
      <s/><s/><s/>
      <centered><glyph ref="explain1"/></centered>
      ]]>
    </body>)


    <body>
      <caption><b>TextToggles &amp; CheckBoxes</b></caption>
      <caption>(notice the slight dimming when hovering over one)</caption>
      <caption>A fixed-width row with radio-like checkboxes</caption>
      <centered turnLeft="45" turnRight="-45">
        <row width={EMS(pageWidthEms*2/3)} frame="yellow/4">
          <glyph ref="boxesL" turned="$turnLeft(0)"/> <fill/> <col frame="green">$active </col><fill/> <glyph ref="boxesR" turned="$turnRight(0)"/>
        </row>
      </centered>
      <s/><s/><s/>
      <caption>A fixed-width row with a single text toggle</caption>
      <centered>$toggle</centered>
      <s/><s/><s/>
      <centered><glyph ref="explain1"/></centered>
    </body>
  }

  //******************
  Page("Hyphenation"){
    xml("explain2") = explainButton("Source of Hyphenation")(<body>
    <![CDATA[
    xml("flocci")     = ""Flo_cci_nau_ci_nihil_ipili_fi_cat_ion""
    xml("hyphenated") = (<splice>
      Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
      The  word &flocci; has many places at which it can be broken.
    </splice>)
    xml("hyphenated2") = (<splice>
      Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
      Hanging material isn't counted in the paragraph width <i>(here it is 40em).</i>
    </splice>)

    <body parSkip="1.7ex">
      <p align="center"><b>Hyphenation</b></p>
      <p align="centre">Paragraphs of different widths</p>
      <div align="right" fontScale="0.9">
        <div frame="yellow/2" labelStyle="Italic">
          <p hang=" 55em " width={EMS(55)}><use ref="hyphenated"/></p>
          <p hang=" 50em " width={EMS(50)}><use ref="hyphenated"/></p>
          <p hang=" 45em " width={EMS(45)}><use ref="hyphenated"/></p>
          <p hang=" 40em " width={EMS(40)}><use ref="hyphenated"/></p>
          <p xhang=" 40em " width={EMS(40)}><use ref="hyphenated2"/></p>
          <p hang=" 35em " width={EMS(35)}><use ref="hyphenated"/></p>
          <p hang=" 30em " width={EMS(30)}><use ref="hyphenated"/></p>
        </div>
        <s/>
        <row class="wide"><fill/><glyph ref="explain2"/><fill/></row>
      </div>
    </body>
    ]]>
    </body>)

    xml("flocci")     = "Flo_cci_nau_ci_nihil_ipili_fi_cat_ion"
    xml("hyphenated") = (<splice>
      Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
      The  word &flocci; has many places at which it can be broken.
    </splice>)
    xml("hyphenated2") = (<splice>
      Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
      Hanging material isn't counted in the paragraph width <i>(here it is 40em).</i>
    </splice>)

    <body parSkip="1.7ex">
      <p align="center"><b>Hyphenation</b></p>
      <p align="centre">Paragraphs of different widths</p>

      <div align="right" fontScale="0.9">
        <div frame="yellow/2" labelStyle="Italic">
          <p hang=" 55em " width={EMS(55)}><use ref="hyphenated"/></p>
          <p hang=" 50em " width={EMS(50)}><use ref="hyphenated"/></p>
          <p hang=" 45em " width={EMS(45)}><use ref="hyphenated"/></p>
          <p hang=" 40em " width={EMS(40)}><use ref="hyphenated"/></p>
          <p xhang=" 40em " width={EMS(40)}><use ref="hyphenated2"/></p>
          <p hang=" 35em " width={EMS(35)}><use ref="hyphenated"/></p>
          <p hang=" 30em " width={EMS(30)}><use ref="hyphenated"/></p>
        </div>
        <s/>
        <row class="wide"><fill/><glyph ref="explain2"/><fill/></row>
      </div>
    </body>
  }

  Page("Spaces"){
    xml("explain3") = explainButton("Source of Spaces")(<body>
        <![CDATA[
       <p>To avoid spurious spaces being placed around a styled element or an entity and
         adjacent text, place  "empty" <string><nb/></string> in the app_ropriate place
        between element/entity-reference and text.
      </p>

      <p>Here's what happens when you <b>don't</b>, if you see what I mean.</p>
      <p>Here's what happens when you <b>do</b><nb/>, if you see what I mean.</p>
      <p>If you don't care to be <i>too meticulous</i><nb/>, <b>you can just include any
         foll_owing punc_tuation in the styled text,</b>if you see what I mean.
      </p>


      <p>Writing <string>"(&amp;amp;)"</string> generates "(&amp;)" (with spaces around the ampersand) -- when you probably meant
        "(<nb/>&amp;<nb/>)".</p>
      <p>Embedding material in a "nonempty"  <string><nb>...</nb></string> suppresses all spaces
         between components generated by the material: like in this repeated example,
        <nb>"(&amp;)"</nb> and <nb>this &ls;silly&gt; example.</nb>
      </p>]]></body>)
    <body parSkip="1.3ex">
      <p align="center"><b></b></p>
      <p align="center">Avoiding spurious spaces</p>

      <p>To avoid spurious spaces being placed around a styled element or an entity and
         adjacent text, place  "empty" <string><nb/></string> in the app_ropriate place
         between element/entity-reference and text.
      </p>

      <p>Here's what happens when you <b>don't</b>, if you see what I mean.</p>
      <p>Here's what happens when you <b>do</b><nb/>, if you see what I mean.</p>
      <p>If you don't care to be <i>too meticulous</i><nb/>, <b>you can just include any
         foll_owing punc_tuation in the styled text,</b>if you see what I mean.
      </p>


      <p>Writing <string>"(&amp;amp;)"</string> generates "(&amp;)" (with spaces around the ampersand) -- when you probably meant
        "(<nb/>&amp;<nb/>)".</p>
      <p>Embedding material in a "nonempty"  <string><nb>...</nb></string> suppresses all spaces
         between components generated by the material: like in this repeated example,
         <nb>"(&amp;)"</nb> and <nb>this &ls;silly&gt; example.</nb>
      </p>
      <s/>
      <row class="wide"><fill/><glyph ref="explain3"/><fill/></row>
    </body>
  }

  Page("Enumeration") {
    xml("listItem")  = new ListItem("%d.")
    xml("#listItem") = <Attributes  align="justify"/> //"align"->"justify", "fontFamily"->"Menlo")
    xml("Item")      = new ListItem("(%a)")
    xml("#Item")     = <Attributes  align="justify" fontFamily="Menlo" fontScale="0.9" textStyle="Italic" labelStyle="Bold"/>

    xml("explain4")  = explainButton("Source of Enumeration") (
      <body>
      <![CDATA[
    xml("BREAK")     = new Abstraction(<fill stretch="100000"/>)
    xml("listItem")  = new ListItem("%d.")
    xml("#listItem") = <Attributes  align="justify" fontFamily="Menlo" /> //"align"->"justify", "fontFamily"->"Menlo")
    xml("Item")      = new ListItem("(%a)")
    xml("#Item")     = <Attributes  align="justify" fontFamily="Menlo" fontScale="0.9" textStyle="Italic" labelStyle="Bold"/>

        <listItem>
          This is the first of a list of explanatory para_graphs. It's short.
          <BREAK/>
          <Item>This should be nested</Item>
          <Item>So should this</Item>
        </listItem>
        ...
        <listItem>
          This is the third explanatory paragraph. It was very in_ter_esting to see what happened to
          just_ification when the <string><BREAK/></string> below here was missing.
          <BREAK/>
          <Item>In fact what happened was that the (short) last line above this item was filled.</Item>
          <Item>This is a bug that I've not been able to trace.</Item>
          <Item>The BREAK hack is a temporary fix.</Item>
        </listItem>
        <listItem>
          This is a repeat of the third explanatory paragraph. Now you can see what happens to
          just_ification when the necessary <string><BREAK/></string> IS missing.
          <Item>As you can see, the (short) last line above this item is filled.</Item>
          <Item>This is a bug that I've not been able to trace.</Item>
          <Item>The BREAK hack is a temporary fix.</Item>
        </listItem>
      ]]>
      </body>
    )

    <body>
      <p align="center"><b>Numeric labels can be generated automatically.</b></p>
      <div leftMargin="4em" rightMargin="4em" >
        <listItem>
          This is the first of a list of explanatory para_graphs. It's short.
          <BREAK/>
          <Item>This should be nested</Item>
          <Item>So should this</Item>
        </listItem>
        <listItem>
          This is the second of a list of explanatory para_graphs.
          <Item count="1">This should also be nested, but the nested numbering should restart</Item>
          <Item>This is because we used <string>&ls;Item count="1"></string></Item>
        </listItem>
        <listItem>
          This is the third explanatory paragraph. It was very in_ter_esting to see what happened to
          just_ification when the <string><BREAK/></string> below here was missing.
          <BREAK/>
          <Item>In fact what happened was that the (short) last line above this item was filled.</Item>
          <Item>This is a bug that I've not been able to trace.</Item>
          <Item>The BREAK hack is a temporary fix.</Item>
        </listItem>
        <listItem>
          This is a repeat of the third explanatory paragraph. Now you can see what happens to
          just_ification when the necessary <string><BREAK/></string> IS missing.
          <Item>As you can see, the (short) last line above this item is filled.</Item>
          <Item>This is a bug that I've not been able to trace.</Item>
          <Item>The BREAK hack is a temporary fix.</Item>
        </listItem>
        <listItem>
          This is the fifth explanatory paragraph.
          <BREAK/>
          <listItem>This should also be nested, but its number should succ_eed that of the item it's nested in,
                    and it should display the number in the same way as its parent.</listItem>
          <listItem>That's because these nested items were written as <string><listItem>...</listItem></string>.</listItem>
        </listItem>
      </div>
      <s/>
      <row class="wide"><fill/><glyph ref="explain4"/><fill/></row>
    </body>


  }

  Page("Numeral styles"){
    val testedItem   = new ListItem("%d %i %I %a %A")
    xml("tItem")     = testedItem
    xml("#tItem")    = <Attributes align="justify" fontFamily="Menlo" fontScale="0.9" labelStyle="Bold" />
    xml("text")      = "More than one numeral style can be specified"
    <body>
        <caption>Definition and use of a new ListItem style</caption>
        <![CDATA[
  xml("tItem")  = new ListItem("%d %i %I %a %A")
  xml("#tItem") = <Attributes align="justify" fontFamily="Menlo" fontScale="0.9" labelStyle="Bold" />
      ]]>
      <div align="justify" leftMargin="30em" rightMargin="4em" >
      <tItem>&text; for a ListItem generator. Here we use all 5 for each num_eral, namely: %d %i %I %a %A.</tItem>
      <tItem>&text;</tItem>
      <tItem>&text;</tItem>
      <tItem>&text;</tItem>
        <tItem count="9">&text;</tItem>
        <tItem>&text;</tItem>
        <tItem>&text;</tItem>
        <tItem count="159">&text;</tItem>
        <tItem>&text;</tItem>
        <tItem>&text;</tItem>
        <tItem>&text;</tItem>
        <tItem>&text;</tItem>
    </div>
    </body>
  }

  val GUI: Glyph = book.Layout.leftButtons()
}

object SheetTest  extends Application {
  override val defaultIconPath: Option[String] = Some("./flag.png")
  val title = "SheetTest"
  val GUI: Glyph = new SheetTestInterface {}.GUI
}

