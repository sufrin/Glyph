package org.sufrin.glyph
package tests

import glyphXML.Translation.AttributeMap
import Glyphs.INVISIBLE
import glyphXML.{Abstraction, Translation}
import sheeted.windowdialogues.Dialogue

import org.sufrin.glyph.sheeted.{Book, BookStyle, TextButton}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.Location.Location
import org.sufrin.glyph.windowdialogues.Dialogue

object glyphXMLTest extends Application {
    import xml._
    import Translation.Target._


    val translator: Translation = new Translation {

      def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation(primitives) {
        override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, children: Seq[Node]): Seq[Target] = {
          super.translate(tag :: tags, paragraph, attributes.updated("textStyle", textStyle), sheet, children)
        }
      }

      meaning("i") = textStyleTranslation("i", "Italic")
      meaning("b") = textStyleTranslation("b", "Bold")
      meaning("bi") = textStyleTranslation("bi", "BoldItalic")
      meaning("n") = textStyleTranslation("n", "Normal")
      meaning("tt") = new Translation(primitives) {
        override def toString: String = "tt"
        override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, children: Seq[Node]): Seq[Target] = {
          super.translate(tags, paragraph, attributes.updated("textFontFamily", "Courier"), sheet, children)
        }
      }
      meaning("caption") =
        new Abstraction(<p align="center"><b>&BODY;</b></p>)
    }



    // Specify application-specific material
    locally {
      import sheeted._
      translator("B1")        =  TextButton("Button1"){ _ => println(s"B1") }(_).turned(10)
      translator("B2")        =  TextButton("Button2"){ _ => println(s"B2") }(_)
      translator("B3")        =  TextButton("Button3"){ _ => println(s"B3") }(_)
      translator("LINK")      =  sheet => TextButton("LINK"){ _ => println(s"LINK") }(sheet.copy(buttonFrame = Styles.Decoration.Unframed))
      translator("L1")        =  Label("Label1")(_)
      translator("L2")        =  Label("Label2")(_)
      translator("L3")        =  Label("Label3")(_)
      translator("LS")        =  sheet => NaturalSize.Col()(Label("LS1")(sheet), Label("LS2")(sheet), Label("LS3")(sheet)).framed().turned(10f)
      translator("LSC")       =  <col frame="black"><glyph gid="L1"/> <glyph gid="LINK"/><glyph gid="L3"/></col>
      translator("B1")        = <ATTRIBUTES buttonForeground="red/2"        buttonBackground="yellow"/>
      translator("B2")        = <ATTRIBUTES buttonForeground="green/2"      buttonBackground="yellow"/>
      translator("B3")        = <ATTRIBUTES buttonForeground="lightgrey/2"  buttonBackground="black"/>
      translator("tag:body")  = <ATTRIBUTES background="white"/>
      translator("tag:p")     = <ATTRIBUTES textBackground="white" align="justify" parSkip="1ex"/>
    }


    // set up the interface
    val sheet: Sheet = Sheet().copy(
        backgroundBrush       = DefaultBrushes.nothing,
        buttonForegroundBrush = DefaultBrushes.red,
        buttonFrame           = Styles.Decoration.Blurred(fg=DefaultBrushes.red(width=10), bg=DefaultBrushes.nothing, blur=5f, spread=5f)
      )
    implicit val pageSheet: Sheet = Sheet().copy(
        backgroundBrush       = DefaultBrushes.nothing,
        buttonForegroundBrush = sheet.textForegroundBrush,
        buttonFontSize        = sheet.textFontSize*0.9f,
        buttonFrame           = Styles.Decoration.Framed(fg=DefaultBrushes.red(width=2), bg=DefaultBrushes.nothing)
    )
    implicit val bookStyle: BookStyle = new BookStyle(buttonStyle = sheet, pageStyle = pageSheet)

  val book = sheeted.Book()
  val Page = book.Page

  import translator.XMLtoGlyph

    val p1 = Page("Paragraphs") {
      <body  width="45em" textFontFamily="Menlo" textFontSize="20" labelFontFamily="Courier" labelFontSize="20" background="nothing">
        <ENTITY key="filler" expansion="(this is some filler to make the paragraph longer)"/>
        <ATTRIBUTES key="tag:caption" textFontSize="25"/>
        <caption logging="caption">
          This is a little tester for various glyphXML features, princ_ipally the mixing of pre_defined glyphs with para_graph text.
        </caption>


        <p align="justify" hang="L1. ">
          The rain in spain falls <b>mainly</b> in the  <glyph gid="B2" scaled="0.5" frame="red/6"/> plain. &filler; <glyph gid="B1"/>
        </p>
        <p align="justify" hang="L2. ">
          The rain in spain falls <b>mainly</b> in the  <glyph gid="B2"/> plain. &filler; <glyph gid="B1"/>
        </p>

        <glyph gid="B1" fg="green"/><glyph gid="B2" fg="green" scaled="2"/>

        <p leftMargin="5em" rightMargin="5em" frame="green/1"  rotated="2">
          The rain (<glyph gid="LINK"/>) in spain falls <i>mainly</i> in the plain.
        </p>

        <p>
          Here is a longish &ls;col&gt;umn in the midst
          <col frame="black"><glyph gid="L1"/> <glyph gid="LINK"/><glyph gid="L3"/></col>
          of a paragraph. Note that its lowest point aligns with the baseline.
        </p>
        <p>
          Here is a longish &ls;glyph&gt; in the midst  <glyph gid="LS"/> of a paragraph. Note that its lowest point
          also aligns with the baseline.
          And note that  <glyph gid="LSC"/> which was invoked by
          <![CDATA[<glyph gid="LSC"/>]]>
          has the same relation to the baseline.
        </p>
        <fill/>
        <row valign="mid" textFontFamily="Menlo" textFontSize="16" inheritwidth="true">
          <p align="justify" width="17em" >This is the left hand col_umn of the two col_umns that are on this row</p>
          <fill width="1em" stretch="1"/>
          <p align="justify"  width="17em">This is the right hand col_umn of two</p>
        </row>
        <fill/>
        <row inheritwidth="true"><glyph gid="L1"/><fill stretch="1"/> <glyph gid="L2"/> <fill stretch="3"/><glyph gid="L3"/></row>
      </body>
    }

    val p2 = Page("Tables") {
      <body align="justify" width="25em" textFontFamily="Menlo" textFontSize="20" labelFontFamily="Courier" labelFontSize="30">
        <SCOPE>
        <ATTRIBUTES key="tag:p" textBackground="" align="left" parSkip="1ex" frame="white" scoped="true"/>
        <ATTRIBUTES key="outer" foreground="blue/5" padY="40px" padX="40px" background="grey2"/>
        <ATTRIBUTES key="tag:table" foreground="grey3/1" />
        <ATTRIBUTES key="tag:rows" foreground="blue/1" background="darkGrey"/>
        <ATTRIBUTES key="tag:cols" foreground="green/1" />
         <!--<debug:attributes id="tag:table"/>
         <debug:attributes id="tag:rows"/>
         <debug:attributes id="tag:cols"/>
         <debug:attributes id="tag:p"/>
         -->
        <table class="outer" cols="2"  >
          <table cols="2" padX="20px" padY="20px" background="yellow">1
            <p>There are
              <tt fontScale="1.5">REALLY</tt>
              several things here.
              <tt>A</tt>
            </p>
            <p>There are several things here. B</p>
            <p>There are several things here. C</p>
            <p fontScale="1.2">There are several things here. (table(cols=2))</p>
            <p width="30em">There are several things here. E</p>
          </table>
          <table cols="2" uniform="true" padX="20px" padY="20px" background="yellow">2
            <p>There are several things here. A</p>
            <p>There are several things here. B</p>
            <p>There are several things here. C</p>
            <p>There are several things here. D (uniform(cols=2))</p>
            <p width="30em">There are several things here. E</p>
            <!--p width="30em">There are actually six things here. F</p-->
          </table>

          <table rows="2" padX="20px" padY="20px" background="green">3
            <p>There are several things here. A</p>
            <p>There are several things here. B</p>
            <p>There are several things here. C</p>
            <p>There are several things here. (table(rows=2))</p>
            <p width="30em">There are several things here. E</p>
          </table>
          <rows cols="2" padX="20px" padY="20px">4
            <p>There are several things here. A</p>
            <p width="30em" fontScale="0.7">There are several things here. B</p>
            <p>There are several things here. C</p>
            <p>There are several things here. D (rows(cols=2))</p>
            <p width="30em" fontScale="1.4">There are several things here. E</p>
          </rows>
          <cols rows="2" padX="20px" padY="20px">5
            <p width="10em">There are several things here. A</p>
            <p width="10em" fontScale="0.7">There are several things here. B</p>
            <p>There are several things here. C</p>
            <p>There are several things here. D (cols(rows=2))</p>
            <p width="30em" fontScale="1.4">There are several things here. E</p>
          </cols>
        </table>
          <!--debug:attributes id="tag:rows"/-->
        </SCOPE>
        <!--debug:attributes id="tag:rows"/-->
      </body>.scaled(0.7f)
    }


    val p3 = Page("Hyphenation"){

      def OK(blurb: Glyph, position: Location=null, title: String="")(implicit sheet: Sheet): sheeted.windowdialogues.Dialogue[Unit] = {
        // Mutual references ok<->popup
        lazy val ok: Glyph = TextButton("OK") { _ => popup.close() }(sheet)
        lazy val popup: sheeted.windowdialogues.Dialogue[Unit] = new sheeted.windowdialogues.Dialogue[Unit](blurb, List(ok), position, title, bg=sheet.backgroundBrush)
        popup
      }

      def EMS(ems: Scalar): String = s"${ems}em"

      def explainButton(caption: String)(explanation: scala.xml.Elem): Glyph = {
        lazy val button: Glyph = TextButton("Source"){
          _ => OK(explanation).OnRootOf(button).start()
        }
        button
      }



    <body width="55em" background="nothing" parSkip="1.7ex">
     <SCOPE>
      <ELEMENT key="hyphenated">
        <span>
           Hyphenation of text within para_graphs is done at dis_cret_ion_ary break_points.
           The  word &flocci; has many places at which it can be broken.
        </span>
      </ELEMENT>
      <ENTITY key="flocci" expansion="Flo_cci_nau_ci_nihil_ipili_fi_cat_ion"/>
              <caption>Hyphenation</caption>
              <col align = "right">
              <p hang=" 55em " width={EMS(55)}><glyph gid="hyphenated"/></p>
              <p hang=" 50em " width={EMS(50)}><glyph gid="hyphenated"/></p>
              <p hang=" 45em " width={EMS(45)}><glyph gid="hyphenated"/></p>
              <p hang=" 40em " width={EMS(40)}><glyph gid="hyphenated"/></p>
              <p xhang=" 40em " width={EMS(40)}><glyph gid="hyphenated"/></p>
              <p hang=" 35em " width={EMS(35)}><glyph gid="hyphenated"/></p>
              <p hang=" 25em " width={EMS(25)}><glyph gid="hyphenated"/></p>
              </col>
     </SCOPE>
    </body>
  }

    val p4 = Page("Span"){
        <p align="justify" width="16em" frame="green">
          This is a text of width 16em. And <span fontScale="1.7" textFontFamily="Courier">this is <span fontScale="1.1">in_side</span> a span</span> to see what's happ_ening.
        </p>
    }

  val p5 = Page("Inherited") {
    <body width="40em">
      <caption>Inherited attributes</caption>
      <p align="justify" width="16em" frame="green">
        This is a text of width 16em. And
        <span fontScale="1.7" textFontFamily="Courier">this is
          <span fontScale="1.1">in_side</span>
          a span</span>
        to see what's happ_ening.
      </p>
      <p align="justify" frame="blue">
        This is a text of
        <b>inherited</b>
        width. And
        <span fontScale="1.7" textFontFamily="Courier">this is
          <span fontScale="1.1">in_side</span>
          a span</span>
        to see what's happ_ening.
      </p>
      <row inheritwidth="true">
        <fill/>
        <p align="justify" width="16em" frame="green">
          This is a text of width 16em. And
          <span fontScale="1.7" textFontFamily="Courier">this is
            <span fontScale="1.1">in_side</span>
            a span</span>
          to see what's happ_ening.
        </p>
        <fill/>
      </row>
    </body>
  }

  val p6=Page("Spaces") {
      <body width="50em" align="justify" background="white" frame="white/14">
        <SCOPE>
          <ENTITY key="entity" expansion="«the expansion of &quot;&amp;entity;&quot;»"/>
          <p >To avoid a spurious spaces being placed adjacent to an element or an entity expansion,
            in a rendered paragraph just adjoin the element (or entity) reference to the nearest character on the app_ropriate side
            in the paragraph body.
            Below are some examples that use the following entity declaration:
          </p>
          <div leftMargin="6ex" rightMargin="5ex" parSkip="2ex">
          <span fontScale="0.75" textFontFamily="Courier">
          <![CDATA[
          <ENTITY key       = "entity"
                  expansion = "«the expansion of &quot;&amp;entity;&quot;»"/>]]></span>

          <p>Here is &entity; and as you can see it has spaces adjacent to each side.</p>
          <p>Here is &entity;and as you can see it is directly adjacent to the text on its right.</p>
          <p>Here is&entity;and as you can see it is adjacent to the text on both its sides.</p>
          <p>Here is <b>some bold material followed by</b> <i>&entity;</i> italicized.</p>
          <p>Here is <b>some bold material followed by</b><i>&entity;</i>italicized.</p>
          </div>
          <p>
          In short, <i>spaces usually appear in a rendered paragraph wherever they appear in the paragraph body.</i></p>
        </SCOPE>
      </body>
  }




  val GUI: Glyph = book.Layout.leftButtons()

    def title: String = "glyphXML Test"

}
