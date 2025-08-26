package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator
import styled.ToggleVariable

import org.sufrin.logging
import org.sufrin.logging.{FINER, FINEST, INFO, WARN}

object trivial extends Application {
  locally{
    logging.SourceDefault.level=FINEST
    Translator.level=INFO
    HYPHENATION.level=INFO
    Paragraph.level=INFO
  }

  import Translator._
  private val translator = new Translator(new Definitions {})(StyleSheet())
  import translator._

  HYPHENATION("flocci-nauci-nihil-ipil-ifica-tion")("-")
  HYPHENATION("hyphen-at-able")("-")
  HYPHENATION("in-form-ed")("-")
  HYPHENATION("mis-tak-enly")("-")
  HYPHENATION("anti-dis-estab-lish-men-t-arian-ism")("-")
  HYPHENATION("a-very-wide-word-with-a-feasible-break-point")("-")
  HYPHENATION("averywideword-withoutafeasiblebreakpoint")("-")
  HYPHENATION("pro-gramm-ing")("-")
  HYPHENATION("tr-act-if-ied")("-")
  HYPHENATION("tr-ans-lat-ion-al-ly")("-")
  HYPHENATION("mor-tif-ied")("-")
  HYPHENATION("ex-erc-itat-ion")("-")
  HYPHENATION("con-se-qu-at")("-")
  HYPHENATION("cu-pid-a-tat")("-")
  HYPHENATION("ad-ip-isc-ing")("-")

  initialDeclarations(
    <element tag="para">
      are we well translationally mort_if_ied and hyphenatable and
      "antidisestablishmentarianism" averywidewordwithafeasiblebreakpoint
      and here is some_thing un_us_ual: na_me_ly more words, if you want. averywidewordwithoutafeasiblebreakpoint
    </element>
    <element tag="lorem">
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    </element>
    <macro tag="show" width="16em">
        <table cols="1" foreground="transparent" background="white">
        <p align="center">(<insert attribute="width" units="width"/>)</p>
        <p><?body?></p>
        </table>
      </macro>
    )

  lazy val source: Glyph =
    <div fontfamily="Courier"   background="white">
      <table rows="3" padx="20px" foreground="red" >
        <attributes id="tag:p" align="justify" fontfamily="Times"/>
        <show width="12em"><para/></show>
        <show width="16em"><para/></show>
        <show width="16em"><lorem/></show>
        <show width="20em"><para/></show>
        <show width="28em"><para/></show>
        <show width="28em"><lorem/></show>
      </table>
    </div>

  val GUI: Glyph = {
    source.framed(Brushes.blackFrame)
  }

  def title: String = "trivial"
}

object para extends Application {
  val frameColor = Brushes.red(width=2)
  import Translator._
  private val style = StyleSheet(
    buttonDecoration = styles.decoration.Edged(fg=frameColor, enlarge=0f),
    buttonForegroundBrush = Brushes.black,
    toggleOnBrush = Brushes.black,
    toggleOffBrush = Brushes.black
    )
  private val translator = new Translator(new Definitions {})(style)
  import translator._

  HYPHENATION("flocci-nauci-nihil-ipil-ifica-tion")("-")
  HYPHENATION("hyphen-at-able")("-")
  HYPHENATION("in-formed")("-")
  HYPHENATION("mis-tak-enly")("-")
  HYPHENATION("anti-dis-estab-lish-men-t-arian-ism")("-")
  HYPHENATION("averywidewordwithaninfeasiblebreak-point")("-")
  HYPHENATION("pro-gramm-ing")("-")
  HYPHENATION("tr-act-if-ied")("-")
  HYPHENATION("alter-/ego")("/")


  //definitions("aswell") = <span> as well as some tag-extending features.</span>


  definitions("row")= style => NaturalSize.Row(styled.Label("This is a")(style).framed(), styled.Label(" long row")(style))

  locally {

    val extend = DecorativeExtensions(definitions)
    import extend._
    definitions("turn") = StoredExtension (turn)
    definitions("rotate") = StoredExtension(turn)
    definitions("scale") = StoredExtension(scale)
    definitions("frame") = StoredExtension(frame)

    val autoScale = ToggleVariable(initially = false){ state => source.guiRoot.autoScale = state }

    import idioms._

      definitions("buttons") =
        Row(align=Mid, skip=10) (
          TextButton("Show Primitives")     { _ => println(definitions.show(".*".r, ".*".r).toList.sorted.mkString("\n")) },
          TextButton("Show Attributes")     { _ => println(definitions.show(".*".r, "StoredAttributeMap".r).mkString("\n")) },
          CaptionedCheckBox("Scaleable ", "Scale window by dragging edges")(autoScale),
          )

      for { num<-1 to 8} definitions(s"B$num")= TextButton(s"B$num"){ _=> println(num) }
      for { num<-1 to 5} definitions(s"L$num")= Label(s"L$num")

      definitions("BIGBUTTON")  = TextButton("😀"){ _=> println("BIG BUTTON") }
      definitions("CHECKBOXES") = Col()(CheckBox(autoScale), CheckBox(autoScale), CheckBox(autoScale))

      initialDeclarations {
          <element tag="aswell">as well as other things.</element>
          <attributes id="class:but" buttonbackground="yellow" buttonforeground="red" fontscale="0.9"/>
          <attributes id="tag:debug" caption="Debugging" local="t" mark="MARK #1"/>
          <attributes id="tag:p" align="justify" textforeground="black"/>
          <attributes id="tag:turn" align="justify" textforeground="black"/>
          <attributes id="class:fat" fontscale="1.3" align="justify"/>
          <attributes id="class:narrow" align="justify" width="280px" textforeground="black"/>
          <attributes id="class:narrower" align="justify" width="240px" textforeground="black"/>
          <attributes id="tag:scope" trace=""/>
          <macro tag="courier" fontfamily="Courier">
            <?body?>
          </macro>
          <attributes id="tag:centred" width="800px"/>
          <macro tag="centred">
            <table cols="1" foreground="transparent" background="transparent">
              <?body?>
            </table>
          </macro>
      }
  }


  lazy val source: Glyph =
    <div fontfamily="Arial" width="400px" labelforeground="black"  cdataforeground="red"
         hangwidth="3em"
         attributeswarning="on">
      <DEFINITIONS/>
      <centred>
      <glyph gid="buttons" refid="buttonsbar"/>
      <p hang="😀">
        This application tests a variety of features, including <span textforeground="blue">local_ization of attributes,</span>  <tt fontscale="1.2">text layout</tt>,
        hy_phenation, and the <courier fontscale="1.3">plugging</courier> in of <b>reactive glyphs,</b>  <aswell/>
        The hanging smiley is specified by the <tt>hang="😀"</tt> attribute of this paragraph.
      </p>

      <p hangref ="CHECKBOXES"  >
        This is the running font family &mdash; and  <i>this is italic.</i> The text may well spill over &gt; one lines, &amp; everything
        depends on the width of the entire <![CDATA[div]]>. The hanging checkboxes were specified by the
        <tt>hangref="CHECKBOXES"</tt> attribute of this paragraph: it refers to a globally-defined (active) glyph.
      </p>

      <turn degrees="5">Anything embedded in <![CDATA[<turn>]]> gets "turned".</turn>

      <p align="center"  fontFamily="Times" fontstyle="BOLDITALIC" fontScale="0.75" textforeground="black">
        This is centred text in a small scale bold-italic font.
      </p>

      <frame fg="red.2"><fixedwidth width="0.9*width">this text <fill fg="red" stretch="200"/> is spread</fixedwidth></frame>

        <element tag="blether">
          This is a long, hyphen_at_able "antidisestablishmentarianism" tract_ified text con_cerning floccinaucinihilipilification. The text, in teletype font,
          spills ov_er a nar_row mar_gin un_less I <frame fg="red.1" bg="pink" fontFamily="Courier">have</frame>  been <turn degrees="-5">mis_takenly</turn><turn degrees="5">informed</turn>
          by my programming alter-ego.
        </element>

      <scale scale=".6">
        <scope>
        <macro tag="SPURIOUS">SPURIOUS</macro>
        <attributes id="tag:p" fontFamily="Arial"/>
          <p class="fat">
            <blether/>
          </p>
        </scope>
        <SPURIOUS></SPURIOUS>
      </scale>



      <table cols="2" padx="1em" pady="1ex" foreground="green.2" background="yellow">
          <p class="narrow">
            <blether/>
          </p>
        <p class="narrower">
          This piece of text contains "floccinaucinihilipilification" and averywidewordwithaninfeasiblebreakpoint, but nothing else.
        </p>
        <p class="narrow" align="center">
          Donkeys led by a Dodo.
        </p>
        <turn degrees="5"><p class="narrow" align="center">
          Donkeys led by a Dodo.
        </p>
        </turn>
      </table>

      <glyph gid="buttonsbar" />

      </centred>
    </div>


  lazy val GUI: Glyph = {
    source.enlarged(20).framed(Brushes.blackFrame)
  }

  def title: String = "Test Translator"

}

object abstraction extends Application {
  locally {
    logging.SourceDefault.level=FINEST // INFO
    HYPHENATION.level=FINER// INFO
    Translator.level=WARN
    Paragraph.level=WARN
  }
  import Translator._
  val translator = new Translator(new Definitions {})(StyleSheet())
  import translator._

initialDeclarations(
    <macro tag="paratag" fontscale="1.2" framed="red">
      <?body?>
      <table>a<deliberatemistake/>b</table>
    </macro>

      <macro tag="nested" align="right" whocares="not me" keepempty="false" >
        <table cols="1" uniform="false" background="white"><?body?></table>
      </macro>
  )

  lazy val source: Glyph =
    <div fontfamily="Times" width="25em" labelforeground="black"  textforeground="black" cdatabackground="transparent" cdataforeground="red"
         attributeswarning="f"
    >

      <attributes id="tag:debug" caption="Debugging" local="t"/>
      <attributes id="xtag:p" framed="red" align="justify" />

     <measured refid="virtual" visible="f" orientation="baseline" background="transparent">

            <p framed="blue">explicitly <tt>framed="blue"</tt> in the invocation of paratag</p>
            <p>within the invocation of paratag <tt>fontscale="1.5" framed="red"</tt>
            </p><![CDATA[PINK DATA]]>

     </measured>
      <fixedwidth width="virtual.width">
        <fill stretch="200" fg="red"/>
        <fixedwidth width="0.75*virtual.width" bg="pink"><insert evaluate="virtual.width"/>x<insert evaluate="virtual.height"/></fixedwidth>
        <fill stretch="200" fg="red"/>
      </fixedwidth>

      <glyph gid="virtual"/>

      <nested>
        <paratag>
           <glyph gid="virtual"/>
        </paratag>
      </nested>
    </div>



  lazy val GUI: Glyph = {
    source.enlarged(20).framed(Brushes.blackFrame)
  }

  def title: String = "Test Translator"

}

object table extends Application {
  implicit val style: StyleSheet = StyleSheet()
  import NaturalSize._

  lazy val data = (0 until 50).map{ n => styled.Label(s"$n")}

  def GUI: Glyph = Row(
    Grid.grid(width=1)(data),
    Grid.table(width=1)(data)
  )

  def title: String = "Table"
}
