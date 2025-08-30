package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator
import styled.ToggleVariable

import org.sufrin.logging
import org.sufrin.logging.{FINER, FINEST, INFO, WARN}
import org.sufrin.SourceLocation._

object trivial extends Application {
  locally{
    logging.SourceDefault.level=FINEST
    Translator.level=INFO
    HYPHENATION.level=INFO
    Paragraph.level=INFO
  }

  implicit val style: StyleSheet = StyleSheet()
  val translator = new Translator(new Definitions {})
  val language = translator()
  import language._


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

  implicit val style: StyleSheet = StyleSheet(
    buttonDecoration = styles.decoration.Edged(fg=frameColor, enlarge=0f),
    buttonForegroundBrush = Brushes.black,
    toggleOnBrush = Brushes.black,
    toggleOffBrush = Brushes.black
    )

  val translator = Translator().withPackage(TransformsPackage)
  import translator.definitions
  val language = translator()
  import language._

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
          This is a long, hy_phen_at_able "antidisestablishmentarianism" tract_ified text con_cerning floccinaucinihilipilification. The text may
          spill ov_er a nar_row mar_gin but be hyphen_ated un_less I
          <sub baseline="-1em" fontFamily="Courier"><frame fg="red.1" bg="pink" >have</frame></sub>  been mis_tak_enly in_form_ed
          by my alter ego -- a pro_gramm_er.
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

      <glyph gid="buttons" />

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
  val language = Translator().withPackage(TransformsPackage)(StyleSheet())
  import idioms._
  import language._

  translator.definitions("over") = Col()(Label("A"), Label("B+C"))

  initialDeclarations(
      <macro tag="makeamistake"><measured refid="makeamistake"><deliberatemistake/></measured></macro>
      <macro tag="makeanothermistake"><measured refid="makeanotheramistake"><anothermistake/></measured></macro>

        <macro tag="paratag" framed="red" nonempty="true">
          <?body?>
          <makeamistake/>
        </macro>

        <macro tag="nested" align="right" whocares="not me" nonempty="true" width="virtual.width">
          <table cols="1" uniform="false" background="yellow" nonempty="true"><debug tree="true"/><?body?></table>
        </macro>
    )

  lazy val source: Glyph = {
    <div fontfamily="Times" width="15em" labelforeground="black"  textforeground="black" cdatabackground="transparent" cdataforeground="red"
         attributeswarning="f"
    >



      <attributes id="tag:debug" caption="Debugging" local="t"/>
      <attributes id="tag:p" align="justify" width="width" textforeground="black" fontfamily="Times"/>
      <attributes id="tag:paratag" align="justify" width="virtual.width" textforeground="black" fontfamily="Times" nonempty="true"/>

     <measured refid="virtual" visible="off" orientation="col" background="transparent">
           <p>
             This section will be inserted after the line containing its
             dimensions.
           </p>
           <p>
              Here we check the functionality of &lt;measured visible="false",
              and the reporting of errors during macro expansion
           </p>
     </measured>

      <macro tag="strut"><space width="0em" stretch="0" height="50px"/></macro>
      <macro tag="emfill"><fill width="0em" stretch="20"/></macro>

      <macro tag="math" fontscale="0.75" nonempty="true"><row fg="transparent"><?body?></row></macro>
      <macro tag="pow" fontscale="0.7" nonempty="true"><sub baseline="" height="3em" offset="-0.5em"><frame fg="transparent"><row><?body?></row></frame></sub></macro>
      <macro tag="over" bar="" fontscale="0.75"><sub baseline="" height="3em" offset="-0.5em"><frame fg="transparent"><col fg="black.2" nonempty="true"><?body0?><?bar?><?body1?></col></frame></sub></macro>
      <macro tag="r" fontstyle="italic" fontscale="0.75"><row><?body?></row></macro>

      <fixedwidth width="virtual.width">
        <fill stretch="200" fg="red"/>
        <fixedwidth width="0.75*width" bg="pink"><insert evaluate="virtual.width"/>x<insert evaluate="virtual.height"/></fixedwidth>
        <fill stretch="200" fg="red"/>
      </fixedwidth>

      <glyph gid="virtual"/><!--insert evaluate="width"/-->

      <p>
        Here we explore simple notations <over><r>A</r><r>B + C</r></over> and <math>B<pow><r>A*B</r></pow>*<r>c</r></math>.
        The implementation is by complex and fragile macros: just because one <i>can</i> do it this way doesn't mean one <i>should</i>.
        And the next paragraph dem_on_strat_es some
        matters of importance: namely attribute inheritance.<insert/>
      </p>

      <nested>
        <insert attribute="width"/>
        <paratag>
          <insert attribute="width"/>
           <p>
             This is a paragraph set in the context /nested /paratag /p Its width is <insert evaluate="width"/> and it has
             deliberate mistakes in both context and macros.
           </p>
           <anotherdeliberate/>
        </paratag>
      </nested>
    </div>
  }


  lazy val GUI: Glyph = {
    source.enlarged(20).framed(Brushes.blackFrame)
  }

  def title: String = "Test Translator"

}

object table extends Application {
  implicit val style: StyleSheet = StyleSheet()
  import NaturalSize._

  lazy val data = (0 until 20).map{ n => styled.Label(s" $n ")}

  def GUI: Glyph = Col(
    Grid(fg=Brushes.black).grid(height=10)(data.map(_.copy())),
    Grid(fg=Brushes.black).table(width=10)(data),
  )

  def title: String = "Table"
}
