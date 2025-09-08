package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator
import styled.ToggleVariable
import Brushes.red

import org.sufrin.logging
import org.sufrin.logging.{FINEST, INFO}
import org.sufrin.SourceLocation._

abstract class App extends Application {
  locally{
    logging.SourceDefault.level=FINEST
    Translator.level=INFO
    HYPHENATION.level=INFO
    Paragraph.level=INFO
  }
  def titles: String
  val title = titles.replace("\n", "").replaceAll("[^A-Za-z]", "")
  override val dock = new Dock() {
    setGlyph(unstyled.Label(titles))
  }

  override protected def whenStarted(): Unit = {
    super.whenStarted()
    GUI.guiRoot.autoScale=true
  }
}

object trivial extends App {
  implicit val style: StyleSheet = StyleSheet()
  val language = Translator(style)
  import language._

  initialDeclarations(
    <element tag="lorem"  >
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
      Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    </element>

      <macro tag="showalign" trace="-">(<insert attribute="alignment"/>, <insert attribute="align"/>)</macro>

      <macro tag="show" width="36em" alignment="justify">
        <p align="?alignment"><showalign/></p>
        <fill width="width" fg="red.1"/>
      </macro>
    )

  lazy val source: Glyph =
    <div fontfamily="Courier" background="white">
      <show ><lorem/></show>
      <show alignment="center"><lorem/></show>
      <show alignment="right"><lorem/></show>
      <show alignment="left"><lorem/></show>
    </div>

  val GUI: Glyph = {
    source.framed(Brushes.blackFrame)
  }

  def titles: String = "Triv\nial"



}

object hyphenation extends App {

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
    <element tag="para" >
      are we well translationally mort_if_ied and hyphenatable and
      "antidisestablishmentarianism" averywidewordwithafeasiblebreakpoint
      and here is some_thing un_us_ual: na_me_ly more words, if you want. averywidewordwithoutafeasiblebreakpoint
    </element>
      <element tag="lorem"  >
        Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      </element>
      <macro tag="show" width="16em" >
        <table cols="1" foreground="transparent" background="white" >
          <p align="center">(<insert attribute="width" units="width"/>)</p>
          <p><?body?></p>
        </table>
      </macro>
    )

  lazy val source: Glyph =
    <div fontfamily="Courier"   background="white" textfontsize="20">
      <table cols="3" padx="20px" foreground="red" >
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

  def titles: String = "hyphen\nation"
}


object para extends App {
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

    val autoScale = ToggleVariable(initially = true){ state => source.guiRoot.autoScale = state }

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
      definitions("CHECKBOXES") = Col()(CheckBox(autoScale), CheckBox(autoScale))

      initialDeclarations {
          <element tag="aswell">as well as other things.</element>
          <attributes id="class:but" buttonbackground="yellow" buttonforeground="red" fontscale="0.9"/>
          <attributes id="tag:debug" caption="Debugging" local="t" mark="MARK #1"/>
          <attributes id="tag:p" align="justify" textforeground="black"/>
          <attributes id="tag:turn" align="justify" textforeground="black"/>
          <attributes id="class:fat" fontscale="1.3" align="justify"/>
          <attributes id="class:narrow" align="justify" width="280px" textforeground="black"/>
          <attributes id="class:narrower" align="justify" width="240px" textforeground="black"/>
          <attributes id="tag:scope" trace="-"/>
          <macro tag="courier" fontfamily="Courier">
            <?body?>
          </macro>
          <attributes id="tag:centred" width="800px"/>
          <macro tag="centred">
            <table cols="1" foreground="transparent" background="transparent">
              <?body?>
            </table>
          </macro>
          <macro tag="pink"><withbaseline offset="-2pt"><frame bg="pink"><?body?></frame></withbaseline></macro>
      }
  }


  lazy val source: Glyph =
    <div fontfamily="Arial" width="400px" labelforeground="black"  cdataforeground="red"
         hangwidth="3em"
         attributeswarning="on">
      <centred>
      <glyph gid="buttons" refid="buttonsbar"/>
      <p hang="😀">
        This application tests a variety of features, including <span textforeground="blue">local_ization of attributes,</span>  <tt fontscale="1.2">text layout</tt>,
        hy_phenation, and the <courier fontscale="1.3">plugging</courier> in of <b>reactive glyphs,</b>  <aswell/>
        The hanging smiley is specified by the <tt>hang="😀"</tt> attribute of this paragraph.
      </p>

      <p hangref ="CHECKBOXES"  >
        This is the running font family and  <i>this is italic.</i> The text may well spill over more than one line  &mdash; everything
        depends on the width of the topmost <![CDATA[div]]>. The hanging checkboxes were specified by the
        <tt>hangref=_"CHECKBOXES"</tt> attribute of this paragraph: it refers to a globally-defined (active) glyph.
      </p>

      <p align="center"><turn degrees="5">Anything embedded in <![CDATA[<turn>]]> gets "turned".</turn></p>

      <p align="center"  fontFamily="Times" fontstyle="BOLDITALIC" fontScale="0.75" textforeground="black">
        This is centred text in a small scale bold-italic font.
      </p>

      <frame fg="red.2"><row width="0.9*width" keepempty="+">this text <fill fg="red" stretch="200"/> is spread</row></frame>

        <element tag="blether">
          This is a long, hy_phen_at_able "antidisestablishmentarianism" tract_ified text con_cerning floccinaucinihilipilification. The text may
          spill ov_er a nar_row mar_gin but be hyphen_ated un_less I
          <pink>have</pink> been mis_tak_enly in_form_ed
          by my alter ego -- a pro_gramm_er.
        </element>

        <space/>

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

      <space/>


      <table cols="2" padx="1em" pady="1ex" foreground="green.2" background="yellow">
          <p class="narrow">
            <blether/>
          </p>
        <p class="narrower">
          This piece of text contains "floccinaucinihilipilification" and averywidewordwithaninfeasiblebreakpoint,
          and some framed check_box_es <frame fg="black.2"><glyph gid="CHECKBOXES"/></frame> but nothing else.
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

  def titles: String = "Para\ngraph"

}

object measured extends App {

  import Translator._
  val language = Translator().withPackage(TransformsPackage)(StyleSheet())
  import idioms._
  import language._

  translator.definitions("over") = Col()(Label("A"), Label("B+C"))

  initialDeclarations(
      <macro tag="makeamistake"><measured refid="makeamistake"><deliberatemistake/></measured></macro>
      <macro tag="makeanothermistake"><measured refid="makeanotheramistake"><anothermistake/></measured></macro>

        <macro tag="paratag" nonempty="+">
          <?body?>
          <makeamistake/>
        </macro>

        <macro tag="nested" align="right" whocares="not me" nonempty="true" width="virtual.width">
          <table cols="1" uniform="false" fg="transparent" bg="yellow" nonempty="true"><?body?></table>
        </macro>
    )

  lazy val source: Glyph = {
    <div fontfamily="Times" width="25em" labelforeground="black"  textforeground="black" cdatabackground="transparent" cdataforeground="red"
         framed="transparent.6" attributeswarning="f"
    >
      <attributes id="tag:debug" caption="Debugging" local="t"/>
      <attributes id="tag:p" align="justify" width="width" textforeground="black" fontfamily="Times"/>
      <attributes id="tag:paratag" trace="-" align="justify" width="virtual.width" textforeground="black" fontfamily="Times" nonempty="true"/>

     <measured refid="virtual" visible="off" orientation="col" background="transparent" bg="pink">
       <col bg="pink">
           <p>
             This section will be inserted after the line containing its
             dimensions; and inserted again at the bottom of the window.
           </p>

         </col>
     </measured>

      <p>
        Here we check the functionality of &lt;measured visible="false",
        and the reporting of errors during macro expansion.
      </p>

      <row alignment="med" width="virtual.width">
        <fill stretch="200" fg="red.2"/>
        <frame fg="transparent" enlarge="15px">
         <insert evaluate="virtual.width"/>x<insert evaluate="virtual.height"/>
        </frame>
        <fill stretch="200" fg="red.2"/>
      </row>

      <glyph gid="virtual"/>

      <nested width="2*width">
        <scope>
          <attributes id="tag:row" keepempty="+"/>
          <row >«nested <row> width = <insert attribute="width" /> == <insert evaluate="width"/></row></row>
          <paratag>
            <row>«paratag <row>width = <insert attribute="width"/> == <insert evaluate="width"/> </row></row>
             <p>
               This is a paragraph set in scope <tt>«nested«paratag«p ...</tt> Its width was specified (in paratag) as
               "virtual.width" and it has
               deliberate mistakes in both context and macros.
             </p>
             <anotherdeliberate/>
          </paratag>
        </scope>
      </nested>

      <rotate degrees="180"><glyph gid="virtual"/></rotate>

    </div>
  }


  lazy val GUI: Glyph = {
    source.enlarged(20).framed(Brushes.blackFrame)
  }

  def titles: String = "Meas\nured"

}

object table extends App {
  implicit val style: StyleSheet = StyleSheet()
  import NaturalSize._

  lazy val data = (0 until 20).map{ n => styled.Label(s" $n ")}

  def GUI: Glyph = Col(
    Grid(fg=Brushes.black).grid(height=10)(data.map(_.copy())),
    Grid(fg=Brushes.black).table(width=10)(data),
  )

  def titles: String = "TABLE\n😀😀😀"
}

object math extends App {

  implicit val style: StyleSheet = StyleSheet()
  import Brushes._
  import NaturalSize._
  val language: language = glyphML.Translator().withPackage(TransformsPackage)()
  import language._
  initialDeclarations(
      <macro tag="math" nonempty="true" >
          <span fontfamily="Arial" fontscale="1*fontscale" fontstyle="italic"><?body?></span>
      </macro>

      <macro tag="r" ><row alignment="baseline" nonempty="+"><span fontstyle="italic" fontfamily="Arial"><?body?></span></row></macro>

      <macro tag="pow" ><superscript><?body?></superscript></macro>
      <macro tag="sub" ><subscript><?body?></subscript></macro>

      <macro tag="display" >
        <frame fg="transparent">
        <scope name="display">
          <attributes id="tag:r" fontfamily="Arial" fontscale="0.9*fontscale" fontstyle="italic"/>
            <fixedrow >
                <fill width="1ex"  height="1ex"  stretch="20"/>
              <math>
                 <?body?>
              </math>
              <fill width="1ex"  height="1ex" stretch="20"/>
            </fixedrow>
        </scope>
        </frame>
      </macro>

      <macro tag="center">
        <fixedrow width="1*width">
          <fill width="1ex"  height="1ex"  stretch="20"/>
          <?body?>
          <fill width="1ex"  height="1ex"  stretch="20"/>
        </fixedrow>
      </macro>

    <macro tag="red">
      <span textforeground="red"><?body?></span>
    </macro>

    <macro tag="abs"><bracket fg="black.1" bra="|" ket="|"><?body?></bracket></macro>
  )

  val GUI: Glyph = Col()(
      <div width="40em" align="justify" fontfamily="Times" textfontsize="36" attributes.warning="-" parskip="0.7ex">
        <attributes id="tag:bracket" bra="(" ket=")"/>
        <attributes id="tag:display" width="0.3*width"/>
        <attributes id="tag:superscript" scalefactor="0.7"/>
        <attributes id="tag:subscript"   scalefactor="0.7"/>
        <p>
          This is an experiment in mathematical layout, although it's not something that
          we recommend for putting complex mathematics in GUIs.
        </p>

        <p>Super- and subscripted expressons -- like  <math fontscale="0.9"><superscript>The n</superscript></math> and
          <math fontscale="0.9"><subscript>The n</subscript></math> -- can appear in plain text or in displayed formulae; and
          they can be nested. The inbuilt
          <row>&lt;superscript></row> and <row>&lt;subscript></row> transforms are ready for setting <i>nested</i> 'scripts
          without tedious human intervention; &lt;bracket> draws brackets around expressions.
        </p>

        <center>
          <table padx="1em" pady="1ex" cols="3" bg="transparent" fg="lightgrey">
            <display><superscript>The x*Y*<superscript>P z</superscript></superscript></display>
            <display><superscript>X Y</superscript></display>
            <display><r><superscript>ABC DEF</superscript></r></display>

            <display><r><bracket bra="&lt;" ket="&gt;"><subscript>XYZ ZY</subscript></bracket></r></display>
            <display><r><bracket bra="{" ket="}"><subscript>A<subscript>B*C<abs><subscript>D E</subscript></abs></subscript></subscript></bracket></r></display>
            <display><r><superscript>A<subscript>b+c <superscript>d 2π</superscript></subscript></superscript></r></display>
          </table>
        </center>

        <p>
          And 'scripts play well with fractions and brackets -- though 'scripting with something substantially taller
          than the 'scriptee is implemented somewhat sketchily.
      </p>
        <center>
          <table padx="1em" pady="1ex" cols="3" bg="transparent" fg="lightgrey">
              <display><r><fraction fg="red.1.stroke"><superscript>AB 2πr</superscript> <r>B+C</r></fraction></r></display>
              <display><r><fraction><r>A</r><r>B+C</r></fraction></r></display>
              <display><fraction><bracket fg="red.3">The B C</bracket><row>D E F</row></fraction></display>

              <display><bracket bra="(" ket=")"><r><fraction fg="green"><superscript>AB <bracket>2πr</bracket></superscript> <r>B+C</r></fraction></r></bracket></display>
              <display><bracket><r><fraction><r>A</r><bracket><r>B+C</r></bracket></fraction></r></bracket></display>
              <display><subscript><r>ABC</r><fraction>The B</fraction></subscript></display>

              <display><superscript><r>ABC</r><bracket><fraction>The B</fraction></bracket></superscript></display>
              <display><superscript><r>ABC</r><fraction>The <superscript>B C</superscript></fraction></superscript></display>
              <display><r>erroneous<fraction>fraction</fraction></r></display>
          </table>
        </center>


        <p>
          Of course, no_body
          with good taste would want to use glyphML to write something like
          <bracket bra="" ket=""><red><math><r><fraction><subscript>X Y</subscript><superscript>ABC DEF</superscript></fraction></r></math></red></bracket>
          in a paragraph in a GUI, if you see what I mean.
        </p>

      </div>
  ).enlarged(10).framed(blackFrame).enlarged(5)


  def titles = "MATH\n😀😀😀"

}

object baselines extends App {
  import org.sufrin.glyph.GlyphTypes.Font
  import org.sufrin.glyph.NaturalSize.Row
  val bigSheet = StyleSheet(textFontSize = 32)
  val smallSheet = StyleSheet(textFontSize = 20, cdataForegroundBrush = bigSheet.textForegroundBrush)

  val font1:Font = bigSheet.textFont
  val font2:Font = smallSheet.textFont
  val h1 = font1.getMetrics.getHeight
  val d1 = font1.getMetrics.getDescent
  val h2 = font2.getMetrics.getHeight
  val d2 = font2.getMetrics.getDescent
  def t1(s: String): Glyph = unstyled.Text(s, font1)
  def t2(s: String): Glyph = unstyled.Text(s, font2)
  def t3(s: String): Glyph = unstyled.Text(s, font2).withBaseline(h2/2+h1, h2+h1+d1, h2/2)


  val GUI: Glyph = {
    val language = Translator().withPackage(TransformsPackage)(smallSheet)
    import language._
    definitions("g1") = Row(align=Baseline)(t1("GH"), t3("yxyzYZ"))
    definitions("g2") = Row(align=Baseline)(t1("GH"), t3("yxyzYZ")).turned(90).framed(red)
    definitions("g3") = Row(align=Baseline)(t1("GH"), t3("yxyzYZ")).turned(-10).framed(red)
    definitions("g4") = Row(align=Baseline)(t1("GH"), t3("yxyzYZ")).framed(red)
    definitions("simulated") =
      NaturalSize.Grid(fg=Brushes.lightGrey, height=1, padx=20)(
       Row(align=Baseline)(t1("GH"), t2("xyz")),
       Row(align=Baseline)(t1("GH"), t3("xyzYZ")),
       Row(align=Baseline)(t1("GH"), t2("xyz"), t1("GH"), t3("xyzYZ")),
      )
    NaturalSize.Col(align=Center)(
      <div width="50em" parskip="1.5ex" align="justify">
        <p>
          Here we show how to simulate subscripts and superscripts using differential font sizes
          and the <![CDATA[.withBaseline(...)]]> transform
        </p>
        <fixedrow width="width"><fill/><glyph gid="simulated"/><fill/></fixedrow>
        <p>
          Here we show differences between glyph transformations made (in scala) before glyphML text embeddings; and made as part of a glyphML embedding.
        </p>
        <attributes  id="tag:p" align="left"/>
        <p>§ The regular  glyph: <glyph gid="g1"/>  embedded in a regular paragraph (baselines coincide).</p>
        <p>§ The  glyph: <frame><scale scale="1.2"><glyph gid="g1"/></scale> </frame>embedded in a <![CDATA[<frame><scale scale="1.2" ]]> in a regular paragraph.</p>

        <p>§ The turned (90).framed() glyph: <glyph gid="g2"/> embedded in a regular paragraph.</p>
        <p>§ The turned (90).framed() glyph: <frame><glyph gid="g2"/></frame> embedded in <![CDATA[<frame>]]> in a regular paragraph.</p>
        <p>§ The turned (-10).framed() glyph: <glyph gid="g3"/> embedded in a regular paragraph.</p>
        <p>§ The turned (-10).framed() glyph: <frame><glyph gid="g3"/></frame>  embedded in a <![CDATA[<frame>]]> in a regular paragraph.</p>
        <p>§ The  framed() glyph: <turn degrees="-20"><glyph gid="g4"/></turn> embedded in a <![CDATA[<turn -20>]]> in a regular paragraph.</p>

      </div>
    ) enlarged (10)
  }

  override def titles: String = "Base\nlines"
}