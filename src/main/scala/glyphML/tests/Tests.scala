package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator
import styled.ToggleVariable

import org.sufrin.logging.{INFO, WARN}
import org.sufrin.logging

object trivial extends Application {
  import Translator._
  private val translator = new Translator(new Definitions {})(StyleSheet())
  import translator._

  HYPHENATION("flocci-nauci-nihil-ipil-ifica-tion")("-")
  HYPHENATION("hyphen-at-able")("-")
  HYPHENATION("in-form-ed")("-")
  HYPHENATION("mis-tak-enly")("-")
  HYPHENATION("anti-dis-estab-lish-men-t-arian-ism")("-")
  HYPHENATION("averywidewordwithaninfeasible-breakpoint")("-")
  HYPHENATION("pro-gramm-ing")("-")
  HYPHENATION("tr-act-if-ied")("-")
  HYPHENATION("alter-/ego")("/")

  def GUI: Glyph =
    <div width="11em" fontfamily="Courier" textbackground="yellow" align="left">
      <p>are we well trans_lation_ally mort_if_ied</p>
    </div>

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
  HYPHENATION("averywidewordwithaninfeasible-breakpoint")("-")
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

      global(
         <definitions tag="DEFINITIONS">
           <element    tag="aswell">     as well as other things.</element>
           <attributes id="class:but"    buttonbackground="yellow" buttonforeground="red" fontscale="0.9"/>
           <attributes id="tag:debug"    caption="Debugging" local="t" mark="MARK #1"/>
           <attributes id="tag:p"        align="justify" />
           <attributes id="class:fat"    fontscale="1.3"  align="justify"/>
           <attributes id="class:narrow" align="justify"  width="280px"  textforeground="black"/>
           <attributes id="tag:scope"    trace=""/>
           <macro tag="courier" fontfamily="Courier"><?body?></macro>
           <attributes id="tag:centred"  width="800px"/>
           <macro tag="centred"><table cols="1" foreground="transparent" background="transparent"><?body?></table></macro>
         </definitions>
      )

  }


  lazy val source: Glyph =
    <div fontfamily="Arial" width="400px" labelforeground="black" textforeground="black"  cdataforeground="red"
         hangwidth="3em"
         attributeswarning="on">
      <DEFINITIONS/>
      <centred>
      <glyph gid="buttons" refid="buttonsbar"/>
      <p hang="😀">
        This application tests a combination of <span textforeground="green">local_ization of attributes</span>, <tt fontscale="1.2">text layout</tt>,
        hy_phenation, and the <courier fontscale="1.3">plugging</courier> in of <b>reactive glyphs,</b>  <aswell/>
        The hanging smiley is specified by the <tt>hang="😀"</tt> attribute of this paragraph.
      </p>

      <p hangref ="CHECKBOXES"  textforeground="black">
        This is the running font family, and  <i>this is italic.</i> The text may well spill over &gt; one lines, &amp; everything
        depends on the width of the entire <tt>div</tt>. The hanging checkboxes were specified by the
        <tt>hangref="CHECKBOXES"</tt> attribute of this paragraph: it refers to a globally-defined (active) glyph.
      </p>

      <turn degrees="5">Anything embedded in <![CDATA[<turn>]]> gets "turned".</turn>

      <p align="center"  fontFamily="Times" fontstyle="BOLDITALIC" fontScale="0.75" textforeground="black">
        This is centred text in a small scale bold-italic font.
      </p>

      <frame fg="red.2"><fixedwidth width="0.9*width">this text <fill fg="red" stretch="2"/> is spread</fixedwidth></frame>

      <scale scale=".6">
        <scope trace="for this experiment">
        <macro tag="SPURIOUS">SPURIOUS</macro>
        <attributes id="tag:p" fontFamily="Courier"/>
          <p class="fat">
            This is a long, hyphen_at_able antidisestablishmentarianism tract_ified text concerning floccinaucinihilipilification. The text, in teletype font,
            spills ov_er a nar_row mar_gin un_less I have been <turn degrees="-5">mistakenly</turn><turn degrees="5">informed</turn> by my programming alter-ego.
          </p>
        </scope>
        <SPURIOUS></SPURIOUS>
      </scale>



      <table cols="2" padx="1em" pady="1ex" foreground="green.2" background="yellow">
          <p class="narrow">
            This is a long, possibly hyphenatable, "antidisestablishmentarianism" tractified text that spills
            ov_er a nar_row <frame fg="red.2" bg="pink" radius="0.1">margin</frame> un_less I am mis_tak_enly in_formed.
          </p>
        <p class="narrow">
          This piece of text contains floccinaucinihilipilification averywidewordwithaninfeasiblebreakpoint, but nothing else.
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
    logging.SourceDefault.level=INFO
    Translator.HYPHENATION.level=WARN
    source.enlarged(20).framed(Brushes.blackFrame)
  }

  def title: String = "Test Translator"

}

object abstraction extends Application {
  import Translator._
  val translator = new Translator(new Definitions {})(StyleSheet())
  import translator._


  definitions("definefoo") = // <definefoo/> defines the macro foo in the current scope
    <macro tag="foo"><p><debug local="t"/>FOO</p></macro>

  lazy val source: Glyph =
    <div fontfamily="Times" width="400px" labelforeground="black" textforeground="black" cdatabackground="pink" cdataforeground="red"
         attributeswarning="f"
    >

      <attributes id="class:but" buttonbackground="yellow" buttonforeground="red" fontscale="0.7"/>
      <attributes id="tag:debug" caption="Debugging" local="t"/>
      <attributes id="tag:p" framed="red" align="justify" />
      <attributes id="class:narrow"  align="justify"  width="280px"  textforeground="black"/>

      <macro tag="paratag" fontscale="1.2" framed="red">
        <attributes id="tag:p"><?body?></attributes>
      </macro>

      <paratag>
        <p framed="blue">explicitly <tt>framed="blue"</tt> in the invocation of paratag
           <macro tag="nested" align="right" whocares="not me" keepempty="false">
             <attributes id="tag:p">
               <p><?body?></p>
             </attributes>
           </macro>
        </p>
        <p>within the invocation of paratag <tt>fontscale="1.5" framed="red"</tt></p>
      </paratag>
      <nested align="left">
        left aligned, prevailing frame colour
      </nested>
      <nested textbackground="pink">
        default (=right) alignment, textbackground pink
        <definefoo/>
      </nested>
      <foo/>
    </div>


  lazy val GUI: Glyph = {
    logging.SourceDefault.level=INFO
    Translator.HYPHENATION.level=WARN
    source.enlarged(20).framed(Brushes.blackFrame)
  }

  def title: String = "Test Translator"

}
