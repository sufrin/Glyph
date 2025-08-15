package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator
import styled.ToggleVariable
import styles.decoration.RoundFramed
import unstyled.reactive.Reaction

import org.sufrin.logging.{INFO, WARN}
import org.sufrin.logging
/**
 * An experiment in providing idiomatic support for building style-dependent (mostly reactive) glyphs to add to
 * translation definitions.
 */
object idioms {

  def Label(text: String): StyleSheet => Glyph =
      {style: StyleSheet => styled.Label(text)(style)}

  def TextButton(name: String): Reaction=>StyleSheet=>Glyph  =
    (reaction: Reaction) => {style: StyleSheet=>styled.TextButton(name)(reaction)(style)}

  def TextToggle(whenTrue: String="❎", whenFalse: String="✅", hint: Hint=NoHint): ToggleVariable => StyleSheet => Glyph =
    (variable: ToggleVariable) => {style: StyleSheet=>styled.TextToggle(whenTrue, whenFalse, variable.value, hint)(variable)(style)}

  def Table(cols: Int=0, rows: Int=0, uniform: Boolean=false)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph = fromSequence.Table(cols, rows, uniform)(glyphs)
  def Col(align: Alignment=Center)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph = fromSequence.Col(align)(glyphs)
  def Row(align: VAlignment=Mid)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph = fromSequence.Row(align)(glyphs)

  /** Constructions for `Row, Col, Table` that take `Seq[Glyph]` arguments  */
  object fromSequence {
    def Col(align: Alignment=Center)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = {
      style: StyleSheet =>
        val reified = glyphs.map(_.apply(style))
        NaturalSize.Col(align = align)(reified)
    }

    def Row(align: VAlignment=Mid)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = {
      style: StyleSheet =>
        val reified = glyphs.map(_.apply(style))
        NaturalSize.Row(align = align)(reified)
    }

    def Table(cols: Int=0, rows: Int=0, uniform: Boolean=false)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = { style: StyleSheet =>
      import style.{padX, padY, backgroundBrush => bg, foregroundBrush => fg}
      val reified = glyphs.map(_.apply(style))
      val Grid = NaturalSize.Grid(fg = fg, bg = bg, padX, padY)
      val buildFrom = if (uniform) Grid.grid(height = rows, width = cols)(_) else Grid.table(height = rows, width = cols)(_)
      buildFrom(reified)
    }
  }

}

object para extends Application {
  import Translator._
  private val style = StyleSheet(buttonDecoration = RoundFramed(enlarge=0.6f, radius=4, fg=Brushes.blackFrame, bg=Brushes.lightGrey))
  private val translator = new Translator(new ValueStore {})(style)
  import translator._

  HYPHENATION("flocci-nauci-nihil-ipil-ifica-tion")("-")
  HYPHENATION("hyphen-at-able")("-")
  HYPHENATION("in-formed")("-")
  HYPHENATION("mis-tak-enly")("-")
  HYPHENATION("anti-dis-estab-lish-men-t-arian-ism")("-")
  HYPHENATION("averywidewordwithaninfeasible-breakpoint")("-")


  locally { for { num<-1 to 8} primitives(s"B$num")=idioms.TextButton(s"B$num"){_=> println(num) } }
  locally { for { num<-1 to 5} primitives(s"L$num")=idioms.Label(s"L$num") }

  primitives("aswell") = <span> as well as some tag-extending features</span>

  /* Add
   * primitives(<attributes id=.../>)
   * primitives(<macro tag=.../>)
   * primitives(<entity name=.../>)
   *
   * If an element has a ref="name" attribute, then it's stored under "name"
   * Unit expressions need generalised
   *
   * */

  private val autoScale = ToggleVariable(initially = false){ state => source.guiRoot.autoScale = state }

  primitives("row")=style => NaturalSize.Row(styled.Label("This is a")(style).framed(), styled.Label(" long row")(style))

  locally {
    import idioms._
      primitives("buttons") =
        Table(uniform=true) (
          TextButton("Show all Primitives") { _ => println(primitives.show(".*".r, ".*".r).mkString("\n")) },
          TextButton("Show Attributes")     { _ => println(primitives.show(".*".r, "StoredAttributeMap".r).mkString("\n")) },
          TextToggle(whenFalse = "Autoscale: ✅", whenTrue = "Autoscale: ❎")(autoScale))
  }


  lazy val source: Glyph =
    <div fontfamily="Times" width="400px" labelforeground="black" textforeground="black" cdatabackground="pink" cdataforeground="red"
         attributeswarning="f">
      <attributes id="class:but"    buttonbackground="yellow" buttonforeground="red" fontscale="0.9"/>
      <attributes id="tag:debug"    caption="Debugging" local="t" mark="MARK #1"/>
      <attributes id="tag:p"        framed="yellow" align="justify" />
      <attributes id="class:fat"    fontscale="1.4"  align="justify"/>
      <attributes id="class:narrow" align="justify"  width="280px"  textforeground="black"/>
      <macro tag="fw" fontfamily="Courier"><?body?></macro>


      <p hang=" * ">
        This application tests a combination of <span textforeground="green">local_ization of attributes</span>, <tt fontscale="1.2">text layout</tt>,
        hy_phenation, and the <fw>plugging</fw> in of <b>reactive glyphs,</b>  <aswell/>.
      </p>

      <p fontFamily="Courier" textforeground="black">
        This is Courier, and  <i>this is italic.</i> The text may well spill over &gt; one lines, &amp; everything
        depends on the width of the entire <tt>div</tt>.
      </p>

      <p align="center"  fontFamily="Times" fontstyle="BOLDITALIC" fontScale="0.75" textforeground="black">
        This is centred text in a small scale bold-italic font.
      </p>

      <span foreground="transparent" background="transparent"><glyph gid="buttons"/></span>
      <glyph gid="unk"/>

      <attributes id="tag:p" fontFamily="Courier" framed="blue">
        <p class="fat">
          This is a long, hyphen_at_able antidisestablishmentarianism tract_ified text concerning floccinaucinihilipilification. The text, in teletype font,
          spills ov_er a nar_row mar_gin un_less I have been mistakenly informed by my programming alter-ego.
        </p>
      </attributes>

      <table cols="2" padx="1em" pady="1ex" foreground="green.2" background="yellow">
          <p class="narrow">
            This is a long, possibly hyphenatable, "antidisestablishmentarianism" tract_ified text that spills
            ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed.
          </p>
        <p class="narrow">
          This piece of text contains floccinaucinihilipilification averywidewordwithaninfeasiblebreakpoint, but nothing else.
        </p>
        <p class="narrow" align="center">
        She is another donkey.
        </p>
      </table>
      <deliberatelyundefinedtag/>
      <mymacro p1="nondefault p1">
        the invocation <tt>SS</tt> <i>TT</i>
      </mymacro>
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
  val translator = new Translator(new ValueStore {})(StyleSheet())
  import translator._


  primitives("definefoo") = // <definefoo/> defines the macro foo in the current scope
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
           <macro tag="nested" align="right" whocares="not me" keepspace="false">
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
