package org.sufrin.glyph
package glyphML
package tests

import glyphML.Context.Context
import glyphML.Translator
import styled.ToggleVariable

import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.logging.{INFO, WARN}
import org.sufrin.logging

object para extends Application {
  val frameColor = Brushes.red(width=2)
  import Translator._
  private val style = StyleSheet(
    buttonDecoration = styles.decoration.Edged(fg=frameColor, enlarge=0f),
    buttonForegroundBrush = Brushes.black,
    toggleOnBrush = Brushes.black,
    toggleOffBrush = Brushes.black
    )
  private val translator = new Translator(new ValueStore {})(style)
  import translator._

  HYPHENATION("flocci-nauci-nihil-ipil-ifica-tion")("-")
  HYPHENATION("hyphen-at-able")("-")
  HYPHENATION("in-formed")("-")
  HYPHENATION("mis-tak-enly")("-")
  HYPHENATION("anti-dis-estab-lish-men-t-arian-ism")("-")
  HYPHENATION("averywidewordwithaninfeasible-breakpoint")("-")
  HYPHENATION("pro-gramm-ing")("-")
  HYPHENATION("alter-/ego")("/")


  primitives("aswell") = <span> as well as some tag-extending features</span>


  primitives("row")=style => NaturalSize.Row(styled.Label("This is a")(style).framed(), styled.Label(" long row")(style))

  locally {
    val autoScale = ToggleVariable(initially = false){ state => source.guiRoot.autoScale = state }

    import idioms._

      primitives("buttons") =
        Row(align=Mid, skip=10) (
          TextButton("Show Primitives") { _ => println(primitives.show(".*".r, ".*".r).mkString("\n")) },
          TextButton("Show Attributes")     { _ => println(primitives.show(".*".r, "StoredAttributeMap".r).mkString("\n")) },
          CaptionedCheckBox("Scaleable ", "Scale window by dragging edges")(autoScale),
        )

      for { num<-1 to 8} primitives(s"B$num")= TextButton(s"B$num"){_=> println(num) }
      for { num<-1 to 5} primitives(s"L$num")= Label(s"L$num")

    def turn(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
        val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
        import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
        import resolved._
        val degrees: Int = inheritedAttributes.Int("degrees", inheritedAttributes.Int("deg", 90*inheritedAttributes.Int("quads", 0)))
        def turn(glyph: Glyph): Glyph =
         degrees match {
          case 0 => glyph
          case d =>
            val glyph$ = glyph.turned(d.toFloat, false)
            glyph$
        }
        val derivedContext: Context = context.updated(inheritedAttributes.without("deg", "degrees", "quads"))
        val glyph = turn(NaturalSize.Row(align=Mid)(children.flatMap(translator.translate(derivedContext))))
        List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
      }

    def scale(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
      val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
      import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
      import resolved._
      val proportion = inheritedAttributes.Float("scale", 0)
      val derivedContext: Context = context.updated(inheritedAttributes.without("proportion"))
      val glyph = NaturalSize.Row(align=Mid)(children.flatMap(translator.translate(derivedContext))).scaled(proportion)
      List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
    }

    def frame(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
      val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
      import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
      import resolved._
      val radius: Scalar = inheritedAttributes.Float("radius", 0)
      val fg  = inheritedAttributes.Brush("fg", inheritedAttributes.Brush("frameforeground", Brushes.black))
      val bg  = inheritedAttributes.Brush("bg", inheritedAttributes.Brush("framebackground", Brushes.transparent))
      val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg"))
      val unframed = NaturalSize.Row(align=Mid)(children.flatMap(translator.translate(derivedContext)))
      val glyph = if (radius==0f) unframed.framed(fg, bg, radius = radius) else unframed.roundFramed(fg, bg, radius = radius)
      List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
    }

    def fixedWidth(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
      val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
      import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
      import resolved._
      val width: Scalar = inheritedAttributes.Units("width", 0)(context.sheet)
      val fg  = inheritedAttributes.Brush("fg", inheritedAttributes.Brush("foreground", Brushes.black))
      val bg  = inheritedAttributes.Brush("bg", inheritedAttributes.Brush("background", Brushes.transparent))
      val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg", "width"))
      val glyph = FixedSize.Row(align=Mid, width=width)(children.flatMap(translator.translate(derivedContext)))
      List(glyph)
    }



    primitives("turn") = StoredExtension (turn)
    primitives("rotate") = StoredExtension(turn)
    primitives("scale") = StoredExtension(scale)
    primitives("frame") = StoredExtension(frame)
    primitives("fixedwidth") = StoredExtension(fixedWidth)
  }


  lazy val source: Glyph =
    <div fontfamily="Times" width="400px" labelforeground="black" textforeground="black" cdatabackground="pink" cdataforeground="red"
         attributeswarning="f">
      <attributes id="class:but"    buttonbackground="yellow" buttonforeground="red" fontscale="0.9"/>
      <attributes id="tag:debug"    caption="Debugging" local="t" mark="MARK #1"/>
      <attributes id="tag:p"        align="justify" />
      <attributes id="class:fat"    fontscale="1.4"  align="justify"/>
      <attributes id="class:narrow" align="justify"  width="280px"  textforeground="black"/>

      <macro tag="courier" fontfamily="Courier"><?body?></macro>
      <macro tag="centred"><table cols="1" foreground="transparent" background="transparent"><?body?></table></macro>

      <centred>
        <glyph gid="buttons"/>
      <p hang=" * ">
        This application tests a combination of <span textforeground="green">local_ization of attributes</span>, <tt fontscale="1.2">text layout</tt>,
        hy_phenation, and the <courier fontscale="1.3">plugging</courier> in of <b>reactive glyphs,</b>  <aswell/>.
      </p>


      <p fontFamily="Courier" textforeground="black">
        This is Courier, and  <i>this is italic.</i> The text may well spill over &gt; one lines, &amp; everything
        depends on the width of the entire <tt>div</tt>.
      </p>
      <turn degrees="15">Things can be turned</turn>

      <p align="center"  fontFamily="Times" fontstyle="BOLDITALIC" fontScale="0.75" textforeground="black">
        This is centred text in a small scale bold-italic font.
      </p>

      <frame fg="red.2"><fixedwidth width="0.9*width">this text <fill fg="red" stretch="2"/> is spread</fixedwidth></frame>

      <scale scale=".6">
      <attributes id="tag:p" fontFamily="Courier">
        <p class="fat">
          This is a long, hyphen_at_able antidisestablishmentarianism tract_ified text concerning floccinaucinihilipilification. The text, in teletype font,
          spills ov_er a nar_row mar_gin un_less I have been <turn degrees="-5">mistakenly</turn><turn degrees="5">informed</turn> by my programming alter-ego.
        </p>
      </attributes>
      </scale>



      <table cols="2" padx="1em" pady="1ex" foreground="green.2" background="yellow">
          <p class="narrow">
            This is a long, possibly hyphenatable, "antidisestablishmentarianism" tract_ified text that spills
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
