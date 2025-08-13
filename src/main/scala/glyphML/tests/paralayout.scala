package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator

import org.sufrin.glyph.NaturalSize.Col
import org.sufrin.glyph.glyphXML.PrettyPrint
import org.sufrin.glyph.unstyled.reactive.Reaction
import org.sufrin.logging.{FINE, FINEST, INFO, OFF, SourceDefault, WARN}
import org.sufrin.logging

import scala.collection.immutable.ListMap

object paralayout extends Application {
    import Translator._



    object delayed {

      def Label(text: String): StyleSheet=>Glyph  = (style: StyleSheet)=>styled.Label(text)(style)
      def TextButton(name: String)(reaction: Reaction) = (style: StyleSheet)=>styled.TextButton(name)(reaction)(style)

      def Column(glyphs: Seq[(StyleSheet=>Glyph)]): StyleSheet=>Glyph =
      {case style: StyleSheet  => {
        val refied = glyphs.map(_.apply(style))
        Col(align=Left)(refied)
      }}

    }

    val translator = new Translator(new ValueStore {})
    import translator._

    HYPHENATION("flocci-nauci-nihil-ipil-ifica-tion")("-")
    HYPHENATION("hyphen-at-able")("-")
    HYPHENATION("in-formed")("-")
    HYPHENATION("mis-tak-enly")("-")
    HYPHENATION("anti-dis-estab-lish-men-t-arian-ism")("-")
    HYPHENATION("averywidewordwithaninfeasible-breakpoint")("-")


    locally { for { num<-1 to 8} primitives(s"B$num")=delayed.TextButton(s"B$num"){_=> println(num) } }
    locally { for { num<-1 to 5} primitives(s"L$num")=delayed.Label(s"L$num") }

    primitives("pfft") = <span>as well as some tag-extending features</span>

    primitives("col")=delayed.Column(List(delayed.Label("C1"), delayed.Label("C2")))
    primitives("row")=style => NaturalSize.Row(styled.Label("This is a")(style).framed(), styled.Label(" long row")(style))
    primitives("buttoncol") =
      delayed.Column(List(
                        delayed.TextButton("Show all Primitives"){_=> import PrettyPrint._;  primitives(".*".r, ".*".r).toSeq.prettyPrint()},
                        delayed.TextButton("Show Attributes"){_=> import PrettyPrint._;  primitives(".*".r, "StoredAttributeMap".r).toSeq.prettyPrint()},
                        delayed.TextButton("Nothing"){_=>}))

  lazy val source: Glyph =
      <div fontfamily="Times" width="400px" labelforeground="black" textforeground="black" cdatabackground="pink" cdataforeground="red">
        <attributes id="class:but" buttonbackground="yellow" buttonforeground="red" fontscale="0.7"/>
        <attributes id="tag:debug" caption="Debugging" local="t" mark="MARK #1"/>
        <attributes id="tag:p" framed="red" align="justify" />
        <attributes id="class:fat"  fontscale="1.4"  align="justify"/>
        <attributes id="class:narrow"  align="justify"  width="200px"  textforeground="black"/>

        <p hang=" * ">
          This application tests a combination of <span textforeground="green">local_ization of attributes</span>, <tt fontscale="1.2">text layout</tt>,
          hy_phenation, and the plugging in of <b>reactive glyphs,</b> <pfft/>.
        </p>

        <p fontFamily="Courier" textforeground="black">
          This is Courier, and  <i>this is italic.</i> The text may well spill over &gt; one lines, &amp; everything
          depends on the width of the entire <tt>div</tt>.
        </p>

        <p align="center"  fontFamily="Times" fontstyle="BOLDITALIC" fontScale="0.75" textforeground="black">
          This is centred text in a small scale bold-italic font.
        </p>

        <glyph gid="buttoncol" class="but"/>
        <glyph gid="unk"/>

        <attributes id="tag:p" fontFamily="Courier" framed="blue">
          <p class="fat">
            This is a long, hyphen_at_able antidisestablishmentarianism tract_ified text concerning floccinaucinihilipilification. The text, in teletype font,
            spills ov_er a nar_row mar_gin un_less I have been mistakenly informed by my programming alter-ego.
          </p>
        </attributes>

        <p class="narrow">
          This is a long, possibly hyphenatable, "antidisestablishmentarianism" tract_ified text that spills
          ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed.
        </p>
        <p class="narrow">
          This piece of text contains floccinaucinihilipilification averywidewordwithaninfeasiblebreakpoint, but nothing else.
        </p>
        <deliberatelyundefinedtag/>
      </div>


    lazy val GUI: Glyph = {
      logging.SourceDefault.level=WARN
      Translator.HYPHENATION.level=WARN
      source.enlarged(20).framed(Brushes.blackFrame)
    }

    def title: String = "Test Translator"

}
