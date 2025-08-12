package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator

import org.sufrin.glyph.NaturalSize.Col
import org.sufrin.glyph.glyphXML.PrettyPrint
import org.sufrin.glyph.unstyled.reactive.Reaction
import org.sufrin.logging.{FINE, INFO, OFF, SourceDefault, WARN}
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
    import primitives.Define

    HYPHENATION("flocci/nauci/nihil/ipil/ifica/tion")("/")
    HYPHENATION("anti_dis_estab_lish_men_t_arian_ism")("_")
    HYPHENATION("averywidewordwithaninfeasible/breakpoint")("/")

    // Define("tag:debug",   ListMap("local"->"t", "tree"->"t", "mark"->"generic debug mark", "labelforeground"->"red"))
    // Define("class:debug", ListMap("a"->"aa", "b"->"bb", "caption" -> "class caption"))
    // Define("class:but",   ListMap("buttonbackground"->"yellow", "fontscale"->"0.7"))
    // Define("class:but",   <attributes buttonbackground="yellow" fontscale="0.7"/>)

    locally { for { num<-1 to 8} Define(s"B$num", delayed.TextButton(s"B$num"){_=>})}
    locally { for { num<-1 to 5} Define(s"L$num", delayed.Label(s"L$num"))}

    Define("col",       delayed.Column(List(delayed.Label("C1"), delayed.Label("C2"))))
    Define("row",       style => NaturalSize.Row(styled.Label("This is a")(style).framed(), styled.Label(" long row")(style)))
    Define("buttoncol", delayed.Column(List(
                        delayed.TextButton("Primitives"){_=> import PrettyPrint._;  primitives(".*".r, ".*".r).toSeq.prettyPrint()},
                        delayed.TextButton("Attributes"){_=> import PrettyPrint._;  primitives(".*".r, "StoredAttributeMap".r).toSeq.prettyPrint()},
                        delayed.TextButton("Nothing"){_=>})))

  lazy val source: Glyph =
      <div fontfamily="Menlo" width="40em" textforeground="red" cdatabackground="pink" cdataforeground="red">
        <attributes id="class:but" buttonbackground="yellow" buttonforeground="red" fontscale="0.7"/>
        <attributes id="tag:debug" caption="Debugging" local="t" mark="MARK #1"/>
        <attributes id="tag:p" framed="red"/>

        <p align="justify" hang=" * ">Menlo the rain in spain <span textforeground="green">falls mainly</span> in the plain, and may go further.</p>
        <p align="justify"  fontFamily="Courier" fontscale="0.9" textforeground="black">Courier the <i>italic font</i> rain in spain may well spill over two lines if I am not mistaken.</p>
        <p align="center"  fontFamily="Arial" fontScale="0.75" textforeground="black">Arial the rain in spain may well spill over two lines if I am not mistaken.</p>
        <debug fontscale="0.9"  framed="blue.4.dashed(10,10)">
          <glyph gid="B1" class="but"/>
          <glyph gid="B4" id="herebut"/>
          <glyph gid="buttoncol" class="but"/>
          <glyph gid="unk"/>
        </debug>

        <attributes id="tag:p" align="right" width="400px" fontFamily="Courier" >
          <p  fontscale="1" textforeground="black">
            This is going to be a long, hyphen_at_able antidisestablishmentarianism tract_ified text <tt textbackground="pink" fontscale="1.1">tele_type font</tt>
            that spills ov_er a nar_row mar_gin un_less I  am mis_tak_enly in_formed.
          </p>
        </attributes>

        <p align="justify"  width="300px" fontFamily="Menlo" fontscale="0.8" textforeground="black">
          <glyph gid="B2" class="but"/>This is going to be a long, possibly <glyph gid="B3" class="but"/> hyphen_at_able "antidisestablishmentarianism" tract_ified text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
        </p>
        <p align="justify"  width="300px" fontFamily="Arial" fontscale="0.8" textforeground="black">
          This is going to be a long, goddam, floccinaucinihilipilification text that spills ov_er a  nar_row mar_gin un_less I am mis_tak_enly in_formed
          by the fans of antidisestablishmentarianism <glyph gid="col"/> or floccinaucinihilipilification at the end of a sen_tence.
        </p>
        <p align="justify"  width="200px" fontFamily="Arial" fontscale="0.8" textforeground="black">
          This is going <glyph gid="row"/> to be a long, goddam, floccinaucinihilipilification text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
          by the fans of antidisestablishmentarianism <glyph gid="buttoncol"/> or floccinaucinihilipilification or averywidewordwithaninfeasiblebreakpoint at the end of a sen_tence.
        </p>
      </div>


    lazy val GUI: Glyph = { logging.SourceDefault.level=WARN; source.enlarged(20).framed(Brushes.blackFrame) }

    def title: String = "Test Translator"

}
