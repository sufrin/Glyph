package org.sufrin.glyph
package glyphML
package tests

import glyphML.Translator

object paralayout extends Application {
    import Translator._
    val translator = new Translator
    import translator._

    HYPHENATION("flocci/nauci/nihil/ipil/ifica/tion")("/")
    HYPHENATION("anti_dis_estab_lish_men_t_arian_ism")("_")
    HYPHENATION("averywidewordwithaninfeasible/breakpoint")("/")

    val source: Glyph =
      <div fontfamily="Menlo" width="40em" textforeground="red" cdatabackground="pink" cdataforeground="red" frameparagraphs="red.0.dashed(3,3)">
        <p align="justify" hang=" * ">Menlo the rain in spain <span textforeground="green">falls mainly</span> in the plain, and may go further.</p>
        <p align="justify"  fontFamily="Courier" fontscale="0.9" textforeground="black">Courier the <i>italic font</i> rain in spain may well spill over two lines if I am not mistaken.</p>
        <p align="center"  fontFamily="Arial" fontScale="0.75" textforeground="black">Arial the rain in spain may well spill over two lines if I am not mistaken.</p>
        <debug local="t" tree="t" caption="DEBUG1" mark="HERE IS DEBUG1" fontscale="0.9" labelforeground="red" framed="blue.4.dashed(10,10)">
          <![CDATA[
            this < is
        quoted text >
      ]]>
        </debug>
        <p align="justify"  width="400px" fontFamily="Arial" fontscale="1" textforeground="black">
          This is going to be a long, hyphen_at_able antidisestablishmentarianism tract_ified text <tt textbackground="pink" fontscale="1.1">tele_type font</tt>
          that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed.
        </p>

        <p align="justify"  width="300px" fontFamily="Menlo" fontscale="0.8" textforeground="black">
          This is going to be a long, hyphen_at_able "antidisestablishmentarianism" tract_ified text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
        </p>
        <p align="justify"  width="300px" fontFamily="Arial" fontscale="0.8" textforeground="black">
          This is going to be a long, goddam, floccinaucinihilipilification text that spills ov_er a  nar_row mar_gin un_less I am mis_tak_enly in_formed
          by the fans of antidisestablishmentarianism or floccinaucinihilipilification at the end of a sen_tence.
        </p>
        <p align="justify"  width="200px" fontFamily="Arial" fontscale="0.8" textforeground="black">
          This is going to be a long, goddam, floccinaucinihilipilification text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
          by the fans of antidisestablishmentarianism or floccinaucinihilipilification or averywidewordwithaninfeasiblebreakpoint at the end of a sen_tence.
        </p>
      </div>


    val GUI: Glyph = source.enlarged(20).framed(Brushes.blackFrame)

    def title: String = "Test Translator"

}
