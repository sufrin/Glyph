package org.sufrin.glyph
package tests

import GlyphXMLOld._
import tests.DemonstrationNotebook.pageStyle

import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.glyphXML.Translation
import org.sufrin.glyph.glyphXML.Translation.Target.{ColTarget, Target}
import org.sufrin.glyph.glyphXML.Visitor.AttributeMap

import scala.xml._

object TestGXML extends Application {

  val translator = new Translation {
    meaning("body") = new Translation(primitives) {
      override def toString: String = "body translation"
      override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
        val child$ = child.filterNot(Translation.isBlank(_))
        List(ColTarget(sheet.backgroundBrush, chunks=super.translate(tags, false, attributes, sheet, child$)))
      }
    }

    def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation(primitives) {
      override def toString: String = s"StyleTranslation($tag, $textStyle)"
      override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
        super.translate(tag::tags, paragraph, attributes.updated("textStyle", textStyle), sheet, child)
      }
    }

    meaning("i")  = textStyleTranslation("i",  "Italic")
    meaning("b")  = textStyleTranslation("b",  "Bold")
    meaning("bi") = textStyleTranslation("bi", "BoldItalic")
    meaning("n")  = textStyleTranslation("n",  "Normal")
  }

  import translator.XMLtoGlyph

  DefaultBrushes.buttonPointSize = 16
  // Definitions whose scope is the translation.

  translator("QUIT")     = { implicit sheet => sheeted.TextButton("QUIT"){ _ => println("Quit Pressed")}.framed() }
  translator("PRESSME")  = { implicit sheet => sheeted.TextButton("PRESS ME"){ _ => println("Pressed")}.framed() }
  translator("TWIT")     = { implicit sheet => sheeted.TextButton("TWIT"){ _ => println("Twit Pressed")}.framed()}
  translator("p:tag")    = <ATTTRIBUTES framed="true" frame="red"/>
  translator("row:tag")  = <ATTRIBUTES framed="true" frame="green"/>
  translator("unframed") = <ATTRIBUTES framed="true" frame="green"/>
  translator("DIFFB")    = { implicit sheet => sheeted.TextButton("A DIFFERENT BUTTON"){ _ => println("Different Pressed")}(sheet) }
  translator("DIFFB2")   = { implicit sheet => sheeted.TextButton("A DIFFERENT BUTTON"){ _ => println("Different2 Pressed")}(sheet).framed() }
  translator("BUTTONS")  = <col><glyph gid="QUIT"/><glyph gid="PRESSME"/><glyph gid="TWIT"/></col>
  translator("FLOCCI")   = "Flocci_nauci_nihil_ipil_if_icat_ion"


  val test0w = "50em"
  val test0 : Node =
    <body fontFamily="Menlo" fontScale="1" fontSize="16" width={test0w} align="justify" parSkip="0.4ex" framed="0XFF227722/1" padX="3em" padY="3ex" background="yellow" textBackground="yellow">
      <glyph gid="QUIT"/>
      <p>This is a test of  <i>paragraphing</i> and related features. It is justified in a width of {test0w}
          with an overall <b>parSkip </b> of <b>3.5ex</b>.
      </p>
      <p>
        This is some chatter to force the
        first part of a <b>paragraph</b> to the right edge.
        You are about to see an indent within the paragraph.
      </p>
      <p leftMargin="8em">
        This is a paragraph indented by 8em. It should still extend to the
        rightmost margin and be justified  there.
      </p>

      <row width={test0w}><fill/><glyph gid="PRESSME"/><fill/><glyph gid="QUIT"/></row>
      <p>This is an ordinary paragraph that has ''&FLOCCI;'' in it.</p>

      <p><glyph gid="QUIT"/>
      <glyph gid="BUTTONS"/></p>

      <p rightMargin="6em">
        This is a paragraph narrowed by 6em. It should still
        be justified at its right margin.
      </p>
      <p leftMargin="12em" rightMargin="8em">
        This is another indented and potentially much-_hyphen_ated para_graph end_ing in the hyph_enated long word
        ''&FLOCCI;'' <b>and this is bold</b>.
      </p>
      <p leftMargin="8em" rightMargin="8em">
        This is a paragraph both indented and narr_owed by 8em.
        Its text should be right-justified, but as a whole it should appear obv_iously  centred
        in the space occupied by the paragraph.
      </p>
      <p align="right">
        This is some right-justified chatter. By right-justified I mean only that
        the extra space on a line appears at the start of the line.
      </p>
      <p align="center">
        This is some center-justified chatter. By center-justified I mean  that
        the extra space on a line is evenly divided
        between the start and the end of the line.
      </p>
      <p align="center">GOODBYE</p>
    </body>


  val table1 =
    <table cols="3" padY="2em" padX="2em" background="yellow" textBackground="yellow">
      1 2 3 4
        <col>5a 5b 5c</col>
        6 7 8
        10 11
        <glyph ref="TWIT" copy="true"/>
      </table>

  translator("table1") = table1


  val table2 =
    <table rows="3" padY="2em" padX="2em" background="yellow" textBackground="yellow">
      1 2 3 4
      <col>5a 5b 5c</col>
      6 7 8 9 10 11
      <glyph ref="QUIT" copy="true"/>
    </table>
  translator("table2") = table2

  val gridsWidth = "70em" // s"${(table1.w + table2.h)*1.1f}pt"


  val test1  =
    <body width="50em" fontFamily="Menlo" fontScale="1" fontSize="16"
          align="justify" parSkip="0.4ex" padX="3em" padY="3ex" background="yellow" textBackground="yellow" class="unframed">
      <col class="unframed" align="center">
        <p class="unframed">Grids, with some deliberate errors in glyph references</p>
        <p class="unframed">FIX: buttons in rotated tables/grids/...</p>

        <row class="unframed" width={gridsWidth}  align="top">
        <fill/>
          <col class="unframed">
            <i>Columns: </i>
            <glyph gid="table1"/>
          </col>
        <fill stretch="3"/>
          <col class="unframed">
            <i>Rows:</i>
            <glyph gid="table2"/>

          </col>
        <fill/>
      </row>

        <!-- glyph ref="table1" copy="true"/ -->
      </col>
    </body>

  val noteBook: Notebook = Notebook()
  val Page: noteBook.DefinePage.type = noteBook.DefinePage

  implicit val sheet: Sheet = Sheet()

  Page("Paragraphs", "")(test0)
  //Page("Grids", "")(test1)
  //Page("Spaces", "")( <body width="40em"> <p>Punctuation --<nb/><i>&FLOCCI;<nb/></i>.</p> </body>)

  val title = "TestGXML"
  val GUI: Glyph = noteBook.Layout.rightButtons()
}



