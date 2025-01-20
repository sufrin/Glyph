package org.sufrin.glyph
package tests

import GlyphXML._
import tests.DemonstrationNotebook.pageStyle

import org.sufrin.SourceLocation.SourceLocation

import scala.collection.immutable.ListMap
import scala.xml._

object TestGXML extends Application {
  val defs = new GlyphXML {}




  /**
   * Applied when an (outermost) xml `Elem`ent is intended to denote a `Glyph`. This
   * translates the element in a context that records its source location in scala text.
   */
  implicit def XML(elem: Elem)(implicit source: SourceLocation): Glyph = {
    val within = List("")
    NaturalSize.Col().atLeft$(defs.translate(List(s"$source"))(within)(elem)(Map.empty)(new Sheet()))
  }


  DefaultBrushes.buttonPointSize = 16
  val PRESSME = ReactiveGlyphs.TextButton("PRESS ME"){ _ => println("Pressed")}.framed()
  val QUIT    = ReactiveGlyphs.TextButton("QUIT"){ _ => println("Quit Pressed")}.framed()
  // Definitions whose scope is the translation.
  defs("PRESSME")  = PRESSME
  defs("QUIT")     = QUIT
  defs("TWIT")     = ReactiveGlyphs.TextButton("TWIT"){ _ => println("Twit Pressed")}.framed()
  defs("#p")       = <ATTTRIBUTES framed="true" frame="red"/>
  defs("#row")     = <ATTRIBUTES framed="true" frame="green"/>
  defs("unframed") = <ATTRIBUTES framed="true" frame="green"/>
  defs("DIFFB")    = ReactiveGlyphs.TextButton("A DIFFERENT BUTTON"){ _ => println("Different Pressed")}
  defs("DIFFB2")   = ReactiveGlyphs.TextButton("A DIFFERENT BUTTON"){ _ => println("Different2 Pressed")}.framed()
  defs("BUTTONS")  = NaturalSize.Row().centered(PRESSME.copy(), QUIT.copy()).rotated(2)
  defs("FLOCCI")   = "Flocci_nauci_nihil_ipil_if_icat_ion"

  val test0w = "50em"
  val test0 =
    <body fontFamily="Menlo" fontScale="1" fontSize="16" width={test0w} align="justify" parSkip="0.4ex" framed="0XFF227722/1" padX="3em" padY="3ex" background="yellow" textBackground="yellow">
      <p>This is a test of  <i>paragraphing</i> and related features. It is justified in a width of {test0w}
          with an overall <row rotated="1"><b>parSkip</b></row> of <row rotated="2"><b>3.5ex</b></row>.
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
      <row width={test0w}><fill/><glyph ref="PRESSME"/><fill/><glyph ref="QUIT"/></row>
      <p>This is an ordinary paragraph that has ''<nb/>&FLOCCI;<nb/>'' in it, as well as the button $TWIT</p>
      <p>Here is  $DIFFB and here it is again maybe, maybe...</p>

      <glyph ref="DIFFB2"/>
      <glyph ref="BUTTONS"/>
      <p rightMargin="6em">
        This is a paragraph narrowed by 6em. It should still
        be justified at its right margin.
      </p>
      <p leftMargin="12em" rightMargin="8em">
        This is another indented and potentially much-_hyphen_ated para_graph end_ing in the hyph_enated long word
        ''<nb/>&FLOCCI;<nb/>'' --<nb/><b>and this is bold</b><nb/>.
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
        the extra space on a line is evenly divided between the start and the end of the line.
      </p>
    </body>


  val table1 =
    <table cols="3" padY="2em" padX="2em" background="yellow" textBackground="yellow">
      1 2 3 4
        <col>5a 5b 5c</col>
        6 7 8

        10 11
        <glyph ref="TWIT" copy="true"/>
      </table>
  defs("table1") = table1


  val table2 =
    <table rows="3" padY="2em" padX="2em" background="yellow" textBackground="yellow">
      1 2 3 4
      <col>5a 5b 5c</col>
      6 7 8 9 10 11
      <glyph ref="QUIT" copy="true"/>
    </table>

  defs("table2") = table2.rotated(1)

  val gridsWidth = s"${(table1.w + table2.h)*1.1f}pt"


  val test1 =
    <body width="50em" fontFamily="Menlo" fontScale="1" fontSize="16"
          align="justify" parSkip="0.4ex" padX="3em" padY="3ex" background="yellow" textBackground="yellow" class="unframed">
      <col class="unframed" align="center">
        <p class="unframed">Grids, with some deliberate errors in glyph references</p>
        <p class="unframed">FIX: buttons in rotated tables/grids/...</p>

        <row class="unframed" width={gridsWidth}  align="top">
        <fill/>
          <col class="unframed">
            <i>Columns: </i>
            $table1
          </col>
        <fill stretch="3"/>
          <col class="unframed">
            <i>Rows:</i>
            $table2
          </col>
        <fill/>
      </row>

        <!-- glyph ref="table1" copy="true"/ -->
      </col>
    </body>

  val noteBook: Notebook = Notebook()
  val Page: noteBook.DefinePage.type = noteBook.DefinePage

  Page("Paragraphs", "")(test0)
  Page("Grids", "")(test1)
  Page("Spaces", "")(
    <body width="40em">
      <p>Punctuation --<nb/><i>&FLOCCI;<nb/></i>.</p>
    </body>
  )

  val title = "TestGXML"
  val GUI: Glyph = noteBook.Layout.rightButtons()
}



