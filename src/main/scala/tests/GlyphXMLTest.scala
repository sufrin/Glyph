package org.sufrin.glyph
package tests

import GlyphML.Context
import Glyphs.{blue, nothing, white}
import GlyphTypes.Scalar
import GlyphXML._
import Styles.{Decoration, GlyphStyle}

import scala.collection.immutable.ListMap

object GlyphXMLTest extends Application {

  def title: String = "GlyphXML"

  implicit val LocalStyle: StyleSheet =  new Styles.DefaultSheet {
    override def buttonFontSize: Scalar  = 22
    override def labelFontSize: Scalar   = 22
    override def labelStyle: GlyphStyle  = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)
  }//.unFramed

  implicit val local: Context =
    Context(style=LocalStyle)
      .fontFamily("Roman")
      .parSkip(0f)
      .fontSize(16)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)
      .frameStyle(Decoration.Blurred(fg=blue, blur=15f, spread=15f), fg=white)
      .paragraphEms(45)
      .withEntity("circle")(styled.TextButton("circle"){ _ => println("Circle Pressed")})
      .withEntity("button")(styled.TextButton("button"){ _ => println("Button Pressed")})
      .withEntity("RECT")(Glyphs.Rect(50, 30, fg=DefaultBrushes.red.strokeWidth(10), bg=DefaultBrushes.yellow)
                                .framed(fg=DefaultBrushes.green.strokeWidth(6), bg=DefaultBrushes.yellow))
      .withElement("square"){
         case (localAttributes, local) =>
          val w = localAttributes.Units("w", 10f)(local)
          val h = localAttributes.Units("h", 10f)(local)
          Glyphs.Rect(w,h,fg=local.fg, bg=local.bg)
      }
      .withReaction("act1"){
        _ => println("act1")
      }
      .withReaction("act2"){
        _ => println("act2")
      }
      .withAttrs("#button")(ListMap("hover"->"yellow", "down"->"red", "bg"->"darkGrey", "up"->"blue", "label"->"Default Button",
                                    "framed"->"darkGrey"))
      .withAttrs("#but")(ListMap("hover"->"green", "down"->"blue",  "up"->"yellow", "label"->"Another Default Button",
        "framed"->"blue"))
      .withAttrs("thisButton")(ListMap("label"->"Another Default Button", "framed"->"blue/10"))

  val square = <square fg="red" bg="blue" w="5.5em" h="2.5em"/>
  val button = <button action="act1" label="Act 1" id="thisButton" class="#but"/>
  val embedded = <p align="center">embedded &button; for you</p>

  import org.sufrin.SourceLocation.sourceLocation

  def GUI =
    XML.Col(
      <body width="55em" fontFamily="Courier" fontSize="20" align="justify" parSkip="10pt" source={s"$sourceLocation"}>
        <p>
          This is an experiment in defining GUIs using
          (mainly) <b>XML</b>
        </p>
        <p framed="yellow/16/ROUND" bg="green">
          GlyphXML is a domain specific language expressed as XML: its elements denote sequences.
        </p>


        <p fg="red"><i>Its <n>API</n> &amp; xml-based markup may be somewhat more
          convenient for interface designers &RECT;
          than the standard Glyphs API.</i></p>
        <![CDATA[
  <p>
    <i>Its <n>API</n> may be somewhat more convenient
    for interface designers than the standard Glyphs API.</i>
  </p>
  ]]>
        <p align="center">Here are some embedded glyphs (3 rows x 2 columns wide)</p>
        <table cols="2" fontScale="0.75">
          <verb>{button}</verb>   {button}
          <verb>{square}</verb>   {square}
          <verb>{embedded}</verb> {embedded}
        </table>
        <col alignment="center" fg="red">

          <verb framed="red">able <i>was</i> I</verb>
          &circle;
        </col>


        <div align="center" fontScale="0.8">
          <p align="center">Here are some grids and tables</p>
          <grid cols="4" source={s"$source"}>
            <row><i>grid(3,0)</i> <grid columns="3" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X--</grid></row>
            <row><i>grid(4,0)</i> <grid columns="4" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X-- </grid></row>
            <row><i>grid(0,3)</i> <grid rows="3" fg="blue" >1 2 3 -4- 5 6 7 8 9 X</grid></row>
            <row><i>grid(0,4)</i> <grid rows="4" fg="blue" >1 2 3 -4- 5 6 7 8 9 X</grid></row>

            <row><i>rows(3,0)</i> <rows columns="3" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X-- Y</rows></row>
            <row><i>rows(4,0)</i> <rows columns="4" fg="blue" >1 2 3 --4-- 5 6 7 8 9 --X-- Y</rows></row>
            <row><i>cols(0,3)</i> <cols rows="3" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X-- Y</cols></row>
            <row><i>cols(0,4)</i> <cols rows="4" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X-- Y</cols></row>

            <row><i>table(3,0)</i> <table columns="3" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X-- Y</table></row>
            <row><i>table(4,0)</i> <table columns="4" fg="blue" >1 2 3 -4- 5 6 7 8 9 --X-- Y</table></row>
            <row><i>table(0,3)</i> <table rows="3" fg="blue" >1 2 3 -4- 5 6 7 8 9 X Y</table></row>
            <row><i>table(0,4)</i> <table rows="4" fg="blue" >1 2 3 -4- 5 6 7 8 9 X Y</table></row>
          </grid>
        </div>

      </body>
    )

}
