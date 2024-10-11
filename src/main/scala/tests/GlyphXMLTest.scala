package org.sufrin.glyph
package tests

import GlyphML.Context
import Glyphs.{blue, nothing, white}
import GlyphTypes.Scalar
import GlyphXML._
import Styles.{Decoration, GlyphStyle}

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

  val square = <square w="2em" h="2em" fg="red"/>
  val button = <button action="act1" fontScale="1.3" hover="lightGrey" fg="red" bg="yellow" label="Button Act1" framed="blue4"/>
  val embedded = <p>embedded &button; for you</p>

  def GUI =
    XML.Col(
      <body width="45em" fontFamily="Courier" fontSize="20" align="justify" parSkip="5pt">
        <p>
          This is the start of an experiment in defining GUIs using
          (mainly) <b>XML</b>
        </p>
        <p>
          GlyphXML is a domain specific language expressed as XML: its elements denote sequences.
        </p>
        <square fg="red" bg="blue" w="4.5em" h="4.5em"/>
        <p fg="red"><i>Its <n>API</n> &amp; xml-based markup may be somewhat more
          convenient for interface designers <col>{square}</col>
          than the standard Glyphs API.</i></p>
        <col alignment="center">
          <p align="center">Here are some embedded glyphs</p>
          <verb>{button}</verb>
          {button}
          <verb>{square}</verb>
          {square}
          <verb>{embedded}</verb>
          {embedded}
        </col>
        <col alignment="center" fg="red">
          <![CDATA[
  <p>
    <i>Its <n>API</n> may be somewhat more convenient
    for interface designers than the standard Glyphs API.</i>
  </p>
  ]]>
          <verb framed="red">able <i>was</i> I</verb>
          &circle;
        </col>

        <div align="center">
        <rows width="15em" colWidth="2" fg="red" bg="lightGrey">
          <p bg="lightGrey"><i>the rain</i></p><p><b>in spain</b></p>
          <verb>Some verbatim</verb><verb>Material</verb>
          <p><i>falls © mainly</i></p><p><b>in the &amp; plain</b></p>
          <col>
            <p>X</p><p>Y</p>
          </col>
        </rows>
        </div>

        <div align="center">
          <cols width="15em" rowHeight="3" fg="blue" >
            <p bg="lightGrey"><i>the rain</i></p><p><b>in spain</b></p>
            <verb>Some verbatim</verb><verb>Material</verb>
            <p><i>falls mainly</i></p><p><b>in the plain</b></p>
          </cols>
        </div>

      </body>
    )

}
