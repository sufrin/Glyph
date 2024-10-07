package org.sufrin.glyph
package tests

import GlyphML.Context
import Glyphs.{blue, nothing, white, FilledOval}
import GlyphTypes.Scalar
import GlyphXML._
import Styles.{Decoration, GlyphStyle}

object GlyphXMLTest extends Application {

  def title: String = "GlyphXML"

  implicit object LocalStyle extends Styles.DefaultSheet {
    override def buttonFontSize: Scalar  = 22
    override def labelFontSize: Scalar   = 22
    override def labelStyle: GlyphStyle  = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)
  }

  implicit val local: Context =
    Context(style= LocalStyle)
      .fontFamily("Roman")
      .parSkip(40f)
      .fontSize(20)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)
      .frameStyle(Decoration.Blurred(fg=blue, blur=15f, spread=15f), fg=white)
      .paragraphEms(45)
      .withGlyph("circle")(FilledOval(100, 100, fg=blue))
      .withElement("square"){
        { case (localAttributes, local) =>
          val w = localAttributes.Float("w", 10f)
          val h = localAttributes.Float("h", 10f)
          Glyphs.Rect(w,h,fg=local.fg, bg=local.bg) }
      }

  val circle = <copy id="circle" fg="blue"/>

  def GUI =
    XML.Col(
      <xml width="45em" fontFamily="Courier" fontSize="32" align="justify" parSkip="20">
        <p>
          <b>GlyphXML</b>is a domain specific language expressed as XML: its elements denote <i>Glyph sequences.</i>
        </p>
        <square fg="red" bg="blue" w="45" h="45"/>
        <p fg="red"><i>Its <n>API</n> may  be  somewhat more convenient for interface designers than the standard Glyphs API.</i></p>
        <col alignment="center" fg="red">
          <p>
            <i>Its <n>API</n> may  be somewhat more convenient for interface designers than the standard Glyphs API.</i>
          </p>
          <verb framed="red">able <i>was</i> I</verb>
          {circle}
        </col>
        <p>
          This is the start of an experiment in defining GUIs using
          (mainly) <b>XML.</b>
        </p>
        <xml align="center">
        <rows width="15em" colWidth="2" fg="red" bg="lightGrey">
          <p bg="lightGrey"><i>the rain</i></p><p><b>in spain</b></p>
          <verb>Some verbatim</verb><verb>Material</verb>
          <p><i>falls © mainly</i></p><p><b>in the &amp; plain</b></p>
          <col>
            <p>X</p><p>Y</p>
          </col>
        </rows>
        </xml>

        <xml align="center">
          <cols width="15em" rowHeight="3" fg="blue" >
            <p bg="lightGrey"><i>the rain</i></p><p><b>in spain</b></p>
            <verb>Some verbatim</verb><verb>Material</verb>
            <p><i>falls mainly</i></p><p><b>in the plain</b></p>
          </cols>
        </xml>

      </xml>
    )

}
