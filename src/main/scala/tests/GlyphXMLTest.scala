package org.sufrin.glyph
package tests

import GlyphML.Context
import Glyphs.{blue, nothing, white}
import GlyphTypes.Scalar
import GlyphXML.COLUMN
import Styles.{Decoration, GlyphStyle}

object GlyphXMLTest extends Application {

  def title: String = "GlyphXML"

  object LocalStyle extends Styles.DefaultSheet {
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

  def GUI: Glyph = GUIMarkup.toGlyph(local)

  val x = <i>the rain in spain</i>

  def GUIMarkup =
    COLUMN(
      <xml fontFamily="Courier" fontSize="32" align="justify" parSkip="20">
        <p>
          {x}
          <b>GlyphXML</b>is a domain specific language expressed as XML: its elements denote <i>Glyph sequences.</i>
        </p>
        <p fg="red"><i>Its <n>API</n> may  be  somewhat more convenient for interface designers than the standard Glyphs API.</i></p>
        <!--row alignment="left" fg="red"-->
          <p><i>Its <n>API</n> may  be somewhat more convenient for interface designers than the standard Glyphs API.</i></p>
          able <i>was</i>I
        <!--/row--><!--
        <verb  bg="yellow" fontScale="1.3" fontFamily="Courier">
          <p l="2" r="2">
            <i fg="red">Its <n>API</n> may  be somewhat
              more convenient for interface
              designers than the standard
              Glyphs API.
            </i>
          </p>
        </verb>
        -->
        <p bg="lightGrey">
          This is the start of an experiment in defining GUIs using
          (mainly) <b>XML.</b>
        </p>
        <xml align="center">
        <rows width="200" colWidth="2" fg="red" bg="lightGrey">
          <p bg="lightGrey"><i>the rain</i></p><p><b>in spain</b></p>
          <verb>Some verbatim</verb><verb>Material</verb>
          <p><i>falls mainly</i></p><p><b>in the plain</b></p>
          <col>
            <p>X</p><p>Y</p>
          </col>
        </rows>
        </xml>

        <xml align="center">
          <cols width="200" rowHeight="3" fg="blue" >
            <p bg="lightGrey"><i>the rain</i></p><p><b>in spain</b></p>
            <verb>Some verbatim</verb><verb>Material</verb>
            <p><i>falls mainly</i></p><p><b>in the plain</b></p>
          </cols>
        </xml>

      </xml>
    )

}
