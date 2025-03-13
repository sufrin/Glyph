package org.sufrin.glyph
package tests

import NaturalSize.{Col, Row}

import org.sufrin.glyph.ReactiveGlyphs.TextButton

import scala.xml.Elem

object ResizeableTest extends Application {
  import glyphXML.Language._

  implicit val style: StyleSheet = StyleSheet(windowDiagonal = Vec(400,150))

  def guiSpec(implicit style: StyleSheet): Glyph = Col(align=Center)(
    { val Vec(w, h) = style.windowDiagonal
      lazy val wider: Glyph = TextButton(s"> $w") {
        _ =>  println(s"SETTING $w -> ${(w*1.2f).toInt}")
          wider.guiRoot.setContentSize(Vec(w*1.2f, h))
      }
      lazy val narrower: Glyph = TextButton(s"< $w") {
        _ =>
          println(s"SETTING $w -> ${(w*1.2f).toInt}")
          wider.guiRoot.setContentSize(Vec(w*0.9f, h))
      }
      FixedSize.Row(w)(wider, FixedSize.Space(5,5,1), narrower)
    },
    <div width="0.99*windowwidth" align="justify" frame="red/4/ROUND">
      <p>
        This is a very long piece of text that may change shape as the win_dow chan_ges shape.
        Or it may not.
        This is a very long piece of text that may change shape as the win_dow chan_ges shape.
        Or it may not.
        This is a very long piece of text that may change shape as the win_dow chan_ges shape.
        Or it may not.
        This is a very long piece of text that may change shape as the win_dow chan_ges shape.
        Or it may not.
      </p>
    </div>
  )

  val GUI: Glyph = styled.Resizeable(style => guiSpec(style))

  def title: String = "Resizeable Test"

  override def whenStarted(): Unit = {
    println(s"STARTED $GUI")
    Application.confirmCloseRequestsFor(GUI)
    styled.Resizeable.loggingLevel(org.sufrin.logging.ALL)
  }
}