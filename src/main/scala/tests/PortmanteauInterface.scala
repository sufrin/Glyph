package org.sufrin.glyph
package tests
import GlyphTypes.Window
import styled.text.{Label, Paragraphs}
import NaturalSize.{Col, Row}
import Styles.NotebookStyle

class PortmanteauInterface(implicit val style: StyleSheet) extends Notebook {
  implicit val PageStyle: NotebookStyle = style.notebookStyle
  def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
    import windowdialogues.Dialogue.OKNO
    val prompt = Row.centered(PolygonLibrary.closeButtonGlyph scaled 5 enlarged 50,
      Label("Do you want to Exit?") scaled 1.5f
    ).enlarged(50)
    OKNO(prompt,
      title = "Exit Dialogue", ok = " Exit now ", no = " Continue ").InFront(glyph).andThen(close => if (close) window.close())
  }

  Page("Welcome", "") {
    Col.centered(
      Paragraphs(30, Justify)(
        """
          | Welcome to the Portmanteau Notebook: its source code is more
          |modular than that of DemonstrationNotebook -- which evolved as
          |a monolith.
          |
          |""".stripMargin)(style) enlarged 50)
  }

  Page("New Instance", "")(new PortmanteauInstantiation().GUI)

  Page("Transforms*", "")(new PortmanteauTransforms().Layout.leftButtons())

  Page("GlyphML", "") (new PortmanteauGlyphML().GUI)

  import utils.Output.withWriteBar

  lazy val asMenu      = withWriteBar()(Layout.menuBar)
  lazy val asRNotebook = withWriteBar()(Layout.rightButtons())
  lazy val asLNotebook = withWriteBar()(Layout.leftButtons())
  lazy val asVNotebook = withWriteBar()(Layout.rotatedButtons(3))
  lazy val asSNotebook = withWriteBar()(Layout.skewedButtons(0.2f, 0f, uniform = true))
  lazy val asTNotebook = withWriteBar()(Layout.topButtons())
}
