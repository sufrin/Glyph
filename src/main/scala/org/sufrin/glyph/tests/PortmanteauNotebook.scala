package org.sufrin.glyph.tests
import org.sufrin.glyph.GlyphTypes._
import org.sufrin.glyph.NaturalSize.{Col, Row}
import org.sufrin.glyph._
import org.sufrin.glyph.styled.TextLayout.{TextLabel, TextParagraphs}



trait PortmanteauInterface extends Notebook {
  import PortmanteauStyle.Spaces._
  implicit val buttonStyle: Styles.ButtonStyle = PortmanteauStyle.ButtonStyle
  implicit val labelStyle:  Styles.GlyphStyle  = PortmanteauStyle.labelStyle

  def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
    import windowdialogues.Dialogue.OKNO
    val prompt = Row.centered(PolygonLibrary.closeButtonGlyph scaled 5 enlarged(50),
      TextLabel("Do you want to Exit?") scaled 0.7f
    ).enlarged(50)
    OKNO(prompt,
      title="Exit Dialogue", ok=" Exit now ", no=" Continue ").InFront(glyph).andThen{
      case close => if (close) window.close()
    }
  }

  Page("Welcome", "") {
    Col.centered(TextParagraphs(30, Justify)(
      """
        | Welcome to the Portmanteau Notebook: it has some pages the same
        |as those of the Demonstration Notebook, but its source code is structured
        |differently -- with each page defined in its own source file.
        |
        |""".stripMargin))
  }

  Page("Transforms*", "")(TransformsPage.Layout.topButtons())
}

object PortmanteauNotebook extends Application with PortmanteauInterface {

  lazy val GUI = Layout.rightButtons()

  def title = s"""PortmanteauNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")



  override
  def onClose(window: Window): Unit = confirmCloseOn(GUI)(window)
}
