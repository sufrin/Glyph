package org.sufrin.glyph
package tests

object ResizeableTest extends Application {
  import glyphXML.Language._

  implicit val style: StyleSheet = StyleSheet(windowDiagonal = Vec(400,150))

  def guiSpec(implicit style: StyleSheet): Glyph =
    <div width="0.8*windowwidth" align="justify" frame="red/4/ROUND">
      <p>This is a very long piece of text that may change shape as the window changes shape. Or it may not.</p>
      <p>This is a very long piece of text that may change shape as the window changes shape. Or it may not.</p>
      <p>This is a very long piece of text that may change shape as the window changes shape. Or it may not.</p>
    </div>

  val GUI: Glyph = styled.Resizeable(style => guiSpec(style))

  def title: String = "Resizeable Test"

  override def whenStarted(): Unit = {
    println(s"STARTED $GUI")
    Application.confirmCloseRequestsFor(GUI)
    styled.Resizeable.loggingLevel(org.sufrin.logging.ALL)
  }
}