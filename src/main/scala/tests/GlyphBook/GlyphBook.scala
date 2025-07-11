package org.sufrin.glyph
package tests.GlyphBook
import styled.BookSheet

object GlyphBook extends Application  {
  import styles._

  /**
   * Default sheet
   */
  val LocalSheet: StyleSheet = StyleSheet()

  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonDecoration=decoration.RoundFramed(fg=Brushes("blue.6.round"), bg=Brushes("cornflower").alpha(0.3), enlarge=0.25f, radius=20f),
    buttonFontSize = 20,
    labelFontSize = 20,
    textFontSize = 20,
    backgroundBrush = Brushes.white
  )
  implicit val bookSheet: BookSheet =
    BookSheet(buttonSheet=interfaceStyle,
              pageSheet=interfaceStyle.copy(buttonDecoration=decoration.unDecorated, fontScale=0.9f))

  import glyphXML.Language.translation

  lazy val GUI = new SplashScreen().GUI.enlarged(20)

  def title = s"""Glyph Book"""

  override val dock = new Dock() {
      setGlyph(styled.Label("Glyph\nBook")(bookSheet.pageSheet))
  }

}
