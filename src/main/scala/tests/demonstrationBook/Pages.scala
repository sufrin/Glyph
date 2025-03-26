package org.sufrin.glyph
package tests.demonstrationBook
import org.sufrin.glyph.styled.{BookSheet, Label}
import org.sufrin.glyph.GlyphTypes.Window
import org.sufrin.glyph.NaturalSize.Row

object Pages extends Application  {
  import styles._

  /**
   * Default sheet
   */
  val LocalSheet: StyleSheet = StyleSheet()

  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonDecoration=styles.decoration.Blurred(fg=Brushes.blue, blur=5, spread=5, delta=5),
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

  def title = s"""Demonstration Book"""

  override
  val defaultIconPath: Option[String] = Some ("./cherub.png")

}
