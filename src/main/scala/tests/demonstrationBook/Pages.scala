package org.sufrin.glyph
package tests.demonstrationBook
import styled.BookSheet

import org.sufrin.glyph.Brushes.{green, red}
import org.sufrin.glyph.styles.decoration.Decoration

object Pages extends Application  {
  import styles._

  /**
   * Default sheet
   */
  val LocalSheet: StyleSheet = StyleSheet()
  val decor: Decoration = {
    import styles.decoration._
    // Blurred(red, green, 10, 5)
    RoundFramed(fg=Brushes("blue.6.round"), bg=Brushes("cornflower").alpha(0.3), enlarge=0.25f, radius=20f)
  }

  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonDecoration=decor,
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
  val defaultIconPath: Option[String] = Some ("PNG/WorcesterCrest.png")

}
