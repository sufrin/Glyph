package org.sufrin.glyph
package tests.demonstrationBook
import org.sufrin.glyph.styled.BookSheet

object Pages extends Application  {
  import styles._

  /**
   * Default sheet
   */
  val LocalSheet: StyleSheet = StyleSheet()

  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonFrame=styles.decoration.Blurred(fg=DefaultBrushes.blue, blur=5, spread=5, delta=5),
    buttonFontSize = 20,
    labelFontSize = 20,
    textFontSize = 20,
    backgroundBrush = DefaultBrushes.white
  )
  implicit val bookStyle: BookSheet =
    BookSheet(buttonSheet=interfaceStyle,
              pageSheet=interfaceStyle.copy(buttonFrame=decoration.Unframed, fontScale=0.9f))

  import glyphXML.Language._

  //val interface = new Interface
  //lazy val GUI: Glyph = interface.asRNotebook
  lazy val GUI = new SplashScreen().GUI.enlarged(20)

  def title = s"""Pages -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  //override def onClose(window: Window): Unit = interface.confirmCloseOn(GUI)(window)
}
