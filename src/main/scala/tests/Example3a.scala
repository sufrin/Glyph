
package org.sufrin.glyph
package tests
import Brushes._

object Example3a extends Application  with Example3Interface {
  val style: StyleSheet = StyleSheet(
    labelBackgroundBrush  = green().rounded(18),
    labelForegroundBrush  = white,
    labelFontFamily       = FontFamily("Courier"),
    labelFontSize         = 32,
    buttonBackgroundBrush = grey2,
    buttonForegroundBrush = black,
    buttonDecoration      = styles.decoration.Framed(fg=darkGrey(cap=ROUND, width=6), bg=grey2, radius = 0.3f)
  )
  override def title: String = "Example 3a"
}



object Example3b extends Application  with Example3Interface {
  val style: StyleSheet = StyleSheet(
    labelBackgroundBrush  = green().rounded(18),
    labelForegroundBrush  = white,
    labelFontFamily       = FontFamily("Courier"),
    labelFontSize         = 32,
    buttonBackgroundBrush = transparent,
    buttonForegroundBrush = red,
    buttonDecoration      = styles.decoration.Blurred(fg=yellow, bg=transparent, 16, 5)
  )
  override def title: String = "Example 3b"
}

object Example3c extends Application  with Example3Interface {
  val style: StyleSheet = StyleSheet(
    labelBackgroundBrush  = green().rounded(18),
    labelForegroundBrush  = white,
    labelFontFamily       = FontFamily("Courier"),
    labelFontSize         = 32,
    buttonBackgroundBrush = transparent,
    buttonForegroundBrush = black,
    buttonDecoration      = styles.decoration.Shaded(fg=darkGrey, bg=transparent, 16, 5)
  )
  override def title: String = "Example 3c"
}

/** Extending an interface to provide a clone button  */

/** A parameterized class implementing `Example3Interface` by defining  (its) style. */
class Example3dInterface(val style: StyleSheet) extends Example3Interface { }

/** The application's main now makes an instance of `Example3dInterface` and puts it
 *  beside a button whose reaction is to clone a new Application with an identical
 *  GUI.
 */
object Example3d extends Application {
  def roundGreen: Brush = green().rounded(18)

  implicit val style: StyleSheet = StyleSheet(
    labelBackgroundBrush  = roundGreen,
    labelForegroundBrush  = white,
    labelFontFamily       = FontFamily("Menlo"),
    labelFontSize         = 32,
    buttonBackgroundBrush = transparent,
    buttonForegroundBrush = black,
    buttonDecoration      = styles.decoration.Shaded(fg=darkGrey, bg=transparent, 16, 5)
  )

  val cloneButton: Glyph = styled.TextButton("Clone") {
    _ =>
      val clone = new Application {
        override def title: String = "Example 3d clone"
        val GUI: Glyph = new Example3dInterface(style.copy(labelBackgroundBrush = roundGreen)).GUI beside (cloneButton rotated 1)
      }
      clone.main(Array())
  }

  val GUI: Glyph = new Example3dInterface(style).GUI beside (cloneButton rotated 1)

  override def title: String = "Example 3d"
}


/**
 * As Example3d, but with a dialogue confirming close-window requests on the ORIGINAL window
 */
object Example3e extends Application {
  def roundGreen: Brush = green().rounded(18)

  implicit val style: StyleSheet = StyleSheet(
    labelBackgroundBrush  = roundGreen,
    labelForegroundBrush  = white,
    labelFontFamily       = FontFamily("Courier"),
    labelFontSize         = 32,
    buttonBackgroundBrush = transparent,
    buttonForegroundBrush = black,
    buttonDecoration      = styles.decoration.Shaded(fg=darkGrey, bg=transparent, 16, 5)
  )

  val cloneButton: Glyph = styled.TextButton("Clone") {
    _ =>
      val clone = new Application {
        override def title: String = "Example 3d clone"
        val GUI: Glyph = new Example3dInterface(style.copy(labelBackgroundBrush = roundGreen)).GUI beside (cloneButton rotated 1)
      }
      clone.main(Array())
  }

  val GUI: Glyph = new Example3dInterface(style).GUI beside (cloneButton rotated 1)

  override def title: String = "Example 3d"

  override def whenStarted(): Unit = {
    Application.confirmCloseRequestsFor(GUI)
  }
}

/**
 * As example 3e, but all windows have close-window confirm dialogues.
 */
object Example3f extends Application {
  def roundGreen: Brush = green().rounded(18)

  implicit val style: StyleSheet = StyleSheet(
    labelBackgroundBrush  = roundGreen,
    labelForegroundBrush  = white,
    labelFontFamily       = FontFamily("Courier"),
    labelFontSize         = 32,
    buttonBackgroundBrush = transparent,
    buttonForegroundBrush = black,
    buttonDecoration      = styles.decoration.Shaded(fg=darkGrey, bg=transparent, 16, 5)
  )

  val cloneButton: Glyph = styled.TextButton("Clone") {
    _ =>
      val clone = new Application {
        override def title: String = "Example 3d clone"
        val GUI: Glyph = new Example3dInterface(style.copy(labelBackgroundBrush = roundGreen)).GUI beside (cloneButton rotated 1)
        override def whenStarted(): Unit = {
          Application.confirmCloseRequestsFor(GUI)
        }
      }
      clone.main(Array())
  }

  val GUI: Glyph = new Example3dInterface(style).GUI beside (cloneButton rotated 1)

  override def title: String = "Example 3d"

  override def whenStarted(): Unit = {
    Application.confirmCloseRequestsFor(GUI)
  }
}

