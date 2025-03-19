package org.sufrin.glyph
package tests
import unstyled.static.Rect
import NaturalSize.{Col, Row}
import DefaultBrushes._

/**
 * Interface using implicitly-styled glyphs
 */

trait Example3Interface {
  implicit val style: StyleSheet

  val spacer = Rect(0, 20, fg=nothing)

  import styled.TextButton
  import styled.Label


  def GUI: Glyph = Col(align=Center, bg=lightGrey) (
    Label("A simple label") enlarged(20),
    spacer,
    Row(skip=10)(
      TextButton("make it blue") { _ => style.labelBackgroundBrush.color(blue.color) },
      TextButton("make it red")  { _ => style.labelBackgroundBrush.color(red.color) }
    )
  ).enlarged(20)

}

object Example3 extends Application with Example3Interface  {

    override def title: String = "Example 3"

    implicit val style: StyleSheet = StyleSheet(
      labelBackgroundBrush  = green().rounded(18),
      labelForegroundBrush  = white,
      labelFontFamily       = FontFamily("Courier"),
      labelFontSize         = 32,
      buttonDecoration      = styles.decoration.Edged(blue(cap=ROUND, width=18))
    )
}
