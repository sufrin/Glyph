package org.sufrin.glyph
package tests
import unstyled.static.{FilledRect, Rect}
import Brushes._
import NaturalSize.{Col, Row}

import org.sufrin.glyph.unstyled.Text
import sun.awt.resources.awt

/**
 * Interface using implicitly-styled glyphs. The interface is a "mixin" trait that expects to
 * be mixed with a class/trait that provides a definition for `style`.
 */

trait Example3Interface {
  implicit val style: StyleSheet

  val spacer = Rect(0, 20, fg=transparent)

  import styled.{Label, TextButton}

  lazy val GUI: Glyph = Col(align=Center, bg=lightGrey) (
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

    override val dock: Dock = new Dock() {
        val icon = trayIcon(Text("\u2462", fg=Brushes.red, bg=Brushes.yellow).framed(blue))
        icon.handleClick {
          location =>
            styled.windowdialogues.Dialogue.FLASH(styled.Label("You clicked\non the system\ntray icon")).OnRootOf(GUI, location).start()
        }
      setGlyph(Text("\u24e7\u2462", fg=Brushes.red, bg=Brushes.yellow).framed(red(width=2)))
    }

    implicit val style: StyleSheet = StyleSheet(
      labelBackgroundBrush  = green().rounded(18),
      labelForegroundBrush  = white,
      labelFontFamily       = FontFamily("Courier"),
      labelFontSize         = 32,
      buttonDecoration      = styles.decoration.RoundFramed(fg=blue(cap=ROUND, width=18), enlarge=0, radius=0.01f),
    )
}

