package org.sufrin.glyph
package tests

import styled.{Book, BookSheet, Paragraph, TextButton}
import DefaultBrushes.{black, darkGrey}
import GlyphTypes.Scalar
import NaturalSize.Col

/**
 * Demonstrates that a SplitScreen can function properly.
 *
 * [Investigating the Etcetera SplitScreen page]
 */
object SplitScreenTest extends Application {
  implicit val sheet: StyleSheet = StyleSheet()
  import sheet.ex
  import glyphXML.Language._
  val title: String = "SplitScreenTest"

  def SplitTest(enlargement: Scalar = 30f): Glyph = {
    import DynamicGlyphs.SplitScreen
    import ReactiveGlyphs.Slider

    lazy val enableDynamic: Glyph = styled.CheckBox(initially=false) {
      state => enableDynamic.guiRoot.autoScale = state
    }

    val left = <div width="30em" align="justify">
      <p>This is a justified piece of text that may be quite small.
      You'll see it on a split screen. When the text on the other
      screen is not the same width we'll see what happens.
      </p>
      <p>
        The main point to note is that the button loses focus when
        the pointer moves into a region where it is occluded.
      </p>
    </div> above
      styled.TextButton("The Left Button") { _ => println("LEFT") } above
      enableDynamic
    val right = <div width="40em" align="left">
      <p>This is a left-justified piece of text that may be quite small.
        You'll see it on a split screen. It'll be a bit wider
        than the other thing on the screen.
      </p>
      <p>
        The main point to note is that the button loses focus when
        the pointer moves into a region where it is occluded.
      </p>
    </div> above
      styled.TextButton("The Right Button") { _ =>  println("RIGHT") }

    val dynamic = SplitScreen(left enlarged 30, right enlarged 30, dynamic=true, fg=darkGrey.strokeWidth(6f))
    def blob    = Glyphs.FilledRect(28f, 14f, fg=black.blurred(6f))
    val slider  = Slider.Horizontal(Glyphs.Rect(dynamic.w, 2f), blob, dynamic.proportion){
      case proportion: Scalar => dynamic.setBoundary(proportion)
    }

    val static = SplitScreen(left() enlarged 30, right() enlarged 30, dynamic=false, fg=darkGrey.strokeWidth(6f))

    Col(align=Center)(
      dynamic,
      slider,
      TextButton("<"){
        _ => dynamic.setBoundary(0.0f); slider.dragTo(0f)
      } beside
        TextButton("<>") {
          _ => dynamic.exchange()
        } beside
        TextButton(">"){
          _ => dynamic.setBoundary(1.0f); slider.dragTo(0.999f)
        },
      ex, ex, ex,
      static,
      TextButton("L<->R") {
        _ => static.exchange()
      }
    ) enlarged enlargement framed black(width=4)
  }

  val GUI: Glyph = {
    implicit val bookSheet: BookSheet = BookSheet(sheet, sheet)
    val book = Book()
    val Page = book.Page
    Page("30")  { SplitTest(30) }
    Page("50")  { SplitTest(50) }
    Page("150") { SplitTest(150) }
    Page("650") { SplitTest(650) }
    Page("750") { SplitTest(750) }
    book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign = Center).enlarged(30)
  }
}
