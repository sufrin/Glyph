package org.sufrin.glyph
package tests.GlyphBook

import styled.windowdialogues.Dialogue.POPUP
import NaturalSize.{Col, Row}
import styled.{Book, BookSheet, Label, TextButton}
import unstyled.static.{INVISIBLE, Rect}


class WindowMenus(implicit val style: BookSheet, implicit val translator: glyphML.Translator) {
  implicit val pageSheet: StyleSheet = style.buttonSheet
  import Brushes.{blue, red}

  import pageSheet.{em, ex}
  val language = translator(pageSheet)
  import language._


  val book = Book()
  val Page = book.Page


  Page("Dialogues", "") {
    val briefing: Glyph = <p>Choose one of the buttons, or press the close button</p>
    val anchor = INVISIBLE()
    val c1 = POPUP(briefing(), Location.South(anchor), "Choice")("One")
    val c2 = POPUP(briefing(), Location.South(anchor), "Choice")("One", "Two")
    val c3 = POPUP(briefing(), Location.South(anchor), "Choice")("One", "Two", "Three")

    def showChoice(s: String): Unit =
      styled.overlaydialogues.Dialogue.OK(Label(if (s eq null) "You made no choice" else s"You chose $s")).South(anchor).start()

    Col(align=Center)(
      <div width="40em" align="justify">
      <p>
        This page tests popup dialogues from which multiple choices can be made.
        </p>
        <p>
          Each of the buttons below pops up a CHOOSE popup with the specified number of choices on it.
          Pressing its close button pops the dialogue down without making a choice (and
          yields null to the popup's continuation).
      </p>
      </div>,
      ex,

      Row(
        TextButton("One")   { _ => c1.andThen(showChoice) },
        TextButton("Two")   { _ => c2.andThen(showChoice) },
        TextButton("Three") { _ => c3.andThen(showChoice) },
        anchor
      )

    ).enlarged(40)
  }

  Page("Locators", "Locators for Popups and Menus") {

    import io.github.humbleui.types.IRect

    def rect = Rect(250, 350, red).framed(blue)

    lazy val theTarget = rect

    def content: IRect = theTarget.guiRoot.eventHandler.window.getContentRect

    def loc(text: String) = styled.windowdialogues.Menu.topBar(List(Label(text)))

    import Location._

    Row(em,
      Col(align=Center)(
        <div width="60em" align="justify">
          <p>This test places <tt>windowdialogues.Menu</tt> instances around the outside of the target (red)
          square, and in a couple of other places, using
          locators defined by <tt>Location.Locators</tt>. Along the
          north and west edges these use an additional argument to
          specify the glyph that will be located "there".</p>

          <p>When the red button on it is clicked or the
          <b>esc</b> key is typed, a <tt>windowdialogues</tt> menu pops down.</p>

        </div>,
        ex scaled 5,
        TextButton("Show Locators") {
          _ =>
            val cardinals: Seq[(String, Location)] = {
              List(
                  ("NorthWest", NorthWestFor(loc("NorthWest"))(theTarget))
                , ("North ", NorthFor(loc("North"))(theTarget))
                , ("NorthEast", NorthEastFor(loc("NorthEast"))(theTarget))
                , ("East", EastFor(loc("East"))(theTarget))
                , ("SouthEast", SouthEast(theTarget))
                , ("South", SouthFor(loc("South"))(theTarget))
                , ("SouthWest", SouthWestFor(loc("SouthWest"))(theTarget))
                , ("West", WestFor(loc("West"))(theTarget))
                , ("OnScreen(target)(50, 50)", OnScreen(theTarget)(50, 50))
                , ("AtTop", AtTopFor(loc("AtTop"))(theTarget))
                , ("AtBottom", AtBottomFor(loc("AtBottom"))(theTarget))
                , ("InFront", InFrontFor(loc("InFront"))(theTarget))
                , //("RelativeTo(root)(100,100)", RelativeTo(theTarget.guiRoot)(100f, 100f))
              )
            }
            for {(label, locator) <- cardinals} {
              styled.windowdialogues.Menu.at(locator)(Label(label)).start()
            }
        },
        ex scaled 10,
        theTarget,
        ex scaled 10,
        TextButton("Dialogue at the south") {
          _ =>
            styled.windowdialogues.Dialogue.OK(<p width="25em">This window should be  at the South edge of the target.</p>, South(theTarget)).start()
        },
        TextButton("Dialogue at the north west") {
          _ =>
            val body = <p width="25em">This window should be  at the north-west edge of the target.</p>
            // Construct a dummy from the APPEARANCE of the intended Dialogue
            val dummy: Glyph = styled.windowdialogues.Dialogue.OK(body, East(theTarget)).GUI
            styled.windowdialogues.Dialogue.OK(body, NorthWestFor(dummy)(theTarget)).start()
        },
        TextButton("Dialogue at the east") {
          _ =>
            val body = <p width="25em">This window should be  at the eastern edge of the target.</p>
            styled.windowdialogues.Dialogue.OK(body, East(theTarget)).start()
        }
      ), em)
  }

  val GUI: Glyph = book.Layout.leftCheckBoxes().enlarged(40)
}
