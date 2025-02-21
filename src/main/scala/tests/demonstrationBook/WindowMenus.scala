package org.sufrin.glyph
package tests.demonstrationBook

import sheeted.TextButton
import sheeted.Label
import sheeted.windowdialogues.Dialogue
import Dialogue.{CHOOSE,OK}
import NaturalSize.{Col, Row}
import sheeted.Book
import sheeted.BookSheet
import Glyphs.{INVISIBLE, Rect}


class WindowMenus(implicit val style: BookSheet, implicit val translation: glyphXML.Translation) {
  implicit val pageSheet: Sheet = style.buttonSheet
  import translation._
  import pageSheet.{ex, em}
  import DefaultBrushes.{ red,blue}


  val book = Book()
  val Page = book.Page

  Page("Dialogues", "") {
    val briefing: Glyph = <p>Choose one of the buttons, or press the close button</p>
    val anchor = INVISIBLE()
    val c1 = CHOOSE(briefing(), Location.South(anchor), "Choice")("One")
    val c2 = CHOOSE(briefing(), Location.South(anchor), "Choice")("One", "Two")
    val c3 = CHOOSE(briefing(), Location.South(anchor), "Choice")("One", "Two", "Three")

    def showChoice(s: String): Unit =
      sheeted.overlaydialogues.Dialogue.OK(Label(if (s eq null) "You made no choice" else s"You chose $s")).South(anchor).start()

    Col.centered(
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

  Page("Menus",    "Locating Popups and Menus") {
    import Location._

    val flag = sheeted.Label("×")
    val X = windowdialogues.Menu.topBar(List(flag))

    def startLocatorAt(position: Location) = {
      sheeted.windowdialogues.Menu.at(position)(flag).start()
    }


    /** Construct a menu with buttons that (may) bind to nested menus */
    def Menu(name: String, nested: Boolean = true)(glyphs: Glyph*): ReactiveGlyph = {
      lazy val but: ReactiveGlyph = TextButton(name) {
        _ => windowdialogues.Menu(glyphs map (_.asMenuButton)).InFront(but).start()
      }.asInstanceOf[ReactiveGlyph] // TODO: CODE SMELL
      if (nested) but.asMenuButton else but
    }

    lazy val theTarget = Label("This is the target").scaled(2).framed()

    Col.centered(
        <div width="60em" align="justify">
          <p>
            This page tests various Dialogue.Location locators for window dialogues and menus.
            Clicking a button or menu button puts a little popup at the indicated
            location relative to the target.  An × on the button denotes that
            glyph placed in that popup. Of particular interest is:
          </p>
          <p> (1) whether the popup appears in the named location relative to the target glyph. </p>
          <p> (2) whether menus behave properly; including whether closing them causes their 'host' glyph to become responsive. </p>
          <p>"Self-referential" buttons that make themselves the target  glyph are straightforward to implement.</p>
        </div>,
      Col.atRight(
        TextButton("A real button!")          { _ => OK(Label("Congratulations!\nYou found\na real button."), South(theTarget)).start() },
        TextButton("South(the target)")       { _ => startLocatorAt(South(theTarget)) },
        TextButton("NorthFor(×)(the target)") { _ => startLocatorAt(NorthFor(X)(theTarget)) },
        TextButton("SouthEast(the target)")   { _ => startLocatorAt(SouthEast(theTarget)) },
        Menu("SouthWestFor / South / East / SouthEast Placements >")(
          TextButton("SouthWestFor(×)(the target")
                                              { _ => startLocatorAt(SouthWestFor(X)(theTarget)) },
          TextButton("South(the target")      { _ => startLocatorAt(South(theTarget)) },
          TextButton("East(the target")       { _ => startLocatorAt(East(theTarget)) },
        ),
        {
          lazy val here: Glyph = TextButton("NorthFor(×)(THIS BUTTON ITSELF)")
                                              { _ => startLocatorAt(NorthFor(X)(here)) }
          here
        },
        ex scaled 2,
        theTarget,
        ex scaled 2,
        Menu("RelativeTo Placements >")(
          Menu("RelativeTo/By Placements >")(
            TextButton("RelativeTo(the target)") { _ => startLocatorAt(RelativeTo(theTarget)) },
            TextButton("RelativeTo(the target, Vec(the target.w,0))") { _ => startLocatorAt(RelativeTo(theTarget, Vec(theTarget.w, 0f))) },
            //TextButton("RelativeTo(the target)(theTarget.w,0))") { _ => OK(RelativeTo(theTarget)(theTarget.w, 0f)) },
            TextButton("Another real button!") { _ => OK(Label("Congratulations!\nYou found another\nreal button."), South(theTarget)).start() },
          ),
          Menu("SouthWestFor / South / East / SouthEast Placements >")(
            TextButton("SouthWestFor(×)(the target") { _ => startLocatorAt(SouthWestFor(X)(theTarget)) },
            TextButton("South(the target") { _ => startLocatorAt(South(theTarget)) },
            TextButton("East(the target") { _ => startLocatorAt(East(theTarget)) },
            TextButton("SouthEast(the target)") { _ => startLocatorAt(SouthEast(theTarget)) },
            TextButton("Another real button!") { _ => OK(Label("Congratulations!\nYou found another\nreal button."), South(theTarget)).start() },
          ))),
      ex scaled 5)
  }

  Page("Locators", "Locators for Popups and Menus") {

    import io.github.humbleui.types.IRect

    def rect = Rect(200, 250, red).framed(blue)

    lazy val theTarget = rect

    def content: IRect = theTarget.guiRoot.eventHandler.window.getContentRect

    def loc(text: String) = windowdialogues.Menu.topBar(List(Label(text)))

    import Location._

    Row(em,
      Col.centered(
        <div width="60em" align="justify">
          <p>This test places windowdialogues.Menu instances around the outside of the target (red)
          square, and in a couple of other places, using the
          locators defined by `Location`.
          Locators along the
          north and west edges use an additional argument to
          specify the glyph that will be located "there".</p>

          <p>When the red button on it is clicked or
          ESC is typed, a windowdialogues menu pops down.</p>

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
                , //("RelativeTo(root)(100,100)", RelativeTo(theTarget.guiRoot)(100f, 100f))
              )
            }
            for {(label, locator) <- cardinals} {
              windowdialogues.Menu.at(locator)(Label(label)).start()
            }
        },
        ex scaled 10,
        theTarget,
        ex scaled 10,
        TextButton("Dialogue a window at the south") {
          _ =>
            sheeted.windowdialogues.Dialogue.OK(<p width="25em">This window should be  at the South edge of the target.</p>, South(theTarget)).start()
        },
        TextButton("Dialogue a window at the north west") {
          _ =>
            val body = <p width="25em">This window should be  at the north-west edge of the target.</p>
            // Construct a dummy from the APPEARANCE of the intended Dialogue
            val dummy: Glyph = sheeted.windowdialogues.Dialogue.OK(body, East(theTarget)).theRoot
            sheeted.windowdialogues.Dialogue.OK(body, NorthWestFor(dummy)(theTarget)).start()
        }
      ), em)
  }

  val GUI: Glyph = book.Layout.rightButtons().enlarged(20)
}
