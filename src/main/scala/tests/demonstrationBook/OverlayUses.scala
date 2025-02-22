package org.sufrin.glyph
package tests.demonstrationBook

import styled.{MenuCheckBox, TextButton}
import styled.Label
import styled.overlaydialogues.Dialogue
import styled.overlaydialogues.Dialogue.{CHOOSE, Menu, NestedMenu, OK, OKNO}
import NaturalSize.{Col, Row}
import styled.Book
import styled.BookSheet
import styled.{MenuButton, CheckBox, MenuGlyphButton}
import Glyphs._
import DefaultBrushes.{blue, red, redLine}

import org.sufrin.glyph.styles.decoration

class OverlayUses(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  private val noteBook = Book()
  private val Page = noteBook.DefinePage
  implicit val pageStyle: StyleSheet = style.buttonSheet
  import pageStyle.{ex, em}
  import translation._

  // Each subpage of this page declares its own style,
  // usually the standard application style, except for
  // the Menus subpage, which declares local styles
  // for some menus.

  Page("Dialogues", "") {

    val anchor = FilledRect(150, 70, blue)

    def showChoice(choice: String): Unit = {
      val response = if (choice ne null)  s"You chose $choice" else "You quit the dialogue"
      OK(Label(response)).South(anchor).start()
    }

    def diaGlyph(s: String): Glyph =
      Row.centered(PolygonLibrary.star7(fg=redLine, R=50f, C=50f), em, <p>This is a long pro-forma text for a dialogue that I expect to be justified properly in all the dialogues.</p>)

    Col.centered(
      <p width="50em" align="justify">
        On this page we are testing modal dialogues implemented on
        overlays within the current window. You can
        exit the dialogue without making a choice by clicking on
        the topmost grey bar or typing ESC. You can shift the
        location of the dialogue by using the direction keys.

        Each dialogue explains where it was intended to pop up
        (relative to the blue rectangle). The location of a dialogue
        that might be partly off screen are adjusted to make it (as
        far as possible) visible.
      </p>.enlarged(30),
      ex,
      anchor, ex,
      Row(
        TextButton("E/OKNO"){
          _ => Dialogue.OKNO(diaGlyph("East"))
            .East(anchor)
            .andThen{ result  => showChoice(s"$result") }
        }, em,
        TextButton("W/OKNO") {
          _ =>
            Dialogue.OKNO(diaGlyph("West"))
              .West(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
        TextButton("N/ABC") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("North"))("A", "B", "C")
              .North(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
      ), ex,
      Row(
        TextButton("W/ABC") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("West"))("A long button", "a Bigger button", "C")
              .West(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
        TextButton("InFront/ABC") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("InFront"))("A long button", "a Bigger button", "C")
              .InFront(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
        TextButton("S/ABC") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("South"))("A long button", "a Bigger button", "C")
              .South(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
      ), ex,
      Row(
        TextButton("W/ABC[long]") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("West"))("An extraordinarily long button", "a Bigger button that might cause jiggle", "C")
              .West(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
        TextButton("InFront/ABC[long]") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("InFront"))("An extraordinarily long button", "a Bigger button that might cause jiggle", "C")
              .InFront(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
        TextButton("S/ABC[long]") {
          _ =>
            Dialogue.CHOOSE(diaGlyph("East"))("An extraordinarily long button", "a Bigger button that might cause jiggle", "Really")
              .East(anchor)
              .andThen { result => showChoice(s"$result") }
        }, em,
      ), ex,
    )
  }


  Page("Menus", "") {

    lazy val menuA: Glyph = {
      Menu("A") (
        MenuButton("A1") { _ => println("A1")  },
        Label("This is not a button"),
        NestedMenu("innerCC >")(
          MenuButton("CCC1") { _ => println("CCC1") },
          MenuButton("CC2") { _ => println("CCC2") },
        ),
        MenuButton("A2") { _ => println("A2") },
        MenuGlyphButton(PolygonLibrary.hideButtonGlyph().scaled(2.5f), exact=false) {
          _ => println("GlyphButton")
        },
        MenuButton("A3") { _ => println("A3") },
        MenuButton("Disable C"){ _ =>
          menuC.enabled(false); ()
        },
        MenuButton("Enable C") { _ =>
          menuC.enabled(true); ()
        }
      )
    }

    lazy val menuC: Glyph = {
      Menu("C")(
        MenuButton("C1") { _ => println("C1") },
        MenuButton("C2") { _ => println("C2") },
        NestedMenu("innerC >")(
          MenuButton("CC1") { _ => println("CC1") },
          MenuButton("CC2") { _ => println("CC2") },
          NestedMenu("innerCC >")(
            MenuButton("CCC1") { _ => println("CCC1") },
            MenuButton("CC2") { _ => println("CCC2") },
          )
        )
      )
    }

    val smallStyle = style.pageSheet.copy(
        buttonFrame=decoration.Blurred(fg=DefaultBrushes.green, blur=5, spread=5),
        buttonForegroundBrush = DefaultBrushes.black,
        buttonHoverBrush = DefaultBrushes.darkGrey,
        labelForegroundBrush = DefaultBrushes.green,
        fontScale = 1.3f,
        backgroundBrush = DefaultBrushes.white
    )

    lazy val menuD =  {
      implicit val pageStyle: StyleSheet = smallStyle
      Menu("D")(
        MenuButton("A1") { _ => println("A1") },
        Row(bg=pageStyle.backgroundBrush).centered(Label("Checked when E is enabled: "), MenuCheckBox(true){ state=>menuE.enabled(state) }),
        Label("Nor is this a button"),
        NestedMenu("innerCC >")(
          MenuButton("CCC1") { _ => println("CCC1") },
          MenuButton("CC2") { _ => println("CCC2") },
        ),
        MenuButton("A2") { _ => println("A2") },
        //FilledRect(20f, 50f, fg = red),
        MenuGlyphButton(PolygonLibrary.hideButtonGlyph().scaled(2.5f), exact=false) {
          _ => println("GlyphButton")
        },
        MenuButton("A3") { _ => println("A3") },
        MenuButton("Disable C") { _ =>
          menuC.enabled(false); ()
        },
        MenuButton("Enable C") { _ =>
          menuC.enabled(true); ()
        }
      )(style.buttonSheet)
    }


    lazy val menuE: Glyph = {
      implicit val pageStyle: StyleSheet = smallStyle
      Menu("E")(
        MenuButton("C1") { _ => println("C1") },
        NestedMenu("innerCC >")(
          MenuButton("CCC1") { _ => println("CCC1") },
          MenuButton("CC2") { _ => println("CCC2") },
        ),
        MenuButton("C2") { _ => println("C2") },
        NestedMenu("innerC >")(
          MenuButton("CC1") { _ => println("CC1") },
          MenuButton("CC2") { _ => println("CC2") },
          NestedMenu("innerCC >")(
            MenuButton("CCC1") { _ => println("CCC1") },
            MenuButton("CC2") { _ => println("CCC2") },
          )
        )
      )(style.buttonSheet)
    }


      Col.centered(
        Label("Two menus"),
        Row(menuA, em, menuC), ex, ex,
        Label("Two menus with larger content"),
        Row(menuD, em, menuE), ex,
      )
    }


  Page("Locators", "") {

    import io.github.humbleui.types.IRect

    def rect = Rect(200, 250, red).framed(blue)

    lazy val theTarget = rect

    def content: IRect = theTarget.guiRoot.eventHandler.window.getContentRect

    Row(em,
      Col.centered(
        <div width="50em" align="justify">
          <p>This test places (OK) dialogues around the outside of the target red
             square using the intrinsic methods of <tt>overlaydialogues.Dialogue</tt>
          </p>
          <p>
            A dialogue menu disappears when the red button on it is clicked, or the ESC
            key is hit. As an exception to the <tt>overlaydialogues.Dialogue</tt> norm,
            for this page only, a mouseclick nowhere near any dialogue
            will make one of the dialogues disappear.
          </p>
        </div>,
        ex scaled 5,
        TextButton("Show Locators") {
          _ =>
            def dial(string: String): Dialogue[Unit] = {
              val d = Dialogue.OK(Label(string))
              d.isMenu=true
              d
            }
            dial("NorthWest").NorthWest(theTarget).start()
            dial("North").North(theTarget).start()
            dial("NorthEast").NorthEast(theTarget).start()
            dial("East").East(theTarget).start()
            dial("SouthEast").SouthEast(theTarget).start()
            dial("South").South(theTarget).start()
            dial("SouthWest").SouthWest(theTarget).start()
            dial("West").West(theTarget).start()
            dial("In front of").InFront(theTarget).start()
        },
        ex scaled 10,
        theTarget,
      ), em)
  }

  Page("Annotation", "") {

    import BooleanGlyphs.OnOffButton
    val anchor = INVISIBLE()

    var gridAnnotation: Option[RootLayer] = None

    /** Make a grid that covers `root` and has the grid-off button on it */
    def makeGridGlyph(root: Glyph): Glyph = {
      Concentric.atTop(PolygonLibrary.grid(root.diagonal), gridCheckboxForGrid)
    }

    def setGridState(state: Boolean): Unit = {
      gridCheckboxForPage.asInstanceOf[OnOffButton].set(state)
      gridCheckboxForGrid.asInstanceOf[OnOffButton].set(state)
      state match {
        case true =>
          gridAnnotation match {
            case Some(layer) =>
              layer.visible = true
            case None =>
              val root = gridCheckboxForPage.guiRoot
              gridAnnotation =
                Some(root.Overlay.newAnnotation("grid", makeGridGlyph(root), isModal = false, strictHiding = false, visible = true, active = true))
          }
        case false =>
          gridAnnotation match {
            case None =>
            case Some(layer) => layer.visible = false
          }
      }
    }

    lazy val gridCheckboxForGrid: OnOffButton with Glyph = CheckBox(false) {
      state => setGridState(state)
    }

    lazy val gridCheckboxForPage: OnOffButton with Glyph = CheckBox(false) {
      state => setGridState(state)
    }

    //////////////////////////////

    var localAnnotation: Option[RootLayer] = None

    def makeLocalGlyph(anchor: Glyph): Glyph = {
      val thisButton: Glyph = TextButton("<<< This is the North West corner, click to pop me down"){
        _ => setLocalState(false)
      }.turned(45.0f)
      thisButton@@(Vec.Origin)
    }

    def setLocalState(state: Boolean): Unit = {
      state match {
        case true =>
          localAnnotation match {
            case Some(layer) =>
              layer.visible = true
            case None =>
              val root = anchor.guiRoot
              localAnnotation = Some(root.Overlay.newAnnotation("local", makeLocalGlyph(anchor), isModal = false, strictHiding = false, visible = true, active = true))
          }
        case false =>
          localAnnotation match {
            case None =>
            case Some(layer) => layer.visible = false
          }
      }
    }

    //////////////////////////////

    Col.centered(
      anchor, // invisible anchor, to locate the GUI root
      <div width="45em" align="justify">
        <p>
          This page tests annotation-style overlays, using the low-level annotation API.
        </p>
        <p>
          The checkbox below enables/disables overlaying of a 10x10 grid on
          the whole of the current window, no matter what page/subpage is showing.
          The grid can be useful when exploring the dimensions of
          glyphs.
        </p>
        <p>
          A checkbox always appears on the grid: pressing this disables the grid, which can only be re-enabled by
          the checkbox below.
        </p>
      </div>,
      Row.centered(Label("Grid: "), gridCheckboxForPage), ex, ex,
      <div width="45em" align="justify">
        <p>
          The button below pops up an annotation overlay that points to
          the East wall of the window. The overlay stays up until its button is
          pressed, no matter what page/subpage of the app is showing.
        </p>
      </div>,ex,
      TextButton("Point to the North West corner of the window") {
        _ => setLocalState(true)
      }
    )
  }

  val GUI: Glyph = noteBook.Layout.leftCheckBoxes(pageAlign = Center).enlarged(40)

}
