package org.sufrin.glyph
package tests

import CellFit.ShiftEast
import FixedSize.Space.tab
import PolygonLibrary.brown
import ReactiveGlyphs.RawButton
import Styles.NotebookStyle

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.unused

/**
 * An expansive test comprising examples of UI components
 */
trait DemonstrationPages extends Brushes {

  import Glyphs._
  import GlyphTypes._
  import NaturalSize.{Col, Row}
  import Styles.{ButtonStyle, Decoration, GlyphStyle}
  import styled._
  import styled.text.{ActiveString, Label, Paragraphs}

  /**
   * The style used throughout this application, save in a few
   * places where local derived styles are used.
   */
  val ApplicationStyle: StyleSheet =
  if (true) Styles.Default else
    new Styles.Default.Derived {
    import Styles._
    override def buttonStyle: ButtonStyle  = {
      val light: Brush = lightGrey
      delegate.buttonStyle.copy(frame = Decoration.Blurred(light, nothing, 15f, 5f),
        up = GlyphStyle(font = buttonFont, fg = darkGrey, bg = nothing))
    }
  }

  import ApplicationStyle.Spaces.{em, ex}

  object HelpStyle extends Styles.DefaultSheet {
    override def face: Typeface = FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL)
    override def labelFontSize: Scalar = 12

    override lazy val labelStyle: GlyphStyle = ApplicationStyle.labelStyle.copy(fg = black, font = labelFont)
    override lazy val buttonStyle: ButtonStyle =
      ApplicationStyle.buttonStyle.copy(
        up    = labelStyle.copy(fg = blue),
        down  = labelStyle.copy(fg = red),
        hover = labelStyle.copy(fg = green))
  }

  val HugeLabelStyle = new Styles.DefaultSheet {
    override def face: Typeface = FontManager.default.matchFamilyStyle("Courier", FontStyle.BOLD)
    override def buttonFontSize: Scalar = 36
    override def labelFontSize: Scalar = 36
    override lazy val labelStyle: GlyphStyle = ApplicationStyle.labelStyle.copy(fg = red, font = labelFont)
  }

  implicit val pageStyle: NotebookStyle = ApplicationStyle.notebookStyle

  /**
   *  The GUI manifests as a `Notebook`.
   *  Although it is  not necessary to do so, here we
   *  bind each top-level `Page`  of the notebook to a
   *  constant with an analogous name. This is just done to help with IDE
   *  navigation.
   */
  val noteBook: Notebook = Notebook()
  val Page: noteBook.DefinePage.type = noteBook.DefinePage

  val helpText: String =
    """This application demonstrates aspects of the Glyphs library
      |by offering the choice of several demonstration GUIs. These are usually shown on
      |the pages of a tabbed notebook, with tabs placed
      |to the right. Several of the pages have pages nested  within them:
      |their names have * by them.
      |
      | Command-line arguments affect the notebook style (normally -notebook)
      |and scale (normally 1.00).
      |
      |<<<<<
      |<<<<< -notebook => tabs to the right (the default)
      |<<<<< -rnotebook => tabs to the right
      |<<<<< -lnotebook => tabs to the left
      |<<<<< -vnotebook => rotated tabs along the top
      |<<<<< -tnotebook => tabs along the top
      |<<<<< -snotebook => rotated and skewed tabs along the top
      |<<<<<
      |<<<<< -menu => individual popup windows selected from a menu bar
      |<<<<<
      |<<<<< -scale=d.dd the viewing scale is d.dd (1.00 by default)
      |<<<<<
      | The "New" page enables instantiation of a new GUI with
      |a choice of tab style, viewing scale and starting screen.
      |
      | The "Rescaling enabled" button enables rescaling of the GUI in response to the
      |user changing the width of the application window interactively.
      |
      | The application evolved naturally during development because we saw that unit-testing was
      |not going to be effective. It is not, and not intended to be, a comprehensive test; but if it
      |works at all then a very substantial proportion of the toolkit must be functioning adequately.
      |""".stripMargin

  // -------------------------------------------------------------------------------------------------------

  def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
    import overlaydialogues.Dialogue.OKNO
    import styled.text._
    val prompt = Row.centered(PolygonLibrary.closeButtonGlyph scaled 5 enlarged(50),
                             Label("Do you want to Exit?")(HugeLabelStyle)
                ).enlarged(50)
    OKNO(prompt,
         title="Exit Dialogue", ok=" Exit now ", no=" Continue ")(ApplicationStyle).OnRootOf(glyph).andThen{
            case close => if (close) window.close()
         }
  }

  /**
   * Is the save-bar (at the top of the GUI) enabled?
   * This part of the state is managed by the `saveEnable` variable that itself
   * is controlled by a couple of toggles with which it is kept synchronised.
   */
  var enableSave: Boolean = false
  import BooleanGlyphs._

  /**
   *
   * This `ToggleVariable` keeps the state(s) of (and appearances) of all the buttons
   * with which it is associated  in  sync.
   *
   */
  val saveEnable = BooleanVariable[OnOffButton](initially=enableSave){
    state => enableSave = state
  }

  /** Checkbox indicating whether the save-bar is enabled. */
  val enableSaveCheckBox: OnOffButton =
    CheckBox(initially=saveEnable.value) (saveEnable) (ApplicationStyle)

  /** A text toggle indicating in its text whether the save-bar is enabled. */
  val enableSaveTextToggle: OnOffButton =
    TextToggle(
       whenTrue = "Image Saving Enabled\n(click top bar to save)",
       whenFalse = "Image Saving Disabled\n(click to enable)",
       initially = saveEnable.value) (saveEnable) (ApplicationStyle)


  val HelpPage = Page("Help", "Help for the Demonstration Notebook"){
    implicit val Style: StyleSheet = ApplicationStyle
    val anchor = INVISIBLE()
    locally { HintManager(enableSaveCheckBox, 5, "Click this to enable the image-save bar")(HelpStyle) }

    Col.centered(
      anchor,
      Paragraphs(60, Justify)(helpText), ex, ex,
      NaturalSize.Grid.Table(width=2)(
      Label("Event logging: ").cellFit(ShiftEast), CheckBox(initially=false) {
        state => anchor.guiRoot.eventHandler.logEvents = state
      },
      Label("Confirm exit: ").cellFit(ShiftEast),  CheckBox(initially=true) {
        case false  =>
          anchor.guiRoot.onCloseRequest( _.close() )
        case true =>
          anchor.guiRoot.onCloseRequest ( confirmCloseOn(anchor)(_) )
      }) enlargedBy(50, 0) beside
      NaturalSize.Grid.Table(width=2)(
      Label("Rescaling: ").cellFit(ShiftEast), CheckBox(initially=false) {
        state =>
          if (anchor.guiRoot.platform=="X11")
            overlaydialogues.Dialogue.OK(Label("Warning: X11 doesn't always implement automatic rescaling properly")).OnRootOf(anchor).start()
          anchor.guiRoot.autoScale = state

      },
      Label("Image saving: ").cellFit(ShiftEast), enableSaveCheckBox
      )
    )
  }

  val NewPage = Page("New", "Make a new or cloned GUI ") {
    implicit val Style: StyleSheet = ApplicationStyle

    lazy val Duplicated = new DemonstrationPages with Application {
      enableSave = extraArgs contains "-enablesave"
      def GUI: Glyph =
        if (extraArgs contains "-notebook") asRNotebook else
        if (extraArgs contains "-rnotebook") asRNotebook else
        if (extraArgs contains "-lnotebook") asLNotebook else
        if (extraArgs contains "-snotebook") asSNotebook else
        if (extraArgs contains "-vnotebook") asVNotebook else
        if (extraArgs contains "-tnotebook") asTNotebook else
        if (extraArgs contains "-menu") asMenu else asRNotebook

      def title = s"""DemonstrationNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

      override
      val defaultIconPath: Option[String] = Some("./flag.png")

      override
      def onClose(window: Window): Unit = confirmCloseOn(GUI)(window)

    }


    var style: String = "-notebook"
    var scale: String = "-scale=0.7"
    var screen: String = "-screen=p"
    var saveable: String = ""
    val styles = "-notebook/-lnotebook/-snotebook/-vnotebook/-tnotebook/-menu".split("/").toList
    val scales = "-scale=1.2/-scale=1.0/-scale=0.9/-scale=0.8/-scale=0.75/-scale=0.7/-scale=0.6".split("/").toList.reverse
    val screens = "-screen=p/-screen=0/-screen=1/-screen=2".split("/").toList

    lazy val styleSelect: RadioCheckBoxes = RadioCheckBoxes(styles, "-notebook") {
      case None => styleSelect.select(0); style = styles.head
      case Some(i) => style = styles(i)
    }

    lazy val scaleSelect: RadioCheckBoxes = RadioCheckBoxes(scales, "-scale=0.7") {
      case None => scaleSelect.select(0); scale = scales.head
      case Some(i) => scale = scales(i)
    }

    lazy val screenSelect: RadioCheckBoxes = RadioCheckBoxes(screens) {
      case None => screenSelect.select(0); screen = screens.head
      case Some(i) => screen = screens(i)
    }

    val cloneButton = TextButton("Clone this instance") {
      _ =>
        DemonstrationNotebook.main(Array(scale, style, screen))
    }
    locally {
      cloneButton.enabled(false)
    }

    Col.centered(
      Paragraphs(ems = 60, Justify)(
        """The button below starts a completely new instance of the GUI.
          |The checkboxes determine what tab layout and scale the new instance will have; as well
          |as what screen (if there are many) it will be shown on at first.
          | There is no artificial limit to the number of instances that can be running at once within a single JVM,
          |(though space constraints within the JVM will impose a natural limit).
          |""".stripMargin), ex,
      Row(
        TextButton("New instance")   {
          _ => Duplicated.main(Array(scale, style, screen, saveable)) }, em,
      ), ex,
      Row.atTop(
        styleSelect.arrangedVertically(), em scaled 6,
        scaleSelect.arrangedVertically(), em scaled 6,
        screenSelect.arrangedVertically(), em scaled 6,
      ), ex,
      Paragraphs(ems = 60, Justify)("""Unlike a new instance, a cloned instance always shares some state with the
                                       |current GUI -- for example the notebook page currently being viewed, and the state
                                       |of checkboxes on a page. Changes made
                                       |in a GUI, are reflected in its clones only when
                                       |they next receive an event (such as entry of the mouse).
                                       |
                                       | Because this is intended to be used only during development, the clone button needs to be enabled
                                       |(using its adjacent checkbox) before it responds to presses. Tab layout, scale, and screen are specified as
                                       |for new instances.
                                       |
                                       |""".stripMargin), ex,
      Row.centered(cloneButton, CheckBox(false) { state => cloneButton.enabled(state) })
    )
  }

  val MenusPage = Page("Menus*", "Window menus and dialogues") {
    implicit val Style: StyleSheet = ApplicationStyle
    val nested = Notebook()
      val Page = nested.DefinePage
      import windowdialogues.Dialogue

      Page("Dialogues", "") {
        val briefing = Paragraphs(25, Left)("Choose one of the buttons, or press the close button")
        val anchor = INVISIBLE()
        val c1 = Dialogue.CHOOSE(briefing(), Location.South(anchor), "Choice")("One")
        val c2 = Dialogue.CHOOSE(briefing(), Location.South(anchor), "Choice")("One", "Two")
        val c3 = Dialogue.CHOOSE(briefing(), Location.South(anchor), "Choice")("One", "Two", "Three")

        def showChoice(s: String): Unit =
          overlaydialogues.Dialogue.OK(Label(if (s eq null) "You made no choice" else s"You chose $s")).South(anchor).start()

        Col.centered(
          Paragraphs(40, Left)(
            """
              |This page tests popup dialogues from which multiple choices can be made.
              |Each of the buttons below pops up
              |a CHOOSE popup with the specified number of choices
              |on it.
              |
              | Pressing its close button pops the dialogue down without making a choice (and
              |yields null to the popup's continuation).
              |
              |""".stripMargin), ex,
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


          val flag = styled.text.Label("×")
          val X = windowdialogues.Menu.topBar(List(flag))

          def OK(position: Location) = {
            windowdialogues.Menu.at(position)(flag).start()
          }


          /** Construct a menu with buttons that (may) bind to nested menus */
          def Menu(name: String, nested: Boolean = true)(glyphs: Glyph*): ReactiveGlyph = {
            lazy val but: ReactiveGlyph = TextButton(name) {
              _ => windowdialogues.Menu(glyphs map (_.asMenuButton)).InFront(but).start()
            }.asInstanceOf[ReactiveGlyph] // TODO: CODE SMELL
            if (nested) but.asMenuButton else but
          }

          lazy val theTarget = Label("This is the target")(HugeLabelStyle).framed()

          Col.centered(
            Paragraphs(60 * em.w, Left)(
              """This page tests various Dialogue.Location locators for window dialogues and menus.
                |Clicking a button or menu button puts a little popup at the indicated
                |location relative to the target.  An × on the button denotes that
                |glyph placed in that popup. Of particular interest is:
                |
                |<<<<<< (1) whether the popup appears in the named location relative to the target glyph. >>>>>>
                |
                |<<<<<< (2) whether menus behave properly; including whether closing them causes their
                |'host' glyph to become responsive. >>>>>>
                |
                |< "Self-referential" buttons that make themselves the target  glyph are
                |straightforward to implement.
                |
                |
                |""".stripMargin),
            Col.atRight(
              TextButton("A real button!") { _ => Dialogue.OK(Label("Congratulations!\nYou found\na real button."), South(theTarget)).start() },
              TextButton("South(the target)") { _ => OK(South(theTarget)) },
              TextButton("NorthFor(×)(the target)") { _ => OK(NorthFor(X)(theTarget)) },
              TextButton("SouthEast(the target)") { _ => OK(SouthEast(theTarget)) },
              Menu("SouthWestFor / South / East / SouthEast Placements >")(
                TextButton("SouthWestFor(×)(the target") { _ => OK(SouthWestFor(X)(theTarget)) },
                TextButton("South(the target") { _ => OK(South(theTarget)) },
                TextButton("East(the target") { _ => OK(East(theTarget)) },
              ),
              {
                lazy val here: Glyph = TextButton("NorthFor(×)(THIS BUTTON ITSELF)") { _ => OK(NorthFor(X)(here)) }
                here
              },
              ex scaled 2,
              theTarget,
              ex scaled 2,
              Menu("RelativeTo Placements >")(
                Menu("RelativeTo/By Placements >")(
                  TextButton("RelativeTo(the target)") { _ => OK(RelativeTo(theTarget)) },
                  TextButton("RelativeTo(the target, Vec(the target.w,0))") { _ => OK(RelativeTo(theTarget, Vec(theTarget.w, 0f))) },
                  //TextButton("RelativeTo(the target)(theTarget.w,0))") { _ => OK(RelativeTo(theTarget)(theTarget.w, 0f)) },
                  TextButton("Another real button!") { _ => Dialogue.OK(Label("Congratulations!\nYou found another\nreal button."), South(theTarget)).start() },
                ),
                Menu("SouthWestFor / South / East / SouthEast Placements >")(
                  TextButton("SouthWestFor(×)(the target") { _ => OK(SouthWestFor(X)(theTarget)) },
                  TextButton("South(the target") { _ => OK(South(theTarget)) },
                  TextButton("East(the target") { _ => OK(East(theTarget)) },
                  TextButton("SouthEast(the target)") { _ => OK(SouthEast(theTarget)) },
                  TextButton("Another real button!") { _ => Dialogue.OK(Label("Congratulations!\nYou found another\nreal button."), South(theTarget)).start() },
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
            Paragraphs(60 * em.w, Left)(
              """This test places windowdialogues.Menu instances around the outside of the target (red)
                |square, and in a couple of other places, using the
                |locators defined by `Location`.
                |Locators along the
                |north and west edges use an additional argument to
                |specify the glyph that will be located "there".
                |
                |When the red button on it is clicked or
                |ESC is typed, a windowdialogues menu pops down.
                |
                |""".stripMargin),
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
                Dialogue.OK(Paragraphs(align = Left, ems = 30)(
                  """
                    |This window should be
                    |at the South edge of the target.
                    |""".stripMargin), South(theTarget)).start()
            },
            TextButton("Dialogue a window at the north west") {
              _ =>
                val body = Paragraphs(align = Left, ems = 30)(
                  """
                    |This window should be located
                    |at the north west edge of the target.
                    |It's a little wider than the one at the south edge; and
                    |needed a little trick to be used so that its appearance
                    |could be known to the NorthWestFor locator.
                    |""".stripMargin).enlarged(20f)
                // Construct a dummy from the APPEARANCE of the intended Dialogue
                val dummy: Glyph = Dialogue.OK(body, East(theTarget)).theRoot
                Dialogue.OK(body, NorthWestFor(dummy)(theTarget)).start()
            }
          ), em)
      }
      nested.Layout.rightButtons().enlarged(20)
    }

  val TransformsPage = Page("Transforms*", "") {
    implicit val Style: StyleSheet = ApplicationStyle
    val nested = Notebook()
    val Page = nested.DefinePage
    Page("Turn", "Turn transforms") {

      def circ = PolygonLibrary.closeButtonGlyph.scaled(4)

      val h = PolygonLibrary.closeButtonGlyph.scaled(4).h
      val w = h * 0.25f

      def rect = Rect(w, h, fg = blueLine)
      // Concentric.centered(Rect(w, w/4, red), FilledOval(w, w, blue(alpha=0.3f))).framed(blue)

      def wr(deg: Scalar): Glyph = {
        val d = rect.diagonal
        val dd: Vec = d.turned(deg, Vec(d.x / 2, d.y / 2)) + Vec(d.x / 2, d.y / 2)
        Rect(dd.x, dd.y, redLine)
      }

      val (tightBox, nontightBox) = (redLine(width=2), redLine(color=green.color, width=2))

      def L(text: String, rot: Scalar, g: Glyph): Glyph =
        Label(f" $text%1s $rot%2.2f ").above(g.turned(rot).framed(nontightBox).enlarged(8f)).framed(fg = nothing).enlarged(8f)

      def T(text: String, rot: Scalar, g: Glyph): Glyph =
          Label(f" $text%1s $rot%2.2f ").above(g.turned(rot, tight=true).framed(tightBox).enlarged(8f)).framed(fg = nothing).enlarged(8f)

      def B(text: String, rot: Scalar, w: Scalar, h: Scalar): Glyph =
          Label(f" ($w%2.2f, $h%2.2f)\n($rot%2.2f)").scaled(0.8f).above(rect.turnedBoxed(w, h)(rot)).framed(tightBox).enlarged(8f)

      Col.centered(
        Paragraphs(60 * em.w, Left)(
          """The .turned transform with `tight=true` always yields a square bbox whose side is the larger of the two of the present glyph.
            |For near-rotationally-symmetric glyphs this bbox
            |fits more closely than the one yielded by`tight=false`.
            |
            | Hereunder R denotes a rectangle, C denotes a circular glyph, and T denotes a triangle.
            |Tight bounding boxes are shown in red, non-tight in green.
            |""".stripMargin),
        ex scaled 2,
        Row(
          L("R", 0, rect),
          L("R", 25, rect),
          L("R", 45, rect),
          L("R", 70, rect),
          L("R", 90, rect),
          L("R", 135, rect),
        ),
        Row(
          L("C", 0, circ),
          L("C", 25, circ),
          L("C", 45, circ),
          L("C", 70, circ),
          L("C", 90, circ),
          L("C", 135, circ),
        ),
        Row(
          T("C", 0, circ),
          T("C", 25, circ),
          T("C", 45, circ),
          T("C", 70, circ),
          T("C", 90, circ),
          T("C", 135, circ),
        ), Row(
          T("R", 0, rect),
          T("R", 25, rect),
          T("R", 45, rect),
          T("R", 70, rect),
          T("R", 90, rect),
          T("R", 135, rect),
        ),
        ex scaled 1.5f,
        Label("R/T turned d, for d in 0, -22.5, -45, -67.5, -90"), ex,
        Row.centered(
          {
            val (hh, ww) = (4.5f * h, h / 1.5f)
            val r = Rect(ww, hh, fg = Brush()(width = 2.5f, color = 0XFFff00ff))
            Label("tight=true\n").above(Concentric(
              (r(fg = black(width = 2.5f))).turned(0f, tight = true).framed(tightBox),
              (r(fg = redLine(width = 2.5f))).turned(-22.5f, tight = true).framed(tightBox),
              (r(fg = green(width = 2.5f))).turned(-45f, tight = true).framed(tightBox),
              (r(fg = blueLine(width = 2.5f))).turned(-67.5f, tight = true).framed(tightBox),
              (r().turned(-90f, true)).framed(tightBox),
              Point(fg = red(width = 4))
            ))
          }, em, em,
          {
            val r = PolygonLibrary.regularPolygon(3, fg = Brush()(width = 2.5f, color = 0XFFff00ff))
            Label("tight=true\n").above(Concentric(
              (r(fg = black(width = 2.5f))).turned(0f, tight = true).framed(tightBox),
              (r(fg = redLine(width = 2.5f))).turned(-22.5f, tight = true).framed(tightBox),
              (r(fg = green(width = 2.5f))).turned(-45f, tight = true).framed(tightBox),
              (r(fg = blueLine(width = 2.5f))).turned(-67.5f, tight = true).framed(tightBox),
              (r()).turned(-90f, tight = true).framed(tightBox),
              Point(fg = red(width = 4))
            ))
          }, em, em, Label("non-tight\nbboxes with\ncorresponding colours\n").above
          { val c0 = black(width = 2.5f, cap=SQUARE)
            val c1 = c0(color = red.color)
            val c2 = c0(color = green.color)
            val c3 = c0(color = blue.color)
            val c4 = c0(color = brown.color)
            val c5 = c0(color = lightGrey.color)
            val r = PolygonLibrary.regularPolygon(3, fg = Brush()(width = 2.5f, color = 0XFFff00ff))
            def  f(b: Brush, d: Scalar): Glyph = r(fg=b).turned(d).framed(b)
            Concentric(
              f(c1, -22.5f),
              f(c2, -45f),
              f(c3, -67.5f),
              f(c4, -90f),
              f(c0(width=4.5f, alpha=0.3f, cap=BUTT), 0f),
              Point(fg = red(width = 4))
            )
          }
        ) scaled 0.7f
      )
    }

    Page("Tight", "") {
      def circ = PolygonLibrary.closeButtonGlyph.scaled(4)
      val d = circ.w * (0.35f)
      val w = d*5f
      val h = w*.25f

      def rect = Rect(w, h, fg = blueLine)
      def tr = PolygonLibrary.star7(C=50f, R=50f, fg = Brush()(width = 2.5f, color = 0XFFff00ff))

      val (r, g, b) = (red(width=1, cap=SQUARE), green(width=1, cap=SQUARE), black(width=1, cap=SQUARE))
      Col.centered(Label("Tight  (red) versus non-tight (green) bounding boxes"), ex,
          Row.centered$(
            for { tight <- List(true, false) } yield
            Col.centered$(
              for { a <- List(0f, 20f, 50f, 90f, 140f, 180f, 230f, 275f) } yield
                Row.atTop(Label(f"$a%2.1f"), em scaled 2,
                  Row.atTop$(
                    for { glyph <- List(rect, tr) } yield
                    Row.atTop(
                      Concentric(
                          glyph(b).turned(a, tight).framed(if (tight) r else g).enlarged(10f),
                          Point(if (tight) r(width=4) else g(width=4))
                    ), em scaled 2)
                  )
                )
             )
          )
      )
    }

    Page("Skew", "Skew transforms") {
      import GlyphTransforms.Skewed
      import PolygonLibrary.star7

      def circ = PolygonLibrary.closeButtonGlyph.scaled(4)

      val h = PolygonLibrary.closeButtonGlyph.scaled(4).h
      val w = h * 0.25f

      def rect = Rect(w, h, fg = blueLine)

      def L(@unused text: String, skewX: Scalar, g: Glyph): Glyph =
        Label(f" x=$skewX%1.1f ").above(Skewed(skewX, 0f)(g).framed())


      val pics = Col.centered(
        Label(".skewed(x, 0)"), ex,
        Row(
          L("C", .0f, circ),
          L("C", .3f, circ),
          L("C", .4f, circ),
          L("C", .5f, circ),
          L("C", .6f, circ),
          L("C", .7f, circ),
        ),
        ex scaled 3,
        Row(
          L("C", .0f, rect),
          L("C", .3f, rect),
          L("C", .4f, rect),
          L("C", .5f, rect),
          L("C", .6f, rect),
          L("C", .7f, rect),
        ),
      )


      val base = rect.diagonal
      val BASE = base scaled 2f
      val skews = List(-0.95f, -0.7f, -0.5f, -0.3f, 0f, 0.3f, 0.5f, 0.7f, 0.9f)

      def Sk(sx: Scalar, sy: Scalar): Glyph = Col.centered(
        Label(f"$sx%1.1f\n$sy%1.1f"), Skewed(sx, sy)(Col.centered(star7(fg=red).scaled(.15f), Rect(BASE.x, BASE.y, fg = blueLine), Label("A"))).framed()
      )

      Col.centered(
        Paragraphs(60, Left)("g.skewed(dx, dy) skews g rightwards as y increases, and downwards as x increases"),
        ex, ex,
        Row.centered(
          Sk(0.5f, 0f), em,
          Sk(-0.5f, 0f), em,
          Sk(0f, 0.5f), em,
          Sk(0f, 0f), em,
          Sk(0f, -0.5f), em,
          Sk(0.5f, 0.5f), em,
          Sk(-0.5f, -0.5f)
        ), ex, ex,
        pics, ex, ex,
        Label("R.skewed(x, 0).above(R.skewed(0, x))"), ex,
        Row.centered$(
          skews.map { x =>
            Col.centered(
              Label(f"x = $x%3.1f"),
              FilledRect(BASE.x, BASE.y, fg = redLine).skewed(x, 0f).framed(),
              FilledRect(BASE.x, BASE.y, fg = redLine).skewed(0f, x).framed()
            ) enlarged 18f
          }) scaled 0.75f
      )
    }

    Page("Mirror", "Mirroring and skewing\n(using intrinsic Glyph Transforms)") {
      val bigAB = Label("AB")(HugeLabelStyle).scaled(1.5f)
      val bigCD = Label("CD")(HugeLabelStyle).scaled(1.5f)

      def framedA = bigAB().framed()

      def rotA = bigAB().rotated(3).framed()

      def rowABCD = Row(bigAB().rotated(3), bigCD().rotated(3)).framed()

      Col.centered(
        Label("S=_.skewed(0.5,0.5); M=_.mirrored(true, true)"), ex,
        ex,
        Row.centered(
          Label("g ").above(framedA.framed()), ex, ex,
          Label("S(g) ").above(framedA.skewed(0.5f, 0.5f).framed()), em, em,
          Label("M(g) ").above(framedA.mirrored(leftRight = true,
            topBottom = true).framed()), em, em,
          Label("S(M(g)) ").above(framedA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0.5f).framed()), em, em,
          Label("M(S(M(g))) ").above(framedA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0.5f).mirrored(leftRight = true, topBottom = true).framed())
        ), ex, ex, Label("s=_.skewed(0.5, 0)"), ex,
        Row.centered(
          Label("g ").above(rotA.framed()), ex, ex,
          Label("s(g) ").above(rotA.skewed(0.5f, 0f).framed()), em, em,
          Label("M(g) ").above(rotA.mirrored(leftRight = true, topBottom = true).framed()), em, em,
          Label("s(M(g)) ").above(rotA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0f).framed()), em, em,
          Label("M(s(M(g))) ").above(rotA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0f).mirrored(leftRight = true, topBottom = true).framed()),
        ), ex, ex, Label("m=_.mirrored(false, true)"), ex,
        Row.centered(
          Label("g ").above(rowABCD.framed()), ex, ex,
          Label("s(g) ").above(rowABCD.skewed(0.5f, 0f).framed()), em, em,
          Label("m(g) ").above(rowABCD.mirrored(leftRight = false, topBottom = true).framed()), em, em,
          Label("s(m(g)) ").above(rowABCD.mirrored(leftRight = false, topBottom = true).skewed(0.5f, 0f).framed()), em, em,
          Label("m(s(m(g))) ").above(rowABCD.mirrored(leftRight = false, topBottom = true).skewed(0.5f, 0f).mirrored(leftRight = false, topBottom = true).framed())
        ), ex,
        Label("Notice how the row (g) of vertical glyphs was skewed\nto the right as if from the top by m(s(m(g)))\n\nThis is the same effect as Skewed(-0.5,0)"), ex,
        rowABCD.skewed(-0.5f, 0)
      )
    }

    nested.Layout.rightButtons().enlarged(20)
  }

  val OverlayPage = Page("Overlays*", "Features implemented as overlay layers and annotations"){
    val noteBook = Notebook()
    val Page = noteBook.DefinePage
    // Each subpage of this page declares its own style,
    // usually the standard application style, except for
    // the Menus subpage, which declares local styles
    // for some menus.

    Page("Dialogues", "") {
      implicit val Style: StyleSheet = ApplicationStyle
      import styled.TextButton
      val anchor = FilledRect(150, 70, blue)

      def showChoice(choice: String): Unit = {
        val response = if (choice ne null)  s"You chose $choice" else "You quit the dialogue"
        overlaydialogues.Dialogue.OK(Label(response)).South(anchor).start()
      }

      def diaGlyph(s: String): Glyph =
        Row.centered(PolygonLibrary.star7(fg=redLine, R=50f, C=50f), em,
          Paragraphs(35, Justify)(
          s"""$s:
            |This is a long pro-forma text for a dialogue that I expect to be justified properly
            |in all the dialogues.
            |""".stripMargin))

      import overlaydialogues.Dialogue._
      Col.centered(
        Paragraphs(50, Justify)(
          """
            |On this page we are testing modal dialogues implemented on
            |overlays within the current window. You can
            |exit the dialogue without making a choice by clicking on
            |the topmost grey bar or typing ESC. You can shift the
            |location of the dialogue by using the direction keys.
            |
            | Each dialogue explains where it was intended to pop up
            |(relative to the blue rectangle). The location of a dialogue
            |that might be partly off screen are adjusted to make it (as
            |far as possible) visible.
            |
            |""".stripMargin), ex,
        anchor, ex,
        Row(
          TextButton("E/OKNO"){
              _ => OKNO(diaGlyph("East"))
                   .East(anchor)
                   .andThen{ result => showChoice(s"$result") }
          }, em,
          TextButton("W/OKNO") {
            _ =>
              OKNO(diaGlyph("West"))
                .West(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
          TextButton("N/ABC") {
            _ =>
              CHOOSE(diaGlyph("North"))("A", "B", "C")
                .North(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
        ), ex,
        Row(
          TextButton("W/ABC") {
            _ =>
              CHOOSE(diaGlyph("West"))("A long button", "a Bigger button", "C")
                .West(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
          TextButton("InFront/ABC") {
            _ =>
              CHOOSE(diaGlyph("InFront"))("A long button", "a Bigger button", "C")
                .InFront(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
          TextButton("S/ABC") {
            _ =>
              CHOOSE(diaGlyph("South"))("A long button", "a Bigger button", "C")
                .South(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
        ), ex,
        Row(
          TextButton("W/ABC[long]") {
            _ =>
              CHOOSE(diaGlyph("West"))("An extraordinarily long button", "a Bigger button that might cause jiggle", "C")
                .West(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
          TextButton("InFront/ABC[long]") {
            _ =>
              CHOOSE(diaGlyph("InFront"))("An extraordinarily long button", "a Bigger button that might cause jiggle", "C")
                .InFront(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
          TextButton("S/ABC[long]") {
            _ =>
              CHOOSE(diaGlyph("East"))("An extraordinarily long button", "a Bigger button that might cause jiggle", "Really")
                .East(anchor)
                .andThen { result => showChoice(s"$result") }
          }, em,
        ), ex,
      )
    }

    Page("Menus", "") {
      import overlaydialogues.Dialogue._
      import Styles.MenuStyle

      lazy val menuA: Glyph = {
        implicit val Style: StyleSheet = ApplicationStyle
        Menu("A") (
          MenuButton("A1") { _ => println("A1")  },
          Label("Not a button"),
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
        implicit val Style: StyleSheet = ApplicationStyle
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

      val LocalSheet: StyleSheet = new ApplicationStyle.Derived {
        override lazy val menuStyle: MenuStyle = delegate.menuStyle.copy(
          button = delegate.menuStyle.button.copy(frame = Decoration.Shaded(fg = red(width = 0), bg = lightGrey, enlarge = 15, delta = 15, down = true)),
          nestedButton = delegate.menuStyle.button.copy(frame = Decoration.Shaded(fg = blue(width = 0), bg = green(alpha = 0.25f), down = true)),
          reactive = delegate.menuStyle.reactive.copy(frame = Decoration.Framed(fg = lightGrey(width = 5, cap=ROUND), bg = green(alpha = 0.25f))),
          inactive = Decoration.Framed(fg = darkGrey(width = 5, cap=ROUND), bg = green(alpha = 0.25f)),
          bg = green(alpha = 0.25f)
        )
      }


      lazy val menuD =  {
        implicit val Style: StyleSheet = LocalSheet
        Menu("D")(
          MenuButton("A1") { _ => println("A1") },
          Row.centered(Label("Enable E: "), MenuCheckBox(true){ state=>menuE.enabled(state) }),
          Label("WTF!"),
          NestedMenu("innerCC >")(
            MenuButton("CCC1") { _ => println("CCC1") },
            MenuButton("CC2") { _ => println("CCC2") },
          ),
          MenuButton("A2") { _ => println("A2") },
          FilledRect(20f, 50f, fg = red), //PolygonLibrary.hideButtonGlyph().scaled(2.5f),
          MenuButton("A3") { _ => println("A3") },
          MenuButton("Disable C") { _ =>
            menuC.enabled(false); ()
          },
          MenuButton("Enable C") { _ =>
            menuC.enabled(true); ()
          }
        )
      }

      lazy val menuE: Glyph = {
        implicit val Style: StyleSheet = LocalSheet
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
        ) // TODO: ShadedMenuStyle
      }

      lazy val menuF: Glyph = {
        implicit val localStyle: StyleSheet = HugeLabelStyle
        Menu("F")(
          MenuButton("C1") { _ => println("C1") },
          NestedMenu("innerCC >")(
            MenuButton("CCC1") { _ => println("CCC1") },
            MenuButton("CC2")  { _ => println("CCC2") },
          ),
          MenuButton("C2") { _ => println("C2") },
          NestedMenu("innerC >")(
            MenuButton("CC1") { _ => println("CC1") },
            MenuButton("CC2") { _ => println("CC2") },
            NestedMenu("innerCC >")(
              MenuButton("CCC1") { _ => println("CCC1") },
              MenuButton("CC2")  { _ => println("CCC2") },
            )
          )
        )(ApplicationStyle)
      }



      { implicit val Style: StyleSheet = ApplicationStyle
        Col.centered(
          Label("Two menus (application style)"),
          Row(menuA, em, menuC), ex,
          Label("Two menus (different style)"),
          Row(menuE, em, menuD), ex,
          Label("A huge menu, hiding behind a 'normal' button"),
          menuF
        )
      }
    }

    Page("Locators", "") {
      implicit val Style: StyleSheet = ApplicationStyle

      import io.github.humbleui.types.IRect

      def rect = Rect(200, 250, red).framed(blue)

      lazy val theTarget = rect

      def content: IRect = theTarget.guiRoot.eventHandler.window.getContentRect

      Row(em,
        Col.centered(
          Paragraphs(50, Justify)(
            """This test places (OK) dialogues around the outside of the target Red
              |square using the intrinsic methods of overlaydialogues.Dialogue
              |
              |A dialogue menu disappears when the red button on it is clicked, or the ESC
              |key is hit. As an exception to the overlaydialogues.Dialogue norm,
              |for this page only, a mouseclick nowhere near any dialogue
              |will make one of the dialogues disappear.
              |
              |""".stripMargin),
          ex scaled 5,
          TextButton("Show Locators") {
            _ =>
              def dial(string: String): overlaydialogues.Dialogue[Unit] = {
                val d = overlaydialogues.Dialogue.OK(Label(string))
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
      // TODO: design a high-level annotation API
      implicit val Style: StyleSheet = ApplicationStyle

      import BooleanGlyphs.OnOffButton
      val anchor = INVISIBLE()

      var gridAnnotation: Option[RootLayer] = None

      /** Make a grid that covers `root` and has the grid-off button on it */
      def makeGridGlyph(root: Glyph): Glyph = {
        Concentric.atRight(PolygonLibrary.grid(root.diagonal), gridCheckboxForGrid)
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
        val thisButton: Glyph = TextButton("This is the East, click to pop me down >>> "){
          _ => setLocalState(false)
        }
        thisButton@@(anchor.guiRoot.diagonal.scaled(1.0f, 0.5f) - thisButton.diagonal)
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
        Paragraphs(40, Justify)(
          """
            |This page tests annotation-style overlays, using
            |the low-level annotation API.
            |
            | The checkbox below enables/disables overlaying of a 10x10 grid on
            |the whole of the current window, no matter what page/subpage is showing.
            |The grid can be useful when exploring the dimensions of
            |glyphs.
            |
            |  A checkbox always appears on the grid: pressing this disables the grid, which can only be re-enabled by
            |the checkbox below.
            |""".stripMargin)(ApplicationStyle),
        Row.centered(Label("Grid: "), gridCheckboxForPage), ex, ex,
        Paragraphs(40, Justify)(
          """
            |The button below pops up
            |an annotation overlay that points to
            |the East wall of the window. The overlay stays up until its button is
            |pressed, no matter what page/subpage of the app is showing.
            |""".stripMargin), ex,
        TextButton("Point to the East wall of the window") {
          _ => setLocalState(true)
        }
      ).enlarged(20)
    }

    Page("Raw", "") {
      implicit val Style: StyleSheet = ApplicationStyle
      import styled.TextButton
      val anchor = INVISIBLE()
      var strictHiding = true
      def applyOverlapPolicy(): Unit = {
        anchor.guiRoot.Overlay.top match {
          case None =>
          case Some(layer) => layer.strictHiding = strictHiding
        }
      }
      val overButton = Row(
        TextButton("[[[[[ Pop this layer ]]]]]") {
          _ => anchor.guiRoot.Overlay.pop()
        }
      )

      def SEText = "↘"
      val NWText = "↖"
      def CROSS  = "❌"

      val text = Paragraphs(35, Justify)(
        """
          |This is an overlay that can be nudged or removed.
          |It is here to demonstrate that pop-ups
          |on the main body of the controlling window
          |can be straightforwardly implemented.
          |""".stripMargin)

      lazy val overlay: Glyph = {
        Col.centered(
          Row(TextButton(CROSS) {
            _ =>
              anchor.guiRoot.Overlay.top match {
                case None =>
                case _ => anchor.guiRoot.Overlay.pop()
              }
          }, em, em, text), ex,
          Row.centered(
            TextButton(NWText)  {
              _ =>
                overlay @@ (overlay.location - (30, 50))
                anchor.guiRoot.Overlay.set(overlay)
                applyOverlapPolicy()
            }, em,
            TextButton(SEText) {
              _ =>
                overlay @@ (overlay.location + (30, 50))
                anchor.guiRoot.Overlay.set(overlay)
                applyOverlapPolicy()
            }, em,
          ), ex,
          TextButton("Push an (effectively modal) overlay") {
            _ =>
              overButton @@ (overlay.location + (overlay.w/2f-overButton.w/2f, 0f))
              anchor.guiRoot.Overlay.pushLayer(overButton, isModal = false)
          },
        ).enlarged(30, bg = white).framed(fg = blue)
      }
      Col.centered(
        Paragraphs(35, Left)(
          """
            |This page provides a few simple tests of the "raw" overlay
            |implementation.
            |""".stripMargin), ex,
        TextButton("Show the non-modal overlay") {
          _ =>
            anchor.guiRoot.Overlay.set(overlay @@ (90, 90), isModal = false)
            applyOverlapPolicy()
        }, ex,
        TextButton("Show the modal overlay") {
          _ =>
            anchor.guiRoot.Overlay.set(overlay @@ (190, 190), isModal = true)
            applyOverlapPolicy()
        }, ex,
        anchor,
        Row(
          TextButton(NWText) {
            _ =>
              overlay @@ (overlay.location - (30, 50))
              anchor.guiRoot.Overlay.set(overlay, strictHiding = strictHiding)
          }, em,
          TextButton(SEText) {
            _ =>
              overlay @@ (overlay.location + (30, 50))
              anchor.guiRoot.Overlay.set(overlay, strictHiding = strictHiding)
          }
        ), ex,
        Col.centered(
          Row.centered(
              Label("Enable strict hiding policy: "),
              CheckBox(initially = strictHiding) {
                state =>
                  strictHiding = state
                  applyOverlapPolicy()
              }),
          Label("viz: partly occluded buttons are effectively hidden")
        ), ex,
      )
    }

    noteBook.Layout.rightButtons().enlarged(20)
  }

  val FramePage = Page("Framing*", "") {
    implicit val Style: StyleSheet = ApplicationStyle
    val noteBook = Notebook()
    val Page = noteBook.DefinePage

    Page("Text 1", "Text (curved framing)") {
      val fg = blue(width = 15, cap = ROUND)

      def short = Label("short").enlarged(10)

      def med = Label("A medium length text").enlarged(10)

      def long = Paragraphs(25, Justify)("A text paragraph of width 25em that may be touched at corners by a thick low curvature frame.").enlarged(10)

      def row(rf: Scalar): Glyph = {
        Col.atLeft(
          Label(f"radiusFactor=$rf%1.3f\n").scaled(0.8f),
          Row.centered(short.framed(fg, nothing, rf), em,
            med.framed(fg, nothing, rf), em,
            long.framed(fg, nothing, rf)))
      }



      Col.centered$(
        List(.5f, .3f, .25f, .125f).map(row(_))
      )
    }

    Page("Text 2", "Text (curved framing)") {
      val fg = blue(width = 6, cap = ROUND)

      def short = Label("short").enlarged(10).rotated(1)

      def med = Label("A medium length text").enlarged(10).rotated(1)

      def long = Paragraphs(14, Left)("A tall text paragraph. It may be touched at corners by its frame, unless it has first been enlarged.").enlarged(10)

      val width = 3.5f*(short.w+med.w+long.w)

      def row(rf: Scalar, enlarge: Scalar = 0f): Glyph = {
        Col.centered(
          ex.scaled(0.5f),
          Label(f"radiusFactor=$rf%1.3f").scaled(0.8f), ex.scaled(0.5f),
          Row.centered(
            short.enlarged(enlarge).framed(fg, nothing, rf), em,
            med.enlarged(enlarge).framed(fg, nothing, rf), em,
            long.enlarged(enlarge).framed(fg, nothing, rf)))
      }

      val emWidth = em.w

      Col.centered(
        FixedSize.Row(width)(row(0.5f), FixedSize.Space(emWidth, 100f), row(.3f)), ex,
        FixedSize.Row(width)(row(0.25f), FixedSize.Space(emWidth, 100f), row(.125f)), ex,
        Label("Effects of enlargement by 25f before framing\n"),
        FixedSize.Row(width)(row(.3f, 25f), tab, row(0.125f, 25f)),
      )

    }

    Page("Framed", "Glyph framing") {
      val (x, y) = (150f, 100f)
      def glyph = Label("Text Label").scaled(1.5f)
      def star = PolygonLibrary.filledStargon(9, fg=blueLine).scaled(.5f).framed()
      def cross = Polygon(star.w, star.h, blue(width=4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0) ) scaled 0.5f

      Col.centered(
          Row(nothing, nothing).centered(
            star, em,
            star.mounted(red(width = 38, cap = SQUARE), green), em,
            star.mounted(red(width = 8, cap = ROUND), green), em,
            star.mounted(red(width = 8, cap = ROUND), red)), ex,
          Row.centered(cross, em,
            cross.mounted(yellow(width = 8, cap = ROUND), nothing), em,
            cross.mounted(yellow(width = 8, cap = ROUND), red), em,
            cross.mounted(yellow(width = 16, cap = ROUND), nothing), em,
            cross.mounted(yellow(width = 16, cap = ROUND), red), em,
          ), ex,
          Row.centered(
            star.mounted(fg = black(width = 4, cap = ROUND), bg = nothing), em,
            star.mounted(fg = black(width = 10, cap = ROUND), bg = nothing), em,
            star.mounted(fg = black(width = 20, cap = ROUND), bg = nothing), em,
            star.mounted(fg = black(width = 30, cap = ROUND), bg = nothing), em,
            star.mounted(fg = black(width = 40, cap = ROUND), bg = yellow), em
          ), ex,
          Row.centered(
            cross.framed(fg = yellow(width = 24, cap = SQUARE), bg = nothing), em,
            cross.framed(fg = yellow(width = 24, cap = ROUND), bg = nothing), em,
            cross.mounted(yellow(width = 18, cap=ROUND), nothing), em,
            cross.mounted(yellow(width = 18, cap=ROUND), green), em,
            Label("FOOTLE").rotated(1).framed(fg = green(width = 24, cap = ROUND), bg = nothing), em,
            Label("FOOTLE").rotated(1).framed(fg = green(width = 24, cap = ROUND), bg = red), em,
            Label("FOOTLE").rotated(1).framed(fg = green(width = 24, cap = SQUARE), bg = nothing), em,
          ), ex,
        Row.centered(
          glyph.framed(red(width = 30, cap = SQUARE), green), em,
          glyph.framed(red(width = 30, cap = ROUND), nothing)), ex,
        Row.centered(
          glyph.framed(red(width = 30, cap = ROUND), red), em,
          glyph.framed(red(width = 30, cap = ROUND), green)
        ), ex,
        ).scaled(0.98f)
    }

    Page("Edged", "Glyph edging") {
      val (x, y) = (150f, 100f)
      def glyph = Label("Text Label").scaled(1.5f)
      def star = PolygonLibrary.filledStargon(9, fg=blueLine).scaled(.5f).framed()
      def cross = Polygon(star.w, star.h, blue(width = 4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0)) scaled 0.5f

      Col.centered(
        Row(nothing, nothing).centered(
          star, em,
          star.edged(red(width = 8, cap = SQUARE), green), em,
          star.edged(red(width = 8, cap = ROUND), green), em,
          star.edged(red(width = 8, cap = ROUND), red)
        ), ex,
        Row.centered(cross, em,
          cross.edged(yellow(width = 8, cap = ROUND), nothing), em,
          cross.edged(yellow(width = 8, cap = ROUND), red), em,
          cross.edged(yellow(width = 16, cap = ROUND), nothing), em,
          cross.edged(yellow(width = 16, cap = ROUND), red), em,
        ), ex,
        Row.centered(
          star.edged(fg = black(width = 4, cap = ROUND), bg = nothing), em,
          star.edged(fg = black(width = 10, cap = ROUND), bg = nothing), em,
          star.edged(fg = black(width = 20, cap = ROUND), bg = nothing), em,
          star.edged(fg = black(width = 30, cap = ROUND), bg = nothing), em,
          star.edged(fg = black(width = 40, cap = ROUND), bg = yellow), em
        ), ex,
        Row.centered(
          cross.framed(fg = yellow(width = 24, cap = SQUARE), bg = nothing), em,
          cross.framed(fg = yellow(width = 24, cap = ROUND), bg = nothing), em,
          cross.edged(yellow(width = 18, cap = ROUND), nothing), em,
          cross.edged(yellow(width = 18, cap = ROUND), green), em,
          Label("FOOTLE").rotated(1).edged(fg = green(width = 24, cap = ROUND), bg = nothing), em,
          Label("FOOTLE").rotated(1).edged(fg = green(width = 24, cap = ROUND), bg = red), em,
          Label("FOOTLE").rotated(1).edged(fg = green(width = 24, cap = SQUARE), bg = nothing), em,
        ), ex,
        Row.centered(
          glyph.edged(red(width = 30, cap = SQUARE), green), em,
          glyph.edged(red(width = 30, cap = ROUND), nothing)), ex,
        Row.centered(
          glyph.edged(red(width = 30, cap = ROUND), red), em,
          glyph.edged(red(width = 30, cap = ROUND), green)
        ), ex,
      ).scaled(0.98f)
    }

    noteBook.Layout.rightButtons().enlarged(20)
  }

  val StylesPage = Page("Styles*", "") {
    implicit val Style: StyleSheet = ApplicationStyle
    val noteBook = Notebook()
    val Page = noteBook.DefinePage

    Page("Framed", "") {
      import Styles.Decoration.Framed
      import styled.TextButton
      val buttonStyle: ButtonStyle = ApplicationStyle.buttonStyle
      Col.centered(
        Paragraphs(55, Left)(
          """
            | The buttons here are all of the form
            |
            |<<<< styled.TextButton("..."){ _ => }(style)
            |
            |
            | where
            |<<<< style =
            |<<<<<<< buttonStyle.copy(
            |<<<<<<<<<<<< frame = Framed(fg = darkGrey(width=...),
            |<<<<<<<<<<<<<<<<<<<<<<<<<< bg = lightGrey,
            |<<<<<<<<<<<<<<<<<<<<<<<<<< enlarge = 0.25f,
            |<<<<<<<<<<<<<<<<<<<<<<<<<< radiusFactor = ...
            |<<<<<<< ))
            |<<<<
            |""".stripMargin), ex,
        DetailedTextButton("(width=2, radiusFactor=0.5))") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width=2), lightGrey, enlarge=0.25f, radiusFactor = 0.5f))), ex,
        DetailedTextButton("(width=4, radiusFactor=0.5)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width=4), lightGrey, enlarge=0.25f, radiusFactor = 0.5f))), ex,
        DetailedTextButton("(width=8, radiusFactor=0.5)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width=8), lightGrey, enlarge=0.25f, radiusFactor = 0.5f))), ex,
        DetailedTextButton("(width=10, radiusFactor=0.5)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width=10), lightGrey, enlarge=0.25f, radiusFactor = 0.5f))), ex,
        DetailedTextButton("(width=2, radiusFactor=0.25)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width = 2), lightGrey, enlarge = 0.25f, radiusFactor = 0.25f))), ex,
        DetailedTextButton("(width=4, radiusFactor=0.25)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width = 4), lightGrey, enlarge = 0.25f, radiusFactor = 0.25f))), ex,
        DetailedTextButton("(width=8, radiusFactor=0.25)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width = 8), lightGrey, enlarge = 0.25f, radiusFactor = 0.25f))), ex,
        DetailedTextButton("(width=10, radiusFactor=0.25)") { _ => }(buttonStyle.copy(frame = Framed(darkGrey(width = 10), lightGrey, enlarge = 0.25f, radiusFactor = 0.25f))), ex,
      )
    }

    Page("Blurred", "") {
      import styled.TextButton
      val buttonStyle = ApplicationStyle.buttonStyle
      val localStyle: ButtonStyle = ApplicationStyle.buttonStyle.copy(up=buttonStyle.up.copy(fg=white))
      Col.centered(
        Paragraphs(60, Left)(
          """
            | The buttons here are all of the form
            |
            |<<<< styled.TextButton("..."){ _ => }(style)
            |
            |
            | where
            |<<<< localStyle =
            |<<<<<<< buttonStyle.copy(up=buttonStyle.up.copy(fg=white))
            |<<<< style =
            |<<<<<<< localStyle.copy(
            |<<<<<<<<<<<< frame=
            |<<<<<<<<<<<<<<<< Decoration.Blurred(...)
            |<<<<<<< ))
            |<<<<
            |""".stripMargin), ex,
        DetailedTextButton("Blurred(fg=blue, blur=10f, spread=5f)"){ _ => }(localStyle.copy(frame=Decoration.Blurred(fg=blue, blur=10f, spread=5f))), ex,
        DetailedTextButton("Blurred(fg=blue, blur=10f, spread=10f)"){ _ => }(localStyle.copy(frame=Decoration.Blurred(fg=blue, blur=10f, spread=10f))), ex,
        DetailedTextButton("Blurred(fg=blue, blur=20f, spread=10f)"){ _ => }(localStyle.copy(frame=Decoration.Blurred(fg=blue, blur=20f, spread=10f))), ex,
        ex, Paragraphs(55, Left)(
          """
            |<<<< style=buttonStyle.copy(frame=Decoration.Unframed)
            |""".stripMargin),
        ex, DetailedTextButton("This is an unframed button with an edge around it"){ _ => }(buttonStyle.copy(frame=Decoration.Unframed)).edged(), ex,
      )
    }

    Page("Shaded", "") {
      import Styles.Decoration._
      implicit val buttonStyle: ButtonStyle = ApplicationStyle.buttonStyle.copy(frame = Shaded(black, white))
      implicit val labelStyle: GlyphStyle = ApplicationStyle.labelStyle.copy(fg = darkGrey, bg = green)

      Col.centered(
        DetailedTextButton("Shaded StyledButton (18, down)") { _ => }(buttonStyle.copy(frame = Shaded(darkGrey, lightGrey, delta = 18f, down = true))), ex,
        DetailedTextButton("Shaded StyledButton (8, up)") { _ => }(buttonStyle.copy(frame = Shaded(darkGrey, lightGrey, delta = 8f, down = false))), ex,
        DetailedTextButton("Shaded StyledButton (12, up)") { _ => }(buttonStyle.copy(frame = Shaded(darkGrey, lightGrey, delta = 12f, down = false))), ex,
        DetailedTextButton("Shaded StyledButton (18, up)") { _ => }(buttonStyle.copy(frame = Shaded(darkGrey, green, delta = 18f, down = false))), ex,
        Label("Shaded Label (18, up)").shaded(delta = 18f, down = false), ex,
        Label("Shaded Label (18, enlarge=0, up)").shaded(enlarge = 0f, delta = 18f, down = false), ex,
      )
    }

    noteBook.Layout.rightButtons().enlarged(20)
  }

  val EventsPage = Page("Events*", "") {
    implicit val Style: StyleSheet = ApplicationStyle

    val nested = Notebook()
    val Page = nested.DefinePage

    Page("Events", "") {
      val labelStyle = ApplicationStyle.labelStyle
      val theLog = StringLog(60, 25)(labelStyle)

      object CatchEvents extends ReactiveGlyph {

        import io.github.humbleui.jwm._

        var nEvents: Int = 0
        var elideAdjacentMoves: Boolean = false
        var lastEvent: String = ""
        var lastRecord: String = ""

        def record(s: String): Unit = {
          lastRecord = f"$nEvents%05d $s"
          if (elideAdjacentMoves && s.startsWith("Move") && lastEvent.startsWith("Move")) {

          } else {
            theLog.println(lastRecord)
          }
          lastEvent = s
          reDraw()
          nEvents += 1
        }

        override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = {
          record(s"StyledButton ${Modifiers.toBitmap(event).toLongString}@$location")
        }

        /** If the glyph wants to react to a mouse movement */
        override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = {
          record(s"Move${Modifiers.toBitmap(event).toLongString}@$location")
        }

        override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = {
          record(s"Wheel(${event.getDeltaX}, ${event.getDeltaY})")
        }

        override def accept(key: EventKey, location: Vec, window: Window): Unit = {
          record(s"Key ${Modifiers.toBitmap(key).toLongString} ${key.getKey} ")
        }

        override def accept(key: EventTextInput, location: Vec, window: Window): Unit = {
          record(s"Text ${Modifiers.toBitmap(key).toLongString} ${key.getText} ")
        }

        override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
          record(s"TextMarked ${Modifiers.toBitmap(key).toString} ${key.getText}")
        }

        // Synthetic events delivered by the standard event handler
        override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {
          var key = ""
          event match {
            case _: GlyphEnter =>
              import io.github.humbleui.jwm.MouseCursor
              key = s"GlyphEnter [keyboard grabbed]) ${Modifiers.toBitmap(event).toLongString}"
              guiRoot.grabKeyboard(this)
              window.setMouseCursor(MouseCursor.POINTING_HAND)
              reDraw() // window.requestFrame()
            case _: GlyphLeave =>
              key = s"GlyphLeave ${Modifiers.toBitmap(event).toLongString}"
              guiRoot.freeKeyboard()
              guiRoot.giveupFocus()
          }
          record(key)
        }

        // Synthetic events delivered by the standard event handler on focusin/focusout
        override def accept(event: RootGlyphEvent, location: Vec, window: Window): Unit = {
          record(event.toString)
        }

        /**
         * Draw the glyph on the surface at its given size (as if at the origin).
         */
        def draw(surface: Surface): Unit = {
          drawBackground(surface)
          surface.declareCurrentTransform(this)
          val theLast = Text(lastRecord, font = labelStyle.font).asGlyph(fg)
          surface.withClip(diagonal) {
            val dy = (diagonal.y - theLast.h) / 2f
            val dx = em.w // (diagonal.x - theLast.w) / 2f
            surface.withOrigin(dx, dy) {
              theLast.draw(surface)
            }
          }
        }


        /**
         * The diagonal size of the glyph
         */
        def diagonal: Vec = Vec(60 * em.w, 5 * ex.h)

        /** A copy of this glyph; perhaps with different foreground/background */
        def copy(fg: Brush, bg: Brush): Glyph = null

        val fg: Brush = black
        val bg: Brush = realYellow
      }

      Col.centered(
        Paragraphs(50 * em.w, Left)(
          """Mouse and keyboard events happening in the coloured frame below are shown in the log beneath it.
            |The most recent event is also shown in the coloured frame.
            |
            | Move events occur very frequently as the mouse traverses the frame, and successive Moves
            |in a sequence are usually for physically close locations. Checking the box below suppresses the second
            |and subsequent Move reports in such a sequence.
            |""".stripMargin), ex scaled 2,
        ex scaled 2,
        Row.centered(Label("Shorten the log of Move sequences: "), CheckBox(initially = CatchEvents.elideAdjacentMoves) {
          state =>
            CatchEvents.elideAdjacentMoves = state
            theLog.println(if (state) "You are now eliding adjacent move events" else "You are now showing adjacent move events")
        }), ex scaled 2,
        CatchEvents.framed(), ex scaled 2,
        theLog.framed()
      )

    }

    Page("Windows", "") {
      import Location._
      import windowdialogues.Dialogue

      import io.github.humbleui.jwm.{App, Screen}

      // App.methods can only be called after the App has started.
      // So we can only configure the popup in response to a button-press
      lazy val screenButton: Glyph = TextButton("Screens") { _ =>
        val screens = App.getScreens.toList

        def showScreen(s: Screen): Glyph = {
          import RectIO._
          val prim = if (s.isPrimary) "*" else " "

          Label(center(15)(f"${s._id}%15d$prim%1s") +
            center(26)(ir(s._bounds)) +
            center(26)(ir(s._workArea)))
        }

        def center(n: Int)(s: String): String = {
          val sp = "                              "
          val l = sp.take((n - s.length) / 2)
          val r = sp.take(n - s.length - l.length)
          l + s + r
        }

        val header = center(16)("ID") + center(25)("BOUNDS") + center(25)("WORK")
        Dialogue.OK(
          Col.atLeft(
            Label(header, Center, HelpStyle.labelStyle),
            Col.atLeft$(screens.map(showScreen(_)))),
          RelativeTo(screenButton), "Screens").start()
      }

      import RectIO._

      // App.methods can only be called after the App has started.
      // So we can only configure this popup in response to a button-press
      lazy val windowsButton: Glyph = TextButton("Windows") { _ =>
        import io.github.humbleui.jwm.Window
        lazy val windows: List[Window] = App._windows.toArray.toList.map(_.asInstanceOf[Window]) // horrid!

        def showWindow(w: Window): Glyph = {
          Label(s"${ir(w.getWindowRect)} ${ir(w.getContentRect)} ${w.getScreen._id}[${w.getScreen.getScale}]")
        }

        val header = "Window"
        Dialogue.OK(
          Col.atLeft(
            Col.atLeft$(windows.map(showWindow(_)))),
          RelativeTo(windowsButton),
          "Windows")
          .start()
      }

      import HelpStyle.Spaces.{em, ex}

      Col.centered(
        Paragraphs(50, Left)(
          """The Screens/Windows buttons pop up lists of screens/windows whenever they are pressed.
            |Sizes and locations are in physical (pixel) coordinates. Locations are given relative
            |to the location of the primary screen; and this means that in a multi-screen layout
            |some coordinates may be negative.
            |
            | Each Screen entry shows the ID of the screen, its bounds (as size@location), and its
            |work area (as size@location). The work area may be somewhat smaller than the size depending
            |on the exact physical layout of the screens.
            |
            | Window entries show the size and locations of the application's windows and work area
            |on the screen, followed by the id (and scale factor) of the screen on which they are
            |being displayed at the moment the Windows button was pressed.
            |
            |""".stripMargin), ex,
        Row(screenButton, em, windowsButton)
      )
    }


    nested.Layout.rightButtons().enlarged(20)
  }

  val etcPage = Page("Etc*", "") {
    implicit val Style: StyleSheet = ApplicationStyle
    val nested = Notebook()
    val Page = nested.DefinePage

    Page("Animation", "") {
      import DynamicGlyphs.{Periodic, Transform, Transformable}
      val shape = Glyphs.Concentric(bg=yellow)(
                       FilledOval(40, 40, fg=blue),
                       FilledRect(30, 10, fg=red) beside FilledRect(10, 10, fg=green))

      var lastDriver: List[Periodic[Int]] = Nil

      class Animation() {
        lazy val button = ReactiveGlyphs.ColourButton(shape, green, red, background = true) {
          _ =>
            if (driver.running) driver.stop() else driver.start()
            lastDriver = List(driver)
        }

        val transforms: Seq[Transform] = {
          val steps = (5 to 10).map{ i => i.toFloat / 5 }
          val sizes = steps ++ steps.reverse
          for { s <- sizes; r <- 0 to 15  }
            yield { glyph: Glyph => glyph.scaled(s).turned(r*22.5f, tight = true) }
        }

        lazy val animated: Transformable = Transformable(button, transforms)
        lazy val driver:   Periodic[Int] = Periodic[Int](animated, 2.0)
      }

      val animations: Seq[Animation] = for { i<-1 to 12 } yield new Animation()

      Col.centered(
        Paragraphs(50, Justify)(
          """A grid of rotating buttons. Individual buttons are started/stopped
            |by clicking on them; and can be started or stopped together with
            |the Start all / Stop all toggle button. The speed of the last
            |started/stopped button(s) can be adjusted with the Faster/Slower
            |buttons.
            |
            |""".stripMargin), ex,
        Row(
          TextToggle(whenFalse="Start all", whenTrue="Stop all", initially = false){
            case true  =>
              lastDriver = animations.map(_.driver).toList
              for { animation <- animations } animation.driver.start()
            case false =>
              lastDriver = animations.map(_.driver).toList
              for { animation <- animations } animation.driver.stop()
          }, em,
          TextButton("Faster"){
            _ => for { driver <- lastDriver  } if (driver.msPerFrame>4) driver.msPerFrame /= 2
          }, em,
          TextButton("Slower"){
            _ => for { driver <- lastDriver  } driver.msPerFrame *= 2
          }
        ), ex, ex,
        NaturalSize.Grid(bg=lightGrey).grid(width=4)(animations.map(_.animated)), ex, ex, ex,
      )
    }

    Page("Grid", "") {
      val nested = Notebook()
      val Page = nested.DefinePage

      Page("Rows/Cols", "data = 8 blurred-frame buttons") {
        val prevailingButtonStyle = ApplicationStyle.buttonStyle
        implicit val blurred: ButtonStyle =
          prevailingButtonStyle.copy(
            frame = Decoration.Blurred(fg = blue, blur = 10, spread = 5),
            up = prevailingButtonStyle.up.copy(fg = yellow),
            hover = prevailingButtonStyle.hover.copy(fg = white),
          )

        def data = (1 to 8).map {
          i => DetailedTextButton(f"Button $i%d") { _ => println(i) }(blurred)
        }

        Col.centered(
          Col.atLeft(
            Label(".grid(width=5)(data) as rows") above
              NaturalSize.Grid(fg = red(width = 0), padx = 20f, pady = 2f).grid(width = 5)(data), ex,
            Label(".grid(width=4)(data) as rows") above
              NaturalSize.Grid(fg = red(width = 0), padx = 20f, pady = 2f).grid(width = 4)(data)).scaled(1f), ex, ex, ex,
          NaturalSize.Row(
            Label(".grid(height=5)(data) as cols") above
              NaturalSize.Grid.grid(height = 5)(data).framed(fg = redFrame), em,
            Label(".grid(height=4)(data) as cols") above
              NaturalSize.Grid.grid(height = 4)(data.map(_.copy())).framed(fg = redFrame)).scaled(1f), ex,
        ) enlarged (50)
      }

      Page("Cell sizes", "data = 9 variable sized labels. Grid(... padx=10, pady=10)") {
        val data =
          for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
            Label(f"$i.scaled($scale%1.1f)").scaled(scale)

        Col.centered(
          Col.atLeft(
            Label(".grid(width=3)(data) -- row data as uniform size cells"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(data), ex,
            Label(".grid(height=3)(data) -- col data as uniform size cells"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(height = 3)(data), ex, ex, ex,
            Label(".rows(width=3)(data) -- row data as uniform width rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).rows(width = 3)(data), ex,
            Label(".cols(height=3)(data) -- col data as uniform height rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).cols(height = 3)(data), ex, ex, ex,
            Label(".table(width=3)(data) -- row data as minimal width/height cols/rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).table(width=3)(data), ex,
            Label(".table(height=3)(data) -- col data as minimal width/height cols/rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).table(height=3)(data)

          ) scaled 0.8f enlarged (50))
      }

      Page("Glyph Placement", "") {
        import CellFit._
        val data =
          for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
            Label(f"$i.scaled($scale%1.1f)").scaled(scale)

        def expanded(method: Method): Seq[Glyph] = {
          val lab = Label(s"fitToCell($method)").scaled(0.75f).cellFit(method)
          data.updated(4, lab)
        }

        Col.centered(
            Label("grid with data(4).fitToCell(...) [Enlarge/ShiftNorth/ShiftWest/ShiftSouth/ShiftEast/Stretch]"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(Enlarge)), ex, ex,
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftNorth)), ex, ex,
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftWest)), ex, ex,
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftSouthEast)), ex, ex,
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftEast)), ex, ex,
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(Stretch)), ex, ex,
          ) scaled 0.75f enlarged (50)
      }


      nested.Layout.topButtons()
    }

    Page("Blurred", "") {
      val bl = blue(width=0)
      def sr(blur: Scalar, spread: Scalar, delta: Scalar=1.8f): Glyph = {
        val l =  Glyphs.Label(f"(${blur.toInt}%02d, ${spread.toInt}%02d)", fg=white).enlarged(10).edged(white(width=1))
          BlurredFrame(blur, spread)(l).framed()
      }

      def sd(blur: Scalar, spread: Scalar): Glyph = {
        val l =
          Glyphs.Label(f"(${blur.toInt}%02d, ${spread.toInt}%02d)", fg=yellow).enlarged(10, fg=blue).edged(white(width=1))
          BlurredFrame(blur, spread, fg=blue, dx= -blur/2f, dy= -blur)(l).framed()
      }

      val b30  = Brush("black").color(0xFF000000).blurred(30f)
      val b15  = blue(width=0).blurred(15f)

      Col.centered(
        Paragraphs(60, Justify)(
          s"""
            | Painting filled material with a blurred brush leads to
            |the paint coverage being enlarged by the blur of the brush
            |
            |<<<<< b30=$b30
            |<<<<< FilledRect(150, 30, fg=b30).enlargedBy(30f, 30f).framed()
            |<<<<< FilledRect(150, 30, fg=b30).framed()
            |
            |<<<<<
            |<<<<< b15=$b15
            |<<<<< filledStargon(5, fg=b15, C=64f, R=60f).framed()
            |
            |""".stripMargin), ex,
        Row.centered(
          FilledRect(150, 30, fg=b30).enlargedBy(30f, 30f).framed(), em, em,
          FilledRect(150, 30, fg=b30).framed(), em, em,
          PolygonLibrary.filledStargon(5, fg=b15, C=64f, R=60f).framed()), ex, ex,

        Label("BlurredFrame(blur, spread)(...)"),
        NaturalSize.Grid(pady=10f).Table(width=4)(
        sr(10, 5),
        sr(10, 20),
        sr(10, 30),
        sr(10, 50),

        //sr(20, 5),
        //sr(20, 20),
        //sr(20, 30),
        //sr(20, 50),

        sr(30, 5),
        sr(30, 20),
        sr(30, 30),
        sr(30, 50),
      ).scaled(0.7f), ex, ex,
        Label("BlurredFrame(blur, spread, -blur/2f, -blur)(...)"),
        NaturalSize.Grid(pady=10f).Table(width=4)(
        sd(10, 10),
        sd(10, 20),
        sd(10, 30),
        sd(10, 50),

        //sd(20, 10),
        //sd(20, 20),
        //sd(20, 30),
        //sd(20, 50),

        sd(30, 10),
        sd(30, 20),
        sd(30, 30),
        sd(30, 50),
      ).scaled(0.7f)
      )
    }

    Page("Split", "") {
      import DynamicGlyphs.SplitScreen
      import ReactiveGlyphs.Slider
      val left = Paragraphs(30, Justify)(
        """
          |This is a justified piece of text that may be quite small.
          |You'll see it on a split screen. When the text on the other
          |screen is not the same width we'll see what happens.
          |""".stripMargin) above ReactiveGlyphs.TextButton("The Left Button") { _ => }.framed()
      val right = Paragraphs(40, Left)(
        """
          |This is a left-justified piece of text that may be quite small.
          |You'll see it on a split screen. It'll be a bit wider
          |than the other thing on the screen.
          |""".stripMargin) above ReactiveGlyphs.TextButton("The Right Button") { _ => }.framed()
      val dynamic = SplitScreen(left enlarged 30, right enlarged 30, dynamic=true, fg=darkGrey.strokeWidth(6f))
      def blob    = Glyphs.FilledRect(10f, 10f, fg=black.blurred(2f))
      val slider  = Slider.Horizontal(Glyphs.Rect(dynamic.w, 2f), blob, dynamic.proportion){
        case proportion: Scalar => dynamic.setBoundary(proportion)
      }
      val static = SplitScreen(left() enlarged 30, right() enlarged 30, dynamic=false, fg=darkGrey.strokeWidth(6f))
      Col.centered(
        Paragraphs(60, Justify)(
          """
            |This is a test of the SplitScreen glyph. The test shows a pair of glyphs side by
            |side, each of which contains some text and a reactive glyph. Here we have coupled
            |the SplitScreen dynamically with a
            |Slider.Horizontal that sets the boundary between the left and right
            |glyphs, accompanied by three buttons that respectively move the boundary to the left,
            |exchange left and right, and move the boundary to the right.
            |
            | Notice how the reactives respond when the cursor hovers over
            |parts of them that are not visible, namely by giving up the
            |focus if they happened to have it.
            |""".stripMargin), ex, ex,
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
        Paragraphs(60, Justify)(
          """
            |Below we test the SplitScreen with a static size large enough to accomodate both glyphs.
            |It was, incidentally, built from copies of the left and right glyphs that appear above;
            |so also acts as a test for deep-copying of all the glyphs involved in their construction.
            |""".stripMargin), ex,
        static,
        TextButton("L<->R") {
          _ => static.exchange()
        }
      ) enlarged 20f framed(blue)
    }

    Page("Scroll", "Scrolling and Scaling with ViewPort"){

      val image   = PolygonLibrary.PalestinianFlag scaled 0.5f
      val viewPort = DynamicGlyphs.ViewPort(image scaled 2f, fg=redFrame(width=10))

      def ScaleButton(scale: Scalar) = TextButton(f"*${scale}%1.1f") {
        _ => viewPort.scaleBy(scale)
      }.enlarged(15f, bg=white)

      val description =
        """ The image is framed in RED when the mouse is on or over it. Moving the mouse with its
          |PRIMARY button pressed "drags" the image.
          |
          |The mousewheel can also be used to scale or scroll it.
          |
          |<<<<< SHIFT+mousewheel scales.
          |
          |<<<<< Mousewheel scrolls vertically.
          |
          |<<<<< CONTROL+mousewheel scrolls horizontally.
          |
          |<<<<< CONTROL+SHIFT mouseclick resets scale and position, as does the
          |reset button.
          |
          |""".stripMargin

      val describe = Paragraphs(50*em.w, align=Left)(description)

      Col.centered(
        describe, ex,
        Row.centered$(List(1.2f, 0.75f, 0.5f).map(ScaleButton)), ex,
        viewPort, ex,
        TextButton("Reset") {
          _ => viewPort.reset()
        }
      ) enlarged 20f
    }

    Page("TextField", "") {
        import overlaydialogues.Dialogue.OK
        val tiny: StyleSheet = new Styles.DefaultSheet {
          override val labelFontSize = 20
        }
        val anchor = Label("")

        val theText = TextField(
          size = 40,
          onEnter = { s =>
            import windowdialogues.Dialogue
            OK(Label(s"You typed $s"))
              .South(anchor)
              .start()
          }
        )(tiny.labelStyle)

        Col.centered(
          Label("(6) TextField: a textlayout-entry field"),
          ex,
          ex,
          ex,
          Label("(the cursor is always kept in view)"),
          Label("(visual cues are given for off-field textlayout)"),
          Label(" "),
          Label("Ctrl/Cmd C - copy all"),
          Label("Ctrl/Cmd X - cut all to clipboard"),
          Label("Ctrl/Cmd V - insert from clipboard"),
          Label("Home/End/Left/Right/Backspace"),
          Label(" "),
          theText.framed(blue),
          anchor
        )
    }

    Page("OneOf", "OneOf backgrounds") {
      import DynamicGlyphs.OneOf

      val aaa = Label("AAA").copy(fg=blue,bg=yellow(width=2f)).framed(fg=yellow)
      val bbb = Label("BBB").copy(fg=blue, bg=nothing).scaled(2f)
      val ccc = Label("CCCCCC").copy(fg=blue, bg=red(width=2f)).scaled(2f).enlarged(10f)
      val ddd = Label("Ping").copy(bg=nothing, fg=black).scaled(1.5f).enlarged(10f)
      val eee = Label("Poobah is longer").copy(bg=green, fg=black).scaled(1.5f).enlarged(10f)
      val oneOf = OneOf()(aaa(), bbb(), ccc())
      val oneOfBG = OneOf(bg=black(alpha=0.25f))(aaa(), bbb(), ccc())
      val oneOfPB = OneOf()(ddd(), eee())

      val next = TextButton("Next State") {
        _ => oneOf.next(); oneOfBG.next(); oneOfPB.next()
      }

      val sel0 = TextButton(".select(0)") {
        _ => oneOf.select(0); oneOfBG.select(0); oneOfPB.select(0)
      }


      Col(fg=nothing, bg=white).centered(
        Paragraphs(align=Left, ems=60)(
          """
            |The background of a OneOf can be specified. If left unspecified it
            |is taken to be the background
            |of one of the glyphs of maximal area.
            |
            |Click on the buttons below to cycle through the
            |states of the OneOfs.
            |
            |""".stripMargin), ex,
        Row(fg=nothing, bg=white)(next), ex, ex,
        Col.centered(Label(s"""oneOf=OneOf(bg=grey)(AAA,BBB,CCCCCC)"""), ex, oneOfBG scaled .7f).enlarged(40).framed(), ex, ex,
        Col.centered(Label(s"""oneOf=OneOf()(AAA,BBB,CCCCCC)"""), ex, oneOf scaled .7f).enlarged(40).framed(), ex, ex,
        Col.centered(Label(s"""oneOf=OneOf()(Ping, Poobah)"""), ex, oneOfPB scaled .7f).enlarged(40).framed(), ex, ex, ex,
        Label("The OneOf component glyphs AAA, BBB, ... are:"), ex,
        Row(fg=nothing, bg=white)(aaa(), em, bbb(), em, ccc(), em, ddd(), em, eee()) scaled 0.8f

      ).enlarged(40)
    }

    Page("Sliders", "") {
      import ReactiveGlyphs.Slider

      import Slider.{Horizontal, Vertical}
      val trackh = Rect(500f, 55f, bg=yellow, fg=black)
      val trackv = Rect(15f, 250f, bg=yellow, fg=black)
      val imageh = FilledRect(15f, 35f, fg=red)
      val imagev = FilledRect(35f, 5f, fg=red)

      def reaction(proportion: Scalar): Unit = {
        for { sl<-slides } sl.dragTo(proportion)
        show.set(f"$proportion%1.3f (${sh.w*proportion}%3.2f, ${sv.h*proportion}%3.2f)")
      }

      lazy val sh:  Slider = Horizontal(trackh, imageh)(reaction)
      lazy val shu: Slider = Horizontal(trackh, imageh)(reaction)
      lazy val shr: Slider = Horizontal(trackh, imageh)(reaction)
      lazy val sv:  Slider = Vertical(trackv, imagev)(reaction)
      lazy val svu: Slider = Vertical(trackv() scaled 1.5f, imagev scaled 1.5f)(reaction)
      lazy val svr: Slider = Vertical(trackv(), imagev)(reaction)

      lazy val show = ActiveString(f"X.XXX: (XXX.XX, XXX.XX)")
      lazy val slides: Seq[Slider] = List(sh, shu, shr, sv, svu, svr)

      implicit class WithHint(g: Slider) {
        def hint(hint: String): Slider = {
          HintManager(g, 5, s"$g: $hint")
          g
        }
      }

      Col.centered(
        Paragraphs(50, Justify)(
          """
            |Several linked sliders subjected to a variety of
            |scalings, rotations, and skewings.
            |Hover over them for the details. Click
            |or slide or rotate the wheel to set.
            |""".stripMargin), ex,
        sh hint "",
        shu hint " turned 5" turned 5f framed (black),
        Row.centered(sv hint " scaled 1.5" scaled(1.5f) framed(black), em,
                     svu hint " rotated 2" rotated(2) framed(black), em,
                     svr hint " skewed (.2,0) turned 180" skewed(0.2f, 0f) turned(180f) framed(black), em,
                     shr hint " scaled 0.5f rotated 3" scaled 0.5f rotated 3),
        show.framed()
      )
    }

    Page("CheckBox", "Toggles, Checkboxes, ColourButtons") {
      import BooleanGlyphs._
      import DynamicGlyphs.OneOf

      def Monitor(whenFalse: String, whenTrue: String, toggle: OnOffButton): OneOf = {
        val oneOf = OneOf()(Label(whenFalse, align=Left), Label(whenTrue, align=Left))
        oneOf.select(if (toggle.get) 1 else 0)
        oneOf
      }

      val star  = PolygonLibrary.openStargon(7, C=64f, R=55f, fg=blue(width=2))
      val other = PolygonLibrary.filledRegularPolygon(7, C=64f, R=55f, fg=blue(width=2))

      lazy val state1 = Monitor(whenFalse="The toggle is off", whenTrue="The toggle is on", t1)
      lazy val state2 = Monitor(whenFalse="The tick is off", whenTrue="The tick is on", t2)
      lazy val state3 = Monitor(whenFalse="The red shape is showing", whenTrue="The blue shape is showing", t3)

      lazy val t1:OnOffButton = TextToggle(whenFalse="Turn the toggle On", whenTrue="Turn the toggle Off", initially=true) {
        case true => state1.select(1)
        case false => state1.select(0)
      }
      lazy val t2:OnOffButton = CheckBox(false) {
        case true  => state2.select(1)
        case false => state2.select(0)
      }

      lazy val t3:OnOffButton = GlyphToggle(
        whenFalse=other(fg=red),
        whenTrue=star(),
        initially=true)
      {
        case true  => state3.select(1)
        case false => state3.select(0)
      }

      val colourGlyphExample = {
        val bl = blue(width=10)
        val wh = white(width=10, cap=SQUARE)
        val gr = green(width=10)
        val re = red(width=10)
        def RectBut(background: Boolean): Glyph =
          ReactiveGlyphs.ColourButton(Rect(40, 40, fg=wh, bg=bl), gr, re, background){
          _ => println(s"Rect($background)")
        }

        def TextBut(background: Boolean): Glyph = {
          val caption = if (background) "Background" else "Foreground"
          ReactiveGlyphs.ColourButton(Glyphs.Label(s"$caption Changes", bg=bl, fg=wh), gr, re, background){
            _ => println(s"Text($background)")
          }
        }

        Col.centered(
          Paragraphs(50, Justify)(
            """Four ColourButtons. In the top row, the foreground colour changes
              |as the mouse hovers or is pressed. In the bottom row, the background colour changes
              |as the mouse hovers or is pressed.
              |""".stripMargin), ex,
          NaturalSize.Grid(bg=lightGrey, padx=20, pady=20).Table(height=2)(
            TextBut(false),  TextBut(true),
            RectBut(false),  RectBut(true)
          ).framed(),
        ).enlarged(20).framed()
      }



      locally {
        HintManager(enableSaveTextToggle, 5.0, "Toggle this to enable or disable the image-save bar")(HelpStyle)
        HintManager(t1, 5.0, "Guess what this is for!")(HelpStyle)
        HintManager(t3, 5.0, "Guess what this is for!")(HelpStyle)
      }

      Col.centered(
        NaturalSize.Grid(fg=blue(width=0), padx=10f).table(width=3)(List(
          state1, state2,            state3,
          t1,     t2 scaled 1.7f,    t3
        )).enlarged(40f).framed(blue), ex, ex,
        NaturalSize.Grid(fg=blue(width=0)).Width(2)(
          Col.centered(
          Paragraphs(40, Center)("A TextToggle can have multi-line legends in either or both states."), ex,
          TextToggle(whenTrue = ("True"), whenFalse = ("Not True\n(or true)"), initially = true) { _ => }, ex,
          enableSaveTextToggle
        ).enlarged(10f),
          Col.centered(
          Paragraphs(40, Center)("A GlyphToggle can have differently sized and shaped glyphs in each state"), ex,
          GlyphToggle(
            whenTrue = star(fg = blue)  scaled 1f,
            whenFalse = other(fg = red) scaled 1.2f,
            initially = true) { _ => },
          ex,
        ).enlarged(10f)
        ), ex, ex,
      ).above(colourGlyphExample).scaled(0.9f).enlarged(10f)
    }

    Page("Fonts*", "Font families\n(available on this computer)\n\n\n") {
        object FontFamilies {
          import GlyphTypes._
          lazy val names: Seq[String] =
            for {i <- 0 until FontManager.default.getFamiliesCount} yield FontManager.default.getFamilyName(i)
        }

      val familiesPerGroup = 60
      val noteBook = Notebook()
      val Page = noteBook.DefinePage
      var names = FontFamilies.names.sorted.toList

      def makePages(): Unit = {
        implicit val labelStyle: GlyphStyle= HelpStyle.labelStyle
        while (names.nonEmpty) {
          val group = names.take(familiesPerGroup)
          names = names.drop(familiesPerGroup)
          Page(s"${group.head.takeWhile(c => c!=' ')} ⇝ ${group.last.takeWhile(c => c!=' ')}", "") {
            //import styled.text.Label
            val labels = group.map { name => Label(name, Left) }
            val llength = labels.length / 2
            Row(NaturalSize.Col.atLeft$(labels.take(llength)).enlarged(20).framed(), em, em,
              NaturalSize.Col.atLeft$(labels.drop(llength)).enlarged(20).framed())
          }
        }
      }

      makePages()
      noteBook.Layout.rightButtons(uniform = true)
    }
    nested.Layout.rightButtons().enlarged(30)
  }

  /** Help dialogue for the menu-based interface */
  lazy val help: Glyph = TextButton("Help") {
    implicit val Style: StyleSheet = ApplicationStyle
    import windowdialogues.Dialogue
    import Location.RelativeTo
    _ =>
      Dialogue.OK(Paragraphs(40*ex.w, Left)(helpText)(HelpStyle), RelativeTo(help), "Help").start()
  }(ApplicationStyle)

  /////////////////////////////////////////// Debugging Tools

  object RectIO {
    import io.github.humbleui.types.IRect

    def ir(r: IRect): String = {
      f"${id(r.getWidth, r.getHeight)}%-12s@${id(r._left, r._top)}%-13s"
    }

    def id(x: Int, y: Int): String = s"($x, $y)"
  }

  def write(path: String)(glyph: Glyph): Unit = {
    val dir = "SAVED"
    External.renderBitmap(glyph, s"${dir}/$path", scale=0.5f)
  }

  def stringOfDate(date: LocalDateTime = LocalDateTime.now()) = {
    date.format(DateTimeFormatter.ofPattern("dd-MM-yyyy@HHmmss"))
  }

  def saveable(gui: Glyph): Glyph = {
    implicit val Style: StyleSheet = ApplicationStyle
    val r = FilledRect(gui.w-5, 6f, fg=lightGrey)
    lazy val topBar: RawButton = ReactiveGlyphs.RawButton(r(), r(), r()) {
      _ =>
        if (enableSave) {
          val fileName = stringOfDate() + ".png"
          windowdialogues.Dialogue.OKNO(Label(s"Save image to ${fileName}").enlarged(20f)).InFront(topBar).andThen {
            case false =>
            case true => write(fileName)(gui.guiRoot)
          }
        }
    }
    val HintStyle: StyleSheet = HelpStyle
    val saveHint = styled.text.Paragraphs(60, Justify)(
        """
          |When enabled, clicking on this grey strip enables the current GUI's
          |appearance to be saved in a .png file.
          |
          |It can be enabled/disabled with the checkbox on the Help page,
          |or with a button that appears on the Etc*/CheckBox page.
          |""".stripMargin) (HintStyle) scaled 1.5f enlarged 15 framed (fg=redFrame, bg=white)

    // Show the hint if the client doesn't already know what the fuss is about
    HintManager(topBar, 5.0, saveHint).onlyWhen(!enableSave)

    Col.centered(
      topBar,
      gui
    )
  }

  // We can do this with Popups or as a menu driven OneOf
  lazy val asMenu      = saveable(noteBook.Layout.menuBar)
  lazy val asRNotebook = saveable(noteBook.Layout.rightButtons())
  lazy val asLNotebook = saveable(noteBook.Layout.leftButtons())
  lazy val asVNotebook = saveable(noteBook.Layout.rotatedButtons(3))
  lazy val asSNotebook = saveable(noteBook.Layout.skewedButtons(0.2f, 0f, uniform = true))
  lazy val asTNotebook = saveable(noteBook.Layout.topButtons())
}

object DemonstrationNotebook extends DemonstrationPages with Application {
  import GlyphTypes.Window
  lazy val GUI =
            if (extraArgs contains "-notebook")   asRNotebook else
            if (extraArgs contains "-rnotebook")  asRNotebook else
            if (extraArgs contains "-lnotebook")  asLNotebook else
            if (extraArgs contains "-snotebook")  asSNotebook else
            if (extraArgs contains "-vnotebook")  asVNotebook else
            if (extraArgs contains "-tnotebook")  asTNotebook else
            if (extraArgs contains "-menu") asMenu else asRNotebook
  def title = s"""Demonstration Notebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""
  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  override
  def onClose(window: Window): Unit = confirmCloseOn(GUI)(window)
}



