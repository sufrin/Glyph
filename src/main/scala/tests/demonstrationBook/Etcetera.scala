package org.sufrin.glyph
package tests.demonstrationBook

import styled.TextButton
import styled.Label
import styled.windowdialogues.Dialogue
import styled.Book
import styled.BookSheet
import styled.TextToggle
import styled.Paragraph
import NaturalSize.{Col, Grid, Row}
import GlyphTypes.Scalar

import unstyled.{static, reactive}
import static._
import unstyled.dynamic.SplitScreen

import styles.decoration.{Edged, Framed}

import java.util.Date

class Etcetera(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: StyleSheet = style.pageSheet
  import translation._
  import pageSheet.{ex, em}
  val book = Book()
  val Page = book.Page
  import Brushes._


    Page("Animation", "") {
      import unstyled.dynamic.{Periodic, Transform, Transformable}
      val shape = static.Concentric(rowAlign=Mid, colAlign=Center)(
        FilledOval(40, 40, fg=blue),
        FilledRect(30, 10, fg=red) beside FilledRect(10, 10, fg=green))

      var lastDriver: List[Periodic[Int]] = Nil

      class Animation() {
        lazy val button = reactive.ColourButton(shape, green, red, background = true, NoHint) {
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

      Col(align=Center)(

        Paragraph(50, Justify)(
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
      val nested = Book()
      val Page = nested.Page

      Page("Cell size", "data = 9 variable sized labels. Grid(... padx=10, pady=10)") {
        val data =
          for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
            Label(f"$i.scaled($scale%1.1f)").scaled(scale)

        Col(align=Center)(
          Col(align=Left)(
            Label(".grid(width=3)(data) -- row data as uniform size cells"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(data), ex,
            Label(".grid(height=3)(data) -- col data as uniform size cells"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(height = 3)(data), ex, ex, ex,
            Label(".rows(width=3)(data) -- row data as uniform width rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, width=3).rows(data), ex,
            Label(".cols(height=3)(data) -- col data as uniform height rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, height = 3).cols(data), ex, ex, ex,
            Label(".table(width=3)(data) -- row data as minimal width/height cols/rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).table(width=3)(data), ex,
            Label(".table(height=3)(data) -- col data as minimal width/height cols/rows"),
            NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).table(height=3)(data)

          ) scaled 0.8f enlarged (50))
      }

      Page("Glyph pos'n", "") {
        import CellFit._
        val data =
          for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
            Label(f"$i.scaled($scale%1.1f)").scaled(scale)

        def expanded(method: Method): Seq[Glyph] = {
          val lab = Label(s"fitToCell($method)").scaled(0.75f).cellFit(method)
          data.updated(4, lab)
        }

        Col(align=Center)(
          Label("grid with data(4).fitToCell(...) [Enlarge/ShiftNorth/ShiftWest/ShiftSouth/ShiftEast/Stretch]"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(Enlarge)), ex, ex,
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftNorth)), ex, ex,
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftWest)), ex, ex,
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftSouthEast)), ex, ex,
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftEast)), ex, ex,
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(Stretch)), ex, ex,
        ) scaled 0.75f enlarged (50)
      }

      nested.Layout.leftCheckBoxes(buttonAlign=Right, pageAlign=Center)
    }


    Page("Scroll", "Scrolling and Scaling with ViewPort"){

      val image    = PolygonLibrary.PalestinianFlag scaled 0.5f
      val viewPort = unstyled.dynamic.ViewPort(image scaled 2f, fg=redFrame(width=10)).enableDrag(true).enableScale(true)

      def ScaleButton(scale: Scalar) = TextButton(f"*${scale}%1.1f") {
        _ => viewPort.scaleBy(scale)
      }.enlarged(15f, bg=white)

      val describe: Glyph = <div width="50em" >
        <p align="justify">
          The image is edged in RED when the mouse is on or over the viewport. Moving the mouse with its
          primary button pressed drags the image. The <b>UP, DOWN, LEFT, RIGHT, END, PGUP,</b> and <b>PGDOWN</b> buttons move
          the viewport an appropriate amount in the appropriate direction; the <b>HOME</b> button resets scale and position.
        </p>
        <p hang="* ">The mousewheel alone scrolls vertically, scrolls horizontally with <b>CTRL</b> pressed, and scales with <b>SHIFT</b> pressed.
        </p>
        <p hang="* ">Clicking the primary mouse button with <b>CTRL+SHIFT</b> pressed resets scale and position, as does the <b>Reset</b> button</p>
      </div>

      Col(align=Center)(
        describe, ex,
        Row(align=Mid)(List(1.2f, 0.75f, 0.5f).map(ScaleButton)), ex,
        viewPort, ex,
        TextButton("Reset") {
          _ => viewPort.reset()
        }
      ) enlarged 20f
    }

    if (false) Page("TextField", "") {
      val anchor = Label("")

      val theText = styled.TextField(
        size = 40,
        onEnter = { s =>
          import styled.windowdialogues.Dialogue.OK
          OK(Label(s"You typed $s"))
            .South(anchor)
            .start()
        }
      )

      Col(align=Center)(
        Label("TextField: a text entry field"),
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
        theText.edged(blue),
        anchor
      )
    }

    Page("OneOf", "OneOf backgrounds") {
      import unstyled.dynamic.OneOf

      val aaa = Label("AAA").copy(fg=blue).enlarged(90, bg=yellow)
      val bbb = Label("BBB").copy(fg=blue, bg=nothing).scaled(2f)
      val ccc = Label("CCCCCC").copy(fg=blue, bg=red(width=2f)).scaled(2f).enlarged(10f)
      val ddd = Label("Ping").copy(bg=nothing, fg=black).scaled(1.5f).enlarged(10f)
      val eee = Label("Poobah is longer").copy(bg=green, fg=black).scaled(1.5f).enlarged(10f)

      val oneOf   = OneOf()(aaa(), bbb(), ccc())
      val oneOfBG = OneOf(bg=lightGrey)(aaa(), bbb(), ccc())
      val oneOfPB = OneOf()(ddd(), eee())

      val buttonSheet: StyleSheet = pageSheet.copy(buttonDecoration=styles.decoration.Edged(fg=lightGrey.copy(width=16, cap=ROUND)))

      val next = TextButton("Next State") {
        _ => oneOf.next(); oneOfBG.next(); oneOfPB.next()
      }(buttonSheet)

      val sel0 = TextButton(".select(0)") {
        _ => oneOf.select(0); oneOfBG.select(0); oneOfPB.select(0)
      }(buttonSheet)


      Col(fg=nothing, bg=white, align=Center)(
        Paragraph(60, Left)(
          """
            |The background of a OneOf can be specified. If left unspecified it
            |is taken to be the background
            |of one of the glyphs of maximal area.
            |
            |Click on the buttons below to cycle through the
            |states of the OneOfs.
            |
            |""".stripMargin), ex,
        Row(fg=nothing, bg=white)(next, em, em, sel0), ex, ex,
        Col(align=Center)(Label(s"""oneOf=OneOf(bg=grey)(AAA,BBB,CCCCCC)"""), ex, oneOfBG scaled .7f).enlarged(40).edged(), ex, ex,
        Col(align=Center)(Label(s"""oneOf=OneOf()(AAA,BBB,CCCCCC)"""), ex, oneOf scaled .7f).enlarged(40).edged(), ex, ex,
        Col(align=Center)(Label(s"""oneOf=OneOf()(Ping, Poobah)"""), ex, oneOfPB scaled .7f).enlarged(40).edged(), ex, ex, ex,
        Label("The OneOf component glyphs AAA, BBB, ... are:"), ex,
        Row(bg=nothing, fg=nothing, align=Mid)(aaa(), em, bbb(), em, ccc(), em, ddd(), em, eee()) scaled 0.8f

      ).enlarged(40)
    }

    Page("Sliders", "") {
      import org.sufrin.glyph.unstyled.reactive.Slider

      import Slider.{Horizontal, Vertical}
      val trackh = Rect(500f, 55f, bg=yellowHuge, fg=black)
      val trackv = Rect(15f, 250f, bg=yellowHuge, fg=black)
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

      lazy val show = styled.ActiveString(initial=f"X.XXX: (XXX.XX, XXX.XX)")
      lazy val slides: Seq[Slider] = List(sh, shu, shr, sv, svu, svr)

      implicit class WithHint(g: Slider) {
        def hint(hint: String): Slider = {
          HintManager(g, 5, ()=>s"$g: $hint", constant = true)
          g
        }
      }

      Col(align=Center)(
        Paragraph(50, Justify)(
          """
            |Several linked sliders subjected to a variety of

            |scalings, rotations, and skewings.
            |Hover over them for the details. Click
            |or slide or rotate the wheel to set.
            |""".stripMargin), ex,
        sh hint "",
        shu hint " turned 5" turned 5f edged (black),
        Row(align=Mid)(sv hint " scaled 1.5" scaled(1.5f) edged(black), em,
          svu hint " rotated 2" rotated(2) edged(black), em,
          svr hint " skewed (.2,0) turned 180" skewed(0.2f, 0f) turned(180f) edged(black), em,
          shr hint " scaled 0.5f rotated 3" scaled 0.5f rotated 3),
        show.edged()
      )
    }

    Page("CheckBox", "Toggles, Checkboxes, ColourButtons") {
      import unstyled.BooleanGlyphs._
      import unstyled.dynamic.OneOf
      import styled._
      implicit val pageSheet=style.pageSheet.copy(buttonDecoration = Edged(fg=blue(width=6, cap=ROUND)))


      /**
       *    A `OneOf` showing one of two messages: initially indicated by the state of the toggle`.
       *    The associated toggle must invoke `select` on the monitor when its state changes.
       */
      def Monitor(whenFalse: String, whenTrue: String, toggle: OnOffButton): OneOf = {
        val oneOf = OneOf()(Label(whenFalse), Label(whenTrue))
        oneOf.select(if (toggle.get) 1 else 0)
        oneOf
      }

      var t2Hit = new Date().toString

      val star  = PolygonLibrary.openStargon(7, C=64f, R=55f, fg=red(width=2))
      val other = PolygonLibrary.filledRegularPolygon(6, C=64f, R=55f, fg=blue(width=2))


      lazy val state1 = Monitor(whenFalse="The toggle is off", whenTrue="The toggle is on", t1)
      lazy val t1:OnOffButton = TextToggle(whenFalse="Turn the toggle On", whenTrue="Turn the toggle Off", initially=true) {
        case true => state1.select(1)
        case false => state1.select(0)
      }

      lazy val state2 = Monitor(whenFalse="The checkbox shows false", whenTrue="The checkbox shows true", t2)
      lazy val t2:OnOffButton = CheckBox(false, Hint(3, s"Last clicked: $t2Hit", false)) {
        case true  => t2Hit = new Date().toString; state2.select(1)
        case false => t2Hit = new Date().toString; state2.select(0)
      }

      lazy val state3 = Monitor(whenFalse="The hexagon is showing", whenTrue="The star is showing", t3)
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
          reactive.ColourButton(Rect(40, 40, fg=wh, bg=bl), gr, re, background, Hint(5, if (background) "Background changes" else "Foreground changes")){
            _ => println(s"Rect($background)")
          }

        def TextBut(background: Boolean): Glyph = {
          val caption = if (background) "Background" else "Foreground"
          reactive.ColourButton(static.Label(s"$caption Changes", bg=bl, fg=wh), gr, re, background, NoHint){
            _ => println(s"Text($background)")
          }
        }

        Col(align=Center)(
          Paragraph(50, Justify)(
            """Four ColourButtons. In the top row, the foreground colour changes
              |as the mouse hovers or is pressed. In the bottom row, the background colour changes
              |as the mouse hovers or is pressed. The square buttons are inside out: the foreground
              |is the white square with wide edges; the background is blue; seen through the foreground.
              |""".stripMargin), ex,
          NaturalSize.Grid(bg=lightGrey, padx=20, pady=20, height=2)(
            TextBut(false),  TextBut(true),
            RectBut(false),  RectBut(true)
          ).edged(),
        ).enlarged(20).edged()
      }



      locally {
        HintManager(t1, 5.0, ()=>"Guess what this is for!")
        HintManager(t3, 5.0, ()=>"Guess what this is for!")
      }

      Col(align=Center)(
        <p width="60em" parSkip="2ex">The checkbox in the middle demonstrates dynamic hinting:
           here the hint shows the last time at which the checkbox was
           clicked.</p>,
        NaturalSize.Grid(fg=black(width=0), padx=10f).table(width=3)(List(
          state1, state2,            state3,
          t1,     t2 scaled 1.7f,    t3.enlarged(15)
        )).enlarged(40f).edged(black), ex, ex,

        NaturalSize.Grid.Width(1)(
          Col(align=Center)(
            Label("A TextToggle can have multi-line legends in either or both states."), ex,
            TextToggle(whenTrue = ("True"), whenFalse = ("Not True\n(or true)"), initially = true) { _ => }, ex,
          ).enlarged(10f),
          Col(align=Center)(
            Label("A GlyphToggle can have differently sized and shaped glyphs in each state"), ex,
            GlyphToggle(
              whenTrue =  star(fg = blue)  scaled 1f,
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
      val book = Book()
      val Page = book.Page
      var names = FontFamilies.names.sorted.toList

      def makePages(): Unit = {
        while (names.nonEmpty) {
          val group = names.take(familiesPerGroup)
          names = names.drop(familiesPerGroup)
          Page(s"${group.head.takeWhile(c => c!=' ')} ⇝ ${group.last.takeWhile(c => c!=' ')}", "") {
            //import styled.text.Label
            val labels = group.map { name => styled.Label(name, Left) }
            val llength = labels.length / 2
            Row(NaturalSize.Col(align=Left)(labels.take(llength)).enlarged(20).edged(), em, em,
              NaturalSize.Col(align=Left)(labels.drop(llength)).enlarged(20).edged())
          }
        }
      }

      makePages()
      book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign=Center)
    }

    Page("Split", "") {
    import org.sufrin.glyph.unstyled.reactive.Slider
    import pageSheet.ex
    implicit val pageSheet : StyleSheet = style.pageSheet.copy(buttonDecoration = Framed(Brushes.black(width=2)))
    val left =
      Row(<p width="25em" align="justify">
        This is a justified piece of text that may be quite long, and which
        you'll see on a split screen. The text is pointless in and of itself; but
        will be useful for de_bugging the Split_Screen active glyph.
      </p>)  above styled.TextButton("The Left Button") { _ => println("LEFT") }
    val right =
      Row(<p width="40em" align="left">
        This is a filled piece of text that may be quite short, and which
        you will see on a split screen. It'll be a bit wider
        than the other thing on the screen.
      </p>) above styled.TextButton("The Right Button") { _ =>  println("RIGHT") }

    val dynamic = SplitScreen(left enlarged 30, right enlarged 30, dynamic=true, fg=darkGrey.strokeWidth(6f))
    def blob    = FilledRect(28f, 14f, fg=black.blurred(6f))
    val slider  = Slider.Horizontal(Rect(dynamic.w, 2f), blob, dynamic.proportion){
      case proportion: Scalar => dynamic.setBoundary(proportion)
    }

    val static = SplitScreen(left() enlarged 30, right() enlarged 30, dynamic=false, fg=darkGrey.strokeWidth(6f))

    Col(align=Center)(
      <div width="65em" align="justify">
        <p>
          This is a test of the SplitScreen glyph. The test shows a pair of glyphs side by
          side, each of which contains some text and a reactive glyph. Here we have coupled
          the SplitScreen dynamically with a
          Slider.Horizontal that sets the boundary between the left and right
          glyphs, accompanied by three buttons that respectively move the boundary to the left,
          exchange left and right, and move the boundary to the right.
        </p>
        <p>
          Reactives <b>should</b> respond by giving up the
          focus (if they happened to have it) when the cursor hovers over
          parts of them that are not visible.
        </p>
      </div>, ex,
      dynamic,
      slider,
      TextButton("Divider to left"){
        _ => dynamic.setBoundary(0.0f); slider.dragTo(0f)
      } beside
        TextButton("Swap left and right") {
          _ => dynamic.exchange()
        } beside
        TextButton("Divider to right"){
          _ => dynamic.setBoundary(1.0f); slider.dragTo(0.999f)
        },
      ex, ex, ex,
      Paragraph(60, Justify)(
        """
          |Below we test the SplitScreen with a static size large enough to accomodate both glyphs.
          |It was, incidentally, built from copies of the left and right glyphs that appear above;
          |so also acts as a test for deep-copying of all the glyphs involved in their construction.
          |""".stripMargin), ex,
      static,
      TextButton("Swap left and right") {
        _ => static.exchange()
      }
    ) enlarged 20f
  }


  val GUI: Glyph = book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign = Center).enlarged(30)

}
