package org.sufrin.glyph
package tests.GlyphBook

import styled.{Label, _}
import GlyphTypes.{FontStyle, Scalar}
import NaturalSize.{Col, Row}
import styles.decoration.{Edged, Framed, RoundFramed}
import unstyled.{reactive, static}
import unstyled.dynamic.SplitScreen
import unstyled.static._

import io.github.humbleui.skija.{PaintMode, PathFillMode}
import org.sufrin.glyph.GlyphShape.{arc, circle, rect, FILL, PathShape, STROKE}

import java.util.Date

class Etcetera(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: StyleSheet = style.pageSheet
  import pageSheet.{em, ex}
  import translation._
  val book = Book()
  val Page = book.Page
  import Brushes._





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
      val bbb = Label("BBB").copy(fg=blue, bg=transparent).scaled(2f)
      val ccc = Label("CCCCCC").copy(fg=blue, bg=red(width=2f)).scaled(2f).enlarged(10f)
      val ddd = Label("Ping").copy(bg=transparent, fg=black).scaled(1.5f).enlarged(10f)
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


      Col(fg=transparent, bg=white, align=Center)(
        Paragraph(60, Justify, Label("* "))(
          """
            |The background of a OneOf can be specified. If left unspecified it
            |is taken to be the background
            |of one of the glyphs of maximal area.
            |
            |Click on the buttons below to cycle through the
            |states of the OneOfs.
            |
            |""".stripMargin), ex,
        Row(fg=transparent, bg=white)(next, em, em, sel0), ex, ex,
        Col(align=Center)(Label(s"""oneOf=OneOf(bg=grey)(AAA,BBB,CCCCCC)"""), ex, oneOfBG scaled .7f).enlarged(40).edged(), ex, ex,
        Col(align=Center)(Label(s"""oneOf=OneOf()(AAA,BBB,CCCCCC)"""), ex, oneOf scaled .7f).enlarged(40).edged(), ex, ex,
        Col(align=Center)(Label(s"""oneOf=OneOf()(Ping, Poobah)"""), ex, oneOfPB scaled .7f).enlarged(40).edged(), ex, ex, ex,
        Label("The OneOf component glyphs AAA, BBB, ... are:"), ex,
        Row(bg=transparent, fg=transparent, align=Mid)(aaa(), em, bbb(), em, ccc(), em, ddd(), em, eee()) scaled 0.8f

      ).enlarged(40)
    }

    Page("Sliders", "") {
      import org.sufrin.glyph.unstyled.reactive.Slider
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
      import styled._
      import unstyled.BooleanGlyphs._
      import unstyled.dynamic.OneOf
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

        NaturalSize.Grid(width=1)(
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


    if (false) Page("Fonts*", "Font families\n(available on this computer)\n\n\n") {
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

    Page("Path"){
      val p0 = new Path(blue, yellow, strictContains = true) {
        moveTo(0, 0)
        lineTo(200, 0)
        lineTo(200, 200)
        closePath
        addCircle(100, 100, 40)
        addCircle(0, 0, 40)
        addCircle(200, 200, 40)
        addCircle(200, 0, 40)
        fillMode(PathFillMode.EVEN_ODD)
      }
      val p1 = new Path(blue, yellow, strictContains = true) {
        moveTo(0, 0)
        lineTo(200, 0)
        lineTo(200, 200)
        closePath
        addCircle(100, 100, 40)
        fillMode(PathFillMode.EVEN_ODD)
      }

      val p2=p1(green(width=16, mode=STROKE).sliced(20, 5))
      val p3=p1(red(width=16, mode=STROKE).dashed(20, 5))
      val pDiamond = new Path (blue, transparent, strictContains = true) {
        val Vec(hh, ww)  = p3.diagonal
        moveTo(ww/2, 0)
        lineTo(ww, hh/2)
        lineTo(ww/2, hh)
        lineTo(0, hh/2)
        closePath
        fillMode(PathFillMode.EVEN_ODD)
      }
      val b1 = new unstyled.reactive.RawButton(p1, p3, p2.rotated(2), fg=transparent, bg=transparent, { _=> }) {
        override val withDetailedShape: Boolean = true
      }
      val b2 = unstyled.reactive.ColourButton(pDiamond(blue.sliced(2,4)), red.sliced(2,15), green.sliced(2,10), false, NoHint){ _=>}
      Col(align=Center, skip=20)(
        <div width="80em" align="justify">
          <p>
             This example shows shapes and buttons constructed from two
              <b>Path</b> glyphs. The glyphs both have
              <tt>fillMode(PathFillMode.EVEN_ODD)</tt>, and <tt>strictContains = true</tt>.
            Notice that the buttons show their "hovered" state only when
            the mouse enters the blue "inside" of the showing glyph.
          </p>
        </div>, ex,
        Row( Label("A Path") above p0().framed(), em, em, em, Label("A simpler path") above p1().framed()), ex, ex,
        Label("A RawButton using three distinctly-brushed copies of the simpler path; one rotated."), ex, b1.framed(),
        ex, ex,
        Label("A ColourButton using three sliced brushes"), ex,(b2.framed())
        )
    }

    Page("Polygon", "") {

      def p1 = Polygon(Vec.Origin, blackFrame(width=40, cap=ROUND), transparent)((-100, -100), (-100, +100), (+100, +100), (+100, -100), (-100, -100))
      def p2 = FilledPolygon(200, 200, red(width=40, cap=ROUND, mode=STROKE), transparent)((0, 200), (200, 200), (200, 0), (0, 0), (0, 200))
      def p3 =  Concentric.Center(p1, p2)
      val explain = <div width="80em" align="justify">
        <p>Connected lines specified by vertices, with diagonal specified by <tt>box</tt>, unless
           box is <tt>Vec.Zero</tt>. In that case the diagonal is calculated ``naturally'' from the positions the vertices normalized (to the
           positive quadrant), with allowance made for the strokewidth of <tt>fg</tt>, so that the entire drawn polygon
          is within the diagonal. In the former case the drawn polygon may be clipped by the declared size of <tt>box.</tt>
        </p>
        <p>
          The example below shows:
          </p>
          <![CDATA[
                  Polygon(Vec.Zero, blackFrame(width=40, cap=ROUND), transparent)
                          ((-100, -100), (-100, +100),
                           (+100, +100), (+100, -100), (-100, -100))]]>
          <p>above:</p>
          <![CDATA[
                  Polygon(200, 200, red(width=40, cap=ROUND), transparent)
                          ((0, 200), (200, 200),
                           (200, 0), (0, 0), (0, 200))]]>
        <p>and then concentric, with it. Notice the abovementioned clipping of the latter.</p>
      </div>

      NaturalSize.Col(align=Center)(explain, pageSheet.vSpace(2), p1, pageSheet.vSpace(), p2, pageSheet.vSpace(), p3, pageSheet.vSpace())

    }

    if (false) Page("TextShape", "") {
      import textshape._
      val font = FontFamily("Courier").makeFont(FontStyle.NORMAL, 32.0f)
      val text = new TextShape(font, 100, Brushes.black, Brushes.yellow)
      val emWidth = pageSheet.emWidth
      text.addText(
        """Hitherto we have asked not what we could do for our
          |country, but what our country could do for us. At
          |this point, however, we beg to differ from our previous practice.
          |
          |The astounding matter is just that this is working
          |at all.
          |
          |""".stripMargin)
      val glyph = text.toGlyph(40*emWidth)
      Col(align=Center)(
        glyph.scaled(0.8f).edged()
      )
    }

    Page("GlyphShape", "") {
      import GlyphShape._
      var cr = rect(30, 30)(Brushes.redLine(width=15, mode=PaintMode.STROKE))
      var cb = circle(30)(Brushes.blue(width=15, mode=PaintMode.STROKE))
      var cg = circle(60)(Brushes.green(width=20, mode=PaintMode.STROKE))
      def poly1 = polygon((-50, -50), (-50, +50), (+50, +50), (+50, -50), (-50, -50))
      def poly2 = polygon((0,0), (50, 50), (50, 0), (0, 50))
      def poly3 = polygon((150, 0), (150,150), (100, 150), (150,0))
      val redStroke = red(width=20, cap=ROUND, mode=PaintMode.STROKE)
      val redFill   = red(width=20, cap=ROUND, mode=PaintMode.FILL)
      val yellowFill = yellow(mode=FILL)
      val greenFill = green(mode=FILL)
      val em = pageSheet.em


      val shapes: List[GlyphShape] =
        List(
          arrow(red)~~~circle(60)(greenFill), arrow(red(width=3,mode=STROKE)), arrow(blue)~~~circle(60)(greenFill), arrow(blue(mode=STROKE))~~~rect(120,120)(greenFill),
        cr~~~circle(60)(greenFill),           cb,                     cg,                                   cg.withBackground(yellowFill),
        (cr ~~~ cb),  (cr~~~cb).withBackground(yellowFill),   (cr~~~cb.withBackground(yellowFill)),                 (cr.withBackground(yellowFill)~~~cb),
        (cr --- cg),  (cr --- cg).withBackground(yellowFill), (cr---cb.withBackground(yellowFill)),                 (cr.withBackground(yellowFill)---cb),
        (cr---cb.withBackground(yellowFill)).withBackground(greenFill).turn(45), (cr---cb.withBackground(yellowFill)).withBackground(greenFill).turn(45).withBackground(red),
           (cr---cb.withBackground(yellowFill)).withBackground(greenFill),       (cr.withBackground(yellowFill)---cb).withBackground(greenFill),
        poly3(redStroke), poly3(redStroke).withBackground(yellowFill), poly3(redStroke).withBackground(yellowFill).withBackground(blue), poly3(redStroke).withBackground(yellowFill).withBackground(greenFill),
        poly1(blue(width=25, cap=SQUARE, mode=PaintMode.STROKE)),
        poly2(blue(width=5, cap=ROUND, mode=PaintMode.STROKE)),
        poly2(blue(width=5, cap=ROUND, mode=PaintMode.STROKE)).withBackground(greenFill), poly2(redFill).turn(45f, true).withBackground(yellowFill),
        )

      Col(align=Center)(
          <p width="70em" align="justify">
            A cursory test of <b>GlyphShape</b>: a precursor type to <b>Glyph</b>. Shapes have a <b>draw(Surface)</b> method, and a bounding box <b>diagonal:Vec.</b>
            They can be composed laterally <span><b>(left|||right)</b></span> and vertically <b>(top---bottom)</b>, and superimposed <b>(l~~~r)</b>. In the latter case, the shape
            with the smaller area (<b>l</b> when they have the same area) is drawn centred on the other shape.
          </p>, ex,
        NaturalSize.Grid(fg=black, padx=10, pady=10).table(width=4)(shapes.map(_.asGlyph)), ex, ( <p width="70em" align="justify">
          Shapes also have intrinsic methods <b>scale(factor:Scalar),</b> and <b>turn(degrees:Scalar)</b> with much the same
          interpretation as those present in ordinary glyphs. The method <b>bg(Brush): GlyphShape</b> constructs a new
          shape <i>as far as possible like the original shape</i> with the given brush as its background colour. Shapes derived by
          intrinsics are treated as rectangles for the purposes of <b>bg</b> and this
          can yield an interesting colour scheme (as seen on the last three rows).
        </p>)
      )
    }

    Page ("Sound", "Buttons bound to sounds"){
      import java.io.File
      import Sound._
      def filesInDir(path: String): Seq[File] = {
        val dir = new File(path)
        if (dir.exists && dir.isDirectory) {
          dir.listFiles.toSeq.filter(_.isFile).sortWith { (f1, f2) =>f1.getName.toLowerCase<f2.getName.toLowerCase }
        } else {
          Seq.empty
        }
      }
      val clips = filesInDir("WAV").map(Clip) ++ List(Clip(new File("BOGUS")))
      val buttons = clips.map { clip => styled.TextButton(clip.file.toString) { _ => clip.play() }(pageSheet.copy(buttonDecoration = RoundFramed(fg=black(width=3),radius=0.25f))) }
      NaturalSize.Col(align=Left)(buttons)
    }




  val GUI: Glyph = book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign = Center).enlarged(30)

}
