package org.sufrin.glyph
package tests.GlyphBook

import styled.{Book, BookSheet, Label}
import GlyphTypes.Scalar
import NaturalSize.{Col, Grid, Row}
import unstyled.static.{INVISIBLE, Polygon}
import Shape.FILL
import styled.windowdialogues.Dialogue
import unstyled.dynamic.OneOf

import io.github.humbleui.skija.{PaintMode, PaintStrokeCap}

class Framing(implicit val style: BookSheet, implicit val translator: glyphML.Translator)  {
  implicit val pageSheet: StyleSheet = style.buttonSheet.copy(fontScale = 0.8f)
  import pageSheet.{em, ex}
  val language = translator(pageSheet)
  import language._
  val book = Book()
  val Page = book.Page
  import Brushes._

  Page("Framed Text #1", "Texts .framed(fg, yellow, rf)\n(showing effect of fg brush size/cap") {
      val fg = red(width = 10, cap = ROUND).copy()

      def short = Label(" short ")(pageSheet.copy(labelBackgroundBrush = yellow))

      def med = Label(" A medium length label ")(pageSheet.copy(labelBackgroundBrush = yellow))

      def long: Glyph = <p backgroundBrush="yellow" textBackgroundBrush="yellow" align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>


      def row(rf: Scalar, cap:PaintStrokeCap = ROUND): Glyph = {
        val FG = fg(width=rf, cap=cap)
        Col(align=Center)(
          Label(s".framed($FG)\n").scaled(0.9f),
          Row(align=Mid, skip=10)(
            short.framed(FG, bg=yellow),
            med.framed(FG, bg=yellow),
            long.enlarged(20).framed(FG, bg=yellow)), ex)
      }


      Col(align=Center)(
        Row(Col(align=Center)(List(2f, 5f, 10f, 20, 35f).map(row(_, ROUND))),
            Col(align=Center)(List(2f, 5f, 10f, 20f, 35f).map(row(_, SQUARE))))
      ).enlarged(40)
    }

  Page("RoundFramed Text #1", "Texts .roundFramed(fg, yellow, rf)\n(smaller rf => tighter corners)") {
    val fg = red(width = 10, cap = ROUND, mode=FILL).copy()

    def short = Label(" short ")

    def med = Label(" A medium length label ")

    def long: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)
    def longer: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)

    def row(rf: Scalar, fg: Brush= red(width = 10, cap = ROUND)): Glyph = {
      Col(align=Center)(
        Label(s"rf=$rf, $fg\n").scaled(0.9f),
        Row(align=Mid)(
          short.roundFramed(fg, yellow, rf), em,
          med.roundFramed(fg, yellow, rf), em,
          long.roundFramed(fg, yellow, rf)), ex)
    }


    Col(align=Center)(
      Grid(padx=20, pady=10, height=4)(
        (List(3f, .25f, .125f, 0f).map(row(_))) ++
          (List(3f, .25f, .125f, 0f).map(rf => row(rf, red(width=20, cap=ROUND))))
      )
    ).enlarged(40)
  }

  Page("Framed Text #2") {
    val fg = red(width = 10, cap = ROUND).copy()

    def short = Label(" short ")(pageSheet.copy(labelBackgroundBrush = yellow)).rotated(1)

    def med = Label(" A medium length label ")(pageSheet.copy(labelBackgroundBrush = yellow)).rotated(1)

    def long: Glyph = <p backgroundBrush="yellow" textBackgroundBrush="yellow" align="justify" width="15em">A text par_agraph that may ob_scure a low curv_ature frame..</p>


    def row(rf: Scalar, cap: PaintStrokeCap = ROUND): Glyph = {
      val FG = fg(width=rf, cap=cap)
      Col(align=Center)(
        Label(s".edged($FG)\n").scaled(0.9f),
        Row(align=Mid, skip=10)(
          short.edged(FG, bg=yellow),
          med.edged(FG, bg=yellow),
          long.enlarged(20).edged(FG, bg=yellow)), ex)
    }


    Col(align=Center)(
      Row(Col(align=Center)(List(2f, 5f, 10f, 20f).map(row(_, ROUND))),
        Col(align=Center)(List(2f, 5f, 10f, 20f).map(row(_, SQUARE))))
    ).enlarged(40)
  }

  Page("RoundFramed Text #2", "Texts .roundFramed(fg, yellow, rf)") {
    val fg = red(width = 10, cap = ROUND).copy()

    def short = Label(" short ").rotated(1)

    def med = Label(" A medium length label ").rotated(1)

    def long: Glyph = <p align="justify" width="15em">A text par_agraph that may ob_scure a low curv_ature frame.</p>.enlarged(25)

    def row(rf: Scalar, fg: Brush= red(width = 10, cap = ROUND)): Glyph = {
      Col(align=Center)(
        Label(s"rf=$rf, $fg)\n").scaled(0.9f),
        Row(align=Mid)(
          short.roundFramed(fg, yellow, rf), em,
          med.roundFramed(fg, yellow, rf), em,
          long.roundFramed(fg, yellow, rf)), ex)
    }


    Col(align=Center)(
      Grid(padx=20, pady=10, height=4)(
        (List(3f, .25f, .125f, 0f).map(row(_))) ++
          (List(3f, .25f, .125f, 0f).map(rf => row(rf, red(width=20, cap=SQUARE))))
      )
    ).enlarged(40)
  }

  Page("Framed #3", "Text.copy(bg=transparent).framed(fg, bg)\n(text bg prevails over edged bg unless transparent)") {
    def label(bg: Brush) = SimpleParagraphs(15, align=Center, font=FontFamily("Courier")(25), fg=darkGrey, bg=bg)("""The rain in spain falls mainly in the plain""")

    Grid(padx=10, pady=10, fg=transparent, width=4).rows(
      for {cap<-List(ROUND); tbg<-List("yellow", "transparent"); bg<-List("green", "transparent"); width<-List(2f, 4f, 10f, 15f)} yield
        Label(s"fg=red($width,$cap)\ntext bg=$tbg, bg=$bg") above label(Brushes(tbg)).framed(fg=red(width = width, cap = cap), bg=Brushes(bg)) ,
    )
  }

  if (false) Page("Sampler", "") {
    import styled.TextButton
    import styles.decoration.{Blurred, Decoration, Shaded}
    val styleSheet = pageSheet
    val anchor = INVISIBLE()

    lazy val fg: Brush = Brush("darkGrey.16.square")
    lazy val bg: Brush = Brush("lightGrey")
    lazy val buttonBG: Brush = Brush("transparent")
    lazy val buttonFG: Brush = Brush("white")
    var enlarge: Scalar = 5f
    var radius: Scalar = 0.8f
    var blur: Scalar = 10f
    var spread: Scalar = 12f
    var delta: Scalar = 12f
    var belta: Scalar = 0f

    import styled.windowdialogues.Dialogue.FLASH


    def exemplar(id: String, caption: () => String, decor: () => Decoration): Glyph = {
      lazy val button: Glyph = TextButton(id)
      { _ =>
        val style = styleSheet.copy(buttonForegroundBrush=buttonFG, buttonBackgroundBrush=buttonBG, buttonDecoration = decor())
        lazy val dialogue: Dialogue[Unit] =
          FLASH(
            Col(align=Center)(
              styled.Label(id)(style),
              styled.Label(caption().replace(',', '\n'))
            )
          )
        dialogue.InFront(anchor).start()
      }(styleSheet.copy(buttonBackgroundBrush=buttonBG, buttonDecoration = decor()))//(styleSheet.copy(buttonDecoration = Framed(black(width=4), enlarge=20)))
      button
    }


    def rectangle(w: Scalar, h: Scalar)(brush: Brush): Shape = {
      val path=new PathShape(brush, false)
      path.moveTo(0, 0)
      path.lineTo(w, 0)
      path.lineTo(w, h)
      path.lineTo(0, h)
      path.closePath
      path
    }

    def Framed(fg: Brush, bg: Brush, enlarge: Scalar): Decoration = new Decoration {
      import Shape._
      val ffg=fg mode PaintMode.STROKE
      val fbg=bg mode PaintMode.FILL
      def decorate(glyph: Glyph): Glyph = {
        val fenlarge = if (enlarge<1) (glyph.w min glyph.h)*enlarge else enlarge
        val frame = rect(glyph.w+2*fg.strokeWidth+fenlarge, glyph.h+2*fg.strokeWidth+fenlarge)(ffg)
        val background = rect(glyph.w+2*fg.strokeWidth+fenlarge, glyph.h+2*fg.strokeWidth+fenlarge)(fbg)
        (superimposed(List(background, frame, glyph)).asGlyph)
      }
    }

    def RoundFramed(fg: Brush, bg: Brush, enlarge: Scalar, radius: Scalar): Decoration = new Decoration {
      import Shape._
      val rad = radius max 0.1f
      val ffg=fg(mode=PaintMode.STROKE).rounded(radius)
      val fbg=bg(mode=PaintMode.FILL).rounded(radius)
      def decorate(glyph: Glyph): Glyph = {
        val fenlarge = if (enlarge<1) (glyph.w min glyph.h)*enlarge else enlarge
        val frame = rectangle(glyph.w+2*fg.strokeWidth+enlarge, glyph.h+2*fg.strokeWidth+enlarge)(ffg)
        val background = rectangle(glyph.w+2*fg.strokeWidth+enlarge, glyph.h+2*fg.strokeWidth+enlarge)(fbg)
        (superimposed(List(background, frame, glyph)).asGlyph)
      }
    }


    lazy val roundframed: Glyph =
      exemplar("RoundFramed(fg, bg, enlarge, radius)",
        ()=>s"$fg, $bg, $enlarge, $radius, buttonfg=$buttonFG, buttonbg=$buttonBG",
        ()=>RoundFramed(fg, bg, enlarge, radius))

    lazy val  framed: Glyph =
      exemplar("Framed(fg, bg, enlarge)",
               ()=>s"$fg, $bg, $enlarge, buttonfg=$buttonFG, buttonbg=$buttonBG",
               ()=>Framed(fg, bg, enlarge))


    def selector(caption: String, preferred: Scalar, choices: Scalar*)(action: Scalar=> Unit) : Glyph = {
      styled.Label(s" $caption: ").beside(chooser(choices, preferred)(action)).framed(black(width=4))
    }


    def chooser(numbers: Seq[Scalar], preferred: Scalar)(select: Scalar => Unit): Glyph = {
      val labels = numbers.map {
        d => styled.Label(f"${d}%2.2f")
      }
      val hintText=numbers.mkString(" ")
      val oneOf= OneOf.seq(align=Center, fg=transparent, bg=transparent)(labels)
      val next = styled.TextButton(">", hint=Hint(5, hintText)){ _ => oneOf.next(); select(numbers(oneOf.selection))  }
      val prev = styled.TextButton("<", hint=Hint(5, hintText)){ _ => oneOf.prev(); select(numbers(oneOf.selection))  }
      locally {
        var prefs = for { i <- 0 until numbers.length if numbers(i)==preferred } yield i
        for { pref <- prefs } oneOf._selection=pref
      }
      NaturalSize.Row(align=Mid)(prev.framed(black), oneOf, next.framed(black))
    }


    Col(align=Center)(
      <div width="60em" align="justify" parSkip="3ex">
        <p align="center">Button decoration.</p>
        <p>Select colours and properties, then use one of the four
          buttons below to bring up a new
          example of the decoration style that embodies them all exactly.
        </p>
        <p>
          <b>Note</b> that the four buttons will always embody most
          properties of the brushes you have chosen, but that their other
          properties are fixed when this GUI is constructed.
        </p>
        <fill height="3ex"/>
      </div>,
      anchor,
      Grid(width=2, padx=20, pady=20)(
        framed,
        roundframed,
      ),

      Grid(width=3, padx=20, pady=20)(
        selector("Enlarge", enlarge, 0.0f, 0.1f, 0.2f, 0.3f, 0.5f, 0.8f, 0.9f, 20.0f, 30.0f, 40.0f, 50f, 60f){ v=>enlarge=v },
        selector("Radius", radius, 0.0f, 0.1f, 0.2f, 0.3f, 0.5f, 0.8f, 0.9f, 20.0f, 30.0f, 40.0f, 50f, 60f){ v=>radius=v },
        selector("Delta", delta, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>delta=v },

        selector("Blur", blur, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>blur=v},
        selector("Spread", spread, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>spread=v },
        selector("Belta", belta, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>belta=v },
      ),

      Grid(width=2, padx=20, pady=10).rows(
        styled.Label("FG"), styled.Label("BG"),
        new BrushChooser(fg, fg, { _=> }).COLOURGUI,
        new BrushChooser(bg, bg, { _=> }).COLOURGUI,
        styled.Label("buttonFG"), styled.Label("buttonBG"),
        new BrushChooser(buttonFG, buttonFG, { _=> }).COLOURGUI,
        new BrushChooser(buttonBG, buttonBG, { _=> }).COLOURGUI
      )
    )
  }


  Page("Miscellaneous", "") {
      val (x, y) = (150f, 100f)
      def glyph = Label(" Text(bg=transparent) ").copy(bg=transparent)
      def star = PolygonLibrary.filledStargon(9, fg=blue(width=4)).scaled(.55f)
      def cross = Polygon(star.w, star.h, blue(width=4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0) )
      def starbstar: Glyph = star beside star
      def starastar: Glyph = star above star
      def staracross: Glyph = star above cross

      Col(align=Center)(
        Label(".edged (fg=black(width=2, bg<-{red, yellow, transparent})"), ex,
        Row(skip=10, align=Mid)(
          star.edged(fg=black(width=2), bg=red),
          cross.edged(fg=black(width=2), bg=red),
          star.edged(fg=black(width=2), bg=yellow),
          cross.edged(fg=black(width=2), bg=yellow),
          star.edged(fg=black(width=2), bg=transparent),
          cross.edged(fg=black(width=2), bg=transparent),
        ), ex, ex, ex, ex,
        Label(".roundFramed (fg=black(width=10, bg=yellow, radius<-{0.1, 0.3})"), ex,
        Row(skip=10, align=Mid)(
          star.roundFramed(fg=black(width=10), bg=yellow, radius = .1f),
          cross.roundFramed(fg=black(width=10), bg=yellow, radius = .1f),
          star.roundFramed(fg=black(width=10), bg=yellow, radius = .3f),
          cross.roundFramed(fg=black(width=10), bg=yellow, radius = .3f),
        ), ex, ex, ex, ex,
        Label(" radius=.9\n(glyphs may extend beyond a narrow frame at low curvature) "),
        Row(skip=10, align=Mid)(
          star.roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          cross.roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          starbstar.roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          starastar.roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          staracross.roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          Label("  width=20"),
          cross.roundFramed(fg=black(width=20), bg=yellow, radius = .9f),
          staracross.roundFramed(fg=black(width=20), bg=yellow, radius = .9f),

        ), ex, ex, ex, ex,
        Label(" radius=.9\n(overlap remedy: .enlarged(30).roundFramed(...) "),
        Row(skip=10, align=Mid)(
          star.enlarged(30).roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          cross.enlarged(30).roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          starbstar.enlarged(30).roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          starastar.enlarged(30).roundFramed(fg=black(width=10), bg=yellow, radius = .9f),
          staracross.enlarged(30).roundFramed(fg=black(width=10), bg=yellow, radius = .9f)
        ), ex scaled 4,
        Label(".roundFramed text [text's bg must be transparent if frame bg is always to prevail]"),
        Row(skip=10, align=Mid)(
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), transparent, .35f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), green, .35f), em,
          glyph.roundFramed(yellowHuge(width = 20, cap=ROUND), transparent, .5f), em,
          glyph.roundFramed(yellowHuge(width = 20, cap=ROUND), green, .5f), em,
          Label("Text(bg=lightGrey)").copy(bg=lightGrey).roundFramed(yellow(width = 10), bg=green, 0f), em,
        ), ex, ex,
          Row(skip=10, align=Mid)(
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), transparent, .35f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), green, .35f), em,
          glyph.roundFramed(yellowHuge(width = 20, cap=ROUND), transparent, .5f), em,
          glyph.roundFramed(yellowHuge(width = 20, cap=ROUND), green, .5f), em,
          Label("Text(bg=lightGrey)").copy(bg=lightGrey(width=30)).roundFramed(yellow(width = 20), bg=green, .5f), em,
        )
      ).scaled(0.7f).enlarged(40)
    }

  Page("GlyphML", "") {
    // TODO: decoration implemntations for round framed need checking
    language.definitions("star") = { style => PolygonLibrary.filledStargon(9, fg=blue(width=4)).scaled(.5f) }
    <div width="70em" >
      <col align="center">
      <p align="center">
        Many glyphXML constructs can specify decorations. Below we show a few examples of
        such specifications, and their outcomes; using a glyph defined in Scala by adding
        a generator to the translation:
      </p>
      <fill/>
      <div fontFamily="Courier" fontScale="0.9" textForeground="black" textBackground="transparent" background="lightGrey">
      <![CDATA[
             translation("star") = {
               style => PolygonLibrary.filledStargon(9, fg=blue(width=4)).scaled(.55f)
             }
      ]]>
      </div>
      <fill height="3ex"/>
      <table foreground="darkGrey.3" padX="2em" padY="2em" cols="3">
          <glyph gid="star"/>
          <glyph gid="star" scaled="0.5"/>
          <glyph gid="star" frame="red.2.sliced(3,2)" />

          <glyph gid="star" decorate="edge;enlarge;frame" frame="red.20.dashed(10,10)"  enlarged="25px"/>
          <glyph gid="star" decorate="edge;enlarge;frame" frame="red.20" radius="0.3" enlarged="25px" edge="black.3"/>
          <div decorate="rotate;edge;frame" rotated="1" frame="red.25" radius="0.2" enlarged="25px" edge="black.3">
            <glyph gid="star"/>
            <glyph gid="star"/>
          </div>

          <glyph gid="star" decorate="enlarge;frame" frame="red.5.stroke.sliced(5,5)" radius="3" enlarged="25px"/>
          <glyph gid="star" decorate="edge;enlarge;frame" frame="red.20" radius="3" enlarged="25px" edge="black.3"/>
          <div decorate="rotate;edge;frame" rotated="1" frame="red.25" radius="3" enlarged="25px" edge="black.3">
            <glyph gid="star"/>
            <glyph gid="star"/>
          </div>
      </table>
      <fill height="3ex"/>
      <div normalizePCData="true" fontFamily="Courier" fontScale="0.9" textForeground="black" textBackground="transparent" background="lightGrey">
      <![CDATA[
           <rows foreground="darkGrey.3" padX="2em" padY="2em" cols="3">
              <glyph gid="star"/>
              <glyph gid="star" scaled="0.5"/>
              <glyph gid="star" frame="red.2.sliced(3,2)" />

              <glyph gid="star" decorate="edge;enlarge;frame" frame="red.20.dashed(10,10)"  enlarged="25px"/>
              <glyph gid="star" decorate="edge;enlarge;frame" frame="red.20" radius="0.3" enlarged="25px" edge="black.3"/>
              <div decorate="rotate;edge;frame" rotated="1" frame="red.25" radius="0.2" enlarged="25px" edge="black.3">
                <glyph gid="star"/>
                <glyph gid="star"/>
              </div>

              <glyph gid="star" decorate="enlarge;frame" frame="red.5.fill.sliced(5,5)" radius="3" enlarged="25px"/>
              <glyph gid="star" decorate="edge;enlarge;frame" frame="red.20" radius="3" enlarged="25px" edge="black.3"/>
              <div decorate="rotate;edge;frame" rotated="1" frame="red.25" radius="3" enlarged="25px" edge="black.3">
                <glyph gid="star"/>
                <glyph gid="star"/>
              </div>
          </rows>
      ]]>
      </div>
      </col>
    </div>
  }


  val GUI: Glyph = book.Layout.leftCheckBoxes().enlarged(40)


}
