package org.sufrin.glyph
package tests.demonstrationBook

import styled.TextButton
import styled.Label
import styled.windowdialogues.Dialogue
import styled.Book
import styled.BookSheet
import styled.StringLog
import styled.CheckBox
import NaturalSize.{Col, Grid, Row}
import GlyphTypes.Scalar
import org.sufrin.glyph.unstyled.static.{Concentric, FilledRect, Polygon, Rect, RRect}

import io.github.humbleui.skija.PaintStrokeCap
import org.sufrin.glyph.unstyled.static

class Framing(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: StyleSheet = style.buttonSheet.copy(fontScale = 0.8f)
  import translation._
  import pageSheet.{ex, em}
  val book = Book()
  val Page = book.Page
  import Brushes._


  Page("Edged Text #1", "Texts .edged(fg, yellow, rf)\n(showing effect of fg brush size/cap") {
      val fg = red(width = 10, cap = ROUND).copy()

      def short = Label(" short ")(pageSheet.copy(labelBackgroundBrush = yellow))

      def med = Label(" A medium length label ")(pageSheet.copy(labelBackgroundBrush = yellow))

      def long: Glyph = <p backgroundBrush="yellow" textBackgroundBrush="yellow" align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>


      def row(rf: Scalar, cap:PaintStrokeCap = ROUND): Glyph = {
        val FG = fg(width=rf, cap=cap)
        Col(align=Center)(
          Label(s".edged($FG)\n").scaled(0.9f),
          Row(align=Mid, skip=10)(
            short.edged(FG, bg=yellow),
            med.edged(FG, bg=yellow),
            long.enlarged(20).edged(FG, bg=yellow)), ex)
      }


      Col(align=Center)(
        Row(Col(align=Center)(List(2f, 5f, 10f, 20, 35f).map(row(_, ROUND))),
            Col(align=Center)(List(2f, 5f, 10f, 20f, 35f).map(row(_, SQUARE))))
      ).enlarged(40)
    }

  Page("RoundFramed Text #1", "Texts .roundFramed(fg, yellow, rf)\n(smaller rf => tighter corners)") {
    val fg = red(width = 10, cap = ROUND).copy()


    def short = Label(" short ")

    def med = Label(" A medium length label ")

    def long: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)
    def longer: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)

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
          (List(3f, .25f, .125f, 0f).map(rf => row(rf, red(width=20, cap=ROUND))))
      )
    ).enlarged(40)
  }

  Page("Edged Text #2") {
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

  Page("Edged #3", "Text.copy(bg=transparent).edged(fg, bg)\n(text bg prevails over edged bg unless transparent)") {
    def label(bg: Brush) = SimpleParagraphs(15, align=Center, font=FontFamily("Courier")(25), fg=darkGrey, bg=bg)("""The rain in spain falls mainly in the plain""")

    Grid(padx=10, pady=10, fg=transparent, width=5).rows(
      for {cap<-List(ROUND); tbg<- List(yellow, transparent); bg<-List(green, transparent); width<-List(2f, 4f, 10f, 20f, 40f)} yield
        Label(s"fg=red($width,$cap)\ntext bg=$tbg, bg=$bg") above label(tbg).edged(fg=red(width = width, cap = cap), bg=bg) ,
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

  Page("GlyphXML", "") {
    translation("star") = { style => PolygonLibrary.filledStargon(9, fg=blue(width=4)).scaled(.5f) }
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
      <rows foreground="darkGrey/3" padX="2em" padY="2em" cols="3">
          <glyph gid="star"/>
          <glyph gid="star" scaled="0.5"/>
          <glyph gid="star" frame="red/1~5~5" />
          <glyph gid="star" decorate="enlarge;edge;frame" frame="red/5~5~5" radius="0.1" enlarged="25px"/>
          <glyph gid="star" decorate="edge;enlarge;frame" frame="red/20" radius="0.3" enlarged="25px" edge="black/3"/>
          <div decorate="rotate;edge;frame" rotated="1" frame="red/25" radius="0.2" enlarged="25px" edge="black/3">
            <glyph gid="star"/>
            <glyph gid="star"/>
          </div>
          <glyph gid="star" decorate="enlarge;edge;frame" frame="red/5~5~5" radius="3" enlarged="25px"/>
          <glyph gid="star" decorate="edge;enlarge;frame" frame="red/20" radius="3" enlarged="25px" edge="black/3"/>
          <div decorate="rotate;edge;frame" rotated="1" frame="red/25" radius="3" enlarged="25px" edge="black/3">
            <glyph gid="star"/>
            <glyph gid="star"/>
          </div>
      </rows>
      <fill height="3ex"/>
      <div normalizePCData="true" fontFamily="Courier" fontScale="0.9" textForeground="black" textBackground="transparent" background="lightGrey">
      <![CDATA[
           <rows foreground="darkGrey/3" padX="2em" padY="2em" cols="3">
                <glyph gid="star"/>
                <glyph gid="star" scaled="0.5"/>
                <glyph gid="star" frame="red/1~5~5" />

                <glyph gid="star" decorate="enlarge;edge;frame" frame="red/5~5~5" radius="0.1" enlarged="25px"/>
                <glyph gid="star" decorate="edge;enlarge;frame" frame="red/20" radius="0.3" enlarged="25px" edge="black/3"/>
                <div decorate="rotate;edge;frame" rotated="1" frame="red/25" radius="3" enlarged="25px" edge="black/3">
                  <glyph gid="star"/>
                  <glyph gid="star"/>
                </div>

                <glyph gid="star" decorate="enlarge;edge;frame" frame="red/5~5~5" radius="3" enlarged="25px"/>
                <glyph gid="star" decorate="edge;enlarge;frame" frame="red/20" radius="3" enlarged="25px" edge="black/3"/>
                <div decorate="rotate;edge;frame" rotated="1" frame="red/25" radius="3" enlarged="25px" edge="black/3">
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
