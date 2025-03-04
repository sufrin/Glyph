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
import Glyphs.{Concentric, FilledRect, Polygon, Rect, RRect}

import org.sufrin.glyph.Glyphs

class Framing(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: StyleSheet = style.buttonSheet.copy(fontScale = 0.8f)
  import translation._
  import pageSheet.{ex, em}
  val book = Book()
  val Page = book.Page
  import DefaultBrushes._


  Page("Framed Text #1", "Texts .framed(red(width=10, cap=ROUND), nothing, radius) at different radii") {
      val fg = red(width = 10, cap = ROUND).copy()


      def short = Label(" short ")

      def med = Label(" A medium length label ")

      def long: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)
      def longer: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)

      def row(rf: Scalar): Glyph = {
        Col(align=Center)(
          Label(f"radius=$rf%1.3f\n").scaled(0.7f),
          Row(align=Mid)(
            short.framed(fg, nothing, rf), em,
            med.framed(fg, nothing, rf), em,
            long.framed(fg, nothing, rf)), ex)
      }


      Col(align=Center)(
        Col(align=Center)(List(3f, 5f, .25f, .125f).map(row(_))),
        ex,ex,
        Label("The effect of a wider frame brush [red(width=30)] and longer text."), ex,
        Row(
          longer.framed(fg=fg.copy(width=30), nothing, 3f), em, em,
          longer.framed(fg=fg.copy(width=30), nothing, 5f)
        )
      ).enlarged(40)
    }

  Page("RoundFramed Text #2", "Texts .roundFramed(red(width=10, cap=ROUND), nothing, radius) at different radii") {
    val fg = red(width = 10, cap = ROUND).copy()


    def short = Label(" short ")

    def med = Label(" A medium length label ")

    def long: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)
    def longer: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)

    def row(rf: Scalar): Glyph = {
      Col(align=Center)(
        Label(f"radius=$rf%1.3f\n").scaled(0.7f),
        Row(align=Mid)(
          short.roundFramed(fg, nothing, rf), em,
          med.roundFramed(fg, nothing, rf), em,
          long.roundFramed(fg, nothing, rf)), ex)
    }


    Col(align=Center)(
      Col(align=Center)(List(3f, 0.35f, .25f, .125f).map(row(_))),
      ex,ex,
      Label("The effect of a wider frame brush [red(width=30)] and longer text."), ex,
      Row(
        longer.roundFramed(fg=fg.copy(width=30), nothing, 3f), em, em,
        longer.roundFramed(fg=fg.copy(width=30), nothing, 5f)
      )
    ).enlarged(40)
  }

  Page("Framed Text #3","Texts .framed(red(width=10, cap=ROUND), nothing, radius) at different radii") {
    val fg = red(width = 10, cap = ROUND).copy()

    def short = Label(" short ").rotated(1)

    def med = Label(" A medium length label ").rotated(1)

    def long: Glyph = <p align="justify" width="12em">A text para_graph that may ob_scure a low-_curv_a_ture frame</p>.enlarged(25)

    val width = 3.5f*(short.w+med.w+long.w)

    def row(rf: Scalar, enlarge: Scalar = 0f, strokeWidth: Scalar=fg.strokeWidth): Glyph = {
      fg.strokeWidth(strokeWidth)
      Col(align=Center)(
        ex.scaled(0.5f),
        Label(f"radius=$rf%1.3f strokeWidth=$strokeWidth%1.1f\n").scaled(0.7f),
        Row(align=Mid)(
          short.enlarged(enlarge).framed(fg, nothing, rf), em,
          med.enlarged(enlarge).framed(fg, nothing, rf), em,
          long.enlarged(enlarge).framed(fg, nothing, rf)))
    }

    val emWidth = em.w

    NaturalSize.Grid(padx=10f, pady=10f, width=2)(
      row(3f),         row(5f),
      row(0.25f),      row(.125f),
      row(3f, 0f, 30), row(5f, 0f, 30)
    ).enlarged(40)

  }

  Page("RoundFramed Text #4","Texts .roundFramed(red(width=10, cap=ROUND), nothing, radius) at different radii") {
    val fg = red(width = 10, cap = ROUND).copy()

    def short = Label(" short ").rotated(1)

    def med = Label(" A medium length label ").rotated(1)

    def long: Glyph = <p align="justify" width="12em">A text para_graph that may ob_scure a low-_curv_a_ture frame</p>.enlarged(25)

    val width = 3.5f*(short.w+med.w+long.w)

    def row(rf: Scalar, enlarge: Scalar = 0f, strokeWidth: Scalar=fg.strokeWidth): Glyph = {
      fg.strokeWidth(strokeWidth)
      Col(align=Center)(
        ex.scaled(0.5f),
        Label(f"radius=$rf%1.3f strokeWidth=$strokeWidth%1.1f\n").scaled(0.7f),
        Row(align=Mid)(
          short.enlarged(enlarge).roundFramed(fg, nothing, rf), em,
          med.enlarged(enlarge).roundFramed(fg, nothing, rf), em,
          long.enlarged(enlarge).roundFramed(fg, nothing, rf)))
    }

    val emWidth = em.w

    NaturalSize.Grid(padx=10f, pady=10f, width=2)(
      row(0f),         row(.15f),
      row(0.25f),      row(.125f),
      row(3f, 0f, 30), row(5f, 0f, 30)
    ).enlarged(40)

  }

  Page("Edged Text", "Texts variously enlarged then edged with red(width = 10, cap = ROUND)") {
    val fg = red(width = 10, cap = ROUND).copy()

    def short = Label("short")

    def med = Label("A medium length label")

    def long: Glyph = <p align="justify" width="20em">Edging is not as attractive as fram_ing, but never over_laps.</p>

    def row(px: Scalar): Seq[Glyph] =
      List(
          Label(s"enlarged($px)").scaled(0.7f),
          short.enlarged(px).edged(fg, nothing),
          med.enlarged(px).edged(fg, nothing),
          long.enlarged(px).edged(fg, nothing))

    Col(align=Center)(
      Grid(padx=10, pady=10, width=4, height=5).rows(List(0f, 20f, 30f, 40f).flatMap(row(_))), ex,
      Label("The effect of 20px enlargement and a wider frame brush  [red(width=30)]."), ex,
      long.enlarged(20).edged(fg=fg.copy(width=30), nothing)
    ) enlarged 40
  }

  Page("Edged/Framed", "Glyph edging and framing") {
    val (x, y) = (150f, 100f)
    def label = SimpleParagraphs(15, align=Center, font=FontFamily("Courier")(35))("""The rain in spain falls mainly in the plain""")
    def long = SimpleParagraphs(35, align=Center, font=FontFamily("Courier")(35))("""The rain in spain falls mainly in the plain""")
    def star = PolygonLibrary.filledStargon(9, fg=blueLine).scaled(.5f).framed()
    def cross = Polygon(star.w, star.h, blue(width = 4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0)) scaled 0.5f

    def frame(fg: Brush=nothing, bg: Brush=nothing, radius: Scalar)(glyph: Glyph): Glyph = glyph.framed(fg, bg, radius)
    def rframe(fg: Brush=nothing, bg: Brush=nothing, radius: Scalar)(glyph: Glyph): Glyph = glyph.roundFramed(fg, bg, radius)

    Grid(padx=10, pady=20, fg=nothing, width=5).rows(
      label.edged(fg=red(width = 10, cap = ROUND), bg=nothing),
      label.edged(fg=red(width = 20, cap = ROUND), bg=nothing),
      label.edged(fg=red(width = 30, cap = ROUND), bg=nothing),
      label.edged(fg=red(width = 40, cap = ROUND), bg=nothing),
      Label(s"Edged(fg<-red({10, 20, 30, 40})"),

      label.edged(fg=red(width = 10, cap = ROUND), bg=green),
      label.edged(fg=red(width = 20, cap = ROUND), bg=green),
      label.edged(fg=red(width = 30, cap = ROUND), bg=green),
      label.edged(fg=red(width = 40, cap = ROUND), bg=yellow.rounded(170)),
      Label(s"Edged(fg<-red({10, 20, 30, 40}, bg=green)"),

      label.edged(bg=red(cap = ROUND), fg=nothing),
      label.edged(bg=red(cap = ROUND).rounded(20), fg=nothing),
      label.edged(bg=red(cap = ROUND).rounded(30), fg=nothing),
      label.edged(bg=red(cap = ROUND).rounded(40), fg=nothing),
      Label(s"Edged(bg<-red.radius({0, 20, 30, 40}, fg=nothing)"),

      label.framed(fg=red(width = 10, cap = ROUND), bg=nothing),
      label.framed(fg=red(width = 20, cap = ROUND), bg=nothing),
      label.framed(fg=red(width = 30, cap = ROUND), bg=nothing),
      label.framed(fg=red(width = 40, cap = ROUND), bg=nothing, radius=0),
      Label(s"Framed(fg<-red({10, 20, 30, 40})"),


      frame(bg=greenFrame, radius= .1f)(label),
      frame(bg=greenFrame, radius= .2f)(label),
      frame(bg=greenFrame, radius= .3f)(label),
      frame(bg=greenFrame, radius= .4f)(label),
      Label(s"Framed(bg=greenFrame, radius<-{.1, .2, .3. .4})"),

      frame(fg=redFrame(width=20), radius= .1f)(label),
      frame(fg=redFrame(width=20), radius= .2f)(label),
      frame(fg=redFrame(width=20), radius= .3f)(label),
      frame(fg=redFrame(width=20), radius= .4f)(label),
      Label(s"Framed(bg=redFrame(20), radius<-{.1, .2, .3. .4})"),


      rframe(fg=redFrame(width=20), radius= .01f)(label),
      rframe(fg=redFrame(width=20), radius= .1f)(label),
      rframe(fg=redFrame(width=20), radius= .2f)(label),
      rframe(fg=redFrame(width=20), radius= .3f)(label),
      rframe(fg=redFrame(width=20), radius= .4f)(label),

      rframe(fg=redFrame(width=30), radius= .0f)(label),
      rframe(fg=redFrame(width=30), radius= .1f)(label),
      rframe(fg=redFrame(width=30), radius= .2f)(label),
      rframe(fg=redFrame(width=30), radius= .3f)(label),
      rframe(fg=redFrame(width=30), radius= .4f)(label),

      rframe(fg=redFrame(width=50), radius= .3f)(label),
      rframe(fg=redFrame(width=50), radius= .45f)(label),
      rframe(fg=redFrame(width=50), radius= .5f)(label),
      rframe(fg=redFrame(width=50), radius= .9f)(label),
      rframe(fg=redFrame(width=50), radius= 5f)(label),
    ).scaled(0.7f)
  }

  Page("Framed Glyphs", "Glyph framing") {
      val (x, y) = (150f, 100f)
      def glyph = Label("Text Label").scaled(1.5f)
      def star = PolygonLibrary.filledStargon(9, fg=blueLine).scaled(.5f).framed()
      def cross = Polygon(star.w, star.h, blue(width=4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0) ) scaled 0.5f

      Col(align=Center)(
        Row(align=Mid)(
          star, em,
          star.mounted(red(width = 38, cap = SQUARE), green), em,
          star.mounted(red(width = 8, cap = ROUND), green), em,
          star.mounted(red(width = 8, cap = ROUND), red)), ex,
        Row(align=Mid)(cross, em,
          cross.mounted(yellowHuge(width = 8, cap = ROUND), nothing), em,
          cross.mounted(yellowHuge(width = 8, cap = ROUND), red), em,
          cross.mounted(yellowHuge(width = 16, cap = ROUND), nothing), em,
          cross.mounted(yellowHuge(width = 16, cap = ROUND), red), em,
        ), ex,
        Row(align=Mid)(
          star.mounted(fg = black(width = 10, cap = ROUND), bg = nothing), em,
          star.mounted(fg = black(width = 20, cap = ROUND), bg = nothing), em,
          star.mounted(fg = black(width = 30, cap = ROUND), bg = nothing), em,
          star.mounted(fg = black(width = 40, cap = ROUND), bg = yellowHuge), em
        ), ex,
        Row(align=Mid)(
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), nothing), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), green), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), nothing, .15f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), green, .15f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), nothing, .25f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), green, .25f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), nothing, .35f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), green, .35f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), nothing, .5f), em,
          cross.roundFramed(yellowHuge(width = 18, cap=ROUND), green, .5f), em,
        ), ex, ex,
        Row(align=Mid)(
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), nothing, 0), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), green, 0), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), nothing, .25f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), green, .25f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), nothing, .35f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), green, .35f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), nothing, .5f), em,
          glyph.roundFramed(yellowHuge(width = 10, cap=ROUND), green, .5f), em,
        )
      ).scaled(0.98f).enlarged(40)
    }


    val GUI: Glyph = book.Layout.leftCheckBoxes().enlarged(40)


}
