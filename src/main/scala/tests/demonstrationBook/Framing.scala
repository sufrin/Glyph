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

  if (false) Page("Edged", "Glyph edging") {
    val (x, y) = (150f, 100f)
    def label = SimpleParagraphs(15, align=Center, font=FontFamily("Courier")(35))("""The rain in spain falls mainly in the plain""").edged()
    def long = SimpleParagraphs(35, align=Center, font=FontFamily("Courier")(35))("""The rain in spain falls mainly in the plain""").edged()
    def star = PolygonLibrary.filledStargon(9, fg=blueLine).scaled(.5f).framed()
    def cross = Polygon(star.w, star.h, blue(width = 4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0)) scaled 0.5f


    def frame(fg: Brush=nothing,  bg: Brush=nothing, radius: Scalar = 0f)(glyph: Glyph): Glyph = glyph.framed(fg, bg, radius)

      Grid(padx=10, pady=20, fg=nothing, width=5).rows(
        label.edged(fg=red(width = 10, cap = ROUND), bg=nothing),
        label.edged(fg=red(width = 20, cap = ROUND), bg=nothing),
        label.edged(fg=red(width = 30, cap = ROUND), bg=nothing),
        label.edged(fg=blue(width = 40, cap = ROUND), bg=green),
        label.edged(fg=blue(width = 50, cap = ROUND), bg=green),

        frame(fg=greenFrame, radius= 10)(label),
        frame(fg=greenFrame, radius= 20)(label),
        frame(fg=greenFrame, radius= 30)(label),
        frame(fg=greenFrame, radius= 40)(label),
        frame(fg=greenFrame, radius= 50)(label),

        frame(bg=greenFrame, radius= .0f)(label),
        frame(bg=greenFrame, radius= .1f)(label),
        frame(bg=greenFrame, radius= .2f)(label),
        frame(bg=greenFrame, radius= .3f)(label),
        frame(bg=greenFrame, radius= .4f)(label),

        frame(fg=redFrame(width=20), radius= .01f)(label),
        frame(fg=redFrame(width=20), radius= .1f)(label),
        frame(fg=redFrame(width=20), radius= .2f)(label),
        frame(fg=redFrame(width=20), radius= .3f)(label),
        frame(fg=redFrame(width=20), radius= .4f)(label),
      ).above(Row(align=Mid)(
        frame(fg=redFrame(width=20), radius= .2f)(long),
        frame(fg=redFrame(width=20), radius= .3f)(label),
        frame(fg=redFrame(width=20), radius= .4f)(long),
      )) .scaled(0.7f)
  }

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

  Page("Framed Text #2","Texts .framed(red(width=10, cap=ROUND), nothing, radius) at different radii") {
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
          cross.framed(fg = yellowHuge(width = 24, cap = SQUARE), bg = nothing), em,
          cross.framed(fg = yellowHuge(width = 24, cap = ROUND), bg = nothing), em,
          cross.mounted(yellowHuge(width = 18, cap=ROUND), nothing), em,
          cross.mounted(yellowHuge(width = 18, cap=ROUND), green), em,
          Label("FOOTLE").rotated(1).framed(fg = green(width = 24, cap = ROUND), bg = nothing), em,
          Label("FOOTLE").rotated(1).framed(fg = green(width = 24, cap = ROUND), bg = red), em,
          Label("FOOTLE").rotated(1).framed(fg = green(width = 24, cap = SQUARE), bg = nothing), em,
        ), ex,
        Row(align=Mid)(
          glyph.framed(red(width = 30, cap = SQUARE), green), em,
          glyph.framed(red(width = 30, cap = ROUND), nothing)), ex,
        Row(align=Mid)(
          glyph.framed(red(width = 30, cap = ROUND), red), em,
          glyph.framed(red(width = 30, cap = ROUND), green)
        ), ex,
      ).scaled(0.98f).enlarged(40)
    }


    val GUI: Glyph = book.Layout.leftCheckBoxes().enlarged(40)


}
