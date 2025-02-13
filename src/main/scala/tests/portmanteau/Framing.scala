package org.sufrin.glyph
package tests.portmanteau

import sheeted.TextButton
import sheeted.Label
import sheeted.windowdialogues.Dialogue
import sheeted.Book
import sheeted.BookSheet
import sheeted.StringLog
import sheeted.CheckBox
import NaturalSize.{Col, Row, Grid}
import GlyphTypes.Scalar
import Glyphs.Polygon

class Framing(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: Sheet = style.buttonSheet.copy(fontScale = 0.8f)
  import translation._
  import pageSheet.{ex, em}
  val book = Book()
  val Page = book.Page
  import DefaultBrushes._


  Page("Framed Text #1", "Texts framed with red(width = 10, cap = ROUND) at different curvatures") {
      val fg = red(width = 10, cap = ROUND).copy()


      def short = Label("short")

      def med = Label("A medium length label")

      def long: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)
      def longer: Glyph = <p align="justify" width="20em">A text paragraph that may obscure a low curvature frame.</p>.enlarged(25)

      def row(rf: Scalar): Glyph = {
        Col.atLeft(
          Label(f"curvature=${1f/rf}%1.2f radiusFactor=$rf%1.3f\n").scaled(0.7f),
          Row.centered(
            short.framed(fg, nothing, rf), em,
            med.framed(fg, nothing, rf), em,
            long.framed(fg, nothing, rf)))
      }


      Col.centered(
      Col.centered$(
        List(.5f, .3f, .25f, .125f).map(row(_))
      ), ex,ex,
        Label("The effect of 35px enlargement and a wider frame brush  [red(width=30)]."), ex,
        Row(
          longer.framed(fg=fg.copy(width=30), nothing, .3f), em, em,
          longer.framed(fg=fg.copy(width=30), nothing, .5f)
        )
      )
    }

  Page("Framed Text #2","Texts framed with red(width = 10, cap = ROUND) at different curvatures") {
    val fg = red(width = 10, cap = ROUND).copy()

    def short = Label("short").rotated(1)

    def med = Label("A medium length label").rotated(1)

    def long: Glyph = <p align="justify" width="12em">A text para_graph that may ob_scure a low-_curv_a_ture frame</p>.enlarged(25)

    val width = 3.5f*(short.w+med.w+long.w)

    def row(rf: Scalar, enlarge: Scalar = 0f, strokeWidth: Scalar=fg.strokeWidth): Glyph = {
      fg.strokeWidth(strokeWidth)
      Col.centered(
        ex.scaled(0.5f),
        Label(f"curvature=${1f/rf}%1.2f radiusFactor=$rf%1.3f strokeWidth=$strokeWidth%1.1f\n").scaled(0.7f),
        Row.centered(
          short.enlarged(enlarge).framed(fg, nothing, rf), em,
          med.enlarged(enlarge).framed(fg, nothing, rf), em,
          long.enlarged(enlarge).framed(fg, nothing, rf)))
    }

    val emWidth = em.w

    NaturalSize.Grid(fg=black(width=0), padx=10f, pady=10f, width=2)(
      row(0.5f),     row(.3f),
      row(0.25f),    row(.125f),
      row(.5f, 0f, 30), row(0.3f, 0f, 30)
    ).enlarged(20)

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



    Col.centered(
      Grid(fg=black(width=0), padx=10, pady=10, width=4, height=5).rows(List(0f, 20f, 30f, 40f).flatMap(row(_))), ex,
      Label("The effect of 20px enlargement and a wider frame brush  [red(width=30)]."), ex,
      long.enlarged(20).edged(fg=fg.copy(width=30), nothing)
    )
  }


    Page("Framed Glyphs", "Glyph framing") {
      val (x, y) = (150f, 100f)
      def glyph = Label("Text Label").scaled(1.5f)
      def star = PolygonLibrary.filledStargon(9, fg=blueLine).scaled(.5f).framed()
      def cross = Polygon(star.w, star.h, blue(width=4))((0, 0), (star.w, star.h), (0, star.h), (star.w, 0), (0,0) ) scaled 0.5f

      Col.centered(
        Row().centered(
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
        Row().centered(
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

    val GUI: Glyph = book.Layout.rightButtons().enlarged(20)


}
