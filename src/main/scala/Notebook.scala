package org.sufrin.glyph

import NaturalSize.nothing

/**
 * Manage the declaration of notebook pages and the construction of tabbedNotebook
 * notebook GUIs.
 */
trait Notebook {

    import NaturalSize.{Col, Row}
    import Styles._
    import styled._

    import TextLayout._

    import scala.collection.mutable.ArrayBuffer

    /**
     * Page descriptor
     */
    class Page(val title: String, val gloss: String, val glyph: Glyph)(implicit sheet: Styles.Sheet) {
      val detail = sheet.labelStyle
      import detail.Spaces._

      override val toString: String = s"Page($title, $gloss){$glyph}"

      def root(): Glyph =
        if (gloss.isEmpty)
          glyph
        else
          Col.centered(TextLabel(gloss), ex, glyph)
    }

    val pages: ArrayBuffer[Page] = new ArrayBuffer[Page]()

    /** Declare a page */
    def Page(title: String, gloss: String, publish: Boolean = true)(glyph: Glyph)(implicit sheet: Styles.Sheet): Page = {
      val page = new Page(title, gloss, glyph)
      if (publish) {
        pages += (page)
      }
      page
    }

    object DefinePage {
      def apply(title: String, gloss: String, publish: Boolean = true)(glyph: Glyph)(implicit sheet: Styles.Sheet): Page = {
        val page = new Page(title, gloss, glyph)
        if (publish) {
          pages += (page)
        }
        page
      }
    }

    def popupButtons(implicit sheet: Styles.Sheet): List[Glyph]  = {
      def button(page: Page): Glyph = {
        lazy val here: Glyph = TextButton(page.title) {
          _ => windowdialogues.Dialogue.OK(page.root()).SouthEast(here).start() }
        here
      }
      pages.toList.map(button(_))
    }

    import DynamicGlyphs.OneOf

    /** 
     * In principle `buttons(i)` has a reaction `oneOf.select(i)`. This
     * is true when the `buttons` have been constructed by `tabbedNotebook`.
     */
    case class TabbedNotebook(buttons: Seq[Glyph], oneOf: OneOf)

  /**
   * Derive a `TabbedNotebook` that corresponds to the declared pages of thus
   * notebook. When `uniform` is true, `buttons` will all have the same diagonal, else
   * they will be at their natural size (modulo styled framing)
   */
    def tabbedNotebook(uniform: Boolean)(implicit sheet: Styles.Sheet): TabbedNotebook = {
      val glyphs: Seq[Glyph] = pages.toList.map(_.root())
      val oneOf = DynamicGlyphs.OneOf.seq()(glyphs)
      val keyed = (0 until glyphs.length) zip pages

      lazy val buttons = keyed map  {
        case (n: Int, page: Page) =>
          TextButton(page.title) { _ => oneOf.select(n) }
      }

      lazy val uniformButtons: Seq[UniformSize.ButtonSpecification] =
        keyed.map {
          case (n: Int, page: Page) => UniformSize(page.title){ _ => oneOf.select(n) }
        }

      TabbedNotebook(if (uniform) UniformSize.constrained(uniformButtons) else buttons, oneOf)
    }

  /** 
   * Deliver complete glyphs based on the declared pages of the notebook 
   */
  object Layout {
    import GlyphTypes.Scalar

    val dividerBlack = Brush("dividerblack")(color=0xFF770000, width=4)

    def blackLine(width: Scalar, height: Scalar): Glyph = {
      Glyphs.Polygon(width, height, fg=dividerBlack)((0,0), (width,0), (width, height), (0, height), (0,0))
    }

    /** A menu bar on which there is a popup button for each page */
    def menuBar(implicit sheet: Styles.Sheet): Glyph = Col.centered(Row.centered(Row.centered$(popupButtons)))

    /**
     * A `TabbedNotebook` with a button corresponding to each page, and a `OneOf` holding the pages.
     * Each button whose the corresponding page on the `OneOf` when clicked.
     */
    def raw(sheet: Styles.Sheet, uniform: Boolean = true): TabbedNotebook = tabbedNotebook(uniform)(sheet)

    def rightButtons(uniform: Boolean=true)(implicit sheet: Styles.Sheet):  Glyph   = {
      val TabbedNotebook(buttons, oneOf) = tabbedNotebook(uniform)
      val rhs = Col().atRight$(buttons)
      val lhs = oneOf
      val divider = blackLine(4, rhs.h max lhs.h)
      Row.centered(lhs, divider, rhs)
    }

    def leftButtons(uniform: Boolean=true)(implicit sheet: Styles.Sheet): Glyph = {
      val TabbedNotebook(buttons, oneOf) = tabbedNotebook(uniform)
      val lhs = Col().atRight$(buttons)
      val rhs = oneOf
      val divider = blackLine(2, rhs.h max lhs.h)
      Row.centered(lhs, divider, rhs)
    }

    def rotatedButtons(quads: Int, uniform: Boolean=true)(implicit sheet: Styles.Sheet) = {
      val TabbedNotebook(buttons, oneOf) = tabbedNotebook(uniform)
      val lhs = Row().atBottom$(buttons.map { b => b.rotated(quads, bg=nothing) })
      val rhs = oneOf
      val divider = blackLine(rhs.w max lhs.w, 4)
      Col.centered(lhs, divider, rhs)
    }

    def skewedButtons(skewX: Scalar, skewY: Scalar, uniform: Boolean=true)(implicit sheet: Styles.Sheet) = {
      val TabbedNotebook(buttons, oneOf) = tabbedNotebook(uniform)
      val lhs = Row().atBottom$(buttons.map { b => (b.rotated(3, bg=nothing)) })
      val rhs = oneOf
      val divider = blackLine(rhs.w max lhs.w, 4)
      Col.centered(lhs.skewed(-skewX, skewY), divider, rhs)
    }

    def topButtons(uniform: Boolean=true)(implicit sheet: Styles.Sheet) = {
      val TabbedNotebook(buttons, oneOf) = tabbedNotebook(uniform)
      import sheet.buttonStyle.up.Spaces.{em, ex}
      val lhs = Row(bg=nothing).atBottom$(buttons)
      val rhs = oneOf
      val divider = blackLine(rhs.w max lhs.w, 4)
      Col.centered(lhs, divider, rhs)
    }
  }
}

object Notebook {
  def apply(): Notebook = new Notebook {}
}
