package org.sufrin.glyph
package styled

import NaturalSize.transparent
import unstyled.static


case class BookSheet(buttonSheet:  StyleSheet, pageSheet: StyleSheet)

/**
 * Manage the declaration of notebook pages and the construction of
 * notebook GUIs.
 */
trait Book {

  import NaturalSize.{Col, Row}
  import styles._

  import scala.collection.mutable.ArrayBuffer

  /**
   * Page descriptor
   */
  class Page(val title: String, val gloss: String)(val glyph: Glyph)(implicit sheet: StyleSheet) {
    val detail: GlyphStyle = sheet.labelStyle
    import detail.ex

    override val toString: String = s"Page($title, $gloss){$glyph}"
    def root(): Glyph =
      if (gloss.isEmpty)
        glyph
      else
        Col(align=Center)(Label(gloss), ex, glyph)
  }

  val pages: ArrayBuffer[Page] = new ArrayBuffer[Page]()


  object Page {
    def apply(title: String, gloss: String="", publish: Boolean = true)(glyph: Glyph)(implicit sheet: StyleSheet): Page = {
      val page = new Page(title, gloss)(glyph)(sheet)
      if (publish) {
        pages += (page)
      }
      page
    }
  }

  object DefinePage {
    def apply(title: String, gloss: String="", publish: Boolean = true)(glyph: Glyph)(implicit sheet: StyleSheet): Page = {
      val page = new Page(title, gloss)(glyph)(sheet)
      if (publish) {
        pages += (page)
      }
      page
    }
  }

//  def popupButtons(implicit sheet: BookStyle): List[Glyph]  = {
//    def button(page: Page): Glyph = {
//      lazy val here: Glyph = TextButton(page.title) {
//        _ => windowdialogues.Dialogue.OK(page.root())(sheet.pageStyle).SouthEast(here).start() }(sheet.buttonStyle)
//      here
//    }
//    pages.toList.map(button(_))
//  }

  import unstyled.dynamic.OneOf

  /**
   * In principle `buttons(i)` has a reaction `oneOf.select(i)`. This
   * is true when the `buttons` have been constructed by `buttonedBook`.
   */
  case class BookComponents(buttons: Seq[Glyph], oneOf: OneOf)

  /**
   * Derive a `BookComponents` that corresponds to the declared pages of thus
   * notebook. When `buttonAlign` is `Justify`, `buttons` will all have the same diagonal, else
   * they will be at their natural size (modulo styled framing).
   *
   * `pageAlign` specifies the alignment of each page of the book within
   * the bounding box of the book as a whole.
   *
   * @see OneOf
   */
  def buttonedBook(buttonAlign: Alignment=Justify, pageAlign: Alignment, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): BookComponents = {
    implicit val style: StyleSheet = sheet.buttonSheet
    val glyphs: Seq[Glyph] = pages.toList.map(_.root())
    val oneOf = OneOf.seq(bg=sheet.pageSheet.backgroundBrush, align=pageAlign, valign=pageVAlign)(glyphs)
    val keyed = (0 until glyphs.length) zip pages
    val uniform = buttonAlign==Justify

    lazy val buttons = keyed map  {
      case (n: Int, page: Page) =>
        TextButton(page.title) { _ => oneOf.select(n) }
    }

    lazy val uniformButtons: Seq[UniformSize.ButtonSpecification] =
      keyed.map {
        case (n: Int, page: Page) => UniformSize(page.title){ _ => oneOf.select(n) }
      }

    BookComponents(if (uniform) UniformSize.constrained(uniformButtons) else buttons, oneOf)
  }


  /**
   * Derive a `BookComponents` that corresponds to the declared pages of thus
   * notebook. In this case the  "buttons" as a whole will be checkboxes
   * derived from  `RadioCheckBoxes` and captioned with the page titles. Ticking a checkbox will select the
   * corresponding page.
   *
   * `buttonAlign` specifies the relationship between the checkbox and caption on each "button" of the
   * column of buttons.
   * {{{
   *   Left    the checkbox is immediately to the left of the checkbox
   *   Right   the checkbox is on the right of the entire column
   *           the caption is on the left
   *   Center  the checkbox is on the right of the entire column
   *           the caption is centered
   *   Justify the checkbox is on the right of the entire column
   *           the caption is just to the left of the checkbox
   * }}}
   *
   * `pageAlign` specifies the alignment of each page of the book within
   * the bounding box of the book as a whole.
   *
   * @see OneOf
   */
  def checkBoxedBook(buttonAlign: Alignment=Left, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): BookComponents = {
    implicit val style: StyleSheet = sheet.buttonSheet
    val glyphs: Seq[Glyph] = pages.toList.map(_.root())
    val oneOf = OneOf.seq(bg=sheet.pageSheet.backgroundBrush, align=pageAlign, valign=pageVAlign)(glyphs)

    val radio = RadioCheckBoxes(pages.toList.map(_.title), prefer=pages.head.title){
      case Some(n) => oneOf.select(n)
      case None    => oneOf.select(0)
    }(sheet.buttonSheet.copy(buttonDecoration=decoration.Edged()))

    BookComponents(radio.glyphButtons(buttonAlign), oneOf)

  }

  /**
   * Deliver complete notebook glyphs based on the declared pages of the notebook
   * The bounding box of each notebook page is the union of the bounding boxes of
   * the pages.
   *
   * Complete glyphs are delivered by one of the methods below, whose
   * names suggest the placement of the buttons associated with the notebook
   * in relation to the glyph itself.
   *
   * The `align` parameter indicates the horizontal alignment of each page with respect to
   * the overall bounding box (by default this is `Center`). If `uniform` is true,
   * then the buttons all have the same size.
   * {{{
   *   rightCheckBoxes(uniform: Boolean=true, align: Alignment=Center): Glyph
   *   rightButtons(uniform: Boolean=true, align: Alignment=Center): Glyph
   *   leftButtons(uniform: Boolean=true, align: Alignment=Center): Glyph
   *   rotatedButtons(quads: Int, uniform: Boolean=true, align: Alignment=Center): Glyph
   *   skewedButtons(skewX: Scalar, skewY: Scalar, uniform: Boolean=true, align: Alignment=Center): Glyph
   *   topButtons(uniform: Boolean=true, align: Alignment=Center): Glyph
   *   menuBar: Glyph
   * }}}
   *
   * Deliver a `BookComponents(buttons, oneOf)` without placing the buttons or the `oneOf`. This
   * makes it possible to implement non-standard juxtapositions of buttons and the notebook page
   * (for example, by having two rows/columns of buttons).
   * {{{
   *   raw(sheet: StyleSheet, uniform: Boolean = true, align: Alignment=Center): BookComponents
   * }}}
   */
  object Layout {
    import GlyphTypes.Scalar

    val dividerBlack = Brush("black.4")(color=0xFF770000, width=4)

    def blackLine(width: Scalar, height: Scalar): Glyph = {
      static.Polygon(width, height, fg=dividerBlack)((0,0), (width,0), (width, height), (0, height), (0,0))
    }

    /**
     * A `BookComponents` with a button corresponding to each page, and a `OneOf` holding the pages.
     * Each button selects the corresponding page on the `OneOf` when clicked.
     */
    def rawButtoned(buttonAlign: Alignment = Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): BookComponents =
      buttonedBook(buttonAlign, pageAlign)(sheet)

    /**
     * A `BookComponents` with a captioned checkbox corresponding to each page, and a `OneOf` holding the pages.
     * Each checkbox selects the corresponding page on the `OneOf` when clicked.
     */
    def rawCheckBoxed(buttonAlign: Alignment = Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): BookComponents =
      checkBoxedBook(buttonAlign, pageAlign, pageVAlign)(sheet)

    def rightCheckBoxes(buttonAlign: Alignment = Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): Glyph = {
      val BookComponents(buttons, oneOf) = checkBoxedBook(buttonAlign, pageAlign, pageVAlign)
      val rhs = Col(align=Left)(buttons)
      val lhs = oneOf
      val divider = blackLine(4, rhs.h max lhs.h)
      Row(align=Mid, bg=sheet.buttonSheet.backgroundBrush)(lhs, divider, rhs)
    }

    def rightButtons(buttonAlign: Alignment=Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet):  Glyph   = {
      val BookComponents(buttons, oneOf) = buttonedBook(buttonAlign, pageAlign, pageVAlign)
      val rhs = Col(align=Right)(buttons)
      val lhs = oneOf
      val divider = blackLine(4, rhs.h max lhs.h)
      Row(align=Mid, bg=sheet.buttonSheet.backgroundBrush)(lhs, divider, rhs)
    }

    def leftButtons(buttonAlign: Alignment=Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): Glyph = {
      val BookComponents(buttons, oneOf) = buttonedBook(buttonAlign, pageAlign, pageVAlign)
      val lhs = Col(align=Right)(buttons)
      val rhs = oneOf
      val divider = blackLine(2, rhs.h max lhs.h)
      Row(align=Mid, bg=sheet.buttonSheet.backgroundBrush)(lhs, divider, rhs)
    }

    def leftCheckBoxes(buttonAlign: Alignment=Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet): Glyph = {
      val BookComponents(buttons, oneOf) = checkBoxedBook(buttonAlign, pageAlign, pageVAlign)
      val lhs = Col(align=Right)(buttons)
      val rhs = oneOf
      val divider = blackLine(2, rhs.h max lhs.h)
      Row(align=Mid, bg=sheet.buttonSheet.backgroundBrush)(lhs, divider, rhs)
    }

    def rotatedButtons(quads: Int, buttonAlign: Alignment=Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet) = {
      val BookComponents(buttons, oneOf) = buttonedBook(buttonAlign, pageAlign, pageVAlign)
      val lhs = Row(Bottom)(buttons.map { b => b.rotated(quads, bg=transparent) })
      val rhs = oneOf
      val divider = blackLine(rhs.w max lhs.w, 4)
      Col(align=Center, bg=sheet.buttonSheet.backgroundBrush)(lhs, divider, rhs)
    }

    def skewedButtons(skewX: Scalar, skewY: Scalar, buttonAlign: Alignment=Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet) = {
      val BookComponents(buttons, oneOf) = buttonedBook(buttonAlign, pageAlign, pageVAlign)
      val lhs = Row(align=Bottom)(buttons.map { b => (b.rotated(3, bg=transparent)) })
      val rhs = oneOf
      val divider = blackLine(rhs.w max lhs.w, 4)
      Col(align=Center, bg=sheet.buttonSheet.backgroundBrush)(lhs.skewed(-skewX, skewY), divider, rhs)
    }

    def topButtons(buttonAlign: Alignment=Justify, pageAlign: Alignment=Center, pageVAlign: VAlignment=Mid)(implicit sheet: BookSheet) = {
      val BookComponents(buttons, oneOf) = buttonedBook(buttonAlign, pageAlign, pageVAlign)
      val lhs = Row(align=Bottom, bg=transparent)(buttons)
      val rhs = oneOf
      val divider = blackLine(rhs.w max lhs.w, 4)
      Col(align=Center, bg=sheet.buttonSheet.backgroundBrush)(lhs, divider, rhs)
    }

    def popupMenu(sheet: BookSheet, decorate: Glyph=>Glyph = { g=>g }): Seq[Glyph] = {
      implicit val style: StyleSheet = sheet.buttonSheet
      val glyphs: Seq[Glyph] = pages.toList.map(page => decorate(page.root()))
      val titles: Seq[String] = pages.toList.map(_.title)
      val uniformButtons: Seq[UniformSize.ButtonSpecification] =
        (titles zip glyphs).map{
          case (pageTitle, page) => UniformSize(pageTitle){
            _ => new Application {
              def GUI: Glyph = page
              def title: String = pageTitle
              override def whenStarted(): Unit = {
                super.whenStarted()
                Application.confirmCloseRequestsFor(GUI)
                Application.enableAutoScaleFor(GUI)
              }
            }.main(Array())
          }
        }
      UniformSize.constrained(uniformButtons)
    }
  }
}

object Book {
  def apply(): Book = new Book {}
}
