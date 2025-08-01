package org.sufrin.glyph
package tests

import styled.overlaydialogues.Dialogue
import Brushes._
import styled.BookSheet
import styles.decoration

import io.github.humbleui.skija.PaintMode


class AdderGUI()(implicit sheet: StyleSheet)  {
  import NaturalSize.{Col, Row}
  import styled.{TextButton, _}

  import sheet.ex


  var `a⊕b`:   Double => Double => Double = _.+
  var `c⊕a`:   Double => Double => Double = _.-
  var `c⊕b`:   Double => Double => Double = _.-
  var opName: String = "+"

  val opGlyph: ActiveString = ActiveString(opName)
  val helpGlyph: ActiveParagraph = ActiveParagraph(50, Justify)(helpText())


  def setOp(opName: String, `a⊕b`: Double => Double => Double, `c⊕a`: Double => Double => Double, `c⊕b`: Double => Double => Double): Unit = {
    this.opName  = opName
    helpGlyph.set(helpText())
    opGlyph.set(opName)
    this.`a⊕b`   = `a⊕b`
    this.`c⊕a`   = `c⊕a`
    this.`c⊕b`   = `c⊕b`
  }

  def helpText(): String =
    s""" This app solves c = a $opName b
      |if at least two of a, b, c look like numbers.
      |
      | Moving the cursor
      |into a text field when the other two look like numbers
      |causes that field to be recalculated.
      |
      |""".stripMargin


  /** Check that the named field looks like a number or a candidate for filling */
  def check(name: String, field: TextField): Unit = {
    if (!(field.string.toDoubleOption.isDefined || field.string.isBlank))
    {
      Dialogue.CHOOSE(Paragraph(30, Justify)(
        s"""
          |${field.string} doesn't look much like a number.
          |""".stripMargin))("Clear", "Carry on entering a number").North(field).andThen {
        case "Clear" => field.string = ""; field.takeKeyboardFocus()
        case _       => field.takeKeyboardFocus()
      }
    }
  }

  lazy val a: TextField  = TextField(size = 10, onEnter = { _ => calculemus() }, onCursorLeave = _ => check("a", a))
  lazy val b: TextField  = TextField(size = 10, onEnter = { _ => calculemus() }, onCursorLeave = _ => check("b", b))
  lazy val c: TextField  = TextField(size = 10, onEnter = { _ => calculemus() }, onCursorLeave = _ => check("c", c))
  lazy val fields: Seq[TextField] = List(a, b, c)

  locally {
    for { field <- fields }
      field.onCursorEnter = { text => field.takeKeyboardFocus(); calculemus(field) }
  }

  def clear(): Unit = {
    for { field <- fields }  field.string=""
  }

  def text(d: Double): String = f"$d%.5g"

  def calculemus(target: TextField*): Unit = {
    val av = a.string.toDoubleOption
    val bv = b.string.toDoubleOption
    val cv = c.string.toDoubleOption
    (cv, av, bv) match {
      case (Some(c), Some(a), None) if b.string.isBlank => b.string = text(`c⊕a`(c)(a))
      case (Some(c), None, Some(b)) if a.string.isBlank => a.string = text(`c⊕b`(c)(b))
      case (None, Some(a), Some(b)) if c.string.isBlank => c.string = text(`a⊕b`(a)(b))
      case (Some(cv), Some(a), Some(b))                 =>
           if   (cv==`a⊕b`(a)(b)) {}
           else
           if (target.nonEmpty) {
             for { field <- target } field.string=""
             calculemus()
           } else {
             Dialogue.OK(Label("Eh?"))
           }
      case _ =>
    }
  }

  def root: Glyph = Col(align=Center)(
    helpGlyph, ex,
    Row(TextButton("Clear") { _ => clear() }), ex,
    Row(align=Mid)(c.framed(), Label(" = "), a.framed(), opGlyph, b.framed())
  )

}

class CalculatorGUI()(implicit sheet: StyleSheet) extends AdderGUI()(sheet) {

  import NaturalSize._
  import styled.RadioCheckBoxes

  import sheet.ex

  def flip[S,T,U](op: S=>T=>U):T=>S=>U = { t:T => s:S => op(s)(t) }

  lazy val operations: RadioCheckBoxes = RadioCheckBoxes(List("+", "-", "*", "/"), "+") {
    // `a⊕b` `c⊕a` `c⊕b`
    case Some(0) =>  setOp("+", _.+, _.-, _.-)
    case Some(1) =>  setOp("-", _.-, flip(_.-), _.+)
    case Some(2) =>  setOp("*", _.*, _./, _./)
    case Some(3) =>  setOp("/", _./, flip(_./), _.*)
    case None => operations.select(0)
    case _ =>
  }(sheet.copy(fontScale = 0.75f, buttonDecoration=decoration.unDecorated))

  override def root: Glyph = Col(align=Center)(
    super.root, ex,
    Row(align=Mid)(styled.Label("Chose an operator "),
                 Grid().grid(height=2)(operations.glyphRows)) enlarged 20
  ) enlarged 20

}

trait TopLevelGUI {
  import styled.Book
  val book: Book = Book()
  val Page = book.Page

  implicit val pageStyle: StyleSheet = StyleSheet(backgroundBrush=white, buttonDecoration=decoration.Blurred(Brush("pink")))//RoundFramed(blue(width=2), radius = 0.5f))
  implicit val bookStyle: BookSheet = BookSheet(buttonSheet = pageStyle, pageSheet=pageStyle.copy(buttonDecoration = decoration.unDecorated))


  Page("Adder", "") {
    val GUI = new AdderGUI()
    GUI.root
  }

  Page("Calculator", "") {
    val GUI = new CalculatorGUI()
    GUI.root
  }

  val root: Glyph = {
    book.Layout.leftButtons(Left, Left)
  }
}

object CalculatorExample extends Application {
    val GUI: Glyph = new TopLevelGUI {} . root
    override def title: String = "CalculatorExample"
    //override val defaultIconPath: Option[String] = Some("./PNG/redpoly.png")
    override val dock = new Dock {
      import GlyphShape._
      setGlyph((text("=")() ~~~ rect(25,25)(red(width=1, mode=PaintMode.STROKE))).scale(1f).asGlyph)
    }
}


