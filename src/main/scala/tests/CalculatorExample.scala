package org.sufrin.glyph
package tests

import overlaydialogues.Dialogue
import styled.TextLayout.TextParagraphs

object BasicStyle extends Styles.Sheet {
  import GlyphTypes._
  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Courier", FontStyle.NORMAL)
  override lazy val buttonFontSize: Scalar = 25
}

object MenuStyle extends Styles.Sheet {
  import GlyphTypes._
  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL)
  override lazy val buttonFontSize: Scalar = 25
}

class AdderGUI()(implicit sheet: Styles.Sheet)  {
  import NaturalSize.{Col, Row}
  import styled.TextButton
  import styled.TextLayout.{ActiveParagraphs, ActiveString, TextLabel}

  import sheet.Spaces._

  var `a⊕b`:   Double => Double => Double = _.+
  var `c⊕a`:   Double => Double => Double = _.-
  var `c⊕b`:   Double => Double => Double = _.-
  var opName: String = "+"

  val opGlyph: DynamicGlyphs.ActiveString = ActiveString(opName)
  val helpGlyph: DynamicGlyphs.ActiveGlyph[String] = ActiveParagraphs(50, Justify)(helpText())


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
    if (!(field.text.toDoubleOption.isDefined || field.text.isBlank))
    {
      Dialogue.CHOOSE(TextParagraphs(30, Justify)(
        s"""
          |${field.text} doesn't look much like a number.
          |""".stripMargin))("Clear", "Carry on entering a number").North(field).andThen {
        case "Clear" => field.text = ""; field.takeKeyboardFocus()
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
    for { field <- fields }  field.text=""
  }

  def text(d: Double): String = f"$d%.5g"

  def calculemus(target: TextField*): Unit = {
    val av = a.text.toDoubleOption
    val bv = b.text.toDoubleOption
    val cv = c.text.toDoubleOption
    (cv, av, bv) match {
      case (Some(c), Some(a), None) if b.text.isBlank => b.text = text(`c⊕a`(c)(a))
      case (Some(c), None, Some(b)) if a.text.isBlank => a.text = text(`c⊕b`(c)(b))
      case (None, Some(a), Some(b)) if c.text.isBlank => c.text = text(`a⊕b`(a)(b))
      case (Some(cv), Some(a), Some(b))                 =>
           if   (cv==`a⊕b`(a)(b)) {}
           else
           if (target.nonEmpty) {
             for { field <- target } field.text=""
             calculemus()
           } else {
             Dialogue.OK(TextLabel("Eh?"))
           }
      case _ =>
    }
  }

  def root: Glyph = Col.centered(
    helpGlyph, ex,
    Row(TextButton("Clear") { _ => clear() }), ex,
    Row.centered(c.framed(), TextLabel(" = "), a.framed(), opGlyph, b.framed())
  )

}

class CalculatorGUI()(implicit sheet: Styles.Sheet) extends AdderGUI() {

  import NaturalSize._
  import styled.RadioCheckBoxes

  import sheet.Spaces._

  def flip[S,T,U](op: S=>T=>U):T=>S=>U = { t:T => s:S => op(s)(t) }

  lazy val operations: RadioCheckBoxes = RadioCheckBoxes(List("+", "-", "*", "/"), "+") {
    // `a⊕b` `c⊕a` `c⊕b`
    case Some(0) =>  setOp("+", _.+, _.-, _.-)
    case Some(1) =>  setOp("-", _.-, flip(_.-), _.+)
    case Some(2) =>  setOp("*", _.*, _./, _./)
    case Some(3) =>  setOp("/", _./, flip(_./), _.*)
    case None => operations.select(0)
    case _ =>
  }

  override def root: Glyph = Col.centered(
    super.root, ex,
    Row.centered(styled.TextLayout.TextLabel("Choose an operator "),
                 Grid(fg=Brush()(width=0)).grid(height=2)(operations.glyphRows)) enlarged 20
  ) enlarged 20

}

trait TopLevelGUI {
  val noteBook: Notebook = Notebook()
  val Page: noteBook.DefinePage.type = noteBook.DefinePage

  Page("Adder", "") {
    val GUI = new AdderGUI()(BasicStyle)
    GUI.root
  }(MenuStyle)

  Page("Calculator", "") {
    val GUI = new CalculatorGUI()(BasicStyle)
    GUI.root
  }(MenuStyle)

  val root: Glyph = {
    noteBook.Layout.rightButtons(true)((MenuStyle))
  }
}

object CalculatorExample extends Application {
    val GUI: Glyph = new TopLevelGUI {} . root
    override def title: String = "CalculatorExample"
    override val defaultIconPath: Option[String] = Some("./flag.png")
}


