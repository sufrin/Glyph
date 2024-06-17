package org.sufrin.glyph.tests
import org.sufrin.glyph._

import NaturalSize.{Col, Row}
import Styles.{ButtonStyle, GlyphStyle}
import styled.TextButton
import overlaydialogues.Dialogue
import styled.TextLayout.TextParagraphs

object BasicStyle extends Styles.Basic

object MenuStyle extends Styles.Basic {
  import GlyphTypes._
  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Arial", FontStyle.NORMAL)
  override lazy val buttonFontSize: Scalar = 30
}

class AdderGUI()(implicit detail: Styles.Basic)  {
  import NaturalSize.{Col, Row}
  import styled.TextButton
  import styled.TextLayout.{ActiveParagraphs, ActiveString, TextLabel}

  implicit val buttonFeatures: ButtonStyle = detail.buttonStyle
  implicit val glyphStyle:    GlyphStyle      = buttonFeatures.up
  import glyphStyle.Spaces._

  var `a⊕b`:   Double => Double => Double = _.+
  var `c⊕a`:   Double => Double => Double = _.-
  var `c⊕b`:   Double => Double => Double = _.-
  var opName: String = "+"

  val opGlyph = ActiveString(opName)
  val helpGlyph = ActiveParagraphs(25, Justify)(helpText())(MenuStyle.labelStyle)


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
  lazy val fields = List(a, b, c)

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
      case (Some(c), Some(a), None) if (b.text.isBlank) => b.text = text(`c⊕a`(c)(a))
      case (Some(c), None, Some(b)) if (a.text.isBlank) => a.text = text(`c⊕b`(c)(b))
      case (None, Some(a), Some(b)) if (c.text.isBlank) => c.text = text(`a⊕b`(a)(b))
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

class CalculatorGUI()(implicit detail: Styles.Basic) extends AdderGUI() {

  import NaturalSize._
  import styled.RadioCheckBoxes
  import glyphStyle.Spaces._

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

  override def root = Col.centered(
    super.root, ex,
    Row.centered(styled.TextLayout.TextLabel("Choose an operator "), Row.atTop$(operations.colGlyphs))
  )

}

trait TopLevelGUI {
  
  val noteBook = new Notebook
  val Page     = noteBook.DefinePage
  import MenuStyle._


  Page("Adder", "") {
    val GUI=  new AdderGUI()(BasicStyle)
    GUI.root
  }

  Page("Calculator", "") {
    val GUI=new CalculatorGUI()(BasicStyle)
    GUI.root
  }

  Page("Clone", "Clone this app with a new GUI") {
    lazy val Duplicated = new Application {
      def GUI: Glyph = new TopLevelGUI {} . root
      def title = s"""CalculatorExample -scale=$scaleFactor ${extraArgs.mkString(", ")}"""
      override
      val defaultIconPath: Option[String] = Some("./flag.png")
    }

    implicit val buttonStyle: ButtonStyle =  MenuStyle.buttonStyle

    import labelStyle.Spaces.{em, ex}

    def button(args: String): Glyph = {
      val arguments = args.split("[ ]+")
      TextButton(args) { _ => Duplicated.main(arguments) }
    }

    val styles  = "-scale=1.0/-scale=0.9/-scale=0.8/-scale=0.7/-scale=0.6/-scale=0.5".split("/").toList
    val buttons: List[Glyph] = styles.map(button)
    val leftButtons = buttons.take(buttons.length/2)
    val rightButtons = buttons.drop(leftButtons.length)

    Col.centered(
      TextParagraphs(ems = 40, Justify)(
        """Each of the buttons below clones a new instance of the
          |application with the properties specified on it.
          |
          | There is no limit on the number of instances that can be
          |running at once.
          |""".stripMargin), ex,
      Row(
        Col.atLeft$(leftButtons), em, em,
        Col.atRight$(rightButtons),
      )
    ) enlarged 60
  }


  val root = {
    import MenuStyle._
    noteBook.Layout.rightButtons()
  }
}

object CalculatorExample extends Application {
    val GUI = new TopLevelGUI {} . root
    override def title: String = "CalculatorExample"
    override val defaultIconPath: Option[String] = Some("./flag.png")
}


