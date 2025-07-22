package org.sufrin.glyph
package tests
import NaturalSize.{Col, Row}
import styled._
import styled.overlaydialogues.Dialogue.OK

class Example4Interface(sheet: StyleSheet) {
  implicit val style: StyleSheet = sheet

  import glyphXML.Language._

  val help: Glyph =
    <div width="45em" align="justify">
    <p>
    This app solves the equation <i>c = a + b</i> if at least two of <i>a, b, c</i>
    are well-formed numbers: possibly floating-point.
    </p>
    <p>
      Typing <tt>↩</tt> (<i>ie. the enter key</i>) in any of the text fields, causes the
      equation to be re-solved.
    </p>
    </div>

  val a, b, c = textField()
  val fields = List(a, b, c)

  val GUI: Glyph = Col(align=Center)(
    help,
    Row(align=Mid)(
      c.framed(),
      Label(" = "),
      a.framed(),
      Label(" + "),
      b.framed()
    )
  ) enlarged 25

  def textField(): TextField = TextField(
    size = 8,
    onEnter = {
      case s: String if s.toDoubleOption.isDefined => calculemus()
      case s: String =>
        OK( <p align="justify" width="20em">The text {s"'$s'"} doesn't look much like a num_ber. Enter it again please.</p> ).InFront(help).start()
    }
  )

  def calculemus(): Unit =
    (
      c.string.toDoubleOption,
      a.string.toDoubleOption,
      b.string.toDoubleOption
    ) match {
      case (None, Some(av), Some(bv)) => c.string = text(av + bv)
      case (Some(cv), Some(av), None) => b.string = text(cv - av)
      case (Some(cv), None, Some(bv)) => a.string = text(cv - bv)
      case (Some(cv), Some(av), Some(bv)) =>
        if (cv == av + bv) {} else c.string = text(av + bv)
      case _ =>
    }

  def text(d: Double): String = f"$d%.5g"

  def clear(): Unit = {
    for { field <- fields } field.string = ""
  }
}

object Example4 extends Application {
  val sheet: StyleSheet = StyleSheet()
  val GUI: Glyph = new Example4Interface(sheet).GUI
  override def title: String = "Example4"
}
