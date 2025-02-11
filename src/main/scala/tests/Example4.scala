package org.sufrin.glyph
package tests

import NaturalSize.{Col, Row}
import sheeted.overlaydialogues.Dialogue.OK
import sheeted._

trait Example4Interface {
  implicit val Style: Sheet = Sheet()
  import glyphXML.Language._

  lazy val fields = List(a, b, c)

  import overlaydialogues.Dialogue.OK
  val help: Glyph =
    <div width="40em" textFontSize="18" align="justify">
    <p>
    This app solves the equation <i>c = a + b</i> if at least two of <i>a, b, c</i>
    are well-formed numbers: possibly floating-point.
    </p>
    <p>
      Typing <tt>↩</tt> (<i>ie. the enter key</i> in any of the text fields, causes the
      equation to be re-solved.
    </p>
    </div>

  val a, b, c = field()
  val GUI: Glyph = Col.centered(
    help,
    Row.centered(
      c.framed(),
      Label(" = "),
      a.framed(),
      Label(" + "),
      b.framed()
    )
  ) enlarged 25

  def field(): TextField = TextField(
    size = 8,
    onEnter = {
      case s: String if s.toDoubleOption.isDefined => calculemus()
      case s: String =>
        OK( <p align="center" width="15em">The text {s"'$s'"} doesn't look much like a number. Enter it again plase.</p> ).InFront(help).start()
    }
  )

  def calculemus(): Unit =
    (
      c.text.toDoubleOption,
      a.text.toDoubleOption,
      b.text.toDoubleOption
    ) match {
      case (None, Some(av), Some(bv)) => c.text = text(av + bv)
      case (Some(cv), Some(av), None) => b.text = text(cv - av)
      case (Some(cv), None, Some(bv)) => a.text = text(cv - bv)
      case (Some(cv), Some(av), Some(bv)) =>
        if (cv == av + bv) {} else c.text = text(av + bv)
      case _ =>
    }

  def text(d: Double): String = f"$d%.5g"

  def clear(): Unit = {
    for { field <- fields } field.text = ""
  }
}

object Example4 extends Application with Example4Interface {
  override def title: String = "Example4"
}
