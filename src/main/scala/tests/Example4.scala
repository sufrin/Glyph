package org.sufrin.glyph
package tests

import NaturalSize.{Col, Row}
import styled.TextLayout._


trait Example4Interface {
  object LocalStyle extends Styles.Basic
  import LocalStyle._

  lazy val fields = List(a, b, c)

  import overlaydialogues.Dialogue.OK
  val help = TextParagraphs(50, Justify)(
    """This app solves c = a + b if at least two of a,b,c
      |are well-formed (possibly floating point) numbers.
      |
      | Typing ↩ in
      |any of the text fields, causes the
      |calculation to be done again.
      |
      |""".stripMargin
  )
  val a, b, c = field()
  val GUI: Glyph = Col.centered(
    help enlarged 25,
    Row.centered(
      c.framed(),
      TextLabel(" = "),
      a.framed(),
      TextLabel(" + "),
      b.framed()
    )
  ) enlarged 25

  def field(): TextField = TextField(
    size = 8,
    onEnter = {
      case s: String if s.toDoubleOption.isDefined => calculemus()
      case s: String =>
        OK(
          TextLabel(
            s"""\"$s\" doesn't look much like a number.
           |Correct it or re-enter it please.
           |You can clear any field by typing ctrl-U.""".stripMargin,
            Left
          ).enlarged(20)
        ).InFront(help).start()
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
