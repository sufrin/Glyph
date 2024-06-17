
# The Glyph UI Library for Scala

        Bernard Sufrin
        Emeritus: Department of Computer Science
        Oxford University


**Glyph** is a (prototype) user-interface library for Scala.
Its graphical features are geometrically compositional: rendered using a
small subset of the facilities of Google's **Skia** 
2-D graphics library, as delivered (in the JVM) by
the **Skija** library.

Our motivation for developing the library was our frustration, over
many years, with complex UI frameworks that impose a uniform style,
and that make it hard to develop novel modes of interaction. There
is nothing wrong with uniform styling: it's the cost of straying
outside the styling envelope that we want to diminish.

The belief that has guided this work is that a rich set of interactive
interface components can be composed using a small collection of
combining forms, together with a suitable collection of elementary
visual components -- some reactive.  So instead of (just) providing
a uniformly-styled “high level” toolkit, we have provided enough
elements and combining forms for an innovative UI designer to
experiment with, and a collection of implementations of conventional
components (for example buttons, checkboxes, text input components,
tabbed-notebooks) to serve as models. Its component-styling framework
straightforwardly supports the separation of component functionality from
component appearance; and although we have not yet done so, we think
it would be straightforward for a developer to generalize it into
a system of application “skins”.

**Glyph** has been tested on `OS/X` (x86 and Mx processors), `Linux` (x86),
and `Windows`. 

## Documentation

A draft introduction to **Glyph** appears in `Glyph-Document/Glyph.pdf`.
It provides a few explained examples of
**GUI**s (**G**lyph **U**ser **I**nterfaces). Prospective
users may also want to read the source code of the largest
example `DemonstrationNotebook`, which embodies many
useful **GUI** idioms.

## Example

Here's a little example.

````
package org.sufrin.glyph.tests
import org.sufrin.glyph._
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
  ) enlarged (25)

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

  def calculemus() =
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
````

## Getting Started