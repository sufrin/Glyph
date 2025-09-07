package org.sufrin.glyph
package glyphML
package expression

import utility.{IteratorCursor, StringCursor}
import GlyphTypes.Scalar
import glyphML.AbstractSyntax.Scope
import glyphML.Context.Context

import org.sufrin.logging.{ALL, SourceDefault}
import org.sufrin.SourceLocation.{sourceLocation, SourceLocation}

object Expr {

  def apply(source: String, context: Context)(implicit at: SourceLocation = sourceLocation): Expr = new Expr(source, context)(at)

  implicit class Coercion(val scalar: Scalar) extends AnyVal {
    def asScale:  Value = Scale(scalar)
    def asPixels: Value = Pixels(scalar)
  }

  case class ExpressionError(why: String) extends Throwable with Value {
    def +(other: Value): Value = this
    def *(other: Value): Value = this
    def -(other: Value): Value = this
    def /(other: Value): Value = this
    def asScale: Scalar = throw this
    def asPixels: Scalar = throw this
  }

  def Erroneous(why: String): Unit = throw new ExpressionError(why)

  trait Value {
    def +(other: Value): Value
    def *(other: Value): Value
    def -(other: Value): Value
    def /(other: Value): Value
    def asScale: Scalar
    def asPixels: Scalar
  }

  case class Pixels(n: Scalar) extends Value {
    def asScale: Scalar = { Erroneous(s"Type error (not a scalar: $this"); 0f}
    def asPixels: Scalar = n
    def +(other: Value): Value = Pixels(n+other.asPixels)
    def -(other: Value): Value = Pixels(n-other.asPixels)
    def *(other: Value): Value = Pixels(n*other.asScale)
    def /(other: Value): Value = other match {
      case _: Pixels => Scale(n/other.asPixels)
      case _: Scale => Pixels(n/other.asScale)
    }
  }

  case class Scale(n: Scalar) extends Value {
    def asScale: Scalar = n
    def asPixels: Scalar =  { Erroneous(s"Type error (not a measurement: $this"); 0f }
    def +(other: Value): Value = Scale(n+other.asScale)
    def -(other: Value): Value = Scale(n-other.asScale)
    def *(other: Value): Value = other match {
      case _: Pixels => Pixels(n*other.asPixels)
      case _: Scale => Scale(n*other.asScale)
    }
    def /(other: Value): Value = other match {
      case _: Pixels => Pixels(n/other.asPixels)
      case _: Scale => Scale(n/other.asScale)
    }
  }

}

class Expr(source: String, context: Context)(implicit at: SourceLocation = sourceLocation) {
  import glyphML.Context.ExtendedStyleSheetAsMapping

  import Expr._

  val symbols = Lexemes(source)

  @inline private def result[T](r: T): T = {
    symbols.next()
    r
  }

  @inline private def checkFor(symbol: Lexeme): Unit = {
    if (symbols.current==symbol) symbols.next() else Erroneous(s"Notation error (expecting $symbol) at ${symbols.current} at ${context.scope}")
  }

  def fetchGlyphDiagonal(id: String): Vec = {
    val stored = context.definitions.getKind(StoreType.GlyphConstant)(id)
    val result = {
      stored match {
        case Some(StoredGlyphConstant(glyph)) =>
          glyph.diagonal
        case _ =>
          Erroneous(s"Unknown glyph: $id in $source  at ${context.scope}")
          Vec.Zero
      }
    }
    result
  }

  def factor(): Value = {
    symbols.current match {
      case num(name) =>
        val n = Scale(name.toFloat)
        symbols.next()
        symbols.current match {
          case e: id => n*factor()
          case other => n
        }

      case id(name) =>
        val sheet=context.sheet
        val value = name match {
          case "width" => sheet.parWidth.asPixels
          case "indent" => sheet.parIndent.asPixels
          case "leftmargin" => sheet.leftMargin.asPixels
          case "rightmargin" => sheet.rightMargin.asPixels
          case "container.width" => sheet.containerWidth.asPixels
          case "container.height" => sheet.containerHeight.asPixels
          case "window.width" => sheet.windowWidth.asPixels
          case "window.height" => sheet.windowHeight.asPixels
          case "screen.width" => sheet.screenWidth.asPixels
          case "screen.height" => sheet.screenHeight.asPixels
          case "em" => sheet.emWidth.asPixels
          case "ex" => sheet.exHeight.asPixels
          case "px" => 1f.asPixels
          case "ux" => 1f.asPixels
          case "baseline" => sheet.baseLine.asPixels
          case s"$id.width"  =>
            fetchGlyphDiagonal(id).x.asPixels
          case s"$id.height" =>
            fetchGlyphDiagonal(id).y.asPixels
          // Scalar field of the style sheet
          case "fontscale" => sheet.fontScale.asScale
          // Any dimension field of the style sheet
          case "parskip" => sheet.parSkip.asPixels
          case other =>
            sheet.field[Scalar](other).asPixels
        }
        result(value)
      case BRA =>
        symbols.next()
        val s=expression()
        checkFor(KET)
        s
      case op("-") =>
        symbols.next()
        val f=factor()
        Scale(-1f) * f
      case other =>
        Erroneous(s"Unexpected ${symbols.current}")
        result(Scale(0f))
    }
  }

  def term(): Value = {
    var r = factor()
    var going = true
    while (going && symbols.hasCurrent) symbols.current match {
      case op("*") => symbols.next(); r=r*factor()
      case op("/") => symbols.next(); r=r/factor()
      case other =>
        going=false
    }
    r
  }

  def expression(): Value = {
    var r = term()
    var going = true
    while (going && symbols.hasCurrent) symbols.current match {
      case op("+") => symbols.next(); r=r+term()
      case op("-") => symbols.next(); r=r-term()
      case other =>
        going=false
    }
    r
  }

  def value: Option[Value] = if (symbols.hasCurrent) {
    try { val v = expression(); checkFor(END); Some(v) } catch {
      case ExpressionError(s) =>
        SourceDefault.error(s"${s} in '${source.strip}'")(at)
        None
    }
  } else None

  def evaluate: Value = if (symbols.hasCurrent) {
    try { val v = expression(); checkFor(END); v } catch { case e: ExpressionError => e }
  } else ExpressionError("Empty expression")

}

trait Expression {
}

case object NilExpression extends Expression

trait Lexeme {}
case class id(name: String) extends Lexeme
case class op(name: String) extends Lexeme
case class num(name: String) extends Lexeme
case class nonsymbol(name: String) extends Lexeme
case object BRA extends Lexeme { override val toString = "'('"}
case object KET extends Lexeme { override val toString = "')'"}
case object END extends Lexeme { override val toString = "the end of the expression"}

class Symbols(source: String) extends Iterator[Lexeme] {
  val cursor = new StringCursor(source)
  locally { cursor.dropWhile(_.isWhitespace) }

  def hasNext: Boolean = { cursor.hasCurrent }

  val operators = "+-*/&".toSet

  @inline private def result(l: Lexeme): Lexeme = { cursor.next(); l }

  def isNumeric(c: Char): Boolean = c.isDigit || c=='.'
  def isIdentifier(c: Char): Boolean = c.isLetterOrDigit || c=='.'
  def next(): Lexeme = if (cursor.hasCurrent) {
    val r = cursor.current match {
      case '(' => result(BRA)
      case ')' => result(KET)
      case c if c.isWhitespace => cursor.dropWhile(_.isWhitespace); next()
      case c if operators.contains(c) => result(op(s"$c"))
      case '.' => cursor.next(); num("."+cursor.stringWhile(_.isDigit))
      case c if c.isDigit => num(cursor.stringWhile(isNumeric))
      case c if c.isLetterOrDigit => id(cursor.stringWhile(isIdentifier))
      case other =>
        nonsymbol(s"$other")
    }
    r
  } else END
}

object Lexemes {
  def apply(source: String): IteratorCursor[Lexeme] = new IteratorCursor(new Symbols(source))
}


object testExpression {
  locally { SourceDefault.level = ALL }
  def main (args: Array[String]): Unit = {
    val context = Context(Map.empty, StyleSheet(fontScale=3.141f), new Definitions, Scope())
    //val l0 = new Symbols(" a * b + (c * d) ")
    //while (l0.hasNext) { println(l0.next()) }

    val trials =
      """ (.2/3.0)
        | 2*3*4+5
        |  em
        |  fontscale
        |  - - 2*em
        |  em*2
        |  2em
        |  2px
        |  2
        |  ( 2 + 3 ) * em
        |  em*(2+3)
        |  em
        |  em/px
        |  px/em * em
        |  1/px * em
        |  ex/ex
        |  parskip
        |  width
        |  parskip+width
        |  foo
        |  3+em
        |  em+3
        |  em/(em*2
        |  em*em
        |  em*4&4)""".stripMargin.split("\n").toList
    for { trial <- trials } println(s"$trial = ${new Expr(trial, context).value}")


  }
}
