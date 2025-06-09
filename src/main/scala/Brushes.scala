package org.sufrin
package glyph

import org.sufrin.glyph.NumberUtils.hexToInt
import org.sufrin.glyph.unstyled.Text
import org.sufrin.glyph.Colour.HSV
import org.sufrin.glyph.GlyphShape.{FILL, STROKE, STROKE_AND_FILL}
import org.sufrin.glyph.GlyphTypes.Scalar

import scala.annotation.tailrec
import scala.util.matching.Regex.{MatchData, MatchIterator}

/**
 * A convenience trait that defines several brushes.
 *
 * TODO: these should be systematically named and featured. The range of
 *       widths, and caps is unnecessary.
 */
trait DefaultBrushes {

  import io.github.humbleui.skija.PaintStrokeCap
  val SQUARE     = PaintStrokeCap.SQUARE
  val ROUND      = PaintStrokeCap.ROUND
  val BUTT       = PaintStrokeCap.BUTT
  val red        = Brush("red.0.fill")
  val redLine    = Brush("red.2.square.fill")
  val redWide    = Brush("red.25.fill.round")
  val redFrame   = Brush("red.25.fill.round")
  val blue       = Brush("blue.0.fill")
  val blueLine   = Brush("blue.2.round.fill")
  val blueFrame  = Brush("blue.5.round.fill")
  val green      = Brush("green.fill")
  val greenLine  = Brush("green.2.round.fill")
  val greenFrame = Brush("green.5.round.fill")
  val white      = Brush("white.fill")
  val whiteFrame = Brush("white.5.round")
  val black      = Brush("black.fill")
  val blackLine  = Brush("black.2.square.fill")
  val blackFrame = Brush("black.5.round.fill")
  val transparent= Brush("transparent")
  val invisible  = Brush("transparent.1")
  val lightGrey  = Brush("lightGrey")
  val darkGrey   = Brush("darkGrey")
  val grey1      = Brush(s"grey1")
  val grey2      = Brush(s"grey2")
  val grey3      = Brush(s"grey3")
  val grey4      = Brush(s"grey4")
  val yellow     = Brush("yellow")
  val yellowLine = Brush("yellow.2.square.stroke")
  val yellowFrame= Brush("yellow.5.square.stroke")
  /** Using the new Brush API */
  val yellowHuge = Brush("yellow.75.round")
  val brown: Brush = Brush("brown")(color=0xFF964b00, width=0f)

}

/**
 * Concrete definitions of a variety of brushes, and implementation of
 * a tiny language for specifying brushes.
 *
 * {{{
 *   Brushes(specification): Brush yields a brush as specified
 * }}}
 *
 * {{{
 *   specification ::= decorated
 *
 *   decorated     ::= basic [decoration]*
 *
 *   basic         ::= named
 *                 |   named.#strokewidth
 *
 *   named         ::= 0Xaarrggbb   // 4 hex bytes: alpha, red, blue, green
 *                 |   "one of the named colours"
 *
 *   decoration    ::=  .width(#strokewidth)
 *                 |    .stroke(#strokewidth)
 *                 |    .rounded(#strokeradius)
 *                 |    (#strokeRadius)
 *                 |    -#on-#off
 *                 |    ~#sliceLength~#displacement
 *                 |    .stroke | .fill | .stroke&fill
 *                 |    .round | .butt | .square
 *                 |    .blur(#blur)
 * }}}
 */
object Brushes extends DefaultBrushes {

  trait Lex
  case object EOS extends Lex
  case class Id(id: String) extends Lex
  case class ScalarValue(value: Scalar) extends Lex
  case class WithParameters(id: String, params: Seq[Scalar]) extends Lex {
    override def toString: String = s"$id(${params.mkString(",")})"
  }
  case class Hexadecimal(color: Int) extends Lex {
    override def toString: String = f"0x$color%6x"
  }

  lazy val Chunks = new scala.util.matching.Regex("(([.]?)(([0-9a-zA-Z&~+-])+([(][0-9.,]+[)])?))")

  def Parse(spec: String): Brush = {
    def Lex(state: Brush, spec: String): List[Lex] = {
      val explode = Chunks.findAllMatchIn(spec).map(_.group(3)).toList
      explode.map {
        case v if v matches("[0-9]+([.][0-9]*)?") => ScalarValue(v.toFloat)
        case s"0x${hex}" if hex.matches("([0-9a-fA-F])+") => Hexadecimal(hexToInt(hex))
        case v if v matches("[a-zA-Z0-9&~+-]+") => Id(v)
        case s"$id($params)" if id matches ("[a-zA-Z0-9&~+-]+") =>
          val scalars = params.split(',').toSeq.map(_.toFloat).toList
          WithParameters(id, scalars)
        case other => throw new NonBrush(s"Lexical-error at $other in $spec", state)
      }
  }
  def eval(b: Brush, l: List[Lex]): Brush = {
    l match {
      case WithParameters("hsv", List(h,s,v)) :: rest=>
        b.color(HSV(h,s,v).argb)
        eval(b, rest)
      case WithParameters("hsv", List(h,s)) :: rest=>
        b.color(HSV(h,s,1.0).argb)
        eval(b, rest)
      case WithParameters("hsv", List(h)) :: rest=>
        b.color(HSV(h,1.0,1.0).argb)
        eval(b, rest)
      case Hexadecimal(colour)::rest =>
        b.color(colour)
        eval(b, rest)
      case ScalarValue(width)::rest =>
        b.strokeWidth(width)
        eval(b, rest)
      case Id("fill")::rest =>
        b.mode(FILL)
        eval(b, rest)
      case Id("stroke")::rest =>
        b.mode(STROKE)
        eval(b, rest)
      case Id("stroke&fill")::rest =>
        b.mode(STROKE_AND_FILL)
        eval(b, rest)
      case Id("round")::rest =>
        b.cap(ROUND)
        eval(b, rest)
      case Id("butt")::rest =>
        b.cap(BUTT)
        eval(b, rest)
      case Id("square")::rest =>
        b.cap(SQUARE)
        eval(b, rest)
      case Id("antialias")::rest =>
        b.antiAlias(true)
        eval(b, rest)
      case Id("dither")::rest =>
        b.dither(true)
        eval(b, rest)
      case WithParameters("alpha", List(n))::rest =>
        b.alpha(n)
        eval(b, rest)
      case Id("--")::rest =>
        b.Effect.dashed(10*b.strokeWidth, 10*b.strokeWidth)
        eval(b, rest)
      case Id("-.")::rest =>
        b.Effect.dashed(10*b.strokeWidth, 5*b.strokeWidth)
        eval(b, rest)
      case Id(".-")::rest =>
        b.Effect.dashed(5*b.strokeWidth, 10*b.strokeWidth)
        eval(b, rest)
      case Id("..")::rest =>
        b.Effect.dashed(5*b.strokeWidth, 5*b.strokeWidth)
        eval(b, rest)
      case WithParameters("dashed", List(n))::rest =>
        b.Effect.dashed(n, n)
        eval(b, rest)
      case WithParameters("dashed", List(m, n))::rest =>
        b.Effect.dashed(m, n)
        eval(b, rest)
      case WithParameters("dashed", List(m, n, o, p))::rest =>
        b.Effect.dashed(m, n, o, p)
        eval(b, rest)
      case WithParameters("sliced", List(m, n))::rest =>
        b.Effect.sliced(m, n)
        eval(b, rest)
      case WithParameters("rounded", List(radius))::rest =>
        b.Effect.rounded(radius)
        eval(b, rest)
      case WithParameters("blurred", List(blur))::rest =>
        b.Effect.blurred(blur)
        eval(b, rest)
      case WithParameters("blurred", List(blur, delta))::rest =>
        b.Effect.blurred(blur, delta, delta)
        eval(b, rest)
      case WithParameters("blurred", List(blur, dx, dy))::rest =>
        b.Effect.blurred(blur, dx, dy)
        eval(b, rest)
      case Id(name)::rest =>
        namedColours.get(name) match {
          case Some(colour) =>
            b.name=name
            b.color(colour)
            eval(b, rest)
          case None =>
            throw new NonBrush(s"Unknown colour name: $name in $spec", b)
        }
      case Nil =>
      case other =>
        throw new NonBrush(s"Brush notation error ${other.mkString(".")} in $spec", b)
    }
    b
  }

    val state = new Brush(name = "", description = "")
    eval(state, Lex(state, spec.toLowerCase.replace(" ","")))
  }

  def main(args: Array[String]): Unit = {
    for { arg <- args } println(Brushes(arg))
  }

  case class NonBrush(why: String, brushState: Brush) extends Throwable


  lazy val namedColours: collection.mutable.Map[String, Int] = collection.mutable.LinkedHashMap[String, Int](
      "red" ->  0xFFFF0000,
      "rose" -> 0xFFFE28A2,
      "pink" -> 0xFFFFC0CB,
      "purple" -> 0xFFA020F0,
      "green" ->  0xFF00FF00,
      "blue" -> 0XFF0000FF,
      "persianblue" -> 0xFF1C39BB,
      "cornflower" -> 0xFF6495ED,
      "periwinkle" -> 0xFFCCCCFF,
      "yellow" -> 0XFFFFFF00,
      "yellow+pantone" -> 0xFFFEDF00,
      "yellow+ryb" -> 0xFFFEFE33,
      "fuchsia" -> 0xFFFE4164,
      "orange" -> 0xFFFFAE42,
      "brown" ->0xFF964b00,
      "black+warm" -> 0XFF004242,
      "black" -> 0XFF000000,
      "white" -> 0XFFFFFFFF,
      "grey1" -> 0XFFaaaaaa,
      "grey2" -> 0XFF999999,
      "grey3" -> 0XFF888888,
      "grey4" -> 0XFF777777,
      "darkgrey" -> 0XFF777777,
      "lightgrey" -> 0XFFBBBBBB,
      "transparent" -> 0X000000,
      "" -> 0X000000,
    )

  def colourName(colour: Int): String = {
    import Colour._
    var result = "" // f"0X${colour}%08X"
    for { (name, col) <- namedColours if col==colour} result=name

    if (result=="") {
      val HSV(h,s,v) = rgbToHSV(intToRGB(colour))
      //result=f"0X${colour}%08X"
      result=f"hsv(${h.toInt}%d,${s}%1.1f,${v}%1.1f)"
    }
    result
  }

  def apply(specification: String): Brush = try {
    Parse(specification)
  } catch {
    case ex: NonBrush =>
      logging.Default.error(s"${ex.why} [using ${ex.brushState.toString}]")
      ex.printStackTrace()
      ex.brushState
  }

  // The following are used to set the default attributes of unstyled glyphs
  //
  var upFrame: Brush = Brush("black.2.round.fill")
  var downFrame: Brush = Brush("red.2.round.fill")
  var hoverFrame: Brush = Brush("green.2.round.fill")

  var buttonFamily: FontFamily = FontFamily("Menlo")
  var buttonPointSize: Float = 22.0f
  var buttonFace = buttonFamily.normalFace
  def buttonFont = buttonFamily.makeFont(GlyphTypes.FontStyle.NORMAL, buttonPointSize)
  def buttonText(s: String, fg: Brush=buttonForeground, bg: Brush=buttonBackground): Text = Text(s, buttonFont, fg, bg)

  var buttonForeground: Brush = Brush("white.fill")
  var buttonBackground: Brush = Brush("black.fill")
  var buttonDown:       Brush = Brush("red.fill")
  var buttonHover:      Brush = Brush("green.fill")

  /** Default paint for a point: black */
  val point: Brush = Brush("black.1")

}
