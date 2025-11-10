package org.sufrin
package glyph

import org.sufrin.glyph.NumberUtils.hexToInt
import org.sufrin.glyph.unstyled.Text
import org.sufrin.glyph.Colour.HSV
import org.sufrin.glyph.Shape.{FILL, STROKE, STROKE_AND_FILL}
import org.sufrin.glyph.GlyphTypes.Scalar


/**
 * A convenience trait that defines several brushes.
 *
 * TODO: these should be systematically named and featured. The range of
 *       widths, and caps is unnecessary.
 */
trait DefaultBrushes {
  import io.github.humbleui.skija.PaintStrokeCap

  def LineBrush(color: String): Brush = Brush(s"$color.2.round.stroke")
  def FrameBrush(color: String): Brush = Brush(s"$color.5.round.stroke")
  val SQUARE     = PaintStrokeCap.SQUARE
  val ROUND      = PaintStrokeCap.ROUND
  val BUTT       = PaintStrokeCap.BUTT
  val red        = Brush("red.0.fill")
  val redLine    = LineBrush("red")
  val redFrame   = FrameBrush("red")
  val pink       = Brush("pink.0.fill")
  val pinkLine   = LineBrush("pink")
  val pinkFrame  = FrameBrush("pink")
  val blue       = Brush("blue.0.fill")
  val blueLine   = LineBrush("blue")
  val blueFrame  = FrameBrush("blue")
  val green      = Brush("green.0.fill")
  val greenLine  = LineBrush("green")
  val greenFrame = FrameBrush("green")
  val white      = Brush("white.0.fill")
  val whiteLine  = LineBrush("white")
  val whiteFrame = FrameBrush("white")
  val black      = Brush("black.0.fill")
  val blackLine  = LineBrush("black")
  val blackFrame = FrameBrush("black")
  val transparent= Brush("transparent.0")
  val invisible  = Brush("transparent.1")
  val lightGrey  = Brush("lightGrey")
  val darkGrey   = Brush("darkGrey")
  val grey1      = Brush(s"grey1")
  val grey2      = Brush(s"grey2")
  val grey3      = Brush(s"grey3")
  val brown      = Brush("brown.0")
  val brownLine  = LineBrush("brown")
  val brownFrame = FrameBrush("brown")
  val yellow     = Brush("yellow")
  val yellowLine = LineBrush("yellow")
  val yellowFrame= FrameBrush("yellow")
  val yellowHuge = Brush("yellow.75.round.stroke")
}




/**
 * Concrete definitions of a variety of brushes, and implementation of
 * a tiny language for specifying brushes. A `brush.toString` yields
 * a specification that can be parsed to yield that `brush: Brush`.
 *
 * {{{
 *   Brushes(specification): Brush yields a brush as specified
 *   specification ::= named [decoration]*
 *
 *   named         ::= 0Xaarrggbb   // 4 hex bytes: alpha, red, blue, green
 *                 |   hsv(#hue,#saturation,#brightness)
 *                 |   "one of the named colours"
 *
 *   decoration    ::=  #strokewidth)
 *                 |    .rounded(#strokeradius)
 *                 |    .dashed(#on, #off)
 *                 |    .sliced(#sliceLength,#maxdisplacement)
 *                 |    .stroke | .fill | .stroke&fill
 *                 |    .round  | .butt | .square
 *                 |    .alpha(#alpha)
 *                 |    .blurred(#blur)
 * }}}
 */
object Brushes extends DefaultBrushes {

  val brushSpecificationNotation: String =
    """
      |   specification ::= named [decoration]*
      |
      |   named         ::= 0Xaarrggbb   // 4 hex bytes: alpha, red, blue, green
      |                 |   0Xrrggbb     // 3 hex bytes: red, blue, green
      |                 |   hsv(#hue,#saturation,#brightness)
      |                 |   "one of the named colours"
      |
      |   decoration    ::=  #strokewidth)
      |                 |    .rounded(#strokeradius)
      |                 |    .dashed(#on, #off)
      |                 |    .sliced(#sliceLength,#maxdisplacement)
      |                 |    .stroke | .fill | .stroke&fill
      |                 |    .round  | .butt | .square
      |                 |    .alpha(#alpha) // fractional alpha
      |                 |    .blurred(#blur)
      |""".stripMargin

  trait Lex
  case object EOS extends Lex
  case class Id(id: String) extends Lex
  case class ScalarValue(value: Scalar) extends Lex
  case class WithParameters(id: String, params: Seq[Scalar]) extends Lex {
    override def toString: String = s"$id(${params.mkString(",")})"
  }
  case class WithParameter(id: String, param: String) extends Lex {
    override def toString: String = s"$id($param)"
  }
  case class Hexadecimal(color: Int) extends Lex {
    override def toString: String = f"0x$color%6x"
  }

  lazy val Chunks = new scala.util.matching.Regex("(([.]?)(([0-9a-zA-Z&~+-])+([(]([a-z]+)[)]|[(][0-9.,]+[)])?))")

  def Parse(spec: String): Brush = {
    @inline def alphaDefault(i: Int):Int = if ((i&0xFF000000) != 0) i else i|0xFF000000
    def Lex(state: Brush, spec: String): List[Lex] = {
      val explode = Chunks.findAllMatchIn(spec).map(_.group(3)).toList
      explode.map {
        case v if v matches("[0-9]+([.][0-9]*)?") => ScalarValue(v.toFloat)
        case s"0x${hex}" if hex.matches("([0-9a-fA-F])+") => Hexadecimal(alphaDefault(hexToInt(hex)))
        case v if v matches("[a-zA-Z0-9&~+-]+") => Id(v)
        case s"$id($param)" if id.matches ("[a-zA-Z0-9&~+-]+") && param.matches("[a-z]+") =>
           WithParameter(id, param)
        case s"$id($params)" if id.matches ("[a-zA-Z0-9&~+-]+")  =>
          val scalars = params.split(',').toSeq.map(_.toFloat).toList
          WithParameters(id, scalars)
        case other => throw new NonBrush(s"Lexical-error at $other in $spec", state)
      }
  }
  def eval(b: Brush, symbols: List[Lex]): Brush = {
    for { symbol <- symbols }
    symbol match {
      case WithParameters("hsv", List(h,s,v)) =>
        b.color(HSV(h,s,v).toInt)
        
      case WithParameters("hsv", List(h,s)) =>
        b.color(HSV(h,s,1.0).toInt)
        
      case WithParameters("hsv", List(h)) =>
        b.color(HSV(h,1.0,1.0).toInt)
        
      case Hexadecimal(colour) =>
        b.color(colour)
        
      case ScalarValue(width) =>
        b.strokeWidth(width)
        
      case Id("fill") =>
        b.mode(FILL)
        
      case Id("stroke") =>
        b.mode(STROKE)
        
      case Id("stroke&fill") =>
        b.mode(STROKE_AND_FILL)
        
      case Id("round") =>
        b.cap(ROUND)
        
      case Id("butt") =>
        b.cap(BUTT)
        
      case Id("square") =>
        b.cap(SQUARE)
        
      case Id("antialias") =>
        b.antiAlias(true)
        
      case Id("dither") =>
        b.dither(true)
        
      case WithParameters("alpha", List(n)) =>
        b.alpha(n)

      case WithParameters("dashed", List(n)) =>
        b.ComposeEffect.dashed(n, n)
        
      case WithParameters("dashed", List(m, n)) =>
        b.ComposeEffect.dashed(m, n)
        
      case WithParameters("dashed", List(m, n, o, p)) =>
        b.ComposeEffect.dashed(m, n, o, p)
        
      case WithParameters("sliced", List(m, n)) =>
        b.ComposeEffect.sliced(m, n)
        
      case WithParameters("rounded", List(radius)) =>
        b.ComposeEffect.rounded(radius)
        
      case WithParameters("blurred", List(blur)) =>
        b.ComposeEffect.blurred(blur)
        
      case WithParameters("blurred", List(blur, delta)) =>
        b.ComposeEffect.blurred(blur, delta, delta)
        
      case WithParameters("blurred", List(blur, dx, dy)) =>
        b.ComposeEffect.blurred(blur, dx, dy)
        
      case WithParameter("tag", tag) =>
        b.tagged(tag)
        
      case Id(name) =>
        namedColours.get(name) match {
          case Some(colour) =>
            b.color(colour)
            
          case None =>
            throw new NonBrush(s"Brush specification error at $name in $spec", b)
        }

      case other =>
        throw new NonBrush(s"Brush specification error at $other in $spec", b)
    }
    b
  }

    val state = new Brush("")
    eval(state, Lex(state, spec.toLowerCase.replace(" ","")))
  }

  def main(args: Array[String]): Unit = {
    for { arg <- args } println(Brushes(arg))
  }

  case class NonBrush(why: String, brushState: Brush) extends Throwable


  lazy val namedColours: collection.mutable.Map[String, Int] = collection.mutable.LinkedHashMap[String, Int](
      "red" ->  0xFFFF0000,
      "rose" -> 0xFFFE28A2,
      "fuchsia" -> 0xFFFE4164,
      "pink" -> 0xFFFFC0CB,
      "purple" -> 0xFFA020F0,
      "green" ->  0xFF00FF00,
      "aqua" -> 0X7700FFFF,
      "teal" -> 0X77008080,
      "blue" -> 0XFF0000FF,
      "persianblue" -> 0xFF1C39BB,
      "cornflower" -> 0xFF6495ED,
      "periwinkle" -> 0xFFCCCCFF,
      "yellow" -> 0XFFFFFF00,
      "yellow+pantone" -> 0xFFFEDF00,
      "orange" -> 0xFFFFAE42,
      "brown" ->0xFF964b00,
      "black+warm" -> 0XFF004242,
      "black" -> 0XFF000000,
      "white" -> 0XFFFFFFFF,
      "olive" -> 0X77808000,
      "grey1" -> 0XFFaaaaaa,
      "grey2" -> 0XFF999999,
      "grey3" -> 0XFF888888,
      "darkgrey" -> 0XFF777777,
      "lightgrey" -> 0XFFBBBBBB,
      "transparent" -> 0X000000,
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

  /** Default paint for a point: black */
  val point: Brush = Brush("black.1")

  val fallbackBrush: Brush = Brush("black.2", tag="fallbackBrush").sliced(1,1)

  def apply(specification: String): Brush = try {
    Parse(specification)
  } catch {
    case ex: NonBrush =>
      logging.Default.error(s"${ex.why} [parsed ${ex.brushState.toString}]")
      ex.printStackTrace()
      fallbackBrush.copy()
  }
}
