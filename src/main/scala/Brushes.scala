package org.sufrin
package glyph

import org.sufrin.glyph.NumberUtils.hexToInt
import org.sufrin.glyph.unstyled.Text
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}

/**
 * A convenience trait that defines several brushes.
 *
 * TODO: these should be systematically named and featured. The range of
 *       widths, and caps is unnecessary.
 */
trait Brushes {

  import io.github.humbleui.skija.PaintStrokeCap
  val SQUARE     = PaintStrokeCap.SQUARE
  val ROUND      = PaintStrokeCap.ROUND
  val BUTT       = PaintStrokeCap.BUTT
  val red        = Brush("red").color(0xFFee0000).strokeWidth(0f)
  val redLine    = Brush("redLine").color(0xFFee0000).strokeWidth(2.0f).cap(SQUARE)
  val redWide    = Brush("redWide").color(0xFFee0000).strokeWidth(25.0f).cap(ROUND)
  val redFrame   = Brush("redFrame").color(0xFFee0000).strokeWidth(5.0f).cap(ROUND)
  val blue       = Brush("blue").color(0xFF0000ff).strokeWidth(0f)
  val blueLine   = Brush("blueLine").color(0xFF0000ff).strokeWidth(2.0f).cap(SQUARE)
  val blueFrame  = Brush("blueFrame").color(0xFF0000ff).strokeWidth(5.0f).cap(ROUND)
  val green      = Brush("green").color(0xFF00ff00).strokeWidth(0f)
  val greenLine  = Brush("greenLine").color(0xFF00ff00).strokeWidth(2.0f).cap(SQUARE)
  val greenFrame = Brush("greenFrame").color(0xFF00ff00).strokeWidth(5.0f).cap(ROUND)
  val white      = Brush("white")       col 0xFFffffff strokeWidth 0
  val whiteFrame = Brush("whiteFrame")  col 0xFFffffff width 5 cap ROUND
  val black      = Brush("black")         col 0xFF000000 width 0
  val blackLine  = Brush("blackLine")     col 0xFF000000 width 2f cap SQUARE
  val blackFrame = Brush("blackFrame")    col 0xFF000000 width 5f cap ROUND
  val transparent= Brush("transparent") col 0 alpha(0f)
  val invisible  = Brush("invisible") col 0 width 1 alpha(0f)
  val lightGrey  = Brush("lightGrey") col 0xFFbbbbbb width 1
  val darkGrey   = Brush("darkGrey")  col 0xFF777777 width 1
  val grey1      = Brush(s"grey1")(color = 0XFFBBBBBB)
  val grey2      = Brush(s"grey2")(color = 0XFFCDCDCD)
  val grey3      = Brush(s"grey3")(color = 0XFFC5C5C5)
  val grey4      = Brush(s"grey4")(color = 0XFFC2C2C2)
  val yellow     = Brush("yellow").color(0xFFffdd00)
  val yellowLine = Brush("yellow").color(0xFFffdd00).strokeWidth(2).cap(SQUARE)
  val yellowFrame= Brush("yellow").color(0xFFffdd00).strokeWidth(5).cap(SQUARE)
  /** Using the new Brush API */
  val yellowHuge = Brush("yellowHuge")(color=0xFFffdd00, width=75f, cap=ROUND, antiAlias = true)
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
 *   specification ::= basic
 *                 |   specification(radius)                // rounded
 *                 |   specification-on-off                 // dashed
 *                 |   specification~segmentSize~variation  // sliced
 *
 *   basic         ::= named
 *                 |   named/strokewidth
 *                 |   named/strokewidth/strokeCap
 *
 *   strokeCap     ::= ROUND | SQUARE | BUTT | FLAT
 *
 *   named         ::= 0Xaarrggbb   // 4 hex bytes: alpha, red, blue, green
 *                 |   red   | blue  | green | white
 *                 |   grey1 | grey2 | grey3 | grey4
 *                 |   lightgrey | darkgrey | yellow
 *                 |   black     | transparent
 * }}}
 */
object Brushes extends Brushes {


  def withName(specification: String): Brush = {

    def isFloat(s: String):Boolean = s.matches("[0-9]+([.][0-9]+)?")

    def basicBrush(specification: String): Brush =
          specification match {
            case s"$specification.$stroke" if isFloat(stroke) =>
              namedBrush(specification)(width=stroke.toFloat)
            case _ =>
              namedBrush(specification)
          }

    def decoratedBrush(specification: String): Brush = {
      specification match {
        case s"$specification.width($stroke)" if isFloat(stroke) =>
          decoratedBrush(specification)(width=stroke.toFloat)
        case s"$prefix.stroke($width)" if isFloat(width) =>
          decoratedBrush(prefix)(mode=STROKE).width(width.toFloat)
        case s"$prefix.rounded($radius)" if isFloat(radius) =>
          decoratedBrush(prefix).rounded(radius.toFloat)
        case s"$prefix($radius)" if isFloat(radius) =>
          decoratedBrush(prefix).rounded(radius.toFloat)
        case s"$prefix-$on-$off" if isFloat(on) && isFloat(off) =>
          decoratedBrush(prefix).dashed(on.toFloat, off.toFloat)
        case s"$prefix~$seg~$lim" if isFloat(seg) && isFloat(lim) =>
          decoratedBrush(prefix).sliced(seg.toFloat, lim.toFloat)
        case s"$prefix.stroke" =>
          decoratedBrush(prefix)(mode=STROKE)
        case s"$prefix.fill" =>
          decoratedBrush(prefix)(mode=FILL)
        case s"$prefix.round" =>
          decoratedBrush(prefix)(cap=ROUND)
        case s"$prefix.square" =>
          decoratedBrush(prefix)(cap=SQUARE)
        case s"$prefix.butt" =>
          decoratedBrush(prefix)(cap=BUTT)
        case s"$prefix.blur($blur)" if isFloat(blur) =>
          decoratedBrush(prefix).blurred(blur.toFloat, 0f, 0f)
        case _ =>
          basicBrush(specification)
      }
    }

    def namedBrush(specification: String): Brush = specification.toLowerCase match {
      case "red" => Brush(s"red")(color = 0XFFFF0000)
      case "blue" => Brush(s"blue")(color = 0XFF0000FF)
      case "green" => Brush(s"green")(color = 0XFF00FF00)
      case "white" => Brush(s"white")(color = 0XFFFFFFFF)
      case "grey1" => Brush(s"grey1")(color = 0XFFBBBBBB)
      case "grey2" => Brush(s"grey2")(color = 0XFFCDCDCD)
      case "grey3" => Brush(s"grey3")(color = 0XFFC5C5C5)
      case "grey4" => Brush(s"grey4")(color = 0XFFC2C2C2)
      case "lightgrey" => Brush(s"lightgrey")(color = 0XFFBBBBBB)
      case "darkgrey" => Brush(s"darkgrey")(color = 0XFF777777)
      case "black" => Brush(s"black")(color = 0XFF000000)
      case "yellow" => Brush(s"yellow")(color = 0XFFFFDD00)
      case "transparent" => Brush(s"transparent")(color = 0X00000000, alpha=0f)
      case "" => Brush(s"transparent")(color = 0X00000000, alpha=0f)
      case s"0x${hex}" if hex.matches("([0-9a-f])+") =>
        Brush(s"0X$hex")(color = hexToInt(hex))
      case specification =>
        logging.Default.warn(s"$specification is not the specification of a colour")
        Brush(s"red", s"[because invalid $specification]")(width=2, mode=STROKE)
    }

    decoratedBrush(specification)

  }

  def apply(specification: String): Brush = withName(specification)

  // The following are used to set the default attributes of unstyled glyphs
  //
  var upFrame: Brush = Brush("Brushes.upFrame")       color 0xFF000000 strokeWidth 2f strokeCap ROUND
  var downFrame: Brush = Brush("Brushes.downFrame")   color 0xFFFF0000 strokeWidth 2f strokeCap ROUND
  var hoverFrame: Brush = Brush("Brushes.hoverFrame") color 0xFF00FF00 strokeWidth 2f strokeCap ROUND

  var buttonFamily: FontFamily = FontFamily("Menlo")
  var buttonPointSize: Float = 22.0f
  var buttonFace = buttonFamily.normalFace
  def buttonFont = buttonFamily.makeFont(GlyphTypes.FontStyle.NORMAL, buttonPointSize)
  def buttonText(s: String, fg: Brush=buttonForeground, bg: Brush=buttonBackground): Text = Text(s, buttonFont, fg, bg)

  var buttonForeground: Brush = Brush("Brushes.buttonForeground") color 0xFF000000 strokeWidth 1.0f
  var buttonBackground: Brush = Brush("Brushes.buttonBackground") color 0xFFFFFFFF strokeWidth 1.0f
  var buttonDown:       Brush = Brush("Brushes.buttonDown") color 0xFFFF0000 strokeWidth 1.0f
  var buttonHover:      Brush = Brush("Brushes.buttonHover") color 0xFF00FF00 strokeWidth 1.0f

  /** Default paint for a point: black */
  val point: Brush = Brush("Brushes.point") color 0xFF000000 strokeWidth 1.0f

}
