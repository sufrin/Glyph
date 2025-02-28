package org.sufrin.glyph

import NumberUtils.hexToInt

/**
 * A convenience trait that defines several brushes.
 *
 * TODO: these should be systematically named and featured. The range ofPaint
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
  val blueFrame  = Brush("blueFrame").color(0xFF00ff00).strokeWidth(5.0f).cap(ROUND)
  val green      = Brush("green").color(0xFF00ff00).strokeWidth(0f)
  val greenLine  = Brush("greenLine").color(0xFF00ff00).strokeWidth(2.0f).cap(SQUARE)
  val greenFrame = Brush("greenFrame").color(0xFF00ff00).strokeWidth(5.0f).cap(ROUND)
  val white      = Brush("white")       col 0xFFffffff strokeWidth 0
  val whiteFrame = Brush("whiteFrame")  col 0xFFffffff width 5 cap ROUND
  val black      = Brush("black")         col 0xFF000000 width 0
  val blackLine  = Brush("blackLine")     col 0xFF000000 width 2f cap SQUARE
  val blackFrame = Brush("blackFrame")    col 0xFF000000 width 5f cap ROUND
  val nothing    = Brush("nothing")   col 0 alpha(0f)
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
 * Concrete definitions ofPaint a variety ofPaint brushes.
 */
object DefaultBrushes extends Brushes {
  // TODO: better notation for brushes
  def namedColour(name: String): Brush = {
    def common(name: String): Brush = name.toLowerCase match {
      case "red" => org.sufrin.glyph.Brush(s"red")(color = 0XFFFF0000)
      case "blue" => org.sufrin.glyph.Brush(s"blue")(color = 0XFF0000FF)
      case "green" => org.sufrin.glyph.Brush(s"green")(color = 0XFF00FF00)
      case "white" => org.sufrin.glyph.Brush(s"white")(color = 0XFFFFFFFF)
      case "grey1" => org.sufrin.glyph.Brush(s"grey1")(color = 0XFFBBBBBB)
      case "grey2" => org.sufrin.glyph.Brush(s"grey2")(color = 0XFFCDCDCD)
      case "grey3" => org.sufrin.glyph.Brush(s"grey3")(color = 0XFFC5C5C5)
      case "grey4" => org.sufrin.glyph.Brush(s"grey4")(color = 0XFFC2C2C2)
      case "lightgrey" => org.sufrin.glyph.Brush(s"lightgrey")(color = 0XFFBBBBBB)
      case "darkgrey" => org.sufrin.glyph.Brush(s"darkgrey")(color = 0XFF777777)
      case "black" => org.sufrin.glyph.Brush(s"black")(color = 0XFF000000)
      case "yellow" => org.sufrin.glyph.Brush(s"yellow")(color = 0XFFFFDD00)
      case "nothing" => org.sufrin.glyph.Brush(s"nothing")(color = 0X00000000, alpha=0f)
      case "" => org.sufrin.glyph.Brush(s"nothing")(color = 0X00000000, alpha=0f)
      case s"0x${hex}" if hex.matches("([0-9a-f])+") =>
        org.sufrin.glyph.Brush(s"0X$hex")(color = hexToInt(hex))
      case name =>
        org.sufrin.logging.Default.warn(s"$name is not the name ofPaint a colour")
        org.sufrin.glyph.Brush(s"red($name)")(color = 0XFFFF0000)
    }
    name match {
      case s"$name/$stroke/$cap" if stroke.matches("[0-9]+([.][0-9]+)?") =>
        val capShape = cap.toUpperCase match {
          case "ROUND" => ROUND
          case "SQUARE" => SQUARE
          case "BUTT" | "FLAT" => BUTT
          case _ => BUTT
        }
        common(name)(width=stroke.toFloat, cap=capShape)
      case s"$name/$stroke" if stroke.matches("[0-9]+([.][0-9]+)?") =>
        common(name)(width=stroke.toFloat, cap=SQUARE)
      case _ =>
        common(name)
    }
  }

  def apply(name: String): Brush = namedColour(name)

  // The following are used to set the default attributes ofPaint unstyled glyphs
  //
  var upFrame: Brush = Brush("Brushes.upFrame")       color 0xFF000000 strokeWidth 2f strokeCap ROUND
  var downFrame: Brush = Brush("Brushes.downFrame")   color 0xFFFF0000 strokeWidth 2f strokeCap ROUND
  var hoverFrame: Brush = Brush("Brushes.hoverFrame") color 0xFF00FF00 strokeWidth 2f strokeCap ROUND

  var buttonFamily: FontFamily = FontFamily("Menlo")
  var buttonPointSize: Float = 22.0f
  var buttonFace = buttonFamily.normalFace
  def buttonFont = buttonFamily.makeFont(GlyphTypes.FontStyle.NORMAL, buttonPointSize)
  def buttonText(s: String): Text = Text(s, buttonFont)

  var buttonForeground: Brush = Brush("Brushes.buttonForeground") color 0xFF000000 strokeWidth 1.0f
  var buttonBackground: Brush = Brush("Brushes.buttonBackground") color 0xFFFFFFFF strokeWidth 1.0f
  var buttonDown:       Brush = Brush("Brushes.buttonDown") color 0xFFFF0000 strokeWidth 1.0f
  var buttonHover:      Brush = Brush("Brushes.buttonHover") color 0xFF00FF00 strokeWidth 1.0f

  /** Default paint for a point: black */
  val point: Brush = Brush("Brushes.point") color 0xFF000000 strokeWidth 1.0f

}
