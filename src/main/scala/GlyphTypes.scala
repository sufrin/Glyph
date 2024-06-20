package org.sufrin.glyph

/**
 * One-stop package of things frequently inherited from Skija or mandated by it.
 */
object GlyphTypes {
  type Paint       = io.github.humbleui.skija.Paint
  type Font        = io.github.humbleui.skija.Font
  type Typeface    = io.github.humbleui.skija.Typeface
  type PathEffect  = io.github.humbleui.skija.PathEffect
  type ImageFilter = io.github.humbleui.skija.ImageFilter
  type Window      = io.github.humbleui.jwm.Window

  object FontManager {
    import io.github.humbleui.skija.FontMgr
    lazy val default = FontMgr.getDefault
  }

  object Font {
    def apply(face: Typeface, size: Scalar): Font = new Font(face, size)
  }

  object FontStyle {
    val BOLD = io.github.humbleui.skija.FontStyle.BOLD
    val ITALIC = io.github.humbleui.skija.FontStyle.ITALIC
    val NORMAL = io.github.humbleui.skija.FontStyle.NORMAL
    val BOLD_ITALIC = io.github.humbleui.skija.FontStyle.BOLD_ITALIC
  }

  object PathEffect {
    def makeDiscrete(sliceLength: Scalar, displacementLimit: Scalar, seed: Int): PathEffect =
        io.github.humbleui.skija.PathEffect.makeDiscrete(sliceLength, displacementLimit, seed)

    def makeDash(v: Scalar)(floats: Scalar*): PathEffect =
        io.github.humbleui.skija.PathEffect.makeDash(floats.toArray, v)
  }

  object ImageFilter {
    def makeDropShadow(dx: Scalar, dy: Scalar, blur: Scalar, colour: Int): ImageFilter =
        io.github.humbleui.skija.ImageFilter.makeDropShadowOnly(dx, dy, blur / 2f, blur / 2f, colour)
  }

  type Scalar  = Float
  type Scale   = Float
  type Degrees = Float

  type ButtonState = Modifiers.Bitmap
  type Pixel   = Int              // Physical units
  type Pixels  = (Pixel, Pixel)   // Physical Units

  implicit class Px(val pair: (Int, Int)) extends AnyVal {
    def px: Int=pair._1
    def py: Int=pair._2
    def scaled(scale: Scale): Px = new Px(((scale*px).toInt, (scale*py).toInt))
    def +(other:Px):Px = new Px(px+other.px, py+other.py)
    def -(other:Px):Px = new Px(px-other.px, py-other.py)
    def toVec:Vec = Vec(px, py)
    override def toString = s"Px($pair)"
  }
}

