package org.sufrin.glyph

object Colour {

  trait Colour {
    def rgb:   RGB
    def hsv:   HSV
    def toInt: Int // max alpha + RGB as an integer

    def argb:  ARGB = ARGB(toInt)
    def hue: Double = hsv.h
    def sat: Double = hsv.s
    def vib: Double = hsv.v
    def hue(h: Double): Colour = HSV(h, sat, vib)
    def sat(s: Double): Colour = HSV(hue, s, vib)
    def vib(v: Double): Colour = HSV(hue, sat, v)
  }

  // values in [0, 1]
  case class RGB(r: Double, g: Double, b: Double) extends Colour {
    def rgb: RGB = this
    def hsv: HSV = rgbToHSV(this)
    def toInt: Int = (0XFF<<24)|rgbToInt(this)
  }

  // h in [0, 360), s,v in [0,1]
  case class HSV(h: Double, s: Double, v: Double) extends Colour {
    def rgb: RGB = hsvToRGB(this)
    def hsv: HSV = this
    def toInt: Int = (0XFF<<24)|rgbToInt(this.rgb)
  }

  case class ARGB(color: Int) extends Colour {
    def rgb: RGB = intToRGB(color)
    def hsv: HSV = rgbToHSV(rgb)
    def toInt: Int =  (0XFF<<24)|rgbToInt(this.rgb)
  }

  def rgbToInt(rgb: RGB): Int = {
    @inline def X(d: Double): Int = ((255*d).toInt) max 0 min 255
    val r = X(rgb.r)
    val g = X(rgb.g)
    val b = X(rgb.b)
    (r << 16) | (g << 8) | b
  }

  def intToRGB(color: Int): RGB = {
    val r = ((color >> 16) & 0xFF) / 255.0
    val g = ((color >> 8) & 0xFF) / 255.0
    val b = (color & 0xFF) / 255.0
    RGB(r, g, b)
  }

  def rgbToHSV(rgb: RGB): HSV = {
    val RGB(r, g, b) = rgb
    val max = r max g max b
    val min = r min g min b
    val delta = max - min

    val h: Double = if (delta == 0) 0.0
    else if (max == r) 60.0 * (((g - b) / delta) % 6)
    else if (max == g) 60.0 * (((b - r) / delta) + 2)
    else               60.0 * (((r - g) / delta) + 4)

    val hue: Double = if (h < 0) h + 360 else h
    val s : Double= if (max == 0) 0.0 else delta / max
    val v: Double = max
    HSV(hue, s, v)
  }

  def hsvToRGB(hsv: HSV): RGB = {
    val HSV(h, s, v) = hsv
    val c: Double = v * s
    val x: Double = c * (1 - math.abs((h / 60) % 2 - 1))
    val m: Double = v - c

    val (rp, gp, bp) =
      if (h < 60.0)  (c, x, 0.0)
    else if (h < 120) (x, c, 0.0)
    else if (h < 180) (0.0, c, x)
    else if (h < 240) (0.0, x, c)
    else if (h < 300) (x, 0.0, c)
    else              (c, 0.0, x)
    RGB(m+rp, m+gp, m+bp)
  }

}
