package org.sufrin.glyph

import unstyled.static


object PolygonLibrary {
  import Brushes._
  import GlyphTypes.Scalar
  import NaturalSize.Col

  import static._

  import scala.collection.mutable.ArrayBuffer


  def closeButtonGlyph: Glyph = IconLibrary.CLOSE(14f)

  def hideButtonGlyph: Glyph =  IconLibrary.HIDE(14f)

  val PalestinianFlag: Glyph = {
    val white = Brush("white").color(0xFFffffff)
    val black = Brush("black").color(0xFF000000)
    val green = Brush("green").color(0xFF00aa00)
    val red   = Brush("red").color(0xFFee0000)

    val triangle = FilledPolygon(400, 240, red)(
      (0, 0), (0, 240), (160, 120)
    )
    val flag =
    Envelope(bg=white)(
      Col(FilledRect(450, 80, fg=black),
        FilledRect(450, 80, white),
        FilledRect(450, 80, green)),
      triangle
    )
    flag
  }

  def steppedGrid(diagonal: Vec, step: Scalar, fg: Brush): Glyph = {
    import scala.collection.mutable
    val h = diagonal.x max diagonal.y
    val w = h
    val vertices = new mutable.ListBuffer[Vec]
    var y = 0f
    while (y < h) {
      vertices += Vec(0, y)
      vertices += Vec(w, y)
      y += step
      vertices += Vec(w, y)
      vertices += Vec(0, y)
      y += step
    }
    var x = 0f
    while (x < h) {
      vertices += Vec(x, 0)
      vertices += Vec(x, h)
      x += step
      vertices += Vec(x, h)
      vertices += Vec(x, 0)
      x += step
    }
    val lines = Polygon(diagonal, fg = fg, bg = transparent)(vertices)
    lines // Concentric(lines, lines().rotated(1))
  }

  def grid(diagonal: Vec, unit: Scalar = 10f): Glyph =
      Concentric(steppedGrid(diagonal, unit, blue(alpha = 0.3f, width = 1f)),
                 steppedGrid(diagonal, 10*unit, red(alpha = 0.3f, width = 1f)))


  val star7Path =  regularStarPath(7)

  val PI     = Math.PI
  val `2PI`  = PI*2.0
  val `PI/2` = PI*0.5

  /** Centered at `(C,C)`  */
  def regularStarPath(n: Int, C: Scalar = 128.0f, R: Scalar = 115f): Seq[(Scalar, Scalar)] = {
    val star = new ArrayBuffer[(Scalar, Scalar)]
    val theta = PI - PI/n
    star += (((C + R, C)))
    for {i <- 0 until n} {
      val a = theta * i
      star += (((C + R * Math.cos(a)).toFloat, (C + R * Math.sin(a)).toFloat))
    }
    star += ((C + R, C))
    star.toSeq
  }

  /** Centered at `(C,C)`  */
  def regularPolygonPath(n: Int, C: Scalar = 128.0f, R: Scalar = 115f): Seq[(Scalar, Scalar)] = {
    val star = new ArrayBuffer[(Scalar, Scalar)]
    val theta = `2PI` / n
    star += ((C + R, C))
    for {i <- 0 until n} {
      val a = theta * i
      star += ((C + R * Math.cos(a).toFloat, C + R * Math.sin(a).toFloat))
    }
    star += ((C + R, C))
    star.toSeq
  }

  def regularPolygon(n: Int, C: Scalar = 128.0f, R: Scalar = 115f, fg: Brush = blue, bg: Brush = transparent): Glyph =
    Polygon.apply(2*C, 2*C, fg, bg)(regularPolygonPath(n, C, R))

  def filledRegularPolygon(n: Int, C: Scalar = 128.0f, R: Scalar = 115f, fg: Brush = blue, bg: Brush = transparent): Glyph =
    FilledPolygon(2*C, 2*C, fg, bg)(regularPolygonPath(n, C, R))

  def star7(fg: Brush = blue, bg: Brush = transparent, C: Scalar = 128.0f, R: Scalar = 115f): Glyph = openStargon(7, fg, bg, C, R)

  def filledStargon(n: Int, fg: Brush = blue, bg: Brush = transparent, C: Scalar = 128.0f, R: Scalar = 115f): Glyph =
    FilledPolygon(2*C, 2*C, fg, bg)(regularStarPath(n, C, R))

  def openStargon(n: Int, fg: Brush = blue, bg: Brush = transparent, C: Scalar = 128.0f, R: Scalar = 115f): Glyph =
    Polygon(2*C, 2*C, fg, bg)(regularStarPath(n, C, R))

}

