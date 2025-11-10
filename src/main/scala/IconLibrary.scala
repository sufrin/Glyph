package org.sufrin.glyph

import Brushes.{blue, white}
import GlyphTypes.Scalar

import io.github.humbleui.skija.PaintMode

object IconLibrary {

  /** An 18x15 settings icon */
  def SETTINGS(brush: Brush = Brushes.darkGrey): Glyph = {
    import Brushes.{transparent => T}
    import Shape._
    val gap: Shape = rect(18, 3)(T)
    val plank: Shape = rect(18, 3)(brush)
    (plank --- gap --- plank --- gap --- plank).asGlyph
  }

  /** A settings icon suitable for use in inline text set using the given (implicit) `style` */
  def SETTINGSSYMBOL(brush: Brush = Brushes.darkGrey)(implicit style: StyleSheet): Glyph = {
    val raw = SETTINGS(brush)
    raw.scaled(style.exHeight / raw.h).withBaseline(baseLine = style.textFontSize, height = style.exHeight, offset =
      style.textFontSize / 3)
  }

  lazy val redFill = Brushes.red(mode = PaintMode.FILL)
  lazy val brownFill = Brushes.brown(mode = PaintMode.FILL)
  lazy val crossBrush = Brushes.white

  def HIDE(diam: Scalar = 28f): Glyph = {
    import Shape._
    val whiteLine = white(width = diam / 8f)
    val halfDiam: Float = diam / 2
    val crossBar = line(-halfDiam, 0f, halfDiam, 0f)(whiteLine)
    superimposed(List(oval(diam, diam)(brownFill), crossBar)).asGlyph
  }

  def CLOSE(diam: Scalar = 28f): Glyph = if (true) CROSS45(diam) else {
    import Shape._
    val whiteLine = blue(width = diam / 8f)
    val halfDiam: Float = (Math.sin(Math.PI * 0.25) * diam / 2).toFloat
    val crossBar1 = line(-halfDiam, -halfDiam, halfDiam, halfDiam)(whiteLine)
    val crossBar2 = line(-halfDiam, halfDiam, halfDiam, -halfDiam)(whiteLine)
    superimposed(List(oval(diam, diam)(redFill), crossBar1, crossBar2)).asGlyph
  }

  def CROSS45(diam: Scalar = 28f, down: Brush = crossBrush, up: Brush = crossBrush, fill: Brush = redFill): Glyph = {
    import Shape._
    val halfDiam: Float = (Math.sin(Math.PI * 0.25) * diam / 2).toFloat
    val downBar: Shape = line(-halfDiam, -halfDiam, halfDiam, halfDiam)(down(width = diam / 8f))
    val upBar: Shape = line(-halfDiam, halfDiam, halfDiam, -halfDiam)(up(width = diam / 8f))
    superimposed(List(oval(diam, diam)(fill), downBar, upBar)).asGlyph
  }

  def CROSS(diam: Scalar = 28f, ns: Brush = crossBrush, ew: Brush = crossBrush, fill: Brush = redFill): Glyph = {
    import Shape._
    val halfDiam: Float = diam / 2
    val nsBar: Shape = line(0, -halfDiam, 0, halfDiam)(ns(width = diam / 8f))
    val ewBar: Shape = line(-halfDiam, 0, halfDiam, 0)(ew(width = diam / 8f))
    superimposed(List(oval(diam, diam)(fill), nsBar, ewBar)).asGlyph
  }

}