package org.sufrin.glyph
package unstyled
package dynamic

import GlyphTypes.{Font, Scalar}
import gesture.{Gesture, GestureBasedReactiveGlyph, Keystroke, MouseClick, MouseEnters, MouseLeaves, MouseMove, MouseScroll}
import Modifiers.{Bitmap, Command, Control, Pressed, Primary, Secondary, Shift}

import io.github.humbleui.jwm.Key
import org.sufrin.glyph.unstyled.reactive.Reaction

class SeqViewer(cols: Int, rows: Int, font: Font, override val fg: Brush, override val bg: Brush,
                val selBrush: Brush,
                val seq: Seq[String])(
                reaction: (Int, Bitmap)=>Unit) extends GestureBasedReactiveGlyph { thisViewer =>
  val metrics = font.getMetrics
  val charW   = metrics.getMaxCharWidth
  val charH   = metrics.getHeight+metrics.getDescent
  val descent = metrics.getDescent
  val bell    = Sound.Clip("WAV/glass.wav")


  def toForegroundBrush(i: Int): Brush = if (i==current) selBrush else fg
  def isUnderlined(i: Int): Boolean = true

  var rowOrigin, colOrigin = 0
  var current = 0
  val diagonal: Vec = Vec(charW*cols, charH*rows)

  def yToRow(y: Scalar): Int = {
    val r = (y / charH).toInt
    r
  }

  /**
   * Draw the glyph on the surface at its given size (as if at the origin).
   */
  def draw(surface: Surface): Unit = {
    drawBackground(surface)
    surface.declareCurrentTransform(this)
    surface.withClip(diagonal) {
      var row     = rowOrigin
      var lastRow = seq.length
      var y: Scalar = charH-descent
      while (row<lastRow && y+charH<h) {
        val string = seq(row).substring(colOrigin)                      //
        val text = io.github.humbleui.skija.TextLine.make(string, font) // todo: cache the entire TextLine
        surface.drawTextLine(toForegroundBrush(row), text, 0, y)
        if (isUnderlined(row)) surface.drawLines$(fg, 0, y+descent, w, y+descent)
        row += 1
        y   += charH
      }
    }
  }

  def copy(fg: Brush=fg, bg: Brush=bg): ReactiveGlyph =
      new SeqViewer(cols, rows, font, fg, bg, selBrush, seq)(reaction)


  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    //println(gesture, location, delta)
    val mods: Bitmap  = gesture.modifiers
    val PRESSED       = mods.includeAll(Pressed)
    val CONTROL       = mods.includeSome(Command|Control)
    val PRIMARY       = mods.includeAll(Primary   | Pressed)
    val SECONDARY     = mods.includeAll(Secondary | Pressed) || (PRIMARY && CONTROL)
    val COMPLEMENT    = mods.includeSome(Shift)
    val SHIFT         = mods.includeSome(Shift)
    gesture match {
      case _: MouseEnters => guiRoot.grabKeyboard(thisViewer)
      case _: MouseLeaves => guiRoot.freeKeyboard(completely = true)

      case _: MouseScroll if !CONTROL => rowOrigin = (rowOrigin+(if (delta.x+delta.y > 0f) -1 else 1)) max 0
      case _: MouseScroll if CONTROL  => colOrigin = (colOrigin+(if (delta.x+delta.y > 0f) -1 else 1)) max 0

      case Keystroke(key, _) if !PRESSED =>

      case Keystroke(key, _) if PRESSED =>
        key match {

          case Key.DOWN =>
            current += 1
            println(current, current-rowOrigin, rows)
            if (current-rowOrigin>=rows-1) {
              rowOrigin += 1
              reDraw()
            }

          case Key.UP =>
            current = 0 max (current-1)
            if (current<rowOrigin) {
              rowOrigin = current
              reDraw()
            }
          // ignore shift buttons
          case Key.SHIFT | Key.CONTROL | Key.CAPS_LOCK | Key.ALT | Key.MAC_COMMAND | Key.MAC_FN | Key.MAC_OPTION =>

          case _ =>
            bell.play()
        }

      case MouseClick(_)  if (PRESSED && COMPLEMENT) =>

      case MouseClick(_) if PRESSED =>
        val row = yToRow(location.y) + rowOrigin
        current = row min seq.length-1
        reaction(current, mods)

      case _ =>
    }
    //println(displayList)
    //println(selection)
    reDraw()
  }

}
