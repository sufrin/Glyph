package org.sufrin.glyph
package unstyled
package dynamic

import GlyphTypes.{Font, Scalar}
import gesture._
import Modifiers.{Bitmap, Command, Control, Pressed, Primary, Secondary, Shift}

import io.github.humbleui.jwm.Key

import scala.collection.mutable

class SeqViewer(cols: Int, rows: Int, font: Font, override val fg: Brush, override val bg: Brush,
                val selBrush: Brush,
                initially: => Seq[String],
                autoScale: Boolean = true
               )  extends GestureBasedReactiveGlyph { thisViewer =>

  def onClick(mods: Bitmap, selected: Int): Unit = {}
  def onKeystroke(keystroke: Gesture): Unit = {}
  def onHover(mods: Bitmap, hovered: Int): Unit = {}


  val metrics = font.getMetrics
  val descent = metrics.getDescent
  val charW   = Text("m", font).width // getAvgCharWidth/2
  val charH   = metrics.getHeight+descent
  val bell    = Sound.Clip("WAV/glass.wav")

  var scale: Scalar = 1.0f

  def toForegroundBrush(i: Int): Brush = if (selectedRows.contains(i)) selBrush else fg
  def isUnderlined(i: Int): Boolean = true

  var seq: Seq[String] = initially

  def refresh(current: Seq[String]): Unit = {
    seq = current
    reDraw()
  }

  def current(i: Int): String = seq(i)

  var rowOrigin, colOrigin = 0
  var hovered = 0
  def currentMinRow = if (selectedRows.isEmpty) 0 else selectedRows.min
  def currentMaxRow = if (selectedRows.isEmpty) 0 else selectedRows.max
  def currentRow    = if (selectedRows.isEmpty) 0 else selectedRows.max
  val selectedRows  = mutable.HashSet[Int]()


  def setCurrentRow(row: Int): Unit = {
    selectedRows.clear()
    if (row<seq.length) selectedRows.add(row)
  }

  def clearSelection(): Unit = { selectedRows.clear(); reDraw() }

  val margin: Scalar = 10
  val rowsDiagonal: Vec = Vec(charW*cols, charH*rows+descent)
  val diagonal: Vec     = rowsDiagonal+Vec(margin, margin)

  def yToRow(y: Scalar): Int = {
    val r = ((y-margin/2) / charH).toInt
    r
  }

  /**
   * Compute the scale needed to fit the entire first row; then display at that scale
   */
  def draw(surface: Surface): Unit = {
    val scaledH =
    if (autoScale&&seq.nonEmpty&&seq(0).length>cols) {
      scale = (cols.toDouble/seq(0).length.toDouble).toFloat
      rowsDiagonal.y/scale
    } else {
      scale=1
      h
    }
    drawBackground(surface)
    surface.withClip(diagonal) {
      // margins
      val offset=margin/2
      if (rowOrigin==0) surface.drawLines$(marginBrush, offset,2, w-offset,2) else surface.drawLines$(outsideBrush, offset,2, w-offset,2)
      if (rowOrigin+rows>=seq.length) surface.drawLines$(marginBrush, offset,h-offset, w-offset,h-offset) else surface.drawLines$(outsideBrush, offset,h-offset, w-offset,h-offset)
      surface.drawLines$(marginBrush, offset, offset, offset, h-offset)
      surface.drawLines$(marginBrush, offset+rowsDiagonal.x, offset, offset+rowsDiagonal.x, h-offset)
      // draw the rows
      surface.withScale(scale) {
      surface.declareCurrentTransform(this)
        // the rows
        var row = rowOrigin
        var lastRow = seq.length
        var y: Scalar = charH - descent
        while (row < lastRow && y <= scaledH) {
          val string = seq(row).substring(colOrigin) //
          val text = io.github.humbleui.skija.TextLine.make(string, font) // todo: cache the entire TextLine
          val width = text.getWidth
          surface.drawTextLine(toForegroundBrush(row), text, margin, y)
          if (isUnderlined(row)) surface.drawLines$(fg, margin, y + descent, width, y + descent)
          row += 1
          y += charH
        }
      }
    }
  }

  def copy(fg: Brush=fg, bg: Brush=bg): ReactiveGlyph =
      new SeqViewer(cols, rows, font, fg, bg, selBrush, seq, autoScale)

  var clicks: Int = 0

  lazy val marginBrush: Brush = Brushes.red(alpha=0.85f)
  lazy val outsideBrush: Brush = marginBrush(width=2).dashed(5,5)
  def isVisible(row: Int): Boolean = (rowOrigin<=row) && (row-rowOrigin<rows)


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
      case _: MouseEnters =>
        guiRoot.grabKeyboard(thisViewer)
        reDraw()
      case _: MouseLeaves =>
        hovered = -1
        guiRoot.freeKeyboard(completely = true)
        refresh(initially)

      case _: MouseScroll if !CONTROL =>
        val nextRowOrigin = (rowOrigin+(if (delta.x+delta.y > 0f) -1 else 1)) max 0
        if (nextRowOrigin+rows<=seq.length) rowOrigin=nextRowOrigin

      case _: MouseScroll if CONTROL  => colOrigin = (colOrigin+(if (delta.x+delta.y > 0f) -1 else 1)) max 0

      case Keystroke(key, _) if !PRESSED =>

      case Keystroke(key, _) if PRESSED =>
        key match {

          case Key.DOWN =>
            setCurrentRow(currentMaxRow + 1)
            if (currentMaxRow-rowOrigin>=rows-1) {
              rowOrigin += 1
              reDraw()
            }

          case Key.UP =>
            setCurrentRow(0 max (currentMinRow-1))
            if (currentMinRow<rowOrigin) {
              rowOrigin = currentMinRow
              reDraw()
            }

          case Key.HOME =>
            setCurrentRow(0)
            rowOrigin = 0
            reDraw()

          case Key.END =>
            setCurrentRow(seq.length-1)
            if (currentRow-rowOrigin>=rows-2) {
              rowOrigin = seq.length-rows
            }
            reDraw()

          case Key.PAGE_DOWN =>
            if (rowOrigin+rows-1<seq.length) {
              rowOrigin += rows-2
              setCurrentRow(rowOrigin)
              reDraw()
            }

          case Key.PAGE_UP =>
            rowOrigin = 0 max rowOrigin-rows
            setCurrentRow(rowOrigin)
            reDraw()

          case Key.RIGHT | Key.ENTER =>
            val currentRow=currentMinRow
            if (currentMaxRow==currentRow && isVisible(currentRow)) onClick(mods, currentRow) else bell.play()

          // ignore shift buttons
          case Key.SHIFT | Key.CONTROL | Key.CAPS_LOCK | Key.ALT | Key.MAC_COMMAND | Key.MAC_FN | Key.MAC_OPTION =>

          case Key.W if CONTROL =>
               onKeystroke(gesture.asInstanceOf[Keystroke])

          case _ =>
            bell.play()
        }

      case MouseMove(modifiers) if !PRESSED =>
        val row = yToRow(location.y) + rowOrigin
        val oldHovered = hovered
        hovered = row min seq.length
        if (oldHovered!=hovered) onHover(mods, hovered)

      case MouseMove(modifiers) if PRESSED && SHIFT =>
        val row = yToRow(location.y) + rowOrigin
        hovered = row min seq.length
        if (row<seq.length) selectedRows.add(hovered)
        reDraw()

      case MouseMove(modifiers) if PRESSED && CONTROL =>
        val row = yToRow(location.y) + rowOrigin
        val oldHovered = hovered
        hovered = row min seq.length
        if (row<seq.length && hovered!=oldHovered)  {
          if (selectedRows.contains(hovered)) selectedRows.remove(hovered) else selectedRows.add(hovered)
        }
        reDraw()


      case MouseClick(_)  if (SECONDARY) =>
        val row = yToRow(location.y) + rowOrigin
        val hovered = row min seq.length
        if (row<seq.length)
           if (selectedRows.contains(hovered)) selectedRows.remove(hovered) else selectedRows.add(hovered)
        else
           selectedRows.clear()
        reDraw()


      case MouseClick(_) if PRIMARY && SHIFT =>
        val row = yToRow(location.y) + rowOrigin
        val hovered = row min seq.length-1
        selectedRows.add(hovered)
        val top = selectedRows.min min hovered
        val bot = selectedRows.max max hovered
        selectedRows.addAll(top to bot)
        reDraw()

      case MouseClick(_) if PRESSED =>
        if (location.x<=margin) {
          selectedRows.clear()
        } else {
          val row = yToRow(location.y) + rowOrigin
          val hovered = row min seq.length
          if (selectedRows.contains(row)) {
            setCurrentRow(row)
            onClick(mods, currentRow)
          } else {
            selectedRows.clear()
            if (row < seq.length) selectedRows.add(hovered)
          }
        }
        reDraw()


      case _ =>
    }
    //println(displayList)
    //println(selection)
    reDraw()
  }

}
