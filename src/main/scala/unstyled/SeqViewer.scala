package org.sufrin.glyph
package unstyled
package dynamic

import GlyphTypes.{Font, Scalar}
import gesture._
import Modifiers.Bitmap

import io.github.humbleui.jwm.Key
import io.github.humbleui.skija.TextLine

import scala.collection.mutable

class SeqViewer(cols: Int, rows: Int, font: Font, override val fg: Brush, override val bg: Brush,
                val selBrush: Brush,
                initially: => Seq[String],
                autoScale: Boolean = true,
                header: => Seq[String],
                headBrush: Brush = Brushes.black
               )  extends GestureBasedReactiveGlyph { thisViewer =>

  def onDoubleClick(mods: Bitmap, selected: Int): Unit = {}
  def onHover(mods: Bitmap, hovered: Int): Unit = {}
  def onOther(gesture: Gesture): Unit = { println(s"other($gesture)")}
  def underlineBrush: Brush = fg


  val metrics = font.getMetrics
  val descent = metrics.getDescent
  val charW   = Text("m", font).width // getAvgCharWidth/2
  val charH   = metrics.getHeight+descent
  val bell    = Sound.Clip("WAV/glass.wav")
  val shadeHeight = metrics.getHeight

  var scale: Scalar = 1.0f

  def toForegroundBrush(i: Int): Brush = if (_selectedRows.contains(i)) selBrush else fg
  def isUnderlined(i: Int): Boolean = true

  var seq: Seq[String]     = initially
  var heading: Seq[String] = header

  val margin: Scalar = 10
  val rowsDiagonal:   Vec = Vec(charW*cols, charH*rows+descent)
  val headerDiagonal: Vec = Vec(charW*cols, charH*heading.length+descent)
  val diagonal: Vec       = rowsDiagonal+Vec(margin, margin)+Vec(0, headerDiagonal.y)

  def refresh(current: Seq[String]=initially, reset: Boolean = false): Unit = {
    seq     = current
    heading = header
    if (cacheing) {
      textCache.clear()
      headCache.clear()
    }
    if (reset) {
      rowOrigin=0
      clearSelection()
    }
    reDraw()
  }

  def current(i: Int): String = seq(i)

  var rowOrigin, colOrigin = 0
  var hovered = 0
  def currentMinRow = if (_selectedRows.isEmpty) 0 else _selectedRows.min
  def currentMaxRow = if (_selectedRows.isEmpty) 0 else _selectedRows.max
  def currentRow    = if (_selectedRows.isEmpty) 0 else _selectedRows.max
  val _selectedRows  = mutable.HashSet[Int]()


  def selectedRows: Seq[Int] = _selectedRows.toSeq.sorted

  def setCurrentRow(row: Int): Unit = {
    _selectedRows.clear()
    if (row<seq.length) _selectedRows.add(row)
  }

  def addToSelection(row: Int): Unit = {
    if (row<seq.length) _selectedRows.add(row)
  }

  def clearSelection(): Unit = { _selectedRows.clear(); reDraw() }



  def yToRow(y: Scalar): Int = {
    val r = 0 max (((y-margin/2) / charH).toInt - header.length)
    r
  }

  private val cacheing = true
  private val textCache = mutable.HashMap.newBuilder[Int, io.github.humbleui.skija.TextLine](2*rows, 1.5).result()
  private val headCache = mutable.HashMap.newBuilder[Int, io.github.humbleui.skija.TextLine](1, 2).result()

  val stripeBrush: Brush = Brushes.lightGrey.alpha(0.2f)

  /**
   * Compute the scale needed to fit the entire first row; then display at that scale
   */
  def draw(surface: Surface): Unit = {
    val scaledH: Scalar =
    if (autoScale&&seq.nonEmpty&&seq(0).length>cols) {
      scale = (cols.toDouble/seq(0).length.toDouble).toFloat
      h/scale-margin
    } else {
      scale=1
      h
    }

    val underHeader: Scalar = headerDiagonal.y*scale
    val underlineColour = underlineBrush

    drawBackground(surface)
    surface.withClip(diagonal) {
      var nextVisibleRow: Int = 0
      // draw the heading
      var y: Scalar = charH - descent
      val shadeWidth = w/scale
      surface.withScale(scale) {
        surface.declareCurrentTransform(this)
        // the header
        for {i <- 0 until heading.length} {
          val text = if (cacheing)  headCache.getOrElseUpdate(nextVisibleRow, TextLine.make(heading(i), font)) else TextLine.make(heading(i), font)
          surface.drawTextLine(headBrush, text, margin, y)
          y += charH
        }
        // the rows
        nextVisibleRow= rowOrigin
        val lastRow = seq.length
        while (nextVisibleRow < lastRow && y <= scaledH) {
          val text = if (cacheing) textCache.getOrElseUpdate(nextVisibleRow, TextLine.make(seq(nextVisibleRow), font)) else TextLine.make(seq(nextVisibleRow), font)
          val width = text.getWidth
          if ((nextVisibleRow&1)==0) surface.fillRect(stripeBrush, margin, y-shadeHeight, shadeWidth, charH)
          surface.drawTextLine(toForegroundBrush(nextVisibleRow), text, margin, y)
          if (isUnderlined(nextVisibleRow)) surface.drawLines$(underlineColour, margin, y + descent, width, y + descent)
          nextVisibleRow += 1
          y += charH
        }
      }
      // show the state of the viewport
      val offset=margin/4
      val topBrush = if (rowOrigin==0) marginBrush else outsideBrush
      val botBrush = if (nextVisibleRow>=seq.length) marginBrush else outsideBrush
      surface.drawLines$(topBrush, offset,underHeader, w-offset,underHeader)
      surface.drawLines$(botBrush, offset,h-offset, w-offset,h-offset)
    }
  }

  /*
  // margins


  * */

  def copy(fg: Brush=fg, bg: Brush=bg): ReactiveGlyph = null
      //new SeqViewer(cols, rows, font, fg, bg, selBrush, seq, autoScale)

  var clicks: Int = 0

  lazy val marginBrush: Brush = Brushes.red(alpha=0.85f)
  lazy val outsideBrush: Brush = marginBrush(width=2).dashed(5,5)
  def isVisible(row: Int): Boolean = (rowOrigin<=row) && (row-rowOrigin<rows)


  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    import gesture._
    gesture match {
      case _: MouseEnters =>
        guiRoot.grabKeyboard(thisViewer)
        refresh((initially)) //??
      case _: MouseLeaves =>
        hovered = -1
        guiRoot.freeKeyboard(completely = true)
        refresh(initially)

      case _: MouseScroll if !CONTROL =>
        val nextRowOrigin = (rowOrigin+(if (delta.x+delta.y > 0f) -1 else 1)) max 0
        if (nextRowOrigin+rows<=seq.length) rowOrigin=nextRowOrigin
        reDraw()

      case _: MouseScroll if CONTROL  =>
        colOrigin = (colOrigin+(if (delta.x+delta.y > 0f) -1 else 1)) max 0
        reDraw()

      case Keystroke(key, _) if !PRESSED =>

      case Keystroke(key, _) if PRESSED =>
        key match {

          case Key.DOWN =>
            if (selectedRows.nonEmpty) {
              (if (SHIFT) addToSelection(_) else setCurrentRow(_))(currentMaxRow + 1)
              if (currentMaxRow - rowOrigin >= rows-1 ) {
                rowOrigin += 1
              }
            } else {
              bell.play()
              setCurrentRow(rowOrigin+rows-1)
            }
            reDraw()

          case Key.UP =>
            if (selectedRows.nonEmpty) {
              (if (SHIFT) addToSelection(_) else setCurrentRow(_))(currentMinRow - 1)
              if (currentMinRow < rowOrigin) {
                rowOrigin = 0 max (currentMinRow - 1)
              }
            } else {
              bell.play()
              setCurrentRow(rowOrigin)
            }
            reDraw()

          case Key.HOME =>
            clearSelection()
            rowOrigin = 0
            reDraw()

          case Key.END =>
            clearSelection()
            val lastRow = (seq.length-1)
            if (lastRow-rowOrigin>=rows-2) {
              rowOrigin = seq.length-rows
            }
            reDraw()

          case Key.PAGE_DOWN =>
            if (rowOrigin+rows-1<seq.length) {
              rowOrigin += rows-2
              reDraw()
            }

          case Key.PAGE_UP =>
            rowOrigin = 0 max rowOrigin-rows
            reDraw()

          case Key.RIGHT | Key.ENTER =>
            val currentRow=currentMinRow
            if (currentMaxRow==currentRow && isVisible(currentRow)) onDoubleClick(modifiers, currentRow) else bell.play()
            reDraw()

          case Key.A if CONTROL =>
            _selectedRows.addAll(0 until seq.length)
            reDraw()

          // ignore shift buttons
          case Key.SHIFT | Key.CONTROL | Key.CAPS_LOCK | Key.ALT | Key.MAC_COMMAND | Key.MAC_FN | Key.MAC_OPTION =>

          case _ =>
            onOther(gesture)

        }

      // motion near a row with nothing pressed is a hover
      case MouseMove(modifiers) if !PRESSED =>
        val row = yToRow(location.y) + rowOrigin
        val oldHovered = hovered
        hovered = row min seq.length
        if (oldHovered!=hovered) onHover(modifiers, hovered)
        reDraw()

      // motion near a row with control pressed => invert selection state of row
      case MouseMove(modifiers) if PRESSED && CONTROL =>
        val row = yToRow(location.y) + rowOrigin
        val oldHovered = hovered
        hovered = row min seq.length
        if (row<seq.length && hovered!=oldHovered)  {
          if (_selectedRows.contains(hovered)) _selectedRows.remove(hovered) else _selectedRows.add(hovered)
        }
        reDraw()

      // motion near a row without control pressed => add row to selection
      case MouseMove(modifiers) if PRESSED && !CONTROL =>
        val row = yToRow(location.y) + rowOrigin
        hovered = row min seq.length
        if (row<seq.length) _selectedRows.add(hovered)
        reDraw()

      case other: MouseMove =>

      // secondary button (or primary with control) clicked near row => invert selection state of row
      case MouseClick(_)  if (SECONDARY) =>
        val row = yToRow(location.y) + rowOrigin
        val hovered = row min seq.length
        if (row<seq.length)
           if (_selectedRows.contains(hovered)) _selectedRows.remove(hovered) else _selectedRows.add(hovered)
        else
           _selectedRows.clear()
        ClickTimer.clear()
        reDraw()


      // primary button with shift clicked near a row => extend the selection (from its other end) to that row
      case MouseClick(_) if PRIMARY && SHIFT =>
        val row = yToRow(location.y) + rowOrigin
        val hovered = row min seq.length-1
        _selectedRows.add(hovered)
        val top = _selectedRows.min min hovered
        val bot = _selectedRows.max max hovered
        _selectedRows.addAll(top to bot)
        ClickTimer.clear()
        reDraw()

      // click in the left gutter => clear the selection
      // first click near a row => sets selection to that row
      // second click within 1/2sec on the same row => invokes `onDoubleClick(modifiers, currentRow)`
      case MouseClick(_) if PRESSED =>
        if (location.x<=margin) {
          _selectedRows.clear()
        } else {
          val row = yToRow(location.y) + rowOrigin
          val hovered = row min seq.length
          println(s"row: $row; hovered: $hovered")
          if (_selectedRows.contains(row)) {
            setCurrentRow(row)
            if (ClickTimer.doubleClick) {
              onDoubleClick(modifiers, currentRow)
              ClickTimer.clear()
            }
            println(s"selected contained row: $row; hovered: $hovered")
          } else {
            _selectedRows.clear()
            ClickTimer.start()
            if (row < seq.length) _selectedRows.add(hovered)
            println(s"selected added row: $row; hovered: $hovered")
          }
        }
        reDraw()

      case other =>
        if (PRESSED) onOther(other)
    }
    //println(displayList)
    //println(selection)
  }

  object ClickTimer {
    var lastClick: Long = 0
    def doubleClick: Boolean = {
      val timeNow = System.currentTimeMillis()
      val result = (timeNow-lastClick) < 500
      lastClick = timeNow
      result
    }
    def start(): Unit = lastClick = System.currentTimeMillis()
    def clear(): Unit = lastClick=0L
  }

}
