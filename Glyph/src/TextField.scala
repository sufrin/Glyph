package org.sufrin
package glyph

import io.github.humbleui.jwm.{EventKey, EventTextInput, EventTextInputMarked, Key}
import io.github.humbleui.skija.TextLine
import org.sufrin.glyph.GlyphTypes.{Font, Scalar}
import org.sufrin.glyph.unstyled.Text
import org.sufrin.utility.CodePointSeqMap.{CodePoint, CodePointSeq}
import org.sufrin.glyph.tests.StockAbbreviations
import org.sufrin.logging
import org.sufrin.logging.{FINER, SourceDefault}


/**
 *  A fixed-width reactive glyph that can be edited from the keyboard. The width of
 *  the glyph is `size * em`, where `em` is the size of an "m" in the specified font.
 *
 *  The text being edited can be of any length.
 *  It is panned, if necessary, to keep the cursor in view.
 *  Simple visual indications are given at each if its ends
 *  when there is non-visible text at that end.
 *
 *  When the mouse cursor enters this component, it grabs the keyboard focus, and this
 *  directs subsequent keystrokes to it.
 *
 *  When the mouse cursor enters this reactive glyph, the method `onCursorLeave` is invoked.
 *
 *
 * CODEPOINTS, UNIGLYPHS, POLYCODED UNIGLYPHS, RIGHT-TO-LEFT
 *
 * A-The text is represented internally as a sequence of unicode codepoints ("Unicodes"). Usually each codepoint corresponds to a single
 * visible letter in the given font ("unicode glyph", or "uniglyph"); but in some circumstances what is shown as a single visible letter may be represented by more than just one codepoint.
 * Below these are knpwn as "polycoded uniglyphs". For example a national flag such as 🇬🇧 is represented as the two adjacent uniglyphs  🇬 🇧 that are themselves represented as
 * `1f1ec` `1f1e7`. Other examples (including the ZWJ sequences) require several adjacent codepoints punctuated with ZWJ (zero-width join: 200d) codepoints. Thus 🧑‍🤝‍🧑 is
 * represented as 1f9d1 200d 1f91d 200d 1f9d1 -- the non-ZWJ codepoints themselves represent individual uniglyphs (🧑🤝🧑). The rationale for this in
 * the unicode standard is that as a fallback, the ZWJ sequences can be rendered as their individual uniglyphs.
 *
 * Finally, right-To-left codepoints  (`cp`) are usually represented
 * as triples: `(RLM, cp, LRM)` -- where RLM, LRM are the Unicode directional marks RIGHT-TO-LEFT MARK and LEFT-TO-RIGHT MARK.
 *
 * B-Pasteing a right-to-left piece of text usually inserts each of its unicodepoints in the text in such a way that its visual appearance is
 * maintained, and it can be edited "logically"; likewise inserting a right-to-left character places the three codepoints representing it to the right of the
 * cursor; and henceforth treats the triple as a single character so that it can be edited. This behaviour is suppressed if `normalizeRightToLeft` is false.
 *
 * https://en.wikipedia.org/wiki/Bidirectional_text#Unicode_bidi_support
 *
 * C-The bottom line here is that  polycoded uniglyphs are treated in this component as if they were represented as single codepoints: PROVIDED that they
 * * are defined as subsitutions for abbreviations, or that they are pasted into the component as single entitites, or typed at the keyboard.
 *
 * NOTES
 *
 * 1-This served as a serious exercise in understanding the ramificatons of unicode representations of glyphs. Among
 * other things there is not a 1-1 correspondence between UTF32 codepoints and glyphs as displayed. So although I chose
 * a representation consisting of codepoints, working out the details of glyph widths (for navigation) has been a bit
 * of a trial.  Skia/Skija doesn't treat non-BMP codepoints as first-class citizens; and that makes measuring (so as
 * to implement mouse-pointing) quite tricky. Earlier the presence of glyphs represented by more than a
 * single (UTF32) codepoint ("polycoded glyphs") mucked up mouse-pointing, there is now a workaround that (quite inefficiently)
 * deals with these cases -- but only when they occur.
 *
 * 2-Our method of constructing the glyph to codepointcount mappings is designed to make it unnecessary to
 * preload data for glyphs that are the (sole)-targets of abbreviations, or inserted (as singleton glyphs) from
 * the clipboard (ctrl-V), or as right-to-left characters from the keyboard (though data-preloading is possible).
 *
 * 3-Tests (OS/X) suggest correct functioning for left-to-right material, including polycoded emoji glyphs. Arabic and modern Hebrew also seem to work.
 *
 * @param fg  brush for text
 * @param bg brush for background
 * @param font font for text
 * @param onEnter invoked when an `ENTER` key is pressed
 * @param onError invoked when an unknown key is pressed
 * @param onCursorLeave invoked when the cursor leaves the reactive
 * @param onChange  invoked when the text being edited changes
 * @param size number of unicodepoints
 * @param initialText the initial text (as a string)
 * @param abbreviations mapping from abbreviations to their substitutions (and vice-versa)
 * @param polyCodings data representing polycoded glyphs
 * @param onNewGlyph invoked when a new polycoded glyph is "discovered"
 * @param normalizeRightToLeft see (C) above
 */
class TextField(override val fg: Brush, override val bg: Brush, font: Font,
                var onEnter: String => Unit,
                var onError: (EventKey, Glyph) => Unit,
                var onCursorLeave: String => Unit,
                var onChange: Option[String => Unit],
                val size: Int,
                initialText: String,
                val abbreviations: TextAbbreviations,
                val polyCodings: PolyCodings,
                var onNewGlyph: (String, CodePointSeq) => Unit,
                var normalizeRightToLeft: Boolean = true // normalize RTL in pasted material
               ) extends ReactiveGlyph
{
  import TextField._

  /** A copy of this glyph; perhaps with different foreground/background */
  def copy(fg: Brush, bg: Brush): Glyph = new TextField(fg, bg, font, onEnter, onError, onCursorLeave, onChange, size, initialText, abbreviations, polyCodings, onNewGlyph, normalizeRightToLeft)

  import io.github.humbleui.jwm.{EventMouseButton, Window}
  private val metrics = font.getMetrics
  private val fontHeight = metrics.getDescent - metrics.getAscent //metrics.getHeight
  private val emWidth    = metrics.getMaxCharWidth
  private val spacing    = metrics.getDescent + metrics.getAscent + metrics.getLeading
  private val emDrop     = fontHeight// - spacing

  private val em         = Text("M", font)
  private val emDiagonal = Vec(emWidth, emDrop)
  private val atBaseLine = metrics.getXHeight+metrics.getDescent - 1//fontHeight
  private val nudge      = emWidth / 2
  private val deltaY     = emDiagonal.y*0.2f
  def diagonal: Vec      = Vec(emWidth*size, emDrop)//emDiagonal.y*1.2)

  var onCursorEnter: String => Unit = {
    text => takeKeyboardFocus(); reDraw()
  }

  locally { TextModel.string = initialText }

  def string: String = TextModel.string

  def string_=(newText: String): Unit = {
    TextModel.string = newText
    reDraw()
  }

  /** Index of the character boundary of the cursor 0..length  */
  def cursor: Int = TextModel.left

  /** text.length  */
  def length: Int = TextModel.length

  private def focussed: Boolean =
    if (hasGuiRoot) guiRoot.hasKeyboardFocus(this) else false



  /**
   * Accept input denoting a diacritical that will become part of a composite character.
   *
   * TODO: Clarify whether _selection..., _replacement... fields denote anything useable
   *       It's not clear from the examples in the documentation.
   */
  override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
    val start       = key.getReplacementStart
    val end         = key.getReplacementEnd
    var deleteLater = 1+end-start
    val text        = key.getText
    // Cases I know of are for single accent characters
    TextModel.insToReplace(text, deleteLater) // pending characters to delete
    if (onChange.isDefined) onChange.get.apply(string)
    reDraw()
    resetAbbreviationTrigger()
  }

  override def accept(key: EventTextInput, location: Vec, window: Window): Unit = {
    TextModel.ins(key.getText)
    if (onChange.isDefined) onChange.get.apply(string)
    reDraw()
    resetAbbreviationTrigger()
  }

  /**
   * The last shift-key that was pressed alone.
   * Two successive presses of the same shift key
   * triggers an abbreviation hunt.
   */
  private var abbreviationTrigger = Key.UNDEFINED
  private def resetAbbreviationTrigger(): Unit = abbreviationTrigger = Key.UNDEFINED

  import Modifiers._
  val ANYCONTROL  = Control | Command
  val ANYSHIFT    = ANYCONTROL | Alt

  protected var abbreviationKey: Key = Key.UNDEFINED
  protected var abbreviationMods: Bitmap = Bitmap(0)

  /**
   * Set the abbreviation key (and modifiers).
   * @param key
   * @param mods
   * @return
   */
  def withAbbreviationKey(key: Key, mods: Int=0): TextField = {
    abbreviationKey = key
    abbreviationMods = Bitmap(mods|Pressed)
    TextField.info(s"Abbreviation key: $key $abbreviationMods")
    this
  }

  override def accept(key: EventKey, location: Vec, window: Window): Unit = {
    import io.github.humbleui.jwm.{Clipboard, ClipboardEntry, ClipboardFormat}
    import io.github.humbleui.jwm.Key._


    val mods: Bitmap = toBitmap(key)

    if (mods.includeSome(Pressed)) key._key match {
      case END        => TextModel.end()
      case LEFT if mods.includeSome(ANYCONTROL) => TextModel.end()
      case LEFT       => TextModel.mvLeft()
      case HOME       => TextModel.home()
      case RIGHT if mods.includeSome(ANYCONTROL) => TextModel.home()
      case RIGHT      => TextModel.mvRight()

      case ENTER      => onEnter(string)
      case C if mods.includeSome(ANYCONTROL) =>
        val toCopy = if (TextModel.hasMark) TextModel.markedString else string
        Clipboard.set(new ClipboardEntry(ClipboardFormat.TEXT, toCopy.getBytes()))

      case X if mods.includeSome(ANYCONTROL) =>
        val toCopy = if (TextModel.hasMark) TextModel.markedString else string
        Clipboard.set(new ClipboardEntry(ClipboardFormat.TEXT, toCopy.getBytes()))
        if (TextModel.hasMark) TextModel.cutMarked() else TextModel.clear()

      case U if mods.includeSome(ANYCONTROL) =>
        if (mods.includeSome(Shift))
            while (TextModel.hasRight) {
              TextModel.mvRight()
              TextModel.del()
            }
          else
            while (TextModel.hasLeft) TextModel.del()

      case TAB  =>
        TextModel.ins("\t")

      case V if mods.includeSome(ANYCONTROL) =>
        val text = Clipboard.get(ClipboardFormat.TEXT).getString
        if (text ne null) {
          TextModel.insPaste(text)
          TextModel.accountForCodePointCounts(text)
        } else beep()

      case S if mods.includeSome(ANYCONTROL) =>
        TextModel.swapMark()

      case PERIOD if mods.includeSome(ANYCONTROL) =>
        TextModel.swapMark()

      case Key.DOWN if mods.includeSome(ANYCONTROL) =>
        if (mods.includeSome(Shift))
          TextModel.mark = -1
        else
          TextModel.markToCursor()

      case BACKSPACE  if mods.includeSome(ANYCONTROL) =>
        TextModel.swap2()

      case BACKSPACE  => TextModel.del()

      case DELETE     => TextModel.mvRight(); TextModel.del()

      // two successive presses on the SHIFT key triggers an abbreviation
      case CONTROL | MAC_COMMAND | SHIFT | LINUX_SUPER | ALT | MAC_OPTION | MAC_FN =>
        if ((abbreviationTrigger eq key._key) && ((abbreviationTrigger==SHIFT) || (abbreviationTrigger==MAC_FN))) {
          TextModel.abbreviation()
          resetAbbreviationTrigger()
        } else {
          abbreviationTrigger = key._key
        }

      case other  if abbreviationKey!=UNDEFINED && other==abbreviationKey && mods.includeAll(abbreviationMods.modifiers)=>
        TextModel.abbreviation()

      case other  =>
        if (mods.includeSome(ANYSHIFT)) {
          onError(key, this)
        }
    }
    if (onChange.isDefined) onChange.get.apply(string)
    reDraw()
  }

  def unicode(): Unit =
    if (TextModel.hasLeft) {
      TextModel.unicode()
    }

  def unabbreviation(): Unit = TextModel.unabbreviation()
  def tryAbbreviation(): Unit = TextModel.tryAbbreviation()

  /** Width of the serifs of the I-beam drawn as the cursor */
  val cursorSerifWidth = 5f
  /**
   *  Top and bottom vertical shrink of the I-beam serifs
   */
  val cursorHeightDelta = 1.0f

  /**
   * Brush used to show the cursor when focussed
   */
  val focussedBrush = Brushes.black(width=3f)

  /**
   * Brush used to show the cursor when focussed
   */
  val unfocussedBrush = focussedBrush(alpha=0.4f)

  /**
   * Brush used to show panned warnings
   */
  val panWarningBrush = fg(width=6, alpha=0.5f).dashed(2,2)

  /**
   *  Offset from start/end of the glyph of x of the pan-warning stroke
   */
  val panWarningOffset = panWarningBrush.strokeWidth

  val markBrush:   Brush = Brushes.green(width=3)
  val markedBrush: Brush = markBrush(width=diagonal.y, alpha=0.2f)
  val r2lBrush: Brush = Brushes.red(width=diagonal.y, alpha=0.3f)

  /** The most recent origin of the displayed textlayout */
  def panBy: Int = TextModel.pan

  /**
   * Draw the glyph on the surface at its given size, with a "cursor" indicating the current
   * editing position.
   */
  def draw(surface: Surface): Unit = {
    drawBackground(surface)
    surface.declareCurrentTransform(this)
    var panning = panBy>0
    var overflow = false
    surface.withClip(diagonal) {
      surface.withOrigin(0, deltaY) {
        TextModel.rePan()
        val (leftWidth, allWidth) = TextModel.draw(surface)
        overflow=allWidth>=w

        // prepare to draw the cursor
        val cursorNudge = if (TextModel.left==0) focussedBrush.strokeWidth/2f else 0
        val cursorLeft = leftWidth + cursorNudge
        val cursorBrush: Brush = if (focussed) focussedBrush else unfocussedBrush
        val cursorBottom = diagonal.y-cursorHeightDelta-deltaY


        @inline def drawCursor(brush: Brush, position: Scalar): Unit = {
          surface.drawLines(brush, position, cursorHeightDelta-deltaY, position, cursorBottom) // vertical
          surface.drawLines(brush, position - cursorSerifWidth, cursorHeightDelta-deltaY, position + cursorSerifWidth, cursorHeightDelta-deltaY) // top bar
          surface.drawLines(brush, position - cursorSerifWidth, diagonal.y - cursorHeightDelta-deltaY, position + cursorSerifWidth, diagonal.y - cursorHeightDelta-deltaY) // bottom bar
        }

        // show the text margins when logging
        if (TextField.loggingLevel(FINER)) {
          surface.drawPolygon$(cursorBrush(color=0XFFFF0000), w-TextModel.margin, 0, w-TextModel.margin, diagonal.y)
          surface.drawPolygon$(cursorBrush(color=0XFFFF0000), TextModel.margin, 0, TextModel.margin, diagonal.y)
        }

        // Draw the cursor as an I-Beam
        //surface.drawLines(cursorBrush, cursorLeft, cursorHeightDelta-deltaY, cursorLeft, cursorBottom) // vertical
        //surface.drawLines(cursorBrush, cursorLeft - cursorSerifWidth, cursorHeightDelta-deltaY, cursorLeft + cursorSerifWidth, cursorHeightDelta-deltaY) // top bar
        //surface.drawLines(cursorBrush, cursorLeft - cursorSerifWidth, diagonal.y - cursorHeightDelta-deltaY, cursorLeft + cursorSerifWidth, diagonal.y - cursorHeightDelta-deltaY) // bottom bar

        if (normalizeRightToLeft)
          drawCursor(cursorBrush, cursorLeft)
        else
        // Show RTL position: works
        // TODO: inconsistent with mark, cut, etc.
        TextModel.rtlPosition match {
          case None =>
            drawCursor(cursorBrush, cursorLeft)
          case Some(position) =>
            val r2lCursorBrush: Brush = cursorBrush(color=r2lBrush.color)
            drawCursor(r2lCursorBrush, position)
            surface.drawLines(r2lBrush, position, cursorBottom/2, cursorLeft, cursorBottom/2)
            drawCursor(cursorBrush, cursorLeft)
        }

        //Show the mark position
          TextModel.markPosition match {
          case None =>
          case Some(position) =>
            surface.drawLines(markedBrush, position min cursorLeft, cursorBottom/2, position max cursorLeft, cursorBottom/2)
            drawCursor(markBrush, position)
          //if (overflow && markPosition>w) surface.drawPolygon$(markBrush, w - panWarningOffset, 0f, w - panWarningOffset, diagonal.y)
          //if (panning && markPosition==0f) surface.drawPolygon$(markBrush, panWarningOffset, 0f, panWarningOffset, diagonal.y)
        }
      }



    // Indicate when there's invisible text to the right
    if (overflow) surface.drawPolygon$(panWarningBrush, w - panWarningOffset, 0f, w - panWarningOffset, diagonal.y)

    // Indicate when there's invisible text to the left
    if (panning) surface.drawPolygon$(panWarningBrush, panWarningOffset, 0f, panWarningOffset, diagonal.y)


  }
}

/** The component takes the keyboard focus */
def takeKeyboardFocus(): Unit = if (hasGuiRoot) guiRoot.grabKeyboard(this)

/** The component gives up the keyboard focus unconditionally */
def giveUpKeyboardFocus(): Unit = if (hasGuiRoot) guiRoot.giveupFocus()


  /** Seize focus on entry [prototype only] */
  override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = event match {
    case _: GlyphEnter =>
      import io.github.humbleui.jwm.MouseCursor
      onCursorEnter(string)
      window.setMouseCursor(MouseCursor.IBEAM)
      reDraw() // window.requestFrame()
    case _: GlyphLeave =>
      onCursorLeave(string)
  }

  override def accept(mouse: EventMouseButton, location: Vec, window: Window): Unit = {
    import Modifiers._
    import TextModel.{markTo, moveTo}
    val mods = toBitmap(mouse)
    val ctrl = Control|Command|Secondary
    if (mouse.isPressed)
      if (mods.includeSome(ctrl)) markTo(location.x) else moveTo(location.x)
    reDraw() // window.requestFrame()
  }

  /** Whether or not to pan right when the cursor is in the right margin. */
  var rightMargin: Boolean  = false

  /** A simple and inefficient textlayout model, for short texts in single fonts.
   *
   *  The textlayout being edited is a string `textlayout` formed from a sequence of characters
   *  with a current position, represented as a "gap-array"  (buffer)
   *  of Unicode codepoints.
   *
   *      {{{
   *          buffer[0 until left] ++ buffer[right until buffer.size]
   *      }}}
   *
   *  When it needs to be shown, a  `Text` glyph is made from it; this
   *  may well be quite inefficient, but it happens only at human-finger speed.
   *
   *  The model could easily be equipped with an undo/redo feature but it hardly
   *  seems worth doing so in the prototype toolkit.
   */
  private object TextModel {
    type CodePoint = Int
    var buffer: Array[CodePoint] = Array.ofDim[CodePoint](size + 3)

    @inline private def N = buffer.size

    def tryAbbreviation(): Unit = {
      if (abbreviations != null && abbreviations.onLineTrigger) abbreviation()
    }

    var left = 0
    var right = N

    /** Number of characters in the buffer */
    def length: Int = left + N - right

    def clear(): Unit = {
      left = 0; right = N
    }

    override def toString: String = s"TextField.TextModel(${leftString}, ${rightString})"

    /**  String represented by the buffer */
    def string: String = {
      val builder = new java.lang.StringBuilder
      for {cp <- 0 until left} builder.appendCodePoint(buffer(cp))
      for {cp <- right until N} builder.appendCodePoint(buffer(cp))
      builder.toString
    }

    /**  Set the string represented by the buffer (don't unabbreviate) */
    def string_=(newText: String): Unit = {
      clear()
      newText.codePoints.forEach(insCodePoint(_))
    }

    def leftCodePoint: CodePoint = {
      assert(hasLeft)
      buffer(left-1)
    }

    def leftString: String = leftString(0)

    def leftString(from: Int): String = if (from < 0 || left - from <= 0) "" else {
      //new String(buffer, from, left-from)
      val builder = new java.lang.StringBuilder
      for {cp <- from until left} builder.appendCodePoint(buffer(cp))
      builder.toString
    }

    def rightString: String = {
      //new String(buffer, right, N-right)
      val builder = new java.lang.StringBuilder
      for {cp <- right until N} builder.appendCodePoint(buffer(cp))
      builder.toString
    }

    def leftWidth(from: Int): Scalar = {
      //val codePoints = visiblePointArray(from)
      //TextLine.make(new String(codePoints, 0, left-from), font).getWidth
      leftWidths(from).sum
    }

    var lastTextLine: io.github.humbleui.skija.TextLine = null
    var lastVisiblePointArray: Array[CodePoint] = null         // the codepoints from which `lastTextLine` was constructed

    /**
     * True iff all CodePoints in the last-displayed line were in the BMP
     */
    var allBMP: Boolean = true

    /**
     * (TextLine, cursorPosition, textWidth) -- used by `draw`
     * POST: `allBMP` iff all codePoints are in the BMP
     */
    @inline private def allTextLine(from: Int): (TextLine, Scalar, Scalar) = {
      lastVisiblePointArray = visiblePointArray(from)
      val leftWidth = TextLine.make(new String(lastVisiblePointArray, 0, left-from), font).getWidth
      lastTextLine = TextLine.make(new String(lastVisiblePointArray, 0, length-from), font)
      (lastTextLine, leftWidth, lastTextLine.getWidth)
    }


    /** Delegated to by the main `draw` */
    def draw(surface: Surface): (Scalar, Scalar) =
    { val (tl, cursor, width) = allTextLine(pan: Int)
      surface.drawTextLine(fg, tl, 0, atBaseLine)//metrics.getXHeight+metrics.getDescent
      (cursor, width)
    }

    def markPosition: Option[Scalar] = {
      if (mark>=pan) {
        //Some(visiblePoints(pan).take(mark-pan).map(codePointWidth(_)).sum)
        Some(lateralPosition(mark-pan))
      } else None
    }

    /** Only reliable way of determining the lateral position of the `pos`th glyph */
    def lateralPosition(pos: Int): Scalar = {
      TextLine.make(new String(lastVisiblePointArray, 0, pos), font).getWidth
    }

    /** Offset from the left of the next place where r2l material becomes l2r material */
    def rtlPosition: Option[Scalar] = {
      if (hasLeft && isRightToLeft(leftCodePoint)) {
        val points = leftCodePoints
        var pos = points.length
        while (pos>0 && (isRightToLeft(points(pos-1)) || Character.isSpaceChar(points(pos-1)))) { pos -= 1 }
        val delta = if (Character.isSpaceChar(points(pos))) 1 else 0 // omit a leading spoace
        //Some(visiblePoints(pan).take(pos-delta-pan).map(codePointWidth(_)).sum)
        Some(lateralPosition(pos+delta-pan))
      } else None
    }

    @inline def hasLeft: Boolean = left != 0

    @inline def hasRight: Boolean = right != N


    /**
     * Implementation of cursor motion to a horizontal location.
     * Heavy-duty when outside `allBMP`
     */
    def moveTo(x: GlyphTypes.Scalar): Unit = {
      val nudgex = x - nudge
      if (allBMP) {
        val index = lastTextLine.getLeftOffsetAtCoord(x) + panBy
        while (left < index && left != right) mvRight()
        while (left > index && left != 0) mvLeft()
      } else {
        // An inefficient way to deal with widths of nonBMP material; because Skia/Skija doesn't do the accounting properly
        val codePoints = lastVisiblePointArray

        @inline def rightWidth: Scalar = {
          val n = rightCodepoints
          if (n == 1)
            codePointWidth(buffer(right))
          else {
            TextLine.make(new String(codePoints, 0, n), font).getWidth
          }
        }

        var xPos = 0f
        while (panBy < left) MVLEFT() // left visible end
        while (xPos < nudgex && hasRight) {
          xPos += rightWidth
          mvRight()
        }
      }
    }

    // MARK MANIPULATION

    def markedString: String = {
      if (mark>0) {
        val codePoints = visiblePointArray(mark min left)
        new String(codePoints, 0, (left-mark).abs)
      } else ""
    }

    var mark: Int = 0

    def hasMark: Boolean = mark>0

    def markTo(x: GlyphTypes.Scalar): Unit = {
      val l=left
      moveTo(x)
      mark=left
      while (left<l && hasRight) mvRight()
      while (left>l && hasLeft) mvLeft()
    }

    def markToCursor(): Unit = {
      mark=left
    }

    def swapMark(): Unit = {
      if (mark>=0) {
        val nextMark = left
        while (left < mark && left != right) mvRight()
        while (left > mark && left != 0) mvLeft()
        mark = nextMark
      }
    }

    def cutMarked(): Unit = {
      if (hasMark) {
        if (mark>left) swapMark()
        val m = mark
        while (hasLeft && m<left) del()
      }
    }

    // END MARK MANIPULATION

    /**
     * Grow buffer if necessary to make room for another few characters.
     */
    def ensureAdequateSize(): Unit = {
      val quantum = 10
      if (left == right) {
        val newBuffer = Array.ofDim[CodePoint](buffer.length + quantum)
        var newRight, j = right+quantum
        for {i <- 0 until left} newBuffer(i) = buffer(i)
        for {i <- right until buffer.length} {
          newBuffer(j) = buffer(i)
          j += 1
        }
        buffer = newBuffer
        right = newRight
      }
    }


    def insCodePoint(cp: CodePoint): Unit = {
      mark = -1
      ensureAdequateSize()
      assert(left < right, s"TextModel(size=$size) is full")
      buffer(left) = cp
      left += 1
    }


    def del(): Unit = {
      mark = -1
      val n = leftCodepoints
      if (left >=n ) left -= n
    }

    /**  translate the glyph at the left of the cursor to its unicode (or unicode-sequence, if it is polycoded) equivalent */
    def unicode(): Unit = {
      mark = -1
      val n = leftCodepoints
      if (left >=n ) left -= n
      val lcps = for { i<-0 until n } yield buffer(left+i)
      //println(lcps.map(_.toChar))
      for { cp <- lcps } insPaste(TextAbbreviations.decodeUnicode(cp))
    }

    @inline private def leftGlyphRep: CodePointSeq = reverseLeftCodePoints().take(leftCodepoints).toSeq.reverse
    @inline private def rightGlyphRep: CodePointSeq = rightCodePoints.take(rightCodepoints).toSeq

    /**
     * Swap the last two glyphs to the left of the cursor
     */
    def swap2(): Unit = {
      mark = -1
      if (left>1) {
        mvLeft()
        val ls=leftGlyphRep
        val rs=rightGlyphRep
          mvRight()
          left -=(ls.length+rs.length)   // delete both glyphs
          rs.foreach(insCodePoint(_))    // reinsert in the opposite order
          ls.foreach(insCodePoint(_))
      } else
        TextField.beep()
    }

    private var pendingDeletions: Int = 0

    private def doPendingDeletions(): Unit =
      while (pendingDeletions > 0) {
        del(); pendingDeletions -= 1
      }

    /** Insert a character */
    def ins(ch: Char): Unit = {
      doPendingDeletions()
      insCodePoint(ch)
      tryAbbreviation()
    }

    /**
     * Insert the codepoint `cp`. If it's an RTL codepoint then embed it in RLM/LRM marks
     * and mark it as a (single) polycoded glyph; and if `atRight` then let it appear
     * to the right of the cursor.
     */
    def insRightToLeftCodepoint(cp: Int, atRight: Boolean=false): Unit = {
      import TextField._
      if (isRightToLeft(cp)) {
        val cps = forceRightToLeft(cp)
        polyCodings.set(cps)
        cps.foreach(insCodePoint)
        if (atRight) for  { c<-cps } MVLEFT()
      }
      else {
        insCodePoint(cp)
        correctDirection(cp)
      }

    }

    @inline private def correctDirection(cp: CodePoint): Unit =
            if (normalizeRightToLeft && Character.isSpaceChar((cp)) && hasRight && rightCodepoints==3 && isRightToLeft(buffer(right+1))) MVLEFT()

    /**
     * Insert the codepoint `cp` at the appropriate (according to its directionality)
     * side of the cursor.
     */
    @inline private def insDirectedCodepoint(cp: Int): Unit = {
      if (normalizeRightToLeft) insRightToLeftCodepoint(cp, true) else insCodePoint(cp)
    }

    /**
     * Isolate pasted characters and show in the visual order
     * TODO:further refinement for combining marks (eg Biblical Hebrew Vowels)
     */
    def insPaste(string: String): Unit = {
      doPendingDeletions()
      if (normalizeRightToLeft && string.exists(isRightToLeft))
          string.reverse.codePoints.forEach(insRightToLeftCodepoint(_, false))
      else
          string.codePoints.forEach(insCodePoint)
    }

    /**
     * The Skia/Skija machine may be treating non-escaping grave
     * accents incorrectly. This variable controls whether
     * or not we react to this.
     */
    var graveHack:    Boolean = true

    /**
     * A grave accent (non-escaping) has been typed at the keyboard; and
     * the underlying Skia/Skija state-machine has immediately followed it
     * with \u2018 (which doesn't even resemble a grave accent) rather than
     * awaiting the next character.
     */
    var gravePending: Boolean = false

    /**
     * Insert the given `string`. It may contain "surrogate pairs" arising from
     * characters (for editText Smileys, Kanji, ...) outside the BMP.
     */
    def ins(string: String): Unit = {
      //
      // A ' has appeared immediately following a grave accent
      //
      if (graveHack && gravePending && string=="\u2018") {
        insCodePoint('\u0060')
        gravePending=false
        return
      }
      //
      //
      //
      doPendingDeletions()
      string.codePoints.forEach{ insDirectedCodepoint(_) }
      // string.codePoints.forEach{  cp => println(cp.toHexString, cp.toChar) }
      tryAbbreviation()
    }

    /**
     * Insert the given `string` at `left`.
     * `toReplace` characters are marked for replacement at the
     * following `ins`, `move..`, `del`.
     *
     * @see gravePending
     */
    def insToReplace(string: String, toReplace: Int): Unit = {
      //
      if (graveHack && string=="\u0060") gravePending=true
      //
      pendingDeletions = toReplace
      string.codePoints.forEach(insCodePoint(_))
      string.codePoints.forEach{  cp => println(toReplace, cp.toHexString, cp.toChar) }
      tryAbbreviation()
    }

    @inline private def MVLEFT(): Unit = {
      left -= 1
      right -= 1
      buffer(right) = buffer(left)
    }

    @inline private def MVRIGHT(): Unit =  {
      buffer(left) = buffer(right)
      right += 1
      left += 1
    }

    def mvLeft(): Unit = if (left != 0) {
      val n = leftCodepoints
      for { i<-0 until n} MVLEFT()
    }

    def mvRight(): Unit = if (right != N) {
      val n = rightCodepoints
      for { i<-0 until n } MVRIGHT()
    }

    def home(): Unit = {
      while (left != 0) {
        left -= 1
        right -= 1
        buffer(right) = buffer(left)
      }
    }

    def end(): Unit = {
      while (right < N) {
        buffer(left) = buffer(right)
        left += 1
        right += 1
      }
    }

    var pan: Int = 0

    // Define a sensible margin for panning
    val marginChars: Int = size / 10 max 3
    val margin: Scalar = Text("M" * (marginChars), font).width

    def rePan(): Unit = {
      import TextField.{finest, logging}
      val size: Scalar = w

      @inline def vleft: Scalar = leftWidth(pan)

      if (logging) finest(s"rePan: $pan $vleft $size $margin ${(vleft < size, vleft < margin, vleft >= size - margin)}")
      if (leftWidth(0) < size) {
        pan = 0
        if (logging) finest("<<")
      } else {
        (vleft < size, vleft < margin, vleft >= size - margin) match {
          case (true, _, true) => // visible, but in the right margin
            if (logging) finest("RM")
            if (rightMargin) while (vleft >= size - margin) pan += marginChars
          case (true, true, _) => // visible, but in left margin
            if (logging) finest("LM")
            while (vleft < margin) pan -= marginChars
          case (true, false, false) => // still visible
            if (logging) finest("V")
          case _ =>
            if (logging) finest("J")
            while (0 <= vleft && vleft <= size && pan - marginChars >= 0)
              pan -= marginChars
            while (vleft >= size)
              pan += marginChars
        }
      }
      if (logging) finest(s"rePan= $pan $vleft $size $margin ${(vleft < size, vleft < margin, vleft >= size - margin)}")
    }

    var abbreviating: Boolean = false

    /** codepoints at the left of the cursor */
    private def leftCodePoints: Seq[CodePoint] = new Seq[CodePoint] {
      def apply(i: Int): CodePoint = buffer(i)
      def length: CodePoint = left
      def iterator: Iterator[CodePoint] = new Iterator[CodePoint] {
        var ix: Int = 0
        def hasNext: Boolean = ix < left
        def next(): CodePoint = {
          val v = buffer(ix); ix += 1; v
        }
      }
    }

    /** codepoints at the left of the cursor in reverse order */
    def reverseLeftCodePoints(): Iterator[CodePoint] = new Iterator[CodePoint] {
        var ix: Int = left
        def hasNext: Boolean = ix > 0
        def next(): CodePoint = {
          ix -= 1; buffer(ix)
        }
      }


    /** widths of characters between from and the cursor */
    private def leftWidths(from: Int): Iterator[Scalar] = new Iterator[Scalar] {
        var ix: Int = from
        def hasNext: Boolean = ix < left
        def next(): Scalar = {
          val v = codePointWidth(buffer(ix)); ix += 1; v
        }
    }


    /** codepoints at the right of the cursor */
    private def rightCodePoints: Seq[CodePoint] = new Seq[CodePoint] {
      def apply(i: Int): CodePoint = buffer(right+i)
      def length: CodePoint = N - right
      def iterator: Iterator[CodePoint] = new Iterator[CodePoint] {
        var ix: Int = right
        def hasNext: Boolean = ix < N
        def next(): CodePoint = {
          val v = buffer(ix); ix += 1; v
        }
      }
    }

    /** The codepoints to the right of `pan` */
    private def visiblePoints(): Iterator[CodePoint] = visiblePoints(pan)

    private def visiblePoints(from: Int): Iterator[CodePoint] = new Iterator[CodePoint] {
      val offset = right-left
      var ix = from
      val thisLength = left+N-right
      def hasNext: Boolean = ix<thisLength
      def next(): CodePoint = {
        val cp = buffer(if (ix<left) ix else ix+offset)
        ix += 1
        cp
      }
    }


    @inline private def bufferedPointAt(i: Int): CodePoint = {
      val cp = buffer(i)
      if (!Character.isBmpCodePoint(cp)) allBMP = false
      if (cp=='\u0020') THINSPACE else cp // thin spaces
    }

    /**
     * POST: `allBMP = all (about to be) visible codepoints are in the BMP`. Used only
     * while `draw`ing.
     *
     * All ordinary (`'\u0020'`) spaces are represented as `THINSPACE` to correct the Skija/Skia
     * problem of overestimating the widths of ordinary spaces that follow emojis.
     */
    private def visiblePointArray(pan: Int): Array[CodePoint] = {
      allBMP = true
      val r = Array.ofDim[CodePoint](length - pan)
      var o = 0
      for {i <- pan until left} {
        r(o) = bufferedPointAt(i); o += 1
      }
      for {i <- right until N} {
        r(o) = bufferedPointAt(i); o += 1
      }
      r
    }

    /**
     * PRECONDITION: all the visible codepoints must be in the BMP
     * @return the sequence of glyph encodings for the visible code points
     */
    private def unsafeVisibleGlyphs: Array[Short] = font.getUTF32Glyphs(visiblePointArray(pan))

    /**
     * PRECONDITION:: all the visible codepoints must be in the BMP
     * @return the sequence of widths of the visible code points
     */
    private def unsafeVisibleGlyphWidths: Array[Scalar] = font.getWidths(unsafeVisibleGlyphs)

    /**
     * PRECONDITION:: all the visible codepoints must be in the BMP
     * @return iterator over the sequence of glyph boundaries of the visible code points
     * @see  visibleBoundaries()
     */
    private def unsafeVisibleBoundaries(): Iterator[Scalar] = {
      var sum = 0f
      val widths = unsafeVisibleGlyphWidths.iterator
      widths.scanLeft(0f)((l, r) => (l + r))
    }

    /**
     * @param codePoint
     * @return the width of the given `codePoint` as it will be shown in the current `font`
     *
     * In the case of a non-BMP codepoint we use `TextLine.make` to force the
     * codepoint to be rendered as it will be if it appears in a TextLine
     * made using the current font.
     */
    @inline private def codePointWidth(codePoint: CodePoint): Scalar = {
      if (Character.isBmpCodePoint(codePoint))
        font.getWidths(Array(font.getUTF32Glyph(codePoint)))(0) else {
        val chars = Character.toChars(codePoint)
        // font.measureText(new String(chars, 0, chars.length), fg).getWidth // FAILS -- too short
        val line = TextLine.make(new String(chars, 0, chars.length), font)
        line.getWidth
      }
    }

    /**
     * @return iterator over the widths of the visible characters
     */
    @inline private def visibleWidths(): Iterator[Scalar] = visiblePoints().map(codePointWidth(_))

    /**
     * @return (increasing) iterator over the left boundaries of the visible characters
     */
    @inline private def visibleBoundaries(): Iterator[Scalar] = visiblePoints().map(codePointWidth(_)).scanLeft(0f)(_+_)


    final val workaround = false
    /**
     * PRECONDITION: the buffer must not have changed since the last `draw` (so that
     * `allBMP` accurately reflects the presence of non-BMP codePoints)
     * @param distance
     * @return the index of the first character whose displayed form includes `distance`
     *
     * NOTE: when all characters in the (last-displayed) text line were from the BMP it
     * is safe to use the Skia/Skija `getLeftOffsetAtCoord`; but it is unsafe
     * otherwise, so we calculate character  boundaries independently. (See `visibleBoundaries`)
     *
     */
    def indexOfVisible(distance: Scalar): Int = {
      //println("V", visibleBoundaries().toList)
      //println("U", unsafeVisibleBoundaries().toList)
      if (allBMP) {
        if (lastTextLine eq null) 0 else lastTextLine.getLeftOffsetAtCoord(distance) + panBy
      } else {
        // linear search up a nonDecreasing iterator.
        val positions = visibleBoundaries()
        var index = pan
        while (positions.hasNext && distance>positions.next()) {
          index += 1
        }
       // println("D", index, distance)
        index-pan-1
      }
    }




    /** Map the GLYPH at the left of the cursor to the number of codepoints that represent it */
    @inline private def leftCodepoints: Int = polyCodings.leftCodepoints(reverseLeftCodePoints())

    /** Map the GLYPH at the right of the cursor to the number of codepoints that represent it */
    @inline private def rightCodepoints: Int = polyCodings.rightCodepoints(rightCodePoints.iterator)

    def glyphCount(multiCode: String): Int = {
      val line = TextLine.make(new String(multiCode), font)
      line.getGlyphs.length
    }


    /**
     * Count the GLYPHS yielded by `string`, and bring the
     * left and right glyph size  mappings up to date.
     *
     * This should be applied when any (individual) glyph (represented as `string`)
     * is introduced to this  `TextLine`  from "outside".
     */
    def accountForCodePointCounts(string: String): Unit = {
      @inline def glyphCount = TextLine.make(new String(string), font).getGlyphs.length
      if (string.size>1) {
        val cps = TextAbbreviations.toCodePoints(string)
        if ((glyphCount==1 && cps.length>1) || isDirectional(cps)) {
          val novelty = polyCodings.set(cps)
          if (novelty) onNewGlyph(string, cps)
        }
      }
    }

    def abbreviation(): Unit = if (abbreviations!=null) {
      abbreviations.reverseFindSubstitution(reverseLeftCodePoints) match {
        case None =>

        case Some((repl, size)) =>
          if (!abbreviating) {
            abbreviating = true
            if (left >= size) left -= size
            accountForCodePointCounts(repl)
            if (normalizeRightToLeft&&repl(0)==StockAbbreviations.RLM) {
              ins(StockAbbreviations.toNonRLM(repl))
            }
            else
              ins(repl)
            abbreviating = false
          }
      }
    } else ()

    def unabbreviation(): Unit = if (abbreviations!=null) {
      abbreviations.reverseFindAbbreviation(reverseLeftCodePoints) match {
        case None =>

        case Some((repl, size)) =>
          if (!abbreviating) {
            abbreviating = true
            if (left >= size) left -= size
            ins(repl)
            //accountForCodePointCounts(repl)
            abbreviating = false
          }
      }
    } else ()
  }
}

/**
 * Unstyled TextField companion object.
 *
 * @see TextField
 */
object TextField extends logging.SourceLoggable {
  import Location._
  val bell = Sound.Clip("WAV/tonk.wav")
  def beep() = bell.play()

  def popupError(key: EventKey, glyph: Glyph): Unit = {
    import Modifiers._
    implicit object Style extends StyleSheet
    bell.play()
    styled.windowdialogues.Dialogue.OK(unstyled.static.Label(s"Unknown key: ${toBitmap(key).toShortString} ${key._key}"), RelativeTo(glyph), "Error").start()
  }

  /** bidi start left-right isolate */
  val LRI =0x2066
  /** bidi start right-left isolate */
  val RLI =0x2066
  /** bidi pop  directional isolate */
  val PDI =0x2069
  /** bidi start LR MODE */
  val LRM = 0x200E
  /** bidi start RL MODE */
  val RLM = 0x200F

  /**
   * Internal representation of a regular space: DURING DRAWING.
   *
   * @see visiblePointArray
   */
  val THINSPACE = '\u2009'


  def isDirectional(cps: CodePointSeq): Boolean = cps.length>2 && cps.head==RLM && cps.last==LRM

  def reportNewGlyph(glyph: String, codePoints: CodePointSeq): Unit = {
    SourceDefault.info(s"New polycoded glyph: $glyph ")
  }

  def isRightToLeft(cp: CodePoint): Boolean =
    (Character.getDirectionality(cp)) match {
      case Character.DIRECTIONALITY_RIGHT_TO_LEFT
           | Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC => true
      case _ => false
    }

  def isRightToLeft(ch: Char): Boolean =  (Character.getDirectionality(ch)) match {
    case Character.DIRECTIONALITY_RIGHT_TO_LEFT
         | Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC => true
    case _ => false
  }

  def forceRightToLeft(cp: CodePoint): CodePointSeq = List(RLM, cp, LRM)

  /**
   *
   * @see TextField
   */
  def apply(fg: Brush = fallback.textForeground, bg: Brush = fallback.textBackground, font: Font=fallback.textFont,
            onEnter: String=>Unit              = { case text: String => },
            onError: (EventKey, Glyph) => Unit = popupError(_,_),
            onCursorLeave: String=>Unit        = { case text: String => },
            onChange: Option[String=>Unit]     = None,
            size: Int,
            initialText: String = "",
            abbreviations: TextAbbreviations = null,
            polyCodings: PolyCodings = PolyCodings(),
            onNewGlyph: (String, CodePointSeq) => Unit = reportNewGlyph(_, _),
            visualOrderPaste: Boolean = true // normalize RTL in pasted material
           ): TextField =
      new TextField(fg, bg, font,
                    onEnter=onEnter,
                    onError=onError,
                    onCursorLeave=onCursorLeave,
                    onChange=onChange,
                    size=size,
                    initialText=initialText,
                    abbreviations=abbreviations,
                    polyCodings = polyCodings,
                    onNewGlyph = onNewGlyph,
                    normalizeRightToLeft = visualOrderPaste // normalize RTL in pasted material
                    )

}

/**
 * Data representing polycoded glyphs. Enables a uniglyph adjacent to the cursor to divulge the
 * length of its polycoding.
 */
case class PolyCodings() {
          import org.sufrin.utility.CodePointSeqMap
          /** domain: the (reversed) sequences of codepoints (known to us) that represent a single glyph */
          private val leftGlyphCodepointCount: CodePointSeqMap[Boolean] = new CodePointSeqMap[Boolean]
          /** domain: the (forward) sequences of codepoints (known to us) that represent a single glyph */
          private val rightGlyphCodepointCount: CodePointSeqMap[Boolean] = new CodePointSeqMap[Boolean]

          /**
           *  Declare that the given codepoint sequence represents a single glyph
           * @return is this sequence being seen for the first time
           */
          def set(cps: CodePointSeq) = {
            rightGlyphCodepointCount.update(cps, true)
            leftGlyphCodepointCount.reverseChange(cps, true).isEmpty
          }

          /** Map the GLYPH at the left of the cursor to the number of codepoints that represent it */
          def leftCodepoints(reverseLeft: Iterator[CodePoint]): Int =
            leftGlyphCodepointCount.longestPrefixMatch(reverseLeft) match {
              case None => 1
              case Some((_, n)) => n
            }

          /** Map the GLYPH at the right of the cursor to the number of codepoints that represent it */
          def rightCodepoints(right: Iterator[CodePoint]): Int =
            rightGlyphCodepointCount.longestPrefixMatch(right.iterator) match {
              case None => 1
              case Some((_, n)) => n
            }
      }



