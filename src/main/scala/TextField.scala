package org.sufrin
package glyph

import io.github.humbleui.jwm.{EventKey, EventTextInput, EventTextInputMarked, Key}
import io.github.humbleui.skija.TextLine
import org.sufrin
import org.sufrin.glyph.GlyphTypes.{Font, Scalar}
import org.sufrin.glyph.unstyled.Text
import org.sufrin.logging.FINER

import scala.::
import scala.collection.IndexedSeqView.Slice
import scala.collection.mutable


/**
 *  A fixed-width reactive glyph that can be edited from the keyboard. The width of
 *  the glyph is `size * em`, where `em` is the size of an "m" in the specified font.
 *  The textlayout being edited can be of any length.
 *  It is panned, if necessary, to keep the cursor in view.
 *  Simple visual indications are given at each end of the glyph
 *  when there is non-visible textlayout at that end.
 *
 *  When the mouse cursor enters this glyph, it grabs the keyboard focus, and this
 *  directs subsequent keystrokes to it.
 *
 *  When the mouse cursor leaves this glyph, it gives up the keyboard focus.
 *
 *  TODO: give visual feedback when this has the keyboard focus.
 *  TODO: styled colouration of cursor
 *
 * @see styled.TextField for a styled companion object.
 */
class TextField(override val fg: Brush, override val bg: Brush, font: Font,
                var onEnter: String => Unit,
                var onError: (EventKey, Glyph) => Unit,
                var onCursorLeave: String => Unit,
                var onChange: Option[String => Unit],
                size: Int,
                initialText: String,
                abbreviations: org.sufrin.utility.TextAbbreviations
               ) extends ReactiveGlyph
{
  /** A copy of this glyph; perhaps with different foreground/background */
  def copy(fg: Brush, bg: Brush): Glyph = new TextField(fg, bg, font, onEnter, onError, onCursorLeave, onChange, size, initialText, abbreviations)

  import io.github.humbleui.jwm.{EventMouseButton, Window}
  val em = Text("M", font)
  val emDiagonal = Vec(em.width, em.drop)

  var onCursorEnter: String => Unit = {
    text => takeKeyboardFocus()
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


  /**
   * The diagonal size of the glyph
   */
  def diagonal: Vec = Vec(emDiagonal.x*size, emDiagonal.y*1.2)
  val atBaseLine = em.height
  val deltaY = emDiagonal.y*0.2f

  private def focussed: Boolean =
    if (hasGuiRoot) guiRoot.hasKeyboardFocus(this) else false

  /**
   * Accept input denoting a diacritical that will become part of a composite character.
   *
   * TODO: Clarify whether _selection..., _replacement... fields denote anything useable
   *       It's not clear from the example programs
   */
  override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
    val start= key.getReplacementStart
    val end  = key.getReplacementEnd
    // Cases I know of are for single accent characters
    TextModel.insForReplacement(key.getText, 1+end-start) // pending characters to delete
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
   * (with transparent else pressed) triggers an abbreviation hunt.
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
    abbreviationMods = Bitmap(mods)
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

      case V if mods.includeSome(ANYCONTROL) =>
        val text = Clipboard.get(ClipboardFormat.TEXT).getString
        TextModel.ins(text)

      case S if mods.includeSome(ANYCONTROL) =>
        TextModel.swapMark()

      case Key.PERIOD if mods.includeSome(ANYCONTROL) =>
        TextModel.markToCursor()


      case BACKSPACE  if mods.includeSome(ANYCONTROL) =>
        TextModel.swap2()

      case BACKSPACE  => TextModel.del()

      case DELETE     => TextModel.mvRight(); TextModel.del()

      case ESCAPE     => TextModel.abbreviation()

      // two successive presses on the same shift key triggers an abbreviation
      case CONTROL | MAC_COMMAND | SHIFT | LINUX_SUPER | ALT | MAC_OPTION =>
        if (abbreviationTrigger eq key._key) {
          TextModel.abbreviation()
          resetAbbreviationTrigger()
        } else {
          abbreviationTrigger = key._key
        }

      // to support pan testing
      case S if mods.includeSome(ANYSHIFT) && TextField.loggingLevel(FINER) =>
           for (i<-0 until 3*size) {
             TextModel.ins(f"$i%03d ")
           }

      case other  if abbreviationKey!=UNDEFINED && other==abbreviationKey && mods ==abbreviationMods =>
        TextModel.abbreviation()

      case other  =>
        if (mods.includeSome(ANYSHIFT)) onError(key, this)
    }
    if (onChange.isDefined) onChange.get.apply(string)
    reDraw()
  }


  /** Width of the serifs of the I-beam drawn as the cursor */
  val cursorSerifWidth = 5f
  /**
   *  Top and bottom vertical shrink of the I-beam serifs
   */
  val cursorHeightDelta = 6.0f

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

  val markBrush: Brush = Brushes.red(width=diagonal.y, alpha=0.3f)

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

        // show the text margins when logging
        if (TextField.loggingLevel(FINER)) {
          surface.drawPolygon$(cursorBrush(color=0XFFFF0000), w-TextModel.margin, 0, w-TextModel.margin, diagonal.y)
          surface.drawPolygon$(cursorBrush(color=0XFFFF0000), TextModel.margin, 0, TextModel.margin, diagonal.y)
        }
        // Draw the cursor as an I-Beam
        surface.drawLines$(cursorBrush, cursorLeft, cursorHeightDelta-deltaY, cursorLeft, cursorBottom) // vertical
        surface.drawLines$(cursorBrush, cursorLeft - cursorSerifWidth, cursorHeightDelta-deltaY, cursorLeft + cursorSerifWidth, cursorHeightDelta-deltaY) // top bar
        surface.drawLines$(cursorBrush, cursorLeft - cursorSerifWidth, diagonal.y - cursorHeightDelta-deltaY, cursorLeft + cursorSerifWidth, diagonal.y - cursorHeightDelta-deltaY) // bottom bar

        //Show the mark
          TextModel.markPosition match {
          case None =>
          case Some(markPosition) =>
            surface.drawLines$(markBrush, markPosition min cursorLeft, cursorBottom/2, markPosition max cursorLeft, cursorBottom/2)
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

def takeKeyboardFocus(): Unit = if (hasGuiRoot) guiRoot.grabKeyboard(this)
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
      if (hasGuiRoot) guiRoot.freeKeyboard()
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

    /**  Set the string represented by the buffer */
    def string_=(newText: String): Unit = {
      clear()
      ins(newText)
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

    /**
     * [only used by width-calculation workaround]
     * True iff all CodePoints in the last-displayed line were in the BMP
     */
    var allBMP: Boolean = true

    /**
     * (TextLine, cursorPosition, textWidth) -- used by `draw`
     * POST: `allBMP` iff all codePoints are in the BMP
     */
    @inline private def allTextLine(from: Int): (TextLine, Scalar, Scalar) = {
      val codePoints = visiblePointArray(from)
      val leftWidth = TextLine.make(new String(codePoints, 0, left-from), font).getWidth
      lastTextLine = TextLine.make(new String(codePoints, 0, length-from), font)
      (lastTextLine, leftWidth, lastTextLine.getWidth)
    }

    /** Delegated to by the main `draw` */
    def draw(surface: Surface): (Scalar, Scalar) =
    { val (tl, cursor, width) = allTextLine(pan: Int)
      surface.drawTextLine(fg, tl, 0, tl.getHeight)

      (cursor, width)
    }

    def markPosition: Option[Scalar] = {
      if (mark>=0) {
        Some(visiblePoints(pan).take(mark-pan).map(codePointWidth(_)).sum)
      } else None
    }

    @inline def hasLeft: Boolean = left != 0

    @inline def hasRight: Boolean = right != N

    private val nudge = em.width / 2

    /**
     * Implementation of cursor motion to a horizontal location.
     */
    def moveTo(x: GlyphTypes.Scalar): Unit = {
      val nudgex = x + nudge
      val index = indexOfVisible(nudgex) + panBy
      //println(x, left, panBy, index)
      while (left < index && left != right) mvRight()
      while (left > index && left != 0) mvLeft()
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
      val nudgex = x + nudge
      val index = indexOfVisible(nudgex) + panBy
      mark = index
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
        var newRight = newBuffer.size
        for {i <- 0 until left} newBuffer(i) = buffer(i)
        for {i <- right until N} {
          newRight -= 1
          newBuffer(newRight) = buffer(i)
        }
        buffer = newBuffer
        right = newRight
      }
    }

    def isRight(cp: CodePoint): Boolean =
      (Character.getDirectionality(cp)) match {
        case Character.DIRECTIONALITY_RIGHT_TO_LEFT
             | Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC => true
        case _ => false
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
      if (left != 0) left -= 1
    }

    def swap2(): Unit = {
      mark = -1
      if (left > 1) {
        val c = buffer(left - 2)
        buffer(left - 2) = buffer(left - 1)
        buffer(left - 1) = c
      }
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
      if (abbreviations != null && abbreviations.onLineTrigger) abbreviation()
    }

    /**
     * Insert a string that may contain "surrogate pairs" arising from
     * characters (for example Smileys, Kanji, ...) outside the BMP
     */
    def ins(string: String): Unit = {
      doPendingDeletions()
      string.codePoints.forEach(insCodePoint(_))
      if (abbreviations != null && abbreviations.onLineTrigger) abbreviation()
    }

    /**
     * Insert a string that may contain "surrogate pairs" arising from
     * characters (for example Smileys, Kanji, ...) outside the BMP.
     * `toReplace` characters are marked for replacement at the
     * following keystroke.
     */
    def insForReplacement(string: String, toReplace: Int): Unit = {
      pendingDeletions = toReplace
      string.codePoints.forEach(insCodePoint(_))
      if (abbreviations != null && abbreviations.onLineTrigger) abbreviation()
    }

    def mvLeft(): Unit = if (left != 0) {
      left -= 1
      right -= 1
      buffer(right) = buffer(left)
    }

    def mvRight(): Unit = if (right != N) {
      buffer(left) = buffer(right)
      right += 1
      left += 1
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
    val marginChars: Int = size / 5 max 2
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
    private def reverseLeftCodePoints: Iterator[CodePoint] = new Iterator[CodePoint] {
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
      def apply(i: Int): CodePoint = buffer(i)
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
      cp
    }

    /**
     * POST: `allBMP = allBMP && buffer(i).isBMPCodePoint`
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
    @inline private def codePointWidth(codePoint: CodePoint): Scalar =
      if (Character.isBmpCodePoint(codePoint))
        font.getWidths(Array(font.getUTF32Glyph(codePoint)))(0) else {
        val chars = Character.toChars(codePoint)
        TextLine.make(new String(chars, 0, chars.length), font).getWidth
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
        if (lastTextLine eq null) 0 else lastTextLine.getLeftOffsetAtCoord(distance)
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

    def abbreviation(): Unit = if (abbreviations!=null) {
      abbreviations.reverseFindAbbreviation(reverseLeftCodePoints) match {
        case None =>
        case Some((repl, size)) =>
          if (!abbreviating) {
            abbreviating = true
            if (left >= size) left -= size
            ins(repl)
            abbreviating = false
          }
      }
    } else ()
  }
}

/**
 * Unstyled TextField companion object.
 *
 * @see styled.TextField
 */
object TextField extends logging.Loggable {
  import Location._
  def popupError(key: EventKey, glyph: Glyph): Unit = {
    import Modifiers._
    implicit object Style extends StyleSheet
    styled.windowdialogues.Dialogue.OK(unstyled.static.Label(s"Unknown key: ${toBitmap(key).toShortString} ${key._key}"), RelativeTo(glyph), "Error").start()
  }

  def apply(fg: Brush = fallback.textForeground, bg: Brush = fallback.textBackground, font: Font=fallback.textFont,
            onEnter: String=>Unit              = { case text: String => },
            onError: (EventKey, Glyph) => Unit = popupError(_,_),
            onCursorLeave: String=>Unit        = { case text: String => },
            onChange: Option[String=>Unit]     = None,
            size: Int,
            initialText: String = "",
            abbreviations: org.sufrin.utility.TextAbbreviations = null
           ): TextField =
      new TextField(fg, bg, font,
            onEnter=onEnter,
            onError=onError,
            onCursorLeave=onCursorLeave,
            onChange=onChange,
            size=size,
            initialText=initialText,
            abbreviations=abbreviations)
}


