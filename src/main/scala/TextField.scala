package org.sufrin
package glyph

import io.github.humbleui.jwm.{EventKey, EventTextInput, EventTextInputMarked, Key}
import org.sufrin.glyph.GlyphTypes.{Font, Scalar}
import org.sufrin.glyph.unstyled.Text
import org.sufrin.logging.FINER


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
                abbreviations: utility.TextAbbreviations
               ) extends ReactiveGlyph
{

  import io.github.humbleui.jwm.{EventMouseButton, Window}
  val em = Text("M", font)
  val emDiagonal = Vec(em.width, em.drop)

  var onCursorEnter: String => Unit = {
    text => takeKeyboardFocus()
  }

  locally { TextModel.text = initialText }

  def text: String = TextModel.text

  def text_=(newText: String): Unit = {
    TextModel.text = newText
    reDraw()
  }

  /**
   * The diagonal size of the glyph
   */
  def diagonal: Vec = Vec(emDiagonal.x*size, emDiagonal.y*1.2)
  val atBaseLine = em.height
  val deltaY = emDiagonal.y*0.2f

  private def focussed: Boolean =
    if (hasGuiRoot) guiRoot.hasKeyboardFocus(this) else false

  /**
   * Accept textlayout input denoting a diacritical that will become part of a composite character.
   *
   * TODO: Clarify whether _selection..., _replacement... fields denote anything useable
   *       It's not clear from the example programs
   */
  override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
    val start= key.getReplacementStart
    val end  = key.getReplacementEnd
    // Cases I know of are for single accent characters
    TextModel.insForReplacement(key.getText, 1+end-start) // pending characters to delete
    if (onChange.isDefined) onChange.get.apply(text)
    reDraw()
    resetAbbreviationTrigger()
  }

  override def accept(key: EventTextInput, location: Vec, window: Window): Unit = {
    TextModel.ins(key.getText)
    if (onChange.isDefined) onChange.get.apply(text)
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

      case ENTER      => onEnter(text)
      case C if mods.includeSome(ANYCONTROL) =>
        Clipboard.set(new ClipboardEntry(ClipboardFormat.TEXT, text.getBytes()))

      case X if mods.includeSome(ANYCONTROL) =>
        Clipboard.set(new ClipboardEntry(ClipboardFormat.TEXT, text.getBytes()))
        TextModel.clear()

      case U if mods.includeSome(ANYCONTROL) =>
        if (mods.includeSome(Shift))
            while (TextModel.hasRight) {
              TextModel.mvRight()
              TextModel.del()
            }
          else
            while (TextModel.hasLeft) TextModel.del()

      case V if mods.includeSome(ANYCONTROL) =>
        while (TextModel.left>0) TextModel.del()

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

      case other  =>
        if (mods.includeSome(ANYSHIFT)) onError(key, this)
    }
    if (onChange.isDefined) onChange.get.apply(text)
    reDraw()
  }


  /** Width of the serifs of the I-beam drawn as the cursor */
  val cursorSerifWidth = 5f
  /**
   *  Top and bottom vertical shrink of the I-beam serifs
   */
  val cursorSerifShrink = 6.0f

  /**
   * Brush used to show the cursor when focussed
   */
  val focussedBrush = Brushes("black") strokeWidth 2.5f

  /**
   * Brush used to show the cursor when focussed
   */
  val unfocussedBrush = Brushes("0X33000000") strokeWidth 2.5f

  /**
   * Brush used to show panned warnings
   */
  val panWarningBrush = fg.copy() strokeWidth 20.0f alpha 0.3

  /**
   *  Offset from start/end of the glyph of x of the pan-warning stroke
   */
  val panWarningOffset = panWarningBrush.strokeWidth / 2

  /** The most recent origin of the displayed textlayout */
  def panBy: Int = TextModel.pan

  /**
   * Draw the glyph on the surface at its given size, with a "cursor" indicating the current
   * editing position.
   */
  def draw(surface: Surface): Unit = {
    drawBackground(surface)
    surface.declareCurrentTransform(this)
    surface.withClip(diagonal) {
      // NB: The text is going to be aligned on the baseline
      // in case we start supporting mixed fonts in `TextField`s
      surface.withOrigin(location.x, deltaY) {
        TextModel.rePan()
        val right = TextModel.rightText(fg)
        val left = TextModel.leftText(panBy, fg)
        //println(s"${left.baseLine} ${right.baseLine}")
        left.draw(surface)
        surface.withOrigin(left.w, 0) {
          right.draw(surface)
        }

        // prepare to draw the cursor
        val cursorNudge = if (TextModel.left==0) focussedBrush.strokeWidth/2f else 0
        val cursorLeft = left.w + cursorNudge
        val cursorBrush: Brush = if (focussed) focussedBrush else unfocussedBrush

        // show the text margins when logging
        if (TextField.loggingLevel(FINER)) {
          surface.drawPolygon$(cursorBrush(color=0XFFFF0000), w-TextModel.margin, 0, w-TextModel.margin, diagonal.y)
          surface.drawPolygon$(cursorBrush(color=0XFFFF0000), TextModel.margin, 0, TextModel.margin, diagonal.y)
        }
        // Draw the cursor as an I-Beam
        surface.withOrigin(location.x, 0) {
          surface.drawPolygon$(cursorBrush, cursorLeft, cursorSerifShrink-deltaY, cursorLeft, diagonal.y - cursorSerifShrink-deltaY) // ertical
          surface.drawPolygon$(cursorBrush, cursorLeft - cursorSerifWidth, cursorSerifShrink-deltaY, cursorLeft + cursorSerifWidth, cursorSerifShrink-deltaY)
          surface.drawPolygon$(cursorBrush, cursorLeft - cursorSerifWidth, diagonal.y - cursorSerifShrink-deltaY, cursorLeft + cursorSerifWidth, diagonal.y - cursorSerifShrink-deltaY)

          // Indicate when there's invisible text to the right
          if (left.w+right.w >= w) {
            surface.drawPolygon$(panWarningBrush, w - panWarningOffset, 0f, w - panWarningOffset, diagonal.y)
          }

          // Indicate when there's invisible text to the left
          if (panBy > 0) {
            surface.drawPolygon$(panWarningBrush, panWarningOffset, 0f, panWarningOffset, diagonal.y)
          }
        }
      }
    }
  }

def takeKeyboardFocus(): Unit = if (hasGuiRoot) guiRoot.grabKeyboard(this)
def giveUpKeyboardFocus(): Unit = if (hasGuiRoot) guiRoot.giveupFocus()



  /** A copy of this glyph; perhaps with different foreground/background */
  def copy(fg: Brush, bg: Brush): Glyph = null

  /** Seize focus on entry [prototype only] */
  override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = event match {
    case _: GlyphEnter =>
      import io.github.humbleui.jwm.MouseCursor
      onCursorEnter(text)
      window.setMouseCursor(MouseCursor.IBEAM)
      reDraw() // window.requestFrame()
    case _: GlyphLeave =>
      onCursorLeave(text)
      if (hasGuiRoot) guiRoot.freeKeyboard()
  }

  override def accept(mouse: EventMouseButton, location: Vec, window: Window): Unit = {
    import Modifiers._
    import TextModel.{markTo, moveTo}
    val mods = toBitmap(mouse)
    val ctrl = Control|Command
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
   *  When it needs to be shown, a couple of `Text` glyphs are made from it; this
   *  may well be quite inefficient, but it happens only at human-finger speed.
   *
   *  The model could easily be equipped with an undo/redo feature but it hardly
   *  seems worth doing so in the prototype toolkit.
   */
  object TextModel {
    type CodePoint = Int
    var buffer: Array[CodePoint] = Array.ofDim[CodePoint](size+3)
    @inline private def N = buffer.size
    var left  = 0
    var right = N
    def length: Int = left+N-right

    def clear(): Unit = { left=0; right = N }

    override def toString: String = s"TextField.TextModel(${leftString}, ${rightString})"

    def text: String = {
      val builder = new java.lang.StringBuilder
      for { cp <- 0     until left } builder.appendCodePoint(buffer(cp))
      for { cp <- right until N } builder.appendCodePoint(buffer(cp))
      builder.toString
    }

    def text_=(newText: String): Unit = {
      clear()
      ins(newText)
    }

    def leftString:  String            = new String(buffer, 0, left)
    def leftString(from: Int):  String = if (from<0 || left-from<=0) "" else new String(buffer, from, left-from)
    def rightString: String            = new String(buffer, right, N-right)

    /** The `Text` to the left of the cursor: for drawing */
    @inline def leftText: Text = Text(leftString, font)

    /** The `Text` to the right of the cursor */
    @inline def rightText(fg: Brush): Text = Text(rightString, font, fg, transient = true)

    /** The `Text` to the left of the cursor from the `from`th character */
    @inline def leftText(from: Int, fg: Brush): Text = Text(leftString(from), font, fg, transient = true)

    /** The entire `Text` */
    @inline def allText(from: Int): Text = Text(new String(buffer, from, left+N-right-from), font)

    @inline def hasLeft:  Boolean = left!=0
    @inline def hasRight: Boolean = right!=N

    /**
     * Implementation of cursor motion to a horizontal location.
     */
    def moveTo(x: GlyphTypes.Scalar): Unit = {
      val index = allText(panBy).charIndexOf(x)+panBy
      while (left<index) mvRight()
      while (left>index) mvLeft()
    }

    def markTo(x: GlyphTypes.Scalar): Unit = {
      val index = allText(panBy).charIndexOf(x)+panBy
      while (left<index) mvRight()
      while (left>index) mvLeft()
    }

    /**
     *   Grow buffer if necessary to make room for another few characters.
     */
    def ensureAdequateSize(): Unit = {
      val quantum = 10
      if (left==right) {
        val newBuffer = Array.ofDim[CodePoint](buffer.length+quantum)
        var newRight = newBuffer.size
        for { i <- 0 until left } newBuffer(i) = buffer(i)
        for { i <- right until N } {
           newRight -= 1
           newBuffer(newRight) = buffer(i)
        }
        buffer = newBuffer
        right  = newRight
      }
    }

    def isRight(cp: CodePoint): Boolean =
      (Character.getDirectionality(cp)) match {
      case Character.DIRECTIONALITY_RIGHT_TO_LEFT
         | Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC =>  true
      case _ => false
    }

    def insCodePoint(cp: CodePoint): Unit = {
      ensureAdequateSize()
      assert(left < right, s"TextModel(size=$size) is full")
      buffer(left) = cp
      left += 1
    }

    def del(): Unit  = {
      if (left != 0) left -= 1
    }

    def swap2(): Unit  =
      if (left>1) {
        val c = buffer(left-2)
        buffer(left-2) = buffer(left-1)
        buffer(left-1) = c
      }

    private var pendingDeletions: Int = 0
    private def doPendingDeletions(): Unit =
      while (pendingDeletions>0) { del(); pendingDeletions -= 1 }

    /** Insert a character from the Unicode BMP  */
    def ins(ch: Char): Unit = {
      doPendingDeletions()
      insCodePoint(ch)
      if (abbreviations!=null && abbreviations.onLineTrigger) abbreviation()
    }

    /**
     *  Insert a string that may contain "surrogate pairs" arising from
     *  characters (for example Smileys, Kanji, ...) outside the BMP
     */
    def ins(string: String): Unit = {
      doPendingDeletions()
      string.codePoints.forEach(insCodePoint(_))
      if (abbreviations!=null && abbreviations.onLineTrigger) abbreviation()
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
      if (abbreviations!=null && abbreviations.onLineTrigger) abbreviation()
    }

    def mvLeft(): Unit = if (left!=0) {
      left -= 1
      right -= 1
      buffer(right) = buffer(left)
    }

    def mvRight(): Unit = if (right!=N) {
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
      while (right<N) {
        buffer(left) = buffer(right)
        left += 1
        right += 1
      }
    }

    var pan:         Int = 0

    // Define a sensible margin for panning
    val marginChars: Int      = size/10 max 1
    val margin: Scalar        = Text("M"*(marginChars), font).width

    def rePan(): Unit = {
      import TextField.{finest, logging}
      val size: Scalar = w
      @inline def vleft: Scalar = leftText(pan, fg).width
      if (logging) finest(s"rePan: $pan $vleft $size $margin ${ (vleft < size, vleft<margin, vleft>=size-margin)}")
      if (leftText.width<size) {
        pan = 0
        if (logging) finest("<<")
      } else {
        (vleft < size, vleft<margin, vleft>=size-margin) match {
          case (true, _, true) => // visible, but in the right margin
            if (logging) finest("RM")
            if (rightMargin) while (vleft>=size-margin) pan += marginChars
          case (true, true, _) => // visible, but in left margin
            if (logging) finest("LM")
            while (vleft<margin) pan -= marginChars
          case (true, false, false)  => // still visible
            if (logging) finest("V")
          case _ =>
            if (logging) finest("J")
            while (0<=vleft && vleft<=size && pan-marginChars>=0)
              pan -= marginChars
            while (vleft>=size)
              pan += marginChars
        }
      }
      if (logging) finest(s"rePan= $pan $vleft $size $margin ${ (vleft < size, vleft<margin, vleft>=size-margin)}")
    }

    var abbreviating: Boolean = false

    def leftCodePoints: Seq[CodePoint] =
        buffer.toIndexedSeq.take(left)

    def abbreviation(): Unit = if (abbreviations!=null) {
      abbreviations.findAbbreviation(leftCodePoints, left) match {
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
            abbreviations: utility.TextAbbreviations = null
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
