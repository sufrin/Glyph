package org.sufrin.glyph

import GlyphTypes.Font

import io.github.humbleui.jwm.{EventKey, EventTextInput, EventTextInputMarked}

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
class TextField(val fg: Brush, val bg: Brush, font: Font,
                var onEnter: String => Unit,
                var onError: (EventKey, Glyph) => Unit,
                var onCursorLeave: String => Unit,
                size: Int,
                initialText: String
               ) extends ReactiveGlyph
{

  import io.github.humbleui.jwm.{EventMouseButton, Window}
  val em = Text("\uD83D\uDE1B", font)
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
  def diagonal: Vec = Vec(emDiagonal.x*size, emDiagonal.y)
  val atBaseLine = em.height

  private def focussed: Boolean = guiRoot.hasKeyboardFocus(this)

  /**
   * Accept textlayout input denoting a diacritical that will become part of a composite character.
   *
   * TODO: Clarify whether _selection..., _replacement... fields denote anything useable
   *       It's not clear from the example programs
   */
  override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
    //println(key)
    TextModel.insMarked(key.getText, 1)
    reDraw()
  }

  override def accept(key: EventTextInput, location: Vec, window: Window): Unit = {
    //println(key)
    TextModel.ins(key.getText)
    reDraw()
  }
  override def accept(key: EventKey, location: Vec, window: Window): Unit = {
    import Modifiers._

    import io.github.humbleui.jwm.{Clipboard, ClipboardEntry, ClipboardFormat}
    import io.github.humbleui.jwm.Key._
    val ANYCONTROL  = Control | Command
    val ANYSHIFT    = ANYCONTROL | Alt
    val mods: Bitmap = toBitmap(key)
    if (mods.include(Pressed)) key._key match {
      case END        => TextModel.end()
      case LEFT if mods.include(ANYCONTROL) => TextModel.end()
      case LEFT       => TextModel.mvLeft()
      case HOME       => TextModel.home()
      case RIGHT if mods.include(ANYCONTROL) => TextModel.home()
      case RIGHT      => TextModel.mvRight()
      case BACKSPACE  => TextModel.del()
      case DELETE     => TextModel.mvRight(); TextModel.del()
      case ENTER      => onEnter(text)
      case C if mods.include(ANYCONTROL) =>

        Clipboard.set(new ClipboardEntry(ClipboardFormat.TEXT, text.getBytes()))

      case X | U if mods.include(ANYCONTROL) =>
        Clipboard.set(new ClipboardEntry(ClipboardFormat.TEXT, text.getBytes()))
        TextModel.clear()

      case V if mods.include(ANYCONTROL) =>
        val entry = Clipboard.get(ClipboardFormat.TEXT).getString
        if (entry ne null) TextModel.ins(entry)

      case CONTROL | MAC_COMMAND | SHIFT | ALT | LINUX_SUPER =>

      case other  =>
        if (mods.include(ANYSHIFT)) onError(key, this)
    }
    //println(s"${TextModel} ${TextModel.revLeft} ${TextModel.right}")
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
  val focussedBrush = Brush("cursorfocussed") color 0Xff000000 strokeWidth 2.5f

  /**
   * Brush used to show the cursor when focussed
   */
  val unfocussedBrush = Brush("cursorunfocussed") color 0X33000000 strokeWidth 2.5f

  /** 
   * Brush used to show panned warnings
   */
  val panWarningBrush = fg.copy() strokeWidth 20.0f alpha 0.3
  
  /** 
   *  Offset from start/end of the glyph of x of the pan-warning stroke
   */
  val panWarningOffset = panWarningBrush.strokeWidth / 2

  /** The most recent origin of the displayed textlayout */
  var panBy = 0

  /**
   * Draw the glyph on the surface at its given size, with a "cursor" indicating the current
   * editing position.
   */
  def draw(surface: Surface): Unit = {
    //panBy = 0
    drawBackground(surface)
    surface.declareCurrentTransform(this)
    surface.withClip(diagonal) {
      // NB: The text is going to be aligned on the baseline
      // in case we start supporting mixed fonts in `TextField`s
      surface.withOrigin(location.x, atBaseLine) {
        val right = TextModel.rightText.atBaseline(fg)
        var left = TextModel.leftText(panBy).atBaseline(fg)
        // pan (move the display "window") rightwards if necessary to bring the cursor into view
        while (left.w >= w) {
          panBy += size/3
          left = TextModel.leftText(panBy).atBaseline(fg)
        }
        // println(s"${left.w} $size $w")

        // draw the visible left and the visible right
        left.draw(surface)
        surface.withOrigin(left.w, baseLine) {
          right.draw(surface)
        }
        // prepare to draw the cursor
        val cursorNudge = if (TextModel.left==0) focussedBrush.strokeWidth/2f else 0
        val cursorLeft = left.w + cursorNudge
        val cursorBrush: Brush = if (focussed) focussedBrush else unfocussedBrush
        surface.withOrigin(location.x, -atBaseLine) {
          // Draw the "cursor" as an I-Beam
          surface.drawPolygon$(cursorBrush, cursorLeft, cursorSerifShrink, cursorLeft, diagonal.y - cursorSerifShrink) // ertical
          surface.drawPolygon$(cursorBrush, cursorLeft - cursorSerifWidth, cursorSerifShrink, cursorLeft + cursorSerifWidth, cursorSerifShrink)
          surface.drawPolygon$(cursorBrush, cursorLeft - cursorSerifWidth, diagonal.y - cursorSerifShrink, cursorLeft + cursorSerifWidth, diagonal.y - cursorSerifShrink)

          // Indicate when there's invisible textlayout to the right
          if (left.w + right.w >= w) {
            surface.drawPolygon$(panWarningBrush, w - panWarningOffset, 0f, w - panWarningOffset, diagonal.y)
          }

          // cancel panning if at the left of the display while panned
          if (panBy>0 && left.w==0) {
            panBy = 0
          }

          // Indicate when there's invisible textlayout to the left
          if (panBy > 0) {
            surface.drawPolygon$(panWarningBrush, panWarningOffset, 0f, panWarningOffset, diagonal.y)
          }

        }
      }
    }
  }

def takeKeyboardFocus(): Unit = guiRoot.grabKeyboard(this)


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
      guiRoot.freeKeyboard()
  }

  override def accept(mouse: EventMouseButton, location: Vec, window: Window): Unit = {
    import Modifiers._
    val mods = toBitmap(mouse)
    val ctrl = Control|Command
    if (mouse.isPressed)
      if (mods.include(ctrl)) markTo(location.x) else moveTo(location.x)
    reDraw() // window.requestFrame()
  }

  /**
   * Inefficient implementation of cursor motion to a horizontal location, because
   * a `Text` object is constructed on each iteration.
   * The inefficiency can be overlooked at human-interaction speeds.
   */
  def moveTo(x: GlyphTypes.Scalar): Unit = {
    import TextModel._
    // println(s"${leftText(panBy).width} $x")
    while (leftText(panBy).width < x && hasRight) mvRight()
    while (leftText(panBy).width > x && hasLeft) mvLeft()
  }

  def markTo(x: GlyphTypes.Scalar): Unit = {
    import TextModel._
    while (leftText(panBy).width < x && hasRight) mvRight()
    while (leftText(panBy).width > x && hasLeft) mvLeft()
  }

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
    def leftString(from: Int):  String = new String(buffer, from, left-from)
    def rightString: String            = new String(buffer, right, N-right)

    /** The `Text` to the left of the cursor: for drawing */
    @inline def leftText:         Text = Text(leftString, font)
    /** The `Text` to the right of the cursor */
    @inline def rightText:        Text = Text(rightString, font)
    /** The `Text` to the left of the cursor from the `from`th character */
    @inline def leftText(from: Int): Text = Text(leftString(from), font)

    @inline def hasLeft:  Boolean = left!=0
    @inline def hasRight: Boolean = right!=N

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

    private var pendingReplacements: Int = 0
    private def doPendingReplacements(): Unit = while (pendingReplacements>0) { del(); pendingReplacements -= 1 }

    /** Insert a character from the Unicode BMP  */
    def ins(ch: Char): Unit = {
      doPendingReplacements()
      insCodePoint(ch)
    }

    /**
     *  Insert a string that may contain "surrogate pairs" arising from
     *  characters (for example Smileys, Kanji, ...) outside the BMP
     */
    def ins(string: String): Unit = {
      doPendingReplacements()
      string.codePoints.forEach(insCodePoint(_))
    }

    /**
     * Insert a string that may contain "surrogate pairs" arising from
     * characters (for example Smileys, Kanji, ...) outside the BMP.
     * `toReplace` characters are marked for replacement at the
     * following keystroke.
     */
    def insMarked(string: String, toReplace: Int): Unit = {
      pendingReplacements = toReplace
      string.codePoints.forEach(insCodePoint(_))
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
  }
}

/**
 * Unstyled TextField companion object.
 *
 * @see styled.TextField
 */
object TextField {
  import Location._
  def popupError(key: EventKey, glyph: Glyph): Unit = {
    import Glyphs.Label
    import Modifiers._
    import windowdialogues.Dialogue
    implicit object Style extends Styles.DefaultSheet
    Dialogue.OK(Label(s"Unknown key: ${toBitmap(key).toShortString} ${key._key}"), RelativeTo(glyph), "Error").start()
  }

  def apply(fg: Brush = Brushes.buttonForeground, bg: Brush = Brushes.buttonBackground, font: Font=Brushes.buttonFont,
            onEnter: String=>Unit              = { case text: String => },
            onError: (EventKey, Glyph) => Unit = popupError(_,_),
            onCursorLeave: String=>Unit        = { case text: String => },
            size: Int,
            initialText: String = ""
           ): TextField =
      new TextField(fg, bg, font,
            onEnter=onEnter,
            onError=onError,
            onCursorLeave=onCursorLeave,
            size=size,
            initialText=initialText)
}
