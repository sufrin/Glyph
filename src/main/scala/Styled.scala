package org.sufrin.glyph.styled
import org.sufrin.glyph.Glyphs.nothing
import org.sufrin.glyph.OnOffButton._
import org.sufrin.glyph.ReactiveGlyphs.Reaction
import org.sufrin.glyph.Styles.{ButtonStyle, GlyphStyle}
import org.sufrin.glyph._

import scala.collection.mutable.ArrayBuffer

/**
 *  Systematic construction of glyphs using implicit styles.
 */


  /**
   * A `StyledButton` is a reactive glyph generator that specifies the essence of the appearance of
   * a button-like reactive glyph that responds to a click of a mouse (or other pointer) button on the
   * screen.
   *
   * Applying a `StyledButton` to a `Reaction` yields a (reactive) `Glyph`, that responds to mouse movement and clicks.
   *
   *   type ButtonState = Modifiers.toBitMap
   *   type Reaction = ButtonState => Unit
   *
   * Mouse movements and button presses change the tracking state of a button, which is one of:
   * {{{
   *   up:      when the mouse cursor is nowhere near it
   *   hovered: when the mouse cursor is contained by it
   *   down:    when it is hovered, and a mouse button is held down
   * }}}
   *
   * The tracking state of a button (usually) determines how it is shown, and different button implementations
   * provide visual feedback about their tracking state in different ways.
   *
   * A button's `reaction` is invoked (ie applied to the state of the buttons and keyboard shift keys)
   * when the mouse button is released in the `down` tracking state. Note that a "click" can be abandoned
   * by dragging the mouse far enough away from the button for its tracking
   * state to change to `up` before the button is released.
   *
   * @see GlyphButton
   * @see TextButton
   */
  trait StyledButton {
    def apply(reaction: Reaction)(implicit detail: ButtonStyle): Glyph
  }


/**
 *  A `ToggleButton` is a reactive glyph generator that specifies the essence of the appearance of
 *  a button-like reactive glyph that responds to a click of a mouse (or other pointer) button on the
 *  screen.
 *
 *  The generated button has the physical appearance of a `StyledButton`, but its state is supplemented by
 *  a boolean that determines which of its appearances is shown. Clicking on the button inverts the boolean (thereby changing
 *  the button's appearance) and applies the `reaction` to the inverted boolean.
 */
 trait ToggleButton {
    def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton
  }

  /**
   * Generates a button on which the given glyphs are shown, according
   * to its tracking state. This is determined by whether the cursor is nowhere
   * near it (up), hovering over it (hover), or pressed on it (down).
   * Its bounding box is the union of the bounding boxes of the component
   * glyphs.
   *
   * Unless `exact` is false, the EXACT shapes of the glyphs determine which of
   * its states it is in.
   *
   * Usually the shape is considered to be the bounding box of the glyph;
   * but for `Polygon` and `FilledPolygon` glyphs (and others like them as yet to be defined),
   * the shape is considered to be the set of points within the polygon.
   *
   * The cursor must be contained by the `up` glyph before
   * it is considered to be hovering; and must be contained by the `hover` glyph
   * before pressing the mouse button on it is considered a press.
   *
   * The details of its frame, if any, are specified by the (implicit) `ButtonStyle`.
   */
  case class GlyphButton(up: Glyph, down: Glyph, hover: Glyph, exact: Boolean = true) extends StyledButton {
    def apply(action: Reaction)(implicit detail: ButtonStyle): Glyph = {
      @inline def enlarged(glyph: Glyph): Glyph = glyph.enlarged(detail.border, nothing, nothing)
      val button =
        if (exact)
          Decorate(ReactiveGlyphs.RawButton.exact(enlarged(up), enlarged(down), enlarged(hover))(action))
         else
          Decorate(ReactiveGlyphs.RawButton(enlarged(up), enlarged(down), enlarged(hover))(action))
      button
    }
  }

  /**  As GlyphButton but destined for a menu; hence deferred decoration */
  case class MenuGlyphButton(up: Glyph, down: Glyph = null, hover: Glyph = null, exact: Boolean = true) extends StyledButton {
    def apply(action: Reaction)(implicit detail: ButtonStyle): Glyph = {
      @inline def reify(glyph: Glyph): Glyph = (if (glyph eq null) up() else glyph).enlarged(detail.border, nothing, nothing)
      val button =
        if (exact)
          (ReactiveGlyphs.RawButton.exact(reify(up), reify(down), reify(hover))(action))
        else
          (ReactiveGlyphs.RawButton(reify(up), reify(down), reify(hover))(action))
      button
    }
  }



  // TODO: a "defer" parameter for use on styled components destined for menus
  //       this is because menu constructors decorate menu items post-hoc
  object Decorate {
    def apply(glyph: Glyph)(implicit detail: ButtonStyle): Glyph = detail.frame.decorate(glyph)
  }

  /**
   * Generates a button on which the given text is shown. The details
   * of how it is shown (font, foreground and background colours, etc)
   * in each of its states are specified by the (implicit) `ButtonStyle`,
   * as are the details of its frame, if any.
   *
   * The button's "sensitive" region does not include its decoration.
   */
  case class LightweightTextButton(text: String) extends StyledButton {
    def apply(action: Reaction)(implicit detail: ButtonStyle): Glyph = {
      val up     = detail.up.toGlyph(text, fg=detail.up.fg, bg=detail.up.bg)
      val down   = detail.down.fg
      val hover  = detail.hover.fg
      val button = new ReactiveGlyphs.ColourButton(up, down, hover, react = action, background = false)
      Decorate(button)
    }
  }

  /**
   * Generates a button on which the given text is shown. The details
   * of how it is shown (font, foreground and background colours, etc)
   * in each of its states are specified by the (implicit) `ButtonStyle`,
   * as are the details of its frame, if any.
   *
   * The button's "sensitive" region includes its decoration.
   */
  case class TextButton(text: String) extends StyledButton {
    def apply(action: Reaction)(implicit detail: ButtonStyle): Glyph = {
      val up     = Decorate(detail.up.toGlyph(text))
      val down   = Decorate(detail.down.toGlyph(text))
      val hover  = Decorate(detail.hover.toGlyph(text))
      val button = ReactiveGlyphs.RawButton(up, down, hover)(action)
      button
    }
  }

  /**
   * As `LightweightTextButton`, but allows the menu to defer decoration until all its buttons have
   * been constructed.
   */
  case class LightweightMenuButton(text: String) extends StyledButton {
      def apply(action: Reaction)(implicit detail: ButtonStyle): Glyph = {
        val up     = detail.up.toGlyph(text, fg=detail.up.fg, bg=detail.up.bg)
        val down   = detail.down.fg
        val hover  = detail.hover.fg
        val button = new ReactiveGlyphs.ColourButton(up, down, hover, react=action, background = false)
        (button)
      }
  }

/**
 * As `TextButton`, but allows the menu to defer decoration until all its buttons have
 * been constructed.
 */
case class MenuButton(text: String) extends StyledButton {
  def apply(action: Reaction)(implicit detail: ButtonStyle): Glyph = {
      val up = (detail.up.toGlyph(text))
      val down = (detail.down.toGlyph(text))
      val hover = (detail.hover.toGlyph(text))
      ReactiveGlyphs.RawButton(up, down, hover)(action)
    }
}


/** Generator for a sequence of text buttons of identical sizes.
   * Intended for use when constructing a row or column of
   * buttons that should all occupy the same space. The main
   * motivation for this was setting up the alignment of tab
   * buttons for notebooks.
   *
   * @see Notebook
   */
  object UniformSize {
    case class ButtonSpecification(text: String, action: Reaction)
    def apply(text: String)(action: Reaction): ButtonSpecification = ButtonSpecification(text, action)

    def constrained(buttonSpecs: Seq[ButtonSpecification])(implicit detail: ButtonStyle): Seq[ReactiveGlyph] = {
      val upGlyphs    = buttonSpecs.map {b => detail.up.toGlyph (b.text)}
      val downGlyphs  = buttonSpecs.map {b => detail.down.toGlyph (b.text)}
      val hoverGlyphs = buttonSpecs.map {b => detail.hover.toGlyph (b.text)}

      val border = detail.border

      val theWidth = border +
        (upGlyphs.map (_.w).max max
         downGlyphs.map (_.w).max max
         hoverGlyphs.map (_.w).max)

      val theHeight = border +
        (upGlyphs.map (_.h).max max
         downGlyphs.map (_.h).max max
         hoverGlyphs.map (_.h).max)

      def frame(glyph: Glyph): Glyph = {
      Decorate (glyph.enlargedTo (theWidth, theHeight) )
    }

      val buttons =
      for {i <- 0 until buttonSpecs.length} yield
         ReactiveGlyphs.RawButton (frame (upGlyphs (i) ),
                                   frame (downGlyphs (i) ),
                                   frame (hoverGlyphs (i) ) ) (buttonSpecs (i).action)

      buttons
    }
  }

/**
 * Generates a button on which the given texts are shown, according to
 * its internal boolean state. The details
 * of how they are shown (font, foreground and background colours, etc)
 * in each of its states are specified by the (implicit) `ButtonStyle`,
 * as are the details of its frame, if any.
 *
 * @param whenTrue the text to show when the boolean state is true
 * @param whenFalse the text to show when the boolean state is false
 * @param initially the initial boolean state
 */

  case class TextToggle(whenTrue: String, whenFalse: String, initially: Boolean) extends ToggleButton {
    def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton = {
      import styled.TextLayout.TextLabel

      val offFG = detail.toggle.off.fg
      val offBG = detail.toggle.off.bg
      val onFG = detail.toggle.on.fg
      val onBG = detail.toggle.on.bg

      val whenTTrue: Glyph  = TextLabel(whenTrue)(detail.up)
      val whenFFalse: Glyph = TextLabel(whenFalse)(detail.down)
      val Vec(w, h) = (whenTTrue.diagonal union whenFFalse.diagonal)

      OnOffButton(
        new OnOff(whenTrue  = Decorate(whenTTrue.enlargedTo(w, h)),
                  whenFalse = Decorate(whenFFalse.enlargedTo(w, h)), initially = initially, fg = offFG, bg = offBG),
                  initially = initially,
        fg = detail.toggle.off.fg,
        bg = detail.toggle.off.bg,
        reaction)
     }
  }

 /**
  *  As TextToggle but destined for am menu; hence defer decoration
  */
  case class MenuTextToggle(whenTrue: String, whenFalse: String, initially: Boolean) extends ToggleButton {
    def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton = {
      import styled.TextLayout.TextLabel

      val offFG = detail.toggle.off.fg
      val offBG = detail.toggle.off.bg
      val onFG = detail.toggle.on.fg
      val onBG = detail.toggle.on.bg

      val whenTTrue: Glyph  = TextLabel(whenTrue)(detail.up)
      val whenFFalse: Glyph = TextLabel(whenFalse)(detail.down)
      val Vec(w, h) = (whenTTrue.diagonal union whenFFalse.diagonal)

      OnOffButton(
        new OnOff(
          whenTrue  = (whenTTrue.enlargedTo(w, h)),
          whenFalse = (whenFFalse.enlargedTo(w, h)), initially = initially, fg = offFG, bg = offBG),
        initially = initially,
        fg = detail.toggle.off.fg,
        bg = detail.toggle.off.bg,
        reaction)
    }
  }

  case class GlyphToggle(whenTrue: Glyph, whenFalse: Glyph, initially: Boolean) extends ToggleButton {
    import OnOffButton._

    def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton = {
      val ww=whenTrue.w max whenFalse.w
      val hh=whenTrue.h max whenFalse.h
      OnOffButton(new OnOff(whenTrue=Decorate(whenTrue.enlargedTo(ww,hh)),
                            whenFalse=Decorate(whenFalse.enlargedTo(ww,hh)),
                            initially=initially, fg=NOTHING, bg=NOTHING),
        initially=initially,
        fg=detail.toggle.off.fg,
        bg=detail.toggle.off.bg,
        reaction)
    }
  }

/** A glyphtoggle destined for a menu: defers decoration  */
case class MenuGlyphToggle(whenTrue: Glyph, whenFalse: Glyph, initially: Boolean) extends ToggleButton {
  import OnOffButton._

  def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton = {
    val ww=whenTrue.w max whenFalse.w
    val hh=whenTrue.h max whenFalse.h
    OnOffButton(new OnOff(
      whenTrue=(whenTrue.enlargedTo(ww,hh)),
      whenFalse=(whenFalse.enlargedTo(ww,hh)),
      initially=initially, fg=NOTHING, bg=NOTHING),
      initially=initially,
      fg=detail.toggle.off.fg,
      bg=detail.toggle.off.bg,
      reaction)
  }
}

  case class CheckBox(initially: Boolean) extends ToggleButton {
    def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton = {
        val tick = detail.checkbox.tick
        val cross = detail.checkbox.cross
        TextToggle(whenFalse=cross, whenTrue=tick, initially=initially)(reaction)
    }
  }

/** Checkbox destined for a menu; hence deferred decoration */
case class MenuCheckBox(initially: Boolean) extends ToggleButton {
  def apply(reaction: Boolean => Unit)(implicit detail: ButtonStyle): OnOffButton = {
    val tick = detail.checkbox.tick
    val cross = detail.checkbox.cross
    MenuTextToggle(whenFalse=cross, whenTrue=tick, initially=initially)(reaction)
  }
}



  /**
   * A bounded log to record strings reported by its `println` method.
   * The log may be longer than the size of the window in which the most
   * recent reports are shown.
   */
  class EventLog[Event](size: Int, lines: Int, keepLines: Int=0)(implicit detail: GlyphStyle) extends Glyph {
      import scala.collection.mutable
      override def toString: String = s"EventLog($size, $lines, $keepLines)"

      val maxQueue = if (keepLines==0) lines else keepLines
      val queue = new mutable.Queue[Event](maxQueue)
      def canElide(ev: Event): Boolean = false
      def toString(ev: Event): String = ev.toString

      def log(ev: Event): Unit = {
        if (canElide(ev)) {
          // elide successive moves
        } else {
          if (queue.size == maxQueue) queue.dequeue()
          queue.enqueue(ev)
          reDraw()
        }
      }

      /**
       * Draw the glyph on the surface at its given size (as if at the origin).
       */
      def draw(surface: Surface): Unit = {
        drawBackground(surface)
        var y = 0f
        for {event <- queue} {
          surface.withOrigin(2, y) {
            val glyph = Text(toString(event), font = detail.font).asGlyph(detail.fg)
            glyph.draw(surface)
            y += glyph.h
          }
        }
      }
      /**
       * The diagonal size of the glyph
       */
      def diagonal: Vec = {
        val em = Text("M", detail.font).asGlyph()
        Vec(size * em.w, maxQueue * em.h)
      }

      /** A copy of this glyph; perhaps with different foreground/background */
      def copy(fg: Brush, bg: Brush): Glyph = new EventLog(size, lines, keepLines)

      val fg: Brush = detail.fg
      val bg: Brush = detail.bg
  }

  class StringLog (size: Int, lines: Int, keepLines: Int)(detail: GlyphStyle) extends EventLog[String](size, lines, keepLines)(detail) {
    def println(s: String): Unit = log(s)
  }

  object StringLog {

    import Styles.GlyphStyle

    def apply(size: Int, lines: Int, keepLines: Int=0)(detail: GlyphStyle): StringLog =
      new StringLog(size, lines, keepLines)(detail)
  }

/** Styled and unstyled TextFields are implemented by the same class. */
object TextField {

  import GlyphTypes.Font
  import io.github.humbleui.jwm.EventKey

  def apply(onEnter: String => Unit            = { case text: String => },
            onError: (EventKey, Glyph) => Unit = { case (key, glyph) => },
            onCursorLeave: String=>Unit        = { case text: String => },
            size:    Int,
            initialText: String = ""
           )
            (implicit detail: Styles.GlyphStyle): TextField = {
    val fg: Brush = detail.fg
    val bg: Brush = detail.bg
    val font: Font = detail.font
    new TextField(fg, bg, font, onEnter, onError, onCursorLeave, size, initialText)
  }
}

object RadioCheckBoxes {
  def apply(captions: Seq[String], prefer: String = null, inheritFramed: Boolean = false)(action: Option[Int]=>Unit)(implicit detail: Styles.Basic): RadioCheckBoxes =
       new RadioCheckBoxes(captions, prefer, inheritFramed, action)(detail)
}

/**
 * A group of captioned checkboxes, labelled with `captions`, and with `prefer` as the
 * caption of the to-be-checkedinitially box.  At most one box can be checked at a time:
 * so whenever one is checked all the others are unchecked. When the currently-checked box
 * is clicked and becomes unchecked, then the box (if any) captioned `prefer` is checked.
 * @param captions
 * @param prefer
 * @param inheritFramed
 * @param action invoked after a checkbox changes state,
 *               with argument `None` if none of the boxes is checked, and
 *               `Some(ix)` if the `ix`the box is checked.
 * @param detail implicit style applied while constructing the checkboxes.
 */
class RadioCheckBoxes(captions: Seq[String], prefer: String, inheritFramed: Boolean,
                      action: Option[Int]=>Unit)(implicit detail: Styles.Basic) {
  import styled.TextLayout.TextLabel

  val preferred  = if (prefer eq null) captions(0) else prefer

  val frameStyle =
    if (inheritFramed)
      detail.buttonStyle
    else
      detail.buttonStyle.copy(frame = Styles.Decoration.Unframed)

  lazy val checkBoxes =
       for {i <- 0 until captions.length} yield
         CheckBox(initially = (captions(i) == preferred))(reaction(i))(frameStyle)

  def reaction(boxIndex: Int): Boolean => Unit = {
    case true =>
      for {i <- 0 until captions.length if i != boxIndex } checkBoxes(i).set(false)
      action(Some(boxIndex))
    case false =>
      action(None)
  }

  def select(boxIndex: Int): Unit = {
    assert(boxIndex<checkBoxes.length, s"RadioCheckBoxes.select($boxIndex) index out of range")
    checkBoxes(boxIndex).set(true)
  }



  lazy val glyphRows: Seq[Glyph] = {
    val glyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
    for {i <- 0 until captions.length } {
       glyphs += TextLabel(s"${captions(i)}")(detail.labelStyle)
       glyphs += checkBoxes(i)
    }
    glyphs.toSeq
  }

  lazy val glyphCols: Seq[Glyph] = {
    val glyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
    for {i <- 0 until captions.length } {
      glyphs += TextLabel(s"${captions(i)}")(detail.labelStyle)
    }
    for {i <- 0 until captions.length } {
      glyphs += checkBoxes(i)
    }
    glyphs.toSeq
  }

  def gridGlyphs(width: Int=0, height: Int=captions.length): Glyph =
    NaturalSize.Grid.table(width, height)(glyphCols).framed()


}


