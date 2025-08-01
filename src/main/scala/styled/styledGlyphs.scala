package org.sufrin
package glyph
package styled
import org.sufrin.glyph.unstyled.{dynamic, reactive}
import org.sufrin.glyph.unstyled.reactive.Enterable

/** Intermediate definitions refactored from `styled` by (more or less) substitution */

import org.sufrin.glyph.Brushes.transparent
import org.sufrin.glyph.unstyled.BooleanGlyphs._
import org.sufrin.glyph.unstyled.reactive.Reaction

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
  def apply(reaction: Reaction)(implicit sheet: StyleSheet): Glyph
}

trait DetailedButton {
  def apply(reaction: Reaction)(implicit detail: StyleSheet): Glyph
}


/**
 *  A `ToggleButton` is a reactive glyph generator that specifies the essence of the appearance of
 *  a button-like reactive glyph that responds to a click of a mouse (or other pointer) button on the
 *  screen.
 *
 *  The state of the generated button is supplemented by a boolean that determines which of its appearances is shown.
 *  Clicking on the button inverts the boolean (thereby changing the button's appearance) and applies the `reaction` to the inverted boolean.
 *
 *  If the button is made with a `ToggleVariable` then it is styled and associated with the variable.
 *  Clicking the button changes the variable's value, and sets the states of all
 *  buttons associated with the variable.
 *  This is the way to generate several buttons that show and control a single boolean.
 */
trait ToggleButton { thisToggle =>
  /**
   *  Yield a styled  `OnOffButton` associated with this `variable`. This
   *  is the preferred usage.
   *
   *  The button's initial state is that of the `variable`.
   */
  def apply(variable: ToggleVariable)(implicit sheet: StyleSheet): Glyph = {
    val button = thisToggle.apply {
      state =>
        variable.value = state
        for { button <- variable.registered} button.set(state)
    }
    button.set(variable.get) //** 28JUL
    if (variable.get!=button.get) logging.SourceDefault.warn(s"Contradictory initial states ${variable.get} ${button.get}")
    variable.register(button)
    Decorate(button)
  }

  /**
   *  Yield the UNSTYLED  `OnOffButton`, with the given `reaction`. This
   *  is used only by implementations of `ToggleButton`.
   */
  protected def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton
}

class ToggleVariable(initially: Boolean, reaction: Boolean=>Unit) extends  BooleanVariable[OnOffButton](initially = initially, reaction = reaction)
object ToggleVariable {
  def apply(initially: Boolean)(reaction: Boolean=>Unit): ToggleVariable = new ToggleVariable(initially, reaction)
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
case class GlyphButton(up: Glyph, down: Glyph, hover: Glyph, exact: Boolean = true) extends styled.StyledButton {
  def apply(action: Reaction)(implicit sheet: StyleSheet): Glyph = {
    val detail = sheet.buttonStyle
    @inline def enlarged(glyph: Glyph): Glyph = glyph.enlarged(detail.border, transparent, transparent)
    val button =
      if (exact)
        Decorate(reactive.RawButton.exact(enlarged(up), enlarged(down), enlarged(hover))(action))
      else
        Decorate(reactive.RawButton(enlarged(up), enlarged(down), enlarged(hover))(action))
    button
  }
}

/**  As GlyphButton but destined for a menu; hence deferred decoration */
case class MenuGlyphButton(up: Glyph, down: Glyph = null, hover: Glyph = null, exact: Boolean = true) extends styled.StyledButton {
  def apply(action: Reaction)(implicit sheet: StyleSheet): Glyph = {
    val detail = sheet.buttonStyle
    @inline def reify(glyph: Glyph): Glyph = (if (glyph eq null) up() else glyph).enlarged(detail.border, transparent, transparent)
    val button =
      if (exact)
        (reactive.RawButton.exact(reify(up), reify(down), reify(hover))(action))
      else
        (reactive.RawButton(reify(up), reify(down), reify(hover))(action))
    button
  }
}



// TODO: a "defer" parameter for use on styled components destined for menus
//       this is because menu constructors decorate menu items post-hoc
object Decorate {
  def apply(glyph: Glyph)(implicit sheet: StyleSheet): Glyph = sheet.buttonStyle.frame.decorate(glyph)
}

object DecorateWithDetail {
  def apply(glyph: Glyph)(implicit sheet: StyleSheet): Glyph = sheet.buttonStyle.frame.decorate(glyph)
}

/**
 * Generates a button on which the given text is shown. The details
 * of how it is shown (font, foreground and background colours, etc)
 * in each of its states are specified by the (implicit) `ButtonStyle`,
 * as are the details of its frame, if any.
 *
 * The button's "sensitive" region does not include its decoration.
 */
case class LightweightTextButton(text: String, hint: Hint=NoHint) extends styled.StyledButton {
  def apply(action: Reaction)(implicit sheet: StyleSheet): Glyph = {
    val detail = sheet.buttonStyle
    val up     = detail.up.toGlyph(text, fg=detail.up.fg, bg=detail.up.bg)
    val down   = detail.down.fg
    val hover  = detail.hover.fg
    val button = new reactive.ColourButton(up, down, hover, react = action, background = false)
    hint(button)
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
case class TextButton(text: String, hint: Hint=NoHint) extends styled.StyledButton {
  def apply(action: Reaction)(implicit sheet: StyleSheet): Glyph = {
    val detail = sheet.buttonStyle
    val up     = Decorate(detail.up.toGlyph(text))
    val down   = Decorate(detail.down.toGlyph(text))
    val hover  = Decorate(detail.hover.toGlyph(text))
    val button = reactive.RawButton(up, down, hover)(action)
    hint(button)
    (button)
  }
}

case class DetailedTextButton(text: String, hint: Hint=NoHint) extends styled.DetailedButton {
  def apply(action: Reaction)(implicit detail: StyleSheet): Glyph = {
    val up     = DecorateWithDetail(detail.buttonStyle.up.toGlyph(text))
    val down   = DecorateWithDetail(detail.buttonStyle.down.toGlyph(text))
    val hover  = DecorateWithDetail(detail.buttonStyle.hover.toGlyph(text))
    val button = reactive.RawButton(up, down, hover)(action)
    hint(button)
    button
  }
}

/**
 * As `LightweightTextButton`, but allows the menu to defer decoration until all its buttons have
 * been constructed.
 */
case class LightweightMenuButton(text: String, hint: Hint=NoHint) extends styled.StyledButton {
  def apply(action: Reaction)(implicit sheet: StyleSheet): Glyph = {
    val detail = sheet.buttonStyle
    val up     = detail.up.toGlyph(text, fg=detail.up.fg, bg=detail.up.bg)
    val down   = detail.down.fg
    val hover  = detail.hover.fg
    val button = new reactive.ColourButton(up, down, hover, react=action, background = false)
    hint(button)
    (button)
  }
}

/**
 * As `TextButton`, but allows the menu to defer decoration until all its buttons have
 * been constructed.
 */
case class MenuButton(text: String, hint: Hint=NoHint) extends styled.StyledButton {
  def apply(action: Reaction)(implicit sheet: StyleSheet): Glyph = {
    val detail = sheet.buttonStyle
    val up = (detail.up.toGlyph(text))
    val down = (detail.down.toGlyph(text))
    val hover = (detail.hover.toGlyph(text))
    val button=reactive.RawButton(up, down, hover)(action)
    hint(button)
    button
  }
}


/** Generator for a sequence of text buttons of identical sizes.
 * Intended for use when constructing a row or column of
 * buttons that should all occupy the same space. The main
 * motivation for this was setting up the alignment of tab
 * buttons for notebooks.
 *
 * Individual buttons can be associated with hints in the usual way.
 *
 * @see Book
 */
object UniformSize {
  case class ButtonSpecification(text: String, action: Reaction, hint: Hint = NoHint)

  def apply(text: String)(action: Reaction): ButtonSpecification = ButtonSpecification(text, action)

  def constrained(buttonSpecs: Seq[ButtonSpecification])(implicit sheet: StyleSheet): Seq[ReactiveGlyph] = {
    val detail = sheet.buttonStyle
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
      for {i <- buttonSpecs.indices} yield {
        val button =
          reactive.RawButton(
            frame(upGlyphs(i)),
            frame(downGlyphs(i)),
            frame(hoverGlyphs(i)))(buttonSpecs(i).action)

        buttonSpecs(i).hint(button)
        button
      }

    buttons
  }
}

object Label {

  def apply(text: String, align: Alignment = Center)(implicit sheet: StyleSheet): Glyph = Label(text, align, sheet.labelStyle)

  def Label(text: String, align: Alignment = Center)(implicit sheet: StyleSheet): Glyph = Label(text, align, sheet.labelStyle)

  def apply(text: String, align: Alignment, detail: styles.GlyphStyle): Glyph = Label(text, align, detail)

  /**
   * As `Label` above but with an explicit `GlyphStyle` parameter
   */
  def Label(text: String, align: Alignment, detail: styles.GlyphStyle): Glyph = {
    import NaturalSize.Col
    val lines = text.split('\n').toList
    lines.length match {
      case 1 => unstyled.Text(text, detail.font, detail.fg, detail.bg)
      case _ => {
        val texts = lines.map { line => unstyled.Text(line, detail.font, detail.fg, detail.bg) }
        Col(align=align, bg = detail.bg)(texts)
      }
    }
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

case class TextToggle(whenTrue: String, whenFalse: String, initially: Boolean, hint: Hint=NoHint) extends ToggleButton {
  def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton = {
    //import styled.text.Label
    val detail = sheet.buttonStyle
    val offFG = detail.toggle.off.fg
    val offBG = detail.toggle.off.bg
    val onFG = detail.toggle.on.fg
    val onBG = detail.toggle.on.bg

    val whenTTrue: Glyph  = unstyled.Label(whenTrue, sheet.labelFont,  fg=onFG, bg=onBG)
    val whenFFalse: Glyph = unstyled.Label(whenFalse, sheet.labelFont, fg=offFG, bg=offBG)
    val Vec(w, h) = (whenTTrue.diagonal union whenFFalse.diagonal)

    unstyled.BooleanGlyphs(
      new OnOff(whenTrue  = whenTTrue.enlargedTo(w, h),
                whenFalse = whenFFalse.enlargedTo(w, h), initially = initially, fg = offFG, bg = offBG),
      initially = initially,
      fg = detail.toggle.off.fg,
      bg = detail.toggle.off.bg,
      hint,
      reaction)
  }
}

/**
 *  As TextToggle but destined for am menu; hence defer decoration
 */
case class MenuTextToggle(whenTrue: String, whenFalse: String, initially: Boolean, hint: Hint = NoHint) extends ToggleButton {
  def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton = {
    //import styled.text.Label
    val detail = sheet.buttonStyle
    val offFG = detail.toggle.off.fg
    val offBG = detail.toggle.off.bg
    val onFG = detail.toggle.on.fg
    val onBG = detail.toggle.on.bg

    val whenTTrue: Glyph  = Label(whenTrue, Center, detail.up)
    val whenFFalse: Glyph = Label(whenFalse, Center, detail.down)
    val Vec(w, h) = (whenTTrue.diagonal union whenFFalse.diagonal)

    unstyled.BooleanGlyphs(
      new OnOff(
        whenTrue  = (whenTTrue.enlargedTo(w, h)),
        whenFalse = (whenFFalse.enlargedTo(w, h)), initially = initially, fg = offFG, bg = offBG),
      initially = initially,
      fg = detail.toggle.off.fg,
      bg = detail.toggle.off.bg,
      hint,
      reaction)
  }
}

case class GlyphToggle(whenTrue: Glyph, whenFalse: Glyph, initially: Boolean, hint: Hint = NoHint) extends ToggleButton {

  def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton = {
    val ww=whenTrue.w max whenFalse.w
    val hh=whenTrue.h max whenFalse.h
    val detail = sheet.buttonStyle
    unstyled.BooleanGlyphs(new OnOff(whenTrue=Decorate(whenTrue.enlargedTo(ww,hh)),
                            whenFalse=Decorate(whenFalse.enlargedTo(ww,hh)),
                            initially=initially, fg=Brushes.transparent, bg=Brushes.transparent
                           ),
      initially=initially,
      fg=detail.toggle.off.fg,
      bg=detail.toggle.off.bg,
      hint,
      reaction)
  }
}

/** A glyphtoggle destined for a menu: defers decoration  */
case class MenuGlyphToggle(whenTrue: Glyph, whenFalse: Glyph, initially: Boolean, hint: Hint = NoHint) extends ToggleButton {

  def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton = {
    val ww=whenTrue.w max whenFalse.w
    val hh=whenTrue.h max whenFalse.h
    val detail = sheet.buttonStyle
    unstyled.BooleanGlyphs(new OnOff(
      whenTrue=(whenTrue.enlargedTo(ww,hh)),
      whenFalse=(whenFalse.enlargedTo(ww,hh)),
      initially=initially, fg=Brushes.transparent, bg=Brushes.transparent),
      initially=initially,
      fg=detail.toggle.off.fg,
      bg=detail.toggle.off.bg,
      hint,
      reaction)
  }
}

case class CheckBox(initially: Boolean, hint: Hint = NoHint) extends ToggleButton {
  def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton = {
    val detail = sheet.buttonStyle
    val tick = detail.checkbox.tick
    val cross = detail.checkbox.cross
    TextToggle(whenFalse=cross, whenTrue=tick, initially=initially, hint=hint)(reaction)
  }
}

case class ActiveString(initial: String)(implicit style: StyleSheet) extends dynamic.ActiveString(style.labelStyle.font, style.labelStyle.fg, style.labelStyle.bg, initial)


/** Checkbox destined for a menu; hence deferred decoration */
case class MenuCheckBox(initially: Boolean) extends ToggleButton {
  def apply(reaction: Boolean => Unit)(implicit sheet: StyleSheet): OnOffButton = {
    val detail = sheet.buttonStyle
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
class EventLog[Event](size: Int, lines: Int, keepLines: Int=0)(implicit style: StyleSheet) extends Glyph {
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
        val glyph = Label(toString(event))//Text(toString(event), font = style.font).asGlyph(style.fg)
        glyph.draw(surface)
        y += glyph.h
      }
    }
  }
  /**
   * The diagonal size of the glyph
   */
  def diagonal: Vec = {
    val em = Label("M")
    Vec(size * em.w, maxQueue * em.h)
  }

  /** A copy of this glyph; perhaps with different foreground/background */
  def copy(fg: Brush, bg: Brush): Glyph = new EventLog(size, lines, keepLines)

  override val bg: Brush = style.labelBackgroundBrush
  override val fg: Brush = style.labelForegroundBrush
}

class StringLog (size: Int, lines: Int, keepLines: Int)(implicit style: StyleSheet) extends EventLog[String](size, lines, keepLines) {
  def println(s: String): Unit = log(s)
}

object StringLog {

  def apply(size: Int, lines: Int, keepLines: Int=0)(implicit style: StyleSheet): StringLog =
    new StringLog(size, lines, keepLines)
}



object RadioCheckBoxes {
  def apply(captions: Seq[String], prefer: String = null, inheritFramed: Boolean = false)(action: Option[Int]=>Unit)
           (implicit sheet: StyleSheet): RadioCheckBoxes =
    new RadioCheckBoxes(captions, prefer, inheritFramed, action)(sheet)
}

/**
 * A group of captioned checkboxes, labelled with `captions`, and with `prefer` as the
 * caption of the to-be-checked-initially box.  At most one box can be checked at a time:
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
                      action: Option[Int]=>Unit)(implicit sheet: StyleSheet) {
  //import styled.text.Label

  val preferred  = if (prefer eq null) captions(0) else prefer

  val frameStyle: StyleSheet = sheet
  // TODO: fix this
//    if (inheritFramed)
//      sheet
//    else
//      sheet.unFramed

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
      glyphs += Label(s"${captions(i)}", Center, sheet.labelStyle)
      glyphs += checkBoxes(i)
    }
    glyphs.toSeq
  }

  lazy val glyphCols: Seq[Glyph] = {
    val glyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
    for {i <- 0 until captions.length } {
      glyphs += Label(s"${captions(i)}", Center, sheet.labelStyle)
    }
    for {i <- 0 until captions.length } { glyphs += checkBoxes(i) }
    glyphs.toSeq
  }

  def emTab: Glyph = FixedSize.Space(sheet.emWidth, 1, 1)

  def glyphButtons(align: Alignment=Justify): Seq[Glyph] = {
    val glyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
    val labels = captions.map{caption => Label(caption, Left, sheet.labelStyle)}

    align match {
      case Left =>
        for {i <- 0 until captions.length } {
          glyphs += NaturalSize.Row(align=Mid)(checkBoxes(i), emTab, labels(i)).enlargedBy(15, 0)
        }

      case Right =>
        val width = labels.map(_.w).max + checkBoxes.head.w*3
        for {i <- 0 until captions.length } {
          glyphs += FixedSize.Row(width, align=Mid)(labels(i), emTab, checkBoxes(i)).enlargedBy(15, 0)
        }

      case Center =>
        val width = labels.map(_.w).max + checkBoxes.head.w*3
        for {i <- 0 until captions.length } {
          glyphs += FixedSize.Row(width, align=Mid)(emTab, labels(i), emTab, checkBoxes(i)).enlargedBy(15, 0)
        }

      case Justify =>
        val width = labels.map(_.w).max + checkBoxes.head.w*3
        for {i <- 0 until captions.length } {
          glyphs += FixedSize.Row(width, align=Mid)(emTab, labels(i), sheet.em, checkBoxes(i)).enlargedBy(15, 0)
        }
    }

    glyphs.toSeq
  }

  def arrangedVertically(): Glyph =
    NaturalSize.Grid(bg=sheet.buttonBackgroundBrush).table(width=2)(glyphRows)

  def arrangedHorizontally(): Glyph =
    NaturalSize.Grid(bg=sheet.buttonBackgroundBrush, padx=5).table(width=captions.length)(glyphCols)

}



class Chooser(captions: Seq[String], prefer: String, inheritFramed: Boolean,
                      action: Option[Int]=>Unit)(implicit sheet: StyleSheet) {
  //import styled.text.Label

  val preferred  = if (prefer eq null) captions(0) else prefer

  val frameStyle: StyleSheet = sheet
  // TODO: fix this
  //    if (inheritFramed)
  //      sheet
  //    else
  //      sheet.unFramed

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
      glyphs += Label(s"${captions(i)}", Center, sheet.labelStyle)
      glyphs += checkBoxes(i)
    }
    glyphs.toSeq
  }

  lazy val glyphCols: Seq[Glyph] = {
    val glyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
    for {i <- 0 until captions.length } {
      glyphs += Label(s"${captions(i)}", Center, sheet.labelStyle)
    }
    for {i <- 0 until captions.length } { glyphs += checkBoxes(i) }
    glyphs.toSeq
  }

  def emTab: Glyph = FixedSize.Space(sheet.emWidth, 1, 1)

  def glyphButtons(align: Alignment=Justify): Seq[Glyph] = {
    val glyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
    val labels = captions.map{caption => Label(caption, Left, sheet.labelStyle)}

    align match {
      case Left =>
        for {i <- 0 until captions.length } {
          glyphs += NaturalSize.Row(align=Mid)(checkBoxes(i), emTab, labels(i)).enlargedBy(15, 0)
        }

      case Right =>
        val width = labels.map(_.w).max + checkBoxes.head.w*3
        for {i <- 0 until captions.length } {
          glyphs += FixedSize.Row(width, align=Mid)(labels(i), emTab, checkBoxes(i)).enlargedBy(15, 0)
        }

      case Center =>
        val width = labels.map(_.w).max + checkBoxes.head.w*3
        for {i <- 0 until captions.length } {
          glyphs += FixedSize.Row(width, align=Mid)(emTab, labels(i), emTab, checkBoxes(i)).enlargedBy(15, 0)
        }

      case Justify =>
        val width = labels.map(_.w).max + checkBoxes.head.w*3
        for {i <- 0 until captions.length } {
          glyphs += FixedSize.Row(width, align=Mid)(emTab, labels(i), sheet.em, checkBoxes(i)).enlargedBy(15, 0)
        }
    }

    glyphs.toSeq
  }

  def arrangedVertically(): Glyph =
    NaturalSize.Grid(bg=sheet.buttonBackgroundBrush).table(width=2)(glyphRows)

  def arrangedHorizontally(): Glyph =
    NaturalSize.Grid(bg=sheet.buttonBackgroundBrush, padx=5).table(width=captions.length)(glyphCols)

}