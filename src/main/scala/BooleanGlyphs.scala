package org.sufrin.glyph

import DefaultBrushes.nothing

object BooleanGlyphs {

    import ReactiveGlyphs.{GenericButton, RawButton}

    import javax.swing.AbstractButton


    /** A settable `OneOf`  showing `whenTrue` if on, and `whenFalse` if off */
    class OnOff(whenTrue: Glyph, whenFalse: Glyph, initially: Boolean, fg: Brush, bg: Brush) extends Settable[Boolean]
    { val oneOf = DynamicGlyphs.OneOf(bg=DefaultBrushes.nothing, enableBG = false)(whenFalse, whenTrue)

      /** Set the state */
      def set(state: Boolean): Unit = { oneOf.select(if (state) 1 else 0) }

      /** Get the state */
      def get: Boolean = if (oneOf.selection == 0) false else true

      /** Invert the state then get it */
      def invert(): Boolean = { set(!get); get }

      locally {
        set(initially)
      }
    }


    /** A button used to "drive" a `OnOff` */
    class OnOffButton(tickBox: OnOff, initially: Boolean, fg: Brush, bg: Brush, reaction: Boolean => Unit)
      extends RawButton(tickBox.oneOf, tickBox.oneOf, tickBox.oneOf, fg, bg=nothing,
                        react = { _ => reaction(tickBox.invert()) })
      with Settable[Boolean] {
      def set(state: Boolean): Unit = tickBox.set(state)
      def get: Boolean = tickBox.get
      def invert(): Boolean = tickBox.invert()
      locally {
        set(initially)
      }
    }

    def apply(tickBox:    OnOff,
              initially:  Boolean,
              fg:         Brush=DefaultBrushes.buttonForeground,
              bg:         Brush=DefaultBrushes.buttonBackground,
              hint:       Hint=NoHint,
              reaction:   Boolean => Unit): OnOffButton = {
       val button = new OnOffButton(tickBox: OnOff, initially: Boolean, fg, bg, reaction: Boolean => Unit)
       hint(button)
       button
    }
}
