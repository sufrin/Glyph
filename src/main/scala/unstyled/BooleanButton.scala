package org.sufrin.glyph
package unstyled

import GlyphTypes.Font



/**
 *  A family of boolean button constructors. Instantiate this trait and override
 *  the default definitions if necessary.
 *
 *  A boolean button shows glyphs that depend on its internal state (true or false).
 *  It inverts its state when its showing glyph is pressed, and also applies
 *  its `reaction` function to its (new) state.
 *
 * @see Styled for a convenient approach to the styling of buttons like these.
 * @see EarlyMenuTest for a very simple example.
 */
trait BooleanButton  {
  def buttonFont: Font = DefaultBrushes.buttonFont
  def fg: Brush = DefaultBrushes.black
  def bg: Brush = DefaultBrushes.white
  def toGlyph(string: String): Glyph = Text(string, buttonFont, fg, bg)
  def cross: Glyph = Text("✖", buttonFont, fg, bg)
  def tick: Glyph  = Text("✔", buttonFont, fg, bg)

  import BooleanGlyphs._

  /** An on-off button showing `toGlyph(whenTrue)` or `toGlyph(whenFalse)` */
  def onOff(whenTrue: String, whenFalse: String, initially: Boolean, fg: Brush, bg: Brush, hint: Hint=NoHint)
           (reaction: Boolean => Unit): OnOffButton = {
      BooleanGlyphs(new OnOff(whenTrue=toGlyph(whenTrue), whenFalse=toGlyph(whenFalse), initially, fg, bg),
        initially,
        fg, bg,
        hint,
        reaction)
  }

  /** An on-off button showing `whenTrue` or `whenFalse` */
  def onOff(whenTrue: Glyph, whenFalse: Glyph,  initially: Boolean, fg: Brush, bg: Brush, hint: Hint)
           (reaction: Boolean => Unit): OnOffButton = {
      val button =
          new OnOffButton(new OnOff(whenTrue=whenTrue, whenFalse=whenFalse, initially, fg, bg), initially, fg, bg, reaction)
      hint(button)
      button

  }

  /** An on-off button showing a tick or a cross */
  def onOff(initially: Boolean, fg: Brush, bg: Brush, hint: Hint)
           (reaction: Boolean => Unit): OnOffButton =
              onOff(whenTrue=tick, whenFalse=cross, initially, fg, bg, hint)(reaction)


  /** A captioned on-off button associated with a `Variable[Boolean]` */
  def onOff(caption: String, variableState: Variable[Boolean], fg: Brush, bg: Brush, hint: Hint)
           (reaction: Boolean => Unit): Glyph = {
      NaturalSize.Row(
          unstyled.static.Label(caption),
          onOff(whenTrue = tick, whenFalse = cross, initially=variableState.value, fg, bg, hint){
                state =>
                  variableState.value=state
                  reaction(state)
          })
      }

}
