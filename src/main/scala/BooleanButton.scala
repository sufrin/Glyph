package org.sufrin.glyph

import GlyphTypes.Font


/**
 *  A family ofPaint boolean button constructors. Instantiate this trait and override
 *  the default definitions if necessary.
 *
 *  A boolean button shows glyphs that depend on its internal state (true or false).
 *  It inverts its state when its showing glyph is pressed, and also applies
 *  its `reaction` function to its (new) state.
 *
 * @see Styled for a convenient approach to the styling ofPaint buttons like these.
 * @see EarlyMenuTest for a very simple example.
 */
trait BooleanButton  {
  def buttonFont: Font = DefaultBrushes.buttonFont
  def fg: Brush = DefaultBrushes.black
  def bg: Brush = DefaultBrushes.white
  def toGlyph(string: String): Glyph = Text(string, buttonFont).asGlyph(fg, bg)
  def cross: Glyph = Text("✖", buttonFont).asGlyph(fg, bg)
  def tick: Glyph  = Text("✔", buttonFont).asGlyph(fg, bg)

  import BooleanGlyphs._

  /** An on-off button showing `toGlyph(whenTrue)` or `toGlyph(whenFalse)` */
  def onOff(whenTrue: String, whenFalse: String, initially: Boolean, fg: Brush, bg: Brush)
           (reaction: Boolean => Unit): OnOffButton = {
      BooleanGlyphs(new OnOff(whenTrue=toGlyph(whenTrue), whenFalse=toGlyph(whenFalse), initially, fg, bg),
        initially,
        fg, bg,
        reaction)
  }

  /** An on-off button showing `whenTrue` or `whenFalse` */
  def onOff(whenTrue: Glyph, whenFalse: Glyph,  initially: Boolean, fg: Brush, bg: Brush)
           (reaction: Boolean => Unit): OnOffButton = {
      new OnOffButton(new OnOff(whenTrue=whenTrue, whenFalse=whenFalse, initially, fg, bg), initially, fg, bg, reaction)
  }

  /** An on-off button showing a tick or a cross */
  def onOff(initially: Boolean, fg: Brush, bg: Brush )
           (reaction: Boolean => Unit): OnOffButton = {
      onOff(whenTrue=tick, whenFalse=cross, initially, fg, bg)(reaction)
  }

  /** A captioned on-off button associated with a `Variable[Boolean]` */
  def onOff(caption: String, variableState: Variable[Boolean], fg: Brush=DefaultBrushes.buttonForeground, bg: Brush = DefaultBrushes.buttonBackground)(reaction: Boolean => Unit): Glyph = {
      import Glyphs._
      NaturalSize.Row(Label(caption),
          onOff(whenTrue = tick, whenFalse = cross, initially=variableState.value, fg, bg){
                state =>
                  variableState.value=state
                  reaction(state)
          })
    }

}
