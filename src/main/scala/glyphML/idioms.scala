package org.sufrin.glyph
package glyphML

import styled.ToggleVariable
import unstyled.reactive.Reaction

/**
 * An experiment in providing idiomatic support for building style-dependent (mostly reactive) glyphs to add to
 * translation definitions.
 */
object idioms {

  def Label(text: String): StyleSheet => Glyph =
  {style: StyleSheet => styled.Label(text)(style)}

  def TextButton(name: String): Reaction=>StyleSheet=>Glyph  =
    (reaction: Reaction) => {style: StyleSheet=>styled.TextButton(name)(reaction)(style)}

  def TextToggle(whenTrue: String="❎", whenFalse: String="✅", hint: Hint=NoHint): ToggleVariable => StyleSheet => Glyph =
    (variable: ToggleVariable) => {style: StyleSheet=>styled.TextToggle(whenTrue, whenFalse, variable.value, hint)(variable)(style)}

  def CheckBox(variable: ToggleVariable, hintText: String="", seconds: Double=3.0, fontScale: Double=0.6f): StyleSheet => Glyph =
      {style: StyleSheet =>
        val smaller = style.copy(fontScale=fontScale.toFloat)
        val hint = if (hintText.isEmpty) NoHint else Hint(seconds, hintText, true)(smaller)
        styled.CheckBox(initially=variable.value, hint=hint)(variable)(style) }

  def hint(text: String, seconds: Double = 3.0): StyleSheet => Hint = { style: StyleSheet => Hint(seconds, text, true)(style) }

  def Table(cols: Int=0, rows: Int=0, uniform: Boolean=false)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph = fromSequence.Table(cols, rows, uniform)(glyphs)
  def Col(align: Alignment=Center)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph = fromSequence.Col(align)(glyphs)
  def Row(align: VAlignment=Mid)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph = fromSequence.Row(align)(glyphs)

  /** Constructions for `Row, Col, Table` that take `Seq[Glyph]` arguments  */
  object fromSequence {
    def Col(align: Alignment=Center)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = {
      style: StyleSheet =>
        val reified = glyphs.map(_.apply(style))
        NaturalSize.Col(align = align)(reified)
    }

    def Row(align: VAlignment=Mid)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = {
      style: StyleSheet =>
        val reified = glyphs.map(_.apply(style))
        NaturalSize.Row(align = align)(reified)
    }

    def Table(cols: Int=0, rows: Int=0, uniform: Boolean=false)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = { style: StyleSheet =>
      import style.{padX, padY, backgroundBrush => bg, foregroundBrush => fg}
      val reified = glyphs.map(_.apply(style))
      val Grid = NaturalSize.Grid(fg = fg, bg = bg, padX, padY)
      val buildFrom = if (uniform) Grid.grid(height = rows, width = cols)(_) else Grid.table(height = rows, width = cols)(_)
      buildFrom(reified)
    }
  }

}
