package org.sufrin.glyph
package glyphML

import styled.ToggleVariable
import unstyled.reactive.Reaction

import org.sufrin.glyph.styles.decoration.unDecorated
import org.sufrin.glyph.GlyphTypes.Scalar

/**
 * An experiment in providing idiomatic support for building style-dependent (mostly reactive) glyphs to add to
 * translation definitions.
 */
object idioms {

  def Label(text: String): StyleSheet => Glyph =
  {style: StyleSheet => styled.Label(text)(style)}

  val edgeColor = Brushes.transparent.copy()

  def TextButton(name: String): Reaction=>StyleSheet=>Glyph  =
    (reaction: Reaction) => {style: StyleSheet=>styled.TextButton(name)(reaction)(style).edged(edgeColor)}

  def TextToggle(whenTrue: String="❎", whenFalse: String="✅", hint: Hint=NoHint): ToggleVariable => StyleSheet => Glyph =
    (variable: ToggleVariable) =>
       { style: StyleSheet => styled.TextToggle(whenTrue, whenFalse, variable.value, hint)(variable)(style).edged(edgeColor)}

  def CaptionedCheckBox(caption: String, hintText: String="", hintDwell: Double=3.0, hintScale: Double=0.6f): ToggleVariable => StyleSheet => Glyph =
    (variable: ToggleVariable) =>
      { style: StyleSheet =>
          val smaller = style.copy(fontScale=hintScale.toFloat)
          val hint = if (hintText.isEmpty) NoHint else Hint(hintDwell, hintText, true)(smaller)
          styled.CaptionedCheckBox(caption, initially=variable.value, hint=hint)(variable)(style).edged(edgeColor)
  }

  def CheckBox(variable: ToggleVariable, hintText: String="", seconds: Double=3.0, fontScale: Double=0.6f): StyleSheet => Glyph =
      {style: StyleSheet =>
        val smaller = style.copy(fontScale=fontScale.toFloat)
        val hint = if (hintText.isEmpty) NoHint else Hint(seconds, hintText, true)(smaller)
        styled.CheckBox(initially=variable.value, hint=hint)(variable)(style).edged(edgeColor) }


  def Table(cols: Int=0, rows: Int=0, uniform: Boolean=false)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph =
        fromSequence.Table(cols, rows, uniform)(glyphs)

  def Col(align: Alignment=Center, uniformWidth: Boolean=true, skip:Scalar=0)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph =
        fromSequence.Col(align, uniformWidth, skip)(glyphs)

  def Row(align: VAlignment=Mid, uniformHeight: Boolean=true, skip:Scalar=0)(glyphs: (StyleSheet => Glyph)*): StyleSheet => Glyph =
        fromSequence.Row(align, uniformHeight, skip)(glyphs)

  /** Constructions for `Row, Col, Table` that take `Seq[Glyph]` arguments  */
  object fromSequence {

    @inline def makeUniformHeight(glyphs: Seq[Glyph]): Seq[Glyph] = {
      val height = Measure.maxHeight(glyphs)
      glyphs.map(_.enlargedTo(0, height))
    }

    @inline def makeUniformWidth(glyphs: Seq[Glyph]): Seq[Glyph] = {
      val width = Measure.maxWidth(glyphs)
      glyphs.map(_.enlargedTo(width, 0))
    }

    /** Reify (from style with unDecorated buttons) then button-decorate the given glyph-generators at a uniform height */
    def decorateUniformHeight(glyphs: Seq[StyleSheet=>Glyph])(style: StyleSheet): Seq[Glyph] = {
      val noDecor: StyleSheet = style.copy(buttonDecoration = unDecorated)
      val reified = glyphs.map(_.apply(noDecor))
      val uniform = makeUniformHeight(reified)
      uniform.map(style.buttonDecoration.decorate)
    }

    /** Reify  (from style with unDecorated buttons) then button-decorate the given glyph-generators at a uniform width */
    def decorateUniformWidth(glyphs: Seq[StyleSheet=>Glyph])(style: StyleSheet): Seq[Glyph] = {
      val noDecor: StyleSheet = style.copy(buttonDecoration = unDecorated)
      val reified = glyphs.map(_.apply(noDecor))
      val uniform = makeUniformWidth(reified)
      uniform.map(style.buttonDecoration.decorate)
    }

    def Row(align: VAlignment=Mid, uniformHeight: Boolean=true, skip:Scalar=0)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = {
      style: StyleSheet =>
        val reified = if (uniformHeight) decorateUniformHeight(glyphs)(style) else glyphs.map(_.apply(style))
        NaturalSize.Row(align = align, skip=skip)(reified)
    }

    def Col(align: Alignment=Center, uniformWidth: Boolean=true, skip:Scalar=0)(glyphs: Seq[StyleSheet => Glyph]): StyleSheet => Glyph = {
      style: StyleSheet =>
        val reified = if (uniformWidth) decorateUniformWidth(glyphs)(style) else glyphs.map(_.apply(style))
        NaturalSize.Col(align = align, skip=skip)(reified)
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
