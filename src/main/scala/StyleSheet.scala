package org.sufrin.glyph

trait StyleSheet { origin =>
  import DefaultBrushes._
  import GlyphTypes._
  import Styles._

  import Decoration._
  /** Default: can be overridden */
  def face: Typeface

  /** Default: can be overridden */
  def buttonFontSize: Scalar

  /** Default: can be overridden */
  val buttonFont: Font

  /** Default: can be overridden */
  val buttonBorderWidth: Scalar

  /** Default: can be overridden */
  val buttonBorderColor: Int
  val buttonBackgroundColor: Int

  /** Default: can be overridden */
  val buttonBorderBrush: Brush
  val buttonBackgroundBrush: Brush
  val buttonStyle: ButtonStyle
  val unFramed: StyleSheet
  val menuStyle: MenuStyle
  val labelStyle: GlyphStyle
  val Spaces: Spaces

  /**
   * A `StyleSheet`` derived fro the current sheet by delegation. Itended to
   * support incremental stylesheet definitions.
   */
  class Derived extends StyleSheet {
    val delegate: StyleSheet = origin
    lazy val face: Typeface = delegate.face
    lazy val buttonFontSize: Scalar = delegate.buttonFontSize
    lazy val buttonFont: Font = delegate.buttonFont
    lazy val buttonBorderWidth: Scalar = delegate.buttonBorderWidth
    lazy val buttonBorderColor: Pixel = delegate.buttonBorderColor
    lazy val buttonBackgroundColor: Pixel = delegate.buttonBackgroundColor
    lazy val buttonBorderBrush: Brush = delegate.buttonBorderBrush
    lazy val buttonBackgroundBrush: Brush = delegate.buttonBackgroundBrush
    lazy val buttonStyle: ButtonStyle = delegate.buttonStyle
    lazy val unFramed: StyleSheet = delegate.unFramed
    lazy val menuStyle: MenuStyle = delegate.menuStyle
    lazy val labelStyle: GlyphStyle = delegate.labelStyle
    lazy val Spaces: Spaces = delegate.Spaces
  }
}
