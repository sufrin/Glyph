package org.sufrin.glyph

trait StyleSheet { origin =>
  import GlyphTypes._
  import Styles._

  import Decoration._
  /** Default: can be overridden */
  def face: Typeface

  /** Default: can be overridden */
  def buttonFontSize: Scalar

  /** Default: can be overridden */
  def buttonFont: Font

  /** Default: can be overridden */
  def buttonBorderWidth: Scalar

  /** Default: can be overridden */
  def buttonBorderColor: Int
  def buttonBackgroundColor: Int

  /** Default: can be overridden */
  def buttonBorderBrush: Brush
  def buttonBackgroundBrush: Brush
  def buttonStyle: ButtonStyle
  def unFramed: StyleSheet
  def menuStyle: MenuStyle
  def labelStyle: GlyphStyle
  def notebookStyle: NotebookStyle
  val Spaces: Spaces

  /**
   * A `StyleSheet`` derived fro the current sheet by delegation. Itended to
   * support incremental stylesheet definitions.
   */
  class Derived extends StyleSheet {
    val delegate: StyleSheet = origin
    def face: Typeface = delegate.face
    def buttonFontSize: Scalar = delegate.buttonFontSize
    def buttonFont: Font = delegate.buttonFont
    def buttonBorderWidth: Scalar = delegate.buttonBorderWidth
    def buttonBorderColor: Pixel = delegate.buttonBorderColor
    def buttonBackgroundColor: Pixel = delegate.buttonBackgroundColor
    def buttonBorderBrush: Brush = delegate.buttonBorderBrush
    def buttonBackgroundBrush: Brush = delegate.buttonBackgroundBrush
    def buttonStyle: ButtonStyle = delegate.buttonStyle
    def unFramed: StyleSheet = delegate.unFramed
    def menuStyle: MenuStyle = delegate.menuStyle
    def labelStyle: GlyphStyle = delegate.labelStyle
    def notebookStyle: NotebookStyle = delegate.notebookStyle
    lazy val  Spaces: Spaces = delegate.Spaces
  }
}
