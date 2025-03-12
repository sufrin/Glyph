package org.sufrin.glyph
package styled


/**
 *   A glyph, whose appearance is denoted by the `element`
 *   delivered by `element`. It rebuilds its appearance at a given size on request.
 *   Its initial appearance is determined by `context`; and if the context hasn't
 *   set a positive `boundingBox`, this will be the "natural" (bottom-up)
 *   appearance determined by the rest of the context. Subsequent invocations of
 *   `atSize(box: Vec)` regenerate the appearance using `context.copy(boundingBox=box)`
 *
 *   It is intended for use as the top-level `Glyph` of a GUI whose
 *   window may be resized, and whose layout may need to be adapted to
 *   the current size.
 *
 */

import org.sufrin.logging.Loggable

import scala.xml.Node
import glyphXML.Language._

abstract class Resizeable(val splash: Glyph, val style: StyleSheet) extends Glyph with GlyphTransforms {
  override def resizeable: Boolean = true

  def element:  Node
  var delegate: Glyph = { splash.parent=this; splash }

  override def atSize(boundingBox: Vec): Glyph = {
    Resizeable.finest(s"atSize($boundingBox) with scale=${guiRoot.softwareScale}")
    delegate = XMLtoGlyph(element)(style.copy(windowDiagonal = boundingBox))// element.toGlyph(context.copy(boundingBox = boundingBox))
    Resizeable.finest(s"delegate.diagonal=${delegate.diagonal} => $diagonal")
    delegate.parent = this
    delegate
  }

  def draw(surface: Surface): Unit = delegate.draw(surface)
  def diagonal: Vec = delegate.diagonal
  def copy(fg: Brush=delegate.fg, bg: Brush=delegate.bg): Glyph = delegate.copy(fg, bg)
  val fg: Brush = delegate.fg
  val bg: Brush = delegate.bg
  override def glyphContaining(p: Vec): Option[Hit] = delegate.glyphContaining(p)
  override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = delegate.reactiveContaining(p)

}

/**
 *   A glyph, whose appearance is denoted by the GlyphML element
 *   denoted by `theElement`. It rebuilds its appearance at a given size on request.
 *   Its initial appearance is determined by `context`; and if the context hasn't
 *   set a positive `boundingBox`, this will be the "natural" (bottom-up)
 *   appearance determined by the rest of the context. Subsequent invocations of
 *   `atSize(box: Vec)` regenerate the appearance using `context.copy(boundingBox=box)`
 *
 *   It is intended for use as the top-level `Glyph` of a GUI whose
 *   window may be resized, and whose layout may need to be adapted to
 *   the current size.
 *
 */
object Resizeable extends Loggable {
  def apply(splash: Glyph, xml: => Node)(implicit style: StyleSheet): Resizeable = new Resizeable(splash, style) {
    def element: Node = xml
  }
}