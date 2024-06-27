package org.sufrin.glyph
import ReactiveGlyphs.GenericButton

class HintManager(target: GenericButton, hint: Glyph) {
  val id = s"hint:${target.hashCode()}"
  // the new layer is constructed lazily (in fact, at the point of first entry) because
  // the target glyph will certainly have been rooted before it is entered,
  // so target.guiRoot will by then be meaningful.
  lazy val layer =
    target.guiRoot.Overlay.newAnnotation(id, glyph=hint, isModal = false, visible = false, strictHiding = false, active = false)
  locally {
    target.onGlyphEvent {
      case true =>
        hint @@ (target.rootDistance)
        layer.visible = true
      case false =>
        layer.visible = false
    }
  }
}

object HintManager {
  def apply(target: GenericButton, hint: Glyph): Unit  = new HintManager(target, hint)
  def apply(target: GenericButton, hint: String)(implicit style: StyleSheet): Unit = {
    new HintManager(target, Glyphs.Label(hint, style.labelStyle.font, fg=DefaultBrushes.red, bg=DefaultBrushes.white).enlarged(10).framed())
  }
}
