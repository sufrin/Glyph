package org.sufrin.glyph
import ReactiveGlyphs.GenericButton

class HintManager(val target: GenericButton, val hint: Glyph, val seconds: Double) {
  val id = s"HintManager${this.hashCode()}"
  /** The new layer is constructed lazily (in fact, at the point of first entry) because
   * the target glyph will certainly have been rooted before it is entered,
   * so `target.guiRoot` will by then be meaningful.
   */
  lazy val layer =
    target.guiRoot.Overlay.newAnnotation(id, glyph=hint, isModal = false, visible = false, strictHiding = false, active = false)

  lazy val schedule = new Schedule()

  locally {
    target.onGlyphEvent {
      case (true, where) =>
        hint @@ where
        if (!layer.visible && seconds>=0) {
          layer.visible = true
          schedule((seconds * 1000L).toLong) {
            layer.visible = false
            target.reDraw()
          }
        }
      case (false, _) =>
        if (seconds==0)
          layer.visible = false
        else
          schedule.now()
    }
  }

  def remove(): Unit = {
    target.guiRoot.Overlay.annotations.remove(id)
  }
}

object HintManager {
  def apply(target: GenericButton, seconds: Double=0, hint: Glyph): HintManager = new HintManager(target, hint, seconds)
  def apply(target: GenericButton, seconds: Double, hint: String)(implicit style: StyleSheet): HintManager = {
    new HintManager(
           target,
           Glyphs.Label(hint, style.labelStyle.font, fg=DefaultBrushes.red, bg=DefaultBrushes.white).enlarged(10).framed(),
           seconds)
  }
}

