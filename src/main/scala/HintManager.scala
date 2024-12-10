package org.sufrin.glyph
import ReactiveGlyphs.Enterable

/**
 *
 * @param target a reactive glyph for which a popup hint is to be attached.
 * @param hint the glyph that will be popped-up when the mouse enters the reactive glyph
 * @param seconds the time for which the hint will remain popped-up
 */
class HintManager(val target: Enterable, val hint: Glyph, val seconds: Double) {
  private var _allow: () => Boolean = ()=>true

  def onlyWhen(allow: => Boolean): this.type = { _allow = { ()=>allow }; this }

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
        if (_allow() && !layer.visible && seconds>=0) {
          layer.visible = true
          schedule.once((seconds * 1000L).toLong) {
            layer.visible = false
            target.reDraw()
          }
        }
      case (false, _) =>
        if (seconds==0)
          layer.visible = false
        else
          schedule.immediately()
    }
  }

  def remove(): Unit = {
    target.guiRoot.Overlay.annotations.remove(id)
  }
}

/**
 * Add a hint manager to the `target` (reactive glyph), that enables a that a glyph (usually some text) to be shown as an overlay near the cursor whenever
 * the cursor enters the target. The hint will be removed `seconds` later.
 *
 * If `h: HintManager` then `h.onlyWhen(allow: => Boolean)` is the same hint manager, except that the expression
 * `allow` is evaluated whenever `h`'s hint is about to be shown and (if false) the hinto is not shown. This
 * allows the designer to provide ways of suppressing hints, and of avoiding "nagging".
 */
object HintManager {
  def apply(target: Enterable, seconds: Double, hint: Glyph): HintManager = new HintManager(target, hint, seconds)
  def apply(target: Enterable, seconds: Double, hint: String)(implicit style: StyleSheet): HintManager = {
    new HintManager(
           target,
           Glyphs.Label(hint, style.labelStyle.font, fg=DefaultBrushes.red, bg=DefaultBrushes.white).enlarged(10).framed(),
           seconds)
  }
}

