package org.sufrin.glyph
import unstyled.reactive.Enterable
import unstyled.static
import unstyled.static.INVISIBLE

/**
 *
 * A manager that arranges for a hint glyph to be computed and displayed "near" a reactive
 * glyph whenever the mouse cursor enters the glyph.
 *
 * @param target a reactive (`Enterable`) glyph to which a popup hint is to be attached.
 *
 * @param hint a computation yielding the glyph that will be popped-up when the mouse enters the reactive glyph. This
 *             can be computed *whenever* the hint is about to be shown, and this makes dynamic hints
 *             (showing state) feasible.
 *
 * @param seconds the time for which the hint will remain popped-up if the mouse doesn't leave the glyph
 *
 * @param constant true if the hint computation always yields the same glyph
 */
class HintManager(val target: Enterable, val hint: ()=>Glyph, val seconds: Double, constant: Boolean = true) {
  private var _allow: () => Boolean = ()=>true

  def onlyWhen(allow: => Boolean): this.type = { _allow = { ()=>allow }; this }

  val id = s"HintManager${this.hashCode()}"

  /**
   * Get the overlay layer that corresponds to this hint manager.
   *
   * NB: The new layer is constructed at the point of first entry or exit as the target's
   * `onGlyphEvent` method is invoked. The target glyph will certainly have been rooted
   * before it is entered and its `guiRoot` will by then be meaningful. The glyph that the layer
   * will show is computed only after it is determined that the layer is visible, and this makes
   * it feasible to generate hints dynamically.
   */
  def getLayer(guiRoot: RootGlyph): RootLayer =
    guiRoot.Overlay.annotations.getOrElse(id, guiRoot.Overlay.newAnnotation(id, glyph=INVISIBLE(), isModal = false, visible = false, strictHiding = false, active = false))

  lazy val schedule = new Schedule()

  val hintCache: Option[Glyph] = if (constant) Some(hint()) else None

  locally {
    target.onGlyphEvent {
      case (rootGlyph, true, where) =>
        val layer = getLayer(target.guiRoot)
        if (_allow() && !layer.visible && seconds>=0) {
          layer.visible = true
          layer.glyph = hintCache.getOrElse(hint())
          layer.glyph @@ where
          schedule.once((seconds * 1000L).toLong) {
            layer.visible = false
            target.reDraw()
          }
        }
      case (rootGlyph, false, _) =>
        val layer = getLayer(target.guiRoot)
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
 * `allow` is evaluated whenever `h`'s hint is about to be shown and (if false) the hint is not shown. This
 * allows the designer to provide ways of suppressing hints, and of avoiding "nagging".
 */
object HintManager {
  def ofGlyph(target: Enterable, seconds: Double, hint: ()=>Glyph, constant: Boolean = true): HintManager =
      new HintManager(target, hint, seconds, constant)

  def apply(target: Enterable, seconds: Double, hint: ()=>String, constant: Boolean = true)(implicit style: StyleSheet): HintManager = {
      new HintManager(
      target,
        ()=>styled.Label(hint())(style.copy(labelForegroundBrush = Brushes.red, labelBackgroundBrush = Brushes.white)).enlarged(10).framed(),
      seconds,
      constant)
  }
}

/**
 * A hint to be applied to a reactive glyph as it is specified
 */
trait Hint {
  def apply(reactive: Enterable): Unit
}

object NoHint extends Hint {
  def apply(reactive: Enterable): Unit = {}
}

object Hint {
  /** No hint is to be managed for `reactive` */
  def apply(): Hint = NoHint

  /**
   * The `hint` is to be managed for `reactive`; generated once only if `constant`, otherwise whenever the hint
   * becomes visible.
   * */
  def apply(seconds: Double, hint: =>String, constant: Boolean=true)(implicit style: StyleSheet): Hint = new Hint {
    def apply(reactive: Enterable): Unit =
      HintManager(reactive, seconds, ()=>hint, constant)
  }

  /**
   * The `hint` is to be managed for `reactive`; generated once only if `constant`, otherwise whenever the hint
   * becomes visible.
   * */
  def ofGlyph(seconds: Double, hint: =>Glyph, constant: Boolean=true)(implicit style: StyleSheet): Hint = new Hint {
    def apply(reactive: Enterable): Unit =
      HintManager.ofGlyph(reactive, seconds, ()=>hint, constant)
  }


}



