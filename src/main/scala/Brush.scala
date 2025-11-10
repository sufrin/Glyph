
package org.sufrin.glyph

/**
 * A `Brush` delivers `Paint`. The `Brush` API is (for the most part)
 * straightforwardly declarative, and the algebra of brushes supports
 * the derivation of brushes from other brushes, path effects, etc.
 */

import GlyphTypes.{Paint, PathEffect, Scalar}

import io.github.humbleui.skija.BlendMode
import org.sufrin.glyph.Brush.BUTT
import org.sufrin.glyph.Colour.ARGB
import io.github.humbleui.skija.PaintMode.{FILL, STROKE, STROKE_AND_FILL}

object Brush {

  import io.github.humbleui.skija.PaintStrokeCap

  /**
   *  Make a brush from the given specification; if necessary catching and logging `Brushes.NoBrush` exceptions
   *  then yielding a copy of the `Brushes.fallbackBrush`.
   *
   *  The `tag` has no formal semantics; it's there to support debugging.
   *
   * @see Brushes.apply
   * @see Brushes.Parse
   * @see Brushes.fallbackBrush
   */
  def apply(specification: String = "", tag: String = ""): Brush = Brushes(specification)

  /**
   *  Make a brush from the given specification; throwing  `Brushes.NoBrush` exceptions
   *  when the specification is badly-formed.
   *  The `tag` has no formal semantics; it's there to support debugging.
   *
   * @see Brushes.apply
   * @see Brushes.Parse
   */
  def Parse(specification: String = "", tag: String = ""): Brush = Brushes.Parse(specification)

  val ROUND: PaintStrokeCap = PaintStrokeCap.ROUND
  val SQUARE: PaintStrokeCap = PaintStrokeCap.SQUARE
  val BUTT: PaintStrokeCap = PaintStrokeCap.BUTT

  trait Effect {
    thisEffect =>
    def effect: PathEffect

    def compose(inner: Effect): Effect = {
      val outer = effect
      new Effect {
        def effect: PathEffect = outer.makeCompose(inner.effect)

        override def toString: String = s"$thisEffect$inner"
      }
    }
  }

  case class sliced(sliceLength: Scalar, displacementLimit: Scalar, seed: Int = 42) extends Effect {
    def effect = GlyphTypes.PathEffect.makeDiscrete(sliceLength, displacementLimit, seed)
    override def toString: String = f".sliced($sliceLength%.2f, $displacementLimit%.2f${if (seed == 42) "" else s", $seed"})"
  }

  case class dashed(onOff: Seq[Scalar]) extends Effect {
    def effect: PathEffect = GlyphTypes.PathEffect.makeDash(onOff)
    override def toString: String = f".dashed(${onOff.mkString(", ")})"
  }

  case class rounded(radius: Scalar) extends Effect {
    def effect: PathEffect = GlyphTypes.PathEffect.makeRoundedCorners(radius)
    override def toString: String = f".rounded($radius%.2f)"
  }

  case object noEffect extends Effect {
    def effect: PathEffect = null
    override def toString: String = ""
    override def compose(inner: Effect): Effect =  inner
  }

  /**
   * Transform a `Paint` into the equivalent `Brush` -- not
   * needed except for acquired or inherited `Paint`; and never
   * used in the `Glyph` toolkit.
   * @param p
   * @return
   */
  def ofPaint(p: Paint): Brush = {
    p match {
      case b: Brush => b
      case _ =>
        new Brush("ofPaint")
          .color(p.getColor)
          .strokeWidth(p.getStrokeWidth)
          .strokeCap(p.getStrokeCap)
          .antiAlias(p.isAntiAlias)
          .dither(p.isDither)
          .mode(p.getMode)
          .alpha(p.getAlphaf)
          .strokeMiter(p.getStrokeMiter)
          .pathEffect(p.getPathEffect)
          .shader(p.getShader)
    }
  }
}


/**
 * `Brush`es are the principal means by which pigments (paints, colours) are applied to `Surface`s
 * when a `Glyph` is rendered on it. This class is a subclass of `io.github.humbleui.skija.Paint`, but
 * provides an intelligible and efficient API for building, modifying, and deriving paint-potent objects.
 *
 * The standard notation for constructing a `Brush` with features `(f1=v1, f2=v2, ...)` is:
 * {{{
 *   Brush()(f1=v1, f2=v2, ...)
 * }}}
 *
 * Features not mentioned in the parameter list take default values -- usually "neutral" or "ignorable", as described in the `Paint`.
 * Example: a `ROUND` red brush of width 30.
 * {{{
 *   val rre = Brush()(color=0XFFFF0000, cap=Brush.ROUND, width=30f)
 * }}}
 *
 * A `Brush` is mutable: its individual attributes can be changed dynamically by invoking one of the
 * inline methods marked "Mutation" below. Each of these return the brush itself; so they can be
 * "chained". For example, the following expression yields `rre` itself after the specified changes
 * have been applied.
 * {{{
 *   rre.width(50).color(0XFF00FF00).cap(Brush.BUTT).antiAlias(true)
 * }}}
 *
 * As far as possible we have named brush attributes ("getters"), brush mutation methods ("setters"), and brush construction and
 * copying parameters systematically. They won't necessarily have the same names as their counterparts within `Paint`.
 *
 * @param tag basis for a way of identifying the role of the brush -- intended for use when debugging systematically
 *
 * We strongly advise against using "pure" `Paint` or its methods in a `Glyph` application. Nothing will
 * run faster, and debugging will be considerably harder.
 */
class Brush(val specification: String, var tag: String="") extends Paint {

  import GlyphTypes.ImageFilter

  import io.github.humbleui.skija.{PaintMode, PaintStrokeCap, PathEffect, Shader}

  var filterId: String=""
  var currentEffect: Brush.Effect = Brush.noEffect

  override def toString: String = {
    import Brush.{ROUND, SQUARE}
           val id = Brushes.colourName(this.color)
           val width = s"${strokeWidth.toInt}"
           val cap = strokeCap match {
             case ROUND => ".round"
             case SQUARE => ".square"
             case BUTT => ".butt"
             case _ => ""
           }
           val mode = this.mode match {
             case FILL => ".fill"
             case STROKE => ".stroke"
             case STROKE_AND_FILL => ".stroke&fill"
             case _ => ""
           }
    val anti = if (this.antiAliased) "" else ".antialias(false)"
    val dither = if (this.dithered) ".dither" else ""
    val alpha = if (this.alpha==1) "" else f".alpha(${getAlphaf}%.1f)"
    val tagged = if (tag.isEmpty) "" else s".tag($tag)"
   s"$id.$width$cap$mode$anti$dither$alpha$currentEffect$filterId$tagged"
  }

  /** A copy of `this`` brush with changed attributes as specified. */
  def apply(specification: String = this.specification,
            color: Int          = this.color,
            width: Float        = this.strokeWidth,
            cap: PaintStrokeCap = this.strokeCap,
            antiAlias: Boolean  = this.antiAliased,
            dither: Boolean     = this.dithered,
            mode: PaintMode     = this.mode,
            alpha: Float        = this.alpha,
            miter: Float        = this.strokeMiter,
            pathEffect: PathEffect = this.pathEffect,
            filter: ImageFilter    = this.getImageFilter,
            shader: Shader         = this.shader,
            blendMode: BlendMode   = this.getBlendMode,
            currentEffect: Brush.Effect  = this.currentEffect,
            filterId: String       = this.filterId,   // documentation for the applied filter
            tag: String            = this.tag
           ): Brush =
    Brush()
      . col(color)
      . strokeWidth(width)
      . strokeCap(cap)
      . antiAlias(antiAlias)
      . dither(dither)
      . mode(mode)
      . alpha(alpha)
      . strokeMiter(miter)
      . pathEffect(pathEffect)
      . filter(filter)
      . shader(shader)
      . blendMode(blendMode)
      . currentEffect(currentEffect)
      . filterId(filterId)
      . tagged(tag)

  /** An exact copy of this brush */
  def copy: Brush =
    new Brush(specification)
      .color(color)
      .strokeWidth(strokeWidth)
      .strokeCap(strokeCap)
      .antiAlias(antiAliased)
      .dither(dithered)
      .mode(mode)
      .alpha(alpha)
      .strokeMiter(strokeMiter)
      .pathEffect(getPathEffect)
      .filter(getImageFilter)
      .shader(getShader)
      .blendMode(getBlendMode)
      .currentEffect(currentEffect)
      .filterId(filterId)
      .tagged(tag)

  /** Copy the properties of `that: Brush` to `this: Brush` */
  def copyFrom(that: Brush): Brush = {
      color(that.color)
      strokeWidth(that.strokeWidth)
      strokeCap(that.strokeCap)
      antiAlias(that.antiAliased)
      dither(that.dithered)
      mode(that.mode)
      alpha(that.alpha)
      strokeMiter(that.strokeMiter)
      pathEffect(that.getPathEffect)
      filter(that.getImageFilter)
      shader(that.getShader)
      blendMode(that.getBlendMode)
      pathEffect(that.pathEffect)
      currentEffect(that.currentEffect)
      filterId(that.filterId)
      tagged(that.tag)
      this
  }

  @inline def tagged(tag: String): Brush = {
    this.tag = tag; this
  }

  /** Mutation */
  @inline def color(i: Int): Brush = {
    super.setColor(i); this
  }

  @inline def blendMode(blendMode: BlendMode): Brush = {
    super.setBlendMode(blendMode); this
  }

  @inline def blendMode: BlendMode = getBlendMode


  /** Mutation */
  @inline def currentEffect(newEffect: Brush.Effect): Brush = {
    currentEffect = newEffect
    pathEffect(currentEffect.effect)
    this
  }

  def hue: Double = ARGB(color).hue
  def sat: Double = ARGB(color).sat
  def vib: Double = ARGB(color).vib

  def hue(h: Double): Brush = color(ARGB(color).hue(h).toInt)
  def sat(s: Double): Brush = color(ARGB(color).sat(s).toInt)
  def vib(v: Double): Brush = color(ARGB(color).vib(v).toInt)

  /** Mutation */
  @inline def filterId(newId: String): Brush = {
    filterId = newId
    this
  }

  @inline def color: Int = super.getColor

  /** Mutation */
  @inline def strokeWidth(i: Float): Brush = {
    super.setStrokeWidth(i); this
  }

  @inline def strokeWidth: Float = getStrokeWidth

  /** Mutation */
  @inline def strokeCap(cap: PaintStrokeCap): Brush = {
    super.setStrokeCap(cap);
    this
  }

  @inline def strokeCap: PaintStrokeCap = getStrokeCap
  @inline def cap: PaintStrokeCap = getStrokeCap

  /** Mutation */
  @inline def strokeMiter(miter: Float): Brush = {
    super.setStrokeMiter(miter);
    this
  }

  @inline def strokeMiter: Float = getStrokeMiter

  /** Mutation */
  @inline def antiAlias(on: Boolean): Brush = {
    super.setAntiAlias(on);
    this
  }

  @inline def antiAliased: Boolean = isAntiAlias

  /** Mutation */
  @inline def dither(on: Boolean): Brush = {
    super.setDither(on);
    this
  }

  @inline def dithered: Boolean = isDither

  /** Mutation */
  @inline def mode(mode: PaintMode): Brush = {
    super.setMode(mode);
    this
  }

  @inline def mode: PaintMode = getMode

  /** Mutation */
  @inline def alpha(alpha: Float): Brush = {
    super.setAlphaf(alpha);
    this
  }

  /** Mutation */
  @inline def alpha(alpha: Double): Brush = {
    super.setAlphaf(alpha.toFloat);
    this
  }

  @inline def alpha: Float = getAlphaf

  /** Mutation abbreviation */
  @inline def col(i: Int): Brush = {
    super.setColor(i);
    this
  }

  /** Mutation abbreviation */
  @inline def width(i: Int): Brush = {
    super.setStrokeWidth(i.toFloat)
    this
  }

  /** Mutation abbreviation */
  @inline def width(w: Float): Brush = {
    super.setStrokeWidth(w)
    this
  }

  /** Mutation abbreviation */
  @inline def cap(cap: PaintStrokeCap): Brush = {
    strokeCap(cap);
    this
  }

  @inline def pathEffect: PathEffect =  getPathEffect

  @inline def filter: ImageFilter = getImageFilter

  /** Mutation abbreviation */
  @inline def pathEffect(effect: PathEffect): Brush = {
    setPathEffect(effect)
    this
  }

  @inline def filter(filter: ImageFilter): Brush = {
    setImageFilter(filter)
    this
  }

  @inline def shader: Shader = getShader

  /** Mutation abbreviation */
  @inline def shader(shader: Shader): Brush = {
    setShader(shader)
    this
  }

  import GlyphTypes.Scalar

  /**
   * A new dropshadowed brush, with shadow based on this brush's colour.
   *
   * @param blur extent of the blur
   * @param dx horizontal displacement of the blur
   * @param dy vertical displacement of the blur
   * @return a new brush
   */
  def blurred(blur: Scalar, dx: Scalar=0f, dy: Scalar=0f): Brush = {
    val result = this.copy(filterId=s".blurred($blur,$dx,$dy)")
    result.filter(ImageFilter.makeDropShadow(dx, dy, blur, color)).col(color)
    result
  }

  /**
   * Paths drawn with the resulting brush are sliced into pieces and their endpoints are displaced at random.
   * @param sliceLength
   * @param displacementLimit
   * @param seed
   *
   * @see PathEffect.makeDiscrete
   */
  def sliced(sliceLength: Scalar, displacementLimit: Scalar, seed: Int=42): Brush = {
    val effect = Brush.sliced(sliceLength, displacementLimit, seed) // GlyphTypes.PathEffect.makeDiscrete(sliceLength, displacementLimit, seed)
    val result = this.copy().currentEffect(effect)
    result
  }

  /** A new brush painted with dashes. */
  def dashed(onOff: Scalar*): Brush = {
    val result: Brush = this.copy().currentEffect(Brush.dashed(onOff))
    result
  }

  /** A new brush that rounds sharp corners. */
  def rounded(radius: Scalar): Brush = {
    val result: Brush = this.copy().currentEffect(Brush.rounded(radius))
    result
  }

  /** Methods that compose the current effect with a new effect */
  object ComposeEffect {
    def sliced(sliceLength: Scalar, displacementLimit: Scalar, seed: Int=42): Unit = {
      currentEffect(currentEffect.compose(Brush.sliced(sliceLength, displacementLimit, seed)))
    }

    def dashed(onOff: Scalar*): Unit = {
      currentEffect(currentEffect.compose(Brush.dashed(onOff)))
    }

    def rounded(radius: Scalar): Unit = {
      currentEffect(currentEffect.compose(Brush.rounded(radius)))
    }

    def noEffect(): Unit = {
      currentEffect(Brush.noEffect)
    }

    def noFilter(): Unit = {
      filter(null)
      filterId=""
    }

    def blurred(blur: Scalar, dx: Scalar=0f, dy: Scalar=0f): Unit = {
      filter(ImageFilter.makeDropShadow(dx, dy, blur, color)).col(color)
      filterId=s".blurred($blur,$dx,$dy)"
    }
  }


}

