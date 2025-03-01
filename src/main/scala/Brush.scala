package org.sufrin.glyph

/**
 * A `Brush` delivers `Paint` with a more convenient API
 */

import GlyphTypes.{Paint, PathEffect}

import io.github.humbleui.skija.BlendMode

object Brush {

  import io.github.humbleui.skija.PaintStrokeCap

  def of(p: Paint): Brush = {
    p match {
      case b: Brush => b
      case _        =>
        new Brush (f"of(0X${p.getColor}%08X)")
        .color (p.getColor)
        .strokeWidth (p.getStrokeWidth)
        .strokeCap (p.getStrokeCap)
        .antiAlias (p.isAntiAlias)
        .dither (p.isDither)
        .mode (p.getMode)
        .alpha (p.getAlphaf)
        .strokeMiter (p.getStrokeMiter)
        .pathEffect (p.getPathEffect)
        .shader (p.getShader)
    }
  }

  def apply(name: String=""): Brush = new Brush(name)

  def ofString(descriptor: String): Brush = DefaultBrushes(descriptor)

  val ROUND:  PaintStrokeCap  = PaintStrokeCap.ROUND
  val SQUARE: PaintStrokeCap  = PaintStrokeCap.SQUARE
  val BUTT:   PaintStrokeCap  = PaintStrokeCap.BUTT

  val includeDetail: Boolean = true
}

/**
 * `Brush`es are the principal means by which pigments (paints, colours) are applied to `Surface`s
 * when a `Glyph` is rendered on it. This class is a subclass of `io.github.humbleui.skija.Paint`, but
 * provides an intelligible and efficient API for building, modifying, and deriving paint-potent objects.
 *
 * The standard notation for constructing a named `Brush` with features `(name="...", f1=v1, f2=v2, ...)` is:
 * {{{
 *   Brush("name")(f1=v1, f2=v2, ...)
 * }}}
 *
 * Features not mentioned in the parameter list take default values -- usually "neutral" or "ignorable", as described in the `Paint`.
 * Example: a `ROUND` red brush of width 30.
 * {{{
 *   val rre = Brush("roundredexample")(color=0XFFFF0000, cap=Brush.ROUND, width=30f)
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
 * @param name the name of the brush: this can be changed, but it's not advisable to do so except
 *             to contribute to a systematic debugging discipline.
 *
 * We strongly advise against using "pure" `Paint` or its methods in a `Glyph` application. Nothing will
 * run faster, and debugging will be considerably harder.
 */
class Brush(var name: String) extends Paint {

  import io.github.humbleui.skija.{PaintMode, PaintStrokeCap, PathEffect, Shader}
  import GlyphTypes.ImageFilter

  override def toString: String = {
    import Brush.{ROUND, SQUARE}
           val id = if (name.isEmpty) f"Brush() color=0X${color}%08X" else name
           val width = if (strokeWidth<2f) "" else s" width ${strokeWidth.toInt}"
           val cap = strokeCap match {
             case ROUND => " cap ROUND"
             case SQUARE => " cap SQUARE"
             case _ => ""
           }
   if (Brush.includeDetail) s"$id$width$cap" else id
  }

  /** A copy of `this`` brush with changed attributes as specified. */
  def apply(name: String        = this.name,
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
            blendMode: BlendMode   = this.getBlendMode
           ): Brush =
    Brush(name)
      . col(color)
      . strokeWidth(width)
      . strokeCap(cap)
      . antiAlias(antiAlias)
      . dither(dither)
      . mode(mode)
      . alpha(alpha)
      . strokeMiter(miter)
      . pathEffect(pathEffect)
      . shader(shader)
      . blendMode(blendMode)

  /** A copy of this brush with the same name. */
  def copy: Brush =
    Brush(name)
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

  /** Mutation */
  @inline def color(i: Int): Brush = {
    super.setColor(i); this
  }

  @inline def blendMode(blendMode: BlendMode): Brush = {
    super.setBlendMode(blendMode); this
  }

  @inline def blendMode: BlendMode = getBlendMode

  /** Mutation */
  @inline def name(newName: String): Brush = {
    name = newName
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
    val result = Brush(f"$this.blurred($blur%2.2f, $dx%4.2f, $dy%4.2f)")
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
    val effect = GlyphTypes.PathEffect.makeDiscrete(sliceLength, displacementLimit, seed)
    val result = this.copy(name=s"$this.chopped($sliceLength,$displacementLimit, $seed)", pathEffect=effect)
    result
  }

  /** A new brush painted with dashes. */
  def dashed(onOff: Scalar*): Brush = {
    val result: Brush = this.copy(name=s"$this.dashed(${onOff.mkString(", ")})", pathEffect=GlyphTypes.PathEffect.makeDash(onOff))
    result
  }

  /** A new brush that rounds sharp corners. */
  def rounded(radius: Scalar): Brush = {
    val result: Brush = copy(name=s"$this.rounded(${radius})", pathEffect=GlyphTypes.PathEffect.makeRoundedCorners(radius))
    result
  }


}

