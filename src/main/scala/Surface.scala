package org.sufrin.glyph

import io.github.humbleui.skija.{Canvas, Paint, shaper => Shaper}
import io.github.humbleui.types.Rect

/**
 *  Glyphs are drawn on drawing surfaces.
 */
trait Surface {
  import GlyphTypes.{Degrees, Scalar, Scale}
  import Surface.{arrayOfPairs, arrayOfVectors}

  import io.github.humbleui.skija.{Image, Matrix33, Path, TextLine}

  val canvas: Canvas
  val scale: Scalar

  def drawPoint(loc: Vec, paint: Paint): Unit =
    canvas.drawPoint(loc.x, loc.y, paint)

  def drawLines(paint: Paint, locs: Seq[Vec]): Unit = {
    canvas.drawLines(arrayOfVectors(locs), paint)
  }

  def drawLines$(paint: Paint, locs: Scalar*): Unit = {
    canvas.drawLines(locs.toArray, paint)
  }

  def drawPoints(paint: Paint, locs: Seq[Vec]): Unit = {
    canvas.drawPoints(arrayOfVectors(locs), paint)
  }

  def drawPoints$(paint: Paint, locs: Scalar*): Unit = {
    canvas.drawPoints(locs.toArray, paint)
  }

  def drawPolygon(paint: Paint, locs: Seq[(Scalar, Scalar)]): Unit = {
    canvas.drawPolygon(arrayOfPairs(locs), paint)
  }

  /** Pre-arrayed pairs */
  def drawPolygon(paint: Paint, pairs: Array[Scalar]): Unit =
      canvas.drawPolygon(pairs, paint)

  def drawPolygon$(paint: Paint, locs: Scalar*): Unit =
      canvas.drawPolygon(locs.toArray, paint)

  def fillRect(paint: Paint, origin: Vec, diagonal: Vec): Unit =
      fillRect(paint, origin.x, origin.y, diagonal.x, diagonal.y)

  def fillRect(paint: Paint, x: Scalar, y: Scalar, w: Scalar, h: Scalar): Unit =
      canvas.drawRect(Rect.makeXYWH(x, y, w, h), paint)

  def fillRect(paint: Paint, diagonal: Vec): Unit =
      canvas.drawRect(Rect.makeXYWH(0f, 0f, diagonal.x, diagonal.y), paint)

  //def fillRRect(paint: Paint, diagonal: Vec): Unit =
  //  canvas.drawRRect(RRect.makeXYWH(0f, 0f, diagonal.x, diagonal.y), paint)

  def drawRect(paint: Paint, o: Vec, d: Vec): Unit =
      drawPolygon$(paint, o.x, o.y, o.x, o.y + d.y, o.x + d.x, o.y + d.y, o.x + d.x, o.y, o.x, o.y)

  def drawLine(paint: Paint, d: Vec): Unit =
      drawPolygon$(paint, 0f, 0f, d.x, d.y)

  def drawPath(paint: Paint, path: Path): Unit =
      canvas.drawPath(path, paint)

  def drawRect(paint: Paint, d: Vec): Unit =
      drawRect(paint, Vec.Origin, d)

  def drawShadow(color: Int, d: Vec, dx: Scalar, dy: Scalar, blur: Scalar, spread: Scalar, noClip: Boolean=false): Unit =
    if (noClip)
      canvas.drawRectShadowNoclip(Rect.makeXYWH(0f, 0f, d.x, d.y), dx, dy, blur, spread, color)
    else
      canvas.drawRectShadow(Rect.makeXYWH(0f, 0f, d.x, d.y), dx, dy, blur, spread, color)

  def drawOval(paint: Paint, o: Vec, d: Vec): Unit =
      canvas.drawOval(Rect.makeXYWH(o.x, o.y, d.x, d.y), paint)

  def drawTextLine(paint: Paint, text: TextLine, x: Scalar, y: Scalar): Unit =
      canvas.drawTextLine(text, x, y, paint)

  def drawTextLine(paint: Paint, text: TextLine, loc: Vec): Unit =
      canvas.drawTextLine(text, loc.x, loc.y, paint)

  def drawImage(image: Image): Unit = canvas.drawImage(image, 0f, 0f)

  /** Set the canvas to the given colour */
  def clear(colour: Int): Unit = canvas.clear(colour)

  /** Transform the coordinates  */
  def rotate(degrees: Float): Unit = canvas.rotate(degrees)

  def saveLayer(origin: Vec, diagonal: Vec, paint: Paint): Int = canvas.saveLayer(origin.x, origin.y, origin.x+diagonal.x, origin.y+diagonal.y, paint)
  def restoreLayer(savedLayer: Int): Unit = canvas.restoreToCount(savedLayer)
  def restore(): Unit = canvas.restore()

  def clipRect(origin: Vec, diagonal: Vec): Unit =
      canvas.clipRect(Rect.makeXYWH(origin.x, origin.y, diagonal.x, diagonal.y))

  /**
   * Run the given effect with the canvas translated to the offset
   */
  def withOrigin[T](offset: Vec)(effect: => T): T = withOrigin[T](offset.x, offset.y) { effect }

  /**
   * Run the given effect with the canvas rotated by degrees.
   * TODO: Optimise the no-rotation case
   */
  def withRot(degrees: Degrees)(effect: => Unit): Unit =  {
    val l = canvas.saveLayer(null, null)
    canvas.rotate(degrees)
    try effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }

  def withVReversed(effect: => Unit): Unit = {
    val l = canvas.saveLayer(null, null)
    canvas.rotate(180)
    try
      effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
    canvas.rotate(180)
  }

  def withHReversed(effect: => Unit): Unit = {
    val l = canvas.saveLayer(null, null)
    canvas.rotate(90)
    try
      effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
    canvas.rotate(270)
  }

  /**
   * Run the given effect with the canvas rotated by degrees, about the given point.
   */
  def withRot(degrees: Degrees, point: Vec)(effect: => Unit): Unit = {
    import Matrix33._
    val l = canvas.saveLayer(null, null)
    val current = canvas.getLocalToDeviceAsMatrix33
    canvas.setMatrix(current.makeConcat(makeRotate(degrees, point.x, point.y)))
    try effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }

  def withTransform(m: Array[Float])(effect: => Unit): Unit = {
    import Matrix33._
    val l = canvas.saveLayer(null, null)
    val current = canvas.getLocalToDeviceAsMatrix33
    canvas.setMatrix(current.makeConcat(new Matrix33(m(0), m(1),  m(2),  m(3), m(4), m(5),  m(6), m(7), m(8))))
    try
      effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }

  /**
   * Run the given effect with the canvas scaled by `hardwareScale` in both directions
   * TODO: Optimise the unit-scaling case
   */
  def withScale(scale: Scale)(effect: => Unit): Unit = {
    val l = canvas.saveLayer(null, null)
    canvas.scale(scale, scale)
    try effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }

  def withScale(scale: Vec)(effect: => Unit): Unit = {
    val l = canvas.saveLayer(null, null)
    canvas.scale(scale.x, scale.y)
    try effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }

  def withAlpha(diag: Vec, alpha: Int)(effect: => Unit): Unit = {
    val l = canvas.saveLayerAlpha(Rect.makeWH(diag.x, diag.y), alpha)
    try effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }

  def withAlpha(diag: Vec, alpha: Float)(effect: => Unit): Unit =
      withAlpha(diag, (0x77*alpha).toInt){ effect }

    /**
   * Run the given effect with the canvas translated to (x, y)
   * TODO: optimise the origin case
   */
  def withOrigin[T](x: Scalar, y: Scalar)(effect: => T): T = {
    val l = canvas.saveLayer(null, null)
    canvas.translate(x, y)
    try   effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
        null.asInstanceOf[T]
    } finally {
      canvas.restoreToCount(l)
    }
  }

  def withClip(diagonal: Vec)(effect: => Unit): Unit = {
    val l = canvas.saveLayer(null, null)
    canvas.clipRect(Rect.makeWH(diagonal.x, diagonal.y))
    try effect
    catch {
      // TODO: log the error in a logging stream
      case err: Throwable => err.printStackTrace()
    } finally {
      canvas.restoreToCount(l)
    }
  }


  private var _scope: Vec = Vec.Zero

  /**
   * Set the scope of reactive glyph gestures to be `scope`
   * during the computation of `effect`. When nonzero, `_scope` is the
   * diagonal of the (absolute) bounding box within which a
   * reactive glyph can still be considered to contain the cursor.
   *
   * TODO: It looks like `scope` should be set whenever a clip is
   *       set on a container -- because one doesn't want a glyph that has been partly clipped
   *       to continue to respond to mouse movements over its clipped part. But this
   *       may be avoidable in many cases.
   */
  def withScope(scope: Vec)(effect: => Unit): Unit = {
    val s = _scope
    val trans      = AffineTransform.from(canvas.getLocalToDeviceAsMatrix33)
    val thisOrigin = AffineTransform.transform(trans, 0f, 0f)
    _scope = scope + thisOrigin
    try effect
    catch   { case err: Throwable => err.printStackTrace() }
    finally { _scope = s }
  }

  /**
   * Tell the given reactive glyph what the most recent co-ordinate transformation
   * was. Currently intended to be used (only) by the draw methods of reactive glyphs
   * in the implementation `contains`; which is a key component of the focus machinery.
   */
  def declareCurrentTransform(glyph: ReactiveGlyph) = {
      //println(s"$hardwareScale => $scaleT")
      val transform = canvas.getLocalToDeviceAsMatrix33
      glyph.declareCurrentTransform(AffineTransform.from(transform), scale, _scope)
  }

  @inline def currentTransform: AffineTransform.Transform = AffineTransform.from(canvas.getLocalToDeviceAsMatrix33)

  def currentReverseTransform: Vec => Vec = AffineTransform.reverse(currentTransform)
  def currentForwardTransform: Vec => Vec = {
    val current = currentTransform
    return { case v: Vec => AffineTransform.transform(current, v.x, v.y) }
  }

}

object Surface {
  import GlyphTypes.Scalar
  def apply(skijaCanvas: io.github.humbleui.skija.Canvas, _scale: Scalar): Surface = new Surface {
    val canvas = skijaCanvas
    val scale = _scale
  }

  import GlyphTypes.Scalar

  /**
   * Flattens a sequence of 'Vec'tors
   */
  def arrayOfVectors(locs: Seq[Vec]): Array[Float] = {
    val floats = Array.ofDim[Float](2 * locs.length)
    var n = 0
    for {loc <- locs} {
      floats(n) = loc.x; floats(n + 1) = loc.y; n += 2
    }
    floats
  }

  /**
   * Flattens a sequence of `Scalar` pairs
   */
  def arrayOfPairs(locs: Seq[(Scalar, Scalar)]): Array[Float] = {
    val floats = Array.ofDim[Float](2 * locs.length)
    var n = 0
    for {(x, y) <- locs} {
      floats(n) = x; floats(n + 1) = y; n += 2
    }
    floats
  }

  /**
   * Flattens an iterable of `Scalar` pairs
   */
  def arrayOfPairs(locs: Iterable[(Scalar, Scalar)]): Array[Float] = {
    val floats = Array.ofDim[Float](2 * locs.size)
    var n = 0
    for {(x, y) <- locs.iterator } {
      floats(n) = x; floats(n + 1) = y; n += 2
    }
    floats
  }

}
