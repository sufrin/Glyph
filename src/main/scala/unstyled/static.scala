package org.sufrin.glyph
package unstyled
package static

import GlyphTypes.Scalar

/** A collection of static `Glyph` constructors */



  class Image(glyph: Glyph) extends Glyph {
    val theImage = External.toImage(glyph)
    override def toString: String = s"Image($diagonal)"
    override val fg: Brush = glyph.fg
    override val bg: Brush = glyph.bg

    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = {
      surface.drawImage(theImage)
    }

    /**
     * The diagonal size of the glyph
     */
    def diagonal: Vec = glyph.diagonal

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = new Image(glyph(fg, bg))

  }

  /** Shaded glyphs suitable for use in making buttons */
  object Shaded  {



    case class ShadingPaths(topLeft: GlyphTypes.Path, bottomRight: GlyphTypes.Path)

    def shadingPaths(w: Scalar, h: Scalar, delta: Scalar): ShadingPaths = {
      val bottomRight = new GlyphTypes.Path()
      locally {
        bottomRight.moveTo(0f, h)
        bottomRight.lineTo(w, h)
        bottomRight.lineTo(w, 0f)
        bottomRight.lineTo(w + delta, delta)
        bottomRight.lineTo(w + delta, h + delta)
        bottomRight.lineTo(delta, h + delta)
        //
        val left = io.github.humbleui.types.Rect.makeWH(1f, h)
        val top = io.github.humbleui.types.Rect.makeWH(w, 1f)
        bottomRight.moveTo(0f, 0f)
        bottomRight.addRect(left)
        bottomRight.moveTo(0f, 0f)
        bottomRight.addRect(top)
      }

      val topLeft = new GlyphTypes.Path()
      locally {
        topLeft.lineTo(w, 0f)
        topLeft.lineTo(w + delta, delta)
        topLeft.lineTo(delta, delta)
        topLeft.lineTo(delta, h + delta)
        topLeft.lineTo(0f, h)
        topLeft.lineTo(0f, 0f)
      }

      ShadingPaths(topLeft, bottomRight)
    }

    /**
     * A button-like glyph, whose diagonal is `glyph.diagonal+(delta, delta)`.
     *
     * Shown (when `down.value` is true)  as glyph, with shading of width zdelta` around the bottom-left corner of glyph.
     * Shown (when `down.value` is false) as glyph, in the bottom-left corner of the whole.
     */
    def Dynamic(fg: Brush=fallback.buttonForeground, bg: Brush=fallback.buttonBackground, delta: Scalar, down: Variable[Boolean] = Variable(false))(glyph: Glyph) =
      new Dynamic(glyph, fg, bg, delta, down)

    /**
     * A button-like glyph, whose diagonal is `glyph.diagonal+(delta, delta)`.
     *
     * Shown (when `down` is true)  as glyph, with shading of width zdelta` around the bottom-left corner of glyph.
     * Shown (when `down` is false) as glyph, in the bottom-left corner of the whole.
     */
    def Static(fg: Brush = fallback.buttonForeground, bg: Brush = fallback.buttonBackground, delta: Scalar, down: Boolean=false)(glyph: Glyph) =
        GlyphTransforms.Shaded(glyph, fg, bg, 0f, delta, down)

    class  Dynamic(glyph: Glyph, override val fg: Brush, override val bg: Brush, delta: Scalar, val down: Variable[Boolean]) extends Glyph {
      override def toString: String = s"Shaded.Dynamic($fg, $bg, delta=$delta, $down)\n  ($glyph)"
      val fgWidth = fg.getStrokeWidth
      val offset = delta
      val diagonal = glyph.diagonal + (offset, offset)
      val linePaint = Brush().setColor(0x99070707).setStrokeWidth(0f)

      val shading = shadingPaths(glyph.w, glyph.h, delta)

      def draw(surface: Surface): Unit = {
        if (down.value) {
          surface.drawPath(linePaint, shading.topLeft)
          surface.withOrigin(delta, delta) {
            surface.fillRect(bg, glyph.diagonal)
            glyph.draw(surface)
          }
        } else {
          surface.fillRect(bg, glyph.diagonal)
          glyph.draw(surface)
          surface.drawPath(linePaint, shading.bottomRight)
        }

        locally { glyph.parent = this }

      }

      def copy(fg: Brush, bg: Brush): Glyph = new Dynamic(glyph.copy(), fg, bg, delta, down)
    }
   }

  /**
   * A composite made of a collection of  glyphs. Its diagonal is the
   * bounding rectangle of all its glyphs placed at their locations. It is
   * drawn by drawing its glyphs in their order of appearance in argument sequence.
   *
   * Only the first glyph may be (or contain) a reactive.
   *
   * @see Concentric
   */
  object Envelope  {

    def apply(fg: Brush = Brushes.transparent, bg: Brush = Brushes.transparent)(located: Glyph*): Glyph = new Envelope(located, fg, bg)

    class Envelope(located: Seq[Glyph], override val fg: Brush, override val bg: Brush) extends Composite(located) {
      override val kind: String = "Envelope"
      assert(located.nonEmpty, "Envelope must contain at least one glyph")

      val glyphs = located

      // only the lead glyph may be reactive
      val glyph = glyphs.head
      override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p - glyph.location) // was + HOW DID THIS WORK AT ALL?
      override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p - glyph.location) // was + HOW DID THIS WORK AT ALL?
      // override def contains(p: Vec): Boolean = glyph.contains(p)

      val diagonal = {
        val height = glyphs.map(_.bottom).max
        val width = glyphs.map(_.right).max
        Vec(width, height)
      }

      locally {
        setParents()
        for { glyph <- glyphs} assert(glyph.parent eq this, "Parent left unset")
      }

      override def copy(fg: Brush = fg, bg: Brush = bg): Envelope =
        new Envelope(glyphs.map(_.copy()), fg, bg)

    }
  }

  /**
   *  All `Concentric.method(...)` lay out their argument glyphs in some sense concentrically.
   *
   *  `Center` aligns the centres of the glyphs.
   *
   *  `Top` aligns their north-south axes, and their top edges
   *
   *  `Bottom` aligns their north-south axes, and their bottom edges
   *
   *  `Left` aligns their west-east axes, and their west (left) edges
   *
   *  `Right` aligns their west-east axes, and their east (right) edges
   *
   *  The glyphs are drawn in their order in the argument sequence -- last on top
   *
   *  The search for reactive glyphs is conducted last-to-first. This is so that a reactive glyph,
   *  or a subtree containing a reactive glyph, may be
   *  drawn concentrically "on top" of a composite background,
   *  but still be found first.
   */
  trait ConcentricGenerators { theseGenerators =>

    val fg: Brush
    val bg: Brush
    val rowAlign: VAlignment
    val colAlign: Alignment

    def apply(theGlyphs: Seq[Glyph]): Composite = aligned(rowAlign.proportion, colAlign.proportion, theGlyphs, s"($rowAlign,$colAlign)")
    def apply(theGlyph: Glyph, theGlyphs: Glyph*): Composite = aligned(rowAlign.proportion, colAlign.proportion, theGlyph :: theGlyphs.toList, s"($rowAlign,$colAlign)")

    def Center(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(0.5f, 0.5f, glyph::theGlyphs.toList, "Center")
    def Center(theGlyphs: Seq[Glyph]): Composite = aligned(0.5f, 0.5f, theGlyphs, "Center")

    def Mid(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(0.5f, 0.5f, glyph::theGlyphs.toList, "Mid")
    def Mid(theGlyphs: Seq[Glyph]): Composite = aligned(0.5f, 0.5f, theGlyphs, "Mid")

    def Top(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(0.5f, 0f, glyph::theGlyphs.toList, "Top")
    def Top(theGlyphs: Seq[Glyph]): Composite = aligned(0.5f, 0f, theGlyphs, "Top")

    def Left(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(0f, 0.5f, glyph::theGlyphs.toList, "Left")
    def Left(theGlyphs: Seq[Glyph]): Composite = aligned(0f, 0.5f, theGlyphs, "Left")

    def Bottom(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(0.5f, 1f, glyph::theGlyphs.toList, "Bottom")
    def Bottom(theGlyphs: Seq[Glyph]): Composite = aligned(0.5f, 1f, theGlyphs, "Bottom")

    def Right(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(1f, 0.5f, glyph::theGlyphs.toList, "Right")
    def Right(theGlyphs: Seq[Glyph]): Composite = aligned(1f, 0.5f, theGlyphs, "Right")


    def apply(rowAlign: VAlignment=rowAlign, colAlign: Alignment=colAlign, fg: Brush = Brushes.transparent, bg: Brush = Brushes.transparent): ConcentricGenerators = {
      val (_fg, _bg, _r, _c) = (fg, bg, rowAlign, colAlign)
      new ConcentricGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val rowAlign: VAlignment = _r
        val colAlign: Alignment = _c
      }
    }


    def aligned(proportionw: Float, proportionh: Float, theGlyphs: Seq[Glyph], factory: String): Composite = {
      require(theGlyphs.nonEmpty)
      val height = theGlyphs.map(_.h).max
      val width = theGlyphs.map(_.w).max

      var x, y = 0f
      for {glyph <- theGlyphs} {
        val inset = Vec((width - glyph.w) * proportionw, (height - glyph.h) * proportionh)
        glyph @@ inset
        x += glyph.w
      }

      new Composite(theGlyphs) {
        override val kind: String = s"Concentric.$factory"
        val glyphs        = theGlyphs
        val reverseGlyphs = theGlyphs.reverse

        override def searchGlyphs: Seq[Glyph] = reverseGlyphs

        val diagonal = Vec(width, height)
        override
        val baseLine = 0f

        override val fg = theseGenerators.fg
        override val bg = theseGenerators.bg

        locally {
          setParents()
        }

        def copy(fg: Brush=fg, bg: Brush=bg): Composite = aligned(proportionw, proportionh, theGlyphs.map(_.copy()()), factory)
      }
    }
  }

  object Concentric extends ConcentricGenerators {
    val bg: Brush = Brushes.transparent
    val fg: Brush = Brushes.transparent
    val rowAlign: VAlignment=org.sufrin.glyph.Mid
    val colAlign: Alignment=org.sufrin.glyph.Center
  }

  /** An empty glyph with the given dimensions */
  class Skip(width: Scalar, height: Scalar) extends Glyph {
    val diagonal = Vec(width, height)
    override val fg = Brushes.invisible
    override val bg = Brushes.invisible
    override def draw(surface: Surface): Unit = ()

    override val toString: String = s"Skip($width, $height)"

    def copy(fg: Brush=fg, bg: Brush=bg): Skip = new Skip(width, height)
  }

  object Skip {
    /** A horizontal skip of size `d`. */
    def h(d: Scalar): Skip = new Skip(d, 0f)

    /** A vertical skip of size `d`. */
    def v(d: Scalar): Skip = new Skip(0f, d)

    def apply(width: Scalar, height: Scalar): Glyph = new Skip(width, height)

    /** Square skip */
    def apply(width: Scalar): Glyph = new Skip(width, width)
  }

  /** A zero-diagonal empty glyph; useful in defining anchors for dialogues. Colours can be inherited, too.  */
  class INVISIBLE(override val fg: Brush, override val bg: Brush, width: Scalar, height: Scalar) extends Glyph {
    override def toString: String = s"INVISIBLE(fg=$fg, bg=$bg, ($w, $h))"
    def draw(surface: Surface): Unit = {}
    def diagonal: Vec = Vec(width, height)
    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new INVISIBLE(fg, bg, w, h)
  }

  object INVISIBLE {
    def apply(fg: Brush = Brushes.invisible, bg: Brush = Brushes.invisible, w: Scalar=0, h: Scalar=0): Glyph = new INVISIBLE(fg, bg, w, h)
  }

  /**
   * A point, made with the given paint. If the paint is thick then it's more of a blob than a point.
   */
  class Point(override val fg: Brush) extends Glyph {
    val diagonal = Vec(paint.getStrokeWidth, paint.getStrokeWidth)
    override val bg = Brushes.transparent

    def draw(surface: Surface): Unit =
      surface.drawPoints$(paint, diagonal.x/2, diagonal.y/2)

    override val toString: String = s"Point($fg)"

    def copy(fg: Brush=fg, bg: Brush=bg): Point = new Point(fg)
  }

  object Point  {
    def apply(fg: Brush = Brushes.black): Point = new Point(fg)
  }


  /**
   * A rectangular frame drawn such that the paint falls (just) inside
   * the box with the given diagonal.
   *
   * @see FilledRect
   */
  class Rect(val diagonal: Vec, override val fg: Brush, override val bg: Brush) extends Glyph {
    override val kind: String = "Rect"
    override def toString: String = s"Rect($diagonal, fg=$fg, bg=$bg)"

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      val sw = paint.getStrokeWidth / 2
      val ww = (w - sw)
      val hh = (h - sw)
      val vertices = Array(
        sw, sw,
        ww, sw,
        ww, hh,
        sw, hh,
        sw, sw)
      surface.canvas.drawPolygon(vertices, fg)
    }

    def copy(fg: Brush=fg, bg: Brush=bg): Rect = new Rect(diagonal, fg, bg)
  }

  object Rect  {
    def apply(w: Scalar, h: Scalar, fg: Brush = Brushes.blackLine, bg: Brush = Brushes.transparent): Rect = new Rect(Vec(w, h), fg, bg)
  }

  object BlurredFrame  {
    def apply(blur: Scalar, spread: Scalar, fg: Brush=Brushes.blackLine, bg: Brush=Brushes.transparent, dx: Scalar=0f, dy: Scalar=0f, fudge: Scalar=1.75f)(glyph: Glyph): Glyph =
      new BlurredFrame(Some(glyph), glyph.diagonal, blur, spread, fg, bg, dx, dy).enlarged(fudge*(blur+spread), bg=Brushes.transparent)
  }

  /**
   * A shadow frame, possibly surrounding a glyph. The extent of the colouring of the frame
   * is `diagonal+fudge*(blur+spread)` and the frame is drawn at with `(-dx, -dy)` so that
   * if there's an `inside` and/or `bg` they get placed at the "densest" part of the frame.
   *
   * The bounding box is given as `diagonal` though this doesn't take account of the extent of
   * the frame. The companion object's `apply`, and `empty` methods both yield a diagonal that
   * does take account of the extent of the frame.
   *
   * @param inside the (optional) glyph
   * @param diagonal the diagonal of the inside of the rectangle
   * @param blur the degree of blur of the rectangle (outside the diagonal)
   * @param spread the extent of the spread of the rectangle (outside the diagonal)
   * @param fg the colour of the shadow box
   * @param bg the colour (if any) of the background on which inside (if any) is drawn
   * @param dx the horizontal offset of the shadow from the origin of the drawn shadow box
   * @param dy the vertical  offset of the shadow from the origin of the drawn shadow box
   */
  class BlurredFrame(val inside: Option[Glyph], val diagonal: Vec, blur: Scalar, spread: Scalar, override val fg: Brush, override val bg: Brush, val dx: Scalar, dy: Scalar) extends Glyph {
    override val kind: String = "BlurredFrame"
    override def toString: String = s"BlurredFrame($diagonal, $blur, $spread dx=$dx, dy=$dy fg=$fg, bg=$bg)"

    def draw(surface: Surface): Unit = {
      //println(s"$this.draw($inside)")
      surface.withOrigin(-dx, -dy) {
        surface.drawShadow(fg.color, diagonal, dx, dy, blur, spread, true)
        surface.withClip(diagonal) { drawBackground(surface) }
        inside match {
          case None =>
          case Some(glyph) => glyph.draw(surface)
        }
      }
    }

    def copy(fg: Brush=fg, bg: Brush=bg): BlurredFrame = new BlurredFrame(inside, diagonal, blur, spread, fg, bg, dx, dy)
  }

  /**
   *
   * An open (or solid) round rectangle of the given diagonal. The curve at each corner is in two arcs, whose radii are determined by
   * multiples of `s` -- the smaller of `diagonal.x, diagonal.y`. The arc that ends laterals has radius `s*xrf`, and the arc
   * that ends verticals has radius `s*yrf`.  The smaller the multiple, the tighter-looking are the corners.
   */
  class RRect(val solid: Boolean, val xrf: Scalar, val yrf: Scalar, val diagonal: Vec, override val fg: Brush, override val bg: Brush) extends Glyph {

    import io.github.humbleui.types.{RRect => SRRECT}
    override val kind: String = "RRect"

    override def toString: String = s"RRect($diagonal, xrf=$xrf, fg=$fg, bg=$bg)"
    val sw = paint.getStrokeWidth
    val rr = diagonal.x min diagonal.y

    val r  = SRRECT.makeLTRB(0f, 0f, diagonal.x, diagonal.y, rr*xrf, rr*yrf)
    @inline private def s  = SRRECT.makeLTRB(sw, sw, diagonal.x-sw, diagonal.y-sw, (rr-2*sw)*xrf, (rr-2*sw)*yrf)

    def draw(surface: Surface): Unit = {
      //drawBackground(surface)// not needed
      if (solid)
        surface.canvas.drawRRect(r, paint)       // solid is its own background
      else {
        if (bg.color!=0) surface.canvas.drawRRect(r, bg)     // substantive  background
        surface.canvas.drawDRRect(r, s, paint)  // overlaid with drrect
      }
    }

    def copy(fg: Brush = fg, bg: Brush = bg): RRect = new RRect(solid, xrf, yrf, diagonal, fg, bg)
  }

  /**
   * An open (or solid) round rectangle of the given diagonal. The curve at each corner is in two arcs, whose radii are determined by
   * multiples of `s` -- the smaller of `diagonal.x, diagonal.y`. The arc that ends laterals has radius `s*xrf`, and the arc
   * that ends verticals has radius `s*yrf`. The smaller the multiple, the tighter-looking are the corners.
   */
  object RRect {

    def apply(w: Scalar, h: Scalar, solid: Boolean = true, xrf: Scalar = .25f, yrf: Scalar = .25f, fg: Brush = Brushes.black, bg: Brush = Brushes.transparent): RRect =
      new RRect(solid, xrf, yrf, Vec(w, h), fg, bg)
  }


  /**
   * A filled rectangular box with the given diagonal.
   *
   * @see Rect
   */
  class FilledRect(val diagonal: Vec, override val fg: Brush, override val bg: Brush) extends Glyph {
    override val kind: String = "FilledRect"
    override def toString: String = s"FilledRect($diagonal, fg=$fg, bg=$bg)"

    def draw(surface: Surface): Unit = {
      surface.fillRect(paint, 0f, 0f, w, h)
    }

    def copy(fg: Brush=fg, bg: Brush=bg): FilledRect = new FilledRect(diagonal, fg, bg)
  }

  object FilledRect  {
    def apply(w: Scalar, h: Scalar, fg: Brush = Brushes.transparent, bg: Brush = Brushes.transparent): FilledRect = new FilledRect(Vec(w, h), fg, bg)
  }

  /**
   * A filled oval that fits in a box with the given `diagonal`.
   */
  class FilledOval(val diagonal: Vec, override val fg: Brush, override val bg: Brush) extends Glyph {
    override val kind: String = "FilledOval"
    override def toString: String = s"FilledOval($diagonal, fg=$fg, bg=$bg)"

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.drawOval(paint, Vec.Origin, diagonal)
    }

    def copy(fg: Brush=fg, bg: Brush=bg): FilledOval = new FilledOval(diagonal, fg, bg)
  }

  object FilledOval  {
    def apply(w: Scalar, h: Scalar, fg: Brush = Brushes.black, bg: Brush = Brushes.transparent): FilledOval = new FilledOval(Vec(w, h), fg, bg)
  }

  /**
   * Connected lines specified by vertices. Bounding box as specified by `box`, unless
   * box is `Vec.Zero`; in that case the box is calculated from the normalized (to the
   * positive quadrant) vertices, with allowance made for the strokewidth of `fg`.
   */
  class Polygon(box: Vec, vertices: Iterable[(Scalar, Scalar)], override val fg: Brush, override val bg: Brush) extends Glyph {
    override val kind: String = "Polygon"
    override def toString: String = s"Polygon($diagonal, fg=$fg, bg=$bg)(\n     ${vertices.mkString(",")}\n)"
    val compiled: Array[Scalar] = Surface.arrayOfPairs(vertices)
    val diagonal: Vec =
      if (box != Vec.Zero) box else {
        val xs = vertices.map{ case (x, _) => x }
        val ys = vertices.map{ case (y, _) => y }
        val minx = xs.min
        val maxx = xs.max
        val miny = ys.min
        val maxy = ys.max
        // normalize the Polygon
        if (minx<0 || miny<0) {
          for { ix <- 0 until compiled.length } {
            if (ix%2==0) compiled(ix) -= minx else compiled(ix) -= miny
            compiled(ix) += fg.strokeWidth/2
          }
        }
        Vec(maxx-minx+fg.strokeWidth, maxy-miny+fg.strokeWidth)
      }

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withClip(diagonal) { surface.drawPolygon(paint, compiled) }
    }

    def copy(fg: Brush=fg, bg: Brush=bg): Polygon = new Polygon(diagonal, vertices, fg, bg)
  }

  object Polygon {

    def apply(diagonal: Vec, fg: Brush, bg: Brush): Constructor = Constructor(diagonal, fg, bg)
    def apply(w: Scalar, h: Scalar, fg: Brush = Brushes.black, bg: Brush = Brushes.transparent): Constructor = Constructor(Vec(w, h), fg, bg)

    case class Constructor(diagonal: Vec, fg: Brush, bg: Brush) {
      def apply(vertices: Iterable[Vec]): Glyph =
          new Polygon(diagonal, vertices.map { v => (v.x, v.y) }, fg, bg)
      def apply(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Glyph =
          new Polygon(diagonal, vertex::vertices.toList, fg, bg)
      def apply(vertices: Seq[(Scalar, Scalar)]): Glyph =
        new Polygon(diagonal, vertices, fg, bg)
    }
  }

  /**
   * Filled Polygon: lines specified by vertices
   *
   * TODO: subler `glyphContaining`
   */
  class FilledPolygon(val diagonal: Vec, vertices: Iterable[(Scalar, Scalar)], override val fg: Brush, override val bg: Brush) extends Glyph {

    import io.github.humbleui.skija.Path

    override val kind: String = "FilledPolygon"
    override def toString: String = s"FilledPolygon($diagonal, fg=$fg, bg=$bg)(\n       ${vertices.mkString(",")}\n)"
    val path = new GlyphTypes.Path()
    locally {
      val (sx, sy) = vertices.head
      path.moveTo(sx, sy)
      for {(x, y) <- vertices} path.lineTo(x, y)
    }

    override def glyphContaining(p: Vec): Option[Hit] = {
      val result = { // if (0 < p.x && p.x < diagonal.x && 0 < p.y && p.y < diagonal.y)
          if (path.contains(p.x, p.y)) Some(Hit(this, p)) else None
      }
      // println(s"$this.contains($p) = $result")
      result
    }

    override def contains(p: Vec): Boolean = {
      val ap = p - rootDistance
      path.contains(ap.x, ap.y)
    }

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withClip(diagonal) { surface.drawPath(paint, path) }
    }

    def copy(fg: Brush = fg, bg: Brush = bg): FilledPolygon = new FilledPolygon(diagonal, vertices, fg, bg)
  }

  object FilledPolygon  {
    import Brushes.{black, transparent}

    case class Constructor(diagonal: Vec, fg: Brush, bg: Brush) {
      def apply(vertices: Iterable[Vec]): Glyph =
        new FilledPolygon(diagonal, vertices.map { v => (v.x, v.y) }, fg, bg)
      def apply(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Glyph =
        new FilledPolygon(diagonal, vertex::vertices.toList, fg, bg)
      def apply(vertices: Seq[(Scalar, Scalar)]): Glyph =
        new FilledPolygon(diagonal, vertices, fg, bg)
    }

    def apply(w: Scalar, h: Scalar, fg: Brush = black, bg: Brush = transparent): Constructor = Constructor(Vec(w, h), fg, bg)
  }

/**
 * A (Mutable) Path Glyph.
 *
 * Its diagonal always denotes the distance from the top left to the
 * bottom right coordinate of the pixels drawn on by the path, and
 * these coordinates are always "normalized" (ie made relative to
 * the bounding rectangle) before the path is drawn.
 *
 * If `strictContains` then the bounds of the glyph are taken to be the
 * "outside" of the path; else they are taken to be its
 * bounding rectangle. This may be useful when constructing reactive glyphs
 * such as circular buttons.
 *
 */
class Path(override val fg: Brush, override val bg: Brush, strictContains: Boolean, val path: GlyphTypes.Path = new GlyphTypes.Path) extends Glyph {
  hostShape =>

  import GlyphTypes.PathFillMode

  /** Normalize all coordinates before drawing */
  protected val normalize: Boolean = true

  locally {
    if (!normalize) path.moveTo(0, 0)
  }

  def reset(): Unit = {
    path.reset()
    if (!normalize) path.moveTo(0, 0)
  }

  /**
   * Does this glyph contain the absolute location `p`.
   */
  override def contains(p: Vec): Boolean = {
    val relp = p - rootDistance
    if (strictContains) path.contains(relp.x, relp.y) else super.contains(p)
  }

  override def glyphContaining(p: Vec): Option[Hit] = {
    val result = { // if (0 < p.x && p.x < diagonal.x && 0 < p.y && p.y < diagonal.y)
      if (path.contains(p.x, p.y)) Some(Hit(this, p)) else None
    }
    // println(s"$this.contains($p) = $result")
    result
  }

  def draw(surface: Surface): Unit = surface.withClip(diagonal) {
    if (normalize){
      drawBackground(surface)
      val bounds = path.getBounds()
      val offsetL = bounds._left - fg.strokeWidth / 2
      val offsetT = bounds._top - fg.strokeWidth / 2
      surface.withOrigin(-offsetL, -offsetT) {
        surface.drawPath(fg, path)
      }
    }
    else {
      drawBackground(surface)
      surface.drawPath(fg, path)
    }
  }

  def topLeft: Vec = {
    val bounds = path.getBounds()
    Vec(bounds._left, bounds._top)
  }

  def diagonal: Vec = {
    val bounds = path.getBounds()
    Vec(bounds._right - bounds._left + fg.strokeWidth, bounds._bottom - bounds._top + fg.strokeWidth)
  }

  def fillMode: PathFillMode = path.getFillMode

  def fillMode(mode: PathFillMode) = path.setFillMode(mode)

  def moveTo(x: Scalar, y: Scalar): Path = {
    path.moveTo(x, y)
    this
  }

  def moveTo(pos:(Scalar, Scalar)): Path = {
    path.moveTo(pos._1, pos._2)
    this
  }

  def lineTo(x: Scalar, y: Scalar): Path = {
    path.lineTo(x, y)
    this
  }

  def lineTo(pos:(Scalar, Scalar)): Path = {
    path.lineTo(pos._1, pos._2)
    this
  }

  def closePath: Path = {
    path.closePath()
    this
  }

  def addRect(x: Scalar, y: Scalar, w: Scalar, h: Scalar): Path = {
    import io.github.humbleui.types.Rect.makeXYWH
    path.addRect(makeXYWH(x, y, w, h)); this
  }

  def addOval(x: Scalar, y: Scalar, w: Scalar, h: Scalar): Path = {
    import io.github.humbleui.types.Rect.makeXYWH
    path.addOval(makeXYWH(x, y, w, h)); this
  }

  def addOval(pos: (Scalar, Scalar), dim: (Scalar,Scalar)): Path = {
    import io.github.humbleui.types.Rect.makeXYWH
    path.addOval(makeXYWH(pos._1, pos._2, dim._1, dim._2)); this
  }

  def addArc(x: Scalar, y: Scalar, w: Scalar, h: Scalar, startAngle: Scalar, sweepAngle: Scalar): Path = {
    import io.github.humbleui.types.Rect.makeXYWH
    path.addArc(makeXYWH(x, y, w, h), startAngle, sweepAngle); this
  }

  def addArc(pos: (Scalar, Scalar), dim: (Scalar,Scalar), startAngle: Scalar, sweepAngle: Scalar): Path = {
    import io.github.humbleui.types.Rect.makeXYWH
    path.addArc(makeXYWH(pos._1, pos._2, dim._1, dim._2), startAngle, sweepAngle); this
  }

  def addCircle(x: Scalar, y: Scalar, r: Scalar): Path = {
    path.addCircle(x, y, r); this
  }

  def addCircle(pos: (Scalar, Scalar), r: Scalar): Path = {
    path.addCircle(pos._1, pos._2, r); this
  }

  def addPath(shape: Path, x: Scalar, y: Scalar): Path = {
    path.addPath(shape.path, x-shape.w/2, y-shape.h/2)
    this
  }

  def addPath(shape: Path, pos: (Scalar, Scalar)): Path = {
    path.addPath(shape.path, pos._1-shape.w/2, pos._2-shape.h/2)
    this
  }

  override def toString: String = s"Path($fg)(${path.getVerbs.toSeq.mkString(",")})"


  /** A copy of this glyph that shares the same path */
  def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new Path(fg, bg, strictContains, path)
}


/**.
 * (Obsolete but retained) class that was used to support hyphenation in a prototype
 * of `glyphML` (now `GlyphXMLDeprecated`. Superseded by classes defined in the `HYPHENATION` module.
 *
 * @param hyphen
 * @param glyphs
 *
 * @see HYPHENATION
 */
  case class BreakableGlyph(hyphen: Glyph, glyphs: Seq[Glyph]) extends Glyph {
    val rep = NaturalSize.Row(Top)(glyphs)
    override val baseLine = glyphs.head.baseLine
    def draw(surface: Surface): Unit = rep.draw(surface)
    def diagonal: Vec = rep.diagonal
    def copy(fg: Brush=this.fg, bg: Brush=this.bg): Glyph = new BreakableGlyph(hyphen, glyphs)
    override val fg: Brush = glyphs.head.fg
    override val bg: Brush = glyphs.head.bg
    /** Index of the first of the glyphs that doesn't
     * fit within `w`.
     */
    def maximal(width: Scalar):Int = {
      var w: Scalar = 0
      var i = 0
      while (i<glyphs.length && w+glyphs(i).w<width) {
        w += glyphs(i).w
        i += 1
      }
      i
    }
  }

  object Label
  {

    import Brushes.{black => defaultFG, transparent => defaultBG}

    import io.github.humbleui.skija.Font

    def apply(text: String, font: Font = fallback.buttonFont, fg: Brush = defaultFG, bg: Brush = defaultBG): Glyph =
        unstyled.Text(text, font, fg, bg)
  }

  /**
   *  An empty glyph of diagonal `(width, height)` intended to be used as a spacer
   */
  object Empty {
    def apply(width: Scalar, height: Scalar): Glyph = new Glyph {
      def draw(surface: Surface): Unit = ()
      def diagonal: Vec = Vec(width, height)
      def copy(fg: Brush=Brushes.transparent, bg: Brush=Brushes.transparent): Glyph = Empty(width, height)
      override val fg: Brush = Brushes.transparent
      override val bg: Brush = Brushes.transparent
    }
  }

