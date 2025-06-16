package org.sufrin
package glyph

import org.sufrin.glyph.GlyphTypes._

trait GlyphColours {
  val fg : Brush
  val bg : Brush

  @inline def paint: Brush = fg
  def colourString: String = s"[fg=$fg bg=$bg]"
  override def toString: String = colourString
}

object CellFit {
  trait Method
  case object ShiftNorth extends Method
  case object ShiftNorthWest extends Method
  case object ShiftWest extends Method
  case object ShiftSouthWest extends Method
  case object ShiftSouth extends Method
  case object ShiftSouthEast extends Method
  case object ShiftEast extends Method
  case object ShiftNorthEast extends Method
  case object Stretch extends Method
  case object Enlarge extends Method

  val transparent: Brush = Brush("transparent")

  /**
   *  The given glyph: transformed according to its `cellFitMethod` to fit the given `(w,h)`
   */
    import GlyphTransforms.Scaled
    /** method should be `glyph.cellFitMethod` */
    def fitToCell(method: Method, w: Scalar, h: Scalar, fg: Brush, bg: Brush)(glyph: Glyph): Glyph = {
      method match {
        case Enlarge     => glyph.enlargedTo(w, h, fg, bg)
        case Stretch  =>
          val wscale = w/glyph.w
          val hscale = h/glyph.h
          Scaled(wscale, hscale, fg, bg)(glyph)
        case ShiftNorth      => glyph.inCavity(w, h, (w-glyph.w)/2f, 0f)
        case ShiftNorthWest  => glyph.inCavity(w, h, 0f, 0f)
        case ShiftWest       => glyph.inCavity(w, h, 0f, (h-glyph.h)/2f)
        case ShiftSouthWest  => glyph.inCavity(w, h, 0f, h-glyph.h)
        case ShiftSouth      => glyph.inCavity(w, h, (w-glyph.w)/2f, h-glyph.h)
        case ShiftSouthEast  => glyph.inCavity(w, h, w-glyph.w, h-glyph.h)
        case ShiftEast       => glyph.inCavity(w, h, w-glyph.w, (h-glyph.h)/2f)
        case ShiftNorthEast  => glyph.inCavity(w, h, w-glyph.w, 0f)
      }
    }
}


/**
 *  A `Glyph` is the unit of interface construction.
 *
 *  `Glyph`s are not "aware" of the absolute location on the screen
 *  surface at which they will be drawn. Instead, all glyphs are
 *  drawn on drawing surfaces as if they were placed at the origin.
 *
 *  TODO: The present signature has a few too many auxiliary methods, many of which have
 *        only very rarely been used (for example, the various cellFit, and
 *        shift methods). These should probably migrate or be removed.
 *
 *  Composite `Glyph`s are composed of sequences of glyphs, each of
 *  which has a location relative to the origin of the composite.
 *  The origin of the drawing surface is translated by this amount
 *  before each component is drawn.
 */
trait Glyph extends GlyphShape with GlyphColours with GlyphTransforms { thisGlyph =>

  import scala.annotation.tailrec

  lazy val baseLineText = s"[$baseLine]"
  def kind = "Glyph"
  override def toString: String = s"$kind(${diagonal.x},${diagonal.y})$colourString$baseLineText@@$location"

  def isReactive: Boolean = false

  /**
   * The closest glyph containing the point `p`.
   *
   * @see Composite
   */
  def glyphContaining(p: Vec): Option[Hit] = {
    if (0 < p.x && p.x < diagonal.x && 0 < p.y && p.y < diagonal.y) Some(Hit(this, p)) else None
  }

  /**
   * The closest reactive ancestor glyph containing this glyph.
   * Used by the event handler when mouse focus must be
   * re-established after an event.
   */
  def reactiveParent: Option[ReactiveGlyph] = {
    var target: Glyph = this
    while (!target.isRoot && !target.isInstanceOf[ReactiveGlyph]) target = target.parent
    target match {
      case glyph: ReactiveGlyph => Some(glyph)
      case _ => None
    }
  }

  def reactiveContaining(p: Vec): Option[ReactiveGlyph]  =
    glyphContaining(p) match {
      case None                => None
      case Some(Hit(glyph, _)) =>
        glyph.reactiveParent
        /*match { //TODO: relativise the hit
          case None  => None
          case other =>  other
        }*/
    }

  /**
   * Run `effect(this)` only if `sat(this)`. For implementing
   * glyph-tree visitors.
   *
   * @see Composite
   */
  def forEach(sat: Glyph=>Boolean)(effect: Glyph=> Unit): Unit = {
    if (sat(this)) effect(this)
  }

  /**
   * draw the glyph's background, if its colour means something
   */
  def drawBackground(surface: Surface): Unit =
  {
    if ((bg ne null) && bg.getAlpha != 0) surface.fillRect(bg, diagonal)
  }


  @inline def bottom: Scalar = h + location.y
  @inline def right: Scalar  = w + location.x


  /**
   *  Expandability of this glyph (within `NaturalSize` grids)
   */
  private var cellFitMethod: CellFit.Method = CellFit.Enlarge

  /**
   * Set the current `cellFitMethod` of this glyph
   * @param method
   * @return this
   */
  def cellFit(method: CellFit.Method): this.type = { cellFitMethod=method; this }

  /**
   * Fit this glyph to a cell of size `(w, h)` as specified by its current
   * `cellFitMethod`
   *
   * @param w width of the cell
   * @param h height of the cell
   * @param fg foreground used if any expansion happens
   * @param bg background used if any expansion happens
   * @return this glyph padded (etc) to fit `(w, h)` using its current `cellFitMethod`
   */
   def fitToCell(w: Scalar, h: Scalar, fg: Brush = CellFit.transparent, bg: Brush=CellFit.transparent): Glyph =
       CellFit.fitToCell(thisGlyph.cellFitMethod, w, h, fg, bg)(thisGlyph)

  /**
   * Stretchability: used when (eg) a stretchable space is to be expanded to make
   * the content of a FixedSize container (Row, Col) fit.
   */
  val xStretch: Scalar = 0.0f
  val yStretch: Scalar = 0.0f


  /**
   *  Vertical drop from a given location to the baseline. Usually
   *  derived (or inherited) from the baselines of one or more fonts.
   */
  def baseLine: Scalar = 0f

  ////////////// TODO: invoke an efficient redrawing
  /**
   *  Request a redraw of the (entire) associated window
   *  NB: it might be possible to make this more efficient
   *  by redrawing only higher nodes in the glyph tree,
   *  but the complexity appears not to be worth it, since
   *  there is no perceivable difference in rendering speed
   *  -- the redraw is hidden by double-buffering of the drawn
   *  GUI.
   */
  def reDraw(): Unit = guiRoot.reDraw()

  /** Is this an enabled reactive glyph */
  def enabled(state: Boolean): Boolean = {
    false
  }


  ////////////// Parents and ancestors

  /*
   * TODO: Could it be more efficient to use a single public variable?
   *       If so would the loss of potential versatility (presently unused anywhere)
   *       be compensated?
   */

  /**
   *  A glyph is its own parent until it becomes the child
   *  of a composite glyph (further down the tree / higher up the
   *  containment hierarchy).
   */
  private var _parent: Glyph = this

  /** This glyph with the specified `Glyph` as parent. */
  def @@(parent: Glyph): this.type = {
    _parent = parent
    this
  }

  def parent_= (theParent: Glyph): Unit = _parent = theParent

  /** Parent in the glyph tree */
  def parent: Glyph = _parent

  /** Is this a root of a glyph tree */
  def isRoot: Boolean = _parent eq this

  /**
   * The path from this glyph to the closest enclosing root glyph
   */
  private final def pathToRoot: List[Glyph] = {
    if (this.isRoot) Nil else this :: this.parent.pathToRoot
  }

  /**
   * Find the ancestor root glyph, or fail
   * @see guiRoot
   */
  @tailrec final def findRoot: RootGlyph = {
    if (this.isRoot) this match {
      case glyph: RootGlyph => glyph
      case _                =>
        assert(false, s"findRoot finds a non-RootGlyph (${this.getClass})"); null
    } else
    //if (this.parent eq this) this else
      this.parent.findRoot
  }

  @tailrec private def isRooted(): Boolean = {
    if (this.isRoot) this match {
      case glyph: RootGlyph => true
      case _                => false
    } else
      this.parent.isRooted()
  }

  /** This glyph has a `guiRoot`
   * TODO: this should be a cache that's invalidated
   *       after re-rooting. It has been a lazy val
   *       but laziness isn't enough.
   *
   */
  @inline final def hasGuiRoot: Boolean = isRooted()

  /**
   *  Cache, invalidated if the current value's window was closed
   */
  private var theRoot: RootGlyph = null

  /**
   * The cached closest enclosing root glyph to this.
   * The cache is refreshed if the root's window is closed.
   * The effect of this is that a GUI tree can be re-used
   * in a popup without having to be rebuilt.
   *
   */
  @inline final def guiRoot: RootGlyph = {
    if ((theRoot eq null) || theRoot.isWindowClosed) theRoot = findRoot
    theRoot
  }

  // Containment

  /**
   * Result: distance from this node to the root of the GUI tree in
   * which it is embedded. This is constant, because the tree stays invariant,
   * once it has been built.
   *
   * Precondition: the glyph is already embedded in a
   * "complete" gui tree.
   */
  lazy val rootDistance: Vec = {
    var loc: Vec = Vec.Zero
    var here: Glyph = this
    while (!here.isRoot) {
      loc = loc+here.location
      here = here.parent
    }
    loc
  }

  /**
   * Returns the `(x, y)` location of the content of this glyph on the screen
   *
   * Used in positioning popup and popup menu windows.
   *
   * @see Location.
   * @see Dialogue.OnScreen
   */
  def rootWindowOrigin: (Int, Int) = guiRoot.windowOrigin

  /**
   * Does this glyph contain the absolute location `p`. The default
   * uses the glyph's bounding box; but implementations
   * for non-rectangular shapes can be more picky.
   */
  def contains(p: Vec): Boolean = {
    val loc: Vec = rootDistance
      loc.x <= p.x && p.x <= loc.x + diagonal.x &&
      loc.y <= p.y && p.y <= loc.y + diagonal.y
  }

  /** Relevant to popping down of menus.
   *  TODO: move elsewhere
   */
  def asMenuButton: this.type = this
  def isMenuButton: Boolean = false

  /**
   * The location of this glyph relative to the parent glyph.
   * This is nearly always fixed by the layout algorithm of its container/composite
   * parent node.
   *
   * @see Row
   * @see Col
   * @see Concentric
   * @see Envelope
   *
   * @see FixedSize.Row
   * @see FixedSize.Col
   */
  var location: Vec = Vec.Zero

  /**
   * Set this glyph's location, and return it.
   */
  def @@(location: Vec): Glyph = { this.location = location; this }

  /**
   * Set this glyph's location.
   */
  def @@(x: Scalar, y: Scalar): Glyph = this @@ Vec(x, y)

  /** A copy of this glyph; perhaps with different foreground/background */
  def copy(fg: Brush=fg, bg: Brush=bg): Glyph
  /** A copy of this glyph; perhaps with different foreground/background */
  def apply(fg: Brush=fg, bg: Brush=bg): Glyph = this.copy(fg, bg)

  /** Abstract a glyphShape from a  `this.copy(fg, bg)` -- necessary only because `Glyph extends GlyphShape` */
  override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = GlyphShape.fromGlyph(this.copy(fg, bg))

  /**
   * A functionally equivalent copy of this glyph, generated within the
   * given `boundingBox`. By default this is
   * the identity. Glyph-generators can be implemented
   * that use the size parameters to determine the
   * details of their internal layout.
   */
   def atSize(boundingBox: Vec): Glyph = this

   /** False unless `atSize` will generate a distinct glyph */
   def resizeable: Boolean = false

   /** yield a glyph that draws bounding box and baseline of this glyph around it */
   def showingBaseline(fg: Brush): Glyph = ShowingBaseline(fg, thisGlyph)
   def showingBaseline: Glyph            = ShowingBaseline(ShowingBaseline.fg, thisGlyph)
}

/**
 *  A hit on glyph at location (relative to its parent)
 */
case class Hit(glyph: Glyph, location: Vec) {
  override def toString: String = s"Hit($glyph@@$location)"
}

/**
 *  A composite glyph that is rendered by rendering each individual glyph at its associated location.
 *  The overall diagonal is the smallest rectangle that bounds the individual glyphs
 *  placed at their locations
 */
abstract class Composite(components: Seq[Glyph]) extends Glyph {

  override def toString: String = s"""$kind(${glyphs.mkString(",\n ")})"""

  def draw(surface: Surface): Unit = {
    drawBackground(surface)
    val delta: Vec = Vec.Zero
    for { glyph <- components} {
          surface.withOrigin(glyph.location+delta){
          glyph.draw(surface)
        }
    }
  }

  /**
   * The glyphs from which this composite was composed
   */
  val glyphs: Seq[Glyph]

  /**
   * The glyphs  from which this composite was composed; but in the order
   * they should be searched (when a reactive glyph is sought).
   */
  def searchGlyphs: Seq[Glyph] = glyphs

  def setParents(): Unit =  {
    for { glyph <- components} glyph.parent = this
  }

  override def glyphContaining(p: Vec): Option[Hit] =
    super.glyphContaining(p) match {
      case None    => None
      case here    =>
        var result: Option[Hit] = None
        val it = searchGlyphs.iterator
        while (result.isEmpty && it.hasNext) {
          val glyph = it.next()
          result = glyph.glyphContaining(p - glyph.location)
        }
        if (result.isEmpty) here else result
    }

  override def forEach(sat: Glyph => Boolean)(effect: Glyph => Unit): Unit = {
    if (sat(this)) effect(this)
    for { glyph <- glyphs} glyph.forEach(sat)(effect)
  }

}

object Glyph {
  @inline def logicalLocation(glyph: Glyph, offset: Vec): (Int, Int) = logicalLocation(glyph.rootDistance+offset)
  @inline def logicalLocation(offset: Vec): (Int, Int)               = (offset.x.toInt, offset.y.toInt)
  /**  Consistent displacement to align (by proportion) a glyph of a given size in the given space */
  @inline def displaceToFit (space: Scalar, proportion: Scalar, size: Scalar): Scalar =  (space - size) * proportion

}

trait AlignableGlyph extends Glyph {

}




