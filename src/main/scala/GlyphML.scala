package org.sufrin.glyph

import Styles.{ButtonStyle, GlyphStyle}
import Styles.Decoration.Decoration

import org.sufrin.logging.Loggable

object GlyphML {
  import java.io.{PrintWriter, StringWriter}

  /**
   * Abstract syntax of a markup language the  meaning of whose elements are `Glyphs`.
   */
   import GlyphTypes._



  /**
   * A context embodies a stylesheet and some useful dimensions and brushes.
   *
   * The `boundingBox` is, if positive, the space permitted for a
   * glyph that is about to be composed in this context.
   *
   * @see Paragraph
   * @see Text
   * @see framed
   */
  case class Context(style: StyleSheet,
                     boundingBox: Vec = Vec.Zero,
                     paragraphWidth: Scalar=0f,
                     leftMargin:  Scalar=0f,
                     rightMargin: Scalar=0f,
                     fg: Brush = DefaultBrushes.black,
                     bg: Brush = DefaultBrushes.nothing,
                     padX: Scalar = 0f,
                     padY: Scalar = 0f,
                     fontFamily: FontFamily = FontFamily(),
                     fontSize:   Scalar = 22,
                     fontStyle:  FontStyle = FontStyle.NORMAL,
                     parIndent:  ()=>Seq[Glyph] = { ()=>Nil },
                     parHang:    Boolean = false,
                     parAlign:   Alignment = Left,
                     parSkip:    Scalar    = 5f
                    ) {

      def font: Font = fontFamily.makeFont(fontStyle, fontSize)

      def labelStyle(fg: Brush=null, bg: Brush=null, font: GlyphTypes.Font=font): Context = {
        val sheet = this.style
        val ls    = sheet.labelStyle
        val nfg   = if (fg ne null) fg else ls.fg
        val nbg   = if (bg ne null) bg else ls.bg
        val nfont = if (font ne null) font else ls.font
        val newStyle = new sheet.Derived {
          override def labelStyle: GlyphStyle = GlyphStyle(fg=nfg, bg=nbg, font=nfont)
        }
        copy(style=newStyle)
      }

      /** Width of the given text using `brush` (default the current `fg`) */
      def textWidth(text: String, brush: Brush=fg): Scalar = font.measureTextWidth(text, brush)

      def emWidth: Scalar = font.measureTextWidth("M")

      def frameStyle(decoration: Decoration, fg: Brush=null, bg: Brush=null): Context = {
        val ls    = this.style.buttonStyle.up
        val nfg   = if (fg ne null) fg else ls.fg
        val nbg   = if (bg ne null) bg else ls.bg
        val newUp = this.style.buttonStyle.up.copy(fg=nfg, bg=nbg)
        val newButtonStyle = this.style.buttonStyle.copy(up = newUp, frame=decoration)
        val newStyle = new this.style.Derived {
          override lazy val buttonStyle: ButtonStyle = newButtonStyle
        }
        copy(style=newStyle)
      }

    def gridStyle(fg: Brush=this.fg, bg: Brush=this.bg, padX: Scalar=this.padX, padY: Scalar=this.padY): Context =
        copy(fg=fg, bg=bg, padX=padX, padY=padY)

    def italicStyle: Context = copy(fontStyle=FontStyle.ITALIC)
    def boldStyle:   Context = copy(fontStyle=FontStyle.BOLD)
    def normalStyle: Context = copy(fontStyle=FontStyle.NORMAL)
    def boldItalicStyle: Context = copy(fontStyle=FontStyle.BOLD_ITALIC)
    def fontFamily(familyName: String): Context = copy(fontFamily=FontFamily(familyName))
    def fontSize(size: Scalar): Context = copy(fontSize = size)
    def fontScale(scale: Scalar): Context = copy(fontSize = this.fontSize*scale)
    def parIndent(ems: Int = 2): Context = {
        copy(parIndent={ () => List(FixedSize.Space(ems*style.labelStyle.emWidth, 0f, 0f, fg, bg)) } )
    }

    def parSkip(points: Scalar): Context = copy(parSkip=points)

    def parEnum(start: Int = 0): Context = {
      var count = start-1
      val label: ()=>List[Glyph] = {
        () =>
           count += 1
           List(AtBaseLine(s"$count.").toGlyph(this))
      }
      copy(parIndent=label)
    }

    def indentEms(left: Int, right: Int): Context =
      copy(leftMargin=leftMargin+(left), rightMargin=rightMargin+(right))

    def paragraphEms(ems: Int): Context =
      paragraphPoints(ems*emWidth)

    def paragraphPoints(points: Scalar): Context =
      copy(paragraphWidth = points)

  }

  val Default: Context =
        Context(Styles.Default, Vec.Zero, 1200f, 10, 10, fg=Glyphs.black, bg=Glyphs.nothing, padX=0, padY=0)

  type ContextTransform = Context => Context
  type GlyphTransform   = Glyph   => Glyph
  def compose[T](transforms: Seq[T=>T]): T=>T = transforms.foldLeft({t:T=>t}){ (tl, tr) => { t: T => tr(tl(t)) }}

  val italicStyle: ContextTransform = _.italicStyle
  val boldStyle:   ContextTransform = _.boldStyle
  def fontSize(size: Scalar): ContextTransform = _.fontSize(size)
  def fontScale(scale: Scalar): ContextTransform = _.fontScale(scale)
  def fontFamily(familyName: String): ContextTransform = _.copy(fontFamily=FontFamily(familyName))
  def parEnum(start: Int = 1): ContextTransform = _.parEnum(start)
  def indentEms(left: Int, right: Int): ContextTransform = _.indentEms(left, right)
  def paragraphEms(ems: Int): ContextTransform = _.paragraphEms(ems)
  def paragraphPoints(points: Scalar): ContextTransform = {
      cxt => cxt.paragraphPoints(points)
  }
  def unBounded: ContextTransform = _.copy(boundingBox = Vec.Zero)
  def bounded(w: Scalar, h: Scalar): ContextTransform = _.copy(boundingBox = Vec(w, h))
  def bounded(box: Vec): ContextTransform = _.copy(boundingBox = box)


  /** A GlyphML `Tree t` derives a `Glyph` in a `Context c` using `t.toGlyph(c)` */
  trait Tree {
      def toGlyph(local: Context): Glyph
  }

  /**
   *  An `Element` can have "local" `ContextTransform`s declared that transform the
   *  context in which the element is being derived.
   */
  trait Element extends Tree { thisElement =>
    def original: Element = this
    /**
     *   Transform the inherited context while deriving the glyph
     *   The `transforms` are applied in left-to-right order:
     *   {{{
     *       elt.locally(t1, t2, ... tN)
     *     = elt(t1)(t2)...(tN)
     *     = (...((elt(t1))(t2))...)(tN)
     *   }}}
     *
     */
    def locally(transforms: ContextTransform*): Element = new Element {
      override def original: Element = thisElement.original
      def toGlyph(local: Context): Glyph = thisElement.toGlyph(compose(transforms)(local))
    }

    /**
     * Yields the element bound forever to the given context; with cached glyph, etc.
     */
    def constant(implicit context: Context): Constant = new Constant(this)(context)

    /** Transform the inherited context locally while deriving the glyph */
    def apply(transform: ContextTransform): Element = new Element {
      override def original: Element = thisElement.original
      def toGlyph(local: Context): Glyph = thisElement.toGlyph(transform(local))
    }

    /** Transform the glyph generated by this `Element` after it has been derived. */
    def \\(glyphTransform: GlyphTransform): Element = new Element {
      def toGlyph(local: Context): Glyph = glyphTransform(thisElement.toGlyph(local))
    }

    /**
     * Measure the enlargement produced by the glyph transform `.framed(...)`
     * This might be considered a wasteful method, but the alternative (dead-reckoning)
     * would induce cross-module dependencies in its implementation.
     */
    object measureFrame {
      val dummy = Glyphs.INVISIBLE()
      def apply(fg: Brush, bg: Brush): Vec = dummy.framed(fg, bg).diagonal
    }

    /**
     *  This `Element` with glyph framed with the given brushes. The
     *  framed glyph is derived in a bounding box that will
     *  accomodate it after framing.
     */
    def framed(fg: Brush=DefaultBrushes.black, bg: Brush = DefaultBrushes.nothing): Element = new Element {
      override def original: Element = thisElement.original
      def toGlyph(local: Context): Glyph = {
        val inset        = measureFrame(fg, bg)
        val boundingBox  = (local.boundingBox - inset).whenPositive
        thisElement.toGlyph(local.copy(boundingBox = boundingBox)).framed(fg, bg)
      }
    }

    /**
     *  This `Element` derived in bounding box scaled by ratios `(wr, hr)`
     */
    def scaled(wr: Scalar, hr: Scalar): Element = new Element {
      override def original: Element = thisElement.original
      def toGlyph(local: Context): Glyph = {
        val Vec(bw, bh) = local.boundingBox
        thisElement.toGlyph(local.copy(boundingBox = Vec(bw*wr, bh*hr)))
      }
    }

  }

  /**
   *   Text in the local font that can be mixed with other such text within an `Element`, styled directly from the context
   */
  case class AtBaseLine(text: String) extends Element {
      def toGlyph(local: Context) : Glyph = {
        val fg=local.style.labelStyle.fg
        val bg=local.style.labelStyle.bg
        org.sufrin.glyph.Text(text, font=local.font, fg=fg, bg=bg).atBaseline(fg, bg)
      }
    }

  /**
   *   Text in the local font that can be mixed with other such text within an `Element`, styled directly from the context
   */
  def S(text: String) : Element = AtBaseLine(text)

  /**
   * Verbatim text in the local font that can stand alone.
   */
  case class Verb(text: String) extends Element {
      def toGlyph(local: Context) : Glyph = {
        val fg = local.style.labelStyle.fg
        val bg = local.style.labelStyle.bg
        org.sufrin.glyph.Text(text, font = local.font, fg = fg, bg = bg).asGlyph(fg, bg)
      }
  }



    /**
     *   A glyph, whose appearance is denoted by the GlyphML element
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
  abstract class Resizeable(context: Context) extends Glyph with GlyphTransforms {
      override def resizeable: Boolean = true

      def element: Element
      var delegate: Glyph = element.toGlyph(context)

      override def atSize(boundingBox: Vec): Glyph = {
        Resizeable.finest(s"atSize($boundingBox) with scale=${guiRoot.softwareScale}")
        delegate = element.toGlyph(context.copy(boundingBox = boundingBox))
        Resizeable.finest(s"delegate.diagonal=${delegate.diagonal} => $diagonal")
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
    def apply(context: Context)(theElement: => Element): Resizeable = new Resizeable(context) {
      def element: Element = theElement
    }
  }

  /**
   *   An `Element` whose glyph is derived from an element that is determined dynamically
   *   in `local: Context` by {{{
   *   select(local.boundingBox).toGlyph(local)
   *   }}}
   *
   *   `Dynamic` elements are intended for use in a context where the
   *   detailed layout of (part of) a GUI depends on the size of the
   *   window in which it will be rendered.
   */
  case class Dynamic (select: Vec => Element, enlarge: Scalar=0f) extends Element {
    def toGlyph(local: Context): Glyph = {
      val elt = select(local.boundingBox)
      val result=elt.toGlyph(local).enlarged(enlarge)
      Resizeable.finest(s"Dynamic.toGlyph(${local.boundingBox})=>${result.diagonal}")
      result
    }
  }

  /** A flexible filler for placement in a `MenuBar` or `SideBar` */
  case object Gap extends Element {
    def toGlyph(local: Context): Glyph = FixedSize.Space.tab
  }

  /**
   * Generate a menu bar from `elements` in the given `local: Context`.  Its width is
   * that specified by `local`'s bounding box.
   */
  case class MenuBar (local: Context)(elements: Element*) extends Element {
    def toGlyph(local: Context): Glyph =
      FixedSize.Row(local.boundingBox.x, local.fg, local.bg).atBottom$(elements.map(_.toGlyph(local)))
  }

  case class FixedWidthRow(width: Scalar)(elements: Element*) extends Element {
    def toGlyph(local: Context): Glyph =
        FixedSize.Row(width, local.fg, local.bg).atBottom$(elements.map(_.toGlyph(local)))
  }

  /**
   * Generate a menu bar from `elements` in the given `local: Context`.  Its height is
   * that specified by `local`'s bounding box.
   */
  case class SideBar (local: Context)(elements: Element*) extends Element {
    def toGlyph(local: Context): Glyph =
      FixedSize.Col(local.boundingBox.y, local.fg, local.bg).atLeft$(elements.map(_.toGlyph(local)))
  }

  case class FixedWidth(width: Scalar)(elements: Element*) extends Element {
    def toGlyph(local: Context): Glyph =
      FixedSize.Row(width, local.fg, local.bg).centered$(elements.map(_.toGlyph(local)))
  }

  case class FixedHeight(height: Scalar)(elements: Element*) extends Element {
    def toGlyph(local: Context): Glyph =
      FixedSize.Col(height, local.fg, local.bg).centered$(elements.map(_.toGlyph(local)))
  }

  /**
   * An element that always generates the same `Glyph`, namely the one
   * generated by `element` in the given `context`.
   *
   * Its `w` and `h` and `diagonal` properties can safely be used after construction; and
   * its `root` property can be used once it has been incorporated in a GUI.
   */
  case class Constant(element: Element)(implicit val local: Context) extends Element {
    val cached: Glyph     = element.toGlyph(local)
    def root: RootGlyph   = cached.findRoot
    def diagonal: Vec     = cached.diagonal
    def w: Scalar         = diagonal.x
    def h: Scalar         = diagonal.y
    def toGlyph(local: Context): Glyph = cached
  }

  /** A glyph viewed as a GlyphML element */
  case class GlyphElement(glyph: Glyph) extends Element {
      def toGlyph(local: Context): Glyph = glyph
  }

  import scala.language.implicitConversions

  /** Coerce a `Glyph` to a `GlyphElement`  */
  implicit def fromGlyphToElement(glyph: Glyph): Element = GlyphElement(glyph)

  /**
   *   Literal text that will be nested within an `Element`, styled directly from local.style.labelStyle
   */
  case class Label(val text: String) extends Element {
    def toGlyph(local: Context): Glyph = {
      val style = local.style
      Glyphs.Label(text, font = style.labelStyle.font, fg = style.labelStyle.fg, bg = style.labelStyle.bg)
    }
  }

  /** A sequence of one or more paragraphs derived from `text`, with paragraph boundaries at empty lines */
  case class Text(val text: String) extends Element { derivedFrom =>
    def toGlyph(local: Context): Glyph = {
      val paras: Seq[Element] =
        text.split("""\n\n+""")
          .filter(_.nonEmpty)
          .toSeq.map(_.split("[ \n]").toSeq.map { word => AtBaseLine(word) })
          .map(Paragraph(_))
      val column = Column$(paras)
      val result = column.toGlyph(local)
      //println(s"[Column${result.diagonal}]")
      result
    }
  }

  def glyphsToPara(glyphs: Seq[Glyph])(local: Context): Glyph = {
      val emWidth   = local.emWidth//.labelStyle.emWidth
      val interWord = FixedSize.Space(w=emWidth / 1.5f, h=0f, stretch = 2f)
      val glyphs$   = local.parIndent() ++ glyphs

      // The overall width is determined by the context
      // If the bounding box is unspecified, then use the column width
      val galley =
        styled.text.glyphParagraph(
          overallWidth = local.paragraphWidth,
          align        = local.parAlign,
          leftMargin   = local.leftMargin * emWidth,
          rightMargin  = local.rightMargin * emWidth,
          interWord,
          glyphs$
        )

      val column = NaturalSize.Col(bg = local.bg).atLeft$(galley.toSeq)
      if (local.leftMargin > 0f)
        NaturalSize.Row(bg = local.bg)
                   .centered(FixedSize.Space(w=local.leftMargin * emWidth, h=0f, stretch=0f),
                             column,
                             FixedSize.Space(w=local.rightMargin * emWidth, h=0f, stretch=0f))
      else
        column
  }

  def Centered(body: Element*): Element = Centered$(body)
  def Column(body: Element*):   Element = Column$(body)

  case class Centered$(body: Seq[Element]) extends Element {
      def toGlyph(local: Context): Glyph = {
        val theGlyph: Glyph = body.length match {
          case 0 => Glyphs.INVISIBLE()
          case 1 => body(0).toGlyph(local)
          case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).rows(1)(body.map(_.toGlyph(local)))
        }
        theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
      }
  }

  case class Column$(body: Seq[Element]) extends Element {
    def toGlyph(local: Context): Glyph = {
      val theGlyph: Glyph = body.length match {
        case 0 => Glyphs.INVISIBLE()
        case 1 => body(0).toGlyph(local)
        case _ => NaturalSize.Col(fg = local.fg, bg = local.bg).atLeft$(body.map(_.toGlyph(local)))
      }
      theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
    }
  }

  def Row(body: Element*): Element =
      Row$(body)

    case class Row$(body: Seq[Element]) extends Element {
      def toGlyph(local: Context): Glyph = {
        val theGlyph: Glyph = body.length match {
          case 0 => Glyphs.INVISIBLE()
          case 1 => body(0).toGlyph(local)
          case _ => NaturalSize.Row(fg = local.fg, bg = local.bg).centered$(body.map(_.toGlyph(local)))
        }
        theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
      }
    }

    case class Table(width: Int=0, height: Int=0)(body: Element*) extends Element {
      def toGlyph(local: Context): Glyph = {
        val theGlyph: Glyph = body.length match {
          case 0 => NaturalSize.Col().centered()
          case 1 => body(0).toGlyph(local)
          case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).table(width, height)(body.map(_.toGlyph(local)))
        }
        theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
      }
    }

    case class Rows(width: Int=0)(body: Element*) extends Element {
      def toGlyph(local: Context): Glyph = {
        val theGlyph: Glyph = body.length match {
          case 0 => NaturalSize.Col().centered()
          case 1 => body(0).toGlyph(local)
          case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).uniformlyByRows(width)(body.map(_.toGlyph(local)))
        }
        theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
      }
    }

  case class RowsFromGlyphs(width: Int=0)(body: Seq[Glyph]) extends Element {
    def toGlyph(local: Context): Glyph = {
      val theGlyph: Glyph = body.length match {
        case 0 => NaturalSize.Col().centered()
        case 1 => body(0).toGlyph(local)
        case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).uniformlyByRows(width)(body)
      }
      theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
    }
  }

  case class ColsFromGlyphs(height: Int=0)(body: Seq[Glyph]) extends Element {
    def toGlyph(local: Context): Glyph = {
      val theGlyph: Glyph = body.length match {
        case 0 => NaturalSize.Col().centered()
        case 1 => body(0).toGlyph(local)
        case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).uniformlyByCols(height)(body)
      }
      theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
    }
  }

    case class Cols(height: Int = 0)(body: Element*) extends Element {
        def toGlyph(local: Context): Glyph = {
          val theGlyph: Glyph = body.length match {
            case 0 => NaturalSize.Col().centered()
            case 1 => body(0).toGlyph(local)
            case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).uniformlyByCols(height)(body.map(_.toGlyph(local)))
          }
          theGlyph.enlargedBy(dw = local.padX, dh = local.padY, fg = local.fg, bg = local.bg)
        }
    }

    /** Paragraph */
    def P(body: Element*) = Paragraph(body)

    case class Paragraph(body: Seq[Element]) extends Element {
        def toGlyph(local: Context): Glyph = {
          val style = local.style
          val emWidth = style.labelStyle.emWidth
          val interWord = FixedSize.Space(emWidth / 1.5f, emWidth, stretch = 2f)
          val glyphs = local.parIndent() ++ body.map(_.toGlyph(local))
          // The overall width is determined by the context
          // If the bounding box is unspecified, then use the column width
          val galley =
            styled.text.glyphParagraph(
                 overallWidth = local.paragraphWidth,
                 align        = local.parAlign,
                 leftMargin   = (local.leftMargin) * emWidth,
                 rightMargin  = (local.rightMargin) * emWidth,
                 interWord,
                 glyphs
            )

          val column = NaturalSize.Col(bg = local.bg).atLeft$(galley.toSeq)
          //println(s"[Para(${contextDeterminedWidth})${column.diagonal}]")
          if (local.leftMargin > 0f)
            NaturalSize.Row(bg = local.bg).centered(FixedSize.Space(local.leftMargin * emWidth, 0f, 0f), column, FixedSize.Space(local.rightMargin * emWidth, 0f, 0f)) else column
        }
    }

  }