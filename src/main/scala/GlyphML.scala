package org.sufrin.glyph

import Styles.GlyphStyle

object markup {
  import java.io.{PrintWriter, StringWriter}

  /**
   * Abstract syntax of a markup language the  meaning of whose elements are `Glyphs`.
   */
   import GlyphTypes._

   case class Family(name: String="Menlo") {
     lazy val normalFace:     Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.NORMAL)
     lazy val boldFace:       Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.BOLD)
     lazy val italicFace:     Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.ITALIC)
     lazy val boldItalicFace: Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.BOLD_ITALIC)
     def  makeFont(style: GlyphTypes.FontStyle, size: Scalar): Font = {
       style match {
         case FontStyle.NORMAL      => new Font(normalFace, size)
         case FontStyle.BOLD        => new Font(boldFace, size)
         case FontStyle.ITALIC      => new Font(italicFace, size)
         case FontStyle.BOLD_ITALIC => new Font(boldItalicFace, size)
       }
     }
     def apply(size: Scalar)(style: FontStyle): Font = makeFont(style, size)
     def apply(style:FontStyle)(size: Scalar): Font = makeFont(style, size)
   }

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
                     columnWidth: Scalar=1200f,
                     leftMargin:  Scalar=0f,
                     rightMargin: Scalar=0f,
                     fg: Brush = Glyphs.black,
                     bg: Brush = Glyphs.nothing,
                     padX: Scalar = 0f,
                     padY: Scalar = 0f,
                     fontFamily: Family = Family(),
                     fontSize:   Scalar = 22,
                     fontStyle:  FontStyle = FontStyle.NORMAL,
                     parIndent:  ()=>Seq[Glyph] = { ()=>Nil },
                     parHang:    Boolean = false,
                     parAlign:   Alignment = Left
                    ) {

      lazy val font: Font = fontFamily.makeFont(fontStyle, fontSize)

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

    def gridStyle(fg: Brush=this.fg, bg: Brush=this.bg, padX: Scalar=this.padX, padY: Scalar=this.padY): Context =
        copy(fg=fg, bg=bg, padX=padX, padY=padY)

    def italicStyle: Context = copy(fontStyle=FontStyle.ITALIC)
    def boldStyle:   Context = copy(fontStyle=FontStyle.BOLD)
    def fontSize(size: Scalar): Context = copy(fontSize = size)
    def fontScale(scale: Scalar): Context = copy(fontSize = this.fontSize*scale)
    def parIndent(ems: Int = 2): Context =
        copy(parIndent={ () => List(FixedSize.Space(ems*style.labelStyle.emWidth, 0f, 0f, fg, bg)) } )
    def parEnum(start: Int = 0): Context = {
      var count = start-1
      val label: ()=>List[Glyph] = {
        () =>
           count += 1
           List(AtBaseLine(s"$count.").toGlyph(this))
      }
      copy(parIndent=label)
    }

  }

  val Default: Context =
        Context(Styles.Default, Vec.Zero, 1200f, 10, 10, fg=Glyphs.black, bg=Glyphs.nothing, padX=0, padY=0)

  type ContextTransform = Context=>Context
  val italicStyle: ContextTransform = _.italicStyle
  val boldStyle:   ContextTransform = _.boldStyle
  def fontSize(size: Scalar): ContextTransform = _.fontSize(size)
  def fontScale(scale: Scalar): ContextTransform = _.fontScale(scale)
  def fontFamily(familyName: String): ContextTransform = _.copy(fontFamily=Family(familyName))
  def parEnum(start: Int = 1): ContextTransform = _.parEnum(start)
  def margins(left: Int, right: Int): ContextTransform = {
      case c: Context => c.copy(leftMargin=c.leftMargin+left, rightMargin=c.rightMargin+right)
  }


  trait Tree {
      def toGlyph(local: Context): Glyph
  }

  trait Element extends Tree { derivedFrom =>
    /** Transform the inherited context while generating the glyph */
    def locally(transformer: ContextTransform): Element = new Element {
      def toGlyph(local: Context): Glyph = derivedFrom.toGlyph(transformer(local))
    }

    /** Transform the inherited context locally while generating the glyph */
    def apply(transformer: ContextTransform): Element = this.locally(transformer)

    def \\(glyphTransform: Glyph=>Glyph): Element = new Element {
      def toGlyph(local: Context): Glyph = glyphTransform(derivedFrom.toGlyph(local))
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

    def framed(fg: Brush=DefaultBrushes.black, bg: Brush = DefaultBrushes.nothing): Element = new Element {
      def toGlyph(local: Context): Glyph = {
        val inset        = measureFrame(fg, bg)
        val boundingBox  = (local.boundingBox - inset).whenPositive
        derivedFrom.toGlyph(local.copy(boundingBox = boundingBox)).framed(fg, bg)
      }
    }
  }

  /**
   *   Text that will be nested within an `Element`, styled directly from the context
   */
  case class AtBaseLine(text: String) extends Element {
      def toGlyph(local: Context) : Glyph = {
        val fg=local.style.labelStyle.fg
        val bg=local.style.labelStyle.bg
        org.sufrin.glyph.Text(text, font=local.font, fg=fg, bg=bg).atBaseline(fg, bg)
      }
    }

  /**
   *   Text that will be nested within an `Element`, styled directly from the context
   */
  def S(text: String) : Element = AtBaseLine(text)

  /**
   * Verbatim text
   */
  case class Verb(text: String) extends Element {
      def toGlyph(local: Context) : Glyph = {
        val fg = local.style.labelStyle.fg
        val bg = local.style.labelStyle.bg
        org.sufrin.glyph.Text(text, font = local.font, fg = fg, bg = bg).asGlyph(fg, bg)
      }
  }



    /**
     *   A glyph, whose appearance is the glyph denoted by the markup element
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
      var delegate: Glyph = atSize(context.boundingBox)

      override def atSize(boundingBox: Vec): Glyph = {
        //print(s"atSize($boundingBox)")
        delegate = element.toGlyph(context.copy(boundingBox = boundingBox))
        //println(s"->${delegate.diagonal}")
        delegate
      }

      def element: Element

      /**
       * Draw the glyph on the surface at its given size (as if at the origin).
       */
      def draw(surface: Surface): Unit = delegate.draw(surface)

      /**
       * The diagonal size of the glyph
       */
      def diagonal: Vec = delegate.diagonal

      /** A copy of this glyph; perhaps with different foreground/background */
      def copy(fg: Brush=delegate.fg, bg: Brush=delegate.bg): Glyph = delegate.copy(fg, bg)

      val fg: Brush = delegate.fg
      val bg: Brush = delegate.bg

      override def glyphContaining(p: Vec): Option[Hit] = delegate.glyphContaining(p)
      override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = delegate.reactiveContaining(p)

    }

  /** A glyph viewed as a markup element */
  case class GlyphElement(glyph: Glyph) extends Element {
      def toGlyph(local: Context): Glyph = glyph
  }

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
          .toSeq.map(_.split(' ').toSeq.map { word => AtBaseLine(word) })
          .map(Paragraph(_))
      val column = Column$(paras)
      val result = column.toGlyph(local)
      //println(s"[Column${result.diagonal}]")
      result
    }
  }

  def Column(body: Element*): Element = Column$(body)

  case class Column$(body: Seq[Element]) extends Element {
      def toGlyph(local: Context): Glyph = {
        val theGlyph: Glyph = body.length match {
          case 0 => Glyphs.INVISIBLE()
          case 1 => body(0).toGlyph(local)
          case _ => NaturalSize.Grid(fg = local.fg, bg = local.bg, pady = local.padY).rows(1)(body.map(_.toGlyph(local)))
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
                 overallWidth = if (local.boundingBox.x==0) local.columnWidth else local.boundingBox.x.floor,
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