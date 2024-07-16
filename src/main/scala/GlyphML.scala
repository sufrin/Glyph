package org.sufrin.glyph

import Styles.GlyphStyle

object markup {
  import java.io.{PrintWriter, StringWriter}

  /**
   * Abstract syntax of a markup language the principle meanings of whose elements are `Glyphs`.
   *
   * "Why use a templating language when we already have one?"
   *  (Fr Saul N BrainDrane: "the Pragmatic Philistine")
   *
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
   * A context embodies a stylesheet and some ad-hoc
   * dimensions and brushes.
   */
  case class Context(style: StyleSheet, paperWidth: Scalar, leftMargin: Scalar, rightMargin: Scalar,
                       fg:   Brush,  bg: Brush,
                       padX: Scalar, padY: Scalar,
                       fontFamily: Family = Family(),
                       fontSize:   Scalar = 22,
                       fontStyle:  FontStyle = FontStyle.NORMAL,
                       parIndent:  ()=>Seq[Glyph] = { ()=>Nil },
                       parHang:    Boolean = false
                    ) {

      def font: Font = fontFamily.makeFont(fontStyle, fontSize)

      def labelStyle(fg: Brush=null, bg: Brush=null, font: GlyphTypes.Font=null): Context = {
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
      def boldStyle: Context = copy(fontStyle=FontStyle.BOLD)
      def parIndent(ems: Int): Context = copy(parIndent={ () => List(FixedSize.Space(ems*style.labelStyle.emWidth, 0f, 0f, fg, bg)) } )
      def parEnum(ems: Int=2): Context = {
        var count = 0
        val label: ()=>List[Glyph] = {
          () =>
             count += 1
             List(AtBaseLine(this)(s"$count.").toGlyph.enlargedTo(ems*style.labelStyle.emWidth, style.labelStyle.exHeight))
        }
        copy(parIndent=label)
      }
    }

    val Default: Context =
        Context(Styles.Default, 50, 10, 10, fg=Glyphs.black, bg=Glyphs.nothing, padX=0, padY=0)

    type Transform = Context=>Context
    import Styles._



    trait Tree {
      def toGlyph: Glyph
    }

    trait Element extends Tree

    /**
     *   Literal text that will be nested within an `Element`, styled directly from context
     */
    case class AtBaseLine(local: Context=Default)(text: String) extends Element {
      val fg=local.style.labelStyle.fg
      val bg=local.style.labelStyle.bg
      def toGlyph: Glyph = org.sufrin.glyph.Text(text, font=local.font, fg=fg, bg=bg).atBaseline(fg, bg)
    }

    case class Lit(local: Context=Default)(text: String) extends Element {
      val fg=local.style.labelStyle.fg
      val bg=local.style.labelStyle.bg
      def toGlyph: Glyph = org.sufrin.glyph.Text(text, font=local.font, fg=fg, bg=bg).asGlyph(fg, bg)
    }

    /** Coerce a `String` to a `Text` element */
    implicit def fromStringToElement(text: String)(implicit local: Context): Element = Text(local)(text)
    /** Coerce a `Glyph` to a `GlyphElement`  */
    implicit def fromGlyphToElement(glyph: Glyph): Element = GlyphElement(glyph)
    /** Coerce an `Element` to a `Glyph` */
    implicit def fromElementToGlyph(element: Element): Element = element.toGlyph
    /** Coerce a pair of `Strings` to a */


  case class GlyphElement(glyph: Glyph) extends Element {
      def toGlyph: Glyph = glyph
    }

  /**
   *   Literal text that will be nested within an `Element`, styled directly from local.style.labelStyle
   */
  case class Label(local: Context=Default)(val text: String) extends Element {
    def toGlyph: Glyph = {
      val style = local.style
      // Text(text, font = style.labelStyle.font, fg = style.labelStyle.fg, bg = style.labelStyle.bg).asGlyph(fg, bg)
      Glyphs.Label(text, font = style.labelStyle.font, fg = style.labelStyle.fg, bg = style.labelStyle.bg)
    }
  }

  /** A sequence of one or more paragraphs derived from `text`, with paragraph boundaries at empty lines */
  case class Text(local: Context=Default)(val text: String) extends Element {
      val paras: Seq[Element] =
          text.split("""\n\n+""")
              .filter(_.nonEmpty)
              .toSeq.map(_.split(' ')
                         .toSeq.map{ word => AtBaseLine(local)(word)})
              .map(Paragraph(local)(_))
      val column = Column$(local)(paras)
      def toGlyph: Glyph = column.toGlyph
    }

    def Column(local: Context=Default)(body: Element*): Element =
      Column$(local)(body)

    case class Column$(local: Context=Default)(body: Seq[Element]) extends Element {
      lazy val theGlyph: Glyph = body.length match {
        case 0 => NaturalSize.Col().centered()
        case 1 => body(0).toGlyph
        case _ => NaturalSize.Col(fg=local.fg, bg=local.bg).centered$(body.map(_.toGlyph))
        //NaturalSize.Grid(fg=local.fg, bg=local.bg, pady=local.padY).table(width=1)(body.map(_.toGlyph))
      }
      def toGlyph: Glyph = theGlyph.enlargedBy(dw=local.padX, dh=local.padY, fg=local.fg, bg=local.bg)
    }

    def Row(local: Context=Default)(body: Element*): Element =
      Row$(local)(body)

    case class Row$(local: Context=Default)(body: Seq[Element]) extends Element {
      lazy val theGlyph: Glyph = body.length match {
        case 0 => NaturalSize.Row().centered()
        case 1 => body(0).toGlyph
        case _ => NaturalSize.Row(fg=local.fg, bg=local.bg).centered$(body.map(_.toGlyph))
      }
      def toGlyph: Glyph = theGlyph.enlargedBy(dw=local.padX, dh=local.padY, fg=local.fg, bg=local.bg)
    }

    case class Table(local: Context=Default)(width: Int=0, height: Int=0)(body: Element*) extends Element {
      lazy val theGlyph: Glyph = body.length match {
        case 0 => NaturalSize.Col().centered()
        case 1 => body(0).toGlyph
        case _ => NaturalSize.Grid(fg=local.fg, bg=local.bg, pady=local.padY).table(width, height)(body.map(_.toGlyph))
      }
      def toGlyph: Glyph = theGlyph.enlargedBy(dw=local.padX, dh=local.padY, fg=local.fg, bg=local.bg)
    }

  case class Rows(local: Context=Default)(width: Int=0)(body: Element*) extends Element {
    lazy val theGlyph: Glyph = body.length match {
      case 0 => NaturalSize.Col().centered()
      case 1 => body(0).toGlyph
      case _ => NaturalSize.Grid(fg=local.fg, bg=local.bg, pady=local.padY).uniformlyByRows(width)(body.map(_.toGlyph))
    }
    def toGlyph: Glyph = theGlyph.enlargedBy(dw=local.padX, dh=local.padY, fg=local.fg, bg=local.bg)
  }

  case class Cols(local: Context=Default)(height: Int=0)(body: Element*) extends Element {
    lazy val theGlyph: Glyph = body.length match {
      case 0 => NaturalSize.Col().centered()
      case 1 => body(0).toGlyph
      case _ => NaturalSize.Grid(fg=local.fg, bg=local.bg, pady=local.padY).uniformlyByCols(height)(body.map(_.toGlyph))
    }
    def toGlyph: Glyph = theGlyph.enlargedBy(dw=local.padX, dh=local.padY, fg=local.fg, bg=local.bg)
  }





  /** Paragraph */
    def P(local: Context=Default)(body: Element*): Element = Paragraph(local)(body)

    case class Paragraph(local: Context=Default)(body: Seq[Element]) extends Element {
      val style     = local.style
      val strut     = style.labelStyle.em
      val interWord = FixedSize.Space(strut.w / 1.9f, strut.h, stretch=100f)
      val glyphs    = local.parIndent() ++ body.map(_.toGlyph)
      val galley    = styled.text.glyphParagraph(local.paperWidth * strut.w, Justify,
                                                 local.leftMargin*strut.w,
                                                 local.rightMargin*strut.w, interWord,
                                                 glyphs)
      val bg        = style.labelStyle.bg
      def toGlyph: Glyph = {
        val column = NaturalSize.Col(bg=bg).atLeft$(galley.toSeq)
        if (local.leftMargin > 0f) NaturalSize.Row(bg=bg).centered(FixedSize.Space(local.leftMargin*strut.w, 0f, 0f), column) else column
      }
  }

}

