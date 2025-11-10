package org.sufrin.glyph
package glyphML
import glyphML.Context.Context
import GlyphTypes.Scalar
import NaturalSize.{Col, Row}
import unstyled.static.INVISIBLE

import org.sufrin.logging.SourceDefault
import org.sufrin.logging.SourceDefault._

/** Geometric transforms and framing package */
object TransformsPackage extends Package {
  def define(primitives: Definitions): Unit = {
    new TransformsPackage(primitives).define()
  }
}


class TransformsPackage(definitions: Definitions) {

  def makeRow(glyphs: Seq[Glyph]): Glyph = glyphs.length match {
    case 1 => glyphs.head
    case _ => NaturalSize.Row(align=Mid)(glyphs)
  }

  def turn(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val degrees: Int = inheritedAttributes.Int("degrees", inheritedAttributes.Int("deg", 90*inheritedAttributes.Int("quadrants", 0)))
    def turn(glyph: Glyph): Glyph =
      degrees match {
        case 0 => glyph
        case d =>
          val glyph$ = glyph.turned(d.toFloat, false)
          glyph$
      }
    val derivedContext: Context = context.updated(inheritedAttributes.without("deg", "degrees", "quads", "quadrants"), element.tag)
    val glyph = turn(makeRow(children.flatMap(translator.translate(derivedContext))))
    List(glyph.withBaseline(0.5f *(glyph.h + derivedContext.sheet.exHeight)))
  }

  def scale(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val proportion = inheritedAttributes.Scale("scale", 1f)(context)
    val derivedContext: Context = context.updated(inheritedAttributes.without("proportion"), element.tag)
    val glyph = makeRow(children.flatMap(translator.translate(derivedContext))).scaled(proportion)
    List(glyph.withBaseline(0.5f *(glyph.h + derivedContext.sheet.exHeight)))
  }

  def skew(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val sky = inheritedAttributes.Scale("skewy", 0)(context)
    val skx = inheritedAttributes.Scale("skewx", 0)(context)
    val derivedContext: Context = context.updated(inheritedAttributes.without("skewx", "skewy"), element.tag)
    val glyph = makeRow(children.flatMap(translator.translate(derivedContext))).skewed(skewX=skx, skewY=sky)
    List(glyph.withBaseline(0.5f *(glyph.h + derivedContext.sheet.exHeight)))
  }

  def frame(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val enlarge: Scalar = inheritedAttributes.Units("enlarge", 0.14f)(context)
    val radius: Scalar = inheritedAttributes.Units("radius", 0)(context)
    val fg  = inheritedAttributes.Brush("fg", inheritedAttributes.Brush("frameforeground", Brushes.black))
    val bg  = inheritedAttributes.Brush("bg", inheritedAttributes.Brush("framebackground", Brushes.transparent))
    val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg", "radius", "enlarge"), element.tag)
    val unframed = makeRow(children.flatMap(translator.translate(derivedContext)))
    val glyph =
      if (radius==0f)
         styles.decoration.Edged(fg, bg, enlarge, radius).decorate(unframed)
       else
         styles.decoration.RoundFramed(fg, bg, enlarge, radius).decorate(unframed)
         //unframed.roundFramed(fg, bg, radius = radius)
    List(glyph.withBaseline(0.5f *(glyph.h + derivedContext.sheet.exHeight)))
  }

  def superscript(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val scaleFactor    = inheritedAttributes.Scale("scalefactor", 0.8f)(context)
    val scriptScale = inheritedAttributes.Scale("scriptscale", scaleFactor*context.sheet.fontScale)(context)
    val mainScale   = inheritedAttributes.Scale("mainscale", context.sheet.fontScale)(context)
    val attrs       = inheritedAttributes.without("scriptscale", "mainscale")
    val scriptCxt   = context.updated(Map("fontscale" -> s"$scriptScale") supersede attrs, element.tag)
    val mainCxt     = context.updated(Map("fontscale" -> s"$mainScale") supersede attrs, element.tag)
    val nonempties  = element.child.flatMap(AbstractSyntax.topmostNonempty)

    if (nonempties.length<2) {
      SourceDefault.warn(s"Not enough arguments ${context.scope}<${element.tag}")
      nonempties.flatMap(translator.translate(mainCxt))
    } else {
      val main   = (Row(align = Baseline)(nonempties.take(1).flatMap(translator.translate(mainCxt))))
      val script = (Row(align = Baseline)(nonempties.drop(1).flatMap(translator.translate(scriptCxt))))
      val scaling = script.h / main.h > 1f
      val scaledscript = if (scaling) Col(align=Left)(script.scaled(1.4f*main.h/script.h), INVISIBLE(w=1, h=main.h/2)) else script
      val glyph = if (scaling) Row(align = Bottom)(List(main, scaledscript)) else Row(align = Top)(List(main, script))
      List(glyph.withBaseline(0.5f *(glyph.h + mainCxt.sheet.exHeight)))
    }
  }

  def subscript(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val scaleFactor    = inheritedAttributes.Scale("scalefactor", 0.8f)(context)
    val scriptScale    = inheritedAttributes.Scale("scriptscale", scaleFactor*context.sheet.fontScale)(context)
    val mainScale      = inheritedAttributes.Scale("mainscale", context.sheet.fontScale)(context)
    val attrs          = inheritedAttributes.without("scriptscale", "mainscale")
    val scriptCxt      = context.updated(Map("fontscale" -> s"$scriptScale") supersede attrs, element.tag)
    val mainCxt        = context.updated(Map("fontscale" -> s"$mainScale") supersede attrs, element.tag)
    val nonempties     = element.child.flatMap(AbstractSyntax.topmostNonempty)

    if (nonempties.length<2) {
      SourceDefault.warn(s"Not enough arguments ${context.scope}<${element.tag}")
      nonempties.flatMap(translator.translate(mainCxt))
    } else {
      val main   = (Row(align = Baseline)(nonempties.take(1).flatMap(translator.translate(mainCxt))))
      val script = (Row(align = Baseline)(nonempties.drop(1).flatMap(translator.translate(scriptCxt))))
      val scaling = script.h / main.h > 1f
      val scaledscript = if (scaling) Col(align=Left)(INVISIBLE(w=1, h=10), script.scaled(1.4f*main.h/script.h)) else script
      val glyph = if (scaling) Row(align = Top)(List(main, scaledscript)) else Row(align = Bottom)(List(main, script))
      List(glyph.withBaseline(0.5f *(glyph.h + mainCxt.sheet.exHeight)))
    }
  }

  def fraction(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._

    val scaleFactor    = inheritedAttributes.Scale("scalefactor", 0.8f)(context)
    val scale          = inheritedAttributes.Scale("scale", scaleFactor*context.sheet.fontScale)(context)
    val linebrush      = inheritedAttributes.Brush("fg", context.sheet.textForegroundBrush)
    val attrs          = inheritedAttributes.without("scale", "scalefactor")
    val derivedContext = context.updated(Map("fontscale" -> s"$scale") supersede attrs, element.tag)
    val nonempties     = element.child.flatMap(AbstractSyntax.topmostNonempty)
    val aboveBarSkip   = derivedContext.sheet.textFont.getMetrics.getDescent

    if (nonempties.length<2) {
      SourceDefault.error(s"Not enough arguments ${context.scope}<${element.tag}")
      nonempties.flatMap(translator.translate(context))
    } else {
      val top    = (Row(align = Baseline)(nonempties.take(1).flatMap(translator.translate(derivedContext))))
      val bottom = (Row(align = Baseline)(nonempties.drop(1).flatMap(translator.translate(derivedContext))))
      val glyph  = (top --- INVISIBLE(h=aboveBarSkip) --- Shape.rect((top.w max bottom.w)*1.05f, 3*(1+linebrush.strokeWidth))(linebrush) --- bottom).asGlyph
      List(glyph.withBaseline(0.5f *(glyph.h + derivedContext.sheet.exHeight)))
    }
  }


  def bracket(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}
    val resolved = new ResolveScopedAttributes(definitions, element)
    import resolved._
    val fg  = inheritedAttributes.Brush("fg", context.sheet.textForegroundBrush(width=2))
    val bg  = inheritedAttributes.Brush("bg", context.sheet.textBackgroundBrush(width=2))
    val bra = inheritedAttributes.String("bra", "(")
    def mirror(symbol: String): String = symbol match {
      case "(" => ")"
      case "[" => "]"
      case "{" => "}"
      case "<" => ">"
      case "|" => "|"
      case ""  => ""
      case other => other
    }
    val ket = inheritedAttributes.String("ket", mirror(bra))
    val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg", "bra", "ket"), element.tag)
    if (children.isEmpty) {
      SourceDefault.error(s"Not enough arguments ${context.scope}<bracket ${inheritedAttributes.mkString}")
      Nil
    } else {
      val coreGlyph = makeRow(children.flatMap(translator.translate(derivedContext)))
      val w = context.sheet.emWidth*0.75f
      val w2 = w*0.5f
      val d = fg.strokeWidth
      val h = coreGlyph.h -2*d
      val dd = d


      def sym(symbol: String): Glyph =
      symbol match {
        case "(" => unstyled.static.Polygon(w=w, h=h, fg)((w/2+d, 0), (d+d,w/2),   (d+d,h-w/2),   (w/2+d,h))
        case "{" => unstyled.static.Polygon(w=w, h=h, fg)((w/2+dd, 0), (d+d,w/2),  (dd+d,h/2-w/2),  (d, h/2), (dd+d,h/2+w/2), (dd+d,h-w/2), (d+w/2,h))
        case "}" => unstyled.static.Polygon(w=w, h=h, fg)((w/2-d,0),  (w-d-d, w/2),  (w-d-d,h/2-w/2), (w-d, h/2), (w-d-d,h/2+w/2), (w-d-d, h-w/2),  (w/2-d,h))
        case ")" => unstyled.static.Polygon(w=w, h=h, fg)((w/2,0),  (w-d, w/2),  (w-d,h-w/2),                 (w/2,h))
        case "[" => unstyled.static.Polygon(w=w, h=h, fg)((w/2, 0), (d,0),   (d,h), (w/2,h))
        case "<" => unstyled.static.Polygon(w=w, h=h, fg)((w/2, 0), (d,h/2), (w/2,h))
        case "|" => unstyled.static.Polygon(w=w, h=h, fg)((w/2, 0), (w/2,h))
        case ">" => unstyled.static.Polygon(w=w, h=h, fg)((w/2, 0), (w-d,h/2), (w/2,h))
        case "]" => unstyled.static.Polygon(w=w, h=h, fg)((w/2,0),  (w-d, 0),  (w-d,h), (w/2,h))
        case ""  => INVISIBLE(w=w, h=h)
        case _   => unstyled.static.Polygon(w=w*1.1f, h=h, fg)((w/2, 0), (w/2,h))
      }

      val ketGlyph = sym(ket)
      val braGlyph = sym(bra)
      val glyph = Row(align = Mid)(braGlyph, coreGlyph, ketGlyph)
      List(glyph.withBaseline(0.5f *(glyph.h + derivedContext.sheet.exHeight)))
    }
  }

  def define(): Unit = {
    definitions("turn") = StoredExtension (turn)
    definitions("rotate") = StoredExtension(turn)
    definitions("scale") = StoredExtension(scale)
    definitions("skew") = StoredExtension(skew)
    definitions("frame") = StoredExtension(frame)
    definitions("superscript") = StoredExtension(superscript)
    definitions("subscript") = StoredExtension(subscript)
    definitions("bracket") = StoredExtension(bracket)
    definitions("fraction") = StoredExtension(fraction)
  }
}
