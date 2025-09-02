package org.sufrin.glyph
package glyphML
import glyphML.Context.Context
import GlyphTypes.Scalar
import NaturalSize.Row

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
    val derivedContext: Context = context.updated(inheritedAttributes.without("deg", "degrees", "quads"))
    val glyph = turn(makeRow(children.flatMap(translator.translate(derivedContext))))
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def scale(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val proportion = inheritedAttributes.Float("scale", 0)
    val derivedContext: Context = context.updated(inheritedAttributes.without("proportion"))
    val glyph = makeRow(children.flatMap(translator.translate(derivedContext))).scaled(proportion)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def skew(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val sky = inheritedAttributes.Float("skewy", 0)
    val skx = inheritedAttributes.Float("skewx", 0)
    val derivedContext: Context = context.updated(inheritedAttributes.without("skewx", "skewy"))
    val glyph = makeRow(children.flatMap(translator.translate(derivedContext))).skewed(skewX=skx, skewY=sky)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def frame(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._
    val enlarge: Scalar = inheritedAttributes.Float("enlarge", 0.14f)
    val radius: Scalar = inheritedAttributes.Float("radius", 0)
    val fg  = inheritedAttributes.Brush("fg", inheritedAttributes.Brush("frameforeground", Brushes.black))
    val bg  = inheritedAttributes.Brush("bg", inheritedAttributes.Brush("framebackground", Brushes.transparent))
    val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg", "radius", "enlarge"))
    val unframed = makeRow(children.flatMap(translator.translate(derivedContext)))
    val glyph =
      if (radius==0f)
         styles.decoration.Edged(fg, bg, enlarge, radius).decorate(unframed)
       else
         styles.decoration.RoundFramed(fg, bg, enlarge, radius).decorate(unframed)
         //unframed.roundFramed(fg, bg, radius = radius)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def superscript(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._

    val topScale = inheritedAttributes.Scale("superscriptscale", 0.7f*context.sheet.fontScale)(context)
    val botScale = inheritedAttributes.Scale("mainscale", context.sheet.fontScale)(context)
    val attrs    = inheritedAttributes.without("superscriptscale", "mainscale")
    val topCxt = context.updated(Map("fontscale" -> s"$topScale") supersede attrs)
    val botCxt = context.updated(Map("fontscale" -> s"$botScale") supersede attrs)
    val nonempties = element.child.flatMap(AbstractSyntax.topmostNonempty)

    if (nonempties.length<2) {
      SourceDefault.error(s"Not enough arguments ${element.scope}")
      Nil
    } else {
      val bottom = (Row(align = Baseline)(nonempties.take(1).flatMap(translator.translate(botCxt))))
      val top = (Row(align = Baseline)(nonempties.drop(1).flatMap(translator.translate(topCxt))))
      val glyph = Row(align = Top)(List(bottom, top))

      List(glyph)
    }
  }

  def subscript(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{ExtendedAttributeMap, TypedAttributeMap}

    import resolved._

    val subscriptScale = inheritedAttributes.Scale("subscriptscale", 0.7f*context.sheet.fontScale)(context)
    val mainScale    = inheritedAttributes.Scale("mainscale", context.sheet.fontScale)(context)
    val attrs        = inheritedAttributes.without("subscriptscale", "mainscale")
    val subscriptCxt = context.updated(Map("fontscale" -> s"$subscriptScale") supersede attrs)
    val mainCxt      = context.updated(Map("fontscale" -> s"$mainScale") supersede attrs)
    val nonempties   = element.child.flatMap(AbstractSyntax.topmostNonempty)

    if (nonempties.length<2) {
      SourceDefault.error(s"Not enough arguments ${element.scope}")
      Nil
    } else {
      val main = (Row(align = Baseline)(nonempties.take(1).flatMap(translator.translate(mainCxt))))
      val subscript = (Row(align = Baseline)(nonempties.drop(1).flatMap(translator.translate(subscriptCxt))))
      val glyph = Row(align = Bottom)(List(main, subscript))
      List(glyph)
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
  }
}
