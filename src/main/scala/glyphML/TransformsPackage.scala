package org.sufrin.glyph
package glyphML
import Context.Context
import GlyphTypes.Scalar

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
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
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
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
    import resolved._
    val proportion = inheritedAttributes.Float("scale", 0)
    val derivedContext: Context = context.updated(inheritedAttributes.without("proportion"))
    val glyph = makeRow(children.flatMap(translator.translate(derivedContext))).scaled(proportion)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def skew(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
    import resolved._
    val sky = inheritedAttributes.Float("skewy", 0)
    val skx = inheritedAttributes.Float("skewx", 0)
    val derivedContext: Context = context.updated(inheritedAttributes.without("skewx", "skewy"))
    val glyph = makeRow(children.flatMap(translator.translate(derivedContext))).skewed(skewX=skx, skewY=sky)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def frame(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(definitions, element)
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
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

  def define(): Unit = {
    definitions("turn") = StoredExtension (turn)
    definitions("rotate") = StoredExtension(turn)
    definitions("scale") = StoredExtension(scale)
    definitions("skew") = StoredExtension(skew)
    definitions("frame") = StoredExtension(frame)
  }
}
