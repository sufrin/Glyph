package org.sufrin.glyph
package tests

// TODO: clean up the baseline/non-baseline mess.

class PortmanteauGlyphML(implicit sheet: StyleSheet) {
  import GlyphML._
  import DefaultBrushes.{blue, black, nothing, red}
  implicit val local: Context =
    Context(     style        = sheet,
                 parWidth  = 800f,
                 leftMargin   = 0,
                 rightMargin  = 0,
                 parAlign     = Justify,
                 fontFamily   = FontFamily("Menlo"),
                 fontSize     = 22)
      .labelStyle(fg=black, bg=nothing)
      .gridStyle(bg=nothing, fg=nothing, padY=8, padX=8)

  val intro = Row(S("An introduction to "), S("GlyphML")(boldStyle))(fontFamily("Arial"))(fontScale(1.6f))

  val GUI: Glyph = new Resizeable(local) {
     def element: Element = Centered(
      intro,
      Text("""GlyphML is a domain-specific language embedded in Scala: its expressions denote Glyphs.
             |
             |We hope the GlyphML API (which is still evolving) will eventually be more
             |convenient than the standard Glyphs API for use
             |by interface designers who are not inclined to study the latter in detail.
             |
             |Nevertheless, there are a couple of important principles to get to grips with:
             |""".stripMargin),
      Text(
        """Markup Elements are the abstract syntax of a language
          |of expressions whose meanings are, in effect, functions
          |from contexts to glyphs. Contexts provide the style and dimension
          |information needed when an element is translated to a Glyph for use in a GUI.
          |
          |The defs context can be modified in the scope of an element,
          |by applying transforms to the element itself.
          |
          |For example, the current
          |section of text has enumerated paragraphs, is framed, and is  narrower than those surrounding it.
          |""".stripMargin)(parEnum(1))(fontSize(20))(indentEms(2, 2)).framed(fg=blue),
      Text(
        """The Text expression denoting the framed section above used
          |context transforms and an element transform as follows:
          |""".stripMargin),
       Verb("Text(\"...\")(parEnum(1))(fontSize(20))(margins(5, 5)).framed(fg=blue)")(fontFamily("Courier"))(fontScale(0.75f)),
       styled.TextButton("I want to know more!"){ _ => }
     ).framed(fg=red strokeWidth 20f)
  }
}
