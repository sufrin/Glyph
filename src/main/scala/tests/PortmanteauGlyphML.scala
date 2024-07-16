package org.sufrin.glyph
package tests


class PortmanteauGlyphML(implicit sheet: StyleSheet) {
  import Glyphs.{blue, lightGrey, nothing, red}
  import markup._
  implicit val local: Context =
    Context(     style        = sheet,
                 overallWidth = 60,
                 leftMargin   = 0,
                 rightMargin  = 0,
                 parAlign     = Justify)
      .copy(fontFamily=Family("Courier"))
      .fontSize(28)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)

  val intro = P(S("An introduction to"), S("GlyphML")(local.boldStyle))(local.copy(parAlign=Center))

  val GUI: Glyph = Column(
      intro,
      Text("""GlyphML is a domain-specific language embedded in Scala: its expressions denote Glyphs.
      |
      |The GlyphML API (which is still evolving rapidly) may eventually be slightly more convenient
      |than the standard Glyphs API for interface designers who don't need to get to grips with its
      |underview.
      |
      |There are a couple of important principles to get to grips with:
      |""".stripMargin),
      Text(
        """Most markup expressions take a context as their
          |final parameter, and this is usually supplied implicitly.
          |
          |But it can be modified: for example, the current
          |paragraph has a narrower overall width.
          |
          |""".stripMargin)(local.copy(overallWidth = 50)).toGlyph.framed().enlarged(30, bg=Glyphs.lightGrey),
     Text(
        """The narrower paragraph appears to be centred because the overall makeup of this
          |page is as a centred column of rectangles.
          |
          |The markup expression denoting the previous paragraph modified the current implicit
          |context by supplying a modified context explicitly:
          |""".stripMargin),
     Text("""Text("Most ... width")(local.copy(overallWidth=40))""")(local.labelStyle(fg=red).copy(parAlign=Center)),
     styled.TextButton("I want to know more!"){
       _ =>
     }
  ).toGlyph
}
