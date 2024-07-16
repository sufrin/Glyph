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
      |There are few important principles to get to grips with:
      |""".stripMargin),
      Text(
        """Most markup expressions take an implicit context as their
          |final parameter. This can be modified. For example, this
          |paragraph will be set with a narrower overall width. It
          |appears to be centred because the overall makeup of this
          |page is as a centred column of rectangles.
          |
          |""".stripMargin)(local.copy(overallWidth = 50)).toGlyph.framed().enlarged(30, bg=Glyphs.lightGrey),
     Text(
        """We should be back to the usual margins by now. The
          |markup expression that set the previous paragraph was
          |""".stripMargin),
     Text("""Text(...)((local.copy(overallWidth=40))""")(local.labelStyle(fg=red).copy(parAlign=Center)),
     styled.TextButton("I want to know more!"){
       _ =>
     }
  ).toGlyph
}
