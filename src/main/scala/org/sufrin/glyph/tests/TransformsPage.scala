package org.sufrin.glyph.tests
import org.sufrin.glyph.DefaultBrushes._
import org.sufrin.glyph.GlyphTypes._
import org.sufrin.glyph.NaturalSize.Col
import org.sufrin.glyph.Styles.GlyphStyle
import org.sufrin.glyph.styled.TextLayout.TextLabel
import org.sufrin.glyph.{Glyph, Notebook}


object TransformsPage extends Notebook {
    implicit val labelStyle: GlyphStyle = PortmanteauStyle.labelStyle
    def dummy(name: String): Glyph = Col.centered(TextLabel(s"Page ($name) intentionally left blank"))
    Page("Turn", "Turn transforms") (dummy("Turn"))

    Page("Tight", "") (dummy("Tight"))

    Page("Skew", "Skew transforms") (dummy("Skew"))

    Page("Mirror", "Mirroring and skewing\n(using intrinsic Glyph Transforms)") (dummy("Mirror"))
}
