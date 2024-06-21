package org.sufrin.glyph
package tests

import NaturalSize.Col
import styled.TextLayout.TextLabel


object PortmanteauTransforms extends Notebook {
    implicit val sheet: Styles.Sheet = PortmanteauStyle
    def dummy(name: String): Glyph = Col.centered(TextLabel(s"Page ($name) intentionally left blank")) enlarged 50
    Page("Turn", "Turn transforms") (dummy("Turn"))

    Page("Tight", "") (dummy("Tight"))

    Page("Skew", "Skew transforms") (dummy("Skew"))

    Page("Mirror", "Mirroring and skewing\n(using intrinsic Glyph Transforms)") (dummy("Mirror"))
}
