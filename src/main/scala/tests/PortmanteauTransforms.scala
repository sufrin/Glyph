package org.sufrin.glyph
package tests

import NaturalSize.Col
import styled.TextLayout.TextLabel

import Styles.NotebookStyle


class PortmanteauTransforms(implicit style: StyleSheet) extends Notebook {
      implicit val pageStyle: NotebookStyle = style.notebookStyle
      def dummy(name: String): Glyph = Col.centered(TextLabel(s"Page ($name) intentionally left blank")) enlarged 50

      Page("Turn", "Turn transforms")(dummy("Turn"))

      Page("Tight", "")(dummy("Tight"))

      Page("Skew", "Skew transforms")(dummy("Skew"))

      Page("Mirror", "Mirroring and skewing\n(using intrinsic Glyph Transforms)")(dummy("Mirror"))
}
