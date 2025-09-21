package org.sufrin.glyph
import org.sufrin.glyph.cached.Cached
import org.sufrin.glyph.NaturalSize.{Col, Row}

import java.nio.file.Path

object Shelf {

  def GUI(implicit sheet: StyleSheet): Glyph = styled.TextButton("Shelf", hint=shelfHint(sheet)){
    _ =>
  }

  private var paths: Seq[Path] = Seq.empty
  private var localSheet: StyleSheet = null

  val shelfGlyphs: Cached[Seq[Glyph]] = Cached {
    val detail = localSheet.labelStyle
    paths.map { path => unstyled.Text(path.toString, detail.font, detail.fg, detail.bg) }
  }

  val shelfShow: Cached[Glyph] = Cached {
    val length = shelfGlyphs.value.length
    val left   = Col(shelfGlyphs.value.take(length/2))
    val right  = Col(shelfGlyphs.value.drop(length/2))
    Row(bg=Brushes.white)(left, localSheet.em, right).enlarged(10) framed(Brushes.blackLine)
  }

  def shelfHint(sheet: StyleSheet): Hint = {
    localSheet = sheet
    Hint.ofGlyph(5, shelfShow.value, constant = false)(sheet)
  }


  def put(files: Seq[Path]): Unit = {
    paths=files
    shelfGlyphs.clear()
    shelfShow.clear()
  }

  def clear(): Unit = {
    paths=Seq.empty
    shelfGlyphs.clear()
    shelfShow.clear()
  }

  def isEmpty: Boolean  = paths.isEmpty
  def nonEmpty: Boolean = paths.nonEmpty

}