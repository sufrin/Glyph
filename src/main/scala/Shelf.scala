package org.sufrin.glyph
import cached.Cached
import NaturalSize.{Col, Row}
import unstyled.reactive.Reaction

import java.nio.file.Path

object Shelf {

  def shelfHint(sheet: StyleSheet): Hint = {
    localSheet = sheet
    Hint.ofGlyph(5, _shelfHint.value, constant = false)(sheet)
  }

  def Button(caption: String, withHint: Boolean)(action: Reaction)(implicit sheet: StyleSheet): Glyph =
      styled.TextButton(caption, hint = if (withHint) Shelf.shelfHint(sheet) else Hint(2, "Copy the selection"))(action)

  private var _onChange: Int => Unit = { _ => }
  def onChange(action: Int=>Unit): Unit = _onChange = action

  private var _paths: Seq[Path] = Seq.empty
  def paths: Seq[Path] = _paths
  def paths_(paths: Seq[Path]): Unit = put(paths)
  private var localSheet: StyleSheet = null

  val _shelfGlyphs: Cached[Seq[Glyph]] = Cached {
    val detail = localSheet.copy(fontScale=0.8f).labelStyle
    paths.map { path => unstyled.Text(path.toString, detail.font, detail.fg, detail.bg) }
  }

  val _shelfHint: Cached[Glyph] = Cached {
    val length = _shelfGlyphs.value.length
    val left   = Col(_shelfGlyphs.value.take(length/2))
    val right  = Col(_shelfGlyphs.value.drop(length/2))
    if (length==0)
      styled.Label("Shelf is empty")(localSheet).enlarged(10).enlarged(10, bg=Brushes.lightGrey).framed(Brushes.blackLine)
    else
    Col(align=Center, bg=Brushes.lightGrey)(
       styled.Label("Shelf")(localSheet),
       Row(bg=Brushes.white)(left, localSheet.em, right),
    ).enlarged(10).framed(Brushes.blackLine)
  }

  def put(paths: Seq[Path]): Unit = {
    _paths=paths
    _shelfGlyphs.clear()
    _shelfHint.clear()
    _onChange(paths.length)
  }

  def clear(): Unit = {
    _paths=Seq.empty
    _shelfGlyphs.clear()
    _shelfHint.clear()
    _onChange(0)
  }

  def isEmpty: Boolean  = paths.isEmpty
  def nonEmpty: Boolean = paths.nonEmpty

}