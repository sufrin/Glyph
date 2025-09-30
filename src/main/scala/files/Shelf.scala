package org.sufrin.glyph
package files

import cached.Cached
import NaturalSize.{Col, Row}

import java.nio.file.Path

object Shelf {

  def hintGlyph(caption: String)(implicit sheet: StyleSheet): Glyph = {
    localSheet = sheet
    Col(align=Center)(styled.Label(caption)(localSheet), _shelfHint.value).enlarged(10, bg=Brushes.white).framed(Brushes.blackLine)
  }

  private var _onChange: Int => Unit = { _ => }
  def onChange(action: Int=>Unit): Unit = _onChange = action

  private var _paths: Seq[Path] = Seq.empty
  def paths: Seq[Path] = _paths
  def paths_(paths: Seq[Path]): Unit = add(paths)
  private var localSheet: StyleSheet = null

  var forCut: Boolean = false

  val _shelfGlyphs: Cached[Seq[Glyph]] = Cached {
    val detail = localSheet.copy(fontScale=0.8f).labelStyle
    paths.map { path => unstyled.Text(path.toString, detail.font, detail.fg, detail.bg) }
  }

  val _shelfHint: Cached[Glyph] = Cached {
    val length = _shelfGlyphs.value.length
    val left   = Col(_shelfGlyphs.value.take(length/2))
    val right  = Col(_shelfGlyphs.value.drop(length/2))
    if (length==0)
      styled.Label("(Shelf is empty)")(localSheet)
    else
    Col(align=Center, bg=Brushes.lightGrey)(
       Row(bg=Brushes.white)(left, localSheet.em, right),
    )
  }

  def add(paths: Seq[Path]): Unit = {
    _paths=paths
    _shelfGlyphs.clear()
    _shelfHint.clear()
    _onChange(paths.length)
  }

  def remove(paths: Seq[Path]): Unit = {
    _paths=_paths.filterNot(paths.contains(_))
    _shelfGlyphs.clear()
    _shelfHint.clear()
    _onChange(paths.length)
  }

  def clear(): Unit = {
    _paths=Seq.empty
    _shelfGlyphs.clear()
    _shelfHint.clear()
    forCut = false
    _onChange(0)
  }

  def contains(path: Path): Boolean = _paths.contains(path)
  def isEmpty: Boolean  = paths.isEmpty
  def nonEmpty: Boolean = paths.nonEmpty
  def length: Int = paths.length

  // Registrations
  val


}