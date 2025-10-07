package org.sufrin.glyph
package tests.explorer


import cached._
import files.Folder
import styled._
import tests.explorer.Explorer.{dialogueLabel, dialogueSheet, fileSheet, iconSheet, menuIcon, openOrdinaryFile}
import tests.explorer.PathProperties._
import unstyled.dynamic.Keyed

import java.awt.Desktop
import java.nio.file._


object ExplorerCollection {
  private var _serial = 0
  def nextSerial(): Int = { _serial += 1; _serial }
}

/** A collection of `Explorer` with a single GUI that shows the currently-selected `Explorer`.
  * When there is more than one explorer others can be selected from a menu popped up by the
  * topmost leftmost button.
  *
  * @param rootPath the initially-selected `Explorer`s path.
  */

class ExplorerCollection(rootPath: Path) extends ExplorerServices {
  thisExplorerCollection =>

  lazy val explorers = Keyed[Path, Explorer](_.GUI)(
    rootPath -> new Explorer(Folder(rootPath), thisExplorerCollection)
  )

  def visibleExplorer:  Explorer         = explorers.selected
  def visibleProvider:  ExplorerServices = visibleExplorer.provider // = this
  def visibleFolder:    Folder    = visibleExplorer.folder
  def visibleSelection: Seq[Path] = visibleExplorer.selectedPaths

  lazy val GUI: Glyph = {
    lazy val iconHint = Hint(
      3,
      if (explorers.size > 1) s"(Menu of size ${explorers.size})"
      else s"Open $rootPath",
      false,
      Some(Vec(0, menuIcon.diagonal.y / 2))
    )

    lazy val iconButton: Glyph = MenuGlyphButton(menuIcon, hint = iconHint) {
      _ =>
        if (explorers.size > 1)
          styled.windowdialogues
            .Menu(explorers.keys.toSeq.map { key =>
              MenuButton(s"Open ${key.toString}") { _ =>
                openExplorerWindow(key)
              }
            })
            .East(iconButton)
            .start()
        else
          openExplorerWindow(rootPath)
    }(iconSheet)

    val closeButton = TextButton("Close Directory"){
      _ =>
        visibleExplorer.provider.closeExplorer(visibleExplorer.folder.path)
    }

    lazy val newButton = TextButton("New Window", hint=Hint(2, "Start a new window")){
      _ =>
        visibleSelection match {
          case paths if paths.length != 1 => visibleExplorer.provider.openServices(visibleExplorer.folder.path)
          case paths if paths.length == 1 => visibleExplorer.provider.openServices(paths.head)
        }

    }

    NaturalSize.Col(align = Center)(
      FixedSize.Row(width = explorers.w, align = Mid)(
        iconButton,
        newButton,
        closeButton,
        iconSheet.hFill(),
        HelpGUI.button(Explorer.fileSheet)
      ),
      explorers
    ).enlarged(10)

  }

  val serial: Int = ExplorerCollection.nextSerial()

  override val toString: String = explorers.keys
    .map(_.toString)
    .mkString(s"ExplorerCollection#$serial(", " ", ")")

  def closeExplorer(path: Path): Unit = {
    if (explorers.size > 1) {
      explorers.selectPrev(path)
      explorers.remove(path)
    }
  }

  def nextExplorer(path: Path): Unit = {
    if (explorers.size > 1) {
      explorers.selectNext(path)
    }
  }

  def prevExplorer(path: Path): Unit = {
    if (explorers.size > 1) {
      explorers.selectPrev(path)
    }
  }

  def openServices(path: Path): Unit = {
    assert(path.isReadable, s"$path is not readable")
    val window = styled.windowdialogues.Dialogue
      .FLASH(new ExplorerCollection(path).GUI)(fileSheet)
      .NorthEast(GUI)
      .withAutoScale()
    window.andThen(
      floating = false,
      onClose = { _ => Explorer.fine(s"Closed $thisExplorerCollection") }
    )
  }

  def openExplorerWindow(path: Path): Unit = {
    if (explorers.isDefinedAt(path)) {
      explorers.select(path)
    } else {
      val folder = Folder(path)
      val explorer = new Explorer(folder, thisExplorerCollection)
      explorers.add(path, explorer)
      explorers.select(path)
    }
    explorers.selected.giveupFocus()
    explorers.select(path)
  }

  def openPath(path: Path): Unit = {
    if (path.isReadable) {
      if (path.isDir) {
        try {
          openExplorerWindow(path)
        } catch {
          case ex: NotDirectoryException =>
            styled.windowdialogues.Dialogue
              .OK(Label(s"Not a directory\n${ex.getMessage}"))
              .OnRootOf(GUI)
              .start()
        }
      } else
        openOrdinaryFile(path)
    } else if (path.isDir) { // Apparently non-readable: may still have ACL permissions as a directory
      val desktop = Desktop.getDesktop
      if (desktop.isSupported(Desktop.Action.OPEN))
        try desktop.open(path.toFile) // Opens with platform default
        catch {
          case ex: java.io.IOException =>
            styled.windowdialogues.Dialogue
              .OK(dialogueLabel(s"Failed to open\n${path.toRealPath()}"))(
                dialogueSheet
              )
              .InFront(GUI)
              .start(floating = false)
        }
    } else {
      styled.windowdialogues.Dialogue
        .OK(dialogueLabel(s"Unreadable\n$path"))(dialogueSheet)
        .InFront(GUI)
        .start()
    }
  }
}
