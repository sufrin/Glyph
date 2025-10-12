package org.sufrin.glyph
package tests.barents


import cached._
import files.{Folder, Shelf}
import styled._
import tests.barents.PathProperties._
import tests.barents.Viewer.{dialogueLabel, dialogueSheet, fileSheet, openOrdinaryFile}
import unstyled.dynamic.Keyed
import GlyphTypes.Scalar
import unstyled.static.{Concentric, FilledRect}
import NaturalSize.{Col, Row}

import java.awt.Desktop
import java.nio.file._


object ViewerWindow {
  private var _serial = 0
  def nextSerial(): Int = { _serial += 1; _serial }

  lazy val SHELF: Glyph = {
    val plank   = FilledRect(120, 20, fg=Brushes.darkGrey)
    val planket = FilledRect(50, 20, fg=Brushes.black) above plank()
    val plonket = FilledRect(30, 25, fg=Brushes.black) above plank()
    val upright = FilledRect(20, 4*plank.h+plonket.h, fg=Brushes.darkGrey)
    Row(align=Bottom)(upright,
                   Col(plonket,
                       plank(fg=Brushes.transparent),
                       planket,
                       plank(fg=Brushes.transparent),
                       plank(),
                       ),
                   upright()).scaled(0.15f).withBaseline(20)
  }

  lazy val CLEARSHELF: Glyph = {
    Concentric(SHELF, GlyphShape.line(SHELF.diagonal.scaled(-1, -1), SHELF.diagonal)(Brushes.redLine).asGlyph)
  }

}

/**
  *  A collection of `Viewer`, one of which is deemed to be selected.
  *  Its GUI shows the currently-selected `Viewer`.
  *
  *  When there is more than one viewer others can be selected from a menu
  *
  * @param rootPath the initially-selected `Viewer`s path.
  */

class ViewerWindow(rootPath: Path) extends ViewerServices {
  thisViewerWindow =>

  lazy val viewers = Keyed[Path, Viewer](_.GUI)(
    rootPath -> new Viewer(Folder(rootPath), thisViewerWindow)
  )

  def close(): Unit = viewers.values.foreach(_.close())

  def visibleViewer:    Viewer           = viewers.selected
  def visibleServices:  ViewerServices   = visibleViewer.services // usually = this
  def visibleFolder:    Folder           = visibleViewer.folder
  def visiblePath:      Path             = visibleViewer.folder.path
  def visibleSelection: Seq[Path]        = visibleViewer.selectedPaths
  def visibleActions:   ActionProvider   = visibleViewer

  lazy val GUI: Glyph = {
    def menuHint = Hint(0.8, viewers.keys.map(_.toString).mkString(s"Views\n", "\n", ""), constant=false)

    lazy val viewButton: Glyph = TextButton("View", hint = menuHint) {
      _ =>
        if (viewers.size > 1)
          styled.windowdialogues
            .Menu(viewers.keys.toSeq.map { key =>
              MenuButton(s"Open view of ${key.toString}") { _ =>
                openExplorerWindow(key)
              }
            } . prepended(
               MenuButton(s"Close view of $visiblePath"){ _ => closeExplorer(visiblePath) }
            ))
            .East(viewButton)
            .start()
        else
          openExplorerWindow(rootPath)
    }

    lazy val openButton = TextButton("Open", hint=Hint(0.8, "Start a new view of\nthe selected directory\nor of the current directory\nif none is selected")){
      _ =>
        visibleSelection match {
          case paths if paths.length != 1 => visibleViewer.services.openServices(visibleViewer.folder.path)
          case paths if paths.length == 1 => visibleViewer.services.openServices(paths.head)
        }
    }

    lazy val shelfButton = MenuGlyphButton(ViewerWindow.SHELF, hint = Hint(2, "Selection to (s)helf\n... marked for copying (^C)\n... marked for deletion(^X)")) {
      _ => visibleActions.shelf(forCut = false)
    }

    lazy val clearButton = MenuGlyphButton(ViewerWindow.CLEARSHELF, hint = Hint(2, if (Shelf.nonEmpty && Shelf.forCut) "Clear shelf deletion mark" else "Clear shelf completely", constant = false)) {
      _ =>
        if(Shelf.nonEmpty && Shelf.forCut) Shelf.forCut = false else visibleActions.clearShelf()
        visibleActions.view.reDraw()
    }

    lazy val shelfButtons = NaturalSize.Row(align=Mid)(clearButton, shelfButton)

    def shelfHint(caption: () => String): Hint = Hint.ofGlyph(4, Shelf.hintGlyph(caption()), constant = false, preferredLocation = Hint.NoPreference)

    lazy val trashButton = TextButton("trash", hint=shelfHint(()=>"Move files to .Trash")){
      _ => visibleActions.trash()
    }

    lazy val copyButton = TextButton("copy", hint=shelfHint(()=>s"Copy files to ${visibleActions.currentImplicitDestinationString} (C-V)")){
      _ => visibleActions.paste()
    }

    lazy val moveButton = TextButton("move", hint=shelfHint(()=>s"Move files to ${visibleActions.currentImplicitDestinationString}")){
      _ => visibleActions.move()
    }

    lazy val linkButton = TextButton("link", hint=shelfHint(()=>s"Link files to ${visibleActions.currentImplicitDestinationString}")){
      _ => visibleActions.link()
    }

    lazy val symlinkButton = TextButton("ln-s", hint=shelfHint(()=>s"Symbolically link files to ${visibleActions.currentImplicitDestinationString}")){
      _ => visibleActions.symlink()
    }



    val perimeter: Scalar = 10

    NaturalSize.Col(align = Center)(
      FixedSize.Row(width = viewers.w, align = Mid)(
        openButton,
        viewButton,
        fileSheet.hFill(stretch = 1),
        shelfButtons,
        fileSheet.hFill(stretch = 1),
        symlinkButton, linkButton,copyButton, moveButton, trashButton,
        fileSheet.hFill(stretch = 2),
        HelpGUI.button(Viewer.fileSheet),
        ),
      fileSheet.vSpace(),
      viewers
      ).enlarged(perimeter)

  }

  val serial: Int = ViewerWindow.nextSerial()

  override val toString: String = viewers.keys
    .map(_.toString)
    .mkString(s"ViewerWindow#$serial(", " ", ")")

  def closeExplorer(path: Path): Unit = {
    if (viewers.size > 1) {
      viewers(path).close()
      viewers.selectPrev(path)
      viewers.remove(path)
    }
  }

  def nextExplorer(path: Path): Unit = {
    if (viewers.size > 1) {
      viewers.selectNext(path)
    }
  }

  def prevExplorer(path: Path): Unit = {
    if (viewers.size > 1) {
      viewers.selectPrev(path)
    }
  }

  def openServices(path: Path): Unit = {
    import PathProperties._
    assert(path.isReadable, s"$path is not readable")
    val collection = new ViewerWindow(path)
    val window = styled.windowdialogues.Dialogue
      .FLASH(collection.GUI, title=path.abbreviatedString())(fileSheet)
      .NorthEast(GUI)
      .withAutoScale()
    window.andThen(
      floating = false,
      onClose = { _ =>
        collection.close()
        Viewer.fine(s"Closed $collection")
      }
    )
  }

  def openExplorerWindow(path: Path): Unit = {
    if (viewers.isDefinedAt(path)) {
      viewers.select(path)
    } else {
      val folder = Folder(path)
      val explorer = new Viewer(folder, thisViewerWindow)
      viewers.add(path, explorer)
      viewers.select(path)
    }
    viewers.selected.giveupFocus()
    viewers.selected.setTitle(path)
    viewers.select(path)
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
