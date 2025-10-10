package org.sufrin.glyph
package tests.barents


import cached._
import files.{Folder, Shelf}
import styled._
import tests.barents.PathProperties._
import tests.barents.Viewer.{dialogueLabel, dialogueSheet, fileSheet, openOrdinaryFile}
import unstyled.dynamic.Keyed
import GlyphTypes.Scalar

import java.awt.Desktop
import java.nio.file._


object ViewerWindow {
  private var _serial = 0
  def nextSerial(): Int = { _serial += 1; _serial }
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

    lazy val shelfButton = TextButton("Shelf", hint = Hint(2, "Selection to (s)helf\n... marked for copying (^C)\n... marked for deletion(^X)")) {
      _ => visibleActions.shelf(forCut = false)
    }

    lazy val clearButton = TextButton("Clear", hint = Hint(2, if (Shelf.nonEmpty && Shelf.forCut) "Clear shelf deletion mark" else "Clear shelf completely", constant = false)) {
      _ =>
        if(Shelf.nonEmpty && Shelf.forCut) Shelf.forCut = false else visibleActions.clearShelf()
        visibleActions.view.reDraw()
    }

    lazy val shelfButtons = NaturalSize.Row(shelfButton, clearButton)

    def shelfHint(caption: () => String): Hint = Hint.ofGlyph(4, Shelf.hintGlyph(caption()), constant = false, preferredLocation = Hint.NoPreference)

    lazy val deleteButton = TextButton("del", hint=shelfHint(()=>"Delete files")){
      _ => visibleActions.delete()
    }

    lazy val copyButton = TextButton("cp", hint=shelfHint(()=>s"Copy files to ${visibleActions.currentImplicitDestinationString} (C-V)")){
      _ => visibleActions.paste()
    }

    lazy val moveButton = TextButton("mv", hint=shelfHint(()=>s"Move files to ${visibleActions.currentImplicitDestinationString}")){
      _ => visibleActions.move()
    }

    lazy val linkButton = TextButton("ln", hint=shelfHint(()=>s"Link files to ${visibleActions.currentImplicitDestinationString}")){
      _ => visibleActions.link()
    }



    val perimeter: Scalar = 10

    NaturalSize.Col(align = Center)(
      FixedSize.Row(width = viewers.w, align = Mid)(
        openButton,
        viewButton,
        fileSheet.hFill(),
        shelfButtons, copyButton, linkButton, moveButton, deleteButton,
        fileSheet.hFill(),
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
