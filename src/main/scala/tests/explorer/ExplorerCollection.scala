package org.sufrin.glyph
package tests.explorer


import cached._
import files.{Folder, Shelf}
import styled._
import tests.explorer.Explorer.{dialogueLabel, dialogueSheet, fileSheet, openOrdinaryFile}
import tests.explorer.PathProperties._
import unstyled.dynamic.Keyed
import GlyphTypes.Scalar

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

  def close(): Unit = explorers.values.foreach(_.close())

  def visibleExplorer:  Explorer         = explorers.selected
  def visibleProvider:  ExplorerServices = visibleExplorer.provider // usually = this
  def visibleFolder:    Folder           = visibleExplorer.folder
  def visiblePath:      Path             = visibleExplorer.folder.path
  def visibleSelection: Seq[Path]        = visibleExplorer.selectedPaths
  def visibleActions:   ActionProvider   = visibleExplorer

  lazy val GUI: Glyph = {
    def menuHint = Hint(2, explorers.keys.map(_.toString).mkString(s"Views\n", "\n", ""), constant=false )

    lazy val viewButton: Glyph = TextButton("View", hint = menuHint) {
      _ =>
        if (explorers.size > 1)
          styled.windowdialogues
            .Menu(explorers.keys.toSeq.map { key =>
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


    lazy val openButton = TextButton("Open", hint=Hint(2, "Start a new view of\nthe selected directory\nor of the current directory\nif none is selected")){
      _ =>
        visibleSelection match {
          case paths if paths.length != 1 => visibleExplorer.provider.openServices(visibleExplorer.folder.path)
          case paths if paths.length == 1 => visibleExplorer.provider.openServices(paths.head)
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

    def shelfHint(caption: () => String): Hint = Hint.ofGlyph(4, Shelf.hintGlyph(caption()), constant = false, preferredLocation = Some(Vec(15,15)))

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
      FixedSize.Row(width = explorers.w, align = Mid)(
        openButton,
        viewButton,
        fileSheet.hFill(),
        shelfButtons, copyButton, linkButton, moveButton, deleteButton,
        fileSheet.hFill(),
        HelpGUI.button(Explorer.fileSheet),
      ),
      fileSheet.vSpace(),
      explorers
    ).enlarged(perimeter)

  }

  val serial: Int = ExplorerCollection.nextSerial()

  override val toString: String = explorers.keys
    .map(_.toString)
    .mkString(s"ExplorerCollection#$serial(", " ", ")")

  def closeExplorer(path: Path): Unit = {
    if (explorers.size > 1) {
      explorers(path).close()
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
    val collection = new ExplorerCollection(path)
    val window = styled.windowdialogues.Dialogue
      .FLASH(collection.GUI)(fileSheet)
      .NorthEast(GUI)
      .withAutoScale()
    window.andThen(
      floating = false,
      onClose = { _ =>
        collection.close()
        Explorer.fine(s"Closed $collection")
      }
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
