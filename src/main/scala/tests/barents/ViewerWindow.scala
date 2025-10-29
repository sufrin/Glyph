package org.sufrin.glyph
package tests.barents


import cached._
import files.{Folder, Shelf}
import styled._
import tests.barents.PathProperties._
import tests.barents.Viewer.{dialogueLabel, dialogueSheet, fileSheet, openOrdinaryFile}
import unstyled.dynamic.Keyed
import GlyphTypes.Scalar
import Hint.West

import io.github.humbleui.skija.PaintMode

import java.awt.Desktop
import java.nio.file._


object ViewerWindow {
  private var _serial = 0
  def nextSerial(): Int = { _serial += 1; _serial }

  lazy val SHELF: Glyph = {
    import Brushes.{blue => B, darkGrey => G, red => R, transparent => T}
    import GlyphShape._
    val box: Brush=>GlyphShape = rect(5.25f, 3)
    val gap:       GlyphShape  = rect(18, 3)(T)
    val gapB:      GlyphShape  = rect(18, 1.5f)(T)
    val plank0B:   GlyphShape  = rect(18, 3)(G) // no boxes
    val plank1B:   GlyphShape  = ((rect(1.5f, 3)(T) ||| box(R))                       --- plank0B) // 1 box
    val plank2B:   GlyphShape  = ((rect(0.75f,3)(T) ||| box(R(width=1, mode=PaintMode.STROKE)) ||| box(T) ||| box(B)) --- plank0B) // 2 boxes
    val upright:   GlyphShape  = rect(3, 21)(G)

    val image: GlyphShape = upright ||| (plank1B --- gapB --- plank2B --- gap --- plank0B) ||| upright
      image.asGlyph.withBaseline(20)
  }

  lazy val CLEARSHELF: Glyph = {
    import GlyphShape._
    superimposed(List(SHELF, GlyphShape.line(SHELF.diagonal.scaled(-1, -1), SHELF.diagonal)(Brushes.redLine))).asGlyph
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
  def viewerCount:      Int              = viewers.size

  lazy val GUI: Glyph = {
    val viewHint = Hint(0.75, if (viewers.size<=1) "[There are no other views in this viewer]" else s"Choose another view (${viewers.size-1})", constant=false)(Barents.hintSheet)

    lazy val viewButton: Glyph = TextButton("View", hint = viewHint) {
      _ =>
        if (viewers.size > 1) {
          val buttons = viewers.keys.toSeq.map {
                key => UniformSize(key.toString) { _ => openExplorerWindow(key) }
              }
          styled.windowdialogues
            .Menu(UniformSize.constrained(buttons), bg=Barents.menuBackgroundBrush)
            .East(viewButton)
            .start()
        } else
          openExplorerWindow(rootPath)
    }

    lazy val openButton = TextButton("New Viewer", hint=Hint(0.8, "Start a new viewer of\nthe selected directory\nor of the current directory\nif none is selected")(Barents.hintSheet)){
      _ =>
        visibleSelection match {
          case paths if paths.length != 1 => visibleViewer.services.openServices(visibleViewer.folder.path)
          case paths if paths.length == 1 => visibleViewer.services.openServices(paths.head)
        }
    }

    lazy val shelfButton = MenuGlyphButton(ViewerWindow.SHELF, hint = Hint(2, "Selection to (s)helf\n... B2=>marked for deletion")(Barents.hintSheet)) {
      case mods  =>
        visibleActions.shelf(forCut = mods.are(Modifiers.Secondary|Modifiers.Released))
    }

    lazy val clearButton = MenuGlyphButton(ViewerWindow.CLEARSHELF, hint = Hint(2, if (Shelf.nonEmpty && Shelf.forCut) "Clear shelf deletion mark" else "Clear shelf completely", constant = false)(Barents.hintSheet)) {
      _ =>
        if(Shelf.nonEmpty && Shelf.forCut) Shelf.forCut = false else visibleActions.clearShelf()
        visibleActions.viewer.reDraw()
    }

    lazy val shelfButtons = NaturalSize.Row(align=Mid)(clearButton, shelfButton)

    def shelfHint(caption: () => String): Hint = Hint.ofGlyph(4, Shelf.hintGlyph(caption())(Barents.hintSheet), constant = false, preferredLocation = Hint.West)(Barents.hintSheet)

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

    lazy val terminalButton =  TextButton("> _", hint=Hint(0.85, s"Open a terminal in ${visibleActions.currentImplicitDestinationString}", preferredLocation = West)){
      _ => visibleActions.openTerminal()
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
        terminalButton,
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
