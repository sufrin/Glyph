package org.sufrin.glyph
package tests.explorer
import files.{FileAttributes, Folder, Shelf}
import unstyled.dynamic.SeqViewer

import java.nio.file.Path

trait ActionProvider {
  this: Object =>

  val view:               SeqViewer
  val folder:             Folder
  def selectedPaths:      Seq[Path]
  val GUI:                Glyph
  def theRows:            Seq[FileAttributes.Row]
  def popupErrors(errors: Seq[Exception]): Unit


  def delete(): Unit = delete(Shelf.paths)

  def delete(paths: Seq[Path]): Unit = {
    Explorer.finer(s"Delete: ${paths.mkString(" ")}")
    popupErrors(paths.flatMap(FileOperations.delete(_)))
  }


  def currentImplicitDestination: Option[Path] =
    view.selectedRows.length match {
      case 0 => Some(folder.path)
      case 1 if theRows(0).isDirectory =>
        Some(theRows(view.selectedRows.head).path)
      case _ =>
        view.bell.play()
        None
    }

  def currentImplicitDestinationString: String =
    currentImplicitDestination match {
      case None       => "(destination is ambiguous)"
      case Some(path) => path.toString
    }


  /** selected paths to the shelf */
  def clearShelf(): Unit = Shelf.clear()

  def shelf(forCut: Boolean): Unit = {
    shelf(selectedPaths)
    Shelf.forCut = forCut
  }

  def shelf(paths: Seq[Path]): Unit = {
    Explorer.finer(s"Shelf =  ${paths.mkString(" ")}")
    Shelf.clear()
    Shelf.add(paths)
    view.clearSelection()
  }

  def unShelf(): Unit = unShelf(selectedPaths)

  def unShelf(paths: Seq[Path]): Unit = {
    Explorer.finer(s"Shelf -=  ${paths.mkString(" ")}")
    Shelf.remove(paths)
    view.clearSelection()
  }

  // implicit source: the clipboard/shelf
  def pasteTo(path: Path): Seq[Exception] = {
    Explorer.finer(s"Paste: ${Shelf.paths.mkString(" ")} to ${path}")
    FileOperations.copy(Shelf.paths, path)
  }

  def paste(): Unit = {
    currentImplicitDestination match {
      case None =>
      case Some(destination) =>
        val errors = pasteTo(destination)
        if (errors.nonEmpty) popupErrors(errors)
        else if (Shelf.forCut) delete()
    }
    clearShelf()
    //folder.withValidCaches { view.reDraw() }  // Notification now does this
  }


  // implicit source: the shelf
  def move(path: Path): Unit = {
    Explorer.finer(s"Move: ${Shelf.paths.mkString(" ")}  to ${path}")
    popupErrors(FileOperations.move(Shelf.paths, path))
    Shelf.clear()
    //folder.withValidCaches { view.reDraw() }  // Notification now does this
  }

  def move(): Unit = currentImplicitDestination.foreach(move)

  // implicit source: the shelf
  // implicit source: the shelf
  def link(path: Path): Unit = {
    Explorer.finer(s"Link: ${Shelf.paths.mkString(" ")}  to ${path}")
    popupErrors(FileOperations.link(Shelf.paths, path))
    Shelf.clear()
    //folder.withValidCaches { view.reDraw() } // Notification now does this
  }

  def link(): Unit = currentImplicitDestination.foreach(link)
}
