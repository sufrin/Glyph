package org.sufrin.glyph
package tests.barents
import files.{FileAttributes, Folder, Shelf}
import unstyled.dynamic.SeqViewer

import java.nio.file.Path

trait ActionProvider {
  this: Object =>

  val viewer:             SeqViewer
  val folder:             Folder
  def selectedPaths:      Seq[Path]
  val GUI:                Glyph
  def theRows:            Seq[FileAttributes.Row]
  def popupErrors(errors: Seq[Exception]): Unit


  def delete(): Unit = delete(Shelf.paths)

  def delete(paths: Seq[Path]): Unit = {
    Viewer.finer(s"Delete: ${paths.mkString(" ")}")
    popupErrors(paths.flatMap(FileOperations.delete(_)))
  }

  def trash(): Unit = trash(Shelf.paths)

  def trash(paths: Seq[Path]): Unit = {
    Viewer.finer(s"Trash: ${paths.mkString(" ")}")
    popupErrors(paths.flatMap(FileOperations.trash(_)))
  }

  def currentImplicitDestination: Option[Path] =
    viewer.selectedRows.length match {
      case 0 => Some(folder.path)
      case 1 if theRows(0).isDirectory =>
        Some(theRows(viewer.selectedRows.head).path)
      case _ =>
        viewer.bell.play()
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
    Viewer.finer(s"Shelf =  ${paths.mkString(" ")}")
    Shelf.clear()
    Shelf.add(paths)
    viewer.clearSelection()
  }

  def unShelf(): Unit = unShelf(selectedPaths)

  def unShelf(paths: Seq[Path]): Unit = {
    Viewer.finer(s"Shelf -=  ${paths.mkString(" ")}")
    Shelf.remove(paths)
    viewer.clearSelection()
  }

  // implicit source: the clipboard/shelf
  def pasteTo(path: Path): Seq[Exception] = {
    Viewer.finer(s"Paste: ${Shelf.paths.mkString(" ")} to ${path}")
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
    //folder.withValidCaches { viewer.reDraw() }  // Notification now does this
  }

  // implicit source: the shelf
  def move(path: Path): Unit = {
    Viewer.finer(s"Move: ${Shelf.paths.mkString(" ")}  to ${path}")
    popupErrors(FileOperations.move(Shelf.paths, path))
    Shelf.clear()
    //folder.withValidCaches { viewer.reDraw() }  // Notification now does this
  }

  def move(): Unit = currentImplicitDestination.foreach(move)

  // implicit source: the shelf
  def link(path: Path): Unit = {
    Viewer.finer(s"Link: ${Shelf.paths.mkString(" ")}  to ${path}")
    popupErrors(FileOperations.link(Shelf.paths, path))
    Shelf.clear()
    //folder.withValidCaches { viewer.reDraw() } // Notification now does this
  }

  def link(): Unit = currentImplicitDestination.foreach(link)

  // implicit source: the shelf
  def symlink(path: Path): Unit = {
    Viewer.finer(s"Symlink: ${Shelf.paths.mkString(" ")}  to ${path}")
    popupErrors(FileOperations.symboliclink(Shelf.paths, path))
    Shelf.clear()
    //folder.withValidCaches { viewer.reDraw() } // Notification now does this
  }

  def symlink(): Unit = currentImplicitDestination.foreach(symlink)

}
