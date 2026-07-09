package org.sufrin.glyph
package tests
package barents

import tests.barents.PathProperties._

import org.sufrin.logging.SourceLoggable

import java.util.Date

object FileOperations extends SourceLoggable {
  import org.sufrin.glyph.files._

  import java.nio.file._

  def dirChanged(from: Path, to: Path): Unit = {
    finest(s"changed at: ${from.getParent} and: $to")
  }

  def dirChanged(to: Path): Unit = {
    finest(s"changed at: $to")
  }

  def copy(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.copy(
          from,
          to.resolve(from.getFileName),
          StandardCopyOption.COPY_ATTRIBUTES
        )
        dirChanged(to)
        Seq.empty
      } catch { case ex: java.io.IOException => List(ex) }
    } else
      List(
        new java.nio.file.NoSuchFileException(
          from.toString,
          "",
          " file is unreadable."
        )
      )
  }

  def copy(from: Seq[Path], to: Path): Seq[Exception] = {
    val errors =
      if (to.isDir) from.flatMap { case path => copy(path, to) }
      else List(new NotDirectoryException(to.toString))
    Folder.withFolderFor(to)(_.notifyChange())
    errors
  }

  def move(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.move(
          from,
          to.resolve(from.getFileName)
        )
        dirChanged(from, to)
        Seq.empty
      } catch {
        case ex: java.io.IOException => List(ex)
      }
    } else {
      List(
        new java.nio.file.NoSuchFileException(
          from.toString,
          "",
          " file is unreadable."
        )
      )
    }
  }

  def move(from: Seq[Path], to: Path): Seq[Exception] = {
    val errors =
      if (to.isDir)
        from.flatMap { case path => move(path, to) }
      else
        List(new NotDirectoryException(to.toString))
    Folder.withFolderFor(to)(_.notifyChange())

    val fromParents: Set[Path] = from.map(_.getParent).toSet
    fromParents.foreach { case source: Path => Folder.withFolderFor(source)(_.notifyChange()) }
    errors
  }

  def link(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.createLink(to.resolve(from.getFileName), from)
        dirChanged(to)
        Seq.empty
      } catch {
        case ex: java.io.IOException => List(ex)
      }
    } else
      List(
        new java.nio.file.NoSuchFileException(
          from.toString,
          "",
          " file is unreadable."
        )
      )
  }

  def link(from: Seq[Path], to: Path): Seq[Exception] = {
    val errors = if (to.isDir) from.flatMap { case path => link(path, to) }
    else List(new NotDirectoryException(to.toString))
    Folder.withFolderFor(to)(_.notifyChange())
    errors
  }

  def symboliclink(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.createSymbolicLink(to.resolve(from.getFileName), from)
        dirChanged(to)
        Seq.empty
      } catch {
        case ex: java.io.IOException => List(ex)
      }
    } else
      List(
        new java.nio.file.NoSuchFileException(
          from.toString,
          "",
          " file is unreadable."
        )
      )
  }

  def symboliclink(from: Seq[Path], to: Path): Seq[Exception] = {
    val errors = if (to.isDir) from.flatMap { case path => symboliclink(path, to) }
    else List(new NotDirectoryException(to.toString))
    Folder.withFolderFor(to)(_.notifyChange())
    errors
  }

  def createDirectory(path: Path): Seq[Exception] = {
    try {
      Files.createDirectory(path)
      Seq.empty
    } catch {
      case ex: FileAlreadyExistsException => Seq.empty
      case ex: java.io.IOException        => List(ex)
    }
  }

  val TRASH: Path = Paths.get(java.lang.System.getProperty("user.home"), ".Trash")

  /** Expunge */
  def delete(path: Path): Seq[Exception] =
    try {
      Files.delete(path)
      dirChanged(path.getParent)
      Folder.withFolderFor(path.getParent)(_.notifyChange())
      Seq.empty
    }
    catch {
      case ex: java.io.IOException        => List(ex)
    }


  def renamingMove(from: Path, to: Path): Seq[Exception] = {
    // pre: to is in a writeable folder
    if (from.isReadable) {
      try {
        Files.move(from,  to)
        Seq.empty
      } catch {
        case ex: java.io.IOException => List(ex)
      }
    } else {
      List(
        new java.nio.file.NoSuchFileException(
          from.toString,
          "",
          " file is unreadable."
          )
        )
    }
  }

  /** Really delete files already in the trash */
  def trash(path: Path): Seq[Exception] = {
      val localTrash = path.getParent.resolve(".TRASH")
      if (path.getParent == localTrash) {
        delete(path)
      } else
      createDirectory(localTrash).toList match {
        case Nil =>
          val timeStamped = Paths.get(path.getFileName.toString+"@"+new Date().toString)
          val stamped = localTrash.resolve(timeStamped)
          renamingMove(path, stamped) match {
            case Nil =>
              Folder.withFolderFor(path.getParent)(_.notifyChange())
              Nil
            case errors => errors
          }
        case errors =>
          errors
      }
  }

}
