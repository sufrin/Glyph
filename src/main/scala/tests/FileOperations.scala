package org.sufrin.glyph
package tests

import tests.PathProperties._

object FileOperations {
  import org.sufrin.glyph.files._

  import java.nio.file._

  def copy(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.copy(
          from,
          to.resolve(from.getFileName),
          StandardCopyOption.COPY_ATTRIBUTES
        )
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
    if (to.isDir) from.flatMap { case path => copy(path, to) }
    else Seq.empty
  }

  def move(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.move(
          from,
          to.resolve(from.getFileName)
        )
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

  def move(from: Seq[Path], to: Path): Seq[Exception] = {
    if (to.isDir) from.flatMap { case path => move(path, to) }
    else Seq.empty
  }

  def link(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.createLink(to.resolve(from.getFileName), from)
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
    if (to.isDir) from.flatMap { case path => link(path, to) }
    else Seq.empty
  }

  def symboliclink(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.createSymbolicLink(to.resolve(from.getFileName), from)
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
    if (to.isDir) from.flatMap { case path => symboliclink(path, to) }
    else Seq.empty
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

}
