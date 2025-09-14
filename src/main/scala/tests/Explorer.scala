package org.sufrin.glyph
package tests

import files.{FileAttributes, Folder}
import styled.{CaptionedCheckBox, Label}
import tests.PathProperties._
import Modifiers.Bitmap
import cached._
import gesture.Gesture

import org.sufrin.logging.SourceLoggable

import java.io.File
import java.nio.file.{FileSystems, Path}
import java.util.Date


class Explorer(folder: Folder)(implicit val fileSheet: StyleSheet)  {
  import styled.TextButton

  var showingInvisibles = true

  locally {
    folder.onValidate{
      case event: String =>
        Explorer.finest(s"about to refresh after: $event")
        view.refresh(theListing)
        Explorer.finest(s"refreshed after: $event")
    }
  }

  def thePaths: Seq[Path] = {
    val dirs  = folder.dirs.value
    val files = folder.notDirs.value
    if (showingInvisibles) (dirs ++ files) else (dirs++files).filter(_.isVisible)
  }

  def theListing: CachedSeq[Path, String] = folder.withValidCaches {
    Explorer.finest(s"thePaths is ${thePaths.mkString(", ")}")
    CachedSeq(thePaths) {
      case path =>
        import FileAttributes._
        val key = folder.path.resolve(path)
        val map = folder.attributeMap.value
        if (map.isDefinedAt(key)) {
          val attrs = map(key)
          val name = {
            val name = path.getFileName.toString
            if (name.length <= 30) name else {
              val prefix = name.take(20)
              val suffix = name.drop(name.length - 7)
              s"$prefix...$suffix"
            }
          }
          val legible = legibleAttributes(attrs)
          import legible._

          import java.nio.file.attribute.FileTime.fromMillis
          val modMillis = lastModifiedTime.toMillis
          val d = new Date(modMillis)

          (f"$name%-30s $dmode%-10s $owner%s $size%8s ${d.toString}%s ")
        } else {
          path.getFileName.toString
        }
    }
  }

  def theSortedListing = {
    val paths = thePaths
    CachedSeq[Int, String](0 until paths.length){
      case i: Int =>
        import FileAttributes._
        val map: Seq[Row] = folder.sortedRows
        val row: FileAttributes.Row = map(i)
        row.asString
    }
  }

  lazy val columns = theListing.prepended(folder.path.toString).map(_.length).max

  lazy val view = new unstyled.dynamic.SeqViewer(
                                columns, 30,
                                fileSheet.buttonFont,
                                fileSheet.buttonForegroundBrush,
                                fileSheet.buttonBackgroundBrush,
                                Brushes.red,
                                theListing)
  {

    override def onClick(mods: Bitmap, selected: Int): Unit = Explorer.open((folder.path.resolve(thePaths(selected))))

    override def onHover(mods: Bitmap, hovered: Int): Unit = folder.withValidCaches {
      if (thePaths.isDefinedAt(hovered)) {
          val key = folder.path.resolve(thePaths(hovered))
          val map = folder.attributeMap.value
          import FileAttributes._
          if (map.isDefinedAt(key)) {
            val attributes = map(key)
            println(f"$key%30s ${attributes.asString}%s")
          }
          else {
            println(f"$key%30s (deleted)")
          }
      }
    }

    override def onKeystroke(keystroke: Gesture): Unit = GUI.guiRoot.close()
  }

  locally{
    println(theSortedListing.mkString("\n"))
  }
  val invisibilityButton = CaptionedCheckBox("Showing invisible", showingInvisibles){
    state =>
      showingInvisibles = state
      view.refresh(theListing)
  }

  val validateButton = TextButton("X"){
    state =>
      folder.reValidate()
      view.refresh(theListing)
  }

  /**
   * `buttons` is a sequence of buttons; each corresponding to an ancestor of this
   * folder in the filestore, and labelled with its filename (not its path). Pressing a button
   * opens an explorer on the ancestor.
   */
  object Navigation {
    private lazy val prefixDirs = folder.prefixPaths.toSeq.scanLeft(Path.of("/"))((b, p) => b.resolve(p))
    private lazy val buttons: Seq[Glyph] = prefixDirs.tail.map {
      case path => TextButton(s"/${path.getFileName.toString}") { _ => Explorer.open(path) }.framed()
    }
    lazy val pathButtons: Glyph = NaturalSize.Row(Navigation.buttons)
  }

  lazy val GUI: Glyph =
    NaturalSize.Col()(
      FixedSize.Row(width=view.w)(Navigation.pathButtons, fileSheet.hSpace(), validateButton, fileSheet.hFill(), invisibilityButton),
      view
    ).enlarged(15, fg=Brushes.lightGrey)
}

object Explorer extends Application with SourceLoggable {
  import GlyphTypes.FontStyle.NORMAL

  implicit val fileSheet: StyleSheet = StyleSheet(buttonFontSize = 18, labelFontSize = 18, buttonFontStyle=NORMAL, labelForegroundBrush= Brushes.black)

  val root = new Folder(FileSystems.getDefault.getPath("/", "Users", "sufrin"))


  lazy val GUI: Glyph = new Explorer(root)(fileSheet).GUI

  def title: String = "Explore"

  def open(path: Path): Unit =
    if (path.isReadable) {
      if (path.isDir) {
        val folder = new Folder(path)
        styled.windowdialogues.Dialogue.FLASH(new Explorer(folder).GUI, title=path.toString).OnRootOf(GUI).start()
      } else {
        import java.awt.Desktop
        if (Desktop.isDesktopSupported) {
          val desktop = Desktop.getDesktop
          if (desktop.isSupported(Desktop.Action.OPEN))
            try desktop.open(path.toFile) // Opens with default application
            catch {
              case ex: java.io.IOException =>
                styled.windowdialogues.Dialogue.OK(Label(s"${ex.getMessage}")).InFront(GUI).start()
            }
          else
            styled.windowdialogues.Dialogue.OK(Label(s"Desktop open not supported")).InFront(GUI).start()
        }
        else
          styled.windowdialogues.Dialogue.OK(Label(s"Desktop not supported on this platform")).InFront(GUI).start()
      }
    } else {
      styled.windowdialogues.Dialogue.OK(Label(s"Not readable\n$path")).InFront(GUI).start()
    }

}

object PathProperties {
  implicit class PathProperty(val path: Path) extends AnyVal {
    def isHidden: Boolean  = path.getFileName.toString.charAt(0)=='.'//java.nio.file.Files.isHidden(path)
    def isVisible: Boolean = !isHidden
    def isDir: Boolean     = java.nio.file.Files.isDirectory(path)
    def toFile: File       = path.toFile
    def isReadable: Boolean= path.toFile.canRead
  }
}

