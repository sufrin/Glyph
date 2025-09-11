package org.sufrin.glyph
package tests

import files.{FileAttributes, Folder}
import styled.{CaptionedCheckBox, Label}
import tests.PathProperties._
import Modifiers.Bitmap
import gesture.Gesture

import java.io.File
import java.nio.file.{FileSystems, LinkOption, Path}
import java.nio.file.Files.readAttributes
import java.nio.file.attribute.PosixFileAttributes
import java.util.Date

class Explorer(folder: Folder)(implicit val fileSheet: StyleSheet)  {
  import styled.TextButton

  val (dirs, files) = folder.splitDirs.value

  var showingInvisibles = true
  def thePaths: Seq[Path] = if (showingInvisibles) (dirs ++ files) else (dirs++files).filter(_.isVisible)

  def theListing: Seq[String] = for {path <- thePaths} yield {
    import FileAttributes._

    val attrs = readAttributes(path, classOf[PosixFileAttributes], LinkOption.NOFOLLOW_LINKS)
    val name = {
      val name = path.getFileName.toString
      if (name.length<=30) name else {
        val prefix = name.take(20)
        val suffix = name.drop(name.length-7)
        s"$prefix...$suffix"
      }
    }
    val legible = legibleAttributes(attrs)
    import legible._

    import java.nio.file.attribute.FileTime.fromMillis
    val modMillis = lastModifiedTime.toMillis
    val d = new Date(modMillis)

    f"$name%-30s $dmode%-10s $owner%s $size%10d ${d.toString}%s "
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
    override def onClick(mods: Bitmap, selected: Int): Unit = Explorer.open(thePaths(selected))
    override def onKeystroke(keystroke: Gesture): Unit = GUI.guiRoot.close()
  }

  val invisibilityButton = CaptionedCheckBox("Showing invisible", showingInvisibles){
    state =>
      showingInvisibles = state
      view.refresh(theListing)
  }


  val prefixDirs = folder.prefixPaths.toSeq.scanLeft(Path.of("/"))((b, p)=>b.resolve(p))
  val buttons = prefixDirs.tail.map {
    case path => TextButton(s"/${path.getFileName.toString}"){
      _ => Explorer.open(path)
    }.framed()
  }

  lazy val GUI: Glyph = NaturalSize.Col()(
    FixedSize.Row(width=view.w)(NaturalSize.Row(buttons), fileSheet.hFill(), invisibilityButton),
    view).enlarged(15, fg=Brushes.lightGrey)
}

object Explorer extends Application {
  import GlyphTypes.FontStyle.NORMAL

  val dirSheet: StyleSheet = StyleSheet(buttonFontSize = 18, labelFontSize = 18,  labelForegroundBrush= Brushes.black)
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
    def isHidden: Boolean  = java.nio.file.Files.isHidden(path)
    def isVisible: Boolean = !isHidden
    def isDir: Boolean     = java.nio.file.Files.isDirectory(path)
    def toFile: File       = path.toFile
    def isReadable: Boolean= path.toFile.canRead
  }
}

