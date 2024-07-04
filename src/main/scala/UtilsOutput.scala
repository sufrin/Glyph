package org.sufrin.glyph
package utils
import NaturalSize.Col

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Output
{
  def output(dir: String, path: String)(glyph: Glyph): Option[String] = {
    try { External.renderBitmap(glyph, s"${dir}/$path", scale = 0.5f); None }
    catch {
      case exn: Exception => Some(s"Failed to write ${dir}/$path")
    }
  }

  def stringOfDate(date: LocalDateTime = LocalDateTime.now()) = {
    date.format(DateTimeFormatter.ofPattern("dd-MM-yyyy@HHmmss"))
  }

  def withWriteBar(folder: String="SAVEDGUI")(gui: Glyph): Glyph = {
    import Glyphs._
    implicit object Style extends Styles.DefaultSheet
    val r = FilledRect(gui.w - 5, 6f, fg = lightGrey)
    lazy val writeBar: Glyph = ReactiveGlyphs.RawButton(r(), r(), r()) {
      _ =>
        val fileName = stringOfDate() + ".png"
        windowdialogues.Dialogue.OKNO(Label(s"Write image to ${folder}/${fileName}").enlarged(20f)).InFront(writeBar).andThen {
          case false =>
          case true =>
            output(folder, fileName)(gui.guiRoot) match {
              case None => ()
              case Some(error) =>
                windowdialogues.Dialogue.OK(Label(error).enlarged(20f)).InFront(writeBar).start()
            }
        }
    }
    Col.centered(
      writeBar,
      gui
    )
  }
}

