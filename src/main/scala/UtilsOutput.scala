package org.sufrin.glyph
package utils
import NaturalSize.Col
import unstyled.reactive
import unstyled.reactive.RawButton

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Output
{
  def output(dir: String, path: String)(glyph: Glyph): Option[String] = {
    try { External.writeGlyph(glyph, s"${dir}/$path", scale = 0.5f); None }
    catch {
      case exn: Exception => Some(s"Failed to write ${dir}/$path")
    }
  }

  def stringOfDate(date: LocalDateTime = LocalDateTime.now()) = {
    date.format(DateTimeFormatter.ofPattern("dd-MM-yyyy@HHmmss"))
  }

  /**
   *  Return a Glyph for use as a GUI. It consists of the given `gui` beneath a thin grey bar that when clicked
   *  invites the user to save the current appearance of the GUI in a (timestamped) `.png` file.
   *
   *  If non-null, the  `hint` shows for 5 seconds whenever the cursor hovers over the thin bar.
   *
   *  Useful for generating documentation images.
   */

  def withWriteBar(folder: String="SAVED", hint: Glyph=null, enabled: => Boolean)(gui: Glyph)(implicit style: StyleSheet): Glyph = {
    import unstyled.static._
    implicit object Style extends StyleSheet
    val r = FilledRect(gui.w - 5, 6f, fg = Brushes.lightGrey)
    lazy val writeBar: RawButton = reactive.RawButton(r(), r(), r()) {
      _ =>
        val fileName = stringOfDate() + ".png"
        styled.windowdialogues.Dialogue.OKNO(styled.Label(s"Write image to ${folder}/${fileName}").enlarged(15)).InFront(writeBar).andThen {
          case false =>
          case true =>
            output(folder, fileName)(gui.guiRoot) match {
              case None => ()
              case Some(error) =>
                styled.windowdialogues.Dialogue.OK(Label(error).enlarged(20f)).InFront(writeBar).start()
            }
        }
    }
    if (hint ne null) HintManager.ofGlyph(target=writeBar, 5.0, ()=>hint, constant = true).onlyWhen(enabled)
    Col(align=Center)(
      writeBar.asInstanceOf[Glyph],
      gui
    )
  }
}

