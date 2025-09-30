package org.sufrin
package glyph
package styled.windowdialogues

import io.github.humbleui.jwm.Screen
import org.sufrin.glyph.unstyled.reactive.GenericButton
import org.sufrin.logging.Loggable

/**
 *   A running dialogue is a glyph-tree contained in its own top-level window, and
 *   responding to user events at that window.
 *
 *   Pressing its system close button causes the dialogue to invoke its `close()`
 *   method, and to invoke the continuation (if any) with which it was
 *   started with parameter `null`.
 *
 *   Several kinds of GUI components can be implemented as Popups, including
 *   status/error reports, etc.
 *
 *   TODO: pre-select one of the bottom-row as the meaning of [ENTER]
 *
 */

import org.sufrin.glyph.Location._

object Dialogue extends Loggable {

  /** A dialogue whose gui consists of `blurb` atop a bottom row consisting of buttons built from glyphs. */
  //def apply[T](blurb: Glyph, bottomRow: Seq[Glyph], position: Location=null, title: String = ""): Dialogue[T] = {
  //  new Dialogue[T](blurb, bottomRow, position, title)
  //}


  def FLASH(content: Glyph, position: Location=null, title: String="")(implicit sheet: StyleSheet): Dialogue[Unit] = {
    val bg = sheet.popupBackgroundBrush
    new Dialogue[Unit](content, Seq.empty, position, title, bg=bg, preferred = -1)
  }

  def BORDERLESS(content: Glyph, position: Location=null, title: String="")(implicit sheet: StyleSheet): Menu = {
    val bg = sheet.popupBackgroundBrush
    styled.windowdialogues.Menu.at(position)(content)
  }

  def OK(blurb: Glyph, position: Location=null, title: String="")(implicit sheet: StyleSheet): Dialogue[Unit] = {
    val bg = sheet.popupBackgroundBrush
    // Mutual references ok<->popup
    lazy val ok: Glyph = styled.TextButton("OK") {
      _ => popup.close()
    }
    lazy val popup: Dialogue[Unit] = new Dialogue[Unit](blurb, List(ok), position, title, bg=bg)
    popup
  }

  def OKNO(blurb: Glyph, position: Location=null, title: String = "", ok: String = " OK ", no: String=" NO ")(implicit sheet: StyleSheet): Dialogue[Boolean] = {
    val bg = sheet.popupBackgroundBrush
    // Mutual references ok<->popup, no<->popup
    lazy val okButton: Glyph = styled.TextButton(ok) {
      _ => popup.close(true)
    }
    lazy val noButton: Glyph = styled.TextButton(no) {
      _ => popup.close(false)
    }
    lazy val popup: Dialogue[Boolean] = new Dialogue[Boolean](blurb, List(okButton, noButton), position, title, bg=bg)
    popup
  }

  def POPUP(blurb: Glyph, position: Location=null, title: String = "")(choices: String*)(implicit sheet: StyleSheet): Dialogue[String] = {
    val bg = sheet.popupBackgroundBrush
    lazy val buttons = choices.map {
      choice => styled.TextButton(choice) { _ => popup.close(choice) }
    }
    lazy val popup: Dialogue[String] = new Dialogue[String](blurb, buttons, position, title, bg=bg)
    popup
  }

}

class Dialogue[T](blurb:        Glyph,
                  buttons:      Seq[Glyph],
                  var location: Location,
                  theTitle:     String,
                  bg:            Brush,
                  var preferred: Int = 0
                 )(implicit sheet: StyleSheet) { thisPopup =>

  val maxPreferred = buttons.length
  /**
   * Make a primitive popup from `blurb` atop `bottomRow`; placing it at `location` on the screen.
   */

  import GlyphTypes.Pixels
  import NaturalSize.{Col, Row}
  import io.github.humbleui.jwm.App

  val closeResult = Variable[Option[T]](None)

  import styled.overlaydialogues.NavigationManager

  val navigation = new NavigationManager(buttons, preferred, nested=false, menu=false)(close())

  /** Close this popup, setting `closeResult` to `Some(result)` */
  def close(result: T=null.asInstanceOf[T]): Unit =
    App.runOnUIThread
    { () =>
      running = None
      closeResult.value = Some(result)
      blurb.guiRoot match {
        case root: RootGlyph =>
          root.close() //**!
          _onClose match {
            case None =>
            case Some(onClose) =>
              //App.runOnUIThread(() => onClose(result))
              //App.runOnUIThread(() => location.reDraw())
              onClose(result)
              location.reDraw()
          }
        case _ =>
      }
    }

  @inline def theBottomRow = Row(align=Mid, bg=bg)(buttons)

  val GUI = if (buttons.isEmpty) Col(align=Center, bg=bg)(blurb) else  Col(align=Center, bg=bg)(blurb, theBottomRow)

  var running: Option[Interaction] = None

  def North(glyph: Glyph): this.type = {
    location = Location.NorthFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def NorthEast(glyph: Glyph): this.type = {
    location = Location.NorthEastFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def East(glyph: Glyph): this.type = {
    location = Location.EastFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def SouthEast(glyph: Glyph): this.type = {
    location = Location.SouthEast(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def South(glyph: Glyph): this.type = {
    location = Location.SouthFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def SouthWest(glyph: Glyph): this.type = {
    location = Location.SouthWestFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def West(glyph: Glyph): this.type = {
    location = Location.WestFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def NorthWest(glyph: Glyph): this.type = {
    location = Location.NorthWestFor(GUI)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to the root of `glyph` */
  def InFront(glyph: Glyph): this.type = {
    val loc = glyph.rootDistance
    location = Location.OnRootOf(glyph)(loc.x + (glyph.w - GUI.diagonal.x) / 2f, loc.y + (glyph.h - GUI.diagonal.y) / 2f)
    thisPopup
  }

  def OnRootOf(glyph: Glyph, loc: Vec=Vec.Zero): this.type = {
    location = Location.OnRootOf(glyph)(loc.x, loc.y)
    thisPopup
  }

  def show(): Unit =
    if (running.isEmpty) start()

  /** Allocate the window and start the interaction */
  def start(logEvents: Boolean = false, floating: Boolean = true): Unit = {
    App.runOnUIThread {
      () =>
        val theInteraction = new Interaction(App.makeWindow(), GUI, location.softwareScale) {

          import io.github.humbleui.jwm.{EventKey, Window}

          override def inset: Pixels = (0, 0)

          override val title: String = theTitle

          override def position(window: Window): Pixels = {
            val (cx, cy): Pixels = thisPopup.location.contentLocation
            val (rx, ry): Pixels = Vec.scaleToPixels(location.effectiveScale, location.logicalLocation)
            val result = (rx+cx, ry+cy)
            //println(s"Dialogue.position (${location.softwareScale})(${location.logicalLocation})(${location.contentLocation}) =$result")
            result
          }

          override def size: Pixels = {
            // theRoot.diagonal.scaled(location.effectiveScale).toPair
            val px = Vec.scaleToPixels(location.effectiveScale, GUI.diagonal.x, GUI.diagonal.y)
            Dialogue.finest(s"Dialogue size:$px")
            px
          }

          override def screen: Screen = location.screen

          def onKeyboardUnfocussedX(key: EventKey): Unit = {
            import io.github.humbleui.jwm.Key._
            var drag = true
            key.getKey match {
              case ESCAPE  if !key.isPressed => drag=false; navigation.Action(false)
              case ENTER   if !key.isPressed => drag=false; navigation.Action(true)
              case TAB     if !key.isPressed => drag=false; navigation.Next()
              case other =>
                // beep()
            }
          }

          locally {
            import io.github.humbleui.jwm.{WindowMac, ZOrder}
            window match {
              case mac: WindowMac =>
                if (floating) mac.setZOrder(ZOrder.MODAL_PANEL) // Floats above a MAIN_MENU
              case _ =>
                window.setZOrder(ZOrder.FLOATING)
            }
          }
          window.bringToFront()
          window.focus()
        }
        theInteraction.handler.logEvents = logEvents; theInteraction.start()
        // Henceforth turn native close requests into calls of close()
        GUI.guiRoot.onCloseRequest(_ => close())
        running = Some(theInteraction)
        // mark preferred
        if (preferred>=0) buttons(preferred) match {
          case button: GenericButton => button.setHover(true)
          case _ =>
        }
    }
  }

  private var _onClose: Option[T=>Unit] = None

  def onClose(onClose: T => Unit): Dialogue[T] = {
    _onClose = Some(onClose)
    this
  }

  /**  Start the popup window, invoking `onClose` when it's closed with a result */
  def andThen(onClose: T => Unit, floating: Boolean = true): Unit = {
    _onClose = Some(onClose)
    start(floating=floating)
  }

}
