package org.sufrin
package glyph
package unstyled.windowdialogues

import io.github.humbleui.jwm.Screen
import logging.Loggable

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

import Location._

object Dialogue extends Loggable {

  /** A dialogue whose gui consists of `blurb` atop a bottom row consisting of buttons built from glyphs. */
  def apply[T](blurb: Glyph, bottomRow: Seq[Glyph], position: Location=null, title: String = ""): Dialogue[T] = {
    new Dialogue[T](blurb, bottomRow, position, title)
  }

  def OK(blurb: Glyph, position: Location=null, title: String="")(implicit sheet: StyleSheet): Dialogue[Unit] = {
    // Mutual references ok<->popup
    lazy val ok: Glyph = styled.TextButton("OK") {
       _ => popup.close()
    }
    lazy val popup: Dialogue[Unit] = new Dialogue[Unit](blurb, List(ok), position, title)
    popup
  }

  def OKNO(blurb: Glyph, position: Location=null, title: String = "", ok: String = " OK ", no: String=" NO ")(implicit sheet: StyleSheet): Dialogue[Boolean] = {
    // Mutual references ok<->popup, no<->popup
    lazy val okButton: Glyph = styled.TextButton(ok) {
      _ => popup.close(true)
    }
    lazy val noButton: Glyph = styled.TextButton(no) {
      _ => popup.close(false)
    }
    lazy val popup: Dialogue[Boolean] = new Dialogue[Boolean](blurb, List(okButton, noButton), position, title)
    popup
  }

  def CHOOSE(blurb: Glyph, position: Location=null, title: String = "")(choices: String*)(implicit sheet: StyleSheet): Dialogue[String] = {
    lazy val buttons = choices.map {
        choice => styled.TextButton(choice) { _ => popup.close(choice) }
    }
    lazy val popup: Dialogue[String] = new Dialogue[String](blurb, buttons, position, title)
    popup
  }

}

class Dialogue[T](blurb: Glyph, bottomRow: Seq[Glyph], var location: Location, theTitle: String, bg: Brush=Brushes.nothing) { thisPopup =>
  /**
   * Make a primitive popup from `blurb` atop `bottomRow`; placing it at `location` on the screen.
   */

    import org.sufrin.glyph.unstyled.static.Label
    import GlyphTypes.Pixels
    import NaturalSize.{Col, Row}

    import io.github.humbleui.jwm.App

    val closeResult = Variable[Option[T]](None)

    // println(s"$windowRect, ${nearby.rootDistance}, ${theOffset}")

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

    val ex = Label("X")

    val theBottomRow = Row(bg=bg, align=Top)(bottomRow)

    val theRoot = Col(bg=bg, align=Center)(blurb, theBottomRow)

    var running: Option[Interaction] = None

  def North(glyph: Glyph): this.type = {
    location = Location.NorthFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def NorthEast(glyph: Glyph): this.type = {
    location = Location.NorthEastFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def East(glyph: Glyph): this.type = {
    location = Location.EastFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def SouthEast(glyph: Glyph): this.type = {
    location = Location.SouthEast(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def South(glyph: Glyph): this.type = {
    location = Location.SouthFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def SouthWest(glyph: Glyph): this.type = {
    location = Location.SouthWestFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def West(glyph: Glyph): this.type = {
    location = Location.WestFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def NorthWest(glyph: Glyph): this.type = {
    location = Location.NorthWestFor(theRoot)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to the root of `glyph` */
  def InFront(glyph: Glyph): this.type = {
    val loc = glyph.rootDistance
    location = Location.OnRootOf(glyph)(loc.x + (glyph.w - theRoot.diagonal.x) / 2f, loc.y + (glyph.h - theRoot.diagonal.y) / 2f)
    thisPopup
  }

  import GlyphTypes.Scalar
  def OnRootOf(glyph: Glyph, loc: Vec=Vec.Zero): this.type = {
    location = Location.OnRootOf(glyph)(loc.x, loc.y)
    thisPopup
  }

    /** Allocate the window and start the interaction */
  def start(logEvents: Boolean = false): Unit = {
      App.runOnUIThread {
          () =>
            val theInteraction = new Interaction(App.makeWindow(), theRoot, location.softwareScale) {

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
                val px = Vec.scaleToPixels(location.effectiveScale, theRoot.diagonal.x, theRoot.diagonal.y)
                Dialogue.finest(s"Dialogue size:$px")
                px
              }

            override def screen: Screen = location.screen

              locally {
                import io.github.humbleui.jwm.{WindowMac, ZOrder}
                window match {
                  case mac: WindowMac =>
                    mac.setZOrder(ZOrder.MODAL_PANEL) // Floats above a MAIN_MENU
                  case _ =>
                    window.setZOrder(ZOrder.FLOATING)
                }
              }
              window.bringToFront()
              window.focus()
            }
            theInteraction.handler.logEvents = logEvents; theInteraction.start()
            // Henceforth turn native close requests into calls of close()
            theRoot.guiRoot.onCloseRequest(_ => close())
            running = Some(theInteraction)
        }
    }

  private var _onClose: Option[T=>Unit] = None

    def onClose(onClose: T => Unit): Dialogue[T] = {
      _onClose = Some(onClose)
      this
    }

   /**  Start the popup window, invoking `onClose` when it's closed with a result */
   def andThen(onClose: T => Unit): Unit = {
       _onClose = Some(onClose)
       start()
   }

}
