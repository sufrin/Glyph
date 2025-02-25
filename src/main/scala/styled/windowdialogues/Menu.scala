package org.sufrin.glyph
package styled.windowdialogues

import Location.Location
import ReactiveGlyphs.{GenericButton, RawButton}

/**
 *
 * NB: `overlaydialogues.Dialogue.Menu` is usually a more-effective way of constructing and popping-up menus. This
 * Menu API is (nearly) consistent with that, and will eventually be deprecated.
 *
 * Constructs a popup menu whose entries may include `AbstractButton`s.
 * Every menu has a "kill" button on it somewhere, that when pressed causes
 * the menu to pop down; and typing an ESC when that menu has the focus
 * also causes it to pop down.
 *
 * A popup menu also pops down when any of its buttons is pressed, unless
 * that button is associated with a nested Menu. In that case, the menu stays
 * up after a nested menu button is pressed or the nested menu is abandoned.
 *
 *
 * TODO: The kill button was originally invented to work around an implementation problem that arose when trying to
 *       implement "automatic popdown when mouse leaves menu". Whilst we can now (in principle) do that, we
 *       await inspiration for a more straightforward/better-structured implementation.
 *       RootEnterEvent, and RootLeaveEvent are the key events needed to work with, but some refactoring
 *       will be needed.
 *
 */

class Menu(entries: Seq[Glyph]) { thisPopup =>

  var location: Location = null

  import GlyphTransforms.Framed
  import GlyphTypes.Pixels
  import NaturalSize.Col

  import io.github.humbleui.jwm.App

  var running: Boolean = false

  /**
   * Workaround automatic popdown when focus leaves menu:
   * Click the red blob
   */
  import PolygonLibrary.closeButtonGlyph

  val killButton = RawButton(closeButtonGlyph(), closeButtonGlyph(), closeButtonGlyph()) { _ => close() }

  /** Close this menu/window */
  def close(): Unit = App.runOnUIThread {
    () =>
      val rootWindow = root.guiRoot.rootWindow
      running = false
      rootWindow.close()
  }

  /**
   *  The root as a column: each non-menu button causes the menu to close.
   *
   *  (The laziness of `root` is essential because root and `close()` are mutually recursive.)
   */
  lazy val root: Glyph = Framed(fg=DefaultBrushes.red(width=0f))({
    def afterReact(glyph: Glyph): Glyph = glyph match  {
      case button: GenericButton =>
        if (button.isMenuButton) button  else button.afterReact{ _ => close() }
      case _ => glyph
    }
    val col = Col(align=Left)(entries.map(afterReact))
    Col(align=Left)(killButton, col)
  })

  // TODO: Perhaps we need an "annular" reactiveGlyph into which menus can be nested.
  //       When the cursor enters the anulus "from the inside", the menu close
  //       can be triggered. The present implementation provides "intentional"
  //       menu-closing down by mouseclicking on the kill button or hitting the ESC key.

  // TODO: Fix multiply-nested menus [DONE: see below]
  // TODO: Fix consequence of ESCing immediately after the menu appears, ie. while
  //       the cursor is still within the popup-generating button. The "host"
  //       window goes semi-unresponsive. The handler is still locating buttons
  //       (evidenced by the cursor changing shape).
  //       [DONE: 21/03/2024] The ESC-up event had nowhere to go, because
  //       we acted on ESC-down. Now we act on ESC-up, and ESC-downs are
  //       simply absorbed.

  def North(glyph: Glyph): this.type = {
    location = Location.NorthFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def NorthEast(glyph: Glyph): this.type = {
    location = Location.NorthEastFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def East(glyph: Glyph): this.type = {
    location = Location.EastFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def SouthEast(glyph: Glyph): this.type = {
    location = Location.SouthEast(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def South(glyph: Glyph): this.type = {
    location = Location.SouthFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def SouthWest(glyph: Glyph): this.type = {
    location = Location.SouthWestFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def West(glyph: Glyph): this.type = {
    location = Location.WestFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to `glyph` */
  def NorthWest(glyph: Glyph): this.type = {
    location = Location.NorthWestFor(root)(glyph); thisPopup
  }

  /** set the location of this dialogue relative to the root of `glyph` */
  def InFront(glyph: Glyph): this.type = {
    val loc = glyph.rootDistance
    location = Location.OnRootOf(glyph)(loc.x + (glyph.w - root.diagonal.x) / 2f, loc.y + (glyph.h - root.diagonal.y) / 2f)
    thisPopup
  }

  def OnRootOf(glyph: Glyph, loc: Vec=Vec.Zero): this.type = {
    location = Location.OnRootOf(glyph)(loc.x, loc.y)
    thisPopup
  }

  /** Pop-up the menu in the location specified by position */
  def start(logEvents: Boolean=false): Unit = {
    assert(location ne null, "Menu must have defined a non-null location before starting")

    val theInteraction = new Interaction(App.makeWindow(), root, location.softwareScale) {

      import io.github.humbleui.jwm.{EventKey, Window}

      override def inset: Pixels = (0, 0)

      override val title: String = ""

      override def position(window: Window): Pixels = {
        val (cx, cy): Pixels = thisPopup.location.contentLocation
        val (rx, ry): Pixels = Vec.scaleToPixels(location.effectiveScale, location.logicalLocation)
        val result = (rx + cx, ry + cy)
        // println(s"Menu.position (${location.softwareScale})(${location.logicalLocation})(${location.contentLocation}) =$result")
        result
      }

      override def size: Pixels = Vec.scaleToPixels(location.effectiveScale, root.w, root.h)

      /**
       * The popped-up `Interaction` responds to ESCAPE being pressed by closing
       * TODO: consider keyboard accelerators for individual buttons
       */
      override def onKeyboardUnfocussed(key: EventKey): Unit = {
        import io.github.humbleui.jwm.Key._
        key.getKey match {
          case ESCAPE
            // IMPORTANT TO WAIT FOR THE RELEASE
            // For otherwise
            //   (a) there's nowhere for subsequent keys to be accepted
            //   (b) "lingering" on the ESCAPE key for too long causes a race between
            //       the close and the event handler
            //  This approach works: but I have only empirical evidence.
            if !key.isPressed => App.runOnUIThread(() => close())

          case _ =>
            root.reDraw()
        }
      }

      locally {
        import io.github.humbleui.jwm.{WindowMac, ZOrder}
        window.setTitlebarVisible(false)
        window match {
          case mac: WindowMac =>
            mac.setZOrder(ZOrder.POP_UP_MENU)
          case _ =>
            window.setZOrder(ZOrder.FLOATING) // Always on top
        }
      }
    }

    App.runOnUIThread{
      () =>
        theInteraction.handler.logEvents=logEvents
        theInteraction.start()
        theInteraction.window.bringToFront() //**
        theInteraction.window.focus() //**
    }
  }
}

object Menu {
  import Location._

  /**
   * A glyph that appears exactly the same as a `Menu(entries, ...)` and the equivalent `Menu.at(...)()`. Used
   * only to compute the sizes of menus that are to be placed at certain `Location`s.
   */
  def topBar(entries: Seq[Glyph]): Glyph = {
    NaturalSize.Col(align=Left)(PolygonLibrary.closeButtonGlyph, NaturalSize.Col(align=Left)(entries))
  }

  /** A popup menu with `entries` as glyphs: located as specified by `position`. */
  def apply(entries: Seq[Glyph]): Menu = {
    new Menu(entries)
  }

  /**
   * A popup menu with `entries` as glyphs: located as specified by `position`.
   */
  def at(position: Location)(entries: Glyph*): Menu = {
    val menu = new Menu(entries)
    menu.location=position
    menu
  }

}
