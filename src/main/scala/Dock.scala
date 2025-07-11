package org.sufrin.glyph
import org.sufrin.glyph.unstyled.static.FilledRect
import org.sufrin.glyph.GlyphTypes.Font

import java.awt.TrayIcon
import java.awt.event.{MouseEvent, MouseListener}
import java.io.ByteArrayInputStream

/**
 * A proxy class for system tray and dock. A simple use  is exemplified in
 * the `GlyphBook` `Application`, where the dock is assigned an icon made from a simple text glyph.
 *
 * A more ambitious use appears in the `Example3` `Application`, where
 * a system tray icon showing "3" is made that responds to a mouse click
 * by popping up a FLASH window dialogue "near" the tray icon.
 * {{{
 * override val dock: Dock = new Dock() {
 *         val icon = trayIcon(Text("3", fg=Brushes.red, bg=Brushes.yellow))
 *         icon.handleClick {
 *           location =>
 *             styled.windowdialogues.Dialogue.FLASH(styled.Label("You clicked\non the system\ntray icon")).OnRootOf(GUI, location).start()
 *         }
 *         setGlyph(Text("eg3"))
 *     }
 * }}}
 *
 * The `dock` of the currently-running application is available (on the application has established its gui)  at `guiRoot.dock`.
 *
 * Example3e and Example3f show how to clone an application's gui while keeping the same dock.
 *
 */

class Dock() {
  import Dock._

  var glyph: Glyph = FilledRect(20,10, Brushes.red)

  lazy val taskBar = java.awt.Taskbar.getTaskbar

  def setProgressValue(percentage: Int): Unit =
    if (java.awt.Taskbar.isTaskbarSupported) taskBar.setProgressValue(percentage)

  /** Set the dock icon image from the given glyph */
  def setGlyph(glyph: Glyph): Unit = {
    if (java.awt.Taskbar.isTaskbarSupported) taskBar.setIconImage(Dock.AWTImage(glyph))
    this.glyph = glyph
  }

  lazy val systemTray: java.awt.SystemTray = java.awt.SystemTray.getSystemTray

  /** Construct a system tray icon from the given glyph, add it to the system tray, and return a proxy for it */
  def trayIcon(glyph: Glyph, toolTip: String = null): Dock.TrayIcon = {
       val icon = Dock.makeTrayIcon(glyph, toolTip)
       icon.addTo(systemTray)
       icon
  }

}

object Dock {

  case class TrayIcon(private val icon: java.awt.TrayIcon) {
    override val toString: String = if (isDefined) "TrayIcon(defined)" else  "TrayIcon()"

    def isDefined: Boolean = icon ne null

    def addTo(tray: java.awt.SystemTray): Unit =
      if (isDefined) tray.add(icon)

    def setGlyph(glyph: Glyph): Unit =
      if (isDefined) {
        icon.setImage(Dock.AWTImage(glyph))
      }

    def toolTip(tip: String=null): Unit =
      if (isDefined) icon.setToolTip(tip)

    def handleClick(handle: Vec=>Unit): Unit =
      if (isDefined) {
        icon.addMouseListener(new MouseListener {
          def mouseClicked(e: MouseEvent): Unit = handle(Vec(e.getX, e.getY))
          def mousePressed(e: MouseEvent): Unit = {}
          def mouseReleased(e: MouseEvent): Unit = {}
          def mouseEntered(e: MouseEvent): Unit = {}
          def mouseExited(e: MouseEvent): Unit = {}
        })
      }
  }

  /** Returns the `AWT Image` derived from glyph` (png encoding) */
  def AWTImage(glyph: Glyph): java.awt.Image = {
    val bytes  = External.toByteArray(glyph, "png")
    val stream = new ByteArrayInputStream(bytes)
    val result = javax.imageio.ImageIO.read(stream)
    stream.close()
    result
  }

  /** Returns a system tray icon with image made from `glyph`, or a no-op proxy if system tray is not supported */
  def makeTrayIcon(glyph: Glyph, toolTip: String=null): TrayIcon = if (java.awt.SystemTray.isSupported) {
    val ic = new java.awt.TrayIcon(AWTImage(glyph), toolTip)
    //ic.setImageAutoSize(true)
    TrayIcon(ic)
  } else TrayIcon(null)

  /** A  `Dock` whose icon is derived from `iconGlyph` */
  def apply(iconGlyph: Glyph): Dock = new Dock { setGlyph(iconGlyph) }

  /** Dock with glyph copied from an existing dock (for cloned interfaces) */
  def apply(dock: Dock): Dock = apply(dock.glyph)

  /** Dock with glyph copied from an existing application (for cloned interfaces) */
  def apply(application: Application): Dock = apply(application.dock)

  /** A  `Doc` with a label that uses the `fallback` (style) properties */
  def apply(label: String, font: Font=fallback.textFont, fg: Brush=fallback.textForeground, bg: Brush=Brushes.transparent, align: Alignment=Center): Dock = new Dock {
    setGlyph(unstyled.Label(label, font, fg, bg, align))
  }

}
