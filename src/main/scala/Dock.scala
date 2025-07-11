package org.sufrin.glyph
import org.sufrin.glyph.unstyled.static.FilledRect

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
 *    override val dock: Dock = new Dock() {
 *       setGlyph(Text("eg3"))
 *       onTrayClick(Text("3", fg=Brushes.red, bg=Brushes.yellow)){
 *         location =>
 *             styled.windowdialogues.Dialogue.FLASH(
 *                styled.Label("You clicked\non the system\ntray icon")
 *             ). OnRootOf(GUI, location).start()
 *       }
 *    }
 * }}}
 *
 * The `Dock` of the current application is always available to (rooted) GUI components as `guiRoot.dock`, and
 * its properties can be changed dynamically.
 *
 */

class Dock() {
  var glyph: Glyph = FilledRect(20,10, Brushes.red)

  lazy val taskBar = java.awt.Taskbar.getTaskbar

  def setProgressValue(percentage: Int): Unit =
    if (java.awt.Taskbar.isTaskbarSupported) taskBar.setProgressValue(percentage)

  /** Set the dock icon image from the given glyph */
  def setGlyph(glyph: Glyph): Unit = {
    if (java.awt.Taskbar.isTaskbarSupported) taskBar.setIconImage(Dock.AWTImage(glyph))
    this.glyph = glyph
  }

  lazy val tray: java.awt.SystemTray = java.awt.SystemTray.getSystemTray

  /** Construct a system tray icon from the given glyph */
  def trayIcon(glyph: Glyph): TrayIcon = if (java.awt.SystemTray.isSupported) {
   val icon = Dock.makeTrayIcon(glyph)
   tray.add(icon)
   icon
  } else null

  /**
   * Construct a system tray icon from the given glyph, and have
   * it handle mouseClicks with the given handler
   */
  def onTrayClick(glyph: Glyph)(handle: Vec=>Unit): Unit =  {
    val icon = trayIcon(glyph)
    if (icon ne null)
      icon.addMouseListener(new MouseListener {
        def mouseClicked(e: MouseEvent): Unit = handle(Vec(e.getX, e.getY))
        def mousePressed(e: MouseEvent): Unit = {}
        def mouseReleased(e: MouseEvent): Unit = {}
        def mouseEntered(e: MouseEvent): Unit = {}
        def mouseExited(e: MouseEvent): Unit = {}
      })
  }
}

object Dock {
  def AWTImage(glyph: Glyph): java.awt.Image = {
    val bytes  = External.toByteArray(glyph, "png")
    val stream = new ByteArrayInputStream(bytes)
    val result = javax.imageio.ImageIO.read(stream)
    stream.close()
    result
  }

  def makeTrayIcon(image: java.awt.Image): TrayIcon = {
    val ic = new TrayIcon(image, "Bridge Icon")
    ic.setImageAutoSize(true)
    ic
  }

  def makeTrayIcon(glyph: Glyph): TrayIcon = {
    val ic = new TrayIcon(AWTImage(glyph), "Bridge Icon")
    ic.setImageAutoSize(true)
    ic
  }


}
