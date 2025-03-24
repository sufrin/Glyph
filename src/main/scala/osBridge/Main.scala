package org.sufrin
package glyph
package osBridge

import io.github.humbleui.skija.{ColorAlphaType, ColorType, Image, ImageInfo, Pixmap}
import logging.{SourceLoggable, update => setLevel}

import java.awt.desktop._
import java.awt.Desktop
import java.awt.datatransfer.DataFlavor
import java.awt.dnd.{DnDConstants, DropTarget, DropTargetAdapter, DropTargetDropEvent}
import java.awt.image.DataBufferInt
import java.io.ByteArrayInputStream
import java.nio.{ByteBuffer, IntBuffer}
import javax.swing.{Icon, ImageIcon}


/**
 *
 * Bridge between OS and Glyph applications
 *
 * INCOMPLETE EXPERIMENT: Roadmap
 *
 */

trait Bridge {

  def wherePossible(body: => Unit): Unit = try { body } catch { case exn: UnsupportedOperationException => {} }

  def withDesktop(body: => Unit): Unit = {
      val desk: Desktop = Desktop.getDesktop

      // One or more of these facilities are not available on Linux

      wherePossible {
        desk.disableSuddenTermination()
      }

      wherePossible {
        desk.setQuitHandler {
          new QuitHandler() {
            def handleQuitRequestWith(qe: QuitEvent, qr: QuitResponse): Unit = {
              SwingMain.fine(s"QuitEvent(${qe.getSource})")
              if (canQuit())
                qr.performQuit()
              else
                qr.cancelQuit()
            }
          }
        }
      }
      // FINALLY
      body
    }

    def canQuit(): Boolean = false

  }

object SwingMain extends Bridge with SourceLoggable {


  import javax.swing._
  import java.awt

  def main(args: Array[String]): Unit = withDesktop {
    level = org.sufrin.logging.ALL
    val frame: JFrame = new JFrame("Glyph Bridge")
    frame.setLayout(new awt.FlowLayout)
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)

    val drop: JLabel = new JLabel()
    val image = IconUtilities.AWTImage(unstyled.Label("HELLO") above unstyled.Label("World"))
    drop.setIcon(IconUtilities.makeIcon(image))
    frame.add(drop)

    new DropTarget(drop, new DropTargetAdapter {
      def drop(dropEvent: DropTargetDropEvent): Unit = {
        dropEvent.acceptDrop(DnDConstants.ACTION_COPY)
        SwingMain.finest(s"Drop: $dropEvent")
        val files = dropEvent.getTransferable.getTransferData(DataFlavor.javaFileListFlavor)
        println(files)
      }
    })

    awt.SystemTray.getSystemTray.add(IconUtilities.makeTrayIcon(IconUtilities.AWTRectangularImage(w=64, h=32)))

    if (awt.Taskbar.isTaskbarSupported) {
      val taskbar = awt.Taskbar.getTaskbar
      taskbar.setProgressValue(75)
      try
          taskbar.setIconImage(IconUtilities.AWTRectangularImage())
      catch {
          case e: UnsupportedOperationException => warn("Cannot set Dock Icon")
      }
    }
    frame.pack()
    frame.setVisible(true)

    // Now try an application
    //tests.demonstrationBook.Pages.main(args)

  }

  //def whenDropped
}


object IconUtilities {

  import java.awt._
  import java.awt.image.BufferedImage

  def AWTRectangularImage(color: Color=Color.GREEN, w: Int=16, h: Int=16): java.awt.Image = {
    val redImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val g2d = redImage.createGraphics()
    g2d.setColor(color)
    g2d.fillRect(0, 0, w, h)
    g2d.dispose()
    redImage
  }

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

  def makeIcon(image: java.awt.Image): Icon = {
    val ic = new ImageIcon(image)
    ic
  }


}

