package org.sufrin.glyph.tests

import java.awt.desktop.{QuitEvent, QuitResponse}

object HandleQuit {
  /**
   * This prevents CMD-Q from torpedoing an in-flight app.
   */
  def main(args: Array[String]): Unit =  {
    if (java.awt.Desktop.isDesktopSupported) {
      val desk = java.awt.Desktop.getDesktop
      desk.disableSuddenTermination()
      if (!desk.isSupported(java.awt.Desktop.Action.APP_QUIT_HANDLER)) println(s"Can't handle Quit")
      desk.setQuitHandler(new java.awt.desktop.QuitHandler {
        def handleQuitRequestWith(e: QuitEvent, response: QuitResponse): Unit = {
          println(s"Quit: $e")
          if (false) response.performQuit() else response.cancelQuit()
        }
      })
      println("QuitHandler installed")
    } else println("Desktop not supported")

    for { i<-0 until 25 } {
      println(i)
      Thread.sleep(1000L)
    }
  }
}
