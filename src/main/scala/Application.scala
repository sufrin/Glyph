package org.sufrin
package glyph

import org.sufrin.glyph.GlyphTypes.{EventKey, Window}
import org.sufrin.glyph.NaturalSize.Row

/**
 * An application specified by a GUI, and a title. Some generic arguments
 * may be specified on the command line. Other command-line arguments are
 * made available to the GUI (as `extraArgs`)
 */
trait Application {
  thisApplication =>
  def wherePossible(body: => Unit): Unit = try { body } catch { case exn: UnsupportedOperationException => { println(exn) } }

  import io.github.humbleui.jwm.App

  import scala.collection.mutable.ArrayBuffer
  def GUI:   Glyph
  def title: String

  /**
   * Invoked on requests to close the window directly associated with
   * this application. May be overridden
   * @param window
   * @see onStart
   */
  def handleWindowCloseRequest(window: Window): Unit = window.close()

  val bell = Sound.Clip("WAV/tonk.wav")

  /**
   * Invoked when a keystroke is made in the window directly associated with
   * this application and that window has no glyph with the keyboard focus.
   * May be overridden.
   *
   * @param event
   * @see onStart
   */
  def handleUnfocussedKey(event: EventKey): Unit =  {
      if (event.isPressed) bell.play()
      val key = s"Key ${Modifiers.toBitmap(event).toLongString} ${event.getKey}"
      logging.Default.warn(s"Keystroke unexpected: ${key}")
  }

  /**
   * Invoked when the application's GUI has been placed on the screen. This is the right place
   * to declare handlers for window close requests, etc.
   *
   */
  protected def whenStarted(): Unit = {
      GUI.guiRoot.onCloseRequest { window => window.close() }
      GUI.guiRoot.onUnfocussedKey { event: EventKey => handleUnfocussedKey(event) }
  }

  val defaultIconPath: Option[String] = None

  /** The dock/system-tray proxy */
  val dock: Dock = Dock()

  val extraArgs = new ArrayBuffer[String]()
  var useScreen: Char = 'p'
  var scaleFactor = 1.0f

  /**
   * @param args "command-line" arguments that were passed to the application
   *
   * Parse the standard `Application` arguments (listed on the terminal in response to
   * a flag of "-h"), then establish and startan `Interaction` accordingly. Its `args` will
   * be the non-standard arguments that were passed to `main`.
   */
  def main(args: Array[String]): Unit = {
    import io.github.humbleui.jwm.Screen
    var icon: Option[String] = defaultIconPath
    var logPrefix: String = ""
    extraArgs.clear()

    try { java.awt.Taskbar.getTaskbar } catch { case error: Throwable => () }

    for {arg <- args} arg match {
      case s"-log($logprefix)" => logPrefix=logprefix
      case s"-log($logPaths)=$level" =>
           for { obj <- logPaths.split("[,:]").map(_.trim) }
               logging(s"$logPrefix.$obj")=level


      case s"-log:$logPaths=$level" =>
        for { obj <- logPaths.split("[,:]").map(_.trim) }
          logging(s"$logPrefix.$obj")=level

      case s"-log:$logprefix" => logPrefix=logprefix

      case s"-scale=$scale" if scale.matches("[0-9]*.[0-9]+")       => scaleFactor = scale.toFloat
      case s"-icon=$path"                                           => icon = if (java.nio.file.Path.of(path).toFile.exists) Some(path) else None
      case s"-screen=$whichScreen" if whichScreen.matches("[0-9p]") => useScreen = whichScreen.head
      case s"-h" => {
        import scala.sys.process.stderr
        stderr.println(
        s"""(bad flag: $arg)
          |Flags are:
          |-log(logPrefix) => set the (package) prefix for subsequent logPath components
          |-log(logPath,...logPath)=levelname => set the logging level of the objects named logPrefix.logPath
          |-scale=d.dd     => scale the initial display
          |-screen=[0123p] => use the numbered screen (012) or the primary screen (p) for the initial display
          |-icon=$$path    => set the icon path to $$path
          |-logevents      => log (on the terminal) every event. Usually provides far too much detail.
          |-noresize       => forbid window resizing
          |""".stripMargin)
      }
      case _ =>  extraArgs += arg
    }


    App.start(() => {
      new Interaction(App.makeWindow(), GUI, scaleFactor, { if (GUI.hasGuiRoot) whenStarted()}) {

        def getScreen(n: Int): Screen = {
          val screens = App.getScreens
          screens(n min screens.length-1)
        }

        override def args: List[String] = extraArgs.toList
        override def title: String = thisApplication.title
        override def inset = (0, 0)
        override def iconPath = icon

        def onCloseRequest(action: Window=>Unit): Unit = {
          if (GUI.hasGuiRoot) GUI.findRoot.onCloseRequest(action)
          window.requestFrame()
        }

        override def onKeyboardUnfocussed(key: EventKey): Unit = {
          if (GUI.hasGuiRoot) GUI.findRoot.handleUnfocusssedKey(key)
          window.requestFrame()
        }

        locally {
          handler.logEvents = args.contains("-logevents")
          handler.noResize = args.contains("-noresize")
        }

        override def screen = useScreen match {
          case 'p' => App.getPrimaryScreen
          case _   => getScreen(useScreen-'0')
        }

        override val dock: Dock = thisApplication.dock
      }.start()
    })
  }

}

object Application {

  var GUI: Option[Glyph] = None

  def confirmCloseRequestsFor(GUI: Glyph)(implicit sheet: StyleSheet): Unit = {
    GUI.guiRoot.onCloseRequest{ window: Window => confirmCloseOn(GUI, window) }
  }

  def enableAutoScaleFor(GUI: Glyph): Unit = {
    GUI.guiRoot.autoScale = true
  }

  private def confirmCloseOn(glyph: Glyph, window: Window)(implicit sheet: StyleSheet): Unit = {
    import styled.windowdialogues.Dialogue.OKNO

    // TODO: windowdialogues needs to set software scale more carefully than now if autoScale
    val prompt = Row(align=Mid)(PolygonLibrary.closeButtonGlyph scaled 5 enlarged 50,
      styled.Label("Do you want to Exit?")(sheet) scaled 1.5f
    ).enlarged(50)
    OKNO(prompt,
      title = "Exit Dialogue", ok = " Exit now ", no = " Continue ").InFront(glyph).andThen(close => if (close) window.close())
  }
}
