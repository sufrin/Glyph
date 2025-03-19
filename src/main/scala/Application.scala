package org.sufrin
package glyph

import io.github.humbleui.jwm.{EventKey, Window}
import NaturalSize.Row

/**
 * An application specified by a GUI, and a title. Some generic flags
 * may be given.
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

  /**
   * Invoked when a keystroke is made in the window directly associated with
   * this application and that window has no glyph with the keyboard focus.
   * May be overridden.
   *
   * @param event
   * @see onStart
   */
  def handleUnfocussedKey(event: EventKey): Unit =  {
      val key = s"Key ${Modifiers.toBitmap(event).toLongString} ${event.getKey}"
      logging.Default.warn(s"Keystroke unexpected: ${key}")
  }

  /**
   * Invoked when the application's GUI has been placed on the screen. This is the right place
   * to declare handlers for window close requests, etc.
   *
   */
  protected def whenStarted(): Unit = {
    GUI.guiRoot.onCloseRequest  { window => window.close() }
    GUI.guiRoot.onUnfocussedKey { event: EventKey => handleUnfocussedKey(event)}
  }

  val defaultIconPath: Option[String] = None
  val extraArgs = new ArrayBuffer[String]()
  var useScreen: Char = 'p'
  var scaleFactor = 1.0f

  def main(args: Array[String]): Unit = {
    import io.github.humbleui.jwm.Screen
    var icon: Option[String] = defaultIconPath
    var logPrefix: String = ""
    extraArgs.clear()


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
          |""".stripMargin)
      }
      case _ =>  extraArgs += arg
    }

    println(s"$logPrefix")

    App.start(() => {
      new Interaction(App.makeWindow(), GUI, scaleFactor, whenStarted()) {

        def getScreen(n: Int): Screen = {
          val screens = App.getScreens
          screens(n min screens.length-1)
        }

        override def args: List[String] = extraArgs.toList
        override def title: String = thisApplication.title
        override def inset = (0, 0)
        override def iconPath = icon

        def onCloseRequest(action: Window=>Unit): Unit = {
          GUI.findRoot.onCloseRequest(action)
        }

        override def onKeyboardUnfocussed(key: EventKey): Unit = {
          GUI.findRoot.handleUnfocusssedKey(key)
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
      }.start()
    })
  }

}

object Application {

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
