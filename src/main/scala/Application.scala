package org.sufrin.glyph

import io.github.humbleui.jwm.Window

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
   * Invoked on requests to close
   * @param window
   */
  def onClose(window: Window): Unit = window.close()

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
               org.sufrin.logging(s"$logPrefix.$obj")=level
      case s"-scale=$scale" if scale.matches("[0-9]*.[0-9]+") => scaleFactor = scale.toFloat
      case s"-icon=$path"                                     => icon = if (java.nio.file.Path.of(path).toFile.exists) Some(path) else None
      case s"-screen=$whichScreen" if whichScreen.matches("[0123p]")    => useScreen = whichScreen.head
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

    App.start(() => {
      new Interaction(App.makeWindow(), GUI, scaleFactor) {

        def getScreen(n: Int): Screen = {
          val screens = App.getScreens
          screens(n min screens.length)
        }

        override def args: List[String] = extraArgs.toList
        override def title: String = thisApplication.title
        override def inset = (0, 0)
        override def iconPath = icon

        override def screen = useScreen match {
          case 'p' => App.getPrimaryScreen
          case '0' => getScreen(0)
          case '1' => getScreen(1)
          case '2' => getScreen(2)
          case '3' => getScreen(3)
        }
      }.start()
      GUI.findRoot.onCloseRequest(onClose(_))
    })
  }

}
