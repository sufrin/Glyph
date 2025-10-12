package org.sufrin.glyph
package tests.barents

import styles.decoration.RoundFramed
import GlyphTypes.FontStyle.NORMAL
import files.Folder
import styled.Logging

import org.sufrin.glyph.tests.barents.PathProperties.PathProperty
import org.sufrin.logging.SourceLoggable

import java.nio.file.{Path, Paths}

/**
 * The main application's root. Its GUI consistes of a single `ViewerWindow` whose
 * initially selected `Viewer`  is bound to the current user's home directory.
 */
object Barents extends Application with SourceLoggable {
  implicit val fileSheet: StyleSheet =
    StyleSheet(buttonDecoration=RoundFramed(fg=Brushes.darkGrey(width=3), bg=Brushes.lightGrey, enlarge=9f, radius=10), buttonFontSize = 15, labelFontSize = 15, buttonFontStyle=NORMAL, labelForegroundBrush= Brushes.blue)

  val homePath: Path  = Paths.get(System.getProperty("user.home"))

  val icon:     Glyph = External.Image.readResource("/explorer")

  lazy val GUI: Glyph = {
    val host: ViewerWindow = new ViewerWindow(homePath)
    val hostGUI: Glyph = host.GUI
    Viewer.dialogueAnchor = hostGUI
    Logging(Viewer, Folder, FileOperations)
    Logging.Decorate(hostGUI, Mid)
  }

  def title: String = homePath.abbreviatedString()
  override
  val dock:  Dock   = Dock(icon)


  override protected def whenStarted(): Unit = {
    super.whenStarted()
    GUI.guiRoot.autoScale=true
  }
}


