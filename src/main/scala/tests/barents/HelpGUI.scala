package org.sufrin.glyph
package tests.barents

import glyphML.language

object HelpGUI {
  private implicit val style: StyleSheet = Viewer.fileSheet.copy(
    textFontSize = 16f,
    textForegroundBrush = Brushes.black
  )
  private val language: language = glyphML.Translator(style)
  language.definitions("settings") = {
    val raw = Barents.viewSettingsIcon
    raw.scaled(style.exHeight/raw.h).withBaseline(baseLine=style.textFontSize, offset=style.textFontSize/3)
  }
  language.definitions("closeview") = {
    val raw = Barents.viewCloseIcon
    raw.scaled(style.exHeight/raw.h).withBaseline(baseLine=style.textFontSize, offset=style.textFontSize/3)
  }
  language.definitions("shelf")      = ViewerWindow.SHELF
  language.definitions("clearshelf") = ViewerWindow.CLEARSHELF

  import language._

  val text: Glyph = <div width="80em" align="justify" parskip="0.5ex">
    <p align="center">
      <b>🍉 The Barents Display 🍉</b>
    </p>
    <p>Each viewer window is associated with one or more <i>directory views</i>, of which one is always shown. Each directory or file in a directory view
      is shown as a single row whose columns show its relevant/selected attributes. </p>
    <p>
      <i>If more than one directory view has been opened in this window but is not yet closed</i>, pressing <glyph gid="closeview"/> closes the currently visible view and
      makes another view visible.
    </p>
    <p>The attributes shown can be  selected using the panel popped up by the
      <glyph gid="settings"/> button; as can the column on which the rows are
      ordered when shown, and whether the order is ascending or descending. The "Invisible" checkbox controls whether
      nominally "invisible" files are shown.</p>
    <p align="center">
      <b>Columns</b>
    </p>
    <p>
      The
      <tt>size+</tt>
      column shows file sizes in compact human-readable form, namely a number followed by a suffix from
      <b>b k m g t</b>
      denoting
      its multiplication by a power of 10:
      <b>0 3 6 9 12</b>
      . Directory sizes are given as the number of files in them (with suffix
      <b>f</b>
      ).
    </p>
    <p>
      An
      <b>S</b>
      to the right of a date/time in the display indicates
      daylight-saving (summer) time for the current locality on the given date.
    </p>
    <p align="center">
      <b>Path</b>
    </p>
    <p>The path to the directory currently on view in the window is shown as a sequence of buttons, each of which corresponds to an
      ancestor and is associated with a "hover-hint" that shows the corresponding
      <i>real path</i>
      -- <i>viz.</i> the path with symbolic links expanded when appropriate. Pressing one opens it or selects a view of it. Pressing the ".." button
      selects the <b>real</b> parent of the current directory for display (which differs only if there was a symbolic link
      somewhere on the path).
    </p>
    <p align="center">
      <b>Selection</b>
    </p>
    <p>Individual windows have a (possibly empty) selection: its rows are selected by mouse:</p>
    <scope>
      <attributes id="tag:p" hang=" *  "/>
      <p>Primary mouse click: selects uniquely</p>
      <p>Primary mouse drag: extends the selection</p>
      <p>Shifted primary mouse click: extends the selection at one end</p>
      <p>Secondary mouse click (or C-Primary) (or drag) inverts selection status of the indicated row(s)</p>
      <p>Mouse double-click, or (ENTER): if  <i>the unique</i> selected row denotes a directory then open a view of it; otherwise open the corresponding file on the desktop if possible. </p>
      <p>Mouse click in the "gutter" at the left of the display: clears the selection</p>
    </scope>
    <p align="center">
      <b>Shelf</b>
    </p>
    <p>There is an Explorer-wide <i>conceptual</i>
      <b>Shelf</b>
      on which is a collection of paths that denote
      individual files in the filestore, and a file or directory is said to be "shelved" if its path is
      on the shelf, and "selected" if its path is in the selection. Shelved files are the objects of the actions described below, and they are
      underlined in the display.
    </p>
    <p>If the underlining is "textured" (see <b>Actions C-X</b>) then the corresponding file has been marked for deletion
      as part of the next "paste" (C-V).
    </p>

    <p>Actions are invoked by pressing GUI buttons or keys on the keyboard.</p>
    <scope>
      <attributes id="tag:p" hang=" *  "/>
      <p>
        <glyph gid="shelf"/> <b>(shelve)</b>
        shelves the selected files.</p>
      <p>
        <b>C-C</b>
        shelves the selected files.</p>
      <p>
        <b>C-X</b>
        shelves the selected files and marks them for later deletion: this is shown by textured underlining. The next "paste" (C-V) action will perform
        the deletion.</p>
      <p>
        <b>C-V</b>
        copies the shelved files to the folder in which it is clicked; deleting them if they were marked for deletion by (C-X).</p>
      <p>
        <b>C-BACKSPACE</b>
        completely deletes the shelved files.</p>
     <p>
       <b>BACKSPACE</b>
       moves the shelved files into their local .TRASH folder with "timestamped" names.</p>
      <p>
        <b>trash</b>
        moves the shelved files into their local .TRASH folder with "timestamped" names.</p>
      <p>
        <b>copy move link ln-s</b>
        respectively copy, move, link, and sym_bol_ical_ly-link the shelved files to the
        current implicit destination. This is
        <i>either</i>
        the directory denoted by a single selected path in the window in which
        the button is pressed,
        <i>or if none is selected</i>
        the folder being shown in that window.
      </p>
      <p>
        <glyph gid="clearshelf"/> <b>(clear shelf)</b>
        removes the deletion mark from shelved paths if they are so marked; otherwise clears the shelf.</p>
      <p>The usual navigation methods:
        <b>home end page-up page-down scroll-wheel</b>
        can be used to scroll the view; and the
        <b>up down</b>
        keys add to the selection in the specified direction if shifted; otherwise selecting a single row in the specified direction.
      </p>
      </scope>
      <p align="center"><b>Opening and Selecting Views</b></p>
      <p>
        The <b>New Viewer</b> button opens a new window in a view of the currently-selected directory (if any), or of
        the currently viewed directory (if none).</p>
    <p>
        When the window has more than one view available, the <b>View</b> button pops up a menu
        each of whose whose buttons selects a view.
    </p>

  </div>.enlarged(20, bg = Brushes.white)

  lazy val dialogue: styled.windowdialogues.Dialogue[Unit] = {
    val port = unstyled.dynamic
      .ViewPort(
        text,
        fg = Brushes.blueFrame,
        portDiagonal = Vec(
          text.w,
          text.h /
            2
        )
      )
      .withDragging(true)
      .withPanning(false)
      .withScaling(false)
    styled.windowdialogues.Dialogue.FLASH(
      port.enlarged(port.fg.strokeWidth * 4),
      title = "Viewer Help"
    )(style)
  }

  def button(style: StyleSheet): Glyph = {
    lazy val but: Glyph =
      styled.TextButton("Help") { _ =>
        if (HelpGUI.dialogue.running.isEmpty)
          HelpGUI.dialogue.East(but).start(floating = false)
      }(style)
    but
  }

}
