package org.sufrin.glyph
package tests.demonstrationBook

import styled.TextButton
import styled.Label
import styled.windowdialogues.Dialogue
import styled.Book
import styled.BookSheet
import styled.StringLog
import styled.CheckBox
import NaturalSize.{Col, Row}


class EventsAndWindows(implicit val style: BookSheet, implicit val translation: glyphXML.Translation){

  implicit val pageSheet: StyleSheet = style.buttonSheet
  import translation._
  import pageSheet.{ex, em}
    val nested = Book()
    val Page = nested.Page

    Page("Events", "") {
      val theLog = StringLog(60, 25)

      object CatchEvents extends ReactiveGlyph {

        import io.github.humbleui.jwm._

        var nEvents: Int = 0
        var elideAdjacentMoves: Boolean = false
        var lastEvent: String = ""
        var lastRecord: String = ""

        def record(s: String): Unit = {
          lastRecord = f"$nEvents%05d $s"
          if (elideAdjacentMoves && s.startsWith("Move") && lastEvent.startsWith("Move")) {

          } else {
            theLog.println(lastRecord)
          }
          lastEvent = s
          reDraw()
          nEvents += 1
        }

        override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = {
          record(s"StyledButton ${Modifiers.toBitmap(event).toLongString}@$location")
        }

        /** If the glyph wants to react to a mouse movement */
        override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = {
          record(s"Move${Modifiers.toBitmap(event).toLongString}@$location")
        }

        override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = {
          record(s"Wheel(${event.getDeltaX}, ${event.getDeltaY})")
        }

        override def accept(key: EventKey, location: Vec, window: Window): Unit = {
          record(s"Key ${Modifiers.toBitmap(key).toLongString} ${key.getKey} ")
        }

        override def accept(key: EventTextInput, location: Vec, window: Window): Unit = {
          record(s"Text ${Modifiers.toBitmap(key).toLongString} ${key.getText} ")
        }

        override def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
          record(s"TextMarked ${Modifiers.toBitmap(key).toString} ${key.getText}")
        }

        // Synthetic events delivered by the standard event handler
        override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {
          var key = ""
          event match {
            case _: GlyphEnter =>
              import io.github.humbleui.jwm.MouseCursor
              key = s"GlyphEnter [keyboard grabbed]) ${Modifiers.toBitmap(event).toLongString}"
              guiRoot.grabKeyboard(this)
              window.setMouseCursor(MouseCursor.POINTING_HAND)
              reDraw() // window.requestFrame()
            case _: GlyphLeave =>
              key = s"GlyphLeave ${Modifiers.toBitmap(event).toLongString}"
              guiRoot.freeKeyboard()
              guiRoot.giveupFocus()
          }
          record(key)
        }

        // Synthetic events delivered by the standard event handler on focusin/focusout
        override def accept(event: RootGlyphEvent, location: Vec, window: Window): Unit = {
          record(event.toString)
        }

        /**
         * Draw the glyph on the surface at its given size (as if at the origin).
         */
        def draw(surface: Surface): Unit = {
          drawBackground(surface)
          surface.declareCurrentTransform(this)
          val theLast = Label(lastRecord) //Text(lastRecord, font = labelStyle.font).asGlyph(fg)
          surface.withClip(diagonal) {
            val dy = (diagonal.y - theLast.h) / 2f
            val dx = em.w // (diagonal.x - theLast.w) / 2f
            surface.withOrigin(dx, dy) {
              theLast.draw(surface)
            }
          }
        }


        /**
         * The diagonal size of the glyph
         */
        def diagonal: Vec = Vec(60 * em.w, 5 * ex.h)

        /** A copy of this glyph; perhaps with different foreground/background */
        def copy(fg: Brush, bg: Brush): Glyph = null
        val fg: Brush = DefaultBrushes.black
        val bg: Brush = DefaultBrushes.yellowHuge
      }

      Col.centered(
        <div width="50em" align="justify">
          <p align="center"><b>Events</b></p>
          <p>Mouse and keyboard events happening in the coloured frame below are shown in the log beneath it.
             The most recent event is also shown in the coloured frame.</p>
          <p>Move events occur very frequently as the mouse traverses the frame; so adjacent Moves
             in a sequence are usually for physically close locations. Checking the box below suppresses the second
             and subsequent <tt>Move</tt> reports in such a sequence of moves.</p>
        </div>,
        ex scaled 2,
        Row.centered(Label("Shorten the log of Move sequences: "), CheckBox(initially = CatchEvents.elideAdjacentMoves) {
          state =>
            CatchEvents.elideAdjacentMoves = state
            theLog.println(if (state) "You are now eliding adjacent move events" else "You are now showing adjacent move events")
        }), ex scaled 2,
        CatchEvents.framed(), ex scaled 2,
        theLog
      ).enlarged(20)

    }

    Page("Windows", "") {
      import Location._

      import io.github.humbleui.jwm.{App, Screen}

      // App.methods can only be called after the App has started.
      // So we can only configure the popup in response to a button-press
      lazy val screenButton: Glyph = TextButton("Screens") { _ =>
        val screens = App.getScreens.toList

        def showScreen(s: Screen): Glyph = {
          import RectIO._
          val prim = if (s.isPrimary) "*" else " "

          Label(center(15)(f"${s._id}%15d$prim%1s") +
            center(26)(ir(s._bounds)) +
            center(26)(ir(s._workArea)))
        }

        def center(n: Int)(s: String): String = {
          val sp = "                              "
          val l = sp.take((n - s.length) / 2)
          val r = sp.take(n - s.length - l.length)
          l + s + r
        }

        val header = center(16)("ID") + center(25)("BOUNDS") + center(25)("WORK")
        Dialogue.OK(
          Col.atLeft(
            Label(header),
            Col.atLeft$(screens.map(showScreen(_)))),
            RelativeTo(screenButton), "Screens").start()
      }

      object RectIO {
        import io.github.humbleui.types.IRect
        def ir(r: IRect): String = {
          f"${id(r.getWidth, r.getHeight)}%-12s@${id(r._left, r._top)}%-13s"
        }

        def id(x: Int, y: Int): String = s"($x, $y)"
      }
      // App.methods can only be called after the App has started.
      // So we can only configure this popup in response to a button-press
      lazy val windowsButton: Glyph = TextButton("Windows") { _ =>
        import io.github.humbleui.jwm.Window
        lazy val windows: List[Window] = App._windows.toArray.toList.map(_.asInstanceOf[Window]) // horrid!

        def showWindow(w: Window): Glyph = {
          Label(s"${RectIO.ir(w.getWindowRect)} ${RectIO.ir(w.getContentRect)} ${w.getScreen._id}[${w.getScreen.getScale}]")
        }

        val header = "Window"
        Dialogue.OK(
            Col.atLeft(
              Col.atLeft$(windows.map(showWindow(_)))),
            RelativeTo(windowsButton),
            "Windows")
          .start()
      }


      Col.centered(
        <div width="55em" align="justify">
          <p align="center"><b>Windows and Screens</b></p>
          <p>An exploration of the low-level interface implemented by the
             underlying library module <tt>io.github.humbleui.jwm.App</tt>.
          </p>
          <p>The Screens/Windows buttons pop up lists of screens/windows whenever they are pressed.
            Sizes and locations are in physical (pixel) coordinates. Locations are given relative
            to the location of the primary screen; and this means that in a multi-screen layout
            some coordinates may be negative.</p>
          <p>Each Screen entry shows the ID of the screen, its bounds (as size@location), and its
            work area (as size@location). The work area may be somewhat smaller than the size depending
            on the exact physical layout of the screens.</p>
          <p>Window entries show the size and locations of the application's windows and work area
            on the screen, followed by the id (and scale factor) of the screen on which they are
            being displayed at the moment the Windows button was pressed.</p>

        </div>, ex, ex,
        Row(screenButton, em, windowsButton)
      )
    }


  val GUI: Glyph = nested.Layout.rightCheckBoxes(pageAlign = Center).enlarged(20)

}
