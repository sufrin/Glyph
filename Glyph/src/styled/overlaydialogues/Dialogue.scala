package org.sufrin.glyph
package styled
package overlaydialogues


import Location._
import NaturalSize.{Col, Row}
import unstyled.{reactive, static}
import unstyled.reactive.GenericButton
import unstyled.static.INVISIBLE

import io.github.humbleui.jwm.{App, EventMouseScroll}


/**
 * Overlay variant of `Dialogue`
 */
object Dialogue {

  def defaultCloseGlyph: Glyph = IconLibrary.CLOSE(16)

  /**
   *  A generic overlaydialogues "choice" popup, located at the given `position`. It can be popped down without using
   *  any of the buttons on its bottom row, by hitting the kill button placed on its top row.
   */
  def POPUP[T](blurb: Glyph, buttons: Seq[Glyph], closeGlyph: Option[Glyph] = Some(defaultCloseGlyph))(implicit style: StyleSheet): Dialogue[T] = {
    new Dialogue[T](blurb, buttons, location=null, bg = style.popupBackgroundBrush, fg=style.popupForegroundBrush, isNested = false, isMenu = false, closeGlyph = closeGlyph)
  }

  import unstyled.reactive.GenericButton

  /**
   * If `glyph` is an `AbstractButton` and glyph.isMenuButton` then make `continue` its continuation: that is,
   * the code that runs whenever `glyph`'s reaction is invoked (and just after the reaction).
   *
   */
  def afterReact(glyph: Glyph)(continue: => Unit): Glyph = glyph match {
    case button: GenericButton =>
      if (button.isMenuButton) button else button.afterReact { _ => continue }
    case _ => glyph
  }


  /**  Yield a menu button in the given implicit style that activates a pop-up menu of the glyphs `buttons`.
   *   An activated menu is modal: it takes the entire application focus while it is up; and any nested menus popped up by it will
   *   (likewise) take the entire application focus from the menu from which they were activated. The `buttons` that are active will behave as expected when clicked,
   *   then cause the whole menu to be dismissed, and focus to revert to an appropriate glyph or to become uncommitted.
   *
   *   A menu popup can be dismissed by clicking on its
   *   close button/close bar or using the ESC key or
   *   by clicking somewhere outside the popup.
   *
   *   Every attempt is made to accomodate the popup in a sensible place (to the right of the menu button), or at least in
   *   a place where it is completely visible (on its window). But if it has popped up in an inconvenient place, it can be moved
   *   using the
   *   HOME, END, UP, LEFT, DOWN, RIGHT keys, and the mousewheel. The first moves the popup north-west by its own dimension;
   *   the second moves it south-east by its own dimension. The wheel moves it incrementally in the direction indicated.
   *
   */
  def Menu(name: String)(button: Glyph, buttons: Glyph*)(implicit sheet: StyleSheet): Glyph = {
    Menu(name, false, button :: buttons.toList)
  }

  def NestedMenu(name: String)(button: Glyph, buttons: Glyph*)(implicit sheet: StyleSheet): Glyph = {
    Menu(name, nested = true, button :: buttons.toList)
  }

  /**
   *
   * Yield a menu button in the given button style that is bound to a menu consisting of the glyphs `buttons`.
   *
   */
  def Menu(name: String, nested: Boolean, buttons: Seq[Glyph])(implicit sheet: StyleSheet): Glyph = {
    lazy val popDowns: Seq[Glyph] = buttons.map { button => afterReact(button) { popup.close() }}
    lazy val width   = popDowns.map(_.w).max
    lazy val uniform = popDowns.map  {
      glyph =>
        //if (glyph.isMenuButton) //** Why was this different to any old reactive?
         // glyph.enlargedTo(width, glyph.h)
        //else
          if (glyph.isReactive)
            sheet.menuStyle.reactive.frame.decorate(glyph.enlargedTo(width, glyph.h))
          else
            sheet.menuStyle.inactive.decorate(glyph.enlargedTo(width, glyph.h))
    }

    def locatePopup(glyph: Glyph): RelativeTo = {
      val Vec(xo, yo) = glyph.rootDistance
      OnRootOf(glyph)(xo + glyph.w, yo+glyph.h)
    }

    lazy val popup: Dialogue[Unit] =
      new Dialogue[Unit](INVISIBLE(), uniform, East(button),
            bg = sheet.popupBackgroundBrush,//menuStyle.bg,
            fg = sheet.menuStyle.fg,
            isNested = nested,
            isMenu = true,
            closeGlyph = Some(defaultCloseGlyph)) {
        // Reactivate the button when the menu is popped down
        onClose{ _ =>
          button match {
            case b: reactive.GenericButton =>
              b.inactive = false
              b.hovered = false
            case _ =>
          }
        }
      }

    lazy val reaction: reactive.Reaction = {
      _ =>
        //println(s"Popping up $name at ${button.location}")
        // Deactivate the button when the menu is popped up
        button match {
          case b: reactive.GenericButton =>
            b.inactive = true
            b.hovered  = false
            b.guiRoot.giveupFocus()
          case _ =>
        }

        popup.start()
    }

    /** The menu button itself */
    lazy val button: Glyph =
      if (nested)
        styled.MenuButton(name) (reaction) (sheet)
      else
        styled.TextButton(name) (reaction) (sheet)

    button.asMenuButton
  }

  /**
   * A modal, closeable popup that shows the `blurb`, and offers "OK".
   * Its `start()` or `andThen` method yields  `()` if "OK" is clicked; and `null` if the
   * popup is closed from its close button.
   *
   * The position must be set by one of
   * the `Dialogue` methods `North`, ... before the dialogue is started.
   * For example:
   * {{{
   *    OK("You blew it!").North(anchor).start()
   * }}}
   */
  def OK(blurb: Glyph, ok: String="OK")(implicit sheet: StyleSheet): Dialogue[Unit] = {
    // Mutual references ok<->popup
    lazy val okButton: Glyph = styled.TextButton(ok) {
      _ => popup.close(())
    }
    lazy val popup: Dialogue[Unit] = POPUP[Unit](blurb, List(okButton))
    popup
  }

  /**
   * A modal, closeable popup `Dialogue[Boolean]` that shows the `blurb`, and offers "OK" or "NO". Its `start()`, or `andThen` method
   * yields true`/`false` respectively when "OK"/"NO" are pressed; or `null` if the dialogue was closed from its
   * close button.
   *
   * The position must be set by one of
   * the `Dialogue` methods `North`, `NorthEast`,  ... before the dialogue is started.
   * For example:
   * {{{
   *    OKNO("Do you really want this?")
   *    .SouthWest(anchor)
   *    .andThen{
   *       case true  => ...
   *       case false => ...
   *    }
   * }}}
   *
   */
  def OKNO(blurb: Glyph, ok: String = " OK ", no: String = " NO ", title: String = "")(implicit sheet: StyleSheet): Dialogue[Boolean] = {
    // Laziness because there are mutual references ok<->popup, no<->popup
    lazy val okButton: Glyph = styled.TextButton(ok) {
      _ => popup.close(true)
    }
    lazy val noButton: Glyph = styled.TextButton(no) {
      _ => popup.close(false)
    }
    lazy val popup: Dialogue[Boolean] = POPUP[Boolean](blurb, List(okButton, noButton))
    popup
  }

  /** A modal, closeable popup `Dialogue[String]` that shows the `blurb`, and offers each of
   *  the given choices on a button. Its
   * `start()`, or `andThen` method yields
   * the string on the button that was used; or `null` if the dialogue was closed from its
   * close button.
   *
   * The position must be set by one of
   * the `Dialogue` methods `North`, `NorthEast`,  ... before the dialogue is started.
   * For example:
   * {{{
   *    CHOOSE("The document has been modified.")("Keep editing", "Save and Quit", "Abandon and Quit")
   *    .East(anchor)
   *    .andThen{
   *       case "Keep editing"  => ...
   *       case "Save and Quit" => ...
   *       case "Abandon and Quit" => ...
   *    }
   * }}}
   */
  def CHOOSE(blurb: Glyph)(choices: String*)(implicit sheet: StyleSheet): Dialogue[String] = {
    lazy val buttons = choices.map {
      choice => styled.TextButton(choice) { _ => popup.close(choice) }
    }
    lazy val popup: Dialogue[String] = POPUP(blurb, buttons)
    popup
  }

  /**
   * A generalization of  `CHOOSE`, with buttons  labelled with (copies of) the `Glyph` components of `choices`. When
   * `(t, g)` appears as a choice; pressing the button labelled with `g` yields the value `t`. For example:
   * {{{
   *        CHOICE("How many hands?")((1, Label("One")), (2, Label("Two")))
   *        .West(anchor)
   *        .andThen{
   *           case 1  => ...
   *           case 2 => ...
   *        }
   * }}}
   */
  def CHOICE[T](blurb: Glyph)(choices: (T, Glyph)*)(implicit style: StyleSheet): Dialogue[T] = {
    lazy val buttons = choices.map {
      case (t, g) => reactive.RawButton(g(), g(), g()) { _ => popup.close(t) }//.framed().enlarged(20)
    }
    lazy val popup: Dialogue[T] = POPUP(blurb, buttons)
    popup
  }
}

/**
 *  Manager for TAB/ENTER/ESC/ and dragging interactions with a Dialogue
 *  TODO: fix nested menu behaviour bug (non-popup of nested menus)
 *        WORKAROUND: managers work only for dialogues
 */
class NavigationManager(buttons: Seq[Glyph], var preferred: Int, menu: Boolean, nested: Boolean)(close: => Unit) {
  def Action(accepting: Boolean): Unit = {
    if (!accepting) close else
    if (accepting && preferred>=0) buttons(preferred) match {
      case button: GenericButton =>
        button.invokeReaction()
        if (menu && !nested) close
      case _ =>
        // Beep
    }
    else
      close
  }

  def Next(): Unit = if (preferred>=0) {
    buttons(preferred) match {
      case button: GenericButton =>
        button.setHover(false)
      case _ =>
    }
    preferred = (preferred + 1) % buttons.length
    buttons(preferred) match {
      case button: GenericButton =>
        button.setHover(true)
      case _ =>
    }
  }

  def Init(): Unit = if (menu) {
      preferred = -1
    } else
    // mark preferred
    if (preferred>=0) buttons(preferred) match {
      case button: GenericButton => button.setHover(true)
      case _ =>
    }
}


/**
 * A popup implemented as an `overlaydialogues`` layer that will be
 * rendered on the window referred to by `location`.
 *
 * @param blurb the glyph that explains the dialogue
 *
 * @param buttons the reactive glyphs to be set in a row at the foot of the dialogue (or in a column if `isMenu`
 *
 * @param location the location at which the top-left corner of the glyph is to be shown -- typically set non-null post-construction by (eg) `.North(glyph)`
 *
 * @param closeGlyph If the popup is to have a close button showing `g` in its top left corner, then `Some(g)`
 *
 * @param isModal When true (the default case) this means that the popup grabs all input from its host window from the moment it is started until (just after) it is `close`d.
 *
 * @param isMenu When true this dialogue represents a popup menu
 *
 * @param isNested if the dialogue/menu is nested
 *
 * @tparam T the type of value passed to the continuation (if any) by invoking `close`
 *
 */
class Dialogue[T](blurb: Glyph,
                  buttons: Seq[Glyph],
                  var location: RelativeTo = null,
                  bg: Brush, // background of the entire popup
                  fg: Brush, // edge placed around the popup
                  val isNested:   Boolean,
                  var isMenu:     Boolean = false,
                  var isModal:    Boolean = true,
                  var canEscape:  Boolean = false,
                  val closeGlyph: Option[Glyph] = None,
                  var preferred: Int = 0)
{
  thisPopup =>


  val GUI      =
    if (isMenu)
      Col(align=Center, bg=bg)(buttons).edged(fg)
    else
      Col(align=Center, bg=bg)(blurb, Row(align=Mid, bg=bg)(buttons)).edged(fg)

  val navigation = new NavigationManager(buttons, if (buttons.isEmpty) -1 else preferred, nested=isNested, menu=isMenu)(close())

  import NaturalSize.Col
  import unstyled.reactive.RawButton

  /**
   * This will be the reactive glyph, if any, that also responds to ESCAPE/HOME/END and the mousewheel
   * It's essential to have one of these if the popup is modal and we wish
   * the user to be able to abandon the popup without doing anything (eg. making
   * a choice or hitting a nested menu button).
   */
  var theCloseButton: Option[ReactiveGlyph] = None

  /**
   * Close this popup, delivering `result: T` to its continuation, if any.
   *
   * TODO: There is a certain delicacy about this implementation, because
   *       the close button/bar is used to handle keystrokes, such as
   *       ESCAPE -- whose interpretation is `close`; and the popup-movement keys.
   *       The implementation gives up the focus after a close.
   *       IDEA: It might be better if after popping the overlay we restored the
   *       previous keyboard focus.
   */
  def close(result: T = null.asInstanceOf[T]): Unit = App.runOnUIThread { ()=>
    //println("Closing")
    val RelativeTo(glyph, _) = location
    glyph.guiRoot.Overlay.pop() // gives up keyboard focus at last pop
    if (_onClose.isDefined) (_onClose.get)(result)
  }


  protected val closeButtonAppearance: Glyph = closeGlyph match {
    case Some(glyph) =>
      static.Concentric(rowAlign=Mid, colAlign=Center).Left(
        static.FilledRect(GUI.w, glyph.h*1.3f, fg=Brushes.lightGrey, bg=Brushes.lightGrey),
        glyph,
      )
    case None        =>
      static.FilledRect(GUI.w-2, 5f, fg=Brushes.lightGrey, bg=Brushes.lightGrey) // TODO: 5f is a magic number -- width of the close bar
  }


  /**
   * The `guiRoot` decorated with a close button/bar: this is the effective GUI root of the overlay that the dialogue will inhabit.
   * It is constructed exactly once; even when the dialogue is repeatedly opened and closed.
   */
  lazy val overlayRoot: Glyph = {
    // set up the killbutton to (also) respond to ESCAPE with a `close()`
    val closeButton = new RawButton(
      closeButtonAppearance(),
      closeButtonAppearance(),
      closeButtonAppearance, closeButtonAppearance.fg, closeButtonAppearance.bg,
      { _: Modifiers.Bitmap  => close() }
    )
    {
      import io.github.humbleui.jwm.{EventKey, Window}
      import io.github.humbleui.jwm.Key._

      override def extra: Vec = Vec.Zero

      override def accept(key: EventKey, location: Vec, window: Window): Unit = {
        // Nudge the window on certain keystrokes; close on esc.
        // TODO: this is where ENTER (accept the current lit-up) favoured, and TAB (move the lit-up) will go
        //       some pervasive structural changes may be necessary
        //       annotation layer to show the favoured choice?
        // Buttons take effect when the key is released; otherwise after a close action on press, there would be
        // no dialogue for the release key to be directed at.
        var (dx, dy, drag) = (0f, 0f, true)
        key.getKey match {
          case ESCAPE  if isModal && !key.isPressed => close()
          case ESCAPE  if canEscape && !key.isPressed => close()
          case ESCAPE  if !key.isPressed => drag=false; navigation.Action(false)
          case ENTER   if !key.isPressed => drag=false; navigation.Action(true)
          case TAB     if !key.isPressed => drag=false; navigation.Next()
          case UP      if isModal && !key.isPressed => dy -= 15f
          case DOWN    if isModal && !key.isPressed => dy += 15f
          case RIGHT   if isModal && !key.isPressed => dx += 15f
          case LEFT    if isModal && !key.isPressed => dx -= 15f
          case HOME    if isModal && !key.isPressed =>
                          dx -= overlayRoot.location.x
                          dy -= overlayRoot.location.y
          case END     if isModal && !key.isPressed =>
                          dx += overlayRoot.diagonal.x
                          dy += overlayRoot.diagonal.y
          case other => super.accept(key, location, window)
        }
        if (drag)
        {
          val Vec(x, y) = overlayRoot.guiRoot.diagonal-overlayRoot.diagonal-(5,5) // limit
          overlayRoot.location =
            Vec(x min (overlayRoot.location.x+dx) max 0,
                y min (overlayRoot.location.y+dy) max 0)
          overlayRoot.reDraw()
        }
      }

      override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = {
        val dx = event._deltaX
        val dy = event._deltaY
        if (isModal) overlayRoot.location = overlayRoot.location + (dx, dy); overlayRoot.reDraw()
      }

    }
    theCloseButton = Some(closeButton)
    Col(align=Left, bg = Brushes.transparent)(
      closeButton,
      GUI//.enlargedTo(closeButton.w, guiRoot.h, bg = Brushes.white)
    ).framed(bg = Brushes.white)
  }

  val MAGIC= Vec(20,20) // I've no idea why this offset is needed, and no time to find out

  /** set the location of this dialogue relative to `glyph`  */
  def North(glyph: Glyph): this.type = { location = Location.NorthFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def NorthEast(glyph: Glyph): this.type = { location = Location.NorthEastFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def East(glyph: Glyph): this.type = { location = Location.EastFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def SouthEast(glyph: Glyph): this.type = { location = Location.SouthEast(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def South(glyph: Glyph): this.type = { location = Location.SouthFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def SouthWest(glyph: Glyph): this.type = { location = Location.SouthWestFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def West(glyph: Glyph): this.type = { location = Location.WestFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def NorthWest(glyph: Glyph): this.type = { location = Location.NorthWestFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to the root of `glyph`  */
  def InFront(glyph: Glyph): this.type = { location = Location.InFrontFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to the root of `glyph`  */
  def AtTop(glyph: Glyph): this.type = { location = Location.AtTopFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to the root of `glyph`  */
  def AtBottom(glyph: Glyph): this.type = { location = Location.AtBottomFor(overlayRoot)(glyph)(MAGIC); thisPopup }
  /** set the location of this dialogue relative to the root of `glyph`  */
  def OnRootOf(glyph: Glyph): this.type = {
    val loc = glyph.rootDistance
    location = Location.OnRootOf(glyph)(5, 5)
    thisPopup
  }

  /**
   * Allocate an Overlay layer and continue the interaction. The overlayRoot is constructed exactly one.
   */
  def start(): Unit = {
    assert(location ne null, "Dialogue must have defined a non-null location before starting")
    val RelativeTo(glyph, offset) = location
    // TODO: eliminate magic offset: without it the dialogues are mislocated
    val MAGIC = closeGlyph match {
      case None => Vec.Zero
      case Some(glyph) => glyph.diagonal
    } //WAS Vec(11f, 14f)
    val nudgeX = -15
    val nudgeY = -15
    var requested = glyph.rootDistance + offset + MAGIC
    // Nudge the request so the glyph is (mostly) visible in the actual window
    //    while (requested.x<0) requested -= Vec(requested.x, 0)
    //    while (requested.y<0) requested -= Vec(0, requested.y)
    //while (requested.x + overlayRoot.w > glyph.guiRoot.W) requested -= Vec(2, 0)
    //while (requested.y + overlayRoot.h > glyph.guiRoot.H) requested -= Vec(0, 2)
    overlayRoot.location = requested
    // NUDGE
    val Vec(x, y) = glyph.guiRoot.diagonal-overlayRoot.diagonal-(5,5) // limit
    overlayRoot.location =
      Vec(x min (overlayRoot.location.x+nudgeX) max 0,
          y min (overlayRoot.location.y+nudgeY) max 0)
    // END NUDGE
    glyph.guiRoot.giveupFocus()
    glyph.guiRoot.Overlay.pushLayer(overlayRoot, isModal, isMenu=isMenu, offMenuClick={ () => close() })
    if (theCloseButton.isDefined) glyph.guiRoot.grabKeyboard(theCloseButton.get)
    overlayRoot.parent = glyph.guiRoot
    navigation.Init()
  }


  private var _onClose: Option[T => Unit] = None

  /** Declare that `continuation(t)` should run just after `this.close(t)`. */
  def onClose(continuation: T => Unit): this.type = {
    _onClose = Some(continuation)
    this
  }

  /** Declare that `continuation(t)` should run just after `this.close(t)`; then start this popup. */
  def andThen(continuation: T => Unit): Unit = {
    _onClose = Some(continuation)
    start()
  }

}
