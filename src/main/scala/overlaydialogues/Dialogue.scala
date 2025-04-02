package org.sufrin.glyph
package overlaydialogues


import Location._
import NaturalSize.{Col, Row}
import unstyled.{reactive, static}

import io.github.humbleui.jwm.{App, EventMouseScroll}


/**
 * Overlay variant of `Dialogue`
 */
object Dialogue {

  import PolygonLibrary.{closeButtonGlyph => defaultCloseGlyph}


  /**
   *  A generic overlaydialogues "choice" popup, located at the given `position`. It can be popped down without using
   *  any of the buttons on its bottom row, by hitting the kill button placed on its top row.
   *
   *  TODO: unbundle first Dialogue parameter
   */
  def POPUP[T](guiRoot: Glyph, bottomRow: Seq[Glyph]): Dialogue[T] =
    new Dialogue[T](Col(align=Center)(guiRoot, Row(Top)(bottomRow)), closeGlyph = Some(defaultCloseGlyph))

  import reactive.GenericButton

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


  /**  Yield a menu button in the given implicit style that activates a pop-up menu on which are the glyphs `buttons`.
   *   An activated menu is modal: it takes the entire application focus while it is up; and any nested menus popped up by it will
   *   (likewise) take the entire application focus from the menu from which they were activated. The `buttons` that are active will behave as expected when clicked,
   *   then cause the whole menu to be dismissed, and focus to revert to an appropriate glyph or to become uncommitted.
   *
   *   A menu popup can be dismissed by clicking on its
   *   close button/close bar or using the ESC key or
   *   by clicking somewhere outside the popup.
   *
   *   Every attempt is made to accomodate the popup in a sensible place (to the right of the menu button), or at least in
   *   a place where it is completely visible (on its window). But if it has popped up in an inconvenient place, it can be moved using the
   *   HOME and END keys, and the mousewheel. The first moves the popup north-west by its own dimension;
   *   the second moves it south-east by its own dimension. The wheel moves it incrementally in the direction indicated.
   *
   */
  def Menu(name: String, nested: Boolean=false)(button: Glyph, buttons: Glyph*)(implicit sheet: StyleSheet): Glyph = {
     Menu$(name, nested)(button :: buttons.toList)
  }

  def NestedMenu(name: String)(button: Glyph, buttons: Glyph*)(implicit sheet: StyleSheet): Glyph = {
    Menu$(name, nested = true)(button :: buttons.toList)
  }

  /**  Yield a menu button in the given button style that is bound to a menu on which are the glyphs `buttons`.
   *
   *   @see Menu
   */
  def Menu$(name: String, nested: Boolean)(buttons: Seq[Glyph])(implicit sheet: StyleSheet): Glyph = {
    lazy val popDowns: Seq[Glyph] = buttons.map { button => afterReact(button) { popup.close() }}
    lazy val width   = popDowns.map(_.w).max
    lazy val uniform = popDowns.map  {
      glyph =>
        if (glyph.isMenuButton)
           glyph.enlargedTo(width, glyph.h)
        else
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
      new Dialogue[Unit](Col(align=Center, bg=sheet.menuStyle.bg)(uniform), East(button), Some(defaultCloseGlyph), isMenu = true) {
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

    lazy val button: Glyph =
      if (nested)
        styled.MenuButton(name) (reaction) (sheet.copy(buttonDecoration=styles.decoration.unDecorated))
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

  /** A modal, closeable popup `Dialogue[String]` that shows the `blurb`, and offers each of the given choices on a button. Its
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
  def CHOICE[T](blurb: Glyph)(choices: (T, Glyph)*): Dialogue[T] = {
    lazy val buttons = choices.map {
      case (t, g) => reactive.RawButton(g(), g(), g()) { _ => popup.close(t) }.framed().enlarged(20)
    }
    lazy val popup: Dialogue[T] = POPUP(blurb, buttons)
    popup
  }
}

/**
 * A popup implemented as an `overlaydialogues`` layer that will be
 * rendered on the window referred to by `location`.
 *
 * @param guiRoot the glyph to be GUI for the dialogue when it is started
 *
 * @param location the location at which the top-left corner of the glyph is to be shown -- typically set non-null post-construction by (eg) `North(glyph)`
 *
 * @param closeGlyph If the popup is to have a close button showing `g` in its top left corner, then `Some(g)`
 *
 * @param isModal When true (the default case) this means that the popup grabs all input from its host window from the moment it is started until (just after) it is `close`d.
 *
 * @param isMenu When true this dialogue represents a popup menu
 *
 * @tparam T the type of value passed to the continuation (if any) by invoking `close`
 *
 */
class Dialogue[T](guiRoot:        Glyph,
                  var location:   RelativeTo = null,
                  val closeGlyph: Option[Glyph] = None,
                  var isModal:    Boolean = true,
                  var isMenu:     Boolean = false)
{
  thisPopup =>


  import NaturalSize.Col

  import reactive.RawButton

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
      static.Concentric(rowAlign=Mid, colAlign=Center)(
         static.FilledRect(guiRoot.w, glyph.h*1.2f, fg=Brushes.lightGrey, bg=Brushes.lightGrey), // TODO: 5f is a magic number
        glyph,
      )
    case None        =>
      static.FilledRect(guiRoot.w-2, 5f, fg=Brushes.lightGrey, bg=Brushes.lightGrey)
  }

  /**
   * The `guiRoot` decorated with a close button/bar: this is the effective GUI root of the overlay that the dialogue will inhabit.
   */
  lazy val overlayRoot: Glyph = {
        // set up the killbutton to (also) respond to ESCAPE with a `close()`
        val closeButton = new RawButton(closeButtonAppearance(),
                                        closeButtonAppearance(),
                                        closeButtonAppearance(), closeButtonAppearance.fg, closeButtonAppearance.bg,
                                        { _: Modifiers.Bitmap  => close() }
        )
        {
          import io.github.humbleui.jwm.{EventKey, Window}
          import io.github.humbleui.jwm.Key._

          override def extra: Vec = Vec.Zero

          override def accept(key: EventKey, location: Vec, window: Window): Unit = {
            //println(s"closeButton $key")
             key.getKey match {
               case ESCAPE  if isModal && !key.isPressed => close()
               case UP      if isModal && !key.isPressed =>
                 overlayRoot.location = overlayRoot.location + (0f, -15f)
                 overlayRoot.reDraw()
               case DOWN    if isModal && !key.isPressed =>
                 overlayRoot.location = overlayRoot.location + (0f, 15f)
                 overlayRoot.reDraw()
               case RIGHT   if isModal && !key.isPressed =>
                 overlayRoot.location = overlayRoot.location + (15f, 0f)
                 overlayRoot.reDraw()
               case LEFT    if isModal && !key.isPressed =>
                 overlayRoot.location = overlayRoot.location + (-15f, 0f)
                 overlayRoot.reDraw()
               case HOME    if isModal && !key.isPressed =>
                 overlayRoot.location = Vec.Zero // overlayRoot.location - overlayRoot.diagonal
                 overlayRoot.reDraw()
               case END     if isModal && !key.isPressed =>
                 overlayRoot.location = overlayRoot.location + overlayRoot.diagonal
                 overlayRoot.reDraw()
               case other => super.accept(key, location, window)
             }
          }

          override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = {
              val dx = event._deltaX
              val dy = event._deltaY
              if (isModal) overlayRoot.location = overlayRoot.location + (dx, dy); overlayRoot.reDraw()
          }

        }
        theCloseButton = Some(closeButton)
        Col(align=Left, bg = Brushes.transparent) (
            closeButton,
            guiRoot//.enlargedTo(closeButton.w, guiRoot.h, bg = Brushes.white)
        ).framed(bg = Brushes.white)
    }

  /** set the location of this dialogue relative to `glyph`  */
  def North(glyph: Glyph): this.type = { location = Location.NorthFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def NorthEast(glyph: Glyph): this.type = { location = Location.NorthEastFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def East(glyph: Glyph): this.type = { location = Location.EastFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def SouthEast(glyph: Glyph): this.type = { location = Location.SouthEast(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def South(glyph: Glyph): this.type = { location = Location.SouthFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def SouthWest(glyph: Glyph): this.type = { location = Location.SouthWestFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def West(glyph: Glyph): this.type = { location = Location.WestFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to `glyph`  */
  def NorthWest(glyph: Glyph): this.type = { location = Location.NorthWestFor(overlayRoot)(glyph); thisPopup }
  /** set the location of this dialogue relative to the root of `glyph`  */
  def InFront(glyph: Glyph): this.type = {
    val loc = glyph.rootDistance
    location = Location.OnRootOf(glyph)(loc.x + (glyph.w - overlayRoot.diagonal.x)/2f, loc.y + (glyph.h - overlayRoot.diagonal.y)/2f)
    thisPopup
  }

  def OnRootOf(glyph: Glyph): this.type = {
    val loc = glyph.rootDistance
    location = Location.OnRootOf(glyph)(5, 5)
    thisPopup
  }

  /**
   * Allocate an Overlay layer and continue the interaction.
   */
  def start(): Unit = {
    assert(location ne null, "Dialogue must have defined a non-null location before starting")
    val RelativeTo(glyph, offset) = location
        // TODO: eliminate magic offset: without it the dialogues are mislocated
        val MAGIC = Vec(11f, 14f)
        var requested = glyph.rootDistance + offset + MAGIC
        // Nudge the request so the glyph is (mostly) visible in the actual window
        while (requested.x<0) requested -= Vec(requested.x, 0)
        while (requested.y<0) requested -= Vec(0, requested.y)
        while (requested.x + overlayRoot.w > glyph.guiRoot.W) requested -= Vec(10f, 0)
        while (requested.y + overlayRoot.h > glyph.guiRoot.H) requested -= Vec(0, 10f)
        overlayRoot.location = requested
        glyph.guiRoot.giveupFocus()
        glyph.guiRoot.Overlay.pushLayer(overlayRoot, isModal, isMenu=isMenu, offMenuClick={ () => close() })
        if (theCloseButton.isDefined) glyph.guiRoot.grabKeyboard(theCloseButton.get)
        overlayRoot.parent = glyph.guiRoot
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
