package org.sufrin.glyph

/**
 * The state of the keyboard modifiers, mouse buttons, keyboard location, etc. at relevant events
 * can be encoded as a single integer (`Modifiers.toBitMap`). This module maps events to such
 * bitmaps. This more uniform encoding makes it fairly straightforward to use tabular mappings
 * of user-initiated events to actions.
 *
 * Examples:
 *
 * The primary mouse button is pressed with the shift key down. Other shift keys may
 * be present; and other buttons may be down.
 * {{{
 *   Modifiers(event).includes(Shift | Primary | Pressed)
 * }}}
 *
 * The primary mouse button is pressed with the command and shift keys down.
 * {{{
 *   Modifiers(event).are(Command | Shift | Primary | Pressed)
 * }}}
 * The primary mouse button is pressed with the command and shift keys down, or the
 * alt and shift keys down
 * {{{
 *   Modifiers(event).are(Command | Shift | Primary | Pressed) ||
 *   Modifiers(event).are(Alt | Shift | Primary | Pressed)
 * }}}
 * A key or mousebutton is released
 * {{{
 *   Modifiers(event).are(Released)
 * }}}
 */
object Modifiers {
  import io.github.humbleui.jwm._
  import io.github.humbleui.jwm.KeyLocation._
  import io.github.humbleui.jwm.KeyModifier._

  def apply(event: Event): Int = event match {
    case key: EventKey =>
      key._modifiers | (if (key.isPressed) Pressed else Released) |
      (key._location match {
        case KEYPAD => Keypad
        case DEFAULT => Default
        case RIGHT => Right
      })
    case mouse: GlyphEnter => mouse.modifiers
    case mouse: GlyphLeave => mouse.modifiers
    case mouse: EventMouseMove =>
      import io.github.humbleui.jwm.MouseButton._
      var buttons = 0
      if ((mouse._buttons & PRIMARY._mask)   !=0) buttons |= Primary
      if ((mouse._buttons & SECONDARY._mask) !=0) buttons |= Secondary
      if ((mouse._buttons & MIDDLE._mask)    !=0) buttons |= Middle
      if ((mouse._buttons & BACK._mask)      !=0) buttons |= Back
      if ((mouse._buttons & FORWARD._mask)   !=0) buttons |= Forward
      if (buttons!=0) buttons |= Pressed
      mouse._modifiers | buttons
    case mouse: EventMouseScroll =>
         mouse._modifiers
    case mouse: EventMouseButton =>
      import io.github.humbleui.jwm.MouseButton._
      mouse._modifiers | (if (mouse.isPressed) Pressed else Released) |
        (mouse._button match {
            case PRIMARY => Primary
            case SECONDARY => Secondary
            case MIDDLE => Middle
            case BACK => Back
            case FORWARD => Forward
      })

    case _  => 0
  }

  val empty: Bitmap = Bitmap(0)

  implicit def BitmapOfEvent(event: Event) : Bitmap = Bitmap(Modifiers(event))

  implicit class Bitmap(val modifiers: Int) extends AnyVal {
        override def toString: String =  toShortString

        def toShortString: String = {
          var s = ""
          for {shift <- 0 until modString.size} {
            val bit: Int = 1 << shift
            if ((bit & modifiers) != 0) s = s"$s ${shortModString(shift)}"
          }
          s
        }

        def toLongString: String = {
          var s = ""
          for {shift <- 0 until modString.size} {
            val bit: Int = 1 << shift
            if ((bit & modifiers) != 0) s = s"$s ${modString(shift)}"
          }
          s++ f" (0x${modifiers}%04x)"
        }


    @inline def includeAll(mask: Int): Boolean = mask == (modifiers & mask)
    @inline def includeSome(mask: Int): Boolean = 0 != (modifiers & mask)
    @inline def exclude(mask: Int): Boolean = 0 == (modifiers & mask)
    @inline def are(mask: Int): Boolean = modifiers == mask
    @inline def any: Boolean = modifiers  != 0
  }

    def toBitmap(event: Event): Bitmap = Modifiers.apply(event)



  /** Modifier bits with the same encoding in Skija event.*/
  val CapsLock: Int = CAPS_LOCK._mask
  val Shift: Int = SHIFT._mask
  val Control: Int = CONTROL._mask
  val Alt: Int = ALT._mask
  val Windows: Int = WIN_LOGO._mask
  val Meta: Int = LINUX_META._mask
  val Super: Int = LINUX_SUPER._mask
  val Command: Int = MAC_COMMAND._mask
  val Option: Int = MAC_OPTION._mask
  /**
   * Mac Keyboards report Fn for some commonly-provided keys
   * such as HOME, END, PAGEUP, ... It is probably not advisable
   * to use this bit semantically.
   */
  val Fn: Int = MAC_FN._mask

  /**
   * Synthetic modifiers encoding what happened and/or where it happened
   */
  val Released: Int  = Fn << 1
  val Pressed: Int   = Released << 1
  val Default: Int   = Pressed << 1
  val Keypad: Int    = Default << 1
  val Right: Int     = Keypad  << 1

  /**
   * Synthetic modifiers encoding the states of mouse buttons
   */
  val Primary: Int  = Right<<1
  val Secondary: Int = Primary << 1
  val Middle: Int = Secondary << 1
  val Back: Int = Middle << 1
  val Forward: Int = Back << 1

  /** Maps `shift` to the name of the modifier encoded as `(1<<shift)` */
  val modString: Array[String] =
    """CapsLock
      |Shift
      |Control
      |Alt
      |Windows
      |Meta
      |Super
      |Command
      |Option
      |Fn
      |Released
      |Pressed
      |Default
      |KeyPad
      |Right
      |Primary
      |Secondary
      |Middle
      |Back
      |Forward""".stripMargin.split('\n')

  val shortModString:  Array[String] =
    """Lock
      |Shi
      |Ctrl
      |Alt
      |Win
      |Meta
      |Supr
      |Cmd
      |Opt
      |Fn
      |Rel
      |Prs
      |Dflt
      |Kpad
      |Rght
      |Pri
      |Sec
      |Mid
      |Back
      |Frwd""".stripMargin.split('\n')
}


