package org.sufrin.glyph
package tests


import org.sufrin.utility.TextAbbreviations
import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.styled.{text, CheckBox}

import scala.xml.Elem
import sheeted._


class PortmanteauText(implicit style: Sheet) extends Notebook {
  implicit val pageStyle: BookStyle = BookStyle(style, style)
  val anchor = Glyphs.INVISIBLE()
  val abbrev = new TextAbbreviations(onLineTrigger = true)
  abbrev("(c)") = "\u00A9"
  abbrev("\u00A9") = "(c)"
  abbrev("(r)") = "\u00AE"
  abbrev("<3")  = "❤"
  abbrev("❤")  =  "<3"
  // translations to strings including surrogate pairs
  abbrev(":-|") = "\uD83D\uDE10"
  abbrev(":|")  = "\uD83D\uDE11"
  abbrev(":)")  = "\uD83D\uDE00"
  abbrev(":O")  = "\uD83D\uDE2E"
  // translations from strings including surrogate pairs
  abbrev("\uD83D\uDE11")   = ":|"   // unabbreviate
  abbrev("\uD83D\uDE10")   = ":-|"  // unabbreviate
  abbrev("\uD83D\uDE00)")  = "\uD83D\uDE00\uD83D\uDE00" // double-up a smiley
  abbrev("\uD83D\uDE00\uD83D\uDE00") = ":))"
  abbrev("\uD83D\uDE2E") = ":O"


  import sheeted._
  import glyphXML.Language._
  val defs = translation.meaning

  defs("CONTROLS") =
    _=>Label("Log events") beside sheeted.CheckBox(initially=false) {
      state => anchor.guiRoot.eventHandler.logEvents=state
    } beside Label(" Live abbreviations") beside sheeted.CheckBox(initially=abbrev.onLineTrigger) {
      state => abbrev.onLineTrigger=state
    }

  val textField: TextField = sheeted.TextField(size = 40, onEnter = { _ =>  }, onCursorLeave = { _ => anchor.guiRoot.giveupFocus() }, abbreviations = abbrev)
  defs("TEXTFIELD") = _=>textField.framed()


  val GUI: Glyph = NaturalSize.Col.centered(
    anchor,
    <body width="60em" align="justify" fg="blue" parSkip="0.75em">
      <p>
        This is an example of a TextField that has been set up by mapping a few abbreviations to emojis,
        namely:
      </p>
      <fill/>
      <p align="center" bg="nothing" fontFamily="Courier">
        <![CDATA[(c) (r) :) :O <3 :-| :|]]>
      </p>
      <fill/>
      <p align="center">
        <glyph gid="TEXTFIELD"/>
        <glyph gid="CONTROLS"/>

      </p>
      <fill/>
      <p>
        When "Live abbreviations" is set, typing an abbreviation results in the insertion of the
        unicode sequence it abbreviates. At any other time, typing the same shift key twice
        in succession has the same result. The machinery is straightforward, and is intended to be used
        in text editors and other text components to make it easy for users to generate characters
        that aren't natively available on their input device. [see Input Method@Wikipedia]
      </p>
      <fill/>
      <p>
        Some of the emojis are also mapped back to their original abbreviations: something you can check
        by using the "any-shift-key-twice" method.
      </p>
      </body>,
  )
}
