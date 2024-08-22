package org.sufrin.glyph
package tests

import Styles.NotebookStyle
import styled.{text, CheckBox}

import org.sufrin.utility.TextAbbreviations

class PortmanteauText(implicit style: StyleSheet) extends Notebook {
  implicit val pageStyle: NotebookStyle = style.notebookStyle
  val anchor = style.Spaces.ex
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

  val textField: TextField = TextField(size = 40, onEnter = { _ =>  }, onCursorLeave = { _ => anchor.guiRoot.giveupFocus() }, abbreviations = abbrev)
  val GUI: Glyph = NaturalSize.Col.centered(
    anchor,
    text.Paragraphs(50, Justify)(
      """
        |This is an example of a TextField that has been set up by mapping a few abbreviations to emojis,
        |namely: (c) (r) :) :O <3 :-| :|
        |
        | When "Live abbreviations" is set, typing an abbreviation results in the insertion of the
        |unicode sequence it abbreviates. At any other time, typing the same shift key twice
        |in succession has the same result. The machinery is straightforward, and is intended to be used
        |in text editors and other text components to make it easy for users to generate characters
        |that aren't natively available on their input device. [see Input Method@Wikipedia]
        |
        | Some of the emojis are also mapped back to their original abbreviations: something you can check
        |by using the "any-shift-key-twice" method.
        |
        |""".stripMargin),
    text.Label("Log events") beside CheckBox(initially=false) {
      state => anchor.guiRoot.eventHandler.logEvents=state
    } beside
    text.Label(" Live abbreviations") beside CheckBox(initially=abbrev.onLineTrigger) {
      state => abbrev.onLineTrigger=state
    },
    textField.framed()
  )
}
