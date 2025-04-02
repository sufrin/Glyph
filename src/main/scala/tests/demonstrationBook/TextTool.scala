package org.sufrin.glyph
package tests.demonstrationBook


import styled._
import unstyled.static

import org.sufrin.utility.TextAbbreviations
import org.sufrin.SourceLocation.SourceLocation


class TextTool(implicit style: StyleSheet)  {
  implicit val pageStyle: BookSheet = BookSheet(style, style)
  val anchor = static.INVISIBLE()
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


  import glyphXML.Language._
  import styled._
  def SOURCE(implicit source: SourceLocation): String = source.toString()

  val defs = translation.meaning

  defs("CONTROLS") =
    _=>Label("Log events") beside styled.CheckBox(initially=false) {
      state => anchor.guiRoot.eventHandler.logEvents=state
    } beside Label(" Live abbreviations") beside styled.CheckBox(initially=abbrev.onLineTrigger) {
      state => abbrev.onLineTrigger=state
    }

  val textField: TextField = styled.TextField(size = 40, onEnter = { _ =>  }, onCursorLeave = { _ => anchor.guiRoot.giveupFocus() }, abbreviations = abbrev)(style.copy(fontScale = 1.5f))
  defs("TEXTFIELD") = _=>textField.framed()


  val GUI: Glyph = NaturalSize.Col(align=Center)(
    anchor,
    <body width="60em" align="justify" fg="blue" parSkip="0.75em" itemWidth="60em" source={SOURCE}>
      <p>
        This is an example of a TextField that has been set up by mapping a few abbreviations to emojis,
        namely:
      </p>
      <fill/>
      <p align="center" bg="transparent" fontFamily="Courier" source={SOURCE}>
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
      <fill/>
      <itemize itemIndent="5em" logging="false">
          <item>The cursor is always kept in view</item>
        <item>Visual cues are given for out-of-field text</item>
        <item>The usual cut, copy, paste, and navigate keys are provided:</item>
        <itemize itemIndent="10em" hang="+">
              <item>Ctrl/Cmd C - copy all</item>
              <item>Ctrl/Cmd X - cut all to clipboard</item>
              <item>Ctrl/Cmd V - insert from clipboard</item>
              <item>Home, End, Left, Right, and Backspace - their usual effect</item>
        </itemize>
      </itemize>
      </body>
  )
}
