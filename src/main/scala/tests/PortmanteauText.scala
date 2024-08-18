package org.sufrin.glyph
package tests

import Styles.NotebookStyle
import styled.{text, CheckBox, TextButton}

import io.github.humbleui.jwm.{App, EventTextInputMarked}

class PortmanteauText(implicit style: StyleSheet) extends Notebook {
  implicit val pageStyle: NotebookStyle = style.notebookStyle
  val anchor = style.Spaces.ex
  val textField: TextField = TextField(size = 40, onEnter = { _ =>  }, onCursorLeave = { _ => /*anchor.guiRoot.giveupFocus()*/ })
  val GUI: Glyph = NaturalSize.Col.centered(
    anchor,
    text.Label("Log events: ") beside CheckBox(initially=false) {
      state => anchor.guiRoot.eventHandler.logEvents=state
    },
    TextButton("X"){
      _ =>
        val ex = textField.TextModel.leftString
          //for { _<- 1 to 3} textField.TextModel.del()
          //textField.TextModel.ins("Bar")
        App.runOnUIThread(()=>anchor.guiRoot.eventHandler.accept(new EventTextInputMarked("X", 1, 1, -1, -1)))
        //App.runOnUIThread(()=>anchor.guiRoot.eventHandler.accept(new EventTextInput("Bar\uD83D\uDE00", -1, -1)))
    },
    textField.framed()
  )
}
