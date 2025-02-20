package org.sufrin.glyph
package tests.portmanteau

import sheeted.TextButton
import sheeted.Label
import sheeted.windowdialogues.Dialogue
import Dialogue.{CHOOSE,OK}
import NaturalSize.{Col, Row}
import sheeted.Book
import sheeted.BookSheet
import Glyphs.{INVISIBLE, Rect}


class ButtonStyles (implicit val style: BookSheet, implicit val translation: glyphXML.Translation) {
  implicit val styleSheet: Sheet = style.pageSheet
  import translation._
  import styleSheet.{ex, em}
  import DefaultBrushes.{red,blue,lightGrey,darkGrey,green,yellowHuge}
  val book = Book()
  val Page = book.DefinePage

  Page("Framed", "") {
    import Styles.Decoration.{Framed,Unframed}
    import sheeted.TextButton
    Col.centered(
      <div width="55em">
        <p> These buttons are of the form:</p>
        <![CDATA[
        TextButton("Framed(width=..., radiusFactor=...)") { _ => }(
            styleSheet.copy(buttonBackgroundBrush=lightGrey,
                            buttonFrame =
                              Framed(fg=darkGrey(width=...),
                                     bg=lightGrey, enlarge=0.15f, radiusFactor = ...)))]]>
        <fill/>
      </div>.enlarged(20), ex,
      TextButton("Framed(width=4, radiusFactor=0.35)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width=4), lightGrey, enlarge=0.15f, radiusFactor = 0.35f))), ex,
      TextButton("Framed(width=8, radiusFactor=0.35)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width=8), lightGrey, enlarge=0.15f, radiusFactor = 0.35f))), ex,
      TextButton("Framed(width=10, radiusFactor=0.35)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width=10), lightGrey, enlarge=0.15f, radiusFactor = 0.35f))), ex,
      TextButton("Framed(width=2, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width = 2), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex,
      TextButton("Framed(width=4, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width = 4), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex,
      TextButton("Framed(width=8, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width = 8), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex,
      TextButton("Framed(width=10, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Framed(darkGrey(width = 10), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex, ex,
      <p width="55em" align="center">A hand-styled button</p>, ex,
      <div><![CDATA[
        TextButton("Hand-framed")  { _ => }
                (styleSheet.copy(
                        buttonBackgroundBrush=lightGrey,
                        buttonFrame          = Unframed)).framed(bg=lightGrey,
                                                                 fg=yellow(width=8))]]></div>, ex, ex,
        TextButton("Hand Styled")  { _ => }
                (styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonFrame = Unframed)).framed(bg=lightGrey, fg=yellowHuge(width=8)), ex,
      <p width="55em" align="center"><tt>styleSheet</tt> is the implicit style <tt>Sheet</tt> for this <tt>Book.Page</tt></p>
    )
  }

  Page("Blurred"){
    import Styles.Decoration.Blurred
    def blurFrame(blur: Float, spread: Float) = Blurred(fg=red, blur=blur, spread=spread)
    Col.centered(
    <div width="55em">
      <p> These buttons are of the form:
      </p>
      <![CDATA[
        TextButton("Blurred(blur=..., spread=...)") { _ => }(
            styleSheet.copy(buttonBackgroundBrush=red,
                            buttonFrame =
                              Blurred(fg=red,
                                      blur=..., spread=...)))]]>
    </div>.enlarged(20), ex,
      TextButton("Blurred(blur=10f, spread=5f)") { _ => }(styleSheet.copy(buttonBackgroundBrush=red, buttonFrame=blurFrame(10f, 5f))), ex,
      TextButton("Blurred(blur=10f, spread=10f)"){ _ => }(styleSheet.copy(buttonBackgroundBrush=red, buttonFrame=blurFrame(10f, 10f))), ex,
      TextButton("Blurred(blur=20f, spread=10f)"){ _ => }(styleSheet.copy(buttonBackgroundBrush=red, buttonFrame=blurFrame(20f, 10f))), ex,
      <p width="55em" align="center"><tt>styleSheet</tt> is the implicit style <tt>Sheet</tt> for this <tt>Book.Page</tt></p>
    )
  }

  Page("Shaded", "") {
    import Styles.Decoration.Shaded
    Col.centered(
      <div width="55em">
        <p> These buttons are of the form:</p>
        <![CDATA[
        TextButton("Shaded(delta=..., down=...)") { _ => }(
            styleSheet.copy(buttonFrame =
                              Shaded(fg=darkGrey,
                                     bg=lightGrey, delta=..., down=...)))]]>
      </div>.enlarged(20), ex,
      TextButton("Shaded  (18,  down=true)") { _ => }(styleSheet.copy(buttonFrame = Shaded(delta = 18f, down = true))), ex,
      TextButton("Shaded  (8,  down=false)") { _ => }(styleSheet.copy(buttonFrame = Shaded(delta = 8f, down = false))), ex,
      TextButton("Shaded  (12, down=false)") { _ => }(styleSheet.copy(buttonFrame = Shaded(delta = 12f, down = false))), ex,
      TextButton("Shaded  (18, down=false)") { _ => }(styleSheet.copy(buttonFrame = Shaded(delta = 18f, down = false))), ex, ex,
      <p width="55em" align="center"><tt>styleSheet</tt> is the implicit style <tt>Sheet</tt> for this <tt>Book.Page</tt></p>, ex, ex,


        <div width="55em" >
        <p>These glyphs are of the form</p>
          <![CDATA[
          Label("Label (delta=..., enlarge=..., down=...)")
             .shaded(delta = ..., enlarge=..., down = ...)]]>
      </div>, ex, ex,
      Label("Label (delta=8, enlarge=0f   down=false)").shaded(enlarge=0f, delta = 8f, down = false), ex,
      Label("Label (delta=8, enlarge=10f   down=false)").shaded(enlarge=10f, delta = 8f, down = false), ex,
      Label("Label (delta=8, enlarge=25f, down=false)").shaded(enlarge=25f, delta = 8f, down = false)
    )
  }


  val GUI: Glyph = book.Layout.rightButtons()
}
