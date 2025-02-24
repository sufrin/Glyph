package org.sufrin.glyph
package tests.demonstrationBook

import styled.TextButton
import styled.Label
import styled.windowdialogues.Dialogue

import Dialogue.{CHOOSE, OK}
import NaturalSize.{Col, Row}
import styled.Book
import styled.BookSheet
import Glyphs.{INVISIBLE, Rect}

import org.sufrin.glyph.DefaultBrushes.ROUND
import org.sufrin.glyph.ReactiveGlyphs.Reaction


class ButtonStyles (implicit val style: BookSheet, implicit val translation: glyphXML.Translation) {
  implicit val styleSheet: StyleSheet = style.pageSheet
  import translation._
  import styleSheet.{ex, em, hSpace, vSpace}
  import DefaultBrushes.{red,blue,lightGrey,darkGrey,green,yellowHuge}
  val book = Book()
  val Page = book.DefinePage
  val anchor = INVISIBLE()

  /** A reaction used below that pops up a Dialogue on which `glyph` is shown. */
  def showSourceCode(source: Glyph): Reaction = {
    _ => styled.windowdialogues.Dialogue.OK(source.enlarged(20).edged().enlarged(20)).East(anchor).start()
  }

  locally {
    translation("CENTERCODE") = new glyphXML.Macro(
      <div width="$width(75em)" textFontFamily="Courier" ><row width="1*width"><fill/>&BODY;<fill/></row></div>
    )

    translation("SOURCECODE") = new glyphXML.Macro(
      <div width="$width(75em)">
        <CENTERCODE width="$width">
          &BODY0;
        </CENTERCODE>
        <fill/>
        &BODY1..;
      </div>
    )
  }

  Page("Framed", "") {
    import styles.decoration.{Framed,unDecorated, Edged, Decorated}
    import styled.TextButton
    Col.centered(
      anchor,
      <div width="75em">
        <p>The <b>Framed</b> buttons below were constructed with implicit styles that included specifications of rim width and radius:</p>

        <CENTERCODE width="75em">
        <![CDATA[
            buttonBackgroundBrush=lightGrey,
            buttonFrame = Framed(fg=darkGrey(width=...),
                                 bg=lightGrey, enlarge=0.15f, radiusFactor = ...)))]]>
        </CENTERCODE>
        <fill/>
      </div>.enlarged(20), vSpace(),
      TextButton("Framed(width=4, radiusFactor=0.35)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width=4), lightGrey, enlarge=0.15f, radiusFactor = 0.35f))), ex,
      TextButton("Framed(width=8, radiusFactor=0.35)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width=8), lightGrey, enlarge=0.15f, radiusFactor = 0.35f))), ex,
      TextButton("Framed(width=10, radiusFactor=0.35)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width=10), lightGrey, enlarge=0.15f, radiusFactor = 0.35f))), ex,
      TextButton("Framed(width=2, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width = 2), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex,
      TextButton("Framed(width=4, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width = 4), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex,
      TextButton("Framed(width=8, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width = 8), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex,
      TextButton("Framed(width=10, radiusFactor=0.25)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Framed(darkGrey(width = 10), lightGrey, enlarge = 0.15f, radiusFactor = 0.25f))), ex, ex,
      TextButton("Edged(width=10)") { _ => }(styleSheet.copy(buttonBackgroundBrush=lightGrey, buttonDecoration = Edged(darkGrey(width = 10, cap=ROUND), lightGrey, enlarge = 0.15f, radiusFactor = 0f))), ex, ex,
      <p width="75em" align="center">Buttons framed in a variety of ways (click to see their source)</p>,
      vSpace(2),
        TextButton("Button #1")
        { showSourceCode(
          <SOURCECODE width="75em" DEC="Undecorated">
      <![CDATA[
       TextButton("Button #1")
       { _=> ... }(styleSheet.copy(
          buttonBackgroundBrush = lightGrey,
          buttonDecoration      = Undecorated
         )
         .enlarged(10)
         .edged(bg=lightGrey,
                fg=brown(width=10, cap=ROUND).dashed(20,10))
     ]]>
    </SOURCECODE>
        ) }
        (styleSheet.copy(buttonDecoration = unDecorated, buttonBackgroundBrush=lightGrey))
        .enlarged(10).edged(fg=DefaultBrushes.brown(width=10, cap=ROUND).dashed(20,10))

      beside hSpace(4) beside

          TextButton("Button #2")
        { showSourceCode(
         <SOURCECODE width="75em" DEC="Undecorated">
    <![CDATA[
     TextButton("Button #2")
     { _=> ... }(styleSheet.copy(
        buttonBackgroundBrush = lightGrey,
        buttonDecoration      = unDecorated)
       )
       .enlarged(20)
       .edged(bg=lightGrey,
              fg=.red(width=20, cap=ROUND).dashed(20,20))
   ]]>
         </SOURCECODE>
        ) }
        (styleSheet.copy(buttonBackgroundBrush=lightGrey,
                         buttonDecoration = unDecorated))
        .enlarged(20, bg=lightGrey).edged(bg=lightGrey, fg=DefaultBrushes.red(width=20, cap=ROUND).dashed(20,20))
      beside hSpace(4) beside
      TextButton("Button #3")
      { showSourceCode(
        <SOURCECODE width="75em"  DEC="Decorated">
    <![CDATA[
       TextButton("Show source #3")
       { _=> ... }
        (styleSheet.copy(buttonBackgroundBrush=lightGrey.rounded(5),
                         buttonDecoration = Decorated
                         { glyph => glyph.enlarged(20)
                                         .edged(bg=lightGrey,
                                                fg=red(width=20, cap=ROUND)
                                                      .dashed(20,20)) }]]>
        </SOURCECODE>
      ) }
      (styleSheet.copy(buttonBackgroundBrush=lightGrey.rounded(5),
                       buttonDecoration = Decorated
                        { glyph => glyph.enlarged(20).edged(bg=lightGrey, fg=DefaultBrushes.red(width=20, cap=ROUND).dashed(20,20)) }))
    )
  }

  Page("Blurred"){
    import styles.decoration.Blurred
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
      TextButton("Blurred(blur=10f, spread=5f)") { _ => }(styleSheet.copy(buttonBackgroundBrush=red, buttonDecoration=blurFrame(10f, 5f))), ex,
      TextButton("Blurred(blur=10f, spread=10f)"){ _ => }(styleSheet.copy(buttonBackgroundBrush=red, buttonDecoration=blurFrame(10f, 10f))), ex,
      TextButton("Blurred(blur=20f, spread=10f)"){ _ => }(styleSheet.copy(buttonBackgroundBrush=red, buttonDecoration=blurFrame(20f, 10f))), ex,
      <p width="55em" align="center"><tt>styleSheet</tt> is the implicit style <tt>Sheet</tt> for this <tt>Book.Page</tt></p>
    )
  }

  Page("Shaded", "") {
    import styles.decoration.Shaded
    Col.centered(
      <div width="55em">
        <p> These buttons are of the form:</p>
        <![CDATA[
        TextButton("Shaded(delta=..., down=...)") { _ => }(
            styleSheet.copy(buttonFrame =
                              Shaded(fg=darkGrey,
                                     bg=lightGrey, delta=..., down=...)))]]>
      </div>.enlarged(20), ex,
      TextButton("Shaded  (18, down=true)") { _ => }(styleSheet.copy(buttonDecoration = Shaded(delta = 18f, down = true))), ex,
      TextButton("Shaded  (8,  down=false)") { _ => }(styleSheet.copy(buttonDecoration = Shaded(delta = 8f, down = false))), ex,
      TextButton("Shaded  (12, down=false)") { _ => }(styleSheet.copy(buttonDecoration = Shaded(delta = 12f, down = false))), ex,
      TextButton("Shaded  (18, down=false)") { _ => }(styleSheet.copy(buttonDecoration = Shaded(delta = 18f, down = false))), ex, ex,
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


  val GUI: Glyph = book.Layout.leftCheckBoxes()
}
