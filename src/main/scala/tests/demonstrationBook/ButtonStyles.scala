package org.sufrin.glyph
package tests.demonstrationBook

import styled.TextButton
import styled.Label
import styled.windowdialogues.Dialogue

import Dialogue.{POPUP, OK}
import NaturalSize.{Col, Grid, Row}
import styled.Book
import styled.BookSheet
import Glyphs.{INVISIBLE, Rect}

import org.sufrin.glyph.DefaultBrushes.{nothing, redFrame, ROUND}
import org.sufrin.glyph.GlyphTypes.Scalar
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
    def roundframed(fg: String, bg: String, radius: Scalar, buttonBG: String="lightGrey"): Glyph =
      TextButton(s"RoundFramed($fg, $bg, $radius)")
      { showSourceCode(
        <div align="left" width="75em" DEC="Undecorated">
          <p>buttonBackgroundBrush={buttonBG}</p><p>buttonDecoration = RoundFramed({fg}, {bg}, enlarge=0.9f, radius={radius.toString})</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=DefaultBrushes(buttonBG),
        buttonDecoration = Decorated { glyph => glyph.roundFramed(DefaultBrushes(fg), DefaultBrushes(bg), radius=radius)}
        // TODO: IMPLEMENT RoundFramed DECORATION + enlarged
        ))

    def framed(fg: String, bg: String, radius: Scalar, buttonBG: String="lightGrey"): Glyph =
      TextButton(s"Framed($fg, $bg)")
      { showSourceCode(
        <div align="Left" width="75em" DEC="Undecorated">
          <p>buttonBackgroundBrush={buttonBG}</p><p>Framed({fg}, {bg}, enlarge=2f)</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=DefaultBrushes(buttonBG),
                        buttonDecoration = Framed(DefaultBrushes(fg), DefaultBrushes(bg), enlarge=2f)))

    Col(align=Center)(
      <div width="60em" align="justify">
        <p align="center">Button decoration with Framed and RoundFramed.</p>
        <p>Click on any of the buttons to see the relevant parts of its style.</p>
      </div>,
      anchor,
      Grid(width=3, padx=10, pady=10).rows(
      framed("darkGrey/0/ROUND", "", 10), framed("darkGrey/0/ROUND", "", 20), framed("darkGrey/0/ROUND", "", 30),
      framed("darkGrey/10/ROUND", "", 10) ,  framed("darkGrey/20/ROUND", "yellow", 20) ,  framed("darkGrey/30/ROUND", "", 30),
      framed("darkGrey/10/SQUARE", "", 10) ,  framed("darkGrey/20/SQUARE", "yellow", 20) ,  framed("darkGrey/30/SQUARE", "", 30),
        roundframed("nothing", "lightGrey", 10) ,  roundframed("nothing", "lightGrey", 20) ,  roundframed("nothing", "lightGrey", 30),
        roundframed("red/10", "yellow", 0.1f, "yellow") ,  roundframed("red/20", "yellow", 0.1f, "yellow") ,  roundframed("red/30", "yellow", 0.1f, "yellow"),
        roundframed("red/10", "green", 0.3f, "yellow") ,  roundframed("red/20", "green", 0.3f, "yellow") ,  roundframed("red/30", "green", 0.3f, "yellow"),
        roundframed("red/10", "nothing", 0.6f, "nothing") ,  roundframed("red/20", "yellow", 0.6f, "yellow") ,  roundframed("red/30", "yellow", 0.6f, "yellow"),
        roundframed("darkGrey/10/ROUND", "lightgrey", 0.8f) ,  roundframed("darkGrey/20/ROUND", "lightgrey", 0.8f) ,  roundframed("darkGrey/30/ROUND", "lightgrey", 0.8f)
      )
    )
  }

  Page("Blurred"){
    import styles.decoration.Blurred
    def blurFrame(blur: Float, spread: Float) = Blurred(fg=red, blur=blur, spread=spread)
    Col(align=Center)(
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
    Col(align=Center)(
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
