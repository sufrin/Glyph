package org.sufrin.glyph
package tests.demonstrationBook

import styled.{Book, BookSheet, Label, TextButton}
import styled.windowdialogues.Dialogue

import Dialogue.{OK, POPUP}
import NaturalSize.{Col, Grid, Row}
import Glyphs.{FilledRect, INVISIBLE, Rect}

import org.sufrin.glyph.DefaultBrushes.{nothing, redFrame, ROUND}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.HintManager
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
    import styles.decoration.{Framed,unDecorated, RoundFramed, Decorated}
    import styled.TextButton

    def roundframed(fg: String, bg: String, radius: Scalar, buttonBG: String="lightGrey"): Glyph = {
      val button = TextButton(s"RoundFramed($fg, $bg, $radius)", Hint(10.0, s"buttonBackgroundBrush=$buttonBG"))
      { showSourceCode(
        <div align="left" width="75em" DEC="Undecorated">
          <p>buttonBackgroundBrush={buttonBG}</p><p>buttonDecoration = RoundFramed({fg}, {bg}, enlarge=30f, radius={radius.toString})</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=DefaultBrushes(buttonBG),
        buttonDecoration = RoundFramed(DefaultBrushes(fg), DefaultBrushes(bg), radius=radius, enlarge=30f)))
      button
    }

    def framed(fg: String, bg: String, buttonBG: String="lightGrey"): Glyph =
      TextButton(s"Framed($fg, $bg)", hint=Hint(10.0, s"buttonBackgroundBrush=$buttonBG"))
      { showSourceCode(
        <div align="Left" width="75em" DEC="Undecorated">
          <p>buttonBackgroundBrush={buttonBG}</p><p>Framed({fg}, {bg}, enlarge=35f)</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=DefaultBrushes(buttonBG),
                        buttonDecoration = Framed(DefaultBrushes(fg), DefaultBrushes(bg), enlarge=35f)))

    Col(align=Center)(
      <div width="60em" align="justify" parSkip="3ex">
        <p align="center">Button decoration with Framed and RoundFramed.</p>
        <p>Click or hover over any of the buttons to see detail, and to understand
           the relationship between button backgrounds and frame backgrounds. Unless "nothing"
           a button background has higher priority than the background of its frame.
        </p>
        <fill height="3ex"/>
      </div>,
      anchor,
      Grid(width=3, padx=10, pady=5).rows(
        framed("darkGrey/2-10-10", "nothing", "nothing"), framed("darkGrey/2~5~2", "nothing", "nothing"), framed("darkGrey/3-10-10~5~3", "nothing", "yellow"),
        framed("darkGrey/10/ROUND", "nothing") ,  framed("darkGrey/20/ROUND", "yellow") ,  framed("darkGrey/30/ROUND", "nothing"),
        framed("darkGrey/10/SQUARE", "nothing") ,  framed("darkGrey/20/SQUARE", "yellow") ,  framed("darkGrey/30/SQUARE", "nothing"),

        roundframed("nothing", "lightGrey", 10, "nothing") ,  roundframed("nothing", "lightGrey(10)", 20, "green") ,  roundframed("nothing", "lightGrey", 30, "green"),
        roundframed("red/10", "yellow", 0.1f, "yellow") ,  roundframed("red/20", "yellow", 0.1f, "yellow") ,  roundframed("red/30", "yellow", 0.1f, "yellow"),
        roundframed("red/10", "green", 0.3f, "yellow") ,  roundframed("red/20", "green", 0.3f, "yellow") ,  roundframed("red/30", "green", 0.3f, "yellow"),
        roundframed("red/10", "nothing", 0.6f, "nothing") ,  roundframed("red/20", "yellow", 0.6f, "yellow") ,  roundframed("red/30", "yellow", 0.9f, "yellow"),
      )
    )
  }

  Page("Blurred/Shaded"){
    import styles.decoration.{Blurred, Shaded}

    def blurred(fg: String, bg: String, blur: Scalar, spread: Scalar, buttonBG: String="lightGrey", delta: Scalar=0f): Glyph =
      TextButton(s"Blurred($fg, $bg, $blur, $spread)", hint=Hint(10.0, s"buttonBackgroundBrush=$buttonBG\ndelta=$delta"))
      { showSourceCode(
        <div align="Left" width="75em">
          <p>decoration=Blurred({fg}, {bg}, blur={blur}, spread={spread}, delta={delta})</p>
          <p>buttonBackgroundBrush={buttonBG}</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=DefaultBrushes(buttonBG),
        buttonDecoration = Blurred(DefaultBrushes(fg), DefaultBrushes(bg), blur, spread, delta=delta, enlarge=20)))

    def shaded(fg: String, bg: String, down: Boolean, buttonBG: String="lightGrey", delta: Scalar=10f): Glyph =
      TextButton(s"Shaded($fg, $bg, ∂=$delta, down=$down)", hint=Hint(10.0, s"buttonBackgroundBrush=$buttonBG\ndelta=$delta"))
      { showSourceCode(
        <div align="Left" width="75em">
          <p>decoration=Shaded({fg}, {bg}, enlarge=25, delta={delta}, down={down})</p>
          <p>buttonBackgroundBrush={buttonBG}</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=DefaultBrushes(buttonBG),
        buttonDecoration = Shaded(DefaultBrushes(fg), DefaultBrushes(bg), enlarge=20, delta=delta, down=down)))

    Col(align=Center)(
        <div width="65em" enlarged="20">
            <p>Click/hover over any of the buttons to see detail, and to understand
              the relationship between button backgrounds and frame backgrounds. Unless "nothing"
              a button background has higher priority than the background of its frame.
            </p>
        </div>,
        Grid(width=3, padx=30, pady=5).rows(
            blurred("red",    "nothing",  10f, 5f, "nothing"),
            blurred("red",    "red",      20f, 5f, "nothing"),
            blurred("red",    "red",      20f, 20f, "green"),

            blurred("red",    "nothing",  10f, 5f, "nothing", 10),
            blurred("red",    "red",      20f, 5f, "nothing", 12),
            blurred("red",    "red",      20f, 20f, "green",  12),

          shaded("black", "nothing", true, "lightgrey", 5),
          shaded("black", "nothing", true, "lightgrey", 10),
          shaded("black", "nothing", true, "lightgrey", 15),

          shaded("black", "nothing", false, "lightgrey", 5),
          shaded("black", "nothing", false, "lightgrey", 10),
          shaded("black", "nothing", false, "lightgrey", 15),

          shaded("blue", "lightgrey", false, "nothing", 5),
          shaded("blue", "lightgrey", false, "green", 10),
          shaded("blue", "lightgrey", false, "nothing", 15),
        )
    )
  }

  //Page("Hints", "") {
  //  import styles.decoration.Shaded
  //}


  val GUI: Glyph = book.Layout.leftCheckBoxes()
}
