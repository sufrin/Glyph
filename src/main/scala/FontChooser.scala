package org.sufrin.glyph

import styled.{ActiveString, CheckBox, Label, MenuButton, ToggleVariable}
import unstyled.reactive.{Enterable, Reaction}

import io.github.humbleui.jwm.Key
import org.sufrin.glyph.Brush.{BUTT, ROUND, SQUARE}
import org.sufrin.glyph.Brushes.{black, lightGrey, red, white, NonBrush}
import org.sufrin.glyph.Colour.{ARGB, HSV}
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}
import org.sufrin.glyph.NaturalSize.{transparent, Col, Grid, Row}
import org.sufrin.glyph.styles.decoration.Framed
import org.sufrin.glyph.GlyphTypes.{Font, FontManager, Scalar}
import org.sufrin.glyph.styled.windowdialogues.Dialogue
import org.sufrin.glyph.unstyled.dynamic.ActiveGlyph
import org.sufrin.glyph.unstyled.Text
import org.sufrin.glyph.unstyled.static.{INVISIBLE, Rect}
import org.sufrin.glyph.Modifiers.{toBitmap, Bitmap}
import org.sufrin.glyph.tests.StockAbbreviations
import org.sufrin.glyph.FontFamily.Extensions
import org.sufrin.logging
import org.sufrin.utility.TextAbbreviations

/**
 *
 * @param initialFont
 * @param initialBrush
 * @param aboveDisplay
 * @param sheet
 */
class FontChooser(initialFont: Font, initialBrush: Brush, aboveDisplay: (Glyph, Glyph) = (INVISIBLE(), INVISIBLE()), abbreviations: TextAbbreviations = null)(implicit sheet: StyleSheet) {

  protected def chooserMenu(fieldname: String, choices: Seq[String])(choose: String => Unit)(implicit sheet: StyleSheet): Glyph = {
    val buttons = choices.map {
      name => MenuButton(name){ _ => choose(name) }
    }
    val menu = styled.overlaydialogues.Dialogue.Menu(fieldname, nested=false, buttons)
    menu
  }

  protected def chooserMenu(fieldname: String, choice: String, choices: String*)(choose: String => Unit)(implicit sheet: StyleSheet): Glyph =
    chooserMenu(fieldname, choice::choices.toList)(choose)


  protected def scalarMenu(fieldname: String, choices: Seq[Scalar])(choose: Scalar => Unit)(implicit sheet: StyleSheet): Glyph =
    chooserMenu(fieldname, choices.map(_.toString)){ s => choose(s.toFloat) }

  var fontString: String = FontFamily.fontString(initialFont)
  var _font: Font = initialFont
  val fontText = styled.TextField(size=editText.size, initialText=fontString, onEnter=setFontFrom)

  import FontFamily._

  def setFont(newFont: Font): Unit = {
    _font = newFont
    fontString = _font.asString
    fontText.string=fontString
    showExample(editText.string)
  }

  @inline def isFloat(s: String): Boolean = s.toFloatOption.isDefined

  def setFontFrom(description: String): Unit = {
    description match {
      case s"$family::${style}@$size" if isFloat(size) =>
        setFont(FontFamily(family, style, size.toFloat))
      case _ =>
    }
  }

  val chooserStyle: StyleSheet = sheet.copy(buttonFontSize = 16, labelFontSize = 16)

  lazy val familyNames: Seq[String] =
    for {i <- 0 until FontManager.default.getFamiliesCount} yield FontManager.default.getFamilyName(i)

  val common = List("Arial", "Courier", "Comic Sans", "Menlo")

  val familyChooser = {
      val rows = 6
      val groupSize = (familyNames.size/(rows*3.0)).ceil.toInt
      def chooserFrom(familyNames: Seq[String]): Glyph = {
        @inline def narrowed(name: String): String = name.takeWhile(ch => ch!=' ')
        chooserMenu(s"${narrowed(familyNames.head)} to\n${narrowed(familyNames.last)}", familyNames) {
          family => setFont(_font.familied(family))
        }(chooserStyle)
      }
      val groups = familyNames.toList.sorted.grouped(groupSize)
      val menus  = groups.map(chooserFrom(_)).toSeq
      Row(align=Mid)(chooserFrom(common).framed(), sheet.em, NaturalSize.Grid(fg=black,height=rows)(menus))
  }



  val sizeChooser = scalarMenu("Size", (1 to 20).map(_*4f)){
    size => setFont(_font.sized(size))
  }(chooserStyle)

  val styleChooser = chooserMenu("Style", "Normal", "Bold", "Italic", "BoldItalic"){
    style => setFont(_font.styled(style))
  }(chooserStyle)


  def showExample(text: String=editText.string): Unit = {} //exampleDisplay.set(text)


  def interpretSpecialKey(key: GlyphTypes.EventKey, glyph: Glyph): Unit = {
    val mods: Bitmap = toBitmap(key)
    editText.string=s"${editText.string} ${key._key} ${mods.toString} "
  }

  lazy val abbrs = if (abbreviations ne null) abbreviations else new TextAbbreviations(onLineTrigger = true, implicitUnicode = true)

  lazy val brush: Brush = initialBrush
  lazy val editText: TextField = styled.TextField(size=45,
                                                  onChange=Some(showExample),
                                                  onError={ (_,_) => TextField.bell.play() },
                                                  initialText = "\uD83C\uDF08 this text can be edited \uD83C\uDF08",
                                                  onCursorLeave  = { _ => GUI.guiRoot.giveupFocus() },
                                                  abbreviations = abbrs)(sheet.copy(textForegroundBrush = initialBrush)).withAbbreviationKey(Key.ESCAPE)


  import sheet.{em, ex}
  import CellFit._

  val frameGrey = Brushes.darkGrey(width=2)

  val hintSheet   = sheet.copy(fontScale=0.6f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))
  val buttonSheet = sheet.copy(fontScale=0.8f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))

  val liveSubstitution: ToggleVariable = ToggleVariable(abbrs.onLineTrigger)   { state => abbrs.onLineTrigger = state }
  val implicitUnicode: ToggleVariable  = ToggleVariable(abbrs.implicitUnicode) { state => abbrs.implicitUnicode=state }

  def triggerButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable automatic substitution for abbreviations\nas they are typed.\n(SHIFT-SHIFT substitutes when disabled)")(hintSheet))(liveSubstitution)(buttonSheet)

  def implicitButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable implicit abbreviations\nof unicode glyphs\nexpressed as hex digit sequences\n followed by \"uu\" ")(hintSheet))(implicitUnicode)(buttonSheet)

  lazy val popupAnchor: Glyph = INVISIBLE()

  lazy val tryoutButton: Glyph =
    styled.TextButton("Popup an Editor", hint=Hint(5, "Edits the current text\nin the current font\nand colour")(hintSheet)){
      _ =>
        lazy val localText: TextField = TextField(
                  font       =  _font,
                  fg         =  brush,
                  size       =  (fontText.w/_font.getMetrics.getAvgCharWidth).toInt,
                  onChange   =  Some(showExample),
                  onEnter    =  { string => editText.string = string },
                  initialText    = editText.string,
                  abbreviations  = abbrs,
                  onCursorLeave  = { _ => playGUI.guiRoot.giveupFocus() },
                  polyCodings    = editText.polyCodings)//

        lazy val unicodebutton = styled.TextButton("Unicode", hint=Hint(5, "Replace glyph at cursor left\nby its unicode codepoint(s)")(hintSheet)) {
          _ => localText.unicode()
        }(buttonSheet)

        lazy val unabbreviatebutton = styled.TextButton("Unsubstitute", hint=Hint(5, "Replace substitution at\ncursor left\nby its abbreviation")(hintSheet)) {
          _ => localText.unabbreviation()
        }(buttonSheet)

        lazy val substitutebutton = styled.TextButton("Substitute", hint=Hint(5, "Replace abbreviation at cursor left\nby its substitution (SHIFT-SHIFT has the same effect)")(hintSheet)) {
          _ => localText.tryAbbreviation()
        }(buttonSheet)


        lazy val playGUI: Glyph = NaturalSize.Col(align=Left)(
          FixedSize.Row(align=Mid, width=localText.w)(unabbreviatebutton, unicodebutton, substitutebutton, sheet.hFill(), Label(" Live substitution: "), triggerButton, em, Label("Implicit unicode: "), implicitButton),
          localText.enlarged(2).framed().enlarged(10),
        )
        styled.windowdialogues.Dialogue.FLASH(playGUI,null,s"[${_font.asString}]").South(popupAnchor).start()
    }(buttonSheet)

  import styled.Decorate

  lazy val labelledFields = Grid(width=2, pady=15).rows(
    Label("Font "), fontText.framed(frameGrey).cellFit(ShiftWest),
    Label("Text "), editText.framed(frameGrey),
    )

  lazy val GUI: Glyph = Col(align=Center, bg=sheet.backgroundBrush,skip = 4)(
    Row(align=Mid)(familyChooser, em, styleChooser.framed(), em,  sizeChooser.framed()), ex,
    labelledFields,
    FixedSize.Row(labelledFields.w, align=Mid)(Label("Live substitution: "), triggerButton, sheet.hFill(2),
                                               Label("Implicit  unicode: "), implicitButton, sheet.hFill(2, stretch=1.2f),
                                               tryoutButton),
    popupAnchor
  )

}

class FontAndBrushChooser(fontChooserFont: Font=null, abbreviations: TextAbbreviations=null)(implicit val sheet: StyleSheet) {

  val brush: Brush = Brush("black.1.fill").copy()

  import FontFamily._

  val phont = if (fontChooserFont eq null) sheet.textFont.scaled(1.5f) else fontChooserFont

  lazy val brushName    = new ActiveString(initial=f"${brush.toString}%-50s")
  lazy val fontChooser  = new FontChooser(phont, brush, abbreviations=abbreviations)
  lazy val brushChooser = new BrushChooser(brush, brush, {_=>}, { brush => brushName.set(brush.toString)})

  locally {
    //brushName.set(brush.toString)
  }


  lazy val GUI: Glyph = NaturalSize.Col(align=Center, bg=sheet.backgroundBrush)(
    brushChooser.NOTEXTGUI,
    sheet.vSpace(2),
    fontChooser.GUI,
  ) .enlargedBy(20, 60)

}

object FontChooserTest extends Application {
  implicit val sheet: StyleSheet = StyleSheet(backgroundBrush = lightGrey)

  val abbreviations = new TextAbbreviations(true, true)
  locally {
    abbreviations.reversible=true
    for { (abbr, repl) <- StockAbbreviations.all } abbreviations.update(abbr, repl)
  }

  val chooser = new FontAndBrushChooser(sheet.textFont.scaled(1.5f), abbreviations)

  val GUI: Glyph = chooser.GUI.enlarged(5).framed(red(width=1))

  def title: String = "FontChooserTest"

  override def whenStarted(): Unit = {
    super.whenStarted()
    chooser.fontChooser.showExample()
  }
}
