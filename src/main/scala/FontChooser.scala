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
class FontChooser(initialFont: Font, initialBrush: Brush, aboveDisplay: (Glyph, Glyph) = (INVISIBLE(), INVISIBLE()))(implicit sheet: StyleSheet) {

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
  val fontText = styled.TextField(size=60, initialText=fontString, onEnter=setFontFrom)

  import FontFamily._

  def setFont(newFont: Font): Unit = {
    _font = newFont
    fontString = _font.asString
    fontText.string=fontString
    showExample(example.string)
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


  def showExample(text: String=example.string): Unit = exampleDisplay.set(text)


  def interpretSpecialKey(key: GlyphTypes.EventKey, glyph: Glyph): Unit = {
    val mods: Bitmap = toBitmap(key)
    example.string=s"${example.string} ${key._key} ${mods.toString} "
  }

  val abbrs = new TextAbbreviations(onLineTrigger = true, implicitUnicode = true)
  locally {
    abbrs.reversible = true
    for {  (abbr, symb) <- StockAbbreviations.all } abbrs.update(abbr, symb)
  }
  
  lazy val brush: Brush = initialBrush
  lazy val example      = styled.TextField(size=60,
                                           onChange=Some(showExample),
                                           onError={ (_,_) => TextField.bell.play() },
                                           initialText = "This editable text is shown in the dashed frame in Font",
                                           abbreviations = abbrs).withAbbreviationKey(Key.ESCAPE)


  import sheet.{em, ex}
  import CellFit._

  val frameGrey = Brushes.darkGrey(width=2)

  val hintSheet   = sheet.copy(fontScale=0.6f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))
  val buttonSheet = sheet.copy(fontScale=0.8f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))

  val onLine: ToggleVariable = ToggleVariable(abbrs.onLineTrigger) { state => abbrs.onLineTrigger = state }
  val implicitUnicode        = ToggleVariable(abbrs.implicitUnicode) { state => abbrs.implicitUnicode=state }

  val triggerButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable/disable automatic substitution for abbreviations\nas they are typed.\nWhen disabled, SHIFT-SHIFT is used\nto make a substitution.")(hintSheet))(onLine)(buttonSheet)

  val implicitButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable/disable implicit abbreviations\nof unicode glyphs\nexpressed as hex digit sequences\n followed by \"u+\" ")(hintSheet))(implicitUnicode)(buttonSheet)

  val tryoutButton: Glyph =
    styled.TextButton("Popup an Edit Field", hint=Hint(5, "The popup edits\nthe editable text\nusing the\ncurrent font.")(hintSheet)){
      _ =>
        val playGUI = NaturalSize.Col(align=Center)(
          TextField(font       =  _font,
                    size       =  example.size,
                    onChange   =  Some(showExample),
                    onEnter    =  { string => example.string = string },
                    initialText    = exampleDisplay.get,
                    abbreviations  = abbrs,
                    polyCodings    = example.polyCodings).enlarged(20).framed().enlarged(10) //share the count data
          )
        styled.windowdialogues.Dialogue.FLASH(playGUI,null,s"Play TextField ${_font.asString}").OnRootOf(GUI).start()
    }(buttonSheet)

  import styled.Decorate

  lazy val labelledFields = Grid(width=2, pady=15).rows(
        Label("Edit "), example.framed(frameGrey),
        Label("Font "), fontText.framed(frameGrey).cellFit(ShiftWest),
        aboveDisplay._1.cellFit(ShiftWest), aboveDisplay._2.cellFit(ShiftWest)
    )

  lazy val exampleDisplay: ActiveGlyph[String] = new ActiveGlyph[String]("", Rect(1.1f*labelledFields.w, 2*fontText.h, black)) {
    override val forcedSet: Boolean = true
    def toGlyph(t: String): Glyph = Text(t, _font, fg=brush)
    def copy(fg: Brush, bg: Brush): Glyph = this
  }

  lazy val GUI: Glyph = Col(align=Center, bg=sheet.backgroundBrush,skip = 20)(
    Row(align=Mid)(familyChooser, em, styleChooser.framed(), em,  sizeChooser.framed()), ex,
    labelledFields,
    FixedSize.Row(labelledFields.w, align=Mid)(Label("Live abbrs: "),       triggerButton, sheet.hFill(2),
                                               Label("Implicit unicode: "), implicitButton, sheet.hFill(2, stretch=1.2f),
                                               tryoutButton),
    exampleDisplay.framed(Brushes.darkGrey(width=3).dashed(10, 10)),
    )

}

class FontAndBrushChooser(fontChooserFont: Font=null)(implicit val sheet: StyleSheet) {

  val brush: Brush = Brush("black.1.fill").copy()

  import FontFamily._

  val phont = if (fontChooserFont eq null) sheet.textFont.scaled(1.5f).familied("Courier") else fontChooserFont

  lazy val brushName    = new ActiveString(initial=f"${brush.toString}%-50s")
  lazy val fontChooser  = new FontChooser(phont, brush, aboveDisplay = (Label("Brush ", align=Left), brushName))
  lazy val brushChooser = new BrushChooser(brush, brush, {_=>}, { brush => brushName.set(brush.toString)})

  locally {
    //brushName.set(brush.toString)
  }


  lazy val GUI: Glyph = NaturalSize.Col(align=Center, bg=sheet.backgroundBrush)(
    fontChooser.GUI,
    sheet.vSpace(2),
    brushChooser.GUI
    )

}

object FontChooserTest extends Application {
  implicit val sheet: StyleSheet = StyleSheet(backgroundBrush = lightGrey)

  val chooser = new FontAndBrushChooser(sheet.textFont.scaled(1.5f).familied("Courier"))

  val GUI: Glyph = chooser.GUI.enlarged(5).framed(red(width=1))

  def title: String = "FontChooserTest"

  override def whenStarted(): Unit = {
    super.whenStarted()
    chooser.fontChooser.showExample()
  }
}
