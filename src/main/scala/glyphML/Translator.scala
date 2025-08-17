package org.sufrin.glyph
package glyphML

import glyphXML.PrettyPrint

import org.sufrin.glyph.unstyled.static.BreakableGlyph
import org.sufrin.SourceLocation.{sourcePath, SourceLocation}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.glyphML.AbstractSyntax.{isEmptyText, Scope, Tree}
import org.sufrin.glyph.glyphML.Context.{AttributeMap, Context, ExtendedAttributeMap}
import org.sufrin.glyph.glyphXML.PrettyPrint.AnyPretty
import org.sufrin.glyph.glyphXML.Translation.AttributeMap
import org.sufrin.logging.{Loggable, SourceDefault}

import scala.util.matching.Regex


object Translator {
  implicit class SemanticStyleSheet(val style: StyleSheet) extends AnyVal {
    def makeText(text: String): Glyph = unstyled.Text(text, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush)
  }

  implicit class IntelligibleAttributeMap(val attrs: AttributeMap) extends AnyVal {
    def toSource: String = attrs.toSeq.map{case (k: String,d: String)=>s"$k=\"$d\"" }.mkString(" ")
  }


  type Extension = Translator => Context => AbstractSyntax.Element => Seq[Glyph]

  private val punctChar  = ".\";:',!?".toList

  private def isPunctuated(text: String): Boolean = // pre: word.nonEmpty
    !text.head.isWhitespace && punctChar.contains(text.head) || punctChar.contains(text.last)

  private def punctSplit(text: String): (String, String, String) = {
    val pre = text.takeWhile{punctChar.contains(_)}.mkString("")
    val mid = text.dropWhile{punctChar.contains(_)}
    val quoted = mid.takeWhile{ p => !punctChar.contains(p)}.mkString("")
    val suff = mid.dropWhile{ p => !punctChar.contains(p)}.mkString("")
    (pre, quoted, suff)
  }


  object HYPHENATION extends Loggable {
    // TODO: polyglot, automation, ...
    def forText(text: String, style: StyleSheet): Option[Glyph] = {
      val discretionaryWordBreak = style.discretionaryWordBreak

      @inline def discretionary: Option[Seq[String]] =
        if (text.contains(discretionaryWordBreak)) {
          val parts = text.split(discretionaryWordBreak).toSeq
          Some(parts)
        }
        else None

      discretionary match {
        case Some(parts) =>
          val hyphen = style.makeText("-")
          val glyphs = parts.toSeq.map(style.makeText(_))
          Some(new BreakableGlyph(hyphen, glyphs))

        case None =>
          hyphenation.get(text) match {
          case Some(parts) =>
            finest(s"$text->[${parts.toList.mkString(", ")}]")
            val glyphs = parts.toSeq.map(style.makeText(_))
            val hyphen = style.makeText("-")
            Some(new BreakableGlyph(hyphen, glyphs))

          case None =>
            val (pre, quoted, suff) = punctSplit(text)
            hyphenation.get(quoted) match {
              case Some(parts) =>
                val last = parts.length - 1
                finest(s"$text->($pre,$quoted,$suff) ==>")
                val pieces =
                  for {i <- 0 to last} yield
                      if (i == 0) pre + parts(i) else if (i == last) parts(i) + suff else parts(i)
                finest(s"  $text->[${pieces.toList.mkString(", ")}]")
                val glyphs = pieces.map(style.makeText(_))
                val hyphen = style.makeText("-")
                Some(new BreakableGlyph(hyphen, glyphs))

              case None => None
            }
        }
      }

    }

    val hyphenation: collection.mutable.Map[String, Array[String]] = collection.mutable.Map[String, Array[String]]()

    def apply(word: String)(implicit punct: String="-"): Unit =
      hyphenation.update(word.replaceAll(punct,""), word.split(punct))

  }

}

/**
 * Resolve the given attributes of an element using the scoping rules
 * @param definitions
 * @param scope
 * @param tag
 * @param givenAttributes
 * @param child
 */
class ResolveScopedAttributes(definitions: Definitions, scope: Scope, tag: String, givenAttributes: glyphML.Context.AttributeMap, child: Seq[Tree]) {
  import glyphML.Context._
  val selfid   = givenAttributes.String("id", "")
  val StoredAttributeMap(selfattributes) = if (selfid.isEmpty) StoredAttributeMap(givenAttributes) else definitions.getKindElse(StoreType.AttributeMap)(selfid)

  val classid = selfattributes.String("class", "")
  val StoredAttributeMap(classattributes) = if (classid.isEmpty) StoredAttributeMap(selfattributes) else definitions.getKindElse(StoreType.AttributeMap)(s"class:$classid")
  val StoredAttributeMap(tagattributes) =  definitions.getKindElse(StoreType.AttributeMap)(s"tag:$tag")

  /** Effective attributes after inheritance */
  val inheritedAttributes: AttributeMap = (givenAttributes supersede selfattributes supersede classattributes supersede tagattributes).without("id", "class")

  /** Should subtrees that are empty be translated */
  val keepEmpty = inheritedAttributes.Bool("keepempty", false)

  /** Subtrees: with empty texts filtered out unless `keepEmpty` */
  val children = if (keepEmpty) child else child.filterNot(isEmptyText)
}

class Translator(val definitions: Definitions)(rootStyle: StyleSheet) { thisTranslator =>
  import glyphML.AbstractSyntax._
  import glyphML.Context._
  import Translator._

  /**
   *  Add stock entitites
   */
  locally {
    definitions("amp") = "&"
    definitions("lt") = "<"
    definitions("gt") = ">"
  }

  /**
   *  Translate this tree (and its descendants, if any) to `Glyph`s
   *  derived from it in the given environment.
   */
  def translate(context: Context)(tree: Tree): Seq[Glyph] = {
    implicit val style: StyleSheet = context.sheet

    def interWordSpace() = style.textWordFill(0.5f)

    def toText(text: String): Glyph = {
      if (text.isEmpty || text(0).isWhitespace)
        interWordSpace()
      else {
        HYPHENATION.forText(text, style) match {
          case None             => style.makeText(text)
          case Some(hyphenated) => hyphenated
        }
      }
    }

    tree match {
      case Element(scope: Scope, tag: String, attributes: AttributeMap, child: Seq[Tree]) => translateElement(context)(scope, tag, attributes, child)
      case Text(text: String)       => List(toText(text))
      case Para(texts: Seq[String]) => texts.map(toText(_))
      case Quoted(text: String)     => List(translateQuoted(context)(text))
      case Entity(name: String)     =>
        val StoredString(text) = definitions.getKindElse(StoreType.String)(name)
        List(style.makeText(text))
      case Comment(target: String, text: String) => Nil
      case MacroParam(name) =>
        SourceDefault.warn(s"Unbound macro parameter: <?$name?>")
        Nil
    }
  }

  object PCDATA {
    private def leadingSpaces(s: String): Int = {
      var n = 0
      while (n < s.length && s(n) == ' ') n += 1
      n
    }

    private val indentationOrdering: Ordering[String] = new Ordering[String] {
      def compare(x: String, y: String): Int = leadingSpaces(x) - leadingSpaces(y)
    }

    def stripCommonIndentation(lines: Seq[String]): Seq[String] = {
      val lines$ = lines.dropWhile(_.isBlank)
      val prefix = leadingSpaces(lines$.filterNot(_.isBlank).min(indentationOrdering))
      @inline
      def stripPrefix(line: String): String = if (line.length < prefix) line else line.substring(prefix)

      if (lines$.nonEmpty && lines$.last.isBlank)
        lines$.init.map(stripPrefix)
      else
        lines$.map(stripPrefix)
    }
  }


  def translateQuoted(context: Context)(text: String): Glyph = {
    val sheet=context.sheet
    val rawLines = text.split('\n').toSeq
    val lines = PCDATA.stripCommonIndentation(rawLines)
    NaturalSize.Col(align=Left, bg = sheet.cdataBackgroundBrush)(lines.map(unstyled.Text(_, font=sheet.cdataFont, fg=sheet.cdataForegroundBrush)))
  }

  /**
   * In general the for an element is:  `<tag `*givenAttributes*`>`*child*`</tag>`
   * or (when *child* is empty)
   * `<tag `*givenAttributes*`/>`
   *
   *
   * CODING STANDARD:
   *
   * `localAttributes` -- all element kinds are translated with this attribute map defined as
   * {{{(
   *      givenAttributes   supersede
   *      selfattributes    supersede
   *      classattributes   supersede
   *      tagattributes).without("id", "class")
   *   }}}
   *
   *  where
   *
   *  `selfattributes` is the attribute map defined for *x* if `id=`*x* appears in `givenAttributes`
   *
   *  `classattributes` is the attribute map defined for `class:*x*`` if `class=`*x* appears in `givenAttributes`
   *
   *  `tagattributes` is the attribute map defined for `tag:*x*`` (`tag` is the element tag)
   *
   *  The children of such elements are (usually) translated in the inherited `context` updated by the
   *  `localAttributes`. Its `attributes` is  `localattributes supersede context.attributes`, and its
   *  style sheet, `sheet` is derived from its `context.sheet` by setting properties defined
   *  by its `attributes`.
   *
   * @see Context
   * @see Context.Env
   * @see Context.deriveSheet
   * @param context inherited attribute mapping and style sheet
   * @param scope the location (as a stack of element tags, source code locations) of the element
   * @param tag the element is of form `<tag ...>...</tag>`
   * @param givenAttributes the attributes explicitly given at the tag
   * @param child the subtrees of the element (if any)
   * @return translation of the subtrees
   */


  def translateElement(context: Context)(scope: Scope, tag: String, givenAttributes: AttributeMap, child: Seq[Tree]): Seq[Glyph] = {
    import glyphML.Context.TypedAttributeMap


    val resolved = new ResolveScopedAttributes(definitions, scope, tag, givenAttributes, child)
    import resolved._

    val localAttributes = inheritedAttributes

    tag match {
      case "div" | "body" =>
        val derivedContext = context.updated(localAttributes)
        children.filterNot(isEmptyText).flatMap(translate(derivedContext))

      case "p" =>
        val derivedContext = context.updated(localAttributes)
        val words = children.flatMap(translate(derivedContext))
        val hang  = derivedContext.attributes.String("hang", "")
        val hangGlyph = if (hang.isEmpty) None else Some(styled.Label(hang)(derivedContext.sheet))
        val theGlyph = Paragraph.fromGlyphs(derivedContext.sheet, words, hangGlyph).enlargedBy(0f, derivedContext.sheet.parSkip)
        val frame = derivedContext.attributes.Brush("framed", Brushes.transparent)
        List(if (frame.getAlpha==0) theGlyph else theGlyph.framed(frame))

      case "span" =>
        val derivedContext = context.updated(localAttributes)
        children.flatMap(translate(derivedContext))

      case "i" =>
        val style = Map("textstyle"->"italic")
        val derivedContext = context.updated(style supersede localAttributes)
        children.flatMap(translate(derivedContext))

      case "b" =>
        val style = Map("textstyle"->"bold")
        val derivedContext = context.updated(style supersede localAttributes)
        children.flatMap(translate(derivedContext))

      case "tt" =>
        val style = Map("fontfamily"->"courier")
        val derivedContext = context.updated(style supersede localAttributes)
        children.flatMap(translate(derivedContext))

      case "debug" =>
        val tree = localAttributes.Bool("tree", false)
        val env = localAttributes.Bool("env", false)
        val att = localAttributes.Bool("local", false)
        val mark = localAttributes.String("mark", "")
        val title = localAttributes.String("caption", "debug")
        val framed = localAttributes.Brush("framed", Brushes.transparent)
        val derivedContext = context.updated(localAttributes.without("tree", "env", "local", "mark", "caption"))
        if (tree || env || att) println(s"$title $scope")
        if (env) println(PrettyPrint.prettyPrint(derivedContext)) else if (att) println(PrettyPrint.prettyPrint(derivedContext.attributes)) else {}
        if (tree && children.nonEmpty)
          children.filterNot(isEmptyText).foreach{ tree => println(PrettyPrint.prettyPrint(tree)) }
        val body0 = children.flatMap(translate(context)) // do the body in the existing context
        val body = if (body0.size==1) body0.map(_.framed(framed)) else body0
        (if (mark=="") Nil else List(styled.Label(mark)(derivedContext.sheet).framed(framed))) ++  body

      case "glyph" =>
        // Shown as the named (by `gid`) glyph, with baseline as the average of the ambient textFont baseline and the height of the named glyph
        val derivedContext = context.updated(localAttributes)
        val ambientHeight = derivedContext.sheet.textFont.getMetrics.getHeight
        @inline def raiseBy(by: Scalar)(glyph: Glyph): Glyph = glyph.withBaseline(by)
        val id = givenAttributes.String("gid", "")
        definitions.getKind(StoreType.GlyphGenerator)(id) match {
          case Some(StoredGlyphGenerator(generator)) =>
            val glyph = generator(derivedContext.sheet)
            List(raiseBy((ambientHeight+glyph.h)*0.5f)(glyph))
          case Some(StoredGlyphConstant(glyph)) =>
            List(raiseBy((ambientHeight+glyph.h)*0.5f)(glyph))
          case Some(other) =>
            SourceDefault.error(s"Unexpected stored value at $scope<glyph gid=\"$id\" ...")
            Nil
          case None =>
            SourceDefault.warn(s"No such glyph at $scope<glyph gid=\"$id\" ...")
            Nil
        }

      case "attributes" =>
        // Give the name denoted by "id" to the remaining attributes, and
        // treat the collection as a primitive: globally if
        // there is no body; otherwise for the scope of the evaluation of the body.
        val attrId: String  = givenAttributes.String("id", "")
        val warn:   Boolean = localAttributes.Bool("warn", context.attributes.Bool("attributeswarning", true))
        if (attrId.nonEmpty) {
          val before =  definitions.getKind(StoreType.AttributeMap)(attrId)
          if (children.isEmpty) {// global definition
            if (warn && before.isDefined) SourceDefault.warn(s"Overriding primitive assignment $scope<attributes ${givenAttributes.toSource}")
            definitions(attrId) = StoredAttributeMap(givenAttributes.without("id"))
            Nil
          } else // scoped definition
          { before match {
              case None =>
                definitions(attrId) = StoredAttributeMap(givenAttributes.without("id"))
                val body = children.flatMap(translate(context))
                definitions.remove(StoreType.AttributeMap)(attrId)
                body
              case Some(restore) =>
                if (warn) SourceDefault.warn(s"Overriding scoped primitive $scope<attributes ${givenAttributes.toSource}")
                definitions(attrId) = StoredAttributeMap(givenAttributes.without("id"))
                val body = children.filterNot(isEmptyText).flatMap(translate(context))
                definitions(attrId) = restore
                body
            }
          }
        }
        else Nil

      case "macro" =>
        val tag = givenAttributes.String("tag", "")
        val warn   = localAttributes.Bool("warn", context.attributes.Bool("macrowarning", true))
        if (tag.nonEmpty) {
          // The given attributes are the defaults for upcoming applications of the macro.
          // The context binds the current free attributes.
          // Attributes inherited by class or by id or by tag:macro are ignored.
          if (warn && definitions.getKind(StoreType.Macro)(tag).isDefined) {
             SourceDefault.warn(s"Overriding macro definition $scope<attributes ${givenAttributes.toSource}")
          }
          definitions(tag) = Macro(scope, tag, givenAttributes.without("tag", "warn"), context, children)
        }
        Nil

      case "table" =>
        val width     = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val height    = localAttributes.Int("rows", 0)
        val uniform   = localAttributes.Bool("uniform", false)
        val derivedContext = context.updated(localAttributes.without("columns", "cols", "uniform"))
        val glyphs    =
          children.filterNot(isEmptyText).flatMap(translate(derivedContext))
        import derivedContext.sheet.{foregroundBrush=>fg, backgroundBrush=>bg, padX, padY}
        val Grid      = NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY)
        val buildFrom = if (uniform) Grid.grid(height=height, width=width)(_) else Grid.table(height=height, width=width)(_)
        List(buildFrom(glyphs))

      case "fill" =>
        val width  = localAttributes.Units("width", context.sheet.emWidth)(context.sheet)
        val height = localAttributes.Units("height", context.sheet.exHeight)(context.sheet)
        val stretch = localAttributes.Float("stretch", 1f)
        val background = localAttributes.Brush("background", localAttributes.Brush("bg", Brushes.transparent))
        val foreground = localAttributes.Brush("foreground", localAttributes.Brush("fg", Brushes.transparent))
        List(FixedSize.Space(width, height, stretch, fg=foreground, bg=background))

      case "fixedwidth" =>
        val resolved = new ResolveScopedAttributes(definitions, scope, tag, givenAttributes, child)
        import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
        import resolved._
        val width: Scalar = inheritedAttributes.Units("width", 0)(context.sheet)
        val fg  = inheritedAttributes.Brush("fg", inheritedAttributes.Brush("foreground", Brushes.black))
        val bg  = inheritedAttributes.Brush("bg", inheritedAttributes.Brush("background", Brushes.transparent))
        val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg", "width"))
        val glyph = FixedSize.Row(align=Mid, width=width)(children.flatMap(translate(derivedContext)))
        List(glyph)

      case _ =>
        // (Context, AbstractSyntax.Scope, String, AttributeMap, Seq[AbstractSyntax.Tree])

        @inline def translateExtension(tag: String): Option[Seq[Glyph]] =
          definitions.getKind(StoreType.Extension)(tag) match {
            case None => None
            case Some(StoredExtension(translate)) =>
              Some(translate(thisTranslator)(context)(Element(scope, tag, givenAttributes, child)))
            case _ =>
              SourceDefault.error(s"Internal error at $scope<$tag ...\n")
              None
          }

        @inline def translateMacro(tag: String): Option[Seq[Glyph]] =
            definitions.getKind(StoreType.Macro)(tag) match {
              case None => None
              case Some(StoredMacro(abstraction)) =>
                Some(abstraction.expanded(thisTranslator, localAttributes, children))
              case _ =>
                SourceDefault.error(s"Internal error at $scope<$tag ...\n")
                None
            }

        @inline def translateElement(tag: String): Option[Seq[Glyph]] =
            definitions.getKind(StoreType.Element)(tag) match {
              case None => None
              case Some(StoredElement(element)) =>
                val derivedContext = context.updated(localAttributes)
                Some(translate(derivedContext)(element))
              case _ =>
                SourceDefault.error(s"Internal error at $scope<$tag ...\n")
                None
            }

        @inline def mistake(tag: String): Option[Seq[Glyph]] = {
           SourceDefault.warn(s"No such tag at $scope<$tag ...")
           Some(Nil)
        }

       ( translateExtension(tag) orElse
         translateMacro(tag)    orElse
         translateElement(tag)  orElse
         mistake(tag)
       ).get
    }
  }

  val rootContext = Context(Map.empty, rootStyle)

  def fromXML(source: scala.xml.Node)(implicit location: SourceLocation = sourcePath): Glyph = {
    import glyphXML.PrettyPrint._
    val ast = AbstractSyntax.fromXML(source)(location)
    //ast.prettyPrint()
    val tr = translate(rootContext)(ast)
    NaturalSize.Col(align=Left)(tr)
  }

  implicit def toGlyph(source: scala.xml.Elem)(implicit location: SourceLocation = sourcePath): Glyph = fromXML(source)(location)


}



