package org.sufrin.glyph
package glyphML

import glyphXML.PrettyPrint

import org.sufrin.glyph.unstyled.static.BreakableGlyph
import org.sufrin.SourceLocation.{sourcePath, SourceLocation}

import scala.sys.process.ProcessBuilder.Source

object Translator {
  implicit class SemanticStyleSheet(val style: StyleSheet) extends AnyVal {
    def makeText(text: String): Glyph = unstyled.Text(text, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush)
  }

  object HYPHENATION {
    // TODO: polyglot, automation, ...
    def forText(text: String, style: StyleSheet): Option[Glyph] = {
      val discretionaryWordBreak = style.discretionaryWordBreak

      @inline def discretionary: Option[Glyph] =
        if (text.contains(discretionaryWordBreak)) {
          val parts = text.split(discretionaryWordBreak)
          val glyphs = parts.toSeq.map(style.makeText(_))
          val hyphen = style.makeText("-")
          Some(new BreakableGlyph(hyphen, glyphs))
        }
        else None

      hyphenation.get(text) match {
        case Some(parts) =>
          println(s"$text->[${parts.toList.mkString(", ")}]")
          val glyphs = parts.toSeq.map(style.makeText(_))
          val hyphen = style.makeText("-")
          Some(new BreakableGlyph(hyphen, glyphs))

        case None =>
          discretionary
      }

    }

    val hyphenation: collection.mutable.Map[String, Array[String]] = collection.mutable.Map[String, Array[String]]()

    def apply(word: String)(implicit punct: String="-"): Unit =
      hyphenation.update(word.replaceAll(punct,""), word.split(punct))

  }

}

import Context.AttributeMap



class Translator (primitives: ValueStore) {
  import org.sufrin.glyph.glyphML.AbstractSyntax._
  import Context._
  import Translator._


  /**
   *  Translate this tree (and its descendants, if any) to `Glyph`s
   *  derived from it in the given environment.
   */
  def translate(context: Env)(tree: Tree): Seq[Glyph] = {
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

      case Entity(name: String)     => Nil

      case Comment(target: String, text: String) => Nil

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


  def translateQuoted(context: Env)(text: String): Glyph = {
    val sheet=context.sheet
    val rawLines = text.split('\n').toSeq
    val lines = PCDATA.stripCommonIndentation(rawLines)
    NaturalSize.Col(align=Left, bg = sheet.cdataBackgroundBrush)(lines.map(unstyled.Text(_, font=sheet.cdataFont, fg=sheet.cdataForegroundBrush)))
  }

  def isEmptyText(tree: Tree): Boolean = tree match {
    case Text(text) => text.forall(_.isWhitespace)
    case Para(texts) => texts.forall(_.forall(_.isWhitespace))
    case _ => false
  }

  def translateElement(context: Env)(scope: Scope, tag: String, givenAttributes: AttributeMap, child: Seq[Tree]): Seq[Glyph] = {
    import Context.TypedAttributeMap
    // Calculate effective local attributes: given supersedes self supersedes class supersedes tag:$tag
    val selfid   = givenAttributes.String("id", "")
    val Attributes(selfattributes) = if (selfid.isEmpty) Attributes(givenAttributes) else primitives.getLikeElse(Empty.attributes)(selfid)
    val classid = selfattributes.String("class", "")
    val Attributes(classattributes) = if (classid.isEmpty) Attributes(selfattributes) else primitives.getLikeElse(Empty.attributes)(s"class:$classid")
    val Attributes(tagattributes) =  primitives.getLikeElse(Empty.attributes)(s"tag:$tag")

    val localAttributes: AttributeMap = (givenAttributes supersede selfattributes supersede classattributes supersede tagattributes).without("id", "class")
    tag match {
      case "div" | "body" =>
        val derivedContext = context.updated(localAttributes)
        child.filterNot(isEmptyText).flatMap(translate(derivedContext))

      case "p" =>
        val derivedContext = context.updated(localAttributes)
        val words = child.flatMap(translate(derivedContext))
        val hang = derivedContext.attributes.String("hang", "")
        val hangGlyph = if (hang.isEmpty) None else Some(styled.Label(hang)(derivedContext.sheet))
        val theGlyph = Paragraph.fromGlyphs(derivedContext.sheet, words, hangGlyph).enlargedBy(0f, derivedContext.sheet.parSkip)
        val frame = derivedContext.attributes.Brush("frameparagraphs", Brushes.transparent)
        List(if (frame.getAlpha==0) theGlyph else theGlyph.framed(frame))
      //Paragraph.fromGlyphs(sheet, targets.map(_.asGlyph), parHang).enlargedBy(0f, sheet.parSkip)

      case "span" =>
        val derivedContext = context.updated(localAttributes)
        child.flatMap(translate(derivedContext))

      case "i" =>
        val style = Map("textstyle"->"italic")
        val derivedContext = context.updated(style supersede localAttributes)
        child.flatMap(translate(derivedContext))

      case "b" =>
        val style = Map("textstyle"->"bold")
        val derivedContext = context.updated(style supersede localAttributes)
        child.flatMap(translate(derivedContext))

      case "tt" =>
        val style = Map("fontfamily"->"courier")
        val derivedContext = context.updated(style supersede localAttributes)
        child.flatMap(translate(derivedContext))

      case "debug" =>
        val tree = localAttributes.Bool("tree", false)
        val env = localAttributes.Bool("env", false)
        val att = localAttributes.Bool("local", false)
        val mark = localAttributes.String("mark", "")
        val title = localAttributes.String("caption", "debug")
        val framed = localAttributes.Brush("framed", Brushes.transparent)
        val derivedContext = context.updated(localAttributes.without("tree","env", "local","mark","caption"))
        if (tree || env || att) println(title, scope)
        if (env) println(PrettyPrint.prettyPrint(derivedContext)) else if (att) println(PrettyPrint.prettyPrint(derivedContext.attributes)) else {}
        if (tree && child.nonEmpty)
          child.filterNot(isEmptyText).foreach{ tree => println(PrettyPrint.prettyPrint(tree)) }
        val body0 = child.flatMap(translate(context)) // do the body in the existing context
        val body = if (body0.size==1) body0.map(_.framed(framed)) else body0
        (if (mark=="") Nil else List(styled.Label(mark)(derivedContext.sheet).framed(framed))) ++  body

      case _ => Nil
    }
  }

  val rootContext = Context.Env(Map.empty, StyleSheet())

  def fromXML(source: scala.xml.Node)(implicit location: SourceLocation = sourcePath): Glyph = {
    import glyphXML.PrettyPrint._
    val ast = AbstractSyntax.fromXML(source)(location)
    //ast.prettyPrint()
    val tr = translate(rootContext)(ast)
    NaturalSize.Col(align=Left)(tr)
  }

  implicit def toGlyph(source: scala.xml.Elem)(implicit location: SourceLocation = sourcePath): Glyph = fromXML(source)(location)

}

