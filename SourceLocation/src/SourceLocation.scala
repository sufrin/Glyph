package org.sufrin


import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 *  Two macros that yield, respectively, the entire path to the source line
 *  on which they were invoked, and the path, without folder details, to
 *  that same line.
 */


object SourceLocation {
  /** yields filename:linenumber.position of the source line at which this is invoked */
  implicit def sourceLocation: SourceLocation = macro sourceLocationMACRO

  /** yields completefilepath/filename:linenumber.position of the source line at which this is invoked */
  def sourcePath: SourceLocation = macro sourcePathMACRO

  /** Source location coordinates */
  case class SourceLocation(file: String, line: Int, offset: Int) {
    override def toString = s"$file:$line.$offset"
  }

  def sourceLocationMACRO(c: Context): c.universe.Tree = {
    import c.universe._

    val pos = c.enclosingPosition
    val filename = pos.source.file.name
    val line = pos.line
    val charOffset = pos.column
    q"""org.sufrin.SourceLocation.SourceLocation($filename, $line, $charOffset)"""
  }

  def sourcePathMACRO(c: Context): c.universe.Tree = {
    import c.universe._
    val pos = c.enclosingPosition
    val fileURI =
      try { pos.source.file.file.toPath.toString }
      catch { case _: NullPointerException => pos.source.file.name }
    val line = pos.line
    val charOffset = pos.column
    q"""org.sufrin.SourceLocation.SourceLocation($fileURI, $line, $charOffset)"""
  }

}
