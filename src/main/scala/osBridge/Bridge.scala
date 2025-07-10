package org.sufrin
package glyph
package osBridge

import org.sufrin.logging.SourceLoggable

/**
 *
 * Experiments with dynamic loading
 *
 */

class Bridge extends SourceLoggable { }

object Bridge extends Bridge {
  import java.awt
  import javax.swing._

  def main(args: Array[String]): Unit = {
      org.sufrin.logging("org.sufrin.glyph.osBridge.Bridge") = "ALL"


      import java.net.{URL, URLClassLoader}
      import java.lang.reflect.Method

      // this may be a jar or the class files folder
      val codeURL = classOf[org.sufrin.glyph.osBridge.Bridge].getProtectionDomain.getCodeSource.getLocation

      // Load the JAR (and any dependencies)
      val classLoader = new URLClassLoader(Array(codeURL), this.getClass.getClassLoader)

      // Load the class by name
      val applicationClass = classLoader.loadClass("org.sufrin.glyph.tests.demonstrationBook.Pages") // fully qualified name

      // Get the `main` method (static, takes Array[String])
      val mainMethod: Method = applicationClass.getMethod("main", classOf[Array[String]])

      // Invoke it with arguments
      //val args: Array[String] = Array("arg1", "arg2")
      mainMethod.invoke(null, args.asInstanceOf[AnyRef])

    }

}

