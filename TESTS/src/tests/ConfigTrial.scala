package tests
import org.ekrich.config.ConfigFactory.systemProperties
import org.ekrich.config.ConfigFormatOptions
import org.ekrich.config.parser.ConfigDocumentFactory
import org.ekrich.config.parser.ConfigDocument
import org.ekrich.config._
import org.ekrich.config.impl.SimpleConfigList

object ConfigTrial extends App {
  val text =
    """xyzzy = {
      |    something="This value comes from complex-app's complex2.conf"
      |
      |    # here we want a simple-lib-context unique to our app
      |    # which can be custom-configured. In code, we have to
      |    # pull out this subtree and pass it to simple-lib.
      |    simple-lib-context = {
      |        simple-lib {
      |            foo="This value comes from complex-app's complex2.conf in its custom simple-lib-context"
      |            whatever = "This value comes from complex-app's complex2.conf in its custom simple-lib-context"
      |            bar = 42
      |            barf = 42.6
      |        }
      |        alist.x.y=["foo", "bar"]
      |    }
      |}
      |
      |""".stripMargin

  //println(ConfigDocumentFactory.parseString(text).render)

  val config: Config = ConfigFactory.parseFile( new java.io.File("/Users/sufrin/configurationtest.file")) .resolve()

  def mapping(config: Config): collection.mutable.Map[String, Any] = {
    val map = new collection.mutable.HashMap[String, Any]()
    val it = config.entrySet.iterator()
    while (it.hasNext) {
      val entry = it.next
      map.addOne((entry.getKey, entry.getValue))
    }
    map
  }


  println(mapping(config))
  println(mapping(config.getConfig("abbreviations")))
  println(mapping(config.getConfig("abbreviations.math")))



}
