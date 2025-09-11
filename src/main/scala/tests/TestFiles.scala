package org.sufrin.glyph
package tests

/**
 *  Cursory tests/explorations of the files package components
 */
object TestFiles extends Application {
  import files._
  import java.nio.file._
  import styled.TextButton
  import NaturalSize._
  implicit val sheet: StyleSheet = StyleSheet()
  val byModification = new Ordering[Path] { def compare(o1: Path, o2: Path): Int = (o1.toFile.lastModified-o2.toFile.lastModified).sign.toInt}
  lazy val fileSystem = FileSystems.getDefault
  val home = new Folder(fileSystem.getPath("/", "Users", "sufrin"))

  def GUI: Glyph = Col(
    TextButton("/Users/sufrin"){ _ =>
      println(home.path)
      home.withValidCaches {
        val (dirs, notDirs) = home.splitDirs.value
        for {path <- dirs} println(path)
        for {path <- notDirs} println(path)
      }
      println("----")
    },
    TextButton("/Users/sufrin (dirs)"){ _ =>
      println(home.path)
      home.withValidCaches {
        for {path <- home.dirs.value} println(path)
      }
      println("----")
    },
    TextButton("/Users/sufrin (dirs) -T"){ _ =>
      println(home.path)
      home.withValidCaches {
        for {path <- home.dirs.value.sorted(byModification)} println(path)
      }
      println("----")
    },
    TextButton("/Users/sufrin (non dirs)"){ _ =>
      println(home.path)
      home.withValidCaches {
        for {path <- home.notDirs.value} println(path)
      }
      println("----")
    },
    TextButton("/Users/sufrin (non dirs) -T"){ _ =>
      println(home.path)
      home.withValidCaches {
        for {path <- home.notDirs.value.sorted(byModification)} println(path)
      }
      println("----")
    },
    TextButton("/Users/sufrin (attrs)"){ _ =>
      println(home.path)
      home.withValidCaches {
        import FileAttributes.legibleAttributes
        for {(path, attrs) <- home.attributes(home.dirs.value)} println(s"$path ${attrs.asString}")
        for {(path, attrs) <- home.attributes(home.notDirs.value)} println(s"$path ${attrs.asString}")
      }
      println("----")
    },
    TextButton("/Users/sufrin (map)"){ _ =>
      println(home.path)
      home.withValidCaches {
        import FileAttributes.legibleAttributes
        for { (path, attrs) <- home.attributeMap.value }
          println(s"$path ${attrs.asString}")
      }
      println("----")
    },
    TextButton("Filestores"){ _ =>
      println(f"${"Volume"}%-30s ${"Size"}%-7s ${"Used"}%7s / ${"Available"}%-7s " )
      for { store <- System.fileStores if (store.toString=="/" || !store.toString.startsWith("/System")) } {
        val useable: Long = store.getUsableSpace
        val total: Long   = store.getTotalSpace
        val unallocated: Long = store.getUnallocatedSpace
        val used: Long = total-unallocated
        val mb: Long = 1000*1000
        val gb: Long = 1000*mb
        println(f"${store.toString.replaceFirst("""[(][^)]+[)]""","")}%-30s ${total/gb}%-7d ${used/gb}%7d / ${unallocated/gb}%-7d  " )
      }
      println("----")
    },
    TextButton("[X]"){ _ =>
      home.invalidate()
    }


    )

  def title: String = "Files"

  override val dock = Dock(" Phy \nles", bg=Brushes.yellow)
}
