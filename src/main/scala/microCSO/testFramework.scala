package org.sufrin.microCSO

import org.sufrin.logging.{Default, FINEST, INFO}

import scala.sys.process.stdout

trait testFramework {
  val logging: Boolean = true

  def test(): Unit

  def main(args: Array[String]): Unit = test()

  def run(p: process): Unit = {
    Thread.currentThread.setName(s"RUN($p)")
    println(s"============= RUN $p ==============")
    stdout.flush()
    val handle = p.fork()
    handle.terminationStatus(5e6.toLong) match {
      case (true, status)  => println(s"$p terminated ($status)")
      case (false, status) => println(s"$p STILL RUNNING after 5 seconds ($status)")
    }
    println(s"=====================")
    stdout.flush()
  }

  def show(s: String): Unit = {
    if (Default.level>INFO) println(s) else print(s"$s ")
    stdout.flush()
  }

  def frun(p: process): Unit = {
    val l = Default.level
    Default.level=FINEST
    run(p)
    Default.level=l
  }

  def fapply(p: process): Unit = {
    val l = Default.level
    Default.level=FINEST
    apply(p)
    Default.level=l
  }

    def apply(p: process): Unit = {
    println(s"============= APPLY $p ==============")
    try p() catch {
      case exn: Throwable =>
        exn.printStackTrace(System.out)
        System.out.flush()
    }
    println(s"===========================")
  }

}
