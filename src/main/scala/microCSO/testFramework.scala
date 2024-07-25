package org.sufrin.microCSO

import org.sufrin.logging.{Default, FINEST, INFO}
import org.sufrin.microCSO.Time._

import scala.sys.process.stdout

trait testFramework {
  val logging: Boolean = true

  val deadline: Nanoseconds = Time.seconds(5.0)

  def test(): Unit

  def main(args: Array[String]): Unit = test()

  def run(p: process): Unit = {
    RuntimeDatabase.reset()
    Thread.currentThread.setName(s"RUN($p)")
    println(s"============= RUN $p ==============")
    System.out.flush()
    val handle = p.fork()
    handle.terminationStatus(deadline) match {
      case (true, status)  => println(s"\n$p\n TERMINATED ($status)")
      case (false, status) =>
        println(s"\n$p\nTIMEOUT after ${deadline.toDouble/seconds(1.0)} seconds ($status)")
        println("RuntimeDatabase:")
        RuntimeDatabase.forEach {
          case thread: Thread =>
            RuntimeDatabase.remove(thread)
            Threads.showThreadTrace(thread)
        }
        println("Open Channels:")
        RuntimeDatabase.forEachChannel{
          case chan: Chan[_] =>
            println(s"$chan")
        }
    }
    println(s"=====================")
    System.out.flush()
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
        System.out.println(exn.getMessage)
        Threads.showStackTrace(exn.getStackTrace)
        System.out.flush()
    }
    println(s"===========================")
    System.out.flush()
  }

}
