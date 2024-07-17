package org.sufrin.microCSO

import org.sufrin.logging.{Default, INFO}
import org.sufrin.microCSO.portTools._
import org.sufrin.microCSO.proc._
import org.sufrin.microCSO.termination._

object test1 extends testFramework {

  def test(): Unit = {
    Default.level = INFO
    val terminate = proc("Terminate") {
      show("'Output From Terminate'")
    }

    val failure = proc("Failure") {
      fail("within Failure")
    }

    val close = proc("Close") {
      throw new Closed("<no channel>")
    }

    val runStop = proc("runStop"){ stop }

    def sleep(ms: Long) = proc(s"sleep($ms)"){ Thread.sleep(ms); show(s"slept $ms")}

   // run(sleep(5000))
    //apply(terminate)
    apply(failure)
    apply(close)
    apply(failure || terminate)
    apply(runStop || terminate)
    apply(runStop || failure)
    apply(failure || terminate)
    apply(close || terminate)
    apply(terminate || close)

    def stopping(n: Int) = proc(s"stopping($n)") {
      repeatFor ( 1 to 15 ){
        case i if i==n => stop
        case i: Int    => print(s"(s:$i)")
      }
    }

    def failing(n: Int) = proc(s"failing($n)") {
      repeatFor ( 1 to 15 ){
        case i if i==n => assert(false, s"reached $n")
        case i: Int    => print(s"(f:$i)")
      }
    }

    run(stopping(10))
    run(failing(10))
    frun(stopping(10)||failing(6))

  }
}

object test2 extends testFramework {

    def test() = {
      Default.level = INFO
      run((proc("first") {
        show("first")
      } || proc("second") {
        show("second")
      }))

      val names = "first second third fourth".split(' ').toSeq
      val procs = names map { name => proc(name) { show(name)}}

      run(||(procs))

      run(||(for {i <- 0 until 10} yield proc(s"$i") {
        show(s"$i")
      }))

      {
        val as = SyncChan[Int]("as", 10)
        run(
          source(as, (0 until 15).toList) || sink(as) { s => show(s.toString) }
        )
      }


      {
        val as = SyncChan[Int]("as", 1)
        run(
          source(as, (0 until 15).toList) || sink(as) { s => show(s.toString) }
        )
      }

      {
        val c = SyncChan[String]("c", 4)
        val s = source(c, "the rain in spain falls mainly in the plain".split(' '))
        val t = sink(c) { s => show(s"'$s'") }
        run(s || t)
      }

      {
        val c = SyncChan[String]("c", 1)
        val s = source(c, "the rain in spain falls mainly stop in the plain".split(' '))
        val t = sink(c) {
          case "stop" => stop
          case s => show(s"'$s' ")
        }
        run(s || t)
      }

    }
  }

object test3 extends testFramework {
  def test(): Unit = {

    if (true) for {bufSize <- List(100, 50, 30, 2)} {
      if (logging) Default.level = INFO
      if (logging) Default.info(s"Zip trial $bufSize")
      val as = SyncChan[Int]("as", bufSize)
      val bs = SyncChan[Int]("bs", bufSize)
      val zipped = SyncChan[(Int, Int)]("zipped", 10)
      run(
        source(as, (1 to 25).toList)
          || source(bs, (1 to 35).toList)
          || zip(as, bs)(zipped)
          || sink(zipped) { p => show(s"$p") }
      )
    }


    if (true) for {bufSize <- List(100, 50, 30, 2)} {
      if (logging) Default.level = INFO
      if (logging) Default.info(s"Zip trial $bufSize taking 15")
      val as = SyncChan[Int]("as", bufSize)
      val bs = SyncChan[Int]("bs", bufSize)
      val zipped = SyncChan[(Int, Int)]("zipped", 10)
      run(
        source(as, (1 to 25).toList)
          || source(bs, (1 to 35).toList)
          || zip(as, bs)(zipped)
          || proc("take 15") {
          for {_ <- 0 until 15} show(s"${zipped ? ()}")
          zipped.closeIn()
        }
        // || sink(zipped) { p => show(s"$p") }
      )
    }

  }
}



