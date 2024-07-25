package org.sufrin.microCSO

import org.sufrin.logging.{Default, INFO}
import org.sufrin.microCSO.portTools._
import org.sufrin.microCSO.proc._
import org.sufrin.microCSO.termination._

object test1 extends testFramework {

  def test(): Unit = {
    Default.level = INFO
    val terminate = proc("Terminate") { show("'Output From Terminate'") }

    val failure = proc("Failure") { fail("within Failure") }

    val close = proc("Close") { throw new Closed("<no channel>") }

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

    run(failing(10))
    run(stopping(10))
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
        val as = Chan[Int]("as", 10)
        run(
          source(as, (0 until 15).toList) || sink(as) { s => show(s.toString) }
        )
      }


      {
        val as = Chan[Int]("as", 1)
        run(
          source(as, (0 until 15).toList) || sink(as) { s => show(s.toString) }
        )
      }

      {
        val c = Chan[String]("c", 4)
        val s = source(c, "the rain in spain falls mainly in the plain".split(' '))
        val t = sink(c) { s => show(s"'$s'") }
        run(s || t)
      }

      {
        val c = Chan[String]("c", 1)
        val s = source(c, "the rain in spain falls mainly stop in the plain".split(' '))
        val t = sink(c) {
          case "stop" => stop
          case s => show(s"'$s' ")
        }
        run(s || t)
      }

      {
        val c = Chan[String]("c", 0)
        val s = source(c, "the rain in spain falls mainly stop in the plain".split(' '))
        val t = sink(c) {
          case "stop" => stop
          case s => show(s"'$s' ")
        }
        run(s || t)
      }

      { println("UNSOUNDNESS OF SHARED SYNCED CHANNEL CLOSING")
        val c = Chan.Shared(1, 1)[String]("c", 0)
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

    for {bufSize <- List(100, 50, 30, 2)} {
      if (logging) Default.level = INFO
      if (logging) Default.info(s"Zip trial $bufSize")
      val as = Chan[Int]("as", bufSize)
      val bs = Chan[Int]("bs", bufSize)
      val zipped = Chan[(Int, Int)]("zipped", 10)
      run(
        source(as, (1 to 25).toList)
          || source(bs, (1 to 35).toList)
          || zip(as, bs)(zipped)
          || sink(zipped) { p => show(s"$p") }
      )
    }


    for {bufSize <- List(30, 2, 0)} {
      if (logging) Default.level = INFO
      if (logging) Default.info(s"Zip trial $bufSize taking 15")
      val as = Chan[Int]("as", bufSize)
      val bs = Chan[Int]("bs", bufSize)
      val zipped = Chan[(Int, Int)]("zipped", 10)
      run(
        source(as, (1 to 25).toList)
          || source(bs, (1 to 35).toList)
          || zip(as, bs)(zipped)
          || proc("take 15") {
          for {_ <- 0 until 15} show(s"${zipped ? ()}")
          zipped.closeIn()
        }
      )
    }

    for {bufSize <- List(30, 2, 0)} {
      if (logging) Default.level = INFO
      if (logging) Default.info(s"Zip trial $bufSize taking 15")
      val as = Chan[Int]("as", bufSize)
      val bs = Chan[Int]("bs", bufSize)
      val cs = Chan[Char]("cs", bufSize)
      val zipped = Chan[(Int, Int, Char)]("zipped", 100)
      run(
             source(as, (1 to 25).toList)
          || source(bs, (1 to 35).toList)
          || source(cs, "abcdefghijk")
          || zip(as, bs, cs)(zipped)
          || proc("take 15") {
               for {_ <- 0 until 150} show(s"${zipped ? ()}")
               zipped.closeIn()
             }
      )
    }

  }
}

object test4 extends testFramework {
  Default.level = INFO

  def test(): Unit = {
    import Time._
    def useChan(share: Chan[String]): process =
      ||(proc("l") {
        var n = 0
        repeat(n < 10) {
          share ! s"l$n";
          n += 1
        }
        show(s"l STOP")
        share.closeOut()
      }
        , proc("r") {
          var n = 0
          repeat(n < 10) {
            share ! s"r$n";
            n += 1
          }
          show(s"r STOP")
          share.closeOut()
        }
        , sink(share) { n => show(s"1->$n") }
        , sink(share) { n => show(s"2->$n") }
      )

    def shareTest(bufSize: Int): Unit = {
      RuntimeDatabase.reset()
      val shared = Chan.Shared(readers = 2, writers = 2)[String]("Shared", bufSize)
      println(s"ShareTest $bufSize")
      run(useChan(shared))
    }

    println(s"==================== NonSync Overtaking Test (Assertion Error, then timeout expected)")
    val ch1: Chan[String] = Chan[String]("ch1", 0)
    run(useChan(ch1))

    println(s"==================== Finite sharing test 1")
    val ch2: Chan[String] = Chan.Shared(readers=2, writers=2)[String]("ch2", 0)
    run(proc("w1"){ ch2!"from w1"} ||
        proc("w2"){ ch2!"from w2"} ||
        proc("ra"){ ch2?{s=>show(s"ra<-$s")} } ||
        proc("rb"){ ch2?{s=>show(s"rb<-$s")} })

    println(s"==================== Finite sharing test 2")
    val ch3: Chan[String] = Chan.Shared(readers=2, writers=2)[String]("ch3", 0)
    run(||(proc("w1"){ ch3!"from w1"},
           proc("rb"){ Time.sleepms(1000); ch3?{s=>show(s"rb<-$s")} },
           proc("w2"){ ch3.writeBefore(2000*milliSec)("from w2")},
           proc("ra"){ ch3?{s=>show(s"ra<-$s")} },
       ))

    println("===================== Unshared/unbuffered linear termination test")
    val unsharedunbuffered: Chan[Int] = Chan[Int]("Unshared Unbuffered", 0)
    frun((source(unsharedunbuffered, 0 until 16) || sink(unsharedunbuffered) { n => show(s" $n") }))

    println("===================== Unshared/buffered linear termination test")
    val unshared: Chan[Int] = Chan[Int]("Unshared Buffered", 2)
    frun((source(unshared, 0 until 16) || sink(unshared) { n => show(s" $n") }))

    println("===================== Convergent unshared/buffered termination test")
    val conv: Chan[Int] = Chan[Int]("Converge", 2)
    frun((source(conv, 0 until 160)  ||
          source(conv, 16 until 320) ||
          sink(conv) { n => show(s" $n"); if (n==20) Time.sleepms(20) }
        ))

    println("===================== Shared unbuffered linear termination test")
    val shared: Chan[Int] = Chan.Shared(readers = 1, writers = 1)[Int]("Shared", 0)
    frun((source(shared, 0 until 16) || sink(shared) { n => show(s" $n") }))

    println("===================== Multiway shared tests")
    shareTest(0)
    shareTest(1)
    shareTest(10)

  }
}

object test5 extends testFramework {
  import Time._
  override val deadline = seconds(0.95)

  /**
   * This exercises the dynamic test for overtaking in a synchronous channel.
   */
  def test(): Unit = {
    Default.level = INFO
    frun ( proc("deliberate assertion failure") { assert(false, "this is a deliberate assertion failure ")} )

    val c  = Chan[String]("c", 0)
    run ( proc ("caption"){ println("This should report an assertion error, then deadlock (one of the branches fails to terminate)") } ||
           proc ("A"){ c!"A"; c.closeOut(); println("!A done"); System.out.flush() } ||
           proc ("B"){ c!"B"; c.closeOut(); println("!B done"); System.out.flush() } )



    val d  = Chan[String]("d", 0)
    run ( || (
      proc ("caption") { println("This should report an assertion error, then deadlock (one of the branches fails to terminate)") } ,
      proc ("D2") { d?{ s => show(s); d.closeIn() } } ,
      proc ("D1") { d?{ s => show(s); d.closeIn() } } )
    )

    val e  = Chan[String]("e", 0)
    run ( (
      proc ("caption") { println("This should report an assertion error, then deadlock (one of the branches fails to terminate)") } ||
      proc ("E2") { e?{ s => show(s); e.closeIn() } } ||
      proc ("E1") { e?{ s => show(s); e.closeIn() } } )
    )
  }
}

object test6 extends testFramework {
  /**
   * This is a simple test for readBefore/writeBefore.
   *
   * Each trial uses a new channel and it single-shot reads and writes on the channel.
   * The reader in `readDeadline`  waits for a read to complete in `readR`, and
   * the writer delays the write for `waitW`.
   *
   * Readers/writers in  `writeDeadline`  delays the read and awaits the write.
   *
   * All readers(writers) close their input(output) port when done: this
   * averts non-termination of their peer, thus timeout of the test
   * as a whole.
   */

  def test(): Unit = {
     import Time._
     val Sec=seconds(1)

     def readDeadline(chan: Chan[String], writeDelay: Double, readDead: Double): process = {
       val delay = if (writeDelay>readDead) "(reader times out)" else ""
       ||(
         proc ("caption") { println(f"readDeadline($chan%s, writeDelay=${writeDelay}%f, readDead=$readDead)" + delay) },
         proc ("reader")  { val r = chan.readBefore(seconds(readDead)); println(s"reader=$r"); chan.closeIn() },
         proc ("writer")  { sleep(seconds(writeDelay)); chan!"WRITTEN"; println("written"); chan.closeOut() }
       )
     }

     def writeDeadline(chan: Chan[String], writeDead: Double, readDelay: Double): process = {
      val delay = if (writeDead<readDelay) "(writer times out)" else ""
      ||(
        proc ("caption") { println(s"writeDeadline($chan, writeDead=$writeDead, readDelay=$readDelay)" + delay) },
        proc ("reader")  { sleep(seconds(readDelay)); val r = chan?(); println(s"reader?$r"); chan.closeIn() },
        proc ("writer")  { if (chan.writeBefore(seconds(writeDead))("WRITTEN")) println("written") else println("unwritten"); chan.closeOut() }
      )
    }

    //                        write delay     read deadline
    run(readDeadline(Chan(0), (0.01),         (0.5)))
    run(readDeadline(Chan(0), (0.5),          (1.0)))
    run(readDeadline(Chan(0), (1.0),          (1.5)))
    run(readDeadline(Chan(0), (1.499999),     (1.5)))
    run(readDeadline(Chan(0), (0.4999999999), (0.5)))
    run(readDeadline(Chan(1), (1.5),          (1.0)))
    run(readDeadline(Chan(1), (4.0),          (1.5)))
    run(readDeadline(Chan(1), (1.499999),     (1.5)))
    run(readDeadline(Chan(1), (0.4999999999), (0.5)))

    //                         write deadline read delay
    run(writeDeadline(Chan(0), (0.1),         (0.5)))
    run(writeDeadline(Chan(0), (0.5),         (1.0)))
    run(writeDeadline(Chan(0), (1.0),         (0.5)))
    run(writeDeadline(Chan(1), (0.01),        (0.5)))
    run(writeDeadline(Chan(1), (0.5),         (1.0)))
    run(writeDeadline(Chan(1), (1.0),         (0.5)))
    run(writeDeadline(Chan(1), (1.0),         (0.99999999)))

  }
}



