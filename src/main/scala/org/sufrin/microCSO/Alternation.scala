package org.sufrin.microCSO

import org.sufrin.microCSO.Time.{microSec, Nanoseconds}
import org.sufrin.microCSO.proc.{repeatedly, stop}

import java.util.concurrent.atomic.AtomicReference

object Alternation  {
  private final val nanoDelta: Nanoseconds = 5*microSec

  trait Port[T] {
    def asIn:  InPort[T]  = null
    def asOut: OutPort[T] = null
  }

  /** Outcome of a tentative read/write */
  trait  AltOutcome {}
  class  AltResult[T] extends AtomicReference[T]

  object AltOutcome {
    case object OK extends AltOutcome
    case object CLOSED extends AltOutcome
    case object NO extends AltOutcome
  }

  case class Out[T](port: OutPort[T]) extends Port[T] { override def asOut = port }
  case class In[T](port: InPort[T]) extends Port[T]   { override def asIn: InPort[T] = port }
  case class InOut[T](chan: Chan[T]) extends Port[T]  {
    override def asIn: InPort[T]   = chan.in
    override def asOut: OutPort[T] = chan.out
  }

  trait Event
  case class `Input-Event`[T](guard: () => Boolean, port: InPort[T], f: T => Unit) extends Event
  case class `Output-Event`[T](guard: () => Boolean, port: OutPort[T], f: () => T) extends Event
  case class `Or-Else`(eval: ()=>Unit)
  case class `After-NS`(ns: Nanoseconds, eval: ()=>Unit)

  import AltOutcome._

  /**
   *  Repeatedly find and fire one of the `events`, until
   *  none are feasible. The search for firing events is in
   *  sequential order: it is "fair" inasmuch as
   *  at each iteration `events` is rotated by a single place.
   *
   *  The "Finding" is done by polling periodically (the period is half a microsecond). This
   *  is somewhat inelegant, but drastically simplifies the implementation.
   *
   * @see ServeBefore
   */
  def Serve(events: Seq[Event]): Unit = {
    val fairness = new Rotater(events)
    repeatedly {
      eventFired(fairness, deadline=0, afterDeadline = {()=>}) match {
        case true  => fairness.rot()
        case false => stop
      }
    }
  }
  /**
   *  Repeatedly find and fire one of the `events` until
   *  none are feasible. The search for firing events is in
   *  sequential order: it is "fair" inasmuch as
   *  at each iteration `events` is rotated by a single place.
   *
   * The "finding" is performed by polling every `waitDelta` nanoseconds until
   * {{{
   *   (a) one of the `events` has fired -- then rotate `events` and continue serving
   *   (b) the deadline (if positive) has expired -- then evaluate `afterDeadline` and continue serving
   *   (c) none of the `events` are feasible -- then evaluate `orElse`, and stop
   * }}}
   */
  def ServeBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = nanoDelta )(events: Seq[Event]): Unit = {
    val fairness = new Rotater(events)
    repeatedly {
      eventFired(fairness, deadline, afterDeadline = {()=>afterDeadline}) match {
        case true  =>
          fairness.rot()
        case false =>
          orElse
          stop
      }
    }
  }

  /**
   * @see ServeBefore
   */
  def serveBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = nanoDelta )(events: Event*): Unit = {
    val fairness = new Rotater(events)
    repeatedly {
      eventFired(fairness, deadline, afterDeadline = {()=>afterDeadline}) match {
        case true  =>
          fairness.rot()
        case false =>
          orElse
          stop
      }
    }
  }

  /**
   *  Repeatedly find one of the `events` that fires, until
   *  none are feasible. The search for firing events is in
   *  sequential order: it is "fair" inasmuch as
   *  at each iteration `events` is rotated by a single place.
   *
   *  The "Finding" is done by polling periodically (the period is half a microsecond). This
   *  is somewhat inelegant, but drastically simplifies the implementation.
   *
   * @see serveBefore
   */
  def serve(events: Event*): Unit = Serve(events)

  /**
   * Poll periodically until one of the `events` fires or none is feasible.
   * In the latter case an error is thrown.
   *
   * @see altBefore
   */
  def alt(events: Event*): Unit = Alt(events)

  /**
   * Poll periodically until one of the `events` fires or none is feasible. In
   * the latter case an error is thrown.
   *
   * @see AltBefore
   */
  def Alt(events: Seq[Event]): Unit =
    eventFired(events, deadline=0, afterDeadline = {()=>})  match {
      case false => throw new Error("alternation: no feasible events")
      case true =>
    }

  /**
   * Poll every `waitDelta` nanoseconds until
   * {{{
   *   (a) one of the `events` has fired
   *   (b) the deadline (if positive) has expired -- then evaluate `afterDeadline`
   *   (c) none of the `events` are feasible -- then evaluate `orElse`.
   * }}}
   *
   */
  def altBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {} , waitDelta: Nanoseconds = nanoDelta)(events: Event*): Unit =
    eventFired(events, deadline, afterDeadline = { ()=>afterDeadline }, waitDelta) match {
      case false => orElse
      case true  =>
    }

  /**
   * Poll every `waitDelta` nanoseconds until
   * {{{
   *   (a) one of the `events` has fired
   *   (b) the deadline (if positive) has expired -- then evaluate `afterDeadline`
   *   (c) none of the `events` are feasible -- then evaluate `orElse`
   * }}}
   *
   */
  def AltBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = nanoDelta )(events: Seq[Event]): Unit =
    eventFired(events, deadline, afterDeadline = { ()=>afterDeadline }, waitDelta) match {
      case false => orElse
      case true  =>
    }

  /**
   *  (busy) wait until one of `events`  fires or none are feasible.
   *  Yield true in the former case and false in the latter case.
   *
   *  If there's a positive `deadline` then
   *  evaluate `afterDeadline()` and return true if none has fired before the
   *  deadline has elapsed; else just continue (busy) waiting.
   *
   *  `waitDelta` is the delay between successive attempts to find and fire an event.
   *
   */
  def eventFired(events: Seq[Event], deadline: Nanoseconds=0, afterDeadline: ()=>Unit, waitDelta: Nanoseconds = nanoDelta): Boolean = {
    var outcome = false
    var waiting = true
    var remainingTime = deadline
    val hasDeadline   = remainingTime>0

    while (waiting) {
      val (feasibles, result) = fireFirst(events)
      // result is OK or all events refused or infeasible
      result match {
        case OK =>
          // an event fired
          outcome = true
          waiting = false
        case CLOSED | NO =>
          // no event seen on this pass fired
          if (feasibles == 0) {
            // now nothing CAN happen
            // print("* "); System.out.flush()
            outcome = false
            waiting = false
          } else if (hasDeadline && remainingTime<=0) {
            afterDeadline()
            outcome = true
            waiting = false
          } else {
            // wait a bit and try again
            Time.sleep(waitDelta)
            remainingTime -= waitDelta
          }
      }
    }
    outcome
  }

  /**
   * Find and fire the first ready event.
   * Return the number of feasible events.
   */
  @inline private final def fireFirst(events: Seq[Event]): (Int, AltOutcome) = {
    var outcome:  AltOutcome = NO
    var feasible: Int = 0
    val feasibles = events.filter{
      case `Output-Event`(guard, port, _) => guard() && port.canOutput
      case `Input-Event`(guard, port, _) => guard() && port.canInput
    }
    //if (feasibles.isEmpty) Default.finer(s"fireFirst ${feasibles.length} feasible")
    val iter: Iterator[Event] = feasibles.iterator
    // find and fire the first ready event
    while (outcome!=OK && iter.hasNext) {
      iter.next() match {
        case `Output-Event`(guard, port, f) =>
          outcome = port.offer(f)
          outcome match {
            case OK =>
              feasible += 1
            case CLOSED =>
              outcome = CLOSED
            case NO     =>
              outcome = NO            }
        case `Input-Event`(guard, port, f) =>
          val result = new AltResult[Any]
          port.poll(result) match {
            case OK     =>
              f(result.get)
              outcome = OK
              feasible += 1
            case CLOSED =>
              outcome = CLOSED
            case NO     =>
              outcome = NO
          }
      }
    }
    // outcome==OK || all events refused or infeasible // TODO: need to distinguish?
    // Default.finer(s"fireFirst $feasible/${feasibles.length} $outcome")
    (feasibles.length, outcome)
  }

  class Rotater[T](val original: Seq[T]) extends Seq[T] {
    val length: Int = original.length
    private val LENGTH = length
    private var offset: Int = 0
    def rot(): Unit = { offset = (offset+1) % LENGTH}

    def iterator: Iterator[T] = new Iterator[T] {
      var current: Int = offset
      var count : Int  = LENGTH
      def hasNext: Boolean = count>0

      def next(): T = {
        val r = original(current)
        current = (current + 1) % LENGTH
        count -=1
        r
      }
    }

    def apply(i: Int): T = original((offset+i)%LENGTH)
  }
}
