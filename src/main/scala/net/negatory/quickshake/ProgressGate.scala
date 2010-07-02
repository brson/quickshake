package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ProgressGate {
  case class Candidates(total: Int)
  case object OneStarted
  case object OneKept
  case object OneDiscarded
  case object OneWaiting
  case object OneResumed
  case object WaitUntilAllSeen
  case object AllSeen
  case object WaitUntilAllProcessed
  case object AllProcessed
  case object GetTotals
  case class Totals(candidates: Int, kept: Int, discarded: Int)
  case object End
}

class ProgressGate extends Actor {
  def act() {
    import actors.OutputChannel
    import ProgressGate._

    var candidates: Option[Int] = None
    var started = 0
    var kept = 0
    var discarded = 0
    var waiting = 0
    def processed = kept + discarded
    def live = started - kept - discarded - waiting

    var seenWaiter: Option[OutputChannel[Any]] = None
    var processedWaiter: Option[OutputChannel[Any]] = None

    def printStatus() {
      println(candidates + " " + started + " " + kept + " " + discarded + " " + waiting + " " + live)
    }

    def checkConditions() {
      candidates match {
	case Some(candidates) =>
	  assert(started <= candidates)
	  if (processed == candidates) {
	    seenWaiter match {
	      case Some(actor) =>
		actor ! AllSeen
		seenWaiter = None
	      case None => ()
	    }
	    processedWaiter match {
	      case Some(actor) =>
		actor ! AllProcessed
		processedWaiter = None
	      case None => ()
	    }
	  } else if (started == candidates && live == 0) {
	    printStatus()
	    seenWaiter match {
	      case Some(actor) =>
		actor ! AllSeen
		seenWaiter = None
	      case None => ()
	  }
	}
	case None => ()
      }
    }

    loop {
      react {
	case Candidates(total) =>
	  candidates = Some(total)
	  checkConditions()
	case OneStarted =>
	  started += 1
	  checkConditions()
	case OneKept => 
	  kept += 1
	  checkConditions()
	case OneDiscarded =>
	  discarded += 1
	  checkConditions()
	case OneWaiting =>
	  waiting += 1
	  checkConditions()
	case OneResumed =>
	  waiting -= 1
	  checkConditions()
	case WaitUntilAllSeen =>
	  seenWaiter = Some(sender)
	  checkConditions()
	case WaitUntilAllProcessed =>
	  processedWaiter = Some(sender)
	  checkConditions()
	case GetTotals =>
	  assert(candidates isDefined)
	  reply(Totals(candidates.get, kept, discarded))
	case End => exit()
      }
    }
  }

}
