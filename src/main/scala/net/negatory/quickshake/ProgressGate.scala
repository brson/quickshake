package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ProgressGate {
  case class Candidates(total: Int)
  case object OneStarted
  case object OneProcessed
  case object OneWaiting
  case object OneResumed
  case object WaitUntilAllSeen
  case object AllSeen
  case object WaitUntilAllProcessed
  case object AllProcessed
  case object End
}

class ProgressGate extends Actor {
  def act() {
    import actors.OutputChannel
    import ProgressGate._

    var candidates: Option[Int] = None
    var started = 0
    var waiting = 0
    var processed = 0
    def live = started - processed - waiting

    var seenWaiter: Option[OutputChannel[Any]] = None
    var processedWaiter: Option[OutputChannel[Any]] = None

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
	case OneProcessed =>
	  processed += 1
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
	case End => exit()
      }
    }
  }

}
