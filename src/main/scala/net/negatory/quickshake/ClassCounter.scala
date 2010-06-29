package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ClassCounter {
  case object Inspected
  case object Kept
  case object Discarded
  case object GetResults
  case class Results(inspected: Int, kept: Int, Discarded: Int)
}

class ClassCounter extends Actor {
  import ClassCounter._
  import actors.OutputChannel

  var inspected = 0
  var kept = 0
  var discarded = 0

  var resultRequester: Option[OutputChannel[Any]] = None

  def checkResults() = if (kept + discarded == inspected) {
    assert(resultRequester.isDefined)
    resultRequester.get ! Results(inspected, kept, discarded)
    exit
  }

  def act() = loop {
    react {
      case Inspected => inspected += 1
      case Kept => kept += 1
      case Discarded => discarded += 1
      case GetResults => {
	resultRequester = Some(sender)
	checkResults()
	// If we haven't heard back from all the classes we inspected
	// then keep waiting for the numbers to add up
	waitForResults()
      }
    }
  }

  private def waitForResults() = loop {
    react {
      case Inspected => {
	inspected += 1
	checkResults()
      }
      case Kept => {
	kept += 1
	checkResults()
      }
      case Discarded => {
	discarded += 1
	checkResults()
      }
    }
  }

}
