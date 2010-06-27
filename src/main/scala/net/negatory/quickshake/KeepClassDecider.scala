package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepClassDecider {
  case class Keep(className: String)
  case class Decide(className: String)
  case object Kept
  case object Discarded
  case object End
}

class KeepClassDecider(private val keepNamespace: String) extends Actor {
  self: Logger =>

  import KeepClassDecider._

  def act() {
    loop {
      react {
	case Keep(className) => keep(className)
	case Decide(className) => decide(className, sender)
	case End => 
	  drainRequesters()
	  debug("Decider exiting")
	  exit
      }
    }
  }

  import collection.mutable.{HashSet, HashMap}
  import actors.OutputChannel

  private val keepSet = new HashSet[String]
  private val requesterMap = new HashMap[String, OutputChannel[Any]]
  
  private def keep(className: String) {
    keepSet.add(className)

    // See if somebody was looking for this class
    requesterMap remove className match {
      case Some(requester) => requester ! Kept
      case None => Unit
    }
  }

  private def decide(className: String, requester: OutputChannel[Any]) {

    debug("Deciding whether to keep " + className)

    def withInternalSeparators(name: String) = name map { 
      (char) =>
      if (char == '.') '/'
      else char
    }

    val internalishNamespace = withInternalSeparators(keepNamespace)
    debug("Comparing " + className + " to " + internalishNamespace)

    // Check if this is in a preserved namespace
    if (className contains internalishNamespace) requester ! Kept
    // Check if we've already been told to keep it
    else if (keepSet contains className) requester ! Kept
    // Hold on to it for later
    else requesterMap put (className, requester)
  }

  private def drainRequesters() {
    debug("Discarding remaining class keep decisions")
    for (Pair(_, requester) <- requesterMap) {
      requester ! Discarded
    }
    requesterMap.clear()
  }

}
