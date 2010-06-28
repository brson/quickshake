package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepClassDecider {
  case class Keep(className: ClassName)
  case class Decide(className: ClassName)
  case object Kept
  case object Waiting
  case object Discarded
  case object End
}

class KeepClassDecider(private val keepNamespaces: List[String]) extends Actor {
  self: Logger =>

  import KeepClassDecider._

  private val internalKeepNamespaces = {
    keepNamespaces map { ClassName.internalize _ }
  }

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

  private val keepSet = new HashSet[ClassName]
  private val requesterMap = new HashMap[ClassName, OutputChannel[Any]]
  
  private def keep(className: ClassName) {

    keepSet.add(className)

    // See if somebody was looking for this class
    requesterMap remove className match {
      case Some(requester) => requester ! Kept
      case None => Unit
    }
  }

  private def decide(className: ClassName, requester: OutputChannel[Any]) {

    debug("Deciding whether to keep " + className)

    // Check if this is in a preserved namespace
    val isInKeptNs = internalKeepNamespaces.foldRight (false) {
      (ns, res) =>
	res || (className isInNamespace ns)
    }
    if (isInKeptNs) requester ! Kept
    // Check if we've already been told to keep it
    else if (keepSet contains className) requester ! Kept
    // Hold on to it for later
    else {
      requester ! Waiting
      requesterMap put (className, requester)
    }
  }

  private def drainRequesters() {
    debug("Discarding remaining class keep decisions")
    for (Pair(_, requester) <- requesterMap) {
      requester ! Discarded
    }
    requesterMap.clear()
  }

}
