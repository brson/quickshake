package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepClassDecider {
  case class Keep(className: ClassName, preWakeAction: () => Unit)
  case class Decide(className: ClassName)
  case object Kept
  case object Waiting
  case object Discarded
  case object DrainWaiters
  case object End
}

class KeepClassDecider(
  keepNamespaces: List[String]
) extends Actor with Logging {

  import KeepClassDecider._

  private val internalKeepNamespaces = {
    keepNamespaces map { ClassName.internalize _ }
  }

  def act() {
    loop {
      react {
	case Keep(className, preWakeAction) => keep(className, preWakeAction)
	case Decide(className) => decide(className, sender)
	case DrainWaiters => drainRequesters()
	case End => 
	  debug("Decider exiting")
	  exit
      }
    }
  }

  import collection.mutable.{HashSet, HashMap}
  import actors.OutputChannel

  private val keepSet = new HashSet[ClassName]
  private val requesterMap = new HashMap[ClassName, OutputChannel[Any]]
  
  private def keep(className: ClassName, preWakeAction: () => Unit) {

    keepSet += className

    if (requesterMap contains className) {
      preWakeAction()
      val requester = requesterMap(className)
      requester ! Kept
      requesterMap -= className
    }
    reply(())
  }

  private def decide(className: ClassName, requester: OutputChannel[Any]) {

    debug("Deciding whether to keep " + className)

    // Check if this is in a preserved namespace
    val isInKeptNs = internalKeepNamespaces.foldLeft (false) {
      (res, ns) =>
	res || (className isInNamespace ns)
    }
    if (isInKeptNs) {
      requester ! Kept
    }
    // Check if we've already been told to keep it
    else if (keepSet contains className) {
      requester ! Kept
    }
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
