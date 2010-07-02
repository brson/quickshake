package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepClassDecider {
  case class Keep(className: ClassName)
  case object DoneKeeping
  case class Decide(className: ClassName, preWakeAction: () => Unit)
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
	case Keep(className) => keep(className)
	case Decide(className, preWakeAction) => decide(className, preWakeAction)
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
  private val requesterMap = new HashMap[ClassName, Pair[OutputChannel[Any], () => Unit]]
  
  private def keep(className: ClassName) {

    keepSet += className

    if (requesterMap contains className) {
      val (requester, preWakeAction) = requesterMap(className)
      preWakeAction()
      requester ! Kept
      requesterMap -= className
    }
    reply(DoneKeeping)
  }

  private def decide(className: ClassName, preWakeAction: () => Unit) {

    debug("Deciding whether to keep " + className)

    // Check if this is in a preserved namespace
    val isInKeptNs = internalKeepNamespaces.foldLeft (false) {
      (res, ns) =>
	res || (className isInNamespace ns)
    }
    if (isInKeptNs) {
      sender ! Kept
    }
    // Check if we've already been told to keep it
    else if (keepSet contains className) {
      sender ! Kept
    }
    // Hold on to it for later
    else {
      sender ! Waiting
      requesterMap put (className, (sender, preWakeAction))
    }
  }

  private def drainRequesters() {
    debug("Discarding remaining class keep decisions")
    for (Pair(_, Pair(requester, preWakeAction)) <- requesterMap) {
      preWakeAction()
      requester ! Discarded
    }
    requesterMap.clear()
  }

}
