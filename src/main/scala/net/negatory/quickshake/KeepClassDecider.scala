package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepClassDecider {
  case class KeepClass(className: ClassName)

  case class DecideOnClass(className: ClassName)
  case object Kept
  case object Discarded

  case object DrainWaiters
  case object End
}

class KeepClassDecider(
  val keepNamespaces: List[String]
) extends Actor with Decider with Logging {

  import KeepClassDecider._

  def act(): Unit = loop {
    react {
      case KeepClass(className) => keepClass(className)
      case DecideOnClass(className) => decideOnClass(className)
      case KeepClassDecider.DrainWaiters => drainRequesters()
      case KeepClassDecider.End =>
	debug("Class decider exiting")
	exit()
    }
  }

  import collection.mutable.{HashSet, HashMap}
  import actors.OutputChannel

  private val keepSet = new HashSet[ClassName]
  private val requesterMap = new HashMap[ClassName, OutputChannel[Any]]
  
  private def keepClass(className: ClassName) {

    keepSet += className

    if (requesterMap contains className) {
      val requester = requesterMap(className)
      requester ! Kept
      requesterMap -= className
    }
  }

  private def decideOnClass(className: ClassName) {

    debug("Deciding whether to keep " + className)

    // Check if this is in a preserved namespace
    if (isInKeptNs(className)) {
      reply(Kept)
    }
    // Check if we've already been told to keep it
    else if (keepSet contains className) {
      reply(Kept)
    }
    // Hold on to it for later
    else {
      assert(!(requesterMap contains className))
      requesterMap put (className, sender)
    }
  }

  private def drainRequesters() {
    debug("Discarding remaining class keep decisions")

    for ((_, requester) <- requesterMap) {
      requester ! Discarded
    }
    requesterMap.clear()
  }

}


