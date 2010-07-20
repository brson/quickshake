package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepMethodDecider {
  case class KeepMethod(className: ClassName, methodName: String)

  case class DecideOnMethod(props: MethodProps)
  case class KeptMethod(props: MethodProps)
  case class DiscardedMethod(props: MethodProps)

  case object DrainWaiters
  case class Drained(count: Int)
  case object End
}

class KeepMethodDecider (
  val keepNamespaces: List[String]
) extends Actor with Decider with Logging {

  import KeepMethodDecider._

  def act(): Unit = loop {
    react {
      case KeepMethod(className, methodName) => keepMethod(className, methodName)
      case DecideOnMethod(props) => decideOnMethod(props)
      case KeepMethodDecider.DrainWaiters => drainRequesters()
      case KeepMethodDecider.End =>
	debug("Method decider exiting")
	exit()
    }
  }

  import collection.mutable.{HashSet, HashMap}
  import actors.OutputChannel

  private val keepSet = new HashSet[(ClassName, String)]
  private val requesterMap =
    new HashMap[(ClassName, String), List[(MethodProps, OutputChannel[Any])]]

  private def keepMethod(className: ClassName, methodName: String) {

    keepSet += Pair(className, methodName)

    if (requesterMap contains (className, methodName)) {
      val requesterList = requesterMap((className, methodName))
      requesterList foreach {
	item =>
	  val (props, requester) = item
	  requester ! KeptMethod(props)
      }
      requesterMap -= Pair(className, methodName)
    }
  }

  private def decideOnMethod(props: MethodProps) {
    val MethodProps(className, methodName, _, _) = props

    debug("Deciding whether to keep method " + methodName)
    if (isInKeptNs(className)) {
      sender ! KeptMethod(props)
    } else if (keepSet contains (className, methodName)) {
      sender ! KeptMethod(props)
    } else {
      val currentList = if (requesterMap contains (className, methodName)) requesterMap((className, methodName))
			else Nil
      requesterMap put ((className, methodName), (props, sender) :: currentList)
      
    }
  }

  private def drainRequesters() {
    debug("Discarding remaining method keep decisions")

    for (
      (_, requests) <- requesterMap;
      (props, requester) <- requests
    ) {
      requester ! DiscardedMethod(props)
    }
    val count = requesterMap.size
    requesterMap.clear()
    reply(Drained(count))
  }

}
