package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object KeepClassDecider {
  case class KeepClass(className: ClassName)
  case class KeepMethod(methodName: String)
  case object DoneKeeping
  case class DecideOnClass(className: ClassName)
  case class DecideOnMethod(className: ClassName, methodName: String)
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
	case KeepClass(className) => keepClass(className)
	case KeepMethod(methodName) => keepMethod(methodName)
	case DecideOnClass(className) => decideOnClass(className)
	case DecideOnMethod(className, methodName) => decideOnMethod(className, methodName)
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
  private val methodSet = new HashSet[String]
  private val methodRequesterMap = new HashMap[String, List[OutputChannel[Any]]]
  
  private def keepClass(className: ClassName) {

    keepSet += className

    if (requesterMap contains className) {
      val requester = requesterMap(className)
      requester ! Kept
      requesterMap -= className
    }
    reply(DoneKeeping)
  }

  private def keepMethod(methodName: String) {

    methodSet += methodName

    if (methodRequesterMap contains methodName) {
      val requesterList = methodRequesterMap(methodName)
      requesterList foreach { _ ! Kept }
      methodRequesterMap -= methodName
    }
    reply(DoneKeeping)
  }

  private def isInKeptNs(className: ClassName) = internalKeepNamespaces.foldLeft (false) {
    (res, ns) =>
      res || (className isInNamespace ns)
  }

  private def decideOnClass(className: ClassName) {

    debug("Deciding whether to keep " + className)

    // Check if this is in a preserved namespace
    if (isInKeptNs(className)) {
      sender ! Kept
    }
    // Check if we've already been told to keep it
    else if (keepSet contains className) {
      sender ! Kept
    }
    // Hold on to it for later
    else {
      sender ! Waiting
      requesterMap put (className, sender)
    }
  }

  private def decideOnMethod(
    className: ClassName,
    methodName: String
  ) {
    debug("Deciding whether to keep method " + methodName)
    if (isInKeptNs(className)) {
      sender ! Kept
    } else if (methodSet contains methodName) {
      sender ! Kept
    } else {
      sender ! Waiting
      val currentList = if (methodRequesterMap contains methodName) methodRequesterMap(methodName)
			else Nil
      methodRequesterMap put (methodName, sender :: currentList)
      
    }
  }

  private def drainRequesters() {
    debug("Discarding remaining class keep decisions")

    for (
      (_, requests) <- methodRequesterMap;
      requester <- requests
    ) {
      requester ! Discarded
    }
    methodRequesterMap.clear()

    for ((_, requester) <- requesterMap) {
      requester ! Discarded
    }
    requesterMap.clear()
  }

}
