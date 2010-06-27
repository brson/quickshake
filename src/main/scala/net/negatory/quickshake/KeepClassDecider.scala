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
	case Keep(_) => 
	case Decide(className) => reply(decide(className))
	case End => 
	  debug("Decider exiting")
	  exit
      }
    }
  }
  
  private def decide(className: String): Any = {

    debug("Deciding whether to keep " + className)

    def withInternalSeparators(name: String) = name map { 
      (char) =>
      if (char == '.') '/'
      else char
    }

    val internalishNamespace = withInternalSeparators(keepNamespace)
    debug("Comparing " + className + " to " + internalishNamespace)

    if (className contains internalishNamespace) Kept
    else Discarded
  }
}
