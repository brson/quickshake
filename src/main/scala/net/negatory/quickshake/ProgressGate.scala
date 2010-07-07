package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ProgressGate {
  case class Tasks(total: Int)
  case object OneStarted
  case object OneComplete
  case object OneBlocked
  case object OneResumed
  case object AlertWhenAllBlocked
  case object AllBlocked
  case object AlertWhenAllComplete
  case object AllComplete
  case object End
}

class ProgressGate extends Actor {
  def act() {
    import actors.OutputChannel
    import ProgressGate._

    var tasks: Option[Int] = None
    var started = 0
    var blocked = 0
    var complete = 0
    def live = started - complete - blocked

    var blockedListener: Option[OutputChannel[Any]] = None
    var completeListener: Option[OutputChannel[Any]] = None

    def allComplete = tasks match {
      case Some(tasks) => complete == tasks
      case None => false
    }
    def allBlocked = tasks match { 
      case Some(tasks) => started == tasks && live == 0
      case None => false
    }

    def checkConditions() = {
      if (allBlocked) {
	blockedListener match {
	  case Some(listener) =>
	    listener ! AllBlocked
	    blockedListener = None
	  case None => ()
	}
      }
      
      if (allComplete) {
	completeListener match {
	  case Some(listener) =>
	    listener ! AllComplete
	    completeListener = None
	  case None => ()
	}
      }
    }

    loop {
      react {
	val f: PartialFunction[Any, Unit] = {
	  case Tasks(total) => tasks = Some(total)
	  case OneStarted => started += 1
	  case OneComplete => complete += 1
	  case OneBlocked => blocked += 1
	  case OneResumed => blocked -= 1
	  case AlertWhenAllBlocked => blockedListener = Some(sender)
	  case AlertWhenAllComplete => completeListener = Some(sender)
	  case End => exit()
	}

	f andThen { _ =>
	  checkConditions()
	}
      }
    }

  }

}
