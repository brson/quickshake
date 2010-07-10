package net.negatory.quickshake

import actors.Actor
import actors.Actor._

/*
 * Implements Mattern's
 * 'Distributed Termination Detection with Sticky State Indicators'
 */

// TODO: Implement 'lazy' waves, only propogating when an actor becomes passive
// This would require explicitly identifying when actors are passive

object Terminator {
  sealed trait ProcessState
  case object Active extends ProcessState
  case object Passive extends ProcessState

  case class Register(actor: Actor)
  case object AwaitTermination
  case object Terminated

  case object CollectAndResetStickyState
  case class StickyState(state: ProcessState)

  case object End
}

class Terminator extends Actor {

  import Terminator._

  trait TerminationMixin extends Actor {

    private var procState: ProcessState = Active
    private var stickyState: ProcessState = Active

    abstract override def start(): Actor = {
      val ret = super.start()
      Terminator.this ! Register(this)
      ret
    }
    
    abstract override def react(handler: PartialFunction[Any, Unit]): Nothing = {
      procState = Passive

      val h = new PartialFunction[Any, Unit] {
	override def isDefinedAt(x: Any) = handler.isDefinedAt(x) || {
	  x match {
	    case CollectAndResetStickyState => true
	    case _ => false
	  }
	}

	override def apply(x: Any) = if (handler.isDefinedAt(x)) {
	  procState = Active
	  stickyState = Active

	  handler(x)
	} else {
	  assert(x == CollectAndResetStickyState)

	  reply(StickyState(stickyState))
	  stickyState = procState
	}
      }

      super.react(h)
    }
					
    abstract override def exit(): Nothing = {
      react {
	case End => super.exit()
      }
    }
  }

  def act() = {
    var tracked: List[Actor] = Nil

    loop {
      react {
	case Register(actor) => tracked = actor :: tracked
	case AwaitTermination => awaitTermination(sender, tracked) 
	case End => endActors(tracked); exit()
      }
    }
  }

  import actors.OutputChannel

  def awaitTermination(requester: OutputChannel[Any], tracked: List[Actor]): Unit = actor {
    initiateControlWave(self, tracked)
    react {
      case StickyState(Active) => awaitTermination(requester, tracked)
      case StickyState(Passive) => requester ! Terminated
    }
  }

  def initiateControlWave(requester: OutputChannel[Any], tracked: List[Actor]) = actor {
    val total = tracked.foldRight (0) {
      (a, t) => 
	a ! CollectAndResetStickyState
	t + 1
    }

    var passive = 0
    var active = 0
    def recvd = passive + active

    loopWhile (recvd < total) {
      react {
	case StickyState(Active) => active += 1
	case StickyState(Passive) => passive += 1
      }
    } andThen {
      requester ! StickyState(if (active > 0) Active else Passive)
    }
  }

  def endActors(tracked: List[Actor]): Unit = tracked foreach { _ ! End }
}
