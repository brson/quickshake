package net.negatory.quickshake

import actors.Actor
import actors.Actor._

/*
 * Implements Mattern's
 * 'Distributed Termination Detection with Sticky State Indicators'
 */

// TODO: Implement 'lazy' waves, only propogating when an actor becomes passive
// This would require explicitly identifying when actors are passive. Probably
// the easiest way to to this would be to create a reactFast method that
// doesn't put the actor into a passive state, then gradually find which
// reacts shouldn't 'block'

object Terminator {
  sealed trait ProcessState
  case object Active extends ProcessState
  case object Passive extends ProcessState
  case object Done extends ProcessState

  case class Register(actor: Actor)
  case object AwaitAllPassive
  case object AllPassive
  case object AllDone

  case object CollectAndResetStickyState
  case class StickyState(actor: Actor, state: ProcessState)
  case class WaveResult(stillTracked: List[Actor], state: ProcessState)
  case class KeepTracking(actors: List[Actor])

  case object End
}

class Terminator extends Actor with Logging {

  import Terminator._
  override def minLogLevel = LogLevel.Debug
  trait TerminationMixin extends Actor with Logging {

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

	override def apply(x: Any) = if (x == CollectAndResetStickyState) {
	  assert(x == CollectAndResetStickyState)

	  reply(StickyState(self, stickyState))
	  stickyState = procState
	} else {
	  assert(handler.isDefinedAt(x))
	  procState = Active
	  stickyState = Active

	  handler(x)
	}
      }

      super.react(h)
    }
					
    abstract override def exit(): Nothing = {
      debug("Waiting for end message to exit tracked actor " + this)

      lazy val f: PartialFunction[Any, Unit] = {
	case CollectAndResetStickyState =>
	  reply(StickyState(self, Done))
	  super.exit()
	/*case End =>
	  debug("Exiting tracked actor " + this)
	  super.exit()*/
      }

      super.react(f)
    }

  }

  def act() = {
    var tracked: List[Actor] = Nil

    loop {
      react {
	case Register(actor) => tracked = actor :: tracked
	case AwaitAllPassive =>
	  val currentTracked = tracked
	  tracked = Nil
	  awaitAllPassive(sender, currentTracked)
	case KeepTracking(actors) => tracked = tracked ::: actors
	case End => exit()
      }
    }
  }

  import actors.OutputChannel

  def awaitAllPassive(requester: OutputChannel[Any], tracked: List[Actor]): Unit = actor {
    debug("Awaiting termination")
    initiateControlWave(self, tracked)
    self.react {
      case WaveResult(stillTracking, Active) =>
	Terminator.this ! KeepTracking(stillTracking)
	Terminator.this ! AwaitAllPassive
	self.react {
	  case result => requester ! result
	}
      case WaveResult(stillTracking, Passive) =>
	Terminator.this ! KeepTracking(stillTracking)
	debug("All actors passive")
	requester ! AllPassive
      case WaveResult(_, Done) =>
	debug("All actors done")
	requester ! AllDone
    }
  }

  def initiateControlWave(requester: OutputChannel[Any], tracked: List[Actor]) = actor {
    debug("Initiating control wave")
    val total = tracked.foldLeft (0) {
      (t, a) => 
	a ! CollectAndResetStickyState
	t + 1
    }

    var active = 0
    var passive = 0
    var done = 0
    def recvd = passive + active + done

    var stillTracked: List[Actor] = Nil

    loopWhile (recvd < total) {
      self.react {
	case StickyState(a, Active) => active += 1; stillTracked = a :: stillTracked
	case StickyState(a, Passive) => passive += 1; stillTracked = a :: stillTracked
	case StickyState(a, Done) => done += 1
      }
    } andThen {
      debug("Found " + active + " active " + passive + " passive " + done + " done")
      requester ! WaveResult(stillTracked, 
	if (active > 0) Active
	else if (passive > 0) Passive
	else Done
      )
    }
  }

}
