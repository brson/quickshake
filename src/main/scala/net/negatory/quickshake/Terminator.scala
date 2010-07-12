package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import actors.OutputChannel


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

  private case class CollectAndResetStickyState(
    remaining: List[Actor],
    keep: List[Actor],
    state: ProcessState,
    requester: OutputChannel[Any]
  )
  private case class WaveResult(stillTracked: List[Actor], state: ProcessState)
  private case class KeepTracking(actors: List[Actor])

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
	    case CollectAndResetStickyState(_, _, _, _) => true
	    case _ => false
	  }
	}

	override def apply(x: Any) = x match {
	  case CollectAndResetStickyState(remaining, keep, state, requester) =>
	    val newState = if (stickyState == Active || state == Active) Active
			   else Passive
	    val newKeep = self :: keep
	    if (remaining != Nil) {
	      remaining.head ! CollectAndResetStickyState(remaining.tail, newKeep, newState, requester)
	    } else {
	      requester ! WaveResult(newKeep, newState)
	    }

	    stickyState = procState
	    react(handler)
	  case _ =>
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
	case CollectAndResetStickyState(remaining, keep, state, requester) =>
	  val newState = if (state == Active) Active
			 else if (state == Passive) Passive
			 else Done
	  if (remaining != Nil) {
	    remaining.head ! CollectAndResetStickyState(remaining.tail, keep, newState, requester)
	  } else {
	    requester ! WaveResult(keep, newState)
	  }
	  //super.exit()
      }

      super.react(f)
    }

  }

  def act() = {
    var tracked: List[Actor] = Nil

    loop {
      react {
	case Register(actor) =>
	  tracked = actor :: tracked
	  debug("Registered actor " + actor)
	case AwaitAllPassive =>
	  val currentTracked = tracked
	  tracked = Nil
	  awaitAllPassive(sender, currentTracked)
	case KeepTracking(actors) =>
	  tracked = tracked ::: actors
	  debug("Continuing to track " + actors.size + " actors")
	case End => exit()
      }
    }
  }

  def awaitAllPassive(requester: OutputChannel[Any], tracked: List[Actor]): Unit = actor {
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
      case WaveResult(stillTracking, Done) =>
	assert(stillTracking == Nil)
	debug("All actors done")
	requester ! AllDone
    }
  }

  def initiateControlWave(requester: OutputChannel[Any], tracked: List[Actor]) = actor {

    debug("Initiating control wave against " + tracked.size + " actors")

    tracked.head ! CollectAndResetStickyState (
      tracked.tail,
      Nil,
      Done,
      requester
    )
  }

}
