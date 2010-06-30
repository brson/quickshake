package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ActorTracker {
  private[ActorTracker] case class Register(actor: Actor)
  private[ActorTracker] case class Unregister(actor: Actor)
  private[ActorTracker] case object End
}

class ActorTracker extends AnyRef with Logging {

  import ActorTracker._
  import concurrent.SyncVar

  private val sync = new SyncVar[Unit]

  private val registrar = actor {
    var counter = 0
    loop {
      react {
	case Register(actor) =>
	  debug("Registering actor " + actor.toString)
	  counter += 1
	case Unregister(actor) =>
	  debug("Unregistering actor " + actor.toString)
	  counter -= 1
	  if (counter == 0) {
	    sync.set(())
	    exit
	  }
      }
    }
  }

  trait TrackerMixin extends Actor {
    type TrackerMixin = ActorTracker.this.TrackerMixin

    val guard = actor {
      var tracking = false
      loop {
	self.react {
	  case r @ Register(_) => if (!tracking) {
	    tracking = true
	    registrar ! r
	  }
	  case u @ Unregister(_) => if (tracking) {
	    tracking = false
	    registrar ! u
	  }
	  case End => exit()
	}
      }
    }
    
    abstract override def start(): Actor = {
      guard ! Register(this)
      super.start()
    }
    abstract override def exit(): Nothing = {
      stopTracking()
      guard ! End
      super.exit()
    }
    def stopTracking() {
      guard ! Unregister(this)
    }
  }

  // This is the first actor registered, and it is unregistered once
  // the caller wait's for actors. Should prevent the possibility of
  // the actor count reaching 0 before all actors have been started.
  val initBlocker = new Actor with TrackerMixin {

    def act() {
      react {
	case End => exit
      }
    }
  }.start()

  def waitForActors() = {
    initBlocker ! End
    sync.get
  }
}

