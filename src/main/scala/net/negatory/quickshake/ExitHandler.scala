package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ExitHandler {
  case class Watch(actor: Actor)
  case object Watched
}

class ExitHandler extends Actor with Logging {

  import ExitHandler._
  import actors.Exit

  val handler = this
  var watched = 0

  trait TrapMixin extends Actor {
    handler !? Watch(this)
  }

  trapExit = true

  def act() = loop {
    react {
      case Watch(actor) =>
	watched += 1
	link(actor)
	reply(Watched)
      case Exit(from, ex: Exception) => {
	ex.printStackTrace
	error("Actor failed: " + ex)
	tryExit()
      }
      case Exit(from, _) => tryExit()
     }
  }

  private def tryExit() {
    watched -= 1
    if (watched == 0) {
      debug("Exit handler has nothing left to monitor. Exiting.")
      exit()
    }
  }
}

