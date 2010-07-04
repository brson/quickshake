package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ExitHandler {
  case object End
  case class Watch(actor: Actor)
}

class ExitHandler extends Actor with Logging {

  import ExitHandler._
  import actors.Exit

  val handler = this

  trait TrapMixin extends Actor {
    handler !? Watch(this)
  }

  trapExit = true

  def act() = loop {
    react {
      case Watch(actor) => link(actor); reply(())
      case Exit(from, ex: Exception) => {
	ex.printStackTrace
	error("Actor failed: " + ex)
      }
      case Exit(from, _) => ()
      case End => exit()
    }
  }
}

