package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object MethodCoordinator {
  // These will be used to track the set of methods
  // on a class that need to be retained
  case class KeepMethod(methodName: String)
  case object DiscardMethod
}

class MethodCoordinator(
  props: MethodProps,
  // TODO: Sending the results to the accumulator one at a time is inefficient
  methodAccumulator: Actor,
  decider: KeepClassDecider
) extends Actor with Logging {

  def act() {
    val MethodProps(className, methodName, classDeps, methodDeps) = props
    
    decider ! KeepClassDecider.DecideOnMethod(props)
    react {
      case KeepClassDecider.Kept =>
	methodAccumulator ! MethodCoordinator.KeepMethod(methodName)
	classDeps foreach {
	  decider ! KeepClassDecider.KeepClass(_)
	}
	methodDeps foreach {
	  decider ! KeepClassDecider.KeepMethod(_)
	}
	exit()
      case KeepClassDecider.Discarded =>
	methodAccumulator ! MethodCoordinator.DiscardMethod
	exit()
    }
  }
}

