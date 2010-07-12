package net.negatory.quickshake

import actors.Actor
import actors.Actor._

case class MethodProps(
  className: ClassName,
  methodName: String,
  classDeps: List[ClassName],
  methodDeps: List[String]
)

object MethodCoordinator {
  // These will be used to track the set of methods
  // on a class that need to be retained
  case class KeepMethod(methodName: String)
  case object DiscardMethod
}

class MethodCoordinator(
  props: MethodProps,
  methodAccumulator: Actor,
  decider: KeepClassDecider
) extends Actor with Logging {

  def act() {
    val MethodProps(className, methodName, classDeps, methodDeps) = props
    
    decider ! KeepClassDecider.DecideOnMethod(className, methodName)
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

