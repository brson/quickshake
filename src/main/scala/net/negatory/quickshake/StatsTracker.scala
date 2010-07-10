package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object StatsTracker {
  case class KeptClass(methods: Int)
  case object DiscardedClass
  case object LogStats
  case object End
}

class StatsTracker extends Actor with Logging {

  import StatsTracker._

  def act() = {
    var keptClasses = 0
    var keptMethods = 0
    var discardedClasses = 0
    loop {
      react {
	case KeptClass(methods) =>
	  keptClasses += 1
	  keptMethods += methods
	case DiscardedClass =>
	  discardedClasses += 1
	case LogStats =>
	  info("Kept " + keptClasses + " classes")
	  info("Kept " + keptMethods + " methods")
	  info("Discarded " + discardedClasses + " classes")
	case End => exit()
      }
    }
  }

}
