package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object StatsTracker {
  case object KeptClass
  case object DiscardedClass
  case object KeptMethod
  case object DiscardedMethod
  case object LogStats
  case object End
}

class StatsTracker extends Actor with Logging {

  import StatsTracker._

  def act() = {
    var keptClasses = 0
    var discardedClasses = 0
    var keptMethods = 0
    var discardedMethods = 0
    loop {
      react {
	case KeptClass => keptClasses += 1
	case DiscardedClass => discardedClasses += 1
	case KeptMethod => keptMethods += 1
	case DiscardedMethod => discardedMethods += 1
	case LogStats =>
	  info("Kept " + keptClasses + " classes")
	  info("Discarded " + discardedClasses + " classes")
	  info("Kept " + keptMethods + " methods")
	  info("Discarded " + discardedMethods + " methods")
	case End => exit()
      }
    }
  }

}
