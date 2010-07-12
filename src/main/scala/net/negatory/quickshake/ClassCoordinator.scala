package net.negatory.quickshake

import actors.Actor

class ClassCoordinator(
  classData: Array[Byte],
  decoder: ClassDecoder,
  decider: KeepClassDecider,
  dataWriter: ClassDataWriter,
  statsTracker: StatsTracker,
  newMethodCoordinator: (MethodProps, Actor) => MethodCoordinator
) extends Actor with Logging {

  import Actor._

  def act() {
    decoder ! ClassDecoder.GetName

    react {
      case ClassDecoder.Name(className) =>
	debug("Decoded name of class " + className)
	decider ! KeepClassDecider.DecideOnClass(className)
	var methods = 0
	react {
	  case KeepClassDecider.Kept =>
	    debug("Keeping " + className)
	    decoder ! ClassDecoder.FindDependencies
	    loop {
	      react {
		case ClassDecoder.ClassDependency(depName) =>
		  decider ! KeepClassDecider.KeepClass(depName)
		case ClassDecoder.Method(methodName, classDeps, methodDeps) =>
		  methods += 1
		  val methodAccumulator = self
		  newMethodCoordinator (
		    MethodProps(className, methodName, classDeps, methodDeps),
		    methodAccumulator
		  )
		case ClassDecoder.End =>
		  // Get the list of methods to keep
		  import collection.mutable.HashSet
		  var methodsDecided = 0
		  val methodsKept = new HashSet[String]
		  loopWhile(methodsDecided < methods) {
		    react {
		      val f: PartialFunction[Any, Unit] = {
			case MethodCoordinator.KeepMethod(methodName) =>
			  methodsKept += methodName
			case MethodCoordinator.DiscardMethod => ()
		      }

		      f andThen { _ => methodsDecided += 1 }
		    }
		  } andThen {
		    dataWriter ! ClassDataWriter.AddClass(className, classData)
		    statsTracker ! StatsTracker.KeptClass(methodsKept.size)
		    exit()
		  }
	      }
	    }
	  case KeepClassDecider.Discarded => 
	    debug("Discarding " + className)
	    decoder ! ClassDecoder.Discard
	    statsTracker ! StatsTracker.DiscardedClass
	    exit()
	}
    }
  }
}
