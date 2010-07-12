package net.negatory.quickshake

import actors.Actor

class ClassCoordinator(
  classData: Array[Byte],
  decoder: ClassDecoder,
  decider: KeepClassDecider,
  dataWriter: ClassDataWriter,
  statsTracker: StatsTracker
) extends Actor with Logging {

  import Actor._

  def act() {
    decoder ! ClassDecoder.GetName

    react {
      case ClassDecoder.Name(className) =>
	debug("Decoded name of class " + className)
	decider ! KeepClassDecider.DecideOnClass(className)
	react {
	  case KeepClassDecider.Kept =>
	    debug("Keeping " + className)
	    decoder ! ClassDecoder.FindDependencies
	    var methods = 0
	    loop {
	      react {
		case ClassDecoder.ClassDependency(depName) =>
		  decider ! KeepClassDecider.KeepClass(depName)
		case ClassDecoder.Method(methodName, classDeps, methodDeps) =>
		  methods += 1
		  val props = MethodProps(className, methodName, classDeps, methodDeps)
		  decider ! KeepClassDecider.DecideOnMethod(props)
		case ClassDecoder.End =>
		  // Get the list of methods to keep
		  import collection.mutable.HashSet
		  var methodsDecided = 0
		  val methodsKept = new HashSet[String]
		  loopWhile(methodsDecided < methods) {
		    react {
		      val f: PartialFunction[Any, Unit] = {
			case KeepClassDecider.KeptMethod(props) =>
			  val MethodProps(_, methodName, classDeps, methodDeps) = props
			  methodsKept += methodName
			  classDeps foreach {
			    decider ! KeepClassDecider.KeepClass(_)
			  }
			  methodDeps foreach {
			    decider ! KeepClassDecider.KeepMethod(_)
			  }
			case KeepClassDecider.DiscardedMethod(_) => ()
		      }

		      f andThen { _ => methodsDecided += 1 }
		    }
		  } andThen {
		    statsTracker ! StatsTracker.KeptClass(methodsKept.size)
		    dataWriter ! ClassDataWriter.AddClass(className, classData)
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
