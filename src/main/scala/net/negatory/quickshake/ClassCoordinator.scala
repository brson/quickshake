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
    decoder ! ClassDecoder.GetProps

    react {
      case ClassDecoder.Props(ClassProps(className, _, _)) =>
	debug("Decoded name of class " + className)
	decider ! KeepClassDecider.DecideOnClass(className)
	react {
	  case KeepClassDecider.Kept =>
	    debug("Keeping class " + className)
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
			  debug("Keeping method " + methodName)
			  statsTracker ! StatsTracker.KeptMethod
			  methodsKept += methodName
			  classDeps foreach {
			    decider ! KeepClassDecider.KeepClass(_)
			  }
			  methodDeps foreach { p =>
			    val (cn, mn) = p
			    decider ! KeepClassDecider.KeepMethod(cn, mn)
			  }
			case KeepClassDecider.DiscardedMethod(props) =>
			  val MethodProps(_, methodName, _, _) = props
			  debug("Discarding method " + methodName)
			  statsTracker ! StatsTracker.DiscardedMethod
		      }

		      f andThen { _ => methodsDecided += 1 }
		    }
		  } andThen {
		    statsTracker ! StatsTracker.KeptClass
		    dataWriter ! ClassDataWriter.AddClass(className, classData, methodsKept)
		    exit()
		  }
	      }
	    }
	  case KeepClassDecider.Discarded => 
	    debug("Discarding class " + className)
	    decoder ! ClassDecoder.Discard
	    statsTracker ! StatsTracker.DiscardedClass
	    exit()
	}
    }
  }
}
