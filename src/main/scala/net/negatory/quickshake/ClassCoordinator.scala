package net.negatory.quickshake

import actors.Actor

class ClassCoordinator(
  classData: Array[Byte],
  decoder: ClassDecoder,
  classDecider: KeepClassDecider,
  methodDecider: KeepMethodDecider,
  dataWriter: ClassDataWriter,
  statsTracker: StatsTracker
) extends Actor with Logging {

  import Actor._

  def act() {
    decoder ! ClassDecoder.GetProps

    react {
      case ClassDecoder.Props(classProps @ ClassProps(className, _, _)) =>
	debug("Decoded name of class " + className)
	classDecider ! KeepClassDecider.DecideOnClass(className)
	react {
	  case KeepClassDecider.Kept =>
	    keepClass(classProps)
	  case KeepClassDecider.Discarded => 
	    debug("Discarding class " + className)
	    decoder ! ClassDecoder.Discard
	    statsTracker ! StatsTracker.DiscardedClass
	    exit()
	}
    }
  }

  private[this] def keepClass(classProps: ClassProps) {
    val ClassProps(className, _, _) = classProps
    debug("Keeping class " + className)
    decoder ! ClassDecoder.FindDependencies
    var methods = 0
    loop {
      react {
	case ClassDecoder.ClassDependency(depName) =>
	  classDecider ! KeepClassDecider.KeepClass(depName)
	case ClassDecoder.Method(methodName, classDeps, methodDeps) =>
	  methods += 1
	  val props = MethodProps(className, methodName, classDeps, methodDeps)
	  methodDecider ! KeepMethodDecider.DecideOnMethod(props)
	case ClassDecoder.End =>
	  // Get the list of methods to keep
	  import collection.mutable.HashSet
	  var methodsDecided = 0
	  val methodsKept = new HashSet[String]
	  loopWhile(methodsDecided < methods) {
	    react {
	      val f: PartialFunction[Any, Unit] = {
		case KeepMethodDecider.KeptMethod(props) =>
		  val MethodProps(_, methodName, classDeps, methodDeps) = props
		  debug("Keeping method " + methodName)
		  statsTracker ! StatsTracker.KeptMethod
		  methodsKept += methodName
		  classDeps foreach {
		    classDecider ! KeepClassDecider.KeepClass(_)
		  }
		  methodDeps foreach {
		    p =>
		      val (cn, mn) = p
		      methodDecider ! KeepMethodDecider.KeepMethod(cn, mn)
		  }
		case KeepMethodDecider.DiscardedMethod(props) =>
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
  }

}
