package net.negatory.quickshake

class ShakeFactory(
  options: Options,
  loggerFactory: (LogLevel.LogLevel) => Logger
) {

  import actors.Actor
  import actors.Actor._

  val logger = loggerFactory(options.logLevel)

  val exitHandler = new ExitHandler with logger.LoggerMixin { start }

  trait ShakeMixin extends logger.LoggerMixin with exitHandler.TrapMixin

  val decider = new KeepClassDecider(options.keepNamespaces) with ShakeMixin { start }
  val statsTracker = new StatsTracker with ShakeMixin { start }

  import java.io.File

  def newDirDataReader(dir: File) = new DirectoryDataReader(dir) with ShakeMixin { start }
  def newJarDataReader(jar: File) = new JarDataReader(jar) with ShakeMixin { start }
  def newDirDataWriter(dir: File) = new DirectoryDataWriter(dir) with ShakeMixin { start }
  def newJarDataWriter(jar: File) = new JarDataWriter(jar) with ShakeMixin { start }
  def newDecoder(classData: Array[Byte]) = new ClassDecoder(classData) with ShakeMixin { start }

  val terminator = new Terminator with ShakeMixin { start }

  trait TerminationMixin extends ShakeMixin with terminator.TerminationMixin

  def trackedActor(body: => Unit) = new Actor with TerminationMixin {
    def act() = body
    start()
  }

  def newClassCoordinator(
    classData: Array[Byte],
    decoder: ClassDecoder,
    dataWriter: ClassDataWriter
  ) = trackedActor {
    decoder ! ClassDecoder.GetName

    react {
      case ClassDecoder.Name(className) =>
	logger.debug("Decoded name of class " + className)
	decider ! KeepClassDecider.DecideOnClass(className)
	var methods = 0
	react {
	  case KeepClassDecider.Kept =>
	    logger.debug("Keeping " + className)
	    decoder ! ClassDecoder.FindDependencies
	    loop {
	      react {
		case ClassDecoder.ClassDependency(depName) =>
		  decider ! KeepClassDecider.KeepClass(depName)
		case ClassDecoder.Method(methodName, classDeps, methodDeps) =>
		  methods += 1
		  val methodAccumulator = self
		  newMethodCoordinator (
		    (className, methodName, classDeps, methodDeps),
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
			case MethodAccumulator.KeepMethod(methodName) =>
			  methodsKept += methodName
			case MethodAccumulator.DiscardMethod => ()
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
	    logger.debug("Discarding " + className)
	    decoder ! ClassDecoder.Discard
	    statsTracker ! StatsTracker.DiscardedClass
	    exit()
	}
    }
  }

  def newMethodCoordinator(
    methodProps: (ClassName, String, List[ClassName], List[String]),
    methodAccumulator: Actor
  ) = trackedActor {

    val (className, methodName, classDeps, methodDeps) = methodProps
    
    decider ! KeepClassDecider.DecideOnMethod(className, methodName)
    react {
      case KeepClassDecider.Kept =>
	methodAccumulator ! MethodAccumulator.KeepMethod(methodName)
	classDeps foreach {
	  decider ! KeepClassDecider.KeepClass(_)
	}
	methodDeps foreach {
	  decider ! KeepClassDecider.KeepMethod(_)
	}
	exit()
      case KeepClassDecider.Discarded =>
	methodAccumulator ! MethodAccumulator.DiscardMethod
	exit()
    }
  }

}

object MethodAccumulator {
  // These will be used to track the set of methods
  // on a class that need to be retained
  case class KeepMethod(methodName: String)
  case object DiscardMethod
}
