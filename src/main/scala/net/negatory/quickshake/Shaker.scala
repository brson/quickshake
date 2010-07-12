package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File
import LogLevel._

class Shaker(
  options: Options,
  logger: Logger
) {

  val shakeFactory = new ShakeFactory(logger)
  val decider = shakeFactory.newDecider(options.keepNamespaces)
  val statsTracker = shakeFactory.newStatsTracker()
  val dataReaders = options.inputs map {
    (in: File) =>
      if (in.isDirectory) shakeFactory.newDirDataReader(in)
      else if (in.isFile) shakeFactory.newJarDataReader(in)
      else error("Input " + in.getPath + " not found")
  }
  val dataWriter = {
      val isJar = options.output.getAbsolutePath.endsWith(".jar")
      if (isJar) shakeFactory.newJarDataWriter(options.output)
      else shakeFactory.newDirDataWriter(options.output)
  }

  val terminator = shakeFactory.newTerminator()
  trait TerminationMixin extends shakeFactory.ShakeMixin with terminator.TerminationMixin

  def trackedActor(body: => Unit) = new Actor with TerminationMixin {
    def act() = body
    start()
  }

  def newClassCoordinator(classData: Array[Byte]) = trackedActor {
    val decoder = shakeFactory.newDecoder(classData)

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

  def run() {

    // Create a client for each reader that processes the input classes.
    dataReaders map { 
      
      (reader) =>

	trackedActor {
          reader ! ClassDataReader.Search

          loop {
            react {
              case ClassDataReader.Visit(classData) =>
		newClassCoordinator(classData)
              case ClassDataReader.End =>
		exit()
            }
          }
	}
    }

    var continue = true
    while (continue) {
      terminator !? Terminator.AwaitAllPassive match {
	case Terminator.AllPassive =>
	  logger.debug("Draining waiters")
	  decider ! KeepClassDecider.DrainWaiters
	case Terminator.AllDone => continue = false
      }
    }

    statsTracker ! StatsTracker.LogStats

    logger.debug("Cleaning up")
    statsTracker ! StatsTracker.End
    decider ! KeepClassDecider.End
    dataWriter !? ClassDataWriter.End
    // TODO: Need to stop gracefully, but fast
    Runtime.getRuntime.exit(0)
    terminator ! Terminator.End
  }
}

