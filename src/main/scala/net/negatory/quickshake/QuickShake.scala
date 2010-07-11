package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

// TODO: Test performence of 'symbols vs case objects

object QuickShake {

  def main(args: Array[String]) {

    val options = new CommandLineOptions(args)
    runShake(options, (logLevel) => new ConsoleLogger(logLevel))
  }

  def runShake(
    options: Options,
    loggerFactory: (LogLevel.LogLevel) => Logger
  ) {
    val logger = loggerFactory(options.logLevel)
    import logger.LoggerMixin

    logger.info("inputs:")
    options.inputs foreach {dir => logger.info(dir.toString)}
    logger.info("output: " + options.output)
    logger.info("keepNamespaces:")
    options.keepNamespaces foreach {ns => logger.info(ns)}

    val exitHandler = new ExitHandler with LoggerMixin
    exitHandler.start()

    import exitHandler.TrapMixin

    trait ShakeMixin extends LoggerMixin with TrapMixin

    val dataReaders = options.inputs map {
      (in: File) => {
	if (in.isDirectory) {
	  new DirectoryDataReader(in) with ShakeMixin
	} else if (in.isFile) {
	  new JarDataReader(in) with ShakeMixin
	} else {
	  error("Input " + in.getPath + " not found")
	}
      }.start()
    }
    val dataWriter = {
      val isJar = options.output.getAbsolutePath.endsWith(".jar")
      if (isJar) new JarDataWriter(options.output) with ShakeMixin
      else new DirectoryDataWriter(options.output) with ShakeMixin
    }.start()
    val decider = {
      new KeepClassDecider(options.keepNamespaces) with ShakeMixin
    }.start()
    val statsTracker = {
      new StatsTracker with ShakeMixin
    }.start()


    val terminator = new Terminator with ShakeMixin
    terminator.start()
    trait TerminationMixin extends ShakeMixin with terminator.TerminationMixin
    def trackedActor(body: => Unit) = new Actor with TerminationMixin {
      def act() = body

      start()
    }

    def decode(classData: Array[Byte]) {
      val decoder = {
	new ClassDecoder(classData) with ShakeMixin
      }.start()

      trackedActor {
        decoder ! ClassDecoder.GetName

        react {
          case ClassDecoder.Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    decider ! KeepClassDecider.DecideOnClass(className)
	    var methods = 0
	    loop {
	      react {
		case KeepClassDecider.Kept =>
		  logger.debug("Keeping " + className)
		  decoder ! ClassDecoder.FindDependencies
		  // These will be used to track the set of methods
		  // on this class that need to be retained
		  case class KeepMethod(methodName: String)
		  case object DiscardMethod
		  loop {
		    react {
		      case ClassDecoder.ClassDependency(depName) =>
			decider ! KeepClassDecider.KeepClass(depName)
		      case ClassDecoder.Method(methodName, classDeps, methodDeps) =>
			methods += 1
			val methodAccumulator = self
			actor {
			  decider ! KeepClassDecider.DecideOnMethod(className, methodName)
			  loop {
			    react {
			      case KeepClassDecider.Kept =>
				methodAccumulator ! KeepMethod(methodName)
				classDeps foreach {
				  decider ! KeepClassDecider.KeepClass(_)
				}
				methodDeps foreach {
				  decider ! KeepClassDecider.KeepMethod(_)
				}
				exit()
			      case KeepClassDecider.Discarded =>
				methodAccumulator ! DiscardMethod
				exit()
			    }
			  }
			}
		      case ClassDecoder.End =>
			// Get the list of methods to keep
			import collection.mutable.HashSet
			var methodsDecided = 0
			val methodsKept = new HashSet[String]
			loopWhile(methodsDecided < methods) {
			  react {
			    val f: PartialFunction[Any, Unit] = {
			      case KeepMethod(methodName) =>
				methodsKept += methodName
			      case DiscardMethod => ()
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
      }
    }

    // Create a client for each reader that processes the input classes.
    dataReaders map { 
      
      (reader) =>

	trackedActor {
          reader ! ClassDataReader.Search

          loop {
            react {
              case ClassDataReader.Visit(classData) =>
		decode (classData)
              case ClassDataReader.End =>
		exit()
            }
          }
	}
    }

    // TODO: Need to find a better way to estimate when we need
    // to begin probing for termination
    var continue = true
    while (continue) {
      terminator !? Terminator.AwaitAllPassive match {
	case Terminator.AllPassive =>
	  logger.debug("Draining waiters")
	  decider ! KeepClassDecider.DrainWaiters
	case Terminator.AllDone => continue = false
      }
    }
    logger.debug("Cleaning up")
    statsTracker ! StatsTracker.LogStats
    statsTracker ! StatsTracker.End
    decider ! KeepClassDecider.End
    dataWriter !? ClassDataWriter.End

    terminator ! Terminator.End
  }

}

