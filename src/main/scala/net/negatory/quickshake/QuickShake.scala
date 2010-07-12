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
    val shakeFactory = new ShakeFactory(options, loggerFactory)

    val logger = shakeFactory.logger

    logger.info("inputs:")
    options.inputs foreach {dir => logger.info(dir.toString)}
    logger.info("output: " + options.output)
    logger.info("keepNamespaces:")
    options.keepNamespaces foreach {ns => logger.info(ns)}

    val terminator = shakeFactory.terminator

    type ShakeMixin = shakeFactory.ShakeMixin

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
    val decider = shakeFactory.decider
    val statsTracker = shakeFactory.statsTracker

    import shakeFactory.trackedActor

    def decode(classData: Array[Byte]) {
      val decoder = shakeFactory.newDecoder(classData)

      trackedActor {
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
		      shakeFactory.newMethodCoordinator (
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
    Runtime.getRuntime.exit(0)
    terminator ! Terminator.End
  }

}

