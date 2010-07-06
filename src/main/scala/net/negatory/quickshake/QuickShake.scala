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
    val progressGate = {
      new ProgressGate with ShakeMixin
    }.start()

    def decode(origFile: Option[File], classData: Array[Byte]) {
      val decoder = new ClassDecoder(classData) with ShakeMixin
      decoder.start

      actor {
        decoder ! ClassDecoder.GetName

        react {
          case ClassDecoder.Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    val resume = () => progressGate ! ProgressGate.OneResumed
	    decider ! KeepClassDecider.DecideOnClass(className, resume)
	    progressGate ! ProgressGate.OneStarted
	    loop {
	      react {
		case KeepClassDecider.Waiting =>
		  logger.debug("Waiting for decision on "  + className)
		  progressGate ! ProgressGate.OneWaiting
		case KeepClassDecider.Kept =>
		  logger.debug("Keeping " + className)
		  // TODO: This must only happen after we know which methods to keep
	          dataWriter ! ClassDataWriter.AddClass(origFile, className, classData)
		  decoder ! ClassDecoder.FindDependencies
		  loop {
		    react {
		      case ClassDecoder.ClassDependency(depName) =>
			decider ! KeepClassDecider.KeepClass(depName)
			react {
			  case KeepClassDecider.DoneKeeping => ()
			}
		      case ClassDecoder.Method(methodName, classDeps, methodDeps) =>
			actor {
			  decider ! KeepClassDecider.DecideOnMethod(className, methodName, () => ())
			  loop {
			    react {
			      case KeepClassDecider.Waiting => ()
			      case KeepClassDecider.Kept =>
				var remainingClassDeps = classDeps
				var remainingMethodDeps = methodDeps
				loopWhile (remainingClassDeps != Nil) {
				  decider ! KeepClassDecider.KeepClass(remainingClassDeps.head)
				  remainingClassDeps = remainingClassDeps.tail
				  react {
				    case KeepClassDecider.DoneKeeping => ()
				  }
				} andThen loopWhile (remainingMethodDeps != Nil) {
				  decider ! KeepClassDecider.KeepMethod(remainingMethodDeps.head)
				  remainingMethodDeps = remainingMethodDeps.tail
				  react {
				    case KeepClassDecider.DoneKeeping => ()
				  }
				} andThen {
				  exit()
				}
			      case KeepClassDecider.Discarded => exit()
			    }
			  }
			}
		      case ClassDecoder.End =>
			progressGate ! ProgressGate.OneKept
		        exit()
		    }
		  }
		case KeepClassDecider.Discarded => 
		  logger.debug("Discarding " + className)
		  progressGate ! ProgressGate.OneDiscarded
	          decoder ! ClassDecoder.Discard
		  exit()
	      }
	    }
        }
      }
    }

    import concurrent.SyncVar

    // Create a client for each reader that processes the input classes.
    // Get a list of futures containing the 
    val perReaderTotals = dataReaders map { 
      
      (reader) =>
	val total = new SyncVar[Int]

	actor {
          reader ! ClassDataReader.Search

	  var candidateCount = 0
          loop {
            react {
              case ClassDataReader.Visit(origFile, classData) =>
		decode (origFile, classData)
		candidateCount += 1
              case ClassDataReader.End =>
		total set candidateCount
		exit()
            }
          }
	}

	total
    }

    val totalCandidates = perReaderTotals.foldLeft (0) { (total, readerTotal) => total + readerTotal.get }

    logger.debug("Waiting until all classes have been seen")
    progressGate ! ProgressGate.Candidates(totalCandidates)
    progressGate !? ProgressGate.WaitUntilAllSeen

    logger.debug("Draining waiters")
    decider ! KeepClassDecider.DrainWaiters
    decider ! KeepClassDecider.End

    logger.debug("Waiting until all classes have been processed")
    progressGate !? ProgressGate.WaitUntilAllProcessed
    progressGate !? ProgressGate.GetTotals match {
      case ProgressGate.Totals(candidates, kept, discarded) =>
	logger.info("Analyzed: " + candidates)
	logger.info("Kept: " + kept)
	logger.info("Discarded: " + discarded)
    }
    progressGate ! ProgressGate.End

    dataWriter ! ClassDataWriter.End
    exitHandler ! ExitHandler.End
  }

}

