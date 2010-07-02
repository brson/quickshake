package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import actors.Exit
import java.io.File

// TODO: Test performence of 'symbols vs case objects

object ExitHandler {
  case object End
  case class Watch(actor: Actor)
}

class ExitHandler extends Actor with Logging {

  import ExitHandler._

  val handler = this

  trait TrapMixin extends Actor {
    handler !? Watch(this)
  }

  trapExit = true

  def act() = loop {
    react {
      case Watch(actor) => link(actor); reply(())
      case Exit(from, ex: Exception) => {
	error("Actor failed: " + ex.toString)
      }
      case Exit(from, _) => ()
      case End => exit()
    }
  }
}

object QuickShake {

  def main(args: Array[String]) {

    val options = new Options(args)
    val logger = new ConsoleLogger(options.logLevel)
    logger.info("indirs:")
    options.indirs foreach {dir => logger.info(dir.toString)}
    logger.info("outdir: " + options.outdir)
    logger.info("keepNamespaces:")
    options.keepNamespaces foreach {ns => logger.info(ns)}

    runShake(options, logger)
  }

  def runShake(options: Options, logger: Logger) {

    import logger.LoggerMixin

    val exitHandler = new ExitHandler with LoggerMixin
    exitHandler.start()

    import exitHandler.TrapMixin

    trait ShakeMixin extends LoggerMixin with TrapMixin

    val dataReaders = options.indirs map {
      (dir: File) => {
	new ClassDataReader(dir) with ShakeMixin
      }.start()
    }
    val dataWriter = {
      new ClassDataWriter(options.outdir) with ShakeMixin
    }.start()
    val decider = {
      new KeepClassDecider(options.keepNamespaces) with ShakeMixin
    }.start()
    val progressGate = {
      new ProgressGate with ShakeMixin
    }.start()

    def decode(origFile: File, classData: Array[Byte]) {
      val decoder = new ClassDecoder(classData) with ShakeMixin
      decoder.start

      actor {
        decoder ! ClassDecoder.GetName

        react {
          case ClassDecoder.Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    val resume = () => progressGate ! ProgressGate.OneResumed
	    decider ! KeepClassDecider.Decide(className, resume)
	    progressGate ! ProgressGate.OneStarted
	    loop {
	      react {
		case KeepClassDecider.Waiting =>
		  logger.debug("Waiting for decision on "  + className)
		  progressGate ! ProgressGate.OneWaiting
		case KeepClassDecider.Kept =>
		  logger.debug("Keeping " + className)
	          dataWriter ! ClassDataWriter.AddClass(origFile, className, classData)
		  decoder ! ClassDecoder.FindDependencies
		  loop {
		    react {
		      case ClassDecoder.Dependency(depName) =>
			decider ! KeepClassDecider.Keep(depName)

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

    //Thread.sleep(10000)
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

class Options(args: Array[String]) {

  val indirs = (args(0) split ":").toList map { d => new File(d) }
  val outdir = new File(args(1))
  val keepNamespaces = (args(2) split ":").toList
  val logLevel = args(3) match {
    case "debug" => LogLevel.Debug
    case "info" => LogLevel.Info
    case "warning" => LogLevel.Warning
    case "error" => LogLevel.Error
  }
}
