package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

// TODO: Test performence of 'symbols vs case objects

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

    val dataReaders = options.indirs map {(dir: File) => (new ClassDataReader(dir) with LoggerMixin).start()}
    val dataWriter = (new ClassDataWriter(options.outdir) with LoggerMixin).start()
    val decider = (new KeepClassDecider(options.keepNamespaces) with LoggerMixin).start()
    val progressGate = (new ProgressGate).start()

    def decode(origFile: File, classData: Array[Byte]) {
      val decoder = new ClassDecoder(classData) with LoggerMixin
      decoder.start
      actor {
        decoder ! ClassDecoder.GetName
        react {
          case ClassDecoder.Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    decider ! KeepClassDecider.Decide(className)
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
			val resume = () => progressGate ! ProgressGate.OneResumed
			decider ! KeepClassDecider.Keep(depName, resume)
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

object ProgressGate {
  case class Candidates(total: Int)
  case object OneStarted
  case object OneKept
  case object OneDiscarded
  case object OneWaiting
  case object OneResumed
  case object WaitUntilAllSeen
  case object AllSeen
  case object WaitUntilAllProcessed
  case object AllProcessed
  case object GetTotals
  case class Totals(candidates: Int, kept: Int, discarded: Int)
  case object End
}

class ProgressGate extends Actor {
  def act() {
    import actors.OutputChannel
    import ProgressGate._

    var candidates: Option[Int] = None
    var started = 0
    var kept = 0
    var discarded = 0
    var waiting = 0
    def processed = kept + discarded

    var seenWaiter: Option[OutputChannel[Any]] = None
    var processedWaiter: Option[OutputChannel[Any]] = None

    def checkConditions() {
      //println(candidates + " " + started + " " + kept + " " + discarded + " " + waiting)
      candidates match {
	case Some(candidates) =>
	  assert(started <= candidates)
	  if (processed == candidates) {
	    seenWaiter match {
	      case Some(actor) =>
		actor ! AllSeen
		seenWaiter = None
	      case None => ()
	    }
	    processedWaiter match {
	      case Some(actor) =>
		actor ! AllProcessed
		processedWaiter = None
	      case None => ()
	    }
	  } else if (started == candidates && kept + waiting == candidates) {
	    seenWaiter match {
	      case Some(actor) =>
		actor ! AllSeen
		seenWaiter = None
	      case None => ()
	  }
	}
	case None => ()
      }
    }

    loop {
      react {
	case Candidates(total) =>
	  candidates = Some(total)
	  checkConditions()
	case OneStarted =>
	  started += 1
	  checkConditions()
	case OneKept => 
	  kept += 1
	  checkConditions()
	case OneDiscarded =>
	  discarded += 1
	  checkConditions()
	case OneWaiting =>
	  waiting += 1
	  checkConditions()
	case OneResumed =>
	  waiting -= 1
	  checkConditions()
	case WaitUntilAllSeen =>
	  seenWaiter = Some(sender)
	  checkConditions()
	case WaitUntilAllProcessed =>
	  processedWaiter = Some(sender)
	  checkConditions()
	case GetTotals =>
	  assert(candidates isDefined)
	  reply(Totals(candidates.get, kept, discarded))
	case End => exit()
      }
    }
  }

}
