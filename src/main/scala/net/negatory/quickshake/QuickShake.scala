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

    val dataReaders = options.indirs map {(dir: File) => (new ClassDataReader(dir) with LoggerMixin).start}
    val dataWriter = (new ClassDataWriter(options.outdir) with LoggerMixin).start
    val decider = (new KeepClassDecider(options.keepNamespaces) with LoggerMixin).start

    object ProgressGate {
      case class Candidates(total: Int)
      case object OneKept
      case object OneDiscarded
      case object OneWaiting
      case object OneResumedAndKept
      case object OneResumedAndDiscarded
      case object WaitUntilAllSeen
      case object AllSeen
      case object WaitUntilAllProcessed
      case object AllProcessed
      case object GetTotals
      case class Totals(candidates: Int, kept: Int, discarded: Int)
      case object End
    }

    val progressGate = actor {
      import actors.OutputChannel
      import ProgressGate._

      var candidates: Option[Int] = None
      var kept = 0
      var discarded = 0
      var waiting = 0
      def processed = kept + discarded
      def seen = processed + waiting
      var seenWaiter: Option[OutputChannel[Any]] = None
      var processedWaiter: Option[OutputChannel[Any]] = None

      def checkConditions() = candidates match {
	case Some(candidates) => {
	  assert(waiting <= candidates)
	  if (processed == candidates) {
	    seenWaiter match {
	      case Some(waiter) =>
		waiter ! AllSeen
	        seenWaiter = None
	      case _ => ()
	    }
	    processedWaiter match {
	      case Some(waiter) =>
		waiter ! AllProcessed
		processedWaiter = None
	      case _ => ()
	    }
	  } else if (waiting + processed == candidates) {
	    seenWaiter match {
	      case Some(waiter) =>
		waiter ! AllSeen
	        seenWaiter = None
	      case _ => ()
	    }
	  }
	}
	case None => ()
      }

      loop {
	react {
	  case Candidates(total) => candidates = Some(total); checkConditions()
	  case OneKept => kept += 1; checkConditions()
	  case OneDiscarded => discarded += 1; checkConditions()
	  case OneWaiting => waiting += 1; checkConditions()
	  case OneResumedAndKept => waiting -= 1; kept +=1; checkConditions()
	  case OneResumedAndDiscarded => waiting -= 1; discarded +=1; checkConditions()
	  case WaitUntilAllSeen => seenWaiter = Some(sender); checkConditions()
	  case WaitUntilAllProcessed => processedWaiter = Some(sender); checkConditions()
	  case GetTotals =>
	    assert(candidates isDefined)
	    reply(Totals(candidates.get, kept, discarded))
	  case End => exit()
	}
      }
    }

    def decode(origFile: File, classData: Array[Byte]) {
      val decoder = new ClassDecoder(classData) with LoggerMixin
      decoder.start
      var waiting = false
      actor {
        decoder ! ClassDecoder.GetName
        react {
          case ClassDecoder.Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    decider ! KeepClassDecider.Decide(className)
	    loop {
	      react {
		case KeepClassDecider.Waiting =>
		  logger.debug("Waiting for decision on "  + className)
		  waiting = true
		  progressGate ! ProgressGate.OneWaiting
		case KeepClassDecider.Kept =>
		  logger.debug("Keeping " + className)
	          dataWriter ! ClassDataWriter.AddClass(origFile, className, classData)
		  decoder ! ClassDecoder.FindDependencies
		  loop {
		    react {
		      case ClassDecoder.Dependency(depName) => ()//decider ! KeepClassDecider.Keep(depName)
		      case ClassDecoder.End =>
			progressGate ! {
			  if (waiting) ProgressGate.OneResumedAndKept
			  else ProgressGate.OneKept
			}
		        exit()
		    }
		  }
		case KeepClassDecider.Discarded => 
		  logger.debug("Discarding " + className)
	          decoder ! ClassDecoder.Discard
		  progressGate ! {
		    if (waiting) ProgressGate.OneResumedAndDiscarded
		    else ProgressGate.OneDiscarded
		  }
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

    progressGate ! ProgressGate.Candidates(totalCandidates)
    progressGate !? ProgressGate.WaitUntilAllSeen

    decider ! KeepClassDecider.DrainWaiters
    decider ! KeepClassDecider.End

    progressGate ! ProgressGate.WaitUntilAllProcessed
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

