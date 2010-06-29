package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

object QuickShake {

  def main(args: Array[String]) {

    val options = new Options(args)
    val logger = new ConsoleLogger(options.logLevel)
    import logger.LoggerMixin
    val tracker = new ActorTracker with LoggerMixin
    import tracker.TrackerMixin

    logger.info("indirs:")
    options.indirs foreach {dir => logger.info(dir)}
    logger.info("outdir: " + options.outdir)
    logger.info("keepNamespaces:")
    options.keepNamespaces foreach {ns => logger.info(ns)}

    val dataReaders = options.indirs map {(dir: String) => (new ClassDataReader(dir) with LoggerMixin with TrackerMixin).start}
    val dataWriter = (new ClassDataWriter(options.outdir) with LoggerMixin).start
    val decider = (new KeepClassDecider(options.keepNamespaces) with LoggerMixin).start
    val counter = new ClassCounter().start

    def trackedActor(body: => Unit) = new Actor with TrackerMixin {
      def act() = body
      start()
    }

    def decode(origFile: File, classData: Array[Byte]) {
      val decoder = new ClassDecoder(classData) with LoggerMixin with TrackerMixin
      decoder.start
      trackedActor {
        decoder ! ClassDecoder.GetName
        react {
          case ClassDecoder.Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    counter ! ClassCounter.Inspected
	    decider ! KeepClassDecider.Decide(className)
	    loop {
	      react {
		case KeepClassDecider.Waiting =>
		  logger.debug("Waiting on "  + decider)
		  logger.debug("Stop tracking " + self + " & " + decoder)
		  decoder.stopTracking
		  (self.asInstanceOf[TrackerMixin]).stopTracking
		case KeepClassDecider.Kept =>
		  logger.debug("Keeping " + className)
		  counter ! ClassCounter.Kept
	          dataWriter ! ClassDataWriter.AddClass(origFile, className, classData)
		  decoder ! ClassDecoder.FindDependencies
		  loop {
		    react {
		      case ClassDecoder.Dependency(depName) => decider ! KeepClassDecider.Keep(depName)
		      case ClassDecoder.End => exit
		    }
		  }
		case KeepClassDecider.Discarded => 
		  logger.debug("Discarding " + className)
		  counter ! ClassCounter.Discarded
	          decoder ! ClassDecoder.Discard
		  exit
	      }
	    }
        }
      }
    }

    dataReaders foreach {
      (reader) => trackedActor {
        reader ! ClassDataReader.Search
        loop {
          react {
            case ClassDataReader.Visit(origFile, classData) =>
	      decode (origFile, classData)
            case ClassDataReader.End => exit
          }
        }
      }
    }

    tracker.waitForActors
    logger.debug("All actors done")
    decider ! KeepClassDecider.End
    dataWriter ! ClassDataWriter.End
    counter !? ClassCounter.GetResults match {
      case ClassCounter.Results(inspected, kept, discarded) =>
	logger.info("Classes inspected: " + inspected)
	logger.info("Classes kept: " + kept)
        logger.info("Classed discarded: " + discarded)
    }
    
  }

}

class Options(args: Array[String]) {

  val indirs = (args(0) split ":").toList
  val outdir = args(1)
  val keepNamespaces = (args(2) split ":").toList
  val logLevel = args(3) match {
    case "debug" => LogLevel.Debug
    case "info" => LogLevel.Info
    case "warning" => LogLevel.Warning
    case "error" => LogLevel.Error
  }
}

