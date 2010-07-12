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
    val shaker = new Shaker(options, logger)

    val shakeFactory = shaker.shakeFactory

    logger.info("inputs:")
    options.inputs foreach {dir => logger.info(dir.toString)}
    logger.info("output: " + options.output)
    logger.info("keepNamespaces:")
    options.keepNamespaces foreach {ns => logger.info(ns)}

    val terminator = shaker.terminator
    val dataReaders = shaker.dataReaders
    val dataWriter = shaker.dataWriter
    val decider = shaker.decider
    val statsTracker = shaker.statsTracker

    // Create a client for each reader that processes the input classes.
    dataReaders map { 
      
      (reader) =>

	import shakeFactory.trackedActor

	trackedActor {
          reader ! ClassDataReader.Search

          loop {
            react {
              case ClassDataReader.Visit(classData) =>
		val decoder = shakeFactory.newDecoder(classData)
		shakeFactory.newClassCoordinator(
		  classData, decoder, decider, dataWriter, statsTracker
		)
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

