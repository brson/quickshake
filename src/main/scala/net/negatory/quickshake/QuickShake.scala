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

    logger.info("inputs:")
    options.inputs foreach {dir => logger.info(dir.toString)}
    logger.info("output: " + options.output)
    logger.info("keepNamespaces:")
    options.keepNamespaces foreach {ns => logger.info(ns)}

    new Shaker(options, logger).run()
  }

}

