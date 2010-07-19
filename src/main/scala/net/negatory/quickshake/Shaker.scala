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

  val classDecider = new KeepClassDecider(options.keepNamespaces) with TerminationMixin {
    start()
  }
  val methodDecider = new KeepMethodDecider(options.keepNamespaces) with TerminationMixin {
    start()
  }

  def trackedActor(body: => Unit) = new Actor with TerminationMixin {
    def act() = body
    start()
  }

  def newClassCoordinator(
    classData: Array[Byte]
  ) = new ClassCoordinator(
    classData,
    shakeFactory.newDecoder(classData),
    classDecider,
    methodDecider,
    dataWriter,
    statsTracker
  ) with TerminationMixin {
    start
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
	case Terminator.AllPassive(remaining) =>
	  if (remaining > 2) {
	    logger.debug("Draining waiters")
	    methodDecider ! KeepMethodDecider.DrainWaiters
	    classDecider ! KeepClassDecider.DrainWaiters
	  } else {
	    // These little guys are the only ones left alive
	    // but they still can't escape the Terminator
	    methodDecider ! KeepMethodDecider.End
	    classDecider ! KeepClassDecider.End
	  }
	case Terminator.AllDone => continue = false
      }
    }
  
    statsTracker !? StatsTracker.LogStats

    logger.debug("Cleaning up")
    statsTracker ! StatsTracker.End
    dataWriter !? ClassDataWriter.End
    terminator ! Terminator.End
  }
}

