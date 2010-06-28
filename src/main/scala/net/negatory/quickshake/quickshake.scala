package net.negatory.quickshake

import actors.Actor
import actors.Actor._

// TODO: Create a descriptor type to deal with the various representations of classes

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

    // TODO: Switch to a ThreadPoolRunner
    val taskRunner = new scala.concurrent.ThreadRunner

    def decode(classData: Array[Byte]) {
      val decoder = new ClassDecoder(classData, taskRunner) with LoggerMixin with TrackerMixin
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
	          dataWriter ! ClassDataWriter.AddClass(className, classData)
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
            case ClassDataReader.Visit(classData) => decode (classData)
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

object ClassDataReader {
  case object Search
  case class Visit(classData: Array[Byte])
  case object End
}

class ClassDataReader(root: String) extends Actor with Logging {

  import org.apache.commons.io.IOUtils.toByteArray
  import org.apache.commons.io.DirectoryWalker
  import java.io.{File, FileInputStream}
  import ClassDataReader._

  def act() {
    react {
      case Search => search()
    }
  }

  private def search() {
    debug("Searching for class files in " + root)
    for (classFile <- allClassFiles(root)) {
      debug("Found " + classFile)
      reply(Visit(loadClassData(classFile)))
    }
    reply(End)
  }

  private def allClassFiles(root: String): List[String] = {
    new DirectoryWalker {
      protected[this] override def handleFile(file: File, depth: Int, results: java.util.Collection[_]): Unit = {
	import java.util.Collection
	if (file.getName.endsWith(".class")) results.asInstanceOf[Collection[String]].add(file.getAbsolutePath)
      }
      def findClassFiles() = {
	val classFiles = new java.util.ArrayList[String]
	walk (new File(root), classFiles)
	import collection.JavaConversions._
	val classFilesIter: Iterable[String] = classFiles 
	classFilesIter.toList
      }
    }.findClassFiles
  }

  private def loadClassData(file: String): Array[Byte] = {
    val stream = new FileInputStream(file)
    try {
      toByteArray(stream)
    }
    finally {
      stream.close
    }
  }
}

object ClassDataWriter {
  case class AddClass(className: ClassName, classData: Array[Byte])
  case object End
}

class ClassDataWriter(dir: String) extends Actor with Logging {
  def act() {
    import ClassDataWriter._
    loop {
      react {
	case AddClass(className, classData) => debug("Writing " + className)
	case End => exit
      }
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

object LogLevel extends Enumeration {
  type LogLevel = Value
  val Debug, Info, Warning, Error = Value
}

import LogLevel._

trait Logging {
  def debug(msg: => String) = Unit
  def info(msg: => String) = Unit
  def warning(msg: => String) = Unit
  def error(msg: => String) = Unit
}
 
trait Logger {

  // Allows creation of a single logger, then mixing in the instance's Mixin trait
  trait LoggerMixin extends Logging {
    val minLogLevel = Logger.this.minLogLevel
    def log(level: LogLevel, msg: String) = Logger.this.log(level, msg)
  }

  val minLogLevel: LogLevel

  // TODO: Consider eliding this method
  def debug(msg: => String): Unit = trylog(Debug, msg)
  def info(msg: => String): Unit = trylog(Info, msg)
  def warning(msg: => String): Unit = trylog(Warning, msg)
  def error(msg: => String): Unit = trylog(Error, msg)

  def trylog(level: LogLevel, msg: => String) {
    if (level >= minLogLevel) log(level, msg)
  }

  def log(level: LogLevel, msg: String)
}

class ConsoleLogger(val minLogLevel: LogLevel) extends Logger {

  def log(level: LogLevel, msg: String) {
    println(level.toString + ": " + msg)
  }

}

object ActorTracker {
  private[ActorTracker] case class Register(actor: Actor)
  private[ActorTracker] case class Unregister(actor: Actor)
  private[ActorTracker] case object End
}

class ActorTracker extends AnyRef with Logging {

  import ActorTracker._
  import concurrent.SyncVar

  private val sync = new SyncVar[Unit]

  private val registrar = actor {
    var counter = 0
    loop {
      react {
	case Register(actor) =>
	  debug("Registering actor " + actor.toString)
	  counter += 1
	case Unregister(actor) =>
	  debug("Unregistering actor " + actor.toString)
	  counter -= 1
	  if (counter == 0) {
	    sync.set(Unit)
	    exit
	  }
      }
    }
  }

  trait TrackerMixin extends Actor {
    type TrackerMixin = ActorTracker.this.TrackerMixin
    abstract override def start(): Actor = {
      registrar ! Register(this)
      super.start()
    }
    abstract override def exit(): Nothing = {
      stopTracking()
      super.exit()
    }
    def stopTracking() {
      registrar ! Unregister(this)
    }
  }

  // This is the first actor registered, and it is unregistered once
  // the caller wait's for actors. Should prevent the possibility of
  // the actor count reaching 0 before all actors have been started.
  val initBlocker = new Actor with TrackerMixin {

    def act() {
      react {
	case End => exit
      }
    }
  }

  def waitForActors() = {
    initBlocker ! End
    sync.get
  }
}


object ClassCounter {
  case object Inspected
  case object Kept
  case object Discarded
  case object GetResults
  case class Results(inspected: Int, kept: Int, Discarded: Int)
}

class ClassCounter extends Actor {
  import ClassCounter._

  var inspected = 0
  var kept = 0
  var discarded = 0

  def act() = loop {
    react {
      case Inspected => inspected += 1
      case Kept => kept += 1
      case Discarded => discarded += 1
      case GetResults =>
	val resultRequester = sender
	def checkResults() = if (kept + discarded == inspected) {
	  resultRequester ! Results(inspected, kept, discarded)
	  exit
	}
	checkResults()
	// If we haven't heard back from all the classes we inspected
	// then keep waiting for the numbers to add up
	loop { react {
	  case Inspected =>
	    inspected += 1
	    checkResults()
	  case Kept =>
	    kept += 1
	    checkResults()
	  case Discarded =>
	    discarded += 1
	    checkResults()
	}}
    }
  }
}
