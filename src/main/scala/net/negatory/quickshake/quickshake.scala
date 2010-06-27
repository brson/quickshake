package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object QuickShake {

  def main(args: Array[String]) {

    val options = new Options(args)
    val logger = new ConsoleLogger(options.logLevel)
    import logger._


    info("indirs: ")
    options.indirs foreach (info _)
    info("outdir: " + options.outdir)
    info("keepNamespace: " + options.keepNamespace)

    val dataReaders = options.indirs map {(dir: String) => (new ClassDataReader(dir) with logger.Mixin).start}
    val dataWriter = (new ClassDataWriter(options.outdir) with logger.Mixin).start
    val decider = (new KeepClassDecider(options.keepNamespace) with logger.Mixin).start

    def decode(classData: Array[Byte]) {
      import ClassDecoder._
      val decoder = (new ClassDecoder(classData) with logger.Mixin).start
      actor {
        decoder ! GetName
        react {
          case Name(className) =>
	    logger.debug("Decoded name of class " + className)
	    import KeepClassDecider._
	    decider ! Decide(className)
	    react {
	      case Kept =>
		logger.debug("Keeping " + className)
		decoder ! FindDependencies
		loop {
		  react {
		    case Dependency(depName) => decider ! Keep(depName)
		    case End => exit
		  }
		}
	      case Discarded => 
		logger.debug("Discarding " + className)
	        decoder ! Discard
		exit
	    }
        }
      }
    }

    dataReaders foreach {
      import ClassDataReader._
      (reader) => actor {
        reader ! Search
        loop {
          react {
            case Visit(classData) => decode (classData)
            case End => exit
          }
        }
      }
    }

  }

}

object ClassDataReader {
  case object Search
  case class Visit(classData: Array[Byte])
  case object End
}

class ClassDataReader(private val root: String) extends Actor {
  self: Logger =>

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

object ClassDecoder {
  case object GetName
  case class Name(className: String)
  case object Discard
  case object FindDependencies
  case class Dependency(className: String)
  case object End
}

class ClassDecoder(classData: Array[Byte]) extends Actor {
  self: Logger =>
  def act() {
    import ClassDecoder._
    react {
      case GetName => reply(Name("test"))
      case Discard => exit
      case FindDependencies => reply(End); exit
    }
  }
}

object KeepClassDecider {
  case class Keep(className: String)
  case class Decide(className: String)
  case object Kept
  case object Discarded
}

class KeepClassDecider(keepNamespace: String) extends Actor {
  self: Logger =>
  def act() {
    import KeepClassDecider._
    loop {
      react {
	case Keep(_) => 
	case Decide(_) => reply(Discarded)
      }
    }
  }
}

object ClassDataWriter {
  case class AddClass(classData: Array[Byte])
}

class ClassDataWriter(dir: String) extends Actor {
  self: Logger =>
  def act() {}
}

class Options(args: Array[String]) {

  val indirs = args(0) split ":"
  val outdir = args(1)
  val keepNamespace = args(2)
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
 
trait Logger {

  trait Mixin extends Logger {
    val minLogLevel = Logger.this.minLogLevel
    def log(level: LogLevel, msg: String) = Logger.this.log(level, msg)
  }

  val minLogLevel: LogLevel

  def debug(msg: String): Unit = trylog(Debug, msg)
  def info(msg: String): Unit = trylog(Info, msg)
  def warning(msg: String): Unit = trylog(Warning, msg)
  def error(msg: String): Unit = trylog(Error, msg)

  def trylog(level: LogLevel, msg: String) {
    if (level >= minLogLevel) log(level, msg)
  }

  def log(level: LogLevel, msg: String)
}

class ConsoleLogger(val minLogLevel: LogLevel) extends Logger {

  def log(level: LogLevel, msg: String) {
    println(level.toString + ": " + msg)
  }

}
