package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object QuickShake {

  def main(args: Array[String]) {

    val logger = new ConsoleLogger
    import logger._

    val options = new Options(args)

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
        decoder ! Decode
        loop {
          react {
            case Name(className) => ()
            case Dependency(className) => ()
            case End => exit
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

class ClassDataReader(dir: String) extends Actor {
  self: Logger =>
  def act() {
    import ClassDataReader._
    react {
      case Search => reply(End); exit
    }
  }
}

object ClassDecoder {
  case object Decode
  case class Name(className: String)
  case class Dependency(className: String)
  case object End
}

class ClassDecoder(classData: Array[Byte]) extends Actor {
  self: Logger =>
  def act() {
    import ClassDecoder._
    react {
      case Decode => reply(End); exit
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
  def act() {}
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
}

object LogLevel extends Enumeration {
  type LogLevel = Value
  val Debug, Info, Warning, Error = Value
}

trait Logger {

  import LogLevel._
 
  trait Mixin extends Logger {
    val minLogLevel = Logger.this.minLogLevel
    def log(level: LogLevel, msg: String) = Logger.this.log(level, msg)
  }

  val minLogLevel: LogLevel

  def debug(msg: String): Unit = log(Debug, msg)
  def info(msg: String): Unit = log(Info, msg)
  def warning(msg: String): Unit = log(Warning, msg)
  def error(msg: String): Unit = log(Error, msg)

  def log(level: LogLevel, msg: String)
}

class ConsoleLogger extends Logger {

  import LogLevel._

  val minLogLevel = Debug

  def log(level: LogLevel, msg: String) {
    println(level.toString + ": " + msg)
  }

}
