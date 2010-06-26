package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import org.objectweb.asm._

object QuickShake {

  def main(args: Array[String]) {

    val logger = new ConsoleLogger
    import logger._

    val options = new Options(args)

    info("indirs: ")
    options.indirs foreach (info _)
    info("outdir: " + options.outdir)
    info("keepNamespace: " + options.keepNamespace)
  }

}

object ClassRepo {
  case object Search
  case class Visit(classData: Array[Byte])
  case object End
}

class ClassRepo extends Actor {
  def act() {}
}

object KeepClassDecider {
  case class Keep(className: String)
  case class Decide(className: String)
  case object Kept
  case object Discarded
}

class KeepClassDecider extends Actor {
  def act() {}
}

object DependencyScanner {
  case object Scan
  case class DependencyFound(className: String)
  case object End
}

class DependencyScanner(classData: Array[Byte]) extends Actor {
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

  val minLogLevel: LogLevel

  def debug(msg: String): Unit = log(Debug, msg)
  def info(msg: String): Unit = log(Info, msg)
  def warning(msg: String): Unit = log(Warning, msg)
  def error(msg: String): Unit = log(Error, msg)

  protected[this] def log(level: LogLevel, msg: String)
}

import LogLevel.LogLevel

class ConsoleLogger extends Logger {

  override val minLogLevel = LogLevel.Debug

  protected[this] override def log(level: LogLevel, msg: String) {
    println(level.toString + ": " + msg)
  }

}
