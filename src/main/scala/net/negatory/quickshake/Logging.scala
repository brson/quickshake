package net.negatory.quickshake

object LogLevel extends Enumeration {
  type LogLevel = Value
  val Debug, Info, Warning, Error = Value
}

import LogLevel._

trait Logging extends Logger

trait Logger {

  // Allows creation of a single logger, then mixing in the instance's Mixin trait
  trait LoggerMixin extends Logging {
    override val minLogLevel = Logger.this.minLogLevel
    override def log(level: LogLevel, msg: String) = Logger.this.log(level, msg)
  }

  val minLogLevel: LogLevel = Debug

  // TODO: Consider eliding this method
  def debug(msg: => String): Unit = trylog(Debug, msg)
  def info(msg: => String): Unit = trylog(Info, msg)
  def warning(msg: => String): Unit = trylog(Warning, msg)
  def error(msg: => String): Unit = trylog(Error, msg)

  def trylog(level: LogLevel, msg: => String) {
    if (level >= minLogLevel) log(level, msg)
  }

  def log(level: LogLevel, msg: String) {}
}

class ConsoleLogger(override val minLogLevel: LogLevel) extends Logging {

  override def log(level: LogLevel, msg: String) {
    println(level.toString + ": " + msg)
  }

}

