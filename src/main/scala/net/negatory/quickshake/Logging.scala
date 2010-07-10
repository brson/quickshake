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
    override def minLogLevel = {
      if (super.minLogLevel < Logger.this.minLogLevel) super.minLogLevel
      else Logger.this.minLogLevel
    }
    override def log(level: LogLevel, msg: String) = Logger.this.log(level, msg)
  }

  def minLogLevel: LogLevel = Error

  // TODO: Consider eliding this method
  def debug(msg: => String): Unit = trylog(Debug, msg)
  def info(msg: => String): Unit = trylog(Info, msg)
  def warning(msg: => String): Unit = trylog(Warning, msg)
  def error(msg: => String): Unit = {
    trylog(Error, msg)
    Runtime.getRuntime().exit(-1)
  }

  def trylog(level: LogLevel, msg: => String) {
    if (level >= minLogLevel) log(level, msg)
  }

  def log(level: LogLevel, msg: String) {}
}

class ConsoleLogger(level: LogLevel) extends Logging {

  override def minLogLevel: LogLevel = level

  override def log(level: LogLevel, msg: String) {
    val levelStr = level match {
      case Debug => "debug"
      case Info  => "info"
      case Warning => "warning"
      case Error => "error"
    }
    println(levelStr + ": " + msg)
  }

}

