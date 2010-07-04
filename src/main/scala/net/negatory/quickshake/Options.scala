package net.negatory.quickshake

import java.io.File

import LogLevel._

trait Options {
  val inputs: List[File]
  val outdir: File
  val keepNamespaces: List[String]
  val logLevel: LogLevel
}

class CommandLineOptions(args: Array[String]) extends Options {

  val inputs = (args(0) split ":").toList map { d => new File(d) }
  val outdir = new File(args(1))
  val keepNamespaces = (args(2) split ":").toList
  val logLevel = args(3) match {
    case "debug" => Debug
    case "info" => Info
    case "warning" => Warning
    case "error" => Error
  }
}
