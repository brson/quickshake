package net.negatory.quickshake

import java.io.File
import LogLevel._

class Shaker(
  options: Options,
  logger: Logger
) {

  val shakeFactory = new ShakeFactory(logger)
  val decider = shakeFactory.newDecider(options.keepNamespaces)
  val statsTracker = shakeFactory.newStatsTracker()
  val terminator = shakeFactory.terminator
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
}

