package net.negatory.quickshake

import LogLevel._

class ShakeFactory(val logger: Logger) {

  import actors.Actor
  import actors.Actor._
  import java.io.File

  val exitHandler = new ExitHandler with logger.LoggerMixin { start }

  trait ShakeMixin extends logger.LoggerMixin with exitHandler.TrapMixin

  def newStatsTracker() = new StatsTracker with ShakeMixin { start }
  def newDirDataReader(dir: File) = new DirectoryDataReader(dir) with ShakeMixin { start }
  def newJarDataReader(jar: File) = new JarDataReader(jar) with ShakeMixin { start }
  def newDirDataWriter(dir: File) = new DirectoryDataWriter(dir) with ShakeMixin { start }
  def newJarDataWriter(jar: File) = new JarDataWriter(jar) with ShakeMixin { start }
  def newDecoder(classData: Array[Byte]) = new ClassDecoder(classData) with ShakeMixin { start }
  def newTerminator() = new Terminator with ShakeMixin { start }
}

