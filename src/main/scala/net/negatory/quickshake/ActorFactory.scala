package net.negatory.quickshake

class ActorFactory(val logger: Logger) {

  val exitHandler = new ExitHandler with logger.LoggerMixin
  exitHandler.start()

  trait ShakeMixin extends logger.LoggerMixin with exitHandler.TrapMixin

  val terminator = new Terminator with ShakeMixin
  terminator.start()

  trait TerminationMixin extends ShakeMixin with terminator.TerminationMixin

  import java.io.File

  def newDirDataReader(dir: File) = {
    new DirectoryDataReader(dir) with ShakeMixin
  }.start()
  def newJarDataReader(jar: File) = {
    new JarDataReader(jar) with ShakeMixin
  }.start()
  def newDirDataWriter(dir: File) = {
    new DirectoryDataReader(dir) with ShakeMixin
  }.start()
  def newJarDataWriter(jar: File) = {
    new JarDataReader(jar) with ShakeMixin
  }.start()
  def newKeepClassDecider(keptNamespaces: List[String]) = {
    new KeepClassDecider(keptNamespaces) with ShakeMixin
  }.start()
  def newStatsTracker() = {
    new StatsTracker with ShakeMixin
  }.start()
}
