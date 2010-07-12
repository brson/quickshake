package net.negatory.quickshake

class ShakeFactory(
  options: Options,
  loggerFactory: (LogLevel.LogLevel) => Logger
) {

  import actors.Actor
  import actors.Actor._

  val logger = loggerFactory(options.logLevel)

  val exitHandler = new ExitHandler with logger.LoggerMixin
  exitHandler.start()

  trait ShakeMixin extends logger.LoggerMixin with exitHandler.TrapMixin

  val terminator = new Terminator with ShakeMixin { start }
  val decider = new KeepClassDecider(options.keepNamespaces) with ShakeMixin { start }
  val statsTracker = new StatsTracker with ShakeMixin { start }

  trait TerminationMixin extends ShakeMixin with terminator.TerminationMixin

  import java.io.File

  def newDirDataReader(dir: File) = new DirectoryDataReader(dir) with ShakeMixin { start }
  def newJarDataReader(jar: File) = new JarDataReader(jar) with ShakeMixin { start }
  def newDirDataWriter(dir: File) = new DirectoryDataWriter(dir) with ShakeMixin { start }
  def newJarDataWriter(jar: File) = new JarDataWriter(jar) with ShakeMixin { start }
  def newDecoder(classData: Array[Byte]) = new ClassDecoder(classData) with ShakeMixin { start }

  def trackedActor(body: => Unit) = new Actor with TerminationMixin {
    def act() = body
    start()
  }

  def newMethodCoordinator(
    methodProps: (ClassName, String, List[ClassName], List[String]),
    methodAccumulator: Actor
  ) = trackedActor {

    val (className, methodName, classDeps, methodDeps) = methodProps
    
    decider ! KeepClassDecider.DecideOnMethod(className, methodName)
    react {
      case KeepClassDecider.Kept =>
	methodAccumulator ! MethodAccumulator.KeepMethod(methodName)
	classDeps foreach {
	  decider ! KeepClassDecider.KeepClass(_)
	}
	methodDeps foreach {
	  decider ! KeepClassDecider.KeepMethod(_)
	}
	exit()
      case KeepClassDecider.Discarded =>
	methodAccumulator ! MethodAccumulator.DiscardMethod
	exit()
    }
  }


}

object MethodAccumulator {
  // These will be used to track the set of methods
  // on a class that need to be retained
  case class KeepMethod(methodName: String)
  case object DiscardMethod
}
