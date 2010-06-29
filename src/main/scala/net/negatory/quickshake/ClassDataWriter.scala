package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ClassDataWriter {
  case class AddClass(className: ClassName, classData: Array[Byte])
  case object End
}

class ClassDataWriter(dir: String) extends Actor with Logging {
  def act() {
    import ClassDataWriter._
    loop {
      react {
	case AddClass(className, classData) => debug("Writing " + className)
	case End => exit
      }
    }
  }
}

