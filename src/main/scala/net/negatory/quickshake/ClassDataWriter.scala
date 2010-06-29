package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

object ClassDataWriter {
  case class AddClass(className: ClassName, classData: Array[Byte])
  case object End
}

class ClassDataWriter(dir: String) extends Actor with Logging {
  
  val outputDir = new File(dir)

  def act() {
    import ClassDataWriter._
    loop {
      react {
	case AddClass(className, classData) => addClass(className, classData)
	case End => exit
      }
    }
  }

  private def addClass(className: ClassName, classData: Array[Byte]) {
    val filePath = new File(outputDir, className.filePath)

    debug("Writing " + className + " to " + filePath)

    assert(filePath.getParent != null)
    import org.apache.commons.io.FileUtils.forceMkdir
    forceMkdir(filePath.getParentFile)
  }
}

