package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

object ClassDataWriter {
  case class AddClass(
    origFile: File,
    className: ClassName,
    classData: Array[Byte]
    )
  case object End
}

class ClassDataWriter(outputDir: File) extends Actor with Logging {
  
  def act() {
    import ClassDataWriter._
    loop {
      react {
	case AddClass(origFile, className, classData) =>
	  addClass(origFile, className, classData)
	case End => exit
      }
    }
  }

  private def addClass(
    origFile: File, 
    className: ClassName,
    classData: Array[Byte]
  ) {
    val filePath = new File(outputDir, className.filePath)

    debug("Writing " + className + " to " + filePath)

    val dirPath = filePath.getParentFile
    assert(dirPath != null)
    import org.apache.commons.io.FileUtils
    import FileUtils.{forceMkdir, copyFileToDirectory}
    forceMkdir(dirPath)

    copyFileToDirectory(origFile, dirPath)
  }
}

