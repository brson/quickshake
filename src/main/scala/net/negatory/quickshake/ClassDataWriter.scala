package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

object ClassDataWriter {
  case class AddClass(
    origFile: Option[File],
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
    origFile: Option[File], 
    className: ClassName,
    classData: Array[Byte]
  ) {
    import org.apache.commons.io.FileUtils
    import FileUtils.{forceMkdir, copyFileToDirectory, writeByteArrayToFile}

    val filePath = new File(outputDir, className.filePath)

    debug("Writing " + className + " to " + filePath)

    val dirPath = filePath.getParentFile
    assert(dirPath != null)

    forceMkdir(dirPath)

    origFile match {
      case Some(file) => copyFileToDirectory(file, dirPath)
      case None => writeByteArrayToFile(filePath, classData)
    }
  }
}

