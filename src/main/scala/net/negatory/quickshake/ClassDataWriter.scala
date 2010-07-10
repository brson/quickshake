package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File

object ClassDataWriter {
  case class AddClass(
    className: ClassName,
    classData: Array[Byte]
    )
  case object End
}

trait ClassDataWriter extends Actor

class DirectoryDataWriter(outputDir: File) extends ClassDataWriter with Logging {
  
  def act() {
    import ClassDataWriter._
    loop {
      react {
	case AddClass(className, classData) =>
	  addClass(className, classData)
	case End => reply('done); exit()
      }
    }
  }

  private def addClass(
    className: ClassName,
    classData: Array[Byte]
  ) {
    import org.apache.commons.io.FileUtils
    import FileUtils.{forceMkdir, writeByteArrayToFile}

    val filePath = new File(outputDir, className.filePath)

    debug("Writing " + className + " to " + filePath)

    val dirPath = filePath.getParentFile
    assert(dirPath != null)

    forceMkdir(dirPath)

    writeByteArrayToFile(filePath, classData)
  }
}

class JarDataWriter(jar: File) extends ClassDataWriter with Logging {
  def act() {
    import ClassDataWriter._
    import org.apache.commons.io.IOUtils.closeQuietly
    import java.util.jar.JarOutputStream
    import java.util.zip.ZipEntry
    import java.io.{BufferedOutputStream, FileOutputStream}

    val jarOutput = new JarOutputStream(
      new BufferedOutputStream(
	new FileOutputStream(jar)
      )
    )

    loop {
      react {
	case AddClass(className, classData) =>
	  val entry = new ZipEntry(className.filePath)
	  jarOutput.putNextEntry(entry)
	  jarOutput.write(classData)
	case End =>
	  reply('done)
	  // TODO: This isn't enough to guarantee that jarOutput gets closed
	  closeQuietly(jarOutput)
	  exit()
      }
    }
  }
}
