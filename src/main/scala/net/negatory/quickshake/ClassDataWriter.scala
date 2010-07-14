package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.File
import collection.mutable.HashSet
import ClassEncoder.stripClass

object ClassDataWriter {
  case class AddClass(
    className: ClassName,
    classData: Array[Byte],
    methodsKept: HashSet[String]
  )
  case object End
}

trait ClassDataWriter extends Actor

class DirectoryDataWriter(outputDir: File) extends ClassDataWriter with Logging {
  
  def act() {
    import ClassDataWriter._
    loop {
      react {
	case AddClass(className, classData, keptMethods) =>
	  addClass(className, classData, keptMethods)
	case End => reply('done); exit()
      }
    }
  }

  private def addClass(
    className: ClassName,
    classData: Array[Byte],
    keptMethods: HashSet[String]
  ) {
    import org.apache.commons.io.FileUtils
    import FileUtils.{forceMkdir, writeByteArrayToFile}

    val filePath = new File(outputDir, className.filePath)

    debug("Writing " + className + " to " + filePath)

    val dirPath = filePath.getParentFile
    assert(dirPath != null)

    forceMkdir(dirPath)

    val strippedClass = stripClass(classData, keptMethods)
    writeByteArrayToFile(filePath, strippedClass)
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
	case AddClass(className, classData, keptMethods) =>
	  val entry = new ZipEntry(className.filePath)
	  jarOutput.putNextEntry(entry)
	  val strippedClass = stripClass(classData, keptMethods)
	  jarOutput.write(strippedClass)
	case End =>
	  reply('done)
	  // TODO: This isn't enough to guarantee that jarOutput gets closed
	  closeQuietly(jarOutput)
	  exit()
      }
    }
  }
}
