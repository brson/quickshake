package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.{File, FileInputStream}
import org.apache.commons.io.IOUtils.closeQuietly

object ClassDataReader {
  case object Search
  case class Visit(origFile: Option[File], classData: Array[Byte])
  case object End
}

trait ClassDataReader extends Actor

class DirectoryDataReader(root: File) extends ClassDataReader with Logging {

  import org.apache.commons.io.IOUtils.toByteArray
  import org.apache.commons.io.DirectoryWalker
  import ClassDataReader._

  def act() {
    react {
      case Search => search()
    }
  }

  private def search() {
    debug("Searching for class files in " + root)
    for (classFile <- allClassFiles(root)) {
      debug("Found " + classFile)
      reply(Visit(Some(classFile), loadClassData(classFile)))
    }
    reply(End)
  }

  private def allClassFiles(root: File): List[File] = {
    new DirectoryWalker {
      protected[this] override def handleFile(file: File, depth: Int, results: java.util.Collection[_]): Unit = {
	import java.util.Collection
	if (file.getName.endsWith(".class")) results.asInstanceOf[Collection[File]].add(file)
      }
      def findClassFiles() = {
	import scala.collection.jcl
	val classFiles = new java.util.ArrayList[File]
	walk (root, classFiles)
	new jcl.ArrayList(classFiles).toList
      }
    }.findClassFiles
  }

  private def loadClassData(file: File): Array[Byte] = {
    val stream = new FileInputStream(file)
    try {

      toByteArray(stream)
    }
    finally {
      closeQuietly(stream)
    }
  }
}

class JarDataReader(jar: File) extends ClassDataReader with Logging {

  import java.io.{InputStream, BufferedInputStream}
  import java.util.jar.JarInputStream
  import ClassDataReader._

  def act() {
    react {
      case Search => search()
    }
  }

  def search() {
    debug("Beginning search of jar file " + jar.getPath)

    val jarStream = new JarInputStream(
      new BufferedInputStream(
	new FileInputStream(jar)
      )
    )
    try {

      var entry = jarStream.getNextEntry()
      while(entry != null) {

	debug("Reading jar entry " + entry.getName)

	if (entry.getName().endsWith(".class")) {
	  require(entry.getSize <= Math.MAX_INT, "Can't support this ridiculously huge class")
	  val classData = new Array[Byte](entry.getSize.toInt)
	  val classDataSize = classData.length
	  def readClass(classData: Array[Byte], start: Int): Unit = {
	    val bytesLeft = classDataSize - start
	    val read = jarStream.read(classData, start, bytesLeft)
	    if (read < bytesLeft) readClass(classData, start + read)
	    else ()
	  }
	  readClass(classData, 0)
	  reply(Visit(None, classData))
	}
	
	entry = jarStream.getNextEntry()
      }
      
    } finally {
      closeQuietly(jarStream)
      reply(End)
    }
	
  }
}
