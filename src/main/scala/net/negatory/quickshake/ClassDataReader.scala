package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ClassDataReader {
  case object Search
  case class Visit(origFile: String, classData: Array[Byte])
  case object End
}

class ClassDataReader(root: String) extends Actor with Logging {

  import org.apache.commons.io.IOUtils.toByteArray
  import org.apache.commons.io.DirectoryWalker
  import java.io.{File, FileInputStream}
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
      reply(Visit(classFile, loadClassData(classFile)))
    }
    reply(End)
  }

  private def allClassFiles(root: String): List[String] = {
    new DirectoryWalker {
      protected[this] override def handleFile(file: File, depth: Int, results: java.util.Collection[_]): Unit = {
	import java.util.Collection
	if (file.getName.endsWith(".class")) results.asInstanceOf[Collection[String]].add(file.getAbsolutePath)
      }
      def findClassFiles() = {
	val classFiles = new java.util.ArrayList[String]
	walk (new File(root), classFiles)
	import collection.JavaConversions._
	val classFilesIter: Iterable[String] = classFiles 
	classFilesIter.toList
      }
    }.findClassFiles
  }

  private def loadClassData(file: String): Array[Byte] = {
    val stream = new FileInputStream(file)
    try {
      toByteArray(stream)
    }
    finally {
      stream.close
    }
  }
}

