package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import java.io.{File, FileInputStream}

object ClassDataReader {
  case object Search
  case class Visit(origFile: File, classData: Array[Byte])
  case object End
}

class ClassDataReader(root: File) extends Actor with Logging {

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
      reply(Visit(classFile, loadClassData(classFile)))
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
      stream.close
    }
  }
}

