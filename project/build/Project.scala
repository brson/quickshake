import sbt._
import java.io.File

class Project(info: ProjectInfo) extends DefaultProject(info) {

  val objectwebRepo = "Objectweb Maven" at "http://maven.ow2.org/maven2"
  val asm = "asm" % "asm" % "3.2"
}
