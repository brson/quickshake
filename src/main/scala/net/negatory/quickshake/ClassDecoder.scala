package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import scala.concurrent.TaskRunner

object ClassDecoder {
  case object GetName
  case class Name(className: ClassName)
  case object Discard
  case object FindDependencies
  case class Dependency(className: ClassName)
  case object End
}

class ClassDecoder(
  classData: Array[Byte],
  runner: TaskRunner
) extends Actor with Logging {

  import ClassDecoder._
  import org.objectweb.asm._
  import org.objectweb.asm.commons.EmptyVisitor
  
  def act() {
    react {
      case GetName =>
	getName()
	react {
	  case Discard => Unit; exit()
	  case FindDependencies =>
	    findDependencies()
	    reply(End)
	    exit()
	}
    }
  }

  private def getName() {
    import runtime.NonLocalReturnControl

    val visitor = new EmptyVisitor {
      override def visit(
	version: Int,
	access: Int,
	name: String,
	signature: String,
	superName: String,
	interfaces: Array[String]
      ) {
	reply(Name(new ClassName(name)))
	throw new NonLocalReturnControl(Unit, Unit)
      }
    }
    val reader = new ClassReader(classData)
    try {
      // TODO: Are there more efficient flags?
      reader.accept(visitor, 0)
    } catch {
      case _: NonLocalReturnControl[_] =>
    }
  }

  private def findDependencies() {
    val visitor = new EmptyVisitor {
      override def visit(
	version: Int,
	access: Int,
	name: String,
	signature: String,
	superName: String,
	interfaces: Array[String]
      ) {
	reportDependency(new ClassName(superName))
	interfaces foreach {
	  (i: String) => reportDependency(new ClassName(superName))
	}
      }
      
      override def visitAnnotation(
	desc: String,
	visible: Boolean
      ): AnnotationVisitor = {
	reportDependencies(new Descriptor(desc))
	null // TODO
      }

      override def visitInnerClass(
	name: String,
	outerName: String,
	innerName: String,
	access: Int
      ) {
	reportDependency(new ClassName(name))
      }

      override def visitField(
	access: Int,
	name: String,
	desc: String,
	signature: String,
	value: Any
      ): FieldVisitor = {
	reportDependencies(new Descriptor(desc))
	null // TODO
      }

      override def visitMethod(
	access: Int,
	name: String,
	desc: String,
	signature: String,
	exceptions: Array[String]
      ): MethodVisitor = {
	reportDependencies(new Descriptor(desc))
	// exceptions
	null // todo
      }

    }
    
  }

  private def reportDependency(depName: ClassName) {
    debug("Reporting dependency " + depName)
    reply(Dependency(depName))
  }

  private def reportDependencies(desc: Descriptor) {
    desc.classNames foreach { reportDependency _ }
  }
  
}
