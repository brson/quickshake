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

class ClassDecoder(private val classData: Array[Byte], private val runner: TaskRunner) extends Actor {
  self: Logger =>
  
  def act() {
    import org.objectweb.asm._
    import org.objectweb.asm.commons.EmptyVisitor
    import ClassDecoder._
    import concurrent.SyncChannel
    import runtime.NonLocalReturnControl

    val channel = new SyncChannel[Any]
    val decoder = self
    
    val task = () => {

      val visitor = new EmptyVisitor {
	override def visit(
	  version: Int,
	  access: Int,
	  name: String,
	  signature: String,
	  superName: String,
	  interfaces: Array[String]
	) {
	  channel.write(Name(new ClassName(name)))
	  channel.read match {
	    case Discard => 
	      // Short-circuit the rest of the visit for speed
	      throw new NonLocalReturnControl(Unit, Unit)
	    case FindDependencies =>
	      reportDependency(new ClassName(superName))
	      interfaces foreach {
		(i: String) =>
		  reportDependency(new ClassName(i))
	      }
	  }
	}

	override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = {
	  reportDependencies(new Descriptor(desc))
	  null // TODO: Do I need to visit the annotation?
	}

	override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) {
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
	  new EmptyVisitor {
	    // TODO
	  }
	}

	override def visitEnd() = decoder ! End

	private def reportDependency(depName: ClassName) {
	  debug("Reporting dependency " + depName)
	  decoder ! Dependency(depName)
	}

	private def reportDependencies(desc: Descriptor) {
	  desc.classNames foreach { reportDependency _ }
	}
      }

      val reader = new ClassReader(classData)
      try {
	// TODO: Are there better flags?
	reader.accept(visitor, 0)
      } catch {
	case _: NonLocalReturnControl[_] =>
      }
    }

    import runner.functionAsTask
    runner.execute(task)

    react {
      case GetName =>
	val response = channel.read
	val className = response match {
	  case Name(n) => n
	}
	reply(response)
	react {
	  case Discard => channel.write(Discard)
	  case FindDependencies =>
	    channel.write(FindDependencies)
            val client = sender
	    loop {
	      react {
		case dep @ Dependency(_) => client ! dep
		case End => client ! End; exit
	      }
	    }
	}
    }
  }
}
