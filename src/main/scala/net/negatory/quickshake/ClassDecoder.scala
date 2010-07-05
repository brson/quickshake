package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ClassDecoder {
  case object GetName
  case class Name(className: ClassName)
  case object Discard
  case object FindDependencies
  case class ClassDependency(className: ClassName)
  case object End
}

class ClassDecoder(classData: Array[Byte]) extends Actor with Logging {

  import ClassDecoder._
  import org.objectweb.asm._
  import org.objectweb.asm.commons.EmptyVisitor
  
  def act() {
    react {
      case GetName =>
	getName()
	react {
	  case Discard => exit()
	  case FindDependencies =>
	    findDependencies()
	    reply(End)
	    exit()
	}
    }
  }

  private def getName() {
    import runtime.NonLocalReturnException

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
	throw new NonLocalReturnException((), ())
      }
    }
    val reader = new ClassReader(classData)
    try {
      // TODO: Are there more efficient flags?
      reader.accept(visitor, 0)
    } catch {
      case _: NonLocalReturnException[_] =>
    }
  }

  // TODO: Find methods as well
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

      class DecoderAnnotationVisitor extends EmptyVisitor {
	override def visitEnum(name: String, desc: String, value: String) {
	  reportDependencies(new Descriptor(desc))
	}

	override def visitAnnotation(
	  name: String, desc: String
	): AnnotationVisitor = {
	  reportDependencies(new Descriptor(desc))
	  new DecoderAnnotationVisitor
	}
      }

      override def visitAnnotation(
	desc: String,
	visible: Boolean
      ): AnnotationVisitor = {

	reportDependencies(new Descriptor(desc))

	new DecoderAnnotationVisitor
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

	new EmptyVisitor {
	  override def visitAnnotation(
	    desc: String,
	    visible: Boolean
	  ): AnnotationVisitor = {
	    reportDependencies(new Descriptor(desc))
	    new DecoderAnnotationVisitor
	  }
	}
      }

      override def visitMethod(
	access: Int,
	name: String,
	desc: String,
	signature: String,
	exceptions: Array[String]
      ): MethodVisitor = {
	reportDependencies(new Descriptor(desc))
	if (exceptions != null ) {
	  exceptions foreach { e => reportDependency(new ClassName(e)) }
	}

	new EmptyVisitor {
	  override def visitAnnotation(
	    desc: String,
	    visible: Boolean
	  ): AnnotationVisitor = {
	    reportDependencies(new Descriptor(desc))
	    new DecoderAnnotationVisitor
	  }

	  override def visitAnnotationDefault() = new DecoderAnnotationVisitor

	  override  def visitTypeInsn(opcode: Int, `type`: String) {
	    // TODO
	  }

	  override def visitFieldInsn(
	    opcode: Int,
	    owner: String,
	    name: String,
	    desc: String
	  ) {
	    reportDependencies(new Descriptor(desc))
	  }

	  override def visitMethodInsn(
	    opcode: Int,
	    owner: String,
	    name: String,
	    desc: String
	  ) {
	    reportDependencies(new Descriptor(desc))
	  }

	  override def visitMultiANewArrayInsn(desc: String, dims: Int) {
	    reportDependencies(new Descriptor(desc))
	  }

	  override def visitTryCatchBlock(
	    start: Label,
	    end: Label,
	    handler: Label,
	    `type`: String
	  ) {
	    if (`type` != null) reportDependency(new ClassName(`type`))
	  }

	  override def visitLocalVariable(
	    name: String,
	    desc: String,
	    signature: String,
	    start: Label,
	    end: Label,
	    index: Int
	  ) {
	    reportDependencies(new Descriptor(desc))
	  }

	}
      }

    }

    val reader = new ClassReader(classData)
    // TODO: Are there more efficient flags?
    reader.accept(visitor, 0)
  }

  import collection.mutable.HashSet
  val cache = new HashSet[ClassName]
    
  private def reportDependency(depName: ClassName) {
    if (!(cache contains depName)) {
      debug("Reporting dependency " + depName)
      cache += depName
      reply(ClassDependency(depName))
    }
  }

  private def reportDependencies(desc: Descriptor) {
    desc.classNames foreach { reportDependency _ }
  }
  
}
