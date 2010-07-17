package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ClassDecoder {
  case object GetProps
  case class Props(props: ClassProps)
  case object Discard
  case object FindDependencies
  case class Method(
    methodName: String,
    classDeps: List[ClassName],
    methodDeps: List[(ClassName, String)]
  )
  case class ClassDependency(className: ClassName)
  case object End
}

class ClassDecoder(classData: Array[Byte]) extends Actor with Logging {

  import ClassDecoder._
  import org.objectweb.asm._
  import org.objectweb.asm.commons.EmptyVisitor
  
  def act() {
    react {
      case GetProps =>
	getClassProps()
	react {
	  case Discard => exit()
	  case FindDependencies =>
	    findDependencies()
	    sender ! End
	    exit()
	}
    }
  }

  private def getClassProps() {
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
	val sup =
	  if (superName != null) new ClassName(name)
	  else error("Visiting java.lang.Object?")
	val inter = 
	  if (interfaces != null ) {
	    interfaces.toList map { new ClassName(_) }
	  }
	  else Nil

	val props = new ClassProps(
	  new ClassName(name), sup, inter
	)
	reply(Props(props))
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
	reportClassDependency(new ClassName(superName))
	interfaces foreach {
	  (i: String) => reportClassDependency(new ClassName(superName))
	}
      }

      class DecoderAnnotationVisitor extends EmptyVisitor {
	override def visitEnum(name: String, desc: String, value: String) {
	  reportClassDependencies(new Descriptor(desc))
	}

	override def visitAnnotation(
	  name: String, desc: String
	): AnnotationVisitor = {
	  reportClassDependencies(new Descriptor(desc))
	  new DecoderAnnotationVisitor
	}
      }

      override def visitAnnotation(
	desc: String,
	visible: Boolean
      ): AnnotationVisitor = {

	reportClassDependencies(new Descriptor(desc))

	new DecoderAnnotationVisitor
      }

      override def visitInnerClass(
	name: String,
	outerName: String,
	innerName: String,
	access: Int
      ) {
	reportClassDependency(new ClassName(name))
      }

      override def visitField(
	access: Int,
	name: String,
	desc: String,
	signature: String,
	value: Any
      ): FieldVisitor = {
	reportClassDependencies(new Descriptor(desc))

	new EmptyVisitor {
	  override def visitAnnotation(
	    desc: String,
	    visible: Boolean
	  ): AnnotationVisitor = {
	    reportClassDependencies(new Descriptor(desc))
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
	val methodName = name

	var classDeps: List[ClassName] = Nil
	var methodDeps: List[(ClassName, String)] = Nil

	def reportClassDependency(className: ClassName) {
	  classDeps = className :: classDeps
	}

	def reportClassDependencies(desc: Descriptor) {
	  classDeps = desc.classNames ::: classDeps
	}

	def reportMethodDependency(className: ClassName, name: String) {
	  methodDeps = (className, name) :: methodDeps
	}

	def reportNameOrDescriptor(s: String) = if (ClassName.rawIsNotADescriptor(s)) {
	  reportClassDependency(new ClassName(s))
	} else {
	  reportClassDependencies(new Descriptor(s))
	}

	reportClassDependencies(new Descriptor(desc))
	if (exceptions != null ) {
	  exceptions foreach { e => reportClassDependency(new ClassName(e)) }
	}

	new EmptyVisitor {
	  override def visitAnnotation(
	    desc: String,
	    visible: Boolean
	  ): AnnotationVisitor = {
	    reportClassDependencies(new Descriptor(desc))
	    new DecoderAnnotationVisitor
	  }

	  override def visitAnnotationDefault() = new DecoderAnnotationVisitor

	  override  def visitTypeInsn(opcode: Int, `type`: String) = reportNameOrDescriptor(`type`)

	  override def visitFieldInsn(
	    opcode: Int,
	    owner: String,
	    name: String,
	    desc: String
	  ) {
	    reportClassDependencies(new Descriptor(desc))
	  }

	  override def visitMethodInsn(
	    opcode: Int,
	    owner: String,
	    name: String,
	    desc: String
	  ) {
	    if (owner != Opcodes.INVOKEDYNAMIC_OWNER) {
	      reportNameOrDescriptor(owner)
	    }
	    reportClassDependencies(new Descriptor(desc))

	    val cn = if (ClassName.rawIsNotADescriptor(owner)) {
	      Some(new ClassName(owner))
	    } else {
	      val desc = new Descriptor(owner)
	      if (!desc.classNames.isEmpty) Some(desc.classNames.head)
	      else {
		// Operations on arrays of primitives, possibly
		// other methods. Shouldn't matter if they're ignored.
		None
	      }
	    }
	    cn match {
	      case Some(cn) => reportMethodDependency(cn, name)
	      case None => ()
	    }
	  }

	  override def visitMultiANewArrayInsn(desc: String, dims: Int) {
	    reportClassDependencies(new Descriptor(desc))
	  }

	  override def visitTryCatchBlock(
	    start: Label,
	    end: Label,
	    handler: Label,
	    `type`: String
	  ) {
	    if (`type` != null) reportClassDependency(new ClassName(`type`))
	  }

	  override def visitLocalVariable(
	    name: String,
	    desc: String,
	    signature: String,
	    start: Label,
	    end: Label,
	    index: Int
	  ) {
	    reportClassDependencies(new Descriptor(desc))
	  }

	  override def visitEnd() {
	    sender ! Method(methodName, classDeps, methodDeps)
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
    
  private def reportClassDependency(depName: ClassName) {
    if (!(cache contains depName)) {
      debug("Reporting dependency " + depName)
      cache += depName
      reply(ClassDependency(depName))
    }
  }

  private def reportClassDependencies(desc: Descriptor) {
    desc.classNames foreach { reportClassDependency _ }
  }

}
