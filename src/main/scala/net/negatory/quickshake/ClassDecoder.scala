package net.negatory.quickshake

import actors.Actor
import actors.Actor._
import scala.concurrent.TaskRunner

object ClassDecoder {
  case object GetName
  case class Name(className: String)
  case object Discard
  case object FindDependencies
  case class Dependency(className: String)
  case object End
}

class ClassDecoder(private val classData: Array[Byte], private val runner: TaskRunner) extends Actor {
  self: Logger =>
  
  def act() {
    import org.objectweb.asm._
    import org.objectweb.asm.commons.EmptyVisitor
    import ClassDecoder._
    import concurrent.SyncChannel

    val channel = new SyncChannel[Any]
    val decoder = self
    
    val visitor = new EmptyVisitor {
      override def visit(
	version: Int,
	access: Int,
	name: String,
	signature: String,
	superName: String,
	interfaces: Array[String]) {

	channel.write(Name(name))
	channel.read match {
	  case Discard => Unit
	  case FindDependencies => decoder ! End
	}
      }
    }

    val task = () => {
      val reader = new ClassReader(classData)
      // TODO: Are there better flags?
      reader.accept(visitor, 0)
    }

    import runner.functionAsTask
    runner.execute(task)

    react {
      case GetName =>
	reply(channel.read)
	react {
	  case Discard => channel.write(Discard)
	  case FindDependencies =>
	    channel.write(FindDependencies)
            val client = sender
	    loop {
	      react {
		case dep @ Dependency(_) => client ! dep
		case End => client ! End
	      }
	    }
	}
    }
  }
}
