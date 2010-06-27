package net.negatory.quickshake

import actors.Actor
import actors.Actor._

object ClassDecoder {
  case object GetName
  case class Name(className: String)
  case object Discard
  case object FindDependencies
  case class Dependency(className: String)
  case object End
}

class ClassDecoder(classData: Array[Byte]) extends Actor {
  self: Logger =>
  def act() {
    import ClassDecoder._
    react {
      case GetName => reply(Name("test"))
      case Discard => exit
      case FindDependencies => reply(End); exit
    }
  }
}
