package net.negatory.quickshake

object ClassName {
  def internalize(name: String) = name map {
    (char) =>
      if (char == '.') '/'
      else char
  }
  def rawIsNotADescriptor(raw: String) = {
    import Descriptor._
    val noPrefixes = fieldTypePrefixes forall {
      (x: Char) => !(raw startsWith x :: Nil)
    }
    val noMethod = rawIsNotAMethodDescriptor(raw)
    noPrefixes && noMethod
  }
}

object Descriptor {
  val fieldTypePrefixes = "BCDFIJSZL["
  def withoutTypePrefixes(internalFieldName: String) = {
    require(rawIsNotAMethodDescriptor(internalFieldName))
    "^\\[*L".r replaceFirstIn(internalFieldName, "")
  }
  def rawIsNotAMethodDescriptor(raw: String) = !(raw startsWith "(")
}

/**
 * Represents an internal class name
 */
class ClassName(val raw: String) {

  import ClassName._

  require(rawIsNotADescriptor(raw))

  private val internalized = internalize(raw)

  def isInNamespace(internalizedNamespace: String) = {
    require(internalizedNamespace == internalize(internalizedNamespace), "Not an internalized namespace")
    internalized startsWith internalizedNamespace
  }

  override def equals(that: Any) = internalized equals that
  override def hashCode() = internalized hashCode
  override def toString() = internalized
}

/**
 * An internal field or method descriptor
 */
class Descriptor(val raw: String) {

  require(!ClassName.rawIsNotADescriptor(raw))

  val classNames: List[ClassName] = {
    Nil
  }

}
