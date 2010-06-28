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
    // Parse the class names out of the descriptor by pulling out
    // sequences that look like "L.*?;", starting from the beginning
    val headRegex = "L.+?;".r // non-greedy
    def getClassNames(buffer: String, current: List[ClassName]): List[ClassName] = {
      if (buffer isEmpty) current
      else headRegex findPrefixOf buffer match {
	case Some(classDesc) =>
	  val className = trimClassDescriptor(classDesc)
	  val newBuf = buffer drop (classDesc.length)
	  getClassNames(newBuf, new ClassName(className) :: current)
	case None => getClassNames(buffer tail, current)
      }
    }
    getClassNames(raw, Nil)
  }

  private def trimClassDescriptor(classDesc: String) = {
    require((classDesc startsWith "L") && (classDesc endsWith ";"))
    classDesc.tail take (classDesc.length - 2)
  }

}
