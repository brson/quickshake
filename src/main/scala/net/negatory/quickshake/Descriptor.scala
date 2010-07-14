package net.negatory.quickshake

object ClassName {
  def internalize(name: String): String = (name map {
    (char: Char) =>
      if (char == '.') '/'
      else char
  }).mkString
  def rawIsNotADescriptor(raw: String) = {
    import Descriptor._
    val noPrefixes = fieldTypePrefixes forall {
      (x: Char) => !(raw startsWith x.toString)
    }
    val noMethod = rawIsNotAMethodDescriptor(raw)
    noPrefixes && noMethod
  }
}

/**
 * Represents an internal class name
 */
class ClassName(val raw: String) {

  import ClassName._

  require(rawIsNotADescriptor(raw))

  private val internalized: String = internalize(raw)

  def isInNamespace(internalizedNamespace: String) = {
    require(
      internalizedNamespace == internalize(internalizedNamespace),
      "Not an internalized namespace: " + internalizedNamespace
    )
    internalized startsWith internalizedNamespace
  }

  def filePath = {
    internalized + ".class"
  }

  override def equals(that: Any) = that match {
    case that: ClassName => internalized equals that.internalized
    case _ => false
  }
  override def hashCode() = internalized hashCode
  override def toString() = internalized
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
	case None => getClassNames(buffer drop 1, current)
      }
    }
    getClassNames(raw, Nil)
  }

  private def trimClassDescriptor(classDesc: String) = {
    require((classDesc startsWith "L") && (classDesc endsWith ";"))
    (classDesc drop 1) take (classDesc.length - 2)
  }

}

case class MethodProps(
  className: ClassName,
  methodName: String,
  classDeps: List[ClassName],
  methodDeps: List[(ClassName, String)]
)

