package net.negatory.quickshake

object ClassName {
  def internalize(name: String) = name map {
    (char) =>
      if (char == '.') '/'
      else char
  }
  def rawIsNotADescriptor(raw: String) = {
    Descriptor.fieldTypePrefixes forall ((x: Char) => !(raw startsWith x :: Nil))
  }
}

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

object Descriptor {
  val fieldTypePrefixes = "BCDFIJSZL["
  def withoutTypePrefixes(internalFieldName: String) = {
    "^\\[*L".r replaceFirstIn(internalFieldName, "")
  }
}

class Descriptor(val raw: String) {

  val classNames: List[ClassName] = Nil // TODO

}
