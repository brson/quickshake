package net.negatory.quickshake

object ClassEncoder {

  import collection.mutable.HashSet
  import org.objectweb.asm._

  def stripClass(
    classData: Array[Byte],
    keptMethods: HashSet[String]
  ): Array[Byte] = {
    val reader = new ClassReader(classData)
    // TODO: Providing reader to writer is supposed to be
    // an optimization. Does it work here?
    val writer = new ClassWriter(reader, 0)
    val adapter = new ClassAdapter(writer) {
    }

    reader.accept(adapter, 0)

    writer.toByteArray()
  }
}
