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
      override def visitMethod(
	access: Int,
	name: String,
	desc: String,
	signature: String,
	exceptions: Array[String]
      ): MethodVisitor = {
	if (keptMethods contains name) {
	  super.visitMethod(
	    access,
	    name,
	    desc,
	    signature,
	    exceptions
	  )
	} else null
      }
    }

    reader.accept(adapter, 0)

    writer.toByteArray()
  }
}
