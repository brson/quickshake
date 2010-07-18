package net.negatory.quickshake

trait Decider {
  val keepNamespaces: List[String]

  private val internalKeepNamespaces = {
    keepNamespaces map { ClassName.internalize _ }
  }

  def isInKeptNs(className: ClassName) = internalKeepNamespaces.foldLeft (false) {
    (res, ns) =>
      res || (className isInNamespace ns)
  }

}

