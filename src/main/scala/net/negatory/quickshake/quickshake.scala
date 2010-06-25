package net.negatory.quickshake

import org.objectweb.asm._

object QuickShake {

  def main(args: Array[String]) {

    val options = new Options(args)

    println ("indirs: ")
    options.indirs foreach (println _)
    println ("outdir: " + options.outdir)
    println ("keepNamespace: " + options.keepNamespace)
  }

}

class Options(args: Array[String]) {

  val indirs = args(0) split ":"
  val outdir = args(1)
  val keepNamespace = args(2)
}
