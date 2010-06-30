#!/bin/sh

java -cp target/scala_2.7.7/classes/:/home/brian/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.7.7.jar:/home/brian/.ivy2/cache/commons-io/commons-io/jars/commons-io-1.4.jar:/home/brian/.ivy2/cache/asm/asm/jars/asm-3.2.jar:/home/brian/.ivy2/cache/asm/asm-commons/jars/asm-commons-3.2.jar net.negatory.quickshake.QuickShake /home/brian/Dev/stoptime/target/scala_2.8.0-local/classes/:/home/brian/Dev/scala/build/quick/classes/library/ testoutput/ net.negatory.stoptime $1

