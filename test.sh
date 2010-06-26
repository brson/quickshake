#!/bin/sh

java -cp target/scala_2.8.0.RC6/classes/:/home/brian/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.8.0.RC6.jar:/home/brian/.ivy2/cache/commons-io/commons-io/jars/commons-io-1.4.jar net.negatory.quickshake.QuickShake /home/brian/Dev/stoptime/target/scala_2.8.0-local/classes/:/home/brian/Dev/scala/build/quick/classes/library/ testoutput/ net.negatory.stoptime $1

