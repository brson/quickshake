#!/bin/sh

QUICKSHAKE_CP=target/scala_2.7.7/classes/
SCALA_CP=/home/brian/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.7.7.jar
#SCALA_CP=project/boot/scala-2.7.7/lib/scala-library.jar

#LIBRARY_DIR=lib_managed/scala_2.7.7/compile
#COMMONS_IO=$LIBRARY_DIR/commons-io-1.4.jar
#ASM=$LIBRARY_DIR/asm-3.2.jar
#ASM_COMMONS=$LIBRARY_DIR/asm-commons-3.2.jar

LIBRARY_DIR=/home/brian/.ivy2/cache
COMMONS_IO=$LIBRARY_DIR/commons-io/commons-io/jars/commons-io-1.4.jar
ASM=$LIBRARY_DIR/asm/asm/jars/asm-3.2.jar
ASM_COMMONS=$LIBRARY_DIR/asm/asm-commons/jars/asm-commons-3.2.jar

LIB_CP=$COMMONS_IO:$ASM:$ASM_COMMONS

CLASSPATH=$QUICKSHAKE_CP:$SCALA_CP:$LIB_CP

MAIN=net.negatory.quickshake.QuickShake
INDIRS=/home/brian/Dev/stoptime/target/scala_2.8.0-local/classes/:/home/brian/Dev/scala/build/quick/classes/library/
OUTDIR=testoutput/
KEEP=net.negatory.stoptime

java -cp $CLASSPATH $MAIN $INDIRS $OUTDIR $KEEP $1

