#!/bin/sh

QUICKSHAKE_CP=target/scala_2.7.7/classes/
SCALA_CP=project/boot/scala-2.7.7/lib/scala-library.jar

LIBRARY_DIR=lib_managed/scala_2.7.7/compile
COMMONS_IO=$LIBRARY_DIR/commons-io-1.4.jar
ASM=$LIBRARY_DIR/asm-3.2.jar
ASM_COMMONS=$LIBRARY_DIR/asm-commons-3.2.jar

LIB_CP=$COMMONS_IO:$ASM:$ASM_COMMONS

export CLASSPATH=$QUICKSHAKE_CP:$SCALA_CP:$LIB_CP

MAIN=net.negatory.quickshake.QuickShake

PROFILEOPTS=
#PROFILEOPTS='-agentlib:hprof=cpu=samples'

java $PROFILEOPTS $MAIN $@
