#!/bin/sh

INDIRS=/home/brian/Dev/stoptime/target/scala_2.8.0-local/classes/:/home/brian/Dev/scala/build/pack/lib/scala-library.jar
OUTDIR=testoutput/
KEEP=net.negatory.stoptime

./quickshake.sh $INDIRS $OUTDIR $KEEP $1

