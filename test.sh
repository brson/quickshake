#!/bin/sh

INDIRS=/home/brian/Dev/stoptime/target/scala_2.8.0-local/classes/:/home/brian/Dev/scala/build/quick/classes/library/
OUTDIR=testoutput/
KEEP=net.negatory.stoptime

./quickshake.sh $INDIRS $OUTDIR $KEEP $1

