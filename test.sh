#!/bin/sh

INPUTS=/home/brian/Dev/stoptime/target/scala_2.8.0-local/classes/:/home/brian/Dev/scala/build/pack/lib/scala-library.jar
OUTPUTS=testoutput.jar
KEEP=net.negatory.stoptime

./quickshake.sh $INPUTS $OUTPUTS $KEEP $1

