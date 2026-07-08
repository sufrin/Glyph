#!/bin/bash
#
# Run a glyph test program after IntelliJ has made the jar artefact
#
#

RELEASE=Glyph
CP=""
# add the Skija lib to the class path
for file in $RELEASE/*.jar
do
 CP=$CP:$file
done

PROGRAM=${1-barents.Barents}
shift

#CLASSPATH=$CP java org.sufrin.glyph.tests.$PROGRAM
#exit

scala -J-Xdock:name=${PROGRAM/*./} -cp $CP org.sufrin.glyph.tests.$PROGRAM "$@" &
#2>/dev/null &



 