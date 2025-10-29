#!/bin/bash
#
# Run a glyph test program after IntelliJ has made the jar artefact
#
#

SKLIB=/Users/sufrin/GitHomes/Glyph/SkijaLib
CP=$SKLIB/../out/artifacts/Glyph/Glyph.jar 

# add the Skija lib to the class path
for file in $SKLIB/*.jar
do
 CP=$CP:$file
done

PROGRAM=${1-barents.Barents}
shift

scala -J-Xdock:name=${PROGRAM/*./} -cp $CP org.sufrin.glyph.tests.$PROGRAM "$@" &
#2>/dev/null &



 