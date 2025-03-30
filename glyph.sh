#
# Run a glyph test program independently of IntelliJ
#

lib=/Users/sufrin/GitHomes/Glyph/lib

LIBPATH=$lib/Logging.jar:\
$lib/SourceLocation.jar:\
$lib/jwm-0.4.17.jar:\
$lib/skija-macos-arm64-0.116.2.jar:\
$lib/skija-macos-x64-0.116.2.jar:\
$lib/skija-shared-0.116.2.jar:\
$lib/types-0.1.1.jar:\
$lib/scala-xml_2.13-2.2.0.jar:\
$lib/Glyph.jar 

# echo $LIBPATH
PROGRAM=${1-demonstrationBook.Pages}

CLASSPATH=$LIBPATH scala org.sufrin.glyph.tests.$PROGRAM 2>/dev/null &



 