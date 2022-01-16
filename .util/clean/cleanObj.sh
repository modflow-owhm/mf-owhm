#!/bin/env bash 


#---- Get Shell Scripts path  ------------------------------------------------------------

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
#
#---- Get location of find  --------------------------------------------------------------
#
source ../findfind.sh
#
#---- Keep No File Warnings From Happening  ----------------------------------------------
#
touch ../../obj/tmp.del.me
touch ../../postprocessors/zonebudget/obj/tmp.del.me
touch ../../postprocessors/hydfmt/obj/tmp.del.me
#
#---- Run Script  ------------------------------------------------------------------------
#
echo 
echo "OBJ Clean: ${FND} ./obj -not -name 'obj' -not -name '.keep' -delete"
${FND} ../../obj -not -name 'obj' -not -name '.keep' -delete

echo 
echo "OBJ Clean: ${FND} ./postprocessors/zonebudget/obj -not -name 'obj' -not -name '.keep' -delete"
${FND} ../../postprocessors/zonebudget/obj -not -name 'obj' -not -name '.keep' -delete

echo 
echo "OBJ Clean: ${FND} ./postprocessors/hydfmt/obj -not -name 'obj' -not -name '.keep' -delete"
${FND} ../../postprocessors/hydfmt/obj -not -name 'obj' -not -name '.keep' -delete
#
#
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
