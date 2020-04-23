#!/bin/env bash 


#---- Get Shell Scripts path  ------------------------------------------------------------

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD=$(pwd)
#
cd "$SHELLDIR"
#
#---- Get location of find  --------------------------------------------------------------
#
source ../findfind.sh
#
#---- Run Script  ------------------------------------------------------------------------
#
echo 
echo "OBJ Clean: ${FND} ./obj -not -name 'obj' -not -name '.keep' -delete"
${FND} ../../obj -not -name 'obj' -not -name '.keep' -delete
#
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
