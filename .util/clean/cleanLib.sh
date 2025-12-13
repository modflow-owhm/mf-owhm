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
source ../bash_variables/findfind.sh
#
#---- Keep No File Warnings From Happening  ----------------------------------------------
#
touch ../../lib/tmp.del.me
#
#---- Run Script  ------------------------------------------------------------------------
#
echo 
echo "LIB Clean: ${FND} ./lib -not -name 'lib' -not -name '.keep' -delete"
${FND} ../../lib -not -name 'lib' -not -name '.keep' -delete
#
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#