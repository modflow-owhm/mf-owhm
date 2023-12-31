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
if [ "$OS" = "Windows_NT" ]
then
  touch ../../bin/mf-owhm.exe
else
  touch ../../bin/mf-owhm.nix
fi
#
#---- Run Script  ------------------------------------------------------------------------
#
echo 
echo "BIN Clean: ${FND} ./bin -not -name 'bin' -not -name '.keep' -not -name 'readme.txt' -delete"
${FND} ../../bin -not -name 'bin' -not -name '.keep' -not -name 'readme.txt' -delete
#
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
