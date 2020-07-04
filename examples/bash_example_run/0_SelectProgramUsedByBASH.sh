#!/bin/bash

# This will be the location of the exectuable that will be used by
# All of the batch scripts in this folder
#
# The variable PROGRAM is the name of the executable to run, eg  mf-owhm.exe  or  mf-owhm.nix   or  mf-owhm_debug.exe
#
# The variable OWHM represents the location of owhm that will be used in all the batch scripts
#
#
# ---- Automatically determine extension based on OS  -----------------------------------------------------
#
if [[ $OS -eq "Windows_NT" ]]
then
  PROGRAM=mf-owhm.exe
else
  PROGRAM=mf-owhm.nix
fi
#
#
# ---- Override and use specified PROGRAM. Uncomment and set to use  --------------------------------------
#
#
#PROGRAM=mf-owhm_debug.exe
#PROGRAM=mf-owhm_debug.nix
#
#
#---- Main shell script for determining paths  ------------------------------------------------------------
#
SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD=$(pwd)
#
cd "$SHELLDIR"
cd ../../bin
#
BINPATH=$(pwd)
#
#
OWHM=$BINPATH/$PROGRAM
#
#
# ---- Override the version and path for mf-owhm used by calling scripts  ---------------------------------
# ----                    Uncomment and set to use                        ---------------------------------
#      Note this is an example if you use WINE to run the windows version on linux.
#        Requires WINE/WINEHQ/WINELIB (WINE acronym => Wine Is Not an Emulator)
#
#
# OWHM="wine $BINPATH/mf-owhm.exe"
#
#
#---- Return to calling folder  ---------------------------------------------------------------------------
#
cd "$CWD"

