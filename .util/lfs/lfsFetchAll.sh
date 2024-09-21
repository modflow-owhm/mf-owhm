#!/bin/env bash 
#
# Download all lfs objects from default remote.
# Also accepts fetching from remote specified as first argument.
#
#---- Get Shell Scripts path  ------------------------------------------------------------
#
SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
cd ../..
#
#---- Run Script  ------------------------------------------------------------------------
#
echo ""
echo "git lfs fetch --all  ${1}"
echo ""
#
git lfs fetch --all
#
#
#---- Check For "nopause"  ---------------------------------------------------------------
#
echo ""
echo ""
read -p "${BLN}Press [ENTER] to end script ... "
echo ""
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
