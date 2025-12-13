#!/bin/env bash 
#
# Push all lfs objects to remote.
# First argument must be the remote name
# Optional second argument "nopause" disables stopping script at end
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
# Requires remote name as an argument
#
if [ "$1" = "" ]
then
   echo ""
   echo "Error you must pass an the git remote name as an argument."
   echo "The available remotes are: "
   echo ""
   git remote -v
   echo ""
   cd "$CWD"
   echo ""
   exit 66
fi
#
#---- Run Script  ------------------------------------------------------------------------
#
echo ""
echo "git lfs push --all  ${1}"
echo ""
#
git lfs push --all "$1"
#
#
#---- Check For "nopause"  ---------------------------------------------------------------
#
echo ""
read -p "${BLN}Press [ENTER] to end script ... "
echo ""
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
