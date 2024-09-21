#!/bin/env bash 
#
# Remove all locally stored lfs objects.
# 
# Recommended to run lfsPushAll.sh before 
#    to ensure files are backed up.
#
#---- Get Shell Scripts path  ------------------------------------------------------------
#
SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
#
#---- Get NL and BLN variables  ----------------------------------------------------------
#
source ../bash_variables/setNL.sh
#
#---- Check if you want to continue  -----------------------------------------------------
#
echo "${BLN}About to prune local LFS storage."
read -p "Do you want to continue? (yes/no): " answer
case "$answer" in
    [yY][eE][sS] | [yY] | [yY][eE] )
        echo ""
        ;;
    * )
        echo ""
        echo "Goodbye..."
        echo ""
        exit 0
        ;;
esac
#
#---- Run Script  ------------------------------------------------------------------------
#
cd ../..

echo "git lfs prune --verify-remote"
echo ""
#
git lfs prune --verify-remote
#
#
#---- Pause before exit  -----------------------------------------------------------------
#
echo ""
read -p "${BLN}Press [ENTER] to end script ... "
echo ""
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
