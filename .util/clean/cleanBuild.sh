#!/bin/env bash 
#
# Delete the CMake and Meson build directories:
#    build/                                 (root project; also holds build/vscode-* and build/zed-*)
#    postprocessors/zonebudget/build/
#    postprocessors/hydfmt/build/
#  and the cmake/__cmake_systeminformation folder that "cmake --system-information" leaves behind.
#
# The intermediate files of GNU Make and Visual Studio live in obj/ and lib/,
#  which cleanObj.sh and cleanLib.sh handle.
#
#---- Get Shell Scripts path  ------------------------------------------------------------
#
SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
#
#---- Repository root (absolute, so rm -rf never sees a relative or empty path)  ---------
#
ROOT="$( cd ../.. && pwd )"
#
#---- Clean  -----------------------------------------------------------------------------
#
echo 
for DIR in "${ROOT}/build"                             \
           "${ROOT}/postprocessors/zonebudget/build"   \
           "${ROOT}/postprocessors/hydfmt/build"       \
           "${ROOT}/cmake/__cmake_systeminformation"
do
   echo "BUILD Clean: rm -rf ${DIR#${ROOT}/}"
   if [ -d "${DIR}" ]; then
      rm -rf "${DIR}"
   fi
done
echo 
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
#---- Check For "nopause"  ---------------------------------------------------------------
#
if [ "$1" != "nopause" ]
then
   echo
   read -p "Press [Enter] to end script  "
fi
