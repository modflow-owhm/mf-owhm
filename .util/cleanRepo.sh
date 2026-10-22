#!/bin/env bash 
#
# Set command arg to 
#  "example" to only do example/outputs
#  "lib"     to only do lib folder
#  "object"  to only do the obj folder
#  "build"   to only do the CMake and Meson build folders
#  "bin"     to only do the compiled binaries (bin folders)
#  "all"     to do all of the above, including "bin"
#  "nopause" disables stopping script at completion.
#
#    Not including one or more of
#       "example", "lib", "object", "build", "bin", or "all" args
#       will automatically select the first four (binaries are kept).
#
#
#---- Get Shell Scripts path  ------------------------------------------------------------
#
SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
#
#---- Run Script  ------------------------------------------------------------------------
#
bash ./clean/cleanRepoDriver.sh "$@"
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
