#!/bin/env bash 
#
# Set command arg to 
#  "example" to only do example/outputs
#  "lib"     to only do lib folder
#  "object"  to only do the obj folder
#  "build"   to only do the CMake and Meson build folders
#  "nopause" disables stopping script at completion.
# 
#    Not including one or more of 
#       "example", "lib", "object", or "build" args
#       will automatically select all four.
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
