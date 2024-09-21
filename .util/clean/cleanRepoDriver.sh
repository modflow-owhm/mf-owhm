#!/bin/env bash 
#
# Set command arg to 
#  "example" to only do example/outputs
#  "lib"     to only do lib folder
#  "object"  to only do the obj folder
#  "nopause" disables stopping script at completion.
# 
#    Not including one or more of 
#       "example", "lib", or "object" args
#       will automatically select all three.
#
#shopt -s nocasematch
#
#---- Get Shell Script path  -------------------------------------------------------------
#
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
#---- Get NL and BLN variables  ----------------------------------------------------------
#
source ../bash_variables/setNL.sh
#
#---- Parse Arguments  -------------------------------------------------------------------
#
T="true"
F="false"

ALL=$T

OBJ=$F
LIB=$F
EX=$F
Pause=$T

for ARG in "$@" 
do
  case "${ARG:0:1}" in
   e | E )
          EX=$T
          ALL=$F
           ;;
   l | L )
          LIB=$T
          ALL=$F
           ;;
   o | O )
          OBJ=$T
          ALL=$F
           ;;
   n | N )
          Pause=$F
           ;;
   *) ALL=$T;;
  esac
done

if [ $ALL = $T ]; then
                 EX=$T
                 OBJ=$T
                 LIB=$T
fi

#---- Clean Repo  ------------------------------------------------------------------------
#
#Clean out the object file output
if [ $OBJ = $T ]; then
             bash ./cleanObj.sh "nopause"
fi

#Clean out the object file output
if [ $LIB = $T ]; then
             bash ./cleanLib.sh "nopause"
fi

#Clean out the example problem output
if [ $EX = $T ]; then
             bash ./cleanExampleOutput.sh "nopause"
fi
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
#
#---- Check For "nopause"  ---------------------------------------------------------------
#
if [ $Pause = $T ]
then
   read -p "${NL}Repo Now Clean.${BLN}Press [ENTER] to end script ... "
else
   echo "${BLN}Repo Now Clean.${NL}"
fi
#
echo ""
#---- Random Notes - Ignore  -------------------------------------------------------------
#
# find ./obj -not \(-name 'obj' -or -name '*odt' -or -name '*.jpg' \) -delete
# find ./obj -not -name 'obj' -not -name '.keep' -name '*'  -delete
#
# find ./obj -not \(-name 'obj' -or -name '.keep' \) -delete