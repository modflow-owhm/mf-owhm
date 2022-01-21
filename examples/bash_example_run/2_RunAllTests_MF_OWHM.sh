#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-owhm/output

CWD="$(pwd)"

nopause=" "
debug=" "
for ARG in "$@" 
do
  case "${ARG:0:1}" in
   d | D )
          debug="debug"
           ;;
   n | N )
          nopause="nopause"
           ;;
  esac
done

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh $debug

cd "$SHELLDIR"

cd ../mf-owhm

cd 0_Base
"${OWHM}" OWHM_Examples.nam

cd ../1_GHB
"${OWHM}" OWHM_Examples.nam

cd ../2_FMP
"${OWHM}" OWHM_Examples.nam

cd ../3_SFR
"${OWHM}" OWHM_Examples.nam

echo
echo  
echo "***  MF-OWHM TESTS HAVE FINISHED  ****"
echo " **SEE output DIRECTORY FOR RESULTS** "
echo "  *****************************  "
echo "    *************************    "
echo "     **********************      "
echo "       ******************        "
echo "         **************          "
echo "           **********            "
echo "             ******              "
echo "               **                "
echo   
echo   

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi

nopause=
debug=
cd "$CWD"