#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-swi/output

CWD="$(pwd)"

nopause=" "
debug="release"
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

source ./0_SelectProgramUsedByBASH.sh $debug gmg

cd "$SHELLDIR"

cd ../mf-owhm-gmg

"${OWHM}"  owhm_gmg_example.nam

echo
echo  
echo "*** MF-OWHM MF-OWHM-GMG HAS FINISHED ****"
echo "  **SEE output DIRECTORY FOR RESULTS**"
echo "   *****************************  "
echo "     *************************    "
echo "      **********************      "
echo "        ******************        "
echo "          **************          "
echo "            **********            "
echo "              ******              "
echo "                **                "
echo   
echo   

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi

nopause=
debug=
cd "$CWD"