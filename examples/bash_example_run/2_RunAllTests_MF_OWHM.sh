#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-owhm/output

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-owhm

cd 0_Base
$OWHM OWHM_Examples.nam

cd ../1_GHB
$OWHM OWHM_Examples.nam

cd ../2_FMP
$OWHM OWHM_Examples.nam

cd ../3_SFR
$OWHM OWHM_Examples.nam

cd "$SHELLDIR"

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

if [ "$1" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi
