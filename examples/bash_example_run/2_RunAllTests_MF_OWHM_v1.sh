#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN TEST-OUT

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-owhm-v1

$OWHM owhm_v1_example_a_pcg.nam

$OWHM owhm_v1_example_b_nwt.nam


cd "$SHELLDIR"

echo
echo  
echo "*** MF-OWHMv1 TESTS HAVE FINISHED ****"
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
