#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-2005/output

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-2005

$OWHM bcf2ss.nam
$OWHM etsdrt.nam
$OWHM fhb.nam
$OWHM ibs2k.nam
$OWHM l1a2k.nam
$OWHM l1b2k.nam
$OWHM l1b2k_bath.nam
$OWHM mnw1.nam
$OWHM MNW2-Fig28.nam
$OWHM restest.nam
$OWHM str.nam
$OWHM swtex4.nam
$OWHM tc2hufv4.nam
$OWHM test1ss.nam
$OWHM test1tr.nam
$OWHM testsfr2.nam
$OWHM testsfr2_tab.nam
$OWHM tr2k_s3.nam
$OWHM twri.nam
$OWHM twrihfb.nam
$OWHM twrip.nam
$OWHM UZFtest2.nam

cd "$SHELLDIR"

echo
echo  
echo "***MF-OWHM MF2005 TESTS HAVE FINISHED***"
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
