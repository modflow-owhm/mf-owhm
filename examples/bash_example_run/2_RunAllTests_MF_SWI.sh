#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-swi/output

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-swi

$OWHM swi2ex1.nam
$OWHM swi2ex2_cont.nam
$OWHM swi2ex2_strat.nam
$OWHM swi2ex3.nam
$OWHM swi2ex4_2d.nam
$OWHM swi2ex4_2d_sww.nam
$OWHM swi2ex5.nam
$OWHM swi2ex6_1.nam
$OWHM swi2ex6_2.nam
$OWHM swi2ex6_3_0.005.nam
$OWHM swi2ex6_3_0.010.nam
$OWHM swi2ex6_3_0.100.nam
$OWHM swi2ex6_3_1.000.nam

cd "$SHELLDIR"

echo
echo  
echo "*** MF-OWHM MF-SWI  HAVE  FINISHED ****"
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

if [ "$1" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi
