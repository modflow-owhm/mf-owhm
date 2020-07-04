#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-swi/output

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-swr

$OWHM  SWRSample01.nam
$OWHM  SWRSample01.01min.nam
$OWHM  SWRSample02.nam
$OWHM  SWRSample03.nam
$OWHM  SWRSample04.nam
$OWHM  SWRSample05-nwt.nam

cd "$SHELLDIR"

echo
echo  
echo "*** MF-OWHM MF-SWR  HAVE  FINISHED ****"
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
