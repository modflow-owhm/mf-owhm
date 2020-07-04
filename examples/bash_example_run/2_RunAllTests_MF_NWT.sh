#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-nwt/output

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-nwt

$OWHM Pr1a_MFNWT.nam

$OWHM Pr1b_MFNWT.nam

$OWHM Pr2MFNWT.nam

$OWHM Pr3_MFNWT_higher.nam

$OWHM Pr3_MFNWT_lower.nam

$OWHM swi2ex4sww.nam

cd "$SHELLDIR"

echo
echo  
echo "****MF-OWHM NWT TESTS HAVE FINISHED****"
echo " **SEE output DIRECTORY FOR RESULTS**  "
echo "   *******************************     "
echo "     ***************************       "
echo "       ***********************         "
echo "         *******************           "
echo "           ***************             "
echo "            ************               "
echo "              ********                 "
echo "                ****                   "
echo "                 **                    "
echo     
echo 

if [ "$1" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi
