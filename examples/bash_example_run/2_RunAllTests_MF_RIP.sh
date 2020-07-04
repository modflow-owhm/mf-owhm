#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-rip/output

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../mf-rip

$OWHM RIP-ET_EX1.nam

cd "$SHELLDIR"

echo
echo  
echo "****MF-OWHM RIP TESTS HAVE FINISHED****"
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
