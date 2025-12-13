#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-rip/output

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

cd ../mf-rip

"${OWHM}" RIP-ET_EX1.nam

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

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi

nopause=
debug=
cd "$CWD"