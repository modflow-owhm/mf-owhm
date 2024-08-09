#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-nwt/output

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

cd ../mf-nwt

"${OWHM}" Pr1a_MFNWT.nam

"${OWHM}" Pr1b_MFNWT.nam

"${OWHM}" Pr2MFNWT.nam

"${OWHM}" Pr3_MFNWT_higher.nam

"${OWHM}" Pr3_MFNWT_lower.nam

"${OWHM}" swi2ex4sww.nam

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

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi

nopause=
debug=
cd "$CWD"