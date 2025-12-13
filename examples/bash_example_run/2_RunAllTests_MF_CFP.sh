#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-owhm/output

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

cd ../mf-cfp

cd cfp_mode1_benchmark_examples/exch
"${OWHM}" exch.nam

cd ../q_lam
"${OWHM}" q_lam.nam

cd ../q_turb
"${OWHM}" q_turb.nam

cd ../../cfp_mode1_example
"${OWHM}" cfp_mode1_example.nam

# Currently only MODE 1 is supported in MF-OWHM
# cd ../cfp_mode2_example
# "${OWHM}" cfp_mode2_example.nam
# 
# cd ../cfp_mode3_example
# "${OWHM}" cfp_mode3_example.nam


echo
echo  
echo "****MF-OWHM CFP TESTS HAVE FINISHED****"
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