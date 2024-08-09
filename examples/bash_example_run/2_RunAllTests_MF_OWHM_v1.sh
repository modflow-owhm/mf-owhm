#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN TEST-OUT

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

cd ../mf-owhm-v1

echo
echo "Starting MF-OWHMv1 Example A - PCG Solver (takes ~90 minutes)"
echo

"${OWHM}" owhm_v1_example_a_pcg.nam

echo
echo "Starting MF-OWHMv1 Example B - NWT Solver (takes ~80 minutes)"
echo

"${OWHM}" owhm_v1_example_b_nwt.nam

echo
echo  
echo "*** MF-OWHMv1 EXAMPLE HAS FINISHED ****"
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

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi

nopause=
debug=
cd "$CWD"