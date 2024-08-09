#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-swi/output

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

cd ../mf-swi

"${OWHM}" swi2ex1.nam
"${OWHM}" swi2ex2_cont.nam
"${OWHM}" swi2ex2_strat.nam
"${OWHM}" swi2ex3.nam
"${OWHM}" swi2ex4_2d.nam
"${OWHM}" swi2ex4_2d_sww.nam
"${OWHM}" swi2ex5.nam
"${OWHM}" swi2ex6_1.nam
"${OWHM}" swi2ex6_2.nam
"${OWHM}" swi2ex6_3_0.005.nam
"${OWHM}" swi2ex6_3_0.010.nam
"${OWHM}" swi2ex6_3_0.100.nam
"${OWHM}" swi2ex6_3_1.000.nam

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

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi

nopause=
debug=
cd "$CWD"