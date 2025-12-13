#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-2005-nwt/output

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

cd ../mf-2005-nwt

"${OWHM}" etsdrt_nwt.nam

"${OWHM}" swtex4_nwt.nam

"${OWHM}" test1ss_nwt.nam

"${OWHM}" test1tr_nwt.nam

"${OWHM}" testsfr2_nwt.nam

"${OWHM}" testsfr2_tab_nwt.nam

"${OWHM}" twrip_nwt.nam

"${OWHM}" UZFtest2_nwt.nam

# note this test takes NWT forever to solve, so it is set as a separate script
#"${OWHM}" MNW2-Fig28_nwt.nam

echo
echo  
echo "***MF-OWHM MF2005 TESTS USING NWT HAVE FINISHED***"
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