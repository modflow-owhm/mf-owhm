#!/bin/env bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN mf-2005/output

# Options supported:
#    all      -> include running extremely long test cases
#    check    -> skip running examples and only CHECK the results in the output directories
#    debug    -> runs mf-owhm_debug program instead of mf-owhm
#    fast     -> run test examples as background processes (more than one at once)
#    nopause  -> disables requiring the user to hit pause at the end of the script (auto ends)
#    save     -> do not delete intermediate testing program mf_owhm_validate_example_results
#    reuse    -> if mf_owhm_validate_example_results already exists, reuse it instead of recompiling
#                reuse implies the "save" option
#
# Example use:
#             $ ./1_RunValidation.sh all fast
# will run all the examples, including the very long ones,
# and run them in parallel
#
start_time=$SECONDS
echo
echo "  Run Validation Start:  `date +"%T  %F"`"
echo

CWD="$(pwd)"

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"
status=66

nopause=" "
debug=" "
runExamples="T"
savePROG="F"
multiThread="F"
runAll=" "
reUse="F"
for ARG in "$@" 
do
  case "${ARG:0:1}" in
   d | D )
          debug="debug"
           ;;
   n | N )
          nopause="nopause"
           ;;
   c | C )
          runExamples="F"
           ;;
   s | S )
          savePROG="T"
           ;;
   f | F )
          multiThread="T"
           ;;
   a | A | - )
          runAll="-a"
           ;;
   r | R | - )
          reUse="T"
           ;;
  esac
done

if [ "${runExamples}" = "T" ]
then
   source ./0_SelectProgramUsedByBASH.sh $debug
   cd "$SHELLDIR"

   echo
   echo "  Cleaning out the old example files"
   echo
   bash ../../.util/clean/cleanExampleOutput.sh
   echo
   echo "  Test binary: ${OWHM}"
   echo
   curTim=`date +"%T"`
   echo "  Starting to run tests at ${curTim}"
   curTim=
   echo
   
   cd "$SHELLDIR"
   if [ "${multiThread}" = "F" ]
   then
      echo "    MF_2005      test 1 of 8"
      bash 2_RunAllTests_MF_2005.sh     "nopause"  $debug  > out1_MF_2005.txt
      
      echo "    MF_2005_NWT  test 2 of 8"
      bash 2_RunAllTests_MF_2005_NWT.sh "nopause"  $debug  > out2_MF_2005_NWT.txt
      
      echo "    MF_CFP       test 3 of 8"
      bash 2_RunAllTests_MF_CFP.sh      "nopause"  $debug  > out3_MF_CFP.txt
      
      echo "    MF_NWT       test 4 of 8"
      bash 2_RunAllTests_MF_NWT.sh      "nopause"  $debug  > out4_MF_NWT.txt
      
      echo "    MF_OWHM      test 5 of 8"
      bash 2_RunAllTests_MF_OWHM.sh     "nopause"  $debug  > out5_MF_OWHM.txt
      
      echo "    MF_RIP       test 6 of 8"
      bash 2_RunAllTests_MF_RIP.sh      "nopause"  $debug  > out6_MF_RIP.txt
      
      echo "    MF_SWI       test 7 of 8"
      bash 2_RunAllTests_MF_SWI.sh      "nopause"  $debug  > out7_MF_SWI.txt
      
      echo "    MF_SWR       test 8 of 8"
      bash 2_RunAllTests_MF_SWR.sh      "nopause"  $debug  > out8_MF_SWR.txt
   else
      trap "exit" INT TERM ERR
      trap "kill 0" EXIT
      
      bash 2_RunAllTests_MF_2005_NWT.sh "nopause"  $debug  > out2_MF_2005_NWT.txt & echo "    MF_2005_NWT  test 2 of 8 -> Started"
      bash 2_RunAllTests_MF_CFP.sh      "nopause"  $debug  > out3_MF_CFP.txt      & echo "    MF_CFP       test 3 of 8 -> Started"
      bash 2_RunAllTests_MF_OWHM.sh     "nopause"  $debug  > out5_MF_OWHM.txt     & echo "    MF_OWHM      test 5 of 8 -> Started"
      bash 2_RunAllTests_MF_RIP.sh      "nopause"  $debug  > out6_MF_RIP.txt      & echo "    MF_RIP       test 6 of 8 -> Started"
      wait
      echo "                                Tests 2, 3, 5, 6 Finished"
      bash 2_RunAllTests_MF_NWT.sh      "nopause"  $debug  > out4_MF_NWT.txt      & echo "    MF_NWT       test 4 of 8 -> Started"
      bash 2_RunAllTests_MF_SWI.sh      "nopause"  $debug  > out7_MF_SWI.txt      & echo "    MF_SWI       test 7 of 8 -> Started"
      wait
      echo "                                Tests 4, 7 Finished"
      bash 2_RunAllTests_MF_2005.sh     "nopause"  $debug  > out1_MF_2005.txt     & echo "    MF_2005      test 1 of 8 -> Started"
      bash 2_RunAllTests_MF_SWR.sh      "nopause"  $debug  > out8_MF_SWR.txt      & echo "    MF_SWR       test 8 of 8 -> Started"
      wait
      echo "                                Tests 1, 8 Finished"
      echo
   fi

   if [ "${runAll}" = "-a" ]
   then
      curTim=`date +"%T"`
      echo "    MF_OWHM_v1   extended test 1 of 1 -> Started at ${curTim} and takes ~3hr"
      bash 2_RunAllTests_MF_OWHM_v1.sh      "nopause"  $debug  > out9_MF_OWHM_v1.txt
      curTim=
   fi
else
   echo
   echo "  Skipping the run tests and"
   echo "    reusing existing example output"
   echo
fi

echo
echo "  Now checking results..."
echo

cd "$SHELLDIR"

if [ "$OS" = "Windows_NT" ]
then
   prog=./mf_owhm_validate_example_results.exe
else
   touch ./tmp_fs_access_check_SsFfEe > /dev/null 2> /dev/null  # Check for access to script location
   status=$?
   if [ $status -eq 0 ];
   then
      rm -f ./tmp_fs_access_check_SsFfEe > /dev/null 2> /dev/null
      prog=./mf_owhm_validate_example_results.nix
   else
      prog=~/mf_owhm_validate_example_results.nix
   fi
fi

# Check if program already exists and is executable
[ "${reUse}" = "T" ] && [ ! -x "${prog}" ] && reUse="F"

if [ "${reUse}" = "T" ]
then
   echo
   echo "Reuse option found."
   echo "now running existing: ${prog}"
   echo
   echo
   status=0
   "${prog}" "${runAll}" || status=1
else
   status=0
   gfortran  -o "${prog}"  -fno-backtrace  validate_example_results.f90 || status=1
   if [ $status -eq 0 ]; then
      "${prog}" "${runAll}" || status=1
   fi
fi

if [ "${savePROG}" = "F" ]  && [ "${reUse}" = "F" ]
then
   echo "Removing ${prog}"
   rm -f ${prog}
else
   echo "Save or Reuse options found."
   echo "keeping: ${prog}"
fi

echo
echo
echo  
echo "***VALIDATION TESTS HAVE FINISHED***"
echo  
echo  
echo "************************************"
if [ $status -eq 0 ]; then
echo "***  ALL TESTS      PASSED       ***"
else
echo "***  SOME TESTS     FAILED       ***"
fi
echo "************************************"
echo "  *******************************  "
echo "    **************************   "
echo "     **********************      "
echo "       ******************        "
echo "         **************          "
echo "           **********            "
echo "             ******              "
echo "               **                "
echo   
echo   

debug=
runExamples=
savePROG=
multiThread=
runAll=
reUse=
prog=
cd "$CWD"

echo
echo "  Run Validation Finish At: `date +"%T  %F"`"
echo
elapsed=$(( SECONDS - start_time ))
if [ $elapsed -lt 86400 ]
then
   elapsed=$(eval "echo $(date -ud "@$elapsed" +'%H hr %M min %S sec')")
else
   elapsed=$(eval "echo $(date -ud "@$elapsed" +'$((%s/3600/24)) days %H hr %M min %S sec')")
fi
echo "  Run Validation Runtime:   $elapsed"
echo
echo
start_time=
elapsed=

if [ "$nopause" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi
nopause=


[ $status -ne 0 ] && exit 66