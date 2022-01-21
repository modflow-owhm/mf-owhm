#!/bin/env bash


#---- Get Shell Scripts path  ------------------------------------------------------------

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
#
#---- Get location of find  --------------------------------------------------------------
#
source ../findfind.sh
#
#---- Keep No File Warnings From Happening  ----------------------------------------------
#
touch ../../examples/mf-2005/output/bcf2ss_CumHCHG.txt
touch ../../examples/mf-2005-nwt/output/etsdrt_CumHCHG.txt
touch ../../examples/mf-nwt/output/Pr1aMFNWT_CumHCHG.txt
touch ../../examples/mf-owhm/0_Base/output/CumHCHG.txt
touch ../../examples/mf-owhm/1_GHB/output/CumHCHG.txt
touch ../../examples/mf-owhm/2_FMP/output/CumHCHG.txt
touch ../../examples/mf-owhm/3_SFR/output/CumHCHG.txt
touch ../../examples/mf-owhm-v1/output/owhm_v1_example_a_CumHCHG.txt
touch ../../examples/mf-rip/output/RIP-ET_EX1_CumHCHG.txt
touch ../../examples/mf-swi/output/swi2ex1_CumHCHG.txt
touch ../../examples/mf-swr/output/SWRSample01.01min_CumHCHG.txt
touch ../../examples/bash_example_run/out1_MF_2005.txt
#
#---- Run Script  ------------------------------------------------------------------------
#
echo
echo "Ex  Clean: ${FND} ./examples/*/output/*  -not -name 'output' -not -name '.keep' -delete"
${FND} ../../examples/mf-2005/output         -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-2005-nwt/output     -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-nwt/output          -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-owhm/0_Base/output  -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-owhm/1_GHB/output   -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-owhm/2_FMP/output   -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-owhm/3_SFR/output   -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-owhm-v1/output      -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-rip/output          -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-swi/output          -not -name 'output' -not -name '.keep' -delete
${FND} ../../examples/mf-swr/output          -not -name 'output' -not -name '.keep' -delete
#
echo
echo "Ex  Clean: ${FND} ./examples/bash_example_run/out?_*.txt  -not -name 'output' -not -name '.keep' -delete"
${FND} ../../examples/bash_example_run/out?_*.txt   -not -name 'bash_example_run' -not -name '.keep' -delete
echo
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
