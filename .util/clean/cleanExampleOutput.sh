#!/bin/env bash 


#---- Get Shell Scripts path  ------------------------------------------------------------

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD=$(pwd)
#
cd "$SHELLDIR"
#
#---- Get location of find  --------------------------------------------------------------
#
source ../findfind.sh
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
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
