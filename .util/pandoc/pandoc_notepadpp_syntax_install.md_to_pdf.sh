#!/bin/env bash 
#
#---- Get Shell Scripts Location  --------------------------------------------------------
#
OriginalPath="$(pwd)"
#
WD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
cd "$WD"
#
#---- Run Script  ------------------------------------------------------------------------
#
file=Notepadpp_Syntax_Install
#
meta=`echo "$file" | tr '[:upper:]' '[:lower:]'`
#
# Location of ${file}.md
cd ../../doc/Notepadpp_Syntax_Highlighting
#
echo 
echo "pandoc  ${file}.md  ${file}.pdf"
echo 
#
#
pandoc  ${file}.md  \
        --metadata-file="${WD}/meta-data-${meta}.md.yaml" \
        --lua-filter="${WD}/remove_relative_links.lua" \
        -H "${WD}/formatting.tex" \
        --pdf-engine=lualatex  \
        --toc  \
        -f gfm \
        -t pdf \
        --standalone \
        --embed-resources \
        --variable=linkcolor:blue \
        --variable=geometry:letterpaper \
        --variable=geometry:margin=1in \
        -o ${file}.pdf
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "${OriginalPath}"
#
#
#---- Check For "nopause"  ---------------------------------------------------------------
#
#
if [ "$1" != "nopause" ]
then
   echo
   read -p "Press [Enter] to end script  "
fi
#
#---- End of Scipt  ----------------------------------------------------------------------
#