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
file1=CHANGELOG
file2=CHANGELOG_Features
#
meta=`echo "$file1" | tr '[:upper:]' '[:lower:]'`
#
# Location of ${file1}.md
cd ../../
#
echo 
echo "pandoc  ${file1}.md  ${file2}.md  -o ${file1}.pdf"
echo 
#
pandoc  ${file1}.md  ${file2}.md \
        --metadata-file="${WD}/meta-data-${meta}.md.yaml" \
        --lua-filter="${WD}/remove_relative_links.lua" \
        -H "${WD}/formatting.tex" \
        --pdf-engine=lualatex  \
        --toc  \
        -f gfm \
        -t pdf \
        --standalone \
        --self-contained \
        --variable=linkcolor:blue \
        --variable=geometry:letterpaper \
        --variable=geometry:margin=1in \
        -o ${file1}.pdf
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