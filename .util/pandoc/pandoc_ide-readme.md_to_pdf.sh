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
file=README
#
meta=`echo "$file" | tr '[:upper:]' '[:lower:]'`
#
# Location of ${file}.md
cd ../../ide
#
echo 
echo "pandoc  ${file}.md  ${file}.pdf"
echo 
#
#
pandoc  ${file}.md  \
        --metadata=title:"MF-OWHM - ide/README.md" \
        --metadata=subtitle:"from https://code.usgs.gov/modflow/mf-owhm" \
        --metadata=abstract:"File ide/README.md converted to pdf using pandoc and LuaLaTex pdf-engine and package management with MiKTeX. For an accurate representation of the document, please view the original markdown file." \
        --metadata=author:"Scott E. Boyce <seboye@usgs.gov>" \
        --metadata=lang:"en-US" \
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