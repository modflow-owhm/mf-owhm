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
cd ../../
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
        --self-contained \
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
#---- End of Scipt  ----------------------------------------------------------------------
#
echo
read -p "Repo Now Clean. Press [ENTER] to end script ... "
#
#
#
#
#
# --> Original Command --------------------------------------------------------------------
#
# pandoc  README.md  \
#         --metadata-file=meta-data-readme.md.yaml \
#         -H formatting.tex \
#         --pdf-engine=lualatex  \
#         --toc  \
#         -f gfm \
#         -t pdf \
#         --standalone \
#         --self-contained \
#         --variable=linkcolor:blue \
#         --variable=geometry:letterpaper \
#         --variable=geometry:margin=1in \
#         --variable=fontsize:20pt \
#          --lua-filter remove_relative_links.lua \
#         -o README.pdf