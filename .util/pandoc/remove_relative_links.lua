--[[ 
    Filter prevents pandoc from automatic links to specific files
       Otherwise it will automake the entire path
       For example assume the link is just CHANGELOG.md, 
       then it would auto-file the link as:
                                           file:///C|/dir1/dir2/dir3/CHANGELOG.md
                                           
       Instead it will italicize the word and remove the hyperlink
--]]


function Link(el)
    if string.sub(el.target, 1, 1) == "#" or string.lower(string.sub(el.target, 1, 4)) == "http" then
       -- Pass
    elseif string.sub(el.target, 1, 13) == "code.usgs.gov" then
       -- Pass
    elseif string.sub(el.target, 1, 7) == "mailto:" then
       -- Pass
    else
       -- Destroy "link" object type and 
       -- instead make the original text, el.content, an italic object type
       el = pandoc.Emph(el.content)
    end 
    return el
end

