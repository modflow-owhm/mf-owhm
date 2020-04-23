
---------------------------------------------------------------------------------------------------

All Notepad++ syntax highlighting was developed for MF-OWHM, except for:

VisualStudioProjectNPPStyler.xml
VisualStudioSolutionSLNstyler.xml

which were obtained from the public source repository:

https://gist.github.com/CADbloke

which is maintained by 

Ewen Wallace (CADbloke)
Newcastle, NSW, Australia 
https://au.linkedin.com/in/cadbloke

---------------------------------------------------------------------------------------------------

These files were modified to include the extensions "vfproj vcxproj vcproj"
by changing the following information:

<UserLang name="VS Projects" ext="csproj vbproj vstemplate targets proj" udlVersion="2.1">

to

<UserLang name="VS Projects" ext="vfproj vcxproj vcproj csproj vbproj vstemplate targets proj" udlVersion="2.1">

---------------------------------------------------------------------------------------------------

Supplemental json description:

{
	{
		"id-name": "VisualStudioProjectNPPStyler",
		"display-name": "Visual Studio Project Files Syntax Highlighting with Notepad++",
		"repository": "https://gist.githubusercontent.com/CADbloke/7478607/raw/8fc2739ffaf3de97be915fc1562a7338402e5c15/VisualStudioProjectNPPStyler.xml",
		"description": "Visual Studio Project Files Syntax Highlighting with Notepad++",
		"author": "CADbloke",
		"homepage": "https://gist.github.com/CADbloke/7478607"
	},
	{
		"id-name": "VisualStudioSolutionSLNstyler",
		"display-name": "Visual Studio Solution (.sln) Files Syntax Highlighting with Notepad++",
		"repository": "https://gist.githubusercontent.com/CADbloke/5493663/raw/fc217e40f7a451bc3d679196d0111f61786c775f/VisualStudioSolutionSLNstyler.xml",
		"description": "Visual Studio Solution (.sln) Files Syntax Highlighting with Notepad++",
		"author": "CADbloke",
		"homepage": "https://gist.github.com/CADbloke/5493663"
	},
}
