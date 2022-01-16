# FMP_Template

This directory contains the Farm Process version 4 input template.  
The actual template is `FMP_Template.fmp`  
and `FMP_Template.fmp.html` is provided to view 
the template in a web-browser with syntax highlighting.

------

A comment in this file is preceded by a `#`, such as  
```
   Input Part    # Commented Part
```
and is grayed out if you enable syntax highlighting.

------

If you want to see `FMP_Template.fmp` with syntax highlighting,  
please review the files in `doc/Notepadpp_Syntax_Highlighting`.  
This explains how to setup the Notepad++ text editor for 
syntax highlighting for most the MODFLOW packages.

------

The FMP template file <u>contains all the supported keywords</u> and 
comments to explain their function and expected input.

There are also comments at the start of the template 
that explains how the read utilities work. 

<u>Note that nearly all the keywords are optional</u>, 
so don't feel overwhelmed. 

Just skim through the file and 
if the keyword does not make sense 
or apply to your modeling situation, 
then do not include in your input file. 

FMP checks for the presence of a keyword, 
and when it is not present,  
then it is set to the default value 
or the option is not enabled.

For example, in the extreme case,  
this is an FMP input with nearly all the keywords and blocks removed.  
This input is very primitive and only simulates groundwater evaporation  
if the water table is within 0.25 L of the upper most active cell.   
(L generic for whatever the length unit is defined for the model.)

```
BEGIN GLOBAL DIMENSION
   #
   NWBS        1
   NCROP       0
   NSOIL       1
   #
   NIRRIGATE   0
   NRD_TYPES   0
   NSFR_DELIV  0
   NSFR_RETURN 0
END
#
BEGIN WATER_BALANCE_SUBREGION
   #
   LOCATION  CONSTANT 1
END
#
BEGIN SOIL
   #
   CAPILLARY_FRINGE CONSTANT 0.25
   #
   SOIL_ID  CONSTANT 1
END

```
