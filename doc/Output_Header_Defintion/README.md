# Output File Header Definition

This folder contains a set of folders that contain text files that 
describe common MODFLOW-OWHM output files. The description includes a basic summary, 
followed by defining the text output file header, the binary file structure (if supported), 
and any additional helpful notes.

Folders are named after the package that the output file is specified in 
(such as, BAS for Basic Package and FMP for Farm Process). In each folder 
there are text files that are named after the input option that created the file. 
The file names separate the description of where the file is created by using 
a period (dot). The general structure of the file name is:

`input_block.keyword.default_name.txt` 

where:  
`input_block` is the input block that specified the keyword,   
`keyword`  is the keyword that created the output file or the input variable name that declared it,  
`default_name`, the default name of the file if one is not specified.

Depending on the output file, not all the descriptive parts may be included. 
For example, if an output file is not specified as part of an input block 
and does not have a default name, then `input_block` and `default_name` 
are not included in the file name. Also, if the output is declared by 
multiple, space delimited keywords, only the most descriptive word is used. 
For example, the non-descriptive parts of a multi-word keyword will 
only include the descriptive keywords.

This following is a simplified FMP input file (only the output parts) 
an example from the FMP folder that contains all the parts:

```
BEGIN OUTPUT
   FARM_DEMAND_SUPPLY_SUMMARY  ./output/fmp_summary.txt  # if no filename is specified, the default is "fds.out"
END OUTPUT

BEGIN SURFACE_WATER
   PRINT SFR_RETURN  ./output/fmp_sfr_return_flow.txt  # option has no default name, so file name must be specified.
END SURFACE_WATER
```

which would have the following file names in the FMP folder:

`output.farm_demand_supply_summary.fds.out.txt`

`surface_water.sfr_return.txt`

Note that `PRINT` is not included in the name because it is 
a non-description keyword that is space separated. 
In contrast, the following is an option block from the BAS package:

```
BEGIN OPTIONS
   PRINT_ITERATION_INFO  ./output/iter_info.txt   # No default name
END OPTIONS
```

which would have the following file names in the BAS folder:

`options.print_iteration_info.txt`

If the output option is prefaced by a `PRINT` keyword, then it is not 
included in the name. For example, the `LAND_USE` block keyword `ByWBS`, 
and does not have a default output name, will have a file name `land_use.bywbs.txt`.

Note a lot of the option block keywords are defined in the folder 
`doc/Option_Block_Cheatsheets` and a full listing of the FMP supported keywords, 
their definitions, and the input block they should be specified in are in the folder `doc/FMP_Template`.

Also, note that many of these output files are also defined in Appendix 1, 3, and 6 of

> Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

However, the files here represent the current ordering and definitions applied to the files.
