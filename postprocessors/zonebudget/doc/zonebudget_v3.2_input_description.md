# ZoneBudget version 3.2

For background information please review:
- [Harbaugh, A.W., 1990, A computer program for calculating subregional water budgets using results from the U.S. Geological Survey modular three dimensional finite-difference ground-water-flow model: U.S. Geological Survey Open-File Report 90-392, 46 p.](ofr90392.pdf)
  - File: `ofr90392.pdf`

- [Version 3 Errata File: `zonbud3.pdf`](zonbud3.pdf)

## Input Description

Input may include blank spaces and comments that are preceded by a `#` symbol.  
Comments are not allowed between rows while reading `IZONE(NCOL,NROW)`.   
That is because the `IZONE` array is read uses `FMTIN` to read the entire array.

Input can be read using free format, `"(FREE)"`, which parses numbers by blanks spaces.  
Note, commas and tabs are treated as blank spaces, and multiple spaces are treated as one.  
So, parsing `2  4 ,  6  8`, will parse out the numbers as `2`, `4`, `6`, and `8`.

Examples input setups are available in `postprocessors/zonebudget/test/Zbtest`.  
To run the example test cases run the Windows Batch (`0_run_tests.bat`) or Linux Bash (`0_run_tests.sh`) shell scripts.  
The results from the example cases are written to `postprocessors/zonebudget/test/Zbtest-out`.  
The accepted, true, results are stored in `postprocessors/zonebudget/test/Zbtest-true`.  

The example test cases all process the same cell-by-cell file (`zbtest.bud`) that is included in the ZoneBudget `v3.01` example case. The rest of the cases are only different in how they read the input zone arrays.  
The following are the example test cases:

| Example Test Case                   | Answers To Command  Line          | Zone File That is Read  |
| :---------------------------------- | --------------------------------- | ----------------------- |
| ZoneBudget `v3.01` Example          | `Zone_CMD_Answers.txt`            | `zbtest.zon `           |
| `INTERNAL`                          | `Zone_CMD_Answers_INTERNAL1.txt`  | `Zbtest_INTERNAL1.zon`  |
| `INTERNAL` with `FMTIN` or `IPRN`   | `Zone_CMD_Answers_INTERNAL2.txt`  | `Zbtest_INTERNAL2.zon`  |
| `EXTERNAL`                          | `Zone_CMD_Answers_EXTERNAL1.txt`  | `Zbtest_EXTERNAL1.zon`  |
| `EXTERNAL` with `FMTIN` or `IPRN`   | `Zone_CMD_Answers_EXTERNAL2.txt`  | `Zbtest_EXTERNAL2.zon`  |
| `OPEN/CLOSE`                        | `Zone_CMD_Answers_OPENCLOSE1.txt` | `Zbtest_OPENCLOSE1.zon` |
| `OPEN/CLOSE` with `FMTIN` or `IPRN` | `Zone_CMD_Answers_OPENCLOSE2.txt` | `Zbtest_OPENCLOSE2.zon` |



### Input DataSets

For an in depth description of the input, please see the [zonbud3.pdf](zonbud3.pdf) and [ofr90392.pdf](ofr90392.pdf) 

The following input is provided for a quick review of the structure and options present in version 3.2 or newer.

The input reads three sets of *items*, which are also called datasets.  
The first dataset defines the model grid that should be in the cell-by-cell file.  
The second dataset reads the the integer zone arrays (`IZONE`) for each model layer.   
The third dataset, is optional, and defines composite zone groups to allow multiple  
`IZONE` integers to represent the same zone.

`NLAY     NROW      NCOL         ➣ DataSet 1`

`DataSet 2` is repeated `NLAY` times,  
and for each layer in input can be `2a`, `2b`, `2c`,  or `2d`

`CONSTANT  ICONST                ➣ DataSet 2a` 

`INTERNAL    FMTIN  IPRN         ➣ DataSet 2b`  
`IZONE(NCOL,NROW)                ➣ DataSet 2b`

`EXTERNAL    FMTIN  IPRN         ➣ DataSet 2C`  
`FileName                        ➣ DataSet 2c`

`OPEN/CLOSE  FMTIN  IPRN         ➣ DataSet 2d`  
`FileName                        ➣ DataSet 2d`

`DataSet 3` is read after `DataSet 2` is read `NLAY` times.  
`DataSet 3` may be repeated for as many composite zones as needed.  
If you do not use composite zones, then the rest of the input file must be comments or blank.

  `[NAMCOMP] ICOMP(50)             ➣ DataSet 3`

### Input DataSets Explanation

`NLAY`, `NROW`, and `NCOL` are the model's number of layers, rows, and columns, respectively.  
They must match the model grid in the processed cell-by-cell.

The array control record (input directive): `CONSTANT`, `INTERNAL`, `EXTERNAL`, and `OPEN/CLOSE`,  
which indicates where the zone array is located:

*  `CONSTANT`: reads `ICONST` and applies its value for the entire layer.

*  `INTERNAL`: indicates that the zone data, `IZONE(NCOL,NROW)`, for the layer is specified on the subsequent lines.

*  `EXTERNAL`: indicates that the zone data, `IZONE(NCOL,NROW)`,  is in  `FileName`.

     *  The file remains open until the program terminates.
          So, reading the same file twice requires the file to contain two zone arrays.
          The first `EXTERNAL` reads the first array in the file, then the second `EXTERNAL` reads the second array in the file.

*  `OPEN/CLOSE`: indicates that the zone data, `IZONE(NCOL,NROW)`,  is in  `FileName`.

     *  This closes the file once the data is read in, so if the file is read again it starts on the first line.  
        So, reading the same file twice will read the first array stored in it.

     *  DO NOT mix `EXTERNAL` with `OPEN/CLOSE` with the same `FileName` 

  * `FMTIN` is the Fortran read format specification.  
    This takes the form of a count of numbers specified per line with the number of columns the number occupies.  
    For example, a `FMTIN`  that is set to `“(20I4)”` indicates that there are 20 integers per line (record) and each integer occupies a field of 4 columns. A field of 4 columns means that the first four spaces (1-4) contains the first integer, the next four spaces (5-8) the next integer, and so forth until the 20th integer is read on spaces 77-80. 
    * If `FMTIN` = `'(FREE)'`, then the zone array is read using the MODFLOW free format option,  
      which is `NCOL` space-delimited numbers per line, and `NROW` line read.  
      ➣ Note that a comma `,` and tab `\t` are treated as blank spaces.  
  * `IPRN` is an integer flag that indicates if input zone array is printed in the listing file.  
    A value of zero or greater will print, negative will suppress printing.
  * `IZONE(NCOL,NROW)` is a `NROW` by `NCOL` integer zone array that is read for the layer.
  * `NAMCOMP`  is the name of the optional name of the composite zone. It must start with a letter.
  * `ICOMP(50) ` are a list of zone numbers that are associated with the composite zone.  
    The number `0` must be placed at the end of the list to terminate reading of the zone numbers.  
    For example, `2 4 6 0` would make the composite zone associated with zone numbers 2, 4, and 6.