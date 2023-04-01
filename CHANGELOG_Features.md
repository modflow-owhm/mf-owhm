# Changelog - Features

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

&nbsp; 

------

[[_TOC_]]

------

&nbsp;

## 2.3.0

2023-4-23

### `HYD` supports the `SWT` package

The HydMod package provides time series observation output for various packages (`SFR`, `STR`, `SUB`, `IBS`, and `BAS`). HydMod package now supports observations from the Subsidence Water Table (`SWT`) package.

The input for HydMod remains the same, but Data Set 2 now supports the following new options:

`PCKG ARR INTYP KLAY XL YL HYDLBL  # Data Set 2`

New Options:

Set `PCKG` = `"SWT"` to use the `SWT` package

and `ARR` supports the following `CHARACTER(len=2)` options:

```
  "SB" -> subsidence from KLAY to lay 1
  "CP" -> total compaction        for layer KLAY
  "HC" -> preconsolidation stress for layer KLAY
  "GS" -> geostatic stress        for layer KLAY
  "ES" -> effective stress        for layer KLAY
  "VR" -> void ratio              for layer KLAY
```

### `HYD` added `DOUBLE_PRECISION_BINARY` option

The HydMod package now supports an optional keyword `DOUBLE_PRECISION_BINARY`. If this keyword is present, then Hydmod writes all variables to the unformatted binary file using double precision (14 digits) instead of single precision (7 digits). This keyword is specified at the start of the input file before the regular input. The following is the new input structure:

```
[DOUBLE_PRECISION_BINARY]  # Optional Keyword
NHYD IHYDUN HYDNOH
PCKG ARR INTYP KLAY XL YL HYDLBL # Repeat as needed
```

The following is an input example without the keyword:

```
3  70  -999                    # NHYD IHYDUN HYDNOH
BAS  HD  I  1  1.  1.  Label1  # PCKG ARR INTYP KLAY XL YL HYDLBL 
BAS  HD  I  1  3.  2.  Label2     
BAS  HD  I  1  5.  2.  Label3
```

and with the option enabled:

```
DOUBLE_PRECISION_BINARY
3  70  -999                    # NHYD IHYDUN HYDNOH
BAS  HD  I  1  1.  1.  Label1  # PCKG ARR INTYP KLAY XL YL HYDLBL 
BAS  HD  I  1  3.  2.  Label2     
BAS  HD  I  1  5.  2.  Label3
```

### `LPF` and `UPW` Improved #Comment Support

* Expanded comment support in the package input. If `LPF`/`UPW` properties are defined with parameters, then the packages used list-directed reads for the print factor `IPRN`. This caused problems if there are empty lines or commented lines between reading different `IPRN` values. Also added comment support when reading the `WET`ting parameters.

### `BAS`, `FMP`, `MNW2` Output File Header Description

* `doc/Output_Header_Defintion/*` 
* Describes  `BAS`, `FMP`, and `MNW2` common output files and defines the file headers.
* See [doc/Output_Header_Defintion/README.md](doc/Output_Header_Defintion/README.md) for a description of how the files are named and formatted.

### `GHB` Block Input Cheat Sheet – `GHB_Options_All.ghb`

* The file `doc/Option_Block_Cheatsheets/GHB_Options_All.ghb` is added to help users develop the GHB package.  
  This file includes documentation for:
  * `OPTIONS` block
  * `BudgetGroups` block
  * `LineFeed` Alternative Input

### `FMP` Improvements

* `SUPPLY_WELL` Block new output option: `PRINT ByWBS_ByLAYER`
  * This output option writes for each Water Balance Subregion (WBS) the total pumpage in and out of groundwater from each model layer from FMP Supply Wells, FMP Linked to MNW2 Wells, Non-Lined MNW2 Wells, and Non-Lined Well Package wells.
  * Note that FMP supply wells are any well linked to the WBS, consequently they may NOT be within the WBS area.  
    The non-FMP wells are those located within the WBS area.

* `SUPPLY_WELL` Block ignores row and column read in for supply wells that are linked to `MNW2`. Their input is still required, but is not used. Instead the row and column are copied over from the `MNW2` input.
* `SUPPLY_WELL` Block now checks if non-MNW2 linked wells are within model grid and raises an error if a well is not.
* Improved reading in the spatial location of the Water Balance Subregion (WBS). This input is read in the`WATER_BALANCE_SUBREGION` Block with the the keyword `LOCATION`, as an integer array, with a value of `0` to indicate no WBS and non-zero to indicate the WBS that is associated with that (row, col) loocation. If the number specified in the input is greater than the total number (`>NWBS`) or less than `0`, then the WBS number is changed to `0` and the model cell location is ignored. 
* `CLIMATE` Block `DIRECT_RECHARGE` keyword can be specified multiple times. The sum of the recharge arrays are applied to deep percolations.  
  For example:  
  `DIRECT_RECHARGE  FLUX  STATIC ARRAY OPEN/CLOSE rch1.txt`  
  `DIRECT_RECHARGE  FLUX  STATIC ARRAY OPEN/CLOSE rch2.txt`  
  `DIRECT_RECHARGE  RATE  STATIC ARRAY OPEN/CLOSE rch3.txt`  
  would apply to deep percolation the sum from the arrays read in from rch1.txt, rch2.txt, and rch3.txt
* Part of runoff can be defined to leave the model.
  * Runoff that is generated from FMP either flows to SFR or leaves the model as lost runoff. The SFR locations are either defined explicitly as a semi-routed return (`SRR`) points or automatically as fully-routed return (`FRR`). `FRR` just searched for any SFR segments/reaches that reside in a Water Balance Subregion (WBS) and set then as `SRR` locations.
  * The code was updated to allow defining `SRR`  locations with the segment = 0 to indicate that runoff is supposed to leave the model. There is no limit for how many `SRR` locations that remove water from the model, but it is recommended to only have one per WBS.
  * For example,  given that `NSFR_RETURN` = 2 and the user wants WBS 4 to have 60% of the runoff to go to segment 5, reach 1 and 40% of the runoff to leave the model domain. The input would have the following format:  
    `SEMI_ROUTED_RETURN  STATIC  LIST  INTERNAL`  
    `# ISRR, WBS_ID, SEGMENT, REACH, [FRAC]`  
    `     1  4       5        1      0.6   # 60% runoff applied to Seg 5, Reach 1`  
    `     2  4       0        0      0.4   # 40% runoff removed from model domain` 

* `GLOBAL DIMENSION` block can now set `NWBS` to `0` and not include the `WATER_BALANCE_SUBREGION`. Internally, this just sets `NWBS` to `1` and assumes the entire model grid is associated with WBS 1.  
  That is:  
  `BEGIN GLOBAL DIMENSION `  
  `  NWBS  0 `  
  `END`  
  becomes internally to FMP:  
  `BEGIN GLOBAL DIMENSION `  
  `  NWBS  1 `  
  `END`  
  `BEGIN WATER_BALANCE_SUBREGION `  
  `  LOCATION STATIC LIST CONSTANT 1 `  
  `END`

### `HOB` Improvements

* Drawdown observations must be in chronological order or an error is raised. Either the user will need to fix the order or change the observation order or change to head observations.

* Head observations that are not within the simulation are set to a null value.
  * Set to the `NOT_OBSERVED_VALUE`, if it is specified, otherwise set to `HOBDRY`.
  * Allows simulation to continue with an HOB file that includes observations that are not within the simulation timeframe.
    * Typically occurs if you want to run the model with feature stress periods than it was designed for, or when using the `FASTFORWARD` option.

* Drawdown observations that have the first head observation not within the simulation have all the observations set to the `HOBDRY` value.
* The HOB supports keywords that are specified at the start o the input file, one per line and in any order.  
  Keywords supported:
  * `TIME_STEP_PRINT`  *Generic_Output*
    * Writes head observations to file when they are simulated.
    * The order of the observations is the order they appear in the simulation.

  * `TIME_STEP_PRINT_ALL`  *Generic_Output*
    * Writes all head observations to file at the end of each time step.
      * The file is replaced after each time step.

    * This is useful to obtain the head observations when the simulation crashes.

  * `OBSNAM_LENGTH` *ilen*
    * Specifies the character length, *ilen*, of observation names, `OBSNAM`.
      * `CHARACTER( len=ilen ):: OBSNAM`

    * If not specified, the default size is 12 characters.
      * `CHARACTER( len=12 ):: OBSNAM`

  * `NOT_OBSERVED_VALUE` *value*
    * Value to set observation points that are not within the simulation timeframe or have yet to be simulated.
    * If not specified, then they ware set to `HOBDRY`

* It is recommended for simulations to use the keywords `TIME_STEP_PRINT_ALL`  and `NOT_OBSERVED_VALUE`.

**Keyword Input Example:**

```
#
# HOB input file for MODFLOW-OWHM v2
#
TIME_STEP_PRINT_ALL  ./output/head_obs.txt 
NOT_OBSERVED_VALUE  -8888.   # Value if obs point is never observed
OBSNAM_LENGTH          16    # Observation names will have a length of 16
168190 5310 12 0    -9999.   # NH, MOBS, MAXM, IUHOBSV, HOBDRY
```

**Example output:**  
Lets assume a model with 1 stress period and  4 time steps that are 10 days long.   
The `BAS` package has the option "`START_DATE 10/1/2000`" .  
The total simulation time is from 0.0 days to 40.0 days  
or from 10/01/2000 to 11/10/2000.

Lets assume the HOB defines the observation location `XYZ`, that measures the following simulation times:

```
# Starting Date:   10/01/2000  (note this date is at 00:00:00, aka midnight)
# Starting SimTim: 0.0
#
# OBSNAM  SimTime    Equivalent_Date
  XYZ_1     3.    #  10/04/2000
  XYZ_2     5.    #  10/06/2000
  XYZ_3     18.   #  10/19/2000
  XYZ_4     32.   #  11/02/2000
  XYZ_5     35.   #  11/05/2000
  XYZ_6     38.   #  11/08/2000
  XYZ_7     100.  #  01/09/2001
```

The observation names will be printed with a length of 16, such as:
`"XYZ_1           "`

After time step 1 (simulation time from 0.0 to 10.0), the the file `./output/head_obs.txt` will contain:

```
"SIMULATED EQUIVALENT"   "OBSERVED VALUE"    "OBSERVATION NAME"      DATE    DECIMAL_YEAR
   8.62315256343E-01   8.90000000000E-01       XYZ_1            2000-10-04     2000.756831
   1.21231151577E+00   1.31000000000E+00       XYZ_2            2000-10-06     2000.762295
  -8.88800000000E+03   3.10000000000E-01       XYZ_3            2000-10-19     2000.797814
  -8.88800000000E+03   1.22000000000E+00       XYZ_4            2000-11-02     2000.836066
  -8.88800000000E+03   1.16000000000E+00       XYZ_5            2000-11-05     2000.844262
  -8.88800000000E+03   1.13000000000E+00       XYZ_6            2000-11-08     2000.852459
  -8.88800000000E+03   1.50000000000E+00       XYZ_7            2001-01-09     2001.021918
```

After time step 2 (simulation time from 10.0 to 20.0), the the file `./output/head_obs.txt` will contain:

```
"SIMULATED EQUIVALENT"   "OBSERVED VALUE"    "OBSERVATION NAME"      DATE    DECIMAL_YEAR
   8.62315256343E-01   8.90000000000E-01       XYZ_1            2000-10-04     2000.756831
   1.21231151577E+00   1.31000000000E+00       XYZ_2            2000-10-06     2000.762295
   4.13250847373E+03   3.10000000000E-01       XYZ_3            2000-10-19     2000.797814
  -8.88800000000E+03   1.22000000000E+00       XYZ_4            2000-11-02     2000.836066
  -8.88800000000E+03   1.16000000000E+00       XYZ_5            2000-11-05     2000.844262
  -8.88800000000E+03   1.13000000000E+00       XYZ_6            2000-11-08     2000.852459
  -8.88800000000E+03   1.50000000000E+00       XYZ_7            2001-01-09     2001.021918
```

After time step 3 (simulation time from 20.0 to 30.0), the the file `./output/head_obs.txt` will contain (same as Time Step 2):

```
"SIMULATED EQUIVALENT"   "OBSERVED VALUE"    "OBSERVATION NAME"      DATE    DECIMAL_YEAR
   8.62315256343E-01   8.90000000000E-01       XYZ_1            2000-10-04     2000.756831
   1.21231151577E+00   1.31000000000E+00       XYZ_2            2000-10-06     2000.762295
   4.13250847373E+03   3.10000000000E-01       XYZ_3            2000-10-19     2000.797814
  -8.88800000000E+03   1.22000000000E+00       XYZ_4            2000-11-02     2000.836066
  -8.88800000000E+03   1.16000000000E+00       XYZ_5            2000-11-05     2000.844262
  -8.88800000000E+03   1.13000000000E+00       XYZ_6            2000-11-08     2000.852459
  -8.88800000000E+03   1.50000000000E+00       XYZ_7            2001-01-09     2001.021918
```

After time step 4 (simulation time from 30.0 to 40.0), the the **final version of the file** `./output/head_obs.txt` will contain:

```
"SIMULATED EQUIVALENT"   "OBSERVED VALUE"    "OBSERVATION NAME"      DATE    DECIMAL_YEAR
   8.62315256343E-01   8.90000000000E-01       XYZ_1            2000-10-04     2000.756831
   1.21231151577E+00   1.31000000000E+00       XYZ_2            2000-10-06     2000.762295
   4.13250847373E+03   3.10000000000E-01       XYZ_3            2000-10-19     2000.797814
   1.28547769725E+03   1.22000000000E+00       XYZ_4            2000-11-02     2000.836066
   1.22985850204E+03   1.16000000000E+00       XYZ_5            2000-11-05     2000.844262
   1.21408626843E+03   1.13000000000E+00       XYZ_6            2000-11-08     2000.852459
  -8.88800000000E+03   1.50000000000E+00       XYZ_7            2001-01-09     2001.021918
```

Observation point `XYZ_7` is always set to the `NO_OBS_VALUE` because it is beyond the simulation.

### `ZoneBudget` Improved Zone Number Output Formatting

* Changed text output from standard Fortran formatting to MODFLOW-OWHM `INT2STR` function for writing the zone numbers to the command prompt and the ZoneBudget output file.  
  This prevents truncation of zone numbers to `****` if they exceed `999`.   
  Now any positive `int32` zone number will work.

### `Tabfile` Improved Open File Keyword Support

* The `Tabfile` module previously would previously read a set of filenames to open or use the keyword `EXTERNAL` read a *unit* number. However if the user used another keyword, such as `OPEN/CLOSE`, the program would stop and not raise an ambiguous error message.
* The `Tabfile` code can now just read the tabfile files as:  
  `EXTERAL` *unit*  
  *filename*  
  `OPEN/CLOSE` *filename*  
  `DATAFILE` *filename*

### Misc. Options Added

* The `SFR` package added the option `SEGOUTPUT` that is followed by a unit number to write segment flow output to.
  * That is the input structure is:  
    `SEGOUTPUT  iu`  
    where `iu` is a unit number declared in the name file as:  
    `DATA  iu filename`
* The `UZF` package added the option `SAVEFINF`.

&nbsp; 

------

## 2.2.0

2022-01-20

### ZoneBudget version 3.2

  * Added historical publications to `postprocessors/zonebudget/doc`
    * [`postprocessors/zonebudget/doc/zonebudget_v3.2_input_description.md`](postprocessors/zonebudget/doc/zonebudget_v3.2_input_description.md) or  
      `postprocessors/zonebudget/doc/zonebudget_v3.2_input_description.pdf`  
      for the zonebudget input structure that includes new options.
    * Added an `OPEN/CLOSE` array control record.
    * Input can include blank lines and `# Comments`
    * Improved error messages.
    * Adds support for `BAS` option `CONSTANT_HEAD_BUDGET_OPTIONAL`

### `NWT` Improvements  

- `NWT` considers the MODFLOW Outer Iteration (`1` to `mxiter`) to be each time it solves for the Jacobian, and does not include iterations solved with Backtracking (residual control). This causes a disconnect with the other MODFLOW packages that assume an outer iteration occurs every time the aquifer flow equations are formulated (package's  `FM` routines). To fix this issue, the number of MODFLOW Outer Iterations is always accounted for as the number of times the `FM` routines are called.  
  
    Due to this, previous models may need to increase their maximum number of `NWT` iterations (`MAXITEROUT`) to allow for convergence. Previously, the number of outer iterations was under-reported such that a model that said it converged in `XX` iterations, really converged in `YY` iterations, where `YY >> XX`.  
    
    Note – Backtracking is enabled for the `OPTIONS` keyword is `MODERATE`, `COMPLEX`, or `SPECIFIED` with `BACKFLAG=1`.

* `NWT` now imposes conditional minimum `mxiter` iteration limit.  
    This input for NWT is called `MAXITEROUT` and the imposed limit is based on the `OPTIONS` keyword:
    
    * `SIMPLE:    if mxiter < 200; then mxiter = 200`
    * `MODERATE:  if mxiter < 300; then mxiter = 500`
    * `COMPLEX:   if mxiter < 500; then mxiter = 500`
    * `SPECIFIED: if mxiter < 500; then mxiter = 500`  
       &nbsp; 
    
    To disable this min-max limit, include the option `KEEP_MXITER`

### `NWT` new options

* `KEEP_MXITER`
    * Does not allow NWT to increase `MAXITEROUT` if it is less then the minimum allowed.

- `MAX_HEAD_CHANGE`  **FLOAT**
    - Specify a maximum head change allowed per solver iteration.

* `HEAD_DISTANCE_ABOVE_GSE_LIMIT` *ULOAD(NROW, NCOL)*
    * Specify a maximum distance above the land surface elevation (ground surface elevation, `GSE`) that any model cell's head can be. If the head exceeds this threshold, then it is set to the threshold.
    * Input uses ULOAD to read in a 2D array of maximum distances allowed above the `GSE`. 
    * The `GSE` is either set to the top elevation of the upper most active model cells (typically layer 1) or is defined in the `DIS` or `FMP` packages with the keyword `SURFACE_ELEVATION`.
        * For documentation on how ULOAD works, please review [Boyce and others, 2020](https://pubs.er.usgs.gov/publication/tm6A60) Appendix 1.

- `THIN_CELL_CHECK`
    - Enables NWT thin cell check that removes any model cells that have a vertical thickness less than 1% of the thickest model cell. See in Changelog.md under "`NWT` thin cell check is disabled" for a full description why this feature was made an option rather than the default.

* `NWT` options can be defined at the start of the input file, one per line, and specified in any order.
  *  If the keyword `SPECIFIED` is defined at the start of the input file, that same line must include its required input. That is, you must have on the same line:  
      `SPECIFIED  DBDTHETA  DBDGAMMA  MOMFACT  BACKFLAG  [MAXBACKITER  BACKTOL  BACKREDUCE]`

### `DIS` — Keywords `DAILY` and `MONTHLY` — Automatic Stress Period Setup

* The standard Discretization Package (`DIS`) input defines `NPER` stress period simulation lengths (`PERLEN`) and the number of time steps (`NSTP`) in the stress period. This input is defined as Data Set 7 and is as follows:  
  `PERLEN NSTP TSMULT Ss/tr `  
  which is read `NPER` times and must include `Ss` or `Tr` to indicate if the stress period is solved using the Steady State or Transient solution.

* Instead of reading in `NPER` stress period lengths, a single keyword can represent all stress period lengths and number of time steps. This then requires only specifying once a keyword, which indicates how all stress periods are handled. 

* The two keywords that are supported are:

  * `DAILY`, to indicate that all stress periods have `PERLEN` set equal to 1 day.
    * Note, depending on `ITMUNI` the time units are adjusted to:  
      86400 seconds, 1440 minutes, or 24 hours, but if set to unknown or years will raise an error.

  * `MONTHLY`, to indicate that all stress periods have `PERLEN` set equal to number of days in the month. 
    * This option requires that a simulation starting date is specified. This can be done with the `BAS` package OPTIONS keyword `START_DATE` or by specifying `DATE` along with the `MONTHLY` keyword. If the starting date does not fall on the first of the month, then the first stress period length is set such that the second stress period is on the first. For example, if the starting date is `1/25/2022`, then the first stress period will have 7 days to make the second stress period start on February 1.
    * This option does take into account leap years, where the number of days in February depends on the year.

* The `DAILY` keyword full input is as follows:

  *  `DAILY    NSTP   [Ss]`   
    where:  
    `NSTP` is the number of time steps to subdivide the daily stress period (1 is recommended),  
    `SS` is an optional keyword that indicates that the <u>first stress period</u> is solved using Steady State and the remaining stress periods are solved as Transient. If not present, than all stress periods are solved using Transient.

* The `MONTHLY` keyword full input is as follows:

  * `MONTHLY   NSTP   [Ss]   [DATE]  `  
    where  

    `NSTP` is the number of time steps to subdivide the daily stress period.  
    If it is set to a negative number, then the absolute value is the number of time steps and  
    MF-OWHM attempts to make the time step lengths natural numbers in increasing order.  

    For example, if a model has time units of `DAYS` and  
    the first stress period is March (31 days) and  
    second stress period is April (30 days) then:  
     `NSTP = -3`  has the March time step lengths `= [10, 10, 11]` and April time step lengths `= [10, 10, 10]`  
     `NSTP = -4`  has the March time step lengths `= [7, 7, 7, 8]` and April time step lengths `= [7, 7, 8, 8]`  
     `NSTP = -5`  has the March time step lengths `= [6, 6, 6, 6, 7]` and April time step lengths `= [6, 6, 6, 6, 6]`  
    `SS` is an optional keyword that indicates that the <u>first stress period</u> is solved using Steady State and the remaining stress periods are solved as Transient. If not present, than all stress periods are solved using Transient.  
    `DATE` is only required if the BAS package does not include the `START_DATE` option keyword. 

The following is a simplified example DIS file:
```
# NLAY, NROW, NCOL, NPER, ITMUNI, LENUNI
  2     5     4     120   DAYS    METERS   # ITMUNI and LENUNI can be defined as keywords or integers
NO_LAYCBD               # Keyword indicates no quasi-confining layers
CONSTANT     100        # DELR - rows are 100 meters
CONSTANT     100        # DELC - columns are 100 meters
CONSTANT     100        # TOP elevation of Lay1
CONSTANT     50         # BOTM, Lay1
CONSTANT     0          # BOTM, Lay2
MONTHLY  -4   SS        # MONTHLY input with 4 time steps, and first stress period is steady state
                        # START_DATE must be defined in BAS options because it is not defined here
```

### `BAS` — Options Block — Making the `OC` package optional

* `CBC_UNIT` **INT**
    * Defines a global cell-by-cell unit (**INT**) and overwrites each package's `IxxxCB` input variable (for example, `IWELCB`).

- `COMPACT BUDGET`
    - Indicates the cell-by-cell uses a compact/smaller structure. Same effect as specifying keyword in the `OC` package

* `SAVE_HEAD  LAST_TIMESTEP   Generic_Output  [BINARY]  [SIGFIG  NDIG]`  
  `SAVE_HEAD  EVERY_TIMESTEP  Generic_Output  [BINARY]  [SIGFIG  NDIG]`
  * Save entire model grid head value using the MODFLOW-2005 standard write utility.  
    This produces an equivalent output to `OUTPUT CONTROL` (`OC`) options `SAVE HEAD` and `PRINT HEAD`  
    Best for writing to the cell-by-cell (CBC) file declared in the `NAME` file as `DATA(BINARY)`  
    Can only specify once, with one of the following two keywords:
      * `LAST_TIMESTEP` indicates to write the head for the last time step of every stress period
      * `EVERY_TIMESTEP` indicates to write the head for every time step  
          &nbsp; 
    
  - `Generic_Output` is the location to write the output head.
    - If this represents the CBC, then the file needs to be binary formatted.  
      This is either done by specifying in the Name file a binary input:  
      `DATA(binary)   40     cbc.bin`  
      and then replacing generic input with  
      `SAVE_HEAD  LAST_TIMESTEP   EXTERNAL 40`  
      
      or by not including it in the Name file and creating it with `Generic_Output`,   
      but you must include the post-keyword `BINARY`  
      `SAVE_HEAD  LAST_TIMESTEP   OPEN/CLOSE  cbc.bin  BINARY`  
      
    - If the output is text-based (not binary), then the optional post-keyword `SIGFIG`  
      specifies the number of significant figure digits to write out (`NDIG`). For example:  
      `SAVE_HEAD  LAST_TIMESTEP   OPEN/CLOSE  heads.txt  SIGFIG 11`  
      will produce head output that contains `11` significant digits. If not specified, the default is `5` digits.

### `BAS` — Options Block — Improvements

* `CONSTANT_HEAD_BUDGET_OPTIONAL`
  * If present and there are no constant heads in the model (`IBOUND < 0`, `CHD`, or `FHB`), then the `'   CONSTANT HEAD'` array is not written to the CBC file. This reduces the CBC size by about 5%.
  * ZoneBudget `v3.2` or newer is required for a CBC made with this option,  
    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; which is part of MF-OWHM `v2.1.1` or newer.

- `PRINT_HEAD         SPTS  GENERIC_OUTPUT   [SIGFIG  NDIG]`  
     `PRINT_WATER_TABLE  SPTS   Generic_Output  [SIGFIG  NDIG]`  
     `PRINT_WATER_DEPTH  SPTS   Generic_Output  [SIGFIG  NDIG]`

     - Expands option added in [Version 2.0.2](#2.0.2) to include 
          the post-keyword `SIGFIG` specifies the number of significant figure digits to write out (`NDIG`).
          For example:  
          `PRINT_HEAD  5 2  ./output/Head_SP5_TS2.txt   SIGFIG  11`  
          `PRINT_HEAD  8 1  ./output/Head_SP8_TS1.txt   SIGFIG   9`  
          `PRINT_HEAD  3 2  ./output/Head_SP3_TS2.txt   SIGFIG  15`  
          which would have in the three files head output arrays with `11`, `9`, and `15` significant figures, respectively.

### `FMP` Improvements

* `SURFACE_WATER` block includes output files for `NON_ROUTED_DELIVERY` (NRD) imported water:
     * `PRINT  NRD         GENERIC_OUTPUT`
          * Output for each NRD in use during a time step that includes the DEMAND'ed water, the NRD's available water `SUPPLY`, and amount of NRD's water that is `CONSUMED`.
     * `PRINT  NRD_BY_WBS  GENERIC_OUTPUT`
          * Output summarized by Water Balance Subregion for each time step that includes the WBS water `DEMAND`, the NRD available water `SUPPLY`, and amount of NRD water that is `CONSUMED`.

### `SUB` — Options Block — Write Initial Critical Heads to separate files for all Interbeds

* `PRINT_INITIAL_CRITICAL_HEAD`&nbsp; &nbsp; &nbsp; *[OUTDIR]*
  * For all interbeds being simulated, delay and instantaneous, write the critical head at the start of a simulation. 
    * The files are placed in the *OUTDIR* directory.  
      If *OUTDIR* is not specified, then it is assumed to be `./` (current directory)
  * Each interbed's critical head is written as an `NROW` by `NCOL` array to separate files.
  * If the critical head is below the model cell's bottom, or not defined, then it is set to `-3.40E+38` to represent `-inf`
  * The filename structure is:  
    `DBED_CRIT_HEAD_LAYxx_BEDyy.txt` for delay interbeds and  
    `INST_CRIT_HEAD_LAYxx_BEDyy.txt` for instantaneous interbeds,
    where `xx` is replaced by the layer number, and `yy` is the delay or instantaneous interbed number.

### `HOB` can specify the max size of an observation name (obsnam).

* Include a the start of the input file the following  
  `OBSNAM_LENGTH`&nbsp; &nbsp; &nbsp; *maxlen*  
  where *maxlen* is replaced by the maximum size of an observation name.  
  Note, when not specified the default size is `12`.
  
* If there are other options specified at the start of the input, such as `TIME_STEP_PRINT`,  
  then only one option may be specified per line and the order they are specified does not matter.

### `LIST` writes solver convergence info after every timestep

* Independent of the solver in use
  
* The following are the convergence terms:
  
  * `HClose` 
       * Larges change in head between the last solver iteration and the previous
  * `RClose`
       * Largest model cell residual error  
         `r = Ah - RHS`  
         where  
         `r` is a vector that is the error residual, each row is a model cell,  
         `A` is the system matrix of the model,
         `h` is the head solution vector, each row is a model cell,
         `RHS` is the right hand side vector, which represents sources and sinks in the model and each row is a model cell.
  * `L2-RClose`
    *  L<sub>2</sub> norm of the residual errors, or equivalently: |`r`|<sub>2</sub>
  * `Rel-Vol-Err`
    * Largest relative volume error (`|r/vol|`<sub>∞</sub>)
    * `r` is the residual error vector, `vol`  is a vector of the model cell's volume. 

&nbsp; 

------

## 2.1.0

2021-05-25

### Surface Water Operations (`SWO`) incorporated to `FMP`

* Feature based on the following publication:
    * Ferguson, I.M.., Llewellyn, D., Hanson, R.T., and Boyce S.E., 2016,  
      User guide to the surface water operations process—An integrated approach to  
      simulating large-scale surface water management in MODFLOW-based hydrologic models:  
      Denver, Colo., Bureau of Reclamation Technical Memorandum no. 86-68210–2016-02, 96 p.
* Input is a `SURFACE_WATER_OPERATIONS` block in the FMP input file.

### S Interpretive Language for Customizable User Input (`slang`)

- Custom scripting langauge that can be used for dynmaic changes to MODFLOW-OWHM.
- Developed to enable custom reservoir opeations decision trees in MODFLOW.

### `U1DREL`, `U2DREL`, and `U2DINT` no longer require specifying `CNSTNT`, `FMTIN`, and `IPRN`

* If the values are not specified, then they are set to:  
  `CNSTNT = 1`  
  `FMTIN  = '(FREE)'`
  `IPRN   = -1`
* Read utility checks for end of line or comment symbol `#` to terminate reading input.
* The following are acceptable input options:  
  `OPEN/CLOSE ./input.txt                    # All set to default values`  
  `OPEN/CLOSE ./input.txt  1.0               # FMTIN and IPRN set to default`  
  `OPEN/CLOSE ./input.txt  1.0  '(FREE)'     # IPRN set to default`  
  `OPEN/CLOSE ./input.txt  1.0  '(FREE)'  -1 # Normal input structure`  
  `OPEN/CLOSE ./input.txt       '(FREE)'     # CNSTNT and IPRN set to default`  
  `OPEN/CLOSE ./input.txt       '(FREE)'  -1 # CNSTNT set to default`
* This is NOT allowed:  
  `OPEN/CLOSE ./input.txt  1.0            -1 # NOT ALLOWED - Specify only CNSTNT and IPRN`  
  `OPEN/CLOSE ./input.txt                 -1 # NOT ALLOWED - Specify only IPRN`  
* The directive `CONSTANT` still requires specifying `CNSTNT`,  
  That is, `CONSTANT CNSTNT` must specify the value that is made constant.

### `U1DREL` and `U2DREL` can specify keyword `SHIFT` **FLOAT** multiple times  

and can be used when `CNSTNT`, `FMTIN`, and `IPRN` are not specified.

  * For example, `OPEN/CLOSE ./IC.txt 1 (FREE) -1 SHIFT 15 SHIFT -5` would add 10 to the input read in `IC.txt`
  * For example, `OPEN/CLOSE ./IC.txt             SHIFT 15 SHIFT -5` would add 10 to the input read in `IC.txt`

&nbsp; 

------

## 2.0.3

2021-05-25

### `FMP` — `Land_Use` Block Output Option

* `PRINT  ET_ByWBS_ByCROP  GENERIC_OUTPUT `
    * Writes the total Reference ET (`ETref`), Potential ET (`ETpot`), and Actual ET (`ETact`) for each crop in each farm that is in use during the time step.
    * Note that in FMP that `ETact` is equivalent to Consumptive Use (CU), and `ETpot` is equivalent to `CU_INI`

&nbsp; 

------

## 2.0.2

2021-04-23

### `BAS` — Options Block

- `PRINT_HEAD         SPTS   Generic_Output `  
  `PRINT_WATER_TABLE  SPTS   Generic_Output `  
  `PRINT_WATER_DEPTH  SPTS   Generic_Output `
    - Prints the head, water table, or depth to water to a file. If head is requested then `NLAY` 2D (`NROW,NCOL`) head arrays are written with `NaN` as the placeholder for inactive or dry cells. Water table writes a single 2D array that specifies for each (`row, col`) the upper most active (saturated) cell's head. Water depth is the distance from either the upper most active cells top elevation to the water table or if the distance from the `SURFACE_ELEVATION` specified in the`DIS` or `FMP`.
    - `SPTS` indicates what time step to use for the output. It must either be a single date (eg `4/23/1979`), or a stress period and time step (eg `5 2`), can be set to `NPER` for the last stress period and time step, the keyword `LAST_TIMESTEP`  to indicate it should write the last time step of every stress period, or `EVERY_TIMESTEP` to write for every time step.
    - The keywords may be repeated multiple times to specify different times.  
      For example:  
      `PRINT_HEAD  5 2  ./output/Head_SP5_TS2.txt `  
      `PRINT_HEAD  8 1  ./output/Head_SP8_TS1.txt `  
      `PRINT_HEAD  3 2  ./output/Head_SP3_TS2.txt `  
      however you can only specify it once if using either `LAST_TIMESTEP`  or `EVERY_TIMESTEP`:  
      `PRINT_HEAD  LAST_TIMESTEP  ./output/Head_End_of_Each_SP.txt `

&nbsp; 

------

## 2.0.1

2021-03-15

### Output files will now automatically make missing directories if they do not exist rather than raising an error

- For example, in the if the following entry is in the Name file  
  `DATA 44  ./output/heads.out`  
  If the folder `output` does not exist, then it is created along with the file `heads.out`

### `U1DREL` and `U2DREL` support a post keyword `SHIFT` **FLOAT** that adds the specified float to the array that is read

* For example, `OPEN/CLOSE ./IC.txt 1 (FREE) -1 SHIFT 15` would add 15 to the input read in `IC.txt`

### **EXTERNAL** and **DATAUNIT** can reference a file's base name 

- Normal input is to follow these keywords with a unit number specified in the Name file.
- Now *GENERIC_Input* and *GENERIC_OUTPUT* check for a unit number and the `basename` of a file.  
  For example, if the Name file declare the following Data File  
  `DATA   23    ./Dir1/Dir2/MyFile.txt`  
  Then the file can be accessed by one of the two following commands (the first is the normal method):  
  `EXTERNAL 23`  
  `EXTERNAL MyFile.txt`
- This allows for file output/input locations to be defined at different locations in the name file, but enables packages to refer to he same root file name.

### `BAS` — Options Block

* `SHOWPROGRESS  [NPRT]`
    * Indicates that the command prompt should show during each time step a live iteration count (a number that auto-updates).  
      `NPRNT` is optional, and if present specifies the print interval and indicates the printing should include the iteration's mass error.  
      If not present, then the iteration count is printed every 10 iterations (that is 10, 20, 30, ...) and no mass printing occurs.
    * Note that `SHOWPROGRESS` is disabled if any stress period is solved in less than 12 seconds because the iteration counter appears as a blur from being updated too quickly.

### `DRT` Option

* `AUTOMATIC_NEGATIVE_ITMP`
    * Indicates that the first stress period input is read and then reused for the remainder of the simulation.

### `FMP` Updates

- Land Use Block has the keyword:
    - `PRINT ROW_COLUMN` **INT INT** *GENERIC_OUTPUT*
        - Prints detailed/verbose land use information at every time step for the specified Row and Column.
        - For example, `PRINT ROW_COLUMN  3  4  ./Row3_Col4.txt`
- Global Dimension has the keyword:
  
    - `HEAD_PREDICTOR_FACTOR` **FLOAT**
        - Keyword to indicate how the water table elevation is calculated. The water table elevation determines the amount of evaporation and transpiration from groundwater.
        - The default is to use the head solution (`HNEW`) at the end of the time step.
        - It may be more accurate to use a combination of the head at the start of the time step (`HOLD`) with `HNEW`.
        - `HEAD_PREDICTOR_FACTOR` specifies a weight (`FACT` as a float) for calculating the water table (`WT`):  
          `WT = FACT*HNEW + (1-FACT)*HOLD`
            - `FACT` should be set to `0.0` or `0.5` or `1.0` to indicate using only `HOLD`, the average, or only `HNEW`
            - If keyword is not specified, then, by default, `FACT = 1` which makes:  
              `WT = HNEW`

### `MNW2` Print Options

* The following can be added to the MNW2 `OPTIONS` block for additional output:
    * `PRINT_WELL_PUMPING`   &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;                      *GENERIC_OUTPUT*
    * `PRINT_NODE_INFO`      &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; *GENERIC_OUTPUT*
    * `PRINT_WELL_NODE_FLOW` &nbsp; &nbsp; &nbsp; &nbsp;                                    *GENERIC_OUTPUT*
    * `PRINT_WELL_INOUT`     &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;        *GENERIC_OUTPUT*

### `RCH` can set `NRCHOP` to `-1` 

* This indicates that the initial upper most non-zero `IBOUND` is used. This is a hybrid between `NRCHOP` as `1` and `3`.
* This option was created to mimic the original behavior of `RCH` with the `NWT` solver and  `NRCHOP=3`.  
  NWT does not zero the `IBOUND` value for dry cells resulting in recharge not being passed to the upper most non-dry cell.  
  See `Version 2.0.1` bug fixes for more details.

### `SFR` Option

- `HEAD_PREDICTOR_FACTOR` **FLOAT**
    - Same effect as described for FMP with the same keyword, except it affects how the water table elevation is calculated with respect to the SFR stream bottom and stream aquifer interaction.

### `UPW` Option — Data Set 1 "Options"

* `LPF_STORAGE`
    * Keyword indicates that UPW follows the storage formulation used in the LPF package.
    * Specifically, it assumes the derivative is zero, Specific Storage is zero for unconfined conditions, and Specific Yield does not vary with saturated thickness.

### `UPW` and `LPF` package — New Keywords as alternative to integer flags
- `LAYTYP` flag can specify:
    - `CONFINED`
        - All layers are set to `LAYTYPE = 0` (confined) and use fixed transmissivity and specific storage
    - `CONVERTIBLE`  
      `CONVERTIBLE` **INT**
        - All layers are set to `LAYTYP = 1` (convertible) and have variable transmissivity and specific yield.
        - If **INT** is specified, then it represents the deepest layer that is convertible and all layers above it are convertible.
        - For example, `CONVERTIBLE 3` indicates that Layers 1, 2, and 3 are convertible and 4 to `NLAY` are confined.
- `LAYAVE` flag can specify:
    - `HARMONIC` to automatically assign 0 for all layers (harmonic averaging)
    - `LOGARITHMIC` to automatically assign 1 for all layers (logarithmic averaging)
    - `ARITHMETIC` to automatically assign 2 for all layers (arithmetic averaging)
- `CHANI` flag can specify:
    - `NO HORIZONTAL ANISOTROPY` to automatically set for all layers `CHANI = 1.0`
  - `LAYVKA` flag can specify:
    - `VKA` to set all layers to `LAYVKA = 0` (`VKA` is specified as vertical hydraulic conductivity)
    - `RATIO` to set all layers to `LAYVKA = 1` (`VKA` is specified as the ratio of horizontal to vertical hydraulic conductivity)
- `LAYWET` flag can specify:
    - `NO WETTING`
        - Sets all layers to `LAYWET = 0`
    - `WETTING`  
      `WETTING` **INT**
        - All layers are set to `LAYWET = 1`
        - If **INT** is specified, then it represents the deepest layer that has `WETTING` and all layers above it that have it.
        - For example, `WETTING 3` indicates that Layers 1, 2, and 3 have enabled `WETTING` and 4 to `NLAY` do not.

&nbsp; 

------

## 2.0.0

2020-04-07

### Initial Release of MF-OWHMv2

For a full listing of features in this release see:
- Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60