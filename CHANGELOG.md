# Changelog

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

**[New Feature Changelog](CHANGELOG_Features.md)**

Boyce, S.E., 2023, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version X.Y.Z: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp; 

------

[[_TOC_]]

------

&nbsp;

## 2.3.0

2023-4-23

git commit log: `git log 9d9f5b50c77a03b538e4ec818f5a67e7bcf3e5ea..HEAD`

### HYDFMT v1.2

The update includes the MF-OWHM read utilities, new options, and fixed minor bugs.  
See [CHANGELOG_Features.md](CHANGELOG_Features.md#2.1.1) for a listing of new Hydmod and HYDFMT features.

### ZoneBudget v3.3

Update to ZoneBudget from `3.2` to `3.3`  
The update includes the MF-OWHM read utilities, new options, and fixed minor bugs.  
See [CHANGELOG_Features.md](CHANGELOG_Features.md#2.1.1) for a listing of new ZoneBudget features.

### Merge ⯬ MF-NWT v1.3

- `NWT` Solver version 1.3 source code merged
    - Obtained from: https://water.usgs.gov/water-resources/software/MODFLOW-NWT/MODFLOW-NWT_1.3.0.zip

### Merge ⯬ BiF v1.2.0

- Batteries Included Fortran (BiF) version 1.2.0 source code merged.
    - https://code.usgs.gov/fortran/bif/-/tags/1.2.0

### Fixed

* `FMP` Bug Fixes
  
    * Can use `FMP` without `SFR`.
      * Previously, if `FMP` is used without `SFR`, then an index error occurred when `FMP` attempted to check for runoff and delivery locations. To prevent this index error from occurring the user had to specify the `WATERSOURCE` keyword to indicate that `SFR` is not available. 
      * Now if `SFR` is not in use, then `FMP` automatically disables `SFR` diversions and runoff is flagged for flowing out of the model (rather than to a specific `SFR` location).  
    * `MNW2`-`FMP` Supply well link no longer crashes if row and column do not match (`SUPPLY_WELL` Block). Instead, `FMP` now copies the row and column specified in `MNW2`. The row and column input is still required in `FMP` as a placeholder, but not used.
    * `SURFACE_WATER` block `PRINT SFR_RETURN` keyword with binary output (`BINARY` keyword) wrote a random integer if the WBS did not have assigned any SFR return flow locations.
      * The fix changed this to write the segment and reach as zero to indicate no SFR return flow point was specified.
      * Note this is already how the text version of the option already works.
    * FMP failed to identify other blocks when `LAND_USE` was not specified in the input.
      * While this block should always be specified, the input indicates it is optional.  
        The code now reflects this flexibility.
    * Fixed errors when NOT including the `LAND_USE` block in the input and:
      * `CLIMATE` block specified `REFERENCE_ET` to result in bare soil evaporative fallback calculations (fixed allocation error).
      * Raise an error when `NCROP > 0`
    * Fixed errors with FMP `LAND_USE` block and Bare Soil:
      * Improved logic for bare soil evaporation when `NCROP = 0`
    
* Surface Water Operations (`SWO`) 
  
  * Fixed an index error that occurs when the Slang input does not declare any required flow variables.
  
  * Budget routine missing initialization for 3 diversion accounting variables. 
  
* `BAS` Options Fixes

    *  `PRINT_HEAD`, `PRINT_WATER_TABLE`, and `PRINT_WATER_DEPTH` no longer sorts the stress period and time step (`SPTS`).
        * In `MODULE BAS_OPTIONS_AND_STARTDATE`, if multiple `PRINT_HEAD`, `PRINT_WATER_TABLE`, or `PRINT_WATER_DEPTH` keywords are included to indicate output for different `SPTS`, then they would be sorted by stress period and time step. However, this only slowed the runtime rather than improved it.

    *  `PRINT_WATER_TABLE` and `PRINT_WATER_DEPTH` file header write error.
        * In `bas.f` the subroutine `GWF2BAS7OT` checks if `PRINT_WATER_TABLE` and/or `PRINT_WATER_DEPTH` options are in use and if the corresponding arrays should be written to a file for the stress period and time step that just finished. While writing the header to the output, the code would use the file unit number associated with `PRINT_HEAD` option, which would either put an extra header in those files or raise a random access violation error.
    *  Convergence output files changed date format to ISO Standard. This effects the following options:
        *  `PRINT_ITERATION_INFO`
        *  `PRINT_CONVERGENCE`
        *  `PRINT_FLOW_RESIDUAL`
        *  `PRINT_RELATIVE_VOLUME_ERROR`

* `SWR` Bug Fixes

    * Reach ending layer assignment is set to 1 if the elevation is above the top of layer 1 and set to `NLAY` if it is below the bottom of `NLAY`. Previously, this situation resulted in the layer assignment being undefined.
    * The code block that updates the number of QAQ connections (`NQAQCONN`) was specified too early in the code. The `CQAQCONN` loop was moved to the correct location.

* `NWT` Bug Fixes

    * Isolated model cells (that is surrounded by `IBOUND=0` cells) were previously set `HDRY` and the  `IBOUND` changed to zero. To be consistent with other flow packages, the head value is instead changed to `HNOFLO`.
      
    * `SUBROUTINE XMD7DA` added declaration for `IGRID` argument rather than using implicit typing.

    * `xmd_lib.f` reorder declaring variables.
      
      * Several routines in `xmd_lib.f` declare the subroutine arguments after local variables. Most compilers work around this, but it can cause strange effects for arguments that represent local array dimensions.
      
      * For example:   
        ```
        subroutine array_routine(dim)
             integer, dimension(dim):: work
             integer:: dim
        end subroutine
        ```
        Has the issue of `dim` being used as an array dimension before being declared. Most compilers, will catch this and automatically use dim appropriately, but it is best to restructure the routine as:  
        ```
        subroutine array_routine(dim)
             integer:: dim
             integer, dimension(dim):: work
        end subroutine
        ```


* `HOB` requires drawdown observations to be in chronological order or an error is raised. 
  

    * Either the user will need to fix the order or change the observation order or change to head observations.

* `MULT` Bug Fixes

    * `ExpressionParser` inline `IF` conditional index error.

        * If the `ExpressionParser` solved an expression within an inline `IF` that did not result in all the values being True or all False (that is changing the entire array), then an index error was raised.

        * Inline IF is defined as follows:  
            `IF[ COND, trueANS, falseANS ]`   
            &nbsp;  
            where:   
            `COND` is a conditional expression (must contain <, <=, >, >=)  
            `trueANS`  is the result returned where `COND` is True  
            `falseANS` is the result returned where `COND` is False.  
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; It is optional to include `falseANS`,  
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; if not present, then where `COND` is False,  
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; then the result returned is `NaN`.
            &nbsp;  
        * For example, given  
            `x = 10.`  
            `y = 5.`  
            would have:  
            `IF[ x + 3 > y, 2*x, 10*y ]`  
            return the `trueANS`, which is`2*x` or `20`  
            
            Another example is  
            `IF[ A < -1e-6 & A > 1e-6, 0, B / A ]`  
            would return an array with values of zero where `A` is near zero,  
            otherwise sets the location to `B` divided by `A`


### Refactoring

* `LPF` and `UPW` expanded comment support by using MF-OWHM read utilities.
  
    * Provides better error and warning messages for bad input.
    * Input allows comments and empty lines.
      * This mainly effected when properties were defined by parameters and   
        then the input would use a Fortran list-directed read to load the print factor (`IPRN`).  
        In particular, the following code:  
        `READ(IN,*) LAYFLG(1,K)`  
        was changed to:  
        `CALL READ_TO_DATA(LINE, IN, IOUT)`  
        `LLOC = 1`  
        `CALL GET_INTEGER(LINE, LLOC, ISTART, ISTOP, IOUT, IN, LAYFLG(1,K), ...` 
    * `DIS` improved end of file error message when not enough stress period information is specified.
    
* `FMP` `CLIMATE` Block refactored code to allow specifying `DIRECT_RECHARGE` multiple times and the sum of the recharge arrays are applied to deep percolation.

* `main.f90` white space cleanup and reordering of `HYD` subroutine calls.

  * Fixed indentation to make code line up.
  * Modified an if statement to improve speed when using `HYD` and calling `GWF2HYD7STR7RP` and `GWF2HYD7SFR7RP` routines.

* `xmd.f` indentation cleanup.

* `tabfile_module.f` reading the the actual tabfile's filenames now accept the keywords `OPEN/CLOSE`, `DATAFILE`, and `DATAUNIT`. 
  * Previously, the tabfile filename was specified without a keyword as just *FNAME* or `EXTERNAL` *UNIT*.  
    If the user used a keyword, such as `OPEN/CLOSE ./myTabFile.txt`,  
    then the program would stop saying `Failed to open "OPEN/CLOSE"` causing confusing to the user.

* `util.f` subroutines no longer raises a warning if the optional inputs `CNSTNT` or `IPRN` are not provided.

  * This effects the following subroutines:

    * `U1DREL`
    * `U2DINT`
    * `U2DREL`
    * `U2DDBL`

  *  Instead now, if either is not present, the following is written to the LIST file:  
    `Reading for XYZ, autoset the scale factor CNSTNT to 1.0, and the print flag IPRN to -1`
    

* `WEL` and `MNW2` improved missing `ITMP` warning.

  * For the `MNW2` and `WEL` packages, if the end of file is reached or the input fails to read `ITMP`, then `ITMP` is assumed to be zero for the rest of the simulation.
  * When this occurres a formal warning was made.  
    This was changed to being a minor message in the `LIST`ing file with the worlds:  
    `Don't Panic`  
    in the warning message.
  * It also indicates to the user that if they are using `LINEFEED` or `FMP-Link` that the rates may be initialized outside of the package.

* `FMP` variable `DRTFLOW` is always allocated when using FMP.

  * The `DRT-FMP` link requires the intermediate variable `DRTFLOW`. This variable was only allocated when `DRT` and `FMP` are both in use. To simplify the connection, `FMP` now always allocates `DRTFLOW` when `FMP` is in use.

* `FMP` improved "runoff leave model" warning message.

  * If a WBS/Farm contains runoff, but has no where to go, a soft warning is raised to let the user know which WBS has runoff and that the runoff is flowing out of the model because it has no where within the model to go.  

    The warning has been expanded to include suggestions on how to fix the problem and what potential causes of the warning are.  

    The warning also indicates that, it is not a problem, but instead is letting the user know that water is leaving the model domain automatically since the user did not specify a runoff location.  

* `FMP` source file standardization of indentation for the following files:

  * `src/fmp/allotment_data.f90`
  * `src/fmp/climate_data.f90`
  * `src/fmp/crop_data.f90`
  * `src/fmp/options_data.f90`
  * `src/fmp/output_data.f90`
  * `src/fmp/salinity_data.f90`
  * `src/fmp/soil_data.f90`
  * `src/fmp/surface_water_data.f90`
  * `src/fmp/surface_water_operations_data.f90`

* `src/fmp/wbs_data.f90`  added `SETUP_BASIC_VAR_WBS_DATA` subroutine in `WBS_DATA_FMP_MODULE`

    * In module `WBS_DATA_FMP_MODULE` the basic variable initialization and allocations done by `SUBROUTINE INITIALIZE_WBS_DATA` and `SUBROUTINE SETUP_NO_WBS_DATA` are moved to `SUBROUTINE SETUP_BASIC_VAR_WBS_DATA`.

    * This minimizes the redundant code between the two routines.

* `src/fmp/crop_data.f90`  moved crop initial array allocations to separate subroutine.

    * The FMP `LAND_USE` block, `MODULE CROP_DATA_FMP_MODULE`, contained conditional array allocations that occurred in the `SETUP_NEXT_STRESS_PERIOD` subroutine (RP routine). These have been moved to `SUBROUTINE SETUP_DEPENDENT_PARTS`.
    * By doing this, allows the allocations to occur at the FMP allocate and read routine (`SUBOURITNE FMP_AR`) after all the FMP block inputs have been read.

* `hydfmt.f` minor character variable cleanup to reduce final executable binary size.

    * To reduce the size of the binary, variables that were set to the same static string only set the first variable and then the remaining were set to the first.  
    * For example:  
        &nbsp; &nbsp; &nbsp; `a = '---------'`  
        &nbsp; &nbsp; &nbsp; `b = '---------'`  
        &nbsp; &nbsp; &nbsp; `c = '---------'`  
        is changed to:  
        &nbsp; &nbsp; &nbsp; `a = '---------'`  
        &nbsp; &nbsp; &nbsp; `b = a`  
        &nbsp; &nbsp; &nbsp; `c = a`



&nbsp; 

------

## 2.2.0

2022-01-20

git commit log: `git log 4bfb023b3a0f18d8a53a35146f85a93528d6ddd0..9d9f5b50c77a03b538e4ec818f5a67e7bcf3e5ea` 

### ZoneBudget v3.2

Update to ZoneBudget from `3.01` to `3.2`  
The update includes the MF-OWHM read utilities, new options, and fixed minor bugs.  
See [CHANGELOG_Features.md](CHANGELOG_Features.md#2.1.1) for a listing of new ZoneBudget features.

### Merge ⯬ BiF v1.0.2

- Batteries Included Fortran (BiF) version 1.0.2 source code merged.
    - https://code.usgs.gov/fortran/bif/-/tags/1.0.2

### Fixed

* `NWT` Bug Fixes

    * NWT considers the MODFLOW Outer Iteration (`1` to `mxiter`) to be each time it solves for the Jacobian, and does not include iterations solved with Backtracking (residual control). This causes a disconnect with the other MODFLOW packages that assume an outer iteration occurs every time the aquifer flow equations are formulated (package's  `FM` routines). To fix this issue, the number of MODFLOW Outer Iterations is always accounted for as the  number f times the `FM` routines are called.  

        As a result of this, previous models may need to increase their maximum number of `NWT` iterations (`MAXITEROUT`) to allow for convergence. Previously, the number of outer iterations was under-reported such that a model that said it converged in `XX` iterations, really converged in `YY` iterations, where `YY >> XX`.  

        Please see the [CHANGELOG_Features.md section 2.1.1 Section NWT Improvements for detailed information about this](CHANGELOG_Features.md#2.1.1).

    * `NWT` thin cell check is now disabled by default. 

        * To enable the thin cell check, add the option `THIN_CELL_CHECK`.  
            All "thin" cells removed from the simulation are written to the LIST file.

- `SFR` Modifications
    * The flow-depth-width lookup table, `ICALC = 4`, use `log10` interpolation,  
        except when the flow rate is less than the first point, then it uses linear interpolation.  
        If the first lookup point is `<1.0E-30`, then SFR uses linear interpolation for the first two points.
        * This prevents a floating point overflows from doing `log10(0.0)`.

* `FMP` Bug Fixes

    - `EVAPORATION_IRRIGATION_FRACTION  BY_IRRIGATE  LIST`  keyword changed to read a list of `NIRRIGATE` fractions instead of the incorrect `NCROP` fractions. 
        - The keyword `EVAPORATION_IRRIGATION_FRACTION  BY_CROP  LIST` is how to read `NCROP` fractions.  

    * Fixed issue when defining `NON_ROUTED_DELIVERY  VOLUME  STATIC` and `NON_ROUTED_DELIVERY  STATIC` that would continue to reduce for each stress period the available NRD water.
        * FMP would read the NRD volumes once, divide the volume by the stress period time to get a rate, then for each subsequent stress period divide the previous rate by the next stress period's time. Now, the rate is determined using the original volume defined.  

    - Added checks for when `NCROP = 0` to allowing running FMP without any land use/crops defined.


- `MNW2` Bug Fixes

    - Previously, the well head (`hwel`) was set to cell head for wells that went dry  
      (that is, pumping that exceeded the well capacity).
      - Now `hwel` is set to the bottom of the well when it goes dry.
      - This affects the actual pumping for wells with partial penetration (PP) enabled in a convertible layer.  

    * `MNW2` now only does the `RHS` formulation for the last 10 solver iterations in a time step that does not converge.
      - That is, if the solver has `MXITER = 200` iterations,  
        then `MNW2` solves for odd iterations by modifying the `RHS` (WEL-like action)  
         and even iterations by modifying the `HCOF`  (GHB-like action),  
        then only modifies RHS for solver iterations `≥190`.
      - This improves the mass error for time steps that fail to converge,  
        but have an option to continue to the simulation.  
        For example, `BAS` option `NO_FAILED_CONVERGENCE_STOP` or `NWT` `CONTINUE` option.  

    - `MNW2` now recalculates the node flows if the time step converges on an even iteration.  
      On odd iterations MNW2 specifies the node flow rate directly by modifying the `RHS`, but  
      for even iterations it uses the previous iterations `HNEW` to modify `HCOF`  
      and make the node flow a function of the next iterations `HNEW`.  
      - The difference between the last two iterations of `HNEW` can significantly change the reported node flows.  

    * `MNW2` and `NWT` fixed issue when writing to the cell-by-cell for nodes that have gone dry because NWT/UPW does not change the IBOUND array.  

* `SWT` now zeros out the `BUF`fer array before using it fixing an issue during Steady State stress periods.
  - Previously, `SWT` would write what the previous package had stored in `BUF` causing it to report flows that did not exist.  

- `SUB` fixes
  * Inelastic compaction is assumed to be complete if the head falls below the cell bottom.
    * That is, if the critical head falls below the cell bottom, then it is set to `-3.40E+38` to represent `-inf`.  

  - Compaction (inelastic and elastic) and expansion (elastic) only occurs  
    when the cell head is greater than the cell's  bottom elevation.
    - Assume `HOLD` is the time step starting head, `HNEW` time step ending head, `BOT` is the cell bottom:  
      if a time step has `HOLD>BOT` and `HNEW<BOT`, then compaction occurs but solves with `HNEW=BOT`.  
      if a time step has `HOLD<BOT` and `HNEW>BOT`, then expansion occurs but solves with `HOLD=BOT`.  
      if a time step has `HOLD<BOT` and `HNEW<BOT`, then nothing happens.

  - Changed the use of the PACK function to to use a mask array that is the same dimensions as BUFFER array. Previously, the mask was set to the scalar variable TRUE. This can have different meanings on different compilers.  
    An example of the changed code is:  
    `RNB(LOC1:LOC2) = PACK(BUFFER, TRUE)` was changed to:  
    `RNB(LOC1:LOC2) = PACK(BUFFER, PACK_MASK)`  
    where
    `real, dimension(NCOL,NROW):: BUFFER`  
    `logical:: TRUE = .true.`  
    `logical, dimension(NCOL,NROW):: PACK_MASK = .true.`  

* `CHOB` now works when using the `UPW` flow package and `NWT` solver.  

- `U2DREL` with `BINARY` input option now raises a warning instead of stopping with an error.
    - When the `BINARY`  option is found, MF-OWHM attempts to identify the binary input structure.  
      If it succeeds, a warning is raised about the portability of binary-unformatted files.  
      If it fails, then the program stops with an error message.  

* `expression_parser.f90` improved error message that is raised when a variable name is similar to a reserved keyword.  

- `linefeed.f90` now raises an error if the end of a FeedFile is reached before the end of the simulation.
  - A proper FeedFile should have one line of input per stress period simulated.  
    Previously, if the end of the file was reached, then the last lines input would be reused.

* Removed from the visual studio solution (`ide/visual_studio/OneWater_Project.sln`) the key entry:`GenAlternateCodePaths="codeForAVX"`
    * `AVX` acceleration caused floating point truncation that resulted in model results that were not repeatable. That is, the same model input would produce different numbers after the tenth digit. Results are now identical when running the multiple times with the same input.  
* Multiple files changed floating literal numbers that did not include a decimal point to have one.
    * For example, `X = 0D0` sets `X` to the integer `0`, rather than double precision `0.0`. The compiler than changes the zero from the integer to a double on the fly, but at the minor expense to runtime. To be syntactically correct, this is changed to `X = 0.D0` to indicate a double precision number.



### Refactoring

* `SFR` merged `SUBROUTINE GWF2SFR7LAKE` into `SUBROUTINE GWF2SFR7AD`.
  * Routines were redundant and called at the start of the time step.  
    Combining the two routines as one improves the readability of the code and has no affect on simulation results.

* Added newly supported keywords to `doc/Option_Block_Cheatsheets`
  * `BAS_Options_All.bas`
  * `BAS_Options_Recommended.bas`

- Added newly supported keywords to `doc/Notepadpp_Syntax_Highlighting/userDefineLangs`
  * `BAS-DIS.xml`
  * `MF-Flow-Packages.xml`
  * `MF-Packages.xml`

* Changed `DO CONCURRENT` loops to regular `DO` loops for code with multiple branches (`IF`-`ElSE`).  
  The current Fortran compilers seem to have issues with numerical accuracy  
  for complicated statements in a `DO CONCCURENT` loops.

- `fmp_main_driver.f90` changed `.NE.` to `/=` 

* `surface_water_data.f90` changed the `NON_ROUTED_DELIVERY` code from procedural to object oriented.  
  The new object is `TYPE NRD_VALUES` and keeps track of consumed and available imported water based on assigned demand. 

- `SUBROUTINE FMP3WELBD(KSTP,KPER,DELT,DATE,IGRID)` for the FB_DETAILS and FB_COMPACT output files rearranged the code for clarity and execution speed.  
  Changes occurred in `IF(FMPOUT%FB_COMPACT%IS_OPEN .OR. FMPOUT%FB_DETAILS%IS_OPEN) THEN` statement block.
- `BAS` option`BUDGETDB` , which writes for each time step the volumetric budget for each package writes the absolute value of the mass balance errors (always positive) for the `PERCENT_ERROR`  column.

&nbsp; 

------

## 2.1.0 ⯬ MODFLOW Surface Water Operations (`SWO`)

2021-05-25

Initial release of MODFLOW Surface Water Operations (`SWO`) in MF-OWHM
* Ferguson, I.M.., Llewellyn, D., Hanson, R.T., and Boyce S.E., 2016,  
  User guide to the surface water operations process—An integrated approach to  
  simulating large-scale surface water management in MODFLOW-based hydrologic models:  
  Denver, Colo., Bureau of Reclamation Technical Memorandum no. 86-68210–2016-02, 96 p.

git commit log: `git log d8ec82ae504a2aaec594ccd576f8674961f59404..4bfb023b3a0f18d8a53a35146f85a93528d6ddd0`

### Fixed

* `UZF` incorrectly set `ZEROD9 = 1.0d0-9`, now it is `1.0d-9` (that is, it was set to -8 and now it is 10<sup>-9</sup>).
    * This variable was used for near zero conditional checks.
* `GMRES` Solver incorrectly set `Stop_toldum = 1.0d0-9`, now it is `1.0d-9`.
    * `Stop_tol_gmres` is the tolerance for convergence of the iterative solver

- `MNW2` reported the well head (`hwel`) as the cell head for wells that went were dry (that is when pumping that exceeded the well capacity).
    - This affects the actual pumping if the well has partial penetration (PP) enabled and was in a convertible layer.  
      Because it would apply the PP correction to what is a seepage face rather than a pumping well.

* `CHOB` now works when using the `UPW` flow package and `NWT` solver.

- `U2DREL` with "`BINARY`" input option now raises a warning instead of stopping with an error.
    - When the "`BINARY`"  option is found, MF-OWHM attempts to identify the binary input structure.  
      If it succeeds, a warning is raised about the portability of binary-unformatted files.  
      If it fails, then the program stops with an error message. 

### Refactoring

* Added `slang/s_language.f90`
* Added `slang/s_language_global_pull.f90`
* Added `fmp/surface_water_operations_data.f90`
- Added `bif_lib/types_and_containers/variable_pointer_list_interface.f90`  
    from Batteries Included Fortran (BiF) version 1.0.1 source code.  
    https://code.usgs.gov/fortran/bif/-/tags/1.0.1

&nbsp; 

------

## 2.0.3

2021-05-25

git commit log: `git log 3adf1e3b8e634d83b8296fd673b3e3360a5cae06..d8ec82ae504a2aaec594ccd576f8674961f59404` 

### Fixed

* Improvement on warning and error messages in `MNW2`, `UPW`, and `NWT` packages.

&nbsp; 

------

## 2.0.2

2021-05-15

git commit log: `git log 12b331ce38c47a7e88f7da234c189ffa585d637a..3adf1e3b8e634d83b8296fd673b3e3360a5cae06`  
or web view at: https://code.usgs.gov/modflow/mf-owhm/-/compare/2.0.1..2.0.2

### Merge ⯬ BiF v1.0.1

- Batteries Included Fortran (BiF) version 1.0.1 source code merged.
    - https://code.usgs.gov/fortran/bif/-/tags/1.0.1

### Fixed

* `SFR` issue for segments with `ICALC ≥ 2` that would solve for stream depth with Newton–Raphson with an initial stream flow guess of zero instead of the stream reaches inflow.
    * For most cases this fix only affected simulation runtime and only altered the solution after the sixth significant digit (single precision tolerance).

- `FMP` — `LAND_USE (Crop) Block` output files did not correctly write `TOT_SURF_RUNOFF`  
    for crops that set `SURFACEWATER_LOSS_FRACTION_PRECIPITATION` to one. 
    - The affected options were `BYWBS`, `BYWBS_BYCROP`, `BYCROP`, `ALL`, and `ALL_VERBOSE`
    - This error had no affect on the actual simulation results nor other output files.

* `MNW2` using the `THIEM` losstype resulted in a warnings being triggered for a near zero skin radius, which is an input option used by he `SKIN` losstype.

- `MNW2` using `NWT` resulted in the specific storage was not calculated correctly for use in the partial penetration correction.

&nbsp; 

------

## 2.0.1

2021-03-15

git commit log: `git log 12b331ce38c47a7e88f7da234c189ffa585d637a`

### Merge ⯬ MF-NWT v1.2

- `NWT` Solver version 1.2 source code merged
    - `AG` package available (Agricultural Water Use Package)
    - It is recommended to use `FMP` instead of the `AG` package.
    - Obtained from: https://water.usgs.gov/water-resources/software/MODFLOW-NWT/MODFLOW-NWT_1.2.0.zip

### Fixed

* `BCF` was not recognized as one of the supported packages.  
    While not recommended, `BCF` is supported.

- `SFR` issue that causes it to use one layer deeper then the water table layer when all layers are defined as `convertible`.

* `MNW2` using `QLIMIT` with `NWT` resulted in the models that failed to converge do to a bad index reference for well head.

- `RCH` and `NWT` packages with `NRCHOP=3` did not pass recharge to the time step's upper most active layer. Previously, it only passed water to the upper most non-zero `IBOUND` cell rather than the upper most non-dry cell.
    - To mimic the original behavior of `RCH` with `NWT` set `NRCHOP = -1`, which applies recharge to the initial upper most non-zero `IBOUND` cell.

* `UPW`/`NWT` packages with convertible layers kept releasing water from storage after a model cell was dry.

- `HydMod` issue with `HD` (head) interpolation used the same four points for all observation points, which resulted in an extrapolation. Fixed such that head observations are interpolated by creating a four point finite element from the four closest cells to interpolate for the requested `X,Y` head location. (this is similar to how Hob operates).

### Refactoring

* Source code organized and renamed for clarity and  
    for some of the source code the package main module was moved to a separate file.
    * `gwf2ghb7_OWHM.f` is now `ghb.f`
    * `gwf2mnw27.f` is now `mnw2.f` and `mnw2_module.f90`

- Minor spelling corrections in warning and error messages.

* Added `path_interface.f90`, which provides subroutines for building Windows and Unix Paths and allows creating directories.

- The modules in `all_util.f90` were split into 48 separate files. The file name pertains to the util module it contains.

&nbsp; 

------

## 2.0.0

2020-04-07

Initial Release of MF-OWHMv2.

For a full listing of changes in this release see:
* Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60


### Refactoring

Naming convention of source files:

- Is the name of the package or the name of the package plus `_module` to indicate it contains the packages global variables.
- Is a generic name for the collection of subroutines within the source file.
- `_interface` indicates source code contains a generic `INTERFACE` call for a set of subroutines for a specific task.
- `_instruction` indicates source code defines one or more `Derived Data Types` definitions (Fortran Objects) and their associated methods (subroutines and functions associated with the object). 

