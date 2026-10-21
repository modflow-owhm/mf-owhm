# Changelog

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

**[New Feature Changelog](CHANGELOG_Features.md)**

Boyce, S.E., 2025, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version X.Y.Z: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp; 

------

[[_TOC_]]

------

&nbsp;

## 2.X.Y

2026-10-21  *-date is when file was last edited*

git commit log: `git log --reverse b6e95e5fe295c59427fe3b38ad575a7384666dec^..`

### Fixed
* `FMP` Farm Process
  * `PRINT SFR_RETURN` listed the runoff of each return reach under another reach's segment/reach label whenever a WBS defined its return points out of SFR reach order (or had a `SEG=0` point). The runoff applied to SFR and the totals by WBS were correct.
  * `SEMI_ROUTED_DELIVERY` points with `FRAC` weights applied `SEMI_ROUTED_DELIVERY_LOWER_LIMIT` and `SEMI_ROUTED_DELIVERY_UPPER_LIMIT` to the wrong diversion reach, and the `PRINT SFR_DELIVERY` output was mislabeled.
  * `SEMI_ROUTED_DELIVERY_LOWER_LIMIT` and `SEMI_ROUTED_DELIVERY_UPPER_LIMIT` ignored the scale factor given after the file name or with `SFAC`, so limits given in other units were used unscaled.
  * The binary `PRINT SFR_RETURN` wrote an extra integer per fully routed return record, and the per-WBS lost runoff record wrote two uninitialized values. Text output was not affected.
  * `MIN_BARE_FRACTION >= 1` crashed when using `MULTIPLE_LAND_USE_PER_CELL` model with an access violation. It performs as expected by disabling bare soil everywhere, as the FMP template documents.
    * The default `MIN_BARE_FRACTION` for `SINGLE_LAND_USE_PER_CELL` changed from `1` to `1E-12`, so cells with crop ID 0 inside a WBS remain bare soil as before.
  * WBS budget output (`FB_DETAILS`, `FB_COMPACT`, and `LAND_USE PRINT BYWBS`) reported the precipitation over the full WBS area. When a cell's crop fractions sum to <1 and bare soil is disabled, the remainder appeared as a budget discrepancy. The precipitation column now reports only the precipitation over the simulated crop and bare soil area.
  * The cumulative pumping header for MNW2-linked farm wells used an undefined farm index.
  * `CLIMATE` block `DIRECT_RECHARGE` set an array before it was allocated, and the crop area array size was checked before allocation. 
* `SFR` Streamflow Routing Package
  * The SFR reach net flow (runoff, stream ET, etc) calculation is different in the FM and BD routines; the BD was changed to match the FM. 
  * Negative runoff (an FMP delivery) larger than the water in a reach was booked as negative stream ET in the budget instead of being clipped as the FM routine does.
    * This effect was exacerbated due to the difference between FM and BD routines (see the previous bullet).
  * `ICALC=4` rating tables:
    * A first flow, depth, or width that is zero or less was silently reset to 0.1, 0.01, and 1.0, which distorted small flows and could make a valid table fail. The table is now checked once when it is read: a first row of zeros is dropped (it is always implied), and a first row with zero flow and depth but a width gives the bottom width of a flat-bottomed channel. A zero-flow row with a depth, a value that is zero or less, a flow or depth that does not increase, or a width that decreases stops the simulation with the table printed. Interpolation is linear from the origin (or bottom-width row) to the next row and log-log above it, and a table may have a single row.
    * The width for precipitation and evaporation on a reach with no flow was the first table width plus half the last width instead of their average.
  * 8-point cross sections (`ICALC=2`): the secant search for the stream depth divided by a near-zero difference when the first depth guess gave near-zero flow, which crashed with a floating-point divide by zero.
  * The residual water content used the specific yield of the wrong layer, or read out of bounds, when a confined layer was above the reach layer (LPF and BCF).
* `NWT` Newton Solver and `UPW` Upstream Weighting Package
  * `DHORIZ` returned zero for a partially saturated cell, which removed the Newton terms for convertible layers and made NWT behave close to a Picard solver. 
    * Effectively, past models operated similar to the Picard solution used by LPF. 
    * This fix should result in models converging in fewer outer iterations now.
  * `UPW` specific yield was indexed by layer instead of by convertible layer, so cells got the specific yield of another layer when a confined layer is above a convertible layer.
  * The `UPW` option `STORAGECOEFFICIENT` printed that it is not supported but was still applied incorrectly (storage coefficient times thickness). It now stops with an error that explains how to give specific storage instead.
  * `UPW` `LAYAVG=2` (confined logarithmic mean) raised a divide-by-zero exception when a neighboring cell had zero hydraulic conductivity.
  * `UPW` allocated the wetting flag twice and used it uninitialized.
  * The minimum `MXITER` for the `SIMPLE`, `MODERATE`, and `COMPLEX` options is lowered to 25, 100, and 200, and `SPECIFIED` has no minimum. The minimum is now also applied to the `MXITER` of later stress periods (before, it was overwritten), and the warning explains that MF-OWHM counts every outer iteration, including backtracking and residual control.
  * `MAX_HEAD_CHANGE` and `HEAD_DISTANCE_ABOVE_GSE_LIMIT` now scale the head change by the smallest damping factor over all cells and both limits, so a cell cannot stay above the limit after scaling. A disabled `MAX_HEAD_CHANGE` no longer compares a `NaN`.
  * With `IBOTAV`, a dry cell's previous head was raised to `BOT*1.0E-6` above the bottom, which is below the bottom for a negative bottom elevation. The offset is now `1.0E-6`.
  * `GMRES` ignored the stop tolerance from the input and used a fixed absolute residual of `1E-3`. Heads match to print precision; outer iterations decrease, sometimes greatly, and inner iterations increase.
  * `GMRES` ILU(k) wrote past the end of its factor arrays with a high fill level.
  * `XMD`: red-black ordering failed on models with few active cells; the conjugate gradient search direction was not initialized; the small-pivot guard was overwritten and never took effect.
  * `GMRES` and `XMD` produced `NaN` heads when the linear system started with a zero residual.
  * Corrected the single-cell model error and the thin cell warning messages.
* `UZF` Unsaturated Zone Flow Package
  * With the `BAS` global `CBC` unit, `IUZFCB2` always received the unit; `IUZFCB1` and `IUZFCB2` are now tested as intended.
  * With `UPW`, a confined uppermost active cell now uses the specific storage for the residual water content, as with `LPF`.
* `GHB` General-Head Boundary Package
  * `BHEAD` set to `NaN` for a cell that is not linked to a TabFile was silently set to `1.0`, a boundary at an elevation of one model unit. This now stops the simulation with an error that lists the cells.
    * When the cell is linked to a TabFile, `BHEAD` is a placeholder that is replaced by the TabFile value every time step, so a `NaN` there is allowed and ignored.
* `WEL` Well Package
  * `SPECIFY PHIRAMP` without the optional `IUNITRAMP` stopped with the error "Failed to successfully parse the line". The check for the optional unit was always true, so an output file was opened from an empty word. `IUNITRAMP` is now optional, as documented.
* `LineFeed`
  * Binary feed files with a scale factor other than `1.0` multiplied every value by the scale factor once per active (non-`NaN`) feature, that is `SCALE^N`. Each value is now scaled once. Text feed files were not affected.
  * gfortran release builds crashed with a segmentation fault on packages without auxiliary variables (for example the GHB and SFR examples). Related option flags are now initialized before the `LINEFEED` keyword test.
* `MNW2` Multi-Node Well Package
  * The pump capacity table (`PUMPCAP`) always interpolated with a zero slope.
* `HFB` Horizontal Flow Barrier Package
  * Layer-routed barriers that change between stress periods could deallocate an unallocated array or leave it too small, and when all routes were removed NWT kept the old routed connections and did not converge.
* `DRN` Drain Package
  * Fixed format input started the `AUXILIARY` and `NOPRINT` option scan at an undefined position.
* `SUB` Subsidence Package
  * Saving delay bed compaction without printing it used a stale array bound.
* `SWI2` Seawater Intrusion Package
  * The last column and row of the boundary solver conductances were used uninitialized.
* `PCGN` Solver
  * The maximum head change node was undefined when every head change was zero.
* `CHD`, `DRT`, and `HFB` wrote messages to an undefined output unit.
* `BAS` and `DIS`
  * The `CUMULATIVE_HEAD_CHANGE` file could mix parts of its header into the value line (ifx debug runtime).
  * The `DIS` end-of-file error handler read from an undefined unit and could raise a second runtime error.
  * Time unit conversion returned an undefined value for an unknown time unit.
* `ZoneBudget` and `HYDFMT` postprocessors
  * `HYDFMT` can now be built with gfortran (Fortran 2018 `IMPORT` statements removed).
  * `ZoneBudget` used an undeclared constant in its file error routine, and its test inputs did not match the file name case on case-sensitive file systems.

### Added
* `FMP` keywords `SEMI_ROUTED_DELIVERY_MISMATCH_TOLERANCE` and `SEMI_ROUTED_DELIVERY_MISMATCH_ABSOLUTE_TOLERANCE`, and the output `PRINT SFR_DELIVERY_MISMATCH`. FMP diversions use the SFR reach inflow of the previous iteration, so at convergence a delivery could exceed the final reach inflow; the check now forces further outer iterations until the mismatch is within the tolerance. See [CHANGELOG_Features.md](CHANGELOG_Features.md) and the FMP template.
* CMake and Meson build systems next to the makefile and the Visual Studio projects, sharing one set of source lists (`cmake/*-source.txt`) and compiler flags. [doc/BUILD.md](doc/BUILD.md) describes all build systems, compiler requirements, and setup on Linux, WSL, Windows, and containers.

### Refactoring
* Removed the compiler flag `-assume ieee_compares` from every build (makefile, CMake, Meson, and the Visual Studio projects).
  * MF-OWHM uses `NaN` as a flag for unset or disabled input and tested for it with `X /= X` (true only when `X` is `NaN`).
    * ifort allows `==` and `/=` with a `NaN`.
    * ifx 2026 with `-fpe0` raises a floating-point exception, stopping the program, for any comparison involving a `NaN` (including `MAX` and `MIN`).
  * `-assume ieee_compares` makes ifx behave like ifort, but models ran about 40 percent slower.
  * All `NaN` checks now use the Fortran 2003 standard function `IEEE_IS_NAN`, and the `NaN` test comes before any other comparison of a value that may be `NaN`.
  * Without the flag, an ifx build stops with a "floating invalid" error if a `NaN` reaches a comparison. A `NaN` can also come from an operation such as `inf - inf` or `0 * inf`. Please report any such error with the input that caused it.
* ifx is the default Fortran compiler (Intel discontinued ifort after oneAPI 2024.2). ifort is still supported with `make F90=ifort` and the Visual Studio `ReleaseIFORT` configuration (`mf-owhm.ifort.exe`). The Visual Studio `Debug`, `Fast_Debug`, and `Release` configurations use ifx and build with Visual Studio 2022 or 2026.
* Code cleanup: removed an unreachable `UZF` budget branch and the unused NWT backtracking counter, declared `HFBROUTECELL` with its `PURE` interface in NWT, removed a redundant `BiF` time series assignment, and reordered parts of `LMT`, `SFR`, and `SUB`.

&nbsp;

------

&nbsp;

## 2.3.1

2025-12-10

git commit log: `git log --reverse 455800a6bf6138e1b00c5ee17b6eb2108614e2e3^..e915dcdc65a2a8418f6ef57d3f8015d6750b24a9`

### Fixed
* `FMP` Farm Process
  * `BREAKING CHANGE`: Farm Net Recharge (`FNR`) writes to the Cell-By-Cell (`CBC`) in a way that separates Evapotranspiration from Groundwater (`ETgw`) and Deep Percolation (`DPERC`). This changes the volumetric budget so the `FNR` `IN` only contains `DPERC` and `FNR` `OUT` only contains `ETgw`. Note the net of `IN-OUT` remains the same.
    * Previously, the FMP writes `FNR` as `DPERC-ETgw` that is stored as a single array. This resulted in programs, such as ZoneBudget, to collect FNR IN as being the set of locations who had a net-in rather than just being `DPERC`. 
    * Now, `FNR` is written using the list-style/compact `CBC` writer that enables writing separate records for `ETgw` and `DPERC`.
  * SFR is not required when using the `SURFACE_WATER` block options: `SEMI_ROUTED_DELIVERY`, `SEMI_ROUTED_RETURN`, or `ROUTED_RETURN_ANY_REACH`, `ROUTED_RETURN_ANY_NON_DIVERSION_REACH`. Instead, if SFR is not part of the simulation, they are disabled a warning is raised.
  * `BARE_PRECIPITATION_CONSUMPTION_FRACTION` removed from FMP_Template.
    * The Farm Process input template, [doc/FMP_Template/FMP_Template.fmp](doc/FMP_Template/FMP_Template.fmp), incorrectly had a legacy keyword. This option was superseded by the `SOIL` block `EFFECTIVE_PRECIPITATION_TABLE` and the `CLIMATE` block `PRECIPITATION_POTENTIAL_CONSUMPTION`, which are applied to both bare and non-bare land uses.
  * Deep percolation conversion to runoff if all model cells are `IBOUND=0` beneath it.
    * If all the cells beneath the land surface are `IBOUND=0`, then deep percolation is shifted to runoff. This was correctly implemented for the solver, but not for the output files. This should improve some of the reported mass errors in a simulation, but will not change the actual head solution.
  * Keywords that support optional List-Array Input (LAI) failed to identify the LAI keyword if it was `CONSTANT` or `INTERNAL`. 
    * Several FMP keywords may only be the keyword itself to apply to all records, or optionally specify a `LAI[S,T,L]` input to specify the input on a record by record basis.
    * This effected:  
      `NO_RETURN_FLOW`  
      `ZERO_CONSUMPTIVE_USE_BECOMES_BARE_SOIL`  
      `EVAPORATION_IRRIGATION_FRACTION_SUM_ONE_CORRECTION`  
      `ALLOCATION_BENEFICIARY_SPLIT`
    * For example, `NO_RETURN_FLOW  [ LAI[S,T,L] ]` indicates that all runoff should be converted to deep percolation for all Farms/WBSs. The following is an example, where the last two bullets would not pick up the LAI keyword but now does (assumes `NWBS=3`):
      * `NO_RETURN_FLOW   # Applies to all WBS`   
      * `NO_RETURN_FLOW STATIC LIST OPEN/CLOSE no_return_flow.txt  # input located in no_return_flow.txt`   
      * `NO_RETURN_FLOW CONSTANT 1   # Applies to all WBS - Now works`   
      * `NO_RETURN_FLOW INTERNAL  # Specify for each WBS - Now works`  
        `1  1   # WBS 1 has it enabled`     
        `1  0   # WBS 2 has it disabled`  
        `1  1   # WBS 3 has it enabled`  
  * Cleaned up information written to the list file that describe the Groundwater Allotment that is assigned. 
  * `ADDED_DEMAND` without specifying the `ADDED_DEMAND_RUNOFF_SPLIT` now raises a warning.
  * If a model is started at a stress period other than 1 with the `FASTFORWARD` `BAS` option, then the headers were not written to the various FMP output files. This is now fixed.

* `BAS` Basic Package
  * `PRINT_WATER_DEPTH` output file wrote an empty line to `PRINT_WATER_TABLE` output file.
    * Fixed `PRINT_WATER_DEPTH` from using the unit number associated with the BAS option `PRINT_WATER_TABLE` when writing an empty line to separate records. If `PRINT_WATER_TABLE`, then a random file called fort.xxx with xxx being a random number, would be written with nothing but blank spaces.
  * `FASTFORWARD` caused `HOB` to not write out the header at the top of its output.
  * `FASTFORWARD` caused `HYD` to not write skipped time steps with `HYDNOH`.
  
* `NWT` Newton Solver (MF-OWHM specific version)
  * Allow keyword `CONTINUE` after the `SPECIFY` keyword.
    * The NWT solver instruction manual defines that a set of numbers are read after the `SPECIFY` keyword option. However, the MODFLOW-NWT source code allows for the `CONTINUE` keyword option to appear after `SPECIFY` and before the numerical parameters. This is also the default behavior for FloPy when making a NWT solver file. This feature was added back to maintain compatibility with MODFLOW-NWT and FloPy.
    * It is recommended to use the BAS option `NO_FAILED_CONVERGENCE_STOP`, which has the same effect and is not solver dependent.

* `UZF` did not set correctly the `CBC` unit number when using the GLOBAL `CBC` unit from the `BAS` package.
* `SFR` changed reach depth error to warning to allow a simulation to continue.
* `SWO` Surface Water Operations
  * Improved warning message descriptions.
  * `ULOAD` error when reading the `ABSOLUTE_CONVERGENCE_CRITERIA` and `RELATIVE_CONVERGENCE_CRITERIA` keywords that erroneously raised "Unable to open EXTERNAL XYZ" error, where XYZ was a random, small integer. This problem arose because the `ULOAD` unit number must be set to zero to indicate a new file or unit number must be specified, so the last value stored in that number was passed in resulting in the error message.
  * `RESERVOIR_SPLIT_FRACTIONS` keyword added.
    * In an early beta release of SWO, the split reservoir setting required the keyword `RESERVOIR_SPLIT_FRACTIONS`, however for the final release of SWO this keyword was changed to `RESERVOIR_PRIMARY_REREGULATION_SPLIT`. For backward compatibility, both keywords are now checked for and serve the same purpose.

* `Slang` MF-OWHM Language Interpreter (only used by SWO)
  * `SFR.seg.INFLOW` and `SFR.seg.OUTFLOW` are now correctly set for iteration-based slang scripts.
    * Slang can be invoked at different times of the simulation, model start, stress period start, time step start, iteration start, and at the end of those same times. For scripts invoked at the start of each iteration, the variables that are set to the inflow or outflow of a specific SFR segment reused the solution from the second iteration. It now updates with the solution of the previous iteration.
  * Add check for valid `REQ.DELIVERY.FARM` variables.
    * Slang property variables are set to values within MF-OWHM based on the current model state. The variables `REQ.DELIVERY.FARM.#` and `REQ.DELIVERY_VOL.FARM.#`, where # is replaced by a valid FARM/WBS number, are set to the water delivered to the WBS. However, if the user specified a number <1 or >NWBS resulted in random behavior, so a check was added to prevent this.
  * Fixed incorrect handling of duplicate variable names.
    * If a variable name, in the variable definition input is specified twice, then only one value should be stored. However, if a user happen to define the same variable name in the `PROPERTY` and `RETURN` variable blocks, then slang would correctly drop the duplicate from the `PROPERTY` block, but incorrectly drop the last variable defined in the block. However if the user entered a bad number, then either the variable was set to a random value or resulted in a runtime index error.

* `DRT` Drains with Return Flow (MF-OWHM specific version)
  * `AUTOMATIC_NEGATIVE_ITMP` option for drains that used FMP to collect the drain flow as runoff did not update the corresponding Water Balance Subregion (WBS) if the DRT cell was set to automatically determine the WBS and the WBS changes by stress period. This issue has been fixed.
  
* `CFP` Conduit Flow Process (MF-OWHM specific version)
  * Fixed an input read error for the advanced CFP input, which reads in a set of boundary condition flags to modify the input structure. If the advanced input is skipped, then an `X` must be used as a placeholder. However, this raised an error that the option was not found.
  * Index error for Time-Dependent Boundary Conditions (TD)
    * The search algorithm for finding the time interval no longer raises an index error if the time falls within the first interval. 
    * The algorithm was also refactored to improve the execution speed.
  * Remove potential of taking `sqrt` of a negative number.

* `validate_example_results.f90` 
  * Fixed missing character initialization.

  * Includes owhm-v2 simple examples as part of validation.

* `examples/bash_example_run/1_RunValidation.sh`:
  * The example driver bash script runs all the example problems and then runs a Fortran tool to check if they match a known hash value. If one or more examples do not match, then the Fortran tool returns a non-zero exit status code. However, on some versions of bash this would instead exit the entire script preventing the clean up and closing comments from running. Instead a set of if and conditional `||` are used to check if the examples pass and then the script completes normally.

* `options.print_convergence.txt` fixed an incorrect header description.

* `BiF` code updates that fix an issue with the `COMPRESSED_VALUE_STORAGE` not correctly handling zero stored values. This only occurs when it previously had a value and then was reallocated to no values.


### Refactoring

* `FMP`
  * `EFFICIENCY` keyword changed to `IRRIGATION_EFFICIENCY`
  * `EFFICIENCY_IMPROVEMENT ` keyword changed to `IRRIGATION_EFFICIENCY_IMPROVEMENT`
    * The original keywords are still supported to maintain backward compatibility, but the new versions are now used in the FMP_Template and LIST output.

* `LPF` and `UPW` improved wettable description.  
  The LPF/UPW packages output the layer wettable description to the list file as `NEVER-DRY` or `WETTABLE`.  
  To be more descriptive the variable now outputs:
    * `NEVER-DRY`
      * Indicates layer is confine
    * `STAY-DRY`
      * Indicates layer is convertible without wetting
    * `WETTABLE`
      * Indicates layer is convertible with wetting
* `fmp_template.fmp`
  * Several of the FMP keywords did not indicate the units of the input. This has been corrected to include the units for all keywords.

* `CFP` changed automatic arrays to allocatable to remove potential of a stack overflow for large models.
* `CFP` changed exponents from real to integers to improve speed.
  * For example, `x**3.0` was changed to `x**3` to tell the compiler to expand the variable rather than using a floating point pow library. That is, `x**3` results in the compiler doing `x*x*x` in the assembly code.
* LineFeed comment cleaning and changed Fortran equality letters to symbols (`.EQ.` was changed to `==`; `.GT.` was changed to `>`; ...)
* `Slang` added to `TYPE(VARIABLE_NAME_MEANING)` the routine `SUBROUTINE EQUALITY_VARIABLE_NAME_MEANING`, which is assigned as the `GENERIC :: OPERATOR(==)` to allow variable equality checks.
* Bash scripts now start with `#!/bin/env bash` instead of `#!/bin/bash`

&nbsp;

------

&nbsp;

## 2.3.0

2024-01-10

git commit log: `git log --reverse 9d9f5b50c77a03b538e4ec818f5a67e7bcf3e5ea^..455800a6bf6138e1b00c5ee17b6eb2108614e2e3`

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

### CFP MODE 2 and 3 is Disabled

* `CFP` input specifies `MODE` as being 1, 2, or 3. 
  * `MODE 1` solves the conduit pipes (available)
  * `MODE 2` solves conduit layers (disabled)
  * `MODE 3` combines 1 and 2  (disabled)


Mode 2, and consequently 3, is failing to converge for the example problems nor matching the example results. In an abundance of caution this feature has been disabled until the reason for the discrepancy is found. If mode 2 or 3 is selected then an error message is raised.

Only MODE 1 is allowed until this issue is fixed.

### Fixed

* `RCH` reverted back to using upper most `IBOUND /= 0` cell to match results from MF-NWT.
  
    * This removes the RCH fix in [2.0.1](#2.0.1) that changed the check to include for convertible layers a check for if the head is above the cell bottom.    
    * Added `NRCHOP = 4` and `NRCHOP = 5` as options to mimic the old behavior.    
    * For details about this change, please see [CHANGELOG_Features.md](CHANGELOG_Features.md#2.3.0)
    
* `FMP` Bug Fixes
  
    * Can use `FMP` without `SFR`.
      * Previously, if `FMP` is used without `SFR`, then an index error occurred when `FMP` attempted to check for runoff and delivery locations. To prevent this index error from occurring the user had to specify the `WATERSOURCE` keyword to indicate that `SFR` is not available. 
      * Now if `SFR` is not in use, then `FMP` automatically disables `SFR` diversions and runoff is flagged for flowing out of the model (rather than to a specific `SFR` location).  
    * `MNW2`-`FMP` Supply well link no longer crashes if row and column do not match (`SUPPLY_WELL` Block). Instead, `FMP` now copies the row and column specified in `MNW2`. The row and column input is still required in `FMP` as a placeholder, but not used.
    * `SURFACE_WATER` block `PRINT SFR_RETURN` keyword with binary output (`BINARY` keyword) wrote a random integer if the WBS did not have assigned any SFR return flow locations.
      * The fix changed this to write the segment and reach as zero to indicate no SFR return flow point was specified.
      * Note this is how the text version of the option already works.
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
        * Previously, in `MODULE BAS_OPTIONS_AND_STARTDATE`, if multiple `PRINT_HEAD`, `PRINT_WATER_TABLE`, or `PRINT_WATER_DEPTH` keywords were included to indicate output for different `SPTS`, then they would be sorted by stress period and time step. However, this only slowed the runtime rather than improved it.

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

    * Isolated model cells (ones that are surrounded by `IBOUND=0` cells) were previously set to `HDRY` and the `IBOUND` changed to zero. To be consistent with other flow packages, the head value is instead changed to `HNOFLO`.
      
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

* `HOB` now requires drawdown observations to be in chronological order or an error is raised. 

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
      * This mainly affected when properties were defined by parameters and   
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

* `tabfile_module.f` reading the actual tabfile's filenames now accept the keywords `OPEN/CLOSE`, `DATAFILE`, and `DATAUNIT`. 

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
  * When this occurs a formal warning was made.  
    This was changed to being a minor message in the `LIST`ing file with the words:  
    `Don't Panic`  
    in the warning message.
  * It also indicates to the user that if they are using `LINEFEED` or `FMP-Link` the rates may be initialized outside of the package.

* `FMP` variable `DRTFLOW` is always allocated when using FMP.

  * The `DRT-FMP` link requires the intermediate variable `DRTFLOW`. This variable was only allocated when `DRT` and `FMP` are both in use. To simplify the connection, `FMP` now always allocates `DRTFLOW` when `FMP` is in use.

* `FMP` improved "runoff leave model" warning message.

  * If a WBS/Farm contains runoff, but has no where to go, a soft warning is raised to let the user know which WBS has runoff and that the runoff is flowing out of the model because it has no where within the model to go.  

    The warning has been expanded to include suggestions on how to fix the problem and what potential causes of the warning are.  

    The warning also indicates that water is leaving the model domain automatically since the user did not specify a runoff location.  

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
    * By doing this, the allocations can occur at the FMP allocate and read routine (`SUBOURITNE FMP_AR`) after all the FMP block inputs have been read.

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

&nbsp;

## 2.2.0

2022-01-20

git commit log: `git log --reverse 4bfb023b3a0f18d8a53a35146f85a93528d6ddd0^..9d9f5b50c77a03b538e4ec818f5a67e7bcf3e5ea` 

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

&nbsp;

## 2.1.0 ⯬ MODFLOW Surface Water Operations (`SWO`)

2021-05-25

Initial release of MODFLOW Surface Water Operations (`SWO`) in MF-OWHM
* Ferguson, I.M.., Llewellyn, D., Hanson, R.T., and Boyce S.E., 2016,  
  User guide to the surface water operations process—An integrated approach to  
  simulating large-scale surface water management in MODFLOW-based hydrologic models:  
  Denver, Colo., Bureau of Reclamation Technical Memorandum no. 86-68210–2016-02, 96 p.

git commit log: `git log --reverse d8ec82ae504a2aaec594ccd576f8674961f59404^..4bfb023b3a0f18d8a53a35146f85a93528d6ddd0`

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

&nbsp;

## 2.0.3

2021-05-25

git commit log: `git log --reverse 3adf1e3b8e634d83b8296fd673b3e3360a5cae06^..d8ec82ae504a2aaec594ccd576f8674961f59404` 

### Fixed

* Improvement on warning and error messages in `MNW2`, `UPW`, and `NWT` packages.

&nbsp; 

------

&nbsp;

## 2.0.2

2021-05-15

git commit log: `git log --reverse 12b331ce38c47a7e88f7da234c189ffa585d637a^..3adf1e3b8e634d83b8296fd673b3e3360a5cae06`  
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

&nbsp;

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

- *`RCH` and `NWT` packages with `NRCHOP=3` did not pass recharge to the time step's upper most active layer. Previously, it only passed water to the upper most non-zero `IBOUND` cell rather than the upper most non-dry cell.*
    - *To mimic the original behavior of `RCH` with `NWT` set `NRCHOP = -1`, which applies recharge to the initial upper most non-zero `IBOUND` cell.*
    - **Removed in 2.3.0, see [CHANGELOG_Features](CHANGELOG_Features.md) for more information.**

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

&nbsp;

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

clear
