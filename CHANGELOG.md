# Changelog

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

**[New Feature Changelog](CHANGELOG_Features.md)**

Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version X.Y.Z: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp; 

------

[[_TOC_]]


## 2.2.0

2022-01-17

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

    * `NWT` thin cell check is disabled. 

        * By default, the `NWT` solver would check all model cells vertical thickness (`thick`) against the largest/thickest cell (`mxthick`). Any cell that had it's `thick < 0.01*mxthick` was changed to `IBOUND=0` (removed from the simulation/assumed impermeable rock). This caused models that had a few very model cells to drop out smaller ones, which might be thin clay layers in the middle of the model. Users were no aware that part of their model was not being simulated. To prevent this situation from occurring, this check is disabled by default.

        * To enable the thin cell check, add the option `THIN_CELL_CHECK`.  
            All cells removed from the simulation are written to the LIST file.

- `SFR` Modifications
    * The flow-depth-width lookup table, `ICALC = 4`, use `log10` interpolation,  
        except when the flow rate is less than the first point, then it uses linear interpolation.  
        If the first point is lookup point is `<1.0E-30`, then SFR uses linear interpolation for the first two points.
        * This prevents a floating point overflows from doing `log10(0.0)`.

* `FMP` Bug Fixes

    - `EVAPORATION_IRRIGATION_FRACTION  BY_IRRIGATE  LIST`  keyword changed to read a list of `NIRRIGATE` fractions instead of the incorrect `NCROP` fractions. 
        - The keyword `EVAPORATION_IRRIGATION_FRACTION  BY_CROP  LIST` is how to read `NCROP` fractions.  

    * Fixed issue when defining `NON_ROUTED_DELIVERY  VOLUME  STATIC` and `NON_ROUTED_DELIVERY  STATIC` that would continue to reduce for each stress period the available NRD water.
        * FMP would read the NRD volumes once, divide the volume by the stress period time to get a rate, then for each subsequent stress period divide the previous rate by the next stress period's time. Now, the rate is determined using the original volume defined.  

    - Added checks for when `NCROP = 0` to allowing running FMP without any land use/crops defined.


- `MNW2` Bug Fixes

    - Reported the well head (`hwel`) as the cell head for wells that went were dry  
      (that is, when pumping that exceeded the well capacity).
      - This affects the actual pumping if the well has partial penetration (PP) enabled and was in a convertible layer.  
        Because it would apply the PP correction to what is a seepage face rather than a pumping well.  

    * `MNW2` now only does the `RHS` formulation for the last 10 solver iterations in a time step that does not converge.
      - That is, if the solver has `MXITER = 200 ` iteration,  
        then `MNW2` solves for odd iterations by modifying the `RHS` (WEL-like action)  
         and even iterations modifies `HCOF`  (GHB-like action) ,  
        then only modifies RHS for solver iterations `≥190` ,
      - This improves the mass error for time steps that fail to converge,  
        but have an option to continue to the simulation.  
        For example, `BAS` option `NO_FAILED_CONVERGENCE_STOP` or `NWT` `CONTINUE` option.  

    - `MNW2` recalculates the node flows if the time step converges on an even iteration.  
      On odd iterations MNW2 specifies the node flow rate directly by modifying the `RHS`, but  
      for even iterations it uses the previous iterations `HNEW` to modify `HCOF`  
      and make the node flow a function of the next iterations `HNEW`.  
      - The difference between the last two iterations of `HNEW` can significantly change the reported node flows.  

    * `MNW2` and `NWT` fixed issue when writing to the cell-by-cell for nodes that have gone dry.  

* `SWT` now zeros out the `BUF`fer array before using it fixing an issue during Stead State stress periods.
  - Previously, `SWT` would write what the previous package had stored in `BUF` causing it to report flows that did not exist.  

- `SUB` fixes
  * Inelastic compaction is assumed to be complete if the head falls below the cell bottom.
    * That is, if the critical head fallows below the cell bottom, then it is set to `-3.40E+38` to represent `-inf`.  

  - Compaction (inelastic and elastic) and expansion (elastic) only occurs  
    when the cell head is greater than the cell's  bottom elevation. 
    - Assume `HOLD` is the time step starting head, `HNEW` time step's ending head, `BOT` is the cell bottom:   
      if a time step has `HOLD>BOT` and `HNEW<BOT`, then compaction occurs but solves with `HNEW=BOT`.  
      if a time step has `HOLD<BOT` and `HNEW>BOT`, then expansion&nbsp; &nbsp; occurs but solves with `HOLD=BOT`.  
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

- `U2DREL` with "`BINARY`" input option now raises a warning instead of stopping with an error.
    - When the "`BINARY`"  option is found, MF-OWHM attempts to identify the binary input structure.  
      If it succeeds, a warning is raised about the portability of binary-unformatted files.  
      If it fails, then the program stops with an error message.  

* `expression_parser.f90` improved error message that is raised when a variable name is similar to a reserved keyword.  

- `linefeed.f90` now raises an error if the end of a FeedFile is reached before the end of the simulation.
  - A proper FeedFile should have one line of input per stress period simulated.  
    Previously, if the end of the file was reached, then the last lines input would be reused.

* Removed from the visual studio solution (`ide/visual_studio/OneWater_Project.sln`) the key entry:`GenAlternateCodePaths="codeForAVX"`
    * `AVX` acceleration resulted floating point truncation that resulted in model results that were not repeatable. That is, the same model input would produce different numbers after the tenth digit. Results are now identical when running the multiple times with the same input.  


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

* Changed `DO CONCURRENT` loops to a regular `DO` loops for code with multiple branches (`IF`-`ElSE`).  
  The current Fortran compilers seem to have issues with numerical accuracy  
  for complicated statements in a `DO CONCCURENT` loops.

- `fmp_main_driver.f90` changed `.NE.` to `/=` 

* `surface_water_data.f90` changed the `NON_ROUTED_DELIVERY` code from procedural to object oriented.  
  The new object is `TYPE NRD_VALUES` and keeps track of consumed and available imported water based on assigned demand. 

- `SUBROUTINE FMP3WELBD(KSTP,KPER,DELT,DATE,IGRID)` for the FB_DETAILS and FB_COMPACT output files rearranged the code for clarity and execution speed.  
  Changes occurred in `IF(FMPOUT%FB_COMPACT%IS_OPEN .OR. FMPOUT%FB_DETAILS%IS_OPEN) THEN` statement block.

&nbsp; 

------

## 2.1.0 ⯬ MODFLOW Surface Water Operations (`SWO`)

2021-05-25

Initial release of MODFLOW Surface Water Operations (`SWO`) in MF-OWHM
* Ferguson, I.M.., Llewellyn, D., Hanson, R.T., and Boyce S.E., 2016,  
  User guide to the surface water operations process—An integrated approach to  
  simulating large-scale surface water management in MODFLOW-based hydrologic models:  
  Denver, Colo., Bureau of Reclamation Technical Memorandum no. 86-68210–2016-02, 96 p.

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

### Fixed

* Improvement on warning and error messages in `MNW2`, `UPW`, and `NWT` packages.

&nbsp; 

------

## 2.0.2

2021-05-15

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

