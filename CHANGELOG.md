# Changelog

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

**[New Feature Changelog](CHANGELOG_Features.md)**

Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version X.Y.Z: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp; 

------

[[_TOC_]]

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

### Refactoring

* Source code organized and renamed for clarity and  
    for some of the source code the package main module was moved to a separate file.
    * `gwf2ghb7_OWHM.f` is now `ghb.f`
    * `gwf2mnw27.f` is now `mnw2.f` and `mnw2_module.f90`

- Minor spelling corrections in warning and error messages.

* Added `path_interface.f90`, which provides subroutines for building Windows and Unix Paths and allows creating directories.

- The modules in `all_util.f90` were split into 48 separate files. The file name pertains to the util module it contains.

### Fixed

* `BCF` was not recognized as one of the supported packages.  
    While not recommended, `BCF` is supported.

- `SFR` issue that causes it to use one layer deeper then the water table layer when all layers are defined as `convertible`.

* `MNW2` using `QLIMIT` with `NWT` resulted in the models that failed to converge do to a bad index reference for well head.

- `RCH` and `NWT` packages with `NRCHOP=3` did not pass recharge to the time step's upper most active layer. Previously, it only passed water to the upper most non-zero `IBOUND` cell rather than the upper most non-dry cell.
    - To mimic the original behavior of `RCH` with `NWT` set `NRCHOP = -1`, which applies recharge to the initial upper most non-zero `IBOUND` cell.

* `UPW`/`NWT` packages with convertible layers kept releasing water from storage after a model cell was dry.

- `HydMod` issue with `HD` (head) interpolation used the same four points for all observation points, which resulted in an extrapolation. Fixed such that head observations are interpolated by creating a four point finite element from the four closest cells to interpolate for the requested `X,Y` head location. (this is similar to how Hob operates).

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

