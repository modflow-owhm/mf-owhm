# Changelog

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

**[New Feature Changelog](CHANGELOG_Features.md)**

Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version X.Y.Z: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp; 

------

[[_TOC_]]

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

