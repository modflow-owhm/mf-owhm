# Changelog

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

**[New Feature Changelog](CHANGELOG_Features.md)**

Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version X.Y.Z: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp; 

------

[[_TOC_]]

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

