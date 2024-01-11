

# MODFLOW-OWHM Example Problems

This directory contains the example problems used to validate MF-OWHM. 

These examples have been compiled from all the releases of MODFLOW  
that MF-OWHM supports and includes additional developed examples.

Special thanks to Jon Traum for developing the `MF-OWHM` example problem, which includes a writeup (`examples/mf-owhm/README.md`) on the steps for building a basic mf-owhm model. The steps are: `0_Base`, `1_GHB`, `2_FMP`, and `3_SFR`.

In each example directory there two sub-directories: `output-true` and `output`. 

- `output` is the location that example problem simulated results are written to. 
- `output-true` contains select output files that are considered the correct solution that are used for comparison purposes.

&nbsp; 

## Bash Driver Files

To run the example problems, a series of bash scripts are provided in the `bash_example_run` directory. 

All bash scripts reference `0_SelectProgramUsedByBASH.sh`, which defines the executable to run. This script selects `mf-owhm.exe` if invoked on a MS Windows (WinNT), otherwise it will run `mf-owhm.nix`. 

The bash scripts are named after the example directory that they are setup to run.  
For example, `2_RunAllTests_MF_2005.sh` will run all the examples in the directory `mf-2005`.

The bash script `1_RunValidation.sh` is a special script that will clean out all the example problems `output` directories, then call all the included example problem bash scripts. Once they have completed, it will compile using `gfortran` the `validate_example_results.f90,` then execute the resulting program, and then remove the compiled executable. The program compiled by `validate_example_results` reads in each of the `output` directories and compares the results with the files stored in the `output-true` directory. 

`1_RunValidation.sh` supports the following options command line options:

| Command Argument | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| all              | Includes extended tests (`2_RunAllTests_MF_OWHM_v1`).  <br />These additional tests add approximately 3 hours to the total testing runtime.  <br />This option is not recommended. |
| check            | Do not clean the example problem output directories, nor run the example problems.  <br />Instead only check what is currently stored in the example problem `output` directory with the `output-true` directory. |
| debug            | Indicates that the debug version of mf-owhm should be run instead of the release version.  <br />Assumes that `mf-owhm-debug` is located in the `bin` directory. |
| fast             | Run the the example tests in parallel. This reduces the total runtime of the testing, but requires at least 3 cpu threads. |
| nopause          | Disable pausing the program once complete.                   |
| save             | Do not delete the the the resulting executable from compiling `validate_example_results.f90` |
| resuse           | if the compiled executable from `mf_owhm_validate_example_results.f90` already exists, reuse it instead of recompiling it. This also also implies the `save` option (does not delete the compiled executable). |

&nbsp; 

## Directory Names

| Directory        | Description                                                  |
| :--------------- | ------------------------------------------------------------ |
| bash_example_run | Directory that contains Bash scripts to run the files in the example directories. <br /><br />Contains the unit testing script, `1_RunValidation.sh`, <br />which runs all the example problems and then compiles with<br />`gfortran` the file `validate_example_results.f90` to check the <br />results of the example problems with the output-true directories. |
| mf-2005          | MODFLOW-2005 Standard Test example models.<br />They have been modified to use the additional features unique to MODFLOW-OWHM. |
| mf-2005-nwt      | MODFLOW-2005 Standard Test example models converted to use the NWT solver and UPW flow package. |
| mf-cfp           | MODFLOW-CFP Standard Test example models for MODE 1.<br />They have been modified to use the additional features unique to MODFLOW-OWHM. |
| mf-nwt           | MODFLOW-NWT Standard Test example models.<br />They have been modified to use the additional features unique to MODFLOW-OWHM. |
| mf-owhm          | Example problem that demonstrate the advance features of MODFLOW-OWHMv2 developed by Jon Traum. See [MODFLOW-OWHM Example Readme](./mf-owhm/README.md) |
| mf-owhm-v1       | MODFLOW-OWHM example problems documented in <br />Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey Techniques and Methods 6–A51, 120 p., https://dx.doi.org/10.3133/tm6A51 |
| mf-rip           | Riparian Evapotranspiration Package (RIP) example problems.<br />Maddock, T., III, Baird, K.J., Hanson, R.T., Schmid, Wolfgang, and Ajami, H., 2012, RIP-ET: A riparian evapotranspiration package for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A39, 76 p. |
| mf-swi           | Seawater intrusion Package (SWI2) example problems.<br />Bakker, M., Schaars, F., Hughes, J.D., Langevin, C.D., Dausman, A.M., 2013, Documentation of the seawater intrusion (SWI2) package for MODFLOW: U.S. Geological Survey Techniques and Methods 6-A46, 47 p. |
| mf-swr           | Surface-Water Routing Process (SWR1) example problems.<br />Hughes, J.D., Langevin, C.D., Chartier, K.L., and White, J.T., 2012, Documentation of the Surface-Water Routing (SWR1) Process for modeling surface-water flow with the U.S. Geological Survey Modular Ground-Water Model: U.S. Geological Survey Techniques and Methods 6-A40, 113 p. |

