

# MODFLOW-OWHM Example Problems

This directory contains the example problems used to validate MF-OWHM. 

These examples have been compiled from all the releases of MODFLOW that MF-OWHM supports and includes additional developed examples.

In each example directory contains two sub-directories: `output-true` and `output`. 

- The directory `output` is the location that example problem simulated results are written to. 
- The directory `output-true` contains select output files that are considered the correct solution that are used for comparison purposes.

Special thanks to Jon Traum for developing the MF-OWHM

## Bash Driver File

A series of bash files are written that when executed run the example problems. 

## Directory Names

| Directory        | Description                                                  |
| :--------------- | ------------------------------------------------------------ |
| bash_example_run | Directory that contains script written in BASH to run each example directory model cells. <br />Also includes BASH scripts that run specific examples. |
| mf-2005          | MODFLOW-2005 Standard Test example models.<br />They have been modified to use the additional features unique to MODFLOW-OWHM. |
| mf-2005-nwt      | MODFLOW-2005 Standard Test example models converted to use the NWT solver and UPW flow package. |
| mf-nwt           | MODFLOW-NWT Standard Test example models.<br />They have been modified to use the additional features unique to MODFLOW-OWHM. |
| mf-owhm          | Example problem that demonstrate the advance features of MODFLOW-OWHMv2 developed by Jon Traum. See [MODFLOW-OWHM Example Readme](./mf-owhm/README.md) |
| mf-owhm-v1       | MODFLOW-OWHM example problems documented in <br />Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey Techniques and Methods 6–A51, 120 p., https://dx.doi.org/10.3133/tm6A51 |
| mf-rip           | Riparian Evapotranspiration Package (RIP) example problems.<br />Maddock, T., III, Baird, K.J., Hanson, R.T., Schmid, Wolfgang, and Ajami, H., 2012, RIP-ET: A riparian evapotranspiration package for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A39, 76 p. |
| mf-swi           | Seawater intrusion Package (SWI2) example problems.<br />Bakker, M., Schaars, F., Hughes, J.D., Langevin, C.D., Dausman, A.M., 2013, Documentation of the seawater intrusion (SWI2) package for MODFLOW: U.S. Geological Survey Techniques and Methods 6-A46, 47 p. |
| mf-swr           | Surface-Water Routing Process (SWR1) example problems.<br />Hughes, J.D., Langevin, C.D., Chartier, K.L., and White, J.T., 2012, Documentation of the Surface-Water Routing (SWR1) Process for modeling surface-water flow with the U.S. Geological Survey Modular Ground-Water Model: U.S. Geological Survey Techniques and Methods 6-A40, 113 p. |

