# GMG Numerical Solver Readme

MF-OWHM is mostly written in Fortran (77, 95, 2003, and 2008) except that the GMG solver is written in C. 

To provide an executable that is written in pure Fortran, MF-OWHM does not include the GMG solver in the *Windows x64 Executable* download link, which downloads `mf-owhm.exe`. 

If your project uses the GMG solver, then instead use the download link *Windows x64 Executable with GMG Solver*, which downloads `mf-owhm-gmg.exe`.  

If you compile the executable binary, then it will be necessary to decide to include GMG or not. If you want to compile with GMG, it is necessary to have both a Fortran and C compilers that can co-compile code together.

The provided `makefile` is setup to compile both with and without GMG by setting the `USEGMG=` option to `YES` or `NO`. There also Visual Studio Solution files for compiling with, `OneWater_GMG_Project.sln`, or without GMG, `OneWater_Project.sln`.

For more detailed information about the Visual Studio solutions, please see `ide/visual_studio_2017/readme.md`



## Compiling GMG With GMG

When compiling with GMG the following source files are used.

### C components

- `gmg_c/ccfd.c`
- `gmg_c/mf2kgmg_OWHM.c`
- `gmg_c/r_vector.c`
- `gmg_c/solvers.c`

### C headers

- `gmg_c/ccfd.h`
- `gmg_c/mf2kgmg_OWHM.h`
- `gmg_c/r_vector.h`
- `gmg_c/solvers.h`

### Fortran components

- `gmg_c/gmg7_c_interface.f90`
- `gmg_c/gmg7_dble.f`



## Compiling GMG Without GMG

When compiling without GMG the the C code is skipped and the Fortran is replaced with a special file that raises a warning if GMG is used.

### Fortran components

- `gmg_c/0_nogmg.f90`
- `gmg_c/gmg7_c_interface.f90`



