# GMG Numerical Solver Readme

## GMG is Optional, but requires a C compiler and is co-compiled with the Fortran

### Skip Compilng GMG

If you do have a C compiler or do not want to co-compile with GMG, then only compile with MODFLOW-OWHM the following fortran file. This disables GMG and provides a bypass to the C code.

./gmg_c/0_nogmg.f 

### Co-compile GMG - C with Fortran

#### C components are:

./gmg_c/ccfd.c
./gmg_c/mf2kgmg_OWHM.c
./gmg_c/r_vector.c
./gmg_c/solvers.c

#### C headers are:

./gmg_c/ccfd.h
./gmg_c/mf2kgmg_OWHM.h
./gmg_c/r_vector.h
./gmg_c/solvers.h

#### Fortran compents are:

./gmg_c/gmg7_c_interface.f90
./gmg_c/gmg7_OWHM_dble.f