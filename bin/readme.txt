
    Windows and Ubuntu compiled executables

The executables are compiled using the Intel oneAPI toolkit.
Fortran files are compiled using either ifort or ifx 
and C code is compiled using icx.

Historically MODFLOW is compiled using ifort.
Intel support for ifort ends in December, 2024.

A future release will switch mf-owhm.exe to ifx.
Currently there is an issue with ifx because it does not allow
for NaN compares, which is used extensively in MODFLOW-OWHM.
That is, 
    x=NaN
    then 
    x == x  is always False for NaN

IFX should accept the compiler flag, /assume:nan_compares 
but it does not seem to fix the final executable to allow it.
Instead I had to change the floating point model to all all NaN
operations (ie, changing fpe0 to fpe1).

The two compilers have similar runtimes, but due to
different floating point models and optimizations 
may yield slightly different results with the same input.

Name             Compiler   OS
------------------------------------------
mf-owhm.exe      ifort      Windows x64

mf-owhm.ifx.exe  ifx        Windows x64

mf-owhm-gmg.exe  ifort,icx  Windows x64

mf-owhm.nix      ifort      Ubuntu/Debian flavor Linux x64 (may work depending on runtime, otherwise use makefile to recompile)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note 1:
   The default name, mf-owhm.exe uses ifort.
   A future release will switch to ifx for the default. 
   A new file will be added called mf-owhm.ifort.exe that uses ifort 
      if it is still possible to get the compiler.
   
Note 2:
   In MODFLOW-OWHM:
      Windows files end with ".exe"
      Ubuntu Linux/Unix files end with ".nix".

Note 3:
   The Unix file may need to have the executable bit set 
   for the program to be executable, via: chmod +x mf-owhm.nix
   or any related chmod/GUI option that works for your needs.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
