
    Beta Release Executable Branch

The beta branch contains compiled executables using code 
from the "develop" branch. 

For git users/software developers, the beta branch 
is UNSTABLE and drops commits that contain executables 
when a newer beta release is created.

The executables are compiled using the Intel oneAPI toolkit.
Fortran files are compiled using either ifort or ifx.

Historically MODFLOW is compiled using ifort.
Intel support for ifort ends in December, 2024.

A future release will switch mf-owhm.exe to ifx.

The two compilers have similar runtimes, but due to
different floating point models and optimizations 
may yield slightly different results with the same input.

Name             Compiler   OS
------------------------------------------
mf-owhm-debug.exe ifort      Windows x64 Debug version (slow, but more info on crash)

mf-owhm.exe       ifort      Windows x64

mf-owhm.ifx.exe   ifx        Windows x64

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note 1:
   The default name, mf-owhm.exe uses ifort.
   A future release will switch to ifx for the default. 
   A new file will be added called mf-owhm.ifort.exe that uses ifort 
      if it is still possible to get the compiler.
   
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
