# Compiler flags and output naming shared by mf-owhm, zonebudget and hydfmt.
#
# The flag sets mirror the root makefile: Intel Fortran (ifort or ifx) and
# GNU gfortran, each with a Debug and a Release set, optional promotion of
# REAL to 8 bytes (OWHM_DOUBLE) and optional static linking (OWHM_STATIC).
# Other compilers get only the generic CMake defaults.
#
# Functions
#   owhm_fortran_flags(<target> <use_double>)   compile and link options
#   owhm_c_flags(<target>)                       flags for the GMG C sources
#   owhm_output_name(<target> <base> <bin_dir>)  base[-debug][-gmg].nix|.exe
#
include_guard(GLOBAL)

option(OWHM_STATIC "Link the executables statically" ON)

# Note: the default build type (Release) is set by the CMakeLists.txt files
# before project(), because on Windows CMake would otherwise default to Debug.

# Drop the CMake per-configuration default flags (such as -O3 -DNDEBUG) so
# that only the makefile flag sets apply. CMAKE_Fortran_FLAGS itself is kept
# so extra flags can still be passed on the command line.
foreach(_cfg _DEBUG _RELEASE _RELWITHDEBINFO _MINSIZEREL)
  set(CMAKE_Fortran_FLAGS${_cfg} "")
  set(CMAKE_C_FLAGS${_cfg}       "")
endforeach()

function(owhm_fortran_flags target use_double)
  set(_debug)
  set(_release)
  set(_common)
  set(_link)
  if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")     # Intel = ifort, IntelLLVM = ifx
    if(WIN32)
      set(_common  /nologo /assume:nocc_omp,ieee_compares /fpe:0 /fp:source /warn:nousage)
      set(_debug   /Od /debug:full /traceback /check:bounds,pointers,stack,format,output_conversion)   # no uninit check on Windows
      set(_release /O2)
      if(use_double)
        list(APPEND _common /real-size:64)
      endif()
      if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        list(APPEND _common /Qdiag-disable:10448)
      endif()
      if(OWHM_STATIC)
        list(APPEND _common /libs:static /threads)
      endif()
    else()
      set(_common  -nologo -assume nocc_omp,ieee_compares -fpe0 -fp-model source -warn nousage)
      set(_debug   -O0 -g -debug -traceback -check bounds,pointers,stack,format,output_conversion,uninit)
      set(_release -O2 -threads)
      if(use_double)
        list(APPEND _common -real-size 64)
      endif()
      if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        list(APPEND _common -diag-disable=10448)
      endif()
      if(OWHM_STATIC)
        # ifx does not allow -check uninit together with -static
        if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
          list(TRANSFORM _debug REPLACE ",uninit$" "")
        endif()
        set(_link -static -static-intel -qopenmp-link=static -static-libstdc++ -static-libgcc)
      endif()
    endif()
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(_common  -fdefault-double-8 -ffree-line-length-2048)
    set(_debug   -O0 -g -w -fbacktrace -fmax-errors=10 -ffpe-trap=zero,overflow,invalid -finit-real=snan -fcheck=all)
    set(_release -O2 -w -fno-backtrace)
    if(use_double)
      list(APPEND _common -fdefault-real-8)
    endif()
    if(OWHM_STATIC)
      set(_link -static -static-libgfortran -static-libgcc -static-libstdc++)
    endif()
  else()
    message(STATUS "owhm: no flag set for ${CMAKE_Fortran_COMPILER_ID}; using CMake defaults")
  endif()
  target_compile_options(${target} PRIVATE
    $<$<COMPILE_LANGUAGE:Fortran>:${_common}>
    $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:${_debug}>
    $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<NOT:$<CONFIG:Debug>>>:${_release}>)
  target_link_options(${target} PRIVATE ${_link})
endfunction()

function(owhm_c_flags target)
  set(_debug)
  set(_release)
  if(CMAKE_C_COMPILER_ID MATCHES "^Intel")
    if(WIN32)
      set(_debug   /Od /debug:full)
      set(_release /O2)
    else()
      set(_debug   -O0 -g -debug -fbuiltin)
      set(_release -O2 -fbuiltin)
    endif()
  elseif(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    set(_debug   -O0 -g -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast)
    set(_release -O2 -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast)
  endif()
  target_compile_options(${target} PRIVATE
    $<$<AND:$<COMPILE_LANGUAGE:C>,$<CONFIG:Debug>>:${_debug}>
    $<$<AND:$<COMPILE_LANGUAGE:C>,$<NOT:$<CONFIG:Debug>>>:${_release}>)
endfunction()

# Name the binary like the makefile does: base, plus -debug for a Debug
# build, with the .exe extension on Windows and .nix elsewhere, placed in
# bin_dir (no per-configuration subdirectory). The sources are not run
# through the C preprocessor (the Ninja generator would otherwise
# preprocess every file for dependency scanning, which breaks code that
# was never written for cpp); files that need it set Fortran_PREPROCESS
# ON individually.
function(owhm_output_name target base bin_dir)
  if(WIN32)
    set(_ext ".exe")
  else()
    set(_ext ".nix")
  endif()
  set_target_properties(${target} PROPERTIES
    OUTPUT_NAME                      "${base}"
    OUTPUT_NAME_DEBUG                "${base}-debug"
    SUFFIX                           "${_ext}"
    RUNTIME_OUTPUT_DIRECTORY         "${bin_dir}"
    RUNTIME_OUTPUT_DIRECTORY_DEBUG   "${bin_dir}"
    RUNTIME_OUTPUT_DIRECTORY_RELEASE "${bin_dir}"
    Fortran_MODULE_DIRECTORY         "${CMAKE_CURRENT_BINARY_DIR}/mod/${target}"
    Fortran_PREPROCESS               OFF)
endfunction()
