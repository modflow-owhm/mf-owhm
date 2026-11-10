# CMake build for MODFLOW One-Water Hydrologic Flow Model (MF-OWHM)
#
#   cmake -S . -B build                      # gfortran or ifort/ifx found on PATH, Release
#   cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug
#   cmake -S . -B build -DCMAKE_BUILD_TYPE=Fast       # no floating-point traps, AVX2 code -> bin/mf-owhm-fast (see doc/BUILD.md)
#   cmake -S . -B build -DCMAKE_BUILD_TYPE=FastPGOGen # then run a model with bin/mf-owhm-pgo-gen, then
#   cmake -S . -B build -DCMAKE_BUILD_TYPE=FastPGO    # profile-guided Fast build -> bin/mf-owhm-pgo (ifx only; -DOWHM_PROFDIR=<dir>)
#   cmake -S . -B build -DCMAKE_Fortran_COMPILER=ifx
#   cmake -S . -B build -DOWHM_USE_GMG=ON    # include the C GMG solver, needs a C compiler
#   cmake --build build
#
# Windows, from a command prompt with the oneAPI environment loaded:
#   cmake -S . -B build\vs2026 -G "Visual Studio 18 2026" -A x64 -T fortran=ifx     (ifx)
#   cmake -S . -B build\vs2022 -G "Visual Studio 17 2022" -A x64 -T fortran=ifort   (classic ifort, oneAPI 2024.2)
#   cmake --build build\vs2022 --config Release
#   Visual Studio 18 writes a .slnx solution that cmake --build cannot yet pass to devenv;
#   build it with  devenv.com build\vs2026\mf-owhm.slnx /Build "Release|x64"  or in the IDE.
#
# The binary is written to bin/ with the same names as the makefile:
# mf-owhm.nix (Linux/macOS) or mf-owhm.exe (Windows), with -gmg for the GMG
# variant followed by -debug, -fast, -pgo-gen or -pgo (mf-owhm-gmg-pgo). The source lists are the text files in cmake/, shared
# with meson.build; the compiler flags are in cmake/CompilerFlagInput.cmake. The postprocessors zonebudget and hydfmt are built by
# default into postprocessors/<name>/bin and can be switched off.
#
cmake_minimum_required(VERSION 3.20)

# Default a single configuration generator to Release, as the makefile does.
# This must happen before project(), which on Windows would otherwise pick Debug.
if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type: Release, Debug, Fast, FastPGOGen or FastPGO")
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Release Debug Fast FastPGOGen FastPGO)
endif()

project(mf-owhm
  VERSION     2.3.1
  DESCRIPTION "MODFLOW One-Water Hydrologic Flow Model"
  HOMEPAGE_URL "https://code.usgs.gov/modflow/mf-owhm"
  LANGUAGES   Fortran)

# Multi-configuration generators (Visual Studio): offer the Fast configurations too
if(CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release;Fast;FastPGOGen;FastPGO" CACHE STRING "" FORCE)
endif()

option(OWHM_USE_GMG          "Compile the GMG solver (C code); otherwise a stub stops the model if GMG is requested" OFF)
option(OWHM_DOUBLE           "Promote default REAL to 8 bytes" ON)
option(OWHM_BUILD_ZONEBUDGET "Build the ZoneBudget postprocessor" ON)
option(OWHM_BUILD_HYDFMT     "Build the HydFMT postprocessor" ON)
set(OWHM_BIN_DIR "${PROJECT_SOURCE_DIR}/bin" CACHE PATH "Directory that receives the mf-owhm binary")

list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")
include(CompilerFlagInput)
include(mf-source-list)

set(_sources ${OWHM_BIF_SOURCES} ${OWHM_MAIN_SOURCES})
set(_base mf-owhm)
if(OWHM_USE_GMG)
  enable_language(C)
  list(APPEND _sources ${OWHM_GMG_SOURCES})
  set(_base mf-owhm-gmg)
else()
  list(APPEND _sources ${OWHM_NOGMG_SOURCES})
endif()

add_executable(mf-owhm ${_sources})

# The only preprocessed Fortran file (#ifdef __INTEL_COMPILER, __unix__)
set_source_files_properties(src/bif_lib/io/generic_open_interface.fpp
  PROPERTIES Fortran_PREPROCESS ON)

owhm_fortran_flags(mf-owhm ${OWHM_DOUBLE})
owhm_pgo_flags(mf-owhm ${_base})
if(OWHM_USE_GMG)
  owhm_c_flags(mf-owhm)
  target_include_directories(mf-owhm PRIVATE src/gmg_c)
endif()
owhm_output_name(mf-owhm ${_base} "${OWHM_BIN_DIR}")
# Fast and FastPGO binaries are named like the makefile builds (the postprocessors keep their names)
set_target_properties(mf-owhm PROPERTIES
  OUTPUT_NAME_FAST    "${_base}-fast"
  OUTPUT_NAME_FASTPGO "${_base}-pgo")

if(OWHM_BUILD_ZONEBUDGET)
  add_subdirectory(postprocessors/zonebudget)
endif()
if(OWHM_BUILD_HYDFMT)
  add_subdirectory(postprocessors/hydfmt)
endif()
