# CMake build for HydFMT, a MODFLOW-OWHM postprocessor.
#
# Built as part of the root mf-owhm project (option OWHM_BUILD_HYDFMT)
# or on its own:
#   cmake -S postprocessors/hydfmt -B postprocessors/hydfmt/build
#   cmake --build postprocessors/hydfmt/build
#
# The binary is written to postprocessors/hydfmt/bin/hydfmt.nix (.exe on
# Windows), with the -debug suffix for a Debug build, like the makefile.
#
cmake_minimum_required(VERSION 3.20)

# Default a single configuration generator to Release, as the makefile does.
# This must happen before project(), which on Windows would otherwise pick Debug.
if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type: Release or Debug")
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Release Debug)
endif()

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  project(hydfmt LANGUAGES Fortran)
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")
endif()
include(CompilerFlagInput)
include(mf-source-list)

option(OWHM_HYDFMT_DOUBLE "HydFMT: promote default REAL to 8 bytes" OFF)

# Paths in the shared list are relative to the repository root
list(TRANSFORM OWHM_HYDFMT_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../../")
add_executable(hydfmt ${OWHM_HYDFMT_SOURCES})
owhm_fortran_flags(hydfmt ${OWHM_HYDFMT_DOUBLE})
owhm_output_name(hydfmt hydfmt "${CMAKE_CURRENT_SOURCE_DIR}/bin")
