# CMake build for ZoneBudget, a MODFLOW-OWHM postprocessor.
#
# Built as part of the root mf-owhm project (option OWHM_BUILD_ZONEBUDGET)
# or on its own:
#   cmake -S postprocessors/zonebudget -B postprocessors/zonebudget/build
#   cmake --build postprocessors/zonebudget/build
#
# The binary is written to postprocessors/zonebudget/bin/zonebudget.nix (.exe on
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
  project(zonebudget LANGUAGES Fortran)
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")
endif()
include(CompilerFlagInput)
include(mf-source-list)

option(OWHM_ZONEBUDGET_DOUBLE "ZoneBudget: promote default REAL to 8 bytes" OFF)

# Paths in the shared list are relative to the repository root
list(TRANSFORM OWHM_ZONEBUDGET_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../../")
add_executable(zonebudget ${OWHM_ZONEBUDGET_SOURCES})
owhm_fortran_flags(zonebudget ${OWHM_ZONEBUDGET_DOUBLE})
owhm_output_name(zonebudget zonebudget "${CMAKE_CURRENT_SOURCE_DIR}/bin")
