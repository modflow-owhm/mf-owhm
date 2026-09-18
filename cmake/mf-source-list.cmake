# Reads the shared source lists in this directory into CMake variables.
#
# The lists are plain text files (one path per line, relative to the
# repository root, # comments) shared with meson.build:
#
#   bif-source.txt        -> OWHM_BIF_SOURCES
#   owhm-source.txt       -> OWHM_MAIN_SOURCES
#   gmg-source.txt        -> OWHM_GMG_SOURCES       (C solver plus its Fortran driver)
#   nogmg-source.txt      -> OWHM_NOGMG_SOURCES     (Fortran stub used without GMG)
#   zonebudget-source.txt -> OWHM_ZONEBUDGET_SOURCES
#   hydfmt-source.txt     -> OWHM_HYDFMT_SOURCES
#
# The paths are returned as given (relative to the repository root), so the
# caller prefixes them with the repository directory when needed.
#
include_guard(GLOBAL)

function(owhm_read_source_list out_var list_file)
  file(STRINGS "${list_file}" _lines)
  set(_result)
  foreach(_line IN LISTS _lines)
    string(STRIP "${_line}" _line)
    if(_line STREQUAL "" OR _line MATCHES "^#")
      continue()
    endif()
    list(APPEND _result "${_line}")
  endforeach()
  set(${out_var} "${_result}" PARENT_SCOPE)
endfunction()

owhm_read_source_list(OWHM_BIF_SOURCES        "${CMAKE_CURRENT_LIST_DIR}/bif-source.txt")
owhm_read_source_list(OWHM_MAIN_SOURCES       "${CMAKE_CURRENT_LIST_DIR}/owhm-source.txt")
owhm_read_source_list(OWHM_GMG_SOURCES        "${CMAKE_CURRENT_LIST_DIR}/gmg-source.txt")
owhm_read_source_list(OWHM_NOGMG_SOURCES      "${CMAKE_CURRENT_LIST_DIR}/nogmg-source.txt")
owhm_read_source_list(OWHM_ZONEBUDGET_SOURCES "${CMAKE_CURRENT_LIST_DIR}/zonebudget-source.txt")
owhm_read_source_list(OWHM_HYDFMT_SOURCES     "${CMAKE_CURRENT_LIST_DIR}/hydfmt-source.txt")

# Reconfigure when a list changes
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${CMAKE_CURRENT_LIST_DIR}/bif-source.txt"
  "${CMAKE_CURRENT_LIST_DIR}/owhm-source.txt"
  "${CMAKE_CURRENT_LIST_DIR}/gmg-source.txt"
  "${CMAKE_CURRENT_LIST_DIR}/nogmg-source.txt"
  "${CMAKE_CURRENT_LIST_DIR}/zonebudget-source.txt"
  "${CMAKE_CURRENT_LIST_DIR}/hydfmt-source.txt")
