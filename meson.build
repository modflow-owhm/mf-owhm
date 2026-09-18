# Meson build for MODFLOW One-Water Hydrologic Flow Model (MF-OWHM)
#
#   meson setup build                          # gfortran or ifort/ifx found on PATH, release
#   meson setup build -Dconfig=debug
#   FC=ifx meson setup build
#   meson setup build -Duse_gmg=true           # include the C GMG solver, needs a C compiler
#   meson compile -C build
#   meson install -C build                     # copies the binaries into bin/ like the makefile
#
# Meson writes the executables into the build directory; "meson install"
# places them in bin/ (and postprocessors/<name>/bin) with the makefile
# names mf-owhm.nix or mf-owhm.exe, plus the -debug and -gmg suffixes.
# All flags come from this file (buildtype=plain), selected by -Dconfig.
# Source lists are the text files in cmake/, shared with CMake.
#
project('mf-owhm', 'fortran',
  version : '2.3.1',
  license : 'CC0-1.0',
  meson_version : '>=1.1',
  default_options : ['buildtype=plain', 'warning_level=0'])

config     = get_option('config')
use_gmg    = get_option('use_gmg')
use_double = get_option('double')
static     = get_option('static')
root_dir   = meson.project_source_root()

# ---------------------------------------------------------------------------
# Compiler flags, mirroring the root makefile for Intel (ifort, ifx) and GNU
# ---------------------------------------------------------------------------
fc  = meson.get_compiler('fortran')
fid = fc.get_id()

fflags  = []   # all configurations
ldflags = []
if fid == 'intel' or fid == 'intel-llvm'
  fflags += ['-nologo', '-assume', 'nocc_omp,ieee_compares', '-fpe0', '-fp-model', 'source', '-warn', 'nousage']
  if fid == 'intel'
    fflags += ['-diag-disable=10448']
  endif
  if config == 'debug'
    checks = 'bounds,pointers,stack,format,output_conversion'
    # ifx does not allow -check uninit together with -static
    if not (fid == 'intel-llvm' and static)
      checks += ',uninit'
    endif
    fflags += ['-O0', '-g', '-debug', '-traceback', '-check', checks]
  else
    fflags += ['-O2', '-threads']
  endif
  if use_double
    fflags += ['-real-size', '64']
  endif
  if static
    ldflags += ['-static', '-static-intel', '-qopenmp-link=static', '-static-libstdc++', '-static-libgcc']
  endif
elif fid == 'intel-cl' or fid == 'intel-llvm-cl'
  fflags += ['/nologo', '/assume:nocc_omp,ieee_compares', '/fpe:0', '/fp:source', '/warn:nousage']
  if fid == 'intel-cl'
    fflags += ['/Qdiag-disable:10448']
  endif
  if config == 'debug'
    fflags += ['/Od', '/debug:full', '/traceback', '/check:bounds,pointers,stack,format,output_conversion']   # no uninit check on Windows
  else
    fflags += ['/O2']
  endif
  if use_double
    fflags += ['/real-size:64']
  endif
  if static
    fflags += ['/libs:static', '/threads']
  endif
elif fid == 'gcc'
  fflags += ['-fdefault-double-8', '-ffree-line-length-2048']
  if config == 'debug'
    fflags += ['-O0', '-g', '-w', '-fbacktrace', '-fmax-errors=10', '-ffpe-trap=zero,overflow,invalid', '-finit-real=snan', '-fcheck=all']
  else
    fflags += ['-O2', '-w', '-fno-backtrace']
  endif
  if use_double
    fflags += ['-fdefault-real-8']
  endif
  if static
    ldflags += ['-static', '-static-libgfortran', '-static-libgcc', '-static-libstdc++']
  endif
else
  message('mf-owhm: no flag set for Fortran compiler "' + fid + '", using Meson defaults')
endif

exe_suffix = host_machine.system() == 'windows' ? 'exe' : 'nix'
dbg_suffix = config == 'debug' ? '-debug' : ''

# ---------------------------------------------------------------------------
# Sources: the lists are the text files in cmake/ (one path per line,
# relative to the repository root, # comments), shared with CMake so a new
# source file is added in one place.
# ---------------------------------------------------------------------------
fs = import('fs')

bif_sources   = []
main_sources  = []
gmg_sources   = []
nogmg_sources = []
foreach pair : [['bif', 'bif-source.txt'], ['main', 'owhm-source.txt'], ['gmg', 'gmg-source.txt'], ['nogmg', 'nogmg-source.txt']]
  list_files = []
  foreach line : fs.read('cmake' / pair[1]).split('\n')
    line = line.strip()
    if line != '' and not line.startswith('#')
      list_files += line
    endif
  endforeach
  if pair[0] == 'bif'
    bif_sources = files(list_files)
  elif pair[0] == 'main'
    main_sources = files(list_files)
  elif pair[0] == 'gmg'
    gmg_sources = files(list_files)
  else
    nogmg_sources = files(list_files)
  endif
endforeach

gmg_inc = []
gmg_c_args = []
if use_gmg
  add_languages('c', native : false)
  cc  = meson.get_compiler('c')
  cid = cc.get_id()
  if cid == 'intel' or cid == 'intel-llvm'
    gmg_c_args += config == 'debug' ? ['-O0', '-g', '-debug', '-fbuiltin'] : ['-O2', '-fbuiltin']
  elif cid == 'intel-cl' or cid == 'intel-llvm-cl'
    gmg_c_args += config == 'debug' ? ['/Od', '/debug:full'] : ['/O2']
  else
    gmg_c_args += config == 'debug' ? ['-O0', '-g'] : ['-O2']
    gmg_c_args += ['-Wno-int-to-pointer-cast', '-Wno-pointer-to-int-cast']
  endif
  gmg_inc = include_directories('src/gmg_c')
  owhm_name = 'mf-owhm-gmg'
else
  gmg_sources = nogmg_sources
  owhm_name = 'mf-owhm'
endif

executable(owhm_name + dbg_suffix,
  bif_sources, main_sources, gmg_sources,
  fortran_args : fflags,
  c_args : gmg_c_args,
  link_args : ldflags,
  include_directories : gmg_inc,
  name_suffix : exe_suffix,
  install : true,
  install_dir : root_dir / 'bin')

# ---------------------------------------------------------------------------
# Postprocessors (REAL stays 4 bytes, as in their own makefiles)
# ---------------------------------------------------------------------------
pp_flags = []
foreach f : fflags
  if f != '-fdefault-real-8' and f != '-real-size' and f != '64' and f != '/real-size:64'
    pp_flags += f
  endif
endforeach

foreach pp : ['zonebudget', 'hydfmt']
  if get_option('build_' + pp)
    pp_files = []
    foreach line : fs.read('cmake' / (pp + '-source.txt')).split('\n')
      line = line.strip()
      if line != '' and not line.startswith('#')
        pp_files += line
      endif
    endforeach
    executable(pp + dbg_suffix,
      files(pp_files),
      fortran_args : pp_flags,
      link_args : ldflags,
      name_suffix : exe_suffix,
      install : true,
      install_dir : root_dir / 'postprocessors' / pp / 'bin')
  endif
endforeach
