# Meson build for ZoneBudget, a MODFLOW-OWHM postprocessor, as a standalone project.
# (The root meson.build also builds it, option build_zonebudget.)
#
#   meson setup build            # in this directory
#   meson compile -C build
#   meson install -C build       # copies zonebudget.nix (or .exe) into bin/
#
project('zonebudget', 'fortran',
  meson_version : '>=1.1',
  default_options : ['buildtype=plain', 'warning_level=0'])

config     = get_option('config')
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

executable('zonebudget' + dbg_suffix, files('src/zonebudget.f'),
  fortran_args : fflags,
  link_args : ldflags,
  name_suffix : exe_suffix,
  install : true,
  install_dir : root_dir / 'bin')
