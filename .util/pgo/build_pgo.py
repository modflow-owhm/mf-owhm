#!/usr/bin/env python3
"""
build_pgo.py - profile-guided MF-OWHM build in one step (Intel ifx only).

The script runs the two stages of the fast-pgo build and the training runs
between them:

  1. builds the instrumented binary (bin/<name>-pgo-gen.exe|.nix),
  2. runs every model listed in the ini file with it, each in its own
     working folder (each run adds to the profile in the profile directory),
  3. merges the raw profiles with llvm-profdata,
  4. builds the final binary bin/<name>.exe|.nix with the merged profile.

The build engine is GNU make (make CONFIG=fast-pgo-gen, then CONFIG=fast-pgo)
on Linux, macOS and msys2, and the Visual Studio solution (devenv.com with
the Fast_PGO_Gen and Fast_PGO configurations) on Windows. Everything is
controlled by an ini file (default: pgo.ini next to this script); a few
settings can be overridden on the command line.

Usage (from any directory, the repository root is found from the script):

  python3 .util/pgo/build_pgo.py                        #   Linux: pgo.ini -> bin/mf-owhm.nix
  py      .util/pgo/build_pgo.py                        # Windows: pgo.ini -> bin/mf-owhm.exe
  python3 .util/pgo/build_pgo.py my.ini --name mf-owhm-pgo --gmg
  python3 .util/pgo/build_pgo.py --dry-run              # print the commands only

Needs Python 3.8 or newer and nothing outside the standard library.
"""
import argparse
import configparser
import glob
import os
import platform
import shlex
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent          # <repo>/.util/pgo/build_pgo.py -> <repo>/.util/pgo/
ROOT = HERE.parent.parent                       # <repo>/.util/pgo/             -> <repo>/
IS_WIN = os.name == 'nt'
EXT = '.exe' if IS_WIN else '.nix'

DEFAULTS = {
    'build': {'name': 'mf-owhm', 'gmg': 'no', 'engine': 'auto', 'profdir': 'obj/pgo',
              'keep_gen': 'no', 'log': 'obj/pgo/build_pgo.log'},
    'make':  {'args': '', 'oneapi': 'auto'},
    'vs':    {'devenv': 'auto', 'msbuild': 'auto', 'oneapi': 'auto'},
    'train': {'models': '', 'run_dir': ''},
}


class Fail(Exception):
    pass


class Log:
    """Console and log file; the log file keeps the full output of every command."""

    def __init__(self, path, dry):
        self.path = path
        self.f = None
        if not dry:
            path.parent.mkdir(parents=True, exist_ok=True)
            self.f = open(path, 'w', encoding='utf-8', errors='replace')

    def __call__(self, msg=''):
        print(msg, flush=True)
        if self.f:
            self.f.write(msg + '\n')
            self.f.flush()

    def raw(self, text):
        if self.f:
            self.f.write(text)
            self.f.flush()


def yes(s):
    return str(s).strip().lower() in ('yes', 'y', 'true', '1', 'on')


def fmt_cmd(cmd):
    return ' '.join(shlex.quote(str(c)) if not IS_WIN else
                    (f'"{c}"' if ' ' in str(c) else str(c)) for c in cmd)


def elapsed(t0):
    s = time.time() - t0
    return f'{int(s // 60)}:{s % 60:04.1f}' if s >= 60 else f'{s:.1f} s'


class Builder:
    def __init__(self, cfg, args):
        self.dry = args.dry_run
        b = cfg['build']
        self.gmg = args.gmg or yes(b['gmg'])
        self.name = args.name or b['name'].strip()
        if self.gmg and not args.name and self.name == 'mf-owhm':
            self.name = 'mf-owhm-gmg'
        engine = (args.engine or b['engine']).strip().lower()
        if engine == 'auto':
            engine = 'vs' if IS_WIN else 'make'
        if engine not in ('make', 'vs'):
            raise Fail(f'engine must be make, vs or auto, not "{engine}"')
        if engine == 'vs' and not IS_WIN:
            raise Fail('engine = vs needs Windows')
        self.engine = engine
        self.profdir = (ROOT / (args.profdir or b['profdir'].strip())).resolve()
        if engine == 'vs':
            fixed = (ROOT / 'obj' / 'pgo').resolve()
            if self.profdir != fixed:
                print(f'note: the Visual Studio projects write the profile to {fixed}; profdir ignored')
            self.profdir = fixed
        self.keep_gen = args.keep_gen or yes(b['keep_gen'])
        self.log = Log(ROOT / b['log'].strip(), self.dry)
        self.cfg = cfg
        self.models = self.parse_models(cfg['train']['models'], cfg['train']['run_dir'])
        if not self.models:
            raise Fail('no training model listed under [train] models')
        self.times = []
        self.vs_name = 'mf-owhm-gmg' if self.gmg else 'mf-owhm'   # TargetName fixed in the .vfproj
        self.setvars = None                                       # Linux: setvars.sh to source
        self.gen_bin = None
        self.final_bin = ROOT / 'bin' / (self.name + EXT)

    # ------------------------------------------------------------------ helpers
    @staticmethod
    def parse_models(models_text, run_dir_text):
        """[train] models and run_dir: line i of run_dir is the working folder of
        model i; NAME_DIR, or a missing line, means the folder of the name file.
        Returns (name file, extra arguments, working folder) per model."""
        def lines(text):
            return [ln.strip() for ln in text.splitlines() if ln.strip() and ln.strip()[0] not in ';#']
        names, dirs = lines(models_text), lines(run_dir_text)
        if len(dirs) > len(names):
            raise Fail(f'[train] run_dir has {len(dirs)} lines but models only {len(names)}')
        models = []
        for i, line in enumerate(names):
            parts = shlex.split(line, posix=not IS_WIN)
            nam = Path(parts[0].strip('"'))
            if not nam.is_absolute():
                nam = ROOT / nam
            rd = dirs[i].strip('"') if i < len(dirs) else 'NAME_DIR'
            if rd.upper() == 'NAME_DIR':
                run_dir = nam.parent
            else:
                run_dir = Path(rd)
                if not run_dir.is_absolute():
                    run_dir = ROOT / run_dir
            models.append((nam, [p.strip('"') for p in parts[1:]], run_dir))
        return models

    @staticmethod
    def nam_arg(nam, run_dir):
        """The name file as passed on the command line of a run in run_dir."""
        if run_dir == nam.parent:
            return nam.name
        try:
            return os.path.relpath(nam, run_dir)
        except ValueError:                      # Windows: another drive
            return str(nam)

    def run(self, cmd, cwd=ROOT, env_shell=False, check=True):
        """Run a command; its full output goes to the log file. env_shell wraps the
        command in a bash that sources the oneAPI setvars.sh first (make engine)."""
        if env_shell and self.setvars:
            cmd = ['bash', '-c', 'source "$1" >/dev/null 2>&1; shift; exec "$@"', 'pgo',
                   str(self.setvars)] + [str(c) for c in cmd]
        self.log(f'$ {fmt_cmd(cmd)}' + ('' if cwd == ROOT else f'    (in {cwd})'))
        if self.dry:
            return subprocess.CompletedProcess(cmd, 0, '', '')
        t0 = time.time()
        p = subprocess.run([str(c) for c in cmd], cwd=str(cwd), stdout=subprocess.PIPE,
                           stderr=subprocess.STDOUT, text=True, errors='replace',
                           preexec_fn=None if IS_WIN else unlimited_stack)
        self.log.raw(p.stdout)
        if check and p.returncode != 0:
            tail = '\n'.join(p.stdout.rstrip().splitlines()[-15:])
            raise Fail(f'command failed (exit {p.returncode}) after {elapsed(t0)}:\n'
                       f'  {fmt_cmd(cmd)}\n{tail}\n(full output in {self.log.path})')
        return p

    def clean_dir(self, d):
        d = Path(d)
        self.log(f'clean {d}')
        if not self.dry and d.exists():
            shutil.rmtree(d)

    def expect_file(self, path, since):
        """path must exist and be newer than the time stamp since (the build start)."""
        if self.dry:
            return
        p = Path(path)
        if not p.exists():
            raise Fail(f'{p} was not produced')
        if p.stat().st_mtime < since - 2:
            raise Fail(f'{p} is older than this build; the build did not replace it')

    # ------------------------------------------------------------------- stages
    def build(self):
        t_all = time.time()
        self.log(f'build_pgo.py  {time.strftime("%Y-%m-%d %H:%M:%S")}  {platform.platform()}')
        self.log(f'repository {ROOT}')
        self.log(f'engine {self.engine}   final binary bin/{self.name}{EXT}   gmg {"yes" if self.gmg else "no"}')
        self.log(f'profile directory {self.profdir}')
        self.log(f'training models ({len(self.models)}):')
        for nam, extra, run_dir in self.models:
            self.log(f'  {nam} {" ".join(extra)}'.rstrip() +
                     ('' if run_dir == nam.parent else f'    (run in {run_dir})'))
            if not self.dry and not nam.is_file():
                raise Fail(f'training model not found: {nam}')
            if not self.dry and not run_dir.is_dir():
                raise Fail(f'run folder not found: {run_dir}  (for {nam})')
        self.log()
        if self.engine == 'make':
            self.setup_make()
        else:
            self.setup_vs()
        # stage 1
        self.log('==== stage 1: instrumented build')
        t0 = time.time()
        self.stage_gen()
        self.log(f'==== instrumented binary {self.gen_bin}  ({elapsed(t0)})\n')
        # training
        self.log('==== training runs')
        self.train()
        self.log()
        # merge
        self.log('==== profile merge')
        profdata = self.merge()
        self.log()
        # stage 2
        self.log('==== stage 2: optimized build')
        t0 = time.time()
        self.stage_use(profdata)
        self.log(f'==== final binary {self.final_bin}  ({elapsed(t0)})\n')
        if not self.keep_gen and self.gen_bin and not self.dry and self.gen_bin.exists():
            self.gen_bin.unlink()
            self.log(f'removed {self.gen_bin}')
        if self.engine == 'vs' and not self.dry:     # export files of the instrumented link
            for f in (ROOT / 'bin').glob(f'{self.vs_name}-pgo-gen.*'):
                if f.suffix.lower() in ('.exp', '.lib'):
                    f.unlink()
        self.log(f'done in {elapsed(t_all)}')
        self.log(f'  binary   {self.final_bin}' +
                 ('' if self.dry else f'  ({self.final_bin.stat().st_size / 1e6:.1f} MB)'))
        self.log(f'  profile  {profdata}')
        for nam, t in self.times:
            self.log(f'  trained  {t:>8}  {nam}')
        if self.log.f:
            self.log(f'  log      {self.log.path}')

    # --------------------------------------------------------------------- make
    def setup_make(self):
        m = self.cfg['make']
        self.make_args = shlex.split(m['args'])
        self.f90 = next((a.split('=', 1)[1] for a in self.make_args if a.upper().startswith('F90=')), 'ifx')
        if os.path.basename(self.f90).lower() != 'ifx':
            raise Fail(f'the profile-guided build needs ifx, not {self.f90} (make args: {m["args"]})')
        if not shutil.which(self.f90):
            self.setvars = self.find_setvars(m['oneapi'])
            if not self.setvars:
                raise Fail(f'{self.f90} is not on PATH and no oneAPI setvars.sh was found; set '
                           '"oneapi" under [make] to the path of setvars.sh (or source it first)')
            self.log(f'sourcing {self.setvars} for every make command')
        self.make = shutil.which('make') or shutil.which('gmake') or 'make'
        self.ifx_dir = self.query('command -v ' + shlex.quote(self.f90))
        if not self.ifx_dir and not self.dry:
            raise Fail(f'{self.f90} not found even after sourcing {self.setvars}')
        self.ifx_dir = Path(self.ifx_dir).parent if self.ifx_dir else None
        self.log(f'compiler {self.ifx_dir / self.f90 if self.ifx_dir else self.f90}')
        ver = self.query(shlex.quote(self.f90) + ' --version | head -n 1')
        if ver:
            self.log(f'  {ver}')

    def query(self, shell_cmd):
        """Run a shell command inside the compiler environment, return its stdout."""
        if self.dry:
            return ''
        cmd = ['bash', '-c', shell_cmd]
        if self.setvars:
            cmd = ['bash', '-c', f'source "{self.setvars}" >/dev/null 2>&1; {shell_cmd}']
        p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)
        return p.stdout.strip()

    @staticmethod
    def find_setvars(opt):
        opt = (opt or 'auto').strip()
        cands = []
        if opt.lower() != 'auto':
            p = Path(opt).expanduser()
            cands = [p, p / 'setvars.sh']
        else:
            root = os.environ.get('ONEAPI_ROOT')
            if root:
                cands.append(Path(root) / 'setvars.sh')
            cands += [Path('/opt/intel/oneapi/setvars.sh'), Path.home() / 'intel/oneapi/setvars.sh']
        return next((c for c in cands if c.is_file()), None)

    def make_cmd(self, config, int_dir):
        # no -j: the makefile compiles in list order and has no module dependencies
        cmd = [self.make, 'compile', f'CONFIG={config}', f'PROGRAM={self.name}',
               f'PROFDIR={self.profdir}', f'int_dir={int_dir}']
        if self.gmg:
            cmd.append('USEGMG=YES')
        return cmd + self.make_args

    def stage_gen(self):
        if self.engine == 'vs':
            return self.vs_build('Fast_PGO_Gen', gen=True)
        int_dir = ROOT / 'obj' / f'fast-pgo-gen_{self.f90}'
        self.clean_dir(int_dir)
        self.gen_bin = ROOT / 'bin' / f'{self.name}-pgo-gen{EXT}'
        t0 = time.time()
        self.run(self.make_cmd('fast-pgo-gen', int_dir), env_shell=True)
        self.expect_file(self.gen_bin, t0)

    def stage_use(self, profdata):
        if self.engine == 'vs':
            return self.vs_build('Fast_PGO', gen=False)
        int_dir = ROOT / 'obj' / f'fast-pgo_{self.f90}'
        self.clean_dir(int_dir)
        t0 = time.time()
        self.run(self.make_cmd('fast-pgo', int_dir), env_shell=True)
        self.expect_file(self.final_bin, t0)

    # ----------------------------------------------------------- visual studio
    def setup_vs(self):
        v = self.cfg['vs']
        self.devenv = self.find_vs_tool(v['devenv'], 'Common7/IDE/devenv.com')
        self.msbuild = self.find_vs_tool(v['msbuild'], 'MSBuild/Current/Bin/MSBuild.exe') if self.gmg else None
        self.oneapi = self.find_oneapi_win(v['oneapi'])
        self.sln = ROOT / 'ide' / 'visual_studio' / ('OneWater_GMG_Project.sln' if self.gmg else 'OneWater_Project.sln')
        self.vfproj = ROOT / 'ide' / 'visual_studio' / f'{self.vs_name}.vfproj'
        self.log(f'devenv   {self.devenv}')
        if self.msbuild:
            self.log(f'msbuild  {self.msbuild}')
        self.log(f'oneAPI   {self.oneapi}')
        self.log(f'solution {self.sln}')

    def find_vs_tool(self, opt, rel):
        opt = (opt or 'auto').strip()
        if opt.lower() != 'auto':
            p = Path(opt)
            if p.is_file():
                return p
            raise Fail(f'{p} not found (set under [vs])')
        # newest Visual Studio: vswhere first, then the standard install folders
        pf86 = os.environ.get('ProgramFiles(x86)', r'C:\Program Files (x86)')
        pf = os.environ.get('ProgramFiles', r'C:\Program Files')
        vswhere = Path(pf86) / 'Microsoft Visual Studio' / 'Installer' / 'vswhere.exe'
        bases = []
        if vswhere.is_file():
            p = subprocess.run([str(vswhere), '-latest', '-products', '*', '-property', 'installationPath'],
                               stdout=subprocess.PIPE, text=True)
            bases = [Path(x.strip()) for x in p.stdout.splitlines() if x.strip()]
        for d in (pf, pf86):
            found = [Path(x).parents[2] for x in glob.glob(f'{d}/Microsoft Visual Studio/*/*/Common7/IDE/devenv.com')]
            bases += sorted(found, key=lambda x: x.stat().st_mtime, reverse=True)
        for base in bases:
            tool = base / rel
            if tool.is_file():
                return tool
        if self.dry:
            return Path(rel)
        raise Fail(f'{Path(rel).name} not found; set it under [vs] (devenv = C:\\...\\Common7\\IDE\\devenv.com)')

    def find_oneapi_win(self, opt):
        opt = (opt or 'auto').strip()
        cands = []
        if opt.lower() != 'auto':
            cands = [Path(opt)]
        else:
            root = os.environ.get('ONEAPI_ROOT')
            if root:
                cands.append(Path(root) / 'compiler' / 'latest')
            cands.append(Path(os.environ.get('ProgramFiles(x86)', r'C:\Program Files (x86)')) /
                         'Intel' / 'oneAPI' / 'compiler' / 'latest')
        for c in cands:
            if (c / 'bin' / 'compiler' / 'llvm-profdata.exe').is_file():
                return c
        if self.dry:
            return cands[0]
        raise Fail('llvm-profdata.exe not found; set "oneapi" under [vs] to the oneAPI compiler folder '
                   r'(for example C:\Program Files (x86)\Intel\oneAPI\compiler\2026.1)')

    def vs_build(self, config, gen):
        """devenv /Rebuild of the Fortran project. The projects always write
        bin\\mf-owhm[-gmg][-pgo-gen].exe; when another name was asked for, an
        existing binary of the fixed name is moved aside and put back afterwards."""
        fixed = ROOT / 'bin' / (self.vs_name + ('-pgo-gen' if gen else '') + '.exe')
        wanted = ROOT / 'bin' / (self.name + ('-pgo-gen' if gen else '') + '.exe')
        backup = None
        if wanted != fixed and fixed.exists() and not self.dry:
            backup = fixed.with_suffix('.exe.before-pgo')
            fixed.replace(backup)
        try:
            if self.gmg and gen:      # devenv (VS 2026) skips GMG.vcxproj from the command line
                self.run([self.msbuild, ROOT / 'ide' / 'visual_studio' / 'GMG.vcxproj', '/nologo', '/v:m',
                          '/p:Configuration=Release', '/p:Platform=x64'])
            t0 = time.time()
            self.run([self.devenv, self.sln, '/Rebuild', f'{config}|x64', '/Project', self.vfproj])
            self.expect_file(fixed, t0)
            if wanted != fixed and not self.dry:
                fixed.replace(wanted)
        finally:
            if backup and backup.exists():
                backup.replace(fixed)
        if gen:
            self.gen_bin = wanted
        return wanted

    # --------------------------------------------------------------- training
    def profraw_files(self):
        return sorted(self.profdir.glob(f'{self.profile_name()}-*.profraw'))

    def train(self):
        self.profdir.mkdir(parents=True, exist_ok=True)
        stale = self.profraw_files() + [self.profdir / f'{self.profile_name()}.profdata']
        for f in stale:
            if f.exists():
                self.log(f'remove old {f}')
                if not self.dry:
                    f.unlink()
        for nam, extra, run_dir in self.models:
            t0 = time.time()
            before = {f: f.stat().st_mtime for f in self.profraw_files()} if not self.dry else {}
            self.run([self.gen_bin, self.nam_arg(nam, run_dir)] + extra, cwd=run_dir)
            if not self.dry:
                after = {f: f.stat().st_mtime for f in self.profraw_files()}
                if after == before:
                    raise Fail(f'no profile was written by the run of {nam}; the instrumented binary '
                               f'should write {self.profdir}/{self.profile_name()}-<id>.profraw')
            self.times.append((nam.relative_to(ROOT) if ROOT in nam.parents else nam, elapsed(t0)))
            self.log(f'  {elapsed(t0):>8}  {nam.name}  in {run_dir}')

    def profile_name(self):
        return self.name if self.engine == 'make' else self.vs_name

    def merge(self):
        raws = self.profraw_files()
        if not raws and not self.dry:
            raise Fail(f'no .profraw files in {self.profdir}')
        profdata = self.profdir / f'{self.profile_name()}.profdata'
        if self.engine == 'vs':
            tool = self.oneapi / 'bin' / 'compiler' / 'llvm-profdata.exe'
        else:
            tool = (self.ifx_dir / 'compiler' / 'llvm-profdata') if self.ifx_dir else None
            if not tool or not tool.is_file():
                tool = self.query('command -v llvm-profdata') or 'llvm-profdata'
        self.log(f'{len(raws)} raw profile file(s)')
        self.run([tool, 'merge', f'-output={profdata}'] + raws, env_shell=True)
        self.expect_file(profdata, time.time() - 60)
        return profdata


# Eventually this will be removed once stack arrays become allocatable
def unlimited_stack():
    """Linux: lift the 8 MB stack limit for the model runs (large models need it)."""
    try:
        import resource
        soft, hard = resource.getrlimit(resource.RLIMIT_STACK)
        resource.setrlimit(resource.RLIMIT_STACK, (hard, hard))
    except Exception:
        pass


def main():
    ap = argparse.ArgumentParser(description='Profile-guided MF-OWHM build: instrumented build, '
                                 'training runs, profile merge, optimized build.')
    ap.add_argument('ini', nargs='?', default=str(HERE / 'pgo.ini'), help='settings file (default: pgo.ini next to the script)')
    ap.add_argument('--name', help='base name of the final binary (bin/<name>.exe|.nix)')
    ap.add_argument('--gmg', action='store_true', help='build the GMG solver variant')
    ap.add_argument('--engine', choices=['make', 'vs', 'auto'], help='build engine (default from the ini, auto = make, vs on Windows)')
    ap.add_argument('--profdir', help='profile directory (make engine)')
    ap.add_argument('--keep-gen', action='store_true', help='keep the instrumented binary')
    ap.add_argument('--dry-run', action='store_true', help='print the commands without running them')
    args = ap.parse_args()

    ini = Path(args.ini)
    if not ini.is_file():
        sys.exit(f'settings file not found: {ini}')
    cfg = configparser.ConfigParser(interpolation=None, inline_comment_prefixes=(';', '#'))
    cfg.read_dict(DEFAULTS)
    cfg.read(ini, encoding='utf-8')
    for sec in cfg.sections():
        if sec not in DEFAULTS:
            sys.exit(f'{ini}: unknown section [{sec}]')
        for key in cfg[sec]:
            if key not in DEFAULTS[sec]:
                sys.exit(f'{ini}: unknown setting "{key}" in [{sec}]')
    try:
        Builder(cfg, args).build()
    except Fail as e:
        sys.exit(f'build_pgo: {e}')
    except KeyboardInterrupt:
        sys.exit('build_pgo: interrupted')


if __name__ == '__main__':
    main()
