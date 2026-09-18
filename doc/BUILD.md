# Building MF-OWHM from Source

This page explains how to compile MF-OWHM and its two postprocessors,
ZoneBudget and HydFMT, on Linux, Windows Subsystem for Linux (WSL) and
Windows, with GNU Make, CMake or Meson, and how to connect those builds to
Visual Studio, Visual Studio Code and Zed.

**Only one build system is needed to compile the code.** Several are offered
so that developers can use whichever tool they already know or whichever fits
their editor. They read the same source lists, apply the same compiler flags,
and write binaries with the same names, so switching between them changes
nothing in the result. If you only want a working executable, pick the first
one in the table below that is installed on your machine and skip the rest.

&nbsp;

## Contents

1. [Build systems at a glance](#build-systems-at-a-glance)
2. [Compilers and requirements](#compilers-and-requirements)
3. [Setting up a Linux or WSL build environment](#setting-up-a-linux-or-wsl-build-environment)
4. [Installing Intel oneAPI on Linux and WSL](#installing-intel-oneapi-on-linux-and-wsl)
5. [Installing Meson (and other Python tools)](#installing-meson-and-other-python-tools)
6. [Options shared by every build system](#options-shared-by-every-build-system)
7. [Output names](#output-names)
8. [GNU Make](#gnu-make)
9. [CMake](#cmake)
10. [Meson](#meson)
11. [Editors and IDEs](#editors-and-ides)
12. [Compiler notes](#compiler-notes)
13. [Building MF-OWHM on Fedora, RHEL and derivatives](#building-mf-owhm-on-fedora-rhel-and-derivatives)

&nbsp;

## Build systems at a glance

| Build system  | Input files in this repository                               | What it is                                                   |
| ------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| GNU Make      | `makefile`, `postprocessors/*/makefile`                      | The reference build. A single hand-written makefile with the full source list and flag sets. Needs `make` and a Unix-like shell (Linux, WSL, msys2 on Windows). |
| CMake (3.20+) | `CMakeLists.txt`, `cmake/`, `postprocessors/*/CMakeLists.txt` | A build *generator*: it writes Ninja files, Unix makefiles or Visual Studio solutions from one description. Most widely supported by IDEs. |
| Meson (1.1+)  | `meson.build`, `meson.options`, `postprocessors/*/meson.*`   | A build generator written in Python that always drives Ninja. Short, readable build files and fast configure; needs Python 3. |
| Visual Studio | `ide/visual_studio/OneWater_Project.sln`, `*.vfproj`, `GMG.vcxproj` | Hand-maintained projects for Windows with Intel Fortran. This is how the released Windows binaries are built. |

How they differ in practice:

* **Make** runs the compiler directly and knows the compile order from the
  hand-maintained list. It is the simplest to read and needs nothing but `make`.
  It does not generate IDE projects.
* **CMake** and **Meson** both scan the Fortran sources for `MODULE` and `USE`
  statements, work out the dependency order themselves, build in parallel with
  Ninja and only recompile what changed. CMake can also emit Visual Studio
  solutions and is understood by Visual Studio, VS Code (CMake Tools) and most
  other IDEs. Meson has the shortest build files but can only generate Ninja
  (and Visual Studio or Xcode backends that are less used with Fortran).
* The source file lists for CMake and Meson are the plain text files in
  `cmake/` (`bif-source.txt`, `owhm-source.txt`, `gmg-source.txt`,
  `nogmg-source.txt`, `zonebudget-source.txt`, `hydfmt-source.txt`). Add a new
  source file there and to the makefile.

&nbsp;

## Compilers and requirements

MF-OWHM is Fortran 2008 with one preprocessed file. The GMG solver is the only
C code; it is optional and off by default, in which case a Fortran stub stops
the model with a message if a model asks for GMG.

| Compiler              | Minimum version         | Notes                                                        |
| --------------------- | ----------------------- | ------------------------------------------------------------ |
| Intel Fortran `ifx`   | 2024.2                  | The default compiler for every build system here. Part of Intel oneAPI, Windows and Linux. Version 2026 and later need `-assume ieee_compares`, which every flag set includes. |
| Intel Fortran `ifort` | 2021.13 (oneAPI 2024.2) | The classic compiler, discontinued after oneAPI 2024.2 and no longer downloadable. Still supported by all build files (`F90=ifort`, `-DCMAKE_Fortran_COMPILER=ifort`, `FC=ifort`) for those who have it installed, but no longer the default. |
| GNU `gfortran`        | 15.2                    | Older 13/14 releases compile the code but have shown internal compiler errors or runtime faults with it. |
| C compiler (GMG only) | any                     | `icx` (oneAPI), `gcc`, or Visual Studio's `cl` through the Visual Studio projects. |

What to install, per platform:

| Platform            | gfortran route                                               | Intel route                                                  |
| ------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Linux, WSL (Ubuntu) | `apt install gfortran` plus the tools in the next section    | The Intel Fortran Compiler download (ifx only) or the Intel oneAPI Toolkit (ifx and icx), see [Installing Intel oneAPI](#installing-intel-oneapi-on-linux-and-wsl) |
| Windows (native)    | gfortran through [msys2](https://www.msys2.org/) (`pacman -S mingw-w64-ucrt-x86_64-gcc-fortran`) and build from its shell | Visual Studio 2026 (Community is enough, select the "Desktop development with C++" workload) and then the Intel Fortran Compiler or the Intel oneAPI Toolkit, which install the Visual Studio integration. ifx supports Visual Studio 18 (2026); the classic ifort only supports Visual Studio 17 (2022). CMake and Ninja are included with Visual Studio. |

&nbsp;

## Setting up a Linux or WSL build environment

The following was used to set up Ubuntu 26.04 LTS under WSL 2 and works the same
on a plain Ubuntu machine or in a container. After these steps `make`, `cmake` and
`meson` builds all work with gfortran; add Intel oneAPI afterwards if you want
`ifx`.

Update the package index and the installed packages first:

```bash
sudo apt update && sudo apt upgrade -y
```

Install the basic development tools, git with large-file support (the binaries
in the repository are stored with git LFS), and a few utilities used by the
scripts:

```bash
sudo apt install -y build-essential git git-lfs curl wget unzip zip pkg-config bash-completion
```

Install the GNU Fortran compiler and the debugger:

```bash
sudo apt install -y gfortran gdb
```

Install CMake and Ninja (GNU Make came with `build-essential`); Meson can also
come from apt, or from a Python tool manager as described in the next sections:

```bash
sudo apt install -y cmake ninja-build meson
```

Optional, only needed to turn the markdown documentation into PDF:

```bash
sudo apt install -y pandoc texlive-luatex texlive-latex-recommended texlive-latex-extra texlive-fonts-recommended fonts-lmodern fonts-dejavu
```

Then get the code and build it (any one of the three commands):

```bash
git clone https://code.usgs.gov/modflow/mf-owhm.git
cd mf-owhm
make COMPILER=GCC F90=gfortran CC=gcc          # GNU Make
cmake -S . -B build && cmake --build build      # CMake
meson setup build && meson compile -C build && meson install -C build   # Meson
```

### WSL specifics

Install WSL and Ubuntu from an administrator PowerShell with `wsl --install -d Ubuntu`,
open the Ubuntu shell and run the commands above. Keep the repository on the
Linux file system (for example `~/dev/mf-owhm`), not under `/mnt/c`, for normal
build speed. The Windows editors (VS Code, Visual Studio) can open that folder
through the `\\wsl$` path or the WSL remote extension.

### Docker or Podman

The same steps make a reproducible container image. Save this as
`Dockerfile` next to the repository (the base image is the current Ubuntu LTS
release; `ubuntu:latest` also points to it):

```dockerfile
FROM ubuntu:26.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential git git-lfs curl ca-certificates pkg-config \
      gfortran gdb cmake ninja-build meson \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /work
```

Build the image once and compile inside it with the repository mounted
(replace `podman` with `docker` if that is what you use):

```bash
podman build -t mf-owhm-build .
podman run --rm -v "$PWD:/work" mf-owhm-build make COMPILER=GCC F90=gfortran CC=gcc
podman run --rm -v "$PWD:/work" mf-owhm-build sh -c 'cmake -S . -B build && cmake --build build'
```

The binaries appear in `bin/` on the host because the repository directory is
mounted into the container.

#### Container with Intel ifx

ifx usually produces the faster executable. Intel publishes an apt repository
for oneAPI, so a container can install the current Fortran compiler (and,
optionally, the C/C++ compiler for GMG) without an interactive installer:

```dockerfile
FROM ubuntu:26.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential git git-lfs curl ca-certificates gnupg pkg-config \
      cmake ninja-build meson \
    && curl -fsSL https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
       | gpg --dearmor -o /usr/share/keyrings/oneapi-archive-keyring.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" \
       > /etc/apt/sources.list.d/oneAPI.list \
    && apt-get update && apt-get install -y --no-install-recommends \
      intel-oneapi-compiler-fortran \
    && rm -rf /var/lib/apt/lists/*
# Load the compiler environment for every command run in the container
ENTRYPOINT ["/bin/bash", "-c", "source /opt/intel/oneapi/setvars.sh > /dev/null && exec \"$@\"", "--"]
WORKDIR /work
```

Add `intel-oneapi-compiler-dpcpp-cpp` to the install line for `icx` if the GMG
solver should be compiled with the Intel C compiler (`gcc` from
`build-essential` compiles it as well). The package `intel-oneapi-compiler-fortran`
always installs the newest ifx; a specific release is pinned with a versioned
package name such as `intel-oneapi-compiler-fortran-2026.1`. Then:

```bash
podman build -t mf-owhm-ifx .
podman run --rm -v "$PWD:/work" mf-owhm-ifx make                              # ifx is the makefile default
podman run --rm -v "$PWD:/work" mf-owhm-ifx sh -c 'cmake -S . -B build -DCMAKE_Fortran_COMPILER=ifx && cmake --build build'
```

&nbsp;

## Installing Intel oneAPI on Linux and WSL

Intel distributes the compilers as free downloads. Two downloads matter for
MF-OWHM:

| Download                                                     | Contains                                            | Installed size (observed)                       | Use it when                                                  |
| ------------------------------------------------------------ | --------------------------------------------------- | ----------------------------------------------- | ------------------------------------------------------------ |
| [Intel Fortran Compiler](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html) | `ifx`, the Fortran runtime, `gdb-oneapi`            | about 2.4 GB (a Fortran-only install of 2024.2) | You build MF-OWHM without GMG, which is the normal case. Fortran is all that is needed. |
| [Intel oneAPI Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/oneapi-toolkit.html) | `ifx`, `icx`/`icpx`, MPI, OpenMP, MKL, TBB and more | about 4.6 GB (the 2026.1 toolkit install)       | You want the GMG solver compiled with `icx`, or you also develop C/C++ code. |

Since oneAPI 2026 there is a single toolkit download; the separate HPC and
Base toolkits of earlier releases no longer exist. The sizes are what the two
installs used for testing occupy; the toolkit grows with every extra component
selected. If you only need the GMG solver, `gcc` from `build-essential`
compiles it just as well (`make USEGMG=YES CC=gcc` or
`cmake -DOWHM_USE_GMG=ON -DCMAKE_C_COMPILER=gcc`), so the Fortran-only download
is sufficient for almost everyone.

The classic compiler `ifort` was discontinued with oneAPI 2024.2 and is not
part of these downloads. Users with a paid Intel support plan, or who still
have the oneAPI 2024.2 offline installer, can install it alongside ifx; the
build files accept it (`F90=ifort`, `-DCMAKE_Fortran_COMPILER=ifort`,
`FC=ifort`). Everyone else uses ifx, which is the default.

### Installing with the offline installer

On the download page choose *Linux*, *Offline installer*, and copy the link of
the `.sh` file; that link always points at the current release, so fetch it
with `curl` rather than relying on a version number written here:

```bash
curl -L -o intel-fortran-compiler-offline.sh "<link copied from the download page>"
sh ./intel-fortran-compiler-offline.sh -a --silent --eula accept \
    --install-dir "$HOME/.intel/oneapi-$(date +%Y)"
```

Replace `$(date +%Y)` with the version printed by the installer (for example
`oneapi-2026.1`) if you prefer. The installer runs without administrator
rights when the install directory is under your home, and installing each
version into its own directory makes it easy to keep more than one. The
components can be narrowed with `--components`; the identifier for the
Fortran compiler is `intel.oneapi.lin.ifort-compiler` (ifx and the Fortran
runtime) and for the C/C++ compiler `intel.oneapi.lin.dpcpp-cpp-compiler`; run
the installer with `-a --help` to list them. The same installer works inside
WSL.

Intel also provides an apt repository (packages
`intel-oneapi-compiler-fortran` and `intel-oneapi-compiler-dpcpp-cpp`, set up
as shown in the [container example](#container-with-intel-ifx)) that installs
under `/opt/intel/oneapi`; the environment script below works for either
location.

### Loading the compiler environment

oneAPI puts nothing on the `PATH` by default. Each install has a `setvars.sh`
that sets up the shell, and only one version can be loaded in a shell at a
time. A small script in `~/.bashrc` (or `~/.intel/env.sh` sourced from it)
turns that into a command. The first version pins a release, the second picks
the newest version found:

```bash
# ~/.intel/env.sh  --  source this from ~/.bashrc
#
# Load one Intel oneAPI version per shell. Open a new shell to switch.

_oneapi_load() {              # $1 = root directory of a oneAPI install
    local root=$1
    shift
    if [ -n "$INTEL_ONEAPI_ROOT" ]; then
        echo "oneAPI from $INTEL_ONEAPI_ROOT is already loaded in this shell" >&2
        return 1
    fi
    [ -r "$root/setvars.sh" ] || { echo "no setvars.sh in $root" >&2; return 1; }
    . "$root/setvars.sh" "$@" > /dev/null && export INTEL_ONEAPI_ROOT=$root
}

# A specific version:  oneapi-2026.1
oneapi-2026.1() { _oneapi_load "$HOME/.intel/oneapi-2026.1" "$@"; }

# The classic compiler, only for those who still have oneAPI 2024.2 installed
# (IFORTCFG silences the ifort deprecation remark 10448)
oneapi-ifort()  { _oneapi_load "$HOME/.intel/oneapi-2024.2" "$@" && export IFORTCFG="$HOME/.intel/ifort.cfg"; }

# Whatever is newest:  oneapi
oneapi() {
    local root
    root=$(ls -d "$HOME"/.intel/oneapi-* /opt/intel/oneapi 2>/dev/null | sort -V | tail -1)
    [ -n "$root" ] || { echo "no oneAPI installation found" >&2; return 1; }
    _oneapi_load "$root" "$@"
}
```

With `. ~/.intel/env.sh` in `~/.bashrc` (and, only if ifort is used,
`echo '-diag-disable=10448' > ~/.intel/ifort.cfg`), a build with the current
compiler is then:

```bash
oneapi                                   # loads the newest install
make                                     # ifx is the default; or: cmake -S . -B build -DCMAKE_Fortran_COMPILER=ifx
```

Scripts that must not depend on the interactive shell can load the
environment in a subshell instead, which is what the VS Code tasks do:

```bash
bash -c 'source ~/.intel/oneapi-2026.1/setvars.sh > /dev/null; make F90=ifx CC=icx'
```

&nbsp;

## Installing Meson (and other Python tools)

Meson is a Python program. Linux package managers carry it (`apt install meson`,
`dnf install meson`), but the version can lag. Three Python tool managers give a
current version on Linux, WSL and Windows, either for the whole user account
("global") or only for one project ("local"):

| Tool                                                   | What it is                                                   | Install Meson for the user                         | Install Meson for one project                                |
| ------------------------------------------------------ | ------------------------------------------------------------ | -------------------------------------------------- | ------------------------------------------------------------ |
| [uv](https://docs.astral.sh/uv/)                       | Fast Python package and project manager; also installs Python itself. | `uv tool install meson`                            | `uv venv && uv pip install meson ninja`, then `uv run meson setup build` |
| [pipx](https://pipx.pypa.io/)                          | Installs Python command-line tools, each in its own isolated environment. | `pipx install meson`                               | `python -m venv .venv && . .venv/bin/activate && pip install meson ninja` |
| [miniforge](https://conda-forge.org/download/) (conda) | Minimal conda installer that uses the community conda-forge channel. | `conda install -n base -c conda-forge meson ninja` | `conda create -n mf-owhm -c conda-forge meson ninja gfortran`, then `conda activate mf-owhm` |

Notes:

* `uv` is installed with `curl -LsSf https://astral.sh/uv/install.sh | sh` on
  Linux and WSL, or with `winget install astral-sh.uv` on Windows; `uv tool
  install meson` puts `meson` on the `PATH` for the user and keeps it isolated
  from any other Python. This is what the development team used for testing.
* `pipx` comes from `apt install pipx` (or `pip install --user pipx`) and
  behaves the same way; `pipx upgrade meson` updates it.
* miniforge is the heaviest of the three but also provides compilers
  (`gfortran` from conda-forge) and runs on Windows without msys2, so a single
  `conda` environment can hold Meson, Ninja and gfortran. Use it when you
  already work in conda environments.
* All three can install `fpm`, `fortls` (the Fortran language server used by
  VS Code and Zed) and `cmake` the same way, for example `uv tool install fortls`.

&nbsp;

## Options shared by every build system

| Choice                      | Make                                             | CMake                                                        | Meson                                                   |
| --------------------------- | ------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------- |
| Release or debug flags      | `CONFIG=RELEASE|DEBUG`                           | `-DCMAKE_BUILD_TYPE=Release|Debug`                           | `-Dconfig=release|debug`                                |
| Include the GMG solver      | `USEGMG=YES`                                     | `-DOWHM_USE_GMG=ON`                                          | `-Duse_gmg=true`                                        |
| Promote `REAL` to 8 bytes   | `DBLE=YES` (default)                             | `-DOWHM_DOUBLE=ON` (default)                                 | `-Ddouble=true` (default)                               |
| Static linking              | `STATIC=YES` (default)                           | `-DOWHM_STATIC=ON` (default)                                 | `-Dstatic=true` (default)                               |
| Fortran compiler            | `F90=ifx` (default), `F90=ifort`, `F90=gfortran` | `-DCMAKE_Fortran_COMPILER=ifx` (default: first found on PATH) | `FC=ifx meson setup ...` (default: first found on PATH) |
| C compiler (GMG only)       | `CC=icx`                                         | `-DCMAKE_C_COMPILER=icx`                                     | `CC=icx meson setup ...`                                |
| Build ZoneBudget and HydFMT | run their own makefiles                          | `-DOWHM_BUILD_ZONEBUDGET=ON -DOWHM_BUILD_HYDFMT=ON` (default) | `-Dbuild_zonebudget=true -Dbuild_hydfmt=true` (default) |

The postprocessors keep `REAL` at 4 bytes by default, as their makefiles do.
The flag sets themselves are in `makefile`, `cmake/CompilerFlagInput.cmake` and
`meson.build`; they are kept identical.

&nbsp;

## Output names

| Binary           | Linux and macOS                                | Windows                                        |
| ---------------- | ---------------------------------------------- | ---------------------------------------------- |
| MF-OWHM          | `bin/mf-owhm.nix`                              | `bin/mf-owhm.exe`                              |
| MF-OWHM with GMG | `bin/mf-owhm-gmg.nix`                          | `bin/mf-owhm-gmg.exe`                          |
| ZoneBudget       | `postprocessors/zonebudget/bin/zonebudget.nix` | `postprocessors/zonebudget/bin/zonebudget.exe` |
| HydFMT           | `postprocessors/hydfmt/bin/hydfmt.nix`         | `postprocessors/hydfmt/bin/hydfmt.exe`         |

A debug build appends `-debug` before the extension, for example
`bin/mf-owhm-debug.nix`. These are the names the example scripts in
`examples/bash_example_run` look for. Intermediate files go to `obj/` (Make)
or to the build directory you choose (CMake, Meson); `build/` is ignored by git.
`.util/cleanRepo.sh` removes the object, library, example output and build
directories (`.util/cleanRepo.sh build nopause` for the build directories only).

&nbsp;

## GNU Make

Run `make` from the repository root. Every option is passed on the command
line; the defaults in the makefile need not be edited.

```bash
make                                    # ifx release -> bin/mf-owhm.nix
make CONFIG=debug                       # -> bin/mf-owhm-debug.nix
make F90=ifort                          # classic ifort release (oneAPI 2024.2)
make COMPILER=GCC F90=gfortran CC=gcc   # gfortran
make USEGMG=YES CC=icx bin_out=bin/mf-owhm-gmg
make compile ...                        # same as make, without the banner
make clean | cleanOBJ | reset           # objects only | all of obj/ | obj/ and bin/
make print-F90FLAGS                     # show any makefile variable

cd postprocessors/zonebudget && make    # each postprocessor has the same options
```

Use distinct `int_dir=` and `bin_out=` values when several compilers are
compared, so the object files do not collide (the VS Code tasks do this).

&nbsp;

## CMake

Configure once into a build directory, then build. Each configure line below
is an alternative; the compiler is taken from the `PATH` unless given.

```bash
cmake -S . -B build                                            # Release
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug
cmake -S . -B build -DCMAKE_Fortran_COMPILER=ifx
cmake -S . -B build -DOWHM_USE_GMG=ON -DCMAKE_C_COMPILER=gcc
cmake --build build                                            # or: cmake --build build --parallel
```

CMake writes the binaries straight into `bin/` and `postprocessors/*/bin/`
(`-DOWHM_BIN_DIR=<dir>` moves the MF-OWHM binary). With a multi-configuration
generator (Visual Studio) the configuration is chosen at build time:
`cmake --build build --config Debug`.

Each postprocessor can also be configured on its own:

```bash
cmake -S postprocessors/zonebudget -B postprocessors/zonebudget/build
cmake --build postprocessors/zonebudget/build
```

Generators: `-G Ninja` (fastest, used by the IDE tasks), `-G "Unix Makefiles"`
(the default on Linux), or a Visual Studio generator on Windows, see
[Visual Studio](#visual-studio) below.

&nbsp;

## Meson

```bash
meson setup build                       # release, compiler found on PATH
meson setup build -Dconfig=debug
FC=ifx meson setup build
meson setup build -Duse_gmg=true
meson compile -C build
meson install -C build                  # copies the binaries into bin/ and postprocessors/*/bin/
```

Meson builds into the build directory; `meson install` places the binaries
where the other build systems put them. The project uses `buildtype=plain`, so
Meson adds no optimization flags of its own and every flag comes from
`meson.build`. Options of an existing build directory are changed with
`meson configure build -Dconfig=debug`. The postprocessor directories hold
standalone `meson.build` files with the same options (`config`, `double`,
`static`).

&nbsp;

## Editors and IDEs

### Visual Studio

The hand-maintained solution `ide/visual_studio/OneWater_Project.sln` (and
`OneWater_GMG_Project.sln` for the GMG variant) builds with Visual Studio and
the Intel Fortran integration; open it and run *Build Solution*. This is how
the released Windows executables are produced. Its configurations `Debug`,
`Fast_Debug` and `Release` use ifx and produce `mf-owhm-debug.exe`,
`mf-owhm-fast-debug.exe` and `mf-owhm.exe`; `ReleaseIFORT` uses the classic
ifort (only with Visual Studio 2022 and oneAPI 2024.2) and produces
`mf-owhm.ifort.exe`.

The GMG solution `OneWater_GMG_Project.sln` adds the C project `GMG.vcxproj`,
which builds the solver into `lib\GMG_x64_<Configuration>.lib` and is linked
by `mf-owhm-gmg.vfproj` (ifx only, no ifort configuration). The C project
picks the newest Intel C++ Compiler (icx) toolset installed for the Visual
Studio in use and the newest Windows SDK; without an Intel toolset it falls
back to the Visual Studio C compiler. The environment variable
`OWHM_GMG_TOOLSET` overrides the choice (for example `v145` for the Visual
Studio 2026 C compiler) when an Intel toolset is installed but its integration
is broken ("Could not expand ICInstallDir variable"). Visual Studio 2022 builds
both projects of the solution from the IDE and from `devenv.com /Build`. The
Visual Studio 2026 `devenv.com` skips the C project when run from the command
line; build it first with
`MSBuild.exe ide\visual_studio\GMG.vcxproj /p:Configuration=Release /p:Platform=x64`
and then run `devenv.com` on the solution, or build inside the IDE.

**Open the solution from a local Windows drive.** If the repository lives in
WSL and the solution is opened through `\\wsl$` or `\\wsl.localhost`, the
Intel integration hands the object files to the linker as
`//wsl.localhost/...` paths, which `link.exe` reads as options and ignores
("unrecognized option" warnings for every `.obj`), so the link fails with
`LNK2001: unresolved external symbol mainCRTStartup`, and the manifest tool
fails with `c1010070`. Keep a checkout, or a `git worktree`, on an NTFS drive
for Visual Studio builds; the same solution builds cleanly there.

CMake can also generate a fresh solution that uses the current source lists.
Which Visual Studio to generate for depends on the compiler: ifx supports
Visual Studio 18 (2026), while the classic ifort from oneAPI 2024.2 supports
Visual Studio 17 (2022) at most. From a Windows command prompt, load the
matching oneAPI environment and generate:

```bat
rem ifx with Visual Studio 2026
"C:\Program Files (x86)\Intel\oneAPI\compiler\latest\env\vars.bat" intel64 vs2026
cmake -S . -B build\vs2026 -G "Visual Studio 18 2026" -A x64 -T fortran=ifx
"C:\Program Files\Microsoft Visual Studio\18\Community\Common7\IDE\devenv.com" build\vs2026\mf-owhm.slnx /Build "Release|x64"

rem classic ifort with Visual Studio 2022 (oneAPI 2024.2 only)
"C:\Program Files (x86)\Intel\oneAPI\compiler\2024.2\env\vars.bat" intel64 vs2022
cmake -S . -B build\vs2022 -G "Visual Studio 17 2022" -A x64 -T fortran=ifort
cmake --build build\vs2022 --config Release
```

The generated solution (`mf-owhm.sln` for Visual Studio 17, `mf-owhm.slnx` for
Visual Studio 18) and the `.vfproj`/`.vcxproj` files are written to the build
directory (which git ignores) and can be opened in Visual Studio for editing
and debugging; the executables still go to `bin\`. Regenerate the solution
after adding a source file to the lists in `cmake/`. CMake 4.1 or newer is
needed for the Visual Studio 18 generator (the CMake bundled with Visual
Studio 2022 is older; install CMake from https://cmake.org or use the copy
bundled with Visual Studio 2026). Fortran projects are built by `devenv`
rather than MSBuild, and with Visual Studio 18 `cmake --build` currently fails
to hand the `.slnx` file to `devenv` ("The parameter is incorrect"), so build
the 2026 solution with `devenv.com ... /Build` as shown, or from inside Visual
Studio; `cmake --build` works for the Visual Studio 17 solution.

### Visual Studio Code

The repository carries a ready `.vscode/` folder (open the repository folder,
or `ide/vscode/mf-owhm.code-workspace`). It provides:

* **Tasks** (Terminal > Run Task) for every build: the GNU Make tasks
  (`Build gfortran Debug`, `Build ifx Release`, ...), CMake tasks
  (`CMake: Build Debug (gfortran)`, `CMake: Build Release (ifx)`, ...), Meson
  tasks, and native Windows tasks (`Windows ifx: CMake Build Debug`, ...) that
  load oneAPI through `vars.bat` and build with Ninja or generate the Visual
  Studio solution.
* **Launch configurations** (Run and Debug) that build first and then start
  the model under `gdb` (Linux and WSL) or the Visual Studio debugger
  (Windows), either in the directory set by `ProgramRun.CWD` in
  `.vscode/settings.json` or in a directory you type in.
* Recommended extensions: Modern Fortran, C/C++ (for the debugger), CMake
  Tools and Meson.

With the CMake Tools extension, `CMakeLists.txt` is also picked up directly:
choose a kit (gfortran or ifx) and press F7 to build; the extension's build
directory is set to `build/vscode-cmake` in the workspace settings. See
`ide/vscode/README.md` for details and the WSL setup.

### Zed

Zed runs builds through its task system. The repository carries
`.zed/tasks.json` with tasks for GNU Make, CMake and Meson (gfortran and ifx,
debug and release; build trees under `build/zed-*` and `obj/zed_*`) plus a
task that runs the debug binary on an example model, and `.zed/settings.json`
with the Fortran file associations. Run a task with the *task: spawn* command
or from the terminal panel. Zed uses `fortls` for Fortran language support
(install it with `uv tool install fortls`; the repository's `.fortls` file
configures it) and has no built-in debugger for Fortran, so debug with `gdb`
in the terminal or from VS Code.

&nbsp;

## Compiler notes

* **ifx** needs `-assume ieee_compares` (included in every flag set) so that
  the `X /= X` tests the code uses to detect NaN work under `-fpe0`. Without it
  ifx 2026 traps on the first NaN comparison. ifort and gfortran do not need it.
* **ifx debug builds** cannot combine `-check uninit` with static linking; the
  build systems drop `uninit` from the check list in that case.
* **gfortran debug builds** initialize reals to a signaling NaN and trap invalid
  operations, divide by zero and overflow, matching what `-fpe0` gives with
  Intel Fortran. Underflow is not trapped.
* **Windows with Intel Fortran** is normally built from the Visual Studio
  solution. CMake and Meson carry the Windows spellings of the Intel options;
  ifx has no `/check:uninit` on Windows, so the Windows debug flags omit it.
  ifx works with Visual Studio 18 (2026) and 17 (2022); the classic ifort only
  with Visual Studio 17 (2022) and earlier.

&nbsp;

## Building MF-OWHM on Fedora, RHEL and derivatives

The commands above are for Debian and Ubuntu based systems. This section lists
the package manager commands for Fedora, Red Hat Enterprise Linux and their
derivatives. Everything else on this page (compilers, options, build systems,
editors) applies unchanged.

### Fedora (dnf)

Update the system:

```bash
sudo dnf upgrade --refresh -y
```

Development tools, git with large-file support, and utilities:

```bash
sudo dnf install -y gcc gcc-c++ make git git-lfs curl wget unzip zip pkgconf-pkg-config bash-completion
```

GNU Fortran and the debugger:

```bash
sudo dnf install -y gcc-gfortran gdb
```

CMake, Ninja and Meson:

```bash
sudo dnf install -y cmake ninja-build meson
```

Optional, for converting the markdown documentation to PDF:

```bash
sudo dnf install -y pandoc texlive-scheme-medium texlive-luatex
```

### RHEL 9 and 10, Rocky Linux, AlmaLinux

The same packages exist, but `git-lfs`, `ninja-build` and `meson` come from the
EPEL and CodeReady Builder (CRB) repositories. Enable them first:

```bash
sudo dnf install -y epel-release
sudo dnf config-manager --set-enabled crb          # on RHEL: subscription-manager repos --enable codeready-builder-for-rhel-9-x86_64-rpms
sudo dnf install -y gcc gcc-c++ gcc-gfortran make gdb git git-lfs curl wget unzip zip pkgconf-pkg-config bash-completion cmake ninja-build meson
```

RHEL ships an older `gfortran` as the system compiler. A current GCC is
available through the GCC Toolset collections (`sudo dnf install gcc-toolset-14-gcc-gfortran`
and then `scl enable gcc-toolset-14 bash`), or install gfortran from
conda-forge with miniforge as described [above](#installing-meson-and-other-python-tools).
Check `gfortran --version`; MF-OWHM expects 15.2 or newer.

### Intel oneAPI on Fedora and RHEL

The offline installer and the environment script above work the same on
Fedora and RHEL. Intel also provides a yum/dnf repository; the package names
are `intel-oneapi-compiler-fortran` and `intel-oneapi-compiler-dpcpp-cpp`, and
the compilers install under `/opt/intel/oneapi`, which the `oneapi` function in
the environment script finds automatically.

### Containers

Replace the `apt-get` line of the Dockerfile above with:

```dockerfile
FROM fedora:latest
RUN dnf install -y gcc gcc-c++ gcc-gfortran make gdb git git-lfs cmake ninja-build meson && dnf clean all
WORKDIR /work
```
