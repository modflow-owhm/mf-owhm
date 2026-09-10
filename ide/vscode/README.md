# [Visual Studio Code (vscode)](https://code.visualstudio.com/docs/editor/codebasics)

VS Code support for MF-OWHM lives in the repository's `.vscode` folder:

| File                       | Purpose                                                                                         |
| -------------------------- | ----------------------------------------------------------------------------------------------- |
| `.vscode/settings.json`    | Workspace settings: run directory and name file for the debugger, binary and directory names, Intel oneAPI script locations, editor and Fortran language-server settings. |
| `.vscode/tasks.json`       | Build tasks for GNU Make, CMake and Meson (Linux and WSL) and for native Windows builds with Intel ifx. |
| `.vscode/launch.json`      | Debugger launch configurations that build first and then run a model under `gdb` (Linux, WSL) or the Visual Studio debugger (Windows). |
| `.vscode/extensions.json`  | Recommended extensions; VS Code offers to install them when the folder is opened.               |

Open the repository folder directly (File > Open Folder) or open the
workspace file in this directory, `mf-owhm.code-workspace`, which points to
the repository root. The workspace file holds no settings of its own.

General build instructions, including how to set up Linux, WSL and Windows,
are in [doc/BUILD.md](../../doc/BUILD.md).

&nbsp;

## Extensions

| Extension                          | Identifier                     | Used for                                                              |
| ---------------------------------- | ------------------------------ | --------------------------------------------------------------------- |
| Modern Fortran                     | `fortran-lang.linter-gfortran` | Syntax highlighting, the `fortls` language server, gfortran linting. |
| C/C++                              | `ms-vscode.cpptools`           | The `gdb` (cppdbg) and Visual Studio (cppvsdbg) debugger front ends. |
| CMake Tools                        | `ms-vscode.cmake-tools`        | Optional: configure and build `CMakeLists.txt` from the status bar.  |
| Meson                              | `mesonbuild.mesonbuild`        | Optional: syntax support and build integration for `meson.build`.    |
| WSL                                | `ms-vscode-remote.remote-wsl`  | Run VS Code against the Linux side of Windows Subsystem for Linux.   |
| Python                             | `ms-python.python`             | Optional: helper scripts.                                             |

`fortls` itself is a separate program: install it with `uv tool install fortls`
(or `pipx install fortls`). The repository's `.fortls` file configures it.

&nbsp;

## Settings to adjust

In `.vscode/settings.json`:

* `ProgramRun.CWD` and `ProgramRun.NAM`: the model directory and name file
  used by the "(settings.json)" launch configurations. The "(Specify)"
  configurations prompt for them instead.
* `ProgramIntel.SetVars`: the oneAPI `setvars.sh` the ifx tasks load on Linux
  and WSL (default `~/.intel/oneapi-2026.1/setvars.sh`; use
  `/opt/intel/oneapi/setvars.sh` for a system-wide install).
* `ProgramIntel.VarsBat`: the oneAPI `vars.bat` used by the native Windows
  tasks (default `C:\Program Files (x86)\Intel\oneAPI\compiler\latest\env\vars.bat`).
* `ProgramVS.DevEnv`: the Visual Studio 2026 `devenv.com` that builds the
  generated `.slnx` solution (CMake's own build step cannot pass a `.slnx`
  to devenv yet).
* `ProgramName.*`, `ProgramSRC.Dir`, `ProgramOBJ.Dir`: binary and directory
  names; normally left alone.

&nbsp;

## Building (Terminal > Run Task)

| Task group                                   | Tool              | Where it runs     | Output                                                        |
| -------------------------------------------- | ----------------- | ----------------- | ------------------------------------------------------------- |
| `Build gfortran Debug` ... `Rebuild ifx Release GMG` | GNU Make  | Linux, WSL, msys2 | `bin/mf-owhm-debug.nix`, `bin/mf-owhm-ifx.nix`, ... (objects under `obj/vscode_*`) |
| `CMake: Build Debug (gfortran)` ... `CMake: Build Release (ifx)` | CMake + Ninja | Linux, WSL | `bin/mf-owhm-debug.nix` or `bin/mf-owhm.nix`; build trees under `build/vscode-cmake-*` |
| `Meson: Build Debug (gfortran)` ... `Meson: Build Release (ifx)` | Meson + Ninja | Linux, WSL | same names, installed into `bin/`; build trees under `build/vscode-meson-*` |
| `Windows ifx: CMake Build Debug` / `Release`  | CMake + Ninja     | Windows (cmd.exe) | `bin\mf-owhm-debug.exe` or `bin\mf-owhm.exe`                  |
| `Windows ifx: Generate Visual Studio 2026 solution` | CMake       | Windows (cmd.exe) | `build\vs2026\mf-owhm.sln` and `.vfproj`, to open in Visual Studio |
| `Windows ifx: Build Visual Studio solution Debug` / `Release` | devenv.com (Visual Studio 2026) | Windows | `bin\mf-owhm-debug.exe` or `bin\mf-owhm.exe`; `ProgramVS.DevEnv` points at devenv.com |

`Build gfortran Debug` is the default build task (Ctrl+Shift+B). The ifx tasks
load the oneAPI environment themselves, so VS Code does not need to be started
from an Intel command prompt. The Windows tasks run through `cmd.exe` and
require Visual Studio 2026 (for Ninja, CMake and the linker) and Intel oneAPI
with the Visual Studio integration. ifx supports Visual Studio 18 2026; the
classic ifort from oneAPI 2024.2 only supports Visual Studio 17 2022, so for
ifort change the generate task to `vars.bat ... vs2022`,
`-G "Visual Studio 17 2022"` and `-T fortran=ifort`.

With the CMake Tools extension installed, `CMakeLists.txt` can also be driven
from the status bar: pick a kit (gfortran, or ifx after loading oneAPI in the
shell that starts VS Code) and a variant (Debug or Release), then build with
F7. The extension uses `build/vscode-cmake` as its build directory.

&nbsp;

## Debugging (Run and Debug)

Every launch configuration builds first through its `preLaunchTask`, then
starts the chosen binary in the model directory with the name file as the
argument.

* `Launch gfortran Debug ...`, `Launch ifx Debug (Linux) ...`: the Make-built
  binaries under `gdb`. Works on Linux and in WSL (install `gdb` with apt; for
  ifx the oneAPI `gdb-oneapi` also works, set `miDebuggerPath` if you prefer it).
* `CMake Debug (gfortran, gdb)`, `CMake Debug (ifx, gdb)`: the CMake-built
  `bin/mf-owhm-debug.nix` under `gdb`.
* `Launch ifx Debug (Windows) ...`, `Windows ifx: CMake Debug ...`: native
  Windows executables under the Visual Studio debugger (`cppvsdbg`), which
  understands the Intel Fortran debug information.

The "(settings.json)" variants use `ProgramRun.CWD` and `ProgramRun.NAM`; the
"(Specify)" variants ask for the directory and name file when started.
Breakpoints work in both fixed-form `.f` and free-form `.f90` files.

&nbsp;

## WSL

Install the WSL extension, open the repository folder from the WSL side
(`code .` inside the Ubuntu shell, or "WSL: Open Folder in WSL"), and VS Code
runs the tasks and `gdb` inside Linux while the editor runs on Windows. The
Linux tasks (Make, CMake, Meson, gfortran and ifx) are the ones to use there;
the native Windows tasks need a VS Code window opened on the Windows side of
the same folder (`\\wsl$\Ubuntu\home\...`), where `cmd.exe` is available.

&nbsp;

## Windows notes

* The Make tasks need GNU Make and a Unix-like shell on Windows; msys2 is the
  tested option. The CMake and Meson tasks above do not need that: use the
  `Windows ifx:` tasks, which only need Visual Studio and oneAPI.
* VS Code's default terminal on Windows is PowerShell. The Windows tasks set
  `cmd.exe` as their shell explicitly, so the default terminal does not matter
  for them.
* A portable VS Code (the `.zip` download with an empty `data` folder next to
  `Code.exe`) keeps its extensions inside its own folder and needs no
  administrator rights.
