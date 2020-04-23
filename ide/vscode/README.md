# [Visual Studio Code (vscode)](https://code.visualstudio.com/docs/editor/codebasics)



## Project Workspace

This file, `mf-owhm.code-workspace`, sets the root directory as the head of the path and points to the `.vscode` folder for recommended extensions, tasks for compiling, and settings.



## Extensions

This project workspace includes the file `.vscode/extensions.json`

This checks for the following extensions and if they are not present automatically installs them:

- C/C++ for Visual Studio Code
  - https://github.com/Microsoft/vscode-cpptools
- Modern Fortran language support for VSCode
  - https://github.com/krvajal/vscode-fortran-support
- Fortran Breakpoint Support
  - https://github.com/ekibun/FortranBreaker
- EasyZoom
  - https://github.com/nabeelvalley/EasyZoomVSCodeExtension



## Portable Windows VSCode

Microsoft provides a version of VSCode that does not require administrative privileges and only requires extracting a single `.zip`   file. Where you extract the file, lets call it `vscodePortable`, is where the `vscode.exe` is placed and can run the editor.

To download the `.zip` version, please go to the alternate downloads (https://code.visualstudio.com/#alt-downloads) and select the `64 bit` `.zip` version. 

When you run the portable vscode, it will look for extensions in your user profile location, `%userprofile%` 

For example, 
C:\Users\\*username* 
where username is your windows user name.

If you want to make  `vscodePortable` truly portable, then you have to add the empty directory `data` inside the folder. 

That is you need to make sure the following folder exists: ` vscodePortable/data`. 

When VSCode sees there is a data folder in its directory, it installs the extensions to that location.



## Windows Compilation

VSCode requires the GNU GCC compiler set, in particular `gfortran `. 

On windows options for obtaining GNU GCC are [msys2](https://www.msys2.org/), [mingw](http://mingw-w64.org/doku.php), and [cygwin](https://www.cygwin.com/). 

Note that the `bash` terminal that comes with `git` does NOT include `gfortran`. 

All tests are run using [msys2](https://www.msys2.org/) with the msys bash terminal.

The windows terminal `cmd.exe` works, but `powershell` does not.

Note that VSCode by default uses `powershell`, so you will have to change that.

### Windows Terminal

On windows VSCode and GNU Makefiles do not run well on `powershell`. 

VSCode has been set up to automatically use the windows terminal `cmd.exe`, but it does not always work. 

If it defaults to opening with powershell you may have to select a new default terminal as `cmd.exe`.

### Windows BASH terminal

If you want to change this to bash you have to edit `.vscode/settings.json` by changing the following `json` command:

`"terminal.integrated.shell.windows": "C:\\Windows\\System32\\cmd.exe",`

to

`"terminal.integrated.shell.windows": "c:\\path\\to\\bash.exe",`

If this does not work, then you will have to go and edit your user  `Settings`, and make sure it is set to `User` and not `Workspace`, then go to `Features`, then `Terminal`,  then `Integrated › Shell: Windows` and click `Edit in settings.json`, which will open up that file and add the following entry:

`"terminal.integrated.shell.windows": "C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\powershell.exe",`

which will require editing the path to cmd.exe or bash.exe. Note that this `settings.json` is your user settings rather than workspace settings. 

### make clean Errors

Compiling with the GNU makefile will work using `cmd.exe` if there is `GNU Make` installed. However the commands in the `makefile` are Unix-based and may not work. In particular, windows has the same `find` command, but with different syntax.

Either make sure that the msys `find.exe` is in the path ahead of the windows `find.exe`, or run the compilation from a bash terminal (which automatically makes the Unix `find.exe` ahead of the windows).

Another work around is to just manually delete the files located in `obj/`, which is where the compiler places all object and module files. 

You should not delete the `obj/.keep` file, which is used to ensure git carries forward the `obj` directory.