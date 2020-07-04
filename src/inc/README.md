------

------

------

**If you do not know what this is about, then ignore this folder**

**This folders contents are not required for compilation.**

------

------

------

# OpenSpec.inc Readme

------

------

`openspec_module.f` allows custom file operations that are Fortran Compiler Dependent

By default, Fortran binary files are opened with the `“UNFORMATTED”` and  `“STREAM”` options. 

------

------

------

## Using OpenSpec.inc

------

#### Option 1

To use openspec.inc you must either modify the file:

`modflow_base/openspec_module.f`

to specify what you want to use. See on line 15 for a comment explanation of what to do.

------

#### Option 2

Or you can compile 

`inc/openspec_module.f`

in the place of 

`modflow_base/openspec_module.f`

and modify 

`inc/openspec.inc`

as needed.

