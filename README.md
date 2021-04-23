###### -

![MF-OWHM_banner_v2b](img/MF-OWHM_banner_v2b.png)

&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; **IPDS No.**&nbsp; `NA`
# Version 2.0.2-beta

MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software.

Pronounced as Mod-Flow-Ohm (mäd flō ōm) or colloquially called OneWater.

------

**Version Release Notes**

- [CHANGELOG.md](CHANGELOG.md) &emsp; &emsp; &emsp; &ensp; for bug fixes and issues that have been resolved.
- [CHANGELOG_Features.md](CHANGELOG_Features.md) &thinsp; for new features added.

* [code.json](code.json) &emsp; for version metadata as a text file in json format.



**Important Repository Branches**

If new to git branching and tagging, please review the **[Git Branches Section](#git-branches-and-organization)**,  
which describes the major branches in this repository.  
&nbsp;  
For help on how to download a specific version see the **[Git Tags Section](#gitlab-tags-page)**.   
&nbsp;

------

## Downloads

The following downloads the latest  64-bit executables for Windows (`.exe`) and Ubuntu Linux (`.nix`)  
and includes the source code, examples, postprocessors, and  supplemental documentation (`~40MB` download).

- **[Full MF-OWHM Release Download](https://code.usgs.gov/modflow/mf-owhm/-/archive/release/mf-owhm-release.zip)** &nbsp; &nbsp; &nbsp; &nbsp; ➤ *Recommended option*
-  **[Git Tags Page Download Listing](https://code.usgs.gov/modflow/mf-owhm/-/tags)**

The following downloads the latest Windows x64 executable only:

- **[mf-owhm.exe](https://code.usgs.gov/modflow/mf-owhm/-/raw/release/bin/mf-owhm.exe)**
- **[mf-owhm-gmg.exe](https://code.usgs.gov/modflow/mf-owhm/-/raw/release/bin/mf-owhm-gmg.exe)**
- **[ZoneBudget and HydFMT](#post-processors-zonebudget-and-hydfmt)**

### USGS Software Release Types

There are two types of [USGS Software releases](https://www.usgs.gov/about/organization/science-support/survey-manual/im-osqi-2019-01-review-and-approval-scientific):  

- Enterprise &ensp; ➣ &nbsp; Production&thinsp; Release
- Provisional &thinsp; ➢ &nbsp; Preliminary Release

The difference between the two release types are:

1. The `DISCLAIMER.md` 
   - The git commit will contain either the *Production Release* or *Preliminary Release* disclaimer.
   - Here is the current commit's [DISCLAIMER.md](DISCLAIMER.md)
   - The [USGS Software Disclaimers Section](#usgs-software-disclaimers) includes both disclaimers for comparison purposes.  
     &nbsp; 
1. The text file [code.json](#code.json) will contain the following line:
   - `"status": "Production"` &nbsp; for a  *Production Release* and  
   - `"status": "Preliminary"` for a  *Preliminary Release*  
      &nbsp; 
1. A *Production Release* is a *git tag* registered with the USGS Information Product Data System (`IPDS`).  
   Any other commit is considered a *Preliminary Release*.
   - All code updates and bug fixes start as a *Preliminary Release*;   
     after the code commit is registered with `IPDS` it becomes a *Production Release*.
   - Typically the *Production Release*'s are kept in the `release` branch and all other branches are  *Preliminary Release*'s.

&nbsp;

> :warning: **Note that it is possible for a *Production Release* and *Preliminary Release* to have the same version number.**  
> &emsp;&emsp;That is, they are the same release but contain a different disclaimer.

> :warning: **Contact a developer if you need to convert the current *Preliminary Release* to a *Production Release*.**  
> &emsp;&emsp;Some publication sources—particularly USGS Scientific Investigations Report (SIR)—can only cite a *Production Release*.

### Obtaining Code via Git

Please see the [Git Branches Section](#git-branches-and-organization) for an overview of the major branches in this repository.

To obtain this repository via git run: 

- `git clone  --no-tags  https://code.usgs.gov/modflow/mf-owhm.git ` 
  + The `--no-tags` flag is optional  
    it prevents the repo from downloading all the git release tags, which can make the `git log` messy.
  + [Git Large File Storage (LFS) ](https://git-lfs.github.com/) pointer files are used for executable binaries and example output.
  + By default only the `main` branch is unpacked. 
    + To view all the branches in the clone, run: `git branch --all` to see a list that might include:  
      `remotes/origin/BranchName`, and then to unpack it run: `git switch BranchName`

&nbsp;

## Code Citation

Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.0.2: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS



## Documentation Citation

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., [https://doi.org/10.3133/tm6A60](https://doi.org/10.3133/tm6A60)

&nbsp;

## Let Us Know What  You Are Up To

If you have questions and comments feel free to contact the developers.

Also share any publications that involve MF-OWHM so we can add them to our **[publication list](#application-bibliography)**.

&nbsp;

------


# Table of Contents

[[_TOC_]]     

------

&nbsp;

# Notes About Repository

## Markdown Files (.md)

[Markdown](https://en.wikipedia.org/wiki/Markdown)   (`.md`) files are placed throughout the repository to describe different aspects of the repo or provide general information. Markdown files are simple text files with some additional formatting for fast rendering in html. 

Viewing a markdown file in a text editor can be difficult to follow due to the additional formatting (especially viewing tables).
If you search for Markdown viewers you can find a number of tools that will render the final formatting of the `.md` files. 

The **[readme.txt](readme.txt)** file includes an overview on markdown formatting and includes a list of markdown viewers that can be downloaded .

## GMG Solver Information

MF-OWHM is written in Fortran (77, 95, 2003, and 2008) except the [Geometric Multigrid Solver (GMG)](https://pubs.usgs.gov/of/2004/1261/) is written in C. 

To provide an executable that is written only in Fortran, the default executable binary, `mf-owhm.exe`, 
<u>does not include the `GMG` solver</u>. Note the default binary does include the following solvers: `PCG`, `PCGN`, `SIP`, `DE4`, and `NWT`. 

For new models, it is recommended to use either the `PCGN` solver or `NWT` solver rather than `GMG`.

If your project does use the `GMG` solver, then use the executable binary named `mf-owhm-gmg.exe` instead. If you want to compile the binary yourself, then it is required to have both a Fortran and C compiler (e.g., `ifort` and `icc` or `gfortran` and `gcc`). To compile GMG use the provided `makefile` with the option `USEGMG = YES` or the Visual Studio Solution `OneWater_GMG_Project.sln`.

> :warning: ​Note that the [Downloads](#downloads) links include in the `bin` directory `mf-owhm.exe` and  `mf-owhm-gmg.exe`. 

## Code Versioning

MF-OWHM follows a modified [Semantic Versioning](https://semver.org/) system (`major.minor.patch`) . 

For example, version `12.34.5` is major version `12`, minor version `34`, and patch version `5`. 

Since major and minor versions rarely change, MF-OWHM informally calls a patch version a release number.   
For example, version `2.5.7`, is referred to as `Release 7` of version `2.5`.

For code in development-beta stages the patch number will contain a letter, starting with `a`, positioned after the patch version number. Each subsequent development/beta release will increase the letter by 1. 

For example, if version `2.5.7` is under development with bug fixes, it would be versioned as `2.5.8a`, `2.5.8b`,  `2.5.8c`, and so forth until the final version `2.5.8` is complete. Then the letter process starts over again for `2.5.9a`.

Since versions with a post-pended letter are an intermediate product, these versions will not have a *git tag* associated to them. Using this letter designation is meant to provide a way to provide rapid code releases with small changes without exhausting version numbers. Particularly when fixing a bug that is necessary for a project, but is not significant enough to immediately warrant a patch number increase.

The chronological ordering of version numbers is the `major` version has the highest priority, then the `minor` version, then the `patch` version. The version numbers do not include zero padding, so the number between the decimal points should be interpreted as a single number.

Examples of newer versus older versions are:

- `11.9.7` is newer than `8.9.7`
- `8.9.7` is newer than `2.13.1` 
-  `2.13.1` is newer than `2.5.12`
- `2.5.12` is newer than `2.5.7`
- `2.5.7` is newer than `2.5.7b` is newer than `2.5.7a` is newer than `2.5.6a`

  

## Code Versioning Citation

The version specified at the top of this `README.md` is part of the final software citation.
If there is a *git tag* or *GitLab Release*, then the tagname should match the `README.md` version number.

For example, the for a report that used version `2.7.69` should have the following citation: 

> Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version **2.7.69**: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

  

## Identifying the Code Version

The easiest way to identify the code version is to look at the `README.md` text file. On line 9 of the file you will  
see `# Version X.Y.Z`, where `X.Y.Z` is the version number. The USGS meta data file, `code.json`, also specifies  
the version number under the json record: `"version": "X.Y.Z"`.

If you only have a compiled executable binary, such a `mf-owhm.exe`, the version number is presented when running the program. The image below is a screenshot taken after starting the `mf-owhm.exe` program and underlines in red the code's version number (`X.Y.Z`). The figure also underlines in yellow the date that the version was released (`mm` is for month, `dd` is for day, and `yyyy` is for year). The part that is underline in indigo shows the modified version scheme that references only the major and minor versions (`X.Y`) and then calls the patch version a Release Number (`Z`).  



![download_arrow](img/cmd_prompt.png)

&nbsp;

## Git Branches and Organization

The git repository for MF-OWHM will always contain the following branches:

1. `main`  &emsp;&thinsp;&nbsp; ➢ &nbsp; Default branch with stable source code and no compiled binaries.
1. `release`&nbsp; ➢ &nbsp; *Preliminary/Production Release* of code and binaries for Windows (`.exe`) and Ubuntu Linux (`.nix`)
1. *Git Tags* that are pointers to specific versions (commits) of the code and compiled binaries.   
   &emsp;&ensp;The **[GitLab Releases Page](https://code.usgs.gov/modflow/mf-owhm/-/releases)** contains the important releases (that is, highlighting important git tags).  
   &emsp;&ensp;The **[GitLab Tags Page](https://code.usgs.gov/modflow/mf-owhm/-/tags)** contains a listing of all version tags in the repository (recommended for old version downloads).

There may be additional branches in the repository, but they are considered development-beta branches. The development-beta branches may or may not contain compiled executable binaries and are typically named `develop-feature` where `feature` is the name of the specific feature being added.

If a branch contains compiled executable binaries (`*.exe` and `*.nix` files), then they are placed in a `bin` directory.  
Note that there are multiple bin directories in this repository. 

Compiled executable binaries and the `examples/**/output-true` folders are stored using [Git Large File Storage (LFS)](https://git-lfs.github.com/). LFS is an open source Git extension that reduces the impact of large files stored in a git repository. Specifically, large files that are manage by LFS are downloaded during the `git checkout` (or `git switch`) process rather than during `git clone`, `git fetch`, or `git pull`. The USGS gitlab server, [code.usgs.gov](code.usgs.gov), includes a [GitLab Git LFS server](https://docs.gitlab.com/ee/topics/git/lfs/), so all downloads will automatically handle the LFS operations. 

If you `git clone` this repository without Git LFS installed, then you will see a pointer file in the place of the compiled binaries. If Git LFS is installed, the pointer file is swapped automatically with the appropriate executable binary. 
Note that the windows installation of Git (https://git-scm.com/downloads) includes Git-LFS as part of the installation.

## Git Workflow

The general workflow for the branching model used here is:

- Create a `develop` type branch from the `main` branch &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;`main ⤇ develop`
- Once the `develop` branch is stable, submit a merge request to the `main` branch&emsp;&emsp;&emsp;`develop ⤇ main`
- Merge the `main` branch to the `release` branch and build the executable binaries&emsp;&ensp;&ensp;`main ⤇ release`

  

To convert code from a *Preliminary Release* to *Production Release*:

- Register the `release` branch version with the USGS Information Product Data System (`IPDS`)
- Change the `release` branch's `DISCLAIMER.md` to the *Production Release Disclaimer*.
- Make an annotated `git tag` that points to that `release` branch's commit that was approved.
  - The the *git tag* `<tagname>` should be the approved code version number.
  - For important releases the *git tag* may be targeted as a GitLab Release.
- The *git tag* now points to a *Production Release*.

&nbsp;

## code.usgs.gov GitLab Website

The official USGS git server (code.usgs.gov) uses the [GitLab Community Edition](https://about.gitlab.com/install/?version=ce) (CE) server software.  
This section presents some of the basics for website navigation and use of GitLab.

For the https://code.usgs.gov/modflow/mf-owhm website the default branch is the `main` branch.  
The default branch is what is automatically created when you run git clone and what you see when you first open the website. 

### GitLab Download Button

Throughout `code.usgs.gov` repositories there are download buttons that looks like: ![download_arrow](img/code-usgs-gov_page_download_arrow.png)

The download button—depending on where it is placed—can download the current commit shown on on the webpage (eg the `main` branch) , a specific folder in the repository, a specific file in the repository, or a *GitLab Tag*.

The download has four options for how to compress the download: `zip`, `tar.gz`, `tar.bz` and `tar`.   
It is recommended to use the `zip` download option.

The following image shows the pull down menu that appears when you click on the download button:

<p>
<img src="img/code-usgs-gov_page_download.png" alt="code-usgs-gov_page_download" width="40%" height="40%">
</p>

The "`Download source code`" downloads more than just the source code. It instead downloads a specific git commit, such as the `release` or `main`, or a specific *git tag*. Typically, the "`Download source code`" downloads the currently active commit on the website. For example, if the `release` branch is active on the website, then the download will include everything in the `release` branch, which includes the source code, compiled executable binaries, example problems, documentation, etc, etc, etc.

The "`Download this directory`" downloads what is currently being shown on the website.

### GitLab Branch Switching

The https://code.usgs.gov/modflow/mf-owhm website presents the `main` branch by default. 

On the main page there is a pull-down menu that allows the user to select any of the repository branches or git tags. When these branches or tags are selected the entire website changes to view that specific branch or tag (which also changes what the download button will download). The image below highlights in yellow the pull-down menu that is currently set to the `release` branch.



![header-code-usgs-gov](img/code-usgs-gov_page_header.png)



### GitLab Tags Page

A *GitLab Tag* entry is automatically created whenever a *git tag* commit pointer is created on `code.usgs.gov`. Most of the information on the GitLab Tags Page can be ignored. What is important are the *Tag Names*, which are the code version number that the tag points to. 

The following figure highlights the [GitLab Tags Page](https://code.usgs.gov/modflow/mf-owhm/-/tags), which can be accessed by moving the mouse to the *Left sidebar* of the website to the "Repository" header and then by clicking on the "Tags" header (see the indigo box in the image). 

The tag for version `2.0.0` has a green box around it to illustrate its location. To the far right of that tag name is the download button (indigo box) that will download that specific version of the repository (viz. MF-OWHM Version `2.0.0` ).

&nbsp;

![tags-1-code-usgs-gov](img/code-usgs-gov_tags-1.png)

&nbsp;

The download has four options for how to compress the download: `zip`, `tar.gz`, `tar.bz` and `tar`.  

The simplest one to pick is the `zip` option, which is illustrated below:

&nbsp;

![tags-2-code-usgs-gov](img/code-usgs-gov_tags-2.png)

&nbsp;

The structure of tagged urls follows a consistent pattern. The base url for a `.zip` download is always:

- code.usgs.gov/modflow/mf-owhm/-/archive/**TagName**/mf-owhm-**TagName**.zip

where **TagName** is replaced by the actual tag's name. 

For example, the url for the download of versions `2.0.3` is:

- code.usgs.gov/modflow/mf-owhm/-/archive/**2.0.3**/mf-owhm-**2.0.3**.zip

&nbsp;

### GitLab Releases Page

A *GitLab Releases* entry is created for important  *git tag*s that require additional meta-data. Generally, it is best to either download the most current *Preliminary Release* from the [Downloads  Section](#downloads) or search for a specific version in the *GitLab Tags* Page described in the previous section. However, if you want to only see major updates or have direct access to special uploads, then they will be placed in the *GitLab Releases* page. An entry in the *GitLab Releases* page is automatically made whenever a  *git tag*  includes release comments, but all this can be obtained directly from the GitLab Tags Page.

The following figure highlights in yellow where the *GitLab Releases* link is located. The "assets" are download links that download the *git tag* repository that the release was created from. For example, the `Version 2.0.0` "Source Code (zip)" download link downloads the *git tag* `2.0.0`, which be `code.usgs.gov/modflow/mf-owhm/-/archive/2.0.0/mf-owhm-2.0.0.zip`. (Note that this is the same url as the ones present the *GitLab Tags* page.) 

The "Other" under Assets, highlighted in green, contain additional download links to files that may not be in the repository and were needed by the release. In this example, the additional files added are the `mf-owhm.exe`, `mf-owhm-gmg.exe`, and the `Full Repository`. These additional downloads, may or may not be present, so it is recommend to instead use the [Downloads](#downloads) section located at the start of this readme or search for the version you want to download in the [GitLab Tags Page](https://code.usgs.gov/modflow/mf-owhm/-/tags). <p>
<img src="img/code-usgs-gov_releases.png" alt="release-code-usgs-gov" width="80%" height="80%">
</p>

&nbsp;

## Visual Studio IDE and Intel Fortran Setup

The windows executable binary is compiled using Microsoft Visual Studio 2019 (vs2019) integrated development environment (IDE) and Intel OneAPI's Fortran 2018 standard (`ifort`) and ANSI C (`icc`). The vs2019 solution is located in the `ide/visual_studio` directory. Please review the [Visual Studio and Intel Fortran Installation Guide](doc/Visual_Studio_and_Intel_Fortran_Installation_Guide.md) for information on how to install and use both programs. The visual studio solution (input file) for mf-owhm is located in the ide/visual_studio directory.

To assist in compilation and development of source code; however, a set of input files for different IDEs are provided. 

Note that the repository also includes a `makefile` that is written in [GNU Make](https://www.gnu.org/software/make/) for compiling code directly (some of the IDEs use this makefile for their compilation process).

GNU gfortran is supported via a makefile, however the unit tests were completed using the Intel OneAPI compiler suite. 

&nbsp;

## Example Models

The `examples` directory contains the standard MODFLOW test cases and examples.

The example models are documented in: 

- Harbaugh, A. W. McDonald., MG 1996. User’s documentation for MODFLOW-96: An update to the US Geological Survey modular finite-difference groundwater flow model. US Geological Survey Open-File Report, 96-485.

- Harbaugh, A. W., Banta, E. R., Hill, M. C., & McDonald, M. G. (2000). Modflow-2000, the u. s. geological survey modular ground-water model-user guide to modularization concepts and the ground-water flow process. Open-file Report. U. S. Geological Survey, (92), 134.

- Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular ground-water model -- the Ground-Water Flow Process: U.S. Geological Survey Techniques and Methods 6-A16.

- Niswonger, R.G., Panday, Sorab, and Ibaraki, Motomu, 2011, MODFLOW-NWT, A Newton formulation for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A37, 44 p.

- Hughes, J.D., Langevin, C.D., Chartier, K.L., and White, J.T., 2012, Documentation of the Surface-Water Routing (SWR1) Process for modeling surface-water flow with the U.S. Geological Survey Modular Ground-Water Model (MODFLOW-2005): U.S. Geological Survey Techniques and Methods, book 6, chap. A40 (Version 1.0), 113 p.
  Additional information and resources on the SWR Process for MODFLOW

- Bakker, Mark, Schaars, Frans, Hughes, J.D., Langevin, C.D., and Dausman, A.M., 2013, Documentation of the seawater intrusion (SWI2) package for MODFLOW: U.S. Geological Survey Techniques and Methods, book 6, chap. A46, 47 p.

- Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey Techniques and Methods 6–A51, 120 p., https://dx.doi.org/10.3133/tm6A51

- Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60

&nbsp;

## Post Processors: ZoneBudget and HydFMT

[ZoneBudget](https://www.usgs.gov/software/zonebudget-a-program-computing-subregional-water-budgets-modflow-groundwater-flow-models) and [HydFMT](https://water.usgs.gov/nrp/gwsoftware/modflow/modflow-96h.htm) are two commonly used post-processing programs. 

The source code, makefile, and Visual Studio Project has been provided for compilation and use of these two post-processing programs. Included in the repository are MS Windows and Ubuntu Linux compiled versions.

**Windows x64 Download**

- [postprocessors/zonebudget/bin/zonebudget.exe](https://code.usgs.gov/modflow/mf-owhm/-/raw/release/postprocessors/zonebudget/bin/zonebudget.exe)
- [postprocessors/hydfmt/bin/hydfmt.exe](https://code.usgs.gov/modflow/mf-owhm/-/raw/release/postprocessors/hydfmt/bin/hydfmt.exe) 

**Ubuntu Linux x64 Download**

- [postprocessors/zonebudget/bin/zonebudget.nix](https://code.usgs.gov/modflow/mf-owhm/-/raw/release/postprocessors/zonebudget/bin/zonebudget.nix)
- [postprocessors/hydfmt/bin/hydfmt.nix](https://code.usgs.gov/modflow/mf-owhm/-/raw/release/postprocessors/hydfmt/bin/hydfmt.nix) 

**Citations**

- Harbaugh, A.W., 1990, [A computer program for calculating subregional water budgets using results from the U.S. Geological Survey modular three-dimensional ground-water flow model](https://pubs.er.usgs.gov/publication/ofr90392): U.S. Geological Survey Open-File Report 90-392, 46 p.

  

- Hanson, R.T., and Leake, S.A., 1999, [Documentation for HYDMOD, a program for extracting and processing time-series data from the U.S. Geological Survey's modular three-dimensional finite-difference ground-water flow model](https://pubs.er.usgs.gov/publication/ofr98564): U.S. Geological Survey Open-File Report 98-564, 57 p.

&nbsp;

## Key Root Directory Files

| Directory             | Description                                                  |
| --------------------- | ------------------------------------------------------------ |
| .fortls               | Input file for a [Fortran Language Server](https://github.com/hansec/fortran-language-server) used by vscode [FORTRAN IntelliSense Extension](https://marketplace.visualstudio.com/items?itemName=hansec.fortran-ls) |
| .gitattributes        | File that specifies the check-in/check-out behavior of git. <br />Specifies that the majority of files should keep Unix text formatting (`LF`) and only preservers windows text formatting (`CR` `LF`) for Visual Studio solution files.<br />Defines what files are managed by *Git LFS*. |
| .gitignore            | Specifies files that should be ignored and not tracked by git. |
| CHANGELOG.md          | Version log of changes to this code repository, this includes corrected code errors (bug fixes). |
| CHANGELOG_Features.md | Version log of new features added to the code repository.  <br />These are also included in the regular changelog, but this file contains a detailed explanation. |
| cleanRepo.sh          | Bash script that deletes files in `obj` and `lib` folders and the `examples/**/output` folders. |
| code.json             | Metadata file required by the USGS for code.usgs.gov         |
| DISCLAIMER.md         | Is either the USGS's *Production Release* or *Preliminary Release* disclaimer. |
| LICENSE.md            | License and copyright information regarding the repository.  |
| `makefile`            | Makefile for comping MF-OWHM with GNU Make. <br />Used by `CodeBlocks` and `VScode` for compiling and running debug sessions. |
| README.md             | The file you are currently reading.                          |
| readme.txt            | Basic explanation of markdown (`.md`) syntax and software with markdown rendering. |

&nbsp;

## Key Root Directory Names

| Directory         | Description                                                  |
| ----------------- | ------------------------------------------------------------ |
|                   |                                                              |
|                   |                                                              |
| bin               | May contains 64-bit executable binaries for Windows (`.exe`) and Ubuntu Linux (`.nix`). <br />Also, the location where the `makefile` and integrated development environments (ide) <br />are set to save compiled executable binaries at. |
| .vscode           | Workspace `.json` files necessary for using the [vscode](https://code.visualstudio.com/) integrated develop environment for comping MF-OWHM with `gfortran`.<br />The vscode workspace is in `ide/vscode/mf-owhm.code-workspace` |
| doc               | General text documents that may assist in model building. <br />Contains `FMP_Template`, `Notepad++ Syntax Highlighting`, and `Option Block Cheatsheets` |
| examples          | Example problems to illustrate model input and for unit testing for validating code changes. |
| icon              | MF-OWHM icon in a different image formats (`png`, `svg`, `ai`, and `ico`).<br />Also, contains Windows Resource (`rc`) file for comping the ico with the executable binary. |
| ide               | Integrated development environment (ide) files to assist with compiling. <br />The windows compiled executable uses Visual Studio 2019 Community Edition with Intel OneAPI-Fortran Compiler Classic. <br />Other IDEs are provided as is, but have not been tested. |
| ide/visual_studio | [Visual Studio Community Edition](https://visualstudio.microsoft.com/) solution (`.sln`) and project files (`.vcxproj` and `.vfproj`)<br />The IDE is setup using Intel Fortran Compiler Classic 2021.1.1 that is part of the Intel OneAPI. |
| ide/vscode        | Contains the file `mf-owhm.code-workspace` for loading the [vscode](https://code.visualstudio.com/) workspace properties <br />(which are specified in the `.vscode` folder). |
| img               | Images used in the readme.md and other `.md` files.          |
| lib               | Directory used by Visual Studio for making the `GMG.lib` from the gmg source files (`src/gmc_c`) |
| obj               | Git version control is set to keep this directory empty, but it is used for<br />intermediate object files during compilation (e.g. `.obj`, `.o`, and `.mod`). |
| postprocessors    | Compiled binary, source code, and visual studio input for `HydFMT` and `ZoneBudget` |
| src               | MF-OWHM Fortran and C code.                                  |

&nbsp;

------

&nbsp;

# Executive Summary

The U.S. Geological Survey’s MODFLOW One-Water Hydrologic Flow Model (MF-OWHM or OneWater) is a MODFLOW-based conjunctive use simulation software. The first version (Hanson et al., 2014) was selected by the World Bank Water Resource Software Review (Borden et al., 2016) as one of three recommended simulation programs for conjunctive use and management modeling. MF-OWHM builds upon the MODFLOW-2005 groundwater-flow simulation framework and multiple MODFLOW-2005 variants. This fusion of the MODFLOW-2005 variants and MF-OWHM specific enhancements allow for the following process-based simulations: saturated groundwater flow (three-dimensional); surface-water flow (one- and two-dimensional); landscape and irrigated agriculture; estimation of unknown agricultural irrigation and additional irrigation requirements for salinity flushing; reservoir operations and management; aquifer compaction and subsidence; seawater intrusion by a sharp-interface assumption; karst-aquifer and fractured-bedrock flow; and vertical unsaturated groundwater flow (one-dimensional).

![mf05_family_tree](img/mf05_family_tree.png)



Ground-Water Flow Model (MODFLOW-2005) is a computer program that simulates through finite differences the groundwater-flow equation (Harbaugh, 2005). The MODFLOW-2005 framework employed a modular design that allows for the easy development and incorporation of new features called packages and processes that communicate with the groundwater flow equation. Packages are code features that relate to the groundwater-flow process, e.g. subsidence; a process may solve a separate flow equation or represent a non-groundwater-related process, e.g. surface-water routing. With the development of new packages and processes, the MODFLOW-2005 base framework began to diverge into multiple independent versions designed for specific simulation needs. This divergence limited each independent MODFLOW release to its specific designed purpose, such that there was no longer a single, comprehensive, general-purpose hydraulic-simulation framework. MF-OWHM is an integrated hydrologic flow model that “integrates” into one cohesive simulation software multiple MODFLOW-2005 variants; enhancements were added that take advantage of multiple capabilities within one code. This enhanced fusion of MODFLOW-2005 versions resulted in a simulation software capable of addressing and understanding a diverse type of conjunctive-use, water-management, water-food-security, and climate-crop-water scenario analyses. 

While maintaining compatibility with existing MODFLOW-2005 versions, MF-OWHM includes:

- Process-based simulation that includes:
   - Saturated groundwater flow (three-dimensional)
   - Surface water flow (one- and two-dimensional)
       -  Stream and river flow
       -  Lake and reservoir storage 
   - Landscape simulation and irrigated agriculture 
     -  Land-use and crop simulation
     -  Root uptake of groundwater
     -  Precipitation
     -  Actual evapotranspiration
     -  Runoff 
     -  Infiltration
     -  Estimated irrigation demand
   - Reservoir operations
   - Aquifer compaction and subsidence with vertical model-grid deformation
   - Seawater intrusion with a sharp-interface assumption
   - Karst-aquifer and fractured-bedrock flow
   - Turbulent and laminar-pipe network flow
   - Unsaturated groundwater flow (one-dimensional)
- Internal linkages between the processes that couple hydraulic head, flow, and deformation,
- Facilitation of the self-updating-model concept (making it easy to maintain models),
- Additional observation, scale factors, and parameter options for advanced calibration,
- An interpretive computer language, S-Language, for constructing custom command sets for reservoir-operation rules and decision trees.

MF-OWHM is a MODFLOW-2005 based integrated hydrologic model (IHM) that allows for the simulation, analysis, and evaluation of management options from nearly all components of human and natural water movement through a physically-based supply-and-demand framework. The term integrated, in the context of this report, refers to the tight coupling of groundwater flow, surface-water flow, landscape processes, aquifer compaction and subsidence, reservoir operations, and conduit/karst flow. 

Another benefit of this IHM is that models developed to run with MODFLOW-2005, MODFLOW-NWT, MODFLOW-CFP, or MODFLOW-FMP can also be simulated with MF-OWHM. However, the current release of MF-OWHM does not include a direct, internal simulation of snowmelt; advanced mountainous-watershed rainfall-runoff simulation; detailed, shallow soil-moisture accounting; or atmospheric moisture content. Atmospheric moisture may be accounted for indirectly by, optionally, specifying a pan evaporation rate, reference evapotranspiration, and precipitation. These features are currently not included to ensure that simulation runtime remains small enough to enable automated methods of calibrating model parameters to field observations, which typically require a large number of model runs. The MF-OWHM philosophy is to include as detailed hydrological processes as possible, so long as simulation run times remain reasonable enough to allow for robust parameter estimation and model calibration. 

To represent both natural and human-influenced flow, MF-OWHM integrates physically-based flow processes—derived from MODFLOW-2005—into a supply and demand framework. The model simulates and tracks the movement and use of groundwater, surface water, precipitation, and imported water supply that meets the consumptive demands from irrigated and non-irrigated agriculture, natural vegetation, and urban water uses. Water consumption is determined by balancing the available water supply with water demand, leading to the concept of a demand-driven, supply-constrained simulation.  

The MF-OWHM Supply-and-Demand Framework is especially useful for the analysis of agricultural water use, where there is often a paucity of available data describing land-use changes over time, such as crop type and distribution, and associated groundwater pumpage. For example, MF-OWHM provides the option to automatically increase groundwater irrigation pumping, which often is unknown, to meet the residual demand after all sources of water supply have been exhausted in a particular region. Supply and demand regions are simulated in the software as Water Balance Subregions (WBS).

To achieve the physically-based supply and demand framework, the MODFLOW-2005 philosophy of no inter-package and -process communication was relaxed for MF-OWHM. Traditional MODFLOW simulation models required that all packages and processes communicate (i.e. the movement of water) either through the groundwater-flow equation or through the removal of water from the simulation domain. For example, the MODFLOW-2005 representation of a groundwater well extracts water by subtracting from the groundwater-flow equation and removes it from the simulation domain. This feature is available within the MF-OWHM Framework, but options were added to allow the user to specify a particular a use or destination of pumped groundwater within the model domain (e.g., irrigation, managed aquifer recharge, return-flow to streams). 

This repository contains the source code for MF-OWHM and compiled windows binary.



**Executive Summary References**

Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey Techniques and Methods 6-A51, 120 p., https://dx.doi.org/10.3133/tm6A51.

Borden, John Carter; Gaur, Anju; Singh, Chabungbam Rajagopal, 2016, Water resource software: application overview and review. Washington, D.C.: World Bank Group. 

&nbsp;

------

&nbsp;

# Publications Involving MODFLOW-OWHM

## Basic Documentation and Code Publications



- Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., [https://doi.org/10.3133/tm6A60](https://doi.org/10.3133/tm6A60)



- Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey Techniques and Methods 6–A51, 120 p., [http://dx.doi.org/10.3133/tm6A51](http://dx.doi.org/10.3133/tm6A51)



- Hanson, R.T., and Schmid, Wolfgang, 2013, Economic resilience through “One-Water” Management: U.S. Geological Survey Open-File Report 2013–1175, 2 p.



- Hanson, R.T., Kauffman, L.K., Hill, M.C., Dickinson, J.E., and Mehl, S.W., 2013, Advective transport observations with MODPATH-OBS—Documentation of the MODPATH observation process, using four types of observations and Predictions: U.S. Geological Survey Techniques and Methods book 6–chap. A42, 94 p.



- Maddock III, T., Baird, K.J., Hanson, R.T., Schmid, Wolfgang, and Ajami, H., 2012, RIP-ET: A Riparian Evapotranspiration Package for MODFLOW-2005, U.S. Geological Survey Techniques and Methods 6-A39 p. 39 (http://pubs.usgs.gov/tm/tm6a39/)



- Schmid, Wolfgang, and Hanson R.T., 2009, The farm process version 2 (FMP2) for MODFLOW-2005—Modifications and upgrades to FMP1: U.S. Geological Survey Techniques in Water Resources Investigations, book 6, chap. A32, 102 p.



- Schmid, W., Hanson, R.T., Maddock III, T.M., and Leake, S.A., 2006, User’s guide for the farm process (FMP) for the U.S. Geological Survey’s modular three-dimensional finite-difference ground-water flow model, MODFLOW-2000: U.S. Geological Survey Techniques and Methods 6–A17, 127 p.



- Schmid, W., 2004, A Farm Package for MODFLOW-2000: Simulation of Irrigation Demand and Conjunctively Managed Surface-Water and Ground-Water Supply; PhD Dissertation: Department of Hydrology and Water Resources, The University of Arizona, 278 p.



------



## Application Bibliography

### Highlighted Publications



- Traylor, J.P., Mashburn, S.L., Hanson, R.T., and Peterson, S.M., 2021, Assessment of water availability in the Osage Nation using an integrated hydrologic-flow model: U.S. Geological Survey Scientific Investigations Report 2020–5141, 96 p., https://doi.org/10.3133/sir20205141.

  

- Alattar, M., Troy, T., Russo, T. and Boyce, S. E., 2020, Modeling the surface water and groundwater budgets of the US using MODFLOW-OWHM. Advances in Water Resources, 143, p. 103682, https://doi.org/10.1016/j.advwatres.2020.103682 

  

- Hanson, R.T., Ritchie, A.B., Boyce, S.E., Galanter, A.E., Ferguson, I.A., Flint, L.E., Flint, A., and Henson, W.R., 2020, Rio Grande transboundary integrated hydrologic model and water-availability analysis, New Mexico and Texas, United States, and northern Chihuahua, Mexico: U.S. Geological Survey Scientific Investigations Report 2019–5120, 186 p., https://doi.org/10.3133/sir20195120.



### Publications



- Azeref, B. G., & Bushira, K. M., 2020, Numerical groundwater flow modeling of the Kombolcha catchment northern Ethiopia. Modeling Earth Systems and Environment, 6(2), 1233-1244.

  

- Ebrahim, G.Y., Villholth, K.G., Boulos, M., 2019, Integrated hydrogeological modelling of hard-rock semi-arid terrain: supporting sustainable agricultural groundwater use in Hout catchment, Limpopo Province, South Africa, Hydrogeology Journal, 17p., https://doi.org/10.1007/s10040-019-01957-6

  

- Hevesi, J. A., Henson, W. R., Hanson, R. T., & Boyce, S. E., 2019, Integrated hydrologic modeling of the Salinas River, California, for sustainable water management. SEDHYD 2019 Conference.

  

- Mohammed, K., 2019, MODFLOW-Farm Process Modeling for Determining Effects of Agricultural Activities on Groundwater Levels and Groundwater Recharge, J. Soil Groundwater Environ. Vl. 24, No. 5, p. 17-30, https://doi.org/10.7857/JSGE.2019.24.5.017

  

- Rossetto, R., De Filippis, G., Triana, F., Ghetta, M., Borsi, I., Schmid, Wolfgang, 2019, Software tools for management of conjunctive use of surface- and groundwater in the rural environment: integration of the Farm Process and the Crop Growth Module in the FREEWAT platform: Agricultural Water Management, Vol 223, No. 105717, 18p. (https://doi.org/10.1016/j.agwat.2019.105717)

  

- Ritchie, A.B., Hanson, R.T., Galanter, A.E., Boyce, S.E., Damar, N.A., and Shephard, Z.M., 2018, Digital hydrologic and geospatial data for the Rio Grande transboundary integrated hydrologic model and water-availability analysis, New Mexico and Texas, United States, and Northern Chihuahua, Mexico: U.S. Geological Survey data release, https://doi.org/10.5066/P9J9NYND

  

- Bushira, K. M., Hernandez, J. R., & Sheng, Z., 2017, Surface and groundwater flow modeling for calibrating steady state using MODFLOW in Colorado River Delta, Baja California, Mexico. Modeling Earth Systems and Environment, 3(2), 815-824.

  

- Borsi, I., Rossetto, R., Cannata, M., De Filippis, G., & Ghetta, M., 2016, Open Source for Water Management: including capabilities of MODFLOW-OWHM in the FREEWAT GIS modelling environment (No. e2209v2). PeerJ Preprints. https://peerj.com/preprints/2209.pdf

  

- Fowler, K.R., Jenkins, E.W., Parno, M. , Chrispell, J.C., Col´on, A.I., and Hanson, R.T., 2016, Development and Use of Mathematical Models and Software Frameworks for Integrated Analysis of Agricultural Systems and Associated Water Use Impacts: AIMS Agriculture and Food, Vol.1, No. 2, pp. 208–226, DOI: 10.3934/agrfood.2016.2.208 (http://www.aimspress.com/article/10.3934/agrfood.2016.2.208)

  

- Boyce, S.E., Nishikawa, T., and Yeh, W.G., 2015, Reduced order modeling of the Newton formulation of MODFLOW to solve unconfined groundwater flow: Advances in Water Resources, 83, pp. 250-262. http://dx.doi.org/10.1016/j.advwatres.2015.06.005

  

- Boyce, S.E., 2015, Model Reduction via Proper Orthogonal Decomposition of Transient Confined and Unconfined Groundwater-Flow: PhD Dissertation, Dept. of Civil Engineering, University of California at Los Angeles, 64p.

  

- Doble, Rebecca C., and Crosbie, Russell S., 2015, Towards best practice for modeling recharge and evapotranspiration in shallow groundwater environments, MODFLOW-OWHM: MODFLOW and More 2015: Modeling a Complex World – Integrated Modeling to Understand and Manage Water Supply, Water Quality, and Ecology, pp. 22 – 26

  

- Faunt, C.C., Stamos, C.L., Flint, L.E., Wright, M.T., Burgess, M.K., Sneed, Michelle, Brandt, Justin, Coes, A.L., and Martin, Peter, 2015, Hydrogeology, Hydrologic Effects of Development, and Simulation of Groundwater Flow in the Borrego Valley, San Diego County, California: U.S. Geological Survey Scientific-Investigations Report 2015-5150, 154 p.

  

- Ferguson, I.A., and Llewellyn, D., 2015, Simulation of Rio Grande Project Operations in the Rincon and Mesilla Basins: Summary of Model Configuration and Results, U.S. Bureau of Reclamation Technical Memorandum No. 86-68210–2015-05, 56p.

  

- Fowler, K., R., Jenkins, E.W., Ostrove, C., Chrispell, J.C., Farthing, M.W., Parnoe, M., 2015, A decision making framework with MODFLOW-FMP2 via optimization: determining trade-offs in crop selection: Environmental Modelling and Software, v. 69, p. 280-291, http://www.sciencedirect.com/science/article/pii/S1364815214003624

  

- Hanson, R.T., Traum J., Boyce, S.E., Schmid, W., Hughes, J.D, W. W. G., 2015, Examples of Deformation-Dependent Flow Simulations of Conjunctive Use with MF-OWHM. Ninth International Symposium on Land Subsidence (NISOLS), Nagoya, Japan, 6p.

  

- Hanson, R.T., Chávez-Guillen, R., Tujchneider, O., Alley, W. M., Rivera, A., Dausman, A., Batista, L., y Espinoza, M., 2015, Conocimientos Científico Básico y Técnico Necesarios para la Evaluación y el Manejo de SAT (Basic Scientific and Technical Knowledge Required for the Evaluation and Management of SAT), en Estrategia Regional para la Gestión de los Sistemas de Acuíferos Transfronterizos (SAT) en las Americas (Regional Strategy for the Management of Transboundary Aquifers Systems in the Americas), UNESCO/OEA--ISARM AMERICAS Book IV, A. Rivera ed., 205p., UNESCO, Paris, France.

  

- Knight, J.A., 2015, Use of an Integrated Hydrologic Model to Assess the Effects of Pumping on Streamflow in the Lower Rio Grande, Master’s Thesis, Department of Hydrology and Water Resources, University of Arizona, 117p.

  

- Mehl, S., Houk, E., Morgado, K., Reid, N., and Anderson, K., 2015, Agricultural Water Transfers in Northern California: Effects on Aquifer Declines, Energy, and Food Production: MODFLOW and More 2015: Modeling A Complex World - Integrated GroundWater Modeling Center, Golden, Colorado, May 31–June 3, 2015, p. 121-123. http://igwmc.mines.edu/conference/Mod2015/MM15_Proceedings.pdf

  

- Nava, A.P., Villanueva, C.C., Villarreal, F.C., Hanson, R.T., and Boyce, S.E., 2015, A New Integrated Hydrologic Model for Mexico Valley, Mexico City, Mexico. MODFLOW and More 2015: Modeling A Complex World - Integrated GroundWater Modeling Center, Golden, Colorado, May 31–June 3, 2015, p. 148. http://igwmc.mines.edu/conference/Mod2015/MM15_Proceedings.pdf

  

- Phillips, S.P., Rewis, D.L., and Traum, J.A., 2015, Hydrologic model of the Modesto Region, California, 1960–2004: U.S. Geological Survey Scientific Investigations Report, 2015–5045, 69 p., http://dx.doi.org/10.3133/sir20155045.

  

- Turnadge C.J., and Lamontagne S., 2015, A MODFLOW–based approach to simulating wetland–groundwater interactions in the South East region of South Australia, MODSIM2015 conference, 29/11/15–04/12/15, Broadbeach, Queensland, Australia.

  

- Turnadge C.J., and Lamontagne S., 2015, A MODFLOW-based approach to simulating wetland–groundwater interactions in the Lower Limestone Coast Prescribed Wells Area, Goyder Institute for Water Research Technical Report Series No. 15/12, Adelaide, South Australia.

  

- Zeiler, Kurt K., Bitner, Robert J., Krausnick, Marie, Weaver, Jeffery D., and Foged, Nathan, 2015, Sub-Regional Groundwater Flow Modeling of the Upper Big Blue Basin Using the MODFLOW-2005 Farm Process, MODFLOW-OWHM: MODFLOW and More 2015: Modeling a Complex World – Integrated Modeling to Understand and Manage Water Supply, Water Quality, and Ecology, pp. 64 – 69

  

- Hanson, R. T., and Sweetkind, Donald, 2014, Cuyama Valley, California hydrologic study -- An assessment of water availability: U.S. Geological Survey Fact Sheet 2014-3075, 4 p., http://dx.doi.org/10.3133/fs20143075.

  

- Hanson, R.T., Schmid, Wolfgang, Faunt, C.C., Lear, Jonathan, Lockwood, B., and Harich, C., 2014, Integrated hydrologic model of Pajaro Valley, Santa Cruz and Monterey Counties, California: U.S. Geological Survey Scientific Investigations Report 2014–5111, 166 p.

  

- Hanson, R.T., Lockwood, B., and Schmid, Wolfgang, 2014, Analysis of projected water availability with current basin management plan, Pajaro Valley, California: Journal of Hydrology, v. 519, p. 131–147.

  

- Hanson, R.T., Flint, L.E., Faunt, C.C., Gibbs, D.R., and Schmid, W., 2014, Hydrologic models and analysis of water availability in Cuyama Valley, California: U.S. Geological Survey Scientific Investigations Report 2014–5150, 151 p.

  

- Boyce, S.E., and Yeh, W.G., 2014, Parameter-independent model reduction of transient groundwater flow models: Application to inverse problems, Advances in Water Resources, 69, pp. 168–180, http://dx.doi.org/10.1016/j.advwatres.2014.04.009

  

- Russo, T.A, Fisher, A.T., and Lockwood, B.S., 2014, Assessment of Managed Aquifer Recharge Site Suitability Using a GIS and Modeling, Ground Water, pp.1-12, doi: 10.1111/gwat.12213

  

- Schmid, Wolfgang, Hanson, R.T., Hughes, J., Leake, S.A., and Niswonger, R., 2014, Feedback of land subsidence on the movement and conjunctive use of water resources: Environmental Modelling and Software, vol. 62, pp. 253-270, http://dx.doi.org/10.1016/j.envsoft.2014.08.006

  

- Traum, J.A., Phillips, S.P., Bennett, G.L., Zamora, Celia, and Metzger, L.F., 2014, Documentation of a groundwater flow model (SJRRPGW) for the San Joaquin River Restoration Program study area, California: U.S. Geological Survey Scientific Investigations Report 2014–5148, 151 p., http://dx.doi.org/10.3133/sir20145148.

  

- Hanson, R.T., Schmid, Wolfgang, Knight, Jake, and Maddock III, T., 2013, Integrated Hydrologic Modeling of a Transboundary Aquifer System — Lower Rio Grande: MODFLOW and More 2013: Translating Science into Practice, Golden, CO, June 2-6, 2013, 5p.

  

- Harter, T., and Morel-Seytoux, H., 2013, Peer review of the IWFM, MODFLOW and HGS Model Codes: Potential for water management applications in California’s Central Valley and other irrigated groundwater basins: Final Report, California Water and Environmental Modeling Forum, 112 p., Sacramento, California (http://www.cwemf.org/Pubs/index.htm)

  

- Quinn, N., Wainwright, H., Jordan, P., Zhou, Q., Birkholzer, J., 2013, Potential Impacts of Future Geological Storage of CO2 on the Groundwater Resources in California’s Central Valley, Simulations of Deep Basin Pressure Changes and Effect on Shallow Water Resources, Lawrence Berkeley National Laboratory, Final Project Report to the California Energy Commission, 111p. (http://escholarship.org/uc/item/83k284c3)

  

- Schmid, Wolfgang, Ali, Riasat, 2013, Application of the Farm Process to land-use change scenarios of the Lake Nowergup MODFLOW model. In: IAH 2013, 15–20 September, 2013, Perth. International Asssociation of Hydrogeologists, 2013. p. 84-85.

  

- Hanson, R.T., Flint, L.E., Flint, A.L., Dettinger, M.D., Faunt, C.C., Cayan, D., and Schmid, Wolfgang, 2012, A method for physically based model analysis of conjunctive use in response to potential climate changes: Water Resources Research, v. 48, 23 p., doi:10.1029/2011WR010774

  

- Liu, T. and Luo, Y., 2012, An empirical approach simulating evapotranspiration from groundwater under different soil water conditions: Journal of Environmental Earth Sciences, 11 p. (DOI 10.1007/s12665-012-1577-3)

  

- Russo, T.A, 2012, Hydrologic System Response to Environmental Change: Three Case Studies in California, PhD Dissertation, Department of Earth Sciences, University of California at Santa Cruz, 56p.

  

- Faunt, C. C., Hanson, R. T, Martin, P., Schmid, Wolfgang, 2011, Planned Updates and Refinements to the Central Valley Hydrologic Model, with an Emphasis on Improving the Simulation of Land Subsidence in the San Joaquin Valley, World Environmental and Water Resources Congress 2011: Bearing Knowledge for Sustainability, pp. 864-870, 2011, ASCE

  

- Porta, L., Lawson, P., Brown, N., Faunt, C., and Hanson R. 2011. Application of the Central Valley Hydrologic Model to Simulate Groundwater and Surface-Water Interaction in the Sacramento-San Joaquin Delta. Poster presentation at the California Water and Environmental Modeling Forum Annual Meeting. Pacific Grove, California.

  

- Schmid, Wolfgang, Dogrul , E.C., Hanson, R.T., Kadir, T.N., and Chung, F.I., 2011, Comparison of Simulations of Land-use Specific Water Demand and Irrigation Water Supply by MF-FMP and IWFM: California Department of Water Resources Technical Information Record TIR-2, 80p.

  

- Hanson, R.T., Flint, A.L., Flint, L.E., Faunt, C.C., Schmid, Wolfgang, Dettinger, M.D., Leake, S.A., Cayan, D.R., 2010, Integrated simulation of consumptive use and land subsidence in the Central Valley, California, for the past and for a future subject to urbanization and climate change, paper presented at the Eighth International Symposium on Land Subsidence (EISOLS), Queretaro, Mexico, IAHS Publ. 339, pp. 467-471.

  

- Hanson, R.T., Schmid, Wolfgang, Faunt, C.C., and Lockwood, B., 2010, Simulation and analysis of conjunctive use with MODFLOW’s Farm Process: Ground Water v. 48, no. 5, p. 674–689. (DOI: 10.1111/j.1745-6584.2010.00730.x)

  

- Faunt, C.C., ed., 2009, Groundwater availability of the Central Valley Aquifer, California: U.S. Geological Survey Professional Paper 1766, 225 p.

  

- Faunt, C.C., Hanson, R.T., Belitz, Kenneth, and Rogers, Laurel, 2009, California’s Central Valley Groundwater Study: A Powerful New Tool to Assess Water Resources in California's Central Valley: U.S. Geological Survey Fact Sheet 2009-3057, 4 p. ( http://pubs.usgs.gov/fs/2009/3057/)

  

- Schmid, W., King, J.P., and Maddock III., T.M., 2009, Conjunctive Surface-Water / Ground-Water Model in the Southern Rincon Valley using MODFLOW-2005 with the Farm Process, prepared for the Elephant Butte Irrigation District, Las Cruces, NM; New Mexico Water Resources Research Institute Completion Report No. 350.

  

- Schmid, W., King, J.P., and Maddock, T.M., III, 2009, Conjunctive surface-water/ground-water model in the southern Rincon Valley using MODFLOW-2005 with the farm process: Las Cruces, N. Mex., New Mexico Water Resources Research Institute Technical Report, no. 350.

  

- Faunt, CC, Hanson, RT, Schmid, W, Belitz, K, 2008, Application of MODFLOW’s Farm Process to California’s Central Valley, Modflow and More—Ground Water and Public Policy Conference Proceedings, 496-500, 2008.

  

- Hanson, R.T., Schmid, Wolfgang, Leake, SA, 2008, Assessment of Conjunctive Use Water-Supply Components Using Linked Packages and Processes in MODFLOW: Modflow and More–Ground Water and Public Policy, Golden, Colorado, 5 p.

  

- Hanson, R.T, Schmid, Wolfgang, Lear, Jonathan, Faunt, Claudia C, 2008, Simulation of an Aquifer-Storage-and-Recovery (ASR) System for Agricultural Water Supply using the Farm Process in MODFLOW for the Pajaro Valley, Monterey Bay, California, Modflow and More—Ground Water and Public Policy, pp. 501-505.

  

- Schmid, W, Hanson, RT, Faunt, CC, Phillips, SP, 2008, Hindcast of water availability in regional aquifer systems using MODFLOW’s Farm Process,Hydropredict 2008 Conference Proceedings, pp. 311-314.

  

- Schmid, W., and Hanson, R.T., 2007, Simulation of Intra- or Trans-Boundary Water-Rights Hierarchies using the Farm Process for MODFLOW-2000, ASCE Journal of Water Resources Planning and Management , Vol. 133, No. 2, pp. 166-178 (DOI: 10.1061/(ASCE)0733-9496(2007)133:2(166))

  

- Schmid, W., Hanson, R.T., Maddock III, T., 2006, Overview and Advancements of the Farm Process for MODFLOW-2000, Modflow and More - Managing Ground-Water Systems, Golden, Colorado, pp. 23-27.

  

- Tillery, S., and King, J.P., 2006, MODFLOW-2000 farm package case study: Southern Rincon Valley, New Mexico: Technical Report prepared for the Las Cruces, N. Mex., U.S. Army Corps of Engineers, New Mexico State University, Department of Civil & Geological Engineering.

&nbsp;

------

&nbsp;

## Code Comparisons



- Department of Water Resources. 2020. Draft Handbook for Water Budget Development: With or Without Models (Water Budget Handbook). Sacramento, CA: California Department of Water Resources. 446 pp. Available online at: https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/Groundwater-Management/Data-and-Tools/Files/Water-Budget-Handbook.pdf.

  

- Borden, C., Gaur, A., and Singh, C, 2016, Water Resource Software – Application overview and Review: World Bank, South Asia Water Initiative, March, 2016, 76p.California 

  

- Moran, Tara, 2016, PROJECTING FORWARD A Framework for Groundwater Model Development Under the Sustainable Groundwater Management Act: Stanford Water in the West, November, 2016, 56 p., https://waterinthewest.stanford.edu/sites/default/files/Groundwater-Model-Report.pdf 

  

- Harter, T., and Morel-Seytoux, H., 2013, Peer review of the IWFM, MODFLOW and HGS Model Codes: Potential for water management applications in California’s Central Valley and other irrigated groundwater basins: Final Report, California Water and Environmental Modeling Forum, 112 p., Sacramento, California (http://www.cwemf.org/Pubs/index.htm)

  

- Dogrul , E.C., Schmid, Wolfgang, Hanson, R.T., Kadir, T.N., and Chung, F.I., 2011, Integrated Water Flow Model and Modflow-Farm Process: A Comparison of Theory, Approaches, and Features of two Integrated Hydrologic Models: California Department of Water Resources Technical Information Record, TIR-1, 80p.

  

- Schmid, Wolfgang, Dogrul , E.C., Hanson, R.T., Kadir, T.N., and Chung, F.I., 2011, Comparison of Simulations of Land-use Specific Water Demand and Irrigation Water Supply by MF-FMP and IWFM: California Department of Water Resources Technical Information Record TIR-2, 80p.

&nbsp;

------

&nbsp;

# USGS Software Disclaimers

As described in the [USGS Software Release Types section](#usgs-software-release-types) there code.usgs.gov have two different release types (*Production Release* and *Preliminary Release*). One of the key differences between the two release types is the text in the `DISCLAIMER.md` file.

The following is the text that is included in the two disclaimer types.

**Production Release Disclaimer**

> This software has been approved for release by the U.S. Geological Survey  
> (USGS). Although the software has been subjected to rigorous review, the USGS  
> reserves the right to update the software as needed pursuant to further analysis  
> and review. 
>
> No warranty, expressed or implied, is made by the USGS or the U.S. Government   
> as to the functionality of the software and related material nor shall the   
> fact of release constitute any such warranty. 
>
> Furthermore, the software is released on condition that neither the USGS nor   
> the U.S. Government shall be held liable for any damages resulting from its   
> authorized or unauthorized use.

&nbsp;

**Preliminary Release Disclaimer**

> This software is preliminary or provisional and is subject to revision. It is  
> being provided to meet the need for timely best science. The software has not  
> received final approval by the U.S. Geological Survey (USGS). 
>
> No warranty, expressed or implied, is made by the USGS or the U.S. Government   
> as to the functionality of the software and related material nor shall the   
> fact of release constitute any such warranty. 
>
> The software is provided on the condition that neither the USGS nor   
> the U.S. Government shall be held liable for any damages resulting from the   
> authorized or unauthorized use of the software.

&nbsp;

**This is the current commit's [DISCLAIMER.md](DISCLAIMER.md)**

&nbsp;

# MF-OWHM Disclaimer

Although the software has been subjected to rigorous review, it represents a specific compilation for the source code presented in https://code.usgs.gov/modflow/mf-owhm for that specific commit and version number. 

Binary versions may not contain bug fixes that are applied to a higher version number. Generally the highest available version number plus letter will contain the most up to date set of bug fixes, features, and had the most historical code review. Lower version binaries are kept on this repository to have a historical record and easy access to compiled binaries used in the past. 

The https://code.usgs.gov/modflow/mf-owhm.git code maintainers and developers (MF-OWHM development team) reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the MF-OWHM development team, USGS, or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the MF-OWHM development team, USGS, nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.

&nbsp;

# License

MF-OWHM is licensed under the Apache License, Version 2.0. Please see the [LICENSE.md](LICENSE.md) file for the full license information.

In short summary, MF-OWHM is an opensource project that is free to use without warranties or conditions of any kind.

The following is the Apache license boilerplate notice:

&nbsp;

```
  Copyright 2020 U.S. Geological Survey
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
  
      http://www.apache.org/licenses/LICENSE-2.0
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
```

&nbsp;

# Non-Endorsement of commercial products

```
Any use of trade, firm, or product names is for descriptive purposes only 
and does not imply endorsement by the U.S. Government.
```
