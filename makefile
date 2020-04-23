#
# MAKEFILE for compiling One-Water Hydrologic Flow Model (MF-OWHM)
#
# Developed by Scott E Boyce <seboyce@usgs.gov>
#
# If you use MF-OWHM, this MAKEFILE, a derivative of this makefile 
#   please include in any publications the following citations:
#
#    Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60
#
#    Boyce, S.E., 2020, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.0.x: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS
#
# PROVIDED AS IS WITHOUT WARRANTY OR HELP.
#
# See next section to set the compiler type variables
#   The initial setup has the compiler type variables set to the 
#   GNU gfortran AND gcc to make a debug x64 version
#
# To Invoke/Compile type from command prompt:
# make                  Compile the src files and testing files. Binary located in testing/bin
# make run              Same as above, but runs the resulting test binary. Arguments to the test binary are set by including ARG=XYZ and replacing XYZ with the argument lest. For example:   make run ARG=XYZ
# make clean            Delete all object (.o) and module (.mod) files --> Note this uses the Unix find command and not Windows
# make reset            Same as clean, except deletes all binaries.    --> Note this uses the Unix find command and not Windows
#
# Specify debug or release to run without modifying the makefile:
# make CONFIG=debug       or
# make CONFIG=release     or
# make run CONFIG=debug   or
# 
# make COMPILER=gcc F90=gfortran       or
# make COMPILER=gcc F90=gfortran CONFIG=debug
#
# Accepted keywords are (Note that all have defaults within this script):
#     CONFIG    => debug or release
#     COMPILER  => GCC or INTEL or LLVM  --> Indicates the compiler collection used for auto setting compiler flags
#     F90       => gfortran or ifort
#     CC        => gcc or icc
#
#     bin_out   => Location and Name (plus extension) of the final program. If not specified, then bin_out = $(bin_dir)/$(PROGRAM)$(ext)
#     src_dir   => Location of the source files                      -- Do not include a trailing / (that is, ./out1/out2)
#     int_dir   => Location of the intermediate files (.o and .mod)  -- Do not include a trailing / (that is, ./out1/out2)
#     test_dir  => Location to run program when using "make run"     -- Do not include a trailing / (that is, ./out1/out2)
#
#     ARG       => Arguments passed to program when using "make run"
#     PROGRAM   => Name file program without extension (note variable is ignored if bin_out is set).
#     bin_dir   => Location to place final program (note variable is ignored if bin_out is set).      -- Do not include a trailing / (that is, ./out1/out2)
#
#   --note all these variables should have detailed explanations where they are defined in this makefile.
#   --bin_out overrides the default binary name with the user specified one, that is: $(F90) -o $(bin_out)
#
# If you want to use Intel Fortran on Windows 10 
#    then run the makefile in the Intel Command Prompt (for example, run from the start menu: Compiler 19.1 Update 1 for Intel 64 Visual Studio 2019 environment)
#    or you will get path or license errors from Intel.
#    -- Linux Intel Fortran works fine with this makefile.
#
#         
#############################################################################
#############################################################################
###   Set the following Variables                                         ###
#############################################################################
#############################################################################
#
# Compilation Configuration-Optimization Scheme
#   ===> Accepted Answers: RELEASE, DEBUG
CONFIG := RELEASE
#
# Decide if you want to compile the GMG solver.
#  This is the only C code dependency. If set to No then the C compiler is not used.)
#  ===> Accepted Answers: YES, NO
USEGMG := NO
#
# Compilation Software                        --> Indicates the compiler collection used for setting compiler flags
#   ===> Accepted Answers: INTEL, GCC, LLVM       --> LLVM not fully-supported yet
COMPILER := INTEL
#
# Define the Fortran Compiler
#                    ===> For example: gfortran, gfortran-9, ifort
#                         ****Note that the version of your Fortran compiler may not support all of the Fortran Standards (viz 2003, 2008, 2015)
F90 := ifort
#
# Define the C Compiler
#   ===> Accepted Answers: gcc, icc
CC  := icc
#
# Program Name - Do not include extension (eg .exe). Also _debug will automatically be added if CONFIG = debug. Use bin_out= to specify exact location and name for binary.
#
PROGRAM := mf-owhm
#
# Compile STATICALLY? That is, no library dependence and compile with STATIC options.
#   ===> Accepted Answers: YES, NO
STATIC := YES
#
# Should compilation force DOUBLE PRECISION for all REAL variables? That is: REAL => DOUBLE PRECIONS.
# ===> Accepted Answers: YES, NO
#
DBLE := YES
#
# Pass Command Arguments when running executable.
#  This is only used for "make run", for example "make run ARG=Name.nam"
ARG = 
#
#
########################################################################
########################################################################
#             DO NOT MODIFY BELOW THIS LINE
#            UNLESS YOU KNOW WHAT YOUR DOING
########################################################################
########################################################################
#
########################################################################
#
#
#Define final BIN, SOURCE, and Testing directories => Can be blank, but do not include the trailing forward slash /
#
bin_dir ?=./bin
#
src_dir ?=./src
#
cfp_src_dir :=$(src_dir)/cfp
fmp_src_dir :=$(src_dir)/fmp
gmg_src_dir :=$(src_dir)/gmg_c
mfb_src_dir :=$(src_dir)/modflow_base
utl_src_dir :=$(src_dir)/utility_lib
#
#src_slg := $(src)/slang
#src_swo := $(src)/swo
#
# Location where test files are run
test_dir ?=./examples
#
# define the intermediate directory object and mod files are placed here
#
obj_dir :=$(strip $(shell echo $(CONFIG) | tr A-Z a-z))_$(strip $(shell echo $(F90) | tr A-Z a-z))
int_dir ?=./obj/$(obj_dir)
#int_dir:= ./obj
#
#
#############################################################################
#
#Define the source code directory search path (really only need source directories)
#
#
VPATH := $(src_dir)     $(int_dir)     $(bin_dir)     \
         $(cfp_src_dir) $(fmp_src_dir) $(gmg_src_dir) \
         $(mfb_src_dir) $(nwt_src_dir) $(utl_src_dir) \
         $(slg_src_dir) $(swo_src_dir)
#
#
#############################################################################
#
# Construct a function for converting space delimited variables to new lines

null :=
sp := ${null} ${null}
sp2 := ${null}  ${null}
sp4 := ${null}    ${null}
#${sp} := ${sp}    # ${sp}  function returns a space
#${sp2} := ${sp2}  # ${sp2} function returns 2 spaces

#funct -> @echo -e " $(subst ${sp},\n,${text})"
#
#
#############################################################################
#
# Define the C source files.
#   This is only enabled if GMG is going to be included in compilation
# 
ifeq ($(strip $(shell echo $(USEGMG) | tr a-z A-Z)), YES)
  #
  GMG_src := $(gmg_src_dir)/r_vector.c     \
             $(gmg_src_dir)/solvers.c      \
             $(gmg_src_dir)/ccfd.c         \
             $(gmg_src_dir)/mf2kgmg_OWHM.c
  #                                               GMG_obj       := $(patsubst %.c,%.o,$(notdir $(GMG_src)))
  GMG_f_file := gmg_dble.f
else
  GMG_src    := 
  GMG_f_file := 0_nogmg.f
endif
#
#############################################################################
#
# Define the Fortran source files.
#
util_src:= \
           $(utl_src_dir)/generic_open_module.fpp   \
           $(utl_src_dir)/all_util.f90
#
main_src:= \
           $(mfb_src_dir)/globalmodule.f90           \
           $(mfb_src_dir)/gwfBASmodule.f90           \
           $(mfb_src_dir)/gwflgrmodule.f             \
           $(mfb_src_dir)/lmt8_module.f              \
           $(mfb_src_dir)/mach_mod.f90               \
           $(mfb_src_dir)/mhc7.f                     \
           $(mfb_src_dir)/modules.f90                \
           $(mfb_src_dir)/ExpressionParser.f90       \
           $(mfb_src_dir)/pak_prop_interface.f90     \
           $(mfb_src_dir)/utility_pack_specific.f90  \
           $(mfb_src_dir)/linefeed.f90               \
           $(mfb_src_dir)/tabfilemodule.f            \
           $(mfb_src_dir)/gwfsfrmodule.f90           \
           $(mfb_src_dir)/gwflakmodule.f90           \
           $(mfb_src_dir)/gwfuzfmodule.f90           \
           $(mfb_src_dir)/sfr_name_to_seg.f90        \
           $(mfb_src_dir)/utl7_sav_subroutines.f90   \
           $(mfb_src_dir)/NWT1_module.f90            \
           $(mfb_src_dir)/NWT1_gmres.f90             \
           $(mfb_src_dir)/NWT1_ilupc_mod.f90         \
           $(mfb_src_dir)/NWT1_xmdlib.f              \
           $(mfb_src_dir)/NWT1_xmd.f                 \
           $(mfb_src_dir)/gwf2upw1.f                 \
           $(mfb_src_dir)/NWT1_solver.f              \
           $(gmg_src_dir)/gmg7_c_interface.f90       \
           $(gmg_src_dir)/$(GMG_f_file)              \
           $(mfb_src_dir)/pcg7R.f                    \
           $(mfb_src_dir)/pcgn_solve2.f90            \
           $(mfb_src_dir)/pcgn2.f90                  \
           $(mfb_src_dir)/sip7.f                     \
           $(mfb_src_dir)/gwf2bas8.f                 \
           $(mfb_src_dir)/gwf2bcf7.f                 \
           $(mfb_src_dir)/gwf2chd7.f                 \
           $(mfb_src_dir)/gwf2ibs7.f                 \
           $(mfb_src_dir)/gwf2sub7_OWHM.f            \
           $(mfb_src_dir)/gwf2swt7.f                 \
           $(mfb_src_dir)/de47.f                     \
           $(mfb_src_dir)/gwf2drn7_OWHM.f            \
           $(mfb_src_dir)/gwf2ets7.f                 \
           $(mfb_src_dir)/gwf2evt7.f                 \
           $(mfb_src_dir)/gwf2fhb7.f                 \
           $(mfb_src_dir)/gwf2gag7.f                 \
           $(mfb_src_dir)/gwf2huf7.f                 \
           $(mfb_src_dir)/hufutl7.f                  \
           $(mfb_src_dir)/gwf2lpf7.f                 \
           $(mfb_src_dir)/gwf2lak7.f                 \
           $(mfb_src_dir)/gwf2mnw17.f                \
           $(mfb_src_dir)/gwf2mnw27.f                \
           $(mfb_src_dir)/gwf2mnw2i7.f               \
           $(fmp_src_dir)/FMP_Block_Interface.f90    \
           $(fmp_src_dir)/FMP_Well_Interface.f90     \
           $(fmp_src_dir)/fmp_global.f90             \
           $(mfb_src_dir)/parutl7.f                  \
           $(mfb_src_dir)/gwf2rch7.f                 \
           $(mfb_src_dir)/gwf2res7.f                 \
           $(mfb_src_dir)/gwf2rip4.f90               \
           $(mfb_src_dir)/gwf2riv7_OWHM.f            \
           $(mfb_src_dir)/gwf2sfr7_OWHM.f            \
           $(mfb_src_dir)/gwf2str7.f                 \
           $(mfb_src_dir)/gwf2swi27.f                \
           $(mfb_src_dir)/gwf2swr7.f                 \
           $(mfb_src_dir)/gwf2swr7util.f             \
           $(mfb_src_dir)/utl7.f                     \
           $(mfb_src_dir)/gwf2uzf1.f                 \
           $(mfb_src_dir)/gwf2bfh2.f                 \
           $(mfb_src_dir)/gwf2drt7.f                 \
           $(mfb_src_dir)/gwf2ghb7_OWHM.f            \
           $(mfb_src_dir)/gwf2hfb7_OWHM.f            \
           $(mfb_src_dir)/gwf2hydmod71.f             \
           $(mfb_src_dir)/obs2bas7.f                 \
           $(mfb_src_dir)/obs2chd7.f                 \
           $(mfb_src_dir)/obs2drn7.f                 \
           $(mfb_src_dir)/obs2drt.f                  \
           $(mfb_src_dir)/obs2ghb7.f                 \
           $(mfb_src_dir)/obs2riv7.f                 \
           $(mfb_src_dir)/solver_rp.f                \
           $(mfb_src_dir)/gwf2wel8_OWHM.f90          \
           $(mfb_src_dir)/gwf2wel7.f                 \
           $(mfb_src_dir)/gwf2lgr2.f                 \
           $(mfb_src_dir)/lmt8_OWHM.f                \
           $(fmp_src_dir)/FMP_MAIN.f90               \
           $(cfp_src_dir)/cfp2ar.f                   \
           $(cfp_src_dir)/cfp2bd.f                   \
           $(cfp_src_dir)/cfp2fm.f                   \
           $(cfp_src_dir)/cfp2oc.f                   \
           $(cfp_src_dir)/cfp2rp.f                   \
           $(cfp_src_dir)/cfp2utl.f                  \
           $(cfp_src_dir)/cfp2_TM_utl.f              \
           $(src)OWHM_Main.f90
#
owhm_src := $(GMG_src) $(util_src) $(main_src)
#
obj:=  $(patsubst               %.f90, %.o,   \
         $(patsubst             %.f,   %.o,   \
           $(patsubst           %.fpp, %.o,   \
             $(patsubst         %.c,   %.o,   \
               $(patsubst       %.for, %.o,   \
                 $(patsubst     %.F,   %.o,   \
                   $(patsubst   %.F90, %.o,   \
                     $(patsubst %.FOR, %.o,   \
                            $(notdir $(owhm_src))  \
        ) ) ) ) ) ) ) )
#
#obj:= $(patsubst %.f90,%.o,$(patsubst %.f,%.o,$(src)))
#
########################################################################
#
# Check for if Windows or Unix
#
#
ifeq ($(OS),Windows_NT)
    ext:=.exe
else
    ext:=.nix
endif
#
########################################################################
#
# Check for if Windows and set resource file for icon  -> windres ./icon/owhm_icon_win.rc  ./icon/owhm_icon_win.res
#
#  --> windres and gfortran can have issues so skipping makefile icon generation
#
###icon_rc  := ./icon/owhm_icon_win.rc
###icon_res := ./icon/owhm_icon_win.res
####
###ifeq ($(OS),Windows_NT)
###    res:=$(shell windres $(icon_rc) $(icon_res) || echo )
###	ifeq ($(res), )
###        res:=$(icon_res)
###    else
###        res:=
###    endif
###    
###else
###        res:=
###endif
#
########################################################################
#
# Set up new line echo commands -- Replaced with printf
#
#
# ------------------------------------
###ifeq ($(OS),Windows_NT)
###    echoNL:="echo -e" 
###else
###    echoNL:="echo"
###endif
# ------------------------------------
define echo2
            @echo; echo
endef
# ------------------------------------
define echo3
            @echo; echo; echo
endef
# ------------------------------------
define echo4
            @echo; echo; echo; echo
endef
# ------------------------------------
#
#
########################################################################
#
# Remove blanks to ensure checks match to names
#
CFG     :=$(strip $(shell echo $(CONFIG) | tr [:lower:] [:upper:]))
CMPLR   :=$(strip $(shell echo $(COMPILER) | tr a-z A-Z))
STATIC  :=$(strip $(shell echo $(STATIC) | tr a-z A-Z))
DBLE    :=$(strip $(shell echo $(DBLE)   | tr a-z A-Z))
F90     :=$(strip $(F90))
CC      :=$(strip $(CC))
PROGRAM :=$(strip $(PROGRAM))
#
########################################################################
#
# Set up names and optimizations depending on compiler and configuration
#
ifeq ($(CFG), DEBUG)
   #
   PROGRAM:=$(PROGRAM)_debug
   #
   F90FlagsIntel :=-O0 -g -debug -traceback -assume nocc_omp -fpe0 -fp-model source -nologo -warn nousage -check bounds,pointers,stack,format,output_conversion,uninit
   F90FlagsGCC   :=-O0 -g -w -fbacktrace -fdefault-double-8  -ffree-line-length-2048 -fmax-errors=10 -ffpe-trap=zero,overflow,underflow -finit-real=nan #-fstack-usage  #<= THIS PROVIDES LOTS OF INFO   -std=f2008
   F90FlagsLLVM  :=
   #
   CFlagsIntel   :=-O0 -debug -g  -fbuiltin 
   CFlagsGCC     :=-O0        -g  -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
   CFlagsLLVM    :=
else
   # NOTE "-ip" can sometimes cause catastrophic error: **Internal compiler error:
   #
   F90FlagsIntel :=-O2 -assume nocc_omp -fpe0 -fp-model source -threads -warn nousage -nologo
   F90FlagsGCC   :=-O2 -w -fno-backtrace -fdefault-double-8 -ffree-line-length-2048
   F90FlagsLLVM  :=
   #
   CFlagsIntel   :=-O2  -fbuiltin
   CFlagsGCC     :=-O2  -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
   CFlagsLLVM    :=
endif
#
########################################################################
#
# Check if DBLE == YES to add default REAL-8 options
#
ifeq ($(DBLE), YES)
   #
   F90FlagsIntel+= -real-size 64                        
   F90FlagsGCC  += -fdefault-real-8                  
   F90FlagsLLVM += -fdefault-real-8
endif
#
########################################################################
#
# Establish Proper Optimization Flags and Static Flags
#
# Ensure that variables are set if STATIC not in use
ifneq ($(strip $(STATIC)), YES)
  STATIC:=
  STATICLNK:=
endif
#
# STATICLNK+= -static-libgfortran  => This can cause problems with gcc runtime so best not to use
ifeq ($(CMPLR), GCC)
  mod:=-J$(int_dir)
  F90FLAGS:=$(F90FlagsGCC)
  #
  ifdef GDB
       F90FLAGS += -ggdb3
  endif
  #
  ifeq ($(strip $(STATIC)), YES)
    STATIC   :=-static -static-libgfortran -static-libgcc  -static-libstdc++
    STATICLNK:=-static -static-libgfortran -static-libgcc  -static-libstdc++
  endif
  #
  CFLAGS:=$(CFlagsGCC)
endif
#
ifeq ($(CMPLR), INTEL)
  mod:=-module $(int_dir)
  F90FLAGS:=$(F90FlagsIntel)
  ifeq ($(strip $(STATIC)), YES)
    STATIC   :=-static -static-intel -qopenmp-link=static -static-libstdc++ -static-libgcc
    STATICLNK:=-static -static-intel -qopenmp-link=static -static-libstdc++ -static-libgcc
  endif
  #
  CFLAGS:=$(CFlagsINTEL)
endif
#
ifeq ($(CMPLR), LLVM)
  mod:=-module $(int_dir)
  F90FLAGS:=$(F90FlagsLLVM)
  #
  ifdef GDB
       F90FLAGS += -ggdb3
  endif
  #
  ifeq ($(strip $(STATIC)), YES)
    STATIC   :=-static-flang-libs 
    STATICLNK:=-static-flang-libs 
  endif
  #
  CFLAGS:=$(CFlagsLLVM)
endif
#
#
########################################################################
#
# Remove Variable Blank Space For Cleaner Output
#
F90FLAGS :=$(strip $(F90FLAGS))
CFLAGS   :=$(strip $(CFLAGS))
STATIC   :=$(strip $(STATIC))
STATICLNK:=$(strip $(STATICLNK))
#
########################################################################
#
#SET UP PROGRAM NAME
#
bin_out ?= $(bin_dir)/$(PROGRAM)$(ext)
#
###########################################################################
###########################################################################
#     DEFINE ALL TASK FUNCTIONS                                         ###
#     THE FOLLOW TARGETS EVALUATE THE COMPILATION                       ###
###########################################################################
###########################################################################
#
#
all: 
	@$(MAKE) --no-print-directory runMake || $(MAKE) --no-print-directory errorClean
#
run: 
	@$(MAKE) --no-print-directory runTest || $(MAKE) --no-print-directory errorClean
#
rebuild: quietClean all
#
compile: prinLink:=NO
compile: preClean $(int_dir) $(bin_out) completed
#
runMake: prinLink:=YES
runMake: startMSG preClean CompFlags $(int_dir) $(bin_out) dashes completed
#
runTest: runMake
	${echo2}
	@echo "Now running program to evaluate tests."
	${echo2}
	cd $(test_dir) ; $(abspath $(bin_out)) $(ARG)
	${echo3}
	@echo "   MAKEFILE TESTS COMPLETE"
	${echo2}
#
startMSG:
	${echo2}
	@echo "                 $(CFG) COMPILATION"
	@echo
	@echo "                        OF"
	@echo
	@echo "                     MF-OWHM"
	${echo2}
#
CompFlags:
	@echo "--------------------------------------------------------------------------------"
	@echo 
	@echo "Starting compilation with the following flags: "
	${echo2}
	@echo " $(F90):"
	@printf "    $(subst ${sp},\n${sp4},$(F90FLAGS) $(mod) $(STATIC))"
	${echo3}
	@echo " $(CC):"
	@printf "    $(subst ${sp},\n${sp4},$(CFLAGS) $(STATIC))"
	${echo3}
	@echo "Compiled object, module, and submodule files will be placed in:"
	@echo; echo "    $(realpath $(int_dir))"
	${echo2}
	@echo "================================================================================"
	${echo2}
#
preClean:
	@rm -rf $(bin_out) 
#
$(int_dir):
	@mkdir -p $@
#
$(bin_out): $(addprefix $(int_dir)/,$(obj))
	@echo
	@if [ "$(prinLink)" = "YES" ]; then                                                           \
	   echo                                                                                    ;  \
	   echo "================================================================================" ;  \
	   echo; echo                                                                              ;  \
	   echo "OBJECTS HAVE BEEN CREATED NOW LINKING FINAL BINARY:"                              ;  \
	   echo                                                                                    ;  \
	   echo "                              $(bin_out)"                                         ;  \
	   echo                                                                                    ;  \
	else                                                                                          \
	   echo "NOW LINKING FINAL BINARY"                                                        ;  \
	   echo                                                                                    ;  \
	fi
	@$(F90) $(F90FLAGS) $(mod)   $^   $(STATICLNK)  -o  $@
#
dashes:
	@echo
	@echo "--------------------------------------------------------------------------------"
	${echo2}
        
completed:
	@echo "   MAKEFILE COMPILATION COMPLETE"
	${echo2}
#
#      --> Note this uses the Unix find command and NOT Windows
#          If you get an error on windows, its using the wrong one --> See MinGW or MySYS64)
clean: 
	@echo
	find ./obj -not -name 'obj' -name '*.o'    -delete
	find ./obj -not -name 'obj' -name '*.mod'  -delete
	find ./obj -not -name 'obj' -name '*.smod' -delete
	${echo2}
#
cleanOBJ: 
	@echo
	find ./obj -not -name 'obj' -not -name '.keep' -name '*'  -delete
	${echo2}
#
quietClean: 
	@echo
	find $(int_dir) -name '*.o'    -delete  2>/dev/null  ||  true
	find $(int_dir) -name '*.mod'  -delete  2>/dev/null  ||  true
	find $(int_dir) -name '*.smod' -delete  2>/dev/null  ||  true
	${echo2}
#
errorClean: 
	${echo4}
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "@@########################################################@@"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo
	@echo "                   MAKEFILE FAILED"
	@echo
	@echo "             TO MAKE $(CFG) COMPILATION OF"
	@echo
	@echo "               $(bin_out)"
	${echo2}
	rm -rf $(bin_out) 
	${echo2}
#
reset: 
	@echo
	find . -name '*.o'    -delete
	find . -name '*.mod'  -delete
	find . -name '*.smod' -delete
	@echo
	find ./obj -not -name 'obj' -not -name '.keep' -name '*'  -delete
	@echo
	find $(notdir $(bin_dir)) -not -name '$(notdir $(bin_dir))' -not -name '.keep' -name '*'  -delete
	${echo2}
#
# The following target allows for printing a specific variable.  old method: @echo $*=$($*)
#  make print-CC  --> will print out hte value of $(CC)
#
print-%:
	@echo '$*=$($*)'
	@echo ' origin = $(origin $*)'
	@echo ' flavor = $(flavor $*)'
	@echo ' value = $(value $*)'
#
#################################################################################
###  Object Code Recipes  ######################################################
#################################################################################
#
$(int_dir)/%.o : %.c
	@echo "$(CC)   $(notdir $<)"
	@echo
	@$(CC) $(CFLAGS) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.f
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.F
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.f90
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.F90
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.fpp
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.for
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#

#
###############################################################################
#
# Setup suffixes that will be processes # .h .mod
.SUFFIXES: .o .f .for .F .FOR .f90 .F90 .fpp .c 
#
# #############################################################################
#
# Phony Targets
.PHONY: all run clean cleanOBJ reset runMake runTest preclean errorClean startMSG print-% completed dashes compile
#
# #############################################################################
#
# Supress echoing of commands
.SILENT: startMSG CompFlags dashes completed print-%
#
#
# THE END #####################################################################


