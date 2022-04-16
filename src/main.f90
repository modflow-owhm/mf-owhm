!
!     ******************************************************************
!     MAIN CODE FOR U.S. GEOLOGICAL SURVEY SIMULATION MODEL
!
!         MODFLOW - ONE-WATER Hydrologic Model (OWHM) Version 2.x
!
!     WITH THE FARM PROCESS (version 4), LGR2, NWT1, SWR1, RIP1, and SWI2
!
!     ******************************************************************
!
SUBROUTINE PRINT_MAIN_HEADER(IU)  ! Set to 6 for cmd prompt or use output_unit from: "use, intrinsic:: iso_fortran_env, only: output_unit"
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: stdout=>OUTPUT_UNIT
  USE CONSTANTS, ONLY: NL
  USE OWHM_HEADER_INTERFACE
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN)::IU
  !
  !1 ASSIGN VERSION NUMBER AND DATE
  !
  CHARACTER(:),ALLOCATABLE:: VERSION_OWHM
  CHARACTER(:),ALLOCATABLE:: VERSION_MF, VERSION_FMP
  CHARACTER(:),ALLOCATABLE:: VERSION_SWR,VERSION_LGR
  CHARACTER(:),ALLOCATABLE:: VERSION_SWI,VERSION_NWT
  CHARACTER(:),ALLOCATABLE:: VERSION_CFP
  CHARACTER(:),ALLOCATABLE:: VERSION_SWO
  CHARACTER(:),ALLOCATABLE:: Revision
  !
  VERSION_OWHM='2.2'
  Revision    ='1b'
  VERSION_MF  ='1.12'
  VERSION_FMP ='4.1'
  VERSION_SWR ='1.04'
  VERSION_SWI ='2.0'
  VERSION_LGR ='2.0'
  VERSION_NWT ='1.2'
  VERSION_CFP ='1.09.57'
  VERSION_SWO ='1.0'
  !
  IF(IU == stdout) THEN
                   WRITE (IU,'(A)') ''
  ELSE
                   WRITE (IU,'(A,/,20x,A,/,/)') '💧🧙💦🌊  <-These symbols help text editors identify file as Unicode UTF8.', &
                                                'UTF8 is not necessary to view numerical results, but no UTF8 support can have artifacts where non-ascii symbols are used (such as the degree symbol).'
  END IF
  !
  WRITE (IU,'(4A)') 'MODFLOW-OWHM v', VERSION_OWHM,".",Revision !(:MIN(len_trim(Revision), 4))
  !
  IF(IU /= stdout) WRITE(IU,'(/A/)') OWHM_HEADER()     ! Only print to file and skip cmd prompt
  !
  WRITE (IU,'(/, A)')     '                        MODFLOW'
  WRITE (IU,'(A)')        '            ONE-WATER HYDROLOGIC-FLOW MODEL'

  WRITE (IU,'(/, A)')     '    U.S. Geological Survey Modular Finite-Difference'
  WRITE (IU,'(A)')        '           Conjunctive Use Simulation Program'

  WRITE (IU,'(/, 2A)')    '                      Version ', VERSION_OWHM

  WRITE (IU,'(/, 2A)')    ' Release: ', Revision
  IF(IU /= stdout) THEN
     WRITE (IU,'(/,A,/)') ' Includes the following base versions with enhancements:'
     WRITE (IU,'(2A)')  '          MODFLOW-2005 Version ', VERSION_MF
     WRITE (IU,'(2A)')  '          MODFLOW-FMP  Version ', VERSION_FMP
     WRITE (IU,'(2A)')  '          MODFLOW-SWR  Version ', VERSION_SWR
     WRITE (IU,'(2A)')  '          MODFLOW-SWI  Version ', VERSION_SWI
     WRITE (IU,'(2A)')  '          MODFLOW-LGR  Version ', VERSION_LGR
     WRITE (IU,'(2A)')  '          MODFLOW-NWT  Version ', VERSION_NWT
     WRITE (IU,'(2A)')  '          MODFLOW-CFP  Version ', VERSION_CFP
     WRITE (IU,'(2A)')  '                  SWO  Version ', VERSION_SWO
  END IF
  !
  WRITE (IU,'(/, *(A, /))')                                                                                  &
                    REPEAT('-',84),                                                                          &
                    'USGS Software Disclaimer:                                                          |',  &
                    '                                                                                   |',  &
                    '   No warranty, expressed or implied, is made by the USGS or the U.S. Government   |',  &
                    '   as to the functionality of the software and related material nor shall the      |',  &
                    '   fact of release constitute any such warranty.                                   |',  &
                    '                                                                                   |',  &
                    '   The software is provided on the condition that neither the USGS nor             |',  &
                    '   the U.S. Government shall be held liable for any damages resulting from the     |',  &
                    '   authorized or unauthorized use of the software.                                 |',  &
                    '                                                                                   |',  &
                    '   Newer versions may be present at the official code and download repository:     |',  &
                    '      https://code.usgs.gov/modflow/mf-owhm                                        |',  &
                    '                                                                                   |',  &
                    REPEAT('-',84)
                    !
  !IF(IU /= stdout) WRITE(IU,'(A,/)') ''
END SUBROUTINE
!
!
PROGRAM MODFLOW_OWHM
  IMPLICIT NONE
  CHARACTER(1):: NAME_FILE                  !If blank then MF-OWHM checks command arguments
  !
  NAME_FILE = ""
  CALL MODFLOW_OWHM_RUN(NAME_FILE)
  !
  !CALL MODFLOW_OWHM_RUN('bcf2ss.nam'      )
  !CALL MODFLOW_OWHM_RUN('etsdrt.nam'      )
  !CALL MODFLOW_OWHM_RUN('fhb.nam'         )
  !CALL MODFLOW_OWHM_RUN('ibs2k.nam'       )
  !CALL MODFLOW_OWHM_RUN('l1a2k.nam'       )
  !CALL MODFLOW_OWHM_RUN('l1b2k.nam'       )
  !CALL MODFLOW_OWHM_RUN('l1b2k_bath.nam'  )
  !CALL MODFLOW_OWHM_RUN('mnw1.nam'        )
  !CALL MODFLOW_OWHM_RUN('restest.nam'     )
  !CALL MODFLOW_OWHM_RUN('str.nam'         )
  !CALL MODFLOW_OWHM_RUN('swtex4.nam'      )
  !CALL MODFLOW_OWHM_RUN('tc2hufv4.nam'    )
  !CALL MODFLOW_OWHM_RUN('test1ss.nam'     )
  !CALL MODFLOW_OWHM_RUN('test1tr.nam'     )
  !CALL MODFLOW_OWHM_RUN('testsfr2.nam'    )
  !CALL MODFLOW_OWHM_RUN('testsfr2_tab.nam')
  !CALL MODFLOW_OWHM_RUN('tr2k_s3.nam'     )
  !CALL MODFLOW_OWHM_RUN('twri.nam'        )
  !CALL MODFLOW_OWHM_RUN('twrihfb.nam'     )
  !CALL MODFLOW_OWHM_RUN('twrip.nam'       )
  !CALL MODFLOW_OWHM_RUN('MNW2-Fig28.nam'  )
  !CALL MODFLOW_OWHM_RUN('UZFtest2.nam'    )
  !
  !CALL CHDIR('../mf-2005-nwt')
  !
  !CALL MODFLOW_OWHM_RUN('etsdrt_nwt.nam'      )
  !CALL MODFLOW_OWHM_RUN('swtex4_nwt.nam'      )
  !CALL MODFLOW_OWHM_RUN('test1ss_nwt.nam'     )
  !CALL MODFLOW_OWHM_RUN('test1tr_nwt.nam'     )
  !CALL MODFLOW_OWHM_RUN('testsfr2_nwt.nam'    )
  !CALL MODFLOW_OWHM_RUN('testsfr2_tab_nwt.nam')
  !CALL MODFLOW_OWHM_RUN('twrip_nwt.nam'       )
  !CALL MODFLOW_OWHM_RUN('MNW2-Fig28_nwt.nam'  )
  !CALL MODFLOW_OWHM_RUN('UZFtest2_nwt.nam'    )
  !
  !CALL CHDIR('../mf-nwt')
  !
  !CALL MODFLOW_OWHM_RUN('Pr1a_MFNWT.nam'      )
  !CALL MODFLOW_OWHM_RUN('Pr1b_MFNWT.nam'      )
  !CALL MODFLOW_OWHM_RUN('Pr2MFNWT.nam'        )
  !CALL MODFLOW_OWHM_RUN('Pr3_MFNWT_higher.nam')
  !CALL MODFLOW_OWHM_RUN('Pr3_MFNWT_lower.nam' )
  !CALL MODFLOW_OWHM_RUN('swi2ex4sww.nam'      )
  !
  !CALL CHDIR('../mf-swr')
  !
  !CALL MODFLOW_OWHM_RUN('SWRSample01.01min.nam')
  !CALL MODFLOW_OWHM_RUN('SWRSample01.nam'      )
  !CALL MODFLOW_OWHM_RUN('SWRSample02.nam'      )
  !CALL MODFLOW_OWHM_RUN('SWRSample03.nam'      )
  !CALL MODFLOW_OWHM_RUN('SWRSample04.nam'      )
  !CALL MODFLOW_OWHM_RUN('SWRSample05-nwt.nam'  )
  !
  !CALL CHDIR('../mf-swi')
  !
  !CALL MODFLOW_OWHM_RUN( 'swi2ex1.nam'         )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex2_cont.nam'    )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex2_strat.nam'   )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex3.nam'         )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex4_2d.nam'      )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex4_2d_sww.nam'  )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex5.nam'         )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex6_1.nam'       )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex6_2.nam'       )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex6_3_0.005.nam' )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex6_3_0.010.nam' )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex6_3_0.100.nam' )
  !CALL MODFLOW_OWHM_RUN( 'swi2ex6_3_1.000.nam' )
  !!
  !CONTINUE
  !
END PROGRAM MODFLOW_OWHM
!
SUBROUTINE MODFLOW_OWHM_RUN(NAME)
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: STDIN=>INPUT_UNIT, STDOUT=>OUTPUT_UNIT
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  !
  ! Utility Modules
  !
  USE CONSOLE_COMMANDER, ONLY: CMD_CLEAR
  USE OWHM_HEADER_INTERFACE
  USE OPENSPEC
  USE CONSTANTS,                 ONLY: NL, BLN, BLNK, NEG, Z, ONE, TWO, FIVE, TEN, TRUE, FALSE
  USE ERROR_INTERFACE,           ONLY: STOP_ERROR, GET_WARN, PAUSE
  USE NUM2STR_INTERFACE,         ONLY: NUM2STR
  USE BAS_UTIL,                  ONLY: GET_NAME_AND_LGR_CHECK, OPEN_NAME_FILE, GETNAMFILLGR, MASS_ERROR_COUNT_PRINT
  USE GENERIC_OPEN_INTERFACE,    ONLY: SET_TO_NULL_UNIT
  USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
  !
  ! Package Mudules
  !
  USE GLOBAL
  USE GWFBASMODULE
  !
  USE WEL_SUBROUTINES
  USE GHB_SUBROUTINES
  !
  USE GWFEVTMODULE,  ONLY: EVT_NEVTOP => NEVTOP
  !
  USE GWFRCHMODULE,  ONLY: RCH_NRCHOP => NRCHOP
  !
  USE CFPMODULE,     ONLY: CFP_ICFPCNVG => ICFPCNVG                        !TR: 2017 09 13 CFP
  !
  USE GWFMNW2MODULE, ONLY: GWF2MNW27DA
  !
  !slang USE SLANG_PACKAGE_INTERFACE
  !
  ! Flow Package Modules
  !
  USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
  !
  ! FMP Modules
  !
  USE FMP_GLOBAL,      ONLY:FMP3DA, FMP_LGR_PNT
  USE FMP_MAIN_DRIVER, ONLY:FMP_AR, FMP_RP, FMP_AD, FMP_SUBLINK, FMP_FM, FMP_CNVG, FMP_BD
  !
  !swo USE SWO_INTERFACE, ONLY: SWO_DATA, SWO_AR
  !
  !LGR Module
  !
  USE LGRMODULE
  !
  ! Solver Modules
  !
  USE GWFNWTMODULE, ONLY: LINMETH, ITREAL
  USE PCGMODULE
  USE SIPMODULE
  USE DE4MODULE
  USE GMGMODULE
  USE PCGN
  !
  IMPLICIT NONE
  !
  CHARACTER(*), INTENT(IN):: NAME
  !
  ! Variable Declaration -----------------------------------------
  !
  !swo TYPE(SWO_DATA), ALLOCATABLE:: SWO        !Only Allocated if SWO is enabled
  !
  TYPE(DATE_OPERATOR):: SIM_START, SIM_END
  !
  CHARACTER(512):: FNAME
  !INTEGER IBDT(8)
  CHARACTER(:),ALLOCATABLE:: INTER  !Print out of iteration info
  !
  INTEGER(4):: START, FINISH, ClockRate
  INTEGER:: ISTP
  REAL:: CPU_TIME
  LOGICAL:: FASTFORWARD
  !
  INTEGER:: I
  INTEGER:: NAM_UNIT, LGR_UNIT, IGRID, LG, NSOL, NCVGERR
  INTEGER:: KPER, KKPER, KSTP, KKSTP
  INTEGER:: ICNVG, KITER, KKITER, LGRCNVG, LGRITER, ITREAL2
  INTEGER:: IERR, IBDRET
  INTEGER:: IC1,IC2,IR1,IR2,IL1,IL2
  INTEGER:: CFPMODE
  INTEGER:: IOUTS !USED BY GWT --Kept set to null file if not used
  REAL:: BUDPERC
  LOGICAL:: USE_PAUSE
  LOGICAL:: ITER_PRINT
  INTEGER:: ITER_SIZE
  !
  NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)  !Variable in Global Module. On IEEE CPUs same as: TRANSFER(-2251799813685248_INT64, 1._REAL64)
  !
  !1.5----SET UP OPENSPEC VARIABLES FOR USE BY OTHER PACKAGES (Replaces include openspec.inc)
  !
  CALL SET_OPENSPEC()
  !
  !2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
  !
  CALL PRINT_MAIN_HEADER(STDOUT)  !PRINT TO COMMAND PROMPT  --Note STDOUT=6
  !
  NAM_UNIT = Z
  LGR_UNIT = Z
  !
  NGRIDS   = ONE
  ILGR     = Z
  !
  NCVGERR  = Z
  CFPMODE  = Z
  !
  KPER     = ONE
  ISTP     = Z
  !
  USE_PAUSE  = FALSE
  ITER_PRINT = FALSE
  ITER_SIZE  = Z
  !
  IL1=ONE  !This was originally set this way at declaration...which would make it have implied SAVE
  !
  CALL SET_TO_NULL_UNIT(IOUTS)
  !
  !3---Check if LGR is in use
  !
  IF(NAME /= "") THEN
      ILGR   = Z
      NGRIDS = ONE
      FNAME  = NAME
  ELSE
      CALL GET_NAME_AND_LGR_CHECK(ILGR, NGRIDS, FNAME)  !Set FNAME to LGR Control or Name File
  END IF
  !
  ! Set Simulation Starting Date and Time
  !
  CALL SIM_START%NOW()
  !
  WRITE(STDOUT,'(//1x,2A//)') 'OneWater Simulation Initiated at ', SIM_START%STR('  ')
  !
  !GET THE NAME OF THE NAME FILE
  GRID_AR: DO IGRID = ONE, NGRIDS !-----------------------------------------------------------------------------------------------------------
      !
      !4A------IF USING LGR, READ NAMES FROM LGR NAME FILE
      !
      IF(ILGR /= Z) THEN
                      CALL GETNAMFILLGR(LGR_UNIT,FNAME,IGRID)  !Set LGR_UNIT from FNAME, update FNAME to current NAME_FILE
      ELSE
                      LGR_UNIT = Z
      END IF
      !
      !4B-----OPEN NAME FILE.
      !
      CALL OPEN_NAME_FILE(FNAME, NAM_UNIT, LGR_UNIT)
      !
      WRITE(*,'(2A/)')' Loading Packages from Name file: ',TRIM(FNAME)
      !
      !
      !6------ALLOCATE AND READ (AR) PROCEDURE
      !
      NSOL = ONE  !seb think this is Number of Solutes, but never actually updated used other than loop index
      !
      !              (INUNIT,   CUNIT,DIS,ZON,MLT, IGRID, OC, PVAL)
      CALL GWF2BAS7AR(NAM_UNIT, CUNIT, 24, 31, 32, IGRID, 12, 26, USE_PAUSE)
      !
      IF(IGRID==ONE) THEN
                   !
                   I = Z
                   IF    (IUNIT( 9) /= Z) I = I + ONE
                   IF    (IUNIT(10) /= Z) I = I + ONE
                   IF    (IUNIT(13) /= Z) I = I + ONE
                   IF    (IUNIT(42) /= Z) I = I + ONE
                   IF    (IUNIT(62) /= Z) I = I + ONE
                   IF    (IUNIT(70) /= Z) I = I + ONE
                   IF(I == Z ) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='No Solver Package Found in Name File.'//BLN//'Simulation requires that 1 Solver Package is specified in the Name File (PCG, PCGN, NWT, GMG, DE4, or SIP).')
                   IF(I > ONE) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='More than 1 Solver Package Found in Name File.'//BLN//'Simulation requires that only 1 Solver Package is specified in the Name File (PCG, PCGN, NWT, GMG, DE4, or SIP).')
                   !
                   IF    (IUNIT( 9) /= Z) THEN; GW_SOLVER = 'SIP'
                   ELSEIF(IUNIT(10) /= Z) THEN; GW_SOLVER = 'DE4'
                   ELSEIF(IUNIT(13) /= Z) THEN; GW_SOLVER = 'PCG'
                   ELSEIF(IUNIT(42) /= Z) THEN; GW_SOLVER = 'GMG'
                   ELSEIF(IUNIT(62) /= Z) THEN; GW_SOLVER = 'NWT'
                   ELSEIF(IUNIT(70) /= Z) THEN; GW_SOLVER = 'PCGN'
                   ELSE
                                                  GW_SOLVER = ''
                   END IF
                   !---------------------------------------------------------------
                   I = Z
                   IF    (IUNIT(23) /= Z) I = I + ONE
                   IF    (IUNIT(37) /= Z) I = I + ONE
                   IF    (IUNIT(63) /= Z) I = I + ONE
                   IF    (IUNIT(1 ) /= Z) I = I + ONE
                   IF(I == Z ) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='No Flow Package Found in Name File.'//BLN//'Simulation requires that 1 Flow Package is specified in the Name File (LPF, UPW, BCF, or HUF).')
                   IF(I > ONE) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='More than 1 Flow Package Found in Name File.'//BLN//'Simulation requires that only 1 Flow Package is specified in the Name File (LPF, UPW, BCF, or HUF).')
                   !
                   IF    (IUNIT(23) /= Z) THEN; GW_FLOW_PACK = 'LPF'
                   ELSEIF(IUNIT(37) /= Z) THEN; GW_FLOW_PACK = 'HUF'
                   ELSEIF(IUNIT(63) /= Z) THEN; GW_FLOW_PACK = 'UPW'
                   ELSEIF(IUNIT(1 ) /= Z) THEN; GW_FLOW_PACK = 'BCF'
                   ELSE
                                                  GW_FLOW_PACK = ''
                   END IF
      END IF
      !
      IF(ILGR /= Z) CALL GWF2LGR2AR(LGR_UNIT,FNAME,NGRIDS,IGRID)
      !
      IF(IUNIT(50) /= Z .AND. IUNIT(52) /= Z) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='OneWater Cannot simulate both MNW1 and MNW2 at the same time. Please remove one of the packages from the NAME file.')
      !
      !BASIC AND FLOW PACKAGES
      IF(IUNIT(21) /= Z) CALL GWF2HFB7AR(IUNIT(21),IGRID)
      IF(IUNIT(1)  /= Z) CALL GWF2BCF7AR(IUNIT(1),ILGR,IGRID)
      IF(IUNIT(23) /= Z) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      IF(IUNIT(37) /= Z) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47), IUNIT(53), ILGR, IGRID)
      !
      ! Allocate arrays for Newton Solver
      IF(IUNIT(63) /= Z) CALL GWF2NWT1AR(IUNIT(63),MXITER,IUNIT(22),ILGR,IGRID)
      IF(IUNIT(62) /= Z) CALL GWF2UPW1AR(IUNIT(62), Igrid)
      !
      !CALCULATE INITIAL UPPER MOST LAYER AND WATER TABLE--REQUIRES LAYHDT TO BE SET BY FLOW PACKAGE
      CALL SET_ADV_GLOBAL_ARRAYS(IGRID)
      !
      ! Packages
      !
      IF(IUNIT(67) /= Z) CALL GWF2WEL7AR(IUNIT(67),IUNIT(63),IGRID)
      IF(IUNIT(2)  /= Z) CALL GWF2WEL8AR(IUNIT(2),IGRID)
      IF(IUNIT(3)  /= Z) CALL GWF2DRN7AR(IUNIT(3),IGRID)
      IF(IUNIT(4)  /= Z) CALL GWF2RIV7AR(IUNIT(4),IGRID)
      IF(IUNIT(5)  /= Z) CALL GWF2EVT7AR(IUNIT(5),IGRID)
      If(IUNIT(6)  /= Z) CALL GWF2RIP4AR(IUNIT(6),IGRID)               !inserted by schmid
      IF(IUNIT(7)  /= Z) CALL GWF2GHB7AR(IUNIT(7),IGRID)
      IF(IUNIT(8)  /= Z) CALL GWF2RCH7AR(IUNIT(8),IGRID)
      IF(IUNIT(16) /= Z) CALL GWF2FHB7AR(IUNIT(16),IGRID)
      IF(IUNIT(17) /= Z) CALL GWF2RES7AR(IUNIT(17),IGRID)
      IF(IUNIT(18) /= Z) CALL GWF2STR7AR(IUNIT(18),IGRID)
      IF(IUNIT(19) /= Z) CALL GWF2IBS7AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20) /= Z) CALL GWF2CHD7AR(IUNIT(20),IGRID)
      !
      IF(IUNIT(44) /= Z) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23), IUNIT(37),IUNIT(15),NSOL,IOUTS, IUNIT(62),IUNIT(55),IGRID)  !IOUTS used by GWT..not longer used
      !
      IF(IUNIT(55) /= Z) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1), IUNIT(23),IUNIT(37), IUNIT(63), IUNIT(49), IGRID)
      !
      IF(IUNIT(22) /= Z .OR. IUNIT(44) /= Z) CALL GWF2LAK7AR(IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL,IGRID)
      !
      IF(IUNIT(46) /= Z) CALL GWF2GAG7AR(IUNIT(46),IUNIT(44), IUNIT(22),IGRID)
      IF(IUNIT(39) /= Z) CALL GWF2ETS7AR(IUNIT(39),IGRID)
      IF(IUNIT(40) /= Z) CALL GWF2DRT7AR(IUNIT(40),IGRID)
      IF(IUNIT(54) /= Z) CALL GWF2SUB7AR(IUNIT(54),IGRID)
      !
      IF(IUNIT(48) /= Z) CALL GWF2BFH2AR(IUNIT(48),ILGR,IGRID)
      IF(IUNIT(9)  /= Z) CALL SIP7AR(IUNIT(9),MXITER,IGRID)
      IF(IUNIT(10) /= Z) CALL DE47AR(IUNIT(10),MXITER,IGRID)
      IF(IUNIT(13) /= Z) CALL PCG7AR(IUNIT(13),MXITER,IGRID)
      IF(IUNIT(42) /= Z) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(70) /= Z) CALL PCGN2AR(IUNIT(70),IFREFM,MXITER,IGRID)
      IF(IUNIT(50) /= Z) CALL GWF2MNW27AR(IUNIT(50),IUNIT(63),IGRID)
      IF(IUNIT(51) /= Z) CALL GWF2MNW2I7AR(IUNIT(51),IUNIT(50),IGRID)
      IF(IUNIT(52) /= Z) CALL GWF2MNW17AR(IUNIT(52),IUNIT(9),IUNIT(10),IUNIT(63),0,IUNIT(13),0,IUNIT(42),IUNIT(70),FNAME,IGRID)
      IF(IUNIT(57) /= Z) CALL GWF2SWT7AR(IUNIT(57),IGRID)
      IF(IUNIT(64) /= Z) CALL GWF2SWR7AR(IUNIT(64),IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IUNIT(44),IUNIT(63),IGRID)  !SWR  - JDH
      IF(IUNIT(65) /= Z) CALL GWF2SWI2AR(IUNIT(65),IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)                      !SWI  - JDH
      !
      IF(IUNIT(43) /= Z) THEN
                           CALL GWF2HYD7BAS7AR(IUNIT(43),IGRID,IUNIT(63))
                           !
                           IF(IUNIT(19) /= Z) CALL GWF2HYD7IBS7AR(IUNIT(43),IGRID)
                           IF(IUNIT(54) /= Z) CALL GWF2HYD7SUB7AR(IUNIT(43),IGRID)
                           IF(IUNIT(18) /= Z) CALL GWF2HYD7STR7AR(IUNIT(43),IGRID)
                           IF(IUNIT(44) /= Z) CALL GWF2HYD7SFR7AR(IUNIT(43),IGRID)
      END IF
      !
      !barc**add CFP AR                                                       !TR: 2017 07 20 CFPv2
      !--PROCESS AND CHECK CONDUIT DATA                                       !TR: 2017 07 20 CFPv2
      !
      IF(IUNIT(58) /= Z) CALL GWF2CFP1AR(IUNIT(58), IUNIT(59),IUNIT(60),CFPMODE)             !TR: 2017 07 20 CFPv2
      IF(IUNIT(49) /= Z) CALL LMT8BAS7AR(NAM_UNIT,CUNIT,IGRID)
      IF(IUNIT(66) /= Z) CALL GWF2AG7AR(IUNIT(66),IUNIT(44),IUNIT(63))
      IF(IUNIT(50) /= Z) CALL GWF2MNW27AR2(IUNIT(50),KPER,IUNIT(9),IUNIT(10),0,IUNIT(13),0,IUNIT(42),IUNIT(70),IUNIT(15),IUNIT(63),IUNIT(61),IGRID)!SECOND CALL LOADS MNW2 WELLS...MOVED FROM RP UP  --KPER = 1
      !
      !                                (IN_FMP,   IUNITSFR, IUNITMNW1,IUNITMNW2, IUNITUZF,  IUNITNWT, IUNITDRT,IGRID,ILGR,MXITER)
      IF(IUNIT(61) /= Z) CALL FMP_AR( IUNIT(61),IUNIT(44),IUNIT(52),IUNIT(50),IUNIT(55), IUNIT(63),IUNIT(40),IGRID,ILGR,MXITER )
      !
      ! Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
      !
      IF(IUNIT(33) /= Z) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
      IF(IUNIT(34) /= Z) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35) /= Z) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
      IF(IUNIT(38) /= Z) CALL OBS2CHD7AR(IUNIT(38),IGRID)
      IF(IUNIT(41) /= Z) CALL OBS2DRT7AR(IUNIT(41),IUNIT(40),IGRID)
      !
      IF(IUNIT(58) /= Z .AND. IUNIT(62) /= Z) THEN  !Check for CFP
          IF(ANY(IBOUND < Z) .OR. IUNIT(20) /= Z) THEN
               CALL STOP_ERROR(MSG='Cannot simulate both CFP with the NWT Solver when there are Constant Head Cells (IBOUND<0 or CHD Package) at this time. Contact the developer and he may be able to add that feature. Regretibly this was set aside due to time contraints.')
          END IF
      END IF
      !
      IF(MXITER < 10) MXITER=10
      !
      !slang IF(IUNIT(69) /= Z) CALL SLANG_AR(IUNIT(69), IOUT, NPER, HAS_STARTDATE, IGRID)
      !
      !swo IF(IUNIT(45) /= Z) THEN
      !swo                      ALLOCATE(SWO)
      !swo                      CALL SWO_AR(SWO, IUNIT(45), IUNIT(61), IUNIT(44), IOUT, ILGR, SPSTART)
      !swo END IF
      !
      CALL GWF2BAS7AR2(IGRID)  !Setup some further AR variables (GSE)
      !
      IF(IUNIT(63) /= Z) CALL SETUP_NWT_GSE_LIM()  !IGRED Setup already for from previous AR call
      !
      CLOSE(NAM_UNIT)  !Name no longer used in simulation
      !
      ! END LOOP FOR ALLOCATING AND READING DATA FOR EACH GRID
      !      IF(ILGR /= 0.AND.IGRID > 1.AND.IUNIT(62) /= 0) CALL #2PRTOCH()
  END DO GRID_AR  !-----------------------------------------------------------------------------------------------------------------------------------
  !
  IF(LGR_UNIT /= Z) CLOSE(LGR_UNIT)  !Close the LGR Control File
  !
  FLUSH(IOUT)  ! dump what is in the list to the list file from the buffer
  !
  IF(ILGR /= Z) THEN
                  CMD_ITER_INFO = Z
  ELSEIF(ABS(CMD_ITER_INFO) == 4590077) THEN
      !
      I = ONE
      IF(CMD_ITER_INFO < Z) I = NEG
      !
      IF    (MXITER < 250  ) THEN
                             CMD_ITER_INFO = 10*I
      ELSE
                             CMD_ITER_INFO = 25*I
      END IF
      !
  END IF
  !
  !7------SIMULATE EACH STRESS PERIOD.
  !
  START     = Z
  FINISH    = Z
  ClockRate = Z
  I         = Z
  DO WHILE(ClockRate <= Z .AND. I < 1000)     !  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
     CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate) ! Find the rate
     I = I + ONE
  END DO
  IF(ClockRate <= Z) CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
  !
  STRESS_PERIOD: DO KPER = ONE, NPER  ! ============================================================================================================
      !
      FASTFORWARD = KPER < SPSTART .OR. SPEND < KPER .OR. INPUT_CHECK
      !
      ! If simulation is too fast, then disable cmd iteration printing
      ! If ITER_PRINT = TRUE, then CALL CMD_PRINT_STOP(ITER_SIZE) else CALL CMD_PRINT_ITER(KITER, ITER_SIZE)
      IF(KPER == 3)  THEN
         IF(CMD_ITER_INFO /= Z) ITER_PRINT = CPU_TIME >= 0. .AND. FINISH > Z .AND. START > Z  .AND. CPU_TIME < 0.2
      END IF
      !
      CALL SYSTEM_CLOCK(COUNT=START)                  ! Start timing
      I = Z
      DO WHILE(START <= Z .AND. I < 1000)
         CALL SYSTEM_CLOCK(COUNT=START)               ! Start timing  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
         I = I + ONE
      END DO
      IF(START <= Z) CALL SYSTEM_CLOCK(COUNT=START)
      !
      KKPER = KPER
      !
      GRID_RP: DO IGRID = ONE, NGRIDS !---------------------------------------------------------------------------------------------------------------
          !
          CALL SGWF2BAS7PNT(IGRID)
          !
          IF(IUNIT(62) /= Z) CALL GWF2UPWUPDATE(1,Igrid)
          !
          CALL GWF2BAS7ST(KKPER,IGRID)
          !
          IF(IUNIT(19) /= Z) CALL GWF2IBS7ST(KKPER,IGRID)
          IF(IUNIT(54) /= Z) CALL GWF2SUB7ST(KKPER,IGRID)
          IF(IUNIT(57) /= Z) CALL GWF2SWT7ST(KKPER,IGRID)
          !
          !7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
          !----------READ USING PACKAGE READ AND PREPARE MODULES.
          !swm: note the '1' below is hardwired for the parent grid
          !
          IF(ILGR      /= Z) CALL GWF2LGR2RP(KKPER,1,IGRID)
          IF(IUNIT(67) /= Z) CALL GWF2WEL7RP(IUNIT(67),IGRID)
          IF(IUNIT(2)  /= Z) CALL GWF2WEL8RP(IUNIT(2),IGRID)
          IF(IUNIT(3)  /= Z) CALL GWF2DRN7RP(IUNIT(3),IGRID,IUNIT(63))
          IF(IUNIT(4)  /= Z) CALL GWF2RIV7RP(IUNIT(4),IGRID)
          IF(IUNIT(5)  /= Z) CALL GWF2EVT7RP(IUNIT(5),IGRID)
          If(IUNIT(6)  /= Z) CALL GWF2RIP4RP(IUNIT(6),IGRID)                 ! inserted by schmid
          IF(IUNIT(7)  /= Z) CALL GWF2GHB7RP(IUNIT(7),IGRID,KKPER)           ! ,IUNIT(63) for NWT
          !
          IF(IUNIT(8)  /= Z) CALL GWF2RCH7RP(IUNIT(8),IGRID)
          IF(IUNIT(17) /= Z) CALL GWF2RES7RP(IUNIT(17),IGRID)
          IF(IUNIT(18) /= Z) CALL GWF2STR7RP(IUNIT(18),IGRID)
          IF(IUNIT(20) /= Z) CALL GWF2CHD7RP(IUNIT(20),IGRID)
          IF(IUNIT(21) /= Z) CALL GWF2HFB7RP(IUNIT(21),KKPER,IGRID)
          !
          IF(IUNIT(44) /= Z) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),       &
                                      IUNIT(22),IUNIT(54),KKPER,KKSTP,  &
                                      NSOL,IOUTS,IUNIT(1),              &
                                      IUNIT(23),IUNIT(37),              &
                                      IUNIT(62), IUNIT(55), IGRID)
          IF(IUNIT(43) /= Z ) THEN
             IF(KKPER == 1) THEN
                IF( IUNIT(18) /= Z ) CALL GWF2HYD7STR7RP(IUNIT(43),KKPER,IGRID)
                IF( IUNIT(44) /= Z ) CALL GWF2HYD7SFR7RP(IUNIT(43),KKPER,IGRID)
             END IF
          END IF
          !
          IF(IUNIT(55) /= Z) CALL GWF2UZF1RP(IUNIT(55),KKPER,IUNIT(44),IGRID)
          !
          IF(IUNIT(22) /= Z) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),IUNIT(62),KKPER,NSOL,IOUTS,IGRID) !formerly IOUTS???
          !
          IF(IUNIT(46) /= Z) THEN
              IF(KKPER == ONE) CALL GWF2GAG7RP(IUNIT(15),IUNIT(22),IUNIT(55),NSOL,IGRID)
          END IF
          !
          IF(IUNIT(39) /= Z) CALL GWF2ETS7RP(IUNIT(39),IGRID)
          IF(IUNIT(50) /= Z) CALL GWF2MNW27RP(IUNIT(50),KKPER,IUNIT(9),IUNIT(10),0,IUNIT(13),0,IUNIT(42),IUNIT(70),IUNIT(15),IUNIT(63),IUNIT(61),     +                                                           IGRID)
          IF(IUNIT(51) /= Z) THEN
              IF(KKPER == ONE) CALL GWF2MNW2I7RP(IUNIT(51), 0, IGRID)
          END IF
          !
          IF(IUNIT(52) /= Z) CALL GWF2MNW17RP(IUNIT(52),IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),KKPER,IGRID)
          !
          IF(IUNIT(66) /= Z) CALL GWF2AG7RP(IUNIT(66),IUNIT(44),KKPER)
          !
          !
          !--barc**READ DATA FOR CONDUIT RECHARGE PACKAGE
          IF(IUNIT(58) /= Z) CALL GWF2CFP1RP(IUNIT(60))                   !TR: 2017 07 20 CFPv2
          !
          IF(IUNIT(48) /= Z) CALL GWF2BFH2RP(IUNIT(48),KKPER,IGRID)
          !
          IF(IUNIT(61) /= Z) CALL FMP_RP(KKPER,IGRID)
          !
          IF(IUNIT(40) /= Z) CALL GWF2DRT7RP(IUNIT(40),IGRID)             !DRT must be after FMP_RP
          IF(IUNIT(64) /= Z) CALL GWF2SWR7RP(IUNIT(64),KKPER,IGRID)       !SWR - JDH
          !
          IF(IUNIT(66) /= Z) CALL GWF2AG7AD(IUNIT(66),KKPER)
          !
          IF(IUNIT(63) /= Z) CALL GWF2NWT1RP(IUNIT(63),KPER,Mxiter,IGRID)
          IF(IUNIT(13) /= Z) CALL PCG7RP(IUNIT(13),KPER,Mxiter,IGRID)
          IF(IUNIT(70) /= Z) CALL PCGN2RP(IUNIT(70),KPER,IGRID,IOUT,Mxiter)
          !
          !slang IF(HAS_SLANG) CALL SLANG_RP(KPER)  !Determine what scripts are run
          !
          !swo IF(IUNIT(45) /= Z) CALL SWO % SETUP_NEXT_STRESS_PERIOD(KPER)
          !
      END DO GRID_RP  ! ------------------------------------------------------------------------------------------------------------------------------
      !
      !7C-----SIMULATE EACH TIME STEP.
      INTER = ''
      TIME_STEP: DO KSTP = ONE, NSTP(KPER) !############################################################################################################
          !
          KKSTP = KSTP
          !
          GRID_AD: DO IGRID = ONE, NGRIDS   ! ----------------------------------------------------------------------------------------------------------
              !
              !7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
              !
              CALL SGWF2BAS7PNT(IGRID)                       !seb lgr
              !
              IF(IUNIT(62) /= Z ) CALL GWF2UPWUPDATE(1,Igrid)
              !
              CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
              !
              IF(IUNIT(54) /= Z) CALL GWF2SUB7AD(KKPER,KKSTP,IUNIT(54),IGRID)
              !
              ! Update Drain Elevations if SUB-LINK active
              IF( SUBLNK )THEN
                  IF(IUNIT(3)  /= Z) CALL GWF2DRN7AD(KKSTP,IGRID)
                  IF(IUNIT(40) /= Z) CALL GWF2DRT7AD(KKSTP,IGRID)
              ENDIF
              !
              IF(IUNIT(62) /= Z) CALL GWF2UPW1AD(IGRID)
              IF(IUNIT(20) /= Z) CALL GWF2CHD7AD(KKPER,IGRID)
              IF(IUNIT(1)  /= Z) CALL GWF2BCF7AD(KKPER,IGRID)
              IF(IUNIT(17) /= Z) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
              IF(IUNIT(23) /= Z) CALL GWF2LPF7AD(KKPER,IGRID)
              IF(IUNIT(37) /= Z) CALL GWF2HUF7AD(KKPER,IGRID)
              IF(IUNIT(16) /= Z) CALL GWF2FHB7AD(IGRID)
              IF(IUNIT(22) /= Z) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),IUNIT(62),IUNIT(15),IGRID)
              IF(IUNIT(55) /= Z) CALL GWF2UZF1AD(IUNIT(55), KKPER, KKSTP, Igrid)
              IF(IUNIT(65) /= Z) CALL GWF2SWI2AD(KKSTP,KKPER,IGRID)  !SWI
              IF(IUNIT(44) /= Z) CALL GWF2SFR7AD(IUNIT(44),IUNIT(22),KKSTP,KKPER,IGRID) !ADDED FOR SUB-LINK BY WSCHMID 07/29/10 --> sfr only uses AD for Sublink
              !
              IF(IUNIT(7)  /= Z) CALL GWF2GHB7AD(KKSTP,IGRID)                !seb added GHB and WEL AD
              IF(IUNIT(67) /= Z) CALL GWF2WEL7AD(KKSTP,IGRID)
              IF(IUNIT(2)  /= Z) CALL GWF2WEL8AD(KKSTP,IGRID)
              !
              !--BARC**
              IF(IUNIT(58) /= Z) CALL GWF2CFP1AD(KKPER,KKSTP)               !TR: 2017 07 20 CFPv2
              !
              !--BARC** DISTRIBUTE RECHARGE IN CONDUIT AND MATRIX SYSTEM      !TR: 2017 07 20 CFPv2
              IF (IUNIT(58) /= Z) CALL CFP1DCRCH(KKPER, KKSTP,IUNIT(8))     !TR: 2017 07 20 CFPv2
              !
              IF(IUNIT(50) /= Z) THEN
                  !
                  CALL MNW2_GET_COND(KPER,IGRID,IUNIT(50),IUNIT(1),IUNIT(23),IUNIT(62),IUNIT(37))
                  !
                  CALL GWF2MNW27AD(KKSTP,KKPER,IGRID)
              END IF
              !
              IF(IUNIT(52) /= Z) CALL GWF2MNW17AD(IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
              IF(IUNIT(48) /= Z) CALL GWF2BFH2AD(IUNIT(48),IGRID)
              !
              IF(IUNIT(61) /= Z) THEN
                  !
                  CALL FMP_AD(KKPER, KSTP, IGRID)
                  !
                  IF(SUBLNK) CALL FMP_SUBLINK(IGRID)
              ENDIF
              !
              IF(IUNIT(6)  /= Z .AND. SUBLNK ) CALL GWF2RIPAD(KKPER,IGRID) !RIP-SUBLINK Update - rth
              !
              IF(IUNIT(64) /= Z) CALL GWF2SWR7AD(KKPER,KSTP,IGRID,IUNIT(54))  !SWR - JDH
              !
              IF(IUNIT(66) /= Z) CALL GWF2AG7AD(IUNIT(66),KKPER)
              !
              !slang IF(HAS_SLANG) THEN
              !slang               IF(KPER == ONE .AND. KSTP == ONE) CALL SLANG_SIM_EVAL(KPER, KSTP, IOUT)
              !slang               !
              !slang               CALL SLANG_SP_TS_EVAL(KPER, KSTP, IOUT)
              !slang END IF
              !
              !---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
              CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
              !
          END DO GRID_AD   ! -------------------------------------------------------------------------------------------------------------------------
          !
          IF(KSTP == ONE .AND. HAS_STARTDATE) THEN
              WRITE(*,24) KPER,KSTP,DATE_SP(KPER)%TS(0)%STR_MONTHYEAR()
          ELSE
              WRITE(*,25) KPER,KSTP                            !seb moved outside of IGRID LOOP
          END IF
          !
          !24  FORMAT(' Solving:  Stress Period: ',i6,4x,'Time step: ',i6,4x,'Groundwater-Flow Eqn.',14x A)
          !25  FORMAT(' Solving:  Stress Period: ',i6,4x,'Time step: ',i6,4x,'Groundwater-Flow Eqn.')
          24  FORMAT(' Solving:  Stress Period: ',i6,4x,'Time step: ',i6,14x,A)
          25  FORMAT(' Solving:  Stress Period: ',i6,4x,'Time step: ',i6)
          26  FORMAT('Skipping:  Stress Period: ',i6,4x,'Time step: ',i6)
          !
          ! If simulation is too fast, then disable cmd iteration printing
          IF(ITER_PRINT) THEN
                 CALL CMD_PRINT_STOP(ITER_SIZE)
                 CMD_ITER_INFO = Z
                 ITER_PRINT = FALSE
          END IF
          !
          !---------CHECK IF FASTFORWARD FEATURE IS IN EFFECT --IF FIRST MODEL INTERATION, THEN LET ONE FM LOOP PASS TO INITIALIZE ADVANCE PACKAGE VARIABLES
          !
          CALL SGWF2BAS7PNT(ONE)  !ONLY PARENT CAN HAVE FASTFORWARD OPTION
          !
          !---------BEGIN LOOP FOR ITERATING BETWEEN GRIDS (LGR ITERATIONS)
          !
          LGRCNVG = Z
          LGRITER = Z
          ICNVG   = Z
          !
          IF(INPUT_CHECK) THEN
                                 ! Note that if IINPUT_CHECK = True, then FASTFORWARD = True
                                 !
                                 IF( .NOT.( KPER == ONE .AND. KSTP == ONE ) )  THEN ! BY PASS FM LOOPS AND GRID LOOPS
                                     !
                                     LGRCNVG = ONE  !By Pass GRID_CNVG Loop
                                     LGRITER = ONE
                                 END IF
                                 !
                                 ICNVG = ONE
                                 KITER = ONE
                                 IF(KSTP == ONE) INTER = INTER//' skipped  '
                                 !
          ELSEIF ( FASTFORWARD ) THEN
                                 DO IGRID = ONE, NGRIDS
                                               CALL SGWF2BAS7PNT(IGRID)
                                               CALL GWF2BAS7OC(KSTP, KPER, 1, IUNIT(12), IGRID)   !FILLS DUMMY "ICNVG" TO "1"
                                 END DO
                                 !
                                 IF(KSTP == ONE) INTER = INTER//' skipped  '
                                 IF( .NOT.( KPER == ONE .AND. KSTP == ONE ) ) CYCLE TIME_STEP  !SKIP TIME STEPS BEFORE SPSTART AND AFTER IT
          ELSE
               ISTP = ISTP + ONE
          END IF
          !
          !
          GRID_CNVG: DO WHILE (LGRCNVG == Z)
             !
             LGRITER = LGRITER + ONE
             !
             !7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS FOR EACH GRID
             !
             GRID_FM: DO IGRID = ONE, NGRIDS ! ---------------------------------------------------------------------------------------------------------
                !
                CALL SGWF2BAS7PNT(IGRID)
                !-------------CHECK IF LGR IS ACTIVE
                IF(ILGR /= Z)THEN
                    !
                    CALL SGWF2LGR2PNT(IGRID)
                    !
                    !---------------CHECK IF PARENT OR CHILD GRID
                    !
                    IF(ISCHILD /= NEG)THEN
                        !
                        !-----------------INTERPOLATE PARENT HEAD TO GHOST NODES AND RELAX
                        !swm: NOTE: the '1' in the arguement list is hardwired for the parent grid
                        !
                        IF (SPSTART == KKPER .AND. KKSTP == ONE) THEN
                            CALL GWF2LGR2DARCY(1,1,LGRITER,1,IGRID)           ! FAKE FIRST STRESS PERIOD IF USING FASTFORWARD
                        ELSE
                            CALL GWF2LGR2DARCY(KKPER,KKSTP,LGRITER,1,IGRID)
                        END IF
                    ENDIF
                ENDIF
                !
                !7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
                KITER   = Z
                ITREAL2 = Z
                !ccrth                NOITER = ONE
                IF ( IUNIT(63) /= Z ) ITREAL = Z
                !ccrth                IF ( IRESTART > Z ) THEN
                !ccrth                  NOITER = Z
                !ccrth                  IF ( KPER > KPERSTART ) THEN
                !ccrth                    NOITER = ONE
                !ccrth                  ELSE IF ( KPER == KPERSTART .AND. KSTP >= KSTPSTART ) THEN
                !ccrth                    NOITER = ONE
                !ccrth                  END IF
                !ccrth                END IF
                !ccrth                DO WHILE (ITREAL2 < MXITER .AND. NOITER == 1)
                FM_LOOP: DO WHILE (KITER < MXITER) !***********************************************************************************************
                   !
                   KITER = KITER + ONE
                   KKITER = KITER
                   !
                   !slang IF(HAS_SLANG_ITER) CALL SLANG_IT_EVAL(KPER, KSTP, KITER, IOUT)
                   !
                   !IF ( IUNIT(63) == Z ) ITREAL2 = KITER
                   IF(IUNIT(62) /= Z) CALL GWF2UPWUPDATE(2,Igrid)
                   !
                   !7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
                   !
                   CALL GWF2BAS7FM(IGRID)
                   !
                   IF(IUNIT(1)  /= Z) CALL GWF2BCF7FM(KKITER,KKSTP,KKPER,ILGR,IGRID)                       !SCOTT ORIGINALLY KKPER,ILGR,IGRID) BUT SUBROUTINE IS NOT DESIGNED TO RECIEVE ILGR, THIS MAY NEED TO BE INCORPORATED !swm: I added back in and modified BCF
                   IF(IUNIT(62) /= Z) CALL GWF2UPWFMS(KKITER,KKSTP,KKPER,IGRID)
                   IF(IUNIT(23) /= Z) CALL GWF2LPF7FM(KKITER,KKSTP,KKPER,ILGR,IGRID)
                   IF(IUNIT(37) /= Z) CALL GWF2HUF7FM(KKITER,KKSTP,KKPER,IUNIT(47),ILGR,IGRID)
                   !
                   !BARC**MODIFY CONDUCTANCES HERE WHEN MODE 2 IS ACTIVE
                   IF(IUNIT(58) /= Z) CALL GWF2CFPM2FM(KKITER,KKSTP,KKPER,ICNVG)                                                !TR: 2017 07 20 CFPv2 TODO CHECK FOR MODE2
                   !
                   IF ( IUNIT(62) == Z ) THEN
                     IF(IUNIT(21) /= Z) CALL GWF2HFB7FM(IGRID)
                   END IF
                   !
                   IF(IUNIT(67) /= Z) CALL GWF2WEL7FM(IUNIT(63),IGRID)
                   IF(IUNIT(2)  /= Z) CALL GWF2WEL8FM(IGRID)
                   IF(IUNIT(3)  /= Z) CALL GWF2DRN7FM(IGRID)
                   IF(IUNIT(4)  /= Z) CALL GWF2RIV7FM(IGRID)
                   !
                   IF(IUNIT(5)  /= Z) THEN
                       IF(IUNIT(22) /= Z .AND. EVT_NEVTOP == 3) CALL GWF2LAK7ST(0,IGRID)
                       !
                       CALL GWF2EVT7FM(IGRID)
                       !
                       IF(IUNIT(22) /= Z .AND. EVT_NEVTOP == 3) CALL GWF2LAK7ST(1,IGRID)
                   END IF
                   !
                   IF(IUNIT(6) /= Z) CALL GWF2RIP4FM(IGRID)
                   IF(IUNIT(7) /= Z) CALL GWF2GHB7FM(IGRID)
                   !
                   IF(IUNIT(8) /= Z) THEN
                      IF(IUNIT(22) /= Z .AND. RCH_NRCHOP == 3) CALL GWF2LAK7ST(0,IGRID)
                      !
                      CALL GWF2RCH7FM(IGRID)
                      !
                      IF(IUNIT(22) /= Z .AND. RCH_NRCHOP == 3) CALL GWF2LAK7ST(1,IGRID)
                   END IF
                   !
                   IF(IUNIT(16) /= Z) CALL GWF2FHB7FM(IGRID)
                   IF(IUNIT(17) /= Z) CALL GWF2RES7FM(IGRID)
                   IF(IUNIT(18) /= Z) CALL GWF2STR7FM(IGRID)
                   IF(IUNIT(19) /= Z) CALL GWF2IBS7FM(KKPER,IGRID)
                   IF(IUNIT(39) /= Z) CALL GWF2ETS7FM(IGRID)
                   IF(IUNIT(40) /= Z) CALL GWF2DRT7FM(IGRID)                   !DRT MUST OCCUR BEFORE FMP seb
                   !
                   IF(IUNIT(61) /= Z) CALL FMP_FM(KKITER, KKPER, KKSTP, IGRID, NGRIDS, ILGR, LGRITER, SPTIM(KKPER)%DT(KKSTP))
                   !
                   IF(IUNIT(55) /= Z) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,            &
                                                        IUNIT(44),IUNIT(22),IUNIT(58), &
                                                        IUNIT(63),                     &
                                                        IUNIT(64),                     &
                                                        IUNIT(66), IGRID)               ! SWR - JDH ADDED IUNIT(64)
                   !
                   IF(IUNIT(44) /= Z) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,                      &
                                                        IUNIT(22),IUNIT(63),IUNIT(8),IUNIT(55),  &
                                                        ILGR,LGRITER,NGRIDS,IGRID)   !cjm (added IUNIT(8))
                   !
                   IF(IUNIT(22)  /= Z) CALL GWF2LAK7FM(KKITER,KKPER,KKSTP,IUNIT(44),IUNIT(55),IGRID)
                   !
                   IF(IUNIT(50) /= Z) THEN
                         !
                         CALL MNW2_GET_COND(KPER,IGRID,IUNIT(50),IUNIT(1),IUNIT(23),IUNIT(62),IUNIT(37))
                         !
                         CALL GWF2MNW27FM(KKITER,kkstp,kkper,mxiter,IGRID)
                   END IF
                   !
                   IF(IUNIT(52) /= Z) CALL GWF2MNW17FM(KKITER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
                   IF(IUNIT(54) /= Z) CALL GWF2SUB7FM(KKPER,KKITER,IUNIT(9),IGRID)
                   IF(IUNIT(57) /= Z) CALL GWF2SWT7FM(KKPER,IGRID)
                   IF(IUNIT(64) /= Z) CALL GWF2SWR7FM(KKITER,KKPER,KKSTP,IGRID)  !SWR - JDH
                   IF(IUNIT(66) /= Z) CALL GWF2AG7FM(Kkper, Kkstp, Kkiter, IUNIT(63))
                   IF(IUNIT(65) /= Z) CALL GWF2SWI2FM(KKSTP,KKPER,KKITER,IGRID)  !SWI - JDH
                   !
                   IF(IUNIT(48) /= Z) CALL GWF2BFH2FM(KKPER,KKSTP,KKITER,IGRID)
                   !
                   !--BARC**SOLVE MODE 1 PIPE FLOW EQUATIONS
                   IF(IUNIT(58) /= Z) CALL GWF2CFP1M1FM(KKITER,KKPER,KKSTP,1)  !TR: 2017 07 20 CFPv2
                   !
                   !-----------------ADJUST HCOF AND RHS IF LGR IS ACTIVE
                   !
                   IF(ILGR /= Z)THEN
                        IF(IGRID == ONE)THEN
                             DO LG =2,NGRIDS
                               IF(LGRDAT(IGRID)%IBPFLG(LG) /= Z) CALL GWF2LGR2PFM(KKPER,KKSTP,KKITER,LGRITER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),LG)
                             ENDDO
                        ELSEIF(ISCHILD >= Z)THEN
                             CALL GWF2LGR2CFM(KKITER,LGRITER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
                        ENDIF
                   ENDIF
                   !
                   !--------------CHECK IF FASTFORWARD FEATURE IS IN EFFECT
                   !
                   IF ( FASTFORWARD .AND. IGRID==NGRIDS) CYCLE TIME_STEP
                   IF ( FASTFORWARD ) CYCLE GRID_FM
                   !
                   CALL BAS_PRE_SOLVER(IGRID, KPER, KSTP, KITER) !SAVE PREVIOUS HNEW AND SET UP ADVANCE DAMPING IF REQUESTED
                   !
                   !
                   !7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
                   IERR = Z
                   IF (IUNIT(9) /= Z) THEN
                          CALL SIP7PNT(IGRID)
                          CALL SIP7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,  &
                            V,W,HDCG,LRCH,NPARM,KKITER,HCLOSE,ACCL,ICNVG,      &
                            KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP(KKPER),      &
                            NCOL,NROW,NLAY,NODES,IOUT,0,IERR)
                   END IF
                   !
                   IF (IUNIT(10) /= Z) THEN
                          CALL DE47PNT(IGRID)
                          CALL DE47AP(HNEW,IBOUND,AU,AL,IUPPNT,IEQPNT,D4B,MXUP, &
                            MXLOW,MXEQ,MXBW,CR,CC,CV,HCOF,RHS,ACCLDE4,KITER,    &
                            ITMX,MXITER,NITERDE4,HCLOSEDE4,IPRD4,ICNVG,NCOL,    &
                            NROW,NLAY,IOUT,LRCHDE4,HDCGDE4,IFREQ,KKSTP,KKPER,   &
                            DELT,NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,               &
                            DELTL,NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW,IERR)
                   END IF
                   !
                   IF (IUNIT(13) /= Z) THEN
                          CALL PCG7PNT(IGRID)
                          CALL PCG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,VPCG,SS,  &
                            P,CD,HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,         &
                            HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,     &
                            MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,  &
                            NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF, &
                            HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY,       &
                            IHCOFADD,BPOLY)
                   END IF
                   !
                   IF (IUNIT(42) /= Z) THEN
                          CALL GMG7PNT(IGRID)
                          CALL GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,    &
                                      IITER,MXITER,RCLOSEGMG,HCLOSEGMG,        &
                                      KKITER,KKSTP,KKPER,NCOL,NROW,NLAY,ICNVG, &
                                      SITER,TSITER,DAMPGMG,IADAMPGMG,IOUTGMG,  &
                                      IOUT,GMGID,                              &
                                      IUNITMHC,DUP,DLOW,CHGLIMIT,              &
                                      BIGHEADCHG,HNEWLAST)
                   ENDIF
                   !
                   IF (IUNIT(70) /= Z) THEN
                                       CALL PCGN2AP(HNEW,RHS,CR,CC,CV,HCOF,IBOUND,KKITER,KKSTP,KKPER,ICNVG,HNOFLO,IGRID)
                   ENDIF
                   !
                   IF(IUNIT(63) /= Z) THEN
                                      CALL GWF2NWT1FM(ICNVG, KKITER, KSTP, KPER, MXITER, IGRID)
                                      ITREAL2 = ITREAL
                   END IF
                   !
                   IF(IERR == ONE) CALL USTOP(' ')
                   !
                   !-------ENSURE CONVERGENCE OF SWR - BASEFLOW CHANGES LESS THAN TOLF - JDH
                   IF(IUNIT(64) /= Z) THEN
                     CALL GWF2SWR7CV(KKITER,IGRID,ICNVG,MXITER)
                   END IF
                   !
                   IF(IUNIT(61) /= Z) CALL FMP_CNVG(IGRID, KPER, KSTP, KKITER, ICNVG)
                   !
                   CALL BAS_POST_SOLVER(KPER, KSTP, KITER, MXITER, ICNVG)  !APPLY ADDITIONAL DAMPENING, CONVERGENCE CHECK AND UPDATE UPLAY AND WTABLE
                   !
                   !slang IF(HAS_SLANG_ITER_END) CALL SLANG_IT_END_EVAL(KPER, KSTP, KITER, IOUT)
                   !
                   !C-WSCHMID-END OF MODIFICATION
                   !
                   !7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
                   !
                   IF(IUNIT(58) /= Z .AND. ICNVG == ONE) THEN                  !TR: 2017 07 20 CFOPv2
                       IF (CFPMODE == 2 .AND. CFP_ICFPCNVG /= ONE) ICNVG = Z     !TR: 2017 07 20 CFOPv2 !TR: 2011 10 14 JUST DO THIS FOR CFPM2
                   ENDIF
                   !
                   IF (ICNVG == ONE) EXIT FM_LOOP
                   !
                   IF(CMD_ITER_INFO /= Z) THEN
                                     ITER_PRINT = MODULO(KKITER, ABS(CMD_ITER_INFO)) == Z .OR. KKITER == ONE .OR. KKITER == FIVE .OR. KKITER == TEN
                                     !
                                     IF(ITER_PRINT) THEN
                                         CALL CMD_PRINT_ITER(KITER, ITER_SIZE)
                                         ITER_PRINT = CMD_ITER_INFO > Z .AND. KKITER < MXITER .AND. RCloseL2BAS==RCloseL2BAS .AND. KKITER > FIVE
                                     END IF
                                     !
                                     IF(ITER_PRINT) CALL CMD_PRINT_ERROR(ITER_SIZE)
                                     ITER_PRINT = FALSE
                   END IF
                   !
                END DO FM_LOOP  !*********************************************************************************************************************
                !
                IF (KITER > MXITER) KITER = MXITER
                !
                !   33     CONTINUE
                !crth          IF ( NOITER == 1 ) THEN
                !
                !--BARC**CALL CONDUIT ONCE MORE
                !  TO ADJUST THE CONDUIT HEADS TO THE HEADS IN THE FISSURED SYSTEM
                !  AFTER THE LAST MODFLOW-ITERATION
                !
                IF(IUNIT(58) /= Z) CALL GWF2CFP1M1FM(KKITER,KKPER,KKSTP,2)    !TR: 2017 09 13 FINAL CFP call
                !
                IF(IUNIT(62) /= Z ) CALL GWF2UPWUPDATE(2,Igrid)
                !
                !
                !-------------PREPARE THE NEXT GRID FOR LGR ITERATION
                IF(ILGR /= Z)THEN
                   IF(ISCHILD == NEG)THEN
                           DO LG =2,NGRIDS

                             CALL GWF2LGR2INITP(KKPER,KKSTP,LGRITER,                    &
                                                LGRDAT(LG)%NPCBEG,LGRDAT(LG)%NPRBEG,    &
                                                LGRDAT(LG)%NPLBEG,LGRDAT(LG)%NPCEND,    &
                                                LGRDAT(LG)%NPREND,LGRDAT(LG)%NPLEND,    &
                                                LGRDAT(LG)%ISHFLG,LGRDAT(LG)%MXLGRITER, &
                                                IUNIT(5),IUNIT(8),IUNIT(17),LG,IGRID)
                           ENDDO
                           !
                   ELSEIF(IGRID /= ONE)THEN
                                     ! CALCULATE FLUX ENTERING THE CHILD INTERFACE
                                     !
                                     CALL GWF2LGR2FMBF(KKPER,KKSTP,LGRITER)
                   ENDIF
                ENDIF
                !
             ENDDO GRID_FM ! -------------------------------------------------------------------------------------------------------------------------
             !
             ! CHECK CONVEGENCE OF LGR IF LGR IS ACTIVE
             !
             IF(ILGR == Z)THEN
                            LGRCNVG = ONE
                            !
                            IF(ITER_SIZE > Z) CALL CMD_CLEAR(ITER_SIZE)
             ELSE
                            CALL GWF2LGR2CNVG(IGRID,NGRIDS,LGRCNVG,LGRITER,KKPER,KKSTP)
             ENDIF
             !
          END DO GRID_CNVG
          !
          !
          !7C3----DETERMINE WHICH OUTPUT IS NEEDED FOR EACH GRID
          !
          GRID_OC: DO IGRID = ONE, NGRIDS
              !
              CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,GLOBALDAT(IGRID)%IUNIT(12),IGRID)
              !
              ! SWAP POINTERS FOR LGR DATA   !swm: needed for SFR
              IF(ILGR /= Z) CALL SGWF2LGR2PNT(IGRID)
              !
              !7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
              !
              MSUM = ONE  !Budget Counter
              !
              IF (IUNIT(1) /= Z) THEN
                  !
                  CALL GWF2BCF7BDS(KKSTP,KKPER,IGRID)
                  !
                  IF (IUNIT(58) /= Z) THEN
                                        CALL CFPBCF7BDCH(KKSTP,KKPER,IGRID)     !CFP SUBROUTINE THAT MODIFIES PIPE FLOWS TO CONSTANT HEAD BOUNDARY. NEED THIS IF STATEMENT SO FLOWS TO CONSTANT HEADS ARE NOT COMPUTED TWICE IN BUDGETS WHEN CFP IS ACTIVE.
                  ELSE
                                        CALL GWF2BCF7BDCH(KKSTP,KKPER,IGRID)
                  END IF
                  !
                  IBDRET=Z
                  IC1=ONE
                  IC2=NCOL
                  IR1=ONE
                  IR2=NROW
                  IL1=ONE
                  IL2=NLAY
                  !                            !IDIR
                  CALL GWF2BCF7BDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2BCF7BDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2BCF7BDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  !DO IDIR = 1, 3
                  !              CALL GWF2BCF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  !END DO
              ENDIF
              !
              IF(IUNIT(23) /= Z) THEN
                  !
                  CALL GWF2LPF7BDS(KKSTP,KKPER,IGRID)
                  !
                  IF (IUNIT(58) /= Z) THEN
                                        CALL CFPLPF7BDCH(KKSTP,KKPER,IGRID)     !CFP SUBROUTINE THAT MODIFIES PIPE FLOWS TO CONSTANT HEAD BOUNDARY. NEED THIS IF STATEMENT SO FLOWS TO CONSTANT HEADS ARE NOT COMPUTED TWICE IN BUDGETS WHEN CFP IS ACTIVE.
                  ELSE
                                        CALL GWF2LPF7BDCH(KKSTP,KKPER,IGRID)
                  END IF
                  !
                  IBDRET = Z
                  IC1    = ONE
                  IC2    = NCOL
                  IR1    = ONE
                  IR2    = NROW
                  IL1    = ONE
                  IL2    = NLAY                     !IDIR
                  CALL GWF2LPF7BDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2LPF7BDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2LPF7BDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
              ENDIF
              !
              IF(IUNIT(62) /= Z) THEN
                  !
                  CALL GWF2UPWBDS(KKSTP,KKPER,IGRID)
                  CALL GWF2UPWBDCH(KKSTP,KKPER,IGRID)    !TR: 2017 07 20 CFPv2 TODO ADD UPW SUPPORT / MODIFY UPWBDCH ACCORDINGLY
                  IBDRET = Z
                  IC1    = ONE
                  IC2    = NCOL
                  IR1    = ONE
                  IR2    = NROW
                  IL1    = ONE
                  IL2    = NLAY                     !IDIR
                  CALL GWF2UPWBDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2UPWBDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2UPWBDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
              ENDIF
              !
              IF(IUNIT(37) /= Z) THEN
                  !
                  CALL GWF2HUF7BDS(KKSTP,KKPER,IGRID)
                  !
                  IF (IUNIT(58) /= Z) THEN
                                        CALL CFPHUF7BDCH(KKSTP,KKPER,IUNIT(47),IGRID)     !CFP SUBROUTINE THAT MODIFIES PIPE FLOWS TO CONSTANT HEAD BOUNDARY. NEED THIS IF STATEMENT SO FLOWS TO CONSTANT HEADS ARE NOT COMPUTED TWICE IN BUDGETS WHEN CFP IS ACTIVE.
                  ELSE
                                        CALL GWF2HUF7BDCH(KKSTP,KKPER,IUNIT(47), IGRID)
                  END IF
                  !
                  IBDRET = Z
                  IC1    = ONE
                  IC2    = NCOL
                  IR1    = ONE
                  IR2    = NROW
                  IL1    = ONE
                  IL2    = NLAY                  !IDIR
                  CALL GWF2HUF7BDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
                  CALL GWF2HUF7BDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
                  CALL GWF2HUF7BDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
              ENDIF
              !
              IF(IUNIT(67) /= Z) CALL GWF2WEL7BD(KKSTP,KKPER,IUNIT(63),IGRID)
              IF(IUNIT(2)  /= Z) CALL GWF2WEL8BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(3)  /= Z) CALL GWF2DRN7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(4)  /= Z) CALL GWF2RIV7BD(KKSTP,KKPER,IGRID)
              !
              IF(IUNIT(5) /= Z) THEN
                  !
                  IF(IUNIT(22) /= Z.AND.EVT_NEVTOP == 3) CALL GWF2LAK7ST(0,IGRID)
                  !
                  CALL GWF2EVT7BD(KKSTP,KKPER,IGRID)
                  !
                  IF(IUNIT(22) /= Z.AND.EVT_NEVTOP == 3) CALL GWF2LAK7ST(1,IGRID)
              END IF
              !
              IF(IUNIT(6) /= Z) CALL GWF2RIP4BD(KKSTP,KKPER,IGRID)        !inserted by schmid
              IF(IUNIT(7) /= Z) CALL GWF2GHB7BD(KKSTP,KKPER,IGRID)
              !
              IF(IUNIT(8) /= Z) THEN
                  !
                  IF(IUNIT(22) /= Z.AND.RCH_NRCHOP == 3) CALL GWF2LAK7ST(0,IGRID)
                  !
                  CALL GWF2RCH7BD(KKSTP,KKPER,IGRID)
                  !
                  IF(IUNIT(22) /= Z.AND.RCH_NRCHOP == 3) CALL GWF2LAK7ST(1,IGRID)
              END IF
              !
              IF(IUNIT(16) /= Z) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(17) /= Z) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(18) /= Z) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(19) /= Z) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(39) /= Z) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(40) /= Z) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
              !
              ! (CJM) Added RCH unit number for RCH->SFR.
              !
              IF(IUNIT(44) /= Z) CALL GWF2SFR7BD(KKSTP,KKPER,IUNIT(15),                       &
                                                   IUNIT(22),IUNIT(46),IUNIT(55),NSOL,IUNIT(8), &
                                                   ILGR,NGRIDS,IGRID)
              !
              ! Moved call to UZF1BD to follow SFR7BD for printing net recharge in UZF.
              !
              IF(IUNIT(55) /= Z) CALL GWF2UZF1BD(KKSTP,KKPER,IUNIT(22),IUNIT(44),IUNIT(66),IGRID)
              IF(IUNIT(22) /= Z) CALL GWF2LAK7BD(KKSTP,KKPER,IUNIT(15),IUNIT(46),IUNIT(44),IUNIT(55),NSOL,IGRID)
              IF(IUNIT(50) /= Z) CALL GWF2MNW27BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(52) /= Z) CALL GWF2MNW17BD(NSTP(KPER),KKSTP,KKPER,IGRID)
              IF(IUNIT(54) /= Z) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(57) /= Z) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(64) /= Z) CALL GWF2SWR7BD(KKSTP,KKPER,IGRID)  !SWR - JDH
              !
              ! FARM DEMAND AND SUPPLY, FARM WELLS, AND FARM NET-RECHARGE
              !    FMP_FM is inserted to allow recalculating FMP-flowrates, which
              !        may de a function of SFR & MNW flowrates: Q-fmp(h,Q-sfr,Q-mnw).
              !
              IF (IUNIT(61) /= Z)  CALL FMP_BD(KITER,KPER,KSTP,IGRID,NGRIDS,ILGR,LGRITER)
              !
              IF(IUNIT(48) /= Z) CALL GWF2BFH2BD(KKSTP,KKPER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
              !
              IF(ILGR /= Z)THEN
                   IF(ISCHILD <= Z) CALL GWF2LGR2PBD(KKSTP,KKPER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),NGRIDS)
                   IF(ISCHILD >  Z) CALL GWF2LGR2CBD(KKSTP,KKPER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62))
              ENDIF
              !
              IF(IUNIT(65) /= Z) CALL GWF2SWI2BD(KKSTP,KKPER,IGRID)  !SWI - JDH
              IF(IUNIT(66) /= Z) CALL GWF2AG7BD(KKSTP,KKPER,IUNIT(63))
              !BARC**ADD THESE
              !BIRK----EXCHANGE BUDGET OF CONDUIT AND FISSURED SYSTEM
              !
              IF(IUNIT(58) /= Z) CALL GWF2CFP1BD(KKPER,KKSTP,IUNIT(59),MSUM) !TR: 2017 07 20 CFPv2
              !LMT
              !LMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
              !LMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
              !LMT
              !
              IF(IUNIT(49) /= Z) CALL LMT8BD(KKSTP,KKPER,IGRID)
              !
              !  Set NWT heads to Hdry when head is below bottom.
              !
              IF(IUNIT(63) /= Z) CALL GWF2NWT1BD(KITER,IGRID)
              !
              !  Observation simulated equivalents
              CALL OBS2BAS7SE(IUNIT(28),IGRID)
              !
              IF(IUNIT(33) /= Z) CALL OBS2DRN7SE(IGRID)
              IF(IUNIT(34) /= Z) CALL OBS2RIV7SE(IGRID)
              IF(IUNIT(35) /= Z) CALL OBS2GHB7SE(IGRID)
              IF(IUNIT(38) /= Z) CALL OBS2CHD7SE(KKPER, IUNIT(62), IGRID)
              IF(IUNIT(41) /= Z) CALL OBS2DRT7SE(IGRID)
              IF(IUNIT(43) /= Z) THEN
                                   CALL GWF2HYD7BAS7SE(1,IGRID)
                                   IF(IUNIT(19) /= Z) CALL GWF2HYD7IBS7SE(1,IGRID)
                                   IF(IUNIT(54) /= Z) CALL GWF2HYD7SUB7SE(1,IGRID)
                                   IF(IUNIT(18) /= Z) CALL GWF2HYD7STR7SE(1,IGRID)
                                   IF(IUNIT(44) /= Z) CALL GWF2HYD7SFR7SE(1,IGRID)
              END IF
              !
              !7C5---PRINT AND/OR SAVE DATA.
              !
              IF(IUNIT(13) /= Z)                                         &
                CALL PCG7OT(HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,           &
                         HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,     &
                         MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,  &
                         NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF, &
                         HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY,       &
                         IHCOFADD,BPOLY)
              !
              CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID,BUDPERC,KITER,MXITER)
              !
              IF(IUNIT(19) /= Z) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),IGRID)
              !
              IF(IUNIT(37) /= Z)THEN
                            IF(IOHUFHDS  /= Z .OR.IOHUFFLWS  /= Z) CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
              ENDIF
              !
              IF(IUNIT(51) /= Z) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,KKPER,IGRID)
              IF(IUNIT(54) /= Z) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),IGRID)
              IF(IUNIT(57) /= Z) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
              IF(IUNIT(43) /= Z) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
              !
              IF(ILGR  /= Z) THEN
                               IF(ISCHILD >= Z) CALL GWF2LGR2COT(KKSTP,KKPER,IGRID)
              ENDIF
              !
              !------CHECK FOR CHANGES IN HEAD AND FLUX BOUNDARY CONDITIONS
              !
              IF(IUNIT(48) /= Z) CALL GWF2BFH2OT(KKSTP,KKPER,IGRID)
              !
              !7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
              !
              INTER = INTER//NUM2STR(KITER)//', '
              IF(KKSTP < NSTP(KPER)) THEN
                                     IF(MOD(KKSTP,4) == Z) INTER = INTER//NL//REPEAT(BLNK,79)
              END IF
              !
              IF ( ICNVG == Z ) THEN  !seb moved outside of IGRID LOOP
                  !
                  NCVGERR = NCVGERR + ONE
                  !
                  IF(ABS(BUDPERC) > STOPER) CALL STOP_ERROR(MSG=NL//'Failure to meet solver convergence criteria'//BLN//'Budget percent discrepancy is '//NUM2STR(BUDPERC)//' %'//BLN//'The maximum allowed budget error is '//NUM2STR(STOPER)//' %'//BLN//'To disable this stop/check use the BAS option "NO_FAILED_CONVERGENCE_STOP"'//BLN//'Or you can change the maximum allowed budget error is with the BAS option "STOPERROR" followed by the new max allowed error in percent.')
                  !
              END IF
              !
              CALL LISTSPLIT%SIZE_CHECK()            !CHECK IF LIST FILE SHOULD BE SPLIT INTO A NEW FILE DUE TO ITS SIZE  --POINTER ALREADY SET UP BY GWF2BAS7OT
              !
              IF(IOUT /= LISTSPLIT%IU) IOUT=LISTSPLIT%IU
              !
              !slang IF(HAS_SLANG) CALL SLANG_TS_END_EVAL(KPER, KSTP, KITER, IOUT)
              !
          ENDDO GRID_OC
          !
      END DO  TIME_STEP  !############################################################################################################
      !
      !slang IF(HAS_SLANG) CALL SLANG_SP_END_EVAL(KPER, KSTP, KITER, IOUT)
      !
      !Stop Stress Period Timer and print clock time
      !
      I = Z
      CALL SYSTEM_CLOCK(COUNT=FINISH)                    ! Stop timing
      DO WHILE(FINISH <= Z .AND. I < 1000)
         CALL SYSTEM_CLOCK(COUNT=FINISH)
         I = I + ONE
      END DO
      IF(FINISH <= Z) CALL SYSTEM_CLOCK(COUNT=FINISH)    ! Stop timing
      !
      CPU_TIME=REAL( (FINISH-START))/REAL(ClockRate*60)  ! in minutes
      !
      IF(CPU_TIME < 0. .OR. FINISH <= Z .OR. START <= Z) THEN
          !
          WRITE(*,'(11x,A,I6,4X,4A/)') 'Stress Period: ', KPER, ' CPU Time: ', '   ???', ' min      Solver Iter: ', INTER(:LEN(INTER)-2)
          !
      ELSEIF(CPU_TIME < 1E3) THEN
          !
          WRITE(*,'(11x,A,I6,3x,A,F7.3, 2A/)') 'Stress Period: ',KPER,' CPU Time: ', CPU_TIME,' min      Solver Iter: ',INTER(:LEN(INTER)-2)
      ELSE
          WRITE(*,'(11x,A,I6,3X,4A/)') 'Stress Period: ', KPER, ' CPU Time: ', NUM2STR( CPU_TIME,7 ), ' min      Solver Iter: ', INTER(:LEN(INTER)-2)
      END IF
      !
  END DO STRESS_PERIOD  ! ============================================================================================================+++++++++++++++
  !
  !8------END OF SIMULATION
  !
  GRID_OT: DO IGRID = ONE, NGRIDS
      !
      CALL SGWF2BAS7PNT(IGRID)
      IF(IUNIT(52) /= Z .AND. ICNVG /= Z) CALL GWF2MNW17OT(IGRID)
      !
      !-SAVE RESTART RECORDS FOR SUB PACKAGE
      IF(IUNIT(54) /= Z) CALL GWF2SUB7SV(IGRID)
      !
      ! Observation output
      IF(IUNIT(28) /= Z) CALL OBS2BAS7OT(IUNIT(28),IGRID)
      IF(IUNIT(33) /= Z) CALL OBS2DRN7OT(IGRID)
      IF(IUNIT(34) /= Z) CALL OBS2RIV7OT(IGRID)
      IF(IUNIT(35) /= Z) CALL OBS2GHB7OT(IGRID)
      IF(IUNIT(38) /= Z) CALL OBS2CHD7OT(IGRID)
      IF(IUNIT(41) /= Z) CALL OBS2DRT7OT(IGRID)
      !
      !-------OUTPUT RESULTS OF SWR TIMER
      !
      IF(IUNIT(64) /= Z) CALL GWF2SWR7OT(IGRID)
      !
      !slang IF(HAS_SLANG) CALL SLANG_SIM_END_EVAL(KPER, KSTP, IOUT)
      !
  ENDDO GRID_OT

  !
  !!!CALL GLO1BAS6ET(IOUT,IBDT,1)
  !
  GRID_DA: DO IGRID = ONE, NGRIDS
      !
      !9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
      !9------LAST BECAUSE IT DEALLOCATES IUNIT.
      !
      CALL SGWF2BAS7PNT(IGRID)
      !
      IF(ILGR      /= Z) CALL GWF2LGR2DA(IGRID)
      IF(IUNIT(1)  /= Z) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(67) /= Z) CALL GWF2WEL7DA(IGRID)
      IF(IUNIT(2)  /= Z) CALL GWF2WEL8DA(IGRID)
      IF(IUNIT(3)  /= Z) CALL GWF2DRN7DA(IGRID)
      IF(IUNIT(4)  /= Z) CALL GWF2RIV7DA(IGRID)
      IF(IUNIT(5)  /= Z) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(6)  /= Z) CALL GWF2RIP4DA(IGRID)
      IF(IUNIT(7)  /= Z) CALL GWF2GHB7DA(IGRID)
      IF(IUNIT(8)  /= Z) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(9)  /= Z) CALL SIP7DA(IGRID)
      IF(IUNIT(10) /= Z) CALL DE47DA(IGRID)
      IF(IUNIT(13) /= Z) CALL PCG7DA(IGRID)
      !
      IF(IUNIT(63) /= Z) THEN
          !
          CALL SGWF2NWT1PNT(IGRID)
          !
          IF    (LINMETH == ONE) THEN
                               CALL GMRES7DA(IGRID)
          ELSEIF(LINMETH == TWO) THEN
                               CALL XMD7DA(IGRID)
          END IF
          CALL GWF2NWT1DA(IGRID)
      END IF
      !
      IF(IUNIT(62) /= Z) CALL GWF2UPW1DA(IGRID)
      IF(IUNIT(16) /= Z) CALL GWF2FHB7DA(IGRID)
      IF(IUNIT(17) /= Z) CALL GWF2RES7DA(IGRID)
      IF(IUNIT(18) /= Z) CALL GWF2STR7DA(IGRID)
      IF(IUNIT(19) /= Z) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(20) /= Z) CALL GWF2CHD7DA(IGRID)
      IF(IUNIT(21) /= Z) CALL GWF2HFB7DA(IGRID)
      !
      IF(IUNIT(22) /= Z .OR. IUNIT(44) /= Z) CALL GWF2LAK7DA(IUNIT(22), IGRID)
      !
      IF(IUNIT(23) /= Z) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(37) /= Z) CALL GWF2HUF7DA(IGRID)
      IF(IUNIT(39) /= Z) CALL GWF2ETS7DA(IGRID)
      IF(IUNIT(40) /= Z) CALL GWF2DRT7DA(IGRID)
      IF(IUNIT(42) /= Z) CALL GMG7DA(IGRID)
      IF(IUNIT(70) /= Z) CALL PCGN2DA(IGRID)
      IF(IUNIT(44) /= Z) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46) /= Z) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50) /= Z) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(51) /= Z) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(52) /= Z) CALL GWF2MNW17DA(IGRID)
      IF(IUNIT(54) /= Z) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55) /= Z) CALL GWF2UZF1DA(IGRID)
      IF(IUNIT(57) /= Z) CALL GWF2SWT7DA(IGRID)
      IF(IUNIT(64) /= Z) CALL GWF2SWR7DA(IGRID)  !SWR - JDH
      IF(IUNIT(65) /= Z) CALL GWF2SWI2DA(IGRID)  !SW1 - JDH
      IF(IUNIT(66) /= Z) CALL GWF2AG7DA()
      !
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
      !
      IF(IUNIT(33) /= Z) CALL OBS2DRN7DA(IGRID)
      IF(IUNIT(34) /= Z) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35) /= Z) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(38) /= Z) CALL OBS2CHD7DA(IGRID)
      IF(IUNIT(41) /= Z) CALL OBS2DRT7DA(IGRID)
      IF(IUNIT(43) /= Z) CALL GWF2HYD7DA(IGRID)
      IF(IUNIT(49) /= Z) CALL LMT8DA(IGRID)
      IF(IUNIT(61) /= Z) CALL FMP3DA(IGRID)
      !
  ENDDO GRID_DA
  !
  !
  !10-----END OF PROGRAM.
  !      WRITE CLOSE OUT MESSAGE AND DEALLOCATE BAS
  !
  CALL SGWF2BAS7PNT(1)
  !
  CALL MASS_ERROR_COUNT_PRINT(IOUT,MXITER,NCVGERR,ISTP,PVOL_ERR)
  !
  CALL SIM_END%NOW()
  !
  WRITE(STDOUT,'(/ 2x,2A )') 'Simulation finished at   ', SIM_END%STR('  ')
  WRITE(STDOUT,'(/ 2x,2A/)') 'with an elapsed time of: ', SIM_END%STR_ELAPSED(SIM_START)
  !
  WRITE(IOUT  ,'(/// 1x,2A/)') 'Simulation finished at  ', SIM_END%STR('  ')
  WRITE(IOUT  ,'(/ 1x,2A/)') 'Which had an elapsed run time of: ', SIM_END%STR_ELAPSED(SIM_START)
  !WRITE(IOUT  ,'(/ 1x,2A/)') 'Elapsed run time: ', SIM_END%STR_ELAPSED(SIM_START)
  !
  I = GET_WARN()
  IF(I /= Z) THEN
      WRITE(I,'(//1x,2A///)') 'OneWater simulation completed with an elapsed run time of: ', SIM_END%STR_ELAPSED(SIM_START)
      WRITE(I,'(A//////)') REPEAT('#',104)
      FLUSH(I)
  END IF
  !
  WRITE(*,'(//19x,A//)')   'OneWater Simulation Now Complete'
  WRITE(IOUT,'( /4x,A//)') 'OneWater Simulation Now Complete'
  !
  !CALL PROGRAM_DONE(IOUT)
  !
  FLUSH(IOUT)
  !
  !9------DEALLOCATE BAS MEMORY (MADE LAST BECAUSE IT DEALLOCATES IOUT).
  !
  DO IGRID = ONE, NGRIDS
                      CALL GWF2BAS7DA(IGRID)
  ENDDO
  !
  IF(USE_PAUSE) CALL PAUSE('BAS "PAUSE" OPTION ACTIVATED, SIMULATION IS NOW PAUSED.'//BLN//'       PRESS ENTER TO END SIMULATION')
  !
END SUBROUTINE
!
SUBROUTINE CMD_PRINT_STOP(SIZ)
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  USE CONSOLE_COMMANDER, ONLY: CMD_PRINT
  INTEGER, INTENT(INOUT):: SIZ
  !
  CALL CMD_PRINT('Solver Iter: Fast Sim. so disabling iter print out. Enabling BAS Option "NO_SHOWPROGRESS"', SIZ)
  !
END SUBROUTINE
!
SUBROUTINE CMD_PRINT_ITER(KITER, SIZ)
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  USE CONSOLE_COMMANDER, ONLY: CMD_PRINT
  INTEGER, INTENT(IN   ):: KITER
  INTEGER, INTENT(INOUT):: SIZ
  CHARACTER(32):: LINE
  !
  WRITE(LINE,'(3x,A , 1x,A)') "Solver Iter:", NUM2STR(KITER, 3)
  CALL CMD_PRINT(LINE, SIZ)
  !
END SUBROUTINE
!
SUBROUTINE CMD_PRINT_ERROR(SIZ)
  USE CONSTANTS,         ONLY: Z, ONE, DZ, NEARZERO_12
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  USE CONSOLE_COMMANDER, ONLY: CMD_EXTEND
  USE GLOBAL,            ONLY:NCOL,NROW,NLAY,HNEW,HNEW_OLD,IBOUND,HCloseBAS, RCloseL2BAS !CELL_MASS_BALANCE
  INTEGER,      INTENT(INOUT):: SIZ
  CHARACTER(128):: LINE
  INTEGER:: I, J, K, N
  DOUBLE PRECISION:: DIF, RES_SUM, HED_MAX!, HED_SUM, RES_MAX
  !
  WRITE(LINE,'(A , (2x,A, 1x,A, 3x,A, 1x,A))') " =>", &
                       "HClose", NUM2STR(HCloseBAS,9),   &
                    "L2-RClose", NUM2STR(RCloseL2BAS)
  !
!!!  RES_SUM = DZ
!!!!  HED_SUM = DZ
!!!!  RES_MAX = DZ
!!!  HED_MAX = DZ
!!!  DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
!!!     IF(IBOUND(J,I,K)>Z) THEN
!!!         !
!!!         DIF = CELL_MASS_BALANCE(I,J,K)
!!!         !
!!!         IF(DIF < DZ) DIF = -DIF
!!!         !
!!!!         IF( RES_MAX < DIF ) RES_MAX = DIF
!!!         !
!!!         RES_SUM = RES_SUM + DIF*DIF
!!!     END IF
!!!  END DO; END DO; END DO
!!!  !
!!!  DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
!!!     IF(IBOUND(J,I,K)>Z) THEN
!!!         !
!!!         DIF = HNEW(J,I,K) - HNEW_OLD(J,I,K)
!!!         !
!!!         IF(DIF < DZ) DIF = -DIF
!!!         !
!!!         IF( HED_MAX < DIF ) HED_MAX = DIF
!!!         !
!!!!         HED_SUM = HED_SUM + DIF*DIF
!!!     END IF
!!!  END DO; END DO; END DO
!!!  !
!!!  IF(RES_SUM > NEARZERO_12) RES_SUM = SQRT(RES_SUM)
!!!!  IF(HED_SUM > NEARZERO_12) HED_SUM = SQRT(HED_SUM)
!!!  !
!!!  N=9
!!!  WRITE(LINE,'(A , (2x A, 1x A, 3x A, 1x A))') " =>", &
!!!                    "HClose",    NUM2STR(HED_MAX,N),   &
!!!!                    "L2-HClose", NUM2STR(HED_SUM,N),   &
!!!!                    "RClose",   NUM2STR(RES_MAX,N),    &
!!!!                    "L2-RClose", NUM2STR(RES_SUM)
  !
  CALL CMD_EXTEND(LINE, SIZ)
  !
END SUBROUTINE
!
!
!!!SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
!!!  ! ******************************************************************
!!!  ! Get end time and calculate elapsed time
!!!  ! ******************************************************************
!!!  !
!!!  !    SPECIFICATIONS:
!!!  ! ------------------------------------------------------------------
!!!  INTEGER IBDT(8), IEDT(8), IDPM(12)
!!!  DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
!!!  DATA NSPD/86400/  ! Seconds per day
!!!  !  ------------------------------------------------------------------
!!!  !
!!!  !  Get current date and time, assign to IEDT, and write.
!!!  CALL DATE_AND_TIME(VALUES=IEDT)
!!!  WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
!!! 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
!!!  IF(IPRTIM > 0) THEN
!!!      WRITE(IOUT,'(1X)')
!!!      WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
!!!  END IF
!!!  !
!!!  !     Calculate elapsed time in days and seconds
!!!  NDAYS=0
!!!  LEAP=0
!!!  IF (MOD(IEDT(1),4) == 0) LEAP = 1
!!!  IBD = IBDT(3)            ! BEGIN DAY
!!!  IED = IEDT(3)            ! END DAY
!!!  ! FIND DAYS
!!!  IF (IBDT(2) /= IEDT(2)) THEN
!!!    ! MONTHS DIFFER
!!!    MB = IBDT(2)             ! BEGIN MONTH
!!!    ME = IEDT(2)             ! END MONTH
!!!    NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
!!!    IF (MB > ME) NM = NM+12
!!!    MC=MB-1
!!!    DO 10 M=1,NM
!!!      MC=MC+1                ! MC IS CURRENT MONTH
!!!      IF (MC == 13) MC = 1
!!!      IF (MC == MB) THEN
!!!        NDAYS = NDAYS+IDPM(MC)-IBD
!!!        IF (MC == 2) NDAYS = NDAYS + LEAP
!!!      ELSEIF (MC == ME) THEN
!!!        NDAYS = NDAYS+IED
!!!      ELSE
!!!        NDAYS = NDAYS+IDPM(MC)
!!!        IF (MC == 2) NDAYS = NDAYS + LEAP
!!!      ENDIF
!!!    10 CONTINUE
!!!      ELSEIF (IBD < IED) THEN
!!!        !    START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
!!!        NDAYS = IED-IBD
!!!      ENDIF
!!!      ELSEC=NDAYS*NSPD
!!!      !
!!!      ! ADD OR SUBTRACT SECONDS
!!!      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
!!!      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
!!!      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
!!!      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
!!!      !
!!!      ! CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
!!!      NDAYS = ELSEC/NSPD
!!!      RSECS = MOD(ELSEC,86400.0)
!!!      NHOURS = RSECS/3600.0
!!!      RSECS = MOD(RSECS,3600.0)
!!!      NMINS = RSECS/60.0
!!!      RSECS = MOD(RSECS,60.0)
!!!      NSECS = RSECS
!!!      RSECS = MOD(RSECS,1.0)
!!!      MSECS = NINT(RSECS*1000.0)
!!!      NRSECS = NSECS
!!!      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
!!!      !
!!!      ! Write elapsed time to screen
!!!        IF (NDAYS > 0) THEN
!!!          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
!!! 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2, ' Minutes, ',I2,' Seconds',/)
!!!        ELSEIF (NHOURS > 0) THEN
!!!          WRITE(*,1020) NHOURS,NMINS,NRSECS
!!! 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2, ' Minutes, ',I2,' Seconds',/)
!!!        ELSEIF (NMINS > 0) THEN
!!!          WRITE(*,1030) NMINS,NSECS,MSECS
!!! 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ', I2,'.',I3.3,' Seconds',/)
!!!        ELSE
!!!          WRITE(*,1040) NSECS,MSECS
!!! 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
!!!        ENDIF
!!!        !
!!!        !  Write times to file if requested
!!!      IF(IPRTIM > 0) THEN
!!!        IF (NDAYS > 0) THEN
!!!            WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
!!!        ELSEIF (NHOURS > 0) THEN
!!!            WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
!!!        ELSEIF (NMINS > 0) THEN
!!!            WRITE(IOUT,1030) NMINS,NSECS,MSECS
!!!        ELSE
!!!            WRITE(IOUT,1040) NSECS,MSECS
!!!        ENDIF
!!!      ENDIF
!!!      !
!!!END SUBROUTINE
!!!!
!!!!
!!!SUBROUTINE RESTARTHEADS(IOUT)
!!!  !  ******************************************************************
!!!  !  READ HEADS FOR RESTART AND COPY INTO HNEW
!!!  !  ******************************************************************
!!!  !
!!!      USE GLOBAL,      ONLY:STRT,NCOL,NROW,NLAY,IUNITSTART,HNEW,IBOUND,IXSEC
!!!      USE GWFBASMODULE,ONLY:HNOFLO
!!!      DOUBLE PRECISION HNF
!!!      CHARACTER(24) ANAME(1)
!!!      DATA ANAME(1) /'            RESTART HEAD'/
!!!      !     SPECIFICATIONS:
!!!      !  ------------------------------------------------------------------
!!!      !
!!!      !8G-----READ INITIAL HEADS FOR RESTART.
!!!      IF(IXSEC == 0) THEN
!!!         DO 300 K=1,NLAY
!!!         KK=K
!!!         CALL U2DREL(STRT(:,:,KK),ANAME(1),NROW,NCOL,KK,IUNITSTART,IOUT)
!!!  300    CONTINUE
!!!      ELSE
!!!         CALL U2DREL(STRT(:,:,1),ANAME(1),NLAY,NCOL,-1,IUNITSTART,IOUT)
!!!      END IF
!!!      !
!!!      !9------COPY INITIAL HEADS FROM STRT TO HNEW.
!!!      HNF = HNOFLO
!!!      DO 400 K=1,NLAY
!!!      DO 400 I=1,NROW
!!!      DO 400 J=1,NCOL
!!!      HNEW(J,I,K)=STRT(J,I,K)
!!!      IF(IBOUND(J,I,K) == 0) HNEW(J,I,K)=HNF
!!!  400 CONTINUE
!!!      RETURN
!!!END SUBROUTINE
!
!USE UTIL_INTERFACE, ONLY: WRITE_DATA
!IF(  ) THEN
!   CALL WRITE_DATA(CR, 321, "CR")
!   CALL WRITE_DATA(CC, 321, "CC")
!   CALL WRITE_DATA(CV, 321, "CV")
!   CALL WRITE_DATA(BOTM, 321, "BOTM")
!   CALL WRITE_DATA(RHS, 321, "RHS", .TRUE.)
!   CALL WRITE_DATA(HCOF, 321, "HCOF", .TRUE.)
!   CALL WRITE_DATA(HNEW, 321, "HNEW", .TRUE.)
!   STOP 0
!END IF
