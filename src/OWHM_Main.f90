!     ******************************************************************
!     MAIN CODE FOR U.S. GEOLOGICAL SURVEY SIMULATION MODEL
!
!         MODFLOW - ONE-WATER Hydrologic Model (OWHM) Version 2
!
!     WITH THE FARM PROCESS (version 4), LGR2, NWT1, SWR1, RIP1, and SWI2
!
!     ******************************************************************
!
PROGRAM MODFLOW_OWHM
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: STDIN=>INPUT_UNIT, STDOUT=>OUTPUT_UNIT
  !
  ! Utility Modules
  !
  USE OWHM_HEADER_INTERFACE
  USE OPENSPEC
  USE CONSTANTS,                 ONLY: NL, BLN, BLNK, Z, ONE
  USE UTIL_INTERFACE,            ONLY: STOP_ERROR, GET_WARN, PAUSE
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
  !USE SLANG_PACKAGE_INTERFACE
  !
  ! Flow Package Modules
  !
  USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
  !
  ! FMP Modules
  !
  USE FMP_GLOBAL,   ONLY:ISTARTFL, FMP3DA, FMP_LGR_PNT                            
  USE FMP_MAIN,     ONLY:FMP_AR, FMP_RP, FMP_AD, FMP_SUBLINK, FMP_FM, FMP_CNVG, FMP_BD
  !
  !USE SWO_INTERFACE, ONLY: SWO_DATA, SWO_AR
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
  ! Variable Declaration -----------------------------------------
  !
  !TYPE(SWO_DATA), ALLOCATABLE:: SWO        !Only Allocated if SWO is enabled
  !
  TYPE(DATE_OPERATOR):: SIM_START, SIM_END
  !
  CHARACTER(:), ALLOCATABLE:: FNAME
  !INTEGER IBDT(8)
  CHARACTER(:),ALLOCATABLE:: INTER  !Print out of iteration info
  !
  INTEGER(4):: START, FINISH, ClockRate
  INTEGER:: ISTP
  REAL:: CPU_TIME
  LOGICAL:: FASTFORWARD
  !
  INTEGER:: I
  INTEGER:: NAM_UNIT, LGR_UNIT, IGRID, NGRIDS, LG, NSOL, NCVGERR
  INTEGER:: KPER, KKPER, KSTP, KKSTP
  INTEGER:: ICNVG, KITER, KKITER, LGRCNVG, LGRITER, ITREAL2
  INTEGER:: IERR, IBDRET
  INTEGER:: IC1,IC2,IR1,IR2,IL1,IL2,IDIR
  INTEGER:: CFPMODE
  INTEGER:: IOUTS !USED BY GWT --Kept set to null file if not used
  REAL:: BUDPERC
  LOGICAL:: USE_PAUSE
  !
  !Set up package names ----------------------------------------------------------------------------
  !
  CUNIT = [  'BCF  ', 'WEL  ', 'DRN  ', 'RIV  ', 'EVT  ', 'RIP  ', 'GHB  ',  & !  7 
             'RCH  ', 'SIP  ', 'DE4  ', '     ', 'OC   ', 'PCG  ', 'lmg  ',  & ! 14
             'gwt  ', 'FHB  ', 'RES  ', 'STR  ', 'IBS  ', 'CHD  ', 'HFB  ',  & ! 21 
             'LAK  ', 'LPF  ', 'DIS  ', '     ', 'PVAL ', 'ssub ', 'HOB  ',  & ! 28
             '     ', '     ', 'ZONE ', 'MULT ', 'DROB ', 'RVOB ', 'GBOB ',  & ! 35
             '     ', 'HUF  ', 'CHOB ', 'ETS  ', 'DRT  ', 'DRTOB', 'GMG  ',  & ! 42
             'HYD  ', 'SFR  ', 'swo  ', 'GAGE ', 'LVDA ', 'BFH  ', 'LMT  ',  & ! 49  IUNIT(45) = SWO
             'MNW2 ', 'MNWI ', 'MNW1 ', 'KDEP ', 'SUB  ', 'UZF  ', 'GWM  ',  & ! 56
             'SWT  ', 'CFP  ', 'COC  ', 'CRCH ', 'FMP  ', 'UPW  ', 'NWT  ',  & ! 63
             'SWR  ', 'SWI  ', 'WEL1 ', '     ', '     ', 'slang', 'PCGN '   ] ! 70     
             !('    ',I=1,30)                                       ]    ! 71-100 
  !
  ! ------------------------------------------------------------------------------------------------
  !
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
  USE_PAUSE = .FALSE.
  !
  IL1=ONE  !This was originally set this way at declaration...which would make it have implied SAVE
  !
  ALLOCATE(CHARACTER(700)::FNAME)
  !
  CALL SET_TO_NULL_UNIT(IOUTS)
  !
  !3---Check if LGR is in use
  !
  CALL GET_NAME_AND_LGR_CHECK(ILGR, NGRIDS, FNAME)  !Set FNAME to LGR Control or Name File
  !
  ! Set Simulation Starting Date and Time
  !
  CALL SIM_START%NOW()
  !
  WRITE(STDOUT,'(//1x 2A//)') 'OneWater Simulation Initiated at ', SIM_START%STR('  ')
  !
  !GET THE NAME OF THE NAME FILE
  GRID_AR: DO IGRID = 1, NGRIDS !-----------------------------------------------------------------------------------------------------------
      !
      !4A------IF USING LGR, READ NAMES FROM LGR NAME FILE
      !
      IF(ILGR .NE. 0) THEN
                      CALL GETNAMFILLGR(LGR_UNIT,FNAME,IGRID)  !Set LGR_UNIT from FNAME, update FNAME to current NAME_FILE
      ELSE
                      LGR_UNIT = Z
      END IF
      !
      !4B-----OPEN NAME FILE.
      !
      CALL OPEN_NAME_FILE(FNAME, NAM_UNIT, LGR_UNIT)
      !
      WRITE(*,'(2A/)')' Loading Packges from Name file: ',TRIM(FNAME)
      !
      !
      !6------ALLOCATE AND READ (AR) PROCEDURE
      !
      NSOL = ONE  !seb think this is Number of Solutes, but never actually updated used other than loop index
      !
      !              (INUNIT,   CUNIT,DIS,ZON,MLT, IGRID, OC, PVAL)
      CALL GWF2BAS7AR(NAM_UNIT, CUNIT, 24, 31, 32, IGRID, 12, 26, USE_PAUSE)
      !
      IF(IGRID==1) THEN
                   !
                   I = 0
                   IF    (IUNIT( 9) .NE. Z) I = I + 1
                   IF    (IUNIT(10) .NE. Z) I = I + 1
                   IF    (IUNIT(13) .NE. Z) I = I + 1
                   IF    (IUNIT(42) .NE. Z) I = I + 1
                   IF    (IUNIT(62) .NE. Z) I = I + 1
                   IF    (IUNIT(70) .NE. Z) I = I + 1
                   IF(I == Z ) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='No Solver Package Found in Name File.'//BLN//'Simulation requires that 1 Solver Package is specified in the Name File (PCG, PCGN, NWT, GMG, DE4, or SIP).')
                   IF(I > ONE) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='More than 1 Solver Package Found in Name File.'//BLN//'Simulation requires that only 1 Solver Package is specified in the Name File (PCG, PCGN, NWT, GMG, DE4, or SIP).')
                   !
                   IF    (IUNIT( 9) .NE. Z) THEN; GW_SOLVER = 'SIP'
                   ELSEIF(IUNIT(10) .NE. Z) THEN; GW_SOLVER = 'DE4'
                   ELSEIF(IUNIT(13) .NE. Z) THEN; GW_SOLVER = 'PCG'
                   ELSEIF(IUNIT(42) .NE. Z) THEN; GW_SOLVER = 'GMG'
                   ELSEIF(IUNIT(62) .NE. Z) THEN; GW_SOLVER = 'NWT'
                   ELSEIF(IUNIT(70) .NE. Z) THEN; GW_SOLVER = 'PCGN'
                   ELSE
                                                  GW_SOLVER = ''
                   END IF
                   !---------------------------------------------------------------
                   I = 0
                   IF    (IUNIT(23) .NE. Z) I = I + 1
                   IF    (IUNIT(37) .NE. Z) I = I + 1
                   IF    (IUNIT(63) .NE. Z) I = I + 1
                   IF(I == Z ) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='No Flow Package Found in Name File.'//BLN//'Simulation requires that 1 Flow Package is specified in the Name File (LPF, UPW, or HUF).')
                   IF(I > ONE) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='More than 1 Flow Package Found in Name File.'//BLN//'Simulation requires that only 1 Flow Package is specified in the Name File (LPF, UPW, or HUF).')
                   !
                   IF    (IUNIT(23) .NE. Z) THEN; GW_FLOW_PACK = 'LPF'
                   ELSEIF(IUNIT(37) .NE. Z) THEN; GW_FLOW_PACK = 'HUF'
                   ELSEIF(IUNIT(63) .NE. Z) THEN; GW_FLOW_PACK = 'UPW'
                   ELSE
                                                  GW_FLOW_PACK = ''
                   END IF
      END IF
      !
      IF(ILGR .NE. Z) CALL GWF2LGR2AR(LGR_UNIT,FNAME,NGRIDS,IGRID)
      !
      IF(IUNIT(50).NE.Z .AND. IUNIT(52).NE.Z) CALL STOP_ERROR(INFILE=NAM_UNIT, MSG='OneWater Cannot simulate both MNW1 and MNW2 at the same time. Please remove one of the packages from the NAME file.')
      !
      !BASIC AND FLOW PACKAGES
      IF(IUNIT(21) .NE. Z) CALL GWF2HFB7AR(IUNIT(21),IGRID)
      IF(IUNIT(1)  .NE. Z) CALL GWF2BCF7AR(IUNIT(1),ILGR,IGRID)
      IF(IUNIT(23) .NE. Z) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      IF(IUNIT(37) .NE. Z) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47), IUNIT(53), ILGR, IGRID)
      !
      ! Allocate arrays for Newton Solver
      IF(IUNIT(63) .NE. Z) CALL GWF2NWT1AR(IUNIT(63),MXITER,IUNIT(22),ILGR,IGRID)
      IF(IUNIT(62) .NE. Z) CALL GWF2UPW1AR(IUNIT(62), Igrid)
      !
      !CALCULATE INITIAL UPPER MOST LAYER AND WATER TABLE--REQUIRES LAYHDT TO BE SET BY FLOW PACKAGE
      CALL GWF2BAS7UPLAY(IGRID)
      !
      ! Packages
      !
      IF(IUNIT(66) .NE. Z) CALL GWF2WEL7AR(IUNIT(66),IUNIT(63),IGRID)
      IF(IUNIT(2)  .NE. Z) CALL GWF2WEL8AR(IUNIT(2),IGRID)
      IF(IUNIT(3)  .NE. Z) CALL GWF2DRN7AR(IUNIT(3),IGRID)
      IF(IUNIT(4)  .NE. Z) CALL GWF2RIV7AR(IUNIT(4),IGRID)
      IF(IUNIT(5)  .NE. Z) CALL GWF2EVT7AR(IUNIT(5),IGRID)
      If(IUNIT(6)  .NE. Z) CALL GWF2RIP4AR(IUNIT(6),IGRID)               !inserted by schmid
      IF(IUNIT(7)  .NE. Z) CALL GWF2GHB7AR(IUNIT(7),IGRID)
      IF(IUNIT(8)  .NE. Z) CALL GWF2RCH7AR(IUNIT(8),IGRID)
      IF(IUNIT(16) .NE. Z) CALL GWF2FHB7AR(IUNIT(16),IGRID)
      IF(IUNIT(17) .NE. Z) CALL GWF2RES7AR(IUNIT(17),IGRID)
      IF(IUNIT(18) .NE. Z) CALL GWF2STR7AR(IUNIT(18),IGRID)
      IF(IUNIT(19) .NE. Z) CALL GWF2IBS7AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20) .NE. Z) CALL GWF2CHD7AR(IUNIT(20),IGRID)
      !
      IF(IUNIT(44) .NE. Z) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23), IUNIT(37),IUNIT(15),NSOL,IOUTS, IUNIT(62),IUNIT(55),IGRID)  !IOUTS used by GWT..not longer used
      !
      IF(IUNIT(55) .NE. Z) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1), IUNIT(23),IUNIT(37), IUNIT(63),IGRID)
      !
      IF(IUNIT(22) .NE. Z .OR. IUNIT(44) .NE. Z) CALL GWF2LAK7AR(IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL,IGRID)
      !
      IF(IUNIT(46) .NE. Z) CALL GWF2GAG7AR(IUNIT(46),IUNIT(44), IUNIT(22),IGRID)
      IF(IUNIT(39) .NE. Z) CALL GWF2ETS7AR(IUNIT(39),IGRID)
      IF(IUNIT(40) .NE. Z) CALL GWF2DRT7AR(IUNIT(40),IGRID)
      IF(IUNIT(54) .NE. Z) CALL GWF2SUB7AR(IUNIT(54),IGRID)
      !
      IF(IUNIT(48) .NE. Z) CALL GWF2BFH2AR(IUNIT(48),ILGR,IGRID)
      IF(IUNIT(9)  .NE. Z) CALL SIP7AR(IUNIT(9),MXITER,IGRID)
      IF(IUNIT(10) .NE. Z) CALL DE47AR(IUNIT(10),MXITER,IGRID)
      IF(IUNIT(13) .NE. Z) CALL PCG7AR(IUNIT(13),MXITER,IGRID)
      IF(IUNIT(42) .NE. Z) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(70) .NE. Z) CALL PCGN2AR(IUNIT(70),IFREFM,MXITER,IGRID)
      IF(IUNIT(50) .NE. Z) CALL GWF2MNW27AR(IUNIT(50),IUNIT(63),IGRID)
      IF(IUNIT(51) .NE. Z) CALL GWF2MNW2I7AR(IUNIT(51),IUNIT(50),IGRID)
      IF(IUNIT(52) .NE. Z) CALL GWF2MNW17AR(IUNIT(52),IUNIT(9),IUNIT(10),IUNIT(63),0,IUNIT(13),0,IUNIT(42),IUNIT(70),FNAME,IGRID)
      IF(IUNIT(57) .NE. Z) CALL GWF2SWT7AR(IUNIT(57),IGRID)
      IF(IUNIT(64) .NE. Z) CALL GWF2SWR7AR(IUNIT(64),IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IUNIT(44),IUNIT(63),IGRID)  !SWR  - JDH
      IF(IUNIT(65) .NE. Z) CALL GWF2SWI2AR(IUNIT(65),IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)                      !SWI  - JDH
      !
      IF(IUNIT(43) .NE. Z) THEN
                           CALL GWF2HYD7BAS7AR(IUNIT(43),IGRID,IUNIT(63))
                           !
                           IF(IUNIT(19) .NE. Z) CALL GWF2HYD7IBS7AR(IUNIT(43),IGRID)
                           IF(IUNIT(54) .NE. Z) CALL GWF2HYD7SUB7AR(IUNIT(43),IGRID)
                           IF(IUNIT(18) .NE. Z) CALL GWF2HYD7STR7AR(IUNIT(43),IGRID)
                           IF(IUNIT(44) .NE. Z) CALL GWF2HYD7SFR7AR(IUNIT(43),IGRID)
      END IF
      !
      !barc**add CFP AR                                                       !TR: 2017 07 20 CFPv2
      !seba--PROCESS AND CHECK CONDUIT DATA                                   !TR: 2017 07 20 CFPv2
      !
      IF(IUNIT(58) .NE. Z) CALL GWF2CFP1AR(IUNIT(58), IUNIT(59),IUNIT(60),CFPMODE)             !TR: 2017 07 20 CFPv2
      IF(IUNIT(49) .NE. Z) CALL LMT8BAS7AR(NAM_UNIT,CUNIT,IGRID)
      IF(IUNIT(50) .NE. Z) CALL GWF2MNW27AR2(IUNIT(50),KPER,IUNIT(9),IUNIT(10),0,IUNIT(13),0,IUNIT(42),IUNIT(70),IUNIT(15),IUNIT(63),IUNIT(61),IGRID)!SECOND CALL LOADS MNW2 WELLS...MOVED FROM RP UP  --KPER = 1 
      !
      !                                (IN_FMP,   IUNITSFR, IUNITMNW1,IUNITMNW2, IUNITUZF,  IUNITNWT, IUNITDRT,IGRID,ILGR,MXITER)
      IF(IUNIT(61) .NE. Z) CALL FMP_AR( IUNIT(61),IUNIT(44),IUNIT(52),IUNIT(50),IUNIT(55), IUNIT(63),IUNIT(40),IGRID,ILGR,MXITER )
      !
      ! Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
      !
      IF(IUNIT(33) .NE. Z) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
      IF(IUNIT(34) .NE. Z) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35) .NE. Z) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
      IF(IUNIT(38) .NE. Z) CALL OBS2CHD7AR(IUNIT(38),IGRID)
      IF(IUNIT(41) .NE. Z) CALL OBS2DRT7AR(IUNIT(41),IUNIT(40),IGRID)
      !
      IF(IUNIT(58) .NE. Z .AND. IUNIT(62) .NE. Z) THEN  !Check for CFP
          IF(ANY(IBOUND < Z) .OR. IUNIT(20) .NE. Z) THEN
               CALL STOP_ERROR(MSG='OneWater Cannot simulate both CFP with the NWT Solver when there are Constant Head Cells (IBOUND<0 or CHD Package) at this time. Contact the developer and he may be able to add that feature. Regretibly this was set aside due to time contraints.')
          END IF
      END IF
      !
      !IF(IUNIT(69) .NE. Z) CALL SLANG_AR(IUNIT(69), IOUT, NPER, HAS_STARTDATE, IGRID)
      !
      !IF(IUNIT(45) .NE. Z) THEN
      !                     ALLOCATE(SWO)
      !                     CALL SWO_AR(SWO, IUNIT(45), IUNIT(61), IUNIT(44), IOUT, ILGR, SPSTART)
      !END IF
      !
      CLOSE(NAM_UNIT)  !Name no longer used in simulation
      !
      ! END LOOP FOR ALLOCATING AND READING DATA FOR EACH GRID
      !      IF(ILGR.NE.0.AND.IGRID.GT.1.AND.IUNIT(62).NE.0) CALL #2PRTOCH()
  END DO GRID_AR  !-----------------------------------------------------------------------------------------------------------------------------------
  !
  IF(LGR_UNIT .NE. Z) CLOSE(LGR_UNIT)  !Close the LGR Control File
  !
  FLUSH(IOUT)  ! dump what is in the list to the list file from the buffer
  !
  !7------SIMULATE EACH STRESS PERIOD.
  !
  START=0 
  FINISH=0
  ClockRate=0
  I = 0
  DO WHILE(ClockRate.LE.0 .AND. I < 1000)     !  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
     CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate) ! Find the rate  
     I = I + 1
  END DO
  IF(ClockRate.LE.0) CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
  !
  STRESS_PERIOD: DO KPER = 1, NPER  ! ============================================================================================================
      !
      FASTFORWARD = KPER < SPSTART .OR. SPEND < KPER .OR. INPUT_CHECK
      !
      CALL SYSTEM_CLOCK(COUNT=START)                  ! Start timing 
      I = 0
      DO WHILE(START.LE.0 .AND. I < 1000)
         CALL SYSTEM_CLOCK(COUNT=START)               ! Start timing  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
         I = I + 1
      END DO
      IF(START.LE.0) CALL SYSTEM_CLOCK(COUNT=START)
      !
      KKPER = KPER
      !
      GRID_RP: DO IGRID = 1, NGRIDS !---------------------------------------------------------------------------------------------------------------
          !
          CALL SGWF2BAS7PNT(IGRID)
          !
          IF(IUNIT(62) .NE. Z) CALL GWF2UPWUPDATE(1,Igrid)
          !
          CALL GWF2BAS7ST(KKPER,IGRID)
          !
          IF(IUNIT(19) .NE. Z) CALL GWF2IBS7ST(KKPER,IGRID)
          IF(IUNIT(54) .NE. Z) CALL GWF2SUB7ST(KKPER,IGRID)
          IF(IUNIT(57) .NE. Z) CALL GWF2SWT7ST(KKPER,IGRID)
          !
          !7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
          !----------READ USING PACKAGE READ AND PREPARE MODULES.
          !swm: note the '1' below is hardwired for the parent grid
          !
          IF(ILGR      .NE. Z) CALL GWF2LGR2RP(KKPER,1,IGRID)
          IF(IUNIT(66) .NE. Z) CALL GWF2WEL7RP(IUNIT(66),IGRID)
          IF(IUNIT(2)  .NE. Z) CALL GWF2WEL8RP(IUNIT(2),IGRID)
          IF(IUNIT(3)  .NE. Z) CALL GWF2DRN7RP(IUNIT(3),IGRID,IUNIT(63))
          IF(IUNIT(4)  .NE. Z) CALL GWF2RIV7RP(IUNIT(4),IGRID)
          IF(IUNIT(5)  .NE. Z) CALL GWF2EVT7RP(IUNIT(5),IGRID)
          If(IUNIT(6)  .NE. Z) CALL GWF2RIP4RP(IUNIT(6),IGRID)             !inserted by schmid
          IF(IUNIT(7)  .NE. Z) CALL GWF2GHB7RP(IUNIT(7),IGRID,IUNIT(63))   !inserted Iunit(63) for NWT by rth
          !
          IF(IUNIT(8)  .NE. Z) CALL GWF2RCH7RP(IUNIT(8),IGRID)
          IF(IUNIT(17) .NE. Z) CALL GWF2RES7RP(IUNIT(17),IGRID)
          IF(IUNIT(18) .NE. Z) CALL GWF2STR7RP(IUNIT(18),IGRID)
          IF(IUNIT(43) .NE. Z .AND. &
             IUNIT(18) .NE. Z)      CALL GWF2HYD7STR7RP(IUNIT(43),KKPER,IGRID)
          IF(IUNIT(20) .NE. Z) CALL GWF2CHD7RP(IUNIT(20),IGRID)
          IF(IUNIT(21) .NE. Z) CALL GWF2HFB7RP(IUNIT(21),KKPER,IGRID)
          !
          IF(IUNIT(44) .NE. Z) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),     &
                                      IUNIT(22),IUNIT(54),KKPER,KKSTP,  &
                                      NSOL,IOUTS,IUNIT(1),              &
                                      IUNIT(23),IUNIT(37),              &
                                      IUNIT(62), IUNIT(55), IGRID)
        IF(IUNIT(43) .NE. Z .AND. &
           IUNIT(44) .NE. Z)      CALL GWF2HYD7SFR7RP(IUNIT(43),KKPER,IGRID)
        !
        IF(IUNIT(55) .NE. Z) CALL GWF2UZF1RP(IUNIT(55),KKPER,IUNIT(44),IGRID)
        !
        IF(IUNIT(22) .NE. Z) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),IUNIT(62),KKPER,NSOL,IOUTS,IGRID) !formerly IOUTS???
        !
        IF(IUNIT(46) .NE. Z) THEN
            IF(KKPER.EQ.1) CALL GWF2GAG7RP(IUNIT(15),IUNIT(22),IUNIT(55),NSOL,IGRID)
        END IF
        !
        IF(IUNIT(39) .NE. Z) CALL GWF2ETS7RP(IUNIT(39),IGRID)
        IF(IUNIT(50) .NE. Z) CALL GWF2MNW27RP(IUNIT(50),KKPER,IUNIT(9),IUNIT(10),0,IUNIT(13),0,IUNIT(42),IUNIT(70),IUNIT(15),IUNIT(63),IUNIT(61),     +                                                           IGRID)
        IF(IUNIT(51) .NE. Z) THEN
            IF(KKPER.EQ.1) CALL GWF2MNW2I7RP(IUNIT(51), 0, IGRID)
        END IF
        !
        IF(IUNIT(52) .NE. Z) CALL GWF2MNW17RP(IUNIT(52),IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),KKPER,IGRID)     
        !
        !
        !--barc**READ DATA FOR CONDUIT RECHARGE PACKAGE
        IF(IUNIT(58) .NE. Z) CALL GWF2CFP1RP(IUNIT(60))                   !TR: 2017 07 20 CFPv2
        !
        IF(IUNIT(48) .NE. Z) CALL GWF2BFH2RP(IUNIT(48),KKPER,IGRID)
        !
        IF(IUNIT(61) .NE. Z) CALL FMP_RP(KKPER,IGRID) 
        !
        IF(IUNIT(40) .NE. Z) CALL GWF2DRT7RP(IUNIT(40),IGRID)             !DRT must be after FMP_RP
        IF(IUNIT(64) .NE. Z) CALL GWF2SWR7RP(IUNIT(64),KKPER,IGRID)       !SWR - JDH
        !
        IF(IUNIT(63) .NE. Z) CALL GWF2NWT1RP(IUNIT(63),KPER,Mxiter,IGRID)
        IF(IUNIT(13) .NE. Z) CALL PCG7RP(IUNIT(13),KPER,Mxiter,IGRID)
        IF(IUNIT(70) .NE. Z) CALL PCGN2RP(IUNIT(70),KPER,IGRID,IOUT,Mxiter)
        !
        !IF(HAS_SLANG) CALL SLANG_RP(KPER)  !Determine what scripts are run
        !
        !IF(IUNIT(45) .NE. Z) CALL SWO % SETUP_NEXT_STRESS_PERIOD(KPER)
        !
      END DO GRID_RP  ! ------------------------------------------------------------------------------------------------------------------------------
      !
      !7C-----SIMULATE EACH TIME STEP.
      INTER = ''
      TIME_STEP: DO KSTP = 1, NSTP(KPER) !############################################################################################################
          !
          KKSTP = KSTP
          !
          GRID_AD: DO IGRID = 1, NGRIDS   ! ----------------------------------------------------------------------------------------------------------
              !
              !7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
              !
              CALL SGWF2BAS7PNT(IGRID)                       !seb lgr
              !
              IF(IUNIT(62) .NE. Z ) CALL GWF2UPWUPDATE(1,Igrid)
              !
              CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
              !
              IF(IUNIT(54).NE.0) CALL GWF2SUB7AD(KKPER,KKSTP,IUNIT(54),IGRID)
              !
              ! Update Drain Elevations if SUB-LINK active
              IF( SUBLNK )THEN 
                  IF(IUNIT(3)  .NE. Z) CALL GWF2DRN7AD(KKSTP,IGRID)
                  IF(IUNIT(40) .NE. Z) CALL GWF2DRT7AD(KKSTP,IGRID)
              ENDIF
              !
              IF(IUNIT(62) .NE. Z) CALL GWF2UPW1AD(IGRID)
              IF(IUNIT(20) .NE. Z) CALL GWF2CHD7AD(KKPER,IGRID)
              IF(IUNIT(1)  .NE. Z) CALL GWF2BCF7AD(KKPER,IGRID)
              IF(IUNIT(17) .NE. Z) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
              IF(IUNIT(23) .NE. Z) CALL GWF2LPF7AD(KKPER,IGRID)
              IF(IUNIT(37) .NE. Z) CALL GWF2HUF7AD(KKPER,IGRID)
              IF(IUNIT(16) .NE. Z) CALL GWF2FHB7AD(IGRID)
              IF(IUNIT(22) .NE. Z) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),IUNIT(62),IUNIT(15),IGRID)
              IF(IUNIT(65) .NE. Z) CALL GWF2SWI2AD(KKSTP,KKPER,IGRID)  !SWI
              IF(IUNIT(44) .NE. Z .AND. SUBLNK )  CALL GWF2SFR7AD(KKSTP,IGRID) !ADDED FOR SUB-LINK BY WSCHMID 07/29/10 --> sfr only uses AD for Sublink
              !
              IF(IUNIT(7)  .NE. Z) CALL GWF2GHB7AD(KKSTP,IGRID)                !seb added GHB and WEL AD
              IF(IUNIT(66) .NE. Z) CALL GWF2WEL7AD(KKSTP,IGRID)
              IF(IUNIT(2)  .NE. Z) CALL GWF2WEL8AD(KKSTP,IGRID)
              IF(IUNIT(44) .NE. Z) CALL GWF2SFR7LAKE(IUNIT(22),KPER,KSTP,IGRID)
              !
              !--BARC**
              IF(IUNIT(58) .NE. Z) CALL GWF2CFP1AD(KKPER,KKSTP)               !TR: 2017 07 20 CFPv2
              !
              !--BARC** DISTRIBUTE RECHARGE IN CONDUIT AND MATRIX SYSTEM      !TR: 2017 07 20 CFPv2          
              IF (IUNIT(58) .NE. Z) CALL CFP1DCRCH(KKPER, KKSTP,IUNIT(8))     !TR: 2017 07 20 CFPv2          
              !
              IF(IUNIT(50) .NE. Z) THEN
                  !
                  CALL MNW2_GET_COND(KPER,IGRID,IUNIT(50),IUNIT(1),IUNIT(23),IUNIT(62),IUNIT(37))
                  !
                  CALL GWF2MNW27AD(KKSTP,KKPER,IGRID)
              END IF
              !
              IF(IUNIT(52) .NE. Z) CALL GWF2MNW17AD(IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
              IF(IUNIT(48) .NE. Z) CALL GWF2BFH2AD(IUNIT(48),IGRID)
              !
              IF(IUNIT(61) .NE. Z) THEN 
                  !
                  CALL FMP_AD(KKPER, KSTP, IGRID)
                  !
                  IF(SUBLNK) CALL FMP_SUBLINK(IGRID)
              ENDIF
              !
              IF(IUNIT(6)  .NE. Z .AND. SUBLNK ) CALL GWF2RIPAD(KKPER,IGRID) !RIP-SUBLINK Update - rth
              !
              IF(IUNIT(64) .NE. Z) CALL GWF2SWR7AD(KKPER,KSTP,IGRID,IUNIT(54))  !SWR - JDH
              !
              !IF(HAS_SLANG) THEN
              !              IF(KPER == ONE .AND. KSTP == ONE) CALL SLANG_SIM_EVAL(KPER, KSTP, IOUT)
              !              !
              !              CALL SLANG_SP_TS_EVAL(KPER, KSTP, IOUT)
              !END IF
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
          24  FORMAT(' Solving:  Stress Period: ',i6,4x,'Time step: ',i6,4x,'Groundwater-Flow Eqn.',14x A)
          25  FORMAT(' Solving:  Stress Period: ',i6,4x,'Time step: ',i6,4x,'Groundwater-Flow Eqn.')
          26  FORMAT('Skipping:  Stress Period: ',i6,4x,'Time step: ',i6)
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
                                 IF( .NOT.( KPER.EQ.1 .AND. KSTP.EQ.1 ) )  THEN ! BY PASS FM LOOPS AND GRID LOOPS
                                     !
                                     LGRCNVG = 1  !By Pass GRID_CNVG Loop
                                     LGRITER = 1
                                 END IF
                                 !
                                 ICNVG = ONE
                                 KITER = ONE
                                 IF(KSTP.EQ.1) INTER = INTER//' skipped  '
                                 !
          ELSEIF ( FASTFORWARD ) THEN
                                 DO IGRID = 1, NGRIDS
                                               CALL SGWF2BAS7PNT(IGRID)
                                               CALL GWF2BAS7OC(KSTP, KPER, 1, IUNIT(12), IGRID)   !FILLS DUMMY "ICNVG" TO "1"
                                 END DO
                                 !
                                 IF(KSTP.EQ.1) INTER = INTER//' skipped  '
                                 IF( .NOT.( KPER.EQ.1 .AND. KSTP.EQ.1 ) ) CYCLE TIME_STEP  !SKIP TIME STEPS BEFORE SPSTART AND AFTER IT
          ELSE
               ISTP = ISTP + 1
          END IF
          !
          !
          GRID_CNVG: DO WHILE (LGRCNVG .EQ. 0)
             !
             LGRITER = LGRITER + ONE
             !
             !7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS FOR EACH GRID
             !
             GRID_FM: DO IGRID = 1, NGRIDS ! ---------------------------------------------------------------------------------------------------------
                !
                CALL SGWF2BAS7PNT(IGRID)
                !-------------CHECK IF LGR IS ACTIVE
                IF(ILGR .NE. 0)THEN
                    !
                    CALL SGWF2LGR2PNT(IGRID)
                    !
                    !---------------CHECK IF PARENT OR CHILD GRID 
                    !
                    IF(ISCHILD .NE. -1)THEN
                        !
                        !-----------------INTERPOLATE PARENT HEAD TO GHOST NODES AND RELAX
                        !swm: NOTE: the '1' in the arguement list is hardwired for the parent grid
                        !
                        IF (SPSTART == KKPER .AND. KKSTP == 1) THEN
                            CALL GWF2LGR2DARCY(1,1,LGRITER,1,IGRID)           ! FAKE FIRST STRESS PERIOD IF USING FASTFORWARD
                        ELSE
                            CALL GWF2LGR2DARCY(KKPER,KKSTP,LGRITER,1,IGRID)
                        END IF
                    ENDIF          
                ENDIF
                !
                !7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
                KITER = 0
                ITREAL2 = 0
                !ccrth                NOITER = 1
                IF ( IUNIT(63).NE.0 ) ITREAL = 0
                !ccrth                IF ( IRESTART.GT.0 ) THEN
                !ccrth                  NOITER = 0
                !ccrth                  IF ( KPER.GT.KPERSTART ) THEN
                !ccrth                    NOITER = 1
                !ccrth                  ELSE IF ( KPER.EQ.KPERSTART .AND. KSTP.GE.KSTPSTART ) THEN
                !ccrth                    NOITER = 1 
                !ccrth                  END IF
                !ccrth                END IF
                !ccrth                DO WHILE (ITREAL2.LT.MXITER .AND. NOITER.EQ.1)
                FM_LOOP: DO WHILE (ITREAL2.LT.MXITER) !***********************************************************************************************
                   !
                   KITER = KITER + 1
                   KKITER = KITER
                   !
                   !IF(HAS_SLANG_ITER) CALL SLANG_IT_EVAL(KPER, KSTP, KITER, IOUT)
                   !
                   IF ( IUNIT(63).EQ.0 ) ITREAL2 = KITER
                   IF(IUNIT(62).NE.0) CALL GWF2UPWUPDATE(2,Igrid)
                   ! 
                   !7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
                   !
                   CALL GWF2BAS7FM(IGRID)
                   !
                   IF(IUNIT(1)  .NE. Z) CALL GWF2BCF7FM(KKITER,KKSTP,KKPER,ILGR,IGRID)                       !SCOTT ORIGINALLY KKPER,ILGR,IGRID) BUT SUBROUTINE IS NOT DESIGNED TO RECIEVE ILGR, THIS MAY NEED TO BE INCORPORATED !swm: I added back in and modified BCF
                   IF(IUNIT(62) .NE. Z) CALL GWF2UPWFMS(KKITER,KKSTP,KKPER,IGRID)
                   IF(IUNIT(23) .NE. Z) CALL GWF2LPF7FM(KKITER,KKSTP,KKPER,ILGR,IGRID)
                   IF(IUNIT(37) .NE. Z) CALL GWF2HUF7FM(KKITER,KKSTP,KKPER,IUNIT(47),ILGR,IGRID)
                   !
                   !BARC**MODIFY CONDUCTANCES HERE WHEN MODE 2 IS ACTIVE
                   IF(IUNIT(58).NE.0) CALL GWF2CFPM2FM(KKITER,KKSTP,KKPER,ICNVG)                                                !TR: 2017 07 20 CFPv2 TODO CHECK FOR MODE2
                   !
                   IF ( IUNIT(62).EQ.0 ) THEN
                     IF(IUNIT(21).NE.0) CALL GWF2HFB7FM(IGRID)
                   END IF
                   !
                   IF(IUNIT(66) .NE. Z) CALL GWF2WEL7FM(IUNIT(63),IGRID)
                   IF(IUNIT(2)  .NE. Z) CALL GWF2WEL8FM(IGRID)
                   IF(IUNIT(3)  .NE. Z) CALL GWF2DRN7FM(IGRID)
                   IF(IUNIT(4)  .NE. Z) CALL GWF2RIV7FM(IGRID)
                   !
                   IF(IUNIT(5)  .NE. Z) THEN
                       IF(IUNIT(22) .NE. Z .AND. EVT_NEVTOP .EQ. 3) CALL GWF2LAK7ST(0,IGRID)
                       !
                       CALL GWF2EVT7FM(IGRID)
                       !
                       IF(IUNIT(22) .NE. Z .AND. EVT_NEVTOP .EQ. 3) CALL GWF2LAK7ST(1,IGRID)
                   END IF
                   !
                   IF(IUNIT(6) .NE. Z) CALL GWF2RIP4FM(IGRID)
                   IF(IUNIT(7) .NE. Z) CALL GWF2GHB7FM(IGRID)
                   !
                   IF(IUNIT(8) .NE. Z) THEN
                      IF(IUNIT(22) .NE. Z .AND. RCH_NRCHOP .EQ. 3) CALL GWF2LAK7ST(0,IGRID)
                      !
                      CALL GWF2RCH7FM(IGRID)
                      !
                      IF(IUNIT(22) .NE. Z .AND. RCH_NRCHOP .EQ. 3) CALL GWF2LAK7ST(1,IGRID)
                   END IF
                   !
                   IF(IUNIT(16) .NE. Z) CALL GWF2FHB7FM(IGRID)
                   IF(IUNIT(17) .NE. Z) CALL GWF2RES7FM(IGRID)
                   IF(IUNIT(18) .NE. Z) CALL GWF2STR7FM(IGRID)
                   IF(IUNIT(19) .NE. Z) CALL GWF2IBS7FM(KKPER,IGRID)
                   IF(IUNIT(39) .NE. Z) CALL GWF2ETS7FM(IGRID)
                   IF(IUNIT(40) .NE. Z) CALL GWF2DRT7FM(IGRID)                   !DRT MUST OCCUR BEFORE FMP seb
                   !
                   IF(IUNIT(61) .NE. Z) CALL FMP_FM(KKITER, KKPER, KKSTP, IGRID, NGRIDS, ILGR, LGRITER, SPTIM(KKPER)%DT(KKSTP))
                   !
                   IF(IUNIT(55) .NE. Z) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,            &
                                                        IUNIT(44),IUNIT(22),IUNIT(58), &
                                                        IUNIT(63),                     &
                                                        IUNIT(64),IGRID)               ! SWR - JDH ADDED IUNIT(64)
                   !
                   IF(IUNIT(44) .NE. Z) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,                      &
                                                        IUNIT(22),IUNIT(63),IUNIT(8),IUNIT(55),  &
                                                        ILGR,LGRITER,NGRIDS,IGRID)   !cjm (added IUNIT(8))
                   !
                   IF(IUNIT(22)  .NE. Z) CALL GWF2LAK7FM(KKITER,KKPER,KKSTP,IUNIT(44),IUNIT(55),IGRID)
                   !
                   IF(IUNIT(50) .NE. Z) THEN
                         !
                         CALL MNW2_GET_COND(KPER,IGRID,IUNIT(50),IUNIT(1),IUNIT(23),IUNIT(62),IUNIT(37))
                         !
                         CALL GWF2MNW27FM(KKITER,kkstp,kkper,IGRID)
                   END IF
                   !
                   IF(IUNIT(52) .NE. Z) CALL GWF2MNW17FM(KKITER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
                   IF(IUNIT(54) .NE. Z) CALL GWF2SUB7FM(KKPER,KKITER,IUNIT(9),IGRID)
                   IF(IUNIT(57) .NE. Z) CALL GWF2SWT7FM(KKPER,IGRID)
                   IF(IUNIT(64) .NE. Z) CALL GWF2SWR7FM(KKITER,KKPER,KKSTP,IGRID)  !SWR - JDH
                   IF(IUNIT(65) .NE. Z) CALL GWF2SWI2FM(KKSTP,KKPER,KKITER,IGRID)  !SWI - JDH
                   !
                   IF(IUNIT(48) .NE. Z) CALL GWF2BFH2FM(KKPER,KKSTP,KKITER,IGRID) 
                   !
                   !--BARC**SOLVE MODE 1 PIPE FLOW EQUATIONS
                   IF(IUNIT(58) .NE. Z) CALL GWF2CFP1M1FM(KKITER,KKPER,KKSTP,1)  !TR: 2017 07 20 CFPv2
                   !            
                   !-----------------ADJUST HCOF AND RHS IF LGR IS ACTIVE
                   !
                   IF(ILGR .NE. 0)THEN
                        IF(IGRID .EQ. 1)THEN  
                             DO LG =2,NGRIDS
                               IF(LGRDAT(IGRID)%IBPFLG(LG) .NE. 0) CALL GWF2LGR2PFM(KKPER,KKSTP,KKITER,LGRITER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),LG) 
                             ENDDO
                        ELSEIF(ISCHILD .GE. 0)THEN    
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
                   IERR=0
                   IF (IUNIT(9) .NE. Z) THEN
                          CALL SIP7PNT(IGRID)
                          CALL SIP7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,  &
                            V,W,HDCG,LRCH,NPARM,KKITER,HCLOSE,ACCL,ICNVG,      &
                            KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP(KKPER),      &
                            NCOL,NROW,NLAY,NODES,IOUT,0,IERR)
                   END IF
                   !
                   IF (IUNIT(10) .NE. Z) THEN
                          CALL DE47PNT(IGRID)
                          CALL DE47AP(HNEW,IBOUND,AU,AL,IUPPNT,IEQPNT,D4B,MXUP, &
                            MXLOW,MXEQ,MXBW,CR,CC,CV,HCOF,RHS,ACCLDE4,KITER,    &
                            ITMX,MXITER,NITERDE4,HCLOSEDE4,IPRD4,ICNVG,NCOL,    &
                            NROW,NLAY,IOUT,LRCHDE4,HDCGDE4,IFREQ,KKSTP,KKPER,   &
                            DELT,NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,               &
                            DELTL,NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW,IERR)
                   END IF
                   !
                   IF (IUNIT(13) .NE. Z) THEN
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
                   IF (IUNIT(42) .NE. Z) THEN
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
                   IF (IUNIT(70) .NE. Z) THEN
                                       CALL PCGN2AP(HNEW,RHS,CR,CC,CV,HCOF,IBOUND,KKITER,KKSTP,KKPER,ICNVG,HNOFLO,IGRID)
                   ENDIF
                   !
                   IF(IUNIT(63) .NE. Z) THEN
                                      CALL GWF2NWT1FM(KKITER,ICNVG,KSTP,KPER,Mxiter,IUNIT(22),IGRID)
                                      ITREAL2 = ITREAL
                   END IF
                   !
                   IF(IERR.EQ.1) CALL USTOP(' ')
                   !
                   !-WSCHMID-INSERT (06/18/2009)
                   !-------ALLOW CONVERGENCE CRITERIA FOR MNW WELL PUMPAGE LINKED TO FARM PROCESS
                   !       (will be active in next version once net MNW rates are computed at the end of FMP-routine)
                   !             IF (IUNIT(62) .NE. Z.AND.
                   !     1        IUNIT(52) .NE. Z.AND.ICNVG.EQ.1) THEN !.AND.MCLOSE.EQ.1)THEN
                   !              CALL FMP_LGR_PNT(IGRID) 
                   !              CALL FMP3QCNVG(IUNIT(52),IUNIT(13),IUNIT(9),              !FMP3QCNVG CALL ADDED BY SCHMID / WSCHMID-03/23/11 - MNW1 changed unit#
                   !     1                       IUNIT(10),IUNIT(42),IUNIT(63),IUNIT(70))
                   !             ENDIF
                   !-WSCHMID-END OF INSERT
                   !
                   !-WSCHMID-MODIFIED (07/21/08)
                   !-------ENSURE CONVERGENCE OF SWR - BASEFLOW CHANGES LESS THAN TOLF - JDH
                   IF(IUNIT(64) .NE. Z) THEN
                     CALL GWF2SWR7CV(KKITER,IGRID,ICNVG,MXITER)
                   END IF
                   !
                   IF(IUNIT(61) .NE. Z) CALL FMP_CNVG(IGRID, KPER, KSTP, KKITER, ICNVG)
                   !
                   CALL BAS_POST_SOLVER(KPER, KSTP, KITER, MXITER, ICNVG)  !APPLY ADDITIONAL DAMPENING, CONVERGENCE CHECK AND UPDATE UPLAY AND WTABLE
                   !
                   !IF(HAS_SLANG_ITER_END) CALL SLANG_IT_END_EVAL(KPER, KSTP, KITER, IOUT)
                   !
                   !C-WSCHMID-END OF MODIFICATION
                   !
                   !7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.      
                   !
                   IF(IUNIT(58) .NE. Z .AND. ICNVG.EQ.1) THEN                  !TR: 2017 07 20 CFOPv2
                       IF (CFPMODE.EQ.2 .AND. CFP_ICFPCNVG.NE.1) ICNVG = Z     !TR: 2017 07 20 CFOPv2 !TR: 2011 10 14 JUST DO THIS FOR CFPM2
                   ENDIF
                   !
                   IF (ICNVG.EQ.1) EXIT FM_LOOP
                   !
                END DO FM_LOOP  !*********************************************************************************************************************
                !
                IF (KITER > MXITER) KITER = MXITER
                !
                !   33     CONTINUE
                !crth          IF ( NOITER.EQ.1 ) THEN
                !  
                !--BARC**CALL CONDUIT ONCE MORE
                !  TO ADJUST THE CONDUIT HEADS TO THE HEADS IN THE FISSURED SYSTEM
                !  AFTER THE LAST MODFLOW-ITERATION
                !
                IF(IUNIT(58) .NE. Z) CALL GWF2CFP1M1FM(KKITER,KKPER,KKSTP,2)    !TR: 2017 09 13 FINAL CFP call
                !
                IF(IUNIT(62) .NE. Z ) CALL GWF2UPWUPDATE(2,Igrid)
                !
                !
                !-------------PREPARE THE NEXT GRID FOR LGR ITERATION
                IF(ILGR .NE. Z)THEN 
                   IF(ISCHILD .EQ. -1)THEN
                           DO LG =2,NGRIDS
                           
                             CALL GWF2LGR2INITP(KKPER,KKSTP,LGRITER,                    &
                                                LGRDAT(LG)%NPCBEG,LGRDAT(LG)%NPRBEG,    &
                                                LGRDAT(LG)%NPLBEG,LGRDAT(LG)%NPCEND,    &
                                                LGRDAT(LG)%NPREND,LGRDAT(LG)%NPLEND,    &
                                                LGRDAT(LG)%ISHFLG,LGRDAT(LG)%MXLGRITER, &
                                                IUNIT(5),IUNIT(8),IUNIT(17),LG,IGRID) 
                           ENDDO
                           !
                   ELSEIF(IGRID.NE.1)THEN
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
             IF(ILGR .EQ. 0)THEN
                            LGRCNVG = 1
             ELSE
                            CALL GWF2LGR2CNVG(IGRID,NGRIDS,LGRCNVG,LGRITER,KKPER,KKSTP)
             ENDIF
             !
          END DO GRID_CNVG
          !
          !
          !7C3----DETERMINE WHICH OUTPUT IS NEEDED FOR EACH GRID
          !
          GRID_OC: DO IGRID = 1, NGRIDS
              !
              CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,GLOBALDAT(IGRID)%IUNIT(12),IGRID)
              !
              ! SWAP POINTERS FOR LGR DATA   !swm: needed for SFR
              IF(ILGR .NE. Z) CALL SGWF2LGR2PNT(IGRID)
              !
              !7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
              !
              MSUM = 1  !Budget Counter
              !
              IF (IUNIT(1) .NE. Z) THEN
                  !
                  CALL GWF2BCF7BDS(KKSTP,KKPER,IGRID)
                  !
                  IF (IUNIT(58) .NE. Z) THEN
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
              IF(IUNIT(23) .NE. Z) THEN
                  !
                  CALL GWF2LPF7BDS(KKSTP,KKPER,IGRID)
                  !
                  IF (IUNIT(58) .NE. Z) THEN
                                        CALL CFPLPF7BDCH(KKSTP,KKPER,IGRID)     !CFP SUBROUTINE THAT MODIFIES PIPE FLOWS TO CONSTANT HEAD BOUNDARY. NEED THIS IF STATEMENT SO FLOWS TO CONSTANT HEADS ARE NOT COMPUTED TWICE IN BUDGETS WHEN CFP IS ACTIVE.
                  ELSE
                                        CALL GWF2LPF7BDCH(KKSTP,KKPER,IGRID)
                  END IF
                  !
                  IBDRET=0
                  IC1=1
                  IC2=NCOL
                  IR1=1
                  IR2=NROW
                  IL1=1
                  IL2=NLAY                     !IDIR
                  CALL GWF2LPF7BDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2LPF7BDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2LPF7BDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
              ENDIF
              !
              IF(IUNIT(62) .NE. Z) THEN
                  !
                  CALL GWF2UPWBDS(KKSTP,KKPER,IGRID)
                  CALL GWF2UPWBDCH(KKSTP,KKPER,IGRID)    !TR: 2017 07 20 CFPv2 TODO ADD UPW SUPPORT / MODIFY UPWBDCH ACCORDINGLY
                  IBDRET=0
                  IC1=1
                  IC2=NCOL
                  IR1=1
                  IR2=NROW
                  IL1=1
                  IL2=NLAY                     !IDIR
                  CALL GWF2UPWBDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2UPWBDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
                  CALL GWF2UPWBDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
              ENDIF
              !
              IF(IUNIT(37) .NE. Z) THEN
                  !
                  CALL GWF2HUF7BDS(KKSTP,KKPER,IGRID)
                  !
                  IF (IUNIT(58) .NE. Z) THEN
                                        CALL CFPHUF7BDCH(KKSTP,KKPER,IUNIT(47),IGRID)     !CFP SUBROUTINE THAT MODIFIES PIPE FLOWS TO CONSTANT HEAD BOUNDARY. NEED THIS IF STATEMENT SO FLOWS TO CONSTANT HEADS ARE NOT COMPUTED TWICE IN BUDGETS WHEN CFP IS ACTIVE.
                  ELSE
                                        CALL GWF2HUF7BDCH(KKSTP,KKPER,IUNIT(47), IGRID)
                  END IF
                  !
                  IBDRET=0
                  IC1=1
                  IC2=NCOL
                  IR1=1
                  IR2=NROW
                  IL1=1
                  IL2=NLAY                  !IDIR
                  CALL GWF2HUF7BDADJ(KKSTP,KKPER, 1,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
                  CALL GWF2HUF7BDADJ(KKSTP,KKPER, 2,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
                  CALL GWF2HUF7BDADJ(KKSTP,KKPER, 3,IBDRET,IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
              ENDIF
              !
              IF(IUNIT(66) .NE. Z) CALL GWF2WEL7BD(KKSTP,KKPER,IUNIT(63),IGRID)
              IF(IUNIT(2)  .NE. Z) CALL GWF2WEL8BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(3)  .NE. Z) CALL GWF2DRN7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(4)  .NE. Z) CALL GWF2RIV7BD(KKSTP,KKPER,IGRID)
              !
              IF(IUNIT(5) .NE. Z) THEN
                  !
                  IF(IUNIT(22) .NE. Z.AND.EVT_NEVTOP.EQ.3) CALL GWF2LAK7ST(0,IGRID)
                  !
                  CALL GWF2EVT7BD(KKSTP,KKPER,IGRID)
                  !
                  IF(IUNIT(22) .NE. Z.AND.EVT_NEVTOP.EQ.3) CALL GWF2LAK7ST(1,IGRID)
              END IF
              !
              IF(IUNIT(6) .NE. Z) CALL GWF2RIP4BD(KKSTP,KKPER,IGRID)        !inserted by schmid
              IF(IUNIT(7) .NE. Z) CALL GWF2GHB7BD(KKSTP,KKPER,IGRID)
              !
              IF(IUNIT(8) .NE. Z) THEN
                  !
                  IF(IUNIT(22) .NE. Z.AND.RCH_NRCHOP.EQ.3) CALL GWF2LAK7ST(0,IGRID)
                  !
                  CALL GWF2RCH7BD(KKSTP,KKPER,IGRID) 
                  !
                  IF(IUNIT(22) .NE. Z.AND.RCH_NRCHOP.EQ.3) CALL GWF2LAK7ST(1,IGRID)
              END IF
              !
              IF(IUNIT(16) .NE. Z) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(17) .NE. Z) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(18) .NE. Z) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(19) .NE. Z) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(39) .NE. Z) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(40) .NE. Z) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
              !
              ! (CJM) Added RCH unit number for RCH->SFR.
              !
              IF(IUNIT(44) .NE. Z) CALL GWF2SFR7BD(KKSTP,KKPER,IUNIT(15),                       &
                                                   IUNIT(22),IUNIT(46),IUNIT(55),NSOL,IUNIT(8), &
                                                   ILGR,NGRIDS,IGRID)
              !
              ! Moved call to UZF1BD to follow SFR7BD for printing net recharge in UZF.
              !
              IF(IUNIT(55) .NE. Z) CALL GWF2UZF1BD(KKSTP,KKPER,IUNIT(22),IUNIT(44),IGRID)
              IF(IUNIT(22) .NE. Z) CALL GWF2LAK7BD(KKSTP,KKPER,IUNIT(15),IUNIT(46),IUNIT(44),IUNIT(55),NSOL,IGRID)
              IF(IUNIT(50) .NE. Z) CALL GWF2MNW27BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(52) .NE. Z) CALL GWF2MNW17BD(NSTP(KPER),KKSTP,KKPER,IGRID)
              IF(IUNIT(54) .NE. Z) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(57) .NE. Z) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
              IF(IUNIT(64) .NE. Z) CALL GWF2SWR7BD(KKSTP,KKPER,IGRID)  !SWR - JDH
              !
              ! FARM DEMAND AND SUPPLY, FARM WELLS, AND FARM NET-RECHARGE
              !    FMP_FM is inserted to allow recalculating FMP-flowrates, which 
              !        may de a function of SFR & MNW flowrates: Q-fmp(h,Q-sfr,Q-mnw).
              !
              IF (IUNIT(61) .NE. Z)  CALL FMP_BD(KITER,KPER,KSTP,IGRID,NGRIDS,ILGR,LGRITER)
              !
              IF(IUNIT(48) .NE. Z) CALL GWF2BFH2BD(KKSTP,KKPER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
              !
              IF(ILGR .NE. Z)THEN 
                   IF(ISCHILD .LE. 0) CALL GWF2LGR2PBD(KKSTP,KKPER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),NGRIDS)
                   IF(ISCHILD .GT. 0) CALL GWF2LGR2CBD(KKSTP,KKPER,IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62))
              ENDIF  
              !
              IF(IUNIT(65) .NE. Z) CALL GWF2SWI2BD(KKSTP,KKPER,IGRID)  !SWI - JDH
              !BARC**ADD THESE
              !BIRK----EXCHANGE BUDGET OF CONDUIT AND FISSURED SYSTEM
              !
              IF(IUNIT(58) .NE. Z) CALL GWF2CFP1BD(KKPER,KKSTP,IUNIT(59),MSUM) !TR: 2017 07 20 CFPv2          
              !LMT
              !LMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
              !LMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
              !LMT
              !
              IF(IUNIT(49) .NE. Z) CALL LMT8BD(KKSTP,KKPER,IGRID)
              !
              !  Set NWT heads to Hdry when head is below bottom.
              !
              IF(IUNIT(63) .NE. Z) CALL GWF2NWT1BD(IGRID)
              !
              !  Observation simulated equivalents
              CALL OBS2BAS7SE(IUNIT(28),IGRID)
              !
              IF(IUNIT(33) .NE. Z) CALL OBS2DRN7SE(IGRID)
              IF(IUNIT(34) .NE. Z) CALL OBS2RIV7SE(IGRID)
              IF(IUNIT(35) .NE. Z) CALL OBS2GHB7SE(IGRID)
              IF(IUNIT(38) .NE. Z) CALL OBS2CHD7SE(KKPER,IGRID)
              IF(IUNIT(41) .NE. Z) CALL OBS2DRT7SE(IGRID)
              IF(IUNIT(43) .NE. Z) THEN
                                   CALL GWF2HYD7BAS7SE(1,IGRID)
                                   IF(IUNIT(19) .NE. Z) CALL GWF2HYD7IBS7SE(1,IGRID)
                                   IF(IUNIT(54) .NE. Z) CALL GWF2HYD7SUB7SE(1,IGRID)
                                   IF(IUNIT(18) .NE. Z) CALL GWF2HYD7STR7SE(1,IGRID)
                                   IF(IUNIT(44) .NE. Z) CALL GWF2HYD7SFR7SE(1,IGRID)
              END IF
              !             
              !7C5---PRINT AND/OR SAVE DATA.
              !
              IF(IUNIT(13) .NE. Z)                                         &
                CALL PCG7OT(HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,           &
                         HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,     &
                         MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,  &
                         NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF, &
                         HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY,       &
                         IHCOFADD,BPOLY)
              !
              CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID,BUDPERC,KITER,MXITER)
              !
              IF(IUNIT(19) .NE. Z) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),IGRID)
              !
              IF(IUNIT(37) .NE. Z)THEN
                            IF(IOHUFHDS  .NE. Z .OR.IOHUFFLWS  .NE. Z) CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
              ENDIF 
              !
              IF(IUNIT(51) .NE. Z) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,KKPER,IGRID)
              IF(IUNIT(54) .NE. Z) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),IGRID)
              IF(IUNIT(57) .NE. Z) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
              IF(IUNIT(43) .NE. Z) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
              !
              IF(ILGR  .NE. Z) THEN
                               IF(ISCHILD.GE.0) CALL GWF2LGR2COT(KKSTP,KKPER,IGRID)
              ENDIF 
              !
              !------CHECK FOR CHANGES IN HEAD AND FLUX BOUNDARY CONDITIONS 
              !
              IF(IUNIT(48) .NE. Z) CALL GWF2BFH2OT(KKSTP,KKPER,IGRID)
              !
              !7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED. 
              !
              INTER = INTER//NUM2STR(KITER)//', '
              IF(KKSTP < NSTP(KPER)) THEN
                                     IF(MOD(KKSTP,4)==0)INTER = INTER//NL//REPEAT(BLNK,79)
              END IF
              !
              IF ( ICNVG.EQ.0 ) THEN  !seb moved outside of IGRID LOOP
                  !
                  NCVGERR=NCVGERR+1
                  !
                  IF(ABS(BUDPERC).GT.STOPER) CALL STOP_ERROR(MSG=NL//'Failure to meet solver convergence criteria'//BLN//'Budget percent discrepancy is '//NUM2STR(BUDPERC)//' %'//BLN//'The maximum allowed budget error is '//NUM2STR(STOPER)//' %'//BLN//'To disable this stop/check use the BAS option "NO_FAILED_CONVERGENCE_STOP"'//BLN//'Or you can change the maximum allowed budget error is with the BAS option "STOPERROR" followed by the new max allowed error in percent.')
                  !
              END IF
              !
              CALL LISTSPLIT%SIZE_CHECK()            !CHECK IF LIST FILE SHOULD BE SPLIT INTO A NEW FILE DUE TO ITS SIZE  --POINTER ALREADY SET UP BY GWF2BAS7OT
              !
              IF(IOUT.NE.LISTSPLIT%IU) IOUT=LISTSPLIT%IU
              !
              !IF(HAS_SLANG) CALL SLANG_TS_END_EVAL(KPER, KSTP, KITER, IOUT)
              !
          ENDDO GRID_OC
          !
      END DO  TIME_STEP  !############################################################################################################
      !
      !IF(HAS_SLANG) CALL SLANG_SP_END_EVAL(KPER, KSTP, KITER, IOUT)
      !
      !Stop Stress Period Timer and print clock time
      !
      I = 0
      CALL SYSTEM_CLOCK(COUNT=FINISH)                    ! Stop timing 
      DO WHILE(FINISH.LE.0 .AND. I < 1000)
         CALL SYSTEM_CLOCK(COUNT=FINISH)
         I = I + 1
      END DO
      IF(FINISH.LE.0) CALL SYSTEM_CLOCK(COUNT=FINISH)    ! Stop timing 
      !
      CPU_TIME=REAL( (FINISH-START))/REAL(ClockRate*60)  ! in minutes
      !
      IF(CPU_TIME < 0..OR.FINISH.LE.0.OR.START.LE.0) THEN
          !
          WRITE(*,'(11x A,I6,4X,4A/)') 'Stress Period: ', KPER, ' CPU Time: ', '   ???', ' min      Solver Iter: ', INTER(:LEN(INTER)-2)
          !
      ELSEIF(CPU_TIME < 1E3) THEN
          !
          WRITE(*,'(11x A,I6,3X A,F7.3, 2A/)') 'Stress Period: ',KPER,' CPU Time: ', CPU_TIME,' min      Solver Iter: ',INTER(:LEN(INTER)-2)
      ELSE
          WRITE(*,'(11x A,I6,3X,4A/)') 'Stress Period: ', KPER, ' CPU Time: ', NUM2STR( CPU_TIME,7 ), ' min      Solver Iter: ', INTER(:LEN(INTER)-2)
      END IF
      !
  END DO STRESS_PERIOD  ! ============================================================================================================+++++++++++++++
  !
  !8------END OF SIMULATION
  !
  GRID_OT: DO IGRID = 1, NGRIDS
      !
      CALL SGWF2BAS7PNT(IGRID)
      IF(IUNIT(52).NE.0 .AND. ICNVG.NE.0) CALL GWF2MNW17OT(IGRID)
      !
      !-SAVE RESTART RECORDS FOR SUB PACKAGE
      IF(IUNIT(54) .NE. Z) CALL GWF2SUB7SV(IGRID)
      !
      ! Observation output
      IF(IUNIT(28) .NE. Z) CALL OBS2BAS7OT(IUNIT(28),IGRID)
      IF(IUNIT(33) .NE. Z) CALL OBS2DRN7OT(IGRID)
      IF(IUNIT(34) .NE. Z) CALL OBS2RIV7OT(IGRID)
      IF(IUNIT(35) .NE. Z) CALL OBS2GHB7OT(IGRID)
      IF(IUNIT(38) .NE. Z) CALL OBS2CHD7OT(IGRID)
      IF(IUNIT(41) .NE. Z) CALL OBS2DRT7OT(IGRID)
      !
      !-------OUTPUT RESULTS OF SWR TIMER
      !
      IF(IUNIT(64).NE.0) CALL GWF2SWR7OT(IGRID)
      !
      !IF(HAS_SLANG) CALL SLANG_SIM_END_EVAL(KPER, KSTP, IOUT)
      !
  ENDDO GRID_OT
  
  !
  !!!CALL GLO1BAS6ET(IOUT,IBDT,1)
  !
  GRID_DA: DO IGRID = 1, NGRIDS
      !
      !9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
      !9------LAST BECAUSE IT DEALLOCATES IUNIT.
      !
      CALL SGWF2BAS7PNT(IGRID)
      !
      IF(ILGR      .NE. Z) CALL GWF2LGR2DA(IGRID)
      IF(IUNIT(1)  .NE. Z) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(66) .NE. Z) CALL GWF2WEL7DA(IGRID)
      IF(IUNIT(2)  .NE. Z) CALL GWF2WEL8DA(IGRID)
      IF(IUNIT(3)  .NE. Z) CALL GWF2DRN7DA(IGRID)
      IF(IUNIT(4)  .NE. Z) CALL GWF2RIV7DA(IGRID)
      IF(IUNIT(5)  .NE. Z) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(6)  .NE. Z) CALL GWF2RIP4DA(IGRID)
      IF(IUNIT(7)  .NE. Z) CALL GWF2GHB7DA(IGRID)
      IF(IUNIT(8)  .NE. Z) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(9)  .NE. Z) CALL SIP7DA(IGRID)
      IF(IUNIT(10) .NE. Z) CALL DE47DA(IGRID)
      IF(IUNIT(13) .NE. Z) CALL PCG7DA(IGRID)
      !
      IF(IUNIT(63) .NE. Z) THEN
          !
          CALL SGWF2NWT1PNT(IGRID)  
          !
          IF    (LINMETH.EQ.1) THEN
                               CALL GMRES7DA(IGRID)
          ELSEIF(LINMETH.EQ.2) THEN
                               CALL XMD7DA(IGRID)
          END IF
          CALL GWF2NWT1DA(IGRID)
      END IF
      !
      IF(IUNIT(62) .NE. Z) CALL GWF2UPW1DA(IGRID)
      IF(IUNIT(16) .NE. Z) CALL GWF2FHB7DA(IGRID)
      IF(IUNIT(17) .NE. Z) CALL GWF2RES7DA(IGRID)
      IF(IUNIT(18) .NE. Z) CALL GWF2STR7DA(IGRID)
      IF(IUNIT(19) .NE. Z) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(20) .NE. Z) CALL GWF2CHD7DA(IGRID)
      IF(IUNIT(21) .NE. Z) CALL GWF2HFB7DA(IGRID)
      !
      IF(IUNIT(22) .NE. Z .OR. IUNIT(44) .NE. Z) CALL GWF2LAK7DA(IUNIT(22), IGRID)
      !
      IF(IUNIT(23) .NE. Z) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(37) .NE. Z) CALL GWF2HUF7DA(IGRID)
      IF(IUNIT(39) .NE. Z) CALL GWF2ETS7DA(IGRID)
      IF(IUNIT(40) .NE. Z) CALL GWF2DRT7DA(IGRID)
      IF(IUNIT(42) .NE. Z) CALL GMG7DA(IGRID)
      IF(IUNIT(70) .NE. Z) CALL PCGN2DA(IGRID)
      IF(IUNIT(44) .NE. Z) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46) .NE. Z) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50) .NE. Z) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(51) .NE. Z) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(52) .NE. Z) CALL GWF2MNW17DA(IGRID)
      IF(IUNIT(54) .NE. Z) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55) .NE. Z) CALL GWF2UZF1DA(IGRID)
      IF(IUNIT(57) .NE. Z) CALL GWF2SWT7DA(IGRID)
      IF(IUNIT(64) .NE. Z) CALL GWF2SWR7DA(IGRID)  !SWR - JDH
      IF(IUNIT(65) .NE. Z) CALL GWF2SWI2DA(IGRID)  !SW1 - JDH
      !
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
      !
      IF(IUNIT(33) .NE. Z) CALL OBS2DRN7DA(IGRID)
      IF(IUNIT(34) .NE. Z) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35) .NE. Z) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(38) .NE. Z) CALL OBS2CHD7DA(IGRID)
      IF(IUNIT(41) .NE. Z) CALL OBS2DRT7DA(IGRID)
      IF(IUNIT(43) .NE. Z) CALL GWF2HYD7DA(IGRID)
      IF(IUNIT(49) .NE. Z) CALL LMT8DA(IGRID)
      IF(IUNIT(61) .NE. Z) CALL FMP3DA(IGRID)
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
  WRITE(STDOUT,'(/ 2x 2A )') 'Simulation finished at   ', SIM_END%STR('  ')  
  WRITE(STDOUT,'(/ 2x 2A/)') 'with an elapsed time of: ', SIM_END%STR_ELAPSED(SIM_START)
  !
  WRITE(IOUT  ,'(/// 1x 2A/)') 'Simulation finished at  ', SIM_END%STR('  ')  
  WRITE(IOUT  ,'(/ 1x 2A/)') 'Which had an elapsed run time of: ', SIM_END%STR_ELAPSED(SIM_START)
  !WRITE(IOUT  ,'(/ 1x 2A/)') 'Elapsed run time: ', SIM_END%STR_ELAPSED(SIM_START)
  !
  I = GET_WARN()
  IF(I.NE.Z) THEN
      WRITE(I,'(//1x 2A///)') 'OneWater simulation completed with an elapsed run time of: ', SIM_END%STR_ELAPSED(SIM_START)
      WRITE(I,'(A//////)') REPEAT('#',104)
  END IF
  !
  WRITE(*,'(//19x A//)')   'OneWater Simulation Now Complete'
  WRITE(IOUT,'( /4x A//)') 'OneWater Simulation Now Complete'
  !
  !CALL PROGRAM_DONE(IOUT)
  !
  FLUSH(IOUT)
  !
  !9------DEALLOCATE BAS MEMORY (MADE LAST BECAUSE IT DEALLOCATES IOUT).
  !
  DO IGRID = 1, NGRIDS
                      CALL GWF2BAS7DA(IGRID)
  ENDDO
  !
  IF(USE_PAUSE) CALL PAUSE('BAS "PAUSE" OPTION ACTIVATED, SIMULATION IS NOW PAUSED.'//BLN//'       PRESS ENTER TO END SIMULATION')
  !
END PROGRAM MODFLOW_OWHM
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
!!!  IF(IPRTIM.GT.0) THEN
!!!      WRITE(IOUT,'(1X)')
!!!      WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
!!!  END IF
!!!  !
!!!  !     Calculate elapsed time in days and seconds
!!!  NDAYS=0
!!!  LEAP=0
!!!  IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
!!!  IBD = IBDT(3)            ! BEGIN DAY
!!!  IED = IEDT(3)            ! END DAY
!!!  ! FIND DAYS
!!!  IF (IBDT(2).NE.IEDT(2)) THEN
!!!    ! MONTHS DIFFER
!!!    MB = IBDT(2)             ! BEGIN MONTH
!!!    ME = IEDT(2)             ! END MONTH
!!!    NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
!!!    IF (MB.GT.ME) NM = NM+12
!!!    MC=MB-1
!!!    DO 10 M=1,NM
!!!      MC=MC+1                ! MC IS CURRENT MONTH
!!!      IF (MC.EQ.13) MC = 1
!!!      IF (MC.EQ.MB) THEN
!!!        NDAYS = NDAYS+IDPM(MC)-IBD
!!!        IF (MC.EQ.2) NDAYS = NDAYS + LEAP
!!!      ELSEIF (MC.EQ.ME) THEN
!!!        NDAYS = NDAYS+IED
!!!      ELSE
!!!        NDAYS = NDAYS+IDPM(MC)
!!!        IF (MC.EQ.2) NDAYS = NDAYS + LEAP
!!!      ENDIF
!!!    10 CONTINUE
!!!      ELSEIF (IBD.LT.IED) THEN
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
!!!        IF (NDAYS.GT.0) THEN
!!!          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
!!! 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2, ' Minutes, ',I2,' Seconds',/)
!!!        ELSEIF (NHOURS.GT.0) THEN
!!!          WRITE(*,1020) NHOURS,NMINS,NRSECS
!!! 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2, ' Minutes, ',I2,' Seconds',/)
!!!        ELSEIF (NMINS.GT.0) THEN
!!!          WRITE(*,1030) NMINS,NSECS,MSECS
!!! 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ', I2,'.',I3.3,' Seconds',/)
!!!        ELSE
!!!          WRITE(*,1040) NSECS,MSECS
!!! 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
!!!        ENDIF
!!!        !
!!!        !  Write times to file if requested
!!!      IF(IPRTIM.GT.0) THEN
!!!        IF (NDAYS.GT.0) THEN
!!!            WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
!!!        ELSEIF (NHOURS.GT.0) THEN
!!!            WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
!!!        ELSEIF (NMINS.GT.0) THEN
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
!!!      CHARACTER*24 ANAME(1)
!!!      DATA ANAME(1) /'            RESTART HEAD'/
!!!      !     SPECIFICATIONS:
!!!      !  ------------------------------------------------------------------      
!!!      !
!!!      !8G-----READ INITIAL HEADS FOR RESTART.
!!!      IF(IXSEC.EQ.0) THEN
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
!!!      IF(IBOUND(J,I,K).EQ.0) HNEW(J,I,K)=HNF
!!!  400 CONTINUE
!!!      RETURN 
!!!END SUBROUTINE
!
SUBROUTINE PRINT_MAIN_HEADER(FN)  !SET TO 6 FOR CMD PROMPT
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: stdout=>OUTPUT_UNIT
  USE CONSTANTS, ONLY: NL
  USE OWHM_HEADER_INTERFACE
  !  
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN)::FN
  !
  !1 ASSIGN VERSION NUMBER AND DATE
  !
  CHARACTER(:),ALLOCATABLE:: MFPACSUP,VERSION_OWHM
  CHARACTER(:),ALLOCATABLE:: VERSION_MF, VERSION_FMP
  CHARACTER(:),ALLOCATABLE:: VERSION_SWR,VERSION_LGR
  CHARACTER(:),ALLOCATABLE:: VERSION_SWI,VERSION_NWT
  CHARACTER(:),ALLOCATABLE:: VERSION_CFP                            !TR: 2017 09 13 CFP
  !
  !2 SET NAMES TO PRINT
  !
  !OWHM='ONE-WATER HYDROLOGIC-FLOW MODEL'                      ! TITLE
  !MFPACSUP='FMP4-LGR2-NWT1-SWR1-SWI2'  ! PROCESS SUPPORTED
  !'2.00.00.00.00     04/23/1979
  !
  VERSION_OWHM='2.0.0   04/06/2020'  !"Psyduck After Advil" !"2.0" ! 
  VERSION_MF  ='1.12.0  02/03/2017'        
  VERSION_FMP ='4.0.0   04/23/2017'       
  VERSION_SWR ='1.04.0  09/15/2016'       
  VERSION_SWI ='2.0.0   07/22/2013'     
  VERSION_LGR ='2.0.0   09/19/2013'      
  VERSION_NWT ='1.1.4   04/11/2018'     
  VERSION_CFP ='1.9.57  09/12/2017'
  !
  !3-WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
  !
  IF(FN.NE.stdout) WRITE(FN,'(/A//)') OWHM_HEADER()     !ONLY PRINT TO FILE AND SKIP CMD PROMPT
  !
  WRITE (FN,123) VERSION_OWHM, &
                  VERSION_MF,  &
                  VERSION_FMP, &
                  VERSION_SWR, &
                  VERSION_SWI, &
                  VERSION_NWT, &
                  VERSION_LGR, &
                  VERSION_CFP
  !
  123 FORMAT( //,24X,'MODFLOW' /,12X,                                         &
                   'ONE-WATER HYDROLOGIC-FLOW MODEL', //, 4X,                 &
                   'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE ',/ 11x  &
                   'CONJUNCTIVE USE SIMULATION PROGRAM',//,14X,               &
                   'Version  ',A /,/                                          &
                   ' INCLUDES:', / 10x                                        &
                   'MODFLOW-2005 Version ',A,        / 10x                    &
                   'MODFLOW-FMP  Version ',A,        / 10x                    &
                   'MODFLOW-SWR  Version ',A,        / 10x                    &
                   'MODFLOW-SWI  Version ',A,        / 10x                    &
                   'MODFLOW-LGR  Version ',A,        / 10x                    &
                   'MODFLOW-NWT  Version ',A,        / 10x                    &
                   'MODFLOW-CFP  Version ',A / / )                            !TR: 2017 09 13 CFP
     
  !WRITE(FN,'(A)') '¿¡¿¡ The Just For Jon Version ?!?!'
END SUBROUTINE