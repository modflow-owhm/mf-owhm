! BASIC PACKAGE MODULE IS DEPENDENT ON THE GLOBAL MODULE
MODULE GWFBASMODULE
  USE DATE_OPERATOR_INSTRUCTION
  USE FILE_INCREMENTER_INTERFACE,      ONLY: FILE_INCREMENTER
  USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
  USE GENERIC_INPUT_FILE_INSTRUCTION,  ONLY: GENERIC_INPUT_FILE
  PRIVATE:: DATE_OPERATOR, DATE_TS
  PRIVATE:: FILE_INCREMENTER
  PRIVATE:: GENERIC_OUTPUT_FILE, GENERIC_INPUT_FILE
  !
  INTEGER, SAVE, POINTER  ::MSUM,IUBGT
  INTEGER, SAVE, POINTER  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN
  INTEGER, SAVE, POINTER  ::LBHDSV,LBDDSV,LBBOSV
  INTEGER, SAVE, POINTER  ::IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT
  INTEGER, SAVE, POINTER  ::IPRTIM,IPEROC,ITSOC,ICHFLG
  INTEGER, SAVE, POINTER  ::IDDREF,IDDREFNEW
  REAL,    SAVE, POINTER  ::DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER
  !
  CHARACTER(LEN=20),SAVE, POINTER   ::CHEDFM,CDDNFM,CBOUFM
  INTEGER,          SAVE, DIMENSION(:,:),POINTER,CONTIGUOUS::IOFLG
  REAL,             SAVE, DIMENSION(:,:),POINTER,CONTIGUOUS::VBVL
  CHARACTER(LEN=16),SAVE, DIMENSION(:),  POINTER,CONTIGUOUS::VBNM
  !
  DOUBLE PRECISION,               POINTER,SAVE:: PVOL_ERR
  !
  INTEGER,                        POINTER,SAVE::PDIFFPRT      !seb FLAG FOR PRINTING WARNING WHEN BUDGET PERCENT ERROR>PDIFFPRT  
  DOUBLE PRECISION,               POINTER,SAVE::REALTIM
  DOUBLE PRECISION,               POINTER,SAVE::SIMTIM_PER
  DOUBLE PRECISION,               POINTER,SAVE::REALTIM_PER
  DOUBLE PRECISION,               POINTER,SAVE::TOTPERTIM
  DOUBLE PRECISION,               POINTER,SAVE::SIMTIME
  LOGICAL,                        POINTER,SAVE::USE_LEAP_YR
  DOUBLE PRECISION,               POINTER,SAVE:: MAX_REL_VOL_ERROR
  LOGICAL,                        POINTER,SAVE:: MAX_REL_VOL_INVOKED
  !
  INTEGER,                        POINTER,SAVE:: MIN_SOLVER_INTER
  TYPE(GENERIC_INPUT_FILE),       POINTER,SAVE:: MIN_ITER_INPUT      !IF SPECIFIED BY STRESS PERIOD
  INTEGER,                        POINTER,SAVE:: MIN_SOLVER_INTER_NEW
  INTEGER,                        POINTER,SAVE:: MIN_SOLVER_INTER_SP
  !  
  LOGICAL,                        POINTER,SAVE:: DEALLOCATE_MULT
  TYPE(FILE_INCREMENTER),         POINTER,SAVE:: LISTSPLIT
  TYPE(GENERIC_OUTPUT_FILE),      POINTER,SAVE:: BUDGETDB
  TYPE(GENERIC_OUTPUT_FILE),      POINTER,SAVE:: INTER_INFO
  !                                              
  TYPE(GENERIC_OUTPUT_FILE),                     POINTER, SAVE:: PRNT_CUM_HEAD_CHNG
  DOUBLE PRECISION,                              POINTER, SAVE:: CUM_HEAD_CHNG
  INTEGER,                                       POINTER, SAVE:: CUM_HEAD_CHNG_E10
  !
  TYPE(GENERIC_OUTPUT_FILE),                     POINTER, SAVE:: PRNT_RES
  REAL,                                          POINTER, SAVE:: PRNT_RES_LIM
  !                                              
  TYPE(GENERIC_OUTPUT_FILE),                     POINTER, SAVE:: PRNT_RES_CUM
  DOUBLE PRECISION, CONTIGUOUS, DIMENSION(:,:,:),POINTER, SAVE:: PRNT_RES_CUM_ARR
  !
  DOUBLE PRECISION,              POINTER, SAVE:: ABOVE_GSE_LIM
  DOUBLE PRECISION,              POINTER, SAVE:: ABOVE_GSE_PRT_LIM
  TYPE(GENERIC_OUTPUT_FILE),     POINTER, SAVE:: ABOVE_GSE_PRT
  !
  INTEGER,                        POINTER,SAVE:: OSCIL_DMP_OUTER
  INTEGER,                        POINTER,SAVE:: OSCIL_DMP_LRC
  DOUBLE PRECISION,               POINTER,SAVE:: OSCIL_DMP_DIF
  !
  INTEGER,                        POINTER,SAVE::PRNT_CNVG_OUTER
  INTEGER,                        POINTER,SAVE::PRNT_CNVG_NTERM
  TYPE(GENERIC_OUTPUT_FILE),      POINTER,SAVE::PRNT_CNVG
  INTEGER,DIMENSION(:),           POINTER,SAVE, CONTIGUOUS:: PRNT_CNVG_LRC
  DOUBLE PRECISION,DIMENSION(:),  POINTER,SAVE, CONTIGUOUS:: PRNT_CNVG_DIF
  !
  INTEGER,                        POINTER,SAVE::PRNT_FRES_OUTER
  INTEGER,                        POINTER,SAVE::PRNT_FRES_NTERM
  TYPE(GENERIC_OUTPUT_FILE),      POINTER,SAVE::PRNT_FRES
  INTEGER,DIMENSION(:),           POINTER,SAVE, CONTIGUOUS:: PRNT_FRES_LRC
  DOUBLE PRECISION,DIMENSION(:),  POINTER,SAVE, CONTIGUOUS:: PRNT_FRES_DIF
  !
  INTEGER,                        POINTER,SAVE::PRNT_VERR_OUTER
  INTEGER,                        POINTER,SAVE::PRNT_VERR_NTERM
  TYPE(GENERIC_OUTPUT_FILE),      POINTER,SAVE::PRNT_VERR
  INTEGER,DIMENSION(:),           POINTER,SAVE, CONTIGUOUS:: PRNT_VERR_LRC
  DOUBLE PRECISION,DIMENSION(:),  POINTER,SAVE, CONTIGUOUS:: PRNT_VERR_DIF
  !
  TYPE(GENERIC_OUTPUT_FILE),               POINTER, SAVE            :: SAVE_HEAD
  INTEGER,                                 POINTER, SAVE            :: SAVE_HEAD_FLAG  ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
  !
  TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), POINTER, SAVE, CONTIGUOUS:: PRINT_HEAD
  INTEGER,                                 POINTER, SAVE            :: PRINT_HEAD_FLAG  ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
  !
  TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), POINTER, SAVE, CONTIGUOUS:: PRINT_WTAB
  INTEGER,                                 POINTER, SAVE            :: PRINT_WTAB_FLAG  ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
  !
  TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), POINTER, SAVE, CONTIGUOUS:: PRINT_WDEP
  INTEGER,                                 POINTER, SAVE            :: PRINT_WDEP_FLAG  ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
  !
  LOGICAL,          POINTER, SAVE:: DAMPEN_START
  INTEGER,          POINTER, SAVE:: DAMPEN_START_ITR
  DOUBLE PRECISION, POINTER, SAVE:: DAMPEN_START_DMP
  !
  TYPE(GENERIC_INPUT_FILE),        POINTER,SAVE:: ADAMP_INPUT     !IF SPECIFIED BY STRESS PERIOD
  INTEGER,                         POINTER,SAVE:: BAS_ADAMP       !OUTER ITERATION START, IF ZERO, THEN NO ADAMP
  DOUBLE PRECISION,                POINTER,SAVE:: BAS_ADAMP_TOL   !ONLY APPLY IF HEAD DIFF IS GREATER THEN THIS
  DOUBLE PRECISION,                POINTER,SAVE:: BAS_ADAMP_TOL2  !DETERMINES IF HEAD IS LOCKED, AUTO SET DEPENDING ON LENI
  REAL,DIMENSION(:,:,:),CONTIGUOUS,POINTER,SAVE:: HED_CHNG2       !HOLDS PREVIOUS ITERATION HEAD CHANGES
  REAL,DIMENSION(:,:,:),CONTIGUOUS,POINTER,SAVE:: HED_CHNG3       !HOLDS PREVIOUS ITERATION HEAD CHANGES
  INTEGER,DIMENSION(:,:,:),CONTIGUOUS,POINTER,SAVE:: HED_LOCK    !SET >0 IF HEAD IS LOCKED
  !
  TYPE DATE_TS
     TYPE(DATE_OPERATOR),DIMENSION(:),ALLOCATABLE:: TS
  END TYPE
  TYPE(DATE_TS),DIMENSION(:),POINTER,CONTIGUOUS,SAVE:: DATE_SP
  LOGICAL,POINTER,SAVE:: HAS_STARTDATE
  !
  TYPE GWFBASTYPE
    INTEGER, POINTER  ::MSUM,IUBGT
    INTEGER, POINTER  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN
    INTEGER, POINTER  ::LBHDSV,LBDDSV,LBBOSV
    INTEGER, POINTER  ::IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT
    INTEGER, POINTER  ::IPRTIM,IPEROC,ITSOC,ICHFLG
    INTEGER, POINTER  ::IDDREF,IDDREFNEW
    REAL,    POINTER  ::DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER
    CHARACTER(LEN=20), POINTER   ::CHEDFM,CDDNFM,CBOUFM
    INTEGER,           DIMENSION(:,:), POINTER,CONTIGUOUS::IOFLG
    REAL,              DIMENSION(:,:), POINTER,CONTIGUOUS::VBVL
    CHARACTER(LEN=16), DIMENSION(:),   POINTER,CONTIGUOUS::VBNM
    !
    DOUBLE PRECISION,                  POINTER:: PVOL_ERR
    !
    INTEGER,                           POINTER::PDIFFPRT           !seb
    DOUBLE PRECISION,                  POINTER::REALTIM
    DOUBLE PRECISION,                  POINTER:: SIMTIM_PER
    DOUBLE PRECISION,                  POINTER:: REALTIM_PER
    DOUBLE PRECISION,                  POINTER:: TOTPERTIM
    DOUBLE PRECISION,                  POINTER:: SIMTIME
    LOGICAL,                           POINTER:: USE_LEAP_YR
    DOUBLE PRECISION,                  POINTER:: MAX_REL_VOL_ERROR
    LOGICAL,                           POINTER:: MAX_REL_VOL_INVOKED
    !
    INTEGER,                           POINTER:: MIN_SOLVER_INTER
    TYPE(GENERIC_INPUT_FILE),          POINTER:: MIN_ITER_INPUT      !IF SPECIFIED BY STRESS PERIOD
    INTEGER,                           POINTER:: MIN_SOLVER_INTER_NEW
    INTEGER,                           POINTER:: MIN_SOLVER_INTER_SP
    !
    LOGICAL,                           POINTER:: DEALLOCATE_MULT
    TYPE(FILE_INCREMENTER),            POINTER:: LISTSPLIT
    TYPE(GENERIC_OUTPUT_FILE),         POINTER:: BUDGETDB
    TYPE(GENERIC_OUTPUT_FILE),         POINTER:: INTER_INFO
    !                                              
    TYPE(GENERIC_OUTPUT_FILE),                     POINTER:: PRNT_CUM_HEAD_CHNG
    DOUBLE PRECISION,                              POINTER:: CUM_HEAD_CHNG
    INTEGER,                                       POINTER:: CUM_HEAD_CHNG_E10
    !
    TYPE(GENERIC_OUTPUT_FILE),                     POINTER:: PRNT_RES
    REAL,                                          POINTER:: PRNT_RES_LIM
    !                                              
    TYPE(GENERIC_OUTPUT_FILE),                     POINTER:: PRNT_RES_CUM
    DOUBLE PRECISION, CONTIGUOUS, DIMENSION(:,:,:),POINTER:: PRNT_RES_CUM_ARR
    !
    DOUBLE PRECISION,                  POINTER:: ABOVE_GSE_LIM
    DOUBLE PRECISION,                  POINTER:: ABOVE_GSE_PRT_LIM
  TYPE(GENERIC_OUTPUT_FILE),           POINTER:: ABOVE_GSE_PRT
    !
    INTEGER,                           POINTER:: OSCIL_DMP_OUTER
    INTEGER,                           POINTER:: OSCIL_DMP_LRC
    DOUBLE PRECISION,                  POINTER:: OSCIL_DMP_DIF
    !
    INTEGER,                        POINTER::PRNT_CNVG_OUTER
    INTEGER,                        POINTER::PRNT_CNVG_NTERM
    TYPE(GENERIC_OUTPUT_FILE),      POINTER::PRNT_CNVG
    INTEGER,DIMENSION(:),         POINTER,CONTIGUOUS:: PRNT_CNVG_LRC
    DOUBLE PRECISION,DIMENSION(:),POINTER,CONTIGUOUS:: PRNT_CNVG_DIF
    !
    INTEGER,                        POINTER::PRNT_FRES_OUTER
    INTEGER,                        POINTER::PRNT_FRES_NTERM
    TYPE(GENERIC_OUTPUT_FILE),      POINTER::PRNT_FRES
    INTEGER,DIMENSION(:),           POINTER, CONTIGUOUS:: PRNT_FRES_LRC
    DOUBLE PRECISION,DIMENSION(:),  POINTER, CONTIGUOUS:: PRNT_FRES_DIF
    !
    INTEGER,                        POINTER::PRNT_VERR_OUTER
    INTEGER,                        POINTER::PRNT_VERR_NTERM
    TYPE(GENERIC_OUTPUT_FILE),      POINTER::PRNT_VERR
    INTEGER,DIMENSION(:),           POINTER, CONTIGUOUS:: PRNT_VERR_LRC
    DOUBLE PRECISION,DIMENSION(:),  POINTER, CONTIGUOUS:: PRNT_VERR_DIF
    !
    TYPE(GENERIC_OUTPUT_FILE),               POINTER            :: SAVE_HEAD
    INTEGER,                                 POINTER            :: SAVE_HEAD_FLAG    ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
    !                                        
    TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), POINTER, CONTIGUOUS:: PRINT_HEAD
    INTEGER,                                 POINTER            :: PRINT_HEAD_FLAG    ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
    !
    TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), POINTER, CONTIGUOUS:: PRINT_WTAB
    INTEGER,                                 POINTER            :: PRINT_WTAB_FLAG  ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
    !
    TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), POINTER, CONTIGUOUS:: PRINT_WDEP
    INTEGER,                                 POINTER            :: PRINT_WDEP_FLAG  ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
    !
    LOGICAL,          POINTER:: DAMPEN_START
    INTEGER,          POINTER:: DAMPEN_START_ITR
    DOUBLE PRECISION, POINTER:: DAMPEN_START_DMP
    !
    TYPE(GENERIC_INPUT_FILE),           POINTER:: ADAMP_INPUT       !IF SPECIFIED BY STRESS PERIOD
    INTEGER,                            POINTER:: BAS_ADAMP         !OUTER ITERATION START, IF ZERO, THEN NO ADAMP
    DOUBLE PRECISION,                   POINTER:: BAS_ADAMP_TOL     !ONLY APPLY IF HEAD DIFF IS GREATER THEN THIS
    DOUBLE PRECISION,                   POINTER:: BAS_ADAMP_TOL2    !DETERMINES IF HEAD IS LOCKED, AUTO SET DEPENDING ON LENI
    REAL,DIMENSION(:,:,:),CONTIGUOUS,   POINTER:: HED_CHNG2         !HOLDS PREVIOUS ITERATION HEAD CHANGES
    REAL,DIMENSION(:,:,:),CONTIGUOUS,   POINTER:: HED_CHNG3         !HOLDS PREVIOUS ITERATION HEAD CHANGES
    INTEGER,DIMENSION(:,:,:),CONTIGUOUS,POINTER:: HED_LOCK          !SET >0 IF HEAD IS LOCKED
    !
    TYPE(DATE_TS),DIMENSION(:),POINTER,CONTIGUOUS:: DATE_SP
    LOGICAL,POINTER:: HAS_STARTDATE
  END TYPE
  !
  TYPE(GWFBASTYPE), SAVE  ::GWFBASDAT(10)
END MODULE GWFBASMODULE