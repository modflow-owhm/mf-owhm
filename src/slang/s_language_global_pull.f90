! TOTAL PUMPAGE TFDR ALLOTMENT
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! ASSUMES THAT LGR POINTERS HAVE ALREADY BEEN SET FOR THE CORRECT LOCAL VARIABLES
!
! MUST INCLUDE THE FOLLOWING INTERFACE WHENEVER USED:
!
!  INTERFACE
!    PURE SUBROUTINE VARIABLE_GET_GLOBAL_MODEL_PROPERTY(VAR, NAM, KPER, KSTP, KITER, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
!      USE S_LANGUAGE_INTERFACE, ONLY: S_VARIABLE_LIST, VARIABLE_NAME_MEANING
!      CLASS(S_VARIABLE_LIST),                                 INTENT(INOUT):: VAR
!      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
!      INTEGER,                                                INTENT(IN   ):: KPER, KSTP, KITER, LVL
!      CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
!    END SUBROUTINE
!  END INTERFACE
!
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%MAX_STORAGE
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC  
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ  
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ
    !SWODAT%PROJ(IPROJ)%RELEASE
    !
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC  
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ 
    !SWODAT%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD
MODULE IDENTIFY_MODEL_PROPERTY_LOCAL_INTERFACE
    USE CONSTANTS
    USE EQUATION_VARIABLE_LIST
    USE S_LANGUAGE_INTERFACE
    !
    USE GLOBAL
    USE GWFBASMODULE
    USE CALENDAR_FUNCTIONS,        ONLY: DYEAR_TO_DATE, DATE_TO_DYEAR, YEAR_FRACTION
    USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
    USE STRINGS,                   ONLY: JOIN_TXT
    USE NUM2STR_INTERFACE,         ONLY: NUM2STR
    USE BAS_UTIL,                  ONLY: DELT_TO_DAY
    USE FMP_GLOBAL  !SWODAT, FDIM, WBS
    USE SWO_DATA_FMP_MODULE,      ONLY: FRAC_POOL2SPLIT, ACAP_STOR2AREA, ACAP_STOR2ELEV
    USE GWFSFRMODULE,             ONLY: SEG, SEG_NSTRM, ISTRM, STRM
    !
END MODULE
!
PURE SUBROUTINE VARIABLE_GET_GLOBAL_MODEL_PROPERTY(VAR, NAM, KPER, KSTP, KITER, LVL, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
  USE IDENTIFY_MODEL_PROPERTY_LOCAL_INTERFACE
  IMPLICIT NONE
  !
  CLASS(S_VARIABLE_LIST),                                 INTENT(INOUT):: VAR
  TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
  INTEGER,                                                INTENT(IN   ):: KPER, KSTP, KITER, LVL  !LVL = 0 => SP/TS, LVL = 1 => FM, LVL = 2 => Post-Time Step and CLOSEOUT
  CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
  CHARACTER(:),ALLOCATABLE:: PROP
  DOUBLE PRECISION:: VAL, TMP, TMP2, TMP3, DT
  INTEGER:: I,J,K, ID,ID2,ID3, DUM,FLAG, POS
  LOGICAL:: LVAR
  !
  IF (.NOT. ALLOCATED(ERROR)) ALLOCATE(ERROR,SOURCE=NL)
  !
  DT = SPTIM(KPER)%DT(KSTP)
  !
  NAM_LOOP: DO I = ONE, SIZE(NAM)
      !
      CALL NAM(I)%GET(ONE,PROP,ID)
      !
      POS = NAM(I)%POS
      VAL = DZ
      !
      SELECT CASE(PROP)
      CASE("DATE")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       SELECT CASE(PROP)
                       CASE("START")
                           VAL = DATE_SP(KPER)%TS(KSTP-ONE)%DYEAR
                       CASE("END")
                           VAL = DATE_SP(KPER)%TS(KSTP)%DYEAR
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("SOLVER_ITERATION")
                       VAL = DBLE(KITER)
      CASE("RES")
                       CALL NAM(I)%GET(TWO,PROP,ID3)
                       !
                       IF(ID > SIZE(SWODAT%RESBAL2PROJ,TWO)) THEN
                           ERROR=ERROR//VAR%NAM(POS)//' BAD RESERVOIR ID'//NL
                           VAR%VAL(POS) = DZ
                           CYCLE NAM_LOOP
                       END IF
                       !
                       K = ID
                       !
                       ID  = SWODAT%RESBAL2PROJ(ONE,K)
                       ID2 = SWODAT%RESBAL2PROJ(TWO,K)
                       !
                       IF(.NOT. SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                           VAL = DZ
                       ELSE
                           !
                           SELECT CASE(PROP)
                           CASE("CAPACITY")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_CAPACITY
                           CASE("STORAGE")
                                             IF(LVL < TWO) THEN
                                                 TMP  = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_PREV
                                                 TMP2= SWODAT%FRSTART
                                                 LVAR =SWODAT%IS_LEAP_START
                                             ELSE
                                                 TMP = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE
                                                 TMP2= SWODAT%FRSTOP
                                                 LVAR = SWODAT%IS_LEAP_END
                                             END IF
                                             !
                                             IF(ID3 == Z) THEN
                                                 VAL = TMP
                                             ELSEIF(SWODAT%NRES_SPT(ID) == Z) THEN
                                                 VAL = TMP
                                             ELSE! ID3>0
                                                 CALL FRAC_POOL2SPLIT(SWODAT,ID,ID3,TMP2,TMP,VAL,LVAR) !VAL= FRAC STORAGE
                                             END IF
                           CASE("SPEC_MIN_STORAGE")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_SPEC_MIN
                           CASE("USE_STORAGE")
                                             IF(LVL < TWO) THEN
                                                 TMP  = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_PREV
                                                 TMP2= SWODAT%FRSTART
                                                 LVAR =SWODAT%IS_LEAP_START
                                             ELSE
                                                 TMP = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE
                                                 TMP2= SWODAT%FRSTOP
                                                 LVAR = SWODAT%IS_LEAP_END
                                             END IF
                                             !
                                             IF(ID3 == Z) THEN
                                                 VAL = TMP - SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_MIN
                                             ELSEIF(SWODAT%NRES_SPT(ID) == Z) THEN
                                                 VAL = TMP - SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_MIN
                                             ELSE! ID3>0
                                                 !
                                                 CALL FRAC_POOL2SPLIT(SWODAT,ID,ID3,TMP2,TMP,VAL,LVAR) !VAL= FRAC STORAGE
                                                 !
                                                 CALL FRAC_POOL2SPLIT(SWODAT,ID,ID3,TMP2, SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_MIN,TMP3,LVAR) !VAL= FRAC NON_PROJ STORAGE
                                                 !
                                                 VAL = VAL - TMP3
                                             END IF
                                             !
                                             IF(VAL < DZ) VAL = DZ
                           CASE("AREA")
                                             IF(LVL < TWO) THEN
                                                 TMP  = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_PREV
                                                 TMP2= SWODAT%FRSTART
                                                 TMP3=SWODAT%RESDAT(ID)%RESBAL(ID2)%AREA_PREV
                                                 LVAR =SWODAT%IS_LEAP_START
                                             ELSE
                                                 TMP = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE
                                                 TMP2= SWODAT%FRSTOP
                                                 TMP3=SWODAT%RESDAT(ID)%RESBAL(ID2)%AREA
                                                 LVAR = SWODAT%IS_LEAP_END
                                             END IF
                                             !
                                             IF(ID3 == Z) THEN
                                                 VAL = TMP3
                                             ELSEIF(SWODAT%NRES_SPT(ID) == Z) THEN
                                                 VAL = TMP3
                                             ELSE! ID3>0
                                                 CALL FRAC_POOL2SPLIT(SWODAT,ID,ID3,TMP2,TMP,TMP3,LVAR) !VAL= FRAC STORAGE
                                                 CALL ACAP_STOR2AREA(SWODAT,ID,ID3,TMP3,VAL)
                                             END IF
                           CASE("STAGE")
                                             IF(LVL < TWO) THEN
                                                 TMP  = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_PREV
                                                 TMP2= SWODAT%FRSTART
                                                 LVAR =SWODAT%IS_LEAP_START
                                             ELSE
                                                 TMP = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE
                                                 TMP2= SWODAT%FRSTOP
                                                 LVAR = SWODAT%IS_LEAP_END
                                             END IF
                                             !
                                             IF(ID3 == Z .AND. SWODAT%NRES_SPT(ID) == Z .OR. SWODAT%NRES_SPT(ID) == Z) THEN
                                                 CALL ACAP_STOR2ELEV(SWODAT,ID,ID2,TMP,VAL)
                                             ELSEIF(ID3 > Z .AND. SWODAT%NRES_SPT(ID) > Z) THEN ! FIX SCOTT
                                                 CALL FRAC_POOL2SPLIT(SWODAT,ID,ID3,TMP2,TMP,TMP3,LVAR) !VAL= FRAC STORAGE
                                                 CALL ACAP_STOR2ELEV(SWODAT,ID,ID3,TMP3,VAL)
                                             ELSE
                                                 ERROR=ERROR//VAR%NAM(POS)//NL
                                             END IF
                           CASE("INFLOW")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%INFLOW / DT
                           CASE("MAX_AREA")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_AREA
                           CASE("PRECIP")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%PRCP / DT
                           CASE("EVAP")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%EVAP / DT
                           CASE("RELEASE") !
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_SPEC  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_PROJ  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_FLOD  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_ADDF_INI   +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_REQF  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_MIN   +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%SPILL_WAY     +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%OVER_TOP
                                             ELSE
                                                 VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_SPEC  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_PROJ  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_FLOD  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_ADDF  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_REQF  +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_MIN   +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%SPILL_WAY     +  &
                                                       SWODAT%RESDAT(ID)%RESBAL(ID2)%OVER_TOP
                                             END IF
                                             !
                                             VAL = VAL / DT
                           CASE("PROJ_RELEASE")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_PROJ / DT
                           CASE("SPEC_RELEASE_FIN")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_SPEC / DT
                           CASE("REQ_RELEASE")
                                             IF(LVL == Z) THEN
                                                VAL = DZ
                                             ELSE
                                                VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_REQF / DT
                                             END IF
                           !CASE("ADD_RELEASE")
                           !                  IF(LVL < TWO) THEN
                           !                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_ADDF_INI / DT
                           !                  ELSE
                           !                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_ADDF / DT
                           !                  END IF
                           CASE("MIN_RELEASE_FIN")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_MIN / DT
                                             IF(VAL < NEARZERO_10) VAL = DZ
                           CASE("ADD_RELEASE_FIN")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_ADDF / DT
                                             IF(VAL < NEARZERO_10) VAL = DZ
                           CASE("FLOOD_RELEASE")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_FLOD / DT
                           CASE("SPILL")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%SPILL_WAY / DT
                           CASE("MAX_SPILL")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_SPILL / DT
                           CASE("MAX_RELEASE")
                                                  IF(    VAR%VAL(POS) < DZ) THEN
                                                                                   VAL = DZ
                                                  ELSEIF(VAR%VAL(POS) >= D100) THEN
                                                                                   VAL = inf
                                                  ELSE
                                                                                   VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_RELEASE
                                                  END IF
                           CASE("OVER_TOP")
                                             VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%OVER_TOP / DT
                           CASE("TRANSFER")
                                             IF(LVL < TWO) THEN
                                                 VAL = DZ
                                             ELSE
                                                 VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRANSFER/DT
                                             END IF
                           CASE DEFAULT
                                       ERROR=ERROR//VAR%NAM(POS)//NL
                           END SELECT
                       END IF
      CASE("PROJ")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       SELECT CASE(PROP)
                       CASE("CLOSEOUT") !NOTE CAN BE BOTH A PROPERTY AND RETURN VARIABLE, NOT RECOMENDED TO DO BOTH AT ONCE
                                                 VAL = DBLE(SWODAT%PROJ(ID)%AllocClose)
                       CASE("INIT_FLAG")
                                                 VAL = DBLE(SWODAT%PROJ(ID)%AllocStart)
                       CASE("ALLOC_DATE")
                                         IF( SWODAT%PROJ(ID)%AllocDate%NOT_SET() ) THEN
                                             !
                                             VAL = DZ
                                         ELSE
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  ! K holds current year
                                                 IF(LVL < TWO) THEN
                                                     K = INT(SWODAT%TS_START)
                                                 ELSE
                                                     K = INT(SWODAT%TS_STOP)
                                                 END IF
                                             ELSE
                                                 IF(LVL < TWO) THEN
                                                     K = DATE_SP(KPER)%TS(KSTP - ONE)%YEAR
                                                 ELSE
                                                     K = DATE_SP(KPER)%TS(KSTP      )%YEAR
                                                 END IF
                                             END IF
                                             !
                                             VAL = DATE_TO_DYEAR(SWODAT%PROJ(ID)%AllocDate%DAY, SWODAT%PROJ(ID)%AllocDate%MONTH, K) 
                                             !
                                         END IF
                       CASE("ALLOC_DAYS_EXACT") 
                                         IF( SWODAT%PROJ(ID)%AllocDate%NOT_SET() ) THEN
                                             !
                                             IF(LVL < TWO) THEN
                                                 VAL = DBLE(TOTIM - DELT)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             ELSE
                                                 VAL = DBLE(TOTIM)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             END IF
                                         ELSE
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 !
                                                 BLOCK
                                                    TYPE(DATE_OPERATOR):: DATE
                                                    !
                                                    IF(LVL < TWO) THEN
                                                        CALL DATE%INIT(SWODAT%TS_START)
                                                    ELSE
                                                        CALL DATE%INIT(SWODAT%TS_STOP)
                                                    END IF
                                                    !
                                                    K = DATE%DAYS_FROM_MD(SWODAT%PROJ(ID)%AllocDate)
                                                    !
                                                    VAL = DBLE(K)
                                                    !
                                                 END BLOCK
                                             ELSE
                                                 IF(LVL < TWO) THEN
                                                     J = KSTP - ONE
                                                 ELSE
                                                     J = KSTP
                                                 END IF
                                                 !
                                                 K = DATE_SP(KPER)%TS(J)%DAYS_FROM_MD(SWODAT%PROJ(ID)%AllocDate)
                                                 !
                                                 VAL = DBLE(K)
                                                 !
                                             END IF
                                         END IF
                       CASE("ALLOC_DAYS") 
                                         IF( SWODAT%PROJ(ID)%AllocDate%NOT_SET() ) THEN
                                             !
                                             IF(LVL < TWO) THEN
                                                 VAL = DBLE(TOTIM - DELT)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             ELSE
                                                 VAL = DBLE(TOTIM)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             END IF
                                         ELSE
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 !
                                                 BLOCK
                                                    TYPE(DATE_OPERATOR):: DATE
                                                    !
                                                    IF(LVL < TWO) THEN
                                                        CALL DATE%INIT(SWODAT%TS_START)
                                                    ELSE
                                                        CALL DATE%INIT(SWODAT%TS_STOP)
                                                    END IF
                                                    !
                                                    K = DATE%DAYS_FROM_MD(SWODAT%PROJ(ID)%AllocDate_At_TS)
                                                    !
                                                    VAL = DBLE(K)
                                                    !
                                                 END BLOCK
                                             ELSE
                                                 IF(LVL < TWO) THEN
                                                     J = KSTP - ONE
                                                 ELSE
                                                     J = KSTP
                                                 END IF
                                                 !
                                                 K = DATE_SP(KPER)%TS(J)%DAYS_FROM_MD(SWODAT%PROJ(ID)%AllocDate_At_TS)
                                                 !
                                                 VAL = DBLE(K)
                                                 !
                                             END IF
                                         END IF
                       CASE("AREA")
                                                 VAL = SWODAT%PROJ(ID)%AreaTot
                       CASE("IRR_AREA")
                                                 VAL = SWODAT%PROJ(ID)%AreaIrr
                       CASE("STORAGE")
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    IF(LVL < TWO) THEN
                                                        DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                    VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE_PREV
                                                               END IF
                                                        END DO
                                                    ELSE
                                                        DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                    VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE
                                                               END IF
                                                        END DO
                                                    END IF
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_PREV
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                       CASE("SPEC_MIN_STORAGE")
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE_SPEC_MIN
                                                               END IF
                                                    END DO
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_SPEC_MIN
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                       CASE("USE_STORAGE")
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE_PREV - SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE_MIN
                                                               END IF
                                                    END DO
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE_PREV - SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_MIN
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                                                 IF(VAL<DZ) VAL=DZ
                           CASE("TRANSFER")
                                             IF(LVL < TWO) THEN
                                                 VAL = DZ
                                             ELSE
                                                 VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRANSFER
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%STORAGE_TRANSFER
                                                               END IF
                                                    END DO
                                                    IF(ABS(VAL) < NEARZERO_5) VAL = DZ
                                                    !
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRANSFER
                                                     IF(ABS(VAL) < NEARZERO_5) VAL = DZ
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                                             END IF
                       CASE("SPEC_RELEASE_FIN")
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%RELEASE_SPEC
                                                               END IF
                                                    END DO
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_SPEC
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                       CASE("REQ_RELEASE")
                                             IF(LVL == Z) THEN
                                                VAL = DZ
                                             ELSE
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%RELEASE_REQF
                                                               END IF
                                                    END DO
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_REQF
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                                             END IF
                       CASE("DIST_RELEASE")
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%RELEASE_PROJ
                                                               END IF
                                                    END DO
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_PROJ
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                       CASE("INFLOW")
                                                 IF (ID2 == Z) THEN
                                                    VAL = DZ
                                                    DO J=ONE, SWODAT%NRES_BAL(ID)
                                                               IF(SWODAT%RESDAT(ID)%RESBAL(J)%INUSE) THEN
                                                                                VAL = VAL + SWODAT%RESDAT(ID)%RESBAL(J)%INFLOW
                                                               END IF
                                                    END DO
                                                 ELSEIF(   SWODAT%RESDAT(ID)%RESBAL(ID2)%INUSE) THEN
                                                     VAL = SWODAT%RESDAT(ID)%RESBAL(ID2)%INFLOW
                                                 ELSE
                                                     VAL = DZ
                                                 END IF
                       CASE("ADD_RELEASE_FIN")
                                                     VAL = SWODAT%RESDAT(ID)%PROJ_RELEASE_ADD
                                                     IF(VAL < NEARZERO_5) VAL = DZ
                       CASE("RELEASE")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%RELEASE
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%RELEASE
                                                     END IF
                       CASE("RELEASE_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%RELEASE_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%RELEASE_YTD
                                                     END IF
                       CASE("DIVERSION")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DIVERSION
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DIVERSION
                                                     END IF
                       CASE("DIVERSION_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DIVERSION_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DIVERSION_YTD
                                                     END IF
                                                 
                       CASE("DELIVERY")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DELIVERY
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DELIVERY
                                                     END IF
                       CASE("DELIVERY_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DELIVERY_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DELIVERY_YTD
                                                     END IF
                                                 
                       CASE("BYPASS")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%BYPASS
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%BYPASS
                                                     END IF
                       CASE("BYPASS_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%BYPASS_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%BYPASS_YTD
                                                     END IF
                       CASE("DIVRATIO")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DIVRATIO
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DIVRATIO
                                                     END IF
                                                 
                       CASE("DIVRATIO_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DIVRATIO_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DIVRATIO_YTD
                                                     END IF
                       CASE("DELIVEFF")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DELIVEFF
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DELIVEFF
                                                     END IF
                                                     IF(VAL > UNO) VAL = UNO
                                                 
                       CASE("DELIVEFF_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DELIVEFF_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DELIVEFF_YTD
                                                     END IF
                       CASE("CHARGE")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%CHARGE
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%CHARGE
                                                     END IF
                                                 
                       CASE("CHARGE_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%CHARGE_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%CHARGE_YTD
                                                     END IF
                       CASE("CREDIT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%CREDIT
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%CREDIT
                                                     END IF
                       CASE("CREDIT_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%CREDIT_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%CREDIT_YTD
                                                     END IF
                                                 
                       CASE("CHGRATIO")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%CHGRATIO
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%CHGRATIO
                                                     END IF
                       CASE("CHGRATIO_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%CHGRATIO_YTD
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%CHGRATIO_YTD
                                                     END IF
                                                 
                       CASE("DEMAND")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%TFDR
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%TFDR
                                                     END IF
                       CASE("DELORDER")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DELORDER
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DELORDER
                                                     END IF
                       CASE("DIVORDER")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%PROJ_PREV(ID)%DIVORDER
                                                     ELSE
                                                         VAL = SWODAT%PROJ(ID)%DIVORDER
                                                     END IF
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("DIST")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       SELECT CASE(PROP)
                       CASE("AREA")
                                                 VAL = SWODAT%DIST(ID)%AreaTot
                       CASE("IRR_AREA")
                                                 VAL = SWODAT%DIST(ID)%AreaIrr
                       !!!CASE("ALLOCATION")
                       !!!                              IF(LVL < TWO) THEN
                       !!!                                  VAL = SWODAT%DIST_PREV(ID)%ALLOC_TOTAL
                       !!!                              ELSE
                       !!!                                  VAL = SWODAT%DIST(ID)%ALLOC_TOTAL
                       !!!                              END IF
                       CASE("ALLOTMENT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%EQ_ALLOTMENT
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%EQ_ALLOTMENT
                                                     END IF
                       CASE("BALANCE")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%BALANCE
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%BALANCE
                                                     END IF
                                                 
                       CASE("DEMAND")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%TFDR
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%TFDR
                                                     END IF
                       CASE("DELORDER")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DELORDER
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DELORDER
                                                     END IF
                       CASE("DIVORDER")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DIVORDER
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DIVORDER
                                                     END IF
                       CASE("DIVERSION")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DIVERSION
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DIVERSION
                                                     END IF
                       CASE("DIVERSION_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DIVERSION_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DIVERSION_YTD
                                                     END IF
                       CASE("DELIVERY")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DELIVERY
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DELIVERY
                                                     END IF
                       CASE("DELIVERY_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DELIVERY_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DELIVERY_YTD
                                                     END IF
                       CASE("BYPASS")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%BYPASS
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%BYPASS
                                                     END IF
                       CASE("BYPASS_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%BYPASS_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%BYPASS_YTD
                                                     END IF
                       CASE("DELIVEFF")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DELIVEFF
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DELIVEFF
                                                     END IF
                                                     IF(VAL > UNO) VAL = UNO
                       CASE("DELIVEFF_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%DELIVEFF_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%DELIVEFF_YTD
                                                     END IF
                       CASE("CHARGE")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%CHARGE
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%CHARGE
                                                     END IF
                       CASE("CHARGE_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%CHARGE_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%CHARGE_YTD
                                                     END IF
                       CASE("CREDIT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%CREDIT
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%CREDIT
                                                     END IF
                       CASE("CREDIT_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%CREDIT_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%CREDIT_YTD
                                                     END IF
                                                 
                       CASE("CHGRATIO")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%CHGRATIO
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%CHGRATIO
                                                     END IF
                       CASE("CHGRATIO_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%DIST_PREV(ID)%CHGRATIO_YTD
                                                     ELSE
                                                         VAL = SWODAT%DIST(ID)%CHGRATIO_YTD
                                                     END IF
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("UNIT")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       SELECT CASE(PROP)
                       CASE("AREA")
                                                 VAL = SWODAT%UNIT(ID)%AreaTot
                       CASE("IRR_AREA")
                                                 VAL = SWODAT%UNIT(ID)%AreaIrr
                       CASE("ALLOTMENT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%ALLOTMENT
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%ALLOTMENT
                                             END IF
                       CASE("BALANCE")       
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%BALANCE
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%BALANCE
                                             END IF
                                             
                       CASE("DEMAND")        
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%TFDR
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%TFDR
                                             END IF
                       CASE("DELORDER")      
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DELORDER
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DELORDER
                                             END IF
                       CASE("DIVORDER")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DIVORDER
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DIVORDER
                                             END IF
                       CASE("DIVERSION")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DIVERSION
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DIVERSION
                                             END IF
                       CASE("DIVERSION_TOT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DIVERSION_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DIVERSION_YTD
                                             END IF
                       CASE("DELIVERY")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DELIVERY
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DELIVERY
                                             END IF
                       CASE("DELIVERY_TOT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DELIVERY_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DELIVERY_YTD
                                             END IF
                       CASE("BYPASS")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%BYPASS
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%BYPASS
                                             END IF
                       CASE("BYPASS_TOT")    
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%BYPASS_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%BYPASS_YTD
                                             END IF
                       CASE("DELIVEFF")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DELIVEFF
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DELIVEFF
                                             END IF
                                             IF(VAL > UNO) VAL = UNO
                       CASE("DELIVEFF_TOT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%DELIVEFF_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%DELIVEFF_YTD
                                             END IF
                       CASE("CHARGE")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%CHARGE
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%CHARGE
                                             END IF
                       CASE("CHARGE_TOT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%CHARGE_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%CHARGE_YTD
                                             END IF
                       CASE("CREDIT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%CREDIT
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%CREDIT
                                             END IF
                       CASE("CREDIT_TOT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%CREDIT_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%CREDIT_YTD
                                             END IF
                                                 
                       CASE("CHGRATIO")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%CHGRATIO
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%CHGRATIO
                                             END IF
                       CASE("CHGRATIO_TOT")
                                             IF(LVL < TWO) THEN
                                                 VAL = SWODAT%UNIT_PREV(ID)%CHGRATIO_YTD
                                             ELSE
                                                 VAL = SWODAT%UNIT(ID)%CHGRATIO_YTD
                                             END IF
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("WBS", "FARM")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(ID < Z .OR. ID > FDIM%NFARM) THEN
                           ERROR=ERROR//VAR%NAM(POS)//NL
                       ELSE
                           SELECT CASE(PROP)
                           CASE("AREA")
                                                     VAL = SWODAT%FARM(ID)%AreaTot
                           CASE("IRR_AREA")
                                                     VAL = SWODAT%FARM(ID)%AreaIrr
                           CASE("ALLOC_FRAC")
                                                     VAL = SWODAT%FARM(ID)%DIST_ALLOC_FRAC
                           CASE("ALLOTMENT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%FARM_PREV(ID)%ALLOTMENT
                                                     ELSE
                                                         VAL = SWODAT%FARM(ID)%ALLOTMENT
                                                     END IF
                                                     
                           CASE("DEMAND")
                                                     VAL = WBS%DEMAND(ID)
                           CASE("SW_DEMAND")
                                                     VAL = SWODAT%FMP_SW_DEMAND(ID)
                           CASE("ORDER")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%FARM_PREV(ID)%DELORDER / DT
                                                     ELSE
                                                         VAL = SWODAT%FARM(ID)%DELORDER / DT
                                                     END IF
                           CASE("DELIVERY")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%FARM_PREV(ID)%DELIVERY / DT
                                                     ELSE
                                                         VAL = SWODAT%FARM(ID)%DELIVERY / DT
                                                     END IF
                           CASE("DELIVERY_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%FARM_PREV(ID)%DELIVERY_YTD
                                                     ELSE
                                                         VAL = SWODAT%FARM(ID)%DELIVERY_YTD
                                                     END IF
                           CASE("BALANCE")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%FARM_PREV(ID)%BALANCE / DT
                                                     ELSE
                                                         VAL = SWODAT%FARM(ID)%BALANCE / DT
                                                     END IF
                           CASE DEFAULT
                                       ERROR=ERROR//VAR%NAM(POS)//NL
                           END SELECT
                       END IF
      CASE("AUX")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(ID < Z .OR. ID > SWODAT%NAUXDEM) THEN
                           ERROR=ERROR//VAR%NAM(POS)//NL
                       ELSE
                           SELECT CASE(PROP)
                           CASE("AREA")
                                                     VAL = SWODAT%AUXDEM(ID)%AREA
                           !CASE("IRR_AREA")
                           !                          VAL = SWODAT%AUXDEM(ID)%AREA
                           CASE("ALLOTMENT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%ALLOTMENT
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%ALLOTMENT
                                                     END IF
                                                     
                           CASE("DEMAND")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%DEMAND / DT
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%DEMAND / DT
                                                     END IF
                                                     
                           CASE("SW_DEMAND")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%DEMAND / DT
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%DEMAND / DT
                                                     END IF
                                                     
                           CASE("ORDER")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%DELORDER / DT
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%DELORDER / DT
                                                     END IF
                                                     
                           CASE("DELIVERY")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%DELIVERY / DT
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%DELIVERY / DT
                                                     END IF
                           CASE("DELIVERY_TOT")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%DELIVERY_YTD
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%DELIVERY_YTD
                                                     END IF
                           CASE("BALANCE")
                                                     IF(LVL < TWO) THEN
                                                         VAL = SWODAT%AUXDEM_PREV(ID)%BALANCE / DT
                                                     ELSE
                                                         VAL = SWODAT%AUXDEM(ID)%BALANCE / DT
                                                     END IF
                           CASE("ALLOC_FRAC")
                                                 VAL = SWODAT%AUXDEM(ID)%DIST_ALLOC_FRAC
                                                              
                           CASE DEFAULT
                                       ERROR=ERROR//VAR%NAM(POS)//NL
                           END SELECT
                       END IF
      CASE("TS")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(    PROP=="START") THEN
                           FLAG = Z
                       ELSEIF(PROP=="END") THEN
                           FLAG = ONE
                       ELSEIF(PROP=="LENGTH") THEN !STEP LENTH IN DAYS
                           FLAG = NEG
                           !
                           VAL = DELT_TO_DAY(DT,ITMUNI)
                           !
                       ELSEIF(PROP=="NUM") THEN !STEP LENTH IN DAYS
                           FLAG = NEG
                           !
                           VAL = DBLE(KSTP)
                       ELSE
                           FLAG = NEG
                           ERROR=ERROR//VAR%NAM(POS)//NL
                       END IF
                       !
                       CALL NAM(I)%GET(THREE,PROP,ID3)
                       !
                       IF(FLAG >= Z) THEN
                          BLOCK
                             DOUBLE PRECISION:: DYEAR
                             INTEGER         :: DAY, MONTH, YEAR, KKSTP
                             !
                             IF(FLAG == ONE) THEN
                                 KKSTP = KSTP
                                 DYEAR = SWODAT%TS_STOP
                             ELSE
                                 KKSTP = KSTP - ONE
                                 DYEAR = SWODAT%TS_START
                             END IF
                             !
                             SELECT CASE(PROP)
                             CASE("DAY")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR) 
                                             ELSE
                                                 DAY = DATE_SP(KPER)%TS(KKSTP)%DAY
                                             END IF
                                             VAL = DBLE(DAY)
                             CASE("MONTH")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR) 
                                             ELSE
                                                 MONTH = DATE_SP(KPER)%TS(KKSTP)%MONTH
                                             END IF
                                             VAL = DBLE(MONTH)
                             CASE("YEAR")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = INT(DYEAR)
                                             ELSE
                                                 VAL = DBLE(DATE_SP(KPER)%TS(KKSTP)%YEAR)
                                             END IF
                             CASE("DYEAR")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = DYEAR
                                             ELSE
                                                 VAL = DATE_SP(KPER)%TS(KKSTP)%DYEAR
                                             END IF
                             CASE("YEAR_FRAC")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = YEAR_FRACTION(DYEAR)
                                             ELSE
                                                 VAL = YEAR_FRACTION(DATE_SP(KPER)%TS(KKSTP)%DYEAR)
                                             END IF
                             CASE("DAY_FRAC")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR, VAL) 
                                             ELSE
                                                 VAL = DATE_SP(KPER)%TS(KKSTP)%FRAC
                                             END IF
                             CASE("DOY")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR, JDN=ID) 
                                                 VAL = DBLE(ID)
                                             ELSE
                                                 VAL = DBLE(DATE_SP(KPER)%TS(KKSTP)%JDN)
                                             END IF
                             CASE("SIM_TIME")
                                             IF(FLAG == ONE) THEN
                                                 VAL = DBLE(TOTIM)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             ELSE
                                                 VAL = DBLE(TOTIM - DELT)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             END IF
                             CASE("DATE")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = DYEAR
                                             ELSE
                                                 VAL = DATE_SP(KPER)%TS(KKSTP)%DYEAR
                                             END IF
                             CASE DEFAULT
                                         ERROR=ERROR//VAR%NAM(POS)//NL
                             END SELECT
                          END BLOCK
                       END IF
      CASE("SP")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(    PROP=="START") THEN
                           FLAG = Z
                       ELSEIF(PROP=="END") THEN
                           FLAG = ONE
                       ELSEIF(PROP=="LENGTH") THEN !STEP LENTH IN DAYS
                           FLAG = NEG
                           !
                           VAL = DELT_TO_DAY(TOTPERTIM,ITMUNI)
                           !
                       ELSEIF(PROP=="NUM") THEN !STEP LENTH IN DAYS
                           FLAG = NEG
                           !
                           VAL = DBLE(KPER)
                       ELSEIF(PROP=="NSTP") THEN !STEP LENTH IN DAYS
                           FLAG = NEG
                           !
                           VAL = DBLE(NSTP(KPER))
                       ELSE
                           FLAG = NEG
                           ERROR=ERROR//VAR%NAM(POS)//NL
                       END IF
                       !
                       CALL NAM(I)%GET(THREE,PROP,ID3)
                       !
                       IF(FLAG >= Z) THEN
                          BLOCK
                             DOUBLE PRECISION:: DYEAR
                             INTEGER         :: DAY, MONTH, YEAR, KKSTP
                             !
                             IF(FLAG == ONE) THEN
                                 KKSTP = NSTP(KPER)
                                 DYEAR = SWODAT%SP_STOP
                             ELSE
                                 KKSTP = Z
                                 DYEAR = SWODAT%SP_START
                             END IF
                             !
                             SELECT CASE(PROP)
                             CASE("DAY")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR) 
                                             ELSE
                                                 DAY = DATE_SP(KPER)%TS(KKSTP)%DAY
                                             END IF
                                             VAL = DBLE(DAY)
                             CASE("MONTH")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR) 
                                             ELSE
                                                 DAY = DATE_SP(KPER)%TS(KKSTP)%MONTH
                                             END IF
                                             VAL = DBLE(MONTH)
                             CASE("YEAR")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = INT(DYEAR)
                                             ELSE
                                                 VAL = DBLE(DATE_SP(KPER)%TS(KKSTP)%YEAR)
                                             END IF
                             CASE("DYEAR")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = DYEAR
                                             ELSE
                                                 VAL = DATE_SP(KPER)%TS(KKSTP)%DYEAR
                                             END IF
                             CASE("YEAR_FRAC")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = YEAR_FRACTION(DYEAR)
                                             ELSE
                                                 VAL = YEAR_FRACTION(DATE_SP(KPER)%TS(KKSTP)%DYEAR)
                                             END IF
                             CASE("DAY_FRAC")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR, VAL) 
                                             ELSE
                                                 VAL = DATE_SP(KPER)%TS(KKSTP)%FRAC
                                             END IF
                             CASE("DOY")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 CALL DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR, JDN=ID) 
                                                 VAL = DBLE(ID)
                                             ELSE
                                                 VAL = DBLE(DATE_SP(KPER)%TS(KKSTP)%JDN)
                                             END IF
                             CASE("SIM_TIME")
                                             IF(FLAG == ONE) THEN
                                                 VAL = DBLE(SIMTIM_PER)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             ELSE
                                                 VAL = DBLE(SIMTIM_PER-TOTPERTIM)
                                                 VAL = DELT_TO_DAY(VAL,ITMUNI)
                                             END IF
                             CASE("DATE")
                                             IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN  !SET SP STARTING DATE
                                                 VAL = DYEAR
                                             ELSE
                                                 VAL = DATE_SP(KPER)%TS(KKSTP)%DYEAR
                                             END IF
                             CASE DEFAULT
                                         ERROR=ERROR//VAR%NAM(POS)//NL
                             END SELECT
                          END BLOCK
                       END IF
      CASE("SFR")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       K = FDIM%SFR_ID%GET_POS(PROP)
                       !
                       IF(K > Z) THEN
                                                ID  = FDIM%SFR_ID%SEG_RCH(ONE,K)
                                                ID2 = FDIM%SFR_ID%SEG_RCH(TWO,K)
                                                IF(ID2 < ONE) ID2 = ONE
                                                CALL NAM(I)%GET(THREE,PROP,ID3)
                       ELSEIF(PROP=='INT' ) THEN
                                                CALL NAM(I)%GET(THREE,PROP,ID3)
                       ELSE
                           ID2 = NEG
                       END IF
                       !
                       IF(ID < ONE) THEN
                           IF(PROP=='INT') THEN
                               ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD SEGMENT NUMBER (POSSIBLE ERROR IN VARIABLE NAME FORMAT WHICH SHOULD BE "SFR.SEG.RCH." OR "SFR.SFR_NAM."'//NL
                           ELSE
                               ERROR=ERROR//NL//VAR%NAM(POS)//' COULD NOT IDENTIFY THE SFR.SEG.RCH. OR SFR.SFR_NAM.'//NL//'THE PART OF THE VARIABLE NAME THAT HAD ISSES IS "'//TRIM(PROP)//'"'//NL//'THE SFR_NAM MAY NOT BE DEFINED IN THE FMP GLOBAL INPUT BOCK UNDER THE KEYWORD "SFR_NAMES".'//NL//'THE FOLLOWING IS A LIST OF CURRENTLY DEFINED SFR_NAMES:'//NL//JOIN_TXT(FDIM%SFR_ID%NAM,NL,BLN)//'NOTE THAT THIS ERROR COULD HAVE BEEN TRIGGERED IF YOU MEANT TO LOAD A SPECIFIED SEGMENT AND REACH AND THAT FAILED TO LOAD.'//NL
                           END IF
                       ELSE
                           SELECT CASE(PROP)
                           CASE("INFLOW")
                                                     DO J=ONE, SWODAT%NSFR_OLD_FLO
                                                         IF (I == SWODAT%SFR_OLD_FLO(J)%PROP_PUL_POS) THEN
                                                             !
                                                             VAL = SWODAT%SFR_OLD_FLO(J)%FLO
                                                             !
                                                             EXIT
                                                         END IF
                                                     END DO
                           CASE("OUTFLOW")
                                                     DO J=ONE, SWODAT%NSFR_OLD_FLO
                                                         IF (I == SWODAT%SFR_OLD_FLO(J)%PROP_PUL_POS) THEN
                                                             !
                                                             VAL = SWODAT%SFR_OLD_FLO(J)%FLO
                                                             !
                                                             EXIT
                                                         END IF
                                                     END DO
                           CASE("DIV")
                                                     VAL = SEG(2,ID)
                                                     
                           CASE("INFLOW_NO_REL")
                                                     IF(KITER==TWO) THEN
                                                        !
                                                        IF(ID2 == NEG) ID2 = ONE
                                                        !
                                                        K = SEG_NSTRM(ID) + ID2
                                                        VAL=DBLE(STRM(TEN,K))
                                                     ELSE
                                                         CYCLE
                                                     END IF
                           CASE("OUTFLOW_NO_REL")
                                                     IF(KITER==TWO) THEN
                                                        !
                                                        IF(ID2 == NEG) THEN
                                                            K = SEG_NSTRM(ID+ONE)
                                                        ELSE
                                                            K = SEG_NSTRM(ID) + ID2
                                                        END IF
                                                        VAL=DBLE(STRM(9,K))
                                                     ELSE
                                                         CYCLE
                                                     END IF
                           CASE DEFAULT
                                       ERROR=ERROR//VAR%NAM(POS)//NL
                           END SELECT
                       END IF
      CASE("HEAD")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(PROP=='INT' ) THEN
                           CALL NAM(I)%GET(THREE,PROP,ID3)
                       ELSE
                           ID3 = ONE
                       END IF
                       !
                       IF(ID<ONE.OR.ID2<ONE.OR.ID3<ONE.OR.ID>NLAY.OR.ID2>NROW.OR.ID3>NCOL) PROP(:) = 'err'
                       SELECT CASE(PROP)
                       CASE("INT")
                                                 IF(LVL < TWO) THEN
                                                     VAL=HOLD(ID,ID2,ID3)
                                                 ELSE
                                                     VAL=HNEW(ID,ID2,ID3)
                                                 END IF
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("HNEW")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(PROP=='INT' ) THEN
                           CALL NAM(I)%GET(THREE,PROP,ID3)
                       ELSE
                           ID3 = ONE
                       END IF
                       !
                       IF(ID<ONE.OR.ID2<ONE.OR.ID3<ONE.OR.ID>NLAY.OR.ID2>NROW.OR.ID3>NCOL) PROP(:) = 'err'
                       SELECT CASE(PROP)
                       CASE("INT")
                                                 VAL=HNEW(ID3,ID2,ID) !ID=LAY, ID2=ROW, ID3=COL
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("WATER_DEPTH")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(ID<ONE.OR.ID2<ONE.OR.ID>NROW.OR.ID2>NCOL) PROP(:) = 'err'
                       SELECT CASE(PROP)
                       CASE("INT")
                                                 ID3 = UPLAY(ID2,ID)               !ID3=LAY, ID=ROW, ID2=COL
                                                 IF(ID3==Z) THEN
                                                     VAL = WBS%GSE(ID2,ID) - BOTM(ID2,ID,LBOTM(NLAY))
                                                 ELSEIF(LVL < TWO) THEN
                                                     VAL = WBS%GSE(ID2,ID) - HOLD(ID2,ID,ID3)  !ID3=LAY, ID=ROW, ID2=COL
                                                 ELSE
                                                     VAL = WBS%GSE(ID2,ID) - HNEW(ID2,ID,ID3)  !ID3=LAY, ID=ROW, ID2=COL
                                                 END IF
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE("WATER_TABLE")
                       CALL NAM(I)%GET(TWO,PROP,ID2)
                       !
                       IF(ID<ONE.OR.ID2<ONE.OR.ID>NROW.OR.ID2>NCOL) PROP(:) = 'err'
                       SELECT CASE(PROP)
                       CASE("INT")
                                                 ID3 = UPLAY(ID2,ID)               !ID3=LAY, ID=ROW, ID2=COL
                                                 IF(ID3==Z) THEN
                                                     VAL = BOTM(ID2,ID,LBOTM(NLAY))
                                                 ELSEIF(LVL < TWO) THEN
                                                     VAL = HOLD(ID2,ID,ID3)  !ID3=LAY, ID=ROW, ID2=COL
                                                 ELSE
                                                     VAL = HNEW(ID2,ID,ID3)  !ID3=LAY, ID=ROW, ID2=COL
                                                 END IF
                       CASE DEFAULT
                                   ERROR=ERROR//VAR%NAM(POS)//NL
                       END SELECT
      CASE DEFAULT
                  ERROR=ERROR//VAR%NAM(POS)//NL
      END SELECT
      !
      VAR%VAL(POS) = VAL
      !
  END DO NAM_LOOP
  !  
END SUBROUTINE
!
SUBROUTINE VARIABLE_SET_RETURN_VALUES(VAR, NAM, KPER, KSTP, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
  ! Cation, this is only called when necessary and not every time step so DELT can change
  USE IDENTIFY_MODEL_PROPERTY_LOCAL_INTERFACE
  IMPLICIT NONE
  !
  CLASS(S_VARIABLE_LIST),                                 INTENT(INOUT):: VAR
  TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
  INTEGER,                                                INTENT(IN   ):: KPER, KSTP
  CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
  CHARACTER(:),ALLOCATABLE:: PROP
  DOUBLE PRECISION:: VAL, TMP2!, DT
  INTEGER:: I,J,K, ID,ID2,ID3, DUM,FLAG, POS
  !
  IF (.NOT. ALLOCATED(ERROR)) ALLOCATE(ERROR,SOURCE=NL)
  !
  !DT = SPTIM(KPER)%DT(KSTP)
  !
  DO I = ONE, SIZE(NAM)
      !
      CALL NAM(I)%GET(ONE,PROP,ID)
      !
      POS = NAM(I)%POS
      VAL = DZ
      !
      SELECT CASE(PROP)
      CASE("RES")
                  !
                  IF(ID < ONE .OR. ID > SIZE(SWODAT%RESBAL2PROJ,TWO)) THEN
                           ERROR=ERROR//VAR%NAM(POS)//' BAD RESERVOIR ID'//NL
                           CYCLE
                  END IF
                  !
                  CALL NAM(I)%GET(TWO,PROP,ID2)
                  !
                  K   = ID
                  ID  = SWODAT%RESBAL2PROJ(ONE,K)
                  ID2 = SWODAT%RESBAL2PROJ(TWO,K)
                  !
                  SELECT CASE(PROP)
                  CASE("REQFLOW_FRAC")!  		Fraction of total project release that is released from reservoir RES_ID
                                      IF (ID2 > Z .AND. ID2 <= SWODAT%NRES_BAL(ID) ) THEN
                                            SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_REQ_FRAC_INI = VAR%VAL(POS)
                                      ELSE
                                            PROP(:) = BLNK
                                            ERROR=ERROR//VAR%NAM(POS)//' HAS RESERVOIR ID "'//NUM2STR(ID2)//'" THAT IS AN INVALID ID. IT MUST BE BETWEEN 1 AND '//NUM2STR(SWODAT%NRES_BAL(ID))//NL
                                      END IF
                  CASE("PROJ_REL_FRAC")!  	Fraction of total project release that is released from reservoir RES_ID
                                      IF (ID2 > Z .AND. ID2 <= SWODAT%NRES_BAL(ID) ) THEN
                                            SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_DMD_FRAC_INI = VAR%VAL(POS)
                                      ELSE
                                            PROP(:) = BLNK
                                            ERROR=ERROR//VAR%NAM(POS)//' HAS RESERVOIR ID "'//NUM2STR(ID2)//'" THAT IS AN INVALID ID. IT MUST BE BETWEEN 1 AND '//NUM2STR(SWODAT%NRES_BAL(ID))//NL
                                      END IF
                  CASE("CAPACITY")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_CAPACITY = VAR%VAL(POS)
                  CASE("MIN_RELEASE")
                                         IF(    VAR%VAL(POS) < DZ .OR. VAR%VAL(POS) >= D100) THEN
                                                                          SWODAT%RESDAT(ID)%RESBAL(ID2)%MIN_RELEASE_S = DZ
                                         ELSE
                                                                          SWODAT%RESDAT(ID)%RESBAL(ID2)%MIN_RELEASE_S = VAR%VAL(POS) !DELT is multiplied by this and sets variable RELEASE_MIN_INPUT
                                         END IF
                  CASE("MAX_RELEASE")
                                         IF(    VAR%VAL(POS) < DZ) THEN
                                                                          SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_RELEASE_S = DZ
                                         ELSEIF(VAR%VAL(POS) >= D100) THEN
                                                                          SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_RELEASE_S = inf
                                         ELSE
                                                                          SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_RELEASE_S = VAR%VAL(POS) !DELT is multiplied by this and sets variable MAX_RELEASE_S_Vol
                                         END IF
                  CASE("ADD_RELEASE")
                                         IF(VAR%VAL(POS) > NEARZERO_10) THEN
                                            SWODAT%RESDAT(ID)%RESBAL(ID2)%ADD_RELEASE_S = VAR%VAL(POS)  !Converted to VOL at a later time -- read as rate, but SWO expects VOL
                                         ELSE
                                             SWODAT%RESDAT(ID)%RESBAL(ID2)%ADD_RELEASE_S = DZ
                                         END IF
                  CASE("SPEC_RELEASE")
                                         IF(VAR%VAL(POS) > NEARZERO_10) THEN
                                            SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_SPEC_S = VAR%VAL(POS)  !Converted to VOL at a later time
                                         ELSE
                                             SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_SPEC_S = DZ
                                         END IF
                  CASE("MAX_STORAGE")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_STORAGE_S = VAR%VAL(POS)
                  CASE("MAX_STAGE")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_STAGE_S = VAR%VAL(POS)
                  CASE("SPILL_STAGE")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%SPILL_STAGE_S = VAR%VAL(POS)
                  CASE("SPILL_STORAGE")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%SPILL_STORE_S = VAR%VAL(POS)
                  CASE("SPILL_PREF")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%SPILLWAY_PREF = NINT(VAR%VAL(POS)).NE.Z
                  CASE("MIN_STORAGE")
                                         SWODAT%RESDAT(I)%RESBAL(J)%MIN_STORAGE_S = VAR%VAL(POS)
                  CASE("MIN_STORAGE_TRAN")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%MIN_STORAGE_TRAN_S = VAR%VAL(POS)
                  CASE("MAX_SPILL")
                                         IF(VAR%VAL(POS) < DZ .OR. VAR%VAL(POS) > D100) THEN
                                             SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_SPILL_RATE = inf
                                         ELSE
                                             SWODAT%RESDAT(ID)%RESBAL(ID2)%MAX_SPILL_RATE = VAR%VAL(POS)
                                         END IF
                  CASE("MAX_STAGE_DROP")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%ELEV_CHNG_S = VAR%VAL(POS)
                  CASE("TRAN_FRAC")
                                         SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRAN_FRAC = VAR%VAL(POS)
                                         IF    (SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRAN_FRAC>UNO) THEN
                                                SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRAN_FRAC=UNO
                                         ELSEIF(SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRAN_FRAC<DZ) THEN
                                                SWODAT%RESDAT(ID)%RESBAL(ID2)%STORAGE_TRAN_FRAC=DZ
                                         END IF
                  CASE DEFAULT
                              ERROR=ERROR//VAR%NAM(POS)//NL
                  END SELECT
      CASE("DIST")
                  !
                  IF(ID < ONE .OR. SWODAT%NDIST < ID ) THEN
                      ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD DISTRICT ID NUMBER, IT IS EITHER >NDIST OR <1'//NL
                      RETURN
                  END IF
                  !
                  CALL NAM(I)%GET(TWO,PROP,ID2)
                  !
                  SELECT CASE(PROP)
                  CASE("ALLOCATION")
                                    SWODAT%DIST(ID)%ALLOC_TOTAL = VAR%VAL(POS)
                  CASE("ALLOTMENT")
                                    IF(NEGNEARZERO_10 < VAR%VAL(POS) .AND. VAR%VAL(POS) < NEARZERO_10) THEN
                                        SWODAT%DIST(ID)%S_ALLOTMENT = DZ
                                    ELSE
                                        SWODAT%DIST(ID)%S_ALLOTMENT = VAR%VAL(POS)
                                    END IF
                  CASE("ALLOTMENT_VOL")
                                    IF(NEGNEARZERO_10 < VAR%VAL(POS) .AND. VAR%VAL(POS) < NEARZERO_10) THEN
                                        SWODAT%DIST(ID)%S_ALLOTMENT_VOL = DZ
                                    ELSE
                                        SWODAT%DIST(ID)%S_ALLOTMENT_VOL = VAR%VAL(POS)
                                    END IF
                  CASE DEFAULT
                              ERROR=ERROR//VAR%NAM(POS)//NL
                  END SELECT
      CASE("WBS","FARM")
                  !
                  IF(ID < ONE .OR. SWODAT%NFARM < ID ) THEN
                      ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD WBS/FARM ID NUMBER, IT IS EITHER >NWBS OR <1'//NL
                      RETURN
                  END IF
                  !
                  CALL NAM(I)%GET(TWO,PROP,ID2)
                  !
                  SELECT CASE(PROP)  
                  CASE("DELIVERY_LIMIT")
                      IF    (VAR%VAL(POS) < DZ) THEN
                                                  SWODAT%FMP_SW_LIMIT_RULZ(ID) = DZ
                      ELSEIF(VAR%VAL(POS) < D99) THEN
                                                  SWODAT%FMP_SW_LIMIT_RULZ(ID) = VAR%VAL(POS)
                      ELSE
                                                  SWODAT%FMP_SW_LIMIT_RULZ(ID) = inf
                      END IF
                  CASE("ALLOC_FRAC")
                                                  SWODAT%FARM(ID)%DIST_ALLOC_FRAC  = VAR%VAL(POS)
                  CASE("REQ_DELIVERY")
                                                  SWODAT%FARM(ID)%REQ_DELIVERY_S = VAR%VAL(POS)
                  CASE("REQ_DELIVERY_VOL")
                                                  SWODAT%FARM(ID)%REQ_DELIVERY_VOL_S = VAR%VAL(POS)
                  CASE("ALLOTMENT")
                                    IF(NEGNEARZERO_10 < VAR%VAL(POS) .AND. VAR%VAL(POS) < NEARZERO_10) THEN
                                        SWODAT%FARM(ID)%S_ALLOTMENT = DZ
                                    ELSE
                                        SWODAT%FARM(ID)%S_ALLOTMENT = VAR%VAL(POS)
                                    END IF
                  CASE DEFAULT
                              ERROR=ERROR//VAR%NAM(POS)//NL
                  END SELECT
      CASE("AUX")
                  IF(ID < ONE .OR. SWODAT%NAUXDEM < ID ) THEN
                      ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD AUXILIARY ID NUMBER, IT IS EITHER >NAUX_DEMAND OR <1'//NL
                      RETURN
                  END IF
                  !
                  CALL NAM(I)%GET(TWO,PROP,ID2)
                  !
                  SELECT CASE(PROP)  
                  CASE("ALLOTMENT")
                                    IF(NEGNEARZERO_10 < VAR%VAL(POS) .AND. VAR%VAL(POS) < NEARZERO_10) THEN
                                        SWODAT%AUXDEM(ID)%S_ALLOTMENT = DZ
                                    ELSE
                                        SWODAT%AUXDEM(ID)%S_ALLOTMENT = VAR%VAL(POS)
                                    END IF
                  CASE("ALLOC_FRAC")
                                        SWODAT%AUXDEM(ID)%DIST_ALLOC_FRAC  = VAR%VAL(POS)
                  CASE("DEMAND")
                      IF    (VAR%VAL(POS) < DZ) THEN
                                                  SWODAT%AUXDEM(ID)%S_DEMAND = DZ
                      ELSEIF(VAR%VAL(POS) < D100) THEN
                                                  SWODAT%AUXDEM(ID)%S_DEMAND = VAR%VAL(POS)
                      ELSE
                                                  SWODAT%AUXDEM(ID)%S_DEMAND = D100
                      END IF
                  CASE("DELIVERY_LIMIT")
                      IF    (VAR%VAL(POS) < DZ) THEN
                                                  SWODAT%AUXDEM(ID)%S_DEMAND_LIM = DZ
                      ELSEIF(VAR%VAL(POS) < D100) THEN
                                                  SWODAT%AUXDEM(ID)%S_DEMAND_LIM = VAR%VAL(POS)
                      ELSE
                                                  SWODAT%AUXDEM(ID)%S_DEMAND_LIM = D100
                      END IF
                  CASE DEFAULT
                              ERROR=ERROR//VAR%NAM(POS)//NL
                  END SELECT
      CASE("PROJ")
                  CALL NAM(I)%GET(TWO,PROP,ID2)
                  !
                  IF(ID < ONE .OR. SWODAT%NPROJ < ID ) THEN
                      PROP(:) = BLNK
                      ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD PROJECT ID NUMBER, IT IS EITHER >NPROJ OR <1'//NL
                  END IF
                  !
                  SELECT CASE(PROP) ! or 
                  CASE("RELEASE_FRAC_RES")!  	Fraction of total project release that is released from reservoir RES_ID
                                      IF (ID2 > Z .AND. ID2 <= SWODAT%NRES_BAL(ID) ) THEN
                                            SWODAT%RESDAT(ID)%RESBAL(ID2)%RELEASE_DMD_FRAC_INI = VAR%VAL(POS)
                                      ELSE
                                            PROP(:) = BLNK
                                            ERROR=ERROR//VAR%NAM(POS)//' HAS RESERVOIR ID "'//NUM2STR(ID2)//'" THAT IS AN INVALID ID. IT MUST BE BETWEEN 1 AND '//NUM2STR(SWODAT%NRES_BAL(ID))//NL
                                      END IF
                  CASE("CLOSEOUT")
                          IF(VAR%VAL(POS) > HALF) THEN
                              SWODAT%PROJ(ID)%AllocClose = 1
                              !
                              IF(SWODAT%DELT2.GE.1D99) THEN !END OF SIMULATION - DO NOTHING
                                  !
                                  CONTINUE
                                  !
                              ELSEIF(DATE_SP(1)%TS(0)%IS_SET()) THEN
                                  !
                                  IF(SWODAT%PROJ(ID)%AllocDate%DYEAR .NE. SWODAT%TS_STOP_NEXT .AND. SWODAT%TS_STOP_NEXT < 1D99) THEN
                                     !
                                     CALL SWODAT%PROJ(ID)%AllocDate%INIT( SWODAT%TS_STOP_NEXT )
                                     !
                                     SWODAT%PROJ(ID)%AllocDateFrac = SWODAT%PROJ(ID)%AllocDate%DYEAR_FRACTION()
                                  END IF
                              ELSE
                                  SWODAT%PROJ(ID)%AllocDateFrac = YEAR_FRACTION(SWODAT%TS_STOP_NEXT)
                              END IF
                          ELSE
                              SWODAT%PROJ(ID)%AllocClose = 0
                          END IF
                  CASE("DIST")
                          CALL NAM(I)%GET(THREE,PROP,ID3)
                          !
                          IF(ID2 < ONE .OR. SWODAT%NDIST < ID2) THEN
                              PROP(:) = BLNK
                              ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD DISTRIC ID NUMBER, IT IS EITHER >NDIST OR <1'//NL
                          ELSEIF (WBS%PROJ(ID)%NDIST == Z) THEN
                              PROP(:) = BLNK
                              ERROR=ERROR//VAR%NAM(POS)//' IS A PROJECT WITH NO DISTRICTS ASSOCIATED WITH IT'//NL
                          ELSEIF( ALL(ID2 .NE. WBS%PROJ(ID)%DIST) ) THEN
                              PROP(:) = BLNK
                              ERROR=ERROR//VAR%NAM(POS)//' HAS PROJECT '//NUM2STR(ID)//' THAT IS NOT ASSOCIATED IN THE HIERARCY WITH DISTRICT '//NUM2STR(ID2)//NL
                          END IF
                          !
                          SELECT CASE(PROP)
                          CASE("ALLOCATION")
                                                    !VAL = SWODAT%DIST(ID)%
                              CONTINUE
                          CASE(BLNK)
                                                   CONTINUE
                          CASE DEFAULT
                                      ERROR=ERROR//VAR%NAM(POS)//NL
                          END SELECT      
                          !
                  CASE("ADD_RELEASE")
                                      SWODAT%RESDAT(ID)%PROJ_RELEASE_ADD_S = VAR%VAL(POS)  !Note this is a RATE, converted to VOL later
                  CASE(BLNK)
                          CONTINUE     !ALREADY RAISED THE ERROR FOR BAD PROJ ID 
                  END SELECT
                  !
      CASE("REQ")
                  CALL NAM(I)%GET(TWO,PROP,ID)
                  !
                  SELECT CASE(PROP)
                  CASE("FLOW")
                              IF(SWODAT%REQFLOW%HAS_REQ) THEN
                                   ID = SWODAT%REQFLOW%POS(I)            !PREBUILT POINTER FROM SWO
                                   SWODAT%REQFLOW%REQ(ID) = VAR%VAL(POS)
                              ELSE
                                  ERROR=ERROR//VAR%NAM(POS)//' --UNKONWN FAILURE TO IDENTIFY REQ.FLOW VARIABLE.'//NL
                              END IF
                  CASE("DELIVERY")
                             CALL NAM(I)%GET(THREE,PROP,ID)
                             IF(ID < ONE .OR. SWODAT%NFARM < ID ) THEN
                                 ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD WBS/FARM ID NUMBER, IT IS EITHER >NWBS OR <1'//NL
                                 CYCLE
                             END IF
                             SELECT CASE(PROP)
                             CASE("FARM")
                                         SWODAT%FARM(ID)%REQ_DELIVERY_S = VAR%VAL(POS)
                             CASE DEFAULT
                                         ERROR=ERROR//VAR%NAM(POS)//NL
                             END SELECT
                  CASE("DELIVERY_VOL")
                             CALL NAM(I)%GET(THREE,PROP,ID)
                             IF(ID < ONE .OR. SWODAT%NFARM < ID ) THEN
                                 ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD WBS/FARM ID NUMBER, IT IS EITHER >NWBS OR <1'//NL
                                 CYCLE
                             END IF
                             SELECT CASE(PROP)
                             CASE("FARM")
                                         SWODAT%FARM(ID)%REQ_DELIVERY_VOL_S = VAR%VAL(POS)
                             CASE DEFAULT
                                         ERROR=ERROR//VAR%NAM(POS)//NL
                             END SELECT
                  CASE DEFAULT
                              ERROR=ERROR//VAR%NAM(POS)//NL
                  END SELECT  
                  !
      CASE("TRAN")
                  ID = NEG
                  IF(ALLOCATED(SWODAT%TRANS%POS)) THEN
                         ID = SWODAT%TRANS%POS(I)
                     IF( ID > Z) SWODAT%TRANS%RAT(ID) = VAR%VAL(POS)
                  END IF
                  IF(ID==NEG) ERROR=ERROR//VAR%NAM(POS)//NL
                  !
      CASE("SFR")
                  CALL NAM(I)%GET(TWO,PROP,ID2)
                  !
                  K = FDIM%SFR_ID%GET_POS(PROP)
                  !
                  IF(K > Z) THEN
                                           ID  = FDIM%SFR_ID%SEG_RCH(ONE,K)
                                           ID2 = FDIM%SFR_ID%SEG_RCH(TWO,K)
                                           IF(ID2 < ONE) ID2 = ONE
                                           CALL NAM(I)%GET(THREE,PROP,ID3)
                  ELSEIF(PROP=='INT' ) THEN
                                           CALL NAM(I)%GET(THREE,PROP,ID3)
                  ELSE
                      ID2 = NEG
                  END IF
                  !
                  IF(ID < ONE) THEN
                      IF(PROP=='INT') THEN
                          ERROR=ERROR//VAR%NAM(POS)//' HAS A BAD SEGMENT NUMBER (POSSIBLE ERROR IN VARIABLE NAME FORMAT WHICH SHOULD BE "SFR.SEG.RCH." OR "SFR.SFR_NAM."'//NL
                      ELSE
                          ERROR=ERROR//NL//VAR%NAM(POS)//' COULD NOT IDENTIFY THE SFR.SEG.RCH. OR SFR.SFR_NAM.'//NL//'THE PART OF THE VARIABLE NAME THAT HAD ISSES IS "'//TRIM(PROP)//'"'//NL//'THE SFR_NAM MAY NOT BE DEFINED IN THE FMP GLOBAL INPUT BOCK UNDER THE KEYWORD "SFR_NAMES".'//NL//'THE FOLLOWING IS A LIST OF CURRENTLY DEFINED SFR_NAMES:'//NL//JOIN_TXT(FDIM%SFR_ID%NAM,NL,BLN)//'NOTE THAT THIS ERROR COULD HAVE BEEN TRIGGERED IF YOU MEANT TO LOAD A SPECIFIED SEGMENT AND REACH AND THAT FAILED TO LOAD.'//NL
                      END IF
                  ELSE
                      SELECT CASE(PROP)
                      CASE("DIV")
                                  DO J=ONE, SWODAT%S_SFR_SPEC_DIV%N
                                      IF (I == SWODAT%S_SFR_SPEC_DIV%POS(J)) THEN
                                          !
                                          SWODAT%S_SFR_SPEC_DIV%DIV(J) = VAR%VAL(POS)
                                          !
                                          EXIT
                                      END IF
                                  END DO
                      CASE DEFAULT
                                  ERROR=ERROR//VAR%NAM(POS)//NL
                      END SELECT
                  END IF
      CASE DEFAULT
                  ERROR=ERROR//VAR%NAM(POS)//NL
      END SELECT
      !
  END DO
      
END SUBROUTINE
! CASE("DISTRICT")
!                  CALL NAM(I)%GET(TWO,PROP,ID2)
!                  !
!                  SELECT CASE(PROP)
!                  CASE("TOTAL_AREA")
!                                            VAL = SWODAT%DIST(ID)%AreaTot
!                  CASE("IRRIGATED_AREA")
!                                            VAL = SWODAT%DIST(ID)%AreaIrr
!                  CASE("ALLOCATION")
!                                            VAL = SWODAT%DIST(ID)%
    
    
    

! IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN
!     BLOCK 
!         TYPE(DATE_OPERATOR):: DATE
!         CALL DATE%INIT(REALTIM_PER)
!         CALL DATE%ADD_DAY( DNEG * REALTIM_PER )  !SUBTRACT OUT PERIOD LENGTH
!         !
!         DYEAR = DATE%DYEAR
!     END BLOCK
    
! DUBLICATES:
!
!      CASE("PROJECT")
!                       CALL NAM(I)%GET(TWO,PROP,ID2)
!                       !
!                       SELECT CASE(PROP)
!                       CASE("CLOSEOUT") !NOTE CAN BE BOTH A PROPERTY AND RETURN VARIABLE, NOT RECOMENDED TO DO BOTH AT ONCE
    
    
    
    
!CALL NAM(I)%GET(THREE,PROP,ID2)
!!
!K = FDIM%SFR_ID%POS(PROP)
!!
!IF(K > Z) THEN
!                         ID  = FDIM%SFR_ID%SEG_RCH(ONE,K)
!                         ID2 = FDIM%SFR_ID%SEG_RCH(TWO,K)
!                         IF(ID2 < ONE) ID2 = ONE
!ELSEIF(ID2 < ONE ) THEN
!                         ID2 = ONE
!END IF