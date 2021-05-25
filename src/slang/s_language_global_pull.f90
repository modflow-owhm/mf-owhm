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
    USE FMP_GLOBAL  !FDIM, WBS
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
      CASE DEFAULT
                  ERROR=ERROR//VAR%NAM(POS)//NL
      END SELECT
      !
  END DO
      
END SUBROUTINE
