! 
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!
!   EQUATION_VARIABLE_LIST
!
!   S_LANGUAGE_INTERFACE
!
MODULE EQUATION_VARIABLE_LIST
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE CONSTANTS
  USE EquationParser
  USE LOOKUP_TABLE_INSTRUCTION
  USE ARRAY_DATA_TYPES,                  ONLY: CHARACTER_ARRAY, INTEGER_VECTOR
  USE PARSE_WORD_INTERFACE,              ONLY: PARSE_WORD, PARSE_WORD_UP
  USE STRINGS,                           ONLY: UPPER, GET, GET_INTEGER
  USE ERROR_INTERFACE,                   ONLY: STOP_ERROR, WARNING_MESSAGE
  USE NUM2STR_INTERFACE,                 ONLY: NUM2STR
  USE GENERIC_INPUT_FILE_INSTRUCTION,    ONLY: GENERIC_INPUT_FILE
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,   ONLY: GENERIC_OUTPUT_FILE
  USE LINKED_LIST_INSTRUCTION,           ONLY: CHARACTER_LINKED_LIST
  USE GENERIC_BLOCK_READER_INSTRUCTION,  ONLY: GENERIC_BLOCK_READER
  USE DATE_OPERATOR_INSTRUCTION,         ONLY: DATE_OPERATOR
  USE TIME_SERIES_INSTRUCTION,           ONLY: TIME_SERIES_FILE, LOAD_TIME_SERIES_BLOCK
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: VARIABLE_LIST
  !
  TYPE VARIABLE_LIST
      INTEGER:: N=Z
      INTEGER:: IOUT=Z
      CHARACTER(:),    DIMENSION(:),ALLOCATABLE:: NAM
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: VAL
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: BAK
      !
      INTEGER:: NTAB = Z, NTSF = Z
      TYPE(LOOKUP_TABLE_TYPE),   DIMENSION(:), ALLOCATABLE:: TAB
      TYPE(TIME_SERIES_FILE),    DIMENSION(:), ALLOCATABLE:: TSF
      !
      ! OUTPUT OPTIONS
      INTEGER:: NFIL = Z
      CHARACTER(:),              DIMENSION(:), ALLOCATABLE:: OUT_ID
      TYPE(CHARACTER_ARRAY),     DIMENSION(:), ALLOCATABLE:: OUT_NAM
      TYPE(INTEGER_VECTOR),      DIMENSION(:), ALLOCATABLE:: OUT_LOC
      TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:), ALLOCATABLE:: OUTPUT
      TYPE(INTEGER_VECTOR),      DIMENSION(:), ALLOCATABLE:: OUT_PAD
      
      !
      CONTAINS
      PROCEDURE, PASS(VAR):: INIT    => INIT_VARIABLES!(CHAR_LEN, NVAR, [NTAB], [OUT_LEN], [NFIL])
      PROCEDURE, PASS(VAR):: SOLVE_LOGICAL!(LINE, ERRMSG)
      PROCEDURE, PASS(VAR):: SOLVE_EQUATION! (LINE, [IDX], [ERRMSG])
      PROCEDURE, PASS(VAR):: SET     => SET_VARIABLE_VALUE
      PROCEDURE, PASS(VAR):: GET     => GET_VARIABLE_VALUE
      PROCEDURE, PASS(VAR):: GET_POS => GET_VARIABLE_POSITION !(NAM)
      PROCEDURE, PASS(VAR):: BACKUP  => BACKUP_VARIABLES
      PROCEDURE, PASS(VAR):: RESTORE => RESTORE_VARIABLES
      PROCEDURE, PASS(VAR):: LOOKUP => GET_LOOKUP_TABLE!(TABNAM,X)  RESULT(Y)
      PROCEDURE, PASS(VAR):: TABLE_TO_VARIABLE => LOOKUP_TABLE_TO_VARIABLE!(VAR,TABNAM,VARLOOKUP,VARSET)
      PROCEDURE, PASS(VAR):: LOAD_VARIABLE_LIST!(BL,GET_VAL)
      PROCEDURE, PASS(VAR):: LOAD_OUTPUT_BLOCK
      PROCEDURE, PASS(VAR):: BUILD_OUTPUT_INDEX
      PROCEDURE, PASS(VAR):: BUILD_OUTPUT_HEADER
      PROCEDURE, PASS(VAR):: LOAD_LOOKUP_BLOCK
      PROCEDURE, PASS(VAR):: LOAD_TIME_SERIES_BLOCK => LOAD_TIME_SERIES_VARIABLE_BLOCK
      PROCEDURE, PASS(VAR):: PRINT   =>PRINT_SPECIFIED_OUTPUT_VARIABLE!(OUT_ID,[LN])
      PROCEDURE, PASS(VAR)::           GET_OUTPUT_VARIABLE_IU!(OUT_ID, IU)
      PROCEDURE, PASS(VAR):: WARN    =>PRINT_WARNING  !(OUT_ID,LN)
      PROCEDURE, PASS(VAR):: PRINT_VAR_NAM => PRINT_VARIABLE_NAMES
      PROCEDURE, PASS(VAR):: PRINT_OUT_NAM => PRINT_OUTPUT_VARIABLE_NAMES
      GENERIC::              PRINT_VAR     => PRINT_VARIABLE_LIST, PRINT_VARIABLE_LIST_SPECIFY
      PROCEDURE, PASS(VAR):: PRINT_VARIABLE_LIST
      PROCEDURE, PASS(VAR):: PRINT_VARIABLE_LIST_SPECIFY
      FINAL:: FINAL_DEALLOCATE_VARIABLE_LIST
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE FINAL_DEALLOCATE_VARIABLE_LIST(VAR)
    TYPE(VARIABLE_LIST), INTENT(INOUT):: VAR
    CALL DEALLOCATE_VARIABLE_LIST(VAR)
  END SUBROUTINE
  !
  FUNCTION SOLVE_LOGICAL(VAR, LINE, ERRMSG) RESULT(COND)
    CLASS(VARIABLE_LIST),                INTENT(INOUT):: VAR
    CHARACTER(*),                        INTENT(IN   ):: LINE
    CHARACTER(:), OPTIONAL, ALLOCATABLE, INTENT(INOUT):: ERRMSG  !OPTIONAL AND IS SET TO NON-BLANK IF ERR OCCURED
    LOGICAL:: COND
    !
    IF    (LINE=='true') THEN
        COND = TRUE
    ELSEIF(LINE=='false') THEN
        COND = FALSE
    ELSE
        COND = EVAL_CONDITION(LINE,VAR%NAM,VAR%VAL,ERRMSG=ERRMSG)
    END IF
    !
  END FUNCTION
  !
  SUBROUTINE SOLVE_EQUATION(VAR, LINE, ERRMSG, IDX)  !IDX IS INDEX THAT WAS SET FROM EQUATION (VIZ VARIABLE LOCATION)
    CLASS(VARIABLE_LIST),                INTENT(INOUT):: VAR
    CHARACTER(*),                        INTENT(IN   ):: LINE
    CHARACTER(:), OPTIONAL, ALLOCATABLE, INTENT(INOUT):: ERRMSG  !OPTIONAL AND IS SET TO NON-BLANK IF ERR OCCURED
    INTEGER,      OPTIONAL,              INTENT(INOUT):: IDX
    !
    CALL EVAL_EQUATION(LINE,VAR%NAM,VAR%VAL,IDX=IDX, ERRMSG=ERRMSG)
    !
  END SUBROUTINE
  !
  SUBROUTINE SET_VARIABLE_VALUE(VAR, NAM, VAL, POS)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    CHARACTER(*),      INTENT(IN   ):: NAM
    DOUBLE PRECISION,  INTENT(IN   ):: VAL
    INTEGER, OPTIONAL, INTENT(INOUT):: POS  !IF ZERO THEN RETURNS THE POSITION, ELSE LOADS AT POSITION POS
    INTEGER:: I
    LOGICAL:: NOT_FOUND
    !
    NOT_FOUND = TRUE
    !
    IF(PRESENT(POS)) THEN
                     IF(POS > Z) THEN
                                     VAR%VAL(I)=VAL
                                     NOT_FOUND=FALSE
                     ELSE
                                     DO I=ONE, VAR%N
                                         IF (NAM==VAR%NAM(I)) THEN
                                                                 VAR%VAL(I)=VAL
                                                                 POS = I
                                                                 NOT_FOUND=FALSE
                                                                 EXIT
                                         END IF
                                     END DO
                     END IF
    ELSE
         DO I=ONE, VAR%N
             IF (NAM==VAR%NAM(I)) THEN
                                     VAR%VAL(I)=VAL
                                     NOT_FOUND=FALSE
                                     EXIT
             END IF
         END DO
    END IF
    !
    IF(NOT_FOUND) THEN
        CALL STOP_ERROR( OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE SET_VARIABLE_VALUE) FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST.'//NL//'THIS SUBROUTINE IS USED TO SET A VALUE TO A LIST OF VARIABLES USED FOR PARSING AN EQUATION OR LOOKUP TABLE.'//NL//'IF ITS AN ERROR THEN A REQUESTED VARIABLE NAME WAS NOT PRESPECIFIED.'//NL//'THE NAME BEING SEARCHED FOR IS: "'//TRIM(NAM)//'"' )
        !WRITE(*,'(/A,/A,/A)') 'VARIABLE LIST ERROR: (SUBROUTINE SET_VARIABLE_VALUE) FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST.','THIS SUBROUTINE IS USED TO SET A VALUE TO A LIST OF VARIABLES USED FOR PARSING AN EQUATION. IF ITS AN ERROR THEN A REQUESTED VARIABLE NAME WAS NOT PRESPECIFIED.','THE NAME BEING SEARCHED FOR IS: "'//TRIM(NAM)//'"' 
        !ERROR STOP
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL FUNCTION GET_VARIABLE_VALUE(VAR, NAM, POS, READVAL) RESULT(VAL)
    CLASS(VARIABLE_LIST), INTENT(IN):: VAR
    CHARACTER(*),         INTENT(IN):: NAM
    INTEGER, OPTIONAL,    INTENT(IN):: POS
    LOGICAL, OPTIONAL,    INTENT(IN):: READVAL
    DOUBLE PRECISION:: VAL
    INTEGER:: I
    LOGICAL:: NOT_FOUND
    !
    NOT_FOUND = TRUE
    !
    IF(PRESENT(POS)) THEN
                     IF(POS > Z) THEN
                                     VAL=VAR%VAL(POS)
                                     NOT_FOUND=FALSE
                     ELSE
                                     DO I=ONE, VAR%N
                                         IF (NAM==VAR%NAM(I)) THEN
                                                                  VAL=VAR%VAL(I)
                                                                  NOT_FOUND=FALSE
                                                                  EXIT
                                         END IF
                                     END DO
                     END IF
    ELSE
        DO I=ONE, VAR%N
            IF (NAM==VAR%NAM(I)) THEN
                                     VAL=VAR%VAL(I)
                                     NOT_FOUND=FALSE
                                     EXIT
            END IF
        END DO
    END IF
    !
    IF(NOT_FOUND) THEN
        I = NEG
        IF(PRESENT(READVAL)) THEN; IF(READVAL) READ(NAM,*,IOSTAT=I) VAL
        END IF
        !
        IF(I.NE.Z) VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    END IF
    !
  END FUNCTION
  !
  PURE ELEMENTAL SUBROUTINE DEALLOCATE_VARIABLE_LIST(VAR)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    !
    IF (ALLOCATED(VAR%NAM))  DEALLOCATE(VAR%NAM)
    IF (ALLOCATED(VAR%VAL))  DEALLOCATE(VAR%VAL)
    IF (ALLOCATED(VAR%BAK))  DEALLOCATE(VAR%BAK)
    !
    IF (ALLOCATED(VAR%TAB))     DEALLOCATE(VAR%TAB)
    IF (ALLOCATED(VAR%OUT_ID))  DEALLOCATE(VAR%OUT_ID)
    IF (ALLOCATED(VAR%OUT_NAM)) DEALLOCATE(VAR%OUT_NAM)
    IF (ALLOCATED(VAR%OUT_LOC)) DEALLOCATE(VAR%OUT_LOC)
    IF (ALLOCATED(VAR%OUT_PAD)) DEALLOCATE(VAR%OUT_PAD)
    IF (ALLOCATED(VAR%OUTPUT))  DEALLOCATE(VAR%OUTPUT)
    !
    VAR%N=Z
    VAR%IOUT=Z
    VAR%NTAB = Z
    VAR%NFIL = Z
    VAR%NTSF = Z
    !
  END SUBROUTINE  
  !
  SUBROUTINE INIT_VARIABLES(VAR, CHAR_LEN, NVAR, NTAB, OUT_LEN, NFIL, NTSF, IOUT)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    INTEGER,              INTENT(IN   ):: CHAR_LEN, NVAR
    INTEGER, OPTIONAL,    INTENT(IN   ):: NTAB, OUT_LEN, NFIL, NTSF, IOUT
    !
    CALL DEALLOCATE_VARIABLE_LIST(VAR)
    !
    IF(PRESENT(IOUT)) VAR%IOUT = IOUT
    !
    IF(NVAR > Z) THEN
       VAR%N = NVAR
       ALLOCATE(CHARACTER(CHAR_LEN)::VAR%NAM(NVAR))
       ALLOCATE(VAR%VAL(NVAR), SOURCE=DZ)
    END IF
    !
    IF(PRESENT(NTAB)) THEN
        IF(NTAB>Z) THEN
                  VAR%NTAB = NTAB
                  ALLOCATE(VAR%TAB(NTAB))
        END IF
    END IF
    !
    IF(PRESENT(OUT_LEN) .AND. PRESENT(NFIL)) THEN
        IF(NFIL>Z .AND. OUT_LEN>Z) THEN
                  VAR%NFIL = NFIL
                  ALLOCATE(CHARACTER(OUT_LEN)::VAR%OUT_ID(NFIL))
                  ALLOCATE(VAR%OUT_NAM                   (NFIL))
                  ALLOCATE(VAR%OUT_LOC                   (NFIL))
                  ALLOCATE(VAR%OUT_PAD                   (NFIL))
                  ALLOCATE(VAR%OUTPUT                    (NFIL))
                  
        END IF
    END IF
    !
    IF(PRESENT(NTSF)) THEN
        VAR%NTSF = NTSF
        IF(NTSF>Z) ALLOCATE(VAR%TSF(NTSF))
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE RESTORE_VARIABLES(VAR)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    VAR%VAL = VAR%BAK
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE BACKUP_VARIABLES(VAR)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    IF(.NOT. ALLOCATED(VAR%BAK)) THEN
                                       ALLOCATE(VAR%BAK, SOURCE=VAR%VAL)
    ELSEIF(SIZE(VAR%BAK).NE.VAR%N) THEN
                                       DEALLOCATE(VAR%BAK)
                                       ALLOCATE(VAR%BAK, SOURCE=VAR%VAL)
    ELSE                               
                                       VAR%BAK = VAR%VAL
    END IF
  END SUBROUTINE
  !
  PURE FUNCTION PRINT_VARIABLE_LIST(VAR) RESULT(LN)
    CLASS(VARIABLE_LIST), INTENT(IN):: VAR
    CHARACTER(:),        ALLOCATABLE:: LN
    INTEGER:: I, P
    CHARACTER(16):: IDX
    CHARACTER(:),ALLOCATABLE:: HED
    !
    ALLOCATE(CHARACTER(LEN(VAR%NAM))::HED)
    HED(:) = 'VARNAM'
    LN = BLN//HED//'   VARVAL  [INDEX]'
    DEALLOCATE(HED)
    !
    IF(VAR%N>Z) THEN
        P = Z
        IDX = BLNK
        DO I=ONE, VAR%N
            !
            IF(I == ONE) THEN
                IF( VAR%NAM(I) == VAR%NAM(I+1) ) THEN
                    P = P + ONE
                    IDX = '    ['//NUM2STR(P)//']'
                ENDIF
            ELSEIF(I == VAR%N) THEN
                IF( VAR%NAM(I-1) == VAR%NAM(I) ) THEN
                    P = P + ONE
                    IDX = '    ['//NUM2STR(P)//']'
                ELSE
                    P = Z
                    IDX = BLNK
                ENDIF
            ELSE
                IF( VAR%NAM(I-1) == VAR%NAM(I) .OR. VAR%NAM(I) == VAR%NAM(I+1) ) THEN
                    P = P + ONE
                    IDX = '    ['//NUM2STR(P)//']'
                ELSE
                    P = Z
                    IDX = BLNK
                ENDIF
            ENDIF
            !
            LN = LN//NL//VAR%NAM(I)//' = '//NUM2STR(VAR%VAL(I),14)//TRIM(IDX)
        END DO
        LN = LN//NL
    ELSE
          LN = BLN//'NO VARIABLES LOADED??? -- THIS SHOULD NOT HAPPEN!!!'//BLN
    END IF
    !
  END FUNCTION
  !
  FUNCTION PRINT_VARIABLE_LIST_SPECIFY(VAR,LINE) RESULT(LN)
    CLASS(VARIABLE_LIST), INTENT(IN):: VAR
    CHARACTER(*),         INTENT(IN):: LINE
    CHARACTER(:),        ALLOCATABLE:: LN
    INTEGER:: I, LINE_LEN, LLOC, ISTART, ISTOP,ILB,IRB, P
    CHARACTER(:),ALLOCATABLE:: HED
    CHARACTER(16):: IDX
    !
    IF(LINE==BLNK) THEN
        LN = PRINT_VARIABLE_LIST(VAR)
    ELSE
        !
        ALLOCATE(CHARACTER(LEN(VAR%NAM))::HED)
        HED(:) = 'VARNAM'
        LN = BLN//HED//'   VARVAL  [INDEX]'
        DEALLOCATE(HED)
        !
        IF(VAR%N>Z) THEN
            LLOC = ONE
            LINE_LEN = LEN(LINE) + 1
            CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
            DO WHILE (LLOC < LINE_LEN)
                !
                IDX = BLNK
                !
                ILB = INDEX(LINE(ISTART:ISTOP), '[')
                IF(ILB > Z) THEN
                    !
                    I = GET_VARIABLE_POSITION(VAR, LINE(ISTART:ISTART+ILB-2))
                    !
                    IF(I > Z) THEN
                       ILB = ISTART + ILB
                       IRB = ISTART + INDEX(LINE(ISTART:ISTOP), ']') - TWO
                       !
                       P = NINT( EVAL(LINE(ILB:IRB),VAR%NAM,VAR%VAL) )
                       !
                       I = I + P - ONE
                       !
                       IDX = '    ['//NUM2STR(P)//']'
                    ENDIF
                ELSE
                    I = GET_VARIABLE_POSITION(VAR, LINE(ISTART:ISTOP))
                ENDIF
                IF(I > Z) THEN
                          LN = LN//NL//VAR%NAM(I)//' = '//NUM2STR(VAR%VAL(I))//TRIM(IDX)
                ELSE
                          LN = LN//NL//VAR%NAM(I)//' = ¿¿¿ UNKNOWN VARIABLE ???'
                END IF
                CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
            END DO
            LN = LN//NL
        ELSE
              LN = BLN//'NO VARIABLES LOADED??? -- THIS SHOULD NOT HAPPEN!!!'//BLN
        END IF
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION PRINT_VARIABLE_NAMES(VAR) RESULT(LN)
    CLASS(VARIABLE_LIST), INTENT(IN):: VAR
    CHARACTER(:),        ALLOCATABLE:: LN
    INTEGER:: I, P
    CHARACTER(8):: IDX
    !
    LN = BLN//'VARNAM'
    IF(VAR%N>Z) THEN
        P = Z
        DO I=ONE, VAR%N
            !
            IF(I == ONE) THEN
                IF( VAR%NAM(I) == VAR%NAM(I+1) ) THEN
                    P = P + ONE
                    IDX = '    ['//NUM2STR(P)//']'
                ENDIF
            ELSEIF(I == VAR%N) THEN
                IF( VAR%NAM(I-1) == VAR%NAM(I) ) THEN
                    P = P + ONE
                    IDX = '    ['//NUM2STR(P)//']'
                ELSE
                    P = Z
                    IDX = BLNK
                ENDIF
            ELSE
                IF( VAR%NAM(I-1) == VAR%NAM(I) .OR. VAR%NAM(I) == VAR%NAM(I+1) ) THEN
                    P = P + ONE
                    IDX = '    ['//NUM2STR(P)//']'
                ELSE
                    P = Z
                    IDX = BLNK
                ENDIF
            ENDIF
            LN = LN//NL//VAR%NAM(I)//TRIM(IDX)
        END DO
        LN = LN//NL
    ELSE
          LN = BLN//'NO VARIABLES LOADED??? -- THIS SHOULD NOT HAPPEN!!!'//BLN
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION PRINT_OUTPUT_VARIABLE_NAMES(VAR) RESULT(LN)
    CLASS(VARIABLE_LIST), INTENT(IN):: VAR
    CHARACTER(:),        ALLOCATABLE:: LN
    INTEGER:: I
    !
    LN = BLN//'OUTPUT_VARIABLE_NAME'
    IF(VAR%NFIL>Z) THEN
        DO I=ONE, VAR%NFIL
          LN = LN//NL//VAR%OUT_ID(I)
        END DO
        LN = LN//NL
    ELSE
          LN = BLN//'NO OUTPUT VARIABLES LOADED??? -- THIS SHOULD NOT HAPPEN!!!'//BLN
    END IF
    !
    END FUNCTION
    !
  PURE FUNCTION PRINT_INTEGER_VARIABLE(NAM) RESULT(ANS)
    CHARACTER(*),INTENT(IN):: NAM
    LOGICAL:: ANS
    !
    IF(      NAM == 'SOLVER_ITERATION'&      
        .OR. NAM == 'SP.NUM'         &
        .OR. NAM == 'TS.NUM'         &
        .OR. NAM == 'TS.START.DAY'   &
        .OR. NAM == 'TS.START.MONTH' &
        .OR. NAM == 'TS.START.YEAR'  &
        .OR. NAM == 'TS.START.DOY'   &
        .OR. NAM == 'TS.END.DAY'     &
        .OR. NAM == 'TS.END.MONTH'   &
        .OR. NAM == 'TS.END.YEAR'    &
        .OR. NAM == 'TS.END.DOY'     &
        .OR. NAM == 'SP.START.DAY'   &
        .OR. NAM == 'SP.START.MONTH' &
        .OR. NAM == 'SP.START.YEAR'  &
        .OR. NAM == 'SP.START.DOY'   &
        .OR. NAM == 'SP.END.DAY'     &
        .OR. NAM == 'SP.END.MONTH'   &
        .OR. NAM == 'SP.END.YEAR'    &
        .OR. NAM == 'SP.END.DOY'     &
    ) THEN
          ANS = TRUE
    ELSEIF(      INDEX(NAM, '.CLOSEOUT' ) > Z     &
            .OR. INDEX(NAM, '.INIT_FLAG') > Z     &
    ) THEN
          ANS = TRUE
    ELSE
          ANS = FALSE
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION PRINT_FLOAT_VARIABLE(NAM) RESULT(ANS)
    CHARACTER(*),INTENT(IN):: NAM
    LOGICAL:: ANS
    !
    IF(      NAM == 'TS.START.DYEAR'     &
        .OR. NAM == 'TS.START.YEAR_FRAC' &
        .OR. NAM == 'TS.START.DAY_FRAC'  &
        .OR. NAM == 'TS.END.DYEAR'       &
        .OR. NAM == 'TS.END.YEAR_FRAC'   &
        .OR. NAM == 'TS.END.DAY_FRAC'    &
        .OR. NAM == 'SP.START.DYEAR'     &
        .OR. NAM == 'SP.START.YEAR_FRAC' &
        .OR. NAM == 'SP.START.DAY_FRAC'  &
        .OR. NAM == 'SP.END.DYEAR'       &
        .OR. NAM == 'SP.END.YEAR_FRAC'   &
        .OR. NAM == 'SP.END.DAY_FRAC'    &
    ) THEN
          ANS = TRUE
    ELSEIF(      INDEX(NAM, '.RELEASE_FRAC_RES' ) > Z     &
    ) THEN
          ANS = TRUE
    ELSE
          ANS = FALSE
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION PRINT_DATE_VARIABLE(NAM) RESULT(ANS)
    CHARACTER(*),INTENT(IN):: NAM
    LOGICAL:: ANS
    !
    ANS = FALSE
    !
    IF(      NAM == 'TS.START.DATE'  &          
        .OR. NAM == 'TS.END.DATE'    &    
        .OR. NAM == 'SP.START.DATE'  &    
        .OR. NAM == 'SP.END.DATE'    &
    ) THEN
          ANS = TRUE
    ELSEIF(LEN(NAM) > THREE) THEN
          IF    ( NAM(ONE:FOUR) == 'DATE' ) THEN
                                               ANS = TRUE
          ELSEIF( NAM(ONE:FOUR) == 'PROJ' ) THEN
              IF(INDEX(NAM,'.ALLOC_DATE') > Z) ANS = TRUE
          END IF
    END IF
    !
  END FUNCTION
  !
  SUBROUTINE GET_OUTPUT_VARIABLE_IU(VAR,OUT_ID,IU)
    CLASS(VARIABLE_LIST), INTENT(IN   ):: VAR
    CHARACTER(*),         INTENT(IN   ):: OUT_ID
    INTEGER,              INTENT(  OUT):: IU
    !LOGICAL, OPTIONAL,    INTENT(IN   ):: MSG
    INTEGER:: I,NFIL
    !
    IU = Z
    !
    IF(ALLOCATED(VAR%OUTPUT)) THEN
        NFIL=SIZE(VAR%OUTPUT,ONE)
    ELSE
        NFIL=Z
    END IF
    !
    IF(NFIL > Z) THEN
                DO I=ONE, NFIL
                             IF(OUT_ID==VAR%OUT_ID(I)) THEN
                                 IU = VAR%OUTPUT(I)%IU
                                 EXIT
                             END IF
                END DO
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_SPECIFIED_OUTPUT_VARIABLE(VAR,OUT_ID,LN)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    CHARACTER(*),         INTENT(IN   ):: OUT_ID
    CHARACTER(*),OPTIONAL,INTENT(IN   ):: LN
    !LOGICAL, OPTIONAL,    INTENT(IN   ):: MSG
    INTEGER:: I,J, NFIL
    TYPE(DATE_OPERATOR):: DATE
    !
    NFIL=SIZE(VAR%OUTPUT,ONE)
    IF(NFIL==Z .OR. .NOT. ALLOCATED(VAR%OUTPUT))  CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE ERROR: FAILED TO LOCATE UNIQUE FILE NAME TO WRITE OUTPUT TO.'//NL//'THIS LINE SHOULD HAVE THE KEYWORD PRINT FOLLOWED BY A FILENAME IDENTIFIED IN THE OUTPUT BLOCK.'//NL//'THE FOLLOWING WAS THE NAME REQUESTED: '//OUT_ID)
    !
    DO I=ONE, NFIL
        IF(OUT_ID==VAR%OUT_ID(I)) THEN
            IF(VAR%OUT_NAM(I)%N > Z) THEN
                IF(VAR%OUT_LOC(I)%VEC(ONE) > Z) THEN
                    !
                    IF(VAR%OUTPUT(I)%BINARY) THEN
                        DO J = ONE, VAR%OUT_LOC(I)%N
                          ASSOCIATE(VAL => VAR%VAL( VAR%OUT_LOC(I)%VEC(J) ) )
                            !
                            IF( (VAR%OUT_NAM(I)%STR(J) == 'DATE.START' .OR. VAR%OUT_NAM(I)%STR(J) == 'DATE.END') .AND. VAL == VAL ) THEN
                                CALL DATE%INIT( VAL )
                                WRITE(VAR%OUTPUT(I)%IU) DATE%STR('T')
                            ELSE
                                WRITE(VAR%OUTPUT(I)%IU) VAL
                            END IF
                          END ASSOCIATE
                        END DO
                    ELSE
                        DO J = ONE, VAR%OUT_LOC(I)%N
                            !ASSOCIATE( POS => VAR%OUT_LOC(I)%VEC(J), PAD => VAR%OUT_PAD(I)%VEC(J) )
                            ASSOCIATE( NAM => VAR%OUT_NAM(I)%STR(J),            &
                                       VAL => VAR%VAL( VAR%OUT_LOC(I)%VEC(J) ), &
                                       PAD => VAR%OUT_PAD(I)%VEC(J)             )
                               !
                               IF( PRINT_DATE_VARIABLE(NAM) ) THEN ! (VAR%OUT_NAM(I)%STR(J) == 'DATE.START' .OR. VAR%OUT_NAM(I)%STR(J) == 'DATE.END') THEN
                                   !
                                   IF(VAL.NE.VAL) THEN
                                       WRITE(VAR%OUTPUT(I)%IU,'(1x A)', ADVANCE='NO') NUM2STR( VAL, 24, 1 ) !High Precision String
                                   ELSE
                                       CALL DATE%INIT( VAL )
                                       IF(DATE%IS_SET()) THEN
                                           WRITE(VAR%OUTPUT(I)%IU,'(1x A24)', ADVANCE='NO') DATE%STR('T')
                                       ELSE
                                           WRITE(VAR%OUTPUT(I)%IU,'(1x A)', ADVANCE='NO') NUM2STR( VAL, PAD, 1 )  !High Precision String
                                       END IF
                                   END IF
                                   !
                               ELSEIF( PRINT_INTEGER_VARIABLE(NAM) ) THEN
                                   IF(VAL.NE.VAL) THEN
                                       WRITE(VAR%OUTPUT(I)%IU,'(1x A)', ADVANCE='NO') NUM2STR( VAL, PAD, 1 )  !High Precision String
                                   ELSE
                                       WRITE(VAR%OUTPUT(I)%IU,'(1x A)', ADVANCE='NO') NUM2STR( INT(VAL), PAD )
                                   END IF
                               ELSE
                                       WRITE(VAR%OUTPUT(I)%IU,'(1x A)', ADVANCE='NO') NUM2STR( VAL, PAD, 1 )  !High Precision String
                               END IF
                               !
                            END ASSOCIATE
                        END DO
                        WRITE(VAR%OUTPUT(I)%IU,'(A)')
                    END IF
                    !
                    !IF(VAR%OUTPUT(I)%BINARY) THEN
                    !    WRITE(VAR%OUTPUT(I)%IU) (VAR%VAL( VAR%OUT_LOC(I)%VEC(J) ), J=ONE, VAR%OUT_LOC(I)%N)
                    !ELSE
                    !    WRITE(VAR%OUTPUT(I)%IU,'(*(1x ES24.11))') (VAR%VAL( VAR%OUT_LOC(I)%VEC(J) ), J=ONE, VAR%OUT_LOC(I)%N)
                    !END IF
                ELSE
                    IF(VAR%OUTPUT(I)%BINARY) THEN
                        WRITE(VAR%OUTPUT(I)%IU) (VAR%GET(VAR%OUT_NAM(I)%STR(J)), J=ONE, VAR%OUT_NAM(I)%N)
                    ELSE
                        WRITE(VAR%OUTPUT(I)%IU,'(*(1x ES24.11))') (VAR%GET(VAR%OUT_NAM(I)%STR(J)), J=ONE, VAR%OUT_NAM(I)%N)
                    END IF
                END IF
            END IF
            EXIT
        END IF
        !
        IF(I==NFIL)  CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE ERROR: FAILED TO LOCATE UNIQUE FILE NAME (OUTPUT_VARIABLE NAME) TO WRITE OUTPUT TO.'//NL//'THIS LINE SHOULD HAVE THE KEYWORD PRINT FOLLOWED BY A FILENAME IDENTIFIED/OUTPUT_VARIABLE IN THE OUTPUT BLOCK.'//NL//'THE FOLLOWING WAS THE NAME REQUESTED:'//OUT_ID//NL//'WHICH WAS NOT LOCATED WITHIN THE FOLLOWING SPECIFIED OUTPUT NAMES:'//VAR%PRINT_OUT_NAM() )
    END DO
    !
    !IF(PRESENT(MSG)) THEN 
    !    IF(MSG .AND. .NOT. VAR%OUTPUT(I)%BINARY) THEN
    !                                                  WRITE(VAR%OUTPUT(I)%IU, '(3A)') 'ADDITIONAL MSG: "',TRIM(LN),'"'
    !    ELSEIF(MSG .AND.   VAR%OUTPUT(I)%BINARY) THEN
    !                                                  WRITE(VAR%IOUT, '(3A)') 'ADDITIONAL MSG: "',TRIM(LN),'"'
    !    END IF
    !END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_WARNING(VAR,OUT_ID,LN)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    CHARACTER(*),         INTENT(IN   ):: OUT_ID
    CHARACTER(*),         INTENT(IN   ):: LN
    INTEGER:: I, NFIL
    !
    NFIL=SIZE(VAR%OUTPUT,ONE)
    IF(NFIL==Z .OR. .NOT. ALLOCATED(VAR%OUTPUT))  CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE ERROR: FAILED TO LOCATE UNIQUE FILE NAME TO WRITE OUTPUT TO.'//NL//'THIS LINE SHOULD HAVE THE KEYWORD PRINT FOLLOWED BY A FILENAME IDENTIFIED IN THE OUTPUT BLOCK.'//NL//'THE FOLLOWING WAS THE NAME REQUESTED:'//OUT_ID)
    !
    DO I=ONE, NFIL
        IF(OUT_ID==VAR%OUT_ID(I)) THEN
            IF(VAR%OUTPUT(I)%BINARY) THEN
                WRITE(VAR%IOUT, '(3A)') 'WARNING: "',TRIM(LN),'"'
            ELSE
                WRITE(VAR%OUTPUT(I)%IU, '(3A)') 'WARNING: "',TRIM(LN),'"'
            END IF
            EXIT
        END IF
        !
        IF(I==NFIL)  CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE ERROR: FAILED TO LOCATE UNIQUE FILE NAME TO WRITE OUTPUT TO.'//NL//'THIS LINE SHOULD HAVE THE KEYWORD PRINT FOLLOWED BY A FILENAME IDENTIFIED IN THE OUTPUT BLOCK.'//NL//'THE FOLLOWING WAS THE NAME REQUESTED:'//OUT_ID//NL//'WHICH WAS NOT LOCATED WITHIN THE FOLLOWING SPECIFIED OUTPUT NAMES:'//VAR%PRINT_OUT_NAM() )
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_VARIABLE_LIST(VAR,BL,GET_VAL)
    CLASS(VARIABLE_LIST),        INTENT(INOUT):: VAR
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL
    LOGICAL,                     INTENT(IN   ):: GET_VAL
    !
    TYPE(DATE_OPERATOR):: DATE
    INTEGER:: I, J, LLOC, ISTART, ISTOP, MAX_LEN, IERR, ILB, IRB, NDIM, IDIM
    DOUBLE PRECISION:: TMP
    LOGICAL:: CHECK
    !
    IF(BL%NLINE > Z) THEN
       !
       CALL BL%START()
       !
       TMP     = DZ
       MAX_LEN = ONE
       NDIM = Z
       DO I=ONE, BL%NLINE !FIND MAX SIZE OF ALL VARIABLE NAMES
                       IF(GET_VAL) THEN
                                        LLOC = INDEX(BL%LINE, '=')
                                        IF(LLOC > Z) BL%LINE(LLOC:LLOC)=BLNK  !STRIP OUT = SIGN 
                                        !DO CONCURRENT (LLOC=ONE:LEN_TRIM(BL%LINE), BL%LINE(LLOC:LLOC)=='='); BL%LINE(LLOC:LLOC)=BLNK; END DO  !STRIP OUT ANY = SIGN IN ASSIGNEMENTS
                       END IF
                       !
                       ILB = INDEX(BL%LINE, '[')
                       !
                       LLOC=ONE
                       !
                       IF( ILB > Z ) THEN
                           !
                           ILB = ILB + 1
                           IRB = INDEX(BL%LINE, ']') - ONE
                           !
                           READ(BL%LINE(ILB:IRB),*, IOSTAT=IERR) IDIM !DIMENSION OF ARRAY VARIABLE
                           !
                           IF(IERR.NE.Z) CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'VARIABLE LIST BLOCK ERROR: (LOAD_VARIABLE_LIST) FOUND NEXT TO VARIABLE NAME BRACKETS, [ ], WHICH INDICATE IT IS AN ARRAY VARIABLE.'//NL//'BUT FAILED TO READ THE ARRAY DIMENION ['//BL%LINE(ILB:IRB)//'].'//NL//'THIS ERROR OCCURED WITHIN THE BLOCK NAMED "'//BL%NAME//'"')
                           !
                           ILB = ILB - 2
                           !
                           CALL PARSE_WORD(BL%LINE(:ILB),LLOC,ISTART,ISTOP)
                           !
                           NDIM = NDIM + IDIM  !COUNT FOR THE TOTAL DIMENSION
                       ELSE
                           CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP)
                           !
                           NDIM = NDIM + ONE
                       END IF
                       !
                       IF(ISTOP-ISTART+ONE > MAX_LEN) MAX_LEN = ISTOP-ISTART+ONE
                       !
                       CALL BL%NEXT()
       END DO
       !
       CALL INIT_VARIABLES(VAR, MAX_LEN, NDIM)
       !
       CALL BL%START()
       !
       I = Z  ! I is used in place of NDIM to be more compact
       !
       DO NDIM=ONE, BL%NLINE
                       !
                       ILB = INDEX(BL%LINE, '[')
                       !
                       LLOC=ONE
                       !
                       IF( ILB > Z ) THEN
                           !
                           ILB = ILB + 1
                           IRB = INDEX(BL%LINE, ']') - 1
                           !
                           READ(BL%LINE(ILB:IRB),*, IOSTAT=IERR) IDIM !DIMENSION OF ARRAY VARIABLE
                           !
                           IDIM = IDIM - ONE
                           !
                           ILB = ILB - 2
                           !
                           CALL PARSE_WORD(BL%LINE(:ILB),LLOC,ISTART,ISTOP)
                           !
                           LLOC = IRB + 2  !Set up poitner to start past ] for GET
                           !
                       ELSE
                           CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)  !MAKE UPCASE
                           IDIM = Z
                       END IF
                       !
                       I = I + ONE
                       !
                       VAR%NAM(I) = BL%LINE(ISTART:ISTOP)
                       !
                       READ(VAR%NAM(I),*, IOSTAT=IERR) TMP
                       IF(IERR==Z)  CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'VARIABLE LIST BLOCK ERROR: (LOAD_VARIABLE_LIST) FAILED TO LOAD VARIABLE NAME OR THE VARIABLE NAME IS A NUMBER, WHICH IS NOT ALLOWED. THE VARIABLE NAME THAT MUST BE CHANGED IS "'//TRIM(VAR%NAM(I))//'".'//NL//'THIS ERROR OCCURED WITHIN THE BLOCK NAMED "'//BL%NAME//'"')
                       !
                       IF(GET_VAL) THEN
                                      CALL GET(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,VAR%VAL(I),DATE,MSG='NOSTOP',ONLY_DYEAR=TRUE)!ONLY WANT TO GET T AS A DECIMAL YEAR.
                                      !
                                      IF(VAR%VAL(I).NE.VAR%VAL(I))  THEN
                                          CALL UPPER(BL%LINE(ISTART:ISTOP))
                                          IF(BL%LINE(ISTART:ISTOP)=='INF') THEN
                                              VAR%VAL(I) = D250
                                          ELSEIF(BL%LINE(ISTART:ISTOP)=='NINF') THEN
                                              VAR%VAL(I) = negD250
                                          ELSE
                                              CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'VARIABLE LIST BLOCK ERROR: (LOAD_VARIABLE_LIST) FAILED TO LOAD VARIABLE VALUE FOR VARIABLE "'//TRIM(VAR%NAM(I))//'".'//NL//'THE PART OF THE LINE THAT ATTEMPTED TO BE CONVERTED TO A NUMBER WAS: "'//BL%LINE(ISTART:ISTOP)//'"'//NL//'THIS ERROR OCCURED WITHIN THE BLOCK NAMED "'//BL%NAME//'"')
                                          END IF
                                      END IF
                       END IF
                       !
                       IF(IDIM > Z) THEN
                                    CHECK = GET_VAL
                                    DO J=ONE, IDIM
                                        I = I + ONE
                                        !
                                        VAR%NAM(I) = VAR%NAM(I-1)
                                        VAR%VAL(I) = VAR%VAL(I-1)
                                        !
                                        IF(CHECK) THEN
                                                       CALL GET(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,VAR%VAL(I),DATE,MSG='NOSTOP',ONLY_DYEAR=TRUE)!ONLY WANT TO GET T AS A DECIMAL YEAR.
                                                       !
                                                       IF(VAR%VAL(I).NE.VAR%VAL(I))  THEN
                                                           CALL UPPER(BL%LINE(ISTART:ISTOP))
                                                           IF(BL%LINE(ISTART:ISTOP)=='INF') THEN
                                                               VAR%VAL(I) = D250
                                                           ELSEIF(BL%LINE(ISTART:ISTOP)=='NINF') THEN
                                                               VAR%VAL(I) = negD250
                                                           ELSE
                                                               VAR%VAL(I) = VAR%VAL(I-1)
                                                               !
                                                               IF(J==ONE) THEN     !Only 1 value specified not all
                                                                   CHECK = FALSE
                                                               ELSE
                                                                  CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'VARIABLE LIST BLOCK ERROR: (LOAD_VARIABLE_LIST) FAILED TO LOAD VARIABLE VALUE FOR VARIABLE "'//TRIM(VAR%NAM(I))//'".'//NL//'IT WAS DETERMINED TO BE AN ARRAY, THAT WAS SET TO MORE THEN 1 NUMBER SO IT MUST SET THE ENTIRE ARRAY.'//NL//' FOR EXAMPLE Y[3] = 2 4 6 IS CORRECT, Y[3] = 2 4 IS WRONG.'//NL//'THIS ERROR OCCURED WITHIN THE BLOCK NAMED "'//BL%NAME//'"')
                                                               ENDIF
                                                           END IF
                                                       END IF
                                        END IF
                                    END DO
                       ENDIF
                       !
                       CALL BL%NEXT()
       END DO
    ELSE
       CALL INIT_VARIABLES(VAR, ONE, Z)
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE BUILD_OUTPUT_INDEX(VAR, ERR)
    CLASS(VARIABLE_LIST),      INTENT(INOUT):: VAR
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: ERR
    INTEGER:: I,J,NFIL, ILB, IRB, P, IERR
    !
    IF(ALLOCATED(ERR)) DEALLOCATE(ERR)
    ERR = BLNK
    !
    NFIL=SIZE(VAR%OUTPUT,ONE)
    IF(NFIL==Z .OR. .NOT. ALLOCATED(VAR%OUTPUT))  RETURN
    !
    DO  I=ONE, NFIL
      ASSOCIATE( NAM => VAR%OUT_NAM(I)%STR, POS => VAR%OUT_LOC(I)%VEC )
         DO J=ONE, VAR%OUT_NAM(I)%N
             !
             ILB = INDEX(NAM(J), '[')
             IF(ILB > Z) THEN
                 !
                 POS(J) = GET_VARIABLE_POSITION(VAR, NAM(J)(:ILB-1))
                 !
                 IF(POS(J) > Z) THEN
                    ILB = ILB + 1
                    IRB = INDEX(NAM(J), ']') - ONE
                    !
                    READ(NAM(J)(ILB:IRB),*, IOSTAT=IERR) P
                    !
                    IF(IERR.NE.Z) THEN
                        ERR = ERR//VAR%OUT_ID(I)//'   '//VAR%OUT_NAM(I)%STR(J)//NL
                    ELSE
                        POS(J) = POS(J) + P - ONE
                    ENDIF
                    !
                 ENDIF
             ELSE
                 POS(J) = GET_VARIABLE_POSITION(VAR, NAM(J))
             ENDIF
         END DO
      END ASSOCIATE
    END DO
    !
    DO I=ONE, NFIL
        DO J=ONE, VAR%OUT_NAM(I)%N       ! ONLY TRUE IF FAILED TO FIND VARIABLE
            IF(VAR%OUT_LOC(I)%VEC(J) == Z) ERR = ERR//VAR%OUT_ID(I)//'   '//VAR%OUT_NAM(I)%STR(J)//NL
        END DO
    END DO
    !
    IF(ERR == BLNK) THEN
       DO I=ONE, NFIL
           DO J=ONE, VAR%OUT_NAM(I)%N
               VAR%OUT_PAD(I)%VEC(J) = LEN_TRIM( VAR%OUT_NAM(I)%STR(J) )
               !
               IF(VAR%OUT_PAD(I)%VEC(J) < 24 ) VAR%OUT_PAD(I)%VEC(J) = 24
           END DO
           !
           DO J=ONE, VAR%OUT_NAM(I)%N
               ASSOCIATE(NAM => VAR%OUT_NAM(I)%STR(J), PAD => VAR%OUT_PAD(I)%VEC(J))
                  IF(           NAM == 'SOLVER_ITERATION' &
                     .OR. INDEX(NAM, '.CLOSEOUT' ) > Z    &
                     .OR. INDEX(NAM, '.INIT_FLAG') > Z    ) THEN
                     !
                     PAD = 19
                     !
                  ELSEIF(PRINT_INTEGER_VARIABLE(NAM)) THEN
                     PAD = 14
                  END IF
               END ASSOCIATE
           END DO
       END DO
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE BUILD_OUTPUT_HEADER(VAR)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    INTEGER:: I,J,NFIL
    CHARACTER(:),ALLOCATABLE:: HED
    !
    NFIL=SIZE(VAR%OUTPUT,ONE)
    IF(NFIL==Z .OR. .NOT. ALLOCATED(VAR%OUTPUT))  RETURN
    !
    DO I=ONE, NFIL
        !
        IF(VAR%OUT_NAM(I)%N > Z) THEN
           HED=''
           DO J=ONE, VAR%OUT_NAM(I)%N
               ASSOCIATE( N => VAR%OUT_PAD(I)%VEC(J), NAM => VAR%OUT_NAM(I)%STR(J) )
                  !
                  HED = HED//REPEAT(BLNK,N-LEN_TRIM(NAM)+1)//TRIM(NAM)
                  !
               END ASSOCIATE
           END DO
       ELSE
                  HED = BLN//'  NO VARIABLES DEFINED FOR '//TRIM(VAR%OUT_ID(I))//BLN
       END IF
       !
       IF(VAR%OUTPUT(I)%BINARY) THEN
           BLOCK
               CHARACTER(250):: FIL
               INQUIRE(VAR%OUTPUT(I)%IU,NAME=FIL)
               IF(VAR%IOUT.NE.Z) THEN
                                 WRITE(VAR%IOUT,'(A, /A, /2A)') 'VARIABLE LIST OUTPUT WRITTEN TO BINARY FILE:',TRIM(FIL),'EACH RECORD WILL HAVE THE FOLLOWING VARIABLES AS DOUBLE PREICISION BINARY:',HED
               ELSE
                                 WRITE(*,       '(A, /A, /2A)') 'VARIABLE LIST OUTPUT WRITTEN TO BINARY FILE:',TRIM(FIL),'EACH RECORD WILL HAVE THE FOLLOWING VARIABLES AS DOUBLE PREICISION BINARY:',HED
               END IF
           END BLOCK
       ELSE
           CALL VAR%OUTPUT(I)%SET_HEADER(HED)
       END IF
       !
       DEALLOCATE(HED)
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_OUTPUT_BLOCK(VAR,BL)
    CLASS(VARIABLE_LIST),        INTENT(INOUT):: VAR
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL
    !
    INTEGER:: I, J, LLOC, ISTART, ISTOP, MAX_LEN
    TYPE(CHARACTER_LINKED_LIST):: VAR_OUT
    CHARACTER(:), ALLOCATABLE:: TMP
    !
    IF(BL%NLINE > Z) THEN
        CALL BL%START()
        !
        MAX_LEN = ONE
        DO I=ONE, BL%NLINE
                        LLOC=ONE
                        CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP)
                        !
                        IF(ISTOP-ISTART+ONE > MAX_LEN) MAX_LEN = ISTOP-ISTART+ONE
                        !
                        CALL BL%NEXT()
        END DO
        !
        VAR%NFIL = BL%NLINE
        ALLOCATE(CHARACTER(MAX_LEN)::VAR%OUT_ID(VAR%NFIL))
        ALLOCATE(VAR%OUTPUT(BL%NLINE))
        ALLOCATE(VAR%OUT_NAM(BL%NLINE))
        ALLOCATE(VAR%OUT_LOC(BL%NLINE))
        ALLOCATE(VAR%OUT_PAD(BL%NLINE))
        !
        CALL BL%START()
        !
        DO I=ONE, BL%NLINE
                        LLOC=ONE
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)  !MAKE UPCASE
                        !
                        VAR%OUT_ID(I) = BL%LINE(ISTART:ISTOP)
                        !
                        J = INDEX(BL%LINE,'=>')
                        IF(J<ONE) CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'OUTPUT BLOCK ERROR:'//NL//'FOUND OUTPUT FILE ID, BUT FAILED TO LOCATE ON LINE THE FILE POINTER SYMBOL "=>"'//NL//'THIS IS REQUIRED TO DETRMINE LAST PRINT VARIABLE NAME AND THE FILE TO WRITE TOO.')
                        CALL VAR_OUT%INIT()
                        J = J - ONE
                        DO WHILE (LLOC < J)
                            CALL PARSE_WORD(BL%LINE(:J),LLOC,ISTART,ISTOP)
                            !
                            IF(ISTART<=ISTOP) THEN
                                ALLOCATE(TMP, SOURCE=BL%LINE(ISTART:ISTOP))   ! VAR_OUT%ADD DOES NOT ACCEPT BL%LINE AS INPUT BECAUSE IT IS A POINTER TO A POLYMORPHIC, SO IT COULD NOT IDENTIFY. INSTEAD CREATE TMP VARIABLE
                                CALL UPPER(TMP)
                                IF(TMP.NE.BLNK) CALL VAR_OUT%ADD(TMP)
                                DEALLOCATE(TMP)
                            END IF
                        END DO
                        !
                        LLOC = J + THREE
                        !
                        !!!!LINE_LEN = LEN(BL%LINE)
                        !!!!DO
                        !!!!    CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP)
                        !!!!    IF(BL%LINE(ISTART:ISTOP) == '=>') EXIT
                        !!!!    !
                        !!!!    IF(SCAN(BL%LINE(ISTART:ISTOP),'=>') > Z) CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'OUTPUT BLOCK ERROR:'//NL//'FOUND OUTPUT FILE ID, BUT THERE MUST BE AT LEAST ONE BLANK SPACE BEFORE AND AFTER THE "=>"'//NL//'FOR OUTPUT BLOCK TO DETRMINE LAST VARIABLE NAME AND FILE TO WRITE TOO.'//NL//'THE FOLLOWING IS THE WORD THAT WAS PARSED THAT NEEDS TO BE FIXED: "'//BL%LINE(ISTART:ISTOP)//'"')
                        !!!!    !
                        !!!!    IF(LLOC > LINE_LEN)  CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'OUTPUT BLOCK ERROR: FOUND OUTPUT FILE ID, BUT THERE MUST BE AFTER A LIST OF VARIABLE NAMES A " => "'//NL//'FOLLOWED BY THE FILE THAT THE OUTPUT WILL BE WRITTEN TOO.'//NL//'FAILED TO LOCATE A " => " WITHIN THE LINE.')
                        !!!!    !
                        !!!!    ALLOCATE(TMP, SOURCE=BL%LINE(ISTART:ISTOP))   ! VAR_OUT%ADD DOES NOT ACCEPT BL%LINE AS INPUT BECAUSE IT IS A POINTER TO A POLYMORPHIC, SO IT COULD NOT IDENTIFY. INSTEAD CREATE TMP VARIABLE
                        !!!!    CALL UPPER(TMP)
                        !!!!    CALL VAR_OUT%ADD(TMP)
                        !!!!    DEALLOCATE(TMP)
                        !!!!    !
                        !!!!END DO
                        !
                        CALL VAR%OUTPUT(I)%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11)
                        !
                        IF(VAR_OUT%LEN() > Z) THEN
                            CALL VAR_OUT%TOARRAY(VAR%OUT_NAM(I)%STR)
                            VAR%OUT_NAM(I)%N = SIZE(VAR%OUT_NAM(I)%STR,ONE)
                            CALL VAR%OUT_LOC(I)%ALLOC(VAR%OUT_NAM(I)%N, Z)
                            CALL VAR%OUT_PAD(I)%ALLOC(VAR%OUT_NAM(I)%N, 24)
                        ELSE
                            VAR%OUT_NAM(I)%N = Z
                            VAR%OUT_LOC(I)%N = Z
                            VAR%OUT_PAD(I)%N = Z
                        END IF
                        !
                        CALL BL%NEXT()
        END DO
    ELSE
        VAR%NFIL = Z
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_LOOKUP_BLOCK(VAR,BL)
    CLASS(VARIABLE_LIST),        INTENT(INOUT):: VAR
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL
    !
    INTEGER:: I, J, LLOC, ISTART, ISTOP, N
    TYPE(GENERIC_INPUT_FILE):: FL
    CHARACTER(EIGHT):: WORD
    !
    CALL BL%START()
    !
    I = Z
    DO WHILE (BL%LIST%IS_ASSOCIATED())
                    !
                    I = I + ONE
                    LLOC=ONE
                    CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP     )  !NAME
                    CALL GET       (BL%LINE,LLOC,ISTART,ISTOP,WORD)  !OPT
                    !
                    IF(WORD .NE. 'CONSTANT') THEN
                       CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,N,MSG='LOOKUP TABLE LOAD BLOCK ERROR: FAILED TO LOAD COUNT OF LOOK UP TABLE (NUMBER OF ROWS). A VALUE LESS THAN ONE, WILL TRIGGER AN AUTO-COUNT, BUT YOU STILL MUST SPECIFIY AN INTEGER AS A PLACE HOLDER.')  !COUNT
                       IF (N<Z) N=Z
                       !
                       BL%LN(:) = BL%LINE  !USE COPY OF LINE TO PREVENT FILE OBJECT FROM EDITING IT
                       CALL FL%OPEN(BL%LN,LLOC,BL%IOUT,BL%IU)
                       !
                       IF(.NOT. FL%IS_CONSTANT) THEN
                          IF(FL%ERROR .AND. FL%IU==Z) CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'LOOKUP TABLE LOAD BLOCK ERROR: THIS SPECIFIC INPUT LINE DOES NOT ALLOW FOR IMPLIED INTERNAL (ALL DATE ON SAME LINE)'//NL//'INSTEAD YOU MUST USE A KEYWORD SUCH AS "INTERNAL", EXTERNAL, OPEN/CLOSE, OR JUST SPECIFY THE FILE NAME TO OPEN.'//NL//'NOTE THAT LOOK UP TABLE SIZE IS DETERMINED BY WHAT IS WITHIN THE FILE, SO IT WILL LOAD ALL CONTAINS OF AN EXTERNAL FILE.') 
                          IF(FL%IU==Z .AND. N==Z)     CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'LOOKUP TABLE LOAD BLOCK ERROR: INTERNAL KEYWORD IS NOT SUPPORTED WITH AUTO-COUNT (SETTING COUNT<1).'//NL//'PLEASE MOVE THE TIME SERIES TO A SEPARATE FILE OR SPECIFY THE NUMBER OF LINES IT CONTAINS.')
                          IF(FL%BINARY.AND. N==Z)     CALL STOP_ERROR( LINE=BL%LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'LOOKUP TABLE LOAD BLOCK ERROR: BINARY KEYWORD IS NOT SUPPORTED WITH AUTO-COUNT (SETTING COUNT<1).'//NL//'PLEASE MOVE THE TIME SERIES TO A SEPARATE ASCII FILE OR SPECIFY THE NUMBER OF DATA POINTS IT CONTAINS.')
                          IF(FL%IU==Z) THEN
                                           CALL BL%NEXT()  !Load first line to check for SFAC
                                           DO J=ONE, N
                                               LLOC = ONE
                                               CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                                               IF( BL%LINE(ISTART:ISTOP) == 'SFAC' ) THEN
                                                   CALL BL%NEXT()
                                               ELSE
                                                   EXIT
                                               END IF
                                           END DO
                                           !
                                           DO J=TWO, N               !MOVE DOWN N LINES
                                                 CALL BL%NEXT()
                                           END DO
                          END IF
                       END IF
                    END IF
                    !
                    CALL BL%NEXT()    !NOVE TO NEXT TABFILE
    END DO
    !
    VAR%NTAB = I
    ALLOCATE(VAR%TAB(I))
    !
    CALL BL%START()
    !
    I = Z
    DO WHILE (BL%LIST%IS_ASSOCIATED())
                    I = I + ONE
                    LLOC=ONE
                    !
                    CALL VAR%TAB(I)%LOAD(LLOC,ISTART,ISTOP,BL,TRUE)
                    !
                    CALL BL%NEXT()
    END DO
    !
    !CALL FL%CLOSE()  --AUTOCLOSES WHEN SUBROUTINE EXITS
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_TIME_SERIES_VARIABLE_BLOCK(VAR,BL)
    CLASS(VARIABLE_LIST),        INTENT(INOUT):: VAR
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL
    !
    CALL LOAD_TIME_SERIES_BLOCK(BL,VAR%TSF, VAR%NTSF, READ_NAME=TRUE)
    !
  END SUBROUTINE
  !
  SUBROUTINE LOOKUP_TABLE_TO_VARIABLE(VAR, TABNAM, VARLOOKUP, VARSET)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    CHARACTER(*),         INTENT(IN   ):: TABNAM, VARLOOKUP, VARSET
    DOUBLE PRECISION:: X, Y
    INTEGER:: POS
    !
    POS = GET_VARIABLE_POSITION(VAR, VARLOOKUP)
    IF(POS > Z) THEN
        X = GET_VARIABLE_VALUE(VAR, VARLOOKUP, POS)
    ELSE
        READ(VARLOOKUP,*, IOSTAT=POS) X
        IF(POS .NE. Z) THEN
               CALL STOP_ERROR( OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE LOOKUP_TABLE_TO_VARIABLE) FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST OR CONVERT IT TO A FLOATING POINT NUMBER.'//NL//'THIS SUBROUTINE IS USED TO LOOK UP A VARIABLE NAME, SEND ITS VALUE TO A LOOKUP TABLE AND SET ANOTHER VARIABLE WITH ITS VALUE.'//NL//'THE NAME BEING SEARCHED FOR AND NOT FOUND IS: "'//TRIM(VARLOOKUP)//'"'//NL//'THE VARIABLE IT WAS SUPPOSED TO SET IS '//VARSET//NL//'IT WAS BEING LOOKED UP WITHIN LOOKUP TABLE: '//TABNAM  )
               !WRITE(*,'(/A,/A,/A)') 'VARIABLE LIST ERROR: (SUBROUTINE LOOKUP_TABLE_TO_VARIABLE) FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST OR CONVERT IT TO A FLOATING POINT NUMBER.','THIS SUBROUTINE IS USED TO LOOK UP A VARIABLE NAME, SEND ITS VALUE TO A LOOKUP TABLE AND SET ANOTHER VARIABLE WITH ITS VALUE.','THE NAME BEING SEARCHED FOR IS: "'//TRIM(VARLOOKUP)//'"' 
               !ERROR STOP
        END IF
    END IF
    !
    Y = GET_LOOKUP_TABLE(VAR,TABNAM,X)
    IF(Y.NE.Y) CALL STOP_ERROR( OUTPUT=VAR%IOUT, MSG= 'LOOKUP TABLE ERROR: FAILED TO LOCATE UNIQUE LOOKUP TABLE NAME.'//NL//'THE FOLLOWING WAS THE REQUESTED LOOKUP TABLE NAME :'//TABNAM//NL//'AND HAD A LOOKUP VARIABLE OF: '//VARLOOKUP//NL//'WHICH HAS A NUMERICAL VALUE OF '//NUM2STR(X))
    !
    CALL SET_VARIABLE_VALUE(VAR, VARSET, Y)
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL FUNCTION GET_VARIABLE_POSITION(VAR, NAM) RESULT(POS)
    CLASS(VARIABLE_LIST), INTENT(IN):: VAR
    CHARACTER(*),         INTENT(IN):: NAM
    INTEGER:: POS
    LOGICAL:: NOT_FOUND
    !
    NOT_FOUND = TRUE
    DO POS=ONE, VAR%N
        IF (NAM==VAR%NAM(POS)) THEN
                                 NOT_FOUND=FALSE
                                 EXIT
        END IF
    END DO
    !
    IF (NOT_FOUND) POS = Z
    !
  END FUNCTION
  !
  FUNCTION GET_LOOKUP_TABLE(VAR,TABNAM,X) RESULT(Y)
    CLASS(VARIABLE_LIST), INTENT(INOUT):: VAR
    CHARACTER(*),         INTENT(IN   ):: TABNAM
    DOUBLE PRECISION,     INTENT(IN   ):: X
    DOUBLE PRECISION:: Y
    INTEGER:: I
    !
    DO I=ONE, VAR%NTAB
        IF(TABNAM == VAR%TAB(I)%NAM) EXIT
        IF(I==VAR%NTAB)  CALL STOP_ERROR( OUTPUT=VAR%IOUT, MSG= 'LOOKUP TABLE ERROR: FAILED TO LOCATE UNIQUE LOOKUP TABLE NAME.'//NL//'THE FOLLOWING WAS THE REQUESTED LOOKUP TABLE NAME :'//TABNAM)
    END DO
    !
    CALL VAR%TAB(I)%LOOKUP(X,Y)
    !Y = VAR%TAB(I)%GET(X)
    !
    IF(Y.NE.Y) CALL STOP_ERROR( OUTPUT=VAR%IOUT, MSG= 'LOOKUP TABLE ERROR: FAILED TO PULL Y VALUE FOR LOOKUP TABLE FOR GIVEN X. (POSSIBLE CAUSE FOR THIS ERROR IS THAT THE LOOKUP TABLE IS EMPTY.)'//NL//'THE FOLLOWING WAS THE REQUESTED LOOKUP TABLE NAME: '//TABNAM//NL//'WITH LOOKUP VALUE OF: '//NUM2STR(X))
    !
  END FUNCTION
  !
END MODULE
!
MODULE S_LANGUAGE_INTERFACE!, ONLY: S_INTERPRETER, S_VARIABLE_LIST
  USE CONSTANTS
  USE PARSE_WORD_INTERFACE,              ONLY: PARSE_WORD
  USE STRINGS,                           ONLY: UPPER, GET_WORD
  USE ERROR_INTERFACE,                   ONLY: STOP_ERROR, WARNING_MESSAGE, PAUSE
  USE NUM2STR_INTERFACE,                 ONLY: NUM2STR
  USE IS_ASCII_INTERFACE,                ONLY: ASCII_CHECK
  USE IS_ROUTINES,                       ONLY: IS_INTEGER
  USE EquationParser
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,   ONLY: GENERIC_OUTPUT_FILE
  USE GENERIC_BLOCK_READER_INSTRUCTION,  ONLY: GENERIC_BLOCK_READER
  USE SUB_BLOCK_INPUT_INTERFACE,         ONLY: SUB_BLOCK_INPUT
  USE LINKED_LIST_INSTRUCTION,           ONLY: CHARACTER_LINKED_LIST, INTEGER_LINKED_LIST
  USE DATE_OPERATOR_INSTRUCTION,         ONLY: DATE_OPERATOR
  USE EQUATION_VARIABLE_LIST
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: S_INTERPRETER, S_VARIABLE_LIST, SLANG_VAR_BLOCKS, VARIABLE_NAME_MEANING
  !
  !!!ABSTRACT INTERFACE
  !!!                  SUBROUTINE SLANG_SET_PROPERTY(VAR, NAM, KPER, KSTP, KITER, LVL, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
  !!!                      CLASS(S_VARIABLE_LIST),                                 INTENT(INOUT):: VAR
  !!!                      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
  !!!                      INTEGER,                                                INTENT(IN   ):: KPER, KSTP, KITER, LVL
  !!!                      CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
  !!!                  END SUBROUTINE
  !!!                  !
  !!!                  SUBROUTINE SLANG_SET_RETURN(VAR, NAM, KPER, KSTP, ERROR)
  !!!                      CLASS(S_VARIABLE_LIST),                                 INTENT(INOUT):: VAR
  !!!                      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
  !!!                      INTEGER,                                                INTENT(IN   ):: KPER, KSTP
  !!!                      CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
  !!!                  END SUBROUTINE
  !!!END INTERFACE
  !
  CHARACTER(17), DIMENSION(11), PARAMETER:: SLANG_VAR_BLOCKS=[ 'VARIABLE         ', &
                                                               'VARIABLES        ', &
                                                               'SYSTEM           ', &
                                                               'PROPERTY         ', &
                                                               'RETURN           ', &
                                                               'OUTPUT           ', &
                                                               'LOOKUP           ', &
                                                               'TIME             ', &
                                                               'TIME_SERIES      ', &
                                                               'TIME_SERIES_FILE ', &
                                                               'TIME_SERIES_FILES'  ]
  !
  TYPE, EXTENDS(SUB_BLOCK_INPUT):: S_INTERPRETER  !LOADS RULES, PROCESS THEM BASED ON PASSED "VARIABLE_LIST"
      CONTAINS
      PROCEDURE, PASS(INP):: INIT           => INITIALIZE_S_INTERPRETER!(INP, TYP, LLOC, LINE, IOUT, IN, SCRATCH, NO_TRANSIENT, END_KEY, ALLOW_INTERNAL, BEGIN_END_OPTIONAL)
      PROCEDURE, PASS(RUL):: PREPARE_RULES => PREPARE_DECISION_RULES_IF_NEW!()  MUST RUN THIS BEFORE RUN_S_LANG 
      PROCEDURE, PASS(RUL):: RUN_S_LANG!(VARIABLE_LIST, OUTPUT)
  END TYPE
  !
  !################################################################
  !
  TYPE VARIABLE_NAME_MEANING
      INTEGER:: N=Z, POS=Z  !P HOLDS POSITION WITHIN S_VARIABLE_LIST OF VARIABLE NAME
      CHARACTER(:),DIMENSION(:),ALLOCATABLE:: PROP
      INTEGER,     DIMENSION(:),ALLOCATABLE:: ID
      CONTAINS
      !PROCEDURE, PASS(VMN):: ADD_PROP => ADD_PROP_VARIABLE_NAME_MEANING  !(NAM,ID,LVL)
      PROCEDURE, PASS(VMN):: INIT     => PARSE_VARIABLE_NAME!(NAM,POS)
      PROCEDURE, PASS(VMN):: GET      => GET_VARIABLE_NAME_MEANING  !(LVL,PROP,ID)
      PROCEDURE, PASS(VMN):: DESTROY  => DEALLOCATE_VARIABLE_NAME_MEANING!()
      FINAL:: DEALLOCATE_VARIABLE_NAME_MEANING_FINAL
  END TYPE
  !
  TYPE, EXTENDS(VARIABLE_LIST):: S_VARIABLE_LIST
      INTEGER:: NRET=Z  ! RETURN VALUE TO SWO
      INTEGER:: NSYS=Z  ! SYSTEM REQUESTED VALUE (viz SFR)
      INTEGER:: NVAR=Z  ! SCRATCH VARIABLES
      !INTEGER:: POS_SYS=Z  ! POSITION IN ARRAY OF SYSTEM QUERY VARIABLES 
      INTEGER:: POS_VAR=Z  ! POSITION IN ARRAY OF VARIABLES               
      !
      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), ALLOCATABLE:: PROP_PUL  !DIM(NSYS)
      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), ALLOCATABLE:: PROP_RET  !DIM(NRET)
      !LOGICAL,                      DIMENSION(:), ALLOCATABLE:: REQUIRED  !DIM(NRET)
      !LOGICAL,                      DIMENSION(:), ALLOCATABLE:: AT_LEAST   !DIM(NRET)
      !
      CONTAINS
      PROCEDURE, PASS(VAR):: LOAD    => INIT_VARIABLES!(IN,IOUT)
      !FINAL:: FINAL_DEALLOCATE_SWO_VARIABLE_LIST
  END TYPE
  !
  !################################################################
  !
  !
  !TYPE SWO_EXTRA
  !    LOGICAL:: INUSE=FALSE
  !    CHARACTER(:), ALLOCATABLE:: TYP
  !    INTEGER,      ALLOCATABLE:: FLAG
  !    CONTAINS
  !    PROCEDURE, PASS(EX):: SET    => SET_SWO_EXTRA!(TYP,FLAG)
  !    PROCEDURE, PASS(EX):: DESTROY => DEALLOCATE_SWO_EXTRA!()
  !    FINAL:: FINAL_DEALLOCATE_SWO_EXTRA
  !END TYPE
  !
  !################################################################
  !
  CONTAINS
  !
  SUBROUTINE INIT_VARIABLES(VAR,IN,IOUT,BYPASS)
    CLASS(S_VARIABLE_LIST), INTENT(INOUT):: VAR
    INTEGER,                  INTENT(IN   ):: IN,IOUT
    LOGICAL,         OPTIONAL,INTENT(IN   ):: BYPASS  ! IF SET TO TRUE, THEN ANY BLOCK THAT IS NOT RECONIZED IS BYPASSED. OTHERWISE A WARNING IS RAISED
    !
    TYPE(VARIABLE_LIST):: SVAR, SYS, RET, OTF, TBL, TSF
    TYPE(GENERIC_BLOCK_READER):: BL
    TYPE(CHARACTER_LINKED_LIST):: LST
    INTEGER:: I,J,K,N,MAX_LEN, OUT_LEN, NFIL, NTAB, NTSF
    CHARACTER(:), ALLOCATABLE:: ERR
    CHARACTER(35):: FMT
    LOGICAL:: HAS_VAR, HAS_SYS, HAS_RET, HAS_TSF, HAS_LOKTAB, HAS_OUT, BYPAS
    !    
    !ONLY_LOAD =[ 'VARIABLE         ', &
    !             'VARIABLES        ', &
    !             'SYSTEM           ', &
    !             'PROPERTY         ', &
    !             'RETURN           ', &
    !             'OUTPUT           ', &
    !             'LOOKUP           ', &
    !             'TIME             ', &
    !             'TIME_SERIES      ', &
    !             'TIME_SERIES_FILE ', &
    !             'TIME_SERIES_FILES'  ]
    !
    HAS_VAR     = FALSE
    HAS_SYS     = FALSE
    HAS_RET     = FALSE
    HAS_TSF     = FALSE
    HAS_LOKTAB  = FALSE
    HAS_OUT     = FALSE
    !
    IF(PRESENT(BYPASS)) THEN
                            BYPAS = BYPASS
    ELSE                   
                            BYPAS = FALSE
    END IF                 
    !
    FMT = "('FOUND BEGIN ',A, ' NOW LOADING')"
    !
    DO I=ONE,SIX
         !
         IF(BYPAS) THEN
                   CALL BL%LOAD(IN,IOUT)
         ELSE
                   CALL BL%LOAD(IN,IOUT, ONLY_LOAD=SLANG_VAR_BLOCKS)
         END IF
         !
         SELECT CASE(BL%NAME)
         CASE('VARIABLE','VARIABLES');
                                       WRITE(IOUT,FMT) BL%NAME
                                       IF(BL%NLINE>0) CALL SVAR%LOAD_VARIABLE_LIST(BL,TRUE)
                                       HAS_VAR = TRUE
         CASE('SYSTEM','PROPERTY');  
                                       WRITE(IOUT,FMT) BL%NAME 
                                       IF(BL%NLINE>0) CALL SYS%LOAD_VARIABLE_LIST(BL,FALSE)
                                       HAS_SYS = TRUE
         CASE('RETURN'  );            
                                       WRITE(IOUT,FMT) BL%NAME
                                       IF(BL%NLINE>0) CALL RET%LOAD_VARIABLE_LIST(BL,TRUE)
                                       HAS_RET = TRUE
         CASE('OUTPUT'  );            
                                       WRITE(IOUT,FMT) BL%NAME
                                       IF(BL%NLINE>0) CALL OTF%LOAD_OUTPUT_BLOCK(BL)
                                       HAS_OUT = TRUE
         CASE('LOOKUP'  );            
                                       WRITE(IOUT,FMT) BL%NAME
                                       IF(BL%NLINE>0) CALL TBL%LOAD_LOOKUP_BLOCK(BL)
                                       HAS_LOKTAB = TRUE
         CASE('TIME','TIME_SERIES','TIME_SERIES_FILE','TIME_SERIES_FILES')
                                       WRITE(IOUT,FMT) BL%NAME
                                      IF(BL%NLINE>0) CALL TSF%LOAD_TIME_SERIES_BLOCK(BL)
                                       HAS_TSF = TRUE
         CASE DEFAULT
              !
              IF(.NOT. BYPAS)  THEN
                  CALL WARNING_MESSAGE(INFILE=IN,OUTPUT=IOUT,MSG='S-LANGAUGE ATTEMPTED TO LOAD VAIRABLE BLOCKS TO IDENTIFY ANY VARIABLE DEFINITION BLOCKS.'//NL//'MAKE SURE INPUT IS SUPPOSED TO NOT INCLUDE BEGIN FOLLOWED BY ANY OF THE FOLLOWING VARIABE DEFINTION BLOCK NAMES:'//NL//'"VARIABLE", "SYSTEM", "RETURN", "OUTPUT", "LOOKUP", AND "TIME_SERIES_FILE" ARE ACCEPTED VARIABLE BLOCK NAMES.')
                  EXIT
              END IF
              !
         END SELECT
         !
         IF(BL%EXTRA == 'EOF') EXIT
    END DO
    !
    MAX_LEN = Z
    IF(ALLOCATED(SVAR%NAM)) MAX_LEN = MAX(MAX_LEN, LEN(SVAR%NAM))
    IF(ALLOCATED(SYS%NAM))  MAX_LEN = MAX(MAX_LEN, LEN(SYS%NAM ))
    IF(ALLOCATED(RET%NAM))  MAX_LEN = MAX(MAX_LEN, LEN(RET%NAM ))
    !
    VAR%NVAR = Z !SVAR%N
    VAR%NSYS = Z !SYS%N
    VAR%NRET = Z !RET%N
    !VAR%NEX  = RET%N
    !
    CALL LST%INIT()
    DO I = ONE, RET%N
                     CALL LST%ADD_UNIQUE(RET%NAM(I), K)
                     !
                     IF(K==Z) VAR%NRET = VAR%NRET + ONE
    END DO
    DO I = ONE, SYS%N
                     CALL LST%ADD_UNIQUE(SYS%NAM(I), K)
                     !
                     IF(K==Z) VAR%NSYS = VAR%NSYS + ONE
    END DO
    !
    VAR%POS_VAR = LST%LEN() + ONE
    !
    ! First check if Scratch Variables match any SYS or RET
    DO I = ONE, SVAR%N
                     IF( LST%IS_UNIQUE(SVAR%NAM(I))) THEN
                         !
                         VAR%NVAR = VAR%NVAR + ONE
                         !
                     ELSE
                         !
                         IF(I<SVAR%N) THEN
                             IF( SVAR%NAM(I) == SVAR%NAM(I+ONE)) CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG='S LANGAUGE VARIABLES VARIABLE ERROR: A VARIABLE NAME (VARNAM) WITH AN ARRAY INDEX (VARNAM[]) IS INDENTICAL TO A VARIABLE NAME IN EITHER THE SYSTEM PROPERTY (SYSNAM) OR RETURN (RETNAM) NAMES.'//NL//'THE VARIABLE FOUND THAT IS CAUSING PROBLEMS IS "'//SVAR%NAM(I)//'"'//BLN//'PLEASE EITHER REMOVE THE ARRAY DIMENSION OR CHANGE THE VARIABLE NAME (VARNAM[]).') 
                         END IF
                         !
                         SVAR%NAM(I) = BLNK
                         !
                     END IF
    END DO
    !
    DO I = ONE, SVAR%N
                     IF(SVAR%NAM(I) .NE. BLNK) CALL LST%ADD(SVAR%NAM(I))  !Can have multiple VARIABLES names cause of DIM[N]
    END DO
    !    
    !VAR%POS_SYS = VAR%NRET + ONE             !position of system variables
    !VAR%POS_VAR = VAR%NRET + VAR%NSYS + ONE  !position of the dummy variables
    !VAR%POS_RET = ONE
    !VAR%N    = VAR%NVAR + VAR%NSYS + VAR%NRET
    !
    N       = LST%LEN() !VAR%NVAR + VAR%NSYS + VAR%NRET
    NTAB    = TBL%NTAB
    NTSF    = TSF%NTSF
    !
    OUT_LEN = ONE
    IF(ALLOCATED(OTF%OUT_ID)) OUT_LEN = LEN(OTF%OUT_ID)
    NFIL    = OTF%NFIL
    !
    CALL VAR%INIT(MAX_LEN,N,NTAB,OUT_LEN,NFIL,NTSF,IOUT)
    !
    CALL LST%FIRST_LINE
    DO I=ONE, N
                VAR%NAM(I) = LST%LN
                CALL LST%NEXT_LINE
    END DO
    !
    I = Z
    DO WHILE (I < SVAR%N)
       I = I + ONE
       IF(SVAR%NAM(I) .NE. BLNK) THEN
          !
          DO J=ONE, N 
                IF( SVAR%NAM(I) == VAR%NAM(J) )THEN
                                                  VAR%VAL(J) = SVAR%VAL(I)
                                                  !
                                                  IF( I < SVAR%N .AND. J < N) THEN
                                                      !
                                                      IF( SVAR%NAM(I) == VAR%NAM(J+1) ) THEN !ONLY TRUE IF ARRAY 
                                                          !
                                                          DO K = J+1, N  !Note K is 1 ahead of J so check against next I vale
                                                              !
                                                              IF( SVAR%NAM(I) == VAR%NAM(K) ) THEN !Current I is equal to next Name (K)
                                                                  I = I + ONE
                                                                  VAR%VAL(K) = SVAR%VAL(I)
                                                              ELSE
                                                                  EXIT
                                                              ENDIF
                                                              !
                                                              IF( K == N .AND. I+1 < SVAR%N) THEN
                                                                  !
                                                                  IF( SVAR%NAM(I+1) == VAR%NAM(K-1) ) THEN !Current I is equal to next Name (K)
                                                                      I = I + ONE
                                                                      VAR%VAL(K) = SVAR%VAL(I)
                                                                   END IF
                                                              END IF
                                                          END DO
                                                      END IF
                                                  ENDIF
                                                  !
                                                  EXIT
                END IF
          END DO
       END IF
    END DO
    !
    DO I=ONE, NTAB
              CALL TBL%TAB(I)%MOVE( VAR%TAB(I) )
    END DO
    !
    DO I=ONE, NFIL
                   VAR%OUT_ID(I)    = OTF%OUT_ID(I)                             !COPY NAME OF TABLE
                   CALL OTF%OUT_NAM(I)%MOVE( VAR%OUT_NAM(I) )
                   CALL OTF%OUT_LOC(I)%MOVE( VAR%OUT_LOC(I) )
                   CALL OTF%OUT_PAD(I)%MOVE( VAR%OUT_PAD(I) )
                   !VAR%OUT_NAM(I)%N = OTF%OUT_NAM(I)%N
                   !ALLOCATE(VAR%OUT_NAM(I)%STR, SOURCE = OTF%OUT_NAM(I)%STR)  !MOVE OVER CHARACTER ARRAY OF VARIABLES TO PRINT OUT
                   CALL OTF%OUTPUT(I)%MOVE( VAR%OUTPUT(I) )    !MOVE GENERIC_INPUT_FILE TO NEW LOCATION
    END DO
    !
    IF(NTSF > Z) CALL TSF%TSF%MOVE( VAR%TSF )
    !
    CALL VAR%BUILD_OUTPUT_INDEX(ERR)
    !
    IF(ERR.NE.BLNK) CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG='S LANGAUGE OUTPUT VARIABLE ERROR: A VARIABLE TO PRINT (VARNAM) IS NOT DEFINED IN THE'//NL//'"VARIABLE", "SYSTEM", OR "RETURN" BLOCKS.'//NL//'THE FOLLOWING IS A LIST OF OUTPUT VARIABLE NAMES (FILEID)'//NL//'AND THEIR ASSOCIATED VARIABLE TO PRINT (VARNAM) THAT WAS NOT DEFINED.'//BLN//'FILEID'//REPEAT(BLNK,OUT_LEN-6)//'   VARNAM'//NL//ERR(2:))
    !
    CALL VAR%BUILD_OUTPUT_HEADER
    !
!    ALLOCATE(VAR%REQUIRED(VAR%NRET), SOURCE = FALSE)
!    ALLOCATE(VAR%AT_LEAST(VAR%NRET), SOURCE = FALSE)
    !
    ALLOCATE(VAR%PROP_PUL(VAR%NSYS))
    DO I=ONE, VAR%NSYS
       DO J=ONE, N 
                IF( SYS%NAM(I) == VAR%NAM(J) )THEN
                                                  VAR%VAL(J)  = SYS%VAL(I)
                                                  !
                                                  CALL VAR%PROP_PUL(I)%INIT( SYS%NAM(I), J )
                                                  EXIT
                END IF
       END DO
    END DO
    !
    ALLOCATE(VAR%PROP_RET(VAR%NRET))
    !
    DO I=ONE, VAR%NRET
       DO J=ONE, N 
                IF( RET%NAM(I) == VAR%NAM(J) )THEN
                                                  VAR%VAL(J)  = RET%VAL(I)
                                                  !
                                                  CALL VAR%PROP_RET(I)%INIT( RET%NAM(I), J )
                                                  EXIT
                END IF
       END DO
    END DO
    !
  END SUBROUTINE
  !
  !################################################################
  !
  SUBROUTINE RUN_S_LANG(RUL,VAR,OUTPUT)
    CLASS(S_INTERPRETER), INTENT(INOUT):: RUL
    CLASS(VARIABLE_LIST),  INTENT(INOUT):: VAR
    INTEGER, OPTIONAL,     INTENT(IN   ):: OUTPUT
    INTEGER:: NLINE, POS, PRNT
    !
    IF(PRESENT(OUTPUT)) THEN
        PRNT = OUTPUT
    ELSE
        PRNT = Z
    END IF
    !
    IF(RUL%INUSE .AND. .NOT. RUL%SKIP) THEN
                      CALL RUL%BL%START()
                      CALL RUL%BL%LIST%SET_LN()
                      POS = ONE
                      NLINE = RUL%BL%NLINE
                      !
                      CALL EVALUATE_S_LINKED_LIST(RUL%BL%LIST,VAR,NLINE,POS,PRNT)
    END IF
    !
  END SUBROUTINE
  !
  !################################################################
  !
  RECURSIVE SUBROUTINE EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT,GOTO_ENDIF,USE_LLOC)  !MUST HAVE ISTART AND ISTOP ALREADY SET
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
    INTEGER,                      INTENT(IN   ):: NLINE
    INTEGER,                      INTENT(INOUT):: POS
    INTEGER,                      INTENT(INOUT):: LLOC,ISTART,ISTOP
    INTEGER,                      INTENT(IN   ):: PRNT
    LOGICAL, OPTIONAL,            INTENT(INOUT):: GOTO_ENDIF
    LOGICAL, OPTIONAL,            INTENT(IN   ):: USE_LLOC
    !
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF ( POS > Z) THEN
        SELECT CASE (LST%LN(ISTART:ISTOP))
        CASE ('DO')
                       CALL EVALUATE_DO_LOOP(LST,VAR,NLINE,POS,PRNT)
        CASE ('WHILE')
                       CALL EVALUATE_WHILE_LOOP(LST,VAR,NLINE,POS,PRNT,ERRMSG)
        CASE ('IF')
                       CALL EVALUATE_IF_LOGICAL(LST,VAR,NLINE,POS,PRNT,ERRMSG)
        CASE ('IF[')
                       CALL EVALUATE_SINGLE_LINE_IF_LOGICAL(LST,VAR,NLINE,POS,PRNT,ERRMSG)
        CASE ('GO_TO_END')
                       IF(PRNT.NE.Z) WRITE(PRNT,'(A)') ' GO_TO_END KEYWORD FOUND, NOW MOVING TO END OF SCRIPT -- NO FUTHER INSTRUCTIONS READ'
                       POS = TWO*NLINE
        CASE ('EXIT')
                       IF(PRNT.NE.Z) WRITE(PRNT,'(A)') ' EXIT KEYWORD FOUND, NOW MOVING TO "END_DO" OR "END_WHILE"'
                       CALL GO_TO_LOOP_END(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,'???') !GO TO ENDDO OR ENDWHILE
                       !DO
                       !   CALL LST%NEXT()
                       !   CALL LST%SET_LN()
                       !   POS = POS + ONE
                       !   LLOC=ONE
                       !   CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
                       !   SELECT CASE (LST%LN(ISTART:ISTOP))
                       !   CASE('ENDDO','END_DO','ENDWHILE','END_WHILE')
                       !       POS = NEG*POS   !NEGATIVE INDICATES THAT IF STATEMENTS SHOULD BE AUTOMATICALLY ENDED
                       !       EXIT
                       !   END SELECT
                       !   IF(LST%LN=='ERROR') THEN
                       !                   CALL LST%POS(POS-ONE)
                       !                   CALL LST%SET_LN()
                       !                    CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND "EXIT" KEYWORD, BUT FAILED TO FIND ENDDO OR ENDWHILE AFTER IT.  (NOTE THAT "END DO" OR "END WHILE" IS NOT ALLOWED)')
                       !   END IF
                       !END DO
!        CASE ('REQUIRED')
!                       SELECT TYPE (VAR)
!                       CLASS IS (S_VARIABLE_LIST)
!                                    CALL SET_REQUIRED_VARIABLE(VAR,LST%LN,LLOC,ISTART,ISTOP,TRUE)
!                       CLASS DEFAULT
!                                     CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT,MSG='SWO ERROR: ONLY SWO RULES READER SUPPORTS REQUIRED KEYWORD.')
!                       END SELECT
!                       CALL LST%NEXT_LINE()
!                       POS = POS + ONE
!        CASE ('NOT_REQUIRED','NOTREQUIRED')
!                       SELECT TYPE (VAR)
!                       CLASS IS (S_VARIABLE_LIST)
!                                    CALL SET_REQUIRED_VARIABLE(VAR,LST%LN,LLOC,ISTART,ISTOP,FALSE)
!                       CLASS DEFAULT
!                                     CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT,MSG='SWO ERROR: ONLY SWO RULES READER SUPPORTS REQUIRED KEYWORD.')
!                       END SELECT
!                       CALL LST%NEXT_LINE()
!                       POS = POS + ONE
        CASE('LOOKUP')
                       CALL EVALUATE_LOOKUP_TABLE(VAR,LST%LN,LLOC,ISTART,ISTOP,PRNT)
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE('TIME_SERIES')
                       CALL EVALUATE_TIME_SERIES_TABLE(VAR,LST%LN,LLOC,ISTART,ISTOP,PRNT)
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE ('ELSEIF','ELSE_IF','ELSE')
                       IF(PRESENT(GOTO_ENDIF)) THEN
                           GOTO_ENDIF=TRUE
                       ELSE
                            CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='ERROR: WHILE EVALUATING LINE FOUND KEYWORD "ELSE" OR "ELSEIF", HOWEVER THE STRUCTURE SHOULD NOT FIND THIS KEYWORD (MOST LIKELY MISSING STARTING "IF").'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR LOGIC.')
                       END IF
                       IF(PRNT.NE.Z) WRITE(PRNT,'(/ 1x A)') LST%LN(ISTART:ISTOP)//' KEYWORD FOUND, NOW MOVING TO "END_IF"'
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE('PRINT')
                       CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
                       IF(LST%LN(ISTART:ISTOP) == 'VARIABLES') THEN
                           IF(PRNT.NE.Z) THEN
                               IF(LST%LN(LLOC:)==BLNK) THEN
                                   WRITE(PRNT,'(/A,A)') TRIM(LST%LN), VAR%PRINT_VAR()
                               ELSE
                                   WRITE(PRNT,'(/A,A)') TRIM(LST%LN), VAR%PRINT_VAR(LST%LN(LLOC:))
                               END IF
                           !ELSE
                           !    WRITE(PRNT,'(3(/A))') 'S-Langauge Requested Variable Print, Without Assinging a Transcript File', VAR%PRINT_VAR()
                           END IF
                       ELSE
                           CALL VAR%PRINT(LST%LN(ISTART:ISTOP),LST%LN)
                           IF(PRNT.NE.Z) WRITE(PRNT,'(1x A)') TRIM(LST%LN)
                       END IF
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE('ERROR_STOP','ERRORSTOP')
                       IF(PRNT.NE.Z) WRITE(PRNT,'(/1x A/)') TRIM(LST%LN)
                       CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
                       CALL STOP_ERROR( OUTPUT=VAR%IOUT, MSG= 'S-Language "ERROR_STOP" KEYWORD FOUND WITH MESSAGE: '//BLN//'"'//TRIM(LST%LN(ISTART:))//'"'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR() )
        CASE('ERROR_WARN','ERRORWARN')
                       IF(PRNT.NE.Z) WRITE(PRNT,'(/1x A/)') TRIM(LST%LN)
                       CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
                       CALL WARNING_MESSAGE(OUTPUT=VAR%IOUT, MSG= 'S-Language "ERROR_WARN" KEYWORD FOUND WITH MESSAGE: '//BLN//'"'//TRIM(LST%LN(ISTART:))//'"'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR() )
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE('WARN','WARNING')
                       IF(PRNT.NE.Z) WRITE(PRNT,'(/1x A/)') TRIM(LST%LN)
                       CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP,FIND_NEXT=TRUE)
                       CALL VAR%WARN(LST%LN(ISTART:ISTOP),LST%LN(LLOC:))
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE('ENDIF','END_IF')
                       CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR: WHILE EVALUATING LINE FOUND KEYWORD "'//LST%LN(ISTART:ISTOP)//','//NL//'HOWEVER THE S LANGUAGE LOGIC NEVER FOUND AN APPROPIATELY PLACED "IF", "ELSEIF", OR "ELSE" BEFORE IT.'//NL//'(THAT IS MOST LIKELY MISSING STARTING "IF").'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR LOGIC.')
        CASE('ENDDO','END_DO')
                       CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR: WHILE EVALUATING LINE FOUND KEYWORD "'//LST%LN(ISTART:ISTOP)//','//NL//'HOWEVER THE S LANGUAGE LOGIC NEVER FOUND AN APPROPIATELY PLACED "DO". (THAT IS MOST LIKELY MISSING STARTING "DO KOUNT").'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR LOGIC.')
        CASE('ENDWHILE','END_WHILE')
                       CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR: WHILE EVALUATING LINE FOUND KEYWORD "'//LST%LN(ISTART:ISTOP)//','//NL//'HOWEVER THE S LANGUAGE LOGIC NEVER FOUND AN APPROPIATELY PLACED "WHILE". (THAT IS MOST LIKELY MISSING STARTING "WHILE COND").'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR LOGIC.')
        CASE('PAUSE')
                       IF(PRNT.NE.Z) THEN
                           IF(LST%LN(LLOC:)==BLNK) THEN
                               WRITE(PRNT,'(/A,A)') TRIM(LST%LN), VAR%PRINT_VAR()
                           ELSE
                               WRITE(PRNT,'(/A,A)') TRIM(LST%LN), VAR%PRINT_VAR(LST%LN(LLOC:))
                           END IF
                           FLUSH(PRNT)
                       END IF
                       IF(LST%LN(LLOC:)==BLNK) THEN
                           WRITE(*,'(/A,/A,/A)') 'PAUSE STATEMENT FOUND.','THE FOLLOWING ARE THE CURRENT VARIABLE LIST', VAR%PRINT_VAR()
                       ELSE
                           WRITE(*,'(/A,/A,/A)') 'PAUSE STATEMENT FOUND.','THE FOLLOWING ARE THE CURRENT VARIABLE LIST', VAR%PRINT_VAR(LST%LN(LLOC:))
                       END IF
                       CALL PAUSE()
                       CALL LST%NEXT_LINE()
                       POS = POS + ONE
        CASE DEFAULT
                       CALL EVALUATE_EQUATION(LST,VAR,POS,PRNT,ERRMSG,LLOC,USE_LLOC)
        END SELECT 
    END IF
  END SUBROUTINE
  !
  !################################################################
  !
  SUBROUTINE EVALUATE_EQUATION(LST,VAR,POS,PRNT,ERRMSG,LLOC,USE_LLOC)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
    INTEGER,                      INTENT(INOUT):: POS
    INTEGER,                      INTENT(IN   ):: PRNT
    CHARACTER(:),  ALLOCATABLE,   INTENT(INOUT):: ERRMSG  
    INTEGER,                      INTENT(INOUT):: LLOC
    LOGICAL, OPTIONAL,            INTENT(IN   ):: USE_LLOC
    INTEGER:: I
    !
    IF(PRESENT(USE_LLOC)) THEN
            IF(USE_LLOC) THEN
                  CALL VAR%SOLVE_EQUATION(LST%LN(LLOC:), ERRMSG, I)
            ELSE
                  CALL VAR%SOLVE_EQUATION(LST%LN, ERRMSG, I)
            END IF
    ELSE
                  CALL VAR%SOLVE_EQUATION(LST%LN, ERRMSG, I)
    END IF
    !
    IF(PRNT.NE.Z) THEN
        LLOC = INDEX(LST%LN,'=')
        !
        WRITE(PRNT,'(A)') ' SOLVED EQUATION '//TRIM(LST%LN)//' AND SET VARIABLE "'//TRIM(ADJUSTL(LST%LN(:LLOC-1)))//'" = '//NUM2STR( VAR%GET(LST%LN(:LLOC-1), I) )
    END IF
    !
    IF(ALLOCATED(ERRMSG))  THEN
        BLOCK
            CHARACTER(:),  ALLOCATABLE:: NON_ASCII
            !
            CALL ASCII_MSG(LST%LN, NON_ASCII)
            !
            IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
            !
        END BLOCK
        !
        CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING EQUATION.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR()) 
    END IF
    !
    CALL LST%NEXT_LINE()
    POS = POS + ONE
  END SUBROUTINE
  !
  !######################################################################
  !
  SUBROUTINE INITIALIZE_S_INTERPRETER(INP, TYP, LLOC, LINE, IOUT, IN, SCRATCH, NO_TRANSIENT, END_KEY, ALLOW_INTERNAL, BEGIN_END_OPTIONAL, SKIP_END_KEY) !DO NOT SPECIFIY SKIP_END_KEY
    CLASS(S_INTERPRETER),  INTENT(INOUT):: INP
    CHARACTER(*),          INTENT(IN   ):: TYP
    CHARACTER(*),          INTENT(IN   ):: LINE
    INTEGER,               INTENT(INOUT):: LLOC
    INTEGER,               INTENT(IN   ):: IOUT, IN
    INTEGER,     OPTIONAL, INTENT(IN   ):: SCRATCH
    LOGICAL,     OPTIONAL, INTENT(IN   ):: NO_TRANSIENT
    CHARACTER(*),OPTIONAL, INTENT(IN   ):: END_KEY
    LOGICAL,     OPTIONAL, INTENT(IN   ):: ALLOW_INTERNAL, BEGIN_END_OPTIONAL
    CHARACTER(*), DIMENSION(:),OPTIONAL,INTENT(IN):: SKIP_END_KEY
    !
    CHARACTER(5),  DIMENSION(3 ):: KEYS
    !    
    KEYS      =[ 'IF   ', &
                 'DO   ', &
                 'WHERE'  ]
    !
    CALL INP%SUB_BLOCK_INPUT%INIT(TYP, LLOC, LINE, IOUT, IN, SCRATCH, NO_TRANSIENT, END_KEY, ALLOW_INTERNAL, BEGIN_END_OPTIONAL, SKIP_END_KEY=KEYS)
    !
  END SUBROUTINE
  !
  !######################################################################
  !
  SUBROUTINE PREPARE_DECISION_RULES_IF_NEW(RUL)
    CLASS(S_INTERPRETER), INTENT(INOUT):: RUL
    INTEGER:: I, LLOC, ISTART,ISTOP, N
    !
    IF(RUL%INUSE) THEN
         IF(.NOT. RUL%REPEAT) THEN
                                   CALL RUL%BL%START()
                                   CALL RUL%BL%LIST%SET_LN()
                                   DO I=ONE, RUL%BL%NLINE  !MAKE SURE LINES ARE ALL UPPER CASE
                                                   !
                                                   CALL UPPER(RUL%BL%LINE)
                                                   !
                                                   N = LEN(RUL%BL%LINE)
                                                   !
                                                   IF(N > TWO) THEN                               !CHECK FOR "IF ["
                                                       IF( 'IF ' == RUL%BL%LINE(ONE:THREE) ) THEN
                                                            CALL CHECK_SINGLE_LINE_IF(RUL%BL%LINE,LLOC,ISTART,ISTOP,RUL%BL%IOUT)
                                                            !
                                                            !MOVE [ OVER TO MAKE IF[
                                                            IF(LLOC > Z) RUL%BL%LINE(:) = 'IF[ '//RUL%BL%LINE(ISTART:)
                                                       END IF
                                                   END IF
                                                   !
                                                   IF(N > FOUR) THEN                               !CHECK FOR "IF ["
                                                       IF( 'END ' == RUL%BL%LINE(ONE:FOUR) ) THEN
                                                            !
                                                            LLOC = FIVE
                                                            CALL PARSE_WORD(RUL%BL%LINE,LLOC,ISTART,ISTOP)
                                                            !
                                                            !CHEC FOR END IF, END DO, END WHERE
                                                            !
                                                            IF    ('IF' == RUL%BL%LINE(ISTART:ISTOP) ) THEN
                                                                                                       RUL%BL%LINE(:) = 'END_IF'
                                                            ELSEIF('DO' == RUL%BL%LINE(ISTART:ISTOP) ) THEN
                                                                                                       RUL%BL%LINE(:) = 'END_DO'
                                                            ELSEIF('WHERE' == RUL%BL%LINE(ISTART:ISTOP) ) THEN
                                                                                                       RUL%BL%LINE(:) = 'END_WHERE'
                                                            ELSE
                                                                CALL STOP_ERROR( LINE=RUL%BL%LINE, OUTPUT=RUL%BL%IOUT, MSG= 'SLang ERROR: FOUND IN RULE SET "END". THIS SHOULD NEVER HAPPEN UNLESS ITS bEING USED TO TERMINATE LOADING OF THE S INSTRUCTIONS.'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//RUL%BL%LIST%NBACK(NINE,NL)  )
                                                            END IF
                                                       END IF
                                                   END IF
                                                   !
                                                   IF(N > FIVE) THEN                              !CHECK FOR "ELSE IF"
                                                       !    1234567
                                                       !    ELSE IF
                                                       IF( 'ELSE ' == RUL%BL%LINE(ONE:FIVE) ) THEN
                                                           !
                                                           LLOC = SIX
                                                           CALL PARSE_WORD(RUL%BL%LINE,LLOC,ISTART,ISTOP)
                                                           !
                                                           IF('IF' == RUL%BL%LINE(ISTART:ISTOP) ) THEN
                                                               LLOC = ISTOP + TWO
                                                               IF(RUL%BL%LINE(LLOC:LLOC) == ' ') THEN
                                                                   CALL PARSE_WORD(RUL%BL%LINE,LLOC,ISTART,ISTOP)
                                                                   LLOC = ISTART
                                                               END IF
                                                               RUL%BL%LINE(:) = 'ELSEIF '//RUL%BL%LINE(LLOC:)
                                                           END IF
                                                       END IF
                                                   END IF
                                                   !
                                                   IF(N > SIX) THEN                               !CHECK FOR "ERROR STOP" AND "ERROR WARN"
                                                       !    123456789
                                                       !    ERROR STOP
                                                       IF( 'ERROR ' == RUL%BL%LINE(ONE:SIX) ) THEN
                                                           !
                                                           LLOC = SEV
                                                           CALL PARSE_WORD(RUL%BL%LINE,LLOC,ISTART,ISTOP)
                                                           !
                                                           IF('STOP' == RUL%BL%LINE(ISTART:ISTOP) ) THEN
                                                               LLOC = ISTOP + ONE
                                                               IF(LLOC < N ) THEN
                                                                   RUL%BL%LINE(:) = 'ERRORSTOP '//TRIM(ADJUSTL( RUL%BL%LINE(LLOC:) ))
                                                               ELSE
                                                                   RUL%BL%LINE(:) = 'ERRORSTOP'
                                                               END IF
                                                               !
                                                           ELSEIF('WARN' == RUL%BL%LINE(ISTART:ISTOP) ) THEN
                                                               LLOC = ISTOP + ONE
                                                               IF(LLOC < N ) THEN
                                                                   RUL%BL%LINE(:) = 'ERRORWARN '//TRIM(ADJUSTL( RUL%BL%LINE(LLOC:) ))
                                                               ELSE
                                                                   RUL%BL%LINE(:) = 'ERRORWARN'
                                                               END IF
                                                           END IF
                                                       END IF
                                                   END IF
                                                   !
                                                   CALL RUL%BL%NEXT()
                                                   CALL RUL%BL%LIST%SET_LN()
                                   END DO
                                   CALL RUL%BL%START()
                                   CALL RUL%BL%LIST%SET_LN()
         END IF
    END IF
    !
  END SUBROUTINE
  !
  !################################################################
  !
  SUBROUTINE NEXT_LINE_NEXT_WORD(LST,POS,LLOC,ISTART,ISTOP)
     CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
     INTEGER,                      INTENT(INOUT):: POS,LLOC,ISTART,ISTOP
     CALL LST%NEXT_LINE()
     POS = POS + ONE
     LLOC=ONE
     CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
  END SUBROUTINE
  !
  !################################################################
  !
  SUBROUTINE EVALUATE_S_LINKED_LIST(LST,VAR,NLINE,POS,OUTPUT)
  CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
  CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
  INTEGER,                      INTENT(IN   ):: NLINE
  INTEGER,                      INTENT(INOUT):: POS
  INTEGER, OPTIONAL,     INTENT(IN   ):: OUTPUT
  INTEGER:: LLOC,ISTART,ISTOP,PRNT
  !
  IF(PRESENT(OUTPUT)) THEN
      PRNT = OUTPUT
  ELSE
      CALL VAR%GET_OUTPUT_VARIABLE_IU('TRANSCRIPT', PRNT)!  PRNT = TRANSCRIPT IU OR ZERO
  END IF
  !
  IF(PRNT.NE.Z) WRITE(PRNT,'(/ / 24x A/)') '*** TRANSCRIPT OF S LANGAUGE LINE INTERPRETATION AND OUTPUT ***'
  !
  DO WHILE (POS <= NLINE)
        !
        LLOC=ONE
        CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
        !
        CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT)  !CHECKS FOR DO, IF, OR EVALUATES VALUE, DOES NOT USE NOR PASSS GOTO_ENDIF
        !
        IF(POS<Z) THEN
            CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'EQUATION PARSER ERROR: FOUND EXIT STATEMENT THAT WAS NOT WITHIN A DO LOOP OR WHILE LOOP.'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)  )
            !WRITE(VAR%IOUT,'(A)') 'EQUATION PARSER ERROR: FOUND EXIT STATEMENT THAT WAS NOT WITHIN A DO LOOP OR WHILE LOOP.'
            !ERROR STOP
        END IF
  END DO
  !
  END SUBROUTINE
  !
  !################################################################
  !
  RECURSIVE SUBROUTINE GO_TO_LOOP_END(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,TYP)
  CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
  CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
  INTEGER,                      INTENT(IN   ):: NLINE
  INTEGER,                      INTENT(INOUT):: POS,LLOC,ISTART,ISTOP
  CHARACTER(*),                 INTENT(IN   ):: TYP
  !
  DO
     CALL NEXT_LINE_NEXT_WORD(LST,POS,LLOC,ISTART,ISTOP)
     !
     SELECT CASE (LST%LN(ISTART:ISTOP))
     CASE('DO')
         CALL GO_TO_LOOP_END(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,'DO')
         CALL NEXT_LINE_NEXT_WORD(LST,POS,LLOC,ISTART,ISTOP)
     CASE('WHILE')
         CALL GO_TO_LOOP_END(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,'WHILE')
         CALL NEXT_LINE_NEXT_WORD(LST,POS,LLOC,ISTART,ISTOP)
     END SELECT
     !
     SELECT CASE (TYP)
     CASE ('WHILE')
                 SELECT CASE (LST%LN(ISTART:ISTOP))
                 CASE('ENDWHILE','END_WHILE');  EXIT
                 END SELECT
     CASE('DO')
                 SELECT CASE (LST%LN(ISTART:ISTOP))
                 CASE('ENDDO','END_DO'); EXIT
                 END SELECT
     CASE DEFAULT
                 SELECT CASE (LST%LN(ISTART:ISTOP))
                 CASE('ENDDO','END_DO','ENDWHILE','END_WHILE'); EXIT
                 END SELECT
     END SELECT
     !
     IF(LST%LN=='ERROR') THEN
                     CALL LST%POS(POS-ONE)
                     CALL LST%SET_LN()
                      CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND EITHER "EXIT" KEYWORD OR KOUNT WAS SET TO ZERO, BUT FAILED TO FIND "ENDDO", "END_DO", "ENDWHILE" OR "END_WHILE" AFTER IT.'//BLN//'  *** NOTE THAT "END DO" OR "END WHILE" IS NOT ALLOWED ***'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE START OF THE BAD LOOP):'//BLN//LST%NBACK(NINE,NL))
     END IF
  END DO
  !
  POS = NEG*POS   !NEGATIVE INDICATES THAT IF LOOP INSTRUCTION AUTOMATICALLY ENDED IRRELEVANT OF KOUNT OR COND
  !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE EVALUATE_DO_LOOP(LST,VAR,NLINE,POS,PRNT)
  CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
  CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
  INTEGER,                      INTENT(IN   ):: NLINE
  INTEGER,                      INTENT(INOUT):: POS
  INTEGER,                      INTENT(IN   ):: PRNT
  INTEGER:: LLOC,ISTART,ISTOP, IERR, KOUNT, I, LOOP_START
  CHARACTER(:), ALLOCATABLE:: TXT
  !
  LLOC = THREE  !MOVE PAST 'DO  '
  CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
  READ(LST%LN(ISTART:ISTOP),*, IOSTAT=IERR) KOUNT
  !
  IF(IERR.NE.Z) THEN
    DO I=ONE, VAR%N
        IF (LST%LN(ISTART:ISTOP)==VAR%NAM(I)) THEN
                                 KOUNT=NINT(VAR%VAL(I))
                                 IERR=Z
                                 EXIT
        END IF
    END DO
  END IF
  IF(IERR.NE.Z)  CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FAILED TO CONVERT DO LOOP COUNT TO A NUMBER.'//NL//'THE STRUCTURE SHOULD BE "DO COUNT" WHERE COUNT IS AN INTEGER THAT REPRESENTS THE NUMBER OF LOOPS.'//NL//'COUNT CAN ALSO BE A VARIABLE NAME, BUT THIS ERROR WILL OCCUR IF THE VARIABLE NAME IS NOT FOUND.'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL))
  !
  IF(PRNT.NE.Z) THEN
      TXT = ' START DO LOOP WITH '//NUM2STR(KOUNT)//' ITERATIONS '
      TXT = TXT//REPEAT('-',120 - LEN(TXT) + 10)//'> "'//TRIM(LST%LN)//'"'
      WRITE(PRNT,'(A /)') TXT
  END IF
  !
  IF (KOUNT > Z) THEN
     CALL LST%NEXT_LINE()
     POS = POS + ONE
  ELSE
      KOUNT = Z
      CALL GO_TO_LOOP_END(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,'DO')
  END IF
  !
  LOOP_START = POS
  I=ONE
  DO
      LLOC=ONE
      CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
      !
      SELECT CASE (LST%LN(ISTART:ISTOP))
      CASE ('ENDDO', 'END_DO')
                  !
                  IF(POS < Z) THEN
                      POS = NEG*POS   !FLAG TO INDICATE AN EXIT
                      I = KOUNT + ONE
                  ELSE
                      I=I+ONE
                  END IF
                  !
                  IF(I<=KOUNT) THEN
                      POS=LOOP_START
                      CALL LST%POS(LOOP_START)
                      CALL LST%SET_LN()
                      !
                      IF(PRNT.NE.Z) WRITE(PRNT,'(/ 20x 2A /)') 'LOOP ', NUM2STR(I)
                  ELSE
                      CALL LST%NEXT_LINE()
                      POS = POS + ONE
                      IF(PRNT.NE.Z) THEN
                          TXT(2:6)='END  '
                          WRITE(PRNT,'(/ A)') TXT
                      END IF
                      EXIT
                  END IF
      CASE DEFAULT
                  CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT)  !CHECKS FOR DO, IF, OR EVALUATES VALUE, DOES NOT USE NOR PASSS GOTO_ENDIF
                  !
                  IF(POS == TWO*NLINE) EXIT ! ENCOUNTERED "GO_TO_END"
      END SELECT
      IF(LST%LN=='ERROR') THEN
                      CALL LST%POS(LOOP_START)
                      CALL LST%SET_LN()
                      CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "DO COUNT" COMMAND,'//NL//'BUT IT WAS NOT TERMINATED WITH AN "ENDDO" COMMAND.'//NL//'PLEASE CHECK INPUT TO SEE IF THERE IS LOCATED AFTER THE DO AN RESPECTIVE ENDDO. (NOTE THAT "END DO" IS NOT ALLOWED)'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE START OF THE BAD DO LOOP LINE):'//BLN//LST%NBACK(NINE,NL))
      END IF
  END DO
  !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE EVALUATE_WHILE_LOOP(LST,VAR,NLINE,POS,PRNT,ERRMSG)
  CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
  CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
  INTEGER,                      INTENT(IN   ):: NLINE
  INTEGER,                      INTENT(INOUT):: POS
  INTEGER,                      INTENT(IN   ):: PRNT
  CHARACTER(:),  ALLOCATABLE,   INTENT(INOUT):: ERRMSG  
  INTEGER:: LLOC,ISTART,ISTOP, I, LOOP_START
  CHARACTER(:),ALLOCATABLE:: WHILE_COND, TXT
  !
  !       1234567890
  ! LN = 'WHILE COND'
  IF( LEN_TRIM(LST%LN) > 6) THEN
      ALLOCATE( WHILE_COND, SOURCE= TRIM(ADJUSTL(LST%LN(7:))) )
  ELSE
      ALLOCATE( WHILE_COND, SOURCE= 'true' )
  END IF
  !
  IF(WHILE_COND == 'TRUE' .OR. WHILE_COND==BLNK) THEN
      DEALLOCATE(WHILE_COND)
      ALLOCATE( WHILE_COND, SOURCE= 'true' )
  ELSEIF(WHILE_COND == 'FALSE' ) THEN
      DEALLOCATE(WHILE_COND)
      ALLOCATE( WHILE_COND, SOURCE= 'false' )
  END IF
  !
  IF(PRNT.NE.Z) THEN
      TXT = ' START WHILE LOOP WITH CONDITION EVALUATION OF "'//WHILE_COND//'" '
      TXT = TXT//REPEAT('-',120 - LEN(TXT) + 10)//'> "'//TRIM(LST%LN)//'"'
      WRITE(PRNT,'(A /)') TXT
  END IF
  
  !
  I=Z
  IF( VAR%SOLVE_LOGICAL(WHILE_COND, ERRMSG) ) THEN
       !
       IF(ALLOCATED(ERRMSG))  THEN
           BLOCK
               CHARACTER(:),  ALLOCATABLE:: NON_ASCII
               !
               CALL ASCII_MSG(LST%LN, NON_ASCII)
               !
               IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
               !
           END BLOCK
           !
           CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING A "WHILE" LOOP CONDITION.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR())
       END IF
       !
       CALL LST%NEXT_LINE()
       POS = POS + ONE
       !
       LOOP_START = POS
       !
       DO
         LLOC=ONE
         CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
         !
         SELECT CASE (LST%LN(ISTART:ISTOP))
         CASE ('ENDWHILE', 'END_WHILE')
                     I=I+ONE
                     !
                     IF( POS < ONE) THEN  !EXIT FOUND
                         CALL LST%NEXT_LINE()
                         POS = ABS(POS) + ONE
                         EXIT
                     ELSEIF( VAR%SOLVE_LOGICAL(WHILE_COND, ERRMSG)) THEN  !POS > Z INDICATES NO EXIT FOUND
                         !
                         IF(ALLOCATED(ERRMSG))  THEN
                             BLOCK
                                 CHARACTER(:),  ALLOCATABLE:: NON_ASCII
                                 !
                                 CALL ASCII_MSG(LST%LN, NON_ASCII)
                                 !
                                 IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
                                 !
                             END BLOCK
                             !
                             CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING A "WHILE" LOOP CONDITION.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR())
                         END IF
                         !
                         POS=LOOP_START
                         CALL LST%POS(LOOP_START)
                         CALL LST%SET_LN()
                         !
                         IF(PRNT.NE.Z) WRITE(PRNT,'(/ 20x 2A /)') 'WHILE LOOP ', NUM2STR(I)
                     ELSE
                         CALL LST%NEXT_LINE()
                         POS = ABS(POS) + ONE
                         EXIT
                     END IF
         CASE DEFAULT
                     CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT)  !CHECKS FOR DO, IF, OR EVALUATES VALUE, DOES NOT USE NOR PASSS GOTO_ENDIF
                     !
                     IF(POS == TWO*NLINE) EXIT ! ENCOUNTERED "GO_TO_END"
         END SELECT
         IF(LST%LN=='ERROR') THEN
                         CALL LST%POS(LOOP_START)
                         CALL LST%SET_LN()
                          CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "WHILE CONDITION" COMMAND,'//NL//'BUT IT WAS NOT TERMINATED WITH AN "ENDWHILE" COMMAND.'//NL//'PLEASE CHECK INPUT TO SEE IF THERE IS LOCATED AFTER THE WHILE AN RESPECTIVE ENDWHILE. (NOTE THAT "END WHILE" IS NOT ALLOWED)'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE START OF THE BAD WHILE LOOP LINE):'//BLN//LST%NBACK(NINE,NL))
         ELSEIF( I > 10000) THEN
                         CALL LST%POS(LOOP_START)
                         CALL LST%SET_LN()
                         CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "WHILE CONDITION" COMMAND,'//NL//'BUT IT HAS LOOPED 10,000 TIMES WHICH IS THE MAX LOOP LIMIT.'//NL//'THIS LIMIT IS IMPOSED TO KEEP MODEL FROM GETTING STUCK IN AN INFINITE LOOP.'//NL//'PLEASE CHECK YOUR INPUT TO MAKE SURE THAT THE CONDITION IS SOMETHING THAT IS FALSE AT SOME POINT.'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE START OF THE BAD WHILE LOOP LINE):'//BLN//LST%NBACK(NINE,NL))
         END IF
       END DO
  ELSE
       !FAILED INITIAL CONDITIONAL CHECK MOVE TO END_WHILE
       CALL GO_TO_LOOP_END(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,'WHILE')
       CALL LST%NEXT_LINE()
       POS = ABS(POS) + ONE
  END IF
  !
  IF(PRNT.NE.Z) THEN
      WRITE(PRNT,'(A)') ' WHILE LOOP COMPLETED '//NUM2STR(I)//' ITERATIONS'
      TXT(2:6)='END  '
      WRITE(PRNT,'(/ A)') TXT
  END IF
  !
  DEALLOCATE(WHILE_COND)
  !
  END SUBROUTINE
  !
  SUBROUTINE SOLVE_DATE_LOGICAL_LINE(VAR, COND, LN, ERRMSG)
    CLASS(VARIABLE_LIST),       INTENT(INOUT):: VAR
    LOGICAL,                    INTENT(OUT  ):: COND
    CHARACTER(*),               INTENT(INOUT):: LN
    CHARACTER(:),  ALLOCATABLE, INTENT(INOUT):: ERRMSG  
    TYPE(DATE_OPERATOR):: DT
    TYPE(CHARACTER_LINKED_LIST):: LST
    DOUBLE PRECISION:: DYEAR
    CHARACTER(:), ALLOCATABLE:: TMP, LINE
    CHARACTER(19):: NUM
    INTEGER:: I, J
    CHARACTER:: BR1,BR2
    LOGICAL:: HAS_BLANK
    !
    BR1 = '{'
    BR2 = '}'
    HAS_BLANK = FALSE
    DO I=ONE, LEN_TRIM(LN)
                          IF(LN(I:I)==BLNK .OR. LN(I:I)==TAB) THEN
                                                 HAS_BLANK = TRUE
                                                 EXIT
                          END IF
    END DO
    !
    IF ( HAS_BLANK) THEN
                        I=ONE
                        DO WHILE (I < LEN_TRIM(LN))
                               IF(LN(I:I)==BLNK .OR. LN(I:I)==TAB) THEN   !IF NOT BLANK AND NOT TAB THEN ADD CHARACTER
                                  LN(I:) = LN(I+ONE:)
                               ELSE
                                   I = I + ONE
                               END IF
                        END DO
    END IF
    !
    CALL LST%INIT()   !SEARCH FOR ANY AND or OR OPERATORS
    I = Z
    J = SCAN(LN,'&|')
    DO WHILE (J>Z)
        J = J + I              !HAVE TO SHIFT TO PROPER LOCATION BECAUSE J WAS RELATIVE TO LAST '&|'
        TMP = LN(I+1:J-1)                         !COMPILER BUG THAT CAUSES CRASH WHEN PASSING TO LST%ADD A SINGLE CHARACTER (eg LN(5:5)). TMP VARIABLE SEEMS TO BY PASS BUG
        CALL LST%ADD(TMP)
        TMP = LN(J:J)
        CALL LST%ADD(TMP)
        I = J
        J = SCAN(LN(I+1:),'&|')
    END DO
    TMP = TRIM(LN(I+1:))
    CALL LST%ADD(TMP)
    !
    DEALLOCATE(TMP)
    !
    CALL LST%START()
    CALL LST%SET_LN()
    DO WHILE (LST%IS_ASSOCIATED())
          I = INDEX(LST%LN,BR1)
          IF(I>Z) THEN
                      ALLOCATE(TMP, SOURCE=LST%CHAR())
                      J = INDEX(TMP,BR2);             IF(J==Z) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) FOUND A LEFT BRACE, {, BUT DID NOT FIND A CORRESPONDING RIGHT BRACE, }.'//NL//'A SET OF BRACES INDICATES THAT THE LOGICAL SHOULD CHECK FOR A DATE WTIHIN IT OF THE FORM {mm/dd} OR {mm/dd/YYYY}.' )
                      !
                      CALL DT%INIT(TMP(I+1:J-1))
                      IF(DT%NOT_SET()) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) UNKNOWN ERROR TRYING TO CONVERT DATE OF THE FORM {date}'//NL//'where date ="mm/dd/YYYY" OR "mm/YYYY" OR "mm\dd" OR "YYYY-mm-dd" OR "YYYY-mm" OR "mm/dd/YYYYThh:mm:ss" OR "YYYY-mm-ddThh:mm:ss"'//NL//'THE FOLLOWING IS THE DATE THAT THE CODE ATTEMPTED TO CONVERT TO A DECIMAL YEAR "'//LST%LN(I+1:J-1)//'".'//NL//'THIS DATE SHOULD BE IN THE FORMAT OF mm/dd/YYYY TO BE CONVERTED TO A DECIMAL YEAR' )
                      IF(DT%NO_YEAR()) THEN
                                         DYEAR = VAR%GET('YEAR')
                                         IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('DATE.START')
                                         IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('DATE.END')
                                         IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.START.YEAR')
                                         IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.START.DYEAR')
                                         IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.END.YEAR')
                                         IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.END.DYEAR')
                                         IF(DYEAR.NE.DYEAR) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) DATE WAS DEFINED WITH A \ WHICH INDICATES IT IS LOADED AS mm\dd'//NL//'IF IT IS OF THE FORM {mm\dd} YOU MUST ALSO DEFINE A REGULAR VARIABLE "YEAR" IN ORDER TO GET THE YEAR FOR THE DATE'//NL//'OR HAVE ONE OF THE FOLLOWING SYSTEM VARIABLES DEFINED (WHICH YEAR YEAR IS TAKEN FROM IN ORDER OF PRIORITY)'//NL//'"DATE.START", "DATE.END", OR "TS.START.YEAR" OR "TS.START.DYEAR" OR "TS.END.YEAR" OR "TS.END.DYEAR".' )
                                         !IF(DYEAR.NE.DYEAR) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) FOUND { AND } AFTER AN IF RULE LINE CHECK.'//NL//'THIS INDICATES THERE IS A DATE WITHIN THE {}.'//NL//'THIS DATE MUST BE OF THE FORM {mm/dd} OR {mm/dd/YYYY}.'//NL//'IF IT IS OF THE FORM {mm/dd} YOU MUST ALSO DEFINE THE SYSTEM VARIABLS "TS.START.YEAR" OR "TS.START.DYEAR" OR "TS.END.YEAR" OR "TS.END.DYEAR" OR DEFINE A REGULAR VARIABLE "YEAR" IN ORDER TO GET THE YEAR FOR THE DATE AND HAVE A PROPER COMPARISON.'//NL//'FOR EXAMPLE "{mm/dd} > TS.END.DYEAR" OR "{mm/dd} > {mm/dd}".' )
                                         !
                                         CALL DT%SET_YEAR(INT(DYEAR))
                      END IF
                      !
                      DYEAR = DT%DYEAR
                      !
                      !N = INDEX(TMP,'\') !J - I - ONE  -- N<= FIVE
                      !IF (N > Z) THEN ! {mm\dd}  12\15
                      !    !
                      !    DYEAR = VAR%GET('TS.START.YEAR')
                      !    IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.START.DYEAR')
                      !    IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.END.YEAR')
                      !    IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('TS.END.DYEAR')
                      !    IF(DYEAR.NE.DYEAR) DYEAR = VAR%GET('YEAR')
                      !    !
                      !    IF(DYEAR.NE.DYEAR) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) FOUND { AND } AFTER AN IF RULE LINE CHECK.'//NL//'THIS INDICATES THERE IS A DATE WITHIN THE {}.'//NL//'THIS DATE MUST BE OF THE FORM {mm/dd} OR {mm/dd/YYYY}.'//NL//'IF IT IS OF THE FORM {mm/dd} YOU MUST ALSO DEFINE THE SYSTEM VARIABLS "TS.START.YEAR" OR "TS.START.DYEAR" OR "TS.END.YEAR" OR "TS.END.DYEAR" OR DEFINE A REGULAR VARIABLE "YEAR" IN ORDER TO GET THE YEAR FOR THE DATE AND HAVE A PROPER COMPARISON.'//NL//'FOR EXAMPLE "{mm/dd} > TS.END.DYEAR" OR "{mm/dd} > {mm/dd}".' )
                      !    !
                      !    TMP(N:N)='/'
                      !    !            mm  / dd       /    yyyy
                      !    CALL DT%INIT(TMP(I+1:J-1)//'/'//NUM2STR(INT(DYEAR)))
                      !    IF(DT%DAY==-999) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) UNKNOWN ERROR TRYING TO CONVERT DATE OF THE FORM {mm/dd} TO DECIMAL YEAR.'//NL//'THE CODE PULLS FROM VARIABLE "DATE.START.DYEAR" OR "DATE.END.DYEAR" OR IF THEY ARE NOT DEFINED, THEN SETS THE YEAR TO 1979. THIS YEAR IS THEN ADDED TO THE DATE mm/dd.'//NL//'THE YEAR OBTAINED IS "'//NUM2STR(INT(DYEAR))//'" AND THE RESULTING DATE CONSTRUCTED IS "'//LST%LN(I+1:J-1)//'/'//NUM2STR(INT(DYEAR))//'".'//NL//'THIS CONSTRUCTED DATE SHOULD BE IN THE FORMAT OF mm/dd/YYYY TO BE CONVERTED TO A DECIMAL YEAR' )
                      !    !
                      !    DYEAR = DT%DYEAR
                      !    !
                      !ELSE
                      !    CALL DT%INIT(TMP(I+1:J-1))
                      !    IF(DT%DAY==-999) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) UNKNOWN ERROR TRYING TO CONVERT DATE OF THE FORM {mm/dd/YYYY} TO DECIMAL YEAR.'//NL//'THE FOLLOWING IS THE DATE THAT THE CODE ATTEMPTED TO CONVERT TO A DECIMAL YEAR "'//LST%LN(I+1:J-1)//'".'//NL//'THIS DATE SHOULD BE IN THE FORMAT OF mm/dd/YYYY TO BE CONVERTED TO A DECIMAL YEAR' )
                      !    DYEAR = DT%DYEAR
                      !END IF
                      !
                      WRITE(NUM,'(F17.12)') DYEAR
                      NUM = TRIM(ADJUSTL(NUM))//'D0'
                      !
                      IF     (I==ONE) THEN
                                               ALLOCATE( LINE, SOURCE = TRIM(NUM)//TMP(J+1:) )
                      ELSEIF (J==LEN(TMP)) THEN
                                               ALLOCATE( LINE, SOURCE = TMP(:I-1)//TRIM(NUM) )
                      ELSE
                                               ALLOCATE( LINE, SOURCE = TMP(:I-1)//TRIM(NUM)//TMP(J+1:) )
                      END IF
                      !
                      CALL LST%CHANGE( TRIM(LINE), ERROR=HAS_BLANK)  !HAS_BLANK IS USED TO DETERMINE IF THERE IS AN ERRR
                      !
                      IF(HAS_BLANK) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE BUILD_DATE_LOGICAL_LINE) UNKNOWN ERROR TRYING TO BUILD LOGICAL DATE COMPARISON BY CONVERTING DATES TO DECIMAL YEAR.'//NL//'THE FOLLOWING IS THE ATTEMPED PART TO BE CONVERTED "'//LINE//'".' )
                      !
                      DEALLOCATE(LINE, TMP)
          END IF
          CALL LST%NEXT_LINE()
    END DO
    !
    CALL LST%MAKE_LINE(LINE)
    CALL LST%DESTROY()
    !
    COND = VAR%SOLVE_LOGICAL(LINE, ERRMSG)
    !
    DEALLOCATE(LINE)
    !
  END SUBROUTINE
  !
  PURE FUNCTION NOT_SINGLE_LINE_IF(LN) RESULT(NOT_SNGL)                     !COULD IMPROVE SPEED SINCE ALWAYS SET UP TO 'IF['
    !LLOC IS POSITION AFTER CLOSING BRACKET OR NEGATIVE IF BRACKET NOT FOUND
    !ISTART HOLDS START AFTER IF OR FIRST BRACKET [
    !
    CHARACTER(*), INTENT(IN   ):: LN
    INTEGER:: LLOC, ISTART, ISTOP
    LOGICAL:: NOT_SNGL
    !
    ! CHECK FOR BRACKET
    LLOC=THREE
    CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
    !
    NOT_SNGL = LN(ISTART:ISTART) .NE. '['
    !
  END FUNCTION
  !
  SUBROUTINE CHECK_SINGLE_LINE_IF(LN,LLOC,ISTART,ISTOP,IOUT)                 !COULD IMPROVE SPEED SINCE ALWAYS SET UP TO IF[
    !LLOC IS POSITION AFTER CLOSING BRACKET OR NEGATIVE IF BRACKET NOT FOUND
    !ISTART HOLDS START AFTER IF OR FIRST BRACKET [
    !
    CHARACTER(*), INTENT(IN   ):: LN
    INTEGER,      INTENT(INOUT)::LLOC, ISTART, ISTOP, IOUT
    INTEGER:: CNT
    !
    ! CHECK FOR BRACKET
    LLOC  =THREE
    ISTART=THREE
    !
    !  12345
    !  IF [
    !
    IF(LN(ISTART:ISTART) == ' ') CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
    !
    IF(LN(ISTART:ISTART) .NE. '[') THEN  !NO BRACKET SO DONE
        ISTOP = LEN_TRIM(LN)
        LLOC  = NEG
    ELSE                       !SEARCH FOR  ]
        ISTART = ISTART + ONE  !ONLY GET CONTENTS WITHIN [ COND ]
        !
        IF(LN(ISTART:ISTART) == ' ') THEN !FIND FIRST NON BLANK
                                        LLOC = ISTART
                                        CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP) 
        END IF
        !
        ISTOP = Z              !FINAL STOPPING POINT -- IS STILL ZERO IF ERROR
        CNT   = Z              !BRACKET COUNTER
        !
        DO LLOC=ISTART, LEN_TRIM(LN)
            IF    (LN(LLOC:LLOC) == '[') THEN
                                                      CNT = CNT + ONE
            ELSEIF(LN(LLOC:LLOC) == ']') THEN
                                         IF(CNT == Z) THEN
                                                      ISTOP  = LLOC   - ONE
                                                      EXIT
                                         ELSE
                                                     CNT = CNT - ONE
                                         END IF
            END IF
        END DO
        !
        !CHECK FAILED TO FIND CLOSING BRACKET
        IF(ISTOP == Z) CALL STOP_ERROR( LINE=LN, OUTPUT=IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "IF [ CONDITION ]" COMMAND'//NL//'THAT IS AN OPENING BRAKCET "[" WAS FOUND AFTER THE KEYWORD "IF" INDICATING A SINGLE LINE IF CONDITION."'//NL//'BUT FAILED TO IDENTIFY A CLOSING BRAKCET THAT ENCLOSES THE IF CONDITION.'//NL//'PLEASE CHECK INPUT TO SEE IF YOU MEANT A MULTI-LINE IF THAT IS TERMINATED WITH ENDIF OR A SINGLE LINE IF HTAT IS MISSING THE CLOSING BRACKET ]')
        !
        LLOC = ISTOP + TWO  !MOVE PAST THE CLOSING BRACKET ]
        !
    END IF
    !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE EVALUATE_SINGLE_LINE_IF_LOGICAL(LST,VAR,NLINE,POS,PRNT,ERRMSG)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
    INTEGER,                      INTENT(IN   ):: NLINE
    INTEGER,                      INTENT(INOUT):: POS
    INTEGER,                      INTENT(IN   ):: PRNT
    CHARACTER(:),  ALLOCATABLE,   INTENT(INOUT):: ERRMSG 
    INTEGER:: LLOC,ISTART,ISTOP, CNT
    LOGICAL:: COND
    CHARACTER(:),ALLOCATABLE:: IF_COND
    !
    !       12345678
    !      'IF[ COND'
    !
    ! CHECK FOR BRACKET
    LLOC  =FIVE
    ISTART=FIVE
    !
    IF(LST%LN(ISTART:ISTART) == ' ') CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
    !
    ISTOP = Z              !FINAL STOPPING POINT -- IS STILL ZERO IF ERROR
    CNT   = Z              !BRACKET COUNTER
    !
    DO LLOC=ISTART, LEN_TRIM(LST%LN)
        IF    (LST%LN(LLOC:LLOC) == '[') THEN
                                                  CNT = CNT + ONE
        ELSEIF(LST%LN(LLOC:LLOC) == ']') THEN
                                     IF(CNT == Z) THEN
                                                  ISTOP  = LLOC   - ONE
                                                  EXIT
                                     ELSE
                                                 CNT = CNT - ONE
                                     END IF
        END IF
    END DO
    !
    !IF(ISTOP == Z) CALL STOP_ERROR( LINE=LST%LN, OUTPUT=IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "IF [ CONDITION ]" COMMAND'//NL//'THAT IS AN OPENING BRAKCET "[" WAS FOUND AFTER THE KEYWORD "IF" INDICATING A SINGLE LINE IF CONDITION."'//NL//'BUT FAILED TO IDENTIFY A CLOSING BRAKCET THAT ENCLOSES THE IF CONDITION.'//NL//'PLEASE CHECK INPUT TO SEE IF YOU MEANT A MULTI-LINE IF THAT IS TERMINATED WITH ENDIF OR A SINGLE LINE IF HTAT IS MISSING THE CLOSING BRACKET ]')
    !
    ALLOCATE( IF_COND, SOURCE=LST%LN(ISTART:ISTOP) )
    !
    IF(PRNT.NE.Z) WRITE(PRNT,'(/3A)',ADVANCE='NO') ' SINGLE LINE IF STATEMENT EVALUATED FOR THE FOLLOWING CONDITION "',IF_COND,'" WHICH RETURNED A "'
    !
    IF( INDEX(IF_COND,'{') > 0 ) THEN
        !
        CALL SOLVE_DATE_LOGICAL_LINE(VAR, COND, IF_COND, ERRMSG)
        !
    ELSE
        COND=VAR%SOLVE_LOGICAL(IF_COND, ERRMSG)
    END IF
    !
    IF(PRNT.NE.Z) WRITE(PRNT,'(4A)') NUM2STR(COND,Z,'TR'),'" RESULT FROM LINE "',TRIM(LST%LN),'"'
    !
    !
    IF(COND) THEN 
             !
             IF(ALLOCATED(ERRMSG))  THEN
                 BLOCK
                     CHARACTER(:),  ALLOCATABLE:: NON_ASCII
                     !
                     CALL ASCII_MSG(LST%LN, NON_ASCII)
                     !
                     IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
                     !
                 END BLOCK
                 !
                 CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING THE CONDITIONAL PART OF AN "IF" STATEMENT.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR())
             END IF
             !
             LLOC = ISTOP + TWO  !MOVE PAST THE CLOSING BRACKET ]
             !
             IF(LST%LN(LLOC:LLOC) == ' ') THEN
                 CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
                 LLOC = ISTART
             END IF
             !
             CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT,USE_LLOC=TRUE)
    ELSE
             CALL LST%NEXT_LINE()
             POS = POS + ONE
    END IF
    !
  END SUBROUTINE 
  !
  RECURSIVE SUBROUTINE EVALUATE_IF_LOGICAL(LST,VAR,NLINE,POS,PRNT,ERRMSG)
  CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
  CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
  INTEGER,                      INTENT(IN   ):: NLINE
  INTEGER,                      INTENT(INOUT):: POS
  INTEGER,                      INTENT(IN   ):: PRNT
  CHARACTER(:),  ALLOCATABLE,   INTENT(INOUT):: ERRMSG 
  INTEGER:: LLOC,ISTART,ISTOP, IF_START, IF_COUNT
  LOGICAL:: COND, GOTO_ENDIF
  CHARACTER(:),ALLOCATABLE:: IF_COND
  !
  IF_COUNT = Z
  IF_START = POS
  GOTO_ENDIF = FALSE
  !       123456
  !      'IF COND'
  !
  IF_COND = TRIM(ADJUSTL(LST%LN(FOUR:)))
  !
  IF( INDEX(IF_COND,'{') > 0 ) THEN
      CALL SOLVE_DATE_LOGICAL_LINE(VAR, COND, IF_COND, ERRMSG)
      IF_COND = TRIM(ADJUSTL(LST%LN(FOUR:)))
  ELSE
      COND=VAR%SOLVE_LOGICAL(IF_COND, ERRMSG)
  END IF
  !
  IF(PRNT.NE.Z) THEN
                WRITE(PRNT,'(/A)') ' IF      EVALUATED CONDITION "'//IF_COND//'"'//TAB//TAB//'WHICH RETURNED A "'//NUM2STR(COND,Z,'TR')//'" RESULT FROM LINE "'//TRIM(LST%LN)//'"'
                !
                IF(COND) WRITE(PRNT,'(A)')
  END IF
  !
  IF(COND) THEN
             IF(ALLOCATED(ERRMSG))  THEN
                 BLOCK
                     CHARACTER(:),  ALLOCATABLE:: NON_ASCII
                     !
                     CALL ASCII_MSG(LST%LN, NON_ASCII)
                     !
                     IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
                     !
                 END BLOCK
                 !
                 CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING THE CONDITIONAL PART OF AN "IF" STATEMENT.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR())
             END IF
  END IF
  !
  CALL LST%NEXT_LINE()
  POS = POS + ONE
  !
  DO WHILE (Z < POS)   !POS < Z ==> FOUND EXIT FLAG -- ALREADY BYPASSED TO END_DO/END_WHILE
      !
      LLOC=ONE
      CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
      !   
      IF( LST%LN(ISTART:ISTOP)=='ENDIF' .OR. LST%LN(ISTART:ISTOP)== 'END_IF') THEN
                      !
                      IF(PRNT.NE.Z .AND. IF_COUNT==Z) WRITE(PRNT,'(/ 1x A /)') LST%LN(ISTART:ISTOP)//' REACHED FOR "IF '//IF_COND//'"'
                      !
                      CALL LST%NEXT_LINE()
                      POS = POS + ONE
                      !
                      IF (IF_COUNT > Z) THEN
                          IF_COUNT = IF_COUNT - ONE
                      ELSE
                          EXIT
                      END IF
                      !
      ELSEIF( LST%LN(ISTART:ISTOP)=='IF' .AND. (GOTO_ENDIF .OR. .NOT. COND) ) THEN   !FOUND AN IF STATEMENT WITHIN ANOTHER IF, BUT WITHIN ITS FALSE PORTION OF THE STATEMENT, SO BYPASS IT THAT WAS NOT WITHIN A TRUE PART OF AN ORIGINAL IF BLOCK
                      CALL LST%NEXT_LINE()
                      POS = POS + ONE
                      IF_COUNT = IF_COUNT + ONE
      ELSEIF(GOTO_ENDIF) THEN
                   CALL LST%NEXT_LINE()
                   POS = POS + ONE
      ELSEIF(COND) THEN
                   CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT,GOTO_ENDIF)  !CHECKS FOR DO, IF, OR EVALUATES VALUE, SETS GOTO_ENDIF=TRUE IF ELSE, ELSEIF, IS FOUND
                   !
                   IF(POS == TWO*NLINE) EXIT ! ENCOUNTERED "GO_TO_END"
                   !                  12345678
      ELSEIF(LST%LN(ISTART:ISTOP) == 'ELSEIF' .AND. IF_COUNT == Z) THEN
                                  !
                                  COND=VAR%SOLVE_LOGICAL(LST%LN(7:), ERRMSG)
                                  IF(PRNT.NE.Z) THEN
                                                WRITE(PRNT,'(A)') ' ELSEIF  EVALUATED CONDITION "'//TRIM(ADJUSTL(LST%LN(7:)))//'"'//TAB//TAB//'WHICH RETURNED A "'//NUM2STR(COND,Z,'TR')//'" THAT IS APART OF THE IF BLOCK "IF '//IF_COND//'"'
                                                !
                                                IF(COND) WRITE(PRNT,'(A)')
                                  END IF
                                  !
                                  IF(COND) THEN
                                      IF(ALLOCATED(ERRMSG))  THEN
                                          BLOCK
                                              CHARACTER(:),  ALLOCATABLE:: NON_ASCII
                                              !
                                              CALL ASCII_MSG(LST%LN, NON_ASCII)
                                              !
                                              IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
                                              !
                                          END BLOCK
                                          !
                                          CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING THE CONDITIONAL PART OF AN "IF" STATEMENT.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR())
                                      END IF
                                  END IF
                                  !
                                  CALL LST%NEXT_LINE()
                                  POS = POS + ONE
                   !                  12345678
      ELSEIF(LST%LN(ISTART:ISTOP) == 'ELSE_IF' .AND. IF_COUNT == Z) THEN
                                  !
                                  COND=VAR%SOLVE_LOGICAL(LST%LN(8:), ERRMSG)
                                  IF(PRNT.NE.Z) THEN
                                                WRITE(PRNT,'(A)') ' ELSE_IF EVALUATED CONDITION "'//TRIM(ADJUSTL(LST%LN(7:)))//'"'//TAB//TAB//'WHICH RETURNED A "'//NUM2STR(COND,Z,'TR')//'" THAT IS APART OF THE IF BLOCK "IF '//IF_COND//'"'
                                                !
                                                IF(COND) WRITE(PRNT,'(A)')
                                  END IF
                                  !
                                  IF(COND) THEN
                                      IF(ALLOCATED(ERRMSG))  THEN
                                          BLOCK
                                              CHARACTER(:),  ALLOCATABLE:: NON_ASCII
                                              !
                                              CALL ASCII_MSG(LST%LN, NON_ASCII)
                                              !
                                              IF(NON_ASCII.NE.BLNK) ERRMSG = ERRMSG//BLN//NON_ASCII
                                              !
                                          END BLOCK
                                          !
                                          CALL STOP_ERROR(LINE=LST%LN, OUTPUT=VAR%IOUT, MSG='S LANGUAGE ERROR WHILE SOLVING THE CONDITIONAL PART OF AN "IF" STATEMENT.'//NL//'THE FOLLOWING REMARK WAS MADE FROM THE EQUATION PARSER:'//BLN//ERRMSG//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE (WITH THE LAST LINE BEING THE ERROR LINE):'//BLN//LST%NBACK(NINE,NL)//BLN//'PLEASE CHECK YOUR INPUT.'//BLN//'THE FOLLOWING IS A LIST OF VARIABLE NAMES AND THEIR VALUES WHEN THE PROGRAM TERMINATED'//VAR%PRINT_VAR())
                                      END IF
                                  END IF
                                  !
                                  CALL LST%NEXT_LINE()
                                  POS = POS + ONE
                                  !
      ELSEIF(LST%LN(ISTART:ISTOP) == 'ELSE' .AND. IF_COUNT == Z) THEN
                                  !
                                  IF(PRNT.NE.Z) WRITE(PRNT,'(A/)') ' ELSE    REACHED FOR      "IF '//IF_COND//'"'//TAB//TAB//'SO THE REMAINING PORTION WILL BE EVALAUTED'
                                  !
                                  COND=TRUE
                                  CALL LST%NEXT_LINE()
                                  POS = POS + ONE
      ELSE
                                  CALL LST%NEXT_LINE()
                                  POS = POS + ONE
      END IF
      !
      IF(LST%LN=='ERROR') THEN
                      CALL LST%POS(IF_START)
                      CALL LST%SET_LN()
                      CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "IF CONDITION" COMMAND,'//NL//'BUT IT WAS NOT TERMINATED WITH AN "ENDIF" OR "END_IF" COMMAND.'//NL//'PLEASE CHECK INPUT TO SEE WHICH "IF" DOES NOT HAVE A RESPECTIVE "ENDIF" TO CLOSE IT.'//BLN//'   ***  NOTE THAT ALGORITHM HAS A HARD TIME DETERMING THE "IF" STATEMENT THAT DOES NOT HAVE A CORRESPONDING "ENDIF"  ***'//NL//'   ***      MOSTLY LIKELY THE BAD IF STATEMENT IS AFTER THE "GUESSED LINE THAT THE ERROR OCCURED" MENTIONED ABOVE.  ***'//BLN//'THE FOLLOWING ARE THE PREVIOUS 10 LINES OF THE S-LANGUAGE CODE FROM THE GUESSED BAD IF STATEMENT LINE:'//BLN//LST%NBACK(NINE,NL))
      END IF
  END DO
  !
  END SUBROUTINE
  !
!!!  RECURSIVE SUBROUTINE EVALUATE_IF_LOGICAL(LST,VAR,NLINE,POS,PRNT)
!!!  CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
!!!  CLASS(VARIABLE_LIST),         INTENT(INOUT):: VAR
!!!  INTEGER,                      INTENT(IN   ):: NLINE
!!!  INTEGER,                      INTENT(INOUT):: POS
!!!  INTEGER,                      INTENT(IN   ):: PRNT
!!!  INTEGER:: LLOC,ISTART,ISTOP, IF_START, IF_COUNT
!!!  LOGICAL:: COND, GOTO_ENDIF
!!!  CHARACTER(:),ALLOCATABLE:: IF_COND, CTMP
!!!  !
!!!  IF_COUNT = Z
!!!  IF_START = POS
!!!  GOTO_ENDIF = FALSE
!!!  !       123456
!!!  !      'IF COND'
!!!  !
!!!  !IF_COND = TRIM(ADJUSTL(LST%LN(FOUR:)))
!!!  !
!!!  !CHECK FOR SINGLE LINE IF
!!!  CALL CHECK_SINGLE_LINE_IF(LST%LN, LLOC, ISTART, ISTOP, VAR%IOUT)  !LLOC>Z IF SINGLE LINE IFF FOUND
!!!  !
!!!  ALLOCATE( IF_COND, SOURCE=LST%LN(ISTART:ISTOP) )
!!!  !
!!!  IF( INDEX(IF_COND,'{') > 0 ) THEN
!!!      ALLOCATE(CTMP, SOURCE=IF_COND)
!!!      !
!!!      CALL SOLVE_DATE_LOGICAL_LINE(VAR, COND, IF_COND, LLOC, ISTART, ISTOP)
!!!      !
!!!      IF_COND(:) = CTMP
!!!  ELSE
!!!      COND=VAR%SOLVE_LOGICAL(IF_COND)
!!!  END IF
!!!  !
!!!  IF( LLOC > Z ) THEN !SINGLE LINE IF
!!!     !
!!!     IF(PRNT.NE.Z) WRITE(PRNT,'(/A)') ' SINGLE LINE IF STATEMENT EVALUATED FOR THE FOLLOWING CONDITION "'//IF_COND//'" WHICH RETURNED A '//NUM2STR(COND,Z)//' RESULT FROM LINE "'//TRIM(LST%LN)//'"'
!!!     !
!!!     IF(COND) THEN 
!!!              ISTART = LLOC
!!!              ISTOP  = LEN(LST%LN)
!!!              !
!!!              CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT,USE_ISTART_ISTOP=TRUE)
!!!     ELSE
!!!              CALL LST%NEXT_LINE()
!!!              POS = POS + ONE
!!!     END IF
!!!  ELSE
!!!     IF(PRNT.NE.Z) WRITE(PRNT,'(/A)') ' IF STATEMENT EVALUATED FOR THE FOLLOWING CONDITION "'//IF_COND//'" WHICH RETURNED A '//NUM2STR(COND,Z)//' RESULT FROM LINE "'//TRIM(LST%LN)//'"'
!!!     !
!!!     CALL LST%NEXT_LINE()
!!!     POS = POS + ONE
!!!     !
!!!     DO WHILE (Z < POS)   !POS < Z ==> FOUND EXIT FLAG -- ALREADY BYPASSED TO END_DO/END_WHILE
!!!         !
!!!         LLOC=ONE
!!!         CALL PARSE_WORD(LST%LN,LLOC,ISTART,ISTOP)
!!!         !   
!!!         IF( LST%LN(ISTART:ISTOP)=='ENDIF' .OR. LST%LN(ISTART:ISTOP)== 'END_IF') THEN
!!!                         CALL LST%NEXT_LINE()
!!!                         POS = POS + ONE
!!!                         !
!!!                         IF (IF_COUNT > Z) THEN
!!!                             IF_COUNT = IF_COUNT - ONE
!!!                         ELSE
!!!                             EXIT
!!!                         END IF
!!!                         !
!!!         ELSEIF( LST%LN(ISTART:ISTOP)=='IF' .AND. (GOTO_ENDIF .OR. .NOT. COND) ) THEN   !FOUND AN IF STATEMENT WITHIN ANOTHER IF, BUT WITHIN ITS FALSE PORTION OF THE STATEMENT, SO BYPASS IT THAT WAS NOT WITHIN A TRUE PART OF AN ORIGINAL IF BLOCK
!!!                         !
!!!                         IF( NOT_SINGLE_LINE_IF(LST%LN) ) IF_COUNT = IF_COUNT + ONE
!!!                         !
!!!                         CALL LST%NEXT_LINE()
!!!                         POS = POS + ONE
!!!         ELSEIF(GOTO_ENDIF) THEN
!!!                      CALL LST%NEXT_LINE()
!!!                      POS = POS + ONE
!!!         ELSEIF(COND) THEN
!!!                      CALL EVALUATE_KEYWORDS(LST,VAR,NLINE,POS,LLOC,ISTART,ISTOP,PRNT,GOTO_ENDIF)  !CHECKS FOR DO, IF, OR EVALUATES VALUE, SETS GOTO_ENDIF=TRUE IF ELSE, ELSEIF, IS FOUND
!!!                      !
!!!                      IF(POS == TWO*NLINE) EXIT ! ENCOUNTERED "GO_TO_END"
!!!                      !                  12345678
!!!         ELSEIF(LST%LN(ISTART:ISTOP) == 'ELSEIF' .AND. IF_COUNT == Z) THEN
!!!                                     !
!!!                                     COND=VAR%SOLVE_LOGICAL(LST%LN(7:))
!!!                                     IF(PRNT.NE.Z) WRITE(PRNT,'(/A)') ' ELSEIF STATEMENT IS EVALUATED THE FOLLOWING CONDITION "'//TRIM(ADJUSTL(LST%LN(7:)))//'" WHICH RETURNED A '//NUM2STR(COND,Z)//' THAT IS APART OF THE IF BLOCK "IF '//IF_COND//'"'
!!!                                     !
!!!                                     CALL LST%NEXT_LINE()
!!!                                     POS = POS + ONE
!!!                      !                  12345678
!!!         ELSEIF(LST%LN(ISTART:ISTOP) == 'ELSE_IF' .AND. IF_COUNT == Z) THEN
!!!                                     !
!!!                                     COND=VAR%SOLVE_LOGICAL(LST%LN(8:))
!!!                                     IF(PRNT.NE.Z) WRITE(PRNT,'(/A)') ' ELSE_IF STATEMENT IS EVALUATED THE FOLLOWING CONDITION "'//TRIM(ADJUSTL(LST%LN(7:)))//'" WHICH RETURNED A '//NUM2STR(COND,Z)//' THAT IS APART OF THE IF BLOCK "IF '//IF_COND//'"'
!!!                                     !
!!!                                     CALL LST%NEXT_LINE()
!!!                                     POS = POS + ONE
!!!                      !
!!!         ELSEIF(LST%LN(ISTART:ISTOP) == 'ELSE' .AND. IF_COUNT == Z) THEN
!!!                                     !
!!!                                     IF(PRNT.NE.Z) WRITE(PRNT,'(/A)') ' ELSE STATEMENT REACHED WITHIN IF BLOCK         "IF '//IF_COND//'" SO THE REMAINING PORTION WILL BE EVALAUTED'
!!!                                     !
!!!                                     COND=TRUE
!!!                                     CALL LST%NEXT_LINE()
!!!                                     POS = POS + ONE
!!!         ELSE
!!!                                     CALL LST%NEXT_LINE()
!!!                                     POS = POS + ONE
!!!         END IF
!!!         !
!!!         IF(LST%LN=='ERROR') THEN
!!!                         CALL LST%POS(IF_START)
!!!                         CALL LST%SET_LN()
!!!                          CALL STOP_ERROR( LINE=LST%LN, OUTPUT=VAR%IOUT, MSG= 'RULES BLOCK ERROR: FOUND OPENING "IF CONDITION" COMMAND,'//NL//'BUT IT WAS NOT TERMINATED WITH AN "ENDIF" OR "END_IF" COMMAND.'//NL//'PLEASE CHECK INPUT TO SEE IF THERE IS LOCATED AFTER THE IF AN RESPECTIVE ENDIF.'//BLN//'   *** NOTE THAT "END IF" IS NOT ALLOWED ***')
!!!         END IF
!!!     END DO
!!!  END IF
!!!  !
!!!  END SUBROUTINE
  !
  SUBROUTINE EVALUATE_LOOKUP_TABLE(VAR,LN,LLOC,ISTART,ISTOP,PRNT)
    CLASS(VARIABLE_LIST),  INTENT(INOUT):: VAR
    CHARACTER(*),          INTENT(INOUT):: LN
    INTEGER,               INTENT(INOUT):: LLOC, ISTART, ISTOP
    INTEGER,               INTENT(IN   ):: PRNT
    CHARACTER(:),ALLOCATABLE:: TABNAM, VARLOOKUP
    !
    CALL GET_WORD(LN,LLOC,ISTART,ISTOP,TABNAM,   IS_ALLOC=TRUE,NO_UPCASE=TRUE)
    CALL GET_WORD(LN,LLOC,ISTART,ISTOP,VARLOOKUP,IS_ALLOC=TRUE,NO_UPCASE=TRUE)
    !
    !!!CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
    !!!ALLOCATE(TABNAM, SOURCE=LN(ISTART:ISTOP))
    !!!!
    !!!CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
    !!!ALLOCATE(VARLOOKUP, SOURCE=LN(ISTART:ISTOP))
    !
    CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
    CALL VAR%TABLE_TO_VARIABLE(TABNAM, VARLOOKUP, LN(ISTART:ISTOP))  !VARSET=LN(ISTART:ISTOP)
    !
    IF(PRNT.NE.Z) WRITE(PRNT,'(/A/)') ' LOOKUP KEYWORD FOUND WITH LOOKUP TABLE "'//TABNAM//'" TO LOOK UP VARIABLE "'//VARLOOKUP//'" WITH VALUE '//NUM2STR(VAR%GET(VARLOOKUP,READVAL=TRUE))//' AND RETURNED RESULT TO "'//LN(ISTART:ISTOP)//'" WITH RETURNED VALUE '//NUM2STR(VAR%GET(LN(ISTART:ISTOP)))
    !
    DEALLOCATE(TABNAM, VARLOOKUP)
    !
  END SUBROUTINE
  !
  SUBROUTINE EVALUATE_TIME_SERIES_TABLE(VAR,LN,LLOC,ISTART,ISTOP,PRNT)
    CLASS(VARIABLE_LIST),  INTENT(INOUT):: VAR
    CHARACTER(*),          INTENT(INOUT):: LN
    INTEGER,               INTENT(INOUT):: LLOC, ISTART, ISTOP
    INTEGER,               INTENT(IN   ):: PRNT
    CHARACTER(:),ALLOCATABLE:: NAM, OPTION, VARLOOKUP,VARLOOKUP2
    DOUBLE PRECISION:: DYEAR1, DYEAR2, VAL
    TYPE(DATE_OPERATOR):: DATE
    INTEGER:: I, POS, OPT_BAK
    !
    CALL GET_WORD(LN,LLOC,ISTART,ISTOP,NAM,IS_ALLOC=TRUE,NO_UPCASE=TRUE)
    !
    POS = NEG                  !SEARCH FOR TIME SERIES UNIQUE NAME/ID
    DO I = ONE, VAR%NTSF
        IF(NAM==VAR%TSF(I)%NAM) THEN
            POS = I
            EXIT
        END IF
    END DO
    !
    IF(POS == NEG) THEN           ! FAILED TO FIND NAME, STOP PROGRAM
        IF(VAR%NTSF > Z) THEN
            VARLOOKUP = VAR%TSF(ONE)%NAM//NL
            DO I=TWO, VAR%NTSF
                VARLOOKUP = VARLOOKUP//VAR%TSF(I)%NAM//NL
            END DO
            !
            CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE EVALUATE_TIME_SERIES_TABLE) FOUND KEYWORD TIME_SERIES,'//NL//'WHICH SHOULD BE FOLLOWED BY THE TIME SERIES NAME THAT WILL BE USED,'//NL//'BUT FAILED TO LOCATE TIME SERIES NAME: '//NAM//NL//'FROM THE LIST OF TIME SERIES NAMES:'//NL//VARLOOKUP )
        ELSE
            CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE EVALUATE_TIME_SERIES_TABLE) FOUND KEYWORD TIME_SERIES,'//NL//'WHICH SHOULD BE FOLLOWED BY THE TIME SERIES NAME THAT WILL BE USED,'//NL//'BUT FAILED TO LOCATE TIME SERIES NAME: '//NAM//NL//'DUE TO NO TIME SERIES FILES BEING SPECIFIED'//NL )
        END IF
    END IF
    !
    OPT_BAK = VAR%TSF(POS)%OPT    !CHECK FOR OPTIONAL "OPTION" OVERRIDE
    !
    CALL VAR%TSF(POS)%GET_OPTION(LN, LLOC, ISTART, ISTOP, VAR%TSF(POS)%OPT)
    !
    IF(    VAR%TSF(POS)%OPT == -1) THEN
                                   CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST/TIME_SERIES INPUT ERROR: FOUND OVERRIDE OPTION "SKIP" FOR TIME SERIES FILE,'//NL//'BUT THIS IS NOT ALLOWED NOR DOES IT MAKE SENSE TO DO THIS. PLEASE JUST DELETE THE LINE OR SET THE VARIABLE TO ZERO IF THAT IS WHAT IS DESIRED.') 
    ELSEIF(VAR%TSF(POS)%OPT == -2) THEN
                                   CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST/TIME_SERIES INPUT ERROR: FOUND OVERRIDE OPTION "CONSTANT" FOR TIME SERIES FILE,'//NL//'BUT THIS IS NOT ALLOWED NOR DOES IT MAKE SENSE TO DO THIS. PLEASE JUST SET THE VARIABLE TO THE VALUE YOU ARE USING CONSTANT FOR.') 
    ELSEIF(VAR%TSF(POS)%OPT == -3) THEN
        !
        VAR%TSF(POS)%OPT = OPT_BAK
        !
        OPTION = VAR%TSF(POS)%PRINT_OPTION()
    ELSE
        OPTION = LN(ISTART:ISTOP)
    END IF
    !
    IF(PRNT.NE.Z)  THEN
        WRITE(PRNT,'(A)', ADVANCE=NO) ' TIME SERIES FILE VARIABLE "'//NAM//'" WITH OPTION "'//OPTION//'"'
        IF(VAR%TSF(POS)%OPT > NINE) THEN
            WRITE(PRNT,'(3x A)',ADVANCE=NO) 'USES A STARTING VARIABLE "'
        ELSE
            WRITE(PRNT,'(3x A)',ADVANCE=NO) 'USES THE VARIABLE "'
        END IF
    END IF
    !
    I = INDEX(LN,'{')
    IF(I > Z) THEN
        VARLOOKUP = LN(I+ONE:INDEX(LN,'}')-ONE)
    ELSE
        CALL GET_WORD(LN,LLOC,ISTART,ISTOP,VARLOOKUP,IS_ALLOC=TRUE,NO_UPCASE=TRUE)
    END IF
    !
    DYEAR1 = VAR%GET(VARLOOKUP,READVAL=TRUE)
    !
    IF(DYEAR1.NE.DYEAR1) THEN
        !
        CALL DATE%INIT(VARLOOKUP)
        !
        IF(DATE%NOT_SET()) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE EVALUATE_TIME_SERIES_TABLE) UNKNOWN ERROR TRYING TO CONVERT VARABLE NAME TO DECIMAL YEAR, DATE, OR LOOKING UP ITS NAME WITHIN THE VARIABLE NAME LIST.'//NL//'THE FOLLOWING IS THE DATE THAT THE S-Language ATTEMPTED TO CONVERT TO A DECIMAL YEAR OR FIND IN THE VARIABLE LIST: '//VARLOOKUP )
        !
        IF(DATE%NO_YEAR()) THEN
                               DYEAR1 = VAR%GET('YEAR')
                               IF(DYEAR1.NE.DYEAR1) DYEAR1 = VAR%GET('DATE.START')
                               IF(DYEAR1.NE.DYEAR1) DYEAR1 = VAR%GET('DATE.END')
                               IF(DYEAR1.NE.DYEAR1) DYEAR1 = VAR%GET('TS.START.YEAR')
                               IF(DYEAR1.NE.DYEAR1) DYEAR1 = VAR%GET('TS.START.DYEAR')
                               IF(DYEAR1.NE.DYEAR1) DYEAR1 = VAR%GET('TS.END.YEAR')
                               IF(DYEAR1.NE.DYEAR1) DYEAR1 = VAR%GET('TS.END.DYEAR')
                               IF(DYEAR1.NE.DYEAR1) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE EVALUATE_TIME_SERIES_TABLE) DATE WAS DEFINED WITH A \ WHICH INDICATES IT IS LOADED AS mm\dd'//NL//'IF IT IS OF THE FORM {mm\dd} YOU MUST ALSO DEFINE A REGULAR VARIABLE "YEAR" IN ORDER TO GET THE YEAR FOR THE DATE'//NL//'OR HAVE ONE OF THE FOLLOWING SYSTEM VARIABLES DEFINED (WHICH YEAR YEAR IS TAKEN FROM IN ORDER OF PRIORITY)'//NL//'"DATE.START", "DATE.END", OR "TS.START.YEAR" OR "TS.START.DYEAR" OR "TS.END.YEAR" OR "TS.END.DYEAR".' )
                               !
                               CALL DATE%SET_YEAR(INT(DYEAR1))
                               !
                               VARLOOKUP = VARLOOKUP//'" WHICH WAS CONVERTED TO "'//DATE%STR()
        END IF
        DYEAR1 = DATE%DYEAR
    END IF
    !
    IF(PRNT.NE.Z)  WRITE(PRNT,'(A)',ADVANCE=NO) VARLOOKUP//'"  WITH VALUE '//NUM2STR(DYEAR1)//' '
    !
    IF(VAR%TSF(POS)%OPT > NINE) THEN
        !
        I = INDEX(LN,'{')
        IF(I > Z) THEN
            VARLOOKUP2 = LN(I+ONE:INDEX(LN,'}')-ONE)
        ELSE
            CALL GET_WORD(LN,LLOC,ISTART,ISTOP,VARLOOKUP2,IS_ALLOC=TRUE,NO_UPCASE=TRUE)
        END IF
        !
        DYEAR2 = VAR%GET(VARLOOKUP2,READVAL=TRUE)
        !
        IF(DYEAR2.NE.DYEAR2) THEN
            !
            CALL DATE%INIT(VARLOOKUP2)
            !
            IF(DATE%NOT_SET()) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE EVALUATE_TIME_SERIES_TABLE) UNKNOWN ERROR TRYING TO CONVERT VARABLE NAME TO DECIMAL YEAR, DATE, OR LOOKING UP ITS NAME WITHIN THE VARIABLE NAME LIST.'//NL//'THE FOLLOWING IS THE DATE THAT THE S-Language ATTEMPTED TO CONVERT TO A DECIMAL YEAR OR FIND IN THE VARIABLE LIST: '//VARLOOKUP2 )
            !
            IF(DATE%NO_YEAR()) THEN
                                   DYEAR2 = VAR%GET('YEAR')
                                   IF(DYEAR2.NE.DYEAR2) DYEAR2 = VAR%GET('DATE.START')
                                   IF(DYEAR2.NE.DYEAR2) DYEAR2 = VAR%GET('DATE.END')
                                   IF(DYEAR2.NE.DYEAR2) DYEAR2 = VAR%GET('TS.START.YEAR')
                                   IF(DYEAR2.NE.DYEAR2) DYEAR2 = VAR%GET('TS.START.DYEAR')
                                   IF(DYEAR2.NE.DYEAR2) DYEAR2 = VAR%GET('TS.END.YEAR')
                                   IF(DYEAR2.NE.DYEAR2) DYEAR2 = VAR%GET('TS.END.DYEAR')
                                   IF(DYEAR2.NE.DYEAR2) CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE EVALUATE_TIME_SERIES_TABLE) DATE WAS DEFINED WITH A \ WHICH INDICATES IT IS LOADED AS mm\dd'//NL//'IF IT IS OF THE FORM {mm\dd} YOU MUST ALSO DEFINE A REGULAR VARIABLE "YEAR" IN ORDER TO GET THE YEAR FOR THE DATE'//NL//'OR HAVE ONE OF THE FOLLOWING SYSTEM VARIABLES DEFINED (WHICH YEAR YEAR IS TAKEN FROM IN ORDER OF PRIORITY)'//NL//'"DATE.START", "DATE.END", OR "TS.START.YEAR" OR "TS.START.DYEAR" OR "TS.END.YEAR" OR "TS.END.DYEAR".' )
                                   !
                                   CALL DATE%SET_YEAR(INT(DYEAR2))
                                   
                                   VARLOOKUP2 = VARLOOKUP2//'" WHICH WAS CONVERTED TO "'//DATE%STR()
            END IF
            DYEAR2 = DATE%DYEAR
        END IF
        !
        IF(PRNT.NE.Z)  WRITE(PRNT,'(A)',ADVANCE=NO) 'AND ENDING VARIABLE "'//VARLOOKUP2//'"  WITH VALUE '//NUM2STR(DYEAR2)//' '
    END IF
    !
    CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
    !
    IF(VAR%TSF(POS)%OPT < TEN) THEN
        CALL VAR%TSF(POS)%GET(DYEAR1, VAL)
    ELSE
        CALL VAR%TSF(POS)%GET(DYEAR1, VAL, DYEAR2)
        DEALLOCATE(VARLOOKUP2)
    END IF
    !
    CALL VAR%SET(LN(ISTART:ISTOP), VAL)
    !
    IF(PRNT.NE.Z) WRITE(PRNT,'(A)') 'THAT SET "'//LN(ISTART:ISTOP)//'" TO '//NUM2STR(VAL)
    !
    DEALLOCATE(NAM, VARLOOKUP)
    VAR%TSF(POS)%OPT = OPT_BAK
    !
  END SUBROUTINE
  !
  !################################################################
  !
  !
!  SUBROUTINE SET_REQUIRED_VARIABLE(VAR,LN,LLOC,ISTART,ISTOP,REQ)
!    CLASS(S_VARIABLE_LIST),  INTENT(INOUT):: VAR
!    CHARACTER(*),          INTENT(INOUT):: LN
!    INTEGER,               INTENT(INOUT):: LLOC, ISTART, ISTOP
!    LOGICAL,               INTENT(IN   ):: REQ
!    INTEGER:: P
!    !
!    CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
!    P = VAR%GET_POS(LN(ISTART:ISTOP))
!    !
!    IF(P>Z) THEN
!        VAR%REQUIRED(P) = REQ
!    ELSE
!        CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE SET_REQUIRED_VARIABLE) FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST.'//NL//'THIS SUBROUTINE IS USED TO SET A VALUE TO A LIST OF VARIABLES USED FOR PARSING AN EQUATION. IF ITS AN ERROR THEN A REQUESTED VARIABLE NAME WAS NOT PRESPECIFIED.'//NL//'THE NAME BEING SEARCHED FOR IS: "'//TRIM(LN(ISTART:ISTOP))//'"'//NL//'THE LIST OF VARIABLE NAMES BEING SEARED IS:'//VAR%PRINT_VAR() )
!        !WRITE(*,'(/A,/A,/A)') 'VARIABLE LIST ERROR: (SUBROUTINE SET_REQUIRED_VARIABLE) FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST.','THIS SUBROUTINE IS USED TO SET A VALUE TO A LIST OF VARIABLES USED FOR PARSING AN EQUATION. IF ITS AN ERROR THEN A REQUESTED VARIABLE NAME WAS NOT PRESPECIFIED.','THE NAME BEING SEARCHED FOR IS: "'//TRIM(LN(ISTART:ISTOP))//'"' 
!        !ERROR STOP
!    END IF
!    !
!    IF(REQ) THEN
!        CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
!        SELECT CASE (LN(ISTART:ISTOP) )
!        CASE("ATLEAST","AT_LEAST"); VAR%AT_LEAST = TRUE
!        CASE("ATMOST","AT_MOST");   VAR%AT_LEAST = FALSE
!        CASE DEFAULT
!                    CALL STOP_ERROR( LINE=LN, OUTPUT=VAR%IOUT, MSG= 'VARIABLE LIST ERROR: (SUBROUTINE SET_REQUIRED_VARIABLE) AFTER VARIABLE NAME YOU MUST SPECIFY "AT_MOST" OR FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST.'//NL//'THIS SUBROUTINE IS USED TO SET A VALUE TO A LIST OF VARIABLES USED FOR PARSING AN EQUATION. IF ITS AN ERROR THEN A REQUESTED VARIABLE NAME WAS NOT PRESPECIFIED.'//NL//'THE NAME BEING SEARCHED FOR IS: "'//TRIM(LN(ISTART:ISTOP))//'"'   )
!                    !WRITE(*,'(/A,/A,/A)') 'VARIABLE LIST ERROR: (SUBROUTINE SET_REQUIRED_VARIABLE) AFTER VARIABLE NAME YOU MUST SPECIFY "AT_MOST" OR FAILED TO LOCATE VARIABLE NAME WITHIN VARIABLE LIST.','THIS SUBROUTINE IS USED TO SET A VALUE TO A LIST OF VARIABLES USED FOR PARSING AN EQUATION. IF ITS AN ERROR THEN A REQUESTED VARIABLE NAME WAS NOT PRESPECIFIED.','THE NAME BEING SEARCHED FOR IS: "'//TRIM(LN(ISTART:ISTOP))//'"' 
!                    !ERROR STOP
!        END SELECT
!    END IF
!    !
!  END SUBROUTINE
  !
  !!!SUBROUTINE INIT_VARIABLES(VAR,IN,IOUT)
  !!!  CLASS(S_VARIABLE_LIST), INTENT(INOUT):: VAR
  !!!  INTEGER,                  INTENT(IN   ):: IN,IOUT
  !!!  !
  !!!  TYPE(VARIABLE_LIST):: SVAR, SYS, RET, OTF, TBL
  !!!  TYPE(GENERIC_BLOCK_READER):: BL
  !!!  TYPE(CHARACTER_LINKED_LIST):: LST
  !!!  INTEGER:: I,N,MAX_LEN, OUT_LEN, NFIL, NTAB, DUP
  !!!  !
  !!!  VAR%IOUT=IOUT
  !!!  !
  !!!  DO I=ONE,FIVE
  !!!       CALL BL%LOAD(IN,IOUT)
  !!!       !
  !!!       SELECT CASE(BL%NAME)
  !!!       CASE('VARIABLE','VARIABLES');IF(BL%NLINE>0) CALL SVAR%LOAD_VARIABLE_LIST(BL,TRUE)
  !!!       CASE('SYSTEM','PROPERTY');   IF(BL%NLINE>0) CALL SYS%LOAD_VARIABLE_LIST(BL,FALSE)
  !!!       CASE('RETURN'  );            IF(BL%NLINE>0) CALL RET%LOAD_VARIABLE_LIST(BL,FALSE)
  !!!       CASE('OUTPUT'  );            IF(BL%NLINE>0) CALL OTF%LOAD_OUTPUT_BLOCK(BL)
  !!!       CASE('LOOKUP'  );            IF(BL%NLINE>0) CALL TBL%LOAD_LOOKUP_BLOCK(BL)
  !!!       CASE DEFAULT
  !!!            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG='SWO INITIALIZATION ERROR: YOU MUST SPECIFY AT MOST FIVE BEGIN/END BLOCKS.'//NL//'THE FIRST BEING BEGIN VARIABLE, THE NEXT BEGIN PROPERTY VARIABLE, THE NEXT BEING BEGIN RETURN VARIABLE, THEN NEXT BEING OUTPUT FOR OUTPUT FILE LIST.'//NL//'YOU MUST SPECIFY THE BLOCKS, BUT THEY CAN BE EMPTY IF YOU DO NOT USE THEM.'//NL//'THE BLOCK NAME THAT WAS FOUND, BUT NOT CORRECT IS "'//BL%NAME//'"'//NL//'(THIS MAY HELP YOU LOCATE WHERE THE ERROR OCCURED)')
  !!!       END SELECT
  !!!  END DO
  !!!  !
  !!!  MAX_LEN = Z
  !!!  IF(ALLOCATED(SVAR%NAM)) MAX_LEN = MAX(MAX_LEN, LEN(SVAR%NAM))
  !!!  IF(ALLOCATED(SYS%NAM))  MAX_LEN = MAX(MAX_LEN, LEN(SYS%NAM ))
  !!!  IF(ALLOCATED(RET%NAM))  MAX_LEN = MAX(MAX_LEN, LEN(RET%NAM ))
  !!!  !
  !!!  VAR%NVAR = SVAR%N
  !!!  VAR%NSYS = SYS%N
  !!!  VAR%NRET = RET%N
  !!!  !VAR%NEX  = RET%N
  !!!  !
  !!!  DUP = Z
  !!!  IF(SYS%N > Z .AND. RET%N > Z) THEN
  !!!     DO I=ONE, SYS%N
  !!!         DO N = ONE, RET%N
  !!!                          IF(SYS%NAM(I) == RET%NAM(N)) THEN
  !!!                              DUP = DUP + ONE
  !!!                              EXIT
  !!!                          END IF
  !!!         END DO
  !!!     END DO
  !!!  END IF
  !!!  !    
  !!!  VAR%POS_SYS = VAR%NRET + ONE             !position of system variables
  !!!  VAR%POS_VAR = VAR%NRET + VAR%NSYS + ONE  !position of the dummy variables
  !!!  !VAR%POS_RET = ONE
  !!!  !VAR%N    = VAR%NVAR + VAR%NSYS + VAR%NRET
  !!!  !
  !!!  N       = VAR%NVAR + VAR%NSYS + VAR%NRET
  !!!  NTAB    = TBL%NTAB
  !!!  !
  !!!  OUT_LEN = ONE
  !!!  IF(ALLOCATED(OTF%OUT_ID)) OUT_LEN = LEN(OTF%OUT_ID)
  !!!  NFIL    = OTF%NFIL
  !!!  !
  !!!  CALL VAR%INIT(MAX_LEN,N,NTAB,OUT_LEN,NFIL)
  !!!  !
  !!!  DO CONCURRENT (I=ONE:VAR%NRET);   VAR%NAM(I) = RET%NAM(I)
  !!!  END DO
  !!!  !
  !!!  DO CONCURRENT (I=ONE:VAR%NSYS);   VAR%NAM(I+VAR%NRET) = SYS%NAM(I)
  !!!  END DO
  !!!  !
  !!!  I=VAR%POS_VAR
  !!!  VAR%NAM(I:) = SVAR%NAM
  !!!  VAR%VAL(I:) = SVAR%VAL
  !!!  !
  !!!  DO CONCURRENT (I=ONE:NTAB)
  !!!                 VAR%TAB(I)%N   = TBL%TAB(I)%N
  !!!                 VAR%TAB(I)%OPT = TBL%TAB(I)%OPT
  !!!                 ALLOCATE(VAR%TAB(I)%NAM, SOURCE = TBL%TAB(I)%NAM)
  !!!                 CALL MOVE_ALLOC(TBL%TAB(I)%X, VAR%TAB(I)%X)
  !!!                 CALL MOVE_ALLOC(TBL%TAB(I)%Y, VAR%TAB(I)%Y)
  !!!  END DO
  !!!  !
  !!!  DO CONCURRENT (I=ONE:NFIL)
  !!!                 VAR%OUT_ID(I)    = OTF%OUT_ID(I)                             !COPY NAME OF TABLE
  !!!                 CALL OTF%OUT_NAM(I)%MOVE( VAR%OUT_NAM(I) )
  !!!                 !VAR%OUT_NAM(I)%N = OTF%OUT_NAM(I)%N
  !!!                 !ALLOCATE(VAR%OUT_NAM(I)%STR, SOURCE = OTF%OUT_NAM(I)%STR)  !MOVE OVER CHARACTER ARRAY OF VARIABLES TO PRINT OUT
  !!!                 CALL OTF%OUTPUT(I)%MOVE( VAR%OUTPUT(I) )    !MOVE GENERIC_INPUT_FILE TO NEW LOCATION
  !!!  END DO
  !!!  !
  !!!  ALLOCATE(VAR%REQUIRED(VAR%NRET), SOURCE = FALSE)
  !!!  ALLOCATE(VAR%AT_LEAST(VAR%NRET), SOURCE = FALSE)
  !!!  ALLOCATE(VAR%PROP_RET(VAR%NRET))
  !!!  DO I=ONE, VAR%NRET
  !!!                    CALL VAR%PROP_RET(I)%INIT( RET%NAM(I), I )
  !!!  END DO
  !!!  !
  !!!  ALLOCATE(VAR%PROP_PUL(VAR%NSYS))
  !!!  DO I=ONE, VAR%NSYS
  !!!                    CALL VAR%PROP_PUL(I)%INIT( SYS%NAM(I), I+VAR%NRET )  !POS = I+VAR%NRET
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !---
  !
  !ELEMENTAL SUBROUTINE SET_SWO_EXTRA(EX, TYP, FLAG)
  !  CLASS(SWO_EXTRA), INTENT(INOUT):: EX
  !  CHARACTER(*),     INTENT(IN   ):: TYP
  !  INTEGER,          INTENT(IN   ):: FLAG
  !  !
  !  CALL DEALLOCATE_SWO_EXTRA(EX)
  !  IF(TYP.NE.BLNK) THEN
  !     EX%INUSE = TRUE
  !     ALLOCATE(EX%TYP,  SOURCE=TYP)
  !     ALLOCATE(EX%FLAG, SOURCE=FLAG)
  !  END IF
  !  !
  !END SUBROUTINE
  !!
  !ELEMENTAL SUBROUTINE DEALLOCATE_SWO_EXTRA(EX)
  !  CLASS(SWO_EXTRA), INTENT(INOUT):: EX
  !  !
  !  EX%INUSE = FALSE
  !  IF(ALLOCATED(EX%TYP))  DEALLOCATE(EX%TYP)
  !  IF(ALLOCATED(EX%FLAG)) DEALLOCATE(EX%FLAG)
  !  !
  !END SUBROUTINE
  !!
  !ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_SWO_EXTRA(EX)
  !  TYPE(SWO_EXTRA), INTENT(INOUT):: EX
  !  !
  !  CALL DEALLOCATE_SWO_EXTRA(EX)
  !  !
  !END SUBROUTINE
  !
  SUBROUTINE DEALLOCATE_VARIABLE_NAME_MEANING(VMN)
    CLASS(VARIABLE_NAME_MEANING), INTENT(INOUT):: VMN
    !
    VMN%N=Z
    VMN%POS=Z
    IF(ALLOCATED(VMN%PROP)) DEALLOCATE(VMN%PROP)
    IF(ALLOCATED(VMN%ID  )) DEALLOCATE(VMN%ID  )
    !
  END SUBROUTINE
  !
  SUBROUTINE DEALLOCATE_VARIABLE_NAME_MEANING_FINAL(VMN)
    TYPE(VARIABLE_NAME_MEANING):: VMN
    !
    CALL DEALLOCATE_VARIABLE_NAME_MEANING(VMN)
    !
  END SUBROUTINE
  !
  !SUBROUTINE ADD_PROP_VARIABLE_NAME_MEANING(VMN,NAM,ID)
  !  CLASS(VARIABLE_NAME_MEANING), INTENT(INOUT):: VMN
  !  CHARACTER(*),      INTENT(IN):: NAM
  !  INTEGER, OPTIONAL, INTENT(IN):: ID
  !  !
  !  CALL VMN%PROP%ADD(TRIM(ADJUSTL(NAM)))
  !  !
  !  IF(PRESENT(ID)) THEN
  !      CALL VMN%ID%ADD(ID)
  !  ELSE
  !      CALL VMN%ID%ADD(Z)
  !  END IF
  !  !
  !END SUBROUTINE
  !
  PURE SUBROUTINE GET_VARIABLE_NAME_MEANING(VMN, LVL, PROP, ID) 
    CLASS(VARIABLE_NAME_MEANING), INTENT(IN):: VMN
    INTEGER, OPTIONAL,            INTENT(IN ):: LVL
    CHARACTER(:), ALLOCATABLE,    INTENT(OUT):: PROP
    INTEGER,                      INTENT(OUT):: ID
    !
    IF(ALLOCATED(PROP)) DEALLOCATE(PROP)
    !
    IF (LVL > VMN%N) THEN
        ALLOCATE(PROP, SOURCE=BLNK)
        ID = Z
    ELSE
        ALLOCATE(PROP, SOURCE=VMN%PROP(LVL))
        ID = VMN%ID(LVL)
    END IF
    !
  END SUBROUTINE
  !
  !SUBROUTINE PARSE_SINGLE_VARIABLE_NAME(NAM, LST)
  !  CHARACTER(*), INTENT(IN):: NAM
  !  TYPE(CHARACTER_LINKED_LIST):: LST
  !  INTEGER:: A,B
  !  !
  !  CALL LST%INIT()
  !  !
  !  A=Z
  !  B=INDEX(NAM,DOT)
  !  IF(B < ONE) THEN
  !      CALL LST%ADD(NAM)
  !      RETURN
  !  ELSE
  !      CALL LST%ADD(NAM(A+1:B-1))
  !      A=B
  !      B=INDEX(NAM(A+1:),DOT)
  !  END IF
  !  !
  !  DO WHILE (B > Z)
  !        CALL LST%ADD(NAM(A+1:B-1))
  !        A=B
  !        B=INDEX(NAM(A+1:),DOT)
  !  END DO
  !  !
  !  CALL LST%ADD(NAM(A+1:))
  !  !
  !END SUBROUTINE
  !
  !
  !################################################################
  !
  SUBROUTINE PARSE_VARIABLE_NAME(VMN,NAM,POS)  !PARSE NAM AND HOLD ITS LOCATION IN THE MAIN VAR VECTOR (POS)
    CLASS(VARIABLE_NAME_MEANING), INTENT(INOUT):: VMN
    CHARACTER(*), INTENT(IN):: NAM
    INTEGER,      INTENT(IN):: POS
    !CHARACTER(:),DIMENSION(:),ALLOCATABLE:: PARTS
    TYPE(CHARACTER_LINKED_LIST):: LST
    TYPE(CHARACTER_LINKED_LIST):: PROP
    TYPE(INTEGER_LINKED_LIST)  :: ID
    CHARACTER(:), ALLOCATABLE:: TMP  !HAD TO USE TMP TO BY PASS COMPILER BUG FOR LST%ADD(NAM(A+1:B-1)) RESULTED IN A SINGLE CHARACTER, eg A=4, B=5 
    INTEGER:: IERR, A, B
    CHARACTER:: DOT
    !
    CALL VMN%DESTROY()
    VMN%POS = POS
    !--------------------------------------
    DOT = '.'
    A=Z
    B=INDEX(NAM,DOT)
    IF(B < ONE) THEN                               ! NO DOTS TO PARSE!!!
        ALLOCATE( CHARACTER(LEN(NAM))::VMN%PROP(ONE) )
        VMN%PROP = NAM    
        ALLOCATE( VMN%ID(ONE), SOURCE = Z )
        VMN%N=ONE
        RETURN
    ELSE
        TMP = NAM(A+1:B-1)
        CALL PROP%INIT()
        CALL ID  %INIT()
        CALL LST %INIT()
        CALL LST%ADD(TMP)  !ADD FIRST PART
        A=B                         !MOVE LOWER INDEX TO AFTER FIRST DOT
        B=INDEX(NAM(A+1:),DOT)! + A  !SEARCH FOR SECOND DOT
    END IF
    !
    DO WHILE (B > Z)                 !ADD NEXT WORD AND SERACH FOR ANY ADDITIONAL DOTS
          B = B + A                  ! SHIFT TO CORRECT LOCATION
          TMP = NAM(A+1:B-1)
          CALL LST%ADD(TMP)
          A=B
          B=INDEX(NAM(A+1:),DOT)! + A
    END DO
    !
    TMP = NAM(A+1:)
    CALL LST%ADD(TMP)   !ADD WORD AFTER LAST DOT
    !--------------------------------------
    !
    CALL LST%START()
    CALL LST%SET_LN()
    !
    DO WHILE ( LST%IS_ASSOCIATED() )
          !
          IF(IS_INTEGER(LST%LN)) THEN
              !
              READ(LST%LN, *) B   !HOLDS ID TO BE ADDED TO ID
              !
              CALL PROP%ADD('INT')
              CALL ID%ADD(B)
              CALL LST%NEXT_LINE()
          ELSE
             !
             CALL PROP%ADD(LST%LN)
             CALL LST%NEXT_LINE()
             !
             IF ( LST%IS_ASSOCIATED() ) THEN
                 READ(LST%LN, *, IOSTAT=IERR) B   !HOLDS ID TO BE ADDED TO ID
             ELSE
                 IERR = ONE
             END IF
             !
             IF (IERR == Z) THEN
                 CALL ID%ADD(B)
                 CALL LST%NEXT_LINE()
             ELSE
                 CALL ID%ADD(Z)
             END IF
          END IF
    END DO
    !
    VMN%N = PROP%LEN()
    CALL PROP%TOARRAY( VMN%PROP )
    CALL ID  %TOARRAY( VMN%ID   )
    !
  END SUBROUTINE
  !
  !PURE FUNCTION ASCII_MSG(LN)
  !  CHARACTER(*), INTENT(IN):: LN
  !  CHARACTER(:), ALLOCATABLE:: ASCII_MSG
  !  CHARACTER(:), ALLOCATABLE:: NON_ASCII
  !  !
  !  CALL ASCII_CHECK(LN, NON_ASCII)
  !  !
  !  IF(ALLOCATED(NON_ASCII)) THEN
  !      ASCII_MSG = 'NOTE THAT NON-ASCII TEXT WAS LOCATED WITHIN THE LINE.'//NL//     &
  !                  'THE FOLLOWING CHARACTERS ARE NOT STANDARD ASCII TEXT: '//NON_ASCII//BLN// &
  !                  'NON-ASCII TEXT CAN CAUSE STRANGE BEHAVOIR IN S, ESPECIALLY IF ITS SIMILAR TO AN OPERATOR: [ ] ( ) + - / * ^'//NL// &
  !                  'FOR EXAMPLE AN — (em dash) DOES NOT RESULT IN SUBTRACTION COMPARED TO A - (minus symbol).'//BLN//                  &
  !                  'THIS CAN HAPPEN IF YOU COPY/PAST FROM AN ADVANCED EDITOR LIKE MS-WORD.'//NL//                                      &
  !                  'AN EASY FIX IS TO REWRITE YOUR EQUATION IN AN ASCII/UNICODE BASIC TEXT EDITOR.'
  !  ELSE
  !      ASCII_MSG = ''
  !  END IF
  !  !
  !END FUNCTION
  !
  PURE SUBROUTINE ASCII_MSG(LN, MSG)
    CHARACTER(*),              INTENT(IN   ):: LN
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: MSG
    CHARACTER(:), ALLOCATABLE:: NON_ASCII
    !
    CALL ASCII_CHECK(LN, NON_ASCII)
    !
    IF(ALLOCATED(NON_ASCII)) THEN
        MSG = 'NOTE THAT NON-ASCII TEXT WAS LOCATED WITHIN THE LINE.'//NL//     &
              'THE FOLLOWING CHARACTERS ARE NOT STANDARD ASCII TEXT: '//NON_ASCII//BLN// &
              'NON-ASCII TEXT CAN CAUSE STRANGE BEHAVOIR IN S, ESPECIALLY IF ITS SIMILAR TO AN OPERATOR: [ ] ( ) + - / * ^'//NL// &
              'FOR EXAMPLE AN — (em dash) DOES NOT RESULT IN SUBTRACTION COMPARED TO A - (minus symbol).'//BLN//                  &
              'THIS CAN HAPPEN IF YOU COPY/PAST FROM AN ADVANCED EDITOR LIKE MS-WORD.'//NL//                                      &
              'AN EASY FIX IS TO REWRITE YOUR EQUATION IN AN ASCII/UNICODE BASIC TEXT EDITOR.'
    ELSE
        MSG = BLNK
    END IF
    !
  END SUBROUTINE
  !
END MODULE
