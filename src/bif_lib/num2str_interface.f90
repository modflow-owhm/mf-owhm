! 
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE NUM2STR_INTERFACE!, ONLY: NUM2STR, NUM2STR7, INTFMT, NUMFMT
!  NUM2STR(VAL,[GENERAL]) =>
!                            INT2STR(VAL, [PAD])              PAD > 0 is right justified, PAD < 0 is left justified
!                            REAL2STR(VAL,[GENERAL])
!                            DBLE2STR(VAL,[GENERAL])
  !USE CONSTANTS,                    ONLY: Z, 1, TWO, DNEG, DZ, UNO, DIEZ, HECTO, KILO, inf_R, inf, TRUE, FALSE
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: SNG => REAL32, DBL => REAL64
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: NUM2STR, NUM2STR7, INTFMT, NUMFMT, SEQ2STR
  !
  INTERFACE NUM2STR
    MODULE PROCEDURE INTVEC2STR   !(IVAL,[PAD],  [SEP],[ZPAD])  --ZPAD is logical to indicate padding with 000
    MODULE PROCEDURE INT2STR      !(IVAL,[PAD],  [ZPAD])
    MODULE PROCEDURE REAL2STR     !(RVAL,        [GENERAL])
    MODULE PROCEDURE DBLVEC2STR   !(DVAL,[SEP], [PAD], [GENERAL])
    MODULE PROCEDURE DBLE2STR     !(DVAL,[PAD],  [GENERAL])
    MODULE PROCEDURE DBLE2STRDIG  !(DVAL, DIGIT, [PAD])
    MODULE PROCEDURE DBLEPAD2STR  !(DVAL, PAD, IPREC)          --High Precision Printout -- set PAD=0 to autosize, IPREC > 0 for high precision
    MODULE PROCEDURE TF2STR       !(LVAL,[PAD],  [OPT]) --NO OPT or OPT = 0 returns number, OPT = 1 returns T/F OPT = 2 returns TRUE/FALSE
  END INTERFACE
  !
  INTERFACE NUM2STR7
    MODULE PROCEDURE DBLE2STR7
    MODULE PROCEDURE SNGL2STR7
    !
    MODULE PROCEDURE INTVEC2STR   !(IVAL,[PAD],  [SEP],[ZPAD])  --ZPAD is logical to indicate padding with 000
    MODULE PROCEDURE INT2STR      !(IVAL,[PAD],  [ZPAD])
    MODULE PROCEDURE TF2STR       !(LVAL,[PAD],  [OPT]) --NO OPT or OPT = 0 returns number, OPT = 1 returns T/F OPT = 2 returns TRUE/FALSE
  END INTERFACE
  !
  INTERFACE SEQ2STR
    MODULE PROCEDURE SEQ2STR_INT  !(PRE, SEQ_END, [WIDTH], [SEP], [START], [PAD])
    MODULE PROCEDURE SEQ2STR_VEC  !(PRE, SEQ,     [WIDTH], [SEP],          [PAD])
  END INTERFACE
  ! 
  ! Constants used internally to module ----------------------------------------------------
  ! 
  REAL(DBL),     PARAMETER:: DNEG  = -1_dbl
  REAL(DBL),     PARAMETER:: DZ    =  0_dbl
  REAL(DBL),     PARAMETER:: UNO   =  1_dbl
  REAL(DBL),     PARAMETER:: inf    =  HUGE(DZ)*0.99_dbl
  REAL(DBL),     PARAMETER:: ninf   = -inf
  REAL(SNG),     PARAMETER::  inf_R =  HUGE(1.0_sng)*0.99_sng
  REAL(SNG),     PARAMETER:: ninf_R = -inf_R
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION INTFMT(LINE)
    CHARACTER(*):: LINE
    CHARACTER(8):: INTFMT
    INTEGER:: W
    !  '(I10000)'
    W = LEN_TRIM(ADJUSTL(LINE))
    INTFMT = '(I' // TRIM(INT2STR(W)) // ')'
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  FUNCTION NUMFMT(LINE)
    CHARACTER(*):: LINE
    CHARACTER(10):: NUMFMT
    INTEGER:: W
    !  '(F10000.0)'
    W = LEN_TRIM(ADJUSTL(LINE))
    NUMFMT = '(F' // TRIM(INT2STR(W)) // '.0)'
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION TF2STR(LVAL,PAD,OPT) !NO OPT or OPT = 0 returns number, OPT = 1 returns T/F OPT = 2 returns TRUE/FALSE
    LOGICAL,           INTENT(IN):: LVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    INTEGER, OPTIONAL, INTENT(IN):: OPT
    CHARACTER(:), ALLOCATABLE:: TF2STR
    CHARACTER(5)::TF
    !
    IF(.NOT. PRESENT(OPT)) THEN
                           IF(LVAL) THEN
                               TF = '1'
                           ELSE
                               TF = '0'
                           END IF
    ELSEIF(OPT == 1) THEN
                           IF(LVAL) THEN
                               TF = 'T'
                           ELSE
                               TF = 'F'
                           END IF
    ELSEIF(OPT == 2) THEN
                           IF(LVAL) THEN
                               TF = 'True'
                           ELSE
                               TF = 'False'
                           END IF
    ELSEIF(OPT == 3) THEN
                           IF(LVAL) THEN
                               TF = 'TRUE'
                           ELSE
                               TF = 'FALSE'
                           END IF
    ELSEIF(OPT > 3) THEN
                           IF(LVAL) THEN
                               TF = 'true'
                           ELSE
                               TF = 'false'
                           END IF
    ELSE
                           IF(LVAL) THEN
                               TF = '1'
                           ELSE
                               TF = '0'
                           END IF
    END IF
    !   
    IF(PRESENT(PAD)) THEN
        !
        TF = ADJUSTL(TF)
        !
        IF( LEN_TRIM(TF) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           TF2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(TF))//TF )
                ELSE
                           TF2STR = TRIM(TF)//REPEAT(' ',ABS(PAD)-LEN_TRIM(TF))
                END IF
        ELSE
                           TF2STR = TRIM(TF)
        END IF
    ELSE
        TF2STR = TRIM(ADJUSTL(TF))
    END IF
       
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  !PURE FUNCTION TF2STR(LVAL) !,RIGHT
  !  LOGICAL,INTENT(IN):: LVAL
  !  CHARACTER:: TF2STR
  !  !
  !  IF(LVAL) THEN
  !      TF2STR = 'T'
  !  ELSE
  !      TF2STR = 'F'
  !  END IF
  !  !
  !END FUNCTION
  !
  !PURE FUNCTION COND2STR(LVAL,PAD) !,RIGHT
  !  LOGICAL,         INTENT(IN):: LVAL
  !  INTEGER,         INTENT(IN):: PAD
  !  CHARACTER(:),   ALLOCATABLE:: COND2STR
  !  INTEGER:: I
  !  !
  !  I = ABS(PAD)
  !  !
  !  IF (LVAL) THEN
  !      IF(I == 0) I = 5
  !      IF(I <= 4) I = 4 
  !      COND2STR = REPEAT(' ',I-4)//'True'
  !  ELSE
  !      IF(I <= 5) I = 5 
  !      COND2STR = REPEAT(' ',I-5)//'False'
  !  END IF
  !  IF(PAD<0) COND2STR(:) = ADJUSTL(COND2STR)
  !  !
  !END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION INTVEC2STR(IVAL,PAD,SEP,ZPAD) RESULT(STR)
    INTEGER,DIMENSION(:),CONTIGUOUS,INTENT(IN):: IVAL
    INTEGER,     OPTIONAL,          INTENT(IN):: PAD
    CHARACTER(*),OPTIONAL,          INTENT(IN):: SEP
    LOGICAL,     OPTIONAL,          INTENT(IN):: ZPAD
    !LOGICAL,OPTIONAL,INTENT(IN):: RIGHT
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(IVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = INT2STR(IVAL(1),PAD,ZPAD)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//INT2STR(IVAL(I),PAD,ZPAD)
          END DO
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION INT2STR(IVAL,PAD,ZPAD) !,RIGHT
    INTEGER,         INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: INT2STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN 
                               IF(ZPAD) THEN
                                   INT2STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   INT2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   INT2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   INT2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   INT2STR = TRIM(NUM)
          END IF
    ELSE
                                   INT2STR = TRIM(NUM)
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  !PURE FUNCTION REAL2STR(RVAL,GENERAL)
  !  REAL(SNG),       INTENT(IN):: RVAL
  !  LOGICAL,OPTIONAL,INTENT(IN):: GENERAL
  !  CHARACTER(:),  ALLOCATABLE :: REAL2STR
  !  CHARACTER(41)::NUM
  !  LOGICAL::GEN
  !  GEN=FALSE
  !  IF(PRESENT(GENERAL))GEN=GENERAL
  !  !
  !  NUM=''
  !  IF(RVAL.NE.RVAL) THEN
  !      NUM='NaN'
  !  ELSEIF(RVAL.GE.inf_R) THEN
  !      NUM = 'inf'
  !  ELSEIF(RVAL.LE.-inf_R) THEN
  !      NUM = '-inf'
  !  ELSEIF(.NOT. GEN) THEN
  !   !
  !   IF(RVAL==0E0)                               THEN
  !      WRITE(NUM,'(F3.1)') RVAL
  !   ELSEIF(RVAL>=1E10 .OR. RVAL<=-1E10)         THEN
  !      WRITE(NUM,'(ES40.7E2)') RVAL
  !   ELSEIF(RVAL>=1D6 .OR. RVAL<=-1D6)           THEN
  !      WRITE(NUM,'(ES40.7E1)') RVAL
  !   ELSEIF(RVAL>=1E0 .OR. RVAL<=-1E0 )          THEN
  !      WRITE(NUM,'(F40.5)') RVAL
  !   ELSEIF(RVAL>=0.00099E0 .OR. RVAL<=-0.00099E0 )  THEN
  !      WRITE(NUM,'(F40.7)') RVAL
  !   ELSEIF(RVAL>1E-9 .OR. RVAL<-1E-9)           THEN
  !      WRITE(NUM,'(ES40.5E1)') RVAL
  !   ELSEIF(RVAL>0E0 .OR. RVAL<0E0)              THEN
  !      WRITE(NUM,'(ES40.5E2)') RVAL
  !   END IF
  !   !
  !  ELSE
  !      WRITE(NUM,'(ES40.6)') RVAL 
  !  END IF
  !  !
  !  REAL2STR=TRIM(ADJUSTL(NUM))
  !  !
  !END FUNCTION
  PURE FUNCTION REAL2STR(RVAL,PAD,GENERAL)
    REAL(SNG),         INTENT(IN):: RVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    LOGICAL, OPTIONAL, INTENT(IN):: GENERAL
    CHARACTER(:),  ALLOCATABLE :: REAL2STR
    REAL(DBL):: RVAL1C, RVAL10, RVAL1K
    CHARACTER(16)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    LOGICAL::GEN
    !
    GEN=.FALSE.; IF(PRESENT(GENERAL))GEN=GENERAL
    !
    NUM=''
    RVAL10 = 10_sng*RVAL;   RVAL1C = 100_sng*RVAL;   RVAL1K = 1000_sng*RVAL
    !
    IF(RVAL.NE.RVAL) THEN
        NUM='NaN'
    ELSEIF(RVAL.GE.inf_R) THEN
        NUM = 'inf'
    ELSEIF(RVAL.LE.ninf_R) THEN
        NUM = '-inf'
    ELSEIF(.NOT. GEN) THEN
    !
    IF(RVAL==0E0_sng)                 THEN
       WRITE(NUM,'(F3.1)') RVAL
    ELSEIF(RVAL>=1E10_sng .OR. RVAL<=-1E10_sng)         THEN
       WRITE(NUM,'(ES16.7E2)') RVAL
    ELSEIF( RVAL10 == AINT(RVAL10) .AND. (RVAL10>=1_sng.OR.RVAL10<=-1._sng) ) THEN
       WRITE(NUM,'(F16.1)') RVAL
    ELSEIF( RVAL1C == AINT(RVAL1C) .AND. (RVAL1C>=1_sng.OR.RVAL1C<=-1._sng) ) THEN
       WRITE(NUM,'(F16.2)') RVAL
    ELSEIF( RVAL1K == AINT(RVAL1K) .AND. (RVAL1K>=1_sng.OR.RVAL1K<=-1._sng) ) THEN
       WRITE(NUM,'(F16.3)') RVAL
    ELSEIF(RVAL>=1E6_sng .OR. RVAL<=-1E6_sng)          THEN
       WRITE(NUM,'(ES16.7E1)') RVAL
    ELSEIF(RVAL>=1E2_sng .OR. RVAL<=-1E2_sng)          THEN
       WRITE(NUM,'(F16.5)') RVAL
    ELSEIF(RVAL>=0.00099E0_sng .OR. RVAL<=-0.00099E0_sng )  THEN
       WRITE(NUM,'(F16.7)') RVAL
    ELSEIF(RVAL>=1E-9_sng .OR. RVAL<=-1E-9_sng)         THEN
       WRITE(NUM,'(ES16.7E1)') RVAL
    ELSEIF(RVAL>0E0_sng .OR. RVAL<0E0_sng)              THEN
       WRITE(NUM,'(ES16.7E3)') RVAL
    END IF
    !
    ELSE
        WRITE(NUM,'(ES16.6)') RVAL 
    END IF
    !    
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           REAL2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           REAL2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           REAL2STR = TRIM(NUM)
        END IF
    ELSE
        REAL2STR = TRIM(ADJUSTL(NUM))
    END IF
    !
  END FUNCTION 
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DBLVEC2STR(DVAL,SEP,PAD,GENERAL) RESULT(STR)
    REAL(DBL),DIMENSION(:),CONTIGUOUS,INTENT(IN):: DVAL
    CHARACTER(*),OPTIONAL,            INTENT(IN):: SEP
    INTEGER,     OPTIONAL,            INTENT(IN):: PAD
    LOGICAL,     OPTIONAL,            INTENT(IN):: GENERAL
    !LOGICAL,OPTIONAL,INTENT(IN):: RIGHT
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(DVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = DBLE2STR(DVAL(1),PAD,GENERAL)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//DBLE2STR(DVAL(I),PAD,GENERAL)
          END DO
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DBLE2STR(DVAL,PAD,GENERAL)
    REAL(DBL),         INTENT(IN):: DVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    LOGICAL, OPTIONAL, INTENT(IN):: GENERAL
    CHARACTER(:),  ALLOCATABLE :: DBLE2STR
    REAL(DBL):: DVAL1C, DVAL10, DVAL1K
    CHARACTER(16)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    LOGICAL::GEN
    !
    GEN=.FALSE.; IF(PRESENT(GENERAL)) GEN=GENERAL
    !
    NUM=''
    DVAL10 = 10_dbl*DVAL;   DVAL1C = 100_dbl*DVAL;   DVAL1K = 1000_dbl*DVAL
    !
    IF(DVAL.NE.DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL.GE.inf) THEN
        NUM = 'inf'
    ELSEIF(DVAL.LE.ninf) THEN
        NUM = '-inf'
    ELSEIF(.NOT. GEN) THEN
    !
    IF(DVAL==DZ)                 THEN
       WRITE(NUM,'(F3.1)') DVAL
    ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    ELSEIF(DVAL>=1D10 .OR. DVAL<=-1D10)         THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO.OR.DVAL10<=DNEG) ) THEN
       WRITE(NUM,'(F16.1)') DVAL
    ELSEIF( DVAL1C == AINT(DVAL1C) .AND. (DVAL1C>=UNO.OR.DVAL1C<=DNEG) ) THEN
       WRITE(NUM,'(F16.2)') DVAL
    ELSEIF( DVAL1K == AINT(DVAL1K) .AND. (DVAL1K>=UNO.OR.DVAL1K<=DNEG) ) THEN
       WRITE(NUM,'(F16.3)') DVAL
    ELSEIF(DVAL>=1D6 .OR. DVAL<=-1D6)           THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1D2 .OR. DVAL<=-1D2 )          THEN
       WRITE(NUM,'(F16.5)') DVAL
    ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=1D-9 .OR. DVAL<=-1D-9)         THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1D-99 .OR. DVAL<=-1D-99)       THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    ELSEIF(DVAL>0D0 .OR. DVAL<0D0)              THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    END IF
    !
    ELSE
        WRITE(NUM,'(ES16.6)') DVAL 
    END IF
    !    
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           DBLE2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           DBLE2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           DBLE2STR = TRIM(NUM)
        END IF
    ELSE
        DBLE2STR = TRIM(ADJUSTL(NUM))
    END IF
    !
  END FUNCTION 
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DBLE2STRDIG(DVAL,DIGIT,PAD)
    REAL(DBL),        INTENT(IN):: DVAL
    CHARACTER(*),     INTENT(IN):: DIGIT
    INTEGER,OPTIONAL, INTENT(IN):: PAD
    CHARACTER(:),  ALLOCATABLE :: DBLE2STRDIG
    CHARACTER(:),  ALLOCATABLE :: DIG
    REAL(DBL)::  DVAL100
    CHARACTER(41 )::NUM
    !
    NUM=''
    ALLOCATE(DIG, SOURCE=TRIM(ADJUSTL(DIGIT)))
    !DVAL10 = DIEZ*DVAL !FOR CHECKING IF THE SAME TO THE NEARIEST 
    DVAL100=  100_dbl*DVAL
    !
    IF(DVAL.NE.DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL.GE.inf) THEN
        NUM = 'inf'
    ELSEIF(DVAL.LE.ninf) THEN
        NUM = '-inf'
    ELSEIF(DVAL==DZ .OR. DVAL==UNO)             THEN
       WRITE(NUM,'(F3.1)') DVAL
    ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
       WRITE(NUM,'(ES40.'//DIG//'E3)') DVAL
    ELSEIF(DVAL>=1D10 .OR. DVAL<=-1D10)         THEN
       WRITE(NUM,'(ES40.'//DIG//'E2)') DVAL
    !ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO .OR. DVAL10<=DNEG) ) THEN
    !   WRITE(NUM,'(F40.1)') DVAL
    ELSEIF( DVAL100 == AINT(DVAL100) .AND. (DVAL100>=UNO .OR. DVAL100<=DNEG) ) THEN
       WRITE(NUM,'(F40.2)') DVAL
    ELSEIF(DVAL>=1D6 .OR. DVAL<=-1D6)           THEN
       WRITE(NUM,'(ES40.'//DIG//'E1)') DVAL
    ELSEIF(DVAL>=1D2 .OR. DVAL<=-1D2 )          THEN
       WRITE(NUM,'(F40.5)') DVAL
    ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
       WRITE(NUM,'(F40.'//DIG//')') DVAL
    ELSEIF(DVAL>=1D-9 .OR. DVAL<=-1D-9)         THEN
       WRITE(NUM,'(ES40.'//DIG//'E1)') DVAL
    ELSEIF(DVAL>=1D-99 .OR. DVAL<=-1D-99)       THEN
       WRITE(NUM,'(ES40.'//DIG//'E2)') DVAL
    ELSEIF(DVAL>0D0 .OR. DVAL<0D0)              THEN
       WRITE(NUM,'(ES40.'//DIG//'E3)') DVAL
    END IF
    !    
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           DBLE2STRDIG = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           DBLE2STRDIG = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           DBLE2STRDIG = TRIM(NUM)
        END IF
    ELSE
        DBLE2STRDIG = TRIM(ADJUSTL(NUM))
    END IF
    !
  END FUNCTION 
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DBLEPAD2STR(DVAL, PAD, IPREC)   !HIGH PRECISION PRINT OUT
    REAL(DBL), INTENT(IN):: DVAL
    INTEGER,   INTENT(IN):: PAD, IPREC
    CHARACTER(:),  ALLOCATABLE :: DBLEPAD2STR
    REAL(DBL):: DVAL100
    CHARACTER(18)::NUM
    !
    IF( IPREC < 1) THEN
        DBLEPAD2STR = DBLE2STR(DVAL,PAD)
    ELSE
        NUM=''
        !DVAL10 = DIEZ*DVAL !FOR CHECKING IF THE SAME TO THE NEARIEST 
        DVAL100= 100_dbl*DVAL
        !
        IF(DVAL.NE.DVAL) THEN
            NUM='NaN'
        ELSEIF(DVAL.GE.inf) THEN
            NUM = 'inf'
        ELSEIF(DVAL.LE.ninf) THEN
            NUM = '-inf'
        ELSEIF(DVAL==DZ .OR. DVAL==UNO)             THEN
           WRITE(NUM,'(F3.1)') DVAL
        ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
           WRITE(NUM,'(ES18.10E3)') DVAL
        ELSEIF(DVAL>=1D10 .OR. DVAL<=-1D10)         THEN
           WRITE(NUM,'(ES18.11E2)') DVAL
        !ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO .OR. DVAL10<=DNEG) ) THEN
        !   WRITE(NUM,'(F18.1)') DVAL
        ELSEIF( DVAL100 == AINT(DVAL100) .AND. (DVAL100>=UNO.OR.DVAL100<=DNEG) ) THEN
           WRITE(NUM,'(F18.2)') DVAL
        ELSEIF(DVAL>=1D6 .OR. DVAL<=-1D6)           THEN
           WRITE(NUM,'(ES18.12E1)') DVAL
        ELSEIF(DVAL>=1D2 .OR. DVAL<=-1D2 )          THEN
           WRITE(NUM,'(F18.8)') DVAL
        ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
           WRITE(NUM,'(F18.8)') DVAL
        ELSEIF(DVAL>=1D-9 .OR. DVAL<=-1D-9)         THEN
           WRITE(NUM,'(ES18.12E1)') DVAL
        ELSEIF(DVAL>=1D-99 .OR. DVAL<=-1D-99)       THEN
           WRITE(NUM,'(ES18.11E2)') DVAL
        ELSEIF(DVAL>0D0 .OR. DVAL<0D0)              THEN
           WRITE(NUM,'(ES18.10E3)') DVAL
        END IF
        !    
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           DBLEPAD2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           DBLEPAD2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           DBLEPAD2STR = TRIM(NUM)
        END IF
    END IF
    !
  END FUNCTION 
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DBLE2STR7(DVAL,PAD)
    REAL(DBL),        INTENT(IN):: DVAL
    INTEGER,OPTIONAL, INTENT(IN):: PAD
    CHARACTER(:),  ALLOCATABLE :: DBLE2STR7
    CHARACTER(16)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    !
    NUM=''
    !DVAL10 = DIEZ*DVAL !FOR CHECKING IF THE SAME TO THE NEARIEST 
    !
    IF(DVAL.NE.DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL.GE.inf) THEN
        NUM = 'inf'
    ELSEIF(DVAL.LE.ninf) THEN
        NUM = '-inf'
    ELSE
    !
    IF(DVAL==DZ .OR. DVAL==UNO)                 THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    ELSEIF(DVAL>=1D10 .OR. DVAL<=-1D10)         THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    !ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO .OR. DVAL10<=DNEG) ) THEN
    !   WRITE(NUM,'(F16.1)') DVAL
    ELSEIF(DVAL>=1D5 .OR. DVAL<=-1D5)           THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1D2 .OR. DVAL<=-1D2 )          THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=1D-9 .OR. DVAL<=-1D-9)         THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1D-99 .OR. DVAL<=-1D-99)       THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    ELSE!IF(DVAL>0D0 .OR. DVAL<0D0)              THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    END IF
    END IF
    !    
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           DBLE2STR7 = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           DBLE2STR7 = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           DBLE2STR7 = TRIM(NUM)
        END IF
    ELSE
        DBLE2STR7 = TRIM(ADJUSTL(NUM))
    END IF
    !
  END FUNCTION 
  !
  !#########################################################################################################################
  !
  !!!PURE FUNCTION DBLE2STR9(DVAL,PAD,GENERAL)
  !!!  REAL(DBL),         INTENT(IN):: DVAL
  !!!  INTEGER, OPTIONAL, INTENT(IN):: PAD
  !!!  LOGICAL, OPTIONAL, INTENT(IN):: GENERAL
  !!!  CHARACTER(:),  ALLOCATABLE :: DBLE2STR
  !!!  REAL(DBL):: DVAL1C, DVAL10, DVAL1K
  !!!  CHARACTER(12)::NUM !LARGEST POSSIBLE NUMBER IS 9 CHARACTERS
  !!!  LOGICAL::GEN
  !!!  !
  !!!  GEN=.FALSE.; IF(PRESENT(GENERAL))GEN=GENERAL
  !!!  !
  !!!  NUM=''
  !!!  DVAL10 = DIEZ*DVAL;   DVAL1C = HECTO*DVAL;   DVAL1K = KILO*DVAL
  !!!  !
  !!!  IF(DVAL.NE.DVAL) THEN
  !!!      NUM='NaN'
  !!!  ELSEIF(DVAL.GE.inf) THEN
  !!!      NUM = 'inf'
  !!!  ELSEIF(DVAL.LE.-inf) THEN
  !!!      NUM = '-inf'
  !!!  ELSEIF(.NOT. GEN) THEN
  !!!  !
  !!!  IF(DVAL==DZ)                 THEN
  !!!     WRITE(NUM,'(F3.1)') DVAL
  !!!  ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
  !!!     WRITE(NUM,'(ES10.2E3)') DVAL
  !!!  ELSEIF(DVAL>=1D10 .OR. DVAL<=-1D10)         THEN
  !!!     WRITE(NUM,'(ES10.2E2)') DVAL
  !!!  ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO.OR.DVAL10<=DNEG) ) THEN
  !!!     WRITE(NUM,'(F10.1)') DVAL
  !!!  ELSEIF( DVAL1C == AINT(DVAL1C) .AND. (DVAL1C>=UNO.OR.DVAL1C<=DNEG) ) THEN
  !!!     WRITE(NUM,'(F10.2)') DVAL
  !!!  ELSEIF( DVAL1K == AINT(DVAL1K) .AND. (DVAL1K>=UNO.OR.DVAL1K<=DNEG) ) THEN
  !!!     WRITE(NUM,'(F10.3)') DVAL
  !!!  ELSEIF(DVAL>=1D6 .OR. DVAL<=-1D6)           THEN
  !!!     WRITE(NUM,'(ES10.2E1)') DVAL
  !!!  ELSEIF(DVAL>=1D2 .OR. DVAL<=-1D2 )          THEN
  !!!     WRITE(NUM,'(F10.5)') DVAL
  !!!  ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
  !!!     WRITE(NUM,'(F10.7)') DVAL
  !!!  ELSEIF(DVAL>=1D-9 .OR. DVAL<=-1D-9)         THEN
  !!!     WRITE(NUM,'(ES10.2E1)') DVAL
  !!!  ELSEIF(DVAL>=1D-99 .OR. DVAL<=-1D-99)       THEN
  !!!     WRITE(NUM,'(ES10.2E2)') DVAL
  !!!  ELSEIF(DVAL>0D0 .OR. DVAL<0D0)              THEN
  !!!     WRITE(NUM,'(ES10.2E3)') DVAL
  !!!  END IF
  !!!  !
  !!!  ELSE
  !!!      WRITE(NUM,'(ES10.2)') DVAL 
  !!!  END IF
  !!!  !    
  !!!  IF(PRESENT(PAD)) THEN
  !!!      !
  !!!      NUM = ADJUSTL(NUM)
  !!!      !
  !!!      IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
  !!!              IF (PAD>0) THEN
  !!!                         DBLE2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
  !!!              ELSE
  !!!                         DBLE2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
  !!!              END IF
  !!!      ELSE
  !!!                         DBLE2STR = TRIM(NUM)
  !!!      END IF
  !!!  ELSE
  !!!      DBLE2STR = TRIM(ADJUSTL(NUM))
  !!!  END IF
  !!!  !
  !!!END FUNCTION 
  !
  !#########################################################################################################################
  !
  PURE FUNCTION SNGL2STR7(RVAL,PAD)
    REAL(SNG),       INTENT(IN):: RVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    CHARACTER(:),  ALLOCATABLE :: SNGL2STR7
    !
    SNGL2STR7 = DBLE2STR7(REAL(RVAL,DBL),PAD)
    !
  END FUNCTION 
  !
  !#########################################################################################################################
  !
  PURE FUNCTION SEQ2STR_INT(PRE,SEQ_END,WIDTH,SEP,START,PAD) RESULT(STR)
    CHARACTER(*),          INTENT(IN):: PRE
    INTEGER,               INTENT(IN):: SEQ_END
    INTEGER,     OPTIONAL, INTENT(IN):: WIDTH
    CHARACTER(*),OPTIONAL, INTENT(IN):: SEP
    INTEGER,     OPTIONAL, INTENT(IN):: START, PAD
    !
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: WORD
    INTEGER:: I, N, ISTR, W, NSEP
    LOGICAL:: HAS_SEP
    !
    HAS_SEP = PRESENT(SEP)
    !
    IF(PRESENT(START)) THEN
        ISTR = START
    ELSE
        ISTR = 1
    END IF
    !
    IF(PRESENT(WIDTH)) THEN
           W = WIDTH
    ELSE
           W = 0
    END IF
    !
    IF(HAS_SEP) THEN
        NSEP = LEN(SEP)
    ELSE
        NSEP = 0
    END IF
    
    !
    N = SEQ_END - ISTR + 1
    !
    IF(N < 1) THEN
        STR = PRE
    ELSE
        WORD = PRE // INT2STR(ISTR,PAD,.TRUE.)
        !
        IF     (W>0) THEN
                            WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
        ELSEIF (W<0) THEN
                            WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
        END IF
        !
        STR  = WORD
        ISTR = ISTR + 1
    END IF
    !
    IF(N>1) THEN
          DO I=ISTR, SEQ_END
              !
              WORD = PRE // INT2STR(I,PAD,.TRUE.)
              !
              IF     (W>0) THEN
                                  WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
              ELSEIF (W<0) THEN
                                  WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
              END IF
              !
              IF(HAS_SEP) WORD=SEP//WORD
              !
              STR = STR//WORD
              !
          END DO
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION SEQ2STR_VEC(PRE, SEQ, WIDTH, SEP, PAD) RESULT(STR)
    CHARACTER(*),                      INTENT(IN):: PRE
    INTEGER, DIMENSION(:), CONTIGUOUS, INTENT(IN):: SEQ
    INTEGER,     OPTIONAL,             INTENT(IN):: WIDTH
    CHARACTER(*),OPTIONAL,             INTENT(IN):: SEP
    INTEGER,     OPTIONAL,             INTENT(IN):: PAD
    !
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: WORD
    INTEGER:: I, N, W, NSEP
    LOGICAL:: HAS_SEP
    !
    HAS_SEP = PRESENT(SEP)
    !
    IF(PRESENT(WIDTH)) THEN
           W = WIDTH
    ELSE
           W = 0
    END IF
    !
    IF(HAS_SEP) THEN
        NSEP = LEN(SEP)
    ELSE
        NSEP = 0
    END IF
    !
    N = SIZE(SEQ)
    !
    IF(N < 1) THEN
        STR = PRE
    ELSE
        WORD = PRE // INT2STR(SEQ(1),PAD,.TRUE.)
        !
        IF     (W>0) THEN
                            WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
        ELSEIF (W<0) THEN
                            WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
        END IF
        !
        STR  = WORD
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              !
              WORD = PRE // INT2STR(SEQ(I),PAD,.TRUE.)
              !
              IF     (W>0) THEN
                                  WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
              ELSEIF (W<0) THEN
                                  WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
              END IF
              !
              IF(HAS_SEP) WORD=SEP//WORD
              !
              STR = STR//WORD
              !
          END DO
    END IF
    !
  END FUNCTION
END MODULE
!
!