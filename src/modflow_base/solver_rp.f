      SUBROUTINE GWF2NWT1RP(INNWT,KPER,Mxiter,IGRID)
!     NEW READ AND PREPARE ROUTINE DEVELOPED BY SCOTT E BOYCE 8/8/2013
      IMPLICIT NONE
      INTEGER,INTENT(IN   ):: INNWT, KPER, IGRID
      INTEGER,INTENT(INOUT):: Mxiter
      !
      LOGICAL, SAVE:: NORPINPUT
      !
      IF (KPER == 1) THEN                           ! IF KPER=1 THEN PCG INFORMATION HAS ALREADY BEEN READ IN DURING AR SUBROUTINE
                   NORPINPUT = .FALSE.
      ELSEIF(.not. NORPINPUT) THEN 
                       CALL SGWF2NWT1PNT(IGRID)
                       CALL SGWF2UPW1PNT(IGRID)
                       !
                       CALL GWF2NWT1RP_EVAL(INNWT,KPER,MXITER,NORPINPUT)
                       !
                       CALL SGWF2NWT1PSV(IGRID)
      END IF
      !
      
!
      END SUBROUTINE GWF2NWT1RP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE GWF2NWT1RP_EVAL(INNWT,KPER,Mxiter,NORPINPUT)
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
      USE GWFNWTMODULE
      USE GLOBAL,               ONLY: IOUT,NCOL,NROW
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD, PARSE_WORD_UP
      USE STRINGS,              ONLY: GET_INTEGER, GET_NUMBER
      USE ULOAD_AND_SFAC_INTERFACE, ONLY: ULOAD
      IMPLICIT NONE
      INTEGER,INTENT(IN   ):: INNWT, KPER
      INTEGER,INTENT(INOUT):: Mxiter
      LOGICAL,INTENT(INOUT):: NORPINPUT
      !
      CHARACTER(768):: LINE
      INTEGER::LLOC, LLOCSAVE, ISTART, ISTOP, I
      INTEGER::MXITERDUM,LINMETHDUM,IPRNWTDUM,IBOTAVDUM
      REAL::R, TOLDUM, FTOLDUM, THETADUM, AMOMENTDUM,THICKDUM
      REAL::AKAPPADUM, GAMMADUM, BREDUCDUM, BTOLDUM
      LOGICAL:: ADJUST_MXIter, EOF
      DOUBLE PRECISION:: THK
      !
      WRITE(IOUT,'(/,A,/)')
     +          'READING NWT SOLVER STRESS PERIOD INFORMATION'
      !
      CALL READ_TO_DATA(LINE, INNWT, EOF=EOF)
      IF (EOF) THEN                                               !DO NOTHING IF FAILED TO READ IN LINE
        NORPINPUT=.TRUE.
          WRITE(IOUT,'(/,A)') REPEAT('#',120)
          WRITE(IOUT,100)"NWT INPUT FILE STRESS PERIOD ",
     +   "CONVERGENCE INFORMATION FAILED READ FOR STRESS PERIOD",
     +   KPER," REUSING PREVIOUSLY DEFINED CRITERIA FOR REMAINDER OF",
     +   " SIMULATION."
          WRITE(IOUT,'(A)')REPEAT('#',120)
100      FORMAT(2A,I6,/,2A)
        RETURN
      END IF
      !
      i = 1
      r = 1.
      LLOC=1
      !
      CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
      IF (LINE(ISTART:ISTOP).EQ.'-1')THEN                               !REUSE PREVIOUS STRESS PERIODS INPUT
        WRITE(IOUT,101)"REUSING PREVIOUSLY DEFINED",
     +    " NWT INPUT CONVERGENCE INFORMATION FOR STRESS PERIOD: ", KPER
101      FORMAT(/,2A,I6)
        RETURN
      END IF
      !
      LLOC=ISTART
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,TOLDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,FTOLDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,MXITERDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,THICKDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,LINMETHDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,IPRNWTDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INNWT,IBOTAVDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      IF(NORPINPUT)THEN
         WRITE(IOUT,'(/,A)')REPEAT('#',120)
         WRITE(IOUT,102)"NWT INPUT FILE STRESS PERIOD ",
     +   "CONVERGENCE INFORMATION FAILED READ FOR STRESS PERIOD",
     +   KPER," REUSING PREVIOUSLY DEFINED CRITERIA FOR REMAINDER OF",
     +   " SIMULATION."
         WRITE(IOUT,'(A)')REPEAT('#',120)
102      FORMAT(2A,I6,2A)
        RETURN
      END IF
      
      IF(LINMETHDUM.NE.LINMETH)THEN
        WRITE(IOUT,*)'NWT SOLVER RP DOES NOT ALLOW CHANGING OF SOLVER'
        WRITE(IOUT,*)'CHANGING LINMETH FROM ',LINMETHDUM,' TO ',LINMETH
      WRITE(IOUT,*)'IF YOU ARE USING "SPECIFY" THIS WILL CAUSE PROBLEMS'
       LINMETHDUM=LINMETH
      ENDIF
      !
      WRITE(IOUT,'(/,2A,I6,A)')"************",
     +   "NWT INPUT FILE CONVERGENCE INFORMATION FOR STRESS PERIOD",
     +   KPER,"************"
C      
C3B-----GET OPTIONS.
      ADJUST_MXIter = .TRUE.
      IFDPARAM=0
      DO
        LLOCSAVE = LLOC
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        IF(ISTART>ISTOP) EXIT
        !
        SELECT CASE(LINE(ISTART:ISTOP))
        CASE('SIMPLE')
            IFDPARAM=1
        CASE('MODERATE')
            IFDPARAM=2
        CASE('COMPLEX')
            IFDPARAM=3
        CASE('SPECIFIED')
            IFDPARAM=4
           CALL READ_SPECIFIED_PROPERTIES(LINE,LLOC,INNWT,IOUT)
        CASE('MAX_HEAD_CHANGE')
           CALL GET_NUMBER(line, lloc, istart, istop,IOUT,INNWT,HED_LIM,
     1      MSG='"MAX_HEAD_CHANGE" failed to read the HED_LIM value.')
            IF(HED_LIM <=   0D0 ) HED_LIM=IEEE_VALUE(THK,IEEE_QUIET_NAN)
            IF(HED_LIM >= 0.9D30) HED_LIM=IEEE_VALUE(THK,IEEE_QUIET_NAN)
            IF(HED_LIM <  0.5D0 ) HED_LIM=0.5D0
        CASE('KEEP_MXITER')
            ADJUST_MXIter = .FALSE.
        CASE('CONTINUE', 
     +       'NO_CONVERGENCE_STOP', 
     +       'NO_FAILED_CONVERGENCE_STOP')
            ICNVGFLG = 1
        CASE('HEAD_DISTANCE_ABOVE_GSE_LIMIT')
            IF(UBOUND(GSE_LIM,1) == 1) THEN
               DEALLOCATE(GSE_LIM)
               ALLOCATE(GSE_LIM(NCOL,NROW))
            END IF
            !
            i = 0
            CALL ULOAD(GSE_LIM, LLOC, line, IOUT, INNWT, i, MSG=
     +         '"HEAD_DISTANCE_ABOVE_GSE_LIMIT" failed to read the '//
     +         'NROW by NCOL array of GSE_LIM values.')
        CASE('ALLOW_THIN_CELL')
            CONTINUE
        CASE DEFAULT
                    LLOC = LLOCSAVE
                    EXIT
        END SELECT
      END DO 
!
! Don't need to read these when using default Options.
      IF ( IFDPARAM.EQ.4 ) THEN
         IF(Mxiter < 500 .AND. ADJUST_MXIter) Mxiter = 500
         IF ( BTRACK < 1 ) THEN
            Numtrack  = 0
            Btoldum   = 1.0D0
            Breducdum = 1.0D0
         END IF
      ELSEIF ( IFDPARAM.EQ.1 ) THEN  ! SIMPLE
        IF(Mxiter < 200 .AND. ADJUST_MXIter) Mxiter = 200
        thetadum   = 0.97
        akappadum  = 0.0001
        gammadum   = 0.0
        amomentdum = 0.0
        Btrack     = 0
        Numtrack   = 20
        Btoldum    = 1.5
        Breducdum  = 0.97
      ELSEIF ( IFDPARAM.EQ.2 ) THEN  ! MODERATE
        IF(Mxiter < 300 .AND. ADJUST_MXIter) Mxiter = 300
        thetadum   = 0.90
        akappadum  = 0.0005 !0.00001
        gammadum   = 0.00
        amomentdum = 0.1
        Btrack     = 1 !0
        Numtrack   = 20
        Btoldum    = 1.1
        Breducdum  = 0.9
      ELSEIF ( IFDPARAM.EQ.3 ) THEN  ! COMPLEX
        IF(Mxiter < 500 .AND. ADJUST_MXIter) Mxiter = 500
        thetadum   = 0.85
        akappadum  = 0.005 !0.00001  0.0005
        gammadum   = 0.0
        amomentdum = 0.1
        Btrack     = 1
        Numtrack   = 50
        Btoldum    = 1.1
        Breducdum  = 0.7
      ELSE
        CALL STOP_ERROR(LINE,INNWT,IOUT,MSG=
     +     'NWT has erroneous value for "Options" '//
     +     'Unknown option found: '//LINE(ISTART:ISTOP) )
      END IF
!
      MXITER=MXITERDUM
      THICKFACT=THICKDUM
      LINMETH=LINMETHDUM
      IPRNWT=IPRNWTDUM
      IBOTAV=IBOTAVDUM
      !
      BTOL = BTOLDUM
      BREDUC = BREDUCDUM
      THETA = THETADUM
      AKAPPA = AKAPPADUM
      GAMMA = GAMMADUM
      AMOMENTUM = AMOMENTDUM
      IF ( THETA.LT.CLOSEZERO ) THETA = 0.9D0
      TOL = TOLDUM
      FTOL = FTOLDUM
!2--ECHO NWT INPUT
        WRITE(IOUT,9010) Tol,Ftol,MXITER
        WRITE(IOUT,9011) THETA,AKAPPA,GAMMADUM,AMOMENTUM 
 9010 FORMAT(1X,'  CONVERGENCE CRITERION OF',E15.6,' FOR HEAD SOLUTION',
     +      /1X,'  AND A TOLERANCE OF',E15.6,' FOR FLOW SOLUTION AND ',
     +      /1X,'  A MAXIMUM OF ',I5,' OUTER ITERATIONS. ',//)
 9011 FORMAT(1X,'  D-B-D REDUCTION FACTOR OF ',E15.6,' AND ',
     +      /1X,'  A D-B-D INCREASE FACTOR OF ',E15.6,' AND ',
     +      /1X,'  A D-B-D RELAXATION OF ',E15.6,' AND ', 
     +      /1X,'  A MOMENTUM FACTOR OF ',E15.6,' .',//)
      !
      CONTAINS
         !
         SUBROUTINE READ_SPECIFIED_PROPERTIES(LINE,LOC,IN,IOUT)
           IMPLICIT NONE
           CHARACTER(*), INTENT(IN   ):: LINE
           INTEGER,      INTENT(INOUT):: LOC
           INTEGER,      INTENT(IN   ):: IN, IOUT
           INTEGER:: I,J
           CHARACTER(44):: ER
           CHARACTER(64):: E1, E2, E3, E4, E5, E6, E7, E8
           ER='Found option SPECIFIED, but failed to load: '    ! Lame way of doing error messages, but most compact method for fixed formatting
           E1 = ER // 'DBDTHETA'
           E2 = ER // 'DBDKAPPA'
           E3 = ER // 'DBDGAMMA'
           E4 = ER // 'MOMFACT'
           E5 = ER // 'BACKFLAG'
           E6 = ER // 'MAXBACKITER'
           E7 = ER // 'BACKTOL'
           E8 = ER // 'BACKREDUCE'
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,   thetadum, MSG=E1)  ! thetadum is pulled as global from parent subroutine, ditto for other dum variables
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,  akappadum, MSG=E2)
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,   gammadum, MSG=E3)
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN, amomentdum, MSG=E4)
           CALL GET_INTEGER(LINE,LOC,I,J,IOUT,IN,     Btrack, MSG=E5)
           !
           IF ( BTRACK > 0 ) THEN !.AND. ILGR == 0
              CALL GET_INTEGER(LINE,LOC,I,J,IOUT,IN,  Numtrack, MSG=E6)
              CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,   Btoldum, MSG=E7)
              CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN, Breducdum, MSG=E8)
           END IF
           !
         END SUBROUTINE
         !
      END SUBROUTINE GWF2NWT1RP_EVAL
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      SUBROUTINE PCG7RP(INPCG,KPER,MXITER,IGRID)
C     ******************************************************************
C     ALLOCATE STORAGE FOR PCG ARRAYS AND READ PCG DATA
C     ******************************************************************
      IMPLICIT NONE
      INTEGER,INTENT(IN   ):: INPCG, KPER, IGRID
      INTEGER,INTENT(INOUT):: MXITER
      !
      LOGICAL, SAVE:: NORPINPUT
      !
      IF (KPER == 1) THEN                           ! IF KPER=1 THEN PCG INFORMATION HAS ALREADY BEEN READ IN DURING AR SUBROUTINE
                   NORPINPUT = .FALSE.
      ELSEIF(.not. NORPINPUT) THEN 
                          CALL PCG7PNT(IGRID)
                          !
                          CALL PCG7RP_EVAL(INPCG,KPER,MXITER,NORPINPUT)
                          !
                          CALL PCG7PSV(IGRID)
      END IF
      !
      END SUBROUTINE PCG7RP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE PCG7RP_EVAL(INPCG,KPER,MXITER,NORPINPUT)
      USE GLOBAL,   ONLY:IOUT
      USE PCGMODULE,ONLY:ITER1,NPCOND,NBPOL,IPRPCG,MUTPCG,NITER,
     1                   HCLOSEPCG,RCLOSEPCG,RELAXPCG,DAMPPCG,
     2                   LHCH,HCHG,LRCHPCG,RCHG,IT1,DAMPPCGT,IHCOFADD
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD
      USE STRINGS,              ONLY: GET_INTEGER, GET_NUMBER
C
      IMPLICIT NONE
      CHARACTER(768):: LINE
      INTEGER,INTENT(IN   ):: INPCG, KPER
      INTEGER,INTENT(INOUT):: MXITER
      LOGICAL,INTENT(INOUT):: NORPINPUT
C     ------------------------------------------------------------------
      INTEGER:: MXITERDUM,ITER1DUM,NPCONDDUM,IHCOFADDDUM
      REAL   :: HCLOSEPCGDUM, RCLOSEPCGDUM, RELAXPCGDUM
      REAL   :: DAMPPCGDUM, DAMPPCGTDUM
      INTEGER:: NBPOLDUM, IPRPCGDUM, MUTPCGDUM
      !
      INTEGER::I,ISTART,ISTOP,LLOC,ITMEM,ITMEM_BAK
      REAL::R
      LOGICAL:: READ_IHCOFADD, EOF
      !      
      WRITE(IOUT,'(/,A,/)')
     +          'READING PCG SOLVER STRESS PERIOD INFORMATION'
      !
      CALL READ_TO_DATA(LINE, INPCG, EOF=EOF)
      IF (EOF) THEN                                                !DO NOTHING IF FAILED TO READ IN LINE
        NORPINPUT=.TRUE.
         WRITE(IOUT,'(/,A)')REPEAT('#',120)
         WRITE(IOUT,200)"PCG INPUT FILE STRESS PERIOD ",
     +   "CONVERGENCE INFORMATION FAILED READ FOR STRESS PERIOD",
     +   KPER," REUSING PREVIOUSLY DEFINED CRITERIA FOR REMAINDER OF",
     +   " SIMULATION."
         WRITE(IOUT,'(A)') REPEAT('#',120)
200      FORMAT(2A,I6,/,2A)
        RETURN
      END IF
      LLOC=1
      !
      i = 1
      r = 1.
      LLOC=1
      !
      CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
      IF (LINE(ISTART:ISTOP).EQ.'-1')THEN                               !REUSE PREVIOUS STRESS PERIODS INPUT
        WRITE(IOUT,202)"REUSING PREVIOUSLY DEFINED ",
     +     "PCG INPUT CONVERGENCE INFORMATION FOR STRESS PERIOD: ", KPER
202      FORMAT(2A,I6)
        RETURN
      END IF
C
C-------READ AND PRINT COMMENTS, MXITER,ITER1 AND NPCOND
      LLOC=ISTART
      ITMEM_BAK=MXITER*ITER1
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,MXITERDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,ITER1DUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,NPCONDDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,IHCOFADDDUM,
     +                                              HAS_ERROR=EOF)
      READ_IHCOFADD=EOF
      !
      IF(NORPINPUT)THEN
         WRITE(IOUT,'(A)')REPEAT('#',120)
         WRITE(IOUT,201)"PCG INPUT FILE STRESS PERIOD ",
     +   "CONVERGENCE INFORMATION FAILED READ FOR STRESS PERIOD",
     +   KPER," REUSING PREVIOUSLY DEFINED CRITERIA FOR REMAINDER OF",
     +   " SIMULATION."
         WRITE(IOUT,'(A)')REPEAT('#',120)
201      FORMAT(2A,I6,2A)
        RETURN
      END IF
      !
      CALL READ_TO_DATA(LINE, INPCG, EOF=EOF)
      IF (EOF) THEN 
        NORPINPUT=.TRUE.
         WRITE(IOUT,'(/,A)')REPEAT('#',120)
         WRITE(IOUT,200)"PCG INPUT FILE STRESS PERIOD ",
     +   "CONVERGENCE INFORMATION FAILED READ FOR STRESS PERIOD",
     +   KPER," REUSING PREVIOUSLY DEFINED CRITERIA FOR REMAINDER OF",
     +   " SIMULATION."
         WRITE(IOUT,'(A)') REPEAT('#',120)
        RETURN
      END IF
      !
      IF(NPCOND.NE.NPCONDDUM)THEN
        WRITE(IOUT,*)'PCG SOLVER RP DOES NOT ALLOW CHANGING OF NPCOND'
        WRITE(IOUT,*)'CHANGING NPCOND FROM ', NPCONDDUM,' TO ', NPCOND
       NPCONDDUM=NPCOND
      ENDIF
      !
      MXITER=MXITERDUM
      ITER1=ITER1DUM
      NPCOND=NPCONDDUM
      IF (READ_IHCOFADD)  IHCOFADD=IHCOFADDDUM
!2--ECHO PCG INPUT
          WRITE(IOUT,'(2A,I6,A)')"************",
     +   "PCG INPUT FILE CONVERGENCE INFORMATION FOR STRESS PERIOD",
     +   KPER,"************"
      !
        WRITE (IOUT,509) MXITER, ITER1, NPCOND
  509 FORMAT (' MAXIMUM OF ',I6,' CALLS OF SOLUTION ROUTINE',/,
     &        ' MAXIMUM OF ',I6,
     &        ' INTERNAL ITERATIONS PER CALL TO SOLUTION ROUTINE',/,
     &        ' MATRIX PRECONDITIONING TYPE :',I5)
      IF(IHCOFADD.NE.0) WRITE(IOUT,510)
  510 FORMAT(' IHCOFADD option -- When an active cell is surrounded ',
     & 'by dry cells, convert',/,
     & ' the cell to dry only if storage and head-dependent boundary ',
     & 'flow are 0.')
C
      ITMEM=MXITER*ITER1
      IF(ITMEM.NE.ITMEM_BAK)THEN
        DEALLOCATE(HCHG,LHCH,RCHG,LRCHPCG,IT1)
        !
        ALLOCATE (HCHG(ITMEM))
        ALLOCATE (LHCH(3,ITMEM))
        ALLOCATE (RCHG(ITMEM))
        ALLOCATE (LRCHPCG(3,ITMEM))
        ALLOCATE (IT1(ITMEM))
      END IF
C
C-------READ HCLOSEPCG,RCLOSEPCG,RELAXPCG,NBPOL,IPRPCG,MUTPCG
      !
      DAMPPCGDUM=1.0
      LLOC=1
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,HCLOSEPCGDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,RCLOSEPCGDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,RELAXPCGDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,NBPOLDUM, 
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,IPRPCGDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,MUTPCGDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,DAMPPCGDUM,
     +                                              HAS_ERROR=EOF)
      IF (EOF) NORPINPUT=EOF
      !
      IF (DAMPPCGDUM == 0.0) DAMPPCGDUM = 1.0
      !
      IF ( DAMPPCGDUM < 0.0 ) THEN
          CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INPCG,DAMPPCGTDUM,
     +                                          ERROR_VAL=DAMPPCGDUM)
          DAMPPCGDUM = -DAMPPCGDUM
          IF (DAMPPCGTDUM == 0.0) DAMPPCGTDUM = 1.0
      ELSE
          DAMPPCGTDUM = DAMPPCGDUM
      END IF  
      !
      IF(NORPINPUT)THEN
         WRITE(IOUT,'(A)')REPEAT('#',120)
         WRITE(IOUT,201)"PCG INPUT FILE STRESS PERIOD ",
     +   "CONVERGENCE INFORMATION FAILED READ FOR STRESS PERIOD",
     +   KPER," REUSING PREVIOUSLY DEFINED CRITERIA FOR REMAINDER OF",
     +   " SIMULATION."
         WRITE(IOUT,'(A)')REPEAT('#',120)
        RETURN
      RETURN
      END IF
      
      HCLOSEPCG = HCLOSEPCGDUM
      RCLOSEPCG = RCLOSEPCGDUM
      RELAXPCG  = RELAXPCGDUM
      NBPOL     = NBPOLDUM
      IPRPCG    = IPRPCGDUM
      MUTPCG    = MUTPCGDUM
      DAMPPCG   = DAMPPCGDUM
      DAMPPCGT  = DAMPPCGTDUM
      !
C
C-------PRINT MXITER,ITER1,NPCOND,HCLOSEPCG,RCLOSEPCG,RELAXPCG,NBPOL,IPRPCG,
C-------MUTPCG,DAMPPCG
          WRITE (IOUT,511)
  511   FORMAT (1X,///,36X,'SOLUTION BY THE CONJUGATE-GRADIENT METHOD',
     &        /,35X,43('-'))
        WRITE (IOUT,512) MXITER
  512 FORMAT (1X,19X,'MAXIMUM NUMBER OF CALLS TO PCG ROUTINE =',I9)
        WRITE (IOUT,515) ITER1
  515 FORMAT (1X,23X,'MAXIMUM ITERATIONS PER CALL TO PCG =',I9)
        WRITE (IOUT,520) NPCOND
  520 FORMAT (1X,30X,'MATRIX PRECONDITIONING TYPE =',I9)
        IF (NPCOND.EQ.2) WRITE (IOUT,525)
  525 FORMAT (1X,53X,'THE MATRIX WILL BE SCALED')
        WRITE (IOUT,530) RELAXPCG, NBPOL
  530 FORMAT (1X,7X,'RELAXATION FACTOR (ONLY USED WITH',
     &        ' PRECOND. TYPE 1) =',E15.5,/,1X,
     &        'PARAMETER OF POLYNOMIAL PRECOND.',
     &        ' = 2 (2) OR IS CALCULATED :',I9)
        WRITE (IOUT,535) HCLOSEPCG
  535 FORMAT (1X,24X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5)
        WRITE (IOUT,540) RCLOSEPCG
  540 FORMAT (1X,20X,'RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5)
      IF (IPRPCG.LE.0) IPRPCG = 999
        WRITE (IOUT,545) IPRPCG, MUTPCG
  545 FORMAT (1X,11X,'PCG HEAD AND RESIDUAL CHANGE PRINTOUT INTERVAL =',
     &        I9,/,1X,4X,
     &        'PRINTING FROM SOLVER IS LIMITED(1) OR SUPPRESSED (>1) =',
     &        I9)
        WRITE (IOUT,550) DAMPPCG,DAMPPCGT
  550 FORMAT (1X,27X,'STEADY-STATE DAMPING PARAMETER =',E15.5
     &       /1X,30X,'TRANSIENT DAMPING PARAMETER =',E15.5)
      NITER = 0
C
      END SUBROUTINE PCG7RP_EVAL