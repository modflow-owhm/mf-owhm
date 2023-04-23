!
!  FARM PROCESS VERSION 4.0 (FMP3) FOR MODFLOW-OWHMv2
!
MODULE FMP_DRT_LINK_INTERFACE
  !
  IMPLICIT NONE
  !
  TYPE DRTFLOWLINK
    INTEGER::         FID     = 0
    LOGICAL::         HAS_FLO = .FALSE.
    DOUBLE PRECISION::FLO     = 0.D0
  END TYPE
  !
END MODULE
!
MODULE FMP_GLOBAL
  USE           FMP_DRT_LINK_INTERFACE, ONLY:     DRTFLOWLINK
  USE          FMP_DIMENSION_MODULE, ONLY:      FMP_DIMENSION
  USE           WBS_DATA_FMP_MODULE, ONLY:           WBS_DATA
  USE          WELL_DATA_FMP_MODULE, ONLY:     FARM_WELL_DATA
  USE          SOIL_DATA_FMP_MODULE, ONLY:          SOIL_DATA
  USE          CROP_DATA_FMP_MODULE, ONLY:          CROP_DATA
  USE        OUTPUT_DATA_FMP_MODULE, ONLY:        OUTPUT_DATA
  USE       CLIMATE_DATA_FMP_MODULE, ONLY:       CLIMATE_DATA
  USE       OPTIONS_DATA_FMP_MODULE, ONLY:       OPTIONS_DATA
  USE      SALINITY_DATA_FMP_MODULE, ONLY:      SALINITY_DATA
  USE     ALLOTMENT_DATA_FMP_MODULE, ONLY:     ALLOTMENT_DATA
  USE SURFACE_WATER_DATA_FMP_MODULE, ONLY: SURFACE_WATER_DATA
  USE SFR_INPUT_DATA_TYPES,             ONLY: FMP_FLOW_REMOVE_SFR
  USE           SWO_DATA_FMP_MODULE, ONLY:       SWO_DATA
  !    
  IMPLICIT NONE
  !
  PRIVATE:: DRTFLOWLINK
  PRIVATE:: FMP_DIMENSION, WBS_DATA, FARM_WELL_DATA, SOIL_DATA
  PRIVATE:: CROP_DATA, CLIMATE_DATA, ALLOTMENT_DATA, SWO_DATA
  !
  SAVE
  !
  TYPE(FMP_DIMENSION),                            POINTER:: FDIM
  TYPE(WBS_DATA),                                 POINTER:: WBS
  TYPE(FARM_WELL_DATA),    DIMENSION(:),          POINTER:: FWELL      ! Fix someday to not be an array
  TYPE(SURFACE_WATER_DATA),                       POINTER:: SWFL
  TYPE(SOIL_DATA),                                POINTER:: SOIL
  TYPE(SALINITY_DATA),                            POINTER:: SALT
  TYPE(CLIMATE_DATA),                             POINTER:: CLIMATE
  TYPE(ALLOTMENT_DATA),                           POINTER:: ALLOT
  TYPE(CROP_DATA),                                POINTER:: FCROP
  TYPE(SWO_DATA),                                 POINTER:: SWODAT
  TYPE(OPTIONS_DATA),                             POINTER:: FMPOPT
  TYPE(OUTPUT_DATA),                              POINTER:: FMPOUT
  !
  INTEGER,                                        POINTER:: ISTARTFL
  !
  INTEGER, DIMENSION(:),CONTIGUOUS,               POINTER:: IFA,ISA,ICA
  !
  TYPE(FMP_FLOW_REMOVE_SFR),                      POINTER:: SFR_DELIV
  DOUBLE PRECISION, DIMENSION(:),     CONTIGUOUS, POINTER:: SFR_RUNOFF
  LOGICAL,          DIMENSION(:),     CONTIGUOUS, POINTER:: FMP_MOD_SFR_RUNOFF
  !
  DOUBLE PRECISION, DIMENSION(:),     CONTIGUOUS, POINTER:: TFDROLD
  !
  DOUBLE PRECISION, DIMENSION(:,:),   CONTIGUOUS, POINTER:: UNRD,NRD
  DOUBLE PRECISION, DIMENSION(:,:,:), CONTIGUOUS, POINTER:: RNRD
  !
  TYPE(DRTFLOWLINK),DIMENSION(:),     CONTIGUOUS, POINTER:: DRTFLOW
  !
  TYPE FMPTYPE 
      !
      TYPE(FMP_DIMENSION),                   POINTER:: FDIM
      TYPE(WBS_DATA),                        POINTER:: WBS
      TYPE(FARM_WELL_DATA),    DIMENSION(:), POINTER:: FWELL      ! Fix someday to not be an array
      TYPE(SURFACE_WATER_DATA),              POINTER:: SWFL
      TYPE(SOIL_DATA),                       POINTER:: SOIL
      TYPE(SALINITY_DATA),                   POINTER:: SALT
      TYPE(CLIMATE_DATA),                    POINTER:: CLIMATE
      TYPE(ALLOTMENT_DATA),                  POINTER:: ALLOT
      TYPE(CROP_DATA),                       POINTER:: FCROP
      TYPE(SWO_DATA),                        POINTER:: SWODAT
      TYPE(OPTIONS_DATA),                    POINTER:: FMPOPT
      TYPE(OUTPUT_DATA),                     POINTER:: FMPOUT
      !
      INTEGER,                                        POINTER:: ISTARTFL
      !
      INTEGER, DIMENSION(:),CONTIGUOUS,               POINTER::IFA,ISA,ICA
      !
      TYPE(FMP_FLOW_REMOVE_SFR),                      POINTER:: SFR_DELIV
      DOUBLE PRECISION, DIMENSION(:),     CONTIGUOUS, POINTER:: SFR_RUNOFF
      LOGICAL,          DIMENSION(:),     CONTIGUOUS, POINTER:: FMP_MOD_SFR_RUNOFF
      !
      DOUBLE PRECISION, DIMENSION(:),     CONTIGUOUS, POINTER:: TFDROLD
      !
      DOUBLE PRECISION, DIMENSION(:,:),   CONTIGUOUS, POINTER:: UNRD,NRD
      DOUBLE PRECISION, DIMENSION(:,:,:), CONTIGUOUS, POINTER:: RNRD
      !
      TYPE(DRTFLOWLINK),DIMENSION(:),     CONTIGUOUS, POINTER:: DRTFLOW
       !
  END TYPE
  !
  TYPE(FMPTYPE), DIMENSION(10):: FMPDAT(10)
  !
  CONTAINS
  !
  SUBROUTINE FMP_LGR_PNT(IGRID)
    !
    !     Change FMP data to a different grid.
    !
    INTEGER,INTENT(IN)::IGRID
    !
    FDIM     => FMPDAT(IGRID)%FDIM
    WBS      => FMPDAT(IGRID)%WBS
    FWELL    => FMPDAT(IGRID)%FWELL  
    SWFL     => FMPDAT(IGRID)%SWFL
    SOIL     => FMPDAT(IGRID)%SOIL
    SALT     => FMPDAT(IGRID)%SALT
    CLIMATE  => FMPDAT(IGRID)%CLIMATE
    ALLOT    => FMPDAT(IGRID)%ALLOT
    FCROP    => FMPDAT(IGRID)%FCROP
    SWODAT   => FMPDAT(IGRID)%SWODAT
    FMPOPT   => FMPDAT(IGRID)%FMPOPT
    FMPOUT   => FMPDAT(IGRID)%FMPOUT
    !        
    DRTFLOW  => FMPDAT(IGRID)%DRTFLOW
    !
    ISTARTFL => FMPDAT(IGRID)%ISTARTFL
    !
    IFA      => FMPDAT(IGRID)%IFA
    ISA      => FMPDAT(IGRID)%ISA
    ICA      => FMPDAT(IGRID)%ICA
    !
    SFR_DELIV           => FMPDAT(IGRID)%SFR_DELIV
    SFR_RUNOFF          => FMPDAT(IGRID)%SFR_RUNOFF
    FMP_MOD_SFR_RUNOFF  => FMPDAT(IGRID)%FMP_MOD_SFR_RUNOFF
    !
    TFDROLD => FMPDAT(IGRID)%TFDROLD
    !
    UNRD    => FMPDAT(IGRID)%UNRD
    NRD     => FMPDAT(IGRID)%NRD
    RNRD    => FMPDAT(IGRID)%RNRD
    !
    DRTFLOW => FMPDAT(IGRID)%DRTFLOW
    !
  END SUBROUTINE
  !
  SUBROUTINE FMP_LGR_PNT_SAV(IGRID)
    !
    !  Save FMP data for a grid.
    !
    INTEGER,INTENT(IN)::IGRID
    !
    FMPDAT(IGRID)%FDIM     => FDIM
    FMPDAT(IGRID)%WBS      => WBS
    FMPDAT(IGRID)%FWELL    => FWELL  
    FMPDAT(IGRID)%SWFL     => SWFL
    FMPDAT(IGRID)%SOIL     => SOIL
    FMPDAT(IGRID)%SALT     => SALT
    FMPDAT(IGRID)%CLIMATE  => CLIMATE
    FMPDAT(IGRID)%ALLOT    => ALLOT
    FMPDAT(IGRID)%FCROP    => FCROP
    FMPDAT(IGRID)%SWODAT   => SWODAT
    FMPDAT(IGRID)%FMPOPT   => FMPOPT
    FMPDAT(IGRID)%FMPOUT   => FMPOUT
    !        
    FMPDAT(IGRID)%DRTFLOW  => DRTFLOW
    !
    FMPDAT(IGRID)%ISTARTFL => ISTARTFL
    !
    FMPDAT(IGRID)%IFA      => IFA
    FMPDAT(IGRID)%ISA      => ISA
    FMPDAT(IGRID)%ICA      => ICA
    !
    FMPDAT(IGRID)%SFR_DELIV           => SFR_DELIV
    FMPDAT(IGRID)%SFR_RUNOFF          => SFR_RUNOFF
    FMPDAT(IGRID)%FMP_MOD_SFR_RUNOFF  => FMP_MOD_SFR_RUNOFF
    !
    FMPDAT(IGRID)%TFDROLD => TFDROLD
    !
    FMPDAT(IGRID)%UNRD    => UNRD
    FMPDAT(IGRID)%NRD     => NRD
    FMPDAT(IGRID)%RNRD    => RNRD
    !
    FMPDAT(IGRID)%DRTFLOW => DRTFLOW
    !
  END SUBROUTINE
  ! 
  SUBROUTINE FMP3DA(IGRID)
    !
    !     Save FMP data for a grid. Deallocate FMP MEMORY
    !
    INTEGER,INTENT(IN)::IGRID
    !
    DEALLOCATE(FMPDAT(IGRID)%ISTARTFL) 
    !
    DEALLOCATE(FMPDAT(IGRID)%DRTFLOW)
    !
    IF(FMPDAT(IGRID)%WBS%HAS_SFR) THEN
                                  DEALLOCATE(FMPDAT(IGRID)%SFR_RUNOFF)
                                  DEALLOCATE(FMPDAT(IGRID)%FMP_MOD_SFR_RUNOFF)
    END IF
    !
    DEALLOCATE(FMPDAT(IGRID)%TFDROLD)
    !
    DEALLOCATE(FMPDAT(IGRID)%UNRD)
    DEALLOCATE(FMPDAT(IGRID)%NRD)
    DEALLOCATE(FMPDAT(IGRID)%RNRD)
    !
    DEALLOCATE(FMPDAT(IGRID)%IFA)
    DEALLOCATE(FMPDAT(IGRID)%ISA)
    DEALLOCATE(FMPDAT(IGRID)%ICA)
    !
    DEALLOCATE(FMPDAT(IGRID)%FWELL)
    !
    ! GFORTRAN compiler error work-around for pointer data type FINAL statement
    !
    FDIM=>FMPDAT(IGRID)%FDIM
    FMPDAT(IGRID)%FDIM=> NULL()
    DEALLOCATE(FDIM)
    FDIM=>NULL()
    !
    WBS=>FMPDAT(IGRID)%WBS
    FMPDAT(IGRID)%WBS=> NULL()
    DEALLOCATE(WBS)
    WBS=>NULL()
    !
    ! GFORTRAN compiler error work-around for pointer data type FINAL statement
    !
    SFR_DELIV=>FMPDAT(IGRID)%SFR_DELIV
    FMPDAT(IGRID)%SFR_DELIV=> NULL()
    DEALLOCATE(SFR_DELIV)
    SFR_DELIV=>NULL()
    !
    CLIMATE=>FMPDAT(IGRID)%CLIMATE
    FMPDAT(IGRID)%CLIMATE=> NULL()
    DEALLOCATE(CLIMATE)
    CLIMATE=>NULL()
    !
    ALLOT=>FMPDAT(IGRID)%ALLOT
    FMPDAT(IGRID)%ALLOT=> NULL()
    DEALLOCATE(ALLOT)
    ALLOT=>NULL()
    !
    FCROP=>FMPDAT(IGRID)%FCROP
    FMPDAT(IGRID)%FCROP=> NULL()
    DEALLOCATE(FCROP)
    FCROP=>NULL()
    !
    SOIL=>FMPDAT(IGRID)%SOIL
    FMPDAT(IGRID)%SOIL=> NULL()
    DEALLOCATE(SOIL)
    SOIL=>NULL()
    !
    SALT=>FMPDAT(IGRID)%SALT
    FMPDAT(IGRID)%SALT=> NULL()
    DEALLOCATE(SALT)
    SALT=>NULL()
    !
    SWODAT=>FMPDAT(IGRID)%SWODAT
    FMPDAT(IGRID)%SWODAT=> NULL()
    DEALLOCATE(SWODAT)
    SWODAT=>NULL()
    !
    FMPOPT=>FMPDAT(IGRID)%FMPOPT
    FMPDAT(IGRID)%FMPOPT=> NULL()
    DEALLOCATE(FMPOPT)
    FMPOPT=>NULL()
    !
    FMPOUT=>FMPDAT(IGRID)%FMPOUT
    FMPDAT(IGRID)%FMPOUT=> NULL()
    DEALLOCATE(FMPOUT)
    FMPOUT=>NULL()
    !
    SWFL=>FMPDAT(IGRID)%SWFL
    FMPDAT(IGRID)%SWFL=> NULL()
    DEALLOCATE(SWFL)
    SWFL=>NULL()
    !
    ! NULLIFY THE LOCAL POINTERS
    IF(IGRID.EQ.1)THEN
                      FDIM               =>NULL()
                      WBS                =>NULL()
                      FWELL              =>NULL()
                      SWFL               =>NULL()
                      SOIL               =>NULL()
                      SALT               =>NULL()
                      CLIMATE            =>NULL()
                      ALLOT              =>NULL()
                      FCROP              =>NULL()
                      SWODAT             =>NULL()
                      FMPOPT             =>NULL()
                      FMPOUT             =>NULL()
                      ISTARTFL           =>NULL()
                      IFA                =>NULL()
                      ISA                =>NULL()
                      ICA                =>NULL()
                      SFR_DELIV          =>NULL()
                      SFR_RUNOFF         =>NULL()
                      FMP_MOD_SFR_RUNOFF =>NULL()
                      TFDROLD            =>NULL()
                      UNRD               =>NULL()
                      NRD                =>NULL()
                      RNRD               =>NULL()
                      DRTFLOW            =>NULL()
    END IF
    !
  END SUBROUTINE
  !
END MODULE FMP_GLOBAL
!
!
MODULE FMPBLK
  !
  !     DECLARE SHARED PARAMETERS;
  !     ACCURACY: EXPONENT = NUMBER OF SIGNIFICANT DIGITS BEHIND DECIMAL POINT
  !    (E.G., 10 FOR 1D+10; ONLY NEEDED TO CUT OFF RANDOM REST ADDED 
  !     TO DOUBLE PRECISION VARIABLES THAT WERE CONVERTED FROM REAL(4) -
  !     BUT IRRELEVANT IF DEFAULT REAL KIND WAS SET TO 8 AS COMPILER OPTION)
  !
  DOUBLE PRECISION,PARAMETER :: AC=1D+10 !AC & AR MUST BE THE SAME
  REAL, PARAMETER ::            AR=1E+10
  !
  !   FLOW RATE PRECISION (DEPENDING ON LENUNI AND ITMUNI)       
  DOUBLE PRECISION,PARAMETER :: FPS=1D-10
  !
  !    PRECISION IN ANALYTICAL SOLUTION (ALWAYS IN CENTIMETERS)
  DOUBLE PRECISION,PARAMETER :: EPS=1D-5
  !
  ! TIME PRECISION BEYOND WHICH TIME IS PRINTED AS PG-FORMAT        
  REAL,PARAMETER :: TPL=1D-10 !LOWER
  REAL,PARAMETER :: TPU=1D+07 !UPPER
  !     OPTIMIZATION PRECISION IN SIMPLEX ROUTINE
  DOUBLE PRECISION,PARAMETER :: OPS=1D-10
  !     OTHER
  DOUBLE PRECISION,PARAMETER :: ZERO=0.D0
  REAL, PARAMETER ::            ZER=0.
  !
END MODULE FMPBLK
!
MODULE FMPFUNCT 
  !     ******************************************************************
  !     MODULE CONTAINS FMP FUNCTIONS THAT ARE NOT DEPENDENT ON FMP_GLOBAL
  !     ALLOWS FOR FUNCTION IMPLICIT INTERFACE FROM MODULE
  !     ***THIS MODULE MUST BE COMPILED AFTER GWFMNW1MODULE and GWFMNW2MODULE***
  USE ERROR_INTERFACE, ONLY: STOP_ERROR
  USE CONSTANTS,      ONLY: NL, BLN, ONE, Z, DZ, UNO
  !
  PRIVATE:: STOP_ERROR, NL, BLN, ONE, Z, DZ, UNO
  !
  CONTAINS
  !
  FUNCTION RTFUNC(D,M,N,Y,XACC,IC,IR)      ! seb VARIABLE J REMOVED, ONLY PROVIDES CONVERGENCE ITERATION, BUT NEVER USED. CAN NOT MAKE PURE ROUTINE BECAUSE OF WRITE(*,*) AND STOP STATMENT
    !     ******************************************************************
    !     SOLUTION OF ANALYTICAL FUNTION FITTING VERTICAL PRESSURE HEAD
    !     DISTRIBUTION OVER DEPTH USING BISECTION METHOD.
    !     ******************************************************************
    INTEGER,         INTENT(IN ):: IC,IR
    DOUBLE PRECISION,INTENT(IN ):: D,M,N,Y,XACC
    DOUBLE PRECISION:: RTFUNC    
    !
    INTEGER, PARAMETER:: JMAX=100                                     !MAXIMUM ALLOWED NUMBER OF BISECTIONS.
    DOUBLE PRECISION, PARAMETER:: TOL= 1.D-5                          !PRECISION TO SOLVE ROOT AT
    DOUBLE PRECISION:: X1,X2,DX,F,FMID,XMID
    INTEGER:: J
    CHARACTER(:),ALLOCATABLE:: ERR
    !
    !1===== DEFINE EXCLUSIONS
    IF(D.LE.UNO) THEN
      IF(Y.LT.D) RTFUNC=Y
      IF(Y.GE.D) RTFUNC=D
      RETURN
    ELSE IF(D.GT.UNO.AND.(ABS(D-M).LT.XACC.OR.Y.LE.UNO)) THEN
      RTFUNC=Y
      RETURN
    END IF
    !
    !2===== DEFINE LATERAL BOUNDS X1 AND X2
    IF(Y.GE.D) THEN
        IF(N.GE.1.D-30) THEN
            X1 = ((DLOG(Y/D)/DLOG(M/D))**(UNO/N))*(D-UNO)+UNO - XACC
        ELSE
            X1 = ((DLOG(Y/D)/DLOG(M/D))**(1.D30))*(D-UNO)+UNO - XACC
           !X1 = UNO - XACC
        END IF
    END IF
    IF(Y.LT.D) X1=UNO
    IF(X1.LT.UNO) X1=UNO
    IF(N.GE.1.D-30) THEN
      X2 = ((DLOG((Y+D)/D)/DLOG(M/D))**(UNO/N))*(D-UNO)+UNO +XACC
    ELSE
      X2 = ((DLOG((Y+D)/D)/DLOG(M/D))**(1.D30))*(D-UNO)+UNO +XACC
     !X2 = UNO + XACC
    END IF
    IF(X2.GT.Y) X2=Y +XACC
    !
    !3===== FIND THE ROOT OF A FUNCTION KNOWN TO LIE BETWEEN X1 AND X2.
    FMID=FUNC(X2,D,M,N,Y)
    F=FUNC(X1,D,M,N,Y)
    IF(F*FMID.GE.DZ) CALL STOP_ERROR( MSG='RTFUNC - ANALYTICAL FUNTION FITTING VERTICAL PRESSURE '//NL//         &
                                                     '         HEAD DISTRIBUTION OVER DEPTH USING BISECTION METHOD'//BLN//  &
                                                     '         BISECTION METHOD FAILED DUE TO INITIAL GUESS NOT BEIGN ON BOTH SIDES OF ROOT.')
    !3A-----ORIENT SEARCH SO THAT F>0 LIES AT X+DX    
    IF(F.LT.DZ)THEN
        RTFUNC=X1
        DX=X2-X1
    ELSE
        RTFUNC=X2
        DX=X1-X2
    ENDIF
    !
    !3B-----BISECTION LOOP
    !       (REDEFINE ROOT, RETURNED AS RTFUNC, UNTIL ITS ACCURACY IS ? XACC)
    DO J=1,JMAX
        DX=DX*.5D0
        XMID=RTFUNC+DX
        FMID=FUNC(XMID,D,M,N,Y)
        IF(FMID.LE.DZ) RTFUNC=XMID
        IF(ABS(DX).LT.XACC .OR. ABS(FMID).LE.TOL) EXIT
    END DO
    !
    IF (J.EQ.JMAX)THEN
      ALLOCATE(CHARACTER(300):: ERR)
      WRITE(ERR,'(A,/A,/A,F10.2, 2(A,I5))') 'RTFUNC - ANALYTICAL FUNTION FITTING VERTICAL PRESSURE HEAD DISTRIBUTION OVER DEPTH USING BISECTION METHOD - ', &
                                            'BISECTION METHOD TOOK TOO MANY ITERATIONS.',  &
                                            'DEPTH(PSI) FOR A PRESSURE HEAD OF ',-Y,       &
                                            ' CM AT ROW ',IR,' AND AT COLUMN ',IC
      CALL STOP_ERROR(MSG=ERR)
    END IF
    !    1 FORMAT("TOO MANY BISECTIONS WHEN SOLVING ANALTYICAL FUNCTION ",/,
    !     1"DEPTH(PSI) FOR A PRESSURE HEAD OF ",F10.2,
    !     2" CM AT ROW ",I5," AND AT COLUMN ",I5)
  END FUNCTION
  !
  PURE FUNCTION FUNC(X,D,M,N,Y)
    !     FORMULATION OF ANALYTICAL FUNTION F(X)=Y AS F(X)-Y=0
    !
    DOUBLE PRECISION,INTENT(IN):: X,D,M,N,Y
    DOUBLE PRECISION:: FUNC
    !
    FUNC = D*((M/D)**(((X-UNO)/(D-UNO))**N))+X-D-Y
    !
  END FUNCTION
!
END MODULE FMPFUNCT
!!!C
!!!C
!!!      SUBROUTINE URWORDDP(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,D,IOUT,IN)
!!!C-----VERSION 2 09/21/09 URWORDDP   (MODIFIED VERSION 21AUG2002 OF URWORD - SEE INSERTIONS BY SCHMID)
!!!C     *************************************************************************************************
!!!C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
!!!C     CONVERT THE WORD TO A NUMBER.
!!!C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
!!!C          ENDING CHARACTER POSITIONS OF THE WORD.
!!!C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
!!!C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
!!!C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
!!!C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
!!!C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
!!!C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
!!!C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
!!!C          COMMA, OR TAB                                                      !INSERTED BY SCHMID
!!!C          AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
!!!C          OR COMMA, OR TAB.                                                  !INSERTED BY SCHMID
!!!C          NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
!!!C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
!!!C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
!!!C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
!!!C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
!!!C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
!!!C          A QUOTE CHARACTER.
!!!C        IF NCODE IS 0, THE WORD IS UNMODIFIED AND RETURNED.
!!!C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
!!!C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
!!!C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
!!!C        IF NCODE IS 4, THE WORD IS CONVERTED TO A DOUBLE PRECISION NUMBER.   !INSERTED BY SCHMID
!!!C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
!!!C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
!!!C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
!!!C     ******************************************************************
!!!C        SPECIFICATIONS:
!!!C     ------------------------------------------------------------------
!!!      IMPLICIT NONE
!!!C     ------------------------------------------------------------------
!!!C        ARGUMENTS:
!!!C     ------------------------------------------------------------------
!!!      CHARACTER(*) LINE
!!!      INTEGER ICOL,ISTART,ISTOP,NCODE,N,IOUT,IN
!!!      REAL R      
!!!      DOUBLE PRECISION D
!!!C     ------------------------------------------------------------------
!!!C        LOCAL VARIABLES:
!!!C     ------------------------------------------------------------------
!!!      CHARACTER(20) RW,STRING
!!!      CHARACTER(1) TAB
!!!      CHARACTER(250) FNAME                                               !seb ADDED FILE NAME TO URWORD 
!!!      INTEGER LINLEN,I,J,IDIFF,K,L
!!!C     ------------------------------------------------------------------
!!!      TAB=CHAR(9)
!!!C
!!!C1------Set last char in LINE to blank and set ISTART and ISTOP to point
!!!C1------to this blank as a default situation when no word is found.  If
!!!C1------starting location in LINE is out of bounds, do not look for a
!!!C1------word.
!!!      FNAME=''
!!!      INQUIRE(IN,NAME=FNAME)
!!!      LINLEN=LEN(LINE)
!!!      LINE(LINLEN:LINLEN)=' '
!!!      ISTART=LINLEN
!!!      ISTOP=LINLEN
!!!      LINLEN=LINLEN-1
!!!      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
!!!C
!!!C2------Find start of word, which is indicated by first character that
!!!C2------is not a blank and not a comma,
!!!C2------and not a tab.                                                  !INSERTED BY SCHMID
!!!      DO 10 I=ICOL,LINLEN
!!!      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.','.and.
!!!     1   line(I:I).ne.TAB) GO TO 20                                    !INSERTED BY SCHMID
!!!10    CONTINUE
!!!      ICOL=LINLEN+1
!!!      GO TO 100
!!!C
!!!C3------Found start of word.  Look for end.
!!!C3A-----When word is quoted, only a quote can terminate it.
!!!20    IF(LINE(I:I).EQ.'''') THEN
!!!         I=I+1
!!!         IF(I.LE.LINLEN) THEN
!!!            DO 25 J=I,LINLEN
!!!            IF(LINE(J:J).EQ.'''') GO TO 40
!!!25          CONTINUE
!!!         END IF
!!!C
!!!C3B-----When word is not quoted, space or comma or tab will terminate.  !Modified BY SCHMID
!!!      ELSE
!!!         DO 30 J=I,LINLEN
!!!         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.','.
!!!     1   or.line(J:J).eq.TAB) GO TO 40                                !INSERTED BY SCHMID
!!!30       CONTINUE
!!!      END IF
!!!C
!!!C3C-----End of line without finding end of word; set end of word to
!!!C3C-----end of line.
!!!      J=LINLEN+1
!!!C
!!!C4------Found end of word; set J to point to last character in WORD and
!!!C-------set ICOL to point to location for scanning for another word.
!!!40    ICOL=J+1
!!!      J=J-1
!!!      IF(J.LT.I) GO TO 100
!!!      ISTART=I
!!!      ISTOP=J
!!!C
!!!C4.5------WORD COLLECTED RETURN WITHOUT CONVERTING TO UPPER CASE if NCODE is 0.  seb
!!!      IF(NCODE.EQ.0) THEN
!!!         RETURN
!!!      END IF
!!!C
!!!C5------Convert word to upper case and RETURN if NCODE is 1.
!!!      IF(NCODE.EQ.1) THEN
!!!         IDIFF=ICHAR('a')-ICHAR('A')
!!!         DO 50 K=ISTART,ISTOP
!!!            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')
!!!     1             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
!!!50       CONTINUE
!!!         RETURN
!!!      END IF
!!!C
!!!C6------Convert word to a number if requested.
!!!100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3 .OR. NCODE.EQ.4) THEN               !MODIFIED BY SCHMID
!!!         RW=' '
!!!         L=20-ISTOP+ISTART
!!!         IF(L.LT.1) GO TO 200
!!!         RW(L:20)=LINE(ISTART:ISTOP)
!!!         IF(NCODE.EQ.2) READ(RW,'(I20)',ERR=200) N
!!!         IF(NCODE.EQ.3) READ(RW,'(F20.0)',ERR=200) R
!!!         IF(NCODE.EQ.4) READ(RW,'(D20.0)',ERR=200) D                    !INSERTED BY SCHMID
!!!         IF(RW.EQ.' ' ) GO TO 200                                       !seb ADDED CHECK FOR WHEN THERE IS A FAILED READ. NOTE SOME COMPILERS TO NOT FLAG A READ OF AN EMPTY LINE AS AN ERROR
!!!      END IF
!!!      RETURN
!!!C
!!!C7------Number conversion error.
!!!200   IF(NCODE.EQ.4) THEN                                               !INSERTED BY SCHMID
!!!         STRING= 'A DP-REAL NUMBER'                                     !INSERTED BY SCHMID
!!!         L=16                                                           !INSERTED BY SCHMID
!!!      ELSEIF(NCODE.EQ.3) THEN                                           !MODIFIED BY SCHMID
!!!         STRING= 'A REAL NUMBER'
!!!         L=13
!!!      ELSE
!!!         STRING= 'AN INTEGER'
!!!         L=10
!!!      END IF
!!!C
!!!C7A-----If output unit is negative, set last character of string to 'E'.
!!!      IF(IOUT.EQ.0) THEN
!!!         N=0
!!!         R=0.D0
!!!         LINE(LINLEN+1:LINLEN+1)='E'
!!!         RETURN
!!!C
!!!C7B-----If output unit is positive; write a message to output unit.
!!!      ELSE IF(IOUT.NE.0) THEN
!!!         IF(IN.NE.0) THEN
!!!      WRITE(IOUT,201) IN,TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),
!!!     +                                                        TRIM(LINE)
!!!         ELSE
!!!              WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
!!!         END IF
!!!201      FORMAT(1X,/1X,'FILE UNIT ',I4,' WITH FILE NAME ',A,
!!!     +    ' : ERROR CONVERTING "',A,                                    !seb ADDED FILE NAME TO ERROR REPORTING
!!!     +       '" TO ',A,' IN LINE:',/1X,A)
!!!202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,
!!!     1       '" TO ',A,' IN LINE:',/1X,A)
!!!C
!!!C7C-----If output unit is 0; write a message to default output.
!!!      ELSE
!!!         IF(IN.NE.0) THEN
!!!            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
!!!         ELSE
!!!            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
!!!         END IF
!!!      END IF
!!!C
!!!C7D-----STOP after writing message.
!!!      ERROR STOP
!!!C
!!!C8===== END ==============================================================================================
!!!      END SUBROUTINE
      
