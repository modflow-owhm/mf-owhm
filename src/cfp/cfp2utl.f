C
C
C
C
C
      SUBROUTINE CHECKPARA(LINE,PARA,IOUT,FLAG)
C
C***********************************************************************
C     READ LINE, CHECK FOR PARAMETER KEYWORD AND SET FLAG, IF EXISTENT
C     by Thomas.Reimann@tu-dresden.de 2013 03 25 
C***********************************************************************
C
C PARA      PARAMETER KEYWORD
C LINE		LINE FROM INPUT FILE
C IOUT  	UNIT NUMBER FOR ALL PRINTED OUTPUT
C ISTART    LOCATION FOR THE START OF THE PARAMETER
C IEND      LOCATION FOR THE END OF THE PARAMETER
C LINE		LINE FROM INPUT FILE
C      
      USE CONSTANTS, ONLY: TRUE,FALSE,Z,ONE,NEG,NINER,DZ,DOS,NEARZERO_30
      IMPLICIT NONE
C      
      CHARACTER(80) LINE
      CHARACTER(*) PARA
      INTEGER IOUT,I,II,ISTART,IEND,FLAG
C
C--INITIALIZE FLAG
      FLAG = 0   
C
C--IGNORE BLANK LINES AND COMMENT LINES
      IF(LINE.EQ.' ') GOTO 30
      IF(LINE(1:1).EQ.'#') GOTO 30
C      
C--FIND START AND END OF PARAMETER, COMPARE AND SET FLAG
      ISTART=0
      IEND=80
      DO 20 I=1,80
        IF(LINE(I:I).NE.' '.AND.LINE(I:I).NE.','.AND.LINE(I:I).NE.'_')
     +  THEN
          ISTART=I
          DO 15 II=ISTART,80
            IF((LINE(II:II).EQ.' '.OR.LINE(II:II).EQ.','
     +         .OR.LINE(II:II).EQ.'_').AND.ISTART.NE.0) THEN
              IEND=II
              IF (LINE(ISTART:(IEND-1)).EQ.PARA) THEN
                WRITE(IOUT,*)'KEYWORD ', PARA, ' ACTIVE'
                FLAG = 1
                GOTO 30
              ENDIF  
            ENDIF
  15      CONTINUE
        ENDIF
  20  CONTINUE
C      
  30  RETURN     
      END  
C
C
C
C
C      
      SUBROUTINE EINTRD(INTNO, IN, IOUT, NAME)
C
C  VERSION1                    03FEB1999                          EINTRD
C***********************************************************************
C     READ LINE AND CONVERT TO A INTEGER NUMBER
C***********************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C INTNO    INTEGER NUMBER
C IN       UNIT NUMBER FROM WHICH INPUT FOR THIS PACKAGE WILL BE READ
C IOUT     UNIT NUMBER FOR ALL PRINTED OUTPUT
C ISTART   LOCATION FOR THE START OF THE NUMBER
C IEND     LOCATION FOR THE END OF THE NUMBER
C LINE     LINE FROM INPUT FILE
C NAME     NAME OF THE VARIABLE
C
      USE CONSTANTS, ONLY: TRUE,FALSE,Z,ONE,NEG,NINER,DZ,DOS,NEARZERO_30
      IMPLICIT NONE
C
! ARGUMENTS
      CHARACTER(LEN=8), INTENT(IN) :: NAME
      INTEGER, INTENT(IN) :: IN, IOUT
      INTEGER, INTENT(OUT) :: INTNO
! LOCAL VARIABLES
      CHARACTER(LEN=80) LINE
      INTEGER I, ISTART, IEND
!     ------------------------------------------------------------------
      DO
C
C--READ A LINE; IGNORE BLANK LINES AND COMMENT LINES
        READ (IN, '(A)', ERR=100) LINE
        IF ( LINE.NE.' ' ) THEN
          IF ( LINE(1:1).NE.'#' ) THEN
C--FIND START AND END OF NUMBER: CHARACTER THAT IS NOT A BLANK OR A COMMA
            ISTART = 0
            IEND = 80
            DO I = 1, 80
              IF ( LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' .AND.        
     +             ISTART.EQ.0 ) ISTART = I
              IF ( (LINE(I:I).EQ.' ' .OR. LINE(I:I).EQ.',') .AND.       
     +             ISTART.NE.0 ) THEN
                IEND = I
                EXIT
              ENDIF
            ENDDO
C--CONVERT LINE TO A INTEGER NUMBER
            READ (LINE(ISTART:IEND), '(I5)', ERR=100) INTNO
            WRITE (IOUT, *) NAME, INTNO
            RETURN
          ENDIF
        ENDIF
      ENDDO
C
C--IF AN ERROR OCCURED
C
 100  WRITE (IOUT, *) 'READ ERROR UNIT', IN
      WRITE (IOUT, *) 'PARAMETER ', NAME
C
      END SUBROUTINE EINTRD
C
C
C
C
C        
      SUBROUTINE EINTRD_MULTI(INTNO, IN, IOUT, NAME)
C
C  VERSION1.1                  03FEB1999                          EINTRD
C***********************************************************************
C     READ LINE AND CONVERT TO SEVERAL INTEGER NUMBERS
C     MODIFIED BY THOMAS.REIMANN@TU-DRESDEN.DE
C***********************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C INTNO    INTEGER NUMBER (UP TO 8)
C IN       UNIT NUMBER FROM WHICH INPUT FOR THIS PACKAGE WILL BE READ
C IOUT     UNIT NUMBER FOR ALL PRINTED OUTPUT
C ISTART   LOCATION FOR THE START OF THE NUMBER
C IEND     LOCATION FOR THE END OF THE NUMBER
C LINE     LINE FROM INPUT FILE
C NAME     NAME OF THE VARIABLE
C
      USE CONSTANTS, ONLY: TRUE,FALSE,Z,ONE,NEG,NINER,DZ,DOS,NEARZERO_30
      IMPLICIT NONE
C
! ARGUMENTS
      CHARACTER(LEN=8), INTENT(IN) :: NAME(8)
      INTEGER, INTENT(IN) :: IN, IOUT
      INTEGER, INTENT(OUT) :: INTNO (8)
! LOCAL VARIABLES
      CHARACTER(LEN=80) LINE
      INTEGER I, ISTART, IEND, N, II
C
C--INI
      INTNO = 0      
      DO
C
C--READ A LINE; IGNORE BLANK LINES AND COMMENT LINES
        READ (IN, '(A)', ERR=100) LINE
        IF ( LINE.NE.' ' ) THEN
          IF ( LINE(1:1).NE.'#' ) THEN
C
C--FIND START AND END OF NUMBER: CHARACTER THAT IS NOT A BLANK OR A COMMA
            ISTART = 0
            IEND = 80
            N = 1
            II = 1
            DO
C
C--SEARCH START
              DO I = II, 80
                IF(LINE(I:I).NE.' '.AND.LINE(I:I).NE.',')THEN
                  ISTART = I
                  EXIT
                ENDIF
              ENDDO
              IF(I.EQ.81)GOTO 50
C
C--SEARCH END            
              II = ISTART
              DO I = II, 80
                IF(LINE(I:I).EQ.' '.OR.LINE(I:I).EQ.',') THEN
                  IEND = I
C          
C--CONVERT LINE TO A INTEGER NUMBER
C                 READ (LINE(ISTART:IEND), '(I5)', ERR=100) INTNO(N)    !TR: 2014 02 19 REMOVED FORMATTED INPUT READ
                  READ (LINE(ISTART:IEND),*, ERR=100) INTNO(N)          !TR: 2014 02 19 REMOVED FORMATTED INPUT READ
                  WRITE (IOUT, *) NAME(N), INTNO(N)                
                  N = N+1              
                  EXIT
                ENDIF
              ENDDO
              IF(I.EQ.81)GOTO 50
              II = IEND
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C      
50    CONTINUE      
C
      RETURN
C
C--IF AN ERROR OCCURED
 100  WRITE (IOUT, *) 'READ ERROR UNIT', IN, N
      WRITE (IOUT, *) 'PARAMETER ', NAME
C
      END SUBROUTINE EINTRD_MULTI
C
C
C
C
C 
      DOUBLE PRECISION FUNCTION DYNVISC(TC)
C     *******************************************************************      
C     CALCULATION OF DYNAMIC VISCOSITY
C     DYNAMIC VISCOSITY [PA S]OF WATER ACCORDING TO HANDBOOK OF CHEMISTRY
C     AND PHYSICS (1979)
C     BARC**CONSIDER MAKING THIS WORK WITH DEGREES F ALSO
C     *******************************************************************
C     JAN1999
C
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: TC
C
C     TC         TEMPERATURE IN DEGREE CELSIUS     
C     DYNVISC    DYNAMIC VISCOSITY IN PA S  
C     ------------------------------------------------------------------      
      IF ( TC.GE.0.0 .AND. TC.LT.20.0 ) THEN
        DYNVISC = 10.0D0**                                              
     +            (1301.0D0/(998.33D0+8.1855D0*(TC-20.0D0)+0.00585D0*   
     +            (TC-20.0D0)**2)-1.30233D0)/1000.0D0
      ELSEIF ( TC.GE.20.0 .AND. TC.LE.100.0 ) THEN
        DYNVISC = 10.0D0**                                              
     +            ((1.3272D0*(20.0D0-TC)-0.001053D0*(TC-20.0D0)**2) 
     +            /(TC+105.0D0))*1.002D0/1000.0D0
      ELSE
        WRITE (*, *) 'BAD TEMPERATURE INPUT!'
        STOP
      ENDIF
C
      END FUNCTION DYNVISC
C
C
C
C
C 
C
      DOUBLE PRECISION FUNCTION KINVISC(TC, VISC)
C     *******************************************************************      
C     CALCULATION OF WATER DENSITY AND KINEMATIC VISCOSITY
C     DENSITY [KG/M**3] OF WATER ACCORDING TO HANDBOOK OF CHEMISTRY AND
C     PHYSICS (1979); KINEMATIC VISCOSITY [M**2/S]
C     *******************************************************************
C     JAN1999
C  
      IMPLICIT NONE
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: TC, VISC
C
C--LOCAL VARIABLES
      DOUBLE PRECISION WADENS
C      
C     KINVISC KINEMATIC VISCOSITY IN M**2/S
C     TC      TEMPERATURE IN DEGREE CELSIUS     
C     VISC    DYNAMIC VISCOSITY IN PA S
C     WADENS  DENSITY OF WATER IN KG/M**3   
C     ------------------------------------------------------------------         
      WADENS = (999.83952D0+16.945176D0*TC-7.9870401D-3*TC**2-      
     +         46.170461D-6*TC**3+105.56302D-9*TC**4-           
     +         280.54253D-12*TC**5)/(1.0D0+16.879850D-3*TC)
C 
      KINVISC = VISC/WADENS
C
      END FUNCTION KINVISC
C
C
C
C
C
      SUBROUTINE CALC_DENS_VISC
C     ******************************************************************
C     BARC**CALCULATES WATER DENSITY AND VISCOSITY FOR CFPM2; DENSITY
C     [KG/M**3] OF WATER ACCORDING TO HANDBOOK OF CHEMISTRY AND PHYSICS
C     (1979); DYNAMIC VISCOSITY [PA SEC]OF WATER ACCORDING TO
C      HANDBOOK OF CHEMISTRY AND PHYSICS (1979)
C     ******************************************************************
C
      USE CONSTANTS, ONLY: TRUE,FALSE,Z,ONE,NEG,NINER,DZ,DOS,NEARZERO_30
      USE CFPMODULE, ONLY:TWATER, DENSWA, VISCWA,  CM2INCH
      USE GLOBAL, ONLY:IOUT, ITMUNI, LENUNI
      IMPLICIT NONE
C     ------------------------------------------------------------------
      DENSWA = (999.83952D0+16.945176D0*TWATER-                         
     +         7.9870401D-3*TWATER**2-46.170461D-6*TWATER**3+     
     +         105.56302D-9*TWATER**4-280.54253D-12*TWATER**5)  
     +         /(1.0D0+16.879850D-3*TWATER)
C 
C--CALCULATE DYNAMIC VISCOSITY
      IF ( TWATER.GE.0.0 .AND. TWATER.LT.20.0 ) THEN
        VISCWA = 10.0D0**                                               
     +           (1301.0D0/(998.33D0+8.1855D0*(TWATER-20.0D0)+0.00585D0*
     +           (TWATER-20.0D0)**2)-1.30233D0)/1000.0D0
      ELSEIF ( TWATER.GE.20.0 .AND. TWATER.LE.100.0 ) THEN
        VISCWA = 10.0D0**                                               
     +           ((1.3272D0*(20.0D0-TWATER)-0.001053D0*(TWATER-20.0D0)  
     +           **2)/(TWATER+105.0D0))*1.002D0/1000.0D0
      ELSE
        WRITE (*, *) 'BAD TEMPERATURE INPUT!'
        STOP
      ENDIF
C
      WRITE (IOUT, *) ' DYN VISC OF WATER IS [PA SEC]=', VISCWA    
C 
C--KINEMATIC VISCOSITY NU [M**2/S]
      VISCWA = VISCWA/DENSWA
C 
C--BARC**CORRECT DENSITY UNITS
C--BARC**UNITS ARE FT
      IF ( LENUNI.EQ.1 ) DENSWA = DENSWA*(1.0D0/(CM2INCH**3))
C--BARC**UNITS ARE CM
      IF ( LENUNI.EQ.3 ) DENSWA = DENSWA*(1.0D0/(100.0D0**3))
C 
C--BARC**CORRECT VISCOSITY UNITS
C--BARC**UNITS ARE M2/S
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.2 ) VISCWA = VISCWA*1.0D0
C--BARC**UNITS ARE M2 / MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.2 ) VISCWA = VISCWA*60.0D0
C--BARC**UNITS ARE M2/HOUR
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.2 ) VISCWA = VISCWA*60.0D0**2
C--BARC**UNITS ARE M2/DAY
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.2 )                              
     +     VISCWA = VISCWA*60.0D0**2*24.0D0
C--BARC**UNITS ARE M2/YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.2 )                              
     +     VISCWA = VISCWA*60.0D0**2*24.0D0*365.0D0
C 
C--BARC**UNITS ARE FT2 /SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.1 ) VISCWA = VISCWA*CM2INCH**2
C--BARC**UNITS ARE FT2/MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.1 )                              
     +     VISCWA = VISCWA*CM2INCH**2*60.0D0
C--BARC**UNITS ARE FT2/HR
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.1 )                              
     +     VISCWA = VISCWA*CM2INCH**2*60.0D0**2
C--BARC**UNITS ARE FT2/DAY
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.1 )                              
     +     VISCWA = VISCWA*CM2INCH**2*60.0D0**2*24.0D0
C--BARC**UNITS ARE FT2/YR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.1 )                              
     +     VISCWA = VISCWA*CM2INCH**2*60.0D0**2*24.0D0*365.0D0
C 
C--BARC**UNITS ARE CM2/SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.3 ) VISCWA = VISCWA*100.D0**2
C--BARC**UNITS ARE CM2/MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.3 )                              
     +     VISCWA = VISCWA*100.0D0**2*60.0D0
C--BARC**UNITS ARE CM2/HR
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.3 )                              
     +     VISCWA = VISCWA*100.0D0**2*60.0D0**2
C--BARC**UNITS ARE CM2/DAY
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.3 )                              
     +     VISCWA = VISCWA*100.0D0**2*60.0D0**2*24.0D0
C--BARC**UNITS ARE CM2/YR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.3 )                              
     +     VISCWA = VISCWA*100.0D0**2*60.0D0**2*24.0D0*365.0D0 
CB    WRITE (IOUT, *) ' KINEMATIC VISCOSITY [L^2/T]=', VISCWA
      WRITE (IOUT, *) ' DENSITY OF WATER IS [KG/L^3]=', DENSWA
CB      WRITE (IOUT, *) ' DYN VISC OF WATER IS [KG/LT]=', VISCWA
      WRITE (IOUT, *) ' KIN VISC OF WATER IS [L^2/T]=', VISCWA
C 
      END SUBROUTINE CALC_DENS_VISC
C
C
C
C
C
!     SUBROUTINE CALC_DENS_VISC2
C     ******************************************************************
C     BARC**CALCULATES WATER DENSITY AND VISCOSITY FOR CFPM2 TURB 
C     PARAMETER ARRAYS; ROH--DENSITY [KG/M**3] OF WATER ACCORDING TO
C     HANDBOOK OF CHEMISTRY AND PHYSICS (1979); DYNAMIC VISCOSITY [PA SEC]
C     OF WATER ACCORDING TO HANDBOOK OF CHEMISTRY AND PHYSICS (1979)
C     !TR: 2023 07 02 THIS SUBROUTINE IS FOR CFP MODE2 WITH TEMPERATURE
C     SPATIALLY VARIABLE, CURRENTLY, THIS FEATURE IS DEACTIVATED AND THE
C     ROUTINE IS NOT CALLED
C     ******************************************************************
C
!     USE CONSTANTS, ONLY: TRUE,FALSE,Z,ONE,NEG,NINER,DZ,DOS,NEARZERO_30
!     USE CFPMODULE, ONLY:TWATER2, DENSWA2, VISCWA2, CM2INCH, NCL
!     USE GLOBAL, ONLY:IOUT, ITMUNI, LENUNI, NCOL, NROW
!     IMPLICIT NONE
!     INTEGER K,I,J
C     ------------------------------------------------------------------
C
!     DO K=1,NCL
!       DO I=1,NROW
!         DO J=1,NCOL
      
!           DENSWA2(J,I,K) = (999.83952D0+16.945176D0*TWATER2(J,I,K)    
!    +         -7.9870401D-3*TWATER2(J,I,K)**2-46.170461D
!    +         -6*TWATER2(J,I,K)**3+105.56302D
!    +         -9*TWATER2(J,I,K)**4-280.54253D
!    +         -12*TWATER2(J,I,K)**5)
!    +         /(1.0D0+16.879850D-3*TWATER2(J,I,K))
C 
C--CALCULATE DYNAMIC VISCOSITY
!           IF (TWATER2(J,I,K).GE.0.0.AND.TWATER2(J,I,K).LT.20.0) THEN
!             VISCWA2(J,I,K) = 10.0D0**(1301.0D0/(998.33D0+
!    +            8.1855D0*(TWATER2(J,I,K)-20.0D0)+
!    +            0.00585D0*(TWATER2(J,I,K)-20.0D0)**2)-
!    +            1.30233D0)/1000.0D0
!           ELSEIF (TWATER2(J,I,K).GE.20.0.AND.TWATER2(J,I,K).LE.100.0) 
!    +      THEN
!             VISCWA2(J,I,K) = 10.0D0**((1.3272D0*(20.0D0               
!    +            -TWATER2(J,I,K))-0.001053D0*(TWATER2(J,I,K)-20.0D0)
!    +            **2)/(TWATER2(J,I,K)+105.0D0))*1.002D0/1000.0D0
!           ELSE
!             WRITE (*, *) 'BAD TEMPERATURE INPUT!'
!             STOP
!           ENDIF
CB       WRITE (IOUT,'(3I5,F6.4)') ' DYN VISC OF WATER IS [PA SEC]=', 
CB     +   J,I,K,VISCWA2(J,I,K)    
C
C--KINEMATIC VISCOSITY NU[M**2/S]
!           VISCWA2(J,I,K) = VISCWA2(J,I,K)/DENSWA2(J,I,K)
C      
CBARC**ENDDOS
!         ENDDO
!       ENDDO
!     ENDDO
C
C--BARC**CORRECT DENSITY UNITS
!     DO K=1,NCL
!       DO I=1,NROW
!         DO J=1,NCOL
          
C--BARC**UNITS ARE FT
!           IF(LENUNI.EQ.1)DENSWA2(J,I,K) = DENSWA2(J,I,K)*(1.0D0/
!    +                                          (CM2INCH**3))
C
C--BARC**UNITS ARE CM
!           IF(LENUNI.EQ.3)DENSWA2(J,I,K) = DENSWA2(J,I,K)*(1.0D0/
!    +                   (100.0D0**3))
C 
C--BARC**CORRECT VISCOSITY UNITS
C--BARC**UNITS ARE M2/S
!           IF(ITMUNI.EQ.1.AND.LENUNI.EQ.2) VISCWA2(J,I,K) = 
!    +         VISCWA2(J,I,K)*1.0D0
C
C--BARC**UNITS ARE M2 / MIN
!           IF(ITMUNI.EQ.2 .AND. LENUNI.EQ.2) VISCWA2(J,I,K) = 
!    +         VISCWA2(J,I,K)*60.0D0
C
C--BARC**UNITS ARE M2/HOUR
!           IF(ITMUNI.EQ.3 .AND. LENUNI.EQ.2) VISCWA2(J,I,K) = 
!    +         VISCWA2(J,I,K)*60.0D0**2
C
C--BARC**UNITS ARE M2/DAY
!           IF (ITMUNI.EQ.4 .AND. LENUNI.EQ.2)                 
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*60.0D0**2*24.0D0
C
C--BARC**UNITS ARE M2/YEAR
!           IF (ITMUNI.EQ.5 .AND. LENUNI.EQ.2)        
!    +          VISCWA2(J,I,K)=VISCWA2(J,I,K)*60.0D0**2*24.0D0*365.0D0
C 
C--BARC**UNITS ARE FT2 /SEC
!           IF (ITMUNI.EQ.1 .AND. LENUNI.EQ.1) VISCWA2(J,I,K) = 
!    +          VISCWA2(J,I,K)*CM2INCH**2
C
C--BARC**UNITS ARE FT2/MIN
!           IF (ITMUNI.EQ.2 .AND. LENUNI.EQ.1)              
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*CM2INCH**2*60.0D0
C
C--BARC**UNITS ARE FT2/HR
!           IF (ITMUNI.EQ.3 .AND. LENUNI.EQ.1)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*CM2INCH**2*60.0D0**2
C
C--BARC**UNITS ARE FT2/DAY
!           IF (ITMUNI.EQ.4 .AND. LENUNI.EQ.1)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*CM2INCH**2*60.0D0
!    +         **2*24.0D0
C
C--BARC**UNITS ARE FT2/YR
!           IF (ITMUNI.EQ.5 .AND. LENUNI.EQ.1)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*CM2INCH**2*60.0D0
!    +         **2*24.0D0*365.0D0
C 
C--BARC**UNITS ARE CM2/SEC
!           IF (ITMUNI.EQ.1 .AND. LENUNI.EQ.3) VISCWA2(J,I,K) = 
!    +          VISCWA2(J,I,K)*100.D0**2
!
C--BARC**UNITS ARE CM2/MIN
!           IF (ITMUNI.EQ.2 .AND. LENUNI.EQ.3)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*100.0D0**2*60.0D0
C
C--BARC**UNITS ARE CM2/HR
!           IF (ITMUNI.EQ.3 .AND. LENUNI.EQ.3)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*100.0D0**2*60.0D0**2
C
C--BARC**UNITS ARE CM2/DAY
!           IF (ITMUNI.EQ.4 .AND. LENUNI.EQ.3)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*100.0D0**2*60.0D0
!    +          **2*24.0D0
C
C--BARC**UNITS ARE CM2/YR
!           IF (ITMUNI.EQ.5 .AND. LENUNI.EQ.3)
!    +          VISCWA2(J,I,K) = VISCWA2(J,I,K)*100.0D0**2*60.0D0
!    +          **2*24.0D0*365.0D0
CB    WRITE (IOUT, *) ' KINEMATIC VISCOSITY [L^2/T]=', VISCWA2(J,I,K)
!           WRITE (IOUT,'(A30,1X,A5,3I5,1X,F8.4)') ' DENSITY OF WATER IS
!    + [KG/L^3]=','J,I,K',J,I,K,DENSWA2(J,I,K)
CB      WRITE (IOUT, *) ' DYN VISC OF WATER IS [KG/LT]=', VISCWA2(J,I,K)
!           WRITE (IOUT,'(A30,1X,A5,3I5,1X,F8.4)') ' KIN VISC OF WATER I
!    +S [L^2/T]=','J,I,K',J,I,K,VISCWA2(J,I,K)
C
C--BARC**ENDDOS
!         ENDDO
!       ENDDO
!     ENDDO
C 
!     END SUBROUTINE CALC_DENS_VISC2
C
C
C
C
C 
      SUBROUTINE RESET_LH
C****************************************************************
C     RESET LH BOUNDARY (BEGHEAD) BEFORE ITERATION STARTS
C     by Thomas.Reimann@tu-dresden.de
C****************************************************************
      USE CFPMODULE, ONLY: ACTIVENODE, ILH, BEGHEAD, NBR
C
      IMPLICIT NONE
C
      INTEGER I,J
C      
      DO I=1, ACTIVENODE
        DO J=1, ACTIVENODE
C
C--IF NODE IS LH, SET BEGHEAD TO -1 AND SET NODE TO REGULAR FLOW        
          IF(ILH(J,1).EQ.NBR(I,1)) THEN
            BEGHEAD(I) = -1
            ILH(J,2) = 0
          ENDIF
        ENDDO
      ENDDO   
C
      END SUBROUTINE RESET_LH      
C
C
C
C
C
!      SUBROUTINE READTD (INUNIT,TDDATA,MAXLINES,NONODES)
!C****************************************************************
!C     READ TIME DEPENDENT DATA FROM GIVEN INPUT FILES
!C     BY THOMAS.REIMANN@TU-DRESDEN.DE
!C****************************************************************      
!C     INUNIT = ARRAY WITH INPUT UNIT NUMBERS FOR ALL NODES
!C     TDDATA = TIME DEPENDENT DATA ARRAY (NODE,TIME,VALUE) FOR EACH TD NODE
!C     MAXLINES = MAX NUMBER OF LINES IN INPUT DATA (FOR ALLOCATION PURPOSES)
!C     NONODES = NUMBER OF TD NODES
!C     NOLINES = NUMBER OF LINES IN INPUT DATA
!C
!      USE CFPMODULE, ONLY: ACTIVENODE      
!      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
!C      
!      IMPLICIT NONE
!C
!      INTEGER I,J,N, NOLINES
!      INTEGER, INTENT(IN) :: NONODES, INUNIT(ACTIVENODE), MAXLINES
!      DOUBLE PRECISION, INTENT(OUT)::TDDATA(MAXLINES,3,NONODES)
!      DOUBLE PRECISION DUMMY1, DUMMY2
!C
!C--INITIALIZE COUNTER
!      N=0
!C      
!      DO I=1, ACTIVENODE
!C
!C--IF EXTERNAL TD INPUT IS AVAILABLE      
!        IF (INUNIT(I).GT.0) THEN
!          N=N+1
!          CALL UTF8_BOM_OFFSET_REWIND(INUNIT(I))
!          READ (INUNIT(I),*) NOLINES
!          DO J=1, NOLINES
!            READ (INUNIT(I),*) DUMMY1, DUMMY2
!            TDDATA (J,1,N) = I
!            TDDATA (J,2,N) = DUMMY1
!            TDDATA (J,3,N) = DUMMY2
!          ENDDO
!        ENDIF
!      ENDDO
!C      
!      END SUBROUTINE READTD

      SUBROUTINE READTD (INUNIT,TDDATA,MAXLINES,NONODES,LINE,IOUT)
C****************************************************************
C     READ TIME DEPENDENT DATA FROM GIVEN INPUT FILES
C     BY THOMAS.REIMANN@TU-DRESDEN.DE
C****************************************************************      
C     INUNIT = ARRAY WITH INPUT UNIT NUMBERS FOR ALL NODES
C     TDDATA = TIME DEPENDENT DATA ARRAY (NODE,TIME,VALUE) FOR EACH TD NODE
C     MAXLINES = MAX NUMBER OF LINES IN INPUT DATA (FOR ALLOCATION PURPOSES)
C     NONODES = NUMBER OF TD NODES
C     NOLINES = NUMBER OF LINES IN INPUT DATA
C
      USE CFPMODULE, ONLY: ACTIVENODE      
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      USE CONSTANTS, ONLY: ONE, NL
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER, GET_NUMBER
      !USE NUM2STR_INTERFACE, ONLY: NUM2STRC      
      IMPLICIT NONE
C
      INTEGER I,J,N, NOLINES
      integer,      intent(in) :: NONODES, INUNIT(ACTIVENODE), MAXLINES
      double precision, intent(out)::TDDATA(MAXLINES,3,NONODES)
      character(*), intent(inout) :: LINE
      integer,      intent(in   ) :: IOUT
      DOUBLE PRECISION DUMMY1, DUMMY2
      INTEGER  LLOC,ISTART,ISTOP,IU
      CHARACTER(128):: ERR
C
C--INITIALIZE COUNTER
      N=0
      ERR = 'If line is an empty, "", reader may have prematurely '//
     +      'reached the end of the file (read count on first line '//
     +      'is too big).'
C      
      DO I=1, ACTIVENODE
C
C--IF EXTERNAL TD INPUT IS AVAILABLE      
        IF (INUNIT(I).GT.0) THEN
          N=N+1
          IU = INUNIT(I)
          CALL UTF8_BOM_OFFSET_REWIND(IU)
          CALL READ_TO_DATA(LINE, IU, IOUT)
          LLOC = ONE
          CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IU,NOLINES)  
          
          DO J=1, NOLINES
            CALL READ_TO_DATA(LINE, IU, IOUT)
            LLOC = ONE
            CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IU,DUMMY1,
     +                      MSG=ERR)  
            CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IU,DUMMY2,
     +                      MSG=ERR)  
            !
            TDDATA (J,1,N) = I
            TDDATA (J,2,N) = DUMMY1
            TDDATA (J,3,N) = DUMMY2
          ENDDO
        ENDIF
      ENDDO
C      
      END SUBROUTINE READTD
C
C
C
C
C
      SUBROUTINE GETTDDATA(TIME,OUT,TDDATA,MAXLINES,NONODES,INUNIT,
     + INODE,FLG,BLK_FLG)
C****************************************************************
C     ASSIGN TIME DEPENDENT DATA TO BOUNDARIES; INTERPOLATE FROM
C     GIVEN DATA
C     BY THOMAS.REIMANN@TU-DRESDEN.DE
C****************************************************************   
      USE CONSTANTS, ONLY: TRUE,FALSE,Z,ONE,NEG,NINER,DZ,DOS,NEARZERO_30
      USE CFPMODULE, ONLY: ACTIVENODE
      USE GLOBAL, ONLY: IOUT
C      
      IMPLICIT NONE
C
      INTEGER I,II,J,N,FLG,I_NODE
      REAL TIME
      LOGICAL FOUND, BLK_FLG
      INTEGER, INTENT(IN) :: NONODES, MAXLINES, INUNIT(ACTIVENODE),
     +  INODE(ACTIVENODE,2)
      DOUBLE PRECISION, INTENT(IN):: TDDATA(MAXLINES,3,NONODES)
      DOUBLE PRECISION, INTENT(INOUT):: OUT(ACTIVENODE)
      DOUBLE PRECISION TP,TPP,XP,XPP
      INTEGER :: TIME_INDEX
      DOUBLE PRECISION :: DT, KV, ZERO_TOL
C      
C--INITIALIZE COUNTER
      N = Z
      KV = 273.15d0
      ZERO_TOL = 1.0d-29
C      
      DO I = 1, ACTIVENODE
C
C--INITIALIZE FOUND FLAG
        FOUND = FALSE      
C
C--FOR BC LISTED DATA (I.E. BOUNDARY DATA ARE LISTED AND THE NODE NUMBER IS PART OF BC DATA)
        IF (FLG.EQ.2) THEN
          DO II = 1, ACTIVENODE
            IF (INUNIT(I).GT.Z.AND.INODE(II,1).EQ.I) THEN
              I_NODE = II
              FOUND = TRUE
              N=N+1
            ENDIF
          ENDDO
C
C--FOR NODE LISTED DATA / FLG = 3 FOR TEMPERATURE CORRECTION C - KELVIN
        ELSEIF ((FLG.EQ.1).OR.(FLG.EQ.3)) THEN
C
C--IF NODE REQUIRE TIME DEPENDENT DATA      
          IF (INUNIT(I).GT.Z) THEN
            FOUND = TRUE
            N=N+1
          ENDIF
        ELSE
          WRITE (IOUT,*)'SOME ERROR IN RECEIVING TIME DEPENDENT ',
     +                  'BOUNDARY DATA - EXECUTION STOPPED'
          STOP
        ENDIF
C
C--IF NODE BOUNDARY DATA ARE TIME DEPENDENT
        IF (FOUND.OR.BLK_FLG) THEN
           TIME_INDEX = MAXLINES
           DO J = 1, MAXLINES             ! SEARCH FOR TIME > MODFLOW TIME    
             IF (TDDATA(J,2,N).GE.TIME) THEN
                 TIME_INDEX = J
                 EXIT
             ENDIF
           ENDDO
           !
           J = TIME_INDEX
           !
           IF ( MAXLINES == 1 ) THEN
               IF(FLG.EQ.1) OUT(I)      = TDDATA(1,3,N)
               IF(FLG.EQ.3) OUT(I)      = TDDATA(1,3,N) + KV
               IF(FLG.EQ.2) OUT(I_NODE) = TDDATA(1,3,N)
           ELSE IF ( J == 1 ) THEN
               TP   = TDDATA (J  ,2,N)
               TPP  = TDDATA (J+1,2,N)
               XP   = TDDATA (J  ,3,N)
               XPP  = TDDATA (J+1,3,N)
               DT = TPP-TP
               IF ( DT < ZERO_TOL ) THEN
                  IF(FLG.EQ.1) OUT(I)      = TDDATA(J,3,N)
                  IF(FLG.EQ.3) OUT(I)      = TDDATA(J,3,N) + KV
                  IF(FLG.EQ.2) OUT(I_NODE) = TDDATA(J,3,N)
               ELSE
                  IF(FLG.EQ.1) OUT(I) = XP + (XPP-XP)*(TIME-TP)/DT
                  IF(FLG.EQ.3) OUT(I) = XP + (XPP-XP)*(TIME-TP)/DT + KV
                  IF(FLG.EQ.2) OUT(I_NODE) = XP + (XPP-XP)*(TIME-TP)/DT
               END IF
           ELSE
               TP   = TDDATA (J-1,2,N)
               TPP  = TDDATA (J  ,2,N)
               XP   = TDDATA (J-1,3,N)
               XPP  = TDDATA (J  ,3,N)
               DT = TPP-TP
               IF ( DT < ZERO_TOL ) THEN
                  IF(FLG.EQ.1) OUT(I)      = TDDATA(J,3,N)
                  IF(FLG.EQ.3) OUT(I)      = TDDATA(J,3,N) + KV
                  IF(FLG.EQ.2) OUT(I_NODE) = TDDATA(J,3,N)
               ELSE
                  IF(FLG.EQ.1) OUT(I) = XP + (XPP-XP)*(TIME-TP)/DT
                  IF(FLG.EQ.3) OUT(I) = XP + (XPP-XP)*(TIME-TP)/DT + KV
                  IF(FLG.EQ.2) OUT(I_NODE) = XP + (XPP-XP)*(TIME-TP)/DT
               END IF
           END IF
        END IF
      ENDDO 
C      
      END SUBROUTINE GETTDDATA
C
C
C
C
C
      SUBROUTINE INITDDATA(TDDATA,LINES,NONODES)
C****************************************************************
C     INITIALIZE TIME DEPENDENT DATA ARAY
C     BY THOMAS.REIMANN@TU-DRESDEN.DE
C****************************************************************       
      IMPLICIT NONE
C
      INTEGER J, JJ, JJJ, LINES, NONODES
      DOUBLE PRECISION TDDATA(LINES,3,NONODES)      
C
C--INITIALIZE            
      DO J = 1, LINES
        DO JJ = 1, 3                                              
          DO JJJ = 1, NONODES                                
            TDDATA(J, JJ, JJJ) = 0.0                               
          ENDDO                                                   
        ENDDO                                                     
      ENDDO                                                       
C                  
      END SUBROUTINE INITDDATA
C
C
C
C
C            
!      SUBROUTINE ASSIGNTDDATA(I,READDATA,TD_IN,TD_LINESMAX)
!C****************************************************************
!C     ASSIGN READ IN DATA AS UNIT NUMBER FOR TIME DEPENDENT READ 
!C     IN; BY THOMAS.REIMANN@TU-DRESDEN.DE
!C**************************************************************** 
!      USE CFPMODULE, ONLY: ACTIVENODE      
!      USE 
!C      
!      IMPLICIT NONE
!C
!      INTEGER I, TD_LINES, TD_LINESMAX, TD_IN(ACTIVENODE)
!      DOUBLE PRECISION READDATA       
!C
!C--SET READ IN DATA AS INPUT UNIT NUMBER      
!      TD_IN(I) = INT(READDATA)
!C
!C--ASSESS NUMBER OF LINES IN INPUT FILE      
!      READ (TD_IN(I),*) TD_LINES
!C
!C--UPDATE MAXIMUM NUMBER OF LINES - IF NECESSARY      
!      IF (TD_LINES.GT.TD_LINESMAX)TD_LINESMAX = TD_LINES
!C
!      END SUBROUTINE ASSIGNTDDATA
C            
      SUBROUTINE ASSIGNTDDATA(I,READDATA,TD_IN,TD_LINESMAX,LINE,IOUT)
C****************************************************************
C     ASSIGN READ IN DATA AS UNIT NUMBER FOR TIME DEPENDENT READ 
C     IN; BY THOMAS.REIMANN@TU-DRESDEN.DE
C**************************************************************** 
      USE CFPMODULE, ONLY: ACTIVENODE      
      USE CONSTANTS, ONLY: ONE, NL
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
C      
      IMPLICIT NONE
C
      integer,          intent(in   ) :: I, IOUT  
      double precision, intent(in   ) :: READDATA       
      integer,          intent(inout) :: TD_IN(ACTIVENODE)  
      integer,          intent(inout) :: TD_LINESMAX  
      character(*),     intent(inout) :: LINE
C  
      integer :: TD_LINES, IERR, LLOC, ISTART, ISTOP
      logical :: ISOPEN
      
C--SET READ IN DATA AS INPUT UNIT NUMBER      
      TD_IN(I) = INT(READDATA)
      
      INQUIRE(TD_IN(I), OPENED=ISOPEN)
      IF (.not. ISOPEN) then
         CALL USTOP('CPF input attempted to read from file unit '//
     +        NUM2STR(TD_IN(I))//NL//
     +        'But the unit is not associated with an open file.'//NL//
     +        'The unit number was read from the following line:'//NL//
     +        '"'//TRIM(LINE)//'"')
      END IF
      !
      CALL READ_TO_DATA(LINE, TD_IN(I))
      LLOC = ONE
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,TD_IN(I),TD_LINES)  
C
C--UPDATE MAXIMUM NUMBER OF LINES - IF NECESSARY      
      IF (TD_LINES.GT.TD_LINESMAX)TD_LINESMAX = TD_LINES
C
      END SUBROUTINE ASSIGNTDDATA
C
C
C
C
C    
      SUBROUTINE CADSML_VOL(I,HEAD,VOL)
C****************************************************************
C     COMPUTE CADSML VOLUME; BY THOMAS.REIMANN@TU-DRESDEN.DE
C     LAST CHANGE 2013 07 26 
C**************************************************************** 
C
C     I     = NODE NUMBER
C     HEAD  = CONDUIT HEADS
C     VOL   = CADSML VOLUME
C
      USE CFPMODULE, ONLY:CADSMLDAT,MXNODE,NODEBOT,
     + L_NODE      
C      
      IMPLICIT NONE
C
      INTEGER N,NN,I
      DOUBLE PRECISION VOL,UPPERLIMIT,HEAD(MXNODE)        
C
C--VOLUME - PREVIOUS ITERATION           
      VOL = 0.0   
      NN = 0
C
C--IF HEAD ABOVE CADS        
      IF(HEAD(I).GT.CADSMLDAT(I,1,1)) THEN
        UPPERLIMIT = CADSMLDAT(I,1,1)
        NN = 1        
        GOTO 100
      ENDIF
C
C--IF HEAD INSIDE CADSML        
      DO N = 1, 4
        IF (HEAD(I).LE.CADSMLDAT(I,1,N).AND.
     +      HEAD(I).GT.CADSMLDAT(I,1,N+1)) THEN
          UPPERLIMIT = HEAD(I)
          NN = N
        ENDIF
      ENDDO   
C
C--IF HEAD ABOVE OR INSIDE CADS SUM UP     
      IF (NN.EQ.0) GOTO 200
100   DO N = NN, 4
        IF (UPPERLIMIT.LE.NODEBOT(I)) THEN
          VOL = VOL
          UPPERLIMIT = CADSMLDAT(I,1,N+1)
        ELSE
          VOL = VOL+(UPPERLIMIT-CADSMLDAT(I,1,N+1))*CADSMLDAT(I,2,N)
     +           *L_NODE(I)
          UPPERLIMIT = CADSMLDAT(I,1,N+1)
        ENDIF
      ENDDO        
C        
200   CONTINUE
C
      END SUBROUTINE CADSML_VOL
C
C
C
C
C      
      SUBROUTINE COUNT_LINE_NUMBERS(IN, IOUT, NUM)
C
C***********************************************************************
C     READ LINE AND COUNT NUMBER OF DATA VALUES
C     BY THOMAS.REIMANN@TU-DRESDEN.DE
C***********************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C IN       UNIT NUMBER FROM WHICH INPUT FOR THIS PACKAGE WILL BE READ
C ISTART   LOCATION FOR THE START OF THE NUMBER
C IEND     LOCATION FOR THE END OF THE NUMBER
C LINE     LINE FROM INPUT FILE
C NUM      NUMBER OF PARAMETERS IN LINE
C
      IMPLICIT NONE
C
! ARGUMENTS
      INTEGER, INTENT(IN) :: IN, IOUT
      INTEGER, INTENT(OUT) :: NUM
! LOCAL VARIABLES
      CHARACTER(LEN=80) LINE
      INTEGER I, II, ISTART, IEND
C
C--INI
      NUM = 0      
      DO
C
C--READ A LINE; IGNORE BLANK LINES AND COMMENT LINES
        READ (IN, '(A)', ERR=100) LINE
        IF ( LINE.NE.' ' ) THEN
          IF ( LINE(1:1).NE.'#' ) THEN
C
C--FIND START AND END OF NUMBER: CHARACTER THAT IS NOT A BLANK OR A COMMA
            ISTART = 0
            IEND = 80
            II = 1
            DO
C
C--SEARCH START
              DO I = II, 80
                IF(LINE(I:I).NE.' '.AND.LINE(I:I).NE.','
     +             .AND.LINE(I:I).NE.ACHAR(9))THEN                      !TR: 2014 03 10 ACHAR(9) TO CONSIDER TABS
                  ISTART = I
                  EXIT
                ENDIF
              ENDDO
              IF(I.EQ.81)GOTO 50
C
C--SEARCH END            
              II = ISTART
              DO I = II, 80
                IF(LINE(I:I).EQ.' '.OR.LINE(I:I).EQ.','
     +             .OR.LINE(I:I).EQ.ACHAR(9)) THEN                      !TR: 2014 03 10 ACHAR(9) TO CONSIDER TABS
                  IEND = I
C          
C--INCREASE NUMBER OF PARAMETERS         
                  NUM = NUM+1              
                  EXIT
                ENDIF
              ENDDO
              IF(I.EQ.81)GOTO 50
              II = IEND
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C      
50    CONTINUE      
C
      RETURN
C
C--IF AN ERROR OCCURED
 100  WRITE (IOUT, *) 'READ ERROR UNIT', IN, 'POSITION', NUM
C
      END SUBROUTINE COUNT_LINE_NUMBERS
C
C
C
C
C 