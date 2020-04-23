C***********************************************************************
C  CONDUIT FLOW PROCESS SUBROUTINES - OUTPUT CONTROL
C  
C  A LIST WITH VARIABLE / PARAMETER NAMES IS PROVIDED AT THE END OF THE
C  THE FILE
C
C***********************************************************************
C
C
C
C
C
      SUBROUTINE GWF2CFPM2_OC(KKPER)
C
      USE CONSTANTS, ONLY: Z
      USE CFPMODULE, ONLY:MODE, TURB_FR, TURB_FF, TURB_FV, TL_OUT, 
     +                    NCL, CL
      USE GLOBAL, ONLY:NCOL, NROW, NLAY
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KKPER
C
C--LOCAL VARIABLES
      INTEGER I, IC, J
C
      IF ( MODE.EQ.1 ) RETURN
C 
C--BARC**ENDIF FOR WORK DONE ONLY AT END OF FIRST STRESS PERIOD.
      IF ( KKPER.EQ.1 ) OPEN (UNIT=999, FILE='TURBLAM.TXT',             
     +                        STATUS='UNKNOWN')
C 
C--BARC**INITIALIZE**
      TL_OUT = Z
C 
C--BARC**WRITE TURBULENCE CODES
      DO IC = 1, NCL
        DO I = 1, NROW
          DO J = 1, NCOL
C
C--BARC**ADD CV
C--BARC**ALL LAMINAR
            IF ( TURB_FR(J,I,CL(IC)).EQ.Z .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.Z.AND.TURB_FV(J,I,CL(IC)).EQ.Z ) 
     +               TL_OUT(J, I, CL(IC)) = Z
C     
C--BARC**FLOWR = TURB
            IF ( TURB_FR(J,I,CL(IC)).EQ.1 .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.Z.AND.TURB_FV(J,I,CL(IC)).EQ.Z ) 
     +               TL_OUT(J, I, CL(IC)) = 1
C
C--BARC**FLOWF = TURB
            IF ( TURB_FR(J,I,CL(IC)).EQ.Z .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.1.AND.TURB_FV(J,I,CL(IC)).EQ.Z ) 
     +               TL_OUT(J, I, CL(IC)) = 2
C
C--BARC**FLOWV = TURB
            IF ( TURB_FR(J,I,CL(IC)).EQ.Z .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.Z.AND.TURB_FV(J,I,CL(IC)).EQ.1 ) 
     +               TL_OUT(J, I, CL(IC)) = 3

CBARC**FLOWV,R = TURB     
            IF ( TURB_FR(J,I,CL(IC)).EQ.1 .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.Z.AND.TURB_FV(J,I,CL(IC)).EQ.1 ) 
     +               TL_OUT(J, I, CL(IC)) = 4
C
C--BARC**FLOWV,F = TURB     
            IF ( TURB_FR(J,I,CL(IC)).EQ.Z .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.1.AND.TURB_FV(J,I,CL(IC)).EQ.1 ) 
     +               TL_OUT(J, I, CL(IC)) = 5
C
C--BARC**FLOWV,R,F = TURB     
            IF ( TURB_FR(J,I,CL(IC)).EQ.1 .AND. TURB_FF(J,I,CL(IC))     
     +           .EQ.1.AND.TURB_FV(J,I,CL(IC)).EQ.1 ) 
     +               TL_OUT(J, I, CL(IC)) = 6
          ENDDO
        ENDDO
      ENDDO
C 
C--BARC**WRITE OUT ITURB CODES
      DO IC = 1, NCL
        WRITE (999, '(A13,I3,1X,A5,I3,1X,A14)') 'STRESS PERIOD', KKPER, 
     +         'LAYER', CL(IC), 'TURBULENT CODE'
        DO I = 1, NROW
          WRITE (999, '(5000I3)') (TL_OUT(J,I,CL(IC)), J=1, NCOL)
        ENDDO
      ENDDO
C
C--BARC2**
      END SUBROUTINE GWF2CFPM2_OC
C
C
C
C
C
      SUBROUTINE TS1OUT
C***********************************************************************
C     TIME SERIES OUTPUT      
C***********************************************************************
CB**HCON IS B_MAT
CB**TUBREY IS TUB_REYNOLD
CB**ADD TAUI
C  UPDATES BY THOMAS.REIMANN@TU-DRESDEN.DE
C  2012 11 29 ADDED MATRIX HEAD IN NODE OUTPUT // 2014 02 20 CADS
C  RECHARGE
C
      USE CONSTANTS, ONLY: Z
      USE CFPMODULE, ONLY:NNOD, QBDIR, QTUB, TSNODE, TSTUBE, B_MAT,     
     +    QMAT, QFIX, CON_DATA, TUB_REYNOLD, MXNODE, MXTUBE, NOTSNO,    
     +    NOTSTU, PFPSFLOW, NODEBOT, DTUBE, KTSNO, KTSTU, TAUI,         !TR: 2012 06 08 REPLACED NSTOR BY PFPSFLOW
     +    QFHLQ, CADSFLOW, QWELL, NBR, QCYLQ, CYFLOW, QLH, NCOUNT,      !TR: 2012 05 15 ADDED FHLQ, CADSFLOW, WELL; 2012 11 29 NBR FOR MATRIX HEAD OUTPUT / 2013 03 14 CAUCHY / 2013 03 25 LH // NCOUNT / TCOUNT GLOBAL
     +    TCOUNT, KTSTSAN, NOTSTSAN, TSAN_FLG, KTSTSAT, NOTSTSAT,       !TR: 2013 08 13 TSAN
     +    TSAT_FLG, QSDIR,IWELL, CWC_WELL                               !TR: 2013 08 13 TSAT // 2014 02 20 CADS RECHARGE // 2016 10 11 CWC
C
      USE GLOBAL, ONLY:IOUT, HNEW                                       !TR: 2012 11 29 ADDED HNEW FOR MATRIX HEAD OUTPUT
      USE GWFBASMODULE, ONLY:TOTIM
C
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES
C--BARC***HERE IS WHERE YOU CAN MAKE OUTPUT IF #NODES AND TUBES > 9999
      INTEGER FUNIT, T, N
      INTEGER I, IC, IL, IR, ITEST, J                                   !TR: 2012 11 29 ADDED HNEW MATRIX HEAD OUTPUT
      CHARACTER(LEN=16) FNAME
      CHARACTER(LEN=8) FNO
      DOUBLE PRECISION HWELL
C
C--TIME SERIES OUTPUT NODES
      IF ( NOTSNO.EQ.KTSNO ) THEN
        KTSNO = Z
        NCOUNT = Z
        DO N = 1, MXNODE
          HWELL = 0.0                                                   !TR: 2016 10 11 INI
          IC = NBR(N, 2)                                                !TR: 2012 11 29 ADDED HNEW MATRIX HEAD OUTPUT
          IR = NBR(N, 3)                                                !TR: 2012 11 29 ADDED HNEW MATRIX HEAD OUTPUT
          IL = NBR(N, 4)                                                !TR: 2012 11 29 ADDED HNEW MATRIX HEAD OUTPUT
          IF ( TSNODE(N).NE.Z ) THEN
            NCOUNT = NCOUNT + 1
            WRITE (FNO, FMT='(I8.8)') N
            FNAME = 'NODE'//FNO//'.out'
            FUNIT = 200 + NCOUNT                                        
            IF ( TSNODE(N).GT.Z ) THEN
              TSNODE(N) = -1
              OPEN (FUNIT, FILE=FNAME, STATUS='UNKNOWN', ERR=100)
              WRITE (IOUT, *) 'TIME SERIES OUTPUT IN FILE ', FNAME
              IF(IWELL(N).EQ.Z)THEN                                       !TR: 2016 10 11 
                WRITE (FUNIT, 9001)                                     !TR: 2016 10 11 
              ELSE                                                      !TR: 2016 10 11 
                WRITE (FUNIT, 9011)
              ENDIF                                                     !TR: 2016 10 11 
            ENDIF
C
C--FOR WELLS                                                            !TR: 2016 10 11 
            IF(IWELL(N).NE.0.0)THEN                                     !TR: 2016 10 11 
              IF(CWC_WELL(N).EQ.0.0) THEN                               !TR: 2016 10 11 
                HWELL = B_MAT(N)                                        !TR: 2016 10 11 
              ELSE                                                      !TR: 2016 10 11 
                HWELL = QWELL(N)/CWC_WELL(N)+B_MAT(N)                   !TR: 2016 10 11 
              ENDIF                                                     !TR: 2016 10 11 
C
C--BARC**DRY
              IF ( B_MAT(N).LE.NODEBOT(N) ) THEN
                WRITE (FUNIT, 9012) TOTIM, NODEBOT(N), HWELL,           !TR: 2012 11 29 HMAT
     +                              HNEW(IC, IR, IL),QBDIR(N), QWELL(N),!TR: 2012 06 08 WELL
     +                               -QMAT(N),-CADSFLOW(N)+QSDIR(N),    !TR: 2012 05 25 CADS / 2014 02 20 CADS RECHARGE // 2014 04 15 
     +                              (QTUB(N,T), T=1, NNOD), PFPSFLOW(N),!TR: 2012 06 08 REPLACED NSTOR BY PFPSFLOW
     +                              QFIX(N), QLH(N), QFHLQ(N),          !TR: 2013 03 25 LH
     +                              -CYFLOW(N),QCYLQ(N),                !TR: 2012 05 15 ADDED FHLQ / 2013 03 14 CAUCHY // 2014 04 15 
     +                              -CADSFLOW(N)+QSDIR(N),-CADSFLOW(N),
     +                              QSDIR(N)                            !TR: 2014 04 15 
              ELSE
                WRITE (FUNIT, 9012) TOTIM, B_MAT(N), HWELL,             !TR: 2012 11 29 HMAT
     +                              HNEW(IC, IR, IL),QBDIR(N), QWELL(N),!TR: 2012 06 08 WELL
     +                              -QMAT(N), -CADSFLOW(N)+QSDIR(N),    !TR: 2012 05 25 CADS / 2014 02 20 CADS RECHARGE // 2014 04 15 
     +                              (QTUB(N,T), T=1, NNOD), PFPSFLOW(N),!TR: 2012 06 08 REPLACED NSTOR BY PFPSFLOW
     +                              QFIX(N), QLH(N), QFHLQ(N),          !TR: 2013 03 25 LH
     +                              -CYFLOW(N),QCYLQ(N),                !TR: 2012 05 15 ADDED FHLQ, CAUCHY // 2014 04 15 
     +                              -CADSFLOW(N)+QSDIR(N),-CADSFLOW(N),
     +                              QSDIR(N)                            !TR: 2014 04 15     
              ENDIF
            ELSE
C
C--BARC**DRY
              IF ( B_MAT(N).LE.NODEBOT(N) ) THEN
                WRITE (FUNIT, 9002) TOTIM, NODEBOT(N), HNEW(IC, IR, IL),!TR: 2012 11 29 HMAT
     +                              QBDIR(N), QWELL(N),                 !TR: 2012 06 08 WELL
     +                               -QMAT(N),-CADSFLOW(N)+QSDIR(N),    !TR: 2012 05 25 CADS / 2014 02 20 CADS RECHARGE // 2014 04 15 
     +                              (QTUB(N,T), T=1, NNOD), PFPSFLOW(N),!TR: 2012 06 08 REPLACED NSTOR BY PFPSFLOW
     +                              QFIX(N), QLH(N), QFHLQ(N),          !TR: 2013 03 25 LH
     +                              -CYFLOW(N),QCYLQ(N),                !TR: 2012 05 15 ADDED FHLQ / 2013 03 14 CAUCHY // 2014 04 15 
     +                              -CADSFLOW(N)+QSDIR(N),-CADSFLOW(N),
     +                              QSDIR(N)                            !TR: 2014 04 15 
              ELSE
                WRITE (FUNIT, 9002) TOTIM, B_MAT(N), HNEW(IC, IR, IL),  !TR: 2012 11 29 HMAT
     +                              QBDIR(N), QWELL(N),                 !TR: 2012 06 08 WELL
     +                              -QMAT(N), -CADSFLOW(N)+QSDIR(N),    !TR: 2012 05 25 CADS / 2014 02 20 CADS RECHARGE // 2014 04 15 
     +                              (QTUB(N,T), T=1, NNOD), PFPSFLOW(N),!TR: 2012 06 08 REPLACED NSTOR BY PFPSFLOW
     +                              QFIX(N), QLH(N), QFHLQ(N),          !TR: 2013 03 25 LH
     +                              -CYFLOW(N),QCYLQ(N),                !TR: 2012 05 15 ADDED FHLQ, CAUCHY // 2014 04 15 
     +                              -CADSFLOW(N)+QSDIR(N),-CADSFLOW(N),
     +                              QSDIR(N)                            !TR: 2014 04 15      
              ENDIF
C
            ENDIF
C
          ENDIF
        ENDDO
      ENDIF
C
C--TIME SERIES OUTPUT TUBES      
      IF ( NOTSTU.EQ.KTSTU ) THEN
        KTSTU = Z
        TCOUNT = Z
        DO T = 1, MXTUBE
          IF ( TSTUBE(T).NE.Z ) THEN
            TCOUNT = TCOUNT + 1
            WRITE (FNO, FMT='(I8.8)') T
            FNAME = 'TUBE'//FNO//'.out'
            FUNIT = 200 + NCOUNT + TCOUNT
            IF ( TSTUBE(T).GT.Z ) THEN
              TSTUBE(T) = -1
              OPEN (FUNIT, FILE=FNAME, STATUS='UNKNOWN', ERR=100)
              WRITE (IOUT, *) 'TIME SERIES OUTPUT IN FILE ', FNAME
              WRITE (FUNIT, 9003)
            ENDIF
C
C--BARC**ADD TAUI
!            WRITE (FUNIT, 9004) TOTIM, DABS(DTUBE(T,1)), CON_DATA(T, 2),
!     +                          TUB_REYNOLD(T),TAUI(T)
            WRITE (FUNIT, 9004) TOTIM, DTUBE(T,1), CON_DATA(T, 2),      !TR: 2012 05 15 REMOVED DABS FOR DTUBE TO ALLOW INDICATING CHANGE OF FLOW DIRECTION
     +                          TUB_REYNOLD(T),TAUI(T)
          ENDIF
        ENDDO
      ENDIF
C
C--TIME SERIES OUTPUT ALONG NODES DATA
      IF(TSAN_FLG.AND.(NOTSTSAN.EQ.KTSTSAN))THEN
        KTSTSAN = Z
        CALL TSAN_OUT
      ENDIF    
C
C--TIME SERIES OUTPUT ALONG TUBES DATA
      IF(TSAT_FLG.AND.(NOTSTSAT.EQ.KTSTSAT))THEN
        KTSTSAT = Z
        CALL TSAT_OUT
      ENDIF          
      RETURN
C
 100  WRITE (IOUT, *) 'ERROR WHILE OPENING UNIT', FUNIT, ' (TS1OUT)!' 
 9001 FORMAT ('   TOTIM (T)          HCON', 6X, 'HMAT', 5X,'QDIR', 6X,  !TR: 2012 06 08 WELL / CADS / FHLQ; 2012 11 29 HMAT / 2013 03 14 CAUCHY // 2016 10 11 
     +        'QWELL', 8X,'QMAT', X,'QCADS<->CON',7X,'QTUB1',           !TR: 2014 02 20 CADS RECHARGE (QSDIR) // 2014 04 15 
!     +       'QTUB5', 9X, 'QTUB6', 9X, 'NSTOR', 10X, 'QFIX', 20(A14))
     +        7X,'QTUB2',7X,'QTUB3',7X,'QTUB4',7X,'QTUB5',7X,'QTUB6',7X,
     +        'NSTOR',8X,'QFIX',8X,'QLH',5X,'FHLQ (LQ)',6X,'CAUCHY',X,
     +        'CAUCHY (LQ)',X,'BREAK-DOWN CADS FLOW  QCADS<->CON',4X,   !TR: 2014 04 15 
     +        'Q_CDSSTO',4X,'Q_CDSRCH')
 9011 FORMAT ('   TOTIM (T)          HCON', 9X, 'HWELL',6X,'HMAT', 5X,  !TR: 2012 06 08 WELL / CADS / FHLQ; 2012 11 29 HMAT / 2013 03 14 CAUCHY // 2016 10 11 
     +        'QDIR', 6X,'QWELL', 8X,'QMAT', X,'QCADS<->CON',7X,'QTUB1',!TR: 2014 02 20 CADS RECHARGE (QSDIR) // 2014 04 15 
!     +       'QTUB5', 9X, 'QTUB6', 9X, 'NSTOR', 10X, 'QFIX', 20(A14))
     +        7X,'QTUB2',7X,'QTUB3',7X,'QTUB4',7X,'QTUB5',7X,'QTUB6',7X,
     +        'NSTOR',8X,'QFIX',8X,'QLH',5X,'FHLQ (LQ)',6X,'CAUCHY',X,
     +        'CAUCHY (LQ)',X,'BREAK-DOWN CADS FLOW  QCADS<->CON',4X,   !TR: 2014 04 15 
     +        'Q_CDSSTO',4X,'Q_CDSRCH')
! 9002 FORMAT (F14.0, F14.8, 20(G14.6))
 9002 FORMAT (F12.1, F14.8, F10.4, 4(G12.4),x,6(G11.4,x),6(G12.4),22x,   !TR: 2012 05 15 FHLQ / WELL; 2012 11 29 HMAT / 2013 03 14 CAUCHY / 2013 03 25 LH // 2014 02 20 CADS RECHARGE
     +        6(G12.4))
 9012 FORMAT (F12.1, F14.8, F14.8,F10.4, 4(G12.4),x,6(G11.4,x),6(G12.4),!TR: 2012 05 15 FHLQ / WELL; 2012 11 29 HMAT / 2013 03 14 CAUCHY / 2013 03 25 LH // 2014 02 20 CADS RECHARGE
     +        22x,6(G12.4))
C
C--BARC**REWRITE TO ADD RESIDENCE TIME
CB 9003 FORMAT ('   TOTIM (T)     FLOWRATE     DIAM.         RE', 21X,    
CB     +        20(A14))
 9003 FORMAT ('   TOTIM (T)     FLOWRATE     DIAM.         RE          
     + RT', 21X, 20(A14))    
CB 9004 FORMAT (F14.0, 1X, 3G13.5, 20(G14.6))
 9004 FORMAT (F14.0, 1X, 4G13.5, 20(G14.6))
C
      END SUBROUTINE TS1OUT
C
C
C
C
C
      SUBROUTINE FLOW_OUTPUT(KPER, KSTP)
C     *********************************************************
C     OUTPUT IN FILE RESULT
C     *********************************************************
C     VERSION 1  6 DEZ 1998
C
CB** X0 IS B_MAT
CB**GEOTIMES IS DELT
CB**TOT_TIME IS TOTIM
CBARC**ADD TAUI
      USE CONSTANTS, ONLY: DZ,PI,DOS, NEARZERO_30
      USE CFPMODULE, ONLY:NNOD, NBR, CON_DATA, MXNODE, MXTUBE,          
     +    BEGHEAD, EPSILON, ITERA_MAX, TURB, B_MAT, DTUBE, ITUBE, KONV, 
     +    MATFLOW, TUB_REYNOLD, QBDIR, NODE_LOCA, GEOHIGHT, NODEBOT,    
     +    TORTUOS, ITERS, TAUI, QFHLQ,CADSFLOW,      
     +    QWELL, PFPSFLOW, QCYLQ, CYFLOW, QLH, QFIX, QSDIR              !TR: 2012 05 17 FHLQ / CADS / WELL; 2012 07 16 PFPS / 2013 03 14 CAUCHY / LH // 2014 02 20 CADS RECHARGE
C
      USE GLOBAL, ONLY:HNEW, NCOL, NROW, NLAY, IUNIT, IOUT, LENUNI,     
     +    ITMUNI
      USE GWFBASMODULE, ONLY:DELT, TOTIM
C
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT
C
C--FLAGS READ BY CON1RP FROM FILE 'CONDUIT_MOD':
C HEAD_FLAG     OUTPUT OF HEADS => KANN ENTFERNT WERDEN (BIRK0799)
C FLOW_FLAG     OUTPUT OF FLOW  => KANN ENTFERNT WERDEN (BIRK0799)
C IUNIT         INPUT IUNITS (SEE MODFLOW INSTRUCTION AND README FILE)
C JAN           IF JAN.EQ.0 FIRST CALL AND PRINT CONSTANT VALUES
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KPER, KSTP
C
C--LOCAL VARIABLES
      INTEGER I, IC, IL, IR, ITEST, J
C
C--BARC**TAUI IS NOW AN ARRAY
CB      DOUBLE PRECISION TAUI, X, Y, Z, LENGTH
CBARC
      DOUBLE PRECISION  X, Y, Z, LENGTH
C
C--SET VALUES
!     MAXI = 0
!     TAUIMAX = 0.D0
!     TAUCOUNTER = 0
C--WRITE CONSTANT PAPRAMETERS JUST ONCE AT BEGINNING OF OUTPUT
C 
      WRITE (IOUT, *)
      WRITE (IOUT, *)
      WRITE (IOUT, 9001) EPSILON
      WRITE (IOUT, 9002) ITERA_MAX
C
      WRITE (IOUT, *)
      WRITE (IOUT, *)
      WRITE (IOUT, *) 'CFPM1 RESULTS ',                                 
     +         '+++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE (IOUT, *)
      WRITE (IOUT, *) 'STRESS PERIOD/TIME STEP', KPER, KSTP
C
      WRITE (IOUT, *)
      WRITE (IOUT, 9003) ITERS
      IF ( KONV.EQ.0 ) WRITE (IOUT, *) ' !! NO CONVERGENCE !'
      WRITE (IOUT, *)
C
C--OUTPUT OF HEADS IF HEAD_FLAG NOT EQUAL DZ
      WRITE (IOUT, *) '------- RESULTS OF FLOW CALCULATION -------'
      WRITE (IOUT, *)
C
C--BARC***CHANGE THIS BASED ON LENUNI AND ITMUNI
C
C--BARC**UNITS ARE M SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9004)
     +'NODE#  NODE HEAD[M]   MATRIX HEAD [M] EXCHANGE [M^3/S]'
C--BARC**UNITS ARE M MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9004)
     +'NODE#  NODE HEAD[M]   MATRIX HEAD [M] EXCHANG[M^3/MIN]'
C--BARC**UNITS ARE M HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9004)
     +'NODE#  NODE HEAD[M]   MATRIX HEAD [M]     EXCH[M^3/HR]'
C--BARC**UNITS ARE M DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9004)
     +'NODE#  NODE HEAD[M]   MATRIX HEAD [M] EXCHANGE [M^3/D]'
C--BARC**UNITS ARE M YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9004)
     +'NODE#  NODE HEAD[M]   MATRIX HEAD [M]    EXCH [M^3/YR]'
C 
C--BARC**FEET**
C--BARC**UNITS ARE FT SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[FT]  MATRIX HEAD [FT]  EXCH [FT^3/SEC]'
C--BARC**UNITS ARE FT MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[FT]  MATRIX HEAD [FT]  EXCH [FT^3/MIN]'
C--BARC**UNITS ARE FT HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[FT]  MATRIX HEAD [FT]   EXCH [FT^3/HR]'
C--BARC**UNITS ARE FT DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[FT]  MATRIX HEAD [FT]    EXCH [FT^3/D]'
C--BARC**UNITS ARE FT YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[FT]  MATRIX HEAD [FT]   EXCH [FT^3/YR]'
C 
C--BARC**CM
C--BARC**UNITS ARE CM SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[CM]  MATRIX HEAD [CM]  EXCH [CM^3/SEC]'
C--BARC**UNITS ARE CM MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[CM]  MATRIX HEAD [CM]  EXCH [CM^3/MIN]'
C--BARC**UNITS ARE CM HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[CM]  MATRIX HEAD [CM]   EXCH [CM^3/HR]'
C--BARC**UNITS ARE CM DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[CM]  MATRIX HEAD [CM]    EXCH [CM^3/D]'
C--BARC**UNITS ARE CM YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9004)
     +'NODE# NODE HEAD[CM]  MATRIX HEAD [CM]   EXCH [CM^3/YR]'
C 
      DO I = 1, MXNODE
        IC = NBR(I, 2)
        IR = NBR(I, 3)
        IL = NBR(I, 4)
C
C--DIFFERENT OUTPUT IF NODE IS DRY                                      !TR: 2013 06 06 TODO // SEEMS THIS DO-LOOP IS REDUNDANT
        DO J = 1, NNOD
          ITEST = NBR(I, J+4+NNOD)
C
C-BARC**ITEST IS A TUBE NUMBER !!!
          IF (ITEST.NE.0) NODEBOT(I) = GEOHIGHT(I) - CON_DATA(ITEST, 2) !TR: 2013 06 06 REPLACED NODEBOT BY NODEBOT(I)
     +                                /DOS
        ENDDO
C 
C-BARC**DRY CASE
        IF ( B_MAT(I).LE.NODEBOT(I) ) THEN
C
!RSR      IF ( BEGHEAD(I).EQ.-1.D0 ) THEN
          IF ( DABS(BEGHEAD(I)+1.0D0).LT.NEARZERO_30 ) THEN
            WRITE (IOUT, 9019) I, NODEBOT(I), HNEW(IC, IR, IL),         
     +                         MATFLOW(I), CADSFLOW(I), QSDIR(I), 
     +                         PFPSFLOW(I),QBDIR(I),QWELL(I), QFHLQ(I), 
     +                         CYFLOW(I), QCYLQ(I), QLH(I), QFIX(I)     !TR: 2012 05 17 FHLQ, CADS, WELL; 2012 07 16 PFPS / 2013 03 14 CAUCHY / 2013 03 25 LH // 2014 02 20 CADS RECHARGE
          ELSE
            WRITE (IOUT, 9020) I, NODEBOT(I), HNEW(IC, IR, IL),         
     +                         MATFLOW(I), CADSFLOW(I), QSDIR(I),
     +                         PFPSFLOW(I), QBDIR(I), QWELL(I),  
     +                         QFHLQ(I), CYFLOW(I),QCYLQ(I), QLH(I), 
     +                         QFIX(I)                                  !TR: 2012 05 17 FHLQ, CADS, WELL; 2012 07 16 PFPS / 2013 03 14 CAUCHY / 2013 03 25 LH // 2014 02 20 CADS RECHARGE
          ENDIF
C 
C--BARC**ELSEIF TO END DRY CASE
!RSR    ELSEIF ( BEGHEAD(I).EQ.-1.D0 ) THEN
        ELSEIF ( DABS(BEGHEAD(I)+1.0D0).LT.NEARZERO_30 ) THEN
          WRITE (IOUT, 9019) I, B_MAT(I), HNEW(IC, IR, IL), MATFLOW(I),
     +                       CADSFLOW(I), QSDIR(I),PFPSFLOW(I),QBDIR(I),
     +                       QWELL(I), QFHLQ(I), CYFLOW(I), QCYLQ(I),   !TR: 2012 05 17 FHLQ, CADS, WELL; 2012 07 16 PFPS / 2013 03 14 CAUCHY
     +                       QLH(I), QFIX(I)                            !TR: 2013 03 25 LH // 2014 02 20 CADS RECHARGE
        ELSE
          WRITE (IOUT, 9020) I, B_MAT(I), HNEW(IC, IR, IL), MATFLOW(I),
     +                       CADSFLOW(I), QSDIR(I),PFPSFLOW(I),QBDIR(I), 
     +                       QWELL(I), QFHLQ(I), CYFLOW(I), QCYLQ(I),   !TR: 2012 05 17 FHLQ, CADS, WELL; 2012 07 16 PFPS / CAUCHY
     +                       QLH(I), QFIX(I)                            !TR: 2013 03 25 LH // CADS RECHARGE
        ENDIF
C 
C--BARC**ENDDO FOR MXNODE
      ENDDO
C
      WRITE (IOUT, *)
C
C--OUTPUT OF FLOW IF FLOW_FLAG NOT EQUAL DZ
CBIRK0799     IF (FLOW_FLAG .NE. 0) THEN
C 
C--BARC**UNITS ARE M SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9021)
C--BARC**UNITS ARE M MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9022)
C--BARC**UNITS ARE M HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9023)
C--BARC**UNITS ARE M DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9024)
C--BARC**UNITS ARE M YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.2 ) WRITE (IOUT, 9025)
C
C--BARC**FT UNITS
C--BARC**UNITS ARE FT SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9026)
C--BARC**UNITS ARE FT MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9027)
C--BARC**UNITS ARE FT HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9028)
C--BARC**UNITS ARE FT DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9029)
C--BARC**UNITS ARE FT YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.1 ) WRITE (IOUT, 9030)
C
C--BARC**CM UNITS
C--BARC**UNITS ARE CM SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9031)
C--BARC**UNITS ARE CM MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9032)
C--BARC**UNITS ARE CM HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9033)
C--BARC**UNITS ARE CM DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9034)
C--BARC**UNITS ARE CM YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.3 ) WRITE (IOUT, 9035)
C
C--CALCULATE RESIDENCE TIME TAUI IN TUBE I: PI*(D**2/4)*L IN SECONDS
C--BARC**NEED TO MODIFY FOR FREE SURFACE CASES ?
      DO I = 1, MXTUBE
        IF ( DTUBE(I,1).NE.0.0D0 ) THEN
C
C--BARC
          X = DABS(NODE_LOCA(ITUBE(I,2),2)-NODE_LOCA(ITUBE(I,3),2))
          Y = DABS(NODE_LOCA(ITUBE(I,2),3)-NODE_LOCA(ITUBE(I,3),3))
          Z = DABS(NODE_LOCA(ITUBE(I,2),4)-NODE_LOCA(ITUBE(I,3),4))
          LENGTH = 1.0D0
C
C--BARC**FOR CASES OF Z=0
          IF ( Z.LT.NEARZERO_30 ) THEN
            IF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
              LENGTH = Y
            ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
              LENGTH = X
            ELSE
              LENGTH = DSQRT(X**DOS+Y**DOS)
            ENDIF
C
C--BARC**FOR CASES OF Z>0
          ELSEIF ( X.LT.NEARZERO_30 .AND. Y.GT.0.0D0 ) THEN
            LENGTH = DSQRT(Y**DOS+Z**DOS)
          ELSEIF ( X.GT.0.0D0 .AND. Y.LT.NEARZERO_30 ) THEN
            LENGTH = DSQRT(X**DOS+Z**DOS)
          ELSE
            LENGTH = Z
          ENDIF
C 
          TAUI(I) = DABS((LENGTH*TORTUOS(I))                            
     +           /(DTUBE(I,1)/(PI*(CON_DATA(I,2)/DOS)**DOS)))
CB          TAUI = DABS((LENGTH*TORTUOS(I))                               
CB     +           /(DTUBE(I,1)/(PI*(CON_DATA(I,2)/DOS)**DOS)))
        ELSE
          TAUI(I) = DZ
        ENDIF
C
C--CHECK WITH TIMESTEP-LENGTH
        IF ( TAUI(I).EQ.DZ ) WRITE (IOUT, 9036) I
C
C--FLOW IS LAMINAR IN TUBE I
        IF ( TURB(I).EQ.0 ) THEN
C
C--SEBA FOR OUT
          WRITE (IOUT, 9037) I, ITUBE(I, 2), ITUBE(I, 3), DTUBE(I, 1),  
     +                       (CON_DATA(I,J), J=2, 3), TUB_REYNOLD(I),   
     +                       TAUI(I)
C 
        ELSE
C 
C--FLOW IS TURBULENT IN TUBE I
          WRITE (IOUT, 9038) I, ITUBE(I, 2), ITUBE(I, 3), DTUBE(I, 1),  
     +                       (CON_DATA(I,J), J=2, 3), TUB_REYNOLD(I),   
     +                       TAUI(I)
C 
        ENDIF
C
      ENDDO
C      
C--OUTPUT IF A FAILURE OCCURS
!     WRITE (IOUT, *) 'ERROR WRITING FLOW RESULT TO OUTPUT FILE '
 9001 FORMAT (' CFPM1 CRITERION OF CONVERGENCE:', E18.7)
 9002 FORMAT (' CFPM1 MAXIMUM NUMBER OF ITERATIONS:', I6)
 9003 FORMAT ('NUMBER OF NEWTON RHAPSON ITERATIONS:', I6)
 9004 FORMAT (A54,'      CAD STORAGE    CADS RECHARGE         PFPSFLOW',
     +        '  DIRECT RECHARGE            QWELL             FHLQ',
     +        '           CAUCHY        CAUCHY LQ              QLH',
     +        '             QFIX')                                       !TR: 2012 05 17 FHLQ, CADS, WELL, PFPS / 2013 03 14 CAUCHY / 2013 03 25 LH // CADS RECHARGE
 9019 FORMAT (I5, G14.7, 5X, G13.7, 11(1X, G16.7))
 9020 FORMAT (I5,2X,G9.4,'FIX',5X, G13.7, 11(1X, G16.7))
 9021 FORMAT ('TUBE  B  E  FLOW          Q [M^3/SEC]     DIAM.[M]', 8X, 
     +        'LEN.[M]        RE         RESIDENCE TIME [S]')
 9022 FORMAT ('TUBE  B  E  FLOW          Q [M^3/MIN]     DIAM.[M]', 8X, 
     +        'LEN.[M]        RE         RESIDENCE TIME [MIN]')
 9023 FORMAT ('TUBE  B  E  FLOW          Q [M^3/HR]      DIAM.[M]', 8X, 
     +        'LEN.[M]        RE         RESIDENCE TIME [HR]')
 9024 FORMAT ('TUBE  B  E  FLOW          Q [M^3/D]       DIAM.[M]', 8X, 
     +        'LEN.[M]        RE         RESIDENCE TIME [D]')
 9025 FORMAT ('TUBE  B  E  FLOW          Q [M^3/YR]      DIAM.[M]', 8X, 
     +        'LEN.[M]        RE         RESIDENCE TIME [YR]')
 9026 FORMAT ('TUBE  B  E  FLOW          Q [FT^3/SEC]     DIAM.[FT]',   
     +        7X, 'LEN.[FT]       RE         RESIDENCE TIME [S]')
 9027 FORMAT ('TUBE  B  E  FLOW          Q [FT^3/MIN]     DIAM.[FT]',   
     +        7X, 'LEN.[FT]       RE         RESIDENCE TIME [MIN]')
 9028 FORMAT ('TUBE  B  E  FLOW          Q [FT^3/HR]      DIAM.[FT]',   
     +        7X, 'LEN.[FT]       RE         RESIDENCE TIME [HR]')
 9029 FORMAT ('TUBE  B  E  FLOW          Q [FT^3/D]       DIAM.[FT]',   
     +        7X, 'LEN.[FT]       RE         RESIDENCE TIME [D]')
 9030 FORMAT ('TUBE  B  E  FLOW          Q [FT^3/YR]      DIAM.[FT]',   
     +        7X, 'LEN.[FT]       RE         RESIDENCE TIME [YR]')
 9031 FORMAT ('TUBE  B  E  FLOW          Q [CM^3/SEC]     DIAM.[CM]',   
     +        7X, 'LEN.[CM]       RE         RESIDENCE TIME [SEC]')
 9032 FORMAT ('TUBE  B  E  FLOW          Q [CM^3/MIN]     DIAM.[CM]',   
     +        7X, 'LEN.[CM]       RE         RESIDENCE TIME [MIN]')
 9033 FORMAT ('TUBE  B  E  FLOW          Q [CM^3/HR]      DIAM.[CM]',   
     +        7X, 'LEN.[CM]       RE         RESIDENCE TIME [HR]')
 9034 FORMAT ('TUBE  B  E  FLOW          Q [CM^3/D]       DIAM.[CM]',   
     +        7X, 'LEN.[CM]       RE         RESIDENCE TIME [D]')
 9035 FORMAT ('TUBE  B  E  FLOW          Q [CM^3/YR]      DIAM.[CM]',   
     +        7X, 'LEN.[CM]       RE         RESIDENCE TIME [YR]')
 9036 FORMAT (4X, 'WARNING! TUBE', I5, ' ACTIVE BUT NO FLOW')
 9037 FORMAT (I4, I4, I4, '   LAMI. ', 5F15.5)                          !TR: 2013 06 04 FORMAT CHANGE
 9038 FORMAT (I4, I4, I4, '   TURB. ', 5F15.5)                          !TR: 2013 06 04 FORMAT CHANGE
C 
      END SUBROUTINE FLOW_OUTPUT
C
C
C
C
C
      SUBROUTINE BUDGET_OUTPUT(KPER, KSTP)
C********************************************************
C      OUTPUT OF CONDUIT BUDGET
C********************************************************
C VERSION 1 22MAR1996   BUDGET_OUTPUT
C
      USE CFPMODULE, ONLY:BUD_SOURCE, BUD_SINK,MXNODE, BUD_MATIN,       !TR: 2012 04 24 KU_QBDIR REMOVED
     + BUD_MATOUT, TOTAL_BUDGET, BUD_TOTAL_STEP, TS_IN_MAT, TS_OUT_MAT, !TR: 2012 04 24 BUD_QBDIR REMOVED
     + TS_BUD_SINK, TS_BUD_SOURCE, TS_IN, TS_OUT, DIFF_TOT, DIFF_TS,    !TR: 2012 04 24 TS_QBDIR REMOVED
     + TOT_FLOW, TS_TOT_FLOW, BUD_STORAGE_IN, TS_IN_PFPS,               
     + BUD_STORAGE_OUT, TS_OUT_PFPS, BUD_QBDIRIN, BUD_QBDIROUT,
     + TS_IN_QBDIR, TS_OUT_QBDIR, KU_IN, KU_OUT,                        !TR: 2012 04 24 TO DISTIGUISH RECHARGE IN- AND OUT/ 2012 07 17 KUMULATIVE TERMS
     + BUD_CADSIN, BUD_CADSOUT, TS_IN_CADS, TS_OUT_CADS,                !TR: 2012 04 26 CAD STORAGE 
     + BUD_CDSCONIN, BUD_CDSCONOUT, TS_IN_CDSCON, TS_OUT_CDSCON,        !TR: 2014 04 16 CADS
     + BUD_FHLQIN, BUD_FHLQOUT, TS_IN_FHLQ, TS_OUT_FHLQ,                !TR: 2012 05 11 FHLQ
     + BUD_WELLIN, BUD_WELLOUT, TS_IN_WELL, TS_OUT_WELL,                !TR: 2012 06 08 WELL
     + BUD_CYIN, BUD_CYOUT, TS_IN_CY, TS_OUT_CY,                        !TR: 2013 03 14 CAUCHY
     + BUD_CYLQIN, BUD_CYLQOUT, TS_IN_CYLQ, TS_OUT_CYLQ,                !TR: 2013 03 14 CAUCHY LQ
     + BUD_LHIN, BUD_LHOUT, TS_IN_LH, TS_OUT_LH,                        !TR: 2013 03 25 LH
     + BUD_QSDIRIN, BUD_QSDIROUT,TS_IN_QSDIR, TS_OUT_QSDIR,             !TR: 2014 02 20 CADS RECHARGE
     + CY_FLG, CADS_FLG, WELL_FLG, FHLQ_FLG, LH_FLG, CADSML_FLG         !TR: 2013 03 19 FLAGS FOR OUTPUT
C
C--BARC***COULD REPLACE IOUT WITH DIFFERENT UNIT NUMBER**
      USE GLOBAL, ONLY:IOUT
      USE GWFBASMODULE, ONLY:VBVL, VBNM, TOTIM
C
      IMPLICIT NONE
C
C--INTERNALE VARIABLE:
C I                     INDEX
C TOTAL_BUDGET          TOTAL FLOW DIFFERENCE
C PERCENT_BUDGET        PERCENT OF TOTAL FLOW DIFFERENCE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KPER, KSTP
C
C--SEBA--WRITE CONDUIT BUDGET
      WRITE (IOUT, *)
      WRITE (IOUT, 9001) KSTP, KPER
      WRITE (IOUT, *) ' TOTAL SIMULATION TIME', TOTIM, 'S'
      WRITE (IOUT, *)
C
      WRITE (IOUT, 9026)
C 
C--BARC-------------------IN
      WRITE (IOUT, 9002) 
      WRITE (IOUT, 9003) BUD_SOURCE, TS_BUD_SOURCE 
      IF(LH_FLG)
     +WRITE (IOUT, 9021) DABS(BUD_LHIN), DABS(TS_IN_LH)                 !TR: 2012 05 11 LH
      IF(FHLQ_FLG)
     +WRITE (IOUT, 9017) DABS(BUD_FHLQIN), DABS(TS_IN_FHLQ)             !TR: 2012 05 11 FHLQ
      IF(CY_FLG) WRITE (IOUT, 9019) DABS(BUD_CYIN), DABS(TS_IN_CY)      !TR: 2013 03 14 CAUCHY
      IF(CY_FLG) WRITE (IOUT, 9020) DABS(BUD_CYLQIN), DABS(TS_IN_CYLQ)  !TR: 2013 03 14 CAUCHY LQ
      WRITE (IOUT, 9005) DABS(BUD_MATIN), DABS(TS_IN_MAT)      
      WRITE (IOUT, 9004) BUD_QBDIRIN, TS_IN_QBDIR                       !TR: 2012 04 24 
      IF(WELL_FLG)
     +WRITE (IOUT, 9018) DABS(BUD_WELLIN), DABS(TS_IN_WELL)             !TR: 2012 06 08 WELL
      IF ((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1))     !TR: 2014 02 20 CADS RECHARGE
     +WRITE (IOUT, 9016) DABS(BUD_CDSCONIN),DABS(TS_IN_CDSCON)          !TR: 2014 04 16 CADS
      WRITE (IOUT, 9006) DABS(BUD_STORAGE_IN), DABS(TS_IN_PFPS)         !TR: 2012 07 17 PFPS
      WRITE (IOUT, 9007) KU_IN, TS_IN
C
C--BARC-------------------OUT
      WRITE (IOUT, 9008) 
      WRITE (IOUT, 9003) DABS(BUD_SINK), DABS(TS_BUD_SINK)
      IF(LH_FLG)
     +WRITE (IOUT, 9021) DABS(BUD_LHOUT), DABS(TS_OUT_LH)               !TR: 2013 03 25 LH
      IF(FHLQ_FLG)
     +WRITE (IOUT, 9017) DABS(BUD_FHLQOUT), DABS(TS_OUT_FHLQ)           !TR: 2012 05 11 FHLQ
      IF(CY_FLG) WRITE (IOUT, 9019) DABS(BUD_CYOUT), DABS(TS_OUT_CY)    !TR: 2013 03 14 CAUCHY
      IF(CY_FLG) WRITE (IOUT, 9020) DABS(BUD_CYLQOUT), DABS(TS_OUT_CYLQ)!TR: 2013 03 14 CAUCHY LQ
      WRITE (IOUT, 9005) DABS(BUD_MATOUT), DABS(TS_OUT_MAT)      
      WRITE (IOUT, 9004) DABS(BUD_QBDIROUT), DABS(TS_OUT_QBDIR)         !TR: 2012 04 24 
      IF(WELL_FLG) 
     +WRITE (IOUT, 9018) DABS(BUD_WELLOUT), DABS(TS_OUT_WELL)           !TR: 2012 06 08 WELL
      IF ((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1))     !TR: 2014 02 20 CADS RECHARGE
     +  WRITE (IOUT, 9027) DABS(BUD_CDSCONOUT), DABS(TS_OUT_CDSCON)     !TR: 2014 04 16 CADS
      WRITE (IOUT, 9006) DABS(BUD_STORAGE_OUT), DABS(TS_OUT_PFPS)       !TR: 2012 07 17 PFPS
      WRITE (IOUT, 9012) KU_OUT, TS_OUT
C
      WRITE (IOUT, 9013) DIFF_TOT, DIFF_TS
      WRITE (IOUT, 9014) (DIFF_TOT/TOT_FLOW*100.D0),                    
     +                   (DIFF_TS/TS_TOT_FLOW*100.D0)
C
C--PRINT WARNING IF BUDGET IS WRONG
      IF ( (DIFF_TS/TS_TOT_FLOW).GE.0.0001 ) WRITE (IOUT, *)            
     +      ' WARNING! ', 'PIPE  BUDGET WRONG BY MORE THAN 0.0001 !'
C
C--PRINT CADS BUDGET
      IF ((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1)) THEN!TR: 2014 02 20 CADS RECHARGE
        WRITE (IOUT, 9025)
        WRITE (IOUT, 9026)
C
C--IN        
        WRITE (IOUT, *)
        WRITE (IOUT, *)'TO CONDUIT SYSTEM:'
        WRITE (IOUT, 9022) BUD_QSDIRIN, TS_IN_QSDIR                     !TR: 2014 02 20 CADS RECHARGE
        WRITE (IOUT, 9023) DABS(BUD_CADSIN), DABS(TS_IN_CADS)           !TR: 2012 04 26 CADS
        WRITE (IOUT, 9024)
        WRITE (IOUT, 9016) DABS(BUD_CADSIN) + BUD_QSDIRIN,              !TR: 2014 02 21 CADS RECHARGE
     +                     DABS(TS_IN_CADS) + TS_IN_QSDIR               !TR: 2014 02 21 CADS RECHARGE
C
C--OUT
        WRITE (IOUT, *)
        WRITE (IOUT, *) 'FROM CONDUIT SYSTEM:'        
        WRITE (IOUT, 9022) DABS(BUD_QSDIROUT), DABS(TS_OUT_QSDIR)       !TR: 2014 02 20 CADS RECHARGE
        WRITE (IOUT, 9023) DABS(BUD_CADSOUT), DABS(TS_OUT_CADS)         !TR: 2012 04 26 CADS
        WRITE (IOUT, 9024)
        WRITE (IOUT, 9016) DABS(BUD_QSDIROUT)+DABS(BUD_CADSOUT), 
     +                     DABS(TS_OUT_QSDIR)+DABS(TS_OUT_CADS)         !TR: 2014 02 20 CADS RECHARGE
      ENDIF                                                             !TR: 2014 02 20 CADS RECHARGE
C      
      RETURN
C 
 9001 FORMAT (/, 4X, 'BUDGET OF THE PIPE SYSTEM OF TIMESTEP', I5,       
     +        ' IN STRESS PERIOD', I5, /, 72('-'), /)
 9002 FORMAT (/, 11X, ' IN:', 31X, 'IN:')
 9003 FORMAT (5X,'CONSTANT HEAD =',G16.8,6X,'CONSTANT HEAD =',G16.8)
 9004 FORMAT (5X,'PIPE RECHARGE =',G16.8,6X,'PIPE RECHARGE =',G16.8)
 9005 FORMAT (3X,'MATRIX EXCHANGE =',G16.8,4X,'MATRIX EXCHANGE =',G16.8)
 9006 FORMAT (7X,'PFP STORAGE =', G16.8, 8X, 'PFP STORAGE =', G16.8)
 9007 FORMAT (/,5X,'CUMULATIVE IN =',G16.8,11X,'TOTAL IN =', G16.8)
 9008 FORMAT (/, 11X, 'OUT:', 31X, 'OUT:')
 9012 FORMAT (/,4X,'CUMULATIVE OUT =', G16.8, 10X,'TOTAL OUT =', G16.8)
 9013 FORMAT (/,10X,'IN - OUT =',G16.8,11X,'IN - OUT =',G16.8)
 9014 FORMAT (5X,'PERCENT ERROR =',F6.2,16X,'PERCENT ERROR =',F6.2, //)
 9016 FORMAT (7X,'CADS -> CON =',G16.8,8X,'CADS -> CON =',              !TR: 2014 02 20 CADS RECHARGE
     + G16.8)                                                           !TR: 2014 02 20 CADS RECHARGE 
 9017 FORMAT (6X,'FHLQ BC (LQ) =', G16.8,7X,'FHLQ BC (LQ) =',           !TR: 2012 05 11 FHLQ 
     +        G16.8)   
 9018 FORMAT (11X, 'WELL BC =', G16.8,12X,'WELL BC =', G16.8)           !TR: 2012 06 08 WELL
 9019 FORMAT (9X, 'CAUCHY BC =', G16.8,10X,'CAUCHY BC =', G16.8)        !TR: 2013 03 14 CAUCHY
 9020 FORMAT (4X, 'CAUCHY BC (LQ) =',G16.8,5X,'CAUCHY BC (LQ) =', G16.8)!TR: 2013 03 14 CAUCHY LQ
 9021 FORMAT (1X, 'LIMITED HEAD (LH) =',G16.8,2X,'LIMITED HEAD (LH) =', 
     + G16.8)                                                           !TR: 2013 03 25 LH
 9022 FORMAT (1X, 'CAD DIR. RECHARGE =',G16.8,2X,'CAD DIR. RECHARGE =', !TR: 2014 02 20 CADS RECHARGE
     + G16.8)                                                           !TR: 2014 02 20 CADS RECHARGE
 9023 FORMAT (7X,'CAD STORAGE =', G16.8, 8X, 'CAD STORAGE =',G16.8)     !TR: 2012 04 26 CADS     
 9024 FORMAT (20X,'----------------',21X,'----------------')     
 9025 FORMAT (/,4X,'BREAK DOWN CADS FLOWS',/,72('-'),/)    !TR: 2014 02 21 
 9026 FORMAT (8X,'CUMULATIVE VALUES (L**3)',8X,
     +        'RATES THIS TIME STEP (L**3/T)')
 9027 FORMAT (7X,'CADS <- CON =',G16.8,8X,'CADS <- CON =',              !TR: 2014 02 20 CADS RECHARGE
     + G16.8)                                                           !TR: 2014 02 20 CADS RECHARGE      
C
      END SUBROUTINE BUDGET_OUTPUT     
C
C
C
C
C
      SUBROUTINE NWBOUT(KPER, KSTP)
C***********************************************************************
C     OUTPUT OF NODE WATER BUDGET
C***********************************************************************
C
      USE CFPMODULE, ONLY:MXNODE, QMAT, QTUBIN, QTUBOUT, QFIX, QBDIR,   
     +    NODIN, NODOUT, PFPSFLOW, QSDIR,                               !TR: 2012 06 08 NSTOR REPLACED BY PFPSFLOW // 2014 02 21 CADS RECHARGE
     +    CADSFLOW, QFHLQ, QWELL, QCYLQ, CYFLOW, QLH                    !TR: 2012 05 11 CADS / FHLQ / WELL / 2013 03 18 CAUCHY / 2013 03 25 LH
C
      USE GLOBAL, ONLY:IOUT, LENUNI, ITMUNI
      USE GWFBASMODULE, ONLY:TOTIM
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KPER, KSTP
C
C--LOCAL VARIABLES
      INTEGER N
C
C--WRITE HEADING AND TIME INFORMATION
      WRITE (IOUT, *)
      WRITE (IOUT, *)                                                   
     +'------------------------NODE WATER BUDGET-----------------------'
C
      WRITE (IOUT, *) ' STRESS PERIOD', KPER, ' TIME STEP', KSTP
C 
C--BARC**SOME CHANGES HERE FOR UNITS CONVERSIONS
      WRITE (IOUT, *) 'TOTAL SIMULATION TIME', TOTIM
      WRITE (IOUT, *)
C 
C--BARC**UNITS ARE M SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.2 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN M^3/S'
C
C--BARC**UNITS ARE M MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.2 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN M^3/MIN'
C
C--BARC**UNITS ARE M HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.2 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN M^3/HR'
C
C--BARC**UNITS ARE M DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.2 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN M^3/D'
C
C--BARC**UNITS ARE M YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.2 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN M^3/YR'
C 
C--BARC**UNITS ARE FT SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.1 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN FT^3/S'
C
C--BARC**UNITS ARE FT MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.1 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN FT^3/MIN'
C
C--BARC**UNITS ARE FT HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.1 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN FT^3/HR'
C 
C--BARC**UNITS ARE FT DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.1 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN FT^3/D'
C
C--BARC**UNITS ARE FT YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.1 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN FT^3/YR'
C 
C--BARC**UNITS ARE CM SEC
      IF ( ITMUNI.EQ.1 .AND. LENUNI.EQ.3 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN CM^3/S'
C
C--BARC**UNITS ARE CM MIN
      IF ( ITMUNI.EQ.2 .AND. LENUNI.EQ.3 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN CM^3/MIN'
C
C--BARC**UNITS ARE CM HOURS
      IF ( ITMUNI.EQ.3 .AND. LENUNI.EQ.3 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN CM^3/HR'
C
C--BARC**UNITS ARE CM DAYS
      IF ( ITMUNI.EQ.4 .AND. LENUNI.EQ.3 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN CM^3/D'
C
C--BARC**UNITS ARE CM YEAR
      IF ( ITMUNI.EQ.5 .AND. LENUNI.EQ.3 ) WRITE (IOUT, *)              
     +      'FLOW TO THE NODE IN CM^3/YR'
C 
      WRITE (IOUT, *)
C
C--WRITE NODE WATER BUGDGET
C--BARC**NEED TO ADD STORAGE HERE
      WRITE (IOUT, 9001)
C
C--BARC**NEED TO ADD STORAGE HERE
      DO N = 1, MXNODE
        WRITE (IOUT, 9002) N,QFIX(N),QBDIR(N),QWELL(N),-QMAT(N),
     +                     PFPSFLOW(N),QTUBIN(N),QTUBOUT(N),            !TR: 2012 07 16 NSTOR REPLACED BY PFPSFLOW
     +                     -CADSFLOW(N)+QSDIR(N),QFHLQ(N),-CYFLOW(N),   !TR: 2014 02 21 CADS RECHARGE
     +                     QCYLQ(N),QLH(N),(NODIN(N)-NODOUT(N))         !TR: 2012 06 08 CADS / FHLQ / WELL / 2013 03 14 CAUCHY / 2013 03 25 LH
      ENDDO
C
 9001 FORMAT (' NODE     FIXED HEAD',5X,'RECHARGE',8X,'QWELL',2X,       
     +        'MATRIX EXCH',6X,'STORAGE',6X,'TUBE_IN',5X,'TUBE_OUT',9X, 
     +        'CADS',5X,'FHLQ(LQ)',7X,'CAUCHY',3X,
     +        'CAUCHY(LQ)',10X,'QLH',5X,'IN - OUT')                     !TR: 2013 03 14 CAUCHY / 2013 03 25 LH // 2014 02 21 CADS RECHARGE
!9002 FORMAT (I3, 2X, 7G16.8)
 9002 FORMAT (I5, 2X, 14G13.5)                                           !TR: 2012 05 11 ADDED CADS FHLQ WELL / 2013 03 14 CAUCHY / 2013 03 25 LH / 2013 06 04 FORMAT CHANGE / CADS RECHARGE
C
      END SUBROUTINE NWBOUT
C
C
C
C
C      
      SUBROUTINE TSA_OUT
C***********************************************************************
C     TIME SERIES ANALYSIS OUTPUT
C     LAST CHANGE 2013 05 13 BY THOMAS.REIMANN@TU-DRESDEN.DE      
C***********************************************************************
C
      USE CFPMODULE
      USE GWFBASMODULE, ONLY:TOTIM
      USE GLOBAL, ONLY:IOUT
C
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES                              
      CHARACTER(LEN=16) FNAME
C
C--SET UP OUTPUT FILE FOR THE FIRST TIME
      IF (TSA_FUNIT.EQ.-1) THEN
        TSA_FUNIT = 500
        FNAME = 'TSA.out'
        OPEN (TSA_FUNIT, FILE=FNAME, STATUS='UNKNOWN', ERR=100)
        WRITE (IOUT, *) 'TIME SERIES ANALYSIS OUTPUT IN FILE ', FNAME
        WRITE (TSA_FUNIT, 9001)  
      ENDIF 
C
C--WRITE TSA DATA
      WRITE (TSA_FUNIT, 9002) TOTIM,TS_BUD_SOURCE,DABS(TS_IN_LH),
     +  DABS(TS_IN_FHLQ),DABS(TS_IN_CY),DABS(TS_IN_CYLQ),
     +  DABS(TS_IN_MAT),TS_IN_QBDIR,DABS(TS_IN_WELL),DABS(TS_IN_CDSCON),
     +  DABS(TS_IN_PFPS),TS_IN,DABS(TS_BUD_SINK),
     +  DABS(TS_OUT_LH),DABS(TS_OUT_FHLQ),DABS(TS_OUT_CY),
     +  DABS(TS_OUT_CYLQ),DABS(TS_OUT_MAT),DABS(TS_OUT_QBDIR),
     +  DABS(TS_OUT_WELL),DABS(TS_OUT_CDSCON),
     +  DABS(TS_OUT_PFPS),TS_OUT
      RETURN
C      
 100  WRITE (IOUT, *) 'ERROR WHILE OPENING UNIT',TSA_FUNIT,' (TSA_OUT)!'                    
 9001 FORMAT ('     TOTIM    FIX(IN)     LH(IN)   FHLQ(IN)     ',
     +        'CY(IN)   CYLQ(IN)    MAT(IN)   QDIR(IN)   WELL(IN)   ',
     +        'CADS(IN)   PFPS(IN)    ALL(IN)   FIX(OUT)   ',
     +        ' LH(OUT) ',' FHLQ(OUT)    CY(OUT)  CYLQ(OUT)   MAT(OUT)',
     +        '  QDIR(OUT) ',' WELL(OUT)  CADS(OUT) ',
     +        ' PFPS(OUT)  ',' ALL(OUT)')
 9002 FORMAT (F10.0,x,24(E10.4,x))          
C
      END SUBROUTINE TSA_OUT
C
C
C
C
C      
      SUBROUTINE TSAN_OUT
C***********************************************************************
C     TIME SERIES ALONG NODES (TSAN) DATA OUTPUT
C     LAST CHANGE 2013 08 12 BY THOMAS.REIMANN@TU-DRESDEN.DE
C     2014 02 20 ADDED CADS RECHARGE      
C***********************************************************************
C
      USE CFPMODULE
      USE GWFBASMODULE, ONLY:TOTIM
      USE GLOBAL, ONLY:IOUT, HNEW
C
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES                              
      CHARACTER(LEN=16) FNAME(6)
      CHARACTER(LEN=4) F
      INTEGER I, N, NN, IC, IR, IL, FMT, X
      DOUBLE PRECISION TSANOUT(6, NTSAN)
C
C--SET UP OUTPUT FILE FOR THE FIRST TIME
      FNAME(1)='TSAN_HCON.out'
      FNAME(2)='TSAN_HMAT.out'
      FNAME(3)='TSAN_QMAT.out'      
      FNAME(4)='TSAN_QCDS.out'
      FNAME(5)='TSAN_QDIR.out'    
      FNAME(6)='TSAN_QSDR.out'          
      DO I = 1, 6
        WRITE (F, '(I4.4)') 9000 + I   
        IF (TSAN_FUNIT(I).EQ.-1) THEN
          TSAN_FUNIT(I) = 550 + I
          OPEN (TSAN_FUNIT(I), FILE=FNAME(I), STATUS='UNKNOWN', ERR=100)
          WRITE (IOUT,*)'TIME SERIES ALONG NODES (TSAN) DATA OUTPUT IN F
     +ILE ',FNAME(I)
          IF(I.EQ.1) WRITE (TSAN_FUNIT(I),9001) (TSAN(X), X=1,NTSAN)  
          IF(I.EQ.2) WRITE (TSAN_FUNIT(I),9002) (TSAN(X), X=1,NTSAN)  
          IF(I.EQ.3) WRITE (TSAN_FUNIT(I),9003) (TSAN(X), X=1,NTSAN)  
          IF(I.EQ.4) WRITE (TSAN_FUNIT(I),9004) (TSAN(X), X=1,NTSAN)  
          IF(I.EQ.5) WRITE (TSAN_FUNIT(I),9005) (TSAN(X), X=1,NTSAN)
          IF(I.EQ.6) WRITE (TSAN_FUNIT(I),9006) (TSAN(X), X=1,NTSAN)
        ENDIF 
      ENDDO
C
C--COMPILE OUTPUT DATA
      DO I = 1, 6
        DO N = 1, NTSAN
          DO NN = 1, MXNODE
            IF (I.EQ.1.AND.TSAN(N).EQ.NN) TSANOUT(I,N)=B_MAT(NN)
            IF (I.EQ.2.AND.TSAN(N).EQ.NN) THEN
              IC = NBR(NN, 2)
              IR = NBR(NN, 3)
              IL = NBR(NN, 4)
              TSANOUT(I,N)=HNEW(IC, IR, IL)
            ENDIF
            IF (I.EQ.3.AND.TSAN(N).EQ.NN) TSANOUT(I,N)=-QMAT(NN)
            IF (I.EQ.4.AND.TSAN(N).EQ.NN) TSANOUT(I,N)=-CADSFLOW(NN)
            IF (I.EQ.5.AND.TSAN(N).EQ.NN) TSANOUT(I,N)=QBDIR(NN)  
            IF (I.EQ.6.AND.TSAN(N).EQ.NN) TSANOUT(I,N)=QSDIR(NN)  
          ENDDO
        ENDDO
      ENDDO      
C
C--WRITE TSAN DATA
      DO I = 1, 6
        WRITE (TSAN_FUNIT(I), 9011) TOTIM,(TSANOUT(I,N), N = 1, NTSAN)
      ENDDO
C      
      RETURN      
C      
 100  WRITE (IOUT, *)'ERROR WHILE OPENING UNIT',TSAN_FUNIT,'(TSAN_OUT)!'
 9001 FORMAT ('     TOTIM  HCON / NODE NUMBERS:',*(I4,7X))
 9002 FORMAT ('     TOTIM  HMAT / NODE NUMBERS:',*(I4,7X))   
 9003 FORMAT ('     TOTIM  QMAT / NODE NUMBERS:',*(I4,7X))   
 9004 FORMAT ('     TOTIM  QCDS / NODE NUMBERS:',*(I4,7X))   
 9005 FORMAT ('     TOTIM  QDIR / NODE NUMBERS:',*(I4,7X))     
 9006 FORMAT ('     TOTIM QSDIR / NODE NUMBERS:',*(I4,7X))      
 9011 FORMAT (F10.0,22x,1000(G10.4,x))          
 
C
      END SUBROUTINE TSAN_OUT      
C
C
C
C
C
      SUBROUTINE TSAT_OUT
C***********************************************************************
C     TIME SERIES ALONG TUBES (TSAT) DATA OUTPUT
C     LAST CHANGE 2013 08 12 BY THOMAS.REIMANN@TU-DRESDEN.DE      
C***********************************************************************
C
      USE CFPMODULE
      USE GWFBASMODULE, ONLY:TOTIM
      USE GLOBAL, ONLY:IOUT
C
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES                              
      CHARACTER(LEN=16) FNAME(3)
      CHARACTER(LEN=4) F
      INTEGER I, N, NT, FMT, X
      DOUBLE PRECISION TSATOUT(3, NTSAT)
C
C--SET UP OUTPUT FILE FOR THE FIRST TIME
      FNAME(1)='TSAT_FLOW.out'
      FNAME(2)='TSAT_REYN.out'
      FNAME(3)='TSAT_TIME.out'          
      DO I = 1, 3
        WRITE (F, '(I4.4)') 9000 + I   
        IF (TSAT_FUNIT(I).EQ.-1) THEN
          TSAT_FUNIT(I) = 650 + I
          OPEN (TSAT_FUNIT(I), FILE=FNAME(I), STATUS='UNKNOWN', ERR=100)
          WRITE (IOUT,*)'TIME SERIES ALONG TUBES (TSAT) DATA OUTPUT IN F
     +ILE ',FNAME(I)
          IF(I.EQ.1) WRITE (TSAT_FUNIT(I),9001) (TSAT(X), X=1,NTSAT)  
          IF(I.EQ.2) WRITE (TSAT_FUNIT(I),9002) (TSAT(X), X=1,NTSAT)  
          IF(I.EQ.3) WRITE (TSAT_FUNIT(I),9003) (TSAT(X), X=1,NTSAT)  
        ENDIF 
      ENDDO
C
C--COMPILE OUTPUT DATA
      DO I = 1, 3
        DO N = 1, NTSAT
          DO NT = 1, MXTUBE
            IF (I.EQ.1.AND.TSAT(N).EQ.NT) TSATOUT(I,N)= DTUBE(NT,1)
            IF (I.EQ.2.AND.TSAT(N).EQ.NT) TSATOUT(I,N)= TUB_REYNOLD(NT)
            IF (I.EQ.3.AND.TSAT(N).EQ.NT) TSATOUT(I,N)= TAUI(NT)      
          ENDDO
        ENDDO
      ENDDO      
C
C--WRITE TSAT DATA
      DO I = 1, 3
        WRITE (TSAT_FUNIT(I), 9011) TOTIM,(TSATOUT(I,N), N = 1, NTSAT)
      ENDDO
C      
      RETURN      
C      
 100  WRITE (IOUT, *)'ERROR WHILE OPENING UNIT',TSAT_FUNIT,'(TSAT_OUT)!'
 9001 FORMAT ('     TOTIM  FLOW / TUBE NUMBERS:',*(I4,7X))
 9002 FORMAT ('     TOTIM  RE   / TUBE NUMBERS:',*(I4,7X))   
 9003 FORMAT ('     TOTIM  RESID.TIME/ TUBE NO:',*(I4,7X))
 9011 FORMAT (F10.0,22x,1000(G10.4,x))          
 
C
      END SUBROUTINE TSAT_OUT      
C
C
C
C
C      
C      
C
C--LIST OF VARIABLES
C
C     HCON                  HEAD IN THE CONDUIT SYSTEM, DIMENSION(MXNODE)
C     KTSNO                 COUNTER FOR TIME SERIES OUTPUT OF NODE DATA
C     KTSTU                 COUNTER FOR TIME SERIES OUTPUT OF TUBE DATA
C     MRESNO                NUMBER OF PARAMETERS WRITTEN IN ARRAY RESNOD
C     MRESTU                NUMBER OF PARAMETERS WRITTEN IN ARRAY RESTUB
C     NAMNOD                NAMES OF PARAMETERS WRITTEN IN ARRAY RESNOD,
C                           DIMENSION(MRESNO)
C     NAMTUB                NAMES OF PARAMETERS WRITTEN IN ARRAY RESTUB,
C                           DIMENSION(MRESTU)
C     NOTSNO                FREQUENCY OF TIME SERIES OUTPUT OF NODE DATA
C     NOTSTU                FREQUENCY OF TIME SERIES OUTPUT OF TUBE DATA
C     QDIR                  FLOW FROM DIRECT RECHARGE INTO NODE, DIMENSION(MXNODE)
C     QFIX                  FLOW FROM FIXED HEAD INTO NODE, DIMENSION(MXNODE)
C     QMAT                  FLOW FROM NODE INTO FISSURED SYSTEM, DIMENSION(MXNODE)
C     QTUB                  FLOW FROM TUBES INTO NODE, DIMENSION(MXNODE,NNOD)
C     QTUBIN                SUM OF INFLOW FROM TUBES INTO NODE, DIMENSION(MXNODE)
C     QTUBOUT               SUM OF OUTFLOW FROM NODES INTO TUBES, DIMENSION(MXNODE)
C     RESNOD                ARRAY CONTAINING RESULTS OF CHEMISTRY AND TRANSPORT
C                           CALCULATION FOR THE NODES, DIMENSION(MXNODE,MRESNO)
C     RESTUB                ARRAY CONTAINING RESULTS OF CHEMISTRY AND TRANSPORT
C                           CALCULATION FOR THE TUBES, DIMENSION(MXTUBE,MRESTU)
C     TOTIM                 TOTAL TIME OF SIMULATION IN SEC
C     TSNODE                ARRAY DETERMING IF TSO IS WRITTEN FOR A NODE (NOT 0)
C                           OR NOT (0), DIMENSION(MXNODE)
C     TSTUBE                ARRAY DETERMING IF TSO IS WRITTEN FOR A TUBE (NOT 0)
C                           OR NOT (0), DIMENSION(MXTUBE)
C     TUBREY                REYNOLDS NUMBER OF TUBE, DIMENSION(MXTUBE)      
C     MXNODE                NUMBER OF NODES
C     KU_QBDIR              CUMULATIVE FLOW RATE DIRECTLY IN CONDUIT
C     BUD_SOURCE            FLOW OF SOURCE
C     BUD_SINK              FLOW OF SINK = FLOW OUT OF THE CONDUIT SYSTEM
C     IOUT                  UNIT NUMBER FOR OUTPUT TO LISTING FILE
C     BUD_MATIN             TOTAL FLOW IN CONDUIT SYSTEM COMING FROM MATRIX
C     BUD_MATOU             TOTAL FLOW IN MATRIX
C     BUD_QBDIR             TOTAL DIRECT INFLOW IN CONDUIT SYSTEM
C     VOL_CONDUIT_BEGIN     VOLUME OF THE CONDUIT SYSTEM BEFOR DISSOLUTION
C     VOL_CONDUIT_END       VOLUME OF THE CONDUIT SYSTEM AFTER DISSOLUTION
C     BUDGET_FLAG           OUTPUT FLAG OF BUDGET   
C     EPSILON               CRITERION OF CONVERGENCE
C     MXTUBE                NUMBER OF TUBES
C     TURB                  CONTAINS THE TYPE OF FLOW
C                           0 - LAMINAR
C                           1 - TURBULENT
C     BEG_HEAD              CONTAINING THE HEADS READ FROM FILE
C     NEIBR                 ARRAY CONTAINING THE CONDUITSTRUCTUR:
C                           NODE, NEIGHBOUR NODE  1, 2, 3, 4, TUBE NR. 1, 2, 3, 4,
C                           MODFLOWROW, -COLUMN
C     CON_DATA              CONDUIT DATA WITH:
C                           TUBENUMBER, DIAMETER, LENGTH, ROUGHNESS
C     MOD                   EXCHANGE COEFFICIENT USED FOR EXCHANGE WITH MODFLOW
C     X0                    CALCULATED HEADPRESSURES
C     KONV                  FLAG: 1 - ITERATION CONVERGE
C                                 0 - NO CONVERGENCE
C     ITERA_MAX             MAXIMUM OF ITERATIONSTEPS TO SOLVE THE EQUATIONSYSTEM
C     S                     NUMBER OF ITERATIONS FOR CALCULATION OF HEADS
C     TUBE                  TUBENR., NODE_BEGIN, NODE_END
C                           Q M^3/S (BEGIN_NODE_PRESSUR > END_NODE_PRESSUR => Q >= 0),
C                           CHEMIE: 0 NOT CALCULATED, 1 CALCULATED
C     LSURFACEDEP           FLAG:  0 - CONSTANT EXCHANGE
C                                  1 - SURFACE DEPENDENT EXCHANGE
C     LOUTSTEP              INTERVAL OF OUTPUT STEP
C
C     REP_CYCLE     NUMBER OF REP_CYCLE FOR THIS OUTPUT
C     GEOTIMES      DURATION OF THE 'GEOMETRIC TIMESTEP' IN SECOND
C               OR A FLAG (= 0 THEN SET EQUAL DURATION OF TIMESTEP)
C     IFOUT         UNIT NUMBER FOR FLOW OUTPUT
C     KPER          CONTAINS THE NUMBER OF STRESSPERIOD FOR THE ACTUAL OUTPUT
C     KSTP          CONTAINS THE NUMBER OF TIMESTEP FOT THE ACTUAL OUTPUT
C     HNEW          MODFLOW NEW CALCULATED PRESSURE HEAD
C     NCOL          IS THE NUMBER OF COLUMNS
C     NROW          IS THE NUMBER OF ROWS
C     NLAY          IS THE NUMBER OF LAYS
C     MAT_DIR       STORE EXCHANGE COMING IN (> 0) OR OUT (< 0)
C               OF THE MATRIX   