C
C
C
C
C
      SUBROUTINE CFP1PC
C     ******************************************************
C     PROCESS AND CHECK CONDUIT DATA AND GEOMETRY
C     SEBA--SUBROUTINE FOR DETERMINATION OF TUBE/NODE GEOMETRY:
C     CALLS LENGTH_CALC AND DETERM TUBE FROM CON2RP
C     ******************************************************
C     14DEZ1998
C
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IOUT, BOTM
      USE CFPMODULE, ONLY:TOP
      IMPLICIT NONE
      EXTERNAL DETERM_TUB, GEOCALC, LENGTH_CAL
      EXTERNAL ROUGH_CAL, TEST_CONDUIT_NETWORK
C
C--LOCAL VARIABLES
      INTEGER I, J, K
C     ------------------------------------------------------------------
      WRITE (IOUT, *) 'PROCESSING AND CHECKING CONDUIT-NETWORK'
C 
C--BARC**SET TOP
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            TOP(J, I, K) = BOTM(J, I, K-1)
          ENDDO
        ENDDO
      ENDDO
C
C--CHECK CONDUIT NETWORK FOR ERRORS IN INPUT FILE
      CALL TEST_CONDUIT_NETWORK
C 
C--ASSIGN TUBENR. WITH NODENUMBERS
      CALL DETERM_TUB
C
C--DETERMINE GEOMETRY OF THE CONDUIT NETWORK
      CALL GEOCALC
C
C--CALCULATE REAL TUBE LENGTH
      CALL LENGTH_CAL
C 
C--CALCULATE ROUGHNESS LENGTH OF TUBE WALLS:
      CALL ROUGH_CAL
C 
      END SUBROUTINE CFP1PC
C
C
C
C
C
      SUBROUTINE TEST_CONDUIT_NETWORK
C     *****************************************************************
C     C--SEBATEST_CONDUIT_NETWORK - TESTING OF TUBES BEFORE CALCULATION
C     *****************************************************************
C     VERSION 1      9/1998
C
      USE CONSTANTS, ONLY: Z,NEARZERO_30
      USE CFPMODULE, ONLY:NNOD, MXNODE, MXTUBE, NBR, BEGHEAD
      USE GLOBAL, ONLY:IOUT, IBOUND
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES
      INTEGER TUBE1(MXTUBE, 5), TUBE2(MXTUBE, 5)
      INTEGER NODE_COUNT(MXNODE), TUBE_COUNT(MXTUBE)
      INTEGER NBR_NODES(MXNODE, NNOD), NBR_TUBES(MXNODE, NNOD)
      INTEGER I_NODE, NODE_HELP, TUBE_HELP, J, K, L1, L2
      INTEGER TEST, TEST_T, HELP_NODE, TEST_H, TN, TT
      INTEGER COUNTER, I, TEST_TUBE, NEBRNODE, ERROR, WARNING
C     ------------------------------------------------------------------
      ERROR = Z
      WARNING = Z
      WRITE (IOUT, *) 'TESTING THE CONDUIT NETWORK:'
C
      DO I_NODE = 1, MXNODE
C
C--READ NEIGHBOUR NODES OF NODE I_NODE IN NBR_NODES(,)
        DO J = 1, NNOD
          NBR_NODES(I_NODE, J) = NBR(I_NODE, (J+4))
          NBR_TUBES(I_NODE, J) = NBR(I_NODE, (J+4+NNOD))
        ENDDO
      ENDDO
C
C--TEST, IF NODENUMBER OR TUBENUIMBER IS HIGHER THAN MXNODE OR MXTUBE:
      DO I_NODE = 1, MXNODE
        DO J = 1, NNOD
          IF ( NBR_NODES(I_NODE,J).GT.MXNODE ) THEN
            WRITE (IOUT, *) ' NODE WITH NODENUMEBR HIGHER THAN',        
     +                      ' MXNODE NEIGHBOUR OF NODE ', I_NODE
            STOP
          ENDIF
          IF ( NBR_TUBES(I_NODE,J).GT.MXTUBE ) THEN
            WRITE (IOUT, *) ' TUBE WITH TUBENUMBER HIGHER THAN',        
     +                      ' MXTUBE AT NODE ', I_NODE
            STOP
          ENDIF
        ENDDO
      ENDDO
C 
C--TEST, IF NODE NUMBER APPEARS MORE THAN ONCE OR NOT AT ALL:
      DO I = 1, MXNODE
        NODE_COUNT(I) = Z
      ENDDO
      DO I_NODE = 1, MXNODE
        NODE_COUNT(NBR(I_NODE,1)) = NODE_COUNT(NBR(I_NODE,1)) + 1
      ENDDO
      DO I_NODE = 1, MXNODE
        IF ( NODE_COUNT(I_NODE).EQ.Z ) THEN
          WRITE (IOUT, *) ' NO NODE NR. ', I_NODE
          ERROR = ERROR + 1
        ENDIF
        IF ( NODE_COUNT(I_NODE).GT.1 ) THEN
          WRITE (IOUT, *) ' MORE THAN ONE NODE NR.', I_NODE
          ERROR = ERROR + 1
        ENDIF
      ENDDO
C
C--TEST, IF TUBE NUMBER APPEARS MORE THAN ONCE:
      DO I = 1, MXTUBE
        TUBE_COUNT(I) = Z
      ENDDO
      DO I = 1, MXNODE
        DO J = 1, NNOD
          IF ( NBR(I,J+4+NNOD).NE.Z ) TUBE_COUNT(NBR(I,(J+4+NNOD)))     
     +         = TUBE_COUNT(NBR(I,(J+4+NNOD))) + 1
        ENDDO
      ENDDO
      DO I = 1, MXTUBE
        IF ( TUBE_COUNT(I).NE.2 ) THEN
          WRITE (IOUT, *) ' TUBE NR: ', I, ' APPEARS ', TUBE_COUNT(I),  
     +                    ' TIMES'
          ERROR = ERROR + 1
        ENDIF
      ENDDO
C
C--CHECK, IF EVERY NODE HAS A CONNECTING TUBE:
      DO I = 1, MXNODE
        DO J = 1, NNOD
          IF ( (NBR(I,J+4).NE.Z) .AND. (NBR(I,J+4+NNOD).EQ.Z) )         
     +         WRITE (IOUT, 9001) NBR(I, J+4), J, I
          IF ( (NBR(I,J+4).EQ.Z) .AND. (NBR(I,J+4+NNOD).NE.Z) )         
     +         WRITE (IOUT, 9002) NBR(I, J+4+NNOD), J, I
        ENDDO
      ENDDO
C
C--CHECK FOR CORRECT CONNECTION OF NODES:
      DO I_NODE = 1, MXNODE
C 
C--CHECK IF NEIGHBOUR OF I_NODE HAS I_NODE AS NEIGHBOUR
        DO J = 1, NNOD
          NODE_HELP = NBR_NODES(I_NODE, J)
C         WRITE (IOUT, *) 'NODE_HELP: ', NODE_HELP
          IF ( NODE_HELP.NE.Z ) THEN
            TEST = Z
C--&&6
            DO K = 1, NNOD
              IF ( NBR(NODE_HELP,K+4).EQ.I_NODE ) TEST = 1
C             WRITE (IOUT, *) NODE_HELP, ' ', K, ' ',NBR(NODE_HELP,K+4),
C    +                        ' TEST: ', TEST
            ENDDO
            IF ( TEST.EQ.Z ) WRITE (IOUT, *) ' NODE ', NODE_HELP,       
     +                              ' MISSING NEIGHBOUR NODE ', I_NODE
          ENDIF
        ENDDO
C
C--CHECK, IF ITUBE AT I_NODE IS ITUBE AT ONE NEIGHBOUR OF I_NODE AS WELL
C  J COUNTS TUBES AT NODE I_NODE, L1 COUNTS NEIGHBOURS OF NODE I_NODE,
C  L2 COUNTS TUBES AT NODE L1
        DO J = 1, NNOD
          TUBE_HELP = NBR_TUBES(I_NODE, J)
          IF ( TUBE_HELP.NE.Z ) THEN
            TEST_T = Z
            DO L1 = 1, NNOD
              HELP_NODE = NBR_NODES(I_NODE, L1)
              IF ( HELP_NODE.NE.Z ) THEN
                DO L2 = 1, NNOD
                  IF ( NBR_TUBES(HELP_NODE,L2).EQ.TUBE_HELP ) TEST_T = 1
                ENDDO
              ENDIF
            ENDDO
            IF ( TEST_T.EQ.Z ) THEN
              WRITE (IOUT, *) ' TUBE ', TUBE_HELP, ' AT NODE ',         
     +                        I_NODE, ' MISSING SECOND NEIGHBOUR NODE'
              ERROR = ERROR + 1
            ENDIF
          ENDIF
        ENDDO
C
C--IF NODE I_NODE IS CONST. HEAD, CHECK IF CORRESPONDING MODFLOW CELL IS
C  CONSTANT HEAD AS WELL.
        TEST_H = IBOUND(NBR(I_NODE,2), NBR(I_NODE,3), NBR(I_NODE,4))
        IF ( TEST_H.EQ.Z ) THEN
          WRITE (IOUT, *) ' NODE', I_NODE, ' IN INACTIVE MODFLOW CELL'
          WARNING = WARNING + 1
        ENDIF
!RSR    IF ( BEGHEAD(I_NODE).NE.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(I_NODE)+1.D0).GT.NEARZERO_30 ) THEN
          IF ( TEST_H.EQ.1 ) THEN
            WRITE (IOUT, 9003) I_NODE
            WARNING = WARNING + 1
          ENDIF
          IF ( TEST_H.EQ.Z ) THEN
            WRITE (IOUT, 9004) I_NODE
            WARNING = WARNING + 1
          ENDIF
        ENDIF
      ENDDO
C
C--SEBA--CHECK, IF TUBE NUMBERS ARE ASSIGNED CORRECTLY. DOUBLE CHECK FOR
C  CONNECTING TUBES. FOR EACH NODE, TAKE NEIGHBOUR NODE, GET
C  CONNECTING TUBE, AND CHECK IF THE NEIGHBOUR NODE IS CONNECTED TO
C  THE ACTUAL NODE BY THE SAME TUBE:
      DO I = 1, MXNODE
        DO J = 1, NNOD
          IF ( NBR(I,J+4).NE.Z ) THEN
C
C--GET TESTNODE AND TESTTUBE
            TN = NBR(I, J+4)
            TT = NBR(I, J+4+NNOD)
C
C--CROSS CHECK
            DO K = 1, NNOD
              IF ( NBR(TN,K+4).EQ.I ) THEN
                IF ( NBR(TN,K+4+NNOD).NE.TT ) THEN
                  WRITE (*, 9005) I, TN, TT, TN, I, NBR(TN, K+4+NNOD)
                  ERROR = ERROR + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C--SEBA--CHECK, WETHER TUBES ARE IN THE RIGHT COLUMN:
C  THIS IS, WHAT IS HAPPENING IN DETERM_TUB(..) BY T.CLEMENS
C  DETERMINE TUBE1(..)
      COUNTER = Z
C INITIALISATION OF TUBE (= Z)
      DO I = 1, MXTUBE
        DO J = 1, 5
          TUBE1(I, J) = Z
          TUBE2(I, J) = Z
        ENDDO
      ENDDO
C
C--BEGIN DETERMINATION LOOP
      DO I = 1, MXNODE
        DO J = 1, NNOD
C
C--EXCLUDE NEBR()=0
          IF ( NBR(I,J+4+NNOD).NE.Z ) THEN
C
C--IF THERE IS NO ENTRY IN TUBE THEN ASSIGN
            IF ( TUBE1(NBR(I,J+4+NNOD),1).EQ.Z ) THEN
C
C--THERE IS TUBE
              IF ( NBR(I,J+4+NNOD).NE.Z ) THEN
                TUBE1(NBR(I,J+4+NNOD), 1) = NBR(I, J+4+NNOD)
                TUBE1(NBR(I,J+4+NNOD), 2) = I
                TUBE1(NBR(I,J+4+NNOD), 3) = NBR(I, J+4)
C
C--INCREASE NUMBER OF TUBE
                COUNTER = COUNTER + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C--CHECK THIS:
C  SEARCH FOR TUBES BEGINS:
C  DETERMINE TUBE2(..)
      DO I_NODE = 1, MXNODE
C
C--READ NEIGHBOUR NODES OF NODE I_NODE IN NBR_NODES(,)
        DO J = 1, NNOD
          NBR_NODES(I_NODE, J) = NBR(I_NODE, (J+4))
          NBR_TUBES(I_NODE, J) = NBR(I_NODE, (J+4+NNOD))
        ENDDO
      ENDDO
      DO I_NODE = 1, MXNODE
C
C--CHECK IF NEIGHBOUR OF I_NODE HAS I_NODE AS NEIGHBOUR
        DO J = 1, NNOD
          NODE_HELP = NBR_NODES(I_NODE, J)
          IF ( NODE_HELP.NE.Z ) THEN
            TEST = Z
            DO K = 1, NNOD
              IF ( NBR(NODE_HELP,K+4).EQ.I_NODE ) THEN
C--FOUND DOS NEIGHBOURING NODES, SEARCH FOR CONNECTING TUBE:
                NEBRNODE = NODE_HELP
                TEST = TEST + 1
C               WRITE (IOUT, *) 'I: ', I_NODE, ' NEBR ', NEBRNODE,      
C    +                          ' TEST: ', TEST
                TEST_TUBE = Z
                DO L1 = 1, NNOD
                  TUBE_HELP = NBR_TUBES(I_NODE, L1)
                  IF ( TUBE_HELP.NE.Z ) THEN
                    DO L2 = 1, NNOD
C--FOUND CONNECTING TUBE:
C                     WRITE (IOUT, *) 'TUBE: ', TEST_TUBE
                      IF ( TUBE_HELP.EQ.NBR_TUBES(NEBRNODE,L2) )        
     +                     TEST_TUBE = TUBE_HELP
                    ENDDO
                  ENDIF
                ENDDO
                IF ( TEST_TUBE.NE.Z ) THEN
C
C--WRITE TUBE_HELP-LINE OF TUBE2:
                  TUBE2(TEST_TUBE, 1) = TEST_TUBE
                  TUBE2(TEST_TUBE, 2) = NEBRNODE
                  TUBE2(TEST_TUBE, 3) = I_NODE
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C--COMPARE TUBE1 AND TUBE2:
      DO I = 1, MXTUBE
        DO J = 1, 5
          IF ( TUBE1(I,J).NE.TUBE2(I,J) ) THEN
            WRITE (IOUT, *) ' MISTAKE AT TUBE: ', TUBE1(I, J)
            ERROR = ERROR + 1
          ENDIF
        ENDDO
      ENDDO
C
      IF ( ERROR.EQ.Z ) THEN
        WRITE (IOUT, *) ' NO ERRORS DETECTED           `:) '
      ELSE
        WRITE (IOUT, *) ERROR, ' ERROR(S) DETECTED   :('
        WRITE (IOUT, *) ' CHECK CONDUIT-GEOMETRY INPUT'
        STOP
      ENDIF
      IF ( WARNING.EQ.Z ) THEN
        WRITE (IOUT, *) ' NO WARNINGS             `:)'
      ELSE
        WRITE (IOUT, *) WARNING, ' WARNING(S) DETECTED `:|',            
     +                  ' CHECK CONDUIT-GEOMETRY INPUT '
      ENDIF
C
 9001 FORMAT (4X, 'NODE', I5, ' (NEIBR. NODE NR.', I5, ') IN LINE', I5, 
     +        ' HAS NO CONNECTING TUBE')
 9002 FORMAT (4X, 'TUBE', I5, ' (NEIBR-TUBE NR', I5, ') IN LINE', I5,   
     +        ' HAS NO NODE')
 9003 FORMAT (4X, 'CONSTANT HEAD NODE', I5,                             
     +        ' OF CONDUIT NETWORK IS NOT CONSTANT-HEAD MODFLOW CELL')
 9004 FORMAT (4X, 'CONSTANT HEAD NODE', I5,                             
     +        ' OF CONDUIT NETWORK IN INACTIVE MODFLOW CELL')
 9005 FORMAT (4X, 'NODE', I5, ' CONNECTED TO NODE', I5, ' BY TUBE', I5, 
     +        ' BUT NODE', I5, ' CONNECTED TO NODE', I5, ' BY TUBE', I5)
C
      END SUBROUTINE TEST_CONDUIT_NETWORK
C
C
C
C
C
      SUBROUTINE DETERM_TUB
C     *****************************************************************
C     ASSIGN TUBE (PIPE) NUMBERS WITH NODE NUMBERS
C     WITH SUPPORT OF NBR
C     *****************************************************************
C     VERSION 1 12APR1995 DETERM_TUB
C
      USE CONSTANTS, ONLY: Z,DZ
      USE GLOBAL, ONLY:IOUT
      USE CFPMODULE, ONLY:NNOD, MXNODE, MXTUBE, NBR, ITUBE, DTUBE
      IMPLICIT NONE
C
C ------ OUTPUT
C ITUBE       MATRIX WITH TUBENR., NODE_BEGIN, NODE_END, WETTING FLAG 
C DTUBE       MATRIX WITH Q (M/S) (BEGIN_NODE_PRESSUR > END_NODE_PRESSUR => Q >= 0),
C             CHEMISTRY FLAG: 0 NOT CALCULATED, 1 CALCULATED
C
C--LOCAL VARIABLES
      INTEGER COUNTER, I, J
C
      WRITE (IOUT, *) 'BEGINNING CFPM1 SUBROUTINE DETERM_TUB'
      COUNTER = Z
C
C--INITIALISATION OF TUBE (= 0)
      DO I = 1, MXTUBE
        DO J = 1, 4                                                     !TR: 2012 01 26 INI UP TO J = 4 (MATRIX EXTEND TO CONSIDER WET FLG)
          ITUBE(I, J) = Z
        ENDDO
        DO J = 1, 3                                                     !TR: 2014 08 01 PFPS // DTUBE(*,3) = DEQV
          DTUBE(I, J) = DZ
        ENDDO
      ENDDO
C 
C--BEGIN DETERMINATION LOOP
      DO I = 1, MXNODE
        DO J = 1, NNOD
C
C--IF THERE IS NO ENTRY IN TUBE THEN ASSIGN
C  SEBA--EXCLUDE CASE NEIBR(,)=0
          IF ( NBR(I,J+4+NNOD).NE.Z ) THEN
            IF ( ITUBE(NBR(I,J+4+NNOD),1).EQ.Z ) THEN
C
C--THERE IS TUBE
              IF ( NBR(I,J+4+NNOD).NE.Z ) THEN
                ITUBE(NBR(I,J+4+NNOD), 1) = NBR(I, J+4+NNOD)
                ITUBE(NBR(I,J+4+NNOD), 2) = I
                ITUBE(NBR(I,J+4+NNOD), 3) = NBR(I, J+4)
C
C--INCREASE NUMBER OF TUBE
                COUNTER = COUNTER + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C 
C--VERIFY DETERMINATION OF TUBES
      IF ( COUNTER.NE.MXTUBE ) WRITE (*, *)                             
     +                              '    MISTAKE BY TUBE DETERMINATION!'
C     
      END SUBROUTINE DETERM_TUB
C
C
C
C
C 
      SUBROUTINE GEOCALC
C
C  VERSION 1  13.12.1998
C*****************************************************************
C     3D - GEOMETRY  DETERMINED
C*****************************************************************
C
      USE CONSTANTS, ONLY: DZ, DOS, Z
      USE GLOBAL, ONLY:BOTM, IOUT, DELR, DELC, NLAY
      USE CFPMODULE, ONLY:NNOD, MXNODE, MXTUBE, GEOHIGHT, NODE_LOCA,    
     +                    NODETOP, NODEBOT, CON_DATA, TOP, NBR
      IMPLICIT NONE
C
C--LOCAL VARIABLES
      INTEGER I, J, TOPFLAG, ERRORS, ITEST
      INTEGER I_NODE, NK_COL, NK_ROW, NK_LAY
      DOUBLE PRECISION XCOL, XROW, XLAY
C
C--BARC**FOR COMPUTING AVERAGE HEIGHT OF NODETOP AND NODEBOT
      DOUBLE PRECISION NODETOPAVG, NODEBOTAVG, AVGCOUNT
C------------------------------------------------------------------
C
C--GEOMETRY OF NODES AND TUBES ARE DETERMINED
      WRITE (IOUT, *) 'BEGINNING CFPM1 SUBROUTINE GEOCALC THAT',        
     +                ' CALCULATES XYZ COORDINATES OF NODES'
C
      WRITE (IOUT, *) 'TOP-ARRAY / BOT-ARRAY:'
      DO I = 1, NLAY
        WRITE (IOUT, *) TOP(1, 1, I), '    ', BOTM(1, 1, I)
      ENDDO
C 
      TOPFLAG = Z
      ERRORS = Z
C
C--WRITE HEADER FOR NODE HEIGHTS:
      WRITE (IOUT, *) ' NODE   COLUMN    ROW    LAY      X         Y   '
     +                , '         Z      '
C 
C--FOR EVERY NODE
      DO I_NODE = 1, MXNODE
C
C--GET MODFLOW CELL COORDINATES
        NK_COL = NBR(I_NODE, 2)
        NK_ROW = NBR(I_NODE, 3)
        NK_LAY = NBR(I_NODE, 4)
        XCOL = DZ
        XROW = DZ
        XLAY = DZ
C 
        DO J = 1, NK_COL
C
C--SEBA--ACHTUNG: DELC(NROW) UND DELR(NCOL), ANDERS ALS GEDACHT:
          IF ( J.EQ.1 ) THEN
            XCOL = DELR(1)/DOS
          ELSE
            XCOL = XCOL + (DELR(J)+DELR(J-1))/DOS
          ENDIF
        ENDDO
        DO J = 1, NK_ROW
          IF ( J.EQ.1 ) THEN
            XROW = DELC(1)/DOS
          ELSE
            XROW = XROW + (DELC(J)+DELC(J-1))/DOS
          ENDIF
        ENDDO
C
C--DETERMINE GEOMETRICAL HIGHT OF CONDUIT NODES BY USING GEOHIGHT FROM
C  CONDUIT GEOMETRY INPUT FILE
        XLAY = GEOHIGHT(I_NODE)
C
C--SAVE GEOMETRY DATA IN NODE_LOCA
        NODE_LOCA(I_NODE, 1) = I_NODE
        NODE_LOCA(I_NODE, 2) = XCOL
        NODE_LOCA(I_NODE, 3) = XROW
        NODE_LOCA(I_NODE, 4) = XLAY
        WRITE (IOUT, 9001) I_NODE, NK_COL, NK_ROW, NK_LAY, XCOL, XROW,  
     +                     XLAY
C 
        IF ( NODE_LOCA(I_NODE,4).LT.BOTM(NK_COL,NK_ROW,NK_LAY) ) THEN
          ERRORS = ERRORS + 1
          WRITE (*, 9002) I_NODE, NODE_LOCA(I_NODE, 4),                 
     +                    BOTM(NK_COL, NK_ROW, NK_LAY)
        ENDIF
C
C--BARC***CHECK TO SEE IF GEOHIGHT OF CONDUIT IS ABOVE TOP OF LAYER 1.
        IF ( GEOHIGHT(I_NODE).GT.TOP(NK_COL,NK_ROW,NK_LAY) ) THEN
          WRITE (IOUT, *) 'ERROR, GEOHIGHT OF NODE ', I_NODE,           
     +                    ' IS GREATER THAN THE ELEVATION OF THE TOP',  
     +                    ' OF THE SURROUNDING MODEL CELL ', NK_COL,    
     +                    NK_ROW, NK_LAY
C 
          PRINT *, 'ERROR, GEOHIGHT OF NODE ', I_NODE,                  
     +          ' IS GREATER THAN THE ELEVATION OF THE TOP OF THE',     
     +          ' SURROUNDING MODEL CELL ', NK_COL, NK_ROW, NK_LAY
          STOP
        ENDIF
C 
      ENDDO
C
      IF ( TOPFLAG.NE.Z ) WRITE (IOUT, *)                               
     +                            'NO TOP-ARRAY, USING LAY_THICK*ALPHA'
C
      IF ( ERRORS.NE.Z ) THEN
        WRITE (IOUT, *) ' ERRORS IN CONDUIT INPUT FILE. PROGRAM STOPS'
        STOP
      ENDIF
C
C--BARC**COMPUTE NODETOP AND NODEBOT USING AVERAGE OF HEIGHTS OF INTERSECTING
C  BARC**TUBE TOPS AND BOTTOMS.
      DO I = 1, MXNODE
        NODETOPAVG = DZ
        NODEBOTAVG = DZ
        AVGCOUNT = DZ
        DO J = 1, NNOD
          ITEST = NBR(I, J+4+NNOD)
          IF ( ITEST.NE.Z ) THEN
            NODETOPAVG = NODETOPAVG + GEOHIGHT(I)                       
     +                   + (CON_DATA(ITEST,2)/DOS)
            NODEBOTAVG = NODEBOTAVG + GEOHIGHT(I)                       
     +                   - (CON_DATA(ITEST,2)/DOS)
            AVGCOUNT = AVGCOUNT + 1
C         PRINT *, NODETOPAVG, NODEBOTAVG, AVGCOUNT
          ENDIF
CBARC**ENDDO FOR NNOD LOOP
        ENDDO
        NODETOP(I) = NODETOPAVG/AVGCOUNT
        NODEBOT(I) = NODEBOTAVG/AVGCOUNT
C       PRINT *, NODETOP(I), NODEBOT(I)
C 
C--BARC**ENDDO FOR MXNODE LOOP
      ENDDO
C 
 9001 FORMAT (4I7, 4X, 3G12.6)
 9002 FORMAT ('!!ERROR. GEOMETRIC HIGHT OF CONDUIT NODE', I5, ' OF',    
     +        G13.6, ' BELOW BOTTOM OF MODFLOW LAYER AT', G13.6)
C 
!     WRITE (IOUT, 9003)
!9003 FORMAT ('   NODE', ' COL   ROW   LAY', 4X, 'X', 13X, 'Y', 11X,    
!    +        'HEIGHT')
!9004 FORMAT (4I6, 3G14.6)
C 
      END SUBROUTINE GEOCALC
C
C
C
C
C
      SUBROUTINE LENGTH_CAL
C     *******************************************************************
C     CALCULATE TUBE LENGTH WITH X-, Y-, AND Z-VALUE  &&3D1
C     *******************************************************************
C     VERSION 1  22AUGUST1995  LENGTH_CAL
C
      USE CONSTANTS, ONLY: NEARZERO_30, DOS, DZ
      USE CFPMODULE, ONLY:CON_DATA, MXTUBE, TORTUOS, NBR, NODE_LOCA,    
     +    ITUBE
      USE GLOBAL, ONLY:DELR, DELC, IOUT
C
      IMPLICIT NONE
C ------ INPUT:
C TUBE          TUBENR., NODE_BEGIN, NODE_END
C               Q M/S (BEGIN_NODE_PRESSUR > END_NODE_PRESSUR => Q >= 0),
C               CHEMIE: 0 NOT CALCULATED, 1 CALCULATED
C NEIBR         NODENR., NEIGHBOURNODE  1, 2, 3, 4, TUBENR. 1, 2, 3, 4,
C               MODFLOWROW, -COLUMN
C               0 - THERE IS NO LINK TO THE NODE
C TORTUOS       CALCULATE A VALUE FOR THE TORTUOSITY OF EVERY TUBE
C ------ OUTPUT:
C CON_DATA      CONDUIT DATA WITH:
C               TUBENUMBER, DIAMETER, LENGTH, ROUGHNESS
C NBR_LAY       NUMBER OF LAYERS
C LAY_THICK     THICKNESS OF LAYERS
C
C
      INTRINSIC DSQRT
C
C--LOCAL VARIABLES
      INTEGER I, J, X1, X2, Y1, Y2
!     INTEGER Z1, Z2
      DOUBLE PRECISION LENGTH, X, Y, Z, HEIGHT1, HEIGHT2
C------------------------------------------------------------------
      WRITE (IOUT, *)
      WRITE (IOUT, *) 'BEGINNING CFPM1 LENGTH_CAL SUBROUTINE'
C
      DO J = 1, MXTUBE
        X = DZ
        Y = DZ
        Z = DZ
        HEIGHT1 = DZ
        HEIGHT2 = DZ
C
C--DETERMINE COORDINATE
C--SEBA--X=COLUM, Y=ROW, Z=LAY
        X1 = NBR(ITUBE(J,2), 2)
        Y1 = NBR(ITUBE(J,2), 3)
        X2 = NBR(ITUBE(J,3), 2)
        Y2 = NBR(ITUBE(J,3), 3)
!       Z1 = NBR(ITUBE(J,2), 4)
!       Z2 = NBR(ITUBE(J,3), 4)
C
C--ADD LENGTH IN X-DIRECTION
        IF ( X1.NE.X2 ) THEN
          X = (DELR(X1)+DELR(X2))/DOS
          IF ( X1-1.GT.X2 ) THEN
            DO I = X2 + 1, X1 - 1
              X = X + DELR(I)
            ENDDO
          ENDIF
          IF ( X2-1.GT.X1 ) THEN
            DO I = X1 + 1, X2 - 1
              X = X + DELR(I)
            ENDDO
          ENDIF
        ENDIF
C
C--ADD LENGTH IN Y-DIRECTION
C  SEBA--FEHLER: IN Y-RICHTUNG MUSS ER DELC(ROW) NEHMEN:
        IF ( Y1.NE.Y2 ) THEN
          Y = (DELC(Y1)+DELC(Y2))/DOS
          IF ( Y1-1.GT.Y2 ) THEN
            DO I = Y2 + 1, Y1 - 1
              Y = Y + DELC(I)
            ENDDO
          ENDIF
          IF ( Y2-1.GT.Y1 ) THEN
            DO I = Y1 + 1, Y2 - 1
              Y = Y + DELC(I)
            ENDDO
          ENDIF
        ENDIF
C
C--SEBA--ADD LENGTH IN Z-DIRECTION USING NODE_LOCA
        HEIGHT1 = NODE_LOCA(ITUBE(J,2), 4)
        HEIGHT2 = NODE_LOCA(ITUBE(J,3), 4)
C 
        IF ( HEIGHT1.GT.HEIGHT2 ) THEN
          Z = HEIGHT1 - HEIGHT2
        ELSE
          Z = HEIGHT2 - HEIGHT1
        ENDIF
C
C--CALCULATE TOTAL LENGTH AFTER PYTHAGORAS
        LENGTH = DSQRT(X**DOS+Y**DOS+Z**DOS)
        CON_DATA(J, 3) = TORTUOS(J)*LENGTH
C 
        IF ( CON_DATA(J,3).LT.NEARZERO_30 ) WRITE (IOUT, *)                
     +        'WARNING!! TUBE ', J, ' HAS LENGTH DZ !!'
!       WRITE (IOUT, *) ('LENGTH TUBE ', J, ' ', CON_DATA(J,3))
      ENDDO
      END SUBROUTINE LENGTH_CAL
C
C
C
C
C
      SUBROUTINE ROUGH_CAL
C
C--THIS SUBROUTINE DETERMINES THE ROUGHNESS OF THE TUBE WALLS FROM THE
C  TUBE DIAMETER AND THE ROUGHNESS FACTOR, IF THAT IS < 0. ROUGHNESS
C  LENGTH IS THE TUBE_DIAMETER*(-1)*ROUGHNESS FACTOR.
C  ROUGHNESS FACTOR IS UPDATAD EVERY TIMESTEP
C
      USE GLOBAL, ONLY:IOUT
      USE CFPMODULE, ONLY:MXTUBE, ROUGHNESS, CON_DATA
C
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES
      INTEGER I
C------------------------------------------------------------------
      WRITE (IOUT, *) 'BEGINNING CFPM1 ROUGH_CAL SUBROUTINE'
      DO I = 1, MXTUBE
        IF ( ROUGHNESS(I).LT.0.0D0 ) CON_DATA(I, 4) = CON_DATA(I, 2)    
     +       *DABS(ROUGHNESS(I))
C       WRITE (*, *) ':: ', ROUGHNESS(I), CON_DATA(I,2), CON_DATA(I,4)
      ENDDO
C
      END SUBROUTINE ROUGH_CAL
C
C
C
C
C
      SUBROUTINE GWF2CFP1RP(INRCH)      
C***********************************************************************
C     CONDUIT RP PROCEDURE
C***********************************************************************
C
      USE CONSTANTS, ONLY: Z
      USE CFPMODULE, ONLY:MODE, KTSNO, KTSTU, KTSTSAN, KTSTSAT          !TR: 2013 08 13 TSAN / TSAT
      IMPLICIT NONE
      EXTERNAL CFP1RCH1RP
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: INRCH
C
      IF ( MODE.NE.2 ) THEN
        KTSNO = Z
        KTSTU = Z
        KTSTSAN = Z                                                     !TR: 2013 08 13 TSAN
        KTSTSAT = Z                                                     !TR: 2013 08 13 TSAT
      ENDIF
      IF ( INRCH.GT.Z ) CALL CFP1RCH1RP(INRCH)
      END SUBROUTINE GWF2CFP1RP      
C
C
C
C
C
      SUBROUTINE CFP1RCH1RP(INUNIT)
C     ******************************************************************
C     READ FACTOR FOR RECHARGE INPUT IN CONDUIT (UPDT: AND CADS)
C     ******************************************************************
C     VERSION 3 30JAN1999   RCHCONRD
C     UPDATE 2014 02 20 BY THOMAS.REIMANN@TU-DRESDEN.DE
C
      USE CFPMODULE, ONLY:MXNODE, QCONDIR, IFLAG_RECH, WELL_FLG, QWELL,
     + IWELL, WELLTD_FLG, CADS_FLG, CADSML_FLG, QCDSDIR                 !TR: 2012 06 08 WELL / ADDED WELL_FLG AND QWELL // 2014 02 19 CADS RECHARGE
      USE GLOBAL, ONLY:IOUT
C
      IMPLICIT NONE
C
C IOUT       PRIMARY UNIT NUMBER FOR ALL OUTPUT
C RECH_FACT  FACTOR FOR TOTALRECHARGE GOING IN CONDUITSYSTEM
C QCONDIR    ARRAY OF MXNODE ELEMENTS CONTAINING THE FACTOR FOR FLOW
C NUM        NUMBER OF PARAMETERS IN A LINE
C            COMING FROM RECHARGE IN THE CONDUIT SYSTEM
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: INUNIT
C
C--LOCAL VARIABLES
      INTEGER I, ID, NUM                                                !TR: 2014 02 19 CADS RECHARGE
      CHARACTER(LEN=80) CDUMMY
C
      WRITE (IOUT, *)
      WRITE (IOUT, *) 'CFP1RCH1RP SUBROUTINE READING FROM UNIT ', INUNIT
C 
C--OMIT ONE LINE OF TEXT
      READ (INUNIT, '(A)', ERR=200, END=100) CDUMMY
C
C--READ STRESS PERIOD NUMBER
      READ (INUNIT, *, END=100, ERR=300) IFLAG_RECH
C
C--READ WEIGHTING FOR DIRECT INFLOW IN THE NODES
      IF ( IFLAG_RECH.EQ.-1 ) THEN
C      
C--REUSE DATA FROM LAST STRESS PERIOD (DO NOTHING)
        WRITE (IOUT, *) ' REUSING DATA FROM LAST STRESS PERIOD'
      ELSE
C
C--READ DATA FOR CURRENT PERIOD
        DO I = 1, MXNODE
C        
C--CASE 1: WELL NOT ACTIVE      
          IF (.NOT.WELL_FLG) THEN                                       !TR: 2012 06 08 WELL / THE ORIGINAL FORMULATION
            IF (I.EQ.1) WRITE (IOUT,40)
C
C--CASE 1A: WITH CADS / CADSML              
            IF((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1))
     +      THEN                                                        !TR: 2014 02 19 CADS RECHARGE
C
C--INI QCDSDIR (A VALUE OF DZ IS USED IN CASE THERE IS NO USER INPUT)      
              QCDSDIR(I)=0.0
              CALL COUNT_LINE_NUMBERS(INUNIT,IOUT,NUM)                  !TR: 2014 02 19 CADS RECHARGE, CHECK NUMBER OF PARAMETERS IN LINE
              BACKSPACE (INUNIT)                                        !TR: 2014 02 19 READ AGAIN WITH ADEQUATE FORMAT
              IF (NUM.EQ.2) THEN                                        !TR: 2014 02 19 CADS RECHARGE
                READ(INUNIT,*,END=100,ERR=300)ID, QCONDIR(I)            !TR: 2014 02 19 CADS RECHARGE - NO CADS RECHARGE SPECIFIED - USE INITIAL VALUE (DZ)
              ELSEIF (NUM.EQ.3)THEN                                     !TR: 2014 02 19 CADS RECHARGE
                READ(INUNIT,*,END=100,ERR=300)ID, QCONDIR(I), QCDSDIR(I)!TR: 2014 02 19 CADS RECHARGE - READ CADS RECHARGE ADDITIONALLY
              ELSE                                                      !TR: 2014 02 19 CADS RECHARGE
                WRITE(IOUT,50)                                          !TR: 2014 02 19 CADS RECHARGE
              ENDIF                                                     !TR: 2014 02 19 CADS RECHARGE
C
C--CASE 1B: WITHOUT CADS          
            ELSE
              READ(INUNIT,*,END=100,ERR=300)ID, QCONDIR(I)
            ENDIF
C
C--CASE 2: ACTIVE WELL            
          ELSE                                                          !TR: 2012 06 08 WELL /
            IF (I.EQ.1) WRITE (IOUT,45)
C
C--CASE 2A: WITH CADS / CADSML               
            IF((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1))
     +      THEN                                                        !TR: 2014 02 19 CADS RECHARGE
C
C--INI QCDSDIR (A VALUE OF DZ IS USED IN CASE THERE IS NO USER INPUT)      
              QCDSDIR(I)=0.0                                            !TR: 2014 02 24 CADS RECHARGE
              CALL COUNT_LINE_NUMBERS(INUNIT,IOUT,NUM)                  !TR: 2014 02 19 CADS RECHARGE, CHECK NUMBER OF PARAMETERS IN LINE
              BACKSPACE (INUNIT)                                        !TR: 2014 02 19 CADS RECHARGE
C
C--CASE 2A_1: NODE WITH WELL              
              IF (IWELL(I).EQ.1) THEN
                IF(NUM.EQ.3)THEN
                  READ (INUNIT,*,END=100,ERR=300) ID,QCONDIR(I),QWELL(I)!TR: 2014 02 19 CADS RECHARGE - NO CADS RECHARGE SPECIFIED - USE INITIAL VALUE (DZ)
                ELSEIF(NUM.EQ.4) THEN
                  READ (INUNIT,*,END=100,ERR=300) ID,QCONDIR(I),        !TR: 2014 02 19 CADS RECHARGE - READ CADS RECHARGE ADDITIONALLY
     +                                            QCDSDIR(I),QWELL(I)
                ELSE
                  WRITE(IOUT,50)                                        !TR: 2014 02 19 CADS RECHARGE
                ENDIF
C
C--CASE 2A_2: NODE WITHOUT WELL                
              ELSE
                IF (NUM.EQ.2) THEN                                      !TR: 2014 02 19 CADS RECHARGE
                  READ(INUNIT,*,END=100,ERR=300)ID,QCONDIR(I)           !TR: 2014 02 19 CADS RECHARGE - NO CADS RECHARGE SPECIFIED - USE INITIAL VALUE (DZ)
                ELSEIF (NUM.EQ.3)THEN                                   !TR: 2014 02 19 CADS RECHARGE
                  READ(INUNIT,*,END=100,ERR=300)ID,QCONDIR(I),QCDSDIR(I)!TR: 2014 02 19 CADS RECHARGE - READ CADS RECHARGE ADDITIONALLY
                ELSE                                                    !TR: 2014 02 19 CADS RECHARGE
                  WRITE(IOUT,50)                                        !TR: 2014 02 19 CADS RECHARGE
                ENDIF                                                   !TR: 2014 02 19 CADS RECHARGE
              ENDIF
C
C--CASE 2B: WITHOUT CADS    
            ELSE          
C
C--CASE 2B_2: NODE WITHOUT WELL       
              READ (INUNIT, *, END=100, ERR=300) ID, QCONDIR(I)         !TR: 2012 06 08 WELL /
C
C--CASE 2B_1: NODE WITH WELL              
              IF (IWELL(I).EQ.1) THEN
                BACKSPACE (INUNIT)                                      !TR: 2013 03 15 READ AGAIN WITH ADEQUATE PARAMETERS
                READ (INUNIT,*,END=100,ERR=300) ID,QCONDIR(I),QWELL(I)  !TR: 2012 06 08 WELL
              ENDIF
            ENDIF
          ENDIF                                                         !TR: 2012 06 08 WELL /
          IF (ID.NE.I)WRITE(IOUT,60)
        ENDDO                                                           !TR: 2012 06 08 WELL /
      ENDIF
C
C--PRINT OUT QCONDIR
      WRITE (IOUT,*)'DATA FOR QCONDIR'
      WRITE (IOUT, 9001) (QCONDIR(I), I=1, MXNODE)
C
C--PRINT OUT QCDSDIR
      IF((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1)) THEN !TR: 2014 02 19 CADS RECHARGE
        WRITE (IOUT,*)'DATA FOR QCDSDIR'                                !TR: 2014 02 19 CADS RECHARGE
        WRITE (IOUT, 9001) (QCDSDIR(I), I=1, MXNODE)                    !TR: 2014 02 19 CADS RECHARGE
      ENDIF                                                             !TR: 2014 02 19 CADS RECHARGE
C
C--PRINT OUT QWELL                                                      !TR: 2012 06 08 WELL /
      IF (WELL_FLG) THEN                                                !TR: 2012 06 08 WELL /
        WRITE (IOUT,*)'DATA FOR WELLS:'                                 !TR: 2012 06 08 WELL /
        WRITE (IOUT, 9001) (QWELL(I), I=1, MXNODE)                      !TR: 2012 06 08 WELL /
      ENDIF                                                             !TR: 2012 06 08 WELL /
C      
      RETURN
C
  40  FORMAT (' READING INPUT USED TO PARTICIAN RECHARGE')
  45  FORMAT (' READING INPUT USED TO PARTICIAN RECHARGE AND WELL DATA')!TR: 2012 06 08 WELL /
  50  FORMAT ('ERROR IN CRCH INPUT FILE; CHECK NUMBER OF PARAMETERS IN',
     +        ' LINES')
  60  FORMAT ('ERROR, WRONG NODE IN CRCH PACKAGE')
 100  WRITE (IOUT, *) ' FILE TYPE "CRCH" NOT FOUND, EOL, OR PROTECTED!'
      RETURN
 200  WRITE (IOUT, *) ' READ ERROR CONDUIT INPUT FILE. WRONG DATA TYPE',
     +                'COMMENT LINE EXPECTED, INSTEAD READ: ', CDUMMY
      STOP
 300  WRITE (IOUT, *) ' READ ERROR CONDUIT INPUT FILE. WRONG DATA TYPE',
     +                'DATA LINE EXPECTED. LAST COMMENT LINE: ', CDUMMY
      STOP
 9001 FORMAT (15(G7.2E1,X))
C
      END SUBROUTINE CFP1RCH1RP
C
C
C
C
C
      SUBROUTINE GWF2CFP1AD(KKPER, KKSTP)
C     ******************************************************************
C     CONDUIT AD PROCEDURE
C     ******************************************************************
C     VERSION 4 30JAN1999    RECHCOND1
C
      USE CONSTANTS, ONLY: Z, TRUE, FALSE
      USE CFPMODULE, ONLY:MODE,KTSNO,KTSTU,WELLTD_IN,QWELL,WELLTD_FLG,  !TR: 2013 04 03 WELL TD
     + WELLTD_DAT,WELLTD_LINESMAX,WELLTD_COUNT,CYTD_IN,HCY,CYTD_FLG,    !TR: 2013 04 03 CAUCHY TD
     + CYTD_DAT,CYTD_LINESMAX,CYTD_COUNT,FHTD_IN,FHTD_FLG,FHTD_DAT,     !TR: 2013 04 04 FH TD
     + FHTD_COUNT,FHTD_LINESMAX,BEGHEAD,FHLQTD_IN,FHLQTD_FLG,FHLQTD_DAT,
     + FHLQTD_COUNT,FHLQTD_LINESMAX,ACTIVENODE,IFHLQ,ICY,DFHLQ,         !TR: 2013 04 04 FHLQTD
     + CADSML_FLG,CADSMLDAT,NODEBOT,KTSTSAN, KTSTSAT                    !TR: 2013 07 08 CADSML // 2013 08 13 TSAN // TSAT
      USE GWFBASMODULE, ONLY: TOTIM
C
      IMPLICIT NONE
C
      EXTERNAL EXCHANG_CAL, GETTDDATA                                   !TR: 2013 04 03 WELL TD
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KKPER, KKSTP
      INTEGER I, II, IDUMMY(ACTIVENODE)
      DOUBLE PRECISION DUMMY(ACTIVENODE)
      LOGICAL DUMMYFLG
C
C--INITIALIZE
      DUMMYFLG = FALSE      
C
C--ADVANCE COUNTERS
      IF ( MODE.NE.2 ) THEN
        KTSNO = KTSNO + 1
        KTSTU = KTSTU + 1
        KTSTSAN = KTSTSAN + 1                                           !TR: 2013 08 13 TSAN
        KTSTSAT = KTSTSAT + 1                                           !TR: 2013 08 13 TSAT
      ENDIF
C 
C--DISTRIBUTE RECHARGE IN CONDUIT AND MATRIX SYSTEM
CB      IF ( INCFPRCH.GT.0 .AND. MODE.NE.2 )                              
CB     +     CALL CFP1DCRCH(KKPER, KKSTP)
C 
C--COMPUTE EXCHANGE BETWEEN POROUS MEDIA AND PIPE CONDUITS
      IF ( MODE.NE.2 ) CALL EXCHANG_CAL(KKPER, KKSTP)
C
C--SET NODEBOT AS LOWER BOUNDARY FOR CADSML
      IF ((CADSML_FLG.EQ.1).AND.(KKPER.EQ.1.AND.KKSTP.EQ.1)) THEN       !TR: 2013 07 08 CADSML
        DO I = 1, ACTIVENODE                                            !TR: 2013 07 08 CADSML
          CADSMLDAT(I,1,5) = NODEBOT(I)                                 !TR: 2013 07 08 CADSML
          IF ((CADSMLDAT(I,1,5).GT.CADSMLDAT(I,1,4)).AND.
     +(CADSMLDAT(I,2,4).NE.Z)) CALL USTOP('CADS DEFINED FOR ELEVATIONS B!TR: 2013 07 08 CADSML
     +ELOW NODE BOTTOM - STOP')                                         !TR: 2013 07 08 CADSML
        ENDDO                                                           !TR: 2013 07 08 CADSML
      ENDIF                                                             !TR: 2013 07 08 CADSML
C 
C--IF TIME DEPENDENT BOUNDARY CONDITIONS, GET INTERPOLATED VALUES
C  WELLS
      IF (WELLTD_FLG)CALL GETTDDATA(TOTIM,QWELL,WELLTD_DAT,             !TR: 2013 04 03 WELL TD
     +       WELLTD_LINESMAX,WELLTD_COUNT,WELLTD_IN,IDUMMY,1, DUMMYFLG) !TR: 2013 04 03 WELL TD
C 
C--CAUCHY
      IF (CYTD_FLG)CALL GETTDDATA (TOTIM,HCY,CYTD_DAT,CYTD_LINESMAX,    !TR: 2013 04 03 CAUCHY TD
     +                             CYTD_COUNT,CYTD_IN,ICY,2,DUMMYFLG)            !TR: 2013 04 03 CAUCHY TD
C 
C--FIXED HEAD TD
      IF (FHTD_FLG)CALL GETTDDATA (TOTIM,BEGHEAD,FHTD_DAT,FHTD_LINESMAX,!TR: 2013 04 04 FH TD
     +                             FHTD_COUNT,FHTD_IN,IDUMMY,1,DUMMYFLG)!TR: 2013 04 04 FH TD
C 
C--FHLQ TD
      IF (FHLQTD_FLG)CALL GETTDDATA (TOTIM,DUMMY,FHLQTD_DAT,            !TR: 2013 04 04 FH TD
     +         FHLQTD_LINESMAX,FHLQTD_COUNT,FHLQTD_IN,IFHLQ,2,DUMMYFLG) !TR: 2013 04 04 FH TD
      DO I = 1, ACTIVENODE
        DO II = 1, ACTIVENODE
          IF(FHLQTD_IN(I).GT.Z.AND.(IFHLQ(II,1).EQ.I))
     +       DFHLQ(II,1)=DUMMY(II)
        ENDDO
      ENDDO
C      
      END SUBROUTINE GWF2CFP1AD
C
C
C
C
C
      SUBROUTINE CFP1DCRCH(KKPER, KKSTP,IURCH)
C     ********************************************************************
C     DISTRIBUTE RECHARGE IN CONDUIT AND FISSURED SYSTEM (UPDT: AND CADS)
C     ********************************************************************
C     VERSION 4 30JAN1999    RECHCOND1
C     UPDATE 2014 02 20 BY THOMAS.REIMANN@TU-DRESDEN.DE
C
      USE CONSTANTS, ONLY: DZ, Z
      USE GLOBAL, ONLY:IBOUND, NCOL, NROW, NLAY, IOUT
      USE CFPMODULE, ONLY:MXNODE, MXTUBE, NBR, BEGHEAD, QCONDIR, QBDIR, 
     + QCONDIROLD, IFLAG_RECH, QBDIROLD, QBDIRSAVE, QSDIR,
     + QSDIROLD, QSDIRSAVE, QCDSDIR, QCDSDIROLD, CADS_FLG, CADSML_FLG   !TR: 2014 02 20 CADS RECHARGE
      !RSR?? NEED TO MAKE THIS WORK WITH UZF
      USE GWFRCHMODULE, ONLY:RECH, INRECH
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KKPER, KKSTP
C
C--LOCAL VARIABLES
      INTEGER I, IC, IL, IR,IURCH
C
C ------ INPUT:
C MXNODE      NUMBER OF NODES
C QCONDIR     ARRAY OF MXNODE ELEMENTS CONTAINING THE FACTOR FOR WEIGHTING
C             OF THE RECHARGE FLOW INTO THE CONDUITSYSTEM
C IFLAG_RECH  FLAG IF THERE AREN'T NEW RECHARGE DATA: -1
C INRECH      FLAG FOR NEW MODFLOW DATA
C RECH_FACT   FACTOR OF DIRECT FLOW IN CONDUITSYSTEM:
C             RECH_CON = RECH_FACT * RECHARGE
C 
C ------ INTERNALE VARIABLES:
C TOTAL_RECHARGE  TOTAL RECHARGE OVER THE WHOLE AREA
C 
C ------ OUTPUT:
C RECH          RECHARGE FLOW RATE. (FLOW RATE INJECTED DIRECTLY INTO THE
C               CONDUITS MUST BE REMOVED HERE!)
C QBDIR         FLOW RATE INJECTED DIRECTLY INTO CONDUIT
C QSDIR         FLOW RATE INJECTED DIRECTLY INTO CADS
C RECH_CON      TOTAL FLOW OF RECHARGE IN CONDUIT SYSTEM (=QBDIR)
C RECH_MAT      TOTAL RECHARGE IN MATRIX
C BUDGET_RECH   TOTAL RECHARGE AFTER DISTRIBUTION
C
C--IF THERE ARE NEW RECHARGE DATA IN EITHER MODFLOW (INRECH > 0) OR
C  CONDUIT (IFLAG_RECH > 0) AND IT IS THE FIRST TIMESTEP OF THE
C  STRESS PERIOD AND THE FIRST WD-ITERATION:
C--------------------------------------------------------------------------
C
C--BARC**IF RCH INACTIVE, SET THESE
      IF(IURCH.EQ.Z) THEN
        QBDIROLD=Z
        DO I=1,MXNODE
          QBDIR(I)= 0.0
          QBDIRSAVE(I)= 0.0
        ENDDO
C
C--IF CADS ACTIVE
        IF((CADS_FLG.EQ.1).OR.(CADS_FLG.EQ.-1).OR.(CADSML_FLG.EQ.1))THEN!TR: 2014 02 20 CADS RECHARGE
          QSDIROLD=Z                                                    !TR: 2014 02 20 CADS RECHARGE
          DO I=1,MXNODE                                                 !TR: 2014 02 20 CADS RECHARGE
            QSDIR(I)= 0.0                                               !TR: 2014 02 20 CADS RECHARGE
            QSDIRSAVE(I)= 0.0                                           !TR: 2014 02 20 CADS RECHARGE
          ENDDO                                                         !TR: 2014 02 20 CADS RECHARGE
        ENDIF                                                           !TR: 2014 02 20 CADS RECHARGE
        RETURN
      ENDIF
C
      IF ( (IFLAG_RECH.NE.-1 .OR. INRECH.NE.-1) .AND. KKSTP.EQ.1 ) THEN
C
C--CALCULATE  RECHARGE TO MODFLOW CELLS WITH CONDUIT NODES
        DO I = 1, MXNODE
          IC = NBR(I, 2)
          IR = NBR(I, 3)
          IL = NBR(I, 4)
          QBDIROLD = QBDIR(I)
C
C--RESET RECH TO VALUE BEFORE QBDIR OF LAST TIMESTEP WAS SUBSTRACTED, IF
C  THERE ARE NEW QCONDIR DATA
C--MAKE QCONDIR(I) = 1.0 / QCDSDIR(I) = 1.0 POSSIBLE BY CONSIDERING IT  !TR: 2014 02 20 CADS RECHARGE
          IF ((INRECH.EQ.-1).AND.(QCONDIROLD(I).LT.1.0D0).AND.
     +        (QCDSDIROLD(I).LT.1.0D0)) THEN                            !TR: 2014 02 20 CADS RECHARGE
            RECH(IC, IR) = RECH(IC, IR)/(1.0D0-QCONDIROLD(I)
     +                                   -QCDSDIROLD(I))                !TR: 2014 02 20 CADS RECHARGE
          ELSEIF ((INRECH.EQ.-1).AND.(QCONDIROLD(I).EQ.1.0D0)) THEN
            RECH(IC, IR) = QBDIR(I)
          ELSEIF ((INRECH.EQ.-1).AND.(QCDSDIROLD(I).EQ.1.0D0)) THEN
            RECH(IC, IR) = QSDIR(I)          
          ENDIF
C
C--BARC2**
          QBDIR(I) = DZ
          QSDIR(I) = DZ                                               !TR: 2014 02 20 CADS RECHARGE
C
C--IF MODFLOW CELL IS NOT CONSTANT HEAD
          IF (IBOUND(IC,IR,IL).GE.Z) THEN
C
C--NO RECHARGE IS GIVEN TO CONST. HEAD NODE OF CONDUIT
C  GIVE RECHARGE TO NODE I - CONDUIT
            QBDIR(I) = QCONDIR(I)*RECH(IC, IR)
            QBDIRSAVE(I) = QBDIR(I)
C  GIVE RECHARGE TO NODE I - CADS                                       !TR: 2014 02 20 CADS RECHARGE
            QSDIR(I) = QCDSDIR(I)*RECH(IC, IR)                          !TR: 2014 02 20 CADS RECHARGE
            QSDIRSAVE(I) = QSDIR(I)                                     !TR: 2014 02 20 CADS RECHARGE
C
C--SUBSTRACT DIRECT RECHARGE TO CONDUIT SYSTEM / CADS FROM RECHARGE IN 
C  MATRIX
            RECH(IC, IR) = (1.0D0-QCONDIR(I)-QCDSDIR(I))*RECH(IC, IR)   !TR: 2014 02 20 CADS RECHARGE
          ENDIF
        ENDDO
C
        DO I = 1, MXNODE
          QCONDIROLD(I) = QCONDIR(I)
          QCDSDIROLD(I) = QCDSDIR(I)                                    !TR: 2014 02 20 CADS RECHARGE
        ENDDO
C
      ENDIF
C
      END SUBROUTINE CFP1DCRCH