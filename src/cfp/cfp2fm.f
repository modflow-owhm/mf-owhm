C***********************************************************************
C  CONDUIT FLOW PROCESS SUBROUTINES - FORMULATE
C  
C  A LIST WITH VARIABLE / PARAMETER NAMES IS PROVIDED AT THE END OF THE
C  THE FILE
C
C VERSION 1.0 NOVEMBER 2007
C VERSION 2.0 APRIL 2010
C MODIFIED FOR CFP VERSION 2.0 JANUARY 2011
C***********************************************************************
C
C
C
C
C
      SUBROUTINE GWF2CFPM2FM(KKITER, KKSTP, KKPER,ICNVG)
C **********************************************************************
C MODE 2 FORMULATE
C 
C ADAPTED VERSION = CONDUIT TYPE FLOW IN CONTINUUM CTFC
C
C 1) FADJ COMPUTATION CHANGED, KTURB ITERATION REMOVED;
C    MODE 2 FEEXP IS SET TO 1.5 TO BEHAVE LIKE THE ORIGINAL CFPM2
C 2) DISCHARGE DISCONTINUITY ADDED (IF THIS SHOULD NOT BE CONSIDER, 
C    USE SIMILAR VALUES FOR CRITICAL REYNOLD NUMBER IN INPUT FILE)
C 3) LAMINAR PRECALCULATION ADDED
C 4) ARRAYS CRT, CCT REMOVED
C 5) COMPUTATION OF FLOW AREA MODIFIED (ALSO TO ACCOUNT FOR SOME INADEQUATE
C    INITIAL CONDITIONS)
C
C **********************************************************************
C LAST MODIFIED: 2011 BY BS/TR
C       
C--USE MODULES
      USE GLOBAL, ONLY:IOUT, NCOL, NROW, NLAY, HOLD, BOTM, CR, CC, HNEW,
     +                 DELR, DELC, CV
C
      USE CFPMODULE, ONLY:MODE, NCL, CL, TWATER, DENSWA, VISCWA,        !TR: 2023 07 02 REMOVED SPATIAL VARIABILITY
     +                    LCRITREY, TCRITREY, VOID, TURB_FR, TURB_FF,
!     USE CFPMODULE, ONLY:MODE, NCL, CL, TWATER2, DENSWA2, VISCWA2,
!    +                    LCRITREY2, TCRITREY2, VOID2, TURB_FR, TURB_FF,
!    +                    TURB_FV,KLAM_CR, KLAM_CC, KLAM_CV, CRT, CCT, 
     +                    TURB_FV,KLAM_CR, KLAM_CC, KLAM_CV,
!    +                    CVT, FEEXP,ICFPCNVG,R_LENGTH,IRADFLAG         !TR: 2023 07 01 REMOVED RADIAL FLOW
     +                    FEEXP, ICFPCNVG                          !TR: 2023 07 01 REMOVED RADIAL FLOW      
C
CBARC*************************************************************************
      IMPLICIT NONE
      INTRINSIC DSQRT, DABS
C
C--ARGUMENTS
      !RSR, KKITER, KKSTP, KKPER NOT USED
      INTEGER, INTENT(IN) :: KKITER, KKSTP, KKPER
C
C--LOCAL VARIABLES
C--ELK  LEAVE FADJ IN CODE 
      DOUBLE PRECISION HDIFF, LENGTH, DHCRIT_LT, DHCRIT_TL, THK, KLAM, 
     +                 AREA, FADJ
!    +                 AREA, FADJ,TCRIT_AVG,LCRIT_AVG,VIS_AVG,VOID_AVG,  !TR: 2023 07 02 REMOVED SPATIAL VARIABILITY
!    +                 FE_AVG,AREA_RAD
C
      INTEGER I, IC, J, K, ICNVG
C
C--SKIP IF MODE 1 IS ACTIVE!
      IF ( MODE.EQ.1 ) RETURN
      
      IF(KKITER < 5) ICFPCNVG = 0
C 
C--CBARC**SET LAMINAR CONDUCTANCES AND TURBULENCE FLAGS
      IF(KKSTP.EQ.1.AND.KKITER.EQ.1.AND.KKPER.EQ.1) THEN
       DO J=1,NCOL
	  DO I=1,NROW
	   DO K=1,NLAY
	      KLAM_CR(J,I,K) = CR(J,I,K)
CB	       CRT(J,I,K) = KLAM_CR(J,I,K)
	      KLAM_CC(J,I,K) = CC(J,I,K)
CB	       CCT(J,I,K) = KLAM_CC(J,I,K)       
            TURB_FR(J, I, K) = 0
            TURB_FF(J, I, K) = 0
         ENDDO
	  ENDDO
	 ENDDO
      ENDIF
C
C---CBARC** RUN CTFC ONLY AFTER LAM. CONVERGENCE            
      IF(ICFPCNVG == 0) RETURN
!!!      ICNVG=0
!!!      ICFPCNVG=1
!!!      PRINT*, "COMPUTING TURBULENT CONDUCTANCES CR CC AND CV"
CB      PRINT*, ICNVG,ICFPCNVG
C
C--CBARC&ELK**MODIFY CC AND CR IF FLOW IS TURBULENT 
      DO IC = 1, NCL
        DO I = 1, NROW
          DO J = 1, NCOL
C
C--BARC**FLOW TO RIGHT
C
C--BARC**DON'T DO THIS FOR LAST COLUMN 
            IF ( J.EQ.NCOL ) EXIT
C
C--BARC**SET LENGTH
            LENGTH = (DELR(J)+DELR(J+1))/2
C
C--BARC**SET THCK
CB            PRINT*,BOTM(J, I, CL(IC)-1)-BOTM(J, I, CL(IC))
            THK = BOTM(J, I, CL(IC)-1) - BOTM(J, I, CL(IC))
CB           PAUSE
            IF(HNEW(J, I, CL(IC)).LT.BOTM(J, I, CL(IC)-1)) THEN
             THK = HNEW(J, I, CL(IC)) - BOTM(J, I, CL(IC))
            ENDIF
            AREA = THK*DELC(I)
            KLAM=(CR(J,I,CL(IC))*LENGTH)/AREA                           !TR: 2009 12 07 USED CR INSTEAD OF KLAM_CR, 100% CORRECT ONLY FOR SATURATED SITUATIONS
C
C--BARC**CALC AREA FOR RADIAL FLOW MODELS
!           IF(IRADFLAG.EQ.1)THEN                                       !TR:2023 07 01 REMOVED RADIAL FLOW 
CB             AREA_RAD=THK*(2*3.14159*R_LENGTH(J))
!            KLAM=CR(J,I,CL(IC))*(LENGTH/(2*3.14159*R_LENGTH(J)*AREA))  !TR:2023 07 01 REMOVED RADIAL FLOW
CB             PRINT*,KLAM
!           ENDIF                                                       !TR:2023 07 01 REMOVED RADIAL FLOW
C
C--BARC**COMPUTE AVERAGES
!           TCRIT_AVG=(TCRITREY2(J,I,CL(IC))+TCRITREY2(J+1,I,CL(IC)))/2
!           LCRIT_AVG=(LCRITREY2(J,I,CL(IC))+LCRITREY2(J+1,I,CL(IC)))/2
!           VIS_AVG=(VISCWA2(J,I,CL(IC))+VISCWA2(J+1,I,CL(IC)))/2
!           VOID_AVG=(VOID2(J,I,CL(IC))+VOID2(J+1,I,CL(IC)))/2
!           FE_AVG=(FEEXP(J,I,CL(IC))+FEEXP(J+1,I,CL(IC)))/2
C            
C--BARC**COMPUTE UPPER AND LOWER CRITICAL HEAD DIFFERENCES AT RIGHT FACE
C--ELK DHCRIT IS CORRECT ALONG ROW 1 FROM BENCHMARK COMPUTATION             
!           DHCRIT_LT = (TCRIT_AVG*VIS_AVG*LENGTH)/(VOID_AVG*KLAM)      !TR: 2023 07 02 REMOVED SPATIAL VARIABILITY
!           DHCRIT_TL = (LCRIT_AVG*VIS_AVG*LENGTH)/(VOID_AVG*KLAM)
            dhcrit_lt = (Tcritrey(ic)*Viscwa*length)/
     &                  (Void(ic)*Klam)

            dhcrit_tl = (Lcritrey(ic)*Viscwa*length)/
     &                  (Void(ic)*Klam)
C
C--BARC**COMPUTE HDIFF FOR FLOW TO RIGHT
C--BARCANDELK** USE HNEW
            HDIFF = HNEW(J, I, CL(IC)) - HNEW(J+1, I, CL(IC))

C--BARC**FORCE HDIFF POSITIVE
            HDIFF = DABS(HDIFF)
C            
C--BARC**QLAM TO QTURB-----------------------------------------------
            IF ((HDIFF.GE.DHCRIT_LT).OR.((TURB_FR(J, I, CL(IC)).EQ.1)   !TR: 2010 04 30 FADJ IF HDIFF > DHCRIT_LT OR IF HDIFF > DHCRIT_TL IF DISCHARGE IS ALREADY TURBULENT
     +           .AND.(HDIFF.GE.DHCRIT_TL)))  THEN
C--BARC**SET TURBULENCE FLAG
               TURB_FR(J, I, CL(IC)) = 1
C--BARC**COMPUTE TURBULENT CR
C--BARC**	FADJ=DSQRT((KLAM_CR(J,I,CL(IC))*DHCRIT_LT)/
CB     +(CRT(J,I,CL(IC))*HDIFF))
C
C--!TR: COMPUTE FADJ ACCORDING TO FEEXP
               Fadj = (hdiff / dhcrit_tl)**((1.0 - FEexp(ic))/FEexp(ic)) !TR: 2010 12 23; compact formulation; successfully tested by comparison      
               CR(J, I, CL(IC)) = FADJ*KLAM_CR(J, I, CL(IC))
C
C--BARC**ENDIF FOR IF ( HDIFF.GT.DHCRIT_LT ) 
            ENDIF 
C
C--BARC**QTURB TO QLAM------------------------------------------------
             IF ( HDIFF.LT.DHCRIT_TL ) THEN
                TURB_FR(J, I, CL(IC)) = 0
             ENDIF
C
C--BARC**ENDDO FOR J LOOP
          ENDDO
C
C--BARC**JUMP HERE IF J.EQ.NCOL
C--BARC**ENDDO FOR I LOOP
        ENDDO
C 
C--BARC**FLOW TO FRONT****************************************************
C 
        DO I = 1, NROW
          DO J = 1, NCOL
C
C--BARC**EXIT IF LAST ROW
            IF ( I.EQ.NROW ) EXIT
C 
C--BARC**SET LENGTH
            LENGTH = (DELC(I)+DELC(I+1))/2
C
C--BARC**SET THCK
            THK = BOTM(J, I, CL(IC)-1) - BOTM(J, I, CL(IC))
            IF(HNEW(J, I, CL(IC)).LT.BOTM(J, I, CL(IC)-1)) THEN
              THK = HNEW(J, I, CL(IC)) - BOTM(J, I, CL(IC))
            ENDIF
            AREA = THK*DELR(J)
C
C--ELK CALC KLAM [L/T] FOR DHCRIT CALCULATIONS
            KLAM=(CC(J,I,CL(IC))*LENGTH)/AREA                           !TR: 2009 12 07 USED CC INSTEAD OF KLAM_CC, 100% CORRECT ONLY FOR SATURATED SITUATIONS
C
C--BARC**COMPUTE AVERAGES
!           TCRIT_AVG=(TCRITREY2(J,I,CL(IC))+TCRITREY2(J,I+1,CL(IC)))/2 !TR: 2023 07 02 REMOVED SPATIAL VARIABILITY
!           LCRIT_AVG=(LCRITREY2(J,I,CL(IC))+LCRITREY2(J,I+1,CL(IC)))/2
!           VIS_AVG=(VISCWA2(J,I,CL(IC))+VISCWA2(J,I+1,CL(IC)))/2
!           VOID_AVG=(VOID2(J,I,CL(IC))+VOID2(J,I+1,CL(IC)))/2
!           FE_AVG=(FEEXP(J,I,CL(IC))+FEEXP(J,I+1,CL(IC)))/2
C
C--BARC**COMPUTE CRITICAL HEAD DIFFERENCES AT FRONT FACE
!           DHCRIT_LT = (TCRIT_AVG*VIS_AVG*LENGTH)/(VOID_AVG*KLAM)
!           DHCRIT_TL = (LCRIT_AVG*VIS_AVG*LENGTH)/(VOID_AVG*KLAM)
            
            dhcrit_lt = (Tcritrey(ic)*Viscwa*length)/ 
     &                  (Void(ic)*Klam)

            dhcrit_tl = (Lcritrey(ic)*Viscwa*length)/
     &                  (Void(ic)*Klam)
            
            HDIFF = HNEW(J, I, CL(IC)) - HNEW(J, I+1, CL(IC))
C 
C--BARC**FORCE HDIFF POSITIVE
            HDIFF = DABS(HDIFF)
C
C--BARC**QLAM TO QTURB-------------------------------------------------
            IF ((HDIFF.GE.DHCRIT_LT).OR.((TURB_FF(J, I, CL(IC)).EQ.1)   !TR: 2010 04 30 FADJ IF HDIFF > DHCRIT_LT OR IF HDIFF > DHCRIT_TL IF DISCHARGE IS ALREADY TURBULENT
     +           .AND.(HDIFF.GE.DHCRIT_TL)))  THEN              
C
C--BARC**SET TURBULENCE FLAG
              TURB_FF(J, I, CL(IC)) = 1
C
C--BARC&ELK**MAKE MODIFICATION TO CC
CB      FADJ=DSQRT((KLAM_CC(J,I,CL(IC))*DHCRIT_LT)/
CB     +(CCT(J,I,CL(IC))*HDIFF))
              Fadj = (hdiff / dhcrit_tl)**((1.0 - FEexp(ic))/FEexp(ic)) !TR: 2010 12 23; compact formulation; successfully tested by comparison    
     
              CC(J, I, CL(IC)) = FADJ*KLAM_CC(J, I, CL(IC))
C
C--BARC**ENDIF STATEMENT FOR IF ( HDIFF.GT.DHCRIT_LT )
C--ELK  CCT ARRAY USED TO STORE TURBULENT CONDUCTANCE FOR NEXT ITERATION
            ENDIF
C
C--BARC**QTURB TO QLAM--------------------------------------------------
            IF ( HDIFF.LT.DHCRIT_TL ) THEN
              TURB_FF(J, I, CL(IC)) = 0
            ENDIF
C
C--BARC**ENDDO FOR J LOOP
          ENDDO
C
C--BARC**ENDDO FOR I LOOP
        ENDDO
C
C--BARC**GOTO HERE IF I.EQ.NROW
C
C--BARC**VERT FLOW****************************************************
!TR: 2023 07 02 NOT IN THE ORIGINAL FORMULATION / DEACTIVATED HERE
!       DO I = 1, NROW
!         DO J = 1, NCOL
C
C--BARC**EXIT IF LAST ROW
!           IF ( CL(IC).EQ.NLAY ) EXIT
C 
C--BARC**SET LENGTH
!           LENGTH = (BOTM(J, I, CL(IC)-1) - BOTM(J, I, CL(IC)))/2 +
!    +             (BOTM(J, I, CL(IC)) - BOTM(J, I, CL(IC)+1))/2
!           AREA = DELC(I)*DELR(J)
CB            PRINT*,LENGTH,AREA
!           KLAM=(CV(J,I,CL(IC))*LENGTH)/AREA                           !TR: 2009 12 07 USED CC INSTEAD OF KLAM_CC, 100% CORRECT ONLY FOR SATURATED SITUATIONS
C
C--BARC**COMPUTE AVERAGES, IF NEEDED
C--BARC**THIS INCLUDES CASES WHEN NCL.EQ.1; AND WHEN DARCIAN FLOW LAYER UNDERLIES 
C--BARC**A NON-DARCIAN FLOW LAYER                                       !TR: 2023 07 02 REMOVED SPATIAL VARIABILITY
!           TCRIT_AVG=TCRITREY2(J,I,CL(IC))
!           LCRIT_AVG=LCRITREY2(J,I,CL(IC))
!           VIS_AVG=VISCWA2(J,I,CL(IC))
!           VOID_AVG=VOID2(J,I,CL(IC))
!           FE_AVG=FEEXP(J,I,CL(IC))
C
C--BARC**USE THIS WHEN A NON-DARCIAN FLOW LAYER UNDERLIES A NON-DARCIAN FLOW LAYER
!           IF(NCL.GT.1) THEN
!             IF(CL(IC)+1.EQ.CL(IC+1)) THEN
!               TCRIT_AVG=(TCRITREY2(J,I,CL(IC))
!    +                    +TCRITREY2(J,I,CL(IC)+1))/2
!               LCRIT_AVG=(LCRITREY2(J,I,CL(IC))
!    +                    +LCRITREY2(J,I,CL(IC)+1))/2
!               VIS_AVG=(VISCWA2(J,I,CL(IC))+VISCWA2(J,I,CL(IC)+1))/2
!               VOID_AVG=(VOID2(J,I,CL(IC))+VOID2(J,I,CL(IC)+1))/2
!               FE_AVG=(FEEXP(J,I,CL(IC))+FEEXP(J,I,CL(IC)+1))/2
!             ENDIF
!           ENDIF
C
C--BARC**MAY NEED TO CHANGE THIS, USE AN AVERAGE OF DHCRIT(CL(IC)) AND DHCRIT(CL(IC)+1)
!           DHCRIT_LT = (TCRIT_AVG*VIS_AVG*LENGTH)/ 
!    +                  (VOID_AVG*KLAM)
C
!           DHCRIT_TL = (LCRIT_AVG*VIS_AVG*LENGTH)/
!    +                  (VOID_AVG*KLAM)
C
!           HDIFF = HNEW(J, I, CL(IC)) - HNEW(J, I, CL(IC)+1)
C 
C--BARC**FORCE HDIFF POSITIVE
!           HDIFF = DABS(HDIFF)
C
C--BARC**QLAM TO QTURB--------------------------------------------------
!           IF ((HDIFF.GE.DHCRIT_LT).OR.((TURB_FF(J, I, CL(IC)).EQ.1)   !TR: 2010 04 30 FADJ IF HDIFF > DHCRIT_LT OR IF HDIFF > DHCRIT_TL IF DISCHARGE IS ALREADY TURBULENT
!    +           .AND.(HDIFF.GE.DHCRIT_TL)))  THEN              

C
C--BARC**SET TURBULENCE FLAG
!             TURB_FV(J, I, CL(IC)) = 1
C
C--BARC&ELK**MAKE MODIFICATION TO CV
CB      FADJ=DSQRT((KLAM_CV(J,I,CL(IC))*DHCRIT_LT)/
CB     +(CVT(J,I,CL(IC))*HDIFF))
!             FADJ = ((HDIFF / DHCRIT_TL)**(1.0 / FE_AVG)) *            !TR: 2010 03 19 
!    +               (DHCRIT_TL / HDIFF)
     
!             CV(J, I, CL(IC)) = FADJ*KLAM_CV(J, I, CL(IC))
C
C--BARC**ENDIF STATEMENT FOR IF ( HDIFF.GT.DHCRIT_LT )
!           ENDIF
C
C--BARC**QTURB TO QLAM--------------------------------------------------
!           IF ( HDIFF.LT.DHCRIT_TL ) THEN
!             TURB_FV(J, I, CL(IC)) = 0
!           ENDIF
C
C--BARC**ENDDO FOR J LOOP
!         ENDDO
C
C--BARC**ENDDO FOR I LOOP
!       ENDDO
C
C--BARC***ENDDO FOR IC LOOP
      ENDDO
C
      END SUBROUTINE GWF2CFPM2FM
C
C
C
C
C
      SUBROUTINE EXCHANG_CAL(KPER, KSTP)
C     ******************************************************************
C     CALCULATE THE EXCHANGE BETWEEN CONDUIT AND FISSURED-SYSTEM
C     DEPENDING ON TUBE DIAMETER AND LENGTH: 2*PI * D/2 * L  
C
C     SUBROUTINE EXCHANG_CAL() CALCULATES THE EXCHANGE TERM FOR EXCHANGE
C     WITH MODFLOW. THIS IS EITHER THE INPUT VALUE, OR A DIAMETER-DEPENDENT
C     VALUE DETERMINED IN THE SUBROUTINE.
C     ******************************************************************
C     VERSION 1 3AUG1995   EXCHANG_CAL     &&EPIKARST
C
      USE CONSTANTS, ONLY: PI, TWOPI, DOS, DZ, NEARZERO_30
      USE CFPMODULE, ONLY:NODETOP, GEOHIGHT, NODEBOT, B_MAT, B, HC,     
     +    B_MAT_O, NBR, CON_DATA, MXNODE, LSURFACEDEP, ACTIVENODE, 
     +    NNOD, MOD2, MODD, NODE_LOCA, TORTUOS,  
     +    ONE8TH,CADS_FLG, W_CADS, CADS2, L_NODE, CADS_FLG, L_ABS !TR: 2012 04 25 CADS / B_MAT_O = CONDUIT HEADS FROM PREVIOUS TS; CADS2 SIMILAR TO MOD2 = "CONDUCTANCE" = A/dt // 2013 06 28 // 2014 11 18 L_ABS
      USE GLOBAL, ONLY:IOUT, HNEW
      USE GWFBASMODULE, ONLY:DELT                                       !TR: 2012 04 25 CADS / DELT FOR CADS FLOW
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT, SIN, ASIN
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KPER, KSTP
C
C--LOCAL VARIABLES
      INTEGER I, ITEST, J
      DOUBLE PRECISION ALPHA, X, Y, Z, LENGTH, AF, AT, DKNOT, THETA
      DOUBLE PRECISION NODEWETTD
C-----------------------------------------------------------------------
C
C--INITIALIZE
      ALPHA = DZ
      AF = DZ
      AT = DZ
C 
      DO I = 1, MXNODE
        MOD2(I) = DZ
        IF(CADS_FLG.EQ.1.OR.CADS_FLG.EQ.-1) CADS2(I) = DZ             !TR: 2012 05 25 CADS / 2013 06 28 
        IF ( KPER.EQ.1 .AND. KSTP.EQ.1 ) THEN
          HC(I) = B_MAT(I)
        ELSE
          HC(I) = B(I)
        ENDIF
      ENDDO
C
C--ACTIVATE CADS IN CASE IT WAS PREVIOUSLY DEACTIVATED                  !TR: 2013 02 08 CADS      
      IF (CADS_FLG.EQ.-1) CADS_FLG = 1 
C
C--COMPUTE CADS LENGTH HERE // ONLY FOR THE FIRST TIME                  !TR: 2012 04 25 CADS / COMPUTE LENGTH
      IF(KPER.EQ.1 .AND. KSTP.EQ.1) THEN                                !TR: 2012 04 25 CADS / 
        IF (CADS_FLG.EQ.1) CADS_FLG = -1                                !TR: 2013 02 08 CADS / DEACTIVATE CADS FOR THE FIRST TS
        WRITE (IOUT, *) 'COMPUTE NODE LENGTH'                           !TR: 2012 04 25 CADS / 
        WRITE (IOUT, *) 'NODE I     L_NODE (I)'                         !TR: 2012 04 25 CADS / 
        DO I = 1, MXNODE                                                !TR: 2012 04 25 CADS / 
          DO J = 1, NNOD                                                !TR: 2012 04 25 CADS / THIS LOOP IS COPIED / MODIFIED FROM BELOW AS LENGTH IS NECESSARY FOR CAD STORAGE; TORTUOSITY IS CONSIDERED
            ITEST = NBR(I, J+4+NNOD)
C
C--BARC**NEIGHBOR CHECK
            IF ( ITEST.EQ.0 ) CYCLE
C
C--BARC**DETERMINE LENGTH (L)
            X = DABS(NODE_LOCA(I,2)-NODE_LOCA(NBR(I,J+4),2))
            Y = DABS(NODE_LOCA(I,3)-NODE_LOCA(NBR(I,J+4),3))
            Z = DABS(NODE_LOCA(I,4)-NODE_LOCA(NBR(I,J+4),4))
C
C--BARC**FOR CASES OF Z=0                                               !TR: 2012 05 16 COMPUTE L_NODE
            IF ( Z.LT.NEARZERO_30 ) THEN
              IF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
                L_NODE(I) = L_NODE(I) + Y/DOS * TORTUOS(ITEST)          !TR: 2012 04 25 CADS / 
              ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
                L_NODE(I) = L_NODE(I) + X/DOS * TORTUOS(ITEST)          !TR: 2012 04 25 CADS / 
              ELSE
                L_NODE(I) = L_NODE(I) + (DSQRT(X**2+Y**2)/DOS)      !TR: 2012 04 25 CADS / 
     +                     * TORTUOS(ITEST)                             !TR: 2012 04 25 CADS / 
              ENDIF
C
C--BARC**FOR CASES OF Z>0
            ELSEIF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
              L_NODE(I) = L_NODE(I) + (DSQRT(Y**2+Z**2)/DOS)        !TR: 2012 04 25 CADS / 
     +                     * TORTUOS(ITEST)                             !TR: 2012 04 25 CADS / 
            ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
              L_NODE(I) = L_NODE(I) + (DSQRT(X**2+Z**2)/DOS)        !TR: 2012 04 25 CADS / 
     +                     * TORTUOS(ITEST)                             !TR: 2012 04 25 CADS / 
            ELSE
              L_NODE(I) = L_NODE(I) + Z * TORTUOS(ITEST)                !TR: 2012 04 25 CADS / 
            ENDIF
          ENDDO
          L_ABS = L_ABS+L_NODE(I)                                       !TR: 2014 11 18 L_ABS
          WRITE (IOUT,9001) I, L_NODE(I)                                !TR: 2012 04 25 CADS / 
        ENDDO  
      ENDIF        
C
C--CALCULATE FOR EVERY NODE THE EXCHANGE COEFFICIENT
      DO I = 1, MXNODE
C
C--BARC**NO SURFACE-AREA DEPENDANCE FOR MATRIX EXCHANGE
        IF ( LSURFACEDEP.EQ.0 ) THEN
!          IF (I.EQ.1) WRITE (IOUT, *)                                   !TR: 2012 04 25 JUST DO THIS FOR THE FIRST NODE; 2012 12 11 REMOVED THIS MESSAGE; NOTED IN AR-ROUTINES WHEN LSURFACEDEP IS READ IN
!     +                   'SURFACE-AREA DEPENDENT EXCHANGE IS NOT ACTIVE'
C
C--BARC**SET THE PARTIALLY FULL SCALER TO 1.0 
          AF = 1.0D0
          DO J = 1, NNOD
            ITEST = NBR(I, J+4+NNOD)
            IF ( ITEST.NE.0 ) THEN
              ALPHA = 1.0D0
              NODEBOT(I) = GEOHIGHT(I) - CON_DATA(ITEST, 2)/DOS
C 
              IF (HC(I).LE.NODEBOT(I)) THEN
C
C--BARC**IF MODFLOW HEAD IS <= NODEBOT THEN EXCHANGE IS DZ
                IF ( HNEW(NBR(I,2),NBR(I,3),NBR(I,4)).LE.NODEBOT(I) )   
     +          THEN
                  AF = 0.0D0
                  ALPHA = 0.0D0
                ENDIF
C
C--BARC**ENDIF FOR DRY CONDITION
              ENDIF
C
C--BARC**ENDIF FOR ITEST STATEMENT
            ENDIF
C
C--BARC**ENDDO FOR NNOD LOOP
          ENDDO
C 
C--BARC**ELSE SURFACE-AREA DEPENDENT EXCHANGE IS ACTIVE
C  FOR LENGTH DEPENDENT EXCHANGE TOO
        ELSE
C 
C--ALPHA SURFACEDEPENDENT:-------------------------------------------------
!          IF (I.EQ.1) WRITE (IOUT, *) 'SURFACE-AREA DEPENDENT EXCHANGE I
!     +S ACTIVE'                                                         !TR: 2012 04 25 JUST DO THIS FOR THE FIRST NODE; 2012 12 11 REMOVED THIS MESSAGE; NOTED IN AR-ROUTINES WHEN LSURFACEDEP IS READ IN
          ALPHA = DZ
          AF = DZ
          DO J = 1, NNOD
            ITEST = NBR(I, J+4+NNOD)
C
C--BARC**NEIGHBOR CHECK
            IF ( ITEST.EQ.0 ) CYCLE
            NODETOP(I) = GEOHIGHT(I) + CON_DATA(ITEST, 2)/DOS
            NODEBOT(I) = GEOHIGHT(I) - CON_DATA(ITEST, 2)/DOS
C                                                                      
C--BARC**DETERMINE LENGTH (L)
            X = DABS(NODE_LOCA(I,2)-NODE_LOCA(NBR(I,J+4),2))
            Y = DABS(NODE_LOCA(I,3)-NODE_LOCA(NBR(I,J+4),3))
            Z = DABS(NODE_LOCA(I,4)-NODE_LOCA(NBR(I,J+4),4))
            LENGTH = 1.0D0
C
C--BARC**FOR CASES OF Z=0
            IF ( Z.LT.NEARZERO_30 ) THEN
              IF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
                LENGTH = Y/DOS
              ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
                LENGTH = X/DOS
              ELSE
                LENGTH = DSQRT(X**2+Y**2)/DOS
              ENDIF
C
C--BARC**FOR CASES OF Z>0
            ELSEIF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
              LENGTH = DSQRT(Y**2+Z**2)/DOS
            ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
              LENGTH = DSQRT(X**2+Z**2)/DOS
            ELSE
              LENGTH = Z
            ENDIF
C 
C--BARC**FULLY-SUBMERGED CASES**
            IF ( HNEW(NBR(I,2),NBR(I,3),NBR(I,4)).GE.NODETOP(I) ) THEN
C
C--BARC**FULLY, PARTIALLY, AND DRY CONDUITS GET FULL EXCHANGE BECAUSE CONDUIT
C  IS FULLY SUBMERGED IN MODFLOW CELL.
              AF = 1.0D0
              IF(LSURFACEDEP.EQ.1)ALPHA=PI*LENGTH*TORTUOS(ITEST)+ALPHA
              IF(LSURFACEDEP.EQ.2)ALPHA=LENGTH*TORTUOS(ITEST)+ALPHA
            ENDIF
C 
C--BARC**PARTIALLY-SUBMERGED CASES**
            IF ( HNEW(NBR(I,2),NBR(I,3),NBR(I,4)).LT.NODETOP(I) .AND.   
     +           HNEW(NBR(I,2),NBR(I,3),NBR(I,4)).GT.NODEBOT(I) ) THEN
C 
C--BARC**FULLY-SATURATED CONDUIT
              IF ( HC(I).GE.NODETOP(I) ) THEN
                AF = 1.0D0
                IF(LSURFACEDEP.EQ.1)ALPHA=PI*LENGTH*TORTUOS(ITEST)
     +                                    +ALPHA
                IF(LSURFACEDEP.EQ.2)ALPHA=LENGTH*TORTUOS(ITEST)+ALPHA
              ENDIF
C
C--BARC**PARTIALLY SATURATED CONDUIT
              IF ( HC(I).LT.NODETOP(I) .AND. HC(I).GT.NODEBOT(I) ) THEN
                IF ( HC(I).GE.HNEW(NBR(I,2),NBR(I,3),NBR(I,4)) ) THEN
                  NODEWETTD = HC(I) - NODEBOT(I)
                  DKNOT = CON_DATA(ITEST, 2)
                  THETA = DOS*ASIN                                      
     +                    (DOS*DSQRT((DKNOT/DOS)**2-(NODEWETTD-       
     +                    DKNOT/DOS)**2)/DKNOT)
                  IF ( HC(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA
                  AT = PI*CON_DATA(ITEST, 2)*
     +                 0.5*(LENGTH*TORTUOS(ITEST))
                  AF = ((ONE8TH*(THETA-SIN(THETA))*DKNOT**2*
     +                   0.5*(LENGTH*TORTUOS(ITEST)))/AT)
 
                  IF(LSURFACEDEP.EQ.1)ALPHA=PI*LENGTH*TORTUOS(ITEST)
     +                                      + ALPHA
                  IF(LSURFACEDEP.EQ.2)ALPHA=LENGTH*TORTUOS(ITEST)+ALPHA
C
C--BARC**ELSE MODFLOW HEAD>HC
                ELSE
                  NODEWETTD = HNEW(NBR(I,2), NBR(I,3), NBR(I,4))        
     +                        - NODEBOT(I)
                  DKNOT = CON_DATA(ITEST, 2)
                  THETA = DOS*ASIN                                      
     +                    (DOS*DSQRT((DKNOT/DOS)**2-(NODEWETTD-       
     +                    DKNOT/DOS)**2)/DKNOT)
                  IF ( HC(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA
                  AT = PI*CON_DATA(ITEST, 2)*
     +                 0.5*(LENGTH*TORTUOS(ITEST))
                  AF = ((ONE8TH*(THETA-SIN(THETA))*DKNOT**2*
     +                   0.5*(LENGTH*TORTUOS(ITEST)))/AT)
 
                  IF(LSURFACEDEP.EQ.1)ALPHA=PI*LENGTH*TORTUOS(ITEST)
     +                                      +ALPHA
                  IF(LSURFACEDEP.EQ.2)ALPHA=LENGTH*TORTUOS(ITEST)+ALPHA
C 
C--BARC**ENDIF FOR IF(HC(I).GE.HNEW(NBR(I,2),NBR(I,3),NBR(I,4)) STATEMENT
                ENDIF
C 
C--BARC**ENDIF FOR PARTIALLY-SATURATED CONDUIT**
              ENDIF
C
C--BARC**ENDIF FOR PARTIALLY-SUMBERGED CASES
            ENDIF
C
C--BARC**PERCHED-CONDUIT CASES**
            IF ( HNEW(NBR(I,2),NBR(I,3),NBR(I,4)).LT.NODEBOT(I) ) THEN
C
C--BARC**FULLY-SATURATED CONDUIT**
              IF ( HC(I).GE.NODETOP(I) ) THEN
                AF = 1.0D0
                IF(LSURFACEDEP.EQ.1)ALPHA=PI*LENGTH*TORTUOS(ITEST)
     +                                    +ALPHA
                IF(LSURFACEDEP.EQ.2)ALPHA=LENGTH*TORTUOS(ITEST)+ALPHA  
              ENDIF
C
C--BARC**PARTIALLY-SATURATED CONDUIT**
              IF ( HC(I).LT.NODETOP(I) .AND. HC(I).GT.NODEBOT(I) ) THEN
                NODEWETTD = HC(I) - NODEBOT(I)
                DKNOT = CON_DATA(ITEST, 2)
                THETA = DOS*ASIN                                        
     +                  (DOS*DSQRT((DKNOT/DOS)**2-(NODEWETTD-         
     +                  DKNOT/DOS)**2)/DKNOT)
                IF ( HC(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA
                AT = PI*CON_DATA(ITEST, 2)*
     +               0.5*(LENGTH*TORTUOS(ITEST))
                AF = ((ONE8TH*(THETA-SIN(THETA))*DKNOT**2*
     +                 0.5*(LENGTH*TORTUOS(ITEST)))/AT)
 
                IF(LSURFACEDEP.EQ.1)ALPHA=PI*LENGTH*TORTUOS(ITEST)
     +                                    +ALPHA
                IF(LSURFACEDEP.EQ.2)ALPHA=LENGTH*TORTUOS(ITEST)+ALPHA  
              ENDIF
            ENDIF
C 
C--BARC**ENDDO FOR NEIGHBOR CHECK
          ENDDO
C
C--BARC**ENDIF FOR SURFACE AREA-DEPENDENT MATRIX EXCHANGE
        ENDIF
C 
        MOD2(I) = AF*ALPHA*MODD(I)
C
C--CADS2 COMPUTATION FOR STORAGE
        IF (CADS_FLG.EQ.1) THEN                                         !TR: 2012 04 25 CADS/ 
          IF (HC(I).LE.NODEBOT(I)) THEN                                 !TR: 2012 06 06 CADS/ DRY CASE
            CADS2(I) = 0.0                                              !TR: 2012 06 06 CADS/ DRY CASE
          ELSE
            CADS2(I) = W_CADS(I)*L_NODE(I)/DELT                         !TR: 2012 04 25 CADS/ 
          ENDIF
        ENDIF                                                           !TR: 2012 04 25 CADS/ 
C
C--BARC**ENNDO FOR MXNODE LOOP
      ENDDO
9001  FORMAT(I4, 4X, F9.3)                                              !TR: 2012 04 25 CADS / 
C 
      END SUBROUTINE EXCHANG_CAL
C
C
C
C
C
      SUBROUTINE GWF2CFP1M1FM(KKITER, KKPER, KKSTP, IFLG)
C     **********************************************************************
C     CFPM1 FORMULATE
C
C     SUBROUTINE CONDUIT() CALCULATES CONDUIT FLOW IN CONDUIT-NETWORK.
C
C     IF THERE ARE ONLY LAMINAR TUBES, A LAMINAR PRECALCULATION IS DONE, AND
C     THE HEADS CALCULATED THERE ARE TAKEN AS STARTING VALUES FOR SUBROUTINE
C     ITERA_NEW. THIS LAMIAR CALCULATION INVOLVES SETTING UP OF THE LINEAR
C     EQUATION SYSTEM WITH GLLAM() AND SOLVING IT BY THE ROUTINES LUDCMP() 
C     AND LUBSB(). HEADS FROM THE LAMINAR PRECALCULATION ARE RETURNED IN 
C     VECTOR B_MAT. 
C
C     IF THERE ARE TURBULENT TUBES, THE HEADS FROM THE LAST TIMESTEP,
C     WICH ARE SAVED IN VECTOR HEAD_SAVE, ARE USED AS STARTING HEADS.
C     NEXT TURB_RE2() IS CALLED, WHICH DETERMINES FROM THE HEADS IF THE TUBES
C     ARE LAMINAR-FLOW OR TURBULENT-FLOW.
C
C     CON1FM() DETERMINES THE EXCHANGE FLUX WITH THE MODFLOW-MATRIX AND
C     RESETS HCOF AND RHS FOR THE MODFLOW ITERATION.
C
C     THE CALCULATED HEADS FROM ITERA_NEW ARE STORED IN HEAD_SAVE.
C     **********************************************************************
C
      USE CONSTANTS, ONLY: PI, NEARZERO_30
      USE CFPMODULE, ONLY:MXTUBE, TURB, NBR, B, B_MAT, NU, MOD2, KONV,  
     +    ACTIVENODE, MODD, INDEX1, HEAD_SAVE, G, EPSILON, B_MAT_O,
     +     EQ_MAT, MODE, FHLQ_FLG                              !TR: 2012 05 09 ADDED FHLQ_FLG; FLAG INDICTATING FHLQ IS ACTIVE
C
      USE GLOBAL, ONLY:IOUT
C
      IMPLICIT NONE
      INTRINSIC DABS
C
      EXTERNAL VECNORM, GLLAM, LUDCMP, LUBKSB, VEKTOR_CHANGE, TURB_RE2
      EXTERNAL ITERA_NEW, CON1FM
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KKITER, KKPER, KKSTP, IFLG
      DOUBLE PRECISION TUBELAM, D
C
      IF ( MODE.EQ.2 ) RETURN
C
C--IF ALL TUBES LAMINAR, MAKE A LAMINAR PRECALCULATION. IF A TUBE IS
C  ALREADY TURBULENT, SKIP THIS PRECALCULATION:
      CALL VECNORM(TURB, MXTUBE, TUBELAM)
C 
      IF ( DABS(TUBELAM).LT.NEARZERO_30 ) THEN
        WRITE (IOUT, *) ' LAMINAR PRECALCULATION'
C
C--DETERMINE THE SYSTEM OF EQUATIONS FOR LAMINAR FLOW
        CALL GLLAM
C 
C--SOLVE THE LINEAR EQUATION SYSTEM
C  FOR EXPLANATION SEE PRESS ET AL. CAMBRIDGE PRESS 1992
        CALL LUDCMP(EQ_MAT, ACTIVENODE, ACTIVENODE, INDEX1, D)        
        CALL LUBKSB(EQ_MAT, ACTIVENODE, ACTIVENODE, INDEX1, B_MAT)
C
C--HEADS FROM LAMINAR PRECALCULATION ARE THUS STORED IN B_MAT AND
C  GIVEN TO ITERA_NEW AS STARTING HEADS FOR ITERATION
      ELSE
C 
C--STORE HEADS FROM LAST TIMESTEP IN B_MAT AS STARTING HEADS FOR ITERA_NEW
        CALL VEKTOR_CHANGE(HEAD_SAVE, B_MAT, ACTIVENODE)
      ENDIF
C 
C--DETERMINE FOR EVERY TUBE IF FLOW IS TURBULENT
C  BARC**MODIFIED FOR FREE SURFACE AND DRY
      CALL TURB_RE2
C 
C--SOLVE SYSTEM OF NONLINEAR EQUTIONS BY NEWTON-RAPHSON ITERATION
C  BARC**MODIFIED FOR FREE SURFACE AND DRY
      CALL ITERA_NEW(KKPER, KKSTP, KKITER)                              !TR: 2012 11 01 FHLQ // PASS THROUGH KKPER, KKSTP
C
C--SUBSTRACT FLOW IN CONDUIT SYSTEM FROM MODFLOW IF ITERATION CONVERGED
!     IF ( KONV.EQ.1 ) THEN
      CALL CON1FM                                                       !TR: 2012 07 17 DO THIS ALWAYS - OTHERWISE THE MODFLOW WATER BALANCE IS STRONGLY AFFECTED AS THE TRANSFER BETWEEN CONDUITS AND MATRIX IS EXISTEND EVEN FOR NON-CONVERGED CONDUIT SYSTEMS
C
C--SAVE HEADS AS STARTING HEADS FOR NEXT ITERATION STEP
      CALL VEKTOR_CHANGE(B, HEAD_SAVE, ACTIVENODE)
!     ENDIF
C 
      IF ( KKPER.EQ.1 .AND. KKSTP.EQ.1 ) B_MAT_O = B_MAT
      IF ( IFLG.EQ.2 ) THEN
        IF ( KONV.NE.1 ) THEN
          WRITE (IOUT, *)
          WRITE (IOUT, *) '!WARNING. NO CONVERGENCE IN CONDUIT IN STEP',
     +                    KKSTP, ', PERIOD', KKPER 
        ENDIF
      ENDIF
C
      END SUBROUTINE GWF2CFP1M1FM
C
C
C
C
C
      SUBROUTINE VECNORM(VEC, N, NORM)
C     **********************************************************************
C     --SEBA; CALCULATES NORM OF A VECTOR VEC:
C     NORM = SQRT( SUMM(I): VEC[I]**2)
C     **********************************************************************
C
      IMPLICIT NONE
      INTRINSIC DSQRT
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N
      INTEGER, INTENT(IN) :: VEC(N)
      DOUBLE PRECISION, INTENT(OUT) :: NORM
C
C--LOCAL VARIABLES
      INTEGER I
C---------------------------------------------------------------------------
      NORM = 0.0D0
      DO I = 1, N
        NORM = NORM + VEC(I)**2
      ENDDO
      NORM = DSQRT(NORM)
C
      END SUBROUTINE VECNORM
C
C
C
C
C      
      SUBROUTINE GLLAM
C     **************************************************************
C     SET UP THE SYSTEM OF EQUATIONS FOR LAMINAR FLOW
C     **************************************************************
C     VERSION 2 19JULY1999 GLLAM
C
      USE CONSTANTS, ONLY: PI, DZ,NEARZERO_30
      USE CFPMODULE, ONLY:NBR, CON_DATA, BEGHEAD, B_MAT, NU, EQ_MAT, G,
     +    ACTIVENODE, NNOD, MOD2, NODETOP, MODD, QBDIR,QSDIR,!TR: 2014 02 20 CADS RECHARGE
     +    CADS_FLG, CADS2, B_MAT_O, FHLQ_FLG, QFHLQ, WELL_FLG,
     +    QWELL,CY_FLG,QCYLQ,PFPS2,ICY,CONDCY,HCY,CADSML_FLG,CADSML2    !TR: 2012 04 25 CADS / B_MAT_O = CONDUIT HEADS FROM PREVIOUS TS / TR: 2012 05 11 FHLQ / FHLQ_FLG IF FHLQ BOUNDARY CONDITION IS ACTIVE/2012 06 08 WELL/2013 03 14 CAUCHY/2012 07 12 PFPS // 2013 06 28 CADSML2
      USE GLOBAL, ONLY:IOUT, HNEW, IBOUND
C
C--BARC**EQ_MAT IS STARTING FLOW                                        !TR: 2012 04 24 EQ_MAT IS STARTING FLOW INSTEAD OF MAT?
C--BARC**B_MAT IS STARTING HEAD      
      IMPLICIT NONE
      INTRINSIC DABS
C
C--LOCAL VARIABLES
      DOUBLE PRECISION C
      INTEGER I, J, TUB
C---------------------------------------------------------------------
      DO I = 1, ACTIVENODE
        DO J = 1, ACTIVENODE
          EQ_MAT(I, J) = DZ
        ENDDO
C
C--BARC**TRY SETTING INITIAL HEAD AS ELEVATION OF TOP OF NODE // !TR: B_MAT IS THE RIGHT-HAND-SIDE OF THE LINEAR EQUATION,, FORMULATE SUBSEQUENTLY
        B_MAT(I) = 0.0D0
!TR:    B_MAT(I) = NODETOP(I)
      ENDDO
C 
      C = PI*G/(128.D0*NU)
C
      DO I = 1, ACTIVENODE
C
C--IF NODE HAS A VARIABLE PRESSURE HEAD ASSIGN THE NODE ENTRIES
C  ADD FLOW OF ALL TUBES AT NODE I WHICH ARE ACTIVE
!RSR    IF ( BEGHEAD(I).EQ.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(I)+1.D0).LT.NEARZERO_30 ) THEN
          DO J = 1, NNOD
            IF ( NBR(I,J+4).EQ.0 ) CYCLE
            TUB = NBR(I,J+4+NNOD)
            EQ_MAT(I,NBR(I,J+4))=EQ_MAT(I, NBR(I,J+4))-C                
     +                           *CON_DATA(TUB,2)**4/CON_DATA(TUB,3)
            EQ_MAT(I,I) = EQ_MAT(I,I) - EQ_MAT(I,NBR(I,J+4))
          ENDDO
C 
C--EXCHANGE COMING FROM MODFLOW ONLY IF MODFLOW CELL IS ACTIVE
C  BARC** AND MODFLOW HEAD >= CONDUIT BOTTOM
C  BARC**FREE SURFACE AND DRY CASES ALREADY ACCOUNTED FOR HERE BY EXCHANG_CAL(IOUT)
          IF ( IBOUND(NBR(I,2),NBR(I,3),NBR(I,4)).NE.0 )THEN            !TR: 2012 04 24 IF STATEMENT ENLARGED; !TR: 2012 05 11 TODO: CONSIDER CHD PACKAGE!
            B_MAT(I) = MOD2(I)*HNEW(NBR(I,2),NBR(I,3),NBR(I,4))         !TR: 2012 04 24 CONSIDER B_MAT HERE
            EQ_MAT(I, I) = EQ_MAT(I, I) + MOD2(I)                       
          ENDIF                                                         !TR: 2012 04 24 CLOSE ENLARGED IF STATEMENT
C 
C--DIRECT FLOW INTO CONDUIT SYSTEM COMING FROM RECHARGE
!TR: 2012 04 24 IS THIS MISSED HERE?
          B_MAT(I) = B_MAT(I) + QBDIR(I)                                !TR: 2012 04 24 ADDED DIRECT RECHARGE TO LAMINAR PRECALCULATION
C
C--FLOW FROM CAD STORAGE                                                !TR: 2012 04 25 CADS /
          IF(CADS_FLG.EQ.1) THEN                                        !TR: 2012 04 25 CADS /
C
C--CONSIDER CADS RECHARGE HERE
            IF (B_MAT_O(I).NE.DZ) THEN                                !TR: 2012 04 25 CADS / FOR THE FIRST TS IT IS DZ
              B_MAT(I) = B_MAT(I) + CADS2(I) * B_MAT_O(I) + QSDIR(I)    !TR: 2012 04 25 CADS / 2014 02 20 CADS RECHARGE
              EQ_MAT(I,I) = EQ_MAT(I,I) + CADS2(I)                      !TR: 2012 04 25 CADS /
            ENDIF
          ENDIF
C
C--FLOW FROM CADML STORAGE                                              !TR: 2013 06 28 CADSML
          IF(CADSML_FLG.EQ.1) THEN                                      !TR: 2013 06 28 CADSML
            IF (B_MAT_O(I).NE.DZ) THEN                                !TR: 2013 06 28 CADSML / FOR THE FIRST TS IT IS DZ
              B_MAT(I) = B_MAT(I) + CADSML2(I) * B_MAT_O(I) + QSDIR(I)  !TR: 2013 06 28 CADSML // CADS RECHARGE
              EQ_MAT(I,I) = EQ_MAT(I,I) + CADSML2(I)                    !TR: 2013 06 28 CADSML
            ENDIF
          ENDIF          
C
C--FLOW FROM PFP STORAGE                                                !TR: 2012 07 12 PFPS /
          IF (B_MAT_O(I).NE.DZ) THEN                                  !TR: 2012 07 12 PFPS / FOR THE FIRST TS IT IS DZ
            B_MAT(I) = B_MAT(I) + PFPS2(I) * B_MAT_O(I)                 !TR: 2012 07 12 PFPS /
            EQ_MAT(I,I) = EQ_MAT(I,I) + PFPS2(I)                        !TR: 2012 07 12 PFPS /
          ENDIF      
C
C--FLOW FROM FHLQ                                                       !TR: 2012 05 11 FHLQ /
          IF(FHLQ_FLG) THEN                                             !TR: 2012 05 16 FHLQ /
              B_MAT(I) = B_MAT(I) + QFHLQ(I)                            !TR: 2012 05 11 FHLQ /
          ENDIF                                                         !TR: 2012 05 16 FHLQ
C
C--FLOW FROM WELLS                                                      !TR: 2012 06 08 WELL /
          IF(WELL_FLG) THEN                                             !TR: 2012 06 08 WELL /
              B_MAT(I) = B_MAT(I) + QWELL(I)                            !TR: 2012 06 08 WELL /
          ENDIF                                                         !TR: 2012 06 08 WELL /
C
C--FLOW FROM CAUCHY                                                     !TR: 2013 03 14 CAUCHY
          IF(CY_FLG) THEN                                               !TR: 2013 03 14 CAUCHY
            DO J=1, ACTIVENODE
              IF(ICY(J,1).EQ.I) THEN
                B_MAT(I) = B_MAT(I) + CONDCY(J) * HCY(J)                !TR: 2013 03 15 CAUCHY
                EQ_MAT(I,I) = EQ_MAT(I,I) + CONDCY(J)                   !TR: 2013 03 15 CAUCHY
              ENDIF
            ENDDO  
          ENDIF                                                         !TR: 2013 03 14 CAUCHY
C
C--FLOW FROM CAUCHY LQ                                                  !TR: 2013 03 18 CAUCHY LQ
          IF(CY_FLG) THEN                                               !TR: 2013 03 18 CAUCHY LQ
              B_MAT(I) = B_MAT(I) + QCYLQ(I)                            !TR: 2013 03 18 CAUCHY LQ
          ENDIF            
C          
C--BARC**ELSE STATEMENT FOR IF ( BEGHEAD(I).EQ.-1 ) THEN
        ELSE
C
C--NODE WITH CONSTANT HEADPRESSURE
          EQ_MAT(I, I) = 1.0D0
          B_MAT(I) = BEGHEAD(I)
C 
C--BARC**ENDIF STATEMENT FOR IF ( BEGHEAD(I).EQ.-1 ) THEN
        ENDIF
C 
CBARC**ENDDO FOR I=1,ACTIVENODE LOOP
      ENDDO
C
      END SUBROUTINE GLLAM
C
C
C
C
C
      SUBROUTINE LUDCMP(A, N, NP, INDX, D)
C **************************************************************
C     W. H. PRESS, S. A. TEUKOLSKY, W. T. VETTERLING
C     &  B. P. FLANNERY
C     NUMERICAL RECIPES IN FORTRAN 1992
C **************************************************************
C
      USE CONSTANTS, ONLY:NEARZERO_30, DZ
      IMPLICIT NONE
      INTRINSIC DABS
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N, NP
      DOUBLE PRECISION, INTENT(INOUT) :: A(NP, NP)
      DOUBLE PRECISION, INTENT(OUT) :: INDX(N), D
C
C--LOCAL VARIABLES
      INTEGER I, J, K, IMAX
      DOUBLE PRECISION DUM, AAMAX, TOT, VV(NP)
C---------------------------------------------------------------
      D = 1.D0
      DO I = 1, N
        AAMAX = DZ
        DO J = 1, N
          IF ( DABS(A(I,J)).GT.AAMAX ) AAMAX = DABS(A(I,J))
        ENDDO
C 
        IF ( AAMAX.EQ.DZ ) THEN
C
C--SEBA   PAUSE 'SINGULAR MATRIX.'
          PRINT *, 'SINGULAR MATRIX', I
          STOP
        ENDIF
        VV(I) = 1.D0/AAMAX
      ENDDO
      DO J = 1, N
        IF ( J.GT.1 ) THEN
          DO I = 1, J - 1
            TOT = A(I, J)
            IF ( I.GT.1 ) THEN
              DO K = 1, I - 1
                TOT = TOT - A(I, K)*A(K, J)
              ENDDO
              A(I, J) = TOT
            ENDIF
          ENDDO
        ENDIF
        AAMAX = DZ
        DO I = J, N
          TOT = A(I, J)
          IF ( J.GT.1 ) THEN
            DO K = 1, J - 1
              TOT = TOT - A(I, K)*A(K, J)
            ENDDO
            A(I, J) = TOT
          ENDIF
          DUM = VV(I)*DABS(TOT)
          IF ( DUM.GE.AAMAX ) THEN
            IMAX = I
            AAMAX = DUM
          ENDIF
        ENDDO
        IF ( J.NE.IMAX ) THEN
          DO K = 1, N
            DUM = A(IMAX, K)
            A(IMAX, K) = A(J, K)
            A(J, K) = DUM
          ENDDO
          D = -D
          VV(IMAX) = VV(J)
        ENDIF
        INDX(J) = IMAX
        IF ( J.NE.N ) THEN
          IF ( A(J,J).EQ.DZ ) A(J, J) = NEARZERO_30
          DUM = 1.D0/A(J, J)
          DO I = J + 1, N
            A(I, J) = A(I, J)*DUM
          ENDDO
        ENDIF
      ENDDO
      IF ( A(N,N).EQ.DZ ) A(N, N) = NEARZERO_30
      END SUBROUTINE LUDCMP
C
C
C
C
C
      SUBROUTINE LUBKSB(A, N, NP, INDX, B)
C **************************************************************
C     W. H. PRESS, S. A. TEUKOLSKY, W. T. VETTERLING
C     &  B. P. FLANNERY
C     NUMERICAL RECIPES IN FORTRAN 1992
C **************************************************************
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N, NP
      DOUBLE PRECISION, INTENT(IN) :: INDX(N), A(NP, NP)
      DOUBLE PRECISION, INTENT(INOUT) :: B(N)
C
C--LOCAL VARIABLES
      INTEGER I, II, J, LL
      DOUBLE PRECISION TOT
C---------------------------------------------------------------
      II = 0
      DO I = 1, N
        LL = INDX(I)
        TOT = B(LL)
        B(LL) = B(I)
        IF ( II.NE.0 ) THEN
          DO J = II, I - 1
            TOT = TOT - A(I, J)*B(J)
          ENDDO
        ELSEIF ( TOT.NE.0.D0 ) THEN
          II = I
        ENDIF
        B(I) = TOT
      ENDDO
      DO I = N, 1, -1
        TOT = B(I)
        IF ( I.LT.N ) THEN
          DO J = I + 1, N
            TOT = TOT - A(I, J)*B(J)
          ENDDO
        ENDIF
        B(I) = TOT/A(I, I)
      ENDDO
      END SUBROUTINE LUBKSB
C
C
C
C
C
      SUBROUTINE TURB_RE2
C***********************************************************
C     DISTINGUISH LAMINAR FROM TURBULENT FLOW IN A TUBE
C--SEBA--HYSTERESIS INTRODUCED:
C     IN ORDER TO AVOID FLIPPING OF TUBES FROM LAMINAR TO TURBULENT
C     DURING ITERATION A HYSTERESIS IS INTRODUCED. FOR A GIVEN FLOW
C     VELOCITY LAM.REY IS BIGGER THAN TURB_RE, SO A TUBE JUST AT THE
C     LIMIT OF 2200 WOULD ALTERNATE EACH ITERATION BETWEEN LAMINAR AND
C     TURBULENT. THEREFORE A LAMINAR TUBE SWITCHES TO TURBULENT AT A
C     HIGHER (LAMINAR) REYNOLDS-NUMBER OF 3100, WHILE A TURBULENT TUBE
C     SWITCHES TO LAMINAR AT A (TURBULENT) REYNOLDSNUMBER OF 1400.
C     THIS IS 800 EACH WAY.
C--ELK
C     IN NEWER CFP_MF2005 USER SPECIFIES LOWER AND UPPER CRITICAL 
C     REYNOLDS NUMBER
C***********************************************************
C VERSION 2 20DEZ98 TURB_RE2
C
      USE CONSTANTS, ONLY: TWOPI, PI, DOS, DZ
      USE CFPMODULE, ONLY:MXTUBE, B_MAT, ITUBE, GEOHIGHT, CON_DATA,
     +    ONE8TH, G, NU, TURB, DTUBE, TUB_REYNOLD, FTURB
      USE GLOBAL, ONLY:IOUT
      IMPLICIT NONE
      INTRINSIC ASIN, DSQRT, SIN, DABS
      DOUBLE PRECISION, EXTERNAL :: LAMIN_PF, TURBI_PF, LAMIN, TURBI
C     
C--LOCAL VARIABLES
      DOUBLE PRECISION APF
      DOUBLE PRECISION TUBEHMID, TUBETOP1, TUBETOP2, TUBEBOT1, TUBEBOT2
      DOUBLE PRECISION TUBETOPMID, TUBEBOTMID, TUBEMIDMID, TUBEWETTD
      DOUBLE PRECISION DKNOT, THETA, DEQV, LAMQ, LAM_RE, TURBQ, TURB_RE
      INTEGER I, IC
C-----------------------------------------------------------------------
C
C--CALCULATE FLOW IN ALL TUBES
      DO I = 1, MXTUBE
C
C--BARC**COMPUTE THESE FOR FREE SURFACE CASE
        TUBEHMID = (B_MAT(ITUBE(I,2))+B_MAT(ITUBE(I,3)))/DOS
        TUBETOP1 = GEOHIGHT(ITUBE(I,2)) + CON_DATA(I, 2)/DOS
        TUBETOP2 = GEOHIGHT(ITUBE(I,3)) + CON_DATA(I, 2)/DOS
        TUBEBOT1 = GEOHIGHT(ITUBE(I,2)) - CON_DATA(I, 2)/DOS
        TUBEBOT2 = GEOHIGHT(ITUBE(I,3)) - CON_DATA(I, 2)/DOS
C
        TUBETOPMID = (TUBETOP1+TUBETOP2)/DOS
        TUBEBOTMID = (TUBEBOT1+TUBEBOT2)/DOS
        TUBEMIDMID = (GEOHIGHT(ITUBE(I,2))+GEOHIGHT(ITUBE(I,3)))/DOS
C 
C--BARC**IF FREE SURFACE, YOU NEED EFFECTIVE DIAMETER AND PARTIAL AREA
        IF ( TUBEHMID.LT.TUBETOPMID .AND. TUBEHMID.GT.TUBEBOTMID ) THEN
          WRITE (IOUT, *) I,                                            
     +   'TUBE IN TURB_RE2 CORRECTED FOR PARTIALLY SATURATED CONDITIONS'
C 
          ITUBE(I,4) = 0                                                !TR: 2011 10 25 FLAG INDICATING WETTING STATE
          TUBEWETTD = TUBEHMID - TUBEBOTMID 
          DKNOT = CON_DATA(I, 2)
          THETA = DOS*ASIN                                              
     +            (DOS*DSQRT((DKNOT/DOS)**2-(TUBEWETTD-DKNOT/DOS)     
     +            **2)/DKNOT)
          IF ( TUBEHMID.GE.TUBEMIDMID ) THETA = TWOPI - THETA
          DEQV = DKNOT - DKNOT*(SIN(THETA)/THETA)
          DTUBE(I,3) = DEQV                                             !TR: 2014 07 31 SAVE DEQV IN DTUBE FOR TRANSPORT ROUTINES
C 
C--BARC**ADD THIS
          APF = ONE8TH*(THETA-SIN(THETA))*DEQV**2
C
C--SORT RE-NUMBERS:
          IF ( TURB(I).EQ.0 ) THEN
C
C--IF TUBE WAS LAMINAR CALCULATE LAMINAR REYNOLDSNUMBER AND FLOW RATE
            LAMQ = LAMIN_PF(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)), DEQV, 
     +             CON_DATA(I,3), NU, G, PI, APF) 
            LAM_RE = DABS(4.0D0*LAMQ/(PI*DEQV*NU))
            TUB_REYNOLD(I) = LAM_RE
            DTUBE(I, 1) = LAMQ
C
C--IF LAM. RE IS > TCRITREY TAKE TURB. RE AND SWITCH TUBE TO TURB
C--BARC**MAKE THIS USER-DEFINED
            IF ( LAM_RE.GE.CON_DATA(I,6) ) THEN
              TURBQ = TURBI_PF(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)),    
     +                DEQV, CON_DATA(I,3), CON_DATA(I,4), NU, G, PI,   
     +                APF)
              TURB_RE = DABS(4.D0*TURBQ/(PI*DEQV*NU))
              TUB_REYNOLD(I) = TURB_RE
              TURB(I) = 1
C
C--TUBE(I,4) SWITCHED TO TURBULENT FLOW RATE
              DTUBE(I, 1) = TURBQ
              WRITE (IOUT, 9001) I, LAM_RE, CON_DATA(I, 6)
            ENDIF
C
          ELSE
C
C--IF FLOW WAS TURBULENT TAKE TURB. RE
            TURBQ = TURBI_PF(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)), DEQV,
     +              CON_DATA(I,3), CON_DATA(I,4), NU, G, PI, APF)
            TURB_RE = DABS(4.D0*TURBQ/(PI*DEQV*NU))
            TUB_REYNOLD(I) = TURB_RE
            DTUBE(I, 1) = TURBQ
C
C--BARC**USE USER-DEFINED CRITICAL REYNOLDS #
            IF ( TURB_RE.LE.CON_DATA(I,5) ) THEN
C 
              LAMQ = LAMIN_PF(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)),     
     +               DEQV, CON_DATA(I,3), NU, G, PI, APF)
              LAM_RE = DABS(4.D0*LAMQ/(PI*DEQV*NU))
              TUB_REYNOLD(I) = LAM_RE
              TURB(I) = 0
C
C--TUBE(I,4) SWITCHED FROM TURBULENT TO LAMINAR FLOW RATE
              DTUBE(I, 1) = LAMQ
              WRITE (IOUT, 9002) I, LAM_RE, CON_DATA(I, 5)
            ENDIF
          ENDIF
C
C--DETERMINE IF TURBULENT FLOW EXIST
          FTURB = 0
          DO IC = 1, MXTUBE
            FTURB = TURB(IC) + FTURB
          ENDDO
C
C--BARC**END FOR FREE SURFACE CASES
!        ENDIF
        ELSEIF( TUBEHMID.LE.TUBEBOTMID ) THEN                           !TR: 2014 08 01 
C 
C--BARC**FOR DRY CASE
!        IF ( TUBEHMID.LE.TUBEBOTMID ) THEN
CB        PRINT *, 'DRY', I, TUBEHMID, TUBEBOTMID
          WRITE (IOUT, *) I, 'TUBE IN TURB_RE2 SET DRY '
          ITUBE(I,4) = -1
C 
C--SORT RE-NUMBERS:
          IF ( TURB(I).EQ.0 ) THEN
C
C--IF TUBE WAS LAMINAR CALCULATE LAMINAR REYNOLDSNUMBER AND FLOW RATE
            LAMQ = DZ
            LAM_RE = DZ
            TUB_REYNOLD(I) = LAM_RE
            DTUBE(I, 1) = LAMQ
          ELSE
C
C--IF FLOW WAS TURBULENT TAKE TURB. RE
            TURBQ = DZ
            TURB_RE = DZ
            TUB_REYNOLD(I) = TURB_RE
            DTUBE(I, 1) = TURBQ
C
C--IF TURB_RE < 1600 TAKE LAM_RE AND SWITCH FLOW
            IF ( TURB_RE.LE.CON_DATA(I,5) ) THEN
              LAMQ = DZ
              LAM_RE = DZ
              TUB_REYNOLD(I) = LAM_RE
              TURB(I) = 0
C
C--TUBE(I,4) SWITCHED FROM TURBULENT TO LAMINAR FLOW RATE
              DTUBE(I, 1) = LAMQ
              WRITE (IOUT, 9003) I
            ENDIF
          ENDIF
C
C--DETERMINE IF TURBULENT FLOW EXIST
          FTURB = 0
          DO IC = 1, MXTUBE
            FTURB = TURB(IC) + FTURB
          ENDDO
C
C--BARC**ELSE FOR DRY CASES
        ELSE
C 
C--BARC**FOR SATURATED CASE
C
C--FLAG INDICTATING WETTING
          ITUBE(I,4) = 1                                                !TR: 2011 10 25 
C
C--SORT RE-NUMBERS:
          IF ( TURB(I).EQ.0 ) THEN
C--IF TUBE WAS LAMINAR CALCULATE LAMINAR REYNOLDSNUMBER AND FLOW RATE
            LAMQ = LAMIN(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)),          
     +             CON_DATA(I,2), CON_DATA(I,3), NU, G, PI)
C 
            LAM_RE = DABS(4.D0*LAMQ/(PI*CON_DATA(I,2)*NU))
            TUB_REYNOLD(I) = LAM_RE
            DTUBE(I, 1) = LAMQ
C
C--IF LAM. RE IS > 3000 TAKE TURB. RE AND SWITCH TUBE TO TURB
            IF ( LAM_RE.GE.CON_DATA(I,6) ) THEN
              TURBQ = TURBI(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)),       
     +                CON_DATA(I,2), CON_DATA(I,3), CON_DATA(I,4), NU,  
     +                G, PI)
              TURB_RE = DABS(4.D0*TURBQ/(PI*CON_DATA(I,2)*NU))
              TUB_REYNOLD(I) = TURB_RE
              TURB(I) = 1
C
C--TUBE(I,4) SWITCHED TO TURBULENT FLOW RATE
              DTUBE(I, 1) = TURBQ
              WRITE (IOUT, 9004) I, LAM_RE, CON_DATA(I, 6), TURB_RE     !TR: 2013 03 21 OUTPUT MODIFICATION
            ENDIF
C
          ELSE
C
C--IF FLOW WAS TURBULENT TAKE TURB. RE
            TURBQ = TURBI(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)),         
     +              CON_DATA(I,2), CON_DATA(I,3), CON_DATA(I,4), NU, G, 
     +              PI)
            TURB_RE = DABS(4.D0*TURBQ/(PI*CON_DATA(I,2)*NU))
            TUB_REYNOLD(I) = TURB_RE
            DTUBE(I, 1) = TURBQ
C
C--IF TURB_RE < 1600 TAKE LAM_RE AND SWITCH FLOW
            IF ( TURB_RE.LE.CON_DATA(I,5) ) THEN
              LAMQ = LAMIN(B_MAT(ITUBE(I,2)), B_MAT(ITUBE(I,3)),        
     +               CON_DATA(I,2), CON_DATA(I,3), NU, G, PI)
              LAM_RE = DABS(4.D0*LAMQ/(PI*CON_DATA(I,2)*NU))
              TUB_REYNOLD(I) = LAM_RE
              TURB(I) = 0
C
C--TUBE(I,4) SWITCHED FROM TURBULENT TO LAMINAR FLOW RATE
              DTUBE(I, 1) = LAMQ
              WRITE (IOUT, 9005) I, TURB_RE, CON_DATA(I, 5), LAM_RE     !TR: 2013 03 21 OUTPUT MODIFICATION
            ENDIF
C
C--BARC**ENDIF FOR IF (TURB(I) .EQ. 0) STATEMENT
          ENDIF
C
C--DETERMINE IF TURBULENT FLOW EXIST
          FTURB = 0
          DO IC = 1, MXTUBE
            FTURB = TURB(IC) + FTURB
          ENDDO
C
C--BARC**ENDIF FOR SATURATED CASE
        ENDIF
C 
C--BARC**ENDDO FOR ALL THREE CASES
      ENDDO
C 
 9001 FORMAT ('TUBE', I4, ' LAMREY=>TCRITREY. LAM_RE:', G12.4,          
     +        ' TCRITREY', G12.4)
 9002 FORMAT ('TUBE', I4, ' TURBREY<=LCRITREY LAM_RE:', G12.4,          
     +        ' TURB_RE:', G12.4)
 9003 FORMAT ('TUBE', I4, ' IS DRY')
 9004 FORMAT ('TUBE', I4, ' SWITCH LAM -> TURB: LAM_RE (',G12.4,        !TR: 2013 03 21 OUTPUT MODIFICATION
     +        ') >= TCRITREY:', G12.4,' TURB_RE:', G12.4)
 9005 FORMAT ('TUBE', I4, ' SWITCH TURB -> LAM: TURBREY (',G12.4,       !TR: 2013 03 21 OUTPUT MODIFICATION
     +        ') <= LCRITREY:', G12.4,' LAM_RE:',G12.4)
C 
      END SUBROUTINE TURB_RE2
C
C
C
C
C 
      DOUBLE PRECISION FUNCTION TURBI(HA, HE, D, L, K, NU, G, PI)
C     ***********************************************************
C     CALCULATE TURBULENT FLOW IN A TUBE BY COLEBROOK-WHITE
C     (SEE HORLACHER 1992); OUTPUT: M^3/S
C     ***********************************************************
C     VERSION 1 13APR1995 TURBI
C
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT, DLOG10
C
C--INPUT:
C     HA                  PRESSUREHEAD AT THE BEGIN OF THE TUBE
C     HE                  PRESSUREHEAD AT END 
C     D                   DIAMETER OF THE TUBE
C     L                   LENGTH OF THE TUBE
C     NU                  KINEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C     G                   GRAVITATION CONSTANT
C     K                   ROUGHNESS OF THE TUBE
C--SEBA--IF K < 0 THEN ROUGHNESS = DIAMETER*K
C
C--INTERNALE VARIABLE:
C     SIGNUM              STORE VALUE 1: HA > HE
C                                     0: HA < HE
C     H, Y                STORE INTERNAL VALUES FROM CALCULATION
C
C--OUTPUT:
C     TURBI               FUNCTIONVARIABLE TAKING THE RESULT
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: HA, HE, D, L, K, NU, G, PI
C
C--LOCAL VARIABLES
      DOUBLE PRECISION SIGNUM, Y, H, KK
C-----------------------------------------------------------------------
      IF ( HA.GE.HE ) THEN
        SIGNUM = 1.D0
      ELSE
        SIGNUM = -1.D0
      ENDIF
C
C--KK IS THE CALCULATED ROUGHNESS COEFFICIENT
      KK = K
C
C--SEBA--NEW ROUGHNESS COEFFICIENT:
      IF ( K.LT.0.0D0 ) KK = -1.D0*K*D
C
C--AVOID DIVISION WITH DZ
      H = DABS(HA-HE)
      IF ( H.NE.0.0D0 ) THEN
        Y = DSQRT(H*G*D**5.D0*PI**2/(8.D0*L))
        TURBI = SIGNUM*(-2.D0)                                          
     +          *Y*DLOG10(2.51D0*PI*NU*D/(4.D0*Y)+KK/(3.71D0*D))
C
C--CHECK FOR SIGN OF LOG, THAT CHANGES WITH SMALL DIAMETERS:
        IF ( DLOG10(2.51D0*PI*NU*D/(4.D0*Y)+KK/(3.71D0*D)).GT.0.D0 )    
     +       THEN
          WRITE (*, *) ' WARNING! TURBULENT FLOW HAS WRONG SIGN'
          WRITE (*, 9001) D, L, KK
        ENDIF
      ELSE
C
C--THERE IS NO HEAD DIFFERENCE
        TURBI = 0.D0
      ENDIF
 9001 FORMAT ('DIAM: ', G16.8, ' LENGTH: ', G16.8, ' K: ', G16.8)
      END FUNCTION TURBI
C
C
C
C
C
      DOUBLE PRECISION FUNCTION TURBI_PF(HA, HE, D, L, K, NU, G, PI,APF)
C *****************************************************************
C       CALCULATE TURBULENT FLOW IN A TUBE BY COLEBROOK-WHITE 
C       (SEE HORLACHER 1992); OUTPUT: M^3/S
C--BARC**CORRECTED D FOR FREE SURFACE IN CONDUIT IS PASSED IN HERE.
C--BARC**PARTIAL AREA, A, ALSO PASSED IN HERE
C******************************************************************
C
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT, DLOG10
C
C--INPUT:
C     HA                  PRESSUREHEAD AT THE BEGIN OF THE TUBE
C     HE                  PRESSUREHEAD AT END 
C     D                   DIAMETER OF THE TUBE
C     L                   LENGTH OF THE TUBE
C     NU                  KINEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C     G                   GRAVITATION CONSTANT
C     K                   ROUGHNESS OF THE TUBE
C--SEBA--IF K < 0 THEN ROUGHNESS = DIAMETER*K
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: HA, HE, D, L, K, NU, APF, G, PI
C
C--LOCAL VARIABLES
      DOUBLE PRECISION SIGNUM, Y, H, KK
C
C--INTERNALE VARIABLE:      
C     SIGNUM              STORE VALUE 1: HA > HE
C                                     0: HA < HE
C     H, Y                STORE INTERNAL VALUES FROM CALCULATION
C
C--OUTPUT:
C     TURBI_PF            FUNCTIONVARIABLE TAKING THE RESULT FOR PARTIALLY FULL PIPES
C
      IF ( HA.GE.HE ) THEN
        SIGNUM = 1.D0
      ELSE
        SIGNUM = -1.D0
      ENDIF
C
C--KK IS THE CALCULATED ROUGHNESS COEFFICIENT
      KK = K
C
C--SEBA--NEW ROUGHNESS COEFFICIENT:
      IF ( K.LT.0.D0 ) KK = -1.D0*K*D
C 
C--AVOID DIVISION WITH DZ
      H = DABS(HA-HE)
      IF ( H.NE.0.D0 ) THEN
C 
C--BARC**FOR PARIALLY-FILLED CONDITONS:
        Y = DSQRT((2.D0*H*G*D*APF**2)/L)
        TURBI_PF = SIGNUM*(-2.D0)                                       
     +             *Y*DLOG10(2.51D0*PI*NU*D/(4.D0*Y)+KK/(3.71D0*D))
C
C--CHECK FOR SIGN OF LOG, THAT CHANGES WITH SMALL DIAMETERS:
        IF ( DLOG10(2.51D0*PI*NU*D/(4.D0*Y)+KK/(3.71D0*D)).GT.0.D0 )    
     +  THEN
          WRITE (*, *) ' WARNING! TURBULENT FLOW HAS WRONG SIGN'
          WRITE (*, 9001) D, L, KK
        ENDIF
      ELSE
C
C--THERE IS NO HEAD DIFFERENCE
        TURBI_PF = 0.D0
      ENDIF
 9001 FORMAT ('DIAM: ', G16.8, ' LENGTH: ', G16.8, ' K: ', G16.8)
      END FUNCTION TURBI_PF
C 
C
C
C
C
      DOUBLE PRECISION FUNCTION LAMIN(HA, HE, D, L, NU, G, PI) 
C*******************************************************
C     CALCULATE LAMINAR FLOW IN A TUBE BY HAGEN-POISEUILLE
C             Q = H * G * D**4 * PI / (L * 128 * NU) M^3/S
C***********************************************************
C VERSION 1 13APR1995 LAMIN
C
      IMPLICIT NONE
C
C--INPUT:
C HA          PRESSUREHEAD AT THE BEGIN OF THE TUBE
C HE                  PRESSUREHEAD END 
C D                   DIAMETER OF THE TUBE
C L                   LENGTH OF THE TUBE
C NU                  KINEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C G                   GRAVITATION CONSTANT
C PI                  MATHEMATIC CONSTANT PI
C
C--INTERNALE VARIABLE:
C C             STORE INTERNAL VALUES FROM CALCULATION
C
C--OUTPUT:
C LAMIN       CONTAINS VOLUME OF LAMINAR FLOW
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: HA, HE, D, L, NU, G, PI
C
C--LOCAL VARIABLES
      DOUBLE PRECISION C
C
      C = PI*G/(NU*128.D0)
      LAMIN = C*D**4*(HA-HE)/L
      END FUNCTION LAMIN
C
C
C
C
C 
      DOUBLE PRECISION FUNCTION LAMIN_PF(HA, HE, D, L, NU, G, PI, APF)
C*******************************************************
C     CALCULATE LAMINAR FLOW IN A TUBE BY HAGEN-POISEUILLE
C     Q = H * G * D**4 * PI / (L * 128 * NU) M^3/S
C--BARC**CORRECTED D FOR FREE SURFACE IN CONDUIT IS PASSED IN HERE.
C--BARC**PARTIAL AREA, A, ALSO PASSED IN HERE
C***********************************************************
C
      IMPLICIT NONE
C--INPUT:
C     HA          PRESSUREHEAD AT THE BEGIN OF THE TUBE
C     HE          PRESSUREHEAD        END 
C     D           DIAMETER OF THE TUBE
C     L           LENGTH OF THE TUBE
C     NU          KINEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C     G           GRAVITATION CONSTANT
C     PI          MATHEMATIC CONSTANT PI
C
C--OUTPUT:
C     LAMINPF     CONTAINS VOLUME OF LAMINAR FLOW IN PARTIALLY FULL PIPES
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: HA, HE, D, L, NU, G, PI, APF
C 
C--BARC**RIGHT EQUATION FOR PARTAILLY FULL CONDITIONS
      LAMIN_PF = (D**2*G*(HA-HE)*APF)/(32.D0*NU*L)
C
      END FUNCTION LAMIN_PF
C
C
C
C
C
      SUBROUTINE ITERA_NEW(KKPER, KKSTP, KKITER)                        !TR: 2012 11 01 FHLQ // PASS THROUGH KKPER, KKSTP
C*********************************************************
C     NEWTON-RAPHSON ITERATION TO SOLVE NONLINEAR
C     SYSTEM OF EQUATIONS
C     -F(X_0) = F'(X_0) * (X - X_0)
C--RSR X0 CHANGED TO B_MAT
C--BARC**B_MAT BECOMES X0 IN HERE
C
C     ITERA_NEW() BUILDS AND SOLVES THE EQUATION SYSTEM FOR TURBULENT AND/OR
C     LAMINAR FLOW CONDITIONS BY A NEWTON-RAPHSON ITERATION.
C*********************************************************
C VERSION 2   20DEZ1998    ITERA_NEW
C
      USE CONSTANTS,      ONLY: Z, DZ, TRUE, FALSE
      USE UTIL_INTERFACE, ONLY: NOT_NEAR_ZERO
      USE CFPMODULE, ONLY:FTURB, KONV, EPSILON, ACTIVENODE, INDEX1, B,  
     +    ITERA_MAX, CFPRELAX, P_NR, FX0, EQ_MAT, B_MAT, ITERS, B_MAT_P,!TR: 2012 07 16 ADDED B_MAT_P
     +    FHLQ_FLG, FHLQ_FLIP, CY_FLG, CY_FLIP, LH_FLG, LH_FLIP,        !TR: 2012 11 01 ADDED FHLQ_FLG; FLAG INDICTATING FHLQ IS ACTIVE; FHLQ_FLIP; 2013 03 14 CAUCHY
     +    CADS2, CADSML2, CADSML_FLG                                    !TR: 2013 06 28 
C
C--BARC**ADD ROF FOR OVERLAND FLOW
      USE CFPMODULE, ONLY: DTUBE,MXTUBE,NBR,NNOD,MXNODE,QTUBIN,
     +    ITUBE,QTUB  !,ROF
C
      USE GLOBAL, ONLY:IOUT, ISSFLG
      IMPLICIT NONE    
C
C--SEBA--SUBROUTINE MPROVE INCLUDED, WICH SHOULD IMPROVE CALCULATION
C      OF X BY LUDCMP AND LUBKSB. SEE PRESS ET AL. 1992, P 47FF
C      NEW VARIABLES FOR MPROVE, TO SAVE ORIGINAL A AND B:
C
C--INTERNALE VARIABLES:
C     HELP        STORE INTERNAL VALUE
C     FX0         FLOW BUDGET FOR EVERY NODE USED IN THE NEWTON-RAPHSON
C                 ITERATION (FX0 - SEE HEAD OF THE SUBROUTINE)
C     B           THE NEW HEAD VALUES AFTER THE LAST CALCULATION
C                 NORM VARIABLE STORE RESULT OF FUNCTION 'NORM'
C     EQ_MAT      STORE THE MATRIX OF THE EQUATIONSYSTEM SOLVED
C                 THE FUNCTIONS 'LUDCMP' AND 'LUBKSB'
C     INDEX1      DUE FOR THE FUNCTIONS 'LUDCMP' & 'LUBKSB'
C     J
C     FTURB       FLAG IF TURBULENT FLOW EXIST - 1
C                                      ELSE    - 0
C     FHLQ_FLIP   COUNTER TO PREVENT OSCILLATION DUE TO FLIPPING BETWEEN FH
C                 AND LQ; IF 10 IS EXCEEDED, THE NODE IS SET TO LQ; IF 20 IS
C                 EXCEEDED, THE NODE IS SET TO FH
C
C--OUTPUT:
C     S           COUNTER OF ITERATIONSTEPS
C     KONV        FLAG: 1 - ITERATION CONVERGE
C                       0 - NO CONVERGENCE 
C TUB_REYNOLD VECTOR CONTAINS THE REYNOLD NUMBER FOR EVERY TUBE      
C
      EXTERNAL VEC_SET, VEKTOR_CHANGE, F_X0, FX0_ABL, VEC_SKAL, LUDCMP,
     + LUBKSB, MPROVE, VEC_ADD, TURB_RE2, PFPS2_COMP, CONFHLQ1, CONCY1, !TR: 2012 11 01 FHLQ / 2013 03 14 CAUCHY
     + CONLH1                                                           !TR: 2013 03 23 LH
      DOUBLE PRECISION, EXTERNAL :: MAX_NORM
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KKITER, KKPER, KKSTP                       !TR: 2012 11 01 FHLQ // KKPER, KKSTP
C
C--LOCAL VARIABLES
      DOUBLE PRECISION D
C
C--BARC**ADD THIS FOR OVERLAND FLOW
      DOUBLE PRECISION QT
C
      !DOUBLE PRECISION MAT_A(ACTIVENODE, ACTIVENODE), VEC_B(ACTIVENODE)
      DOUBLE PRECISION, ALLOCATABLE :: MAT_A(:, :), VEC_B(:)
      DOUBLE PRECISION OSC(5),OSCSUM5, OSCSUM5_P,OSCSUM4, OSCSUM4_P     !TR: 2013 07 05 OSC // OSC = ARRAY WITH HEAD CHANGE, OSCSUM = SUM OF CRITERIAS, OSCSUM_P = PREVIOUS VALUE OF OSCSUM
      INTEGER I,II,III                                                  !TR: 2013 07 05 OSC
      LOGICAL OSCFLG                                                    !TR: 2013 07 05 OSC
C
C--BARC**ADD THIS FOR OVERLAND FLOW
      INTEGER N,T,BNOD,ENOD
C-----------------------------------------------------------------------
C
C--INI
      ALLOCATE(MAT_A(ACTIVENODE, ACTIVENODE), VEC_B(ACTIVENODE))
      KONV = Z
      OSC = DZ                                                           !TR: 2013 07 05 OSC
      OSCFLG = FALSE                                                    !TR: 2013 07 05 OSC
      OSCSUM5 = DZ                                                       !TR: 2013 07 08 OSC
      OSCSUM5_P = DZ                                                     !TR: 2013 07 08 OSC
      OSCSUM4 = DZ                                                       !TR: 2013 07 08 OSC
      OSCSUM4_P = DZ                                                    !TR: 2013 07 08 OSC      
      III = Z                                                           !TR: 2013 07 05 OSC
C
C--RESET FHLQ / CY FLIP COUNTER      
      DO I = 1, ACTIVENODE                                              !TR: 2012 11 01 FHLQ
        FHLQ_FLIP(I) = Z                                                !TR: 2012 11 01 FHLQ
        CY_FLIP(I) = Z                                                  !TR: 2013 03 14 CAUCHY
        LH_FLIP(I) = Z
      ENDDO                                                             !TR: 2012 11 01 FHLQ
      CALL RESET_LH
C
C--BARC**MAKE LABELS FOR NR CONVERGENCE TABLE
      IF ( P_NR.EQ.1 ) WRITE (IOUT, '(A)')                              
     +'MODFLOW ITER.#  NEWTON-RAPHSON ITER.#   MAXIMUM NODE HEAD CHANGE'
C 
C--BARC**NR CONVERGENCE INFORMATION NOT DESIRED.
C--!RSR  IF ( P_NR.EQ.0 ) CONTINUE
      DO I = 1, ITERA_MAX
        ITERS = I            
C
C--FHLQ AND CAUCHY BC HERE
        IF (FHLQ_FLG) CALL CONFHLQ1(KKPER, KKSTP, ITERS,OSCFLG)         !TR: 2012 11 01 FHLQ // 2013 07 09 OSC
        IF (CY_FLG) CALL CONCY1(KKPER, KKSTP, ITERS)                    !TR: 2013 03 14 CAUCHY
        IF (LH_FLG) CALL CONLH1(KKPER, KKSTP, ITERS)                    !TR: 2013 03 14 CAUCHY        
C
C--RECOMPUTE PFPS2 TO CONSIDER PARTIALLY FILLED CONDUITS 2/2
        IF (ISSFLG(KKPER).NE.1)THEN
          CALL PFPS2_COMP                                               !TR: 2012 07 16 PFPS
          IF(CADSML_FLG.EQ.1) CALL CADSML2_COMP(2)                      !TR: 2013 06 27 CADSML
        ENDIF 
C
C--CALCULATE THE FLOW BUDGET OF EVERY NODE.
C  OUTPUT: 'FX0' FOR NEWTON-RAPHSON
        CALL F_X0
C 
C--DESIGNATE THE DERIVATION 'F'(X_0)
        CALL FX0_ABL
C
C--CALCULATE '(-1) * FX0'
        CALL VEC_SKAL(FX0, ACTIVENODE, -1.D0)
C
C--INITIALICE INDEX1 = 0
        CALL VEC_SET(INDEX1, ACTIVENODE, 0.D0)
C 
C--SOLUTION OF THE LINEAR EQUATION SYSTEM (SEE PRESS 1992)
C--SAVE ORIGINAL EQ_MAT IN MAT_A AND FX0 IN VEC_B FOR MPROVE:
        CALL MATRIX_CHANGE(EQ_MAT, MAT_A, ACTIVENODE, ACTIVENODE)
        CALL VEKTOR_CHANGE(FX0, VEC_B, ACTIVENODE)
C 
C--SOLVE BY LU-DECOMPOSITION:
        CALL LUDCMP(EQ_MAT, ACTIVENODE, ACTIVENODE, INDEX1, D)
        CALL LUBKSB(EQ_MAT, ACTIVENODE, ACTIVENODE, INDEX1, FX0)
C 
C--CALL MPROVE TO IMPROVE CALCULATION:
        CALL MPROVE(MAT_A, EQ_MAT, ACTIVENODE, ACTIVENODE, INDEX1,      
     +              VEC_B, FX0)

C
C--CALCULATE 'RLAX * 0.05 * FX0'C // CONSIDER OSCILLATION FOR CFPRELAX
        IF(OSCFLG)THEN                                                  !TR: 2013 07 08 OSC
          CALL VEC_SKAL(FX0, ACTIVENODE, CFPRELAX*0.05)                 !TR: 2013 07 08 OSC
          WRITE(IOUT,'(A40)')'RELAX REDUCED BY 95% DUE TO OSCILLATIONS' !TR: 2013 07 08 OSC
        ELSE
          CALL VEC_SKAL(FX0, ACTIVENODE, CFPRELAX)               
        ENDIF     
C
C--CALCULATE 'B = X_N +
        CALL VEC_ADD(FX0, B_MAT, B, ACTIVENODE)
C
C--DETERMINE LAMINAR AND TURBULENT FLOW
        CALL TURB_RE2
C 
C--BARC**NR CONVERGENCE INFORMATION DESIRED.
        IF ( P_NR.EQ.1 ) WRITE (IOUT, '(I10,I20,F30.10)') KKITER, ITERS,
     +                          MAX_NORM(B, B_MAT, ACTIVENODE)
C
C--CONSIDER OSCILLATION
        III = III+1                                                     !TR: 2013 07 08 OSC
        OSC(III)= MAX_NORM(B, B_MAT, ACTIVENODE)                        !TR: 2013 07 08 OSC
        IF (III.EQ.5) THEN                                              !TR: 2013 07 08 OSC
C
C--SAVE OSCSUM5 AS OSCSUM5_P AND RECOMPUTE OSCSUM5        
          OSCSUM5_P = OSCSUM5                                           !TR: 2013 07 08 OSC
          OSCSUM5 = OSC(1)+OSC(2)+OSC(3)+OSC(4)+OSC(5)                  !TR: 2013 07 08 OSC
C
C--SAVE OSCSUM4 AS OSCSUM4_P AND RECOMPUTE OSCSUM4        
          OSCSUM4_P = OSCSUM4                                           !TR: 2013 07 08 OSC
          OSCSUM4 = OSC(1)+OSC(2)+OSC(3)+OSC(4)                         !TR: 2013 07 08 OSC
          IF(NOT_NEAR_ZERO(OSCSUM5_P)) THEN
             IF (((OSCSUM5/OSCSUM5_P).GT.0.99).AND.                        !TR: 2013 07 08 OSC
     +          ((OSCSUM5/OSCSUM5_P).LE.1.01))                             !TR: 2013 07 08 OSC
     +          OSCFLG =TRUE                                             !TR: 2013 07 08 OSC
          END IF
          IF(NOT_NEAR_ZERO(OSCSUM4_P)) THEN
             IF (((OSCSUM4/OSCSUM4_P).GT.0.99).AND.                        !TR: 2013 07 08 OSC
     +          ((OSCSUM4/OSCSUM4_P).LE.1.01))                             !TR: 2013 07 08 OSC
     +          OSCFLG =TRUE                                             !TR: 2013 07 08 OSC                                           !TR: 2013 07 08 OSC
          END IF
          IF (OSCSUM5.GT.10000000)OSCFLG=TRUE                         !TR: 2013 08 01 OSC CONSIDER DIVERGENCE HERE
          III = 0                                                       !TR: 2013 07 08 OSC
        ENDIF                                                           !TR: 2013 07 08 OSC
C 
C--BARC**NR CONVERGENCE INFORMATION NOT DESIRED.
!RSR    IF ( P_NR.EQ.0 ) CONTINUE
C
C--IF ITERATION CONVERGES, THEN STOP ITERATION AND GOTO LABEL NR. 112
        IF ( MAX_NORM(B,B_MAT,ACTIVENODE).LE.EPSILON .AND.              
     +       (ITERS.GT.2 .OR. FTURB.EQ.0) ) THEN
          KONV = 1
          EXIT
        ENDIF
C
C--STORE HEADS FROM PREVIOUS ITERATION IN VECTOR B_MAT_P
        CALL VEKTOR_CHANGE(B_MAT, B_MAT_P, ACTIVENODE)                  !TR: 2012 07 17 PFPS
C
C--STORE NEW CALCULATED HEADS IN VECTOR B_MAT (X0)
        CALL VEKTOR_CHANGE(B, B_MAT, ACTIVENODE)  
      ENDDO
C
C--BARC**COMPUTE OVERLAND FLOW FOR GSFLOW; GO THROUGH ALL NODES
      DO N = 1, MXNODE
CB      ROF(N) = DZ
	  QTUBIN(N) = DZ
        DO T = 1, NNOD
          IF ( NBR(N,T+4+NNOD).NE.0 ) THEN
            QT = DTUBE(NBR(N,T+4+NNOD), 1)
            BNOD = ITUBE(NBR(N,T+4+NNOD), 2)
            ENOD = ITUBE(NBR(N,T+4+NNOD), 3)
            IF ( QT.LT.DZ .AND. N.EQ.BNOD ) THEN
              QTUB(N, T) = -QT
              QTUBIN(N) = QTUBIN(N) + QTUB(N, T)
            ELSEIF ( QT.GT.DZ .AND. N.EQ.ENOD ) THEN
              QTUB(N, T) = QT
              QTUBIN(N) = QTUBIN(N) + QTUB(N, T)
            ENDIF
          ENDIF
        ENDDO
C--BARC**SET ROF
CB       ROF(N) = QTUBIN(N)
CB       PRINT*,N, ROF(N)
      ENDDO
C
      DEALLOCATE(MAT_A, VEC_B, STAT=I)
C
      END SUBROUTINE ITERA_NEW
C
C
C
C
C
      SUBROUTINE F_X0
C **********************************************************************
C     CALCULATE F(X_0) FOR THE NEWTON-RAPHSON EQUATION:
C             -F(X_0) = F'(X_0)(X - X_0)
C     APPLICATION OF THE KIRCHHOFF RULE WITH 
C             Q_1 + Q_2 + Q_3 + Q_4 + MOD_EX + Q_DIR  = 0
C     WITH Q_I FLOW THROUGH THE I.-TUBE
C     IT WILL BE DISTINGUISHED IN TURBULENT AND LAMINAR WITH 'TURB'
C **********************************************************************
C VERSION 2  20DEZ1998 F_X0            &&EPIKARST
C
      USE CONSTANTS, ONLY: PI,DZ, NEARZERO_30
      USE CFPMODULE, ONLY:NNOD, G, NU, TURB, NBR, CON_DATA, B_MAT, 
     +    ACTIVENODE, BEGHEAD, QBDIR, FX0, MOD2, B_MAT_O,
     +    CADS_FLG, CADS2,FHLQ_FLG, QFHLQ, WELL_FLG, QWELL, PFPS2,      !TR: 2012 04 25 CADS; B_MAT_O = CONDUIT HEADS FROM PREVIOUS TS
     +    CY_FLG, QCYLQ, CONDCY, HCY, ICY, CADSML_FLG, CADSML2,         !TR: 2012 05 11 FHLQ; 2012 06 08 WELL; 2012 07 12 PFPS // 2013 03 14 CAUCHY
     +    FHLQ_FLIP, QSDIR                                              !TR: 2013 08 01 FHLQ_FLIP TO SET FIXED HEAD // 2014 02 20 CADS RECHARGE
     
      USE GLOBAL, ONLY:IOUT, HNEW, IBOUND
C
      IMPLICIT NONE
      INTRINSIC DABS
      DOUBLE PRECISION, EXTERNAL :: TURBI, LAMIN
      EXTERNAL VEC_SET
C
C--INTERNALE VARIABLES:
C     TURBI     VARIABLE AVAIL FOR FUNCTION TURBI
C     LAMIN     VARIABLE AVAIL FOR FUNCTION LAMIN
C     I, J      COUNTER
C     TUB       STORE ACTUAL TUBE = NBR(I,J+5)
C
C--OUTPUT:
C     FX0       NEW HEADS OF THE CALCULATED FLOW
C 
C--DATA COMING FROM MAINPROGRAM MODFLOW:
C     NCOL      IS THE NUMBER OF COLUMNS
C     NROW      IS THE NUMBER OF ROWS
C     NLAY      IS THE NUMBER OF LAYS
C     IBOUND    BOUNDARY ARRAY STATUS OF EACH CELL
C               < 0, CONSTANT-HEAD CELL
C               = 0, INACTIVE CELL
C               > 0, VARIABLE-HEAD CELL
C     LCHNEW    DIMENSION (NCOL,NROW,NLAY), MOST RECENT ESTIMATE OF
C               HEAD IN EACH CELL. HNEW CHANGES AT EACH ITERATION
C
C--LOCAL VARIABLES
      INTEGER I, J, TUB,I_LH
      DOUBLE PRECISION HELP
C----------------------------------------------------------------------
C
C--INITIALIZE FX0 (= 0)
      CALL VEC_SET(FX0, ACTIVENODE, 0.D0)
C
      DO I = 1, ACTIVENODE
!        IF (LH_FLG)THEN                                               !TR: 2013 03 14 CAUCHY
!          I_LH = 0
!          DO J=1, ACTIVENODE                                          !TR: 2013 03 14 CAUCHY
!            IF(ILH(J,1).EQ.(NBR(I,1)).AND.ILH(J,2).EQ.1)
!            I_LH = J
!          ENDDO                                                       !TR: 2013 03 14 CAUCHY
!        ENDIF                                                         !TR: 2013 03 14 CAUCHY
C
C--IF I.-NODE HAS VARIABLE HEAD
!RSR    IF ( BEGHEAD(I).EQ.-1.D0 ) THEN
C
C--IF NOT IS LIMITED HEAD BOUNDARY
!        IF (I_LH.GT.0)THEN
!          FX0(NBR(I,1))=B_MAT(NBR(I,1))-HLH(I_LH)
!        ELSE
          IF ( DABS(BEGHEAD(I)+1.D0).LT.NEARZERO_30 ) THEN
C
C--IF I.-NODE HAS A NEIGHBOUR
            DO J = 1, NNOD
              IF ( NBR(I,J+4).EQ.0 ) CYCLE
              TUB = NBR(I, J+4+NNOD)
C 
C--IF FLOW IS LAMINAR
              IF ( TURB(TUB).EQ.0 ) THEN
                HELP = LAMIN(B_MAT(NBR(I,1)), B_MAT(NBR(I,J+4)),          
     +                 CON_DATA(TUB,2), CON_DATA(TUB,3), NU, G, PI)
                FX0(NBR(I,1)) = FX0(NBR(I,1)) + HELP 
              ELSE
C
C--IF FLOW IS TURBULENT
                HELP = TURBI(B_MAT(NBR(I,1)), B_MAT(NBR(I,J+4)),          
     +                CON_DATA(TUB,2), CON_DATA(TUB,3), CON_DATA(TUB,4), 
     +                NU, G, PI)
                FX0(NBR(I,1)) = FX0(NBR(I,1)) + HELP
              ENDIF
            ENDDO
C
C--ADD EXCHANGE COMING FROM MODFLOW ONLY IF MODFLOW CELL IS ACTIVE
C
CB        FX0(NBR(I,1)) = FX0(NBR(I,1)) + AMODD(NBR(I,1)) *
CB   +             (B_MAT(NBR(I,1))- HNEW(NBR(I,2),NBR(I,3),NBR(I,4)))
C 
            IF (IBOUND(NBR(I,2),NBR(I,3),NBR(I,4)).NE.0)                  
     +       FX0(NBR(I,1))=FX0(NBR(I,1))+MOD2(NBR(I,1))*(B_MAT(NBR(I,1)) 
     +                     -HNEW(NBR(I,2),NBR(I,3),NBR(I,4)))
C
C--ADD INFLOW FROM CAD STORAGE                                          !TR: 2012 04 25 CADS /
            IF (CADS_FLG.EQ.1) THEN                                     !TR: 2012 04 25 CADS /
              IF (B_MAT_O(I).NE.DZ)                                   !TR: 2012 04 25 CADS / FOR THE FIRST TS IT IS DZ
     +          FX0(NBR(I,1))=FX0(NBR(I,1))+CADS2(NBR(I,1))             !TR: 2012 04 25 CADS /
     +                        *(B_MAT(NBR(I,1))-B_MAT_O(NBR(I,1)))      !TR: 2012 04 25 CADS /
            ENDIF                                                       !TR: 2012 04 25 CADS /
C
C--ADD INFLOW FROM CADML STORAGE                                        !TR: 2012 04 25 CADS /
            IF (CADSML_FLG.EQ.1) THEN                                   !TR: 2012 04 25 CADS /
              IF (B_MAT_O(I).NE.DZ)                                   !TR: 2012 04 25 CADS / FOR THE FIRST TS IT IS DZ
     +          FX0(NBR(I,1))=FX0(NBR(I,1))+CADSML2(NBR(I,1))           !TR: 2012 04 25 CADS /
     +                        *(B_MAT(NBR(I,1))-B_MAT_O(NBR(I,1)))      !TR: 2012 04 25 CADS /
            ENDIF                                                       !TR: 2012 04 25 CADS /            
C
C--ADD INFLOW FROM PFP STORAGE                                          !TR: 2012 07 12 PFPS /
            IF (B_MAT_O(I).NE.DZ)                                     !TR: 2012 07 12 PFPS / FOR THE FIRST TS IT IS DZ
     +        FX0(NBR(I,1))=FX0(NBR(I,1))+PFPS2(NBR(I,1))               !TR: 2012 07 12 PFPS /
     +                      *(B_MAT(NBR(I,1))-B_MAT_O(NBR(I,1)))        !TR: 2012 07 12 PFPS /
C
C--ADD INFLOW IN THE CONDUIT SYSTEM COMING FROM CADS RECHARGE
            IF((CADS_FLG.NE.0).OR.(CADSML_FLG.NE.0))                    !TR: 2014 04 09 
     +      FX0(NBR(I,1))=FX0(NBR(I,1))-QSDIR(NBR(I,1))
C
C--ADD INFLOW IN THE CONDUIT SYSTEM COMING FROM RECHARGE
            FX0(NBR(I,1))=FX0(NBR(I,1))-QBDIR(NBR(I,1))
C
C--ADD INFLOW IN THE CONDUIT SYSTEM COMING FROM FHLQ (LQ ONLY)          !TR: 2012 05 11 FHLQ /
            IF (FHLQ_FLG) FX0(NBR(I,1))=FX0(NBR(I,1))-QFHLQ(NBR(I,1))     !TR: 2012 05 11 FHLQ /
C
C--ADD INFLOW IN THE CONDUIT SYSTEM COMING FROM WELLS                   !TR: 2012 06 08 WELL /
            IF (WELL_FLG)FX0(NBR(I,1))=FX0(NBR(I,1))-QWELL(NBR(I,1))      !TR: 2012 06 08 WELL /
C
C--ADD INFLOW IN THE CONDUIT SYSTEM COMING FROM CAUCHY BC               !TR: 2013 03 14 CAUCHY
            IF (CY_FLG)THEN                                               !TR: 2013 03 14 CAUCHY
              DO J=1, ACTIVENODE                                          !TR: 2013 03 14 CAUCHY
                IF(ICY(J,1).EQ.(NBR(I,1))) FX0(NBR(I,1)) = FX0(NBR(I,1))  !TR: 2013 03 14 CAUCHY
     +          + CONDCY(J)*(B_MAT(NBR(I,1))-HCY(J))                      !TR: 2013 03 14 CAUCHY
              ENDDO                                                       !TR: 2013 03 14 CAUCHY
            ENDIF                                                         !TR: 2013 03 14 CAUCHY
C
C--ADD INFLOW IN THE CONDUIT SYSTEM COMING FROM CAUCHY LQ               !TR: 2013 03 18 CAUCHY LQ
            IF (CY_FLG) FX0(NBR(I,1))=FX0(NBR(I,1))-QCYLQ(NBR(I,1))     !TR: 2013 03 18 CAUCHY LQ
C                    
          ELSE
C
C--IF I.-NODE HAS CONSTANT HEAD
            FX0(NBR(I,1))=B_MAT(NBR(I,1))-BEGHEAD(NBR(I,1))
          ENDIF
!        ENDIF
C
      ENDDO
C
      END SUBROUTINE F_X0
C
C
C
C
C
      SUBROUTINE VEC_SET(VEC, N, WERT)
C     **********************************************************************
C     SET VECTOR 'VEC' AT THE VALUE 'WERT'
C     **********************************************************************
C     VERSION 14APR1995   VEC_SET
C
      IMPLICIT NONE
C
C--INPUT:      
C     N             SIZE OF THE VECTORS
C     WERT          VALUE TO WICH THE VECTOR WILL BE SET
C
C--OUTPUT:
C     VEC          VECTOR WHICH WILL SET AT ALL POSITION TO WERT
C
C--INTERNAL VARIABLE:
C     I           INDEX
C
C--ARGUMENTS
      INTEGER N
      DOUBLE PRECISION, INTENT(IN) :: WERT
      DOUBLE PRECISION, INTENT(OUT) :: VEC(N)
C
C--LOCAL VARIABLES
      INTEGER I
C----------------------------------------------------------------------------
      DO I = 1, N
        VEC(I) = WERT
      ENDDO
C      
      END SUBROUTINE VEC_SET
C
C
C
C
C
      SUBROUTINE CON1FM
C     ********************************************************
C     SUBSTRACT FLOW IN CONDUIT FROM MODFLOW HEADS (RHS)
C     ********************************************************
C     VERSION 1 18APR1995   CON1FM
C
      USE CFPMODULE, ONLY:NBR, ACTIVENODE, MXNODE, B, MOD2, MODD
      USE GLOBAL, ONLY:IOUT, IBOUND, RHS, HCOF
      IMPLICIT NONE
C
C--LOCAL VARIABLES
      INTEGER IR, IC, IL, I      
C--------------------------------------------------------------
C
C--IF THERE ARE NODES
      IF ( MXNODE.GT.0 ) THEN
C      
C--PROCESS EACH NODE IN THE NODE LIST
        DO I = 1, ACTIVENODE
          IC = NBR(I, 2)
          IR = NBR(I, 3)
          IL = NBR(I, 4)
C
C--NODE IS DECLARED IN THE MODFLOW FIELD
          IF ( (IR.NE.0) .AND. (IC.NE.0) ) THEN
C
C--SUBSTRACT ONLY IF CELL IS NOT INACTIVE
            IF ( IBOUND(IC,IR,IL).NE.0 ) THEN
C
C--CALCULATE EXCHANGE FLOW, IF CELL IS NOT INACTIVE AND NOT CONSTANT
C  HEAD THEN SUBSTACT FLOW INTO THE CONDUIT SYSTEM
              IF ( IBOUND(IC,IR,IL).GT.0 ) THEN
CB              RHS(IC, IR, IL) = RHS(IC, IR, IL) - MODD(I)*B(I)
C
C--BARC**NEED TO USE CORRECTED EXCHANGE HERE
                RHS(IC, IR, IL) = RHS(IC, IR, IL) - MOD2(I)*B(I)
C
C--SEBA--NEW TERM FOR HCOF:
CB              HCOF(IC, IR, IL) = HCOF(IC, IR, IL) - MODD(I)
C
C--BARC**NEED TO USE CORRECTED EXCHANGE HERE
                HCOF(IC, IR, IL) = HCOF(IC, IR, IL) - MOD2(I)
C 
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      END SUBROUTINE CON1FM
C
C
C
C
C
      SUBROUTINE FX0_ABL
C     *****************************************************************
C     CALCULATE THE DERIVATION OF F(X_0) USED BY THE NEWTON-RAPHSON
C     ITERATION

C     -F(X_0) = F'(X_0)(X - X_0)
C     
C     THIS SUBROUTINE USES THE FUNCTIONS: LAMI_ABL  TURB_ABL,
C     WHICH ARE ASSIGNED ABOVE
C     ****************************************************************
C     VERSION 2  20DEZ1998   FX0_ABL
C
      USE CONSTANTS, ONLY: PI, DZ, NEARZERO_30
      USE CFPMODULE, ONLY:NNOD, G, NU, TURB, NBR, CON_DATA, B_MAT, 
     +    ACTIVENODE, BEGHEAD, EQ_MAT, MOD2,            
     +    CADS_FLG, CADS2, PFPS2, CY_FLG, ICY, CONDCY, CADSML_FLG, 
     +    CADSML2                                                       !TR: 2012 04 25 CADS / 2012 07 12 PFPS / 2013 03 15 CAUCHY
C
      USE GLOBAL, ONLY:IBOUND
C
      IMPLICIT NONE
      INTRINSIC DABS
      DOUBLE PRECISION, EXTERNAL :: TURB_ABL, LAMI_ABL
C
C--INTERNAL VARIABLES:
C     TURB_ABL   VARIABLE STORE RESULT OF FUNCTION 'TURB_ABL'
C     LAMI_ABL   VARIABLE STORE RESULT OF FUNCTION 'LAMI_ABL'
C     HELP       INTERNAL VARIABLE
C     I, J       INDEX
C     TUB        STORE ACTIVE TUBE = NBR(I,J+5)        
C
C--LOCAL VARIABLES
      INTEGER I, J, TUB
      DOUBLE PRECISION HELP
CB    DOUBLE PRECISION  AMODD(MXNODE)
C------------------------------------------------------------------------
C
C--INITIALICE MATRIX 'EQ_MAT'
      DO I = 1, ACTIVENODE
        DO J = 1, ACTIVENODE
          EQ_MAT(J, I) = DZ
        ENDDO
      ENDDO
      TUB = 0
C
      DO I = 1, ACTIVENODE
C
C--IF I.-NODE HAS CONSTANT HEAD PRESSURE
!RSR    IF ( BEGHEAD(I).NE.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(I)+1.D0).GT.NEARZERO_30 ) THEN
          EQ_MAT(I, I) = 1.D0
        ELSE
C        
C--IF I.-NODE HAS VARIABLE HEAD PRESSURE THEN ASSIGN IN MATRIX 'EQ_MAT'
          DO J = 1, NNOD
            IF ( NBR(I,J+4).NE.0 ) THEN
C            
C--RECORD VALUES BELONG TO EVERY NEIGHBOURNODE
              TUB = NBR(I, J+4+NNOD) 
              IF ( TURB(TUB).EQ.0 ) THEN
C
C--IF FLOW IS LAMINAR
                HELP = LAMI_ABL(PI, G, NU, CON_DATA(TUB,2),            
     +                 CON_DATA(TUB,3))
                EQ_MAT(I, I) = HELP + EQ_MAT(I, I)
                EQ_MAT(I, NBR(I,J+4)) = -HELP
              ELSE
C 
C--IF FLOW IS TURBULENT
                HELP = TURB_ABL(B_MAT(I), B_MAT(NBR(I,J+4)), G, PI,    
     +                 CON_DATA(TUB,2), CON_DATA(TUB,3), CON_DATA(TUB,4)
     +                 , NU)
                EQ_MAT(I, I) = HELP + EQ_MAT(I, I)
                EQ_MAT(I, NBR(I,J+4)) = -HELP
              ENDIF
            ENDIF
          ENDDO
C
C--ENTRY FOR EXCHANGE WITH MODFLOW
          IF ( IBOUND(NBR(I,2),NBR(I,3),NBR(I,4)).NE.0 ) EQ_MAT(I, I)   
     +         = EQ_MAT(I, I) + MOD2(I)
CB     1      EQ_MAT(I,I) = EQ_MAT(I,I) + AMODD(I)
C
C--BARC**ENDIF FOR BEGHEAD STATEMENT
C
C--ENTRY FOR CADS                                                       !TR: 2012 04 25 CADS /
          IF (CADS_FLG.EQ.1) EQ_MAT(I, I)= EQ_MAT(I, I) + CADS2(I)      !TR: 2012 04 25 CADS /
          IF (CADSML_FLG.EQ.1) EQ_MAT(I, I)= EQ_MAT(I, I) + CADSML2(I)  !TR: 2012 04 25 CADS /
        ENDIF
C
C--ENTRY FOR PFPS
        EQ_MAT(I, I) = EQ_MAT(I, I) + PFPS2(I)                          !TR: 2012 07 12 PFPS /
C
C--ENTRY CAUCHY                                                         
          IF (CY_FLG) THEN                                              !TR: 2013 03 18 CAUCHY
            DO J=1, ACTIVENODE                                          !TR: 2013 03 18 CAUCHY
              IF(ICY(J,1).EQ.(I)) EQ_MAT(I, I) = EQ_MAT(I, I)+CONDCY(J) !TR: 2013 03 18 CAUCHY
            ENDDO                                                       !TR: 2013 03 18 CAUCHY
          ENDIF                                                         !TR: 2013 03 18 CAUCHY
C 
C--BARC**ENDDO FOR MXNODE LOOP
      ENDDO
C
      END SUBROUTINE FX0_ABL
C
C
C
C
C
      DOUBLE PRECISION FUNCTION TURB_ABL(HA, HE, G, PI, D, L, K, NU)
C***********************************************************
C     FUNCTION TO CALCULATE THE DERIVATION OF F(X_0)
C     FOR TURBULENT FLOW
C     APPLICABLE FOR THE NEWTON-RAPHSON ITERATION 
C
C     CALLED BY SUBROUTINE: FX0_ABL
C
C     -F(X_0) = F'(X_0)(X - X_0)
C--BARC**FREE SURFACE DEQV IS PASSED IN HERE
C***********************************************************
C VERSION 1  14APR1995    TURB_ABL
C
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT, DLOG10
C
C--INPUT:
C HA          PRESSUREHEAD AT THE BEGIN OF THE TUBE
C HE          PRESSUREHEAD END 
C G           CONSTANT OF GRAVITATION
C PI          THE MATHEMATICAL CONSTANT PI
C D           DIAMETER OF THE TUBE
C L           LENGTH OF THE TUBE
C K           ROUGHNESS OF THE TUBE. IF K > 0 THEN K = -K*D
C NU          DYNEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C
C--OUTPUT:
C TURB_ABL - FUNCTION VARIABLE TAKING THE RESULT
C
C--INTERNAL VARIABLE 
C X           STORE DIFFERENCE OF HEADPRESSURES
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: G, PI, D, L, K, NU, HA, HE
C
C--LOCAL VARIABLES
      DOUBLE PRECISION BETA, GAMMA, DELTA, TEST, KK, X
C
C--SEBA--NEW ROUGHNESS COEFFICIENT
      KK = K
      IF ( K.LT.0.D0 ) KK = -1.D0*K*D
C
      X = DABS(HA-HE)
C
C--AVOID DIVISION WITH DZ
      IF ( X.NE.0.D0 ) THEN
C
C--SEBA--CORRECT NEW CALCULATION:
        BETA = (G*PI*PI*D**5.D0)/(8.D0*L)
        GAMMA = (2.51D0*PI*D*NU)/4.D0
        DELTA = KK/(3.71D0*D)
        TEST = (-1.D0)*DSQRT(BETA/X)*DLOG10(GAMMA/(DSQRT(BETA*X))+DELTA)
     +         + (GAMMA/X)*DLOG10(DEXP(1.D0))                           
     +         *(GAMMA/(DSQRT(BETA*X))+DELTA)**(-1.D0)
        TURB_ABL = TEST
C
      ELSE
C
C--THERE IS NO HEAD DIFFERENCE
        TURB_ABL = 0.D0
      ENDIF
C      
      END FUNCTION TURB_ABL
C
C
C
C
C
      DOUBLE PRECISION FUNCTION LAMI_ABL(PI, G, NU, D, L)
C***********************************************************
C     FUNCTION TO CALCULATE THE DERIVATION OF F(X_0)
C     FOR LAMINAR FLOW
C     USED FOR THE NEWTON-RAPHSON ITERATION
C     CALLED BY SUBROUTINE: FX0_ABL
C--BARC**FREE SURFACE DEQV IS PASSED IN HERE
C***********************************************************
C VERSION 1  14APR1995    LAMI_ABL
C
      IMPLICIT NONE
C
C--INPUT:
C G     CONSTANT OF GRAVITATION
C PI    THE MATHEMATICAL CONSTANT PI
C D     DIAMETER OF THE TUBE
C L     LENGTH OF THE TUBE
C NU    DYNEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C
C--OUTPUT:
C LAMI_ABL - FUNCTION VARIABLE TAKING THE RESULT
C
C--ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: PI, G, NU, D, L
C
      LAMI_ABL = PI*G*D**4/(128.D0*L*NU)
      END FUNCTION LAMI_ABL
C
C
C
C
C
      SUBROUTINE VEC_SKAL(VECTOR, N, SKALAR)
C **************************************************************
C     SKALAR VECTOR MULTIPLICATION 
C **************************************************************
C VERSION 1    14APR1995   VEC_SKAL
C
C--INPUT:
      IMPLICIT NONE
C N           DIMENSION OF THE VECTOR
C SKALAR      SKALAR TO MULTIPLY WITH VECTOR
C
C--OUTPUT:
C VECTOR      RESULT VECTOR AFTER MULTIPLICATION
C
C--INTERNAL VARIABLES:
C I           INDEX
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: SKALAR
      DOUBLE PRECISION, INTENT(INOUT) :: VECTOR(N)
C
C--LOCAL VARIABLES
      INTEGER I
C
      DO I = 1, N
        VECTOR(I) = SKALAR*VECTOR(I)
      ENDDO
      END SUBROUTINE VEC_SKAL
C
C
C
C
C
      SUBROUTINE MATRIX_CHANGE(MAT1, MAT2, N1, N2)
C     ********************************************************      
C     COPY MATRIX MAT1(N1,N2) INTO MAT2(N1,N2)
C     ********************************************************
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N1, N2
      DOUBLE PRECISION, INTENT(IN) :: MAT1(N1, N2)
      DOUBLE PRECISION, INTENT(OUT) :: MAT2(N1, N2)
C
C--LOCAL VARIABLES
      INTEGER I, J
C--------------------------------------------------------------
      DO I = 1, N1
        DO J = 1, N2
          MAT2(I, J) = MAT1(I, J)
        ENDDO
      ENDDO
      END SUBROUTINE MATRIX_CHANGE
C
C
C
C
C
      SUBROUTINE VEKTOR_CHANGE(VEK1, VEK2, N)
C     *********************************************************
C     STORE 'VEK1' IN 'VEK2' 
C     *********************************************************
C     VERSION 1   14APR1995    VEKTOR_CHANGE
      IMPLICIT NONE
C 
C--INPUT:
C     N           DIMENSION OF THE VECTORS
C     VEK1        VECTOR WICH WILL BE STORED IN VEK2
C
C--OUTPUT:
C     VEK2        OUTPUT VECTOR
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: VEK1(N)
      DOUBLE PRECISION, INTENT(OUT) :: VEK2(N)
C 
C--INTERNAL VARIABLES:
C     I           INDEX      
C
C--LOCAL VARIABLES
      INTEGER I
C----------------------------------------------------------------
      DO I = 1, N
        VEK2(I) = VEK1(I)
      ENDDO
C      
      END SUBROUTINE VEKTOR_CHANGE
C
C
C
C
C
      SUBROUTINE MPROVE(A, ALUD, N, NP, INDX, B, X)
C     ****************************************************************
C     W. H. PRESS ET AL. NUMERICAL RECIPES IN FORTRAN 1992, P 47FF
C     ****************************************************************
      IMPLICIT NONE
      EXTERNAL LUBKSB
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N, NP
      DOUBLE PRECISION, INTENT(IN) :: INDX(N), A(NP, NP), ALUD(NP, NP)
      DOUBLE PRECISION, INTENT(IN) :: B(N)
      DOUBLE PRECISION, INTENT(INOUT) :: X(N)
C
C--LOCAL VARIABLES
      INTEGER I, J
      DOUBLE PRECISION R(NP), SDP
C---------------------------------------------------------------------
C
C--CALCULATE RIGHT HAND SIDE:
      DO I = 1, N
        SDP = -B(I)
        DO J = 1, N
          SDP = SDP + A(I, J)*X(J)
        ENDDO
        R(I) = SDP
      ENDDO
C
C--SOLVE FOR ERROR TERM AND SUBSTRACT FROM OLD SOLUTION:
      CALL LUBKSB(ALUD, N, NP, INDX, R)
      DO I = 1, N
        X(I) = X(I) - R(I)
      ENDDO
C      
      END SUBROUTINE MPROVE
C
C
C
C
C
      SUBROUTINE VEC_ADD(MAT1, MAT2, MAT3, N)
C     ********************************************************
C     ADD 'MAT1' TO 'MAT2' AND STORE THE RESULT IN 'MAT3'
C     ********************************************************
C     VERSION 1   14APR1995    VEC_ADD
C
      IMPLICIT NONE
C
C--INPUT:
C     N             SIZE OF THE VECTORS
C     MAT1, MAT2    INPUT VECTORS WHICH WILL BE ADDED
C
C--OUTPUT:
C     MAT3          RESULT OF VECTOR ADDITION
C
C--INTERNAL VARIABLES:
C     I             INDEX
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: MAT1(N), MAT2(N)
      DOUBLE PRECISION, INTENT(OUT) :: MAT3(N)
C
C--LOCAL VARIABLES
      INTEGER I
C--------------------------------------------------------------
      DO I = 1, N
        MAT3(I) = MAT1(I) + MAT2(I)
      ENDDO
      END SUBROUTINE VEC_ADD
C
C
C
C
C
      DOUBLE PRECISION FUNCTION MAX_NORM(VEC1, VEC2, N)
C     ********************************************************* 
C     MAXIMUMNORM OF DOS VECTORS
C     *********************************************************
C     VERSION 1   14APR1995    NORM
      IMPLICIT NONE
C
C--INPUT:
C     N             SIZE OF THE VECTORS
C     VEC1, VEC2    VECTORS
C
C--OUTPUT:
C     MAX_NORM      RESULT OF THE NORM
C
C--INTERNAL VARIABLES:
C     I             INDEX      
C
      INTRINSIC DABS
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: VEC1(N), VEC2(N)
C
C--LOCAL VARIABLES
      INTEGER I
C----------------------------------------------------------------
      MAX_NORM = 0.0D0
      DO I = 1, N
        IF ( MAX_NORM.LT.DABS(VEC1(I)-VEC2(I)) )                        
     +       MAX_NORM = DABS(VEC1(I)-VEC2(I))
      ENDDO
      END FUNCTION MAX_NORM
C                                                                       !TR: 2012 05 09 SUBSEQUENT LINES ARE CODE ADDITIONS
C
C
C
C
      SUBROUTINE CONFHLQ1(KPER,KSTP,KITER,OSCFLG)
C     *****************************************************************
C     RESET CONDUIT NODE TO FHLQ BOUNDARY:
C     IF Q < Q_LQ -> H(NODE) = H_FH
C     ELSE        -> Q = Q_LQ
C     THIS IS A FIXED HEAD BC FOR SMALL Q AND A CONSTANT FLOW BC FOR 
C     LARGE Q.
C     *****************************************************************
C     ORIGIN: CAVE SRC // 15.1.1999  VERSION 1; BY SEBASTIAN BAUER 2002
C     ADAPTED AND MODIFIED: 2012 11 02 BY THOMAS REIMANN
C
      USE CONSTANTS, ONLY: TRUE, FALSE
      USE CFPMODULE, ONLY: MXNODE, MXTUBE, QBDIR, QBDIRSAVE, BEGHEAD,
     +  QFIX, NNOD, Q_FH_LQ, QFHLQ, B_MAT, DFHLQ,IFHLQ, FHLQ_FLIP, 
     +  CADSML_FLG                                                      !TR: 2012 11 01 FHLQ_FLIP // 2013 07 05 CADSML
      USE GLOBAL, ONLY: IOUT
C      
      IMPLICIT NONE
C
C--ARGUMENTS      
C
C IFHLQ(I,1)    NODENUMBER OF FHLQ BOUNDARY
C IFHLQ(I,2)    0= FIXED HEAD USED, 1= LIMITING FLOW USED
C DFHLQ(I,1)    FIXED HEAD OF THIS NODE (M)
C DFHLQ(I,2)    LIMITING FLOW OF THIS NODE (M**3/S)
C FHLQCOUNT     COUNTER FOR ACTIVE FHLQ BC'S, GIVE OUTPUT MESSAGE
C Q             FLOW IN NODE
C I_FHLQ        COUNTER, IF NODE IS FHLQ BC, THIS IS THE NODE NUMBER
C I_NODE        COUNTER
C J             COUNTER
C II            COUNTER
C QFHLQ         FLOW IN FHLQ BC IF RESTRICTED (LQ FLOW)
C Q_FH_LQ       FLOW IN FHLQ (= QFIX IF NOT RESTRICTED AND QFHLQ IF RESTRICTED)
C FLIPCRIT      DEFINES HOW OFTEN THE BOUNDARY CONDITION CAN SWITCH / 
C               HENCE, THIS DEFINES WHAT FINALLY IS USED (FH OR LQ)
C
      INTEGER KPER,KSTP,KITER,FHLQCOUNT,J,I_FHLQ,I_NODE,II,FLIPCRIT     !TR: 2013 07 05 ADDED FLIPCRIT
      DOUBLE PRECISION Q, H
      LOGICAL OSCFLG                                                    !TR: 2013 07 09 OSC
C
C--SET FLIPCRIT / IF OSCILLATIONS OCCUR, FLIPCRIT CAN BE MODIFIED    
      FLIPCRIT = 31                                                     !TR: 2013 07 05 CADSML
      IF (CADSML_FLG.EQ.1) FLIPCRIT = 81                                !TR: 2013 07 05 CADSML // 2013 07 09 SET TO 81
C
C--SKIP FOR FIRST TIMESTEP
      IF (((KPER.EQ.1).AND.(KSTP.EQ.1)).AND.(KITER.EQ.1)) RETURN     
C
C--RESET COUNTER FOR OUTPUT MESSAGE     
      FHLQCOUNT = 0
C
C--GET QFIX FOR THE FIRST TIMESTEP OF AN PERIOD                               
      CALL QFIX_INTERMEDIATE                                            !TR: 2012 05 11 FHLQ / PROOF QFIX_INTERMEDIATE 
C
C--INITIALIZE Q_FH_LQ      
      DO J=1, MXNODE
        Q_FH_LQ(J) = 0.0
      ENDDO
C
      DO I_NODE=1, MXNODE
        I_FHLQ = 0
C
C--LOOK, IF CONDUIT NODE IS FHLQ-BOUNDARY
        DO J=1, MXNODE
          IF(IFHLQ(J,1).EQ.I_NODE) I_FHLQ = J
        ENDDO     
C
C--IF I_NODE IS FHLQ BOUNDARY
        IF(I_FHLQ.NE.0) THEN
C
C--GET FIXED HEAD FLOW / QFIX > 0: FLOW FROM FIXED HEAD INTO NODE. 
          Q = QFIX(I_NODE)
          H = B_MAT(I_NODE)
C
C--IF NODE IS FIXED HEAD
          IF(IFHLQ(I_FHLQ,2).EQ.0) THEN
C         
C--IF FLOW FROM FIXED HEAD IS LOWER THAN LIMITING FLOW KEEP FIXED HEAD
            IF(Q.LE.DFHLQ(I_FHLQ,2)) THEN                               !TR: 2012 11 20 //2013 08 06 CHANGED EXPRESSION TO .LE.
              BEGHEAD(I_NODE) = DFHLQ(I_FHLQ,1)
              QFHLQ(I_NODE) = 0.0
              Q_FH_LQ(I_NODE) = Q            
C
C--FLOW IS GREATER THAN LIMITING FLOW: SWITCH TO LQ-FLOW              
            ELSEIF (FHLQ_FLIP(I_NODE).LE.FLIPCRIT)THEN                  !TR: ALLOW SWITCH TO LQ 10 TIMES FOR INITIALLY LQ NODES (I.E. IF FLIPCRIT = 31)
              BEGHEAD(I_NODE) = -1
              QFHLQ(I_NODE) = DFHLQ(I_FHLQ,2)
              IFHLQ(I_FHLQ,2) = 1
              Q_FH_LQ(I_NODE) = DFHLQ(I_FHLQ,2)
              IF (FHLQ_FLIP(I_NODE).GE.62) OSCFLG = TRUE              !TR: 2013 07 09 CONSIDER OSCILLATION FOR CADSML HERE (AFTER 40 SWITCHES SET OSCFLG AND ALLOW ANOTHER "= SWITCHES)
              FHLQ_FLIP(I_NODE) = FHLQ_FLIP(I_NODE) + 1                 !TR: 2012 11 01 INCREASE FLIP COUNTER
              WRITE(IOUT,9001) I_NODE, Q, DFHLQ(I_FHLQ,2)
            ENDIF
C
C--IF NODE IS LIMITING FLOW
          ELSE IF(IFHLQ(I_FHLQ,2).EQ.1) THEN
C
C--SET FLIPCOUNTER FOR INITIALLY LQ NODES
            IF (KITER.EQ.1) FHLQ_FLIP(I_NODE) = 21                      !TR: 2012 11 01 SET FHLQ_FLIP TO 21 IF THE BC IS LQ
C
C--IF HEAD IS LESS THAN FIXED HEAD KEEP LQ            
            IF(H.LT.DFHLQ(I_FHLQ,1)) THEN                               !TR: 2012 11 20 
              BEGHEAD(I_NODE) = -1
              QFHLQ(I_NODE) = DFHLQ(I_FHLQ,2)              
              Q_FH_LQ(I_NODE) = DFHLQ(I_FHLQ,2)
C
C--IF HEAD IS GREATER THEN FIXED HEAD, SWITCH TO FH                
            ELSEIF ((FHLQ_FLIP(I_NODE).LE.10).OR.                       !TR: ALLOW SWITCH TO FH 10 TIMES FOR INITIALLY FH NODES
     +              (FHLQ_FLIP(I_NODE).GE.21)) THEN          
              BEGHEAD(I_NODE) =  DFHLQ(I_FHLQ,1)
              IFHLQ(I_FHLQ,2) = 0
              QFHLQ(I_NODE) = 0.0              
              Q_FH_LQ(I_NODE) = 0.0
              FHLQ_FLIP(I_NODE) = FHLQ_FLIP(I_NODE) + 1                 !TR: 2012 11 01
              WRITE(IOUT,9002) I_NODE, DFHLQ(I_FHLQ,1)
C
C--IF SWITCH TO FH IS NOT ALLOWED ANYMORE BUT HEAD SIGNIFICANTLY
C  INCREASES, SWITCH ON FINAL TIME TO FH
            ELSEIF(FHLQ_FLIP(I_NODE).EQ.11) THEN                        !TR: 2013 08 02 FHLQ OPTIMIZED
              IF(H.GT.(DFHLQ(I_FHLQ,1)*100.0)) FHLQ_FLIP(I_NODE)        !TR: 2013 08 02 FHLQ OPTIMIZED
     +        = FHLQ_FLIP(I_NODE)-1                                     !TR: 2013 08 02 FHLQ OPTIMIZED
            ENDIF                                           
          ENDIF
        ENDIF
      ENDDO
C
C--UPDATE QBDIRSAVE TO ACCOUNT FOR LQ FLOW
      DO II=1, MXNODE
         QBDIRSAVE(II) = QBDIR(II)
         IF(FHLQ_FLIP(II).GT.0.AND.FHLQ_FLIP(II).NE.21.)
     +      FHLQCOUNT = FHLQCOUNT + 1                                   !TR: 2012 11 01 COUNT ALL CHANGED FHLQ BOUNDARIES
      ENDDO
!C
!C--MESSAGES
!      IF(FHLQCOUNT.NE.0) THEN
!        WRITE(IOUT,9003) FHLQCOUNT, KPER
!      ENDIF
C
 9001 FORMAT(4X,'SWITCH NODE',I3,' WITH Q ',G14.6, ' TO LQ-FLOW WITH '
     +,G14.6)
 9002 FORMAT(4X,'RESET NODE ',I4,' FROM LQ TO FH WITH HEAD',2G14.6) 
 9003 FORMAT(4X,'CHANGED',I5,' CONDUIT NODE(S) DUE TO FHLQ IN STRESS', 
     +       ' PERIOD ',I6) 
C
      RETURN
      END
C
C
C
C
C
      SUBROUTINE CONCY1(KPER,KSTP,KITER)
C     *****************************************************************
C     CAUCHY BOUNDARY WITH (OPTIONAL) LIMITED FLOW (E.G. IF HEAD DROPS 
C     BELOW A RIVERBED)
C     IF Q < Q_CYLQ -> Q(NODE) = CONDCY(H_NODE - HCY)
C     ELSE          -> Q = Q_CYLQ
C     *****************************************************************
C     
C     LAST CHANGE 2013 03 14 BY THOMAS REIMANN
C
      USE CFPMODULE, ONLY: MXNODE, MXTUBE, QBDIR, QBDIRSAVE, BEGHEAD,
     + QFIX, NNOD, HCY, CONDCY, CONDCYS, CY_FLIP, QCYLQ, ICY, B_MAT, 
     + CYLQ     
      USE GLOBAL, ONLY: IOUT
C      
      IMPLICIT NONE
C
C--ARGUMENTS      
C
C ICY(I,1)      NODENUMBER OF CAUCHY BOUNDARY
C ICY(I,2)      0 = CAUCHY FLOW USED, 1 = LIMITING FLOW USED
C HCY           CAUCHY HEAD OF THIS NODE (M)
C CONDCY        CAUCHY CONDUCTANCE OF THIS NODE (M**2/S)
C CYCOUNT       COUNTER FOR ACTIVE FHLQ BC'S, GIVE OUTPUT MESSAGE
C Q             FLOW IN NODE
C I_FHLQ        COUNTER, IF NODE IS FHLQ BC, THIS IS THE NODE NUMBER
C I_NODE        COUNTER
C J             COUNTER
C II            COUNTER
C CYLQ          FLOW IN FHLQ BC IF RESTRICTED (LQ FLOW)
C Q_FH_LQ       FLOW IN FHLQ (= QFIX IF NOT RESTRICTED AND QFHLQ IF RESTRICTED)
C
      INTEGER KPER, KSTP, KITER, CYCOUNT, J, I_CY, I_NODE, II
      DOUBLE PRECISION Q, H
C
C--SKIP FOR FIRST TIMESTEP
      IF (((KPER.EQ.1).AND.(KSTP.EQ.1)).AND.(KITER.EQ.1)) RETURN
C
C--RESET COUNTER FOR OUTPUT MESSAGE     
      CYCOUNT = 0
C
C--INITIALIZE QCYLQ      
      DO J=1, MXNODE
        QCYLQ(J) = 0.0       
      ENDDO
C
C--LOOK, IF CONDUIT NODE IS CAUCHY-BOUNDARY
      DO I_NODE=1, MXNODE
        I_CY = 0
        DO J=1, MXNODE
          IF(ICY(J,1).EQ.I_NODE) THEN
            I_CY = J
          ENDIF
        ENDDO     
C
C--IF I_NODE IS CAUCHY BOUNDARY
        IF(I_CY.NE.0) THEN
C
C--COMPUTE CAUCHY FLOW AT I_NODE
C  Q > 0: FLOW FROM CAUCHY INTO NODE. 
          H = B_MAT(I_NODE)
          IF(ICY(I_CY,2).EQ.0) Q = -CONDCY(I_CY)*(H-HCY(I_CY))          !TR: 2013 03 18 IF CAUCHY FLOW
          IF(ICY(I_CY,2).EQ.1) Q = -CONDCYS(I_CY)*(H-HCY(I_CY))         !TR: 2013 03 18 IF CAUCHY LQ FLOW
C
C--IF NODE IS CAUCHY
          IF(ICY(I_CY,2).EQ.0) THEN           
C
C--IF FLOW IS GREATER THAN LIMITING FLOW: SWITCH TO LQ-FLOW              
            IF (Q.GT.CYLQ(I_CY).AND.CY_FLIP(I_NODE).LE.31)THEN          !TR: ALLOW SWITCH TO LQ 10 TIMES FOR INITIALLY LQ NODES
              ICY(I_CY,2) = 1                                           !TR: 2013 03 18 SET LQ FLAG
              QCYLQ(I_NODE) = CYLQ(I_CY)                                !TR: 2013 03 18 SET QCYLQ
              CY_FLIP(I_NODE) = CY_FLIP(I_NODE) + 1                     !TR: 2012 11 01 INCREASE FLIP COUNTER
              CONDCYS(I_CY) = CONDCY(I_CY)                              !TR: 2013 03 18 SAVE CONDCY
              CONDCY(I_CY) = 0.0                                        !TR: 2013 03 18 SET CONDCY TO DZ (I.E. NO MORE CAUCHY FLOW)
              WRITE(IOUT,9001) I_NODE, Q, CYLQ(I_CY)
            ENDIF
C
C--IF NODE IS CAUCHY LQ
          ELSE IF(ICY(I_CY,2).EQ.1) THEN
C
C--SET FLIPCOUNTER FOR INITIALLY LQ NODES
            IF (KITER.EQ.1) CY_FLIP(I_NODE) = 21                        !TR: 2012 11 01 SET FHLQ_FLIP TO 21 IF THE BC IS LQ
C
C--IF CAUCHY FLOW IS LARGER THEN LQ KEEP LQ            
            IF(Q.GT.CYLQ(I_CY)) THEN                                    !TR: 2012 11 20 
              QCYLQ(I_NODE) = CYLQ(I_CY)
C
C--IF CAUCHY FLOW SMALLER THEN LQ, SWITCH TO CAUCHY FLOW                
            ELSEIF ((CY_FLIP(I_NODE).LE.10).OR.                         !TR: ALLOW SWITCH TO FH 10 TIMES FOR INITIALLY FH NODES
     +              (CY_FLIP(I_NODE).GE.21)) THEN          
              ICY(I_CY,2) = 0                                           !TR: 2013 03 18 SET FLAG TO CAUCHY
              QCYLQ(I_NODE) = 0                                         !TR: 2013 03 18 SET CAUCHY LQ FLOW TO DZ
              CONDCY(I_CY) = CONDCYS(I_CY)                              !TR: 2013 03 18 SET CONDCY (I.E. RE-ACTIVATE CAUCHY FLOW)
              CY_FLIP(I_NODE) = CY_FLIP(I_NODE) + 1                     !TR: 2012 11 01 
              WRITE(IOUT,9002) I_NODE, Q
            ELSE                                                        !TR: IF SOLUTION OSCILLATES, KEEP LQ FLOW
              IF (H.LT.HCY(I_CY))QCYLQ(I_NODE) = CYLQ(I_CY)             !TR: NOT SURE ABOUT THIS - TODO: CHECK
              IF (H.GE.HCY(I_CY))QCYLQ(I_NODE) = Q                      !TR: NOT SURE ABOUT THIS - TODO: CHECK
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C--UPDATE QBDIRSAVE TO ACCOUNT FOR LQ FLOW
      DO II=1, MXNODE
         QBDIRSAVE(II) = QBDIR(II)
         IF(CY_FLIP(II).GT.0.AND.CY_FLIP(II).NE.21.)
     +      CYCOUNT = CYCOUNT + 1                                       !TR: 2012 11 01 COUNT ALL CHANGED CAUCHY BOUNDARIES
      ENDDO
C
C--MESSAGES
      IF(CYCOUNT.NE.0) THEN
        WRITE(IOUT,9003) CYCOUNT, KPER
      ENDIF
C
 9001 FORMAT(4X,'SWITCH NODE',I3,' WITH Q ',G14.6, ' TO LQ-FLOW WITH '  
     +,G14.6)
 9002 FORMAT(4X,'RESET NODE ',I4,' FROM LQ TO CAUCHY FLOW =',2G14.6) 
 9003 FORMAT(4X,'CHANGED',I5,' CONDUIT NODES DUE TO CYLQ IN STRESS',
     +       ' PERIOD ',I6) 
C
      RETURN
      END
C
C
C
C
C
      SUBROUTINE CONLH1(KPER,KSTP,KITER)
C     *****************************************************************
C     LIMITED HEAD BOUNDARY
C     IF H < HLH -> NODE = REGULAR FLOW (NO BOUNDARY)
C     ELSE       -> NODE = FIXED HEAD WITH HLH
C     *****************************************************************
C     
C     LAST CHANGE 2013 03 23 BY THOMAS REIMANN
C
      USE CFPMODULE, ONLY: MXNODE, MXTUBE, QBDIR, QBDIRSAVE, BEGHEAD,
     + QFIX, NNOD, HLH, LH_FLIP, QLH, ILH, B_MAT  
      USE GLOBAL, ONLY: IOUT
C      
      IMPLICIT NONE
C
C--ARGUMENTS      
C
C ILH(I,1)      NODENUMBER OF LH BOUNDARY
C ILH(I,2)      0 = LH FLOW USED, 1 = FIXED HEAD ACTIVE
C HLH           LIMITED HEAD
C LHCOUNT       COUNTER FOR ACTIVE FHLQ BC'S, GIVE OUTPUT MESSAGE
C Q             FLOW IN NODE
C I_LH          COUNTER, IF NODE IS FHLQ BC, THIS IS THE NODE NUMBER
C I_NODE        COUNTER
C J             COUNTER
C II            COUNTER
C
      INTEGER KPER, KSTP, KITER, LHCOUNT, J, I_LH, I_NODE, II
      DOUBLE PRECISION H
C
C--SKIP FOR FIRST TIMESTEP
      IF (((KPER.EQ.1).AND.(KSTP.EQ.1)).AND.(KITER.EQ.1)) RETURN
C
C--RESET COUNTER FOR OUTPUT MESSAGE     
      LHCOUNT = 0
C
C--INITIALIZE QLH
      DO J=1, MXNODE
        QLH(J) = 0.0       
      ENDDO
C
C--LOOK, IF CONDUIT NODE IS LH-BOUNDARY
      DO I_NODE=1, MXNODE
        I_LH = 0
        DO J=1, MXNODE
          IF(ILH(J,1).EQ.I_NODE) THEN
            I_LH = J
          ENDIF
        ENDDO     
C
C--IF I_NODE IS LH BOUNDARY
        IF(I_LH.NE.0) THEN
C
C--COMPUTE CAUCHY FLOW AT I_NODE
C  Q > 0: FLOW FROM CAUCHY INTO NODE. 
          H = B_MAT(I_NODE)
C
C--IF NODE IS REGULAR (NOT FIXED)
          IF(ILH(I_LH,2).EQ.0) THEN           
C
C--IF HEAD IS GREATER THAN LIMITING HEAD: SWITCH TO FIXED HEAD              
            IF (H.GT.HLH(I_LH).AND.LH_FLIP(I_NODE).LE.31)THEN           !TR: ALLOW SWITCH TO LQ 10 TIMES FOR INITIALLY LQ NODES
              ILH(I_LH,2) = 1                                           !TR: 2013 03 18 SET LQ FLAG
              BEGHEAD(I_NODE) = HLH(I_LH)                               !TR: 2013 03 18 SET QCYLQ
              LH_FLIP(I_NODE) = LH_FLIP(I_NODE) + 1                     !TR: 2012 11 01 INCREASE FLIP COUNTER
              WRITE(IOUT,9001) I_NODE, H, HLH(I_LH)
            ENDIF
C
C--IF NODE IS FIXED HEAD WITH LIMITED HEAD
          ELSE IF(ILH(I_LH,2).EQ.1) THEN
C
C--SET FLIPCOUNTER FOR INITIALLY LQ NODES
            IF (KITER.EQ.1) LH_FLIP(I_NODE) = 21                        !TR: 2012 11 01 SET LH_FLIP TO 21 IF THE BC IS LH
C
C--IF HEAD IS LARGER THEN HLH KEEP FIXED HEAD            
            IF(H.GE.HLH(I_LH)) THEN                                     !TR: 2012 11 20 
              BEGHEAD(I_NODE) = HLH(I_LH)
C
C--IF HEAD IS SMALLER THEN LH, SWITCH BACK TO REGULAR NODE                
            ELSEIF ((LH_FLIP(I_NODE).LE.10).OR.                         !TR: ALLOW SWITCH TO FH 10 TIMES FOR INITIALLY FH NODES
     +              (LH_FLIP(I_NODE).GE.21)) THEN          
              ILH(I_LH,2) = 0                                           !TR: 2013 03 18 SET FLAG TO CAUCHY
              BEGHEAD(I_NODE) = -1                                      !TR: 2013 03 18 SET CAUCHY LQ FLOW TO DZ
              LH_FLIP(I_NODE) = LH_FLIP(I_NODE) + 1                     !TR: 2012 11 01 
              WRITE(IOUT,9002) I_NODE, H
            ELSE                                                        !TR: IF SOLUTION OSCILLATES, KEEP LQ FLOW
              ILH(I_LH,2) = 1                                           !TR: NOT SURE ABOUT THIS - TODO: CHECK
              BEGHEAD(I_NODE) = HLH(I_LH)                               !TR: NOT SURE ABOUT THIS - TODO: CHECK
              LH_FLIP(I_NODE) = LH_FLIP(I_NODE) + 1                     !TR: NOT SURE ABOUT THIS - TODO: CHECK
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C--UPDATE QBDIRSAVE TO ACCOUNT FOR LQ FLOW
      DO II=1, MXNODE
         QBDIRSAVE(II) = QBDIR(II)
         IF(LH_FLIP(II).GT.0.AND.LH_FLIP(II).NE.21.)
     +      LHCOUNT = LHCOUNT + 1                                       !TR: 2012 11 01 COUNT ALL CHANGED CAUCHY BOUNDARIES
      ENDDO
C
C--MESSAGES
      IF(LHCOUNT.NE.0) THEN
        WRITE(IOUT,9003) LHCOUNT, KPER
      ENDIF
C
 9001 FORMAT(4X,'SWITCH NODE',I3,' WITH H ',G14.6, ' TO LH WITH HLH: '
     +,G14.6)
 9002 FORMAT(4X,'RESET NODE ',I4,' FROM LH TO REGULAR FLOW =',2G14.6) 
 9003 FORMAT(4X,'CHANGED',I5,' CONDUIT NODES DUE TO LH IN STRESS',     
     +       ' PERIOD ',I6) 
C
      RETURN
      END
C
C
C
C
C
      SUBROUTINE PFPS2_COMP
C     *****************************************************************
C     COMPUTE THE PFPS2 COEFFICIENT (SIMILAR TO MOD2 USED FOR EXCHANGE
C     PFPS2 CONTAINS ALL INFORMATION TO COMPUTE THE VOLUME CHANGE OF 
C     VARIABLY FILLED PIPES BY MULTIPLYING PFPS2 WITH HEAD CHANGE
C
C     LAST CHANGE: 2012 07 16 THOMAS.REIMANN@TU-DRESDEN.DE
C     *****************************************************************      
C
      USE CONSTANTS, ONLY: PI, TWOPI, DZ, DOS, NEARZERO_30, NEARZERO_15
      USE CFPMODULE, ONLY:NODETOP, GEOHIGHT, NODEBOT, B_MAT, B, HC,     
     +    B_MAT_O, NBR, CON_DATA, MXNODE, NODE_LOCA, ONE8TH,       
     +    NNOD, TORTUOS, L_NODE, PFPS2,    
     +    B_MAT_P
      USE GLOBAL, ONLY:IOUT
      USE GWFBASMODULE, ONLY:DELT
      IMPLICIT NONE
C
      INTRINSIC DABS
      INTEGER I, J, ITEST
      DOUBLE PRECISION VNEW, VOLD, TUBETOP1, TUBEBOT1, DKNOT, THETA
      DOUBLE PRECISION WETTDNEW, WETTDOLD, X, Y, Z, LENGTH  
C
C--BARC**STORAGE CHANGES FOR PARTIALLY-FILLED CONDUITS
      DO I = 1, MXNODE
        PFPS2(I) = DZ                                                 !TR: 2012 07 12 PFPS / INITIALIZE 
        DO J = 1, NNOD
          ITEST = NBR(I, J+4+NNOD)
C
C--BARC**ITEST IS A TUBE NUMBER !!!
          IF ( ITEST.NE.0 ) THEN
            TUBETOP1 = GEOHIGHT(I) + CON_DATA(ITEST, 2)/DOS
            TUBEBOT1 = GEOHIGHT(I) - CON_DATA(ITEST, 2)/DOS
C
C--BARC**ONLY IN PARTIALLY FILLED CASES // !TR: CHECK ALSO FOR PARTIALLY 
C  FILLED PIPES IN THE PREVIOUS TS
            IF ((B_MAT_P(I).LT.TUBETOP1.AND.B_MAT_P(I).GT.TUBEBOT1).OR. !TR: 2012 06 08 REPLACED B BY B_MAT
     +          (B_MAT_O(I).LT.TUBETOP1.AND.B_MAT_O(I).GT.TUBEBOT1))THEN!TR: 2012 06 08 CONSIDER REFILLING 
C
C--BARC**DETERMINE L (LENGTH)
              X = DABS(NODE_LOCA(I,2)-NODE_LOCA(NBR(I,J+4),2))          !TR: 2012 06 08 J+4 INSTEAD OF J
              Y = DABS(NODE_LOCA(I,3)-NODE_LOCA(NBR(I,J+4),3))          !TR: 2012 06 08 J+4 INSTEAD OF J
              Z = DABS(NODE_LOCA(I,4)-NODE_LOCA(NBR(I,J+4),4))          !TR: 2012 06 08 J+4 INSTEAD OF J
              LENGTH = 1.D0
C
C--BARC**FOR CASES OF Z=0
              IF ( Z.LT.NEARZERO_30 ) THEN
                IF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
                  LENGTH = Y/DOS
                ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
                  LENGTH = X/DOS
                ELSE
                  LENGTH = DSQRT(X**2+Y**2)/DOS
                ENDIF
C
C--BARC**FOR CASES OF Z>0
              ELSEIF ( X.LT.NEARZERO_30 .AND. Y.GT.DZ ) THEN
                LENGTH = DSQRT(Y**2+Z**2)/DOS
              ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO_30 ) THEN
                LENGTH = DSQRT(X**2+Z**2)/DOS
              ELSE
                LENGTH = Z
              ENDIF
C
C--CORRECT LENGTH TO CONSIDER REAL VOLUME FOR DEWATERING                !TR: 2014 08 01 PFPS
              IF ((Z.GT.DZ).AND.((Z*0.5).GT.CON_DATA(ITEST,2)))THEN   !TR: 2014 08 01 PFPS
                IF(X.LT.NEARZERO_30 .AND. Y.GT.DZ)THEN                   !TR: 2014 08 01 PFPS
                  LENGTH = (Y/Z)*(CON_DATA(ITEST,2)*0.5)                !TR: 2014 08 01 PFPS
                ELSEIF(X.GT.DZ .AND. Y.LT.NEARZERO_30)THEN               !TR: 2014 08 01 PFPS
                  LENGTH = (X/Z)*(CON_DATA(ITEST,2)*0.5)                !TR: 2014 08 01 PFPS
                ENDIF                                                   !TR: 2014 08 01 PFPS
              ELSEIF ((Z.GT.DZ).AND.(Z*0.5.LT.CON_DATA(ITEST,2)))THEN !TR: 2014 08 01 PFPS
                IF(X.LT.NEARZERO_30 .AND. Y.GT.DZ)THEN                   !TR: 2014 08 01 PFPS
                  LENGTH = Y-0.5*Y*Z/CON_DATA(ITEST,2)                  !TR: 2014 08 01 PFPS
                ELSEIF(X.GT.DZ .AND. Y.LT.NEARZERO_30)THEN               !TR: 2014 08 01 PFPS
                  LENGTH = X-0.5*X*Z/CON_DATA(ITEST,2)                  !TR: 2014 08 01 PFPS
                ENDIF                                                   !TR: 2014 08 01 PFPS
              ENDIF                                                     !TR: 2014 08 01 PFPS
C  
              LENGTH = LENGTH * TORTUOS(ITEST)                          !TR: 2012 06 08 COMPUTE LENGTH
C
C--PREVIOUS ITERATION              
              THETA = DZ
              WETTDNEW = B_MAT_P(I) - TUBEBOT1
              DKNOT = CON_DATA(ITEST, 2)
              IF (WETTDNEW < CON_DATA(ITEST,2) .AND.
     +            WETTDNEW > DZ                     ) THEN
                        THETA = DOS*ASIN(DOS*DSQRT((DKNOT/DOS)**2
     +                          -(WETTDNEW-DKNOT/DOS)**2)/DKNOT)
              END IF
              IF ( B_MAT_P(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA   !TR: 2012 06 08 REPLACED B BY B_MAT
              IF (WETTDNEW.GE.CON_DATA(ITEST,2)) THETA = TWOPI         !TR: 2012 06 08 CONSIDER FILLED PIPE / PREVENT 'NAN' = FULL CASE
              IF (WETTDNEW.LE.DZ) THETA = DZ                        !TR: 2012 06 11 CONSIDER DRY PIPE / PREVENT 'NAN' = DRY CASE
              VNEW = ONE8TH*(THETA-SIN(THETA))*DKNOT**2*LENGTH        !TR: 2012 06 08 REPLACED LTOURT BY LENGTH
C
C--PREVIOUS TIMESTEP
              THETA = DZ
              WETTDOLD = B_MAT_O(I) - TUBEBOT1
              IF (WETTDOLD < CON_DATA(ITEST,2) .AND. 
     +            WETTDOLD > DZ) THEN
                        THETA = DOS*ASIN(DOS*DSQRT((DKNOT/DOS)**2
     +                          -(WETTDOLD-DKNOT/DOS)**2)/DKNOT)
              END IF
              IF ( B_MAT_O(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA
              IF (WETTDOLD.GE.CON_DATA(ITEST,2)) THETA = TWOPI          !TR: 2012 06 08 CONSIDER FILLED PIPE / PREVENT 'NAN'
              IF (WETTDOLD.LE.DZ) THETA = DZ                            !TR: 2012 06 11 CONSIDER DRY PIPE / PREVENT 'NAN' = DRY CASE              
              VOLD = ONE8TH*(THETA-SIN(THETA))*DKNOT**DOS*LENGTH        !TR: 2012 06 08 REPLACED LTOURT BY LENGTH
              IF( ABS(B_MAT_P(I)-B_MAT_O(I)) < NEARZERO_15 ) THEN       !TR: 2012 07 12 THIS OCCURS IN THE FIRST ITERATION
                  PFPS2(I) = DZ
              ELSE
                  PFPS2(I) = (VNEW-VOLD)/((B_MAT_P(I)-B_MAT_O(I))*DELT)
     +                       +PFPS2(I)
              END IF
              !IF ((B_MAT_P(I)-B_MAT_O(I)).EQ.DZ) PFPS2(I) = DZ      
C
C--BARC**ENDIF FOR PARTIALLY FILLED STATEMENT
            ENDIF
C
C--BARC**ENDIF FOR ITEST STATEMENT (ALL TUBES COMING TO THE NODE)
          ENDIF
C
C--BARC**ENDDO FOR NNOD DO
        ENDDO      
C
      ENDDO
      END
C
C
C
C
C      
      SUBROUTINE CADSML2_COMP(FLG)
C     *****************************************************************
C     COMPUTE THE CADS COEFFICIENT FOR MULTILAYER SETUPS (SIMILAR TO 
C     MOD2 USED FOR EXCHANGE, CADSML2 CONTAINS ALL INFORMATION TO
C     COMPUTE THE VOLUME CHANGE OF VARIABLY CADS SETTINGS BY 
C     MULTIPLYING CADSML2 WITH HEAD CHANGE
C
C     LAST CHANGE: 2013 07 05 THOMAS.REIMANN@TU-DRESDEN.DE
C     *****************************************************************      
C
      USE CONSTANTS, ONLY: TWOPI, NEARZERO_30,  DZ, DOS, PI
      USE CFPMODULE, ONLY:NODETOP, GEOHIGHT, NODEBOT, B_MAT, B, HC,     
     +    B_MAT_O, NBR, CON_DATA, MXNODE, NODE_LOCA, ONE8TH,       
     +    NNOD, TORTUOS,  L_NODE, CADSML2,  
     +    B_MAT_P, W_CADS, CADSMLDAT, CADSML2_P                         !TR: 2013 07 26 CADSML2_P
      USE GLOBAL, ONLY:IOUT
      USE GWFBASMODULE, ONLY:DELT
      IMPLICIT NONE
C
      INTRINSIC DABS
      INTEGER I, J, N, NN, FLG                                          !TR: 2013 07 05 FLG TO CONSIDER CADSML // FLG = 1 B_MAT_P // FLG = 2 B_MAT
      DOUBLE PRECISION VNEW, VOLD, TUBETOP1, TUBEBOT1, DKNOT, THETA
      DOUBLE PRECISION WETTDNEW, WETTDOLD, UPPERLIMIT, P_HEAD(MXNODE)   !TR: 2013 07 05 CONSIDER PREVIOUS HEAD
C
C--SET PREVIOUS HEADS
      IF (FLG.EQ.1) THEN
        P_HEAD = B_MAT_P
      ELSE
        P_HEAD = B_MAT
      ENDIF      
C
C--FOR ALL NODES
      DO I = 1, MXNODE
C
C--SAVE PREVIOUS VALUES FOR FIRST ITERATION                             !TR: 2013 07 26 CADSML2_P      
        CADSML2_P(I) = CADSML2(I)
        CADSML2(I) = DZ                                               !TR: 2012 07 12 PFPS / INITIALIZE
C
C--COMPUTE VOLUME OF PREVIOUS ITERATION
        CALL CADSML_VOL(I,P_HEAD,VNEW)        
C        
C--COMPUTE VOLUME PREVIOUS TIMESTEP
        CALL CADSML_VOL(I,B_MAT_O,VOLD)
C
C--COMPUTE CADSML2              
        CADSML2(I) = (VNEW-VOLD)/((P_HEAD(I)-B_MAT_O(I))*DELT)
     +             + CADSML2(I)
        IF ((P_HEAD(I)-B_MAT_O(I)).EQ.DZ) CADSML2(I) = CADSML2_P(I)   !TR: 2012 07 12 THIS OCCURS IN THE FIRST ITERATION
C
      ENDDO     
C      
      END      
C 
C--BARC***END FM SUBROUTINE
C======================================================================
C
C MXNODE                NUMBER OF NODES
C MXTUBE                NUMBER OF TUBES
C LSURFACEDEP FLAG:     0 - CONSTANT EXCHANGE
C                       1 - SURFACE DEPENDENT EXCHANGE
C CON_DATA              CONDUIT DATA WITH:
C                       TUBENUMBER, DIAMETER, LENGTH, ROUGHNESS
C MODD                  EXCHANGE COEFFICIENT 
C PI                    MATHEMATICAL CONSTANT  
C ALPHA                 DEPENDS ON CONDUIT GEOMETRY
C AF                    SURFACE OF EXCHANGE BETWEEN CONDUITS AND POROUS MEDIA
C BEG_HEAD              CONTAINING THE HEADS READ FROM FILE
C NEIBR                 ARRAY CONTAINING THE CONDUITSTRUCTUR:
C                       NODE, NEIGHBOUR NODE  1, 2, 3, 4, TUBE NR. 1, 2, 3, 4, 
C                       MODFLOWROW, -COLUMN
C TURB                  CONTAINS THE TYPE OF FLOW
C                       0 - LAMINAR
C                       1 - TURBULENT
C MOD                   EXCHANGE COEFFICIENT USED FOR EXCHANGE WITH MODFLOW
C X0                    HEADS CALCULATED BY THE NEWTON-RAPHSON ITERATION,
C                       BE STARTING POINT FOR THE FIRST ITERATION STEP 
C                       CALCULATED BY THE SUBROUTINE 'GLLMA'
C G                     CONSTANT OF GRAVITATION
C NU                    DYNEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C VISC                  DYNEMATIC VISCOSITY (DEPEND ON TEMPERATURE TC)
C EQ_MAT                STORE THE MATRIX OF THE EQUATIONSYSTEM SOLVED
C                       THE FUNCTIONS 'LUDCMP' AND 'LUBKSB'
C B                     HEADPRESSURE OF CONDUIT
C TUB_REYNOLD           VECTOR CONTAINS THE REYNOLD NUMBER FOR EVERY TUBE
C FTURB                 FLAG IF TURBULENT FLOW EXIST - 1
C                       ELSE                         - 0
C TUBE                  TUBENR., NODE_BEGIN, NODE_END 
C                       Q M/S (BEGIN_NODE_PRESSUR > END_NODE_PRESSUR => Q >= 0),
C CHEMIE                0 NOT CALCULATED, 1 CALCULATED 
C B_MAT                 HEADS CALCULATED BY THE NEWTON-RAPHSON ITERATION,
C                       STARTING POINT FOR THE FIRST ITERATION STEP 
C                       CALCULATED BY THE SUBROUTINE 'GLLMA'
C                       C--SEBA CHANGED: USE PRESSURE HEADS FROM LAST TIMESTEP
C                       AS STARTING POINT!
C                       STORE ALSO THE LAST HEADS IN THE ITERATION
C EPSILON               CRITERION OF CONVERGENCE
C RELAX                 RELAXATION PARAMETER WICH CHANGE THE
C                       STEPLENGTH: X_I = X_I-1 + (RELAX *  Z)
C HNEW                  MODFLOW NEW CALCULATED PRESSURE HEAD
C                       DUE FOR BUDGET AT EVERY NODE COMING FROM
C                       EXCHANGE WITH THE 'FISSURED SYSTEM'
C X0                    INPUT VECTOR OF PRESSURE HEADS COMING FROM LAMINAR 
C                       (START POINT OF ITERATION) OR ITERATIVE PROCESS
C QBDIR                 FLOW RATE DIRECTLY IN CONDUIT