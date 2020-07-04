      MODULE GWFHFBMODULE
        !
        SAVE
        !
        LOGICAL:: HFB_IN_USE = .FALSE.
        !
        LOGICAL, POINTER:: HAS_NWT
        !
        LOGICAL, POINTER:: NO_RP
        !
        INTEGER, POINTER:: MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB
        INTEGER, POINTER:: HFBRT,MXHFBNP,NACTHFB
        !
        TYPE, PRIVATE:: HFBVAR
          INTEGER::         K1,I1,J1,I2,J2,K2
          DOUBLE PRECISION::HYDCHR,RCON,CON1,CON2
        END TYPE
        !
        TYPE(HFBVAR), DIMENSION(:),POINTER,CONTIGUOUS:: HFB
        !
C NWT SPECIFIC VARIABLES
        INTEGER, DIMENSION(:,:), POINTER,CONTIGUOUS   ::ICEL_HFB
        
      TYPE GWFHFBTYPE
        INTEGER, POINTER:: MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB
        INTEGER, POINTER:: HFBRT,MXHFBNP,NACTHFB
        INTEGER, DIMENSION(:,:), POINTER,CONTIGUOUS   ::ICEL_HFB
        TYPE(HFBVAR), DIMENSION(:),POINTER,CONTIGUOUS:: HFB
        !
        LOGICAL, POINTER:: HAS_NWT
        LOGICAL, POINTER:: NO_RP
      END TYPE
      TYPE(GWFHFBTYPE):: GWFHFBDAT(10)
      END MODULE GWFHFBMODULE


      SUBROUTINE GWF2HFB7AR(INHFB,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HORIZONTAL FLOW BARRIER PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,LAYHDT,CR,CC,BOTM,LBOTM,
     1                      DELR,DELC,IOUT,IUNIT
      USE GWFHFBMODULE,ONLY:MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB,
     1                      HFB,HFBRT,ICEL_HFB, MXHFBNP,NACTHFB,
     2                      HFB_IN_USE, HAS_NWT, NO_RP
      USE ERROR_INTERFACE,   ONLY: STOP_ERROR
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER
      USE CONSTANTS,         ONLY: Z, DZ, ONE, TRUE, FALSE
C
      IMPLICIT NONE
C
      INTEGER INHFB, IGRID, MXACTFB
      CHARACTER(16):: AUX(1)
      CHARACTER(768):: LINE
C ADDED FOR IMPLICIT NONE
      LOGICAL:: HAS_ERROR
      INTEGER:: LLOC,ISTART,ISTOP, N,MXFBP,LSTSUM,LSTBEG
      INTEGER::I,K,IP,NUMINST,NLST
      REAL:: DUM, R
C     ------------------------------------------------------------------
C
      HFB_IN_USE = TRUE
C
C1------Allocate scalar data.
      ALLOCATE(MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB,HFBRT,MXHFBNP,
     +         NACTHFB, HAS_NWT, NO_RP)
      !
      HAS_NWT = IUNIT(63) /= Z
      NO_RP   = FALSE
C
C2------IDENTIFY PACKAGE.
      WRITE(IOUT,1) INHFB
      !
    1 FORMAT(1X,/1X,'HFB2 -- HORIZONTAL-FLOW BARRIER PACKAGE, ',
     +'VERSION OWHM.',/,
     +'   INPUT READ FROM UNIT ',I4)
C
C3------READ AND PRINT NPHFB, MXFB, NHFBNP
      CALL READ_TO_DATA(LINE,INHFB,IOUT,IOUT, 
     +                          HED="-- READING HFB PACKAGE INPUT --")
      LLOC = ONE
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INHFB,NPHFB,
     +                 MSG='HFB ERROR READING NPHFB  (The 1st No.)')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INHFB,MXFBP,
     +                 MSG='HFB ERROR READING MXFB  (The 2nd No.)')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INHFB,NHFBNP,
     +                 MSG='HFB ERROR READING NHFBNP  (The 3rd No.)')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INHFB,MXHFBNP,
     +                 HAS_ERROR=HAS_ERROR)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPHFB,DUM,IOUT,INHFB)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXFBP,DUM,IOUT,INHFB)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHFBNP,DUM,IOUT,INHFB)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXHFBNP,DUM,0,INHFB)
      !
C IF THERE IS AN ERROR READING MXHFBNP THEN THE LAST CHARACTER IS SET TO E
C RESET LOCATION AND SET VALUE OF MXHFBNP IF ERROR IN CONVERSION OCCURS
      IF (HAS_ERROR) THEN
        LLOC = ISTART - 1
        MXHFBNP=NHFBNP
      END IF
      IF (NHFBNP.GT.MXHFBNP) THEN
          CALL STOP_ERROR(LINE,INHFB,IOUT,
     +    'ERROR: HFB INPUT "NHFBNP" MUST BE LESS THAN '//
     +               'OR EQUAL TO "MXHFBNP".')
      END IF
      !
      WRITE(IOUT,500) NPHFB,MXFBP
  500 FORMAT(1X,I5,' PARAMETERS DEFINE A MAXIMUM OF ',I6,
     +       ' HORIZONTAL FLOW BARRIERS')
      !
      WRITE(IOUT,531) MXHFBNP
  531 FORMAT(1X,I6,' MAX HORIZONTAL FLOW BARRIERS NOT DEFINED BY',
     +       ' PARAMETERS')
      !
      WRITE(IOUT,530) NHFBNP
  530 FORMAT(1X,I6,' HORIZONTAL FLOW BARRIERS NOT DEFINED BY',
     +       ' PARAMETERS')
C
C4------LOOK FOR NOPRINT OPTION.
      IPRHFB = ONE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INHFB)
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT' .OR.
     +   LINE(ISTART:ISTOP).EQ.'NO_PRINT') THEN
        WRITE(IOUT,3)
        !
    3   FORMAT(1X,
     +'LISTS OF HORIZONTAL FLOW BARRIER CELLS WILL NOT BE PRINTED')
        IPRHFB = Z
      END IF
C
C5------CALCULATE AMOUNT OF SPACE USED BY HFB PACKAGE AND ALLOCATE HFB.
      MXACTFB = MXHFBNP+MXFBP
      IHFBPB = MXACTFB + 1
      MXHFB = MXACTFB + MXFBP
C seb HFBRT Holds Count of Number Fault Layer Routes
      HFBRT = Z
      ALLOCATE (HFB(MXHFB))
      HFB(:)%K1     = Z
      HFB(:)%I1     = Z
      HFB(:)%J1     = Z
      HFB(:)%I2     = Z
      HFB(:)%J2     = Z
      HFB(:)%K2     = Z
      HFB(:)%HYDCHR = DZ
      HFB(:)%RCON   = DZ
      HFB(:)%CON1   = DZ
      HFB(:)%CON2   = DZ
C
!FLOW PACKAGE HAS NOT BEEN RUN YET SO IT WILL ALWAYS BE NEGATIVE
!C6------CHECK THAT THE FLOW PACKAGE IS A KIND THAT HFB CAN SUPPORT.
!C6------LAYHDT IS -1 UNLESS THE FLOW PACKAGE CHANGES IT.  IF LAYHDT
!C6------IS STILL NEGATIVE, IT IS ASSUMED THAT HFB WILL NOT WORK.
!      IF (LAYHDT(1) < Z) THEN
!          WRITE(IOUT,550)
!  550   FORMAT(/,
!     +' ERROR: SELECTED FLOW PACKAGE DOES NOT SUPPORT HFB PACKAGE',/,
!     +' -- STOP EXECUTION (GWF2HFB7AR)')
!        CALL USTOP(' ')
!      ENDIF
C
C7------READ PARAMETER DEFINITIONS (ITEMS 2 AND 3)
      WRITE(IOUT,600) NPHFB
      !
  600 FORMAT(//,1X,I5,' HFB parameters')
      IF (NPHFB > Z) THEN
        LSTSUM = IHFBPB
        DO 20 K = 1, NPHFB
          LSTBEG = LSTSUM
          CALL UPARLSTRP(LSTSUM,MXHFB,INHFB,IOUT,IP,'HFB ','HFB ',
     +                   1,NUMINST)
          IF(NUMINST > Z) THEN
            !
            CALL USTOP(' INSTANCES ARE NOT SUPPORTED FOR HFB')
          END IF
          NLST=LSTSUM-LSTBEG
          CALL SGWF2HFB7RL(NLST,LSTBEG,MXHFB,INHFB,IOUT,
     1         'BARRIER   LAYER   IROW1  ICOL1  IROW2  ICOL2   FACTOR'//
     2         '      LAYER2',
     3         NCOL,NROW,NLAY,IPRHFB)
          CALL SGWF2HFB7CK(LSTBEG,LSTSUM-1)
   20   CONTINUE
      ENDIF
C
C8------READ BARRIERS NOT DEFINED BY PARAMETERS (ITEM 4)
      NHFB = Z
      WRITE(IOUT,610) NHFBNP
      !
  610 FORMAT(/,1X,I6,' BARRIERS NOT DEFINED BY PARAMETERS')
      IF(NHFBNP > Z) THEN
        CALL SGWF2HFB7RL(NHFBNP,1,MXHFB,INHFB,IOUT,
     1         'BARRIER   LAYER   IROW1  ICOL1  IROW2  ICOL2   HYDCHR'//
     2         '      LAYER2',
     3             NCOL,NROW,NLAY,IPRHFB)
        NHFB = NHFB + NHFBNP
        CALL SGWF2HFB7CK(1,NHFBNP)
      ENDIF
C
C9------SUBSTITUTE DATA FOR PARAMETERIZED BARRIERS INTO ACTIVE SECTION
C9------OF HFB ARRAY
      MXACTFB= IHFBPB-1
      CALL PRESET('HFB ')
      IF(NPHFB >= Z) THEN
C
C10-----READ NUMBER OF ACTIVE HFB PARAMETERS (ITEM 5)
        READ(INHFB,'(A)') LINE
        LLOC = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NACTHFB,DUM,IOUT,INHFB)
        IF (NACTHFB > Z) THEN
          DO 650 I = 1,NACTHFB
C
C11-----READ AND ACTIVATE AN HFB PARAMETER (ITEM 6)
           CALL SGWF2HFB7SUB(INHFB,'HFB ',IOUT,'HFB ',
     1                        MXHFB,MXACTFB,NHFB,
     2      'BARRIER   LAYER   IROW1  ICOL1  IROW2  ICOL2   HYDCHR'//
     3      '      LAYER2',IPRHFB)
  650     CONTINUE
        ENDIF
      ENDIF
C
C seb
C-------CHECK FOR SUPPORTED SOLVERS. MUST BE NON-SYMMETRIC SOLVER(NWT/GMG)
C-------WHEN LAYER ROUTING IS ENABLED
      HFBRT=2*HFBRT  !# of cells effected by fault layer routing, ie 2 per barrier
      IF(HFBRT > Z)THEN
        IF(IUNIT(63) == Z) THEN   !NWT IUNITS
         CALL USTOP('ERROR: HFB WITH INTER-LAYER FLOW ONLY 
     +                             WORKS WITH NWT SOLVER')
        END IF
      END IF

      IF(HFBRT > Z .AND. IUNIT(63) /= Z)THEN
        ALLOCATE(ICEL_HFB(HFBRT,3))   !Only Allocated when NWT is used
!        ALLOCATE(Dc_HFB(MXHFB,2:5))  !Only Allocated when NWT is used ***NOTE SPECIAL INDEXING
!        CALL ORDERCELLHFB()          !POPULATES ICEL_HFB WITH CORRECT VALUES
      END IF
C
C
!MOVED CALL TO GWF2HFB7RP
!C12-----MODIFY HORIZONTAL BRANCH CONDUCTANCES FOR CONSTANT T LAYERS.
!      CALL SGWF2HFB7MC()
        WRITE (IOUT,660) NHFB
  660 FORMAT(/,1X,1I6,' HFB BARRIERS')
C
C13-----SAVE POINTERS TO GRID AND RETURN.
      CALL SGWF2HFB7PSV(IGRID)
      !RETURN
      END SUBROUTINE
      
      SUBROUTINE GWF2HFB7CON(IGRID)
      USE GLOBAL,      ONLY:LAYHDT,IOUT
      USE CONSTANTS, ONLY: Z
      IMPLICIT NONE
      INTEGER::IGRID

C1------CHECK THAT THE FLOW PACKAGE IS A KIND THAT HFB CAN SUPPORT.
C1------LAYHDT IS -1 UNLESS THE FLOW PACKAGE CHANGES IT.  IF LAYHDT
C1------IS STILL NEGATIVE, IT IS ASSUMED THAT HFB WILL NOT WORK.
      IF (LAYHDT(1) < Z) THEN
        CALL USTOP('SELECTED FLOW PACKAGE DOES NOT SUPPORT HFB PACKAGE')
      ENDIF
C      
C2------Set pointers to the specified grid.
      CALL SGWF2HFB7PNT(IGRID)
C
      END SUBROUTINE
      

      SUBROUTINE GWF2HFB7FM(IGRID)
C     ******************************************************************
C     MODIFY HORIZONTAL BRANCH CONDUCTANCES IN VARIABLE-TRANSMISSIVITY
C     LAYERS TO ACCOUNT FOR HORIZONTAL FLOW BARRIERS. STORE UNMODIFIED
C     HORIZONTAL CONDUCTANCE IN HFB(#)%CON1 AND HFB(#)%CON2 TO ALLOW CALCULATION OF
C     SENSITIVITIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,LAYHDT,CR,CC,CV,BOTM,LBOTM,
     1                      DELR,DELC,LAYCBD
      USE GWFHFBMODULE,ONLY:NHFB,HFB
      USE CONSTANTS,   ONLY: Z, DZ, ONE
C     ------------------------------------------------------------------
C
C1------Set pointers to the specified grid.
      CALL SGWF2HFB7PNT(IGRID)
C
C2------FOR EACH BARRIER, MODIFY HORIZONTAL BRANCH CONDUCTANCES IF LAYER
C2------IS CONVERTIBLE.
      DO 10 II=1, NHFB
        K1 = HFB(II)%K1
        K2 = HFB(II)%K2
C
C3-----IF LAYHDT=0, THICKNESS AND CONDUCTANCE DO NOT VARY, AND
C3-----MODIFICATION OF CONDUCTANCE DUE TO BARRIER WAS DONE IN
C3-----SGWF1HFBMC
        IF (LAYHDT(K1) > Z) THEN
C
C4------CELL (J1,I1,K1) IS THE ONE WHOSE HORIZONTAL BRANCH
C4------CONDUCTANCES ARE TO BE MODIFIED.
          I1 = HFB(II)%I1
          J1 = HFB(II)%J1
C
C5------CELL (J2,I2,K1) IS THE CELL NEXT TO CELL (J1,I1,K1) AND
C5------SEPARATED FROM IT BY THE BARRIER.
          I2   = HFB(II)%I2
          J2   = HFB(II)%J2
          HCDW = HFB(II)%HYDCHR
C
C6------IF I1=I2, MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG ROW
C6------DIRECTION.
          IF (I1.EQ.I2 .AND. J1.NE.J2) THEN
C
C7------IF CR(J1,I1,K1) NOT = 0, CELLS ON EITHER SIDE OF BARRIER ARE
C7------ACTIVE
            IF (CR(J1,I1,K1).NE.0. .OR. K2 > Z) THEN
C
C8------CALCULATE AVERAGE SATURATED THICKNESS BETWEEN CELLS
C8------(J1,I1,K1) AND (J2,I2,K1).  NOTE: NEGATIVE SATURATED
C8------THICKNESS DOES NOT OCCUR; OTHERWISE, CR(J1,I1,K1) WOULD BE
C8------ZERO AND THE FOLLOWING CALCULATION FOR SATURATED THICKNESS
C8------WOULD BE SKIPPED.
              HD1 = HNEW(J1,I1,K1)
              HD2 = HNEW(J2,I2,K1)
              IF (HD1.GT.BOTM(J1,I1,LBOTM(K1)-1)) HD1 =
     +                                           BOTM(J1,I1,LBOTM(K1)-1)
              IF (HD2.GT.BOTM(J2,I2,LBOTM(K1)-1)) HD2 =
     +                                           BOTM(J2,I2,LBOTM(K1)-1)
              THKAVG = ((HD1-BOTM(J1,I1,LBOTM(K1))) +
     +                 (HD2-BOTM(J2,I2,LBOTM(K1))))/2.
C
C9------STORE UNMODIFIED CR FOR CALCULATING SENSITIVITIES
              HFB(II)%CON1 = CR(J1,I1,K1)
C
C10-----MODIFY CR(J1,I1,K1) TO ACCOUNT FOR BARRIER.
C seb If Layer Route Enabled Zero Out Existing Conductance
              IF (K2 > Z) THEN
                HFB(II)%CON2  = CR(J1,I1,K2)
                CR(J1,I1,K1) = DZ
                CR(J1,I1,K2) = DZ
                CALL HFB9HARMCON(I1,J1,K1,I2,J2,K2,II)
              ELSE
                IF (HCDW < Z) THEN
                  CR(J1,I1,K1) = CR(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = THKAVG*HCDW
                  CR(J1,I1,K1) = TDW*CR(J1,I1,K1)*DELC(I1)/
     +                          (TDW*DELC(I1)+CR(J1,I1,K1))
                END IF
              END IF
            ENDIF
C
C11-----CASE OF J1=J2. MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG
C11-----COLUMN DIRECTION.
          ELSEIF(J1.EQ.J2 .AND. I1.NE.I2) THEN
C
C12-----IF CC(J1,I1,K1) NOT = 0, CELLS ON EITHER SIDE OF BARRIER ARE
C12-----ACTIVE
            IF (CC(J1,I1,K1).NE.0. .OR. K2 > Z) THEN
C
C13-----CALCULATE AVERAGE SATURATED THICKNESS BETWEEN CELLS
C13-----(J1,I1,K1) AND (J2,I2,K1).  NEGATIVE SATURATED THICKNESS
C13-----DOES NOT OCCUR FOR THE SAME REASON AS DESCRIBED ABOVE.
              HD1 = HNEW(J1,I1,K1)
              HD2 = HNEW(J2,I2,K1)
              IF (HD1.GT.BOTM(J1,I1,LBOTM(K1)-1)) HD1 =
     +                                           BOTM(J1,I1,LBOTM(K1)-1)
              IF (HD2.GT.BOTM(J2,I2,LBOTM(K1)-1)) HD2 =
     +                                           BOTM(J2,I2,LBOTM(K1)-1)
              THKAVG = ((HD1-BOTM(J1,I1,LBOTM(K1))) +
     +                 (HD2-BOTM(J2,I2,LBOTM(K1))))/2.
C
C14-----STORE UNMODIFIED CC FOR CALCULATING SENSITIVITIES.
              HFB(II)%CON1 = CC(J1,I1,K1)
C
C15-----MODIFY CC(J1,I1,K1) TO ACCOUNT FOR BARRIER.
C seb If Layer Route Enabled Zero Out Existing Conductance
              IF (K2 > Z) THEN
                HFB(II)%CON2 = CC(J1,I1,K2)
                CC(J1,I1,K1) = DZ
                CC(J1,I1,K2) = DZ
                CALL HFB9HARMCON(I1,J1,K1,I2,J2,K2,II)
              ELSE
                IF (HCDW < Z) THEN
                  CC(J1,I1,K1) = CC(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = THKAVG*HCDW
                  CC(J1,I1,K1) = TDW*CC(J1,I1,K1)*DELR(J1)/
     +                        (TDW*DELR(J1)+CC(J1,I1,K1))
                END IF
              END IF
            ENDIF
C
C11-----CASE OF J1=J2 and I1=I2. MODIFY VERTICAL BRANCH CONDUCTANCE BETWEEN A LAYER AND THE NEXT
C       LOWER LAYER 
          ELSEIF(J1.EQ.J2 .AND. I1.EQ.I2 .AND. K2 > Z) THEN
            IF(LAYCBD(K1) /= Z) THEN
              WRITE(IOUT,'(/,A,/3A,I6,A,/ A)')
     +           REPEAT('#',50),
     +          'WARNING: HFB VERTICAL BARRIER DOES NOT WORK ',
     +          'WHEN THERE IS A 3D QUASI-CONFINING LAYER (LAYCBD/=0)',
     +          'THE ',II, 'th BARRIER WILL NOT BE APPLIED',
     +           REPEAT('#',50)
              CYCLE
            END IF
            IF (CV(J1,I1,K1).NE.0.) THEN
C14-----STORE UNMODIFIED CV FOR CALCULATING SENSITIVITIES.
              HFB(II)%CON1 = CV(J1,I1,K1)
C
C15-----MODIFY CV(J1,I1,K1) TO ACCOUNT FOR BARRIER.
                IF (HCDW < Z) THEN
                  CV(J1,I1,K1) = CV(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = DELR(J1)*DELC(I1)*HCDW
                  CV(J1,I1,K1)= ( CV(J1,I1,K1)**(-1) + TDW**(-1) )**(-1)!SIMILAR FORMULATION TO MF05 Eqn 5-25 EXCEPT HCDW IS TREATED LIKE A QUASICONFING BARRIER
                END IF
            ENDIF
          ENDIF
        ENDIF
   10 CONTINUE
C
C16-----RETURN
      !RETURN
      END SUBROUTINE
C
      SUBROUTINE SGWF2HFB7MC()
C     ******************************************************************
C     MODIFY HORIZONTAL CONDUCTANCES (CR AND CC) FOR CONFINED LAYERS TO
C     ACCOUNT FOR HORIZONTAL FLOW BARRIERS.  STORE UNMODIFIED HORIZONTAL
C     CONDUCTANCES IN HFB(#)%CON1 AND HFB(#)%CON2 TO ALLOW CALCULATION OF SENSITIVITIES.
C     CONVERTIBLE LAYERS WILL BE MODIFIED AS WELL AND UPDATED DURING EACH OUTER INTERATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,BOTM,LBOTM,DELR,DELC,CR,CC,CV,LAYHDT
      USE GLOBAL,      ONLY:LAYCBD
      USE GWFHFBMODULE,ONLY:NHFB,HFB
      USE CONSTANTS,   ONLY: Z, DZ, ONE
C     ------------------------------------------------------------------
C
C1------INITIALIZE ERROR FLAG TO ZERO.
      IERFLG=0
C
C2----DO FOR EACH BARRIER IN RANGE.
      DO 10 II = 1, NHFB
        K1 = HFB(II)%K1
        K2 = HFB(II)%K2
C
C3------FIND ROW AND COLUMN NUMBERS OF THE TWO CELLS ON BOTH SIDES
C3------OF THE BARRIER.
        I1 = HFB(II)%I1
        J1 = HFB(II)%J1
        I2 = HFB(II)%I2
        J2 = HFB(II)%J2
        TH0 = BOTM(J1,I1,LBOTM(K1)-1) - BOTM(J1,I1,LBOTM(K1))
        TH1 = BOTM(J2,I2,LBOTM(K1)-1) - BOTM(J2,I2,LBOTM(K1))
        THKAVG = (TH0+TH1)/2.0
        HCDW = HFB(II)%HYDCHR
        TDW = THKAVG * HCDW
C
C4------IF I1=I2, BARRIER IS BETWEEN TWO CELLS ON THE SAME ROW.
        IF (I1.EQ.I2 .AND. J1.NE.J2) THEN
C
C5------IF J2-J1=1, THE TWO CELLS ARE NEXT TO ONE ANOTHER (DATA OK).
          IF ((J2-J1).EQ.1) THEN
C
C6------BARRIER CELLS ARE ADJACENT.
C6------IF LAYER IS CONFINED AND BOTH CELLS ARE ACTIVE, SAVE
C-------ORIGINAL CR FOR COMPUTING SENSITIVITIES AND MODIFY CR
            IF (LAYHDT(K1) == Z) THEN
C
C7------IF CR(J1,I1,K1) NOT 0, BOTH CELLS ARE ACTIVE.
              IF (CR(J1,I1,K1) /= 0. .OR. K2 > Z) THEN
                HFB(II)%CON1 = CR(J1,I1,K1)
C
C8------MODIFY CR(J1,I1,K1) TO ACCOUNT FOR BARRIER.
C seb If Layer Route Enabled Zero Out Existing Conductance
                IF (K2 > Z) THEN
                  HFB(II)%CON2 = CR(J1,I1,K2)
                  CR(J1,I1,K1) = DZ
                  CR(J1,I1,K2) = DZ
                  CALL HFB9HARMCON(I1,J1,K1,I2,J2,K2,II)
                ELSE
                  IF (TDW < Z) THEN
                    CR(J1,I1,K1) = CR(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                  ELSE
                    CR(J1,I1,K1) = TDW*CR(J1,I1,K1)*DELC(I1)/
     +                            (TDW*DELC(I1)+CR(J1,I1,K1))
                  END IF
                END IF
              ENDIF
            ENDIF
          ENDIF
C
C9------IF J1=J2, BARRIER IS BETWEEN TWO CELLS ON THE SAME COLUMN.
        ELSEIF (J1.EQ.J2 .AND. I1.NE.I2) THEN
C
C10-----IF I2-I1=1, THE TWO CELLS ARE NEXT TO ONE ANOTHER (DATA OK).
          IF ((I2-I1).EQ.1) THEN
C
C11-----BARRIER CELLS ARE ADJACENT.
C11-----IF LAYER IS CONFINED AND BOTH CELLS ARE ACTIVE, SAVE
C11-----ORIGINAL CC FOR COMPUTING SENSITIVITIES AND MODIFY CC
            IF (LAYHDT(K1) == Z) THEN
C
C12-----IF CC(J1,I1,K1) NOT 0, BOTH CELLS ARE ACTIVE.
              IF (CC(J1,I1,K1).NE.0. .OR. K2 > Z) THEN
                HFB(II)%CON1 = CC(J1,I1,K1)
C
C13-----MODIFY CC(J1,I1,K1) TO ACCOUNT FOR BARRIER.
C seb If Layer Route Enabled Zero Out Existing Conductance
                IF (K2 > Z) THEN
                  HFB(II)%CON2 = CC(J1,I1,K2)
                  CC(J1,I1,K1) = DZ
                  CC(J1,I1,K2) = DZ
                  CALL HFB9HARMCON(I1,J1,K1,I2,J2,K2,II)
                ELSE
                  IF (TDW < Z) THEN
                    CC(J1,I1,K1) = CC(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                  ELSE
                    CC(J1,I1,K1) = TDW*CC(J1,I1,K1)*DELR(J1)/
     +                            (TDW*DELR(J1)+CC(J1,I1,K1))
                  END IF
                END IF
              ENDIF
            ENDIF
          ENDIF
C
C11-----CASE OF J1=J2 and I1=I2. MODIFY VERTICAL BRANCH CONDUCTANCE BETWEEN A LAYER AND THE NEXT
C       LOWER LAYER 
          ELSEIF(J1.EQ.J2 .AND. I1.EQ.I2 .AND. K2 > Z
     +                                       .AND. LAYHDT(K1) == Z) THEN
            IF(LAYCBD(K1) /= Z) THEN
              WRITE(IOUT,'(/A,/3A,I6,A,/A)')
     +           REPEAT('#',50),
     +          'WARNING: HFB VERTICAL BARRIER DOES NOT WORK ',
     +          'WHEN THERE IS A 3D QUASI-CONFINING LAYER (LAYCBD/=0)',
     +          'THE ',II, 'th BARRIER WILL NOT BE APPLIED',
     +           REPEAT('#',50)
              CYCLE
            END IF
            IF (CV(J1,I1,K1).NE.0.) THEN
C14-----STORE UNMODIFIED CV FOR CALCULATING SENSITIVITIES.
              HFB(II)%CON1 = CV(J1,I1,K1)
C
C15-----MODIFY CV(J1,I1,K1) TO ACCOUNT FOR BARRIER.
                IF (HCDW < Z) THEN
                  CV(J1,I1,K1) = CV(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = DELR(J1)*DELC(I1)*HCDW
                  CV(J1,I1,K1)= ( CV(J1,I1,K1)**(-1) + TDW**(-1) )**(-1)!SIMILAR FORMULATION TO MF05 Eqn 5-25 EXCEPT HCDW IS TREATED LIKE A QUASICONFING BARRIER
                END IF
            ENDIF
        ENDIF
   10 CONTINUE
C
C14-----RETURN
      !RETURN
      END SUBROUTINE
C
      SUBROUTINE SGWF2HFB7CK(IB1,IB2)
C     ******************************************************************
C     CHECK HFB CELL LOCATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,            ONLY:IOUT, LAYHDT
      USE GWFHFBMODULE,      ONLY:HFB
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE CONSTANTS,         ONLY: Z, DZ, ONE, NL
C     ------------------------------------------------------------------
C
C1----INITIALIZE ERROR FLAG TO ZERO.
      IERFLG = Z
C
C2----CHECK EACH BARRIER IN RANGE.
      DO 10 II = IB1,IB2
C
C3------FIND ROW AND COLUMN NUMBERS OF THE TWO CELLS ON BOTH SIDES
C3------OF THE BARRIER AND REARRANGE HFB ARRAY.
C seb USE NEW STRUCTURED VARIABLE FOR PULLING COORDINATES OF BARRIER
        K1=HFB(II)%K1
        I1=HFB(II)%I1
        J1=HFB(II)%J1
        !
        I2=HFB(II)%I2
        J2=HFB(II)%J2
        K2=HFB(II)%K2
        !
        IF(K2 > Z)THEN
          IF (LAYHDT(K1).NE.LAYHDT(K2)) THEN
                                        CALL USTOP(
     +  'HFB ERROR: BARRIER WITH FLOW ROUTE OR VERTICAL BARRIER '//
     +  'MUST HAVE BOTH LAYERS AS EITHER CONFINED OR CONVERTABLE'//NL//
     +  'ERROR FOR HFB #: '//NUM2STR(II)//NL//
     +  ' AND LAY, ROW,COL,ROW2,COL2,LAY2: '//NL//NUM2STR(K1)//' '
     +   //NUM2STR(I1)//' '//NUM2STR(J1)//' '//NUM2STR(I2)//' '
     +   //NUM2STR(J2)//' '//NUM2STR(K2)
     +                                            )
          END IF
        END IF

        IF (I1.EQ.I2 .AND. J1.NE.J2) THEN
          IF (J2.LT.J1) THEN
            ITEMP=J2
            J2=J1
            J1=ITEMP
            !
            IF (K2 > Z) THEN
              ITEMP=K2
              K2=K1
              K1=ITEMP
            END IF
          END IF
        ELSEIF (J1.EQ.J2 .AND. I1.NE.I2) THEN
          IF (I2.LT.I1) THEN
            ITEMP=I2
            I2=I1
            I1=ITEMP
            !
            IF (K2 > Z) THEN
              ITEMP=K2
              K2=K1
              K1=ITEMP
            END IF          
          END IF
        ELSEIF (J1.EQ.J2 .AND. I1.EQ.I2 .AND. K2 > Z) THEN
          IF (K2.LT.K1) THEN
            ITEMP=I2
            I2=I1
            I1=ITEMP
            !
            ITEMP=J2
            J2=J1
            J1=ITEMP
            !
            ITEMP=K2
            K2=K1
            K1=ITEMP
          END IF
          IF (K2-K1.NE.1) THEN
            CALL USTOP(
     +     'HFB ERROR: VERTICAL FLOW BARRIER MUST THE LAYERS BE '//
     +     'ADJACENT TO EACH OTHER (Layer2-Layer=1)'//NL//
     +     'ERROR FOR HFB #: '//NUM2STR(II)//NL//
     +     ' AND LAY, ROW,COL,ROW2,COL2,LAY2: '//NL//NUM2STR(K1)//' '
     +      //NUM2STR(I1)//' '//NUM2STR(J1)//' '//NUM2STR(I2)//' '
     +      //NUM2STR(J2)//' '//NUM2STR(K2)
     +                )
          END IF
        ELSE
            CALL USTOP(
     +     'HFB ERROR: UNKNOWN COMBINATION OF LAY, ROW, AND COL TO '//
     +     'MAKE FLOW BARRIER'//NL//'ERROR FOR HFB #: '//NUM2STR(II)//
     +     ' AND LAY, ROW,COL,ROW2,COL2,LAY2: '//NL//NUM2STR(K1)//' '
     +      //NUM2STR(I1)//' '//NUM2STR(J1)//' '//NUM2STR(I2)//' '
     +      //NUM2STR(J2)//' '//NUM2STR(K2)
     +                )
        END IF
C OLD METHOD
C          I1 = MIN(HFB(II)%I1,HFB(II)%I2)
C          J1 = MIN(HFB(II)%J1,HFB(II)%J2)
C          I2 = MAX(HFB(II)%I1,HFB(II)%I2)
C          J2 = MAX(HFB(II)%J1,HFB(II)%J2)

        HFB(II)%K1 = K1
        HFB(II)%I1 = I1
        HFB(II)%J1 = J1
        HFB(II)%I2 = I2
        HFB(II)%J2 = J2
        HFB(II)%K2 = K2
        ID = I2 - I1
        JD = J2 - J1
        KD = K2 - K1                                                    !seb added fix for vertical flow through barries
        IF(K2==0) KD=0
        IF ((ID < Z .OR. ID.GT.1 .OR. JD < Z .OR. JD.GT.1 .OR.
     +      ID.EQ.JD) .AND. KD == Z) THEN
C
C4------CELLS ARE NOT ADJACENT. PRINT ERROR MESSAGE AND SET ERROR FLAG.
   80     WRITE (IOUT,1) II-IB1+1
          !
    1     FORMAT (1X,'ERROR DETECTED IN LOCATION DATA OF BARRIER NO. ',
     +            I6)
          IERFLG = ONE
        ENDIF
   10 CONTINUE
C
C5------HALT EXECUTION IF ERRORS ARE DETECTED.
      IF (IERFLG == ONE) 
     +          CALL USTOP('HFB ERROR DETECTED. PLEASE SEE LIST FILE.')
C
C6------RETURN
      !RETURN
      END SUBROUTINE
C      
      SUBROUTINE SGWF2HFB7RL(NLIST,LSTBEG,MXHFB,INPACK,
     +                       IOUT,LABEL,NCOL,NROW,NLAY,IPRFLG)
C     ******************************************************************
C     Read and print a list of HFB barriers.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHFBMODULE,             ONLY:HFBRT,HFB,HAS_NWT
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      USE FILE_IO_INTERFACE,        ONLY: READ_TO_DATA
      USE CONSTANTS,                ONLY: NL, BLN, Z, ONE, DZ
      USE OPENSPEC
      CHARACTER(*) LABEL
      CHARACTER(768):: LINE,FNAME
      CHARACTER(1),DIMENSION(120):: DASH
      TYPE(WARNING_TYPE):: WRN
      !DATA DASH/120*'-'/
      !DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1------Check for and decode EXTERNAL and SFAC records.
      CALL WRN%INIT()
      DASH = '-'
      IN = INPACK
      ICLOSE = Z
      CALL READ_TO_DATA(LINE,IN,IOUT)
      !READ(IN,'(A)') LINE
      SFAC = 1.
      LLOC = ONE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN = I
         IF(IPRFLG.EQ.1) WRITE(IOUT,111) IN
         !
  111    FORMAT(1X,'Reading list on unit ',I4)
         !READ(IN,'(A)') LINE
         CALL READ_TO_DATA(LINE,IN,IOUT)
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME = LINE(ISTART:ISTOP)
         !IN = NUNOPN
         !IF(IPRFLG.EQ.1) WRITE(IOUT,115) IN,FNAME
         !
  115    FORMAT(1X,/1X,'OPENED FILE ON UNIT ',I4,':',/1X,A)
         OPEN(NEWUNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         IF(IPRFLG.EQ.1) WRITE(IOUT,115) IN,FNAME
         ICLOSE = ONE
         !READ(IN,'(A)') LINE
         CALL READ_TO_DATA(LINE,IN,IOUT)
      END IF
      LLOC = ONE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF (IPRFLG.EQ.1) THEN
            WRITE(IOUT,116) SFAC
            !
  116       FORMAT(1X,'LIST SCALING FACTOR= ',ES12.5)
         ENDIF
         !READ(IN,'(A)') LINE
         CALL READ_TO_DATA(LINE,IN,IOUT)
      END IF
C
C2------Define label for printout.
      NBUF = LEN(LABEL)+3
      IF (IPRFLG.EQ.1) THEN
           WRITE(IOUT,103) LABEL
           WRITE(IOUT,104) (DASH(J),J=1, NBUF)
  103    FORMAT(1X,/1X,A)
  104    FORMAT(1X,400A)
      ENDIF
C
C3------Loop through the number of cells to read.
      N = NLIST+LSTBEG-1
      DO 250 II=LSTBEG,N
C
C4------Read a line into the buffer.  (The first line has already been read
C4------in order to scan for EXTERNAL and SFAC records.)
      !IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
      IF(II.NE.LSTBEG) CALL READ_TO_DATA(LINE,IN,IOUT)
C
C5------Read the non-optional values from a line.
      LLOC = ONE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K1,R,IOUT,IN)                !seb CHANGED K TO K1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FACTOR,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K2,R,0,IN)                  !seb Supress error if Lay2 not present
C seb CAN COMMENT NEXT LINE TO FORCE LAYER ROUTING FOR ALL K2 CELLS
      IF (K2.EQ.K1) K2=0  !DO NOT DO LAYER ROUTING IF SAME LAYER
      !
      IF (K2 > Z) THEN
        IF (I1.NE.I2.OR.J1.NE.J2) THEN
            IF(HAS_NWT) THEN
                        HFBRT=HFBRT+1          !ROUTED CELL ONLY GOES IN HORIZONTAL DIRECTION AND NOT VERTICAL
            ELSE
                        CALL WRN%ADD(TRIM(LINE)//NL)
                        K2=0
            END IF
        END IF
      END IF
      !
      HFB(II)%K1 = K1
      HFB(II)%I1 = I1
      HFB(II)%J1 = J1
      HFB(II)%I2 = I2
      HFB(II)%J2 = J2
      HFB(II)%K2 = K2
      HFB(II)%HYDCHR = FACTOR*SFAC
C
C6------Write the values that were read.
      NN = II-LSTBEG+1
      IF (IPRFLG.EQ.1) THEN
205     FORMAT(1X,I6,2X,I5,1X,4(2X,I5),2X,ES11.4,2X,I5)
        IF (K2 > Z) THEN
            WRITE(IOUT,205) NN,K1,I1,J1,I2,J2,HFB(II)%HYDCHR,K2
        ELSE
            WRITE(IOUT,205) NN,K1,I1,J1,I2,J2,HFB(II)%HYDCHR,K1
        END IF
      END IF
C
C7------Check for illegal grid location.
      IF(K1.LT.1 .OR. K1.GT.NLAY) THEN
         CALL USTOP('HFB LAYER NUMBER IN LIST IS OUTSIDE OF THE GRID')
      END IF
      IF(K2 < Z .OR. K2.GT.NLAY) THEN
         CALL USTOP('HFB LAYER NUMBER IN LIST IS OUTSIDE OF THE GRID')
      END IF
      IF(I1.LT.1 .OR. I1.GT.NROW .OR. I2.LT.1 .OR. I2.GT.NROW) THEN
         CALL USTOP('HFB ROW NUMBER IN LIST IS OUTSIDE OF THE GRID')
      END IF
      IF(J1.LT.1 .OR. J1.GT.NCOL .OR. J2.LT.1 .OR. J2.GT.NCOL) THEN
         CALL USTOP('HFB COLUMN NUMBER IN LIST IS OUTSIDE OF THE GRI ')
      END IF
  250 CONTINUE
      !
      IF(WRN%RAISED)CALL WRN%CHECK('   *** HFB INPUT WARNING ***'//BLN//
     +'INPUT FOUND THAT A BARRIER WAS BETWEEN ADJECENT MODEL CELLS '//
     +'AND [Layer2] WAS SPECIFIED.'//NL//'THAT IS'//BLN//
     +'IROW1 /= IROW2 or ICOL1 /= ICOL2 and Layer2 > 0'//BLN//
     +'THE NWT SOLVER IS NOT INUSE, SO Layer2 IS CHANGED TO 0'//NL//
     +'(HFB ACTS AS A NORMAL BARRIER RATHER THEN ROUTING FLOW)'//
     +BLN//'THIS INPUT SHOULD ONLY BE USED IF YOU WISH TO HAVE HFB'//
     +'ROUTE FLOW BETWEEN LAYERS (Layer AND Layer2)'//NL//
     +'AND ARE USING THE NWT SOLVER.'//NL, IN, IOUT)
      !
      IF(ICLOSE /= Z) CLOSE(UNIT=IN)
C
C8------Return.
      !RETURN
      END SUBROUTINE
C
      SUBROUTINE SGWF2HFB7SUB(IN,PACK,IOUT,PTYP,MXHFB,
     1                MXACTFB,NHFB,LABEL,IPRT)
C     ******************************************************************
C     Read a parameter name, look it up in the list of parameters,
C     and substitute values into active part of HFB array.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHFBMODULE,ONLY:HFB
      USE PARAMMODULE
      USE CONSTANTS,   ONLY: Z, DZ, ONE
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      CHARACTER(*):: PACK, PTYP
      CHARACTER(*):: LABEL
      CHARACTER(768):: LINE
      CHARACTER(MXNAMLEN):: CTMP1,CTMP2,CTMP3,CTMP4                     !seb CHANGED FROM CHAR LEN OF 10
C     ------------------------------------------------------------------
C
C1------The Listing File file unit is the absolute value of IOUTU.  
C1------Read the parameter name.
C      IOUT = ABS(IOUTU)
      READ(IN,'(A)') LINE
      LLOC = ONE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      WRITE(IOUT,1) LINE(ISTART:ISTOP)
      !
    1 FORMAT(/,' Parameter:  ',A)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        CALL USTOP('BLANK PARAMETER NAME IN THE '//PACK//' FILE.')
      END IF
C
C2------Find the parameter in the list of parameters.
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1, IPSUM
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            WRITE(IOUT,11) PARNAM(IP),PARTYP(IP),PACK,PTYP
            !
   11       FORMAT(1X,'Parameter type conflict:',/
     1        1X,'Named parameter:',A,' was defined as type:',A,/
     2        1X,'However, this parameter is used in the ',A,
     3          ' file, so it should be type:',A)
            CALL USTOP('HFB Parameter type conflict. SEE LIST FILE ')
          END IF
C
C3------Set indices to point to the barriers that correspond to the
C3------specified parameter.
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NI = ONE
C
C4------Check that the parameter is not already active.
          IF (IACTIVE(IP) > Z) THEN
            CALL USTOP('PARAMETER "'//PARNAM(IP)//
     +                '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD')
          ENDIF
C
C5------Set the active flag.
          IACTIVE(IP)=NI
C
C6------Accumulate the total number of active barriers in the list.
          NHFB=NHFB+NLST
          IF(NHFB.GT.MXACTFB) THEN
            CALL USTOP('HFB ERROR THE NUMBER OF ACTIVE LIST ENTRIES ('//
     +         NUM2STR(NHFB)//' IS GREATER THAN THE MAXIMUM ALLOWED ('//
     +         NUM2STR(MXACTFB)//')'
     +                )
          END IF
C
C7------Write label for barrier values if IOUTU is positive.
          IF(IPRT > Z) THEN
             WRITE(IOUT,'(1X,A)') LABEL
             WRITE(IOUT,84)
   84        FORMAT(1X,56('-'))
          END IF
C
C8------Copy the values from the paramter location into the front part
C8------of the list where the currently active list is kept.
          DO 90 I=1,NLST
            II=NHFB-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            HFB(II)=HFB(III)
C
C8A-----Scale HYDCHR by the parameter value.
            HFB(II)%HYDCHR = HFB(II)%HYDCHR*B(IP)
            IL1=HFB(II)%K1
            IR1=HFB(II)%I1
            IC1=HFB(II)%J1
            IR2=HFB(II)%I2
            IC2=HFB(II)%J2
            IL2=HFB(II)%K2
            IF(IPRT > Z) THEN
              IF(IL2 > Z) THEN
            WRITE(IOUT,89) II,IL1,IR1,IC1,IR2,IC2,HFB(II)%HYDCHR,IL2
              ELSE
            WRITE(IOUT,89) II,IL1,IR1,IC1,IR2,IC2,HFB(II)%HYDCHR,IL1
              END IF
   89         FORMAT(1X,I6,2X,I5,1X,4(2X,I5),2X,ES11.4,2X,I5)
            END IF
   90     CONTINUE
C
C8B------After moving the data, return.
          RETURN
        END IF
  100 CONTINUE
C
C9------All parameter names have been checked without finding the
C9------parameter. Write an error message and stop.
      !
      CALL USTOP(' The '//PACK//
     1   ' file specifies an undefined parameter:'//LINE(ISTART:ISTOP))
C
      END SUBROUTINE
C
      SUBROUTINE GWF2HFB7UPW(IGRID)
C     ******************************************************************
C     MODIFY HORIZONTAL BRANCH CONDUCTANCES IN VARIABLE-TRANSMISSIVITY
C     LAYERS TO ACCOUNT FOR HORIZONTAL FLOW BARRIERS IN MODFLOW-NWT. 
C     STORE UNMODIFIED HORIZONTAL CONDUCTANCE IN HFB(7,#) TO ALLOW 
C     CALCULATION OF SENSITIVITIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,HNEW,LAYHDT,CR,CC,CV,BOTM,LBOTM,
     1                      DELR,DELC,LAYCBD
      USE GWFHFBMODULE,ONLY:NHFB,HFB
      USE CONSTANTS,   ONLY: Z, ONE, DZ
C     ------------------------------------------------------------------
C
C1------Set pointers to the specified grid.
      CALL SGWF2HFB7PNT(IGRID)
C
C2------FOR EACH BARRIER, MODIFY HORIZONTAL BRANCH CONDUCTANCES IF LAYER
C2------IS CONVERTIBLE.
      DO 10 II=1,NHFB
        K1 = HFB(II)%K1
        K2 = HFB(II)%K2
C
C3-----IF LAYHDT=0, THICKNESS AND CONDUCTANCE DO NOT VARY, AND
C3-----MODIFICATION OF CONDUCTANCE DUE TO BARRIER WAS DONE IN
C3-----SGWF1HFBMC
        IF (LAYHDT(K1) > Z) THEN
C
C4------CELL (J1,I1,K1) IS THE ONE WHOSE HORIZONTAL BRANCH
C4------CONDUCTANCES ARE TO BE MODIFIED.
          I1 = HFB(II)%I1
          J1 = HFB(II)%J1
C
C5------CELL (J2,I2,K1) IS THE CELL NEXT TO CELL (J1,I1,K1) AND
C5------SEPARATED FROM IT BY THE BARRIER.
          I2 = HFB(II)%I2
          J2 = HFB(II)%J2
          HCDW = HFB(II)%HYDCHR
C
C6------IF I1=I2, MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG ROW
C6------DIRECTION.
          IF (I1.EQ.I2) THEN
C
C7------IF CR(J1,I1,K1) NOT = 0, CELLS ON EITHER SIDE OF BARRIER ARE
C7------ACTIVE
            IF (CR(J1,I1,K1).NE.0.  .OR. K2 > Z) THEN
C
C
C9------STORE UNMODIFIED CR FOR CALCULATING SENSITIVITIES
              HFB(II)%CON1 = CR(J1,I1,K1)
C
C10-----MODIFY CR(J1,I1,K1) TO ACCOUNT FOR BARRIER.
C seb If Layer Route Enabled Zero Out Existing Conductance
              IF (K2 > Z) THEN
                HFB(II)%CON2 = CR(J1,I1,K2)
                CR(J1,I1,K1) = DZ
                CR(J1,I1,K2) = DZ
                CALL HFB9HARMCON(I1,J1,K1,I2,J2,K2,II)
              ELSE
                IF (HCDW < Z) THEN
                  CR(J1,I1,K1) = CR(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = HCDW
                  CR(J1,I1,K1) = TDW*CR(J1,I1,K1)*DELC(I1)/
     +                          (TDW*DELC(I1)+CR(J1,I1,K1))
                END IF
              END IF
            ENDIF
C
C11-----CASE OF J1=J2. MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG
C11-----COLUMN DIRECTION.
          ELSEIF (J1.EQ.J2 .AND. I1.NE.I2) THEN
C
C12-----IF CC(J1,I1,K1) NOT = 0, CELLS ON EITHER SIDE OF BARRIER ARE
C12-----ACTIVE
            IF (CC(J1,I1,K1).NE.0. .OR. K2 > Z) THEN
C
C14-----STORE UNMODIFIED CC FOR CALCULATING SENSITIVITIES.
              HFB(II)%CON1 = CC(J1,I1,K1)
C
C15-----MODIFY CC(J1,I1,K1) TO ACCOUNT FOR BARRIER.
C awb If Layer Route Enabled Zero Out Existing Conductance
              IF (K2 > Z) THEN
                HFB(II)%CON2 = CC(J1,I1,K2)
                CC(J1,I1,K1) = DZ
                CC(J1,I1,K2) = DZ
                CALL HFB9HARMCON(I1,J1,K1,I2,J2,K2,II)
              ELSE 
                IF (HCDW < Z) THEN
                  CC(J1,I1,K1) = CC(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = HCDW
                  CC(J1,I1,K1) = TDW*CC(J1,I1,K1)*DELR(J1)/
     +                          (TDW*DELR(J1)+CC(J1,I1,K1))
                END IF
              END IF
            ENDIF
C
C11-----CASE OF J1=J2 and I1=I2. MODIFY VERTICAL BRANCH CONDUCTANCE BETWEEN A LAYER AND THE NEXT
C       LOWER LAYER 
          ELSEIF(J1.EQ.J2 .AND. I1.EQ.I2 .AND. K2 > Z) THEN
            IF(LAYCBD(K1) /= Z) THEN
              WRITE(IOUT,'(/A,/3A,I6,A,/A)')
     +           REPEAT('#',50),
     +          'WARNING: HFB VERTICAL BARRIER DOES NOT WORK ',
     +          'WHEN THERE IS A 3D QUASI-CONFINING LAYER (LAYCBD/=0)',
     +          'THE ',II, 'th BARRIER WILL NOT BE APPLIED',
     +           REPEAT('#',50)
              CYCLE
            END IF
            IF (CV(J1,I1,K1).NE.0.) THEN
C14-----STORE UNMODIFIED CV FOR CALCULATING SENSITIVITIES.
              HFB(II)%CON1 = CV(J1,I1,K1)
C
C15-----MODIFY CV(J1,I1,K1) TO ACCOUNT FOR BARRIER.
                IF (HCDW < Z) THEN
                  CV(J1,I1,K1) = CV(J1,I1,K1) * ABS(HFB(II)%HYDCHR)
                ELSE
                  TDW = DELR(J1)*DELC(I1)*HCDW
                  CV(J1,I1,K1)= ( CV(J1,I1,K1)**(-1) + TDW**(-1) )**(-1)!SIMILAR FORMULATION TO MF05 Eqn 5-25 EXCEPT HCDW IS TREATED LIKE A QUASICONFING BARRIER
                END IF
            ENDIF
          ENDIF
        ENDIF
   10 CONTINUE
C
C16-----RETURN
      !RETURN
      END SUBROUTINE
C      
      SUBROUTINE GWF2HFB7DA(IGRID)
C  Deallocate HFB data for a grid.
      USE GWFHFBMODULE
C
        DEALLOCATE(GWFHFBDAT(IGRID)%ICEL_HFB, STAT=IGNORE)  !STAT PREVENTS DEALLOCATION ERRORS WHEN ICEL_HFB IS NOT ALLOCATED
        DEALLOCATE(GWFHFBDAT(IGRID)%HFBRT)
        DEALLOCATE(GWFHFBDAT(IGRID)%MXHFBNP)
        DEALLOCATE(GWFHFBDAT(IGRID)%MXHFB)
        DEALLOCATE(GWFHFBDAT(IGRID)%NHFB)
        DEALLOCATE(GWFHFBDAT(IGRID)%IPRHFB)
        DEALLOCATE(GWFHFBDAT(IGRID)%NHFBNP)
        DEALLOCATE(GWFHFBDAT(IGRID)%NPHFB)
        DEALLOCATE(GWFHFBDAT(IGRID)%IHFBPB)
        DEALLOCATE(GWFHFBDAT(IGRID)%HFB)
        DEALLOCATE(GWFHFBDAT(IGRID)%NACTHFB)
        DEALLOCATE(GWFHFBDAT(IGRID)%HAS_NWT)
        DEALLOCATE(GWFHFBDAT(IGRID)%NO_RP)
C
C NULLIFY LOCAL POITNERS
      IF(IGRID.EQ.1) THEN
        ICEL_HFB=>NULL()
        HFBRT   =>NULL()
        MXHFBNP =>NULL()
        MXHFB   =>NULL()
        NHFB    =>NULL()
        IPRHFB  =>NULL()
        NHFBNP  =>NULL()
        NPHFB   =>NULL()
        IHFBPB  =>NULL()
        HFB     =>NULL()
        NACTHFB =>NULL()
        HAS_NWT =>NULL()
        NO_RP   =>NULL()
      END IF
      !RETURN
      END SUBROUTINE
C
      SUBROUTINE SGWF2HFB7PNT(IGRID)
C  Set pointers to HFB data for a grid.
      USE GWFHFBMODULE
C
        NACTHFB =>GWFHFBDAT(IGRID)%NACTHFB
        ICEL_HFB=>GWFHFBDAT(IGRID)%ICEL_HFB
        HFBRT   =>GWFHFBDAT(IGRID)%HFBRT
        MXHFBNP =>GWFHFBDAT(IGRID)%MXHFBNP
        MXHFB   =>GWFHFBDAT(IGRID)%MXHFB
        NHFB    =>GWFHFBDAT(IGRID)%NHFB
        IPRHFB  =>GWFHFBDAT(IGRID)%IPRHFB
        NHFBNP  =>GWFHFBDAT(IGRID)%NHFBNP
        NPHFB   =>GWFHFBDAT(IGRID)%NPHFB
        IHFBPB  =>GWFHFBDAT(IGRID)%IHFBPB
        HFB     =>GWFHFBDAT(IGRID)%HFB
        HAS_NWT =>GWFHFBDAT(IGRID)%HAS_NWT
        NO_RP   =>GWFHFBDAT(IGRID)%NO_RP
C
      !RETURN
      END SUBROUTINE
C
      SUBROUTINE SGWF2HFB7PSV(IGRID)
C  Save pointers to HFB data for a grid.
      USE GWFHFBMODULE
C
        GWFHFBDAT(IGRID)%NACTHFB =>NACTHFB
        GWFHFBDAT(IGRID)%ICEL_HFB=>ICEL_HFB
        GWFHFBDAT(IGRID)%HFBRT   =>HFBRT
        GWFHFBDAT(IGRID)%MXHFBNP =>MXHFBNP
        GWFHFBDAT(IGRID)%MXHFB   =>MXHFB
        GWFHFBDAT(IGRID)%NHFB    =>NHFB
        GWFHFBDAT(IGRID)%IPRHFB  =>IPRHFB
        GWFHFBDAT(IGRID)%NHFBNP  =>NHFBNP
        GWFHFBDAT(IGRID)%NPHFB   =>NPHFB
        GWFHFBDAT(IGRID)%IHFBPB  =>IHFBPB
        GWFHFBDAT(IGRID)%HFB     =>HFB
        GWFHFBDAT(IGRID)%HAS_NWT =>HAS_NWT
        GWFHFBDAT(IGRID)%NO_RP   =>NO_RP
C
      !RETURN
      END SUBROUTINE
C##################################################################################################################
C NEW FUNCTION/SUBROUTINES FOR ROUTED FLOW IN NWT BY SCOTT BOYCE 7/15/2012
C##################################################################################################################
      PURE LOGICAL FUNCTION HFBROUTECELL(IC,IR,IL,IJ)
C FUNCTION IS USED IN NWT_SOLVER.f TO ACT AS SWITCH TO IDENTIFY CELLS THAT CONTAIN A BARRIER WITH ROUTE FLOW  
C RETURNS TRUE IF CURRENT CELL (IC,IR,IL) HAS A BARRIER WITH ROUTE FLOW  
      USE GWFHFBMODULE, ONLY: HFB_IN_USE,HFBRT,ICEL_HFB
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IC,IR,IL,IJ
      !
      IF(HFB_IN_USE) THEN
          IF(HFBRT > 0) THEN
              !
              !RETURN TRUE IF IJ IS A CELL WITH A ROUTE BARRIER
              !ICEL_HFB(:,1) CONTAINS ALL CELLS IJ WITH A BARRIER WITH LAYER ROUTE
              HFBROUTECELL=ANY(ICEL_HFB(:,1).EQ.IJ)
          ELSE
              HFBROUTECELL=.FALSE.
          END IF
      ELSE
          HFBROUTECELL=.FALSE.
      END IF
      !
      END FUNCTION
!!!      LOGICAL FUNCTION HFBROUTECELL(IC,IR,IL,IJ)
!!!C FUNCTION IS USED IN NWT_SOLVER.f TO ACT AS SWITCH TO IDENTIFY CELLS THAT CONTAIN A BARRIER WITH ROUTE FLOW  
!!!C RETURNS TRUE IF CURRENT CELL (IC,IR,IL) HAS A BARRIER WITH ROUTE FLOW  
!!!C SET IJ=0 IF YOU WANT TO USE IC,IR,IL TO CHECK CELL
!!!      USE GLOBAL,       ONLY: IUNIT
!!!      USE GWFNWTMODULE, ONLY: Icell
!!!      USE GWFHFBMODULE, ONLY: HFBRT,ICEL_HFB
!!!      IMPLICIT NONE
!!!      INTEGER IC,IR,IL,IJ
!!!      INTEGER::II
!!!      
!!!      HFBROUTECELL=.FALSE.
!!!      IF (IUNIT(21) == Z) RETURN
!!!      IF (HFBRT == Z)     RETURN
!!!      !
!!!      IF (IJ == Z) IJ=Icell(IC,IR,IL)
!!!      !
!!!      !RETURN TRUE IF IJ IS A CELL WITH A ROUTE BARRIER
!!!      !ICEL_HFB(:,1) CONTAINS ALL CELLS IJ WITH A BARRIER WITH LAYER ROUTE
!!!      HFBROUTECELL=ANY(ICEL_HFB(:,1).EQ.IJ)
!!!      !
!!!      !BRUTE FORCE METHOD IF REQUIRED INSTEAD
!!!      !DO II=1,HFBRT
!!!      !  IF(IJ.EQ.ICEL_HFB(II,1))THEN
!!!      !    HFBROUTECELL=.TRUE.
!!!      !    EXIT
!!!      !  END IF
!!!      !END DO
!!!      !
!!!      END FUNCTION

      SUBROUTINE ORDERCELLHFB()
C BUILD RECORD OF LOCATIONS IN NWT OF CELLS THAT CONTAIN A BARRIER WITH LAYER ROUTE
C ICEL_HFB(JJ,1): NWT ROW
C ICEL_HFB(JJ,2): HFB ROW
C ICEL_HFB(JJ,3): LOCATION OF FAULT WITH RESPECT TO CELL
C       2
C     |----|
C   1 | JJ | 3
C     |----|
C       4
      USE GWFNWTMODULE, ONLY: Icell
      USE GWFHFBMODULE, ONLY: HFB, NHFB,HFBRT,ICEL_HFB
      USE CONSTANTS,    ONLY: Z, ONE
      IMPLICIT NONE
      INTEGER:: II,JJ,K1,I1,J1,K2,I2,J2
      
      IF (HFBRT == Z) RETURN  !NO ROUTED CELLS
      
      ICEL_HFB=-999
      JJ = ONE
      DO II=1, NHFB
        K2=HFB(II)%K2
        !
        IF (K2 == Z) CYCLE
        !
        K1=HFB(II)%K1
        I1=HFB(II)%I1
        J1=HFB(II)%J1
        I2=HFB(II)%I2
        J2=HFB(II)%J2
        !
        IF (I1.EQ.I2 .AND. J1.EQ.J2) CYCLE!FLOW IS GOING BETWEEN LAYERS DIRECTLY NEXT TO EACH OTHER, NO ROUTING
        !
        ICEL_HFB(JJ,1)=Icell(J1, I1, K1)  !NWT LOCATION
        ICEL_HFB(JJ,2)=II                 !HFB LOCATION
        IF(I1.EQ.I2) THEN
          ICEL_HFB(JJ,3)=3                !BARRIER TO THE RIGHT OF CELL
        ELSE
          ICEL_HFB(JJ,3)=4                !BARRIER TO THE BOTTOM OF CELL
        END IF
        !
        JJ=JJ+1
        !
        ICEL_HFB(JJ,1)=Icell(J2, I2, K2)  !NWT LOCATION
        ICEL_HFB(JJ,2)=II                 !HFB LOCATION
        IF(I1.EQ.I2) THEN
          ICEL_HFB(JJ,3)=1                !BARRIER TO THE LEFT OF CELL
        ELSE
          ICEL_HFB(JJ,3)=2                !BARRIER TO THE TOP OF CELL
        END IF
        JJ=JJ+1
        END DO
      END SUBROUTINE
      
      SUBROUTINE  FILLINDEXHFB(IC,IR,IL,IJ,IDX)  !NOTE THAT JJ FROM NWT/FILLINDEX BECOMES IDX
C Suplements SUBROUTINE FILLINDEX by including indecies of cells with barriers that route flow
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND
      USE GWFNWTMODULE, ONLY: Icell, JA
      USE GWFHFBMODULE, ONLY: HFB,HFBRT,ICEL_HFB
      USE CONSTANTS,    ONLY: Z, ONE, UNO
      IMPLICIT NONE
      INTEGER:: IC,IR,IL,IJ,IDX
      INTEGER:: II, JJ, KK
      INTEGER, DIMENSION(4)::BR
C BR IS REFERANCE TO WHERE BARRIER IS LOCATED FROM CELL IJ
C BR CONTAINS ROWS OF HFB THAT CORRESPOND TO BARRIERS IN CELL IJ
C BR(1) > Z IF BARRIER IS TO RIGHT  OF CELL IN PLAN VIEW (J-1)
C BR(2) > Z IF BARRIER IS TO TOP    OF CELL IN PLAN VIEW (I-1)
C BR(3) > Z IF BARRIER IS TO LEFT   OF CELL IN PLAN VIEW (J+1)
C BR(4) > Z IF BARRIER IS TO BOTTOM OF CELL IN PLAN VIEW (I+1)
      BR = Z
      DO II=1,HFBRT      
        IF(IJ.EQ.ICEL_HFB(II,1))THEN
          JJ=ICEL_HFB(II,3)  !LOCATION OF BARRIER: 1,2,3, or 4
          BR( JJ)=ICEL_HFB(II,2)
        END IF
      END DO    
      
      IF ( IR.GT.1 ) THEN
        IF(BR(2) > Z)THEN
          II=HFB(BR(2))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(2))%J1
          KK=HFB(BR(2))%K1
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            JA(IDX) = Icell(JJ,II,KK)
            IDX = IDX + ONE
          END IF
        ELSE
          IF ( IBOUND(IC,IR-1,IL) /= Z ) THEN
            JA(IDX) = Icell(ic, ir-1, il)
            IDX = IDX + ONE
          END IF
        END IF
      ENDIF
      !
      IF ( ic.GT.1 ) THEN
        IF(BR(1) > Z)THEN
          II=HFB(BR(1))%I1  !REFER TO ROUTED CELL ON LEFT (IC-1)
          JJ=HFB(BR(1))%J1
          KK=HFB(BR(1))%K1
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            JA(IDX) = Icell(JJ,II,KK)
            IDX = IDX + ONE
          END IF
        ELSE
          IF ( IBOUND(IC-1,IR,IL) /= Z ) THEN
            JA(IDX) = Icell(ic-1, ir, il)
            IDX = IDX + ONE
          END IF
        END IF
      ENDIF
      !
      IF ( ic.LT.NCOL ) THEN
        IF(BR(3) > Z)THEN
          II=HFB(BR(3))%I2  !REFER TO ROUTED CELL ON RIGHT (IC+1)
          JJ=HFB(BR(3))%J2
          KK=HFB(BR(3))%K2
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            JA(IDX) = Icell(JJ,II,KK)
            IDX = IDX + ONE
          END IF
        ELSE
          IF ( IBOUND(IC+1,IR,IL) /= Z ) THEN
            JA(IDX) = Icell(ic+1, ir, il)
            IDX = IDX + ONE
          END IF
        END IF
      ENDIF
      !
      IF ( ir.LT.NROW ) THEN
        IF(BR(4) > Z)THEN
          II=HFB(BR(4))%I2  !REFER TO ROUTED CELL ON BOTTOM (IR+1)
          JJ=HFB(BR(4))%J2
          KK=HFB(BR(4))%K2
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            JA(IDX) = Icell(JJ,II,KK)
            IDX = IDX + ONE
          END IF
        ELSE
          IF ( IBOUND(IC,IR+1,IL) /= Z ) THEN
            JA(IDX) = Icell(ic, ir+1, il)
            IDX = IDX + ONE
          END IF
        END IF
      ENDIF
        
      END SUBROUTINE
      
      SUBROUTINE COUNTACTIVEHFB(IC,IR,IL,IJ,IDX)  !NOTE THAT JJ IS CHANGED TO IDX
C Suplemental to SUBROUTINE COUNTACTIVE to ensure that cells with barriers that route flow are included in active count
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND
      USE GWFNWTMODULE, ONLY: Icell, JA
      USE GWFHFBMODULE, ONLY: HFB,HFBRT,ICEL_HFB
      USE CONSTANTS,    ONLY: Z
      IMPLICIT NONE
      INTEGER:: IC,IR,IL,IJ,IDX
      INTEGER:: II, JJ, KK
      INTEGER, DIMENSION(4)::BR
C BR IS REFERANCE TO WHERE BARRIER IS LOCATED FROM CELL IJ
C BR CONTAINS ROWS OF HFB THAT CORRESPOND TO BARRIERS IN CELL IJ
C BR(1) > Z IF BARRIER IS TO RIGHT  OF CELL IN PLAN VIEW (J-1)
C BR(2) > Z IF BARRIER IS TO TOP    OF CELL IN PLAN VIEW (I-1)
C BR(3) > Z IF BARRIER IS TO LEFT   OF CELL IN PLAN VIEW (J+1)
C BR(4) > Z IF BARRIER IS TO BOTTOM OF CELL IN PLAN VIEW (I+1)
      BR=0
      DO II=1,HFBRT      
        IF(IJ.EQ.ICEL_HFB(II,1))THEN
          JJ=ICEL_HFB(II,3)  !LOCATION OF BARRIER: 1,2,3, or 4
          BR( JJ)=ICEL_HFB(II,2)
        END IF
      END DO  
      
      IF ( ir.GT.1 ) THEN
        IF(BR(2) > Z)THEN
          II=HFB(BR(2))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(2))%J1
          KK=HFB(BR(2))%K1
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            IDX = IDX + 1
          END IF
        ELSE
          IF ( IBOUND(IC,IR-1,IL) /= Z ) THEN
            IDX = IDX + 1
          END IF
        END IF
      ENDIF
      !
      IF ( ic.GT.1 ) THEN
        IF(BR(1) > Z)THEN
          II=HFB(BR(1))%I1  !REFER TO ROUTED CELL ON LEFT (IC-1)
          JJ=HFB(BR(1))%J1
          KK=HFB(BR(1))%K1
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            IDX = IDX + 1
          END IF
        ELSE
          IF ( IBOUND(IC-1,IR,IL) /= Z ) THEN
            IDX = IDX + 1
          END IF
        END IF
      ENDIF
      !
      IF ( ic.LT.NCOL ) THEN
        IF(BR(3) > Z)THEN
          II=HFB(BR(3))%I2  !REFER TO ROUTED CELL ON RIGHT (IC+1)
          JJ=HFB(BR(3))%J2
          KK=HFB(BR(3))%K2
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            IDX = IDX + 1
          END IF
        ELSE
          IF ( IBOUND(IC+1,IR,IL) /= Z ) THEN
            IDX = IDX + 1
          END IF
        END IF
      ENDIF
      !
      IF ( ir.LT.NROW ) THEN
        IF(BR(4) > Z)THEN
          II=HFB(BR(4))%I2  !REFER TO ROUTED CELL ON BOTTOM (IR+1)
          JJ=HFB(BR(4))%J2
          KK=HFB(BR(4))%K2
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
            IDX = IDX + 1
          END IF
        ELSE
          IF ( IBOUND(IC,IR+1,IL) /= Z ) THEN
            IDX = IDX + 1
          END IF
        END IF
      ENDIF
      
      END SUBROUTINE

      SUBROUTINE HFB9HARMCON(I1,J1,K1,I2,J2,K2,IHFB)
C ASSEMBLE CONDUCTANCE VIA HARMONIC MEAN BETWEEN BARRIER ROUTE CELLS
C VARIABLES ENDING IN 1 REFER TO LAYER K1
C VARIABLES ENDING IN 2 REFER TO LAYER K2
      USE GLOBAL,      ONLY:NCOL,NROW,IBOUND,CR,CC,DELR,DELC,
     1                      IUNIT,HNEW,BOTM,LBOTM,LAYHDT,IOUT
      USE GWFHFBMODULE,ONLY: HFB
      USE CONSTANTS,   ONLY: Z, DZ, UNO
      IMPLICIT NONE
C CELL LOCATIONS
      INTEGER:: I1,J1,K1,I2,J2,K2,IHFB
C LOCAL VARIABLES
      DOUBLE PRECISION:: HD1,HD2,TOP1,TOP2,BOT1,BOT2
      DOUBLE PRECISION:: HK1,HK2,HANI1,HANI2
      DOUBLE PRECISION:: T1,T2,THICK1,THICK2,THKAVG,WIDTH
      DOUBLE PRECISION:: ZERO, ONE, TWO
      !
      ZERO=0.0
      ONE =1.0
      TWO =2.0
      !
      HD1=HNEW(J1,I1,K1)
      HD2=HNEW(J2,I2,K2)
      !
      TOP1=BOTM(J1,I1,LBOTM(K1)-1)
      TOP2=BOTM(J2,I2,LBOTM(K2)-1)
      !
      BOT1=BOTM(J1,I1,LBOTM(K1))
      BOT2=BOTM(J2,I2,LBOTM(K2))
      !
C LAY IS CONFINED SET THICK to TOP-BOT
      IF (LAYHDT(K1) == Z) THEN
        THICK1=TOP1-BOT1
      ELSE
C LAY IS CONVERTABLE
C HEAD HIGHER THAN TOP
        IF (IUNIT(62) /= Z) THEN  !NWT Solver icorporates Thickness later on
          THICK1=ONE
        ELSE
          IF (HD1.GT.TOP1) THEN
            THICK1=TOP1-BOT1
          ELSE
C HEAD IS THE TOP
            THICK1=HD1-BOT1
          END IF
        END IF
      END IF  
      IF(THICK1<DZ) THICK1=DZ
      !
      IF (LAYHDT(K2) == Z) THEN
        THICK2=TOP2-BOT2
      ELSE
        IF (IUNIT(62) /= Z) THEN  !NWT Solver icorporates Thickness later on
          THICK2=ONE
        ELSE
          IF (HD2.GT.TOP2) THEN
            THICK2=TOP2-BOT2
          ELSE
            THICK2=HD2-BOT2
          END IF
        END IF
      END IF
      IF(THICK2<DZ) THICK2=DZ
      !
      !GET CORRECT PARAMETERS BASED ON FLOW PACKAGE
      !NOTE THAT HANI1 HOLDS EITHER HANI(J1,I1,KHANI) OR CHANI(K1)
      IF (IUNIT(62) /= Z) THEN
        CALL HFBNWTPARAM(J1,I1,K1,HK1,HANI1)
        CALL HFBNWTPARAM(J2,I2,K2,HK2,HANI2)
      ELSEIF(IUNIT(23) /= Z) THEN
        CALL HFBLPFPARAM(J1,I1,K1,HK1,HANI1)
        CALL HFBLPFPARAM(J2,I2,K2,HK2,HANI2)
        !CHECK IF CELLS ARE WET
        IF(HD1.LT.BOT1 .OR. HD2.LT.BOT2)THEN
          HFB(IHFB)%RCON=ZERO
          RETURN
        END IF 
      ELSE
         CALL USTOP('ERROR: HFB WITH FLOW ROUTE DOES NOT SUPPORT '//
     +              'CURRENTLY SELECTED FLOW PACKAGE. USE UPW OR LPF')
      END IF
      !
      T1=HK1*THICK1
      T2=HK2*THICK2
      !
      IF(THICK1==DZ .AND. THICK2==DZ) THEN
          HFB(IHFB)%RCON=DZ
      ELSEIF(I1.EQ.I2) THEN !CALCULATE CR
         HFB(IHFB)%RCON=TWO*T2*T1*DELC(I1)/(T1*DELR(J2)+T2*DELR(J1))  !Same as CR
      ELSE              !CALCULATE CC
         T1=T1*HANI1
         T2=T2*HANI2 
         HFB(IHFB)%RCON=TWO*T2*T1*DELR(J1)/(T1*DELC(I2)+T2*DELC(I1))  !Same as CC
      END IF
C INCOPORATE THE BARRIER'S CONDUCTANCE
      IF (HFB(IHFB)%HYDCHR > Z) THEN
        THKAVG=0.5 * (THICK1+THICK2)                             !Harmonic Average Barrier Conductance With Route Conductance
        T1 = THKAVG*HFB(IHFB)%HYDCHR
        T2 = HFB(IHFB)%RCON
        IF(I1.EQ.I2) THEN
          HFB(IHFB)%RCON= T1*T2*DELC(I1)/(T1*DELC(I1)+T2)
        ELSE
          HFB(IHFB)%RCON= T1*T2*DELR(I1)/(T1*DELR(I1)+T2)        
        END IF
        ! 
      ELSEIF (HFB(IHFB)%HYDCHR < Z .AND. 
     +        ABS(UNO + HFB(IHFB)%HYDCHR) > 1D-10) THEN          !NOTE that 1 + -1 ~=0
        HFB(IHFB)%RCON= HFB(IHFB)%RCON * ABS(HFB(IHFB)%HYDCHR)   !Scale Route Conductance
      ELSE
        CONTINUE                                                 !Leave Route Conductance Alone
      END IF
      END SUBROUTINE
      
      SUBROUTINE HFBNWTPARAM(J,I,K,HKtemp,HANItemp)
C USE CORRECT MODULES TO OBTAIN PROPER GLOBABL VARIABLES FROM UPW
      USE GWFUPWMODULE,ONLY:HKUPW,CHANI,HANI 
      IMPLICIT NONE
      INTEGER::I,J,K
      DOUBLE PRECISION::HKtemp,HANItemp
      !
      DOUBLE PRECISION:: ZERO
      INTEGER:: KHANI
      
      ZERO=0.0
      
      HKtemp=HKUPW(J,I,K)

       IF(CHANI(K).LE.ZERO) THEN
           KHANI=-CHANI(K)
           HANItemp=HANI(J,I,KHANI)
        ELSE
           HANItemp=CHANI(K)
        END IF

      END SUBROUTINE
      
      SUBROUTINE HFBLPFPARAM(J,I,K,HKtemp,HANItemp)
C USE CORRECT MODULES TO OBTAIN PROPER GLOBABL VARIABLES FROM LPF
      USE GWFLPFMODULE,ONLY:HK,CHANI,HANI
      IMPLICIT NONE
      INTEGER::I,J,K
      DOUBLE PRECISION::HKtemp,HANItemp
      !
      DOUBLE PRECISION:: ZERO
      INTEGER::KHANI
      
      ZERO=0.0
      
      HKtemp=HK(J,I,K)

       IF(CHANI(K).LE.ZERO) THEN
           KHANI=-CHANI(K)
           HANItemp=HANI(J,I,KHANI)
        ELSE
           HANItemp=CHANI(K)
        END IF

      END SUBROUTINE
     
      SUBROUTINE DconHFB(hh,Dv,Dh,IC,IR,IL,IJ)
C FILLS IN Conductance partial dirivative, Dc(IJ,:), FOR CELL IJ WHEN IT CONTAINS A BARRIER WITH LAYER ROUTE
      USE GLOBAL, ONLY: HNEW,CC,CR,CV,NCOL,NROW,NLAY
      USE GWFHFBMODULE, ONLY: HFB,HFBRT,ICEL_HFB
      USE GWFNWTMODULE, ONLY: Dc
      USE CONSTANTS,    ONLY: Z, DZ
      IMPLICIT NONE
      DOUBLE PRECISION:: hh, Dv, Dh
      INTEGER:: IC,IR,IL,IJ
      LOGICAL:: DcHFBInit
      INTEGER:: II, JJ, KK
      INTEGER, DIMENSION(4)::BR
      DOUBLE PRECISION, DIMENSION(4):: CON
C BR IS REFERANCE TO WHERE BARRIER IS LOCATED FROM CELL IJ
C BR CONTAINS ROWS OF HFB THAT CORRESPOND TO BARRIERS IN CELL IJ
C BR(1) > Z IF BARRIER IS TO RIGHT  OF CELL IN PLAN VIEW (J-1)
C BR(2) > Z IF BARRIER IS TO TOP    OF CELL IN PLAN VIEW (I-1)
C BR(3) > Z IF BARRIER IS TO LEFT   OF CELL IN PLAN VIEW (J+1)
C BR(4) > Z IF BARRIER IS TO BOTTOM OF CELL IN PLAN VIEW (I+1)
C CON IS THE CONDUCTANCE BETWEEN BARRIER CELLS
      CON = DZ
      BR  = Z
      DO II=1,HFBRT      
        IF(IJ.EQ.ICEL_HFB(II,1))THEN
          JJ=ICEL_HFB(II,3)  !LOCATION OF BARRIER: 1,2,3, or 4
          BR( JJ)=ICEL_HFB(II,2)
          CON(JJ)=HFB(BR(JJ))%RCON
        END IF
      END DO      
      
      IF ( IL.GT.1 ) THEN
        IF ( hh.GT.HNEW(ic,ir,il-1)) Dc(1,ij) = Cv(ic,ir,il-1)*Dv
      END IF
      !
      !BARRIER TO THE TOP
      IF ( IR.GT.1 ) THEN
        IF(BR(2) > Z)THEN
          II=HFB(BR(2))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(2))%J1
          KK=HFB(BR(2))%K1
          IF ( hh.GT.HNEW(JJ,II,KK))   Dc(2,ij) = CON(2)*Dh  !Dc_HFB(BR(2),2)= CON(2)*Dh
        ELSE
          IF ( hh.GT.HNEW(ic,ir-1,il)) Dc(2,ij) = Cc(ic,ir-1,il)*Dh
        END IF
      END IF
      !
      !BARRIER TO THE LEFT
      IF ( IC.GT.1 ) THEN
        IF(BR(1) > Z)THEN
          II=HFB(BR(1))%I1  !REFER TO ROUTED CELL ON LEFT (IC-1)
          JJ=HFB(BR(1))%J1
          KK=HFB(BR(1))%K1
          IF ( hh.GT.HNEW(JJ,II,KK))   Dc(3,ij) = CON(1)*Dh !Dc_HFB(BR(1),3)= CON(1)*Dh
        ELSE
          IF ( hh.GT.HNEW(ic-1,ir,il)) Dc(3,ij) = Cr(ic-1,ir,il)*Dh
        END IF
      END IF
      !
      !BARRIER TO THE RIGHT
      IF ( IC.LT.NCOL ) THEN
        IF(BR(3) > Z)THEN
          II=HFB(BR(3))%I2  !REFER TO ROUTED CELL ON RIGHT (IC+1)
          JJ=HFB(BR(3))%J2
          KK=HFB(BR(3))%K2
          IF ( hh.GT.HNEW(JJ,II,KK))   Dc(4,ij) = CON(3)*Dh !Dc_HFB(BR(3),4)= CON(3)*Dh
        ELSE
          IF ( hh.GT.HNEW(ic+1,ir,il)) Dc(4,ij) = Cr(ic,ir,il)*Dh
        END IF
      END IF
      !
      IF ( IR.LT.NROW ) THEN
        IF(BR(4) > Z)THEN
          II=HFB(BR(4))%I2  !REFER TO ROUTED CELL ON BOTTOM (IR+1)
          JJ=HFB(BR(4))%J2
          KK=HFB(BR(4))%K2
          IF ( hh.GT.HNEW(JJ,II,KK))   Dc(5,ij) = CON(4)*Dh !Dc_HFB(BR(4),5)= CON(4)*Dh
        ELSE
          IF ( hh.GT.HNEW(ic,ir+1,il)) Dc(5,ij) = Cc(ic,ir,il)*Dh
        END IF
      END IF
      !
      IF ( IL.LT.NLAY ) THEN
        IF ( hh.GT.HNEW(ic,ir,il+1)) Dc(6,ij) = Cv(ic,ir,il)*Dv
      END IF

      END SUBROUTINE
      
      SUBROUTINE TEMPFILLUNHFB(Ic, Ir, Il)
C COPY OF SUBROUTINE TEMPFILLUN BUT TAKES INTO ACCOUNT A BARRIER THAT ROUTES FLOW
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Cv, Hnew, Cc, Cr, Ibound, Hcof,
     +    Rhs, Botm, Lbotm,Iout
      USE GWFNWTMODULE, ONLY:Cvm1,Hvp1,Hvm1,Crm1,Hrm1,Hrp1,Ccm1,Hcm1,
     +                       Hcp1,Ccc,Crr,Cvv,H,Closezero,Icell,Hcoff,
     +                       Rhss
      USE GWFUPWMODULE, ONLY:Sn
      USE GWFHFBMODULE, ONLY: HFB,HFBRT,ICEL_HFB
      USE CONSTANTS,    ONLY: Z, DZ
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER:: itemp
      DOUBLE PRECISION:: THICK
!     -----------------------------------------------------------------
      INTEGER:: IC,IR,IL,IJ
      INTEGER:: II, JJ, KK
      INTEGER, DIMENSION(4)::BR
      DOUBLE PRECISION, DIMENSION(4):: CON
C BR IS REFERANCE TO WHERE BARRIER IS LOCATED FROM CELL IJ
C BR CONTAINS ROWS OF HFB THAT CORRESPOND TO BARRIERS IN CELL IJ
C BR(1) > Z IF BARRIER IS TO RIGHT  OF CELL IN PLAN VIEW (J-1)
C BR(2) > Z IF BARRIER IS TO TOP    OF CELL IN PLAN VIEW (I-1)
C BR(3) > Z IF BARRIER IS TO LEFT   OF CELL IN PLAN VIEW (J+1)
C BR(4) > Z IF BARRIER IS TO BOTTOM OF CELL IN PLAN VIEW (I+1)
C CON IS THE CONDUCTANCE BETWEEN BARRIER CELLS
      CON = DZ
      BR  = Z
      
      IJ=Icell(IC,IR,IL)  !GET NWT ROW      
      DO II=1,HFBRT      
        IF(IJ.EQ.ICEL_HFB(II,1))THEN
          JJ=ICEL_HFB(II,3)  !LOCATION OF BARRIER: 1:LEFT,2:TOP,3:RIGHT, 4:BOTTOM
          BR( JJ)=ICEL_HFB(II,2)
          CON(JJ)=HFB(BR(JJ))%RCON
        END IF
      END DO

      Cvm1 = DZ
      Hvp1 = DZ
      Hvm1 = DZ
      Crm1 = DZ
      Hrm1 = DZ
      Hrp1 = DZ
      Ccm1 = DZ
      Hcm1 = DZ
      Hcp1 = DZ
      Ccc  = DZ
      Crr  = DZ
      Cvv  = DZ
      H    = Hnew(Ic, Ir, Il)
C
C#############################################################################
C      
      IF ( Ir.LT.Nrow ) THEN
        IF(BR(4) > Z)THEN
          II=HFB(BR(4))%I2  !REFER TO ROUTED CELL ON BOTTOM (IR+1)
          JJ=HFB(BR(4))%J2
          KK=HFB(BR(4))%K2
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN 
            Hrp1 = Hnew(JJ, II, KK) !HFB CELL TO BOTTOM
            IF ( Hrp1-H.GT.CLOSEZERO)THEN
              THICK = BOTM(JJ,II,LBOTM(KK)-1) 
     1                - BOTM(JJ,II,LBOTM(KK))
              ij  = Icell(JJ, II, KK)
              Ccc = CON(4)*THICK*Sn(ij)
            ELSE
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij  = Icell(IC,IR,IL)
              Ccc = CON(4)*THICK*Sn(ij)
            END IF
          END IF
          
        ELSE
          IF ( IBOUND(IC,IR+1,IL) /= Z ) THEN
            Hrp1 = Hnew(Ic, Ir+1, Il)
            IF ( Hrp1-H.GT.CLOSEZERO)THEN
              THICK = BOTM(IC,IR+1,LBOTM(IL)-1) 
     1                - BOTM(IC,IR+1,LBOTM(IL))
              ij  = Icell(IC,IR+1,IL)
              Ccc = Cc(Ic, Ir, Il)*THICK*Sn(ij)
            ELSE
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij  = Icell(IC,IR,IL)
              Ccc = Cc(Ic, Ir, Il)*THICK*Sn(ij)
            END IF
          END IF
        END IF
      END IF
C      
C#############################################################################
C      
      IF ( Ic.LT.Ncol ) THEN
        IF(BR(3) > Z)THEN
          II=HFB(BR(3))%I2  !REFER TO ROUTED CELL ON RIGHT (IC+1)
          JJ=HFB(BR(3))%J2
          KK=HFB(BR(3))%K2
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN 
            Hcp1 = Hnew(JJ, II, KK)
            IF ( Hcp1-H.GT.CLOSEZERO )THEN
              THICK = BOTM(JJ,II,LBOTM(KK)-1) 
     1                - BOTM(JJ,II,LBOTM(KK))
              ij = Icell(JJ, II, KK)
              Crr = CON(3)*THICK*Sn(ij)
            ELSE 
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij = Icell(IC,IR,IL)
              Crr = CON(3)*THICK*Sn(ij)
            END IF
          END IF
C
        ELSE
C
          IF ( IBOUND(IC+1,IR,IL) /= Z ) THEN
            Hcp1 = Hnew(Ic+1, Ir, Il)
            IF ( Hcp1-H.GT.CLOSEZERO )THEN
              THICK = BOTM(IC+1,IR,LBOTM(IL)-1) 
     1                - BOTM(IC+1,IR,LBOTM(IL))
              ij = Icell(IC+1,IR,IL)
              Crr = Cr(Ic, Ir, Il)*THICK*Sn(ij)
            ELSE 
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij = Icell(IC,IR,IL)
              Crr = Cr(Ic, Ir, Il)*THICK*Sn(ij)
            END IF
          END IF
        END IF
      END IF
C      
C#############################################################################
C
 ! Need to correct the following calculations for case when CV is head dependent.
      IF ( Il.LT.Nlay ) THEN
        IF ( IBOUND(IC,IR,IL+1) /= Z ) THEN
          Hvp1 = Hnew(Ic, Ir, Il+1)
 ! Set vertical correction for perched conditions
!          IF ( Hvp1.LT.BOTM(IC,IR-1,LBOTM(IL) ) 
!    +         Hvp1 = BOTM(IC,IR-1,LBOTM(IL)
          Cvv = Cv(Ic, Ir, Il)
        END IF
      ENDIF
      IF ( Il.GT.1 ) THEN
        IF ( IBOUND(IC,IR,IL-1) /= Z ) THEN
          Hvm1 = Hnew(Ic, Ir, Il-1)
          Cvm1 = Cv(Ic, Ir, Il-1)
        END IF
      ENDIF
C      
C#############################################################################
C
      IF ( Ir.GT.1 ) THEN
        IF(BR(2) > Z)THEN
          II=HFB(BR(2))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(2))%J1
          KK=HFB(BR(2))%K1
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN 
            Hrm1 = Hnew(JJ, II, KK)
            IF ( Hrm1-H.GT.CLOSEZERO)THEN
              THICK = BOTM(JJ, II,LBOTM(KK)-1) 
     1                - BOTM(JJ, II,LBOTM(KK))
              ij = Icell(JJ, II, KK)
              Ccm1 = CON(2)*THICK*Sn(ij)
            ELSE
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij = Icell(IC,IR,IL)
              Ccm1 = CON(2)*THICK*Sn(ij)
            END IF
          END IF          
C
        ELSE
C
          IF ( IBOUND(IC,IR-1,IL) /= Z ) THEN
            Hrm1 = Hnew(Ic, Ir-1, Il)
            IF ( Hrm1-H.GT.CLOSEZERO)THEN
              THICK = BOTM(IC,IR-1,LBOTM(IL)-1) 
     1                - BOTM(IC,IR-1,LBOTM(IL))
              ij = Icell(IC,IR-1,IL)
              Ccm1 = Cc(Ic, Ir-1, Il)*THICK*Sn(ij)
            ELSE
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij = Icell(IC,IR,IL)
              Ccm1 = Cc(Ic, Ir-1, Il)*THICK*Sn(ij)
            END IF
          END IF
        END IF
      END IF
C      
C#############################################################################
C     
      IF ( Ic.GT.1 ) THEN
        IF(BR(1) > Z)THEN
          II=HFB(BR(1))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(1))%J1
          KK=HFB(BR(1))%K1
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN
            Hcm1 = Hnew(JJ, II, KK)
            IF ( Hcm1-H.GT.CLOSEZERO )THEN
              THICK = BOTM(JJ,II,LBOTM(KK)-1) 
     1                - BOTM(JJ,II,LBOTM(KK))
              ij = Icell(JJ, II, KK)
              Crm1 = CON(1)*THICK*Sn(ij)
            ELSE 
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij = Icell(IC,IR,IL)
              Crm1 = CON(1)*THICK*Sn(ij)
            END IF
          END IF
C
        ELSE
C
          IF ( IBOUND(IC-1,IR,IL) /= Z ) THEN
            Hcm1 = Hnew(Ic-1, Ir, Il)
            IF ( Hcm1-H.GT.CLOSEZERO )THEN
              THICK = BOTM(IC-1,IR,LBOTM(IL)-1) 
     1                - BOTM(IC-1,IR,LBOTM(IL))
              ij = Icell(IC-1,IR,IL)
              Crm1 = Cr(Ic-1, Ir, Il)*THICK*Sn(ij)
            ELSE 
              THICK = BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
              ij = Icell(IC,IR,IL)
              Crm1 = Cr(Ic-1, Ir, Il)*THICK*Sn(ij)
            END IF
          END IF
        END IF
      END IF
C      
C#############################################################################
C     
      Hcoff = Hcof(Ic, Ir, Il)
      Rhss = Rhs(Ic, Ir, Il)
      END SUBROUTINE TEMPFILLUNHFB
!
!     -----------------------------------------------------------------
!
!     SUBROUTINE TEMPFILLCON. SET SCALERS FOR CONFINED FLOW
      SUBROUTINE TEMPFILLCONHFB(Ic, Ir, Il)
C COPY OF SUBROUTINE TEMPFILLCON BUT TAKES INTO ACCOUNT A BARRIER THAT ROUTES FLOW
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Cv, Hnew, Cc, Cr, Ibound, Hcof,
     +    Rhs, Botm, Lbotm,Iout
      USE GWFNWTMODULE, ONLY:Cvm1,Hvp1,Hvm1,Crm1,Hrm1,Hrp1,Ccm1,Hcm1,
     +                       Hcp1,Ccc,Crr,Cvv,H,Closezero,Icell,Hcoff,
     +                       Rhss
      USE GWFHFBMODULE, ONLY: HFB,HFBRT,ICEL_HFB
      USE CONSTANTS,   ONLY: Z, DZ
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Ic, Ir, Il, ij
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER:: itemp
      DOUBLE PRECISION:: THICK
!     -----------------------------------------------------------------
      INTEGER:: II, JJ, KK
      INTEGER, DIMENSION(4)::BR
      DOUBLE PRECISION, DIMENSION(4):: CON
C BR IS REFERANCE TO WHERE BARRIER IS LOCATED FROM CELL IJ
C BR CONTAINS ROWS OF HFB THAT CORRESPOND TO BARRIERS IN CELL IJ
C BR(1) > Z IF BARRIER IS TO RIGHT  OF CELL IN PLAN VIEW (J-1)
C BR(2) > Z IF BARRIER IS TO TOP    OF CELL IN PLAN VIEW (I-1)
C BR(3) > Z IF BARRIER IS TO LEFT   OF CELL IN PLAN VIEW (J+1)
C BR(4) > Z IF BARRIER IS TO BOTTOM OF CELL IN PLAN VIEW (I+1)
C CON IS THE CONDUCTANCE BETWEEN BARRIER CELLS
      CON = DZ
      BR  = Z
      
      IJ=Icell(IC,IR,IL)  !GET NWT ROW      
      DO II=1,HFBRT      
        IF(IJ.EQ.ICEL_HFB(II,1))THEN
          JJ=ICEL_HFB(II,3)  !LOCATION OF BARRIER: 1:LEFT,2:TOP,3:RIGHT, 4:BOTTOM
          BR( JJ)=ICEL_HFB(II,2)
          CON(JJ)=HFB(BR(JJ))%RCON
        END IF
      END DO
C
      Cvm1 = DZ
      Hvp1 = DZ
      Hvm1 = DZ
      Crm1 = DZ
      Hrm1 = DZ
      Hrp1 = DZ
      Ccm1 = DZ
      Hcm1 = DZ
      Hcp1 = DZ
      Ccc  = DZ
      Crr  = DZ
      Cvv  = DZ
      H    = Hnew(Ic, Ir, Il)
C
C#############################################################################
C
      IF ( Ir.LT.Nrow ) THEN
        IF(BR(4) > Z)THEN
          II=HFB(BR(4))%I2  !REFER TO ROUTED CELL ON BOTTOM (IR+1)
          JJ=HFB(BR(4))%J2
          KK=HFB(BR(4))%K2
          IF ( IBOUND(JJ,II,KK) /= Z ) THEN
             Hrp1 = Hnew(JJ,II,KK)
             Ccc = CON(4)
          END IF
        ELSE
          IF ( IBOUND(IC,IR+1,IL) /= Z ) THEN
             Hrp1 = Hnew(Ic, Ir+1, Il)
             Ccc = Cc(Ic, Ir, Il)
          END IF
        END IF
      END IF
C
C#############################################################################
C
      IF ( Ic.LT.Ncol ) THEN
        IF(BR(3) > Z)THEN
          II=HFB(BR(3))%I2  !REFER TO ROUTED CELL ON RIGHT (IC+1)
          JJ=HFB(BR(3))%J2
          KK=HFB(BR(3))%K2
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN 
            Hcp1 = Hnew(JJ, II, KK)
            Crr = CON(3)
          END IF
        ELSE
          IF ( IBOUND(IC+1,IR,IL) /= Z ) THEN
            Hcp1 = Hnew(Ic+1, Ir, Il)
            Crr = Cr(Ic, Ir, Il)
          END IF
        END IF
      END IF
C
C#############################################################################
C
! Need to correct the following calculations for case when CV is head dependent.
      IF ( Il.LT.Nlay ) THEN
        IF ( IBOUND(IC,IR,IL+1) /= Z ) THEN
          Hvp1 = Hnew(Ic, Ir, Il+1)
          Cvv = Cv(Ic, Ir, Il)
        END IF
      ENDIF
      IF ( Il.GT.1 ) THEN
        IF ( IBOUND(IC,IR,IL-1) /= Z ) THEN
          Hvm1 = Hnew(Ic, Ir, Il-1)
          Cvm1 = Cv(Ic, Ir, Il-1)
        END IF
      ENDIF
C
C#############################################################################
C
      IF ( Ir.GT.1 ) THEN
        IF(BR(2) > Z)THEN
          II=HFB(BR(2))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(2))%J1
          KK=HFB(BR(2))%K1
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN 
            Hrm1 = Hnew(JJ, II, KK)
            Ccm1 = CON(2)
          END IF
        ELSE
          IF ( IBOUND(IC,IR-1,IL) /= Z ) THEN
            Hrm1 = Hnew(Ic, Ir-1, Il)
            Ccm1 = Cc(Ic, Ir-1, Il)
          END IF
        END IF
      ENDIF
C
C#############################################################################
C
      IF ( Ic.GT.1 ) THEN
        IF(BR(1) > Z)THEN
          II=HFB(BR(1))%I1  !REFER TO ROUTED CELL ON TOP (IR-1)
          JJ=HFB(BR(1))%J1
          KK=HFB(BR(1))%K1
          IF ( IBOUND(JJ, II, KK) /= Z ) THEN
            Hcm1 = Hnew(JJ, II, KK)
            Crm1 = CON(1)
          END IF
        ELSE
          IF ( IBOUND(IC-1,IR,IL) /= Z ) THEN
            Hcm1 = Hnew(Ic-1, Ir, Il)
            Crm1 = Cr(Ic-1, Ir, Il)
          END IF
        END IF
      END IF
C
C#############################################################################
C
      Hcoff = Hcof(Ic, Ir, Il)
      Rhss = Rhs(Ic, Ir, Il)
      END SUBROUTINE TEMPFILLCONHFB
C
      SUBROUTINE GWF2HFB7RP(INHFB,KPER,IGRID)
C READ AND PREPARE ROUTINE FOR HFB PACKAGE
C IF FIRST STRESS PERIOD MODIFY CODUCTANCES OF IF UPW IS TURNED ON AND OF ALL CONFINED LAYERS.
C STRESS PERIODS >1 WILL READ IN NEW BARRIER INFORMATION AND RESET 
C CONDUCTANCES BACK TO ORIGINAL VALUES AND THEN APPLY NEW BARRIERS
C IF THERE IS NO MORE INPUT IN THE HFB FILE THAN NOTHING IS DONE
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,LAYHDT,CR,CC,CV,BOTM,LBOTM,
     1                      DELR,DELC,IOUT,IUNIT
      USE GWFHFBMODULE,ONLY:MXHFB,NHFB,IPRHFB,NHFBNP,NPHFB,IHFBPB,
     1                      HFB,HFBRT,ICEL_HFB,MXHFBNP,NACTHFB,IPRHFB,
     2                      NO_RP
      USE GWFNWTMODULE, ONLY: Numnonzero
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE CONSTANTS ,        ONLY: NEG, Z, ONE, DZ, BLNK, TRUE
      USE STRINGS,           ONLY: GET_INTEGER
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      INTEGER::INHFB,KPER,IGRID
      CHARACTER(128):: LINE
      INTEGER:: ITEMP, HFBRT_OLD
      LOGICAL:: EOF
      !
      IF(NO_RP) RETURN
      !
      CALL SGWF2HFB7PNT(IGRID)
      !
      !IF KPER>1 THEN READ IN NEW BARRIER INFORMATION
      !IF FIRST STRESS PERIOD THEN BY PASS READ AND PREPARE AND ONLY BUILD FOR CONSTANT T LAYERS AND IF UPW IS ACTIVE
      KPER_CHECK: IF (KPER.GT.1) THEN 
      !
      CALL READ_TO_DATA(LINE,INHFB,IOUT,EOF=EOF)
      !READ(INHFB,'(A)',IOSTAT=IOS) LINE
      IF (EOF) THEN       !DO NOTHING IF FAILED TO READ IN LINE
               NO_RP = TRUE
               RETURN
      END IF
      
      LLOC = ONE
      !ITEMP BECOMES NACTHFB IF > 0
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,Z,Z,ITEMP,HAS_ERROR=EOF)
      IF (EOF)THEN
         WRITE(IOUT,'(2A, /, 3A)') 
     +       'HFB FAILED TO READ STRESS PERIOD FLAG (ITMP) FOR PERIOD ',
     +       NUM2STR(KPER),
     +       'THE HFB INFORMATION FROM THE PREVIOUS STRESS PERIOD (',
     +       NUM2STR(KPER-1),
     +       ') WILL BE USED FOR THE REMAINDER OF THE SIMULATION.'
         NO_RP = TRUE
         RETURN
      END IF
      !
      IF (ITEMP == NEG) THEN
         WRITE(IOUT,*)'RE-USING HFB SPECIFICATIONS FROM PREVIOUS', 
     +              ' STRESS PERIOD '        
       RETURN       !REUSE PREVIOUS STRESS PERIODS INPUT
      END IF
      !
      IF (MXHFBNP > Z) THEN !IF MAX # OF NON PARAMETER BARRIERS IS 0 SKIP READING NHFBNP
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INHFB,NHFBNP,MSG=
     +                                'HFB FAILED TO READ NHFBNP')
        !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHFBNP,DUM,IOUT,INHFB)
      ELSE
        NHFBNP = Z
      END IF
      !
      NACTHFB = ITEMP !SET ITEMP TO THE NUMBER OF PARAMETERS THAT DEFINE BARRIERS
      !
      IF (NHFBNP.GT.MXHFBNP) THEN
        CALL USTOP('ERROR: HFB VARIABLE NHFBNP MUST BE LESS THAN '//
     +               'OR EQUAL TO MXHFBNP.')
      END IF
      !
        WRITE(IOUT,530) KPER,NHFBNP
  530 FORMAT(1X,'STRESS PERIOD ',I6,' HAS ',I6,
     +        ' HORIZONTAL FLOW BARRIERS NOT DEFINED BY PARAMETERS')
      ! 
! RESET CONDUCTANCES TO ORIGINAL VALUES
      DO II=1, NHFB
        K1=HFB(II)%K1
        I1=HFB(II)%I1
        J1=HFB(II)%J1
        !
        I2=HFB(II)%I2
        J2=HFB(II)%J2
        K2=HFB(II)%K2
        !        
        IF     (I1.EQ.I2.AND.J1.NE.J2) THEN
          CR(J1,I1,K1)=HFB(II)%CON1
          IF(K2 > Z) CR(J1,I1,K2) = HFB(II)%CON2 
        ELSEIF (J1.EQ.J2.AND.I1.NE.I2) THEN
          CC(J1,I1,K1)=HFB(II)%CON1
          IF(K2 > Z) CC(J1,I1,K2) = HFB(II)%CON2
        ELSEIF (J1.EQ.J2.AND.I1.EQ.I2) THEN
          CV(J1,I1,K1)=HFB(II)%CON1
        END IF
      END DO
      !
      HFB(1:NHFB)%K1     = Z                                            !ZERO OUT PREVIOUS STORED INFORMATION AS IT IS ABOUT TO BE OVERWRITTEN BY NEW INFORMATION
      HFB(1:NHFB)%I1     = Z                                            !NOTE THAT NHFB GETS UPDATED NEXT TO THE NEW DATA SIZE
      HFB(1:NHFB)%J1     = Z
      HFB(1:NHFB)%I2     = Z
      HFB(1:NHFB)%J2     = Z
      HFB(1:NHFB)%K2     = Z
      HFB(1:NHFB)%HYDCHR = DZ
      HFB(1:NHFB)%RCON   = DZ
      HFB(1:NHFB)%CON1   = DZ
      HFB(1:NHFB)%CON2   = DZ      
      !
      !SET STARTING POINT FOR PARMATER BARRIERS STORAGE LOCATION
      NHFB = NHFBNP
      !
      !READ IN PARAMETER FAULTS AND ADD TO NEW TOTAL COUNT OF FAULTS
      WRITE(IOUT,531) KPER,NACTHFB
      !
  531 FORMAT(/,1X,'STRESS PERIOD ',I6,' HAS ',I10,
     +       ' ACTIVE HORIZONTAL FLOW BARRIER PARAMETERS')
      !
      MXACTFB= IHFBPB-1
      IF (NACTHFB > Z) THEN
        CALL PRESET('HFB ')
        !
        DO II=1,NACTHFB
         CALL SGWF2HFB7SUB(INHFB,'HFB ',IOUT,'HFB ',
     1                      MXHFB,MXACTFB,NHFB,
     2    'BARRIER   LAYER   IROW1  ICOL1  IROW2  ICOL2   HYDCHR'//
     3    '      LAYER2',IPRHFB)
        END DO
      ENDIF

      IF(NHFBNP > Z) THEN
        CALL SGWF2HFB7RL(NHFBNP,1,MXHFB,INHFB,IOUT,
     1         'BARRIER   LAYER   IROW1  ICOL1  IROW2  ICOL2   HYDCHR'//
     2         '      LAYER2',
     3             NCOL,NROW,NLAY,IPRHFB)
        CALL SGWF2HFB7CK(1,NHFBNP)
      ENDIF
      
!COUNT NUMBER OF ROUTED CELL
      HFBRT_OLD = HFBRT
      HFBRT     = Z
      DO II=1, NHFB
        I1 = HFB(II)%I1
        J1 = HFB(II)%J1
        I2 = HFB(II)%I2
        J2 = HFB(II)%J2
        K2 = HFB(II)%K2          
        IF (K2 > Z .AND. (I1.NE.I2.OR.J1.NE.J2) ) HFBRT=HFBRT+2        !ROUTED CELL ONLY GOES IN HORIZONTAL DIRECTION AND NOT VERTICAL
      END DO
      
      HFBROUTE: IF (HFBRT > Z) THEN  !THERE ARE LAYER ROUTES IN CURRENT STRESS PERIOD
        CALL SGWF2NWT1PNT(IGRID)
        IF (HFBRT_OLD > Z) THEN
          IF (HFBRT_OLD < HFBRT)THEN
            DEALLOCATE(ICEL_HFB)
            ALLOCATE(ICEL_HFB(HFBRT,3))
          END IF
          ICEL_HFB = Z
        ELSE
          ALLOCATE(ICEL_HFB(HFBRT,3))
        END IF
        CALL ORDERCELLHFB() !REBUILD LAYER ROUTE INDEX
        !
        CALL COUNTACTIVE(II)!CHECK IF ACTIVE COUNT HAS CHANGED
        IF (II.NE.Numnonzero)THEN
        CALL USTOP('ERROR: ACTIVE CELL COUNT HAS CHANGED WITH '//
     +             'ROUTED BARRIERS. THIS WILL HAVE UNKNOWN EFFECTS'//
     +             ' WITH NWT PACKAGE. THIS MOST LIKELY OCCURED '//
     +        'BECAUSE OF A LAYER ROUTE ADDED/REMOVED A NEW ACTIVECELL')
        END IF
        !
        CALL FILLINDEX(II) !REBUILD NWT INDEX
        CALL SGWF2NWT1PSV(IGRID)!UPDATE NWT POINTERS
      END IF HFBROUTE
      !
      CALL SGWF2HFB7PSV(IGRID)
      !
      END IF KPER_CHECK         !ENDS IF STATEMENT FOR KPER>1

      !MODIFY HORIZONTAL BRANCH CONDUCTANCES FOR CONSTANT T LAYERS AND S.
   10 CALL SGWF2HFB7MC()

      !MODIFY CONDUCTANCE FOR HFB WHEN USING UPW.
      IF ( IUNIT(62) /= Z ) CALL GWF2HFB7UPW(IGRID)
      
      END SUBROUTINE
      


      
