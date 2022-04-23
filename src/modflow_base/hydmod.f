C  The hydrograph labels are established by the SE subroutines the
C  first time they are called.  SE is called the first time by the
C  AR subroutine for the BAS, IBS, and SUB Packages.  SE is called
C  the first time by the RP subroutine for the STR and SFR Packages.
C  Subsequent calls to SE subroutines in the Main must be made in
C  the same order as the initial calls.  A HYDMOD package that
C  has an RP subroutine will always have the initial SE call after
C  the initial SE calls for all of the packages that have the
C  initial SE call in the AR subroutine.
      MODULE HYDBASMODULE
        USE OBS_GROUP_INTERPOLATOR, ONLY: OBS_POINTS, OBS_POINT
        PRIVATE:: OBS_POINTS, OBS_POINT
        !
        INTEGER,SAVE,       POINTER                ::NHYDTOT
        REAL,   SAVE,       POINTER,DIMENSION(:,:),CONTIGUOUS::HYDVAL
        CHARACTER(LEN=20),SAVE, POINTER,DIMENSION(:),CONTIGUOUS::HYDLBL
        INTEGER,SAVE,       POINTER             ::IHYDMUN,NHYDBAS
        REAL,   SAVE,       POINTER             ::HYDNOH
        LOGICAL,SAVE,       POINTER,DIMENSION(:)  ,CONTIGUOUS::IBHYDBAS
        LOGICAL,SAVE,       POINTER,DIMENSION(:),CONTIGUOUS::INTRPHYDBAS
        !INTEGER,SAVE,       POINTER,DIMENSION(:,:),CONTIGUOUS::JIKHYDBAS
        !REAL,   SAVE,       POINTER,DIMENSION(:,:),CONTIGUOUS::HYDBASWT
        REAL,   SAVE,       POINTER,DIMENSION(:),CONTIGUOUS::HYDBASSTRT
        CHARACTER(LEN=4),SAVE,POINTER,DIMENSION(:),CONTIGUOUS::HYDBASARR
        !
        LOGICAL,SAVE,       POINTER:: HAS_NWT_HDRY
        TYPE(OBS_POINT), SAVE,POINTER,DIMENSION(:),CONTIGUOUS:: HD_OP
        TYPE HYDBASTYPE
          INTEGER, POINTER                         ::NHYDTOT
          REAL,             POINTER,DIMENSION(:,:),CONTIGUOUS ::HYDVAL
          CHARACTER(LEN=20),POINTER,DIMENSION(:),CONTIGUOUS   ::HYDLBL
          INTEGER,       POINTER                ::IHYDMUN,NHYDBAS
          REAL,          POINTER                ::HYDNOH
          LOGICAL,       POINTER,DIMENSION(:),CONTIGUOUS   ::IBHYDBAS
          LOGICAL,       POINTER,DIMENSION(:),CONTIGUOUS   ::INTRPHYDBAS
          !INTEGER,       POINTER,DIMENSION(:,:),CONTIGUOUS ::JIKHYDBAS
          !REAL,          POINTER,DIMENSION(:,:),CONTIGUOUS ::HYDBASWT
          REAL,          POINTER,DIMENSION(:),CONTIGUOUS   ::HYDBASSTRT
          CHARACTER(LEN=4),POINTER,DIMENSION(:),CONTIGUOUS ::HYDBASARR
          !
          LOGICAL,        POINTER:: HAS_NWT_HDRY
          TYPE(OBS_POINT), POINTER,DIMENSION(:),CONTIGUOUS:: HD_OP
        END TYPE
        TYPE(HYDBASTYPE),   SAVE  :: HYDBASDAT(10)
      END MODULE

      MODULE HYDIBSMODULE
        INTEGER, SAVE,         POINTER                ::NHYDIBS
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDIBS
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDIBS
        INTEGER, SAVE,         POINTER,DIMENSION(:,:) ::JIKHYDIBS
        REAL,    SAVE,         POINTER,DIMENSION(:,:) ::HYDIBSWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDIBSARR
        TYPE HYDIBSTYPE
          INTEGER,          POINTER                ::NHYDIBS
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDIBS
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDIBS
          INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDIBS
          REAL,             POINTER,DIMENSION(:,:) ::HYDIBSWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDIBSARR
        END TYPE 
        TYPE(HYDIBSTYPE),  SAVE  ::HYDIBSDAT(10)
      END MODULE

      MODULE HYDSUBMODULE
        USE OBS_GROUP_INTERPOLATOR, ONLY: OBS_POINTS, OBS_POINT
        PRIVATE:: OBS_POINTS, OBS_POINT
        !
        INTEGER, SAVE,         POINTER                ::NHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDSUB
        !LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::UPPERLAYER
        !INTEGER, SAVE,         POINTER,DIMENSION(:,:) ::JIKHYDSUB
        !REAL,    SAVE,         POINTER,DIMENSION(:,:) ::HYDSUBWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSUBARR
        !INTEGER, SAVE,         POINTER,DIMENSION(:)   ::IBFACT
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::IBTOP
        !
        TYPE(OBS_POINTS),SAVE,POINTER,DIMENSION(:),CONTIGUOUS:: SB_OB  !Uses SB_OB%OP(IB) to hold each interbed in the same layer 
        TYPE HYDSUBTYPE
          INTEGER,          POINTER                ::NHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDSUB
          !LOGICAL,          POINTER,DIMENSION(:)   ::UPPERLAYER
          !INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDSUB
          !REAL,             POINTER,DIMENSION(:,:) ::HYDSUBWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSUBARR
          !
          TYPE(OBS_POINTS),POINTER,DIMENSION(:),CONTIGUOUS:: SB_OB
        END TYPE 
        TYPE(HYDSUBTYPE),  SAVE  ::HYDSUBDAT(10)
      END MODULE

      MODULE HYDSWTMODULE
        USE OBS_GROUP_INTERPOLATOR, ONLY: OBS_POINTS
        PRIVATE:: OBS_POINTS
        !
        INTEGER, SAVE,         POINTER                ::NHYDSWT
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDSWT
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDSWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSWTARR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::IBTOP
        !
        TYPE(OBS_POINTS),SAVE,POINTER,DIMENSION(:),CONTIGUOUS:: SB_OB  !Uses SB_OB%OP(IB) to hold each interbed in the same layer 
        TYPE HYDSWTTYPE
          INTEGER,          POINTER                ::NHYDSWT
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDSWT
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDSWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSWTARR
          !
          TYPE(OBS_POINTS),POINTER,DIMENSION(:),CONTIGUOUS:: SB_OB
        END TYPE 
        TYPE(HYDSWTTYPE),  SAVE  ::HYDSWTDAT(10)
      END MODULE

      MODULE HYDSTRMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSTR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::ISTRHYD
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSTRARR
        TYPE HYDSTRTYPE
          INTEGER,          POINTER                ::NHYDSTR
          INTEGER,          POINTER,DIMENSION(:)   ::ISTRHYD
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSTRARR
        END TYPE
        TYPE(HYDSTRTYPE),   SAVE   ::HYDSTRDAT(10)
      END MODULE

      MODULE HYDSFRMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSFR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::ISFRHYD
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSFRARR
        TYPE HYDSFRTYPE
          INTEGER,          POINTER                ::NHYDSFR
          INTEGER,          POINTER,DIMENSION(:)   ::ISFRHYD
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSFRARR
        END TYPE
        TYPE(HYDSFRTYPE),   SAVE   ::HYDSFRDAT(10)
      END MODULE

      SUBROUTINE GWF2HYD7BAS7AR(IN,IGRID,IUNITNWT)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGRAPH DATA FOR BAS PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,                ONLY: NL
      USE STRINGS,                  ONLY: GET_INTEGER, GET_NUMBER
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      USE FILE_IO_INTERFACE,        ONLY: READ_TO_DATA
      USE NUM2STR_INTERFACE,        ONLY: NUM2STR
      USE GWFUPWMODULE,             ONLY: IPHDRY
      USE GLOBAL , ONLY: IOUT,NCOL,NROW,NLAY, STRT,IBOUND,
     +                   DELR,DELC,XYGRID
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      USE HYDBASMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IN,IGRID,IUNITNWT
      CHARACTER(256):: LINE
      CHARACTER(1) INTYP
      CHARACTER(20) HYDBASLBL
      TYPE(WARNING_TYPE):: WRN
      INTEGER:: LLOC,ISTART,ISTOP,N
      INTEGER:: NHYDM, NTOT, KLAY, NC1, NR1, NC2, NR2
      REAL:: ONE, ZERO, XL, YL, R
C     ------------------------------------------------------------------
      ALLOCATE(NHYDTOT)
      ALLOCATE(IHYDMUN,NHYDBAS,HYDNOH)
      ONE=1.0
      ZERO=0.0
      NHYDTOT=0
      CALL WRN%INIT()
      !
      ALLOCATE(HAS_NWT_HDRY, SOURCE=.FALSE.)
      !
      IF(IUNITNWT.NE.0) HAS_NWT_HDRY = IPHDRY > 0   !Only flag NWT if HYDRY is recorded for HNEW<BOTM  --> IPHDRY > 0 indicates usings HDRY
          

C
C1------IDENTIFY PROGRAM.
       WRITE(IOUT,1) IN
    1  FORMAT(1X,/1X,'HYD -- HYDROGRAPH DATA FOR BAS, SUB, SWT,',
     1  ' and SFR PACKAGES -- VERSION OWHM',/
     2  1X,'        INPUT READ FROM UNIT',I3)
C
C4------READ NUMBER OF HYDROGRAPHS AND UNIT FOR SAVING UNFORMATTED
C4------HYDROGRAPH FILE AND NUMERIC FLAG FOR DRY/INACTIVE CELLS
      CALL READ_TO_DATA(LINE,IN,IOUT,IOUT, 
     +                          HED="-- READING HYD PACKAGE INPUT --")
C  Number of hydrographs (NHYDM) specified by the user is ignored --
C    the program initially counts the number of hydrographs (NTOT).
C    Note that there may be less than NHTOT hydrographs because some
C    may be eliminated due to invalid values.
      LLOC=1
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN, NHYDM, 
     +                 MSG='HydMod failed to read 1st number: NHYDM')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN, IHYDMUN, 
     +                 MSG='HydMod failed to read 2nd number: IHYDMUN')
      CALL GET_NUMBER (LINE,LLOC,ISTART,ISTOP,IOUT,IN, HYDNOH, 
     +                 MSG='HydMod failed to read 3rd number: HYDNOH')
       WRITE(IOUT,5) IHYDMUN,HYDNOH
  5   FORMAT(1X,'HYDROGRAPH VALUES WILL BE SAVED ON UNIT:',I4,
     2     /,1X,'HYDROGRAPH VALUES AT DRY CELLS WILL BE:',ES14.5)
C
C4------COUNT NUMBER OF BAS PACKAGE AND OVERALL HYDROGRAPHS.
      NTOT=0
      NHYDBAS=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      NTOT=NTOT+1
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'BAS') NHYDBAS=NHYDBAS+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR ALL TYPES OF HYDROGRAPHS
 19   IF(NTOT.GT.0) THEN
        ALLOCATE(HYDVAL(NTOT,2))
        ALLOCATE(HYDLBL(NTOT))
         WRITE(IOUT,17)
 17     FORMAT(1X,/1X,
     1   'ARR INTYP  KLAY     XL            YL        ROW   COL       ',
     2   'HYDLBL', / 1X, 73('-'))
      ELSE
        ALLOCATE(HYDVAL(1,2))
        ALLOCATE(HYDLBL(1))
      END IF
C
C  ALLOCATE MEMORY FOR BAS HYDROGRAPH DATA
      IF(NHYDBAS.GT.0) THEN
        ALLOCATE(IBHYDBAS(NHYDBAS))
        ALLOCATE(INTRPHYDBAS(NHYDBAS))
        ALLOCATE(HYDBASSTRT(NHYDBAS))
        ALLOCATE(HYDBASARR(NHYDBAS))
        ALLOCATE(HD_OP(NHYDBAS))
      ELSE
        ALLOCATE(IBHYDBAS(1))
        ALLOCATE(INTRPHYDBAS(1))
        ALLOCATE(HYDBASSTRT(1))
        ALLOCATE(HYDBASARR(1))
        ALLOCATE(HD_OP(1))
         WRITE(IOUT,18)
  18    FORMAT(1X,'NO HYDROGRAPHS FOR BAS PACKAGE')
        GO TO 999
      END IF
      IF(NTOT.LE.0) THEN
         WRITE(IOUT,16)
  16    FORMAT(1X,'NO HYDROGRAPHS FOR ANY PACKAGE')
        GO TO 999
      END IF
C
C  READ BAS HYDROGRAPH DATA
      NHYDBAS=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 20   READ(IN,'(A)',END=99) LINE
      IF(LINE.EQ.' ') GO TO 20
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'BAS') GO TO 20
C
C Record applies to BAS Package
      NHYDBAS=NHYDBAS+1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      HYDBASARR(NHYDBAS)=LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(1:2),FMT='(A2)') HYDBASARR(NHYDBAS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(3:3),FMT='(A1)') LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDBASLBL(4:6),FMT='(I3.3)')KLAY
      HYDBASLBL(7:20)=LINE(ISTART:ISTOP)
      !seb ADDED CHECK FOR POINTS BEING WITHIN MODEL
      CALL XYGRID%XY2RC(XL,YL,NR1,NC1)
      NC2 = NC1 ! no longer used
      NR2 = NR1 
      IF(NR1 == 0)THEN
         CALL WRN%ADD(TRIM(ADJUSTL(LINE))//NL )
         NHYDBAS=NHYDBAS-1
         GO TO 20
ccrth         CALL USTOP(TRIM(LINE))
      END IF
C
C Determine if head
      IF(HYDBASARR(NHYDBAS).EQ.'HD') THEN
         IBHYDBAS(NHYDBAS)=.TRUE.
C
C Determine if drawdown
      ELSE IF(HYDBASARR(NHYDBAS).EQ.'DD') THEN
         IBHYDBAS(NHYDBAS)=.TRUE.
C
C  Not head or drawdown, so error.
      ELSE
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDBAS=NHYDBAS-1
         GO TO 20
      ENDIF
C
C  Find the grid coordinates for the cell.
      CALL HD_OP(NHYDBAS)%INIT(XYGRID,' ',XL,YL,KLAY,
     +                         INTYP.EQ.'I', 0,NR1,NC1)
      !
Cseb WRITE OUT HYDMOD POINT INFORMATION
      WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDBASARR(NHYDBAS),INTYP,KLAY,XL,YL,NR1,NC1,HYDBASLBL
C
C  Check if interpolating between nodes.
      IF(INTYP.EQ.'C') THEN
C
C  Do not interpolate
         INTRPHYDBAS(NHYDBAS)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
            WRITE(IOUT,26) TRIM(LINE)
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            NHYDBAS=NHYDBAS-1
            GO TO 20
         ENDIF
      ELSE IF(INTYP.EQ.'I') THEN
C
C  Interpolate between cells
         INTRPHYDBAS(NHYDBAS)=.TRUE.
      ELSE
C
C  Interpolation coding error.
         WRITE(IOUT,27) TRIM(LINE)
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following record:',/,A)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDBAS=NHYDBAS-1
         GO TO 20
      ENDIF
C
C  If computing drawdown, save the starting head
      IF(HYDBASARR(NHYDBAS).EQ.'DD') THEN
         !
         CALL HD_OP(NHYDBAS)%BASIS_BUILDER(NROW,NCOL,NLAY,
     +                                   IBOUND,DELC,DELR)
         !
         CALL HD_OP(NHYDBAS)%INTERP(NROW,NCOL,NLAY,STRT) 
         !
         HYDBASSTRT(NHYDBAS)=HD_OP(NHYDBAS)%SIM
      ENDIF
C
C  Save the hydrograph label and continue with the next record.
      HYDLBL(NHYDBAS)=HYDBASLBL
      GO TO 20
C
C  End of file after all BAS HYDROGRAPH data have been processed
C  Note that NHYDTOT is accumulated by each package from this point on.
C  NHYDTOT is the total number of valid hydrographs after data are
C  checked for errors.
 99   NHYDTOT=NHYDBAS
      IF(WRN%RAISED) THEN
          CALL WRN%CHECK('HYDMOD NOTICE - THE FOLLOWING LINES '//
     +     'HAD BAD X, Y COORDIANTES:', OUTPUT=IOUT)
      END IF
C
999   CALL SGWF2HYD7BAS7PSV(IGRID)
      IF(NHYDBAS.GT.0) CALL GWF2HYD7BAS7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7IBS7AR(IN,IGRID)
C     ******************************************************************
C     READ IBS PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW
      USE HYDBASMODULE
      USE HYDIBSMODULE
      USE GWFIBSMODULE,   ONLY: IBQ
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN,IGRID
      CHARACTER(256):: LINE
      CHARACTER( 20):: HYDIBSLBL
      CHARACTER(  3):: PCKG
      CHARACTER(  2):: ARR
      CHARACTER(  1):: INTYP
      INTEGER:: LLOC,ISTART,ISTOP,N
      INTEGER:: KLAY, NC1, NR1, NC2, NR2, NQ, K
      REAL:: XL, YL, X1, X2, Y1, Y2, W1, W2, W3, W4
      REAL:: ONE, ZERO, R
C     ------------------------------------------------------------------
      ALLOCATE (NHYDIBS)
      ONE=1.0
      ZERO=0.0
      NHYDIBS=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'IBS') NHYDIBS=NHYDIBS+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR IBS HYDROGRAPH DATA
 19   IF(NHYDIBS.GT.0) THEN
        ALLOCATE(IBHYDIBS(NHYDIBS))
        ALLOCATE(INTRPHYDIBS(NHYDIBS))
        ALLOCATE(JIKHYDIBS(3,NHYDIBS))
        ALLOCATE(HYDIBSWT(4,NHYDIBS))
        ALLOCATE(HYDIBSARR(NHYDIBS))
      ELSE
        ALLOCATE(IBHYDIBS(1))
        ALLOCATE(INTRPHYDIBS(1))
        ALLOCATE(JIKHYDIBS(3,1))
        ALLOCATE(HYDIBSWT(4,1))
        ALLOCATE(HYDIBSARR(1))
         WRITE(IOUT,18)
  18    FORMAT(1X,'NO HYDROGRAPHS FOR IBS PACKAGE')
        GO TO 999
      END IF
C
C  Read IBS hydrograph data.
      NHYDIBS=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'IBS') GO TO 15
      NHYDIBS=NHYDIBS+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDIBSLBL(4:6),FMT='(I3.3)')KLAY
      HYDIBSLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDIBSARR(NHYDIBS)='HC'
         IBHYDIBS(NHYDIBS)=.TRUE.
cc    TIME SERIES FROM THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDIBSARR(NHYDIBS)='CP'
         IBHYDIBS(NHYDIBS)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDIBSARR(NHYDIBS)='SB'
         IBHYDIBS(NHYDIBS)=.FALSE.
C  Change the layer number to the number of subsidence layers
         NQ=0
         DO 20 K=1,KLAY
         IF(IBQ(K).GT.0) NQ=NQ+1
 20      CONTINUE
         KLAY=NQ
      ELSE
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDIBS=NHYDIBS-1
         GO TO 15
      ENDIF
!      WRITE(IOUT,23) HYDIBSARR(NHYDIBS),INTYP,KLAY,XL,YL,HYDIBSLBL
! 23   FORMAT(1X,A,1X,A,I7,1PE14.5,E14.5,2X,A)
C
C  Get the cell location
      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
Cseb WRITE OUT HYDMOD POINT INFORMATION
      WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDIBSARR(NHYDIBS),INTYP,KLAY,XL,YL,NR1,NC1,HYDIBSLBL
C
      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
         INTRPHYDIBS(NHYDIBS)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
             WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            NHYDIBS=NHYDIBS-1
            GO TO 15
         ENDIF
cc    
         JIKHYDIBS(1,NHYDIBS)=NC1
         JIKHYDIBS(2,NHYDIBS)=NR1
         JIKHYDIBS(3,NHYDIBS)=KLAY
         HYDIBSWT(1,NHYDIBS)=ONE
         HYDIBSWT(2,NHYDIBS)=ZERO
         HYDIBSWT(3,NHYDIBS)=ZERO
         HYDIBSWT(4,NHYDIBS)=ZERO
      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
         INTRPHYDIBS(NHYDIBS)=.TRUE.
         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL-1) THEN
           WRITE(IOUT,26) LINE
            NHYDIBS=NHYDIBS-1
            GO TO 15
         ENDIF
         JIKHYDIBS(1,NHYDIBS)=NC2
         JIKHYDIBS(2,NHYDIBS)=NR2
         JIKHYDIBS(3,NHYDIBS)=KLAY
         HYDIBSWT(1,NHYDIBS)=W1
         HYDIBSWT(2,NHYDIBS)=W2
         HYDIBSWT(3,NHYDIBS)=W3
         HYDIBSWT(4,NHYDIBS)=W4
      ELSE
         WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following hydrograph record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDIBS=NHYDIBS-1
         GO TO 15
      ENDIF
C
C  Save the hydrograph label and process the next line in input file
      HYDLBL(NHYDTOT+NHYDIBS)=HYDIBSLBL
      GO TO 15
C
C  End of file after all IBS hydrograph data have been processed
 99   IF(NHYDIBS.GT.0) THEN
        WRITE(IOUT,108) NHYDIBS
108     FORMAT(/,' A total of ',I10,' points have been added ',
     & 'for the hydrographs of IBS arrays.')
      END IF
C
 999  CALL SGWF2HYD7IBS7PSV(IGRID)
      IF(NHYDIBS.GT.0) CALL GWF2HYD7IBS7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SUB7AR(IN,IGRID)
C     ******************************************************************
C     READ SUB PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,XYGRID
      USE HYDBASMODULE
      USE HYDSUBMODULE
      USE GWFSUBMODULE,   ONLY: LN,NNDB 
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      USE NUM2STR_INTERFACE,        ONLY: NUM2STR
      USE CONSTANTS,                ONLY: NL
      USE GENERIC_OPEN_INTERFACE,   ONLY: UTF8_BOM_OFFSET_REWIND
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN,IGRID
      TYPE(WARNING_TYPE):: WRN
C
      CHARACTER(256):: LINE
      CHARACTER( 20):: HYDSUBLBL
      CHARACTER(  3):: PCKG
      CHARACTER(  2):: ARR
      CHARACTER(  1):: INTYP
      INTEGER:: LLOC,ISTART,ISTOP,N,I, K
      INTEGER:: KLAY, NC1, NR1, NC2, NR2
      REAL:: XL, YL
      REAL:: ONE, ZERO, R
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSUB)
      ONE=1.0
      ZERO=0.0
      NHYDSUB=0
      CALL WRN%INIT()
      CALL UTF8_BOM_OFFSET_REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SUB') NHYDSUB=NHYDSUB+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR SUB HYDROGRAPH DATA
 19   IF(NHYDSUB.GT.0) THEN
        ALLOCATE(IBHYDSUB(NHYDSUB))
        ALLOCATE(INTRPHYDSUB(NHYDSUB))
        ALLOCATE(HYDSUBARR(NHYDSUB))
        ALLOCATE(IBTOP(NHYDSUB))
        ALLOCATE(SB_OB(NHYDSUB))
      ELSE
        ALLOCATE(IBHYDSUB(1))
        ALLOCATE(INTRPHYDSUB(1))
        ALLOCATE(HYDSUBARR(1))
        ALLOCATE(SB_OB(1))
         WRITE(IOUT,18)
  18    FORMAT(1X,'NO HYDROGRAPHS FOR SUB PACKAGE')
        GO TO 999
      END IF
C
C  Read SUB hydrograph data.
      NHYDSUB=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'SUB') GO TO 15
      NHYDSUB=NHYDSUB+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      !
      CALL XYGRID%XY2RC(XL,YL,NR1,NC1)
      NC2 = NC1 ! no longer used
      NR2 = NR1 
      IF(NR1 == 0)THEN
         CALL WRN%ADD(TRIM(ADJUSTL(LINE))//NL )
         NHYDSUB=NHYDSUB-1
         GO TO 15
      END IF
      !
      WRITE(HYDSUBLBL(4:6),FMT='(I3.3)')KLAY
      HYDSUBLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDSUBARR(NHYDSUB)='HC'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE TOTAL COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDSUBARR(NHYDSUB)='CP'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE INELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CE') THEN
         HYDSUBARR(NHYDSUB)='CE'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE ELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CV') THEN
         HYDSUBARR(NHYDSUB)='CV'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDSUBARR(NHYDSUB)='SB'
         IBHYDSUB(NHYDSUB)=.FALSE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL INELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SV') THEN
         HYDSUBARR(NHYDSUB)='SV'
         IBHYDSUB(NHYDSUB)=.FALSE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL ELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SE') THEN
         HYDSUBARR(NHYDSUB)='SE'
         IBHYDSUB(NHYDSUB)=.FALSE.
      ELSE
          WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSUB=NHYDSUB-1
         GO TO 15
      ENDIF
! WRITE OUT HYDMOD POINT INFORMATION
        WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDSUBARR(NHYDSUB),INTYP,KLAY,XL,YL,NR1,NC1,HYDSUBLBL
C
      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
         INTRPHYDSUB(NHYDSUB)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
           WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            NHYDSUB=NHYDSUB-1
            GO TO 15
         ENDIF
      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
         INTRPHYDSUB(NHYDSUB)=.TRUE.
      ELSE
          WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following hydrograph record:',/,A80)
          WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSUB=NHYDSUB-1
         GO TO 15
      ENDIF
      !
      N = NHYDSUB
      IF(HYDSUBARR(N).EQ.'SB'.or.HYDSUBARR(N).EQ.'SE'.or.
     +                           HYDSUBARR(N).EQ.'SV') THEN
        !
        K = COUNT(0 < LN .AND. LN <= KLAY)
        IF (K == 0) THEN
          WRITE(IOUT,28) LINE
 28      FORMAT(' SPECIFIED LAYERS DO NOT HAVE ANY INTERBEDS',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSUB=NHYDSUB-1
         GO TO 15
        END IF
        !
        CALL SB_OB(NHYDSUB)%INIT(K) ! HAS K INTERBEDS WITHIN LAYERS
        !
        K = 0
        DO I=1, NNDB
          IF(0 < LN(I) .AND. LN(I) <= KLAY) THEN
             K=K+1
             CALL SB_OB(NHYDSUB)%OP(K)%INIT(XYGRID,' ',XL,YL,LN(I), 
     +                                      INTYP.EQ.'I', 0,NR1,NC1, I) !ID = I
          END IF
        END DO
      ELSE
        !
        K = COUNT(LN == KLAY)
        IF (K == 0) THEN
          WRITE(IOUT,29) LINE
 29      FORMAT(' SPECIFIED LAYER DOES NOT HAVE ANY INTERBEDS',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSUB=NHYDSUB-1
         GO TO 15
        END IF
        !
        CALL SB_OB(NHYDSUB)%INIT(K) ! HAS K INTERBEDS WITHIN LAYERS
        !
        K = 0
        DO I=1, NNDB
          IF(LN(I) == KLAY) THEN
             K=K+1
             CALL SB_OB(NHYDSUB)%OP(K)%INIT(XYGRID,' ',XL,YL,KLAY, 
     +                                      INTYP.EQ.'I', 0,NR1,NC1, I) !ID = I
          END IF
        END DO
      END IF
C
C  Save the hydrograph label and process the next line in input file
      HYDLBL(NHYDTOT+NHYDSUB)=HYDSUBLBL
      GO TO 15
C
C  End of file after all SUB hydrograph data have been processed
 99   IF(NHYDSUB.GT.0) THEN
        WRITE(IOUT,108) NHYDSUB
108     FORMAT(/,' A total of ',I10,' points have been added ',
     & 'for the hydrographs of SUB arrays.')
      END IF
      !
      IF(WRN%RAISED) THEN
          CALL WRN%CHECK('HYDMOD NOTICE - THE FOLLOWING LINES '//
     +     'HAD BAD X, Y COORDIANTES:', OUTPUT=IOUT)
      END IF
C
 999  CALL SGWF2HYD7SUB7PSV(IGRID)
      IF(NHYDSUB.GT.0) CALL GWF2HYD7SUB7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SWT7AR(IN,IGRID)
C     ******************************************************************
C     READ SWT PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,XYGRID
      USE HYDBASMODULE
      USE HYDSWTMODULE
      USE GWFSWTMODULE,   ONLY: LNWT,NSYSTM 
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      USE NUM2STR_INTERFACE,        ONLY: NUM2STR
      USE CONSTANTS,                ONLY: NL
      USE GENERIC_OPEN_INTERFACE,   ONLY: UTF8_BOM_OFFSET_REWIND
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN,IGRID
      TYPE(WARNING_TYPE):: WRN
C
      CHARACTER(256):: LINE
      CHARACTER( 20):: HYDSWTLBL
      CHARACTER(  3):: PCKG
      CHARACTER(  2):: ARR
      CHARACTER(  1):: INTYP
      INTEGER:: LLOC,ISTART,ISTOP,N,I, K
      INTEGER:: KLAY, NC1, NR1, NC2, NR2
      REAL:: XL, YL
      REAL:: ONE, ZERO, R
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSWT)
      ONE=1.0
      ZERO=0.0
      NHYDSWT=0
      CALL WRN%INIT()
      CALL UTF8_BOM_OFFSET_REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SWT') NHYDSWT=NHYDSWT+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR SWT HYDROGRAPH DATA
 19   IF(NHYDSWT.GT.0) THEN
        ALLOCATE(IBHYDSWT(NHYDSWT))
        ALLOCATE(INTRPHYDSWT(NHYDSWT))
        ALLOCATE(HYDSWTARR(NHYDSWT))
        ALLOCATE(IBTOP(NHYDSWT))
        ALLOCATE(SB_OB(NHYDSWT))
      ELSE
        ALLOCATE(IBHYDSWT(1))
        ALLOCATE(INTRPHYDSWT(1))
        ALLOCATE(HYDSWTARR(1))
        ALLOCATE(SB_OB(1))
         WRITE(IOUT,18)
  18    FORMAT(1X,'NO HYDROGRAPHS FOR SWT PACKAGE')
        GO TO 999
      END IF
C
C  Read SWT hydrograph data.
      NHYDSWT=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'SWT') GO TO 15
      NHYDSWT=NHYDSWT+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDSWTLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDSWTLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      !
      CALL XYGRID%XY2RC(XL,YL,NR1,NC1)
      NC2 = NC1 ! no longer used
      NR2 = NR1 
      IF(NR1 == 0)THEN
         CALL WRN%ADD(TRIM(ADJUSTL(LINE))//NL )
         NHYDSWT=NHYDSWT-1
         GO TO 15
      END IF
      !
      WRITE(HYDSWTLBL(4:6),FMT='(I3.3)')KLAY
      HYDSWTLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDSWTARR(NHYDSWT)='HC'
         IBHYDSWT(NHYDSWT)=.TRUE.
cc    TIME SERIES FROM THE TOTAL COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDSWTARR(NHYDSWT)='CP'
         IBHYDSWT(NHYDSWT)=.TRUE.
cc    TIME SERIES FROM THE GEOSTATIC STRESS
      ELSE IF (ARR.EQ.'GS') THEN
         HYDSWTARR(NHYDSWT)='GS'
         IBHYDSWT(NHYDSWT)=.TRUE.
cc    TIME SERIES FROM THE EFFECTIVE STRESS
      ELSE IF (ARR.EQ.'ES') THEN
         HYDSWTARR(NHYDSWT)='ES'
         IBHYDSWT(NHYDSWT)=.TRUE.
cc    TIME SERIES FROM THE VOID RATIO
      ELSE IF (ARR.EQ.'VR') THEN
         HYDSWTARR(NHYDSWT)='VR'
         IBHYDSWT(NHYDSWT)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDSWTARR(NHYDSWT)='SB'
         IBHYDSWT(NHYDSWT)=.FALSE.
      ELSE
          WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSWT=NHYDSWT-1
         GO TO 15
      ENDIF
C
C WRITE OUT HYDMOD POINT INFORMATION
        WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDSWTARR(NHYDSWT),INTYP,KLAY,XL,YL,NR1,NC1,HYDSWTLBL
C
      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
         INTRPHYDSWT(NHYDSWT)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
           WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            NHYDSWT=NHYDSWT-1
            GO TO 15
         ENDIF
      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
         INTRPHYDSWT(NHYDSWT)=.TRUE.
      ELSE
          WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following hydrograph record:',/,A80)
          WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSWT=NHYDSWT-1
         GO TO 15
      ENDIF
      !
      N = NHYDSWT
      IF(HYDSWTARR(N).EQ.'SB') THEN
        !
        K = COUNT(0 < LNWT .AND. LNWT <= KLAY)
        IF (K == 0) THEN
          WRITE(IOUT,28) LINE
 28      FORMAT(' SPECIFIED LAYERS DO NOT HAVE ANY INTERBEDS',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSWT=NHYDSWT-1
         GO TO 15
        END IF
        !
        CALL SB_OB(NHYDSWT)%INIT(K) ! HAS K INTERBEDS WITHIN LAYERS
        !
        K = 0
        DO I=1, NSYSTM
          IF(0 < LNWT(I) .AND. LNWT(I) <= KLAY) THEN
             K=K+1
             CALL SB_OB(NHYDSWT)%OP(K)%INIT(XYGRID,' ',XL,YL,LNWT(I), 
     +                                      INTYP.EQ.'I', 0,NR1,NC1, I) !NSYSYEM = I
          END IF
        END DO
      ELSE
        !
        K = COUNT(LNWT == KLAY)
        IF (K == 0) THEN
          WRITE(IOUT,29) LINE
 29      FORMAT(' SPECIFIED LAYER DOES NOT HAVE ANY INTERBEDS',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSWT=NHYDSWT-1
         GO TO 15
        END IF
        !
        CALL SB_OB(NHYDSWT)%INIT(K) ! HAS K INTERBEDS WITHIN LAYERS
        !
        K = 0
        DO I=1, NSYSTM
          IF(LNWT(I) == KLAY) THEN
             K=K+1
             CALL SB_OB(NHYDSWT)%OP(K)%INIT(XYGRID,' ',XL,YL,KLAY, 
     +                                      INTYP.EQ.'I', 0,NR1,NC1, I) !NSYSYEM = I
          END IF
        END DO
      END IF
C
C  Save the hydrograph label and process the next line in input file
      HYDLBL(NHYDTOT+NHYDSWT)=HYDSWTLBL
      GO TO 15
C
C  End of file after all SWT hydrograph data have been processed
 99   IF(NHYDSWT.GT.0) THEN
        WRITE(IOUT,108) NHYDSWT
108     FORMAT(/,' A total of ',I10,' points have been added ',
     & 'for the hydrographs of SWT arrays.')
      END IF
      !
      IF(WRN%RAISED) THEN
          CALL WRN%CHECK('HYDMOD NOTICE - THE FOLLOWING LINES '//
     +     'HAD BAD X, Y COORDIANTES:', OUTPUT=IOUT)
      END IF
C
 999  CALL SGWF2HYD7SWT7PSV(IGRID)
      IF(NHYDSWT.GT.0) CALL GWF2HYD7SWT7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7AR(IN,IGRID)
C     ******************************************************************
C     READ STREAM PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      USE GLOBAL,    ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSTRMODULE
      IMPLICIT NONE
C      
      INTEGER, INTENT(IN):: IN,IGRID
C
      CHARACTER(256):: LINE
      INTEGER:: LLOC,ISTART,ISTOP,N
      REAL:: R
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSTR)
C  Count number of Stream hydrographs
      NHYDSTR=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=15) LINE
 10   READ(IN,'(A)',END=15) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'STR') NHYDSTR=NHYDSTR+1
      GO TO 10
C
C  Allocate memory
 15   IF(NHYDSTR.GT.0) THEN
        ALLOCATE (ISTRHYD(NHYDSTR))
        ALLOCATE (HYDSTRARR(NHYDSTR))
      ELSE
        ALLOCATE (ISTRHYD(1))
        ALLOCATE (HYDSTRARR(1))
      END IF
C
C ------RETURN.
      CALL SGWF2HYD7STR7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSTRMODULE
      USE GWFSTRMODULE, ONLY: ISTRM,NSTREM
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN,KPER,IGRID
C
      CHARACTER(256):: LINE
      CHARACTER( 20):: HYDSTRLBL
      CHARACTER(  1):: INTYP
      INTEGER:: LLOC,ISTART,ISTOP,N
      INTEGER:: NUMSTR, ISEG, IRCH, KLAY
      REAL:: XL, YL
      REAL:: R
C     ------------------------------------------------------------------
      CALL SGWF2HYD7STR7PNT(IGRID)
C
      IF(NSTREM.LT.1)THEN
         WRITE(IOUT,8)
  8      FORMAT(' No Active Streams in this Model')
         WRITE(IOUT,'(1X,A)')
     1        'Stream hydrograph records will be ignored.'
         NHYDSTR=0
         RETURN
      END IF
C
C
C ------Read STR hydrograph data
C ------Reading is done here (rather than in AR) because stream data are
C ------not available until the first time step is initiated.
C ---- Reading could be done in a special RP routine right after STRRP
      !IF(KPER.EQ.1) THEN
        NUMSTR=0
        CALL UTF8_BOM_OFFSET_REWIND(IN)
        READ(IN,'(A)',END=99) LINE
 20     READ(IN,'(A)',END=99) LINE
        IF(LINE.EQ.' ') GO TO 20
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).NE.'STR') GO TO 20
C
C ------PROCESS A STR HYDROGRAPH RECORD
        NUMSTR=NUMSTR+1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        HYDSTRARR(NUMSTR)=LINE(ISTART:ISTOP)
        WRITE(HYDSTRLBL(1:2),FMT='(A2)') HYDSTRARR(NUMSTR)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        INTYP=LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        WRITE(HYDSTRLBL(3:5),FMT='(I3.3)') INT(XL)
        WRITE(HYDSTRLBL(6:8),FMT='(I3.3)') INT(YL)
        HYDSTRLBL(9:20)=LINE(ISTART:ISTOP)
C
        IF(HYDSTRARR(NUMSTR).NE.'ST' .AND.
     1      HYDSTRARR(NUMSTR).NE.'SO' .AND.
     2      HYDSTRARR(NUMSTR).NE.'SI' .AND.
     3      HYDSTRARR(NUMSTR).NE.'SA') THEN
           WRITE(IOUT,25) LINE
 25        FORMAT(' Invalid streamflow array was found on the following'
     &     ,' record:',/,A80)
           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSTR=NUMSTR-1
           GO TO 20
        ENDIF
C
C ------Look through all reaches to find the hydrograph reach.
        DO 30 N=1,NSTREM
 26       ISEG=ISTRM(4,N)
          IRCH=ISTRM(5,N)
C    XL contains the SEGMENT number and YL contains the REACH number. 
          IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
            ISTRHYD(NUMSTR)=N
            GO TO 35
          END IF
 30     CONTINUE
         WRITE(IOUT,*)
     1      ' Hydrograph specified for non-existent stream reach'
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        NUMSTR=NUMSTR-1
        GO TO 20
C
C ------The interpolation type must be 'C'
 35     IF(INTYP.NE.'C') THEN
            WRITE(IOUT,39) LINE
 39        FORMAT(' Invalid interpolation type was found on the ',
     &     'following record:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSTR=NUMSTR-1
           GO TO 20
        ENDIF
C
C ------SAVE THE LABEL AND PROCESS NEXT RECORD.
        HYDLBL(NHYDTOT+NUMSTR)=HYDSTRLBL
        GO TO 20
C
C ------End of input file.
 99     WRITE(IOUT,100) NUMSTR
100     FORMAT(/,' A total of ',I10,' hydrographs have been added ',
     &   'for STR arrays.')
        NHYDSTR=NUMSTR
C
C  Create initial values for stream hydrographs
        IF(NHYDSTR.GT.0) CALL GWF2HYD7STR7SE(2,IGRID)
      !END IF
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7AR(IN,IGRID)
C     ******************************************************************
C     READ SFR2 PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      USE HYDBASMODULE
      USE HYDSFRMODULE
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN,IGRID
C
      CHARACTER(256):: LINE
      INTEGER:: LLOC,ISTART,ISTOP,N
      REAL::R
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSFR)
C  Count number of Stream hydrographs
      NHYDSFR=0
      CALL UTF8_BOM_OFFSET_REWIND(IN)
      READ(IN,'(A)',END=15) LINE
 10   READ(IN,'(A)',END=15) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFR') NHYDSFR=NHYDSFR+1
      GO TO 10
C
C  Allocate memory
 15   IF(NHYDSFR.GT.0) THEN
        ALLOCATE (ISFRHYD(NHYDSFR))
        ALLOCATE (HYDSFRARR(NHYDSFR))
      ELSE
        ALLOCATE (ISFRHYD(1))
        ALLOCATE (HYDSFRARR(1))
      END IF
C
C ------RETURN.
      CALL SGWF2HYD7SFR7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSFRMODULE
      USE GWFSFRMODULE,      ONLY: ISTRM,NSTRM
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN,KPER,IGRID
C
      CHARACTER(256):: LINE
      CHARACTER( 20):: HYDSFRLBL
      CHARACTER(  1):: INTYP
      INTEGER:: NUMSFR, ISEG, IRCH, KLAY, LEN_XL, LEN_YL
      INTEGER::LLOC,ISTART,ISTOP,N
      REAL:: R, XL, YL
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SFR7PNT(IGRID)
C
      IF(NSTRM.LT.1)THEN
         WRITE(IOUT,8)
  8      FORMAT(' No Active Streams in this Model')
         WRITE(IOUT,'(1X,A)')
     1        'Stream hydrograph records will be ignored.'
         NHYDSFR=0
         RETURN
      END IF
C
C
C ------Read SFR hydrograph data
C ------Reading is done here (rather than in AR) because stream data are
C ------not available until the first time step is initiated.
C ---- Reading could be done in a special RP routine right after SFRRP
      !IF(KPER.EQ.1) THEN
        NUMSFR=0
        CALL UTF8_BOM_OFFSET_REWIND(IN)
        READ(IN,'(A)',END=99) LINE
 20     READ(IN,'(A)',END=99) LINE
        IF(LINE.EQ.' ') GO TO 20
        HYDSFRLBL=''  !seb START REBUILDING NEW HYDLABEL
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).NE.'SFR') GO TO 20
C
C ------PROCESS A SFR HYDROGRAPH RECORD
        NUMSFR=NUMSFR+1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        HYDSFRARR(NUMSFR)=LINE(ISTART:ISTOP)
        WRITE(HYDSFRLBL(1:2),FMT='(A2)') HYDSFRARR(NUMSFR)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        INTYP=LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        LEN_XL=LEN( NUM2STR(INT(XL)) )                                  !seb CHECK FOR OVERAGE OF SIZE
        LEN_YL=LEN( NUM2STR(INT(YL)) )
        IF(LEN_XL.LE.3 .AND. LEN_YL.LE.3) THEN
          WRITE(HYDSFRLBL(3:5),FMT='(I3.3)') INT(XL)          !XL = SEGMENT #
          WRITE(HYDSFRLBL(6:8),FMT='(I3.3)') INT(YL)          !YL = REACH #
          HYDSFRLBL(9:20)=LINE(ISTART:ISTOP)
        ELSE
          HYDSFRLBL=HYDSFRLBL(1:2) // 
     +        NUM2STR(INT(XL)) // NUM2STR(INT(YL)) // LINE(ISTART:ISTOP)
         !
         WRITE(IOUT,'(//A, / 3x,3A, / 3x,4A, / 3x,2A,//)')
     +    'HYDMOD WARNING: SPECIFIED SFR SEGMENT AND/OR REACH '//
     +                                  'CONTAINS MORE THAN 3 DIGITS.',
     +    'THE UNIQUE ID ',LINE(ISTART:ISTOP),' MAYBE TRUNCATED ',
     +    'FOR SFR SEGMENT ', NUM2STR(INT(XL)), 
     +    ' AND REACH ', NUM2STR(INT(YL)), 
     +    'THE RESULTING ID IS: ', TRIM(HYDSFRLBL)
        END IF
C
        IF(HYDSFRARR(NUMSFR).NE.'ST' .AND.
     1      HYDSFRARR(NUMSFR).NE.'SO' .AND.
     2      HYDSFRARR(NUMSFR).NE.'SI' .AND.
     3      HYDSFRARR(NUMSFR).NE.'SA') THEN
            WRITE(IOUT,25) LINE
 25        FORMAT(' Invalid SFR array was found on the following'
     &     ,' record:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSFR=NUMSFR-1
           GO TO 20
        ENDIF
C
C ------Look through all reaches to find the hydrograph reach.
        DO 30 N=1,NSTRM
 26       ISEG=ISTRM(4,N)
          IRCH=ISTRM(5,N)
C    XL contains the SEGMENT number and YL contains the REACH number. 
          IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
            ISFRHYD(NUMSFR)=N
            GO TO 35
          END IF
 30     CONTINUE
         WRITE(IOUT,*)
     1      ' Hydrograph specified for non-existent stream reach'
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        NUMSFR=NUMSFR-1
        GO TO 20
C
C ------The interpolation type must be 'C'
 35     IF(INTYP.NE.'C') THEN
            WRITE(IOUT,39) LINE
 39        FORMAT(' Invalid interpolation type was found on the ',
     &     'following record:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSFR=NUMSFR-1
           GO TO 20
        ENDIF
C
C ------SAVE THE LABEL AND PROCESS NEXT RECORD.
        HYDLBL(NHYDTOT+NUMSFR)=HYDSFRLBL
        GO TO 20
C
C ------End of input file.
 99     WRITE(IOUT,100) NUMSFR
100     FORMAT(/,' A total of ',I10,' hydrographs have been added ',
     &   'for SFR arrays.')
        NHYDSFR=NUMSFR
C
C  Create initial values for stream hydrographs
        IF(NHYDSFR.GT.0) CALL GWF2HYD7SFR7SE(2,IGRID)
      !END IF
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7BAS7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR BAS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: HNEW,IBOUND,BOTM,LBOTM,LAYHDT,
     +                    DELR,DELC,NROW,NCOL,NLAY
      USE HYDBASMODULE
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IHYDLOC,IGRID
C
      INTEGER::N, I, J, K, IBFACT
C     ------------------------------------------------------------------
      CALL SGWF2HYD7BAS7PNT(IGRID)
C
C -----Return if no BAS hydrographs.
      NHYDTOT=NHYDBAS
      IF(NHYDBAS.LE.0) RETURN
C
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NHYDBAS
      !NN=N
C -----Determine interpolation type, word length, number of weights,
C -----and locations of IBOUND codes at or around hydrograph point.
      I=HD_OP(N)%ROW   !JIKHYDBAS(2,N)
      J=HD_OP(N)%COL   !JIKHYDBAS(1,N)
      K=HD_OP(N)%LAY   !JIKHYDBAS(3,N)
C
C -----If IBOUND is to be checked for inactive cells, retrieve values
C -----at or around hydrograph point.
      !
      IBFACT=IBOUND(J,I,K)
      IF(IBFACT .NE. 0 .AND. HAS_NWT_HDRY) THEN                            !Only apply check if NWT uses HDRY instead of regular head
          IF(LAYHDT(K).NE.0) THEN
             IF(HNEW(J,I,K).LE.BOTM(J,I,LBOTM(K))) IBFACT=0             !seb CHECK TO SEE IF THE CELL IS UPW DRY
          END IF
      END IF
      !
      IF(IBFACT .NE. 0) THEN
        CALL HD_OP(N)%BASIS_BUILDER(NROW,NCOL,NLAY,
     +                                  IBOUND,DELC,DELR)
        !
        CALL HD_OP(N)%INTERP(NROW,NCOL,NLAY,HNEW) 
      END IF
C
C -----Check if hydrograph value is HEAD.
      IF(HYDBASARR(N).EQ.'HD') THEN
         IF(IBFACT.EQ.0) THEN !IBHYDBAS(N) .AND. 
            HYDVAL(N,IHYDLOC)=HYDNOH
         ELSE
            HYDVAL(N,IHYDLOC)=HD_OP(N)%SIM !SHYD7WTAVG(NN)
         ENDIF
C
C -----Hydrograph value is DRAWDOWN if NOT HEAD
      ELSE
         IF(IBFACT.EQ.0) THEN  !IBHYDBAS(N) .AND. 
            HYDVAL(N,IHYDLOC)=HYDNOH
         ELSE
            HYDVAL(N,IHYDLOC)=HYDBASSTRT(N) - HD_OP(N)%SIM !SHYD7WTAVG(NN)
         ENDIF
      ENDIF
   50 CONTINUE
C
C ------ RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7IBS7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR IBS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: HNEW,IBOUND,BOTM,LBOTM,LAYHDT
      USE HYDBASMODULE
      USE HYDIBSMODULE
      USE GWFIBSMODULE,   ONLY: HC,SUB
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IHYDLOC,IGRID
C
      INTEGER:: KLAY
      INTEGER::N, I, J, K, IBFACT
      INTEGER:: IB1, IB2, IB3, IB4
      REAL:: W1,W2,W3,W4,TOTL
C     ------------------------------------------------------------------
      CALL SGWF2HYD7IBS7PNT(IGRID)
C
C -----Return if no IBS hydrographs.
      IF(NHYDIBS.LE.0) RETURN
C
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NHYDIBS
      !NN=N
C -----Determine interpolation type, word length, number of weights,
C -----and locations of IBOUND codes at or around hydrograph point.
      J=JIKHYDIBS(1,N)
      I=JIKHYDIBS(2,N)
      K=JIKHYDIBS(3,N)
C
C -----If IBOUND is to be checked for inactive cells, retrieve values
C -----at or around hydrograph point.
      IF(IBHYDIBS(N)) THEN
         IB1=IBOUND(J,I,K)
         IF(LAYHDT(K).ne.0)then
            IF(HNEW(J,I,K).LE.BOTM(J,I,LBOTM(K))) IB1=0                 !seb CHECK TO SEE IF THE CELL IS UPW DRY
         END IF
         IBFACT=IB1
         IF(INTRPHYDIBS(N)) THEN
            IB2=IBOUND(J+1,I,K)
            IB3=IBOUND(J+1,I-1,K)
            IB4=IBOUND(J,I-1,K)
            IF(LAYHDT(K).ne.0)then
              IF( HNEW(J+1,I,  K).LE.BOTM(J+1,I,  LBOTM(K)) ) IB2=0     !seb ADDED CHECK FOR UPW DRY CELLS
              IF( HNEW(J+1,I-1,K).LE.BOTM(J+1,I-1,LBOTM(K)) ) IB3=0
              IF( HNEW(J,  I-1,K).LE.BOTM(J,  I-1,LBOTM(K)) ) IB4=0
            END IF
            !
            IBFACT=IB1*IB2*IB3*IB4
         ENDIF
      ENDIF
C
C -----Check if hydrograph value is Preconsolidation Head.
      IF(HYDIBSARR(N).EQ.'HC') THEN
         IF(IBHYDIBS(N) .AND. IBFACT.EQ.0) THEN
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH                   
         ELSE
            J=JIKHYDIBS(1,N)
            I=JIKHYDIBS(2,N)
            K=JIKHYDIBS(3,N)
            W1=HYDIBSWT(1,N)
            W2=HYDIBSWT(2,N)
            W3=HYDIBSWT(3,N)
            W4=HYDIBSWT(4,N)
            HYDVAL(NHYDTOT+N,IHYDLOC)=
     1 HC(J,I,K)*W1+HC(J+1,I,K)*W2+HC(J+1,I-1,K)*W3+HC(J,I-1,K)*W4
         ENDIF
C
C -----Check if hydrograph value is compaction
       ELSE IF(HYDIBSARR(N).EQ.'CP') THEN
         IF(IBHYDIBS(N) .AND. IBFACT.EQ.0) THEN
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
         ELSE
            J=JIKHYDIBS(1,N)
            I=JIKHYDIBS(2,N)
            K=JIKHYDIBS(3,N)
            W1=HYDIBSWT(1,N)
            W2=HYDIBSWT(2,N)
            W3=HYDIBSWT(3,N)
            W4=HYDIBSWT(4,N)
            HYDVAL(NHYDTOT+N,IHYDLOC)=
     1 SUB(J,I,K)*W1+SUB(J+1,I,K)*W2+SUB(J+1,I-1,K)*W3+SUB(J,I-1,K)*W4
         ENDIF
C
C -----Hydrograph value must be subsidence (not HC and not CP)
      ELSE
         KLAY=JIKHYDIBS(3,N)
         TOTL=0.
         DO 40 K=1,KLAY
            J=JIKHYDIBS(1,N)
            I=JIKHYDIBS(2,N)
            W1=HYDIBSWT(1,N)
            W2=HYDIBSWT(2,N)
            W3=HYDIBSWT(3,N)
            W4=HYDIBSWT(4,N)
            TOTL=TOTL+
     1 SUB(J,I,K)*W1+SUB(J+1,I,K)*W2+SUB(J+1,I-1,K)*W3+SUB(J,I-1,K)*W4
   40    CONTINUE
         HYDVAL(NHYDTOT+N,IHYDLOC)=TOTL
      ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDIBS
C
C ------ RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SUB7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR SUB
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IBOUND,NROW,NCOL,NLAY,DELC,DELR
      USE HYDBASMODULE
      USE HYDSUBMODULE
      USE GWFSUBMODULE,   ONLY: HC,SUB,SUBE,SUBV,NNDB
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IHYDLOC, IGRID
      !INTEGER, DIMENSION(NNDB):: LNLOC
      INTEGER::NDIM, N, I, J, K
      LOGICAL:: IB_TRUE
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SUB7PNT(IGRID)
C
C -----Return if no SUB hydrographs.
      IF(NHYDSUB.LE.0) RETURN
C
!seb      IK=0             !ADDED BY RTH, BUT NOT INCREMENTED IN SUBRTRACTION AT END OF SUBROUTINE
C -----Calculate value for each hydrograph point.
      NDIM = NROW*NCOL*NNDB
      DO 50 N=1,NHYDSUB
       !
       DO CONCURRENT(K=1:SB_OB(N)%N)
          CALL SB_OB(N)%OP(K)%BASIS_BUILDER(NROW,NCOL,NLAY,
     +                                      IBOUND,DELC,DELR)
       END DO
       !
       IF(HYDSUBARR(N).EQ.'HC'.or.HYDSUBARR(N).EQ.'CE'.or.
     &    HYDSUBARR(N).EQ.'CV'.or.HYDSUBARR(N).EQ.'CP')THEN
          I=SB_OB(N)%OP(1)%ROW
          J=SB_OB(N)%OP(1)%COL
          K=SB_OB(N)%OP(1)%LAY
          IB_TRUE = IBOUND(J,I,K).NE.0
      ELSE 
          IB_TRUE = .TRUE.
      END IF
C
C -----Check if hydrograph value is Preconsolidation Head.
      IF(HYDSUBARR(N).EQ.'HC') THEN
       !
       IF(IB_TRUE) THEN
        DO CONCURRENT(K=1:SB_OB(N)%N)
           CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,HC)
        END DO
        !   
        HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
        DO K=1, SB_OB(N)%N
         !
         HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                               SB_OB(N)%OP(K)%SIM
        END DO
        HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) /
     1                                                  DBLE(SB_OB(N)%N)
       ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
       END IF
C
C -----Check if hydrograph value is compaction
       ELSE IF(HYDSUBARR(N).EQ.'CP') THEN
        !
        IF(IB_TRUE) THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,SUB)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
        ELSE
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
        END IF
       ELSE IF(HYDSUBARR(N).EQ.'CE') THEN
        !
        IF(IB_TRUE) THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,SUBE)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
        ELSE
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
        END IF
       ELSE IF(HYDSUBARR(N).EQ.'CV') THEN
        !
        IF(IB_TRUE) THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,SUBV)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
        ELSE
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
        END IF
C
C -----Hydrograph value must be subsidence (not HC and not CP)
      ELSE IF(HYDSUBARR(N).EQ.'SB') THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,SUB)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
      ELSE IF(HYDSUBARR(N).EQ.'SE') THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,SUBE)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
      ELSE IF(HYDSUBARR(N).EQ.'SV') THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP_SUB(NROW,NCOL,NDIM,SUBV)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
       ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDSUB
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SWT7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR SWT
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IBOUND,NROW,NCOL,NLAY,DELC,DELR
      USE HYDBASMODULE
      USE HYDSWTMODULE
      USE GWFSWTMODULE,   ONLY: PCS,GL,VOID,EST,SUB,NSYSTM
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IHYDLOC, IGRID
      INTEGER::N, I, J, K
      LOGICAL:: IB_TRUE
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SWT7PNT(IGRID)
C
C -----Return if no SWT hydrographs.
      IF(NHYDSWT.LE.0) RETURN
C
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NHYDSWT
       !
       DO CONCURRENT(K=1:SB_OB(N)%N)
          CALL SB_OB(N)%OP(K)%BASIS_BUILDER(NROW,NCOL,NLAY,
     +                                      IBOUND,DELC,DELR)
       END DO
       !
       IF(HYDSWTARR(N).EQ.'HC')THEN
          I=SB_OB(N)%OP(1)%ROW
          J=SB_OB(N)%OP(1)%COL
          K=SB_OB(N)%OP(1)%LAY
          IB_TRUE = IBOUND(J,I,K).NE.0
      ELSE 
          IB_TRUE = .TRUE.
      END IF
C
C -----Check if hydrograph value is Preconsolidation Head.
      IF(HYDSWTARR(N).EQ.'HC') THEN
       !
       IF(IB_TRUE) THEN
        DO CONCURRENT(K=1:SB_OB(N)%N)
           CALL SB_OB(N)%OP(K)%INTERP(NROW,NCOL,NSYSTM,PCS)
        END DO
        !   
        HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
        DO K=1, SB_OB(N)%N
         !
         HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                               SB_OB(N)%OP(K)%SIM
        END DO
        HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) /
     1                                                  DBLE(SB_OB(N)%N)
       ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
       END IF
C
C -----Check if hydrograph value is GEOSTATIC STRESS
      ELSE IF(HYDSWTARR(N).EQ.'GS') THEN
       !
       IF(IB_TRUE) THEN
        DO CONCURRENT(K=1:SB_OB(N)%N)
           CALL SB_OB(N)%OP(K)%INTERP(NROW,NCOL,NSYSTM,GL)
        END DO
        !   
        HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
        DO K=1, SB_OB(N)%N
         !
         HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                               SB_OB(N)%OP(K)%SIM
        END DO
        HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) /
     1                                                  DBLE(SB_OB(N)%N)
       ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
       END IF
C
C -----Check if hydrograph value is EFFECTIVE STRESS
      ELSE IF(HYDSWTARR(N).EQ.'ES') THEN
       !
       IF(IB_TRUE) THEN
        DO CONCURRENT(K=1:SB_OB(N)%N)
           CALL SB_OB(N)%OP(K)%INTERP(NROW,NCOL,NSYSTM,EST)
        END DO
        !   
        HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
        DO K=1, SB_OB(N)%N
         !
         HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                               SB_OB(N)%OP(K)%SIM
        END DO
        HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) /
     1                                                  DBLE(SB_OB(N)%N)
       ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
       END IF
C
C -----Check if hydrograph value is VOID RATIO
      ELSE IF(HYDSWTARR(N).EQ.'VR') THEN
       !
       IF(IB_TRUE) THEN
        DO CONCURRENT(K=1:SB_OB(N)%N)
           CALL SB_OB(N)%OP(K)%INTERP(NROW,NCOL,NSYSTM,VOID)
        END DO
        !   
        HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
        DO K=1, SB_OB(N)%N
         !
         HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                               SB_OB(N)%OP(K)%SIM
        END DO
        HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) /
     1                                                  DBLE(SB_OB(N)%N)
       ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
       END IF
C
C -----Check if hydrograph value is compaction
       ELSE IF(HYDSWTARR(N).EQ.'CP') THEN
        !
        IF(IB_TRUE) THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP(NROW,NCOL,NSYSTM,SUB)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO
        ELSE
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
        END IF
C
C -----Hydrograph value must be subsidence (not HC and not CP)
      ELSE IF(HYDSWTARR(N).EQ.'SB') THEN
         DO CONCURRENT(K=1:SB_OB(N)%N)
            CALL SB_OB(N)%OP(K)%INTERP(NROW,NCOL,NSYSTM,SUB)
         END DO
         !   
         HYDVAL(NHYDTOT+N,IHYDLOC) = 0.0
         DO K=1, SB_OB(N)%N
          !
          HYDVAL(NHYDTOT+N,IHYDLOC) = HYDVAL(NHYDTOT+N,IHYDLOC) +
     1                                SB_OB(N)%OP(K)%SIM
         END DO     
       ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDSWT
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,HNEW,BOTM,LBOTM,LAYHDT,NLAY
      USE HYDBASMODULE
      USE HYDSTRMODULE
      USE GWFSTRMODULE, ONLY: ISTRM,STRM
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IHYDLOC,IGRID
C
      INTEGER::N, I, J, K, II
      INTEGER:: ISTR
C     ------------------------------------------------------------------
      CALL SGWF2HYD7STR7PNT(IGRID)
C
C ------Return if no STR hydrographs.
      IF(NHYDSTR.LE.0) RETURN
C
C ------Calculate value for each stream hydrograph.
      DO 50 N=1,NHYDSTR
      ISTR=ISTRHYD(N)
C
C ------Check for the hydrograph type.
cc    ARR/IRR designate the streamflow option with 
cc    ST==> stream stage, SO==> out of reach,
cc    SI==> into reach, and SA==> into aquifer.
      IF(HYDSTRARR(N).EQ.'ST') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(2,ISTR)
      ELSE IF(HYDSTRARR(N).EQ.'SO') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(9,ISTR)
      ELSE IF(HYDSTRARR(N).EQ.'SI') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(10,ISTR)
      ELSE IF(HYDSTRARR(N).EQ.'SA') THEN
         K=ISTRM(1,ISTR)
         I=ISTRM(2,ISTR)
         J=ISTRM(3,ISTR)
         IF(    IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).EQ.0 ) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISTR)
        ELSEIF (IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).NE.0 .AND. 
     +                        HNEW(J,I,K) .GT. BOTM(J,I,LBOTM(K))) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISTR)
         ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH  !ASSUME DRY
           !
           II = K+1
           DO K=II, NLAY
            IF (IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).NE.0 .AND. 
     +                        HNEW(J,I,K) .GT. BOTM(J,I,LBOTM(K))) THEN
                HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISTR)
                EXIT
            ELSEIF(IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).EQ.0) THEN
                HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISTR)
                EXIT
            END IF
           END DO
         END IF
      ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDSTR
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,HNEW,BOTM,LBOTM,LAYHDT
      USE HYDBASMODULE
      USE HYDSFRMODULE
      USE GWFSFRMODULE, ONLY: ISTRM,STRM
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IHYDLOC,IGRID
C
      INTEGER::N, I, J, K
      INTEGER:: ISFR
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SFR7PNT(IGRID)
C
C ------Return if no SFR hydrographs.
      IF(NHYDSFR.LE.0) RETURN
C
C ------Calculate value for each stream hydrograph.
      DO 50 N=1,NHYDSFR
      ISFR=ISFRHYD(N)
C
C ------Check for the hydrograph type.
cc    ARR/IRR designate the streamflow option with 
cc    ST==> stream stage, SO==> out of reach,
cc    SI==> into reach, and SA==> into aquifer.
      IF(HYDSFRARR(N).EQ.'ST') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(15,ISFR)        
      ELSE IF(HYDSFRARR(N).EQ.'SO') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(9,ISFR)           
      ELSE IF(HYDSFRARR(N).EQ.'SI') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(10,ISFR)            
      ELSE IF(HYDSFRARR(N).EQ.'SA') THEN
         K=ISTRM(1,ISFR)
         I=ISTRM(2,ISFR)
         J=ISTRM(3,ISFR)
         IF(    IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).EQ.0) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISFR)          
        ELSEIF (IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).NE.0 .AND. 
     +                        HNEW(J,I,K) .GT. BOTM(J,I,LBOTM(K)) ) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISFR)          
         ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH                    
         END IF
      ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDSFR
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7BAS7OT(KSTP,KPER,IGRID)
C     ******************************************************************
C     WRITE HYDROGRAPH DATA FOR ONE TIME STEP
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      USE UTIL_INTERFACE,               ONLY: TO_SNGL
      USE GLOBAL,   ONLY:ITMUNI,IOUT,BIN_REAL_KIND
      USE GWFBASMODULE, ONLY: TOTIM
      USE HYDBASMODULE
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: KSTP,KPER,IGRID
C
      INTEGER::N
C     ------------------------------------------------------------------
      CALL SGWF2HYD7BAS7PNT(IGRID)
C
C1------RETURN IF NO HYDROGRAPH RECORDS.
      IF(NHYDTOT.LE.0) RETURN
C
C2------IF THIS IS THE FIRST TIME IN THE SIMULATION, WRITE HEADER RECORD.
      IF(KPER.EQ.1 .AND. KSTP.EQ.1) THEN
        WRITE(IOUT,130) NHYDTOT
 130    FORMAT(/,1X,'A TOTAL OF ',I10,' HYDROGRAPH POINTS HAVE BEEN ',
     1        'PREPARED.')
        SELECT CASE(BIN_REAL_KIND)
        CASE(REAL32); WRITE(IHYDMUN)  NHYDTOT,ITMUNI
        CASE(REAL64); WRITE(IHYDMUN) -NHYDTOT,ITMUNI
        END SELECT
        !
        WRITE(IHYDMUN) 'TIME',HYDLBL(1:NHYDTOT)
        !
        SELECT CASE(KIND(TOTIM))
        CASE(REAL32)
               SELECT CASE(BIN_REAL_KIND)
               CASE(REAL32)
                           WRITE(IHYDMUN) 0.0_REAL32
                           DO N=1,NHYDTOT
                                 WRITE(IHYDMUN) HYDVAL(N,2)
                           END DO
               CASE(REAL64)
                           WRITE(IHYDMUN) 0.0_REAL64
                           DO N=1,NHYDTOT
                                 WRITE(IHYDMUN) REAL(HYDVAL(N,2),REAL64)
                           END DO 
               END SELECT
        CASE(REAL64)
               SELECT CASE(BIN_REAL_KIND)
               CASE(REAL32)
                           WRITE(IHYDMUN) 0.0_REAL32
                           DO N=1,NHYDTOT
                                 WRITE(IHYDMUN) TO_SNGL(HYDVAL(N,2))
                           END DO 
               CASE(REAL64)
                           WRITE(IHYDMUN) 0.0_REAL64
                           DO N=1,NHYDTOT
                                 WRITE(IHYDMUN) HYDVAL(N,2)
                           END DO
               END SELECT
        END SELECT
        !NDECDIG=PRECISION(TOTIM)
        !IF(NDECDIG.GT.9) THEN
        !  WRITE(IHYDMUN) -NHYDTOT,ITMUNI
        !ELSE
        !  WRITE(IHYDMUN) NHYDTOT,ITMUNI
        !END IF
        !WRITE(IHYDMUN) 'TIME',(HYDLBL(N),N=1,NHYDTOT)
        !WRITE(IHYDMUN) 0.0,(HYDVAL(N,2),N=1,NHYDTOT)
      ENDIF
C
C3------WRITE HYDROGRAPH RECORD FOR ONE TIME STEP.
      !WRITE(IHYDMUN) TOTIM,(HYDVAL(N,1),N=1,NHYDTOT)
      SELECT CASE(KIND(TOTIM))
      CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         WRITE(IHYDMUN) TOTIM
                         DO N=1,NHYDTOT
                               WRITE(IHYDMUN) HYDVAL(N,1)
                         END DO
             CASE(REAL64)
                        WRITE(IHYDMUN) REAL(TOTIM,REAL64)
                        DO N=1,NHYDTOT
                              WRITE(IHYDMUN) REAL(HYDVAL(N,1),REAL64)
                        END DO 
             END SELECT
      CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                        WRITE(IHYDMUN) TO_SNGL(TOTIM)
                        DO N=1,NHYDTOT
                              WRITE(IHYDMUN) TO_SNGL(HYDVAL(N,1))
                        END DO 
             CASE(REAL64)
                         WRITE(IHYDMUN) TOTIM
                         DO N=1,NHYDTOT
                               WRITE(IHYDMUN) HYDVAL(N,1)
                         END DO
             END SELECT
      END SELECT
C
C4------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,XX1,XX2,YY1,YY2)
C     ******************************************************************
C     LOCATE CELLS FOR HYDROGRAPH POINTS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: NCOL,NROW,DELR,DELC
      IMPLICIT NONE
C
      INTEGER, INTENT(INOUT):: NR1,NC1,NR2,NC2
      REAL,    INTENT(IN   ):: XL,YL
      REAL,    INTENT(INOUT):: XX1,XX2,YY1,YY2
C
      INTEGER::N
      REAL:: ZERO, HALF, X1,X2,XC,XCB,XCF,DXF, Y1,Y2,YC,YCB,YCF,DYF
C     ------------------------------------------------------------------
      ZERO=0.
      HALF=0.5
      XX1=ZERO
      XX2=ZERO
      YY1=ZERO
      YY2=ZERO
      X1=ZERO
      IF(XL.LT.X1) THEN
         NC1=0
         NC2=0
         GO TO 100
      ENDIF
      XCB=ZERO
      XCF=DELR(1)*HALF
      DO 10 N=1,NCOL
      X2=X1+DELR(N)
      XC=XCF
      DXF=ZERO
      IF(N.LT.NCOL) DXF=DELR(N+1)*HALF
      XCF=X2+DXF
      IF(XL.LE.X2) THEN
         NC1=N
         IF(XL.LT.XC) THEN
            NC2=N-1
            XX1=XCB
            XX2=XC
         ELSE
            NC2=N
            XX1=XC
            XX2=XCF
         ENDIF
         GO TO 100
      ENDIF
      X1=X2
      XCB=XC
 10   CONTINUE
      NC1=NCOL+1
      NC2=NCOL+1
 100  Y1=ZERO
      YCB=ZERO
      YCF=DELC(NROW)*HALF
      IF(YL.LT.Y1) THEN
         NR1=NROW+1
         NR2=NROW+1
         RETURN
      ENDIF
      DO 110 N=NROW,1,-1
      Y2=Y1+DELC(N)
      YC=YCF
      DYF=ZERO
      IF(N.GT.1) DYF=DELC(N-1)*HALF
      YCF=Y2+DYF
      IF(YL.LE.Y2) THEN
         NR1=N
         IF(YL.LT.YC) THEN
            NR2=N+1
            YY1=YCB
            YY2=YC
         ELSE
            NR2=N
            YY1=YC
            YY2=YCF
         ENDIF
         RETURN
      ENDIF
      Y1=Y2
      YCB=YC
 110  CONTINUE
      NC1=0
      NC2=0
      RETURN
      END
!!!      FUNCTION SHYD7WTAVG(N)
!!!C     ******************************************************************
!!!C     COMPUTE WEIGHTED AVERAGE OF HEAD
!!!C     ******************************************************************
!!!C
!!!C     SPECIFICATIONS:
!!!C     ------------------------------------------------------------------
!!!      USE GLOBAL,    ONLY: NCOL,NROW,NLAY,HNEW
!!!      USE HYDBASMODULE, ONLY: JIKHYDBAS,HYDBASWT
!!!      DOUBLE PRECISION W1,W2,W3,W4,HTOT
!!!C     ------------------------------------------------------------------
!!!      J=JIKHYDBAS(1,N)
!!!      I=JIKHYDBAS(2,N)
!!!      K=JIKHYDBAS(3,N)
!!!      W1=HYDBASWT(1,N)
!!!      W2=HYDBASWT(2,N)
!!!      W3=HYDBASWT(3,N)
!!!      W4=HYDBASWT(4,N)
!!!      HTOT=HNEW(J,I,K)*W1
!!!      if(W2.gt.0.)HTOT=HTOT+HNEW(J+1,I,K)*W2
!!!      if(W3.gt.0.)HTOT=HTOT+HNEW(J+1,I-1,K)*W3
!!!      if(W4.gt.0.)HTOT=HTOT+HNEW(J,I-1,K)*W4
!!!      SHYD7WTAVG=HTOT
!!!      RETURN
!!!      END
      SUBROUTINE SGWF2HYD7MW(X0,Y0,X1,X2,Y1,Y2,W1,W2,W3,W4)
C     ******************************************************************
C     COMPUTE WEIGHTS FOR BILINEAR INTERPOLATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN   ):: X0,Y0,X1,X2,Y1,Y2
      REAL, INTENT(  OUT):: W1,W2,W3,W4
      REAL:: DX, DY, DXY
C
C
      DX=(X0-X1)/(X2-X1)
      DY=(Y0-Y1)/(Y2-Y1)
      DXY=DX*DY
      W1=1-DX-DY+DXY
      W2=DX-DXY
      W3=DXY
      W4=DY-DXY
      RETURN
      END
      SUBROUTINE GWF2HYD7DA(IGRID)
C     ******************************************************************
C     SUBROUTINE TO DEALLOCATE ALL HYDMOD VARIABLES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IUNIT
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      
C1------DEALLOCATE HYDBAS VARIABLES      
      CALL SGWF2HYD7BAS7DA(IGRID)
C      
C2------CHECK IF HYDMOD IS USED WITH OTHER PACKAGES.  IF SO, DEALLOCATE
      IF(IUNIT(43).NE.0 .AND. IUNIT(19).NE.0) 
     1                   CALL SGWF2HYD7IBS7DA(IGRID)
      IF(IUNIT(43).NE.0 .AND. IUNIT(54).NE.0) 
     1                   CALL SGWF2HYD7SUB7DA(IGRID)
      IF(IUNIT(43).NE.0 .AND. IUNIT(57).NE.0) 
     1                   CALL SGWF2HYD7SWT7DA(IGRID)
      IF(IUNIT(43).NE.0 .AND. IUNIT(18).NE.0) 
     1                   CALL SGWF2HYD7STR7DA(IGRID)
      IF(IUNIT(43).NE.0 .AND. IUNIT(44).NE.0) 
     1                   CALL SGWF2HYD7SFR7DA(IGRID)
C3
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7DA(IGRID)
C  Deallocate HYD BAS memory
      USE HYDBASMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      INTEGER:: IERR
C
      DEALLOCATE(HYDBASDAT(IGRID)%NHYDTOT,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDVAL,      STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDLBL,      STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%IHYDMUN,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%NHYDBAS,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDNOH,      STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%IBHYDBAS,    STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%INTRPHYDBAS, STAT = IERR)
      !DEALLOCATE(HYDBASDAT(IGRID)%JIKHYDBAS,   STAT = IERR)
      !DEALLOCATE(HYDBASDAT(IGRID)%HYDBASWT,    STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDBASSTRT,  STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDBASARR,   STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HD_OP,       STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HAS_NWT_HDRY,STAT = IERR)
C
C NULLIFY LOCAL POINTERS
      IF(IGRID.EQ.1) THEN
         NHYDTOT    =>NULL()
         HYDVAL     =>NULL()
         HYDLBL     =>NULL()
         IHYDMUN    =>NULL()
         NHYDBAS    =>NULL()
         HYDNOH     =>NULL()
         IBHYDBAS   =>NULL()
         INTRPHYDBAS=>NULL()
         !JIKHYDBAS  =>NULL()
         !HYDBASWT   =>NULL()
         HYDBASSTRT =>NULL()
         HYDBASARR  =>NULL()
         HD_OP      =>NULL()
        HAS_NWT_HDRY=>NULL()
      END IF    

      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7PNT(IGRID)
C  Change HYD BAS data to a different grid.
      USE HYDBASMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      NHYDTOT=>HYDBASDAT(IGRID)%NHYDTOT
      HYDVAL=>HYDBASDAT(IGRID)%HYDVAL
      HYDLBL=>HYDBASDAT(IGRID)%HYDLBL
      IHYDMUN=>HYDBASDAT(IGRID)%IHYDMUN
      NHYDBAS=>HYDBASDAT(IGRID)%NHYDBAS
      HYDNOH=>HYDBASDAT(IGRID)%HYDNOH
      IBHYDBAS=>HYDBASDAT(IGRID)%IBHYDBAS
      INTRPHYDBAS=>HYDBASDAT(IGRID)%INTRPHYDBAS
      !JIKHYDBAS=>HYDBASDAT(IGRID)%JIKHYDBAS
      !HYDBASWT=>HYDBASDAT(IGRID)%HYDBASWT
      HYDBASSTRT=>HYDBASDAT(IGRID)%HYDBASSTRT
      HYDBASARR=>HYDBASDAT(IGRID)%HYDBASARR
      HD_OP=>HYDBASDAT(IGRID)%HD_OP
      HAS_NWT_HDRY=>HYDBASDAT(IGRID)%HAS_NWT_HDRY
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7PSV(IGRID)
C  Save HYD BAS data for a grid.
      USE HYDBASMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      HYDBASDAT(IGRID)%NHYDTOT=>NHYDTOT
      HYDBASDAT(IGRID)%HYDVAL=>HYDVAL
      HYDBASDAT(IGRID)%HYDLBL=>HYDLBL
      HYDBASDAT(IGRID)%IHYDMUN=>IHYDMUN
      HYDBASDAT(IGRID)%NHYDBAS=>NHYDBAS
      HYDBASDAT(IGRID)%HYDNOH=>HYDNOH
      HYDBASDAT(IGRID)%IBHYDBAS=>IBHYDBAS
      HYDBASDAT(IGRID)%INTRPHYDBAS=>INTRPHYDBAS
      !HYDBASDAT(IGRID)%JIKHYDBAS=>JIKHYDBAS
      !HYDBASDAT(IGRID)%HYDBASWT=>HYDBASWT
      HYDBASDAT(IGRID)%HYDBASSTRT=>HYDBASSTRT
      HYDBASDAT(IGRID)%HYDBASARR=>HYDBASARR
      HYDBASDAT(IGRID)%HD_OP=>HD_OP
      HYDBASDAT(IGRID)%HAS_NWT_HDRY=>HAS_NWT_HDRY
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7DA(IGRID)
C  Deallocate HYD IBS memory
      USE HYDIBSMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      DEALLOCATE(HYDIBSDAT(IGRID)%NHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%IBHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%INTRPHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%JIKHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%HYDIBSWT)
      DEALLOCATE(HYDIBSDAT(IGRID)%HYDIBSARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7PNT(IGRID)
C  Change HYD IBS data to a different grid.
      USE HYDIBSMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      NHYDIBS=>HYDIBSDAT(IGRID)%NHYDIBS
      IBHYDIBS=>HYDIBSDAT(IGRID)%IBHYDIBS
      INTRPHYDIBS=>HYDIBSDAT(IGRID)%INTRPHYDIBS
      JIKHYDIBS=>HYDIBSDAT(IGRID)%JIKHYDIBS
      HYDIBSWT=>HYDIBSDAT(IGRID)%HYDIBSWT
      HYDIBSARR=>HYDIBSDAT(IGRID)%HYDIBSARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7PSV(IGRID)
C  Save HYD IBS data for a grid.
      USE HYDIBSMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      HYDIBSDAT(IGRID)%NHYDIBS=>NHYDIBS
      HYDIBSDAT(IGRID)%IBHYDIBS=>IBHYDIBS
      HYDIBSDAT(IGRID)%INTRPHYDIBS=>INTRPHYDIBS
      HYDIBSDAT(IGRID)%JIKHYDIBS=>JIKHYDIBS
      HYDIBSDAT(IGRID)%HYDIBSWT=>HYDIBSWT
      HYDIBSDAT(IGRID)%HYDIBSARR=>HYDIBSARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7DA(IGRID)
C  Deallocate HYD SUB memory
      USE HYDSUBMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      INTEGER:: IERR
C
      DEALLOCATE(HYDSUBDAT(IGRID)%NHYDSUB,     STAT = IERR)
      DEALLOCATE(HYDSUBDAT(IGRID)%IBHYDSUB,    STAT = IERR)
      DEALLOCATE(HYDSUBDAT(IGRID)%INTRPHYDSUB, STAT = IERR)
      DEALLOCATE(HYDSUBDAT(IGRID)%HYDSUBARR,   STAT = IERR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7PNT(IGRID)
C  Change HYD SUB data to a different grid.
      USE HYDSUBMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      NHYDSUB=>HYDSUBDAT(IGRID)%NHYDSUB
      IBHYDSUB=>HYDSUBDAT(IGRID)%IBHYDSUB
      INTRPHYDSUB=>HYDSUBDAT(IGRID)%INTRPHYDSUB
      HYDSUBARR=>HYDSUBDAT(IGRID)%HYDSUBARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7PSV(IGRID)
C  Save HYD SUB data for a grid.
      USE HYDSUBMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      HYDSUBDAT(IGRID)%NHYDSUB=>NHYDSUB
      HYDSUBDAT(IGRID)%IBHYDSUB=>IBHYDSUB
      HYDSUBDAT(IGRID)%INTRPHYDSUB=>INTRPHYDSUB
      HYDSUBDAT(IGRID)%HYDSUBARR=>HYDSUBARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SWT7DA(IGRID)
C  Deallocate HYD SWT memory
      USE HYDSWTMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      INTEGER:: IERR
C
      DEALLOCATE(HYDSWTDAT(IGRID)%NHYDSWT,     STAT = IERR)
      DEALLOCATE(HYDSWTDAT(IGRID)%IBHYDSWT,    STAT = IERR)
      DEALLOCATE(HYDSWTDAT(IGRID)%INTRPHYDSWT, STAT = IERR)
      DEALLOCATE(HYDSWTDAT(IGRID)%HYDSWTARR,   STAT = IERR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SWT7PNT(IGRID)
C  Change HYD SWT data to a different grid.
      USE HYDSWTMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      NHYDSWT=>HYDSWTDAT(IGRID)%NHYDSWT
      IBHYDSWT=>HYDSWTDAT(IGRID)%IBHYDSWT
      INTRPHYDSWT=>HYDSWTDAT(IGRID)%INTRPHYDSWT
      HYDSWTARR=>HYDSWTDAT(IGRID)%HYDSWTARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SWT7PSV(IGRID)
C  Save HYD SWT data for a grid.
      USE HYDSWTMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      HYDSWTDAT(IGRID)%NHYDSWT=>NHYDSWT
      HYDSWTDAT(IGRID)%IBHYDSWT=>IBHYDSWT
      HYDSWTDAT(IGRID)%INTRPHYDSWT=>INTRPHYDSWT
      HYDSWTDAT(IGRID)%HYDSWTARR=>HYDSWTARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7DA(IGRID)
C  Deallocate HYD STR memory
      USE HYDSTRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      DEALLOCATE(HYDSTRDAT(IGRID)%NHYDSTR)
      DEALLOCATE(HYDSTRDAT(IGRID)%ISTRHYD)
      DEALLOCATE(HYDSTRDAT(IGRID)%HYDSTRARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7PNT(IGRID)
C  Change HYD STR data to a different grid.
      USE HYDSTRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      NHYDSTR=>HYDSTRDAT(IGRID)%NHYDSTR
      ISTRHYD=>HYDSTRDAT(IGRID)%ISTRHYD
      HYDSTRARR=>HYDSTRDAT(IGRID)%HYDSTRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7PSV(IGRID)
C  Save HYD STR data for a grid.
      USE HYDSTRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      HYDSTRDAT(IGRID)%NHYDSTR=>NHYDSTR
      HYDSTRDAT(IGRID)%ISTRHYD=>ISTRHYD
      HYDSTRDAT(IGRID)%HYDSTRARR=>HYDSTRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7DA(IGRID)
C  Deallocate HYD SFR memory
      USE HYDSFRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      INTEGER IERR
C
      DEALLOCATE(HYDSFRDAT(IGRID)%NHYDSFR,   STAT=IERR)
      DEALLOCATE(HYDSFRDAT(IGRID)%ISFRHYD,   STAT=IERR)
      DEALLOCATE(HYDSFRDAT(IGRID)%HYDSFRARR,   STAT=IERR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7PNT(IGRID)
C  Change HYD SFR data to a different grid.
      USE HYDSFRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      NHYDSFR=>HYDSFRDAT(IGRID)%NHYDSFR
      ISFRHYD=>HYDSFRDAT(IGRID)%ISFRHYD
      HYDSFRARR=>HYDSFRDAT(IGRID)%HYDSFRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7PSV(IGRID)
C  Save HYD SFR data for a grid.
      USE HYDSFRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
C
      HYDSFRDAT(IGRID)%NHYDSFR=>NHYDSFR
      HYDSFRDAT(IGRID)%ISFRHYD=>ISFRHYD
      HYDSFRDAT(IGRID)%HYDSFRARR=>HYDSFRARR
C
      RETURN
      END
