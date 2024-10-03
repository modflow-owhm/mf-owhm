      MODULE GWFDRTMODULE
        USE LINE_FEEDER,                     ONLY: LINE_FEED
        USE BUDGET_GROUP_INTERFACE,          ONLY: BUDGET_GROUP
        USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
        PRIVATE:: LINE_FEED, BUDGET_GROUP, 
     +            GENERIC_OUTPUT_FILE
        !
        LOGICAL,SAVE, POINTER:: DRT_AUTO_NEG_ITMP
        LOGICAL,SAVE, POINTER:: HAS_FMP, HAS_SFR, HAS_SWR
        INTEGER,SAVE,POINTER:: IOUT, LOUT
        INTEGER,SAVE,POINTER   ::NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB
        INTEGER,SAVE,POINTER   ::NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT
        TYPE(GENERIC_OUTPUT_FILE),SAVE,POINTER::PRTFIL, DRTDB
        !INTEGER,SAVE,POINTER   ::PRTFIL                                 !seb FLAG TO PRINT TO EXTERNAL FILE
        !TYPE(LINE_FEED),SAVE, POINTER::DRTFEED                          !seb
        REAL,         SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS::DRTF
        CHARACTER(16),SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::DRTAUX
        CHARACTER(16),SAVE ,DIMENSION(:),   POINTER,CONTIGUOUS::DRTGRP
        INTEGER      ,SAVE ,DIMENSION(:),   POINTER,CONTIGUOUS::GRP_RTN  !COUNTER FOR RETURN FLOW BUDGETS
        TYPE(BUDGET_GROUP),SAVE,            POINTER:: DRTBUD
      TYPE GWFDRTTYPE
        LOGICAL, POINTER:: DRT_AUTO_NEG_ITMP
        LOGICAL, POINTER:: HAS_FMP, HAS_SFR, HAS_SWR
        INTEGER, POINTER   ::NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB
        INTEGER, POINTER   ::NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT
        TYPE(GENERIC_OUTPUT_FILE),POINTER::PRTFIL, DRTDB
        !INTEGER,POINTER    ::PRTFIL                                     !seb FLAG TO PRINT TO EXTERNAL FILE
        !TYPE(LINE_FEED), POINTER::DRTFEED                               !seb
        REAL,         DIMENSION(:,:), POINTER,CONTIGUOUS::DRTF
        CHARACTER(16),DIMENSION(:),   POINTER,CONTIGUOUS::DRTAUX
        CHARACTER(16),DIMENSION(:),   POINTER,CONTIGUOUS::DRTGRP
        INTEGER      ,DIMENSION(:),   POINTER,CONTIGUOUS::GRP_RTN
        TYPE(BUDGET_GROUP),           POINTER:: DRTBUD
      END TYPE
      TYPE(GWFDRTTYPE), SAVE :: GWFDRTDAT(10)
      END MODULE GWFDRTMODULE
      !
      !
      SUBROUTINE GWF2DRT7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE AND READ PARAMETERS FOR DRAINS AND
C     RETURN FLOWS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,    ONLY: Z, ONE, TRUE, FALSE, NL, BLN
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNIT     !seb ADDED IUNIT TO CHECK FOR FMP, SWR, and SFR PACKAGES BEING AVAILIBLE
      USE GWFDRTMODULE, ONLY:NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB,NPDRT,
     1                       IDRTPB,IDRTFL,NRFLOW,NOPRDT,DRTF,DRTAUX,
     2                       PRTFIL,DRTBUD,DRTGRP,GRP_RTN,DRTDB,
     3                       DRT_AUTO_NEG_ITMP,HAS_FMP, HAS_SFR, HAS_SWR
      USE LINE_FEEDER,          ONLY: LINE_FEED
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD_UP
      USE STRINGS,              ONLY: GET_INTEGER
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
      TYPE(GENERIC_BLOCK_READER):: BL
      !CHARACTER(768):: LINE
C     ------------------------------------------------------------------
      INTEGER:: IUNITFMP,IUNITSWR,IUNITSFR                              !seb ADDED INUIT VARIABLES FOR FMP AND SWR LINK
      LOGICAL:: FOUND_BEGIN
      
      ALLOCATE(DRT_AUTO_NEG_ITMP)
      ALLOCATE(HAS_FMP, HAS_SFR, HAS_SWR)
      ALLOCATE(NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB)
      ALLOCATE(NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT)
      ALLOCATE(PRTFIL,DRTBUD, DRTDB)
C
C1------IDENTIFY PACKAGE AND INITIALIZE NDRTCL.
        WRITE(IOUT,1)IN
    1 FORMAT(1X,/
     &1X,'DRT7 -- DRAIN RETURN PACKAGE, VERSION OWHM',/,
     &' INPUT READ FROM UNIT ',I4)
      NDRTCL = Z
      NDRTNP = Z
      IDRTFL = Z
      NAUX   = Z
      NOPRDT = Z
      DRT_AUTO_NEG_ITMP = FALSE
      HAS_SFR = IUNIT(44) /= Z
      HAS_FMP = IUNIT(61) /= Z
      HAS_SWR = IUNIT(64) /= Z
C
C2------READ MAXIMUM NUMBER OF DRAINS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
C     READ COMMENTS (ITEM 0)
      CALL BL%LOAD(IN,IOUT,FOUND_BEGIN=FOUND_BEGIN, NO_BACKSPACE=TRUE)
      !CALL READ_TO_DATA(LINE,IN,IOUT,IOUT)
      !
      CALL DRTBUD%INIT('DRAINS_DRT')
      !
      DO WHILE (FOUND_BEGIN)!BLOCK GROUPS
       !
       IF(BL%NAME == 'BUDGET_GROUP'.OR.BL%NAME == 'BUDGET_GROUPS') THEN
         CALL DRTBUD%LOAD(BL)
       !
       ELSEIF(BL%NAME == 'OPTION' .OR. BL%NAME == 'OPTIONS' ) THEN
         !
         WRITE(IOUT,'(/1X,A)') 'PROCESSING DRT OPTIONS'
         !
         CALL BL%START()
         DO I=1, BL%NLINE
         LLOC = 1
         CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
         !
         SELECT CASE (BL%LINE(ISTART:ISTOP))
         CASE('RETURNFLOW','RETURN')
            IDRTFL=4
            WRITE(IOUT,13)
         CASE('AUTOMATIC_NEGATIVE_ITMP')
            WRITE(IOUT,'(A)')"AUTOMATIC_NEGATIVE_ITMP option enabled"
            DRT_AUTO_NEG_ITMP = .TRUE.
         CASE('NOPRINT', 'NO_PRINT')
            WRITE(IOUT,14)
            NOPRDT = 1
         CASE('AUXILIARY','AUX')
            CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
            IF (NAUX.LT.20) THEN
              NAUX=NAUX+1
              DRTAUX(NAUX)=BL%LINE(ISTART:ISTOP)
                WRITE(IOUT,12) DRTAUX(NAUX)
            ELSE
             WRITE(IOUT,*)'DRT WARNING: MAX AUX VARIABLES ALLOWED IS 20'
            END IF
            !
         CASE('PRINTFILE')                    !seb ADDED FEATURE TO PRINT FLOW RATES TO A SEPARATE FILE
          CALL PRTFIL%OPEN(BL%LINE,LLOC,IOUT,IN,NO_INTERNAL=TRUE, 
     +                     ALLOW_ONLY_UNIT=.TRUE.)
          WRITE(IOUT,15) PRTFIL%IU
         CASE('DBFILE') !ADD KEYWORD TO WRITE ONCE
            WRITE(IOUT,715)
  715     FORMAT(' DRT INFORMATION WRITTEN TO DATABASE FRIENDLY OUTPUT')
            CALL DRTDB%OPEN(BL%LINE,LLOC,IOUT,IN, NO_INTERNAL=.TRUE.,
     +                      ALLOW_ONLY_UNIT=.TRUE.)      
            IF(DRTDB%BINARY) THEN !IF FILE NOT OPEN THEN NO HEADER IS WRITTEN AND ONLY WRITE HEADER IF NOT BINARY.
                WRITE(IOUT,'(*(A))')'DRT DATABASE FRIENDLY OUTPUT ',
     +          'WRITTEN TO BINARY FILE USING STREAM UNFORMATTED ',
     +          'STRUCTURE.',NL,
     +         'EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',
     +           NL,'"DATE_START (19char), ',
     +          'STRESS PERIOD (int), TIME STEP (int), ',
     +          'TIME STEP LENGTH (double),  SIMULATED TIME (double), ',
     +          'LAY (int), ROW (int), COL (int), ',
     +          'DRT CONDUCTANCE (double), DRT ELEVATION (double), ',
     +          'GROUNDWATER HEAD (double), DRT FLOW RATE (double), ', 
     +          'DRT BUDGET GROUP (16char), RETURN_LOCATION (5char), ',
     +          'RETURN_LAY (int), RETURN_ROW (int), RETURN_COL (int)'
            ELSE
               CALL DRTDB%SET_HEADER( ' DATE_START               '//
     +         'PER     STP             DELT          '//
     +         'SIMTIME    LAY    ROW    COL  DRT_CONDUCTANCE    '//
     +    'DRT_ELEVATION             HEAD    DRT_FLOW_RATE'//
     +    '   DRT_BUD_GROUP RET_PAK'//
     +    '  RET_LAY  RET_ROW  RET_COL' )
            END IF
         !CASE('GROUP_OUTPUT') !ADD KEYWORD TO WRITE ONCE
         !   IGRP_OUT = IGRP_OUT + ONE
         CASE DEFAULT
             ! -- NO OPTIONS FOUND
             WRITE(IOUT,'(/2A,A/)')
     +              'DRT WARNING: FAILED TO IDENTIFY OPTION: ',
     +               BL%LINE(ISTART:ISTOP),
     +              'THIS OPTION IS IGNORED AND NOT APPLIED.'
         END SELECT
         CALL BL%NEXT()
         END DO
         !
       ELSE
           CALL STOP_ERROR(TRIM(BL%LINE),IN,IOUT,
     +   'DRT BLOCK ERROR. FOUND "BEGIN" KEYWORD, BUT IT WAS '//
     +   ' NOT FOLLOWED BY A KNOWN BLOCK NAME.'//BLN//
     +   'THE FOLLOWING ARE ACCEPTED BLOCK NAMES: '//
     +   '"BUDGET_GROUP", "BUDGET_GROUPS", "OPTION", and "OPTIONS"' )
       END IF
       !
       CALL BL%LOAD(IN,IOUT,FOUND_BEGIN=FOUND_BEGIN, NO_BACKSPACE=TRUE)
       !
      END DO  ! WHILE (FOUND_BEGIN) BLOCK GROUPS 
      !
      !ALLOCATE DRTFEED VARIABLE AND OPTIONALLY READ IN FEED FILE LOCATIONS
      !DRTFEED => LINE_FEED(IN,IOUT,LINE)    !=>FEED_ALLOCATE(IN,IOUT,LINE)     
      !
C     READ ITEM 1
      N = Z
      IF ( IFREFM == Z ) THEN
           READ(BL%LN,'(4I10)', IOSTAT=N) MXADRT,IDRTCB,NPDRT,MXL
           LLOC=41
      END IF
      !
      IF ( IFREFM /= Z .OR. N /= Z ) THEN
           LLOC=1
           CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,MXADRT,
     +                                 MSG='DRT FAILED TO READ MXADRT')
           CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IDRTCB,
     +                                 MSG='DRT FAILED TO READ IDRTCB')
           CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,NPDRT,
     +                                 MSG='DRT FAILED TO READ NPDRT')
           CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,MXL,
     +                                 MSG='DRT FAILED TO READ MXL')
           IF (IFREFM == Z) LLOC=41
      ENDIF
      !
      ! CHECK IF GLOBAL SHUTDOWN OF CBC IS IN EFFECT
      CALL CHECK_CBC_GLOBAL_UNIT(IDRTCB, .FALSE.)
      !
C
      IF (NPDRT > Z) THEN
          WRITE(IOUT,9) NPDRT,MXL
    9   FORMAT(1X,I5,' Named Parameters     ',I5,' List entries')
      ELSE
          WRITE(IOUT,'(A)') ' No named parameters'
      END IF

C3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      ALLOCATE (DRTAUX(20))
   10 CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(BL%LN(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     &       BL%LN(ISTART:ISTOP).EQ.'AUX') THEN
        CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF (NAUX.LT.20) THEN
          NAUX=NAUX+1
          DRTAUX(NAUX)=BL%LN(ISTART:ISTOP)
            WRITE(IOUT,12) DRTAUX(NAUX)
   12     FORMAT(1X,'AUXILIARY DRAIN-RETURN FLOW VARIABLE: ',A)
        ENDIF
        GOTO 10
      ELSEIF (BL%LN(ISTART:ISTOP).EQ.'RETURNFLOW' .OR.
     +        BL%LN(ISTART:ISTOP).EQ.'RETURN') THEN
        IDRTFL=4
          WRITE(IOUT,13)
        GOTO 10
   13   FORMAT(1X,'RETURN FLOW OPTION IS SELECTED')
      ELSE IF(BL%LN(ISTART:ISTOP).EQ.'NOPRINT' .OR.
     +        BL%LN(ISTART:ISTOP).EQ.'NO_PRINT') THEN
           WRITE(IOUT,14)
   14    FORMAT(1X,'LISTS OF DRAIN-RETURN CELLS WILL NOT BE PRINTED')
         NOPRDT = 1
         GO TO 10
      ELSE IF(BL%LN(ISTART:ISTOP).EQ.'PRINTFILE') THEN                   !seb ADDED FEATURE TO PRINT FLOW RATES TO A SEPARATE FILE
         CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,0,I,R,IOUT,IN)
         I=1
       CALL PRTFIL%OPEN(BL%LN(ISTART:ISTOP),I,IOUT,IN,NO_INTERNAL=TRUE, 
     +                  ALLOW_ONLY_UNIT=.TRUE.)
         WRITE(IOUT,15) PRTFIL%IU
   15    FORMAT(1X, 'LISTS OF DRAIN-RETURN CELLS WILL BE PRINTED TO ',
     +              'EXTERNAL FILE ON UNIT NUMBER: ',I6)
         GO TO 10
      ELSE IF(BL%LN(ISTART:ISTOP).EQ.'AUTOMATIC_NEGATIVE_ITMP') THEN
         WRITE(IOUT,'(A)')"AUTOMATIC_NEGATIVE_ITMP option enabled"
         DRT_AUTO_NEG_ITMP = .TRUE.
         GO TO 10
      ENDIF
      NDRTVL=5+NAUX+2+IDRTFL
      !
      ! LOAD MODEL CELLS THAT WILL BE DESCRIBED BY THE LINE FEED
      !IF(IDRTFL==0) THEN
      !   CALL DRTFEED%CELLS(3, 1, NAUX, NOPRDT)     !=>FEED_CELLS(LDIM,NPROP,NAUX,IPRT)
      !ELSE
      !   CALL DRTFEED%CELLS(6, 2, NAUX, NOPRDT)     !=>FEED_CELLS(LDIM,NPROP,NAUX,IPRT)
      !END IF
      !
      ! ADD FEED COUNT TO MXADRT
      !MXADRT = MXADRT + DRTFEED%TOTDAT
      !
        WRITE(IOUT,3) MXADRT
    3 FORMAT(1X,'MAXIMUM OF ',I6,
     &' ACTIVE DRAINS WITH RETURN FLOW AT ONE TIME')
        IF (IDRTCB < Z) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
        IF (IDRTCB > Z) WRITE(IOUT,8) IDRTCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
      !
C
C4------ALLOCATE SPACE FOR THE DRTF ARRAY.
      IDRTPB = MXADRT + ONE
      MXDRT  = MXADRT + MXL
      ALLOCATE (DRTF(NDRTVL,MXDRT))
      IF (DRTBUD%BUDGET_GROUPS) THEN
          ALLOCATE(DRTGRP(MXDRT))
          ALLOCATE(GRP_RTN(DRTBUD%NGRP))  !Additional Flow terms cause drain has return flow
      ELSE
          ALLOCATE(DRTGRP(1))
          ALLOCATE(GRP_RTN(1))
      END IF
      DRTF = 0.0
C
C5------READ NAMED PARAMETERS.
        WRITE(IOUT,500) NPDRT
  500 FORMAT(1X,//1X,I5,' Drain-return parameters')
      IF (NPDRT > Z) THEN
        NAUX   = NDRTVL-5-2-IDRTFL
        LSTSUM = IDRTPB
        ITERPU = ONE
        IF (NOPRDT == ONE) ITERPU = 99
        DO 100 K=1,NPDRT
          LSTBEG=LSTSUM
C5A-----READ ITEM 2
          CALL UPARLSTRP(LSTSUM,MXDRT,IN,IOUT,IP,'DRT','DRT',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST > 1) NLST = NLST/NUMINST
C5B-----ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST == Z) THEN
            IB = Z
          ELSE
            IB = ONE
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 50 I=IB,NUMINST
            IF (I > Z) THEN
              CALL UINSRP(I,IN,IOUT,IP,1)
            ENDIF
C5C-----READ ITEM 3
            CALL SGWF2DRT7LR(NLST,DRTF,LB,NDRTVL,MXDRT,IN,IOUT,
     &                       DRTAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,ITERPU,
     &                       IDRTFL,DRTBUD%BUDGET_GROUPS,DRTGRP)
            LB = LB+NLST
   50     CONTINUE
  100   CONTINUE
        IF(ANY(DRTF(6,IDRTPB:) < -0.1))THEN                             !seb CHECK TO SEE IF LINKED PACKAGES ARE ACTIVE OR FLAG AS ERROR
      DO I=IDRTPB,LSTSUM-1
       KR=DRTF(6,I)
       IF (    KR.EQ.-1 .AND. .NOT. HAS_FMP)THEN
        CALL USTOP('ERROR DRT LINK TO FMP REQUIRES FMP TO BE ACTIVE')
       ELSEIF (KR.EQ.-2 .AND. .NOT. HAS_SWR)THEN
        CALL USTOP('ERROR DRT LINK TO SWR REQUIRES SWR TO BE ACTIVE')
       ELSEIF (KR.EQ.-3 .AND. .NOT. HAS_SFR)THEN
        CALL USTOP('ERROR DRT LINK TO SFR REQUIRES SFR TO BE ACTIVE')
       END IF
      END DO
        END IF
      ENDIF
      !
      CALL SGWF2DRT7PSV(IGRID)
      !
      END SUBROUTINE
C-------SUBROUTINE GWF2DRT7AD
      SUBROUTINE GWF2DRT7AD(KSTP,Igrid)
C     *****************************************************************
C     APPLY EFFECTS OF SUBSIDENCE ON DRAIN FLOWS
C     VERSION  7.2.00: July 17, 2012 rth
C     *****************************************************************
      USE GWFDRTMODULE, ONLY:NDRTCL,DRTF,IDRTFL
      USE GLOBAL,       ONLY: IBOUND,SUBLNK
      USE GWFSUBMODULE, ONLY: DVZ,LPFLNK,DVZC
      IMPLICIT NONE
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER KSTP,Igrid,L,IL,IR,IC
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
C
      IF(SUBLNK) THEN
        DO 100 L=1,NDRTCL
C       
C1--------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
          IL=DRTF(1,L)
          IR=DRTF(2,L)
          IC=DRTF(3,L)
C       
C2---------IF THE CELL IS EXTERNAL SKIP IT.
          IF (IBOUND(IC,IR,IL).LE.0) GOTO 100
C       
C3---------IF THE CELL IS INTERNAL GET THE DRAIN DATA and subtract the displacement from that cell.
          DRTF(4,L)=DRTF(4,L) - DVZ(IC,IR,IL)
100     CONTINUE 
      END IF
      RETURN
      END SUBROUTINE
C--------------------------------------------------------------------------
      SUBROUTINE GWF2DRT7RP(IN,IGRID)
C     ******************************************************************
C     READ DRAIN HEAD, CONDUCTANCE AND BOTTOM ELEVATION.  IF THE
C     RETURNFLOW OPTION IS SELECTED, READ RECIPIENT CELL AND PROPORTION.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IBOUND
      USE GWFDRTMODULE, ONLY:NDRTCL,MXDRT,NDRTVL,NDRTNP,NPDRT,
     1                       IDRTPB,IDRTFL,NRFLOW,NOPRDT,DRTF,DRTAUX,
     2                       DRTBUD,DRTGRP,GRP_RTN,
     3                       DRT_AUTO_NEG_ITMP,HAS_FMP,HAS_SFR,HAS_SWR
      USE FMP_GLOBAL,        ONLY: WBS, DRTFLOW, FMP_LGR_PNT
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER
      USE CONSTANTS,         ONLY: NEG, Z, ONE, NL, TRUE
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE ERROR_INTERFACE,   ONLY: WARNING_MESSAGE
C     ------------------------------------------------------------------
      LOGICAL:: FMPLINK
      CHARACTER(96):: LINE
      INTEGER:: LLOC, ISTART, ISTOP

      CALL SGWF2DRT7PNT(IGRID)
      CALL FMP_LGR_PNT(IGRID)
      !
      IF(DRT_AUTO_NEG_ITMP) THEN
          ITMP = NEG
          NP   = Z
      ELSE
C
C1------READ ITMP (NUMBER OF DRAINS OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      CALL READ_TO_DATA(LINE,IN,IOUT, NOSHIFT=TRUE)
      !
      LLOC = ONE
      IF (IFREFM == Z) THEN
        CALL GET_INTEGER(LINE(1:10),LLOC,ISTART,ISTOP,IOUT,IN,ITMP,
     +                   MSG='DRT ERROR READING ITMP FOR STRESS PERIOD')
      ELSE
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,ITMP,
     +                   MSG='DRT ERROR READING ITMP FOR STRESS PERIOD')
      ENDIF
      !
      IF (NPDRT > Z) THEN
        IF (IFREFM == Z) THEN
          CALL GET_INTEGER(LINE(11:20),LLOC,ISTART,ISTOP,IOUT,IN,NP,
     +                     MSG='DRT ERROR READING NP FOR STRESS PERIOD')
        ELSE
          CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,NP,
     +                     MSG='DRT ERROR READING NP FOR STRESS PERIOD')
        ENDIF
      ELSE
          NP = Z
      END IF
      END IF !(DRT_AUTO_NEG_ITMP) 
      !IF (NPDRT > Z) THEN
      !  IF (IFREFM == Z) THEN
      !    READ(IN,'(2I10)') ITMP,NP
      !  ELSE
      !    READ(IN,*) ITMP,NP
      !  ENDIF
      !ELSE
      !  NP=0
      !  IF (IFREFM == Z) THEN
      !    READ(IN,'(I10)') ITMP
      !  ELSE
      !    READ(IN,*) ITMP
      !  ENDIF
      !ENDIF
C
C------CALCULATE SOME CONSTANTS
      NAUX=NDRTVL-5-2-IDRTFL
      ITERPU = ONE
      IPRT   = ONE
      IOUTU  = IOUT
      IF (NOPRDT.EQ.1) THEN
        ITERPU = 99
        !IOUTU = -IOUT
        IPRT = Z
      ENDIF
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER DRAIN-RETURN CELLS.
      IF (ITMP < Z) THEN
          WRITE(IOUT,7)
    7   FORMAT(1X,/,
     &' REUSING NON-PARAMETER DRAIN-RETURN CELLS FROM',
     &' LAST STRESS PERIOD')
      ELSE
        NDRTNP=ITMP
      ENDIF
C
C3------IF THERE ARE NEW NON-PARAMETER DRAIN-RETURN CELLS, READ THEM.
      MXADRT=IDRTPB-1
      IF (ITMP > Z) THEN
        IF (NDRTNP > MXADRT) THEN
          CALL USTOP('THE NUMBER OF ACTIVE DRT DRAINS ('//
     +    NUM2STR(NDRTNP)//') IS GREATER THAN MXADRT('//
     +    NUM2STR(MXADRT)//')'//NL//'PLEASE INCREASE THE SIZE OF MXADRT'
     +              )
        ENDIF
        CALL SGWF2DRT7LR(NDRTNP,DRTF,1,NDRTVL,MXDRT,IN,IOUT,
     &                   DRTAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,ITERPU,
     &                   IDRTFL,DRTBUD%BUDGET_GROUPS,DRTGRP)
      ENDIF
      NDRTCL=NDRTNP
C
C1C-----IF THERE ARE ACTIVE DRT PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('DRT')
      IF (NP > Z) THEN
        NREAD=NDRTVL-2
        DO 30 N=1,NP
          CALL SGWF2DRT7LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,MXADRT,
     +                     NDRTCL,DRTAUX,20,NAUX,IDRTFL,IPRT,
     +                     DRTBUD%BUDGET_GROUPS,DRTGRP)
   30   CONTINUE
      ENDIF
C
C     COUNT NUMBER OF DRAIN-RETURN CELLS THAT CAN HAVE RETURN FLOW
      NRFLOW = Z
      IF (IDRTFL > Z) THEN
        DO 40 I=1,NDRTCL
          IF (DRTF(6,I) > 0.1) NRFLOW = NRFLOW + ONE                   !>0 indicates drain return flow to groundwater
   40   CONTINUE
      ENDIF
C
C3------PRINT NUMBER OF DRAIN-RETURN CELLS IN CURRENT STRESS PERIOD.
        WRITE (IOUT,510) NDRTCL
  510 FORMAT(1X,/1X,I6,' DRAIN-RETURN CELLS')
C
Cseb CHECK TO SEE IF LINKED PACKAGES ARE ACTIVE OR FLAG AS ERROR
      IF (IDRTFL > Z) THEN
       !!!FMPLINK = ANY(NINT(DRTF(6,:)) == NEG)  -> Compiler error
       !!!IF(FMPLINK.AND..NOT. HAS_FMP)THEN
       !!!  CALL USTOP('ERROR DRT LINK TO FMP REQUIRES FMP TO BE ACTIVE')
       !!!END IF
       !!!IF(ANY(NINT(DRTF(6,:)).EQ.-2).AND..NOT. HAS_SWR)THEN
       !!!  CALL USTOP('ERROR DRT LINK TO SWR REQUIRES SWR TO BE ACTIVE')
       !!!END IF
       !!!IF(ANY(NINT(DRTF(6,:)).EQ.-3).AND..NOT. HAS_SFR)THEN
       !!! CALL USTOP('ERROR DRT LINK TO SFR REQUIRES SFR TO BE ACTIVE')
       !!!END IF
       FMPLINK = ANY_DRT6_CHECK(NDRTVL,MXDRT,DRTF,NEG)
       IF(FMPLINK.AND..NOT. HAS_FMP)THEN
         CALL USTOP('ERROR DRT LINK TO FMP REQUIRES FMP TO BE ACTIVE')
       END IF
       IF(ANY_DRT6_CHECK(NDRTVL,MXDRT,DRTF,-2).AND..NOT. HAS_SWR) THEN
         CALL USTOP('ERROR DRT LINK TO SWR REQUIRES SWR TO BE ACTIVE')
       END IF
       IF(ANY_DRT6_CHECK(NDRTVL,MXDRT,DRTF,-3).AND..NOT. HAS_SFR) THEN
        CALL USTOP('ERROR DRT LINK TO SFR REQUIRES SFR TO BE ACTIVE')
       END IF
       !
       IF (FMPLINK) CALL GWF2DRT7_FMPLINK(NDRTCL,NDRTVL,MXDRT,DRTF,
     +                            NCOL,NROW,NLAY,IBOUND,IN,IOUT)
!!!       DRTFLOW%HAS_FLO=.FALSE.
!!!       DO I=1, NDRTCL
!!!        IF (NINT(DRTF(6,I)) == NEG) THEN
!!!          IL=NINT(DRTF(1,I))
!!!          IR=NINT(DRTF(2,I))
!!!          IC=NINT(DRTF(3,I))
!!!          NF=NINT(DRTF(7,I))
!!!          IF(NF == Z) NF=WBS%FID_ARRAY(IC,IR)          ! ROW = 0 SO USE OVER LYING FARM 
!!!          IF(NF > Z .AND. NF <= WBS%NFARM)THEN         ! FARM LINK IS SPECIFIED IN IRR
!!!                                          DRTFLOW(NF)%HAS_FLO=.TRUE.
!!!                                          DRTF(8,I) = REAL(NF)
!!!          ELSEIF( IBOUND(IC,IR,IL) > Z ) THEN
!!!                  ! 
!!!                  CALL WARNING_MESSAGE(INFILE=IN,OUTPUT=IOUT,MSG=
!!!     +       'WARNING DRAIN LOCATED AT (LAY, ROW, COL) = ('//
!!!     +       NUM2STR(IL)//', '//NUM2STR(IR)//', '//NUM2STR(IC)//
!!!     +       ') CAN NOT ROUTE WATER TO OVERLAYING WBS (FARM) '//
!!!     +       'BECAUSE A WBS ID > 0 IS NOT LOCATED ABOVE DRAIN'//NL//
!!!     +       'ANY DRT FLOW AT THIS LOCATION IS SENT OUT OF THE '//
!!!     +       'MODELED SYSTEM (eg NOT ROUTED TO FMP)'//NL//
!!!     +       'FMP LINK FOR THIS DRT CELL WILL BE DISABLED.',INLINE=TRUE)
!!!                  DRTF(6,I) = 0.0
!!!                  CYCLE
!!!          ELSE 
!!!                  DRTF(6,I) = 0.0
!!!          END IF
!!!        END IF
!!!       END DO
!!!       END IF
      END IF
      !
      GRP_RTN = Z
      CALL DRTBUD%RESET()
      !
      IF (NDRTCL > Z) THEN
         IF (DRTBUD%BUDGET_GROUPS) THEN
             CALL DRTBUD%ADD( DRTGRP(1:NDRTCL) )
             !
             !IF (IDRTFL > Z) THEN
             !   DO CONCURRENT (I=1:NDRTCL, DRTF(6,I)  >  0.1)  !>0 indicates drain return flow to groundwater
             !        IG = DRTBUD%GROUPNUM(DRTGRP(I))
             !        GRP_RTN(IG) = GRP_RTN(IG) + ONE
             !   END DO
             !ENDIF
             IF (IDRTFL > Z) THEN
                DO I=1, NDRTCL
                   IF(DRTF(6,I) > 0.1)  THEN  !>0 indicates drain return flow to groundwater
                     IG = DRTBUD%GROUPNUM(DRTGRP(I))
                     GRP_RTN(IG) = GRP_RTN(IG) + ONE
                   END IF
                END DO
             ENDIF
         ELSE
             I = -1*NDRTCL
             CALL DRTBUD%ADD( I )  !Neg value indicates only one group and it is the size of NDRTCL
             !
             IF (IDRTFL > Z) THEN
                 DO CONCURRENT (I=1:NDRTCL, DRTF(6,I) > 0.1) !>0 indicates drain return flow to groundwater
                      GRP_RTN(1) = GRP_RTN(1) + ONE
                 END DO
             END IF
         END IF
      END IF
      !
C8------RETURN.
      RETURN
      !
      CONTAINS
        !
        PURE FUNCTION ANY_DRT6_CHECK(NDRTVL,MXDRT,DRTF,IFLAG)RESULT(ANS)
          ! Replaces ANY(NINT(DRTF(6,:)) == NEG) cuz of compiler error
          INTEGER,                       INTENT(IN) :: NDRTVL, MXDRT
          REAL, DIMENSION(NDRTVL,MXDRT), INTENT(IN) :: DRTF
          INTEGER,                       INTENT(IN) :: IFLAG
          LOGICAL :: ANS
          INTEGER :: I
          !
          DO I=1, MXDRT
             IF(NINT(DRTF(6,I)) == IFLAG) THEN
                ANS = .TRUE.
                RETURN
             END IF
          END DO
          ANS = .FALSE.
        END FUNCTION
      END
C
      SUBROUTINE GWF2DRT7_FMPLINK(NDRTCL,NDRTVL,MXDRT,DRTF,
     +                            NCOL,NROW,NLAY,IBOUND,IN,IOUT)
      USE CONSTANTS,         ONLY: Z, ONE, NEG, TRUE, FALSE, NL
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE ERROR_INTERFACE,   ONLY: WARNING_MESSAGE
      USE FMP_GLOBAL,        ONLY: WBS, DRTFLOW
      IMPLICIT NONE
      INTEGER,                            INTENT(IN   ):: NDRTCL
      INTEGER,                            INTENT(IN   ):: NDRTVL,MXDRT
      REAL, DIMENSION(NDRTVL,MXDRT),      INTENT(INOUT):: DRTF
      INTEGER,                            INTENT(IN   ):: NCOL,NROW,NLAY
      INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   ):: IBOUND
      INTEGER,                            INTENT(IN   ):: IN,IOUT
      INTEGER:: I, IL, IR, IC, NF
      !
      DRTFLOW%HAS_FLO = FALSE
      DO I=1, NDRTCL
         IF (NINT(DRTF(6,I)) == NEG) THEN
            IL=NINT(DRTF(1,I))
            IR=NINT(DRTF(2,I))
            IC=NINT(DRTF(3,I))
            NF=NINT(DRTF(7,I))
            IF(NF == Z) NF=WBS%FID_ARRAY(IC,IR)          ! ROW = 0 SO USE OVER LYING FARM 
            IF(NF > Z .AND. NF <= WBS%NFARM) THEN        ! FARM LINK IS SPECIFIED IN IRR
                                            DRTFLOW(NF)%HAS_FLO= TRUE 
                                            DRTF(8,I) = REAL(NF)
            ELSEIF( IBOUND(IC,IR,IL) > Z ) THEN
                    ! 
                    CALL WARNING_MESSAGE(INFILE=IN,OUTPUT=IOUT,MSG=
     +         'WARNING DRAIN LOCATED AT (LAY, ROW, COL) = ('//
     +         NUM2STR(IL)//', '//NUM2STR(IR)//', '//NUM2STR(IC)//
     +         ') CAN NOT ROUTE WATER TO OVERLAYING WBS (FARM) '//
     +         'BECAUSE A WBS ID > 0 IS NOT LOCATED ABOVE DRAIN'//NL//
     +         'ANY DRT FLOW AT THIS LOCATION IS SENT OUT OF THE '//
     +         'MODELED SYSTEM (eg NOT ROUTED TO FMP)'//NL//
     +         'FMP LINK FOR THIS DRT CELL WILL BE DISABLED.',
     +          INLINE=TRUE)
                    DRTF(6,I) = 0.0
                    CYCLE
            ELSE 
                    DRTF(6,I) = 0.0
            END IF
         END IF
      END DO
      END SUBROUTINE
C
      SUBROUTINE GWF2DRT7FM(IGRID)
C     ******************************************************************
C     ADD DRAIN-RETURN FLOW TO SOURCE TERMS FOR BOTH DRAIN-RETURN CELLS
C     AND RECIPIENT CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:HNEW,HCOF,RHS,IBOUND
      USE GWFDRTMODULE, ONLY:NDRTCL,DRTF,IDRTFL, HAS_FMP
      USE FMP_GLOBAL,    ONLY:WBS,DRTFLOW, FMP_LGR_PNT
C
      INTEGER::FID
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID) 
      CALL FMP_LGR_PNT(IGRID)
C
C1------IF NDRTCL<=0 THERE ARE NO DRAINS. RETURN.
      IF (NDRTCL.LE.0) RETURN
C seb CHECK TO SEE IF THERE ARE ANY LINKS TO FMP AND INITIALIZE THE FLOWS TO ZERO
      IF(HAS_FMP) THEN
        IF(ANY(DRTFLOW%HAS_FLO))  DRTFLOW%FLO=0D0
      END IF
C
C2------PROCESS EACH CELL IN THE DRAIN-RETURN CELL LIST.
      DO 100 L=1,NDRTCL
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
        IL=DRTF(1,L)
        IR=DRTF(2,L)
        IC=DRTF(3,L)
C
C4-------IF THE CELL IS EXTERNAL SKIP IT.
        IF (IBOUND(IC,IR,IL).LE.0) GOTO 100
C
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.
        EL=DRTF(4,L)
C
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.
        IF (HNEW(IC,IR,IL).LE.DBLE(EL)) GOTO 100
C
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.
        C=DRTF(5,L)
        HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*EL
        IF (IDRTFL > 0) THEN
          ILR = DRTF(6,L)
          IRR = DRTF(7,L)                                               !NOTE THAT IF FLOW IS ROUTED TO SWR THE REACH IS STORED IN IRR seb
          ICR = DRTF(8,L)
          RFPROP = DRTF(9,L)
          H = HNEW(IC,IR,IL)
          IF (ILR > 0) THEN
            IF (IBOUND(ICR,IRR,ILR) > 0) THEN
              RHS(ICR,IRR,ILR) = RHS(ICR,IRR,ILR)
     +                           - RFPROP*C*(H-EL)
            END IF
          ELSEIF (ILR.EQ.-1) THEN  !SEND WATER TO FMP
               NF = ICR                                                   !FARM LINK IS SPECIFIED IN ICR
               DRTFLOW(NF)%FLO=DRTFLOW(NF)%FLO+DBLE(RFPROP*C*(H-EL))
          ELSEIF (ILR.EQ.-2) THEN                                       !SWR LINK
               IFLAG=100                                                !VALUE MAY NEED TO BE CHANGED TO FIT SWR NEEDS
               CALL GWF2SWR7EX_V(IGRID,IFLAG,IRR, RFPROP*C*(H-EL) )     !ADD FLOW TO SWR REACH IRR
          ELSEIF (ILR.EQ.-3) THEN                                       !SFR LINK NOT SUPPORTED YET
        WRITE(*,*)'SFR LINK NOT SUPPORTED YET WATER IS LOST/NOT ROUTED'
      WRITE(IOUT,*)'SFR LINK NOT SUPPORTED YET WATER IS LOST/NOT ROUTED'
           CYCLE
          ENDIF
        ENDIF
  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRT7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR DRAIN-RETURN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL ,      ONLY: IOUT,HNEW,IBOUND,BUFF,NCOL,NROW,NLAY
      USE GWFBASMODULE, ONLY: MSUM,VBNM,VBVL,PERTIM,TOTIM,DELT,ICBCFL,
     1                        IAUXSV, HAS_STARTDATE, DATE_SP
      USE GWFDRTMODULE, ONLY: DRTF,NDRTCL,MXDRT,IDRTCB,NDRTVL,IDRTFL,
     1                        NRFLOW,DRTAUX,PRTFIL,DRTBUD,GRP_RTN,DRTDB
      USE FMP_GLOBAL,    ONLY:WBS                                       !seb ADDED CONNECTION TO FMP FOR OUTPUT WITH PRINTFILE OPTION
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
C
      DOUBLE PRECISION HHNEW,EEL,CC,CEL,RATIN,RATOUT,QQ
      CHARACTER(16) TEXT
      CHARACTER(5) DEST
      CHARACTER(19):: DATE
      !CHARACTER(7)  FORMTEST                                             !seb ADDED VARIABLE TO CHECK IF FILE IS BINARY
      !DATA TEXT /'    DRAINS_DRT  '/
      INTEGER:: NDRT_BUD
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      IBD=0
      IF (IDRTCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF (IDRTCB > 0) IBD=ICBCFL
      IBDLBL=0
      !
      IF(PRTFIL%IS_OPEN) CALL PRTFIL%SIZE_CHECK() !CHECK SIZE EVERY 10 STRESS PERIODS
      !
      IF(DRTDB%IS_OPEN) THEN
          CALL DRTDB%SIZE_CHECK() !CHECK SIZE EVERY 10 STRESS PERIODS
          IF(HAS_STARTDATE) THEN
              DATE = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
          ELSE
              DATE='   NaN'
          END IF
      END IF
      !
      GROUPS: DO IG=1, DRTBUD%NGRP
        !
        RATIN=ZERO
        RATOUT=ZERO
        TEXT = DRTBUD%GRP(IG)
        TEXT = ADJUSTR(TEXT)
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
        IF (IBD.EQ.2) THEN
          NAUX = NDRTVL - 5 - 2 - IDRTFL
          IF (IAUXSV.EQ.0) NAUX = 0
          NDRT_BUD = DRTBUD%DIM(IG)+GRP_RTN(IG)
          CALL UBDSV4(KSTP,KPER,TEXT,NAUX,DRTAUX,IDRTCB,NCOL,NROW,NLAY,
     +                NDRT_BUD,IOUT,DELT,PERTIM,TOTIM,IBOUND)
        ENDIF
C
C3------CLEAR THE BUFFER.
        BUFF=ZERO
C
C4------IF THERE ARE NO DRAIN-RETURN CELLS THEN DO NOT ACCUMULATE FLOW.
        BUDGET: IF (DRTBUD%DIM(IG) > 0) THEN ! 
C
C5------LOOP THROUGH EACH DRAIN-RETURN CELL, CALCULATING FLOW.
         DRT_CELLS: DO IDX=1, DRTBUD%DIM(IG)  ! 
           L = DRTBUD%INDEX(IG,IDX)
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING DRAIN.
           IL=DRTF(1,L)
           IR=DRTF(2,L)
           IC=DRTF(3,L)
           Q=ZERO
           ILR=0
           QIN=ZERO                                                     !MOVED QIN TO OUTSIDE OF LOOP TO ENSURE PROPER PRINTING
           IF (IDRTFL > 0) THEN
             !QIN=ZERO
             ILR = DRTF(6,L)
             IRR = DRTF(7,L)
             ICR = DRTF(8,L)
             !
             !IF     (ILR < 0) THEN  !WATER IS SENT TO FMP OR SWR, DO NOT ACCOUNT FOR FLOW RETURNED TO SYSTEM. THOSE PACKAGES WILL HANDEL IT.
             !       ILR = 0                
             !ELSE
             IF (ILR > 0) THEN  ! Condition added 8/26/08 ERB
                   IF ( IBOUND(ICR,IRR,ILR) .LE. 0 ) THEN
                      ILR = 0
                      IRR = 0
                      ICR = 0
                   END IF
             ENDIF
           ELSE
                 ILR = 0
                 IRR = 0
                 ICR = 0
           END IF
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
           IBOUND_CHECK: IF (IBOUND(IC,IR,IL) > 0) THEN
C
C5C-----GET DRAIN PARAMETERS FROM DRAIN-RETURN LIST.
            EL=DRTF(4,L)
            EEL=EL
            C=DRTF(5,L)
            HHNEW=HNEW(IC,IR,IL)
            CC=C
C
C5D-----IF HEAD HIGHER THAN DRAIN, CALCULATE Q=C*(EL-HHNEW).
C5D-----SUBTRACT Q FROM RATOUT.
            IF (HHNEW > EEL) THEN
              CEL=C*EL
              QQ=CEL - CC*HHNEW                                         !Q=C(EL-HHNEW)
              Q=QQ
              RATOUT=RATOUT-QQ
              IF (IDRTFL > 0) THEN
                IF (ILR > 0) THEN
                  RFPROP = DRTF(9,L)
                  QQIN = RFPROP*(CC*HHNEW-CEL)
                  QIN = QQIN
                  RATIN = RATIN + QQIN
                ENDIF
              ENDIF
            ELSE
                QQ = 0D0
                QQIN = 0D0
            ENDIF
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRTCB<0).
           IF (IBD.LT.0) THEN
               IF (IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61        FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
               WRITE(IOUT,62) L,IL,IR,IC,Q
   62        FORMAT(1X,'DRAIN ',I6,'   LAYER ',I3,'   ROW ',I5,
     &          '   COL ',I5,'   RATE ',ES15.6)
             IF (ILR.NE.0) THEN
                 WRITE(IOUT,550) L,ILR,IRR,ICR,QIN
  550          FORMAT(1X,'DRAIN ',I6,' RETURN:  LAYER ',I3,'   ROW ',I5,
     &          '   COL ',I5,'   RATE ',ES15.6)
             ENDIF
             IF(PRTFIL%IS_OPEN) IBDLBL=1                                   !seb IF PRINTFILE NOT USED THEN NO LONGER PRINT HEADER
           ENDIF
Cseb ADDED OPTION TO PRINT FLOWS TO FILE WITH KEYWORD "PRINTFILE"
           IF(PRTFIL%IS_OPEN) THEN
               IU = PRTFIL%IU
             !INQUIRE(UNIT=PRTFIL,FORMATTED=FORMTEST)
             IF(.NOT. PRTFIL%BINARY)THEN
              IF (IBDLBL.EQ.0) WRITE(IU,63) TEXT,NDRTCL,KPER,KSTP,
     +                                          PERTIM,TOTIM,DELT
   63         FORMAT(1X,A,' NDRAINS ',I6,'   PERIOD ',I4,'   STEP ',I3,
     +               ' PERTIM ',ES16.7,' TOTIM ',ES16.7,' DELT ',ES16.7)
              IF     (ILR > 0)  THEN
                   WRITE(IU,64) L,IL,IR,IC,Q,' DRT ',ILR,IRR,ICR,QIN
              ELSEIF (ILR.EQ.0)  THEN
                   WRITE(IU,64) L,IL,IR,IC,Q,' DRT ',0,0,0,0.0
              ELSEIF (ILR.EQ.-1) THEN
                   IF(IRR.EQ.0)IRR=WBS%FID_ARRAY(IC,IR)
                   WRITE(IU,64) L,IL,IR,IC,Q,' FMP ',ILR,IRR,0,QIN
              ELSEIF (ILR.EQ.-2) THEN
                   WRITE(IU,64) L,IL,IR,IC,Q,' SWR ',ILR,IRR,0,QIN
              END IF
   64         FORMAT(1X,I6,3I5,ES15.6,A,3I5,ES15.6)
             ELSE                                                       !UNFORMATTED/BINARY WRITE
              IF (IBDLBL.EQ.0) WRITE(IU) TEXT,KPER,KSTP,NDRTCL
              IF     (ILR > 0)  THEN
                WRITE(IU) L,IL,IR,IC,Q,' DRT ',ILR,IRR,ICR,QIN
              ELSEIF (ILR.EQ.0)  THEN
                WRITE(IU) L,IL,IR,IC,Q,' DRT ',0,0,0,0.0
              ELSEIF (ILR.EQ.-1) THEN
                IF(IRR.EQ.0)IRR=WBS%FID_ARRAY(IC,IR)
                WRITE(IU) L,IL,IR,IC,Q,' FMP ',ILR,IRR,0,QIN
              ELSEIF (ILR.EQ.-2) THEN
                WRITE(IU) L,IL,IR,IC,Q,' SWR ',ILR,IRR,0,QIN
              END IF
             END IF
             IBDLBL=1
           END IF
           IF(DRTDB%IS_OPEN) THEN
               IU = DRTDB%IU
               SELECT CASE(ILR)
               CASE(0);  DEST = ' NONE'
               CASE(1:); DEST = '  DRT'
               CASE(-1); DEST = '  FMP'
               CASE(-2); DEST = '  SWR'
               CASE(-3); DEST = '  SFR'
               END SELECT
               !
               IF(DRTDB%IS_OPEN) THEN
                   IF (ILR.EQ.-1) THEN
                         IF (IRR.EQ.0) IRR=WBS%FID_ARRAY(IC,IR)
                         IF (ICR.NE.0) ICR=0
                   END IF
                   !
                   IF(DRTDB%BINARY) THEN
                     WRITE(DRTDB%IU)
     +                DATE, KPER, KSTP, DBLE(DELT), DBLE(TOTIM), 
     +                IL,IR,IC,CC,EEL,HHNEW,QQ,TEXT,
     +                DEST,ILR,IRR,ICR
                   ELSE
                    ! WRITE (DRTDB%IU,'(1x,A, 1x,2I8, 2( 1x,ES16.9), 
     +              !    3( 1x,I6), 4( 1x,ES16.9), A, A, 3I8  )')
                     WRITE (DRTDB%IU,'(1x,A, 1x,2I8, 2( 1x,A16), 
     +                  3( 1x,I6), 4( 1x,A16), A, A8, 3I9  )')
     +                DATE, KPER, KSTP, NUM2STR(DELT), NUM2STR(TOTIM), 
     +                IL,IR,IC,NUM2STR(CC),NUM2STR(EEL),NUM2STR(HHNEW),
     +                NUM2STR(QQ),TEXT,DEST,ILR,IRR,ICR
                   END IF
               END IF
           END IF
C
C5F-----ADD Q TO BUFFER.
           BUFF(IC,IR,IL) = BUFF(IC,IR,IL) + Q
           IF (IDRTFL > 0 .AND. ILR > 0)
     &         BUFF(ICR,IRR,ILR) = BUFF(ICR,IRR,ILR) + QIN
C          
C5G-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  OR IF
C5G-----RETURNING THE FLOW IN THE DRTF ARRAY, COPY FLOW TO DRTF.
           END IF IBOUND_CHECK
           !
           IF (IBD.EQ.2) THEN
             CALL UBDSVB(IDRTCB,NCOL,NROW,IC,IR,IL,Q,DRTF(:,L),NDRTVL,
     &                   NAUX,10,IBOUND,NLAY)
             IF (IDRTFL.NE.0 .AND. ILR > 0)
     &         CALL UBDSVB(IDRTCB,NCOL,NROW,ICR,IRR,ILR,QIN,DRTF(:,L),
     &                     NDRTVL,NAUX,10,IBOUND,NLAY)
           ENDIF
           DRTF(NDRTVL,L) = Q
           DRTF(NDRTVL-1,L) = QIN
         END DO DRT_CELLS
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
         IF (IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDRTCB,BUFF,NCOL,NROW,
     &                          NLAY,IOUT)
C
C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
         END IF BUDGET
         RIN = RATIN
         ROUT=RATOUT
         VBVL(3,MSUM)=RIN
         VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
         VBVL(4,MSUM)=ROUT
         VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
         VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
         MSUM=MSUM+1
         !
      END DO GROUPS
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2DRT7LR(NLIST,DRTF,LSTBEG,NDRTVL,MXDRT,
     &                       INPACK,IOUT,DRTAUX,NCAUX,NAUX,IFREFM,
     &                       NCOL,NROW,NLAY,ITERP,IDRTFL, BUD, GRP)
C     ******************************************************************
C     Read and print a list of drain and optional associated
C     return-flow recipient cells.  NAUX of the values in the list are
C     optional -- auxiliary data.
C     ******************************************************************
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE FILE_IO_INTERFACE,      ONLY: READ_TO_DATA
      CHARACTER(57):: LABEL1, LABEL2, LABEL3
      CHARACTER(16):: DRTAUX(NCAUX)
      CHARACTER(16), DIMENSION(*):: GRP!(MXDRT)
      LOGICAL:: BUD
      DIMENSION DRTF(NDRTVL,MXDRT)
      CHARACTER(768):: FNAME
      CHARACTER(768):: LINE
      LOGICAL:: NOSHIFT
      !DATA NUNOPN/99/
C     ------------------------------------------------------------------
C
      IERR = 0
      ISCLOC1 = 5
      ISCLOC2 = 5
      IN=INPACK
      ICLOSE=0
      LABEL1='DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR'
      LABEL2='          ----DRAIN CELL----  --RECIPIENT CELL--   RETURN'
      LABEL3='DRAIN NO.  LAYER   ROW   COL   LAYER   ROW   COL    PROP.'
      NOSHIFT = IFREFM == 2
C
C  Check for and decode EXTERNAL and SFAC records.
      CALL READ_TO_DATA(LINE,IN,IOUT, NOSHIFT=NOSHIFT)
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
        IN=I
          WRITE(IOUT,510) IN
  510   FORMAT(1X,'Reading list on unit ',I4)
        CALL READ_TO_DATA(LINE,IN,IOUT, NOSHIFT=NOSHIFT)
      ELSEIF (LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        FNAME=LINE(ISTART:ISTOP)
        IN=0
          WRITE(IOUT,520)FNAME
  520     FORMAT(1X,/1X,'OPENING FILE:',/1X,A)
        !OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
          CALL GENERIC_OPEN(FNAME, IN, IOUT, 'READ', STATUS='OLD', 
     +                      LINE=LINE, INFILE=INPACK)
        ICLOSE=1
        CALL READ_TO_DATA(LINE,IN,IOUT, NOSHIFT=NOSHIFT)
      ENDIF
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'SFAC') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
          WRITE(IOUT,530) SFAC
  530   FORMAT(1X,'LIST SCALING FACTOR= ',ES12.5)
        IF (ISCLOC1.EQ.ISCLOC2) THEN
            WRITE(IOUT,540) ISCLOC1
  540     FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD ',I2,')')
        ELSE
            WRITE(IOUT,550) ISCLOC1,ISCLOC2
  550     FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS ',
     &           I2,'-',I2,')')
        ENDIF
        CALL READ_TO_DATA(LINE,IN,IOUT, NOSHIFT=NOSHIFT)
      ENDIF
C
C  Write a label for the list.
        WRITE(IOUT,'(1X)')
      CALL ULSTLB(IOUT,LABEL1,DRTAUX,NCAUX,NAUX)
C
C  Read the list
      NREAD2=NDRTVL-2
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
      DO 100 II=LSTBEG,N
C  Read a line into the buffer.  (The first line has already been read
C  in order to scan for EXTERNAL and SFAC records.)
       IF (II.NE.LSTBEG) CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=NOSHIFT)
C
C  Read the non-optional values from the line.
        IF (IDRTFL.EQ.0) THEN
          IF (IFREFM.EQ.0) THEN
            READ(LINE,'(3I10,9F10.0)') K,I,J,(DRTF(JJ,II),JJ=4,NREAD1)
            LLOC=10*NREAD1+1
          ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            DO 10 JJ=4,NREAD1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),
     &                    IOUT,IN)
   10       CONTINUE
          ENDIF
        ELSE
          IF (IFREFM.EQ.0) THEN
            READ(LINE,'(3I10,2F10.0,3I10,9F10.0)') K,I,J,
     &          (DRTF(JJ,II),JJ=4,5),KR,IR,JR,(DRTF(JJ,II),JJ=9,NREAD1)
            LLOC=10*NREAD1+1
          ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(4,II),IOUT,
     &                  IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(5,II),IOUT,
     &                  IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KR,R,IOUT,IN)
            IF (KR.EQ.0 .AND. NREAD1.EQ.9 .AND. NAUX.EQ.0) THEN
              IR = 0
              JR = 0
              DRTF(9,II) = 0.0
            ELSE
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IR,R,IOUT,IN)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JR,R,IOUT,IN)
              DO 20 JJ=9,NREAD1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),
     &                      IOUT,IN)
   20         CONTINUE
            ENDIF
          ENDIF
          DRTF(6,II) = KR
          DRTF(7,II) = IR
          DRTF(8,II) = JR
        ENDIF
        DRTF(1,II)=K
        DRTF(2,II)=I
        DRTF(3,II)=J
        DO 50 ILOC=ISCLOC1,ISCLOC2
          DRTF(ILOC,II)=DRTF(ILOC,II)*SFAC
   50   CONTINUE
C
C CHECK IF THERE IS A BUGET GROUP
        IF (BUD) THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
            GRP(II) = LINE(ISTART:ISTOP)
        END IF
C
C  Read the optional values from the line
        IF (NAUX > 0) THEN
          DO 60 JJ=NREAD1+1,NREAD2
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),IOUT,
     &                  IN)
   60     CONTINUE
        ENDIF
C
C  Write the values that were read and that are not related to
C  return flow.
        NN=II-LSTBEG+1
        IF (IDRTFL.EQ.0) THEN
            WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,NREAD2)
  570     FORMAT(1X,I6,I7,I7,I7,14G16.4)
        ELSE
          IF (NREAD2.GE.10) THEN
              WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,5),
     &                      (DRTF(JJ,II),JJ=10,NREAD2)
          ELSE
              WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,5)
          ENDIF
        ENDIF
C
C  Check for illegal grid location
        IF (K.LT.1 .OR. K > NLAY) THEN
            WRITE(IOUT,*) ' ERROR: Layer number is outside of the grid'
          IERR = 1
        ENDIF
        IF (I.LT.1 .OR. I > NROW) THEN
            WRITE(IOUT,*) ' ERROR: Row number is outside of the grid'
          IERR = 1
        ENDIF
        IF (J.LT.1 .OR. J > NCOL) THEN
            WRITE(IOUT,*) ' ERROR: Column number is outside of the grid'
          IERR = 1
        ENDIF
        IF (IERR.NE.0) CALL USTOP('DRT ERROR. PLEASE SEE THE LIST FILE')
  100 CONTINUE
C
C     Check and write data related to return-flow recipient cells
      IF (IDRTFL > 0) THEN
         IF(ANY(DRTF(6,:)<0))THEN                                       !seb CHECK TO SEE IF ANY LayR<0 WHICH INDICATES A LINKAGE TO FMP, SWR, or SFR
          WRITE(IOUT,'(/,1X,A,/,1X,2A)') LABEL2,LABEL3,' LINKED PACKAGE'
         ELSE
          WRITE(IOUT,'(/,1X,A,/,1X,A)') LABEL2,LABEL3
         END IF
        NN = 0
        DO 110 II=LSTBEG,N
          NN = NN + 1
          K = DRTF(1,II)
          I = DRTF(2,II)
          J = DRTF(3,II)
          KR = DRTF(6,II)
          IR = DRTF(7,II)
          JR = DRTF(8,II)
          RFP = DRTF(9,II)
           IF(    KR.EQ.-1)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' FMP LINK'
           ELSEIF(KR.EQ.-2)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SWR LINK'
           ELSEIF(KR.EQ.-3)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SFR LINK'
           ELSE
            WRITE(IOUT,600) NN,K,I,J,KR,IR,JR,RFP
           END IF
  600     FORMAT(1X,I6,3I7,3I7,2X,F8.6)
  601     FORMAT(1X,I6,3I7,3I7,2X,F8.6,A)
C
C  Check for illegal grid location
          IF (KR.NE.0) THEN
            IF (KR.LT.-3 .OR. KR > NLAY) THEN                          !seb CHANGED FROM IF (KR.LT.-0  TO PREVENT ERRORS ON USING NEW FLAGS
                WRITE(IOUT,*) ' ERROR: Layer number is outside of the',
     &                      ' grid'
              IERR = 1
            ENDIF
            IF ((IR.LT.1 .OR. IR > NROW) .AND. KR > 0) THEN
                WRITE(IOUT,*)' ERROR: Row number is outside of the grid'
              IERR = 1
            ENDIF
            IF ((JR.LT.1 .OR. JR > NCOL) .AND. KR > 0) THEN
                WRITE(IOUT,*) ' ERROR: Column number is outside of the',
     &                      ' grid'
              IERR = 1
            ENDIF
C
C  Check for invalid return-flow proportion
            IF (RFP.LT.0.0 .OR. RFP > 1.0) THEN
                WRITE(IOUT,590)
  590         FORMAT(' ERROR: Proportion must be between 0.0 and 1.0')
              IERR = 1
            ENDIF
          ENDIF
C
C  If the proportion = 0 or KR = 0, set all indices and proportion to 0
          IF (KR.EQ.0 .OR. RFP.EQ.0.0) THEN
            DRTF(6,II) = 0.0
            DRTF(7,II) = 0.0
            DRTF(8,II) = 0.0
            DRTF(9,II) = 0.0
          ENDIF
          IF (IERR.NE.0) 
     +                CALL USTOP('DRT ERROR. PLEASE SEE THE LIST FILE')
  110   CONTINUE
      ENDIF
C
      IF (ICLOSE.NE.0) CLOSE(UNIT=IN)
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,MXADRT,
     +                   NDRTCL,DRTAUX,NCAUX,NAUX,IDRTFL,IPRT, BUD, GRP)
C     ******************************************************************
C     Read a list parameter name, look it up in the list of parameters,
C     and substitute values into active part of package array.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE FILE_IO_INTERFACE,  ONLY: READ_TO_DATA
      USE NUM2STR_INTERFACE,  ONLY: NUM2STR
      USE CONSTANTS,          ONLY: NL
      CHARACTER(3) PACK, PTYP
      DIMENSION DRTF(NDRTVL,MXDRT)
      CHARACTER(57) LABEL1, LABEL2, LABEL3
      CHARACTER(16) DRTAUX(NCAUX)
      CHARACTER(16), DIMENSION(*):: GRP!(MXDRT)
      LOGICAL:: BUD
      CHARACTER(768):: LINE
      CHARACTER(MXNAMLEN):: CTMP1, CTMP2, CTMP3, CTMP4                  !seb CHANGED FROM CHAR LEN OF 10
C     ------------------------------------------------------------------
  500 FORMAT(/,' Parameter:  ',A)
!  510 FORMAT(1X,'Parameter type conflict:',/
!     &       1X,'Named parameter:',A,' was defined as type:',A,/
!     &       1X,'However, this parameter is used in the ',A,
!     &       ' file, so it should be type:',A)
!  512 FORMAT(/,1X,'Blank instance name in the ',A,
!     &       ' file for parameter ',A)
  514 FORMAT(3X,'Instance:  ',A)
!  516 FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
!     &       A,'" for parameter ',A)
!  520 FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
!     &       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
  530 FORMAT(1X,I6,I7,I7,I7,14G16.4)
  550 FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &'" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &' -- STOP EXECUTION (SGWF2DRT7LS)')
  600 FORMAT(1X,I6,3I7,3I7,2X,F8.6)
  601 FORMAT(1X,I6,3I7,3I7,2X,F8.6,A)
C
      PACK = 'DRT'
      PTYP = 'DRT'
      IPVL1 = 5
      IPVL2 = 5
      LABEL1='DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE  '
      LABEL2='          ----DRAIN CELL----  --RECIPIENT CELL--   RETURN'
      LABEL3='DRAIN NO.  LAYER   ROW   COL   LAYER   ROW   COL    PROP.'
      !IOUT = ABS(IOUTU)
      IOUT=IOUTU
C
      CALL READ_TO_DATA(LINE,IN,IOUT)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
        WRITE(IOUT,500) LINE(ISTART:ISTOP)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        CALL USTOP('Blank parameter name in the '//PACK//' file.')
      END IF
C
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            CALL USTOP('Parameter type conflict:'//NL//
     +      'Named parameter: '//PARNAM(IP)//' was defined as type: '//
     +      PARTYP(IP)//NL//'However, this parameter is used in the '//
     +      PACK//' file, which should be type: '//PTYP)
          ENDIF
C
C         DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NUMINST=IPLOC(3,IP)
          ILOC=IPLOC(4,IP)
          NI=1
          IF(NUMINST > 0) THEN
            NLST=NLST/NUMINST
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
            CTMP3=LINE(ISTART:ISTOP)
            IF(CTMP3.EQ.' ') THEN
              CALL USTOP('Blank instance name in the '//PACK//
     +                   ' file for parameter '//PARNAM(IP))
            ENDIF
              WRITE(IOUT,514) CTMP3
            CALL UPCASE(CTMP3)
            DO 10 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 15
              ENDIF
   10       CONTINUE
            CALL USTOP('The '//PACK//
     +                 ' file specifies undefined instance "'//CTMP3//
     +                 '" for parameter '//PARNAM(IP))
   15       CONTINUE
          ENDIF
C
          IF (IACTIVE(IP) > 0) THEN
              WRITE(IOUT,550) PARNAM(IP)
            CALL USTOP('SEE LIST FILE')
          ENDIF
C
          IACTIVE(IP)=NI
C
          NDRTCL=NDRTCL+NLST
          IF(NDRTCL > MXADRT) THEN
            CALL USTOP('THE NUMBER OF ACTIVE LIST ENTRIES NDRTCL ('//
     +      NUM2STR(NDRTCL)//')'//NL//
     +      'IS GREATER THAN THE MAXIMUM ALLOWED MXADRT ('//
     +      NUM2STR(MXADRT)//')')
          ENDIF
C
C  Write label for list values
          IF (IPRT > 0) CALL ULSTLB(IOUT,LABEL1,DRTAUX,NCAUX,NAUX)
C
C  Substitute values
          DO 60 I=1,NLST
            II=NDRTCL-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            DO 20 J=1,NREAD
              DRTF(J,II)=DRTF(J,III)
   20       CONTINUE
            DO 40 IPVL=IPVL1,IPVL2
              DRTF(IPVL,II)=DRTF(IPVL,II)*B(IP)
   40       CONTINUE
            IF (BUD) GRP(II) = GRP(III)
            !
            IL=DRTF(1,II)
            IR=DRTF(2,II)
            IC=DRTF(3,II)
            IF (IPRT > 0) THEN
              IF (IDRTFL.EQ.0) THEN
                  WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,NREAD)
              ELSE
                IF (NREAD.GE.10) THEN
                    WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,5),
     &                            (DRTF(JJ,II),JJ=10,NREAD)
                ELSE
                    WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,5)
                ENDIF
              ENDIF
            ENDIF
   60     CONTINUE
          GOTO 120
        ENDIF
  100 CONTINUE
C
      CALL USTOP(' The '//PACK//
     &   ' file specifies an undefined parameter:'//LINE(ISTART:ISTOP))
C
  120 CONTINUE
C
C     WRITE DATA RELATED TO RETURN-FLOW RECIPIENT CELLS
      IF (IDRTFL > 0 .AND. IPRT > 0) THEN
         IF(ANY(DRTF(6,:)<0.))THEN                                       !seb CHECK TO SEE IF ANY LayR<0 WHICH INDICATES A LINKAGE TO FMP, SWR, or SFR
          WRITE(IOUT,'(/,1X,A,/,1X,2A)') LABEL2,LABEL3,' LINKED PACKAGE'
         ELSE
          WRITE(IOUT,'(/,1X,A,/,1X,A)') LABEL2,LABEL3
         END IF
        NN = 0
        DO 140 II=NDRTCL-NLST+1,NDRTCL
          NN = NN + 1
          K = DRTF(1,II)
          I = DRTF(2,II)
          J = DRTF(3,II)
          KR = DRTF(6,II)
          IR = DRTF(7,II)
          JR = DRTF(8,II)
          RFP = DRTF(9,II)
           IF(    KR.EQ.-1)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' FMP LINK'
           ELSEIF(KR.EQ.-2)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SWR LINK'
           ELSEIF(KR.EQ.-3)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SFR LINK'
           ELSE
            WRITE(IOUT,600) NN,K,I,J,KR,IR,JR,RFP
           END IF
  140   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE GWF2DRT7DA(IGRID)
C  Deallocate DRT MEMORY
      USE GWFDRTMODULE
      DEALLOCATE(GWFDRTDAT(IGRID)%NDRTCL)
      DEALLOCATE(GWFDRTDAT(IGRID)%MXDRT )
      DEALLOCATE(GWFDRTDAT(IGRID)%NDRTVL)
      DEALLOCATE(GWFDRTDAT(IGRID)%NDRTNP)
      DEALLOCATE(GWFDRTDAT(IGRID)%IDRTCB)
      DEALLOCATE(GWFDRTDAT(IGRID)%NPDRT )
      DEALLOCATE(GWFDRTDAT(IGRID)%IDRTPB)
      DEALLOCATE(GWFDRTDAT(IGRID)%IDRTFL)
      DEALLOCATE(GWFDRTDAT(IGRID)%NRFLOW)
      DEALLOCATE(GWFDRTDAT(IGRID)%NOPRDT)
      DEALLOCATE(GWFDRTDAT(IGRID)%DRTF  )
      DEALLOCATE(GWFDRTDAT(IGRID)%DRTAUX)
      DEALLOCATE(GWFDRTDAT(IGRID)%DRTGRP)
      DEALLOCATE(GWFDRTDAT(IGRID)%GRP_RTN)
      ! GFORTRAN compiler error work-around for pointer data type FINAL statement
      PRTFIL=>GWFDRTDAT(IGRID)%PRTFIL
      GWFDRTDAT(IGRID)%PRTFIL=>NULL()
      DEALLOCATE(PRTFIL)
      PRTFIL=>NULL()
      !
      DRTBUD=>GWFDRTDAT(IGRID)%DRTBUD
      GWFDRTDAT(IGRID)%DRTBUD=>NULL()
      DEALLOCATE(DRTBUD)
      DRTBUD=>NULL()
      !
      DRTDB=>GWFDRTDAT(IGRID)%DRTDB
      GWFDRTDAT(IGRID)%DRTDB=>NULL()
      DEALLOCATE(DRTDB)
      DRTDB=>NULL()
      !
      DEALLOCATE(GWFDRTDAT(IGRID)%DRT_AUTO_NEG_ITMP)
      DEALLOCATE(GWFDRTDAT(IGRID)%HAS_FMP)
      DEALLOCATE(GWFDRTDAT(IGRID)%HAS_SFR)
      DEALLOCATE(GWFDRTDAT(IGRID)%HAS_SWR)
      !
      !DEALLOCATE(GWFDRTDAT(IGRID)%PRTFIL)
      !DEALLOCATE(GWFDRTDAT(IGRID)%DRTBUD)
      !DEALLOCATE(GWFDRTDAT(IGRID)%DRTDB )
C
C NULLIFY THE LOCAL POINTERS
      IF (IGRID.EQ.1) THEN
        NDRTCL=>NULL()
        MXDRT =>NULL()
        NDRTVL=>NULL()
        NDRTNP=>NULL()
        IDRTCB=>NULL()
        NPDRT =>NULL()
        IDRTPB=>NULL()
        IDRTFL=>NULL()
        NRFLOW=>NULL()
        NOPRDT=>NULL()
        DRTF  =>NULL()
        DRTAUX=>NULL()
        PRTFIL=>NULL() 
        DRTDB =>NULL()
        DRTBUD=>NULL() 
        DRTGRP=>NULL()
        GRP_RTN=>NULL()
        DRT_AUTO_NEG_ITMP=>NULL()
        HAS_FMP=>NULL()
        HAS_SFR=>NULL()
        HAS_SWR=>NULL()
      END IF
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7PNT(IGRID)
C  Change DRT data to a different grid.
      USE GWFDRTMODULE
C
        NDRTCL=>GWFDRTDAT(IGRID)%NDRTCL
        MXDRT=>GWFDRTDAT(IGRID)%MXDRT
        NDRTVL=>GWFDRTDAT(IGRID)%NDRTVL
        NDRTNP=>GWFDRTDAT(IGRID)%NDRTNP
        IDRTCB=>GWFDRTDAT(IGRID)%IDRTCB
        NPDRT=>GWFDRTDAT(IGRID)%NPDRT
        IDRTPB=>GWFDRTDAT(IGRID)%IDRTPB
        IDRTFL=>GWFDRTDAT(IGRID)%IDRTFL
        NRFLOW=>GWFDRTDAT(IGRID)%NRFLOW
        NOPRDT=>GWFDRTDAT(IGRID)%NOPRDT
        DRTF=>GWFDRTDAT(IGRID)%DRTF
        DRTAUX=>GWFDRTDAT(IGRID)%DRTAUX
        PRTFIL=>GWFDRTDAT(IGRID)%PRTFIL
        DRTDB=>GWFDRTDAT(IGRID)%DRTDB
        DRTBUD=>GWFDRTDAT(IGRID)%DRTBUD
        DRTGRP=>GWFDRTDAT(IGRID)%DRTGRP
        GRP_RTN=>GWFDRTDAT(IGRID)%GRP_RTN
        DRT_AUTO_NEG_ITMP=>GWFDRTDAT(IGRID)%DRT_AUTO_NEG_ITMP
        HAS_FMP=>GWFDRTDAT(IGRID)%HAS_FMP
        HAS_SFR=>GWFDRTDAT(IGRID)%HAS_SFR
        HAS_SWR=>GWFDRTDAT(IGRID)%HAS_SWR
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7PSV(IGRID)
C  Save DRT data for a grid.
      USE GWFDRTMODULE
C
        GWFDRTDAT(IGRID)%NDRTCL=>NDRTCL
        GWFDRTDAT(IGRID)%MXDRT=>MXDRT
        GWFDRTDAT(IGRID)%NDRTVL=>NDRTVL
        GWFDRTDAT(IGRID)%NDRTNP=>NDRTNP
        GWFDRTDAT(IGRID)%IDRTCB=>IDRTCB
        GWFDRTDAT(IGRID)%NPDRT=>NPDRT
        GWFDRTDAT(IGRID)%IDRTPB=>IDRTPB
        GWFDRTDAT(IGRID)%IDRTFL=>IDRTFL
        GWFDRTDAT(IGRID)%NRFLOW=>NRFLOW
        GWFDRTDAT(IGRID)%NOPRDT=>NOPRDT
        GWFDRTDAT(IGRID)%DRTF=>DRTF
        GWFDRTDAT(IGRID)%DRTAUX=>DRTAUX
        GWFDRTDAT(IGRID)%PRTFIL=>PRTFIL
        GWFDRTDAT(IGRID)%DRTDB=>DRTDB
        GWFDRTDAT(IGRID)%DRTBUD=>DRTBUD
        GWFDRTDAT(IGRID)%DRTGRP=>DRTGRP
        GWFDRTDAT(IGRID)%GRP_RTN=>GRP_RTN
        GWFDRTDAT(IGRID)%DRT_AUTO_NEG_ITMP=>DRT_AUTO_NEG_ITMP
        GWFDRTDAT(IGRID)%HAS_FMP=>HAS_FMP
        GWFDRTDAT(IGRID)%HAS_SFR=>HAS_SFR
        GWFDRTDAT(IGRID)%HAS_SWR=>HAS_SWR
C
      RETURN
      END
