      MODULE GWFGHBMODULE
        USE PAK_PROP_INTERFACE,              ONLY: PAK_PROPTAB
        USE TABLEFILE_INTERFACE,             ONLY: TABFILETYPE
        USE LINE_FEEDER,                     ONLY: LINE_FEED
        USE BUDGET_GROUP_INTERFACE,          ONLY: BUDGET_GROUP
        USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
        USE PAK_PROP_INTERFACE,              ONLY: ULSTRDSTRUCT   ! Wird compiler bug workaround - moved from GHB_SUBROUTINES
        !USE PACKAGE_LISTING_INTERFACE, ONLY: PACKAGE_LISTING
        !
        PRIVATE:: PAK_PROPTAB
        PRIVATE:: LINE_FEED, BUDGET_GROUP, TABFILETYPE
        PRIVATE:: GENERIC_OUTPUT_FILE
        !PRIVATE:: PACKAGE_LISTING
        !
        INTEGER,SAVE,POINTER:: IOUT, LOUT
        !
        TYPE(TABFILETYPE),POINTER,SAVE::GHBTABFILE
        TYPE(LINE_FEED),  POINTER,SAVE::GHBFEED
        TYPE(PAK_PROPTAB),DIMENSION(:),SAVE,POINTER,CONTIGUOUS:: GHBDATA
        INTEGER,SAVE,POINTER  ::NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB
        INTEGER,SAVE,POINTER  ::NPGHB,IGHBPB,NNPGHB,IPRTGHBTAB
        INTEGER,SAVE,POINTER  ::FLOW_PACK_COND, VARIABLE_COND
        LOGICAL,SAVE,POINTER  ::GHB_AUTO_NEG_ITMP
        INTEGER,SAVE,POINTER  ::MAX_AUX
        CHARACTER(LEN=16),SAVE,DIMENSION(:),  POINTER,CONTIGUOUS::GHBAUX
        !REAL,             SAVE,DIMENSION(:,:),POINTER,CONTIGUOUS::BNDS
        TYPE(BUDGET_GROUP), POINTER,SAVE:: GHBBUD
        TYPE(GENERIC_OUTPUT_FILE), POINTER,SAVE:: GHBDB
        TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:),CONTIGUOUS,
     +                                     POINTER,SAVE:: GRPDB
        CHARACTER(LEN=16),SAVE,DIMENSION(:),POINTER,CONTIGUOUS::GRPDBNAM
        
        !
      TYPE GWFGHBTYPE
        TYPE(TABFILETYPE),POINTER::GHBTABFILE
        TYPE(LINE_FEED),  POINTER::GHBFEED
        INTEGER,POINTER :: NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB
        INTEGER,POINTER :: NPGHB,IGHBPB,NNPGHB,IPRTGHBTAB
        INTEGER,POINTER :: FLOW_PACK_COND, VARIABLE_COND
        LOGICAL,POINTER :: GHB_AUTO_NEG_ITMP
        INTEGER,POINTER  ::MAX_AUX
        CHARACTER(LEN=16), DIMENSION(:),   POINTER,CONTIGUOUS::GHBAUX
        !REAL,              DIMENSION(:,:), POINTER,CONTIGUOUS::BNDS
        TYPE(PAK_PROPTAB),DIMENSION(:),POINTER,CONTIGUOUS:: GHBDATA
        TYPE(BUDGET_GROUP), POINTER:: GHBBUD
        TYPE(GENERIC_OUTPUT_FILE), POINTER:: GHBDB
        TYPE(GENERIC_OUTPUT_FILE), DIMENSION(:),CONTIGUOUS,
     +                                     POINTER:: GRPDB
        CHARACTER(LEN=16),DIMENSION(:),POINTER,CONTIGUOUS::GRPDBNAM
      END TYPE
      TYPE(GWFGHBTYPE), SAVE:: GWFGHBDAT(10)
      END MODULE GWFGHBMODULE


      MODULE GHB_SUBROUTINES
      USE CONSTANTS, ONLY: TRUE,FALSE,NL,BLN,Z,ONE,TWO,DZ,UNO,DOS,BLNK,
     +                     inf_I
      USE GWFGHBMODULE
      USE ERROR_INTERFACE,       ONLY: STOP_ERROR, WARNING_MESSAGE
      USE PARSE_WORD_INTERFACE,  ONLY: PARSE_WORD_UP
      USE NUM2STR_INTERFACE,     ONLY: NUM2STR
      !USE PAK_PROP_INTERFACE,    ONLY: ULSTRDSTRUCT     ! Wird compiler bug workaround - moved from GWFGHBMODULE
      USE PAK_PROP_INTERFACE,    ONLY: UPARLSTSUBSTRUC
      IMPLICIT NONE
      PRIVATE 
      !
      PUBLIC:: GWF2GHB7AR, GWF2GHB7RP, GWF2GHB7AD, 
     +         GWF2GHB7FM, GWF2GHB7BD, 
     +         GWF2GHB7DA, SGWF2GHB7PNT, SGWF2GHB7PSV
      !
      CONTAINS
      !
      SUBROUTINE GWF2GHB7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE AND READ PARAMETER DEFINITIONS FOR GHB
C     PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,              ONLY: IOUT, IFREFM
      USE TABLEFILE_INTERFACE, ONLY: TABFILEPARSE
      USE LINE_FEEDER,         ONLY: LINE_FEED
      USE FILE_IO_INTERFACE,   ONLY: READ_TO_DATA
      USE STRINGS,             ONLY: GET_INTEGER
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
C
      INTEGER, INTENT(IN):: IN, IGRID
C LOCAL
      TYPE(GENERIC_BLOCK_READER):: BL
      CHARACTER(768):: LINE
      CHARACTER(75)::  LABEL
      INTEGER::NAUX,MXPB,MXACTB
      INTEGER::N,NUMINST,NINLST,NLST,LSTSUM,LSTBEG
      INTEGER::LLOC,ISTART,ISTOP,I,J,K,IP, IGRP_OUT
      REAL::R
      LOGICAL:: FOUND_BEGIN, NO_LINEFEED, NO_CBC_READ
      !
      EXTERNAL:: URWORD
      EXTERNAL:: UPARLSTAL, UPARLSTRP, UINSRP, PRESET
      EXTERNAL:: CHECK_CBC_GLOBAL_UNIT
C     ------------------------------------------------------------------
      ALLOCATE(GHB_AUTO_NEG_ITMP)
      ALLOCATE(NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB)
      ALLOCATE(NPGHB,IGHBPB,NNPGHB,IPRTGHBTAB)
      ALLOCATE(GHBBUD,FLOW_PACK_COND,GHBDB,VARIABLE_COND)
C
C1------IDENTIFY PACKAGE AND INITIALIZE NBOUND.
        WRITE(IOUT,1)IN
    1   FORMAT(1X,/1X,'GHB -- GENERAL-HEAD BOUNDARY PACKAGE, VERSION ',
     1   'OWHM',/,9X,'INPUT READ FROM UNIT ',I4)
      NBOUND=Z
      NNPGHB=Z
      IPRTGHBTAB=Z
      FLOW_PACK_COND=Z
      VARIABLE_COND=Z
      NAUX=Z
      IPRGHB=1
      IGRP_OUT = Z
      !
      ALLOCATE(MAX_AUX, SOURCE=5)
      ALLOCATE(GHBAUX(MAX_AUX))
      !
      ALLOCATE(GHBFEED)
      NO_LINEFEED = TRUE
      NO_CBC_READ = TRUE
      GHB_AUTO_NEG_ITMP=FALSE
      GHBAUX=BLNK
C
C2------READ MAXIMUM NUMBER OF GHB'S AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL READ_TO_DATA(LINE,IN,IOUT,IOUT, 
     +                          HED="-- READING GHB PACKAGE INPUT --")
      CALL UPARLSTAL(IN,IOUT,LINE,NPGHB,MXPB)
      !
      CALL GHBBUD%INIT('HEAD DEP BOUNDS')
      !
      DO !BLOCK GROUPS
      !
      CALL BL%LOAD(IN,IOUT,LINE=LINE,FOUND_BEGIN=FOUND_BEGIN)
      !
      IF(BL%NAME == 'PARAMETER') THEN
          CALL UPARLSTAL(IN,IOUT,LINE,NPGHB,MXPB)
          CYCLE
      END IF
      !
      IF (.NOT. FOUND_BEGIN) EXIT
      !
      IF(BL%NAME == 'BUDGET_GROUP' .OR. BL%NAME == 'BUDGET_GROUPS') THEN
        CALL GHBBUD%LOAD(BL)
        !
      ELSEIF(BL%NAME == 'LINEFEED') THEN
         !
         ! ALLOCATE GHBFEED VARIABLE AND 
         ! OPTIONALLY READ IN FEED FILE LOCATIONS
         IF(BL%NLINE>0) THEN
             CALL GHBFEED%INIT(BL,'COND',ONE)    ! =>FEED_ALLOCATE(IN,IOUT,LINE)
             NO_LINEFEED = FALSE
         END IF
      !
      ELSEIF(BL%NAME == 'OPTION' .OR. BL%NAME == 'OPTIONS' ) THEN
        !
        WRITE(IOUT,'(/1X,A)') 'PROCESSING GHB OPTIONS'
        !
        CALL BL%START()
        DO I=1, BL%NLINE
        LLOC = 1
        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
        !
        SELECT CASE (BL%LINE(ISTART:ISTOP))
        CASE('AUXILIARY','AUX')
         CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
         IF(NAUX.LT.MAX_AUX) THEN
              NAUX=NAUX+1
              GHBAUX(NAUX)=BL%LINE(ISTART:ISTOP)
              WRITE(IOUT,12) GHBAUX(NAUX)
   12         FORMAT(1X,'AUXILIARY GHB VARIABLE: ',A)
              IF(GHBAUX(NAUX)=='FLOW_PACK_COND') THEN
                                                 FLOW_PACK_COND = NAUX
              ELSEIF(GHBAUX(NAUX)=='VARIABLE_CONDUCT' .or.                      ! = 'VARIABLE_CONDUCTANCE', except that CHARACTER(16):: GHBAUX
     +               GHBAUX(NAUX)=='VARIABLE_COND'    .or.
     +               GHBAUX(NAUX)=='VARIABLE'             ) THEN
                                    VARIABLE_COND  = NAUX
                                    GHBAUX(NAUX)='VARIABLE_COND'
              END IF
         ELSE
            WRITE(IOUT,*)'GHB WARNING: TOO MANY AUX VARIABLES, MAX OF 5'
         END IF
        CASE('GHB_CBC','CBC')
                        NO_CBC_READ = FALSE
                        CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,
     +                                   IN,IGHBCB,MSG=
     +          'GHB PACKAGE OPTIONS ERROR: FROUND KEYWORD "GHB_CBC" '//
     +          'WHICH SHOULD BE FOLLOWED BY AN INTEGER REPRESENTING '//
     +          'THE UNIT NUMBER TO WRITE THE CELL-BY-CELL FLOWS TOO.')
                 WRITE(IOUT,'(1x,2A /)') 
     +            'GHB PACKAGE CELL-BY-CELL FLOW IS WRITEN TO: '//
     +            NUM2STR(IGHBCB)
        CASE('NOPRINT','NO_PRINT') 
           WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF GENERAL-HEAD BOUNDARY CELLS WILL NOT BE',
     &          ' PRINTED')
         IPRGHB = 0
        CASE('TABPRINT') 
           WRITE(IOUT,14)
   14    FORMAT(1X,'GHB RESULTS FROM TABFILE CALCULATIONS WILL BE ',
     &          'PRINTED TO LIST')
         IPRTGHBTAB = 1
        CASE('AUTOMATIC_NEGATIVE_ITMP') 
            GHB_AUTO_NEG_ITMP = TRUE
        CASE('DBFILE') !ADD KEYWORD TO WRITE ONCE
           !CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
           WRITE(IOUT,715)
  715   FORMAT(1X,'GHB INFORMATION WRITTEN TO DATABASE FRIENDLY OUTPUT')
           CALL GHBDB%OPEN(BL%LINE,LLOC,IOUT,IN,NO_INTERNAL=.TRUE.,
     +                     ALLOW_ONLY_UNIT=.TRUE.)
           IF(GHBDB%BINARY) THEN !IF FILE NOT OPEN THEN NO HEADER IS WRITTEN AND ONLY WRITE HEADER IF NOT BINARY.
               WRITE(IOUT,'(*(A))')'GHB DATABASE FRIENDLY OUTPUT ',
     +         'WRITTEN TO BINARY FILE USING STREAM UNFORMATTED ',
     +         'STRUCTURE.',NL,
     +         'EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',
     +          NL,'"DATE_START (19char), ',
     +         'STRESS PERIOD (int), TIME STEP (int), ',
     +         'TIME STEP LENGTH (double),  SIMULATED TIME (double), ',
     +         'LAY (int), ROW (int), COL (int), ',
     +         'GHB CONDUCTANCE (double), GHB HEAD (double), ',
     +         'GROUNDWATER HEAD (double), GHB FLOW RATE (double), ', 
     +         'GHB BUDGET GROUP (16char)'
           ELSE
              CALL GHBDB%SET_HEADER( ' DATE_START               '//
     +        'PER     STP             DELT          '//
     +        'SIMTIME    LAY    ROW    COL  GHB_CONDUCTANCE         '//
     +   'GHB_HEAD             HEAD    GHB_FLOW_RATE   GHB_BUD_GROUP' )
           END IF
        CASE('BUDGET_GROUP_OUTPUT', 'GROUP_OUTPUT') !ADD KEYWORD TO WRITE ONCE --FOLLOW BY GENERIC_OUTPUT TO SPECIFY OUTPUT OF SPECIFIED BUDGET GROUP ONLY TO SEPARATE FILE
           IGRP_OUT = IGRP_OUT + ONE
        CASE DEFAULT
            ! -- NO OPTIONS FOUND
            WRITE(IOUT,'(/2A,A/)')
     +             'GHB WARNING: FAILED TO IDENTIFY OPTION: ',
     +              BL%LINE(ISTART:ISTOP),
     +             'THIS OPTION IS IGNORED AND NOT APPLIED.'
        END SELECT
        CALL BL%NEXT()
        END DO
        !
        IF(IGRP_OUT>Z) THEN
          ALLOCATE(GRPDB(IGRP_OUT), GRPDBNAM(IGRP_OUT))
          IGRP_OUT = Z
          CALL BL%START()
          DO I=1, BL%NLINE
            LLOC = 1
            CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
            IF(BL%LINE(ISTART:ISTOP)=='BUDGET_GROUP_OUTPUT' .OR. 
     +         BL%LINE(ISTART:ISTOP)==       'GROUP_OUTPUT') THEN
               IGRP_OUT = IGRP_OUT + ONE
               CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
               GRPDBNAM(IGRP_OUT) = BL%LINE(ISTART:ISTOP)
           WRITE(IOUT,716) GRPDBNAM(IGRP_OUT)
  716   FORMAT(1X,'GHB INFORMATION WRITTEN TO DATABASE FRIENDLY OUTPUT',
     +   'FOR BUDGET GROUP ', A)
           CALL GRPDB(IGRP_OUT)%OPEN(BL%LINE,LLOC,IOUT,IN)      
           IF(GRPDB(IGRP_OUT)%BINARY) THEN !IF FILE NOT OPEN THEN NO HEADER IS WRITTEN AND ONLY WRITE HEADER IF NOT BINARY.
               WRITE(IOUT,'(*(A))')'GHB DATABASE FRIENDLY OUTPUT ',
     +         'WRITTEN TO BINARY FILE USING STREAM UNFORMATTED ',
     +         'STRUCTURE.',NL,
     +         'EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',
     +          NL,'"DATE_START (19char), ',
     +         'STRESS PERIOD (int), TIME STEP (int), ',
     +         'TIME STEP LENGTH (double),  SIMULATED TIME (double), ',
     +         'LAY (int), ROW (int), COL (int), ',
     +         'GHB CONDUCTANCE (double), GHB HEAD (double), ',
     +         'GROUNDWATER HEAD (double), GHB FLOW RATE (double)'
           ELSE
              CALL GRPDB(IGRP_OUT)%SET_HEADER(
     +        ' DATE_START               '//
     +        'PER     STP             DELT          '//
     +        'SIMTIME    LAY    ROW    COL  GHB_CONDUCTANCE         '//
     +        'GHB_HEAD             HEAD    GHB_FLOW_RATE' )
           END IF
            END IF
            CALL BL%NEXT()
          END DO
        END IF
        !
        ELSE
           CALL STOP_ERROR(LINE,IN,IOUT,
     +  'GHB BLOCK ERROR. FOUND "BEGIN" KEYWORD, BUT IT WAS '//
     +  ' NOT FOLLOWED BY A KNOWN BLOCK NAME.'//BLN//
     +  'THE FOLLOWING ARE ACCEPTED BLOCK NAMES: '//
     +  '"BUDGET_GROUP", "BUDGET_GROUPS", "OPTION", and "OPTIONS"' )
        END IF
        !
      END DO  !BLOCK GROUPS
      !
      IF(IGRP_OUT==Z) THEN
          IGRP_OUT=ONE
          ALLOCATE(GRPDB(IGRP_OUT), GRPDBNAM(IGRP_OUT))
          GRPDBNAM=''
      END IF
      !
      IF(FLOW_PACK_COND>Z) WRITE(IOUT,'(2A)')'GHB FLOW_PACK_COND ',
     +'IS TURNED ON - CONDUCTANCE IS CALCUATED FROM FLOW PACKAGE'
      !
      IF(VARIABLE_COND>Z) WRITE(IOUT,'(2A)')'GHB VARIABLE_CONDUCTANCE ',
     +'IS TURNED ON - CONDUCTANCE VARIES WITH SATURATED THICKNESS'
      !
      ALLOCATE(GHBTABFILE)
      CALL TABFILEPARSE(IN,IOUT,LINE,GHBTABFILE)
      !
      !ALLOCATE GHBFEED VARIABLE AND OPTIONALLY READ IN FEED FILE LOCATIONS
      IF(NO_LINEFEED) CALL GHBFEED%INIT(IN,IOUT,LINE)    !=>FEED_ALLOCATE(IN,IOUT,LINE)
      !
      LLOC = ONE
      IF(IFREFM.EQ.Z) THEN
         !
         CALL GET_INTEGER(LINE( 1:10),LLOC,ISTART,ISTOP,IOUT,IN,MXACTB,
     +                                                     MSG='NOSTOP')
         !
         IF(MXACTB == inf_I .AND. NO_LINEFEED)THEN
             CALL STOP_ERROR( LINE,IN,LOUT, MSG=NL//
     +            'ERROR: GHB PACKAGE DOES NOT HAVE ANY LINEFEED '//
     +            'FILES SPECIFIED, SO YOU MUST READ "MXACTB", BUT '//
     +            'GET_INTEGER FAILED TO LOAD THAT NUMBER.')
         END IF
         IF(MXACTB == inf_I) MXACTB = Z
         !
         LLOC = ONE
         IF(NO_CBC_READ) CALL GET_INTEGER(LINE(11:20),LLOC,ISTART,ISTOP,
     +                                    IOUT,IN,IGHBCB,MSG='NOSTOP')
         IF(IGHBCB == inf_I)  THEN
             CALL STOP_ERROR(LINE,IN,LOUT, MSG=NL//
     +       'GHB PACKAGE OPTIONS ERROR: DID NOT FIND WITHIN THE '//
     +       'OPTIONS BLOCK THE KEYWORD "GHB_CBC", SO INPUT EXPECTS '//
     +       'TO LOAD AFTER MXACTB THE CBC UNIT NUMBER "IGHBCB".')
         END IF
      ELSE
         !
         CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,MXACTB,
     +                                                     MSG='NOSTOP')
         !
         IF(MXACTB == inf_I .AND. NO_LINEFEED)THEN
             CALL STOP_ERROR( LINE,IN,LOUT, MSG=NL//
     +            'ERROR: GHB PACKAGE DOES NOT HAVE ANY LINEFEED '//
     +            'FILES SPECIFIED, SO YOU MUST READ "MXACTB", BUT '//
     +            'GET_INTEGER FAILED TO LOAD THAT NUMBER.')
         END IF
         IF(MXACTB == inf_I) MXACTB = Z
         !
         IF(NO_CBC_READ) CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,
     +                                    IOUT,IN,IGHBCB,MSG='NOSTOP')
         IF(IGHBCB == inf_I)  THEN
             CALL STOP_ERROR(LINE,IN,LOUT, MSG=NL//
     +       'GHB PACKAGE OPTIONS ERROR: DID NOT FIND WITHIN THE '//
     +       'OPTIONS BLOCK THE KEYWORD "GHB_CBC", SO INPUT EXPECTS '//
     +       'TO LOAD AFTER MXACTB THE CBC UNIT NUMBER "IGHBCB".')
         END IF
         !
      END IF
      !IF(IFREFM.EQ.0) THEN
      !   READ(LINE,'(2I10)') MXACTB,IGHBCB
      !   LLOC=21
      !ELSE
      !   LLOC=1
      !   CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTB,R,IOUT,IN)
      !   CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGHBCB,R,IOUT,IN)
      !END IF
      !
      ! CHECK IF GLOBAL SHUTDOWN OF CBC IS IN EFFECT
      CALL CHECK_CBC_GLOBAL_UNIT(IGHBCB, .FALSE.)
      !
        IF(IGHBCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
        IF(IGHBCB.GT.0) WRITE(IOUT,8) IGHBCB
    8   FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
      DO 
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP)=='AUXILIARY'   .OR.
     +     LINE(ISTART:ISTOP)=='AUX'         .OR.
     +     LINE(ISTART:ISTOP)=='DBFILE'          ) THEN
            !
            CALL WARNING_MESSAGE(TRIM(LINE),IN,IOUT,
     +  'GHB OPTION WARNING. FOUND OPTION "'//LINE(ISTART:ISTOP)//'"'//
     +  BLN//
     + 'BUT GHB NOW EXPECTS OPTIONS BE WITHIN AN OPTIONS BLOCK '//
     + 'RATHER THAN ALONG THE FIRST LINE OF INPUT.'//NL//
     + '(IT WILL STILL PROCESS THIS OPTIONS LINE, '//
     + 'BUT WILL FAIL ON OPTIONS THAT REQUIRE EXTRA INPUT,'//NL//
     + ' SUCH AS "DBFILE GENERIC_OUTPUT".)'
     + //BLN//'PLEASE USE "BEGIN OPTIONS" '//
     + 'AND THEN HAVE ONE OPTION PER LINE AND'//NL//
     + 'END THE OPTIONS BLOCK WITH THE WORD "END" IN PLACE OF SINGLE '//
     + 'LINE OF OPTIONS.'//BLN//'FOR EXAMPLE:'
     +  //BLN//'BEGIN OPTIONS'//BLN//'  NOPRINT'//NL//'  TABPRINT'//NL//
     +  '  DBFILE ./GHB_DATA_OUT.txt'//BLN//'END OPTIONS'//BLN//
     + 'NOTE THAT BLOCK MAYBE EMPTY OR ENTIRELY NOT PRESENT TO NOT '//
     + 'INCLUDE ANY OPTIONS.' )
        ELSEIF(LINE(ISTART:ISTOP)=='NOPRINT' .OR.
     +         LINE(ISTART:ISTOP).EQ.'NO_PRINT') THEN
           WRITE(IOUT,13)
         IPRGHB = 0
        ELSEIF(LINE(ISTART:ISTOP)=='TABPRINT') THEN
           WRITE(IOUT,14)
         IPRTGHBTAB = 1
        ELSEIF(LINE(ISTART:ISTOP)=='AUTOMATIC_NEGATIVE_ITMP') THEN
            GHB_AUTO_NEG_ITMP = TRUE
        ELSE
            EXIT
        END IF
      END DO
C3A-----USING STRUCTURE ARRAY THERE ARE ONLY TWO VALUES READ IN
C3A-----FIRST VALUE IS THE BHEAD AND SECOND VALUE IS THE CONDUCTANCE OR CONDFACT.
      NGHBVL=2
      !
      ! LOAD MODEL CELLS THAT WILL BE DESCRIBED BY THE LINE FEED
      CALL GHBFEED%CELLS(3, 1, NAUX, IPRGHB,
     +                              BUDGET_GROUPS=GHBBUD%BUDGET_GROUPS) !=>FEED_CELLS(LDIM,NPROP,NAUX,IPRT)
      MXACTB = MXACTB + GHBFEED%TOTDAT
      !
      WRITE(IOUT,3) MXACTB
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE GHB CELLS AT ONE TIME')
C
C4------ALLOCATE SPACE FOR THE BNDS ARRAY.
      IGHBPB=MXACTB+1
      MXBND=MXACTB+MXPB
      !ALLOCATE (BNDS(NGHBVL,MXBND))
      ALLOCATE (GHBDATA(MXBND))
      DO I=1, MXBND
                      ALLOCATE(GHBDATA(I)%VAL(NGHBVL))   !BNDS(NGHBVL,MXBND) <=> GHBDATA(MXBND)%VAL(NGHBVL)
        IF(NAUX.GT.0) ALLOCATE(GHBDATA(I)%AUX(NAUX))
        IF(GHBBUD%BUDGET_GROUPS) ALLOCATE(GHBDATA(I)%BUDGET_GROUP)
        IF(GHBTABFILE%NTAB.GT.0)THEN
                      ALLOCATE(GHBDATA(I)%TABNAM   )
                      ALLOCATE(GHBDATA(I)%TSFAC    )
                      !ALLOCATE(GHBDATA(I)%TABIDX   )  !had to comment out cause the ANY function would not work on allocatable TABIDX
           IF(GHBTABFILE%USE_TABEQN) THEN 
                      ALLOCATE( CHARACTER(100):: GHBDATA(I)%TABEQN )
                      ALLOCATE(GHBDATA(I)%TABEQNRES)
           ELSE
                      ALLOCATE( CHARACTER(1):: GHBDATA(I)%TABEQN )
                      GHBDATA(I)%TABEQN=''
           END IF
        END IF
      END DO
C
C-------READ NAMED PARAMETERS.
        WRITE(IOUT,1000) NPGHB
 1000 FORMAT(1X,//1X,I5,' GHB parameters')
      LABEL=
     +  'BOUND. NO. LAYER   ROW   COL     BHEAD      COND FACTOR'
      IF(NPGHB.GT.0) THEN
        LSTSUM=IGHBPB
        DO  K=1,NPGHB
          LSTBEG=LSTSUM
          IF(IPRGHB.EQ.1) WRITE(IOUT,'(/ 2A)') 
     +        ' GHB PACKAGE PARAMETER INFORMATION ',
     +        '[BEFORE APPLICATION OF TABFILES]'
          CALL UPARLSTRP(LSTSUM,MXBND,IN,IOUT,IP,'GHB','GHB',1,
     +                  NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.EQ.0) THEN
C5A-----READ LIST OF CELLS WITHOUT INSTANCES.
            CALL ULSTRDSTRUCT(NLST,GHBDATA,LSTBEG,NGHBVL,MXBND,IN,IOUT,
     +              TRIM(LABEL),GHBAUX,MAX_AUX,NAUX,IFREFM,1,1,IPRGHB)
          ELSEIF(GHB_AUTO_NEG_ITMP) THEN                            ! Not allowed to do automatic ITMP and INSTANCES
               LINE='GHB PARAMETER INSTANCES ARE NOT SUPPORTED WITH '//
     +         'THE GHB OPTION "AUTOMATIC_NEGATIVE_ITMP"'
                CALL STOP_ERROR(INFILE=IN,MSG=LINE,OUTPUT=IOUT)
          ELSE
C5B-----READ INSTANCES
            NINLST=NLST/NUMINST
            DO I=1,NUMINST
               CALL UINSRP(I,IN,IOUT,IP,IPRGHB)
               CALL ULSTRDSTRUCT(NINLST,GHBDATA,LSTBEG,NGHBVL,MXBND,IN,
     1                IOUT,TRIM(LABEL),GHBAUX,MAX_AUX,NAUX,IFREFM,1,1,
     2                IPRGHB)
               LSTBEG=LSTBEG+NINLST
            END DO
          END IF
        END DO
        LSTSUM = LSTSUM-1
      END IF
C IF A TABFILE HAS BEEN SPECIFIED BUILD INDEX FOR PARAMETER VARIABLES TO WHAT 
C TABFILE IS ASSOCIATED WITH THE PARAMETER
        IF(NPGHB.GT.0 .AND. GHBTABFILE%NTAB.GT.0)THEN
          DO I=IGHBPB,LSTSUM
            GHBDATA(I)%TABIDX=0
            IF(GHBDATA(I)%TABNAM.NE.'     NO_TABFILE     ') THEN
              DO J=1,GHBTABFILE%NTAB
                IF(GHBDATA(I)%TABNAM.EQ.GHBTABFILE%TABNAM(J)) THEN
                   GHBDATA(I)%TABIDX=J
                   EXIT
                END IF
              END DO
              IF(GHBDATA(I)%TABIDX.EQ.0)
     +                          GHBDATA(I)%TABNAM=' TABFILE_NOT_FOUND  '
            END IF
          END DO
        END IF
C
C6------RETURN
      CALL SGWF2GHB7PSV(IGRID)
      !
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7RP(IN,IGRID,KPER)
C     ******************************************************************
C     READ GHB HEAD, CONDUCTANCE AND BOTTOM ELEVATION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,            ONLY: IOUT,IFREFM,BOTM,LBOTM,IBOUND
      USE ERROR_INTERFACE,   ONLY: STOP_ERROR
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      INTEGER, INTENT(IN):: IN, IGRID, KPER
C LOCAL
      CHARACTER(768)::LINE
      CHARACTER(75):: LABEL
      INTEGER::NAUX,MXACTB,IAUTO_PARAM
      INTEGER::ITMP,NP,N,I,J,IR,IC,IL,IOUTU, LLOC, ISTART, ISTOP
      REAL::HB,BOT,R
      !
      EXTERNAL:: URWORD
      EXTERNAL:: PRESET
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
      !
      IAUTO_PARAM = 0
C
C1------READ ITMP (NUMBER OF GHB'S OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF(GHB_AUTO_NEG_ITMP .AND. KPER > 1) THEN
         ITMP=-1
         NP=0
         IF(NPGHB.GT.0) IAUTO_PARAM = NBOUND - NNPGHB
      ELSE
         CALL READ_TO_DATA(LINE,IN,IOUT,IOUT)
         IF (LINE .NE. '') THEN
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
          NP=0
          IF(NPGHB.GT.0) CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,
     +                               IOUT,IN)
         ELSE
          WRITE(IOUT,'(/A)')
     +      "GHB PACKAGE WARNING  (don't panic): "//
     +      'FAILED TO READ STRESS PERIOD INFORMATION.'//NEW_LINE(' ')//
     +      'THIS MAY BE DUE TO THE END OF THE FILE BEING REACHED'//
     +      NEW_LINE(' ')//'IF YOU ARE USING LINEFEED, '//
     +      'THEN THOSE RATES ARE STILL APPLIED'      
          ITMP=0
          NP=0
         END IF
      END IF
!      IF(NPGHB.GT.0) THEN
!         IF(IFREFM.EQ.0) THEN
!            READ(IN,'(2I10)') ITMP,NP
!         ELSE
!            READ(IN,*) ITMP,NP
!         END IF
!      ELSE
!         NP=0
!         IF(IFREFM.EQ.0) THEN
!            READ(IN,'(I10)') ITMP
!         ELSE
!            READ(IN,*) ITMP
!         END IF
!      END IF
C
C------CALCULATE SOME CONSTANTS
      NAUX=0
      IF(ALLOCATED(GHBDATA(1)%AUX)) NAUX=UBOUND(GHBDATA(1)%AUX,1)
      IOUTU = IOUT
      !IF (IPRGHB.EQ.0) IOUTU=-IOUT  !CHANGED TO NEW IPRNT FLAG
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER GHB'S.
      IF(ITMP.LT.0) THEN
          IF(GHB_AUTO_NEG_ITMP) THEN
              WRITE(IOUT,'(3A)') '"AUTOMATIC_NEGATIVE_ITMP" Option ',
     +                           'reusing NON-PARAMETER and PARAMETER ',
     +                           'GHB cells from first stress period.'
          ELSE
              WRITE(IOUT,7)
          END IF
    7    FORMAT(1X,/1X,
     1   'REUSING NON-PARAMETER GHB CELLS FROM LAST STRESS PERIOD')
      ELSE
         NNPGHB=ITMP
      END IF
      !
      !READ IN NEXT LINE IN LINE_FEED FILE WHICH CONTAINS THE CURRENT STRESS PERIODS DATA
      CALL GHBFEED%NEXTLINE()
      !
      IF(NNPGHB==0 .AND. NP==0 .AND. GHBFEED%NACT==0)
     +   WRITE(IOUT,'(/2A/)') 'GHB PACKAGE WARNING: ',
     +            'CURRENT STRESS PERIOD DOES NOT CONTAIN ANY GHB CELLS'
      !
C
C3------IF THERE ARE NEW NON-PARAMETER GHB'S, READ THEM.
      MXACTB=IGHBPB-1
      LABEL=
     +    'BOUND. NO. LAYER   ROW   COL     BHEAD      CONDUCTANCE'
      IF(ITMP.GT.0) THEN
         IF(NNPGHB + GHBFEED%NACT.GT.MXACTB) THEN
         LINE='THE NUMBER OF ACTIVE GHB CELLS ('//
     +     NUM2STR(NNPGHB+GHBFEED%NACT)//') IS GREATER THAN MXACTB('
     +     //NUM2STR(MXACTB)
         WRITE(IOUT,'(A)') TRIM(LINE)
         CALL STOP_ERROR(INFILE=IN,MSG=LINE,OUTPUT=IOUT)
         END IF
         !
      IF(IPRGHB.EQ.1) WRITE(IOUT,'(/ 2A)') 
     +      ' GHB PACKAGE STRESS PERIOD INFORMATION ',
     +      '[BEFORE APPLICATION OF TABFILES]'
         CALL ULSTRDSTRUCT(NNPGHB,GHBDATA,1,NGHBVL,MXBND,IN,IOUT,
     1              TRIM(LABEL),GHBAUX,MAX_AUX,NAUX,IFREFM,1,1,IPRGHB)
C IF A TABFILE HAS BEEN SPECIFIED BUILD INDEX FOR NONPARAMETER VARIABLES TO WHAT 
C TABFILE IS ASSOCIATED WITH THE PARAMETER
        IF(GHBTABFILE%NTAB.GT.0)THEN
         DO I=1,NNPGHB
          GHBDATA(I)%TABIDX=0
           IF(GHBDATA(I)%TABNAM.NE.'     NO_TABFILE     ') THEN
             DO J=1,GHBTABFILE%NTAB
               IF(GHBDATA(I)%TABNAM.EQ.GHBTABFILE%TABNAM(J)) THEN
                  GHBDATA(I)%TABIDX=J
                  EXIT
               END IF
             END DO
             IF(GHBDATA(I)%TABIDX.EQ.0)
     +                         GHBDATA(I)%TABNAM=' TABFILE_NOT_FOUND  '
           END IF
         END DO
        END IF
      END IF
      NBOUND=NNPGHB
      !
      ! APPLY THE NEW DATA TO THE GHB PACKAGE ARRAY
      !
      CALL GHBFEED%PAKPROP_APPLY(GHBDATA,1,NBOUND,TRIM(LABEL),
     +                                                 GHBAUX,IPRGHB)
      !
C
C1C-----IF THERE ARE ACTIVE GHB PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('GHB')
      IF(NP.GT.0) THEN
         DO N=1, NP
!         CALL UPARLSTSUB(IN,'GHB',IOUTU,'GHB',BNDS,NGHBVL,MXBND,NREAD,
!     1                MXACTB,NBOUND,5,5,
!     2      'BOUND. NO. LAYER   ROW   COL     STAGE      CONDUCTANCE',
!     3            GHBAUX,20,NAUX)
         CALL UPARLSTSUBSTRUC(IN,'GHB',IOUTU,'GHB',GHBDATA,NGHBVL,MXBND,
     1           MXACTB,NBOUND,2,2,LABEL,GHBAUX,MAX_AUX,NAUX,IPRGHB)
         END DO
      END IF
      !
      IF(IAUTO_PARAM > 0) NBOUND = NBOUND + IAUTO_PARAM
C4
      DO I=1, NBOUND
C
C5------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY.
      IL=GHBDATA(I)%LAY
      IR=GHBDATA(I)%ROW
      IC=GHBDATA(I)%COL
C
C6------IF THE CELL IS EXTERNAL THEN SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0) CYCLE
C
C7------SINCE THE CELL IS INTERNAL GET THE BOUNDARY DATA.
      HB=GHBDATA(I)%VAL(1)
      BOT=BOTM(IC,IR,LBOTM(IL))
ccrth modified to only stop for NWT solver because ghb heads below model cell bottom is generally OK ADDED NTAB CHECK
      IF(HB==HB) THEN
          CONTINUE
                                                  !!!    IF(HB.LT.BOT) CALL WARN%ADD(NUM2STR([IL,IR,IC],4,', ')//', '//
                                                  !!!+                               NUM2STR(HB,10)//', '//
                                                  !!!+                               NUM2STR(BOT)//NL)
       !IF ( HB.LT.BOT.and. IUNITNWT.NE.0)THEN
       !    WRITE(IOUT,103)IC,IR,IL
       !  !CALL USTOP(' ')
       !END IF
      ELSE
          GHBDATA(I)%VAL(1) = UNO
      END IF
      !
!  103 FORMAT(/,'GHB HEAD SET TO BELOW CELL BOTTOM. MODEL WARNING. ',
!     +                'CELL WITH ERROR (IC,IR,IL): ',3I5,/)

      END DO
      !
C
C3------PRINT NUMBER OF GHB'S IN CURRENT STRESS PERIOD.
        WRITE (IOUT,101) NBOUND
  101 FORMAT(1X,/1X,I6,' GHB CELLS')
      !
      IF (GHBBUD%BUDGET_GROUPS) THEN
          CALL GHBBUD%RESET()
          DO I=1, NBOUND
              CALL GHBBUD%ADD( I, GHBDATA(I)%BUDGET_GROUP )
          END DO
      ELSE
              CALL GHBBUD%ADD( -NBOUND )
      END IF
C
C8------RETURN.
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7AD(KSTP,IGRID)
C     ******************************************************************
C     IF TABFILES ARE PRESENT APPLY THEM
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      USE GLOBAL,                   ONLY: IOUT, BOTM, LBOTM, IBOUND
      USE TABLEFILE_INTERFACE,      ONLY: PAK_PROP_TABINTERP
      INTEGER,INTENT(IN)::KSTP,IGRID
C     ------------------------------------------------------------------
      INTEGER:: IPRT, I
      INTEGER:: VLOC
      REAL:: BOT
      TYPE(WARNING_TYPE):: WARN
      !
      VLOC=1
      !
      CALL SGWF2GHB7PNT(IGRID)
      !     
      IPRT=0
      IF(IPRTGHBTAB.NE.0) IPRT=IOUT
      !
      CALL PAK_PROP_TABINTERP(GHBDATA,GHBTABFILE,NBOUND,VLOC,'GHB',
     +                                                        KSTP,IPRT)
      !
      IF(KSTP==1) THEN !Note that tabfiles that are on KSTP>1 will not be checked.
        !
        CALL WARN%INIT()
        !
        DO I=1, NBOUND
          ASSOCIATE( IL=>GHBDATA(I)%LAY, IR=>GHBDATA(I)%ROW,
     +               IC=>GHBDATA(I)%COL, HB=>GHBDATA(I)%VAL(1) )
          !
          IF(IBOUND(IC,IR,IL) > 0) THEN
            !
            IF(HB==HB) THEN
                       BOT = BOTM(IC,IR,LBOTM(IL))
               IF(HB < BOT) CALL WARN%ADD(
     +                          NUM2STR([IL,IR,IC],4,', ')//', '//
     +                          NUM2STR(HB,10)//', '//
     +                          NUM2STR(BOT)//NL)
            END IF
          END IF
          END ASSOCIATE
        END DO
        !
        IF(WARN%RAISED) CALL WARN%CHECK(
     +   HED='GHB HAD BHEAD VALUES BELOW THE BOTTOM OF CELLS.'//NL//
     +  'THE FOLLOWING GHB CELLS MAY CAUSE CONVERGENCE PROBLEMS'//NL//
     +  ' LAY   ROW   COL       BHEAD,  CELL_BOT', OUTPUT=IOUT, TAIL=NL)
      END IF
      !
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7FM(IGRID)
C     ******************************************************************
C     ADD GHB TERMS TO RHS AND HCOF
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,RHS,HCOF,SAT_FRAC
      IMPLICIT NONE
      INTEGER:: IGRID
C LOCAL
      INTEGER:: IR,IC,IL,L,AUX
      DOUBLE PRECISION::HB,C
      LOGICAL:: VAR_COND
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
      IF(VARIABLE_COND == Z) VAR_COND = FALSE
C
C1------IF NBOUND<=0 THEN THERE ARE NO GENERAL HEAD BOUNDS. RETURN.
      IF(NBOUND.LE.Z) RETURN
C
C2------PROCESS EACH ENTRY IN THE GENERAL HEAD BOUND LIST (BNDS).
      DO 100 L=1,NBOUND
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY.
      IL=GHBDATA(L)%LAY
      IR=GHBDATA(L)%ROW
      IC=GHBDATA(L)%COL
C
C4------IF THE CELL IS EXTERNAL THEN SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.Z) GO TO 100
C
C5------SINCE THE CELL IS INTERNAL GET THE BOUNDARY DATA.
C
      IF(VARIABLE_COND > Z) THEN
          VAR_COND = GHBDATA(L)%AUX(VARIABLE_COND) .NE. Z
      END IF
C
      HB=DBLE( GHBDATA(L)%VAL(1) )
      C =DBLE( GHBDATA(L)%VAL(2) )
      !
      IF(FLOW_PACK_COND>Z) THEN
          AUX = GHBDATA(L)%AUX(FLOW_PACK_COND)
          !
          IF(VAR_COND) AUX = ABS(AUX)
          !
          IF(AUX.NE.Z) THEN
              C = C * GET_FLOW_PACK_COND(IL,IR,IC,AUX,HB,VAR_COND)
          ELSEIF(VAR_COND) THEN
              C = C * MAX(SAT_FRAC(IL,IR,IC, HB), SAT_FRAC(IL,IR,IC))
          END IF
      ELSEIF(VAR_COND) THEN
              C = C * MAX(SAT_FRAC(IL,IR,IC, HB), SAT_FRAC(IL,IR,IC))
      END IF
C
C6------ADD TERMS TO RHS AND HCOF.
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*HB
  100 CONTINUE
C
C7------RETURN.
      RETURN
      END SUBROUTINE
      !
      FUNCTION GET_FLOW_PACK_COND(IL,IR,IC,AUX,HB,USE_SAT_THICK)
     +                                                         RESULT(C)
      USE GLOBAL, ONLY: DELR,DELC,HNEW,BOTM,LBOTM,
     +                  IOUT,IUNIT,LAYHDT
      !
      USE GWFLPFMODULE,  ONLY: LAYVKA,    VKA,    HKLPF=>HK,
     +                         CHANI, HANI_LPF=>HANI
      !
      USE GWFUPWMODULE,  ONLY: LAYVKAUPW, VKAUPW, HKUPW,
     +                         HKUPW, CHANI_UPW=>CHANI, HANI_UPW=>HANI
      !
      INTEGER,          INTENT(IN   ):: IL,IR,IC,AUX
      DOUBLE PRECISION, INTENT(IN   ):: HB
      LOGICAL, OPTIONAL,INTENT(IN   ):: USE_SAT_THICK
      DOUBLE PRECISION:: C
      DOUBLE PRECISION:: TOP,BOT,THICK1,THICK2,HK,HANI,H,T1,T2,DR,DC
      INTEGER:: UPW, LPF, TYP
      REAL:: ZERO, RNEG
      LOGICAL:: INC_SAT_THICK
      !
      !IF(AUX == 0) RETURN
      !
      LPF = IUNIT(23)
      UPW = IUNIT(62)
      IF (LPF == Z .AND.  UPW == Z) THEN
         CALL STOP_ERROR(OUTPUT=IOUT, MSG='GHB PACKAGE WITH'//
     +  ' AUX FLOW_PACK_COND ONLY WORKS WITH LPF AND UPW FLOW PACKAGES')
      END IF
      !
      RNEG = -1.0
      ZERO =  0.0
      TYP = ABS(AUX)
      !
      H   = HNEW(IC,IR,IL)
      DR  = DELR(IC)
      DC  = DELC(IR)
      TOP = BOTM(IC,IR,LBOTM(IL)-1)
      BOT = BOTM(IC,IR,LBOTM(IL)  )
      !
      THICK1 = TOP-BOT
      THICK2 = THICK1
      !
      IF(AUX == ONE .OR. AUX == TWO) THEN
         !
         IF(PRESENT(USE_SAT_THICK)) THEN
                    INC_SAT_THICK = USE_SAT_THICK .OR. LAYHDT(IL) > Z
         ELSE
                    INC_SAT_THICK = LAYHDT(IL) > Z
         END IF
         !
         IF(INC_SAT_THICK .AND. HB < TOP) THICK1 = HB-BOT  !AUX = 1 or 2
         IF(INC_SAT_THICK .AND. H  < TOP) THICK2 = H -BOT  !AUX = 1 or 2
      END IF
      !
      IF(THICK1 < DZ) THICK1 = DZ
      IF(THICK2 < DZ) THICK2 = DZ
      !
      IF(THICK1 == DZ .AND. THICK2 == DZ) THEN       !NO THICKNESS, SO NO FLOW!
          C = DZ
      ELSEIF( TYP == ONE .OR. TYP == TWO ) THEN        !PULL HK AND HANI -- REUSING HFB ROUTINE THAT DOES THIS
         IF   (UPW .NE. Z) THEN
                                HK = HKUPW(IC,IR,IL)
         ELSE!(LPF .NE. 0)
                                HK = HKLPF(IC,IR,IL)
         END IF
         !
         IF( TYP == TWO) THEN
               IF (UPW.NE.Z) THEN
                   !
                   IF(CHANI_UPW(IL) < ZERO) THEN
                       HANI = HANI_UPW(IC, IR, INT(RNEG*CHANI_UPW(IL)) )
                   ELSE
                       HANI = CHANI_UPW(IL)
                   END IF
               ELSE!IF(LPF .NE. 0) THEN
                   !
                   IF(CHANI(IL) < ZERO) THEN
                       HANI = HANI_LPF(IC, IR, INT(RNEG*CHANI(IL)) )
                   ELSE
                       HANI = CHANI(IL)
                   END IF
               END IF
         END IF
         !
         IF(TYP == ONE) THEN                              !CALCULATE CR
            T1=HK*THICK1
            T2=HK*THICK2
            C = ( DOS*T1*T2*DC/(T1*DR+T2*DR) )      !Same as CR
         ELSE                                           !CALCULATE CC
            T1=HK*THICK1*HANI
            T2=HK*THICK2*HANI                                   
            C = ( DOS*T1*T2*DR/(T1*DC+T2*DC) )      !Same as CC
         END IF
         !
      ELSE !AUX == 3
         !
         IF (UPW .NE. Z) THEN !USING UPW
              IF(VKAUPW(IC,IR,IL) <= ZERO) THEN
                     HK = DZ
              ELSEIF(LAYVKAUPW(IL).EQ.Z) THEN
                     HK = VKAUPW(IC,IR,IL)
              ELSE
                     HK = HKUPW(IC,IR,IL)/VKAUPW(IC,IR,IL)
              END IF
              !
        ELSE!IF(IUNIT(23).NE.0) THEN !USING LPF
              IF(VKA(IC,IR,IL) <= ZERO) THEN
                     HK = DZ
              ELSEIF(LAYVKA(IL).EQ.Z) THEN
                     HK = VKA(IC,IR,IL)
              ELSE
                     HK = HKLPF(IC,IR,IL)/VKA(IC,IR,IL)
              END IF
              !
        END IF
              C = ( HK*DR*DC/THICK1 )      !Same as SATURATED CV
      END IF
      !
      END FUNCTION
      !
      SUBROUTINE GWF2GHB7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR GHB
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,SAT_FRAC
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM, HAS_STARTDATE, DATE_SP
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      INTEGER:: KSTP,KPER,IGRID
C LOCAL
C
      DOUBLE PRECISION CCGHB,CHB,RATIN,RATOUT,RRATE,DZERO,HB,C,H
      CHARACTER(16) TEXT, TEXT2
      !DATA TEXT /' HEAD DEP BOUNDS'/
C
      INTEGER::NAUX,IBD,IBDLBL,IR,IC,IL,L, IG, IDX, AUX, IGRP
      REAL:: ZERO
      REAL:: RATE,RIN,ROUT
      CHARACTER(19):: DATE
      LOGICAL:: VAR_COND
      EXTERNAL:: UBDSVB, UBUDSV, UBDSV4
      
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
C
      IF(VARIABLE_COND == Z) VAR_COND = FALSE
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0E0
      DZERO=0D0
      IBD=0
      IF(IGHBCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IGHBCB.GT.0) IBD=ICBCFL
      IBDLBL=0
      !
      IF(HAS_STARTDATE) THEN
          DATE = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
      ELSE
          DATE='   NaN'
      END IF
      !
C CALCULATE NAUX
      NAUX=0
      IF(ALLOCATED(GHBDATA(1)%AUX)) NAUX=UBOUND(GHBDATA(1)%AUX,1)
      !
      IF(GHBDB%IS_OPEN) CALL GHBDB%SIZE_CHECK() !CHECK SIZE EVERY 10 STRESS PERIODS
      IF(GRPDB(ONE)%IS_OPEN) THEN
          DO IG=1,SIZE(GRPDB)
              CALL GRPDB(IG)%SIZE_CHECK()
          END DO
      END IF
      !
      GROUPS: DO IG=1, GHBBUD%NGRP
          RATIN =DZERO
          RATOUT=DZERO
          TEXT = GHBBUD%GRP(IG)
          TEXT = ADJUSTR(TEXT)
          TEXT2=TEXT
          IF(TEXT2=='HEAD DEP BOUNDS') THEN
             TEXT2 ='GHB_BUDGET'
             TEXT2 = ADJUSTR(TEXT2)
          END IF
          !
          IGRP=Z
          IF(GRPDB(ONE)%IS_OPEN) THEN
             DO IDX=1, SIZE(GRPDBNAM)
                 IF(GRPDBNAM(IDX) == GHBBUD%GRP(IG) ) THEN
                     IGRP = IDX
                     EXIT
                 END IF
             END DO
          END IF
         
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
          IF(IBD.EQ.2) THEN
           IF(IAUXSV.EQ.0) NAUX=0
           CALL UBDSV4(KSTP,KPER,TEXT,NAUX,GHBAUX,IGHBCB,NCOL,NROW,NLAY,
     1            GHBBUD%DIM(IG),IOUT,DELT,PERTIM,TOTIM,IBOUND)
          END IF
C
C5------LOOP THROUGH EACH BOUNDARY CALCULATING FLOW.
      BUDGET: IF(GHBBUD%DIM(IG) > 0) THEN
C
C3------CLEAR THE BUFFER.
         BUFF=ZERO
         GHB_CELLS: DO IDX=1, GHBBUD%DIM(IG)
           L = GHBBUD%INDEX(IG,IDX)
C
C5A-----GET LAYER, ROW AND COLUMN OF EACH GENERAL HEAD BOUNDARY.
           IL=GHBDATA(L)%LAY
           IR=GHBDATA(L)%ROW
           IC=GHBDATA(L)%COL
           RATE=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, THEN IGNORE IT.
      IBOUND_CHECK: IF(IBOUND(IC,IR,IL) > 0) THEN
C
      IF(VARIABLE_COND > Z) THEN
          VAR_COND = GHBDATA(L)%AUX(VARIABLE_COND) .NE. Z
      END IF
C
C5C-----GET PARAMETERS FROM BOUNDARY LIST.
         HB=GHBDATA(L)%VAL(1)
         C =GHBDATA(L)%VAL(2)
         !
         IF(FLOW_PACK_COND>Z) THEN
             AUX = GHBDATA(L)%AUX(FLOW_PACK_COND)
             !
             IF(VAR_COND) AUX = ABS(AUX)
             !
             IF(AUX.NE.Z) THEN
                 C = C * GET_FLOW_PACK_COND(IL,IR,IC,AUX,HB,VAR_COND)
             ELSEIF(VAR_COND) THEN
                 C = C * MAX(SAT_FRAC(IL,IR,IC, HB), SAT_FRAC(IL,IR,IC))
             END IF
         ELSEIF(VAR_COND) THEN
                 C = C * MAX(SAT_FRAC(IL,IR,IC, HB), SAT_FRAC(IL,IR,IC))
         END IF
         !
         CCGHB=C
C
C5D-----CALCULATE THE FOW RATE INTO THE CELL.
         H = HNEW(IC,IR,IL)
         CHB=C*HB
         RRATE=CHB - CCGHB*H
         RATE=RRATE
         !
         ! PRINT TO DBFILE IF REQUESTED
         IF(GHBDB%IS_OPEN) THEN
               IF(GHBDB%BINARY) THEN
                 WRITE(GHBDB%IU)
     +            DATE, KPER, KSTP, DBLE(DELT), DBLE(TOTIM), 
     +            IL,IR,IC,C,HB,H,RRATE,TEXT
               ELSE
                 WRITE (GHBDB%IU,'(1x,A, 1x,2I8, 2( 1x,A16), 
     +                       3( 1x,I6), 4( 1x,A16), A)')
     +            DATE, KPER, KSTP, NUM2STR(DELT), NUM2STR(TOTIM), 
     +            IL,IR,IC,
     +            NUM2STR(C),NUM2STR(HB),NUM2STR(H),NUM2STR(RRATE),TEXT
               END IF
         END IF
         !
         IF(IGRP>Z) THEN
               IF(GRPDB(IGRP)%BINARY) THEN
                 WRITE(GRPDB(IGRP)%IU)
     +            DATE, KPER, KSTP, DBLE(DELT), DBLE(TOTIM), 
     +            IL,IR,IC,C,HB,H,RRATE
               ELSE
                 WRITE (GRPDB(IGRP)%IU,'(1x,A, 1x,2I8, 2( 1x,A16), 
     +                       3( 1x,I6), 4( 1x,A16))')
     +            DATE, KPER, KSTP, NUM2STR(DELT), NUM2STR(TOTIM), 
     +            IL,IR,IC,NUM2STR(C),
     +            NUM2STR(HB),NUM2STR(H),NUM2STR(RRATE)
               END IF
             
         END IF
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IGHBCB<0).
         IF(IBD.LT.0) THEN
              IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61       FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
              WRITE(IOUT,62) L,IL,IR,IC,RATE,CCGHB
   62    FORMAT(1X,'BOUNDARY ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',
     1          I5,'   RATE ',G15.6,'   COND ',G15.6)
            IBDLBL=1
         END IF
C
C5F-----ADD RATE TO BUFFER.
         BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C5G-----SEE IF FLOW IS INTO AQUIFER OR OUT OF AQUIFER.
         IF(RATE.LT.ZERO) THEN
C
C5H------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
           RATOUT=RATOUT-RRATE
         ELSE
C
C5I-----FLOW IS INTO AQIFER; ADD RATE TO RATIN.
           RATIN=RATIN+RRATE
         END IF
C
C5J-----IF SAVING CELL-BY-CELL FLOWS IN LIST, WRITE FLOW.  ALSO
C5J-----FLOW TO BNDS.
      END IF IBOUND_CHECK
      !
      IF(IBD.EQ.2.AND.NAUX.GT.0) CALL UBDSVB(IGHBCB,NCOL,NROW,IC,IR,IL,
     1               RATE, REAL(GHBDATA(L)%AUX),NAUX,NAUX,1,IBOUND,NLAY)
      IF(IBD.EQ.2.AND.NAUX.EQ.0) CALL UBDSVB(IGHBCB,NCOL,NROW,IC,IR,IL,
     1                       RATE, GHBDATA(L)%VAL,                      ![CONSTRUCT DUMMY ARRAY THAT IS NOT USED]
     2                       NGHBVL,NAUX,NGHBVL,IBOUND,NLAY)
      GHBDATA(L)%CBCFLOW=RATE
      END DO GHB_CELLS
C
C6------IF CELL-BY-CELL TERMS WILL BE SAVED AS A 3-D ARRAY, THEN CALL
C6------UTILITY MODULE UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IGHBCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
      END IF BUDGET
C
C7------MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT THE BUDGET TERM COUNTER.
      MSUM=MSUM+1
      !
      END DO GROUPS
C
C9------RETURN.
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7DA(IGRID)
C  Deallocate GHB MEMORY
      INTEGER, INTENT(IN)::IGRID
C
        DEALLOCATE(GWFGHBDAT(IGRID)%NBOUND    )
        DEALLOCATE(GWFGHBDAT(IGRID)%MXBND     )
        DEALLOCATE(GWFGHBDAT(IGRID)%NGHBVL    )
        DEALLOCATE(GWFGHBDAT(IGRID)%IGHBCB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%IPRGHB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%NPGHB     )
        DEALLOCATE(GWFGHBDAT(IGRID)%IGHBPB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%NNPGHB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%MAX_AUX   )
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBAUX    )
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBTABFILE)
        DEALLOCATE(GWFGHBDAT(IGRID)%IPRTGHBTAB)
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBDATA   )  !NOTE THAT BECAUSE GHBDATA ONLY CONTAINS ONLY ALLOCATABLE ARRAYS AND NORMAL SCALARS WITH NO POINTERS ALL SUBPORTIONS ARE AUTOMATICALLY DEALLOCATED
        DEALLOCATE(GWFGHBDAT(IGRID)%GRPDBNAM )
        DEALLOCATE(GWFGHBDAT(IGRID)%FLOW_PACK_COND)
        DEALLOCATE(GWFGHBDAT(IGRID)%VARIABLE_COND)
        DEALLOCATE(GWFGHBDAT(IGRID)%GHB_AUTO_NEG_ITMP)
        !!!! GFORTRAN compiler error work-around for pointer data type FINAL statement
        !!!GHBFEED=>GWFGHBDAT(IGRID)%GHBFEED
        !!!GWFGHBDAT(IGRID)%GHBFEED=>NULL()
        !!!DEALLOCATE(GHBFEED)
        !!!GHBFEED=>NULL()
        !!!!
        !!!GHBBUD=>GWFGHBDAT(IGRID)%GHBBUD
        !!!GWFGHBDAT(IGRID)%GHBBUD=>NULL()
        !!!DEALLOCATE(GHBBUD)
        !!!GHBBUD=>NULL()
        !!!!
        !!!GHBDB=>GWFGHBDAT(IGRID)%GHBDB
        !!!GWFGHBDAT(IGRID)%GHBDB=>NULL()
        !!!DEALLOCATE(GHBDB)
        !!!GHBDB=>NULL()
        !!!!
        !!!GRPDB=>GWFGHBDAT(IGRID)%GRPDB
        !!!GWFGHBDAT(IGRID)%GRPDB=>NULL()
        !!!DEALLOCATE(GRPDB)
        !!!GRPDB=>NULL()
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBFEED) 
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBBUD )
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBDB  )
        DEALLOCATE(GWFGHBDAT(IGRID)%GRPDB  )
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.EQ.1)THEN
        NBOUND    =>NULL()
        MXBND     =>NULL()
        NGHBVL    =>NULL()
        IGHBCB    =>NULL()
        IPRGHB    =>NULL()
        NPGHB     =>NULL()
        IGHBPB    =>NULL()
        NNPGHB    =>NULL()
        MAX_AUX   =>NULL()
        GHBAUX    =>NULL()
        GHBTABFILE=>NULL()
        GHBFEED   =>NULL()
        IPRTGHBTAB=>NULL()
        GHBDATA   =>NULL()
        GHBBUD    =>NULL()
        GHBDB     =>NULL()
        GRPDB     =>NULL()
        GRPDBNAM  =>NULL()
        FLOW_PACK_COND=>NULL()
        VARIABLE_COND =>NULL()
        GHB_AUTO_NEG_ITMP=>NULL()
      END IF
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE SGWF2GHB7PNT(IGRID)  
C  Change GHB data to a different grid.
        INTEGER, INTENT(IN)::IGRID
C
        NBOUND=>GWFGHBDAT(IGRID)%NBOUND
        MXBND=>GWFGHBDAT(IGRID)%MXBND
        NGHBVL=>GWFGHBDAT(IGRID)%NGHBVL
        IGHBCB=>GWFGHBDAT(IGRID)%IGHBCB
        IPRGHB=>GWFGHBDAT(IGRID)%IPRGHB
        NPGHB=>GWFGHBDAT(IGRID)%NPGHB
        IGHBPB=>GWFGHBDAT(IGRID)%IGHBPB
        NNPGHB=>GWFGHBDAT(IGRID)%NNPGHB
        MAX_AUX=>GWFGHBDAT(IGRID)%MAX_AUX
        GHBAUX =>GWFGHBDAT(IGRID)%GHBAUX
        GHBTABFILE=>GWFGHBDAT(IGRID)%GHBTABFILE
        GHBFEED=>GWFGHBDAT(IGRID)%GHBFEED
        IPRTGHBTAB=>GWFGHBDAT(IGRID)%IPRTGHBTAB
        GHBDATA=>GWFGHBDAT(IGRID)%GHBDATA
        GHBBUD=>GWFGHBDAT(IGRID)%GHBBUD
        GHBDB=>GWFGHBDAT(IGRID)%GHBDB
        GRPDB=>GWFGHBDAT(IGRID)%GRPDB
        GRPDBNAM=>GWFGHBDAT(IGRID)%GRPDBNAM
        FLOW_PACK_COND=>GWFGHBDAT(IGRID)%FLOW_PACK_COND
        VARIABLE_COND=>GWFGHBDAT(IGRID)%VARIABLE_COND
        GHB_AUTO_NEG_ITMP=>GWFGHBDAT(IGRID)%GHB_AUTO_NEG_ITMP
C
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE SGWF2GHB7PSV(IGRID)  
C  Save GHB data for a grid.
        INTEGER, INTENT(IN)::IGRID
C
        GWFGHBDAT(IGRID)%NBOUND=>NBOUND
        GWFGHBDAT(IGRID)%MXBND=>MXBND
        GWFGHBDAT(IGRID)%NGHBVL=>NGHBVL
        GWFGHBDAT(IGRID)%IGHBCB=>IGHBCB
        GWFGHBDAT(IGRID)%IPRGHB=>IPRGHB
        GWFGHBDAT(IGRID)%NPGHB=>NPGHB
        GWFGHBDAT(IGRID)%IGHBPB=>IGHBPB
        GWFGHBDAT(IGRID)%NNPGHB=>NNPGHB
        GWFGHBDAT(IGRID)%MAX_AUX=>MAX_AUX
        GWFGHBDAT(IGRID)%GHBAUX =>GHBAUX
        GWFGHBDAT(IGRID)%GHBTABFILE=>GHBTABFILE
        GWFGHBDAT(IGRID)%GHBFEED=>GHBFEED
        GWFGHBDAT(IGRID)%IPRTGHBTAB=>IPRTGHBTAB
        GWFGHBDAT(IGRID)%GHBDATA=>GHBDATA
        GWFGHBDAT(IGRID)%GHBBUD=>GHBBUD
        GWFGHBDAT(IGRID)%GHBDB=>GHBDB
        GWFGHBDAT(IGRID)%GRPDB=>GRPDB
        GWFGHBDAT(IGRID)%GRPDBNAM=>GRPDBNAM
        GWFGHBDAT(IGRID)%FLOW_PACK_COND => FLOW_PACK_COND
        GWFGHBDAT(IGRID)%VARIABLE_COND=>VARIABLE_COND
        GWFGHBDAT(IGRID)%GHB_AUTO_NEG_ITMP=>GHB_AUTO_NEG_ITMP
C
      RETURN
      END SUBROUTINE
      !
      END MODULE


