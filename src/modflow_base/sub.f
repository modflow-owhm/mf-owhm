      MODULE GWFSUBMODULE
        USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
        PRIVATE:: GENERIC_OUTPUT_FILE
        !
        INTEGER,SAVE,POINTER ::IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE
!        INTEGER,SAVE,POINTER ::ISUBLNK,ILPFLNK                         !SUB-Linkage rth 
        REAL,   SAVE,POINTER ::AC1,AC2
        LOGICAL,SAVE,POINTER ::HAS_DELAY_BED
        LOGICAL,SAVE,POINTER ::HAS_INST_BED
        LOGICAL,SAVE,POINTER ::LPFLNK                           ! SUB-Linkage rth seb MOVED SUBLINK TO BAS GLOBAL
        LOGICAL,SAVE,POINTER ::SEPARTE_FLOWS                    ! IF TRUE THEN KEEP INELASTIC/ELASTIC FLOWS SEPARATE
        REAL,   SAVE,POINTER ::NOCOMV                           ! value HC is set to when a cell goes dry
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: ISBOCF
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: ISBOCU
        LOGICAL,SAVE, DIMENSION(:,:),   POINTER,CONTIGUOUS:: OCFLGS
        LOGICAL,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: OCLAY
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: ILSYS
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: NTSSUM
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: LN
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: LDN
        INTEGER,SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: NZ
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: RNB
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DH
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DHP
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DHC
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DZ
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: HC
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: SCE
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: SCV
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DCOM
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DCOME
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: DCOMV
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: A1
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: A2
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: BB
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: SUB
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: SUBE
        REAL,   SAVE, DIMENSION(:),     POINTER,CONTIGUOUS:: SUBV
        REAL,   SAVE, DIMENSION(:,:),   POINTER,CONTIGUOUS:: DP
        REAL,   SAVE, DIMENSION(:,:),   POINTER,CONTIGUOUS:: DVB
        REAL,   SAVE, DIMENSION(:,:,:), POINTER,CONTIGUOUS:: DVZ                  !SUB-Linkage rth
        REAL,   SAVE, DIMENSION(:,:,:), POINTER,CONTIGUOUS:: DVZC                 !WSCHMID
        TYPE(GENERIC_OUTPUT_FILE),SAVE,DIMENSION(:),POINTER,CONTIGUOUS::
     +                                                        DELAY_HED
        INTEGER,SAVE, DIMENSION(:,:),POINTER,CONTIGUOUS:: DELAY_HED_ID
      TYPE GWFSUBTYPE
        INTEGER, POINTER  ::IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE
!        INTEGER, POINTER  ::ISUBLNK,ILPFLNK                         
        REAL,    POINTER:: AC1,AC2
        LOGICAL, POINTER:: HAS_DELAY_BED
        LOGICAL, POINTER:: HAS_INST_BED
        LOGICAL, POINTER:: LPFLNK                            ! SUB-Linkage rth  seb removed ,SUBLNK it is now in global
        LOGICAL, POINTER:: SEPARTE_FLOWS
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: ISBOCF
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: ISBOCU
        LOGICAL, DIMENSION(:,:),   POINTER,CONTIGUOUS:: OCFLGS
        LOGICAL, DIMENSION(:),     POINTER,CONTIGUOUS:: OCLAY
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: ILSYS
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: NTSSUM
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: LN
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: LDN
        INTEGER, DIMENSION(:),     POINTER,CONTIGUOUS:: NZ
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: RNB
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DH
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DHP
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DHC
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DZ
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: HC
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: SCE
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: SCV
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DCOM
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DCOME
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: DCOMV
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: A1
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: A2
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: BB
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: SUB
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: SUBE
        REAL,    DIMENSION(:),     POINTER,CONTIGUOUS:: SUBV
        REAL,    DIMENSION(:,:),   POINTER,CONTIGUOUS:: DP
        REAL,    DIMENSION(:,:),   POINTER,CONTIGUOUS:: DVB
        REAL,    DIMENSION(:,:,:), POINTER,CONTIGUOUS:: DVZ             ! SUB-Linkage rth
        REAL,    DIMENSION(:,:,:), POINTER,CONTIGUOUS:: DVZC
        REAL,                      POINTER ::NOCOMV                    ! value HC is set to when a cell goes dry
        TYPE(GENERIC_OUTPUT_FILE),DIMENSION(:),POINTER,CONTIGUOUS::
     +                                                        DELAY_HED
        INTEGER, DIMENSION(:,:),POINTER,CONTIGUOUS:: DELAY_HED_ID
      END TYPE
      TYPE(GWFSUBTYPE), SAVE  ::GWFSUBDAT(10)

      END MODULE GWFSUBMODULE
      !
      !
      !
      SUBROUTINE GWF2SUB7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SUBSIDENCE PACKAGE.
C     READ SUBSIDENCE PACKAGE DATA.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,   ONLY: Z, TRUE, FALSE, SNGL_ninf, NewLine=>NL
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,ISSFLG,NPER,NSTP,HNEW,
     1                      DELR,DELC,BOTM,LBOTM,SUBLNK,LAYCBD !,BUFF 
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFSUBMODULE,ONLY:IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE,
     1                      AC1,AC2,ISBOCF,ISBOCU,
     2                      OCFLGS,OCLAY,ILSYS,NTSSUM,LN,LDN,NZ,RNB,
     3                      DH,DHP,DHC,DZ,HC,SCE,SCV,DCOM,DCOME,DCOMV,
     4                      A1,A2,BB,SUB,SUBE,SUBV,DP,DVB,
     5                      LPFLNK,DVZ,DELAY_HED,DELAY_HED_ID,
     6                      DVZC,NOCOMV,SEPARTE_FLOWS,
     7                      HAS_DELAY_BED,HAS_INST_BED
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR, WARNING_MESSAGE
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD, PARSE_WORD_UP
      USE STRINGS,              ONLY: GET_INTEGER, GET_NUMBER
      USE PATH_INTERFACE,       ONLY: ADD_DIR_SLASH_ALLOC
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
C
      REAL,    DIMENSION(NCOL,NROW)::BUFFER                             ! REPLACES GLOBAL MODULE BUFF(:,:,1) WITH BUFFER(:,:) - IF THIS CAUSES STACK OVERFLOW PROBLEMS THEN CAN CHANGE TO ALLOCATABLE ARRAY TO SWITCH USAGE TO HEAP
      LOGICAL, DIMENSION(NCOL,NROW)::PACK_MASK
      CHARACTER(4 )::PTYP
      TYPE SUBPARAM
        !SEQUENCE
        INTEGER:: SRNB=Z,NDHC=Z,SFE =Z,SFV =Z,COME=Z,COMV=Z
        INTEGER:: DSTR=Z,DHC =Z,DCME=Z,DCMV=Z,SDZ =Z
      END TYPE
      TYPE(SUBPARAM):: SUBP                                            ! SUBSIDENCE PARAMETER FLAGS 
      INTEGER::ISUBLNK
      DIMENSION IFL(21)
      CHARACTER(24), DIMENSION(12):: ANAME
      CHARACTER(64):: TLINE
      !CHARACTER(768) LINE
      TYPE(GENERIC_BLOCK_READER):: BL
      LOGICAL:: HAS_ERROR,FOUND_BEGIN
      CHARACTER(:), ALLOCATABLE:: PNT_INI_CRIT
      !DATA ANAME(1) /'   PRECONSOLIDATION HEAD'/
      !DATA ANAME(2) /'ELASTIC INTERBED STORAGE'/
      !DATA ANAME(3) /' VIRGIN INTERBED STORAGE'/
      !DATA ANAME(4) /' ND STRT ELAS COMPACTION'/
      !DATA ANAME(5) /' ND STRT VIRG COMPACTION'/
      !DATA ANAME(6) /'     DELAY STARTING HEAD'/
      !DATA ANAME(7) /'   DELAY PRECOLSOL. HEAD'/
      !DATA ANAME(8) /'  D STRT ELAS COMPACTION'/
      !DATA ANAME(9) /'  D STRT VIRG COMPACTION'/
      !DATA ANAME(10) /'DELAY INTERBED THICKNESS'/
      !DATA ANAME(11) /'   MATERIAL ZONE INDICES'/
      !DATA ANAME(12)/'NUMBER OF BEDS IN SYSTEM'/
      DIMENSION IBUFF(NCOL,NROW)
C     ------------------------------------------------------------------
      ALLOCATE (IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE)                !SUB-Linkage rth
      ALLOCATE (AC1,AC2)
      ALLOCATE (HAS_DELAY_BED,HAS_INST_BED,LPFLNK)                      !SUB-Linkage rth
      ALLOCATE (SEPARTE_FLOWS)
      ALLOCATE (ISBOCF(10), ISBOCU(10))
      !
      ALLOCATE(NOCOMV, SOURCE=SNGL_ninf)                                !HC value set when no more inelastic compaction is capable
      !
      ANAME(1) = '   PRECONSOLIDATION HEAD'
      ANAME(2) = 'ELASTIC INTERBED STORAGE'
      ANAME(3) = ' VIRGIN INTERBED STORAGE'
      ANAME(4) = ' ND STRT ELAS COMPACTION'
      ANAME(5) = ' ND STRT VIRG COMPACTION'
      ANAME(6) = '     DELAY STARTING HEAD'
      ANAME(7) = '   DELAY PRECOLSOL. HEAD'
      ANAME(8) = '  D STRT ELAS COMPACTION'
      ANAME(9) = '  D STRT VIRG COMPACTION'
      ANAME(10)= 'DELAY INTERBED THICKNESS'
      ANAME(11)= '   MATERIAL ZONE INDICES'
      ANAME(12)= 'NUMBER OF BEDS IN SYSTEM'
      !
      ZERO=0.0
      SEPARTE_FLOWS = FALSE
      !
      DO I=1,NROW
      DO J=1,NCOL
             PACK_MASK(J,I) = TRUE
      END DO
      END DO
C
C1------IDENTIFY PACKAGE.
        WRITE(IOUT,1)IN
    1 FORMAT(/,'SUB7 -- SUBSIDENCE PACKAGE, VERSION OWHM',
     1                ' INPUT READ FROM UNIT',I3)
C
C2------CHECK TO SEE THAT SUBSIDENCE OPTION IS APPROPRIATE
C2------IF INAPPROPRIATE PRINT A MESSAGE & STOP THE SIMULATION.
C2------ALSO, SUM TO GET THE TOTAL NUMBER OF TIME STEPS IN THE
C2------SIMULATION.
C
      NSTPT=Z
      DO 12 NS=1,NPER
      NSTPT=NSTPT+NSTP(NS)
      IF(ISSFLG(NS).NE.Z.AND.NS.GT.1) THEN
       CALL USTOP('SUB CANNOT BE USED IN SIMULATIONS '//
     +            'IN WHICH STRESS PERIODS OTHER THAN THE '//
     +            'FIRST ARE STEADY-STATE.')
      ENDIF
 12   CONTINUE
C
C3------ALLOCATE SPACE FOR ARRAY NTSSUM, WHICH WILL CONTAIN THE TOTAL
C3------NUMBER OF TIME STEPS PRIOR TO THE CURRENT TIME STEP.
      ALLOCATE(NTSSUM(NPER))
C
C4------READ FLAG FOR STORING CELL-BY-CELL STORAGE CHANGES AND
C4------FLAG FOR PRINTING AND STORING COMPACTION, SUBSIDENCE, AND
C4------CRITICAL HEAD ARRAYS.
      !CALL URDCOM(IN,IOUT,LINE)
      NSBP=Z
      IPRNTFLG=Z
      ISUBLNK = Z
      N = Z  !FLAG FOR PRINTING OF DBED
      DO
        CALL BL%LOAD(IN,IOUT,FOUND_BEGIN=FOUND_BEGIN,NO_BACKSPACE=TRUE)! BL%LN HOLDS LINE FROM "IN", WHILE BL%LINE HOLDS WHAT IS WITHIN THE BLOCK
        !CALL UPARARRAL(IN,IOUT,LINE,NSBP)                             ! NSBP is the number subsidence parameters - CAN NOT READ PRINT FLAG
        !LLOC=1
        !CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        SELECT CASE(BL%NAME)
        CASE('PARAMETER')
           LLOC = 10
           CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,NSBP,        ! NSBP is the number subsidence parameters
     +                      MSG='SUB - Failed to read: NSBP')
           CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IPRNTFLG,    ! IPRNTFLG is print flag for UPARARRSUB1
     +                      MSG='SUB - Failed to read: IPRNTFLG')
        CASE('OPTION','OPTIONS')
          CALL BL%START()
          DO I=1,BL%NLINE
             LLOC = 1
             CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
             SELECT CASE(BL%LINE(ISTART:ISTOP))
             CASE('SUBLINK','SUB_LINK')
                                      ISUBLNK = 1
             CASE('SEPARATE_FLOWS','SEPARATE_FLOW')
                                      SEPARTE_FLOWS = TRUE
             CASE('PARAMETER')
                  LLOC = 10
           CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,NSBP,             ! NSBP is the number subsidence parameters
     +                      MSG='SUB - Failed to read: NSBP')
           CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,IPRNTFLG,         ! IPRNTFLG is print flag for UPARARRSUB1
     +                      MSG='SUB - Failed to read: IPRNTFLG')
             CASE('PRINT_INITIAL_CRITICAL_HEAD')
                  CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP)
                  !
                  PNT_INI_CRIT = BL%LINE(ISTART:ISTOP)
                  CALL ADD_DIR_SLASH_ALLOC(PNT_INI_CRIT, OS_SLASH=TRUE)
                 !
             CASE DEFAULT
                CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG=
     +               'SUB OPTION BLOCK FAILED TO IDENTIFY'//
     +               ' OPTION "'//BL%LINE(ISTART:ISTOP)//
     +               '" --- IT WILL BE IGNORED',INLINE=TRUE)
             END SELECT
             !
             CALL BL%NEXT()
          END DO               ! END CASE('OPTION','OPTIONS')
          !
        CASE('PRINT_DELAY_HEAD')
          IF (BL%NLINE > Z)THEN
             N = BL%NLINE
             ALLOCATE(DELAY_HED_ID(4,N))  !1 = KQ, 2 = SPSTART, 3 = SPSTOP, 4 = ONLY SP PRINT
             ALLOCATE(DELAY_HED(N)   ) 
             CALL BL%START()
             DO I=1,N
                LLOC=1
                CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                !
                IF(BL%LINE(ISTART:ISTOP) == 'ALL') THEN
                            DELAY_HED_ID(1,I) = Z
                ELSE
                    CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +              DELAY_HED_ID(1,I),TRUE,
     +            MSG='"PRINT_DELAY_HEAD" KEYWORD FAILED TO IDENTIFY '//
     +            'DELAY INTERBED ID THAT IS PRINTED OR KEYWORD "ALL"')
                END IF
                !
                K = LLOC
                CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                !
                SELECT CASE( BL%LINE(ISTART:ISTOP) )
                CASE('BY_STRESS_PERIOD','STRESS_PERIOD')
                            DELAY_HED_ID(2,I) = 1
                CASE('BY_TIME_STEP','TIME_STEP')
                            DELAY_HED_ID(2,I) = Z
                CASE DEFAULT
                 LLOC = K
                 DELAY_HED_ID(2,I) = Z
!                 CALL STOP_ERROR(BL%LINE,IN,IOUT,MSG=
!     +           '"PRINT_DELAY_HEAD" KEYWORD FAILED TO IDENTIFY '//
!     +           'AFTER INTERBED ID THE KEYWORD "BY_STRESS_PERIOD" OR '//
!     +           '"BY_TIME_STEP" TO INDICATE FREQUENCY OF PRINTING THE '//
!     +           'DELAY BED HEAD.')
                END SELECT
                !----------------------------
                K = LLOC
                CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +          DELAY_HED_ID(3,I),HAS_ERROR=HAS_ERROR)
                !
                IF(HAS_ERROR) THEN !ASSUME THAT STARTING SP IS NOT SPECIFIED
                    LLOC = K
                    DELAY_HED_ID(3,I) = 1
                    DELAY_HED_ID(4,I) = NPER
                ELSE
                    K = LLOC
                    CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +              DELAY_HED_ID(4,I),HAS_ERROR=HAS_ERROR)
                    IF(HAS_ERROR) THEN !ASSUME THAT STARTING SP IS NOT SPECIFIED
                        LLOC = K
                        DELAY_HED_ID(4,I) = NPER
                    END IF
                END IF    
                !
                CALL DELAY_HED(I)%OPEN(BL%LINE,LLOC,IOUT,IN,
     +                                     NO_INTERNAL=TRUE)
                !
                CALL BL%NEXT()
             END DO
          END IF
          !      
        CASE DEFAULT
          IF(FOUND_BEGIN) THEN
             CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG=            
     +         'SUB BLOCK NAME IS NOT RECOGNIZED. THE FOLLOWING BLOCK "'
     +         //BL%NAME//'" WILL BE SKIPPED AND NOT INCLUDED',
     +         INLINE=TRUE, CMD_PRINT=TRUE)
             ELSE
                 EXIT
          END IF
        END SELECT
      END DO
      !
      IF(N==Z) THEN
          ALLOCATE(DELAY_HED_ID(1,1))
          ALLOCATE(DELAY_HED(1)     )
      END IF
      !Read -> ISUBCB ISUBOC NNDB NDB NMZ NN AC1 AC2 ITMIN IDSAVE IDREST SUBLNK
      LLOC=1
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IIBSCB, 
     +      MSG='SUB - Failed to read 1st number: ISUBCB')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISUBOC, 
     +      MSG='SUB - Failed to read 2nd number: ISUBOC')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,NNDB, 
     +      MSG='SUB - Failed to read 3rd number: NNDB')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,NDB, 
     +      MSG='SUB - Failed to read 4th number: NDB')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,NMZ, 
     +      MSG='SUB - Failed to read 5th number: NMZ')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,NN, 
     +      MSG='SUB - Failed to read 6th number: NN')
      
      CALL GET_NUMBER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,AC1, 
     +      MSG='SUB - Failed to read 7th number: AC1')
      
      CALL GET_NUMBER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,AC2, 
     +      MSG='SUB - Failed to read 8th number: AC2')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ITMIN, 
     +      MSG='SUB - Failed to read 9th number: ITMIN')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IDSAVE, 
     +      MSG='SUB - Failed to read 10th number: IDSAVE')
      
      CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IDREST, 
     +      MSG='SUB - Failed to read 11th number: IDREST')
      
      IF(IGRID.EQ.1.AND.ISUBLNK.NE.1)
     + CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISUBLNK, 
     +      ERROR_VAL=Z)
      !
      ! CHECK IF GLOBAL SHUTDOWN OF CBC IS IN EFFECT
      CALL CHECK_CBC_GLOBAL_UNIT(IIBSCB)
      !
      IF(AC2.EQ.ZERO) AC2=1.0
      HAS_DELAY_BED  = TRUE
      HAS_INST_BED = TRUE
      IF(NNDB < 1) THEN
                   HAS_INST_BED = FALSE
                   NNDB = Z
      ENDIF
      IF(NDB < 1) THEN
                  HAS_DELAY_BED = FALSE
                  NDB = Z
                  NMZ = Z
                  NN  = Z
      ENDIF
cc
      IF(IGRID.EQ.1.AND.ISUBLNK > Z)then                               !SUB-Linkage rth
       SUBLNK=TRUE                                                    !SUB-Linkage rth
      ELSE                                                              !SUB-Linkage rth
       SUBLNK=FALSE                                                   !SUB-Linkage rth
      ENDIF                                                             !SUB-Linkage rth
      LPFLNK=SUBLNK                                                     !seb removed LPFLNK variable and it is now synonomous with sublnk
!      IF(ILPFLNK > 0)then                                                !SUB-Linkage rth
!       LPFLNK=TRUE                                                      !SUB-Linkage rth
!      ELSE                                                                !SUB-Linkage rth
!       LPFLNK=FALSE                                                     !SUB-Linkage rth
!      ENDIF                                                               !SUB-Linkage rth
      IF(SUBLNK .AND. ANY(LAYCBD.NE.Z)) THEN
          BL%LN='SUB ERROR: SUBLINK IS TURNED ON BUT THERE IS '//
     +         'AT LEAST 1 LAYER WITH A QUASI-CONFINING LAYER '//
     +         'SUBLINK MUST HAVE LAYCBD=0 FORALL LAYERS'
          WRITE(*,*)TRIM(BL%LN)
          CALL USTOP(BL%LN)
      END IF
cc      
        WRITE(IOUT,50) NNDB,NDB,NMZ,NN
   50 FORMAT(/,'         NUMBER OF SYSTEMS OF NO-DELAYED INTERBEDS:',
     1 I3,/,'              NUMBER OF SYSTEMS OF DELAY INTERBEDS:',
     2 I3,/,'                          NUMBER OF MATERIAL ZONES:',
     3 I3,/,'                    NUMBER OF NODES IN EACH STRING:',I3)
      IF(IDSAVE > Z) THEN
         WRITE(IOUT,52) IDSAVE
   52 FORMAT(' RESTART INFORMATION WILL BE SAVED ON UNIT ', I5,
     1 ' FOR DELAY INTERBEDS')
      ELSE
         WRITE(IOUT,53)
   53 FORMAT(' RESTART INFORMATION WILL NOT BE SAVED FOR DELAY',
     1 ' INTERBEDS')
      ENDIF
      IF(IDREST > Z) THEN
         WRITE(IOUT,54) IDREST
   54 FORMAT(' RESTART INFORMATION WILL BE READ FROM UNIT ', I5,
     1 ' FOR DELAY INTERBEDS')
      ELSE
         WRITE(IOUT,55)
   55 FORMAT(' RESTART INFORMATION WILL NOT BE READ FOR DELAY',
     1 ' INTERBEDS')
      ENDIF
C
C4A-----ABORT IF NO LAYERS ARE SPECIFIED FOR INTERBED STORAGE
      IF(.NOT.HAS_INST_BED .AND. .NOT. HAS_DELAY_BED) THEN
       CALL USTOP('NO LAYERS WITH INTERBED STORAGE OF EITHER TYPE '//
     +            'WERE SPECIFIED IN INPUT.')
      ENDIF
C4B-----ABORT IF NO PROPERTY ZONES ARE SPECIFIED
      IF(HAS_DELAY_BED.AND.NMZ.LT.1) THEN
         CALL USTOP('SUB - At least one property zone must '//
     &                 'be specified for delay beds.')
      ENDIF
C4C-----ABORT IF NOT ENOUGH NODES ARE SPECIFIED
      IF(HAS_DELAY_BED.AND.NN.LT.2) THEN
         CALL USTOP('SUB - Number of nodes in strings for '//
     &                 'delay beds (NN) should be at least 2.')
      ENDIF
C
C5------IF CELL-BY-CELL TERMS TO BE SAVED THEN PRINT UNIT NUMBER.
   70 IF(IIBSCB > Z) WRITE(IOUT,80) IIBSCB
   80 FORMAT(1X,'CELL-BY-CELL FLOW TERMS WILL BE SAVED ON UNIT',I3)
C
C5A-----IF OUTPUT CONTROL FOR PRINTING ARRAYS IS SELECTED PRINT MESSAGE.
        IF(ISUBOC > Z) WRITE(IOUT,90)
   90 FORMAT(1X,'OUTPUT CONTROL RECORDS FOR SUB PACKAGE WILL BE ',
     1 'READ EACH TIME STEP.')
C
C6------READ IN MODEL LAYER NUMBERS FOR EACH SYSTEM OF INTERBEDS,
C6------FOR LAYERS WITHOUT DELAY.
      IF(HAS_INST_BED) THEN
         ALLOCATE(LN(NNDB))
         WRITE(IOUT,100) NNDB
  100    FORMAT(/,' MODEL LAYER ASSIGNMENTS FOR EACH OF',I3,' NO-DELAY',
     1  ' SYSTEMS OF INTERBEDS:')
         CALL READ_TO_DATA(TLINE,IN,IOUT, 
     +                          HED="-- READING SUB PACKAGE INPUT --")
         BACKSPACE(IN)
         READ(IN,*) (LN(N),N=1,NNDB)
         WRITE(IOUT,115) (LN(N),N=1,NNDB)
  115    FORMAT(1X,25I4)
         DO 120 N=1,NNDB
         IF(LN(N).GE.1.AND.LN(N).LE.NLAY) GO TO 120
         CALL USTOP('SUB - IMPROPER LAYER ASSIGNMENT FOR NO-DELAY '//
     +              'SYSTEM OF INTERBEDS.')
  120    CONTINUE
      ELSE
         ALLOCATE(LN(1))
      ENDIF
C
C7------READ IN MODEL LAYER NUMBERS FOR EACH SYSTEM OF INTERBEDS,
C7------FOR LAYERS WITH DELAY.
      IF(HAS_DELAY_BED) THEN
       ALLOCATE(LDN(NDB))
         WRITE(IOUT,135) NDB
  135  FORMAT(/,' MODEL LAYER ASSIGNMENTS FOR EACH OF',I3,' DELAY',
     1  ' SYSTEMS OF INTERBEDS:')
       CALL READ_TO_DATA(TLINE,IN,IOUT)
       BACKSPACE(IN)
       READ(IN,*) (LDN(N),N=1,NDB)
         WRITE(IOUT,115) (LDN(N),N=1,NDB)
       DO 140 N=1,NDB
       IF(LDN(N).GE.1.AND.LDN(N).LE.NLAY) GO TO 140
       CALL USTOP('SUB - IMPROPER LAYER ASSIGNMENT FOR DELAY '//
     +            'SYSTEM OF INTERBEDS.')
  140  CONTINUE
      ELSE
       ALLOCATE(LDN(1))
      ENDIF
C
C8------ALLOCATE SPACE FOR THE ARRAYS HC, SCE, SCV, AND SUB.
      NCR=NROW*NCOL
      NND1=NCR*NNDB
      ND1=NCR*NDB
      ND2=Z
C
C8.5---READ PARAMETERS IF NSBP>0 FOR SYSTEMS OF INTERBEDS.
      !SUBP=SUBPARAM(Z,Z,Z,Z,Z,Z,Z,Z,Z,Z,Z)                            !ALL SUBSIDENCE FLAGS SET TO FALSE
      IF(NSBP>Z)THEN
      DO I=1,NSBP
        CALL UPARARRRP(IN,IOUT,NDUM,1,PTYP,1,Z,-1)                      !seb READ IN PARAMETERES --INSTANCES NOT SUPPORTED  
        SELECT CASE(TRIM(PTYP))                                         !seb IF PARAMETER IS ACTIVE SET FLAG TO TRUE TO PREVENT READING OF VARIABLE BY U2DREL
        CASE('SRNB')
          SUBP%SRNB=1
        CASE('NDHC')
          SUBP%NDHC=1
        CASE('SFE' )
          SUBP%SFE=1
        CASE('SFV' )
          SUBP%SFV=1
        CASE('COME')
          SUBP%COME=1
        CASE('COMV')        
          SUBP%COMV=1
        CASE('DSTR')
          SUBP%DSTR=1
        CASE('DHC' )
          SUBP%DHC=1
        CASE('DCME')
          SUBP%DCME=1
        CASE('DCMV')
          SUBP%DCMV=1
        CASE('SDZ' )
          SUBP%SDZ=1
        END SELECT
      END DO
      END IF
C9-----READ IN ARRAY RNB TO SEE HOW MANY STRINGS OF NN CELLS ARE NEEDED.
      IF(HAS_DELAY_BED) THEN
       ALLOCATE(RNB(ND1))
       NNSUM=Z
       DO 190 KQ=1,NDB
       LOC1 = 1+(KQ-1)*NCR                                              !    STARTING LOCATION OF RNB STORAGE FOR CURRENT NONDELAYED BED
       LOC2 = KQ*NCR                                                    !seb ENDING   LOCATION OF RNB STORAGE FOR CURRENT NONDELAYED BED
       LAYNUM=LDN(KQ)
         WRITE(IOUT,144) KQ
 144   FORMAT(/,1X,' SYSTEM',I4,' OF DELAY BEDS:')
C       CALL U2DREL(RNB(LOC1),ANAME(10),NROW,NCOL,LAYNUM,IN,IOUT)
C       CALL U2DREL(BUFF(:,:,1),ANAME(12),NROW,NCOL,LAYNUM,IN,IOUT)     !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,RNB,ND1,LOC1)           !seb REPLACED BY PACK COMMAND
       IF(SUBP%SRNB==1)THEN                                             !seb IF TRUE THEN SUBSITUTE IN PARAMETER VALUE
         CALL UPARARRSUB1(BUFFER,NCOL,NROW,KQ,'SRNB',
     +                    IOUT,ANAME(12),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(12),NROW,NCOL,LAYNUM,IN,IOUT)         !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       RNB(LOC1:LOC2)=PACK(BUFFER, PACK_MASK)                           !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR RNB
       DO 180 N=1,NCR
       IF(RNB(LOC1+N-1).GE.1.0) NNSUM=NNSUM+1
  180  CONTINUE
  190  CONTINUE
       ND2=NN*NNSUM
      ELSE
       ALLOCATE(RNB(1))
      ENDIF
      IF(ND2.LT.1.AND.HAS_DELAY_BED) THEN
           WRITE(IOUT,*) 
         CALL USTOP('SUB - Delay beds were not found in '//
     +             'array specifying numbers of delay beds (RNB).')
      ENDIF
C
C10-----ALLOCATE MEMORY.
      ALLOCATE(OCFLGS(21,NSTPT))
      ALLOCATE(OCLAY(NLAY))
      IF(HAS_INST_BED) THEN
         ALLOCATE(HC(NND1),   SOURCE=NOCOMV)
         ALLOCATE(SCE(NND1),  SOURCE=zero)
         ALLOCATE(SCV(NND1),  SOURCE=zero)
         ALLOCATE(SUB(NND1),  SOURCE=zero)
         ALLOCATE(SUBE(NND1), SOURCE=zero)
         ALLOCATE(SUBV(NND1), SOURCE=zero)
         ALLOCATE(ILSYS(NNDB),SOURCE=0)
      ELSE
         ALLOCATE(HC(1))
         ALLOCATE(SCE(1))
         ALLOCATE(SCV(1))
         ALLOCATE(SUB(1))
         ALLOCATE(SUBE(1))
         ALLOCATE(SUBV(1))
         ALLOCATE(ILSYS(1))
      ENDIF
      IF(HAS_DELAY_BED) THEN
         ALLOCATE(NZ(ND1),    SOURCE=0)
         ALLOCATE(DZ(ND1),    SOURCE=zero)
         ALLOCATE(DCOM(ND1),  SOURCE=zero)
         ALLOCATE(DCOME(ND1), SOURCE=zero)
         ALLOCATE(DCOMV(ND1), SOURCE=zero)
         ALLOCATE(DHP(ND2),   SOURCE=HDRY)
         ALLOCATE(DH(ND2),    SOURCE=HDRY)
         ALLOCATE(DHC(ND2),   SOURCE=NOCOMV)
         ALLOCATE(DP(NMZ,3),  SOURCE=zero)
         ALLOCATE(DVB(NDB,4), SOURCE=zero)
         ALLOCATE(A1(NN),     SOURCE=zero)
         ALLOCATE(A2(NN),     SOURCE=zero)
         ALLOCATE(BB(NN),     SOURCE=zero)
      ELSE
         ALLOCATE(NZ(1))
         ALLOCATE(DZ(1))
         ALLOCATE(DCOM(1))
         ALLOCATE(DCOME(1))
         ALLOCATE(DCOMV(1))
         ALLOCATE(DHP(1))
         ALLOCATE(DH(1))
         ALLOCATE(DHC(1))
         ALLOCATE(DP(1,1))
         ALLOCATE(DVB(1,1))
         ALLOCATE(A1(1))
         ALLOCATE(A2(1))
         ALLOCATE(BB(1))
      ENDIF
cc
      ALLOCATE(DVZ(NCOL,NROW,NLAY))                                     !SUB-Linkage rth
      DVZ=0D0                                                           !wschmid
      ALLOCATE(DVZC(NCOL,NROW,NLAY))                                    !wschmid
      DVZC=0D0                                                          !wschmid
cc   
C
C11-----READ ARRAYS.
      NCR=NROW*NCOL
      ANNI=0.5/(REAL(NN)-.5)
C
C12-----READ RESTART RECORDS IF THIS SIMULATION CONTINUES FROM A
C12-----PREVIOUS SIMULATION
      IF(HAS_DELAY_BED) THEN
       IF(IDREST.GT.Z) THEN
        READ(IDREST) NND2
        IF(NND2.EQ.ND2) THEN
           WRITE(IOUT,242)
  242    FORMAT(' HEAD AND PRECONSOLIDATION HEAD FOR DELAY BEDS ARE',
     1   ' BEING READ FROM RESTART RECORDS')
         READ(IDREST) (DH(N),N=1,ND2)
         READ(IDREST) (DHC(N),N=1,ND2)
         DO 250 N2=1,ND2
         DHP(N2)=DH(N2)
  250    CONTINUE
        ELSE
         CALL USTOP('SUB - HEAD AND PRECONSOLIDATION HEAD FOR DELAY '//
     +              'BEDS CANNOT BE READ FROM RESTART RECORDS')
        ENDIF
       ENDIF
      ENDIF
C
C13-----READ IN ARRAYS FOR SYSTEMS OF NO-DELAY INTERBEDS.
      IF(HAS_INST_BED) THEN
       DO 260 KQ=1,NNDB
       K=LN(KQ)
       LOC1=1+(KQ-1)*NCR
       LOC2=KQ*NCR
       WRITE(IOUT,256) KQ
  256  FORMAT(/,1X,' SYSTEM',I4,' OF NO-DELAY BEDS:')
C       CALL U2DREL(HC(LOC1),ANAME(1),NROW,NCOL,K,IN,IOUT)
C       CALL U2DREL(BUFF(:,:,1),ANAME(1),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,HC,NND1,LOC1)           !seb REPLACED BY PACK COMMAND
       IF(SUBP%NDHC==1)THEN                                             !seb IF TRUE THEN SUBSITUTE IN PARAMETER VALUE 
         CALL UPARARRSUB1(BUFFER,NCOL,NROW,LN(KQ),'NDHC',
     +                    IOUT,ANAME(1),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(1),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       HC(LOC1:LOC2)=PACK(BUFFER,PACK_MASK)                             !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR HC
         WRITE(IOUT,256) KQ
C       CALL U2DREL(SCE(LOC1),ANAME(2),NROW,NCOL,K,IN,IOUT)             
C       CALL U2DREL(BUFF(:,:,1),ANAME(2),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SCE,NND1,LOC1)          !seb REPLACED BY PACK COMMAND
       IF(SUBP%SFE==1)THEN                                              !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
         CALL UPARARRSUB1(BUFFER,NCOL,NROW,LN(KQ),'SFE',
     +                    IOUT,ANAME(2),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(2),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       SCE(LOC1:LOC2)=PACK(BUFFER,PACK_MASK)                            !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR SCE
         WRITE(IOUT,256) KQ
C       CALL U2DREL(SCV(LOC1),ANAME(3),NROW,NCOL,K,IN,IOUT)
C       CALL U2DREL(BUFF(:,:,1),ANAME(3),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SCV,NND1,LOC1)          !seb REPLACED BY PACK COMMAND
       IF(SUBP%SFV==1)THEN                                              !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
         CALL UPARARRSUB1(BUFFER,NCOL,NROW,LN(KQ),'SFV',
     +                    IOUT,ANAME(3),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(3),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       SCV(LOC1:LOC2)=PACK(BUFFER,PACK_MASK)                            !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR SCV
         WRITE(IOUT,256) KQ
C       CALL U2DREL(SUB(LOC1),ANAME(4),NROW,NCOL,K,IN,IOUT)
C       CALL U2DREL(BUFF(:,:,1),ANAME(4),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SUBE,NND1,LOC1)         !seb REPLACED BY PACK COMMAND
       IF(SUBP%COME==1)THEN                                             !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
         CALL UPARARRSUB1(BUFFER,NCOL,NROW,LN(KQ),'COME',
     +                    IOUT,ANAME(4),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(4),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       SUBE(LOC1:LOC2)=PACK(BUFFER,PACK_MASK)                           !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR SUBE
C       CALL U2DREL(BUFF(:,:,1),ANAME(5),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SUBV,NND1,LOC1)         !seb REPLACED BY PACK COMMAND
       IF(SUBP%COMV==1)THEN                                             !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
         CALL UPARARRSUB1(BUFFER,NCOL,NROW,LN(KQ),'COMV',
     +                    IOUT,ANAME(5),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(5),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       SUBV(LOC1:LOC2)=PACK(BUFFER,PACK_MASK)                           !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR SUBV
  260  CONTINUE
C
C14-----INITIALIZE ARRAYS FOR SYSTEMS OF NO-DELAY INTERBEDS.
       DO 280 KQ=1,NNDB
       K  = LN(KQ)
       LB = LBOTM(K)
       NQ=(KQ-1)*NCR
C       NK=(K-1)*NCR
       DO 270 IR=1,NROW
       NQR=NQ+(IR-1)*NCOL
C       NKR=NK+(IR-1)*NCOL
       DO 270 IC=1,NCOL
       LOC2=NQR+IC
C       LOC2H=NKR+IC
C
C15------MULTIPLY STORAGE BY AREA TO GET STORAGE CAPACITY.
       AREA=DELR(IC)*DELC(IR)
       SCE(LOC2)=SCE(LOC2)*AREA
       SCV(LOC2)=SCV(LOC2)*AREA
C15A----SET SUB=SUBE+SUBV (MAY, 2009) SAL
       SUB(LOC2)=SUBE(LOC2)+SUBV(LOC2)
C
C16-----MAKE SURE THAT PRECONSOLIDATION HEAD VALUES
C16-----ARE CONSISTANT WITH STARTING HEADS.
       IF(HC(LOC2) > HNEW(IC,IR,K)) HC(LOC2)=HNEW(IC,IR,K)
       IF(HC(LOC2) < BOTM(IC,IR,LB)) HC(LOC2)=NOCOMV            !seb NO MORE POTENTIAL INELASTIC COMPACTION 
  270  CONTINUE
  280  CONTINUE
      ENDIF
      IF(HAS_DELAY_BED) THEN
C
C17-----READ IN TABLE OF MATERIAL PROPERTIES: K, Sse, Ssv FOR EACH
C17-----OF NMZ ZONES.
         WRITE(IOUT,295)
  295 FORMAT(/,' MATERIAL PROPERTIES OF INTERBEDS WITH DELAY PROPERTIES'
     1 ,//,'   ZONE        HYDRAULIC           ELASTIC            INEL',
     2 'ASTIC       ',/,'  NUMBER      CONDUCTIVITY     SPECIFIC STORA',
     3 'GE    SPECIFIC STORAGE   ',/,' ',69('-'))
       DO N=1, NMZ
          CALL READ_TO_DATA(BL%LN,IN,IOUT)
          LLOC = 1
          CALL GET_NUMBER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,DP(N,1),
     +                    MSG='SUB - Failed to read 1st DP number: '//
     +                        'Vertical Hydraulic Conductivity')
          CALL GET_NUMBER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,DP(N,2),
     +                    MSG='SUB - Failed to read 2nd DP number: '//
     +                        'Elastic Specific Storage')
          CALL GET_NUMBER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,DP(N,3),
     +                    MSG='SUB - Failed to read 3rd DP number: '//
     +                        'Inelastic Specific Storage')
          WRITE(IOUT,305) N, DP(N,1), DP(N,2), DP(N,3)
       END DO
  305  FORMAT(I5,4X,G15.5,5X,G15.5,5X,G15.5)
       LOC3=Z
       LOC4=Z
       DO 380 KQ=1,NDB
       K  = LDN(KQ)
       LB = LBOTM(K)
       LOC1=1+(KQ-1)*NCR
C
C18-----READ IN ARRAYS FOR SYSTEMS OF DELAY INTERBEDS.
       IF(IDREST.LE.Z) THEN
          WRITE(IOUT,308) KQ
 308    FORMAT(/,1X,' SYSTEM',I4,' OF DELAY BEDS:')
        IF(SUBP%DSTR==1)THEN                                            !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
          CALL UPARARRSUB1(BUFFER,NCOL,NROW,KQ,'DSTR',
     +                     IOUT,ANAME(6),IPRNTFLG)
        ELSE
          CALL U2DREL(BUFFER,ANAME(6),NROW,NCOL,K,IN,IOUT)              !seb CHANGED BUFF(:,:,1) TO BUFFER
        END IF
        N1=Z
        DO 320 IR=1,NROW
        DO 320 IC=1,NCOL
        N1=N1+1
        LOC2=LOC1+N1-1
        IF(RNB(LOC2).LT.1.0) GO TO 320
        !
        HH = BUFFER(IC,IR)
        IF(HH < BOTM(IC,IR,LB)) HH = BOTM(IC,IR,LB)
        !
        DO 315 N2=1,NN
        LOC3=LOC3+1
        DHP(LOC3)= HH
        DH(LOC3) = HH
  315   CONTINUE
  320   CONTINUE
          WRITE(IOUT,308) KQ
        IF(SUBP%DHC==1)THEN                                             !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
          CALL UPARARRSUB1(BUFFER,NCOL,NROW,KQ,'DHC',
     +                     IOUT,ANAME(7),IPRNTFLG)
        ELSE
          CALL U2DREL(BUFFER,ANAME(7),NROW,NCOL,K,IN,IOUT)              !seb CHANGED BUFF(:,:,1) TO BUFFER
        END IF
        N1=Z
        DO 330 IR=1,NROW
        DO 330 IC=1,NCOL
        N1=N1+1
        LOC2=LOC1+N1-1
        IF(RNB(LOC2).LT.1.0) GO TO 330
        DO 325 N2=1,NN
        LOC4=LOC4+1
        DHC(LOC4)=BUFFER(IC,IR)  
        !
        IF(DHC(LOC4) > DH(LOC4)) DHC(LOC4)=DH(LOC4)
        !
        IF(DHC(LOC4) < BOTM(IC,IR,LB)) DHC(LOC4) = NOCOMV
  325   CONTINUE
  330   CONTINUE
       ENDIF
         WRITE(IOUT,308) KQ
C       CALL U2DREL(DCOM(LOC1),ANAME(7),NROW,NCOL,K,IN,IOUT)
C       CALL U2DREL(BUFF(:,:,1),ANAME(8),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,DCOME,ND1,LOC1)         !seb REPLACED BY PACK COMMAND
       IF(SUBP%DCME==1)THEN                                             !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
          CALL UPARARRSUB1(BUFFER,NCOL,NROW,KQ,'DCME',
     +                     IOUT,ANAME(8),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(8),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       DCOME(LOC1:KQ*NCR)=PACK(BUFFER,PACK_MASK)                        !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR DCOME  --VARIABLE LOC2=KQ*NCR IN USE ALREADY
         WRITE(IOUT,308) KQ
C       CALL U2DREL(BUFF(:,:,1),ANAME(9),NROW,NCOL,K,IN,IOUT)           !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,DCOMV,ND1,LOC1)         !seb REPLACED BY PACK COMMAND
       IF(SUBP%DCMV==1)THEN                                             !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
          CALL UPARARRSUB1(BUFFER,NCOL,NROW,KQ,'DCMV',
     +                     IOUT,ANAME(9),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(9),NROW,NCOL,K,IN,IOUT)               !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       DCOMV(LOC1:KQ*NCR)=PACK(BUFFER,PACK_MASK)                        !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR DCOMV
         WRITE(IOUT,308) KQ
C       CALL U2DREL(DZ(LOC1),ANAME(8),NROW,NCOL,K,IN,IOUT)
C       CALL U2DREL(BUFF(:,:,1),ANAME(10),NROW,NCOL,K,IN,IOUT)          !seb REPLACED BY BUFFER ARRAY
C       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,DZ,ND1,LOC1)            !seb REPLACED BY PACK COMMAND
       IF(SUBP%SDZ==1)THEN                                              !seb IF TRUE SUBSITUTE IN PARAMETER VALUE
          CALL UPARARRSUB1(BUFFER,NCOL,NROW,KQ,'SDZ',
     +                     IOUT,ANAME(10),IPRNTFLG)
       ELSE
         CALL U2DREL(BUFFER,ANAME(10),NROW,NCOL,K,IN,IOUT)              !seb TEMP STORAGE CHANGED TO BUFFER 
       END IF
       DZ(LOC1:KQ*NCR)=PACK(BUFFER,PACK_MASK)                           !seb STORE SEQUENTIALLY EACH READ OF BUFFER IN VECTOR DZ
         WRITE(IOUT,308) KQ
C       CALL U2DINT(NZ(LOC1),ANAME(9),NROW,NCOL,K,IN,IOUT)
       CALL U2DINT(IBUFF,ANAME(11),NROW,NCOL,K,IN,IOUT)                 !seb fixed bad ANAME reference. Switch to IBUFF
       L=LOC1-1
       DO 335 I=1,NROW
       DO 335 J=1,NCOL
       L=L+1
       NZ(L)=IBUFF(J,I)
 335   CONTINUE
C
C19-----INITIALIZE ARRAYS FOR SYSTEMS OF DELAY INTERBEDS.
       DO 360 NL=1,NCR
       LOC2=LOC1+NL-1
       IF(RNB(LOC2).GE.1.0.AND.DZ(LOC2).LE.ZERO) THEN
         CALL USTOP('A VALUE OF ZERO WAS FOUND IN THE DZ ARRAY WHERE '//
     +         'DELAY INTERBEDS OCCUR.'//NewLine//' MAKE SURE THAT'//
     +         ' DZ IS GREATER THAN 0.0 AT ALL CELLS WHERE RNB '//
     +         ' IS 1.0 OR MORE.')
       ENDIF
       DZ(LOC2)=DZ(LOC2)*ANNI
  360  CONTINUE
       DO 370 N=1,4
       DVB(KQ,N)=ZERO
  370  CONTINUE
  380  CONTINUE
C19A----SET DCOM=DCOME+DCOMV (MAY, 2009) SAL
       DO 382 NN0=1,ND1
       DCOM(NN0)=DCOME(NN0)+DCOMV(NN0)
  382  CONTINUE
      ENDIF
C
C20-----SET ALL FLAGS FOR OUTPUT CONTROL TO "FALSE".
      DO 390 I=1,NSTPT
      DO 385 N=1,21
      OCFLGS(N,I)=FALSE
  385 CONTINUE
  390 CONTINUE
C the following initialization of the NTSSUM array was removed from the
C block IF construct immediatly below and placed here so that it would
C be executed even if ISUBOC is not greater than zero.
C Stan Leake, July 14, 2010
       NTSSUM(1)=Z
       IF(NPER.GT.1) THEN
        DO  N=2,NPER
        NTSSUM(N)=NTSSUM(N-1)+NSTP(N-1)
        ENDDO
       END IF
C
C21-----READ FORMATS AND UNIT NUMBERS OUTPUT FLAGS.
      IF(ISUBOC.GT.Z) THEN
       CALL READ_TO_DATA(BL%LN,IN,IOUT)
       LLOC=1
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(1),
     +            MSG='SUB - Failed to read 1st number: Ifm1')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(1),
     +            MSG='SUB - Failed to read 2nd number: Iun1')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(2),
     +            MSG='SUB - Failed to read 3rd number: Ifm2')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(2),
     +            MSG='SUB - Failed to read 4th number: Iun2')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(3),
     +            MSG='SUB - Failed to read 5th number: Ifm3')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(3),
     +            MSG='SUB - Failed to read 6th number: Iun3')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(4),
     +            MSG='SUB - Failed to read 7th number: Ifm4')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(4),
     +            MSG='SUB - Failed to read 8th number: Iun4')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(5),
     +            MSG='SUB - Failed to read 9th number: Ifm5')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(5),
     +            MSG='SUB - Failed to read 10th number: Iun5')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(6),
     +            MSG='SUB - Failed to read 11th number: Ifm6')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(6),
     +            MSG='SUB - Failed to read 12th number: Iun6')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(7),
     +            MSG='SUB - Failed to read 13th number: Ifm7')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(7),
     +            MSG='SUB - Failed to read 14th number: Iun7')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(8),
     +            MSG='SUB - Failed to read 15th number: Ifm8')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(8),
     +            MSG='SUB - Failed to read 16th number: Iun8')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(9),
     +            MSG='SUB - Failed to read 17th number: Ifm9')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(9),
     +            MSG='SUB - Failed to read 18th number: Iun9')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCF(10),
     +            MSG='SUB - Failed to read 19th number: Ifm10')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISBOCU(10),
     +            MSG='SUB - Failed to read 20th number: Iun10')
         WRITE(IOUT,410) (ISBOCF(N),ISBOCU(N),N=1,10)
  410  FORMAT(/,'             SUBSIDENCE PRINT FORMAT IS NUMBER',I4/
     &            '                 UNIT FOR SAVING SUBSIDENCE IS',I4/
     &            '    COMPACTION BY LAYER PRINT FORMAT IS NUMBER',I4/
     &            '        UNIT FOR SAVING COMPACTION BY LAYER IS',I4/
     &            '   COMPACTION BY SYSTEM PRINT FORMAT IS NUMBER',I4/
     &            '       UNIT FOR SAVING COMPACTION BY SYSTEM IS',I4/
     &            '  VERTICAL DISPLACEMENT PRINT FORMAT IS NUMBER',I4/
     &            '      UNIT FOR SAVING VERTICAL DISPLACEMENT IS',I4/
     &            ' NO-DELAY CRITICAL HEAD PRINT FORMAT IS NUMBER',I4/
     &            '     UNIT FOR SAVING NO-DELAY CRITICAL HEAD IS',I4/
     &            '    DELAY CRITICAL HEAD PRINT FORMAT IS NUMBER',I4/
     &            '        UNIT FOR SAVING DELAY CRITICAL HEAD IS',I4/
     &            '  ELAS COMPACT BY LAYER PRINT FORMAT IS NUMBER',I4/
     &            '      UNIT FOR SAVING ELAS COMPACT BY LAYER IS',I4/
     &            'INELAS COMPACT BY LAYER PRINT FORMAT IS NUMBER',I4/
     &            '   UNIT FOR SAVING INELAS COMPACT BY LAYER  IS',I4/
     &            '  ELAS COMPACT BY SYSTM PRINT FORMAT IS NUMBER',I4/
     &            '      UNIT FOR SAVING ELAS COMPACT BY SYSTM IS',I4/
     &            'INELAS COMPACT BY SYSTM PRINT FORMAT IS NUMBER',I4/
     &            '    UNIT FOR SAVING INELAS COMPACT BY SYSTM IS',I4)
       NTSSUM(1)=Z
       IF(NPER.GT.1) THEN
        DO  N=2,NPER
        NTSSUM(N)=NTSSUM(N-1)+NSTP(N-1)
        ENDDO
       ENDIF
       DO 450 NOCLIN=1,ISUBOC
       CALL READ_TO_DATA(BL%LN,IN,IOUT)
       LLOC=1
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISP1,
     +            MSG='SUB - Failed to read 1st number: ISP1')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,ISP2,
     +            MSG='SUB - Failed to read 2nd number: ISP2')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,JTS1,
     +            MSG='SUB - Failed to read 3rd number: JTS1')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,JTS2,
     +            MSG='SUB - Failed to read 4th number: JTS2')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(1),
     +            MSG='SUB - Failed to read 5th number: IFL1')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(2),
     +            MSG='SUB - Failed to read 6th number: IFL2')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(3),
     +            MSG='SUB - Failed to read 7th number: IFL3')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(4),
     +            MSG='SUB - Failed to read 8th number: IFL4')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(5),
     +            MSG='SUB - Failed to read 9th number: IFL5')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(6),
     +            MSG='SUB - Failed to read 10th number: IFL6')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(7),
     +            MSG='SUB - Failed to read 11th number: IFL7')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(8),
     +            MSG='SUB - Failed to read 12th number: IFL8')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(9),
     +            MSG='SUB - Failed to read 13th number: IFL9')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(10),
     +            MSG='SUB - Failed to read 14th number: IFL10')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(11),
     +            MSG='SUB - Failed to read 15th number: IFL11')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(12),
     +            MSG='SUB - Failed to read 16th number: IFL12')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(13),
     +            MSG='SUB - Failed to read 17th number: IFL13')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(14),
     +            MSG='SUB - Failed to read 18th number: IFL14')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(15),
     +            MSG='SUB - Failed to read 19th number: IFL15')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(16),
     +            MSG='SUB - Failed to read 20th number: IFL16')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(17),
     +            MSG='SUB - Failed to read 21th number: IFL17')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(18),
     +            MSG='SUB - Failed to read 22th number: IFL18')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(19),
     +            MSG='SUB - Failed to read 23th number: IFL19')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(20),
     +            MSG='SUB - Failed to read 24th number: IFL20')
       CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,IOUT,IN,IFL(21),
     +            MSG='SUB - Failed to read 25th number: IFL21')
       IF(ISP1.LT.1) ISP1=1
       IF(ISP1.GT.NPER) ISP1=NPER
       IF(ISP2.LT.1) ISP2=1
       IF(ISP2.GT.NPER) ISP2=NPER
       IF(ISP1.GT.ISP2) ISP1=ISP2
       DO 440 I=ISP1,ISP2
       J1=JTS1
       J2=JTS2
       IF(J1.LT.1) J1=1
       IF(J1.GT.NSTP(I)) J1=NSTP(I)
       IF(J2.LT.1) J2=1
       IF(J2.GT.NSTP(I)) J2=NSTP(I)
       IF(J1.GT.J2) J1=J2
       DO 430 J=J1,J2
       ILOC=NTSSUM(I)+J
       DO 420 N=1,21
       IF(IFL(N).GT.Z) OCFLGS(N,ILOC)=TRUE
       IF(IFL(N).EQ.Z) OCFLGS(N,ILOC)=FALSE
  420  CONTINUE
  430  CONTINUE
  440  CONTINUE
  450  CONTINUE
      ENDIF
C
C22-----RETURN
  500 CALL SGWF2SUB7PSV(IGRID)
      !
      IF(ALLOCATED(PNT_INI_CRIT) .AND. IGRID==1) THEN
          CALL SUB_PRINT_INI_CRIT_HEAD(PNT_INI_CRIT, IN, IOUT, BUFFER)
      END IF
      !
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7ST(KPER,IGRID)
C     ******************************************************************
C        SET PRECONSOLIDATION HEAD (HC AND DHC) EQUAL TO THE STEADY-
C        STATE HEAD IF HEAD IS LOWER THAN PRECONSOLIDATION HEAD.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY:HNEW,NCOL,NROW,ISSFLG,BOTM,LBOTM,IBOUND
      USE GWFSUBMODULE ,ONLY: RNB,LN,LDN,HC,DHP,DH,DHC,
     1                        NNDB,NDB,NN,NOCOMV,
     2                        HAS_DELAY_BED,HAS_INST_BED
C
      IMPLICIT NONE
      INTEGER::KPER,IGRID
      INTEGER::IR,IC,K,KQ,LOC,NCR,LB
      INTEGER::LOC2,LOC3,N1,N2,NQ
      DOUBLE PRECISION::HHNEW,BOT,HTMP
C     ------------------------------------------------------------------
      CALL SGWF2SUB7PNT(IGRID)
C
C1------RETURN IF THIS IS NOT THE SECOND STRESS PERIOD OR IF THE FIRST
C1------STRESS PERIOD WAS TRANSIENT.
      IF(KPER.NE.2) RETURN
      IF(ISSFLG(1).EQ.0) RETURN
C2-----MAKE SURE THAT NO-DELAY PRECONSOLIDATION HEAD VALUES ARE CONSISTENT
C2-----WITH STEADY-STATE HEADS.
      NCR=NROW*NCOL
      IF(HAS_INST_BED) THEN
       DO 20 KQ=1,NNDB
       K  = LN(KQ)
       LB = LBOTM(K)
       LOC=(KQ-1)*NCR
!seb       NQ=(KQ-1)*NCR
C       NK=(K-1)*NCR
       DO 10 IR=1,NROW
!seb       NQR=NQ+(IR-1)*NCOL
C       NKR=NK+(IR-1)*NCOL
       DO 10 IC=1,NCOL
!seb       LOC2=NQR+IC
C       LOC2H=NKR+IC
       LOC=LOC+1                                                        !seb SET POINTER TO COORECT LOCATION FOR IC,IR IN HC
       HHNEW=HNEW(IC,IR,K)
       BOT=BOTM(IC,IR,LB)
       IF(IBOUND(IC,IR,K).EQ.0) HC(LOC)=NOCOMV                          !seb NO MORE INELASTIC COMPACTION WHEN CELL IS DRY
       IF(HHNEW.LT.BOT)         HC(LOC)=NOCOMV                          !seb NO MORE INELASTIC COMPACTION WHEN CELL IS DRY
       IF(HC(LOC).GT.HHNEW)     HC(LOC)=REAL(HHNEW)
   10  CONTINUE
   20  CONTINUE
      ENDIF
C3-----MAKE SURE THAT DELAY PRECONSOLIDATION HEAD VALUES ARE CONSISTENT
C3-----WITH STEADY-STATE HEADS. ALSO, SET HEAD (DH) AND HEAD FOR
C3-----PREVIOUS TIME STEP (DHP) EQUAL TO THE AQUIFER HEAD (HNEW).
      IF(HAS_DELAY_BED) THEN
       LOC3=0
       DO 40 KQ=1,NDB
       K=LDN(KQ)
       NQ=(KQ-1)*NCR
C       NK=(K-1)*NCR
       N1=0
       DO 30 IR=1,NROW
       DO 30 IC=1,NCOL
       N1=N1+1
       LOC2=NQ+N1
C       LOC2H=NK+N1
       HTMP=HNEW(IC,IR,K)
       IF(RNB(LOC2).LT.1.0) GO TO 30
       DO 18 N2=1,NN
       LOC3=LOC3+1
       IF(DHC(LOC3).GT.HTMP) DHC(LOC3)=HTMP
       DH(LOC3)=HTMP
       DHP(LOC3)=HTMP
   18  CONTINUE
   30  CONTINUE
   40  CONTINUE
      ENDIF
C4-----RETURN.
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7AD(KPER,KSTP,Iunitsub,IGRID)
C     ******************************************************************
C     Adjust layer bottoms if Subsidence and Sub-Link used
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,BOTM,IUNIT,GSE,
     1                      LBOTM,LAYCBD,HNEW,SUBLNK,LAYHDT             !added for detecting confining beds - WSCHMID seb MOVED SUBLNK TO GLOBAL
      USE GWFUPWMODULE, ONLY: LAYAVG                                    !Required to update UPW conductances
      USE GWFSUBMODULE, ONLY: DVZ                                       !SUB-Linkage rth
C     ------------------------------------------------------------------
      INTEGER IGRID,KSTP,KPER,K,I,J,IUNITSUB                   !ADDED IUNITSUB WSCHMID
      INTEGER ILPF, IUPW, IHUF, Z
C
      CALL SGWF2BAS7PNT(IGRID)
C
cc--ADJUST BOTM WHENEVER Subsidence is active and Sub-Linkage is used (SUBLNK)
cc--Added to universally adjust layer elevations --rth,wschmid,sal
      IF(SUBLNK)THEN                                                    !ADDED IUNITSUB and SUBLNK rth,WSCHMID
      !
      IF( GSE(1,1)==GSE(1,1) ) THEN
         DO CONCURRENT (J=1:NCOL,I=1:NROW); GSE(J,I)=GSE(J,I)-DVZ(J,I,1)
         END DO
      END IF
      !
      DO K=1,NLAY                                                       !SUB-Linkage rth   
       DO I=1,NROW                                                      !SUB-Linkage rth  
        DO J=1,NCOL                                                     !SUB-Linkage rth  
C2-------IF THE CELL IS EXTERNAL SKIP IT.
        IF (IBOUND(J,I,K).LE.0) GOTO 100
        IF(HNEW(J,I,K).LE.BOTM(J,I,LBOTM(K)))GOTO 100                   !seb ADDED CHECK FOR DRY CELLS, DO NOT ADJUST
           IF(K.EQ.1) THEN                                              !WSCHMID
             BOTM(J,I,LBOTM(K)-1)=BOTM(J,I,LBOTM(K)-1)-DVZ(J,I,K)       !WSCHMID - adjust top of layer 1
           ELSEIF(K.GT.1) THEN
             IF(LAYCBD(K-1).EQ.0) THEN                                  !WSCHMID
               BOTM(J,I,LBOTM(K)-1)=BOTM(J,I,LBOTM(K)-1)-DVZ(J,I,K)     !WSCHMID - if upper layer has no confining bed attached, adjust bottom of next upper layer                !SUB-Linkage rth
             ELSEIF(LAYCBD(K-1).NE.0) THEN                              !WSCHMID
               BOTM(J,I,LBOTM(K)-1)=BOTM(J,I,LBOTM(K)-1)-DVZ(J,I,K)     !WSCHMID - if upper layer has a confining bed attached, adjust bottom of next upper confining bed
               BOTM(J,I,LBOTM(K)-2)=BOTM(J,I,LBOTM(K)-2)-DVZ(J,I,K)     !WSCHMID - if upper layer has a confining bed attached, adjust bottom of next upper aquifer layer
             ENDIF
           ENDIF
 100    Continue
        enddo                                                           !SUB-Linkage rth
       enddo                                                            !SUB-Linkage rth
      enddo                                                             !SUB-Linkage rth
      !
      ILPF=IUNIT(23)
      IHUF=IUNIT(37)
      IUPW=IUNIT(62)
      Z = 0
      DO K=1,NLAY                                                       !IF LAYER IS CONFINED UPDATE ITS CONDUCTANCE
          IF     (LAYHDT(K)==0 .AND. ILPF.NE.0) THEN
                      CALL SGWF2LPF7HCOND(K,Z,Z,Z,Z)                    !NO WETTING SO NO NEED TO PASS ILGR FLAG
          ELSEIF (                   IUPW.NE.0) THEN    
                      IF(LAYAVG(K).EQ.0) THEN
                          IF ( LAYHDT(K).GT.0 ) THEN
                            CALL SGWF2UPW1HHARM(K)
                          ELSE
                            CALL SGWF2UPW1HHARMCON(K)
                          END IF
                      ELSE IF(LAYAVG(K).EQ.1) THEN
                          IF ( LAYHDT(K).GT.0 ) THEN
                            CALL SGWF2UPW1HLOG(K)
                          ELSE
                            CALL SGWF2UPW1HLOGCON(K)
                          END IF
                      ELSE IF(LAYAVG(K).EQ.2) THEN
                          IF ( LAYHDT(K).GT.0 ) THEN
                            CALL SGWF2UPW1HUNCNF(K)
                          ELSE
                            CALL SGWF2UPW1HUNCNFCON(K)
                          END IF
                      END IF
                      CALL SGWF2UPW1VCOND(K)
        ELSEIF (LAYHDT(K)==0 .AND. IHUF.NE.0) THEN
                  CALL USTOP('SUB ERROR: IF USING SUBLINK WITH HUF, '//
     +                       'ALL LAYERS MUST BE SET TO CONVERTABLE.')
        END IF
      END DO
      !
      IF (ILPF.NE.0) THEN
         DO K=1,NLAY-1
           IF (LAYHDT(K)==0 .AND. LAYHDT(K+1)==0) CALL SGWF2LPF7VCOND(K)
         END DO
      END IF
      !
      ENDIF
cc
C4------RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7FM(KPER,KITER,ISIP,IGRID)
C     ******************************************************************
C        ADD INTERBED STORAGE TERMS TO RHS AND HCOF
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS, ONLY: SNGL_ninf
      USE GLOBAL,    ONLY: RHS,HCOF,HNEW,HOLD,IBOUND,DELR,DELC,
     1                     NCOL,NROW,ISSFLG,BOTM,LBOTM
      USE GWFBASMODULE, ONLY: DELT
      USE SIPMODULE,    ONLY: V,HCLOSE
      USE GWFSUBMODULE ,ONLY: RNB,LN,LDN,HC,SCE,SCV,DHP,DH,DHC,NZ,DZ,DP,
     1                        BB,AC1,AC2,ITMIN,NN,
     1                        NDB,NNDB,A1,A2,
     1                        HAS_DELAY_BED,HAS_INST_BED
      IMPLICIT NONE
      INTEGER::KPER,KITER,ISIP,IGRID
      !
      LOGICAL ICHK
      DOUBLE PRECISION::HHNEW,HHOLD,BOT,HCTMP
      REAL::TLED,ZERO,RHO1,RHO2,RNB2,VV,DZZ,SSE,SSV,HHC
      REAL::CI,HAQ,AREA,STRGS,SBGN,SEND,STR1,RATES
      INTEGER::IR,IC,K,NCR,LOC,LOC2,LOC3,LOC4,N,LB
      INTEGER::KQ,NQ,NQR,NZONE,NEND
      INTEGER::L1,L2
C
      DATA ICHK/.FALSE./
C     ------------------------------------------------------------------
      CALL SGWF2SUB7PNT(IGRID)
      ZERO=0.0
C
C0------SKIP CALCULATIONS IF THIS IS A STEADY-STATE STRESS PERIOD.
      IF(ISSFLG(KPER).EQ.1) RETURN
C
C1------INITIALIZE
      TLED=1./DELT
      NCR=NCOL*NROW
C
      IF(HAS_INST_BED) THEN
C2------FIND LAYERS WITH INTERBED STORAGE
       DO KQ=1,NNDB   !110
       K  = LN(KQ)
       LB = LBOTM(K)
!       LOCT=(KQ-1)*NCR
!       N=0
       LOC=(KQ-1)*NCR
       DO IR=1,NROW   !100
       DO IC=1,NCOL
!       N=N+1
!       LOC2=LOCT+N
       LOC=LOC+1
       IF(IBOUND(IC,IR,K).LE.0) CYCLE
C
C3------DETERMINE STORAGE CAPACITIES FOR CELL AT START AND END OF STEP
       RHO1=SCE(LOC)*TLED
       RHO2=RHO1
       HCTMP = HC(LOC)
       HHNEW = HNEW(IC,IR,K)
       HHOLD = HOLD(IC,IR,K)
       BOT   = BOTM(IC,IR,LB)
       !
       IF(HHNEW.LE.BOT .AND. HHOLD.LE.BOT) CYCLE                       ! DO NOT MODIFY RHS AND HCOF IF HNEW ARE LESS THAN THE BOTTOM OF MODEL CELL
       IF(HHOLD < BOT) HHOLD=BOT                                       ! ADDED CHECK FOR WHEN HOLD<BOT THEN SET TO BOT
       !
       IF(HHNEW < HCTMP) THEN
           IF    (HHNEW >= BOT) THEN
                                RHO2=SCV(LOC)*TLED
           ELSEIF(BOT < HCTMP) THEN
                                RHO2=SCV(LOC)*TLED
           ELSE
                                HCTMP = ZERO
           END IF
       ELSE
           HCTMP = ZERO  ! Note that (RHO2-RHO1) = 0, so HCTMP*(RHO2-RHO1) -> 0*(0)
       END IF
C
C4------ADD APPROPRIATE TERMS TO RHS AND HCOF
       !
       IF(HHNEW < BOT) THEN                                             !seb cell is going dry...account for water released as a result from HOLD traveling to bottom of cell
           !
           RHS(IC,IR,K)=RHS(IC,IR,K)-HCTMP*(RHO2-RHO1)-RHO1*HHOLD 
     1                              + RHO2*BOT
           !
       ELSE!IF(HHNEW > BOT) THEN                                         !seb cell is wet do normal SUB calculation
           !
           RHS(IC,IR,K) =RHS(IC,IR,K)-HCTMP*(RHO2-RHO1)-RHO1*HHOLD
           HCOF(IC,IR,K)=HCOF(IC,IR,K)-RHO2
       END IF
       !
  100  END DO
       END DO
  110  END DO
      ENDIF
      !
      IF(HAS_DELAY_BED) THEN
       LOC3=1-NN
       DO 420 KQ=1,NDB
       K  = LDN(KQ)
       LB = LBOTM(K)
       NQ=(KQ-1)*NCR
       DO 410 IR=1,NROW
       NQR=NQ+(IR-1)*NCOL
       DO 410 IC=1,NCOL
       LOC2=NQR+IC
       RNB2=RNB(LOC2)
       IF(RNB2.LT.1.0) GO TO 410
       LOC3=LOC3+NN
       IF(IBOUND(IC,IR,K).LE.0) GO TO 410
       !
       IF(ISIP.NE.0.AND.ICHK) THEN
        VV=V(IC,IR,K)
       ELSE
        VV=ZERO
        ICHK=.TRUE.
       ENDIF
       !
       NZONE=NZ(LOC2)
       DZZ=DZ(LOC2)
       CI=DP(NZONE,1)/DZZ
       IF(ISIP.NE.0) THEN
         IF(KITER.GT.ITMIN.AND.ABS(VV).LT.HCLOSE) GO TO 205
       END IF
C
C5------ASSEMBLE COEFFICIENTS FOR DIRECT SOLUTION OF HEAD IN INTERBED
       !
       HHNEW = HNEW(IC,IR,K)
       BOT   = BOTM(IC,IR,LB)
       !
       IF(HHNEW < BOT) THEN
           HAQ = BOT  +VV*AC1
       ELSE
           HAQ = HHNEW+VV*AC1
       END IF
       !
       SSE = DP(NZONE,2)
       SSV = DP(NZONE,3)
       NEND=LOC3+NN-1                                                   !line added by wschmid to be consistent with mf2005_1.8
       CALL SGWF2SUB7A(HAQ,TLED,CI,SSE,SSV,DZZ,
     1      DH(LOC3:NEND),DHP(LOC3:NEND),DHC(LOC3:NEND),NN)             !:NEND added  by wschmid to be consistent with mf2005_1.8
C
C6------SOLVE FOR HEAD CHANGES IN STRING USING GAUSSIAN ELIMINATION.
C6------ADD CHANGES TO HEAD VALUES TO GET HEAD AT CURRENT ITERATION.
       !CALL SGWF2SUB7S(NN)
       CALL SGWF2SUB7S_FAST(NN,A1,A2,BB)
       !
       L1 = LOC3-1   ! Formerly: LOC3+N-1
       DO N=1, NN
          L1 = L1 + 1
          DH(L1) = DH(L1)+BB(N)*AC2
       END DO
C
C7------CALCULATE STORAGE CHANGE IN INTERBEDS
  205  AREA=DELR(IC)*DELC(IR)
       STRGS=ZERO
       L1=LOC3
       L2=LOC3+NN-1
       DO 210 LOC4=L1,L2
       HHOLD = DHP(LOC4)
       HHNEW = DH(LOC4)
       HHC   = DHC(LOC4)
       !
       IF(HHNEW.LE.BOT .AND. HHOLD.LE.BOT) GO TO 210
C
C8------GET STORAGE CAPACITIES AT BEGINNING AND END OF TIME STEP.
       SBGN=DP(NZ(LOC2),2)
       SEND=SBGN
       !
       IF(HHNEW < HHC) THEN
           IF    (HHNEW >= BOT) THEN
                                SEND=DP(NZ(LOC2),3)
           ELSEIF(BOT < HHC) THEN
                                SEND=DP(NZ(LOC2),3)
           ELSE
                                HHC = ZERO
           END IF
       ELSE
           HHC = ZERO  !Note that (SEND-SBGN) = 0, so HHC*(SEND-SBGN) -> 0*(0)
       END IF
       !
       IF(HHOLD < BOT) HHOLD=BOT
       IF(HHNEW < BOT) HHNEW=BOT
C
C9------CALCULATE VOLUME CHANGE IN INTERBED STORAGE FOR TIME STEP.
       STR1=(HHC*(SEND-SBGN)+SBGN*HHOLD-
     1                 SEND*HHNEW)*DZ(LOC2)*RNB(LOC2)*2.
       IF(LOC4.EQ.L2) STR1=STR1*.5
       STRGS=STRGS+STR1
  210  CONTINUE
       RATES=STRGS*AREA*TLED
C
C10-----ADD APPROPRIATE TERMS TO RHS AND HCOF
       RHS(IC,IR,K)=RHS(IC,IR,K)-RATES
  410  CONTINUE
  420  CONTINUE
      ENDIF
C
C11-----RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR INTERBED STORAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY: IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,HOLD,
     1                        BUFF,DELR,DELC,ISSFLG,BOTM,LBOTM
      USE GWFBASMODULE, ONLY: VBVL,VBNM,MSUM,ICBCFL,DELT
      USE GWFSUBMODULE ,ONLY: RNB,LN,LDN,HC,SCE,SCV,SUB,SUBE,SUBV,DHP,
     1                        DH,DHC,NZ,DZ,DCOM,DCOME,DCOMV,DP,DVB,
     2                        NN,ND2,NDB,NNDB,IIBSCB,
     3                        DVZ,NOCOMV,SEPARTE_FLOWS,          !SUB-Linkage rth 
     4                        HAS_DELAY_BED,HAS_INST_BED
      CHARACTER(16) TEXT(4)
      DOUBLE PRECISION::HHNEW,HHOLD,BOT
      REAL:: STORIN_ELAS, STOROT_ELAS, STORIN_VIRG, STOROT_VIRG
      REAL, DIMENSION(:,:,:), ALLOCATABLE:: BUF2  !ONLY ALLOCATED IF SEPARTE_FLOWS = TRUE
C
      !DATA TEXT(1) /'INST. IB STORAGE'/
      !DATA TEXT(2) /'DELAY IB STORAGE'/
C     ------------------------------------------------------------------
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------SET IF CELL-BY-CELL FLOW TERMS ARE NEEDED.
      IBD=0
      IF(ICBCFL.NE.0 .AND. IIBSCB.GT.0 ) IBD=1
      !
      IF(SEPARTE_FLOWS) THEN
           TEXT(1) = 'SUB INST    ELAS'
           TEXT(2) = 'SUB INST  INELAS'
           TEXT(3) = 'SUB DELAY   ELAS'
           TEXT(4) = 'SUB DELAY INELAS'
           IF(IBD.EQ.1) ALLOCATE(BUF2(NCOL,NROW,NLAY), SOURCE=0.0)
      ELSE
          TEXT(1) = 'INST. IB STORAGE'
          TEXT(2) = 'DELAY IB STORAGE'
      END IF
      !
      ZERO=0.0
      TLED=ZERO
      NCR=NCOL*NROW
      !
      STORIN_ELAS = ZERO 
      STOROT_ELAS = ZERO 
      STORIN_VIRG = ZERO 
      STOROT_VIRG = ZERO
      !
      CALL SGWF2SUB7PNT(IGRID)
      IF(ISSFLG(KPER).EQ.0) TLED=1./DELT
ccrth INITIALIZE Vertical Displacement Array     
        DO  K=1,NLAY                                                    !SUB-Linkage rth
         DO  IR=1,NROW                                                  !SUB-Linkage rth
          DO  IC=1,NCOL                                                 !SUB-Linkage rth
           DVZ(IC,IR,K)=ZERO                                            !SUB-Linkage rth
          enddo                                                         !SUB-Linkage rth
         enddo                                                          !SUB-Linkage rth
        enddo                                                           !SUB-Linkage rth
C
C2------RUN THROUGH EVERY CELL IN THE GRID WITH INTERBED STORAGE.
      IF(HAS_INST_BED) THEN
C
C3-------CELL-BY-CELL FLOW TERMS ARE NEEDED SET IBD AND CLEAR BUFFER.
       IF(IBD.EQ.1) THEN
        DO 90 K=1,NLAY
        DO 90 IR=1,NROW
        DO 90 IC=1,NCOL
        BUFF(IC,IR,K)=ZERO
   90   CONTINUE
       ENDIF
       STOIN=ZERO
       STOUT=ZERO
C
C4------IF THIS IS A STEADY-STATE STRESS PERIOD, SKIP CALCULATIONS
       IF(ISSFLG(KPER).EQ.1) GO TO 111
C
C5------CALCULATE NO-DELAY INTERBED STORAGE CHANGE FOR EACH CELL
       DO KQ=1,NNDB  !110
       K  = LN(KQ)
       LB = LBOTM(K)
       NQ=(KQ-1)*NCR
       LOC=(KQ-1)*NCR                                                   !seb
       DO IR=1,NROW  !100
!       NQR=NQ+(IR-1)*NCOL
       DO IC=1,NCOL  !100
       LOC=LOC+1                                                        !seb
!       LOC2=NQR+IC
C
C6------CALCULATE FLOW FROM STORAGE (VARIABLE HEAD CELLS ONLY)
       IF(IBOUND(IC,IR,K).LE.0) CYCLE !GO TO 100       
       !
       HHOLD = HOLD(IC,IR,K)
       HHNEW = HNEW(IC,IR,K)
       BOT   = BOTM(IC,IR,LB)
       !
       IF(HHNEW.LE.BOT .AND. HHOLD.LE.BOT) CYCLE                        !seb DO NOT MODIFY RHS AND HCOF IF HNEW ARE LESS THAN THE BOTTOM OF MODEL CELL
       !
       IF( HHNEW < BOT ) HHNEW = BOT 
       IF( HHOLD < BOT ) HHOLD = BOT  
       !
       HHC=HC(LOC)
C
C7------GET STORAGE CAPACITIES AT BEGINNING AND END OF TIME STEP.
       SBGN=SCE(LOC)
       SEND=SBGN
       !
       IF(HHNEW < HHC) THEN
                  SEND  = SCV(LOC)
                  STRG  = HHC*(SEND-SBGN)+SBGN*HHOLD-SEND*HHNEW
                  STRGV = SCV(LOC)*(HHC-HHNEW)
                  STRGE = STRG - STRGV
       ELSE
                  STRG  = SBGN*(HHOLD-HHNEW)
                  STRGV = Zero
                  STRGE = STRG
       END IF
C
C9------ACCUMULATE SUBSIDENCE ASSOCIATED WITH CHANGE IN STORAGE
       AREA=DELR(IC)*DELC(IR)
       SUB(LOC) =SUB(LOC)+STRG/AREA
C9A----ACCUMULATE ELASTIC AND INELASTIC COMPACTION SEPARATELY (May, 2009)
       SUBV(LOC)=SUBV(LOC)+STRGV/AREA
       SUBE(LOC)=SUBE(LOC)+STRGE/AREA
C
C10-----IF C-B-C FLOW TERMS ARE TO BE SAVED THEN ADD RATE TO BUFFER.
       IF(STRGE > ZERO) THEN
        STORIN_ELAS = STORIN_ELAS + STRGE
       ELSE
        STOROT_ELAS = STOROT_ELAS - STRGE
       ENDIF
       !
       IF(STRGV > ZERO) THEN
        STORIN_VIRG = STORIN_VIRG + STRGV
       ELSE
        STOROT_VIRG = STOROT_VIRG - STRGV
       ENDIF
       !
       IF(IBD.EQ.1) THEN
         IF(SEPARTE_FLOWS) THEN
           BUFF(IC,IR,K)=BUFF(IC,IR,K)+STRGE*TLED
           BUF2(IC,IR,K)=BUF2(IC,IR,K)+STRGV*TLED
         ELSE
             BUFF(IC,IR,K)=BUFF(IC,IR,K)+STRG*TLED
         END IF
       END IF
C
C11-----SEE IF FLOW IS INTO OR OUT OF STORAGE.
       !IF(STRG.LE.ZERO) THEN
       ! STOUT=STOUT-STRG
       !ELSE
       ! STOIN=STOIN+STRG
       !ENDIF
  100  END DO
       END DO
  110  END DO
C
C12-----IF C-B-C FLOW TERMS WILL BE SAVED CALL UBUDSV TO RECORD THEM.
  111  IF(IBD.EQ.1) THEN
         IF(SEPARTE_FLOWS) THEN
             CALL UBUDSV(KSTP,KPER,TEXT(1),IIBSCB,BUFF,NCOL,
     1                           NROW,NLAY,IOUT)
             CALL UBUDSV(KSTP,KPER,TEXT(2),IIBSCB,BUF2,NCOL,
     1                           NROW,NLAY,IOUT)
         ELSE
             CALL UBUDSV(KSTP,KPER,TEXT(1),IIBSCB,BUFF,NCOL,
     1                           NROW,NLAY,IOUT)
         END IF
       END IF
!  111  IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT(1),IIBSCB,BUFF,NCOL,
!     1                           NROW,NLAY,IOUT)
C
       IF(SEPARTE_FLOWS) THEN
           VBVL(1,MSUM)=VBVL(1,MSUM) + STORIN_ELAS
           VBVL(2,MSUM)=VBVL(2,MSUM) + STOROT_ELAS
           !
           VBVL(3,MSUM)=STORIN_ELAS*TLED
           VBVL(4,MSUM)=STOROT_ELAS*TLED
           VBNM(MSUM)=TEXT(1)
           !
           MSUM=MSUM+1
           !
           VBVL(1,MSUM)=VBVL(1,MSUM) + STORIN_VIRG
           VBVL(2,MSUM)=VBVL(2,MSUM) + STOROT_VIRG
           !
           VBVL(3,MSUM)=STORIN_VIRG*TLED
           VBVL(4,MSUM)=STOROT_VIRG*TLED
           VBNM(MSUM)=TEXT(2)
           MSUM=MSUM+1
       ELSE
C13-----MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
           VBVL(1,MSUM)=VBVL(1,MSUM) + STORIN_ELAS + STORIN_VIRG
           VBVL(2,MSUM)=VBVL(2,MSUM) + STOROT_ELAS + STOROT_VIRG
           !
           VBVL(3,MSUM)=(STORIN_ELAS + STORIN_VIRG)*TLED
           VBVL(4,MSUM)=(STOROT_ELAS + STOROT_VIRG)*TLED
           VBNM(MSUM)=TEXT(1)
C
C14-----INCREMENT BUDGET TERM COUNTER
           MSUM=MSUM+1
       END IF
C
C15-----UPDATE PRECONSOLIDATION HEAD ARRAY
       DO KQ=1,NNDB   !310
       K  = LN(KQ)
       LB = LBOTM(K)
       LOC=(KQ-1)*NCR
!       LOCT=(KQ-1)*NCR
!       N=0
       DO IR=1,NROW  !300
       DO IC=1,NCOL   !300
       LOC=LOC+1
!       N=N+1
!       LOC2=LOCT+N
       HHNEW = HNEW(IC,IR,K)
       BOT   = BOTM(IC,IR,LB)
       IF(IBOUND(IC,IR,K).LE.0 .OR. HHNEW.LE.BOT) THEN !GO TO 300
         HC(LOC)=NOCOMV                                                 !seb NO MORE INELASTIC COMPACTION WHEN CELL IS DRY
         CYCLE
       END IF          
       IF(HHNEW < HC(LOC)) HC(LOC) = HHNEW
  300  END DO
       END DO
  310  END DO
      ENDIF
      !
      IF(HAS_DELAY_BED) THEN
       IF(IBD.EQ.1) THEN
            BUFF=ZERO
            IF(SEPARTE_FLOWS) BUF2 = ZERO
       ENDIF
       !
       STORIN_ELAS = ZERO 
       STOROT_ELAS = ZERO 
       STORIN_VIRG = ZERO 
       STOROT_VIRG = ZERO
C
C16-----IF THIS IS A STEADY-STATE STRESS PERIOD, SKIP CALCULATIONS
       IF(ISSFLG(KPER).EQ.1) GO TO 421
C
C17-----CALCULATE DELAY INTERBED STORAGE CHANGE FOR EACH CELL
       LOC3=1-NN
       DO 420 KQ=1,NDB
       K  = LDN(KQ)
       LB = LBOTM(K)
       NQ = (KQ-1)*NCR
       STRGT=ZERO
       RATBSM=ZERO
       DO 410 IR=1,NROW
       NQR=NQ+(IR-1)*NCOL
       DO 410 IC=1,NCOL
       AREA=DELR(IC)*DELC(IR)
       LOC2=NQR+IC
       RNB2=RNB(LOC2)
       IF(RNB2.LT.1.0) GO TO 410
       LOC3=LOC3+NN
       IF(IBOUND(IC,IR,K).LE.0) GO TO 410
       HD1 = DH(LOC3)
       BOT = BOTM(IC,IR,LB)
C
C18-----CALCULATE CONDUCTANCE BETWEEN AQUIFER AND FIRST CELL IN INTERBED
C18-----ACCOUNTING FOR BOTH HALVES OF RNB(LOC2) BEDS IN SYSTEM
       COND=4.*RNB2*DP(NZ(LOC2),1)*AREA/DZ(LOC2)
C
C19-----CALCULATE THE FLOW RATE INTO THE CELL
       RATB=DBLE(COND)*(DBLE(HD1)-HNEW(IC,IR,K))
       STRGS=ZERO
       STR1VTOT=ZERO 
       STR1ETOT=ZERO
       DZ_RNB = DZ(LOC2)*RNB(LOC2)
       L1=LOC3
       L2=LOC3+NN-1
       DO 401 LOC4=L1,L2
       HHOLD = DHP(LOC4)
       HHNEW = DH(LOC4)
       HHC   = DHC(LOC4)
       !
       IF(HHNEW.LE.BOT .AND. HHOLD.LE.BOT) GO TO 401 
       !
       IF( HHNEW < BOT ) HHNEW = BOT 
       IF( HHOLD < BOT ) HHOLD = BOT 
C
C20-----GET STORAGE CAPACITIES AT BEGINNING AND END OF TIME STEP.
       SBGN=DP(NZ(LOC2),2)
       SEND=SBGN
!
!21-----CALCULATE VOLUME CHANGE IN INTERBED STORAGE FOR TIME STEP.
!      IF(HHNEW.LT.HHC) SEND=DP(NZ(LOC2),3)
!      STR1=(HHC*(SEND-SBGN)+SBGN*HHOLD-
!    1                        SEND*HHNEW)*DZ(LOC2)*RNB(LOC2)*2.
!      IF(LOC4.EQ.L2) STR1=STR1*.5
       !
       IF(HHNEW < HHC) THEN
                  SEND  = DP(NZ(LOC2),3)
                  STR1  = (HHC*(SEND-SBGN)+SBGN*HHOLD-SEND*HHNEW)*DZ_RNB
                  STR1V = SEND*(HHC-HHNEW)*DZ_RNB
                  STR1E = STR1 - STR1V
       ELSE
                  STR1  = SBGN*(HHOLD-HHNEW)*DZ_RNB
                  STR1V = Zero
                  STR1E = STR1
       END IF
       !
       IF(LOC4 < L2) THEN
                  STR1  = STR1*2.
                  STR1E = STR1E*2.
                  STR1V = STR1V*2.
       END IF
       !
       STRGS=STRGS+STR1
       !
       STR1VTOT = STR1VTOT + STR1V
       STR1ETOT = STR1ETOT + STR1E
C
C22-----ACCUMULATE SUBSIDENCE ASSOCIATED WITH CHANGE IN STORAGE
       DCOM(LOC2)=DCOM(LOC2)+STR1
C22A----ACCUMULATE ELASTIC AND INELASTIC COMPACTION SEPARATELY (May, 2009)
       DCOME(LOC2)=DCOME(LOC2)+STR1E
       DCOMV(LOC2)=DCOMV(LOC2)+STR1V
  401  CONTINUE
       !
       STRGS=STRGS*AREA
       STR1VTOT = STR1VTOT*AREA
       STR1ETOT = STR1ETOT*AREA
       !
       STRGT=STRGT+STRGS
       RATS=STRGS*TLED
       !
       IF(STR1ETOT > ZERO) THEN
        STORIN_ELAS = STORIN_ELAS + STR1ETOT
       ELSE
        STOROT_ELAS = STOROT_ELAS - STR1ETOT
       ENDIF
       !
       IF(STR1VTOT > ZERO) THEN
        STORIN_VIRG = STORIN_VIRG + STR1VTOT
       ELSE
        STOROT_VIRG = STOROT_VIRG - STR1VTOT
       ENDIF
       !
       IF(IBD.EQ.1) THEN
         IF(SEPARTE_FLOWS) THEN
           BUFF(IC,IR,K)=BUFF(IC,IR,K)+STR1ETOT*TLED
           BUF2(IC,IR,K)=BUF2(IC,IR,K)+STR1VTOT*TLED
         ELSE
             BUFF(IC,IR,K)=BUFF(IC,IR,K)+STRGS*TLED
         END IF
       END IF
       !
       RATBSM=RATBSM-RATS
       !IF(RATS.LE.ZERO) THEN
       ! RATOUT=RATOUT-RATS
       !ELSE
       ! RATIN=RATIN+RATS
       !ENDIF
       !
C------- If cell has gone dry, then all in-elastic compaction has been relesed
       DO LOC4=L1, L2
           IF ( DH(LOC4).LE.BOT ) DHC(LOC4)=NOCOMV   ! NO MORE INELASTIC COMPACTION WHEN CELL IS DRY
       END DO 
       ! 
  410  CONTINUE
       DVB(KQ,1)=DVB(KQ,1)+STRGT
       DVB(KQ,2)=DVB(KQ,2)+RATBSM*DELT
       DVB(KQ,3)=STRGT*TLED
       DVB(KQ,4)=RATBSM
  420  CONTINUE
C
C23-----IF C-B-C FLOW TERMS WILL BE SAVED CALL UBUDSV TO RECORD THEM.
  421  IF(IBD.EQ.1) THEN
         IF(SEPARTE_FLOWS) THEN
             CALL UBUDSV(KSTP,KPER,TEXT(3),IIBSCB,BUFF,
     1                           NCOL,NROW,NLAY,IOUT)
             CALL UBUDSV(KSTP,KPER,TEXT(4),IIBSCB,BUF2,
     1                           NCOL,NROW,NLAY,IOUT)
         ELSE
             CALL UBUDSV(KSTP,KPER,TEXT(2),IIBSCB,BUFF,
     1                           NCOL,NROW,NLAY,IOUT)
         END IF
      END IF
      !
      IF(SEPARTE_FLOWS) THEN
          VBVL(1,MSUM)=VBVL(1,MSUM) + STORIN_ELAS
          VBVL(2,MSUM)=VBVL(2,MSUM) + STOROT_ELAS
          !
          VBVL(3,MSUM)=STORIN_ELAS*TLED
          VBVL(4,MSUM)=STOROT_ELAS*TLED
          VBNM(MSUM)=TEXT(3)
          !
          MSUM=MSUM+1
          !
          VBVL(1,MSUM)=VBVL(1,MSUM) + STORIN_VIRG
          VBVL(2,MSUM)=VBVL(2,MSUM) + STOROT_VIRG
          !
          VBVL(3,MSUM)=STORIN_VIRG*TLED
          VBVL(4,MSUM)=STOROT_VIRG*TLED
          VBNM(MSUM)=TEXT(4)
          MSUM=MSUM+1
      ELSE
C13-----MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
          VBVL(1,MSUM)=VBVL(1,MSUM) + STORIN_ELAS + STORIN_VIRG
          VBVL(2,MSUM)=VBVL(2,MSUM) + STOROT_ELAS + STOROT_VIRG
          !
          VBVL(3,MSUM)=(STORIN_ELAS + STORIN_VIRG)*TLED
          VBVL(4,MSUM)=(STOROT_ELAS + STOROT_VIRG)*TLED
          VBNM(MSUM)=TEXT(2)
C
C14----INCREMENT BUDGET TERM COUNTER
          MSUM=MSUM+1
      END IF
!  421  IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT(2),IIBSCB,BUFF,
!     1                           NCOL,NROW,NLAY,IOUT)
!C
!C24-----MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
!       VBVL(3,MSUM)=RATIN
!       VBVL(4,MSUM)=RATOUT
!       VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT
!       VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
!       VBNM(MSUM)=TEXT(2)
!C
!C25-----INCREMENT BUDGET TERM COUNTER
!       MSUM=MSUM+1
C
C26-----UPDATE PRECONSOLIDATION HEAD ARRAY, PREVIOUS HEAD ARRAY FOR
C26-----SYSTEMS OF DELAY INTERBEDS.
       DO 500 N=1,ND2
       IF(DH(N).LT.DHC(N)) DHC(N)=DH(N)
       DHP(N)=DH(N)
  500  CONTINUE
      ENDIF
C
C27----RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7OT(KSTP,KPER,IN,IGRID)
C     ******************************************************************
C     PRINT AND STORE SUBSIDENCE, COMPACTION AND CRITICAL HEAD.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY: IOUT,NCOL,NROW,NLAY,NSTP,BUFF,ISSFLG,
     +                      HNEW,BOTM,LBOTM,SUBLNK
      USE GWFBASMODULE, ONLY:PERTIM,TOTIM,HDRY,DELT,
     +                       DATE_SP,HAS_STARTDATE
      USE GWFSUBMODULE ,ONLY: LN,LDN,SUB,SUBE,SUBV,HC,RNB,DCOM,DCOME,
     &                        DCOMV,DHC,DVB,NTSSUM,OCFLGS,
     &                        OCLAY,ILSYS,ISBOCF,ISBOCU,NN,NNDB,NDB,
     &                        LPFLNK,DVZ,                               !SUB-Linkage rth 
     &                        DVZC,DH,DELAY_HED,DELAY_HED_ID,
     &                        HAS_DELAY_BED,HAS_INST_BED
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      CHARACTER(16):: TEXT(13)
      CHARACTER(20):: DATE
      LOGICAL IBDPR
!     DATA TEXT
!    1  /'      SUBSIDENCE',
!    2   'LAYER COMPACTION',
!    3   'NDSYS COMPACTION',   
!    4   ' DSYS COMPACTION',
!    5   '  Z DISPLACEMENT',   
!    6   'ND CRITICAL HEAD',
!    7   ' D CRITICAL HEAD',   
!    8   '   EL LAYER CMPT',
!    9   ' INEL LAYER CMPT',   
!    &   '   NDSYS EL CMPT',
!    1   '    DSYS EL CMPT',   
!    2   ' NDSYS INEL CMPT',
!    3   '  DSYS INEL CMPT'/
C     ------------------------------------------------------------------
      CALL SGWF2SUB7PNT(IGRID)
C
      TEXT(1) = '      SUBSIDENCE'
      TEXT(2) = 'LAYER COMPACTION'
      TEXT(3) = 'NDSYS COMPACTION'
      TEXT(4) = ' DSYS COMPACTION'
      TEXT(5) = '  Z DISPLACEMENT'
      TEXT(6) = 'ND CRITICAL HEAD'
      TEXT(7) = ' D CRITICAL HEAD'
      TEXT(8) = '   EL LAYER CMPT'
      TEXT(9) = ' INEL LAYER CMPT'
      TEXT(10)= '   NDSYS EL CMPT'
      TEXT(11)= '    DSYS EL CMPT'
      TEXT(12)= ' NDSYS INEL CMPT'
      TEXT(13)= '  DSYS INEL CMPT'
      !
      ZERO=0.0
      NCR=NCOL*NROW
C1------INITIALIZE TIME STEP POINTER TO RETRIEVE FLAGS FOR PRINTING AND
C1------SAVING ARRAYS. SET FLAG FOR PRINTING BUDGET FOR DELAY INTERBEDS.
      NNSTP=NTSSUM(KPER)+KSTP
      IBDPR=.FALSE.
      IF(KSTP.EQ.NSTP(KPER).OR.OCFLGS(13,NNSTP)) IBDPR=.TRUE.
      IF(ISSFLG(KPER).EQ.1) IBDPR=.FALSE.
C
C3------PRINT AND STORE SUBSIDENCE, FIRST, CLEAR OUT BUFF.
      IF(OCFLGS(1,NNSTP).OR.OCFLGS(2,NNSTP)) THEN
       DO 30 K=1,NLAY
       DO 30 IR=1,NROW
       DO 30 IC=1,NCOL
       BUFF(IC,IR,K)=ZERO
   30  CONTINUE
C
C4-------SUM COMPACTION IN ALL LAYERS TO GET SUBSIDENCE.
       IF(HAS_INST_BED) THEN
        DO KQ=1,NNDB       !50 
!        LOCT=(KQ-1)*NCR
!        N=0
        K  = LN(KQ)
        !LB = LBOTM(K)
        LOC=(KQ-1)*NCR
        DO IR=1,NROW       !40
        DO IC=1,NCOL       !40
!        N=N+1
!        LOC2=LOCT+N
        LOC=LOC+1
        !HHOLD = HOLD(IC,IR,K)
        !HHNEW = HNEW(IC,IR,K)
        !BOT   = BOTM(IC,IR,LB)
        !IF(IBOUND(IC,IR,K).LE.0) CYCLE
        !IF(HHNEW.LE.BOT.AND.HHOLD.LE.BOT) CYCLE
        !
        BUFF(IC,IR,1)=BUFF(IC,IR,1)+SUB(LOC)
   40   END DO
        END DO
   50   END DO
       ENDIF
       IF(HAS_DELAY_BED) THEN
        DO KQ=1,NDB     !70 
!        LOCT=(KQ-1)*NCR
!        N=0
        K=LDN(KQ)                                                       !Fixed bad reference to LN(Q) for non-delay layering causing out of bounds array reference for delay layer references.
        LOC=(KQ-1)*NCR
        DO IR=1,NROW    !60 
        DO IC=1,NCOL    !60 
!        N=N+1
!        LOC2=LOCT+N
        LOC=LOC+1
        HHNEW=HNEW(IC,IR,K)
        BOT=DBLE(BOTM(IC,IR,LBOTM(K)))
        !IF(IBOUND(IC,IR,K).LE.0) CYCLE
        !IF(HHNEW.LE.BOT) CYCLE
        BUFF(IC,IR,1)=BUFF(IC,IR,1)+DCOM(LOC)
   60   END DO
        END DO
   70   END DO
       ENDIF
C
C5-------PRINT SUBSIDENCE.
       IF(OCFLGS(1,NNSTP)) THEN
        IF(ISBOCF(1).LT.0) CALL ULAPRS(BUFF(:,:,1),TEXT(1),KSTP,KPER,
     1            NCOL,NROW,1,-ISBOCF(1),IOUT)
        IF(ISBOCF(1).GE.0) CALL ULAPRW(BUFF(:,:,1),TEXT(1),KSTP,KPER,
     1            NCOL,NROW,1,ISBOCF(1),IOUT)
       ENDIF
C
C6-------STORE SUBSIDENCE.
       IF(OCFLGS(2,NNSTP)) THEN
        CALL ULASAV(BUFF(:,:,1),TEXT(1),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,1,ISBOCU(1))
       ENDIF
      ENDIF
C
C7------PRINT AND STORE COMPACTION FOR EACH SYSTEM OF INTERBEDS,
C7------INCLUDING DELAY AND NO-DELAY INTERBEDS.
      IF(OCFLGS(5,NNSTP).OR.OCFLGS(6,NNSTP)) THEN
       IF(HAS_INST_BED) THEN
        DO 80 KQ=1,NNDB
        K=LN(KQ)
        LOC2=(KQ-1)*NCR+1
        NEND=LOC2+NCOL*NROW-1                                           !line added by wschmid to be consistent with mf2005_1.8  seb MOVED OUT OF SUBSEQUENT IF
        IF(OCFLGS(5,NNSTP)) THEN
         WRITE(IOUT,76) KQ
  76   FORMAT(/,1X,' SYSTEM',I4,' OF NO-DELAY BEDS:')
        IF(ISBOCF(3).LT.0) CALL ULAPRS(SUB(LOC2:NEND),TEXT(3),KSTP,     !:NEND added by wschmid to be consistent with mf2005_1.8
     1            KPER,NCOL,NROW,K,-ISBOCF(3),IOUT)
        IF(ISBOCF(3).GE.0) CALL ULAPRW(SUB(LOC2:NEND),TEXT(3),KSTP,     !:NEND added by wschmid to be consistent with mf2005_1.8
     1             KPER,NCOL,NROW,K,ISBOCF(3),IOUT)
        ENDIF
        IF(OCFLGS(6,NNSTP)) THEN
         CALL ULASAV(SUB(LOC2:NEND),TEXT(3),KSTP,KPER,PERTIM,TOTIM,     !:NEND added by wschmid to be consistent with mf2005_1.8
     1              NCOL,NROW,KQ,ISBOCU(3))
        ENDIF
   80   CONTINUE
       ENDIF
       IF(HAS_DELAY_BED) THEN
        DO 90 KQ=1,NDB
        K=LDN(KQ)
        LOC2=(KQ-1)*NCR+1
        IF(OCFLGS(5,NNSTP)) THEN
          WRITE(IOUT,82) KQ
  82   FORMAT(/,1X,' SYSTEM',I4,' OF DELAY BEDS:')
         NEND=LOC2+NCOL*NROW-1                                          !line added by wschmid to be consistent with mf2005_1.8 
         IF(ISBOCF(3).LT.0) CALL ULAPRS(DCOM(LOC2:NEND),TEXT(4),KSTP,   !:NEND added by wschmid to be consistent with mf2005_1.8
     1            KPER,NCOL,NROW,K,-ISBOCF(3),IOUT)
         IF(ISBOCF(3).GE.0) CALL ULAPRW(DCOM(LOC2:NEND),TEXT(4),KSTP,   !:NEND added by wschmid to be consistent with mf2005_1.8
     1            KPER,NCOL,NROW,K,ISBOCF(3),IOUT)
        ENDIF
        IF(OCFLGS(6,NNSTP)) THEN
         CALL ULASAV(DCOM(LOC2:NEND),TEXT(4),KSTP,KPER,PERTIM,TOTIM,    !:NEND added by wschmid to be consistent with mf2005_1.8
     1              NCOL,NROW,KQ,ISBOCU(3))
        ENDIF
   90   CONTINUE
       ENDIF
      ENDIF
C7A------PRINT AND STORE ELASTIC COMPACTION FOR EACH SYSTEM OF INTERBEDS,
C7A------INCLUDING DELAY AND NO-DELAY INTERBEDS.
      IF(OCFLGS(18,NNSTP).OR.OCFLGS(19,NNSTP)) THEN
       IF(HAS_INST_BED) THEN
        DO 410 KQ=1,NNDB
        K=LN(KQ)
        !LOC2=(KQ-1)*NCR+1                                               !seb CHANGED DEFINITION FROM LOC2 TO LOC1
        LOC1=1+(KQ-1)*NCR
        LOC2=KQ*NCR
        IF(OCFLGS(18,NNSTP)) THEN
         WRITE(IOUT,76) KQ
        IF(ISBOCF(9).LT.0) CALL ULAPRS(SUBE(LOC1:LOC2),TEXT(10),
     1                     KSTP,KPER,NCOL,NROW,K,-ISBOCF(9),IOUT)       !seb CHANGED SUBE(LOC2) TO SUBE(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
        IF(ISBOCF(9).GE.0) CALL ULAPRW(SUBE(LOC1:LOC2),TEXT(10),        !seb CHANGED SUBE(LOC2) TO SUBE(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1                     KSTP,KPER,NCOL,NROW,K,ISBOCF(9),IOUT)
        ENDIF
        IF(OCFLGS(19,NNSTP)) THEN
         CALL ULASAV(SUBE(LOC1:LOC2),TEXT(10),KSTP,KPER,PERTIM,TOTIM,   !seb CHANGED SUBE(LOC2) TO SUBE(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1               NCOL,NROW,KQ,ISBOCU(9))
        ENDIF
  410   CONTINUE
       ENDIF
       IF(HAS_DELAY_BED) THEN
        DO 420 KQ=1,NDB
        K=LDN(KQ)
        !LOC2=(KQ-1)*NCR+1                                               !seb CHANGED DEFINITION FROM LOC2 TO LOC1
        LOC1=1+(KQ-1)*NCR
        LOC2=KQ*NCR
        IF(OCFLGS(18,NNSTP)) THEN
          WRITE(IOUT,82) KQ
         IF(ISBOCF(9).LT.0) CALL ULAPRS(DCOME(LOC1:LOC2),TEXT(11),      !seb CHANGED DCOME(LOC2) TO DCOME(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1                      KSTP,KPER,NCOL,NROW,K,-ISBOCF(9),IOUT)
         IF(ISBOCF(9).GE.0) CALL ULAPRW(DCOME(LOC1:LOC2),TEXT(11),
     1                      KSTP,KPER,NCOL,NROW,K,ISBOCF(9),IOUT)       !seb CHANGED DCOME(LOC2) TO DCOME(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
        ENDIF
        IF(OCFLGS(19,NNSTP)) THEN
         CALL ULASAV(DCOME(LOC1:LOC2),TEXT(11),KSTP,KPER,PERTIM,        !seb CHANGED DCOME(LOC2) TO DCOME(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1               TOTIM,NCOL,NROW,KQ,ISBOCU(9))
        ENDIF
  420   CONTINUE
       ENDIF
      ENDIF
C7B------PRINT AND STORE INELASTIC COMPACTION FOR EACH SYSTEM OF INTERBEDS,
C7B------INCLUDING DELAY AND NO-DELAY INTERBEDS.
C*****WORK HERE... change ocflgs and isbocf numbers
      IF(OCFLGS(20,NNSTP).OR.OCFLGS(21,NNSTP)) THEN
       IF(HAS_INST_BED) THEN
        DO 430 KQ=1,NNDB
        K=LN(KQ)
        !LOC2=(KQ-1)*NCR+1                                               !seb CHANGED DEFINITION FROM LOC2 TO LOC1
        LOC1=1+(KQ-1)*NCR
        LOC2=KQ*NCR
        IF(OCFLGS(20,NNSTP)) THEN
         WRITE(IOUT,76) KQ
        IF(ISBOCF(10).LT.0) CALL ULAPRS(SUBV(LOC1:LOC2),TEXT(12),       !seb CHANGED SUBV(LOC2) TO SUBV(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1                           KSTP,KPER,NCOL,NROW,K,-ISBOCF(10),IOUT)
        IF(ISBOCF(10).GE.0) CALL ULAPRW(SUBV(LOC1:LOC2),TEXT(12),       !seb CHANGED SUBV(LOC2) TO SUBV(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1                           KSTP,KPER,NCOL,NROW,K,ISBOCF(10),IOUT)
        ENDIF
        IF(OCFLGS(21,NNSTP)) THEN
         CALL ULASAV(SUBV(LOC1:LOC2),TEXT(12),KSTP,KPER,PERTIM,TOTIM,   !seb CHANGED SUBV(LOC2) TO SUBV(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1               NCOL,NROW,KQ,ISBOCU(10))
        ENDIF
  430   CONTINUE
       ENDIF
       IF(HAS_DELAY_BED) THEN
        DO 440 KQ=1,NDB
        K=LDN(KQ)
        !LOC2=(KQ-1)*NCR+1                                               !seb CHANGED DEFINITION FROM LOC2 TO LOC1
        LOC1=1+(KQ-1)*NCR
        LOC2=KQ*NCR
        IF(OCFLGS(20,NNSTP)) THEN
          WRITE(IOUT,82) KQ
         IF(ISBOCF(10).LT.0) CALL ULAPRS(DCOMV(LOC1:LOC2),TEXT(13),     !seb CHANGED DCOMV(LOC2) TO DCOMV(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1                           KSTP,KPER,NCOL,NROW,K,-ISBOCF(10),IOUT)
         IF(ISBOCF(10).GE.0) CALL ULAPRW(DCOMV(LOC1:LOC2),TEXT(13),     !seb CHANGED DCOMV(LOC2) TO DCOMV(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1                            KSTP,KPER,NCOL,NROW,K,ISBOCF(10),IOUT)
        ENDIF
        IF(OCFLGS(21,NNSTP)) THEN
         CALL ULASAV(DCOMV(LOC1:LOC2),TEXT(13),KSTP,KPER,PERTIM,TOTIM,  !seb CHANGED DCOMV(LOC2) TO DCOMV(LOC1:LOC2) DUE TO REDEFINITION OF LOC2 AND TO PREVENT SPURIOUS RAM EFFECTS
     1               NCOL,NROW,KQ,ISBOCU(10))
        ENDIF
  440   CONTINUE
       ENDIF
      ENDIF
C
C8------SUM COMPACTION IN EACH LAYER IN THE BUFF ARRAY FOR SAVING
C8------OR PRINTING COMPACTION OR VERTICAL DISPLACEMENT BY MODEL
C8------LAYER. FIRST, CLEAR OUT BUFF.
      IF(OCFLGS(3,NNSTP).OR.OCFLGS(4,NNSTP).OR.
     & OCFLGS(7,NNSTP).OR.OCFLGS(8,NNSTP).OR.                           !wschmid: clear buffer before DVZ is set equal to buffer
     &  (SUBLNK.OR.LPFLNK)) THEN
       DO NL=1,NLAY
       OCLAY(NL)=.FALSE.
       ENDDO
       DO K=1,NLAY
       DO IR=1,NROW
       DO IC=1,NCOL
       BUFF(IC,IR,K)=ZERO
       ENDDO
       ENDDO
       ENDDO
C
C9-------SUM NO-DELAY COMPACTION IN ALL MODEL LAYERS.
       IF(HAS_INST_BED) THEN
        DO KQ=1,NNDB
        K  = LN(KQ)
        !LB = LBOTM(K)
        OCLAY(K)=.TRUE.
!        LOCT=(KQ-1)*NCR
!        N=0
        LOC=(KQ-1)*NCR
        DO IR=1,NROW
        DO IC=1,NCOL
!        N=N+1
!        LOC2=LOCT+N
        LOC=LOC+1
        !HHOLD = HOLD(IC,IR,K)
        !HHNEW = HNEW(IC,IR,K)
        !BOT   = BOTM(IC,IR,LB)
        !IF(IBOUND(IC,IR,K).LE.0) CYCLE
        !IF(HHNEW.LE.BOT.AND.HHOLD.LE.BOT) CYCLE
        BUFF(IC,IR,K)=BUFF(IC,IR,K)+SUB(LOC)
        ENDDO
        ENDDO
       ENDDO
       ENDIF
C
C10------SUM DELAY COMPACTION IN ALL MODEL LAYERS.
       IF(HAS_DELAY_BED) THEN
        DO KQ=1,NDB
        K=LDN(KQ)
        OCLAY(K)=.TRUE.
!        LOCT=(KQ-1)*NCR
!        N=0
        LOC=(KQ-1)*NCR
        DO IR=1,NROW
        DO IC=1,NCOL
!        N=N+1
!        LOC2=LOCT+N
        LOC=LOC+1
        !HHNEW=HNEW(IC,IR,K)
        !BOT=DBLE(BOTM(IC,IR,LBOTM(K)))
        !IF(IBOUND(IC,IR,K).LE.0) CYCLE
        !IF(HHNEW.LE.BOT) CYCLE
        BUFF(IC,IR,K)=BUFF(IC,IR,K)+DCOM(LOC)
        ENDDO
        ENDDO
       ENDDO
       ENDIF
      ENDIF
C
C11-----PRINT COMPACTION BY LAYER.
      IF(OCFLGS(3,NNSTP)) THEN
       DO KL=1,NLAY
       IF(.NOT.OCLAY(KL)) CYCLE
       KKL=KL
       IF(ISBOCF(2).LT.0) CALL ULAPRS(BUFF(:,:,KL),TEXT(2),KSTP,KPER,
     1           NCOL,NROW,KKL,-ISBOCF(2),IOUT)
       IF(ISBOCF(2).GE.0) CALL ULAPRW(BUFF(:,:,KL),TEXT(2),KSTP,KPER,
     1            NCOL,NROW,KKL,ISBOCF(2),IOUT)
       ENDDO
      ENDIF
C
C12-----STORE COMPACTION BY LAYER.
      IF(OCFLGS(4,NNSTP)) THEN
       DO KL=1,NLAY
       IF(.NOT.OCLAY(KL)) CYCLE
       KKL=KL
       CALL ULASAV(BUFF(:,:,KL),TEXT(2),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1             NROW,KKL,ISBOCU(2))
       ENDDO
      ENDIF
C
C13----CALCULATE VERTICAL DISPLACEMENT.
      IF(OCFLGS(7,NNSTP).OR.OCFLGS(8,NNSTP).OR.
     &  (SUBLNK.OR.LPFLNK)) THEN
       NL1=NLAY-1
       IF(NLAY.GT.1) THEN
        DO KL=NL1,1,-1      !140 
!        LOCT3=(KL-1)*NCR                                               !seb index is never needed
!        N=0
        DO IR=1,NROW        !135 
        DO IC=1,NCOL        !135 
!        N=N+1
        IF(KL.EQ.NL1) THEN
        DVZ(IC,IR,NLAY)=BUFF(IC,IR,NLAY)-DVZC(IC,IR,NLAY)        
        DVZC(IC,IR,NLAY)=BUFF(IC,IR,NLAY)                               !wschmid: define Z displacement in bottom layer!
        ENDIF
        BUFF(IC,IR,KL)=BUFF(IC,IR,KL)+BUFF(IC,IR,KL+1)                  !condition already done further up
c        IF(SUBLNK.OR.LPFLNK)THEN                                       !SUB-Linkage rth
        DVZ(IC,IR,KL)=BUFF(IC,IR,KL)-DVZC(IC,IR,KL) 
        DVZC(IC,IR,KL)=BUFF(IC,IR,KL)                                  !SUB-Linkage rth
c        ELSE                                                           !SUB-Linkage rth
c         DVZ(IC,IR,KL)=ZERO                                            !SUB-Linkage rth   !wschmid: anyway initialized in allcate!
c        ENDIF                                                          !SUB-Linkage rth
  135   END DO
        END DO
  140   END DO
       ENDIF
C
C14-----PRINT VERTICAL DISPLACEMENT FOR ALL MODEL LAYERS.
       IF(OCFLGS(7,NNSTP)) THEN
        DO 145 KL=1,NLAY
        KKL=KL
        IF(ISBOCF(4).LT.0) CALL ULAPRS(BUFF(:,:,KL),TEXT(5),KSTP,KPER,
     1            NCOL,NROW,KKL,-ISBOCF(4),IOUT)
        IF(ISBOCF(4).GE.0) CALL ULAPRW(BUFF(:,:,KL),TEXT(5),KSTP,KPER,
     1             NCOL,NROW,KKL,ISBOCF(4),IOUT)
  145   CONTINUE
       ENDIF
C
C15-----SAVE VERTICAL DISPLACEMENT FOR ALL MODEL LAYERS.
       IF(OCFLGS(8,NNSTP)) THEN
        DO 150 KL=1,NLAY
        KKL=KL
        CALL ULASAV(BUFF(:,:,KL),TEXT(5),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,KKL,ISBOCU(4))
  150   CONTINUE
       ENDIF
      ENDIF
C
C16-----PRINT CRITICAL HEAD FOR SYSTEMS OF NO-DELAY INTERBEDS.
C16-----STORAGE.
      IF(OCFLGS(9,NNSTP).OR.OCFLGS(10,NNSTP)) THEN
       IF(HAS_INST_BED) THEN
       DO 155 NL=1,NLAY
       OCLAY(NL)=.TRUE.
  155  CONTINUE
        DO 160 KQ=1,NNDB
        K=LN(KQ)
        LOC2=(KQ-1)*NCR+1
        IF(.NOT.OCLAY(K)) GO TO 160
        NSYS=1
        ILSYS(1)=KQ
        IF(KQ.LT.NNDB) THEN
         KS1=KQ+1
         DO 152 KS=KS1,NNDB
         IF(LN(KS).EQ.K) THEN
          NSYS=NSYS+1
          ILSYS(NSYS)=KS
         ENDIF
  152    CONTINUE
        ENDIF
        OCLAY(K)=.FALSE.
        IF(OCFLGS(9,NNSTP)) THEN
           WRITE(IOUT,154) (ILSYS(NS),NS=1,NSYS)
  154  FORMAT(/,1X,' SYSTEM OR SYSTEMS OF NO-DELAY BEDS:',20I3)
         NEND=LOC2+NCOL*NROW-1                                          !line added by wschmid to be consistent with mf2005_1.8 
         IF(ISBOCF(5).LT.0) CALL ULAPRS(HC(LOC2:NEND),TEXT(6),KSTP,KPER,!:NEND added by wschmid to be consistent with mf2005_1.8 
     1            NCOL,NROW,K,-ISBOCF(5),IOUT)
         IF(ISBOCF(5).GE.0) CALL ULAPRW(HC(LOC2:NEND),TEXT(6),KSTP,KPER,!:NEND added by wschmid to be consistent with mf2005_1.8
     1             NCOL,NROW,K,ISBOCF(5),IOUT)
        ENDIF
        IF(OCFLGS(10,NNSTP)) THEN
         NEND=LOC2+NCOL*NROW-1                                          !seb mssing definition of NEND
         CALL ULASAV(HC(LOC2:NEND),TEXT(6),KSTP,KPER,PERTIM,TOTIM,NCOL, !:NEND added by wschmid to be consistent with mf2005_1.8
     1            NROW,K,ISBOCU(5))
        ENDIF
  160   CONTINUE
       ENDIF
      ENDIF
C
C17-----PRINT CRITICAL HEAD FOR ALL SYSTEMS OF DELAY INTERBED.
      IF(OCFLGS(11,NNSTP).OR.OCFLGS(12,NNSTP)) THEN
       IF(HAS_DELAY_BED) THEN
        LOC4=0
        DO 190 KQ=1,NDB
        K=LDN(KQ)
        LOCT=(KQ-1)*NCR
        N=0
        DO 180 IR=1,NROW
        DO 180 IC=1,NCOL
        N=N+1
        BUFF(IC,IR,1)=ZERO
        LOC2=LOCT+N
        IF(RNB(LOC2).LT.1.0) GO TO 180
        LOC4=LOC4+NN
        BUFF(IC,IR,1)=DHC(LOC4)
  180   CONTINUE
        IF(OCFLGS(11,NNSTP)) THEN
           WRITE(IOUT,82) KQ
         IF(ISBOCF(6).LT.0) CALL ULAPRS(BUFF(:,:,1),TEXT(7),KSTP,KPER,
     1             NCOL,NROW,K,-ISBOCF(6),IOUT)
         IF(ISBOCF(6).GE.0) CALL ULAPRW(BUFF(:,:,1),TEXT(7),KSTP,KPER,
     1              NCOL,NROW,K,ISBOCF(6),IOUT)
        ENDIF
        IF(OCFLGS(12,NNSTP)) THEN
         CALL ULASAV(BUFF(:,:,1),TEXT(7),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1            NROW,KQ,ISBOCU(6))
        ENDIF
  190   CONTINUE
       ENDIF
      ENDIF
C
C18-----PRINT VOLUMETRIC BUDGET FOR SYSTEMS OF DELAY INTERBEDS
      IF(HAS_DELAY_BED.AND.IBDPR) THEN
         WRITE(IOUT,230) KSTP,KPER
       SUMSV=ZERO
       SUMBV=ZERO
       SUMSR=ZERO
       SUMBR=ZERO
       DO 200 KQ=1,NDB
       DISCV=ZERO
       DISCR=ZERO
       SUMV=DVB(KQ,1)+DVB(KQ,2)
       SUMR=DVB(KQ,3)+DVB(KQ,4)
       SUMSV=SUMSV+DVB(KQ,1)
       SUMBV=SUMBV+DVB(KQ,2)
       SUMSR=SUMSR+DVB(KQ,3)
       SUMBR=SUMBR+DVB(KQ,4)
       IF(DVB(KQ,1).NE.ZERO) DISCV=100.*SUMV/DVB(KQ,1)
       IF(DVB(KQ,3).NE.ZERO) DISCR=100.*SUMR/DVB(KQ,3)
         WRITE(IOUT,240) KQ,DVB(KQ,1),DVB(KQ,2),SUMV,DISCV,
     1                    DVB(KQ,3),DVB(KQ,4),SUMR,DISCR
  200  CONTINUE
       IF(NDB.GT.1) THEN
        DISCV=ZERO
        DISCR=ZERO
        SUMV=SUMSV+SUMBV
        SUMR=SUMSR+SUMBR
        IF(SUMSV.NE.ZERO) DISCV=100.*SUMV/SUMSV
        IF(SUMSR.NE.ZERO) DISCR=100.*SUMR/SUMSR
          WRITE(IOUT,250) SUMSV,SUMBV,SUMV,DISCV,
     1                  SUMSR,SUMBR,SUMR,DISCR
       ENDIF
      ENDIF
C
C19------SUM ELASTIC COMPACTION IN EACH LAYER IN THE BUFF ARRAY FOR 
C19------SAVING OR PRINTING ELASTIC COMPACTION BY MODEL LAYER.
C19------FIRST, CLEAR OUT BUFF.
      IF(OCFLGS(14,NNSTP).OR.OCFLGS(15,NNSTP)) THEN
       DO NL=1,NLAY
       OCLAY(NL)=.FALSE.
       ENDDO
       DO K=1,NLAY
       DO IR=1,NROW
       DO IC=1,NCOL
       BUFF(IC,IR,K)=ZERO
       ENDDO
       ENDDO
       ENDDO
C
C20-------SUM NO-DELAY ELASTIC COMPACTION IN ALL MODEL LAYERS.
       IF(HAS_INST_BED) THEN
        DO KQ=1,NNDB
        K=LN(KQ)
        OCLAY(K)=.TRUE.
        LOCT=(KQ-1)*NCR
        N=0
        DO IR=1,NROW
        DO IC=1,NCOL
        N=N+1
        LOC2=LOCT+N
        BUFF(IC,IR,K)=BUFF(IC,IR,K)+SUBE(LOC2)
        ENDDO
        ENDDO
       ENDDO
       ENDIF
C
C21------SUM DELAY ELASTIC COMPACTION IN ALL MODEL LAYERS.
       IF(HAS_DELAY_BED) THEN
        DO KQ=1,NDB
        K=LDN(KQ)
        OCLAY(K)=.TRUE.
        LOCT=(KQ-1)*NCR
        N=0
        DO IR=1,NROW
        DO IC=1,NCOL
        N=N+1
        LOC2=LOCT+N
        BUFF(IC,IR,K)=BUFF(IC,IR,K)+DCOME(LOC2)
        ENDDO
        ENDDO
       ENDDO
       ENDIF
      ENDIF
C
C22-----PRINT ELASTIC COMPACTION BY LAYER.
      IF(OCFLGS(14,NNSTP)) THEN
       DO KL=1,NLAY
       IF(.NOT.OCLAY(KL)) CYCLE
       KKL=KL
       IF(ISBOCF(7).LT.0) CALL ULAPRS(BUFF(:,:,KL),TEXT(8),KSTP,KPER,
     1           NCOL,NROW,KKL,-ISBOCF(7),IOUT)
       IF(ISBOCF(7).GE.0) CALL ULAPRW(BUFF(:,:,KL),TEXT(8),KSTP,KPER,
     1            NCOL,NROW,KKL,ISBOCF(7),IOUT)
       ENDDO
      ENDIF
C
C23-----STORE ELASTIC COMPACTION BY LAYER.
      IF(OCFLGS(15,NNSTP)) THEN
       DO KL=1,NLAY
       IF(.NOT.OCLAY(KL)) CYCLE
       KKL=KL
       CALL ULASAV(BUFF(:,:,KL),TEXT(8),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1             NROW,KKL,ISBOCU(7))
       ENDDO
      ENDIF
C
C24------SUM INELASTIC COMPACTION IN EACH LAYER IN THE BUFF ARRAY FOR 
C24------SAVING OR PRINTING INELASTIC COMPACTION BY MODEL LAYER.
C24------FIRST, CLEAR OUT BUFF.
      IF(OCFLGS(16,NNSTP).OR.OCFLGS(17,NNSTP)) THEN
       DO NL=1,NLAY
       OCLAY(NL)=.FALSE.
       ENDDO
       DO K=1,NLAY
       DO IR=1,NROW
       DO IC=1,NCOL
       BUFF(IC,IR,K)=ZERO
       ENDDO
       ENDDO
       ENDDO
C
C25-------SUM NO-DELAY INELASTIC COMPACTION IN ALL MODEL LAYERS.
       IF(HAS_INST_BED) THEN
        DO KQ=1,NNDB
        K=LN(KQ)
        OCLAY(K)=.TRUE.
!        LOCT=(KQ-1)*NCR
!        N=0
        LOC=(KQ-1)*NCR
        DO IR=1,NROW
        DO IC=1,NCOL
!        N=N+1
!        LOC2=LOCT+N
        LOC=LOC+1
        !HHOLD=HOLD(IC,IR,K)
        !HHNEW=HNEW(IC,IR,K)
        !BOT=DBLE(BOTM(IC,IR,LBOTM(K)))
        !IF(IBOUND(IC,IR,K).LE.0) CYCLE
        !IF(HHNEW.LE.BOT.AND.HHOLD.LE.BOT) CYCLE
        BUFF(IC,IR,K)=BUFF(IC,IR,K)+SUBV(LOC)
        ENDDO
        ENDDO
       ENDDO
       ENDIF
C
C26------SUM DELAY INELASTIC COMPACTION IN ALL MODEL LAYERS.
       IF(HAS_DELAY_BED) THEN
        DO KQ=1,NDB
        K=LDN(KQ)
        OCLAY(K)=.TRUE.
        LOCT=(KQ-1)*NCR
        N=0
        DO IR=1,NROW
        DO IC=1,NCOL
        N=N+1
        LOC2=LOCT+N
        BUFF(IC,IR,K)=BUFF(IC,IR,K)+DCOMV(LOC2)
        ENDDO
        ENDDO
       ENDDO
       ENDIF
      ENDIF
C
C27-----PRINT INELASTIC COMPACTION BY LAYER.
      IF(OCFLGS(16,NNSTP)) THEN
       DO KL=1,NLAY
       IF(.NOT.OCLAY(KL)) CYCLE
       KKL=KL
       IF(ISBOCF(8).LT.0) CALL ULAPRS(BUFF(:,:,KL),TEXT(9),KSTP,KPER,
     1           NCOL,NROW,KKL,-ISBOCF(8),IOUT)
       IF(ISBOCF(8).GE.0) CALL ULAPRW(BUFF(:,:,KL),TEXT(9),KSTP,KPER,
     1            NCOL,NROW,KKL,ISBOCF(8),IOUT)
       ENDDO
      ENDIF
C
C28-----STORE INELASTIC COMPACTION BY LAYER.
      IF(OCFLGS(17,NNSTP)) THEN
       DO KL=1,NLAY
       IF(.NOT.OCLAY(KL)) CYCLE
       KKL=KL
       CALL ULASAV(BUFF(:,:,KL),TEXT(9),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1             NROW,KKL,ISBOCU(8))
       ENDDO
      ENDIF
C29 WRITE OUT ANY Delay Heads
      !
      IF(DELAY_HED(1)%IS_OPEN) THEN
        DO NF=1, SIZE(DELAY_HED)
         !
         ASSOCIATE(KQ_PRNT=>DELAY_HED_ID(1,NF),  !1 = KQ, 2 = SPSTART, 3 = SPSTOP, 4 = ONLY SP PRINT
     +             SP_ONLY=>DELAY_HED_ID(2,NF),
     +             STRT   =>DELAY_HED_ID(3,NF),
     +             STP    =>DELAY_HED_ID(4,NF))
          IF(KPER < STRT  .OR.  KPER > STP     ) CYCLE ! NF=1, SIZE(DELAY_HED)
          IF(SP_ONLY == 1 .AND. KSTP<NSTP(KPER)) CYCLE ! NF=1, SIZE(DELAY_HED)
          !
          IF(KQ_PRNT == 0) THEN  !GET ALL HEAD VALUES
              LOC4=0
              DO KQ=1,NDB
                DO IR=1, NROW
                DO IC=1, NCOL
                         BUFF(IC,IR,1) = HDRY
                END DO
                END DO
                K=LDN(KQ)
                LOCT=(KQ-1)*NCR
                N=0
                DO IR=1,NROW
                DO IC=1,NCOL
                   N=N+1
                   LOC2=LOCT+N
                   IF(RNB(LOC2).GE.1.0) THEN
                      LOC4=LOC4+NN
                      BUFF(IC,IR,1)=DH(LOC4)
                   END IF
                END DO
                END DO
                !
                ASSOCIATE( IU=>DELAY_HED(NF)%IU )
                IF(DELAY_HED(NF)%BINARY) THEN
                    IF(K<1000)THEN
                        TEXT(1)='DBED_HEAD_LAY'//NUM2STR(K,3,.TRUE.)
                    ELSE
                        TEXT(1)='DBED_HEAD_'   //NUM2STR(K,6,.TRUE.)
                    END IF
                    !
                    CALL ULASAV(BUFF(:,:,1),TEXT(1),KSTP,KPER,PERTIM,
     1                          TOTIM,NCOL,NROW,KQ,IU)
                ELSE
                    IF(HAS_STARTDATE) THEN
                        DATE=DATE_SP(KPER)%TS(KSTP)%STR(' ')
                    ELSE
                        DATE=' '
                    END IF
                       WRITE(IU,'(2I6,2(1x,ES15.7),A,3I8,A,I8,2(4x,A))')
     +                       KPER, KSTP, DELT, TOTIM,' DBED_HEAD ',
     +                       NCOL,NROW,K, ' NDB_ID ', KQ,
     +                       '(*(1x,ES15.7))', TRIM(DATE)
                       DO IR=1,NROW
                         WRITE(IU,'(*(1x,ES15.7))') BUFF(:,IR,1)
                       END DO
                END IF
                END ASSOCIATE
              END DO
          ELSE                    !SPECIFED INTERBED
              BUFF(:,:,1)=HDRY
              LOC4=0
              DO KQ=1,KQ_PRNT
                K=LDN(KQ)
                LOCT=(KQ-1)*NCR
                N=0
                DO IR=1,NROW
                DO IC=1,NCOL
                   N=N+1
                   LOC2=LOCT+N
                   IF(RNB(LOC2).GE.1.0) THEN
                      LOC4=LOC4+NN
                      IF(KQ==KQ_PRNT) BUFF(IC,IR,1)=DH(LOC4)
                   END IF
                END DO
                END DO
              END DO
              !
              KQ = KQ_PRNT
              K=LDN(KQ)
              ASSOCIATE( IU=>DELAY_HED(NF)%IU )
              IF(DELAY_HED(NF)%BINARY) THEN
                  IF(K<1000)THEN
                      TEXT(1)='DBED_HEAD_LAY'//NUM2STR(K,3,.TRUE.)
                  ELSE
                      TEXT(1)='DBED_HEAD_'   //NUM2STR(K,6,.TRUE.)
                  END IF
                  !
                  CALL ULASAV(BUFF(:,:,1),TEXT(1),KSTP,KPER,PERTIM,
     1                        TOTIM,NCOL,NROW,KQ,IU)
              ELSE
                  IF(HAS_STARTDATE) THEN
                      DATE=DATE_SP(KPER)%TS(KSTP)%STR(' ')
                  ELSE
                      DATE=' '
                  END IF
                     WRITE(IU,'(2I6,2(1x,ES15.7),A,3I8,A,I8,2(4x,A))')
     +                     KPER, KSTP, DELT, TOTIM,' DBED_HEAD ',
     +                     NCOL,NROW,K, ' NDB_ID ', KQ,
     +                     '(*(1x,ES15.7))', TRIM(DATE)
                     DO IR=1,NROW
                       WRITE(IU,'(*(1x,ES15.7))') BUFF(:,IR,1)
                     END DO
              END IF
              END ASSOCIATE
          END IF
          !
          CALL DELAY_HED(NF)%SIZE_CHECK()
          !
         END ASSOCIATE
        END DO !NF=1, SIZE(DELAY_HED_ID)
      END IF  
C
! OLD METHOD
!              LOC3=1-NN
!              DO KQ=1,NDB
!                DO CONCURRENT(IR=1:NROW,IC=1:NCOL) 
!                          BUFF(IC,IR,1)=HDRY
!                END DO
!                K=LDN(KQ)
!                NQ=(KQ-1)*NCR
!                DO 910 IR=1,NROW
!                NQR=NQ+(IR-1)*NCOL
!                DO 910 IC=1,NCOL
!                  LOC2=NQR+IC
!                  IF(RNB(LOC2).LT.1.0) GO TO 910
!                  LOC3=LOC3+NN
!                  IF(IBOUND(IC,IR,K).EQ.0) GO TO 910
!                  L1=LOC3
!                  L2=LOC3+NN-1
!                  BUFF(IC,IR,1)=DH(L2)
!  910           CONTINUE
!                !
!                ASSOCIATE( IU=>DELAY_HED(NF)%IU )
!                IF(DELAY_HED(NF)%BINARY) THEN
!                    IF(K<1000)THEN
!                        TEXT(1)='DBED_HEAD_LAY'//NUM2STR(K,3,.TRUE.)
!                    ELSE
!                        TEXT(1)='DBED_HEAD_'   //NUM2STR(K,6,.TRUE.)
!                    END IF
!                    !
!                    CALL ULASAV(BUFF(:,:,1),TEXT(1),KSTP,KPER,PERTIM,
!     1                          TOTIM,NCOL,NROW,KQ,IU)
!                ELSE
!                    IF(DATE_SP(1)%TS(0)%NOT_SET()) THEN
!                        DATE=' '
!                    ELSE
!                        DATE=DATE_SP(KPER)%TS(KSTP)%STR(' ')
!                    END IF
!                       WRITE(IU,'(2I6,2(1x,ES15.7),A,3I8,A,I8,2(4x,A))')
!     +                       KPER, KSTP, DELT, TOTIM,' DBED_HEAD ',
!     +                       NCOL,NROW,K, ' NDB_ID ', KQ,
!     +                       '(*(1x,ES15.7))', TRIM(DATE)
!                       DO IR=1,NROW
!                         WRITE(IU,'(*(1x,ES15.7))') BUFF(:,IR,1)
!                       END DO
!                END IF
!                END ASSOCIATE
!              END DO ! KQ=1,NDB
C29-----RETURN
!      RETURN
C
C30-----FORMATS
C
  230 FORMAT(/,31X,'VOLUMETRIC BUDGET FOR SYSTEMS OF DELAY INTERBEDS',
     1 /,42X,'AT END OF TIME STEP',I3,' IN ','STRESS PERIOD',I3,
     2 //,'          |   C U M U L A T I V E   ',
     3 'V O L U M E S   L**3           | R A T E S   F O R  T H I S',
     4 '  T I M E  S T E P   L**3/T  |',/,'   SYSTEM |    CHANGE IN',
     5 '     BOUNDARY                    PERCENT   |    CHANGE IN  ',
     6 '   BOUNDARY                    PERCENT   |',/,'   NUMBER | ',
     7 '    STORAGE        FLOW           SUM      DISCREPANCY |   ',
     8 '  STORAGE        FLOW           SUM      DISCREPANCY |',/,2X,
     9 8('-'),'|',56('-'),'|',56('-'),'|')
  240 FORMAT(I7,4G15.5,4G15.5)
  250 FORMAT(2X,8('-'),'|',56('-'),'|',56('-'),'|',/,' TOTALS:',
     1 4G15.5,4G15.5)
C
      END SUBROUTINE
      !
      SUBROUTINE SGWF2SUB7A(HAQ,TLED,CI,SSE,SSV,DZ,DH,DHP,DHC,NN)
C     ******************************************************************
C        ASSEMBLE COEFFICIENTS FOR SOLVING FOR HEAD DISTRIBUTION
C        IN ONE STRING OF CELLS REPRESENTING ONE-HALF OF A DOUBLY
C        DRAINING INTERBED
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSUBMODULE ,ONLY:A1,A2,BB
      DIMENSION DH(NN),DHP(NN),DHC(NN)
C     ------------------------------------------------------------------
C
C1------INITIALIZE
C
      CI2=CI*2.0
      DS=DZ*TLED
      NN1=NN-1
C2------SET COEFFICIENTS FOR CELL BORDERING AQUIFER
      SS=SSE
      A2(1)=CI
      HD=DH(1)
      HC=DHC(1)
      IF(HD.LT.HC) SS=SSV
      A1(1)=-3.*CI-DS*SS
      BB(1)=DS*(SSE*(HC-DHP(1))-HC*SS)-CI2*HAQ-A1(1)*HD
C3------SET COEFFICIENTS FOR INTERIOR CELLS
      BB(2)=-CI*HD
      DO 10 N=2,NN1
      SS=SSE
      A2(N)=CI
      HD=DH(N)
      HC=DHC(N)
      IF(HD.LT.HC) SS=SSV
      CHN=-CI*HD
      BB(N-1)=BB(N-1)+CHN
      BB(N+1)=CHN
      A1(N)=-CI2-DS*SS
      BB(N)=BB(N)+DS*(SSE*(HC-DHP(N))-HC*SS)-A1(N)*HD
   10 CONTINUE
C4------SET COEFFICIENTS FOR CELL BORDERING MIDPLANE OF INTERBED
      SS=SSE
      A2(NN)=CI
      HD=DH(NN)
      HC=DHC(NN)
      BB(NN1)=BB(NN1)-CI*HD
      IF(HD.LT.HC) SS=SSV
      A1(NN)=-CI-0.5*DS*SS
      BB(NN)=BB(NN)+DS*0.5*(SSE*(HC-DHP(NN))-HC*SS)-A1(NN)*HD
C5------RETURN
      RETURN
      END SUBROUTINE
      !
      PURE SUBROUTINE SGWF2SUB7S_FAST(NN,A1,A2,BB)
C     ******************************************************************
C        SOLVE SYSTEM OF EQUATIONS WITH A SYMMETRICAL TRI-DIAGONAL
C        COEFFICIENT MATRIX
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: NN
      REAL, DIMENSION(NN),INTENT(INOUT):: A1,A2,BB
      INTEGER:: N, I, NN1
      REAL:: F, C
C     ------------------------------------------------------------------
C
C1------TRIANGULARIZE LEFT-HAND SIDE MATRIX
      NN1=NN-1
      DO N=1,NN1
         F=1./A1(N)
         C=A2(N)*F
         I=N+1
         A1(I)=A1(I)-C*A2(N)
         A2(N)=C
         BB(I)=BB(I)-C*BB(N)
         BB(N)=BB(N)*F
      END DO
      BB(NN)=BB(NN)/A1(NN)
C
C2------BACK SUBSTITE FOR SOLUTION
      DO N=NN1, 1, -1
                     BB(N)=BB(N)-A2(N)*BB(N+1)
      END DO
      !
      END SUBROUTINE
      !
      SUBROUTINE SGWF2SUB7S(NN)
C     ******************************************************************
C        SOLVE SYSTEM OF EQUATIONS WITH A SYMMETRICAL TRI-DIAGONAL
C        COEFFICIENT MATRIX
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSUBMODULE ,ONLY: A1,A2,BB
C     ------------------------------------------------------------------
C
C1------TRIANGULARIZE LEFT-HAND SIDE MATRIX
      NN1=NN-1
      DO 30 N=1,NN1
      F=1./A1(N)
      C=A2(N)*F
      I=N+1
      A1(I)=A1(I)-C*A2(N)
      A2(N)=C
      BB(I)=BB(I)-C*BB(N)
      BB(N)=BB(N)*F
   30 CONTINUE
      BB(NN)=BB(NN)/A1(NN)
C
C2------BACK SUBSTITE FOR SOLUTION
      DO 40 N=NN1,1,-1
      BB(N)=BB(N)-A2(N)*BB(N+1)
   40 CONTINUE
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB72D1D(BUFF,NCOL,NROW,D,ND,LOC)
C     ******************************************************************
C     Move 2-D array into 1-D array
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION BUFF(NCOL,NROW),D(ND)
C     ------------------------------------------------------------------
      L=LOC-1
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
      L=L+1
      D(L)=BUFF(J,I)
   10 CONTINUE
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7SV(IGRID)
C     ******************************************************************
C     SAVE INTERBED STORAGE DATA FOR FUTURE RESTART
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSUBMODULE ,ONLY: DH,DHC,HAS_DELAY_BED,ND2,IDSAVE
C     ------------------------------------------------------------------
      CALL SGWF2SUB7PNT(IGRID)
C
C1-----PROCESS IF SAVE OPTION SELECTED AND DELAY INTERBEDS EXIST
      IF(IDSAVE.GT.0.AND.HAS_DELAY_BED) THEN
C2-----WRITE OUT NUMBER OF NODAL HEAD AND PRECONSOLIDATION HEAD VALUES
C2-----THAT ARE BEING SAVED TO DISK
       WRITE(IDSAVE) ND2
C3-----WRITE ARRAYS FOR EACH SYSTEM OF INTERBEDS
       WRITE(IDSAVE) (DH(N),N=1,ND2)
       WRITE(IDSAVE) (DHC(N),N=1,ND2)
      ENDIF
C4-----RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2SUB7DA(IGRID)
C
C     ******************************************************************
C     DEALLOCATE DYNAMIC STORAGE FOR SUB PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSUBMODULE
C     ------------------------------------------------------------------
C
      DEALLOCATE (GWFSUBDAT(IGRID)%IIBSCB)
      DEALLOCATE (GWFSUBDAT(IGRID)%ITMIN)
      DEALLOCATE (GWFSUBDAT(IGRID)%NNDB)
      DEALLOCATE (GWFSUBDAT(IGRID)%NDB)
      DEALLOCATE (GWFSUBDAT(IGRID)%NMZ)
      DEALLOCATE (GWFSUBDAT(IGRID)%NN)
      DEALLOCATE (GWFSUBDAT(IGRID)%ND2)
      DEALLOCATE (GWFSUBDAT(IGRID)%IDSAVE)
      DEALLOCATE (GWFSUBDAT(IGRID)%AC1)
      DEALLOCATE (GWFSUBDAT(IGRID)%AC2)
      DEALLOCATE (GWFSUBDAT(IGRID)%HAS_DELAY_BED)
      DEALLOCATE (GWFSUBDAT(IGRID)%HAS_INST_BED)
      DEALLOCATE (GWFSUBDAT(IGRID)%ISBOCF)
      DEALLOCATE (GWFSUBDAT(IGRID)%ISBOCU)
      DEALLOCATE (GWFSUBDAT(IGRID)%OCFLGS)
      DEALLOCATE (GWFSUBDAT(IGRID)%OCLAY)
      DEALLOCATE (GWFSUBDAT(IGRID)%ILSYS)
      DEALLOCATE (GWFSUBDAT(IGRID)%NTSSUM)
      DEALLOCATE (GWFSUBDAT(IGRID)%LN)
      DEALLOCATE (GWFSUBDAT(IGRID)%LDN)
      DEALLOCATE (GWFSUBDAT(IGRID)%NZ)
      DEALLOCATE (GWFSUBDAT(IGRID)%RNB)
      DEALLOCATE (GWFSUBDAT(IGRID)%DH)
      DEALLOCATE (GWFSUBDAT(IGRID)%DHP)
      DEALLOCATE (GWFSUBDAT(IGRID)%DHC)
      DEALLOCATE (GWFSUBDAT(IGRID)%DZ)
      DEALLOCATE (GWFSUBDAT(IGRID)%HC)
      DEALLOCATE (GWFSUBDAT(IGRID)%SCE)
      DEALLOCATE (GWFSUBDAT(IGRID)%SCV)
      DEALLOCATE (GWFSUBDAT(IGRID)%DCOM)
      DEALLOCATE (GWFSUBDAT(IGRID)%A1)
      DEALLOCATE (GWFSUBDAT(IGRID)%A2)
      DEALLOCATE (GWFSUBDAT(IGRID)%BB)
      DEALLOCATE (GWFSUBDAT(IGRID)%SUB)
      DEALLOCATE (GWFSUBDAT(IGRID)%DP)
      DEALLOCATE (GWFSUBDAT(IGRID)%DVB)
!      DEALLOCATE (GWFSUBDAT(IGRID)%ISUBLNK)                                !SUB-Linkage rth 
!      DEALLOCATE (GWFSUBDAT(IGRID)%ILPFLNK)                                !SUB-Linkage rth 
!      DEALLOCATE (GWFSUBDAT(IGRID)%SUBLNK)                                 !SUB-Linkage rth
!      GWFSUBDAT(IGRID)%LPFLNK=>NULL()                                      !seb LPFLNK ORIGINALLY POINTED TO SUBLNK, NULLIFY THE POINTER BEFORE DEALLOCATING 
      DEALLOCATE (GWFSUBDAT(IGRID)%LPFLNK)                                  !SUB-Linkage rth
      DEALLOCATE (GWFSUBDAT(IGRID)%DVZ)                                     !SUB-Linkage rth
      DEALLOCATE (GWFSUBDAT(IGRID)%DVZC)                                    !WSCHMID
      DEALLOCATE (GWFSUBDAT(IGRID)%NOCOMV)
      DEALLOCATE (GWFSUBDAT(IGRID)%DELAY_HED   )
      DEALLOCATE (GWFSUBDAT(IGRID)%DELAY_HED_ID)
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.EQ.1)THEN
        IIBSCB  =>NULL()
        ITMIN   =>NULL()
        NNDB    =>NULL()
        NDB     =>NULL()
        NMZ     =>NULL()
        NN      =>NULL()
        ND2     =>NULL()
        IDSAVE  =>NULL()
        AC1     =>NULL()
        AC2     =>NULL()
        HAS_DELAY_BED     =>NULL()
        HAS_INST_BED    =>NULL()
        ISBOCF  =>NULL()
        ISBOCU  =>NULL()
        OCFLGS  =>NULL()
        OCLAY   =>NULL()
        ILSYS   =>NULL()
        NTSSUM  =>NULL()
        LN      =>NULL()
        LDN     =>NULL()
        NZ      =>NULL()
        RNB     =>NULL()
        DH      =>NULL()
        DHP     =>NULL()
        DHC     =>NULL()
        DZ      =>NULL()
        HC      =>NULL()
        SCE     =>NULL()
        SCV     =>NULL()
        DCOM    =>NULL()
        A1      =>NULL()
        A2      =>NULL()
        BB      =>NULL()
        SUB     =>NULL()
        DP      =>NULL()
        DVB     =>NULL()
!        ISUBLNK=>NULL()                                !SUB-Linkage rth 
!        ILPFLNK=>NULL()                                !SUB-Linkage rth 
!        SUBLNK =>NULL()                                !SUB-Linkage rth
        LPFLNK  =>NULL()                                !SUB-Linkage rth
        DVZ     =>NULL()                                !SUB-Linkage rth
        DVZC    =>NULL()                                !WSCHMID
        NOCOMV  =>NULL()
      DELAY_HED     =>NULL()
      DELAY_HED_ID  =>NULL()
      END IF
C2-----RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE SGWF2SUB7PNT(IGRID)
C  Change SUB data to a different grid.
      USE GWFSUBMODULE
C
      IIBSCB=>GWFSUBDAT(IGRID)%IIBSCB
      ITMIN=>GWFSUBDAT(IGRID)%ITMIN
      NNDB=>GWFSUBDAT(IGRID)%NNDB
      NDB=>GWFSUBDAT(IGRID)%NDB
      NMZ=>GWFSUBDAT(IGRID)%NMZ
      NN=>GWFSUBDAT(IGRID)%NN
      ND2=>GWFSUBDAT(IGRID)%ND2
      IDSAVE=>GWFSUBDAT(IGRID)%IDSAVE
      AC1=>GWFSUBDAT(IGRID)%AC1
      AC2=>GWFSUBDAT(IGRID)%AC2
      HAS_DELAY_BED=>GWFSUBDAT(IGRID)%HAS_DELAY_BED
      HAS_INST_BED=>GWFSUBDAT(IGRID)%HAS_INST_BED
      ISBOCF=>GWFSUBDAT(IGRID)%ISBOCF
      ISBOCU=>GWFSUBDAT(IGRID)%ISBOCU
      OCFLGS=>GWFSUBDAT(IGRID)%OCFLGS
      OCLAY=>GWFSUBDAT(IGRID)%OCLAY
      ILSYS=>GWFSUBDAT(IGRID)%ILSYS
      NTSSUM=>GWFSUBDAT(IGRID)%NTSSUM
      LN=>GWFSUBDAT(IGRID)%LN
      LDN=>GWFSUBDAT(IGRID)%LDN
      NZ=>GWFSUBDAT(IGRID)%NZ
      RNB=>GWFSUBDAT(IGRID)%RNB
      DH=>GWFSUBDAT(IGRID)%DH
      DHP=>GWFSUBDAT(IGRID)%DHP
      DHC=>GWFSUBDAT(IGRID)%DHC
      DZ=>GWFSUBDAT(IGRID)%DZ
      HC=>GWFSUBDAT(IGRID)%HC
      SCE=>GWFSUBDAT(IGRID)%SCE
      SCV=>GWFSUBDAT(IGRID)%SCV
      DCOM=>GWFSUBDAT(IGRID)%DCOM
      DCOME=>GWFSUBDAT(IGRID)%DCOME
      DCOMV=>GWFSUBDAT(IGRID)%DCOMV
      A1=>GWFSUBDAT(IGRID)%A1
      A2=>GWFSUBDAT(IGRID)%A2
      BB=>GWFSUBDAT(IGRID)%BB
      SUB=>GWFSUBDAT(IGRID)%SUB
      SUBE=>GWFSUBDAT(IGRID)%SUBE
      SUBV=>GWFSUBDAT(IGRID)%SUBV
      DP=>GWFSUBDAT(IGRID)%DP
      DVB=>GWFSUBDAT(IGRID)%DVB 
!      ISUBLNK=>GWFSUBDAT(IGRID)%ISUBLNK                                !SUB-Linkage rth 
!      ILPFLNK=>GWFSUBDAT(IGRID)%ILPFLNK                                !SUB-Linkage rth  
!      SUBLNK=>GWFSUBDAT(IGRID)%SUBLNK                                !SUB-Linkage rth
      LPFLNK=>GWFSUBDAT(IGRID)%LPFLNK                                !SUB-Linkage rth
      DVZ=>GWFSUBDAT(IGRID)%DVZ                                      !SUB-Linkage rth
      DVZC=>GWFSUBDAT(IGRID)%DVZC                                      !WSCHMID
      NOCOMV=>GWFSUBDAT(IGRID)%NOCOMV
      DELAY_HED     => GWFSUBDAT(IGRID)%DELAY_HED 
      DELAY_HED_ID  => GWFSUBDAT(IGRID)%DELAY_HED_ID
C
      RETURN 
      END SUBROUTINE
      !
      SUBROUTINE SGWF2SUB7PSV(IGRID)
C  Save SUB data for a grid.
      USE GWFSUBMODULE
C
      GWFSUBDAT(IGRID)%IIBSCB=>IIBSCB
      GWFSUBDAT(IGRID)%ITMIN=>ITMIN
      GWFSUBDAT(IGRID)%NNDB=>NNDB
      GWFSUBDAT(IGRID)%NDB=>NDB
      GWFSUBDAT(IGRID)%NMZ=>NMZ
      GWFSUBDAT(IGRID)%NN=>NN
      GWFSUBDAT(IGRID)%ND2=>ND2
      GWFSUBDAT(IGRID)%IDSAVE=>IDSAVE
      GWFSUBDAT(IGRID)%AC1=>AC1
      GWFSUBDAT(IGRID)%AC2=>AC2
      GWFSUBDAT(IGRID)%HAS_DELAY_BED=>HAS_DELAY_BED
      GWFSUBDAT(IGRID)%HAS_INST_BED=>HAS_INST_BED
      GWFSUBDAT(IGRID)%ISBOCF=>ISBOCF
      GWFSUBDAT(IGRID)%ISBOCU=>ISBOCU
      GWFSUBDAT(IGRID)%OCFLGS=>OCFLGS
      GWFSUBDAT(IGRID)%OCLAY=>OCLAY
      GWFSUBDAT(IGRID)%ILSYS=>ILSYS
      GWFSUBDAT(IGRID)%NTSSUM=>NTSSUM
      GWFSUBDAT(IGRID)%LN=>LN
      GWFSUBDAT(IGRID)%LDN=>LDN
      GWFSUBDAT(IGRID)%NZ=>NZ
      GWFSUBDAT(IGRID)%RNB=>RNB
      GWFSUBDAT(IGRID)%DH=>DH
      GWFSUBDAT(IGRID)%DHP=>DHP
      GWFSUBDAT(IGRID)%DHC=>DHC
      GWFSUBDAT(IGRID)%DZ=>DZ
      GWFSUBDAT(IGRID)%HC=>HC
      GWFSUBDAT(IGRID)%SCE=>SCE
      GWFSUBDAT(IGRID)%SCV=>SCV
      GWFSUBDAT(IGRID)%DCOM=>DCOM
      GWFSUBDAT(IGRID)%DCOME=>DCOME
      GWFSUBDAT(IGRID)%DCOMV=>DCOMV
      GWFSUBDAT(IGRID)%A1=>A1
      GWFSUBDAT(IGRID)%A2=>A2
      GWFSUBDAT(IGRID)%BB=>BB
      GWFSUBDAT(IGRID)%SUB=>SUB
      GWFSUBDAT(IGRID)%SUBE=>SUBE
      GWFSUBDAT(IGRID)%SUBV=>SUBV
      GWFSUBDAT(IGRID)%DP=>DP
      GWFSUBDAT(IGRID)%DVB=>DVB
!      GWFSUBDAT(IGRID)%ISUBLNK=>ISUBLNK                                !SUB-Linkage rth 
!      GWFSUBDAT(IGRID)%ILPFLNK=>ILPFLNK                                !SUB-Linkage rth 
!      GWFSUBDAT(IGRID)%SUBLNK=>SUBLNK                                !SUB-Linkage rth
      GWFSUBDAT(IGRID)%LPFLNK=>LPFLNK                                !SUB-Linkage rth
      GWFSUBDAT(IGRID)%DVZ=>DVZ                                      !SUB-Linkage rth
      GWFSUBDAT(IGRID)%DVZC=>DVZC                                      !WSCHMID
      GWFSUBDAT(IGRID)%NOCOMV=>NOCOMV
      GWFSUBDAT(IGRID)%DELAY_HED     => DELAY_HED 
      GWFSUBDAT(IGRID)%DELAY_HED_ID  => DELAY_HED_ID
C
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE SUB_PRINT_INI_CRIT_HEAD(BASE, INFILE, IOUT, BUFF)
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
      USE GLOBAL,                       ONLY: NROW, NCOL
      USE GWFSUBMODULE,                 ONLY: NOCOMV, NNDB, HC ,LN,
     +                                        RNB, DHC, NDB, NN, LDN
      USE NUM2STR_INTERFACE,            ONLY: NUM2STR
      USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
      IMPLICIT NONE
      CHARACTER(*),               INTENT(IN   ):: BASE
      INTEGER,                    INTENT(IN   ):: INFILE, IOUT
      REAL, DIMENSION(NCOL,NROW), INTENT(INOUT):: BUFF 
      !
      TYPE(GENERIC_OUTPUT_FILE):: FL
      CHARACTER(:), ALLOCATABLE:: NAM
      INTEGER:: NCR
      INTEGER:: IR, IC, K, KQ, N
      INTEGER:: LOC, LOC2, LOC4, LOCT
      !
      NCR=NROW*NCOL
      !
      DO KQ=1,NNDB
         K  = LN(KQ)
         LOC=(KQ-1)*NCR
         !
         NAM = BASE//'INST_CRIT_HEAD_LAY'//NUM2STR(K ,2,.TRUE.)//
     +                             '_BED'//NUM2STR(KQ,2,.TRUE.)//'.txt'
         !
         CALL FL%OPEN(NAM, OUTPUT=IOUT, INFILE=INFILE)
         !
         DO IR=1,NROW
         DO IC=1,NCOL
            LOC=LOC+1
            BUFF(IC,IR) = HC(LOC)
         END DO
         END DO
         !
         DO IR=1,NROW
           WRITE(FL%IU,'(*(1x,ES15.7))') BUFF(:,IR)
         END DO
         !
         CALL FL%CLOSE()
      END DO
      !
      LOC4=0
      DO KQ=1,NDB
         K=LDN(KQ)
         !
         NAM = BASE//'DBED_CRIT_HEAD_LAY'//NUM2STR(K ,2,.TRUE.)//
     +                             '_BED'//NUM2STR(KQ,2,.TRUE.)//'.txt'
         !
         CALL FL%OPEN(NAM, OUTPUT=IOUT, INFILE=INFILE)
         !
         DO IR=1, NROW
         DO IC=1, NCOL
                  BUFF(IC,IR) = NOCOMV
         END DO
         END DO
         !
         LOCT=(KQ-1)*NCR
         N=0
         DO IR=1,NROW
         DO IC=1,NCOL
            N=N+1
            LOC2=LOCT+N
            IF(RNB(LOC2).GE.1.0) THEN
               LOC4=LOC4+NN
               BUFF(IC,IR)=DHC(LOC4)
            END IF
         END DO
         END DO
         !
         DO IR=1,NROW
           WRITE(FL%IU,'(*(1x,ES15.7))') BUFF(:,IR)
         END DO
         !
         CALL FL%CLOSE()
      END DO
      ! 
      END SUBROUTINE
