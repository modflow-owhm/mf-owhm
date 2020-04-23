C
C
      SUBROUTINE GWF2BAS7AR(INUNIT,CUNIT,IUDIS,IUZON,IUMLT,
     2                      IGRID,IUOC,IUPVAL,USE_PAUSE)
C     ******************************************************************
C     Allocate and Read for GWF Basic Package
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      USE CONSTANTS,  ONLY:Z,NL,BLN, TRUE, FALSE, BLNK,inf,inf_I,DZ,DNEG
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     MXITER,IUNIT,NIUNIT,HNEW,LBOTM,LAYCBD,LAYHDT,
     3                     LAYHDS,PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,
     4                     BOTM,HOLD,IBOUND,CR,CC,CV,HCOF,RHS,BUFF,STRT,
     5                     DDREF,IRESTART,KPERSTART,KSTPSTART,DATAFILES,
     6                     IUNITSTART,SPSTART,SPEND,NOCBC,RBUF,
     7                     INPUT_CHECK,BIN_REAL_KIND,HNEW_OLD,SPTIM
      USE GLOBAL,     ONLY:SUBLNK, UPLAY, UPLAY_IDX, WTLAY,WTABLE,WTLAY
      USE GLOBAL,     ONLY: SUPER_NAMES
      USE PARAMMODULE,ONLY:MXPAR,MXCLST,MXINST,ICLSUM,IPSUM,
     1                     INAMLOC,NMLTAR,NZONAR,NPVAL,
     2                     B,IACTIVE,IPLOC,IPCLST,PARNAM,PARTYP,
     3                     ZONNAM,MLTNAM,INAME,PROPPRINT
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     1                      LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,
     2                      IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,
     3                      IDDREF,IDDREFNEW,DELT,PERTIM,TOTIM,HNOFLO,
     4                      HDRY,STOPER,CHEDFM,CDDNFM,CBOUFM,VBVL,VBNM,
     5                      IUBGT,PDIFFPRT,REALTIM,USE_LEAP_YR,
     6                      SIMTIM_PER,REALTIM_PER,TOTPERTIM,SIMTIME,
     7                      LISTSPLIT,BUDGETDB,DATE_SP,DEALLOCATE_MULT,
     8                      PRNT_CNVG_OUTER, PRNT_CNVG_NTERM, PRNT_CNVG,
     9                      PRNT_CNVG_LRC,PRNT_CNVG_DIF,
     8                      PRNT_FRES_OUTER, PRNT_FRES_NTERM, PRNT_FRES,
     9                      PRNT_FRES_LRC,PRNT_FRES_DIF,
     8                      PRNT_VERR_OUTER, PRNT_VERR_NTERM, PRNT_VERR,
     9                      PRNT_VERR_LRC,PRNT_VERR_DIF,
     1                      ADAMP_INPUT, BAS_ADAMP, BAS_ADAMP_TOL,
     2                      BAS_ADAMP_TOL2,HED_CHNG2, HED_CHNG3,
     3                      HED_LOCK,INTER_INFO,HAS_STARTDATE,
     4                      MAX_REL_VOL_ERROR,MAX_REL_VOL_INVOKED,
     5                      MIN_ITER_INPUT,   MIN_SOLVER_INTER_SP,
     6                      MIN_SOLVER_INTER, MIN_SOLVER_INTER_NEW,
     7                      OSCIL_DMP_OUTER,OSCIL_DMP_LRC,OSCIL_DMP_DIF,
     8                   DAMPEN_START,DAMPEN_START_ITR,DAMPEN_START_DMP,
     9                   ABOVE_GSE_LIM,ABOVE_GSE_PRT,ABOVE_GSE_PRT_LIM,
     1                       PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM
      USE BAS_UTIL,       ONLY: CHECK_FOR_VALID_DIMENSIONS
      USE UTIL_INTERFACE, ONLY: STOP_ERROR, COMMENT_INDEX,
     +                       WARNING_MESSAGE, READ_TO_DATA,
     +                       PARSE_WORD, PARSE_WORD_UP,
     +                       UPPER, IS_BLANK, IS_INTEGER,
     +                       GET_INTEGER, GET, GET_NUMBER, GET_WORD
      USE NUM2STR_INTERFACE,                ONLY: NUM2STR, INTFMT
      USE DATE_OPERATOR_INSTRUCTION,        ONLY: DATE_OPERATOR
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
      USE EquationParser, ONLY: EQUATION_SETUP_ERROR_ROUTINES
      USE LINKED_LIST_INSTRUCTION, ONLY: CHARACTER_LINKED_LIST
      USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
      USE GENERIC_INPUT_FILE_INSTRUCTION,  ONLY: GENERIC_INPUT_FILE
      USE ULOAD_AND_SFAC_INTERFACE,      ONLY: ULOAD
      USE LOAD_SUPER_NAMES_INTERFACE,    ONLY: LOAD_SUPER_NAMES
      USE BAS_OPTIONS_AND_STARTDATE,     ONLY: GET_BAS_OPTIONS
      !
      TYPE(DATE_OPERATOR):: STARTING_DATE
      TYPE(DATE_OPERATOR):: DATE
      TYPE(GENERIC_BLOCK_READER):: BL
      TYPE(GENERIC_OUTPUT_FILE):: TIME_INFO
      TYPE(GENERIC_INPUT_FILE):: SUPER_NAMES_IN
      !
      LOGICAL, INTENT(INOUT):: USE_PAUSE
C
      REAL:: DIMTOL                                                     !seb USER SPECIFIED TOLLERANCE FOR MINIMUM ACCEPTABLE MODEL DIMENSION SIZE  DELR, DELC, THICK>DIMTO*MAXDIM WHERE MAXDIM IS THE LARGEST OF EACH OF THE RESPECTIVE DIMENSIONS [eg MAXVAL(DELR)]
      DOUBLE PRECISION:: DTMP
C
      CHARACTER(5),DIMENSION(NIUNIT):: CUNIT
      CHARACTER(1250):: LINE
      !CHARACTER(30):: KEY
      CHARACTER(:), ALLOCATABLE:: FASTFORWARD
C
      DOUBLE PRECISION HNF
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
      LOGICAL:: SKIP_OPT_LINE,HAS_OPT_LINE,NO_OPT_LINE
      LOGICAL:: FOUND_BEGIN, WARN_DIM
      LOGICAL:: HAS_CELL_WATCH, NO_CONVERGENCE_STOP
      REAL,DIMENSION(:),ALLOCATABLE:: HSHIFT
C     ------------------------------------------------------------------
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(LISTSPLIT,BUDGETDB,SPSTART,SPEND,NOCBC,DEALLOCATE_MULT)
      ALLOCATE(PRNT_CNVG_OUTER, PRNT_CNVG_NTERM, PRNT_CNVG)
      ALLOCATE(PRNT_FRES_OUTER, PRNT_FRES_NTERM, PRNT_FRES)
      ALLOCATE(PRNT_VERR_OUTER, PRNT_VERR_NTERM, PRNT_VERR)
      ALLOCATE(NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,LENUNI,ITRSS)
      ALLOCATE(IXSEC,INBAS,IFREFM,NODES,IOUT,MXITER,IRESTART)
      ALLOCATE(KPERSTART,KSTPSTART,IUNITSTART)
      ALLOCATE(ADAMP_INPUT, BAS_ADAMP, BAS_ADAMP_TOL, BAS_ADAMP_TOL2)
      ALLOCATE(MAX_REL_VOL_ERROR,MAX_REL_VOL_INVOKED)
      ALLOCATE(MIN_ITER_INPUT,      MIN_SOLVER_INTER,
     +         MIN_SOLVER_INTER_NEW,MIN_SOLVER_INTER_SP,INTER_INFO)
      ALLOCATE(PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM)
      ALLOCATE(DAMPEN_START,DAMPEN_START_ITR,DAMPEN_START_DMP)
      ALLOCATE(OSCIL_DMP_OUTER)
      ALLOCATE(ABOVE_GSE_LIM,     SOURCE=inf)
      ALLOCATE(ABOVE_GSE_PRT_LIM, SOURCE=inf)
      ALLOCATE(ABOVE_GSE_PRT)
      !
      BAS_ADAMP = inf_I
      BAS_ADAMP_TOL = DZ
      BAS_ADAMP_TOL2= DZ
      PRNT_CNVG_OUTER = inf_I
      PRNT_FRES_OUTER = inf_I
      PRNT_VERR_OUTER = inf_I
      MAX_REL_VOL_ERROR = 0.025D0
      MAX_REL_VOL_INVOKED = FALSE
      MIN_SOLVER_INTER = Z
      MIN_SOLVER_INTER_SP = inf_I
      MIN_SOLVER_INTER_NEW = Z
      DAMPEN_START = FALSE
      DAMPEN_START_ITR = Z
      DAMPEN_START_DMP = DZ
      OSCIL_DMP_OUTER = inf_I
      PRNT_RES_LIM = DNEG
      !
      !ALLOCATE(CBC_GLOBAL)
      MXITER=1
      NOCBC = Z  !INDICATES THAT CBC WILL BE WRITTEN IF REQUESTED
      ALLOCATE(IUNIT(NIUNIT))
      !
C
      ALLOCATE(ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL)
      ALLOCATE(PDIFFPRT,REALTIM,USE_LEAP_YR)                            !seb
      ALLOCATE(PROPPRINT, SOURCE=' ')
      ALLOCATE(SIMTIM_PER, REALTIM_PER, TOTPERTIM, SIMTIME)
      PDIFFPRT=5                                                        !IF PERCENT ERROR GOES ABOVE 5% THEN PRINT TO CMD PROMPT
      IF(IGRID.EQ.1)THEN
        ALLOCATE(SUBLNK,INPUT_CHECK,BIN_REAL_KIND)
        SUBLNK      = FALSE
        INPUT_CHECK = FALSE
        BIN_REAL_KIND = REAL32  !SINGLE PRECISION BINARY OUTPUT
      END IF
C      ALLOCATE (B(MXPAR))                                              !MOVED ALLOCATION TO ATER MXPAR IS READ IN seb
C      ALLOCATE (IACTIVE(MXPAR))
C      ALLOCATE (IPLOC(4,MXPAR))
C      ALLOCATE (IPCLST(14,MXCLST))
C      ALLOCATE (PARNAM(MXPAR))
C      ALLOCATE (PARTYP(MXPAR))
C      ALLOCATE (INAME(MXINST))
C
      ALLOCATE(MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,LBHDSV,LBDDSV,
     1         LBBOSV)
      ALLOCATE(IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,
     1         ICHFLG,IDDREF,IDDREFNEW,IUBGT)
      ALLOCATE(DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER)
      ALLOCATE(CHEDFM,CDDNFM,CBOUFM)
      HDRY=1.E30
      !
      ALLOCATE (DATAFILES)  !Note that this does not include UTM8 BOM check when TFR includes EXTERNAL unit REWIND or RELOAD for a unit declaired here 
      CALL DATAFILES%INIT()
C
C2------Open all files in name file.
      CALL SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,INBAS,LISTSPLIT)!LINE = LIST FILE NAME
      !
C
C SET UP EQUATION PARSER ERROR ROUTINES
      IF(IGRID.EQ.1)THEN
          CALL EQUATION_SETUP_ERROR_ROUTINES(IOUT,
     +                          STOP_ERROR,WARNING_MESSAGE)
      END IF
C
C3------PRINT A MESSAGE IDENTIFYING THE BASIC PACKAGE.
        WRITE(IOUT,1)INBAS
    1 FORMAT(1X,/1X,'BAS -- BASIC PACKAGE, VERSION 8, 5/16/2016',
     2' INPUT READ FROM UNIT ',I5)
C
C3A-----SHOW PRECISION OF VARIABLES
      IPBUFF=PRECISION(BUFF)
      IPHNEW=PRECISION(HNEW)
      WRITE(IOUT,*)
      IF(IPBUFF.NE.IPHNEW) THEN
        WRITE(IOUT,'(2A)')'MODFLOW was compiled using mixed precision ',
     +   ' (Precision => Number of meaningful digits in numbers)'
        WRITE(IOUT,'(2A)') 'Precision of REAL variables:             ',
     +                      NUM2STR(IPBUFF)
        WRITE(IOUT,'(2A)') 'Precision of DOUBLE PRECISION variables: ',
     +                      NUM2STR(IPHNEW)
      ELSE
       WRITE(IOUT,'(2A)')'MODFLOW was compiled using uniform precision',
     +   ' (Precision => Number of meaningful digits in numbers)'
        WRITE(IOUT,'(2A)')
     +             'Precision of REAL and DOUBLE PRECISION variables: ',
     +              NUM2STR(IPBUFF)
      END IF
C
C4------Initialize parameter definition variables.                      !seb MOVED TO BELOW OPTIONS
C      IPSUM=0
C      ICLSUM=0
C      INAMLOC=1
C      DO 10 N=1,MXPAR
C        PARNAM(N)=' '
C        PARTYP(N)=' '
C        IPLOC(1,N)=0
C        IPLOC(2,N)=0
C        IACTIVE(N)=0
C   10 CONTINUE
C
C5------Allocate and read discretization data.
      CALL SGWF2BAS7ARDIS(LINE,IUDIS,IOUT,STARTING_DATE)
      NODES=NCOL*NROW*NLAY
C
C6------Allocate space for global arrays except discretization data.
      ALLOCATE (HNEW(NCOL,NROW,NLAY))
      ALLOCATE (HOLD(NCOL,NROW,NLAY))
      ALLOCATE (IBOUND(NCOL,NROW,NLAY))
      !
      ALLOCATE(HNEW_OLD(NCOL,NROW,NLAY))
      !
      !ALLOCATE (WETCEL(NCOL,NROW,NLAY))
      ALLOCATE (CR(NCOL,NROW,NLAY))
      ALLOCATE (CC(NCOL,NROW,NLAY))
      ALLOCATE (CV(NCOL,NROW,NLAY))
      ALLOCATE (HCOF(NCOL,NROW,NLAY))
      ALLOCATE (RHS(NCOL,NROW,NLAY))
      ALLOCATE (BUFF(NCOL,NROW,NLAY))
      ALLOCATE (RBUF(NCOL,NROW,NLAY))
      ALLOCATE (STRT(NCOL,NROW,NLAY))
      DDREF=>STRT
      ALLOCATE (LAYHDT(NLAY))
      ALLOCATE (LAYHDS(NLAY))
      !
      ALLOCATE (UPLAY(NCOL,NROW), SOURCE=Z)
      ALLOCATE (WTLAY(NCOL,NROW), SOURCE=Z)
      ALLOCATE (UPLAY_IDX)
      ALLOCATE (WTABLE(NCOL,NROW))
C
C7------Initialize head-dependent thickness indicator to code that
C7------indicates layer is undefined.
      DO 100 I=1,NLAY
        LAYHDT(I)=-1
        LAYHDS(I)=-1
  100 CONTINUE
        WRITE(IOUT,'(//)')
C
      IXSEC=0
      ICHFLG=0
      IFREFM=1  !NOW DEFAULT TO FREEE FORMAT
      IPRTIM=0
      STOPER=0.0
      MXPAR=2000                                                        !seb SET PARAMETER ARRAY DIMENSIONS
      MXCLST=2000000
      MXINST=50000
      MXBUD = 100
      WARN_DIM = TRUE
      DEALLOCATE_MULT=FALSE
      SPSTART=Z
      SPEND=NPER+1
      !
      CALL GET_BAS_OPTIONS(LINE, INBAS, IOUT, ICHFLG, IPRTIM, MXBUD, 
     +                     HSHIFT, WARN_DIM, USE_PAUSE, TIME_INFO, 
     +                     SUPER_NAMES_IN, STARTING_DATE)
!!!C8------Read BAS Package file.
!!!C8A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
!!!      CALL READ_TO_DATA(LINE,INBAS,IOUT,IOUT)
!!!C
!!!C8B-----LOOK FOR OPTIONS IN THE FIRST ITEM AFTER THE HEADING.

!!!C8B---CHECK FOR FASTFORWARD FLAG
!!!      LLOC=1
!!!      !
!!!      WRITE(IOUT,'(/A)')' THE BAS PACKAGE NOW DEFAULTS TO FREE FORMAT.'
!!!      WRITE(IOUT,'(A/)') ' USE OPTION "NOFREE" TO DISABLE FREE FORMAT.'
!!!      !
!!!      ! CHECK FOR OPTIONS BLOCK
!!!      LLOC=1
!!!      CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,KEY)
!!!      SKIP_OPT_LINE= FALSE
!!!      HAS_OPT_LINE = FALSE
!!!      !
!!!      SELECT CASE(KEY)
!!!      CASE('INTERNAL','EXTERNAL','OPEN/CLOSE','CONSTANT')
!!!          SKIP_OPT_LINE= TRUE
!!!          HAS_OPT_LINE = TRUE
!!!      CASE('XSECTION', 'FREE','CHTOCH','PRINTTIME','NOCBC',
!!!     +     'NOCBCPACK','STOPERROR','PERCENTERROR','MAXBUDGET',
!!!     +     'MAXPARAM','BUDGETDB','FASTFORWARD','NODIMCHECK',
!!!     +     'INPUT_CHECK', 'INPUTCHECK','NO_DIM_CHECK','PAUSE',
!!!     +     'SHOWPROGRESS','NOFREE','CBC_EVERY_TIMESTEP')
!!!           !
!!!           HAS_OPT_LINE = TRUE
!!!       CALL WARNING_MESSAGE(TRIM(LINE),INBAS,IOUT,
!!!     +  'BAS OPTION WARNING. FOUND OPTION "'//TRIM(KEY)//'"'//BLN//
!!!     + 'BUT BAS NOW EXPECTS OPTIONS BE WITHIN AN OPTIONS BLOCK '//
!!!     + 'RATHER THAN ALONG THE FIRST LINE OF INPUT.'//NL//
!!!     + '(IT WILL STILL PROCESS THIS OPTIONS LINE, '//
!!!     + 'BUT WILL FAIL ON OPTIONS THAT REQUIRE EXTRA INPUT,'//NL//
!!!     + ' SUCH AS "STOPERROR STOPER" OR "MAXPARAM MXPAR MXCLST MXINST".)'
!!!     + //BLN//'PLEASE USE "BEGIN OPTIONS" '//
!!!     + 'AND THEN HAVE ONE OPTION PER LINE AND'//NL//
!!!     + 'END THE OPTIONS BLOCK WITH THE WORD "END" IN PLACE OF SINGLE '//
!!!     + 'LINE OF OPTIONS.'//BLN//'FOR EXAMPLE:'
!!!     +  //BLN//'BEGIN OPTIONS'//BLN//'  FREE'//NL//'  NOCBC'//NL//
!!!     +  '  BUDGETDB ./VOL_BUDGET_OUT.txt'//BLN//'END OPTIONS'//BLN//
!!!     + 'NOTE THAT BLOCK MAYBE EMPTY OR ENTIRELY NOT PRESENT TO NOT '//
!!!     + 'INCLUDE ANY OPTIONS.' )
!!!      END SELECT
!!!      !
!!!      NO_OPT_LINE = .NOT. HAS_OPT_LINE
!!!      IF(SKIP_OPT_LINE) THEN
!!!          BL%NAME = 'SKIP'
!!!          BACKSPACE(INBAS)  !NO OPTIOONS SPECIFIED
!!!      ELSEIF(HAS_OPT_LINE) THEN
!!!          CALL BL%INIT()
!!!          BL%NAME = 'OPTION'
!!!          !
!!!          I = COMMENT_INDEX(LINE)
!!!          !
!!!          CALL BL%ADD_LINE(LINE(1:I))  !Add trimmed line to be parsed
!!!          !
!!!          BL%NLINE = Z  !estimate number of loops
!!!          LLOC=1
!!!          CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
!!!          !
!!!          DO WHILE( KEY .NE. BLNK )
!!!            !
!!!            BL%NLINE = BL%NLINE + 1
!!!            !
!!!            CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY,NO_UPCASE=FALSE)
!!!          END DO
!!!          !
!!!          ! Set for use in OPTIONS Block loop
!!!          LLOC=1
!!!      ELSE
!!!            CALL BL%LOAD(INBAS,IOUT,LINE=LINE,FOUND_BEGIN=FOUND_BEGIN)
!!!            IF(FOUND_BEGIN) BACKSPACE(INBAS)                   ! BLOCK LOADER AUTO-MOVES FORWARD A LINE WHEN BLOCK IS FOUND, BUT U2INT READS A LINE RATHER THEN PROCESSING LINE
!!!      END IF
!!!      !
!!!      IF((BL%NAME == 'OPTION' .OR. BL%NAME == 'OPTIONS')
!!!     +                                       .AND. BL%NLINE>Z) THEN
!!!       CALL BL%START()
!!!       DO J=1, BL%NLINE
!!!        IF(  BL%LINE=='ERROR' ) CALL STOP_ERROR(INFILE=BL%IU,
!!!     +           OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN "'//BL%NAME//
!!!     +           '" BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
!!!        !
!!!        IF(NO_OPT_LINE) LLOC=1
!!!        !
!!!        CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
!!!        !KEY = BL%LINE(ISTART:ISTOP)
!!!        !CALL UPPER(KEY)
!!!        !
!!!        SELECT CASE (KEY)
!!!        CASE('XSECTION')
!!!                       IXSEC=1
!!!                       !
!!!        CASE('CHTOCH')
!!!                       ICHFLG=1
!!!!        CASE('ADVANCED_DAMPING') ! BY_STRESS_PERIOD InputFile or OUTER_START
!!!!           WRITE(IOUT,'(2A,/A)')' FOUND OPTION "ADVANCED_DAMPING" ',
!!!!     +     'NOW LOUDING DAMPING DIFFERENCE TOLERANCE,',
!!!!     +     'THEN STARTING OUTER ITERATION OR KEYWORD "BY_STRESS_PERIOD"'
!!!!           CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!!     +     BAS_ADAMP_TOL,MSG='FOUND BAS OPTION "ADVANCED_DAMPING"'//NL//
!!!!     + 'BUT FAILED TO LOAD THE ADVANCED DAMPING HEAD DIFFERENCE '//
!!!!     + 'TOLERANCE (BAS_ADAMP_TOL)')
!!!!
!!!!            CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
!!!!            IF( BL%LINE(ISTART:ISTOP) == 'BY_STRESS_PERIOD') THEN
!!!!                 CALL ADAMP_INPUT%OPEN(BL%LINE,LLOC,IOUT,INBAS,
!!!!     +                                               NO_INTERNAL=TRUE)
!!!!            ELSE
!!!!                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!!     +   BAS_ADAMP,NO_PARSE_WORD=TRUE,
!!!!     +   MSG='FOUND BAS OPTION "ADVANCED_DAMPING"'//NL//
!!!!     + 'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)'//
!!!!     + 'OR KEYWORD "BY_STRESS_PERIOD"')
!!!!                 IF( BAS_ADAMP < 24 ) BAS_ADAMP = 24
!!!!            END IF
!!!!            ALLOCATE(HED_CHNG2(NCOL,NROW,NLAY))
!!!!            ALLOCATE(HED_CHNG3(NCOL,NROW,NLAY))
!!!!            ALLOCATE(HED_LOCK(NCOL,NROW,NLAY), SOURCE=Z)
!!!!            SELECT CASE(LENUNI)
!!!!            CASE(1  ); BAS_ADAMP_TOL2 =  3.28D0 !ft
!!!!            CASE(0,2); BAS_ADAMP_TOL2 =  1.00D0 ! m
!!!!            CASE(3  ); BAS_ADAMP_TOL2 =  1.00D2 !cm
!!!!            END SELECT
!!!!            !
!!!        CASE('PRINT_CONVERGENCE') ! OUTER_START NTERM FILE
!!!            !
!!!            WRITE(IOUT,'(A)')' FOUND OPTION "PRINT_CONVERGENCE"'
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +  PRNT_CNVG_NTERM,MSG='FOUND BAS OPTION "PRINT_CONVERGENCE"'//NL//
!!!     +  'BUT FAILED TO LOAD THE PRINT TERM COUNT (NTERM)')
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +  PRNT_CNVG_OUTER,MSG='FOUND BAS OPTION "PRINT_CONVERGENCE"'//NL//
!!!     +  'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)')
!!!         !
!!!         CALL PRNT_CNVG%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,
!!!     +                       NO_INTERNAL=TRUE)
!!!         !
!!!         IF(.NOT. PRNT_CNVG%BINARY)
!!!     +   CALL PRNT_CNVG%SET_HEADER('   SP   TS  ITER  LAY  ROW  COL'//
!!!     +   '           HEAD     CHNG_HEAD   DATE      CELL_ID')
!!!         !
!!!         ALLOCATE(PRNT_CNVG_LRC(PRNT_CNVG_NTERM) ,
!!!     +            PRNT_CNVG_DIF(PRNT_CNVG_NTERM)  )
!!!            !
!!!        CASE('PRINT_FLOW_RESIDUAL') ! OUTER_START NTERM FILE
!!!            !
!!!            WRITE(IOUT,'(A)')' FOUND OPTION "PRINT_FLOW_RESIDUAL"'
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +                       PRNT_FRES_NTERM,MSG=
!!!     +      'FOUND BAS OPTION "PRINT_FLOW_RESIDUAL"'//NL//
!!!     +      'BUT FAILED TO LOAD THE PRINT TERM COUNT (NTERM)')
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +                       PRNT_FRES_OUTER,MSG=
!!!     +  'FOUND BAS OPTION "PRINT_FLOW_RESIDUAL"'//NL//
!!!     +  'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)')
!!!         !
!!!         CALL PRNT_FRES%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,
!!!     +                       NO_INTERNAL=TRUE)
!!!         !
!!!         IF(.NOT. PRNT_FRES%BINARY)
!!!     +   CALL PRNT_FRES%SET_HEADER('   SP   TS  ITER  LAY  ROW  COL'//
!!!     +   '           HEAD    FLOW_RESIDUAL    VOL_RESIDUAL'//
!!!     +   '     CELL_VOLUME      DATE  CELL_ID')
!!!         !
!!!         ALLOCATE(PRNT_FRES_LRC(PRNT_FRES_NTERM) ,
!!!     +            PRNT_FRES_DIF(PRNT_FRES_NTERM)  )
!!!         !
!!!        CASE('PRINT_RELATIVE_VOLUME_ERROR','PRINT_RELATIVE_VOL_ERROR') ! OUTER_START NTERM FILE
!!!            !
!!!            WRITE(IOUT,'(A)')
!!!     +       ' FOUND OPTION "PRINT_RELATIVE_VOLUME_ERROR"'
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +                       PRNT_VERR_NTERM,MSG=
!!!     +      'FOUND BAS OPTION "PRINT_RELATIVE_VOLUME_ERROR"'//NL//
!!!     +      'BUT FAILED TO LOAD THE PRINT TERM COUNT (NTERM)')
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +                       PRNT_VERR_OUTER,MSG=
!!!     +  'FOUND BAS OPTION "PRINT_RELATIVE_VOLUME_ERROR"'//NL//
!!!     +  'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)')
!!!         !
!!!         CALL PRNT_VERR%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,
!!!     +                       NO_INTERNAL=TRUE)
!!!         !
!!!         IF(.NOT. PRNT_VERR%BINARY)
!!!     +   CALL PRNT_VERR%SET_HEADER('   SP   TS  ITER  LAY  ROW  COL'//
!!!     +   '           HEAD  REL_VOL_ERR    VOL_RESIDUAL'//
!!!     +   '   FLOW_RESIDUAL  DATE      CELL_ID')
!!!         !
!!!         ALLOCATE(PRNT_VERR_LRC(PRNT_VERR_NTERM) ,
!!!     +            PRNT_VERR_DIF(PRNT_VERR_NTERM)  )
!!!         !
!!!        CASE('MAX_RELATIVE_VOLUME_ERROR','MAX_RELATIVE_VOL_ERROR')
!!!              CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +              MAX_REL_VOL_ERROR,MSG=
!!!     +             'FOUND BAS OPTION "MAX_RELATIVE_VOL_ERROR"'//NL//
!!!     +             'BUT FAILED TO THE NUMBER LOCATED AFTER THE KEYWORD')
!!!        !
!!!        CASE('MIN_SOLVER_ITERATION','MIN_SOLVER_ITER')
!!!            !
!!!            CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
!!!            IF( BL%LINE(ISTART:ISTOP) == 'BY_STRESS_PERIOD') THEN
!!!              !
!!!              CALL MIN_ITER_INPUT%OPEN(BL%LINE,LLOC,IOUT,INBAS,
!!!     +                                 NO_INTERNAL=TRUE)
!!!              I = MIN_ITER_INPUT%IU
!!!              CALL BL%READ_AND_SET_LINE(I, EOF=FOUND_BEGIN)              !RESUSING VARIABLE FOUND_BEGIN
!!!              !
!!!              K = 1
!!!              CALL GET_INTEGER(BL%LN,K,ISTART,ISTOP,IOUT,I,
!!!     +              MIN_SOLVER_INTER_SP,MSG=
!!!     +             'FOUND BAS OPTION "MIN_SOLVER_ITER"'//NL//
!!!     +             'WHICH LOADS FROM BY_STRESS_PERIOD FILE '//
!!!     +             'BUT FAILED TO LOAD STRESS PERIOD NUMBER')
!!!              !
!!!              CALL GET_INTEGER(BL%LN,K,ISTART,ISTOP,IOUT,I,
!!!     +              MIN_SOLVER_INTER_NEW,MSG=
!!!     +            'FOUND BAS OPTION "MIN_SOLVER_ITER"'//NL//
!!!     +            'WHICH LOADS FROM BY_STRESS_PERIOD FILE BUT FAILED '//
!!!     +            'TO THE NUMBER LOCATED AFTER STRESS PERIOD NUMBER')
!!!            ELSE
!!!              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +              MIN_SOLVER_INTER,NO_PARSE_WORD=TRUE,MSG=
!!!     +             'FOUND BAS OPTION "MIN_SOLVER_ITER"'//NL//
!!!     +             'BUT FAILED TO THE NUMBER LOCATED AFTER THE KEYWORD')
!!!            END IF
!!!        !
!!!        CASE('DAMPEN_OSCILLATION', 'DAMPEN_OSCILLATIONS')
!!!            !
!!!            WRITE(IOUT,'(A)')' FOUND OPTION "DAMPEN_OSCILLATION"'
!!!            ALLOCATE(OSCIL_DMP_LRC, OSCIL_DMP_DIF)
!!!            OSCIL_DMP_LRC = Z
!!!            OSCIL_DMP_DIF = DZ
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     + OSCIL_DMP_OUTER,MSG='FOUND BAS OPTION "DAMPEN_OSCILLATION"'//NL//
!!!     +      'BUT FAILED TO LOAD THE NUMER OF ITERATIONS AT START '//
!!!     +      'TO CHECK IF OSCILLATIONS OCCUR.')
!!!            WRITE(IOUT,'(4A)')' HEAD OCCILLATIONS WILL BE DAMPEN ',
!!!     +      'BY 50% AFTER ',NUM2STR(OSCIL_DMP_OUTER),' ITERATIONS.'
!!!        !
!!!        CASE('DAMPEN_START')
!!!            !
!!!            WRITE(IOUT,'(A)')' FOUND OPTION "DAMPEN_START"'
!!!            !
!!!            DAMPEN_START = TRUE
!!!            DAMPEN_START_ITR = Z
!!!            DAMPEN_START_DMP = DZ
!!!            !
!!!            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +      DAMPEN_START_ITR,MSG='FOUND BAS OPTION "DAMPEN_START"'//NL//
!!!     +      'BUT FAILED TO LOAD THE NUMER OF ITERATIONS AT START '//
!!!     +      'TO DAMPEN (NITER)')
!!!            !
!!!            CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +      DAMPEN_START_DMP,MSG='FOUND BAS OPTION "DAMPEN_START"'//NL//
!!!     +      'BUT FAILED TO LOAD THE DAMPING VALUE (DAMP)')
!!!         !
!!!        CASE('DOUBLE_PRECISION_CBC');  BIN_REAL_KIND = REAL64
!!!            !
!!!        CASE('SHIFT_STRT')
!!!           WRITE(IOUT,'(2A/)')' SHIFT_STRT OPTION FOUND. NOW READING ',
!!!     +      'WITH ULOAD LIST STYLE NLAY NUMBERS THAT ARE ADDED TO STRT'
!!!            ALLOCATE(HSHIFT(NLAY))
!!!            I = LLOC
!!!            CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)  !CHECK IF THERE IS THE INTERNAL KEYWORD (ULOAD NEEDS A FILE TO READ INTERNAL FROM)
!!!            LLOC = I
!!!            !
!!!            IF(KEY=='INTERNAL') THEN
!!!                CALL BL%MAKE_SCRATCH_FILE(.TRUE.)
!!!                CALL BL%READ_SCRATCH(LINE=LINE)
!!!            END IF
!!!            !
!!!            I = Z !SURROGATE OF IU
!!!            CALL ULOAD(HSHIFT, LLOC, BL%LINE, BL%IOUT, BL%IU, I,
!!!     +                 SCRATCH=BL%SCRATCH)
!!!            !
!!!            IF(KEY=='INTERNAL') CALL BL%CLOSE_SCRATCH()
!!!         !
!!!        CASE('SHOWPROGRESS')  !KEYWORD DOCUMENTED BUT DOES NOTHING IN MF2005 NOR OWHM
!!!            CONTINUE
!!!        CASE('FREE')
!!!            IFREFM=1
!!!           WRITE(IOUT,'(A )')' THE FREE FORMAT OPTION HAS BEEN SELECTED'
!!!           WRITE(IOUT,'(A/)')' NOTE THIS NOW IS THE DEFAULT AND '//
!!!     +                                                  'NOT NECESSARY.'
!!!            !
!!!        CASE('NOFREE')
!!!            IFREFM=Z
!!!           WRITE(IOUT,'(A/)')' THE FREE FORMAT OPTION IS DISABLED'
!!!            !
!!!        CASE('PRINTTIME')
!!!            IPRTIM=1
!!!            WRITE(IOUT,'(A/)')' THE PRINTTIME OPTION HAS BEEN SELECTED'
!!!            !
!!!        CASE('PROPPRINT')
!!!            WRITE(IOUT,'(A )')' THE PROPPRINT OPTION HAS BEEN SELECTED.'
!!!            WRITE(IOUT,'(A/)')' CHECKING IF FOLDER IS SPECIFIED.'
!!!            !
!!!            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,I    ,DUM,IOUT,IU)
!!!            DEALLOCATE(PROPPRINT)
!!!            IF( LINE(ISTART:ISTOP)=='' ) THEN
!!!                ALLOCATE(PROPPRINT, SOURCE='./')
!!!            ELSE
!!!              IF( LINE(ISTOP:ISTOP)=='/'.OR.LINE(ISTOP:ISTOP)=='\' )THEN
!!!                  ALLOCATE(PROPPRINT, SOURCE=LINE(ISTART:ISTOP))
!!!              ELSE
!!!                  ISTOP=ISTOP+1
!!!                  LINE(ISTOP:ISTOP)='/'
!!!                  ALLOCATE(PROPPRINT, SOURCE=LINE(ISTART:ISTOP))
!!!              END IF
!!!            END IF
!!!            WRITE(IOUT,'(/A/,A/)')
!!!     + 'AQUIFER PROPERTIES AFTER PARAMETERS HAVE BEEN APPLIED '//
!!!     + 'WILL BE WRITEN TO SEPARATE FILES. THESE FILES WILL BE PLACED '//
!!!     + 'IN:', '"'//PROPPRINT//'"'
!!!   14 FORMAT (1X,/,1X,'NUMBER OF PARAMETER VALUES TO BE READ FROM',
!!!     1               ' PARAMETER VALUE FILE:',I5)
!!!            !
!!!        CASE('HEAD_DISTANCE_ABOVE_GSE_LIMIT','ABOVE_GSE_LIM')
!!!         CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +       ABOVE_GSE_LIM,
!!!     +      MSG='FOUND BAS OPTION "HEAD_DISTANCE_ABOVE_GSE_LIMIT"'//NL//
!!!     +     'BUT FAILED TO LOAD THE ACTUAL LIMIT (ABOVE_GSE_LIM)')
!!!         WRITE(IOUT,'(1x 1A/,1x 4A/,/1x A/,/1x A/,1x A/)')
!!!     +   '"HEAD_DISTANCE_ABOVE_GSE_LIMIT" OPTION FOUND.',
!!!     +   'HEAD VALUES GREATER THAN ', NUM2STR(ABOVE_GSE_LIM), ' L ',
!!!     +   'ABOVE THE GROUND SURFACE ELEVATION (GSE) ARE SET TO IT.',
!!!     +   'IF HEAD IS CHANGED, THEN CONVERGENCE IS NOT ALLOWED.',
!!!     +   'NOTE THAT IF THE GSE IS NOT SPECIFIED BY THE DIS OR FMP,',
!!!     +   '     THEN THE TOP OF THE UPPER MOST ACTIVE CELL IS USED.'
!!!            !
!!!        CASE('HEAD_DISTANCE_ABOVE_GSE_PRINT')
!!!         CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +       ABOVE_GSE_PRT_LIM,
!!!     +      MSG='FOUND BAS OPTION "HEAD_DISTANCE_ABOVE_GSE_PRINT"'//NL//
!!!     +      'BUT FAILED TO LOAD THE SHIFT FACTOR (ABOVE_GSE_PRT_LIM)')
!!!            !
!!!            CALL ABOVE_GSE_PRT%OPEN(BL%LINE,LLOC,IOUT,INBAS)
!!!            CALL ABOVE_GSE_PRT%SET_HEADER(
!!!     +     'SP  TS   LAY ROW  COL   GSE            HEAD           DATE')
!!!         WRITE(IOUT,'(1x 1A/,1x 3A/,1x 2A/)')
!!!     +   '"HEAD_DISTANCE_ABOVE_GSE_PRINT" OPTION FOUND.',
!!!     +   'AFTER TIME STEP CONVERGES ANY HEAD VALUES GREATER THAN ',
!!!     +   NUM2STR(ABOVE_GSE_PRT_LIM), ' L', 'ABOVE THE GROUND ',
!!!     +   'SURFACE ELEVATION (GSE) ARE PRINTED TO A GENERIC_OUTPUT FILE.'
!!!            !
!!!        CASE('DEALLOCATE_MULT')
!!!            DEALLOCATE_MULT = TRUE
!!!            !
!!!        CASE('NOCBC')
!!!            NOCBC=2
!!!            WRITE(IOUT,'(A/)')
!!!     +           ' NOCBC OPTION SELECTED. NO CBC FLOWS WILL BE WRITTEN.'
!!!            !
!!!        CASE('NOCBCPACK')
!!!            NOCBC=1
!!!            WRITE(IOUT,'(2A/)')' NOCBCPACK OPTION SELECTED. ',
!!!     +           'ONLY FLOW PACKAGE CBC FLOWS WILL BE WRITTEN.'
!!!        CASE('CBC_EVERY_TIMESTEP')
!!!            NOCBC=-1
!!!            WRITE(IOUT,'(2A/)')' CBC_EVERY_TIMESTEP OPTION SELECTED. ',
!!!     +           'THE CBC WILL BE WRITTEN TO EVERY TIME STEP.'
!!!        CASE('CBC_LAST_TIMESTEP')
!!!            NOCBC=-1
!!!            WRITE(IOUT,'(2A/)')' CBC_EVERY_TIMESTEP OPTION SELECTED. ',
!!!     +           'THE CBC WILL BE WRITTEN TO EVERY TIME STEP.'
!!!            !
!!!        CASE('NO_DIM_CHECK', 'NODIMCHECK')
!!!           WRITE(IOUT,'(2A/)')' NO_DIM_CHECK OPTION FOUND. DELR, DELC,',
!!!     + ' AND CELL THICKNESS WILL ONLY CHECK FOR NEGATIVE VALUES.'
!!!                WARN_DIM = FALSE
!!!        CASE('NO_CONVERGENCE_STOP', 'NO_FAILED_CONVERGENCE_STOP')
!!!                NO_CONVERGENCE_STOP = TRUE
!!!         WRITE(IOUT,'(A/)') ' NO_CONVERGENCE_STOP OPTION TURNED ON'
!!!        CASE('STOPERROR')
!!!         CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +       STOPER,MSG='FOUND BAS OPTION "STOPERROR"'//NL//
!!!     +        'BUT FAILED TO LOAD ACCEPTIBLE BUDGET PERCENT ERROR')
!!!         WRITE(IOUT,'(3A/)') ' STOPERROR OPTION TURNED ON WITH MAX ',
!!!     +           'ALLOWED BUDGET PERCENT ERROR BEING: ',NUM2STR(STOPER)
!!!            !
!!!        CASE('PERCENTERROR')
!!!         CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +       PDIFFPRT,MSG='FOUND BAS OPTION "PERCENTERROR"'//NL//
!!!     +        'BUT FAILED TO LOAD ACTUAL PERECENT ERROR VALUE')
!!!            WRITE(IOUT,'(1x,4A/)')
!!!     +        'RATE PERCENT ERROR WILL BE PRINTED FOR EVERY TIME STEP ',
!!!     +        'THAT EXCEDES ',NUM2STR(PDIFFPRT),'%'
!!!            !
!!!        CASE('MAXBUDGET')
!!!         CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +       MXBUD,MSG='FOUND BAS OPTION "MAXBUDGET"'//NL//
!!!     +        'BUT FAILED TO LOAD ACTUAL MAXIMUM BUDGET VALUE (MXBUD)')
!!!            WRITE(IOUT,'(1x 3A/)')
!!!     +          'MAXBUDGET OPTION SET. THE MAXIMUM NUMBER OF BUDGET ',
!!!     +          'ENTRIES IS CHANGED FROM 100 TO', NUM2STR(MXBUD)
!!!            !
!!!        CASE('MAXPARAM')
!!!            WRITE(IOUT,'(3(/,1x,A)/)')
!!!     +          'MAXPARAMETER ARRAY STORAGE OPTION SELECTED (MAXPARAM)',
!!!     +          'NOW READING IN MXPAR, MXCLST, and MXINST',
!!!     +          'THESE VALUSE MUST BE >0, SET TO 1 IF NOT USED'
!!!         CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,2,MXPAR,R,IOUT,INBAS)
!!!         CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,2,MXCLST,R,IOUT,INBAS)
!!!         CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,2,MXINST,R,IOUT,INBAS)
!!!            IF(MXPAR<1)THEN
!!!                WRITE(IOUT,'(/ / A / /)')
!!!     +                       'WARNING MXPAR<1; VALUE CHANGED TO MXPAR=1'
!!!                MXPAR=1
!!!            END IF
!!!            IF(MXCLST<1)THEN
!!!                WRITE(IOUT,'(/ / A / /)')
!!!     +                     'WARNING MXCLST<1; VALUE CHANGED TO MXCLST=1'
!!!                MXCLST=1
!!!            END IF
!!!            IF(MXINST<1)THEN
!!!                WRITE(IOUT,'(/ / A / /)')
!!!     +                     'WARNING MXINST<1; VALUE CHANGED TO MXINST=1'
!!!                MXINST=1
!!!            END IF
!!!            !
!!!        CASE('ITERATION_INFO','INTERATION_INFO')
!!!            CALL INTER_INFO%OPEN(BL%LINE,LLOC,IOUT,INBAS,
!!!     +                                                  SPLITMAXCOUNT=Z)
!!!         CALL INTER_INFO%SET_HEADER(
!!!     +                   '    SP    TS  ITER  P_ERR          EXTRA')
!!!            !
!!!        CASE('BUDGETDB')
!!!            CALL BUDGETDB%OPEN(BL%LINE,LLOC,IOUT,INBAS,
!!!     +                                                  SPLITMAXCOUNT=Z)
!!!            WRITE(IOUT,'(1x,3A/)')
!!!     +    'BUDGET DATABASE OPTION SET. A DATABASE FRIENDLY FILE WILL ',
!!!     +    'BE WRITTEN TO FILE UNIT: ', NUM2STR(BUDGETDB%IU)
!!!            !
!!!        CASE('PRINT_TIME_INFO')
!!!            CALL TIME_INFO%OPEN(BL%LINE,LLOC,IOUT,INBAS,NOBINARY=TRUE)
!!!            WRITE(IOUT,'(1x,3A/)')
!!!     +    'PRINT_TIME_INFO OPTION FOUND, TIME STEP TIME INFORMATION ',
!!!     +    'WILL BE WRITTEN TO FILE UNIT: ', NUM2STR(TIME_INFO%IU)
!!!            !
!!!        CASE('SUPER_NAMES', 'SUPERNAMES')
!!!            CALL SUPER_NAMES_IN%OPEN(BL%LINE,LLOC,IOUT,INBAS)
!!!            WRITE(IOUT,'(1x,2A/)')
!!!     +    'SUPER_NAMES OPTION FOUND, WILL LOAD A LIST OF GLOBAL NAMES ',
!!!     +    'THAT CAN BE USED IN OTHER PACKAGES'
!!!            !
!!!        CASE('INPUT_CHECK', 'INPUTCHECK')
!!!            SPSTART = NPER+1
!!!            SPEND   = SPSTART
!!!            INPUT_CHECK = TRUE
!!!            !
!!!        CASE('PAUSE')
!!!            USE_PAUSE = TRUE
!!!            !
!!!        CASE('FASTFORWARD')
!!!            !
!!!            CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP) !STARTING DATE
!!!            IF( IS_INTEGER(BL%LINE(ISTART:ISTOP)) ) THEN
!!!                !
!!!                CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +                      SPSTART,TRUE,MSG='FASTFORWARD ERROR -- '//
!!!     +                      'FAILED TO IDENTIFY STARTING STRESS PERIOD')
!!!            ELSEIF( .NOT. HAS_STARTDATE ) THEN
!!!                CALL STOP_ERROR(BL%LINE,INBAS,IOUT,
!!!     +       'FASTFORWARD ERROR -- FAILED TO IDENTIFY A STARTING '//
!!!     +       'STRESSS PERIOD No.'//NL//'AND THERE IS NOT A STARTING '//
!!!     +      'DATE SPECIFIED IN THE DIS (viz. "STARTDATE" KEYWORD)'//NL//
!!!     +       'TO MAKE OneWater DATE AWARE'//NL//'AND ALLOW THE USE '//
!!!     +       'OF CALENDAR DATES AS AN INPUT TO THE FASTWORD FEATURE')
!!!            ELSE
!!!                CALL DATE%INIT( BL%LINE(ISTART:ISTOP), 0.001D0 )
!!!                IF(  DATE%NOT_SET() )
!!!     +              CALL STOP_ERROR(TRIM(BL%LINE),INBAS,IOUT,
!!!     +    'FASTFORWARD ERROR -- FAILED TO IDENTIFY STARTING '//
!!!     +    'STRESS PERIOD No.'//NL//'OR STARTING CALENDAR DATE'//NL//
!!!     +    'FOR FASTFORWARD TO IDENTIFY STARTING POINT.')
!!!                !
!!!                CALL DATE_TO_SP(DATE,SPSTART)
!!!            END IF
!!!            !
!!!            CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP) !ENDING DATE
!!!            IF( IS_INTEGER(BL%LINE(ISTART:ISTOP)) ) THEN
!!!                !
!!!                CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!!!     +                      SPEND,TRUE,MSG='FASTFORWARD ERROR -- '//
!!!     +                      'FAILED TO IDENTIFY ENDING STRESS PERIOD')
!!!            ELSE
!!!                CALL DATE%INIT( BL%LINE(ISTART:ISTOP), 0.001D0 )
!!!                IF(  DATE%IS_SET() ) THEN
!!!                    CALL DATE_TO_SP(DATE,SPEND)
!!!                ELSE
!!!                    SPEND = NPER
!!!                    CALL WARNING_MESSAGE(BL%LINE,INBAS,IOUT,
!!!     +  'MINOR WARNING: FASTFORWARD -- FAILED TO IDENTIFY AN ENDING '//
!!!     +              'STRESSS PERIOD No.'//NL//
!!!     +              'NOR AN ENDING CALENDAR DATE'//NL//
!!!     +              'SO IT WILL BE ASSUMED TO HAVE AN ENDING AT '//
!!!     +              'STRESSS PERIOD AT NPER')
!!!                END IF
!!!            END IF
!!!!        CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INBAS)
!!!!        !
!!!!        READ(BL%LINE(ISTART:ISTOP), INTFMT(BL%LINE(ISTART:ISTOP)),
!!!!     +       IOSTAT=IERR) SPSTART
!!!!        !
!!!!        IF (IERR.NE.0) THEN
!!!!          CALL DATE%INIT( BL%LINE(ISTART:ISTOP), 0.001D0 )
!!!!          IF (DATE%NOT_SET()) CALL STOP_ERROR(
!!!!     +            TRIM(BL%LINE),INBAS,IOUT,'FASTFORWARD ERROR -- '//
!!!!     +            'FAILED TO IDENTIFY STARTING STRESS PERIOD')
!!!!      IF( DATE_SP(1)%TS(0)%NOT_SET() .AND. DATE%IS_SET() )    !PARSED DATE FROM STRING BUT NO STARTING DATE
!!!!     +    CALL STOP_ERROR(TRIM(BL%LINE),INBAS,IOUT,
!!!!     +    'FASTFORWARD ERROR -- START DATE SPECIFIED '//
!!!!     +    'BUT DIS DOES NOT HAVE THE "STARTDATE" KEYWORD TO '//
!!!!     +    'MAKE OneWater DATE AWARE')
!!!!          !
!!!!          CALL DATE_TO_SP(DATE,SPSTART)
!!!!          !!
!!!!          !N = UBOUND( DATE_SP(1)%TS,1 )
!!!!          !IF     ( DATE <  DATE_SP(1)%TS(N) ) THEN
!!!!          !                                          SPSTART=1
!!!!          !ELSEIF ( DATE >= DATE_SP(NPER)%TS(0) ) THEN
!!!!          !                                          SPSTART=NPER
!!!!          !ELSE
!!!!          ! DO I=2, NPER
!!!!          !  N = UBOUND( DATE_SP(I)%TS,1 )
!!!!          !  IF( DATE_SP(I)%TS(0)<=DATE .AND. DATE<DATE_SP(I)%TS(N) )THEN
!!!!          !                                          SPSTART=I
!!!!          !                                          EXIT
!!!!          !  END IF
!!!!          ! END DO
!!!!          !END IF
!!!!        END IF
!!!!        !
!!!!        CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
!!!!        READ(BL%LINE(ISTART:ISTOP),INTFMT(BL%LINE(ISTART:ISTOP)),
!!!!     +       IOSTAT=IERR)I
!!!!        CALL DATE%INIT( BL%LINE(ISTART:ISTOP) )   !, 0.01D0
!!!!        !
!!!!      IF( DATE_SP(1)%TS(0)%NOT_SET() .AND. DATE%IS_SET() )    !DATE%DAY = -999 IF FAILED TO PARSE DATE FROM STRING
!!!!     +  CALL STOP_ERROR(TRIM(BL%LINE),INBAS,IOUT,
!!!!     +  'FASTFORWARD ERROR -- END DATE SPECIFIED '//
!!!!     +  'BUT DIS DOES NOT HAVE THE "STARTDATE" KEYWORD TO '//
!!!!     +  'MAKE OneWater DATE AWARE')
!!!!        !
!!!!        IF(IERR==0 .AND. BL%LINE(ISTART:ISTOP).NE.' ' ) THEN         ! FORTRAN SETS ' ' TO ZERO ON READ
!!!!                                                    SPEND=I
!!!!        ELSEIF ( LLOC==LEN(BL%LINE) ) THEN                           !NOTHING AT THE END OF THE LINE
!!!!                                                    SPEND=NPER
!!!!        ELSE
!!!!                                             CALL DATE_TO_SP(DATE,SPEND)
!!!!                                             IF(SPEND<0) SPEND=NPER
!!!!        END IF
!!!!        !
!!!!        !N = UBOUND( DATE_SP(1)%TS,1 )
!!!!        !IF(IERR==0 .AND. LINE(ISTART:ISTOP).NE.' ' ) THEN               ! FORTRAN SETS ' ' TO ZERO ON READ
!!!!        !                                            SPEND=I
!!!!        !ELSEIF ( DATE%DAY==-999.OR. LLOC==LEN(LINE) ) THEN
!!!!        !                                            SPEND=NPER
!!!!        !ELSEIF ( DATE <  DATE_SP(1)%TS(N) ) THEN
!!!!        !                                            SPEND=1
!!!!        !ELSEIF ( DATE >= DATE_SP(NPER)%TS(0) ) THEN
!!!!        !                                            SPEND=NPER
!!!!        !ELSE  !IERR.NE.0
!!!!        !   DO I=1, NPER
!!!!        !     N = UBOUND( DATE_SP(I)%TS,1 )
!!!!        !     IF(DATE_SP(I)%TS(0)<=DATE .AND. DATE<DATE_SP(I)%TS(N) )THEN
!!!!        !                                             SPEND=I
!!!!        !                                             EXIT
!!!!        !     END IF
!!!!        !   END DO
!!!!        !END IF
!!!!        !
!!!          WRITE(IOUT,'(/3A,//33x 2A,/33x 2A,//A/)')      REPEAT('#',35),
!!!     +    '...FASTFORWARD OPTION TURNED ON...',REPEAT('#',35),
!!!     +    '     THE STARTING STRESS PERIOD WILL BE: ', NUM2STR(SPSTART),
!!!     +    '     THE ENDING   STRESS PERIOD WILL BE: ', NUM2STR(SPEND),
!!!     +    REPEAT('#',104)
!!!          !
!!!!        CASE('LISTSPLIT')                   !seb ADD NEW OPTION TO SPECIFY PRINT OUT OF ERRORS THAT EXCEDE A MINIMUM PRECENT
!!!!             CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,2,N,R,IOUT,INBAS)
!!!!             LISTSPLIT%MAXSIZE = N
!!!!             WRITE(IOUT,'(1x,4A)')
!!!!     +         'LIST WILL BE SPLIT TO A NEW FILE WHEN FILE SIZE IS ',
!!!!     +           'APPROXIMATELY ',NUM2STR(N),' MB'
!!!        CASE DEFAULT
!!!                   IF(HAS_OPT_LINE .AND. KEY == BLNK) EXIT  !Reached end of old Options Line
!!!                   !
!!!                   ELINE = ELINE//BL%LINE//BLN
!!!        END SELECT
!!!        !
!!!        IF(NO_OPT_LINE) CALL BL%NEXT()
!!!      END DO
!!!      !
!!!      IF (ELINE .NE. BLNK) THEN
!!!       CALL WARNING_MESSAGE(INFILE=INBAS,OUTPUT=IOUT, MSG=
!!!     +'BAS OPTION WARNING.'//NL//'FAILED TO APPLY SOME OF THE OPTIONS'//
!!!     +' FOUND WITHIN THE OPTIONS BLOCK.'//NL//
!!!     +'THEY WERE NOT RECONIZED AS ONE OF THE BAS PACKAGE OPTIONS.'//NL//
!!!     + 'THE FOLLOWING ARE THE OPTIONS THAT WILL BE IGNORED:'//
!!!     +     BLN//ADJUSTL(ELINE))
!!!        ELINE = BLNK
!!!      END IF
!!!
!!!       !CALL READ_TO_DATA(LINE,INBAS,IOUT)
!!!      !ELSE
!!!      !    BACKSAPCE(INBAS)
!!!      END IF
C-----
      !
      !!!IF(NO_CONVERGENCE_STOP) STOPER = 1E30 !SET TO VERY LARGE VALUE SO CODE NEVER STOPS
      !
      IF(SPSTART>NPER) THEN
       WRITE(IOUT,'(/3A,//28x A,/32x A,//A/)')       REPEAT('#',35),
     + '...INPUT_CHECK OPTION TURNED ON...',REPEAT('#',35),
     + 'OneWater WILL CYCLE THROUGH ALL STRESS PERIOD INPUTS',
     + 'BUT WILL NOT SOLVE FOR THE STRESS PERIOD HEAD.',
     + REPEAT('#',104)
      END IF
      IF(INPUT_CHECK) THEN
          NOCBC=2  !DISABLE CBC WHEN DOING INPUTCHECK
      END IF
C                                                                       !seb ALLOCATE MEMORY FOR PARAMETER VAIRABLES
      WRITE(IOUT,'(/,1x,A,3(/,1x,A,I10),/)')
     +   'THE MAX STORAGE FOR PROPERTIES DEFINED BY PARAMETERS IS: ',
     +   'MAX STORAGE OF PARAMETERS (MXPAR)  IS ', MXPAR,
     +   'MAX STORAGE OF INSTANCES  (MXINST) IS ', MXINST,
     +   'MAX STORAGE OF CLUSTERS   (MXCLST) IS ', MXCLST
      ALLOCATE (B(MXPAR))
      ALLOCATE (IACTIVE(MXPAR))
      ALLOCATE (IPLOC(4,MXPAR))
      ALLOCATE (IPCLST(14,MXCLST))
      ALLOCATE (PARNAM(MXPAR))
      ALLOCATE (PARTYP(MXPAR))
      ALLOCATE (INAME(MXINST))
C
C4------Initialize parameter definition variables.
      IPSUM=Z
      ICLSUM=Z
      INAMLOC=1
      DO 10 N=1,MXPAR
        PARNAM(N)=' '
        PARTYP(N)=' '
        IPLOC(1,N)=Z
        IPLOC(2,N)=Z
        IACTIVE(N)=Z
   10 CONTINUE
C
C13-----WRITE OUT TIME STEP START AND END IF REQUESTED
      IF(TIME_INFO%IS_OPEN) THEN
        IU =  TIME_INFO%IU
        IF  (HAS_STARTDATE) THEN
            WRITE(IU,'(2A)') '   STEP     SP     TS           ',
     +                       'DELT         SIMTIM          DYEAR   DATE'
            WRITE(IU,'(3I7,2A15,F15.7,3x A)') 0, 0, 0,'0.0','0.0',
     +                 DATE_SP(1)%TS(0)%DYEAR,
     +                 DATE_SP(1)%TS(0)%STR('T')
        ELSE
            WRITE(IU,'(2A)') '   STEP     SP     TS           ',
     +                       'DELT         SIMTIM'
        END IF

        K = 0
        DTMP = 0D0
        DO I=1, NPER
          DO J=1, NSTP(I)
               K = K + 1
               DTMP = DTMP + SPTIM(I)%DT(J) !TOTAL TIME
               !
               IF  (HAS_STARTDATE) THEN
                   WRITE(IU,'(3I7,2A15,F15.7,3x A)') K, I, J,
     +                        NUM2STR(SPTIM(I)%DT(J),15),
     +                        NUM2STR(DTMP,15),
     +                        DATE_SP(I)%TS(J)%DYEAR,
     +                        DATE_SP(I)%TS(J)%STR('T')
               ELSE
                   WRITE(IU,'(3I7,2A15)') K, I, J,
     +                        NUM2STR(SPTIM(I)%DT(J),15),
     +                        NUM2STR(DTMP,15)
               END IF
          END DO
        END DO
      END IF
C
C8C-----PRINT A MESSAGE SHOWING OPTIONS.
        IF(IXSEC.NE.Z) WRITE(IOUT,61)
   61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
        IF(ICHFLG.NE.Z) WRITE(IOUT,62)
   62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
C
C8D-----INITIALIZE TOTAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER
C8D-----AND CALCULATE NUMBER OF CELLS.
      TOTIM=0.
      SIMTIME = 0D0
C
C8E-----READ BOUNDARY ARRAY(IBOUND).
      IF(IXSEC.EQ.Z) THEN
         DO 280 K=1,NLAY
         KK=K
         CALL U2DINT(IBOUND(:,:,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT)
  280    CONTINUE
      ELSE
         CALL U2DINT(IBOUND(:,:,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C CHECK FOR VALID MODEL DIMENSIONS NOW THAT IBOUND,DELR,DELC,BOTM HAS BEEN ESTABLISHED seb
      DIMTOL=1E-5
      CALL CHECK_FOR_VALID_DIMENSIONS(DIMTOL,INBAS,IOUT,NROW,NCOL,NLAY,
     +                             IBOUND,LBOTM,BOTM,DELR,DELC,WARN_DIM)
C
      !WETCEL=IBOUND.NE.0                                                !seb establish logical variable that is TRUE for wet cells and false for dry cells
C
C8F-----READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.Z) THEN
         READ(INBAS,'(F10.0)') HNF
      ELSE
         READ(INBAS,*) HNF
      END IF
      !HNF=HNOFLO
      HNOFLO=HNF
        WRITE(IOUT,3) NUM2STR(HNOFLO)
    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',A,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).',/)
C
C8G-----READ INITIAL HEADS.
      IF(IXSEC.EQ.Z) THEN
         DO 300 K=1,NLAY
         KK=K
         CALL U2DREL(STRT(:,:,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT)
  300    CONTINUE
      ELSE
         CALL U2DREL(STRT(:,:,1),ANAME(2),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C9------COPY INITIAL HEADS FROM STRT TO HNEW.
!      DO 400 K=1,NLAY
!      DO 400 I=1,NROW
!      DO 400 J=1,NCOL
      IF(ALLOCATED(HSHIFT)) THEN
      DO CONCURRENT(K=1:NLAY,I=1:NROW,J=1:NCOL,STRT(J,I,K)==STRT(J,I,K))
          !
          STRT(J,I,K) = STRT(J,I,K) + HSHIFT(K)
      END DO
      END IF
      !
      DO CONCURRENT(K=1:NLAY,I=1:NROW,J=1:NCOL)
      IF(STRT(J,I,K).NE.STRT(J,I,K)) THEN
            STRT(J,I,K) = HNF
            HNEW(J,I,K) = HNF
            IF(IBOUND(J,I,K).NE.Z) IBOUND(J,I,K) = Z
      ELSE
            HNEW(J,I,K)=STRT(J,I,K)
            IF(IBOUND(J,I,K).EQ.Z) HNEW(J,I,K)=HNF
      END IF
      END DO
!  400 CONTINUE
C
C10-----SET UP OUTPUT CONTROL.
      CALL SGWF2BAS7I(NLAY,IUNIT(IUOC),IOUT,IFREFM,MXBUD)
C
C11-----INITIALIZE VOLUMETRIC BUDGET ACCUMULATORS TO ZERO.
  590 ZERO=0.
      DO 600 I=1,MXBUD
      DO 600 J=1,4
      VBVL(J,I)=ZERO
  600 CONTINUE
C
C12-----Allocate and read Zone and Multiplier arrays
      CALL SGWF2BAS7ARMZ(IUNIT(IUZON),IUNIT(IUMLT))
C
C13-----READ PARAMETER VALUES FILE.
      CALL SGWF2BAS7ARPVAL(IUPVAL)
      !
      !
      IF(SUPER_NAMES_IN%IU.NE.Z) THEN
       CALL LOAD_SUPER_NAMES(LINE, SUPER_NAMES_IN%IU, IOUT, SUPER_NAMES)
      END IF
C
C14-----SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2BAS7PSV(IGRID)
      RETURN
      END SUBROUTINE
      SUBROUTINE GWF2BAS7ST(KPER,IGRID)
C     ******************************************************************
C     SETUP TIME VARIABLES FOR NEW TIME PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,PERLEN,NSTP,TSMULT,ITMUNI,SPTIM
      USE GWFBASMODULE,ONLY:DELT,PERTIM,DATE_SP,HAS_STARTDATE
      USE GWFBASMODULE,ONLY:REALTIM,USE_LEAP_YR
      USE GWFBASMODULE,ONLY:REALTIM_PER,SIMTIM_PER,TOTPERTIM
      USE GWFBASMODULE,ONLY:MIN_ITER_INPUT,   MIN_SOLVER_INTER_SP,
     +                      MIN_SOLVER_INTER, MIN_SOLVER_INTER_NEW
      USE PARAMMODULE, ONLY:MXPAR,MXINST,MXCLST,IPSUM,INAMLOC,ICLSUM    !seb ADDED MODULE CONNECTION
      USE BAS_UTIL,    ONLY: DECIMAL_YEAR
      USE UTIL_INTERFACE, ONLY: WARNING_MESSAGE,
     +                          READ_TO_DATA, GET_INTEGER
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
      !
C
C1------WRITE STRESS PERIOD INFORMATION
        WRITE (IOUT,1) KPER,PERLEN(KPER),NSTP(KPER),TSMULT(KPER)
    1   FORMAT('1',/28X,'STRESS PERIOD NO. ',I6,', LENGTH =',G15.7,/
     1            28X,47('-'),//
     2            30X,'NUMBER OF TIME STEPS =',I6,//
     3            31X,'MULTIPLIER FOR DELT =',F10.3)
C
C2------CALCULATE THE LENGTH OF THE FIRST TIME STEP.
        DELT= SPTIM(KPER)%DT(1)
C
C2A-----ASSUME TIME STEP MULTIPLIER IS EQUAL TO ONE.
!!!      DELT=PERLEN(KPER)/FLOAT(NSTP(KPER))
!!!C
!!!C2B-----IF TIME STEP MULTIPLIER IS NOT ONE THEN CALCULATE FIRST
!!!C2B-----TERM OF GEOMETRIC PROGRESSION.
!!!      ONE=1.
!!!      IF(TSMULT(KPER).NE.ONE)
!!!     1    DELT=PERLEN(KPER)*(ONE-TSMULT(KPER))/
!!!     2        (ONE-TSMULT(KPER)**NSTP(KPER))
!!!C
!!!      IF(SPTIM(KPER)%SPECIFY_DELT) DELT= SPTIM(KPER)%DT(1)
C
C3------PRINT THE LENGTH OF THE FIRST TIME STEP.
        WRITE (IOUT,9) DELT
    9 FORMAT(1X,/28X,'INITIAL TIME STEP SIZE =',G15.7)
C
C4------INITIALIZE PERTIM (ELAPSED TIME WITHIN STRESS PERIOD).
      PERTIM=0.
C
C5------CHECK THAT ALL PARAMETERS IN PARAMETER VALUE FILE HAVE BEEN DEFINED.
      IF(KPER.EQ.1) CALL SGWF2BAS7STPVAL()                              !seb CHANGED FROM KPER.GT.1
C seb PRINT OUT MAX PARAMETER USAGE
      IF(KPER.EQ.1) THEN
        WRITE(IOUT,'(/A//,A,3(/,A,I10,A,I10),//,A,/,A,//A,/A,/A,3I10/)')
     +'PARAMETERS HAVE BEEN USED IN PACKAGES',
     +'THE CURRENT STORAGE FOR PARAMETERS IS: ',
     +'No. PARAMETER=',IPSUM,    ' WITH MAX STORAGE (MXPAR)  OF',MXPAR,
     +'No. CLUSTERS =',ICLSUM,   ' WITH MAX STORAGE (MXCLST) OF',MXCLST,
     +'No. INSTANCES=',INAMLOC-1,' WITH MAX STORAGE (MXINST) OF',MXINST,
     +'YOU CAN INCREASE/DECREASE STORAGE WITH BAS OPTION:',
     + 'MAXPARAM  MXPAR  MXCLST MXINST',
     +'FYI, YOU CAN REDUCE RAM USUAGE WITH BAS OPTION',
     +  '(NOTE THE THREE NUMBERS ARE A MINUMUM SIZE):',
     +  '   MAXPARAM ',IPSUM+1,ICLSUM+1, INAMLOC
      END IF
C
C
C BUILD END PERIOD TIMES:
      TOTPERTIM=PERLEN(KPER)
      SIMTIM_PER = SIMTIM_PER + TOTPERTIM
      !
      IF  (HAS_STARTDATE) THEN
          REALTIM_PER = DATE_SP(KPER)%TS(NSTP(KPER))%DYEAR
      ELSEIF(REALTIM.GE.0D0)THEN !.AND.ISSFLG(KPER).EQ.0)                  !seb  ADDED POTENTIAL SKIPPING OF SS SP
          CALL DECIMAL_YEAR(REALTIM_PER,TOTPERTIM,ITMUNI,USE_LEAP_YR)
      END IF
      !
      ! UPDATE MINIMUM SOLVER ITERATIONS IF REQUESTED
      !
      IF( MIN_ITER_INPUT%IU .NE. 0) THEN          !CAN LOAD SOLVER INPUT
       IF(MIN_SOLVER_INTER_SP <= KPER) THEN       !UPDATE SOLVER
         BLOCK
           CHARACTER(120):: LINE
           LOGICAL:: EOF
           INTEGER:: IU, LLOC, ISTART, ISTOP
           !
           IU = MIN_ITER_INPUT%IU
           !
           DO WHILE (MIN_SOLVER_INTER_SP <= KPER)
             !
             MIN_SOLVER_INTER = MIN_SOLVER_INTER_NEW
             !
             CALL READ_TO_DATA(LINE,IU,IOUT,EOF=EOF)
             !
             IF(EOF) THEN
                 CALL MIN_ITER_INPUT%CLOSE()
                 EXIT
             ELSE
                LLOC = 1
                CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IU,
     +             MIN_SOLVER_INTER_SP,MSG=
     +            'BAS OPTION "MIN_SOLVER_ITER" EXTERNAL FILE '//
     +            'FAILED TO LOAD STRESS PERIOD NUMBER')
                !
                CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IU,
     +             MIN_SOLVER_INTER_NEW,MSG=
     +            'BAS OPTION "MIN_SOLVER_ITER" EXTERNAL FILE '//
     +            'FAILED TO MIN SOLVER ITERATION NUMBER')
             END IF
           END DO
         END BLOCK
       END IF
      END IF
      !
      ! SET UP WARNING FILE HEADER
      CALL WARNING_MESSAGE(KPER=KPER)
      !
      !IF(SPTIM(KPER)%SPECIFY_DELT) THEN
      !  DDELT = SUM(SPTIM(KPER)%DT)
      !  SIMTIM_PER = SIMTIM_PER + DDELT
      !  IF(REALTIM.GE.0D0) THEN
      !      CALL DECIMAL_YEAR(REALTIM_PER,DDELT,ITMUNI,USE_LEAP_YR)
      !  END IF
      !ELSE
      !  DDELT=DELT
      !  DO I=1, NSTP(KPER)
      !    !
      !    SIMTIM_PER = SIMTIM_PER + DDELT
      !    !
      !    IF(REALTIM.GE.0D0) THEN
      !      CALL DECIMAL_YEAR(REALTIM_PER,DDELT,ITMUNI,USE_LEAP_YR)
      !    END IF
      !  END DO
      !END IF
      !
C6------RETURN
      RETURN
      END SUBROUTINE
      SUBROUTINE GWF2BAS7AD(KPER,KSTP,IGRID)
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,   ONLY:Z, ONE, inf_I,FALSE, BLN, NL, D100
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,TSMULT,HNEW,HOLD,ITMUNI,
     +                     SPTIM, HNEW_OLD, IOUT, GSE,BOTM,LBOTM,IBOUND,
     +                     GLOBALDAT
      USE GWFBASMODULE,ONLY:DELT,TOTIM,PERTIM,REALTIM,USE_LEAP_YR,
     +                      DATE_SP,DEALLOCATE_MULT, SIMTIME,
     +                      ADAMP_INPUT,BAS_ADAMP,MAX_REL_VOL_INVOKED,
     +                      HAS_STARTDATE
      USE PARAMMODULE,ONLY:NMLTAR,MLTNAM,RMLT!,NZONAR,ZONNAM
      USE BAS_UTIL,    ONLY: DECIMAL_YEAR
      USE UTIL_INTERFACE, ONLY: READ_TO_DATA, GET_INTEGER,
     +                          SET_ARRAY, WARNING_MESSAGE
      INTEGER:: YEAR
      LOGICAL:: LEAPYR
      DOUBLE PRECISION::DDELT
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
      !
      ! Initialize flag for RELATIVE VOLUME ICNVG FLAG CHANGE
      MAX_REL_VOL_INVOKED = FALSE
      !
      IF(DEALLOCATE_MULT) THEN
          IF(KPER==ONE.AND.KSTP==ONE) THEN
               IF(NMLTAR.GT.Z) THEN
                    DEALLOCATE (MLTNAM,RMLT)
                    ALLOCATE (MLTNAM(ONE))
                    ALLOCATE (RMLT(ONE,ONE,ONE))
                    CALL SGWF2BAS7PSV(IGRID)
               ENDIF
               DEALLOCATE_MULT = .FALSE.
          END IF
      END IF
      !
      IF(KPER==ONE.AND.KSTP==ONE) THEN
      IF( GSE(1,1).NE.GSE(1,1) ) THEN
          CALL WARNING_MESSAGE(OUTPUT=IOUT,
     +    MSG='DIS PACKAGE: THE GROUND SURFACE ELEVATION WAS NOT '//
     +    'SPECIFIED WITH THE KEYWORD "SURFACE_ELEVATION"'//BLN//
     +    'BEFORE LOADING THE "TOP" ARRAY FOR LAYER 1'//NL//
     +    'NOR WAS IT DEFINED WITH ANOTHER PACKAGE   (e.g. FMPs '//
     +    'GLOBAL DIMENSION KEYWORD "SURFACE_ELEVATION")'//BLN//
     +    'OneWater DIS WILL SET THE GROUND SURFACE ELEVATION '//NL//
     +    'TO THE TOP ELEVATION OF THE UPPER MOST ACTIVE CELL.')
          !
          GLOBALDAT(IGRID)%GSE=>NULL()
          DEALLOCATE(GSE)
          ALLOCATE(GSE(NCOL,NROW))
          GLOBALDAT(IGRID)%GSE=>GSE
          !
          DO I = ONE, NROW
          DO J = ONE, NCOL
                GSE(J,I) = D100
                DO K=ONE, NLAY
                             IF(IBOUND(J,I,K).NE.Z) THEN
                                 !
                                 GSE(J,I) = BOTM(J,I,LBOTM(K)-1)
                                 EXIT
                                 !
                             END IF
                END DO
          END DO
          END DO
          !
      END IF
      END IF
      !
      IF(KSTP==ONE) THEN
        IF(ADAMP_INPUT%IU.NE.Z) THEN  !IS NONZERO IF FILE IS OPEN
          BLOCK
            CHARACTER(96):: LINE
            INTEGER:: LLOC,ISTART,ISTOP
            !
            CALL READ_TO_DATA(LINE,ADAMP_INPUT%IU,IOUT)
            !
            LLOC = ONE
            CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,ADAMP_INPUT%IU,
     +         BAS_ADAMP,MSG='ADVANCED_DAMPING ERROR -- '//
     +        'FAILED TO LOAD THE STARTING OUTER ITERATION FROM FILE')
            IF(BAS_ADAMP <  Z) BAS_ADAMP = inf_I
            IF(BAS_ADAMP < 24) BAS_ADAMP = 24
          END BLOCK
        END IF
      END IF

C
C1------IF NOT FIRST TIME STEP THEN CALCULATE TIME STEP LENGTH.
      DDELT=SPTIM(KPER)%DT(KSTP)
      DELT= SNGL(DDELT)
      !IF(KSTP.NE.1) THEN
      !   IF(SPTIM(KPER)%SPECIFY_DELT) THEN
      !       DELT= SNGL(SPTIM(KPER)%DT(KSTP))
      !   ELSE
      !       DELT=TSMULT(KPER)*DELT
      !   END IF
      !END IF
C
C2------ACCUMULATE ELAPSED TIME IN SIMULATION(TOTIM) AND IN THIS
C2------STRESS PERIOD(PERTIM).
      SIMTIME = SIMTIME + DDELT
      TOTIM=TOTIM+DELT
      PERTIM=PERTIM+DELT
      IF  (HAS_STARTDATE) THEN
          REALTIM = DATE_SP(KPER)%TS(KSTP)%DYEAR
      ELSEIF(REALTIM.GE.0D0)THEN !.AND.ISSFLG(KPER).EQ.Z)                  !seb  ADDED POTENTIAL SKIPPING OF SS SP
          CALL DECIMAL_YEAR(REALTIM,DDELT,ITMUNI,USE_LEAP_YR)
      END IF
C
C3------COPY HNEW TO HOLD.
!      DO 10 K=1,NLAY
!      DO 10 I=1,NROW
!      DO 10 J=1,NCOL
!   10 HOLD(J,I,K)=HNEW(J,I,K)
      !
      CALL SET_ARRAY(NCOL,NROW,NLAY,HNEW,HOLD)
      !
      CALL SET_ARRAY(NCOL,NROW,NLAY,HNEW,HNEW_OLD)  !HOLDS PREVIOUS INTERATION VALUE
C
C4------RETURN
      RETURN
      END SUBROUTINE
      SUBROUTINE GWF2BAS7FM(IGRID)
C     ******************************************************************
C     SET HCOF=RHS=0.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: HCOF, RHS, NROW, NCOL, NLAY
      USE CONSTANTS, ONLY: DZ
      USE UTIL_INTERFACE, ONLY:SET_ARRAY
C     -----------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------FOR EACH CELL INITIALIZE HCOF AND RHS ACCUMULATORS.
      !
      CALL SET_ARRAY(NCOL,NROW,NLAY,DZ,HCOF)
      CALL SET_ARRAY(NCOL,NROW,NLAY,DZ, RHS)
      !
      !CALL GWF2BAS7UPLAY(0) MOVED TO BAS_POST_SOLVER
C
C2------RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7UPLAY(IGRID)                                 !SET UPLAY, UPLAY_IDX, AND WTABLE
      USE GLOBAL,    ONLY: NROW, NCOL, NLAY, HNEW, IBOUND,
     +                     BOTM, LBOTM, LAYHDT, UPLAY, UPLAY_IDX,
     +                     WTABLE, WTLAY
      IMPLICIT NONE
      INTEGER,INTENT(IN)::IGRID
      !
      IF(IGRID.NE.0) CALL SGWF2BAS7PNT(IGRID)
      !
      CALL GWF2BAS7UPLAY_PASS(NROW,NCOL,NLAY,HNEW,IBOUND,
     +                   BOTM,LBOTM,LAYHDT,UPLAY,UPLAY_IDX,WTLAY)
      !
      CALL GWF2BAS7WTABLE(NROW,NCOL,NLAY,WTLAY,HNEW,WTABLE)
      !
      END SUBROUTINE
      !
      PURE SUBROUTINE GWF2BAS7WTABLE(NROW,NCOL,NLAY,WTLAY,HNEW,WTABLE)
      !
      USE CONSTANTS, ONLY: ninf
      !
      IMPLICIT NONE
      INTEGER,       INTENT(IN):: NCOL,NROW,NLAY
      INTEGER,          DIMENSION(NCOL,NROW     ), INTENT(IN   )::WTLAY
      DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::HNEW
      DOUBLE PRECISION, DIMENSION(NCOL,NROW     ), INTENT(  OUT)::WTABLE
      !
      INTEGER:: I, J
      !
C2------SET HEAD FOR UPPER MOST ACTIVE/NONDRY LAYER
      !
      WHERE (WTLAY==0); WTABLE = ninf
      END WHERE
      !
      DO CONCURRENT(I=1:NCOL, J=1:NROW, WTLAY(I,J)>0 )
           !
           WTABLE(I,J) = HNEW(I,J,WTLAY(I,J))
      END DO
      !DO CONCURRENT (I=1:NCOL,J=1:NROW)
      !    IF(UPLAY(I,J)==0) THEN
      !                     WTABLE(I,J) = ninf
      !    ELSE
      !                     WTABLE(I,J) = HNEW(I,J,UPLAY(I,J))
      !    END IF
      !END DO
      !
      END SUBROUTINE
      !
      PURE SUBROUTINE GWF2BAS7UPLAY_PASS(NROW,NCOL,NLAY,HNEW,IBOUND,    !--MAY HAVE COMPILATION ISSUES WHEN NOT USING DOUBLE PRECISION COMPIATION
     +                         BOTM,LBOTM,LAYHDT,UPLAY,UPLAY_IDX,WTLAY)
      !
      USE CONSTANTS,       ONLY: Z,ONE,TWO,THREE
      USE ARRAY_DATA_TYPES,ONLY: INTEGER_MATRIX
      !
      IMPLICIT NONE
      INTEGER, INTENT(IN):: NROW,NCOL,NLAY
      DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::HNEW
      DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::BOTM
      INTEGER,          DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::IBOUND
      INTEGER,          DIMENSION(          NLAY), INTENT(IN   )::LBOTM
      INTEGER,          DIMENSION(          NLAY), INTENT(IN   )::LAYHDT
      INTEGER,          DIMENSION(NCOL,NROW     ), INTENT(INOUT)::UPLAY
      TYPE(INTEGER_MATRIX),                   INTENT(INOUT):: UPLAY_IDX
      INTEGER,          DIMENSION(NCOL,NROW     ), INTENT(INOUT)::WTLAY
      !
      INTEGER:: I, J, K, N, UP, MXDIM
      !
C2------CALCULATE UPPER MOST ACTIVE/NONDRY LAYER
      !
      DO CONCURRENT(I=1:NCOL,J=1:NROW);  UPLAY(I,J)=Z
      END DO
      !
      DO K=ONE,NLAY
          WHERE(UPLAY==Z .AND. IBOUND(:,:,K).NE.Z )
                UPLAY = K
          END WHERE
      END DO
      !
      DO CONCURRENT(I=1:NCOL,J=1:NROW);  WTLAY(I,J) = UPLAY(I,J)
      END DO
      !
      DO CONCURRENT(J=1:NROW, I=1:NCOL, UPLAY(I,J) > Z) !Assumes that there are no convertible layers beneath confine layers
          UP = UPLAY(I,J)
          N  = LBOTM(UP)
          IF(LAYHDT(UP).NE.Z           ) THEN
          IF(HNEW(I,J,UP) < BOTM(I,J,N)) THEN
              DO K=UP+ONE,NLAY
                  IF(IBOUND(I,J,K).NE.Z) THEN
                     WTLAY(I,J) = K
                     N = LBOTM(K)
                     IF(LAYHDT(K)==Z             ) EXIT
                     IF(HNEW(I,J,K) > BOTM(I,J,N)) EXIT
                  END IF
               END DO
          END IF
          END IF
      END DO
      !
!      DO CONCURRENT (I=ONE:NCOL,J=ONE:NROW)  !INDEXING DOES NOT MATTER CAUSE OF LAYER JUMPING (K) -- 65 + 6
!       DO K=ONE,NLAY
!         IF(LAYHDT(K) == Z .AND. IBOUND(I,J,K).NE.Z) THEN
!             UPLAY(I,J) = K
!             EXIT
!         ELSEIF( LAYHDT(K).NE.Z .AND.
!     +   (IBOUND(I,J,K).NE.Z .AND. HNEW(I,J,K)>BOTM(I,J,LBOTM(K))) )THEN
!             UPLAY(I,J) = K
!             EXIT
!         END IF
!       END DO
!      END DO
      !
      MXDIM = COUNT(UPLAY.NE.Z)
      !
      IF(MXDIM.NE.UPLAY_IDX%M) CALL UPLAY_IDX%ALLOC(THREE, MXDIM)
      !
      N = Z
      DO K=ONE, NLAY
         DO J=ONE,NROW
         DO I=ONE,NCOL
               IF(UPLAY(I,J)==K) THEN
                   N = N + ONE
                   UPLAY_IDX%MAT(ONE,  N) = K
                   UPLAY_IDX%MAT(TWO,  N) = J
                   UPLAY_IDX%MAT(THREE,N) = I
               END IF
         END DO
         END DO
         IF(MXDIM == N) EXIT
      END DO
      !
      END SUBROUTINE
      !
!!!      PURE SUBROUTINE UPLAY_ONE_LAYER_0(NROW,NCOL,IBOUND,UPLAY)
!!!      !
!!!      IMPLICIT NONE
!!!      INTEGER, INTENT(IN):: NROW,NCOL
!!!      INTEGER,          DIMENSION(NCOL,NROW), INTENT(IN   )::IBOUND
!!!      INTEGER,          DIMENSION(NCOL,NROW), INTENT(INOUT)::UPLAY
!!!      !
!!!          WHERE(UPLAY==0 .AND. IBOUND.NE.0 )
!!!                UPLAY = K
!!!          END WHERE
!!!      !
!!!      END SUBROUTINE
!!!      !
!!!      PURE SUBROUTINE UPLAY_ONE_LAYER_1(I,J,NROW,NCOL,HNEW,IBOUND,
!!!     +                                  BOTM, UPLAY)
!!!      !
!!!      IMPLICIT NONE
!!!      INTEGER, INTENT(IN):: NROW,NCOL
!!!      DOUBLE PRECISION, DIMENSION(NCOL,NROW), INTENT(IN   )::HNEW
!!!      DOUBLE PRECISION, DIMENSION(NCOL,NROW), INTENT(IN   )::BOTM
!!!      INTEGER,          DIMENSION(NCOL,NROW), INTENT(IN   )::IBOUND
!!!      INTEGER,          DIMENSION(NCOL,NROW), INTENT(INOUT)::UPLAY
!!!      !
!!!      INTEGER, INTENT(INOUT):: I, J
!!!      !
!!!     +DO CONCURRENT(I=1:NCOL,J=1:NROW, UPLAY(I,J)==0
!!!     +                        .AND. IBOUND(I,J).NE.0 )
!!!           IF( HNEW(I,J) > BOTM(I,J)) )THEN
!!!              UPLAY(I,J) = K
!!!           END IF
!!!       END DO
!!!      !
!!!      END SUBROUTINE
      !
      SUBROUTINE BAS_PRE_SOLVER(IGRID, KPER, KSTP, KITER)
C     ******************************************************************
C     COPY PREVIOUS ITERATIONS HEAD TO PREVIOUS FOR USE BY PACKAGES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE UTIL_INTERFACE, ONLY:SET_ARRAY
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,HNEW_OLD,IBOUND
      USE GWFBASMODULE,ONLY:GWFBASDAT!,BAS_ADAMP,HED_CHNG2,HED_CHNG3
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID, KPER, KSTP, KITER
      INTEGER:: I, J, K
      REAL,DIMENSION(:,:,:),POINTER, CONTIGUOUS:: PNT
C     ------------------------------------------------------------------
      !
       !
      !IF(KITER >= BAS_ADAMP - 4) THEN
      !    !
      !    PNT       => HED_CHNG3
      !    HED_CHNG3 => HED_CHNG2
      !    HED_CHNG2 => PNT
      !    PNT       => NULL()
      !    !
      !    DO CONCURRENT(K=1:NLAY,I=1:NROW,J=1:NCOL)
      !        !
      !        IF(IBOUND(J,I,K)>0) THEN
      !          HED_CHNG2(J,I,K) = SNGL(HNEW(J,I,K) - HNEW_OLD(J,I,K))
      !        ELSE
      !          HED_CHNG2(J,I,K) = 0.0
      !        END IF
      !        !
      !    END DO
      !    !
      !    GWFBASDAT(IGRID)%HED_CHNG2 => HED_CHNG2   !REUPDATE GLOBAL POITNERS
      !    GWFBASDAT(IGRID)%HED_CHNG3 => HED_CHNG3
      !END IF
      !
      CALL SET_ARRAY(NCOL,NROW,NLAY,HNEW,HNEW_OLD)
      !
C
C4------RETURN
      END SUBROUTINE
      !
      SUBROUTINE BAS_POST_SOLVER(KPER, KSTP, KITER, MXITER, ICNVG)      !ASSUMES CORRECT LGR GRID IS SET
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,ONLY: NEG,Z,ONE,DZ,FOURTH,HALF,TRES,TRUE,FALSE,D100,
     +                    TENTH,CENTI,MILLI,DOS,DIEZ,UNO,NEARZERO_12
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,HNEW_OLD,IBOUND,
     +                      BOTM,LBOTM,AREA,SPTIM,GSE,
     +                      CELL_MASS_BALANCE, RELATIVE_MASS_ERROR,
     +                      MAX_RELATIVE_VOL_ERROR, CELL_VOL_ERROR
      USE GWFBASMODULE,ONLY:PRNT_CNVG,PRNT_CNVG_OUTER,PRNT_CNVG_NTERM,
     +                      PRNT_CNVG_LRC, PRNT_CNVG_DIF,
     +                      PRNT_FRES,PRNT_FRES_OUTER,PRNT_FRES_NTERM,
     +                      PRNT_FRES_LRC, PRNT_FRES_DIF,
     +                      PRNT_VERR,PRNT_VERR_OUTER,PRNT_VERR_NTERM,
     +                      PRNT_VERR_LRC, PRNT_VERR_DIF,
     +                      DATE_SP,HAS_STARTDATE,
     +                      BAS_ADAMP,BAS_ADAMP_TOL,BAS_ADAMP_TOL2,
     +                      HED_CHNG2,HED_CHNG3,HED_LOCK,
     +                      MAX_REL_VOL_ERROR, MAX_REL_VOL_INVOKED,
     +                      MIN_SOLVER_INTER, DAMPEN_START,
     +                      DAMPEN_START_ITR, DAMPEN_START_DMP,
     +                      OSCIL_DMP_OUTER,OSCIL_DMP_LRC,OSCIL_DMP_DIF,
     +                     ABOVE_GSE_LIM,ABOVE_GSE_PRT_LIM,ABOVE_GSE_PRT
      USE UTIL_INTERFACE,    ONLY: LRC_TO_CELLID, CELLID_TO_LRC
      USE NUM2STR_INTERFACE, ONLY: NUM2STR, NUM2STR7
      USE BAS_UTIL,    ONLY: RELAX_HNEW, TOP_LIM_HNEW,PRINT_TOP_LIM_HNEW
      USE SORT_INTERFACE
      USE GWFSFRMODULE, ONLY: SFR_IN_USE, SFR_FRES
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: KPER, KSTP, KITER, MXITER
      INTEGER, INTENT(INOUT):: ICNVG
      !
      INTEGER:: I, J, K, II, ID, ITMP, CNT
      CHARACTER(8):: DT
      DOUBLE PRECISION:: DIF, ADIF, DTMP, DELT
      LOGICAL:: CHECK
      !
      DELT = SPTIM(KPER)%DT(KSTP)
      !
      !--------------------------------------------------------------------
      !
      IF(ICNVG==ONE) THEN
                     IF(KITER < MIN_SOLVER_INTER)  ICNVG = Z
      END IF
      !
      !--------------------------------------------------------------------
      !
      IF(DAMPEN_START) THEN
       IF(KPER==ONE .AND. KSTP==ONE .AND. KITER<DAMPEN_START_ITR)THEN
           !
           CALL RELAX_HNEW(NCOL,NROW,NLAY, DAMPEN_START_DMP,
     +                                           IBOUND, HNEW_OLD, HNEW)
!!!       DO CONCURRENT (K=ONE:NLAY,I=ONE:NROW,J=ONE:NCOL, IBOUND(J,I,K)>Z)
!!!           !NEW = OLD + RELAX*(NEW-OLD)
!!!           HNEW(J,I,K) = HNEW_OLD(J,I,K) +
!!!     +                   DAMPEN_START_DMP*(HNEW(J,I,K)-HNEW_OLD(J,I,K))
!!!         END DO
         ICNVG = Z
       ELSE
         DAMPEN_START = FALSE
       END IF
      END IF
      !
      !--------------------------------------------------------------------
      !
      IF(ICNVG==ONE .OR. KITER==MXITER) THEN
                         !DTMP =0.01D0
                         !CALL RELATIVE_MASS_ERROR(DIF, DTMP)
                         !
                         CALL MAX_RELATIVE_VOL_ERROR(DIF)
                         !WRITE(*,'(I4,F12.8)') Kiter, DIF
                         !
                         IF(DIF > MAX_REL_VOL_ERROR) THEN
                             ICNVG = Z
                             MAX_REL_VOL_INVOKED = TRUE
                         END IF
      END IF
      !
      !--------------------------------------------------------------------
      !
      IF(OSCIL_DMP_OUTER < Z) THEN
          II = MXITER - OSCIL_DMP_OUTER
      ELSE
          II = OSCIL_DMP_OUTER
      END IF
      !
      IF(KITER >= II .AND. ICNVG==Z .AND. KITER < MXITER) THEN
        !
        ID   = Z
        DTMP = DZ
        DIF  = DZ
        DO K=ONE,NLAY
        DO I=ONE,NROW
        DO J=ONE,NCOL
          IF(IBOUND(J,I,K)>Z) THEN
              ADIF = ABS( HNEW(J,I,K) - HNEW_OLD(J,I,K) )
              IF ( ADIF > DTMP ) THEN
                  DTMP = ADIF
                  DIF = HNEW(J,I,K) - HNEW_OLD(J,I,K)
                  CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
              END IF
          END IF
        END DO
        END DO
        END DO
        !
        IF     (DIF >= DZ .AND. OSCIL_DMP_DIF >= DZ) THEN
                                                       CHECK = FALSE
        ELSEIF (DIF <= DZ .AND. OSCIL_DMP_DIF <= DZ) THEN
                                                       CHECK = FALSE
        ELSEIF (ID .NE. OSCIL_DMP_LRC .OR.  ID == Z) THEN
                                                       CHECK = FALSE
        ELSE
                                                       CHECK = TRUE
        END IF
        !
        IF(CHECK) THEN
          !
          CHECK = FALSE
          DTMP  = ABS(DIF)
          ADIF  = ABS(DIF-OSCIL_DMP_DIF)
          !
          IF    ( DIEZ > DOS   .AND. ADIF < UNO) THEN
                                                   CHECK = TRUE
          ELSEIF( DTMP > UNO   .AND. ADIF < HALF) THEN
                                                   CHECK = TRUE
          ELSEIF( DTMP > TENTH .AND. ADIF < CENTI) THEN
                                                   CHECK = TRUE
          ELSEIF( DTMP > CENTI .AND. ADIF < MILLI) THEN
                                                   CHECK = TRUE
          ELSEIF( DTMP <= CENTI) THEN
                                                   CHECK = TRUE
          END IF
          !
          IF(CHECK) THEN
           !
           CALL RELAX_HNEW(NCOL,NROW,NLAY, 0.52D0, IBOUND,HNEW_OLD,HNEW)
           !
           CALL CELLID_TO_LRC(ID, K, I, J, NLAY, NROW, NCOL)
           !
           DIF = HNEW(J,I,K) - HNEW_OLD(J,I,K)
          END IF
        END IF
        !
        OSCIL_DMP_LRC = ID
        OSCIL_DMP_DIF = DIF
      END IF
      !
      !--------------------------------------------------------------------
      !
      IF(ABOVE_GSE_LIM < D100 .AND. KITER < MXITER) THEN
           !
           CALL TOP_LIM_HNEW(NCOL,NROW,NLAY, ABOVE_GSE_LIM,
     +                       IBOUND, GSE, HNEW, ICNVG)
           !
      END IF
      !
      IF(ABOVE_GSE_PRT%IS_OPEN        ) THEN
      IF(ICNVG==ONE .OR. KITER==MXITER) THEN
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
        ELSE
            DT=''
        END IF
        !
        CALL PRINT_TOP_LIM_HNEW(NCOL,NROW,NLAY,ABOVE_GSE_PRT_LIM,
     +                          IBOUND, GSE, HNEW,
     +                          KPER, KSTP, ABOVE_GSE_PRT%IU, DT)
        !
      END IF
      END IF
      !
      !--------------------------------------------------------------------
      !
!!!      CHECK = ICNVG==Z  --NOT USEFUL
!!!      IF(CHECK .AND. BAS_ADAMP==Z .AND. KITER == MXITER) THEN
!!!                                                              CONTINUE
!!!      ELSEIF(CHECK .AND. BAS_ADAMP>Z) THEN
!!!                        CHECK = KITER >= BAS_ADAMP
!!!      ELSEIF(CHECK) THEN
!!!                        CHECK = KITER >= MXITER + BAS_ADAMP + ONE
!!!      END IF
!!!      !
!!!      IF(CHECK) THEN
!!!        !
!!!        DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
!!!         IF(IBOUND(J,I,K)>Z) THEN
!!!           ASSOCIATE(CHNG2=>HED_CHNG2(J,I,K),CHNG3=>HED_CHNG3(J,I,K),
!!!     +               Hnew => HNEW(J,I,K), Hprev => HNEW_OLD(J,I,K),
!!!     +               LOCK=>HED_LOCK(J,I,K), TOL=>BAS_ADAMP_TOL       )
!!!            !
!!!            DIF = Hnew - Hprev;   ADIF= ABS(DIF)
!!!            !
!!!            IF(LOCK > 3) LOCK = Z     !DISABLE OCCILATION LOCK
!!!            IF(LOCK > 0) THEN         !ASSUME IT IS STILL OCCILATING, SO DAMPEN
!!!                  !
!!!                  Hnew = Hprev + DIF*HALF
!!!                  !
!!!                  IF(ADIF < TOL) LOCK = LOCK + ONE
!!!                  !
!!!            ELSEIF(ADIF > TOL) THEN
!!!                  !
!!!                  IF(    ( CHNG3 >  TOL.AND.CHNG2 < -TOL.AND.DIF >  TOL)!HEAD IS OCCILATING, DAMPEN AND LOCK
!!!     +               .OR.( CHNG3 < -TOL.AND.CHNG2 >  TOL.AND.DIF < -TOL)
!!!     +              ) THEN
!!!                          DTMP =(  ABS(DIF   + CHNG2)
!!!     +                           + ABS(DIF   - CHNG3)
!!!     +                           + ABS(CHNG2 + CHNG3) ) / TRES          !OCCILATIONS ARE ABOUT THE SAME, SO LOCK HEAD FOR FOUR INTERATIONS
!!!                          IF( DTMP < BAS_ADAMP_TOL2) LOCK = ONE
!!!                  END IF
!!!                  !
!!!                  IF(     ( CHNG3 > DZ .AND. CHNG2 < DZ .AND. DIF > DZ )!HEAD IS OCCILATING, DAMPEN AND LOCK
!!!     +               .OR. ( CHNG3 < DZ .AND. CHNG2 > DZ .AND. DIF < DZ )
!!!     +              ) THEN
!!!                          IF(LOCK == ONE) THEN
!!!                              Hnew = Hprev + DIF*HALF    !OCCILATING ABOUT SAME VALUE, LOCK DAMPING
!!!                          ELSE
!!!                              Hnew = Hprev + DIF*FOURTH  !RANDOM OCCILATION -- HEAVY DAMP
!!!                          END IF
!!!                  END IF
!!!            END IF   ! (ADIF > BAS_ADAMP_TOL)
!!!           END ASSOCIATE
!!!         END IF      ! (IBOUND(J,I,K)>Z)
!!!        END DO; END DO; END DO
!!!        !
!!!      END IF
      !
      !--------------------------------------------------------------------
      !
!      IF(ICNVG==Z .AND. BAS_ADAMP > Z) THEN
!       IF(     KITER >= BAS_ADAMP ) THEN
!        DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
!         IF(IBOUND(J,I,K)>Z) THEN
!           ASSOCIATE(CHNG2=>HED_CHNG2(J,I,K),CHNG3=>HED_CHNG3(J,I,K),
!     +               Hnew => HNEW(J,I,K), Hprev => HNEW_OLD(J,I,K),
!     +               LOCK=>HED_LOCK(J,I,K), TOL=>BAS_ADAMP_TOL       )
!            !
!            DIF = Hnew - Hprev
!            !
!            IF(ABS(DIF) > TOL) THEN
!               IF(LOCK > 0) THEN
!                  LOCK = LOCK + ONE
!                  !
!                  SELECT CASE(LOCK)   !ASSUME IT IS STILL OCCILATING, SO DAMPEN
!                  CASE(2,3,4);        Hnew = Hprev + DIF*FOURTH
!                  CASE(5,6,7);        Hnew = Hprev + DIF*HALF
!                  CASE DEFAULT
!                                      LOCK = Z                 !DISABLE OCCILATION LOCK
!                                      CHNG2 = -1.0*SNGL(DIF)   !ALLOW FOR POTENTIAL FUTURE OCCILATION, OTHERWISE IT WOULD BE 4 OUTER BEFORE CHECKING AGIAN
!                  END SELECT
!               ELSE
!                  IF(     ( CHNG3 > DZ .AND. CHNG2 < DZ .AND. DIF > DZ )!HEAD IS OCCILATING, DAMPEN AND LOCK
!     +               .OR. ( CHNG3 < DZ .AND. CHNG2 > DZ .AND. DIF < DZ )
!     +              ) THEN
!                          Hnew = Hprev + DIF*HALF
!                  END IF
!                  !
!                  IF(    ( CHNG3 >  TOL.AND.CHNG2 < -TOL.AND.DIF >  TOL)!HEAD IS OCCILATING, DAMPEN AND LOCK
!     +               .OR.( CHNG3 < -TOL.AND.CHNG2 >  TOL.AND.DIF < -TOL)
!     +              ) THEN
!                          DTMP =(  ABS(DIF   + CHNG2)
!     +                           + ABS(DIF   - CHNG3)
!     +                           + ABS(CHNG2 + CHNG3) ) / TRES          !OCCILATIONS ARE ABOUT THE SAME, SO LOCK HEAD FOR FOUR INTERATIONS
!                          IF( DTMP < BAS_ADAMP_TOL2) LOCK = ONE
!                  END IF
!               END IF ! (LOCK > 0)
!            ELSEIF(LOCK > Z) THEN
!                          LOCK = Z
!            END IF   ! (ABS(DIF) > BAS_ADAMP_TOL)
!           END ASSOCIATE
!         END IF      ! (IBOUND(J,I,K)>Z)
!        END DO; END DO; END DO
!       END IF
!      END IF
      !
      !--------------------------------------------------------------------
      !
      CHECK = PRNT_CNVG%IS_OPEN
      IF(KITER == MXITER.OR.ICNVG==1) THEN
                                       CONTINUE
      ELSEIF(CHECK .AND. PRNT_CNVG_OUTER>Z) THEN
                        CHECK = KITER >= PRNT_CNVG_OUTER
      ELSEIF(CHECK) THEN
                        CHECK = KITER >= MXITER + PRNT_CNVG_OUTER + ONE
      END IF
      !
      IF(CHECK) THEN
        !
        PRNT_CNVG_LRC = Z
        PRNT_CNVG_DIF = DZ
        CNT = Z
        !
        DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
           !
           IF(IBOUND(J,I,K)>Z) THEN
               DIF = ABS(HNEW(J,I,K) - HNEW_OLD(J,I,K))
               CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
               !
               IF(CNT > PRNT_CNVG_NTERM) THEN
                   DO II = ONE, PRNT_CNVG_NTERM
                       IF( DIF > PRNT_CNVG_DIF(II) ) THEN
                         !
                         IF(II<PRNT_CNVG_NTERM) THEN
                           DO ITMP=PRNT_CNVG_NTERM, II+ONE, NEG
                             PRNT_CNVG_LRC(ITMP)=PRNT_CNVG_LRC(ITMP-ONE)
                             PRNT_CNVG_DIF(ITMP)=PRNT_CNVG_DIF(ITMP-ONE)
                           END DO
                         END IF
                         !
                         PRNT_CNVG_LRC(II) = ID
                         PRNT_CNVG_DIF(II) = DIF
                         !
                         EXIT
                       END IF
                   END DO
               ELSE
                   CNT = CNT + ONE
                   PRNT_CNVG_LRC(CNT) = ID
                   PRNT_CNVG_DIF(CNT) = DIF
                   !
                   IF(CNT == PRNT_CNVG_NTERM) THEN
                       CALL SORT(PRNT_CNVG_NTERM,
     +                           PRNT_CNVG_DIF, PRNT_CNVG_LRC)
                       CALL REVERSE_ORDER(PRNT_CNVG_NTERM,PRNT_CNVG_DIF)  !MAKE IT SO ITS LARGEST TO SMALLEST
                       CALL REVERSE_ORDER(PRNT_CNVG_NTERM,PRNT_CNVG_LRC)
                       CNT = CNT + ONE
                   END IF
               END IF
               !IF(CNT < PRNT_CNVG_NTERM) THEN
               !    CNT = CNT + ONE
               !    PRNT_CNVG_LRC(CNT) = ID
               !    PRNT_CNVG_DIF(CNT) = DIF
               !ELSE
               !    DO II = ONE, PRNT_CNVG_NTERM
               !        IF( DIF > PRNT_CNVG_DIF(II) ) THEN
               !            ITMP = PRNT_CNVG_LRC(II)
               !            DTMP = PRNT_CNVG_DIF(II)
               !            !
               !            PRNT_CNVG_LRC(II) = ID
               !            PRNT_CNVG_DIF(II) = DIF
               !            !
               !            ID  = ITMP
               !            DIF = DTMP
               !        END IF
               !    END DO
               !END IF
           END IF
           !
        END DO; END DO; END DO
        !
        CALL SORT(PRNT_CNVG_NTERM, PRNT_CNVG_LRC, PRNT_CNVG_DIF)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, PRNT_CNVG_NTERM
         ID = PRNT_CNVG_LRC(II)
         !
         CALL CELLID_TO_LRC(ID, K, I, J, NLAY, NROW, NCOL)
         !
         DIF = HNEW(J,I,K) - HNEW_OLD(J,I,K)
         !
         IF(PRNT_CNVG%BINARY) THEN
         WRITE(PRNT_CNVG%IU)KPER,KSTP,KITER,K,I,J,HNEW(J,I,K),DIF,DT,ID
         ELSE
         WRITE(PRNT_CNVG%IU,'(2(1x I4),1x I5,3(1x I4),2(1x A),2(2x A))')
     +                  KPER,KSTP,KITER,K,I,J,
     +                  NUM2STR7(HNEW(J,I,K),14), NUM2STR7(DIF,14),DT,
     +                  NUM2STR(ID)
         END IF
        END DO
        !
      END IF !(PRNT_CNVG%IS_OPEN)
      !
      !--------------------------------------------------------------------
      !
      CHECK = PRNT_FRES%IS_OPEN
      IF(KITER == MXITER.OR.ICNVG==1) THEN
                                       CONTINUE
      ELSEIF(CHECK .AND. PRNT_FRES_OUTER>Z) THEN
                        CHECK = KITER >= PRNT_FRES_OUTER
      ELSEIF(CHECK) THEN
                        CHECK = KITER >= MXITER + PRNT_FRES_OUTER + ONE
      END IF
      !
      IF(CHECK) THEN
        !
        PRNT_FRES_LRC = Z
        PRNT_FRES_DIF = DZ
        CNT = Z
        !
        DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
           !
           IF(IBOUND(J,I,K)>Z) THEN
               !
               !=================================================
               !
               DIF = CELL_MASS_BALANCE(I,J,K)
               !
               !=================================================
               !
               CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
               !
               IF(CNT > PRNT_FRES_NTERM) THEN
                   ADIF = ABS(DIF)
                   DO II = ONE, PRNT_FRES_NTERM
                       IF( ADIF > ABS(PRNT_FRES_DIF(II)) ) THEN
                         !
                         IF(II<PRNT_FRES_NTERM) THEN
                           DO ITMP=PRNT_FRES_NTERM, II+ONE, NEG
                             PRNT_FRES_LRC(ITMP)=PRNT_FRES_LRC(ITMP-ONE)
                             PRNT_FRES_DIF(ITMP)=PRNT_FRES_DIF(ITMP-ONE)
                           END DO
                         END IF
                         !
                         PRNT_FRES_LRC(II) = ID
                         PRNT_FRES_DIF(II) = DIF
                         !
                         EXIT
                       END IF
                   END DO
               ELSE
                   CNT = CNT + ONE
                   PRNT_FRES_LRC(CNT) = ID
                   PRNT_FRES_DIF(CNT) = DIF
                   !
                   IF(CNT == PRNT_FRES_NTERM) THEN
                       CALL SORT(PRNT_FRES_NTERM,
     +                           PRNT_FRES_DIF, PRNT_FRES_LRC, TRUE)
                       CALL REVERSE_ORDER(PRNT_FRES_NTERM,PRNT_FRES_DIF)  !MAKE IT SO ITS LARGEST TO SMALLEST
                       CALL REVERSE_ORDER(PRNT_FRES_NTERM,PRNT_FRES_LRC)
                       CNT = CNT + ONE
                   END IF
               END IF
           END IF
           !
        END DO; END DO; END DO
        !
        CALL SORT(PRNT_FRES_NTERM, PRNT_FRES_LRC, PRNT_FRES_DIF)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, PRNT_FRES_NTERM
         !
         ID  = PRNT_FRES_LRC(II)
         DIF = PRNT_FRES_DIF(II)
         DTMP= DIF*DELT  !VOLUME LOST
         !
         CALL CELLID_TO_LRC(ID, K, I, J, NLAY, NROW, NCOL)
         !
         ADIF= AREA(J,I)*(BOTM(J,I,LBOTM(K)-ONE) - BOTM(J,I,LBOTM(K)))    !CELL VOLUME
         !
         IF(PRNT_FRES%BINARY) THEN
         WRITE(PRNT_FRES%IU)KPER,KSTP,KITER,K,I,J,HNEW(J,I,K),DIF,
     +                      DTMP,ADIF,DT,ID
         ELSE
         WRITE(PRNT_FRES%IU,'(2(1x I4),1x I5,3(1x I4),2x A,
     +                        3(1x ES15.7),2(2x A))')
     +                  KPER,KSTP,KITER,K,I,J,
     +                  NUM2STR7(HNEW(J,I,K),14), DIF,DTMP,ADIF,
     +                  DT,NUM2STR(ID)
         END IF
        END DO
        !
      END IF !(PRNT_FRES%IS_OPEN)
      !
      !--------------------------------------------------------------------
      !
      CHECK = PRNT_VERR%IS_OPEN
      IF(KITER == MXITER.OR.ICNVG==1) THEN
                                       CONTINUE
      ELSEIF(CHECK .AND. PRNT_VERR_OUTER>Z) THEN
                        CHECK = KITER >= PRNT_VERR_OUTER
      ELSEIF(CHECK) THEN
                        CHECK = KITER >= MXITER + PRNT_VERR_OUTER + ONE
      END IF
      !
      IF(CHECK) THEN
        !
        PRNT_VERR_LRC = Z
        PRNT_VERR_DIF = DZ
        CNT = Z
        !
        DO K=ONE,NLAY; DO I=ONE,NROW; DO J=ONE,NCOL
           !
           IF(IBOUND(J,I,K)>Z) THEN
               !
               !=================================================
               !
               DIF = CELL_VOL_ERROR(I,J,K)
               !
               !=================================================
               !
               CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
               !
               IF(CNT > PRNT_VERR_NTERM) THEN
                   DO II = ONE, PRNT_VERR_NTERM
                       IF( DIF > PRNT_VERR_DIF(II) ) THEN
                         !
                         IF(II<PRNT_VERR_NTERM) THEN
                           DO ITMP=PRNT_VERR_NTERM, II+ONE, NEG
                             PRNT_VERR_LRC(ITMP)=PRNT_VERR_LRC(ITMP-ONE)
                             PRNT_VERR_DIF(ITMP)=PRNT_VERR_DIF(ITMP-ONE)
                           END DO
                         END IF
                         !
                         PRNT_VERR_LRC(II) = ID
                         PRNT_VERR_DIF(II) = DIF
                         !
                         EXIT
                       END IF
                   END DO
               ELSE
                   CNT = CNT + ONE
                   PRNT_VERR_LRC(CNT) = ID
                   PRNT_VERR_DIF(CNT) = DIF
                   !
                   IF(CNT == PRNT_VERR_NTERM) THEN
                       CALL SORT(PRNT_VERR_NTERM,
     +                           PRNT_VERR_DIF, PRNT_VERR_LRC)
                       CALL REVERSE_ORDER(PRNT_VERR_NTERM,PRNT_VERR_DIF)  !MAKE IT SO ITS LARGEST TO SMALLEST
                       CALL REVERSE_ORDER(PRNT_VERR_NTERM,PRNT_VERR_LRC)
                       CNT = CNT + ONE
                   END IF
               END IF
           END IF
           !
        END DO; END DO; END DO
        !
        CALL SORT(PRNT_VERR_NTERM, PRNT_VERR_LRC, PRNT_VERR_DIF)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, PRNT_VERR_NTERM
         !
         ID  = PRNT_VERR_LRC(II)
         DIF = PRNT_VERR_DIF(II)  ! VOLUME/TIME ERROR / CELL VOLUME
         !
         CALL CELLID_TO_LRC(ID, K, I, J, NLAY, NROW, NCOL)
         !
         ADIF= AREA(J,I)*(BOTM(J,I,LBOTM(K)-ONE) - BOTM(J,I,LBOTM(K)))    !CELL VOLUME
         !
         DTMP=ADIF*DIF  ! FLOW_RESIDUAL
         !
         ADIF = DTMP*DELT  ! VOL_RESIDUAL
         !
         IF(PRNT_VERR%BINARY) THEN
         WRITE(PRNT_VERR%IU)KPER,KSTP,KITER,K,I,J,HNEW(J,I,K),DIF,
     +                      ADIF,DTMP,DT,ID
         ELSE
         WRITE(PRNT_VERR%IU,'(2(1x I4),1x I5,3(1x I4),1x A,1x F12.6,
     +                        2(1x ES15.7),2(2x A))')
     +                  KPER,KSTP,KITER,K,I,J,
     +                  NUM2STR7(HNEW(J,I,K),14), DIF,ADIF,DTMP,
     +                  DT,NUM2STR(ID)
         END IF
        END DO
        !
      END IF !(PRNT_VERR%IS_OPEN)
      !
      ! CHECK IF SFR CAUSED MASS BALANCE ERRORS-----------------------------
      !
      IF(SFR_IN_USE) THEN
        IF(SFR_FRES%IS_OPEN) THEN
            CALL SFR_POST_SOLVER(KPER, KSTP, KITER, MXITER,ICNVG,DELT)
            !
            CALL SFR_FRES%SIZE_CHECK()
        END IF
      END IF

      !
      ! UPDATE THE UPLAY AND WTABLE ARRAYS----------------------------------
      !
      CALL GWF2BAS7UPLAY(0)  !If zero is passed in pointers are not updated - saves time
C
C4------RETURN
      END SUBROUTINE
      !
      SUBROUTINE SFR_POST_SOLVER(KPER, KSTP, KITER, MXITER, ICNVG,DELT)      !ASSUMES CORRECT LGR GRID IS SET
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,   ONLY: NEG,Z,ONE,DZ,TRUE
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,
     +                      BOTM,LBOTM,AREA,UPLAY,
     +                      CELL_MASS_BALANCE
      USE GWFBASMODULE,      ONLY: DATE_SP, HAS_STARTDATE
      USE UTIL_INTERFACE,    ONLY: LRC_TO_CELLID, CELLID_TO_LRC
      USE NUM2STR_INTERFACE, ONLY: NUM2STR, NUM2STR7
      USE SORT_INTERFACE
      USE GWFSFRMODULE, ONLY: NSTRM, ISTRM
      USE GWFSFRMODULE, ONLY: SFR_FRES,SFR_FRES_OUTER,SFR_FRES_NTERM,
     +                        SFR_FRES_LRC, SFR_FRES_DIF
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: KPER, KSTP, KITER, MXITER, ICNVG
      DOUBLE PRECISION, INTENT(IN   ):: DELT
      !
      INTEGER:: I, J, K, II, ITMP, CNT, L, IS, IR, ID
      CHARACTER(8):: DT
      DOUBLE PRECISION:: DIF, ADIF, DTMP
      LOGICAL:: CHECK
      !
      !--------------------------------------------------------------------
      !
      CHECK = SFR_FRES%IS_OPEN
      IF(KITER == MXITER.OR.ICNVG==1) THEN
                                       CONTINUE
      ELSEIF(CHECK .AND. SFR_FRES_OUTER>Z) THEN
                        CHECK = KITER >= SFR_FRES_OUTER
      ELSEIF(CHECK) THEN
                        CHECK = KITER >= MXITER + SFR_FRES_OUTER + ONE
      END IF
      !
      IF(CHECK) THEN
        !
        SFR_FRES_LRC = Z
        SFR_FRES_DIF = DZ
        CNT = Z
        !
        DO L=ONE,NSTRM
           !
           IF(ANY(L == SFR_FRES_LRC) ) CYCLE
           !
           K = ISTRM(1,L)
           I = ISTRM(2,L)
           J = ISTRM(3,L)
           !
           IF(K < UPLAY(J,I)) K = UPLAY(J,I)
           !
           IF(IBOUND(J,I,K)>Z ) THEN
               !
               !=================================================
               !
               DIF = CELL_MASS_BALANCE(I,J,K)
               !
               !=================================================
               !
               IF(CNT > SFR_FRES_NTERM) THEN
                   ADIF = ABS(DIF)
                   DO II = ONE, SFR_FRES_NTERM
                       IF( ADIF > ABS(SFR_FRES_DIF(II)) ) THEN
                         !
                         IF(II<SFR_FRES_NTERM) THEN
                           DO ITMP=SFR_FRES_NTERM, II+ONE, NEG
                             SFR_FRES_LRC(ITMP)=SFR_FRES_LRC(ITMP-ONE)
                             SFR_FRES_DIF(ITMP)=SFR_FRES_DIF(ITMP-ONE)
                           END DO
                         END IF
                         !
                         SFR_FRES_LRC(II) = L
                         SFR_FRES_DIF(II) = DIF
                         !
                         EXIT
                       END IF
                   END DO
               ELSE
                   CNT = CNT + ONE
                   SFR_FRES_LRC(CNT) = L
                   SFR_FRES_DIF(CNT) = DIF
                   !
                   IF(CNT == SFR_FRES_NTERM) THEN
                       CALL SORT(SFR_FRES_NTERM,
     +                           SFR_FRES_DIF, SFR_FRES_LRC, TRUE)
                       CALL REVERSE_ORDER(SFR_FRES_NTERM,SFR_FRES_DIF)  !MAKE IT SO ITS LARGEST TO SMALLEST
                       CALL REVERSE_ORDER(SFR_FRES_NTERM,SFR_FRES_LRC)
                       CNT = CNT + ONE
                   END IF
               END IF
           END IF
           !
        END DO
        !
        CALL SORT(SFR_FRES_NTERM, SFR_FRES_LRC, SFR_FRES_DIF)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, SFR_FRES_NTERM
         !
         L  = SFR_FRES_LRC(II)
         !
         IF( L>Z ) THEN
            !
            DIF = SFR_FRES_DIF(II)
            DTMP= DIF*DELT  !VOLUME LOST
            !
            K = ISTRM(1,L)
            I = ISTRM(2,L)
            J = ISTRM(3,L)
            !
            IS = ISTRM(4,L)
            IR = ISTRM(5,L)
            !
            IF(K < UPLAY(J,I)) K = UPLAY(J,I)
            !
            CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
            !
            ADIF= AREA(J,I)*(BOTM(J,I,LBOTM(K)-ONE)-BOTM(J,I,LBOTM(K)))  !CELL VOLUME
            !
            IF(SFR_FRES%BINARY) THEN
            WRITE(SFR_FRES%IU)KPER,KSTP,KITER,K,I,J,IS,IR,HNEW(J,I,K),
     +                        DIF,DTMP,ADIF,DT,ID
            ELSE
            WRITE(SFR_FRES%IU,'(2(1x I4),1x I5,5(1x I4),2x A,
     +                           3(1x ES15.7),2(2x A))')
     +                     KPER,KSTP,KITER,K,I,J,IS,IR,
     +                     NUM2STR7(HNEW(J,I,K),14), DIF,DTMP,ADIF,
     +                     DT,NUM2STR(ID)
            END IF
         END IF
        END DO
        !
      END IF !(SFR_FRES%IS_OPEN)
      !
      !--------------------------------------------------------------------
      !
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7OC(KSTP,KPER,ICNVG,INOC,IGRID)
C     ******************************************************************
C     OUTPUT CONTROLLER FOR HEAD, DRAWDOWN, AND BUDGET
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NLAY,NSTP,IXSEC,IFREFM,NOCBC
      USE GWFBASMODULE,ONLY:IHDDFL,IBUDFL,ICBCFL,IPEROC,ITSOC,IBDOPT,
     1                      IOFLG,IUBGT,PRNT_CNVG
C
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
      !
      CALL PRNT_CNVG%SIZE_CHECK()
C
C1------TEST UNIT NUMBER (INOC (INOC=IUNIT(12))) TO SEE IF
C1------OUTPUT CONTROL IS ACTIVE.  IF NOT, SET DEFAULTS AND RETURN.
      IF(INOC.EQ.0) THEN
         IHDDFL=0
         IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER))IHDDFL=1
         IBUDFL=0
         IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER))IBUDFL=1
         ICBCFL=0
         GO TO 1000
      END IF
C
C2------OUTPUT CONTROL IS ACTIVE.  IF IPEROC >= 0, READ OUTPUT FLAGS
C2------USING ALPHABETIC INPUT STRUCTURE.
      IF(IPEROC.GE.0) THEN
         CALL SGWF2BAS7N(KPER,KSTP,INOC,IOUT,NLAY)
         GO TO 600
      END IF
C
C3------READ AND PRINT OUTPUT FLAGS AND CODE FOR DEFINING IOFLG USING
C3------THE ORIGINAL NUMERIC INPUT STRUCTURE.
      IF(IFREFM.EQ.0) THEN
         READ(INOC,'(4I10)') INCODE,IHDDFL,IBUDFL,ICBCFL
      ELSE
         READ(INOC,*) INCODE,IHDDFL,IBUDFL,ICBCFL
      END IF
        WRITE(IOUT,3) IHDDFL,IBUDFL,ICBCFL
    3 FORMAT(1X,/1X,'HEAD/DRAWDOWN PRINTOUT FLAG =',I2,
     1    5X,'TOTAL BUDGET PRINTOUT FLAG =',I2,
     2   /1X,'CELL-BY-CELL FLOW TERM FLAG =',I2)
      IF(ICBCFL.NE.0) ICBCFL=IBDOPT
C
C4------DECODE INCODE TO DETERMINE HOW TO SET FLAGS IN IOFLG.
      IF(INCODE.LT.0) THEN
C
C5------INCODE <0, USE IOFLG FROM LAST TIME STEP.
          WRITE(IOUT,101)
  101   FORMAT(1X,'REUSING PREVIOUS VALUES OF IOFLG')
      ELSE IF(INCODE.EQ.0) THEN
C
C6------INCODE=0, READ IOFLG FOR LAYER 1 AND ASSIGN SAME TO ALL LAYERS
        IF(IFREFM.EQ.0) THEN
           READ(INOC,'(4I10)') (IOFLG(1,M),M=1,4)
        ELSE
           READ(INOC,*) (IOFLG(1,M),M=1,4)
        END IF
        IOFLG(1,5)=0
        DO 210 K=1,NLAY
        IOFLG(K,1)=IOFLG(1,1)
        IOFLG(K,2)=IOFLG(1,2)
        IOFLG(K,3)=IOFLG(1,3)
        IOFLG(K,4)=IOFLG(1,4)
        IOFLG(K,5)=IOFLG(1,5)
  210   CONTINUE
          WRITE(IOUT,211) (IOFLG(1,M),M=1,4)
  211   FORMAT(1X,/1X,'OUTPUT FLAGS FOR ALL LAYERS ARE THE SAME:'/
     1     1X,'  HEAD    DRAWDOWN  HEAD  DRAWDOWN'/
     2     1X,'PRINTOUT  PRINTOUT  SAVE    SAVE'/
     3     1X,34('-')/1X,I5,I10,I8,I8)
      ELSE
C
C7------INCODE>0, READ IOFLG IN ENTIRETY -- IF CROSS SECTION, READ ONLY
C7------ONE VALUE.
        IF(IXSEC.EQ.0) THEN
           DO 301 K=1,NLAY
           IF(IFREFM.EQ.0) THEN
              READ(INOC,'(4I10)') (IOFLG(K,M),M=1,4)
           ELSE
              READ(INOC,*) (IOFLG(K,M),M=1,4)
           END IF
           IOFLG(K,5)=0
  301      CONTINUE
             WRITE(IOUT,302) 'OUTPUT FLAGS FOR EACH LAYER:','LAYER'
  302      FORMAT(1X,/1X,A,/
     1     1X,'         HEAD    DRAWDOWN  HEAD  DRAWDOWN'/
     2     1X,A,'  PRINTOUT  PRINTOUT  SAVE    SAVE'/
     3     1X,41('-'))
             WRITE(IOUT,303) (K,(IOFLG(K,M),M=1,4),K=1,NLAY)
  303      FORMAT(1X,I4,I8,I10,I8,I8)
        ELSE
           IF(IFREFM.EQ.0) THEN
              READ(INOC,'(4I10)') (IOFLG(1,M),M=1,4)
           ELSE
              READ(INOC,*) (IOFLG(1,M),M=1,4)
           END IF
             WRITE(IOUT,302) 'OUTPUT FLAGS FOR CROSS SECTION:','     '
             WRITE(IOUT,304) (IOFLG(1,M),M=1,4)
  304      FORMAT(1X,I12,I10,I8,I8)
        END IF
      END IF
C
C8------THE LAST STEP IN A STRESS PERIOD AND STEPS WHERE ITERATIVE
C8------PROCEDURE FAILED TO CONVERGE GET A VOLUMETRIC BUDGET.
  600 IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER)) IBUDFL=1
C
C9------GLOBAL CBC OPTIONS
C
 1000 IF    (NOCBC == -1) THEN
                              ICBCFL=IBDOPT
      ELSEIF(NOCBC == -2) THEN
           IF(KSTP.EQ.NSTP(KPER)) THEN
                              ICBCFL=IBDOPT
           END IF
      ELSEIF(NOCBC == 2) THEN
                              ICBCFL=0

      END IF
C
C9------RETURN
C
      END SUBROUTINE
      SUBROUTINE GWF2BAS7OT(KSTP,KPER,ICNVG,ISA,IGRID,BUDPERC,
     +                      KITER,MXITER)
C     ******************************************************************
C     OUTPUT TIME, VOLUMETRIC BUDGET, HEAD, AND DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32
      USE CONSTANTS,   ONLY: BLNK, BLN, NL, TRUE, FALSE
      USE GLOBAL,      ONLY:ITMUNI,IOUT,NCOL,NROW,NLAY,HNEW,STRT,DDREF,
     1                      INPUT_CHECK,WORST_CELL_MASS_BALANCE,IBOUND,
     2                      MAX_RELATIVE_VOL_ERROR,NPER, NSTP, RBUF,
     +                      CELL_MASS_BALANCE
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,IHDDFL,IBUDFL,BUDGETDB,
     1                     MSUM,VBVL,VBNM,IDDREF,IUBGT,PDIFFPRT,DATE_SP,
     2                     MAX_REL_VOL_ERROR,MAX_REL_VOL_INVOKED,
     3                     INTER_INFO,PVOL_ERR, HAS_STARTDATE, 
     4                     PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM, 
     5                     PRNT_RES_CUM_ARR
      USE UTIL_INTERFACE,    ONLY: WARNING_MESSAGE
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE BUDGET_DATEBASE_WRITER
      USE BAS_UTIL, ONLY: MASS_ERROR_PRINT
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: KSTP,KPER,ICNVG,ISA,IGRID,KITER,MXITER
      REAL,    INTENT(INOUT):: BUDPERC
      !
      CHARACTER(19):: DATE
      !CHARACTER(:),ALLOCATABLE::TXT
      !CHARACTER(5)::CH5
      INTEGER:: IPFLG
      INTEGER:: R, C, L, I, J, K
      LOGICAL:: HAS_PDIFFPRT
      DOUBLE PRECISION:: ERR, VERR, VOL, RAT
      REAL:: TOTRIN,TOTROT
      LOGICAL:: LAST_TS
C     ------------------------------------------------------------------
C
      CALL SGWF2BAS7PNT(IGRID)
      !
      HAS_PDIFFPRT = FALSE
      !
      LAST_TS = FALSE
      !
      IF(KPER == NPER) THEN
          IF( KSTP == NSTP(KPER)) LAST_TS = TRUE
      END IF
      !CH5 = BLNK
C
C1------CLEAR PRINTOUT FLAG (IPFLG)
      IPFLG=0
      BUDPERC=1.E30
C
C PRINT OUT BUDGET DATABASE
      IF(BUDGETDB%IU.NE.0) THEN
         IF(HAS_STARTDATE) THEN
             DATE = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
         ELSE
             DATE = '  NaN'
         END IF
         CALL WRITE_DATEBASE(BUDGETDB,MSUM,VBNM,VBVL,KSTP,KPER,TOTIM,
     +                       DELT,DATE)
      END IF
C
C
      IF(ISA.EQ.0) THEN
           WRITE(IOUT,9) KSTP,KPER
    9    FORMAT(1X,/11X,'NO FLOW EQUATION TO SOLVE IN TIME STEP',I5,
     1      ' OF STRESS PERIOD ',I6,/1X,'ALL HEADS ARE 0.0')
         IPFLG=1
      END IF
C
C3------IF HEAD AND DRAWDOWN FLAG (IHDDFL) IS SET WRITE HEAD,
C3------DRAWDOWN, AND IBOUND IN ACCORDANCE WITH FLAGS IN IOFLG.
      IF(IHDDFL.EQ.0) GO TO 100
C
      CALL SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7IB(KSTP,KPER)
C
  100 CONTINUE

C4------PRINT TOTAL BUDGET IF REQUESTED
      IF(IBUDFL.NE.0 .OR. ICNVG.EQ.0 .OR. LAST_TS) THEN !GO TO 120                               !seb CHANGED TO ALLOW PERCENT ERROR CHECK AT EVERY TIMESTEP
          CALL SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,IUBGT,
     +                                   BUDPERC,TOTRIN,TOTROT,PVOL_ERR)
          IPFLG=1
          !
          HAS_PDIFFPRT = ABS(BUDPERC).GE.REAL(PDIFFPRT) .AND.
     +                  .NOT. INPUT_CHECK.AND. MAX(TOTRIN,TOTROT)>0.1 !0.001
      ELSE
          CALL SGWF2BAS7VNOPRT(MSUM,VBVL,BUDPERC,TOTRIN,TOTROT)
          !
          HAS_PDIFFPRT = ABS(BUDPERC).GE.REAL(PDIFFPRT) .AND.
     +                  .NOT. INPUT_CHECK.AND. MAX(TOTRIN,TOTROT)>0.1 !0.001
          !
          IF(HAS_PDIFFPRT) THEN
                  CALL SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,IUBGT,
     +                                   BUDPERC,TOTRIN,TOTROT,PVOL_ERR)
                  IPFLG=1
          END IF
      END IF
      !
      ! ADDITIONAL OUTPUT DO TO NEW MASS BALANCE CHECK
      !
!!!      IF(MAX_REL_VOL_INVOKED) THEN
!!!         IF(ICNVG.EQ.0) THEN
!!!            TXT=BLN//CH5//'THE SOLVER ITERATION FAILED DUE '//
!!!     +'HAVING AT LEAST ONE MODEL CELL'//NL//CH5//'EXCEED THE '//
!!!     +'A VOLUMETRIC FLOW RATE ERROR FRACTION TOLERANCE'//NL//CH5//
!!!     +'AND/OR FAILING TO MEET SOLVER CONVERGENCE CRITERIA.'
!!!         ELSE
!!!            TXT=BLN//CH5//
!!!     +   'THE REQUIRED NUMBER OF SOLVER ITERATIONS WERE EXTENDED DUE '//
!!!     +   'HAVING AT LEAST ONE MODEL CELL'//NL//CH5//
!!!     +   'EXCEED THE A VOLUMETRIC FLOW RATE ERROR FRACTION TOLERANCE.'
!!!         END IF
!!!         TXT=TXT//BLN//CH5//
!!!     +"THIS FRACTION IS THE CELL'S VOL. RATE ERROR DIVIDED BY THE "//
!!!     +"MODEL CELL'S VOLUME"//NL//CH5//
!!!     +'OneWater HAS A VOL. RATE ERROR PER CELL VOLUME LIMIT '//
!!!     +'OF '//NUM2STR(MAX_REL_VOL_ERROR)//NL//CH5//
!!!     +'BEFORE CONVERGENCE IS ALLOWED.'//BLN//CH5//
!!!     +'THIS CAN BE ADJUSTED WITH THE BASIC (BAS) PACKAGE OPTION '//NL//
!!!     +CH5//'"MAX_RELATIVE_VOLUME_ERROR" FOLLOWED BY THE DESIRED LIMIT.'
!!!      ELSE
!!!          TXT=BLNK
!!!      END IF
      !
      IF(ICNVG.EQ.0 .OR. HAS_PDIFFPRT) THEN
                               CALL WORST_CELL_MASS_BALANCE(R,C,L,ERR)
                               CALL MAX_RELATIVE_VOL_ERROR(VERR,I,J,K)
                               !
            CALL MASS_ERROR_PRINT(IOUT,ICNVG,HAS_PDIFFPRT,MXITER,
     +                            MAX_REL_VOL_INVOKED,MAX_REL_VOL_ERROR,
     +                            PDIFFPRT,KPER,KSTP,BUDPERC,
     +                            R,C,L,ERR,I,J,K,VERR)
      END IF
      !
!!!      IF(HAS_PDIFFPRT) THEN
!!!         WRITE(*,'(25x 2A,F6.1,A)')'Warning: ',
!!!     +         'Volumetric Rate Budget Percent Error is ',
!!!     +          BUDPERC,'%'
!!!         WRITE(*,'(34x 3A,/34x 6A/)')
!!!     +   'The worst vol. rate error is:           ',NUM2STR(ERR),
!!!     +   '   (L^3/T)', 'For model cell (Lay, Row, Col):          ',
!!!     +   NUM2STR(L),', ',NUM2STR(R),', ',NUM2STR(C)
!!!         WRITE(*,'(34x 3A,/34x 6A/)')
!!!     +   'Worst vol. rate error per cell volume is: ',
!!!     +   NUM2STR(VERR),' (1/T)',
!!!     +   'For model cell (Lay, Row, Col):           ',
!!!     +   NUM2STR(L),', ',NUM2STR(R),', ',NUM2STR(C)
!!!      END IF
!!!C
!!!C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
!!!      IF(ICNVG.EQ.0) THEN
!!!       !
!!!       TXT = CH5//
!!!     +'FAILED TO MEET SOLVER CONVERGENCE CRITERIA FOR STRESS PERIOD '//
!!!     + NUM2STR(KPER)//' TIME STEP NUMBER '//NUM2STR(KSTP)//BLN//CH5//
!!!     +"THE LAST SOLVER ITERATION'S VOLUMETRIC RATE BUDGET "//
!!!     +'PERCENT DISCREPANCY IS '//NUM2STR(BUDPERC)//' %'//BLN//CH5//
!!!     +'THE WORST VOL. RATE ERROR IS: '//NUM2STR(ERR,-15)//
!!!     +'                 (L^3/T)'// NL//CH5//
!!!     +'FOR MODEL CELL LAY, ROW, COL:  '//
!!!     + NUM2STR(L)//', '//NUM2STR(R)//', '//NUM2STR(C)//BLN//CH5//
!!!     + 'THE WORST VOL. RATE ERROR PER CELL VOLUME IS: '//
!!!     +   NUM2STR(VERR,-15)//' (1/T)'//NL//CH5//
!!!     + 'FOR MODEL CELL LAY, ROW, COL:                 '//
!!!     + NUM2STR(K)//', '//NUM2STR(I)//', '//NUM2STR(J)//TXT
!!!       !
!!!       CALL WARNING_MESSAGE(OUTPUT=IOUT, MSG=TXT )
!!!      !
!!!      ELSEIF(HAS_PDIFFPRT) THEN
!!!        !
!!!        CALL WARNING_MESSAGE(OUTPUT=IOUT, MSG=CH5//
!!!     + "THE LAST SOLVER ITERATION'S VOLUMETRIC RATE BUDGET "//
!!!     + 'PERCENT DISCREPANCY IS '//NUM2STR(BUDPERC)//' %'//NL//CH5//
!!!     + 'WHICH IS GREATER THEN THE PERCENTERROR LIMIT OF '//
!!!     +  NUM2STR(PDIFFPRT)//'%'//BLN//CH5//
!!!     + 'THIS IS JUST A WARNING, YOU CAN CHANGE THE PERCENTERROR LIMIT'//
!!!     + NL//CH5//'WITH THE BASIC PACKAGE OPTION "PERCENTERROR" '//
!!!     + 'FOLLOWED BY THE NEW LIMIT AS AN INTEGER.'//BLN//CH5//
!!!     +'THE WORST VOL. RATE ERROR IS: '//NUM2STR(ERR,-15)//
!!!     +'                 (L^3/T)'//NL//CH5//
!!!     + 'FOR MODEL CELL LAY, ROW, COL:  '//
!!!     +  NUM2STR(L)//', '//NUM2STR(R)//', '//NUM2STR(C)//BLN//CH5//
!!!     +  'THE WORST VOL. RATE ERROR PER CELL VOLUME IS: '//
!!!     +    NUM2STR(VERR,-15)//' (1/T)'//NL//CH5//
!!!     +  'FOR MODEL CELL LAY, ROW, COL:             '//
!!!     +  NUM2STR(K)//', '//NUM2STR(I)//', '//NUM2STR(J)//TXT
!!!     +  )
!!!      END IF !(ICNVG.EQ.0)
      !
      IF(INTER_INFO%IU.NE.0) THEN
         !
         K = -14  !NUM2STR Pad
         RAT = ABS(TOTRIN-TOTROT)
         VOL = RAT*DELT
         WRITE(INTER_INFO%IU,'(3I6, 2x A, 1x ES14.7, 1x ES14.7, 2x A)',
     -                                                    ADVANCE='NO')
     +            KPER,KSTP,KITER,NUM2STR(DELT,        K),
     +                    VOL,RAT,NUM2STR(ABS(BUDPERC),K)
         !
         IF(HAS_STARTDATE) THEN
             WRITE(INTER_INFO%IU,'(1x A)',ADVANCE='NO')
     +                 DATE_SP(KPER)%TS(KSTP-1)%STR_MONTHYEAR()
         END IF
         !
         WRITE(INTER_INFO%IU,'(A)')
         !
      END IF
      !
      ! Print out Residual Information
      !
      IF(PRNT_RES%IU.NE.0) THEN
      IF(ABS(BUDPERC) >= PRNT_RES_LIM) THEN
        !
        DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL)
              IF(IBOUND(J,I,K) > 0) THEN
                   !
                   RBUF(J,I,K) = REAL(
     +                                ABS(CELL_MASS_BALANCE(I,J,K)),
     +                                                           REAL32)
              ELSE
                   RBUF(J,I,K) = 0.0_REAL32
              END IF
        END DO
        ! RBUF(J,I,1) = TO_SNGL(BUF(J,I))
        IF(PRNT_RES%BINARY) THEN
            WRITE(PRNT_RES%IU) KPER, KSTP, NLAY, NROW, NCOL
            WRITE(PRNT_RES%IU) RBUF
        ELSE
          J = -6  !Format for NUM2STR
          DO K=1, NLAY
            !
            WRITE(PRNT_RES%IU, '(*(A))')
     +                     ' LAY ',NUM2STR(K,J), 
     +                     ' PER  TS ',NUM2STR(KPER,J),NUM2STR(KSTP,J),
     +                     ' NLAY NROW NCOL ',NUM2STR(NLAY,J),
     +                                 NUM2STR(NROW,J),NUM2STR(NCOL)
            DO I=1, NROW
                       WRITE(PRNT_RES%IU, '(*(ES12.5, 1x))') RBUF(:,I,K)
            END DO
          END DO
        END IF
      END IF
      END IF
C
C
      IF(PRNT_RES_CUM%IU.NE.0) THEN
       ASSOCIATE(CUM=>PRNT_RES_CUM_ARR, IU=>PRNT_RES_CUM%IU, 
     +           BIN=>PRNT_RES_CUM%BINARY, IB=>IBOUND       )
        !
        DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IB(J,I,K) > 0)
              !
              CUM(J,I,K) = CUM(J,I,K)
     +                   + ABS( CELL_MASS_BALANCE(I,J,K) ) * DELT
        END DO
        !
        IF(KPER==NPER .AND. KSTP==NSTP(KPER)) THEN
           !
           IF(BIN) THEN
               WRITE(IU) KPER, KSTP, NLAY, NROW, NCOL
               WRITE(IU) CUM
           ELSE
             J = -6  !Format for NUM2STR
             DO K=1, NLAY
               !
               WRITE(IU, '(*(A))')
     +                     ' LAY ',NUM2STR(K,J), 
     +                     ' PER  TS ',NUM2STR(KPER,J),NUM2STR(KSTP,J),
     +                     ' NLAY NROW NCOL ',NUM2STR(NLAY,J),
     +                                 NUM2STR(NROW,J),NUM2STR(NCOL)
               DO I=1, NROW
                          WRITE(IU, '(*(ES14.7, 1x))') CUM(:,I,K)
               END DO
             END DO
           END IF
        END IF
       END ASSOCIATE
      END IF
C
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C5------WILL BE PRODUCED.
  120 IF(IDDREF.NE.0) THEN
         IF(ASSOCIATED(DDREF,STRT)) THEN
            ALLOCATE(DDREF(NCOL,NROW,NLAY))
            CALL SGWF2BAS7PSV(IGRID)
         END IF
         DDREF=HNEW
           WRITE(IOUT,99)
   99    FORMAT(1X,'Drawdown Reference has been reset to the',
     1               ' end of this time step')
         IDDREF=0
      END IF
      IF(IPFLG.NE.0) THEN
      CALL SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
      WRITE(IOUT,*)
      END IF

C
C6------RETURN
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7ARDIS(LINE,IUDIS,IOUT,STARTING_DATE)
C     *****************************************************************
C     ALLOCATE AND READ DIS DATA
C     *****************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IUNIT,LBOTM,LAYCBD,ITRSS, SPTIM,
     3                     PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     +                     XY_GRID_COODINATES,XYGRID,AREA,GSE     !seb XY_GRID_COODINATES IS REQUIRED TO CALL ITS ALLOCATION FUNCTION
      USE GWFBASMODULE,ONLY:REALTIM, USE_LEAP_YR,DATE_SP,HAS_STARTDATE,
     +                      SIMTIM_PER, REALTIM_PER, TOTPERTIM
      USE BAS_UTIL,    ONLY: DELT_TO_DAY
      USE CONSTANTS,   ONLY: NL,TRUE,FALSE,UNO,SET_NAN, NEG, NEARZERO_5
      USE CONSTANTS,   ONLY: Z,ONE,TWO,THREE,FOUR,FIVE
      USE UTIL_INTERFACE, ONLY: PARSE_WORD_UP
      USE UTIL_INTERFACE, ONLY: STOP_ERROR,WARNING_MESSAGE
      USE UTIL_INTERFACE, ONLY: READ_TO_DATA, PARSE_WORD_UP,
     +                          GET_INTEGER, GET_NUMBER, NEAR_ZERO
      USE NUM2STR_INTERFACE,         ONLY: NUM2STR
      USE ULOAD_AND_SFAC_INTERFACE, ONLY: ULOAD
      USE DATE_OPERATOR_INSTRUCTION
C
      TYPE(DATE_OPERATOR), INTENT(INOUT):: STARTING_DATE
      CHARACTER(*), INTENT(INOUT):: LINE
      CHARACTER*24 ANAME(5)
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
      DOUBLE PRECISION:: XFIRSTCORD,YFIRSTCORD,GRIDROTATION             !seb ADDED VARIABLES TO READ IN LAY 1, ROW 1 ,COL 1 XY COORD
      LOGICAL:: PRINTCOORD,LLCOODRINATE,CORNERCOORD
      DOUBLE PRECISION:: SP_LEN, TS_MULT
C     ------------------------------------------------------------------
C
      CALL STARTING_DATE%INIT() !SETS IT TO 'NO_DATE' CAUSE NO ARG PASSED
      !
      ALLOCATE(HAS_STARTDATE)
      HAS_STARTDATE = FALSE
      !
C1------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS.EQ.Z) THEN
          CALL STOP_ERROR(OUTPUT=IOUT,MSG=
     + 'THE DIS PACKAGE WAS NOT DEFINED IN THE NAME FILE.'//
     + 'IT MUST BE DEFINED FOR OneWater TO RUN.')
      END IF
      WRITE(IOUT,11) INDIS
   11 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT ',I4)
C
C
C2------Read comments and the first line following the comments.
      CALL READ_TO_DATA(LINE,INDIS,IOUT)
      !CALL URDCOM(INDIS,IOUT,LINE)
C
C3------Get the number of layers, rows, columns, stress periods,
C3------ITMUNI, and LENUNI from the line.
      LLOC=1
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, NLAY,
     +                 MSG='FAILED TO LOAD NLAY')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, NROW,
     +                 MSG='FAILED TO LOAD NROW')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, NCOL,
     +                 MSG='FAILED TO LOAD NCOL')
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, NPER,
     +                 MSG='FAILED TO LOAD NPER')
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      SELECT CASE(LINE(ISTART:ISTOP))
      CASE('SECOND','SECONDS'); ITMUNI = ONE
      CASE('MINUTE','MINUTES'); ITMUNI = TWO
      CASE('HOUR'  ,'HOURS'  ); ITMUNI = THREE
      CASE('DAY'   ,'DAYS'   ); ITMUNI = FOUR
      CASE('YEAR'  ,'YEARS'  ); ITMUNI = FIVE
      CASE DEFAULT
             CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, ITMUNI,
     +                     NO_PARSE_WORD=TRUE,
     +                     MSG='FAILED TO LOAD ITMUNI, IT MUST BE'//NL//
     +                         '0, 1, 2, 3, 4, 5 or'//NL//
     +                         'SECOND, MINUTE, HOUR, DAY, YEAR')
      END SELECT
      !
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      SELECT CASE(LINE(ISTART:ISTOP))
      CASE('FOOT'      ,'FEET'       ); LENUNI = ONE
      CASE('METER'     ,'METERS'     ); LENUNI = TWO
      CASE('CENTIMETER','CENTIMETERS'); LENUNI = THREE
      CASE DEFAULT
             CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, LENUNI,
     +                     NO_PARSE_WORD=TRUE,
     +                     MSG='FAILED TO LOAD LENUNI, IT MUST BE'//NL//
     +                         '0, 1, 2, 3 or'//NL//
     +                         'FEET, METER, CENTIMETER')
      END SELECT
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT, INDIS)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
      !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
      !
      N=LLOC                                                            !seb TEMP STORAGE OF LLOC
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,K,R,Z,INDIS)             !seb READ IN X COORD
      IF (LINE(LEN(LINE):LEN(LINE)).NE.'E')THEN
          XFIRSTCORD=DBLE(R)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,K,R,IOUT,INDIS)            !seb READ IN Y COORD
          YFIRSTCORD=DBLE(R)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,K,R,IOUT,INDIS)            !seb READ IN ANGLE
          GRIDROTATION=DBLE(R)                                          !GRIDROTATION EXPECTED TO BE A POLAR ANGLES. POLAR ANGLES ARE MEASURED COUNTERCLOCKWISE FROM THE POSITIVE X-AXIS IN DEGREES
        LLCOODRINATE=FALSE
        CORNERCOORD=FALSE
      ELSE
        XFIRSTCORD  =0D0
        YFIRSTCORD  =0D0
        GRIDROTATION=0D0
        !
        LLCOODRINATE= TRUE
        CORNERCOORD = TRUE
        !
        LLOC=N
        !
        WRITE(IOUT,'(/ A,/ 2A, / A /)')
     +   '*** DIS COORDINATE SYSTEM NOT SEPECIFIED ***',
     +   'DEFAULT VALUES APPLIED ARE 0','° ROTATION',
     +  "WITH THE ORIGIN (0,0) LOCATED AT THE MODEL'S LOWER LEFT CORNER"
      END IF
      !
      PRINTCOORD=FALSE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      DO I=1, 3  !AT MOST THREE CALLS TO URWORD
        SELECT CASE(LINE(ISTART:ISTOP))
        CASE('LLCOODRINATE','LLCOORDINATE')
            WRITE(IOUT,'(2A)')
     +            'DIS COORDINATE SYSTEM SEPECIFIED WITH ',
     +            'ORIGIN POINT LOCATED AT LOWER LEFT [ROW NROW, COL 1]'
            LLCOODRINATE=TRUE
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        CASE('CORNERCOORD')
            WRITE(IOUT,'(2A)')
     +  'DIS COORDINATE SYSTEM SEPECIFIED WITH ',
     +  "ORIGIN POINT LOCATED AT THE CELL'S OUTER MOST CORNER",
     +  '         [ie NOT CELL CENTER]'
            CORNERCOORD=TRUE
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        CASE('PRINTCOORD')
            WRITE(IOUT,'(A)')
     +           'DIS COORDINATE SYSTEM WILL BE PRINTED TO LIST'
            PRINTCOORD=TRUE
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        CASE DEFAULT
            WRITE(IOUT,'(A)')''
            EXIT
        END SELECT
      END DO
      IF  (.NOT. LLCOODRINATE) THEN
          WRITE(IOUT,'(2A)')
     +    'DIS COORDINATE SYSTEM SEPECIFIED WITH ORIGIN POINT LOCATED ',
     +    "AT MODEL'S UPPER LEFT [ROW 1, COL 1]"
      END IF
      IF (.NOT. CORNERCOORD) THEN
          WRITE(IOUT,'(2A)')
     +    'DIS COORDINATE SYSTEM SEPECIFIED WITH ',
     +    'ORIGIN LOCATED AT THE CELL CENTER'
      END IF
      !
      IF(.NOT. PRINTCOORD) THEN
        WRITE(IOUT,'(/ / A,/ A)')
     +   'DIS COORDINATE SYSTEM SEPECIFIED WITH:',
     +   'GRID ROTATION OF '//NUM2STR(GRIDROTATION)//'°'
        IF(LLCOODRINATE .AND. .NOT. CORNERCOORD) THEN
         WRITE(IOUT,'(2A,2(/A))')  'WITH THE ORIGIN LOCATED AT THE ',
     +       'CENTER OF MODEL CELL ['//NUM2STR(NROW)//',1] WITH:',
     +       'X-COORDINATE:'//NUM2STR(XFIRSTCORD),
     +       'Y-COORDINATE:'//NUM2STR(YFIRSTCORD)
         ELSEIF(.NOT. LLCOODRINATE .AND. .NOT. CORNERCOORD)THEN
         WRITE(IOUT,'(2A,2(/A))')  'WITH THE ORIGIN LOCATED AT THE ',
     +       'CENTER OF MODEL CELL [1,1] WITH:',
     +       'X-COORDINATE:'//NUM2STR(XFIRSTCORD),
     +       'Y-COORDINATE:'//NUM2STR(YFIRSTCORD)
         ELSEIF(.NOT. LLCOODRINATE .AND. CORNERCOORD)THEN
         WRITE(IOUT,'(2A,2(/A))')  'WITH THE ORIGIN LOCATED AT THE ',
     +       'OUTER MOST CORNER OF MODEL CELL [1,1] WITH:',
     +       'X-COORDINATE:'//NUM2STR(XFIRSTCORD),
     +       'Y-COORDINATE:'//NUM2STR(YFIRSTCORD)
        ELSE
         WRITE(IOUT,'(2A,2(/A))')  'WITH THE ORIGIN LOCATED AT THE ',
     +  'OUTER MOST CORNER OF MODEL CELL ['//NUM2STR(NROW)//',1] WITH:',
     +       'X-COORDINATE:'//NUM2STR(XFIRSTCORD),
     +       'Y-COORDINATE:'//NUM2STR(YFIRSTCORD)
        END IF
      END IF

C
C4------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
        WRITE(IOUT,15) NLAY,NROW,NCOL
   15 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
        WRITE(IOUT,20) NPER
   20 FORMAT(1X,I6,' STRESS PERIOD(S) IN SIMULATION')
C
C5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
      IF(ITMUNI.LT.Z .OR. ITMUNI.GT.5) ITMUNI=Z
      IF(ITMUNI.EQ.Z) THEN
           WRITE(IOUT,30)
   30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
           WRITE(IOUT,40)
   40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
           WRITE(IOUT,50)
   50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
           WRITE(IOUT,60)
   60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
           WRITE(IOUT,70)
   70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
           WRITE(IOUT,80)
   80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI.LT.Z .OR. LENUNI.GT.3) LENUNI=Z
      IF(LENUNI.EQ.Z) THEN
           WRITE(IOUT,90)
   90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI.EQ.1) THEN
           WRITE(IOUT,91)
   91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI.EQ.2) THEN
           WRITE(IOUT,93)
   93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI.EQ.3) THEN
           WRITE(IOUT,95)
   95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C7------ALLOCATE LAYER FLAGS.
      ALLOCATE(LBOTM(NLAY))
      ALLOCATE(LAYCBD(NLAY))
C
C8------Read confining bed information
      CALL READ_TO_DATA(LINE,INDIS,IOUT)
      LLOC = 1
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      IF(LINE(ISTART:ISTOP)=='NOLAYCBD'  .OR.
     +   LINE(ISTART:ISTOP)=='NO_LAYCBD' ) THEN
          LAYCBD = Z
          WRITE(IOUT,'(/2A/)') ' NO_LAYCBD Keyword found.',
     +    ' No Quasi-3D confining beds defined for any layer'
      ELSE
          BACKSPACE(INDIS)
          READ(INDIS,*,IOSTAT=LLOC) (LAYCBD(K),K=1,NLAY)
          !
          IF(LLOC.NE.Z)
     +    CALL STOP_ERROR(LINE,INDIS,IOUT,' FAILED TO LOAD '//
     +     'LAYCBD. MAKE SURE YOU HAVE NLAY INTEGERS (SGWF2BAS7ARDIS)')
          !
          LAYCBD(NLAY)=Z
          WRITE(IOUT,*) ' Confining bed flag for each layer:'
          WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
      END IF
C
C9------Count confining beds, setup the pointer to each layer's
C9------bottom array (LBOTM), and setup LAYCBD to be the confining
C9------bed number for each layer.
      NCNFBD=Z
      DO 100 K=1,NLAY
      LBOTM(K)=K+NCNFBD
      IF(LAYCBD(K).NE.Z) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
  100 CONTINUE
      NBOTM=NLAY+NCNFBD
C
C10-----Allocate space for discretization arrays
C10-----Note that NBOTM+1 arrays are allocated for BOTM
C10-----because BOTM(J,I,Z) contains the top elevation of layer 1.
      ALLOCATE (DELR(NCOL))
      ALLOCATE (DELC(NROW))
      ALLOCATE (AREA(NCOL,NROW))
      ALLOCATE (BOTM(NCOL,NROW,0:NBOTM))
      ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
      ALLOCATE (SPTIM(NPER))                                            !seb
C
C11-----Read the DELR and DELC arrays.
      CALL U1DREL(DELR,ANAME(1),NCOL,INDIS,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,INDIS,IOUT)
      !
      DO CONCURRENT(J=1:NCOL, I=1:NROW); AREA(J,I) = DELR(J)*DELC(I)
      END DO
      !
      CALL READ_TO_DATA(LINE,INDIS,IOUT)
      LLOC=1
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      IF(LINE(ISTART:ISTOP) == 'SURFACE' .OR.
     +   LINE(ISTART:ISTOP) == 'SURFACE_ELEVATION') THEN
          ALLOCATE (GSE(NCOL,NROW))
          K=Z
          CALL ULOAD(GSE, LLOC, LINE, IOUT, INDIS, K, TRUE)
      ELSE
          ALLOCATE (GSE(1,1))
          CALL SET_NAN(GSE(1,1))
          BACKSPACE(INDIS)
      END IF
      !
C
C12-----Read the top elevation of layer 1.
      CALL U2DREL(BOTM(:,:,Z),ANAME(3),NROW,NCOL,Z,INDIS,IOUT)
C
C13-----Read the bottom elevations.
      DO 120 K=1,NLAY
      KK=K
      !
      CALL U2DREL(BOTM(:,:,LBOTM(K)),ANAME(4),NROW,NCOL,KK,INDIS,IOUT)
      IF(LAYCBD(K).NE.Z) CALL U2DREL(BOTM(:,:,LBOTM(K)+1),ANAME(5),
     1          NROW,NCOL,KK,INDIS,IOUT)
!      DO IC=1,NCOL                                                      !wschmid     !adjust bottom of any layer to top of layer 1 (taken to be ground surface elevation),
!      DO IR=1,NROW                                                      !wschmid     !if top of layer 1 < bottom of any layer
!       IF(BOTM(IC,IR,LBOTM(K)).GT.BOTM(IC,IR,0))                         !wschmid    SCOTT THIS DOES NOT MAKE SENSE
!     1  BOTM(IC,IR,LBOTM(K))=BOTM(IC,IR,0)                               !wschmid
!      ENDDO                                                             !wschmid
!      ENDDO                                                             !wschmid
  120 CONTINUE
C
C14-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C14-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
        WRITE(IOUT,161)
  161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
     1            '     MULTIPLIER FOR DELT    SS FLAG  '//
     2            '(OPTIONAL TIME STEP LENGTH)',/1X,99('-'))
      ISS=Z
      ITR=Z
      REALTIM=-1D0
      SIMTIM_PER=0D0
      REALTIM_PER=-1D0
      USE_LEAP_YR=FALSE
      DO 200 N=1,NPER
      CALL READ_TO_DATA(LINE,INDIS,IOUT,IOUT)
      LLOC=1
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, SP_LEN,
     +                 MSG='DIS FAILED TO LOAD PERLEN')
      PERLEN(N) = SP_LEN
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, NSTP(N),
     +                 MSG='DIS FAILED TO LOAD NSTP')
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, TS_MULT,
     +                 MSG='DIS FAILED TO LOAD TSMULT')
       TSMULT(N) = SNGL(TS_MULT)
      IF(ABS(TSMULT(N)-1.0E0) < NEARZERO_5) THEN
          TSMULT(N) = 1.0E0
          TS_MULT   = UNO
      END IF
      IF (TSMULT(N) < NEARZERO_5) THEN
          TSMULT(N) = 1.0E0
          TS_MULT   = UNO
!           WRITE(IOUT,170)
!  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         CALL WARNING_MESSAGE(LINE,INDIS,IOUT,
     +                'TSMULT<=0.0, IT IS RESET TO 1.0', INLINE=TRUE)
      END IF
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
      IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
         ISSFLG(N)=Z
         ITR=1
      ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
         ISSFLG(N)=1
         ISS=1
      ELSE
!           WRITE(IOUT,162)
!  162    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
!     1      ' -- STOP EXECUTION (SGWF2BAS7ARDIS)')
      CALL STOP_ERROR(LINE,INDIS,IOUT,' SSFLAG MUST BE '//
     + 'EITHER "SS" OR "TR" -- STOP EXECUTION (SGWF2BAS7ARDIS)')
      END IF
      IF(NSTP(N)<Z) THEN
          NSTP(N)=ABS(NSTP(N))
          SPTIM(N)%SPECIFY_DELT = TRUE
          TSMULT(N) = 1.0E0
          ALLOCATE( SPTIM(N)%DT( NSTP(N) ) )
          DO I=1,NSTP(N)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,Z,K,R,IOUT,INDIS)  !NOT USING GET_NUMBER TO SAVE ON NUM2STR EVALUATION
              READ(LINE(ISTART:ISTOP), *, IOSTAT=K) SPTIM(N)%DT(I)
              !
              IF(K.NE.Z)CALL STOP_ERROR( LINE, INDIS,
     +         OUTPUT=IOUT,MSG= 'FOUND NEGATIVE NSTP, WHICH INDICATES'//
     +' THAT TIME STEP LENGTHS WILL BE SPECIFIED. FAILED TO LOAD THE '//
     +  NUM2STR(I)// ' TIME STEP OF '//NUM2STR(NSTP(N))//' TIME STEPS.')
          END DO
          PERLEN(N) = SUM(SPTIM(N)%DT)
      ELSE
          ALLOCATE( SPTIM(N)%DT( NSTP(N) ) )
          !
          IF( TS_MULT .NE. UNO) THEN
             SPTIM(N)%DT(1)= SP_LEN*(UNO-TS_MULT)/(UNO-TS_MULT**NSTP(N))
             DO I=2,NSTP(N)
                           SPTIM(N)%DT(I) = SPTIM(N)%DT(I-1)*TS_MULT
             END DO
          ELSE
              SPTIM(N)%DT=SP_LEN/DBLE(NSTP(N))
          END IF
      END IF
      !
      IF(N.EQ.1) THEN                                                   !seb ADDED ABILIITY TO HAVE REAL TIME PASS THROUGH MODEL
        K =ISTART                                                       !TEMP STORAGE OF ISTART AND ISTOP
        KK=ISTOP
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
        IF (LINE(ISTART:ISTOP).EQ.'STARTDATE'  .OR. 
     +      LINE(ISTART:ISTOP).EQ.'START_DATE' .OR. 
     +      LINE(ISTART:ISTOP).EQ.'DATE_START' .OR. 
     +      LINE(ISTART:ISTOP).EQ.'DATESTART'     ) THEN
            !
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
            CALL STARTING_DATE%INIT(LINE(ISTART:ISTOP))  !EXPECTS 1/15/2015 or 2015-1-15
            !
!!!            IF(STARTING_DATE%NOT_SET()) CALL STOP_ERROR(LINE,
!!!     +        INFILE=INDIS,OUTPUT=IOUT,MSG='FOUDN KEYWORD "STARTDATE"'//
!!!     +        ', BULT FAILED TO PARSE A DATE AFTER KEYWORD.'//NL//
!!!     +        'THE ACCEPTED FORMATS ARE:'//NL//
!!!     +       'mm/dd/yyyy                                  '//
!!!     +       '(where mm = Month, dd = Day, yyyy = four digit year '//
!!!     +       'hh = hour in 24 hour format, MM = minute, ss = second, '//
!!!     +       'and T is a flag to indicate 24 hour clock time is '//
!!!     +                                              'specified)'//NL//
!!!     +       'yyyy-mm-dd'//NL//
!!!     +       'mm/yyyy'//NL//
!!!     +       'mm/dd/yyyyThh:MM:ss'//NL//
!!!     +       'yyyy-mm-ddThh:MM:ss'//NL//
!!!     +      'OR A Decimal Year (e.g. 1979.3232)')
!!!            !
!!!            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
!!!            !CALL URWORD(LINE,LLOC,ISTART,ISTOP,Z,I,R,IOUT,INDIS)
!!!            !IF(STARTING_DATE%FRAC.EQ.0D0) THEN
!!!            !      READ(LINE(ISTART:ISTOP),*,IOSTAT=I) STARTING_DATE%FRAC
!!!            !      IF(I.NE.Z) STARTING_DATE%FRAC=0D0
!!!            !END IF
!!!            !
!!!            USE_LEAP_YR=TRUE
!!!            REALTIM=STARTING_DATE%DYEAR
!!!            REALTIM_PER=REALTIM
        END IF
        IF(LINE(ISTART:ISTOP).EQ.'LEAPYEARS' .OR.
     +     LINE(ISTART:ISTOP).EQ.'LEAPYEAR') THEN
          USE_LEAP_YR=TRUE
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
        END IF
        IF(LINE(ISTART:ISTOP).EQ.'STARTTIME')THEN
          CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS,REALTIM,
     -                                  MSG='ERROR READING STARTTIME')
          REALTIM_PER=REALTIM
          IF(USE_LEAP_YR) CALL STARTING_DATE%INIT(REALTIM)  !DATES CAN BE USED IF DECIMAL YEARS TAKE INTO ACCOUNT LEAP YEARS
        END IF
        ISTART=K
        ISTOP =KK
      END IF
      IF(SPTIM(N)%SPECIFY_DELT) THEN
        WRITE (IOUT,162) N,PERLEN(N),NEG*NSTP(N),'1.0',
     1      LINE(ISTART:ISTOP),SPTIM(N)%DT
      ELSE
        WRITE (IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),
     1      LINE(ISTART:ISTOP)
      END IF
  162 FORMAT(1X,I8,ES21.7,I7,A25,  A11,4x *(ES21.7))
  163 FORMAT(1X,I8,ES21.7,I7,F25.5,A11)
C
C15-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
C15-----TSMULT LE 0, OR PERLEN LT 0.
      IF(NSTP(N).LE.Z) THEN
!           WRITE(IOUT,164)
!  164    FORMAT(1X,/1X,
!     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=
     +'NSTP=0!?!? YOU MUST SPECIFY A NON-ZER0 TIME STEP COUNT.')
      END IF
      ZERO=0.
      IF(PERLEN(N).EQ.0D0 .AND. ISSFLG(N).EQ.Z) THEN
!           WRITE(IOUT,165)
!  165    FORMAT(1X,/1X,
!     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=
     +   'PERLEN=0!?!? YOU MUST SPECIFY A NON-ZER0 PERIOD LENGTH FOR '//
     +   '"TR" STRESS PERIODS.')
      END IF
      IF(PERLEN(N).LT.0D0) THEN
         CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=
     +   'PERLEN<0!?!? YOU MUST SPECIFY A POSTIVE, NON-ZER0 PERIOD '//
     +   'LENGTH FOR ALL STRESS PERIODS.')
      END IF
  200 CONTINUE
C
C16-----Assign ITRSS.
      IF(ISS.EQ.Z .AND. ITR.NE.Z) THEN
         ITRSS=1
           WRITE(IOUT,270)
  270    FORMAT(/,1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS.NE.Z .AND. ITR.EQ.Z) THEN
         ITRSS=Z
           WRITE(IOUT,275)
  275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS=-1
           WRITE(IOUT,280)
  280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
      !
!!!      IF (STARTING_DATE%NOT_SET()) THEN
!!!           ALLOCATE(DATE_SP(1))
!!!           ALLOCATE(DATE_SP(1)%TS(0:0))
!!!           CALL DATE_SP(1)%TS(Z)%INIT()
!!!      END IF
!!!      !
!!!      IF(STARTING_DATE%IS_SET()) THEN
!!!          WRITE(IOUT,'(/A,/2A,/A,A)')
!!!     +   '   "STARTDATE" OPTION ACTIVATED',
!!!     + 'A CALENDAR DATE WILL BE USED FOR TIME TRACKING AND DECIMAL ',
!!!     + 'YEAR CALCULATION AND PROPAGATED ALONG WITH THE SIMULATED TIME',
!!!     +    'WITH A STARTING DATE AND TIME OF ', STARTING_DATE%STR(' ')
!!!          USE_LEAP_YR=TRUE
!!!          WRITE(IOUT,'(/2A)') 'THE DECIMAL YEAR CALCULATION WILL ',
!!!     +    'TAKE INTO ACCOUNT LEAP YEARS AND MAKE A CORRECTION FOR THEM.'
!!!          !REALTIM=STARTING_DATE%DYEAR
!!!          !
!!!          IF(ITMUNI==5) THEN
!!!         WRITE(LINE,'(F15.9)') STARTING_DATE%DYEAR
!!!         LINE='DIS ERROR: "STARTDATE" Option does not work with '//
!!!     +   'time units of years (ITMUNI=5).'//NEW_LINE(' ')//'Instead '//
!!!     +  'use the "STARTTIME" option and a starting decimal '//
!!!     +   'year and not "LEAPYEARS STARTTIME" or "STARTDATE".'//
!!!     +   NEW_LINE(' ')//'The decimal year (with leap years) '//
!!!     +   'for the start date of '//STARTING_DATE%STR()//' is '
!!!     +   //NEW_LINE(' ')//TRIM(LINE)
!!!         CALL STOP_ERROR(INFILE=INDIS,OUTPUT=IOUT,MSG=LINE)
!!!          END IF
!!!          !
!!!          HAS_STARTDATE = TRUE
!!!          ALLOCATE(DATE_SP(NPER))
!!!          DO N=1,NPER
!!!              ALLOCATE(DATE_SP(N)%TS( 0:NSTP(N) ))
!!!          END DO
!!!          !
!!!          N = 1
!!!          DATE_SP(N)%TS(Z) = STARTING_DATE
!!!          DO I=1, NSTP(N)-1
!!!              DATE_SP(N)%TS(I) = DATE_SP(N)%TS(I-1)+
!!!     +                              DELT_TO_DAY(SPTIM(N)%DT(I),ITMUNI)
!!!          END DO
!!!              I = NSTP(N)
!!!              DATE_SP(N)%TS(I) = DATE_SP(N)%TS(Z)+
!!!     +                           DELT_TO_DAY(PERLEN(N),ITMUNI)
!!!          !
!!!          DO N=2,NPER
!!!            DATE_SP(N)%TS(Z) = DATE_SP(N-1)%TS( NSTP(N-1) )
!!!            DO I=1, NSTP(N)-1
!!!                DATE_SP(N)%TS(I) = DATE_SP(N)%TS(I-1)+
!!!     +                             DELT_TO_DAY(SPTIM(N)%DT(I),ITMUNI)
!!!            END DO
!!!                I = NSTP(N)
!!!                DATE_SP(N)%TS(I) = DATE_SP(N)%TS(Z)+
!!!     +                             DELT_TO_DAY(PERLEN(N),ITMUNI)
!!!          END DO
!!!          !
!!!      ELSEIF(REALTIM.GE.0D0)THEN
!!!          WRITE(IOUT,'(/A,/2A,/A,F9.4)')
!!!     +   '   "STARTTIME" OPTION ACTIVATED',
!!!     +    'A DECIMAL YEAR CALCULATION WILL BE MADE ',
!!!     +    'AND PROPAGATED ALONG WITH THE SIMULATED TIME',
!!!     +    'WITH A STARTING DECIMAL YEAR OF ', REALTIM
!!!          IF(USE_LEAP_YR)THEN
!!!            WRITE(IOUT,'(/2A)') 'THE DECIMAL YEAR CALCULATION WILL ',
!!!     +    'TAKE INTO ACCOUNT LEAP YEARS AND MAKE A CORRECTION FOR THEM.'
!!!          ELSE
!!!            WRITE(IOUT,'(/2A,//2A/)') 'THE DECIMAL YEAR CALCULATION ',
!!!     +    'ASSUMES THERE ARE 365.2425 DAYS IN A YEAR.',
!!!     +'   NOTE THAT THIS NEGATES ANY CALENDAR DATE FEATURES BECAUSE ',
!!!     +'LEAP YEARS ARE NOT ACCOUNTED FOR.'
!!!          END IF
!!!      END IF
C--------------ALLOCATE AND BUILD COORDINATE SYSTEM OF MODEL AND OPTIONALLY PRINT seb
      !
      XYGRID=>XY_GRID_COODINATES(NROW,NCOL)
      !
      CALL XYGRID%BUILD(XFIRSTCORD,YFIRSTCORD,GRIDROTATION,
     +                  LLCOODRINATE,CORNERCOORD,DELR,DELC)
      !
      IF(PRINTCOORD) CALL XYGRID%PRINT(IOUT)
C
C17-----RETURN.
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,BUFF,IBOUND,
     1                      DDREF,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                      CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
C
      DATA TEXT /'        DRAWDOWN'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS DRAWDOWN NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,2).EQ.0 .AND. IOFLG(KL,4).EQ.0) GO TO 59
C
C4------CALCULATE DRAWDOWN FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=HNEW(J,I,K)
      SSTRT=DDREF(J,I,K)
      IF(IBOUND(J,I,K).NE.0) BUFF(J,I,K)=SSTRT-HNEW(J,I,K)
   58 CONTINUE
   59 CONTINUE
C
C5------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE PRINTED.
C5------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,2).EQ.0) GO TO 69
           IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,-IDDNFM,IOUT)
           IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,IDDNFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C5A-----PRINT DRAWDOWN FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,2).NE.0) THEN
             IF(IDDNFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-IDDNFM,IOUT)
             IF(IDDNFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,IDDNFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C6------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE SAVED.
C6------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD DRAWDOWN.
      IFIRST=1
      IF(IDDNUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,4).EQ.0) GO TO 79
          IF(IFIRST.EQ.1) WRITE(IOUT,74) IDDNUN,KSTP,KPER
   74   FORMAT(1X,/1X,'DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I5,', STRESS PERIOD ',I6)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAV(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDDNUN)
        ELSE
           CALL ULASV2(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDDNUN,CDDNFM,LBDDSV,IBOUND(:,:,K))
        END IF
   79   CONTINUE
C
C6A-----SAVE DRAWDOWN FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,4).NE.0) THEN
            WRITE(IOUT,74) IDDNUN,KSTP,KPER
          IF(CDDNFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IDDNUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IDDNUN,CDDNFM,LBDDSV,IBOUND)
          END IF
        END IF
      END IF
C
C7------RETURN.
   80 RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND RECORD HEADS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,BUFF,
     1                      IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBHDSV,
     2                      CHEDFM,IOFLG
C
      CHARACTER*16 TEXT
      DATA TEXT /'            HEAD'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0) GO TO 59
C
C3------MOVE HNEW TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=HNEW(J,I,K)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT HEAD.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,1).EQ.0) GO TO 69
           IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,-IHEDFM,IOUT)
           IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,IHEDFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,1).NE.0) THEN
             IF(IHEDFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-IHEDFM,IOUT)
             IF(IHEDFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,IHEDFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(IHEDUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,3).EQ.0) GO TO 79
          IF(IFIRST.EQ.1) WRITE(IOUT,74) IHEDUN,KSTP,KPER
   74   FORMAT(1X,/1X,'HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I5,', STRESS PERIOD ',I6)
        IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
           CALL ULASAV(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN)
        ELSE
           CALL ULASV2(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN,CHEDFM,LBHDSV,IBOUND(:,:,K))
        END IF
   79   CONTINUE
C
C5A-----SAVE HEAD FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,3).NE.0) THEN
            WRITE(IOUT,74) IHEDUN,KSTP,KPER
          IF(CHEDFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IHEDUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IHEDUN,CHEDFM,LBHDSV,IBOUND)
          END IF
        END IF
      END IF
C
C6------RETURN.
   80 RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
C
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(IBOUUN.LE.0) RETURN
C
C5------FOR EACH LAYER: SAVE IBOUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,5).EQ.0) GO TO 79
          IF(IFIRST.EQ.1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I5,', STRESS PERIOD ',I6)
        IFIRST=0
        CALL ULASV3(IBOUND(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IBOUUN,CBOUFM,LBBOSV)
   79   CONTINUE
C
C5A-----SAVE IBOUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,5).NE.0) THEN
            WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3(IBOUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IBOUUN,CBOUFM,LBBOSV)
        END IF
      END IF
C
C6------RETURN.
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7I(NLAY,INOC,IOUT,IFREFM,MXBUD)
C     ******************************************************************
C     SET UP OUTPUT CONTROL.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1                        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2                        IBOUUN,LBBOSV,CBOUFM,IAUXSV,IOFLG,
     3                        VBVL,VBNM,IDDREF,IDDREFNEW,IUBGT,PVOL_ERR
      USE GLOBAL,         ONLY: IRESTART,KPERSTART,KSTPSTART,IUNITSTART
      USE UTIL_INTERFACE, ONLY: READ_TO_DATA, PARSE_WORD_UP
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
C
C1-----ALLOCATE SPACE FOR IOFLG, VBVL, AND VBNM ARRAYS.
      ALLOCATE (IOFLG(NLAY,5))
      ALLOCATE (VBVL(4,MXBUD))
      ALLOCATE (VBNM(MXBUD))
      ALLOCATE(PVOL_ERR)
      PVOL_ERR = 0D0
      IDDREF=0
      IDDREFNEW=0
C
C1------ASSIGN DEFAULT VALUES.
      CHEDFM=' '
      CDDNFM=' '
      CBOUFM='(20I4)'
      IHEDFM=0
      IDDNFM=0
      IHEDUN=0
      IDDNUN=0
      IBOUUN=0
      IBDOPT=1
      LBHDSV=0
      LBDDSV=0
      LBBOSV=0
      IAUXSV=0
      IUBGT=0
C
C2------TEST OUTPUT CONTROL INPUT UNIT TO SEE IF OUTPUT CONTROL IS
C2------ACTIVE.
      IF(INOC.EQ.0) THEN
C
C2A-----OUTPUT CONTROL IS INACTIVE. PRINT A MESSAGE LISTING DEFAULTS.
         WRITE(IOUT, 41)
   41    FORMAT(1X,/1X,'DEFAULT OUTPUT CONTROL',/1X,
     1   'THE FOLLOWING OUTPUT COMES AT THE END OF EACH STRESS PERIOD:')
           WRITE(IOUT, 42)
   42    FORMAT(1X,'TOTAL VOLUMETRIC BUDGET')
           WRITE(IOUT, 43)
   43    FORMAT(1X,10X,'HEAD')
C
C2B-----SET DEFAULT FLAGS IN IOFLG SO THAT HEAD IS PRINTED FOR
C2B-----EVERY LAYER.
         DO 80 K=1,NLAY
         IOFLG(K,1)=1
         IOFLG(K,2)=0
         IOFLG(K,3)=0
         IOFLG(K,4)=0
         IOFLG(K,5)=0
   80    CONTINUE
         GO TO 1000
      END IF
C
C3------OUTPUT CONTROL IS ACTIVE.  READ FIRST RECORD AND DECODE FIRST
C3------WORD.  MUST USE URWORD IN CASE FIRST WORD IS ALPHABETIC.
      CALL READ_TO_DATA(LINE,INOC,IOUT,IOUT)
      !CALL URDCOM(INOC,IOUT,LINE)
      LLOC=1
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
C
C4------TEST FOR NUMERIC OUTPUT CONTROL.  FIRST WORD WILL NOT BE
C4------"PERIOD", "HEAD", "DRAWDOWN", OR "COMPACT".
      IRESTART = 0
      KPERSTART = 0
      KSTPSTART = 0
      IUNITSTART = 0
      ITST = 0
      IF(LINE(ISTART:ISTOP).EQ.'RESTART' ) THEN
        IRESTART = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KPERSTART,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KSTPSTART,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITSTART,R,IOUT,IN)
        IF ( KPERSTART.LE.0 .OR. KSTPSTART.LE.0 ) THEN
          IRESTART = 0
        END IF
        IF ( IRESTART.GT.0 ) THEN
            WRITE(IOUT,100)KPERSTART,KSTPSTART
  100     FORMAT(1X,/1X,'RESTART OPTION ACTIVE. RESTART AT PERIOD ',I6,
     +          'STEP ',I5)
            WRITE(IOUT,101)IUNITSTART
  101     FORMAT(1X,/1X,'RESTART HEADS WILL BE READ FROM UNIT ',I5)
        END IF
      END IF
      IF ( IRESTART.GT.0 ) THEN
        CALL READ_TO_DATA(LINE,INOC,IOUT,IOUT)
        !CALL URDCOM(INOC,IOUT,LINE)
        LLOC=1
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      END IF
      IF(LINE(ISTART:ISTOP).NE.'PERIOD' .AND. LINE(ISTART:ISTOP).NE.
     1     'HEAD' .AND. LINE(ISTART:ISTOP).NE.'DRAWDOWN' .AND.
     2     LINE(ISTART:ISTOP).NE.'COMPACT' .AND.
     3     LINE(ISTART:ISTOP).NE.'IBOUND') THEN
C4A-----NUMERIC OUTPUT CONTROL.  DECODE THE INITIAL RECORD ACCORDINGLY.
           WRITE(IOUT,102)
  102    FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED EVERY TIME STEP')
         IF(IFREFM.EQ.0) THEN
            READ(LINE,'(4I10)') IHEDFM,IDDNFM,IHEDUN,IDDNUN
         ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
         END IF
           WRITE(IOUT,103) IHEDFM,IDDNFM
  103    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1     '    DRAWDOWN PRINT FORMAT CODE IS',I4)
           WRITE(IOUT,104) IHEDUN,IDDNUN
  104    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,
     1     '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
         IPEROC=-1
         ITSOC=-1
      ELSE
C4B-----ALPHABETIC OUTPUT CONTROL.  CALL MODULE TO READ INITIAL RECORDS.
         CALL SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
      END IF
C
C5------RETURN.
 1000 RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
C     ******************************************************************
C     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE UTIL_INTERFACE, ONLY: PARSE_WORD,PARSE_WORD_UP,
     +                          STOP_ERROR
      USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1                        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2                        IBOUUN,LBBOSV,CBOUFM,IAUXSV,IDDREFNEW,
     3                        IUBGT
C
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
C
C1------ALPHABETIC OUTPUT CONTROL.  WRITE MESSAGE AND SET INITIAL VALUES
C1------FOR IPEROC AND ITSOC.
        WRITE(IOUT,91)
   91 FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED ONLY AT TIME STEPS',
     1    ' FOR WHICH OUTPUT IS DESIRED')
      IPEROC=9999
      ITSOC=9999
C
C2------LOOK FOR ALPHABETIC WORDS:
C2A-----LOOK FOR "PERIOD", WHICH INDICATES THE END OF INITIAL OUTPUT
C2A-----CONTROL DATA.  IF FOUND, DECODE THE PERIOD NUMBER AND TIME
C2A-----STEP NUMBER FOR LATER USE.
  100 IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
           WRITE(IOUT,101) IHEDFM,IDDNFM
  101    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1        '    DRAWDOWN PRINT FORMAT CODE IS',I4)
           WRITE(IOUT,102) IHEDUN,IDDNUN
  102    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,
     1        '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4,
     2        '  WATER BUDGETs WILL BE SAVED ON UNIT ',I4)
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         GO TO 1000
C
C2B-----LOOK FOR "HEAD PRINT ..." AND "HEAD SAVE ...".  IF
C2B-----FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,
     1            INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
               CHEDFM=LINE(ISTART:ISTOP)
                 WRITE(IOUT,103) CHEDFM
  103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
               CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBHDSV=1
                    WRITE(IOUT,104)
  104             FORMAT(1X,'SAVED HEADS WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2C-----LOOK FOR "DRAWDOWN PRINT ..." AND "DRAWDOWN SAVE ...".
C2C-----IF FOUND, SET APPROPRIATE FLAGS
      ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,
     1                   INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
               CDDNFM=LINE(ISTART:ISTOP)
                 WRITE(IOUT,105) CDDNFM
  105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
               CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBDDSV=1
                    WRITE(IOUT,106)
  106             FORMAT(1X,'SAVED DRAWDOWN WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2D-----LOOK FOR "COMPACT BUDGET FILES" -- "COMPACT" IS SUFFICIENT.
C2D-----IF FOUND, SET APPROPRIATE FLAG.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPACT') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
            IBDOPT=2
              WRITE(IOUT,107)
  107       FORMAT(1X,
     1      'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1         LINE(ISTART:ISTOP).EQ.'AUX') THEN
               IAUXSV=1
                 WRITE(IOUT,108)
  108          FORMAT(1X,
     1     'AUXILIARY DATA WILL BE SAVED IN CELL-BY-CELL BUDGET FILES')
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2E-----LOOK FOR  "IBOUND SAVE ...".  IF FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBOUUN,R,IOUT,
     1            INOC)
                 WRITE(IOUT,111) IBOUUN
  111          FORMAT(1X,'IBOUND WILL BE SAVED ON UNIT ',I4)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
               CBOUFM=LINE(ISTART:ISTOP)
                 WRITE(IOUT,112) CBOUFM
  112          FORMAT(1X,'IBOUND WILL BE SAVED WITH FORMAT: ',A)
               CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBBOSV=1
                    WRITE(IOUT,109)
  109             FORMAT(1X,'SAVED IBOUND WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2F-----LOOK FOR "WBGT SAVE ...".  IF
C2F-----FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'WBGT') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUBGT,R,IOUT,
     1            INOC)
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2G-----ERROR IF UNRECOGNIZED WORD.
      ELSE
         GO TO 2000
      END IF
C
C3------FINISHED READING A RECORD.  READ NEXT RECORD, IGNORING BLANK
C3------LINES.  GO BACK AND DECODE IT.
  110 READ(INOC,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 110
      LLOC=1
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      GO TO 100
C
C4------RETURN.
 1000 RETURN
C
C5------ERROR DECODING INPUT DATA.
 !2000 WRITE(IOUT,2001) LINE
 !2001 FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
 !     CALL USTOP(' ')

 2000   CALL STOP_ERROR(LINE,INOC,IOUT,MSG=
     +'ERROR READING OUTPUT CONTROL (OC) INPUT DATA ON SPECIFIED LINE.')
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
C     ******************************************************************
C     PRINT SIMULATION TIME
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE,ONLY:REALTIM, DATE_SP, HAS_STARTDATE
      USE BAS_UTIL, ONLY:MONTHPRINT
      USE DATE_OPERATOR_INSTRUCTION
      !TYPE(DATE_OPERATOR):: PREV_DATE
C     ------------------------------------------------------------------
C
        WRITE(IOUT,199) KSTP,KPER
  199 FORMAT(1X,/10X,'TIME SUMMARY AT END OF TIME STEP ',I5,
     1     ' IN STRESS PERIOD ',I6)
C
C1------USE TIME UNIT INDICATOR TO GET FACTOR TO CONVERT TO SECONDS.
      ZERO=0.
      CNV=ZERO
      IF(ITMUNI.EQ.1) CNV=1.
      IF(ITMUNI.EQ.2) CNV=60.
      IF(ITMUNI.EQ.3) CNV=3600.
      IF(ITMUNI.EQ.4) CNV=86400.
      IF(ITMUNI.EQ.5) CNV=31557600.
C
C2------IF FACTOR=0 THEN TIME UNITS ARE NON-STANDARD.
      IF(CNV.NE.ZERO) GO TO 100
C
C2A-----PRINT TIMES IN NON-STANDARD TIME UNITS.
        WRITE(IOUT,301) DELT,PERTIM,TOTIM
  301 FORMAT(21X,'     TIME STEP LENGTH =',G15.6/
     1       21X,'   STRESS PERIOD TIME =',G15.6/
     2       21X,'TOTAL SIMULATION TIME =',G15.6)
C
C2B-----RETURN
      RETURN
C
C3------CALCULATE LENGTH OF TIME STEP & ELAPSED TIMES IN SECONDS.
  100 DELSEC=CNV*DELT
      TOTSEC=CNV*TOTIM
      PERSEC=CNV*PERTIM
C
C4------CALCULATE TIMES IN MINUTES,HOURS,DAYS AND YEARS.
      SIXTY=60.
      HRDAY=24.
      DAYYR=365.25
      DELMN=DELSEC/SIXTY
      DELHR=DELMN/SIXTY
      DELDY=DELHR/HRDAY
      DELYR=DELDY/DAYYR
      TOTMN=TOTSEC/SIXTY
      TOTHR=TOTMN/SIXTY
      TOTDY=TOTHR/HRDAY
      TOTYR=TOTDY/DAYYR
      PERMN=PERSEC/SIXTY
      PERHR=PERMN/SIXTY
      PERDY=PERHR/HRDAY
      PERYR=PERDY/DAYYR
C
C5------PRINT TIME STEP LENGTH AND ELAPSED TIMES IN ALL TIME UNITS.
        IF(REALTIM<0D0)THEN                                             !seb
          WRITE(IOUT,200)
        ELSEIF (HAS_STARTDATE) THEN
          WRITE(IOUT,221) DATE_SP(KPER)%TS(KSTP-1)%STR(' '),
     +                    DATE_SP(KPER)%TS(KSTP  )%STR(' ')
        ELSE
          WRITE(IOUT,220)REALTIM,TRIM(MONTHPRINT(REALTIM))
        END IF
  200 FORMAT(19X,' SECONDS     MINUTES      HOURS',7X,
     1    'DAYS        YEARS'/20X,59('-'))
        WRITE (IOUT,201) DELSEC,DELMN,DELHR,DELDY,DELYR
  201 FORMAT(1X,'  TIME STEP LENGTH',1P,5G12.5)
        WRITE(IOUT,202) PERSEC,PERMN,PERHR,PERDY,PERYR
  202 FORMAT(1X,'STRESS PERIOD TIME',1P,5G12.5)
        WRITE(IOUT,203) TOTSEC,TOTMN,TOTHR,TOTDY,TOTYR
  203 FORMAT(1X,'        TOTAL TIME',1P,5G12.5)
  220 FORMAT(19X,' SECONDS     MINUTES      HOURS',7X,
     1    'DAYS        YEARS     DECIMAL YEAR: ',F10.4,' (',A,')',
     2     /20X,59('-'))
  221 FORMAT(19X,' SECONDS     MINUTES      HOURS',7X,
     1  'DAYS        YEARS     DATE:  ',A,'  TO  ',A,
     2     /20X,59('-'))
C
C6------RETURN
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,IUBGT,
     +                                   BUDPERC,TOTRIN,TOTROT,PVOL_ERR)
C     ******************************************************************
C     PRINT VOLUMETRIC BUDGET
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 VBNM(MSUM)
      DIMENSION VBVL(4,MSUM)
      CHARACTER*17 VAL1,VAL2
C     ------------------------------------------------------------------
C
C1------DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.
      BUDPERC=0.
      MSUM1=MSUM-1
      IF(MSUM1.LE.0) RETURN
C
C2------CLEAR RATE AND VOLUME ACCUMULATORS.
      ZERO=0.
      TWO=2.
      HUND=100.
      BIGVL1=9.99999E11
      BIGVL2=9.99999E10
      SMALL=0.1
      TOTRIN=ZERO
      TOTROT=ZERO
      TOTVIN=ZERO
      TOTVOT=ZERO
C
C3------ADD RATES AND VOLUMES (IN AND OUT) TO ACCUMULATORS.
      DO 100 L=1,MSUM1
      TOTRIN=TOTRIN+VBVL(3,L)
      TOTROT=TOTROT+VBVL(4,L)
      TOTVIN=TOTVIN+VBVL(1,L)
      TOTVOT=TOTVOT+VBVL(2,L)
  100 CONTINUE
C
C4------PRINT TIME STEP NUMBER AND STRESS PERIOD NUMBER.
      WRITE(IOUT,260) KSTP,KPER
      WRITE(IOUT,265)
      !
      If(IUBGT>0) WRITE(IUBGT,260) KSTP,KPER
      If(IUBGT>0) WRITE(IUBGT,265)
C
C5------PRINT INDIVIDUAL INFLOW RATES AND VOLUMES AND THEIR TOTALS.
      DO 200 L=1,MSUM1
      IF(VBVL(1,L).NE.ZERO .AND.
     1       (VBVL(1,L).GE.BIGVL1 .OR. VBVL(1,L).LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') VBVL(1,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVL(1,L)
      END IF
      IF(VBVL(3,L).NE.ZERO .AND.
     1       (VBVL(3,L).GE.BIGVL1 .OR. VBVL(3,L).LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') VBVL(3,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVL(3,L)
      END IF
      WRITE(IOUT,275) VBNM(L),VAL1,VBNM(L),VAL2
      If(IUBGT>0) WRITE(IUBGT,275) VBNM(L),VAL1,VBNM(L),VAL2
  200 CONTINUE
      IF(TOTVIN.NE.ZERO .AND.
     1      (TOTVIN.GE.BIGVL1 .OR. TOTVIN.LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') TOTVIN
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVIN
      END IF
      IF(TOTRIN.NE.ZERO .AND.
     1      (TOTRIN.GE.BIGVL1 .OR. TOTRIN.LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') TOTRIN
      ELSE
         WRITE(VAL2,'(F17.4)') TOTRIN
      END IF
      WRITE(IOUT,286) VAL1,VAL2
      If(IUBGT>0)  WRITE(IUBGT,286) VAL1,VAL2
C
C6------PRINT INDIVIDUAL OUTFLOW RATES AND VOLUMES AND THEIR TOTALS.
      WRITE(IOUT,287)
      If(IUBGT>0) WRITE(IUBGT,287)
      DO 250 L=1,MSUM1
      IF(VBVL(2,L).NE.ZERO .AND.
     1       (VBVL(2,L).GE.BIGVL1 .OR. VBVL(2,L).LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') VBVL(2,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVL(2,L)
      END IF
      IF(VBVL(4,L).NE.ZERO .AND.
     1       (VBVL(4,L).GE.BIGVL1 .OR. VBVL(4,L).LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') VBVL(4,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVL(4,L)
      END IF
      WRITE(IOUT,275) VBNM(L),VAL1,VBNM(L),VAL2
      If(IUBGT>0) WRITE(IUBGT,275) VBNM(L),VAL1,VBNM(L),VAL2
  250 CONTINUE
      IF(TOTVOT.NE.ZERO .AND.
     1      (TOTVOT.GE.BIGVL1 .OR. TOTVOT.LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') TOTVOT
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVOT
      END IF
      IF(TOTROT.NE.ZERO .AND.
     1      (TOTROT.GE.BIGVL1 .OR. TOTROT.LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') TOTROT
      ELSE
         WRITE(VAL2,'(F17.4)') TOTROT
      END IF
      WRITE(IOUT,298) VAL1,VAL2
      If(IUBGT>0) WRITE(IUBGT,298) VAL1,VAL2
C
C7------CALCULATE THE DIFFERENCE BETWEEN INFLOW AND OUTFLOW.
C
C7A-----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      DIFFR=TOTRIN-TOTROT
      ADIFFR=ABS(DIFFR)
C
C7B-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR=ZERO
      AVGRAT=(TOTRIN+TOTROT)/TWO
      IF(AVGRAT.NE.ZERO) PDIFFR=HUND*DIFFR/AVGRAT
      BUDPERC=PDIFFR
C
C7C-----CALCULATE DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      DIFFV=TOTVIN-TOTVOT
      ADIFFV=ABS(DIFFV)
C
C7D-----GET PERCENT DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      PDIFFV=ZERO
      AVGVOL=(TOTVIN+TOTVOT)/TWO
      IF(AVGVOL.NE.ZERO) PDIFFV=HUND*DIFFV/AVGVOL
      PVOL_ERR = PDIFFV
C
C8------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT
C8------AND OUTPUT RATES AND VOLUMES.
      IF(ADIFFV.NE.ZERO .AND.
     1      (ADIFFV.GE.BIGVL2 .OR. ADIFFV.LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') DIFFV
      ELSE
         WRITE(VAL1,'(F17.4)') DIFFV
      END IF
      IF(ADIFFR.NE.ZERO .AND.
     1      (ADIFFR.GE.BIGVL2 .OR. ADIFFR.LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') DIFFR
      ELSE
         WRITE(VAL2,'(F17.4)') DIFFR
      END IF
      WRITE(IOUT,299) VAL1,VAL2
      WRITE(IOUT,300) PDIFFV,PDIFFR
      If(IUBGT>0) WRITE(IUBGT,299) VAL1,VAL2
      If(IUBGT>0) WRITE(IUBGT,300) PDIFFV,PDIFFR
C
C9------RETURN.
      RETURN
C
C    ---FORMATS
C
  260 FORMAT(/,/,/,2X, 85('-'),/2X,
     1 'VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF'
     2,' TIME STEP',I5,' IN STRESS PERIOD ',I6/2X,85('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE VOLUMES',6X,'L**3',7X
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/5X,18('-'),17X,24('-')
     2//11X,'IN:',38X,'IN:'/11X,'---',38X,'---')
  275 FORMAT(1X,3X,A16,' =',A17,6X,A16,' =',A17)
  286 FORMAT(1X,/12X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
  287 FORMAT(1X,/10X,'OUT:',37X,'OUT:'/10X,4('-'),37X,4('-'))
  298 FORMAT(1X,/11X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A)
  299 FORMAT(1X,/12X,'IN - OUT =',A,14X,'IN - OUT =',A)
  300 FORMAT(1X,/1X,'PERCENT DISCREPANCY =',F15.2
     1,5X,'PERCENT DISCREPANCY =',F15.2,/)
C
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7VNOPRT(MSUM,VBVL,BUDPERC,TOTRIN,TOTROT)
C     ******************************************************************
C     CALCULATE AND DO NOT PRINT VOLUMETRIC BUDGET
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VBVL(4,MSUM)
C     ------------------------------------------------------------------
C
C1------DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.
      BUDPERC=0.
      MSUM1=MSUM-1
      IF(MSUM1.LE.0) RETURN
C
C2------CLEAR RATE AND VOLUME ACCUMULATORS.
      ZERO=0.
      TWO=2.
      HUND=100.
      TOTRIN=ZERO
      TOTROT=ZERO
      TOTVIN=ZERO
      TOTVOT=ZERO
C
C3------ADD RATES AND VOLUMES (IN AND OUT) TO ACCUMULATORS.
      DO 100 L=1,MSUM1
      TOTRIN=TOTRIN+VBVL(3,L)
      TOTROT=TOTROT+VBVL(4,L)
      TOTVIN=TOTVIN+VBVL(1,L)
      TOTVOT=TOTVOT+VBVL(2,L)
  100 CONTINUE
C
C7------CALCULATE THE DIFFERENCE BETWEEN INFLOW AND OUTFLOW.
C
C7A-----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      DIFFR=TOTRIN-TOTROT
C
C7B-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR=ZERO
      AVGRAT=(TOTRIN+TOTROT)/TWO
      IF(AVGRAT.NE.ZERO) PDIFFR=HUND*DIFFR/AVGRAT
      BUDPERC=PDIFFR
C
C9------RETURN.
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7N(KPER,KSTP,INOC,IOUT,NLAY)
C     ******************************************************************
C     SET OUTPUT FLAGS USING ALPHABETIC OUTPUT CONTROL INPUT STRUCTURE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IOFLG,IHDDFL,IBUDFL,ICBCFL,IPEROC,
     1                        ITSOC,IBDOPT,IDDREF,IDDREFNEW
      USE UTIL_INTERFACE,  ONLY: STOP_ERROR
C
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
C
C1------ERROR IF OUTPUT CONTROL TIME STEP PRECEDES CURRENT SIMULATION
C1------TIME STEP.
      IF((IPEROC.LT.KPER).OR.(IPEROC.EQ.KPER .AND. ITSOC.LT.KSTP)) THEN
           WRITE(IOUT,5) IPEROC,ITSOC,KPER,KSTP
    5    FORMAT(1X,/1X,'OUTPUT CONTROL WAS SPECIFIED FOR A NONEXISTENT',
     1   ' TIME STEP',/
     2   1X,'OR OUTPUT CONTROL DATA ARE NOT ENTERED IN ASCENDING ORDER',
     3   /1X,'OUTPUT CONTROL STRESS PERIOD ',I6,'   TIME STEP ',I5,/
     4   1X,'MODEL STRESS PERIOD ',I6,'   TIME STEP ',I5,/
     5   1X,'APPLYING THE SPECIFIED OUTPUT CONTROL TO THE CURRENT TIME',
     6   ' STEP')
         IPEROC=KPER
         ITSOC=KSTP
      END IF
C
C2------CLEAR I/O FLAGS.
      IHDDFL=0
      IBUDFL=0
      ICBCFL=0
      DO 10 I=1,5
      DO 10 K=1,NLAY
      IOFLG(K,I)=0
10    CONTINUE
C
C3------IF OUTPUT CONTROL TIME STEP DOES NOT MATCH SIMULATION TIME STEP,
C3------WRITE MESSAGE THAT THERE IS NO OUTPUT CONTROL THIS TIME STEP,
C3------AND RETURN.
      IF(IPEROC.NE.KPER .OR. ITSOC.NE.KSTP) THEN
           WRITE(IOUT,11) KPER,KSTP
11       FORMAT(1X,/1X,'NO OUTPUT CONTROL FOR STRESS PERIOD ',I6,
     1              '   TIME STEP ',I5)
         RETURN
      END IF
C
C4------OUTPUT CONTROL TIME STEP MATCHES SIMULATION TIME STEP.
      IDDREF=IDDREFNEW
        WRITE(IOUT,12) IPEROC,ITSOC
12    FORMAT(1X,/1X,'OUTPUT CONTROL FOR STRESS PERIOD ',I6,
     1              '   TIME STEP ',I5)
        IF(IDDREFNEW.NE.0) WRITE(IOUT,52)
   52      FORMAT(1X,'Drawdown Reference will be reset at the',
     1               ' end of this time step')
C
C4A-----OUTPUT CONTROL MATCHES SIMULATION TIME.  READ NEXT OUTPUT
C4A-----RECORD; SKIP ANY BLANK LINES.
50    READ(INOC,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 50
C
C4A1----LOOK FOR "PERIOD", WHICH TERMINATES OUTPUT CONTROL FOR CURRENT
C4A1----TIME STEP.  IF FOUND, DECODE TIME STEP FOR NEXT OUTPUT.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
      IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         RETURN
C
C4A2----LOOK FOR "PRINT", WHICH MAY REFER TO "BUDGET", "HEAD", OR
C4A2----"DRAWDOWN".
      ELSE IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
              WRITE(IOUT,53)
53          FORMAT(4X,'PRINT BUDGET')
            IBUDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
            CALL SGWF2BAS7L(1,LINE,LLOC,IOFLG,NLAY,IOUT,'PRINT HEAD',
     1              INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
            CALL SGWF2BAS7L(2,LINE,LLOC,IOFLG,NLAY,IOUT,
     1              'PRINT DRAWDOWN',INOC)
            IHDDFL=1
         ELSE
            GO TO 2000
         END IF
C
C4A3----LOOK FOR "SAVE", WHICH MAY REFER TO "BUDGET", "HEAD",
C4A3----"DRAWDOWN", OR "IBOUND".
      ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
              WRITE(IOUT,57)
57          FORMAT(4X,'SAVE BUDGET')
            ICBCFL=IBDOPT
         ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
            CALL SGWF2BAS7L(3,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE HEAD',
     &                      INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
            CALL SGWF2BAS7L(4,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE DRAWDOWN',
     1          INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
            CALL SGWF2BAS7L(5,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE IBOUND',
     1                     INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'WBGT') THEN
            WRITE(IOUT,58)
58          FORMAT(4X,'SAVE WATER BUDGET')
            IBUDFL=1
          ELSE
            GO TO 2000
         END IF
C
C4A4----WHEN NO KNOWN ALPHABETIC WORDS ARE FOUND, THERE IS AN ERROR.
      ELSE
         GO TO 2000
C
C4B-----AFTER SUCCESSFULLY DECODING ONE RECORD, READ ANOTHER.
      END IF
      GO TO 50
C
C5------END OF FILE WHILE READING AN OUTPUT CONTROL RECORD, SO THERE
C5------WILL BE NO FURTHER OUTPUT.  SET IPEROC AND ITSOC HIGH ENOUGH
C5------THAT THE MODEL TIME WILL NEVER MATCH THEM.
1000  IPEROC=9999
      ITSOC=9999
      RETURN
C
C6------ERROR DECODING ALPHABETIC INPUT STRUCTURE.
!2000  WRITE(IOUT,2001) LINE
!2001  FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
!      CALL USTOP(' ')
 2000   CALL STOP_ERROR(LINE,INOC,IOUT,MSG=
     +'ERROR READING OUTPUT CONTROL (OC) INPUT DATA ON SPECIFIED LINE.')
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7L(IPOS,LINE,LLOC,IOFLG,NLAY,IOUT,LABEL,INOC)
C     ******************************************************************
C     WHEN USING ALPHABETIC OUTPUT CONTROL, DECODE LAYER
C     NUMBERS FOR PRINTING OR SAVING HEAD OR DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IOFLG(NLAY,5)
      CHARACTER*700 LINE
      CHARACTER*(*) LABEL
      DIMENSION LAYER(999)
C     ------------------------------------------------------------------
C
C1------INITIALIZE COUNTER FOR NUMBER OF LAYERS FOR WHICH OUTPUT IS
C1------SPECIFIED.
      NSET=0
C
C2------CHECK FOR A VALID LAYER NUMBER.  WHEN FOUND, SET FLAG AND
C2------REPEAT.
10    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,L,R,0,INOC)
      IF(L.GT.0 .AND. L.LE.NLAY) THEN
         NSET=NSET+1
         LAYER(NSET)=L
         IOFLG(L,IPOS)=1
         GO TO 10
      END IF
C
C3------DONE CHECKING FOR LAYER NUMBERS.  IF NO LAYER NUMBERS WERE
C3------FOUND, SET FLAGS FOR ALL LAYERS.
      IF(NSET.EQ.0) THEN
         DO 110 K=1,NLAY
         IOFLG(K,IPOS)=1
110      CONTINUE
           WRITE(IOUT,111) LABEL
111      FORMAT(4X,A,' FOR ALL LAYERS')
C
C4------IF ONE OR MORE LAYER NUMBERS WERE FOUND, PRINT THE NUMBERS.
      ELSE
           WRITE(IOUT,112) LABEL,(LAYER(M),M=1,NSET)
112      FORMAT(4X,A,' FOR LAYERS:',(1X,15I3))
      END IF
C
C5------RETURN.
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,
     1              NIUNIT,INBAS,LISTSPLIT)
C     ******************************************************************
C     OPEN FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE OPENSPEC
      USE GENERIC_OPEN_INTERFACE
      USE UTIL_INTERFACE,ONLY:READ_TO_DATA,CHECK_FOR_POST_KEY,
     +             UPPER,STOP_ERROR,WARNING_MESSAGE,
     +             GET_WARN,PARSE_WORD
      USE NUM2STR_INTERFACE,       ONLY: NUM2STR
      USE LINKED_LIST_INSTRUCTION, ONLY: CHARACTER_LINKED_LIST
      USE FILE_INCREMENTER_INTERFACE
      USE CONSTANTS, ONLY: Z, ONE, TWO, TRUE, FALSE, NL, BLN
      USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
      USE GLOBAL, ONLY: DATAFILES
C      INCLUDE 'openspec.inc'
      TYPE(CHARACTER_LINKED_LIST):: LST,DIR
      TYPE(FILE_INCREMENTER):: LISTSPLIT
      INTEGER,        DIMENSION(NIUNIT):: IUNIT
      CHARACTER(5  ), DIMENSION(NIUNIT):: CUNIT
      CHARACTER(7  ):: FILSTAT
      CHARACTER(20 ):: FILACT, FMTARG, ACCARG
      CHARACTER(700):: LINE, FNAME
      CHARACTER(:), ALLOCATABLE:: WARN_FILE
      CHARACTER(20 ):: FILTYP
      LOGICAL::LOP
      INTEGER::BUFBLOCKSIZE, BUFCOUNT, WARN_IU
      !CHARACTER(:),ALLOCATABLE:: OLD,REED,REPL,WRIT,REWR
      CHARACTER(3):: ASYN
      LOGICAL:: UNIT_SPECIFIED, NOPRINT, NOLIST, NOWARN, EOF, CHK
      DOUBLE PRECISION:: DP
      CHARACTER(:), ALLOCATABLE:: TMP !COMPILER BUG REQUIRED THIS TMP VARIABLE
      TYPE(DATE_OPERATOR):: DATE
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.
      ISPLIT=-1
      UNIT_SPECIFIED=.FALSE.
      NOLIST = TRUE
      NOWARN = TRUE
      CHK= FALSE
      INBAS=Z
      NFILE=Z
      IOUT=Z
      WARN_IU=Z
      IUNIT=Z
      !
      CALL LST%INIT()
      CALL DIR%INIT()
      !
      ! SCAN FOR VARIABLES
      CALL UTF8_BOM_OFFSET_REWIND(INUNIT)  ! REWIND(INUNIT)  
      EOF=FALSE
      DO !WHILE (.NOT. EOF)
        CALL READ_TO_DATA(LINE,INUNIT,EOF=EOF)
        IF(EOF) EXIT
        LLOC=ONE
        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
        !
        IF(LINE(ISTART:ISTART)=="%") THEN  !SHOULD BE ON FIRST COLUMN CAUSE OF ADJUSTL
           IF(LINE(ISTOP:ISTOP).NE."%") CALL STOP_ERROR(
     +          LINE,INUNIT,Z, MSG=
     +          'FOUND FIRST %, BUT FAILED TO LOCATE A CORRESPONDING '//
     +          'CLOSING % (e.g. expect %XYZ%, but got %XYZ')
                !
           CALL UPPER(LINE(ISTART:ISTOP))
           CALL PARSE_WORD(LINE,LLOC,ITYP1,ITYP2)
           !
           TMP = LINE(ISTART:ISTOP)
           CALL LST%ADD(TMP)
           !
           TMP = LINE(ITYP1:ITYP2)
           CALL DIR%ADD(TMP)
           CHK = TRUE
        END IF
      END DO
C
C2----OPEN LIST FILE
      CALL UTF8_BOM_OFFSET_REWIND(INUNIT)  ! REWIND(INUNIT)  
      LINE='START'
      DO !WHILE (.NOT. EOF)
        CALL READ_TO_DATA(LINE,INUNIT,Z,EOF=EOF)
        IF(EOF) EXIT
        !
        LLOC=1
        CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,Z,INUNIT)
        !
        IF(LINE(ITYP1:ITYP2).EQ.'LIST') THEN
          IF(CHK) CALL SUB_LST_VAR(LINE,LST,DIR,ITYP1,ITYP2,INUNIT,Z)
          ASYN='NO'
          BUFBLOCKSIZE = Z                                     !SET TO 1048576 WITH BUFFER KEYWORD
          BUFCOUNT = Z
          !FILTYP=LINE(ITYP1:ITYP2)
          CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
          READ(LINE(INAM1:INAM2), *, IOSTAT=IERR) NN
          IF (IERR==Z) THEN
             IOUT=NN
             CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
             FNAME=LINE(INAM1:INAM2)
             UNIT_SPECIFIED=.TRUE.
             IF(IOUT==WARN_IU) THEN
            WRITE(*,11) TRIM(FNAME),IU
   11       FORMAT(1X,/1X,'LIST CANNOT OPEN ',A,' ON UNIT',I4,
     1              ' BECAUSE UNIT IS ALREADY BEING USED')
             END IF
          ELSE
             IOUT=Z
             FNAME=LINE(INAM1:INAM2)
          END IF
          !
          FILSTAT = FNAME
          CALL UPCASE(FILSTAT)
          !
          IF(FILSTAT .NE. 'NOPRINT') THEN
            CALL CHECK_FOR_POST_KEY(LLOC,LINE,INUNIT,Z,BUFBLOCKSIZE,
     +                              ISPLIT,ASYN=ASYN, NOPRINT=NOLIST)   !NOLIST CHANGED TO FALSE IF NOPRINT IS NOT FOUND -- SET TO TRUE OUTSIDE OF LOOP
          END IF
          !
          IF(.NOT. NOLIST) THEN
            IF(BUFBLOCKSIZE.GT.Z) BUFCOUNT = 2
            !
            IF (ASYN=='YES' .AND. BUFBLOCKSIZE>Z) ASYN='NO'
            !
            CALL GENERIC_OPEN(FNAME, IOUT, Z,
     +             ACTION='WRITE', FORM='FORMATTED',
     +             ACCESS='SEQUENTIAL', STATUS='REPLACE', ASYNC=ASYN,
     +             BUFFER_BLOCKSIZE=BUFBLOCKSIZE, BUFFER_COUNT=BUFCOUNT)
            NFILE=NFILE+1
            CALL PRINT_MAIN_HEADER(IOUT)
            !
            CALL DATE%NOW()
            WRITE(IOUT,'( 1x 2A//)')
     +                            'OneWater Simulation Initiated at  ',
     +                             DATE%STR('  ')
            !
            WRITE(IOUT,'(1x A,/6X A)')'LIST FILE: '//TRIM(FNAME),
     +                                  'UNIT: '//NUM2STR(IOUT)
          END IF
          !
        ELSEIF(LINE(ITYP1:ITYP2).EQ.'BOYCE') THEN
          !
          WRITE(*,'(3(/A),/)')'BOYCE PACKAGE FOUND',
     +            ' WOW THANK YOU FOR THINKING I AM ',
     +            ' AWESOME ENOUGH TO HAVE A PACKAGE NAMED AFTER ME.'
          !
        ELSEIF(LINE(ITYP1:ITYP2).EQ.'WARN') THEN
          IF(CHK) CALL SUB_LST_VAR(LINE,LST,DIR,ITYP1,ITYP2,INUNIT,Z)
          ASYN='NO'
          BUFBLOCKSIZE = Z                                     !SET TO 1Z48576 WITH BUFFER KEYWORD
          BUFCOUNT = Z
          !FILTYP=LINE(ITYP1:ITYP2)
          CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
          READ(LINE(INAM1:INAM2), *, IOSTAT=IERR) NN
          IF (IERR==Z) THEN
             WARN_IU=NN
             CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
             ALLOCATE(WARN_FILE, SOURCE=LINE(INAM1:INAM2))
             UNIT_SPECIFIED=.TRUE.
             IF(IOUT==WARN_IU) THEN
            WRITE(*,12) WARN_FILE,IU
   12       FORMAT(1X,/1X,'WARN CANNOT OPEN ',A,' ON UNIT',I4,
     1              ' BECAUSE UNIT IS ALREADY BEING USED')
             END IF
          ELSE
             WARN_IU=Z
             ALLOCATE(WARN_FILE, SOURCE=LINE(INAM1:INAM2))
          END IF
          !
          FILSTAT = WARN_FILE
          CALL UPCASE(FILSTAT)
          !
          IF(FILSTAT .NE. 'NOPRINT') THEN
            I = -1
            CALL CHECK_FOR_POST_KEY(LLOC,LINE,INUNIT,Z,BUFBLOCKSIZE,
     +                              I,ASYN=ASYN, NOPRINT=NOWARN)   !NOLIST CHANGED TO FALSE IF NOPRINT IS NOT FOUND -- SET TO TRUE OUTSIDE OF LOOP
          END IF
          !
          IF(.NOT. NOWARN) THEN
            IF(BUFBLOCKSIZE.GT.Z) BUFCOUNT = 2
            !
            IF (ASYN=='YES' .AND. BUFBLOCKSIZE>Z) ASYN='NO'
            !
            CALL GENERIC_OPEN(WARN_FILE, WARN_IU, Z,
     +             ACTION='WRITE', FORM='FORMATTED',
     +             ACCESS='SEQUENTIAL', STATUS='REPLACE', ASYNC=ASYN,
     +             BUFFER_BLOCKSIZE=BUFBLOCKSIZE, BUFFER_COUNT=BUFCOUNT)
          END IF
        END IF
      END DO
      !
      IF(NOLIST) THEN
          WRITE(*,'(/ A /)')'NO LIST FILE SPECIFIED. NO LIST OUTPUT.'
          NFILE=1
          ISPLIT = -1
          BUFBLOCKSIZE = Z
          CALL GENERIC_NULL_FILE_OPEN(IOUT)
      END IF
      !
      IF(.NOT. NOWARN) THEN
            WRITE(IOUT,'(/1x A,/6X A)')  'WARN FILE: '//WARN_FILE,
     +                                  'UNIT: '//NUM2STR(WARN_IU)
            !
            CALL WARNING_MESSAGE(OUTPUT=WARN_IU, SET_UNIT=TRUE)
            CALL SET_GENERIC_OPEN_WARN_IU(WARN_IU)
      END IF
      !
      IF(NOLIST) FNAME = 'NOLIST.txt'
      !
      CALL LISTSPLIT%INIT(ISPLIT,IOUT,TRIM(FNAME),
     +                                      BUFBLOCKSIZE,MAXCOUNT=5)
      IF(ISPLIT>Z) THEN
        WRITE(IOUT,'(/4A)')
     +  'LIST WILL BE SPLIT TO A NEW FILE WHEN FILE SIZE IS ',
     +    'APPROXIMATELY ',NUM2STR(ISPLIT),' MB'
      END IF
C3----OPEN ALL DATA FILES
      CALL UTF8_BOM_OFFSET_REWIND(INUNIT)  ! REWIND(INUNIT)  
      NFILE = 1
      DO
        CALL READ_TO_DATA(LINE,INUNIT,IOUT,EOF=EOF)
        IF(EOF) EXIT
        LLOC=1
        CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,Z,INUNIT)
        FILTYP=LINE(ITYP1:ITYP2)
        !
        IF( FILTYP == 'DATA'          .OR.
     +      FILTYP == 'DATAGLO'       .OR.
     +      FILTYP == 'DATA(BINARY)'  .OR.
     +      FILTYP == 'DATAGLO(BINARY)' ) THEN
          !
          IF(NFILE==1) WRITE(IOUT,'(/A/)')
     +                      'OPENING DATA FILES [DATA AND DATA(BINARY)]'
          !
        IF( FILTYP == 'DATA'          .OR.
     +      FILTYP == 'DATAGLO'           ) THEN
                                                   FMTARG='FORMATTED'
                                                   ACCARG='SEQUENTIAL'
          ELSE
                                                   FMTARG=FORM          !BINARY FILE
                                                   ACCARG=ACCESS
          END IF
          !
          IF(CHK) CALL SUB_LST_VAR(LINE,LST,DIR,ITYP1,ITYP2,INUNIT,IOUT)
          !
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
          CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
          FNAME=LINE(INAM1:INAM2)
          !
          INQUIRE(UNIT=IU,OPENED=LOP)
          IF(LOP) CALL STOP_ERROR(LINE,INUNIT,IOUT,
     +    MSG=TRIM(FILTYP)//' UNIT '//NUM2STR(IU)//' IS ALREADY IN USE.'
     +    //NL//'PLEASE CHOOSE A DIFFERENT UNIT NUMBER.')
          !
          BUFBLOCKSIZE = 16384  !16KB x 2 = 32KB
          CALL CHECK_FOR_POST_KEY(LLOC,LINE,INUNIT,Z,BUFBLOCKSIZE,
     +                   ISPLIT,FILSTAT=FILSTAT,FILACT=FILACT,ASYN=ASYN)
          !
          IF(FILSTAT=='UNKNOWN' .OR. FILSTAT=='READ') ASYN='NO'
          IF( ASYN=='YES') THEN
            IF(.NOT.(BUFBLOCKSIZE<1 .OR. BUFBLOCKSIZE==16384)) ASYN='NO'  !ASYNC AND BUFFER REQUESTED...ONLY ALLOW BUFFER
          END IF
          IF( ASYN=='YES') BUFBLOCKSIZE = Z                               !ASYNC IS REQUESTED, TURN OFF BUFFER
          !
          IF(BUFBLOCKSIZE.GT.Z) THEN
                                    BUFCOUNT = 2
                        IF(FILSTAT=='UNKNOWN' .OR. FILSTAT=='READ') THEN
                                    BUFCOUNT = 1
                                    BUFBLOCKSIZE=BUFBLOCKSIZE*2
                        END IF
          ELSE
                                    BUFBLOCKSIZE = Z
                                    BUFCOUNT=Z
          END IF
          !
          CALL GENERIC_OPEN(FNAME, IU, IOUT, ACTION=FILACT, FORM=FMTARG,
     +             ACCESS=ACCARG, STATUS=FILSTAT, ASYNC=ASYN,
     +             BUFFER_BLOCKSIZE=BUFBLOCKSIZE, BUFFER_COUNT=BUFCOUNT)
          NFILE=NFILE+1
          !
          CALL DATAFILES%ADD(IU) !Note that this does not include UTM8 BOM check when TFR includes EXTERNAL unit REWIND or RELOAD for a unit declaired here 
          !
          WRITE(IOUT,'(A,I7,2A)') 'OPENED DATA FILE ON UNIT',IU,
     +                            ' WITH FILENAME OF: ',TRIM(FNAME)
          !
        END IF
        !
      END DO
      !
C3----OPEN ALL NULL FILES
      CALL UTF8_BOM_OFFSET_REWIND(INUNIT)  ! REWIND(INUNIT)  
      NFILE = 1
      DO
        CALL READ_TO_DATA(LINE,INUNIT,IOUT,EOF=EOF)
        IF(EOF) EXIT
        LLOC=1
        CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,Z,INUNIT)
        FILTYP=LINE(ITYP1:ITYP2)
        !
        IF( FILTYP == 'NULL'         .OR.
     +      FILTYP == 'NUL'          .OR.
     +      FILTYP == 'NULL(BINARY)' .OR.
     +      FILTYP == 'NUL(BINARY)' )THEN
          !
          IF(NFILE==1) WRITE(IOUT,'(/A/)')
     +                      'OPENING DATA FILES [DATA AND DATA(BINARY)]'
          !
          IF(CHK) CALL SUB_LST_VAR(LINE,LST,DIR,ITYP1,ITYP2,INUNIT,IOUT)
          !
          IF( FILTYP == 'NULL' .OR.
     +        FILTYP == 'NUL'      ) THEN
                                                   FMTARG='FORMATTED'
                                                   ACCARG='SEQUENTIAL'
          ELSE
                                                   FMTARG=FORM          !BINARY FILE
                                                   ACCARG=ACCESS
          END IF
          !
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
          !
         CALL GENERIC_NULL_FILE_OPEN(IU,LINE,INUNIT,IOUT,FMTARG,ACCARG)
          !
          NFILE=NFILE+1
          !
          CALL DATAFILES%ADD(IU)
          !
          WRITE(IOUT,'(A,I7)') 'OPENED NULL FILE ON UNIT',IU
          !
        END IF
        !
      END DO
      !
C2----OPEN PACKAGES
      CALL UTF8_BOM_OFFSET_REWIND(INUNIT)  ! REWIND(INUNIT)  
      WRITE(IOUT,'(/A/)') 'OPENING PACKAGES'
      DO
        CALL READ_TO_DATA(LINE,INUNIT,IOUT,EOF=EOF)
        IF(EOF) EXIT
        LLOC=1
        CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,Z,INUNIT)
        FILTYP=LINE(ITYP1:ITYP2)
        !
        SELECT CASE(FILTYP(:4))  !DROP OLD STYLE PACKAGE VERSION
        CASE("BCF6"); FILTYP = 'BCF'
        CASE("LMT6"); FILTYP = 'LMT'
        CASE("HFB6"); FILTYP = 'HFB'
        CASE("HUF2"); FILTYP = 'HUF'
        CASE("BFH2"); FILTYP = 'BFH'
        CASE("SWI2"); FILTYP = 'SWI'
        END SELECT
        !
        IF( ANY(FILTYP(:5).EQ.CUNIT) .OR. FILTYP(:3).EQ.'BAS') THEN
          FMTARG='FORMATTED'
          ACCARG='SEQUENTIAL'
          FILSTAT='OLD'
          FILACT='READ'
          !
          IF(CHK) CALL SUB_LST_VAR(LINE,LST,DIR,ITYP1,ITYP2,INUNIT,IOUT)
          !
          CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
          READ(LINE(INAM1:INAM2), *, IOSTAT=IERR) NN
          IF (IERR==Z) THEN
             IU=NN
             CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
             FNAME=LINE(INAM1:INAM2)
             UNIT_SPECIFIED=.TRUE.
             !
             INQUIRE(UNIT=IU,OPENED=LOP)
             IF(LOP) CALL STOP_ERROR(LINE,INUNIT,IOUT,
     +       MSG='PACKAGE '//TRIM(FILTYP)//' HAS A UNIT NUMBER '//
     +       NUM2STR(IU)//NL//'WHICH IS ALREADY IN USE.'//NL//
     +       'PLEASE CHOOSE A DIFFERENT UNIT NUMBER.')
          ELSE
             IU=Z
             FNAME=LINE(INAM1:INAM2)
          END IF
          !
          BUFBLOCKSIZE = 65536  !64KB x 2 = 128KB
         CALL CHECK_FOR_POST_KEY(LLOC,LINE,INUNIT,Z,BUFBLOCKSIZE,ISPLIT)
          !
          IF(BUFBLOCKSIZE.GT.Z) THEN
                                    BUFCOUNT = 1
                                    BUFBLOCKSIZE=BUFBLOCKSIZE*2
          ELSE
                                    BUFBLOCKSIZE = Z
                                    BUFCOUNT=Z
          END IF
          !
          CALL GENERIC_OPEN(FNAME, IU, IOUT, ACTION=FILACT, FORM=FMTARG,
     +             ACCESS=ACCARG, STATUS=FILSTAT,
     +             BUFFER_BLOCKSIZE=BUFBLOCKSIZE, BUFFER_COUNT=BUFCOUNT)   !64K READ BUFFER FOR PACKAGES
          !
          WHERE (FILTYP(:5).EQ.CUNIT) IUNIT=IU
          NFILE=NFILE+1
          !
          WRITE(IOUT,'(2A)') FILTYP(:4),' PACKAGE OPENED.'
          !
          IF (FILTYP(:3).EQ.'BAS') INBAS=IU
          !
        ELSEIF(FILTYP(ONE:ONE)=="%") THEN
          CYCLE  ! BY PASS ERROR
        ELSEIF( FILTYP .NE. 'LIST'            .AND.
     +          FILTYP .NE. 'WARN'            .AND.
     +          FILTYP .NE. 'DATA'            .AND.
     +          FILTYP .NE. 'DATA(BINARY)'    .AND.
     +          FILTYP .NE. 'DATAGLO'         .AND.
     +          FILTYP .NE. 'DATAGLO(BINARY)' .AND.
     +          FILTYP .NE. 'BOYCE'           .AND.
     +          FILTYP .NE. 'NULL'            .AND.
     +          FILTYP .NE. 'NUL'             .AND.
     +          FILTYP .NE. 'NULL(BINARY)'    .AND.
     +          FILTYP .NE. 'NUL(BINARY)'       ) THEN
      FNAME='NAME FILE ERROR: LOADING PACKAGES FROM NAME FILE FOUND '//
     +'A PACAKGE THAT IS NOT SUPPORTED OR KNOWN BY OneWater.'//NL//
     +'PACKAGE MAYBE MIS-SPELLED OR NOT CORRECLTLY SPECIFIED.'//BLN//
     +'THE UNKNOWN PACKAGE IS "'//TRIM(FILTYP)//'"'//BLN//
     +'THE FOLLOWING ARE THE CURRENTLY ACCEPTED/KNOWN PACKAGE NAMES '//
     +'IN OneWater:'//NL
          DO I=1,NIUNIT
                IF(CUNIT(I).NE.'') FNAME=TRIM(FNAME)//TRIM(CUNIT(I))//NL
          END DO
          CALL STOP_ERROR(LINE,INUNIT,IOUT,FNAME)
        END IF
      END DO
      !
      IF(UNIT_SPECIFIED) THEN
        WRITE(IOUT,'(/2A,/2A,/A/)') 'NOTIFICATION: A FILE UNIT ',
     +  'NUMBER WAS FOUND FOR EITHER THE LIST FILE OR A PACAKGE.',
     +  'IT WILL BE USED EVEN THOUGH UNIT NUMBERS FOR THE LIST FILE ',
     +  'AND PACKAGES ARE NOW OPTIONAL.',
     +  '[DATA and DATA(BINARY) STILL REQUIRE A UNIT NUMBER]'
      END IF
      !
      ! CHECK FOR DUBLICATE UNIT NUMBERS
      !
!      CALL ICHK%INIT()
!      DO I=1, NIUNIT-1
!          IF(IUNIT(I).NE.Z)THEN
!              IF(ANY(IUNIT(I)==IUNIT(I+1:NIUNIT)))
!     +         CALL ICHK%ADD_UNIQUE(IUNIT(I))
!          END IF
!      END DO
!      IF(ICHK%N > Z)
!     +     CALL STOP_ERROR(INFILE=INUNIT,OUTPUT=IOUT,MSG=
!     +    'FOUND DUPICATE UNIT NUMBERS SPECIFIED IN THE NAME FILE'//NL//
!     +    'THE FOLLOWING UNIT NUMBERS WERE FOUND MORE THAN ONCE: '//
!     +    NUM2STR(ICHK%INT,SEP=', '))
      !
      !!!!CLOSE(INUNIT)  !CLOSE NAME FILE
      CALL LST%DESTROY()
      CALL DIR%DESTROY()
      !
      ! CHECK IF UPW/NWT is in use VS LPF/Other
      !            LPF             UPW                  NWT
      IF    (IUNIT(23).NE.0.AND.IUNIT(62).EQ.0.AND.IUNIT(63).NE.0) THEN ! FLIP LPF TO UPW SINCE USE NWT SOLVER
        IUNIT(62) = IUNIT(23)
        IUNIT(23) = 0
      ELSEIF(IUNIT(23).EQ.0.AND.IUNIT(62).NE.0.AND.IUNIT(63).EQ.0)THEN ! FLIP UPW TO LPF SINCE NOT USING NWT SOLVER
        IUNIT(23) = IUNIT(62)
        IUNIT(62) = 0
      END IF
C
      END SUBROUTINE
      !
      SUBROUTINE SUB_LST_VAR(LINE,LST,DIR,ISTART,ISTOP,IN,IOUT)
      USE LINKED_LIST_INSTRUCTION, ONLY: CHARACTER_LINKED_LIST
      USE UTIL_INTERFACE,          ONLY: STOP_ERROR,
     +                                   WARNING_MESSAGE,
     +                                   COMMENT_INDEX,UPPER
      USE CONSTANTS,               ONLY: ONE, Z, NL, TAB, TRUE
      IMPLICIT NONE
      CHARACTER(*),                INTENT(INOUT):: LINE
      TYPE(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST,DIR
      INTEGER,                     INTENT(INOUT):: ISTART,ISTOP
      INTEGER,                     INTENT(IN   ):: IN,IOUT
      INTEGER:: I,EOL, POS
      !
      IF(LST%LEN() > Z) THEN
        EOL  = COMMENT_INDEX(LINE)
        ISTART = ONE
        I = ONE
        DO WHILE (ISTART > Z .AND. I < EOL)
          !
          ISTART = INDEX(LINE(I:EOL),"%")
          !
          IF(ISTART > Z) THEN
              !
              ISTOP = INDEX(LINE(ISTART+1:EOL),"%") + ISTART
              IF(ISTOP == Z) CALL STOP_ERROR(
     +          LINE,IN,IOUT, MSG='FOUND ON LINE A %, '//
     +          'BUT FAILED TO LOCATE A CORRESPONDING '//
     +          'CLOSING % (e.g. expect %XYZ%, but got %XYZ')
                !
              CALL UPPER(LINE(ISTART:ISTOP))
              POS = LST%FIND(LINE(ISTART:ISTOP))
              IF(POS > Z) THEN
                 IF     (ISTART==ONE) THEN
                   LINE = DIR%CHAR(POS)//LINE(ISTOP+1:)
                 ELSEIF (ISTOP == LEN(LINE)) THEN
                   LINE = LINE(:ISTART-1)//DIR%CHAR(POS)
                 ELSE
                   LINE = LINE(:ISTART-1)//DIR%CHAR(POS)//LINE(ISTOP+1:)
                 END IF
              ELSE
                  CALL WARNING_MESSAGE(LINE,IN,IOUT,MSG='FOUND '//
     +  'A PAIR OF PERCENTS % %, BUT FAILED TO LOCATE THE '//
     +  'VARIABLE NAME WITHIN THEM AS A DEFINED VARIABLE.'//NL//NL//
     +  'TO DEFINE A VARIABLE IT MUST BE THE FIRST PART OF A LINE'//NL//
     +  'AND BE FOLLOWED WITH THE TEXT IT WILL BE DEFINED WITH'//NL//
     +  'FOR EXAMPLE THE FOLLOWING DEFINES VARIABLE XYZ:'//NL//NL//
     +  '%XYZ%  VALUE_TO_STORE'//NL//NL//'   "'//LINE(ISTART:ISTOP)//
     +  '"   IS THE VARIABLE NAME THAT WAS FOUND ON'//
     +  'THE LINE.'//NL//'BUT IT WAS NOT MATCHED TO ANY OF THE '//
     +  'FOLLOWING PREVIOUSLY DEFINED VARIABLE NAMES:'//NL//NL//
     +  'VAR_NAME'//TAB//'VAR_VALUE'//NL//
     +  LST%TOLINE(NL,DIR,TAB)//NL//NL//'NOTE THAT OneWater '//
     +  'MAY SOON CRASH A RESULT OF NOT REPLACING THIS VARIABLE '//
     +  'WITH A VALUE.'//NL//NL , CMD_PRINT=TRUE)
              END IF
              !
              I = ISTOP + ONE
              !
          END IF
        END DO
      END IF
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7ARMZ(INZONE,INMULT)
C     ******************************************************************
C     ALLOCATE AND READ MULTIPLIER AND ZONE ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,IOUT!,XCORD,YCORD seb idea to bring in cords as MULT ARRAYs
      USE PARAMMODULE,ONLY:NZONAR,NMLTAR,ZONNAM,MLTNAM,IZON,RMLT,
     +                                                          MXNAMLEN
      USE UTIL_INTERFACE, ONLY: READ_TO_DATA, STOP_ERROR
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
C seb
      USE ExpressionParser  !ONLY PROVIDES ACCESS TO ExpEVAL AND MLTLOC
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
C
      CHARACTER*20 RW
      CHARACTER*1 COP
      CHARACTER*24 ANAME
      CHARACTER(MXNAMLEN):: CTMP1,CTMP2                                 !seb CHANGED CHAR LEN FROM 10
      CHARACTER*700 LINE
      INTEGER IOUTM,IOUTZ
      INTEGER:: MULTPRINT,ZONEPRINT                                     !seb TEMP VARIABLE USED AS FLAG FOR PRINTING OUT MULT ARRAYS THAT ARE READ IN
      CHARACTER(30)::MultFN,ZoneFN                                      !seb TEMP VARIABLE TO HOLD FILE NAME UNTIL GLOBAL VARIABLE IS USED
C     ------------------------------------------------------------------
crth  Modified to include exponentiation as an addtional binary operator designated by the '^' symbol in the input data file
crth  Note that only exponentiation as a binary operator is allowed since the order of operations is still assummed to occur from left to right
C
      MultFN='MULT_Arrays.txt'                                          !seb HARD WIRED NAME OF PRINTED MULT ARRAY FILE - CHANGE AS NEEDED
      MULTPRINT=0                                                       !DEFAULT IS NOT TO PRINT TO MultFN
      ZoneFN='ZONE_Arrays.txt'                                          !HARD WIRED NAME OF PRINTED ZONE ARRAY FILE - CHANGE AS NEEDED
      ZONEPRINT=0                                                       !DEFAULT IS NOT TO PRINT TO ZoneFN
C
C      IOUTM=1021
C      OPEN(UNIT=IOUTM,FILE='MULT_Arrays.out',STATUS='UNKNOWN')
C1------Read Number of Zone Arrays if Zone Option is active.
      NZONAR=0
      IF(INZONE.NE.0) THEN
         WRITE(IOUT,1) NUM2STR(INZONE)                    !seb CHANGED IOUTM TO IOUT FOR ENTIRE IF
    1    FORMAT(1X,/1X,'ZONE OPTION, INPUT READ FROM UNIT ',A)
         CALL READ_TO_DATA(LINE,INZONE,IOUT,IOUT)
         !CALL URDCOM(INZONE,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NZONAR,R,IOUT,INZONE)
         IF (NZONAR<0) THEN                                             !seb ADDED AUTOCOUNT FOR ZONE ARRAYS
           WRITE(IOUT,'(/A)') 'ZONE PACKAGE: "NZN < 0" '//
     +          'THE NUMBER OF ZONE ARRAYS WILL BE CALCULATED'
           ALLOCATE (IZON(NCOL,NROW,1))
           ANAME=' COUNTING ZONE ARRAYS'
           NZONAR=0
           DO WHILE (.TRUE.)
               CALL READ_TO_DATA(LINE,INZONE,IOUT)
               !CALL URDCOM(INZONE,0,LINE)
               IF(LINE=='') EXIT
               NZONAR=NZONAR+1
               CALL U2DINT(IZON(:,:,1),ANAME,NROW,NCOL,0,INZONE,-1)
           END DO
           DEALLOCATE(IZON)
           CALL UTF8_BOM_OFFSET_REWIND(INZONE)  ! REWIND(INZONE)
           CALL READ_TO_DATA(LINE,INZONE,IOUT,IOUT)
           !CALL URDCOM(INZONE,0,LINE)
           LLOC=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INZONE)
         END IF
         WRITE(IOUT,2) NUM2STR(NZONAR)
    2    FORMAT(1X,A,' ZONE ARRAYS')
         IF(NZONAR.LT.0) NZONAR=0
C seb READ IN FLAG TO PRINT OUT ZONE ARRAYS
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ZONEPRINT,R,0,INZONE)     !seb OPTIONAL READ IN OF PRINT FLAG >0 MEANS PRINT OUT
        IF (LINE(LEN(LINE):LEN(LINE)).EQ.'E') THEN
          WRITE(IOUT,'(/A,/A)') 'ZONEPRINT NOT SPECIFIED',
     +   'ZONE ARRAYS WILL NOT BE PRINTED TO EXTERNAL FILE'
          LLOC=ISTART-1
          ZONEPRINT=0
        END IF
        IF(ZONEPRINT>0)OPEN(NEWUNIT=IOUTZ,FILE=ZoneFN,STATUS='REPLACE') !'REPLACE' WILL OVER WRITE EXISTING FILES OF THE SAME NAME BETTER OPTION THAN UNKOWN SINCE IT COULD POTETIALLY HAVE LEFTE OVER TXT FROM AN EXISTING FILE
      END IF
C2------Allocate memory for zone arrays.  Allocate one array element if
C2------there are no zone arrays.
      IF(NZONAR.GT.0) THEN
        ALLOCATE (ZONNAM(NZONAR))
        ALLOCATE (IZON(NCOL,NROW,NZONAR))
      ELSE
        ALLOCATE (ZONNAM(1))
        ALLOCATE (IZON(1,1,1))
      ENDIF
C
C3------Read Number of Multiplier Arrays if Multiplier Option is active.
      NMLTAR=0
      IF(INMULT.NE.0) THEN
         WRITE(IOUT,11) NUM2STR(INMULT)                    !seb CHANGED IOUTM TO IOUT FOR ENTIRE IF
   11    FORMAT(1X,/1X,'MULTIPLIER OPTION, INPUT READ FROM UNIT ',A)
         CALL READ_TO_DATA(LINE,INMULT,IOUT,IOUT)
         !CALL URDCOM(INMULT,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMLTAR,R,IOUT,INMULT)
         IF (NMLTAR<0) THEN                                             !seb ADDED AUTOCOUNT FOR MULT ARRAYS
           WRITE(IOUT,'(/A)') 'MULT PACKAGE: "NML < 0" '//
     +          'THE NUMBER OF MULTIPLIER ARRAYS WILL BE CALCULATED'
           ALLOCATE(RMLT(NCOL,NROW,1))
           ANAME=' COUNTING MULT. ARRAYS'
           NMLTAR=0
           DO WHILE (.TRUE.)
               CALL READ_TO_DATA(LINE,INMULT,IOUT)
               !CALL URDCOM(INMULT,0,LINE)
               IF(LINE=='') EXIT
               NMLTAR=NMLTAR+1
               LLOC=1
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INMULT)
               IF(  LINE(ISTART:ISTOP).EQ.'FUNCTION' .OR.
     +              LINE(ISTART:ISTOP).EQ.'EXPRESSION' ) THEN
                    READ (INMULT,'(A)') LINE
               ELSE
                   CALL U2DDP(RMLT(:,:,1),ANAME,NROW,NCOL,0,INMULT,0)
               END IF
           END DO
           DEALLOCATE(RMLT)
           CALL UTF8_BOM_OFFSET_REWIND(INMULT)  ! REWIND(INMULT)
           CALL READ_TO_DATA(LINE,INMULT,IOUT)
           !CALL URDCOM(INMULT,0,LINE)
           LLOC=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
         END IF
         WRITE(IOUT,12) NUM2STR(NMLTAR)
   12    FORMAT(1X,A,' MULTIPLIER ARRAYS')
         IF(NMLTAR.LT.0) NMLTAR=0
      END IF
C seb READ IN FLAG TO PRINT OUT MULTIPLIER ARRAYS
      IF(NMLTAR.GT.0)THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MULTPRINT,R,0,INMULT)     !seb OPTIONAL READ IN OF PRINT FLAG >0 MEANS PRINT OUT
        IF (LINE(LEN(LINE):LEN(LINE)).EQ.'E') THEN
          WRITE(IOUT,'(/A,/A)') 'MULTPRINT NOT SPECIFIED',
     +   'MULT ARRAYS WILL NOT BE PRINTED TO EXTERNAL FILE'
          LLOC=ISTART-1
          MULTPRINT=0
        END IF
        IF(MULTPRINT>0)OPEN(NEWUNIT=IOUTM,FILE=MultFN,STATUS='REPLACE') !'REPLACE' WILL OVER WRITE EXISTING FILES OF THE SAME NAME BETTER OPTION THAN UNKOWN SINCE IT COULD POTETIALLY HAVE LEFTE OVER TXT FROM AN EXISTING FILE
      END IF
C
C4------Allocate memory for multiplier arrays.  Allocate one array element if
C4------there are no multiplier arrays.
      IF(NMLTAR.GT.0) THEN
        ALLOCATE (MLTNAM(NMLTAR))
        ALLOCATE (RMLT(NCOL,NROW,NMLTAR))
      ELSE
        ALLOCATE (MLTNAM(1))
        ALLOCATE (RMLT(1,1,1))
      ENDIF
C
C5------Initialize names of zones, multipliers, and parameters.
      IF(NZONAR.GT.0) THEN
        DO 10 I=1,NZONAR
        ZONNAM(I)=' '
10      CONTINUE
      END IF
      IF(NMLTAR.GT.0) THEN
        DO 20 I=1,NMLTAR
        MLTNAM(I)=' '
20      CONTINUE
      END IF
C
C6------Define the multiplier arrays.
      IF(NMLTAR.GT.0) THEN
        DO 2000 M=1,NMLTAR
C
C6A-----Read a line describing a multiplier array.
          !READ (INMULT,'(A)') LINE
          CALL READ_TO_DATA(LINE,INMULT,IOUT)
C
C6B-----Get the name of the new array
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
C
C6C-----Add new multiplier name into list.                              !seb ADDED CHECK FOR MULTNAM LENGTH
          IF (ISTART-ISTOP >= MXNAMLEN) THEN
             LINE=NEW_LINE(' ')//'MULT ERROR:  MLTNAM "'//
     +            LINE(ISTART:ISTOP)//'" EXCEEDS THE LIMIT OF '//
     +            NUM2STR(MXNAMLEN)//' CHARACTERS.'
             CALL STOP_ERROR('',INMULT,IOUT,MSG=LINE)
          END IF
          MLTNAM(M)=LINE(ISTART:ISTOP)
          !
          DO I=1, M-1                                                   !seb CHECK IF MLTNAM ALREADY IN USE
              IF (MLTNAM(I)==MLTNAM(M)) THEN
                 LINE='ERROR IN MULT PACKAGE MLTNAM.'// NEW_LINE(' ')//
     +                'THE FOLLOWING MLTNAM IS SPECIFIED TWICE: '//
     +                TRIM(MLTNAM(M))
                 CALL STOP_ERROR('',INMULT,IOUT,MSG=LINE)
              END IF
          END DO
          !
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INMULT)
C seb added check for flag: EXPRESSION
          IF(  LINE(ISTART:ISTOP).NE.'FUNCTION'
     +    .AND.LINE(ISTART:ISTOP).NE.'EXPRESSION' ) THEN
ccrth          WRITE(*,*)LINE(ISTART:ISTOP)
C
C6D-----Define array using array reader.
             ANAME=' MULT. ARRAY: '//MLTNAM(M)

             READ (INMULT,'(A)') LINE
             BACKSPACE(INMULT)
             CALL U2DDP(RMLT(:,:,M),ANAME,NROW,NCOL,0,INMULT,IOUT)      !seb changed from U2DREL
             IF(MULTPRINT>0) THEN
               WRITE(IOUTM,29) MLTNAM(M)                                !seb ADDED PRINT OPTION FOR REGULAR MULT ARRAYS
   29          FORMAT(1X,/1X,'MULTIPLIER ARRAY: ',A)
             END IF
C seb added flag for: EXPRESSION
          ELSE IF(LINE(ISTART:ISTOP).NE.'EXPRESSION' ) THEN
C
C6E-----Define array as aritmetic combination of other multiplier arrays.
C6E-----Start by initializing the array to 0.
             IF(MULTPRINT>0) WRITE(IOUTM,30) MLTNAM(M)                  !seb ADDED PRINTFLAG FOR FUNCTION MULT ARRAYS
   30        FORMAT(1X,/1X,'FUNCTION MULTIPLIER ARRAY: ',A)
             DO 40 I=1,NROW
             DO 40 J=1,NCOL
             RMLT(J,I,M)=0.D0
   40        CONTINUE
C
C6E1----Get the names of the multipliers and the operands.
             READ (INMULT,'(A)') LINE
             LLOC=1
             NOP=0
C
C6E2----Get the operator.
   45        IF(NOP.EQ.0) THEN
C
C6E2A---No operator is specified before the first operand -- define it to be " "
                COP=' '
             ELSE
C
C6E2B---Get the operator that precedes each operand after the first operand.
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
                IF(INDEX('+-*/^',LINE(ISTART:ISTOP))>0) THEN            ! exponentiation added as 5th binary operator -- rth
                   COP=LINE(ISTART:ISTOP)
                ELSE
                   GO TO 1000
                END IF
             END IF
             NOP=NOP+1
C
C6E3----Get the operand.
             CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
             IF(MULTPRINT>0) WRITE(IOUTM,47 ) COP,LINE(ISTART:ISTOP)
   47        FORMAT(1X,'                        ',A,' ARRAY ',A)
C
C6E4----Lookup the operand in the list of existing multipliers
               CTMP2=LINE(ISTART:ISTOP)
               CALL UPCASE(CTMP2)
             DO 50 MM=1,M
               CTMP1=MLTNAM(MM)
               CALL UPCASE(CTMP1)
               IF(CTMP1.EQ.CTMP2) GO TO 60
   50        CONTINUE
             CALL STOP_ERROR(LINE,INMULT,IOUT,MSG='ARRAY '//
     + 'OPERAND HAS NOT BEEN PREVIOUSLY DEFINED:'//LINE(ISTART:ISTOP))
C
C6E5----Apply the + operator.
   60        IF(COP.EQ.'+' .OR. COP.EQ.' ') THEN
                DO 100 I = 1, NROW
                DO 100 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)+ RMLT(J,I,MM)
  100           CONTINUE
             ELSE IF(COP.EQ.'-') THEN
                DO 200 I = 1, NROW
                DO 200 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)- RMLT(J,I,MM)
  200           CONTINUE
             ELSE IF(COP.EQ.'*') THEN
                DO 300 I = 1, NROW
                DO 300 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)* RMLT(J,I,MM)
  300           CONTINUE
             ELSE IF (COP.EQ.'/') THEN
                DO 400 I = 1, NROW
                DO 400 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)/ RMLT(J,I,MM)
  400           CONTINUE
crth
             ELSE IF(COP.EQ.'^') THEN
                DO 500 I = 1, NROW                                      !seb CONFUSED WITH WHAT IS GOING ON HERE WHY ARE YOU SKIPPING NEGATIVE POWERS
                DO 500 J = 1, NCOL
                if(RMLT(J,I,M).le.0)then
                 RMLT(J,I,M)=0.0
                 goto 500
                endif
                  RMLT(J,I,M) = ABS(RMLT(J,I,M)) ** RMLT(J,I,MM)
  500           CONTINUE
crth
             END IF
C
C6E6----Get the next operator.
             GO TO 45
C
C6E7-----Done defining the array.  Get the print code and print the array.
1000          IPRN=0                                                    !seb MAY NOT BE NEEDED ANYMORE WITH NEW PRINT CODE
              L=20-ISTOP+ISTART
              IF(L.GT.1)  THEN
                 RW=' '
                 RW(L:20)=LINE(ISTART:ISTOP)
                 READ(RW,'(I20)',ERR=1200) IPRN
              END IF
 1200         IF(IPRN.GE.0) THEN
                 ANAME=' MULT. ARRAY: '//MLTNAM(M)
                 CALL ULAPRWC(REAL(RMLT(:,:,M)),NCOL,NROW,0,IOUT,IPRN,  !seb CHANGED IOUTM TO IOUT
     1                 ANAME)
              END IF
              LINE=' '
C seb IF EXPRESSION IS READ IN, CALCULATE EXPRESSION
          ELSE  !LINE(ISTART:ISTOP).EQ.'EXPRESSION'
              READ (INMULT,'(A)') LINE
              RMLT(:,:,M)=ExpEVAL(LINE,MLTNAM(:M-1),RMLT(:,:,:M-1),     !MODULE ExpressionParser WHICH CALCULATES EXPRESSION FROM STRING
     +                                                    .TRUE.,.TRUE.)
              IF(MULTPRINT>0) THEN
                WRITE(IOUTM,1201) MLTNAM(M)
                WRITE(IOUTM,'(A)')TRIM(ADJUSTL(LINE))
 1201           FORMAT(1X,/1X,'EXPRESSION MULTIPLIER ARRAY: ',A)
              END IF
              LINE=' '
          END IF
          IF(MULTPRINT>0)THEN
           LLOC=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,0,0)
           IF (LINE(ISTART:ISTOP).EQ.'CONSTANT') THEN
           !IF (ALL(RMLT(1,1,M).EQ.RMLT(:,:,M)))THEN                       !seb ADDED PRINT OUT OF CONSTANT VALUE THIS CAN REPLACE THE PROCUDING LINES IF THERE IS A PROBLEM
!              WRITE(RW,'(G20.6)') RMLT(1,1,M)
              WRITE(IOUTM,'(2A)')'Scalar Multiplier: ',
     +                                              NUM2STR(RMLT(1,1,M))
           ELSE
            DO I=1,NROW
              WRITE(IOUTM,'(*(G15.6))') RMLT(:,I,M)
            END DO
           END IF
          END IF
 2000   CONTINUE
      ENDIF
C
C7------Read the zone array names and arrays
      IF(NZONAR.GT.0) THEN
         DO 3000 NZ=1,NZONAR
           !READ(INZONE,'(A)') ZONNAM(NZ)                               !seb CHANGED TO USE URWORD
           CALL READ_TO_DATA(LINE,INZONE,IOUT)
           LLOC=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,0,0)
           IF (ISTART-ISTOP >= MXNAMLEN) THEN
             CALL STOP_ERROR(LINE,INZONE,IOUT,MSG='ZONE: '//
     + 'ZONNAM "'//LINE(ISTART:ISTOP)//'" EXCEEDS 20 CHARACTERS.')
           END IF
           ZONNAM(NZ) = LINE(ISTART:ISTOP)
           !
           DO I=1, NZ-1                                                 !seb CHECK IF ZONNAM ALREADY IN USE
               IF (ZONNAM(I)==ZONNAM(NZ)) THEN
              CALL STOP_ERROR(LINE,INZONE,IOUT,MSG='ZONE: '//
     +  'ZONNAM "'//TRIM(ZONNAM(NZ))//'" IS SPECIFIED TWICE/NONUNIQUE.')
               END IF
           END DO
           !
           CALL U2DINT(IZON(:,:,NZ),'  ZONE ARRAY: '//ZONNAM(NZ),
     1              NROW,NCOL,0,INZONE,IOUT)                            !seb CHANGED IOUTM TO IOUT
           IF(ZONEPRINT>0) THEN
             ANAME='  ZONE ARRAY: '//ZONNAM(NZ)
             WRITE(IOUTZ,2999) ZONNAM(NZ)                               !seb ADDED PRINT OPTION FOR REGULAR MULT ARRAYS
 2999        FORMAT(1X,/1X,'ZONE ARRAY: ',A)
             DO I=1,NROW
               WRITE(IOUTZ,'(*(I7))') IZON(I,:,NZ)
             END DO
           END IF
 3000    CONTINUE
      END IF
      IF(MULTPRINT>0) CLOSE(IOUTM)                                      !seb CLOSE MULT AND ZONE PRINT FILES
      IF(ZONEPRINT>0) CLOSE(IOUTZ)
C
C8------Return.
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7ARPVAL(IUPVAL)
C     ******************************************************************
C     READ PARAMETER INPUT FILE
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT,IUNIT
      USE PARAMMODULE,ONLY:MXPAR,IPSUM,PARNAM,B,NPVAL,PROPPRINT,MXNAMLEN
      USE UTIL_INTERFACE, ONLY: READ_TO_DATA, STOP_ERROR
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
C
      CHARACTER(MXNAMLEN):: PNI, PNJ                                    !seb CHANGED FROM CHAR LEN OF 10
      CHARACTER*700 LINE
      CHARACTER,PARAMETER:: NL=ACHAR(10)
      LOGICAL:: END_OF_FILE
      CHARACTER(:),ALLOCATABLE:: ERR
C     ------------------------------------------------------------------
C
C1------CHECK TO SEE IF THE PARAMETER FILE WAS DECLARED IN THE NAME FILE.
      IU=IUNIT(IUPVAL)
      IF(IU.EQ.0) THEN
         NPVAL=0
         !PROPPRINT = ' '                                                !seb  SET TO BLANK BY DEFAULT
         RETURN
      END IF
C
C2------INITIALIZE VARIABLES
      ERR=''
      IERR = 0
      NPE = 0
C
C3------IDENTIFY PARAMETER VALUE OPTION.
        WRITE (IOUT,12) NUM2STR(IU)
   12 FORMAT (1X,/,1X,
     1  'PARAMETER VALUE INPUT FILE,  INPUT READ FROM UNIT ',A)
C
C4------READ & PRINT NUMBER OF PARAMETER VALUES.
      CALL READ_TO_DATA(LINE,IU,IOUT,IOUT)
      !CALL URDCOM(IU,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPVAL,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I    ,DUM,IOUT,IU)           !seb OPTIONAL READ IN OF PRINT FLAG - CAN BE MOVED TO ANOTHER PACKAGE IF NEEDED
      IF (LINE(ISTART:ISTOP).EQ.'PROPPRINT') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,I    ,DUM,IOUT,IU)
        DEALLOCATE(PROPPRINT)
        IF( LINE(ISTART:ISTOP)=='' ) THEN
            ALLOCATE(PROPPRINT, SOURCE='./')
        ELSE
          IF( LINE(ISTOP:ISTOP)=='/' .OR. LINE(ISTOP:ISTOP)=='\' ) THEN
              ALLOCATE(PROPPRINT, SOURCE=LINE(ISTART:ISTOP))
          ELSE
              ISTOP=ISTOP+1
              LINE(ISTOP:ISTOP)='/'
              ALLOCATE(PROPPRINT, SOURCE=LINE(ISTART:ISTOP))
          END IF
        END IF
      !ELSE
      !  PROPPRINT = ' '
      END IF
       WRITE (IOUT,14) NPVAL
       IF(PROPPRINT.NE.'') WRITE(IOUT,'(/A/,A/)')
     + 'PROPPRINT>0: PROPERTIES AFTER PARAMETERS HAVE BEEN APPLIED '//
     + 'WILL BE WRITEN TO SEPARATE FILES. THESE FILES WILL BE PLACED '//
     + 'IN:', '"'//PROPPRINT//'"'
   14 FORMAT (1X,/,1X,'NUMBER OF PARAMETER VALUES TO BE READ FROM',
     1               ' PARAMETER VALUE FILE:',I5)
      IF (NPVAL.LT.0) THEN
          WRITE (IOUT,16)
   16     FORMAT(1X,'NPVAL IN PARAMETER INPUT IS NEGATIVE, PVAL  ',
     1  'WILL AUTO-COUNT PARAMETERS LISTED IN FILE.',/,
     2  'ONLY BLANK LINES, COMMENTS AND PARAMETER NAMES ALLOWED.'   )
        NPVAL = -1  !NOTE THAT EOF WILL CAUSE ONE EXTRA READ OF LINE
        END_OF_FILE = .FALSE.
        DO WHILE (.NOT. END_OF_FILE)
            CALL READ_TO_DATA(LINE,IU,IOUT,EOF=END_OF_FILE)
            NPVAL = NPVAL + 1
        END DO
        CALL UTF8_BOM_OFFSET_REWIND(IU)  ! REWIND(IU)
        CALL READ_TO_DATA(LINE,IU,IOUT)
      ENDIF
      IPSUM=NPVAL
C
C5-----DEACTIVATE OPTION IF THERE ARE NO PARAMETERS IN FILE.
      IF(NPVAL.LE.0) THEN
           WRITE(IOUT,*) ' NPVAL in parameter file is 0,',
     1            ' so ignoring the parameter file'
        CLOSE(UNIT=IU)
        IU=0
        RETURN
      END IF
C
C6------STOP IF THERE ARE MORE THAN THE MAXIMUM NUMBER OF PARAMETERS.
      IF(NPVAL.GT.MXPAR) THEN                                           !seb CHANGED COMMENT TO BE MORE INFORMATIVE ABOUT NEW OPTION MAXPARAM
         LINE=NL//' PARAMETER FILE CONTAINS '//NUM2STR(NPVAL)//
     +        ' VALUES, BUT THE MAXIMUM NUMBER OF PARAMETERS IS '//
     +        NUM2STR(MXPAR)//
     +        NL//NL//
     +        'YOU CAN INCREASE STORAGE IN THE OPTIONS BLOCK OF'//
     +        ' THE BAS (BEGIN OPTIONS) WITH THE KEYWORD: '//
     +        '"MAXPARAM" FOLLOWED BY MXPAR, MXCLST, and MXINST '//
     +        '(as integers), INPUT IS AS FOLLOWS:'//NL//
     +         'MAXPARAM  MXPAR  MXCLST MXINST'//NL//NL//
     +   'TO BE SAFE YOU MAY WANT TO PICK A LARGE NUMBER, AS '//
     +   'THE LIST FILE WILL PRINT OUT THE MINIMUM SIZE REQUIRED'//NL//
     + 'JUST SEARCH FOR THE WORD MAXPARM IN THE LISTING FILE.'//NL//NL//
     + 'NOTE THAT YOU MUST HAVE " MXCLST > MXPAR + MXINST "'
         CALL STOP_ERROR(LINE,IUPVAL,IOUT,MSG=LINE)
      END IF
C
C7------WRITE A HEADING FOR THE LIST OF PARAMETERS.
        WRITE (IOUT,520)
  520 FORMAT (/,' INFORMATION ON PARAMETERS LISTED IN PARAMETER FILE',/,
     &             13X,'  VALUE IN',/,
     &   '    NAME     PARAMETER FILE',/,
     &   ' ----------  --------------')
C
C8-----READ AND WRITE PARAMETER NAMES AND VALUES.
      DO 70 I=1,NPVAL
        !READ(IU,*,ERR=80) PARNAM(I),B(I)                               !seb
        !READ(IU,'(A)')LINE
        CALL READ_TO_DATA(LINE,IU,IOUT)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,0,0)
        PARNAM(I) = LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,B(I),0,0)
        IF (LINE(LEN(LINE):LEN(LINE)).EQ.'E') GO TO 80                  !seb
        WRITE(IOUT,570) PARNAM(I),B(I)
  570   FORMAT(1X,A10,2X,G12.5)
C
C8A-----CHECK FOR DUPLICATE PARAMETER NAME FOR ALL BUT THE FIRST PARAMETER.
        IF (I.GT.1) THEN
          PNI=PARNAM(I)
          CALL UPCASE(PNI)
          IM1 = I-1
          DO 60 J=1,IM1
            PNJ=PARNAM(J)
            CALL UPCASE(PNJ)
            IF (PNI.EQ.PNJ) THEN
!                WRITE(IOUT,500) PARNAM(I)
!  500         FORMAT (' PARAMETER "',A10,
!     &        '" IS LISTED MORE THAN ONCE IN PARAMETER FILE',/,
!     &        ' -- STOP EXECUTION')
                ERR=ERR//PARNAM(I)//NEW_LINE(" ")
                IERR = 1
            ENDIF
   60     CONTINUE
        ENDIF
   70 CONTINUE
C
C9------WRITE A MESSAGE EXPLAINING THAT THE PARAMETER VALUES REPLACE THE
C9------VALUES FROM PACKAGE INPUT FILES..
        WRITE (IOUT,620)
  620 FORMAT(1X,77('-'))
        WRITE (IOUT,630)
  630 FORMAT(' FOR THE PARAMETERS LISTED IN THE TABLE ABOVE,',
     &       ' PARAMETER VALUES IN INDIVIDUAL',/,
     &       ' PACKAGE INPUT FILES ARE REPLACED BY THE VALUES FROM',
     &       ' THE PARAMETER INPUT FILE.')
C
C10-----STOP IF THERE WERE DUPLICATE NAMES.
      IF (IERR.GT.0) THEN
          ERR='THE FOLLOW PARAMETER NAMES ARE NONUNIQUE AND '//
     +        'LISTED MORE THAN ONCE:'//NEW_LINE(" ")//ERR
         CALL STOP_ERROR('',IUPVL,IOUT,MSG=ERR)
      ENDIF
C
C11-----CLOSE FILE AND RETURN.
      CLOSE(UNIT=IU)
      RETURN
C
C
  80   CALL STOP_ERROR(LINE,IUPVAL,IOUT,MSG='ERROR '//
     + 'ENCOUNTERED IN READING PARAMETER INPUT FILE')
C
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7STPVAL()
C     ******************************************************************
C     CHECK THAT PARAMETER DEFINITIONS ARE COMPLETE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT
      USE PARAMMODULE, ONLY:NPVAL,PARTYP,PARNAM
      USE UTIL_INTERFACE, ONLY: WARNING_MESSAGE
      CHARACTER(:),ALLOCATABLE::ERR
C     ------------------------------------------------------------------
      IF(NPVAL.LE.0) RETURN
      IERR=0
      ERR=''
C
Cx------CHECK THAT ALL PARAMETERS IN PARAMETER INPUT FILE HAVE BEEN DEFINED.
      DO 90 IP=1,NPVAL
        IF (PARTYP(IP).EQ.' ') THEN
          IERR = 1
          ERR=ERR//PARNAM(IP)//NEW_LINE(" ")
!            WRITE(IOUT,110) PARNAM(IP)
!  110     FORMAT(1X,/,1X,'PARAMETER "',A10,
!     1      '" IN PARAMETER INPUT FILE HAS NOT BEEN DEFINED',/,
!     2           ' -- STOP EXECUTION')
        ENDIF
   90 CONTINUE
C
      IF(IERR.NE.0) THEN
       ERR='PVAL OR PACKAGE PARAMETER NAMES WERE DECLAIRED,'//
     +                                       NEW_LINE(' ')//
     +  'BUT WERE NEVER APPLIED TO ANY PACKAGE PROPERTY '//
     +  '(ie NEVER ACTUALLY USED).'//NEW_LINE(' ')//
     +   'THE FOLLOWING IS A LIST OF PARAMETER NAMES THAT NEVER '//
     +   'WERE USED:'//NEW_LINE(' ')//ERR
        CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG=ERR)
        !CALL STOP_ERROR(OUTPUT=IOUT,MSG=ERR)
      END IF
C
Cx------RETURN.
      RETURN
      END SUBROUTINE
      SUBROUTINE GWF2BAS7DA(IGRID)
C  DEALLOCATE GLOBAL DATA
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
      USE CONSTANTS,           ONLY: inf_I
      USE GENERIC_INPUT_OUTPUT_DATAFILES,                               ! CLOSE ANY DATA FILES OPENED DURING THE COURSE OF SIMULATION
     +                         ONLY: CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES
      USE UTIL_INTERFACE,      ONLY: CLOSE_WARNING_UNIT
      USE EquationParser,      ONLY: REMOVE_EQUATION_ERROR_ROUTINES
C
        DO I=1,NIUNIT
            IF(IUNIT(I).NE.0) CLOSE(GLOBALDAT(IGRID)%IUNIT(I), IOSTAT=J)
        END DO
        !
        IF(GLOBALDAT(IGRID)%DATAFILES%N > 0) THEN  ! CLOSE ALL FILES ASSOCIATED WITH DATA FILES
            !
            DO I=1,   GLOBALDAT(IGRID)%DATAFILES%N
                CLOSE(GLOBALDAT(IGRID)%DATAFILES%VEC(I), IOSTAT=J)
            END DO
        END IF
        !
        CLOSE(GLOBALDAT(IGRID)%IOUT, IOSTAT=J)
        IF(IGRID.EQ.1) THEN                                 !THE FOLLOWING ARE INDEPENDENT OF LGR GRID
            DEALLOCATE(SUBLNK)
            DEALLOCATE(INPUT_CHECK)
            DEALLOCATE(BIN_REAL_KIND)
            CALL CLOSE_WARNING_UNIT()
            CALL REMOVE_EQUATION_ERROR_ROUTINES()
        END IF
        !DEALLOCATE(GLOBALDAT(IGRID)%CBC_GLOBAL)
        DEALLOCATE(GLOBALDAT(IGRID)%NOCBC)
        DEALLOCATE(GLOBALDAT(IGRID)%SPSTART)
        DEALLOCATE(GLOBALDAT(IGRID)%SPEND  )
        !DEALLOCATE(GLOBALDAT(IGRID)%LSTLVL)
        DEALLOCATE(GLOBALDAT(IGRID)%NCOL)
        DEALLOCATE(GLOBALDAT(IGRID)%NROW)
        DEALLOCATE(GLOBALDAT(IGRID)%NLAY)
        DEALLOCATE(GLOBALDAT(IGRID)%NPER)
        DEALLOCATE(GLOBALDAT(IGRID)%NBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%NCNFBD)
        DEALLOCATE(GLOBALDAT(IGRID)%ITMUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%LENUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%IXSEC)
        DEALLOCATE(GLOBALDAT(IGRID)%ITRSS)
        DEALLOCATE(GLOBALDAT(IGRID)%INBAS)
        DEALLOCATE(GLOBALDAT(IGRID)%IFREFM)
        DEALLOCATE(GLOBALDAT(IGRID)%NODES)
        DEALLOCATE(GLOBALDAT(IGRID)%IOUT)
        DEALLOCATE(GLOBALDAT(IGRID)%MXITER)
        DEALLOCATE(GLOBALDAT(IGRID)%IRESTART)
        DEALLOCATE(GLOBALDAT(IGRID)%KPERSTART)
        DEALLOCATE(GLOBALDAT(IGRID)%KSTPSTART)
C
        DEALLOCATE(GLOBALDAT(IGRID)%IUNIT)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYCBD)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYHDT)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYHDS)
        DEALLOCATE(GLOBALDAT(IGRID)%PERLEN)
        DEALLOCATE(GLOBALDAT(IGRID)%NSTP)
        DEALLOCATE(GLOBALDAT(IGRID)%TSMULT)
        DEALLOCATE(GLOBALDAT(IGRID)%ISSFLG)
        DEALLOCATE(GLOBALDAT(IGRID)%DELR)
        DEALLOCATE(GLOBALDAT(IGRID)%DELC)
        DEALLOCATE(GLOBALDAT(IGRID)%AREA)
        DEALLOCATE(GLOBALDAT(IGRID)%GSE)
        DEALLOCATE(GLOBALDAT(IGRID)%BOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%LBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%WTABLE)
        DEALLOCATE(GLOBALDAT(IGRID)%WTLAY)
        DEALLOCATE(GLOBALDAT(IGRID)%UPLAY)
        DEALLOCATE(GLOBALDAT(IGRID)%HNEW)
        DEALLOCATE(GLOBALDAT(IGRID)%HNEW_OLD)
        DEALLOCATE(GLOBALDAT(IGRID)%HOLD)
        DEALLOCATE(GLOBALDAT(IGRID)%IBOUND)
        DEALLOCATE(GLOBALDAT(IGRID)%CR)
        DEALLOCATE(GLOBALDAT(IGRID)%CC)
        DEALLOCATE(GLOBALDAT(IGRID)%CV)
        DEALLOCATE(GLOBALDAT(IGRID)%HCOF)
        DEALLOCATE(GLOBALDAT(IGRID)%RHS)
        DEALLOCATE(GLOBALDAT(IGRID)%BUFF)
        DEALLOCATE(GLOBALDAT(IGRID)%RBUF)
        DEALLOCATE(GLOBALDAT(IGRID)%STRT)
        IF(.NOT.ASSOCIATED(DDREF,STRT))
     1           DEALLOCATE(GLOBALDAT(IGRID)%DDREF)
        DEALLOCATE(GLOBALDAT(IGRID)%SPTIM)
        !
        ! GFORTRAN compiler error work-around for pointer data type FINAL statement
        DATAFILES=>GLOBALDAT(IGRID)%DATAFILES
        GLOBALDAT(IGRID)%DATAFILES=>NULL()
        DEALLOCATE(DATAFILES)
        DATAFILES=>NULL()
        !
        UPLAY_IDX=>GLOBALDAT(IGRID)%UPLAY_IDX
        GLOBALDAT(IGRID)%UPLAY_IDX=>NULL()
        DEALLOCATE(UPLAY_IDX)
        UPLAY_IDX=>NULL()
        !
        XYGRID=>GLOBALDAT(IGRID)%XYGRID
        GLOBALDAT(IGRID)%XYGRID=>NULL()
        DEALLOCATE(XYGRID)
        XYGRID=>NULL()
        !
        !DEALLOCATE(GLOBALDAT(IGRID)%DATAFILES)  -- Gfortran has problems with FINAL statement, so just deallocate mannualy
        !DEALLOCATE(GLOBALDAT(IGRID)%UPLAY_IDX)
        !DEALLOCATE(GLOBALDAT(IGRID)%XYGRID)
C
        DEALLOCATE (PARAMDAT(IGRID)%ICLSUM)
        DEALLOCATE (PARAMDAT(IGRID)%IPSUM)
        DEALLOCATE (PARAMDAT(IGRID)%INAMLOC)
        DEALLOCATE (PARAMDAT(IGRID)%NMLTAR)
        DEALLOCATE (PARAMDAT(IGRID)%NZONAR)
        DEALLOCATE (PARAMDAT(IGRID)%NPVAL)
        DEALLOCATE (PARAMDAT(IGRID)%PROPPRINT)                          !seb added PROPPRINT
        DEALLOCATE (PARAMDAT(IGRID)%B)
        DEALLOCATE (PARAMDAT(IGRID)%IACTIVE)
        DEALLOCATE (PARAMDAT(IGRID)%IPLOC)
        DEALLOCATE (PARAMDAT(IGRID)%IPCLST)
        DEALLOCATE (PARAMDAT(IGRID)%PARNAM)
        DEALLOCATE (PARAMDAT(IGRID)%PARTYP)
        DEALLOCATE (PARAMDAT(IGRID)%ZONNAM)
        DEALLOCATE (PARAMDAT(IGRID)%MLTNAM)
        DEALLOCATE (PARAMDAT(IGRID)%INAME)
        DEALLOCATE (PARAMDAT(IGRID)%RMLT)
        DEALLOCATE (PARAMDAT(IGRID)%IZON)
C
        DEALLOCATE(GWFBASDAT(IGRID)%MSUM)
        DEALLOCATE(GWFBASDAT(IGRID)%IHEDFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IHEDUN)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDNFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDNUN)
        DEALLOCATE(GWFBASDAT(IGRID)%IBOUUN)
        DEALLOCATE(GWFBASDAT(IGRID)%LBHDSV)
        DEALLOCATE(GWFBASDAT(IGRID)%LBDDSV)
        DEALLOCATE(GWFBASDAT(IGRID)%LBBOSV)
        DEALLOCATE(GWFBASDAT(IGRID)%IBUDFL)
        DEALLOCATE(GWFBASDAT(IGRID)%ICBCFL)
        DEALLOCATE(GWFBASDAT(IGRID)%IHDDFL)
        DEALLOCATE(GWFBASDAT(IGRID)%IAUXSV)
        DEALLOCATE(GWFBASDAT(IGRID)%IBDOPT)
        DEALLOCATE(GWFBASDAT(IGRID)%IPRTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%IPEROC)
        DEALLOCATE(GWFBASDAT(IGRID)%ITSOC)
        DEALLOCATE(GWFBASDAT(IGRID)%ICHFLG)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDREF)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDREFNEW)
        DEALLOCATE(GWFBASDAT(IGRID)%DELT)
        DEALLOCATE(GWFBASDAT(IGRID)%PERTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%TOTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%HNOFLO)
        DEALLOCATE(GWFBASDAT(IGRID)%HDRY)
        DEALLOCATE(GWFBASDAT(IGRID)%STOPER)
        DEALLOCATE(GWFBASDAT(IGRID)%CHEDFM)
        DEALLOCATE(GWFBASDAT(IGRID)%CDDNFM)
        DEALLOCATE(GWFBASDAT(IGRID)%CBOUFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IUBGT)
C
        DEALLOCATE(GWFBASDAT(IGRID)%IOFLG)
        DEALLOCATE(GWFBASDAT(IGRID)%VBVL)
        DEALLOCATE(GWFBASDAT(IGRID)%VBNM)
C
        DEALLOCATE(GWFBASDAT(IGRID)%PDIFFPRT)
        DEALLOCATE(GWFBASDAT(IGRID)%REALTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%SIMTIM_PER)
        DEALLOCATE(GWFBASDAT(IGRID)%REALTIM_PER)
        DEALLOCATE(GWFBASDAT(IGRID)%TOTPERTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%USE_LEAP_YR)
        DEALLOCATE(GWFBASDAT(IGRID)%DEALLOCATE_MULT)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%OSCIL_DMP_OUTER,STAT=I)
        DEALLOCATE(GWFBASDAT(IGRID)%OSCIL_DMP_LRC  ,STAT=I)
        DEALLOCATE(GWFBASDAT(IGRID)%OSCIL_DMP_DIF  ,STAT=I)
        !
        IF(GWFBASDAT(IGRID)%PRNT_CNVG%IS_OPEN) THEN
           DEALLOCATE(GWFBASDAT(IGRID)%PRNT_CNVG_LRC,STAT=I)
           DEALLOCATE(GWFBASDAT(IGRID)%PRNT_CNVG_DIF,STAT=I)
        END IF
        DEALLOCATE(GWFBASDAT(IGRID)%PRNT_CNVG_OUTER)
        DEALLOCATE(GWFBASDAT(IGRID)%PRNT_CNVG_NTERM)
        !
        IF(GWFBASDAT(IGRID)%PRNT_FRES%IS_OPEN) THEN
           DEALLOCATE(GWFBASDAT(IGRID)%PRNT_FRES_LRC,STAT=I)
           DEALLOCATE(GWFBASDAT(IGRID)%PRNT_FRES_DIF,STAT=I)
        END IF
        DEALLOCATE(GWFBASDAT(IGRID)%PRNT_FRES_OUTER)
        DEALLOCATE(GWFBASDAT(IGRID)%PRNT_FRES_NTERM)
        !
        IF(GWFBASDAT(IGRID)%PRNT_VERR%IS_OPEN) THEN
           DEALLOCATE(GWFBASDAT(IGRID)%PRNT_VERR_LRC,STAT=I)
           DEALLOCATE(GWFBASDAT(IGRID)%PRNT_VERR_DIF,STAT=I)
        END IF
        DEALLOCATE(GWFBASDAT(IGRID)%PRNT_VERR_OUTER)
        DEALLOCATE(GWFBASDAT(IGRID)%PRNT_VERR_NTERM)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%DAMPEN_START    )
        DEALLOCATE(GWFBASDAT(IGRID)%DAMPEN_START_ITR)
        DEALLOCATE(GWFBASDAT(IGRID)%DAMPEN_START_DMP)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%MIN_SOLVER_INTER)
        DEALLOCATE(GWFBASDAT(IGRID)%MIN_SOLVER_INTER_SP )
        DEALLOCATE(GWFBASDAT(IGRID)%MIN_SOLVER_INTER_NEW)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%MAX_REL_VOL_ERROR)
        DEALLOCATE(GWFBASDAT(IGRID)%MAX_REL_VOL_INVOKED)
        !
        IF(GWFBASDAT(IGRID)%BAS_ADAMP < inf_I) THEN
            DEALLOCATE(GWFBASDAT(IGRID)%HED_CHNG2,STAT=I)
            DEALLOCATE(GWFBASDAT(IGRID)%HED_CHNG3,STAT=I)
            DEALLOCATE(GWFBASDAT(IGRID)%HED_LOCK, STAT=I)
        END IF
        DEALLOCATE(GWFBASDAT(IGRID)%BAS_ADAMP)
        DEALLOCATE(GWFBASDAT(IGRID)%BAS_ADAMP_TOL)
        DEALLOCATE(GWFBASDAT(IGRID)%BAS_ADAMP_TOL2)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%HAS_STARTDATE)
        DO I=1, UBOUND(GWFBASDAT(IGRID)%DATE_SP,1)
            DEALLOCATE(GWFBASDAT(IGRID)%DATE_SP(I)%TS)
        END DO
        DEALLOCATE(GWFBASDAT(IGRID)%DATE_SP)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%PVOL_ERR)
        !
        DEALLOCATE(GWFBASDAT(IGRID)%ABOVE_GSE_LIM    )
        DEALLOCATE(GWFBASDAT(IGRID)%ABOVE_GSE_PRT_LIM)
        !
        ! Gfortran has problems with FINAL statement if not current pointer
        LISTSPLIT=>GWFBASDAT(IGRID)%LISTSPLIT
        GWFBASDAT(IGRID)%LISTSPLIT=>NULL()
        DEALLOCATE(LISTSPLIT)
        LISTSPLIT=>NULL()
        !
        BUDGETDB=>GWFBASDAT(IGRID)%BUDGETDB
        GWFBASDAT(IGRID)%BUDGETDB=>NULL()
        DEALLOCATE(BUDGETDB)
        BUDGETDB=>NULL()
        !
        INTER_INFO=>GWFBASDAT(IGRID)%INTER_INFO
        GWFBASDAT(IGRID)%INTER_INFO=>NULL()
        DEALLOCATE(INTER_INFO)
        INTER_INFO=>NULL()
        !
        PRNT_RES=>GWFBASDAT(IGRID)%PRNT_RES
        GWFBASDAT(IGRID)%PRNT_RES=>NULL()
        DEALLOCATE(PRNT_RES)
        PRNT_RES=>NULL()
        !
        PRNT_RES_CUM=>GWFBASDAT(IGRID)%PRNT_RES_CUM
        GWFBASDAT(IGRID)%PRNT_RES_CUM=>NULL()
        DEALLOCATE(PRNT_RES_CUM)
        PRNT_RES_CUM=>NULL()
        !
        PRNT_CNVG=>GWFBASDAT(IGRID)%PRNT_CNVG
        GWFBASDAT(IGRID)%PRNT_CNVG=>NULL()
        DEALLOCATE(PRNT_CNVG)
        PRNT_CNVG=>NULL()
        !
        PRNT_FRES=>GWFBASDAT(IGRID)%PRNT_FRES
        GWFBASDAT(IGRID)%PRNT_FRES=>NULL()
        DEALLOCATE(PRNT_FRES)
        PRNT_FRES=>NULL()
        !
        PRNT_VERR=>GWFBASDAT(IGRID)%PRNT_VERR
        GWFBASDAT(IGRID)%PRNT_VERR=>NULL()
        DEALLOCATE(PRNT_VERR)
        PRNT_VERR=>NULL()
        !
        ABOVE_GSE_PRT=>GWFBASDAT(IGRID)%ABOVE_GSE_PRT
        GWFBASDAT(IGRID)%ABOVE_GSE_PRT=>NULL()
        DEALLOCATE(ABOVE_GSE_PRT)
        ABOVE_GSE_PRT=>NULL()
        !
        MIN_ITER_INPUT=>GWFBASDAT(IGRID)%MIN_ITER_INPUT
        GWFBASDAT(IGRID)%MIN_ITER_INPUT=>NULL()
        DEALLOCATE(MIN_ITER_INPUT)
        MIN_ITER_INPUT=>NULL()
        !
        ADAMP_INPUT=>GWFBASDAT(IGRID)%ADAMP_INPUT
        GWFBASDAT(IGRID)%ADAMP_INPUT=>NULL()
        DEALLOCATE(ADAMP_INPUT)
        ADAMP_INPUT=>NULL()
        !DEALLOCATE(GWFBASDAT(IGRID)%LISTSPLIT)
        !DEALLOCATE(GWFBASDAT(IGRID)%BUDGETDB)                           !seb
        !DEALLOCATE(GWFBASDAT(IGRID)%INTER_INFO)
        !DEALLOCATE(GWFBASDAT(IGRID)%PRNT_CNVG      )
        !DEALLOCATE(GWFBASDAT(IGRID)%PRNT_FRES      )
        !DEALLOCATE(GWFBASDAT(IGRID)%PRNT_VERR      )
        !DEALLOCATE(GWFBASDAT(IGRID)%MIN_ITER_INPUT      )
        !DEALLOCATE(GWFBASDAT(IGRID)%ADAMP_INPUT)
        !DEALLOCATE(GWFBASDAT(IGRID)%ABOVE_GSE_PRT    )


C
C NULLIFY THE LOCAL POITNERS
      IF(IGRID.EQ.1)THEN
        !
        CALL CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES()
        !
        !CBC_GLOBAL=>NULL()
        NOCBC  =>NULL()
        SPSTART=>NULL()
        SPEND  =>NULL()
        !LSTLVL=>NULL()
        NCOL=>NULL()
        NROW=>NULL()
        NLAY=>NULL()
        NPER=>NULL()
        NBOTM=>NULL()
        NCNFBD=>NULL()
        ITMUNI=>NULL()
        LENUNI=>NULL()
        IXSEC=>NULL()
        ITRSS=>NULL()
        INBAS=>NULL()
        IFREFM=>NULL()
        NODES=>NULL()
        IOUT=>NULL()
        MXITER=>NULL()
        IRESTART=>NULL()
        KPERSTART=>NULL()
        KSTPSTART=>NULL()
C
        IUNIT=>NULL()
        LAYCBD=>NULL()
        LAYHDT=>NULL()
        LAYHDS=>NULL()
        PERLEN=>NULL()
        NSTP=>NULL()
        TSMULT=>NULL()
        ISSFLG=>NULL()
        DELR=>NULL()
        DELC=>NULL()
        AREA=>NULL()
        GSE=>NULL()
        BOTM=>NULL()
        LBOTM=>NULL()
        WTABLE=>NULL()
        UPLAY=>NULL()
        WTLAY=>NULL()
        UPLAY_IDX=>NULL()
        HNEW=>NULL()
        HNEW_OLD=>NULL()
        HOLD=>NULL()
        IBOUND=>NULL()
        CR=>NULL()
        CC=>NULL()
        CV=>NULL()
        HCOF=>NULL()
        RHS=>NULL()
        BUFF=>NULL()
        RBUF=>NULL()
        STRT=>NULL()

        DDREF=>NULL()
        SPTIM=>NULL()
        DATAFILES=>NULL()
C
        ICLSUM=>NULL()
        IPSUM=>NULL()
        INAMLOC=>NULL()
        NMLTAR=>NULL()
        NZONAR=>NULL()
        NPVAL=>NULL()
        PROPPRINT=>NULL()                          !seb added PROPPRINT
        B=>NULL()
        IACTIVE=>NULL()
        IPLOC=>NULL()
        IPCLST=>NULL()
        PARNAM=>NULL()
        PARTYP=>NULL()
        ZONNAM=>NULL()
        MLTNAM=>NULL()
        INAME=>NULL()
        RMLT=>NULL()
        IZON=>NULL()
C
        MSUM=>NULL()
        IHEDFM=>NULL()
        IHEDUN=>NULL()
        IDDNFM=>NULL()
        IDDNUN=>NULL()
        IBOUUN=>NULL()
        LBHDSV=>NULL()
        LBDDSV=>NULL()
        LBBOSV=>NULL()
        IBUDFL=>NULL()
        ICBCFL=>NULL()
        IHDDFL=>NULL()
        IAUXSV=>NULL()
        IBDOPT=>NULL()
        IPRTIM=>NULL()
        IPEROC=>NULL()
        ITSOC=>NULL()
        ICHFLG=>NULL()
        IDDREF=>NULL()
        IDDREFNEW=>NULL()
        DELT=>NULL()
        PERTIM=>NULL()
        TOTIM=>NULL()
        HNOFLO=>NULL()
        HDRY=>NULL()
        STOPER=>NULL()
        CHEDFM=>NULL()
        CDDNFM=>NULL()
        CBOUFM=>NULL()
        IUBGT=>NULL()
C
        IOFLG=>NULL()
        VBVL=>NULL()
        VBNM=>NULL()
C
        XYGRID=>NULL()
        PDIFFPRT=>NULL()
        REALTIM=>NULL()
        SIMTIM_PER=>NULL()
        REALTIM_PER=>NULL()
        TOTPERTIM=>NULL()
        USE_LEAP_YR=>NULL()
        LISTSPLIT=>NULL()
        BUDGETDB =>NULL()
        INTER_INFO=>NULL()
        PRNT_RES  =>NULL()
        PRNT_RES_LIM     =>NULL()
        PRNT_RES_CUM     =>NULL()
        PRNT_RES_CUM_ARR =>NULL()
        DEALLOCATE_MULT=>NULL()
        MAX_REL_VOL_ERROR=>NULL()
        MAX_REL_VOL_INVOKED=>NULL()
        MIN_SOLVER_INTER     =>NULL()
        MIN_ITER_INPUT       => NULL()
        MIN_SOLVER_INTER_SP  => NULL()
        MIN_SOLVER_INTER_NEW => NULL()
        BAS_ADAMP      =>NULL()
        BAS_ADAMP_TOL  =>NULL()
        BAS_ADAMP_TOL2 =>NULL()
        ADAMP_INPUT    =>NULL()
        HED_CHNG2      =>NULL()
        HED_CHNG3      =>NULL()
        HED_LOCK       =>NULL()
        DATE_SP        =>NULL()
        HAS_STARTDATE  =>NULL()
        !
        OSCIL_DMP_OUTER => NULL()
        OSCIL_DMP_LRC   => NULL()
        OSCIL_DMP_DIF   => NULL()
        !
        PRNT_CNVG_OUTER => NULL()
        PRNT_CNVG_NTERM => NULL()
        PRNT_CNVG       => NULL()
        PRNT_CNVG_LRC   => NULL()
        PRNT_CNVG_DIF   => NULL()
        !
        PRNT_FRES_OUTER => NULL()
        PRNT_FRES_NTERM => NULL()
        PRNT_FRES       => NULL()
        PRNT_FRES_LRC   => NULL()
        PRNT_FRES_DIF   => NULL()
        !
        PRNT_VERR_OUTER => NULL()
        PRNT_VERR_NTERM => NULL()
        PRNT_VERR       => NULL()
        PRNT_VERR_LRC   => NULL()
        PRNT_VERR_DIF   => NULL()
        !
        DAMPEN_START    => NULL()
        DAMPEN_START_ITR=> NULL()
        DAMPEN_START_DMP=> NULL()
        !
        PVOL_ERR        => NULL()
        !
        ABOVE_GSE_LIM    =>NULL()
        ABOVE_GSE_PRT_LIM=>NULL()
        ABOVE_GSE_PRT    =>NULL()
      END IF
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7PNT(IGRID)
C  Change global data to a different grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        NCOL=>GLOBALDAT(IGRID)%NCOL
        NROW=>GLOBALDAT(IGRID)%NROW
        NLAY=>GLOBALDAT(IGRID)%NLAY
        NPER=>GLOBALDAT(IGRID)%NPER
        NBOTM=>GLOBALDAT(IGRID)%NBOTM
        NCNFBD=>GLOBALDAT(IGRID)%NCNFBD
        ITMUNI=>GLOBALDAT(IGRID)%ITMUNI
        LENUNI=>GLOBALDAT(IGRID)%LENUNI
        IXSEC=>GLOBALDAT(IGRID)%IXSEC
        ITRSS=>GLOBALDAT(IGRID)%ITRSS
        INBAS=>GLOBALDAT(IGRID)%INBAS
        IFREFM=>GLOBALDAT(IGRID)%IFREFM
        NODES=>GLOBALDAT(IGRID)%NODES
        IOUT=>GLOBALDAT(IGRID)%IOUT
        MXITER=>GLOBALDAT(IGRID)%MXITER
        IRESTART=>GLOBALDAT(IGRID)%IRESTART
        KPERSTART=>GLOBALDAT(IGRID)%KPERSTART
        KSTPSTART=>GLOBALDAT(IGRID)%KSTPSTART
C
        IUNIT=>GLOBALDAT(IGRID)%IUNIT
        LAYCBD=>GLOBALDAT(IGRID)%LAYCBD
        LAYHDT=>GLOBALDAT(IGRID)%LAYHDT
        LAYHDS=>GLOBALDAT(IGRID)%LAYHDS
        PERLEN=>GLOBALDAT(IGRID)%PERLEN
        NSTP=>GLOBALDAT(IGRID)%NSTP
        TSMULT=>GLOBALDAT(IGRID)%TSMULT
        ISSFLG=>GLOBALDAT(IGRID)%ISSFLG
        DELR=>GLOBALDAT(IGRID)%DELR
        DELC=>GLOBALDAT(IGRID)%DELC
        AREA=>GLOBALDAT(IGRID)%AREA
        GSE=>GLOBALDAT(IGRID)%GSE
        BOTM=>GLOBALDAT(IGRID)%BOTM
        LBOTM=>GLOBALDAT(IGRID)%LBOTM
        WTABLE=>GLOBALDAT(IGRID)%WTABLE
        WTLAY=>GLOBALDAT(IGRID)%WTLAY
        UPLAY=>GLOBALDAT(IGRID)%UPLAY
        UPLAY_IDX=>GLOBALDAT(IGRID)%UPLAY_IDX
        HNEW=>GLOBALDAT(IGRID)%HNEW
        HNEW_OLD=>GLOBALDAT(IGRID)%HNEW_OLD
        HOLD=>GLOBALDAT(IGRID)%HOLD
        IBOUND=>GLOBALDAT(IGRID)%IBOUND
        !WETCEL=>GLOBALDAT(IGRID)%WETCEL
        CR=>GLOBALDAT(IGRID)%CR
        CC=>GLOBALDAT(IGRID)%CC
        CV=>GLOBALDAT(IGRID)%CV
        HCOF=>GLOBALDAT(IGRID)%HCOF
        RHS=>GLOBALDAT(IGRID)%RHS
        BUFF=>GLOBALDAT(IGRID)%BUFF
        RBUF=>GLOBALDAT(IGRID)%RBUF
        STRT=>GLOBALDAT(IGRID)%STRT
        DDREF=>GLOBALDAT(IGRID)%DDREF
        SPTIM=>GLOBALDAT(IGRID)%SPTIM
        DATAFILES=>GLOBALDAT(IGRID)%DATAFILES
C
        ICLSUM=>PARAMDAT(IGRID)%ICLSUM
        IPSUM=>PARAMDAT(IGRID)%IPSUM
        INAMLOC=>PARAMDAT(IGRID)%INAMLOC
        NMLTAR=>PARAMDAT(IGRID)%NMLTAR
        NZONAR=>PARAMDAT(IGRID)%NZONAR
        NPVAL=>PARAMDAT(IGRID)%NPVAL
        PROPPRINT=>PARAMDAT(IGRID)%PROPPRINT                            !seb
C
        B=>PARAMDAT(IGRID)%B
        IACTIVE=>PARAMDAT(IGRID)%IACTIVE
        IPLOC=>PARAMDAT(IGRID)%IPLOC
        IPCLST=>PARAMDAT(IGRID)%IPCLST
        IZON=>PARAMDAT(IGRID)%IZON
        RMLT=>PARAMDAT(IGRID)%RMLT
        PARNAM=>PARAMDAT(IGRID)%PARNAM
        PARTYP=>PARAMDAT(IGRID)%PARTYP
        ZONNAM=>PARAMDAT(IGRID)%ZONNAM
        MLTNAM=>PARAMDAT(IGRID)%MLTNAM
        INAME=>PARAMDAT(IGRID)%INAME
C
        MSUM=>GWFBASDAT(IGRID)%MSUM
        IHEDFM=>GWFBASDAT(IGRID)%IHEDFM
        IHEDUN=>GWFBASDAT(IGRID)%IHEDUN
        IDDNFM=>GWFBASDAT(IGRID)%IDDNFM
        IDDNUN=>GWFBASDAT(IGRID)%IDDNUN
        IBOUUN=>GWFBASDAT(IGRID)%IBOUUN
        LBHDSV=>GWFBASDAT(IGRID)%LBHDSV
        LBDDSV=>GWFBASDAT(IGRID)%LBDDSV
        LBBOSV=>GWFBASDAT(IGRID)%LBBOSV
        IBUDFL=>GWFBASDAT(IGRID)%IBUDFL
        ICBCFL=>GWFBASDAT(IGRID)%ICBCFL
        IHDDFL=>GWFBASDAT(IGRID)%IHDDFL
        IAUXSV=>GWFBASDAT(IGRID)%IAUXSV
        IBDOPT=>GWFBASDAT(IGRID)%IBDOPT
        IPRTIM=>GWFBASDAT(IGRID)%IPRTIM
        IPEROC=>GWFBASDAT(IGRID)%IPEROC
        ITSOC=>GWFBASDAT(IGRID)%ITSOC
        ICHFLG=>GWFBASDAT(IGRID)%ICHFLG
        IDDREF=>GWFBASDAT(IGRID)%IDDREF
        IDDREFNEW=>GWFBASDAT(IGRID)%IDDREFNEW
        DELT=>GWFBASDAT(IGRID)%DELT
        PERTIM=>GWFBASDAT(IGRID)%PERTIM
        TOTIM=>GWFBASDAT(IGRID)%TOTIM
        HNOFLO=>GWFBASDAT(IGRID)%HNOFLO
        HDRY=>GWFBASDAT(IGRID)%HDRY
        STOPER=>GWFBASDAT(IGRID)%STOPER
        CHEDFM=>GWFBASDAT(IGRID)%CHEDFM
        CDDNFM=>GWFBASDAT(IGRID)%CDDNFM
        CBOUFM=>GWFBASDAT(IGRID)%CBOUFM
        IUBGT=>GWFBASDAT(IGRID)%IUBGT

C
        IOFLG=>GWFBASDAT(IGRID)%IOFLG
        VBVL=>GWFBASDAT(IGRID)%VBVL
        VBNM=>GWFBASDAT(IGRID)%VBNM
C
        XYGRID       =>GLOBALDAT(IGRID)%XYGRID                            !seb
        PDIFFPRT    =>GWFBASDAT(IGRID)%PDIFFPRT
        REALTIM     =>GWFBASDAT(IGRID)%REALTIM
        SIMTIM_PER  =>GWFBASDAT(IGRID)%SIMTIM_PER
        REALTIM_PER =>GWFBASDAT(IGRID)%REALTIM_PER
        TOTPERTIM   =>GWFBASDAT(IGRID)%TOTPERTIM
        USE_LEAP_YR =>GWFBASDAT(IGRID)%USE_LEAP_YR
        LISTSPLIT   =>GWFBASDAT(IGRID)%LISTSPLIT
        BUDGETDB    =>GWFBASDAT(IGRID)%BUDGETDB                         !seb
        INTER_INFO  =>GWFBASDAT(IGRID)%INTER_INFO
        PRNT_RES    =>GWFBASDAT(IGRID)%PRNT_RES
        PRNT_RES_LIM     => GWFBASDAT(IGRID)%PRNT_RES_LIM    
        PRNT_RES_CUM     => GWFBASDAT(IGRID)%PRNT_RES_CUM    
        PRNT_RES_CUM_ARR => GWFBASDAT(IGRID)%PRNT_RES_CUM_ARR
        DEALLOCATE_MULT=>GWFBASDAT(IGRID)%DEALLOCATE_MULT
        MAX_REL_VOL_ERROR=>GWFBASDAT(IGRID)%MAX_REL_VOL_ERROR
        MAX_REL_VOL_INVOKED=>GWFBASDAT(IGRID)%MAX_REL_VOL_INVOKED
        !
        MIN_SOLVER_INTER    =>GWFBASDAT(IGRID)%MIN_SOLVER_INTER
        MIN_ITER_INPUT      =>GWFBASDAT(IGRID)%MIN_ITER_INPUT
        MIN_SOLVER_INTER_SP =>GWFBASDAT(IGRID)%MIN_SOLVER_INTER_SP
        MIN_SOLVER_INTER_NEW=>GWFBASDAT(IGRID)%MIN_SOLVER_INTER_NEW

        ADAMP_INPUT    =>GWFBASDAT(IGRID)%ADAMP_INPUT
        BAS_ADAMP      =>GWFBASDAT(IGRID)%BAS_ADAMP
        BAS_ADAMP_TOL  =>GWFBASDAT(IGRID)%BAS_ADAMP_TOL
        BAS_ADAMP_TOL2 =>GWFBASDAT(IGRID)%BAS_ADAMP_TOL2
        HED_CHNG2      =>GWFBASDAT(IGRID)%HED_CHNG2
        HED_CHNG3      =>GWFBASDAT(IGRID)%HED_CHNG3
        HED_LOCK       =>GWFBASDAT(IGRID)%HED_LOCK
        DATE_SP        =>GWFBASDAT(IGRID)%DATE_SP
        HAS_STARTDATE  =>GWFBASDAT(IGRID)%HAS_STARTDATE
C
        !LSTLVL=>GLOBALDAT(IGRID)%LSTLVL
        SPSTART=>GLOBALDAT(IGRID)%SPSTART
        SPEND  =>GLOBALDAT(IGRID)%SPEND
        NOCBC  =>GLOBALDAT(IGRID)%NOCBC
        !CBC_GLOBAL=>GLOBALDAT(IGRID)%CBC_GLOBAL
        !
        OSCIL_DMP_OUTER => GWFBASDAT(IGRID)%OSCIL_DMP_OUTER
        OSCIL_DMP_LRC   => GWFBASDAT(IGRID)%OSCIL_DMP_LRC
        OSCIL_DMP_DIF   => GWFBASDAT(IGRID)%OSCIL_DMP_DIF
        !
        PRNT_CNVG_OUTER => GWFBASDAT(IGRID)%PRNT_CNVG_OUTER
        PRNT_CNVG_NTERM => GWFBASDAT(IGRID)%PRNT_CNVG_NTERM
        PRNT_CNVG       => GWFBASDAT(IGRID)%PRNT_CNVG
        PRNT_CNVG_LRC   => GWFBASDAT(IGRID)%PRNT_CNVG_LRC
        PRNT_CNVG_DIF   => GWFBASDAT(IGRID)%PRNT_CNVG_DIF
        !
        PRNT_FRES_OUTER => GWFBASDAT(IGRID)%PRNT_FRES_OUTER
        PRNT_FRES_NTERM => GWFBASDAT(IGRID)%PRNT_FRES_NTERM
        PRNT_FRES       => GWFBASDAT(IGRID)%PRNT_FRES
        PRNT_FRES_LRC   => GWFBASDAT(IGRID)%PRNT_FRES_LRC
        PRNT_FRES_DIF   => GWFBASDAT(IGRID)%PRNT_FRES_DIF
        !
        PRNT_VERR_OUTER => GWFBASDAT(IGRID)%PRNT_VERR_OUTER
        PRNT_VERR_NTERM => GWFBASDAT(IGRID)%PRNT_VERR_NTERM
        PRNT_VERR       => GWFBASDAT(IGRID)%PRNT_VERR
        PRNT_VERR_LRC   => GWFBASDAT(IGRID)%PRNT_VERR_LRC
        PRNT_VERR_DIF   => GWFBASDAT(IGRID)%PRNT_VERR_DIF
        !
        DAMPEN_START    => GWFBASDAT(IGRID)%DAMPEN_START
        DAMPEN_START_ITR=> GWFBASDAT(IGRID)%DAMPEN_START_ITR
        DAMPEN_START_DMP=> GWFBASDAT(IGRID)%DAMPEN_START_DMP
        !
        PVOL_ERR        => GWFBASDAT(IGRID)%PVOL_ERR
        !
        ABOVE_GSE_LIM    =>GWFBASDAT(IGRID)%ABOVE_GSE_LIM
        ABOVE_GSE_PRT_LIM=>GWFBASDAT(IGRID)%ABOVE_GSE_PRT_LIM
        ABOVE_GSE_PRT    =>GWFBASDAT(IGRID)%ABOVE_GSE_PRT
C
      RETURN
      END SUBROUTINE
      SUBROUTINE SGWF2BAS7PSV(IGRID)
C  Save global data for a grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        GLOBALDAT(IGRID)%NCOL=>NCOL
        GLOBALDAT(IGRID)%NROW=>NROW
        GLOBALDAT(IGRID)%NLAY=>NLAY
        GLOBALDAT(IGRID)%NPER=>NPER
        GLOBALDAT(IGRID)%NBOTM=>NBOTM
        GLOBALDAT(IGRID)%NCNFBD=>NCNFBD
        GLOBALDAT(IGRID)%ITMUNI=>ITMUNI
        GLOBALDAT(IGRID)%LENUNI=>LENUNI
        GLOBALDAT(IGRID)%IXSEC=>IXSEC
        GLOBALDAT(IGRID)%ITRSS=>ITRSS
        GLOBALDAT(IGRID)%INBAS=>INBAS
        GLOBALDAT(IGRID)%IFREFM=>IFREFM
        GLOBALDAT(IGRID)%NODES=>NODES
        GLOBALDAT(IGRID)%IOUT=>IOUT
        GLOBALDAT(IGRID)%MXITER=>MXITER
        GLOBALDAT(IGRID)%IRESTART=>IRESTART
        GLOBALDAT(IGRID)%KPERSTART=>KPERSTART
        GLOBALDAT(IGRID)%KSTPSTART=>KSTPSTART
C
        GLOBALDAT(IGRID)%IUNIT=>IUNIT
        GLOBALDAT(IGRID)%LAYCBD=>LAYCBD
        GLOBALDAT(IGRID)%LAYHDT=>LAYHDT
        GLOBALDAT(IGRID)%LAYHDS=>LAYHDS
        GLOBALDAT(IGRID)%PERLEN=>PERLEN
        GLOBALDAT(IGRID)%NSTP=>NSTP
        GLOBALDAT(IGRID)%TSMULT=>TSMULT
        GLOBALDAT(IGRID)%ISSFLG=>ISSFLG
        GLOBALDAT(IGRID)%DELR=>DELR
        GLOBALDAT(IGRID)%DELC=>DELC
        GLOBALDAT(IGRID)%AREA=>AREA
        GLOBALDAT(IGRID)%GSE=>GSE
        GLOBALDAT(IGRID)%BOTM=>BOTM
        GLOBALDAT(IGRID)%LBOTM=>LBOTM
        GLOBALDAT(IGRID)%WTABLE=>WTABLE
        GLOBALDAT(IGRID)%WTLAY=>WTLAY
        GLOBALDAT(IGRID)%UPLAY=>UPLAY
        GLOBALDAT(IGRID)%UPLAY_IDX=>UPLAY_IDX
        GLOBALDAT(IGRID)%HNEW=>HNEW
        GLOBALDAT(IGRID)%HNEW_OLD=>HNEW_OLD
        GLOBALDAT(IGRID)%HOLD=>HOLD
        GLOBALDAT(IGRID)%IBOUND=>IBOUND
        !GLOBALDAT(IGRID)%WETCEL=>WETCEL
        GLOBALDAT(IGRID)%CR=>CR
        GLOBALDAT(IGRID)%CC=>CC
        GLOBALDAT(IGRID)%CV=>CV
        GLOBALDAT(IGRID)%HCOF=>HCOF
        GLOBALDAT(IGRID)%RHS=>RHS
        GLOBALDAT(IGRID)%BUFF=>BUFF
        GLOBALDAT(IGRID)%RBUF=>RBUF
        GLOBALDAT(IGRID)%STRT=>STRT
        GLOBALDAT(IGRID)%DDREF=>DDREF
        GLOBALDAT(IGRID)%SPTIM=>SPTIM
        GLOBALDAT(IGRID)%DATAFILES=>DATAFILES
C
        PARAMDAT(IGRID)%ICLSUM=>ICLSUM                                  !seb
        PARAMDAT(IGRID)%IPSUM=>IPSUM
        PARAMDAT(IGRID)%INAMLOC=>INAMLOC
        PARAMDAT(IGRID)%NMLTAR=>NMLTAR
        PARAMDAT(IGRID)%NZONAR=>NZONAR
        PARAMDAT(IGRID)%NPVAL=>NPVAL
        PARAMDAT(IGRID)%PROPPRINT=>PROPPRINT                            !seb
C
        PARAMDAT(IGRID)%B=>B
        PARAMDAT(IGRID)%IACTIVE=>IACTIVE
        PARAMDAT(IGRID)%IPLOC=>IPLOC
        PARAMDAT(IGRID)%IPCLST=>IPCLST
        PARAMDAT(IGRID)%IZON=>IZON
        PARAMDAT(IGRID)%RMLT=>RMLT
        PARAMDAT(IGRID)%PARNAM=>PARNAM
        PARAMDAT(IGRID)%PARTYP=>PARTYP
        PARAMDAT(IGRID)%ZONNAM=>ZONNAM
        PARAMDAT(IGRID)%MLTNAM=>MLTNAM
        PARAMDAT(IGRID)%INAME=>INAME
C
        GWFBASDAT(IGRID)%MSUM=>MSUM
        GWFBASDAT(IGRID)%IHEDFM=>IHEDFM
        GWFBASDAT(IGRID)%IHEDUN=>IHEDUN
        GWFBASDAT(IGRID)%IDDNFM=>IDDNFM
        GWFBASDAT(IGRID)%IDDNUN=>IDDNUN
        GWFBASDAT(IGRID)%IBOUUN=>IBOUUN
        GWFBASDAT(IGRID)%LBHDSV=>LBHDSV
        GWFBASDAT(IGRID)%LBDDSV=>LBDDSV
        GWFBASDAT(IGRID)%LBBOSV=>LBBOSV
        GWFBASDAT(IGRID)%IBUDFL=>IBUDFL
        GWFBASDAT(IGRID)%ICBCFL=>ICBCFL
        GWFBASDAT(IGRID)%IHDDFL=>IHDDFL
        GWFBASDAT(IGRID)%IAUXSV=>IAUXSV
        GWFBASDAT(IGRID)%IBDOPT=>IBDOPT
        GWFBASDAT(IGRID)%IPRTIM=>IPRTIM
        GWFBASDAT(IGRID)%IPEROC=>IPEROC
        GWFBASDAT(IGRID)%ITSOC=>ITSOC
        GWFBASDAT(IGRID)%ICHFLG=>ICHFLG
        GWFBASDAT(IGRID)%IDDREF=>IDDREF
        GWFBASDAT(IGRID)%IDDREFNEW=>IDDREFNEW
        GWFBASDAT(IGRID)%DELT=>DELT
        GWFBASDAT(IGRID)%PERTIM=>PERTIM
        GWFBASDAT(IGRID)%TOTIM=>TOTIM
        GWFBASDAT(IGRID)%HNOFLO=>HNOFLO
        GWFBASDAT(IGRID)%HDRY=>HDRY
        GWFBASDAT(IGRID)%STOPER=>STOPER
        GWFBASDAT(IGRID)%CHEDFM=>CHEDFM
        GWFBASDAT(IGRID)%CDDNFM=>CDDNFM
        GWFBASDAT(IGRID)%CBOUFM=>CBOUFM
        GWFBASDAT(IGRID)%IUBGT=>IUBGT
C
        GWFBASDAT(IGRID)%IOFLG=>IOFLG
        GWFBASDAT(IGRID)%VBVL=>VBVL
        GWFBASDAT(IGRID)%VBNM=>VBNM
C
        GLOBALDAT(IGRID)%XYGRID       =>XYGRID                          !seb
        !
        GWFBASDAT(IGRID)%PDIFFPRT     =>PDIFFPRT
        GWFBASDAT(IGRID)%REALTIM      =>REALTIM
        GWFBASDAT(IGRID)%SIMTIM_PER   =>SIMTIM_PER
        GWFBASDAT(IGRID)%REALTIM_PER  =>REALTIM_PER
        GWFBASDAT(IGRID)%TOTPERTIM    =>TOTPERTIM
        GWFBASDAT(IGRID)%USE_LEAP_YR  =>USE_LEAP_YR
        GWFBASDAT(IGRID)%LISTSPLIT    =>LISTSPLIT
        GWFBASDAT(IGRID)%BUDGETDB     =>BUDGETDB                        !seb
        GWFBASDAT(IGRID)%INTER_INFO   =>INTER_INFO
        GWFBASDAT(IGRID)%PRNT_RES     =>PRNT_RES
        GWFBASDAT(IGRID)%PRNT_RES_LIM     => PRNT_RES_LIM    
        GWFBASDAT(IGRID)%PRNT_RES_CUM     => PRNT_RES_CUM    
        GWFBASDAT(IGRID)%PRNT_RES_CUM_ARR => PRNT_RES_CUM_ARR
        GWFBASDAT(IGRID)%DEALLOCATE_MULT=>DEALLOCATE_MULT
        GWFBASDAT(IGRID)%MAX_REL_VOL_ERROR=>MAX_REL_VOL_ERROR
        GWFBASDAT(IGRID)%MAX_REL_VOL_INVOKED=>MAX_REL_VOL_INVOKED
        !
        GWFBASDAT(IGRID)%MIN_SOLVER_INTER    =>MIN_SOLVER_INTER
        GWFBASDAT(IGRID)%MIN_ITER_INPUT      =>MIN_ITER_INPUT
        GWFBASDAT(IGRID)%MIN_SOLVER_INTER_SP =>MIN_SOLVER_INTER_SP
        GWFBASDAT(IGRID)%MIN_SOLVER_INTER_NEW=>MIN_SOLVER_INTER_NEW
        !
        GWFBASDAT(IGRID)%ADAMP_INPUT    =>ADAMP_INPUT
        GWFBASDAT(IGRID)%BAS_ADAMP      =>BAS_ADAMP
        GWFBASDAT(IGRID)%BAS_ADAMP_TOL  =>BAS_ADAMP_TOL
        GWFBASDAT(IGRID)%BAS_ADAMP_TOL2 =>BAS_ADAMP_TOL2
        GWFBASDAT(IGRID)%HED_CHNG2      =>HED_CHNG2
        GWFBASDAT(IGRID)%HED_CHNG3      =>HED_CHNG3
        GWFBASDAT(IGRID)%HED_LOCK       =>HED_LOCK
        GWFBASDAT(IGRID)%DATE_SP        =>DATE_SP
        GWFBASDAT(IGRID)%HAS_STARTDATE  =>HAS_STARTDATE
C
        !GLOBALDAT(IGRID)%LSTLVL       =>LSTLVL
        GLOBALDAT(IGRID)%SPSTART      =>SPSTART
        GLOBALDAT(IGRID)%SPEND        =>SPEND
        GLOBALDAT(IGRID)%NOCBC        =>NOCBC
        !GLOBALDAT(IGRID)%CBC_GLOBAL   =>CBC_GLOBAL
      !
      GWFBASDAT(IGRID)%OSCIL_DMP_OUTER => OSCIL_DMP_OUTER
      GWFBASDAT(IGRID)%OSCIL_DMP_LRC   => OSCIL_DMP_LRC
      GWFBASDAT(IGRID)%OSCIL_DMP_DIF   => OSCIL_DMP_DIF
      !
      GWFBASDAT(IGRID)%PRNT_CNVG_OUTER => PRNT_CNVG_OUTER
      GWFBASDAT(IGRID)%PRNT_CNVG_NTERM => PRNT_CNVG_NTERM
      GWFBASDAT(IGRID)%PRNT_CNVG       => PRNT_CNVG
      GWFBASDAT(IGRID)%PRNT_CNVG_LRC   => PRNT_CNVG_LRC
      GWFBASDAT(IGRID)%PRNT_CNVG_DIF   => PRNT_CNVG_DIF
      !
      GWFBASDAT(IGRID)%PRNT_FRES_OUTER => PRNT_FRES_OUTER
      GWFBASDAT(IGRID)%PRNT_FRES_NTERM => PRNT_FRES_NTERM
      GWFBASDAT(IGRID)%PRNT_FRES       => PRNT_FRES
      GWFBASDAT(IGRID)%PRNT_FRES_LRC   => PRNT_FRES_LRC
      GWFBASDAT(IGRID)%PRNT_FRES_DIF   => PRNT_FRES_DIF
      !
      GWFBASDAT(IGRID)%PRNT_VERR_OUTER => PRNT_VERR_OUTER
      GWFBASDAT(IGRID)%PRNT_VERR_NTERM => PRNT_VERR_NTERM
      GWFBASDAT(IGRID)%PRNT_VERR       => PRNT_VERR
      GWFBASDAT(IGRID)%PRNT_VERR_LRC   => PRNT_VERR_LRC
      GWFBASDAT(IGRID)%PRNT_VERR_DIF   => PRNT_VERR_DIF
      !
      GWFBASDAT(IGRID)%DAMPEN_START    => DAMPEN_START
      GWFBASDAT(IGRID)%DAMPEN_START_ITR=> DAMPEN_START_ITR
      GWFBASDAT(IGRID)%DAMPEN_START_DMP=> DAMPEN_START_DMP
      !
      GWFBASDAT(IGRID)%PVOL_ERR=>PVOL_ERR
      !
      GWFBASDAT(IGRID)%ABOVE_GSE_LIM    =>ABOVE_GSE_LIM
      GWFBASDAT(IGRID)%ABOVE_GSE_PRT_LIM=>ABOVE_GSE_PRT_LIM
      GWFBASDAT(IGRID)%ABOVE_GSE_PRT    =>ABOVE_GSE_PRT

C
      RETURN
      END SUBROUTINE
!      SUBROUTINE UPDATEWETSTATUS(IUNITBCF,IUNITLPF,IUNITNWT,IUNITHUF)
!      USE GLOBAL, ONLY: NROW,NCOL,NLAY,IBOUND,WETCEL
!      IMPLICIT NONE
!      INTEGER::LAYTYPE
!      INTEGER::IR,IC,IL
!
!
!      WETTEDCELL
!      DO IL = 1, NLAY
!          DO IR = 1, NROW
!            DO IC = 1, NCOL
!              IF ( IBOUND(IC,IR,IL).GT.0 .AND. IPHDRY.GT.0 ) THEN
!                IF ( HNEW(IC, IR, IL)-DBLE(BOTM(IC,IR,LBOTM(IL)))
!     +                                                  .LT.2.0E-3 )
!     +               HNEW(IC, IR, IL) = DBLE(HDRY)
!                WETTEDCELL=.FALSE.
!              ELSE
!                WETTEDCELL=.TRUE.
!              END IF
!            ENDDO
!          ENDDO
!      ENDDO
!
!      END SUBROUTINE
