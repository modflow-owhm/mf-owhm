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
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
      USE CONSTANTS,  ONLY:NEG,Z,ONE,NL,BLN,TRUE,FALSE,BLNK,
     1                     inf,inf_I,DZ,DNEG
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     MXITER,IUNIT,NIUNIT,HNEW,LBOTM,LAYCBD,LAYHDT,
     3                     LAYHDS,PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,
     4                     BOTM,HOLD,IBOUND,CR,CC,CV,HCOF,RHS,BUFF,STRT,
     5                     DDREF,IRESTART,KPERSTART,KSTPSTART,
     6                     IUNITSTART,SPSTART,SPEND,NOCBC,RBUF,
     7                     INPUT_CHECK,BIN_REAL_KIND,HNEW_OLD,SPTIM,
     8                     BACKTRACKING, CBC_GLOBAL_UNIT, ALLOC_DDREF,
     9                     RCloseBAS, HCloseBAS, RCloseL2BAS
      USE GLOBAL,     ONLY:NO_CONST_HEAD, SUBLNK, UPLAY, UPLAY_IDX,
     +                     WTABLE, WTABLE_OLD
      USE GLOBAL,     ONLY: SUPER_NAMES
      USE PARAMMODULE,ONLY:MXPAR,MXCLST,MXINST,ICLSUM,IPSUM,
     1                     INAMLOC,NMLTAR,NZONAR,NPVAL,
     2                     B,IACTIVE,IPLOC,IPCLST,PARNAM,PARTYP,
     3                     ZONNAM,MLTNAM,INAME,PROPPRINT
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     +                      LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,
     +                      IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,
     +                      IDDREF,IDDREFNEW,DELT,PERTIM,TOTIM,HNOFLO,
     +                      HDRY,STOPER,CHEDFM,CDDNFM,CBOUFM,VBVL,VBNM,
     +                      IUBGT,PDIFFPRT,REALTIM,USE_LEAP_YR,
     +                      SIMTIM_PER,REALTIM_PER,TOTPERTIM,SIMTIME,
     +                      LISTSPLIT,BUDGETDB,DATE_SP,DEALLOCATE_MULT,
     +                      PRNT_CNVG_OUTER, PRNT_CNVG_NTERM, PRNT_CNVG,
     +                      PRNT_CNVG_LRC,PRNT_CNVG_DIF,
     +                      PRNT_FRES_OUTER, PRNT_FRES_NTERM, PRNT_FRES,
     +                      PRNT_FRES_LRC,PRNT_FRES_DIF,
     +                      PRNT_VERR_OUTER, PRNT_VERR_NTERM, PRNT_VERR,
     +                      PRNT_VERR_LRC,PRNT_VERR_DIF,
     +                      SAVE_HEAD,SAVE_HEAD_FLAG,
     +                      PRINT_HEAD,PRINT_HEAD_FLAG,
     +                      PRINT_WTAB,PRINT_WTAB_FLAG,
     +                      PRINT_WDEP,PRINT_WDEP_FLAG,
     +                      ADAMP_INPUT, BAS_ADAMP, BAS_ADAMP_TOL,
     +                      BAS_ADAMP_TOL2,HED_CHNG2, HED_CHNG3,
     +                      HED_LOCK,INTER_INFO,HAS_STARTDATE,
     +                      MAX_REL_VOL_ERROR,MAX_REL_VOL_INVOKED,
     +                      MIN_ITER_INPUT,   MIN_SOLVER_INTER_SP,
     +                      MIN_SOLVER_INTER, MIN_SOLVER_INTER_NEW,
     +                      OSCIL_DMP_OUTER,OSCIL_DMP_LRC,OSCIL_DMP_DIF,
     +                   DAMPEN_START,DAMPEN_START_ITR,DAMPEN_START_DMP,
     +                   ABOVE_GSE_LIM,ABOVE_GSE_PRT,ABOVE_GSE_PRT_LIM,
     +                   PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM,
     +                   PRNT_CUM_HEAD_CHNG, CUM_HEAD_CHNG,
     +                   CUM_HEAD_CHNG_E10
      USE BAS_UTIL,             ONLY: CHECK_FOR_VALID_DIMENSIONS
      USE IS_ROUTINES,          ONLY: IS_BLANK, IS_INTEGER
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR, WARNING_MESSAGE
      USE FILE_IO_INTERFACE,    ONLY: COMMENT_INDEX, READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD, PARSE_WORD_UP
      USE STRINGS,              ONLY: UPPER, GET,
     +                                GET_INTEGER, GET_NUMBER, GET_WORD
      USE NUM2STR_INTERFACE,                ONLY: NUM2STR, INTFMT
      USE DATE_OPERATOR_INSTRUCTION,        ONLY: DATE_OPERATOR
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
      USE EquationParser, ONLY: EQUATION_SETUP_ERROR_ROUTINES
      USE LINKED_LIST_INSTRUCTION, ONLY: CHARACTER_LINKED_LIST
      USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
      USE GENERIC_INPUT_FILE_INSTRUCTION,  ONLY: GENERIC_INPUT_FILE
      USE ULOAD_AND_SFAC_INTERFACE,      ONLY: ULOAD
      USE LOAD_SUPER_NAMES_INTERFACE,    ONLY: LOAD_SUPER_NAMES
      USE BAS_OPTIONS_AND_STARTDATE,     ONLY: GET_BAS_OPTIONS, 
     +                                         GET_BAS_START_DATE_OPTION
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
      CHARACTER(1280):: LINE
      CHARACTER(:), ALLOCATABLE:: FASTFORWARD
C
      DOUBLE PRECISION:: HNF
      CHARACTER(24):: ANAME(2)
      LOGICAL:: SKIP_OPT_LINE,HAS_OPT_LINE,NO_OPT_LINE
      LOGICAL:: FOUND_BEGIN, WARN_DIM
      LOGICAL:: HAS_CELL_WATCH, NO_CONVERGENCE_STOP
      REAL,DIMENSION(:),ALLOCATABLE:: HSHIFT
      DOUBLE PRECISION:: NaN
C     ------------------------------------------------------------------
      ANAME(1) = '          BOUNDARY ARRAY'
      ANAME(2) = '            INITIAL HEAD'
      !
      NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
      !
      CALL STARTING_DATE%INIT() ! SETS IT TO 'NO_DATE' CAUSE NO ARG PASSED
      !
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(CBC_GLOBAL_UNIT,NOCBC)
      ALLOCATE(LISTSPLIT,BUDGETDB,SPSTART,SPEND,DEALLOCATE_MULT)
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
      ALLOCATE(PRNT_CUM_HEAD_CHNG, CUM_HEAD_CHNG, CUM_HEAD_CHNG_E10)
      ALLOCATE(PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM)
      ALLOCATE(DAMPEN_START,DAMPEN_START_ITR,DAMPEN_START_DMP)
      ALLOCATE(OSCIL_DMP_OUTER)
      ALLOCATE(ABOVE_GSE_LIM,     SOURCE=inf)
      ALLOCATE(ABOVE_GSE_PRT_LIM, SOURCE=inf)
      ALLOCATE(ABOVE_GSE_PRT)
      ALLOCATE(BACKTRACKING, SOURCE=.FALSE.)
      !
      ALLOCATE(SAVE_HEAD_FLAG,  SOURCE=0)   ! 0: not in use, 1 in use and SP specified, 2, print last time step, 3 print every timestep
      ALLOCATE(PRINT_HEAD_FLAG, SOURCE=0) 
      ALLOCATE(PRINT_WTAB_FLAG, SOURCE=0)
      ALLOCATE(PRINT_WDEP_FLAG, SOURCE=0)
      !
      ALLOCATE(SAVE_HEAD)
      ALLOCATE(PRINT_HEAD(1))
      ALLOCATE(PRINT_WTAB(1))
      ALLOCATE(PRINT_WDEP(1))
      !PRINT_HEAD(1)%EXTRA = ""  ! Allocate as empty to indicate option not in use
      !
      ALLOCATE(RCloseBAS, HCloseBAS, RCloseL2BAS)
      !
      RCloseBAS            = DZ
      HCloseBAS            = DZ
      RCloseL2BAS          = DZ
      !
      BAS_ADAMP            = inf_I
      BAS_ADAMP_TOL        = DZ
      BAS_ADAMP_TOL2       = DZ
      PRNT_CNVG_OUTER      = inf_I
      PRNT_FRES_OUTER      = inf_I
      PRNT_VERR_OUTER      = inf_I
      MAX_REL_VOL_ERROR    = 0.025D0
      MAX_REL_VOL_INVOKED  = FALSE
      MIN_SOLVER_INTER     = Z
      MIN_SOLVER_INTER_SP  = inf_I
      MIN_SOLVER_INTER_NEW = Z
      DAMPEN_START         = FALSE
      DAMPEN_START_ITR     = Z
      DAMPEN_START_DMP     = DZ
      OSCIL_DMP_OUTER      = inf_I
      PRNT_RES_LIM         = DNEG
      !
      !ALLOCATE(CBC_GLOBAL)
      MXITER = ONE
      NOCBC  = Z            ! INDICATES THAT CBC WILL BE WRITTEN IF REQUESTED
      CBC_GLOBAL_UNIT = Z   ! Default is not to set global unit number
      !
      ALLOCATE(IUNIT(NIUNIT))
      !
      ALLOCATE(ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL)
      ALLOCATE(PDIFFPRT,REALTIM,USE_LEAP_YR)
      ALLOCATE(PROPPRINT, SOURCE=' ')
      ALLOCATE(SIMTIM_PER, REALTIM_PER, TOTPERTIM, SIMTIME)
      ALLOCATE(HAS_STARTDATE, SOURCE=FALSE)
      !
      PDIFFPRT = 5                                                     ! IF PERCENT ERROR GOES ABOVE 5% THEN PRINT TO CMD PROMPT
      IF(IGRID == ONE)THEN
        ALLOCATE(NO_CONST_HEAD, SUBLNK,INPUT_CHECK,BIN_REAL_KIND)
        NO_CONST_HEAD = FALSE
        SUBLNK      = FALSE
        INPUT_CHECK = FALSE
        BIN_REAL_KIND = REAL32  !SINGLE PRECISION BINARY OUTPUT
      END IF
C
      ALLOCATE(MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,LBHDSV,LBDDSV,
     1         LBBOSV)
      ALLOCATE(IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,
     1         ICHFLG,IDDREF,IDDREFNEW,IUBGT)
      ALLOCATE(DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER)
      ALLOCATE(CHEDFM,CDDNFM,CBOUFM)
      HDRY = 1.E30
      IBDOPT = 1 
C
C2------Open all files in name file.
      CALL SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,INBAS,LISTSPLIT)!LINE = LIST FILE NAME
      !
C SET UP EQUATION PARSER ERROR ROUTINES
      IF(IGRID == ONE)THEN
          CALL EQUATION_SETUP_ERROR_ROUTINES(IOUT,
     +                          STOP_ERROR,WARNING_MESSAGE)
      END IF
C
C3------PRINT A MESSAGE IDENTIFYING THE BASIC PACKAGE.
        WRITE(IOUT,1)INBAS
    1 FORMAT(1X,/1X,'BAS -- BASIC PACKAGE, VERSION OWHM',
     2' INPUT READ FROM UNIT ',I5)
C
C3A-----SHOW PRECISION OF VARIABLES
      IPBUFF = PRECISION(BUFF)
      IPHNEW = PRECISION(HNEW)
      WRITE(IOUT,*)
      IF(IPBUFF /= IPHNEW) THEN
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
C4------Scan BAS options to see if a starting date was specified
      CALL GET_BAS_START_DATE_OPTION(LINE, INBAS, IOUT, STARTING_DATE)
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
      ALLOCATE (CR(NCOL,NROW,NLAY),  SOURCE=DZ)
      ALLOCATE (CC(NCOL,NROW,NLAY),  SOURCE=DZ)
      ALLOCATE (CV(NCOL,NROW,NLAY),  SOURCE=NaN)
      ALLOCATE (HCOF(NCOL,NROW,NLAY),SOURCE=DZ)
      ALLOCATE (RHS(NCOL,NROW,NLAY), SOURCE=DZ)
      ALLOCATE (BUFF(NCOL,NROW,NLAY),SOURCE=DZ)
      ALLOCATE (RBUF(NCOL,NROW,NLAY))
      ALLOCATE (STRT(NCOL,NROW,NLAY))
      DDREF=>STRT
      ALLOCATE(ALLOC_DDREF, SOURCE=FALSE)
      ALLOCATE (LAYHDT(NLAY))
      ALLOCATE (LAYHDS(NLAY))
      !
      ALLOCATE (UPLAY(NCOL,NROW), SOURCE=Z) ! UPLAY = upper most active layer, set to zero if all IBOUND=0, set to NLAY if last layer IBOUND/=0 but H<BOT
      !ALLOCATE (WTLAY(NCOL,NROW), SOURCE=Z) ! WTLAY = upper most active layer with H > BOT, set to zero for all other cases
      ALLOCATE (UPLAY_IDX)
      ALLOCATE (WTABLE(NCOL,NROW), WTABLE_OLD(NCOL,NROW))  ! Set via call set_adv_global_arrays(igrid) in subroutine modflow_owhm_run(name)
      !
C
C7------Initialize head-dependent thickness indicator to code that
C7------indicates layer is undefined.
      DO I=1,NLAY
        LAYHDT(I)=NEG
        LAYHDS(I)=NEG
      END DO
      !
      WRITE(IOUT,'(//)')
C
      IXSEC     = Z
      ICHFLG    = Z
      IFREFM    = ONE  !NOW DEFAULT TO FREEE FORMAT
      IPRTIM    = Z
      STOPER    = DZ
      MXPAR     = 2000
      MXCLST    = 2000000
      MXINST    = 50000
      MXBUD     = 100
      SPSTART   = Z
      SPEND     = NPER+1
      WARN_DIM  = TRUE
      DEALLOCATE_MULT=FALSE
      !
C8------Read BAS Package file.
C8A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
      CALL GET_BAS_OPTIONS(LINE, INBAS, IOUT, ICHFLG, IPRTIM, MXBUD,
     +                     HSHIFT, WARN_DIM, USE_PAUSE, TIME_INFO,
     +                     SUPER_NAMES_IN, STARTING_DATE)
      !
      ALLOCATE (B(MXPAR))
      ALLOCATE (IACTIVE(MXPAR))
      ALLOCATE (IPLOC(4,MXPAR))
      ALLOCATE (IPCLST(14,MXCLST))
      ALLOCATE (PARNAM(MXPAR))
      ALLOCATE (PARTYP(MXPAR))
      ALLOCATE (INAME(MXINST))
C
C4------Initialize parameter definition variables.
      IPSUM   = Z
      ICLSUM  = Z
      INAMLOC = ONE
      DO N=1,MXPAR
        PARNAM(N)=' '
        PARTYP(N)=' '
        IPLOC(1,N)=Z
        IPLOC(2,N)=Z
        IACTIVE(N)=Z
      END DO
C
C13-----WRITE OUT TIME STEP START AND END IF REQUESTED
      IF(TIME_INFO%IS_OPEN) THEN
        IU =  TIME_INFO%IU
        IF  (HAS_STARTDATE) THEN
            WRITE(IU,'(2A)') '   STEP     SP     TS           ',
     +                       'DELT         SIMTIM          DYEAR   DATE'
            WRITE(IU,'(3I7,2A15,F15.7,3x,A)') 0, 0, 0,'0.0','0.0',
     +                 DATE_SP(1)%TS(0)%DYEAR,
     +                 DATE_SP(1)%TS(0)%STR('T')
        ELSE
            WRITE(IU,'(2A)') '   STEP     SP     TS           ',
     +                       'DELT         SIMTIM'
        END IF

        K = Z
        DTMP = DZ
        DO I=1, NPER
          DO J=1, NSTP(I)
               K = K + ONE
               DTMP = DTMP + SPTIM(I)%DT(J) !TOTAL TIME
               !
               IF  (HAS_STARTDATE) THEN
                   WRITE(IU,'(3I7,2A15,F15.7,3x,A)') K, I, J,
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
        IF(IXSEC /= Z) WRITE(IOUT,61)
   61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
        IF(ICHFLG /= Z) WRITE(IOUT,62)
   62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
C
C8D-----INITIALIZE TOTAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER
C8D-----AND CALCULATE NUMBER OF CELLS.
      TOTIM   = 0.0
      SIMTIME = DZ
C
C8E-----READ BOUNDARY ARRAY(IBOUND).
      IF(IXSEC == Z) THEN
         DO K=1, NLAY
         KK=K
         CALL U2DINT(IBOUND(:,:,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT)
         END DO
      ELSE
         CALL U2DINT(IBOUND(:,:,1),ANAME(1),NLAY,NCOL,NEG,INBAS,IOUT)
      END IF
C
C CHECK FOR VALID MODEL DIMENSIONS NOW THAT IBOUND,DELR,DELC,BOTM HAS BEEN ESTABLISHED seb
      DIMTOL = 1E-5
      CALL CHECK_FOR_VALID_DIMENSIONS(DIMTOL,INBAS,IOUT,NROW,NCOL,NLAY,
     +                             IBOUND,LBOTM,BOTM,DELR,DELC,WARN_DIM)
C
      !WETCEL=IBOUND /= 0                                                !seb establish logical variable that is TRUE for wet cells and false for dry cells
C
C8F-----READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      CALL READ_TO_DATA(LINE, INBAS, NOSHIFT=IFREFM.EQ.Z)
      !
      LLOC = ONE
      IF(IFREFM == Z) THEN
         CALL GET_NUMBER(LINE(1:10),LLOC,ISTART,ISTOP,IOUT,INBAS,HNF,
     +                          MSG='BAS Package failed to read HNOFLO')
      ELSE
         CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,HNF,
     +                          MSG='BAS Package failed to read HNOFLO')
      END IF
      HNOFLO = REAL(HNF, kind(HNOFLO))
      WRITE(IOUT,3) NUM2STR(HNOFLO)
    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',A,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).',/)
C
C8G-----READ INITIAL HEADS.
      IF(IXSEC == Z) THEN
         DO K=1, NLAY
         KK=K
         CALL U2DREL(STRT(:,:,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT)
         END DO
      ELSE
         CALL U2DREL(STRT(:,:,1),ANAME(2),NLAY,NCOL,NEG,INBAS,IOUT)
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
      IF(STRT(J,I,K) /= STRT(J,I,K)) THEN
            STRT(J,I,K) = HNF
            HNEW(J,I,K) = HNF
            IF(IBOUND(J,I,K) /= Z) IBOUND(J,I,K) = Z
      ELSE
            HNEW(J,I,K)=STRT(J,I,K)
            IF(IBOUND(J,I,K) == Z) HNEW(J,I,K)=HNF
      END IF
      END DO
!  400 CONTINUE
C
C10-----SET UP OUTPUT CONTROL.
      CALL SGWF2BAS7I(NLAY,IUNIT(IUOC),IOUT,IFREFM,MXBUD)
C
C11-----INITIALIZE VOLUMETRIC BUDGET ACCUMULATORS TO ZERO.
      VBVL = 0.0
      !ZERO=0.
      !DO I=1,MXBUD
      !DO J=1,4
      !       VBVL(J,I)=ZERO
      !END DO
      !END DO
C
C12-----Allocate and read Zone and Multiplier arrays
      CALL SGWF2BAS7ARMZ(IUNIT(IUZON),IUNIT(IUMLT))
C
C13-----READ PARAMETER VALUES FILE.
      CALL SGWF2BAS7ARPVAL(IUPVAL)
      !
      !
      IF(SUPER_NAMES_IN%IU /= Z) THEN
       CALL LOAD_SUPER_NAMES(LINE, SUPER_NAMES_IN%IU, IOUT, SUPER_NAMES)
      END IF
      !
      ! Check if there are constant heads
      !
      IF(NO_CONST_HEAD) NO_CONST_HEAD = IUNIT(16) == Z   ! FHB Package in use
      IF(NO_CONST_HEAD) NO_CONST_HEAD = IUNIT(38) == Z   ! CHD Package in use
      IF(NO_CONST_HEAD) THEN
         LAY_LOOP: 
     +   DO K=1, NLAY
         DO I=1, NROW
         DO J=1, NCOL
         IF(IBOUND(J,I,K) < Z) THEN 
                               NO_CONST_HEAD = FALSE
                               EXIT LAY_LOOP
         END IF
         END DO
         END DO
         END DO LAY_LOOP
      END IF
         
C
C14-----SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2BAS7PSV(IGRID)
      !
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7AR2(IGRID)
      USE CONSTANTS,   ONLY:Z, ONE, TWO, NL, BLN
      USE ERROR_INTERFACE,ONLY: WARNING_MESSAGE
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IOUT, GSE,BOTM,LBOTM,
     +                     IBOUND, GLOBALDAT
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      INTEGER:: I,J,K
      !
      IF( GSE(1,1) /= GSE(1,1) ) THEN
          CALL WARNING_MESSAGE(OUTPUT=IOUT,
     +   MSG='DIS PACKAGE: THE GROUND SURFACE ELEVATION WAS NOT '//
     +   'SPECIFIED WITH THE KEYWORD "SURFACE_ELEVATION"'//BLN//
     +   'BEFORE LOADING THE "TOP" ARRAY FOR LAYER 1'//NL//
     +   'NOR WAS IT DEFINED WITH ANOTHER PACKAGE   (e.g. FMPs '//
     +   'GLOBAL DIMENSION KEYWORD "SURFACE_ELEVATION")'//BLN//
     +   'OneWater WILL SET THE GROUND SURFACE ELEVATION (GSE)'//NL//
     +   'TO THE TOP ELEVATION OF THE UPPER MOST ACTIVE CELL '//
     +   'OR THE DIS TOP OF LAY1.'//BLN//
     +   'NOTE THAT SFR DOES USE GSE FOR ALL DIVERSION SEGMENTS'//NL//
     +   '   WHEN "IRDFLG" IS SET TO 2 or -2.')
          !
          GLOBALDAT(IGRID)%GSE=>NULL()
          DEALLOCATE(GSE)
          ALLOCATE(GSE(NCOL,NROW))
          GLOBALDAT(IGRID)%GSE=>GSE
          !
          DO I = ONE, NROW
          DO J = ONE, NCOL
                GSE(J,I) = BOTM(J,I,0)  !Assumes that TOP assigns the default upper elevation
                IF(IBOUND(J,I,1) == Z) THEN
                   DO K=TWO, NLAY
                                IF(IBOUND(J,I,K) /= Z) THEN
                                    !
                                    GSE(J,I) = BOTM(J,I,LBOTM(K)-1)
                                    EXIT
                                    !
                                END IF
                   END DO
                END IF
          END DO
          END DO
          !
      END IF
      !
      END SUBROUTINE
      !
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
      USE ERROR_INTERFACE,   ONLY: WARNING_MESSAGE, FILE_IO_ERROR
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER
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
!!!      DELT=PERLEN(KPER)/REAL(NSTP(KPER))
!!!C
!!!C2B-----IF TIME STEP MULTIPLIER IS NOT ONE THEN CALCULATE FIRST
!!!C2B-----TERM OF GEOMETRIC PROGRESSION.
!!!      ONE=1.
!!!      IF(TSMULT(KPER) /= ONE)
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
      IF(KPER == 1) CALL SGWF2BAS7STPVAL()                              !seb CHANGED FROM KPER.GT.1
C seb PRINT OUT MAX PARAMETER USAGE
      IF(KPER == 1) THEN
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
      ELSEIF(REALTIM.GE.0D0)THEN !.AND.ISSFLG(KPER) == 0)                  !seb  ADDED POTENTIAL SKIPPING OF SS SP
          CALL DECIMAL_YEAR(REALTIM_PER,TOTPERTIM,ITMUNI,USE_LEAP_YR)
      END IF
      !
      ! UPDATE MINIMUM SOLVER ITERATIONS IF REQUESTED
      !
      IF( MIN_ITER_INPUT%IU  /=  0) THEN          !CAN LOAD SOLVER INPUT
       IF(MIN_SOLVER_INTER_SP <= KPER) THEN       !UPDATE SOLVER
         BLOCK
           CHARACTER(128):: LINE
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
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7AD(KPER,KSTP,IGRID)
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,   ONLY:Z, ONE, TWO, inf_I,FALSE, BLN, NL, D100
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,TSMULT,HNEW,HOLD,ITMUNI,
     +                     SPTIM, HNEW_OLD, IOUT, GSE,BOTM,LBOTM,IBOUND,
     +                     GLOBALDAT, WTABLE, WTABLE_OLD
      USE GWFBASMODULE,ONLY:DELT,TOTIM,PERTIM,REALTIM,USE_LEAP_YR,
     +                      DATE_SP,DEALLOCATE_MULT, SIMTIME,
     +                      ADAMP_INPUT,BAS_ADAMP,MAX_REL_VOL_INVOKED,
     +                      HAS_STARTDATE
      USE PARAMMODULE, ONLY:NMLTAR,MLTNAM,RMLT!,NZONAR,ZONNAM
      USE BAS_UTIL,    ONLY: DECIMAL_YEAR
      USE SET_ARRAY_INTERFACE, ONLY: SET_ARRAY
      USE ERROR_INTERFACE,     ONLY: WARNING_MESSAGE
      USE FILE_IO_INTERFACE,   ONLY: READ_TO_DATA
      USE STRINGS,             ONLY: GET_INTEGER
      INTEGER:: YEAR
      LOGICAL:: LEAPYR
      DOUBLE PRECISION:: DDELT
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
      IF(KSTP==ONE) THEN
        IF(ADAMP_INPUT%IU /= Z) THEN  !IS NONZERO IF FILE IS OPEN
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
      !IF(KSTP /= 1) THEN
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
      ELSEIF(REALTIM.GE.0D0)THEN !.AND.ISSFLG(KPER) == Z)                  !seb  ADDED POTENTIAL SKIPPING OF SS SP
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
      CALL SET_ARRAY(NCOL, NROW, WTABLE, WTABLE_OLD)
      !
      CALL SET_ARRAY(NCOL,NROW,NLAY,HNEW,HNEW_OLD)  !HOLDS PREVIOUS INTERATION VALUE
C
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7FM(IGRID)
C     ******************************************************************
C     SET HCOF=RHS=0.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: HCOF, RHS, NROW, NCOL, NLAY
      USE CONSTANTS, ONLY: DZ
      USE SET_ARRAY_INTERFACE, ONLY:SET_ZERO
C     -----------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------FOR EACH CELL INITIALIZE HCOF AND RHS ACCUMULATORS.
      !
      CALL SET_ZERO(NCOL,NROW,NLAY,HCOF)
      CALL SET_ZERO(NCOL,NROW,NLAY, RHS)
      !
      !CALL GWF2BAS7UPLAY() MOVED TO BAS_POST_SOLVER
C
      END SUBROUTINE
      !
      SUBROUTINE SET_ADV_GLOBAL_ARRAYS(IGRID)                           !SET UPLAY, UPLAY_IDX, AND WTABLE
      USE GLOBAL,         ONLY: NROW, NCOL, WTABLE, WTABLE_OLD
      USE SET_ARRAY_INTERFACE, ONLY: SET_ARRAY
      IMPLICIT NONE
      INTEGER,INTENT(IN):: IGRID
      !
      IF(IGRID /= 0) CALL SGWF2BAS7PNT(IGRID)
      !
      CALL GWF2BAS7UPLAY()
      !
      CALL SET_ARRAY(NCOL, NROW, WTABLE, WTABLE_OLD)
      !
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7UPLAY()                                 !SET UPLAY, UPLAY_IDX, AND WTABLE
      USE GLOBAL,    ONLY: NROW, NCOL, NLAY, NBOTM, HNEW, IBOUND,
     +                     BOTM, LBOTM, LAYHDT, UPLAY, UPLAY_IDX,
     +                     WTABLE
      IMPLICIT NONE
      !
      CALL GWF2BAS7UPLAY_PASS(NROW,NCOL,NLAY,NBOTM,HNEW,IBOUND,
     +                   BOTM,LBOTM,LAYHDT,UPLAY,UPLAY_IDX)
      !
      CALL GWF2BAS7WTABLE(NROW,NCOL,NLAY,UPLAY,HNEW,WTABLE)  !WTABLE is water table or upper most pieziometric surface
      !
      END SUBROUTINE
      !
      PURE SUBROUTINE GWF2BAS7WTABLE(NROW,NCOL,NLAY,UPLAY,HNEW,WTABLE)
      !
      USE CONSTANTS, ONLY: ninf
      !
      IMPLICIT NONE
      INTEGER,       INTENT(IN):: NCOL,NROW,NLAY
      INTEGER,          DIMENSION(NCOL,NROW     ), INTENT(IN   )::UPLAY
      DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::HNEW
      DOUBLE PRECISION, DIMENSION(NCOL,NROW     ), INTENT(  OUT)::WTABLE
      !
      INTEGER:: I, J, WT
      !
C2------SET HEAD FOR UPPER MOST ACTIVE/NONDRY LAYER
      !
      !WHERE (UPLAY==0); WTABLE = ninf
      !END WHERE
      !
      DO J=1, NROW
      DO I=1, NCOL
                  WT = UPLAY(I,J)
                  IF ( WT < 0 ) WT = -WT
                  IF ( WT > 0 ) THEN
                                WTABLE(I,J) = HNEW(I,J, WT)
                  ELSE
                                WTABLE(I,J) = ninf
                  END IF
      END DO
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
      PURE SUBROUTINE GWF2BAS7UPLAY_PASS(NROW,NCOL,NLAY,NBOTM,HNEW,    !--MAY HAVE COMPILATION ISSUES WHEN NOT USING DOUBLE PRECISION COMPIATION
     +                                   IBOUND,BOTM,LBOTM,LAYHDT,
     +                                   UPLAY,UPLAY_IDX)
      !
      USE CONSTANTS,       ONLY: Z,ONE,TWO,THREE
      USE ARRAY_DATA_TYPES,ONLY: INTEGER_MATRIX
      !
      IMPLICIT NONE
      INTEGER, INTENT(IN):: NROW,NCOL,NLAY,NBOTM
      DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::HNEW
      INTEGER,          DIMENSION(NCOL,NROW,NLAY), INTENT(IN   )::IBOUND
      DOUBLE PRECISION, DIMENSION(NCOL,NROW,Z:NBOTM), INTENT(IN)::BOTM
      INTEGER,          DIMENSION(          NLAY), INTENT(IN   )::LBOTM
      INTEGER,          DIMENSION(          NLAY), INTENT(IN   )::LAYHDT
      INTEGER,          DIMENSION(NCOL,NROW     ), INTENT(INOUT)::UPLAY
      TYPE(INTEGER_MATRIX),                   INTENT(INOUT):: UPLAY_IDX
      !
      INTEGER:: I, J, K, N, UP, MXDIM
      !
      ! UPLAY = upper most active layer, set to zero if all IBOUND=0, set to NLAY if last layer IBOUND/=0 but H<BOT
      ! WTLAY = upper most active layer with H > BOT, set to zero if all IBOUND=0, set to -LAY to indicates no water table and LAY is the lowest active layer
      !
C2------CALCULATE UPPER MOST ACTIVE/NONDRY LAYER
      !
      DO CONCURRENT(J=1:NROW, I=1:NCOL);  UPLAY(I,J)=Z
      END DO
      !
      !DO K=ONE,NLAY
      !    WHERE(UPLAY==Z .AND.  )
      !          UPLAY = K
      !    END WHERE
      !END DO
      DO J=1, NROW
      DO I=1, NCOL
         DO K=ONE,NLAY
             IF(IBOUND(I,J,K) /= Z) THEN
                                    UPLAY(I,J) = K
                                    EXIT
             END IF
         END DO
      END DO
      END DO
      !
      DO J=1, NROW
      DO I=1, NCOL
      IF(UPLAY(I,J) > Z) THEN !Assumes that there are no convertible layers beneath confine layers
          UP = UPLAY(I,J)
          N  = LBOTM(UP)
          IF(LAYHDT(UP) /= Z           ) THEN
          IF(HNEW(I,J,UP) < BOTM(I,J,N)) THEN  !Note that UPLAY = Lowest Active Layer, when HNEW<BOTM for Lowest Active Layer
              !
              DO K=UP+ONE,NLAY
                  IF(IBOUND(I,J,K) /= Z) THEN
                     UPLAY(I,J) = K
                     N = LBOTM(K)
                     IF(LAYHDT(K)==Z             ) EXIT
                     IF(HNEW(I,J,K) > BOTM(I,J,N)) EXIT
                  ELSE
                      EXIT  !Lower layers are inactive
                  END IF
               END DO
          END IF
          END IF
      END IF
      END DO
      END DO
      !
      !
      !!!MXDIM = COUNT(UPLAY /= Z)  --Not used anywhere, so commented to save speed/memory
      !!!!
      !!!IF(MXDIM /= UPLAY_IDX%M) CALL UPLAY_IDX%ALLOC(THREE, MXDIM)
      !!!!
      !!!N = Z
      !!!DO K=ONE, NLAY
      !!!   DO J=ONE,NROW
      !!!   DO I=ONE,NCOL
      !!!         IF(UPLAY(I,J)==K) THEN
      !!!             N = N + ONE
      !!!             UPLAY_IDX%MAT(ONE,  N) = K
      !!!             UPLAY_IDX%MAT(TWO,  N) = J
      !!!             UPLAY_IDX%MAT(THREE,N) = I
      !!!         END IF
      !!!   END DO
      !!!   END DO
      !!!   IF(MXDIM == N) EXIT
      !!!END DO
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
!!!          WHERE(UPLAY==0 .AND. IBOUND /= 0 )
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
!!!     +                        .AND. IBOUND(I,J) /= 0 )
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
      USE SET_ARRAY_INTERFACE, ONLY:SET_ARRAY
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,HNEW_OLD,IBOUND
      USE GWFBASMODULE,ONLY:GWFBASDAT!,BAS_ADAMP,HED_CHNG2,HED_CHNG3
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID, KPER, KSTP, KITER
      !INTEGER:: I, J, K
      !REAL,DIMENSION(:,:,:),POINTER, CONTIGUOUS:: PNT
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
     +                    TENTH,CENTI,MILLI,DOS,DIEZ,UNO,NEARZERO_12,TEN
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,HNEW_OLD,IBOUND,
     +                      BOTM,LBOTM,AREA,SPTIM,GSE, !BACKTRACKING,
     +                      CELL_MASS_BALANCE, RELATIVE_MASS_ERROR,
     +                      MAX_RELATIVE_VOL_ERROR, CELL_VOL_ERROR,
     +                      RCloseBAS, HCloseBAS, RCloseL2BAS,
     +                      CALCULATE_MAX_HEAD_CHANGE,
     +                      CALCULATE_RESIDUAL_MASS_ERROR, IOUT
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
      USE SORT_INTERFACE,    ONLY: SORT
      USE BAS_UTIL,    ONLY: RELAX_HNEW, TOP_LIM_HNEW,PRINT_TOP_LIM_HNEW
      USE GWFSFRMODULE, ONLY: SFR_IN_USE, SFR_FRES, SFR_SEG_FRES
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: KPER, KSTP, KITER, MXITER
      INTEGER, INTENT(INOUT):: ICNVG
      !
      INTEGER:: I, J, K, II, ID, ITMP, CNT
      CHARACTER(19):: DT
      DOUBLE PRECISION:: DIF, ADIF, DTMP, DELT
      LOGICAL:: CHECK
      INTEGER, SAVE:: MAX_REL_VOL_ERROR_CNT = Z
      !
      !IF(BACKTRACKING) RETURN
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
        ELSEIF (ID  /=  OSCIL_DMP_LRC .OR.  ID == Z) THEN
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
      IF(ICNVG==ONE .OR. KITER>=MXITER) THEN
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
            DT = ADJUSTL(DT)
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
      IF(ICNVG==ONE .OR. KITER>=MXITER) CALL MAX_RELATIVE_VOL_ERROR(DIF)
      !--------------------------------------------------------------------
      !
      IF(ICNVG==ONE .OR. KITER>=MXITER) THEN
        IF( MAX_REL_VOL_ERROR_CNT < TEN) THEN                ! Can only stop convergences at most 10 times in a row
          !
          !CALL MAX_RELATIVE_VOL_ERROR(DIF)
          !
          IF(DIF > MAX_REL_VOL_ERROR) THEN
              ICNVG = Z
              MAX_REL_VOL_INVOKED   = TRUE
              MAX_REL_VOL_ERROR_CNT = MAX_REL_VOL_ERROR_CNT + ONE
          END IF
        END IF
      ELSE
              MAX_REL_VOL_ERROR_CNT = ONE
      END IF
      !
      !--------------------------------------------------------------------
      ! 
      CALL CALCULATE_MAX_HEAD_CHANGE(HCloseBAS)
      !
      CALL CALCULATE_RESIDUAL_MASS_ERROR(RCloseBAS, RCloseL2BAS)
      !
      IF(ICNVG==ONE .OR. KITER>=MXITER) THEN 
        WRITE(IOUT,'(/,/,A)')'Solver Convergence Information:'
        WRITE(IOUT,'(25x,A, 2x,A)') "     HClose:", NUM2STR(HCloseBAS)
        WRITE(IOUT,'(25x,A, 2x,A)') "     RClose:", NUM2STR(RCloseBAS)
        WRITE(IOUT,'(25x,A, 2x,A)') "  L2-RClose:", NUM2STR(RCloseL2BAS)
        WRITE(IOUT,'(25x,A, 2x,A)') "Rel-Vol-Err:", NUM2STR(DIF)
        WRITE(IOUT,*)
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
      IF(KITER >= MXITER.OR.ICNVG==1) THEN
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
        DO K=ONE,NLAY
        DO I=ONE,NROW
        DO J=ONE,NCOL
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
                       CALL SORT(PRNT_CNVG_DIF, PRNT_CNVG_LRC, 
     +                           SORT_B=FALSE, DESCEND=[TRUE, FALSE])
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
        END DO
        END DO
        END DO
        !
        CALL SORT(PRNT_CNVG_LRC, PRNT_CNVG_DIF, SORT_B=FALSE)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            ! DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
            DT = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, PRNT_CNVG_NTERM
         ID = PRNT_CNVG_LRC(II)
         IF(ID < ONE) CYCLE             !Fewer bad cells than requested for printing.
         !
         CALL CELLID_TO_LRC(ID, K, I, J, NLAY, NROW, NCOL)
         !
         DIF = PRNT_CNVG_DIF(II) ! HNEW(J,I,K) - HNEW_OLD(J,I,K) uncomment if output wants sign
         !
         IF(PRNT_CNVG%BINARY) THEN
         WRITE(PRNT_CNVG%IU)KPER,KSTP,KITER,K,I,J,HNEW(J,I,K),DIF,ID,DT
         ELSE
         WRITE(PRNT_CNVG%IU,'(2(1x,I4),1x,I5,3(1x,I4),2(1x,A),2(2x,A))')
     +                  KPER,KSTP,KITER,K,I,J,
     +                  NUM2STR7(HNEW(J,I,K),14), NUM2STR7(DIF,14),
     +                  NUM2STR(ID,8), DT
         END IF
        END DO
        !
      END IF !(PRNT_CNVG%IS_OPEN)
      !
      !--------------------------------------------------------------------
      !
      CHECK = PRNT_FRES%IS_OPEN
      IF(KITER >= MXITER.OR.ICNVG==1) THEN
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
        DO K=ONE,NLAY
        DO I=ONE,NROW
        DO J=ONE,NCOL
           !
           IF(IBOUND(J,I,K)>Z) THEN
               !
               !=================================================
               !
               DIF  = CELL_MASS_BALANCE(I,J,K)
               ADIF = ABS(DIF)
               !
               !=================================================
               !
               CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
               !
               IF(CNT > PRNT_FRES_NTERM) THEN
                   DO II = ONE, PRNT_FRES_NTERM
                       !IF( ADIF > ABS(PRNT_FRES_DIF(II)) ) THEN
                       IF( ADIF > PRNT_FRES_DIF(II) ) THEN
                         !
                         IF(II<PRNT_FRES_NTERM) THEN
                           DO ITMP=PRNT_FRES_NTERM, II+ONE, NEG
                             PRNT_FRES_LRC(ITMP)=PRNT_FRES_LRC(ITMP-ONE)
                             PRNT_FRES_DIF(ITMP)=PRNT_FRES_DIF(ITMP-ONE)
                           END DO
                         END IF
                         !
                         PRNT_FRES_LRC(II) = ID
                         PRNT_FRES_DIF(II) = ADIF  ! DIF
                         !
                         EXIT
                       END IF
                   END DO
               ELSE
                   CNT = CNT + ONE
                   PRNT_FRES_LRC(CNT) = ID
                   PRNT_FRES_DIF(CNT) = ADIF  ! DIF
                   !
                   IF(CNT == PRNT_FRES_NTERM) THEN
                       CALL SORT(PRNT_FRES_DIF, PRNT_FRES_LRC, 
     +                           SORT_B=FALSE, DESCEND=[TRUE, FALSE] )
                       CNT = CNT + ONE
                   END IF
               END IF
           END IF
           !
        END DO
        END DO
        END DO
        !
        CALL SORT(PRNT_FRES_LRC, PRNT_FRES_DIF, SORT_B=FALSE)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            ! DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
            DT = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, PRNT_FRES_NTERM
         !
         ID  = PRNT_FRES_LRC(II)
         IF(ID < ONE) CYCLE             !Fewer bad cells than requested for printing.
         DIF = PRNT_FRES_DIF(II)
         DTMP= DIF*DELT  !VOLUME LOST
         !
         CALL CELLID_TO_LRC(ID, K, I, J, NLAY, NROW, NCOL)
         !
         ADIF= AREA(J,I)*(BOTM(J,I,LBOTM(K)-ONE) - BOTM(J,I,LBOTM(K)))    !CELL VOLUME
         !
         IF(PRNT_FRES%BINARY) THEN
         WRITE(PRNT_FRES%IU)KPER,KSTP,KITER,K,I,J,HNEW(J,I,K),DIF,
     +                      DTMP,ADIF,ID,DT
         ELSE
         WRITE(PRNT_FRES%IU,'(2(1x,I4),1x,I5,3(1x,I4),2x,A,
     +                        3(1x,ES15.7),2(2x,A))')
     +                  KPER,KSTP,KITER,K,I,J,
     +                  NUM2STR7(HNEW(J,I,K),14), DIF,DTMP,ADIF,
     +                  NUM2STR(ID,8),DT
         END IF
        END DO
        !
      END IF !(PRNT_FRES%IS_OPEN)
      !
      !--------------------------------------------------------------------
      !
      CHECK = PRNT_VERR%IS_OPEN
      IF(KITER >= MXITER.OR.ICNVG==1) THEN
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
        DO K=ONE,NLAY
        DO I=ONE,NROW
        DO J=ONE,NCOL
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
                       CALL SORT(PRNT_VERR_DIF, PRNT_VERR_LRC, 
     +                           SORT_B=FALSE, DESCEND=[TRUE, FALSE] )
                       CNT = CNT + ONE
                   END IF
               END IF
           END IF
           !
        END DO
        END DO
        END DO
        !
        CALL SORT(PRNT_VERR_LRC, PRNT_VERR_DIF, SORT_B=FALSE)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            ! DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
            DT = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
        ELSE
            DT='NaN'
        END IF
        !
        DO II = ONE, PRNT_VERR_NTERM
         !
         ID  = PRNT_VERR_LRC(II)
         IF(ID < ONE) CYCLE       !Fewer bad cells than requested for printing.
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
     +                      ADIF,DTMP,ID,DT
         ELSE
         WRITE(PRNT_VERR%IU,'(2(1x,I4),1x,I5,3(1x,I4),1x,A,1x,F12.6,
     +                        2(1x,ES15.7),2(2x,A))')
     +                  KPER,KSTP,KITER,K,I,J,
     +                  NUM2STR7(HNEW(J,I,K),14), DIF,ADIF,DTMP,
     +                  NUM2STR(ID,8),DT
         END IF
        END DO
        !
      END IF !(PRNT_VERR%IS_OPEN)
      !
      ! CHECK IF SFR CAUSED MASS BALANCE ERRORS-----------------------------
      !
      IF(SFR_IN_USE) THEN
        IF(SFR_FRES%IS_OPEN .OR. SFR_SEG_FRES%IS_OPEN) THEN
            CALL SFR_POST_SOLVER(KPER, KSTP, KITER, MXITER,ICNVG,DELT)
            !
            CALL SFR_FRES%SIZE_CHECK()
            CALL SFR_SEG_FRES%SIZE_CHECK()
        END IF
      END IF
      !
      ! UPDATE THE UPLAY AND WTABLE ARRAYS----------------------------------
      !
      CALL GWF2BAS7UPLAY()
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
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
      USE CONSTANTS,   ONLY: NEG,Z,ONE,DZ,TRUE,FALSE
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,
     +                      BOTM,LBOTM,AREA,UPLAY,
     +                      CELL_MASS_BALANCE
      USE GWFBASMODULE,      ONLY: DATE_SP, HAS_STARTDATE
      USE UTIL_INTERFACE,    ONLY: LRC_TO_CELLID, CELLID_TO_LRC
      USE NUM2STR_INTERFACE, ONLY: NUM2STR, NUM2STR7
      USE SORT_INTERFACE,    ONLY: SORT
      USE GWFSFRMODULE, ONLY: NSTRM, ISTRM, STRM, SEG_NSTRM
      USE GWFSFRMODULE, ONLY: SFR_FRES,SFR_FRES_OUTER,SFR_FRES_NTERM,
     +                        SFR_FRES_LRC, SFR_FRES_DIF,
     +                        SFR_SEG_FRES, SFR_SEG_FRES_OUTER
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: KPER, KSTP, KITER, MXITER, ICNVG
      DOUBLE PRECISION, INTENT(IN   ):: DELT
      !
      INTEGER:: I, J, K, II, ITMP, CNT, L, IS, IR, ID, NTERM, ISTR, ISTP
      CHARACTER(8):: DT
      DOUBLE PRECISION:: DIF, ADIF, DTMP, HSTR, DEPTH, WIDTH, ELEV, COND
      DOUBLE PRECISION:: INFLOW, SEEP, OUTFLOW, RUNOFF, H
      LOGICAL:: CHECK
      LOGICAL:: CHECK2
      !
      !--------------------------------------------------------------------
      !
      CHECK = SFR_FRES%IS_OPEN
      IF(KITER >= MXITER.OR.ICNVG==1) THEN
                                       CONTINUE
      ELSEIF(CHECK .AND. SFR_FRES_OUTER>Z) THEN
                        CHECK = KITER >= SFR_FRES_OUTER
      ELSEIF(CHECK) THEN
                        CHECK = KITER >= MXITER + SFR_FRES_OUTER + ONE
      END IF
      !
      CHECK2 = SFR_SEG_FRES%IS_OPEN
      !
      IF(KITER >= MXITER.OR.ICNVG==1) THEN
                                       CONTINUE
      ELSEIF(CHECK2 .AND. SFR_SEG_FRES_OUTER>Z) THEN
                       CHECK2 = KITER >= SFR_SEG_FRES_OUTER
      ELSEIF(CHECK2) THEN
                       CHECK2 = KITER >= MXITER + SFR_SEG_FRES_OUTER+ONE
      END IF
      !
      IF(CHECK .OR. CHECK2) THEN
        !
        IF(CHECK) THEN
                      NTERM =  SFR_FRES_NTERM
        ELSE !(CHECK2)
                      NTERM = ONE
        END IF
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
               DIF  = CELL_MASS_BALANCE(I,J,K)
               ADIF = ABS(DIF)
               !
               !=================================================
               !
               IF(CNT > NTERM) THEN
                   DO II = ONE, NTERM
                       !IF( ADIF > ABS(SFR_FRES_DIF(II)) ) THEN
                       IF( ADIF > SFR_FRES_DIF(II) ) THEN
                         !
                         IF(II<NTERM) THEN
                           DO ITMP=NTERM, II+ONE, NEG
                             SFR_FRES_LRC(ITMP)=SFR_FRES_LRC(ITMP-ONE)
                             SFR_FRES_DIF(ITMP)=SFR_FRES_DIF(ITMP-ONE)
                           END DO
                         END IF
                         !
                         SFR_FRES_LRC(II) = L
                         SFR_FRES_DIF(II) = ADIF  ! DIF
                         !
                         EXIT
                       END IF
                   END DO
               ELSE
                   CNT = CNT + ONE
                   SFR_FRES_LRC(CNT) = L
                   SFR_FRES_DIF(CNT) = ADIF  ! DIF
                   !
                   IF(CNT == NTERM) THEN
                       CALL SORT(SFR_FRES_DIF, SFR_FRES_LRC, 
     +                           SORT_B=FALSE, DESCEND=[TRUE, FALSE] )
                       CNT = CNT + ONE
                   END IF
               END IF
           END IF
           !
        END DO
        !
        ITMP = SFR_FRES_LRC(ONE)  !Hold backup of worst reach
        !
        CALL SORT(SFR_FRES_LRC, SFR_FRES_DIF)  !SORT ON INDEX
        !
        IF(HAS_STARTDATE) THEN
            DT = DATE_SP(KPER)%TS(KSTP)%STR_MONTHYEAR()
        ELSE
            DT='NaN'
        END IF
        !
        IF(CHECK) THEN
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
             IF(K < UPLAY(J,I)) K = UPLAY(J,I)
             !
             IS = ISTRM(4,L)
             IR = ISTRM(5,L)
             !
             CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
             !
             ADIF= AREA(J,I)*(BOTM(J,I,LBOTM(K)-ONE)-BOTM(J,I,LBOTM(K)))  !CELL VOLUME
             !
             ELEV    = STRM(3, l)
             WIDTH   = STRM(5, l)
             OUTFLOW = STRM(9, l)
             INFLOW  = STRM(10, l)
             SEEP    = STRM(11, l)
             RUNOFF  = STRM(12, l) + STRM(24, l)
             HSTR    = STRM(15, l)
             COND    = STRM(16, l)
             DEPTH   = hstr - STRM(3, l)
             IF(DEPTH < DZ) DEPTH = DZ
             !
             IF(SFR_FRES%BINARY) THEN
             WRITE(SFR_FRES%IU)KPER,KSTP,KITER,K,I,J,IS,IR,HNEW(J,I,K),
     +                         DIF,DTMP,ADIF,DT,ID
             ELSE
          WRITE(SFR_FRES%IU,'(2(1x,I4), 1x,I5,5(1x,I4), 8(2x,A),
     +                        4(1x,ES15.7), 2(2x,A))')
     +                   KPER,KSTP,KITER,K,I,J,IS,IR,
     +                   NUM2STR7(HNEW(J,I,K),14),
     +                   NUM2STR7(HSTR,14),
     +                   NUM2STR7(ELEV,14),
     +                   NUM2STR7(WIDTH,14),
     +                   NUM2STR7(INFLOW,14),
     +                   NUM2STR7(OUTFLOW,14),
     +                   NUM2STR7(SEEP,14),
     +                   NUM2STR7(RUNOFF,14),
     +                   COND, ABS(DIF),DTMP,ADIF,
     +                   DT,NUM2STR(ID)
             END IF
          END IF
         END DO
        END IF
        !
        IF(CHECK2) THEN
         L  = ITMP  !The worst reach
         !
         IF( ITMP>Z ) THEN
                   IS   = ISTRM(4,L)
                   ISTR = SEG_NSTRM(IS)+ONE
                   ISTP = SEG_NSTRM(IS+ONE)
         ELSE
                   IS   = Z
                   ISTR = ONE
                   ISTP = Z
         END IF
         !
         DO L = ISTR, ISTP
          !
          K  = ISTRM(1,L)
          I  = ISTRM(2,L)
          J  = ISTRM(3,L)
          IR = ISTRM(5,L)
          !
          IF(K < UPLAY(J,I)) K = UPLAY(J,I)
          !
          IF(IBOUND(J,I,K)>Z ) THEN
              DIF = CELL_MASS_BALANCE(I,J,K)
              H   = HNEW(J,I,K)
          ELSE
              DIF = DZ
              H   = IEEE_VALUE(H, IEEE_QUIET_NAN)
          END IF
          !
          CALL LRC_TO_CELLID(ID, K, I, J, NLAY, NROW, NCOL)
          !
          ELEV    = STRM(3, l)
          WIDTH   = STRM(5, l)
          OUTFLOW = STRM(9, l)
          INFLOW  = STRM(10, l)
          SEEP    = STRM(11, l)
          RUNOFF  = STRM(12, l) + STRM(24, l)
          HSTR    = STRM(15, l)
          COND    = STRM(16, l)
          DEPTH   = hstr - STRM(3, l)
          IF(DEPTH < DZ) DEPTH = DZ
          !
          IF(SFR_SEG_FRES%BINARY) THEN
          WRITE(SFR_SEG_FRES%IU)KPER,KSTP,KITER,K,I,J,IS,IR,H,
     +                      DIF,ADIF,DT,ID
          ELSE
          WRITE(SFR_SEG_FRES%IU,'(2(1x,I4), 1x,I5,5(1x,I4), 8(2x,A),
     +                        2(1x,ES15.7), 2(2x,A))')
     +                   KPER,KSTP,KITER,K,I,J,IS,IR,
     +                   NUM2STR7(HNEW(J,I,K),14),
     +                   NUM2STR7(HSTR,14),
     +                   NUM2STR7(ELEV,14),
     +                   NUM2STR7(WIDTH,14),
     +                   NUM2STR7(INFLOW,14),
     +                   NUM2STR7(OUTFLOW,14),
     +                   NUM2STR7(SEEP,14),
     +                   NUM2STR7(RUNOFF,14),
     +                   COND, ABS(DIF),
     +                   DT,NUM2STR(ID)
          END IF
         END DO
        END IF
        !
      END IF !(SFR_SEG_FRES%IS_OPEN)
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
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      USE GLOBAL,      ONLY:IOUT,NLAY,NSTP,IXSEC,IFREFM,NOCBC
      USE GWFBASMODULE,ONLY:IHDDFL,IBUDFL,ICBCFL,IPEROC,ITSOC,IBDOPT,
     1                      IOFLG,IUBGT,PRNT_CNVG, 
     2                      SAVE_HEAD_FLAG, PRINT_HEAD_FLAG
C
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
      !
      CALL PRNT_CNVG%SIZE_CHECK()
C
C1------TEST UNIT NUMBER (INOC (INOC=IUNIT(12))) TO SEE IF
C1------OUTPUT CONTROL IS ACTIVE.  IF NOT, SET DEFAULTS AND RETURN.
      IF(INOC == 0) THEN
         IHDDFL=0
         IF(ICNVG == 0 .OR. KSTP == NSTP(KPER))IHDDFL=1
         IF( SAVE_HEAD_FLAG /= 0 ) IHDDFL=0           ! Head output requested through the BAS - Use that output instead
         IF(PRINT_HEAD_FLAG /= 0 ) IHDDFL=0           ! Head output requested through the BAS - Use that output instead
         IBUDFL=0
         IF(ICNVG == 0 .OR. KSTP == NSTP(KPER))IBUDFL=1
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
      IF(IFREFM == 0) THEN
         READ(INOC,'(4I10)') INCODE,IHDDFL,IBUDFL,ICBCFL
      ELSE
         READ(INOC,*) INCODE,IHDDFL,IBUDFL,ICBCFL
      END IF
        WRITE(IOUT,3) IHDDFL,IBUDFL,ICBCFL
    3 FORMAT(1X,/1X,'HEAD/DRAWDOWN PRINTOUT FLAG =',I2,
     1    5X,'TOTAL BUDGET PRINTOUT FLAG =',I2,
     2   /1X,'CELL-BY-CELL FLOW TERM FLAG =',I2)
      IF(ICBCFL /= 0) ICBCFL=IBDOPT
C
C4------DECODE INCODE TO DETERMINE HOW TO SET FLAGS IN IOFLG.
      IF(INCODE.LT.0) THEN
C
C5------INCODE <0, USE IOFLG FROM LAST TIME STEP.
          WRITE(IOUT,101)
  101   FORMAT(1X,'REUSING PREVIOUS VALUES OF IOFLG')
      ELSE IF(INCODE == 0) THEN
C
C6------INCODE=0, READ IOFLG FOR LAYER 1 AND ASSIGN SAME TO ALL LAYERS
        IF(IFREFM == 0) THEN
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
        IF(IXSEC == 0) THEN
           DO 301 K=1,NLAY
           IF(IFREFM == 0) THEN
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
           IF(IFREFM == 0) THEN
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
  600 IF(ICNVG == 0 .OR. KSTP == NSTP(KPER)) IBUDFL=1
C
C9------GLOBAL CBC OPTIONS
C
 1000 IF    (NOCBC == -1) THEN
                              ICBCFL=IBDOPT
      ELSEIF(NOCBC == -2) THEN
           IF(KSTP == NSTP(KPER)) THEN
                              ICBCFL=IBDOPT
           END IF
      ELSEIF(NOCBC == 2) THEN
                              ICBCFL=0

      END IF
C
C9------RETURN
C
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7OT(KSTP,KPER,ICNVG,ISA,IGRID,BUDPERC,
     +                      KITER,MXITER)
C     ******************************************************************
C     OUTPUT TIME, VOLUMETRIC BUDGET, HEAD, AND DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE,
     +                                        IEEE_QUIET_NAN,
     +                                        IEEE_POSITIVE_INF,
     +                                        IEEE_NEGATIVE_INF
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32
      USE CONSTANTS,   ONLY: BLNK, BLN, NL, TRUE, FALSE, DZ, D10, Z, ONE
      USE GLOBAL,      ONLY:ITMUNI,IOUT,NCOL,NROW,NLAY,HNEW,STRT,DDREF,
     1                      INPUT_CHECK,WORST_CELL_MASS_BALANCE,IBOUND,
     2                      MAX_RELATIVE_VOL_ERROR,NPER,NSTP,BUFF,RBUF,
     +                      GSE,CELL_MASS_BALANCE, HOLD, ALLOC_DDREF, 
     +                      WTABLE, IXSEC
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,IHDDFL,IBUDFL,BUDGETDB,
     +                     MSUM,VBVL,VBNM,IDDREF,IUBGT,PDIFFPRT,DATE_SP,
     +                     MAX_REL_VOL_ERROR,MAX_REL_VOL_INVOKED,
     +                     INTER_INFO,PVOL_ERR, HAS_STARTDATE,
     +                     PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM,
     +                     PRNT_RES_CUM_ARR,
     +                     SAVE_HEAD,SAVE_HEAD_FLAG,
     +                     PRINT_HEAD,PRINT_HEAD_FLAG,
     +                     PRINT_WTAB,PRINT_WTAB_FLAG,
     +                     PRINT_WDEP,PRINT_WDEP_FLAG,
     +                     PRNT_CUM_HEAD_CHNG, CUM_HEAD_CHNG,
     +                     CUM_HEAD_CHNG_E10, HDRY
      USE ALLOC_INTERFACE,   ONLY: ALLOC
      USE ERROR_INTERFACE,   ONLY: WARNING_MESSAGE
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE BUDGET_DATEBASE_WRITER
      USE BAS_UTIL, ONLY: MASS_ERROR_PRINT
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: KSTP,KPER,ICNVG,ISA,IGRID,KITER,MXITER
      REAL,    INTENT(INOUT):: BUDPERC
      !
      CHARACTER(19):: DATE

      INTEGER:: IPFLG
      INTEGER:: R, C, L, I, J, K, N
      LOGICAL:: HAS_PDIFFPRT
      DOUBLE PRECISION:: ERR, VERR, VOL, RAT
      REAL:: TOTRIN,TOTROT
      LOGICAL:: LAST_TS
      DOUBLE PRECISION:: NAN, inf, ninf
      DOUBLE PRECISION, dimension(:), allocatable:: HD 
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
      !
      NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN   )
      inf = IEEE_VALUE(NaN, IEEE_POSITIVE_INF)
      ninf= IEEE_VALUE(NaN, IEEE_NEGATIVE_INF)
C
C1------CLEAR PRINTOUT FLAG (IPFLG)
      IPFLG = Z
      BUDPERC=1.E30
C
C PRINT OUT BUDGET DATABASE
      IF(BUDGETDB%IU /= 0) THEN
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
      IF(ISA == 0) THEN
           WRITE(IOUT,9) KSTP,KPER
    9    FORMAT(1X,/11X,'NO FLOW EQUATION TO SOLVE IN TIME STEP',I5,
     1      ' OF STRESS PERIOD ',I6,/1X,'ALL HEADS ARE 0.0')
         IPFLG=1
      END IF
C
C3------IF HEAD AND DRAWDOWN FLAG (IHDDFL) IS SET WRITE HEAD,
C3------DRAWDOWN, AND IBOUND IN ACCORDANCE WITH FLAGS IN IOFLG.
      IF(IHDDFL == 0) GO TO 100
C
      CALL SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7IB(KSTP,KPER)
C
  100 CONTINUE

C4------PRINT TOTAL BUDGET IF REQUESTED
      IF(IBUDFL /= 0 .OR. ICNVG == 0 .OR. LAST_TS) THEN !GO TO 120                               !seb CHANGED TO ALLOW PERCENT ERROR CHECK AT EVERY TIMESTEP
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
!!!         IF(ICNVG == 0) THEN
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
      IF(ICNVG == 0 .OR. HAS_PDIFFPRT) THEN
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
!!!         WRITE(*,'(25x,2A,F6.1,A)')'Warning: ',
!!!     +         'Volumetric Rate Budget Percent Error is ',
!!!     +          BUDPERC,'%'
!!!         WRITE(*,'(34x,3A,/34x,6A/)')
!!!     +   'The worst vol. rate error is:           ',NUM2STR(ERR),
!!!     +   '   (L^3/T)', 'For model cell (Lay, Row, Col):          ',
!!!     +   NUM2STR(L),', ',NUM2STR(R),', ',NUM2STR(C)
!!!         WRITE(*,'(34x,3A,/34x,6A/)')
!!!     +   'Worst vol. rate error per cell volume is: ',
!!!     +   NUM2STR(VERR),' (1/T)',
!!!     +   'For model cell (Lay, Row, Col):           ',
!!!     +   NUM2STR(L),', ',NUM2STR(R),', ',NUM2STR(C)
!!!      END IF
!!!C
!!!C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
!!!      IF(ICNVG == 0) THEN
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
!!!      END IF !(ICNVG == 0)
      !
      ! Print out Interation Information and Mass Error if requested
      !
      IF(INTER_INFO%IU /= 0) THEN
         !
         K = -14  !NUM2STR Pad
         RAT = ABS(TOTRIN-TOTROT)
         VOL = RAT*DELT
         WRITE(INTER_INFO%IU,'(3I6, 2x,A, 1x,ES14.7, 1x,ES14.7, 2x,A)',
     -                                                    ADVANCE='NO')
     +            KPER,KSTP,KITER,NUM2STR(DELT,        K),
     +                    VOL,RAT,NUM2STR(ABS(BUDPERC),K)
         !
         IF(HAS_STARTDATE) THEN
             DATE = DATE_SP(KPER)%TS(KSTP-1)%STR_MONTHYEAR()  ! only want the mmm of mmm-yyyy
             WRITE(INTER_INFO%IU,'(1x,A,2x,A)',ADVANCE='NO')
     +                 DATE(1:3), DATE_SP(KPER)%TS(KSTP-1)%STR('T')
         END IF
         !
         WRITE(INTER_INFO%IU,'(A)')
         !
      END IF
      !
      ! Print out Residual Information
      !
      IF(PRNT_RES%IU /= 0) THEN
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
      !
      ! Keep track of cumulative residual error if requested
      !
      IF(PRNT_RES_CUM%IU /= 0) THEN
       ASSOCIATE(CUM=>PRNT_RES_CUM_ARR, IU=>PRNT_RES_CUM%IU,
     +           BIN=>PRNT_RES_CUM%BINARY, IB=>IBOUND       )
        !
        ! COMPILER ERROR
        !!!    DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IB(J,I,K) > 0)
        !!!          !
        !!!          CUM(J,I,K) = CUM(J,I,K)
        !!! +                   + ABS( CELL_MASS_BALANCE(I,J,K) ) * DELT
        !!!    END DO
        DO K=1, NLAY
        DO I=1, NROW
        DO J=1, NCOL
        IF( IB(J,I,K) > Z ) CUM(J,I,K) = CUM(J,I,K)
     +                          + ABS( CELL_MASS_BALANCE(I,J,K) ) * DELT
        END DO
        END DO
        END DO
        !
        IF(LAST_TS) THEN  ! LAST_TS = KPER==NPER .AND. KSTP==NSTP(KPER)
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
      !
      ! Print Head Arrays if Requested - Use Same Format as OC "SAVE HEAD" would produce ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      IF(SAVE_HEAD_FLAG /= 0) THEN 
        !
        n  = 0
        !
        IF ( SAVE_HEAD_FLAG == 1 ) THEN
          BLOCK
              CHARACTER(8):: SPTS
              SPTS(1:4) = TRANSFER( KPER, SPTS(1:4) )     !  I = TRANSFER(c, I)  !GET SP
              SPTS(5:8) = TRANSFER( KSTP, SPTS(5:8) )     !  J = TRANSFER(PRINT_HEAD(n)%EXTRA(5:8), J)  !GET TS
              DO I=1, SIZE(PRINT_HEAD)
                 IF( SPTS == PRINT_HEAD(I)%EXTRA ) THEN
                                                   n = I
                                                   EXIT                ! Only one file per SP/TS
                 END IF
              END DO
          END BLOCK
        ELSEIF(SAVE_HEAD_FLAG == 2) THEN  ! Last Time Step
                 if(kstp==nstp(kper))  n = 1
        ELSEIF(SAVE_HEAD_FLAG == 3) THEN  ! Every Time Step
                                       n = 1
        END IF
        !
        IF ( n > 0 ) THEN
          !
          CALL SAVE_HEAD%SIZE_CHECK()
          !
          IF(IS_REAL64(BUFF(1,1,1))) THEN
              DO K=1, NLAY
              DO I=1, NROW
              DO J=1, NCOL
                  IF(IBOUND(J,I,K)==0 .or.HNEW(J,I,K)== HDRY) THEN
                                          BUFF(J,I,K) = NaN
                  ELSEIF(HNEW(J,I,K) > 1.d100) THEN
                                          BUFF(J,I,K) = inf
                  ELSEIF(HNEW(J,I,K) < -1.d100) THEN
                                          BUFF(J,I,K) = ninf
                  ELSE
                      BUFF(J,I,K) = HNEW(J,I,K)
                  END IF
              END DO
              END DO
              END DO
          ELSE
              BLOCK
                  REAL:: NaNr, infr, ninfr
                  NaNr = IEEE_VALUE(NaN, IEEE_QUIET_NAN   )
                  infr = IEEE_VALUE(NaN, IEEE_POSITIVE_INF)
                  ninfr= IEEE_VALUE(NaN, IEEE_NEGATIVE_INF)
                  DO K=1, NLAY
                  DO I=1, NROW
                  DO J=1, NCOL
                      IF(IBOUND(J,I,K)==0 .or.HNEW(J,I,K)== HDRY) THEN
                                              BUFF(J,I,K) = NaNr
                      ELSEIF(HNEW(J,I,K) > 3.4d38) THEN
                                              BUFF(J,I,K) = infr
                      ELSEIF(HNEW(J,I,K) < -3.4d38) THEN
                                              BUFF(J,I,K) = ninfr
                      ELSE
                          BUFF(J,I,K) = REAL(HNEW(J,I,K), real32)
                      END IF
                  END DO
                  END DO
                  END DO
              END BLOCK
          END IF
          !
          BLOCK
          INTEGER:: IHEDUN
          CHARACTER(16):: TEXT
          CHARACTER(20):: CHEDFM
          IHEDUN = SAVE_HEAD%IU
          CHEDFM = SAVE_HEAD%FMT
          TEXT = '            HEAD'
          IF(IXSEC == 0) THEN
            IF(CHEDFM == ' ') THEN
               DO K=1,NLAY
                  CALL ULASAV(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,
     1                       NCOL,NROW,K,IHEDUN)
               END DO
            ELSE
               DO K=1,NLAY
                  CALL ULASV2(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,
     1                       NCOL,NROW,K,IHEDUN,CHEDFM,1,IBOUND(:,:,K))
               END DO
            END IF
          ELSE
            IF(CHEDFM == ' ') THEN
               CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IHEDUN)
            ELSE
               CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                    NLAY,-1,IHEDUN,CHEDFM,1,IBOUND)
            END IF
          END IF
          END BLOCK
        END IF
      END IF
      !
      ! Print Head Arrays if Requested ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      IF(PRINT_HEAD_FLAG /= 0) THEN !R, C, L, I, J, K
        !
        n  = 0
        !
        IF ( PRINT_HEAD_FLAG == 1 ) THEN
          BLOCK
              CHARACTER(8):: SPTS
              SPTS(1:4) = TRANSFER( KPER, SPTS(1:4) )     !  I = TRANSFER(c, I)  !GET SP
              SPTS(5:8) = TRANSFER( KSTP, SPTS(5:8) )     !  J = TRANSFER(PRINT_HEAD(n)%EXTRA(5:8), J)  !GET TS
              DO I=1, SIZE(PRINT_HEAD)
                 IF( SPTS == PRINT_HEAD(I)%EXTRA ) THEN
                                                   n = I
                                                   EXIT                ! Only one file per SP/TS
                 END IF
              END DO
          END BLOCK
        ELSEIF(PRINT_HEAD_FLAG == 2) THEN  ! Last Time Step
                 if(kstp==nstp(kper))  n = 1
        ELSEIF(PRINT_HEAD_FLAG == 3) THEN  ! Every Time Step
                                       n = 1
        END IF
        !
        IF ( n > 0 ) THEN
          !
          CALL ALLOC(HD, NCOL)
          !
          CALL PRINT_HEAD(n)%SIZE_CHECK()
          !
          K = -14  !NUM2STR Pad
          RAT = ABS(TOTRIN-TOTROT)
          VOL = RAT*DELT
          !
          WRITE(PRINT_HEAD(n)%IU,'(A)', ADVANCE='NO')
     +    "SP TS  "//NUM2STR(KPER)//"  "//NUM2STR(KSTP)//
     +    "  ITER "//NUM2STR(KITER)//"  DELT "//NUM2STR(DELT)//
     +    "  (NROW,NCOL,NLAY) = ("//NUM2STR(NROW)//", "//
     +                             NUM2STR(NCOL)//", "//
     +                             NUM2STR(NLAY)//")   "//
     +    "ERROR "//NUM2STR(ABS(BUDPERC))//" %  "//
     +    'FORMAT "'//PRINT_HEAD(n)%FMT//'"     '
          !
          IF(HAS_STARTDATE) THEN
              WRITE(PRINT_HEAD(n)%IU,'(A,4x)',ADVANCE='NO')
     +                  DATE_SP(KPER)%TS(KSTP-1)%STR_MONTHYEAR()
          END IF
          !
          DO K=1, NLAY
             WRITE(PRINT_HEAD(n)%IU,"(2A)") "LAY ",NUM2STR(K)
             DO I=1, NROW
                !
                HD(1:NCOL) = HNEW(1:NCOL,I,K)
                DO J=1, NCOL
                   IF    (HD(J) >=  1.D100) THEN
                                             HD(J) = inf
                   ELSEIF(HD(J) <= -1.D100) THEN
                                             HD(J) = ninf
                   ELSEIF(HD(J) == HDRY  .OR.
     +                    HD(J) /= HD(J) .OR. IBOUND(J,I,K) == Z) THEN
                                                          HD(J) = NaN
                   END IF
                END DO
                !
                WRITE(PRINT_HEAD(n)%IU, PRINT_HEAD(n)%FMT) HD
             END DO
          END DO
          !
        END IF
      END IF
      !
      ! Print Water Table Arrays if Requested ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      IF(PRINT_WTAB_FLAG /= 0) THEN !R, C, L, I, J, K
        !
        n  = 0
        !
        IF ( PRINT_WTAB_FLAG == 1 ) THEN
          BLOCK
              CHARACTER(8):: SPTS
              SPTS(1:4) = TRANSFER( KPER, SPTS(1:4) )     !  I = TRANSFER(c, I)  !GET SP
              SPTS(5:8) = TRANSFER( KSTP, SPTS(5:8) )     !  J = TRANSFER(PRINT_WTAB(n)%EXTRA(5:8), J)  !GET TS
              DO I=1, SIZE(PRINT_WTAB)
                 IF( SPTS == PRINT_WTAB(I)%EXTRA ) THEN
                                                   n = I
                                                   EXIT                ! Only one file per SP/TS
                 END IF
              END DO
          END BLOCK
        ELSEIF(PRINT_WTAB_FLAG == 2) THEN  ! Last Time Step
                 if(kstp==nstp(kper))  n = 1
        ELSEIF(PRINT_WTAB_FLAG == 3) THEN  ! Every Time Step
                                       n = 1
        END IF
        !
        IF ( n > 0 ) THEN
          !
          CALL ALLOC(HD, NCOL)
          !
          CALL PRINT_WTAB(n)%SIZE_CHECK()
          !
          K = -14  !NUM2STR Pad
          RAT = ABS(TOTRIN-TOTROT)
          VOL = RAT*DELT
          !
          WRITE(PRINT_WTAB(n)%IU,'(A)', ADVANCE='NO')
     +    "SP TS  "//NUM2STR(KPER)//"  "//NUM2STR(KSTP)//
     +    "  ITER "//NUM2STR(KITER)//"  DELT "//NUM2STR(DELT)//
     +    "  (NROW,NCOL) = ("//NUM2STR(NROW)//", "//
     +                         NUM2STR(NCOL)//")   "//
     +    "ERROR "//NUM2STR(ABS(BUDPERC))//" %  "//
     +    'FORMAT "'//PRINT_WTAB(n)%FMT//'"     '
          !
          IF(HAS_STARTDATE) THEN
              WRITE(PRINT_WTAB(n)%IU,'(A,4x)',ADVANCE='NO')
     +                  DATE_SP(KPER)%TS(KSTP-1)%STR_MONTHYEAR()
          END IF
          WRITE(PRINT_WTAB(n)%IU,"(A)") "WATER_TABLE"
          !
          DO I=1, NROW
             !
             HD(1:NCOL) = WTABLE(1:NCOL, I)
             DO J=1, NCOL
                IF    (HD(J) == HDRY  .OR.
     +                 HD(J) /= HD(J) .OR. HD(J) == -HUGE(HD)) THEN
                                          HD(J) = NaN
                ELSEIF(HD(J) >=  1D100) THEN
                                          HD(J) = inf
                ELSEIF(HD(J) <= -1D100) THEN
                                          HD(J) = ninf
                END IF
             END DO
             !
             WRITE(PRINT_WTAB(n)%IU, PRINT_WTAB(n)%FMT) HD
          END DO
          !
        END IF
      END IF
      !
      ! Print Depth to Water Arrays if Requested ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      IF(PRINT_WDEP_FLAG /= 0) THEN !R, C, L, I, J, K
        !
        n  = 0
        !
        IF ( PRINT_WDEP_FLAG == 1 ) THEN
          BLOCK
              CHARACTER(8):: SPTS
              SPTS(1:4) = TRANSFER( KPER, SPTS(1:4) )     !  I = TRANSFER(c, I)  !GET SP
              SPTS(5:8) = TRANSFER( KSTP, SPTS(5:8) )     !  J = TRANSFER(PRINT_WDEP(n)%EXTRA(5:8), J)  !GET TS
              DO I=1, SIZE(PRINT_WDEP)
                 IF( SPTS == PRINT_WDEP(I)%EXTRA ) THEN
                                                   n = I
                                                   EXIT                ! Only one file per SP/TS
                 END IF
              END DO
          END BLOCK
        ELSEIF(PRINT_WDEP_FLAG == 2) THEN  ! Last Time Step
                 if(kstp==nstp(kper))  n = 1
        ELSEIF(PRINT_WDEP_FLAG == 3) THEN  ! Every Time Step
                                       n = 1
        END IF
        !
        IF ( n > 0 ) THEN
          !
          CALL ALLOC(HD, NCOL)
          !
          CALL PRINT_WDEP(n)%SIZE_CHECK()
          !
          K = -14  !NUM2STR Pad
          RAT = ABS(TOTRIN-TOTROT)
          VOL = RAT*DELT
          !
          WRITE(PRINT_WDEP(n)%IU,'(A)', ADVANCE='NO')
     +    "SP TS  "//NUM2STR(KPER)//"  "//NUM2STR(KSTP)//
     +    "  ITER "//NUM2STR(KITER)//"  DELT "//NUM2STR(DELT)//
     +    "  (NROW,NCOL) = ("//NUM2STR(NROW)//", "//
     +                         NUM2STR(NCOL)//")   "//
     +    "ERROR "//NUM2STR(ABS(BUDPERC))//" %  "//
     +    'FORMAT "'//PRINT_WDEP(n)%FMT//'"     '
          !
          IF(HAS_STARTDATE) THEN
              WRITE(PRINT_WDEP(n)%IU,'(A,4x)',ADVANCE='NO')
     +                  DATE_SP(KPER)%TS(KSTP-1)%STR_MONTHYEAR()
          END IF
          WRITE(PRINT_WDEP(n)%IU,"(A)") "WATER_DEPTH"
          !
          DO I=1, NROW
             !
             HD(1:NCOL) = WTABLE(1:NCOL, I)
             DO J=1, NCOL
                IF    (HD(J) == HDRY  .OR.
     +                 HD(J) /= HD(J) .OR. HD(J) == -HUGE(HD)) THEN
                                          HD(J) = NaN
                ELSEIF(HD(J) >=  1D100) THEN
                                          HD(J) = inf
                ELSEIF(HD(J) <= -1D100) THEN
                                          HD(J) = ninf
                ELSE
                    HD(J) = GSE(J,I) - HD(J)
                END IF
             END DO
             !
             WRITE(PRINT_WDEP(n)%IU, PRINT_WDEP(n)%FMT) HD
          END DO
          !
        END IF
      END IF
      !
      ! Print Cumulative Head Change ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      IF(PRNT_CUM_HEAD_CHNG%IU /= 0) THEN
        !
        RAT = DZ  !HOLDS HEAD DIFFERENCE SUM
        DO K=1, NLAY
        DO I=1, NROW
        DO J=1, NCOL
        IF(IBOUND(J,I,K) > 0) THEN
                              RAT = RAT + ABS( HNEW(J,I,K)-HOLD(J,I,K) )
        END IF
        END DO
        END DO
        END DO
        !
        CUM_HEAD_CHNG = CUM_HEAD_CHNG + RAT
        !
        IF(CUM_HEAD_CHNG > D10) THEN                     !Sum exceeds 1E10m si shuft to integer counter
           CUM_HEAD_CHNG     = CUM_HEAD_CHNG - D10
           CUM_HEAD_CHNG_E10 = CUM_HEAD_CHNG_E10 + ONE
        END IF
        !
        IF(LAST_TS) THEN
                  I = PRNT_CUM_HEAD_CHNG%IU
            WRITE(I, "(F16.3, 3x, A)", ADVANCE="NO")
     +                 CUM_HEAD_CHNG, NUM2STR(CUM_HEAD_CHNG_E10)
            FLUSH(I)                                                     !Some reason intel does not write to a file when using ADVANCE='NO' -- compiler bug to be fixed
            !
            IF(CUM_HEAD_CHNG_E10 > 0) THEN
                                      WRITE(I,"(A)", ADVANCE="NO") "E10"
                                      FLUSH(I)
            END IF
            WRITE(I,"(A)", ADVANCE="NO") NL
            FLUSH(I)
        END IF
      END IF
C
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C5------WILL BE PRODUCED.
  120 IF(IDDREF /= 0) THEN
         IF(.NOT. ALLOC_DDREF) THEN
            ALLOCATE(DDREF(NCOL,NROW,NLAY))
            ALLOC_DDREF = TRUE
            CALL SGWF2BAS7PSV(IGRID)
         END IF
         DDREF=HNEW
           WRITE(IOUT,99)
   99    FORMAT(1X,'Drawdown Reference has been reset to the',
     1               ' end of this time step')
         IDDREF = Z
      END IF
      IF(IPFLG /= 0) THEN
      CALL SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
      WRITE(IOUT,*)
      END IF
      !
      IF(ALLOCATED(HD)) DEALLOCATE(HD, stat=i)  !Clean up the tmp array if allocated
      !
      CONTAINS
      !
      PURE FUNCTION IS_REAL64(x) RESULT(ANS)                  ! Could just do KIND(x)==REAL64
        USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
        CLASS(*), INTENT(IN):: x
        LOGICAL:: ANS
        !
        ANS = FALSE
        SELECT TYPE(x)
        TYPE IS(REAL(REAL64)); ANS = TRUE
        END SELECT
        !
      END FUNCTION
      !
      END SUBROUTINE
      !
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
      USE GWFBASMODULE,ONLY:REALTIM, USE_LEAP_YR,DATE_SP,
     +                      SIMTIM_PER, REALTIM_PER, TOTPERTIM
      USE BAS_UTIL,    ONLY: DELT_TO_DAY
      USE CONSTANTS,   ONLY: NL,TRUE,FALSE,UNO,SET_NAN, NEG, NEARZERO_5,
     +                       Z,ONE,TWO,THREE,FOUR,FIVE,DEGREE_SIGN,
     +                       DNEG, DZ, UNO, BLN
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR, WARNING_MESSAGE
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD, PARSE_WORD_UP
      USE UTIL_INTERFACE,       ONLY: NEAR_ZERO
      USE STRINGS,              ONLY: GET_INTEGER, GET_NUMBER, GET_DATE
      USE NUM2STR_INTERFACE,         ONLY: NUM2STR
      USE ULOAD_AND_SFAC_INTERFACE,  ONLY: ULOAD
      USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
      USE STRINGS,                   ONLY: UPPER
C
      TYPE(DATE_OPERATOR), INTENT(INOUT):: STARTING_DATE
      CHARACTER(*), INTENT(INOUT):: LINE
      CHARACTER(24):: ANAME(5)
      DOUBLE PRECISION:: XFIRSTCORD,YFIRSTCORD,GRIDROTATION             !seb ADDED VARIABLES TO READ IN LAY 1, ROW 1 ,COL 1 XY COORD
      LOGICAL:: PRINTCOORD,LLCOODRINATE,CORNERCOORD
      DOUBLE PRECISION:: SP_LEN, TS_MULT
      REAL:: ONE_SNG
      LOGICAL:: ERROR, FOUND_KEYWORD, EOF
      TYPE(DATE_OPERATOR):: DATE
      INTEGER:: INDIS
C     ------------------------------------------------------------------
      ANAME(1) = '                    DELR'
      ANAME(2) = '                    DELC'
      ANAME(3) = 'TOP ELEVATION OF LAYER 1'
      ANAME(4) = '  MODEL LAYER BOTTOM EL.'
      ANAME(5) = 'BOT. EL. OF QUASI-3D BED'
      ONE_SNG  = 1.0
C
C1------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS == Z) THEN
          CALL STOP_ERROR(OUTPUT=IOUT,MSG=
     + 'THE DIS PACKAGE WAS NOT DEFINED IN THE NAME FILE.'//
     + 'IT MUST BE DEFINED FOR OneWater TO RUN.')
      END IF
      WRITE(IOUT,11) INDIS
   11 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT ',I4)
C
C
C2------Read comments and the first line following the comments.
      CALL READ_TO_DATA(LINE,INDIS,IOUT,IOUT,
     +                            HED="-- READING DIS PACKAGE INPUT --",
     +                            EOF=EOF)
      IF(EOF) CALL EOF_ERROR(
     +             'Failed to read the first line. Is the file empty?')
C
C3------Get the number of layers, rows, columns, stress periods,
C3------ITMUNI, and LENUNI from the line.
      LLOC = ONE
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
      N = LLOC                                                      ! TEMP STORAGE OF LLOC
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,K,R,Z,INDIS)             ! READ IN X COORD
      IF (LINE(LEN(LINE):LEN(LINE)) /= 'E')THEN
          XFIRSTCORD=DBLE(R)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,K,R,IOUT,INDIS)        ! READ IN Y COORD
          YFIRSTCORD=DBLE(R)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,K,R,IOUT,INDIS)        ! READ IN ANGLE
          GRIDROTATION=DBLE(R)                                      ! GRIDROTATION EXPECTED TO BE A POLAR ANGLES. POLAR ANGLES ARE MEASURED COUNTERCLOCKWISE FROM THE POSITIVE X-AXIS IN DEGREES
          LLCOODRINATE=FALSE
          CORNERCOORD=FALSE
      ELSE
        XFIRSTCORD  = DZ
        YFIRSTCORD  = DZ
        GRIDROTATION= DZ
        !
        LLCOODRINATE= TRUE
        CORNERCOORD = TRUE
        !
        LLOC = N
        !
        WRITE(IOUT,'(/ A,/ 3A, / A /)')
     +   '*** DIS COORDINATE SYSTEM NOT SEPECIFIED ***',
     +   'DEFAULT VALUES APPLIED ARE 0',DEGREE_SIGN,' ROTATION',
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
     +   'GRID ROTATION OF '//NUM2STR(GRIDROTATION)//DEGREE_SIGN
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
      IF(ITMUNI < Z .OR. ITMUNI > 5) ITMUNI=Z
      IF(ITMUNI == Z) THEN
           WRITE(IOUT,30)
   30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI == 1) THEN
           WRITE(IOUT,40)
   40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI == 2) THEN
           WRITE(IOUT,50)
   50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI == 3) THEN
           WRITE(IOUT,60)
   60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI == 4) THEN
           WRITE(IOUT,70)
   70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
           WRITE(IOUT,80)
   80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI < Z .OR. LENUNI > 3) LENUNI=Z
      IF(LENUNI == Z) THEN
           WRITE(IOUT,90)
   90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI == 1) THEN
           WRITE(IOUT,91)
   91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI == 2) THEN
           WRITE(IOUT,93)
   93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI == 3) THEN
           WRITE(IOUT,95)
   95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C7------ALLOCATE LAYER FLAGS.
      ALLOCATE(LBOTM(NLAY))
      ALLOCATE(LAYCBD(NLAY))
C
C8------Read confining bed information
      CALL READ_TO_DATA(LINE,INDIS,IOUT,EOF=EOF)
      IF(EOF) CALL EOF_ERROR('Failed to read the LAYCBD line.')
      LLOC = ONE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      IF(LINE(ISTART:ISTOP)=='NOLAYCBD'  .OR.
     +   LINE(ISTART:ISTOP)=='NO_LAYCBD' .OR.
     +   LINE(ISTART:ISTOP)=='NO' ) THEN
          LAYCBD = Z
          WRITE(IOUT,'(/2A/)') ' NO_LAYCBD Keyword found.',
     +    ' No Quasi-3D confining beds defined for any layer'
      ELSE
          BACKSPACE(INDIS)
          READ(INDIS,*,IOSTAT=LLOC) (LAYCBD(K),K=1,NLAY)
          !
          IF(LLOC /= Z)
     +    CALL STOP_ERROR(LINE,INDIS,IOUT,' FAILED TO LOAD '//
     +     'LAYCBD. MAKE SURE YOU HAVE NLAY INTEGERS OR '//
     +     'THE KEYWORD "NO_LAYCBD" or "NOLAYCBD" (SGWF2BAS7ARDIS)')
          !
          LAYCBD(NLAY) = Z
          WRITE(IOUT,*) ' Confining bed flag for each layer:'
          WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
      END IF
C
C9------Count confining beds, setup the pointer to each layer's
C9------bottom array (LBOTM), and setup LAYCBD to be the confining
C9------bed number for each layer.
      NCNFBD = Z
      DO K=1,NLAY
        LBOTM(K)=K+NCNFBD
        IF(LAYCBD(K) /= Z) THEN
           NCNFBD=NCNFBD+1
           LAYCBD(K)=NCNFBD
        END IF
      END DO
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
      ALLOCATE (SPTIM(NPER))
C
C11-----Read the DELR and DELC arrays.
      CALL U1DREL(DELR,ANAME(1),NCOL,INDIS,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,INDIS,IOUT)
      !
      DO CONCURRENT(J=1:NCOL, I=1:NROW); AREA(J,I) = DELR(J)*DELC(I)
      END DO
      !
      CALL READ_TO_DATA(LINE,INDIS,IOUT,EOF=EOF)
      IF(EOF) CALL EOF_ERROR(
     +   'Failed to read the SURFACE_ELEVATION (optional) or TOP line.')
      LLOC = ONE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      IF(LINE(ISTART:ISTOP) == 'SURFACE' .OR.
     +   LINE(ISTART:ISTOP) == 'SURFACE_ELEVATION') THEN
          ALLOCATE (GSE(NCOL,NROW))
          K = Z
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
      DO K=1,NLAY
      KK=K
      !
      CALL U2DREL(BOTM(:,:,LBOTM(K)),ANAME(4),NROW,NCOL,KK,INDIS,IOUT)
      IF(LAYCBD(K) /= Z) CALL U2DREL(BOTM(:,:,LBOTM(K)+1),ANAME(5),
     1          NROW,NCOL,KK,INDIS,IOUT)
!      DO IC=1,NCOL                                                      !wschmid     !adjust bottom of any layer to top of layer 1 (taken to be ground surface elevation),
!      DO IR=1,NROW                                                      !wschmid     !if top of layer 1 < bottom of any layer
!       IF(BOTM(IC,IR,LBOTM(K)).GT.BOTM(IC,IR,0))                         !wschmid    SCOTT THIS DOES NOT MAKE SENSE
!     1  BOTM(IC,IR,LBOTM(K))=BOTM(IC,IR,0)                               !wschmid
!      ENDDO                                                             !wschmid
!      ENDDO                                                             !wschmid
      END DO
C
C14-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C14-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
        WRITE(IOUT,161)
  161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
     1            '     MULTIPLIER FOR DELT    SS FLAG  ',
     2            '(OPTIONAL TIME STEP LENGTH)',/1X,99('-'))
      ISS = Z
      ITR = Z
      REALTIM     = DNEG
      SIMTIM_PER  = DZ
      REALTIM_PER = DNEG
      USE_LEAP_YR = FALSE
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL READ_TO_DATA(LINE,INDIS,IOUT,IOUT,EOF=EOF)
      IF(EOF) CALL EOF_ERROR(
     +             'Failed to read the SURFACE_ELEVATION line.')
      LLOC = ONE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
      FOUND_KEYWORD = FALSE
      IF( LINE(ISTART:ISTOP) == 'DAILY'      ) THEN
          !
          FOUND_KEYWORD = TRUE
          TSMULT(:) = ONE_SNG
          !
          IF(ITMUNI == 1) THEN  ! SECONDS
               PERLEN(:) = 86400.
          ELSE IF(ITMUNI == 2) THEN  ! MINUTES
               PERLEN(:) = 1440.
          ELSE IF(ITMUNI == 3) THEN  ! HOURS
               PERLEN(:) = 24.
          ELSE IF(ITMUNI == 4) THEN  ! DAYS
               PERLEN(:) = 1.
          ELSE
               CALL STOP_ERROR(LINE,INDIS,IOUT,'FOUND KEYWORD '//
     +         '"DAILY", WHICH IS ONLY SUPPORTED FOR ITMUNI = 1, '//
     +         '2, 3, and 4.'//NL//
     +         'It does not supoport ITMUNI = 0 or 5 (years).')
          END IF
          !
          CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, N,
     +                     HAS_ERROR=ERROR)
          IF(ERROR) THEN
               N = 1
               IF( ISTART < ISTOP ) CALL UPPER(LINE(ISTART:ISTOP))
          ELSE
              CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
          END IF
          !
          NSTP(:) = N
          ISSFLG(:) = Z
          IF( LINE(ISTART:ISTOP) == 'SS' ) THEN
                ISSFLG(1) = ONE
                ISS       = ONE
                IF(NPER > 1) ITR = ONE
          ELSE
                ITR       = ONE
          END IF
          !
         WRITE (IOUT,'(9X,A21,I7,F25.5)',ADVANCE='NO') 'DAILY ',N,1.0
      ELSEIF( LINE(ISTART:ISTOP) == 'MONTHLY') THEN
          !
          FOUND_KEYWORD = TRUE
          !
          IF(ITMUNI < 1 .OR. ITMUNI > 4) THEN
             CALL STOP_ERROR(LINE,INDIS,IOUT,'FOUND KEYWORD '//
     +       '"MONTHLY", WHICH IS ONLY SUPPORTED FOR ITMUNI = 1, '//
     +       '2, 3, and 4.'//NL//
     +       'It does not supoport ITMUNI = 0 or 5 (years).')
          END IF
          !
          CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, N,
     +          MSG='DIS found MONTHLY keyword but failed to load NSTP')
          !
          SPTIM(:)%SPECIFY_DELT = N < 0
          NSTP(:) = ABS(N)
          TSMULT(:) = ONE_SNG
          !
          CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
          IF( .NOT. LINE(ISTART:ISTOP) == 'SS' .AND.
     +        .NOT. LINE(ISTART:ISTOP) == 'TR' ) LLOC = ISTART
          !
          CALL GET_DATE(LINE,LLOC,I,J,IOUT,INDIS, DATE, HAS_ERROR=ERROR)        ! I and J are place holders for ISTART and ISTOP
          !
          IF(STARTING_DATE%NOT_SET() ) THEN
             IF( ERROR ) THEN
               CALL STOP_ERROR(LINE,INDIS,IOUT, MSG=
     +        'DIS package found "MONTHLY" keyword, '//NL//
     +'but had no starting date to determine month lengths.'//BLN//
     +'You must define a simulation starting date as one '//
     +'of the following:'//NL//
     +'  BAS "START_DATE" option followed by the starting date or'//NL//
     +'  Specifying a starting date after the MONTHLY keyword.'//BLN//
     +'For Example, add to the BAS package:'//BLN//
     +'BEGIN OPTIONS'//NL//'    START_DATE 4/23/1979'//NL//
     +'END OPTIONS'//BLN//'or'//BLN//
     +'MONTHLY  4/23/1979   2'//BLN//
     +'where 4/23/1979 represents the start of the simulation'//NL//
     +'(that is, simulation time = 0)'//BLN//
     +'Note, the date must contain either a "/" or "-"' )
             END IF
             STARTING_DATE = DATE
          END IF
          !
          IF( .NOT. LINE(ISTART:ISTOP) == 'SS' .AND.
     +        .NOT. LINE(ISTART:ISTOP) == 'TR' ) 
     +              CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
          !
          ISSFLG(:) = Z
          IF( LINE(ISTART:ISTOP) == 'SS' ) THEN
                ISSFLG(1) = ONE
                ISS       = ONE
                IF(NPER > 1) ITR = ONE
          ELSE
                ITR       = ONE
          END IF
          !
          WRITE (IOUT,'(9X,A21,I7,F25.5)',ADVANCE='NO') 'MONTHLY ',N,1.0
          !
          BLOCK
             TYPE(DATE_OPERATOR):: DSTP
             !
             DSTP       = STARTING_DATE
             DSTP%FRAC  = DZ
             CALL DSTP%SET_DAY(1)
             CALL DSTP%ADD_MONTH(1)
             !
             SP_LEN = DSTP - STARTING_DATE ! Fix first period to match month, so if started on 1/15/2000,
             !                                              then first period is 16 days to get to Feb 1
             IF(ITMUNI == 1) THEN  ! SECONDS
                  PERLEN(1) = 86400. * SP_LEN
             ELSE IF(ITMUNI == 2) THEN  ! MINUTES
                  PERLEN(1) = 1440. * SP_LEN
             ELSE IF(ITMUNI == 3) THEN  ! HOURS
                  PERLEN(1) = 24. * SP_LEN
             ELSE IF(ITMUNI == 4) THEN  ! DAYS
                  PERLEN(1) = SP_LEN
             END IF
             !
             DO N=2,NPER
                    SP_LEN = DSTP%MONTHDAYS()
                    !
                    IF(ITMUNI == 1) THEN  ! SECONDS
                         PERLEN(N) = 86400. * SP_LEN
                    ELSE IF(ITMUNI == 2) THEN  ! MINUTES
                         PERLEN(N) = 1440. * SP_LEN
                    ELSE IF(ITMUNI == 3) THEN  ! HOURS
                         PERLEN(N) = 24. * SP_LEN
                    ELSE IF(ITMUNI == 4) THEN  ! DAYS
                         PERLEN(N) = SP_LEN
                    END IF
                    !
                    CALL DSTP%ADD_MONTH(1)
             END DO
          END BLOCK
      END IF
      !
      IF(FOUND_KEYWORD) THEN
!        IF( ITMUNI /= FOUR ) CALL STOP_ERROR(LINE,INDIS,IOUT,
!     +      'DIS PACKAGE FOUND MONTHLY OR DAILY KEYWORD, BUT '//
!     +      'THESE ONLY WORK WITH ITMUNI=4 (DAYS).')
        IF(ISS == Z .AND. ITR /= Z) THEN
                                     WRITE (IOUT,'(A)') ' TR'
        ELSE IF(ISS /= Z .AND. ITR == Z) THEN
                                     WRITE (IOUT,'(A)') ' SS'
        ELSE
                      WRITE (IOUT,'(A)') ' First SS, then TR'
        END IF
        !
        FOUND_KEYWORD = N < 1  ! Now use as flag to indicate if time steps should try to be int numbers
        !
        DO N=1,NPER
          ALLOCATE( SPTIM(N)%DT( NSTP(N) ) )
        END DO
        !
        if(NSTP(1) == 1) then
                DO N=1,NPER
                       SPTIM(N)%DT(1) = DBLE(PERLEN(N))
                END DO
        elseif(SPTIM(1)%SPECIFY_DELT) then
            DO N=1,NPER
               SPTIM(N)%DT    = DZ
               SPTIM(N)%DT(1) = DBLE(PERLEN(N))
               CALL REDISTRIBUTE_SUM_TO_NATURAL_NUMBERS( SPTIM(N)%DT )
            END DO
        else
              DO N=1,NPER
                     SPTIM(N)%DT = DBLE(PERLEN(N)) / DBLE(NSTP(N))
              END DO
        end if
        !
        GO TO 999  ! Skip past regular DIS input read
      END IF
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      DO N=1,NPER
      IF(N>1) CALL READ_TO_DATA(LINE,INDIS,IOUT,IOUT,EOF=EOF)
      IF(EOF) CALL EOF_ERROR(
     +             'Failed to read the PERLEN line.'//NL//
     +             'Input expects to read NPER lines.'//NL//
     +'Input Read '//NUM2STR(N-1)//' lines, when it expected '//
     +NUM2STR(NPER)//' lines.')
      
      LLOC = ONE
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, SP_LEN,
     +                 MSG='DIS FAILED TO LOAD PERLEN')
      PERLEN(N) = SP_LEN
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, NSTP(N),
     +                 MSG='DIS FAILED TO LOAD NSTP')
      !
      CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
      IF( LINE(ISTART:ISTOP) == '1'    .OR.
     +    LINE(ISTART:ISTOP) == '1.'   .OR.
     +    LINE(ISTART:ISTOP) == '1.0'  .OR.
     +    LINE(ISTART:ISTOP) == '1.00'  ) THEN
          TSMULT(N) = ONE_SNG
          TS_MULT   = UNO
      ELSE
         CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS, TS_MULT,
     +                   NO_PARSE_WORD=TRUE,
     +                   MSG='DIS FAILED TO LOAD TSMULT')
          TSMULT(N) = SNGL(TS_MULT)
         IF(ABS(TSMULT(N)-UNO) < NEARZERO_5) THEN
             TSMULT(N) = ONE_SNG
             TS_MULT   = UNO
         END IF
      END IF
      !
      IF (TSMULT(N) < NEARZERO_5) THEN
           TSMULT(N) = ONE_SNG
           TS_MULT   = UNO
!           WRITE(IOUT,170)
!  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         CALL WARNING_MESSAGE(LINE,INDIS,IOUT,
     +                'TSMULT<=0.0, IT IS RESET TO 1.0', INLINE=TRUE)
      END IF
      !
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
      IF (LINE(ISTART:ISTOP) == 'TR') THEN
         ISSFLG(N) = Z
         ITR       = ONE
      ELSE IF (LINE(ISTART:ISTOP) == 'SS') THEN
         ISSFLG(N) = ONE
         ISS       = ONE
      ELSE
!           WRITE(IOUT,162)
!  162    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
!     1      ' -- STOP EXECUTION (SGWF2BAS7ARDIS)')
      CALL STOP_ERROR(LINE,INDIS,IOUT,' SSFLAG MUST BE '//
     + 'EITHER "SS" OR "TR" -- STOP EXECUTION (SGWF2BAS7ARDIS)')
      END IF
      IF(NSTP(N) < Z) THEN
          NSTP(N) = ABS(NSTP(N))
          SPTIM(N)%SPECIFY_DELT = TRUE
          TSMULT(N) = UNO
          ALLOCATE( SPTIM(N)%DT( NSTP(N) ) )
          DO I=1, NSTP(N)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,Z,K,R,IOUT,INDIS)  !NOT USING GET_NUMBER TO SAVE ON NUM2STR EVALUATION
              READ(LINE(ISTART:ISTOP), *, IOSTAT=K) SPTIM(N)%DT(I)
              !
              IF(K /= Z)CALL STOP_ERROR( LINE, INDIS,
     +         OUTPUT=IOUT,MSG= 'FOUND NEGATIVE NSTP, WHICH INDICATES'//
     +' THAT TIME STEP LENGTHS WILL BE SPECIFIED. FAILED TO LOAD THE '//
     +  NUM2STR(I)// ' TIME STEP OF '//NUM2STR(NSTP(N))//' TIME STEPS.')
          END DO
          PERLEN(N) = SUM(SPTIM(N)%DT)
      ELSE
          ALLOCATE( SPTIM(N)%DT( NSTP(N) ) )
          !
          IF( TS_MULT  /=  UNO) THEN
             SPTIM(N)%DT(1)= SP_LEN*(UNO-TS_MULT)/(UNO-TS_MULT**NSTP(N))
             DO I=2,NSTP(N)
                           SPTIM(N)%DT(I) = SPTIM(N)%DT(I-1)*TS_MULT
             END DO
          ELSE
              SPTIM(N)%DT=SP_LEN/DBLE(NSTP(N))
          END IF
      END IF
      !
      IF(N == ONE) THEN                                                ! ADDED ABILIITY TO HAVE REAL TIME PASS THROUGH MODEL
        K =ISTART                                                      ! TEMP STORAGE OF ISTART AND ISTOP
        KK=ISTOP
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
        IF (LINE(ISTART:ISTOP) == 'STARTDATE'  .OR.
     +      LINE(ISTART:ISTOP) == 'START_DATE' .OR.
     +      LINE(ISTART:ISTOP) == 'DATE_START' .OR.
     +      LINE(ISTART:ISTOP) == 'DATESTART'     ) THEN
            !
            CALL GET_DATE(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS,
     +                  STARTING_DATE,MSG=
     +        'DIS PACKAGE FOUND "START_DATE" KEYWORD, '//
     +        'BUT FAILED TO READ THE STARTING DATE AFTER THE KEYWORD.')
        END IF
        IF(LINE(ISTART:ISTOP) == 'LEAPYEARS' .OR.
     +     LINE(ISTART:ISTOP) == 'LEAPYEAR') THEN
          USE_LEAP_YR=TRUE
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
        END IF
        IF(LINE(ISTART:ISTOP) == 'STARTTIME')THEN
          CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,INDIS,REALTIM,
     -                                  MSG='ERROR READING STARTTIME')
          REALTIM_PER = REALTIM
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
  162 FORMAT(1X,I8,G21.7,I7,A25,  A11,4x,*(G21.7))
  163 FORMAT(1X,I8,G21.7,I7,F25.5,A11)
C
C15-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
C15-----TSMULT LE 0, OR PERLEN LT 0.
      IF(NSTP(N) <= Z) THEN
!           WRITE(IOUT,164)
!  164    FORMAT(1X,/1X,
!     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=
     +'NSTP=0!?!? YOU MUST SPECIFY A NON-ZER0 TIME STEP COUNT.')
      END IF
      ZERO=0.
      IF(PERLEN(N) == DZ .AND. ISSFLG(N) == Z) THEN
!           WRITE(IOUT,165)
!  165    FORMAT(1X,/1X,
!     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=
     +   'PERLEN=0!?!? YOU MUST SPECIFY A NON-ZER0 PERIOD LENGTH FOR '//
     +   '"TR" STRESS PERIODS.')
      END IF
      IF(PERLEN(N) < DZ) THEN
         CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=
     +   'PERLEN<0!?!? YOU MUST SPECIFY A POSTIVE, NON-ZER0 PERIOD '//
     +   'LENGTH FOR ALL STRESS PERIODS.')
      END IF
      END DO
C
C16-----Assign ITRSS.
  999 IF(ISS == Z .AND. ITR /= Z) THEN
         ITRSS = ONE
           WRITE(IOUT,270)
  270    FORMAT(/,1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS /= Z .AND. ITR == Z) THEN
         ITRSS = Z
           WRITE(IOUT,275)
  275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS = NEG
           WRITE(IOUT,280)
  280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
      !
C--------------ALLOCATE AND BUILD COORDINATE SYSTEM OF MODEL AND OPTIONALLY PRINT seb
      !
      XYGRID=>XY_GRID_COODINATES(NROW,NCOL)
      !
      CALL XYGRID%BUILD(XFIRSTCORD,YFIRSTCORD,GRIDROTATION,
     +                  LLCOODRINATE,CORNERCOORD,DELR,DELC)
      !
      IF(PRINTCOORD) CALL XYGRID%PRINT(IOUT)
C
      CONTAINS
         !
         SUBROUTINE EOF_ERROR(MSG)
           ! IMPORT:: INDIS, LINE, IOUT, NL   ! currently not supported by gfortran
           CHARACTER(*), INTENT(IN):: MSG
           CHARACTER(:), ALLOCATABLE:: ERR
           !
           BACKSPACE(INDIS)
           READ(IN, '(A)') LINE
           !
           ERR = 'Reached end of file (eof) when reading the '//
     +           'next input line.'//NL//NL
           IF(LINE /= '') ERR=ERR//
     +       'The guessed line is the previous line read.'//NL//NL
           IF(MSG /= '') ERR=ERR//
     +       'The following is an additional note to the '//
     +       'error routine:'//NL//NL//MSG
           !
           CALL STOP_ERROR(LINE,INFILE=INDIS,OUTPUT=IOUT,MSG=ERR)
           !  
         END SUBROUTINE
         !
         PURE SUBROUTINE REDISTRIBUTE_SUM_TO_NATURAL_NUMBERS(val)
           implicit none
           double precision, dimension(:), intent(inout):: val
           double precision:: tot, div, N
           integer:: i, dim
           !
           dim = size(val)
           N   = REAL(dim, kind(N))
           tot = sum(val)
           !
           if ( tot <= N ) then           ! Sum is too small for whole number split
                           val = tot/N
                           return
           end if
           !
           div = tot/N
           div = AINT(div, kind(div))
           val(1) = div
           !
           tot = tot - div
           N   = N - 1.0d0
           do i=2, dim-1
                         div = tot/N
                         div = AINT(div, kind(div))
                         val(i) = div
                         !
                         tot = tot - div
                         N   = N - 1.0d0
           end do
           val(dim) = tot
           !  
         END SUBROUTINE
         !
      END SUBROUTINE
      !
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
      CHARACTER(16):: TEXT
      DOUBLE PRECISION:: SSTRT
C
      TEXT = '        DRAWDOWN'
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS DRAWDOWN NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC /= 0) KL=1
      IF(IOFLG(KL,2) == 0 .AND. IOFLG(KL,4) == 0) GO TO 59
C
C4------CALCULATE DRAWDOWN FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=HNEW(J,I,K)
      SSTRT=DDREF(J,I,K)
      IF(IBOUND(J,I,K) /= 0) BUFF(J,I,K)=SSTRT-HNEW(J,I,K)
   58 CONTINUE
   59 CONTINUE
C
C5------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE PRINTED.
C5------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.
      IF(ISA /= 0) THEN
         IF(IXSEC == 0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,2) == 0) GO TO 69
           IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,-IDDNFM,IOUT)
           IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,IDDNFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C5A-----PRINT DRAWDOWN FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,2) /= 0) THEN
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
      IF(IDDNUN.LE.0) RETURN ! GO TO 80
      IF(IXSEC == 0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,4) == 0) GO TO 79
          IF(IFIRST == 1) WRITE(IOUT,74) IDDNUN,KSTP,KPER
   74   FORMAT(1X,/1X,'DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I5,' IN STRESS PERIOD ',I6)
        IFIRST=0
        IF(CDDNFM == ' ') THEN
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
        IF(IOFLG(1,4) /= 0) THEN
            WRITE(IOUT,74) IDDNUN,KSTP,KPER
          IF(CDDNFM == ' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IDDNUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IDDNUN,CDDNFM,LBDDSV,IBOUND)
          END IF
        END IF
      END IF
C
      END SUBROUTINE
      !
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
      CHARACTER(16):: TEXT
      TEXT = '            HEAD'
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC /= 0) KL=1
      IF(IOFLG(KL,1) == 0 .AND. IOFLG(KL,3) == 0) GO TO 59
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
      IF(ISA /= 0) THEN
         IF(IXSEC == 0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,1) == 0) GO TO 69
           IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,-IHEDFM,IOUT)
           IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,IHEDFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,1) /= 0) THEN
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
      IF(IHEDUN.LE.0) RETURN 
      IF(IXSEC == 0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,3) == 0) GO TO 79
          IF(IFIRST == 1) WRITE(IOUT,74) IHEDUN,KSTP,KPER
   74   FORMAT(1X,/1X,'HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I5,' IN STRESS PERIOD ',I6)
        IFIRST=0
        IF(CHEDFM == ' ') THEN
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
        IF(IOFLG(1,3) /= 0) THEN
            WRITE(IOUT,74) IHEDUN,KSTP,KPER
          IF(CHEDFM == ' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IHEDUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IHEDUN,CHEDFM,LBHDSV,IBOUND)
          END IF
        END IF
      END IF
C
      END SUBROUTINE
      !
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
      CHARACTER(16):: TEXT
      TEXT = '          IBOUND'
C     ------------------------------------------------------------------
      IF(IBOUUN.LE.0) RETURN
C
C5------FOR EACH LAYER: SAVE IBOUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC == 0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,5) == 0) GO TO 79
          IF(IFIRST == 1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I5,' IN STRESS PERIOD ',I6)
        IFIRST=0
        CALL ULASV3(IBOUND(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IBOUUN,CBOUFM,LBBOSV)
   79   CONTINUE
C
C5A-----SAVE IBOUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,5) /= 0) THEN
            WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3(IBOUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IBOUUN,CBOUFM,LBBOSV)
        END IF
      END IF
C
      END SUBROUTINE
      !
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
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD_UP
      USE CONSTANTS, ONLY: Z, ONE, NEG, DZ, BLNK
      CHARACTER(768):: LINE
      LOGICAL:: NOFREE
      NOFREE = IFREFM == Z
C     ------------------------------------------------------------------
C
C1-----ALLOCATE SPACE FOR IOFLG, VBVL, AND VBNM ARRAYS.
      ALLOCATE (IOFLG(NLAY,5))
      ALLOCATE (VBVL(4,MXBUD))
      ALLOCATE (VBNM(MXBUD))
      ALLOCATE(PVOL_ERR)
      PVOL_ERR  = DZ
      IDDREF    = Z
      IDDREFNEW = Z
      DO CONCURRENT (K=1:MXBUD)
                         VBNM(k) = ""
      END DO
C
C1------ASSIGN DEFAULT VALUES.
      CHEDFM = BLNK
      CDDNFM = BLNK
      CBOUFM ='(20I4)'
      IHEDFM = Z
      IDDNFM = Z
      IHEDUN = Z
      IDDNUN = Z
      IBOUUN = Z
      !IBDOPT = ONE
      LBHDSV = Z
      LBDDSV = Z
      LBBOSV = Z
      IAUXSV = Z
      IUBGT  = Z
C
C2------TEST OUTPUT CONTROL INPUT UNIT TO SEE IF OUTPUT CONTROL IS
C2------ACTIVE.
      IF(INOC == Z) THEN
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
         DO K=1,NLAY
         IOFLG(K,1) = ONE
         IOFLG(K,2) = Z
         IOFLG(K,3) = Z
         IOFLG(K,4) = Z
         IOFLG(K,5) = Z
         END DO
         RETURN
      END IF
C
C3------OUTPUT CONTROL IS ACTIVE.  READ FIRST RECORD AND DECODE FIRST
C3------WORD.  MUST USE URWORD IN CASE FIRST WORD IS ALPHABETIC.
      CALL READ_TO_DATA(LINE,INOC,IOUT,IOUT, NOSHIFT=NOFREE,
     +                            HED="-- READING OC PACKAGE INPUT --")
      LLOC=1
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
C
C4------TEST FOR NUMERIC OUTPUT CONTROL.  FIRST WORD WILL NOT BE
C4------"PERIOD", "HEAD", "DRAWDOWN", OR "COMPACT".
      IRESTART   = Z
      KPERSTART  = Z
      KSTPSTART  = Z
      IUNITSTART = Z
      ITST       = Z
      IF(LINE(ISTART:ISTOP) == 'RESTART' ) THEN
        IRESTART = ONE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KPERSTART,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KSTPSTART,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITSTART,R,IOUT,IN)
        IF ( KPERSTART <= Z .OR. KSTPSTART <= Z ) THEN
          IRESTART = Z
        END IF
        IF ( IRESTART > Z ) THEN
            WRITE(IOUT,100)KPERSTART,KSTPSTART
  100     FORMAT(1X,/1X,'RESTART OPTION ACTIVE. RESTART AT PERIOD ',I6,
     +          'STEP ',I5)
            WRITE(IOUT,101)IUNITSTART
  101     FORMAT(1X,/1X,'RESTART HEADS WILL BE READ FROM UNIT ',I5)
        END IF
      END IF
      IF ( IRESTART > Z ) THEN
        CALL READ_TO_DATA(LINE,INOC,IOUT, NOSHIFT=NOFREE)
        LLOC=1
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      END IF
      IF(LINE(ISTART:ISTOP) /= 'PERIOD' .AND. LINE(ISTART:ISTOP) /=
     1     'HEAD' .AND. LINE(ISTART:ISTOP) /= 'DRAWDOWN' .AND.
     2     LINE(ISTART:ISTOP) /= 'COMPACT' .AND.
     3     LINE(ISTART:ISTOP) /= 'IBOUND') THEN
C4A-----NUMERIC OUTPUT CONTROL.  DECODE THE INITIAL RECORD ACCORDINGLY.
           WRITE(IOUT,102)
  102    FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED EVERY TIME STEP')
         IF(NOFREE) THEN
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
         IPEROC = NEG
         ITSOC  = NEG
      ELSE
C4B-----ALPHABETIC OUTPUT CONTROL.  CALL MODULE TO READ INITIAL RECORDS.
         CALL SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
      END IF
C
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
C     ******************************************************************
C     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD, PARSE_WORD_UP
      USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1                        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2                        IBOUUN,LBBOSV,CBOUFM,IAUXSV,IDDREFNEW,
     3                        IUBGT
C
      CHARACTER(*):: LINE
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
  100 IF(LINE(ISTART:ISTOP) == 'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) /= 'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
           WRITE(IOUT,101) IHEDFM,IDDNFM
  101    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1        '    DRAWDOWN PRINT FORMAT CODE IS',I4)
           WRITE(IOUT,102) IHEDUN,IDDNUN
  102    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,
     1        '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4,
     2        '  WATER BUDGETs WILL BE SAVED ON UNIT ',I4)
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) == 'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         GO TO 1000
C
C2B-----LOOK FOR "HEAD PRINT ..." AND "HEAD SAVE ...".  IF
C2B-----FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP) == 'HEAD') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) == 'PRINT') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) /= 'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP) == 'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) == 'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,
     1            INOC)
            ELSE IF(LINE(ISTART:ISTOP) == 'FORMAT') THEN
               CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
               CHEDFM=LINE(ISTART:ISTOP)
                 WRITE(IOUT,103) CHEDFM
  103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
               CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
               IF(LINE(ISTART:ISTOP) == 'LABEL') THEN
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
      ELSE IF(LINE(ISTART:ISTOP) == 'DRAWDOWN') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) == 'PRINT') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) /= 'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP) == 'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) == 'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,
     1                   INOC)
            ELSE IF(LINE(ISTART:ISTOP) == 'FORMAT') THEN
               CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
               CDDNFM=LINE(ISTART:ISTOP)
                 WRITE(IOUT,105) CDDNFM
  105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
               CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
               IF(LINE(ISTART:ISTOP) == 'LABEL') THEN
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
      ELSE IF(LINE(ISTART:ISTOP) == 'COMPACT') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) == 'BUDGET') THEN
            IBDOPT=2
              WRITE(IOUT,107)
  107       FORMAT(1X,
     1      'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) == 'AUXILIARY' .OR.
     1         LINE(ISTART:ISTOP) == 'AUX') THEN
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
      ELSE IF(LINE(ISTART:ISTOP) == 'IBOUND') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) == 'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) == 'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBOUUN,R,IOUT,
     1            INOC)
                 WRITE(IOUT,111) IBOUUN
  111          FORMAT(1X,'IBOUND WILL BE SAVED ON UNIT ',I4)
            ELSE IF(LINE(ISTART:ISTOP) == 'FORMAT') THEN
               CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
               CBOUFM=LINE(ISTART:ISTOP)
                 WRITE(IOUT,112) CBOUFM
  112          FORMAT(1X,'IBOUND WILL BE SAVED WITH FORMAT: ',A)
               CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
               IF(LINE(ISTART:ISTOP) == 'LABEL') THEN
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
      ELSE IF(LINE(ISTART:ISTOP) == 'WBGT') THEN
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF(LINE(ISTART:ISTOP) == 'SAVE') THEN
            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
            IF(LINE(ISTART:ISTOP) == 'UNIT') THEN
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
      IF(LINE == ' ') GO TO 110
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
      !
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
      IF(ITMUNI == 1) CNV=1.
      IF(ITMUNI == 2) CNV=60.
      IF(ITMUNI == 3) CNV=3600.
      IF(ITMUNI == 4) CNV=86400.
      IF(ITMUNI == 5) CNV=31557600.
C
C2------IF FACTOR=0 THEN TIME UNITS ARE NON-STANDARD.
      IF(CNV /= ZERO) GO TO 100
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
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,IUBGT,
     +                                   BUDPERC,TOTRIN,TOTROT,PVOL_ERR)
C     ******************************************************************
C     PRINT VOLUMETRIC BUDGET
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER(16):: VBNM(MSUM)
      DIMENSION VBVL(4,MSUM)
      CHARACTER(17):: VAL1,VAL2
C     ------------------------------------------------------------------
C
C1------DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.
      ETOL = 0.001
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
      IF(VBVL(1,L) /= ZERO .AND.
     1       (VBVL(1,L).GE.BIGVL1 .OR. VBVL(1,L).LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') VBVL(1,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVL(1,L)
      END IF
      IF(VBVL(3,L) /= ZERO .AND.
     1       (VBVL(3,L).GE.BIGVL1 .OR. VBVL(3,L).LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') VBVL(3,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVL(3,L)
      END IF
      WRITE(IOUT,275) VBNM(L),VAL1,VBNM(L),VAL2
      If(IUBGT>0) WRITE(IUBGT,275) VBNM(L),VAL1,VBNM(L),VAL2
  200 CONTINUE
      IF(TOTVIN /= ZERO .AND.
     1      (TOTVIN.GE.BIGVL1 .OR. TOTVIN.LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') TOTVIN
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVIN
      END IF
      IF(TOTRIN /= ZERO .AND.
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
      IF(VBVL(2,L) /= ZERO .AND.
     1       (VBVL(2,L).GE.BIGVL1 .OR. VBVL(2,L).LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') VBVL(2,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVL(2,L)
      END IF
      IF(VBVL(4,L) /= ZERO .AND.
     1       (VBVL(4,L).GE.BIGVL1 .OR. VBVL(4,L).LT.SMALL)) THEN
         WRITE(VAL2,'(ES17.4)') VBVL(4,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVL(4,L)
      END IF
      WRITE(IOUT,275) VBNM(L),VAL1,VBNM(L),VAL2
      If(IUBGT>0) WRITE(IUBGT,275) VBNM(L),VAL1,VBNM(L),VAL2
  250 CONTINUE
      IF(TOTVOT /= ZERO .AND.
     1      (TOTVOT.GE.BIGVL1 .OR. TOTVOT.LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') TOTVOT
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVOT
      END IF
      IF(TOTROT /= ZERO .AND.
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
      IF(AVGRAT > 1.0E-30) PDIFFR=HUND*DIFFR/AVGRAT
      !
      IF( ADIFFR < ETOL .AND.      AVGRAT < ETOL 
     +                  .AND. ABS(PDIFFR) > ETOL ) THEN
                         if(PDIFFR < 0.0) THEN
                            PDIFFR = -ETOL
                         else
                            PDIFFR =  ETOL
                         end if
      END IF
      !
      BUDPERC=PDIFFR
C
C7C-----CALCULATE DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      DIFFV=TOTVIN-TOTVOT
      ADIFFV=ABS(DIFFV)
C
C7D-----GET PERCENT DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      PDIFFV=ZERO
      AVGVOL=(TOTVIN+TOTVOT)/TWO
      IF(AVGVOL > 1.0E-30) PDIFFV=HUND*DIFFV/AVGVOL
      !
      IF( ADIFFV < ETOL .AND.      AVGVOL < ETOL 
     +                  .AND. ABS(PDIFFV) > ETOL ) THEN
                         if(PDIFFV < 0.0) THEN
                            PDIFFV = -ETOL
                         else
                            PDIFFV =  ETOL
                         end if
      END IF
      !
      PVOL_ERR = PDIFFV
C
C8------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT
C8------AND OUTPUT RATES AND VOLUMES.
      IF(ADIFFV /= ZERO .AND.
     1      (ADIFFV.GE.BIGVL2 .OR. ADIFFV.LT.SMALL)) THEN
         WRITE(VAL1,'(ES17.4)') DIFFV
      ELSE
         WRITE(VAL1,'(F17.4)') DIFFV
      END IF
      IF(ADIFFR /= ZERO .AND.
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
      !
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
      ETOL = 0.001
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
      ADIFFR=ABS(DIFFR)
C
C7B-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR=ZERO
      AVGRAT=(TOTRIN+TOTROT)/TWO
      IF( AVGRAT > 1.0E-30) PDIFFR=HUND*DIFFR/AVGRAT
      !
      IF( ADIFFR < ETOL .AND.      AVGRAT < ETOL 
     +                  .AND. ABS(PDIFFR) > ETOL ) THEN
                         if(PDIFFR < 0.0) THEN
                            PDIFFR = -ETOL
                         else
                            PDIFFR =  ETOL
                         end if
      END IF
      BUDPERC=PDIFFR
C
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7N(KPER,KSTP,INOC,IOUT,NLAY)
C     ******************************************************************
C     SET OUTPUT FLAGS USING ALPHABETIC OUTPUT CONTROL INPUT STRUCTURE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IOFLG,IHDDFL,IBUDFL,ICBCFL,IPEROC,
     1                        ITSOC,IBDOPT,IDDREF,IDDREFNEW
      USE ERROR_INTERFACE,  ONLY: STOP_ERROR
C
      CHARACTER(768):: LINE
C     ------------------------------------------------------------------
C
C1------ERROR IF OUTPUT CONTROL TIME STEP PRECEDES CURRENT SIMULATION
C1------TIME STEP.
      IF((IPEROC.LT.KPER).OR.(IPEROC == KPER .AND. ITSOC.LT.KSTP)) THEN
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
      IF(IPEROC /= KPER .OR. ITSOC /= KSTP) THEN
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
        IF(IDDREFNEW /= 0) WRITE(IOUT,52)
   52      FORMAT(1X,'Drawdown Reference will be reset at the',
     1               ' end of this time step')
C
C4A-----OUTPUT CONTROL MATCHES SIMULATION TIME.  READ NEXT OUTPUT
C4A-----RECORD; SKIP ANY BLANK LINES.
50    READ(INOC,'(A)',END=1000) LINE
      IF(LINE == ' ') GO TO 50
C
C4A1----LOOK FOR "PERIOD", WHICH TERMINATES OUTPUT CONTROL FOR CURRENT
C4A1----TIME STEP.  IF FOUND, DECODE TIME STEP FOR NEXT OUTPUT.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
      IF(LINE(ISTART:ISTOP) == 'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP) /= 'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP) == 'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         RETURN
C
C4A2----LOOK FOR "PRINT", WHICH MAY REFER TO "BUDGET", "HEAD", OR
C4A2----"DRAWDOWN".
      ELSE IF(LINE(ISTART:ISTOP) == 'PRINT') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP) == 'BUDGET') THEN
              WRITE(IOUT,53)
53          FORMAT(4X,'PRINT BUDGET')
            IBUDFL=1
         ELSE IF(LINE(ISTART:ISTOP) == 'HEAD') THEN
            CALL SGWF2BAS7L(1,LINE,LLOC,IOFLG,NLAY,IOUT,'PRINT HEAD',
     1              INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP) == 'DRAWDOWN') THEN
            CALL SGWF2BAS7L(2,LINE,LLOC,IOFLG,NLAY,IOUT,
     1              'PRINT DRAWDOWN',INOC)
            IHDDFL=1
         ELSE
            GO TO 2000
         END IF
C
C4A3----LOOK FOR "SAVE", WHICH MAY REFER TO "BUDGET", "HEAD",
C4A3----"DRAWDOWN", OR "IBOUND".
      ELSE IF(LINE(ISTART:ISTOP) == 'SAVE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP) == 'BUDGET') THEN
              WRITE(IOUT,57)
57          FORMAT(4X,'SAVE BUDGET')
            ICBCFL=IBDOPT
         ELSE IF(LINE(ISTART:ISTOP) == 'HEAD') THEN
            CALL SGWF2BAS7L(3,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE HEAD',
     &                      INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP) == 'DRAWDOWN') THEN
            CALL SGWF2BAS7L(4,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE DRAWDOWN',
     1          INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP) == 'IBOUND') THEN
            CALL SGWF2BAS7L(5,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE IBOUND',
     1                     INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP) == 'WBGT') THEN
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
      !
      SUBROUTINE SGWF2BAS7L(IPOS,LINE,LLOC,IOFLG,NLAY,IOUT,LABEL,INOC)
C     ******************************************************************
C     WHEN USING ALPHABETIC OUTPUT CONTROL, DECODE LAYER
C     NUMBERS FOR PRINTING OR SAVING HEAD OR DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IOFLG(NLAY,5)
      CHARACTER(*):: LINE
      CHARACTER(*):: LABEL
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
      IF(NSET == 0) THEN
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
      END SUBROUTINE
      !
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
      USE ERROR_INTERFACE,      ONLY: STOP_ERROR,
     +                                WARNING_MESSAGE, GET_WARN
      USE POST_KEY_SUB,            ONLY: CHECK_FOR_POST_KEY
      USE FILE_IO_INTERFACE,       ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE,    ONLY: PARSE_WORD, PARSE_WORD_UP
      USE STRINGS,                 ONLY: UPPER
      USE NUM2STR_INTERFACE,       ONLY: NUM2STR
      USE LINKED_LIST_INSTRUCTION, ONLY: CHARACTER_LINKED_LIST
      USE FILE_INCREMENTER_INTERFACE
      USE CONSTANTS, ONLY: NEG, Z, ONE, TWO, TRUE, FALSE, NL, BLN, EQ
      USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
      USE FILE_IO_INTERFACE,         ONLY: DATAFILE_UNIT_NUMBER  !Registry of all open unit numbers that are not automatically closed (For BAS this stores all the DATA and DATA(BINARY) files).
C      INCLUDE 'openspec.inc'
      TYPE(CHARACTER_LINKED_LIST):: LST,DIR
      TYPE(FILE_INCREMENTER):: LISTSPLIT
      INTEGER,        DIMENSION(NIUNIT):: IUNIT
      CHARACTER(5  ), DIMENSION(NIUNIT):: CUNIT
      CHARACTER(7  ):: FILSTAT
      CHARACTER(20 ):: FILACT, FMTARG, ACCARG
      CHARACTER(768):: LINE, FNAME
      CHARACTER(:), ALLOCATABLE:: WARN_FILE
      CHARACTER(20 ):: FILTYP
      LOGICAL::LOP
      INTEGER::BUFBLOCKSIZE, BUFCOUNT, WARN_IU
      !CHARACTER(:),ALLOCATABLE:: OLD,REED,REPL,WRIT,REWR
      CHARACTER(3):: ASYN
      LOGICAL:: UNIT_SPECIFIED, NOLIST, NOWARN, EOF, CHK
      CHARACTER(:), ALLOCATABLE:: TMP !COMPILER BUG REQUIRED THIS TMP VARIABLE
      TYPE(DATE_OPERATOR):: DATE
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.
      ISPLIT=NEG
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
           IF(LINE(ISTOP:ISTOP) /= "%") CALL STOP_ERROR(
     +          LINE,INUNIT,Z, MSG=
     +          'FOUND FIRST %, BUT FAILED TO LOCATE A CORRESPONDING '//
     +          'CLOSING % (e.g. expect %XYZ%, but got %XYZ')
                !
           CALL UPPER(LINE(ISTART:ISTOP))
           CALL PARSE_WORD(LINE,LLOC,ITYP1,ITYP2)
           !
           IF( LINE(ITYP1:ITYP1) == EQ ) THEN
               LLOC = ITYP1 + ONE
               CALL PARSE_WORD(LINE,LLOC,ITYP1,ITYP2)
           END IF
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
        IF(LINE(ITYP1:ITYP2) == 'LIST') THEN
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
          IF(FILSTAT  /=  'NOPRINT') THEN
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
            WRITE(IOUT,'( A/, 4x,2A/,A,//)') REPEAT("∙",62),
     +                            'OneWater Simulation Initiated at  ',
     +                             DATE%STR('  '),
     +                             REPEAT("∙",62)
            !
            WRITE(IOUT,'(1x,A,/6x,A)')'LIST FILE: '//TRIM(FNAME),
     +                                  'UNIT: '//NUM2STR(IOUT)
          END IF
          !
        ELSEIF(LINE(ITYP1:ITYP2) == 'BOYCE') THEN
          !
          WRITE(*,'(3(/A),/)')'BOYCE PACKAGE FOUND',
     +            ' WOW THANK YOU FOR THINKING I AM ',
     +            ' AWESOME ENOUGH TO HAVE A PACKAGE NAMED AFTER ME.'
          !
        ELSEIF(LINE(ITYP1:ITYP2) == 'WARN') THEN
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
          IF(FILSTAT  /=  'NOPRINT') THEN
            I = NEG
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
          ISPLIT = NEG
          BUFBLOCKSIZE = Z
          CALL GENERIC_NULL_FILE_OPEN(IOUT)
      END IF
      !
      IF(.NOT. NOWARN) THEN
            WRITE(IOUT,'(/1x,A,/6x,A)')  'WARN FILE: '//WARN_FILE,
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
!          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
!          CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
!          FNAME=LINE(INAM1:INAM2)
!          !
!          INQUIRE(UNIT=IU,OPENED=LOP)
!          IF(LOP) CALL STOP_ERROR(LINE,INUNIT,IOUT,
!     +    MSG=TRIM(FILTYP)//' UNIT '//NUM2STR(IU)//' IS ALREADY IN USE.'
!     +    //NL//'PLEASE CHOOSE A DIFFERENT UNIT NUMBER.')
          CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
          READ(LINE(INAM1:INAM2), *, IOSTAT=IERR) NN
          IF (IERR==Z) THEN
             IU=NN
             CALL PARSE_WORD(LINE,LLOC,INAM1,INAM2)
             FNAME=LINE(INAM1:INAM2)
             !
             INQUIRE(UNIT=IU,OPENED=LOP)
          IF(LOP) CALL STOP_ERROR(LINE,INUNIT,IOUT,
     +    MSG=TRIM(FILTYP)//' UNIT '//NUM2STR(IU)//' IS ALREADY IN USE.'
     +    //NL//'PLEASE CHOOSE A DIFFERENT UNIT NUMBER.')
          ELSE
             IU=Z
             FNAME=LINE(INAM1:INAM2)
          END IF
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
          CALL DATAFILE_UNIT_NUMBER%ADD(IU) !Note that this does not include UTM8 BOM check when TFR includes EXTERNAL unit REWIND or RELOAD for a unit declaired here
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
          CALL DATAFILE_UNIT_NUMBER%ADD(IU)
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
        CASE("AG");   TMP = TRIM(ADJUSTL(LINE))
        END SELECT
        !
        IF( ANY(FILTYP(:5) == CUNIT) .OR. FILTYP(:3) == 'BAS') THEN
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
          WHERE (FILTYP(:5) == CUNIT) IUNIT=IU
          NFILE=NFILE+1
          !
          WRITE(IOUT,'(2A)') FILTYP(:4),' PACKAGE OPENED.'
          !
          IF (FILTYP(:3) == 'BAS') INBAS=IU
          !
        ELSEIF(FILTYP(ONE:ONE)=="%") THEN
          CYCLE  ! BY PASS ERROR
        ELSEIF( FILTYP  /=  'LIST'            .AND.
     +          FILTYP  /=  'WARN'            .AND.
     +          FILTYP  /=  'DATA'            .AND.
     +          FILTYP  /=  'DATA(BINARY)'    .AND.
     +          FILTYP  /=  'DATAGLO'         .AND.
     +          FILTYP  /=  'DATAGLO(BINARY)' .AND.
     +          FILTYP  /=  'BOYCE'           .AND.
     +          FILTYP  /=  'NULL'            .AND.
     +          FILTYP  /=  'NUL'             .AND.
     +          FILTYP  /=  'NULL(BINARY)'    .AND.
     +          FILTYP  /=  'NUL(BINARY)'       ) THEN
      FNAME='NAME FILE ERROR: LOADING PACKAGES FROM NAME FILE FOUND '//
     +'A PACAKGE THAT IS NOT SUPPORTED OR KNOWN BY OneWater.'//NL//
     +'PACKAGE MAYBE MIS-SPELLED OR NOT CORRECLTLY SPECIFIED.'//BLN//
     +'THE UNKNOWN PACKAGE IS "'//TRIM(FILTYP)//'"'//BLN//
     +'THE FOLLOWING ARE THE CURRENTLY ACCEPTED/KNOWN PACKAGE NAMES '//
     +'IN OneWater:'//NL
          DO I=1,NIUNIT
                IF(CUNIT(I) /= '') FNAME=TRIM(FNAME)//TRIM(CUNIT(I))//NL
          END DO
          CALL STOP_ERROR(LINE,INUNIT,IOUT,FNAME)
        END IF
      END DO
      !
      IF(UNIT_SPECIFIED) THEN
        WRITE(IOUT,'(/2A,/2A,/2A/)') 'NOTIFICATION: A FILE UNIT ',
     +  'NUMBER WAS FOUND FOR EITHER THE LIST FILE OR A PACAKGE.',
     +  'IT WILL BE USED EVEN THOUGH UNIT NUMBERS FOR THE LIST FILE ',
     +  'AND PACKAGES ARE NOW OPTIONAL.',
     +  '[IT IS RECOMMENDED TO STILL USE A UNIT NUMBER ',
     +  'FOR DATA and DATA(BINARY) ]'
      END IF
      !
      ! CHECK FOR DUBLICATE UNIT NUMBERS
      !
!      CALL ICHK%INIT()
!      DO I=1, NIUNIT-1
!          IF(IUNIT(I) /= Z)THEN
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
      IF    (IUNIT(23) /= 0.AND.IUNIT(62) == 0.AND.IUNIT(63) /= 0) THEN ! FLIP LPF TO UPW SINCE USE NWT SOLVER
        IUNIT(62) = IUNIT(23)
        IUNIT(23) = 0
      ELSEIF(IUNIT(23) == 0.AND.IUNIT(62) /= 0.AND.IUNIT(63) == 0)THEN  ! FLIP UPW TO LPF SINCE NOT USING NWT SOLVER
        IUNIT(23) = IUNIT(62)
        IUNIT(62) = 0
      END IF
      !
      IF(IUNIT(66) /= 0) CALL WARNING_MESSAGE(TMP, INUNIT, IOUT, MSG=
     +  'AG package specified in NAME file.'//BLN//
     +  'It is recommended to use FMP instead of AG for '//
     +  'conjunctive use simulations.')
C
      END SUBROUTINE
      !
      SUBROUTINE SUB_LST_VAR(LINE,LST,DIR,ISTART,ISTOP,IN,IOUT)
      USE LINKED_LIST_INSTRUCTION, ONLY: CHARACTER_LINKED_LIST
      USE ERROR_INTERFACE,         ONLY: STOP_ERROR, WARNING_MESSAGE
      USE FILE_IO_INTERFACE,       ONLY: COMMENT_INDEX
      USE STRINGS,                 ONLY: UPPER
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
      USE ERROR_INTERFACE,   ONLY: STOP_ERROR
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
C seb
      USE ExpressionParser  !ONLY PROVIDES ACCESS TO ExpEVAL AND MLTLOC
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
C
      CHARACTER(20):: RW
      CHARACTER( 1):: COP
      CHARACTER(24):: ANAME
      CHARACTER(MXNAMLEN):: CTMP1,CTMP2                                 !seb CHANGED CHAR LEN FROM 10
      CHARACTER(768):: LINE
      INTEGER:: IOUTM,IOUTZ
      INTEGER:: MULTPRINT,ZONEPRINT                                     !seb TEMP VARIABLE USED AS FLAG FOR PRINTING OUT MULT ARRAYS THAT ARE READ IN
      CHARACTER(30):: MultFN,ZoneFN                                     !seb TEMP VARIABLE TO HOLD FILE NAME UNTIL GLOBAL VARIABLE IS USED
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
      IF(INZONE /= 0) THEN
         WRITE(IOUT,1) NUM2STR(INZONE)                    !seb CHANGED IOUTM TO IOUT FOR ENTIRE IF
    1    FORMAT(1X,/1X,'ZONE OPTION, INPUT READ FROM UNIT ',A)
         CALL READ_TO_DATA(LINE,INZONE,IOUT,IOUT)
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
               IF(LINE=='') EXIT
               NZONAR=NZONAR+1
               CALL U2DINT(IZON(:,:,1),ANAME,NROW,NCOL,0,INZONE,-1)
           END DO
           DEALLOCATE(IZON)
           CALL UTF8_BOM_OFFSET_REWIND(INZONE)  ! REWIND(INZONE)
           CALL READ_TO_DATA(LINE,INZONE,IOUT)
           LLOC=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INZONE)
         END IF
         WRITE(IOUT,2) NUM2STR(NZONAR)
    2    FORMAT(1X,A,' ZONE ARRAYS')
         IF(NZONAR.LT.0) NZONAR=0
C seb READ IN FLAG TO PRINT OUT ZONE ARRAYS
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ZONEPRINT,R,0,INZONE)     !seb OPTIONAL READ IN OF PRINT FLAG >0 MEANS PRINT OUT
        IF (LINE(LEN(LINE):LEN(LINE)) == 'E') THEN
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
      IF(INMULT /= 0) THEN
         WRITE(IOUT,11) NUM2STR(INMULT)                    !seb CHANGED IOUTM TO IOUT FOR ENTIRE IF
   11    FORMAT(1X,/1X,'MULTIPLIER OPTION, INPUT READ FROM UNIT ',A)
         CALL READ_TO_DATA(LINE,INMULT,IOUT,IOUT)
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
               IF(LINE=='') EXIT
               NMLTAR=NMLTAR+1
               LLOC=1
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INMULT)
               IF(  LINE(ISTART:ISTOP) == 'FUNCTION' .OR.
     +              LINE(ISTART:ISTOP) == 'EXPRESSION' ) THEN
                    READ (INMULT,'(A)') LINE
               ELSE
                   CALL U2DDBL(RMLT(:,:,1),ANAME,NROW,NCOL,0,INMULT,0)
               END IF
           END DO
           DEALLOCATE(RMLT)
           CALL UTF8_BOM_OFFSET_REWIND(INMULT)  ! REWIND(INMULT)
           CALL READ_TO_DATA(LINE,INMULT,IOUT)
           LLOC=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
         END IF
         WRITE(IOUT,12) NUM2STR(NMLTAR)
   12    FORMAT(1X,A,' MULTIPLIER ARRAYS')
         IF(NMLTAR.LT.0) NMLTAR=0
      END IF
C seb READ IN FLAG TO PRINT OUT MULTIPLIER ARRAYS
      IF(NMLTAR.GT.0)THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MULTPRINT,R,0,INMULT)     !seb OPTIONAL READ IN OF PRINT FLAG >0 MEANS PRINT OUT  -> Make Generic_Out Someday
        IF (LINE(LEN(LINE):LEN(LINE)) == 'E') THEN
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
          IF(  LINE(ISTART:ISTOP) /= 'FUNCTION'
     +    .AND.LINE(ISTART:ISTOP) /= 'EXPRESSION' ) THEN
ccrth          WRITE(*,*)LINE(ISTART:ISTOP)
C
C6D-----Define array using array reader.
             ANAME=' MULT. ARRAY: '//MLTNAM(M)

             READ (INMULT,'(A)') LINE
             BACKSPACE(INMULT)
             CALL U2DDBL(RMLT(:,:,M),ANAME,NROW,NCOL,0,INMULT,IOUT)      !seb changed from U2DREL
             IF(MULTPRINT>0) THEN
               WRITE(IOUTM,29) MLTNAM(M)                                !seb ADDED PRINT OPTION FOR REGULAR MULT ARRAYS
   29          FORMAT(1X,/1X,'MULTIPLIER ARRAY: ',A)
             END IF
C seb added flag for: EXPRESSION
          ELSE IF(LINE(ISTART:ISTOP) /= 'EXPRESSION' ) THEN
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
   45        IF(NOP == 0) THEN
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
               IF(CTMP1 == CTMP2) GO TO 60
   50        CONTINUE
             CALL STOP_ERROR(LINE,INMULT,IOUT,MSG='ARRAY '//
     + 'OPERAND HAS NOT BEEN PREVIOUSLY DEFINED:'//LINE(ISTART:ISTOP))
C
C6E5----Apply the + operator.
   60        IF(COP == '+' .OR. COP == ' ') THEN
                DO 100 I = 1, NROW
                DO 100 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)+ RMLT(J,I,MM)
  100           CONTINUE
             ELSE IF(COP == '-') THEN
                DO 200 I = 1, NROW
                DO 200 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)- RMLT(J,I,MM)
  200           CONTINUE
             ELSE IF(COP == '*') THEN
                DO 300 I = 1, NROW
                DO 300 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)* RMLT(J,I,MM)
  300           CONTINUE
             ELSE IF (COP == '/') THEN
                DO 400 I = 1, NROW
                DO 400 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)/ RMLT(J,I,MM)
  400           CONTINUE
crth
             ELSE IF(COP == '^') THEN
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
          ELSE  !LINE(ISTART:ISTOP) == 'EXPRESSION'
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
           IF (LINE(ISTART:ISTOP) == 'CONSTANT') THEN
           !IF (ALL(RMLT(1,1,M) == RMLT(:,:,M)))THEN                       !seb ADDED PRINT OUT OF CONSTANT VALUE THIS CAN REPLACE THE PROCUDING LINES IF THERE IS A PROBLEM
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
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7ARPVAL(IUPVAL)
C     ******************************************************************
C     READ PARAMETER INPUT FILE
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,              ONLY: Z, ONE, NEG, BLNK, NL,TRUE,FALSE
      USE GLOBAL,    ONLY: IOUT,IUNIT
      USE PARAMMODULE,ONLY:MXPAR,IPSUM,PARNAM,B,NPVAL,PROPPRINT,MXNAMLEN
      USE ERROR_INTERFACE,        ONLY: STOP_ERROR
      USE FILE_IO_INTERFACE,      ONLY: READ_TO_DATA
      USE NUM2STR_INTERFACE,      ONLY: NUM2STR
      USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
      USE PARSE_WORD_INTERFACE,   ONLY: PARSE_WORD, PARSE_WORD_UP
C
      CHARACTER(MXNAMLEN):: PNI, PNJ                                    !seb CHANGED FROM CHAR LEN OF 10
      CHARACTER(768):: LINE
      LOGICAL:: END_OF_FILE
      CHARACTER(:),ALLOCATABLE:: ERR
C     ------------------------------------------------------------------
C
C1------CHECK TO SEE IF THE PARAMETER FILE WAS DECLARED IN THE NAME FILE.
      IU = IUNIT(IUPVAL)
      IF(IU == Z) THEN
         NPVAL = Z
         !PROPPRINT = ' '                                                !seb  SET TO BLANK BY DEFAULT
         RETURN
      END IF
C
C2------INITIALIZE VARIABLES
      ERR  = ''
      IERR = Z
      NPE  = Z
C
C3------IDENTIFY PARAMETER VALUE OPTION.
        WRITE (IOUT,12) NUM2STR(IU)
   12 FORMAT (1X,/,1X,
     1  'PARAMETER VALUE INPUT FILE,  INPUT READ FROM UNIT ',A)
C
C4------READ & PRINT NUMBER OF PARAMETER VALUES.
      CALL READ_TO_DATA(LINE,IU,IOUT,IOUT,
     +                          HED="-- READING PVAL PACKAGE INPUT --")
      LLOC = ONE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPVAL,DUM,IOUT,IU)
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      IF (LINE(ISTART:ISTOP) == 'PROPPRINT') THEN
        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
        DEALLOCATE(PROPPRINT)
        IF( LINE(ISTART:ISTOP) == BLNK ) THEN
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
       IF(PROPPRINT /= '') WRITE(IOUT,'(/A/,A/)')
     + 'PROPPRINT>0: PROPERTIES AFTER PARAMETERS HAVE BEEN APPLIED '//
     + 'WILL BE WRITEN TO SEPARATE FILES. THESE FILES WILL BE PLACED '//
     + 'IN:', '"'//PROPPRINT//'"'
   14 FORMAT (1X,/,1X,'NUMBER OF PARAMETER VALUES TO BE READ FROM',
     1               ' PARAMETER VALUE FILE:',I5)
      IF (NPVAL < Z) THEN
          WRITE (IOUT,16)
   16     FORMAT(1X,'NPVAL IN PARAMETER INPUT IS NEGATIVE, PVAL  ',
     1  'WILL AUTO-COUNT PARAMETERS LISTED IN FILE.',/,
     2  'ONLY BLANK LINES, COMMENTS AND PARAMETER NAMES ALLOWED.'   )
        NPVAL = NEG  !NOTE THAT EOF WILL CAUSE ONE EXTRA READ OF LINE
        END_OF_FILE = FALSE
        DO WHILE (.NOT. END_OF_FILE)
            CALL READ_TO_DATA(LINE,IU,IOUT,EOF=END_OF_FILE)
            NPVAL = NPVAL + 1
        END DO
        CALL UTF8_BOM_OFFSET_REWIND(IU)  ! REWIND(IU)
        CALL READ_TO_DATA(LINE,IU,IOUT)
      ENDIF
      IPSUM = NPVAL
C
C5-----DEACTIVATE OPTION IF THERE ARE NO PARAMETERS IN FILE.
      IF(NPVAL <= Z) THEN
           WRITE(IOUT,*) ' NPVAL in parameter file is 0,',
     1            ' so ignoring the parameter file'
        CLOSE(UNIT=IU)
        IU = Z
        RETURN
      END IF
C
C6------STOP IF THERE ARE MORE THAN THE MAXIMUM NUMBER OF PARAMETERS.
      IF(NPVAL > MXPAR) THEN                                           !seb CHANGED COMMENT TO BE MORE INFORMATIVE ABOUT NEW OPTION MAXPARAM
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
        LLOC = ONE
        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
        PARNAM(I) = LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,B(I),0,0)
        IF (LINE(LEN(LINE):LEN(LINE)) == 'E') GO TO 80                  !seb
        WRITE(IOUT,570) PARNAM(I),B(I)
  570   FORMAT(1X,A10,2X,G12.5)
C
C8A-----CHECK FOR DUPLICATE PARAMETER NAME FOR ALL BUT THE FIRST PARAMETER.
        IF (I > ONE) THEN
          PNI=PARNAM(I)
          CALL UPCASE(PNI)
          IM1 = I-1
          DO 60 J=1,IM1
            PNJ=PARNAM(J)
            CALL UPCASE(PNJ)
            IF (PNI == PNJ) THEN
!                WRITE(IOUT,500) PARNAM(I)
!  500         FORMAT (' PARAMETER "',A10,
!     &        '" IS LISTED MORE THAN ONCE IN PARAMETER FILE',/,
!     &        ' -- STOP EXECUTION')
                ERR=ERR//PARNAM(I)//NEW_LINE(" ")
                IERR = ONE
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
      IF (IERR > Z) THEN
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
      !
      SUBROUTINE SGWF2BAS7STPVAL()
C     ******************************************************************
C     CHECK THAT PARAMETER DEFINITIONS ARE COMPLETE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT
      USE PARAMMODULE, ONLY:NPVAL,PARTYP,PARNAM
      USE ERROR_INTERFACE, ONLY: WARNING_MESSAGE
      CHARACTER(:),ALLOCATABLE::ERR
C     ------------------------------------------------------------------
      IF(NPVAL.LE.0) RETURN
      IERR=0
      ERR=''
C
Cx------CHECK THAT ALL PARAMETERS IN PARAMETER INPUT FILE HAVE BEEN DEFINED.
      DO 90 IP=1,NPVAL
        IF (PARTYP(IP) == ' ') THEN
          IERR = 1
          ERR=ERR//PARNAM(IP)//NEW_LINE(" ")
!            WRITE(IOUT,110) PARNAM(IP)
!  110     FORMAT(1X,/,1X,'PARAMETER "',A10,
!     1      '" IN PARAMETER INPUT FILE HAS NOT BEEN DEFINED',/,
!     2           ' -- STOP EXECUTION')
        ENDIF
   90 CONTINUE
C
      IF(IERR /= 0) THEN
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
      END SUBROUTINE
      !
      SUBROUTINE GWF2BAS7DA(IGRID)
      !  DEALLOCATE GLOBAL DATA
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
      USE CONSTANTS,         ONLY: inf_I, Z, ONE, DZ, BLNK
      USE FILE_IO_INTERFACE, ONLY: CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES ! CLOSE ANY DATA FILES OPENED DURING THE COURSE OF SIMULATION
     +
      USE ERROR_INTERFACE,   ONLY: CLOSE_WARNING_UNIT
      USE EquationParser,    ONLY: REMOVE_EQUATION_ERROR_ROUTINES
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID
      INTEGER:: I, J
      !
      ! Deallocate LGR Grid Independent Variables -------------------------------------------
      !
      IF(IGRID == ONE) THEN
          !
          CALL CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES()    ! -> Closes DATAFILE_UNIT_NUMBER
                                                        !           -> Registry of all open unit numbers that are not automatically closed.
                                                        !              This holds all DATAFILE opened files and NameFile DATA and DATA(BINARY) file).
                                                        !              Closes all registered unit numbers
          CALL CLOSE_WARNING_UNIT()
          !
          CALL REMOVE_EQUATION_ERROR_ROUTINES()
          !
          CALL SUPER_NAMES%DESTROY()
          !
          NGRIDS        = ONE
          CMD_ITER_INFO = Z
          RCloseBAS     = DZ
          HCloseBAS     = DZ
          RCloseL2BAS   = DZ
          GW_SOLVER     = BLNK
          GW_FLOW_PACK  = BLNK
          !
          MXPAR  = Z
          MXCLST = Z
          MXINST = Z
          !
          DEALLOCATE(SUBLNK)
          DEALLOCATE(INPUT_CHECK)
          DEALLOCATE(BIN_REAL_KIND)
      END IF
      !
      ! Deallocate the GLOBAL Module Variables -------------------------------------------
      !
      ! Close File Units
      !
      DO I=1,NIUNIT
          IF (     GLOBALDAT(IGRID)%IUNIT(I) /= Z     ) THEN
            CLOSE( GLOBALDAT(IGRID)%IUNIT(I), IOSTAT=J)
                   GLOBALDAT(IGRID)%IUNIT(I) = Z
          END IF
      END DO
      !
      CLOSE(GLOBALDAT(IGRID)%INBAS, IOSTAT=J)
      CLOSE(GLOBALDAT(IGRID)%IOUT,  IOSTAT=J)
      !
      ! Deallocate LGR Data Types
      !
      DEALLOCATE( GLOBALDAT(IGRID)% NCOL            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NROW            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NLAY            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NPER            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NBOTM           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NCNFBD          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% ITMUNI          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% LENUNI          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IXSEC           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% ITRSS           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% INBAS           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IFREFM          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NODES           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IOUT            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% MXITER          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IRESTART        , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% KPERSTART       , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% KSTPSTART       , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IUNITSTART      , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% BACKTRACKING    , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IUNIT           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% HNEW            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% HNEW_OLD        , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% WTABLE          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% WTABLE_OLD      , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% UPLAY           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% LBOTM           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% LAYCBD          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% LAYHDT          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% LAYHDS          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% PERLEN          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NSTP            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% TSMULT          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% ISSFLG          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% DELR            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% DELC            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% AREA            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% GSE             , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% BOTM            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% HOLD            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% IBOUND          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% CR              , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% CC              , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% CV              , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% HCOF            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% RHS             , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% BUFF            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% RBUF            , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% STRT            , STAT=J)
      IF(ALLOC_DDREF) THEN
       DEALLOCATE(GLOBALDAT(IGRID)% DDREF           , STAT=J)
      END IF
      DEALLOCATE( GLOBALDAT(IGRID)% ALLOC_DDREF     , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% UPLAY_IDX       , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% XYGRID          , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% SPTIM           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% SPSTART         , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% SPEND           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% NOCBC           , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% CBC_GLOBAL_UNIT , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% RCloseBAS       , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% HCloseBAS       , STAT=J)
      DEALLOCATE( GLOBALDAT(IGRID)% RCloseL2BAS     , STAT=J)
      !
      ! Nullify LGR Data Types
      !
      NULLIFY( GLOBALDAT(IGRID)% NCOL            )
      NULLIFY( GLOBALDAT(IGRID)% NROW            )
      NULLIFY( GLOBALDAT(IGRID)% NLAY            )
      NULLIFY( GLOBALDAT(IGRID)% NPER            )
      NULLIFY( GLOBALDAT(IGRID)% NBOTM           )
      NULLIFY( GLOBALDAT(IGRID)% NCNFBD          )
      NULLIFY( GLOBALDAT(IGRID)% ITMUNI          )
      NULLIFY( GLOBALDAT(IGRID)% LENUNI          )
      NULLIFY( GLOBALDAT(IGRID)% IXSEC           )
      NULLIFY( GLOBALDAT(IGRID)% ITRSS           )
      NULLIFY( GLOBALDAT(IGRID)% INBAS           )
      NULLIFY( GLOBALDAT(IGRID)% IFREFM          )
      NULLIFY( GLOBALDAT(IGRID)% NODES           )
      NULLIFY( GLOBALDAT(IGRID)% IOUT            )
      NULLIFY( GLOBALDAT(IGRID)% MXITER          )
      NULLIFY( GLOBALDAT(IGRID)% IRESTART        )
      NULLIFY( GLOBALDAT(IGRID)% KPERSTART       )
      NULLIFY( GLOBALDAT(IGRID)% KSTPSTART       )
      NULLIFY( GLOBALDAT(IGRID)% IUNITSTART      )
      NULLIFY( GLOBALDAT(IGRID)% BACKTRACKING    )
      NULLIFY( GLOBALDAT(IGRID)% IUNIT           )
      NULLIFY( GLOBALDAT(IGRID)% HNEW            )
      NULLIFY( GLOBALDAT(IGRID)% HNEW_OLD        )
      NULLIFY( GLOBALDAT(IGRID)% WTABLE          )
      NULLIFY( GLOBALDAT(IGRID)% WTABLE_OLD      )
      NULLIFY( GLOBALDAT(IGRID)% UPLAY           )
      NULLIFY( GLOBALDAT(IGRID)% LBOTM           )
      NULLIFY( GLOBALDAT(IGRID)% LAYCBD          )
      NULLIFY( GLOBALDAT(IGRID)% LAYHDT          )
      NULLIFY( GLOBALDAT(IGRID)% LAYHDS          )
      NULLIFY( GLOBALDAT(IGRID)% PERLEN          )
      NULLIFY( GLOBALDAT(IGRID)% NSTP            )
      NULLIFY( GLOBALDAT(IGRID)% TSMULT          )
      NULLIFY( GLOBALDAT(IGRID)% ISSFLG          )
      NULLIFY( GLOBALDAT(IGRID)% DELR            )
      NULLIFY( GLOBALDAT(IGRID)% DELC            )
      NULLIFY( GLOBALDAT(IGRID)% AREA            )
      NULLIFY( GLOBALDAT(IGRID)% GSE             )
      NULLIFY( GLOBALDAT(IGRID)% BOTM            )
      NULLIFY( GLOBALDAT(IGRID)% HOLD            )
      NULLIFY( GLOBALDAT(IGRID)% IBOUND          )
      NULLIFY( GLOBALDAT(IGRID)% CR              )
      NULLIFY( GLOBALDAT(IGRID)% CC              )
      NULLIFY( GLOBALDAT(IGRID)% CV              )
      NULLIFY( GLOBALDAT(IGRID)% HCOF            )
      NULLIFY( GLOBALDAT(IGRID)% RHS             )
      NULLIFY( GLOBALDAT(IGRID)% BUFF            )
      NULLIFY( GLOBALDAT(IGRID)% RBUF            )
      NULLIFY( GLOBALDAT(IGRID)% STRT            )
      NULLIFY( GLOBALDAT(IGRID)% DDREF           )
      NULLIFY( GLOBALDAT(IGRID)% ALLOC_DDREF     )
      NULLIFY( GLOBALDAT(IGRID)% UPLAY_IDX       )
      NULLIFY( GLOBALDAT(IGRID)% XYGRID          )
      NULLIFY( GLOBALDAT(IGRID)% SPTIM           )
      NULLIFY( GLOBALDAT(IGRID)% SPSTART         )
      NULLIFY( GLOBALDAT(IGRID)% SPEND           )
      NULLIFY( GLOBALDAT(IGRID)% NOCBC           )
      NULLIFY( GLOBALDAT(IGRID)% CBC_GLOBAL_UNIT )
      NULLIFY( GLOBALDAT(IGRID)% RCloseBAS       )
      NULLIFY( GLOBALDAT(IGRID)% HCloseBAS       )
      NULLIFY( GLOBALDAT(IGRID)% RCloseL2BAS     )
      !
      ! NULLIFY local pointers that are no longer used
      !
      IF(IGRID == 1)THEN
         NULLIFY( NCOL            )
         NULLIFY( NROW            )
         NULLIFY( NLAY            )
         NULLIFY( NPER            )
         NULLIFY( NBOTM           )
         NULLIFY( NCNFBD          )
         NULLIFY( ITMUNI          )
         NULLIFY( LENUNI          )
         NULLIFY( IXSEC           )
         NULLIFY( ITRSS           )
         NULLIFY( INBAS           )
         NULLIFY( IFREFM          )
         NULLIFY( NODES           )
         NULLIFY( IOUT            )
         NULLIFY( MXITER          )
         NULLIFY( IRESTART        )
         NULLIFY( KPERSTART       )
         NULLIFY( KSTPSTART       )
         NULLIFY( IUNITSTART      )
         NULLIFY( BACKTRACKING    )
         NULLIFY( IUNIT           )
         NULLIFY( HNEW            )
         NULLIFY( HNEW_OLD        )
         NULLIFY( WTABLE          )
         NULLIFY( WTABLE_OLD      )
         NULLIFY( UPLAY           )
         NULLIFY( LBOTM           )
         NULLIFY( LAYCBD          )
         NULLIFY( LAYHDT          )
         NULLIFY( LAYHDS          )
         NULLIFY( PERLEN          )
         NULLIFY( NSTP            )
         NULLIFY( TSMULT          )
         NULLIFY( ISSFLG          )
         NULLIFY( DELR            )
         NULLIFY( DELC            )
         NULLIFY( AREA            )
         NULLIFY( GSE             )
         NULLIFY( BOTM            )
         NULLIFY( HOLD            )
         NULLIFY( IBOUND          )
         NULLIFY( CR              )
         NULLIFY( CC              )
         NULLIFY( CV              )
         NULLIFY( HCOF            )
         NULLIFY( RHS             )
         NULLIFY( BUFF            )
         NULLIFY( RBUF            )
         NULLIFY( STRT            )
         NULLIFY( DDREF           )
         NULLIFY( ALLOC_DDREF     )
         NULLIFY( UPLAY_IDX       )
         NULLIFY( XYGRID          )
         NULLIFY( SPTIM           )
         NULLIFY( SPSTART         )
         NULLIFY( SPEND           )
         NULLIFY( NOCBC           )
         NULLIFY( CBC_GLOBAL_UNIT )
         NULLIFY( RCloseBAS       )
         NULLIFY( HCloseBAS       )
         NULLIFY( RCloseL2BAS     )
      END IF
      !
      ! Deallocate the BAS Module Variables -------------------------------------------
      !
      DEALLOCATE( GWFBASDAT(IGRID)% MSUM                 , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IUBGT                , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IHEDFM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IHEDUN               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IDDNFM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IDDNUN               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IBOUUN               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% LBHDSV               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% LBDDSV               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% LBBOSV               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IBUDFL               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ICBCFL               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IHDDFL               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IAUXSV               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IBDOPT               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IPRTIM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IPEROC               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ITSOC                , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ICHFLG               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IDDREF               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IDDREFNEW            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% DELT                 , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PERTIM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% TOTIM                , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% HNOFLO               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% HDRY                 , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% STOPER               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% CHEDFM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% CDDNFM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% CBOUFM               , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% IOFLG                , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% VBVL                 , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% VBNM                 , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PVOL_ERR             , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PDIFFPRT             , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% REALTIM              , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% SIMTIM_PER           , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% REALTIM_PER          , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% TOTPERTIM            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% SIMTIME              , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% USE_LEAP_YR          , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% MAX_REL_VOL_ERROR    , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% MAX_REL_VOL_INVOKED  , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% MIN_SOLVER_INTER     , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% MIN_ITER_INPUT       , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% MIN_SOLVER_INTER_NEW , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% MIN_SOLVER_INTER_SP  , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% DEALLOCATE_MULT      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% LISTSPLIT            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% BUDGETDB             , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% INTER_INFO           , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_CUM_HEAD_CHNG   , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% CUM_HEAD_CHNG        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% CUM_HEAD_CHNG_E10    , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_RES             , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_RES_LIM         , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_RES_CUM         , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_RES_CUM_ARR     , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ABOVE_GSE_LIM        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ABOVE_GSE_PRT_LIM    , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ABOVE_GSE_PRT        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% OSCIL_DMP_OUTER      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% OSCIL_DMP_LRC        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% OSCIL_DMP_DIF        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_CNVG_OUTER      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_CNVG_NTERM      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_CNVG            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_CNVG_LRC        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_CNVG_DIF        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_FRES_OUTER      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_FRES_NTERM      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_FRES            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_FRES_LRC        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_FRES_DIF        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_VERR_OUTER      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_VERR_NTERM      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_VERR            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_VERR_LRC        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRNT_VERR_DIF        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% SAVE_HEAD            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% SAVE_HEAD_FLAG       , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRINT_HEAD           , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRINT_HEAD_FLAG      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRINT_WTAB_FLAG      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% PRINT_WDEP_FLAG      , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% DAMPEN_START         , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% DAMPEN_START_ITR     , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% DAMPEN_START_DMP     , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% ADAMP_INPUT          , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% BAS_ADAMP            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% BAS_ADAMP_TOL        , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% BAS_ADAMP_TOL2       , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% HED_CHNG2            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% HED_CHNG3            , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% HED_LOCK             , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% DATE_SP              , STAT=J)
      DEALLOCATE( GWFBASDAT(IGRID)% HAS_STARTDATE        , STAT=J)
      !
      ! Nullify LGR Data Types
      !
      NULLIFY( GWFBASDAT(IGRID)% MSUM                 )
      NULLIFY( GWFBASDAT(IGRID)% IUBGT                )
      NULLIFY( GWFBASDAT(IGRID)% IHEDFM               )
      NULLIFY( GWFBASDAT(IGRID)% IHEDUN               )
      NULLIFY( GWFBASDAT(IGRID)% IDDNFM               )
      NULLIFY( GWFBASDAT(IGRID)% IDDNUN               )
      NULLIFY( GWFBASDAT(IGRID)% IBOUUN               )
      NULLIFY( GWFBASDAT(IGRID)% LBHDSV               )
      NULLIFY( GWFBASDAT(IGRID)% LBDDSV               )
      NULLIFY( GWFBASDAT(IGRID)% LBBOSV               )
      NULLIFY( GWFBASDAT(IGRID)% IBUDFL               )
      NULLIFY( GWFBASDAT(IGRID)% ICBCFL               )
      NULLIFY( GWFBASDAT(IGRID)% IHDDFL               )
      NULLIFY( GWFBASDAT(IGRID)% IAUXSV               )
      NULLIFY( GWFBASDAT(IGRID)% IBDOPT               )
      NULLIFY( GWFBASDAT(IGRID)% IPRTIM               )
      NULLIFY( GWFBASDAT(IGRID)% IPEROC               )
      NULLIFY( GWFBASDAT(IGRID)% ITSOC                )
      NULLIFY( GWFBASDAT(IGRID)% ICHFLG               )
      NULLIFY( GWFBASDAT(IGRID)% IDDREF               )
      NULLIFY( GWFBASDAT(IGRID)% IDDREFNEW            )
      NULLIFY( GWFBASDAT(IGRID)% DELT                 )
      NULLIFY( GWFBASDAT(IGRID)% PERTIM               )
      NULLIFY( GWFBASDAT(IGRID)% TOTIM                )
      NULLIFY( GWFBASDAT(IGRID)% HNOFLO               )
      NULLIFY( GWFBASDAT(IGRID)% HDRY                 )
      NULLIFY( GWFBASDAT(IGRID)% STOPER               )
      NULLIFY( GWFBASDAT(IGRID)% CHEDFM               )
      NULLIFY( GWFBASDAT(IGRID)% CDDNFM               )
      NULLIFY( GWFBASDAT(IGRID)% CBOUFM               )
      NULLIFY( GWFBASDAT(IGRID)% IOFLG                )
      NULLIFY( GWFBASDAT(IGRID)% VBVL                 )
      NULLIFY( GWFBASDAT(IGRID)% VBNM                 )
      NULLIFY( GWFBASDAT(IGRID)% PVOL_ERR             )
      NULLIFY( GWFBASDAT(IGRID)% PDIFFPRT             )
      NULLIFY( GWFBASDAT(IGRID)% REALTIM              )
      NULLIFY( GWFBASDAT(IGRID)% SIMTIM_PER           )
      NULLIFY( GWFBASDAT(IGRID)% REALTIM_PER          )
      NULLIFY( GWFBASDAT(IGRID)% TOTPERTIM            )
      NULLIFY( GWFBASDAT(IGRID)% SIMTIME              )
      NULLIFY( GWFBASDAT(IGRID)% USE_LEAP_YR          )
      NULLIFY( GWFBASDAT(IGRID)% MAX_REL_VOL_ERROR    )
      NULLIFY( GWFBASDAT(IGRID)% MAX_REL_VOL_INVOKED  )
      NULLIFY( GWFBASDAT(IGRID)% MIN_SOLVER_INTER     )
      NULLIFY( GWFBASDAT(IGRID)% MIN_ITER_INPUT       )
      NULLIFY( GWFBASDAT(IGRID)% MIN_SOLVER_INTER_NEW )
      NULLIFY( GWFBASDAT(IGRID)% MIN_SOLVER_INTER_SP  )
      NULLIFY( GWFBASDAT(IGRID)% DEALLOCATE_MULT      )
      NULLIFY( GWFBASDAT(IGRID)% LISTSPLIT            )
      NULLIFY( GWFBASDAT(IGRID)% BUDGETDB             )
      NULLIFY( GWFBASDAT(IGRID)% INTER_INFO           )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_CUM_HEAD_CHNG   )
      NULLIFY( GWFBASDAT(IGRID)% CUM_HEAD_CHNG        )
      NULLIFY( GWFBASDAT(IGRID)% CUM_HEAD_CHNG_E10    )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_RES             )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_RES_LIM         )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_RES_CUM         )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_RES_CUM_ARR     )
      NULLIFY( GWFBASDAT(IGRID)% ABOVE_GSE_LIM        )
      NULLIFY( GWFBASDAT(IGRID)% ABOVE_GSE_PRT_LIM    )
      NULLIFY( GWFBASDAT(IGRID)% ABOVE_GSE_PRT        )
      NULLIFY( GWFBASDAT(IGRID)% OSCIL_DMP_OUTER      )
      NULLIFY( GWFBASDAT(IGRID)% OSCIL_DMP_LRC        )
      NULLIFY( GWFBASDAT(IGRID)% OSCIL_DMP_DIF        )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_CNVG_OUTER      )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_CNVG_NTERM      )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_CNVG            )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_CNVG_LRC        )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_CNVG_DIF        )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_FRES_OUTER      )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_FRES_NTERM      )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_FRES            )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_FRES_LRC        )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_FRES_DIF        )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_VERR_OUTER      )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_VERR_NTERM      )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_VERR            )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_VERR_LRC        )
      NULLIFY( GWFBASDAT(IGRID)% PRNT_VERR_DIF        )
      NULLIFY( GWFBASDAT(IGRID)% SAVE_HEAD            )
      NULLIFY( GWFBASDAT(IGRID)% SAVE_HEAD_FLAG       )
      NULLIFY( GWFBASDAT(IGRID)% PRINT_HEAD           )
      NULLIFY( GWFBASDAT(IGRID)% PRINT_HEAD_FLAG      )
      NULLIFY( GWFBASDAT(IGRID)% PRINT_WTAB_FLAG      )
      NULLIFY( GWFBASDAT(IGRID)% PRINT_WDEP_FLAG      )
      NULLIFY( GWFBASDAT(IGRID)% DAMPEN_START         )
      NULLIFY( GWFBASDAT(IGRID)% DAMPEN_START_ITR     )
      NULLIFY( GWFBASDAT(IGRID)% DAMPEN_START_DMP     )
      NULLIFY( GWFBASDAT(IGRID)% ADAMP_INPUT          )
      NULLIFY( GWFBASDAT(IGRID)% BAS_ADAMP            )
      NULLIFY( GWFBASDAT(IGRID)% BAS_ADAMP_TOL        )
      NULLIFY( GWFBASDAT(IGRID)% BAS_ADAMP_TOL2       )
      NULLIFY( GWFBASDAT(IGRID)% HED_CHNG2            )
      NULLIFY( GWFBASDAT(IGRID)% HED_CHNG3            )
      NULLIFY( GWFBASDAT(IGRID)% HED_LOCK             )
      NULLIFY( GWFBASDAT(IGRID)% DATE_SP              )
      NULLIFY( GWFBASDAT(IGRID)% HAS_STARTDATE        )
      !
      ! NULLIFY local pointers that are no longer used
      !
      IF(IGRID == 1)THEN
         NULLIFY( MSUM                 )
         NULLIFY( IUBGT                )
         NULLIFY( IHEDFM               )
         NULLIFY( IHEDUN               )
         NULLIFY( IDDNFM               )
         NULLIFY( IDDNUN               )
         NULLIFY( IBOUUN               )
         NULLIFY( LBHDSV               )
         NULLIFY( LBDDSV               )
         NULLIFY( LBBOSV               )
         NULLIFY( IBUDFL               )
         NULLIFY( ICBCFL               )
         NULLIFY( IHDDFL               )
         NULLIFY( IAUXSV               )
         NULLIFY( IBDOPT               )
         NULLIFY( IPRTIM               )
         NULLIFY( IPEROC               )
         NULLIFY( ITSOC                )
         NULLIFY( ICHFLG               )
         NULLIFY( IDDREF               )
         NULLIFY( IDDREFNEW            )
         NULLIFY( DELT                 )
         NULLIFY( PERTIM               )
         NULLIFY( TOTIM                )
         NULLIFY( HNOFLO               )
         NULLIFY( HDRY                 )
         NULLIFY( STOPER               )
         NULLIFY( CHEDFM               )
         NULLIFY( CDDNFM               )
         NULLIFY( CBOUFM               )
         NULLIFY( IOFLG                )
         NULLIFY( VBVL                 )
         NULLIFY( VBNM                 )
         NULLIFY( PVOL_ERR             )
         NULLIFY( PDIFFPRT             )
         NULLIFY( REALTIM              )
         NULLIFY( SIMTIM_PER           )
         NULLIFY( REALTIM_PER          )
         NULLIFY( TOTPERTIM            )
         NULLIFY( SIMTIME              )
         NULLIFY( USE_LEAP_YR          )
         NULLIFY( MAX_REL_VOL_ERROR    )
         NULLIFY( MAX_REL_VOL_INVOKED  )
         NULLIFY( MIN_SOLVER_INTER     )
         NULLIFY( MIN_ITER_INPUT       )
         NULLIFY( MIN_SOLVER_INTER_NEW )
         NULLIFY( MIN_SOLVER_INTER_SP  )
         NULLIFY( DEALLOCATE_MULT      )
         NULLIFY( LISTSPLIT            )
         NULLIFY( BUDGETDB             )
         NULLIFY( INTER_INFO           )
         NULLIFY( PRNT_CUM_HEAD_CHNG   )
         NULLIFY( CUM_HEAD_CHNG        )
         NULLIFY( CUM_HEAD_CHNG_E10    )
         NULLIFY( PRNT_RES             )
         NULLIFY( PRNT_RES_LIM         )
         NULLIFY( PRNT_RES_CUM         )
         NULLIFY( PRNT_RES_CUM_ARR     )
         NULLIFY( ABOVE_GSE_LIM        )
         NULLIFY( ABOVE_GSE_PRT_LIM    )
         NULLIFY( ABOVE_GSE_PRT        )
         NULLIFY( OSCIL_DMP_OUTER      )
         NULLIFY( OSCIL_DMP_LRC        )
         NULLIFY( OSCIL_DMP_DIF        )
         NULLIFY( PRNT_CNVG_OUTER      )
         NULLIFY( PRNT_CNVG_NTERM      )
         NULLIFY( PRNT_CNVG            )
         NULLIFY( PRNT_CNVG_LRC        )
         NULLIFY( PRNT_CNVG_DIF        )
         NULLIFY( PRNT_FRES_OUTER      )
         NULLIFY( PRNT_FRES_NTERM      )
         NULLIFY( PRNT_FRES            )
         NULLIFY( PRNT_FRES_LRC        )
         NULLIFY( PRNT_FRES_DIF        )
         NULLIFY( PRNT_VERR_OUTER      )
         NULLIFY( PRNT_VERR_NTERM      )
         NULLIFY( PRNT_VERR            )
         NULLIFY( PRNT_VERR_LRC        )
         NULLIFY( PRNT_VERR_DIF        )
         NULLIFY( SAVE_HEAD            )
         NULLIFY( SAVE_HEAD_FLAG       )
         NULLIFY( PRINT_HEAD           )
         NULLIFY( PRINT_HEAD_FLAG      )
         NULLIFY( PRINT_WTAB_FLAG      )
         NULLIFY( PRINT_WDEP_FLAG      )
         NULLIFY( DAMPEN_START         )
         NULLIFY( DAMPEN_START_ITR     )
         NULLIFY( DAMPEN_START_DMP     )
         NULLIFY( ADAMP_INPUT          )
         NULLIFY( BAS_ADAMP            )
         NULLIFY( BAS_ADAMP_TOL        )
         NULLIFY( BAS_ADAMP_TOL2       )
         NULLIFY( HED_CHNG2            )
         NULLIFY( HED_CHNG3            )
         NULLIFY( HED_LOCK             )
         NULLIFY( DATE_SP              )
         NULLIFY( HAS_STARTDATE        )
      END IF
      !
      ! Deallocate the PARAM Module Variables -------------------------------------------
      !
      DEALLOCATE( PARAMDAT(IGRID)% ICLSUM    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% IPSUM     , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% INAMLOC   , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% NMLTAR    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% NZONAR    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% NPVAL     , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% PROPPRINT , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% B         , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% IACTIVE   , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% IPLOC     , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% IPCLST    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% IZON      , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% RMLT      , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% PARNAM    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% PARTYP    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% ZONNAM    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% MLTNAM    , STAT=J)
      DEALLOCATE( PARAMDAT(IGRID)% INAME     , STAT=J)
      !
      ! Nullify LGR Data Types
      !
      NULLIFY( PARAMDAT(IGRID)% ICLSUM    )
      NULLIFY( PARAMDAT(IGRID)% IPSUM     )
      NULLIFY( PARAMDAT(IGRID)% INAMLOC   )
      NULLIFY( PARAMDAT(IGRID)% NMLTAR    )
      NULLIFY( PARAMDAT(IGRID)% NZONAR    )
      NULLIFY( PARAMDAT(IGRID)% NPVAL     )
      NULLIFY( PARAMDAT(IGRID)% PROPPRINT )
      NULLIFY( PARAMDAT(IGRID)% B         )
      NULLIFY( PARAMDAT(IGRID)% IACTIVE   )
      NULLIFY( PARAMDAT(IGRID)% IPLOC     )
      NULLIFY( PARAMDAT(IGRID)% IPCLST    )
      NULLIFY( PARAMDAT(IGRID)% IZON      )
      NULLIFY( PARAMDAT(IGRID)% RMLT      )
      NULLIFY( PARAMDAT(IGRID)% PARNAM    )
      NULLIFY( PARAMDAT(IGRID)% PARTYP    )
      NULLIFY( PARAMDAT(IGRID)% ZONNAM    )
      NULLIFY( PARAMDAT(IGRID)% MLTNAM    )
      NULLIFY( PARAMDAT(IGRID)% INAME     )
      !
      ! NULLIFY local pointers that are no longer used
      !
      IF(IGRID == 1)THEN
         NULLIFY( ICLSUM    )
         NULLIFY( IPSUM     )
         NULLIFY( INAMLOC   )
         NULLIFY( NMLTAR    )
         NULLIFY( NZONAR    )
         NULLIFY( NPVAL     )
         NULLIFY( PROPPRINT )
         NULLIFY( B         )
         NULLIFY( IACTIVE   )
         NULLIFY( IPLOC     )
         NULLIFY( IPCLST    )
         NULLIFY( IZON      )
         NULLIFY( RMLT      )
         NULLIFY( PARNAM    )
         NULLIFY( PARTYP    )
         NULLIFY( ZONNAM    )
         NULLIFY( MLTNAM    )
         NULLIFY( INAME     )
      END IF
      !
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7PNT(IGRID)
      !  Change global data to a different grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
      !
      ! Setup Pointers for the GLOBAL Module Variables -------------------------------------------
      !
      NCOL            => GLOBALDAT(IGRID)% NCOL
      NROW            => GLOBALDAT(IGRID)% NROW
      NLAY            => GLOBALDAT(IGRID)% NLAY
      NPER            => GLOBALDAT(IGRID)% NPER
      NBOTM           => GLOBALDAT(IGRID)% NBOTM
      NCNFBD          => GLOBALDAT(IGRID)% NCNFBD
      ITMUNI          => GLOBALDAT(IGRID)% ITMUNI
      LENUNI          => GLOBALDAT(IGRID)% LENUNI
      IXSEC           => GLOBALDAT(IGRID)% IXSEC
      ITRSS           => GLOBALDAT(IGRID)% ITRSS
      INBAS           => GLOBALDAT(IGRID)% INBAS
      IFREFM          => GLOBALDAT(IGRID)% IFREFM
      NODES           => GLOBALDAT(IGRID)% NODES
      IOUT            => GLOBALDAT(IGRID)% IOUT
      MXITER          => GLOBALDAT(IGRID)% MXITER
      IRESTART        => GLOBALDAT(IGRID)% IRESTART
      KPERSTART       => GLOBALDAT(IGRID)% KPERSTART
      KSTPSTART       => GLOBALDAT(IGRID)% KSTPSTART
      IUNITSTART      => GLOBALDAT(IGRID)% IUNITSTART
      BACKTRACKING    => GLOBALDAT(IGRID)% BACKTRACKING
      IUNIT           => GLOBALDAT(IGRID)% IUNIT
      HNEW            => GLOBALDAT(IGRID)% HNEW
      HNEW_OLD        => GLOBALDAT(IGRID)% HNEW_OLD
      WTABLE          => GLOBALDAT(IGRID)% WTABLE
      WTABLE_OLD      => GLOBALDAT(IGRID)% WTABLE_OLD
      UPLAY           => GLOBALDAT(IGRID)% UPLAY
      LBOTM           => GLOBALDAT(IGRID)% LBOTM
      LAYCBD          => GLOBALDAT(IGRID)% LAYCBD
      LAYHDT          => GLOBALDAT(IGRID)% LAYHDT
      LAYHDS          => GLOBALDAT(IGRID)% LAYHDS
      PERLEN          => GLOBALDAT(IGRID)% PERLEN
      NSTP            => GLOBALDAT(IGRID)% NSTP
      TSMULT          => GLOBALDAT(IGRID)% TSMULT
      ISSFLG          => GLOBALDAT(IGRID)% ISSFLG
      DELR            => GLOBALDAT(IGRID)% DELR
      DELC            => GLOBALDAT(IGRID)% DELC
      AREA            => GLOBALDAT(IGRID)% AREA
      GSE             => GLOBALDAT(IGRID)% GSE
      BOTM            => GLOBALDAT(IGRID)% BOTM
      HOLD            => GLOBALDAT(IGRID)% HOLD
      IBOUND          => GLOBALDAT(IGRID)% IBOUND
      CR              => GLOBALDAT(IGRID)% CR
      CC              => GLOBALDAT(IGRID)% CC
      CV              => GLOBALDAT(IGRID)% CV
      HCOF            => GLOBALDAT(IGRID)% HCOF
      RHS             => GLOBALDAT(IGRID)% RHS
      BUFF            => GLOBALDAT(IGRID)% BUFF
      RBUF            => GLOBALDAT(IGRID)% RBUF
      STRT            => GLOBALDAT(IGRID)% STRT
      DDREF           => GLOBALDAT(IGRID)% DDREF
      ALLOC_DDREF     => GLOBALDAT(IGRID)% ALLOC_DDREF
      UPLAY_IDX       => GLOBALDAT(IGRID)% UPLAY_IDX
      XYGRID          => GLOBALDAT(IGRID)% XYGRID
      SPTIM           => GLOBALDAT(IGRID)% SPTIM
      SPSTART         => GLOBALDAT(IGRID)% SPSTART
      SPEND           => GLOBALDAT(IGRID)% SPEND
      NOCBC           => GLOBALDAT(IGRID)% NOCBC
      CBC_GLOBAL_UNIT => GLOBALDAT(IGRID)% CBC_GLOBAL_UNIT
      RCloseBAS       => GLOBALDAT(IGRID)% RCloseBAS 
      HCloseBAS       => GLOBALDAT(IGRID)% HCloseBAS 
      RCloseL2BAS     => GLOBALDAT(IGRID)% RCloseL2BAS
      !
      ! Setup Pointers for the BAS Module Variables -------------------------------------------
      !
      MSUM                 => GWFBASDAT(IGRID)% MSUM
      IUBGT                => GWFBASDAT(IGRID)% IUBGT
      IHEDFM               => GWFBASDAT(IGRID)% IHEDFM
      IHEDUN               => GWFBASDAT(IGRID)% IHEDUN
      IDDNFM               => GWFBASDAT(IGRID)% IDDNFM
      IDDNUN               => GWFBASDAT(IGRID)% IDDNUN
      IBOUUN               => GWFBASDAT(IGRID)% IBOUUN
      LBHDSV               => GWFBASDAT(IGRID)% LBHDSV
      LBDDSV               => GWFBASDAT(IGRID)% LBDDSV
      LBBOSV               => GWFBASDAT(IGRID)% LBBOSV
      IBUDFL               => GWFBASDAT(IGRID)% IBUDFL
      ICBCFL               => GWFBASDAT(IGRID)% ICBCFL
      IHDDFL               => GWFBASDAT(IGRID)% IHDDFL
      IAUXSV               => GWFBASDAT(IGRID)% IAUXSV
      IBDOPT               => GWFBASDAT(IGRID)% IBDOPT
      IPRTIM               => GWFBASDAT(IGRID)% IPRTIM
      IPEROC               => GWFBASDAT(IGRID)% IPEROC
      ITSOC                => GWFBASDAT(IGRID)% ITSOC
      ICHFLG               => GWFBASDAT(IGRID)% ICHFLG
      IDDREF               => GWFBASDAT(IGRID)% IDDREF
      IDDREFNEW            => GWFBASDAT(IGRID)% IDDREFNEW
      DELT                 => GWFBASDAT(IGRID)% DELT
      PERTIM               => GWFBASDAT(IGRID)% PERTIM
      TOTIM                => GWFBASDAT(IGRID)% TOTIM
      HNOFLO               => GWFBASDAT(IGRID)% HNOFLO
      HDRY                 => GWFBASDAT(IGRID)% HDRY
      STOPER               => GWFBASDAT(IGRID)% STOPER
      CHEDFM               => GWFBASDAT(IGRID)% CHEDFM
      CDDNFM               => GWFBASDAT(IGRID)% CDDNFM
      CBOUFM               => GWFBASDAT(IGRID)% CBOUFM
      IOFLG                => GWFBASDAT(IGRID)% IOFLG
      VBVL                 => GWFBASDAT(IGRID)% VBVL
      VBNM                 => GWFBASDAT(IGRID)% VBNM
      PVOL_ERR             => GWFBASDAT(IGRID)% PVOL_ERR
      PDIFFPRT             => GWFBASDAT(IGRID)% PDIFFPRT
      REALTIM              => GWFBASDAT(IGRID)% REALTIM
      SIMTIM_PER           => GWFBASDAT(IGRID)% SIMTIM_PER
      REALTIM_PER          => GWFBASDAT(IGRID)% REALTIM_PER
      TOTPERTIM            => GWFBASDAT(IGRID)% TOTPERTIM
      SIMTIME              => GWFBASDAT(IGRID)% SIMTIME
      USE_LEAP_YR          => GWFBASDAT(IGRID)% USE_LEAP_YR
      MAX_REL_VOL_ERROR    => GWFBASDAT(IGRID)% MAX_REL_VOL_ERROR
      MAX_REL_VOL_INVOKED  => GWFBASDAT(IGRID)% MAX_REL_VOL_INVOKED
      MIN_SOLVER_INTER     => GWFBASDAT(IGRID)% MIN_SOLVER_INTER
      MIN_ITER_INPUT       => GWFBASDAT(IGRID)% MIN_ITER_INPUT
      MIN_SOLVER_INTER_NEW => GWFBASDAT(IGRID)% MIN_SOLVER_INTER_NEW
      MIN_SOLVER_INTER_SP  => GWFBASDAT(IGRID)% MIN_SOLVER_INTER_SP
      DEALLOCATE_MULT      => GWFBASDAT(IGRID)% DEALLOCATE_MULT
      LISTSPLIT            => GWFBASDAT(IGRID)% LISTSPLIT
      BUDGETDB             => GWFBASDAT(IGRID)% BUDGETDB
      INTER_INFO           => GWFBASDAT(IGRID)% INTER_INFO
      PRNT_CUM_HEAD_CHNG   => GWFBASDAT(IGRID)% PRNT_CUM_HEAD_CHNG
      CUM_HEAD_CHNG        => GWFBASDAT(IGRID)% CUM_HEAD_CHNG
      CUM_HEAD_CHNG_E10    => GWFBASDAT(IGRID)% CUM_HEAD_CHNG_E10
      PRNT_RES             => GWFBASDAT(IGRID)% PRNT_RES
      PRNT_RES_LIM         => GWFBASDAT(IGRID)% PRNT_RES_LIM
      PRNT_RES_CUM         => GWFBASDAT(IGRID)% PRNT_RES_CUM
      PRNT_RES_CUM_ARR     => GWFBASDAT(IGRID)% PRNT_RES_CUM_ARR
      ABOVE_GSE_LIM        => GWFBASDAT(IGRID)% ABOVE_GSE_LIM
      ABOVE_GSE_PRT_LIM    => GWFBASDAT(IGRID)% ABOVE_GSE_PRT_LIM
      ABOVE_GSE_PRT        => GWFBASDAT(IGRID)% ABOVE_GSE_PRT
      OSCIL_DMP_OUTER      => GWFBASDAT(IGRID)% OSCIL_DMP_OUTER
      OSCIL_DMP_LRC        => GWFBASDAT(IGRID)% OSCIL_DMP_LRC
      OSCIL_DMP_DIF        => GWFBASDAT(IGRID)% OSCIL_DMP_DIF
      PRNT_CNVG_OUTER      => GWFBASDAT(IGRID)% PRNT_CNVG_OUTER
      PRNT_CNVG_NTERM      => GWFBASDAT(IGRID)% PRNT_CNVG_NTERM
      PRNT_CNVG            => GWFBASDAT(IGRID)% PRNT_CNVG
      PRNT_CNVG_LRC        => GWFBASDAT(IGRID)% PRNT_CNVG_LRC
      PRNT_CNVG_DIF        => GWFBASDAT(IGRID)% PRNT_CNVG_DIF
      PRNT_FRES_OUTER      => GWFBASDAT(IGRID)% PRNT_FRES_OUTER
      PRNT_FRES_NTERM      => GWFBASDAT(IGRID)% PRNT_FRES_NTERM
      PRNT_FRES            => GWFBASDAT(IGRID)% PRNT_FRES
      PRNT_FRES_LRC        => GWFBASDAT(IGRID)% PRNT_FRES_LRC
      PRNT_FRES_DIF        => GWFBASDAT(IGRID)% PRNT_FRES_DIF
      PRNT_VERR_OUTER      => GWFBASDAT(IGRID)% PRNT_VERR_OUTER
      PRNT_VERR_NTERM      => GWFBASDAT(IGRID)% PRNT_VERR_NTERM
      PRNT_VERR            => GWFBASDAT(IGRID)% PRNT_VERR
      PRNT_VERR_LRC        => GWFBASDAT(IGRID)% PRNT_VERR_LRC
      PRNT_VERR_DIF        => GWFBASDAT(IGRID)% PRNT_VERR_DIF
      SAVE_HEAD            => GWFBASDAT(IGRID)% SAVE_HEAD
      SAVE_HEAD_FLAG       => GWFBASDAT(IGRID)% SAVE_HEAD_FLAG
      PRINT_HEAD           => GWFBASDAT(IGRID)% PRINT_HEAD
      PRINT_HEAD_FLAG      => GWFBASDAT(IGRID)% PRINT_HEAD_FLAG
      PRINT_WTAB_FLAG      => GWFBASDAT(IGRID)% PRINT_WTAB_FLAG
      PRINT_WDEP_FLAG      => GWFBASDAT(IGRID)% PRINT_WDEP_FLAG
      DAMPEN_START         => GWFBASDAT(IGRID)% DAMPEN_START
      DAMPEN_START_ITR     => GWFBASDAT(IGRID)% DAMPEN_START_ITR
      DAMPEN_START_DMP     => GWFBASDAT(IGRID)% DAMPEN_START_DMP
      ADAMP_INPUT          => GWFBASDAT(IGRID)% ADAMP_INPUT
      BAS_ADAMP            => GWFBASDAT(IGRID)% BAS_ADAMP
      BAS_ADAMP_TOL        => GWFBASDAT(IGRID)% BAS_ADAMP_TOL
      BAS_ADAMP_TOL2       => GWFBASDAT(IGRID)% BAS_ADAMP_TOL2
      HED_CHNG2            => GWFBASDAT(IGRID)% HED_CHNG2
      HED_CHNG3            => GWFBASDAT(IGRID)% HED_CHNG3
      HED_LOCK             => GWFBASDAT(IGRID)% HED_LOCK
      DATE_SP              => GWFBASDAT(IGRID)% DATE_SP
      HAS_STARTDATE        => GWFBASDAT(IGRID)% HAS_STARTDATE
      !
      ! Setup Pointers for the PARAM Module Variables -------------------------------------------
      !
      ICLSUM    => PARAMDAT(IGRID)% ICLSUM
      IPSUM     => PARAMDAT(IGRID)% IPSUM
      INAMLOC   => PARAMDAT(IGRID)% INAMLOC
      NMLTAR    => PARAMDAT(IGRID)% NMLTAR
      NZONAR    => PARAMDAT(IGRID)% NZONAR
      NPVAL     => PARAMDAT(IGRID)% NPVAL
      PROPPRINT => PARAMDAT(IGRID)% PROPPRINT
      B         => PARAMDAT(IGRID)% B
      IACTIVE   => PARAMDAT(IGRID)% IACTIVE
      IPLOC     => PARAMDAT(IGRID)% IPLOC
      IPCLST    => PARAMDAT(IGRID)% IPCLST
      IZON      => PARAMDAT(IGRID)% IZON
      RMLT      => PARAMDAT(IGRID)% RMLT
      PARNAM    => PARAMDAT(IGRID)% PARNAM
      PARTYP    => PARAMDAT(IGRID)% PARTYP
      ZONNAM    => PARAMDAT(IGRID)% ZONNAM
      MLTNAM    => PARAMDAT(IGRID)% MLTNAM
      INAME     => PARAMDAT(IGRID)% INAME
      !
      END SUBROUTINE
      !
      SUBROUTINE SGWF2BAS7PSV(IGRID)
C  Save global data for a grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
      !
      ! Setup Pointers for the GLOBAL Module Variables -------------------------------------------
      !
      GLOBALDAT(IGRID)% NCOL            => NCOL
      GLOBALDAT(IGRID)% NROW            => NROW
      GLOBALDAT(IGRID)% NLAY            => NLAY
      GLOBALDAT(IGRID)% NPER            => NPER
      GLOBALDAT(IGRID)% NBOTM           => NBOTM
      GLOBALDAT(IGRID)% NCNFBD          => NCNFBD
      GLOBALDAT(IGRID)% ITMUNI          => ITMUNI
      GLOBALDAT(IGRID)% LENUNI          => LENUNI
      GLOBALDAT(IGRID)% IXSEC           => IXSEC
      GLOBALDAT(IGRID)% ITRSS           => ITRSS
      GLOBALDAT(IGRID)% INBAS           => INBAS
      GLOBALDAT(IGRID)% IFREFM          => IFREFM
      GLOBALDAT(IGRID)% NODES           => NODES
      GLOBALDAT(IGRID)% IOUT            => IOUT
      GLOBALDAT(IGRID)% MXITER          => MXITER
      GLOBALDAT(IGRID)% IRESTART        => IRESTART
      GLOBALDAT(IGRID)% KPERSTART       => KPERSTART
      GLOBALDAT(IGRID)% KSTPSTART       => KSTPSTART
      GLOBALDAT(IGRID)% IUNITSTART      => IUNITSTART
      GLOBALDAT(IGRID)% BACKTRACKING    => BACKTRACKING
      GLOBALDAT(IGRID)% IUNIT           => IUNIT
      GLOBALDAT(IGRID)% HNEW            => HNEW
      GLOBALDAT(IGRID)% HNEW_OLD        => HNEW_OLD
      GLOBALDAT(IGRID)% WTABLE          => WTABLE
      GLOBALDAT(IGRID)% WTABLE_OLD      => WTABLE_OLD
      GLOBALDAT(IGRID)% UPLAY           => UPLAY
      GLOBALDAT(IGRID)% LBOTM           => LBOTM
      GLOBALDAT(IGRID)% LAYCBD          => LAYCBD
      GLOBALDAT(IGRID)% LAYHDT          => LAYHDT
      GLOBALDAT(IGRID)% LAYHDS          => LAYHDS
      GLOBALDAT(IGRID)% PERLEN          => PERLEN
      GLOBALDAT(IGRID)% NSTP            => NSTP
      GLOBALDAT(IGRID)% TSMULT          => TSMULT
      GLOBALDAT(IGRID)% ISSFLG          => ISSFLG
      GLOBALDAT(IGRID)% DELR            => DELR
      GLOBALDAT(IGRID)% DELC            => DELC
      GLOBALDAT(IGRID)% AREA            => AREA
      GLOBALDAT(IGRID)% GSE             => GSE
      GLOBALDAT(IGRID)% BOTM            => BOTM
      GLOBALDAT(IGRID)% HOLD            => HOLD
      GLOBALDAT(IGRID)% IBOUND          => IBOUND
      GLOBALDAT(IGRID)% CR              => CR
      GLOBALDAT(IGRID)% CC              => CC
      GLOBALDAT(IGRID)% CV              => CV
      GLOBALDAT(IGRID)% HCOF            => HCOF
      GLOBALDAT(IGRID)% RHS             => RHS
      GLOBALDAT(IGRID)% BUFF            => BUFF
      GLOBALDAT(IGRID)% RBUF            => RBUF
      GLOBALDAT(IGRID)% STRT            => STRT
      GLOBALDAT(IGRID)% DDREF           => DDREF
      GLOBALDAT(IGRID)% ALLOC_DDREF     => ALLOC_DDREF
      GLOBALDAT(IGRID)% UPLAY_IDX       => UPLAY_IDX
      GLOBALDAT(IGRID)% XYGRID          => XYGRID
      GLOBALDAT(IGRID)% SPTIM           => SPTIM
      GLOBALDAT(IGRID)% SPSTART         => SPSTART
      GLOBALDAT(IGRID)% SPEND           => SPEND
      GLOBALDAT(IGRID)% NOCBC           => NOCBC
      GLOBALDAT(IGRID)% CBC_GLOBAL_UNIT => CBC_GLOBAL_UNIT
      GLOBALDAT(IGRID)% RCloseBAS       => RCloseBAS 
      GLOBALDAT(IGRID)% HCloseBAS       => HCloseBAS 
      GLOBALDAT(IGRID)% RCloseL2BAS     => RCloseL2BAS
      !
      ! Setup Pointers for the BAS Module Variables -------------------------------------------
      !
      GWFBASDAT(IGRID)% MSUM                 => MSUM
      GWFBASDAT(IGRID)% IUBGT                => IUBGT
      GWFBASDAT(IGRID)% IHEDFM               => IHEDFM
      GWFBASDAT(IGRID)% IHEDUN               => IHEDUN
      GWFBASDAT(IGRID)% IDDNFM               => IDDNFM
      GWFBASDAT(IGRID)% IDDNUN               => IDDNUN
      GWFBASDAT(IGRID)% IBOUUN               => IBOUUN
      GWFBASDAT(IGRID)% LBHDSV               => LBHDSV
      GWFBASDAT(IGRID)% LBDDSV               => LBDDSV
      GWFBASDAT(IGRID)% LBBOSV               => LBBOSV
      GWFBASDAT(IGRID)% IBUDFL               => IBUDFL
      GWFBASDAT(IGRID)% ICBCFL               => ICBCFL
      GWFBASDAT(IGRID)% IHDDFL               => IHDDFL
      GWFBASDAT(IGRID)% IAUXSV               => IAUXSV
      GWFBASDAT(IGRID)% IBDOPT               => IBDOPT
      GWFBASDAT(IGRID)% IPRTIM               => IPRTIM
      GWFBASDAT(IGRID)% IPEROC               => IPEROC
      GWFBASDAT(IGRID)% ITSOC                => ITSOC
      GWFBASDAT(IGRID)% ICHFLG               => ICHFLG
      GWFBASDAT(IGRID)% IDDREF               => IDDREF
      GWFBASDAT(IGRID)% IDDREFNEW            => IDDREFNEW
      GWFBASDAT(IGRID)% DELT                 => DELT
      GWFBASDAT(IGRID)% PERTIM               => PERTIM
      GWFBASDAT(IGRID)% TOTIM                => TOTIM
      GWFBASDAT(IGRID)% HNOFLO               => HNOFLO
      GWFBASDAT(IGRID)% HDRY                 => HDRY
      GWFBASDAT(IGRID)% STOPER               => STOPER
      GWFBASDAT(IGRID)% CHEDFM               => CHEDFM
      GWFBASDAT(IGRID)% CDDNFM               => CDDNFM
      GWFBASDAT(IGRID)% CBOUFM               => CBOUFM
      GWFBASDAT(IGRID)% IOFLG                => IOFLG
      GWFBASDAT(IGRID)% VBVL                 => VBVL
      GWFBASDAT(IGRID)% VBNM                 => VBNM
      GWFBASDAT(IGRID)% PVOL_ERR             => PVOL_ERR
      GWFBASDAT(IGRID)% PDIFFPRT             => PDIFFPRT
      GWFBASDAT(IGRID)% REALTIM              => REALTIM
      GWFBASDAT(IGRID)% SIMTIM_PER           => SIMTIM_PER
      GWFBASDAT(IGRID)% REALTIM_PER          => REALTIM_PER
      GWFBASDAT(IGRID)% TOTPERTIM            => TOTPERTIM
      GWFBASDAT(IGRID)% SIMTIME              => SIMTIME
      GWFBASDAT(IGRID)% USE_LEAP_YR          => USE_LEAP_YR
      GWFBASDAT(IGRID)% MAX_REL_VOL_ERROR    => MAX_REL_VOL_ERROR
      GWFBASDAT(IGRID)% MAX_REL_VOL_INVOKED  => MAX_REL_VOL_INVOKED
      GWFBASDAT(IGRID)% MIN_SOLVER_INTER     => MIN_SOLVER_INTER
      GWFBASDAT(IGRID)% MIN_ITER_INPUT       => MIN_ITER_INPUT
      GWFBASDAT(IGRID)% MIN_SOLVER_INTER_NEW => MIN_SOLVER_INTER_NEW
      GWFBASDAT(IGRID)% MIN_SOLVER_INTER_SP  => MIN_SOLVER_INTER_SP
      GWFBASDAT(IGRID)% DEALLOCATE_MULT      => DEALLOCATE_MULT
      GWFBASDAT(IGRID)% LISTSPLIT            => LISTSPLIT
      GWFBASDAT(IGRID)% BUDGETDB             => BUDGETDB
      GWFBASDAT(IGRID)% INTER_INFO           => INTER_INFO
      GWFBASDAT(IGRID)% PRNT_CUM_HEAD_CHNG   => PRNT_CUM_HEAD_CHNG
      GWFBASDAT(IGRID)% CUM_HEAD_CHNG        => CUM_HEAD_CHNG
      GWFBASDAT(IGRID)% CUM_HEAD_CHNG_E10    => CUM_HEAD_CHNG_E10
      GWFBASDAT(IGRID)% PRNT_RES             => PRNT_RES
      GWFBASDAT(IGRID)% PRNT_RES_LIM         => PRNT_RES_LIM
      GWFBASDAT(IGRID)% PRNT_RES_CUM         => PRNT_RES_CUM
      GWFBASDAT(IGRID)% PRNT_RES_CUM_ARR     => PRNT_RES_CUM_ARR
      GWFBASDAT(IGRID)% ABOVE_GSE_LIM        => ABOVE_GSE_LIM
      GWFBASDAT(IGRID)% ABOVE_GSE_PRT_LIM    => ABOVE_GSE_PRT_LIM
      GWFBASDAT(IGRID)% ABOVE_GSE_PRT        => ABOVE_GSE_PRT
      GWFBASDAT(IGRID)% OSCIL_DMP_OUTER      => OSCIL_DMP_OUTER
      GWFBASDAT(IGRID)% OSCIL_DMP_LRC        => OSCIL_DMP_LRC
      GWFBASDAT(IGRID)% OSCIL_DMP_DIF        => OSCIL_DMP_DIF
      GWFBASDAT(IGRID)% PRNT_CNVG_OUTER      => PRNT_CNVG_OUTER
      GWFBASDAT(IGRID)% PRNT_CNVG_NTERM      => PRNT_CNVG_NTERM
      GWFBASDAT(IGRID)% PRNT_CNVG            => PRNT_CNVG
      GWFBASDAT(IGRID)% PRNT_CNVG_LRC        => PRNT_CNVG_LRC
      GWFBASDAT(IGRID)% PRNT_CNVG_DIF        => PRNT_CNVG_DIF
      GWFBASDAT(IGRID)% PRNT_FRES_OUTER      => PRNT_FRES_OUTER
      GWFBASDAT(IGRID)% PRNT_FRES_NTERM      => PRNT_FRES_NTERM
      GWFBASDAT(IGRID)% PRNT_FRES            => PRNT_FRES
      GWFBASDAT(IGRID)% PRNT_FRES_LRC        => PRNT_FRES_LRC
      GWFBASDAT(IGRID)% PRNT_FRES_DIF        => PRNT_FRES_DIF
      GWFBASDAT(IGRID)% PRNT_VERR_OUTER      => PRNT_VERR_OUTER
      GWFBASDAT(IGRID)% PRNT_VERR_NTERM      => PRNT_VERR_NTERM
      GWFBASDAT(IGRID)% PRNT_VERR            => PRNT_VERR
      GWFBASDAT(IGRID)% PRNT_VERR_LRC        => PRNT_VERR_LRC
      GWFBASDAT(IGRID)% PRNT_VERR_DIF        => PRNT_VERR_DIF
      GWFBASDAT(IGRID)% SAVE_HEAD            => SAVE_HEAD
      GWFBASDAT(IGRID)% SAVE_HEAD_FLAG       => SAVE_HEAD_FLAG
      GWFBASDAT(IGRID)% PRINT_HEAD           => PRINT_HEAD
      GWFBASDAT(IGRID)% PRINT_HEAD_FLAG      => PRINT_HEAD_FLAG
      GWFBASDAT(IGRID)% PRINT_WTAB_FLAG      => PRINT_WTAB_FLAG
      GWFBASDAT(IGRID)% PRINT_WDEP_FLAG      => PRINT_WDEP_FLAG
      GWFBASDAT(IGRID)% DAMPEN_START         => DAMPEN_START
      GWFBASDAT(IGRID)% DAMPEN_START_ITR     => DAMPEN_START_ITR
      GWFBASDAT(IGRID)% DAMPEN_START_DMP     => DAMPEN_START_DMP
      GWFBASDAT(IGRID)% ADAMP_INPUT          => ADAMP_INPUT
      GWFBASDAT(IGRID)% BAS_ADAMP            => BAS_ADAMP
      GWFBASDAT(IGRID)% BAS_ADAMP_TOL        => BAS_ADAMP_TOL
      GWFBASDAT(IGRID)% BAS_ADAMP_TOL2       => BAS_ADAMP_TOL2
      GWFBASDAT(IGRID)% HED_CHNG2            => HED_CHNG2
      GWFBASDAT(IGRID)% HED_CHNG3            => HED_CHNG3
      GWFBASDAT(IGRID)% HED_LOCK             => HED_LOCK
      GWFBASDAT(IGRID)% DATE_SP              => DATE_SP
      GWFBASDAT(IGRID)% HAS_STARTDATE        => HAS_STARTDATE
      !
      ! Setup Pointers for the PARAM Module Variables -------------------------------------------
      !
      PARAMDAT(IGRID)% ICLSUM    => ICLSUM
      PARAMDAT(IGRID)% IPSUM     => IPSUM
      PARAMDAT(IGRID)% INAMLOC   => INAMLOC
      PARAMDAT(IGRID)% NMLTAR    => NMLTAR
      PARAMDAT(IGRID)% NZONAR    => NZONAR
      PARAMDAT(IGRID)% NPVAL     => NPVAL
      PARAMDAT(IGRID)% PROPPRINT => PROPPRINT
      PARAMDAT(IGRID)% B         => B
      PARAMDAT(IGRID)% IACTIVE   => IACTIVE
      PARAMDAT(IGRID)% IPLOC     => IPLOC
      PARAMDAT(IGRID)% IPCLST    => IPCLST
      PARAMDAT(IGRID)% IZON      => IZON
      PARAMDAT(IGRID)% RMLT      => RMLT
      PARAMDAT(IGRID)% PARNAM    => PARNAM
      PARAMDAT(IGRID)% PARTYP    => PARTYP
      PARAMDAT(IGRID)% ZONNAM    => ZONNAM
      PARAMDAT(IGRID)% MLTNAM    => MLTNAM
      PARAMDAT(IGRID)% INAME     => INAME
      !
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

      PURE SUBROUTINE CHECK_CBC_GLOBAL_UNIT(ICBC)
      USE GLOBAL, ONLY: NOCBC, CBC_GLOBAL_UNIT
      IMPLICIT NONE
      INTEGER, intent(inout) :: ICBC
      !
      IF(CBC_GLOBAL_UNIT /= 0)  ICBC = CBC_GLOBAL_UNIT
      !
      IF( NOCBC > 1 ) ICBC = 0
      !
      END SUBROUTINE
