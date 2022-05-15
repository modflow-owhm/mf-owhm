      MODULE ZONBUDMODULE
        USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: real32, real64
        PRIVATE:: real32, real64
        INTEGER:: IPREC
        LOGICAL:: NO_CONST_HEAD
        REAL(real32), DIMENSION(:,:,:), ALLOCATABLE:: BUFF
        REAL(real64), DIMENSION(:,:,:), ALLOCATABLE:: BUFFD
      END MODULE
C     ******************************************************************
C     Program to compute and print volumetric budgets over subregions
C     of a flow system that is being simulated using the USGS Modular
C     Three-Dimensional Finite-Difference Ground-Water Flow Model.
C
C     This program is documented in USGS Open-File Report 90-392,
C     written by Arlen W. Harbaugh
C
C     Jan., 2007 -- Updated to use allocatable memory and work with single
C       or double precision budget files.  Must be compiled with default
C       4-byte real numbers so that single-precision binary budget files
C       can be read.  All computations are done in double precision.
C
C     Jan. 29, 2000 -- Updated to work with MODFLOW's COMPACT BUDGET
C     option.
C
C     Jun. 19, 2021 -- Updated to work without 'CONSTANT HEAD'. 
C                      Added a modified version of U2DINT to read input
C                       This adds OPEN/CLOSE to file input option, but keeps ZoneBudget style EXTERNAL.
C                      Added better error messages.
C                      Minor refactoring to use newer fortran features.
C
C     ******************************************************************
C
      PROGRAM ZONEBUDGET
C
      USE ZONBUDMODULE
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: real32, real64
      IMPLICIT NONE
      !
      INTEGER, PARAMETER:: NTRDIM = 512  ,
     +                     MXCOMP = 1024 ,
     +                     MXZWCZ = 64   ,
     +                     MXZONE = 1024
C-----   NTRDIM must be greater than or equal to the number of budget
C-----          terms, other than flow between zones, that will appear
C-----          the budget.  In the original model, there is a maximum
C-----          of 8 terms -- constant-head, storage, wells, rivers,
C-----          drains, recharge, general-head boundaries, and
C-----          evapotranspiration.
C-----   MXZONE is the maximum number of zones.
C-----   NZDIM  is the actual number of zones being used.
C-----   MXCOMP is the maximum number of composite zones.
C-----   MXZWCZ is the maximum number of numeric zones within each
C-----          composite zone.
C-----   LSTZON is a list of all of the zones.
C-----          used.
      INTEGER,      DIMENSION(:,:,:), ALLOCATABLE:: IZONE ,
     +                                              ICH   ,
     +                                              IBUFF
      REAL(real64), DIMENSION(:,:,:), ALLOCATABLE:: VBVL  ,        ! Equivalent to DOUBLE PRECISION
     +                                              VBZNFL
      !
      REAL(real32), DIMENSION(20):: VAL
      REAL(real32):: DELT, PERTIM, TOTIM
      !
      REAL(real64), DIMENSION(20):: VALD
      REAL(real64):: DELTD, PERTIMD, TOTIMD, DZERO, TOTIMDOLD
      !
      INTEGER, DIMENSION(MXZWCZ,MXCOMP) :: ICOMP
      INTEGER, DIMENSION(MXCOMP)        :: NZWCZ
      INTEGER, DIMENSION(0:MXZONE)      :: LSTZON
      INTEGER, DIMENSION(2,10)          :: ITIME
      !
      CHARACTER(10), DIMENSION(MXCOMP):: NAMCOMP
      CHARACTER(16), DIMENSION(NTRDIM):: VBNM
      !
      CHARACTER(256):: LINE
      CHARACTER(240):: TITLE, NAME, BASENAME
      CHARACTER( 16):: TEXT, CTMP
      CHARACTER(  1):: METHOD, IANS
      CHARACTER( 40):: VERSON
      !
      INTEGER:: NROW, NCOL, NLAY, KPER, KSTP, ITYPE, ICALC
      INTEGER:: INZN1, INZN2, INBUD, IOUT,
     +          IUZBLST, IUCSV, IUCSV2,
     +          I, J, K, K1, K2, N, MSUM, NLIST, NVAL,
     +          NR, NC, NL, NZDIM, MSUMCH, NCOMP, NTIMES, 
     +          LOC, LLOC, ISTART, ISTOP, istat
      INTEGER:: IN
      REAL:: R
      !
      CHARACTER,    PARAMETER:: LF = NEW_LINE(" ")
      CHARACTER(*), PARAMETER:: ERRHED =  
     +                             LF//LF//'Binary Read Failure: '//LF
      CHARACTER(*), PARAMETER:: ERRAFT = 
     +LF//LF//'ZoneBudget specific output files should be fine and '//
     +LF//'contain any output written from before this error. '//LF//
     +LF//'A possible cause for this is: '//LF//
     +'1) The binary file is corrupt and needs to be remade. '//LF//
     +'    - Recommended to delete the file manually before '//
     +                                          'remaking it.'//LF//LF//
     +'2) The NAME file did not include "REPLACE" or "WRITE" '//LF//
     +'   when declaring the Cell-By-Cell binary file name. '//LF//
     +'    - For example, you should have something like: '//LF//
     +'          DATA(BINARY)  55  ./output/CBC.bin  WRITE '//LF//
     +'      to indicate that "./output/CBC.bin" should be '//LF//
     +'      deleted if it exists, then create the '//
     +                                 'file for writing'//LF//LF//
     +'    - Another example, is say you run the model once '//LF//
     +'      and get a CBC file that is 63KB in size, then run '//LF//
     +'      the model again but it writes only 50KB to the CBC'//LF//
     +'      ZoneBudget reads the new model output (first 50KB)'//LF//
     +'      and the older model output (last 13KB).'//LF//LF//
     +'3) The binary is contains double precision numbers '//LF//
     +'   but ZoneBudget thinks its single precision, '//LF//
     +'   is single precision when is double, '//LF//
     +'   or the binary contains a mixture of the two.'//LF//LF//
     +'4) The binary is not compatible with ZoneBudget 3.3. '//LF//
     +'    - Contact a developer for help'//LF//LF
      CHARACTER(:), ALLOCATABLE:: ERRMSG
      !
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
      !
      INTERFACE
              PURE FUNCTION INT2STR(IVAL)
                INTEGER,       INTENT(IN):: IVAL
                CHARACTER(:), ALLOCATABLE:: INT2STR
              END FUNCTION
      END INTERFACE
      
      VERSON='ZONEBUDGET version 3.3'
C
C-----DEFINE INPUT AND OUTPUT UNITS AND INITIALIZE OTHER VARIABLES
      INZN1    = 10
      INZN2    = 11
      INBUD    = 12
      NCOMP    = 0
      IOUT     = 0
      IUZBLST  = 0
      IUCSV    = 0
      IUCSV2   = 0
      K1       = 0
      K2       = 0
      MSUM     = 0
      DZERO    = 0.0_real64
      NLIST    = 0
      NVAL     = 1
      TOTIMD   = -1.0_real64
      TOTIMDOLD= -1.0_real64
      NAMCOMP  = ''
      VBNM     = ''
      istat    = 0
C
C-----TELL THE USER WHAT THIS PROGRAM IS
      WRITE(*,*)
      WRITE(*,4) VERSON
4     FORMAT(1X,A,/,/,
     1' Program to compute a flow budget for subregions of a model using
     2'/' budget data from MODFLOW-2005 flavored models.')
      WRITE(*,'(/,A)')' With enhancements for:'
      WRITE(*,'(2(A,/), /, 2(A,/))')  
     +'                       MODFLOW',
     +'           ONE-WATER HYDROLOGIC-FLOW MODEL',
     +'   The U.S. Geological Survey Modular Finite-Difference',
     +'          Conjunctive Use Simulation Program'
      WRITE(*,'(/,*(A,/))') 
     +'This ZoneBudget code and compiled executable are located at:',
     +'',
     +'         https://code.usgs.gov/modflow/mf-owhm','',
     +'For download info see the README.md or README.pdf',
     +'For bug fixes and new features see the '//
     +                     'CHANGELOG.md or CHANGELOG.pdf'
C
C-----OPEN LISTING FILE(S)
      WRITE(*,*)
      WRITE(*,*)' Enter a LISTING FILE for results',
     1                     ' or a base name and file types:'
      READ(*,'(A)') NAME
      !
      LLOC=1
      CALL PARSE_WORD(NAME, LLOC, ISTART, ISTOP)
      BASENAME=NAME(ISTART:ISTOP)
      CALL PARSE_WORD_UP(NAME, LLOC, ISTART, ISTOP)
      IF(ISTART <= ISTOP .and. NAME(ISTART:ISTOP).NE.' ' .and. 
     +                         NAME(ISTART:ISTART).NE.'#') THEN
8       IF(NAME(ISTART:ISTOP).EQ.'CSV') THEN
          IUCSV=14
          OPEN(UNIT=IUCSV,FILE=TRIM(BASENAME)//'.csv',ERR=7)
          WRITE(*,*) 'CSV output file: ',TRIM(BASENAME)//'.csv'
        ELSE IF(NAME(ISTART:ISTOP).EQ.'CSV2') THEN
          IUCSV2=15
          OPEN(UNIT=IUCSV2,FILE=TRIM(BASENAME)//'.2.csv',ERR=7)
          WRITE(*,*) 'CSV2 output file: ',TRIM(BASENAME)//'.2.csv'
        ELSE IF(NAME(ISTART:ISTOP).EQ.'ZBLST') THEN
          IOUT=13
          IUZBLST=IOUT
          OPEN(UNIT=IOUT,FILE=TRIM(BASENAME)//'.zblst',ERR=7)
          WRITE(*,*) 'Standard Zonebudget output file: ',
     1           TRIM(BASENAME)//'.zblst'
        END IF
        CALL PARSE_WORD_UP(NAME, LLOC, ISTART, ISTOP)
        IF(ISTART <= ISTOP .and. NAME(ISTART:ISTOP).NE.' ' .and. 
     +                           NAME(ISTART:ISTART).NE.'#') GO TO 8
      ELSE
        IOUT=13
        IUZBLST=IOUT
        OPEN(UNIT=IOUT,FILE=BASENAME,ERR=7)
      END IF
      !
      IF(IOUT.EQ.0) THEN
        IOUT=13
        OPEN(UNIT=IOUT,FILE=TRIM(BASENAME)//'.log',ERR=7)
        WRITE(*,*) 'Zonebudget log file: ',
     1         TRIM(BASENAME)//'.log'
      END IF
      !
      GO TO 9                                         ! Lame work-a-around to having ERR= go to the stop statement
7     CALL STOP_ERROR(NAME, 0, 0, 'Failed to open '//
     +       'one of the ZoneBudget output files.')
9     CONTINUE
C
C-----WRITE OUTPUT FILE
      !WRITE(IOUT,4) VERSON
      WRITE(IOUT,4) VERSON
      WRITE(IOUT,'(/,A)')' With enhancements for:'
      WRITE(IOUT,'(2(A,/), /, 2(A,/))')  
     +'                       MODFLOW',
     +'           ONE-WATER HYDROLOGIC-FLOW MODEL',
     +'   The U.S. Geological Survey Modular Finite-Difference',
     +'          Conjunctive Use Simulation Program'
      WRITE(IOUT,'(/,*(A,/))') 
     +'This ZoneBudget code and compiled executable are located at:','',
     +'         https://code.usgs.gov/modflow/mf-owhm','',
     +'Please read the README.md or README.pdf for download info.'
C
C-----OPEN LISTING FILE(S)
      WRITE(*,*)
      WRITE(*,*)' Enter a LISTING FILE for results',
     1                     ' or a base name and file types:'
C
C-----OPEN THE CELL-BY-CELL BUDGET FILE
      WRITE(*,*)
      WRITE(*,*) ' Enter the name of the file containing '//
     +           'CELL-BY-CELL BUDGET TERMS:'
      READ(*,'(A)') NAME
      CALL GET_UNCOMMENT(NAME, ISTART, ISTOP)
      !
      OPEN(UNIT=INBUD,FILE=NAME(ISTART:ISTOP),STATUS='OLD',FORM=FORM,
     1               ACCESS=ACCESS,ERR=10)
      !
      GO TO 11                                         ! Lame work-a-around to having ERR= go to the stop statement
10    CALL STOP_ERROR(NAME, INBUD, IOUT, 'Failed to open the '//
     +       'budget file/cell-by-cell file.'//LF//
     +       'The file may not exist.')
11    CONTINUE
      !
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' The cell-by-cell budget file is:'
      WRITE(IOUT,*) NAME(ISTART:ISTOP)
C
C-----Check for valid budget file, and allocate memory
      CALL BUDGETPRECISION(INBUD,NCOL,NROW,NLAY)
      IF(IPREC.LT.1) THEN
        CALL STOP_ERROR(NAME, INBUD, IOUT, 'Failed to identify '//
     +       'budget file/cell-by-cell file.'//LF//
     +       'It may be corrupt or has an invalid structure.')
      ELSEIF(IPREC.EQ.1) THEN
        WRITE(IOUT,*) ' Single precision budget file'
      ELSE IF(IPREC.EQ.2) THEN
        WRITE(IOUT,*) ' Double precision budget file'
      END IF
      WRITE(*,*)
      WRITE(*,14) NLAY,NROW,NCOL
      WRITE(IOUT,14) NLAY,NROW,NCOL
14    FORMAT(1X,I10,' layers',I10,' rows',I10,' columns')
      ALLOCATE (IZONE(NCOL,NROW,NLAY), SOURCE=-1)
      ALLOCATE (ICH(NCOL,NROW,NLAY),   SOURCE=0)
      ALLOCATE (IBUFF(NCOL,NROW,NLAY), SOURCE=0)
      MSUMCH = 0
C
C-----READ A TITLE TO BE PRINTED IN THE LISTING
      WRITE(*,*)
      WRITE(*,*) ' Enter a TITLE to be printed in the listing:'
      READ(*,'(A)') TITLE
      CALL GET_UNCOMMENT(TITLE, ISTART, ISTOP)
      !
      IF(ISTART <= ISTOP) THEN
                          WRITE(IOUT,'(/,1X,A)') TITLE(ISTART:ISTOP)
                          TITLE = TITLE(ISTART:ISTOP)
      ELSE
                          TITLE = ''
      END IF
C
C-----OPEN THE ZONE FILE IF IT EXISTS
      WRITE(*,*)
      WRITE(*,'(A,/,A)') 
     +  ' Enter the name of your ZONE INPUT FILE ',
     +  ' (Leave blank and press ENTER for interactive input):'
      READ(*,'(A)') NAME
      CALL GET_UNCOMMENT(NAME, ISTART, ISTOP)
C
C-----IF NAME IS BLANK, INPUT ZONES INTERACTIVELY BY BLOCK
      IF(ISTOP<ISTART .OR. NAME(ISTART:ISTOP) == '') THEN
         CALL BLOCK_READ(IZONE,NLAY,NROW,NCOL,IOUT)
         NCOMP=0
      ELSE
C
C-----WHEN NAME IS NOT BLANK, OPEN ZONE FILE, AND CHECK GRID DIMENSIONS
         OPEN(UNIT=INZN1,FILE=NAME(ISTART:ISTOP),STATUS='OLD',ERR=16)
         GO TO 17                                         ! Lame work-a-around to having ERR= go to the stop statement
16       CALL STOP_ERROR(NAME(ISTART:ISTOP), 0, 0, 'Failed to open '//
     +          'one of the ZoneBudget zone input file.')
17       CONTINUE
         !
         WRITE(IOUT,*)
         WRITE(IOUT,*) ' The zone file is:'
         WRITE(IOUT,*) NAME(ISTART:ISTOP)
         !
         CALL READ_TO_DATA(LINE,INZN1,IOUT)
         LLOC = 1
         CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INZN1,NL,
     +                                      'Error reading NLAY')
         CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INZN1,NR,
     +                                      'Error reading NROW')
         CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INZN1,NC,
     +                                      'Error reading NCOL')
         IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) THEN
                CALL STOP_ERROR(LINE, INZN1, IOUT, 
     +           'MISMATCH BETWEEN DIMENSIONS OF CELL-BY-CELL DATA '//
     +           'AND ZONE INPUT FILE DATA:'//LF//
     +           'LAY, ROW, COL IN ZONE INPUT   FILE: '//
     +         INT2STR(NL)//', '//INT2STR(NR)//', '//INT2STR(NC)//LF//
     +           'LAY, ROW, COL IN CELL-BY-CELL FILE: '//
     +         INT2STR(NLAY)//', '//INT2STR(NROW)//', '//INT2STR(NCOL) )
         END IF
C
C-----READ ZONE ARRAY
         CALL IZREAD(IZONE,NLAY,NROW,NCOL,INZN1,INZN2,IOUT)
      END IF
C
C-----DONE WITH ZONE DEFINITION.  Create the zone list, LSTZON.
      CALL ZONCOUNT(NZDIM,LSTZON,MXZONE,IZONE,NLAY,NROW,NCOL,IOUT)
      ALLOCATE (VBVL(2,NTRDIM,NZDIM))
      ALLOCATE (VBZNFL(2,0:NZDIM,0:NZDIM))
C
C-----READ COMPOSITE ZONES
      IF(NAME.NE.' ') THEN
         CALL INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,LSTZON,
     1               NZDIM,IOUT,NAMCOMP)
         CLOSE(UNIT=INZN1)
      END IF
C
C-----CHECK WHAT METHOD TO USE FOR SPECIFYING WHEN TO CALCULATE BUDGETS
      WRITE(*,*)
      WRITE(*,*) ' Choose the option for specifying '//
     1                 'when budgets are calculated:'
      WRITE(*,*) ' A = ALL times stored in the budget file.'
      WRITE(*,*) ' P = For each time stored in the budget file,'//
     +                                            'PROMPT user.'
      WRITE(*,*) ' L = Enter a LIST of times.'
      READ(*,'(A)') LINE
      LINE = ADJUSTL(LINE)
      METHOD = LINE(1:1)
      !
      IF(METHOD.EQ.'A' .OR. METHOD.EQ.'a') THEN
         METHOD='A'
      ELSE IF(METHOD.EQ.'P' .OR. METHOD.EQ.'p') THEN
         METHOD='P'
      ELSE IF(METHOD.EQ.'L' .OR. METHOD.EQ.'l') THEN
         METHOD='L'
         DO 60 I=1,10
         WRITE(*,*) ' Enter a time step, stress period at which '//
     +              'to calculate budgets (0,0=done):'
         READ(*,*) ITIME(1,I),ITIME(2,I)
         IF(ITIME(1,I).EQ.0 .AND. ITIME(2,I).EQ.0) GO TO 65
60       CONTINUE
         I=11
65       NTIMES=I-1
      ELSE
         CALL STOP_ERROR(LINE, 0, IOUT, 
     +        'Invalid choice; you must enter "A", "P", or "L"')
      END IF
      !
      WRITE(*,*)
      ICALC=0
C
C
C-----READ BUDGET DATA AND ACCUMULATE AS LONG AS TIME REMAINS CONSTANT.
C-----WHEN TIME CHANGES, PRINT THE BUDGET, REINITIALIZE, AND START OVER
100   READ(INBUD,END=1000,ERR=1000) KSTP,KPER,TEXT,NC,NR,NL
      ITYPE=0
      IF(NL.LT.0) THEN
         TOTIMDOLD=TOTIMD
         IF(IPREC.EQ.1) THEN
           READ(INBUD,END=5000,ERR=5000) ITYPE,DELT,PERTIM,TOTIM
           DELTD=DELT
           PERTIMD=PERTIM
           TOTIMD=TOTIM
         ELSE
           READ(INBUD,END=5000,ERR=5000) ITYPE,DELTD,PERTIMD,TOTIMD
         END IF
         NVAL=1
         IF(ITYPE.EQ.5) THEN
            READ(INBUD,END=6000,ERR=6000) NVAL
            IF(NVAL.GT.1) THEN
               DO 101 N=2,NVAL
               READ(INBUD,END=6000,ERR=6000) CTMP
101            CONTINUE
            END IF
         END IF
         IF(ITYPE.EQ. 2 .OR. ITYPE.EQ.5) READ(INBUD,END=6000,ERR=6000) 
     +                                                             NLIST
      END IF
C
C-----CHECK IF STARTING A NEW TIME STEP
      IF(K1.NE.KSTP .OR. K2.NE.KPER) THEN
C
C-----IF STARTING A NEW TIME STEP, PRINT A BUDGET AND REINITIALIZE ALL
C-----BUDGET ACCUMULATORS
C-----AT THE VERY BEGINNING WHEN K1=K2=0, DON'T PRINT THE BUDGET BECAUSE
C-----NOTHING HAS BEEN ACCUMULATED YET
         IF(K1.NE.0 .AND. K2.NE.0 .AND. ICALC.NE.0) THEN
C
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
            DO 150 K=0,NZDIM-1
            DO 150 J=K+1,NZDIM
            DO 150 I=1,2
            VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
150         CONTINUE
            IF(IUZBLST.GT.0) THEN
              CALL SUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,IOUT,NTRDIM,
     1            LSTZON,NZDIM,TITLE)
              IF(NCOMP.GT.0) CALL COMPPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1            IOUT,NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,NCOMP,
     2            MXCOMP,MXZWCZ,LSTZON,NAMCOMP)
            END IF
            IF(IUCSV.GT.0) CALL CSVSUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1            IUCSV,NTRDIM,NZDIM,TITLE,TOTIMDOLD,LSTZON)
            IF(IUCSV2.GT.0) CALL CSVSUBPR2(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     2            IUCSV2,NTRDIM,NZDIM,TITLE,TOTIMDOLD,LSTZON)
         END IF
C
C-----SET TIME CHANGE INDICATORS
         K1=KSTP
         K2=KPER
C
C-----DECIDE WHETHER OR NOT TO CALCULATE THE BUDGET FOR THIS TIME STEP
         ICALC=0
         IF(METHOD.EQ.'A') THEN
            ICALC=1
         ELSE IF(METHOD.EQ.'P') THEN
102         WRITE(*,105) KSTP,KPER
105         FORMAT(1X,'Do you want to calculate budgets for time step',
     1             I4,' in stress period',I4,' (Y/N)?')
            READ(*,'(A)') IANS
            IF(IANS.EQ.'Y' .OR. IANS.EQ.'y') THEN
               ICALC=1
            ELSE IF(IANS.EQ.'N' .OR. IANS.EQ.'n') THEN
            ELSE
               GO TO 102
            END IF
         ELSE
            DO 110 I=1,NTIMES
            IF(KSTP.NE.ITIME(1,I) .OR. KPER.NE.ITIME(2,I)) GO TO 110
            ICALC=1
            GO TO 120
110         CONTINUE
120         CONTINUE
         END IF
         IF(ICALC.EQ.0) THEN
            WRITE(*,121) KSTP,KPER
121         FORMAT(' Skipping the budget for time step',I4,
     1       ' in stress period',I4)
         ELSE
            MSUM=1
!            DO 210 I=1,NZDIM
!            DO 210 J=1,NTRDIM
!            DO 210 K=1,2
!            VBVL(K,J,I)=DZERO
!210         CONTINUE
!            DO 220 I=0,NZDIM
!            DO 220 J=0,NZDIM
!            DO 220 K=1,2
!            VBZNFL(K,J,I)=DZERO
!220         CONTINUE
            VBVL  =DZERO
            VBZNFL=DZERO
            WRITE(*,221) KSTP,KPER
221         FORMAT(' Computing the budget for time step',I4,
     1       ' in stress period',I4)
         END IF
      END IF
C
C-----READ THE BUDGET TERM DATA UNDER THE FOLLOWING CONDITIONS:
      IF(ITYPE.EQ.0 .OR. ITYPE.EQ.1) THEN
C  FULL 3-D ARRAY
         IF(IPREC.EQ.1) THEN
           READ(INBUD,END=7000,ERR=7000) BUFF
           BUFFD=BUFF
         ELSE
           READ(INBUD,END=7001,ERR=7001) BUFFD
         END IF
      ELSE IF(ITYPE.EQ.3) THEN
C  1-LAYER ARRAY WITH LAYER INDICATOR ARRAY
         BUFFD=DZERO
         READ(INBUD,END=8002,ERR=8002) IBUFF(:,:,1) !((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IF(IPREC.EQ.1) THEN
           READ(INBUD,END=8000,ERR=8000) BUFF(:,:,1) !((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
           DO 265 I=1,NROW
           DO 265 J=1,NCOL
           BUFFD(J,I,1)=BUFF(J,I,1)
265        CONTINUE
         ELSE
           READ(INBUD,END=8001,ERR=8001) BUFFD(:,:,1)  !((BUFFD(J,I,1),J=1,NCOL),I=1,NROW)
         END IF
         DO 270 I=1,NROW
         DO 270 J=1,NCOL
         IF(IBUFF(J,I,1).NE.1) THEN
            BUFFD(J,I,IBUFF(J,I,1))=BUFFD(J,I,1)
            BUFFD(J,I,1)=DZERO
         END IF
270      CONTINUE
      ELSE IF(ITYPE.EQ.4) THEN
C  1-LAYER ARRAY THAT DEFINES LAYER 1
         IF(IPREC.EQ.1) THEN
           READ(INBUD,END=8000,ERR=8000) BUFF(:,:,1) !((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
           DO 275 I=1,NROW
           DO 275 J=1,NCOL
           BUFFD(J,I,1)=BUFF(J,I,1)
275        CONTINUE
         ELSE
           READ(INBUD,END=8001,ERR=8001) BUFFD(:,:,1) !((BUFFD(J,I,1),J=1,NCOL),I=1,NROW)
         END IF
         IF(NLAY.GT.1) THEN
            DO 280 K=2,NLAY
            DO 280 I=1,NROW
            DO 280 J=1,NCOL
            BUFFD(J,I,K)=DZERO
280         CONTINUE
         END IF
      ELSE IF(ICALC.EQ.0 .AND. NLIST.GT.0) THEN
C  LIST -- READ ONLY IF THE VALUES NEED TO BE SKIPPED.
C  ACCM will read the list if the budget is being computed this time step.
         DO 300 N=1,NLIST
         IF(IPREC.EQ.1) THEN
           READ(INBUD,END=9000,ERR=9000) LOC,(VAL(I),I=1,NVAL)
         ELSE
           READ(INBUD,END=9000,ERR=9000) LOC,(VALD(I),I=1,NVAL)
         END IF
300      CONTINUE
      END IF
C
C-----BEFORE PROCESSING A BUDGET TERM, CHECK IF THERE IS ENOUGH SPACE
      IF(MSUM.GT.NTRDIM) THEN
         CALL STOP_ERROR('', INBUD, IOUT, 
     +   'Program array dimension parameter NTRDIM is too small.'//LF//
     +   'The current maximum size is '//INT2STR(NTRDIM)//LF//
     +   'ZoneBudget will need to be recompiled with a larger value.')
      END IF
C
C-----PROCESS A BUDGET TERM AND THEN START THE READ PROCESS OVER
      IF(ICALC.NE.0) CALL ACCM(IZONE,ICH,NCOL,NROW,NLAY,VBNM,VBVL,
     1           VBZNFL,MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2           ITYPE,NLIST,INBUD,NVAL, istat)
      if(istat /= 0) go to 9000
      GO TO 100
C
C  END OF FILE. PRINT FINAL BUDGET IF FLAG IS SET.
1000  IF(ICALC.NE.0) THEN
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
        DO 1050 K=0,NZDIM-1
        DO 1050 J=K+1,NZDIM
        DO 1050 I=1,2
        VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
1050    CONTINUE
        IF(IUZBLST.GT.0) THEN
          CALL SUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1        IOUT,NTRDIM,LSTZON,NZDIM,TITLE)
          IF(NCOMP.GT.0) CALL COMPPR(K1,K2,VBNM,VBVL,
     1      VBZNFL,MSUM,IOUT,NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,
     2      NCOMP,MXCOMP,MXZWCZ,LSTZON,NAMCOMP)
        END IF
        IF(IUCSV.GT.0) CALL CSVSUBPR(K1,K2,VBNM,VBVL,
     1     VBZNFL,MSUM,IUCSV,NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON)
        IF(IUCSV2.GT.0) CALL CSVSUBPR2(K1,K2,VBNM,VBVL,
     1     VBZNFL,MSUM,IUCSV2,NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON)
      END IF
C
      WRITE(*,*)
      GO TO 9009  ! Move to END PROGRAM
C
      
5000  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     +'Failed to read "ITYPE,DELT,PERTIM,TOTIM" record. '//
     +ERRAFT
      GO TO 9999
6000  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     +'Failed to read "NVAL" or "CTMP" or "NLIST"'//
     + ERRAFT
      GO TO 9999
7000  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     + 'Failed to read "BUFF(nrow,ncol,nlay)" which is a '//
     + 'single precision 3D array'//
     + ERRAFT
      GO TO 9999
7001  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     + 'Failed to read "BUFFD(nrow,ncol,nlay)" which is a '//
     + 'double precision 3D array'//
     + ERRAFT
      GO TO 9999
8000  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     + 'Failed to read "BUFF(nrow,ncol)" which is a '//
     + 'single precision 2D array'//
     + ERRAFT
      GO TO 9999
8001  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     + 'Failed to read "BUFFD(nrow,ncol)" which is a '//
     + 'double precision 2D array'//
     + ERRAFT
      GO TO 9999
8002  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     + 'Failed to read "IBUFF(nrow,ncol)" which is an '//
     + 'integer 2D array'//
     + ERRAFT
      GO TO 9999
9000  ERRMSG=ERRHED//
     +'For TEXT="'//TRIM(TEXT)//'" '//LF//
     +'Failed to read list style record: "ICELL,(VAL(I),I=1,NVAL)"'//
     + ERRAFT
      !
9999  WRITE(*,   '(A)') ERRMSG
      WRITE(IOUT,'(A)') ERRMSG
C
9009  CONTINUE      
      END PROGRAM
C
      SUBROUTINE IZREAD(IZONE,NLAY,NROW,NCOL,INZN1,INZN2,IOUT)
      IMPLICIT NONE
C     ******************************************************************
C     ROUTINE TO INPUT 3-D ZONE MATRIX, IZONE
C       INZN1 IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C        SPECIFICATIONS:
      INTEGER, INTENT(IN):: NLAY, NROW, NCOL, INZN1, INZN2, IOUT
      INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(INOUT):: IZONE
      !
      !
      INTEGER:: I,J,K, IU, IERR, ICONST, IPRN
      INTEGER:: ICOL, ISTART, ISTOP, OPEN_FLAG
      CHARACTER(  1):: NL
      CHARACTER(  2):: BLN
      CHARACTER(  6):: FREE_FORM
      CHARACTER( 16):: KEY
      CHARACTER( 20):: FMTIN
      CHARACTER(768):: CNTRL, FNAME
      INTEGER:: Z, ONE
      LOGICAL:: TRUE, FALSE, CHECK
      INTEGER, DIMENSION(:), ALLOCATABLE:: EXT
      !
      INTERFACE
              PURE FUNCTION INT2STR(IVAL)
                INTEGER,       INTENT(IN):: IVAL
                CHARACTER(:), ALLOCATABLE:: INT2STR
              END FUNCTION
              !
              PURE SUBROUTINE Append_1D_Alloc(IA, VAL)
                IMPLICIT NONE
                INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: IA
                INTEGER,                            INTENT(IN   ):: VAL
              END SUBROUTINE
      END INTERFACE
      !
      NL  = NEW_LINE(' ')
      BLN = NL//NL
      FREE_FORM = '(FREE)'
      Z   = 0
      ONE = 1
      TRUE  = .TRUE.
      FALSE = .FALSE.
      OPEN_FLAG = Z    ! 0:INTERNAL, 1:EXTERNAL, 2:OPEN/CLOSE
      !                  0:EXTERNAL already open
      LAY_LOOP: DO K=1,NLAY ! ------------------------------------------------------------------
      !
      !-READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      CALL READ_TO_DATA(CNTRL,INZN1,IOUT)
      CALL GET_UNCOMMENT(CNTRL, ISTART, ISTOP)
      CNTRL = CNTRL(ISTART:ISTOP)
      !
      !Get Array Control Directive
      ICOL = ONE
      CALL PARSE_WORD_UP(CNTRL,ICOL,ISTART,ISTOP)
      !
      IF(ISTOP < ISTART) THEN                   !Empty line read - Stop program
              !
              FNAME = 'ERROR READING ARRAY CONTROL RECORD FOR '//
     +                '"ZONEBUDGET IZONE" FOR LAYER '//INT2STR(K)//BLN//
     +                'PERHAPS THE END OF FILE WAS REACHED?'
              CALL STOP_ERROR(CNTRL, INZN1, IOUT, FNAME)
      END IF
      !
      KEY = CNTRL(ISTART:ISTOP)
      !
      IF(KEY == 'CONSTANT') THEN
         !
         CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,INZN1,ICONST,
     +   'ZoneBudget found array control record/keyword "CONSTANT",'//
     +   NL//'but failed to read the value, ICONST, after the keyword.')
         !
         CALL SET_ARRAY_0D2D_INT(NCOL, NROW, ICONST, IZONE(:,:,K))
         !
         OPEN_FLAG = -1
         !
         WRITE(IOUT,83) K, INT2STR(ICONST)
83       FORMAT(/,1X,'Zone Array for layer',I4,
     1      ' is set equal to: ',A,24x,'From "CONSTANT"')
         !
         CYCLE LAY_LOOP
         !
      ELSE IF(KEY == 'INTERNAL') THEN
         !
         IU = INZN1
         OPEN_FLAG = Z
         !
         WRITE(IOUT,92) K
92       FORMAT(/,1X,'Zone Array for layer',I4,
     1      ' will be read from the Zone File',24x,'From "INTERNAL"')
         !
      ELSE IF(KEY == 'EXTERNAL') THEN
         !
         CALL READ_TO_DATA(FNAME,INZN1,IOUT)
         !
         CALL GET_UNCOMMENT(FNAME, ISTART, ISTOP)
         FNAME = FNAME(ISTART:ISTOP)
         WRITE(IOUT,93) K,TRIM(ADJUSTL(FNAME)),'From "EXTERNAL"'
93       FORMAT(1X,'Zone Array for layer',I4,
     1         ' will be read from file: ', A, 24x,A)
         !
         INQUIRE(FILE=FNAME, NUMBER=IU, OPENED=CHECK) 
         !
         IF(CHECK) THEN
                   OPEN_FLAG = Z
         ELSE
                   OPEN_FLAG = ONE
                   IU = Z
         END IF
         !
      ELSE IF(KEY == 'OPEN/CLOSE') THEN
         !
         CALL READ_TO_DATA(FNAME,INZN1,IOUT)
         !
         IU = Z
         CALL GET_UNCOMMENT(FNAME, ISTART, ISTOP)
         FNAME = FNAME(ISTART:ISTOP)
         WRITE(IOUT,93) K,TRIM(ADJUSTL(FNAME)),'From "OPEN/CLOSE"'
         !
         OPEN_FLAG = 2
      ELSE
         CALL BACKSPACE_STOP_MSG(INZN1, INZN1, IOUT, Z, 
     +   'IZREAD - ERROR READING THE "ZONEBUDGET IZONE" ARRAY '//
     +   'CONTROL RECORD/KEYWORD.'//BLN//
     +   'ACCEPTED KEYWORDS ARE:'//NL//
     +   'CONSTANT'//NL//
     +   'INTERNAL'//NL//
     +   'EXTERNAL'//NL//
     +   'OPEN/CLOSE'//BLN//
     +   'BUT INSTEAD FOUND: "'//KEY//'"')
      END IF
      !
      IF(OPEN_FLAG > Z) THEN
         OPEN(NEWUNIT=IU, FILE=FNAME, STATUS='OLD', IOSTAT=IERR)
         IF(IERR == Z) THEN
                       REWIND(IU)
         ELSE
             CNTRL = NL//TRIM(ADJUSTL(CNTRL))//NL//
     +                   TRIM(ADJUSTL(FNAME))//NL
             CALL STOP_ERROR(CNTRL,INZN1,IOUT, 
     +'FAILED TO OPEN ZONE FILE SPECIFIED ON THE SECOND LINE.'//BLN//
     +'File may not exist or the specified path is incorrect.'//BLN//
     +'Note the expected input structure is over two lines.'//NL//
     +'The expected input structure is:'//NL//
     +TRIM(KEY)//'   FMTIN   IPRN'//NL//
     +'FileName'//BLN//
     +'where FMTIN is the Fortran Format, such as "('//
     +INT2STR(NCOL)//'I4)" to read with a field of 4 spaces or '//
     +'"(FREE)" to read with free formatting.'//NL//
     +'IPRN is set to 0 or >0 to print the array read '//
     +'in to the output log (LIST). If negative, then '//
     +'printing is supressed.'//NL//
     +'FileName is the name of the file that contains the zone array.'//
     +BLN//'    --Note1: The input FMTIN and IPRN are optional, '//NL//
     +     '             and when not present are set to '//
     +                  '"(FREE)" and -1, respectively'//
     +BLN//'    --Note2: Comments must be preceded by a #'//NL//
     +     '             such as: '//NL//
     +     '             OPEN/CLOSE   (FREE)  1  # Comments'//NL//
     +     '             ./zones.txt             # Comments')
         END IF
      END IF
      !
      CALL PARSE_WORD_UP(CNTRL,ICOL,ISTART,ISTOP)
      !
      FMTIN = FREE_FORM              ! Default Options
      IPRN  = -1
      !
      IF (ISTOP >=  ISTART) THEN  ! Valid word parsed
          !
          IF(CNTRL(ISTART:ISTART) == '(') THEN
              IF(INDEX(CNTRL(ISTART:ISTOP),')') == Z) THEN
                  WRITE(IOUT,'(/,A,I4,*(A))') 'Zone Array for layer',K,
     +            ' Found an "(" but failed to locate matching ")".',
     +            ' It will be ignored and instead use free format.',
     +            ' Note, that it is best to enclose FMTIN in quotes,',
     +            ' such as "(15I3)" or "( 25I6 )"'
              ELSEIF(ISTART+2 < ISTOP             .AND.        ! NOT->  () or ( )
     +           CNTRL(ISTART+1:ISTOP-1) /= '' .AND.        ! NOT->  (  )
     +           INDEX(CNTRL(ISTART:ISTOP),'F') == Z) THEN  ! NOT->  (FREE)
                       FMTIN = CNTRL(ISTART:ISTOP)
              END IF
          ELSE
                 ICOL  = ISTART                             ! Failed to find format so assume its IPRN
          END IF
          !
          CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,INZN1,IPRN,
     +                                                 'NOSTOP')
          IF(IPRN == HUGE(IPRN)) IPRN=-1
          !
      END IF
      !
      CALL MOVE_TO_DATA(CNTRL, IU, IOUT)     ! Load IZONE(NCOL,NROW) from file specifed in IU
      !
      IF(FMTIN == FREE_FORM) THEN
         WRITE(IOUT,'(A,I4,A)') ' Zone Array for layer',K,
     +                          ' will be read using free format.'
         DO I=1, NROW
             READ(IU,*, IOSTAT=IERR) IZONE(1:NCOL,I,K)
             IF(IERR /= 0 ) THEN
             CALL BACKSPACE_STOP_MSG(IU, INZN1, IOUT, IERR, 
     +   'IZREAD - ERROR READING THE "ZONEBUDGET IZONE" ARRAY.'//NL//
     +   'THE ERROR LINE IS THE BEST GUESS WHERE THE ERROR OCCURED')
             ELSE IF( ANY( IZONE(1:NCOL,I,K)<0 ) ) THEN
             CALL BACKSPACE_STOP_MSG(IU, INZN1, IOUT, Z,  
     +     'NEGATIVE ZONE NUMBER NOT ALLOWED'//NL//
     +     'Negative zone number found in layer '//INT2STR(K) )
             END IF
         END DO
      ELSE
         WRITE(IOUT,'(A,I4,2A)') ' Zone Array for layer',K,
     +                           ' will be read using format: ', FMTIN
         DO I=1, NROW
             READ(IU,FMTIN, IOSTAT=IERR) IZONE(1:NCOL,I,K)
             IF(IERR /= 0 ) THEN
             CALL BACKSPACE_STOP_MSG(IU, INZN1, IOUT, IERR, 
     +   'IZREAD - ERROR READING THE "ZONEBUDGET IZONE" ARRAY.'//NL//
     +   'THE ERROR LINE IS THE BEST GUESS WHERE THE ERROR OCCURED')
             ELSE IF( ANY( IZONE(1:NCOL,I,K)<0 ) ) THEN
             CALL BACKSPACE_STOP_MSG(IU, INZN1, IOUT, Z,  
     +     'NEGATIVE ZONE NUMBER NOT ALLOWED'//NL//
     +     'Negative zone number found in layer '//INT2STR(K) )
             END IF
         END DO
      END IF
      !
      IF    (OPEN_FLAG == 1) THEN
                          CALL Append_1D_Alloc(EXT, IU)
      ELSEIF(OPEN_FLAG == 2) THEN
                          CLOSE(IU, IOSTAT=IERR)
      END IF
      !
      !-IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.
      IF(IPRN >= 0) THEN
             !-PRINT COLUMN NUMBERS AT THE TOP OF THE PAGE
             WRITE(IOUT,*)
             WRITE(IOUT,422)
             WRITE(IOUT,421) (I,I=1,NCOL)
421          FORMAT((5X,*(I4)))
             WRITE(IOUT,422)
422          FORMAT(1X,79('-'))
             !
             !-PRINT EACH ROW IN THE ARRAY.
             DO I=1, NROW
                WRITE(IOUT,423) I, IZONE(:,I,K)
423             FORMAT(1X,I3,1X,*(I4))
             END DO
             WRITE(IOUT,*)
             WRITE(IOUT,422)
             WRITE(IOUT,*)
      END IF
      ! 
      END DO LAY_LOOP       ! ------------------------------------------------------------------
      !
      IF(ALLOCATED(EXT)) THEN
          DO I=1, SIZE(EXT)
                  CLOSE(EXT(I), IOSTAT=IERR)
          END DO
      END IF
C
C-----RETURN
      RETURN
      END SUBROUTINE
C
      SUBROUTINE BLOCK_READ(IZONE,NLAY,NROW,NCOL,IOUT)
C     ******************************************************************
C     INPUT ZONE VALUES BY BLOCK
C     ******************************************************************
      DIMENSION IZONE(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
C-----INITIALIZE THE IZONE ARRAY
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      IZONE(J,I,K)=0
5     CONTINUE
      WRITE(IOUT,*)
C
10    WRITE(*,*)
      WRITE(*,*) ' Enter the start layer, stop layer (0,0 means done):'
      READ(*,*) K1,K2
      IF(K1.EQ.0 .AND. K2.EQ.0) RETURN
      IF(K1.LT.1 .OR. K2.GT.NLAY) THEN
         WRITE(*,*) ' NON-EXISTENT LAYER -- TRY AGAIN'
         GO TO 10
      END IF
20    WRITE(*,*) ' Enter the start row, stop row:'
      READ(*,*) I1,I2
      IF(I1.LT.1 .OR. I2.GT.NROW) THEN
         WRITE(*,*) ' NON-EXISTENT ROW -- TRY AGAIN'
         GO TO 20
      END IF
30    WRITE(*,*) ' Enter the start column, stop column:'
      READ(*,*) J1,J2
      IF(J1.LT.1 .OR. J2.GT.NCOL) THEN
         WRITE(*,*) ' NON-EXISTENT COLUMN -- TRY AGAIN'
      END IF
C
40    WRITE(*,*) ' Enter the zone for this block:'
      READ(*,*) ICONST
      IF(ICONST.LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONES ARE NOT ALLOWED'
         GO TO 40
      END IF
C
      WRITE(IOUT,3) K1,K2,I1,I2,J1,J2,ICONST
3     FORMAT(1X,'Zone block: LAYERS ',I3,'-',I3,
     1                  '    ROWS ',I4,'-',I4,
     2               '    COLUMNS ',I4,'-',I4,
     3                 '    VALUE:',I4)
C
      DO 50 K=K1,K2
      DO 50 I=I1,I2
      DO 50 J=J1,J2
      IZONE(J,I,K)=ICONST
50    CONTINUE
      GO TO 10
C
      END SUBROUTINE
C
      SUBROUTINE ZONCOUNT(NZDIM,LSTZON,MXZONE,IZONE,NLAY,NROW,NCOL,IOUT)
C     ******************************************************************
C     Create Zone list
C     ******************************************************************
      DIMENSION LSTZON(0:MXZONE),IZONE(NCOL,NROW,NLAY)
      !
      INTERFACE
              PURE FUNCTION INT2STR(IVAL)
                INTEGER,       INTENT(IN):: IVAL
                CHARACTER(:), ALLOCATABLE:: INT2STR
              END FUNCTION
      END INTERFACE
C     ------------------------------------------------------------------
      LSTZON(0)=-1
      LSTZON(1:)=0
      NZDIM=0
      DO 100 K=1,NLAY
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
      IZ=IZONE(J,I,K)
      IF(IZ.EQ.0) THEN
        LSTZON(0)=0
      ELSE
        IF(NZDIM.EQ.0) THEN
          NZDIM=NZDIM+1
          LSTZON(NZDIM)=IZ
        ELSE
          DO 70 L=1,NZDIM
          IF(IZ.EQ.LSTZON(L)) THEN
             GO TO 100
          ELSE IF(IZ.LT.LSTZON(L)) THEN
C  Found a new zone
             DO 60 M=NZDIM,L,-1
             LSTZON(M+1)=LSTZON(M)
60           CONTINUE
             LSTZON(L)=IZ
             NZDIM=NZDIM+1
             GO TO 100
          END IF
70        CONTINUE
          NZDIM=NZDIM+1
          LSTZON(NZDIM)=IZ
        END IF
      END IF
100   CONTINUE
C
      WRITE(*,   '(/,1x,2A)') INT2STR(NZDIM),' zone numbers defined:'
      WRITE(IOUT,'(/,1x,2A)') INT2STR(NZDIM),' zone numbers defined:'
      IF(NZDIM.EQ.0) THEN
         CALL STOP_ERROR('', 0, IOUT, 'Error - No zones defined')
      END IF
      DO M=1, NZDIM
         WRITE(*,   '(1x,2A)', ADVANCE='NO') INT2STR(LSTZON(M))
         WRITE(IOUT,'(1x,2A)', ADVANCE='NO') INT2STR(LSTZON(M))
         IF(MOD(M, 25) == 0) THEN
            WRITE(*,*)
            WRITE(IOUT,*) 
         END IF
      END DO
      WRITE(*,*)
      WRITE(IOUT,*) 
C
C  Change IZONE to the zone index number
      DO 300 K=1,NLAY
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
        DO 250 M=0,NZDIM
        IF(IZONE(J,I,K).EQ.LSTZON(M)) THEN
          IZONE(J,I,K)=M
          GO TO 300
        END IF
250     CONTINUE
300   CONTINUE
C
      RETURN
      END SUBROUTINE
C
      SUBROUTINE INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,
     1                  LSTZON,NZDIM,IOUT,NAMCOMP)
C     ******************************************************************
C     READ COMPOSITE ZONES
C     ******************************************************************
C       SPECIFICATIONS:
      INTEGER, INTENT(IN   ):: MXCOMP,MXZWCZ,INZN1,NZDIM,IOUT
      INTEGER, INTENT(INOUT):: NCOMP
      INTEGER, DIMENSION(MXZWCZ,MXCOMP), INTENT(INOUT):: ICOMP
      INTEGER, DIMENSION(MXCOMP),        INTENT(INOUT):: NZWCZ
      INTEGER, DIMENSION(0:NZDIM),       INTENT(IN   ):: LSTZON
      CHARACTER(10), DIMENSION(MXCOMP),  INTENT(INOUT):: NAMCOMP
      !
      CHARACTER(1024):: LINE
      CHARACTER(1):: NL
      INTEGER:: IC, IV, ERRNUM
C     ------------------------------------------------------------------
      NL = NEW_LINE(' ')
      ERRNUM = HUGE(ERRNUM)
C
C-----READ THE COMPOSITE ZONES
      IC = 1
      DO 10 I=1,MXCOMP
      !
      CALL READ_TO_DATA(LINE,INZN1,IOUT)
      IF(LINE == "") GO TO 20
      !
      LLOC=1
      CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
      !
      IF(LINE(ISTART:ISTART).GE.'0' .AND.
     1              LINE(ISTART:ISTART).LE.'9') THEN
        NAMCOMP(I)=' '
        WRITE(NAMCOMP(I),2) I
2       FORMAT('CZ',I3.3)
        LLOC=1
      ELSE
        NAMCOMP(I)=LINE(ISTART:ISTOP)
      END IF
      !
      DO J=1, MXZWCZ
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INZN1,IV,'NOSTOP')
        IF ( IV <= 0 .or. IV == ERRNUM) EXIT
        ICOMP(J,I) = IV
      END DO
      IC = IC + 1
10    CONTINUE
      IC=MXCOMP+1
20    NCOMP=IC-1
      IF(NCOMP.EQ.0) RETURN
C
C-----FIND HOW MANY ZONES MAKE UP EACH COMPOSITE ZONE
      DO 40 I=1,NCOMP
        DO 30 J=1,MXZWCZ
          IF(ICOMP(J,I).LE.0) GO TO 35
          DO 25 M=1,NZDIM
            IF(ICOMP(J,I).EQ.LSTZON(M)) THEN
              ICOMP(J,I)=M
              GO TO 30
            END IF
25        CONTINUE
          WRITE(IOUT,26) NAMCOMP(I),ICOMP(J,I)
26        FORMAT(1X,'Nonexistent zone specified for Composite Zone ',
     1                A,':',I5)
          GO TO 35
30      CONTINUE
        J=MXZWCZ+1
35      NZWCZ(I)=J-1
        IF(NZWCZ(I).EQ.0) THEN
           NCOMP=I-1
           IF(NCOMP.EQ.0) RETURN
           GO TO 50
        END IF
40    CONTINUE
C
C-----WRITE THE COMPOSITE ZONES
50    WRITE(IOUT,*)
      WRITE(IOUT,52) NCOMP
52    FORMAT(1X,I3,' Composite Zones Defined:')
      DO 60 I=1,NCOMP
      WRITE(IOUT,54) NAMCOMP(I),(LSTZON(ICOMP(J,I)),J=1,NZWCZ(I))
54    FORMAT(1X,'"',A,'" = ',15I4/(27X,15I4))
60    CONTINUE
C
      RETURN
      END SUBROUTINE
C
      SUBROUTINE ACCM(IZONE,ICH,NCOL,NROW,NLAY,VBNM,VBVL,VBZNFL,
     1                MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2                ITYPE,NLIST,INBUD,NVAL, istat)
C     ******************************************************************
C     ACCUMULATE VOLUMETRIC BUDGET FOR ZONES
C     ******************************************************************
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: real32, real64
      USE ZONBUDMODULE
      DIMENSION VBVL(2,NTRDIM,NZDIM),
     1  VBZNFL(2,0:NZDIM,0:NZDIM),IZONE(NCOL,NROW,NLAY),
     2  ICH(NCOL,NROW,NLAY)
      DOUBLE PRECISION VBVL,VBZNFL,DBUFF
      CHARACTER*16 VBNM(NTRDIM),TEXT
      DIMENSION VAL(20)
      DOUBLE PRECISION VALD(20),DZERO
C     ------------------------------------------------------------------
      DZERO=0.0_real64
      NRC=NROW*NCOL
C
C-----CHECK FOR INTERNAL FLOW TERMS, WHICH ARE USED TO CALCULATE FLOW
C-----BETWEEN ZONES, AND CONSTANT-HEAD TERMS
      IF(TEXT.EQ.'   CONSTANT HEAD') GO TO 200
      IF(TEXT.EQ.'FLOW RIGHT FACE ') GO TO 300
      IF(TEXT.EQ.'FLOW FRONT FACE ') GO TO 400
      IF(TEXT.EQ.'FLOW LOWER FACE ') GO TO 500
C
C-----NOT AN INTERNAL FLOW TERM, SO MUST BE A SOURCE TERM OR STORAGE
C-----ACCUMULATE THE FLOW BY ZONE
      IF(ITYPE.EQ.2 .OR. ITYPE.EQ.5) THEN
C  LIST
         IF(NLIST.GT.0) THEN
            DO 80 N=1,NLIST
            IF(IPREC.EQ.1) THEN
              READ(INBUD, iostat=istat) ICELL,(VAL(I),I=1,NVAL)
              if(istat /= 0) return
              DO 45 I=1,NVAL
              VALD(I)=VAL(I)
45            CONTINUE
            ELSE
              READ(INBUD, iostat=istat) ICELL,(VALD(I),I=1,NVAL)
              if(istat /= 0) return
            END IF
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL +1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            NZ=IZONE(J,I,K)
            IF(NZ.EQ.0) GO TO 80
            DBUFF=VALD(1)
            IF(DBUFF.EQ.DZERO) THEN
            ELSE IF(DBUFF.LT.DZERO) THEN
               VBVL(2,MSUM,NZ)=VBVL(2,MSUM,NZ)-DBUFF
            ELSE
               VBVL(1,MSUM,NZ)=VBVL(1,MSUM,NZ)+DBUFF
            END IF
80          CONTINUE
         END IF
      ELSE
C  ARRAY -- BUFFD already has the data
         DO 100 K=1,NLAY
         DO 100 I=1,NROW
         DO 100 J=1,NCOL
         NZ=IZONE(J,I,K)
         IF(NZ.EQ.0) GO TO 100
         DBUFF=BUFFD(J,I,K)
         IF(DBUFF.EQ.DZERO) THEN
         ELSE IF(DBUFF.LT.DZERO) THEN
            VBVL(2,MSUM,NZ)=VBVL(2,MSUM,NZ)-DBUFF
         ELSE
            VBVL(1,MSUM,NZ)=VBVL(1,MSUM,NZ)+DBUFF
         END IF
  100    CONTINUE
      END IF
C
C-----SAVE THE TERM NAME AND KEEP TRACK OF THE NUMBER OF TERMS
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
      RETURN
C
C-----CONSTANT-HEAD FLOW -- DON'T ACCUMULATE THE CELL-BY-CELL VALUES FOR
C-----CONSTANT-HEAD FLOW BECAUSE THEY MAY INCLUDE PARTIALLY CANCELING
C-----INS AND OUTS.  USE CONSTANT-HEAD TERM TO IDENTIFY WHERE CONSTANT-
C-----HEAD CELLS ARE AND THEN USE FACE FLOWS TO DETERMINE THE AMOUNT OF
C-----FLOW.  STORE CONSTANT-HEAD LOCATIONS IN ICH ARRAY.
200   IF(ITYPE.EQ.2 .OR. ITYPE.EQ.5) THEN
         DO 240 K=1,NLAY
         DO 240 I=1,NROW
         DO 240 J=1,NCOL
         ICH(J,I,K)=0
240      CONTINUE
         IF(NLIST.GT.0) THEN
            DO 250 N=1,NLIST
            IF(IPREC.EQ.1) THEN
              READ(INBUD, iostat=istat) ICELL,(VAL(I),I=1,NVAL)
              if(istat /= 0) return
            ELSE
              READ(INBUD, iostat=istat) ICELL,(VALD(I),I=1,NVAL)
              if(istat /= 0) return
            END IF
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL +1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            ICH(J,I,K)=1
250         CONTINUE
         END IF
      ELSE
         DO 260 K=1,NLAY
         DO 260 I=1,NROW
         DO 260 J=1,NCOL
         ICH(J,I,K)=0
         IF(BUFFD(J,I,K).NE.DZERO) ICH(J,I,K)=1
260      CONTINUE
      END IF
      VBNM(MSUM)=TEXT
      MSUMCH=MSUM
      MSUM=MSUM+1
      RETURN
C
C-----"FLOW RIGHT FACE"  COMPUTE FLOW BETWEEN ZONES ACROSS COLUMNS.
C-----COMPUTE FLOW ONLY BETWEEN A ZONE AND A HIGHER ZONE -- FLOW FROM
C-----ZONE 4 TO 3 IS THE NEGATIVE OF FLOW FROM 3 TO 4.
C-----1ST, CALCULATE FLOW BETWEEN NODE J,I,K AND J-1,I,K
300   IF(NCOL.LT.2) RETURN
      DO 340 K=1,NLAY
      DO 340 I=1,NROW
      DO 340 J=2,NCOL
      NZ=IZONE(J,I,K)
      JL=J-1
      NZL=IZONE(JL,I,K)
      IF(NZL.LE.NZ) GO TO 340
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J-1,I,K).EQ.1) GO TO 340
      DBUFF=BUFFD(JL,I,K)
      IF(DBUFF.LT.DZERO) THEN
         VBZNFL(2,NZ,NZL)=VBZNFL(2,NZ,NZL)-DBUFF
      ELSE
         VBZNFL(1,NZ,NZL)=VBZNFL(1,NZ,NZL)+DBUFF
      END IF
  340 CONTINUE
C
C-----FLOW BETWEEN NODE J,I,K AND J+1,I,K
      DO 370 K=1,NLAY
      DO 370 I=1,NROW
      DO 370 J=1,NCOL-1
      NZ=IZONE(J,I,K)
      JR=J+1
      NZR=IZONE(JR,I,K)
      IF(NZR.LE.NZ) GO TO 370
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J+1,I,K).EQ.1) GO TO 370
      DBUFF=BUFFD(J,I,K)
      IF(DBUFF.LT.DZERO) THEN
         VBZNFL(1,NZ,NZR)=VBZNFL(1,NZ,NZR)-DBUFF
      ELSE
         VBZNFL(2,NZ,NZR)=VBZNFL(2,NZ,NZR)+DBUFF
      END IF
  370 CONTINUE
C
C-----CALCULATE FLOW TO CONSTANT-HEAD CELLS IN THIS DIRECTION
      !
      IF(MSUMCH==0) RETURN  ! No need to compute constant head
      !
      DO 395 K=1,NLAY
      DO 395 I=1,NROW
      DO 395 J=1,NCOL
      IF(ICH(J,I,K).EQ.0) GO TO 395
      NZ=IZONE(J,I,K)
      IF(NZ.EQ.0) GO TO 395
      IF(J.EQ.NCOL) GO TO 380
      IF(ICH(J+1,I,K).EQ.1) GO TO 380
      DBUFF=BUFFD(J,I,K)
      IF(DBUFF.EQ.DZERO) THEN
      ELSE IF(DBUFF.LT.DZERO) THEN
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)+DBUFF
      END IF
380   IF(J.EQ.1) GO TO 395
      IF(ICH(J-1,I,K).EQ.1) GO TO 395
      DBUFF=BUFFD(J-1,I,K)
      IF(DBUFF.EQ.DZERO) THEN
      ELSE IF(DBUFF.LT.DZERO) THEN
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
      END IF
395   CONTINUE
      RETURN
C
C-----"FLOW FRONT FACE"
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I-1,K
400   IF(NROW.LT.2) RETURN
      DO 440 K=1,NLAY
      DO 440 I=2,NROW
      DO 440 J=1,NCOL
      NZ=IZONE(J,I,K)
      IA=I-1
      NZA=IZONE(J,IA,K)
      IF(NZA.LE.NZ) GO TO 440
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I-1,K).EQ.1) GO TO 440
      DBUFF=BUFFD(J,IA,K)
      IF(DBUFF.LT.DZERO) THEN
         VBZNFL(2,NZ,NZA)=VBZNFL(2,NZ,NZA)-DBUFF
      ELSE
         VBZNFL(1,NZ,NZA)=VBZNFL(1,NZ,NZA)+DBUFF
      END IF
  440 CONTINUE
C
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I+1,K
      DO 470 K=1,NLAY
      DO 470 I=1,NROW-1
      DO 470 J=1,NCOL
      NZ=IZONE(J,I,K)
      IB=I+1
      NZB=IZONE(J,IB,K)
      IF(NZB.LE.NZ) GO TO 470
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I+1,K).EQ.1) GO TO 470
      DBUFF=BUFFD(J,I,K)
      IF(DBUFF.LT.DZERO) THEN
         VBZNFL(1,NZ,NZB)=VBZNFL(1,NZ,NZB)-DBUFF
      ELSE
         VBZNFL(2,NZ,NZB)=VBZNFL(2,NZ,NZB)+DBUFF
      END IF
  470 CONTINUE
C
C-----CALCULATE FLOW TO CONSTANT-HEAD CELLS IN THIS DIRECTION
      !
      IF(MSUMCH==0) RETURN  ! No need to compute constant head
      !
      DO 495 K=1,NLAY
      DO 495 I=1,NROW
      DO 495 J=1,NCOL
      IF(ICH(J,I,K).EQ.0) GO TO 495
      NZ=IZONE(J,I,K)
      IF(NZ.EQ.0) GO TO 495
      IF(I.EQ.NROW) GO TO 480
      IF(ICH(J,I+1,K).EQ.1) GO TO 480
      DBUFF=BUFFD(J,I,K)
      IF(DBUFF.EQ.DZERO) THEN
      ELSE IF(DBUFF.LT.DZERO) THEN
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)+DBUFF
      END IF
480   IF(I.EQ.1) GO TO 495
      IF(ICH(J,I-1,K).EQ.1) GO TO 495
      DBUFF=BUFFD(J,I-1,K)
      IF(DBUFF.EQ.DZERO) THEN
      ELSE IF(DBUFF.LT.DZERO) THEN
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
      END IF
495   CONTINUE
      RETURN
C
C-----"FLOW LOWER FACE"
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I,K-1
500   IF(NLAY.LT.2) RETURN
      DO 540 K=2,NLAY
      DO 540 I=1,NROW
      DO 540 J=1,NCOL
      NZ=IZONE(J,I,K)
      KA=K-1
      NZA=IZONE(J,I,KA)
      IF(NZA.LE.NZ) GO TO 540
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I,K-1).EQ.1) GO TO 540
      DBUFF=BUFFD(J,I,KA)
      IF(DBUFF.LT.DZERO) THEN
         VBZNFL(2,NZ,NZA)=VBZNFL(2,NZ,NZA)-DBUFF
      ELSE
         VBZNFL(1,NZ,NZA)=VBZNFL(1,NZ,NZA)+DBUFF
      END IF
  540 CONTINUE
C
C-----CALCULATE FLOW BETWEEN NODE J,I,K AND J,I,K+1
      DO 570 K=1,NLAY-1
      DO 570 I=1,NROW
      DO 570 J=1,NCOL
      NZ=IZONE(J,I,K)
      KB=K+1
      NZB=IZONE(J,I,KB)
      IF(NZB.LE.NZ) GO TO 570
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
      IF(ICH(J,I,K).EQ.1 .AND. ICH(J,I,K+1).EQ.1) GO TO 570
      DBUFF=BUFFD(J,I,K)
      IF(DBUFF.LT.DZERO) THEN
         VBZNFL(1,NZ,NZB)=VBZNFL(1,NZ,NZB)-DBUFF
      ELSE
         VBZNFL(2,NZ,NZB)=VBZNFL(2,NZ,NZB)+DBUFF
      END IF
  570 CONTINUE
C
C-----CALCULATE FLOW TO CONSTANT-HEAD CELLS IN THIS DIRECTION
      !
      IF(MSUMCH==0) RETURN  ! No need to compute constant head
      !
      DO 595 K=1,NLAY
      DO 595 I=1,NROW
      DO 595 J=1,NCOL
      IF(ICH(J,I,K).EQ.0) GO TO 595
      NZ=IZONE(J,I,K)
      IF(NZ.EQ.0) GO TO 595
      IF(K.EQ.NLAY) GO TO 580
      IF(ICH(J,I,K+1).EQ.1) GO TO 580
      DBUFF=BUFFD(J,I,K)
      IF(DBUFF.EQ.DZERO) THEN
      ELSE IF(DBUFF.LT.DZERO) THEN
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)+DBUFF
      END IF
580   IF(K.EQ.1) GO TO 595
      IF(ICH(J,I,K-1).EQ.1) GO TO 595
      DBUFF=BUFFD(J,I,K-1)
      IF(DBUFF.EQ.DZERO) THEN
      ELSE IF(DBUFF.LT.DZERO) THEN
         VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
      ELSE
         VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
      END IF
595   CONTINUE
      RETURN
C
      END SUBROUTINE
C
      SUBROUTINE SUBPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IOUT,
     1               NTRDIM,LSTZON,NZDIM,TITLE)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND PRINT BUDGET
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM),
     1          LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,TOTOUT,TOTIN,TOTBD,DHUN,DTWO,ZERO
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
C     ------------------------------------------------------------------
      ZERO=0.0
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C-----FOR EACH ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 N=1,NZDIM
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=ZERO
      TOTIN=ZERO
      DO 100 I=1,MTOT
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
      DO 150 I=0,NZDIM
      TOTIN=TOTIN+VBZNFL(1,N,I)
      TOTOUT=TOTOUT+VBZNFL(2,N,I)
150   CONTINUE
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.ZERO .AND. TOTOUT.EQ.ZERO) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C     ---PRINT BUDGET---
C
C-----PRINT THE TITLE
      WRITE(IOUT,'(1H1,/1X,A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,601) LSTZON(N),KSTP,KPER
C
C-----PRINT THE IN TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,602)
      DO 200 I=1,MTOT
      WRITE(IOUT,603) VBNM(I),VBVL(1,I,N)
200   CONTINUE
      DO 250 I=0,NZDIM
      IF(VBZNFL(1,N,I).NE.ZERO .OR. VBZNFL(2,N,I).NE.ZERO)
     1              WRITE(IOUT,609) LSTZON(I),LSTZON(N),VBZNFL(1,N,I)
250   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,604) TOTIN
C
C-----PRINT THE OUT TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,605)
      DO 300 I=1,MTOT
      WRITE(IOUT,603) VBNM(I),VBVL(2,I,N)
300   CONTINUE
      DO 350 I=0,NZDIM
      IF(VBZNFL(1,N,I).NE.ZERO .OR. VBZNFL(2,N,I).NE.ZERO)
     1              WRITE(IOUT,609) LSTZON(N),LSTZON(I),VBZNFL(2,N,I)
350   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,606) TOTOUT
C
C-----PRINT IN-OUT AND THE PERCENT ERROR
      WRITE(IOUT,*)
      WRITE(IOUT,607) TOTBD
      WRITE(IOUT,*)
      WRITE(IOUT,608) PERCNT
C
  500 CONTINUE
C
      RETURN
C
C    ---FORMATS---
C
  601 FORMAT(5X,'Flow Budget for Zone',I3,
     1  ' at Time Step',I4,' of Stress Period',I4/5X,61('-'))
  602 FORMAT(23X,'Budget Term',5X,'Flow (L**3/T)'/
     1     23X,29('-')//13X,'IN:'/13X,'---')
  603 FORMAT(18X,A,' =',G14.5)
  604 FORMAT(26X,'Total IN =',G14.5)
  605 FORMAT(13X,'OUT:'/13X,4('-'))
  606 FORMAT(25X,'Total OUT =',G14.5)
  607 FORMAT(26X,'IN - OUT =',G14.5)
  608 FORMAT(15X,'Percent Discrepancy =',F20.2)
  609 FORMAT(19X,'Zone',I4,' to',I4,' =',G14.5)
      END SUBROUTINE
C
      SUBROUTINE COMPPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IOUT,NTRDIM,
     1   NZDIM,TITLE,ICOMP,NZWCZ,NCOMP,MXCOMP,MXZWCZ,LSTZON,NAMCOMP)
C     ******************************************************************
C     COMPUTE BUDGET TOTALS FOR COMPOSITE ZONES AND PRINT BUDGET
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM)
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP),LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL
      DOUBLE PRECISION TOTOUT,TOTIN,TOTBD,DHUN,DTWO,TSUM1,TSUM2
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      CHARACTER*10 NAMCOMP(NCOMP)
C     ------------------------------------------------------------------
      DHUN=100.
      DTWO=2.
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
C
C-----FOR EACH COMPOSITE ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 M=1,NCOMP
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=0.
      TOTIN=0.
C
C-----TOTAL THE BUDGET TERMS
      DO 100 I=1,MTOT
      DO 100 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
C
C-----TOTAL THE FLOW ACROSS ZONE BOUNDARIES
      DO 150 I=0,NZDIM
C
C-----SKIP FLOW TO ANY ZONES THAT ARE PART OF COMPOSITE ZONE
      DO 130 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(I.EQ.N) GO TO 150
130   CONTINUE
      DO 140 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TOTIN=TOTIN+VBZNFL(1,N,I)
      TOTOUT=TOTOUT+VBZNFL(2,N,I)
140   CONTINUE
150   CONTINUE
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.0. .AND. TOTOUT.EQ.0.) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C-----PRINT BUDGET---
C
C-----PRINT THE TITLE
      WRITE(IOUT,'(1H1,/1X,A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,601) NAMCOMP(M),KSTP,KPER
      WRITE(IOUT,*)
      WRITE(IOUT,611) NAMCOMP(M),(LSTZON(ICOMP(J,M)),J=1,NZWCZ(M))
C
C-----TOTAL AND PRINT THE IN BUDGET TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,602)
      DO 200 I=1,MTOT
      TSUM1=0.
      DO 180 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBVL(1,I,N)
180   CONTINUE
      WRITE(IOUT,603) VBNM(I),TSUM1
200   CONTINUE
C
C-----TOTAL AND PRINT THE IN FLOW ACROSS ZONE BOUNDARIES
      DO 250 I=0,NZDIM
      DO 230 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(N.EQ.I) GO TO 250
      TSUM1=0.
      TSUM2=0.
230   CONTINUE
      DO 240  J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBZNFL(1,N,I)
      TSUM2=TSUM2+VBZNFL(2,N,I)
240   CONTINUE
      IF(TSUM1.NE.0. .OR. TSUM2.NE.0.)
     1            WRITE(IOUT,609) LSTZON(I),NAMCOMP(M),TSUM1
250   CONTINUE
C
C-----WRITE THE TOTALS OF ALL INS
      WRITE(IOUT,*)
      WRITE(IOUT,604) TOTIN
C
C-----TOTAL AND PRINT THE OUT BUDGET TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,605)
      DO 300 I=1,MTOT
      TSUM2=0.
      DO 280 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM2=TSUM2+VBVL(2,I,N)
280   CONTINUE
      WRITE(IOUT,603) VBNM(I),TSUM2
300   CONTINUE
C
C-----TOTAL AND PRINT THE OUT FLOW ACROSS ZONE BOUNDARIES
      DO 350 I=0,NZDIM
      DO 330 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(N.EQ.I) GO TO 350
330   CONTINUE
      TSUM1=0.
      TSUM2=0.
      DO 340  J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBZNFL(1,N,I)
      TSUM2=TSUM2+VBZNFL(2,N,I)
340   CONTINUE
      IF(TSUM1.NE.0. .OR. TSUM2.NE.0.)
     1            WRITE(IOUT,610) NAMCOMP(M),LSTZON(I),TSUM2
350   CONTINUE
C
C-----WRITE TOTAL OUTS
      WRITE(IOUT,*)
      WRITE(IOUT,606) TOTOUT
C
C-----PRINT IN-OUT AND THE PERCENT ERROR
      WRITE(IOUT,*)
      WRITE(IOUT,607) TOTBD
      WRITE(IOUT,*)
      WRITE(IOUT,608) PERCNT
C
  500 CONTINUE
C
      RETURN
C
C    ---FORMATS---
C
  601 FORMAT(5X,'Flow Budget for Composite Zone ',A,
     1  ' at Time Step',I4,' of Stress Period',I4/5X,79('-'))
  602 FORMAT(23X,'Budget Term',5X,'Flow (L**3/T)'/
     1     23X,29('-')//13X,'IN:'/13X,'---')
  603 FORMAT(18X,A,' =',G14.5)
  604 FORMAT(26X,'Total IN =',G14.5)
  605 FORMAT(13X,'OUT:'/13X,4('-'))
  606 FORMAT(25X,'Total OUT =',G14.5)
  607 FORMAT(26X,'IN - OUT =',G14.5)
  608 FORMAT(15X,'Percent Discrepancy =',F20.2)
  609 FORMAT(12X,'Zone',I4,' to ',A,' =',G14.5)
  610 FORMAT(12X,A,' to Zone',I4,' =',G14.5)
  611 FORMAT(5X,'Composite Zone ',A,
     1      ' consists of the following numeric zones:'/(5X,15I4))
      END SUBROUTINE
C
      SUBROUTINE BUDGETPRECISION(IU,NCOL,NROW,NLAY)
C     ******************************************************************
C     Determine single or double precision file type for a MODFLOW
C     budget file:  0=unrecognized, 1=single, 2=double.
C     ******************************************************************
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: real32, real64
      USE ZONBUDMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN   ):: IU
      INTEGER, INTENT(INOUT):: NCOL,NROW,NLAY
      !
      INTEGER:: KSTP, KPER, NODES, NLST
      INTEGER:: N,NC,NR,NL, ICELL, ICODE
      REAL(real32):: DELT,  PERTIM,  TOTIM, VAL
      REAL(real64):: DELTD, PERTIMD, TOTIMD, VALD
      CHARACTER(16):: TEXT1, TEXT2
      REAL(real32):: Zs
      REAL(real64):: Zd
      Zs = 0.0_real32
      Zd = 0.0_real64
C
C  Default is unrecognized file
      IPREC  =0
      TEXT1  = ''
      TEXT2  = ''
      DELT   = Zs
      PERTIM = Zs
      TOTIM  = Zs
      DELTD  = Zd
      PERTIMD= Zd
      TOTIMD = Zd
C
C  SINGLE check
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NCOL,NROW,NLAY
      !
      ICODE = NLAY
      NLAY  = ABS(NLAY)
      NODES = NCOL*NROW*NLAY
      !
      IF(NCOL.LT.1 .OR. NROW.LT.1 .OR. NLAY.LT.1) GO TO 100
      IF(NCOL.GT.100000000 .OR.NROW.GT.100000000 .OR.
     1                     NLAY.GT.100000000) GO TO 100
      IF(NCOL*NROW.GT.100000000 .OR. NCOL*NLAY.GT.100000000 .OR.
     1                 NROW*NLAY.GT.100000000) GO TO 100
      !
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
      ALLOCATE (BUFFD(NCOL,NROW,NLAY))
      !
      IF(ICODE < 0) THEN
          READ(IU,ERR=50,END=50) ICODE,DELT,PERTIM,TOTIM
          !
          IF(DELT < Zs .or.    PERTIM < Zs .or.    TOTIM < Zs .or. 
     +       DELT > PERTIM .or. PERTIM > TOTIM) GO TO 50
      END IF
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=50,END=50) BUFF
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=50,END=50) NLST
         IF(NLST.LT.0) GO TO 50
         IF(NLST.GT.0) THEN
            DO 22 N=1,NLST
            READ(IU,END=50,ERR=50) ICELL,VAL
            IF(ICELL.LE.0 .OR. ICELL.GT.NODES) GO TO 50
22          CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF
C
C  Read 2nd header and check for valid type.
      READ(IU,ERR=50,END=50) KSTP,KPER,TEXT2
      IF(TEXT1.EQ.'         STORAGE') THEN
          IF(TEXT2.EQ.'   CONSTANT HEAD' .OR.
     +       TEXT2.EQ.'FLOW RIGHT FACE '     ) THEN
                  IPREC=1
                  GO TO 100
          END IF
      ELSEIF(TEXT1.EQ.'   CONSTANT HEAD') THEN
          IF(TEXT2.EQ.'FLOW RIGHT FACE ') THEN
                  IPREC=1
                  GO TO 100
          END IF
      ELSEIF(TEXT1.EQ.'FLOW RIGHT FACE ' .AND.
     +       TEXT2.EQ.'FLOW FRONT FACE '      ) THEN
                  IPREC=1
                  GO TO 100
      END IF
!      IF(TEXT1.EQ.'         STORAGE' .AND.
!     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
!           IPREC=1
!           GO TO 100
!      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
!     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
!           IPREC=1
!           GO TO 100
!      END IF
C
C  DOUBLE check
50    REWIND(IU)
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NC,NR,NL
      ICODE=0
      IF(NL.LT.0) THEN
        NL=-NL
        READ(IU,ERR=100,END=100) ICODE,DELTD,PERTIMD,TOTIMD
        IF(DELTD < Zd .or.      PERTIMD < Zd .or.    TOTIMD < Zd .or. 
     +     DELTD > PERTIMD .or. PERTIMD > TOTIMD) GO TO 100
      END IF
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=100,END=100) BUFFD
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=100,END=100) NLST
         IF(NLST.LT.0) GO TO 100
         IF(NLST.GT.0) THEN
            DO 72 N=1,NLST
            READ(IU,END=100,ERR=100) ICELL,VALD
            IF(ICELL.LE.0 .OR. ICELL.GT.NODES) GO TO 100
72          CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF
C
C  Read 2nd header and check for valid type.
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT2
      IF(TEXT1.EQ.'         STORAGE') THEN
          IF(TEXT2.EQ.'   CONSTANT HEAD' .OR.
     +       TEXT2.EQ.'FLOW RIGHT FACE '     ) THEN
                  IPREC=2
          END IF
      ELSEIF(TEXT1.EQ.'   CONSTANT HEAD') THEN
          IF(TEXT2.EQ.'FLOW RIGHT FACE ') THEN
                  IPREC=2
          END IF
      ELSEIF(TEXT1.EQ.'FLOW RIGHT FACE ' .AND.
     +       TEXT2.EQ.'FLOW FRONT FACE '      ) THEN
                  IPREC=2
      END IF
!      IF(TEXT1.EQ.'         STORAGE' .AND.
!     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
!           IPREC=2
!      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
!     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
!           IPREC=2
!      END IF
C
100   REWIND(IU)
      RETURN
      END SUBROUTINE
C
      SUBROUTINE CSVSUBPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IUCSV,
     1               NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND WRITE CSV FILE
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM),
     1          LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,DHUN,DTWO,ZERO
      DOUBLE PRECISION TOTOUT(NZDIM),TOTIN(NZDIM)
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      DOUBLE PRECISION TOTIMD
      CHARACTER*16 FIELD(0:NZDIM)
C     ------------------------------------------------------------------
      ZERO=0.0
C
      DO 5 K=1,NZDIM
      TOTIN(K)=ZERO
      TOTOUT(K)=ZERO
5     CONTINUE
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C-----PRINT THE TITLE
      IF(TOTIMD.LT.0.) THEN
        WRITE(IUCSV,601) KSTP,KPER,TRIM(TITLE)
  601   FORMAT('Time Step,',I3,',Stress Period,',I3,',',A,',')
      ELSE
        WRITE(IUCSV,602) KSTP,KPER,TOTIMD,TRIM(TITLE)
  602   FORMAT('Time Step,',I4,',Stress Period,',I4,
     1              ',Sim. Time,',ES21.13,',',A,',')
      END IF
C
C-----GENERATE and PRINT each Row for all zones
C-----Zone Numbers
      FIELD(0)=' '
      DO 10 K=1,NZDIM
      WRITE(FIELD(K),7) LSTZON(K)
    7 FORMAT('  ZONE',I4,'      ')
10    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
   11 FORMAT(2000(A,','))
C
C-----IN Labels
      FIELD(0)=' '
      DO 20 K=1,NZDIM
      FIELD(K)= '    IN'
20    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----ALL BUDGET TERM INFLOWS
      DO 40 M=1,MTOT
      FIELD(0)=VBNM(M)
      DO 30 K=1,NZDIM
      TOTIN(K)=TOTIN(K)+VBVL(1,M,K)
      WRITE(FIELD(K),81) VBVL(1,M,K)
   81 FORMAT(ES16.6)
30    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
40    CONTINUE
C
C-----Inflows from other zones
      DO 60 N=0,NZDIM
      IF(N.EQ.0 .AND. LSTZON(N).LT.0) GO TO 60
      WRITE(FIELD(0),41) LSTZON(N)
   41 FORMAT('   FROM ZONE',I4)
      DO 50 K=1,NZDIM
      TOTIN(K)=TOTIN(K)+VBZNFL(1,K,N)
      WRITE(FIELD(K),81) VBZNFL(1,K,N)
50    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
60    CONTINUE
C
C-----Total inflow
      FIELD(0)='Total IN        '
      DO 70 K=1,NZDIM
      WRITE(FIELD(K),81) TOTIN(K)
70    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----OUT Labels
      FIELD(0)=' '
      DO 200 K=1,NZDIM
      FIELD(K)= '   OUT'
200   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----ALL BUDGET TERM OUTFLOWS
      DO 240 M=1,MTOT
      FIELD(0)=VBNM(M)
      DO 230 K=1,NZDIM
      TOTOUT(K)=TOTOUT(K)+VBVL(2,M,K)
      WRITE(FIELD(K),81) VBVL(2,M,K)
230   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
240   CONTINUE
C
C-----Outflows to other zones
      DO 260 N=0,NZDIM
      IF(N.EQ.0 .AND. LSTZON(N).LT.0) GO TO 260
      WRITE(FIELD(0),242) LSTZON(N)
  242 FORMAT('     TO ZONE',I4)
      DO 250 K=1,NZDIM
      TOTOUT(K)=TOTOUT(K)+VBZNFL(2,K,N)
      WRITE(FIELD(K),81) VBZNFL(2,K,N)
250   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
260   CONTINUE
C
C-----Total outflow
      FIELD(0)='Total OUT       '
      DO 270 K=1,NZDIM
      WRITE(FIELD(K),81) TOTOUT(K)
270   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----IN-OUT
      FIELD(0)=' IN-OUT          '
      DO 280 K=1,NZDIM
      WRITE(FIELD(K),81) TOTIN(K)-TOTOUT(K)
280   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----Percent error
      FIELD(0)='Percent Error   '
      DO 290 K=1,NZDIM
      WRITE(FIELD(K),81) DHUN*(TOTIN(K)-TOTOUT(K))/
     1                       ((TOTIN(K)+TOTOUT(K))/DTWO)
290   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
      WRITE(IUCSV,'(A)') ','
C
      RETURN
      END SUBROUTINE
C
      SUBROUTINE CSVSUBPR2(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IUCSV,
     1               NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND WRITE CSV FILE
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM),
     1         LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,TOTOUT,TOTIN,TOTBD,DHUN,DTWO,ZERO
      DOUBLE PRECISION TOTZONIN,TOTZONOUT
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      DOUBLE PRECISION TOTIMD
      CHARACTER*21 ZONINNAM(0:NZDIM)
      CHARACTER*21 ZONOUTNAM(0:NZDIM)
      CHARACTER*21 FIELD(NZDIM*2+NTRDIM+10)
      INTEGER,SAVE    ::IFIRST=1
C     ------------------------------------------------------------------
      ZERO=0.0
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C-----Create Zone labels
      DO 2 I=0,NZDIM
      WRITE(ZONINNAM(I),11) LSTZON(I)
   11 FORMAT('   FROM ZONE',I4)
      WRITE(ZONOUTNAM(I),12) LSTZON(I)
   12 FORMAT('     TO ZONE',I4)
    2 CONTINUE
C
C-----PRINT THE HEADINGS ONLY FOR THE FIRST TIME STEP
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
C
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
        DO 50 K=0,NZDIM-1
        DO 50 J=K+1,NZDIM
        DO 50 I=1,2
        VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
50      CONTINUE
C
C-----PRINT COLUMN HEADERS
        NFIELD=1
        FIELD(NFIELD)=' TOTIM'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='  PERIOD'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='   STEP'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='   ZONE         '
C  Add storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
        IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)='         STORAGE'
        END IF
        DO 72 M=1,MTOT
        NFIELD=NFIELD+1
        FIELD(NFIELD)=VBNM(M)
   72   CONTINUE
        NFIELD=NFIELD+1
        FIELD(NFIELD)='From Other Zones'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='Total IN        '
C  Add storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
        IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)='         STORAGE'
        END IF
        DO 73 M=1,MTOT
        NFIELD=NFIELD+1
        FIELD(NFIELD)=VBNM(M)
   73   CONTINUE
        NFIELD=NFIELD+1
        FIELD(NFIELD)='To Other Zones'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='Total Out       '
        NFIELD=NFIELD+1
        FIELD(NFIELD)=' IN-OUT          '
        NFIELD=NFIELD+1
        FIELD(NFIELD)='Percent Error   '
C  Put zone fields twice -- once for IN and once for OUT
        IF(LSTZON(0).EQ.0) THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)=ZONINNAM(0)
        END IF
        DO 74 K=1,NZDIM
        NFIELD=NFIELD+1
        FIELD(NFIELD)=ZONINNAM(K)
   74   CONTINUE
        IF(LSTZON(0).EQ.0) THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)=ZONOUTNAM(0)
        END IF
        DO 75 K=1,NZDIM
        NFIELD=NFIELD+1
        FIELD(NFIELD)=ZONOUTNAM(K)
   75   CONTINUE
        WRITE(IUCSV,7) (FIELD(I),I=1,NFIELD)
    7   FORMAT(1000(A,','))
      END IF
C
C-----FOR EACH ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 N=1,NZDIM
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=ZERO
      TOTIN=ZERO
      TOTZONIN=ZERO
      TOTZONOUT=ZERO
      DO 100 I=1,MTOT
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
      DO 150 I=0,NZDIM
      IF(LSTZON(N).LT.0) GO TO 150
      TOTZONIN=TOTZONIN+VBZNFL(1,N,I)
      TOTZONOUT=TOTZONOUT+VBZNFL(2,N,I)
150   CONTINUE
      TOTIN=TOTIN+TOTZONIN
      TOTOUT=TOTOUT+TOTZONOUT
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.ZERO .AND. TOTOUT.EQ.ZERO) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C     ---PRINT BUDGET---
C
      NFIELD=1
      IF(TOTIMD.GE.0.) THEN
         WRITE(FIELD(NFIELD),81) TOTIMD
      ELSE
         WRITE(FIELD(NFIELD),'(A)') 'UNDEFINED'
      END IF
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),'(I21)') KPER
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),'(I21)') KSTP
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),'(I21)') LSTZON(N)
C  Print storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
      IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) 0.0
      END IF
      DO 82 I=1,MTOT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) VBVL(1,I,N)
   81 FORMAT(ES21.13)
   82 CONTINUE
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTZONIN
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTIN
C  Print storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
      IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) 0.0
      END IF
      DO 84 I=1,MTOT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) VBVL(2,I,N)
   84 CONTINUE
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTZONOUT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTOUT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTBD
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) PERCNT
      DO 83 I=0,NZDIM
      IF(LSTZON(I).GE.0) THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) VBZNFL(1,N,I)
      END IF
   83 CONTINUE
      DO 85 I=0,NZDIM
      IF(LSTZON(I).GE.0) THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) VBZNFL(2,N,I)
      END IF
   85 CONTINUE
      WRITE(IUCSV,7) (FIELD(I),I=1,NFIELD)
C
  500 CONTINUE
C
      RETURN
      END SUBROUTINE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
C  The following routines originated from the main mf-owhm code.         |
C    They have been simplified for use in ZoneBudget input structure.    |
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
C
!      SUBROUTINE U2DINT(IA,ANAME,II,JJ,IN,IOUT,K)
!C     ******************************************************************
!C     Modified for use with ZoneBudget
!C     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
!C       IA IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF IA
!C       II IS NO. OF ROWS
!C       JJ IS NO. OF COLS
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      IMPLICIT NONE
!      CHARACTER(24), INTENT(IN):: ANAME
!      INTEGER,       INTENT(IN):: II,JJ,K,IN,IOUT
!      INTEGER, DIMENSION(JJ,II), INTENT(INOUT):: IA
!      CHARACTER(20):: FMTIN
!      CHARACTER(768):: CNTRL, FNAME
!      INTEGER:: IE, N, I, J, IERR
!      INTEGER:: ICOL, ISTART, ISTOP
!      INTEGER:: ICLOSE, IFREE, ICONST, IPRN, LOCAT
!      LOGICAL:: FAIL
!      INTEGER:: Z, ONE
!      LOGICAL:: TRUE, FALSE
!      CHARACTER   :: NL
!      CHARACTER(2):: BLN
!      CHARACTER(16):: cL
!C     ------------------------------------------------------------------
!      NL  = NEW_LINE(' ')
!      BLN = NL//NL
!      Z   = 0
!      ONE = 1
!      TRUE  = .TRUE.
!      FALSE = .FALSE.
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
!      CALL READ_TO_DATA(CNTRL,IN,IOUT)
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
!      ICLOSE = Z
!      IFREE  = ONE
!      ICOL   = ONE
!      CALL PARSE_WORD_UP(CNTRL,ICOL,ISTART,ISTOP)
!      IF     (ISTOP < ISTART) THEN                   !Empty line read - Stop program
!                                  GOTO 600
!      ELSE IF(CNTRL(ISTART:ISTOP) == 'CONSTANT') THEN
!         CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,IN,ICONST,
!     +  'ZoneBudget found input directive "CONSTANT", but'//NL//
!     +  'failed to read the constant value, ICONST, after the keyword.')
!         !
!         IA = ICONST
!         !
!         IF(IOUT /= Z) THEN
!           IF(K >  Z) WRITE(IOUT,82) ANAME,ICONST,K
!           IF(K <= Z) WRITE(IOUT,83) ANAME,ICONST
!         ENDIF
!   82    FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
!   83    FORMAT(1X,/1X,A,' =',I15)
!         RETURN
!         !
!      ELSE IF(CNTRL(ISTART:ISTOP) == 'INTERNAL') THEN
!         LOCAT = IN
!      ELSE IF(CNTRL(ISTART:ISTOP) == 'EXTERNAL') THEN
!         LOCAT = 0
!         CALL PARSE_WORD(CNTRL,ICOL,ISTART,ISTOP)
!         FNAME = CNTRL(ISTART:ISTOP)
!         INQUIRE(FILE=FNAME, NUMBER=LOCAT, OPENED=FAIL)  ! Fail is true when file already opened
!         !
!         IF(FAIL) LOCAT = -1 
!         !
!         IF(IOUT /= Z) THEN
!            IF(FAIL) THEN
!                     WRITE(IOUT,15) FNAME
!            ELSE
!                     WRITE(IOUT,14) FNAME
!            END IF
!         END IF
!   14    FORMAT(1X,/1X,'CONTINUE READING FROM:',1X,A)
!         !
!      ELSE IF(CNTRL(ISTART:ISTOP) == 'OPEN/CLOSE') THEN
!         CALL PARSE_WORD(CNTRL,ICOL,ISTART,ISTOP)
!         FNAME = CNTRL(ISTART:ISTOP)
!         LOCAT = -1 
!         IF(IOUT /= Z) WRITE(IOUT,15) FNAME
!   15    FORMAT(1X,/1X,'OPENING FILE :',1X,A)
!         ICLOSE = ONE
!      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
!         IFREE = Z
!         READ(CNTRL,"(I10,I10)",IOSTAT=IE) N, ICOL
!         IF(IE /= Z) GO TO 60
!         !
!    1    FORMAT(I10,I10,A20,I10)
!         READ(CNTRL,1,IOSTAT=IE) LOCAT,ICONST,FMTIN,IPRN
!         IF(IE /= Z .AND. N == Z) THEN
!                                  IE     = Z
!                                  LOCAT  = N
!                                  ICONST = ICOL
!                                  FMTIN  = ""
!                                  IPRN   = Z
!         END IF
!         !
!   60    IF(IE /= Z) THEN
!           IF(K > Z) THEN
!             WRITE(cL,'(I16)') K
!             cL = ADJUSTL(cL)
!             CALL BACKSPACE_STOP_MSG(IN, IOUT, Z, 
!     +              'U2DINT - ERROR READING FOR "'//ANAME//'"'//NL//
!     +              'WITH AN ARRAY CONTROL RECORD SPECIFIED AS: '//
!     +              '"'//TRIM(CNTRL)//'"'//NL//
!     +              '"FOR LAYER '//TRIM(cL))
!           ELSE
!             CALL BACKSPACE_STOP_MSG(IN, IOUT, Z, 
!     +              'U2DINT - ERROR READING FOR "'//ANAME//'"'//NL//
!     +              'WITH AN ARRAY CONTROL RECORD SPECIFIED AS: '//
!     +              '"'//TRIM(CNTRL)//'"')
!           END IF
!         END IF
!      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
!      IF(IFREE /= Z) THEN
!         CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,IN,ICONST,
!     +                                            'NOSTOP')
!         FAIL = ICONST == HUGE(ICONST)
!         IF(LOCAT == Z .AND. FAIL) THEN
!           CALL STOP_ERROR(CNTRL, IN, IOUT, 
!     +                     'U2DINT - FAILED TO READ ICONST')
!         ELSEIF(FAIL) THEN
!            ICONST=1
!            IF(ISTOP < ISTART) THEN
!                   CONTINUE
!            ELSEIF(CNTRL(ISTART:ISTART) == '#') THEN
!                   ICOL = LEN(CNTRL) + 1
!                   ISTART = ICOL   - 1
!                   ISTOP  = ISTART - 1
!            ELSE
!                   ICOL = ISTART
!            END IF
!         END IF
!         !
!         IF(LOCAT /= Z) THEN
!            CALL PARSE_WORD_UP(CNTRL,ICOL,ISTART,ISTOP)
!            !
!            IF(ISTOP < ISTART) THEN
!                   FMTIN = '(FREE)'
!            ELSEIF(CNTRL(ISTART:ISTART) == '#') THEN
!                   FMTIN = '(FREE)'
!                   ICOL = LEN(CNTRL) + 1
!                   ISTART = ICOL   - 1
!                   ISTOP  = ISTART - 1
!            ELSE
!                   FMTIN=CNTRL(ISTART:ISTOP)
!            END IF
!            !
!            IF(ICLOSE /= Z) THEN
!             OPEN(NEWUNIT=LOCAT, FILE=FNAME, STATUS='OLD', IOSTAT=IERR)
!             IF(IERR /= Z) CALL STOP_ERROR(CNTRL,IN,IOUT, 
!     +                                    'Failed to open zone file')
!            END IF
!            CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,IN,IPRN,
!     +                                             'NOSTOP')
!            FAIL = IPRN == HUGE(IPRN)
!            IF(FAIL) IPRN=-1
!         END IF
!      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
!      IF(LOCAT == Z) THEN
!C
!C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
!        DO 80 I=1,II
!        DO 80 J=1,JJ
!   80   IA(J,I)=ICONST
!        IF(IOUT /= Z) THEN
!          IF(K >  Z) WRITE(IOUT,82) ANAME,ICONST,K
!          IF(K <= Z) WRITE(IOUT,83) ANAME,ICONST
!        ENDIF
!   82   FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
!   83   FORMAT(1X,/1X,A,' =',I15)
!        RETURN
!      ELSE IF(LOCAT /= Z) THEN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
!        IF(IOUT /= Z) THEN
!            IF(K > Z) THEN
!                 WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
!            ELSE IF(K == Z) THEN
!                 WRITE(IOUT,95) ANAME,LOCAT,FMTIN
!            ELSE
!                 WRITE(IOUT,96) ANAME,LOCAT,FMTIN
!            END IF
!   94       FORMAT(1X,///11X,A,' FOR LAYER',I4,/1X,
!     +       'READING ON UNIT ',I4,' WITH FORMAT: ',A)
!   95       FORMAT(1X,///11X,A,/ 1X,
!     +       'READING ON UNIT ',I4,' WITH FORMAT: ',A)
!   96       FORMAT(1X,///11X,A,' FOR CROSS SECTION',/1X,
!     +       'READING ON UNIT ',I4,' WITH FORMAT: ',A)
!        END IF
!        DO 100 I=1,II
!        CALL MOVE_TO_DATA(CNTRL,LOCAT,IOUT)  !Bypass any comments and empty lines to move to next line to read input
!        IF(FMTIN == '(FREE)') THEN
!           READ(LOCAT,    *,IOSTAT=IE) IA(1:JJ,I)
!        ELSE
!           READ(LOCAT,FMTIN,IOSTAT=IE) IA(1:JJ,I)
!        END IF
!           IF(IE /= Z) CALL BACKSPACE_STOP_MSG(LOCAT, IOUT, IE,
!     +     'U2DINT - ERROR READING ON CURRENT INPUT LINE A SET OF '//
!     +     'INTEGERS FOR "'//TRIM(ADJUSTL(ANAME))//'"'//BLN//
!     +     '**NOTE ERROR LINE IS THE BEST GUESS FOR ERROR LOCATION.')
!  100   CONTINUE
!      END IF
!C
!C5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
!      IF(ICLOSE /= Z) CLOSE(UNIT=LOCAT)
!      IF(ICONST == Z .or. ICONST == 1) GO TO 320
!      DO 310 I=1,II
!      DO 310 J=1,JJ
!      IA(J,I)=IA(J,I)*ICONST
!  310 CONTINUE
!C
!C6------IF PRINT CODE (IPRN) <0 THEN RETURN.
!  320 IF(IPRN < Z .OR. IOUT == Z) RETURN
!C
!C-----PRINT COLUMN NUMBERS AT THE TOP OF THE PAGE
!      WRITE(IOUT,421) (I,I=1,JJ)
!421   FORMAT(/,(5X,25I5))
!      WRITE(IOUT,'(A)')REPEAT('-',MAX(4*JJ+4,79))
!C
!C-----PRINT EACH ROW IN THE ARRAY.
!      DO 430 I=1,II
!      WRITE(IOUT,423) I, IA(1:JJ,I)
!423   FORMAT(1X,I3,1X,25I5/(5X,25I5))
!430   CONTINUE
!C
!C9------RETURN
!      RETURN
!C
!C10-----CONTROL RECORD ERROR.
!  600 IF(K > Z) THEN
!          WRITE(cL,'(I16)') K
!          cL = ADJUSTL(cL)
!          FNAME = 'ERROR READING ARRAY CONTROL RECORD FOR "'//
!     +            TRIM(ADJUSTL(ANAME))//'" FOR LAYER '//TRIM(cL)
!      ELSE
!          FNAME = 'ERROR READING ARRAY CONTROL RECORD FOR "'//
!     +            TRIM(ADJUSTL(ANAME))//'"'
!      END IF
!      !CALL USTOP(' ')
!      CALL STOP_ERROR(CNTRL, IN, IOUT, FNAME)
!      END SUBROUTINE
      !
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !
      !  STOP_ERROR Routine
      !
      SUBROUTINE BACKSPACE_STOP_MSG(IU, IN, IOUT, IERR, MSG)
      IMPLICIT NONE
      INTEGER,      INTENT(IN):: IU, IN, IOUT, IERR
      CHARACTER(*), INTENT(IN):: MSG
      INTEGER:: I
      CHARACTER(768):: LINE
      !
      BACKSPACE(IN)
      READ(IN, '(A)', IOSTAT=I) LINE
      !
      IF(I == 0) THEN
                 CALL FILE_IO_ERROR(IERR,IU,LINE,IN,IOUT,MSG)
      ELSE
                 CALL FILE_IO_ERROR(IERR,IU,'',IN,IOUT,MSG)
      END IF
      !
      END SUBROUTINE
      !
      SUBROUTINE STOP_ERROR(LINE, INFILE, OUTPUT, MSG)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN):: LINE       ! Line that error occured on
        INTEGER,      INTENT(IN):: INFILE     ! File Unit that error originated from
        INTEGER,      INTENT(IN):: OUTPUT     ! File unit to write error too
        CHARACTER(*), INTENT(IN):: MSG        ! Supplemental messages to write in error
        !
        CHARACTER(:), ALLOCATABLE:: FNAME
        CHARACTER(:), ALLOCATABLE:: ERR
        !
        LOGICAL:: HAS_LINE, HAS_INFILE, HAS_OUTPUT, HAS_MSG
        !
        INTEGER:: IOUT, IE
        INTEGER:: Z
        CHARACTER   :: NL, BLNK
        CHARACTER(2):: BLN
        Z = 0
        BLNK = ''
        NL = NEW_LINE(' ')
        BLN = NL//NL
        !
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !
        HAS_LINE = LINE /= BLNK
        !
        HAS_INFILE = INFILE /= Z
        !
        HAS_OUTPUT = OUTPUT /= Z
        !
        HAS_MSG = MSG  /= BLNK
        !
        IF(HAS_INFILE) ALLOCATE(CHARACTER(768):: FNAME)
        !
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !
        ERR=BLN//'                           ERROR'//BLN//
     +           '         THE FOLLOWING COMMENTS WERE PASSED TO '//
     +                                         'THE ERROR ROUTINE'//NL
        !
        IF(HAS_INFILE) THEN
            INQUIRE(INFILE,NAME=FNAME)
            ERR = ERR//NL//'THIS ERROR IS BELIEVED TO HAVE '//
     +                     'ORIGINATED FROM THE FOLLOWING FILE:'//NL//
     +                     '"'//TRIM(FNAME)//'"'//NL
        END IF
        !
        IF(HAS_LINE) ERR = ERR//NL//'THE GUESSED LINE THAT THE '//
     +                              'ERROR OCCURED ON IS:'//BLN//
     +                              '"'//TRIM(LINE)//'"'//NL
        !
        IF(HAS_MSG)  ERR = ERR//NL//
     +                    'THE DESCRIPTION OF THE ERROR IS:'//BLN//
     +                    TRIM(MSG)//NL
        !
        IF( .NOT. (HAS_LINE.or.HAS_INFILE.or.HAS_MSG) ) ERR = ERR//BLN//
     +                             ' --- SORRY UNKNOWN ERROR ---'//BLN
        !
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !
        WRITE(*, '(A/)') ERR  ! WRITE TO CMD PROMPT FIRST
        !
        IF(HAS_OUTPUT) WRITE(OUTPUT,'(A/)') ERR
        !
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !
        ERROR STOP ! :(
        !
      END SUBROUTINE
      !
      !#########################################################################################################################
      !
      SUBROUTINE FILE_IO_ERROR(IOSTAT,IU,LINE,INFILE,IOUT,MSG)
      ! IOSTAT IS THE ASSOCIATED IOSTAT ERROR
      ! IU     IS THE FILE THAT IS BEING OPENED, READ FROM, OR WRITTEN TOO
      ! LINE   IS THE LINE THAT IS BEING PROCESSED EITHER FOR PARSING THE INPUT FILE, READING DATA, OR WRITING DATA
      ! INFILE IS THE FILE FROM WHICH LINE ORIGINATED FROM. IT CAN BE THE SAME FILE AS FNAME
      ! MSG    IS AN ADDITIONAL ERROR MESSAGE THAT IS APPEND TO THE END OF THE ERROR REPORT
      !
      INTEGER,      INTENT(IN):: IOSTAT, IU, INFILE, IOUT
      CHARACTER(*), INTENT(IN):: LINE,MSG
      !
      INTEGER:: Z
      LOGICAL:: ISOPEN
      CHARACTER(  1):: BLNK, NL
      CHARACTER(  2):: BLN
      CHARACTER(512):: LN
      CHARACTER(:), ALLOCATABLE:: ERRMSG, FN, ERRLINE, INLINE
      CHARACTER(:), ALLOCATABLE:: MSGLINE, ERR_CODE
      !
      INTERFACE
              PURE FUNCTION INT2STR(IVAL)
                INTEGER,       INTENT(IN):: IVAL
                CHARACTER(:), ALLOCATABLE:: INT2STR
              END FUNCTION
      END INTERFACE
      !
      Z = 0
      BLNK = ''
      NL  = NEW_LINE(' ')
      BLN = NL//NL
      !
      INLINE='¿¿¿UNKOWN FILE???'
      !
      IF (INFILE /= Z) THEN
         INQUIRE(INFILE,NAME=LN)
         INLINE=TRIM(ADJUSTL(LN))
      END IF
      !
      IF(LINE /= BLNK) THEN
          ERRLINE=TRIM(ADJUSTL(LINE))
      ELSE
          ERRLINE='¿¿¿UNKOWN LINE???'
      END IF
      !
      IF(IU /= Z)THEN
          INQUIRE(IU,NAME=LN,OPENED=ISOPEN)
          IF(ISOPEN) THEN
              FN=TRIM(ADJUSTL(LN))
          ELSE
              FN=BLNK
          END IF
      ELSE
          FN=BLNK
          ISOPEN=FALSE
      END IF
      !
      IF(MSG /= BLNK) THEN
                      MSGLINE=TRIM(ADJUSTL(MSG))
      ELSE
                      MSGLINE=BLNK
      END IF
      !
      IF(IOSTAT  /=  Z) THEN
        ERR_CODE='AND HAS THE FOLLOWING IOSTAT ERROR CODE: '//
     +                                         INT2STR(IOSTAT)
        IF(ISOPEN) THEN
          IF(IOSTAT<Z) THEN
            ERR_CODE=ERR_CODE//BLN//
     +'    ERROR<0 INDICATES THAT THE END OF FILE WAS REACHED'//NL//
     +'    OR A END OF RECORD CONDITION OCCURED'//NL//
     +'    OR YOU DID NOT HAVE ENOUGH INPUT VALUES SPECIFIED ON LINE.'//
     +                                                              NL//
     +'    NOTE THAT IF YOU MAY HAVE THE CORRECT NUMBER OF VALUES '//
     +'ON THE LINE'//NL//
     +'      BUT A BAD FORMAT DESCRIPTOR (FMTIN),'//NL//
     +'      GENERALLY FMTIN = "(FREE)" IS THE SAFEST METHOD '//
     +'TO LOAD DATA.'
          ELSE
            ERR_CODE=ERR_CODE//BLN//
     +'    ERROR>0 INDICATES THAT YOU HAVE TO LOOK UP THE '//
     +'SPECIFIC ERROR CONDITION SPECIFIED BY THE COMPILER.'//NL//
     +'    TYPICALLY THIS INDICATES A BAD FORMAT DESCRIPTOR (FMTIN),'//
     +                                                             NL//
     +'      GENERALLY FMTIN = "(FREE)" IS THE SAFEST METHOD '//
     +'TO LOAD DATA.'
          END IF
        END IF
      ELSE
          ERR_CODE=BLNK
      END IF
      !
      IF(INFILE == Z) THEN
         !
         IF(ISOPEN) THEN
          ERRMSG=NL//'FILE I/O ERROR:'                           //BLN//
     +    'FOR FILE UNIT '//INT2STR(IU)                          //BLN//
     +    'WHICH IS ASSOCIATED WITH FILE: '//FN                  //BLN//
     +    'WHILE READING OR WRITING LINE '//NL//'"'//ERRLINE//'"'//BLN//
     +     ERR_CODE
         ELSEIF( .NOT. ISOPEN .AND. (IU /= Z .OR. FN /= BLNK) )THEN
             IF(IU==Z) THEN
                   ERRMSG=NL//'FILE I/O ERROR:'                 //BLN//
     +            'FOR AN UNKNOWN FILE UNIT [POSSIBLE FAILURE TO '//
     +                                       'OPEN/FIND FILE]'  //BLN//
     +            'FOR THE REQUESTED FILE NAME: '//FN           //BLN//
     +            ERR_CODE
             ELSE
                   ERRMSG=NL//'FILE I/O ERROR:'           //BLN//
     +            'FOR FILE UNIT '//INT2STR(IU)//
     +            ' [POSSIBLE FAILURE TO OPEN/FIND FILE]' //BLN//
     +            'WITH UNKNOWN FILE NAME'                //BLN//
     +            ERR_CODE
             END IF          
         ELSE
           ERRMSG=NL//'FILE I/O ERROR:'  //BLN//  
     +     'FOR AN UNKNOWN FILE UNIT AND FILE [POSSIBLE FAILURE TO  '//
     +                                       'OPEN/FIND FILE]'  //BLN//
     +      ERR_CODE          //NL// 
     +     'FOR THE FOLLOWING LINE '//NL//'"'//ERRLINE//'"'
         END IF
         !
         !
      ELSE
         !
         !
         IF(ISOPEN) THEN
             ERRMSG=NL//'FILE I/O ERROR:'               //BLN//
     +       'FOR FILE UNIT '//INT2STR(IU)              //BLN//
     +       'WHICH IS ASSOCIATED WITH FILE: '//NL//FN  //BLN//
     +       'WHILE UTILIZING THE FOLLOWING LINE: '//NL//
     +       '"'//ERRLINE//'"'//BLN//
     +       'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//
     +       '"'//INLINE//'"' //BLN//
     +       ERR_CODE
         ELSEIF( .NOT. ISOPEN .AND. (IU /= Z .OR. FN /= BLNK) )THEN
             IF(IU.EQ.Z) THEN
             ERRMSG=NL//'FILE I/O ERROR:'              //BLN//
     +       'FOR AN UNKNOWN FILE UNIT [POSSIBLE FAILURE TO  '//
     +                              'OPEN/FIND FILE]'  //BLN//
     +       'FOR THE REQUESTED FILE NAME: '//NL//FN   //BLN//
     +       ERR_CODE                                  //BLN//
     +       'WHILE UTILIZING THE FOLLOWING LINE: '//NL//
     +       '"'//ERRLINE//'"'    //BLN// 
     +       'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//
     +       '"'//INLINE//'"'
             ELSE
             ERRMSG=NL//'FILE I/O ERROR:'                  //BLN//
     +       'FOR FILE UNIT '//INT2STR(IU)//' [POSSIBLE FAILURE TO  '//
     +                              'OPEN/FIND FILE]' //BLN// 
     +       'WITH UNKNOWN FILE NAME'                 //BLN//
     +       ERR_CODE                                 //BLN//
     +       'WHILE UTILIZING THE FOLLOWING LINE: '//NL//
     +       '"'//ERRLINE//'"' //BLN//
     +       'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//
     +       '"'//INLINE//'"'
             END IF          
         ELSE
           ERRMSG=NL//'FILE I/O ERROR:'                       //BLN//
     +     'FOR AN UNKNOWN FILE UNIT AND FILE [POSSIBLE FAILURE TO  '//
     +                            'OPEN/FIND FILE]' //BLN//
     +     ERR_CODE                                 //BLN//
     +     'WHILE UTILIZING THE FOLLOWING LINE: '   //NL//
     +     '"'//ERRLINE//'"'//BLN//
     +     'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//
     +     '"'//INLINE//'"'
         END IF
         !
         !
      END IF
      !
      IF(MSGLINE /= BLNK) ERRMSG=ERRMSG//BLN//
     +                   'THE FOLLOWING IS AN ADDITIONAL COMMENT '//
     +                   'INCLUDED WITH ERROR:'//BLN//MSGLINE
      !
      IF(IOUT /= Z) WRITE(IOUT,'(A)') ERRMSG
                    WRITE(*,   '(A)') ERRMSG
      !
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      ERROR STOP ! ;(
      !
      END SUBROUTINE
      !
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !
      !  READ_TO_DATA Routine
      !
      SUBROUTINE READ_TO_DATA(LINE,INFILE,ERROR)
        IMPLICIT NONE
        CHARACTER(*),         INTENT(INOUT):: LINE    !LINE TO LOAD DATA TOO
        INTEGER,              INTENT(IN   ):: INFILE  !UNIT OF FILE TO LOAD LINE FROM
        INTEGER,              INTENT(IN   ):: ERROR   !UNIT TO WRITE ERROR MESSAGE TOO
        INTEGER:: I, C, ERR
        CHARACTER:: CR, LF, TAB
        INTEGER:: Z
        Z = 0
        !
        CR  = ACHAR(13)  !CARAGE RETURN (UNIX ENDING)
        LF  = ACHAR(10)  !LINE   FEED (CRLF IS WINDOWNS ENDING)
        TAB = ACHAR( 9)
        !
        READ_LOOP: DO
              READ(INFILE,'(A)',IOSTAT=ERR) LINE
              IF    (ERR > Z) THEN                                   !LINE FAILED TO READ, THIS IS MOST LIKELY DUE TO END OF FILE LINE,INFILE,OUTPUT,MSG
                              CALL STOP_ERROR('', INFILE, ERROR, 
     +                          'Failed to read from file. Error '//
     +                          'occured in "SUBROUTINE READ_TO_DATA"')
              ELSEIF(ERR < Z) THEN !EOF
                                   LINE=''
                                   C=1
                                   BACKSPACE(INFILE) !NOTE THAT EOF COUNTS OF 1 READ, BUT MULTIPLE READS TO EOF WILL NOT MOVE ANY FURTHER, SO REPOSITION TO THE END OF THE FILE, BUT NOT ONE BEYOND TO KEEP N (THE READ COUNT) CORRET 
                                   EXIT READ_LOOP
              END IF
              !
              !----------------------------------------------------------------------------------------------------------
              DO CONCURRENT (I=1:LEN_TRIM(LINE), LINE(I:I)==TAB .OR. 
     +                                           LINE(I:I)==CR  .OR. 
     +                                           LINE(I:I)==LF)  !TAB = CHAR(9) -> Fortran treates TAB as if it was character--make spaces to improve search --also remove dangling CR or LF  --> Note that this is identicaly to "CALL SPECIAL_BLANK_STRIP(LINE)"   
                  LINE(I:I)=" "
              END DO
              !----------------------------------------------------------------------------------------------------------
              !
              C=INDEX(LINE,'#')-1
              !
              IF (C < Z) THEN
                  C=LEN_TRIM(LINE)      ! NO # FOUND, SO USE ENTIRE STRING
                  IF (C == Z) C=1     ! LINE IS BLANK, SET TO 1
              END IF
              IF (C == Z) C=1         !# IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
              !----------------------------------------------------------------------------------------------------------
              !
              IF(LINE(C:C) /= '#' .AND. LINE(1:C) /= " ") EXIT READ_LOOP         !Start of line is not COM and all char before COM are blank
              !
        END DO READ_LOOP
        !
      END SUBROUTINE
      !
      SUBROUTINE MOVE_TO_DATA(LINE,INFILE,ERROR)      !Moves to start of first uncommented line
        IMPLICIT NONE
        CHARACTER(*),         INTENT(INOUT):: LINE    !LINE TO LOAD DATA TOO
        INTEGER,              INTENT(IN   ):: INFILE  !UNIT OF FILE TO LOAD LINE FROM
        INTEGER,              INTENT(IN   ):: ERROR   !UNIT TO WRITE ERROR MESSAGE TOO
        !
        CALL READ_TO_DATA(LINE,INFILE,ERROR)
        !
        IF(LINE /= '') BACKSPACE(INFILE)
        !
      END SUBROUTINE
      !
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !
      !  Parse Word Routines
      !
      SUBROUTINE PARSE_WORD_UP(LN, LOC, ISTART, ISTOP)
        ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
        IMPLICIT NONE
        CHARACTER(*),      INTENT(INOUT):: LN
        INTEGER,           INTENT(INOUT):: LOC,ISTART,ISTOP
        CHARACTER(*), PARAMETER:: lowerCHAR="abcdefghijklmnopqrstuvwxyz"
        CHARACTER(*), PARAMETER:: upperCHAR="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        INTEGER:: I, N
        !
        CALL PARSE_WORD(LN, LOC, ISTART, ISTOP)
        !
        IF(ISTART <= ISTOP) THEN
                   DO I=ISTART, ISTOP
                                N = INDEX( lowerCHAR, LN(I:I))
                                IF(N > 0) LN(I:I) = upperCHAR(N:N)
                   END DO
        END IF
        !
      END SUBROUTINE
      !
      !#########################################################################################################################
      !
      SUBROUTINE PARSE_WORD(LN, LOC, ISTART, ISTOP)
        ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
        IMPLICIT NONE
        CHARACTER(*),      INTENT(IN   ):: LN
        INTEGER,           INTENT(INOUT):: LOC,ISTART,ISTOP
        INTEGER:: LINE_LEN, LINE_TRIM, LOC0
        CHARACTER:: TAB
        !
        TAB = ACHAR( 9)
        !
        LOC0 = LEN_TRIM(LN)        !Temp use in case of COM_STOP
        !
        LINE_TRIM= LOC0 + 1
        !
        LOC0 = LOC  !Make backup of Loc
        !
        DO WHILE( LOC < LINE_TRIM )
                  IF(LN(LOC:LOC).NE.TAB .AND. LN(LOC:LOC).NE.' ' .AND. 
     +                                        LN(LOC:LOC).NE.',') EXIT
                  IF(LN(LOC:LOC) == '#')  LOC = LINE_TRIM              ! Comment terminates parsing of line
                  LOC = LOC+1
        END DO
        !
        IF( LOC >= LINE_TRIM ) THEN
                   LINE_LEN = LEN(LN)
            LOC   =LINE_LEN+1
            ISTART=LINE_LEN
            ISTOP =LINE_LEN-1
        ELSE
            IF(LN(LOC:LOC)=='"') THEN
                                    LOC = LOC+1
                                    ISTART = LOC
                                    DO WHILE( LOC < LINE_TRIM )
                                        IF( LN(LOC:LOC) == '"' ) EXIT
                                        LOC = LOC+1
                                    END DO
                                    ISTOP = LOC-1
                                    LOC = LOC+1
            ELSEIF(LN(LOC:LOC)=="'") THEN
                                    LOC = LOC+1
                                    ISTART = LOC
                                    DO WHILE( LOC < LINE_TRIM )
                                        IF( LN(LOC:LOC) == "'" ) EXIT
                                        LOC = LOC+1
                                    END DO
                                    ISTOP = LOC-1
                                    LOC = LOC+1
            ELSE
                                    ISTART = LOC
                                    LOC = LOC+1
                                    DO WHILE( LOC < LINE_TRIM )
                                        IF( LN(LOC:LOC)==TAB .OR. 
     +                                      LN(LOC:LOC)==' ' .OR. 
     +                                      LN(LOC:LOC)==',' .OR.
     +                                      LN(LOC:LOC)=='#') EXIT
                                        LOC = LOC+1
                                    END DO
                                    ISTOP = LOC-1
                                    IF(ISTOP<ISTART) ISTOP=ISTART
            END IF
        END IF
        !
      END SUBROUTINE
      !
      !#########################################################################################################################
      !
      PURE SUBROUTINE GET_UNCOMMENT(LN, ISTART, ISTOP)
        !
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN ):: LN
        INTEGER,      INTENT(OUT):: ISTART, ISTOP
        !
        ISTART = 1
        ISTOP  = INDEX(LN, '#')
        !
        IF    ( ISTOP <= 0 ) THEN
                             ISTOP = MAX(ISTART, LEN_TRIM(LN))
        ELSEIF( ISTOP >  1 ) THEN
                             ISTOP  = ISTOP - 1
        ELSE!IF( ISTOP == 1 ) THEN
                             ISTART = 2
                             ISTOP  = 1
        END IF
        !
      END SUBROUTINE
      !
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !
      !  GET Routines
      !
      SUBROUTINE GET_INTEGER(LN, LOC, ISTART, ISTOP, IOUT, IN, VAL, MSG)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
        INTEGER,      INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
        INTEGER,      INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
        INTEGER,      INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
        CHARACTER(*), INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO inf_I = HUGE(VAL)
        LOGICAL:: CHECK
        INTEGER:: IERR
        CHARACTER(:), ALLOCATABLE:: ERRMSG
        CHARACTER(2):: BLN
        !
        BLN = NEW_LINE(' ')//NEW_LINE(' ')
        !
        CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
        !
        READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
        !
        IF(IERR /= 0 .OR. LN(ISTART:ISTOP)=='') THEN
          CHECK = .TRUE.
          IF (MSG=='NOSTOP') THEN
                   CHECK = .FALSE.
                   VAL = HUGE(VAL)
          END IF
          !
          IF(CHECK) THEN
            ERRMSG = 'GET_INTEGER READ UTILITY ERROR: FAILED '//
     +               'TO CONVERT TEXT TO INTEGER NUMBER.'//BLN//
     +               'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE '//
     +               'CONVERTED "'//LN(ISTART:ISTOP)//'".'
            !
            IF(LN(ISTART:ISTOP)=='') 
     +         ERRMSG = ERRMSG//BLN//
     +         'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO '//
     +         'READING IN A BLANK/EMPTY LINE OR YOU DID NOT '//
     +         'PROVIDE ENOUGH NUMBERS ON THE LINE.'
            !
            ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN '//
     +               'ADDITIONAL COMMENT PASSED TO GET_INTEGER:'//BLN//
     +               TRIM(MSG)
            !
            CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
          END IF
        END IF
        !
      END SUBROUTINE
      !
      PURE SUBROUTINE Append_1D_Alloc(IA, VAL)
      IMPLICIT NONE
      INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: IA
      INTEGER,                            INTENT(IN   ):: VAL
      INTEGER, DIMENSION(:), ALLOCATABLE:: TMP
      INTEGER:: DIM
      !
      IF(.not. ALLOCATED(IA)) THEN
          ALLOCATE(IA(1), SOURCE=VAL)
      ELSE
          DIM = SIZE(IA)
          ALLOCATE(TMP(DIM+1))
          TMP(1:DIM) = IA(1:DIM)
          TMP(DIM+1) = VAL
          DEALLOCATE(IA)
          CALL MOVE_ALLOC(TMP,IA)
      END IF
      !
      END SUBROUTINE
      !
      PURE FUNCTION INT2STR(IVAL)
        INTEGER,       INTENT(IN):: IVAL
        CHARACTER(:), ALLOCATABLE:: INT2STR
        CHARACTER(32)::NUM
        !
        WRITE(NUM,'(I32)') IVAL
        INT2STR = TRIM(ADJUSTL(NUM))
        !
      END FUNCTION
      !
      PURE SUBROUTINE SET_ARRAY_0D3D_INT(DIM1, DIM2, DIM3, VAL, ARR2)
        INTEGER,                            INTENT(IN ):: DIM1,DIM2,DIM3
        INTEGER,                            INTENT(IN ):: VAL
        INTEGER, DIMENSION(DIM1,DIM2,DIM3), INTENT(OUT):: ARR2
        INTEGER:: I,J,K
        !
        DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = VAL
        END DO
        !
      END SUBROUTINE
      !
      PURE SUBROUTINE SET_ARRAY_0D2D_INT(DIM1, DIM2, VAL, ARR2)
        INTEGER,                       INTENT(IN   ):: DIM1, DIM2
        INTEGER,                       INTENT(IN   ):: VAL
        INTEGER, DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
        INTEGER:: I,J
        !
        DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = VAL
        END DO
        !
      END SUBROUTINE