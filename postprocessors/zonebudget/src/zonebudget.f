      MODULE ZONBUDMODULE
        INTEGER IPREC
        REAL, ALLOCATABLE, DIMENSION(:,:,:) ::BUFF
        DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:,:,:) ::BUFFD
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
C     ******************************************************************
C        SPECIFICATIONS:
      USE ZONBUDMODULE
      PARAMETER (NTRDIM=250,MXCOMP=999,MXZWCZ=50,MXZONE=999)
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
      ALLOCATABLE IZONE(:,:,:),ICH(:,:,:),IBUFF(:,:,:),
     1            VBVL(:,:,:),VBZNFL(:,:,:)

      DOUBLE PRECISION VBVL,VBZNFL
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD(20),DZERO,TOTIMDOLD
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP),LSTZON(0:MXZONE)
      CHARACTER*10 NAMCOMP(MXCOMP)
      DIMENSION ITIME(2,10)
      CHARACTER*240 TITLE
      CHARACTER*240 NAME,BASENAME
      CHARACTER*16 VBNM(NTRDIM),TEXT,CTMP
      CHARACTER*1 METHOD,IANS
      CHARACTER*40 VERSON
      DIMENSION VAL(20)
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
      VERSON='ZONEBUDGET version 3.01'
C
C-----DEFINE INPUT AND OUTPUT UNITS AND INITIALIZE OTHER VARIABLES
      INZN1=10
      INZN2=11
      INBUD=12
      IOUT=0
      IUZBLST=0
      IUCSV=0
      IUCSV2=0
      K1=0
      K2=0
      MSUM=0
      DZERO=0D0
      NLIST=0
      NVAL=1
      TOTIMD=-1.0D0
      TOTIMDOLD=-1.0D0
C
C-----TELL THE USER WHAT THIS PROGRAM IS
      WRITE(*,*)
      WRITE(*,4) VERSON
4     FORMAT(1X,A/
     1' Program to compute a flow budget for subregions of a model using
     2'/' budget data from MODFLOW, the USGS Modular Ground-Water Flow M
     3odel.')
C
C-----OPEN LISTING FILE(S)
      WRITE(*,*)
7     WRITE(*,*)' Enter a LISTING FILE for results',
     1                     ' or a base name and file types:'
      READ(*,'(A)') NAME
      !NAME='Test CSV2'  !seb
      LLOC=1
      CALL URWORD(NAME,LLOC,ISTART,ISTOP,0,I,R,0,IN)
      BASENAME=NAME(ISTART:ISTOP)
      CALL URWORD(NAME,LLOC,ISTART,ISTOP,1,I,R,0,IN)
      IF(NAME(ISTART:ISTOP).NE.' ') THEN
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
        CALL URWORD(NAME,LLOC,ISTART,ISTOP,1,I,R,0,IN)
        IF(NAME(ISTART:ISTOP).NE.' ') GO TO 8
      ELSE
        IOUT=13
        IUZBLST=IOUT
        OPEN(UNIT=IOUT,FILE=BASENAME,ERR=7)
      END IF
      IF(IOUT.EQ.0) THEN
        IOUT=13
        OPEN(UNIT=IOUT,FILE=TRIM(BASENAME)//'.log',ERR=7)
        WRITE(*,*) 'Zonebudget log file: ',
     1         TRIM(BASENAME)//'.log'
      END IF
C
C-----WRITE OUTPUT FILE
      WRITE(IOUT,4) VERSON
C
C-----OPEN THE CELL-BY-CELL BUDGET FILE
      WRITE(*,*)
10    WRITE(*,*) ' Enter the name of the file containing CELL-BY-CELL BU
     1DGET TERMS:'
      READ(*,'(A)') NAME
      !NAME = '..\..\One-Water_Sim\OUT_TR\SVIHM.cbc'  !seb
      OPEN(UNIT=INBUD,FILE=NAME,STATUS='OLD',FORM=FORM,ACCESS=ACCESS,
     1               ERR=10)
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' The cell-by-cell budget file is:'
      WRITE(IOUT,*) NAME
C
C-----Check for valid budget file, and allocate memory
      CALL BUDGETPRECISION(INBUD,NCOL,NROW,NLAY)
      IF(IPREC.LT.1) THEN
        WRITE(*,*) 'Stopping because budget file is invalid'
        STOP
      ELSEIF(IPREC.EQ.1) THEN
        WRITE(IOUT,*) ' Single precision budget file'
      ELSE IF(IPREC.EQ.2) THEN
        WRITE(IOUT,*) ' Double precision budget file'
      END IF
      WRITE(*,*)
      WRITE(*,14) NLAY,NROW,NCOL
      WRITE(IOUT,14) NLAY,NROW,NCOL
14    FORMAT(1X,I10,' layers',I10,' rows',I10,' columns')
      ALLOCATE (IZONE(NCOL,NROW,NLAY))
      ALLOCATE (ICH(NCOL,NROW,NLAY))
      ALLOCATE (IBUFF(NCOL,NROW,NLAY))
C
C-----READ A TITLE TO BE PRINTED IN THE LISTING
      WRITE(*,*)
      WRITE(*,*) ' Enter a TITLE to be printed in the listing:'
      READ(*,'(A)') TITLE
      !TITLE = 'A Tittle'  !seb
      WRITE(IOUT,*)
      WRITE(IOUT,'(1X,A)') TITLE
C
C-----OPEN THE ZONE FILE IF IT EXISTS
16    WRITE(*,*)
      WRITE(*,*) ' Enter the name of your ZONE INPUT FILE (CR for intera
     1ctive):'
      READ(*,'(A)') NAME
      !NAME ='..\6__WaterBudget\ZoneBudget\wbs_31zones.in'  !seb
C
C-----IF NAME IS BLANK, INPUT ZONES INTERACTIVELY BY BLOCK
      IF(NAME.EQ.' ') THEN
         CALL BLOCK(IZONE,NLAY,NROW,NCOL,IOUT)
         NCOMP=0
      ELSE
C
C-----WHEN NAME IS NOT BLANK, OPEN ZONE FILE, AND CHECK GRID DIMENSIONS
         OPEN(UNIT=INZN1,FILE=NAME,STATUS='OLD',ERR=16)
         WRITE(IOUT,*)
         WRITE(IOUT,*) ' The zone file is:'
         WRITE(IOUT,*) NAME
         READ(INZN1,*) NL,NR,NC
         IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) THEN
            WRITE(*,*) 'MISMATCH BETWEEN DIMENSIONS OF CELL-BY-CELL DATA
     1 AND ZONE DATA:'
            WRITE(*,*) 'LAYERS, ROWS, COLUMNS IN ZONE DATA:',NL,NR,NC
            WRITE(*,*) 'LAYERS, ROWS, COLUMNS IN CELL-BY-CELL FILE:',
     1                  NLAY,NROW,NCOL
            STOP
         END IF
C
C-----READ ZONE ARRAY
         CALL IZREAD(IZONE,NLAY,NROW,NCOL,NZDIM,INZN1,INZN2,IOUT)
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
50    WRITE(*,*)
      WRITE(*,*) ' Choose the option for specifying when budgets are cal
     1culated:'
      WRITE(*,*) ' A = ALL times stored in the budget file.'
      WRITE(*,*) ' P = For each time stored in the budget file, PROMPT u
     1ser.'
      WRITE(*,*) ' L = Enter a LIST of times.'
      READ(*,'(A)') METHOD
      !METHOD = 'A'  !seb
      IF(METHOD.EQ.'A' .OR. METHOD.EQ.'a') THEN
         METHOD='A'
      ELSE IF(METHOD.EQ.'P' .OR. METHOD.EQ.'p') THEN
         METHOD='P'
      ELSE IF(METHOD.EQ.'L' .OR. METHOD.EQ.'l') THEN
         METHOD='L'
         DO 60 I=1,10
         WRITE(*,*) ' Enter a time step, stress period at which to calcu
     1late budgets (0,0=done):'
         READ(*,*) ITIME(1,I),ITIME(2,I)
         IF(ITIME(1,I).EQ.0 .AND. ITIME(2,I).EQ.0) GO TO 65
60       CONTINUE
         I=11
65       NTIMES=I-1
      ELSE
         WRITE(*,*) 'Invalid choice; you must enter "A", "P", or "L"'
         GO TO 50
      END IF
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
           READ(INBUD) ITYPE,DELT,PERTIM,TOTIM
           DELTD=DELT
           PERTIMD=PERTIM
           TOTIMD=TOTIM
         ELSE
           READ(INBUD) ITYPE,DELTD,PERTIMD,TOTIMD
         END IF
         NVAL=1
         IF(ITYPE.EQ.5) THEN
            READ(INBUD) NVAL
            IF(NVAL.GT.1) THEN
               DO 101 N=2,NVAL
               READ(INBUD) CTMP
101            CONTINUE
            END IF
         END IF
         IF(ITYPE.EQ. 2 .OR. ITYPE.EQ.5) READ(INBUD) NLIST
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
           READ(INBUD) BUFF
           BUFFD=BUFF
         ELSE
           READ(INBUD) BUFFD
         END IF
      ELSE IF(ITYPE.EQ.3) THEN
C  1-LAYER ARRAY WITH LAYER INDICATOR ARRAY
         BUFFD=DZERO
         READ(INBUD) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IF(IPREC.EQ.1) THEN
           READ(INBUD) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
           DO 265 I=1,NROW
           DO 265 J=1,NCOL
           BUFFD(J,I,1)=BUFF(J,I,1)
265        CONTINUE
         ELSE
           READ(INBUD) ((BUFFD(J,I,1),J=1,NCOL),I=1,NROW)
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
           READ(INBUD) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
           DO 275 I=1,NROW
           DO 275 J=1,NCOL
           BUFFD(J,I,1)=BUFF(J,I,1)
275        CONTINUE
         ELSE
           READ(INBUD) ((BUFFD(J,I,1),J=1,NCOL),I=1,NROW)
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
           READ(INBUD) LOC,(VAL(I),I=1,NVAL)
         ELSE
           READ(INBUD) LOC,(VALD(I),I=1,NVAL)
         END IF
300      CONTINUE
      END IF
C
C-----BEFORE PROCESSING A BUDGET TERM, CHECK IF THERE IS ENOUGH SPACE
      IF(MSUM.GT.NTRDIM) THEN
         WRITE(*,*) 'PROGRAM PARAMETER NTRDIM IS TOO SMALL'
         WRITE(*,*) 'PARAMETER NTRDIM IS CURRENTLY',NTRDIM
         WRITE(*,*) 'CHANGE NTRDIM TO BE EQUAL TO THE MAXIMUM NUMBER OF
     1BUDGET TERMS'
         STOP
      END IF
C
C-----PROCESS A BUDGET TERM AND THEN START THE READ PROCESS OVER
      IF(ICALC.NE.0) CALL ACCM(IZONE,ICH,NCOL,NROW,NLAY,VBNM,VBVL,
     1           VBZNFL,MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2           ITYPE,NLIST,INBUD,NVAL)
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
      STOP
C
C-----EMPTY BUDGET FILE
2000  WRITE(*,*) 'CELL-BY-CELL FLOW TERM FILE WAS EMPTY'
      STOP
C
      END
      SUBROUTINE IZREAD(IZONE,NLAY,NROW,NCOL,NZDIM,INZN1,INZN2,IOUT)
C     ******************************************************************
C     ROUTINE TO INPUT 3-D ZONE MATRIX, IZONE
C       INZN1 IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C        SPECIFICATIONS:
      DIMENSION IZONE(NCOL,NROW,NLAY)
      CHARACTER*20 FMTIN
      CHARACTER*80 NAME,NAMPRV,LINE
      CHARACTER*10 LOCAT
C     ------------------------------------------------------------------
      NAMPRV=' '
      DO 1000 K=1,NLAY
C
C-----READ ARRAY CONTROL RECORD.
      READ(INZN1,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,0,INZN1)
      LOCAT=LINE(ISTART:ISTOP)
C
C-----USE LOCAT TO SEE WHERE ARRAY VALUES COME FROM.
      IF(LOCAT.NE.'CONSTANT') GO TO 90
C
C-----LOCAT='CONSTANT' -- SET ALL ARRAY VALUES EQUAL TO ICONST.
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICONST,R,0,INZN1)
      DO 80 I=1,NROW
      DO 80 J=1,NCOL
80    IZONE(J,I,K)=ICONST
      WRITE(IOUT,*)
      WRITE(IOUT,83) ICONST,K
83    FORMAT(13X,'Zone Array =',I4,' for layer',I4)
      IF(ICONST.LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE NUMBER IS NOT ALLOWED'
         STOP
      END IF
      GO TO 1000
C
C-----Get FMTIN and IPRN -- there may be an unused value for ICONST
C-----in columns 11-20 if the file is an old file.
90    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,I,R,0,INZN1)
      IF(LINE(ISTART:ISTART).NE.'(' ) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,I,R,0,INZN1)
        IF(LINE(ISTART:ISTART).NE.'(' ) THEN
          WRITE(*,91) LINE
91        FORMAT(1X,
     1   'Format for reading zone array does not contain "(":',/1X,A)
          STOP
        END IF
      END IF
      FMTIN=LINE(ISTART:ISTOP)
C-----Blank inside parentheses indicates free format
      NC=ISTOP-ISTART-1
      IF(NC.LE.0) THEN
         FMTIN=' '
      ELSE
        IF(LINE(ISTART+1:ISTOP-1).EQ.' ') FMTIN=' '
      END IF
C-----Get print flag
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPRN,R,0,INZN1)
C
C-----LOCAT SHOULD BE EITHER 'EXTERNAL' OR 'INTERNAL'
C-----IF 'INTERNAL', READ ARRAY FROM SAME FILE
      IF(LOCAT.EQ.'INTERNAL') THEN
         INUNIT=INZN1
         WRITE(IOUT,*)
         WRITE(IOUT,92) K
92       FORMAT(1X,'Zone Array for layer',I4,
     1      ' will be read from the Zone File')
C
C-----IF 'EXTERNAL', OPEN A SEPARATE FILE
      ELSE IF(LOCAT.EQ.'EXTERNAL') THEN
         READ(INZN1,'(A)') NAME
         INUNIT=INZN2
         WRITE(IOUT,*)
         WRITE(IOUT,93) K,NAME
93       FORMAT(1X,'Zone Array for layer',I4,
     1         ' will be read from file:'/1X,A)
         IF(NAME.NE.NAMPRV) THEN
            IF(NAMPRV.NE.' ') CLOSE(UNIT=INUNIT)
            OPEN(UNIT=INUNIT,FILE=NAME,STATUS='OLD')
            WRITE(IOUT,96)
96          FORMAT(1X,'The file was opened successfully.')
            NAMPRV=NAME
         ELSE
            WRITE(IOUT,97)
97          FORMAT(1X,'This file is already open -- will continue readi
     1ng from the current location.')
         END IF
C
C-----LOCAT IS INVALID
      ELSE
         WRITE(*,*) ' INVALID LOCAT IN ARRAY CONTROL RECORD:',LOCAT
         STOP
      END IF
C
C-----LOCAT>0 -- READ RECORDS USING FREE-FORMAT OR FMTIN.
      IF(FMTIN.EQ.' ') THEN
         WRITE(IOUT,98) K
98       FORMAT(1X,'Zone Array for layer',I4,
     1       ' will be read using free format.'/1X,55('-'))
         DO 100 I=1,NROW
         READ(INUNIT,*) (IZONE(J,I,K),J=1,NCOL)
100      CONTINUE
      ELSE
         WRITE(IOUT,104) K,FMTIN
104      FORMAT(1X,'Zone Array for layer',I4,
     1       ' will be read using format: ',A/1X,71('-'))
         DO 110 I=1,NROW
         READ (INUNIT,FMTIN) (IZONE(J,I,K),J=1,NCOL)
110      CONTINUE
      END IF
C
C-----CHECK FOR NEGATIVE IZONE VALUES
320   DO 400 I=1,NROW
      DO 400 J=1,NCOL
      IF(IZONE(J,I,K).LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE AT (LAYER,ROW,COLUMN):',K,I,J
         STOP
      END IF
400   CONTINUE
C
C-----IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.
      IF(IPRN.LT.0) GO TO 1000
C
C-----PRINT COLUMN NUMBERS AT THE TOP OF THE PAGE
      WRITE(IOUT,421) (I,I=1,NCOL)
421   FORMAT(/,(5X,25I3))
      WRITE(IOUT,422)
422   FORMAT(1X,79('-'))
C
C-----PRINT EACH ROW IN THE ARRAY.
      DO 430 I=1,NROW
      WRITE(IOUT,423) I,(IZONE(J,I,K),J=1,NCOL)
423   FORMAT(1X,I3,1X,25I3/(5X,25I3))
430   CONTINUE
C
1000  CONTINUE
      IF(NAMPRV.NE.' ') CLOSE(UNIT=INZN2)
C
C-----RETURN
      RETURN
      END
      SUBROUTINE BLOCK(IZONE,NLAY,NROW,NCOL,IOUT)
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
      END
      SUBROUTINE ZONCOUNT(NZDIM,LSTZON,MXZONE,IZONE,NLAY,NROW,NCOL,IOUT)
C     ******************************************************************
C     Create Zone list
C     ******************************************************************
      DIMENSION LSTZON(0:MXZONE),IZONE(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
      LSTZON(0)=-1
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
      WRITE(*,*)
      WRITE(*,*) NZDIM,' zones.'
      WRITE(IOUT,*) NZDIM,' zones.'
      IF(NZDIM.EQ.0) THEN
         WRITE(*,*) ' Stopping because there are no zones'
         STOP
      END IF
      WRITE(*,195) (LSTZON(M),M=1,NZDIM)
195   FORMAT(20I5)
      WRITE(IOUT,195) (LSTZON(M),M=1,NZDIM)
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
      END
      SUBROUTINE INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,
     1                  LSTZON,NZDIM,IOUT,NAMCOMP)
C     ******************************************************************
C     READ COMPOSITE ZONES
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP),LSTZON(0:NZDIM)
      CHARACTER*1000 LINE
      CHARACTER*10 NAMCOMP(MXCOMP)
C     ------------------------------------------------------------------
C
C-----READ THE COMPOSITE ZONES
      DO 10 I=1,MXCOMP
      READ(INZN1,'(A)',END=20) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,INZN1)
      IF(LINE(ISTART:ISTART).GE.'0' .AND.
     1              LINE(ISTART:ISTART).LE.'9') THEN
        NAMCOMP(I)=' '
        WRITE(NAMCOMP(I),2) I
2       FORMAT('CZ',I3.3)
        LLOC=1
      ELSE
        NAMCOMP(I)=LINE(ISTART:ISTOP)
      END IF
      DO 3 J=1,MXZWCZ
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICOMP(J,I),RDUM,IOUT,INZN1)
      IF(ICOMP(J,I).LE.0) GO TO 10
3     CONTINUE
10    CONTINUE
      I=MXCOMP+1
20    NCOMP=I-1
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
52    FORMAT(1X,I3,' Composite Zones:')
      DO 60 I=1,NCOMP
      WRITE(IOUT,54) NAMCOMP(I),(LSTZON(ICOMP(J,I)),J=1,NZWCZ(I))
54    FORMAT(1X,'Composite Zone ',A,':',15I4/(27X,15I4))
60    CONTINUE
C
      RETURN
      END
      SUBROUTINE ACCM(IZONE,ICH,NCOL,NROW,NLAY,VBNM,VBVL,VBZNFL,
     1                MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2                ITYPE,NLIST,INBUD,NVAL)
C     ******************************************************************
C     ACCUMULATE VOLUMETRIC BUDGET FOR ZONES
C     ******************************************************************
      USE ZONBUDMODULE
      DIMENSION VBVL(2,NTRDIM,NZDIM),
     1  VBZNFL(2,0:NZDIM,0:NZDIM),IZONE(NCOL,NROW,NLAY),
     2  ICH(NCOL,NROW,NLAY)
      DOUBLE PRECISION VBVL,VBZNFL,DBUFF
      CHARACTER*16 VBNM(NTRDIM),TEXT
      DIMENSION VAL(20)
      DOUBLE PRECISION VALD(20),DZERO
C     ------------------------------------------------------------------
      DZERO=0D0
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
              READ(INBUD) ICELL,(VAL(I),I=1,NVAL)
              DO 45 I=1,NVAL
              VALD(I)=VAL(I)
45            CONTINUE
            ELSE
              READ(INBUD) ICELL,(VALD(I),I=1,NVAL)
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
              READ(INBUD) ICELL,(VAL(I),I=1,NVAL)
            ELSE
              READ(INBUD) ICELL,(VALD(I),I=1,NVAL)
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
      END
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
      END
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
      END
      SUBROUTINE BUDGETPRECISION(IU,NCOL,NROW,NLAY)
C     ******************************************************************
C     Determine single or double precision file type for a MODFLOW
C     budget file:  0=unrecognized, 1=single, 2=double.
C     ******************************************************************
      USE ZONBUDMODULE
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD
      CHARACTER*16 TEXT1,TEXT2
C
C  Default is unrecognized file
      IPREC=0
C
C  SINGLE check
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NCOL,NROW,NLAY
      ICODE=0
      IF(NLAY.LT.0) THEN
        NLAY=-NLAY
        READ(IU,ERR=50,END=50) ICODE,DELT,PERTIM,TOTIM
      END IF
14    FORMAT(1X,I10,' layers',I10,' rows',I10,' columns')
      IF(NCOL.LT.1 .OR. NROW.LT.1 .OR. NLAY.LT.1) GO TO 100
      IF(NCOL.GT.100000000 .OR.NROW.GT.100000000 .OR.
     1                     NLAY.GT.100000000) GO TO 100
      IF(NCOL*NROW.GT.100000000 .OR. NCOL*NLAY.GT.100000000 .OR.
     1                 NROW*NLAY.GT.100000000) GO TO 100
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
      ALLOCATE (BUFFD(NCOL,NROW,NLAY))
      NODES=NCOL*NROW*NLAY
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
      IF(TEXT1.EQ.'         STORAGE' .AND.
     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
           IPREC=1
           GO TO 100
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
           IPREC=1
           GO TO 100
      END IF
C
C  DOUBLE check
50    REWIND(IU)
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NC,NR,NL
      ICODE=0
      IF(NL.LT.0) THEN
        NL=-NL
        READ(IU,ERR=100,END=100) ICODE,DELTD,PERTIMD,TOTIMD
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
      IF(TEXT1.EQ.'         STORAGE' .AND.
     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
           IPREC=2
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
           IPREC=2
      END IF
C
100   REWIND(IU)
      RETURN
      END
      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
C     ******************************************************************
C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
C     CONVERT THE WORD TO A NUMBER.
C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
C          ENDING CHARACTER POSITIONS OF THE WORD.
C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
C          A QUOTE CHARACTER.
C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
      CHARACTER*20 STRING
      CHARACTER*30 RW
      CHARACTER*1 TAB
C     ------------------------------------------------------------------
      TAB=CHAR(9)
C
C1------Set last char in LINE to blank and set ISTART and ISTOP to point
C1------to this blank as a default situation when no word is found.  If
C1------starting location in LINE is out of bounds, do not look for a
C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
C
C2------Find start of word, which is indicated by first character that
C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.','
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
C
C3------Found start of word.  Look for end.
C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
C
C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.','
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
C
C3C-----End of line without finding end of word; set end of word to
C3C-----end of line.
      J=LINLEN+1
C
C4------Found end of word; set J to point to last character in WORD and
C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
C
C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')
     1             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
C
C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
      END IF
      RETURN
C
C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
C
C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
C
C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
C
C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
C
C7D-----STOP after writing message.
      STOP
      END
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
      END
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
        FIELD(NFIELD)='TOTIM'
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
        WRITE(IUCSV,7) (TRIM(FIELD(I)),I=1,NFIELD)
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
      END
