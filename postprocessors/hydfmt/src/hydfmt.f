CC
      PROGRAM HYDFMT
CC
C----- Reads unformatted data from the HYDMOD package and writes out
C----- columns as formatted data to selected files by topic. The files
C----- have extensions that represent the type of data they contain.
C----- The three-letter extensions are as follows:
C-----      TYPE OF DATA                                      OUTPUT FILE
C-----                                                          EXTENSION
C----- (1)  ground-water levels (head)                              .gwh
C----- (2)  ground-water level decline (drawdown)                   .gwd
C----- (3)  ground-water preconsolidation level (critical head)     .crh
C----- (4)  layer(system)-specific total compaction (compaction)    .cmp
C----- (5)  sum of all layer(system)total compaction(subsidence)    .sub
C----- (6)  surface-water streamflow water level (stage)            .sth
C----- (7)  surface-water streamflow into a reach (inflow)          .inf
C----- (8)  surface-water streamflow out of a reach (inflow)        .otf
C----- (9)  flow between ground-water & surface-water (leakage)     .gsf
C----- (10) sum of all layer inelastic compaction (subsidence)      .sbv
C----- (11) sum of all layer elastic compaction (subsidence)        .sbe
C----- (12) layer(system)-specific inelastic compaction(compaction) .cpv
C----- (13) layer(system)-specific elastic compaction(compaction)   .cpe
C-----
C----- R.T. Hanson, USGS-WRD, San Diego, California, rthanson@usgs.gov
C----- S.A. Leake, USGS-WRD, Tucson, Arizona, saleake@usgs.gov
C-----
C----- VERSION 0101 16OCTOBER2009 HYDFMT
C=============================================================================
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      IMPLICIT REAL(REAL64) (A-H,O-Z)
      !
      CHARACTER, PARAMETER:: blank = ' '
      CHARACTER, PARAMETER:: NL = NEW_LINE(blank)
      !
      CHARACTER(2048):: FTMP
      CHARACTER( 256):: CTMP
      CHARACTER(  96):: FMT1
      CHARACTER(  32):: fmtout, fmtoute, fmtoutd
      CHARACTER(   4):: timlbl
      CHARACTER(   2):: arr
      CHARACTER(   1):: TIMTYP, ELPTYP, SITTYP, LEAPYR
      CHARACTER(:), ALLOCATABLE:: FN1, FN2, FBASE
      !
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLID
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDH
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDD
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDCH
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDC
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDS
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDFI
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDHF
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDFO
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDFA
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDSV
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDSE
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDCV
      CHARACTER(20), DIMENSION(:), ALLOCATABLE:: WELLIDCE
      !
      INTEGER, DIMENSION(:), ALLOCATABLE:: id
      !
      INTEGER, DIMENSION(13):: nu
      INTEGER, DIMENSION(13):: icnt
      !
      REAL(REAL32), DIMENSION(:), ALLOCATABLE:: Z
      REAL(REAL32):: TIME
      !
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: DZ
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zh
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zhd
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zhph
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zhc
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zhs
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zfi
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zfo
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zhf
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zfa
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zsv
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zse
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zcv
      REAL(REAL64), DIMENSION(:), ALLOCATABLE:: zce
      !
      REAL(REAL64):: DTIME, DTIME_OLD
      REAL(REAL64):: YR
      !
      !PARAMETER(nmt=500000)
      INTEGER:: IDATE, IERR, P
      INTEGER ISTRNG
      LOGICAL FIRST
C     + + + INCLUDE STATEMENTS + + +
      INCLUDE 'openspec.inc'
      INTERFACE
        INTEGER FUNCTION DAYS(DAT, LEAPYR)
          IMPORT, ONLY: REAL64
          INTEGER,      INTENT(IN):: DAT
          CHARACTER(1), INTENT(IN):: LEAPYR
        END FUNCTION
        !
        PURE SUBROUTINE UPPER(LN)
          CHARACTER(*), INTENT(INOUT):: LN
        END SUBROUTINE
        !
        SUBROUTINE ERROR(MSG)
          CHARACTER(*), INTENT(IN):: MSG
        END SUBROUTINE
        !
        PURE SUBROUTINE ALLOC_CHAR20(VAR, DIM)
          CHARACTER(20), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VAR
          INTEGER,                                  INTENT(IN   ):: DIM
        END SUBROUTINE
        !
        PURE SUBROUTINE ALLOC_DBL(VAR, DIM)
          IMPORT, ONLY: REAL64
          REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VAR
          INTEGER,                                 INTENT(IN   ):: DIM
        END SUBROUTINE
        !
        SUBROUTINE OPEN_OUTPUT(BASE, EXT, IU, timlbl, num, wellid)
          IMPORT, ONLY: REAL64
          CHARACTER(*),                  INTENT(IN   ):: BASE, EXT  ! Base must be the exact size
          INTEGER,                       INTENT(INOUT):: IU
          CHARACTER(4),                  INTENT(IN   ):: timlbl
          INTEGER,                       INTENT(IN   ):: num
          CHARACTER(20), DIMENSION(num), INTENT(IN   ):: wellid
        END SUBROUTINE
        !
        PURE SUBROUTINE sng_to_dbl(dim, sng, dbl)
          IMPORT, ONLY: REAL32, REAL64
          INTEGER,                      INTENT(IN ):: dim
          REAL(REAL32), DIMENSION(dim), INTENT(IN ):: sng
          REAL(REAL64), DIMENSION(dim), INTENT(OUT):: dbl
        END SUBROUTINE
      END INTERFACE
      !
      WRITE(*,'(/,A,/,/)') ' HYDFMT version 1.2'
      !
      WRITE (*,'(A)')' For postprocessing the HYD package output from:'
      WRITE (*,'(A)')
      WRITE (*,'(A)')' MODFLOW ONE-WATER HYDROLOGIC-FLOW MODEL '//
     +                '(MF-OWHM) Version 2'
      !
      WRITE (*,'(A)')
      WRITE (*,'(*(A, /))') REPEAT('-',84),                          
     + 'USGS Software Disclaimer:                         '//
     +                          '                                 |', 
     + '                                                  '//
     +                          '                                 |', 
     + '   No warranty, expressed or implied, is made by t'//
     +                          'he USGS or the U.S. Government   |', 
     + '   as to the functionality of the software and rel'//
     +                          'ated material nor shall the      |', 
     + '   fact of release constitute any such warranty.  '//
     +                          '                                 |', 
     + '                                                  '//
     +                          '                                 |', 
     + '   The software is provided on the condition that '//
     +                          'neither the USGS nor             |', 
     + '   the U.S. Government shall be held liable for an'//
     +                          'y damages resulting from the     |', 
     + '   authorized or unauthorized use of the software.'//
     +                          '                                 |', 
     + '                                                  '//
     +                          '                                 |', 
     + '   Newer versions may be present at the official c'//
     +                          'ode and download repository:     |', 
     + '      https://code.usgs.gov/modflow/mf-owhm       '//
     +                          '                                 |', 
     + '   in the postprocessors/hydfmt/bin directory.    '//
     +                          '                                 |',  
     + '                                                  '//
     +                          '                                 |', 
     + REPEAT('-',84)
      WRITE (*,'(A)')
      WRITE (*,'(A)')
      !
      FIRST = .TRUE.
CC
      icnt=0
      itype=13
      LEAPYR='L'
      fmt1=blank
      fmtout='(i4,1x,a4,12x,*(1x,a20))'
      fmtoute='(f21.3,*(ES21.10))'
      fmtoutd='(1x,i8,3x,*(ES21.10))'
      TIMTYP='D'
      ELPTYP='Y'
      NDATE=1
      START=0.0
      numhh=0
      numhd=0
      numch=0
      numhc=0
      numhs=0
      numhfi=0
      numhfo=0
      numhfa=0
      numhhf=0
      numhsv=0
      numhse=0
      numhcv=0
      numhce=0
      LEN=0
      ISTRNG=2048
      FTMP=blank
CC
C----- Read file names
      WRITE(*,'(A, /)') 
     +            'Enter name of file with unformatted hydrograph data:'
      READ(*,'(A)') FTMP
      IF(FTMP == blank) THEN
        CALL ERROR('Failed to answer command prompt question:'//NL//
     +       '"Enter name of file with unformatted hydrograph data:"')
      END IF
      FN1 = TRIM(ADJUSTL(FTMP))
      OPEN(NEWUNIT=inunit, FILE=FN1, FORM=FORM, ACCESS=ACCESS, 
     +     STATUS='OLD', IOSTAT=IERR)
      !
      IF(IERR /= 0) THEN
        CALL ERROR('Failed to open unformatted hydrograph file.'//NL//
     +  'File may not exist of the path to the file is incorrect.'//NL//
     +  'Input specified the following file:'//NL//
     +  '"'//FN1//'"')
      END IF
CC
C----- Read first unformatted header record with number of hydrographs saved.
C----- Negative number indicates double precision
      READ(inunit, IOSTAT=IERR) NUMH,ITMUNI
      !
      IF(IERR /= 0) THEN
        CALL ERROR(
     +  'The unformatted hydrograph file was successfully opened'//NL//
     +  '  but failed to read the first record in the file.'//NL//NL//
     +  'Is the file bad/corrupt?'//NL//NL//
     +  'The following file is what was attempted to read from:'//NL//
     +  '"'//FN1//'"')
      END IF
      !
      IF(NUMH.LT.0) THEN
        NUMH=-NUMH
        IPREC=2
        WRITE(*,'(/, 5x, A, /, /)')
     +          '-> Double precision unformatted hydrograph file.'
      ELSE
        IPREC=1
        WRITE(*,'(/, 5x, A, /, /)')
     +          '-> Single precision unformatted hydrograph file.'
      END IF
      !
      ALLOCATE(WELLID(NUMH))
      ALLOCATE(    ID(NUMH), SOURCE=0)
      !
      ALLOCATE(Z   (NUMH))
      ALLOCATE(DZ  (NUMH))
      !
C----- Read second unformatted header record with labels of hydrographs saved
      READ(inunit, IOSTAT=IERR) timlbl, WELLID ! (wellid(n)(1:20),n=1,numh)
      !
      IF(IERR /= 0) THEN
        CALL ERROR(
     +  'The unformatted hydrograph file was successfully opened'//NL//
     +  '  but failed to read the second record in the file.'//NL//NL//
     +  'Is the file bad/corrupt?'//NL//NL//
     +  'The following file is what was attempted to read from:'//NL//
     +  '"'//FN1//'"')
      END IF
      !
      DO n = 1, numh
          wellid(n) = ADJUSTL(wellid(n))
      END DO
      !
      numhh  = 0
      numhd  = 0
      numch  = 0
      numhc  = 0
      numhs  = 0
      numhhf = 0
      numhfi = 0
      numhfo = 0
      numhfa = 0
      numhsv = 0
      numhse = 0
      numhcv = 0
      numhce = 0
      !
      do i=1,numh
        SELECT CASE (wellid(i)(1:2))
        CASE('HD')
                  numhh=numhh+1
        CASE('DD')
                  numhd=numhd+1
        CASE('HC')
                  numch=numch+1
        CASE('CP')
                  numhc=numhc+1
        CASE('SB')
                  numhs=numhs+1
        CASE('ST')
                  numhhf=numhhf+1
        CASE('SI')
                  numhfi=numhfi+1
        CASE('SO')
                  numhfo=numhfo+1
        CASE('SA')
                  numhfa=numhfa+1
        CASE('SV')
                  numhsv=numhsv+1
        CASE('SE')
                  numhse=numhse+1
        CASE('CV')
                  numhcv=numhcv+1
        CASE('CE')
                  numhce=numhce+1
        CASE DEFAULT
                  CALL ERROR(
     +    'Unknown wellid encountered in the '//
     +    'unformatted hydrograph file.'//NL//NL//
     +    'Is the file bad/corrupt?'//NL//NL//
     +    'The following is the wellid read in:'//NL//
     +    '"'//wellid(i)//'"'//NL//NL//
     +    'The first two characters in the wellid name should be one '//
     +    'of the following:'//NL//NL//
     +    'HD, DD, HC, CP, SB, ST, SI, SO, SA, SV, SE, CV, CE'//NL//NL//
     +    'but instead it has: "'//wellid(i)(1:2)//'"')
        END SELECT
      END DO
      !
      CALL ALLOC_CHAR20(wellidh , numhh )
      CALL ALLOC_CHAR20(wellidd , numhd )
      CALL ALLOC_CHAR20(wellidch, numch )
      CALL ALLOC_CHAR20(wellidc , numhc )
      CALL ALLOC_CHAR20(wellids , numhs )
      CALL ALLOC_CHAR20(wellidhf, numhhf)
      CALL ALLOC_CHAR20(wellidfi, numhfi)
      CALL ALLOC_CHAR20(wellidfo, numhfo)
      CALL ALLOC_CHAR20(wellidfa, numhfa)
      CALL ALLOC_CHAR20(wellidsv, numhsv)
      CALL ALLOC_CHAR20(wellidse, numhse)
      CALL ALLOC_CHAR20(wellidcv, numhcv)
      CALL ALLOC_CHAR20(wellidce, numhce)
      !
      CALL ALLOC_DBL(zh  , numhh)
      CALL ALLOC_DBL(zhd , numhd)
      CALL ALLOC_DBL(zhph, numch)
      CALL ALLOC_DBL(zhc , numhc)
      CALL ALLOC_DBL(zhs , numhs)
      CALL ALLOC_DBL(zhf , numhhf)
      CALL ALLOC_DBL(zfi , numhfi)
      CALL ALLOC_DBL(zfo , numhfo)
      CALL ALLOC_DBL(zfa , numhfa)
      CALL ALLOC_DBL(zsv , numhsv)
      CALL ALLOC_DBL(zse , numhse)
      CALL ALLOC_DBL(zcv , numhcv)
      CALL ALLOC_DBL(zce , numhce)
      !
      numhh  = 0
      numhd  = 0
      numch  = 0
      numhc  = 0
      numhs  = 0
      numhhf = 0
      numhfi = 0
      numhfo = 0
      numhfa = 0
      numhsv = 0
      numhse = 0
      numhcv = 0
      numhce = 0
      !
      do i=1,numh
        !
        SELECT CASE (wellid(i)(1:2))
        CASE('HD')
                  id(i)=1
                  numhh=numhh+1
                  wellidh(numhh)=wellid(i)
        CASE('DD')
                  id(i)=2
                  numhd=numhd+1
                  wellidd(numhd)=wellid(i)
        CASE('HC')
                  id(i)=3
                  numch=numch+1
                  wellidch(numch)=wellid(i)
        CASE('CP')
                  id(i)=4
                  numhc=numhc+1
                  wellidc(numhc)=wellid(i)
        CASE('SB')
                  id(i)=5
                  numhs=numhs+1
                  wellids(numhs)=wellid(i)
        CASE('ST')
                  id(i)=6
                  numhhf=numhhf+1
                  wellidhf(numhhf)=wellid(i)
        CASE('SI')
                  id(i)=7
                  numhfi=numhfi+1
                  wellidfi(numhfi)=wellid(i)
        CASE('SO')
                  id(i)=8
                  numhfo=numhfo+1
                  wellidfo(numhfo)=wellid(i)
        CASE('SA')
                  id(i)=9
                  numhfa=numhfa+1
                  wellidfa(numhfa)=wellid(i)
        CASE('SV')
                  id(i)=10
                  numhsv=numhsv+1
                  wellidsv(numhsv)=wellid(i)
        CASE('SE')
                  id(i)=11
                  numhse=numhse+1
                  wellidse(numhse)=wellid(i)
        CASE('CV')
                  id(i)=12
                  numhcv=numhcv+1
                  wellidcv(numhcv)=wellid(i)
        CASE('CE')
                  id(i)=13
                  numhce=numhce+1
                  wellidce(numhce)=wellid(i)
        CASE DEFAULT
                  id(i) = 0
        END SELECT
      END DO
      !
      WRITE(*,'(2A, /)') 'Enter root-name of file for formatted output',
     +                ' (ex. hydro):'
      READ(*,'(A)', IOSTAT=IERR) FTMP
      !
      IF(IERR /= 0 .OR. FTMP == '') THEN
        CALL ERROR('Failed to answer command prompt question:'//NL//
     +       '"Enter name of file with unformatted hydrograph data:"')
      END IF
      !
      FBASE = TRIM(ADJUSTL(FTMP))
      !
      WRITE(*,'(/, 2A)')'Do you want Decimal or Calendar(date) ',
     +               'time format(D/C): '
      !
      READ(*,'(A)', IOSTAT=IERR) CTMP
      !
      IF(IERR /= 0) CTMP = blank
      CALL UPPER(CTMP)
      TIMTYP = ADJUSTL(CTMP)
      !
      SELECT CASE(TIMTYP)
      CASE('C')
           WRITE(*,'(/, 2A)')
     +        'Enter initial date of transient simulation ',
     +        '(YYYYMMDD ex. 19920827): '
           READ(*,*, IOSTAT=IERR) ISTRT
           !
           IF(IERR /= 0) THEN
            CALL ERROR('Failed to answer command prompt question:'//NL//
     +      '"Enter initial date of transient simulation (YYYYMMDD):"')
           END IF
           !
           ISTT=(ISTRT/1000000)
           IST=ISTT*1000000
           ISTART=ISTRT-IST
           IDAYS=DAYS(ISTART, LEAPYR)
           !
      CASE('D')
           WRITE(*,'(/, 2A)')
     +        'Enter unit decimal time as years, days, ',
     +        'or minutes (Y,D,M) for output: '
           READ(*,'(A)', IOSTAT=IERR) CTMP
           IF(IERR /= 0) CTMP = blank
           CALL UPPER(CTMP)
           ELPTYP = ADJUSTL(CTMP)
           !
           WRITE(*,'(/, 2A)')'Enter initial decimal time of transient',
     +     ' simulation (ex. 1891.00000):'
           READ(*,*, IOSTAT=IERR) START
           !
           IF(IERR /= 0) THEN
              CALL ERROR(
     + 'Failed to answer command prompt question correctly:'//NL//
     + '"Enter initial decimal time of transient'//
     +     ' simulation (ex. 1891.00000):"')
           END IF
           !
           IF(INDEX('YDM', ELPTYP) < 1) THEN
              CALL ERROR(
     + 'Failed to answer command prompt question correctly:'//NL//
     + '"Enter unit decimal time as years, days, '//
     +        'or minutes (Y,D,M) for output:"'//NL//
     + 'The only accepted answer is "Y", "D", or "M".'//NL//
     + 'but the following answer was given: "'//ELPTYP//'"')
           END IF
           !
      CASE DEFAULT
           CALL ERROR(
     + 'Failed to answer command prompt question correctly:'//NL//
     + '"Do you want decimal or calendar(date) time format(D/C):"'//NL//
     + 'The only accepted answer is "C" or "D".'//NL//
     + 'but the following answer was given: "'//TIMTYP//'"')
      END SELECT
      !
      WRITE(*,'(/, A, /, A, /, A, /, A)')
     +      'Do you ignore leap days,',
     +      'Distribute them uniformly over four years, or',
     +      'want the leap day inserted into the leap year?',
     +      'Enter a choice for Ignore, Uniform, or Leap (I/U/L): '
      READ(*,'(A)', IOSTAT=IERR) CTMP
      IF(IERR /= 0) CTMP = blank
      CALL UPPER(CTMP)
      LEAPYR = ADJUSTL(CTMP)
      !
      IF(INDEX('IUL', LEAPYR) < 1) THEN
         CALL ERROR(
     + 'Failed to answer command prompt question correctly:'//NL//
     + '"Enter a choice for Ignore, Uniform, or Leap (I/U/L):"'//NL//
     + 'The only accepted answer is "I", "U", or "L".'//NL//
     + 'but the following answer was given: "'//LEAPYR//'"')
      END IF
      !
      if(timtyp == 'D')then
             CALL ETIMEFMT(fmtoute,ELPTYP,ITMUNI)
             FMT1=fmtoute
      else ! if(timtyp == 'C')then
             FMT1=fmtoutd
      endif
      FMT1=ADJUSTL(FMT1)
cc
      if(numhh > 0) then
        CALL OPEN_OUTPUT(FBASE, '.gwh', nu(1), timlbl, numhh, wellidh)
      endif
      !
      if(numhd > 0) then
        CALL OPEN_OUTPUT(FBASE, '.gwd', nu(2), timlbl, numhd, wellidd)
      endif
      !
      if(numch > 0) then
        CALL OPEN_OUTPUT(FBASE, '.crh', nu(3), timlbl, numch, wellidch)
      endif
      !
      if(numhc > 0) then
        CALL OPEN_OUTPUT(FBASE, '.cmp', nu(4), timlbl, numhc, wellidc)
      endif
      !
      if(numhs > 0) then
        CALL OPEN_OUTPUT(FBASE, '.sub', nu(5), timlbl, numhs, wellids)
      endif
      !
      if(numhhf > 0) then
        CALL OPEN_OUTPUT(FBASE, '.sth', nu(6), timlbl, numhhf, wellidhf)
      endif
      !
      if(numhfi > 0) then
        CALL OPEN_OUTPUT(FBASE, '.inf', nu(7), timlbl, numhfi, wellidfi)
      endif
      !
      if(numhfo > 0) then
        CALL OPEN_OUTPUT(FBASE, '.otf', nu(8), timlbl, numhfo, wellidfo)
      endif
      !
      if(numhfa > 0) then
        CALL OPEN_OUTPUT(FBASE, '.gsf', nu(9), timlbl, numhfa, wellidfa)
      endif
      !
      if(numhsv > 0) then
        CALL OPEN_OUTPUT(FBASE, '.sbv',nu(10), timlbl, numhsv, wellidsv)
      endif
      !
      if(numhse > 0) then
        CALL OPEN_OUTPUT(FBASE, '.sbe',nu(11), timlbl, numhse, wellidse)
      endif
      !
      if(numhcv > 0) then
        CALL OPEN_OUTPUT(FBASE, '.cpv',nu(12), timlbl, numhcv, wellidcv)
      endif
      !
      if(numhce > 0) then
        CALL OPEN_OUTPUT(FBASE, '.cpe',nu(13), timlbl, numhce, wellidce)
      endif
cc
C----- Read the number of columns to be written to output file
C----- The number of columns does not include the simulation time which is
C----- always the first column in the formatted file.
      WRITE(*,'(/, A)')
     +        'Selected sites (S) not supported after version 1.2'
      WRITE(*,'(A, /)')'Only option allowed is A'
      WRITE(*,'(A)')'Do you want All or Selected sites (A/S): '
      READ(*,'(A)', IOSTAT=IERR) CTMP
      !
      IF(IERR /= 0) CTMP = blank
      CALL UPPER(CTMP)
      SITTYP = ADJUSTL(CTMP)
      !
      IF(SITTYP /= "A") THEN
         CALL ERROR(
     + 'Failed to answer command prompt question correctly:'//NL//
     + '"Do you want all or selected sites(A/S):"'//NL//
     + 'The only accepted answer is "A".'//NL//
     + 'but the following answer was given: "'//SITTYP//'"')
      END IF
cc
      WRITE(*,'(/, A)')
     +               'Enter the units conversion factor for data output'
      WRITE(*,'(A)') 'as number or press enter to skip: '
      READ(*,*,IOSTAT=IERR) DCONVRT
      IF(IERR /= 0) DCONVRT = 1.0
cc
C----- Read the unformatted and Write the formatted results
      WRITE_LOOP: DO
        IF(IPREC == 1) THEN
            READ(inunit, IOSTAT=IERR) TIME, Z
            IF(IERR == 0) THEN
                           DTIME = REAL(TIME, REAL64)
                           CALL sng_to_dbl(NUMH, Z, DZ)
            END IF
        ELSE
            READ(inunit, IOSTAT=IERR) DTIME, DZ
        END IF
        !
        IF(IERR /= 0) THEN
          IF(IERR < 0) EXIT
          CALL ERROR(
     +    'Unknown error reading the unformatted hydrograph file.'//NL//
     +    'The output results may be truncated and not '//
     +    'contain the full simulation.')
        END IF
        !
        IF(DCONVRT /= 1.0) DZ = DZ * DCONVRT
        !
C----- Convert model elapsed time into calendar time
        CALL MODTIME(ITIME,DTIME,ITMUNI,IDAYS,IDATE,ISTART,TIMTYP,
     +               ELPTYP,FIRST,START,LEAPYR)
        FIRST=.FALSE.
        IF(TIMTYP == 'C') IDATE=IDATE+IST
cc
        icnt = 0
        DO i=1, numh
         if (id(i) < 1) CYCLE
         !
         icnt(id(i)) = icnt(id(i)) + 1
         !
         p = icnt(id(i))
         SELECT CASE (id(i))
         CASE( 1); zh  (p) = DZ(i)
         CASE( 2); zhd (p) = DZ(i)
         CASE( 3); zhph(p) = DZ(i)
         CASE( 4); zhc (p) = DZ(i)
         CASE( 5); zhs (p) = DZ(i)
         CASE( 6); zhf (p) = DZ(i)
         CASE( 7); zfi (p) = DZ(i)
         CASE( 8); zfo (p) = DZ(i)
         CASE( 9); zfa (p) = DZ(i)
         CASE(10); zsv (p) = DZ(i)
         CASE(11); zse (p) = DZ(i)
         CASE(12); zcv (p) = DZ(i)
         CASE(13); zce (p) = DZ(i)
         END SELECT    
        END DO
        !
        IF (TIMTYP == 'C')THEN
         if(numhh > 0 ) WRITE(nu( 1), FMT1) IDATE, ZH  
         if(numhd > 0 ) WRITE(nu( 2), FMT1) IDATE, ZHD 
         if(numch > 0 ) WRITE(nu( 3), FMT1) IDATE, ZHPH
         if(numhc > 0 ) WRITE(nu( 4), FMT1) IDATE, ZHC
         if(numhs > 0 ) WRITE(nu( 5), FMT1) IDATE, ZHS
         if(numhhf > 0) WRITE(nu( 6), FMT1) IDATE, ZHF
         if(numhfi > 0) WRITE(nu( 7), FMT1) IDATE, ZFI
         if(numhfo > 0) WRITE(nu( 8), FMT1) IDATE, ZFO
         if(numhfa > 0) WRITE(nu( 9), FMT1) IDATE, ZFA
         if(numhsv > 0) WRITE(nu(10), FMT1) IDATE, ZSV
         if(numhse > 0) WRITE(nu(11), FMT1) IDATE, ZSE
         if(numhcv > 0) WRITE(nu(12), FMT1) IDATE, ZCV
         if(numhce > 0) WRITE(nu(13), FMT1) IDATE, ZCE
        ELSE !IF (TIMTYP == 'D')THEN
         if(numhh > 0 ) WRITE(nu( 1), FMT1) DTIME, ZH  
         if(numhd > 0 ) WRITE(nu( 2), FMT1) DTIME, ZHD 
         if(numch > 0 ) WRITE(nu( 3), FMT1) DTIME, ZHPH
         if(numhc > 0 ) WRITE(nu( 4), FMT1) DTIME, ZHC
         if(numhs > 0 ) WRITE(nu( 5), FMT1) DTIME, ZHS
         if(numhhf > 0) WRITE(nu( 6), FMT1) DTIME, ZHF
         if(numhfi > 0) WRITE(nu( 7), FMT1) DTIME, ZFI
         if(numhfo > 0) WRITE(nu( 8), FMT1) DTIME, ZFO
         if(numhfa > 0) WRITE(nu( 9), FMT1) DTIME, ZFA
         if(numhsv > 0) WRITE(nu(10), FMT1) DTIME, ZSV
         if(numhse > 0) WRITE(nu(11), FMT1) DTIME, ZSE
         if(numhcv > 0) WRITE(nu(12), FMT1) DTIME, ZCV
         if(numhce > 0) WRITE(nu(13), FMT1) DTIME, ZCE
        END IF
      END DO WRITE_LOOP
      !
      CLOSE(inunit, IOSTAT=IERR)
      if(numhh > 0 ) CLOSE(nu( 1), IOSTAT=IERR)
      if(numhd > 0 ) CLOSE(nu( 2), IOSTAT=IERR)
      if(numch > 0 ) CLOSE(nu( 3), IOSTAT=IERR)
      if(numhc > 0 ) CLOSE(nu( 4), IOSTAT=IERR)
      if(numhs > 0 ) CLOSE(nu( 5), IOSTAT=IERR)
      if(numhhf > 0) CLOSE(nu( 6), IOSTAT=IERR)
      if(numhfi > 0) CLOSE(nu( 7), IOSTAT=IERR)
      if(numhfo > 0) CLOSE(nu( 8), IOSTAT=IERR)
      if(numhfa > 0) CLOSE(nu( 9), IOSTAT=IERR)
      if(numhsv > 0) CLOSE(nu(10), IOSTAT=IERR)
      if(numhse > 0) CLOSE(nu(11), IOSTAT=IERR)
      if(numhcv > 0) CLOSE(nu(12), IOSTAT=IERR)
      if(numhce > 0) CLOSE(nu(13), IOSTAT=IERR)
      !
      WRITE(*,'(/, /, A, /, /)') 'HYDFMT Completed Successfully'
      !
      END PROGRAM
C*******************************************************************************
      SUBROUTINE MODTIME(ITIME,TOTIM,ITMUNI,IDAYS,IDATE,ISTART,
     +                   TIMTYP,ELPTYP,FIRST,START,LEAPYR)
CC
C-----
C----- VERSION 0100 29JULY1998 MODTIME
C     ***********************************************************************
C     CONVERT ELAPSED MODEL TIME TO DECIMAL OR CLAENDAR DATES FOR HYDROGRAPHS
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      IMPLICIT REAL(REAL64) (A-H,O-Z)
      !
      CHARACTER(1):: TIMTYP, ELPTYP, LEAPYR
      LOGICAL:: FIRST
      INTEGER:: DATE1
CC
      INTERFACE
        INTEGER FUNCTION DAYS(DAT, LEAPYR)
          IMPORT, ONLY: REAL64
          !
          INTEGER,      INTENT(IN):: DAT
          CHARACTER(1), INTENT(IN):: LEAPYR
        END FUNCTION
      END INTERFACE
CC
      FYY=0.
      IYY=0
      IF(FIRST)REMTIM=0.0
      IF(LEAPYR == 'U')THEN
       YR=365.25
      ELSE
       YR=365.
      ENDIF
CC
      IF(ITMUNI == 0)THEN
      WRITE(*,*)'TIME UNIT OF MODEL DATA IS UNDEFINED CHECK',
     -'BASIC PACKAGE DATA-->ITMUNI'
      STOP
      ENDIF
CC
CC    SET MULTIPLIER BASED ON MODEL TIME, FOR DATE FORMAT CONVERSION
CC        TO ELAPSED DAYS
CC
      IF(TIMTYP == 'C')THEN
       IF(ITMUNI == 1)THEN
        DELTIM=86400.
       ELSE IF(ITMUNI == 2)THEN
        DELTIM=1440.
       ELSE IF(ITMUNI == 3)THEN
        DELTIM=24.
       ELSE IF(ITMUNI == 4)THEN
        DELTIM=1.
       ELSE IF(ITMUNI == 5)THEN
        DELTIM=1./YR
       ENDIF
       ITIME=TOTIM/DELTIM
       REMTIM=(TOTIM/DELTIM)-INT(TOTIM/DELTIM)+REMTIM
       IF((REMTIM-1.) > 0.)THEN
        REMTIM=REMTIM-1.
        ITIME=ITIME+1
       ENDIF
       ITIME=ITIME+IDAYS
       ITIME2=ITIME
       IDATE=DATE1(ITIME)
       IF(ITMUNI.LT.5)THEN
        YY=(IDATE/10000)-(ISTART/10000)
        IF(LEAPYR == 'L')THEN
         IF((MOD(YY,4.) == 0..AND.MOD(YY,100.).NE.0.).OR.
     &       (MOD(YY,400.) == 0.))THEN
           ITIME2=ITIME2+INT(YY/4.)+1
         ELSEIF(MOD(YY,4.).NE.0.)THEN
           ITIME2=ITIME2+INT(YY/4.)
         ENDIF
        ENDIF
        IDATE=DATE1(ITIME2)
       ENDIF
CC
CC    SET MULTIPLIER BASED ON MODEL TIME FOR DECIMAL TIME FLAG ELPTYP
CC
      ELSE IF(TIMTYP == 'D')THEN
       IF(ELPTYP == 'M'.or.ELPTYP == 'm')THEN
        IF(ITMUNI == 1)THEN
         DELTIM=60.
        ELSE IF(ITMUNI == 2)THEN
         DELTIM=1.
        ELSE IF(ITMUNI == 3)THEN
         DELTIM=1/60.
        ELSE IF(ITMUNI == 4)THEN
         DELTIM=1/1440.
        ELSE IF(ITMUNI == 5)THEN
         DELTIM=1./525600.
        ENDIF
       ELSE IF(ELPTYP == 'D'.or.ELPTYP == 'd')THEN
        IF(ITMUNI == 1)THEN
         DELTIM=86400.
        ELSE IF(ITMUNI == 2)THEN
         DELTIM=1440.
        ELSE IF(ITMUNI == 3)THEN
         DELTIM=24.
        ELSE IF(ITMUNI == 4)THEN
         DELTIM=1.
        ELSE IF(ITMUNI == 5)THEN
         DELTIM=1./YR
        ENDIF
       ELSE IF(ELPTYP == 'Y'.or.ELPTYP == 'y')THEN
        IF(ITMUNI == 1)THEN
         DELTIM=31536000.
        ELSE IF(ITMUNI == 2)THEN
         DELTIM=525600.
        ELSE IF(ITMUNI == 3)THEN
         DELTIM=8760.
        ELSE IF(ITMUNI == 4)THEN
         DELTIM=YR
        ELSE IF(ITMUNI == 5)THEN
         DELTIM=1.
        ENDIF
       TOTIM=(TOTIM/DELTIM)+START
       GOTO 99
       ENDIF
      IF(TIMTYP == 'C')THEN
       ISTY=(ISTART/10000)*10000
       IYY=DAYS(ISTART,LEAPYR)-DAYS(ISTY,LEAPYR)-1
       FYY=FLOAT(IYY)/YR
       YY=START+(ISTART/10000)+FYY
      endif
      TOTIM=TOTIM/DELTIM
      ENDIF
  99  RETURN
      END SUBROUTINE
C
C*******************************************************************************
CC
      INTEGER FUNCTION DAYS(DAT,LEAPYR)
C-----
C----- VERSION 0100 29JULY1998 DAYS
C     ***********************************************************************
CC     FOR DATE GIVEN AS AN INTEGER YYMMDD, THIS FUNCTION RETURNS
CC     THE NUMBER OF DAYS SINCE DECEMBER 31, 1899.
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      IMPLICIT REAL(REAL64) (A-H,O-Z)
      !
      INTEGER,      INTENT(IN):: DAT
      CHARACTER(1), INTENT(IN):: LEAPYR
CC
      INTEGER YEAR,DAY,DAYM(12)
CC
      DATA DAYM/0,31,59,90,120,151,181,212,243,273,304,334/
CC
      YEAR=DAT/10000
      M=DAT-YEAR*10000
      MONTH=M/100
      DAY=M-MONTH*100
      IF(MONTH.LT.1)MONTH=1
      IF(DAY.LT.1)DAY=1
      IF(LEAPYR == 'U')THEN
       YR=365.25
       DAYS=YEAR*YR
      ELSEIF(LEAPYR == 'L')THEN
       YR=365.
       DAYS=YEAR*YR+(YEAR+3)/4
      ELSE
       YR=365.
       DAYS=YEAR*YR
      ENDIF
      DAYS=DAYS+DAYM(MONTH)+DAY
      IF(MONTH.LE.2)RETURN
      IF(MOD(YEAR,4).NE.0) RETURN
      IF(LEAPYR == 'L')DAYS=DAYS+1
      RETURN
      END FUNCTION
C*******************************************************************************
      INTEGER FUNCTION DATE1(DAYS)
C-----
C----- VERSION 0100 29JULY1998 DATE1
C     ***********************************************************************
CC     FOR VALUE OF DAYS SINCE DECEMBER 31, 1899 FUNCTION RETURNS THE
CC     DATE AS AN INTEGER IN THE FORM YYMMDD
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      IMPLICIT REAL(REAL64) (A-H,O-Z)
      !
      INTEGER DAYS,DAYM(12,2),YY,MM,DD,DY,NL
CC
      DATA ((DAYM(I,J),I=1,12),J=1,2)
     &/0,31,59,90,120,151,181,212,243,273,304,334,
     & 0,31,60,91,121,152,182,213,244,274,305,335/
CC
      L=1
      NL=0
      IF(DAYS.GE.61) NL=(DAYS-61)/1461+1
      YY=(DAYS-NL-1)/365
      IF(MOD(YY,4) == 0) L=2
      DY=DAYS-365*YY-YY/4-(2-L)
      MM=0
   10 MM=MM+1
      IF(MM == 12) GO TO 20
      IF(DY > DAYM(MM+1,L)) GO TO 10
   20 DD=DY-DAYM(MM,L)
      DATE1=10000*YY+100*MM+DD
      RETURN
      END FUNCTION
C*******************************************************************************
      SUBROUTINE ETIMEFMT(fmtoute,ELPTYP,ITMUNI)
C-----
C----- VERSION 0100 02AUGUST2011 ETIMEFMT
C     ***********************************************************************
CC     RETURNS THE OUTPUT FORMAT string needed for the correct  
CC     number of significant figures to allow resolution at the user-specified time intervals
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
      IMPLICIT REAL(REAL64) (A-H,O-Z)
      !
      CHARACTER(*)::fmtoute
      CHARACTER(len(fmtoute))::FMTNEW
      CHARACTER TIMTYP*1,ELPTYP*1
      Integer ITMUNI
      fmtnew=fmtoute         !'(f21.3,5000G21.4)'
cc    Conversion to Decimal years maintinaing at least 2 significant figures in Model units of time
      IF(ELPTYP == 'Y'.or.ELPTYP == 'y')then
       IF(ITMUNI == 0.or.ITMUNI == 5)then !No conversion from 3 significant figures if output is undefined or in years
        fmtoute=fmtnew
       ELSEIF(ITMUNI == 1)THEN          !Significant figures from seconds to years
        write(fmtnew(2:6),'(a5)')'f21.9'
       ELSEIF(ITMUNI == 2)THEN          !Significant figures from minutes to years
        write(fmtnew(2:6),'(a5)')'f21.7'       
       ELSEIF(ITMUNI == 3)THEN          !Significant figures from hours to years
        write(fmtnew(2:6),'(a5)')'f21.5'       
       ELSEIF(ITMUNI == 4)THEN          !Significant figures from days to years
        write(fmtnew(2:6),'(a5)')'f21.4'       
       ENDIF
        fmtoute=fmtnew
cc    Conversion to Decimal Days Output
      ELSEIF(ELPTYP == 'D'.or.ELPTYP == 'd')then
       IF(ITMUNI == 0.or.ITMUNI == 5)then    !Significant figures from years to days
        fmtoute=fmtnew
       ELSEIF(ITMUNI == 1)THEN              !Significant figures from seconds to days
        write(fmtnew(2:6),'(a5)')'f21.8'      
       ELSEIF(ITMUNI == 2)THEN          !Significant figures from minutes to days
        write(fmtnew(2:6),'(a5)')'f21.5'       
       ELSEIF(ITMUNI == 3)THEN          !Significant figures from hours to days
        fmtoute=fmtnew
       ELSEIF(ITMUNI == 4)THEN          !Significant figures from days to days
        fmtoute=fmtnew       
       ENDIF      
        fmtoute=fmtnew
cc    Conversion to Decimal Minutes Output
      ELSEIF(ELPTYP == 'M'.or.ELPTYP == 'm')then
       IF(ITMUNI == 0.or.ITMUNI == 5)then
        fmtoute=fmtnew
       ELSEIF(ITMUNI == 1)THEN              !Significant figures from seconds to minutes
        fmtoute=fmtnew      
       ELSEIF(ITMUNI == 2)THEN          !Significant figures from minutes to minutes
        fmtoute=fmtnew       
       ELSEIF(ITMUNI == 3)THEN          !Significant figures from hours to minutes
        fmtoute=fmtnew       
       ELSEIF(ITMUNI == 4)THEN          !Significant figures from days to minutes
        fmtoute=fmtnew       
       ENDIF      
        fmtoute=fmtnew
      ENDIF
cc      
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE ERROR(MSG)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN):: MSG        ! Supplemental messages to write in error
        !
        CHARACTER(:), ALLOCATABLE:: FNAME
        CHARACTER(:), ALLOCATABLE:: ERR
        CHARACTER(1):: NL
        NL = NEW_LINE(' ')
        ERR=NL//NL//'                    HYDFMT FATAL ERROR'//NL//NL
        !
        IF(MSG /= '') THEN
         ERR = ERR//NL//'         THE DESCRIPTION OF THE ERROR IS:'//NL
     +            //NL//TRIM(MSG )//NL
        END IF
        !
        WRITE(*, '(A)') ERR
        !
        ERROR STOP ! :(
        !
      END SUBROUTINE
      !
      PURE SUBROUTINE UPPER(LN)
        IMPLICIT NONE
        CHARACTER(*), INTENT(INOUT):: LN
        CHARACTER(*), PARAMETER:: lowerCHAR="abcdefghijklmnopqrstuvwxyz"
        CHARACTER(*), PARAMETER:: upperCHAR="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        INTEGER:: I, N
        !
        DO I=1, LEN_TRIM(LN)
            N = INDEX( lowerCHAR, LN(I:I))
            !
            IF(N > 0) LN(I:I) = upperCHAR(N:N)
        END DO
      END SUBROUTINE
      !
      PURE SUBROUTINE ALLOC_CHAR20(VAR, DIM)
        IMPLICIT NONE
        CHARACTER(20), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VAR
        INTEGER,                                  INTENT(IN   ):: DIM
        !
        IF(ALLOCATED(VAR)) DEALLOCATE(VAR)
        !
        IF(DIM > 0) ALLOCATE(VAR(DIM))
        !
      END SUBROUTINE
      !
      PURE SUBROUTINE ALLOC_DBL(VAR, DIM)
        USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL64
        IMPLICIT NONE
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VAR
        INTEGER,                                 INTENT(IN   ):: DIM
        !
        IF(ALLOCATED(VAR)) DEALLOCATE(VAR)
        !
        IF(DIM > 0) ALLOCATE(VAR(DIM))
        !
      END SUBROUTINE
      !
      SUBROUTINE OPEN_OUTPUT(BASE, EXT, IU, timlbl, num, wellid)
        USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL64
        IMPLICIT NONE
        CHARACTER(*),                  INTENT(IN   ):: BASE, EXT  ! Base must be the exact size
        INTEGER,                       INTENT(INOUT):: IU
        CHARACTER(4),                  INTENT(IN   ):: timlbl
        INTEGER,                       INTENT(IN   ):: num
        CHARACTER(20), DIMENSION(num), INTENT(IN   ):: wellid
        !
        INTEGER:: IERR
        CHARACTER(LEN(BASE)+LEN(EXT)):: FN
        !
        FN = BASE // EXT
        !
        OPEN(NEWUNIT=IU, FILE=FN, STATUS='REPLACE', IOSTAT=IERR)
        !
        IF(IERR /= 0) CALL ERROR(BAD_FILE(FN))
        !
        WRITE(IU, '(i4,1x,a4,12x,*(1x,a20))') num, timlbl, wellid
        !
        CONTAINS
           PURE FUNCTION BAD_FILE(FNAME) RESULT(MSG)
             IMPORT, NONE
             IMPLICIT NONE
             CHARACTER(*), INTENT(IN):: FNAME
             CHARACTER(:), ALLOCATABLE:: MSG
             CHARACTER(1):: NL
             NL = NEW_LINE(' ')
             !
             MSG = 'Failed to create the following output file:'//NL//
     +             '"'//TRIM(ADJUSTL(FNAME))//'"'//NL//NL//
     +             'The file may already exist and HYDFMT cannot '//
     +             'replace it (possibly a locked file)'//NL//
     +             ' or the path to where to make the file is bad.'
           END FUNCTION
      END SUBROUTINE
      !
      PURE SUBROUTINE sng_to_dbl(dim, sng, dbl)
        USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
        IMPLICIT NONE
        INTEGER,                      INTENT(IN ):: dim
        REAL(REAL32), DIMENSION(dim), INTENT(IN ):: sng
        REAL(REAL64), DIMENSION(dim), INTENT(OUT):: dbl
        !
        INTEGER:: I
        !
        DO CONCURRENT(I=1:dim)
            dbl(I) = REAL(sng(I), REAL64)
        END DO
        !
      END SUBROUTINE
