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
      PARAMETER(nmt=500000)
      CHARACTER FN1*2048,FN2*2048,FMT1*80,FN3*2048,WELLID*20
      CHARACTER TIMTYP*1,ELPTYP*1,arr*2,timlbl*4,fmtout*27,fmtoute*18
C      character fmtoutd*20,blank*80,SITTYP*1,LEAPYR*1/'L'/
      character fmtoutd*21,blank,SITTYP*1,LEAPYR*1
      character WELLIDH*20,WELLIDD*20,WELLIDCH*20,
     & WELLIDC*20,WELLIDS*20,WELLIDFI*20,
     & WELLIDHF*20,WELLIDFO*20,WELLIDFA*20,
     & WELLIDSV*20,WELLIDSE*20,WELLIDCV*20,WELLIDCE*20
      DIMENSION JCOL(nmt),Z(nmt),zh(nmt),id(nmt),
     & zhd(nmt),zhph(nmt),zhc(nmt),zhs(nmt),
     & zfi(nmt),zfo(nmt),zhf(nmt),zfa(nmt),
     & zsv(nmt),zse(nmt),zcv(nmt),zce(nmt)
      DOUBLE PRECISION DTIME,DZ(nmt)
       DIMENSION WELLID(nmt),WELLIDH(nmt),WELLIDD(nmt),
     & WELLIDCH(nmt),
     & WELLIDC(nmt),WELLIDS(nmt),WELLIDFI(nmt),
     & WELLIDHF(nmt),WELLIDFO(nmt),WELLIDFA(nmt),
     & WELLIDSV(nmt),WELLIDSE(nmt),WELLIDCV(nmt),WELLIDCE(nmt)
      INTEGER DAYS,IDATE,nu(13),icnt(13)
      INTEGER ISTRNG
      LOGICAL FIRST,GEN
      DATA FIRST,GEN/.TRUE.,.TRUE./
C     + + + INCLUDE STATEMENTS + + +
      INCLUDE 'openspec.inc'
CC
      COMMON /T/YR,LEAPYR
cc
       DATA (nu(i),i=1,13)/61,62,63,64,65,66,67,68,69,70,71,72,73/
C
C seb MOVED INITIALIZTION OUTSIDE OF DATA TO MAKE EXE SMALLER
      wellid   = ' ------------------ '
      wellidh  = wellid
      wellidd  = wellid
      wellidch = wellid
      wellidc  = wellid
      wellids  = wellid
      wellidfi = wellid
      wellidfo = wellid
      wellidfa = wellid
      wellidhf = wellid
      wellidsv = wellid
      wellidse = wellid
      wellidcv = wellid
      wellidce = wellid
      icnt=0
      blank=''
      itype=13
      LEAPYR='L'
      inunit=60
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
      FN1=blank
      FN2=blank
      FN3=blank
      FMT1=blank
      DCONVRT=1.
CC
      WRITE(*,'(/,1X,A,/,/)') 'HYDFMT version 1.1'
CC
C----- Read file names
    1 WRITE(*,*) 'Enter name of file with unformatted hydrograph data:'
      READ(*,'(A)') FN1
      IF(FN1.EQ.'        ') GO TO 100
      ! Use the following line when compiling with Absoft compiler
      !OPEN(UNIT=inunit,FILE=FN1,FORM='UNFORMATTED',ACCESS='TRANSPARENT')
      ! Use the following line when compiling with Lahey or Intel compiler
      OPEN(UNIT=inunit,FILE=FN1,FORM=FORM,ACCESS=ACCESS,STATUS='OLD')
CC
C----- Read first unformatted header record with number of hydrographs saved.
C----- Negative number indicates double precision
      READ(inunit) NUMH,ITMUNI
      IF(NUMH.LT.0) THEN
        NUMH=-NUMH
        IPREC=2
        WRITE(*,*) 'Double precision hydrograph save file'
      ELSE
        IPREC=1
        WRITE(*,*) 'Single precision hydrograph save file'
      END IF
C----- Read second unformatted header record with labels of hydrographs saved
      READ(inunit) timlbl,(wellid(n)(1:20),n=1,numh)
      WRITE(*,*) 'Enter root-name of file for formatted output',
     &' (ex. hydro):'
      READ(*,'(A)') FN2
      CALL LNOTE(FN2,ISTRNG,LEN)
CC    if(LEN.gt.76)LEN=76  !No longer needed test
CC
      WRITE(*,*)'DO YOU WANT DECIMAL OR CALENDAR(DATE) ',
     &'TIME FORMAT(D/C):'
      READ(*,*)TIMTYP
      IF(TIMTYP.EQ.'C'.or.TIMTYP.EQ.'c')THEN
       WRITE(*,*)'ENTER INITIAL DATE OF TRANSIENT SIMULATION ',
     &'(YYYYMMDD ex. 19920827):'
       READ(*,*)ISTRT
       ISTT=(ISTRT/1000000)
       IST=ISTT*1000000
       ISTART=ISTRT-IST
       IDAYS=DAYS(ISTART)
      ELSE IF(TIMTYP.EQ.'D'.or.TIMTYP.EQ.'d')THEN
       WRITE(*,*)'ENTER UNIT DECIMAL TIME AS YEARS, DAYS, OR MINUTES',
     &' (Y,D,M) FOR OUTPUT:'
       READ(*,*)ELPTYP
       WRITE(*,*)'ENTER INITIAL DECIMAL TIME OF TRANSIENT',
     &' SIMULATION (ex. 1891.00):'
       READ(*,*)START
      ENDIF
      WRITE(*,*)'DO YOU IGNORE LEAP DAYS,'
      WRITE(*,*)'DISTRIBUTE THEM UNIFORMLY OVER FOUR YEARS, OR'
      WRITE(*,*)'DO YOU WANT THE LEAP DAY INSERTED INTO THE LEAP YEAR?'
      WRITE(*,*)'ENTER A CHOICE FOR IGNORE, UNIFORM, OR LEAP (I/U/L):'
      READ(*,*)LEAPYR
       if(timtyp.eq.'D'.or.timtyp.eq.'d')then
        CALL ETIMEFMT(fmtoute,ELPTYP,ITMUNI)
        FMT1=fmtoute
       elseif(timtyp.eq.'C'.or.timtyp.eq.'c')then
        FMT1=fmtoutd
       endif
       FMT1=ADJUSTL(FMT1)
       do 22 ii=1,numh
       CALL LEFTJ(wellid(ii))
          read(wellid(ii)(1:2),'(a2)')arr
       if(arr.eq.'HD')then
        id(ii)=1
        numhh=numhh+1
        wellidh(numhh)=wellid(ii)
       elseif(arr.eq.'DD')then
        id(ii)=2
        numhd=numhd+1
        wellidd(numhd)=wellid(ii)
       elseif(arr.eq.'HC')then
        id(ii)=3
        numch=numch+1
        wellidch(numch)=wellid(ii)
       elseif(arr.eq.'CP')then
        id(ii)=4
        numhc=numhc+1
        wellidc(numhc)=wellid(ii)
       elseif(arr.eq.'SB')then
        id(ii)=5
        numhs=numhs+1
        wellids(numhs)=wellid(ii)
       elseif(arr.eq.'ST')then
        id(ii)=6
        numhhf=numhhf+1
        wellidhf(numhhf)=wellid(ii)
       elseif(arr.eq.'SI')then
        id(ii)=7
        numhfi=numhfi+1
        wellidfi(numhfi)=wellid(ii)
       elseif(arr.eq.'SO')then
        id(ii)=8
        numhfo=numhfo+1
        wellidfo(numhfo)=wellid(ii)
       elseif(arr.eq.'SA')then
        id(ii)=9
        numhfa=numhfa+1
        wellidfa(numhfa)=wellid(ii)
       elseif(arr.eq.'SV')then
        id(ii)=10
        numhsv=numhsv+1
        wellidsv(numhsv)=wellid(ii)
       elseif(arr.eq.'SE')then
        id(ii)=11
        numhse=numhse+1
        wellidse(numhse)=wellid(ii)
       elseif(arr.eq.'CV')then
        id(ii)=12
        numhcv=numhcv+1
        wellidcv(numhcv)=wellid(ii)
       elseif(arr.eq.'CE')then
        id(ii)=13
        numhce=numhce+1
        wellidce(numhce)=wellid(ii)
       endif
  22   continue
cc
      if(numhh.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.gwh'
       OPEN(UNIT=nu(1),FILE=FN2,STATUS='REPLACE')
       write(nu(1),fmt=fmtout)numhh,timlbl,
     & (wellidh(n)(1:20),n=1,numhh)
      endif
      if(numhd.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.gwd'
       OPEN(UNIT=nu(2),FILE=FN2,STATUS='REPLACE')
       write(nu(2),fmt=fmtout)numhd,timlbl,
     & (wellidd(n)(1:20),n=1,numhd)
      endif
      if(numch.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.crh'
       OPEN(UNIT=nu(3),FILE=FN2,STATUS='REPLACE')
       write(nu(3),fmt=fmtout)numch,timlbl,
     & (wellidch(n)(1:20),n=1,numch)
      endif
      if(numhc.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.cmp'
       OPEN(UNIT=nu(4),FILE=FN2,STATUS='REPLACE')
       write(nu(4),fmt=fmtout)numhc,timlbl,
     & (wellidc(n)(1:20),n=1,numhc)
      endif
      if(numhs.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.sub'
       OPEN(UNIT=nu(5),FILE=FN2,STATUS='REPLACE')
       write(nu(5),fmt=fmtout)numhs,timlbl,
     & (wellids(n)(1:20),n=1,numhs)
      endif
      if(numhhf.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.sth'
       OPEN(UNIT=nu(6),FILE=FN2,STATUS='REPLACE')
       write(nu(6),fmt=fmtout)numhhf,timlbl,
     & (wellidhf(n)(1:20),n=1,numhhf)
      endif
      if(numhfi.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.inf'
       OPEN(UNIT=nu(7),FILE=FN2,STATUS='REPLACE')
       write(nu(7),fmt=fmtout)numhfi,timlbl,
     & (wellidfi(n)(1:20),n=1,numhfi)
      endif
      if(numhfo.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.otf'
       OPEN(UNIT=nu(8),FILE=FN2,STATUS='REPLACE')
       write(nu(8),fmt=fmtout)numhfo,timlbl,
     & (wellidfo(n)(1:20),n=1,numhfo)
      endif
      if(numhfa.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.gsf'
       OPEN(UNIT=nu(9),FILE=FN2,STATUS='REPLACE')
       write(nu(9),fmt=fmtout)numhfa,timlbl,
     & (wellidfa(n)(1:20),n=1,numhfa)
      endif
      if(numhsv.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.sbv'
       OPEN(UNIT=nu(10),FILE=FN2,STATUS='REPLACE')
       write(nu(10),fmt=fmtout)numhsv,timlbl,
     & (wellidsv(n)(1:20),n=1,numhsv)
      endif
      if(numhse.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.sbe'
       OPEN(UNIT=nu(11),FILE=FN2,STATUS='REPLACE')
       write(nu(11),fmt=fmtout)numhse,timlbl,
     & (wellidse(n)(1:20),n=1,numhse)
      endif
      if(numhcv.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.cpv'
       OPEN(UNIT=nu(12),FILE=FN2,STATUS='REPLACE')
       write(nu(12),fmt=fmtout)numhcv,timlbl,
     & (wellidcv(n)(1:20),n=1,numhcv)
      endif
      if(numhce.gt.0)then
       write(FN2(LEN+1:LEN+4),'(A)')'.cpe'
       OPEN(UNIT=nu(13),FILE=FN2,STATUS='REPLACE')
       write(nu(13),fmt=fmtout)numhce,timlbl,
     & (wellidce(n)(1:20),n=1,numhce)
      endif
cc
C----- Read the number of columns to be written to output file
C----- The number of columns does not include the simulation time which is
C----- always the first column in the formatted file.
      WRITE(*,*)'DO YOU WANT ALL OR SELECTED SITES(A/S):'
      READ(*,*)SITTYP
      IF(SITTYP.EQ.'A'.OR.SITTYP.EQ.'a')then
       GEN=.TRUE.
       NC=numh
       DO 21 IK=1,numh
  21    JCOL(IK)=IK
      ELSEIF(SITTYP.EQ.'S'.OR.SITTYP.EQ.'s')then
   5   WRITE(*,*) 'Enter number of columns to be read:'
       READ(*,*) NC
       WRITE(*,10) NC
       GEN=.FALSE.
C----- Read the column numbers, free format, any order
   10  FORMAT('Enter ',I3,' column numbers:')
       READ(*,*) (JCOL(N),N=1,NC)
C----- Read format for output of data, free format, no quotes.  Example:
C-----      (F10.2,4F10.3)
C----- This example writes simulation time with F10.3 and thirty columns of
C----- observations with F10.2
       WRITE(*,*)'Enter format for output (ex (F10.3,30F10.2) or (*):'
       READ(*,'(A)') FMT1
      DO 25 N=1,NC
      IF(JCOL(N).GT.NUMH) THEN
       WRITE(*,20) JCOL(N),NUMH
   20  FORMAT(' Column number',I3,' is greater than maximum possible of'
     1         ,I3,/,' please reenter column numbers...')
       GO TO 5
      ENDIF
   25 CONTINUE
      ENDIF
cc
      WRITE(*,*)'Enter the units conversion factor for data output'
      READ(*,*,END=30) DCONVRT
cc
C----- Read the unformatted and Write the formatted results
   30 IF(IPREC.EQ.1) THEN
C        READ(inunit,ERR=50) TIME,(Z(N),N=1,NUMH) ! Use when access=transparent
        READ(inunit,END=50) TIME,(Z(N),N=1,NUMH)
        do N=1,NUMH
            Z(N)=Z(N)*DCONVRT
        enddo
      ELSE
C        READ(inunit,ERR=50) DTIME,(DZ(N),N=1,NUMH) ! Use when access=transparent
        READ(inunit,END=50) DTIME,(DZ(N),N=1,NUMH)
        TIME=DTIME
        DO 31 N=1,NUMH
        Z(N)=DZ(N)*DCONVRT
   31   CONTINUE
      END IF
C----- Convert model elapsed time into calendar time
      CALL MODTIME(ITIME,TIME,ITMUNI,IDAYS,IDATE,ISTART,TIMTYP,
     -ELPTYP,FIRST,START)
      FIRST=.FALSE.
      IF(TIMTYP.EQ.'C'.or.TIMTYP.EQ.'c')IDATE=IDATE+IST
cc
       do 89 ii=1,NC
       if(id(ii).gt.0)icnt(id(ii))=icnt(id(ii))+1
       if(id(ii).eq.1)then
        zh(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.2)then
        zhd(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.3)then
        zhph(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.4)then
        zhc(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.5)then
        zhs(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.6)then
        zhf(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.7)then
        zfi(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.8)then
        zfo(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.9)then
        zfa(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.10)then
        zsv(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.11)then
        zse(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.12)then
        zcv(icnt(id(ii)))=Z(JCOL(ii))
       elseif(id(ii).eq.13)then
        zce(icnt(id(ii)))=Z(JCOL(ii))
       endif
   89  continue
cc
      IF(FMT1(1:3).EQ.'(*)')THEN
          WRITE(*,*)'SHOULD NOT BE HERE'
       IYEAR=IDATE/10000
       IMON=(IDATE-(IYEAR*10000))/100
       IDAY=(IDATE-(IYEAR*10000)-(IMON*100))
       IF(IYEAR.LE.99)IYEAR=1900+IYEAR
       IF(IDATE.LT.ISTART)IYEAR=2000+IYEAR
       IDATE=IMON*1000000+IDAY*10000+IYEAR
      if(numhh.gt.0)then
       DO 91 N=1,numhh
 91    WRITE(nu(1),*)WELLIDH(N),',',IDATE,',',ZH(N)
      endif
      if(numhd.gt.0)then
       DO 92 N=1,numhd
 92    WRITE(nu(2),*)WELLIDD(N),',',IDATE,',',ZHD(N)
      endif
      if(numch.gt.0)then
       DO 93 N=1,numch
 93    WRITE(nu(3),*)WELLIDCH(N),',',IDATE,',',ZHPH(N)
      endif
      if(numhc.gt.0)then
       DO 94 N=1,numhc
 94    WRITE(nu(4),*)WELLIDC(N),',',IDATE,',',ZHC(N)
      endif
      if(numhs.gt.0)then
       DO 95 N=1,numhs
 95    WRITE(nu(4),*)WELLIDS(N),',',IDATE,',',ZHS(N)
      endif
      if(numhhf.gt.0)then
       DO 96 N=1,numhhf
 96    WRITE(nu(6),*)WELLIDHF(N),',',IDATE,',',ZHF(N)
      endif
      if(numhfi.gt.0)then
       DO 97 N=1,numhfi
 97    WRITE(nu(7),*)WELLIDFI(N),',',IDATE,',',ZFI(N)
      endif
      if(numhfo.gt.0)then
       DO 98 N=1,numhfo
 98    WRITE(nu(8),*)WELLIDFO(N),',',IDATE,',',ZFO(N)
      endif
      if(numhfa.gt.0)then
       DO 99 N=1,numhfa
 99    WRITE(nu(9),*)WELLIDFA(N),',',IDATE,',',ZFA(N)
      endif
      if(numhsv.gt.0)then
       DO 101 N=1,numhsv
101    WRITE(nu(10),*)WELLIDSV(N),',',IDATE,',',ZSV(N)
      endif
      if(numhse.gt.0)then
       DO 102 N=1,numhsv
102    WRITE(nu(11),*)WELLIDSE(N),',',IDATE,',',ZSE(N)
      endif
      if(numhcv.gt.0)then
       DO 103 N=1,numhsv
103    WRITE(nu(12),*)WELLIDCV(N),',',IDATE,',',ZCV(N)
      endif
      if(numhce.gt.0)then
       DO 104 N=1,numhsv
104    WRITE(nu(13),*)WELLIDCE(N),',',IDATE,',',ZCE(N)
      endif
      ELSE IF (FMT1(1:3).NE.'(*)'.and.(TIMTYP.EQ.'C'.or.
     &TIMTYP.EQ.'c'))THEN
       if(numhh.gt.0)WRITE(nu(1),FMT1) IDATE,(ZH(N),N=1,numhh)
       if(numhd.gt.0)WRITE(nu(2),FMT1) IDATE,(ZHD(N),N=1,numhd)
       if(numch.gt.0)WRITE(nu(3),FMT1) IDATE,(ZHPH(N),N=1,numch)
       if(numhc.gt.0)WRITE(nu(4),FMT1) IDATE,(ZHC(N),N=1,numhc)
       if(numhs.gt.0)WRITE(nu(5),FMT1) IDATE,(ZHS(N),N=1,numhs)
       if(numhhf.gt.0)WRITE(nu(6),FMT1) IDATE,(ZHF(N),N=1,numhhf)
       if(numhfi.gt.0)WRITE(nu(7),FMT1) IDATE,(ZFI(N),N=1,numhfi)
       if(numhfo.gt.0)WRITE(nu(8),FMT1) IDATE,(ZFO(N),N=1,numhfo)
       if(numhfa.gt.0)WRITE(nu(9),FMT1) IDATE,(ZFA(N),N=1,numhfa)
       if(numhsv.gt.0)WRITE(nu(10),FMT1) IDATE,(ZSV(N),N=1,numhsv)
       if(numhse.gt.0)WRITE(nu(11),FMT1) IDATE,(ZSE(N),N=1,numhse)
       if(numhcv.gt.0)WRITE(nu(12),FMT1) IDATE,(ZCV(N),N=1,numhcv)
       if(numhce.gt.0)WRITE(nu(13),FMT1) IDATE,(ZCE(N),N=1,numhce)
      ELSE IF (FMT1(1:3).NE.'(*)'.and.(TIMTYP.EQ.'D'.or.
     &TIMTYP.EQ.'d'))THEN
       if(numhh.gt.0)WRITE(nu(1),FMT1) TIME,(ZH(N),N=1,numhh)
       if(numhd.gt.0)WRITE(nu(2),FMT1) TIME,(ZHD(N),N=1,numhd)
       if(numch.gt.0)WRITE(nu(3),FMT1) TIME,(ZHPH(N),N=1,numch)
       if(numhc.gt.0)WRITE(nu(4),FMT1) TIME,(ZHC(N),N=1,numhc)
       if(numhs.gt.0)WRITE(nu(5),FMT1) TIME,(ZHS(N),N=1,numhs)
       if(numhhf.gt.0)WRITE(nu(6),FMT1) TIME,(ZHF(N),N=1,numhhf)
       if(numhfi.gt.0)WRITE(nu(7),FMT1) TIME,(ZFI(N),N=1,numhfi)
       if(numhfo.gt.0)WRITE(nu(8),FMT1) TIME,(ZFO(N),N=1,numhfo)
       if(numhfa.gt.0)WRITE(nu(9),FMT1) TIME,(ZFA(N),N=1,numhfa)
       if(numhsv.gt.0)WRITE(nu(10),FMT1) TIME,(ZSV(N),N=1,numhsv)
       if(numhse.gt.0)WRITE(nu(11),FMT1) TIME,(ZSE(N),N=1,numhse)
       if(numhcv.gt.0)WRITE(nu(12),FMT1) TIME,(ZCV(N),N=1,numhcv)
       if(numhce.gt.0)WRITE(nu(13),FMT1) TIME,(ZCE(N),N=1,numhce)
      ENDIF
      do 51 ix=1,itype
   51 icnt(ix)=0
      GO TO 30
   50 CLOSE(inunit)
      if(numhh.gt.0)CLOSE(nu(1))
      if(numhd.gt.0)CLOSE(nu(2))
      if(numch.gt.0)CLOSE(nu(3))
      if(numhc.gt.0)CLOSE(nu(4))
      if(numhs.gt.0)CLOSE(nu(5))
      if(numhhf.gt.0)CLOSE(nu(6))
      if(numhfi.gt.0)CLOSE(nu(7))
      if(numhfo.gt.0)CLOSE(nu(8))
      if(numhfa.gt.0)CLOSE(nu(9))
      if(numhsv.gt.0)CLOSE(nu(10))
      if(numhse.gt.0)CLOSE(nu(11))
      if(numhcv.gt.0)CLOSE(nu(12))
      if(numhce.gt.0)CLOSE(nu(13))
  100 STOP
      END
C*******************************************************************************
      SUBROUTINE MODTIME(ITIME,TOTIM,ITMUNI,IDAYS,IDATE,ISTART,
     -TIMTYP,ELPTYP,FIRST,START)
CC
C-----
C----- VERSION 0100 29JULY1998 MODTIME
C     ***********************************************************************
C     CONVERT ELAPSED MODEL TIME TO DECIMAL OR CLAENDAR DATES FOR HYDROGRAPHS
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER TIMTYP*1,ELPTYP*1,LEAPYR*1
      INTEGER DATE1,DAYS
      LOGICAL FIRST
CC
      COMMON /T/YR,LEAPYR
CC

      FYY=0.
      IYY=0
      IF(FIRST)REMTIM=0.0
      IF(LEAPYR.EQ.'U')THEN
       YR=365.25
      ELSE
       YR=365.
      ENDIF
CC
      IF(ITMUNI.EQ.0)THEN
      WRITE(*,*)'TIME UNIT OF MODEL DATA IS UNDEFINED CHECK',
     -'BASIC PACKAGE DATA-->ITMUNI'
      STOP
      ENDIF
CC
CC    SET MULTIPLIER BASED ON MODEL TIME, FOR DATE FORMAT CONVERSION
CC        TO ELAPSED DAYS
CC
      IF(TIMTYP.EQ.'C'.or.TIMTYP.EQ.'c')THEN
       IF(ITMUNI.EQ.1)THEN
        DELTIM=86400.
       ELSE IF(ITMUNI.EQ.2)THEN
        DELTIM=1440.
       ELSE IF(ITMUNI.EQ.3)THEN
        DELTIM=24.
       ELSE IF(ITMUNI.EQ.4)THEN
        DELTIM=1.
       ELSE IF(ITMUNI.EQ.5)THEN
        DELTIM=1./YR
       ENDIF
       ITIME=TOTIM/DELTIM
       REMTIM=(TOTIM/DELTIM)-INT(TOTIM/DELTIM)+REMTIM
       IF((REMTIM-1.).GT.0.)THEN
        REMTIM=REMTIM-1.
        ITIME=ITIME+1
       ENDIF
       ITIME=ITIME+IDAYS
       ITIME2=ITIME
       IDATE=DATE1(ITIME)
       IF(ITMUNI.LT.5)THEN
        YY=(IDATE/10000)-(ISTART/10000)
        IF(LEAPYR.EQ.'L')THEN
         IF((MOD(YY,4.).EQ.0..AND.MOD(YY,100.).NE.0.).OR.
     &       (MOD(YY,400.).EQ.0.))THEN
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
      ELSE IF(TIMTYP.EQ.'D'.or.TIMTYP.EQ.'d')THEN
       IF(ELPTYP.EQ.'M'.or.ELPTYP.EQ.'m')THEN
        IF(ITMUNI.EQ.1)THEN
         DELTIM=60.
        ELSE IF(ITMUNI.EQ.2)THEN
         DELTIM=1.
        ELSE IF(ITMUNI.EQ.3)THEN
         DELTIM=1/60.
        ELSE IF(ITMUNI.EQ.4)THEN
         DELTIM=1/1440.
        ELSE IF(ITMUNI.EQ.5)THEN
         DELTIM=1./525600.
        ENDIF
       ELSE IF(ELPTYP.EQ.'D'.or.ELPTYP.EQ.'d')THEN
        IF(ITMUNI.EQ.1)THEN
         DELTIM=86400.
        ELSE IF(ITMUNI.EQ.2)THEN
         DELTIM=1440.
        ELSE IF(ITMUNI.EQ.3)THEN
         DELTIM=24.
        ELSE IF(ITMUNI.EQ.4)THEN
         DELTIM=1.
        ELSE IF(ITMUNI.EQ.5)THEN
         DELTIM=1./YR
        ENDIF
       ELSE IF(ELPTYP.EQ.'Y'.or.ELPTYP.EQ.'y')THEN
        IF(ITMUNI.EQ.1)THEN
         DELTIM=31536000.
        ELSE IF(ITMUNI.EQ.2)THEN
         DELTIM=525600.
        ELSE IF(ITMUNI.EQ.3)THEN
         DELTIM=8760.
        ELSE IF(ITMUNI.EQ.4)THEN
         DELTIM=YR
        ELSE IF(ITMUNI.EQ.5)THEN
         DELTIM=1.
        ENDIF
       TOTIM=(TOTIM/DELTIM)+START
       GOTO 99
       ENDIF
      IF(TIMTYP.EQ.'C'.or.TIMTYP.EQ.'c')THEN
       ISTY=(ISTART/10000)*10000
       IYY=DAYS(ISTART)-DAYS(ISTY)-1
       FYY=FLOAT(IYY)/YR
       YY=START+(ISTART/10000)+FYY
      endif
      TOTIM=TOTIM/DELTIM
      ENDIF
  99  RETURN
      END
C
C*******************************************************************************
CC
      INTEGER FUNCTION DAYS(DAT)
C-----
C----- VERSION 0100 29JULY1998 DAYS
C     ***********************************************************************
CC     FOR DATE GIVEN AS AN INTEGER YYMMDD, THIS FUNCTION RETURNS
CC     THE NUMBER OF DAYS SINCE DECEMBER 31, 1899.
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
CC
      INTEGER DAT,YEAR,DAY,DAYM(12)
      CHARACTER*1 LEAPYR
CC
      COMMON /T/YR,LEAPYR
CC
      DATA DAYM/0,31,59,90,120,151,181,212,243,273,304,334/
CC
      YEAR=DAT/10000
      M=DAT-YEAR*10000
      MONTH=M/100
      DAY=M-MONTH*100
      IF(MONTH.LT.1)MONTH=1
      IF(DAY.LT.1)DAY=1
      IF(LEAPYR.EQ.'U')THEN
       YR=365.25
       DAYS=YEAR*YR
      ELSEIF(LEAPYR.EQ.'L')THEN
       YR=365.
       DAYS=YEAR*YR+(YEAR+3)/4
      ELSE
       YR=365.
       DAYS=YEAR*YR
      ENDIF
      DAYS=DAYS+DAYM(MONTH)+DAY
      IF(MONTH.LE.2)RETURN
      IF(MOD(YEAR,4).NE.0) RETURN
      IF(LEAPYR.EQ.'L')DAYS=DAYS+1
      RETURN
      END
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
      IF(MOD(YY,4).EQ.0) L=2
      DY=DAYS-365*YY-YY/4-(2-L)
      MM=0
   10 MM=MM+1
      IF(MM.EQ.12) GO TO 20
      IF(DY.GT.DAYM(MM+1,L)) GO TO 10
   20 DD=DY-DAYM(MM,L)
      DATE1=10000*YY+100*MM+DD
      RETURN
      END
CC*****************************************************************************
       SUBROUTINE LNOTE(CSTRING,DIMS,LEN)
C-----
C----- VERSION 0100 29JULY1998 LNOTE
C     ***********************************************************************
CC     RETURNS THE LEFT-JUSTIFIED, NONBLANK LENGTH
CC     FOR A CHARACTER STRING
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
       INTEGER DIMS,LEN
       CHARACTER CSTRING*2048
       DO 10 N=DIMS,1,-1
       IF(CSTRING(N:N).NE.' ')GO TO 20
   10  CONTINUE
       LEN=0
       RETURN
   20  LEN=N
       RETURN
       END
CC*****************************************************************************
      SUBROUTINE LEFTJ(STRING)
C-----
C----- VERSION 0100 29JULY1998 LEFTJ
C     ***********************************************************************
CC     RETURNS THE LEFT-JUSTIFIED, NONBLANK
CC     PORTION OF A CHARACTER STRING
C     ***********************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER STRING*20
      DO 10 I=1,20
      IF(STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
      RETURN
   20 STRING=STRING(I:20)
      RETURN
      END
CC*****************************************************************************
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
      CHARACTER(*)::fmtoute
      CHARACTER(len(fmtoute))::FMTNEW
      CHARACTER TIMTYP*1,ELPTYP*1
      Integer ITMUNI
      fmtnew=fmtoute         !'(f21.3,5000G21.4)'
cc    Conversion to Decimal years maintinaing at least 2 significant figures in Model units of time
      IF(ELPTYP.eq.'Y'.or.ELPTYP.eq.'y')then
       IF(ITMUNI.eq.0.or.ITMUNI.eq.5)then !No conversion from 3 significant figures if output is undefined or in years
        fmtoute=fmtnew
       ELSEIF(ITMUNI.eq.1)THEN          !Significant figures from seconds to years
        write(fmtnew(2:6),'(a5)')'f21.9'
       ELSEIF(ITMUNI.eq.2)THEN          !Significant figures from minutes to years
        write(fmtnew(2:6),'(a5)')'f21.7'       
       ELSEIF(ITMUNI.eq.3)THEN          !Significant figures from hours to years
        write(fmtnew(2:6),'(a5)')'f21.5'       
       ELSEIF(ITMUNI.eq.4)THEN          !Significant figures from days to years
        write(fmtnew(2:6),'(a5)')'f21.4'       
       ENDIF
        fmtoute=fmtnew
cc    Conversion to Decimal Days Output
      ELSEIF(ELPTYP.eq.'D'.or.ELPTYP.eq.'d')then
       IF(ITMUNI.eq.0.or.ITMUNI.eq.5)then    !Significant figures from years to days
        fmtoute=fmtnew
       ELSEIF(ITMUNI.eq.1)THEN              !Significant figures from seconds to days
        write(fmtnew(2:6),'(a5)')'f21.8'      
       ELSEIF(ITMUNI.eq.2)THEN          !Significant figures from minutes to days
        write(fmtnew(2:6),'(a5)')'f21.5'       
       ELSEIF(ITMUNI.eq.3)THEN          !Significant figures from hours to days
        fmtoute=fmtnew
       ELSEIF(ITMUNI.eq.4)THEN          !Significant figures from days to days
        fmtoute=fmtnew       
       ENDIF      
        fmtoute=fmtnew
cc    Conversion to Decimal Minutes Output
      ELSEIF(ELPTYP.eq.'M'.or.ELPTYP.eq.'m')then
       IF(ITMUNI.eq.0.or.ITMUNI.eq.5)then
        fmtoute=fmtnew
       ELSEIF(ITMUNI.eq.1)THEN              !Significant figures from seconds to minutes
        fmtoute=fmtnew      
       ELSEIF(ITMUNI.eq.2)THEN          !Significant figures from minutes to minutes
        fmtoute=fmtnew       
       ELSEIF(ITMUNI.eq.3)THEN          !Significant figures from hours to minutes
        fmtoute=fmtnew       
       ELSEIF(ITMUNI.eq.4)THEN          !Significant figures from days to minutes
        fmtoute=fmtnew       
       ENDIF      
        fmtoute=fmtnew
      ENDIF
cc      
      RETURN
      END
