      Module GWFRIPMODULE
        USE BUDGET_GROUP_INTERFACE,    ONLY: BUDGET_GROUP
        PRIVATE:: BUDGET_GROUP
        !
        INTEGER,SAVE,POINTER:: IOUT, LOUT
        Integer, Save, Pointer :: MAXRIP,IRIPCB,IRIPCB1,NRIPCL
        Integer, Save, Pointer :: MAXPOLY,MAXTS,MXSEG
        Integer, Save, Dimension(:),Pointer,CONTIGUOUS:: NuSeg,NPoly
        Integer, Save, Dimension(:,:), Pointer,CONTIGUOUS:: CLoc
        Real, Save, Dimension(:), Pointer,CONTIGUOUS:: Sxd,Ard,Rmax, &
                                                      Rsxd,RIPET
        Real, Save, Dimension(:,:), Pointer,CONTIGUOUS:: fdh,fdr,HSurf 
        Real, Save, Dimension(:,:,:), Pointer ,CONTIGUOUS:: CovPFSG
        Double Precision, Dimension(:,:,:), Pointer,CONTIGUOUS :: C1,C2 
        CHARACTER(LEN=24), SAVE, DIMENSION(:),POINTER,CONTIGUOUS:: RIPNM
        TYPE(BUDGET_GROUP),SAVE,                         POINTER:: RIPBUD
        TYPE GWFRIPTYPE
          Integer,  Pointer      :: MAXRIP,IRIPCB,IRIPCB1,NRIPCL
          Integer,  Pointer      :: MAXPOLY,MAXTS,MXSEG
          Integer,  Dimension(:), Pointer ,CONTIGUOUS:: NuSeg,NPoly
          Integer, Dimension(:,:), Pointer,CONTIGUOUS:: CLoc
          Real, Dimension(:), Pointer ,CONTIGUOUS:: Sxd,Ard,Rmax,Rsxd,  &
                                                   RIPET
          Real, Dimension(:,:), Pointer  ,CONTIGUOUS:: fdh,fdr,HSurf
          Real, Dimension(:,:,:), Pointer,CONTIGUOUS:: CovPFSG
          Double Precision, Dimension(:,:,:), Pointer,CONTIGUOUS:: C1,C2
          Character(LEN=24),  Dimension(:),   Pointer,CONTIGUOUS:: RIPNM
          TYPE(BUDGET_GROUP),                            POINTER:: RIPBUD
        END TYPE
        TYPE(GWFRIPTYPE), SAVE :: GWFRIPDAT(10)
      End Module GWFRIPMODULE
!       
!     ******************************************************************************
!     MODFLOW 2000 Version II
!     This package calculates riparian ET for a variety of riparian
!     plant functional subgroups (PFSG). Examples of subgroups are: 
!       Obligate Wetland (e.g. cattails & bullrush)
!       Shallow-rooted Riparian(e.g. cockleburn & sacaton)
!       Deep-rooted Riparian (e.g. cottonwood & willow):small, medium, 
!         and large sizes.
!       Transitional riparian (e.g. mesquite & sycamore):small,dium, 
!         and large sizes.
!     This package uses GIS polygons within cells to construct PFSG
!     Coverages.  Each polygon in a cell has an unique surface elevation,
!     and may have a mixture of PFSG.
!     ************************************************************************* 
      Subroutine GWF2RIP4AR(IN,IGRID)        
!     ************************************************************************* 
!     This subroutine reads in dimension variables that remain constant
!     over the simulation, and allocates array storage for riparian
!     cells.  The follow variables are used:
!       IN is the unit number of input file.
!       IGRID is the grid number for LGR.
!       IOUT is the unit number of the LIST output file.
!       IFREFM is a flag indicating free format for data input.
!       MAXRIP is the maximum number of riparian cells over all stress 
!         periods.
!       MAXPOLY is the maximum number of polygons in a cell.
!       IRIPCB is a cell-by-cell printing flag and unit number,
!       IRIPCB1 is a flag to save cell values:location, surface elevation.
!         and ET rate for each plant functional subgroup.
!       NRIPCL is the number of active riparian cells in a stress period.
!       MAXTS is the total number of plant functional subgroups for 
!         all cells.
!       MXSEG is the maximum number of segments for interpolation of ET 
!         flux rate as a function of hydraulic head.
!       NuSEG is the number of active segments for a plant functional
!         subgroup.
!       Sxd is the saturation extinction depth with respect to land 
!         surface.
!       Ard is the active root depth.
!       Rmax is the maximum ET canopy flux rate.
!       Rsxd is the ET canopy flux rate at the saturation extinction 
!         depth.
!       CLoc has the (k,i,j) location of the cell for a stress period
!       NPoly is the number of polygons in a cell for a stress period. 
!       fdh is the active root depth segment factor.
!       fdR is the maximum ET flux segment factor.
!       HSurf is the land surface elevation for a polygon within a cell
!       CovPFSG has the PFSG fractional coverage in a cell for a polygon for
!         a stress period. 
!       RIPET is the total ET rate for a cell.
!       C1 is the HCOF portion of ET rate.
!       C2 is the RHS portion of ET rate.
!       RIPNM is the plant functional subgroup names, may be up to 
!         24 characters in length.
!       LINE is the content of one line read from the input file.
!       LLOC is a pointer used to keep track of position in LINE.
!       ISTART is the starting position of parsed word.
!       ISTOP is the ending position of parsed word.
!       N is a dummy integer variable.
!       R is a dummy real variable.
!       Nfdh is the characters 'fdh'.
!       NfdR is the characters 'fdR'.
!       ITS and ISEG are counters.
!     ************************************************************************* 
!
!     Specifications
!     ************************************************************************* 
      USE FILE_IO_INTERFACE,                ONLY: READ_TO_DATA
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
      Use GLOBAL,       Only: IOUT,IFREFM
      Use GWFRIPMODULE, Only: MAXRIP,MAXPOLY,IRIPCB,IRIPCB1,NRIPCL,     &
                             MAXTS,MXSEG,NuSeg,Sxd,Ard,Rmax,Rsxd,CLoc,  &
                             NPoly,fdh,fdr,HSurf,CovPFSG,RIPET,C1,C2,   &
                             RIPNM, RIPBUD
      Integer              :: IN,IGRID,LLOC,ISTART,ISTOP,N,ITS,ISEG
      Real                 :: R 
      Character(3)         :: Nfdh='fdh',NfdR='dfR'
      Character(200)       :: Line   
      TYPE(GENERIC_BLOCK_READER):: BL
      LOGICAL:: NOSHIFT
!     *************************************************************************
!
!1----Allocate scalar variables, which make it possible for multiple grids
!     to be defined.
!
      Allocate(MAXRIP,MAXPOLY,IRIPCB,IRIPCB1,NRIPCL,MAXTS,MXSEG)
      ALLOCATE(RIPBUD)
!
!2----Identify package and initialize NRIPCL
!      
        Write(IOUT,100) IN
100   Format(1x,/1x,'RIP-ET -- RIPARIAN PACKAGE, VERSION OWHM,',  &
                    ' INPUT READ FROM UNIT',I3) 
!
!   
      NRIPCL=0
!
!3----Read 1) maximum number of riparian cell, 2) the maximum 
!     number of polygons per cell 3) a unit or flag for 
!     printing or storing ET terms, and 4) an option value,if 
!     present,that allows storage in memory of the total ET Rate 
!     for each cell,
!
!      Read(IN,'(A)') LINE
      NOSHIFT = IFREFM == 0
      CALL READ_TO_DATA(LINE,IN,IOUT,IOUT,NOSHIFT=NOSHIFT)
      !
      CALL BL%LOAD(IN,IOUT,LINE=LINE)
      CALL RIPBUD%INIT('RIPARIAN_ET')
      CALL RIPBUD%LOAD(BL)
      !
      If (IFREFM == 0) Then
        Read(LINE,'(4I10)',IOSTAT=N) MAXRIP,MAXPOLY,IRIPCB,IRIPCB1
         IF(N.NE.0) THEN
               LLOC=1
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXRIP,R,IOUT,IN)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXPOLY,R,IOUT,IN)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRIPCB,R,IOUT,IN)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRIPCB1,R,IOUT,IN)
         END IF
      Else
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXRIP,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXPOLY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRIPCB,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRIPCB1,R,IOUT,IN)
      Endif
      !
      ! CHECK IF GLOBAL SHUTDOWN OF CBC IS IN EFFECT
       CALL CHECK_CBC_GLOBAL_UNIT(IRIPCB)
      !
      Write(IOUT, 110) MAXRIP
110   Format(1x,'MAXIMUM OF',I5,' RIPARIAN CELLS')
      Write(IOUT,120) MAXPOLY
120   Format(1x,'MAXIMUM OF',I5,' POLYGONS PER CELL')
      If(IRIPCB .LT. 0) then
          write(IOUT, 130)
130     Format(1x,'THE ET RATE FOR EACH PLANT FUNCTIONAL SUBGROUP WILL ' &
                 ,'BE WRITTEN TO THE LIST FILE WHEN ICBCFL IS NOT 0')
      Elseif (IRIPCB .GT. 0) then
          write(IOUT,140)IRIPCB
140   Format(1x,'TOTAL ET RATE FOR EACH CELL WILL BE SAVED ON UNIT',I3)
      Endif
      If(IRIPCB1.GT.0) Then
        Write(IOUT,150)IRIPCB1
150     Format(1x,'CELL LOCATION, SURFACE ELEVATION, AND PLANT ', &
                 'FUNCTIONAL SUBGROUP ET RATES WILL BE SAVED ',   &
                 'ON UNIT',I3)
      End if
!
!4----Read and print maximum number of plant functional subgroups (MAXTS), 
!     and the maximum of interpolation segments for the ET flux 
!     function (MXSEG).
!
      If (IFREFM == 0) Then
        Read(IN,'(2I10)') MAXTS,MXSEG
      Else
        Read(IN,*) MAXTS,MXSEG
      Endif
        write(IOUT,160) MAXTS,MXSEG
160   Format &
      (1x,'MAXIMUM NUMBERS OF PLANT FUNCTIONAL SUBGROUPS AND SEGMENTS ' &
       /,1x,'PLANT FUNCTIONAL SUBGROUPS  =',I3, &
       /,1x,'MAXIMUM CURVE SEGMENTS    =',I3) 
!
!5----Allocate REAL variables: Sxd, Ard, Rmax, Rsxd, fdh, fdR, HSurf, RIPET 
!     CovPFSG.
! 
      Allocate(Sxd(MAXTS),Ard(MAXTS),Rmax(MAXTS),Rsxd(MAXTS),RIPET(MAXRIP)) 
      Allocate(fdh(MAXTS,MXSEG),fdR(MAXTS,MXSEG),HSurf(MAXRIP,MAXPOLY))
      Allocate(CovPFSG(MAXRIP,MAXPOLY,MAXTS))
!
!6----Allocate INTEGER variables: NuSeg,Cloc,NPOLY
!
      Allocate(NuSeg(MAXTS),NPoly(MAXRIP) )
      Allocate(Cloc(MAXRIP,3))
!
!7----Allocate DOUBLE PRECISION variables: C1,C2
!
      Allocate(C1(MAXRIP,MAXPOLY,MAXTS),C2(MAXRIP,MAXPOLY,MAXTS))
      C1 = 0D0
      C2 = 0D0
!
!8----Allocate CHARACTER variables:
!
      Allocate(RIPNM(MAXTS))
!
!9----For each plant functional subgroup, read the name, saturated extinction  
!     depth (measured from land surface), active root depth, maximum ET flux,
!     ET flux rate at the saturation extinction depth, and the number of  
!     segments used to interpolate the ET flux rate curve. 
!
      Do ITS=1, MAXTS
        If(IFREFM == 0) then
          Read(In,'(A,4F10.0,I10)') RIPNM(ITS),Sxd(ITS),Ard(ITS),          &
               Rmax(ITS),Rsxd(ITS), NuSEG(ITS)
        Else
          Read(IN,*)RIPNM(ITS),Sxd(ITS),Ard(ITS),Rmax(ITS),Rsxd(ITS),      &
                 NuSEG(ITS) 
        End if
!
!10--For each plant functional subgroup, read the active root
!    depth segment factors and ET flux rate segment factors.
!
        If(IFREFM == 0) then
          Read(In,'(10F10.4)') (fdh(ITS,ISEG), ISEG=1,NuSEG(ITS))
          Read(In,'(10F10.4)') (fdR(ITS,ISEG), ISEG=1,NuSEG(ITS))
        Else
            Read(IN,*) (fdh(ITS,ISEG), ISEG=1,NuSeg(ITS))
            Read(IN,*) (fdR(ITS,ISEG), ISEG=1,NuSeg(ITS))
        End if
      End do
!
!11---Print out plant functional subgroup name, saturated extinction depth 
!     (measured from land surface),active root depth, maximum ET flux, 
!      ET flux rate at saturation extinction depth.
!
        Write(IOUT,170)
170   Format(/,'                    RIPARIAN INFORMATION'//, &
             '           NAME            SATURATION    ACTIVE      ', &
             'MAXIMUM     ET FLUX AT'/,                               &  
             '                           EXTINCTION     ROOT         ', &
             'ET        SATURATION'/,                                   &
             '                             DEPTH       DEPTH        ',  &
             'FLUX     EXTINCTION DEPTH')  
!       
      Do ITS=1, MAXTS
        Write(IOUT,180)RIPNM(ITS),Sxd(ITS),Ard(ITS),Rmax(ITS),Rsxd(ITS) 
180     Format(1x,A,T27,F10.4,T38,F10.4,T52,E11.4,T65,E11.4)
      End do
      !
!12---Write PFSG names to the IRIPCB1 file
!
      If(IRIPCB1>0) Then
        Write(IRIPCB1,'(20A24)') (RIPNM(I),I=1,MAXTS)
      End If

!
!13---Print out plant functional subgroup name and active root depth and ET 
!     flux rate segments.
!
      Write(IOUT,190)
190   Format(//,'                    SEGMENT INFORMATION'//, &
           '           NAME                                 SEGMENTS ')
! 
      Do ITS=1, MAXTS
        Write(IOUT,200)RIPNM(ITS),Nfdh,(fdh(ITS,ISEG),ISEG=1,NuSEG(ITS))
200     Format(/,1x,A,T27,A,10F10.4)
        Write(IOUT,210) NfdR,(fdR(ITS,ISEG), ISEG=1,NuSEG(ITS))
210     Format(1x,T27,A,10F10.4)
      End Do  
!
!14---Save pointers to data
!
      Call SGWF2RIP4PSV(IGRID)        
!
!15---Return
!
      Return
      End Subroutine GWF2RIP4AR  
!
!     *************************************************************************
      Subroutine GWF2RIP4RP(IN,IGRID)
!     *************************************************************************
!     This subroutine reads by stress period, the number of riparian cells,the 
!     number of polygons for the cell,the location of the riparian cells
!     (layer, row, column), polygon number,the land surface elevation for the  
!     polygon,and the percentage coverage of each plant functional subgroup in 
!     each polygon.
!       IN      is the unit number of input file.
!       IGRID is the grid number for LGR.
!       IOUT    is the unit number of the LIST output file.
!       IFREFM  is a flag indicating free format for data input.
!               stress period.
!       MAXRIP  is the maximum number of riparian cells over all stress 
!               periods.
!       MAXPOLY is the maximum number of polygons in a cell.
!       MAXTS   is the total number of plant functional subgroups for 
!               all cells.
!       NRIPCL  is the number of active riparian cells in a stress period.
!       CLoc    has the (k,i,j) location of the cell for a stress period
!       NPoly   is the number of polygons in a cell for a stress period..
!       HSurf   is the land surface elevation for a polygon within a cell
!       CovPFSG has the PFSG fractional coverage in a cell for a polygon for
!               a stress period. 
!       RIPNM   is the plant functional subgroup names, may be up to 
!               24 characters in length. 
!       ITMP    is number of riparian cells or flag to repeat data from previous 
!       NC,NP,and I are counters.
!     *************************************************************************
!
!     Specifications
!     ************************************************************************* 
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      Use Global,         Only: IOUT,IFREFM
      Use GWFRIPMODULE,   Only: MAXRIP,MAXPOLY,MAXTS,NRIPCL,CLoc,NPoly, &
                                HSurf,CovPFSG,RIPNM,RIPBUD
        Integer::IN,IGRID,ITMP
        Integer:: NC,NP,I
        INTEGER:: N,LLOC, ISTART, ISTOP
        CHARACTER(768):: LINE
        REAL:: R
        LOGICAL:: NOSHIFT
!     *************************************************************************
!
!1----Set pointers for current grid
!
      Call SGWF2RIP4PNT(IGRID)
!
!2----Read in ITMP (number of riparian cells or flag to reuse data).
!
      If(IFREFM == 0) then
        Read(IN,'(I10)') ITMP
      Else
        Read(IN,*) ITMP
      Endif
!
!3----Test ITMP.
!
      If(ITMP < 0) Then
!
!3a---If ITMP<0, reuse data from last stress period.
!
          write(IOUT,100)
100   Format(1x,/1x,             &
                 'REUSING RIPARIAN CELL DATA FROM LAST STRESS PERIOD')
        Return
      Else
!
!3b---If ITMP>=0, it is the number of riparian cells.
!
        NRIPCL=ITMP
      End if
!
!4----If NRIPCL>MAXRIP then STOP.
!
      If(NRIPCL .GT. MAXRIP) Then
          write(IOUT,110) NRIPCL,MAXRIP
110   Format(1x,/,1x,'NRIPCL(',I4,') IS GREATER THAN MAXRIP(',I4,')')
        ERROR STOP   
      Endif
!
!5----Printout number of active riparian cells for the stress period.
!
        write(IOUT,120) NRIPCL
120   Format(1x,//,1x,I5,' RIPARIAN CELLS.')
!
      IF (RIPBUD%BUDGET_GROUPS) THEN
          CALL RIPBUD%RESET()
      ELSE
          CALL RIPBUD%ADD( -NRIPCL )
      END IF
!
!6----If there are no riparian cells this stress period, return.
!
      If(NRIPCL == 0) Return
!
!7----Initialize NPOLY, CLOC, HSURF, and COVPFSG
!
      NPOLY   = 0
      CLOC    = 0
      HSURF   = 0.0
      COVPFSG = 0.0
!
!8----For each riparian cell, read a cell number and the number of polygons
!      in the cell, 
!
      NOSHIFT = IFREFM == 0
      Do NC=1, NRIPCL
        CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=NOSHIFT)
        If(IFREFM == 0) then
                Read(LINE,'(4I10)', IOSTAT=N) (CLOC(NC,I),I=1,3),NPOLY(NC)
                IF(N.NE.0) THEN
                      LLOC=1
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,CLOC(NC,1),R,IOUT,IN)
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,CLOC(NC,2),R,IOUT,IN)
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,CLOC(NC,3),R,IOUT,IN)
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPOLY(NC),R,IOUT,IN)
                END IF
                LLOC=41
        Else
            LLOC = 1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,CLOC(NC,1),R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,CLOC(NC,2),R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,CLOC(NC,3),R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPOLY(NC),R,IOUT,IN)
          !Read(IN,*) (CLOC(NC,I),I=1,3),NPOLY(NC)
        End If
        !
        IF (RIPBUD%BUDGET_GROUPS) THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
            CALL RIPBUD%ADD( NC, LINE(ISTART:ISTOP) )
        END IF
!
!9----For each polygon, read surface elevation, and then fractional 
!      coverage for each plant functional subgroup. A polygon is limited 
!      to 24 PFSG unless the format is changed.
!
        Do NP =1,NPOLY(NC)      
          If(IFREFM == 0) then         
            Read(IN,'(F10.2,25F10.5)') HSURF(NC,NP),(COVPFSG(NC,NP,I),I=1,MAXTS)
          Else
            Read(IN,*) HSURF(NC,NP),(COVPFSG(NC,NP,I),I=1,MAXTS)
          Endif
        End Do  !End of polygon loop
      End Do  !End of cell loop
!
!10---Print output heading.
!
      Write(IOUT,130)
130   Format(1x/20x,'RIPARIAN CELL INFORMATION')
      Write(IOUT,140) (RIPNM(I),I=1,MAXTS)
140   FORMAT(1x,/,1x,'LAYER  ROW  COLUMN  POLYGON    SURFACE    ', &
                'FRACTIONAL COVERAGE OF PLANT FUNCTIONAL SUBGROUPS',/, &
                '                     NUMBER     ELEVATION',20(2x,A24))
!
!11---Print out polygon data cell by cell.
!
      DO NC=1, NRIPCL
        DO NP=1,NPOLY(NC)
          If(NP.EQ.1) Then
            Write(IOUT,150) &
          (CLOC(NC,I),I=1,3),NP,HSURF(NC,NP), &
          (COVPFSG(NC,NP,I),I=1,MAXTS)
150         Format(1x,I3,3x,I3,3x,I3,6x,I3,4x,F10.2,20(3x,F15.5,8x))
          Else
            Write(IOUT,160) NP,HSURF(NC,NP),(COVPFSG(NC,NP,I),I=1,MAXTS)
160         Format(1x,21x,I3,4x,F10.2,20(3x,F15.5,8x))
          End IF
        End Do  !End of polygon loop
      End do  !End of cell loop
!
!12---Return.
!
      Return
      End Subroutine GWF2RIP4RP 
!
!CC--rth
      SUBROUTINE GWF2RIPAD(KPER,IGRID)
!C-----VERSION 2.01 10/2/2012 GWF2RIPAD
!C*********************************************************************
!C     APPLY SUBSIDENCE DISPLACEMENT (DVZ) TO GROUND-SURFACE ELEVATION,
!C     HSURF, for each Riparian-ET polygon.
!C*********************************************************************
!C        SPECIFICATIONS:
!C     -----------------------------------------------------------------
      USE GWFRIPMODULE,    ONLY:HSURF,CLOC,NPOLY,NRIPCL
      USE FMPBLK,       ONLY:ZER
      USE GLOBAL,       ONLY:IOUT,NROW,NCOL,NLAY,IBOUND,SUBLNK  
      USE GWFSUBMODULE, ONLY:DVZ
      IMPLICIT NONE
!C     -----------------------------------------------------------------
!C        ARGUMENTS:
!C     -----------------------------------------------------------------
      INTEGER KPER,IGRID
!C     -----------------------------------------------------------------
!C        LOCAL VARIABLES:
!C     -----------------------------------------------------------------
      INTEGER IR,IC,IL,IP
!C     -----------------------------------------------------------------
      If(NRIPCL .EQ. 0) Return
      IF(SUBLNK)THEN
      !
      CALL SGWF2RIP4PNT(IGRID)
!C
      IF(ANY(DVZ.NE.ZER)) THEN                                         !seb changed from IF(ABS(MAXVAL(DVZ)).GT.ZER)
!C1-----APPLY DISPLACEMENT TO GROUND-SURFACE ELEVATION OF UPPERMOST ACTIVE LAYER    
      DO ip= 1,NRIPCL
      IR=CLOC(ip,2)
      IC=CLOC(ip,3)
      IL=CLOC(ip,1)
        IF(IBOUND(IC,IR,IL).EQ.0.AND.IL.NE.NLAY) GOTO 60
        HSURF(IC,IR)=HSURF(IC,IR)-DVZ(IC,IR,IL)
 60   ENDDO
!C
      ENDIF
      !
      END IF !SUBLNK
!C
      RETURN
      END
!C
!CC--rth

!     *************************************************************************
      Subroutine GWF2RIP4FM(IGRID) 
!     *************************************************************************
!     Add riparian evapotranspiration to RHS and HCOF. The ET flux
!     rate is estimated using a segmented interpolation structure.
!       IGRID is the grid number for LGR.
!       NCOL is the number of columns in the grid.
!       NROW is the number of rows in the grid. 
!       NLAY is the umber of layers in the grid.
!       DELR is the cell dimension in row direction.
!       DELC is the cell dimension in the column direction.
!       IBOUND is status of cell: <0, constant head;=0, inactive;>0, active.
!       HNEW is the most recent estimate of head in a cell.
!       RHS is the right-hand side of the finite-difference equations.
!       HCOF is the coefficient of head in the finite-difference equations.
!       MAXTS is the total number of plant functional subgroups for 
!         all cells.
!       MXSEG is the maximum number of segments for interpolation of ET 
!         flux rate as a function of hydraulic head.
!       NRIPCL is the number of active riparian cells in a stress period.
!       Sxd is the saturation extinction depth with respect to land 
!         surface.
!       Ard is the active root depth.
!       Rmax is the maximum ET canopy flux rate.
!       Rsxd is the ET canopy flux rate at the saturation extinction 
!         depth.
!       fdh is the active root depth segment factor.
!       fdR is the maximum ET flux segment factor.
!       NuSEG is the number of active segments for a plant functional
!         subgroup.
!       CLoc has the (k,i,j) location of the cell for a stress period
!       NPoly is the number of polygons in a cell for a stress period. 
!       HSurf is the land surface elevation for a polygon within a cell
!       CovPFSG has the PFSG fractional coverage in a cell for a polygon for
!         a stress period. 
!       C1 is the HCOF portion of ET rate.
!       C2 is the RHS portion of ET rate.
!       IL is the cell layer.
!       IR is the cell row.
!       IC is the cell column. 
!       HH is the hydraulic head in the cell.
!       HK(N) is the head value at Nth vertex of the Ard segments.
!       RK(N)is the ET canopy flux at Nth vertex of the R segments.
!       HCOFtrm is a function to calculate HCOF part of ET canopy flux.
!       RHStrm is a function to calculate RHS part of ET canopy flux.
!       fCov is the fractional coverage in a cell by a plant functional 
!         subgroup.
!       Hxd is the extinction depth elevation.
!       Hsxd is the saturated extinction depth elevation.
!       LC,LP,KS,and NTS are counters.
!     *************************************************************************
!
!     Specifications:
!     *************************************************************************
      Use GLOBAL,       Only:NCOL,NROW,NLAY,DELR,DELC,IBOUND,HNEW,RHS,HCOF
      USE GWFRIPMODULE, Only:MAXTS,MXSEG,NRIPCL,Sxd,Ard,Rmax,Rsxd,fdH,fdR,    &
                             NuSeg,CLoc,NPoly,HSurf,CovPFSG,C1,C2
      Integer::IGRID                
      Integer::IL,IR,IC 
      Integer::LC,LP,KS,NTS 
      Double Precision::HH
      Double Precision, Dimension(MXSEG+1)::HK,RK
      Double Precision::HCOFtrm,RHStrm
      Real::fCov,Hxd,Hsxd
!     *************************************************************************
!
!1----Set pointers to current grid.
!
      Call SGWF2RIP4PNT(IGRID)      
!
!2----If NRIPCL<=0, there are no riparian cells, return.
!
      If (NRIPCL<=0) return
!
!3----Process each cell in the riparian cell list.
!
      Cell_Loop: Do LC=1,NRIPCL
!
!4----Get column, row and layer from riparian cell array.
!
        IL=CLoc(LC,1)
        IR=CLoc(LC,2)
        IC=CLoc(LC,3)
!
!5----If cell is external skip it and send a message to screen that cell
!     is inactive.
!
        If(IBOUND(IC,IR,IL) == 0) Then
            Write(*,'(A)') "**** Riparian cell is inactive ****"
            C1(LC,:,:)=0D0                                                 !seb
            C2(LC,:,:)=0D0                                                 !seb
            Cycle Cell_Loop
        End if
!
!6----Set head for cell.
!
        HH=HNEW(IC,IR,IL)
!
!7----Process each polygon in a cell
!
        Poly_Loop: Do LP=1,NPoly(LC)
!
!8----Process each plant functional subgroup  
!
          TS_Loop:Do NTS=1,MAXTS
!
!9----Initialize C1 and C2
!
            C1(LC,LP,NTS)=0D0
            C2(LC,LP,NTS)=0D0
!
!10---Determine cell non-zero fractional coverage for a plant functional 
!     subgroup, calculate Hsxd and Hxd; and initialize the extinction
!     depth segment end points, HK(1) to Hxd, and RK(1) to zero.
! 
            fCov=CovPFSG(LC,LP,NTS)
            Hsxd=HSURF(LC,LP)-Sxd(NTS)
            Hxd=Hsxd-Ard(NTS)
            HK(1)=Hxd
            RK(1)=0.0
!
!11---Check to see if HH is beyond the ends of the ET canopy flux curve;
!     If HH <= Hxd, set both C1 and C2 to zero; or if HH >= Hsxd,
!     set C1=0.0 and C2=-Rsxd*fCov*DELC(IR)*DELR(IC); 
!     in either case, cycle to the next plant functional subgroup. 
!
            If(HH > Hsxd ) Then
              C1(LC,LP,NTS)=0D0
              C2(LC,LP,NTS)=-Rsxd(NTS)*fCov*DELR(IC)*DELC(IR)
              RHS(IC,IR,IL)=RHS(IC,IR,IL)-C2(LC,LP,NTS)
              cycle TS_Loop
            Else if(HH <= Hxd) then
              C1(LC,LP,NTS)=0D0
              C2(LC,LP,NTS)=0D0
              cycle TS_Loop                             
            End if
!
!12---Loop through the ET canopy flux rate function vertices.
!
            Seg_Loop:DO KS=1, NuSeg(NTS)
!
!13---Calculate HKs and RKs as needed.
!
              HK(KS+1)=Hk(KS)+fdh(NTS,KS)*Ard(NTS)
              RK(KS+1)=RK(KS)+fdR(NTS,KS)*Rmax(NTS)
!
!14----Check to see if HK(KS)<HH<=HK(KS+1)
!
              IF(HH > HK(KS) .and. HH <= HK(KS+1)) Then
!
!15----When it is,  calculate C1 and C2 using the functions HCOFtrm and 
!      RHStrm that are adjusted with fCov, DELR, and DELC.
!
                C1(LC,LP,NTS)=-fCov*HCOFtrm(HK(KS),HK(KS+1),    &
                             RK(KS),RK(KS+1))* DELR(IC)*DELC(IR)
                C2(LC,LP,NTS)=-fCov*RHStrm(HK(KS),HK(KS+1),     &
                             RK(KS),RK(KS+1))*DELR(IC)*DELC(IR)
!
!16----Add C1 to HCOF and subtract C2 from RHS.
!
                HCOF(IC,IR,IL)=HCOF(IC,IR,IL)+C1(LC,LP,NTS)
                RHS(IC,IR,IL)=RHS(IC,IR,IL)-C2(LC,LP,NTS)
                EXIT Seg_Loop
              End if
            End do Seg_Loop
          End do TS_Loop
        End Do Poly_Loop
      End do Cell_Loop
!
!17-----Return
!
    Return                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
! 
 End Subroutine GWF2RIP4FM
!
!     *************************************************************************
      Subroutine GWF2RIP4BD(KSTP,KPER,IGRID)
!     *************************************************************************
!     Calculates volumetric budgets for riparian cells, 
!       KSTP is a time step counter.
!       KPER is a stress period counter.
!       IGRID is the grid number for LGR.
!       IOUT is the unit number of the LIST output file.
!       NCOL is the number of columns in the grid.
!       NROW is the number of rows in the grid. 
!       NLAY is the umber of layers in the grid.
!       IBOUND is status of cell: <0, constant head;=0, inactive;>0, active.
!       HNEW is the most recent estimate of head in a cell
!       BUFF is a three dimensional array containing the RATE for each 
!         cell.
!       MSUM is the Counter for budget entries and labels in VBVL and VBNM
!       VBVL are the entries for volumetric budget.
!       VBNM are the abels for entries in the volumetric budgets
!       ICBCFL is the flag for recording cell-by-cell flows
!       DELT is the length of current time step
!       MAXRIP is the maximum number of riparian cells over all stress 
!         periods,
!       MAXPOLY is the maximum number of polygons in a cell.
!       MAXTS is the otal number of plant functional subgroups for 
!          all cells
!       NRIPCL is the number of active riparian cells in a stress period
!       IRIPCB is a cell-by-cell save flag and unit number
!       IRIPCB1 is a flag to save cell values:location, surface elevation
!          and ET rate for each plant functional subgroup.
!       C1 is the HCOF portion of ET rate.
!       C2 is the RHS portion of ET rate.
!       CLoc has the (k,i,j) location of the cell for a stress period
!       NPoly is the number of polygons in a cell for a stress period. 
!       HSurf is the land surface elevation for a polygon within a cell
!       CovPFSG has the PFSG fractional coverage in a cell for a polygon for
!       RIPNM is the plant functional subgroup names, may be up to 
!         24 characters in length.
!       RIPET is the total ET rate for a cell.
!       TEXT is a 16 character string, '     RIPARIAN ET' used in the 
!          budget.
!       IL is the cell layer.
!       IR is the cell row.
!       IC is the cell column. 
!       IBD is a print or save flag.
!       Zero is the number 0.
!       RATE is plant functional subgroup aggregated ET rate for a cell,
!          and is written to the RIP array if CBC is indicated under
!          options. 
!       Sum is an accumulator.
!       TotPFSG is the total ET for each PFSG.
!       RATOUT is the Accumulator for total flow out of flow field 
!          to riparian ET.
!       HH is the hydraulic head in a cell
!       ETR is the ET rate for the plant functional subgroup within a
!         polygon for a cell.
!       LC,LP,NTS and II are counters
!     *************************************************************************
!
!     Specifications:
!     *************************************************************************
      Use GLOBAL,       Only:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      Use GWFBASMODULE, Only:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM 
      USE GWFRIPMODULE, Only:MAXRIP,MAXPOLY,MAXTS,NRIPCL,IRIPCB,IRIPCB1, &
                            C1,C2,CLoc,NPoly,HSurf,CovPFSG,RIPNM,RIPET,RIPBUD
      Character(16)                 :: TEXT,RIPAUX=''
      Integer                       :: KSTP,KPER,IGRID,NAUX=0
      Integer                       :: IL,IC,IR,LC,LP,NTS,II,IBD,NP
      Real                          :: Zero,RATE,Sum
      Real, Dimension(1)            :: AUXVAL
      Real, Dimension(MAXTS)        :: TotPFSG
      Double Precision              :: RATOUT,HH
      Double Precision, Dimension(MAXRIP,MAXPOLY,MAXTS) :: ETR
      INTEGER:: IG, IDX
!
      !DATA TEXT /'     RIPARIAN_ET'/
!     *************************************************************************
!
!1----Set pointers for current grid.
!
      Call SGWF2RIP4PNT(IGRID)
!
!2----Initialize the rate accumulator (RATOUT)
!
      Zero=0.0
      !Ratout=zero
!
!3----Set cell-by-cell budget save flag (IBD) and clear buffer
!
      IBD=0 
      If(IRIPCB < 0 .and. ICBCFL /= 0) IBD=-1
      If(IRIPCB > 0) IBD=ICBCFL
      !
      GROUPS: DO IG=1, RIPBUD%NGRP
        !
        RATOUT=ZERO
        TEXT = RIPBUD%GRP(IG)
        TEXT = ADJUSTR(TEXT)
!
!4----If cell-by-cell flows will be saved as a list, Write header.
!
      IF(IBD==2) THEN         
        CALL UBDSV4(KSTP,KPER,TEXT,NAUX,RIPAUX,IRIPCB,NCOL,NROW,NLAY,  &
              RIPBUD%DIM(IG),IOUT,DELT,PERTIM,TOTIM,IBOUND)  ! NRIPCL
      END IF
!5----Initialize BUFF
        BUFF = zero
!
!5----Loop through each riparian cell if NRIPCL > 0.  
!       
     !If(NRIPCL == 0) CYCLE GROUPS!Return
     !Cell_Loop: Do LC = 1, NRIPCL                      !seb named do loop
      Cell_Loop: DO IDX=1, RIPBUD%DIM(IG)  ! 
           LC = RIPBUD%INDEX(IG,IDX)
!
!6----Get row, column, layer, and surface elevation,
!
        IL=CLoc(LC,1)
        IR=CLoc(LC,2)
        IC=CLoc(LC,3)
!
! seb SKIP CELLS THAT ARE INACTIVE 
        If(IBOUND(IC,IR,IL) == 0) Then
            ETR(LC,:,:)=0.0
            RIPET(LC)=0.0
            RATE=0.0
            IF(IBD==2) CALL UBDSVB(IRIPCB,NCOL,NROW,IC,IR,IL,RATE, &
                                   AUXVAL,1,0,1,IBOUND,NLAY)
            Cycle Cell_Loop
        End if
!
!7----Set HH=HNEW, and initialize RATE to zero 
!
        HH=HNEW(IC,IR,IL)
        RATE=zero
!
!8----Loop through the polygons
!
        Do LP=1,NPoly(LC)
!
!9----Loop through plant functional subgroups, calculate ETR and 
!     aggregate ETR to RATE
!
          Do NTS=1,MAXTS
            ETR(LC,LP,NTS)=C1(LC,LP,NTS)*HH+C2(LC,LP,NTS)
            RATE=RATE+ETR(LC,LP,NTS)
          End do
        End do  !Poly loop
!
!10---Load RATE to budget accumulator and to BUFF and RIPET
!
        RATOUT=RATOUT-RATE
        BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
        RIPET(LC)=RATE  
!
!11---If saving cell-by-cell flows to list, Write flow.
!
        IF(IBD==2) CALL UBDSVB(IRIPCB,NCOL,NROW,IC,IR,IL,RATE, &
                        AUXVAL,1,0,1,IBOUND,NLAY)
!
      End do Cell_Loop
      !
!15---Check to see if cell-by-cell flows are to be saved as three-
!      dimensional array.
!
      If(IBD == 1 .AND. RIPBUD%DIM(IG) > 0) Then
        Call UBUDSV(KSTP,KPER,TEXT,IRIPCB,BUFF,NCOL,NROW,NLAY,IOUT)     
      End if
      !
!17---Move total riparian loss into VBVL for printing in BAS1OT.
!
      VBVL(3,MSUM)=Zero
      VBVL(4,MSUM)=RATOUT
!
!18---Add riparian ET (riparian ET rate times time's step length)
!     to VBVL.
!
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
!
!19---Move budget term labels to VBNM for printing by module BAS1OT.
!
      VBNM(MSUM)=TEXT
!
!20---Increment Budget term counter.
!
      MSUM=MSUM+1
      !
      END DO GROUPS
!
!12---Calculate the total ET for each PFSG.
!
      Do NTS=1,MAXTS
        Sum=0.0
        Do LC=1,NRIPCL 
          Do LP=1,NPOLY(LC)
            Sum=Sum+ETR(LC,LP,NTS)
          End do
        End Do
        TotPFSG(NTS)=Sum
      End Do
!
!13---Print locations, polygon number, HSurf, ET rate by plant functional
!      groups, and total ET rate per cell, if requested. 
!
      If(IBD < 0)Then
        Write(IOUT,100) TEXT,KPER,KSTP
100     Format(1x,/,1x,A,'   PERIOD',I3,'   STEP',I3)
!
!14---Write out ET rates
!
        Write(IOUT,120)(RIPNM(II), II=1,MAXTS)
120     format(1x,/,1x,' RIPARIAN_ET',/                                        &
                    1x,'LAYER  ROW  COLUMN     HEAD       CELL      ',         &
                    'POLYGON   SURFACE                     CELL ET RATE BY ',  &
                    'PLANT FUNCTIONAL GROUP',/                                 &
                    1x,'                                  ET RATE   NUMBER',   &
                    '    ELEVATION       ',20(2x,A24),/)
        DO LC=1, NRIPCL
          IL=CLoc(LC,1)
          IR=CLoc(LC,2)
          IC=CLoc(LC,3)
          HH=HNEW(IC,IR,IL)
          DO LP=1,NPOLY(LC)
            If(LP.EQ.1) Then
              Write(IOUT,130) IL,IR,IC,HH,RIPET(LC)
130           Format(1x,I3,3x,I3,3x,I3,5x,F10.2,1x,F10.5)
              Write(IOUT,140) LP,HSURF(LC,LP),(ETR(LC,LP,II),II=1,MAXTS)
140           Format(1x,45x,I3,3x,F10.2,9x,20(2x,F10.5,14x))
            Else
              Write(IOUT,140) LP,HSURF(LC,LP),(ETR(LC,LP,II),II=1,MAXTS)
            End If
          End Do
        End Do
        Write(IOUT,150) (TotPFSG(II),II=1,MAXTS)
150     Format(/,1x,32x,'Total ET per PFSG',21x,20(2x,F10.5,14x))
      End If
!
!
!16---Check to see if cell location, polygon, head, surface elevation, and 
!     plant functional subgroup et rates are to be saved as a formatted file.
!
      If(IRIPCB1>0) Then
        Write(IRIPCB1,'(4I5)') NRIPCL,NROW,NCOL,NLAY
        Write(IRIPCB1,'(*(I3))') (NPOLY(I),I=1,NRIPCL)        
        DO LC=1,NRIPCL
          IL=CLoc(LC,1)
          IR=CLoc(LC,2)
          IC=CLoc(LC,3)
          NP=NPoly(LC)
          HH=HNEW(IC,IR,IL)
          DO LP=1,NP
            Write(IRIPCB1,160) IL,IR,IC,LP,HH,HSurf(LC,LP),    &
                               (ETR(LC,LP,II),II=1,MAXTS)
160         Format(4I5,2F10.2,25F10.5)
          End Do
        End Do
      End If
!
!21---Return
!
      Return
      End Subroutine GWF2RIP4BD
!
!     *************************************************************************
      Function HCOFtrm(HK,HK1,RK,RK1)
!     *************************************************************************
!     This function calculates the HCOF term for the riparian ET
!       HK = first of two consecutive vertices head values
!       HK1 = second vertex head value
!       RK = first of two consecutive vertices ET flux rate values
!       RK1 = second vertex ET flux rate value
!     *************************************************************************
      Double Precision:: HCOFtrm
      Double Precision:: HK,HK1,RK,RK1
!     *************************************************************************
!
      HCOFtrm=(RK1-RK)/(HK1-HK)
!
      End Function HCOFtrm
!
!
!     *************************************************************************
      Function RHStrm(HK,HK1,RK,RK1)
!     *************************************************************************
!     This function calculates the RHS term for the riparian ET
!       HK = first of two consecutive vertices head values
!       HK1 = second vertex head value
!       RK = first of two consecutive vertices ET flux rate values
!       RK1 = second vertex ET flux rate value
!     *************************************************************************
      Double Precision:: RHStrm
      Double Precision::HK,HK1,RK,RK1
!     *************************************************************************
!
      RHStrm=(RK*HK1-RK1*HK)/(HK1-HK)
!
      End Function RHStrm
!
!
!     *************************************************************************
      Subroutine GWF2RIP4DA(IGRID)
!     *************************************************************************
!     Deallocate RIP memory
!     *************************************************************************
      USE GWFRIPMODULE 
!     *************************************************************************
!
      DEALLOCATE(GWFRIPDAT(IGRID)%MAXRIP)
      DEALLOCATE(GWFRIPDAT(IGRID)%MAXPOLY)
      DEALLOCATE(GWFRIPDAT(IGRID)%IRIPCB) 
      DEALLOCATE(GWFRIPDAT(IGRID)%IRIPCB1)
      DEALLOCATE(GWFRIPDAT(IGRID)%NRIPCL)
      DEALLOCATE(GWFRIPDAT(IGRID)%MAXTS)
      DEALLOCATE(GWFRIPDAT(IGRID)%MXSEG)
      DEALLOCATE(GWFRIPDAT(IGRID)%RIPNM)
      DEALLOCATE(GWFRIPDAT(IGRID)%NUSEG)
      DEALLOCATE(GWFRIPDAT(IGRID)%SXD)
      DEALLOCATE(GWFRIPDAT(IGRID)%ARD)
      DEALLOCATE(GWFRIPDAT(IGRID)%RMAX)
      DEALLOCATE(GWFRIPDAT(IGRID)%RSXD)
      DEALLOCATE(GWFRIPDAT(IGRID)%FDH)
      DEALLOCATE(GWFRIPDAT(IGRID)%FDR)
      DEALLOCATE(GWFRIPDAT(IGRID)%CLOC)
      DEALLOCATE(GWFRIPDAT(IGRID)%NPOLY)
      DEALLOCATE(GWFRIPDAT(IGRID)%HSURF)
      DEALLOCATE(GWFRIPDAT(IGRID)%COVPFSG)
      DEALLOCATE(GWFRIPDAT(IGRID)%C1)
      DEALLOCATE(GWFRIPDAT(IGRID)%C2)
      DEALLOCATE(GWFRIPDAT(IGRID)%RIPET)
      ! GFORTRAN compiler error work-around for pointer data type FINAL statement
      RIPBUD=>GWFRIPDAT(IGRID)%RIPBUD
      GWFRIPDAT(IGRID)%RIPBUD=>NULL()
      DEALLOCATE(RIPBUD)
      RIPBUD=>NULL()
      !
      !DEALLOCATE(GWFRIPDAT(IGRID)%RIPBUD)
!
! NULLIFY LOCAL POINTERS
      IF(IGRID.EQ.1)THEN
        MAXRIP =>NULL()
        MAXPOLY=>NULL()
        IRIPCB =>NULL()
        IRIPCB1=>NULL()
        NRIPCL =>NULL()
        MAXTS  =>NULL()
        MXSEG  =>NULL()
        RIPNM  =>NULL()
        NuSeg  =>NULL()
        Sxd    =>NULL()
        Ard    =>NULL()
        Rmax   =>NULL()
        Rsxd   =>NULL()
        fdh    =>NULL()
        fdR    =>NULL()
        CLoc   =>NULL()
        NPoly  =>NULL()
        HSurf  =>NULL()
        CovPFSG=>NULL()
        C1     =>NULL()
        C2     =>NULL()
        RIPET  =>NULL()
      RIPBUD  => NULL()
      END IF
!
      Return
!
      End Subroutine GWF2RIP4DA
!
!
!     *************************************************************************
      Subroutine SGWF2RIP4PNT(IGRID)
!     *************************************************************************
!     Set pointers to RIP data for grid.
!     *************************************************************************
      USE GWFRIPMODULE 
!     *************************************************************************
!    
!1----MAXRIP,MAXPOLY,IRIPCB,IRIPCB1,NRIPCL,MAXTS,MXSEG
!
      MAXRIP  => GWFRIPDAT(IGRID)%MAXRIP  
      MAXPOLY => GWFRIPDAT(IGRID)%MAXPOLY 
      IRIPCB  => GWFRIPDAT(IGRID)%IRIPCB  
      IRIPCB1 => GWFRIPDAT(IGRID)%IRIPCB1 
      NRIPCL  => GWFRIPDAT(IGRID)%NRIPCL   
      MAXTS   => GWFRIPDAT(IGRID)%MAXTS   
      MXSEG   => GWFRIPDAT(IGRID)%MXSEG    
!
!2----RIPNM,NuSeg,Sxd,Ard,Rmax,Rsxd,fdh,fdr,CLoc,NPoly,HSurf,CovPFSG,RIPET,C1,C2   
!
      RIPNM   => GWFRIPDAT(IGRID)%RIPNM    
      NuSeg   => GWFRIPDAT(IGRID)%NuSeg    
      Sxd     => GWFRIPDAT(IGRID)%Sxd     
      Ard     => GWFRIPDAT(IGRID)%Ard     
      Rmax    => GWFRIPDAT(IGRID)%Rmax   
      Rsxd    => GWFRIPDAT(IGRID)%Rsxd     
      fdh     => GWFRIPDAT(IGRID)%fdh     
      fdR     => GWFRIPDAT(IGRID)%fdR 
      CLoc    => GWFRIPDAT(IGRID)%CLoc
      NPoly   => GWFRIPDAT(IGRID)%NPoly
      HSurf   => GWFRIPDAT(IGRID)%HSurf
      CovPFSG => GWFRIPDAT(IGRID)%CovPFSG
      RIPET   => GWFRIPDAT(IGRID)%RIPET      
      C1      => GWFRIPDAT(IGRID)%C1      
      C2      => GWFRIPDAT(IGRID)%C2  
      RIPBUD  => GWFRIPDAT(IGRID)%RIPBUD  
!
!3----Return
!
      Return
!     
      End Subroutine SGWF2RIP4PNT      
      
!     *************************************************************************
      Subroutine SGWF2RIP4PSV(IGRID)      
!     *************************************************************************
!     Save pointers to RIP data for grid.
!     *************************************************************************
      USE GWFRIPMODULE 
!     *************************************************************************
!    
!1----MAXRIP,MAXPOLY,IRIPCB,IRIPCB1,NRIPCL,MAXTS,MXSEG
!
      GWFRIPDAT(IGRID)%MAXRIP  => MAXRIP
      GWFRIPDAT(IGRID)%MAXPOLY => MAXPOLY
      GWFRIPDAT(IGRID)%IRIPCB  => IRIPCB
      GWFRIPDAT(IGRID)%IRIPCB1 => IRIPCB1
      GWFRIPDAT(IGRID)%NRIPCL  => NRIPCL
      GWFRIPDAT(IGRID)%MAXTS   => MAXTS
      GWFRIPDAT(IGRID)%MXSEG   => MXSEG
!
!2----RIPNM,NuSeg,Sxd,Ard,Rmax,Rsxd,fdh,fdr,RIP,C1,C2   
!
      GWFRIPDAT(IGRID)%RIPNM   => RIPNM
      GWFRIPDAT(IGRID)%NuSeg   => NuSeg
      GWFRIPDAT(IGRID)%Sxd     => Sxd
      GWFRIPDAT(IGRID)%Ard     => Ard
      GWFRIPDAT(IGRID)%Rmax    => Rmax
      GWFRIPDAT(IGRID)%Rsxd    => Rsxd
      GWFRIPDAT(IGRID)%fdh     => fdh
      GWFRIPDAT(IGRID)%fdR     => fdR
      GWFRIPDAT(IGRID)%CLoc    => CLoc
      GWFRIPDAT(IGRID)%NPoly   => NPoly
      GWFRIPDAT(IGRID)%HSurf   => HSurf
      GWFRIPDAT(IGRID)%CovPFSG => CovPFSG
      GWFRIPDAT(IGRID)%RIPET   => RIPET
      GWFRIPDAT(IGRID)%C1      => C1
      GWFRIPDAT(IGRID)%C2      => C2
      GWFRIPDAT(IGRID)%RIPBUD  => RIPBUD
      
!
!3----Return
!
      Return
!     
      End Subroutine SGWF2RIP4PSV  