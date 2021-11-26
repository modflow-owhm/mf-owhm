
!
!-------SUBROUTINE GWF2NWT1AR
!
      SUBROUTINE GWF2NWT1AR(In, Mxiter, Iunitlak, ILGR, Igrid)
      USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
      USE CONSTANTS,                    ONLY:BLNK,BLN,NL,FALSE,TRUE,
     +                                       Z,ONE,TWO,DZ,UNO,inf,D100
      USE NUM2STR_INTERFACE,            ONLY: NUM2STR
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD,
     1                     NCNFBD,IBOUND,BUFF,BOTM,NBOTM,DELR,DELC,IOUT,
     2                     LBOTM,HNEW, IUNIT, GSE
      USE GWFBASMODULE,             ONLY: STOPER !HDRY,
      USE GWFNWTMODULE
      USE XMDMODULE,                ONLY: XMDPSV
      USE GMRESMODULE,              ONLY: GMRESPSV
      USE ERROR_INTERFACE,          ONLY: WARNING_MESSAGE, STOP_ERROR
      USE FILE_IO_INTERFACE,        ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE,     ONLY: PARSE_WORD_UP
      USE STRINGS,                  ONLY: GET_NUMBER, GET_INTEGER 
      USE ULOAD_AND_SFAC_INTERFACE, ONLY: ULOAD
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      EXTERNAL URDCOM, URWORD
      EXTERNAL SGWF2NWT1PSV
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER In, Igrid, Mxiter, Iunitlak, ILGR !, L, NRC
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER:: lloc, LLOCSAVE, istart, istop, i, ic, ir, il, jj
      CHARACTER(768):: line
      REAL:: r, toldum, ftoldum, thetadum, amomentdum
      REAL:: akappadum, gammadum, Breducdum, Btoldum, Thickdum!,ZERO
      INTEGER:: IANAME,KHANI!,N,KK,nc,nr,nl,j,k,NCNVRT,NHANI,NWETD
! Memory use variables
      INTEGER:: lrwrk,liwrk,NODES,MBLACK,NJAF
      REAL:: Memuse1,Memuse2
      DOUBLE PRECISION:: THK,MXTHCK                                     !seb ADDED VARIABLES
      LOGICAL:: THIN_CHECK, ADJUST_MXIter
      TYPE(WARNING_TYPE):: WRN
!     ------------------------------------------------------------------
!
!1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2NWT1PNT(IGRID)
      CALL SGWF2UPW1PNT(IGRID)  !seb lgr
      CALL SGWF2BAS7PNT(IGRID)  !seb lgr
      CALL SGWF2HFB7PNT(IGRID)  !seb lgr
      !
      CALL WRN%INIT()
      !
      ALLOCATE (Tol, Ftol, RMS2, RMS1, Iierr,IFDPARAM,ICNVGFLG)
      ALLOCATE (ITER1,THETA,THICKFACT,BTOL,Numtrack)
      ALLOCATE (RMSAVE,ADAMP,Breduc_Reset,HED_LIM,GSE_LIM(1,1))
      ALLOCATE (Numnonzero, Numactive, Numcell, II, iBtrak)
      ALLOCATE (Akappa,Gamma,Amomentum,Btrack,Breduc)
      ALLOCATE (Nonmeth, Linmeth, IPRNWT, Itreal, Ibt)
      ALLOCATE (IBOTAV)
      ALLOCATE (itertot)
!1------IDENTIFY PACKAGE AND INITIALIZE.
        WRITE (Iout, 9001) In
 9001 FORMAT (1X, /' NWT1 -- Newton Solver, ',
     +    'VERSION OWHM', /, 9X, 'INPUT READ FROM UNIT',
     +        I5,/)
      i         = ONE
      Itreal    = Z
      Ibt       = Z
      ADAMP     = UNO
      RMS2      = DZ
      RMS1      = DZ
      RMSAVE    = DZ
      Numactive = Z
      Numcell   = Z
      akappadum = DZ   
      toldum    = 1.0e-4
      ftoldum   = 100.0
      Mxiter    = 500
      Thickdum  = 1.0e-4
      Linmeth   = TWO      ! Linmeth=1 GMRES; Linmeth=2 XMD; Linmeth=3 SAMG 
      IPRNWT    = ONE      ! Iteration stats (>0 prints)
      IBOTAV    = ONE      ! Doesn't reset to bottom
      Numtrack  = Z
      Btoldum   = UNO
      Breducdum = UNO
      ICNVGFLG  = Z
      THIN_CHECK = FALSE !DEFAULT IS TO NOT DO A THIN CELL CHECK
      itertot    = Z
      HED_LIM       = IEEE_VALUE(THK, IEEE_QUIET_NAN)
      GSE_LIM(1,1)  = IEEE_VALUE(THK, IEEE_QUIET_NAN)
      ADJUST_MXIter = TRUE
      !
      CALL READ_TO_DATA(LINE,IN,IOUT,IOUT)
      DO
        lloc = ONE
        CALL PARSE_WORD_UP(line, lloc, istart, istop)    !Check for these keywords at start of solver input. Note MAX_HEAD_CHANGE can also appear in the normal options line
        !
        SELECT CASE(line(istart:istop))
        CASE('THIN_CELL_CHECK')
           THIN_CHECK = TRUE
           WRITE(IOUT,'(A)')
     1               ' THIN CELL CHECK THAT SETS IBOUND=0 IS ENABLED'
        CASE('MAX_HEAD_CHANGE')
            CALL GET_NUMBER(line, lloc, istart, istop,IOUT,IN,HED_LIM,
     1      MSG='"MAX_HEAD_CHANGE" failed to read the HED_LIM value.')
            IF(HED_LIM <=   0D0 ) HED_LIM=IEEE_VALUE(THK,IEEE_QUIET_NAN)
            IF(HED_LIM >= 0.9D30) HED_LIM=IEEE_VALUE(THK,IEEE_QUIET_NAN)
            IF(HED_LIM <  0.5D0 ) HED_LIM=0.5D0
        CASE('HEAD_DISTANCE_ABOVE_GSE_LIMIT')
            DEALLOCATE(GSE_LIM)
            ALLOCATE(GSE_LIM(NCOL,NROW))
            !
            jj = Z
            CALL ULOAD(GSE_LIM, LLOC, line, IOUT, IN, jj, MSG=
     +         '"HEAD_DISTANCE_ABOVE_GSE_LIMIT" failed to read the '//
     +         'NROW by NCOL array of GSE_LIM values.')
        CASE('KEEP_MXITER')
            ADJUST_MXIter = FALSE
        CASE('SIMPLE')
            IFDPARAM=1
            WRITE(IOUT,21)
        CASE('MODERATE')
            IFDPARAM=2
            WRITE(IOUT,23)
        CASE('COMPLEX')
            IFDPARAM=3
            WRITE(IOUT,25)
        CASE('SPECIFIED')
            IFDPARAM=4
            WRITE(IOUT,26)
            CALL READ_SPECIFIED_PROPERTIES(LINE,LLOC,IN,IOUT)
        CASE('CONTINUE', 
     +       'NO_CONVERGENCE_STOP', 'NO_FAILED_CONVERGENCE_STOP')
           ICNVGFLG = 1
           IF( STOPER>0.0) THEN
               CALL WARNING_MESSAGE('',0,IOUT,
     +              'NWT "CONTINUE" KEYWORD FOUND,'//NL//
     +              'IT WILL OVERRIDE BAS OPTION "STOPERROR"'//NL//
     +              'AND SIMULATION WILL ALWAYS CONTINUE DESPITE '//
     +              'FAILED CONVERGENCE'//NL, TRUE)
           END IF
           STOPER = 1E30
        CASE DEFAULT
          EXIT
        END SELECT
        CALL READ_TO_DATA(LINE,IN,IOUT)
      END DO
      !
      lloc = ONE
      CALL URWORD(line, lloc, istart, istop, 3, i, toldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, ftoldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Mxiter, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, Thickdum, Iout, In)
!      CALL URWORD(line, lloc, istart, istop, 2, Nonmeth, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Linmeth, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IPRNWT, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IBOTAV, r, Iout, In)
! seb ADDED CHECK FOR USING XMD AND LGR
      IF(Linmeth.EQ.2 .AND. ILGR.NE.0) CALL STOP_ERROR(line, In, Iout, 
     + MSG='NWT-LGR ERROR: THE NWT SOLVER PACKAGE USING THE XMD '//
     + '(LINEMETH=2) SOLVER DOES NOT WORK WITH LGR.'//NL//
     + 'PLEASE CHANGE THE SOLVER TO GMRES (LINEMETH=1) TO CONTINUE.')
      !
      NJAF = lloc   ! Temp backup of the start of the options part of the line
C
C3B-----GET OPTIONS.
      
   21 FORMAT(1X,'SIMPLE OPTION:',/,
     1     1X,'DEFAULT SOLVER INPUT VALUES REFLECT NEARLY LINEAR MODEL')
   23 FORMAT(1X,'MODERATE OPTION:',/,1X,'DEFAULT SOLVER',
     1         ' INPUT VALUES REFLECT MODERETELY NONLINEAR MODEL')
   25 FORMAT(1X,'COMPLEX OPTION:',/,1X,'DEFAULT SOLVER',
     1 ' INPUT VALUES REFLECT STRONGLY NONLINEAR MODEL')
   26 FORMAT(1X,'SPECIFIED OPTION:',/,1X,'SOLVER INPUT',
     1 ' VALUES ARE SPECIFIED BY USER')
      
      IFDPARAM = Z
      DO
        LLOCSAVE = LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        SELECT CASE(LINE(ISTART:ISTOP))
        CASE('SIMPLE')
           IFDPARAM=1
           WRITE(IOUT,21)
        CASE('MODERATE')
           IFDPARAM=2
           WRITE(IOUT,23)
        CASE('COMPLEX')
           IFDPARAM=3
           WRITE(IOUT,25)
        CASE('SPECIFIED')
           IFDPARAM=4
           WRITE(IOUT,26)
           CALL READ_SPECIFIED_PROPERTIES(LINE,LLOC,IN,IOUT)
        CASE('CONTINUE', 
     +       'NO_CONVERGENCE_STOP', 'NO_FAILED_CONVERGENCE_STOP')
           ICNVGFLG = 1
           IF( STOPER>0.0) THEN
               CALL WARNING_MESSAGE('',0,IOUT,
     +              'NWT "CONTINUE" KEYWORD FOUND,'//NL//
     +              'IT WILL OVERRIDE BAS OPTION "STOPERROR"'//NL//
     +              'AND SIMULATION WILL ALWAYS CONTINUE DESPITE '//
     +              'FAILED CONVERGENCE'//NL, TRUE)
           END IF
           STOPER = 1E30
        CASE('THIN_CELL_CHECK')
           THIN_CHECK = TRUE
           WRITE(IOUT,'(A)')
     1               ' THIN CELL CHECK THAT SETS IBOUND=0 IS ENABLED'
           !
        CASE('MAX_HEAD_CHANGE')
           CALL GET_NUMBER(line, lloc, istart, istop,IOUT,IN,HED_LIM,
     1     MSG='"MAX_HEAD_CHANGE" failed to read the HED_LIM value.')
           IF(HED_LIM <=   0D0 ) HED_LIM=IEEE_VALUE(THK,IEEE_QUIET_NAN)
           IF(HED_LIM >= 0.9D30) HED_LIM=IEEE_VALUE(THK,IEEE_QUIET_NAN)
           IF(HED_LIM <  0.5D0 ) HED_LIM=0.5D0
           !
        CASE('KEEP_MXITER')
           ADJUST_MXIter = FALSE
        CASE DEFAULT
                    LLOC = LLOCSAVE
                    EXIT
        END SELECT
      END DO
!
! Don't need to read these when using default Options.
      IF ( IFDPARAM.EQ.4 ) THEN
        IF(Mxiter < 500 .AND. ADJUST_MXIter)  THEN
           Mxiter = 500
           CALL ITER_WARN(LINE, IN, IOUT, 'SPECIFIED', '500', NJAF)
        END IF
        !
        !CALL URWORD(line,lloc,istart,istop, 3, i, thetadum,  Iout,In)  -- Replaced by "CALL READ_SPECIFIED_PROPERTIES(LINE,LLOC,IN,IOUT)"
        !CALL URWORD(line,lloc,istart,istop, 3, i, akappadum, Iout,In)
        !CALL URWORD(line,lloc,istart,istop, 3, i, gammadum,  Iout,In)
        !CALL URWORD(line,lloc,istart,istop, 3, i, amomentdum,Iout,In)
        !CALL URWORD(line,lloc,istart,istop, 2, Btrack, r, Iout,In)
        !IF ( BTRACK.GT.0 .AND. ILGR.EQ.0) THEN
        !   CALL URWORD(line,lloc,istart,istop, 2, Numtrack, r, Iout,In)
        !   CALL URWORD(line,lloc,istart,istop, 3, i, Btoldum,  Iout,In)
        !   CALL URWORD(line,lloc,istart,istop, 3, i, Breducdum,Iout,In)
        !END IF
      ELSEIF ( IFDPARAM.EQ.1 ) THEN  ! SIMPLE
        IF(Mxiter < 200 .AND. ADJUST_MXIter) THEN
           Mxiter = 200
           CALL ITER_WARN(LINE, IN, IOUT, 'SIMPLE', '200', NJAF)
        END IF
        !
        thetadum   = 0.97
        akappadum  = 0.0001
        gammadum   = 0.0
        amomentdum = 0.0
        Btrack     = 0
        Numtrack   = 20
        Btoldum    = 1.5
        Breducdum  = 0.97
      ELSEIF ( IFDPARAM.EQ.2 ) THEN  ! MODERATE
        IF(Mxiter < 300 .AND. ADJUST_MXIter)  THEN
           Mxiter = 300
           CALL ITER_WARN(LINE, IN, IOUT, 'MODERATE', '300', NJAF)
        END IF
        !
        thetadum   = 0.90
        akappadum  = 0.0005 !0.00001
        gammadum   = 0.00
        amomentdum = 0.1
        Btrack     = 1 !0
        Numtrack   = 20
        Btoldum    = 1.1
        Breducdum  = 0.9
      ELSEIF ( IFDPARAM.EQ.3 ) THEN  ! COMPLEX
        IF(Mxiter < 500 .AND. ADJUST_MXIter)  THEN
           Mxiter = 500
           CALL ITER_WARN(LINE, IN, IOUT, 'COMPLEX', '500', NJAF)
        END IF
        !
        thetadum   = 0.85
        akappadum  = 0.005 !0.00001  0.0005
        gammadum   = 0.0
        amomentdum = 0.1
        Btrack     = 1
        Numtrack   = 50
        Btoldum    = 1.1
        Breducdum  = 0.7
      ELSE
        CALL STOP_ERROR(line, In, Iout, MSG=
     + 'NWT SOLVER FAILED TO IDENTIFY NWT CONFIG OPTION.'//BLN//
     + 'It must be one of the following: '//NL//
     + 'SIMPLE, MODERATE, COMPLEX, or SPECIFIED')
      END IF
 !     
 !     IF ( Nonmeth==1 )Then
 !       Write(iout,*) '***Newton Linearization will be used***'
 !       Write(iout,*)
 !     ELSEIF ( Nonmeth==0 )Then
 !       Write(iout,*) '***Picard Linearization will be used***'
 !       Write(iout,*)
 !     ELSE
 !       Write(iout,*) '***Incorrect value for variable Nonmeth was ',
 !    +                'specified. Check input.***'
 !       Write(iout,*)
 !       Call USTOP('  ')
 !     END IF
      Nonmeth = 1
!
      IF ( Linmeth==1 )Then
        Write(iout,*) '***GMRES linear solver will be used***'
        Write(iout,*)
      ELSEIF ( Linmeth==2 )Then
        Write(iout,*) '***XMD linear solver will be used***'
        Write(iout,*)
      !ELSEIF ( Linmeth==3 )Then
      ! Write(iout,*) '***SAMG linear solver will be used***'
      ! Write(iout,*)
      ELSE
        CALL STOP_ERROR( line, In, Iout, MSG=
     + 'NWT SOLVER HAS INCORRECT VALUE FOR LINEAR SOLUTION METHOD.'//
     +  NL//'LINMETH can only be set to 1 or 2,'//NL//
     +  'but it was set to '//NUM2STR(Linmeth) )
      END IF
!
      Thickfact = Thickdum
      Btol      = Btoldum
      Breduc    = Breducdum
      Theta     = Thetadum
      Akappa    = akappadum
      Gamma     = gammadum
      Amomentum = amomentdum
      IF ( Theta.LT.CLOSEZERO ) Theta = 0.9D0
      Tol = toldum
      Ftol = ftoldum
      Breduc_Reset = (0.25D0*Breduc + 0.75D0)/(Breduc*Breduc)  !Applies to ADAMP to shift backtracking to remove previous backtrack and shift new value by 25% from previous to old backtrack..that is h1 = B*H0;  H2 = BR*B*H1 = 0.75*H0 + 0.25*H1  
!2--ECHO NWT INPUT
        WRITE(IOUT,9010) Tol,Ftol,MXITER
        WRITE(IOUT,9011) THETA,AKAPPA,GAMMADUM,AMOMENTUM 
 9010 FORMAT(1X,'  CONVERGENCE CRITERION OF',E15.6,' FOR HEAD SOLUTION',
     +      /1X,'  AND A TOLERANCE OF',E15.6,' FOR FLOW SOLUTION AND ',
     +      /1X,'  A MAXIMUM OF ',I5,' OUTER ITERATIONS. ',//)
 9011 FORMAT(1X,'  D-B-D REDUCTION FACTOR OF ',E15.6,' AND ',
     +      /1X,'  A D-B-D INCREASE FACTOR OF ',E15.6,' AND ',
     +      /1X,'  A D-B-D RELAXATION OF ',E15.6,' AND ', 
     +      /1X,'  A MOMENTUM FACTOR OF ',E15.6,' .',//)
      IF ( BTRACK.GT.0 .AND.ILGR.NE.0) THEN
          WRITE(IOUT,'(/ 1x,2A /)')'***BACKTRACKING IS AUTOMATICALLY ',
     +                             'DISABLED WHEN USING NWT WITH LGR***'
                                   BTRACK = 0
      ELSEIF ( BTRACK.GT.0 ) THEN 
          WRITE(IOUT,9012) Numtrack,BTOL,BREDUC
      ELSE
          WRITE(IOUT,*)'***BACKTRACKING IS INACTIVE***'
      END IF
 9012 FORMAT(1X,'  BACKTRACKING IS ACTIVE ',
     +      /1X,'  THE MAXIMUM NUMBER OF BACKTRACKS IS ',I5,' AND ',
     +      /1X,'  THE BACKTRACKING TOLERANCE IS ',E15.6, ' AND',
     +      /1X,'  THE BACKTRACKING REDUCTION FACTOR IS ',E15.6,/)
!
!3-----ALLOCATE SPACE
      
      ALLOCATE (Icell(Ncol, Nrow, Nlay))
      ALLOCATE (Diag(3,Ncol*Nrow*Nlay), Hiter(Ncol, Nrow, Nlay))
      Numnonzero = Z
      II     = Z
      iBtrak = Z
      ICELL  = Z
      DIAG   = Z
 ! Check heads and set to be above bottom.
      IF(THIN_CHECK) THEN
      !
      MXTHCK=MAXVAL(
     +      DBLE( BOTM(:,:,LBOTM(1:NLAY)-1) - BOTM(:,:,LBOTM(1:NLAY)) ),!seb THIS COULD BE MADE AS A LAYER BY LAYER CHECK
     +      MASK=IBOUND(:,:,:).NE.0 )      
      I = Z
      IF(THICKFACT < 0.01) THEN
          Thickdum = THICKFACT
      ELSE
          Thickdum = 0.01
      END IF
      !
      DO IL = 1, NLAY                                                   !seb FIX SEARCH TO NOT NEGATE  CELLS
      DO IR = 1, NROW
      DO IC = 1, NCOL
       IF ( IBOUND(IC,IR,IL).GT.0 ) THEN
        THK=DBLE( BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL)) )
        IF (THK .LT. Thickdum * MXTHCK ) THEN                          !THIS MAY NEED TO BE MULTIPLIED BY 10 TO MAKE MORE SENSITIVE
!           IF(I.EQ.0) THEN                                !WRITE OUT WARNING HEADER
!               WRITE(IOUT,'(/ A )') REPEAT('#',35)//
!     +    '     NWT/UPW  WARNING    ' // REPEAT('#',80)//NEW_LINE(' ')//
!     +                 REPEAT('#',140)// NEW_LINE(' ') //REPEAT('#',140)
!               I=1
!           END IF
!           WRITE(IOUT,'(/ A,G12.5,A,3I5,A,G12.5,A,/A)') 
!     +       'WARNING: EXTREMELY THIN CELL WITH THICKNESS OF ',
!     +       THK,', FOR LAYER, ROW, COL: ',IL,IR,IC, 
!     +     ' WITH MAX TOLERANCE (BASED ON THICKFACT) OF, ',
!     +        Thickdum * MXTHCK,
!     +    ' ( THICKFACT * MAX MODEL LAYER THICKNESS ) ',
!     +       '***CHECK INPUT, SETTING IBOUND = 0 FOR THAT MODEL CELL***'
           CALL WRN%ADD(NUM2STR(IL,6)//BLNK//NUM2STR(IR,6)//BLNK//
     +                  NUM2STR(IC,6)//BLNK//NUM2STR(THK)//NL)
           !
           IBOUND(IC,IR,IL) = Z
        END IF    
 ! THESE NEXT THREE LINES COULD CAUSE SLOW CONVERGENCE WHEN USING A SOLUTION FOR IC.            
!              IF ( HNEW(IC,IR,IL).LT.BOTM(IC,IR,LBOTM(IL)) ) THEN
!                HNEW(IC,IR,IL) = BOTM(IC,IR,LBOTM(IL))+HEPS
!              END IF
       END IF
      END DO
      END DO
      END DO
      ELSE
      WRITE(IOUT,'(A,/A)')'NWT THIN CELL CHECK DISABLED',
     +                    'IBOUND ARRAY WILL REMAIN UNCHANGED'
      END IF!(THIN_CHECK)
      !
      IF(WRN%RAISED) CALL WRN%CHECK(
     +   'NWT SOLVER DETECTED THIN CELLS'//NL//
     +   'THE MINIMUM THICKNESS ALLOWED IS DETERMIEND FOR EACH '//NL//
     +   'LAYER AS THE PRODUCT OF THICKFACT ('//NUM2STR(Thickdum)//
     +   ') MULTIPLIED BY THE THICKEST CELL IN THE LAYER.'//BLN//
     +   'THIS CAN BE DISABLED AND THUS KEEP ALL MODEL CELLS'//NL//
     +   'WITH NWT SOLVER OPTION "ALLOW_THIN_CELL".'//BLN//
     +   'THE FOLLOWING ARE A LIST OF CELLS THAT HAD THEIR IBOUND '//
     +   'CHANGED TO ZERO DUE TO BEING TOO "THIN"'//NL//
     +   '[IBOUND(COL,ROW,LAY)=0]'//BLN//
     +   '   LAY    ROW    COL  THICKNESS',
     +   OUTPUT=IOUT)
      !
      DO CONCURRENT (IL=1:NLAY, IR=1:NROW, IC=1:NCOL)
          HITER(IC,IR,IL) = HNEW(IC,IR,IL)
      END DO
      !
 !  Determine the number of active cells and then numnber of elements
 !  in linear matrix for allocating arrays.
      CALL ORDERCELL()
C seb Build HFB ORDERCELL INDEX
      IF(IUNIT(21).NE.0) CALL ORDERCELLHFB()
      CALL COUNTACTIVE(jj)
      IF ( Numactive.LT.2 ) THEN
        CALL STOP_ERROR( '', In, Iout, MSG=
     + 'NWT SOLVER DOES RUN WITH SINGLE-CELL MODELS.' )
      END IF
      Numnonzero = jj
      Numcell = Numactive
! Allocate global linear solver arrrays
! These are allocated exactly based on active cells
! and constant head cells for now. 
      NODES = NUMACTIVE
cmi
c     MBLACK = NODES
c     NJAF = 7 * Numnonzero
c     liwrk = 3*NODES + 3*mblack + njaf + 1    ! ldcomb = .false.
c     lrwrk = 7/2*mblack + 2*(14+1)*mblack + 14
cmi


      mblack = nodes
c     if (iredsys.eq.1) mblack = nodes * 0.5 + 1
c     njaf = 7 * nja
      njaf = 7 * numnonzero
c     liwrk = 3*nodes + 3*mblack + njaf + 1    ! ldcomb = .false.
c     if (ldcomb) liwrk = 3*nodes + 4*mblack + 2*njaf + 1  !        = .true.
      liwrk = 3*nodes + 4*mblack + 2*njaf + 1  !        = .true.

c     lrwrk = 3*mblack + 2*(north+1)*mblack + north
      lrwrk = 3*mblack + 2*(14+1)*mblack + 14

cmi
C
 !     memuse1 = 0
 !     memuse2 = 0
 !     memuse1 = (11*ncol*nrow*nlay+8*
 !    +           (Numnonzero+6*Numcell+6*Numcell)+
 !    +            4*((Numcell+1)+Numnonzero))
!      memuse1 = memuse1/1.0e9
 !     IF ( Linmeth==2 ) THEN
 !       memuse2 = 4
 !       memuse2 = 16*memuse2*NODES*7/1.0e9
 !     ELSE
 !       memuse2 = 8*(NJAF)/1.0e9
 !       memuse2 = memuse2 + 8*(LRWRK+NODES)/1.0e9
 !       memuse2 = memuse2 + 4*(NODES+LIWRK+30)/1.0e9
 !     END IF
!      Write(*,*) 'Gigabytes required for Nonlinear Solver= ',
!     +            memuse1
!      Write(*,*) 'Gigabytes required for Linear Solver= ',memuse2
!      Write(*,*) 'Total Gigabytes required for Newton Solver= ',
!     +               memuse1+memuse2
      ALLOCATE (A(Numnonzero), IA(Numcell+1))
      ALLOCATE (JA(Numnonzero))
      ALLOCATE (BB(Numcell), Hchange(Numcell))
      ALLOCATE (HCHOLD(Numcell),Wsave(Numcell))
      ALLOCATE (Dc(6,Numcell))
      A  = DZ
      Dc = DZ
      BB = DZ
      Hchange = DZ
      Hchold  = UNO
      Wsave   = DZ
      IA = Z
      JA = Z
!  
      ALLOCATE (Cvm1, Hvm1, Hvp1, Crm1, Hrm1, Hrp1, Ccm1)
      ALLOCATE (Hcm1, Hcp1, Ccc, Crr, Cvv, H)
      ALLOCATE (Hcoff, Rhss)
      ALLOCATE (W, Fhead, Fflux, Fheadsave, NJA)    
!
      W         = UNO
      Fhead     = DZ
      Fheadsave = DZ
      Fflux     = DZ
! Order cells for jacobian and create CRS pointers
      CALL FILLINDEX(jj)
      NJA = IA(Numactive+1) - 1
      IF ( Linmeth.EQ.1 ) THEN
        CALL GMRES7AR(IN,IGRID)
      ELSEIF ( Linmeth==2 ) THEN
        CALL XMD7AR(IN)
 !     ELSEIF ( Linmeth==3 ) THEN
!        CALL SAMG7AR(IN)
      END IF
!
!
C Allocage PCG if requested
!      CALL AR_NWT_PCG(MXITER,IGRID)
! 
!
!--INITIALIZE SOLVER ARRAYS
      Cvm1  = DZ
      Hvm1  = DZ
      Crm1  = DZ
      Hrm1  = DZ
      Hrp1  = DZ
      Ccm1  = DZ
      Hcm1  = DZ
      Hcp1  = DZ
      Ccc   = DZ
      Crr   = DZ
      Cvv   = DZ
      H     = DZ
      Hcoff = DZ
      Rhss  = DZ
!-------SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2NWT1PSV(Igrid)
      CALL GMRESPSV(IGRID)
      CALL XMDPSV(IGRID)
      !
      CONTAINS
         SUBROUTINE ITER_WARN(LINE, IN, IOUT, OPT, ITER, IOPT)
           IMPLICIT NONE
           CHARACTER(*), INTENT(IN):: LINE, OPT, ITER
           INTEGER,      INTENT(IN):: IN, IOUT, IOPT
           !
           CALL WARNING_MESSAGE(LINE=LINE, INFILE=In, OUTPUT=IOUT, MSG=
     +      'NWT solver with '//OPT//
     +      ' option requires MXITER >= '//ITER//NL//NL//
     +      'Setting: MXITER = '//ITER//NL//NL//
     +      'If you want to override this warning and use your '//
     +      'specified MXITER'//NL//'add the option KEEP_MXITER '//
     +      'before specifying '//OPT//NL//NL//
     +      'For example: '//NL//NL//
     +      LINE(:IOPT-1)//" KEEP_MXITER "//ADJUSTL(LINE(IOPT:)) )
           !
         END SUBROUTINE
         !
         SUBROUTINE READ_SPECIFIED_PROPERTIES(LINE,LOC,IN,IOUT)
           IMPLICIT NONE
           CHARACTER(*), INTENT(IN   ):: LINE
           INTEGER,      INTENT(INOUT):: LOC
           INTEGER,      INTENT(IN   ):: IN, IOUT
           INTEGER:: I,J
           CHARACTER(44):: ER
           CHARACTER(64):: E1, E2, E3, E4, E5, E6, E7, E8
           ER='Found option SPECIFIED, but failed to load: '    ! Lame way of doing error messages, but most compact method for fixed formatting
           E1 = ER // 'DBDTHETA'
           E2 = ER // 'DBDKAPPA'
           E3 = ER // 'DBDGAMMA'
           E4 = ER // 'MOMFACT'
           E5 = ER // 'BACKFLAG'
           E6 = ER // 'MAXBACKITER'
           E7 = ER // 'BACKTOL'
           E8 = ER // 'BACKREDUCE'
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,   thetadum, MSG=E1)  ! thetadum is pulled as global from parent subroutine, ditto for other dum variables
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,  akappadum, MSG=E2)
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,   gammadum, MSG=E3)
           CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN, amomentdum, MSG=E4)
           CALL GET_INTEGER(LINE,LOC,I,J,IOUT,IN,     Btrack, MSG=E5)
           !
           IF ( BTRACK > 0 .AND. ILGR == 0) THEN
              CALL GET_INTEGER(LINE,LOC,I,J,IOUT,IN,  Numtrack, MSG=E6)
              CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN,   Btoldum, MSG=E7)
              CALL GET_NUMBER (LINE,LOC,I,J,IOUT,IN, Breducdum, MSG=E8)
           END IF
           !
         END SUBROUTINE
         !
      END SUBROUTINE GWF2NWT1AR
      !
      SUBROUTINE SETUP_NWT_GSE_LIM()
      USE CONSTANTS,  ONLY:inf,D100
      USE GLOBAL,     ONLY:NCOL,NROW, GSE
      USE GWFNWTMODULE, ONLY: GSE_LIM
      IMPLICIT NONE
      INTEGER:: I,J
      !
      IF(GSE_LIM(1,1) == GSE_LIM(1,1)) THEN
            DO CONCURRENT(I=1:NCOL, J=1:NROW)
                IF(GSE_LIM(i,j) < 0.D0 .OR. GSE_LIM(i,j) >= 1D30) THEN
                    GSE_LIM(i,j) = inf
                ELSE
                    GSE_LIM(i,j) = GSE(i,j) + GSE_LIM(i,j)
                END IF
            END DO
      END IF
      END SUBROUTINE
!     -----------------------------------------------------------------
!
!1     SUBROUTINE TEMPFILLUN. SET SCALERS FOR UNCONFINED FLOW
      SUBROUTINE TEMPFILLUN(IC, IR, IL)
      USE CONSTANTS, ONLY: DZ
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Cv, Hnew, Cc, Cr, Ibound, Hcof,
     +    Rhs, Botm, Lbotm,Iout
      USE GWFNWTMODULE, ONLY:Cvm1,Hvp1,Hvm1,Crm1,Hrm1,Hrp1,Ccm1,Hcm1,
     +                       Hcp1,Ccc,Crr,Cvv,H,Closezero,Icell,Hcoff,
     +                       Rhss
      USE GWFUPWMODULE, ONLY:Sn
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IC, IR, IL
      !
      INTEGER:: ij, iBot, iTop
      DOUBLE PRECISION:: THICK, THK0, Sn0
!     -----------------------------------------------------------------
      !
      Hcoff = HCOF(IC,IR,IL)
      Rhss  = RHS (IC,IR,IL)
      H     = HNEW(IC,IR,IL)
      !
      Hvp1 = H
      Hvm1 = H
      Hrm1 = H
      Hrp1 = H
      Hcm1 = H
      Hcp1 = H
      Cvm1 = DZ
      Crm1 = DZ
      Ccm1 = DZ
      Ccc  = DZ
      Crr  = DZ
      Cvv  = DZ
      !
      iBot = LBOTM(IL)
      iTop = iBot - 1
      ij   = Icell(IC,IR,IL)
      Sn0  = Sn(ij)
      THK0 = BOTM(IC,IR,iTop) - BOTM(IC,IR,iBot)
      !
      IF( IL > 1 ) THEN
      IF( IBOUND(IC,IR,IL-1).NE.0 ) THEN
          !
          Hvm1 = Hnew(IC,IR,IL-1)
          Cvm1 =   Cv(IC,IR,IL-1)
      ENDIF
      ENDIF
      !
      IF( IR > 1 ) THEN
      IF( IBOUND(IC,IR-1,IL).NE.0 ) THEN
          !
          Hrm1 = Hnew(IC,IR-1,IL)
          IF( Hrm1-H > CLOSEZERO)THEN
              THICK = BOTM(IC,IR-1,iTop) - BOTM(IC,IR-1,iBot)
              ij   = Icell(IC,IR-1,IL)
              Ccm1 =    Cc(IC,IR-1,IL)*THICK*Sn(ij)
          ELSE
              Ccm1 =    Cc(IC,IR-1,IL)*THK0*Sn0
          END IF
      ENDIF
      ENDIF
      !
      IF( IC > 1 ) THEN
      IF( IBOUND(IC-1,IR,IL).NE.0 ) THEN
          !
          Hcm1 = Hnew(IC-1,IR,IL)
          IF( Hcm1-H > CLOSEZERO )THEN
              THICK = BOTM(IC-1,IR,iTop) - BOTM(IC-1,IR,iBot)
              ij   = Icell(IC-1,IR,IL)
              Crm1 =    Cr(IC-1,IR,IL)*THICK*Sn(ij)
          ELSE 
              Crm1 =    Cr(Ic-1,Ir,Il)*THK0*Sn0
          END IF
      ENDIF
      ENDIF
      !
      IF( IC < NCOL ) THEN
      IF( IBOUND(IC+1,IR,IL).NE.0 ) THEN
          !
          Hcp1 = Hnew(IC+1,IR,IL)
          IF( Hcp1-H > CLOSEZERO )THEN
              THICK = BOTM(IC+1,IR,iTop) - BOTM(IC+1,IR,iBot)
              ij  =  Icell(IC+1,IR,IL)
              Crr =     Cr(IC  ,IR,IL)*THICK*Sn(ij)
          ELSE 
              Crr =     Cr(IC  ,IR,IL)*THK0*Sn0
          END IF
      ENDIF
      ENDIF
      !
      IF( IR < NROW ) THEN
      IF( IBOUND(IC,IR+1,IL).NE.0 ) THEN
          !
          Hrp1 = Hnew(IC,IR+1,IL)
          IF( Hrp1-H > CLOSEZERO)THEN
              THICK = BOTM(IC,IR+1,iTop) - BOTM(IC,IR+1,iBot)
              ij  =  Icell(IC,IR+1,IL)
              Ccc =     Cc(IC,IR  ,IL)*THICK*Sn(ij)
          ELSE
              Ccc =     Cc(IC,IR  ,IL)*THK0*Sn0
          END IF
      ENDIF
      ENDIF
      !
      ! Need to correct the following calculations for case when CV is head dependent.
      IF( IL < NLAY ) THEN
      IF( IBOUND(IC,IR,IL+1).NE.0 ) THEN
          !
          Hvp1 = Hnew(IC,IR,IL+1)
          Cvv  =   Cv(IC,IR,IL)
      ENDIF
      ENDIF
      !
      END SUBROUTINE TEMPFILLUN
!
!     -----------------------------------------------------------------
!
!     SUBROUTINE TEMPFILLCON. SET SCALERS FOR CONFINED FLOW
      SUBROUTINE TEMPFILLCON(IC, IR, IL)
      USE CONSTANTS, ONLY: DZ
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Cv, Hnew, Cc, Cr, Ibound, Hcof,
     +    Rhs, Botm, Lbotm,Iout
      USE GWFNWTMODULE, ONLY:Cvm1,Hvp1,Hvm1,Crm1,Hrm1,Hrp1,Ccm1,Hcm1,
     +                       Hcp1,Ccc,Crr,Cvv,H,Closezero,Icell,Hcoff,
     +                       Rhss
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IC, IR, IL
      !
      Hcoff = Hcof(IC,IR,IL)
      Rhss  = Rhs (IC,IR,IL)
      H     = Hnew(IC,IR,IL)
      !
      Hvp1 = H
      Hvm1 = H
      Hrm1 = H
      Hrp1 = H
      Hcm1 = H
      Hcp1 = H
      Cvm1 = DZ
      Crm1 = DZ
      Ccm1 = DZ
      Ccc  = DZ
      Crr  = DZ
      Cvv  = DZ
      !
      IF( IL > 1 ) THEN
      IF(      IBOUND(IC,IR,IL-1).NE.0 ) THEN
          Hvm1 = Hnew(IC,IR,IL-1)
          Cvm1 =   Cv(IC,IR,IL-1)
      ENDIF
      ENDIF
      !
      IF ( IR > 1 ) THEN
        IF (   IBOUND(IC,IR-1,IL).NE.0 ) THEN
          Hrm1 = Hnew(IC,IR-1,IL)
          Ccm1 =   Cc(IC,IR-1,IL)
        END IF
      ENDIF
      !
      IF ( IC > 1 ) THEN
        IF (   IBOUND(IC-1,IR,IL).NE.0 ) THEN
          Hcm1 = Hnew(IC-1,IR,IL)
          Crm1 =   Cr(IC-1,IR,IL)
        END IF
      ENDIF
      !
      IF( IC < NCOL ) THEN
      IF(      IBOUND(IC+1,IR,IL).NE.0 ) THEN
          Hcp1 = Hnew(IC+1,IR,IL)
          Crr  =   Cr(IC  ,IR,IL)
      ENDIF
      ENDIF
      !
      IF( IR < NROW ) THEN
      IF(       IBOUND(IC,IR+1,IL).NE.0 ) THEN
           Hrp1 = Hnew(IC,IR+1,IL)
           Ccc  =   Cc(IC,IR  ,IL)
      ENDIF
      ENDIF
      !
      ! Need to correct the following calculations for case when CV is head dependent.
      IF( IL < NLAY ) THEN
      IF(      IBOUND(IC,IR,IL+1).NE.0 ) THEN
          Hvp1 = Hnew(IC,IR,IL+1)
          Cvv  =   Cv(IC,IR,IL)
      ENDIF
      ENDIF
      !
      END SUBROUTINE TEMPFILLCON
!
!     -----------------------------------------------------------------
!
      PURE FUNCTION DHORIZ(Hup, Ttop, Bbot, il)
! RETURNS DERIVATIVE OF HORIZONTAL CONDUCTANCE BASED ON SMOOTH FUNCTION
! FUNCTION IS CALCULATED IN UPW PACKAGE IN SUBROUTINE SAT_THICK
      USE CONSTANTS, ONLY: DZ, UNO
      USE GWFNWTMODULE
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ARGUMENTS
!     -----------------------------------------------------------------
      DOUBLE PRECISION, INTENT(IN):: Hup, Ttop, Bbot
      INTEGER, INTENT(IN):: il
      DOUBLE PRECISION DHORIZ
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION factor, x, s, v, cof1, cof2, EPS, ACOF, Y
      DOUBLE PRECISION EPSQD, z    
!     -----------------------------------------------------------------
      DHORIZ = DZ
      IF ( LAYTYPUPW(il) .LE. 0 .OR. Hup .GE. bbot) RETURN
C-------STRAIGHT LINE WITH PARABOLIC SMOOTHING
      EPS = Thickfact
      ACOF = UNO / (UNO - EPS)
      x = (Hup-bbot)/(TTOP-BBOT)
      IF ( x < 1.0d-9 )  x = 1.0d-9
      IF(X < EPS)THEN
        Y = ACOF * X / (EPS*(Ttop-Bbot))
      ELSEIF(X < UNO-EPS)THEN
        Y = ACOF /(Ttop-Bbot)
      ELSEIF(X < UNO)THEN
        X = UNO - X
        Y = - ACOF * x / (EPS * (Ttop - Bbot))
 !       Y = 1.0-Y
        Y = -Y
        !2-26-16. 1 should go away for derivative
      ELSE
        Y = DZ
      ENDIF
      factor = Y
      DHORIZ = factor     
      END FUNCTION DHORIZ
!
!     -----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DVERT(Hh,Ttop,Bbot)
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ******************************************************************
!     COMPUTE THE DERIVATIVE OF THE VERTICAL BRANCH CONDUCTANCE BETWEEN
!     A LAYER AND THE NEXT LOWER LAYER FROM VERTICAL HYDRAULIC 
!     CONDUCTIVITY.
!     ******************************************************************
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     -----------------------------------------------------------------
      DOUBLE PRECISION Hh,Ttop,Bbot
      INTEGER J, I, K, iltyp
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION factor, thick, zero, x
!     ------------------------------------------------------------------
!
      DVERT = 0.0D0
      RETURN   ! CV is constant as of now.  --seb changed all calls to DVERT to just set DV to zero
      zero = 0.0D0
      factor = 1.0D0
      thick = Thickfact*(ttop - bbot)
      x = Hh-bbot  
      factor = 1.0D0/thick
      IF ( x.LE.0.0D0 ) factor = 0.0D0
      IF ( x.GT.thick ) factor = 0.0D0
      DVERT = factor
      END FUNCTION DVERT
!
!     -----------------------------------------------------------------
!
      SUBROUTINE ORDERCELL()
! Order system for MODFLOW storage scheme by relating Row, Col, and Lay to
! Jacobian order
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Ibound, Iout, HNEW
      USE GWFBASMODULE, ONLY: HDRY
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER ic, ir, il, nrncnl, ij, jj, isum
!     -----------------------------------------------------------------
!
!ij points to a new row
!jj points to col. in sol. matrix
      ic = 1
      ir = 1
      il = 1
      ij = 1
      nrncnl = Nrow*Ncol*Nlay
      DO jj = 1, nrncnl
        IF ( ic.EQ.Ncol+1 ) THEN
          ic = 1
          ir = ir + 1
        ENDIF
        IF ( ir.EQ.Nrow+1 ) THEN
          ic = 1
          ir = 1
          il = il + 1
        ENDIF
        isum = 0
        IF ( Ibound(ic, ir, il).NE.0 ) THEN
          IF ( NCOL+NROW.LT.7 ) THEN  !RICH ADDED THIS seb
          IF ( IL.GT.1 ) isum = isum + abs(Ibound(ic, ir, il-1))
            IF ( IL.LT.NLAY ) isum = isum + abs(Ibound(ic, ir, il+1))
          END IF
          IF ( IR.GT.1 ) isum = isum + abs(Ibound(ic, ir-1, il))
          IF ( IC.GT.1 ) isum = isum + abs(Ibound(ic-1, ir, il))
          IF ( IR.LT.NROW ) isum = isum + abs(Ibound(ic, ir+1, il))
          IF ( IC.LT.NCOL ) isum = isum + abs(Ibound(ic+1, ir, il))
          IF ( isum.GT.0 ) THEN
            Diag(1, ij) = il
            Diag(2, ij) = ir
            Diag(3, ij) = ic
            Icell(ic, ir, il) = ij
            ij = ij + 1
          ELSE
              WRITE(IOUT,*)
              WRITE(IOUT,*)'**Active cell surrounded by inactive'//
     1          ' cells**'
              WRITE(IOUT,*)'**Resetting cell to inactive**'
              WRITE(IOUT,*)'ROW=',ir,'COL=',ic,'LAY=',il
              WRITE(IOUT,*)
            Ibound(ic, ir, il) = 0
            HNEW(ic,ir,il) = HDRY
          END IF
        ENDIF
        ic = ic + 1
      ENDDO
      Numactive = ij - 1
      RETURN
      END SUBROUTINE ORDERCELL
!
!     -----------------------------------------------------------------
      SUBROUTINE FILLINDEX(jj)
! Fill CRS pointers for MODFLOW storage scheme by relating Row, Col, and Lay to
! Jacobian order
      USE GWFNWTMODULE
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,NLAY,IBOUND
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER ic, ir, il, nrncnl, ij, jj
      INTEGER ILM, ILP, IRM, IRP, ICM, ICP, itemp
!     -----------------------------------------------------------------
      LOGICAL, EXTERNAL:: HFBROUTECELL                                  !seb
!
!ij is the number of active cells (row in sol. vector)
!jj is the number of non-zero elements in the Jacobian
!IA() is the pointer to a new row in Jacobian (CRS)
!JA() points to the vector of unknowns for each entry in Jacobian (CRS)
      IA = 0
      JA = 0
      jj = 1
      DO ij = 1, Numactive
        il = Diag(1, ij)
        ir = Diag(2, ij)
        ic = Diag(3, ij)
        IA(ij) = jj
! DIAGONAL FIRST
        JA(jj) = Icell(ic, ir, il)
        jj = jj + 1
        IF ( il.GT.1 ) THEN
          IF ( IBOUND(IC,IR,IL-1).NE.0 ) THEN
            JA(jj) = Icell(ic, ir, il-1)
            jj = jj + 1
          END IF
        ENDIF
C   
C seb Ensure that a routed cell k1:k2 is included in FILLINDEX
C
        IF (HFBROUTECELL(IC,IR,IL,IJ))THEN
          CALL FILLINDEXHFB(IC,IR,IL,IJ,jj)
        ELSE
          IF ( ir.GT.1 ) THEN
            IF ( IBOUND(IC,IR-1,IL).NE.0 ) THEN
              JA(jj) = Icell(ic, ir-1, il)
              jj = jj + 1
            END IF
          ENDIF
          IF ( ic.GT.1 ) THEN
            IF ( IBOUND(IC-1,IR,IL).NE.0 ) THEN
              JA(jj) = Icell(ic-1, ir, il)
              jj = jj + 1
            END IF
          ENDIF
          IF ( ic.LT.NCOL ) THEN
            IF ( IBOUND(IC+1,IR,IL).NE.0 ) THEN
              JA(jj) = Icell(ic+1, ir, il)
              jj = jj + 1
            END IF
          ENDIF
          IF ( ir.LT.NROW ) THEN
            IF ( IBOUND(IC,IR+1,IL).NE.0 ) THEN
              JA(jj) = Icell(ic, ir+1, il)
              jj = jj + 1
            END IF
          ENDIF
        END IF
        !
        IF ( il.LT.NLAY ) THEN
          IF ( IBOUND(IC,IR,IL+1).NE.0 ) THEN
            JA(jj) = Icell(ic, ir, il+1)
            jj = jj + 1
          END IF
        END IF
      ENDDO
      IA(Numactive+1) = jj
!      DO ij = 1, Numactive+1
!        write(iout,*)ia(ij)
!      end do
!      DO ij = 1, Numactive
!      DO jj = IA(ij),IA(ij+1)-1
!        write(iout,*)ja(jj)
!      end do 
!      end do
      RETURN     
      END SUBROUTINE FILLINDEX
!
!     -----------------------------------------------------------------
      SUBROUTINE COUNTACTIVE(jj)
      USE GWFNWTMODULE
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,NLAY,IBOUND
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER ic, ir, il, nrncnl, ij, jj
      INTEGER ILM, ILP, IRM, IRP, ICM, ICP, itemp
!     -----------------------------------------------------------------
      LOGICAL, EXTERNAL:: HFBROUTECELL                                  !seb
!
      jj = 0
      DO ij = 1, Numactive
        il = Diag(1, ij)
        ir = Diag(2, ij)
        ic = Diag(3, ij)
! DIAGONAL FIRST
        jj = jj + 1
        IF ( il.GT.1 ) THEN
          IF ( IBOUND(IC,IR,IL-1).NE.0 ) THEN
            jj = jj + 1
          END IF
        ENDIF
C  
C seb Ensure that a routed cell k1:k2 is included active count
C
        IF(HFBROUTECELL(IC,IR,IL,IJ)) THEN
          CALL COUNTACTIVEHFB(IC,IR,IL,IJ,JJ)
        ELSE
          IF ( ir.GT.1 ) THEN
            IF ( IBOUND(IC,IR-1,IL).NE.0 ) THEN
              jj = jj + 1
            END IF
          ENDIF
          IF ( ic.GT.1 ) THEN
            IF ( IBOUND(IC-1,IR,IL).NE.0 ) THEN
              jj = jj + 1
            END IF
          ENDIF
          IF ( ic.LT.NCOL ) THEN
            IF ( IBOUND(IC+1,IR,IL).NE.0 ) THEN
              jj = jj + 1
            END IF
          ENDIF
          IF ( ir.LT.NROW ) THEN
            IF ( IBOUND(IC,IR+1,IL).NE.0 ) THEN
              jj = jj + 1
            END IF
          ENDIF
        END IF
        !
        IF ( il.LT.NLAY ) THEN
          IF ( IBOUND(IC,IR,IL+1).NE.0 ) THEN
            jj = jj + 1
          END IF
        END IF
      ENDDO
      RETURN     
      END SUBROUTINE COUNTACTIVE
!
!     -----------------------------------------------------------------
      SUBROUTINE GWF2NWT1FM(ICNVG, KKITER, KSTP, KPER, MXITER, IGRID)
! Builds and Solves Jacobian
! Calls various unstructured linear solvers to solve Jacobian
      !USE GWFBASMODULE, ONLY:TOTIM, HNOFLO
      USE CONSTANTS, ONLY: Z, ONE, NEG, DZ, UNO, TRUE, FALSE
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Ibound, Hcof, Rhs, Iout,botm,
     +                 LBOTM, HOLD, HNEW, DELR, DELC, ISSFLG, 
     +                 BACKTRACKING, 
     +                 HCloseBAS, RCloseL2BAS
      USE NUM2STR_INTERFACE,   ONLY: NUM2STR
      USE SET_ARRAY_INTERFACE, ONLY: SET_ARRAY
      USE GWFNWTMODULE
      USE XMDMODULE
      USE GMRESMODULE
      USE LGRMODULE, ONLY:ILGR
      USE ilupc_mod
      IMPLICIT NONE
      !
      INTEGER, INTENT(INOUT):: ICNVG
      INTEGER, INTENT(IN   ):: KKITER, KSTP, KPER, MXITER, IGRID
      !
      DOUBLE PRECISION:: R_norm_gmres
      INTEGER:: ITER, ippn, Iss
      INTEGER:: ij, jj, ichld, irhld, ilhld
      INTEGER:: n_iter, n, icfld, irfld, ilfld
      !
      DOUBLE PRECISION:: RMS1_bak, RMS2_bak
      INTEGER:: SUB
      LOGICAL:: RESET_HEAD
      !
      INTERFACE
        PURE DOUBLE PRECISION FUNCTION GW_func()
          USE GWFNWTMODULE, ONLY: Cvm1, Ccm1, Crm1, Crr, Ccc, Cvv,
     +                            H, Hcm1, Hrm1, Hvm1, Hcp1, Hrp1, Hvp1,
     +                            Hcoff, Rhss
        END FUNCTION
      END INTERFACE
!     -----------------------------------------------------------------
!
!
!1------SET POINTERS FOR THE CURRENT GRID.
      IF(ILGR .NE. 0) CALL SGWF2NWT1PNT(IGRID)
      IF(ILGR .NE. 0) CALL SGWF2UPW1PNT(IGRID)  !seb lgr
      IF(ILGR .NE. 0) CALL GMRESPNT(IGRID)      !seb lgr
      IF(ILGR .NE. 0) CALL XMDPNT(IGRID)        !seb lgr
      IF(ILGR .NE. 0) CALL SGWF2BAS7PNT(IGRID)  !seb lgr
      IF(ILGR .NE. 0) CALL SGWF2HFB7PNT(IGRID)  !seb lgr

      ichld = ONE
      irhld = ONE
      ilhld = ONE
      icfld = ONE
      irfld = ONE
      ilfld = ONE
      Icnvg = Z
      iierr = Z
      ippn  = Z
      n_iter = Z
      RESET_HEAD = FALSE
      !
      ISS=ISSFLG(KPER)
      !CALL SET_ARRAY(NCOL,NROW,NLAY,Hnew,Hiter)
      !CALL HEAD_SAVE()                          --Does the same thing as SET_ARRAY
! Save head as previous iteration (Hiter)
!     SOLVE FOR GROUNDWATER HEAD IN 3D USING NEWTON/PICARD
      IF ( Kkiter == ONE ) THEN
        itertot = Z
        II      = Z
        iBtrak  = Z
        RMS1    = DZ
        RMS2    = DZ
        IF ( kkiter+kper.EQ.2) Fheadsave = 2.0*Tol
        Fhead = DZ
      END IF
      !
      IF ( II == Z  ) RMSAVE = RMS1
      !
      RMS1_bak = RMS1
      RMS2_bak = RMS2
      RMS2 = RMS1
      !RMS1 = RMS_func(icfld,irfld,ilfld)
      !
      CALL NWT_RMS_Calc(icfld, irfld, ilfld, Fflux, RMS1)
      !
      !IF ( II == Z  ) RMSAVE = RMS1
      !
      IF (iBtrak == 10) II = NEG    ! Backtracking is not helping disbale for time step
      !
!     IF ( RMS1.GT.FTOL .OR. ABS(Fheadsave).GT.Tol .OR. 
!    +                           kkiter.LT.2 ) THEN
        !
        SUB = -25
        IF(MXITER < 70) SUB = -10
        !
        IF ( Kkiter <= 1 .OR. II < Z) then
            Ibt = 0
            ADAMP = 1.0D0
        ELSEIF ( BTRACK.EQ.0 .AND. kkiter <= MXITER+SUB) then
            Ibt = 0
            ADAMP = 1D0
        ELSE
            Ibt = 1
            !
            IF( kkiter > MXITER+SUB) THEN
                               IF(II>0 .AND. RMS1>=RMS2) THEN
                                  IF( kkiter < MXITER-10 ) THEN
                                     Ibt = 0  !Note that 2nd if sets to zero, but if fails Ibt stays 1 and by passes other checks
                                  ELSE
                                     ADAMP = Breduc_Reset  !reset to H2 = BR*B*H1 = 0.75*H0 + 0.25*H1; given that h1 = B*H0;   B = Breduc and BR = Breduc_Reset
                                  END IF
                               END IF
            ELSEIF(II.GE.Numtrack) THEN
                                     Ibt = 0
                                     ADAMP = 1D0
            ELSEIF(RMS1 < Btol*rmsave) THEN
                                     Ibt = 0
                                     ADAMP = 1D0
            ELSEIF(II>0 .AND. RMS1==RMS2) THEN             !No Change
                                     Ibt   = Z
                                     ADAMP = UNO
            ELSEIF(II>0 .AND. RMS1>RMS2) THEN
                                     IF( ADAMP == Breduc_Reset ) THEN
                                         RESET_HEAD = TRUE
                                         Ibt   = Z
                                         ADAMP = UNO
                                     ELSE
                                         ADAMP = Breduc_Reset    ! reset to H2 = BR*B*H1 = 0.75*H0 + 0.25*H1; given that h1 = B*H0;   B = Breduc and BR = Breduc_Reset
                                         RMS1 = RMS1_bak
                                         RMS2 = RMS2_bak
                                     END IF
            ELSE
                ADAMP = UNO
            END IF
        END IF
        !
        IF (RESET_HEAD) THEN
            CALL SET_ARRAY(NCOL,NROW,NLAY,Hiter,Hnew)
            RMS1 = RMS1_bak
            RMS2 = RMS2_bak
        ELSE
            CALL SET_ARRAY(NCOL,NROW,NLAY,Hnew,Hiter)
        END IF
        !
        BACKTRACKING = Ibt.NE.0
        !
        IF ( BACKTRACKING ) THEN
                      n_iter = 0
                      CALL Back_track(ichld,irhld,ilhld)
                      II = II + 1
        ELSE   ! Solve Jacobian and NWT Step
          II = 0
          jj = 1
          CALL Jacobian(kkiter,kper,kstp)
! Calls for linear solver
          n = Numactive
          IF ( Linmeth.EQ.1 ) THEN
! M_save_dir is size of Krylov space. Same as msdr:
            SELECT CASE (ilu_method)
            CASE(1)
                CALL ilut (n,A,JA,IA,Lev_fill,Drop_tol,Alu,Jlu,Ju,Iierr)
            CASE(2)
                CALL iluk (n,A,JA,IA,lev_fill,Alu,Jlu,Ju,Iierr)
            END SELECT
            IF(Iierr /= 0) CALL USTOP('Error in Preconditioning: '//
     +                                               NUM2STR(Iierr))
            CALL gmres(n,Msdr,BB,Hchange,Stop_tol_gmres,Maxitr_gmres,A,
     +             JA,IA,Alu,Jlu,Ju,iierr,n_iter,R_norm_gmres)
            IF(Iierr == 1) THEN
!              WRITE(Iout,*) 'Linear solver failed to converge: '
!     +                  , Iierr,n_iter, R_norm_gmres
!            CALL USTOP('  ')
            ELSEIF(Iierr /= 0) THEN
                CALL USTOP('Error in gmres: '//
     +               NUM2STR(Iierr,n_iter)//' '//NUM2STR(R_norm_gmres))
            END IF
          ELSEIF(LINMETH.EQ.2)THEN
C
C---------CALL XMD SOLVER     
c
            IF (IDROPTOL.EQ.0 .or.  kkiter.gt.1) THEN
              call xmdnfctr(a, bb, ia, ja, nja, numactive, ierr)
            ELSE
              call xmdprecd(a, bb, epsrn, ia, ja, nja, numactive,
     [                      level, ierr)

cmi
C
            ENDIF                                                     
c  -----------------
c         solve matrix
            iter = Mxiterxmd
            call xmdsolv(a, bb, hchange, hclosexmd, rrctol, ia, ja, nja,
     [                 numactive, north, iter, iacl, ierr)
            n_iter = iter
          END IF
C
C--Update heads.
          CALL GWF2NWT1UPH2(ichld,irhld,ilhld,ISS,Kkiter)
          !
          IF(HED_LIM == HED_LIM .OR. GSE_LIM(1,1) == GSE_LIM(1,1)) THEN
             CALL APPLY_GSE_LIM(ichld,irhld,ilhld)
          END IF
          Fheadsave = Fhead
          ippn = 0
          IF ( RMS1.LT.FTOL .AND. ABS(Fheadsave).LT.Tol ) THEN
            Icnvg = 1
            ippn = 1
          END IF
          Itreal = Itreal + 1
        END IF
 !       Itreal = Itreal + 1
      !ELSE
      !  Icnvg = 1
      !  Itreal = Itreal + 1  !seb increment the iteration count
      !END IF
 ! 888 format(256E20.10)
!
!  Calculate maximum head change and residuals
!  Write head and flux residuals
!  Write iteration header
      HCloseBAS   = ABS(Fheadsave)
      RCloseL2BAS = RMS1
      IF ( IPRNWT.GT.0 ) THEN
        n = Numtrack/2
        if(N<10) N=10
        if(N>50) N=50
        IF ( MOD(Kkiter,N).eq.1 .AND. Icnvg.EQ.0 ) THEN
          IF ( Btrack.GT.0 ) THEN
            WRITE(IOUT,111)
          ELSE
            WRITE(IOUT,112)
          END IF
        END IF
      END IF
  111 FORMAT (1X,'                                  ',
     +           '                Max.-Head-Change',
     +           '                        ',
     +           'Max.-Flux-Residual',/
     +          '  Residual-Control     NWT-Iter.   ',
     +          'Inner-Iter.    ',
     +          ' Column Row Layer ',   
     +          '   Max.-Head-Change   ', 
     +          ' Column Row Layer ',
     +          '   Max.-Flux-Residual',
     +          '            L2-New               L2-Old           ',
     +          'Solver-Max-Delh',
     +          '    Outer-Iter.' )
  112 FORMAT (1X,'                                ',
     +           '                  Max.-Head-Change',
     +           '                         ',
     +           'Max.-Flux-Residual',/
     +          '  Residual-Control     NWT-Iter.   ',
     +          'Inner-Iter.    ',
     +          ' Column Row Layer ',   
     +          '   Maximum-Head-Change ', 
     +          ' Column Row Layer ',
     +          '   Maximum-Flux-Residual          L2-NORM ',
     +          '  Outer-Iter.' )
      itertot = itertot + n_iter
      IF ( IPRNWT.GT.0 ) THEN
        !IF ( Icnvg.EQ.0 .OR. ippn.EQ.1) THEN
          IF ( Btrack.GT.0 ) THEN 
                        WRITE (Iout, 9001) II,itreal,n_iter+1,
     +                 ichld,irhld,ilhld,fhead,icfld,irfld,ilfld,
     +                 Fflux,RMS1,RMS2,FHEADSAVE,Kkiter
          ELSE
            WRITE(Iout, 9002)II,itreal,n_iter+1,ichld,irhld,ilhld,fhead,
     +                  icfld,irfld,ilfld,fflux,RMS1,Kkiter
!     +      HNEW(ichld,irhld,ilhld),BOTM(ichld,irhld,ilhld-1),
!     +      BOTM(ichld,irhld,ilhld)
          END IF
        !END IF
      END IF
      !IF ( Icnvg.GT.0 .OR. itreal.GE.Maxiter) THEN
      !      WRITE (Iout,9003) itreal, itertot
      !END IF
 9001 FORMAT (5X,I6,12X,I6,6X,I6,8X,I6,1x,I4,3X,I3,3X,ES20.6,
     +        2x,I6,1x,I4,3X,I2,1X,4(2X,ES20.6), 4x,I0)
 9002 FORMAT (5X,I6,12X,I6,6X,I6,8X,I6,1x,I4,3X,I3,3X,ES20.6,
     +        2x,I6,1x,I4,3X,I2,3X,2(2X,ES20.6), 4x,I0)
! 9003 FORMAT (/4x,'------------------------------------------------',/
!     +        7x,'NWT REQUIRED     ',i8,' OUTER ITERATIONS ',/
!     +        7x,'AND A TOTAL OF   ',i8,' INNER ITERATIONS.',/
      END SUBROUTINE GWF2NWT1FM
!
!
!     -----------------------------------------------------------------
!     Set the head in dewatered cells to Hdry.
      SUBROUTINE GWF2NWT1BD(KITER, IGRID)
      USE GLOBAL, ONLY:Hnew, Nrow, Ncol, Nlay, BOTM, LBOTM, IBOUND,
     +                 LAYHDT, IOUT  
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFUPWMODULE,ONLY:IPHDRY
      USE GWFNWTMODULE,ONLY:itreal, itertot, Hiter
      USE SET_ARRAY_INTERFACE, ONLY: SET_ARRAY
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER ic, ir, il, IGRID, KITER
!     -----------------------------------------------------------------
      REAL HDRYTOL
!     -----------------------------------------------------------------
C
C-----SET HNEW TO HDRY IF IPHRY>0
      HDRYTOL = 2.0e-3

      CALL SGWF2UPW1PNT(IGRID)  !seb lgr
      CALL SGWF2NWT1PNT(IGRID)  !seb lgr
      CALL SGWF2BAS7PNT(IGRID)  !seb lgr
      !
      WRITE (Iout,9003) itreal, KITER, itertot
 9003 FORMAT (/4x,'------------------------------------------------',/
     +        7x,'NWT REQUIRED     ',i8,' NEWTON ITERATIONS ',/
     +        7x,'AND A TOTAL OF   ',i8,' OUTER  ITERATIONS ',/
     +        7x,'AND A TOTAL OF   ',i8,' INNER  ITERATIONS.',/
     +        4x,'------------------------------------------------')
      !
      CALL SET_ARRAY(NCOL,NROW,NLAY,Hnew,Hiter)
      !
      IF(IPHDRY.GT.0) THEN
         DO il = 1, Nlay
           IF ( LAYHDT(il).GT.0 ) THEN
             DO CONCURRENT(ir=1:Nrow,ic=1:Ncol,IBOUND(ic,ir,il).GT.0)
                   IF ( Hnew(ic, ir, il)-dble(BOTM(ic,ir,LBOTM(il)))
     +                                                     .LT.HDRYTOL )
     +                  Hnew(ic, ir, il) = dble(Hdry)
             END DO
           END IF
         ENDDO
      END IF
      !
      END SUBROUTINE GWF2NWT1BD
!
!
!     -----------------------------------------------------------------
!
      SUBROUTINE Back_track(ichld,irhld,ilhld)
      USE GLOBAL, ONLY:Ibound, Hnew, Lbotm, Botm, Iout, NLAY
      USE GWFUPWMODULE, ONLY:LAYTYPUPW
      USE GWFNWTMODULE
      IMPLICIT NONE
      !
      INTEGER ichld,irhld,ilhld
      !
      INTEGER:: ic, ir, il, jj, i
      DOUBLE PRECISION:: hsave, bot
      LOGICAL:: H_2_BOT
      !
!  APPLY BACKTRACKING
!  
      Fhead = 0.0D0    
      DO jj = 1, Numactive
        il = Diag(1, jj)
        ir = Diag(2, jj)
        ic = Diag(3, jj)
        Hiter(ic, ir, il) = Hiter(ic, ir, il) - Hchange(jj)
        Hchange(jj) = ADAMP*Breduc*Hchange(jj)
        HNEW(ic, ir, il) = HITER(ic, ir, il) + Hchange(jj)
        !
!!!        IF ( IBOTAV.GT.0 .AND. LAYTYPUPW(il).GT.0 ) THEN  --> No need for correction with backtracking what would be already an acceptible solution
!!!          bot = dble(BOTM(ic, ir, Lbotm(il)))
!!!          !
!!!          IF( Hnew(ic, ir, il) < bot ) THEN
!!!              !
!!!              !bot = bot + bot*1.0e-7  !Make cell slightly wet
!!!              !
!!!              H_2_BOT = il == NLAY
!!!              IF( il < NLAY ) H_2_BOT = IBOUND(ic,ir,il+1) == 0
!!!              !
!!!              IF(H_2_BOT) THEN
!!!                 IF( Hiter(ic, ir, il) < bot ) 
!!!     +               Hiter(ic, ir, il) = bot + bot*1.0e-6 !1.0e-6 !
!!!                 !
!!!                 Hnew(ic, ir, il) = ( Hiter(ic, ir, il) + bot ) * 0.5D0
!!!                 Hchange(jj) = Hnew(ic, ir, il) - Hiter(ic, ir, il)
!!!              END IF
!!!          END IF
!!!        END IF
!        IF ( IBOTAV.GT.0 .AND. LAYTYPUPW(il).GT.0 ) THEN
!          ibotlay = il
!          DO i = il + 1, NLAY - 1
!            IF ( IBOUND(ic,ir,i).GT.0 ) ibotlay = ibotlay + 1
!          END DO
!          IF ( il.EQ.NLAY ) THEN
!            IF (Hnew(ic, ir, il).LT.dble(Botm(ic, ir, Lbotm(il))) ) THEN
!              IF ( Hiter(ic, ir, il).LT.dble(BOTM(ic, ir, Lbotm(il))) )
!     +          Hiter(ic, ir, il) = BOTM(ic, ir, Lbotm(il)) + 
!     +                              1.0e-6
!!              sum = 0.0D0
!!              sum = Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat
!!     +              (Sum_sat(sum,ic,ir,il),ic-1,ir,il),ic+1,ir,il),
!!     +               ic,ir-1,il),ic,ir+1,il),ic,ir,il-1),ic,ir,il+1)
!!              IF ( sum .LT.1.0e-7 ) THEN
!              IF(NWT_ALL_DRY(ic,ir,il,1.0e-7)) THEN
!                hsave = Hnew(ic, ir, il)
!                Hnew(ic, ir, il) = (Hiter(ic, ir, il)+
!     +                              dble(BOTM(ic, ir, Lbotm(il))))/2.0d0
!                Hchange(jj) = Hnew(ic, ir, il) - hsave
!              END IF
!            END IF
!          ELSEIF ( IBOUND(ic,ir,ibotlay+1).EQ.0 ) THEN
!            IF (Hnew(ic, ir, il).LT.dble(Botm(ic, ir, Lbotm(ibotlay))) )
!     +                                                        THEN
!              IF ( Hiter(ic, ir, il).LT.
!     +                              dble(BOTM(ic, ir, Lbotm(ibotlay))) )
!     +          Hiter(ic, ir, il) = dble(BOTM(ic, ir, Lbotm(ibotlay))) +
!     +                              1.0e-6
!!              sum = 0.0D0
!!              sum = Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat
!!     +              (Sum_sat(sum,ic,ir,il),ic-1,ir,il),ic+1,ir,il),
!!     +               ic,ir-1,il),ic,ir+1,il),ic,ir,il-1),ic,ir,il+1)
!!              IF ( sum .LT.1.0e-7 ) THEN
!              IF(NWT_ALL_DRY(ic,ir,il,1.0e-7)) THEN
!                hsave = Hnew(ic, ir, il)
!                Hnew(ic, ir, il) = (Hiter(ic, ir, il)+
!     +                     dble(BOTM(ic, ir, Lbotm(ibotlay))))/2.0d0
!                Hchange(jj) = Hnew(ic, ir, il) - hsave
!              END IF
!            END IF
!          ENDIF
!        END IF
        IF ( ABS(Hchange(jj)).GT.ABS(fhead) ) THEN
          Fhead = Hchange(jj)
          ichld = ic
          irhld = ir
          ilhld = il
        ENDIF
      END DO
      END SUBROUTINE Back_track
!
!
!     -----------------------------------------------------------------
      SUBROUTINE GWF2NWT1UPH2(ichld,irhld,ilhld,ISS,Kkiter)
!  Update heads and apply relaxation (delta-bar-delta)
      USE GLOBAL, ONLY:Ibound, Hnew, Lbotm, Botm, Iout, NCOL,NROW,NLAY
      USE GWFUPWMODULE, ONLY:LAYTYPUPW
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
      INTRINSIC ABS
      !DOUBLE PRECISION, EXTERNAL :: GW_func, Sum_sat
!     -----------------------------------------------------------------
!     ARGUMENTS
!     -----------------------------------------------------------------
      INTEGER Kkiter,Icnvg,ichld,irhld,ilhld,ISS
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION s, wstar, ww, change, hsave, sum, bot
      INTEGER ic, ir, il, jj, ibotlay, i
      LOGICAL:: H_2_BOT
!     -----------------------------------------------------------------
      !INTERFACE
      !   PURE FUNCTION NWT_ALL_DRY(ic,ir,il,tol) RESULT(ANS)
      !        USE GWFNWTMODULE, ONLY:Icell
      !        USE GWFUPWMODULE, ONLY:Sn
      !        USE GLOBAL,       ONLY:Ncol,Nrow,Nlay
      !        IMPLICIT NONE
      !        !
      !        INTEGER,          INTENT(IN):: ic,ir,il
      !        DOUBLE PRECISION, INTENT(IN):: tol
      !        LOGICAL:: ANS
      !   END FUNCTION
      !END INTERFACE
      !
      Fhead = 0.0D0
      DO jj = 1, Numactive
        il = Diag(1, jj)
        ir = Diag(2, jj)
        ic = Diag(3, jj)
        Hchange(jj) = Hchange(jj) - Hnew(ic,ir,il)
        IF ( kkiter.EQ.1 )THEN
          Wsave(jj) = 1.0D0
          Hchold(jj) = Hchange(jj)
        END IF            
        ww = Wsave(jj)
        IF ( Hchold(jj)*Hchange(jj).LT.0.0D0 ) THEN
          ww = Theta*Wsave(jj)
        ELSE
          ww = Wsave(jj) + akappa
        END IF
        IF ( ww > 1.1d0 ) ww = 1.1d0
        Hchold(jj) = (1-gamma) * Hchange(jj) + gamma * Hchold(jj)
        Wsave(jj) = ww
        Hchange(jj) = Hchange(jj) * ww + amomentum * Hchold(jj)
        Hnew(ic, ir, il) = Hiter(ic, ir, il) + Hchange(jj)
        !
        IF ( IBOTAV.GT.0 .AND. LAYTYPUPW(il).GT.0 ) THEN
          bot = dble(BOTM(ic, ir, Lbotm(il)))
          !
          IF( Hnew(ic, ir, il) < bot ) THEN
              !
              !bot = bot + bot*1.0e-7  !Make cell slightly wet
              !
              H_2_BOT = il == NLAY
              IF( il < NLAY ) H_2_BOT = IBOUND(ic,ir,il+1) == 0
              !
              IF(H_2_BOT) THEN
                 IF( Hiter(ic, ir, il) < bot ) 
     +               Hiter(ic, ir, il) = bot + bot*1.0e-6 !1.0e-6 !
                 !
                 Hnew(ic, ir, il) = ( Hiter(ic, ir, il) + bot ) * 0.5D0
                 Hchange(jj) = Hnew(ic, ir, il) - Hiter(ic, ir, il)
              END IF
          END IF
        END IF
!        IF ( IBOTAV.GT.0 .AND. LAYTYPUPW(il).GT.0 ) THEN
!          ibotlay = il
!          DO i = il + 1, NLAY - 1
!            IF ( IBOUND(ic,ir,i).GT.0 ) ibotlay = ibotlay + 1
!          END DO
!          IF ( il.EQ.NLAY ) THEN
!            IF (Hnew(ic, ir, il).LT.dble(Botm(ic, ir, Lbotm(il))) ) THEN
!              IF ( Hiter(ic, ir, il).LT.dble(BOTM(ic, ir, Lbotm(il))) )
!     +          Hiter(ic, ir, il) = BOTM(ic, ir, Lbotm(il)) + 
!     +                              1.0e-6
!!              sum = 0.0D0
!!              sum = Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat
!!     +              (Sum_sat(sum,ic,ir,il),ic-1,ir,il),ic+1,ir,il),
!!     +               ic,ir-1,il),ic,ir+1,il),ic,ir,il-1),ic,ir,il+1)
!!              IF ( sum .LT.1.0e-7 ) THEN
!              IF(NWT_ALL_DRY(ic,ir,il,1.0e-7)) THEN
!                hsave = Hnew(ic, ir, il)
!                Hnew(ic, ir, il) = (Hiter(ic, ir, il)+
!     +                              dble(BOTM(ic, ir, Lbotm(il))))/2.0d0
!                Hchange(jj) = Hnew(ic, ir, il) - hsave
!              END IF
!            END IF
!          ELSEIF ( IBOUND(ic,ir,ibotlay+1).EQ.0 ) THEN
!            IF (Hnew(ic, ir, il).LT.dble(Botm(ic, ir, Lbotm(ibotlay))) )
!     +                                                              THEN
!              IF ( Hiter(ic, ir, il).LT.
!     +                              dble(BOTM(ic, ir, Lbotm(ibotlay))) )
!     +          Hiter(ic, ir, il) = dble(BOTM(ic, ir, Lbotm(ibotlay))) +
!     +                              1.0e-6
!!              sum = 0.0D0
!!              sum = Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat
!!     +              (Sum_sat(sum,ic,ir,il),ic-1,ir,il),ic+1,ir,il),
!!     +               ic,ir-1,il),ic,ir+1,il),ic,ir,il-1),ic,ir,il+1)
!!              IF ( sum .LT.1.0e-7 ) THEN
!              IF(NWT_ALL_DRY(ic,ir,il,1.0e-7)) THEN
!                hsave = Hnew(ic, ir, il)
!                Hnew(ic, ir, il) = (Hiter(ic, ir, il)+
!     +                    dble(BOTM(ic, ir, Lbotm(ibotlay))))/2.0d0
!                Hchange(jj) = Hnew(ic, ir, il) - hsave
!              END IF
!            END IF
!          ENDIF
!        END IF
        IF ( ABS(Hchange(jj)).GT.ABS(fhead) ) THEN
          fhead = Hchange(jj)
          ichld = ic
          irhld = ir
          ilhld = il
        ENDIF 
      ENDDO
      END SUBROUTINE GWF2NWT1UPH2
!
!
!     -----------------------------------------------------------------
      SUBROUTINE APPLY_GSE_LIM(ichld,irhld,ilhld)
!  Scale heads if they exceed the land surface
      USE GLOBAL, ONLY: Hnew, Iout, NCOL,NROW,NLAY,GSE
      USE GWFUPWMODULE, ONLY:LAYTYPUPW
      USE GWFNWTMODULE
      IMPLICIT NONE
      !
      INTEGER, INTENT(INOUT):: ichld,irhld,ilhld
      !
      INTEGER ic, ir, il, jj, i, iworst
      DOUBLE PRECISION:: DMP, CHNG, HCHNG, worst
      !
      worst = 0.0D0
      iworst = 0
      IF(HED_LIM == HED_LIM) THEN
        DO jj = 1, Numactive
          !
          HCHNG = ABS(Hchange(jj))
          IF(HCHNG > HED_LIM) THEN
              IF(HCHNG - HED_LIM > worst) THEN
                 DMP = HED_LIM / HCHNG
                 worst = HCHNG - HED_LIM
                 iworst = jj 
              END IF
          END IF
        END DO
      END IF
      !
      IF(GSE_LIM(1,1) == GSE_LIM(1,1)) THEN
        DO jj = 1, Numactive
          il = Diag(1, jj)
          ir = Diag(2, jj)
          ic = Diag(3, jj)
          !
          IF(Hnew(ic, ir, il) > GSE_LIM(ic, ir)) THEN
              IF(Hnew(ic, ir, il) - GSE_LIM(ic, ir) > worst) THEN
                 HCHNG = GSE_LIM(ic, ir) - Hnew(ic,ir,il) + Hchange(jj)
                 IF(HCHNG > 0D0) THEN
                    DMP = HCHNG / ABS(Hchange(jj))
                    worst = HCHNG
                    iworst = jj 
                 END IF
              END IF
          END IF
        END DO
      END IF
      !
      IF(iworst > 0) THEN
         Fhead = 0.0D0
         DMP = 1D0 - DMP
         DO jj = 1, Numactive
           il = Diag(1, jj)
           ir = Diag(2, jj)
           ic = Diag(3, jj)
           !CHNG = Hchange(jj) * (1D0 - DMP)
           CHNG = Hchange(jj) * DMP
           !
           Hnew(ic, ir, il) = Hnew(ic, ir, il) - CHNG
           Hchange(jj) = Hchange(jj) - CHNG
           !
           IF ( ABS(Hchange(jj)).GT.ABS(fhead) ) THEN
             fhead = Hchange(jj)
             ichld = ic
             irhld = ir
             ilhld = il
           ENDIF 
         ENDDO
      END IF
      END SUBROUTINE 
!
!
!     -----------------------------------------------------------------
!     Save previous iteration heads.
      SUBROUTINE Head_save()
      USE GLOBAL, ONLY:Hnew, Nrow, Ncol, Nlay, Iout, BOTM
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      INTEGER ic, ir, il
!     -----------------------------------------------------------------
      DO il = 1, Nlay
        DO ir = 1, Nrow
          DO ic = 1, Ncol
            Hiter(ic, ir, il) = Hnew(ic, ir, il)
          ENDDO
        ENDDO
      ENDDO
      END SUBROUTINE HEAD_SAVE
!
!     -----------------------------------------------------------------
!     Return value of groundwater flow equation
      PURE DOUBLE PRECISION FUNCTION GW_func()
        USE GWFNWTMODULE, ONLY: Cvm1, Ccm1, Crm1, Crr, Ccc, Cvv,
     +                          H, Hcm1, Hrm1, Hvm1, Hcp1, Hrp1, Hvp1,
     +                          Hcoff, Rhss
        USE GWFBASMODULE, ONLY: HNOFLO
        IMPLICIT NONE
        !
        DOUBLE PRECISION:: term1, term2, term3
        !
        !GW_func = 0.0D0
        !
        !IF ( H==HNOFLO ) RETURN    -- Assume
        !
        term1 = Cvm1*Hvm1 + Ccm1*Hrm1 + Crm1*Hcm1
        term2 = (-Cvm1-Ccm1-Crm1-Crr-Ccc-Cvv+Hcoff)*H
        term3 = Crr*Hcp1 + Ccc*Hrp1 + Cvv*Hvp1 - Rhss
        !
        GW_func = term1 + term2 + term3
        !
      END FUNCTION GW_func
!
!!!      DOUBLE PRECISION FUNCTION GW_func(Ic, Ir, Il)
!!!      USE GWFNWTMODULE
!!!      USE GLOBAL,      ONLY:iout
!!!      USE GWFBASMODULE, ONLY:HNOFLO
!!!      IMPLICIT NONE
!!!!     ------------------------------------------------------------------
!!!!     SPECIFICATIONS:
!!!!     -----------------------------------------------------------------
!!!!     ARGUMENTS
!!!!     -----------------------------------------------------------------
!!!      INTEGER Ic, Ir, Il, Iunitlak
!!!!     -----------------------------------------------------------------
!!!!     LOCAL VARIABLES
!!!!     -----------------------------------------------------------------
!!!      DOUBLE PRECISION term1, term2, term3
!!!!     -----------------------------------------------------------------   
!!!      GW_func = 0.0D0
!!!      IF ( H==HNOFLO ) RETURN    
!!!      term1 = Cvm1*Hvm1 + Ccm1*Hrm1 + Crm1*Hcm1
!!!      term2 = (-Cvm1-Ccm1-Crm1-Crr-Ccc-Cvv+Hcoff)*H
!!!      term3 = Crr*Hcp1 + Ccc*Hrp1 + Cvv*Hvp1 - Rhss
!!!      GW_func = term1 + term2 + term3
!!!!      if(ic==271)then
!!!!        write(iout,222)ic,ir,il,cvm1*(Hvm1-h),ccm1*(hrm1-h),crm1*(hcm1-h),
!!!!     +cvv*(hvp1-h),ccc*(hrp1-h),crr*(hcp1-h),rhss,gw_func
!!!!      end if
!!!!  222 format(3i5,8e20.10)
!!!      END FUNCTION GW_func
!
!
!     -----------------------------------------------------------------
!     Return value of L2-Norm of GW equation, max flux
!     and head residuals
      SUBROUTINE NWT_RMS_Calc(icfld, irfld, ilfld, Fflux, RMS)
      USE GWFNWTMODULE, ONLY: Numactive,Diag
      USE GWFUPWMODULE, ONLY: Laytypupw
      USE GLOBAL,       ONLY: Ibound
      IMPLICIT NONE
      !
      DOUBLE PRECISION, INTENT(INOUT):: Fflux, RMS
      INTEGER,          INTENT(INOUT):: icfld, irfld, ilfld
      !
      DOUBLE PRECISION:: ferr
      INTEGER:: ichld, irhld, ilhld
      INTEGER:: IJ, ic, ir, il, iSY
      LOGICAL:: CONVERTIBLE
      !
      INTERFACE
        PURE DOUBLE PRECISION FUNCTION GW_func()
          USE GWFNWTMODULE, ONLY: Cvm1, Ccm1, Crm1, Crr, Ccc, Cvv,
     +                            H, Hcm1, Hrm1, Hvm1, Hcp1, Hrp1, Hvp1,
     +                            Hcoff, Rhss
        END FUNCTION
        !
        PURE LOGICAL FUNCTION HFBROUTECELL(IC,IR,IL,IJ)
          INTEGER, INTENT(IN):: IC,IR,IL,IJ
        END FUNCTION
      END INTERFACE
      !
      rms   = 0.0D0
      Fflux = 0.0D0
      DO IJ = 1, Numactive
        il = Diag(1, IJ)
        ir = Diag(2, IJ)
        ic = Diag(3, IJ)
        !
        IF ( IBOUND(ic,ir,il) > 0 ) THEN
          !
          CONVERTIBLE = Laytypupw(il) > 0
          !
          IF (HFBROUTECELL(IC,IR,IL,IJ)) THEN
            IF ( CONVERTIBLE ) THEN
              CALL TEMPFILLUNHFB(ic, ir, il)
            ELSE 
              CALL TEMPFILLCONHFB(ic, ir, il)
            END IF
          ELSE
            IF( CONVERTIBLE ) THEN
              CALL TEMPFILLUN(ic, ir, il)
            ELSE
              CALL TEMPFILLCON(ic, ir, il)
            END IF
          END IF
          !
          ferr = GW_func()
          !
          rms = rms + ferr*ferr
          !
          IF ( abs(ferr) > abs(Fflux) ) THEN
                                        fflux = ferr
                                        icfld = ic
                                        irfld = ir
                                        ilfld = il
          END IF
        END IF
      END DO
      !
      rms = SQRT(rms)
      !
      END SUBROUTINE
!
!
!!!      DOUBLE PRECISION FUNCTION RMS_func(icfld,irfld,ilfld)
!!!      USE GWFNWTMODULE, ONLY: Fflux,Numactive,Diag
!!!      USE GWFUPWMODULE, ONLY: Laytypupw
!!!      USE GLOBAL,      ONLY:Iout,Hnew,Ibound
!!!      USE GWFBASMODULE, ONLY:HNOFLO
!!!      IMPLICIT NONE
!!!!     ------------------------------------------------------------------
!!!!     SPECIFICATIONS:
!!!!     -----------------------------------------------------------------
!!!      DOUBLE PRECISION, EXTERNAL :: GW_func
!!!!     -----------------------------------------------------------------
!!!!     ARGUMENTS
!!!!     -----------------------------------------------------------------
!!!!     -----------------------------------------------------------------
!!!!     LOCAL VARIABLES
!!!!     -----------------------------------------------------------------
!!!      DOUBLE PRECISION rms, ferr
!!!      INTEGER ichld, irhld, ilhld, icfld, irfld, ilfld
!!!      INTEGER jj, ic, ir, il
!!!!     -----------------------------------------------------------------   
!!!      LOGICAL, EXTERNAL:: HFBROUTECELL                                  !seb
!!!!     -----------------------------------------------------------------  
!!!      rms = 0.0D0
!!!      Fflux = 0.0D0
!!!      DO jj = 1, Numactive
!!!        il = Diag(1, jj)
!!!        ir = Diag(2, jj)
!!!        ic = Diag(3, jj)
!!!        IF ( IBOUND(ic,ir,il).GT.0 ) THEN
!!!C seb USE HFB Specific TEMPFILL Subroutines
!!!          IF (HFBROUTECELL(IC,IR,IL,JJ)) THEN
!!!            IF ( Laytypupw(il).GT.0 ) THEN
!!!              CALL TEMPFILLUNHFB(ic, ir, il)
!!!            ELSE 
!!!              CALL TEMPFILLCONHFB(ic, ir, il)
!!!            END IF
!!!          ELSE
!!!            IF( Laytypupw(il).GT.0 ) THEN
!!!              CALL TEMPFILLUN(ic, ir, il)
!!!            ELSE
!!!              CALL TEMPFILLCON(ic, ir, il)
!!!            END IF
!!!          END IF
!!!          ferr = GW_func()
!!!          rms = rms + ferr**2.0D0
!!!          IF ( abs(ferr).GT.abs(Fflux) ) THEN
!!!            fflux = ferr
!!!            icfld = ic
!!!            irfld = ir
!!!            ilfld = il
!!!          END IF
!!!        END IF
!!!      END DO
!!!      RMS_func = rms**0.5D0
!!!      END FUNCTION RMS_func
!
!
!     -----------------------------------------------------------------
!     Calculates derivatives of conductance.
      SUBROUTINE Dcon(kkiter)
      USE CONSTANTS, ONLY: DZ
      USE GWFNWTMODULE
      USE GWFUPWMODULE, ONLY:LAYTYPUPW
      USE GLOBAL,      ONLY:Iout,HNEW,BOTM,LBOTM,NLAY,CC,CR,CV,NCOL,
     +                      NROW
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     -----------------------------------------------------------------
      DOUBLE PRECISION, EXTERNAL:: Dhoriz  !Dvert, 
      LOGICAL, EXTERNAL:: HFBROUTECELL                                  !seb
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION hh, ttop, bbot, Dv, Dh
      INTEGER ij, ic, ir, il, i, kkiter
!     ----------------------------------------------------------------- 
      DO CONCURRENT(ij=1:Numactive, i=1:6) 
                                          Dc(i, ij) = DZ
      END DO
      DO ij = 1, Numactive
        il = Diag(1, ij)
        ir = Diag(2, ij)
        ic = Diag(3, ij)
!        DO I = 1,6
!          Dc(ij,I) = 0.0D0
!        END DO
        hh = HNEW(ic,ir,il)
        ttop = dble(BOTM(ic,ir,LBOTM(il)-1))
        bbot = dble(BOTM(ic,ir,LBOTM(il)))
        !
        Dv = DZ                                ! Dvert(hh,ttop,bbot) --seb function always returns zero
        Dh = (ttop-bbot)*Dhoriz(hh,ttop,bbot,il)
        IF ( dh == DZ ) THEN  !dh = 0 if LAYTYPUPW(il)==0
          Dv = DZ
          Dh = DZ
        ELSE
          !
C seb USE HFB SPECIFIC DERIVATIVE CALCULATIONS
          IF (HFBROUTECELL(IC,IR,IL,IJ))THEN
            CALL DconHFB(hh,Dv,Dh,IC,IR,IL,IJ)
          ELSE
            !IF ( il.GT.1 ) THEN                                       --DV always zero
            !  IF ( hh.GT.HNEW(ic,ir,il-1)) Dc(1,ij) = 
     +      !                               Cv(ic,ir,il-1)*Dv
            !END IF
            IF ( IR.GT.1 ) THEN
              IF ( hh.GT.HNEW(ic,ir-1,il)) Dc(2,ij) = Cc(ic,ir-1,il)*Dh
            END IF
            IF ( IC.GT.1 ) THEN
              IF ( hh.GT.HNEW(ic-1,ir,il)) Dc(3,ij) = Cr(ic-1,ir,il)*Dh
            END IF
            IF ( IC.LT.NCOL ) THEN
              IF ( hh.GT.HNEW(ic+1,ir,il)) Dc(4,ij) = Cr(ic,ir,il)*Dh
            END IF
            IF ( IR.LT.NROW ) THEN
              IF ( hh.GT.HNEW(ic,ir+1,il)) Dc(5,ij) = Cc(ic,ir,il)*Dh
            END IF
            !IF ( IL.LT.NLAY ) THEN                                    --DV always zero
            !  IF ( hh.GT.HNEW(ic,ir,il+1)) Dc(6,ij) = Cv(ic,ir,il)*Dv
            !END IF
          END IF
        END IF
      END DO
      RETURN
      END SUBROUTINE
!
!     -----------------------------------------------------------------
!     Returns sum of saturated thicknesses for cells connected
!     to ic,ir,il.
!     and head residuals
      DOUBLE PRECISION FUNCTION Sum_sat(sum,ic,ir,il)
      USE GWFNWTMODULE, ONLY:Icell
      USE GWFUPWMODULE, ONLY:Sn
      USE GLOBAL,       ONLY:Hnew,Ibound,Botm,Lbotm,Ncol,Nrow,Nlay
      USE GWFBASMODULE, ONLY:HNOFLO
      IMPLICIT NONE
!     -----------------------------------------------------------------
!     ARGUMENTS
      INTEGER ic,ir,il,jj
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION sum, sat

!     -----------------------------------------------------------------  
      Sum_sat = 0.0D0
      sat = 0.0D0
      IF ( ic.GT.Ncol .OR. ic.LT.1 ) RETURN
      IF ( ir.GT.Nrow .OR. ir.LT.1 ) RETURN
      IF ( il.GT.Nlay .OR. il.LT.1 ) RETURN
      jj = Icell(ic, ir, il)
      IF ( jj.GT.0 ) sat = Sn(jj)
      Sum_sat = sum + sat     
      END FUNCTION Sum_sat
!
!     -----------------------------------------------------------------
!     Checks if cell is isoluated from model (that is it is surrounded by cells with no saturation.
!     Identical to: 
!      tol > Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(Sum_sat(sum,ic,ir,il),ic-1,ir,il),ic+1,ir,il),ic,ir-1,il),ic,ir+1,il),ic,ir,il-1),ic,ir,il+1)      
      PURE FUNCTION NWT_ALL_DRY(ic,ir,il,tol) RESULT(ANS)
      USE GWFNWTMODULE, ONLY:Icell
      USE GWFUPWMODULE, ONLY:Sn
      USE GLOBAL,       ONLY:Ncol,Nrow,Nlay
      IMPLICIT NONE
      !
      INTEGER,          INTENT(IN):: ic,ir,il
      DOUBLE PRECISION, INTENT(IN):: tol
      LOGICAL:: ANS
      !
      INTEGER:: IJ
      DOUBLE PRECISION:: sm
      !
      sm = 0D0
      !
      IJ = Icell(ic, ir,   il)
      IF ( IJ > 0 ) sm = sm + Sn(IJ)
      !
      IF(1 < ic) THEN
          IJ = Icell(ic-1, ir,   il)
          IF ( IJ > 0 ) sm = sm + Sn(IJ)
      END IF
      !
      IF(ic < NCOL) THEN
          IJ = Icell(ic+1, ir,   il)
          IF ( IJ > 0 ) sm = sm + Sn(IJ)
      END IF
      !
      IF(1 < ir) THEN
          IJ = Icell(ic,   ir-1, il)
          IF ( IJ > 0 ) sm = sm + Sn(IJ)
      END IF
      !
      IF(ir < NROW) THEN
          IJ = Icell(ic,   ir+1, il)
          IF ( IJ > 0 ) sm = sm + Sn(IJ)
      END IF
      !
      IF(1 < il) THEN
          IJ = Icell(ic,   ir,   il-1)
          IF ( IJ > 0 ) sm = sm + Sn(IJ)
      END IF
      !
      IF(il < NLAY) THEN
          IJ = Icell(ic,   ir,   il+1)
          IF ( IJ > 0 ) sm = sm + Sn(IJ)
      END IF
      !
      ANS = sm < tol
      !
      END FUNCTION 
!
      SUBROUTINE Jacobian(kkiter,kper,kstp)
      USE CONSTANTS, ONLY: DZ, UNO
      USE GLOBAL, ONLY:Ncol, Nrow, Nlay, Ibound, Iout, Hnew, Botm,
     +                 Lbotm, CR, CC, CV
      USE GWFNWTMODULE
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      DOUBLE PRECISION, EXTERNAL :: GW_func, DVERT, DHORIZ
      EXTERNAL TEMPFILLUN,TEMPFILLCON
      INTEGER kkiter, kper
!     -----------------------------------------------------------------
!     LOCAL VARIABLES
!     -----------------------------------------------------------------
      DOUBLE PRECISION ferr, Adiag, DD, tolf2, hh
      DOUBLE PRECISION ttop, bbot, Dv, Dh, sum, tolf, botcheck
      DOUBLE PRECISION term1, term2, term3, term4, term5, term6, coef
      INTEGER ic, ir, il, icc, irr, ill
      INTEGER ij, iltyp, kstp, I, I1, I2, J, iDiag
      LOGICAL, EXTERNAL:: HFBROUTECELL                                  !seb
      LOGICAL:: HFBROUTE
!     -----------------------------------------------------------------
      !
      tolf = 1.0D-9
      !
! Set conductance derivatives.
      CALL Dcon(kkiter)
! Fill Jacobian
      DO ij = 1, Numactive
        !
        I1 = IA(ij)         !Diag index
        I2 = IA(ij+1)-1     !End of current row index
        !
        il = Diag(1, ij)
        ir = Diag(2, ij)
        ic = Diag(3, ij)
        !
        HFBROUTE=HFBROUTECELL(IC,IR,IL,IJ)                              !seb ESTABLISH IF CELL ROUTES FLOW THROUGH A BARRIER
        !
        hh          = HNEW(ic,ir,il)
        Hchange(ij) = hh
        !botcheck = dble(BOTM(ic,ir,il))
! Constant head cells.
        IF ( IBOUND(ic,ir,il).LT.0 ) THEN
                                     A(I1:I2) = DZ
                                     A(I1)    = UNO
                                     BB(ij)   = hh
! Variable head cells.
        ELSE
          iltyp = LAYTYPUPW(il)
          !
! DIAGONAL FIRST
C#######################################################################
C seb USE HFB Specific TEMPFILL Subroutines
          IF (HFBROUTE) THEN
            IF ( iltyp.GT.0 ) THEN
              CALL TEMPFILLUNHFB(ic, ir, il)
C              CALL ddSum(IC,IR,IL,IJ) !MAY NOT BE NEEDED
            ELSE 
              CALL TEMPFILLCONHFB(ic, ir, il)
            END IF
          ELSE
            IF ( iltyp.GT.0 ) THEN
              CALL TEMPFILLUN(ic, ir, il)
            ELSE 
              CALL TEMPFILLCON(ic, ir, il)
            END IF
          END IF
C#######################################################################
          IF ( iltyp.GT.0 ) THEN
            DO I = I1+1,I2
              ill = Diag(1, JA(I))
              irr = Diag(2, JA(I))
              icc = Diag(3, JA(I))
              dd = DZ
C#######################################################################
              IF     ( ill.LT.il.AND.irr.EQ.ir.AND.icc.EQ.ic ) THEN     !seb Rearranged order of if statements, added .AND. 
                dd = Dc(1,ij)
              ELSEIF ( ill.GT.il.AND.irr.EQ.ir.AND.icc.EQ.ic ) THEN
                dd = Dc(6,ij)
              ELSEIF ( irr.LT.ir ) THEN
                dd = Dc(2,ij)
              ELSEIF ( icc.LT.ic ) THEN
                dd = Dc(3,ij)
              ELSEIF ( icc.GT.ic ) THEN
                dd = Dc(4,ij)
              ELSEIF ( irr.GT.ir ) THEN
                dd = Dc(5,ij)
              ELSEIF ( ill.GT.il ) THEN !extra if block added by rgn
                dd = Dc(6,ij)
              END IF
              IF (abs(dd).GT.CLOSEZERO) 
     +                       A(I1) = A(I1) + dd*(HNEW(icc,irr,ill)-hh)
            END DO
          END IF
C#######################################################################
          !
          A(I1) = A(I1)-Cvm1-Ccm1-Crm1-Cvv-Ccc-Crr+Hcoff
! Calculate the right hand side
          ferr = -GW_func()
          BB(ij) = ferr
          BB(ij) = BB(ij) + A(I1)*hh
          sum  = DZ
          DO I = I1+1,I2
            J   = JA(I)
            ill = Diag(1, J)
            irr = Diag(2, J)
            icc = Diag(3, J)
C####################################################################### seb Rearranged order of if statements, added .AND. 
            IF ( ill.LT.il.AND.irr.EQ.ir.AND.icc.EQ.ic ) THEN
              A(I)   = (Cvm1 + Dc(6,J)*(Hvm1-H))                    ! J = Icell(ic ,ir ,il-1)
              BB(ij) = BB(ij) + A(I)*Hvm1
              !
            ELSEIF ( ill.GT.il.AND.irr.EQ.ir.AND.icc.EQ.ic ) THEN
              A(I)   = (Cvv + Dc(1,J)*(Hvp1-H))                     ! J = Icell(ic ,ir ,il+1)
              BB(ij) = BB(ij) + A(I)*Hvp1
              !
            ELSEIF ( irr.LT.ir ) THEN
              A(I)   = (Ccm1 + Dc(5,J)*(Hrm1-H))                    ! J = Icell(ic ,ir-1,il )
              BB(ij) = BB(ij) + A(I)*Hrm1
              !
            ELSEIF ( icc.LT.ic ) THEN
              A(I)   = (Crm1 + Dc(4,J)*(Hcm1-H))                    ! J = Icell(ic-1,ir ,il )
              BB(ij) = BB(ij) + A(I)*Hcm1
              !
            ELSEIF ( icc.GT.ic ) THEN
              A(I)   = (Crr  + Dc(3,J)*(Hcp1-H))                    ! J = Icell(ic+1,ir ,il )
              BB(ij) = BB(ij) + A(I)*Hcp1
              !
            ELSEIF ( irr.GT.ir ) THEN
              A(I)   = (Ccc  + Dc(2,J)*(Hrp1-H))                    ! J = Icell(ic ,ir+1,il )
              BB(ij) = BB(ij) + A(I)*Hrp1
              !
            ELSEIF ( ill.GT.il ) THEN                               ! Should never occur
              A(I)   = (Cvv + Dc(1,J)*(Hvp1-H))
              BB(ij) = BB(ij) + A(I)*Hvp1
            ENDIF
            !
            sum = sum + A(I)
          END DO
C#######################################################################
! Check for row values bad for linear solver
          sum   = ABS(sum)
          Adiag = ABS(A(I1))
          !                                tolf = 1.0D-9
          IF ( Adiag < tolf ) THEN
            A(I1)  = UNO
            BB(ij) = BB(ij) + hh
          ELSE IF ( sum < tolf ) THEN
            A(I1)  = A(I1)  + tolf
            BB(ij) = BB(ij) + tolf*hh
          END IF
        END IF
!        I1 = IA(ij)
!        I2 = IA(ij+1)-1
!!        if ( kper.gt.28)then
!!        if(ic==49.and.ir==236.and.il==1)then
!          WRITE(IOUT,66)ij,BB(ij),HNEW(ic,ir,il),BOTM(ic,ir,il-1),
!     +                BOTM(ic,ir,il),HCOFF,RHSS,(A(I),I=I1,I2)
!!        end if
!!        end if      
! 66      FORMAT(I9,1X,3G15.6,2X,11G15.6)
      END DO
      RETURN
      END SUBROUTINE Jacobian
!
      

