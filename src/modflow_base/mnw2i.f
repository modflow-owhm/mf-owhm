      MODULE GWFMNW2IMODULE
       INTEGER,SAVE,POINTER  ::Wel1flag,QSUMflag,BYNDflag,MNWOBS
       CHARACTER(LEN=20),SAVE,DIMENSION(:),  POINTER,CONTIGUOUS::MNWIID
       DOUBLE PRECISION, SAVE,DIMENSION(:,:),POINTER,CONTIGUOUS::MNWILST
      TYPE GWFMNWITYPE
        INTEGER,POINTER  ::Wel1flag,QSUMflag,BYNDflag,MNWOBS
        CHARACTER(LEN=20),DIMENSION(:),   POINTER,CONTIGUOUS::MNWIID
        DOUBLE PRECISION, DIMENSION(:,:), POINTER,CONTIGUOUS::MNWILST
      END TYPE
      TYPE(GWFMNWITYPE), SAVE:: GWFMNWIDAT(10)
      END MODULE GWFMNW2IMODULE
c   GZH  20080208 
C   LFK  March 2011  Revision in GWF2MNW2I7OT to fix WEL1 output file for inactive wells.
C   LFK  Nov. 2012   Additional revisions--see read_me file.
C
C GWF2MNW2I7AR READ INIT DATA AND ALLOCATE SPACE FOR MNW WELLS DESIGNATED FOR OBSERVATION
C
C     ******************************************************************
C
      SUBROUTINE GWF2MNW2I7AR(INMNWI,INMNW2,IGRID)    
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,        ONLY:IOUT
      USE GWFMNW2IMODULE, ONLY:Wel1flag,QSUMflag,BYNDflag,MNWOBS,
     1                            MNWIID,MNWILST
      USE CONSTANTS, ONLY: Z, ONE, DZ, BLNK
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER
      CHARACTER(256):: LINE
      INTEGER:: LLOC,ISTART,ISTOP
C
      IF(INMNWI /= Z.AND.INMNW2 == Z) THEN
          CALL USTOP('MNWI PACKAGE CAN ONLY BE USED IF '//
     +               'MNW2 PACKAGE IS ACTIVE')
      END IF  
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(Wel1flag,QSUMflag,BYNDflag,MNWOBS)
C
      IF(INMNWI == Z) THEN
        LCMNIO = ONE
      ELSE
      CALL READ_TO_DATA(LINE,INMNWI,IOUT,IOUT,
     +                       HED="-- READING MNWI PACKAGE INPUT --")
      !
c     if transport on, read concflag
        !READ(INMNWI,*) Wel1flag,QSUMflag,BYNDflag
        LLOC = ONE
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,Wel1flag,
     +                             MSG='MNWI FAILED TO READ Wel1flag')
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,QSUMflag,
     +                             MSG='MNWI FAILED TO READ QSUMflag')
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,BYNDflag,
     +                             MSG='MNWI FAILED TO READ BYNDflag')
          WRITE(IOUT,*) 'MNWI Package input:'
          WRITE(IOUT,*) 'Wel1flag = ',Wel1flag
          WRITE(IOUT,*) 'QSUMflag = ',QSUMflag
          WRITE(IOUT,*) 'BYNDflag = ',BYNDflag
          WRITE(IOUT,*) 
C
        !READ(INMNWI,*) MNWOBS
        CALL READ_TO_DATA(LINE,INMNWI,IOUT)
        LLOC = ONE
        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,MNWOBS,
     +                             MSG='MNWI FAILED TO READ MNWOBS')
        IF(MNWOBS < Z) CALL USTOP('MNWOBS MUST BE >= 0')
C
C5------ALLOCATE SPACE FOR MNWILST ARRAY.
C5------FOR EACH OBS WELL, THERE ARE 6 DATA VALUES
      NMNWIVL=6
      IF(MNWOBS > Z) THEN
        ALLOCATE (MNWILST(NMNWIVL,MNWOBS))
C5------ALLOCATE SPACE FOR MNWIID ARRAY.
        ALLOCATE (MNWIID(MNWOBS+1))
      ELSE
        ALLOCATE (MNWILST(NMNWIVL,1))
C5------ALLOCATE SPACE FOR MNWIID ARRAY.
        ALLOCATE (MNWIID(1))
      END IF
      END IF
      !
      MNWILST = DZ
      MNWIID  = BLNK
C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2MNW2IPSV(IGRID)
C
      RETURN
      END
c
c_________________________________________________________________________________
c
C
C  GWF2MNW2I7RP READ INPUT FILE FOR MNW2 WELLS DESIGNATED FOR OBSERVATION 
C
C     ******************************************************************
C
      SUBROUTINE GWF2MNW2I7RP(INMNWI,GWTUNIT,IGRID)
C
C     ******************************************************************
C
C     READ LOCATIONS OF MNW2 WELLS DESIGNATED FOR OBSERVATION 
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT
      USE GWFMNW2MODULE, ONLY:MNWMAX,MNW2,WELLID
      USE GWFMNW2IMODULE, ONLY:Wel1flag,QSUMflag,BYNDflag,MNWOBS,
     1                       MNWILST,MNWIID
      USE CONSTANTS, ONLY: Z, ONE, TRUE, FALSE, DZ
      USE FILE_IO_INTERFACE, ONLY: READ_TO_DATA
      USE STRINGS,           ONLY: GET_INTEGER, GET_WORD
c     ------------------------------------------------------------------
      INTEGER GWTUNIT
      CHARACTER(20):: SITE,MSITE
      CHARACTER(512):: LINE
      INTEGER:: LLOC,ISTART,ISTOP
      INTEGER:: UNIT, QNDflag, QBHflag, CONCflag
      LOGICAL:: IS_SITE
C
C     ******************************************************************
C
      CALL SGWF2MNW2IPNT(IGRID)
C
      IF(MNWOBS < ONE) RETURN
      !
      IF(MNWOBS == ONE) THEN
                        WRITE (IOUT,120) MNWOBS
      ELSE
                        WRITE (IOUT,140) MNWOBS
      END IF
      !
      WRITE (IOUT,150)
      IF(MNWOBS > MNWMAX)  CALL USTOP('MNWI ERROR: MNWOBS > MNWMAX')
C
C  Initialize data array
      MNWILST = DZ
C READ THE FIRST RECORD
      IOB     = ONE
      IS_SITE = TRUE
c
c MNWILST(1,IOB) is Well # in MNW list
c MNWILST(2,IOB) is net volume in/out well
c MNWILST(3,IOB) is unit number for output
c MNWILST(4,IOB4) is QNDflag
c MNWILST(5,IOB) is QBHflag
c MNWILST(6,IOB) is CONCflag
c
      CALL READ_TO_DATA(LINE,INMNWI,IOUT)
      LLOC = ONE
      CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,MNWIID(IOB),NO_UPCASE=TRUE)
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,UNIT,
     +                          MSG='MNWI FAILED TO READ UNIT')
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,QNDflag,
     +                          MSG='MNWI FAILED TO READ QNDflag')
      !
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,QBHflag,
     +                          MSG='MNWI FAILED TO READ QBHflag')
      MNWILST(3,IOB)  = DBLE(UNIT)
      MNWILST(4,IOB)  = DBLE(QNDflag)
      MNWILST(5,IOB)  = DBLE(QBHflag)
      if(GWTUNIT /= Z) then
         CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,CONCflag,
     +                          MSG='MNWI FAILED TO READ CONCflag')
         MNWILST(6,IOB) = DBLE(CONCflag)
      end if
!      if(GWTUNIT /= Z) then
!       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
!     & MNWILST(5,IOB),MNWILST(6,IOB)
!      else
!       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
!     & MNWILST(5,IOB)
!       MNWILST(6,IOB) = Z
!      end if
      SITE = MNWIID(IOB)
      call UPCASE(SITE)
c check site vs list of site names in MNWSITE
c Loop over all MNW locations
c   Loop over all wells
      do iw=1, MNWMAX
        MSITE=WELLID(iw)
        call UPCASE(MSITE)
        IF(SITE == MSITE) THEN
          IS_SITE = FALSE
          MNWILST(1,IOB) = DBLE(iw)
          EXIT
        END IF
      end do      
C
      WRITE(IOUT,'(I8,3X,A20,3I8)') IOB,SITE,INT(MNWILST(3,IOB)),
     &                          INT(MNWILST(4,IOB)),INT(MNWILST(5,IOB)) 
      !
      IF(IS_SITE) CALL USTOP('MNWI WELLID '//MNWIID(IOB)//' NOT FOUND')
C CYCLE THROUGH THE REMAINING RECORDS
      DO IOB=2, MNWOBS
       
       CALL READ_TO_DATA(LINE,INMNWI,IOUT)
       LLOC = ONE
       CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,MNWIID(IOB),NO_UPCASE=TRUE)
       !
       CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,UNIT,
     +                            MSG='MNWI FAILED TO READ UNIT')
       !
       CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,QNDflag,
     +                            MSG='MNWI FAILED TO READ QNDflag')
       !
       CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,QBHflag,
     +                            MSG='MNWI FAILED TO READ QBHflag')
       MNWILST(3,IOB)  = DBLE(UNIT)
       MNWILST(4,IOB)  = DBLE(QNDflag)
       MNWILST(5,IOB)  = DBLE(QBHflag)
       if(GWTUNIT /= Z) then
           CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INMNWI,CONCflag,
     +                            MSG='MNWI FAILED TO READ CONCflag')
           MNWILST(6,IOB) = DBLE(CONCflag)
        end if
!      if(GWTUNIT /= Z) then
!       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
!     & MNWILST(5,IOB),MNWILST(6,IOB)
!      else
!       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
!     & MNWILST(5,IOB)
!       MNWILST(6,IOB) = Z
!      end if
c check site vs list of site names in WELLID
       SITE = MNWIID(IOB)
       call UPCASE(SITE)
c   Loop over all wells
       do iw=1, MNWMAX
          MSITE = WELLID(iw)
          call UPCASE(MSITE)
          IF(SITE == MSITE) THEN
            IS_SITE = FALSE
            MNWILST(1,IOB) = DBLE(iw)
            EXIT
          END IF
       end do      
C
       WRITE(IOUT,'(I8,3X,A20,3I8)') IOB,SITE,INT(MNWILST(3,IOB)),
     &       INT(MNWILST(4,IOB)),INT(MNWILST(5,IOB))
       !
       IF(IS_SITE) CALL USTOP('MNWI WELLID '//MNWIID(IOB)//' NOT FOUND')
C
      END DO
              WRITE(IOUT,'(140A)') 'DATA FOR MNW WELLS DESIGNATED FOR
     * OBSERVATION WILL BE WRITTEN ON UNIT NUMBERS LISTED ABOVE' 
              WRITE(IOUT,'(/)')
  120 FORMAT(///'SITE ID FOR',I4,
     * ' MNW2 WELL DESIGNATED FOR OBSERVATION:')
  140 FORMAT(///'SITE IDS FOR',I4,
     * ' MNW2 WELLS DESIGNATED FOR OBSERVATION:')
  150 FORMAT(/'  WELL #   SITE ID         UNIT  QNDflag QBHflag')
      RETURN
      END       
C
c_________________________________________________________________________________
c
      SUBROUTINE GWF2MNW2I7OT(nstp,kkstp,kkper,IGRID)
C     VERSION 20070923 GZH
c
c     ******************************************************************
c    Sort well output into useful tables
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,ncol,nrow,nlay,hnew
      USE GWFBASMODULE, ONLY:HDRY,DELT,TOTIM
      USE GWFMNW2MODULE, ONLY:MNWMAX,NMNWVL,MNWAUX,MNW2,WELLID,NODTOT,
     1                       NTOTNOD,MNWNOD,SMALL
      USE GWFMNW2MODULE,  ONLY: SGWF2MNW2PNT
      USE GWFMNW2IMODULE, ONLY:Wel1flag,QSUMflag,BYNDflag,MNWOBS,
     1                       MNWILST,MNWIID
      USE CONSTANTS, ONLY: Z, ONE, DZ, UNO
      ALLOCATABLE QBH(:)
      INTEGER firstnode,lastnode,QNDflag,QBHflag,QCONCflag,
     & iaux,naux
      DOUBLE PRECISION q,hwell,qin,qout,qnet,hcell,
     & QBH
c--LFK  Nov. 2012
      DOUBLE PRECISION COND,SEEPFLG
      CHARACTER(20) obssite
      CHARACTER(50) LFRMAT
c--lfk
      CHARACTER(50) dtext
c
c------------------------------------------------------------------
c-lfk  10/10/2012
      dtext = 'Hwell < value shown because of seepage face calc. '
c
      CALL SGWF2MNW2PNT(IGRID)
      CALL SGWF2MNW2IPNT(IGRID)
C
      ALLOCATE(QBH(NODTOT),STAT=ISTAT)
      IF (ISTAT /= Z) THEN
        WRITE(*,1700)ISTAT
 1700   FORMAT(1X,'ALLOCATION OF MNW ROUTING ARRAY FAILED,',
     &  ' RETURNED ERROR MESSAGE NUMBER: ',I6)
        CALL USTOP(' ')
      ENDIF
C
c Print WEL1 file
      if(Wel1flag > Z) then
c print max number of wells, set IWELCB = Z
C-LFK     CHECK IF AUXILIARY VARIABLES PRESENT, & WRITE THEM IF PRESENT
       NAUX=NMNWVL-30
       IF (NAUX <= Z) THEN
        if(kkper == ONE.and. kkstp == ONE)
     &  write(Wel1flag,'(2i10)') NODTOT, Z
       ELSE
        if(kkper == ONE .and. kkstp == ONE)
     &  write(Wel1flag,1750) NODTOT,Z,(mnwaux(iaux),iaux=1,naux)
 1750 FORMAT(2I10,2x,5(:(' AUX ',16A,2X)))
       END IF
c only write at end of stress period (nstp=kkstp)
        if(nstp.eq.kkstp) then
c write number of wells
          write(Wel1flag,'(i10)') ntotnod 
c   Loop over all wells
          do iw=1,MNWMAX
c   Loop over nodes in well
              firstnode=MNW2(4,iw)
              lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
              do INODE=firstnode,lastnode
                il=MNWNOD(1,INODE)              
                ir=MNWNOD(2,INODE)              
                ic=MNWNOD(3,INODE)              
                q=MNWNOD(4,INODE)
c LFK  check if well is inactive; if yes, set q at all nodes = 0 to assure consistent WEL1 file.
                if(NINT(MNW2(1,iw)) == Z) q = DZ
c   Write L,R,C,q (& OPTIONAL AUX values)
               IF (NAUX <= Z) THEN
c  LFK 3/26/12                write(Wel1flag,'(i9,2i10,1x,g15.6)') il,ir,ic,q       
                write(Wel1flag,'(i10,2i10,1x,g14.6)') il,ir,ic,q       
               ELSE
        write(Wel1flag,1760)il,ir,ic,q,(MNW2(30+IAUX,IW),IAUX=1,NAUX)  
c LFK 1760 FORMAT (i9,2i10,1x,g15.6,2X,5(:(f4.0,4x)))
 1760 FORMAT (i10,2i10,1x,g14.6,2X,5(:(f4.0,4x)))
               END IF
              end do
          end do
        end if
      end if
c  Print QSUM file
      if(QSUMflag > Z) then
c   Write header
        if(kkstp == ONE .and. kkper == ONE) then
           write(QSUMflag,'(200A)') 'WELLID                    Totim    
     &        Qin           Qout           Qnet          hwell'
        end if
c   Loop over all wells
        do iw=1,MNWMAX
          qin = DZ
          qout= DZ
          qnet= DZ
c   Only operate on active wells (MNW2(1,iw)=1)
          if (NINT(MNW2(1,iw)) == ONE) then
c   Loop over nodes in well
c--lfk
            NNDSIW=ABS(MNW2(2,iw))
c-lfk
            firstnode=MNW2(4,iw)
C            lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
            lastnode=MNW2(4,iw)+NNDSIW-1
            hwell=MNW2(17,iw)
            do INODE=firstnode,lastnode
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)              
              q=MNWNOD(4,INODE)
C--LFK
              HCELL=hnew(ic,ir,il)
              SEEPFLG=MNWNOD(15,INODE)
              COND=MNWNOD(14,INODE)
C-LFK
              if(q < DZ) then
                qin=qin+q
              else
                qout=qout+q
              end if
              qnet=qnet+q
            end do
c            if(qnet.lt.(small*qin)) qnet=0.d0
c--lfk  Nov. 2012
              if(SEEPFLG.EQ.hwell.or.
     &           SEEPFLG.eq.Hdry) then
                  write(QSUMflag,'(A20,5(1x,ES14.6))')
     &              WELLID(iw),totim,qin,qout,qnet,hwell
              else
c   If seepage face in cell, MNWNOD(15) will hold the bottom elev of the cell
c--LFK  update Hwell to realistic value (not Hnew)for single-node MNW wells
                if (NNDSIW == ONE) then
                   hwell=HCELL+(q/COND)
                   write(QSUMflag,'(A20,5(1x,ES14.6),3x,A50)')
     &              WELLID(iw),totim,qin,qout,qnet,hwell,dtext
                else
            write(QSUMflag,'(A20,5(1x,ES14.6))')
     &              WELLID(iw),totim,qin,qout,qnet,hwell
            end if
           end if
c--LFK
          end if
        end do     
      end if
c   Print BYND (ByNode) file
      if(BYNDflag > Z) then
c   Write header
        if(kkstp == ONE.and.kkper == ONE) then
           write(BYNDflag,'(101A)') 'WELLID                NODE   Lay 
     &  Row   Col        Totim        Q-node         hwell         hcell
     &   Seepage_elev.'
        end if
c   Loop over all wells
        do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
          if (NINT(MNW2(1,iw)) == ONE) then
c   Loop over nodes in well
            firstnode=MNW2(4,iw)
            lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
            hwell=MNW2(17,iw)
            do INODE=firstnode,lastnode
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)              
              q=MNWNOD(4,INODE)
              hcell=hnew(ic,ir,il)
              nd=INODE-firstnode+1
c   If no seepage face in cell, don't print seepage elev.
              if(MNWNOD(15,INODE).EQ.hwell.or.
c--lfk  Nov. 2012
     &           MNWNOD(15,INODE).eq.Hdry) then
c--lfk     &           MNWNOD(15,INODE).eq.Hdry.OR.MNW2(2,iw).eq.1) then
                write(BYNDflag,'(A20,4i6,1x,1P4e14.6)')
     &              WELLID(iw),nd,il,ir,ic,totim,q,hwell,hcell
              else
c   If seepage face in cell, MNWNOD(15) will hold the bottom elev of the cell,
c   which is used with hcell to get the gradient used to calculate the Q for the
c   seepage face.  
c--LFK  update Hwell to realistic value (not Hnew)for single-node MNW wells
                if (firstnode.eq.lastnode) then
                    hwell=hcell+(q/MNWNOD(14,INODE))
                    write(BYNDflag,'(A20,4i6,1x,1P5e14.6,3x,A50)')
     &                WELLID(iw),nd,il,ir,ic,totim,q,hwell,hcell,
     &                MNWNOD(15,INODE),dtext
                else
                write(BYNDflag,'(A20,4i6,1x,1P5e14.6)')
     &              WELLID(iw),nd,il,ir,ic,totim,q,hwell,hcell,
     &              MNWNOD(15,INODE)
              end if
c--LFK
              end if
            end do
         end if
        end do           
      end if
c
c  Print MNWOBS files
      do iwobs=1,MNWOBS    
        qnet = DZ
        obssite=MNWIID(iwobs)
        iw=MNWILST(1,iwobs)  
c   Loop over nodes in well
c--lfk  Nov. 2012
          NNDSIW=ABS(MNW2(2,iw))
c-lfk
          firstnode=MNW2(4,iw)
C            lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
          lastnode=MNW2(4,iw)+NNDSIW-1
          hwell=MNW2(17,iw)
          qin = DZ
          qout= DZ
          do INODE=firstnode,lastnode
C--LFK
             if (NINT(MNW2(1,iw)) == ONE) then
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)              
              HCELL=hnew(ic,ir,il)
              SEEPFLG=MNWNOD(15,INODE)
              COND=MNWNOD(14,INODE)
             end if
C-LFK
            q=MNWNOD(4,INODE)
            if(q < DZ) then
              qin=qin+q
            else
              qout=qout+q
            end if
            qnet=qnet+q
          end do
c--lfk  Nov. 2012
             if (NINT(MNW2(1,iw)) == ONE) then
              sftest = DZ
              if(SEEPFLG.ne.hwell.and.SEEPFLG.ne.Hdry) then
c   If seepage face in cell, MNWNOD(15) [=seepflg] will hold the bottom elev of the cell
c--LFK  update Hwell to realistic value (not Hnew)for single-node MNW wells
                if (NNDSIW == ONE) then
                   hwell=HCELL+(q/COND)
                   sftest = UNO
                end if
              end if
             end if
c--LFK
c   Cumulative volume for this well
          MNWILST(2,iwobs)=MNWILST(2,iwobs)+qnet*DELT
            
c get NNODES
          NNODES=INT(ABS(MNW2(2,iw)))
c
c  Print according to flags
          QNDflag=INT(MNWILST(4,iwobs))
          QBHflag=INT(MNWILST(5,iwobs))
          QCONCflag=INT(MNWILST(6,iwobs))
c--LFK(Nov.2016)--make sure QNDflag and QBHflag = 0 when NNODES=1 
c         (as described on p. 53 of model documentation).
          IF(NNODES == ONE) THEN
             IF(QNDflag > Z) then
               QNDflag = Z
               MNWILST(4,iwobs) = DZ
               write(iout,30) WELLID(iwobs)
   30 FORMAT('**note** NNODES=1 IN WELL: ',A20,'; QNDflag reset to 0.')    
             END IF  
              IF(QBHflag > Z) then
               QBHflag = Z
               MNWILST(5,iwobs) = DZ
               write(iout,32) WELLID(iwobs)
   32 FORMAT('**note** NNODES=1 IN WELL: ',A20,'; QBHflag reset to 0.')    
             END IF  
C            
          END IF
c
          if(QBHflag > Z)  
     &      call GWF2MNW27BH(iw,IGRID)
c
C  Create format for header--single node
C  Ignore ND and BH flags
C  Create format for header--single node
c--lfk/Nov.2012:  eliminate separate processing for single-node MNW wells
c          if(NNODES.eq.1) then
c            if(kkstp.eq.1.and.kkper.eq.1) then
c          Write (INT(MNWILST(3,iwobs)),'(A)') 'WELLID                
c     &       TOTIM           Qin
c     &          Qout          Qnet         QCumu         hwell'
c            end if
c            write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6)')
c     &             WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell
c          else
c  all multi-node well output below
          if(QNDflag == Z) then
            if(QBHflag == Z) then
              if(QCONCflag == Z) then            
c  QNDflag=0, QBHflag=0, QCONCflag=0
c write header
                if(kkstp == ONE.and.kkper == ONE) then
          Write (INT(MNWILST(3,iwobs)),'(120A)') 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet      Cum.Vol.         hwell
     &   '
                end if
c   Only operate on active wells (MNW2(1,iw)=1)
                if (NINT(MNW2(1,iw)) == ONE) then
c--lfk
                  if (sftest.lt.1.0) then
                write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6)')
     &             WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell
                  else
                 write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6,3x,A50)')
     &             WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell
     &             ,dtext
                  end if
                else
c-lfk-Nov 2012
                  write(INT(MNWILST(3,iwobs)),
     &             '(A20,1x,1Pe14.6,"   (Well is inactive)")')
     &             WELLID(iw),totim
                end if
              else
c  QNDflag=0, QBHflag=0, QCONCflag=1
c write header
                if(kkstp == ONE.and.kkper == ONE) then
          Write (INT(MNWILST(3,iwobs)),'(120A)') 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     &   CONC'
                end if
cgzh debug  concflag not coded yet...separate routine for when GWT active?                
                if (NINT(MNW2(1,iw)) == ONE) then
                 write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6,3A)')
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &  'N/A'
c
                end if
              end if
            else
              if(QCONCflag == Z) then            
c  QNDflag=0, QBHflag=1, QCONCflag=0
c write "smart" header
                if(kkstp == ONE.and.kkper == ONE) then
C  Create format for header  
            WRITE(LFRMAT,14) NNODES-1
  14  FORMAT('(A,',I4,'I12)')
C  WRITE FORMAT FOR HEADER LINE
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & QBH_seg-->1',(ibh,ibh=2,NNODES)
                end if
                if (NINT(MNW2(1,iw)) == ONE) then

C Create format for write
                WRITE (LFRMAT,355) NNODES
  355           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))')
             write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(27,i),i=firstnode,lastnode)
                end if
              else
c  QNDflag=0, QBHflag=1, QCONCflag=1
c write header
                if(kkstp == ONE.and.kkper == ONE) then
C  Create format for header  
            WRITE(LFRMAT,15) NNODES-1
  15  FORMAT('(A,',I4,'I12,A)')
C  WRITE FORMAT FOR HEADER LINE
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & QBH_seg-->1',(ibh,ibh=2,NNODES),'     CONC'
                end if
                if (NINT(MNW2(1,iw)) == ONE) then
C Create format for write
                WRITE (LFRMAT,255) NNODES
  255           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))',3A)
                write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(27,i),i=firstnode,lastnode),'N/A'
                end if
              end if
            end if  
          else
            if(QBHflag == Z) then
              if(QCONCflag == Z) then            
c  QNDflag=1, QBHflag=0, QCONCflag=0
c write "smart" header
                if(kkstp == ONE.and.kkper == ONE) then
C  Create format for header  
            WRITE(LFRMAT,14) NNODES-1
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES)
                end if
                if (NINT(MNW2(1,iw)) == ONE) then
C Create format for write
                WRITE (LFRMAT,155) NNODES
  155           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))')

                write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode)
                end if
              else
c  QNDflag=1, QBHflag=0, QCONCflag=1
c
c write "smart" header
                if(kkstp == ONE.and.kkper == ONE) then
C  Create format for header  
            WRITE(LFRMAT,15) NNODES-1
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES),'   CONC'
                end if
                if (NINT(MNW2(1,iw)) == ONE) then
C Create format for write
                WRITE (LFRMAT,455) NNODES
  455           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4)),3A')

                write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode),'N/A'
c
                end if
              end if
            else
              if(QCONCflag == Z) then            
c  QNDflag=1, QBHflag=1, QCONCflag=0
C Create format for write

c write "smart" header
                if(kkstp == ONE.and.kkper == ONE) then
C  Create format for header  
          WRITE(LFRMAT,16) NNODES-1,NNODES-1
  16  FORMAT('(A,',I4,'I12,A,',I4,'I12)')
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES),
     &' QBH_seg-->1',(ibh,ibh=2,NNODES)
                end if
                if (NINT(MNW2(1,iw)) == ONE) then
       numvals=NNODES+NNODES
               WRITE (LFRMAT,156) numvals
  156          FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))')
                  write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode),
     &              (MNWNOD(27,i),i=firstnode,lastnode)

                end if
              else
c  QNDflag=1, QBHflag=1, QCONCflag=1
       if(kkstp == ONE.and.kkper == ONE) then
          WRITE(LFRMAT,17) NNODES-1,NNODES-1
  17  FORMAT('(A,',I4,'I12,A,',I4,'I12,A)')
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES),
     &' QBH_seg-->1',(ibh,ibh=2,NNODES),'   CONC'
        end if
                if (NINT(MNW2(1,iw)) == ONE) then
                WRITE (LFRMAT,456) NNODES, 
     & NNODES
  456          FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))',I4,'(1pE12.4))')
                  write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode),
     &              (MNWNOD(27,i),i=firstnode,lastnode),'N/A'
                end if
              end if
            end if  
          end if  
c-lfk-Nov. 2012          end if   
      end do
c
      return
      end
C
      SUBROUTINE GWF2MNW2I7DA(IGRID)
C  Deallocate MNW MEMORY
      USE GWFMNW2IMODULE
        DEALLOCATE(GWFMNWIDAT(IGRID)%Wel1flag)
        DEALLOCATE(GWFMNWIDAT(IGRID)%QSUMflag)
        DEALLOCATE(GWFMNWIDAT(IGRID)%BYNDflag)
        DEALLOCATE(GWFMNWIDAT(IGRID)%MNWOBS  )
        DEALLOCATE(GWFMNWIDAT(IGRID)%MNWIID  )
        DEALLOCATE(GWFMNWIDAT(IGRID)%MNWILST )
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.EQ.1)THEN
        Wel1flag=>NULL()
        QSUMflag=>NULL()
        BYNDflag=>NULL()
        MNWOBS  =>NULL()
        MNWIID  =>NULL()
        MNWILST =>NULL()
      END IF
C
      RETURN
      END
      SUBROUTINE SGWF2MNW2IPNT(IGRID)
C  Change MNW data to a different grid.
      USE GWFMNW2IMODULE
C
        Wel1flag=>GWFMNWIDAT(IGRID)%Wel1flag
        QSUMflag=>GWFMNWIDAT(IGRID)%QSUMflag
        BYNDflag=>GWFMNWIDAT(IGRID)%BYNDflag
        MNWOBS=>GWFMNWIDAT(IGRID)%MNWOBS
        MNWIID=>GWFMNWIDAT(IGRID)%MNWIID
        MNWILST=>GWFMNWIDAT(IGRID)%MNWILST
C
      RETURN
      END
      SUBROUTINE SGWF2MNW2IPSV(IGRID)
C  Save MNW2 data for a grid.
      USE GWFMNW2IMODULE
C
        GWFMNWIDAT(IGRID)%Wel1flag=>Wel1flag
        GWFMNWIDAT(IGRID)%QSUMflag=>QSUMflag
        GWFMNWIDAT(IGRID)%BYNDflag=>BYNDflag
        GWFMNWIDAT(IGRID)%MNWOBS=>MNWOBS
        GWFMNWIDAT(IGRID)%MNWIID=>MNWIID
        GWFMNWIDAT(IGRID)%MNWILST=>MNWILST
C
      RETURN
      END
