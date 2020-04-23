      MODULE LGRMODULE
         INTEGER, SAVE::ILGR 
         INTEGER, SAVE, POINTER ::ISCHILD,NGRDS,NPLBEG,NPRBEG,NPCBEG
         INTEGER, SAVE, POINTER ::NPLEND,NPREND,NPCEND,NCPP,NPL,IBOTFLG
         INTEGER, SAVE, POINTER ::ISHFLG,IBFLG,IUPBHSV,IUCBHSV
         INTEGER, SAVE, POINTER ::IUPBFSV,IUCBFSV,MXLGRITER,IOUTLGR 
         INTEGER, SAVE, POINTER ::NBNODES,NPBNODES
         INTEGER, SAVE, POINTER ::IBMAXH,IBMAXF,NCMAXH,NCMAXF
         INTEGER, SAVE, POINTER ::IFMPGRID                              !WSCHMID added IFMPGRID
         INTEGER, SAVE, POINTER ::ISFRGRID                              !WSCHMID added ISFRGRID
         REAL, SAVE, POINTER ::RELAXH,RELAXF,HCLOSELGR,FCLOSELGR
         REAL, SAVE, POINTER ::HDIFFM,FDIFFM
         REAL, SAVE, POINTER ::PRATIN,CRATIN,PRATOUT,CRATOUT 
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::IBPFLG
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::IEDG
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::JEDG
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NCPPL
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NODEH
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NODEF
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NCON
         INTEGER, SAVE, POINTER, DIMENSION(:,:)   ::KPLC
         INTEGER, SAVE, POINTER, DIMENSION(:,:)   ::IPLC
         INTEGER, SAVE, POINTER, DIMENSION(:,:)   ::JPLC
         INTEGER, SAVE, POINTER, DIMENSION(:,:)   ::IFACEGN
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NPINDX
         INTEGER, SAVE, POINTER, DIMENSION(:,:,:) ::ICBOUND
         REAL, SAVE,    POINTER, DIMENSION(:,:)   ::GNHEAD
         REAL, SAVE,    POINTER, DIMENSION(:,:)   ::DHGN
         REAL, SAVE,    POINTER, DIMENSION(:,:)   ::GNFLUX
         REAL, SAVE,    POINTER, DIMENSION(:,:)   ::GNFLUXR
         REAL, SAVE,    POINTER, DIMENSION(:,:)   ::GNFLUXOLD
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::HOLDC
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::CCC  !obsolete
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::CCR  !obsolete  
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::CCV  !obsolete  
         REAL, SAVE,    POINTER, DIMENSION(:,:)   ::GNCOND
         REAL, SAVE,    POINTER, DIMENSION(:)     ::VCB
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::HK
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::VK
        TYPE LGRTYPE
         INTEGER, POINTER ::ISCHILD,NGRDS,NPLBEG,NPRBEG,NPCBEG
         INTEGER, POINTER ::NPLEND,NPREND,NPCEND,NCPP,NPL,IBOTFLG
         INTEGER, POINTER ::ISHFLG,IBFLG,IUPBHSV,IUCBHSV
         INTEGER, POINTER ::IUPBFSV,IUCBFSV,MXLGRITER,IOUTLGR
         INTEGER, POINTER ::NBNODES,NPBNODES
         INTEGER, POINTER ::IBMAXH,IBMAXF,NCMAXH,NCMAXF
         INTEGER, POINTER ::IFMPGRID                                    !WSCHMID added IFMPGRID
         INTEGER, POINTER ::ISFRGRID                                    !WSCHMID added ISFRGRID
         REAL, POINTER ::RELAXH,RELAXF,HCLOSELGR,FCLOSELGR,HDIFFM,FDIFFM
         REAL, POINTER ::PRATIN,CRATIN,PRATOUT,CRATOUT 
         INTEGER, POINTER,    DIMENSION(:)     ::IBPFLG
         INTEGER, POINTER,    DIMENSION(:)     ::IEDG
         INTEGER, POINTER,    DIMENSION(:)     ::JEDG
         INTEGER, POINTER,    DIMENSION(:)     ::NCPPL
         INTEGER, POINTER,    DIMENSION(:)     ::NODEH
         INTEGER, POINTER,    DIMENSION(:)     ::NODEF
         INTEGER, POINTER,    DIMENSION(:)     ::NCON
         INTEGER, POINTER,    DIMENSION(:,:)   ::KPLC
         INTEGER, POINTER,    DIMENSION(:,:)   ::IPLC
         INTEGER, POINTER,    DIMENSION(:,:)   ::JPLC
         INTEGER, POINTER,    DIMENSION(:,:)   ::IFACEGN
         INTEGER, POINTER,    DIMENSION(:)     ::NPINDX
         INTEGER, POINTER,    DIMENSION(:,:,:) ::ICBOUND
         REAL,    POINTER,    DIMENSION(:,:)   ::GNHEAD
         REAL,    POINTER,    DIMENSION(:,:)   ::DHGN
         REAL,    POINTER,    DIMENSION(:,:)   ::GNFLUX
         REAL,    POINTER,    DIMENSION(:,:)   ::GNFLUXR
         REAL,    POINTER,    DIMENSION(:,:)   ::GNFLUXOLD
         REAL,    POINTER,    DIMENSION(:,:,:) ::HOLDC
         REAL,    POINTER,    DIMENSION(:,:,:) ::CCC     !obsolete
         REAL,    POINTER,    DIMENSION(:,:,:) ::CCR     !obsolete 
         REAL,    POINTER,    DIMENSION(:,:,:) ::CCV     !obsolete  
         REAL,    POINTER,    DIMENSION(:,:)   ::GNCOND
         REAL,    POINTER,    DIMENSION(:)     ::VCB
         REAL,    POINTER,    DIMENSION(:,:,:) ::HK
         REAL,    POINTER,    DIMENSION(:,:,:) ::VK
        END TYPE
        TYPE(LGRTYPE), SAVE  ::LGRDAT(10)
      CONTAINS
C***********************************************************************
      PURE SUBROUTINE SGWF2LGR2XYZLOC(J,I,K,NCOL,NROW,NLAY,DELR,DELC,   ! SCOTT THIS SUBROUTINE COULD BE REMOVED AND USE BUILD IN DISCOORD
     1                                BOTM,X,Y,Z)
C     ******************************************************************
C     FIND X, Y, AND Z COORDINATES OF CELL CENTERS.
C     (Y MEASURED FROM UPPER LEFT AND Z MEASURED FROM BOTTOM)
C       NOTE K REFERS TO THE LOCATION WITHIN BOTM THAT CONTAINS THE LAYER OF INTEREST (ie K=LBOTM(IL))
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
!      DIMENSION DELR(NCOL), DELC(NROW), BOTM(NCOL,NROW,0:NLAY)  NO LONGER NEEDED
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER,               INTENT(IN ):: J,I,K,NCOL,NROW,NLAY
      REAL,DIMENSION(:),     INTENT(IN ):: DELR,DELC
      REAL,DIMENSION(:,:,0:),INTENT(IN ):: BOTM
      REAL,                  INTENT(OUT):: X,Y,Z
      X = SUM(DELR(1:J)) - 0.5*DELR(J)
      Y = SUM(DELC(1:I)) - 0.5*DELC(I)
      Z = BOTM(J,I,K) + 0.5*(BOTM(J,I,K-1)-BOTM(J,I,K))
      RETURN
      END SUBROUTINE
C***********************************************************************
      END MODULE LGRMODULE
