MODULE GWFNWTMODULE
  IMPLICIT NONE                                                     
  DOUBLE PRECISION, PARAMETER:: HEPS = 1.0E-7                      
  DOUBLE PRECISION, PARAMETER:: CLOSEZERO = 1.0E-15
  DOUBLE PRECISION, PARAMETER:: BIG = 1.0D20 
  DOUBLE PRECISION, PARAMETER:: SMALL = 1.0D-5              
  DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER,CONTIGUOUS :: A
  DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS :: Dc
  DOUBLE PRECISION, SAVE, DIMENSION(:,:,:),POINTER,CONTIGUOUS::Hiter
  DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER,CONTIGUOUS:: BB, Hchange
  DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER,CONTIGUOUS:: Hchold, Wsave
  DOUBLE PRECISION, SAVE, POINTER :: Cvm1, Hvm1                     
  DOUBLE PRECISION, SAVE, POINTER :: W, Theta                       
  DOUBLE PRECISION, SAVE, POINTER :: Akappa, Gamma, Amomentum       
  DOUBLE PRECISION, SAVE, POINTER :: Hvp1, Crm1                     
  DOUBLE PRECISION, SAVE, POINTER :: Hrm1, Hrp1, Ccm1               
  DOUBLE PRECISION, SAVE, POINTER :: Hcm1, Hcp1                     
  DOUBLE PRECISION, SAVE, POINTER :: Ccc, Crr, Cvv, H               
  DOUBLE PRECISION, SAVE, POINTER :: Hcoff, Rhss, Fflux, Fhead
  DOUBLE PRECISION, SAVE, POINTER :: Fheadsave
  INTEGER, SAVE, POINTER :: Numnonzero, II, Itreal, Ibt, NJA
  INTEGER, SAVE, POINTER :: IFDPARAM, ICNVGFLG
  INTEGER, SAVE, POINTER :: Btrack, Iierr
  DOUBLE PRECISION, SAVE, POINTER :: Tol, Ftol, RELAX, RMS2, RMS1
  DOUBLE PRECISION, SAVE, POINTER :: Thickfact, Breduc, Btol, RMSAVE
  INTEGER, SAVE, POINTER :: Numactive, Numcell   
  INTEGER, SAVE, POINTER :: Nonmeth
  INTEGER, SAVE, POINTER :: Linmeth
  INTEGER, SAVE, POINTER :: IPRNWT
  INTEGER, SAVE, POINTER :: IBOTAV
  INTEGER, SAVE, POINTER :: ITER1,Numtrack 
  INTEGER, SAVE, DIMENSION(:), POINTER,CONTIGUOUS:: IA, JA
  INTEGER, SAVE, DIMENSION(:, :), POINTER,CONTIGUOUS:: Diag
  INTEGER, SAVE, DIMENSION(:, :, :), POINTER,CONTIGUOUS:: Icell
  INTEGER, SAVE, POINTER:: itertot
  !
  TYPE GWFNWTTYPE                                                   
    DOUBLE PRECISION, DIMENSION(:), POINTER,CONTIGUOUS :: A
    DOUBLE PRECISION, DIMENSION(:,:), POINTER,CONTIGUOUS :: Dc
    DOUBLE PRECISION, DIMENSION(:, :, :), POINTER,CONTIGUOUS:: Hiter  
    DOUBLE PRECISION, DIMENSION(:), POINTER,CONTIGUOUS:: BB, Hchange
    DOUBLE PRECISION,DIMENSION(:),POINTER,CONTIGUOUS:: Hchold, Wsave        
    DOUBLE PRECISION, POINTER :: Cvm1, Hvm1                         
    DOUBLE PRECISION, POINTER :: Hvp1, Crm1                         
    DOUBLE PRECISION, POINTER :: Hrm1, Hrp1, Ccm1                   
    DOUBLE PRECISION, POINTER :: Hcm1, Hcp1                         
    DOUBLE PRECISION, POINTER :: Ccc, Crr, Cvv, H                   
    DOUBLE PRECISION, POINTER :: W, Theta                           
    DOUBLE PRECISION, POINTER :: Akappa, Gamma, Amomentum           
    DOUBLE PRECISION, POINTER :: Hcoff, Rhss, Fflux, Fhead
    DOUBLE PRECISION, POINTER :: Fheadsave
    INTEGER, POINTER :: Numnonzero, II, Itreal, Ibt, NJA
    INTEGER, POINTER :: IFDPARAM, ICNVGFLG
    DOUBLE PRECISION, POINTER :: Tol, Ftol, RMS2, RMS1
    DOUBLE PRECISION, POINTER :: Thickfact, Breduc, Btol, RMSAVE
    INTEGER, POINTER :: Numactive, Numcell
    INTEGER, POINTER :: Nonmeth
    INTEGER, POINTER :: Linmeth
    INTEGER, POINTER :: IPRNWT
    INTEGER, POINTER :: IBOTAV
    INTEGER, POINTER :: Btrack, Iierr                 
    INTEGER, POINTER :: ITER1,Numtrack
    INTEGER, DIMENSION(:), POINTER,CONTIGUOUS :: IA, JA                   
    INTEGER, DIMENSION(:, :), POINTER,CONTIGUOUS :: Diag
    INTEGER, DIMENSION(:, :, :), POINTER,CONTIGUOUS :: Icell
    INTEGER,                     POINTER:: itertot
  END TYPE GWFNWTTYPE                                               
  TYPE (GWFNWTTYPE) , SAVE::Gwfnwtdat(10)                           
END MODULE GWFNWTMODULE 
!
!
      SUBROUTINE GWF2NWT1DA(Igrid)
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid    
!     ------------------------------------------------------------------
! Deallocate NWT data.
      DEALLOCATE (GWFNWTDAT(IGRID)%IA)
      DEALLOCATE (GWFNWTDAT(IGRID)%JA)
      DEALLOCATE (GWFNWTDAT(IGRID)%DIAG)
      DEALLOCATE (GWFNWTDAT(IGRID)%ICELL)      
      DEALLOCATE (GWFNWTDAT(IGRID)%A)
      DEALLOCATE (GWFNWTDAT(IGRID)%DC)
      DEALLOCATE (GWFNWTDAT(IGRID)%BB)
      DEALLOCATE (GWFNWTDAT(IGRID)%HCHANGE)
      DEALLOCATE (GWFNWTDAT(IGRID)%NUMNONZERO)
      DEALLOCATE (GWFNWTDAT(IGRID)%NJA)
      DEALLOCATE (GWFNWTDAT(IGRID)%ITREAL)
      DEALLOCATE (GWFNWTDAT(IGRID)%IBT)
      DEALLOCATE (GWFNWTDAT(IGRID)%II)
      DEALLOCATE (GWFNWTDAT(IGRID)%IFDPARAM) 
      DEALLOCATE (GWFNWTDAT(IGRID)%ICNVGFLG)
      DEALLOCATE (GWFNWTDAT(IGRID)%TOL)
      DEALLOCATE (GWFNWTDAT(IGRID)%FTOL)
      DEALLOCATE (GWFNWTDAT(IGRID)%ITER1)     
      DEALLOCATE (GWFNWTDAT(IGRID)%CVM1)
      DEALLOCATE (GWFNWTDAT(IGRID)%HVM1)
      DEALLOCATE (GWFNWTDAT(IGRID)%HVP1)
      DEALLOCATE (GWFNWTDAT(IGRID)%CRM1)
      DEALLOCATE (GWFNWTDAT(IGRID)%HRM1)
      DEALLOCATE (GWFNWTDAT(IGRID)%HRP1)
      DEALLOCATE (GWFNWTDAT(IGRID)%CCM1)
      DEALLOCATE (GWFNWTDAT(IGRID)%HCM1)
      DEALLOCATE (GWFNWTDAT(IGRID)%HCP1)
      DEALLOCATE (GWFNWTDAT(IGRID)%CCC)
      DEALLOCATE (GWFNWTDAT(IGRID)%CRR)
      DEALLOCATE (GWFNWTDAT(IGRID)%CVV)
      DEALLOCATE (GWFNWTDAT(IGRID)%H)
      DEALLOCATE (GWFNWTDAT(IGRID)%HCOFF)
      DEALLOCATE (GWFNWTDAT(IGRID)%RHSS)
      DEALLOCATE (GWFNWTDAT(IGRID)%FHEAD)
      DEALLOCATE (GWFNWTDAT(IGRID)%FHEADSAVE)
      DEALLOCATE (GWFNWTDAT(IGRID)%FFLUX)
      DEALLOCATE (GWFNWTDAT(IGRID)%HCHOLD)
      DEALLOCATE (GWFNWTDAT(IGRID)%NUMACTIVE)
      DEALLOCATE (GWFNWTDAT(IGRID)%NUMCELL)
      DEALLOCATE (GWFNWTDAT(IGRID)%W)
      DEALLOCATE (GWFNWTDAT(IGRID)%HITER)
      DEALLOCATE (GWFNWTDAT(IGRID)%WSAVE)
      DEALLOCATE (GWFNWTDAT(IGRID)%THETA)
      DEALLOCATE (GWFNWTDAT(IGRID)%AKAPPA)
      DEALLOCATE (GWFNWTDAT(IGRID)%GAMMA)
      DEALLOCATE (GWFNWTDAT(IGRID)%AMOMENTUM)
      DEALLOCATE (GWFNWTDAT(IGRID)%BTRACK)
      DEALLOCATE (GWFNWTDAT(IGRID)%BTOL)
      DEALLOCATE (GWFNWTDAT(IGRID)%RMSAVE)
      DEALLOCATE (GWFNWTDAT(IGRID)%THICKFACT)
      DEALLOCATE (GWFNWTDAT(IGRID)%NUMTRACK)
      DEALLOCATE (GWFNWTDAT(IGRID)%RMS2) 
      DEALLOCATE (GWFNWTDAT(IGRID)%RMS1) 
      DEALLOCATE (GWFNWTDAT(IGRID)%IIERR)
      DEALLOCATE (GWFNWTDAT(IGRID)%NONMETH)
      DEALLOCATE (GWFNWTDAT(IGRID)%LINMETH)
      DEALLOCATE (GWFNWTDAT(IGRID)%IPRNWT)
      DEALLOCATE (GWFNWTDAT(IGRID)%IBOTAV)
      DEALLOCATE (GWFNWTDAT(IGRID)%itertot  )
      
      
      IF(IGRID.EQ.1)THEN
        IA        =>NULL()
        JA        =>NULL()
        DIAG      =>NULL()
        ICELL     =>NULL() 
        A         =>NULL()
        DC        =>NULL()
        BB        =>NULL()
        HCHANGE   =>NULL()
        NUMNONZERO=>NULL()
        NJA       =>NULL()
        ITREAL    =>NULL()
        IBT       =>NULL()
        II        =>NULL()
        IFDPARAM  =>NULL()
        ICNVGFLG  =>NULL()
        TOL       =>NULL()
        FTOL      =>NULL()
        ITER1     =>NULL()
        CVM1      =>NULL()
        HVM1      =>NULL()
        HVP1      =>NULL()
        CRM1      =>NULL()
        HRM1      =>NULL()
        HRP1      =>NULL()
        CCM1      =>NULL()
        HCM1      =>NULL()
        HCP1      =>NULL()
        CCC       =>NULL()
        CRR       =>NULL()
        CVV       =>NULL()
        H         =>NULL()
        HCOFF     =>NULL()
        RHSS      =>NULL()
        FHEAD     =>NULL()
        FHEADSAVE =>NULL()
        FFLUX     =>NULL()
        HCHOLD    =>NULL()
        NUMACTIVE =>NULL()
        NUMCELL   =>NULL()
        W         =>NULL()
        HITER     =>NULL()
        WSAVE     =>NULL()
        THETA     =>NULL()
        AKAPPA    =>NULL()
        GAMMA     =>NULL()
        AMOMENTUM =>NULL()
        BTRACK    =>NULL()
        BTOL      =>NULL()
        RMSAVE    =>NULL()
        THICKFACT =>NULL()
        NUMTRACK  =>NULL()
        RMS2      =>NULL()
        RMS1      =>NULL()
        IIERR     =>NULL()
        NONMETH   =>NULL()
        LINMETH   =>NULL()
        IPRNWT    =>NULL()
        IBOTAV    =>NULL()
        itertot   =>NULL() 
      END IF
      END SUBROUTINE GWF2NWT1DA
 
 
 
      SUBROUTINE SGWF2NWT1PNT(Igrid)
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid   
!     ------------------------------------------------------------------
! Set NWT pointers for grid.
      IA => Gwfnwtdat(Igrid)%IA
      JA => Gwfnwtdat(Igrid)%JA
      Diag => Gwfnwtdat(Igrid)%Diag
      Icell => Gwfnwtdat(Igrid)%Icell     
      A => Gwfnwtdat(Igrid)%A
      Dc => Gwfnwtdat(Igrid)%Dc
      BB => Gwfnwtdat(Igrid)%BB
      Hchange => Gwfnwtdat(Igrid)%Hchange
      Numnonzero => Gwfnwtdat(Igrid)%Numnonzero
      NJA => Gwfnwtdat(Igrid)%NJA
      Itreal => Gwfnwtdat(Igrid)%Itreal
      Ibt => Gwfnwtdat(Igrid)%Ibt
      II => Gwfnwtdat(Igrid)%II
      IFDPARAM => Gwfnwtdat(Igrid)%IFDPARAM   
      ICNVGFLG => Gwfnwtdat(Igrid)%ICNVGFLG
      Tol => Gwfnwtdat(Igrid)%Tol
      Ftol => Gwfnwtdat(Igrid)%Ftol                           
      ITER1 => Gwfnwtdat(Igrid)%ITER1
      Cvm1 => Gwfnwtdat(Igrid)%Cvm1
      Hvm1 => Gwfnwtdat(Igrid)%Hvm1
      Hvp1 => Gwfnwtdat(Igrid)%Hvp1
      Crm1 => Gwfnwtdat(Igrid)%Crm1
      Hrm1 => Gwfnwtdat(Igrid)%Hrm1
      Hrp1 => Gwfnwtdat(Igrid)%Hrp1
      Ccm1 => Gwfnwtdat(Igrid)%Ccm1
      Hcm1 => Gwfnwtdat(Igrid)%Hcm1
      Hcp1 => Gwfnwtdat(Igrid)%Hcp1
      Ccc => Gwfnwtdat(Igrid)%Ccc
      Crr => Gwfnwtdat(Igrid)%Crr
      Cvv => Gwfnwtdat(Igrid)%Cvv
      H => Gwfnwtdat(Igrid)%H
      Hcoff => Gwfnwtdat(Igrid)%Hcoff
      Rhss => Gwfnwtdat(Igrid)%Rhss
      Fhead => Gwfnwtdat(Igrid)%Fhead
      Fheadsave => Gwfnwtdat(Igrid)%Fheadsave
      Fflux => Gwfnwtdat(Igrid)%Fflux
      Hchold => Gwfnwtdat(Igrid)%Hchold
      Numactive => Gwfnwtdat(Igrid)%Numactive
      Numcell => Gwfnwtdat(Igrid)%Numcell
      W => Gwfnwtdat(Igrid)%W
      Hiter => Gwfnwtdat(Igrid)%Hiter
      Wsave => Gwfnwtdat(Igrid)%Wsave
      Theta => Gwfnwtdat(Igrid)%Theta
      Akappa => Gwfnwtdat(Igrid)%Akappa
      Gamma => Gwfnwtdat(Igrid)%Gamma
      Amomentum => Gwfnwtdat(Igrid)%Amomentum
      Btrack => Gwfnwtdat(Igrid)%Btrack
      Btol => Gwfnwtdat(Igrid)%Btol
      RMSAVE => Gwfnwtdat(Igrid)%RMSAVE
      Thickfact => Gwfnwtdat(Igrid)%Thickfact
      Numtrack => Gwfnwtdat(Igrid)%Numtrack
      RMS2 => Gwfnwtdat(Igrid)%RMS2 
      RMS1 => Gwfnwtdat(Igrid)%RMS1   
      Iierr => Gwfnwtdat(Igrid)%Iierr
      Nonmeth => Gwfnwtdat(Igrid)%Nonmeth
      Linmeth => Gwfnwtdat(Igrid)%Linmeth 
      IPRNWT => Gwfnwtdat(Igrid)%IPRNWT
      IBOTAV => Gwfnwtdat(Igrid)%IBOTAV  
      itertot   => GWFNWTDAT(IGRID)%itertot  
      END SUBROUTINE SGWF2NWT1PNT
!
      SUBROUTINE SGWF2NWT1PSV(Igrid)
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid
!     ------------------------------------------------------------------
! Save NWT pointers for grid.
!
      Gwfnwtdat(Igrid)%IA => IA
      Gwfnwtdat(Igrid)%JA => JA
      Gwfnwtdat(Igrid)%Diag => Diag
      Gwfnwtdat(Igrid)%Icell => Icell      
      Gwfnwtdat(Igrid)%A => A
      Gwfnwtdat(Igrid)%Dc => Dc
      Gwfnwtdat(Igrid)%BB => BB
      Gwfnwtdat(Igrid)%Hchange => Hchange
      Gwfnwtdat(Igrid)%Numnonzero => Numnonzero
      Gwfnwtdat(Igrid)%NJA => NJA
      Gwfnwtdat(Igrid)%Itreal => Itreal
      Gwfnwtdat(Igrid)%Ibt => Ibt
      Gwfnwtdat(Igrid)%II => II
      Gwfnwtdat(Igrid)%IFDPARAM => IFDPARAM   
      Gwfnwtdat(Igrid)%ICNVGFLG => ICNVGFLG
      Gwfnwtdat(Igrid)%Tol => Tol
      Gwfnwtdat(Igrid)%Ftol => Ftol    
      Gwfnwtdat(Igrid)%ITER1 => ITER1
      Gwfnwtdat(Igrid)%Cvm1 => Cvm1
      Gwfnwtdat(Igrid)%Hvm1 => Hvm1
      Gwfnwtdat(Igrid)%Hvp1 => Hvp1
      Gwfnwtdat(Igrid)%Crm1 => Crm1
      Gwfnwtdat(Igrid)%Hrm1 => Hrm1
      Gwfnwtdat(Igrid)%Hrp1 => Hrp1
      Gwfnwtdat(Igrid)%Ccm1 => Ccm1
      Gwfnwtdat(Igrid)%Hcm1 => Hcm1
      Gwfnwtdat(Igrid)%Hcp1 => Hcp1
      Gwfnwtdat(Igrid)%Ccc => Ccc
      Gwfnwtdat(Igrid)%Crr => Crr
      Gwfnwtdat(Igrid)%Cvv => Cvv
      Gwfnwtdat(Igrid)%H => H
      Gwfnwtdat(Igrid)%Hcoff => Hcoff
      Gwfnwtdat(Igrid)%Rhss => Rhss
      Gwfnwtdat(Igrid)%Fhead => Fhead
      Gwfnwtdat(Igrid)%Fheadsave => Fheadsave
      Gwfnwtdat(Igrid)%Fflux => Fflux
      Gwfnwtdat(Igrid)%Hchold => Hchold
      Gwfnwtdat(Igrid)%Numactive => Numactive
      Gwfnwtdat(Igrid)%Numcell => Numcell
      Gwfnwtdat(Igrid)%W => W
      Gwfnwtdat(Igrid)%Hiter => Hiter
      Gwfnwtdat(Igrid)%Wsave => Wsave
      Gwfnwtdat(Igrid)%Theta => Theta
      Gwfnwtdat(Igrid)%Akappa => Akappa
      Gwfnwtdat(Igrid)%Gamma => Gamma
      Gwfnwtdat(Igrid)%Amomentum => Amomentum
      Gwfnwtdat(Igrid)%Btrack => Btrack
      Gwfnwtdat(Igrid)%Btol => Btol
      Gwfnwtdat(Igrid)%RMSAVE => RMSAVE
      Gwfnwtdat(Igrid)%Thickfact => Thickfact
      Gwfnwtdat(Igrid)%Numtrack => Numtrack
      Gwfnwtdat(IGRID)%RMS2   =>RMS2
      Gwfnwtdat(IGRID)%RMS1   =>RMS1
      Gwfnwtdat(IGRID)%Iierr  =>Iierr
      Gwfnwtdat(IGRID)%Nonmeth=>Nonmeth
      Gwfnwtdat(IGRID)%Linmeth=>Linmeth
      Gwfnwtdat(IGRID)%IPRNWT =>IPRNWT
      Gwfnwtdat(IGRID)%IBOTAV =>IBOTAV
      GWFNWTDAT(IGRID)%itertot=> itertot  
!
      END SUBROUTINE SGWF2NWT1PSV