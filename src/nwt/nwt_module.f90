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
  DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS :: GSE_LIM
  DOUBLE PRECISION, SAVE, POINTER :: HED_LIM
  DOUBLE PRECISION, SAVE, POINTER :: Cvm1, Hvm1                     
  DOUBLE PRECISION, SAVE, POINTER :: W, Theta                       
  DOUBLE PRECISION, SAVE, POINTER :: Akappa, Gamma, Amomentum       
  DOUBLE PRECISION, SAVE, POINTER :: Hvp1, Crm1                     
  DOUBLE PRECISION, SAVE, POINTER :: Hrm1, Hrp1, Ccm1               
  DOUBLE PRECISION, SAVE, POINTER :: Hcm1, Hcp1                     
  DOUBLE PRECISION, SAVE, POINTER :: Ccc, Crr, Cvv, H               
  DOUBLE PRECISION, SAVE, POINTER :: Hcoff, Rhss, Fflux, Fhead
  DOUBLE PRECISION, SAVE, POINTER :: Fheadsave
  INTEGER, SAVE, POINTER :: Numnonzero, II, iBtrak, Itreal, Ibt, NJA
  INTEGER, SAVE, POINTER :: IFDPARAM, ICNVGFLG
  INTEGER, SAVE, POINTER :: Btrack, Iierr
  DOUBLE PRECISION, SAVE, POINTER :: Tol, Ftol,  RMS2, RMS1, ADAMP, Breduc_Reset
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
  DOUBLE PRECISION, DIMENSION(:,:,:),POINTER,CONTIGUOUS::Hiter
  DOUBLE PRECISION, DIMENSION(:), POINTER,CONTIGUOUS:: BB, Hchange
  DOUBLE PRECISION, DIMENSION(:), POINTER,CONTIGUOUS:: Hchold, Wsave
  DOUBLE PRECISION, DIMENSION(:,:), POINTER,CONTIGUOUS :: GSE_LIM
  DOUBLE PRECISION, POINTER :: HED_LIM
  DOUBLE PRECISION, POINTER :: Cvm1, Hvm1                     
  DOUBLE PRECISION, POINTER :: W, Theta                       
  DOUBLE PRECISION, POINTER :: Akappa, Gamma, Amomentum       
  DOUBLE PRECISION, POINTER :: Hvp1, Crm1                     
  DOUBLE PRECISION, POINTER :: Hrm1, Hrp1, Ccm1               
  DOUBLE PRECISION, POINTER :: Hcm1, Hcp1                     
  DOUBLE PRECISION, POINTER :: Ccc, Crr, Cvv, H               
  DOUBLE PRECISION, POINTER :: Hcoff, Rhss, Fflux, Fhead
  DOUBLE PRECISION, POINTER :: Fheadsave
  INTEGER, POINTER :: Numnonzero, II, iBtrak, Itreal, Ibt, NJA
  INTEGER, POINTER :: IFDPARAM, ICNVGFLG
  INTEGER, POINTER :: Btrack, Iierr
  DOUBLE PRECISION, POINTER :: Tol, Ftol,  RMS2, RMS1, ADAMP, Breduc_Reset
  DOUBLE PRECISION, POINTER :: Thickfact, Breduc, Btol, RMSAVE
  INTEGER, POINTER :: Numactive, Numcell   
  INTEGER, POINTER :: Nonmeth
  INTEGER, POINTER :: Linmeth
  INTEGER, POINTER :: IPRNWT
  INTEGER, POINTER :: IBOTAV
  INTEGER, POINTER :: ITER1,Numtrack 
  INTEGER, DIMENSION(:), POINTER,CONTIGUOUS:: IA, JA
  INTEGER, DIMENSION(:, :), POINTER,CONTIGUOUS:: Diag
  INTEGER, DIMENSION(:, :, :), POINTER,CONTIGUOUS:: Icell
  INTEGER, POINTER:: itertot
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
      DEALLOCATE (GWFNWTDAT(IGRID)%A         )
      DEALLOCATE (GWFNWTDAT(IGRID)%Dc        )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hiter     )
      DEALLOCATE (GWFNWTDAT(IGRID)%BB        )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hchange   )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hchold    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Wsave     )
      DEALLOCATE (GWFNWTDAT(IGRID)%GSE_LIM   )
      DEALLOCATE (GWFNWTDAT(IGRID)%HED_LIM   )
      DEALLOCATE (GWFNWTDAT(IGRID)%Cvm1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hvm1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%W         )
      DEALLOCATE (GWFNWTDAT(IGRID)%Theta     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Akappa    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Gamma     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Amomentum )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hvp1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Crm1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hrm1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hrp1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Ccm1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hcm1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hcp1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Ccc       )
      DEALLOCATE (GWFNWTDAT(IGRID)%Crr       )
      DEALLOCATE (GWFNWTDAT(IGRID)%Cvv       )
      DEALLOCATE (GWFNWTDAT(IGRID)%H         )
      DEALLOCATE (GWFNWTDAT(IGRID)%Hcoff     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Rhss      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Fflux     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Fhead     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Fheadsave )
      DEALLOCATE (GWFNWTDAT(IGRID)%Numnonzero)
      DEALLOCATE (GWFNWTDAT(IGRID)%II        )
      DEALLOCATE (GWFNWTDAT(IGRID)%iBtrak    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Itreal    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Ibt       )
      DEALLOCATE (GWFNWTDAT(IGRID)%NJA       )
      DEALLOCATE (GWFNWTDAT(IGRID)%IFDPARAM  )
      DEALLOCATE (GWFNWTDAT(IGRID)%ICNVGFLG  )
      DEALLOCATE (GWFNWTDAT(IGRID)%Btrack    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Iierr     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Tol       )
      DEALLOCATE (GWFNWTDAT(IGRID)%Ftol      )
      DEALLOCATE (GWFNWTDAT(IGRID)%RMS2      )
      DEALLOCATE (GWFNWTDAT(IGRID)%RMS1      )
      DEALLOCATE (GWFNWTDAT(IGRID)%ADAMP     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Breduc_Reset)
      DEALLOCATE (GWFNWTDAT(IGRID)%Thickfact )
      DEALLOCATE (GWFNWTDAT(IGRID)%Breduc    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Btol      )
      DEALLOCATE (GWFNWTDAT(IGRID)%RMSAVE    )
      DEALLOCATE (GWFNWTDAT(IGRID)%Numactive )
      DEALLOCATE (GWFNWTDAT(IGRID)%Numcell   )
      DEALLOCATE (GWFNWTDAT(IGRID)%Nonmeth   )
      DEALLOCATE (GWFNWTDAT(IGRID)%Linmeth   )
      DEALLOCATE (GWFNWTDAT(IGRID)%IPRNWT    )
      DEALLOCATE (GWFNWTDAT(IGRID)%IBOTAV    )
      DEALLOCATE (GWFNWTDAT(IGRID)%ITER1     )
      DEALLOCATE (GWFNWTDAT(IGRID)%Numtrack  )
      DEALLOCATE (GWFNWTDAT(IGRID)%IA        )
      DEALLOCATE (GWFNWTDAT(IGRID)%JA        )
      DEALLOCATE (GWFNWTDAT(IGRID)%Diag      )
      DEALLOCATE (GWFNWTDAT(IGRID)%Icell     )
      DEALLOCATE (GWFNWTDAT(IGRID)%itertot   )
      
      
      IF(IGRID.EQ.1)THEN
         A          =>NULL()
         Dc         =>NULL()
         Hiter      =>NULL()
         BB         =>NULL()
         Hchange    =>NULL()
         Hchold     =>NULL()
         Wsave      =>NULL()
         GSE_LIM    =>NULL()
         HED_LIM    =>NULL()
         Cvm1       =>NULL()
         Hvm1       =>NULL()
         W          =>NULL()
         Theta      =>NULL()
         Akappa     =>NULL()
         Gamma      =>NULL()
         Amomentum  =>NULL()
         Hvp1       =>NULL()
         Crm1       =>NULL()
         Hrm1       =>NULL()
         Hrp1       =>NULL()
         Ccm1       =>NULL()
         Hcm1       =>NULL()
         Hcp1       =>NULL()
         Ccc        =>NULL()
         Crr        =>NULL()
         Cvv        =>NULL()
         H          =>NULL()
         Hcoff      =>NULL()
         Rhss       =>NULL()
         Fflux      =>NULL()
         Fhead      =>NULL()
         Fheadsave  =>NULL()
         Numnonzero =>NULL()
         II         =>NULL()
         iBtrak     =>NULL()
         Itreal     =>NULL()
         Ibt        =>NULL()
         NJA        =>NULL()
         IFDPARAM   =>NULL()
         ICNVGFLG   =>NULL()
         Btrack     =>NULL()
         Iierr      =>NULL()
         Tol        =>NULL()
         Ftol       =>NULL()
         RMS2       =>NULL()
         RMS1       =>NULL()
         ADAMP      =>NULL()
         Breduc_Reset=>NULL()
         Thickfact  =>NULL()
         Breduc     =>NULL()
         Btol       =>NULL()
         RMSAVE     =>NULL()
         Numactive  =>NULL()
         Numcell    =>NULL()
         Nonmeth    =>NULL()
         Linmeth    =>NULL()
         IPRNWT     =>NULL()
         IBOTAV     =>NULL()
         ITER1      =>NULL()
         Numtrack   =>NULL()
         IA         =>NULL()
         JA         =>NULL()
         Diag       =>NULL()
         Icell      =>NULL()
         itertot    =>NULL()
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
      A          => GWFNWTDAT(IGRID)%A
      Dc         => GWFNWTDAT(IGRID)%Dc
      Hiter      => GWFNWTDAT(IGRID)%Hiter
      BB         => GWFNWTDAT(IGRID)%BB
      Hchange    => GWFNWTDAT(IGRID)%Hchange
      Hchold     => GWFNWTDAT(IGRID)%Hchold
      Wsave      => GWFNWTDAT(IGRID)%Wsave
      GSE_LIM    => GWFNWTDAT(IGRID)%GSE_LIM
      HED_LIM    => GWFNWTDAT(IGRID)%HED_LIM
      Cvm1       => GWFNWTDAT(IGRID)%Cvm1
      Hvm1       => GWFNWTDAT(IGRID)%Hvm1
      W          => GWFNWTDAT(IGRID)%W
      Theta      => GWFNWTDAT(IGRID)%Theta
      Akappa     => GWFNWTDAT(IGRID)%Akappa
      Gamma      => GWFNWTDAT(IGRID)%Gamma
      Amomentum  => GWFNWTDAT(IGRID)%Amomentum
      Hvp1       => GWFNWTDAT(IGRID)%Hvp1
      Crm1       => GWFNWTDAT(IGRID)%Crm1
      Hrm1       => GWFNWTDAT(IGRID)%Hrm1
      Hrp1       => GWFNWTDAT(IGRID)%Hrp1
      Ccm1       => GWFNWTDAT(IGRID)%Ccm1
      Hcm1       => GWFNWTDAT(IGRID)%Hcm1
      Hcp1       => GWFNWTDAT(IGRID)%Hcp1
      Ccc        => GWFNWTDAT(IGRID)%Ccc
      Crr        => GWFNWTDAT(IGRID)%Crr
      Cvv        => GWFNWTDAT(IGRID)%Cvv
      H          => GWFNWTDAT(IGRID)%H
      Hcoff      => GWFNWTDAT(IGRID)%Hcoff
      Rhss       => GWFNWTDAT(IGRID)%Rhss
      Fflux      => GWFNWTDAT(IGRID)%Fflux
      Fhead      => GWFNWTDAT(IGRID)%Fhead
      Fheadsave  => GWFNWTDAT(IGRID)%Fheadsave
      Numnonzero => GWFNWTDAT(IGRID)%Numnonzero
      II         => GWFNWTDAT(IGRID)%II
      iBtrak     => GWFNWTDAT(IGRID)%iBtrak
      Itreal     => GWFNWTDAT(IGRID)%Itreal
      Ibt        => GWFNWTDAT(IGRID)%Ibt
      NJA        => GWFNWTDAT(IGRID)%NJA
      IFDPARAM   => GWFNWTDAT(IGRID)%IFDPARAM
      ICNVGFLG   => GWFNWTDAT(IGRID)%ICNVGFLG
      Btrack     => GWFNWTDAT(IGRID)%Btrack
      Iierr      => GWFNWTDAT(IGRID)%Iierr
      Tol        => GWFNWTDAT(IGRID)%Tol
      Ftol       => GWFNWTDAT(IGRID)%Ftol
      RMS2       => GWFNWTDAT(IGRID)%RMS2
      RMS1       => GWFNWTDAT(IGRID)%RMS1
      ADAMP      => GWFNWTDAT(IGRID)%ADAMP
      Breduc_Reset=> GWFNWTDAT(IGRID)%Breduc_Reset
      Thickfact  => GWFNWTDAT(IGRID)%Thickfact
      Breduc     => GWFNWTDAT(IGRID)%Breduc
      Btol       => GWFNWTDAT(IGRID)%Btol
      RMSAVE     => GWFNWTDAT(IGRID)%RMSAVE
      Numactive  => GWFNWTDAT(IGRID)%Numactive
      Numcell    => GWFNWTDAT(IGRID)%Numcell
      Nonmeth    => GWFNWTDAT(IGRID)%Nonmeth
      Linmeth    => GWFNWTDAT(IGRID)%Linmeth
      IPRNWT     => GWFNWTDAT(IGRID)%IPRNWT
      IBOTAV     => GWFNWTDAT(IGRID)%IBOTAV
      ITER1      => GWFNWTDAT(IGRID)%ITER1
      Numtrack   => GWFNWTDAT(IGRID)%Numtrack
      IA         => GWFNWTDAT(IGRID)%IA
      JA         => GWFNWTDAT(IGRID)%JA
      Diag       => GWFNWTDAT(IGRID)%Diag
      Icell      => GWFNWTDAT(IGRID)%Icell
      itertot    => GWFNWTDAT(IGRID)%itertot
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
      GWFNWTDAT(IGRID)%A          => A
      GWFNWTDAT(IGRID)%Dc         => Dc
      GWFNWTDAT(IGRID)%Hiter      => Hiter
      GWFNWTDAT(IGRID)%BB         => BB
      GWFNWTDAT(IGRID)%Hchange    => Hchange
      GWFNWTDAT(IGRID)%Hchold     => Hchold
      GWFNWTDAT(IGRID)%Wsave      => Wsave
      GWFNWTDAT(IGRID)%GSE_LIM    => GSE_LIM
      GWFNWTDAT(IGRID)%HED_LIM    => HED_LIM
      GWFNWTDAT(IGRID)%Cvm1       => Cvm1
      GWFNWTDAT(IGRID)%Hvm1       => Hvm1
      GWFNWTDAT(IGRID)%W          => W
      GWFNWTDAT(IGRID)%Theta      => Theta
      GWFNWTDAT(IGRID)%Akappa     => Akappa
      GWFNWTDAT(IGRID)%Gamma      => Gamma
      GWFNWTDAT(IGRID)%Amomentum  => Amomentum
      GWFNWTDAT(IGRID)%Hvp1       => Hvp1
      GWFNWTDAT(IGRID)%Crm1       => Crm1
      GWFNWTDAT(IGRID)%Hrm1       => Hrm1
      GWFNWTDAT(IGRID)%Hrp1       => Hrp1
      GWFNWTDAT(IGRID)%Ccm1       => Ccm1
      GWFNWTDAT(IGRID)%Hcm1       => Hcm1
      GWFNWTDAT(IGRID)%Hcp1       => Hcp1
      GWFNWTDAT(IGRID)%Ccc        => Ccc
      GWFNWTDAT(IGRID)%Crr        => Crr
      GWFNWTDAT(IGRID)%Cvv        => Cvv
      GWFNWTDAT(IGRID)%H          => H
      GWFNWTDAT(IGRID)%Hcoff      => Hcoff
      GWFNWTDAT(IGRID)%Rhss       => Rhss
      GWFNWTDAT(IGRID)%Fflux      => Fflux
      GWFNWTDAT(IGRID)%Fhead      => Fhead
      GWFNWTDAT(IGRID)%Fheadsave  => Fheadsave
      GWFNWTDAT(IGRID)%Numnonzero => Numnonzero
      GWFNWTDAT(IGRID)%II         => II
      GWFNWTDAT(IGRID)%iBtrak     => iBtrak
      GWFNWTDAT(IGRID)%Itreal     => Itreal
      GWFNWTDAT(IGRID)%Ibt        => Ibt
      GWFNWTDAT(IGRID)%NJA        => NJA
      GWFNWTDAT(IGRID)%IFDPARAM   => IFDPARAM
      GWFNWTDAT(IGRID)%ICNVGFLG   => ICNVGFLG
      GWFNWTDAT(IGRID)%Btrack     => Btrack
      GWFNWTDAT(IGRID)%Iierr      => Iierr
      GWFNWTDAT(IGRID)%Tol        => Tol
      GWFNWTDAT(IGRID)%Ftol       => Ftol
      GWFNWTDAT(IGRID)%RMS2       => RMS2
      GWFNWTDAT(IGRID)%RMS1       => RMS1
      GWFNWTDAT(IGRID)%ADAMP      => ADAMP
      GWFNWTDAT(IGRID)%Breduc_Reset=> Breduc_Reset
      GWFNWTDAT(IGRID)%Thickfact  => Thickfact
      GWFNWTDAT(IGRID)%Breduc     => Breduc
      GWFNWTDAT(IGRID)%Btol       => Btol
      GWFNWTDAT(IGRID)%RMSAVE     => RMSAVE
      GWFNWTDAT(IGRID)%Numactive  => Numactive
      GWFNWTDAT(IGRID)%Numcell    => Numcell
      GWFNWTDAT(IGRID)%Nonmeth    => Nonmeth
      GWFNWTDAT(IGRID)%Linmeth    => Linmeth
      GWFNWTDAT(IGRID)%IPRNWT     => IPRNWT
      GWFNWTDAT(IGRID)%IBOTAV     => IBOTAV
      GWFNWTDAT(IGRID)%ITER1      => ITER1
      GWFNWTDAT(IGRID)%Numtrack   => Numtrack
      GWFNWTDAT(IGRID)%IA         => IA
      GWFNWTDAT(IGRID)%JA         => JA
      GWFNWTDAT(IGRID)%Diag       => Diag
      GWFNWTDAT(IGRID)%Icell      => Icell
      GWFNWTDAT(IGRID)%itertot    => itertot
!
      END SUBROUTINE SGWF2NWT1PSV