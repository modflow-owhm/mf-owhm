! GLOBAL MODULE THAT CONTAINS PROPERTIES UNIQUE TO ALL MODELS
! ADDITIONAL GLOBAL SUBROUTINES EXPECT THAT LGR POITNERS ARE CORRECT
MODULE GLOBAL
  USE XY_GRID_COORDINATE_INTERFACE,    ONLY: XY_GRID_COODINATES   !NOTE THIS IS LEFT WITHOUT PRIVATE FOR OTHER ROUTINES TO USE
  USE ARRAY_DATA_TYPES,                ONLY: INTEGER_VECTOR, INTEGER_MATRIX
  USE NAME_ID_INTERFACE,               ONLY: NAME_ID
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64
  !
  IMPLICIT NONE
  !
  SAVE
  PRIVATE:: INTEGER_MATRIX, INTEGER_VECTOR, NAME_ID, INT64, REAL32, REAL64
  !
  !Set up package names ----------------------------------------------------------------------------
  INTEGER, PARAMETER:: NIUNIT=70
  CHARACTER(5), DIMENSION(NIUNIT), PARAMETER:: CUNIT =                 &
     [                                                                 &
       'BCF  ', 'WEL  ', 'DRN  ', 'RIV  ', 'EVT  ', 'RIP  ', 'GHB  ',  & !  7 
       'RCH  ', 'SIP  ', 'DE4  ', '     ', 'OC   ', 'PCG  ', 'lmg  ',  & ! 14
       'gwt  ', 'FHB  ', 'RES  ', 'STR  ', 'IBS  ', 'CHD  ', 'HFB  ',  & ! 21 
       'LAK  ', 'LPF  ', 'DIS  ', '     ', 'PVAL ', 'ssub ', 'HOB  ',  & ! 28
       '     ', '     ', 'ZONE ', 'MULT ', 'DROB ', 'RVOB ', 'GBOB ',  & ! 35
       '     ', 'HUF  ', 'CHOB ', 'ETS  ', 'DRT  ', 'DRTOB', 'GMG  ',  & ! 42
       'HYD  ', 'SFR  ', 'swo  ', 'GAGE ', 'LVDA ', 'BFH  ', 'LMT  ',  & ! 49   IUNIT(45) = SWO
       'MNW2 ', 'MNWI ', 'MNW1 ', 'KDEP ', 'SUB  ', 'UZF  ', 'GWM  ',  & ! 56
       'SWT  ', 'CFP  ', 'COC  ', 'CRCH ', 'FMP  ', 'UPW  ', 'NWT  ',  & ! 63
       'SWR  ', 'SWI  ', 'AG   ', 'WEL1 ', '     ', 'slang', 'PCGN '   & ! 70   IUNIT(69) = Slang
     ]
  !
  INTEGER, PARAMETER:: KND = REAL64    ! NOTE GMG DOES NOT WORK WITH KND BEING SINGLE PRECISION
  !
  ! NON-IGRID BASED GLOBAL VARIABLES --That is only allowed on parent--------------------------------
  !
  CHARACTER(4):: GW_SOLVER
  CHARACTER(3):: GW_FLOW_PACK
  !
  TYPE(NAME_ID), DIMENSION(3):: SUPER_NAMES
  !  SUPER_NAMES(1) => SFR
  !  SUPER_NAMES(2) => FMP
  !  SUPER_NAMES(3) => SWO
  !
  INTEGER::   NGRIDS = 1
  INTEGER::   CMD_ITER_INFO = 0 !-4590077  !Print interval for command prompt
  REAL(KND):: HCloseBAS   = 0D0  
  REAL(KND):: RCloseL2BAS = 0D0
  REAL(KND):: NaN      = TRANSFER(-2251799813685248_INT64, 1._REAL64)   !Assumes IEEE x86_64 processor for TRANSFER
  !
  !GLOBAL VARIABLES ------------------------------------------------------------------------------------------------------
  !
  TYPE TIMESTEP_DELT
         LOGICAL:: SPECIFY_DELT = .FALSE.
         DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE:: DT        !seb holes each time steps length
  END TYPE
  !
  INTEGER,                                   POINTER:: NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
  INTEGER,                                   POINTER:: ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
  INTEGER,                                   POINTER:: IFREFM,NODES,IOUT,MXITER,IRESTART
  INTEGER,                                   POINTER:: KPERSTART,KSTPSTART,IUNITSTART
  LOGICAL,                                   POINTER:: NO_CONST_HEAD                       ! Set to false if there is CHD pack or IBOUND<0
  LOGICAL,                                   POINTER:: SUBLNK                              ! VARIABLE THAT INDICATES THAT SUBLINK IS ACTIVE. IT IS SET TO FALSE BY DEFAULT AND ALTERED BY SUBPACKAGE
  LOGICAL,                                   POINTER:: INPUT_CHECK
  LOGICAL,                                   POINTER:: BACKTRACKING
  INTEGER,                                   POINTER:: BIN_REAL_KIND                      !KIND VALUE TO USE FOR ULT BINARY WRITERS --DEFAULT IS SNGL/32bit/4byte
  INTEGER,   DIMENSION(:),       CONTIGUOUS, POINTER:: IUNIT
  REAL(KND), DIMENSION(:,:,:),   CONTIGUOUS, POINTER:: HNEW
  REAL(KND), DIMENSION(:,:,:),   CONTIGUOUS, POINTER:: HNEW_OLD     !PREVIOUS SOLVER/FM INTERATION VALUE
  REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: WTABLE       !HOLDS WATER TABLE ELEVATION FROM HNEW
  REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: WTABLE_OLD   !HOLDS WATER TABLE ELEVATION FROM HOLD (previous time step)
  INTEGER,     DIMENSION(:,:),   CONTIGUOUS, POINTER:: UPLAY
  INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LBOTM
  INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LAYCBD
  INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LAYHDT
  INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LAYHDS
  REAL(KND),   DIMENSION(:),     CONTIGUOUS, POINTER:: PERLEN
  INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: NSTP
  REAL,        DIMENSION(:),     CONTIGUOUS, POINTER:: TSMULT
  INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: ISSFLG
  REAL,        DIMENSION(:),     CONTIGUOUS, POINTER:: DELR
  REAL,        DIMENSION(:),     CONTIGUOUS, POINTER:: DELC
  REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: AREA
  REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: GSE 
  REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: BOTM
  REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: HOLD
  INTEGER,     DIMENSION(:,:,:), CONTIGUOUS, POINTER:: IBOUND
  REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CR
  REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CC
  REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CV
  REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: HCOF
  REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: RHS
  REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: BUFF
  REAL(REAL32),DIMENSION(:,:,:), CONTIGUOUS, POINTER:: RBUF
  REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: STRT
  REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: DDREF
  LOGICAL,                                   POINTER:: ALLOC_DDREF
  !
  TYPE(INTEGER_MATRIX),                         POINTER:: UPLAY_IDX
  TYPE(XY_GRID_COODINATES),                     POINTER:: XYGRID
  TYPE(TIMESTEP_DELT),DIMENSION(:), CONTIGUOUS, POINTER:: SPTIM
  !
  INTEGER,  POINTER:: SPSTART,SPEND
  INTEGER,  POINTER:: NOCBC
  INTEGER,  POINTER:: CBC_GLOBAL_UNIT
  !INTEGER,     DIMENSION(:,:,:), CONTIGUOUS,POINTER ::WETCEL
  !
  TYPE GLOBALTYPE
     INTEGER,                                   POINTER:: NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
     INTEGER,                                   POINTER:: ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
     INTEGER,                                   POINTER:: IFREFM,NODES,IOUT,MXITER,IRESTART
     INTEGER,                                   POINTER:: KPERSTART,KSTPSTART,IUNITSTART
     LOGICAL,                                   POINTER:: SUBLNK                              !seb VARIABLE THAT INDICATES THAT SUBLINK IS ACTIVE. IT IS SET TO FALSE BY DEFAULT AND ALTERED BY SUBPACKAGE
     LOGICAL,                                   POINTER:: INPUT_CHECK
     INTEGER,                                   POINTER:: BIN_REAL_KIND                      !KIND VALUE TO USE FOR ULT BINARY WRITERS --DEFAULT IS SNGL/32bit/4byte
     LOGICAL,                                   POINTER:: BACKTRACKING
     INTEGER,   DIMENSION(:),       CONTIGUOUS, POINTER:: IUNIT
     REAL(KND), DIMENSION(:,:,:),   CONTIGUOUS, POINTER:: HNEW
     REAL(KND), DIMENSION(:,:,:),   CONTIGUOUS, POINTER:: HNEW_OLD !PREVIOUS SOLVER/FM INTERATION VALUE
     REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: WTABLE   !HOLDS WATER TABLE ELEVATION FROM HNEW
     REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: WTABLE_OLD
     INTEGER,     DIMENSION(:,:),   CONTIGUOUS, POINTER:: UPLAY
     INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LBOTM
     INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LAYCBD
     INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LAYHDT
     INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: LAYHDS
     REAL(KND),   DIMENSION(:),     CONTIGUOUS, POINTER:: PERLEN
     INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: NSTP
     REAL,        DIMENSION(:),     CONTIGUOUS, POINTER:: TSMULT
     INTEGER,     DIMENSION(:),     CONTIGUOUS, POINTER:: ISSFLG
     REAL,        DIMENSION(:),     CONTIGUOUS, POINTER:: DELR
     REAL,        DIMENSION(:),     CONTIGUOUS, POINTER:: DELC
     REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: AREA 
     REAL(KND),   DIMENSION(:,:),   CONTIGUOUS, POINTER:: GSE
     REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: BOTM
     REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: HOLD
     INTEGER,     DIMENSION(:,:,:), CONTIGUOUS, POINTER:: IBOUND
     REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CR
     REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CC
     REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CV
     REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: HCOF
     REAL(KND),   DIMENSION(:,:,:), CONTIGUOUS, POINTER:: RHS
     REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: BUFF
     REAL(REAL32),DIMENSION(:,:,:), CONTIGUOUS, POINTER:: RBUF
     REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: STRT
     REAL,        DIMENSION(:,:,:), CONTIGUOUS, POINTER:: DDREF
     LOGICAL,                                   POINTER:: ALLOC_DDREF
     !
     TYPE(INTEGER_MATRIX),                         POINTER:: UPLAY_IDX
     TYPE(XY_GRID_COODINATES),                     POINTER:: XYGRID
     TYPE(TIMESTEP_DELT),DIMENSION(:), CONTIGUOUS, POINTER:: SPTIM
     !
     INTEGER,  POINTER:: SPSTART,SPEND
     INTEGER,  POINTER:: NOCBC
     INTEGER,  POINTER:: CBC_GLOBAL_UNIT
  END TYPE GLOBALTYPE
  TYPE(GLOBALTYPE) ::GLOBALDAT(10)
  !      
  CONTAINS
  !
  !------------------------------------------------------------------
  !
  PURE FUNCTION SAT_FRAC(IL,IR,IC, H) RESULT(FRAC)  
    !USE GLOBAL, ONLY: HNEW,BOTM,LBOTM
    INTEGER, INTENT(IN):: IL,IR,IC
    DOUBLE PRECISION, OPTIONAL, INTENT(IN):: H
    DOUBLE PRECISION:: FRAC
    !
    IF(PRESENT(H)) THEN
      IF( H < BOTM(IC,IR,LBOTM(IL)) ) THEN
          FRAC = 0D0
      ELSEIF( H < BOTM(IC,IR,LBOTM(IL)-1) ) THEN
          FRAC = ( H                       - BOTM(IC,IR,LBOTM(IL)) ) /      &
                 ( BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL)) )
      ELSE
          FRAC = 1D0
      END IF
    ELSE
      IF( HNEW(IC,IR,IL) < BOTM(IC,IR,LBOTM(IL)) ) THEN
          FRAC = 0D0
      ELSEIF( HNEW(IC,IR,IL) < BOTM(IC,IR,LBOTM(IL)-1) ) THEN
          FRAC = ( HNEW(IC,IR,IL)          - BOTM(IC,IR,LBOTM(IL)) ) /      &
                 ( BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL)) )
      ELSE
          FRAC = 1D0
      END IF
    END IF
    !
  END FUNCTION
  !
  !------------------------------------------------------------------
  !
  PURE FUNCTION SAT_THICK(IL,IR,IC, H) RESULT(THICK)  
    !USE GLOBAL, ONLY: HNEW,BOTM,LBOTM
    INTEGER, INTENT(IN):: IL,IR,IC
    DOUBLE PRECISION, OPTIONAL,INTENT(IN):: H
    DOUBLE PRECISION:: THICK
    !
    THICK = ( BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL)) ) * SAT_FRAC(IL,IR,IC,H)
    !
  END FUNCTION
  !
  !------------------------------------------------------------------
  !                                        !R,C,L
  PURE FUNCTION CELL_MASS_BALANCE(I,J,K) RESULT(DIF)
    !ASSUMES LGR POINTERS ARE CORRECT
    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
    INTEGER, INTENT(IN):: I,J,K
    DOUBLE PRECISION:: DIF
    DOUBLE PRECISION:: DTMP
    !
    IF(IBOUND(J,I,K)>0) THEN
        !
        !=================================================
        DTMP = 0D0          ! HOLDS CELL CENTER CALC
        DIF  = -RHS(J,I,K)  ! HOLDS RESIDUAL  
        !
        ! ... GET CONDUCTANCES TO NEIGHBORING CELLS
        ! ... NEIGHBOR IS 1 ROW BACK
        IF (I > 1) THEN
           IF (IBOUND(J,I-1,K).NE.0) THEN
              !
              DTMP = DTMP + CC(J,I-1,K)
              DIF  = DIF  + CC(J,I-1,K)*HNEW(J,I-1,K)
              !
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 ROW AHEAD
        IF (I < NROW) THEN
           IF (IBOUND(J,I+1,K).NE.0) THEN
              DTMP = DTMP + CC(J,I,K)
              DIF  = DIF  + CC(J,I,K)*HNEW(J,I+1,K)
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 COLUMN BACK
        IF (J > 1) THEN
           IF (IBOUND(J-1,I,K).NE.0) THEN
              DTMP = DTMP + CR(J-1,I,K)
              DIF  = DIF  + CR(J-1,I,K)*HNEW(J-1,I,K)
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 COLUMN AHEAD
        IF (J < NCOL) THEN
           IF (IBOUND(J+1,I,K).NE.0) THEN
              DTMP = DTMP+ CR(J,I,K)
              DIF  = DIF + CR(J,I,K)*HNEW(J+1,I,K)
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 LAYER BEHIND
        IF (K > 1) THEN
           IF (IBOUND(J,I,K-1).NE.0) THEN
              DTMP = DTMP+ CV(J,I,K-1)
              DIF  = DIF + CV(J,I,K-1)*HNEW(J,I,K-1)
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 LAYER AHEAD
        IF (K < NLAY) THEN
           IF (IBOUND(J,I,K+1).NE.0) THEN
              DTMP = DTMP+ CV(J,I,K)
              DIF  = DIF + CV(J,I,K)*HNEW(J,I,K+1)
           END IF
        END IF
        !
        DTMP = DTMP - HCOF(J,I,K) 
        !
        DIF = DIF - DTMP*HNEW(J,I,K)
    ELSE
        DIF = 0D0
    END IF
  END FUNCTION
  !
  !------------------------------------------------------------------
  !
  PURE SUBROUTINE WORST_CELL_MASS_BALANCE(R,C,L,ERR)
    !ASSUMES LGR POINTERS ARE CORRECT
    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
    INTEGER,          INTENT(OUT):: R,C,L
    DOUBLE PRECISION, INTENT(OUT):: ERR
    DOUBLE PRECISION:: DTMP
    INTEGER:: I,J,K
    !
    ERR = 0D0
    R   = 0
    C   = 0
    L   = 0
    DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IBOUND(J,I,K)>0)
          !
          DTMP = CELL_MASS_BALANCE(I,J,K)
          IF(ABS(DTMP) > ABS(ERR)) THEN
               ERR = DTMP
               R = I
               C = J
               L = K
          END IF
    END DO
    !
  END SUBROUTINE
  !
  !------------------------------------------------------------------
  !
  PURE SUBROUTINE MASS_BALANCE_INOUT(RATIN, RATOUT, RATERR)
    !ASSUMES LGR POINTERS ARE CORRECT
    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
    DOUBLE PRECISION, INTENT(OUT):: RATIN, RATOUT, RATERR
    DOUBLE PRECISION:: Q, HD, DZ, ERR
    INTEGER:: I,J,K
    !
    DZ = 0D0
    !    
    RATIN = DZ
    RATOUT= DZ
    RATERR= DZ
    DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IBOUND(J,I,K)>0)
        !
        HD = HNEW(J,I,K)
        ERR= DZ
        !
        ! ... GET CONDUCTANCES TO NEIGHBORING CELLS
        ! ... NEIGHBOR IS 1 ROW BACK
        IF (I > 1) THEN
           IF (IBOUND(J,I-1,K).NE.0) THEN
              !
              Q = CC(J,I-1,K) * (HNEW(J,I-1,K) - HD)
              !
              IF(Q < DZ) THEN
                  RATOUT = RATOUT - Q
              ELSE
                  RATIN  = RATIN  + Q
              END IF
              ERR = ERR + Q
              !
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 ROW AHEAD
        IF (I < NROW) THEN
           IF (IBOUND(J,I+1,K).NE.0) THEN
              !
              Q = CC(J,I,K) * (HNEW(J,I+1,K) - HD)
              !
              IF(Q < DZ) THEN
                  RATOUT = RATOUT - Q
              ELSE
                  RATIN  = RATIN  + Q
              END IF
              ERR = ERR + Q
              !
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 COLUMN BACK
        IF (J > 1) THEN
           IF (IBOUND(J-1,I,K).NE.0) THEN
              !
              Q = CR(J-1,I,K) * (HNEW(J-1,I,K) - HD)
              !
              IF(Q < DZ) THEN
                  RATOUT = RATOUT - Q
              ELSE
                  RATIN  = RATIN  + Q
              END IF
              ERR = ERR + Q
              !
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 COLUMN AHEAD
        IF (J < NCOL) THEN
           IF (IBOUND(J+1,I,K).NE.0) THEN
              !
              Q = CR(J,I,K) * (HNEW(J+1,I,K) - HD)
              !
              IF(Q < DZ) THEN
                  RATOUT = RATOUT - Q
              ELSE
                  RATIN  = RATIN  + Q
              END IF
              ERR = ERR + Q
              !
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 LAYER BEHIND
        IF (K > 1) THEN
           IF (IBOUND(J,I,K-1).NE.0) THEN
              !
              Q = CV(J,I,K-1) * (HNEW(J,I,K-1) - HD)
              !
              IF(Q < DZ) THEN
                  RATOUT = RATOUT - Q
              ELSE
                  RATIN  = RATIN  + Q
              END IF
              ERR = ERR + Q
              !
           END IF
        END IF
        ! ...
        ! ... NEIGHBOR IS 1 LAYER AHEAD
        IF (K < NLAY) THEN
           IF (IBOUND(J,I,K+1).NE.0) THEN
              !
              Q = CV(J,I,K) * (HNEW(J,I,K+1) - HD)
              !
              IF(Q < DZ) THEN
                  RATOUT = RATOUT - Q
              ELSE
                  RATIN  = RATIN  + Q
              END IF
              ERR = ERR + Q
              !
           END IF
        END IF
        !
        Q = HCOF(J,I,K) * HD - RHS(J,I,K)
        !
        IF(Q < DZ) THEN
            RATOUT = RATOUT - Q
        ELSE
            RATIN  = RATIN  + Q
        END IF
        ERR = ERR + Q
        !
        IF(ERR < DZ) THEN
            RATERR = RATERR - ERR
        ELSE
            RATERR = RATERR + ERR
        END IF
    END DO
    !
  END SUBROUTINE
  !
  !------------------------------------------------------------------
  !                                     !R,C,L
  PURE FUNCTION CELL_VOL_ERROR(I,J,K) RESULT(ERR) ! IS MASS BALANCE NORMALIZED BY CELL VOLUME
    INTEGER,          INTENT(IN):: I,J,K
    DOUBLE PRECISION:: ERR
    DOUBLE PRECISION:: VOL
    !
    VOL = AREA(J,I)*(BOTM(J,I,LBOTM(K)-1) - BOTM(J,I,LBOTM(K)))    !CELL VOLUME
    !
    ERR = CELL_MASS_BALANCE(I,J,K) !VOLUME/TIME ERROR
    !
    IF(ERR < 0D0) ERR = -1D0 * ERR
    !
    IF( ERR  > 0.01D0 .AND. VOL > 0.01D0) THEN
        ERR = ERR/VOL !NORMALIZE BY CELL VOLUME
    ELSE
        ERR = 0D0
    END IF
    !
  END FUNCTION
  !
  !------------------------------------------------------------------
  !
  PURE SUBROUTINE MAX_RELATIVE_VOL_ERROR(VOLERR,R,C,L)
    !ASSUMES LGR POINTERS ARE CORRECT
    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
    DOUBLE PRECISION, INTENT(OUT):: VOLERR
    INTEGER, OPTIONAL,INTENT(OUT):: R,C,L
    DOUBLE PRECISION:: ERR
    INTEGER:: I,J,K
    LOGICAL:: HAS_RCL
    !
    HAS_RCL= PRESENT(R)
    IF(HAS_RCL) THEN; R = 0; C = 0; L = 0
    END IF
    !
    VOLERR= 0D0
    DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IBOUND(J,I,K)>0)
        !
        ERR = CELL_VOL_ERROR(I,J,K)
        !
        IF(ERR > VOLERR) THEN
            !
            VOLERR = ERR
            !
            IF(HAS_RCL) THEN; R = I; C = J; L = K
            END IF
        END IF
    END DO
    !
  END SUBROUTINE
  !
  !------------------------------------------------------------------
  !
!  PURE SUBROUTINE MAX_RELATIVE_VOL_ERROR(DT,VOLERR,R,C,L) !DT IS A BAD IDEA...OTHERWISE FRACTION BECOMES SMALLER WITH LARGER DT
!    !ASSUMES LGR POINTERS ARE CORRECT
!    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
!    DOUBLE PRECISION, INTENT(IN ):: DT     !TIME STEP LENGTH = DELT
!    DOUBLE PRECISION, INTENT(OUT):: VOLERR
!    INTEGER, OPTIONAL,INTENT(OUT):: R,C,L
!    DOUBLE PRECISION:: ERR, VOL, TOL
!    INTEGER:: I,J,K
!    LOGICAL:: HAS_RCL
!    !
!    TOL    = 0.01D0
!    VOLERR = 0D0
!    !
!    HAS_RCL= PRESENT(R)
!    IF(HAS_RCL) THEN
!          R = 0; C = 0; L = 0
!    END IF
!    !
!    DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IBOUND(J,I,K)>0)
!        !
!        VOL=AREA(J,I)*(BOTM(J,I,LBOTM(K)-1) - BOTM(J,I,LBOTM(K)))    !CELL VOLUME
!        !
!        ERR = CELL_MASS_BALANCE(I,J,K) * DT !VOLUME ERROR
!        !
!        IF(ERR < 0D0) ERR = -1D0 * ERR
!        !
!        IF( ERR  > TOL .AND. VOL > TOL) THEN
!            ERR = ERR/VOL
!            IF(ERR > VOLERR) THEN
!                VOLERR = ERR
!                IF(HAS_RCL) THEN
!                                R = I
!                                C = J
!                                L = K
!                END IF
!            END IF
!        END IF
!    END DO
!    !
!  END SUBROUTINE
  !
  !------------------------------------------------------------------
  !
!  PURE SUBROUTINE MAX_RELATIVE_MASS_ERROR(RATERR)
!    !ASSUMES LGR POINTERS ARE CORRECT
!    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
!    DOUBLE PRECISION, INTENT(OUT):: RATERR
!    DOUBLE PRECISION:: RATIN, RATOUT, Q, HD, DZ, ERR, N
!    INTEGER:: I,J,K
!    !
!    DZ = 0D0
!    !    
!    RATERR= DZ
!    DO CONCURRENT(K=1:NLAY, I=1:NROW, J=1:NCOL, IBOUND(J,I,K)>0)
!        !
!        HD = HNEW(J,I,K)
!        ERR= DZ
!        RATIN = DZ
!        RATOUT= DZ
!        !
!        ! ... GET CONDUCTANCES TO NEIGHBORING CELLS
!        ! ... NEIGHBOR IS 1 ROW BACK
!        IF (I > 1) THEN
!           IF (IBOUND(J,I-1,K).NE.0) THEN
!              !
!              Q = CC(J,I-1,K) * (HNEW(J,I-1,K) - HD)
!              !
!              IF(Q < DZ) THEN
!                  RATOUT = RATOUT - Q
!              ELSE
!                  RATIN  = RATIN  + Q
!              END IF
!              ERR = ERR + Q
!              !
!           END IF
!        END IF
!        ! ...
!        ! ... NEIGHBOR IS 1 ROW AHEAD
!        IF (I < NROW) THEN
!           IF (IBOUND(J,I+1,K).NE.0) THEN
!              !
!              Q = CC(J,I,K) * (HNEW(J,I+1,K) - HD)
!              !
!              IF(Q < DZ) THEN
!                  RATOUT = RATOUT - Q
!              ELSE
!                  RATIN  = RATIN  + Q
!              END IF
!              ERR = ERR + Q
!              !
!           END IF
!        END IF
!        ! ...
!        ! ... NEIGHBOR IS 1 COLUMN BACK
!        IF (J > 1) THEN
!           IF (IBOUND(J-1,I,K).NE.0) THEN
!              !
!              Q = CR(J-1,I,K) * (HNEW(J-1,I,K) - HD)
!              !
!              IF(Q < DZ) THEN
!                  RATOUT = RATOUT - Q
!              ELSE
!                  RATIN  = RATIN  + Q
!              END IF
!              ERR = ERR + Q
!              !
!           END IF
!        END IF
!        ! ...
!        ! ... NEIGHBOR IS 1 COLUMN AHEAD
!        IF (J < NCOL) THEN
!           IF (IBOUND(J+1,I,K).NE.0) THEN
!              !
!              Q = CR(J,I,K) * (HNEW(J+1,I,K) - HD)
!              !
!              IF(Q < DZ) THEN
!                  RATOUT = RATOUT - Q
!              ELSE
!                  RATIN  = RATIN  + Q
!              END IF
!              ERR = ERR + Q
!              !
!           END IF
!        END IF
!        ! ...
!        ! ... NEIGHBOR IS 1 LAYER BEHIND
!        IF (K > 1) THEN
!           IF (IBOUND(J,I,K-1).NE.0) THEN
!              !
!              Q = CV(J,I,K-1) * (HNEW(J,I,K-1) - HD)
!              !
!              IF(Q < DZ) THEN
!                  RATOUT = RATOUT - Q
!              ELSE
!                  RATIN  = RATIN  + Q
!              END IF
!              ERR = ERR + Q
!              !
!           END IF
!        END IF
!        ! ...
!        ! ... NEIGHBOR IS 1 LAYER AHEAD
!        IF (K < NLAY) THEN
!           IF (IBOUND(J,I,K+1).NE.0) THEN
!              !
!              Q = CV(J,I,K) * (HNEW(J,I,K+1) - HD)
!              !
!              IF(Q < DZ) THEN
!                  RATOUT = RATOUT - Q
!              ELSE
!                  RATIN  = RATIN  + Q
!              END IF
!              ERR = ERR + Q
!              !
!           END IF
!        END IF
!        !
!        Q = HCOF(J,I,K) * HD - RHS(J,I,K)
!        !
!        IF(Q < DZ) THEN
!            RATOUT = RATOUT - Q
!        ELSE
!            RATIN  = RATIN  + Q
!        END IF
!        ERR = ERR + Q
!        !
!        ! -----------------------
!        !
!        IF    (RATIN  < RATOUT) THEN
!                                 Q = RATOUT
!        ELSE
!                                 Q = RATIN
!        END IF
!        !
!        IF( Q  > 0.1D0 ) THEN
!            Q = ABS(ERR)/Q
!            IF(Q > RATERR) RATERR = Q
!        END IF
!    END DO
!    !
!  END SUBROUTINE
  !
  !------------------------------------------------------------------
  !
  PURE SUBROUTINE RELATIVE_MASS_ERROR(ERR, MINRAT)  !RELATIVE ERROR IS NOT A PERCENT!!! Always positive too!
    !ASSUMES LGR POINTERS ARE CORRECT
    !USE GLOBAL, ONLY:NCOL,NROW,NLAY,HNEW,IBOUND,CC,CR,CV,HCOF,RHS
    DOUBLE PRECISION, INTENT(OUT):: ERR
    DOUBLE PRECISION, INTENT(IN ):: MINRAT
    DOUBLE PRECISION:: RATIN, RATOUT, RATERR, AVERAT
    !
    CALL MASS_BALANCE_INOUT(RATIN, RATOUT, RATERR)
    !
    AVERAT = (RATIN + RATOUT) / 2D0
    !
    IF(AVERAT > MINRAT) THEN                        
                       ERR = RATERR / AVERAT
    ELSE
                       ERR = 0D0  !IF FLOWS ARE TOO SMALL JUST ASSUME ZERO ERROR OR PERCENT ERROR WILL BE HUGE
    END IF 
    !
  END SUBROUTINE
  !
END MODULE GLOBAL
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! 
MODULE PARAMMODULE
!  Data definitions for Named Parameters
!  Explicitly declare all variables to enable subroutines that include
!  this file to use the IMPLICIT NONE statement.
!        PARAMETER (MXPAR=2000,MXCLST=2000000,MXINST=50000)             !wschmid: changed to 2000000 consistent with FMP2.1 release
  IMPLICIT NONE
  INTEGER,  PARAMETER                             :: MXNAMLEN=20
  INTEGER,         SAVE                           :: MXPAR,MXCLST,MXINST                              !seb added read in ability for these parameters. NOTE THEY ARE INDEPENDENTY OF LGR!!!
  INTEGER,         SAVE,                   POINTER:: ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
  CHARACTER(:),    SAVE,                   POINTER:: PROPPRINT
  REAL,            SAVE,  DIMENSION(:),    POINTER:: B
  INTEGER,         SAVE,  DIMENSION(:),    POINTER:: IACTIVE
  INTEGER,         SAVE,  DIMENSION(:,:),  POINTER:: IPLOC
  INTEGER,         SAVE,  DIMENSION(:,:),  POINTER:: IPCLST
  INTEGER,         SAVE,  DIMENSION(:,:,:),POINTER:: IZON
  DOUBLE PRECISION,SAVE,  DIMENSION(:,:,:),POINTER:: RMLT      !seb changed to double precision
  CHARACTER(MXNAMLEN),SAVE, DIMENSION(:),  POINTER:: PARNAM
  CHARACTER(4),       SAVE, DIMENSION(:),  POINTER:: PARTYP
  CHARACTER(MXNAMLEN),SAVE, DIMENSION(:),  POINTER:: ZONNAM
  CHARACTER(MXNAMLEN),SAVE, DIMENSION(:),  POINTER:: MLTNAM
  CHARACTER(MXNAMLEN),SAVE, DIMENSION(:),  POINTER:: INAME
  !
  TYPE PARAMTYPE
    INTEGER,POINTER  ::ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
    CHARACTER(:),                        POINTER:: PROPPRINT
    REAL,               DIMENSION(:),    POINTER:: B
    INTEGER,            DIMENSION(:),    POINTER:: IACTIVE
    INTEGER,            DIMENSION(:,:),  POINTER:: IPLOC
    INTEGER,            DIMENSION(:,:),  POINTER:: IPCLST
    INTEGER,            DIMENSION(:,:,:),POINTER:: IZON
    DOUBLE PRECISION,   DIMENSION(:,:,:),POINTER:: RMLT             !seb changed to double precision
    CHARACTER(MXNAMLEN),DIMENSION(:),    POINTER:: PARNAM
    CHARACTER(4),       DIMENSION(:),    POINTER:: PARTYP
    CHARACTER(MXNAMLEN),DIMENSION(:),    POINTER:: ZONNAM
    CHARACTER(MXNAMLEN),DIMENSION(:),    POINTER:: MLTNAM
    CHARACTER(MXNAMLEN),DIMENSION(:),    POINTER:: INAME
  END TYPE
  TYPE(PARAMTYPE), SAVE  ::PARAMDAT(10)
  !
END MODULE PARAMMODULE