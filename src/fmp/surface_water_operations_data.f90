MODULE SWO_HIERARCHY_DATA_TYPES
  !USE TABLEFILE_INTERFACE,        ONLY: TABFILETYPE1IDX, TABFILE_DEALLOCATE
  USE FILE_IO_INTERFACE,          ONLY: READ_TO_DATA
  USE PARSE_WORD_INTERFACE,       ONLY: PARSE_WORD
  USE STRINGS,                    ONLY: GET_INTEGER, GET_NUMBER
  USE ERROR_INTERFACE,            ONLY: STOP_ERROR
  USE ALLOC_INTERFACE,            ONLY: ALLOC
  USE NUM2STR_INTERFACE,          ONLY: NUM2STR
  USE WARNING_TYPE_INSTRUCTION,   ONLY: WARNING_TYPE
  USE LIST_ARRAY_INPUT_INTERFACE, ONLY: GENERIC_LINE_INPUT
  USE SFR_INPUT_DATA_TYPES,       ONLY: SFR_NAMED_LOCATION
  USE DATE_OPERATOR_INSTRUCTION,  ONLY: DATE_OPERATOR
  USE CONSTANTS
  USE ARRAY_DATA_TYPES, ONLY: DOUBLE_MATRIX
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: FARMDAT, UNITDAT, UNITDAT_ACCOUNTING_LOADER, DISTDAT, PROJDAT, AUXDAT, AUX_SFR_LOADER
  PUBLIC:: SWOPS_RESBAL, SWOPS_RESSPLIT, SWOPS_ACAPTBL, SWOPS_FRACTBL, SWOPS_RESHEAD, SWOPS_RESTYPE
  !-----------------------------------------------------------------------
  !     DERIVED TYPES @ FARM, UNIT, DIST, PROJ, AUX, EXT ...
  !-----------------------------------------------------------------------
  !
  ! FARM-LEVEL DATA -->
  ! Farms are defined by a common point of delivery / common demand ...
  TYPE FARMDAT
      INTEGER:: FarmID                      = Z
      INTEGER:: UnitID                      = Z
      INTEGER:: DistID                      = Z
      INTEGER:: ProjID                      = Z
      INTEGER:: DelSeg                      = Z
      INTEGER:: DelRch                      = Z
      INTEGER:: DEL_TOL_CNT                 = Z
      DOUBLE PRECISION:: AreaTot            = DZ
      DOUBLE PRECISION:: AreaIrr            = DZ
      DOUBLE PRECISION:: AreaSWIrr          = DZ
      DOUBLE PRECISION:: TFDR               = DZ
      DOUBLE PRECISION:: ALLOTMENT          = DZ
      DOUBLE PRECISION:: S_ALLOTMENT        = DZ
      DOUBLE PRECISION:: BALANCE            = DZ
      DOUBLE PRECISION:: DELORDER           = DZ
      DOUBLE PRECISION:: DELIVERY           = DZ
      DOUBLE PRECISION:: DELIVERY_YTD       = DZ
      DOUBLE PRECISION:: DIST_ALLOC_FRAC    = DZ
      DOUBLE PRECISION:: REQ_DELIVERY_S     = DZ
      DOUBLE PRECISION:: REQ_DELIVERY_VOL_S = DZ
      DOUBLE PRECISION:: REQ_DELIVERY_VOL   = DZ
      CONTAINS
      PROCEDURE, PRIVATE            ::  COPY_FARMDAT_TO_FARMDAT
      GENERIC::        ASSIGNMENT(=) => COPY_FARMDAT_TO_FARMDAT
  END TYPE
  !
  ! UNIT-LEVEL DATA -->
  ! Units are defined by a common point of diversion / point of charge ...
  !
  TYPE, EXTENDS(GENERIC_LINE_INPUT):: UNITDAT_ACCOUNTING_LOADER
      INTEGER:: IU
      CONTAINS
      PROCEDURE, PASS(GIN):: LOAD  => LOAD_UNITDAT_ACCOUNTING
      PROCEDURE, PASS(GIN):: ALLOC => ALLOC_UNITDAT_ACCOUNTING_NULL !--DOES NOTHING--
  END TYPE
  !
  TYPE UNITDAT
      INTEGER:: UnitID  = Z
      INTEGER:: DistID  = Z
      INTEGER:: ProjID  = Z
      INTEGER:: DivSeg  = Z
      INTEGER:: DivRch  = Z
      INTEGER:: ChgSeg  = Z
      INTEGER:: ChgRch  = Z
      INTEGER:: NBySeg  = Z
      INTEGER:: NCrdSeg = Z
      INTEGER, DIMENSION(:), ALLOCATABLE:: BySeg
      INTEGER, DIMENSION(:), ALLOCATABLE:: ByRch
      INTEGER, DIMENSION(:), ALLOCATABLE:: CrdSeg
      INTEGER, DIMENSION(:), ALLOCATABLE:: CrdRch
      !
      DOUBLE PRECISION::ChgFactor         = DZ
      DOUBLE PRECISION::DivFactor         = DZ
      DOUBLE PRECISION::AreaTot           = DZ
      DOUBLE PRECISION::AreaIrr           = DZ
      DOUBLE PRECISION::TFDR              = DZ
      DOUBLE PRECISION::ALLOTMENT         = DZ
      DOUBLE PRECISION::BALANCE           = DZ
      DOUBLE PRECISION::DELORDER          = DZ
      DOUBLE PRECISION::DIVORDER          = DZ
      DOUBLE PRECISION::DELIVERY          = DZ
      DOUBLE PRECISION::DELIVERY_YTD      = DZ
      DOUBLE PRECISION::DIVERSION         = DZ
      DOUBLE PRECISION::DIVERSION_YTD     = DZ
      DOUBLE PRECISION::BYPASS            = DZ
      DOUBLE PRECISION::BYPASS_YTD        = DZ
      DOUBLE PRECISION::CHARGE            = DZ
      DOUBLE PRECISION::CHARGE_YTD        = DZ
      DOUBLE PRECISION::CREDIT            = DZ
      DOUBLE PRECISION::CREDIT_YTD        = DZ
      DOUBLE PRECISION::DELIVEFF          = DZ
      DOUBLE PRECISION::DELIVEFF_YTD      = DZ
      DOUBLE PRECISION::DELIVEFF_PREV     = DZ
      DOUBLE PRECISION::CHGRATIO          = DZ
      DOUBLE PRECISION::CHGRATIO_YTD      = DZ
      DOUBLE PRECISION::NETCHGRATIO       = DZ
      DOUBLE PRECISION::NETCHGRATIO_YTD   = DZ
      DOUBLE PRECISION::NETCHGRATIO_PREV  = DZ
      DOUBLE PRECISION::DIVIN             = DZ
      DOUBLE PRECISION::DIVIN_YTD         = DZ
      DOUBLE PRECISION::SUMIN             = DZ
      DOUBLE PRECISION::SUMIN_YTD         = DZ
      DOUBLE PRECISION::SUMOUT            = DZ
      DOUBLE PRECISION::SUMOUT_YTD        = DZ
      !
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: ByFactor
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: CrdFactor
      !
      CONTAINS
      !
      GENERIC::             ASSIGNMENT(=) => COPY_UNITDAT_TO_UNITDAT
      PROCEDURE, PRIVATE::  COPY_UNITDAT_TO_UNITDAT
      PROCEDURE, PASS(UNI):: DEALLOCATE => DEALLOCATE_UNIT_ARRAYS
      FINAL::                              DEALLOCATE_UNIT_ARRAYS_FINAL
  END TYPE
  !
  ! DISTRICT-LEVEL DATA -->
  ! Districts are defined by a common allocation / allotment
  TYPE DISTDAT
      INTEGER:: DistID   = Z
      INTEGER:: ProjID   = Z
      INTEGER:: NFARM    = Z
      INTEGER:: NAUXDEM  = Z
      INTEGER, DIMENSION(:), ALLOCATABLE:: FARM
      INTEGER, DIMENSION(:), ALLOCATABLE:: AUXDEM
      LOGICAL:: S_ALLOTMENT_BYBEN           = FALSE
      DOUBLE PRECISION:: ALLOC_ANN          = DZ
      DOUBLE PRECISION:: ALLOC_CO           = DZ
      DOUBLE PRECISION:: ALLOC_TOTAL        = DZ
      DOUBLE PRECISION:: S_ALLOTMENT        = DZ
      DOUBLE PRECISION:: S_ALLOTMENT_VOL    = DZ
      DOUBLE PRECISION:: EQ_ALLOTMENT       = DZ
      DOUBLE PRECISION:: EQ_ALLOTMENT_LIMIT = DZ
      DOUBLE PRECISION:: AreaTot            = DZ
      DOUBLE PRECISION:: AreaIrr            = DZ
      DOUBLE PRECISION:: AreaAlloc          = DZ
      DOUBLE PRECISION:: BALANCE            = DZ
      DOUBLE PRECISION:: TFDR               = DZ
      DOUBLE PRECISION:: DELORDER           = DZ
      DOUBLE PRECISION:: DIVORDER           = DZ
      DOUBLE PRECISION:: DIVERSION          = DZ
      DOUBLE PRECISION:: DIVERSION_YTD      = DZ
      DOUBLE PRECISION:: DELIVERY           = DZ
      DOUBLE PRECISION:: DELIVERY_YTD       = DZ
      DOUBLE PRECISION:: BYPASS             = DZ
      DOUBLE PRECISION:: BYPASS_YTD         = DZ
      DOUBLE PRECISION:: CHARGE             = DZ
      DOUBLE PRECISION:: CHARGE_YTD         = DZ
      DOUBLE PRECISION:: CREDIT             = DZ
      DOUBLE PRECISION:: CREDIT_YTD         = DZ
      DOUBLE PRECISION:: DELIVEFF           = DZ
      DOUBLE PRECISION:: DELIVEFF_YTD       = DZ
      DOUBLE PRECISION:: DELIVEFF_PREV      = DZ
      DOUBLE PRECISION:: CHGRATIO           = DZ
      DOUBLE PRECISION:: CHGRATIO_YTD       = DZ
      DOUBLE PRECISION:: NETCHGRATIO        = DZ
      DOUBLE PRECISION:: NETCHGRATIO_YTD    = DZ
      DOUBLE PRECISION:: NETCHGRATIO_PREV   = DZ
      DOUBLE PRECISION:: DIVIN              = DZ
      DOUBLE PRECISION:: DIVIN_YTD          = DZ
      DOUBLE PRECISION:: SUMIN              = DZ
      DOUBLE PRECISION:: SUMIN_YTD          = DZ
      DOUBLE PRECISION:: SUMOUT             = DZ
      DOUBLE PRECISION:: SUMOUT_YTD         = DZ
      !
      CONTAINS
      !
      GENERIC::             ASSIGNMENT(=) => COPY_DISTDAT_TO_DISTDAT
      PROCEDURE, PRIVATE::  COPY_DISTDAT_TO_DISTDAT
  END TYPE
  !
  ! PROJECT-LEVEL DATA -->
  ! Projects are defined by a common water supply
  TYPE PROJDAT
      INTEGER:: ProjID      = Z
      INTEGER:: AllocType   = Z
      INTEGER:: AllocStart  = Z
      INTEGER:: AllocClose  = Z
      INTEGER:: AllocInit   = Z
      INTEGER:: DroughtYr   = Z
      INTEGER:: DroughtCount= Z
      !
      TYPE(DATE_OPERATOR):: AllocDate
      TYPE(DATE_OPERATOR):: AllocDate_At_TS
      DOUBLE PRECISION:: AllocDateFrac       = DZ
      DOUBLE PRECISION:: AllocDateFrac_At_TS = DZ
      DOUBLE PRECISION:: StorMax          = DZ
      DOUBLE PRECISION:: AreaTot          = DZ
      DOUBLE PRECISION:: AreaIrr          = DZ
      DOUBLE PRECISION:: ALLOCATION       = DZ
      DOUBLE PRECISION:: TFDR             = DZ
      DOUBLE PRECISION:: DELORDER         = DZ
      DOUBLE PRECISION:: DIVORDER         = DZ
      DOUBLE PRECISION:: RELEASE          = DZ
      DOUBLE PRECISION:: RELEASE_YTD      = DZ
      DOUBLE PRECISION:: DIVERSION        = DZ
      DOUBLE PRECISION:: DIVERSION_YTD    = DZ
      DOUBLE PRECISION:: DELIVERY         = DZ
      DOUBLE PRECISION:: DELIVERY_YTD     = DZ
      DOUBLE PRECISION:: BYPASS           = DZ
      DOUBLE PRECISION:: BYPASS_YTD       = DZ
      DOUBLE PRECISION:: DIVRATIO         = DZ
      DOUBLE PRECISION:: DIVRATIO_YTD     = DZ
      DOUBLE PRECISION:: DELIVEFF         = DZ
      DOUBLE PRECISION:: DELIVEFF_YTD     = DZ
      DOUBLE PRECISION:: DELIVEFF_PREV    = DZ
      DOUBLE PRECISION:: CHARGE           = DZ
      DOUBLE PRECISION:: CHARGE_YTD       = DZ
      DOUBLE PRECISION:: CREDIT           = DZ
      DOUBLE PRECISION:: CREDIT_YTD       = DZ
      DOUBLE PRECISION:: CHGRATIO         = DZ
      DOUBLE PRECISION:: CHGRATIO_YTD     = DZ
      DOUBLE PRECISION:: NETCHGRATIO      = DZ
      DOUBLE PRECISION:: NETCHGRATIO_YTD  = DZ
      DOUBLE PRECISION:: NETCHGRATIO_PREV = DZ
      DOUBLE PRECISION:: DIVIN            = DZ
      DOUBLE PRECISION:: DIVIN_YTD        = DZ
      DOUBLE PRECISION:: SUMIN            = DZ
      DOUBLE PRECISION:: SUMIN_YTD        = DZ
      DOUBLE PRECISION:: SUMOUT           = DZ
      DOUBLE PRECISION:: SUMOUT_YTD       = DZ
      !DOUBLE PRECISION:: OUTFLOW         = DZ
      !DOUBLE PRECISION:: OUTFLOW_YTD   !OUTFLOW NOT USED ANYMORE
      !
      CONTAINS
      !
      GENERIC::             ASSIGNMENT(=) => COPY_PROJDAT_TO_PROJDAT
      PROCEDURE, PRIVATE::  COPY_PROJDAT_TO_PROJDAT
  END TYPE
  !
  ! AUXILIARY DEMAND DATA -->
  ! Aux Demands are used to specify delivery requirements
  ! that are not represented by FMP
  TYPE AUXDAT 
      INTEGER:: AuxID       = Z
      INTEGER:: UnitID      = Z
      INTEGER:: DistID      = Z
      INTEGER:: ProjID      = Z
      INTEGER:: AuxSeg      = Z
      INTEGER:: AuxRch      = Z
      INTEGER:: DEL_TOL_CNT = Z
      !
      DOUBLE PRECISION:: Factor          = DZ
      DOUBLE PRECISION:: AREA            = DZ
      DOUBLE PRECISION:: DEMAND          = DZ
      DOUBLE PRECISION:: S_DEMAND        = DZ
      DOUBLE PRECISION:: DEMAND_LIM      = DZ
      DOUBLE PRECISION:: S_DEMAND_LIM    = DZ
      DOUBLE PRECISION:: ALLOTMENT       = DZ
      DOUBLE PRECISION:: S_ALLOTMENT     = DZ
      DOUBLE PRECISION:: BALANCE         = DZ
      DOUBLE PRECISION:: DELORDER        = DZ
      DOUBLE PRECISION:: DELIVERY        = DZ
      DOUBLE PRECISION:: DELIVERY_YTD    = DZ
      DOUBLE PRECISION:: DIST_ALLOC_FRAC = DZ
      !
      !TYPE(TABFILETYPE1IDX):: TABIDX_DEMAND, TABIDX_AREA
  END TYPE
  !
  TYPE, EXTENDS(GENERIC_LINE_INPUT):: AUX_SFR_LOADER
      INTEGER:: NAUX
      INTEGER,          DIMENSION(:),ALLOCATABLE:: SEG, RCH
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: FACTOR
      CONTAINS
      PROCEDURE, PASS(GIN):: ALLOC => ALLOCATE_AUX_SFR_TYPE
      PROCEDURE, PASS(GIN):: LOAD => LOAD_AUX_SFR_TYPE
  END TYPE
  !

!
!
!-----------------------------------------------------------------------
!     DERIVED TYPES @ RESERVOIRS/STORAGE ...
!-----------------------------------------------------------------------
!
      ! SWOPS_RESBAL -->
      ! holds inputs/variables to compute reservoir mass balance for current timestep for a single reservoir
      TYPE SWOPS_RESBAL
        CHARACTER(:),ALLOCATABLE:: RESNAME                                        ! name of reservoir
        !
        INTEGER :: RESBAL_PROJID   = Z              ! project associated with reservoir
        INTEGER :: RESBAL_RESID    = Z              ! unique reservoir ID
        INTEGER :: RESBAL_RELSEG   = Z              ! segment receiving reservoir release
        INTEGER :: SFR_INFLOW_SEG  = Z              ! segment whose outflow becomes reservoir inflow
        INTEGER :: SFR_INFLOW_ISTRM= Z              ! segment whose outflow becomes reservoir inflow
        INTEGER :: RESBAL_POOLFLG  = Z              ! flag indicating whether reservoir is individual or pooled/lumped
        INTEGER :: HEADCELL        = Z              ! flag indicating whether reservoir contributes to head boundary
        !
        INTEGER:: SPSTART = NEG                     ! STRESS PERIOD RESERVOIR BEGINS ITS USE
        LOGICAL:: INUSE   = FALSE
        !
        INTEGER:: MAIN_SPLT       = Z
        !
        LOGICAL:: SPILLWAY_PREF   = FALSE
        LOGICAL:: NO_MAX_SPILL_S  = TRUE
        !
        DOUBLE PRECISION :: &
              STORAGE_MIN_PREV     = DZ ,  &        ! non-project storage @ previous TS [L^3]
              STORAGE_PREV         = DZ ,  &        ! storage @ end of previous TS [L^3]
              AREA_PREV            = DZ ,  &        ! surface area @ end of previous TS [L^2]
              INFLOW               = DZ ,  &        ! total inflow during current TS [L^3]
              SFR_INFLOW           = DZ ,  &        ! total inflow from SFR (dynamic)
              PRCP                 = DZ ,  &        ! total precip onto reservoir during current TS [L]
              PRCP_FIN             = DZ ,  &        ! total precip onto reservoir during current TS [L3]
              EVAP                 = DZ ,  &        ! total evap from reservoir surface during current TS [L]
              EVAP_FIN             = DZ ,  &        ! total evap from reservoir surface during current TS [L3]
              RELEASE_TOT          = DZ ,  &        ! Sum of all release from main gate
              RELEASE_ALL          = DZ ,  &        ! Sume of all releases out of reservoir
              RELEASE_POT          = DZ ,  &        ! maximum potential release during current TS [L^3]
              RELEASE_PROJ_POT     = DZ ,  &        ! maximum potential release during current TS [L^3]
              RELEASE_PMAX         = DZ ,  &        ! maximum project release during current TS [L^3]
              RELEASE_DMND         = DZ ,  &        ! release required to meet all delivery orders during current TS [L^3]
              RELEASE_MIN_INPUT    = DZ ,  &        ! Minimum release that must always occur [L^3]
              RELEASE_MIN          = DZ ,  &        ! Minimum release that must always occur [L^3]
              RELEASE_SPEC         = DZ ,  &        ! total non-project release from reservoir during current TS [L^3]
              RELEASE_SPEC_FIN     = DZ ,  & 
              RELEASE_SPEC_S       = DZ ,  & 
              RELEASE_PROJ         = DZ ,  &        ! total project release from reservoir during current TS [L^3]
              RELEASE_PROJ_ADD_INI = DZ ,  & 
              RELEASE_PROJ_ADD     = DZ ,  & 
              RELEASE_FLOD         = DZ ,  &        ! total flood release from reservoir during current TS [L^3]
              RELEASE_REQF         = DZ ,  &        ! released water to meet a downstream required flow rate during current TS [L^3]
              RELEASE_REQF_PREV    = DZ ,  &        ! previous iteration RELEASE_REQF
              MAX_SPILL_RATE       = DZ ,  &        ! limit on max volume of spill per time step
              MAX_SPILL            = DZ ,  &        ! limit on max volume of spill per time step
              SPILL_WAY            = DZ ,  &        ! total spill release from reservoir during current TS [L^3]
              OVER_TOP             = DZ ,  &        ! total over hte top of reservoir release during current TS [L^3]
              STORAGE_SPEC_MIN     = DZ ,  &        ! non-project storage @ current TS [L^3]
              STORAGE_DPL          = DZ ,  &        ! Dead Pool Storage [L^3]
              STORAGE              = DZ ,  &        ! storage @ end of current TS [L^3]
              STORAGE_TRANSFER     = DZ ,  &        ! Water transfered into Rerevoir (negative is water removed)
              STORAGE_TRAN_FRAC    = DZ ,  &        ! Fraction of transfered water that is imediately availble in storage remainder is added at end of time step
              AREA                 = DZ ,  &        ! area @ end of current TS [L^2]
              AREA_DPL             = DZ ,  &        ! area @ dead pool (defined as drained of project storage leaving only non-project storage)
              MIN_RELEASE_S        = DZ ,  & 
              MAX_RELEASE          = DZ ,  & 
              MAX_RELEASE_VOL      = DZ ,  & 
              MAX_RELEASE_S        = DZ ,  & 
              MAX_RELEASE_S_VOL    = DZ ,  & 
              ADD_RELEASE_S        = DZ ,  & 
              ADD_RELEASE_S_VOL    = DZ ,  & 
              RELEASE_ADDF         = DZ ,  & 
              RELEASE_ADDF_INI     = DZ ,  & 
              STORAGE_CAPACITY     = DZ ,  & 
              STORAGE_SPILL_INPUT  = DZ ,  & 
              STORAGE_SPILL        = DZ ,  & 
              SPILL_STORE_S        = DZ ,  & 
              SPILL_STAGE_S        = DZ ,  & 
              MAX_STORAGE_S        = DZ ,  & 
              MAX_STAGE_S          = DZ ,  & 
              MIN_STORAGE_S        = DZ ,  & 
              MIN_STAGE_S          = DZ ,  & 
              MIN_STORAGE_TRAN_S   = DZ ,  & 
              !MAX_ELEV            = DZ ,  & 
              MIN_ELEV             = DZ ,  & 
              STOR_SPEC_MIN_ELEV   = DZ ,  & 
              STORAGE_MIN          = DZ ,  &        ! MIN ALLOWED STORAGE
              STORAGE_MAX          = DZ ,  &        ! MAX ALLOWED STORAGE
              STORAGE_MIN_TRAN     = DZ ,  &        ! MIN ALLOWED STORAGE ALLOWED FOR TRAN.RES.RES VARIABLE
              ELEV_CHNG            = DZ ,  & 
              ELEV_CHNG_S          = DZ ,  & 
              ELEV                 = DZ ,  & 
              ELEV_PREV            = DZ ,  & 
              MAX_AREA             = DZ ,  & 
              ACAP_MIN_ELEV        = DZ ,  & 
              ACAP_MAX_ELEV        = DZ ,  & 
              ACAP_MIN_STOR        = DZ ,  & 
              ACAP_MAX_STOR        = DZ ,  & 
              PRECIP_AREA_FRAC     = DZ ,  & 
              RELEASE_DMD_FRAC_INI = DZ ,  & 
              RELEASE_REQ_FRAC_INI = DZ ,  & 
              REL_DMD_FRAC         = DZ ,  & 
              REL_REQ_FRAC         = DZ ,  & 
              RELEASE              = DZ
          !TYPE(TABFILETYPE1IDX), ALLOCATABLE ::&
          !    TABIDX_INFLOW,         &                                   ! tabfile index
          !    TABIDX_PRCP,           &                                   ! tabfile index
          !    TABIDX_EVAP,           &                                   ! tabfile index
          !    TABIDX_RELEASE_SPEC,   &                                   ! tabfile index
          !    TABIDX_STORAGE_SPEC_MIN                                        ! tabfile index
      END TYPE

!
      ! SWOPS_RESSPLIT -->
      ! holds inputs/variables to compute reservoir mass balance for current timestep for a single reservoir
      TYPE SWOPS_RESSPLIT
        CHARACTER(:),ALLOCATABLE:: RESNAME                                        ! name of reservoir
        !
        LOGICAL:: MAIN_RES        = TRUE                                ! SET TO TRUE IF IT IS THE LARGEST OF THE SPLIT RESERVOIRS
        INTEGER:: RESSPLIT_PROJID = Z                                   ! project associated with reservoir
        INTEGER:: RESSPLIT_RESID  = Z                                   ! unique reservoir ID
        INTEGER:: HEADCELL        = Z                                   ! flag indicating whether reservoir contributes to head boundary
        DOUBLE PRECISION :: &
              DOY       = DZ ,   &                                      ! day-of-year used for FRACTBK look-up
              STORAGE   = DZ ,   &                                      ! storage @ end of current TS [L^3]
              AREA      = DZ ,   &                                      ! area @ end of current TS [L^2]
              MAX_ELEV  = DZ ,   &
              MIN_ELEV  = DZ ,   &
              ELEV      = DZ ,   &
              ELEV_PREV = DZ
      END TYPE
!
      ! SWOPS_ACAPTBL -->
      ! holds ACAP look-up table for single reservoir
      TYPE SWOPS_ACAPTBL
        CHARACTER(:),ALLOCATABLE :: RESNAME                           ! name of reservoir
        INTEGER ::            &
          ACAP_PROJID = Z ,   &                                       ! project associated with reservoir
          ACAP_RESID  = Z ,   &                                       ! unique reservoir ID
          ACAP_COUNT  = Z                                             ! number of values in ACAP table
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: &
              ACAP_STORAGE,   &                                       ! storage values for look-up
              ACAP_AREA   ,   &                                       ! area vales for look-up
              ACAP_ELEV                                               ! elevation values for look-up
      END TYPE
!
      ! SWOPS_FRACTBL -->
      ! holds reservoir split look-up table for single reservoir
      TYPE SWOPS_FRACTBL
        CHARACTER(:),ALLOCATABLE:: RESNAME                                                   ! name of reservoir
        INTEGER ::           &
          FRAC_PROJID = Z ,  &                                        ! project associated with reservoir
          FRAC_RESID  = Z ,  &                                        ! unique reservoir ID
          FRAC_COUNT  = Z 
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: & !FRAC_LEAP
              FRAC_NOLEAP,  &                                         ! day-of-year values for look-up
              FRAC_FRAC                                               ! fraction of total storage in res. for look-up
      END TYPE
!
      ! SWOPS_RESHEAD -->
      ! holds list of cells where each reservoir contributes to head boundary
      TYPE SWOPS_RESHEAD
        CHARACTER(:),ALLOCATABLE:: RESNAME                            ! name of reservoir
        INTEGER :: HEAD_PROJID = Z                                    ! project associated with reservoir
        INTEGER :: HEAD_RESID  = Z                                    ! unique reservoir ID
        INTEGER :: HEAD_NCELL  = Z                                    ! number of head cells for reservoir
        INTEGER,DIMENSION(:,:),ALLOCATABLE :: HEAD_CELLS              ! list of [row,col] where reservoir contributes to head boundary
      END TYPE
!
      ! RESTYPE --> wrapper type to hold all reservoir data types for a project
      TYPE SWOPS_RESTYPE
        ! DERIVED TYPE VARIABLES
          INTEGER:: NRESBAL = Z
          TYPE(SWOPS_ACAPTBL), DIMENSION(:),ALLOCATABLE :: ACAP
          TYPE(SWOPS_FRACTBL), DIMENSION(:),ALLOCATABLE :: FRAC
          TYPE(SWOPS_RESHEAD), DIMENSION(:),ALLOCATABLE :: HEAD
          TYPE(SWOPS_RESBAL),  DIMENSION(:),ALLOCATABLE :: RESBAL
          TYPE(SWOPS_RESSPLIT),DIMENSION(:),ALLOCATABLE :: RESSPLIT
          TYPE(DOUBLE_MATRIX), DIMENSION(:),ALLOCATABLE :: MAX_SPILL
          !DOUBLE PRECISION,    DIMENSION(:),ALLOCATABLE :: RELEASE_FRAC
          !
          DOUBLE PRECISION:: RELEASE_POT           = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_MAX      = DZ
          DOUBLE PRECISION:: PROJ_STORAGE_TOT      = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_DMD      = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_DMD_PREV = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_FIN      = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_PREV_FIN = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_ADD      = DZ
          DOUBLE PRECISION:: PROJ_RELEASE_ADD_S    = DZ
          !
          CONTAINS
          PROCEDURE, PASS(RESDAT):: SET_RELEASE_POT!()
      END TYPE
  !
  ! EXTERNAL DEMAND DATA -->
  ! Ext Demands are used to specify delivery obligations of project
  ! that are not part of a district allocation. This may include
  ! treaty deliveries, environmental flow targets, etc. External
  ! demands are served by an external allotment, but are not subject
  ! to district accounting
  !TYPE EXTDAT
  !    INTEGER:: ExtID
  !    INTEGER:: ProjID
  !    INTEGER:: ExtSeg
  !    INTEGER:: ExtRch
  !    DOUBLE PRECISION:: Factor
  !    DOUBLE PRECISION:: DEMAND
  !    DOUBLE PRECISION:: ALLOTMENT
  !    DOUBLE PRECISION:: BALANCE
  !    DOUBLE PRECISION:: DELORDER
  !    DOUBLE PRECISION:: DELIVERY,  DELIVERY_YTD
  !    TYPE(TABFILETYPE1IDX):: TABIDX_EXT
  !END TYPE
  !
  CONTAINS
  !
  PURE ELEMENTAL SUBROUTINE SET_RELEASE_POT(RESDAT)
    CLASS(SWOPS_RESTYPE), INTENT(INOUT):: RESDAT
    DOUBLE PRECISION:: AREA_AVG, PRCP_AREA, PRCP, EVAP, INFLOW, STOR_TRAN
    INTEGER::I
    !
    ! DETERMINE MAXIMUM POTENTIAL RELASE FROM RESERVOIR GIVEN CURRENT CONDITIONS (ASSUMING IT IS DRAINED TO NON-PROJECT STORAGE LEVELS)
    !
    RESDAT%RELEASE_POT = DZ  !TOTAL POTENTIAL FOR ALL RESERVOIRS
    !
    DO I=1, RESDAT%NRESBAL
        IF(RESDAT%RESBAL(I)%INUSE) THEN
           !
           ASSOCIATE(                                                   &
                     STORAGE      => RESDAT%RESBAL(I)%STORAGE_PREV    , &   ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
                     TSF_INFLOW   => RESDAT%RESBAL(I)%INFLOW          , &   ! INFLOW       specified during current step (updated @ AD routine!)          [L3]
                     SFR_INFLOW   => RESDAT%RESBAL(I)%SFR_INFLOW      , &   ! SFR_INFLOW   that originated from an SFR outflow
                     PRCP_LENTH   => RESDAT%RESBAL(I)%PRCP            , &   ! PRCP         during current step (updated @ AD routine!)          [L]
                     EVAP_LENTH   => RESDAT%RESBAL(I)%EVAP            , &   ! EVAP         during current step (updated @ AD routine!)          [L]
                     STORAGE_MIN  => RESDAT%RESBAL(I)%STORAGE_MIN     , &   ! Min allowed storage (either STORAGE_SPEC_MIN or greater) --Furthers point that can be released too
                     AREA_START   => RESDAT%RESBAL(I)%AREA_PREV       , &
                     AREA_END     => RESDAT%RESBAL(I)%AREA_DPL        , &   ! AREA_END @ non-project storage only ...
                     AREA_MAX     => RESDAT%RESBAL(I)%MAX_AREA        , &   ! Maximum area the reservoir could have (also represents maximum area precep falls on and makes its way to the reservoir)
                     PRCP_FRAC    => RESDAT%RESBAL(I)%PRECIP_AREA_FRAC, &   ! FRACTION OF TOTAL AREA TO AVERAGE AREA THAT PRECIP FALLS OVER
                     TRANSFER     => RESDAT%RESBAL(I)%STORAGE_TRANSFER, &
                     TRAN_FRAC    => RESDAT%RESBAL(I)%STORAGE_TRAN_FRAC,&
                     !
                     RELEASE_LIMIT=> RESDAT%RESBAL(I)%MAX_RELEASE_VOL , &   ! PHYSICAL LIMIT TO RESERVOIR RELEASE
                     !
                     RELEASE_POT  => RESDAT%RESBAL(I)%RELEASE_POT       &   ! FINAL POTENTIAL RELEASE FOR RESERVOIR
                    )
                    IF    (TRANSFER > DZ) THEN
                                          STOR_TRAN = TRANSFER * TRAN_FRAC  ! AVAILIBLE FOR IMMEDIATE RELEASE, REMAINDER AVAILIBLE AFTER RELEASES -- DEFAULT IS TRAN_FRAC = 0.5 FOR HALF BEING AVAILBLE AND HALF HELD
                    ELSEIF(TRANSFER < DZ) THEN
                                          STOR_TRAN = TRANSFER              !TRANFER REMOVES WATER, ENSURE IT SUPERCEDES RELEASES IN PRIORITY
                    ELSE
                                          STOR_TRAN = DZ
                    END IF
                    !
                    INFLOW = TSF_INFLOW + SFR_INFLOW !+... any additional inflows
                    !
                    AREA_AVG  = (AREA_START + AREA_END) * HALF
                    !
                    PRCP_AREA = PRCP_FRAC*(AREA_MAX - AREA_AVG) + AREA_AVG
                    !
                    PRCP = PRCP_LENTH*PRCP_AREA
                    !
                    EVAP = EVAP_LENTH*AREA_AVG
                    !
                    RELEASE_POT = STORAGE - STORAGE_MIN + STOR_TRAN + INFLOW + PRCP - EVAP !CONTINUITY
                    !
                    IF(RELEASE_POT > RELEASE_LIMIT) RELEASE_POT = RELEASE_LIMIT
                    IF(RELEASE_POT < DZ           ) RELEASE_POT = DZ
                    !
                    RESDAT%RELEASE_POT = RESDAT%RELEASE_POT + RELEASE_POT
                    !
           END ASSOCIATE
        ELSE
            RESDAT%RESBAL(I)%RELEASE_POT = DZ
        END IF
    END DO
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE COPY_FARMDAT_TO_FARMDAT(FARMDAT_OUT, FARMDAT_IN)
    CLASS(FARMDAT),INTENT(INOUT)::FARMDAT_OUT
    CLASS(FARMDAT),INTENT(IN   )::FARMDAT_IN
    !
    FARMDAT_OUT%FarmID       = FARMDAT_IN%FarmID
    FARMDAT_OUT%UnitID       = FARMDAT_IN%UnitID
    FARMDAT_OUT%DistID       = FARMDAT_IN%DistID
    FARMDAT_OUT%ProjID       = FARMDAT_IN%ProjID
    FARMDAT_OUT%DelSeg       = FARMDAT_IN%DelSeg
    FARMDAT_OUT%DelRch       = FARMDAT_IN%DelRch
    FARMDAT_OUT%AreaTot      = FARMDAT_IN%AreaTot
    FARMDAT_OUT%AreaIrr      = FARMDAT_IN%AreaIrr
    FARMDAT_OUT%TFDR         = FARMDAT_IN%TFDR
    FARMDAT_OUT%ALLOTMENT    = FARMDAT_IN%ALLOTMENT
    FARMDAT_OUT%BALANCE      = FARMDAT_IN%BALANCE
    FARMDAT_OUT%DELORDER     = FARMDAT_IN%DELORDER
    FARMDAT_OUT%DELIVERY     = FARMDAT_IN%DELIVERY
    FARMDAT_OUT%DELIVERY_YTD = FARMDAT_IN%DELIVERY_YTD
    FARMDAT_OUT%DIST_ALLOC_FRAC = FARMDAT_IN%DIST_ALLOC_FRAC
    FARMDAT_OUT%S_ALLOTMENT     = FARMDAT_IN%S_ALLOTMENT
    FARMDAT_OUT%REQ_DELIVERY_VOL   = FARMDAT_IN%REQ_DELIVERY_VOL
    FARMDAT_OUT%REQ_DELIVERY_S     = FARMDAT_IN%REQ_DELIVERY_S
    FARMDAT_OUT%REQ_DELIVERY_VOL_S = FARMDAT_IN%REQ_DELIVERY_VOL_S
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE COPY_UNITDAT_TO_UNITDAT(UNITDAT_OUT, UNITDAT_IN)
    CLASS(UNITDAT),INTENT(INOUT)::UNITDAT_OUT
    CLASS(UNITDAT),INTENT(IN   )::UNITDAT_IN
    !
    UNITDAT_OUT%UnitID  = UNITDAT_IN%UnitID
    UNITDAT_OUT%DistID  = UNITDAT_IN%DistID
    UNITDAT_OUT%ProjID  = UNITDAT_IN%ProjID
    UNITDAT_OUT%DivSeg  = UNITDAT_IN%DivSeg
    UNITDAT_OUT%DivRch  = UNITDAT_IN%DivRch
    UNITDAT_OUT%ChgSeg  = UNITDAT_IN%ChgSeg
    UNITDAT_OUT%ChgRch  = UNITDAT_IN%ChgRch
    UNITDAT_OUT%NBySeg  = UNITDAT_IN%NBySeg
    UNITDAT_OUT%NCrdSeg = UNITDAT_IN%NCrdSeg
    !
    IF(UNITDAT_OUT%NBySeg>Z) THEN
        CALL ALLOC(UNITDAT_OUT%BySeg, UNITDAT_OUT%NBySeg )
        CALL ALLOC(UNITDAT_OUT%ByRch, UNITDAT_OUT%NBySeg )
        UNITDAT_OUT%BySeg = UNITDAT_IN%BySeg
        UNITDAT_OUT%ByRch = UNITDAT_IN%ByRch
    END IF
    IF(UNITDAT_OUT%NCrdSeg>Z) THEN
        CALL ALLOC(UNITDAT_OUT%CrdSeg, UNITDAT_OUT%NCrdSeg)
        CALL ALLOC(UNITDAT_OUT%CrdRch, UNITDAT_OUT%NCrdSeg)
        UNITDAT_OUT%CrdSeg = UNITDAT_IN%CrdSeg
        UNITDAT_OUT%CrdRch = UNITDAT_IN%CrdRch
    END IF
    !
    UNITDAT_OUT%ChgFactor       = UNITDAT_IN%ChgFactor
    UNITDAT_OUT%DivFactor       = UNITDAT_IN%DivFactor
    UNITDAT_OUT%AreaTot         = UNITDAT_IN%AreaTot
    UNITDAT_OUT%AreaIrr         = UNITDAT_IN%AreaIrr
    UNITDAT_OUT%TFDR            = UNITDAT_IN%TFDR
    UNITDAT_OUT%DELORDER        = UNITDAT_IN%DELORDER
    UNITDAT_OUT%DIVORDER        = UNITDAT_IN%DIVORDER
    UNITDAT_OUT%DELIVERY        = UNITDAT_IN%DELIVERY
    UNITDAT_OUT%DIVERSION       = UNITDAT_IN%DIVERSION
    UNITDAT_OUT%BYPASS          = UNITDAT_IN%BYPASS
    UNITDAT_OUT%CHARGE          = UNITDAT_IN%CHARGE
    UNITDAT_OUT%CREDIT          = UNITDAT_IN%CREDIT
    UNITDAT_OUT%DELIVEFF        = UNITDAT_IN%DELIVEFF
    UNITDAT_OUT%CHGRATIO        = UNITDAT_IN%CHGRATIO
    UNITDAT_OUT%NETCHGRATIO     = UNITDAT_IN%NETCHGRATIO
    UNITDAT_OUT%DIVIN           = UNITDAT_IN%DIVIN
    UNITDAT_OUT%SUMIN           = UNITDAT_IN%SUMIN
    UNITDAT_OUT%SUMOUT          = UNITDAT_IN%SUMOUT
    UNITDAT_OUT%DELIVERY_YTD    = UNITDAT_IN%DELIVERY_YTD
    UNITDAT_OUT%DIVERSION_YTD   = UNITDAT_IN%DIVERSION_YTD
    UNITDAT_OUT%BYPASS_YTD      = UNITDAT_IN%BYPASS_YTD
    UNITDAT_OUT%CHARGE_YTD      = UNITDAT_IN%CHARGE_YTD
    UNITDAT_OUT%CREDIT_YTD      = UNITDAT_IN%CREDIT_YTD
    UNITDAT_OUT%DELIVEFF_YTD    = UNITDAT_IN%DELIVEFF_YTD
    UNITDAT_OUT%CHGRATIO_YTD    = UNITDAT_IN%CHGRATIO_YTD
    UNITDAT_OUT%NETCHGRATIO_YTD = UNITDAT_IN%NETCHGRATIO_YTD
    UNITDAT_OUT%DIVIN_YTD       = UNITDAT_IN%DIVIN_YTD
    UNITDAT_OUT%SUMIN_YTD       = UNITDAT_IN%SUMIN_YTD
    UNITDAT_OUT%SUMOUT_YTD      = UNITDAT_IN%SUMOUT_YTD
    UNITDAT_OUT%ALLOTMENT       = UNITDAT_IN%ALLOTMENT
    UNITDAT_OUT%BALANCE         = UNITDAT_IN%BALANCE
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE COPY_DISTDAT_TO_DISTDAT(DISTDAT_OUT, DISTDAT_IN)
    CLASS(DISTDAT),INTENT(INOUT)::DISTDAT_OUT
    CLASS(DISTDAT),INTENT(IN   )::DISTDAT_IN
    !
    DISTDAT_OUT%DistID          = DISTDAT_IN%DistID
    DISTDAT_OUT%ProjID          = DISTDAT_IN%ProjID
    DISTDAT_OUT%ALLOC_ANN       = DISTDAT_IN%ALLOC_ANN
    DISTDAT_OUT%ALLOC_CO        = DISTDAT_IN%ALLOC_CO
    DISTDAT_OUT%ALLOC_TOTAL     = DISTDAT_IN%ALLOC_TOTAL
    DISTDAT_OUT%S_ALLOTMENT     = DISTDAT_IN%S_ALLOTMENT
    DISTDAT_OUT%EQ_ALLOTMENT    = DISTDAT_IN%EQ_ALLOTMENT
    DISTDAT_OUT%EQ_ALLOTMENT_LIMIT=DISTDAT_IN%EQ_ALLOTMENT_LIMIT
    DISTDAT_OUT%AreaTot         = DISTDAT_IN%AreaTot
    DISTDAT_OUT%AreaIrr         = DISTDAT_IN%AreaIrr
    DISTDAT_OUT%AreaAlloc       = DISTDAT_IN%AreaAlloc
    DISTDAT_OUT%BALANCE         = DISTDAT_IN%BALANCE
    DISTDAT_OUT%TFDR            = DISTDAT_IN%TFDR
    DISTDAT_OUT%DELORDER        = DISTDAT_IN%DELORDER
    DISTDAT_OUT%DIVORDER        = DISTDAT_IN%DIVORDER
    DISTDAT_OUT%DIVERSION       = DISTDAT_IN%DIVERSION
    DISTDAT_OUT%DELIVERY        = DISTDAT_IN%DELIVERY
    DISTDAT_OUT%BYPASS          = DISTDAT_IN%BYPASS
    DISTDAT_OUT%CHARGE          = DISTDAT_IN%CHARGE
    DISTDAT_OUT%CREDIT          = DISTDAT_IN%CREDIT
    DISTDAT_OUT%DELIVEFF        = DISTDAT_IN%DELIVEFF
    DISTDAT_OUT%CHGRATIO        = DISTDAT_IN%CHGRATIO
    DISTDAT_OUT%NETCHGRATIO     = DISTDAT_IN%NETCHGRATIO
    DISTDAT_OUT%DIVIN           = DISTDAT_IN%DIVIN
    DISTDAT_OUT%SUMIN           = DISTDAT_IN%SUMIN
    DISTDAT_OUT%SUMOUT          = DISTDAT_IN%SUMOUT
    DISTDAT_OUT%DIVERSION_YTD   = DISTDAT_IN%DIVERSION_YTD
    DISTDAT_OUT%DELIVERY_YTD    = DISTDAT_IN%DELIVERY_YTD
    DISTDAT_OUT%BYPASS_YTD      = DISTDAT_IN%BYPASS_YTD
    DISTDAT_OUT%CHARGE_YTD      = DISTDAT_IN%CHARGE_YTD
    DISTDAT_OUT%CREDIT_YTD      = DISTDAT_IN%CREDIT_YTD
    DISTDAT_OUT%DELIVEFF_YTD    = DISTDAT_IN%DELIVEFF_YTD
    DISTDAT_OUT%CHGRATIO_YTD    = DISTDAT_IN%CHGRATIO_YTD
    DISTDAT_OUT%NETCHGRATIO_YTD = DISTDAT_IN%NETCHGRATIO_YTD
    DISTDAT_OUT%DIVIN_YTD       = DISTDAT_IN%DIVIN_YTD
    DISTDAT_OUT%SUMIN_YTD       = DISTDAT_IN%SUMIN_YTD
    DISTDAT_OUT%SUMOUT_YTD      = DISTDAT_IN%SUMOUT_YTD
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE COPY_PROJDAT_TO_PROJDAT(PROJDAT_OUT, PROJDAT_IN)
    CLASS(PROJDAT),INTENT(INOUT)::PROJDAT_OUT
    CLASS(PROJDAT),INTENT(IN   )::PROJDAT_IN
    !
    PROJDAT_OUT%ProjID             = PROJDAT_IN%ProjID
    PROJDAT_OUT%AllocType          = PROJDAT_IN%AllocType
    PROJDAT_OUT%AllocStart         = PROJDAT_IN%AllocStart
    PROJDAT_OUT%AllocClose         = PROJDAT_IN%AllocClose
    PROJDAT_OUT%AllocInit          = PROJDAT_IN%AllocInit
    PROJDAT_OUT%DroughtYr          = PROJDAT_IN%DroughtYr
    PROJDAT_OUT%DroughtCount       = PROJDAT_IN%DroughtCount
    PROJDAT_OUT%AllocDate          = PROJDAT_IN%AllocDate
    PROJDAT_OUT%AllocDateFrac      = PROJDAT_IN%AllocDateFrac
    PROJDAT_OUT%AllocDate          = PROJDAT_IN%AllocDate_At_TS
    PROJDAT_OUT%AllocDateFrac      = PROJDAT_IN%AllocDateFrac_At_TS
    PROJDAT_OUT%AreaTot            = PROJDAT_IN%AreaTot
    PROJDAT_OUT%AreaIrr            = PROJDAT_IN%AreaIrr
    PROJDAT_OUT%ALLOCATION         = PROJDAT_IN%ALLOCATION
    PROJDAT_OUT%TFDR               = PROJDAT_IN%TFDR
    PROJDAT_OUT%DELORDER           = PROJDAT_IN%DELORDER
    PROJDAT_OUT%DIVORDER           = PROJDAT_IN%DIVORDER
    PROJDAT_OUT%RELEASE            = PROJDAT_IN%RELEASE
    PROJDAT_OUT%DIVERSION          = PROJDAT_IN%DIVERSION
    !PROJDAT_OUT%OUTFLOW            = PROJDAT_IN%OUTFLOW
    PROJDAT_OUT%DELIVERY           = PROJDAT_IN%DELIVERY
    PROJDAT_OUT%BYPASS             = PROJDAT_IN%BYPASS
    PROJDAT_OUT%DIVRATIO           = PROJDAT_IN%DIVRATIO
    PROJDAT_OUT%DELIVEFF           = PROJDAT_IN%DELIVEFF
    PROJDAT_OUT%CHARGE             = PROJDAT_IN%CHARGE
    PROJDAT_OUT%CREDIT             = PROJDAT_IN%CREDIT
    PROJDAT_OUT%CHGRATIO           = PROJDAT_IN%CHGRATIO
    PROJDAT_OUT%NETCHGRATIO        = PROJDAT_IN%NETCHGRATIO
    PROJDAT_OUT%DIVIN              = PROJDAT_IN%DIVIN
    PROJDAT_OUT%SUMIN              = PROJDAT_IN%SUMIN
    PROJDAT_OUT%SUMOUT             = PROJDAT_IN%SUMOUT
    PROJDAT_OUT%RELEASE_YTD        = PROJDAT_IN%RELEASE_YTD
    PROJDAT_OUT%DIVERSION_YTD      = PROJDAT_IN%DIVERSION_YTD
    !PROJDAT_OUT%OUTFLOW_YTD        = PROJDAT_IN%OUTFLOW_YTD
    PROJDAT_OUT%DELIVERY_YTD       = PROJDAT_IN%DELIVERY_YTD
    PROJDAT_OUT%BYPASS             = PROJDAT_IN%BYPASS
    PROJDAT_OUT%BYPASS_YTD         = PROJDAT_IN%BYPASS_YTD
    PROJDAT_OUT%DIVRATIO_YTD       = PROJDAT_IN%DIVRATIO_YTD
    PROJDAT_OUT%DELIVEFF_YTD       = PROJDAT_IN%DELIVEFF_YTD
    PROJDAT_OUT%CHARGE_YTD         = PROJDAT_IN%CHARGE_YTD
    PROJDAT_OUT%CREDIT_YTD         = PROJDAT_IN%CREDIT_YTD
    PROJDAT_OUT%CHGRATIO_YTD       = PROJDAT_IN%CHGRATIO_YTD
    PROJDAT_OUT%NETCHGRATIO_YTD    = PROJDAT_IN%NETCHGRATIO_YTD
    PROJDAT_OUT%DIVIN_YTD          = PROJDAT_IN%DIVIN_YTD
    PROJDAT_OUT%SUMIN_YTD          = PROJDAT_IN%SUMIN_YTD
    PROJDAT_OUT%SUMOUT_YTD         = PROJDAT_IN%SUMOUT_YTD
    !
  END SUBROUTINE
  !
  !PURE SUBROUTINE ADD_FARMDAT_HIERARCHY_SRD(FDAT, FID, FARM, ISRD)
  !  CLASS(FARMDAT),           INTENT(INOUT):: FDAT
  !  INTEGER,                  INTENT(IN   ):: FID
  !  TYPE (FARM_RELATIONSHIP), INTENT(IN   ):: FARM
  !  INTEGER, DIMENSION(TWO),  INTENT(IN   ):: ISRD
  !  !
  !  FDAT%FarmID = FID
  !  FDAT%UnitID = FARM%UNIT
  !  FDAT%DistID = FARM%DIST
  !  FDAT%ProjID = FARM%PROJ
  !  FDAT%DelSeg = ISRD(ONE)
  !  FDAT%DelRch = ISRD(TWO)
  !  !
  !END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE DEALLOCATE_UNIT_ARRAYS(UNI)
    CLASS(UNITDAT), INTENT(INOUT):: UNI
    !
    IF(ALLOCATED(UNI%BySeg    )) DEALLOCATE(UNI%BySeg    )
    IF(ALLOCATED(UNI%ByRch    )) DEALLOCATE(UNI%ByRch    )
    IF(ALLOCATED(UNI%ByFactor )) DEALLOCATE(UNI%ByFactor )
    IF(ALLOCATED(UNI%CrdSeg   )) DEALLOCATE(UNI%CrdSeg   )
    IF(ALLOCATED(UNI%CrdRch   )) DEALLOCATE(UNI%CrdRch   )
    IF(ALLOCATED(UNI%CrdFactor)) DEALLOCATE(UNI%CrdFactor)
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE DEALLOCATE_UNIT_ARRAYS_FINAL(UNI)
    TYPE(UNITDAT), INTENT(INOUT):: UNI
    CALL DEALLOCATE_UNIT_ARRAYS(UNI)
  END SUBROUTINE
  !
  SUBROUTINE ALLOC_UNITDAT_ACCOUNTING_NULL(GIN, DIM, WILD_IN, WILD_OUT, WILD_1D_IN, WILD_1D_OUT)
    CLASS(UNITDAT_ACCOUNTING_LOADER),      INTENT(INOUT):: GIN
    INTEGER,     DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: DIM
    CLASS(*),    OPTIONAL,                 INTENT(IN   ):: WILD_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL,                 INTENT(INOUT):: WILD_OUT
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: WILD_1D_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: WILD_1D_OUT
  END SUBROUTINE
  !
  SUBROUTINE LOAD_UNITDAT_ACCOUNTING(GIN, LLOC, LINE, IU, WILD_IN, WILD_OUT, WILD_1D_IN, WILD_1D_OUT)
    CLASS(UNITDAT_ACCOUNTING_LOADER), INTENT(INOUT):: GIN
    INTEGER,                          INTENT(INOUT):: LLOC
    CHARACTER(*),                     INTENT(INOUT):: LINE
    INTEGER,                          INTENT(IN   ):: IU
    CLASS(*),    OPTIONAL,            INTENT(IN   ):: WILD_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL,            INTENT(INOUT):: WILD_OUT
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: WILD_1D_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: WILD_1D_OUT
    INTEGER:: ISTART, ISTOP
    !
    IF(IU == Z) THEN
        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP) !NO NEED TO UPCASE CAUSE IT WILL ALREADY BE SO
        SELECT CASE(LINE(ISTART:ISTOP))
        CASE('SKIP');     CALL STOP_ERROR( LINE=LINE, OUTPUT=GIN%IOUT, MSG= 'SWO ACCOUNTING UNIT CREDIT AND CHARGE ERROR: THIS INPUT READ UTILITY DOES NOT ALLOW FOR THE KEYWORD "SKIP". PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)')
        CASE('CONSTANT'); CALL STOP_ERROR( LINE=LINE, OUTPUT=GIN%IOUT, MSG= 'SWO ACCOUNTING UNIT CREDIT AND CHARGE ERROR: THIS INPUT READ UTILITY DOES NOT ALLOW FOR THE KEYWORD "CONSTANT". PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)')
        !CASE('REPEAT')
        END SELECT
    END IF
    !
    GIN%IU = IU  ! NOTE IU = Z IF 'REPEAT'
    !
  END SUBROUTINE
  !
  SUBROUTINE ALLOCATE_AUX_SFR_TYPE(GIN, DIM, WILD_IN, WILD_OUT, WILD_1D_IN, WILD_1D_OUT)
    CLASS(AUX_SFR_LOADER), INTENT(INOUT):: GIN
    !INTEGER,                   INTENT(IN):: NAUX
    INTEGER,     DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: DIM
    CLASS(*),    OPTIONAL,                 INTENT(IN   ):: WILD_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL,                 INTENT(INOUT):: WILD_OUT
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: WILD_1D_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: WILD_1D_OUT
    !
    GIN%NAUX = DIM(ONE)
    !
    IF(ALLOCATED(GIN%SEG)) DEALLOCATE(GIN%SEG)
    IF(ALLOCATED(GIN%RCH)) DEALLOCATE(GIN%RCH)
    IF(ALLOCATED(GIN%FACTOR)) DEALLOCATE(GIN%FACTOR)
    !
    IF(GIN%NAUX>Z) THEN
                   ALLOCATE(GIN%SEG   (GIN%NAUX))
                   ALLOCATE(GIN%RCH   (GIN%NAUX))
                   ALLOCATE(GIN%FACTOR(GIN%NAUX))
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_AUX_SFR_TYPE(GIN, LLOC, LINE, IU, WILD_IN, WILD_OUT, WILD_1D_IN, WILD_1D_OUT)
    CLASS(AUX_SFR_LOADER), INTENT(INOUT):: GIN
    INTEGER,               INTENT(INOUT):: LLOC
    CHARACTER(*),          INTENT(INOUT):: LINE
    INTEGER,               INTENT(IN   ):: IU
    CLASS(*),    OPTIONAL, INTENT(IN   ):: WILD_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL, INTENT(INOUT):: WILD_OUT
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: WILD_1D_IN  !ANY OPTIONAL DATA TYPE THAT MAYBE SET OR NECESSARY TO PASS IN (WILD CARD)
    CLASS(*),    OPTIONAL, DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: WILD_1D_OUT
    INTEGER:: ISTART, ISTOP, I
    TYPE(WARNING_TYPE):: WRN
    !
    CALL WRN%INIT()
    !
    IF(IU == Z) THEN
        !
        SELECT CASE(LINE(ISTART:ISTOP))
        CASE('REPEAT'  ); CONTINUE  !DO NOTHING
        CASE('SKIP'    )
                          GIN%SEG = Z
                          GIN%RCH = Z
                          GIN%FACTOR = UNO
        CASE('CONSTANT')
                          CALL STOP_ERROR( LINE=LINE, OUTPUT=GIN%IOUT, MSG= 'LOAD FMP SFR SEMI-ROUTED DELIVERY SEGMENT AND REACH ERROR: THIS INPUT DOES NOT ALLOW FOR THE KEYWORD "CONSTANT". PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)')
        CASE DEFAULT
                          CALL STOP_ERROR( LINE=LINE, OUTPUT=GIN%IOUT, INFILE=IU, MSG= 'LOAD FMP SFR SEMI-ROUTED DELIVERY SEGMENT AND REACH ERROR: THIS INPUT DOES NOT ALLOW FOR IMPLIED INTERNAL LOADING. PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)')
        END SELECT
    ELSE
       SELECT TYPE(SFR=>WILD_IN)
       TYPE IS(SFR_NAMED_LOCATION)
          DO I=ONE, GIN%NAUX
                 CALL READ_TO_DATA(LINE, IU, GIN%IOUT)
                 LLOC=ONE
                 CALL SFR%GET(LINE, LLOC, ISTART, ISTOP, GIN%IOUT, IU, GIN%SEG(I), GIN%RCH(I))
                 !CALL PARSE_WORD( LINE,LLOC,ISTART,ISTOP) !PASS AUX ID
                 !CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,GIN%IOUT,IU,GIN%SEG(I),    MSG='FAILED TO LOAD AUX SFR SEGMENT')
                 !CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,GIN%IOUT,IU,GIN%RCH(I),    MSG='FAILED TO LOAD AUX SFR REACH')
                 CALL GET_NUMBER( LINE,LLOC,ISTART,ISTOP,GIN%IOUT,IU,GIN%FACTOR(I), MSG='FAILED TO LOAD AUX SFR LOSS FACTOR')
                 !
                 IF(GIN%FACTOR(I) < NEARZERO_5) THEN
                     CALL WRN%ADD('AUXILIARY '//NUM2STR(I)//' FACTOR IS LESS THEN 1E-5, SO IT WAS SET TO 1E-5'//NL)
                     GIN%FACTOR(I) = NEARZERO_5
                 ELSEIF(GIN%FACTOR(I) > UNO) THEN
                     IF(GIN%FACTOR(I) > NEAR_ONE_5) CALL WRN%ADD('AUXILIARY '//NUM2STR(I)//' FACTOR IS GREATER THEN 1, SO IT WAS SET TO 1'//NL)
                     GIN%FACTOR(I) = UNO
                 END IF
          END DO
       END SELECT
    END IF
    !
  END SUBROUTINE
END MODULE
!
!######################################################################################################################
!######################################################################################################################
!######################################################################################################################
!
MODULE SWO_SURFACE_WATER_DATA_TYPES
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: SEGDAT, RCHDAT, SWOPS_SEGLIST, SWOPS_SEGTREE, SEGINFODAT, DIVSEGDAT, SPTSEGDAT
  !-----------------------------------------------------------------------
  !     DERIVED TYPES @ SURFACE WATER NETWORK ...
  !-----------------------------------------------------------------------
  !
      ! SEGDATA -->
      ! Holds SFR data at segment level (original or computed)...
      TYPE SEGDAT
          INTEGER ::  &
            SegID,    &
            RchIDX,   &                                                 ! Index of first reach in segment (mapping between NSS and NSTRM for first reach in segment)
            NRCH,     &                                                 ! Number of reaches in segment
            IUPSEG,   &                                                 ! IDIVAR(1,NSEG)
            OUTSEG,   &                                                 ! IOTSG(NSEG)
            IPRIOR,   &                                                 ! IDIVAR(2,NSEG)
            DDFLAG,   &                                                 ! Flag indicating if there is a delivery order downstream of segment ...
            RRFLAG                                                      ! Flag indicating if segment is a reservoir release segment
          DOUBLE PRECISION :: &
            LENGTH,           &                                         ! SEG(1,NSEG)
            FLOW,             &                                         ! SEG(2,NSEG)
            INFLOW,           &                                         ! STRM(10,[first reach in segment])
            OUTFLOW,          &                                         ! STRM( 9,[last reach in segment])
            SEEPAGE,          &                                         ! sum{STRM(11,[all reaches in segment])}
            RUNOFF,           &                                         ! sum{STRM(12,[all reaches in segment])}
            STRM_ET,          &                                         ! sum{STRM(13,[all reaches in segment])}
            STRM_PRCP,        &                                         ! sum{STRM(14,[all reaches in segment])}
            DIVERSION,        &                                         ! diversion into conveyance OR flow into split
            DELIVERY,         &                                         ! farm delivery from segment
            EFFICIENCY                                                  ! segment conveyance efficiency
      END TYPE
      !
      ! Holds SFR data at reach level (original or computed)...
      TYPE RCHDAT
          INTEGER :: &
            RchID,   &                                                   ! Reach number (index from NSTRM loop)
            RchSeg,  &                                                   ! Segment corresponding to reach
            RchCnt                                                       ! Reach number within segment
          DOUBLE PRECISION :: &
            LENGTH,           &                                         ! STRM( 1, xx )
            INFLOW,           &                                         ! STRM(10, xx )
            OUTFLOW,          &                                         ! STRM( 9, xx )
            SEEPAGE,          &                                         ! STRM(11, xx )
            RUNOFF,           &                                         ! STRM(12, xx )
            STRM_ET,          &                                         ! STRM(13, xx )
            STRM_PRCP,        &                                         ! STRM(14, xx )
            EFFICIENCY                                                  ! segment conveyance efficiency
      END TYPE
      !
      ! SEGLIST -->
      ! holds list of immediate up- or down-stream segments for each SFR segment in model
      ! (immediate upstream segments include segments that contribute flow as outflow
      !  as well as segments that contribute flow by diversion)
      TYPE SWOPS_SEGLIST
          INTEGER ::  &
              NSEG,   &                                                 ! segment number for current segment
              NLIST                                                     ! number of immediate up/down-stream segments contributing to current segment
          INTEGER,DIMENSION(:),ALLOCATABLE :: SEGLIST                   ! array of segment numbers for immediate up/down-stream segments
      END TYPE
      !
      ! SWOPS_SEGTREE -->
      ! holds list of ALL up- or down-stream segments of a given segtype
      ! for each SFR segment in model
      TYPE SWOPS_SEGTREE
          INTEGER :: &
              NSEG,  &                                                  ! segment number for current segment
              NTREE                                                     ! number of up/down-stream segments connected to current segment
          INTEGER,DIMENSION(:),ALLOCATABLE :: SEGTREE                   ! array of segment numbers for segments connected to current segment
      END TYPE
!
!
!-----------------------------------------------------------------------
!     DERIVED TYPES @ DIVERSION, SPLIT, BRANCH ...
!-----------------------------------------------------------------------
!
      ! SEGMENT-LEVEL DATA -->
      TYPE SEGINFODAT
          INTEGER ::SegID, SegType, ProjID, UnitID, DistID, FarmID, AuxID
      END TYPE
      !
      ! DIVERSION-LEVEL DATA -->
      ! Data for each unique point of diversion defined by SWOPS inputs
      ! (list of unit divsegs with duplicates removed)
      TYPE DIVSEGDAT
          INTEGER :: DivID, DivSeg, ProjID, NUnit, NRES
          INTEGER, DIMENSION(:), ALLOCATABLE :: UnitID, RES
          DOUBLE PRECISION ::                               &
            TFDR,                                           &
            DELORDER,                                       &
            DIVORDER,                                       &
            DIVORDER_PREV,                                  &
            DIVERSION,                                      &
            DELIVERY,                                       &
            DELIVEFF,                                       &
            DELIVEFF_PREV,                                  &
            DSEEP, DFLOW_IN, DFLOW_RT,                      &
            UP_DIVORDER, UP_DSEEP, UP_DFLOW_IN, UP_DFLOW_RT, UP_DFLOW_RN, UP_DFLOW_ET
      END TYPE
      !
      ! SPLIT-LEVEL DATA -->
      ! Data for each junction in surface water network
      TYPE SPTSEGDAT
          INTEGER :: DivID, DivSeg, SplitID, SplitSeg, NBRANCH, ProjID
          DOUBLE PRECISION ::        &
            TFDR,                    &
            DELORDER,                &
            DIVORDER,                &
            DIVORDER_PREV,           &
            DIVERSION,               &
            DELIVERY,                &
            DELIVEFF,                &
            DELIVEFF_PREV,           &
            DSEEP, DFLOW_IN, DFLOW_RT
          INTEGER, DIMENSION(:), ALLOCATABLE ::BrcSeg, BrcType     ! 1=outseg, 2=divseg
          DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  &
            BrcTFDR,                                      &
            BrcDELORDER,                                  &
            BrcDIVORDER,                                  &
            BrcDIVORDER_PREV,                             &
            BrcDIVERSION,                                 &
            BrcDELIVERY,                                  &
            BrcDELIVEFF,                                  &
            BrcDELIVEFF_PREV,                             &
            BrcDSEEP, BrcDFLOW_IN, BrcDFLOW_RT
      END TYPE

END MODULE

MODULE SWO_TREE_BUILDER!, ONLY: MAP_UPTREE, MAP_DNTREE
  USE SWO_SURFACE_WATER_DATA_TYPES, ONLY: SWOPS_SEGTREE, SEGINFODAT, SWOPS_SEGLIST
  USE ALLOC_INTERFACE,             ONLY: ALLOC
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: MAP_UPTREE, MAP_DNTREE
  CONTAINS
  SUBROUTINE MAP_UPTREE(UPTREE,NSEGTYPE,UNIT_OPTION,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_UPTREE
!     ******************************************************************
!     OVERVIEW:
!     Return list of *all* upstream segments for each stream segment and
!     segment type ...
!
!     For example, return list of all connected river segments upstream
!     of a specified segment
!
!     NOTES:
!     --> At least one immediate upstream segment must be of the type
!         being mapped ... e.g., if you are mapping river segments upstream
!         of a specified conveyance segment, then at least one segment
!         immediately upstream of the specified conveyance segment must
!         be a river segment (i.e., one of UPSEG(ISEG)%SEGLIST must be a
!         river segment)
!
!     --> Argument "NSEGTYPE" is integer indicator for segment type to be
!         mapped ...
!         ** 1 = Natural channel
!         ** 2 = Conveyance channel (canal, lateral, ditch w/o water removed for delivery)
!         ** 3 = Delivery channel   (delivery segment ... used here as dummy seg from canal to farm gate)
!         ** 4 = Return channel     (drain or wasteway segment)
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    LOGICAL,                                     INTENT(IN   ):: UNIT_OPTION
    INTEGER,                                     INTENT(IN   ):: NSEGTYPE
    TYPE(SWOPS_SEGTREE), DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: UPTREE
    TYPE(SEGINFODAT),    DIMENSION(:),CONTIGUOUS,INTENT(IN):: SEGINFO
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(IN):: UPSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER :: NSEG,SEGCOUNT,COUNTER
!   ------------------------------------------------------------------
!
    ! Loop over segments...
    ! (1) count segments in upstream tree
    ! (2) allocate UPTREE(NSS)%SEGTREE
    ! (3) fill in tree
    DO NSEG = 1,NSS

        ! assign NSEG
        UPTREE(NSEG)%NSEG = NSEG

        ! deallocate UPTREE(NSEG)%SEGTREE if already allocated
        !IF (ALLOCATED(UPTREE(NSEG)%SEGTREE)) THEN
        !    DEALLOCATE(UPTREE(NSEG)%SEGTREE)
        !END IF
        !
        ! initialize/reset counters
        SEGCOUNT  = 0
        COUNTER   = 0
        !
        ! count segments...
        CALL MAP_UPCOUNT(UPTREE(NSEG),NSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
        !
        ! allocate segment tree to dimension SEGCOUNT...
        UPTREE(NSEG)%NTREE = SEGCOUNT
        CALL ALLOC( UPTREE(NSEG)%SEGTREE,SEGCOUNT )

        ! fill segment tree...
        CALL MAP_UPSTEP(UPTREE(NSEG),NSEG,NSEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
        !
    END DO !(NSEG  1,NSS)
    !
  END SUBROUTINE MAP_UPTREE
  !
  RECURSIVE SUBROUTINE MAP_UPCOUNT(UPTREE,NSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_UPCOUNT
!     ******************************************************************
!     OVERVIEW:
!     Recursive function to **count** segments for MAP_UPTREE
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    LOGICAL, INTENT(IN)                :: UNIT_OPTION
    INTEGER, INTENT(IN)                :: NSEG,NSEGTYPE
    INTEGER, INTENT(INOUT)             :: SEGCOUNT
    TYPE(SWOPS_SEGTREE), INTENT(INOUT) :: UPTREE
    TYPE(SEGINFODAT),    DIMENSION(:),CONTIGUOUS,INTENT(IN):: SEGINFO
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(IN):: UPSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER :: JSEG,JTYPE,I,JUNIT,IUNIT
!   ------------------------------------------------------------------
!
    ! recursive loop to count segments in tree
    DO I = 1,UPSEG(NSEG)%NLIST

        JSEG  = UPSEG(NSEG)%SEGLIST(I)
        JTYPE = SEGINFO(JSEG)%SegType

        IF (UNIT_OPTION) THEN
            IUNIT = SEGINFO(NSEG)%UnitID
            JUNIT = SEGINFO(JSEG)%UnitID
        ELSE
            IUNIT = 1
            JUNIT = 1
        END IF !(UNIT_OPTION)

        IF (NSEGTYPE.LT.5) THEN                                       ! Single type ...
            IF (JTYPE.EQ.NSEGTYPE) THEN
                IF (IUNIT.EQ.JUNIT) THEN
                    SEGCOUNT = SEGCOUNT + 1
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_UPCOUNT(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
                    END IF
                    ! CALL MAP_UPCOUNT(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,NSS,IDIVAR,IOTSG)
                END IF !(IUNIT=JUNIT)
            END IF !(JTYPE)

        ELSE IF (NSEGTYPE.EQ.5) THEN                                  ! NSEGTYPE=5 --> conveyance *or* delivery!
            IF ((JTYPE.EQ.2).OR.(JTYPE.EQ.3)) THEN
                IF (IUNIT.EQ.JUNIT) THEN
                    SEGCOUNT = SEGCOUNT + 1
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_UPCOUNT(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
                    END IF
                    ! CALL MAP_UPCOUNT(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,NSS,IDIVAR,IOTSG)
                END IF !(IUNIT=JUNIT)
            END IF !(JTYPE)

        END IF !(NSEGTYPE LT/EQ 5)

    END DO !(I = 1,UPSEG(NSEG)%NLIST)
    !
  END SUBROUTINE MAP_UPCOUNT
  !
  RECURSIVE SUBROUTINE MAP_UPSTEP(UPTREE,NSEG,NSEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_UPSTEP
!     ******************************************************************
!     OVERVIEW:
!     Recursive function to populate MAP_UPTREE
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    LOGICAL, INTENT(IN)                :: UNIT_OPTION
    INTEGER, INTENT(IN)                :: NSEG,NSEGTYPE
    INTEGER, INTENT(INOUT)             :: COUNTER
    TYPE(SWOPS_SEGTREE), INTENT(INOUT) :: UPTREE
    TYPE(SEGINFODAT),    DIMENSION(:),CONTIGUOUS,INTENT(IN):: SEGINFO
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(IN):: UPSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER :: JSEG,JTYPE,I,JUNIT,IUNIT
!   ------------------------------------------------------------------
!
    ! recursive loop to fill segments in tree
    DO I = 1,UPSEG(NSEG)%NLIST

        JSEG  = UPSEG(NSEG)%SEGLIST(I)
        JTYPE = SEGINFO(JSEG)%SegType

        IF (UNIT_OPTION) THEN
            IUNIT = SEGINFO(NSEG)%UnitID
            JUNIT = SEGINFO(JSEG)%UnitID
        ELSE
            IUNIT = 1
            JUNIT = 1
        END IF !(UNIT_OPTION)

        IF (NSEGTYPE.LT.5) THEN                                       ! Single type ...
            IF (JTYPE.EQ.NSEGTYPE) THEN
                IF (JUNIT.EQ.IUNIT) THEN
                    COUNTER = COUNTER + 1
                    UPTREE%SEGTREE(COUNTER) = JSEG
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_UPSTEP(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
                    END IF
                    ! CALL MAP_UPSTEP(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,COUNTER,NSS,IDIVAR,IOTSG)
                END IF !(JUNIT=IUNIT)
            END IF !(JTYPE)

        ELSE IF (NSEGTYPE.EQ.5) THEN                                  ! NSEGTYPE=5 --> conveyance *or* delivery!
            IF ((JTYPE.EQ.2) .OR. (JTYPE.EQ.3)) THEN
                IF (JUNIT.EQ.IUNIT) THEN
                    COUNTER = COUNTER + 1
                    UPTREE%SEGTREE(COUNTER) = JSEG
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_UPSTEP(UPTREE,JSEG,NSEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,UPSEG,NSS,IDIVAR,IOTSG)
                    END IF
                    ! CALL MAP_UPSTEP(UPTREE,JSEG,NSEGTYPE, UNIT_OPTION,COUNTER,NSS,IDIVAR,IOTSG)
                END IF !(JUNIT=IUNIT)
            END IF !(JTYPE)

        END IF !(NSEGTYPE LT/EQ 5)

    END DO !(I = 1,UPSEG(NSEG)%NLIST)
    !
  END SUBROUTINE MAP_UPSTEP
  !
  SUBROUTINE MAP_DNTREE(DNTREE,NSEGTYPE,UNIT_OPTION,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_DNTREE
!     ******************************************************************
!     OVERVIEW:
!     Return list of *all* downstream segments for each stream segment and
!     segment type ...
!
!     For example, return list of all connected river segments downstream
!     of a specified segment
!
!     NOTES:
!     --> At least one immediate downstream segment must be of the type
!         being mapped ... e.g., if you are mapping river segments downstream
!         of a specified conveyance segment, then at least one segment
!         immediately downstream of the specified conveyance segment must
!         be a river segment (i.e., one of DNSEG(ISEG)%SEGLIST must be a
!         river segment)
!
!     --> Argument "NSEGTYPE" is integer indicator for segment type to be
!         mapped ...
!         ** 1 = Natural channel
!         ** 2 = Conveyance channel (canal, lateral, ditch w/o water removed for delivery)
!         ** 3 = Delivery channel   (delivery segment ... used here as dummy seg from canal to farm gate)
!         ** 4 = Return channel     (drain or wasteway segment)
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    LOGICAL, INTENT(IN)               :: UNIT_OPTION
    INTEGER, INTENT(IN)               :: NSEGTYPE
    TYPE(SWOPS_SEGTREE), DIMENSION(:) :: DNTREE
    TYPE(SEGINFODAT),    DIMENSION(:),CONTIGUOUS,INTENT(IN):: SEGINFO
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(IN):: DNSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER :: NSEG,SEGCOUNT,COUNTER
!   ------------------------------------------------------------------
!
    ! Loop over segments...
    ! (1) count segments in downstream tree
    ! (2) allocate DNTREE(NSS)%SEGTREE
    ! (3) fill in tree
    DO NSEG = 1,NSS

        ! assign NSEG
        DNTREE(NSEG)%NSEG = NSEG

        ! deallocate DNTREE(NSEG)%SEGTREE if already allocated
        !IF (ALLOCATED(DNTREE(NSEG)%SEGTREE)) THEN
        !    DEALLOCATE(DNTREE(NSEG)%SEGTREE)
        !END IF

        ! initialize/reset counters
        SEGCOUNT  = 0
        COUNTER   = 0

        ! count segments...
        CALL MAP_DNCOUNT(DNTREE(NSEG),NSEG,NSEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)

        ! allocate segment tree to dimension SEGCOUNT...
        DNTREE(NSEG)%NTREE = SEGCOUNT
        CALL ALLOC( DNTREE(NSEG)%SEGTREE, SEGCOUNT )

        ! fill segment tree...
        CALL MAP_DNSTEP(DNTREE(NSEG),NSEG,NSEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
    END DO !(NSEG  1,NSS)
    !
  END SUBROUTINE MAP_DNTREE
  !
  RECURSIVE SUBROUTINE MAP_DNCOUNT(DNTREE,ISEG,ISEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_DNCOUNT
!     ******************************************************************
!     OVERVIEW:
!     Recursive function to **count** segments for MAP_DNTREE
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    LOGICAL, INTENT(IN)                :: UNIT_OPTION
    INTEGER, INTENT(IN)                :: ISEG,ISEGTYPE
    INTEGER, INTENT(INOUT)             :: SEGCOUNT
    TYPE(SWOPS_SEGTREE), INTENT(INOUT) :: DNTREE
    TYPE(SEGINFODAT),    DIMENSION(:),CONTIGUOUS,INTENT(IN):: SEGINFO
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(IN):: DNSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER :: JSEG,JTYPE,I,IUNIT,JUNIT
!   ------------------------------------------------------------------
!
    ! TODO --
    ! Added if/then to stop MAP_DNCOUNT / MAP_DNSTP at delivery segments...
    ! This is needed so that delivery segments appear as "dead ends" in
    ! segment tree, which in turn is required for accumulation routines
    ! (e.g., summing TFDR and DELORDER over divsegs, splits, and branches)
    ! This should be revisited to see if something more elegant can be done...
    IF (SEGINFO(ISEG)%SegType.NE.3) THEN
      ! recursive loop to count segments in tree
      DO I = 1,DNSEG(ISEG)%NLIST

        JSEG  = DNSEG(ISEG)%SEGLIST(I)
        JTYPE = SEGINFO(JSEG)%SegType

        IF (UNIT_OPTION) THEN
            IUNIT = SEGINFO(ISEG)%UnitID
            JUNIT = SEGINFO(JSEG)%UnitID
        ELSE
            IUNIT = 1
            JUNIT = 1
        END IF !(UNIT_OPTION)

        IF (ISEGTYPE.LT.5) THEN                                       ! Single type ...
            IF (JTYPE.EQ.ISEGTYPE) THEN
                IF (IUNIT.EQ.JUNIT) THEN
                    SEGCOUNT = SEGCOUNT + 1
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_DNCOUNT(DNTREE,JSEG,ISEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
                    END IF

                END IF !(IUNIT=JUNIT)
            END IF !(JTYPE)

        ELSE IF (ISEGTYPE.EQ.5) THEN                                  ! ISEGTYPE=5 --> conveyance *or* delivery!
            IF ((JTYPE.EQ.2) .OR. (JTYPE.EQ.3)) THEN
                IF (IUNIT.EQ.JUNIT) THEN
                    SEGCOUNT = SEGCOUNT + 1
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_DNCOUNT(DNTREE,JSEG,ISEGTYPE,UNIT_OPTION,SEGCOUNT,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
                    END IF

                END IF !(IUNIT=JUNIT)
            END IF !(JTYPE)

        END IF !(ISEGTYPE LT/EQ 5)

      END DO !(I = 1,DNSEG(ISEG)%NLIST)

    END IF !(SEGTYPE.EQ.3)                                            ! Klugey force @ delivery seg dead-end
    !
  END SUBROUTINE MAP_DNCOUNT
  !
  RECURSIVE SUBROUTINE MAP_DNSTEP(DNTREE,ISEG,ISEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_DNSTEP
!     ******************************************************************
!     OVERVIEW:
!     Recursive function to populate MAP_DNTREE
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    LOGICAL, INTENT(IN)                :: UNIT_OPTION
    INTEGER, INTENT(IN)                :: ISEG,ISEGTYPE
    INTEGER, INTENT(INOUT)             :: COUNTER
    TYPE(SWOPS_SEGTREE), INTENT(INOUT) :: DNTREE
    TYPE(SEGINFODAT),    DIMENSION(:),CONTIGUOUS,INTENT(IN):: SEGINFO
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(IN):: DNSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER :: JSEG,JTYPE,I,IUNIT,JUNIT
!   ------------------------------------------------------------------
!
    ! TODO --
    ! Added if/then to stop MAP_DNCOUNT / MAP_DNSTP at delivery segments...
    ! This is needed so that delivery segments appear as "dead ends" in
    ! segment tree, which in turn is required for accumulation routines
    ! (e.g., summing TFDR and DELORDER over divsegs, splits, and branches)
    ! This should be revisited to see if something more elegant can be done...
    IF (SEGINFO(ISEG)%SegType.NE.3) THEN

      ! recursive loop to fill segments in tree
      DO I = 1,DNSEG(ISEG)%NLIST

        JSEG  = DNSEG(ISEG)%SEGLIST(I)
        JTYPE = SEGINFO(JSEG)%SegType

        IF (UNIT_OPTION) THEN
            IUNIT = SEGINFO(ISEG)%UnitID
            JUNIT = SEGINFO(JSEG)%UnitID
        ELSE
            IUNIT = 1
            JUNIT = 1
        END IF !(UNIT_OPTION)

        IF (ISEGTYPE.LT.5) THEN                                       ! Single type ...
            IF (JTYPE.EQ.ISEGTYPE) THEN
                IF (IUNIT.EQ.JUNIT) THEN

                    COUNTER = COUNTER + 1
                    DNTREE%SEGTREE(COUNTER) = JSEG
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_DNSTEP(DNTREE,JSEG,ISEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
                    END IF
                    ! CALL MAP_DNSTEP(DNTREE,JSEG,ISEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)

                END IF !(IUNIT.EQ.JUNIT)
            END IF !(JTYPE)

        ELSE IF (ISEGTYPE.EQ.5) THEN                                  ! ISEGTYPE=5 --> conveyance *or* delivery!
            IF ((JTYPE.EQ.2) .OR. (JTYPE.EQ.3)) THEN
                IF (IUNIT.EQ.JUNIT) THEN

                    COUNTER = COUNTER + 1
                    DNTREE%SEGTREE(COUNTER) = JSEG
                    IF (JTYPE.NE.3) THEN                              ! Stop @ delivery segments!!
                        CALL MAP_DNSTEP(DNTREE,JSEG,ISEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)
                    END IF
                    ! CALL MAP_DNSTEP(DNTREE,JSEG,ISEGTYPE,UNIT_OPTION,COUNTER,SEGINFO,DNSEG,NSS,IDIVAR,IOTSG)

                END IF !(IUNIT.EQ.JUNIT)
            END IF !(JTYPE)

        END IF !(ISEGTYPE LT/EQ 5)

      END DO !(I = 1,DNSEG(ISEG)%NLIST)

    END IF !(SEGTYPE.EQ.3)                                            ! Klugey force @ delivery seg dead-end

    
  END SUBROUTINE MAP_DNSTEP
  !
END MODULE
!
!######################################################################################################################
!######################################################################################################################
!######################################################################################################################
!
MODULE SWO_DATA_FMP_MODULE!, ONLY: SWO_DATA, INITIALIZE_SWO_DATA, FRAC_POOL2SPLIT, ACAP_STOR2AREA, ACAP_STOR2ELEV
  USE, INTRINSIC:: IEEE_ARITHMETIC,      ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  !
  USE CONSTANTS
  USE GLOBAL,                            ONLY: SPTIM, NPER, NSTP
  USE GWFBASMODULE,                      ONLY: HAS_STARTDATE, DATE_SP, SIMTIME, REALTIM, REALTIM_PER
  USE SWO_HIERARCHY_DATA_TYPES
  USE SWO_SURFACE_WATER_DATA_TYPES
  USE SWO_TREE_BUILDER,                  ONLY: MAP_UPTREE, MAP_DNTREE
  USE BAS_UTIL,                          ONLY: DECIMAL_YEAR
  USE PARSE_WORD_INTERFACE,              ONLY: PARSE_WORD, PARSE_WORD_UP
  USE POSITION_INTERFACE,                ONLY: STR_POS, INT_POS
  USE STRINGS,                           ONLY: UPPER, IS_IN_STR, GET_INTEGER, GET_NUMBER, GET_WORD, GET_DOUBLE_DATE
  USE FILE_IO_INTERFACE,                 ONLY: READ_TO_DATA
  USE IS_ROUTINES,                       ONLY: IS_CLOSE
  USE UTIL_INTERFACE,                    ONLY: ZERO_OR_GREATER, ZERO_OR_LESS, MAKE_ZERO_IF_NEG,    &
                                               VEC_ADJUST_MAXSUM, REDUCE_SUM_BY, NOT_NEAR_ZERO, NEAR_ZERO
  USE ERROR_INTERFACE,                   ONLY: STOP_ERROR, FILE_IO_ERROR, WARNING_MESSAGE
  USE ALLOC_INTERFACE,                   ONLY: ALLOC
  USE NUM2STR_INTERFACE,                 ONLY: NUM2STR
  USE RELAX_INTERFACE,                   ONLY: RELAXER, DAMP_IT, RELAX_IT
  USE WARNING_TYPE_INSTRUCTION,          ONLY: WARNING_TYPE
  USE LINKED_LIST_INSTRUCTION,           ONLY: INTEGER_LINKED_LIST, CHARACTER_LINKED_LIST
  USE VARIABLE_POINTER_LIST_INTERFACE,   ONLY: VAR_POINTER_LIST
  USE ARRAY_DATA_TYPES,                  ONLY: CHARACTER_TYPE, DOUBLE_MATRIX, DOUBLE_VECTOR, INTEGER_VECTOR, LOGICAL_VECTOR
  USE CALENDAR_FUNCTIONS,                ONLY: ISLEAPYEAR, YEAR_FRACTION
  USE DATE_OPERATOR_INSTRUCTION,         ONLY: DATE_OPERATOR
  USE GENERIC_BLOCK_READER_INSTRUCTION,  ONLY: GENERIC_BLOCK_READER
  USE SUB_BLOCK_INPUT_INTERFACE,         ONLY: SUB_BLOCK_INPUT
  USE ULOAD_AND_SFAC_INTERFACE,          ONLY: ULOAD, SFAC_DATA
  USE LIST_ARRAY_INPUT_INTERFACE,        ONLY: LIST_ARRAY_INPUT, LIST_ARRAY_INPUT_INT
  USE GENERIC_INPUT_FILE_INSTRUCTION,    ONLY: GENERIC_INPUT_FILE
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,   ONLY: GENERIC_OUTPUT_FILE
  USE SFR_INPUT_DATA_TYPES,              ONLY: SFR_SEGRCH_TFR, SFR_SEG_TFR, SFR_NAMED_LOCATION
  USE FMP_DIMENSION_MODULE,           ONLY: FMP_DIMENSION
  USE WBS_DATA_FMP_MODULE,            ONLY: WBS_DATA
  USE SURFACE_WATER_DATA_FMP_MODULE,  ONLY: SURFACE_WATER_DATA, SRD_LOC
  USE TIME_SERIES_INSTRUCTION,           ONLY: TIME_SERIES_FILE
  USE S_LANGUAGE_INTERFACE,              ONLY: S_INTERPRETER, S_VARIABLE_LIST, VARIABLE_NAME_MEANING
  USE SORT_INTERFACE,                    ONLY: SORT
  !USE TABLEFILE_INTERFACE, ONLY:  TABFILETYPE1IDX,TABFILEPARSE,TABFILELINKS,  &
  !                                TABFILEPACKINDEX,TABFILEUPDATE,TABFILE_DEALLOCATE
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: SWO_DATA, INITIALIZE_SWO_DATA, FRAC_POOL2SPLIT, ACAP_STOR2AREA, ACAP_STOR2ELEV, ACAP_ELEV2STOR, SPLIT_RES_FRAC
  !
  INTERFACE
    PURE SUBROUTINE VARIABLE_GET_GLOBAL_MODEL_PROPERTY(VAR, NAM, KPER, KSTP, KITER, LVL, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
      USE S_LANGUAGE_INTERFACE, ONLY: S_VARIABLE_LIST, VARIABLE_NAME_MEANING
      CLASS(S_VARIABLE_LIST),                                 INTENT(INOUT):: VAR
      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
      INTEGER,                                                INTENT(IN   ):: KPER, KSTP, KITER, LVL !LVL = 0 => SP/TS, LVL = 1 => FM, LVL = 2 => CLOSEOUT
      CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
    END SUBROUTINE
    !
    PURE SUBROUTINE VARIABLE_SET_RETURN_VALUES(VAR, NAM, KPER, KSTP, ERROR)
      USE S_LANGUAGE_INTERFACE, ONLY: S_VARIABLE_LIST, VARIABLE_NAME_MEANING
      CLASS(S_VARIABLE_LIST),                               INTENT(INOUT):: VAR
      TYPE(VARIABLE_NAME_MEANING),  DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NAM
      INTEGER,                                                INTENT(IN   ):: KPER, KSTP
      CHARACTER(:),ALLOCATABLE,                               INTENT(INOUT):: ERROR
    END SUBROUTINE
  END INTERFACE
  !
  TYPE REQUIRED_FLOW
      LOGICAL:: HAS_REQ = FALSE
      INTEGER:: N = Z
      TYPE(CHARACTER_TYPE), DIMENSION(:), ALLOCATABLE:: NAM    !VARIABLE NAME
      INTEGER,              DIMENSION(:), ALLOCATABLE:: POS    !VARIABLE POSITION IN RETURN VARIABLE LIST
      DOUBLE PRECISION,     DIMENSION(:), ALLOCATABLE:: REQ    !REQUIRED FLOW RATE AT ISTRM
      INTEGER,              DIMENSION(:), ALLOCATABLE:: SEG    !REQUIRED SEGMENT AND REACH
      INTEGER,              DIMENSION(:), ALLOCATABLE:: RCH    !REQUIRED SEGMENT AND REACH
      INTEGER,              DIMENSION(:), ALLOCATABLE:: ISTRM  !SFR POINT THAT HAS A REQUIRED FLOW
      INTEGER,              DIMENSION(:), ALLOCATABLE:: HEDSEG !COMMON SEGEMENT FOR MULTIPLE RESERVOIRS
      TYPE(INTEGER_VECTOR), DIMENSION(:), ALLOCATABLE:: ORDER  !DOWNSTREAM ORDER OF REQURIED FLOWS, COULD HAVE MULTIPLE PATH WAY
      DOUBLE PRECISION,     DIMENSION(:), ALLOCATABLE:: REL    !WATER THAT SHOULD BE RELEASED TO MEET AND ORDER PATHAY
      DOUBLE PRECISION,     DIMENSION(:), ALLOCATABLE:: REL_OLD!WATER THAT SHOULD BE RELEASED TO MEET AND ORDER PATHAY FROM PREVIOUS ITERATION
      TYPE(INTEGER_VECTOR), DIMENSION(:), ALLOCATABLE:: RES    !RESERVOIRS THAT ARE CONNECTED TO REQUIRED FLOW
  END TYPE
  !
  TYPE SFR_SPEC_DIV
      INTEGER:: N = Z
      INTEGER,              DIMENSION(:), ALLOCATABLE:: POS    !VARIABLE POSITION IN RETURN VARIABLE LIST
      INTEGER,              DIMENSION(:), ALLOCATABLE:: SEG    !Diverison SEGMENT
      DOUBLE PRECISION,     DIMENSION(:), ALLOCATABLE:: DIV    !Specified Diversion for SEGREQUIRED FLOW RATE AT ISTRM
  END TYPE
  !
  TYPE RESERVOIR_TRANSFER
      LOGICAL:: HAS_TRAN = FALSE
      INTEGER:: N = Z
      TYPE(CHARACTER_TYPE), DIMENSION(:),   ALLOCATABLE:: NAM
      DOUBLE PRECISION,     DIMENSION(:),   ALLOCATABLE:: RAT
      INTEGER,              DIMENSION(:),   ALLOCATABLE:: POS
      INTEGER,              DIMENSION(:,:), ALLOCATABLE:: RES
  END TYPE
  !
  TYPE RESERVOIR_RELEASE_FRACTION
      LOGICAL:: HAS_FRAC = FALSE
      INTEGER:: N = Z
      LOGICAL,              DIMENSION(:), ALLOCATABLE:: HAS_S
      INTEGER,              DIMENSION(:), ALLOCATABLE:: BACK_CALC
      !TYPE(DOUBLE_VECTOR ), DIMENSION(:), ALLOCATABLE:: FRAC
      TYPE(INTEGER_VECTOR), DIMENSION(:), ALLOCATABLE:: POS
  END TYPE
  !
  TYPE SFR_OLD_FLOW
      DOUBLE PRECISION:: FLO
      INTEGER         :: PROP_PUL_POS
      INTEGER         :: ISTRM
      LOGICAL:: IS_INFLOW
  END TYPE
  !
  TYPE SWO_MAIN_OUTPUT_FILES
      LOGICAL:: IT_FARM=FALSE, IT_AUXDEM=FALSE, IT_UNIT=FALSE, IT_DIST=FALSE, IT_PROJ=FALSE, IT_DIVSEG=FALSE, IT_SPLITSEG=FALSE, IT_STORAGE=FALSE, IT_SFR=FALSE, IT_CONVERGENCE=FALSE
      TYPE(GENERIC_OUTPUT_FILE):: FARM
      TYPE(GENERIC_OUTPUT_FILE):: AUXDEM
      TYPE(GENERIC_OUTPUT_FILE):: UNIT
      TYPE(GENERIC_OUTPUT_FILE):: DIST
      TYPE(GENERIC_OUTPUT_FILE):: PROJ
      TYPE(GENERIC_OUTPUT_FILE):: DivSeg
      TYPE(GENERIC_OUTPUT_FILE):: SplitSeg
      TYPE(GENERIC_OUTPUT_FILE):: Storage
      TYPE(GENERIC_OUTPUT_FILE):: SFR
      !TYPE(GENERIC_OUTPUT_FILE):: ALLOCATION
      TYPE(GENERIC_OUTPUT_FILE):: Convergence
  END TYPE
  !
  TYPE SWO_DATA
      LOGICAL:: HAS_SWO       = FALSE
      LOGICAL:: LEAP_YEAR     = TRUE
      LOGICAL:: IS_LEAP_START = FALSE
      LOGICAL:: IS_LEAP_END     = FALSE
      LOGICAL:: IS_LEAP_END_NEXT= FALSE
      INTEGER:: INNER_ITERATION = ONE
      INTEGER:: BEGIN_SP         = INF_I
      INTEGER:: IOUT=Z, LOUT=Z
      INTEGER:: NROW, NCOL, ITMUNI
      INTEGER:: NFARM, NSEG, NSTRM, NAUXDEM, NPROJ, NDIST, NUNIT
      LOGICAL:: TFR_READ   = FALSE
      LOGICAL:: HAS_OUTPUT = FALSE
      !
      INTEGER:: NRES_TOTTOT, NRES_BALTOT, NRES_SPTTOT                                         ! counters for reservoirs (total, mass bal calcs, split calcs)
      !
      INTEGER:: DIVCOUNT, SPTCOUNT                                                            ! number of unique diversion segments and splits on model
      !
      INTEGER,          DIMENSION(:),  ALLOCATABLE:: IRESFL
      INTEGER,          DIMENSION(:),  ALLOCATABLE:: NRES_PRJ, NRES_BAL, NRES_SPT, NRES_TOT    ! reservoir counter arrays
      INTEGER,          DIMENSION(:,:),ALLOCATABLE:: RESLINK             !ARRAY THAT HAS COMMON RESERVOIR SEGMENTS. ZERO IF NO COMMON POINT
      !
      DOUBLE PRECISION::                            NaN                  ! Holds Not A Number (NaN)
      !
      DOUBLE PRECISION::                            RELEASE_RELAX        ! RELAXATION FACTOR ON DEMANDED RELEASES
      DOUBLE PRECISION::                            REQFLOW_RELAX        ! RELAXATION FACTOR ON Over-Estimate of Required Flow. ReqRel_New = ReqRel_Old - REQFLOW_RELAX*OverRelease
      DOUBLE PRECISION::                            DELT, DELT2          ! HOLDS CURRENT TIME STEP LENGTH AND NEXT TIME STEP'S LENGTH
      DOUBLE PRECISION::                            DELT_INV             ! HOLDS 1/DELT or 0
      INTEGER,          DIMENSION(:), ALLOCATABLE:: SWMXITER             ! weighting factor with orders / without orders
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: REL_CNVG             ! convergence criteria @ flows / flow vs. order / REQ FLOW
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: ABS_CNVG             ! convergence criteria @ flows / flow vs. order / REQ FLOW
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: WTFACTOR
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: DIST_DELTOL          ! FRACTION OF DELIVERY/DEL_ORDER THAT SENDS DEL_ORDER TO 0 IF RATIO GOES BELOW THAT
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: MIN_DELORDER         ! IF DELIVERY REQUEST IS LESS THEN THIS AMMOUNT IT IS SENT TO ZERO, TOO SMALL TO BE WORTH THE RELEASE
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: MIN_PROJ_ORDER
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: MIN_DELORDER_RAT     ! IF DELIVERY REQUEST IS LESS THEN THIS AMMOUNT IT IS SENT TO ZERO, TOO SMALL TO BE WORTH THE RELEASE -- DEFINED AS RATE AND APPLIED TO VOL COUNTER PARTS
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: MIN_PROJ_ORDER_RAT
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: FMP_SW_DEMAND        ! (NFARM) MUST BE SET BEFORE CALLING COMPUTE_ALLOCATION_AND_RELEASE (AKA TFDR)
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: FMP_SW_ALLOTMENT     ! (NFARM) SET TO ANY FMP SURFACE WATER LIMITS
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: FMP_SW_LIMIT         ! (NFARM) IS AN ADDITIONAL LIMIT IMPOSED ON FMP
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: FMP_SW_LIMIT_RULZ    ! (NFARM) IS AN ADDITIONAL LIMIT IMPOSED ON FMP
      LOGICAL                                    :: HAS_FMP_SW_LIMIT_RULZ! IS TRUE IF THERE IS VARIABLE THAT DEFINES A SW LIMIT
      LOGICAL                                    :: HAS_REQ_DELIVERY     ! IS TRUE IF THERE IS VARIABLE THAT DEFINES A SW LIMIT
      LOGICAL                                    :: HAS_ELEV_CHNG_S
      LOGICAL                                    :: HAS_ELEV_CHNG
      !LOGICAL                                    :: HAS_RELEASE_FRAC_S
      LOGICAL                                    :: HAS_MIN_RELEASE_S
      LOGICAL                                    :: HAS_ADD_RELEASE_S
      LOGICAL                                    :: HAS_MAX_RELEASE_S
      LOGICAL                                    :: HAS_MAX_STORAGE_S
      LOGICAL                                    :: HAS_MIN_STORAGE_S
      LOGICAL                                    :: HAS_MIN_STORAGE_TRAN_S
      LOGICAL                                    :: HAS_RELEASE_SPEC_S
      LOGICAL                                    :: HAS_SPILL_S
      LOGICAL                                    :: HAS_MAX_STAGE_S
      LOGICAL                                    :: HAS_MAX_SPILL
      LOGICAL                                    :: HAS_PROJ_RELEASE_ADD
      LOGICAL                                    :: HAS_MAX_SPILL_S
      !
      CHARACTER(:), ALLOCATABLE:: RES_CONV_ERROR
      !
      TYPE(CHARACTER_TYPE), DIMENSION(:), ALLOCATABLE:: RESNAM, SPLITNAM
      !
      TYPE(FARMDAT), DIMENSION(:), ALLOCATABLE:: FARM,   FARM_PREV
      TYPE(UNITDAT), DIMENSION(:), ALLOCATABLE:: UNIT,   UNIT_PREV
      TYPE(DISTDAT), DIMENSION(:), ALLOCATABLE:: DIST,   DIST_PREV
      TYPE(PROJDAT), DIMENSION(:), ALLOCATABLE:: PROJ,   PROJ_PREV
      TYPE(AUXDAT),  DIMENSION(:), ALLOCATABLE:: AUXDEM, AUXDEM_PREV
      !
      !TYPE(SWOPS_RESBAL),  DIMENSION(:),ALLOCATABLE:: RESBAL
      !TYPE(SWOPS_RESSPLIT),DIMENSION(:),ALLOCATABLE:: RESSPLIT
      !TYPE(SWOPS_ACAPTBL), DIMENSION(:),ALLOCATABLE:: ACAP
      !TYPE(SWOPS_FRACTBL), DIMENSION(:),ALLOCATABLE:: FRAC
      !TYPE(SWOPS_RESHEAD), DIMENSION(:),ALLOCATABLE:: HEAD
      !
      TYPE(SWOPS_RESTYPE),DIMENSION(:),ALLOCATABLE:: RESDAT
      !
      TYPE(SEGDAT),DIMENSION(:),ALLOCATABLE:: SEGDATA
      TYPE(SEGDAT),DIMENSION(:),ALLOCATABLE:: SEGDATA_PREV
      TYPE(SEGDAT),DIMENSION(:),ALLOCATABLE:: SEGDATA_SAVE      ! input values (from SFR input)
      TYPE(SEGDAT),DIMENSION(:),ALLOCATABLE:: FLOWDATA
      TYPE(SEGDAT),DIMENSION(:),ALLOCATABLE:: FLOWDATA_PREV
      TYPE(SEGDAT),DIMENSION(:),ALLOCATABLE:: FLOWDATA_SAVE     ! input values (from SFR input)
      !
      TYPE(RCHDAT),DIMENSION(:),ALLOCATABLE:: RCHDATA
      TYPE(RCHDAT),DIMENSION(:),ALLOCATABLE:: RCHDATA_PREV
      TYPE(RCHDAT),DIMENSION(:),ALLOCATABLE:: RCHDATA_SAVE      ! input values (from SFR input)
      !
      TYPE(SWOPS_SEGLIST),DIMENSION(:),ALLOCATABLE:: UPSEG
      TYPE(SWOPS_SEGLIST),DIMENSION(:),ALLOCATABLE:: DNSEG
      !
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: UPTREE_NAT         ! uptree @ natural segments
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: UPTREE_CON1        ! uptree @ conveyance segments (SEGTYPE=2)
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: UPTREE_CON2        ! uptree @ conveyance+delivery segments (SEGTYPE=2.OR.3)
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: UPTREE_RET         ! uptree @ return segments
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: DNTREE_NAT         ! dntree @ natural segments
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: DNTREE_CON1        ! dntree @ conveyance segments (SEGTYPE=2)
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: DNTREE_CON2        ! dntree @ conveyance+delivery segments (SEGTYPE=2.OR.3)
      TYPE(SWOPS_SEGTREE),DIMENSION(:),ALLOCATABLE:: DNTREE_RET         ! dntree @ return segments
      !
      TYPE(SEGINFODAT),DIMENSION(:),ALLOCATABLE:: SEGINFO
      TYPE(DIVSEGDAT), DIMENSION(:),ALLOCATABLE:: DIVSEG
      TYPE(DIVSEGDAT), DIMENSION(:),ALLOCATABLE:: DIVSEG_PREV
      TYPE(SPTSEGDAT),DIMENSION(:), ALLOCATABLE:: SPTSEG
      TYPE(SPTSEGDAT),DIMENSION(:), ALLOCATABLE:: SPTSEG_PREV
      !
      INTEGER,          DIMENSION(:), ALLOCATABLE:: NSPLIT              ! number of splits within each unit (dimensioned at proj,dist,unit)
      !
      ! DIMENSIONAL VARIABLES
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: SEGRCH                               ! mapping between segments (NSS) and reaches (NSTRM)
      !INTEGER,DIMENSION(:,:),ALLOCATABLE:: FARMID,UNITID,DISTID,PROJID,CropID   ! unit, district, and project indicator arrays
      INTEGER,DIMENSION(:),ALLOCATABLE:: SEGRCH_IN, SEGRCH_OUT                  ! PNT FOR RCH START AND END FOR SEGMENT. COULD BE REPLACED BY SEGDATA(ISEG)%RchIDX
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: REL_CNVG_CHK                ! holds convergence criteria
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: ABS_CNVG_CHK                ! holds convergence criteria
      !
      ! MISC VARIABLES
      DOUBLE PRECISION:: FRSTART, FRSTOP, FRSTOP_NEXT
      DOUBLE PRECISION:: TS_START,TS_STOP, TS_STOP_NEXT
      DOUBLE PRECISION:: SP_START,SP_STOP
      INTEGER,DIMENSION(:,:), ALLOCATABLE:: RESBAL2PROJ                  !POINTER FOR A GIVEN RESERVOIR ID TO A PROJECT/RES INDEX USED IN RESBAL
      !
      ! DERIVED TYPE VARIABLES
      !
      TYPE(GENERIC_OUTPUT_FILE):: PRT_RUL_SIM,PRT_RUL_SP,PRT_RUL_TS,PRT_RUL_IT, PRT_RUL_CLOSEOUT, PRT_RUL_TS_END, PRT_RUL_IT_END
      TYPE(S_INTERPRETER)      :: DEC_RUL_SIM,DEC_RUL_SP,DEC_RUL_TS,DEC_RUL_IT, DEC_RUL_CLOSEOUT, DEC_RUL_TS_END, DEC_RUL_IT_END
      TYPE(S_VARIABLE_LIST)    :: DEC_VAR
      TYPE(SWO_MAIN_OUTPUT_FILES),             ALLOCATABLE:: OUTPUT
      TYPE(GENERIC_OUTPUT_FILE):: PRT_RESDAT, PRT_RESDAT_CMPCT, PRT_RESDAT_DETAIL
      DOUBLE PRECISION:: STOR_CNVT=UNO, FLOW_CNVT=UNO, LEN_CNVT=UNO, AREA_CNVT=UNO
      !
      !TYPE(GENERIC_BLOCK_READER), DIMENSION(:), ALLOCATABLE:: PROJ_ALLOC_RULES !EVENTUALY CHANGE TO S_INTERPRETER TYPE
      !TYPE(SUB_BLOCK_INPUT),      DIMENSION(:), ALLOCATABLE:: PROJ_ALLOC_TFR   !DIM = NPROJ
      !
      TYPE(UNITDAT_ACCOUNTING_LOADER):: UNIT_ACCOUNTING
      TYPE(AUX_SFR_LOADER)::            AUX_SFR_DELV
      !
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: AUXDEM_TSF
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: AUXAREA_TSF
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_INFLOW
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_PRECIP
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_EVAP
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_RELEASE_SPEC
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_STORAGE_MIN
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_RELEASE_MIN
      !
      TYPE(REQUIRED_FLOW):: REQFLOW
      TYPE(RESERVOIR_TRANSFER):: TRANS
      TYPE(SFR_SPEC_DIV):: S_SFR_SPEC_DIV
      TYPE(RESERVOIR_RELEASE_FRACTION):: S_RELEASE_FRAC
      LOGICAL:: S_REQ_RELEASE_FRAC = FALSE
      !
      INTEGER:: NSFR_OLD_FLO
      TYPE(SFR_OLD_FLOW),DIMENSION(:),ALLOCATABLE:: SFR_OLD_FLO
      !
      !!!LOGICAL:: AUXDEM_TSF_SIMTIM       = FALSE  --NOT ALLOWING FOR SIMTIME FOR TIME SERIES
      !!!LOGICAL:: AUXAREA_TSF_SIMTIM      = FALSE
      !!!!
      !!!LOGICAL:: RES_INFLOW_SIMTIM       = FALSE
      !!!LOGICAL:: RES_PRECIP_SIMTIM       = FALSE
      !!!LOGICAL:: RES_EVAP_SIMTIM         = FALSE
      !!!LOGICAL:: RES_RELEASE_SPEC_SIMTIM = FALSE
      !!!LOGICAL:: RES_STORAGE_SPEC_MIN_SIMTIM = FALSE
      !
      TYPE(SFR_NAMED_LOCATION  ):: SFR_ID
      TYPE(SFR_SEG_TFR         ):: RESBAL_RELSEG
      TYPE(SFR_SEG_TFR         ):: RESBAL_INFLOW_SEG
      TYPE(LIST_ARRAY_INPUT_INT):: SEG_TYP       !DIM-NSEG
      TYPE(LIST_ARRAY_INPUT    ):: DIST_ALLOC_FRAC
      TYPE(LIST_ARRAY_INPUT    ):: RELEASE_DMD_FRAC
      TYPE(LIST_ARRAY_INPUT    ):: RELEASE_REQ_FRAC
      TYPE(LIST_ARRAY_INPUT    ):: PRECIP_AREA_FRAC
      INTEGER:: DIST_ALLOC_FRAC_TYP
      LOGICAL:: REQ_FRAC_USES_DMD_FRAC = TRUE
      !
      !CUSTOM OUTPUT VARIABLE NAME LIST
      INTEGER:: NUM_PRNT_VAR
      CHARACTER(:),DIMENSION(:),ALLOCATABLE:: PRNT_VAR_NAM
      INTEGER,     DIMENSION(:),ALLOCATABLE:: PRNT_VAR_FLG
      LOGICAL:: HAS_BY_ITERATION
      !
      CONTAINS
      !
      PROCEDURE, PASS(SWO):: NEXT     => SETUP_NEXT_STRESS_PERIOD
      PROCEDURE, PASS(SWO):: ALLOC_N_INIT => ALLOCATE_INITIALIZE_ADDITIONAL_VARIABLES!(ISTRM)
      PROCEDURE, PASS(SWO):: NEXT_TIME_STEP => SETUP_NEXT_TIME_STEP!(WBS,KPER,KSTP,SEG)
      PROCEDURE, PASS(SWO):: COMPUTE_ALLOCATION_AND_RELEASE !(KPER,KSTP,KITER,DELT,TFDR,IDIVAR,STRM,SEG)  !FM ROUTINE
      PROCEDURE, PASS(SWO):: CONVERGENCE_CHECK!(KPER,KSTP,KITER,DELT,STRM,ICNVG)
      PROCEDURE, PASS(SWO):: SWO_FINALIZE_TIMESTEP!(KPER,KSTP,KITER,SEG)
      PROCEDURE, PASS(SWO):: RES_INUSE!(IPROJ,IRES)
      FINAL:: DEALLOCATE_SWO_DATA_FINAL
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE DEALLOCATE_SWO_DATA_FINAL(SWO)
    TYPE(SWO_DATA)::SWO
    CALL DEALLOCATE_SWO_DATA(SWO)
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE DEALLOCATE_SWO_DATA(SWO)
    CLASS(SWO_DATA), INTENT(INOUT)::SWO
    INTEGER:: I
    !
    SWO%HAS_SWO         = FALSE
    !
    SWO%INNER_ITERATION = ONE
    !
    SWO%NRES_TOTTOT = Z
    SWO%NRES_BALTOT = Z
    SWO%NRES_SPTTOT = Z                                         ! counters for reservoirs (total, mass bal calcs, split calcs)
    SWO%DIVCOUNT    = Z
    SWO%SPTCOUNT    = Z                                                            ! number of unique diversion segments and splits on model
    !
    !
    SWO%IOUT       = Z
    SWO%LOUT       = Z
    SWO%NROW       = Z
    SWO%NCOL       = Z
    SWO%ITMUNI     = Z
    SWO%NFARM      = Z
    SWO%NSEG       = Z
    SWO%NSTRM      = Z
    SWO%NAUXDEM    = Z
    SWO%NPROJ      = Z
    SWO%NDIST      = Z
    SWO%NUNIT      = Z
    !
    SWO%NSFR_OLD_FLO = Z
    !
    SWO%TFR_READ   = FALSE
    SWO%HAS_OUTPUT = FALSE
    !
    ! LAZY DEALLOCATE/CLEAN UP
    !
    DEALLOCATE(SWO%IRESFL  , STAT=I)
    DEALLOCATE(SWO%NRES_PRJ, STAT=I)
    DEALLOCATE(SWO%NRES_BAL, STAT=I)
    DEALLOCATE(SWO%NRES_SPT, STAT=I)
    DEALLOCATE(SWO%NRES_TOT, STAT=I)    ! reservoir counter arrays
    DEALLOCATE(SWO%RESLINK,  STAT=I)
    !
    DEALLOCATE(SWO%SWMXITER   , STAT=I)          ! weighting factor with orders / without orders
    DEALLOCATE(SWO%REL_CNVG   , STAT=I)          ! convergence criteria @ flows / flow vs. order / outflow
    DEALLOCATE(SWO%ABS_CNVG   , STAT=I)          ! convergence criteria @ flows / flow vs. order / outflow
    DEALLOCATE(SWO%REL_CNVG_CHK,STAT=I)
    DEALLOCATE(SWO%ABS_CNVG_CHK,STAT=I)
    DEALLOCATE(SWO%WTFACTOR   , STAT=I)
    DEALLOCATE(SWO%DIST_DELTOL, STAT=I)          ! FRACTION OF DELIVERY/DEL_ORDER THAT SENDS DEL_ORDER TO 0 IF RATIO GOES BELOW THAT
    !
    DEALLOCATE(SWO%FARM       , STAT=I)
    DEALLOCATE(SWO%FARM_PREV  , STAT=I)
    DEALLOCATE(SWO%UNIT       , STAT=I)
    DEALLOCATE(SWO%UNIT_PREV  , STAT=I)
    DEALLOCATE(SWO%DIST       , STAT=I)
    DEALLOCATE(SWO%DIST_PREV  , STAT=I)
    DEALLOCATE(SWO%PROJ       , STAT=I)
    DEALLOCATE(SWO%PROJ_PREV  , STAT=I)
    DEALLOCATE(SWO%AUXDEM     , STAT=I)
    DEALLOCATE(SWO%AUXDEM_PREV, STAT=I)
    DEALLOCATE(SWO%AUXDEM_TSF,  STAT=I)
    DEALLOCATE(SWO%AUXAREA_TSF, STAT=I)
    !
    DEALLOCATE(SWO%RES_INFLOW      , STAT=I)
    DEALLOCATE(SWO%RES_PRECIP      , STAT=I)
    DEALLOCATE(SWO%RES_EVAP        , STAT=I)
    DEALLOCATE(SWO%RES_RELEASE_SPEC, STAT=I)
    DEALLOCATE(SWO%RES_STORAGE_MIN, STAT=I)
    DEALLOCATE(SWO%RES_RELEASE_MIN, STAT=I)
    !
    !DEALLOCATE(SWO%RESBAL  , STAT=I)
    !DEALLOCATE(SWO%RESSPLIT, STAT=I)
    !DEALLOCATE(SWO%ACAP    , STAT=I)
    !DEALLOCATE(SWO%FRAC    , STAT=I)
    !DEALLOCATE(SWO%HEAD    , STAT=I)
    !
    DEALLOCATE(SWO%SEGDATA      , STAT=I)
    DEALLOCATE(SWO%SEGDATA_PREV , STAT=I)
    DEALLOCATE(SWO%SEGDATA_SAVE , STAT=I)     ! input values (from SFR input)
    DEALLOCATE(SWO%FLOWDATA     , STAT=I)
    DEALLOCATE(SWO%FLOWDATA_PREV, STAT=I)
    DEALLOCATE(SWO%FLOWDATA_SAVE, STAT=I)     ! input values (from SFR input)
    !
    DEALLOCATE(SWO%RCHDATA     , STAT=I)
    DEALLOCATE(SWO%RCHDATA_PREV, STAT=I)
    DEALLOCATE(SWO%RCHDATA_SAVE, STAT=I)      ! input values (from SFR input)
    !
    DEALLOCATE(SWO%UPSEG, STAT=I)
    DEALLOCATE(SWO%DNSEG, STAT=I)
    !
    DEALLOCATE(SWO%UPTREE_NAT , STAT=I)        ! uptree @ natural segments
    DEALLOCATE(SWO%UPTREE_CON1, STAT=I)        ! uptree @ conveyance segments (SEGTYPE=2)
    DEALLOCATE(SWO%UPTREE_CON2, STAT=I)        ! uptree @ conveyance+delivery segments (SEGTYPE=2.OR.3)
    DEALLOCATE(SWO%UPTREE_RET , STAT=I)        ! uptree @ return segments
    DEALLOCATE(SWO%DNTREE_NAT , STAT=I)        ! dntree @ natural segments
    DEALLOCATE(SWO%DNTREE_CON1, STAT=I)        ! dntree @ conveyance segments (SEGTYPE=2)
    DEALLOCATE(SWO%DNTREE_CON2, STAT=I)        ! dntree @ conveyance+delivery segments (SEGTYPE=2.OR.3)
    DEALLOCATE(SWO%DNTREE_RET , STAT=I)        ! dntree @ return segments
    !
    DEALLOCATE(SWO%SEGINFO    , STAT=I)
    DEALLOCATE(SWO%DIVSEG     , STAT=I)
    DEALLOCATE(SWO%DIVSEG_PREV, STAT=I)
    DEALLOCATE(SWO%SPTSEG     , STAT=I)
    DEALLOCATE(SWO%SPTSEG_PREV, STAT=I)
    !
    DEALLOCATE(SWO%NSPLIT,      STAT=I)
    !
    DEALLOCATE(SWO%RESNAM,      STAT=I)
    DEALLOCATE(SWO%SPLITNAM,    STAT=I)
    !
    DEALLOCATE(SWO%MIN_DELORDER,     STAT=I)
    DEALLOCATE(SWO%MIN_PROJ_ORDER,   STAT=I)
    DEALLOCATE(SWO%MIN_DELORDER_RAT,  STAT=I)
    DEALLOCATE(SWO%MIN_PROJ_ORDER_RAT,STAT=I)
    DEALLOCATE(SWO%FMP_SW_DEMAND,    STAT=I)
    DEALLOCATE(SWO%FMP_SW_ALLOTMENT, STAT=I)
    DEALLOCATE(SWO%FMP_SW_LIMIT,     STAT=I)
    DEALLOCATE(SWO%FMP_SW_LIMIT_RULZ,STAT=I)
    !
    DEALLOCATE(SWO%SFR_OLD_FLO,      STAT=I)
    !
  END SUBROUTINE
  !
  PURE FUNCTION RES_INUSE(SWO,IPROJ,IRES) RESULT(INUSE)
    CLASS(SWO_DATA), INTENT(IN)::SWO
    INTEGER,         INTENT(IN):: IPROJ, IRES
    LOGICAL:: INUSE
    !
    INUSE = SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE
    !
  END FUNCTION
  !
  SUBROUTINE INITIALIZE_SWO_DATA( BL, SWO, LINE, FDIM, NSEG, NSTRM, SEG_NSTRM, FASTFORWARD, MXITER )
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL   !DATA BLOCKUSE GWFSFRMODULE, ONLY: SEG_NSTRM
    CLASS(SWO_DATA),             INTENT(INOUT):: SWO
    CHARACTER(*),                INTENT(INOUT):: LINE
    TYPE(FMP_DIMENSION),         INTENT(IN   ):: FDIM
    INTEGER,                     INTENT(IN   ):: NSEG, NSTRM
    INTEGER, DIMENSION(:),       INTENT(IN   ):: SEG_NSTRM
    INTEGER,                     INTENT(IN   ):: FASTFORWARD, MXITER
    !
    CONTIGUOUS:: SEG_NSTRM
    !
    TYPE(GENERIC_INPUT_FILE):: FL
    TYPE(GENERIC_INPUT_FILE), DIMENSION(:), ALLOCATABLE:: FILES
    TYPE(SFAC_DATA):: SFAC
    !
    TYPE(INTEGER_LINKED_LIST  ):: PRNT_FLG, ILIST
    TYPE(CHARACTER_LINKED_LIST):: PRNT_VAR, CLIST
    !
    CHARACTER(30),         DIMENSION(:),ALLOCATABLE:: RESNAM, SPLITNAM
    INTEGER,               DIMENSION(:),ALLOCATABLE:: SPILL_PREF
    DOUBLE PRECISION,      DIMENSION(:),ALLOCATABLE:: RES_INITIAL_STORAGE, MAX_RELEASE, STORAGE_CAPACITY,STORAGE_SPILL, MAX_AREA, ELEV_CHNG, ALLOT_LIM, STORAGE_DPL
    TYPE(DATE_OPERATOR),   DIMENSION(:),ALLOCATABLE:: ALLOC_DATE, RES_START
    INTEGER,               DIMENSION(:),ALLOCATABLE:: RES_START_SP
    TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: RES_INITIAL_STORAGE_TSF !ONLY USED ONCE IF USER USES THAT TO SPECIFY INITIAL STORAGE
    TYPE(WARNING_TYPE):: WARN_MSG1, WARN_MSG2
    CHARACTER(4):: STEP
    CHARACTER(5):: ERROR
    CHARACTER(20):: TXT
    CHARACTER(:),ALLOCATABLE:: CUSTOM, STR_NRES_BAL, CTMP
    DOUBLE PRECISION:: TMP1, TMP2
    INTEGER:: I, J, K, N, LLOC, ISTART, ISTOP, IERR
    INTEGER:: NRES_BAL, NRES_SPT
    INTEGER:: JRES, IMAX
    LOGICAL:: NO_ALLOCATION_DATE, BY_IT, HAS_RESNAM, HAS_SPLITNAM, HAS_READHEAD, HAS_SPLIT_FRAC, NO_INITIAL_STORAGE, NO_ACAP, NO_MAX_RELEASE, NO_MAX_STORAGE, NO_RELEASE_FRAC, NO_REQ_REL_FRAC, NO_MAX_AREA, NO_ELEV_CHNG, IS_TIME_SERIES, NO_STORAGE_DPL, NO_MAX_SPILL, HAS_SPILLWAY
    !
    TMP1 = HECTO
    WRITE(BL%IOUT,'(/A/)') 'SWO BLOCK FOUND AND NOW LOADING PROPERTIES'
    CALL WARN_MSG1%INIT()
    CALL WARN_MSG2%INIT()
    !
    IF(.NOT. HAS_STARTDATE .AND. REALTIM < DZ) CALL WARN_MSG1%ADD('SWO BLOCK REQUIRES THAT THE DIS PACKAGE SPECIFY A STARTING TIME WITH THE KEYWORD "STARTDATE", "LEAPYEARS STARTIME" OR "STARTIME". NOTE THAT "STARTIME" DOES NOT ACCOUNT FOR LEAP YEARS AND IS NOT RECOMMENDED.'//BLN)
    !
    IF(FDIM%NFARM < ONE)         CALL WARN_MSG1%ADD('SWO BLOCK REQUIRES THAT THE GLOBAL DEMENSION BLOCK SPECIFY NWBS > ZERO'//BLN)
    IF(.NOT. FDIM%HAS_HIERARCHY) CALL WARN_MSG1%ADD('SWO BLOCK REQUIRES THAT THE GLOBAL DEMENSION BLOCK SPECIFY NPROJ, NDIST, AND NUNIT'//BLN)
    IF(FDIM%NPROJ < ONE)         CALL WARN_MSG1%ADD('SWO BLOCK REQUIRES THAT THE GLOBAL DEMENSION BLOCK SPECIFY NPROJ > ZERO'//BLN)
    IF(FDIM%NDIST < ONE)         CALL WARN_MSG1%ADD('SWO BLOCK REQUIRES THAT THE GLOBAL DEMENSION BLOCK SPECIFY NDIST > ZERO'//BLN)
    IF(FDIM%NUNIT < ONE)         CALL WARN_MSG1%ADD('SWO BLOCK REQUIRES THAT THE GLOBAL DEMENSION BLOCK SPECIFY NUNIT > ZERO'//BLN)
    IF(NSEG       < ONE)         CALL WARN_MSG1%ADD('CURRENTLY SFR IS REQURIED IN ORDER TO USE SURFACE WATER OPERATIONS. OTHERWISE IT HAS NO MEANS OF MAKING A RELEASE INTO A STREAM NETWORK AND SIMULATING OPERATIONS.'//BLN)
    !
    CALL WARN_MSG1%CHECK('SURFACE WATER OPERATIONS FATAL ERRORS. THE FOLLOWING ARE ERROR MESSAGES THAT INDICATE WHY SWO CAN NOT CONTINUE:'//BLN,BL%IU,BL%IOUT,KILL=TRUE,TAIL=BLN)
    !
    !WRITE(123,'(A)') 'KPER KSTP KITER  PROJ_REL      REQ_REL     INFLOW     REQ        DSEEP   UnderRelease OverRelease  RES1   RES2'
    !NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    SWO%NaN = IEEE_VALUE(TMP1, IEEE_QUIET_NAN)
    !
    SWO%IOUT = BL%IOUT
    SWO%LOUT = BL%IOUT
    SWO%NSEG = NSEG
    SWO%NSTRM= NSTRM
    SWO%DELT = DZ
    SWO%DELT2= DZ
    SWO%DELT_INV= DZ
    !
    SWO%NFARM  = FDIM%NFARM
    SWO%NPROJ  = FDIM%NPROJ
    SWO%NDIST  = FDIM%NDIST
    SWO%NUNIT  = FDIM%NUNIT
    SWO%NAUXDEM = FDIM%NAUXDEM
    !
    SWO%ITMUNI=FDIM%ITMUNI
    SWO%NROW = FDIM%NROW
    SWO%NCOL = FDIM%NCOL
    !
    SWO%SFR_ID = FDIM%SFR_ID
    !
    IF(ALLOCATED(SWO%RES_CONV_ERROR)) THEN
        IF(SWO%RES_CONV_ERROR.NE.NL) THEN
           DEALLOCATE(SWO%RES_CONV_ERROR)
             ALLOCATE(SWO%RES_CONV_ERROR, SOURCE=NL)  !ERROR if SWO%RES_CONV_ERROR.NE.NL
        END IF
    ELSE
             ALLOCATE(SWO%RES_CONV_ERROR, SOURCE=NL)  !ERROR if SWO%RES_CONV_ERROR.NE.NL
    END IF
    !
    SWO%LEAP_YEAR      = TRUE
    SWO%HAS_MAX_SPILL  = FALSE
    !
    NO_ACAP            = TRUE
    HAS_SPLIT_FRAC     = FALSE
    HAS_READHEAD       = FALSE
    NO_ALLOCATION_DATE = TRUE
    NO_INITIAL_STORAGE = TRUE
    NO_MAX_RELEASE     = TRUE
    NO_MAX_STORAGE     = TRUE
    NO_ELEV_CHNG       = TRUE
    NO_MAX_AREA        = TRUE
    NO_RELEASE_FRAC    = TRUE
    NO_REQ_REL_FRAC    = TRUE
    NO_STORAGE_DPL     = TRUE
    NO_MAX_SPILL       = TRUE
    ERROR='ERROR'
    STEP = 'STEP'
    !
    SWO%RELEASE_RELAX = UNO
    SWO%REQFLOW_RELAX = UNO
    SWO%DIST_ALLOC_FRAC_TYP = NEG
    !
    SWO%NUM_PRNT_VAR = Z
    SWO%HAS_BY_ITERATION = FALSE
    CALL PRNT_FLG%INIT()
    CALL PRNT_VAR%INIT()
    !
    ALLOCATE(SWO%IRESFL(SWO%NPROJ), SWO%NRES_PRJ(SWO%NPROJ), SWO%NRES_TOT(SWO%NPROJ), SOURCE=Z)
    ALLOCATE(SWO%SWMXITER(THREE), SOURCE=NEG)
    ALLOCATE(SWO%REL_CNVG(FIVE),  SOURCE=DNEG)
    ALLOCATE(SWO%ABS_CNVG(FIVE),  SOURCE=DNEG)
    ALLOCATE(SWO%REL_CNVG_CHK(FIVE),SOURCE=DNEG)
    ALLOCATE(SWO%ABS_CNVG_CHK(FIVE),SOURCE=DNEG)


    ALLOCATE(SWO%WTFACTOR(TWO),   SOURCE=DNEG)
    !
    ALLOCATE(SWO%PROJ(SWO%NPROJ), SWO%PROJ_PREV(SWO%NPROJ))
    ALLOCATE(SWO%DIST(SWO%NDIST), SWO%DIST_PREV(SWO%NDIST))
    ALLOCATE(SWO%UNIT(SWO%NUNIT), SWO%UNIT_PREV(SWO%NUNIT))
    !
    ALLOCATE( SWO%NRES_BAL(SWO%NPROJ), SWO%NRES_SPT(SWO%NPROJ), SOURCE=Z )
    ALLOCATE( SWO%RESDAT(SWO%NPROJ))
    ALLOCATE(ALLOC_DATE(SWO%NPROJ))
    !
    ALLOCATE(SWO%FMP_SW_DEMAND(SWO%NFARM),    SOURCE=DZ)
    ALLOCATE(SWO%FMP_SW_ALLOTMENT(SWO%NFARM), SOURCE=inf)  !LEFT TO inf if never used
    !
    ALLOCATE(SWO%FMP_SW_LIMIT     (SWO%NFARM))
    !
    STR_NRES_BAL = NUM2STR(NRES_BAL)
    !
    !DO CONCURRENT(I=ONE:WBS%NFARM); WBS%FARM_NAME(I) = 'RES_'//NUM2STR(I,PAD=THREE)
    !END DO
    !
    ! allocate array for SFR output
    ! (BD uses stored values instead of recalculating after FM loop)
    !ALLOCATE( SFROUT(32,NSTRM) )
    ! allocate array for XCHECK output from SFR
    !ALLOCATE( XOUT_SFR(6,NSTRM) )
    ! allocate array for XCHECK output from CNVG
    !ALLOCATE( DCNVG(8,SWO%NPROJ) )
    !
    IF(SWO%NFARM   > Z) ALLOCATE(SWO%FARM(SWO%NFARM),     SWO%FARM_PREV(SWO%NFARM)    )
    IF(SWO%NAUXDEM > Z) ALLOCATE(SWO%AUXDEM(SWO%NAUXDEM),SWO%AUXDEM_PREV(SWO%NAUXDEM), SWO%AUXDEM_TSF(SWO%NAUXDEM), SWO%AUXAREA_TSF(SWO%NAUXDEM) )
    !
    !CALL BL%LIST%START()
    !CALL BL%LIST%SET_LN()
    !DO I=ONE, BL%NLINE
    !                LLOC=ONE
    !                CALL PARSE_WORD(BL%LIST%LN,LLOC,ISTART,ISTOP,TRUE)
    !                IF (BL%LIST%LN(ISTART:ISTOP)=='NAUX') THEN
    !                      CALL URWORD(BL%LIST%LN,LLOC,ISTART,ISTOP,TWO,SWO%NAUX,R,BL%IOUT,BL%IU)
    !                      EXIT
    !                END IF
    !                !
    !                CALL BL%LIST%NEXT()
    !                CALL BL%LIST%SET_LN()
    !END DO
    !
    CALL BL%MAKE_SCRATCH_FILE()
    !
    READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    !
    DO I=ONE, BL%NLINE
                    LLOC=ONE
                    CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                    SELECT CASE ( LINE(ISTART:ISTOP) )
                    CASE ("NRES_PROJ","PROJECT_RESERVOIR_COUNT")
                                      WRITE(BL%IOUT,'(A)') 'PROJECT_RESERVOIR_COUNT (NRES_PROJ)  KEYWORD FOUND. NOW LOADING RESERVOIR COUNTS.'
                                      IERR=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                                      CALL ULOAD(SWO%IRESFL, LLOC, LINE, BL%IOUT, BL%IU, IERR, SCRATCH=BL%SCRATCH, MSG='ERROR IN SWO AFTER KEWORD PROJECT_RESERVOIR_COUNT')
                                      SWO%NRES_PRJ = ABS(SWO%IRESFL)
                                      EXIT
                    END SELECT
                    !
                    READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    END DO
    !
    !-------------------------------------------------------------------------------
    !
    CALL ALLOCATE_NRES_N_RESDAT_ARRAYS(SWO, I, J, N)  !ALLOCATE RESDAT ARRAY
    !
    !-------------------------------------------------------------------------------
    !
    NRES_BAL = SWO%NRES_BALTOT !SUM(SWO%NRES_BAL)
    HAS_RESNAM = NRES_BAL > Z
    IF(HAS_RESNAM)  THEN
                        SWO%HAS_SWO = TRUE
                        SWO%BEGIN_SP = ONE
                        ALLOCATE(    RESNAM(NRES_BAL))
                        ALLOCATE(SWO%RESNAM(NRES_BAL))
                        K = Z
                        DO I = ONE, SWO%NPROJ
                              DO J = ONE, SWO%NRES_BAL(I)
                                     K = K + ONE
                                     IF(SWO%IRESFL(I).GT.Z) THEN
                                           RESNAM(K)='PROJ_'//NUM2STR(I,2,TRUE)//'_RES_'//NUM2STR(J,2,TRUE)
                                     ELSE
                                           RESNAM(K)='PROJ_'//NUM2STR(I,2,TRUE)//'_LUMPED_POOL_RES'
                                     END IF
                              END DO
                        END DO
    ELSE
        CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG= 'SWO BLOCK ERROR: THE TOTAL NUMBER OF SIMULATED RESERVOIRS IS ZERO.'//BLN//'WHY ARE YOU EVEN USING SWO???'//BLN//'SWO WILL BE DISABLED AND IGNORED.',CMD_PRINT=TRUE)
        CALL DEALLOCATE_SWO_DATA(SWO)
        RETURN
    END IF
    !
    NRES_SPT = SUM(SWO%NRES_SPT)
    HAS_SPLITNAM = NRES_SPT > Z
    IF(HAS_SPLITNAM)  THEN
                        ALLOCATE(    SPLITNAM(NRES_SPT))
                        ALLOCATE(SWO%SPLITNAM(NRES_SPT))
                        K = Z
                        DO I = ONE, SWO%NPROJ
                              DO J = ONE, SWO%NRES_SPT(I)
                                     K = K + ONE
                                     SPLITNAM(K)='PROJ_'//NUM2STR(I,2,TRUE)//'_RES_SPLIT_'//NUM2STR(J,2,TRUE)
                              END DO
                        END DO
    END IF
    !
    ALLOCATE(RES_INITIAL_STORAGE(NRES_BAL), MAX_RELEASE(NRES_BAL), STORAGE_CAPACITY(NRES_BAL), STORAGE_SPILL(NRES_BAL), SPILL_PREF(NRES_BAL), MAX_AREA(NRES_BAL), ELEV_CHNG(NRES_BAL), RES_START(NRES_BAL), RES_START_SP(NRES_BAL), STORAGE_DPL(NRES_BAL) )
    ALLOCATE(SWO%RES_INFLOW(NRES_BAL), SWO%RES_PRECIP(NRES_BAL), SWO%RES_EVAP(NRES_BAL), SWO%RES_RELEASE_SPEC(NRES_BAL), SWO%RES_STORAGE_MIN(NRES_BAL), SWO%RES_RELEASE_MIN(NRES_BAL))
    ALLOCATE(ALLOT_LIM(SWO%NDIST), SOURCE=DOS)
    ALLOCATE(SWO%RESLINK(NRES_BAL,NRES_BAL), SOURCE=Z)
    !
    ALLOCATE(SWO%MIN_DELORDER  ( SWO%NFARM+SWO%NAUXDEM ), SOURCE=DZ )
    ALLOCATE(SWO%MIN_PROJ_ORDER( SWO%NPROJ             ), SOURCE=DZ )
    !
    MAX_AREA         = DNEG
    STORAGE_CAPACITY = DNEG
    STORAGE_SPILL    = DNEG
    STORAGE_DPL      = DNEG
    ELEV_CHNG        = DNEG
    RES_START_SP     = ONE
    SPILL_PREF       = Z
    !
    DO I=ONE, NRES_BAL
              RES_START(I) = DATE_SP(ONE)%TS(Z)
    END DO
    !
    ! ----------------------------------------------------------------------------------------------------
    !
    ! LOAD STARTING KEYWORDS AND ACAP TABLES
    !
    REWIND(BL%SCRATCH)
    READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    DO N=ONE, BL%NLINE
         LLOC=ONE
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         SELECT CASE ( LINE(ISTART:ISTOP) )
         !
         CASE ("STARTING_STRESS_PERIOD") !ALLOW RESERVOIR OPERATIONS TO BEGIN AT A DATE OTHER THAN THE FIRST FIRST SP
                           !
                           CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,BL%IU,SWO%BEGIN_SP, MSG='FAILED TO LOAD AFTER KEYWORD "STARTING_STRESS_PERIOD" THE STARTING STRESS PERIOD THAT SURFACE WATER OPERATIONS WILL BEGIN ON.')
         !
         CASE ("RESERVOIR_OPERATION_START", "OPERATION_START","RESERVOIR_OPERATIONS_START", "OPERATIONS_START") !ALLOW RESERVOIR OPERATIONS TO BEGIN AT A DATE OTHER THAN THE FIRST FIRST SP
                           !
                           WRITE(BL%IOUT,'(A)') 'RESERVOIR_OPERATIONS_START KEYWORD FOUND. NOW LOADING KEYWORD "STRESS_PERIOD" OR "DATE" TO INDICATE HOW THE STARTING DATE IS SPECIFIED.'
                           CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                           !
                           I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                           !
                           SELECT CASE ( LINE(ISTART:ISTOP) )
                           CASE("STRESS_PERIOD")
                                                WRITE(BL%IOUT,'(A)') LINE(ISTART:ISTOP)//' KEYWORD FOUND. NOW LOADING'//STR_NRES_BAL//' STARTING RESERVOIR STRESS PERIODS. THIS IS WHEN THE RESERVOIR WILL BE APART OF THE OPERATIONS AND AVAILIBLE FOR RELEASE. THIS WILL LOAD WITH ULOAD.'
                                                !
                                                CALL ULOAD(RES_START_SP, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR IN SWO AFTER KEWORD RESERVOIR_OPERATION_START')
                                                !
                                                DO I=ONE, NRES_BAL
                                                       IF(RES_START_SP(I) > NPER) THEN
                                                           K = UBOUND(DATE_SP(NPER)%TS,ONE)
                                                           RES_START(I) = DATE_SP(NPER)%TS(K)
                                                       ELSEIF(RES_START_SP(I) < ONE) THEN
                                                           RES_START_SP(I) = ONE
                                                           RES_START(I)    = DATE_SP(ONE)%TS(Z)
                                                       ELSE
                                                           RES_START(I) = DATE_SP(RES_START_SP(I))%TS(Z)
                                                       END IF
                                                END DO
                           CASE("DATE")
                                                WRITE(BL%IOUT,'(A)') LINE(ISTART:ISTOP)//' KEYWORD FOUND. NOW LOADING'//STR_NRES_BAL//' STARTING RESERVOIR DATES. THIS IS WHEN THE RESERVOIR WILL BE APART OF THE OPERATIONS AND AVAILIBLE FOR RELEASE. THIS WILL LOAD WITH ULOAD.'
                                                !
                                                CALL ULOAD(RES_START, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR IN SWO AFTER KEWORD RESERVOIR_OPERATION_START')
                                                CALL RES_START%ADD_DAY(0.001D0) !SLIGHT OFF SET
                                                DO I=ONE, NRES_BAL
                                                       CALL DATE_TO_SP(RES_START(I),RES_START_SP(I))
                                                END DO
                                                CALL RES_START%ADD_DAY(-0.001D0) !Move back to start
                           CASE DEFAULT
                                                CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FOUND KEYWORD "RESERVOIR_START", WHICH MUST BE FOLLOWED BY THE KEYWORD "STRESS_PERIOD" OR "DATE" .')
                           END SELECT
                           !
         CASE ("AREA_CAPACITY_TABLE","ELEVATION_AREA_CAPACITY_TABLE")
                           WRITE(BL%IOUT,'(A)') 'ELEVATION_AREA_CAPACITY_TABLE (AREA_CAPACITY_TABLE) KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_TOTTOT)//' INITIAL RESERVOIR STORAGE VOLUMES WITH ULOAD.).'
                           !
                           NO_ACAP = FALSE
                           ALLOCATE(FILES(SWO%NRES_TOTTOT))
                           I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                           !
                           CALL ULOAD(FILES, LLOC, LINE, BL%IOUT, BL%IU, I, SFAC=SFAC, EX1_WORD='CAPACITY', EX1_DIM=ONE, EX2_WORD='AREA', EX2_DIM=ONE, EX3_WORD='ELEVATION', EX3_DIM=ONE, SCRATCH=BL%SCRATCH, MSG='ERROR IN SWO AFTER KEWORD ELEVATION_AREA_CAPACITY_TABLE (AREA_CAPACITY_TABLE)')
                           !
                           IF(ANY(FILES(:)%IS_CONSTANT)) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: KEYWORD "ELEVATION_AREA_CAPACITY_TABLE" KEYWORD DOES NOT SUPPORT THE KEYWORD "CONSTANT"')
                           !
                           K=Z
                           DO I = ONE, SWO%NPROJ
                                 DO J = ONE, SWO%NRES_TOT(I)
                                                           K = K + ONE
                                                           CALL SWOPS_READACAP(SWO,FILES(K),LINE,I,J,SFAC)
                                 END DO
                           END DO
                           DEALLOCATE(FILES)
                           CALL ACAP_CHECK(SWO,BL%IOUT,BL%IU)
                           !
          CASE ("DEFINE_SWO_VARIABLE","DEFINE_SWO_VARIABLES","DEFINE_S_VARIABLE","DEFINE_S_VARIABLES","DEFINE_VARIABLE","DEFINE_VARIABLES")
                           WRITE(BL%IOUT,'(A)') LINE(ISTART:ISTOP)//'     KEYWORD FOUND. NOW OPENING EXTERNAL FILE THAT CONTAINS THE FOLLOWING BLOCKS: "VARIABLE", "PROPERTY", "OUTPUT", "LOOKUP", and "RETURN"'
                           CALL FL%OPEN(LINE, LLOC, SWO%IOUT, BL%IU)
                           IF(FL%IS_INTERNAL) CALL STOP_ERROR( LINE,BL%IU,BL%IOUT, MSG= 'SWO BLOCK ERROR: KEYWORD "DEFINE_SWO_VARIABLE" DOES NOT ALLOW FOR THE "INTERNAL" KEYWORD OR IMPLIED INTERNAL (LOADING ON SAME LINE). PLEASE MOVE INTERNAL INPUT TO SEPARATE FILE AND CHANGE KEYWORD TO "OPEN/CLOSE" OR "EXTERNAL".'//NL//'[NOTE THIS IS BECAUSE THE INPUT READ UTILITY CAN NOT DISTINGUISH BETWEEN THE END STATEMENT FOR DECLAIRING THE DECISION VARIABLES COMPARED TO THE END OF THE SWO INPUT BLOCK.]')
                           CALL SWO%DEC_VAR%LOAD(FL%IU, SWO%IOUT)
                           CALL FL%CLOSE()
         END SELECT
         !
         READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    END DO
    !
    ! ENSURE THAT ACAP TABLES HAVE BEEN LOADED
    IF(NO_ACAP)  CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FAILED TO IDENTIFY/LOCATE WITHIN BLOCK THE KEYWORD "ELEVATION_AREA_CAPACITY_TABLE" KEYWORD,'//NL//'WHICH LOADS FOR ALL RESERVOIRS THEIR ELEVATION, AREA, CAPACITY RELATIONSHIP.'//NL//'[NOTE THAT "AREA_CAPACITY_TABLE" KEYWORD WORKS AS WELL.]')
    !
    !
    ! CHECK FOR RESERVOIR STARTING TIME BEFORE FASTFORWARD
    !
    IF(FASTFORWARD > NPER) K = UBOUND(DATE_SP(NPER)%TS,ONE)
    !
    DO I=ONE, NRES_BAL
      IF( RES_START_SP(I) < FASTFORWARD ) THEN
               RES_START_SP(I) = FASTFORWARD
               !
               IF(FASTFORWARD > NPER) THEN
                   RES_START(I)    = DATE_SP(NPER)%TS(K)  !FASTWORD IS TO THE END OF THE SIMUALTION
               ELSE
                   RES_START(I)    = DATE_SP(FASTFORWARD)%TS(Z)
               END IF
      END IF
    END DO
    !
    ! LOAD MAIN KEYWORDS -------------------------------------------------------------------------------------
    !
    REWIND(BL%SCRATCH)
    READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    DO WHILE (IERR == Z)
      !
      LLOC=ONE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      !
      SELECT CASE ( LINE(ISTART:ISTOP) )
      !
      !CASE ("NO_LEAP_YEAR","NO_LEAP_YEARS");
      !                  WRITE(BL%IOUT,'(A)') 'NO_LEAP_YEARS     KEYWORD FOUND. FRACTIONS OF YEAR WILL BE BASED ON 365.2425 DAYS IN A YEAR RATHER THAN TAKING INTO ACCOUNT LEAP YEARS (365/366).'
      !                  SWO%LEAP_YEAR = FALSE
      !
      CASE ("STARTING_STRESS_PERIOD") ! BY PASS KEYWORD
                        !
                        CONTINUE
                        !
      CASE ("RESERVOIR_OPERATION_START", "OPERATION_START","RESERVOIR_OPERATIONS_START", "OPERATIONS_START") ! BY PASS KEYWORD
                        !
                        IF(IS_IN_STR('INTERNAL',LINE,TRUE,TRUE)) THEN  !IS_IN_STR(SUB,LINE,COM_STOP,UPCASE)
                              DO I = ONE, SWO%NPROJ
                                                   READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
                              END DO
                        END IF
                        !
      CASE ("PROJECT_RESERVOIR_COUNT") ! BY PASS KEYWORD
                        !
                        IF(IS_IN_STR('INTERNAL',LINE,TRUE,TRUE)) THEN  !IS_IN_STR(SUB,LINE,COM_STOP,UPCASE)
                              DO I = ONE, SWO%NPROJ
                                                   READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
                              END DO
                        END IF
                        !
      CASE ("AREA_CAPACITY_TABLE","ELEVATION_AREA_CAPACITY_TABLE") ! BY PASS KEYWORD
                        !
                        IF(IS_IN_STR('INTERNAL',LINE,TRUE,TRUE)) THEN  !IS_IN_STR(SUB,LINE,COM_STOP,UPCASE)
                              DO I = ONE, SWO%NRES_TOTTOT
                                                   READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
                                                   LLOC=ONE
                                                   CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                                                   DO WHILE(LINE(ISTART:ISTOP).EQ.'SFAC')
                                                         READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
                                                         LLOC=ONE
                                                         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                                                   END DO
                              END DO
                        END IF
                        !
      CASE ("INTERNAL_ITERATION","INTERNAL_ITERATIONS")
                        WRITE(BL%IOUT,'(A)') 'INTERNAL_ITERATION     KEYWORD FOUND. NOW READING SINGLE INTEGER THAT REPRESENTS THE NUMBER OF SWO-SFR ALLOCATION, RELEASE, GAINS/LOSSES ITERATIONS WITHIN A SINGLE ITERATION.'
                        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,BL%IU,SWO%INNER_ITERATION, MSG='FAILED TO LOAD AFTER KEYWORD "INTERNAL_ITERATION" THE NUMBER OF INTERNAL INTERATIONS')
                        !
                        IF(SWO%INNER_ITERATION < ONE) SWO%INNER_ITERATION = ONE
      !CASE ("DEMAND_RELEASE_RELAXATION","RELEASE_RELAX")
      !                  WRITE(BL%IOUT,'(A)') 'DEMAND_RELEASE_RELAXATION (RELEASE_RELAX)  KEYWORD FOUND NOW LOADING SINGLE RELAXATION FACTOR.'
      !                  CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%RELEASE_RELAX, MSG='FAILED TO LOAD AFTER KEYWORD "DEMAND_RELEASE_RELAXATION" THE VALUE OF RELEASE_RELAX')
      !                  !
      CASE ("REQUIRED_FLOW_OVER_RELEASE_RELAXATION")
                        WRITE(BL%IOUT,'(A)') 'REQUIRED_FLOW_OVER_RELEASE_RELAXATION KEYWORD FOUND NOW LOADING SINGLE RELAXATION FACTOR.'
                        CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%REQFLOW_RELAX, MSG='FAILED TO LOAD AFTER KEYWORD "DEMAND_RELEASE_RELAXATION" THE VALUE OF RELEASE_RELAX')
                        !
      CASE ("SWO_ITERATION_THRESHOLDS","SWO_ITERATION_THRESHOLD","SWMXITER")
                        WRITE(BL%IOUT,'(A)') 'SWO_ITERATION_THRESHOLDS KEYWORD FOUND. NOW READING THREE INTEGERS THAT REPRESENT SWMXITER1, SWMXITER2, and SWMXITER3.'
                        CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,BL%IU,SWO%SWMXITER, MSG='FAILED TO LOAD AFTER KEYWORD "ITERATION_THRESHOLD" THE VALUE OF SWMXITER')
                        IF(SWO%SWMXITER(1) < 25)  SWO%SWMXITER(1) = 25
                        IF(SWO%SWMXITER(2) < ONE) SWO%SWMXITER(2) = ONE
                        IF(SWO%SWMXITER(3) < SWO%SWMXITER(1) + 2*SWO%SWMXITER(2) + ONE) SWO%SWMXITER(3) = SWO%SWMXITER(1) + 2*SWO%SWMXITER(2) + ONE
                        !
      CASE ("ABSOLUTE_CONVERGENCE_CRITERIA")
                        WRITE(BL%IOUT,'(A)') 'ABSOLUTE_CONVERGENCE_CRITERIA KEYWORD FOUND. NOW READING FOUR FLOATING POINT NUMBERS THAT REPRESENT:'//NL//'MAX ABSOLUTE RESERVOIR RELEASE CHANGE,'//NL//'MAX ABSOLUTE CHANGE IN SWO OPERATED DIVERSIONS,'//NL//'MAX ABSOLUTE CHANGE IN BENEFICIARY (FARM/AUXDEM) DELIVERY,'//NL//'MAX ABSOLUTE CHANGE IN BENEFICIARY (FARM/AUXDEM) DELIVERY ORDER TO ACTUAL DELIVERY,'//NL//'MAX ABSOLUTE RESERVOIR REQUIRED FLOW RELEASE ESTIMATE CHANGE.'//NL//'WHICH REPRESENT: [ACNVG_1, ACNVG_2, ACNVG_3, ACNVG_4, AND ACNVG_5].'
                        CALL ULOAD(SWO%ABS_CNVG,LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR OCCURED AFTER SWO KEYWORD ABSOLUTE_CONVERGENCE_CRITERIA')
                        !
      CASE ("RELATIVE_CONVERGENCE_CRITERIA","SWMXCNVG")
                        WRITE(BL%IOUT,'(A)') 'RELATIVE_CONVERGENCE_CRITERIA KEYWORD FOUND. NOW READING FOUR FLOATING POINT NUMBERS THAT REPRESENT:'//NL//'MAX RELATIVE RESERVOIR RELEASE CHANGE,'//NL//'MAX RELATIVE CHANGE IN SWO OPERATED DIVERSIONS,'//NL//'MAX RELATIVE CHANGE IN BENEFICIARY (FARM/AUXDEM) DELIVERY,'//NL//'MAX RELATIVE CHANGE IN BENEFICIARY (FARM/AUXDEM) DELIVERY ORDER TO ACTUAL DELIVERY,'//NL//'MAX RELATIVE RESERVOIR REQUIRED FLOW RELEASE ESTIMATE CHANGE.'//NL//'WHICH REPRESENT: [REL_CNVG1, REL_CNVG2, REL_CNVG3, REL_CNVG4, AND REL_CNVG5].'
                        CALL ULOAD(SWO%REL_CNVG,LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR OCCURED AFTER SWO KEYWORD RELATIVE_CONVERGENCE_CRITERIA')
                        !
      CASE ("DIVERSION_ORDER_DAMPENING","DIVERSION_ORDERS_DAMPENING","WTFACTOR")
                        WRITE(BL%IOUT,'(A)') 'DIVERSION_ORDER_DAMPENING     KEYWORD FOUND. NOW READING TWO FLOATING POINT NUMBERS THAT REPRESENT WTFACTOR1 AND WTFACTOR2. '//NL//'WHICH SPECIFIES THE DAMPENING FACTOR IMPOSED ON CHANGES IN DIVERSIONS TO MEET DELIVERY ORDERS.'//NL//'THE FIRST NUMBER REPRESENTS THE RELAXATION APPLIED TO WHEN THERE ARE DOWNSTREAM DELIVERY ORDERS OF THE DIVERSION.'//NL//'THE SECOND IS A RELAXATION FACTOR APPLIED WHEN THERE IS NO ADDITIONAL DOWNSTREAM DEMAND OF THE DIVERSION.'
                        CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%WTFACTOR, MSG='FAILED TO LOAD AFTER KEYWORD "WEIGHT_FACTOR" THE VALUE OF WTFACTORS')
                        !
      CASE ("DISTRICT_ALLOTMENT_LIMIT","ALLOTMENT_LIMIT")

                        WRITE(BL%IOUT,'(A)') 'DISTRICT_ALLOTMENT_LIMIT (ALLOTMENT_LIMIT) KEYWORD FOUND. NOTE THIS EXPECTS TO LOAD NDIST MULTIPLIER VALUES THAT ARE MULTIPLIED BY THE DISTRICTS ALLOCATION TO DETERMINE ITS MAXIMUM ALLOTMENT AT ANY TIME. SET TO "1E30" TO NOT USE'
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(ALLOT_LIM, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR IN SWO AFTER KEWORD DISTRICT_ALLOTMENT_LIMIT (ALLOTMENT_LIMIT)')
                        !
      CASE ("DISTRICT_DELIVERY_FRACTION_TOLERANCE","DIST_DELTOL")
                        WRITE(BL%IOUT,'(A)') 'DISTRICT_DELIVERY_FRACTION_TOLERANCE (DIST_DELTOL) KEYWORD FOUND. NOTE THIS EXPECTS TO LOAD NDIST VALUES'
                        !
                        ALLOCATE(SWO%DIST_DELTOL(SWO%NDIST))
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%DIST_DELTOL, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR IN SWO AFTER KEWORD DISTRICT_DELIVERY_FRACTION_TOLERANCE')
                        !
      CASE ("ALLOCATION_BENEFICIARY_SPLIT")
                        WRITE(BL%IOUT,'(A)') 'ALLOCATION_BENEFICIARY_SPLIT KEYWORD FOUND. NOW LOADING KEYWORDS "EVEN_SPLIT", "BY_IRRIGATED_AREA", "S_LANGUAGE", OR "LIST".'
                        !'TRANSIENT/STATIC, THEN LOADING NFARM + NAUX FRACTIONS THAT INDICATE HOW A DISTRIC ALLOCATION IS SPLIT AMONG THE FARMS AND AUXDEMs.'
                        N = LLOC
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        !
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE("EVEN_SPLIT", "EVEN")
                                                 WRITE(BL%IOUT,'(A)') '   EVEN_SPLIT KEYWORD FOUND. THE DISTRICT ALLOCATION WILL BE SPLIT EVENLY AMONG THE BENEFICIARIES.'
                                                 SWO%DIST_ALLOC_FRAC_TYP = Z
                        CASE("BY_IRRIGATED_AREA")
                                                 WRITE(BL%IOUT,'(A)') "   BY_IRRIGATED_AREA KEYWORD FOUND. THE DISTRICT ALLOCATION WILL BE SPLIT BASED ON THE MODEL'S IRRIGATED AREA"
                                                 SWO%DIST_ALLOC_FRAC_TYP = ONE
                                                 !
                        CASE("LIST","TRANSIENT","STATIC", "CONSTANT")
                                                 WRITE(BL%IOUT,'(A)') '   '//LINE(ISTART:ISTOP)//' KEYWORD FOUND. NOW LOADING FRACTIONS WITH LIST_ARRAY INPUT, THEN LOADING NFARM + NAUX FRACTIONS THAT INDICATE HOW A DISTRIC ALLOCATION IS SPLIT AMONG THE FARMS AND AUXDEMs.'
                                                 SWO%DIST_ALLOC_FRAC_TYP = TWO
                                                 LLOC = N
                                                 CALL SWO%DIST_ALLOC_FRAC%INIT('DIST_ALLOC_FRAC',  LLOC, LINE, BL%IOUT, BL%IU, SWO%NFARM+SWO%NAUXDEM, ONE, Z, Z, SCRATCH=BL%SCRATCH)
                        CASE("S_LANGUAGE")
                                                 WRITE(BL%IOUT,'(A)') '   S_LANGUAGE KEYWORD FOUND. THE BENIFICIARY FRACTION OF THE DISTRICT ALLOCATION WILL BE DETERMINED BY THE S-LANGAUGE RULES WITH THE VARIABLE NAMES FARM.ID.ALLOC_FRAC'
                                                 SWO%DIST_ALLOC_FRAC_TYP = THREE
                        CASE DEFAULT
                                          CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FOUND KEYWORD "BY_IRRIGATED_AREA", "EVEN_SPLIT", OR LIST-ARRAY INPUT FLAGS ("STATIC", "TRANSIENT", "LIST").')
                        END SELECT
                        !
      CASE ("RESERVOIR_INITIAL_STORAGE","INITIAL_STORAGE")
                        WRITE(BL%IOUT,'(A)') 'INITIAL_RESERVOIR_STORAGE KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' INITIAL RESERVOIR STORAGE VOLUMES WITH ULOAD.).'
                        !
                        CALL LOAD_VOL_STAGE_TSF(SWO, SWO%NRES_BAL, RES_INITIAL_STORAGE, LINE, LLOC, BL%IU, BL%SCRATCH, DATE_SP(1)%TS(0), RES_START, STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_INITIAL_STORAGE')
                        !
                        NO_INITIAL_STORAGE = FALSE   !FLAG TO SHOW THAT KEYWORD IS FOUND
                        !IS_TIME_SERIES     = FALSE   !FLAG TO INDICATE A TIME SERIES FILE IS LOOKED UP TO GET STARTING STORAGE
                        !!
                        !DO I=ONE, TWO  ! COLLECT KEYWORDS
                        !    !
                        !    N = LLOC
                        !    CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                        !    !
                        !    SELECT CASE ( LINE(ISTART:ISTOP) )
                        !    CASE("BY_VOLUME", "BYVOLUME")
                        !                             WRITE(BL%IOUT,'(A)') '   BY_VOLUME KEYWORD FOUND. THE INITIAL STORAGE IS SPECIFIED AS A VOLUME IN MODEL UNITS'
                        !                             !
                        !    CASE("BY_STAGE","BY_ELEVATION","BY_ELEV","BYELEVATION","BYELEV")
                        !                             WRITE(BL%IOUT,'(A)') '   BY_ELEVATION (or BY_STAGE) KEYWORD FOUND. THE INITIAL STORAGE IS SPECIFIED AS A STAGE ELEVATION (IN MODEL UNITS) AND CONVERTED TO A VOLUME FROM THE ELEVATION_AREA_CAPACITY_TABLE.'
                        !                             INIT_AS_ELEV = TRUE
                        !                             !
                        !    CASE("TIME_SERIES","TIME_SERIES_FILE","TIME_SERIES_FILES")
                        !                             WRITE(BL%IOUT,'(A)') '   TIME_SERIES KEYWORD FOUND. THE INITIAL STORAGE WILL BE PULLED FROM THE LOADED TIME SERIES FILE AND THE INITIAL MODEL STARTING DATE.'
                        !                             IS_TIME_SERIES = TRUE
                        !    CASE DEFAULT
                        !                             IF(I==ONE .OR. IS_TIME_SERIES) WRITE(BL%IOUT,'(A)') '   NO SPECIFIED BY_VOLUME OR BY_STAGE KEYWORD FOUND, ASSUMING ITS NOT SPECIFIED AND THE INITIAL STORAGE WILL BE LOADED AS IF IT IS SPECIFIED AS A VOLUME IN MODEL UNITS'
                        !                             LLOC = N
                        !                             IF(I==ONE) EXIT
                        !    END SELECT
                        !END DO
                        !!
                        !I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !!
                        !IF(IS_TIME_SERIES) THEN
                        !                        ALLOCATE(  RES_INITIAL_STORAGE_TSF(NRES_BAL) )
                        !                        !
                        !                        CALL ULOAD(RES_INITIAL_STORAGE_TSF, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP) !USE STEP FUNCTION BY DEFAULT IF NOT SPECIFIED
                        !                        !
                        !                        DO K = ONE, NRES_BAL                      !STARTING DATE
                        !                              CALL RES_INITIAL_STORAGE_TSF(K)%GET(DATE_SP(1)%TS(0),RES_INITIAL_STORAGE(K))
                        !                        END DO
                        !                        DEALLOCATE(RES_INITIAL_STORAGE_TSF)
                        !ELSE
                        !                        CALL ULOAD(RES_INITIAL_STORAGE, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH)
                        !END IF
                        !
      CASE ("RESERVOIR_MAXIMUM_CAPACITY","MAXIMUM_CAPACITY")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_MAXIMUM_CAPACITY (MAX_STORAGE) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' MAXIMUM STORAGE FOR EACH RESERVOIR WITH ULOAD (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        NO_MAX_STORAGE = FALSE
                        !
                        CALL LOAD_VOL_STAGE_TSF(SWO, SWO%NRES_BAL, STORAGE_CAPACITY, LINE, LLOC, BL%IU, BL%SCRATCH, DATE_SP(1)%TS(0), RES_START, STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_MAXIMUM_CAPACITY')
                        !
                        !I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !!
                        !CALL ULOAD(STORAGE_CAPACITY, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH)
                        !
                        !
      CASE ("RESERVOIR_SPILLWAY_STORAGE","SPILLWAY_STORAGE")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_SPILLWAY_STORAGE (SPILLWAY_STORAGE) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' MAXIMUM STORAGE FOR EACH RESERVOIR WITH ULOAD (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        CALL LOAD_VOL_STAGE_TSF(SWO, SWO%NRES_BAL, STORAGE_SPILL, LINE, LLOC, BL%IU, BL%SCRATCH, DATE_SP(1)%TS(0), RES_START, STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_SPILLWAY_STORAGE')
                        !!I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !!!
                        !!CALL ULOAD(STORAGE_SPILL, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH)
                        !
      CASE ("RESERVOIR_DEADPOOL_STORAGE","RESERVOIR_DEAD_POOL_STORAGE")
                        !
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_DEAD_POOL_STORAGE KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' TIME SERIES FILES WITH ULOAD.'
                        !
                        NO_STORAGE_DPL = FALSE
                        CALL LOAD_VOL_STAGE_TSF(SWO, SWO%NRES_BAL, STORAGE_DPL, LINE, LLOC, BL%IU, BL%SCRATCH, DATE_SP(1)%TS(0), RES_START, STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_DEADPOOL_STORAGE')
                        !
                        !I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !!
                        !CALL ULOAD(STORAGE_DPL, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP)
                        !
      CASE ("RESERVOIR_SPILLWAY_PREFERENCE","SPILLWAY_PREFERENCE")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_SPILLWAY_STORAGE (SPILLWAY_STORAGE) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' MAXIMUM STORAGE FOR EACH RESERVOIR WITH ULOAD (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SPILL_PREF, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_SPILLWAY_PREFERENCE')
                        !
      CASE ("RESERVOIR_SPILLWAY_DISCHARGE_TABLE","SPILLWAY_DISCHARGE_TABLE") !MAX SPILL LOOK UP TABLE
                        NO_MAX_SPILL = FALSE
                        !
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_SPILLWAY_FLOW_LIMIT (SPILLWAY_FLOW_LIMIT) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' FILES WITH ULOAD THAT CONTAIN STORAGE AND MAX_FLOW_RATE RELATIONSHIP FOR THE SPILLWAY FOR EACH RESERVOIR (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,CUSTOM,TRUE)
                        !
                        IMAX = Z ! FLAG TO INDICATE BYVOLUME
                        SELECT CASE ( CUSTOM )
                        CASE("BY_VOLUME", "BYVOLUME")
                                                 WRITE(SWO%IOUT,'(A)') '   BY_VOLUME KEYWORD FOUND INPUT SPECIFIED AS A VOLUME IN MODEL UNITS'
                                                 !
                        CASE("BY_STAGE","BY_ELEVATION","BY_ELEV","BYELEVATION","BYELEV","ELEVATION","STAGE")
                                                 WRITE(SWO%IOUT,'(A)') '   BY_ELEVATION (or BY_STAGE) KEYWORD FOUND INPUT IS SPECIFIED AS A STAGE ELEVATION (IN MODEL UNITS) AND CONVERTED TO A VOLUME FROM THE ELEVATION_AREA_CAPACITY_TABLE.'
                                                 !
                                                 IF(ANY(SWO%NRES_SPT > Z))  CALL STOP_ERROR( LINE, BL%IU, SWO%IOUT, MSG='RESERVOIR_SPILLWAY_FLOW_LIMIT KEYWORD FOUND, BUT THIS INPUT FORMAT IS NOT ALLOWED FOR SPLIT RESERVOIR SYSTEMS. THERE IS AT LEAST ONE PROJECT THAT HAS "PROJECT_RESERVOIR_COUNT" WITH AN NEGATIVE COUNT. PLEASE EITHER SPECIFY PROPERTY WITH "BY_VOLUME" OPTION INSTEAD.')
                                                 !
                                                 IMAX = ONE
                        CASE DEFAULT
                                                 WRITE(SWO%IOUT,'(A)') '   "BY_VOLUME" OR "BY_ELEVATION" KEYWORD NOT FOUND. ASSUMING "BY_VOLUME" AND INPUT WILL BE LOADED AS A VOLUME IN MODEL UNITS'
                                                 LLOC = ISTART
                        END SELECT
                        !
                        ALLOCATE(FILES(NRES_BAL))
                        !
                        I = Z
                        CALL ULOAD(FILES, LLOC, LINE, BL%IOUT, BL%IU, I, SFAC=SFAC, EX1_WORD='STORAGE', EX1_DIM=ONE, EX2_WORD='DISCHARGE', EX2_DIM=ONE, EX3_WORD='ELEVATION', EX3_DIM=ONE, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_SPILLWAY_DISCHARGE_TABLE')
                        !
                        K=Z
                        DO I = ONE, SWO%NPROJ
                              DO J = ONE, SWO%NRES_BAL(I)
                                                        K = K + ONE
                                                        CALL SWO_READ_MAX_SPILL( SWO, FILES(K), LINE, SWO%RESDAT(I)%MAX_SPILL(J), I, J, SFAC)
                              END DO
                        END DO
                        !
                        IF(IMAX == ONE) THEN
                           CUSTOM=NL
                           DO I = ONE, SWO%NPROJ
                           DO J = ONE, SWO%NRES_BAL(I)
                           IF(SWO%RESDAT(I)%MAX_SPILL(J)%M == TWO) THEN !%M = 1 if CONSTANT or SKIP
                               !
                               ASSOCIATE( DIM=>SWO%RESDAT(I)%MAX_SPILL(J)%N, STOR=> SWO%RESDAT(I)%MAX_SPILL(J)%MAT(:,ONE) )
                                  DO K = ONE, DIM
                                                              TMP1 = STOR(K)
                                                              CALL ACAP_ELEV2STOR(SWO,I,J,TMP1,STOR(K))
                                  END DO
                               END ASSOCIATE
                           END IF
                           END DO
                           END DO
                           IF(CUSTOM.NE.NL) CALL STOP_ERROR( BLNK, BL%IU, BL%IOUT, MSG= 'SWO BLOCK ERROR: KEYWORD "RESERVOIR_SPILLWAY_FLOW_LIMIT" HAD "BY_ELEVATION" OPTION'//NL//'WHICH INDICATES THAT THE FIRST COLUMN OF DATA IS A STAGE/ELEVATION,'//NL//'BUT SWO FAILED TO CONVERT THE ELEVATION TO A STORGAGE FOR THE GIVEN REASONS:'//CUSTOM)
                        END IF
                        DEALLOCATE(FILES)
                        !
      CASE ("RESERVOIR_MAXIMUM_ELEVATION_CHANGE","MAXIMUM_ELEVATION_CHANGE")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_MAXIMUM_ELEVATION_CHANGE (MAXIMUM_ELEVATION_CHANGE) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' MAXIMUM STORAGE FOR EACH RESERVOIR WITH ULOAD (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        NO_ELEV_CHNG = FALSE
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(ELEV_CHNG, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_MAXIMUM_ELEVATION_CHANGE')
                        !
      CASE ("RESERVOIR_MAXIMUM_AREA","MAXIMUM_AREA")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_MAXIMUM_AREA (MAXIMUM_AREA) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' MAXIMUM STORAGE FOR EACH RESERVOIR WITH ULOAD (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX AREA THAT REPRESENTS THE LUMPED TOTAL MAX POOLED AREA).'
                        !
                        NO_MAX_AREA = FALSE
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(MAX_AREA, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_MAXIMUM_AREA')
                        !
      CASE ("RESERVOIR_MAXIMUM_RELEASE","MAXIMUM_RELEASE")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_MAXIMUM_RELEASE (MAXIMUM_RELEASE) KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' MAXIMUM RELEASE (L^3/T) FOR EACH RESERVOIR WITH ULOAD (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX RELEASE HERE THAT REPRESENTS THE LUMPED RELEASE FOR THE POOLED STORAGE).'
                        !
                        NO_MAX_RELEASE = FALSE
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(MAX_RELEASE, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_MAXIMUM_RELEASE')
                        !
      CASE ("RESERVOIR_PROJECT_RELEASE_FRACTION","RELEASE_FRACTION")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_PROJECT_RELEASE_FRACTION (RELEASE_FRACTION) KEYWORD FOUND. NOW LOADING KEYWORDS STATIC OR TRANSIENT FOLLOWED BY LIST INPUT OF '//STR_NRES_BAL//' FRACTIONS THAT REPRESENT THE AMMOUNT OF TOTAL DEMANDED RELEASE THAT IS RELEASED FROM EACH RESERVOIR (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'//NL//'THIS INPUT CAN BE OVERRIDEN WITH THE S VARIABLE "PROJ.ID.RELEASE_FRAC_RES.ID"'
                        !
                        NO_RELEASE_FRAC = FALSE
                        !
                        N = LLOC
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        !
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE("EVEN_SPLIT", "EVEN")
                                                 WRITE(BL%IOUT,'(A)') '   EVEN_SPLIT KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED EVENLY ACROSS RESERVOIRS WITHIN THE SAME PROJECT'
                                                 CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC',  DNEG, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_POTENTIAL_RELEASE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_POTENTIAL_RELEASE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC', -2.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_AVAILABLE_RELEASE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_AVAILABLE_RELEASE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC', -3.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_STORAGE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_STORAGE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC', -4.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_USABLE_STORAGE","BY_USEABLE_STORAGE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_USABLE_STORAGE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC', -5.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE DEFAULT
                                                 LLOC = N
                                                 CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC',  LLOC, LINE, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z, SCRATCH=BL%SCRATCH)
                        END SELECT
                        !
      CASE ("RESERVOIR_RELEASE_REQUIRED_FLOW_FRACTION","RELEASE_REQUIRED_FLOW_FRACTION")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_RELEASE_REQUIRED_FLOW_FRACTION KEYWORD FOUND. NOW LOADING EITHER KEYWORD EVEN_SPLIT OR BY_POTENTIAL OR BY_DEMAND_FRACTION OR LIST ARRAY INPUT KEYWORDS STATIC/TRANSIENT FOLLOWED BY LIST INPUT OF '//STR_NRES_BAL//' FRACTIONS THAT REPRESENT THE AMMOUNT OF REQUIRED RELEASE THAT IS PROTRATED ACROSS RESERVOIRS TO MAINTAIN REQUIRED FLOWS (NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        NO_REQ_REL_FRAC = FALSE
                        !
                        N = LLOC
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        !
                        SWO%REQ_FRAC_USES_DMD_FRAC = FALSE
                        !
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE("EVEN_SPLIT", "EVEN")
                                                 WRITE(BL%IOUT,'(A)') '   EVEN_SPLIT KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED EVENLY ACROSS RESERVOIRS WITHIN THE SAME PROJECT'
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC',  DNEG, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_POTENTIAL_RELEASE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_POTENTIAL_RELEASE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC', -2.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_AVAILABLE_RELEASE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_AVAILABLE_RELEASE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC', -3.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_STORAGE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_STORAGE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC', -4.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_USABLE_STORAGE","BY_USEABLE_STORAGE")
                                                 WRITE(BL%IOUT,'(A)') '   BY_USABLE_STORAGE KEYWORD FOUND. REQUIRED RELEASES WILL BE APPLIED BASED ON THE RATIO OF THE USUALABLE STORAGE'
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC', -5.D0, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                        CASE("BY_PROJECT_RELEASE_FRACTION","BY_RELEASE_FRACTION")
                                                 WRITE(BL%IOUT,'(A)') '   BY_RELEASE_DEMAND_FRACTION KEYWORD FOUND. REQUIRED RELEASES WILL USE THE VALUES DEFINED BY "RESERVOIR_MAXIMUM_RELEASE" KEYWORD OR ITs S COUNTERPART'
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC',DZ, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
                                                 SWO%REQ_FRAC_USES_DMD_FRAC = TRUE
                        CASE DEFAULT
                                                 LLOC = N
                                                 CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC',  LLOC, LINE, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z, SCRATCH=BL%SCRATCH)
                        END SELECT
                        !
      CASE ("RESERVOIR_PRECIPITATION_AREA_FRACTION","PRECIPITATION_AREA_FRACTION")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_PRECIPITATION_AREA_FRACTION KEYWORD FOUND. NOW LOADING KEYWORDS STATIC OR TRANSIENT FOLLOWED BY LIST INPUT OF '//STR_NRES_BAL//' FRACTIONS BETWEEN 0 AND 1 THAT REPRESENT THE AMMOUNT OF AREA THAT PRECIPTIATION FALLS ON AND REACHES THE RESERVOIR. A VALUE OF 1 INDICATES THAT THE PRECPITATION FALLS ON THE MAXIMUM AREA OF THE RESERVOIR, WHILE A VALUE OF 0 INDICATES THAT THE PRECIPITATION ONLY FALLS ON THE CURRENT AREA OF THE RESERVOIR. VALUES BETWEEN 0 AND 1 REPRESENTS THE AN AREA BETWEEN THE ACTUAL AND MAX AREA.(NOTE SPLIT RESERVOIRS ONLY LOAD ONE MAX STORAGE THAT REPRESENTS THE LUMPED TOTAL MAX POOLED STORAGE).'
                        !
                        CALL SWO%PRECIP_AREA_FRAC%INIT('PRECIP_AREA',  LLOC, LINE, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z, SCRATCH=BL%SCRATCH)
                        !
      CASE ("MINIMUM_PROJECT_DIVERSION_ORDER","MIN_PROJ_ORDER")
                        WRITE(BL%IOUT,'(A)') 'MINIMUM_PROJECT_DIVERSION_ORDER (MIN_PROJ_ORDER) KEYWORD FOUND. NOW LOADING NPROJ PROJECT MINIMUM DIVERSION ORDERS. ANY TOTAL DIVERSION REQUESTS FOR THE PROJECT LESS THEN THIS AMMOUNT IS REDUCE TO ZERO (ASSUMED PROJECT REQUESTS TOO LITTLE WATER SO IT GETS ZERO).'
                        !
                        ALLOCATE(SWO%MIN_PROJ_ORDER_RAT(SWO%NPROJ))
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%MIN_PROJ_ORDER_RAT, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD MINIMUM_PROJECT_DIVERSION_ORDER')
                        !
      CASE ("MINIMUM_BENEFICIARY_DELIVERY_ORDER")
                        WRITE(BL%IOUT,'(A)') 'MINIMUM_BENEFICIARY_DELIVERY_ORDER KEYWORD FOUND. LOADING NFARM + NAUX MINIMUM DELIVERY ORDERS.'
                        !
                        ALLOCATE(SWO%MIN_DELORDER_RAT( SWO%NFARM+SWO%NAUXDEM ) )
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%MIN_DELORDER_RAT, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD MINIMUM_BENEFICIARY_DELIVERY_ORDER')
                        !
      CASE ("RESERVOIR_NAME","RESERVOIR_NAMES")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_NAME           KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' SIMULATED EXPLICITLY RESERVOIR NAMES (NOTE SPLIT RESERVOIRS ONLY LOAD ONE NAME HERE THAT REPRESENTS THE LUMPED STORAGES NAME).'
                        !
                        IF(HAS_RESNAM) THEN
                                          I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                                          CALL ULOAD(RESNAM, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, ENTIRE_LINE=TRUE)
                        ELSE
                            CALL STOP_ERROR( LINE,BL%IU,BL%IOUT, MSG= 'SWO BLOCK ERROR: KEYWORD "RESERVOIR_NAME" REQUIRES THAT THERE BE AT LEAST ONE EXPLICITLY SIMULATED RESERVOIR TO DECLAIRE ITS NAME. PLEASE REMOVE KEYWORD OR DEFINE AT LESS ONE RESERVOIR.')
                        END IF
                        IF(LLOC<Z) CALL WARN_MSG2%ADD('FOUND KEYWORD "'//LINE(ISTART:ISTOP)//'"'//NL//', WHICH INDICATES THAT NRES NAMES ARE LOADED BUT WHEN LOADING THE NAMES ONE OF THEM WAS CLIPPED (IT WAS LONGER THAN 30 CHARACTERS).'//NL//'THIS SHOULD HAVE NO EFFECT ON SIMULATION OTHER THAN THE TRUNCATED NAME BEING PRINTED.'//BLN)
                        !
      CASE ("SPLIT_RESERVOIR_NAME","SPLIT_RESERVOIR_NAMES")
                        WRITE(BL%IOUT,'(A)') 'SPLIT_RESERVOIR_NAME     KEYWORD FOUND. NOW LOADING '//NUM2STR(NRES_SPT)//' SIMULATED AS FRACTION OF LUMPED POOL (LUMPED/SPLIT) RESERVOIR NAMES (NOTE THIS ONLY READS SPLIT RESERVOIRS IN PROJECT ORDER -- PROJECTS WITHOUT SPLIT RESIVORS ARE NOT READ.).'
                        !
                        IF(HAS_SPLITNAM) THEN
                                          I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                                          CALL ULOAD(SPLITNAM, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, ENTIRE_LINE=TRUE, MSG='ERROR SOMETIME AFTER SWO KEYWORD SPLIT_RESERVOIR_NAME')
                        ELSE
                            CALL STOP_ERROR( LINE,BL%IU,BL%IOUT, MSG= 'SWO BLOCK ERROR: KEYWORD "SPLIT_RESERVOIR_NAME" REQUIRES THAT THERE BE AT LEAST ONE EXPLICITLY SIMULATED RESERVOIR TO DECLAIRE ITS NAME. PLEASE REMOVE KEYWORD OR DEFINE AT LESS ONE RESERVOIR.')
                        END IF
                        !
      CASE ("SFR_SEG_TYPE", "SFR_SEGMENT_TYPE")
                        WRITE(BL%IOUT,'(A)') 'SFR_SEGMENT_TYPE         KEYWORD FOUND.'
                        CALL SWO%SEG_TYP%INIT('SFR_SEG_TYP',  LLOC, LINE, BL%IOUT, BL%IU, NSEG, ONE, Z, Z, SCRATCH=BL%SCRATCH)
                        !
      CASE ("RESERVOIR_RELEASE_SEGMENT","RELEASE_SEGMENT")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_RELEASE_SEGMENT KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' RESERVOIR RELEASE SFR SEGMENTS WITH LIST-ARRAY INPUT (ONLY LIST FORMAT ACCEPTED).'
                        CALL SWO%RESBAL_RELSEG%INIT('RESBAL_RELSEG',  LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, DIM=[NRES_BAL], WILD_IN=SWO%SFR_ID)
                        !
      CASE ("RESERVOIR_SFR_INFLOW_SEGMENT","SFR_INFLOW_SEGMENT")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_SFR_INFLOW_SEGMENT KEYWORD FOUND. NOW LOADING '//STR_NRES_BAL//' RESERVOIR SFR SEGMENTS, WHOSE OUTFLOW BECOMES INFLOW TO THE RESPECTIVE RESERVOIR, WITH LIST-ARRAY INPUT (ONLY LIST FORMAT ACCEPTED).'
                        CALL SWO%RESBAL_INFLOW_SEG%INIT('SFR_INFLOW_SEG',  LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, DIM=[NRES_BAL], WILD_IN=SWO%SFR_ID)
                        !
      CASE ("TIME_SERIES_RESERVOIR_INFLOW")
                        WRITE(BL%IOUT,'(A)') 'TIME_SERIES_RESERVOIR_INFLOW KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' BALANCE RESERVOIR TIME SERIES FILES WITH ULOAD.'
                        !
                        !N = LLOC
                        !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                        !!
                        !SELECT CASE ( LINE(ISTART:ISTOP) )
                        !CASE("SIMTIME");  SWO%RES_INFLOW_SIMTIM = TRUE
                        !CASE("REALTIME"); SWO%RES_INFLOW_SIMTIM = FALSE
                        !CASE DEFAULT
                        !                  LLOC = N
                        !                  SWO%RES_INFLOW_SIMTIM = FALSE
                        !                  CALL WARNING_MESSAGE(BLNK,BL%IU,BL%IOUT,'FOUND KEYWORD "RESERVOIR_INFLOW"'//NL//'THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME" OR "REALTIM".'//BLN//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN//'THIS MAY RESULT IN A ULOAD CRASH IF YOU MIS-SPELLED THE KEYWORD'//NL//'AS IT WILL ATTEMPT TO LOAD FROM THE FOLLOWING PART OF THE LINE:'//BLN//TRIM(LINE(LLOC:))//BLN, TRUE)
                        !                  !CALL WARN_MSG2%ADD('FOUND KEYWORD "RESERVOIR_INFLOW" THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME".'//NL//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN)
                        !END SELECT
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%RES_INFLOW, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD TIME_SERIES_RESERVOIR_INFLOW')
                        !
      CASE ("TIME_SERIES_RESERVOIR_PRECIPITATION","TIME_SERIES_PRECIPITATION")
                        WRITE(BL%IOUT,'(A)') 'TIME_SERIES_RESERVOIR_PRECIPITATION KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' TIME SERIES FILES WITH ULOAD.'
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%RES_PRECIP, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD TIME_SERIES_RESERVOIR_PRECIPITATION')
                        !
      CASE ("TIME_SERIES_RESERVOIR_EVAPORATION","TIME_SERIES_EVAPORATION")
                        WRITE(BL%IOUT,'(A)') 'TIME_SERIES_RESERVOIR_EVAPORATION KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' TIME SERIES FILES WITH ULOAD.'
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%RES_EVAP, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD TIME_SERIES_RESERVOIR_EVAPORATION')
                        !
      CASE ("TIME_SERIES_RESERVOIR_SPECIFIED_RELEASE","TIME_SERIES_SPECIFIED_RELEASE")
                        WRITE(BL%IOUT,'(A)') 'TIME_SERIES_RESERVOIR_SPECIFIED_RELEASE KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' TIME SERIES FILES WITH ULOAD.'
                        !
                        !N = LLOC
                        !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                        !!
                        !SELECT CASE ( LINE(ISTART:ISTOP) )
                        !CASE("SIMTIME");  SWO%RES_RELEASE_SPEC_SIMTIM = TRUE
                        !CASE("REALTIME"); SWO%RES_RELEASE_SPEC_SIMTIM = FALSE
                        !CASE DEFAULT
                        !                  LLOC = ISTART
                        !                  SWO%RES_RELEASE_SPEC_SIMTIM = FALSE
                        !                  CALL WARNING_MESSAGE(BLNK,BL%IU,BL%IOUT,'FOUND KEYWORD "RESERVOIR_NON_PROJECT_RELEASE" '//NL//'THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME" OR "REALTIM".'//BLN//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN//'THIS MAY RESULT IN A ULOAD CRASH IF YOU MIS-SPELLED THE KEYWORD'//NL//'AS IT WILL ATTEMPT TO LOAD FROM THE FOLLOWING PART OF THE LINE:'//BLN//TRIM(LINE(LLOC:))//BLN, TRUE)
                        !                  !CALL WARN_MSG2%ADD('FOUND KEYWORD "RESERVOIR_NON_PROJECT_RELEASE" THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME".'//NL//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN)
                        !END SELECT
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%RES_RELEASE_SPEC, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD TIME_SERIES_RESERVOIR_SPECIFIED_RELEASE')
                        !
      CASE ("TIME_SERIES_RESERVOIR_MINIMUM_STORAGE","TIME_SERIES_MINIMUM_STORAGE")
                        !
                        WRITE(BL%IOUT,'(A)') 'TIME_SERIES_RESERVOIR_MINIMUM_STORAGE KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' TIME SERIES FILES WITH ULOAD.'
                        !
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%RES_STORAGE_MIN, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD TIME_SERIES_RESERVOIR_MINIMUM_STORAGE')
                        !
      CASE ("TIME_SERIES_RESERVOIR_MINIMUM_RELEASE","TIME_SERIES_MINIMUM_RELEASE")
                        !
                        WRITE(BL%IOUT,'(A)') 'TIME_SERIES_RESERVOIR_MINIMUM_RELEASE KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_BALTOT)//' TIME SERIES FILES WITH ULOAD.'
                        !
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%RES_RELEASE_MIN, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD TIME_SERIES_RESERVOIR_MINIMUM_RELEASE')
                        !
      CASE ("HEAD_BOUNDARY_CELLS","HEAD_BOUNDARY_CELL")
                        WRITE(BL%IOUT,'(A)') 'HEAD_BOUNDARY_CELLS KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_TOTTOT)//' FILES THAT CONTAIN HEAD_BOUNDARY INTEGER FLAG ARRAYS.).'
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        ALLOCATE(FILES(SWO%NRES_TOTTOT))
                        CALL ULOAD(FILES, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD HEAD_BOUNDARY_CELLS')
                        !
                        K=Z
                        DO I = ONE, SWO%NPROJ
                              DO J = ONE, SWO%NRES_TOT(I)
                                                        K = K + ONE
                                                        !
                                                        CALL SWOPS_READHEAD( SWO, FILES(K), LINE, SWO%RESDAT(I)%HEAD(J)%HEAD_NCELL,SWO%RESDAT(I)%HEAD(J)%HEAD_CELLS, I, J)
                              END DO
                        END DO
                        DEALLOCATE(FILES)
                        !
      CASE ("RESERVOIR_PRIMARY_REREGULATION_SPLIT", "REREGULATION_SPLIT")
                        WRITE(BL%IOUT,'(A)') 'RESERVOIR_PRIMARY_REREGULATION_SPLIT (REREGULATION_SPLIT) KEYWORD FOUND. NOW LOADING '//NUM2STR(SWO%NRES_TOTTOT)//' RESERVOIR FRACTIONS. USE THE WORD SKIP OR NULL FOR RESERVOIRS THAT ARE NOT PART OF A PRIMARY/REREGULATION SYSTEM.'
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        ALLOCATE(FILES(SWO%NRES_TOTTOT))
                        HAS_SPLIT_FRAC = TRUE
                        !
                        CALL ULOAD(FILES, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD RESERVOIR_SPLIT_FRACTIONS')
                        !
                        K=Z
                        DO I = ONE, SWO%NPROJ
                              DO J = ONE, SWO%NRES_TOT(I)
                                                        K = K + ONE
                                                        IF(.NOT. FILES(K)%NULL_FILE) CALL SWOPS_READFRAC(SWO,FILES(K),LINE,I,J, BL%IU)
                              END DO
                        END DO
                        DEALLOCATE(FILES)
                        !
      CASE ("AUXILIARY_SFR_DELIVERY","AUX_SFR_DELIVERY","AUXILARY_SFR_DELIVERY")
                        WRITE(BL%IOUT,'(A)') 'AUXILIARY_SFR_DELIVERY   KEYWORD FOUND. NOTE THIS EXPECTS TO LOAD NAUXDEM VALUE DELIVERY SEGMENT/REACHES.'
                        IF(SWO%NAUXDEM>Z) THEN
                          !CALL SWO%AUX_SFR_DELV%ALLOCATE(SWO%NAUXDEM)  !SET UP INITIAL ARRAYS
                          CALL SWO%AUX_SFR_DELV%INIT('AUX_SFR_DELIV',  LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, DIM = [SWO%NAUXDEM], WILD_IN=SWO%SFR_ID)
                        ELSE
                            CALL WARN_MSG2%ADD('FOUND KEYWORD "'//LINE(ISTART:ISTOP)//'" WHEN NAUXDEM FROM THE GLOBAL DIMENSION BLOCK WAS SET TO ZER0. THIS LINE WILL BE IGNORED AND NO AUXILIARY DEMANDS WILL BE APPLIED.'//BLN)
                        END IF
                        !
      CASE ("AUXILIARY_DEMAND","AUXILIARY_TIME_SERIES_DEMAND","AUXILARY_DEMAND","AUXILARY_TIME_SERIES_DEMAND")
                        WRITE(BL%IOUT,'(A)') 'AUXILIARY_TIME_SERIES_DEMAND (AUXILIARY_DEMAND) KEYWORD FOUND. NOW LOADING NAUX TIME SERIES FILES WITH ULOAD.'
                        !
                        !N = LLOC
                        !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                        !!
                        !SELECT CASE ( LINE(ISTART:ISTOP) )
                        !CASE("SIMTIME");  SWO%AUXDEM_TSF_SIMTIM = TRUE
                        !CASE("REALTIME"); SWO%AUXDEM_TSF_SIMTIM = FALSE
                        !CASE DEFAULT
                        !                  LLOC = N
                        !                  SWO%AUXDEM_TSF_SIMTIM = FALSE
                        !                  CALL WARNING_MESSAGE(BLNK,BL%IU,BL%IOUT,'FOUND KEYWORD "AUXILIARY_DEMAND" '//NL//'THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME" OR "REALTIM".'//BLN//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN//'THIS MAY RESULT IN A ULOAD CRASH IF YOU MIS-SPELLED THE KEYWORD'//NL//'AS IT WILL ATTEMPT TO LOAD FROM THE FOLLOWING PART OF THE LINE:'//BLN//TRIM(LINE(LLOC:))//BLN, TRUE)
                        !                  !CALL WARN_MSG2%ADD('FOUND KEYWORD "AUXILIARY_DEMAND" THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME".'//NL//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN)
                        !END SELECT
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%AUXDEM_TSF, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD AUXILIARY_DEMAND')
                        !
      CASE ("AUXILIARY_AREA","AUXILIARY_TIME_SERIES_AREA","AUXILARY_AREA","AUXILARY_TIME_SERIES_AREA")
                        WRITE(BL%IOUT,'(A)') 'AUXILIARY_TIME_SERIES_AREA (AUXILIARY_AREA) KEYWORD FOUND. NOW LOADING NAUX TIME SERIES FILES WITH ULOAD.'
                        !
                        !N = LLOC
                        !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                        !!
                        !SELECT CASE ( LINE(ISTART:ISTOP) )
                        !CASE("SIMTIME");  SWO%AUXAREA_TSF_SIMTIM = TRUE
                        !CASE("REALTIME"); SWO%AUXAREA_TSF_SIMTIM = FALSE
                        !CASE DEFAULT
                        !                  LLOC = N
                        !                  SWO%AUXAREA_TSF_SIMTIM = FALSE
                        !                  CALL WARNING_MESSAGE(BLNK,BL%IU,BL%IOUT,'FOUND KEYWORD "AUXILIARY_AREA" '//NL//'THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME" OR "REALTIM".'//BLN//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN//'THIS MAY RESULT IN A ULOAD CRASH IF YOU MIS-SPELLED THE KEYWORD'//NL//'AS IT WILL ATTEMPT TO LOAD FROM THE FOLLOWING PART OF THE LINE:'//BLN//TRIM(LINE(LLOC:))//BLN, TRUE)
                        !                  !CALL WARN_MSG2%ADD('FOUND KEYWORD "AUXILIARY_AREA" THAT SHOULD BE FOLLOWED BY EITHER THE KEYWORDS "SIMTIME".'//NL//'NETHER OF THESE WORDS WERE FOUND SO IT IS ASSUMED THEY WERE NOT SPECIFIED'//NL//'AND THE TIME SERIES FILE WILL USE THE "REALTIME" OPTION.'//BLN)
                        !END SELECT
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(SWO%AUXAREA_TSF, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, TEXT=STEP, MSG='ERROR SOMETIME AFTER SWO KEYWORD AUXILIARY_AREA')
                        !
!      CASE ("AUX_EFF","AUX_EFFICIENCY")
!                        WRITE(BL%IOUT,'(A)') 'AUX_EFFICIENCY           KEYWORD FOUND.'
!                        IF (SWO%NAUXDEM > Z) THEN
!                            CALL SWO%AUX_EFF%INIT('AUX_EFF',  LLOC, LINE, BL%IOUT, BL%IU, SWO%NAUXDEM, ONE, Z, Z, SCRATCH=BL%SCRATCH)
!                        ELSE
!                            CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='FMP SWO ERROR. FOUND KETYWORD "AUX_EFF" WHICH INDICATES YOU WANT TO SPECIFY AUXILIARY DEVERSION SEGMENT AND REACHES, BUT YOU DID NOT SPECIFY THE KEYWORD "NAUXDEM" IN THE GLOBAL DIMENSION BLOCK OR IT WAS SET TO ZERO. PLEASE REMOVE KEYWORD "'//LINE(ISTART:ISTOP)//'" OR SPECIFY NAUXDEM > 0')
!                        END IF
!                        !
!!      CASE ("AUX_DELIV","AUX_DELIVERY")
!!                        WRITE(BL%IOUT,'(A)') 'AUX_DELIVERY             KEYWORD FOUND.'
!!                        IF (SWO%NAUXDEM > Z) THEN
!!                            CALL SWO%AUX_DELIV%INIT('AUX_DELIV',  LLOC, LINE, BL%IOUT, BL%IU, SWO%NAUXDEM, THREE, Z, Z, SCRATCH=BL%SCRATCH)
!!                        ELSE
!!                            CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='FMP SWO ERROR. FOUND KETYWORD "AUX_EFF" WHICH INDICATES YOU WANT TO SPECIFY AUXILIARY DEVERSION SEGMENT AND REACHES, BUT YOU DID NOT SPECIFY THE KEYWORD "NAUXDEM" IN THE GLOBAL DIMENSION BLOCK OR IT WAS SET TO ZERO. PLEASE REMOVE KEYWORD "'//LINE(ISTART:ISTOP)//'" OR SPECIFY NAUXDEM > 0')
!!                        END IF
!!                        !
      CASE ("ALLOCATION_DATE")
                        WRITE(BL%IOUT,'(A)') 'ALLOCATION_DATE          KEYWORD FOUND. NOTE THIS EXPECTS TO LOAD NPROJ ALLOCATION STARTING DATES'
                        NO_ALLOCATION_DATE = FALSE
                        !
                        I=Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
                        !
                        CALL ULOAD(ALLOC_DATE, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH, MSG='ERROR SOMETIME AFTER SWO KEYWORD ALLOCATION_DATE')
                        !
                        DO I=ONE, SWO%NPROJ
                                           SWO%PROJ(I)%AllocDateFrac = ALLOC_DATE(I)%DYEAR_FRACTION()   !ALLOC_DATE(I)%DYEAR
                                           SWO%PROJ(I)%AllocDate     = ALLOC_DATE(I)
                        END DO
                        !CALL LOAD_DATE_ALLOCATION(SWO, LLOC, LINE, BL%SCRATCH, BL%IU)
                        !
      CASE ("UNIT_ACCOUNTING", "ACCOUNTING")  !CURRENTLY STATIC IN TIME
                        WRITE(BL%IOUT,'(A)') 'UNIT_ACCOUNTING          KEYWORD FOUND. NOW READING GENERIC_INPUT KEYWORD WHICH POINTS TO WHERE NUNIT CHARGE AND CREDIT INFORMATION IS PROVDIDED.'
                        CALL SWO%UNIT_ACCOUNTING%INIT('U_ACNT',  LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, NO_TRANSIENT=TRUE)
                        CALL LOAD_UNIT_CHARGE_CREDIT(SWO, LINE, SWO%UNIT_ACCOUNTING%IU, SWO%SFR_ID)
                        !
      !CASE ("TABFILES","TABFILE")
      !                  WRITE(BL%IOUT,'(A)') 'TABFILES                 KEYWORD FOUND. NOTE THIS EXPECTS TO LOAD ALL ASSOCIATED TABFILES'
      !                  CALL LOAD_SWO_TABFILES(SWO, LLOC, LINE, BL%SCRATCH, BL%IU)
      CASE ("PRINT")
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        !
                        IF( LINE(ISTART:ISTOP) == 'BY_ITERATION' ) THEN
                            SWO%HAS_BY_ITERATION = TRUE
                            BY_IT = TRUE
                            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        ELSE
                            BY_IT = FALSE
                        END IF
                        ! TS 1  SP 2
                        !
                        IF    ( LINE(ISTART:ISTOP) == 'BY_TIMESTEP' .OR. LINE(ISTART:ISTOP) == 'BYTIMESTEP' ) THEN
                            CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,CUSTOM,TRUE)
                            !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                            CALL PRNT_VAR%ADD(CUSTOM)
                            CALL PRNT_FLG%ADD(TWO)
                        !
                        ELSEIF( LINE(ISTART:ISTOP) == 'BY_STRESSPERIOD' .OR. LINE(ISTART:ISTOP) == 'BYSTRESSPERIOD' ) THEN
                            CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,CUSTOM,TRUE)
                            !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                            CALL PRNT_VAR%ADD(CUSTOM)
                            CALL PRNT_FLG%ADD(ONE)
                        !
                        ELSEIF( LINE(ISTART:ISTOP) == 'OUTPUT_VARIABLE' .OR. LINE(ISTART:ISTOP) == 'OUTPUT_VARIABLES' ) THEN
                            CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,CUSTOM,TRUE)
                            !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
                            CALL PRNT_VAR%ADD(CUSTOM)
                            CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,BL%IU, I, ERROR_VAL=ONE)
                            IF(BY_IT) THEN
                                CALL PRNT_FLG%ADD(I)
                            ELSE
                                CALL PRNT_FLG%ADD(inf_I)
                            END IF
                        ELSEIF( LINE(ISTART:ISTOP) == 'S_TRANSCRIPT' .OR. LINE(ISTART:ISTOP) == 'TRANSCRIPT' ) THEN
                            CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,CUSTOM,TRUE)
                            SELECT CASE ( CUSTOM )
                            CASE("SIMULATION");                  CALL SWO%PRT_RUL_SIM     %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                            CASE("STRESS_PERIOD");               CALL SWO%PRT_RUL_SP      %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                            CASE("TIME_STEP");                   CALL SWO%PRT_RUL_TS      %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                            CASE("ITERATION");                   CALL SWO%PRT_RUL_IT      %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                            CASE("CLOSEOUT_COMMANDS","CLOSEOUT");CALL SWO%PRT_RUL_CLOSEOUT%OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                            CASE("TIME_STEP_END");               CALL SWO%PRT_RUL_TS_END  %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                            CASE("ITERATION_END");               CALL SWO%PRT_RUL_IT_END  %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE,SPLITMAXCOUNT=21)
                            CASE DEFAULT
                                              CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FOUND KEYWORD "PRINT S_TRANSCRIPT",'//NL//'WHICH MUST BE FOLLOWED BY "SIMULATION", "STRESS_PERIOD", "TIME_STEP", "ITERATION", "ITERATION_END", "TIME_STEP_END", OR "CLOSEOUT"'//NL//'TO INDICATE WHICH OF THE S LANGAUGE COMMANDS THAT A TRANSCRIPT IS MADE OF.')
                            END SELECT
                        ELSE
                           IF(.NOT. SWO%HAS_OUTPUT) THEN
                                                         SWO%HAS_OUTPUT = TRUE
                                                         ALLOCATE(SWO%OUTPUT)
                           END IF
                           !
                           SELECT CASE ( LINE(ISTART:ISTOP) )
                           CASE("FARM");             CALL SWO%OUTPUT%FARM       %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_FARM        = BY_IT
                           CASE("AUXDEM");           CALL SWO%OUTPUT%AUXDEM     %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_AUXDEM      = BY_IT
                           CASE("UNIT");             CALL SWO%OUTPUT%UNIT       %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_UNIT        = BY_IT
                           CASE("DIST", "DISTRICT"); CALL SWO%OUTPUT%DIST       %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_DIST        = BY_IT
                           CASE("PROJ","PROJECT");   CALL SWO%OUTPUT%PROJ       %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_PROJ        = BY_IT
                           CASE("DIVSEG");           CALL SWO%OUTPUT%DivSeg     %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_DivSeg      = BY_IT
                           CASE("SPLITSEG");         CALL SWO%OUTPUT%SplitSeg   %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_SplitSeg    = BY_IT
                           CASE("STORAGE");          CALL SWO%OUTPUT%Storage    %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_Storage     = BY_IT
                           CASE("SFR");              CALL SWO%OUTPUT%SFR        %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_SFR         = BY_IT
                           CASE("CONVERGENCE");      CALL SWO%OUTPUT%Convergence%OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE); SWO%OUTPUT%IT_Convergence = BY_IT
                           !
                           CASE("CONVERSION_FACTOR_STORAGE"); CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%STOR_CNVT, MSG='FAILED TO LOAD AFTER KEYWORD "CONVERSION_FACTOR_STORAGE" THE CONVERSION FACTOR TO APPLY TO OUPUT FILES')
                           CASE("CONVERSION_FACTOR_RATE"   ); CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%FLOW_CNVT, MSG='FAILED TO LOAD AFTER KEYWORD "CONVERSION_FACTOR_RATE" THE CONVERSION FACTOR TO APPLY TO OUPUT FILES')
                           CASE("CONVERSION_FACTOR_LENGTH" ); CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%LEN_CNVT,  MSG='FAILED TO LOAD AFTER KEYWORD "CONVERSION_FACTOR_LENGTH" THE CONVERSION FACTOR TO APPLY TO OUPUT FILES')
                           CASE("CONVERSION_FACTOR_AREA"   ); CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SWO%AREA_CNVT,  MSG='FAILED TO LOAD AFTER KEYWORD "CONVERSION_FACTOR_AREA" THE CONVERSION FACTOR TO APPLY TO OUPUT FILES')
                           CASE("RESERVOIR_DATA","RESDAT", "RESDATA");
                               CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,TXT)
                               SELECT CASE ( TXT )
                               !CASE("COMPACT"           ); CALL SWO%PRT_RESDAT_CMPCT %OPEN(LINE, LLOC, BL%IOUT, BL%IU)
                               CASE("RES_BY_COLUMN",     "EXPAND","EXPANDED" ); CALL SWO%PRT_RESDAT       %OPEN(LINE, LLOC, BL%IOUT, BL%IU)
                               CASE("ALL_RELEASES","ALL_RELEASE","DETAIL","DETAILED" ); CALL SWO%PRT_RESDAT_DETAIL%OPEN(LINE, LLOC, BL%IOUT, BL%IU)
                               CASE DEFAULT
                                                           LLOC = ISTART
                                                           CALL SWO%PRT_RESDAT_CMPCT %OPEN(LINE, LLOC, BL%IOUT, BL%IU)
                               END SELECT
                           CASE DEFAULT
                                       CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FOUND KEYWORD PRINT,'//NL//'WHICH MUST BE FOLLOWED BY ONE OF THE FOLLOWING KEYWORDS TO INDICATE TYPE OF OUTPUT TO WRITE AS A CHECK:'//NL//'S_TRANSCRIPT, RESERVOIR_DATA, CONVERSION_FACTOR_STORAGE, CONVERSION_FACTOR_RATE, CONVERSION_FACTOR_LENGTH, CONVERSION_FACTOR_AREA')!FARM, AUXDEM, UNIT, DIST, PROJ, DIVSEG, SPLITSEG, STORAGE, SFR, ALLOCATION')
                           END SELECT
                        END IF
      CASE ("S_TRANSCRIPT")
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE("SIMULATION");                  CALL SWO%PRT_RUL_SIM     %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                        CASE("STRESS_PERIOD");               CALL SWO%PRT_RUL_SP      %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                        CASE("TIME_STEP");                   CALL SWO%PRT_RUL_TS      %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                        CASE("ITERATION");                   CALL SWO%PRT_RUL_IT      %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                        CASE("CLOSEOUT","CLOSEOUT_COMMANDS");CALL SWO%PRT_RUL_CLOSEOUT%OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                        CASE("TIME_STEP_END");               CALL SWO%PRT_RUL_TS_END  %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE)
                        CASE("ITERATION_END");               CALL SWO%PRT_RUL_IT_END  %OPEN(LINE, LLOC, BL%IOUT, BL%IU, NO_BINARY=TRUE,SPLITMAXCOUNT=21)
                        CASE DEFAULT
                                          CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FOUND KEYWORD "S_TRANSCRIPT",'//NL//'WHICH MUST BE FOLLOWED BY "SIMULATION", "STRESS_PERIOD", "TIME_STEP", "ITERATION", "ITERATION_END", "TIME_STEP_END", OR "CLOSEOUT"'//NL//'TO INDICATE WHICH OF THE S LANGAUGE COMMANDS THAT A TRANSCRIPT IS MADE OF.')
                        END SELECT
      CASE ("DEFINE_SWO_VARIABLE","DEFINE_SWO_VARIABLES","DEFINE_S_VARIABLE","DEFINE_S_VARIABLES","DEFINE_VARIABLE","DEFINE_VARIABLES")
                        !
                        CONTINUE
                        !
      CASE ("DECISION_RULES","DECISION_RULE","RULE","RULES","DECISION_RULZ","RULZ")
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        WRITE(BL%IOUT,'(A)') 'DECISION_RULES           KEYWORD FOUND. NOW CHECKING FOR KEYWORD "STRESS_PERIOD", "TIME_STEP", OR "ITERATION" THEN STATIC OR TRANSIENT, THEN LOADING FIRST DECISION RULE BLOCK. [NOTE THE FOLLOWING KEYWORDS WORK FOR THIS AS WELL: "DECISION_RULE","RULE", and "RULES"]'
                        !
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE("SIMULATION");               CALL SWO%DEC_RUL_SIM%INIT('RULZ_SIM',       LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE, NO_TRANSIENT=TRUE )
                        CASE("STRESS_PERIOD");            CALL SWO%DEC_RUL_SP %INIT('RULZ_SP',        LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        CASE("TIME_STEP"    );            CALL SWO%DEC_RUL_TS %INIT('RULZ_TS',        LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        CASE("ITERATION"    );            CALL SWO%DEC_RUL_IT %INIT('RULZ_IT',        LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        CASE("TIME_STEP_END");            CALL SWO%DEC_RUL_TS_END%INIT('RULZ_TS_END', LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        CASE("ITERATION_END");            CALL SWO%DEC_RUL_IT_END%INIT('RULZ_IT',        LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        CASE("CLOSEOUT","CLOSE_OUT");     CALL SWO%DEC_RUL_CLOSEOUT%INIT('CLOSEOUT',  LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        CASE DEFAULT
                                          CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FOUND KEYWORD "DECISION_RULES",'//NL//'WHICH MUST BE FOLLOWED BY "STRESS_PERIOD", "TIME_STEP", "ITERATION", OR "CLOSEOUT"'//NL//'TO INDICATE THE FREQUENCY THAT THE RULE SET IS APPLIED.')
                        END SELECT
                        !CALL SWO%DEC_RUL%INIT('RULES', LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH )
                        !CALL SWO%DEC_RUL%PREPARE_RULES()
      CASE("CLOSEOUT_COMMANDS","CLOSEOUT")
                        CALL SWO%DEC_RUL_CLOSEOUT%INIT('CLOSEOUT', LLOC, LINE, BL%IOUT, BL%IU, SCRATCH=BL%SCRATCH, BEGIN_END_OPTIONAL=TRUE )
                        !
      CASE DEFAULT
                        CALL WARN_MSG1%ADD('FOUND UNKNOWN KEYWORD "'//LINE(ISTART:ISTOP)//'" ***IT WILL BE IGNORED***'//BLN)

      END SELECT
      !
      READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
      !
    END DO
    CALL WARN_MSG1%CHECK(HED='SWO BLOCK MESSAGE:',INFILE=BL%IU,OUTPUT=BL%IOUT,INLINE=TRUE,CMD_PRINT=TRUE,TAIL=NL,INIT=TRUE)
    CALL WARN_MSG2%CHECK(HED='SWO BLOCK MESSAGE:',INFILE=BL%IU,OUTPUT=BL%IOUT,INIT=TRUE)
    !
    IF(.NOT. HAS_STARTDATE) THEN
        CALL WARN_MSG1%ADD('SWO DETERMINED THAT THE DIS PACKAGE SPECIFED A STARTING TIME WITH THE KEYWORD "STARTIME"'//NL//'WHICH INDICATES THAT LEAP YEARS ARE NOT ACCOUNTED FOR AND IT IS ASSUMED TO HAVE 365.2425 DAYS IN A YEAR.'//NL//'PLEASE NOTE THAT CALENDAR DATE REFERENCES WILL NOT WORK AS THEY ASSUME 365 OR 366 DAY YEARS TO ACCOUNT FOR LEAP YEARS. ALL CLOSE OUT CALCULATIONS WILL BE BASED ON A FRACTION OF YEAR THAT OCCURS ON A 365.2425 DAY YEAR.'//NL//'IF YOU WANT TO INCLUDE LEAP YEARS THEN USE IN THE DIS PACKAGE THE KEYWORDS "LEAPYEARS STARTIME" INSTEAD.'//BLN)
        SWO%LEAP_YEAR = FALSE
    END IF
    !
    IF(.NOT. SWO%RES_INFLOW(1)%OPENED()) THEN !.NOT. ANY(SWO%RES_INFLOW%OPENED())
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "TIME_SERIES_RESERVOIR_INFLOW" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO ZERO!!!'//NL//'[NOTE THAT "RES_INFLOW" KEYWORD WORKS AS WELL.]'//BLN)
        CALL SWO%RES_INFLOW%INIT(DZ)
    END IF
    !
    IF(.NOT. SWO%RES_PRECIP(1)%OPENED()) THEN ! .NOT. ANY(SWO%RES_PRECIP%OPENED())
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "TIME_SERIES_RESERVOIR_PRECIPITATION" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO ZERO!!!'//BLN)
        CALL SWO%RES_PRECIP%INIT(DZ)
    END IF
    !
    IF(.NOT. SWO%RES_EVAP(1)%OPENED()) THEN  !.NOT. ANY(SWO%RES_EVAP%OPENED())
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "TIME_SERIES_RESERVOIR_EVAPORATION" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO ZERO!!!'//BLN)
        CALL SWO%RES_EVAP%INIT(DZ)
    END IF
    !
    IF(.NOT. SWO%RES_RELEASE_SPEC(1)%OPENED()) THEN  !.NOT. ANY(SWO%RES_RELEASE_SPEC%OPENED())
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "TIME_SERIES_RESERVOIR_SPECIFIED_RELEASE" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO ZERO.'//BLN)
        CALL SWO%RES_RELEASE_SPEC%INIT(DZ)
    END IF
    !
    IF(NO_STORAGE_DPL) THEN
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_DEAD_POOL_STORAGE" KEYWORD,'//NL//'THIS WILL BE AUTOMATICALLY SET TO THE MINIMUM STORAGE SPECIFIED IN THE "ELEVATION_AREA_CAPACITY_TABLE"'//BLN)
    END IF
    !
    IF(.NOT. SWO%RES_STORAGE_MIN(1)%OPENED()) THEN  !.NOT. ANY(SWO%RES_STORAGE_MIN%OPENED())
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "TIME_SERIES_RESERVOIR_MINIMUM_STORAGE" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO THE DEAD POOL STORAGE'//BLN)
    END IF
    !
    IF(.NOT. SWO%RES_RELEASE_MIN(1)%OPENED()) THEN
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "TIME_SERIES_RESERVOIR_MINIMUM_RELEASE" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO 0 TO DISABLE IT.'//BLN)
        CALL SWO%RES_RELEASE_MIN%INIT( DZ )
    END IF
    !
    IF(SWO%NAUXDEM > Z) THEN
        IF(.NOT. SWO%AUXDEM_TSF(1)%OPENED()) THEN !.NOT. ANY(SWO%AUXDEM_TSF%OPENED())
            CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "AUXILIARY_DEMAND" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO ZERO!!!'//NL//'[NOTE THAT "AUX_DEMAND" KEYWORD WORKS AS WELL.]'//BLN)
            CALL SWO%AUXDEM_TSF%INIT(DZ)
        END IF
        !
        IF(.NOT. SWO%AUXAREA_TSF(1)%OPENED()) THEN !.NOT. ANY(SWO%AUXAREA_TSF%OPENED())
            CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "AUXILIARY_AREA" KEYWORD,'//NL//'. THIS WILL BE AUTOMATICALLY SET TO ONE!!!'//NL//'[NOTE THAT "AUX_AREA" KEYWORD WORKS AS WELL.]'//BLN)
            CALL SWO%AUXAREA_TSF%INIT(UNO)
        END IF
    END IF
    !
    CALL WARN_MSG2%CHECK(HED='*** IMPORTANT SWO BLOCK MESSAGES ***'//NL,INFILE=BL%IU,OUTPUT=BL%IOUT,INIT=TRUE)
    !
    ! CHECK FOR MISSING KEYWORDS THAT ARE NOT FATAL
    !
    IF(SWO%SWMXITER(ONE) < Z) THEN
                                  !
                                  SWO%SWMXITER(ONE)   = MXITER*TWO
                                  SWO%SWMXITER(TWO)   = FIVE
                                  SWO%SWMXITER(THREE) = MXITER*THREE
                                  !IF    (MXITER < 50) THEN
                                  !    SWO%SWMXITER(ONE)   = 51
                                  !    SWO%SWMXITER(TWO)   = ONE
                                  !    SWO%SWMXITER(THREE) = 51
                                  !ELSEIF(MXITER < 100) THEN
                                  !   SWO%SWMXITER(ONE)   = 50
                                  !   SWO%SWMXITER(TWO)   = FIVE
                                  !   IF(MXITER < 70) THEN
                                  !       SWO%SWMXITER(THREE) = 70
                                  !   ELSE
                                  !       SWO%SWMXITER(THREE) = MXITER - 10
                                  !   ENDIF
                                  !ELSE
                                  !   SWO%SWMXITER(ONE)   = MXITER/2
                                  !   SWO%SWMXITER(TWO)   = FIVE
                                  !   SWO%SWMXITER(THREE) = (MXITER*FOUR)/FIVE
                                  !END IF
                                  !CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "ITERATION_THRESHOLDS" KEYWORD,'//NL//'WHICH SPECIFIES THE NUMBER OF ITERATIONS THAT SWO SETS ALLOCATIONS AND ADJUSTS DIVERSIONS (SWMXITER).'//BLN//'THEY WILL BE SET TO '//NUM2STR(SWO%SWMXITER(ONE))//', '//NUM2STR(SWO%SWMXITER(TWO))//', '//NUM2STR(SWO%SWMXITER(THREE))//'.'//BLN//'THIS MEANS THAT SWO WILL OPERATE/DETERMINE RELEASES FOR THE FIRST '//NUM2STR(SWO%SWMXITER(ONE))//' OUTER ITERATIONS (NONLINEAR-SOLVER INTERATIONS), THEN AFTER '//NUM2STR(SWO%SWMXITER(ONE))//' ITERATIONS IT WILL OPERATE EVER '//NUM2STR(SWO%SWMXITER(TWO))//' ITERATIONS UNTIL THE '//NUM2STR(SWO%SWMXITER(THREE))//' ITERATION AND THEN RELEASES WILL REMAIN FIXED UNTIL CONVERGENCE.'//NL//'[NOTE THAT "SWMXITER" KEYWORD WORKS AS WELL.]'//BLN)
    END IF
    !
    IF(SWO%ABS_CNVG(ONE) < DZ) SWO%ABS_CNVG = UNO
    IF(SWO%REL_CNVG(ONE) < DZ) SWO%REL_CNVG = 0.0025D0
    !
    IF(SWO%WTFACTOR(ONE) <DZ) THEN
                                  !CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "WEIGHT_FACTOR" KEYWORD,'//NL//'WHICH SPECIFIES THE RELAXATION FACTOR IMPOSED ON CHANGES IN DIVERSIONS TO MEET DELIVERY ORDERS.'//NL//'THE FIRST NUMBER REPRESENTS THE RELAXATION APPLIED TO WHEN THERE ARE DOWNSTREAM DELIVERY ORDERS OF THE DIVERSION.'//NL//'THE SECOND IS A RELAXATION FACTOR APPLIED WHEN THERE IS NO ADDITIONAL DOWNSTREAM DEMAND OF THE DIVERSION.'//BLN//'THE TWO RELAXATION FACTORS WILL BE SET TO A DEFAULT VALUE OF 0.25 and 0.75.'//BLN//'[NOTE THAT "WTFACTOR" KEYWORD WORKS AS WELL.]'//BLN)
                                  SWO%WTFACTOR(ONE) = FOURTH
                                  SWO%WTFACTOR(TWO) = 0.75D0
    END IF
    !
    IF(NO_MAX_RELEASE) THEN
                                  CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_MAXIMUM_RELEASE" KEYWORD,'//NL//'WHICH SPECIFIES THE MAXIMUM RELEASE FOR EACH RESERVOIR.'//NL//'ONLY ONE VALUE PER RESERVOIR (NOTE SPLIT RESERVOIRS ARE TREATED AS ONE RESERVOIR SINCE THEY HAVE ONE RELEASE POINT AND LUMPED STORAGE).'//BLN//'NO MAXIMUM STRUCTURAL RELEASE WILL BE IMPOSED ON THE RESERVOIRS.'//BLN//'[NOTE THAT "MAXIMUM_RELEASE" KEYWORD WORKS AS WELL.]'//BLN)
                                  MAX_RELEASE = inf
    END IF
    !
    IF(.NOT.ALLOCATED(SWO%DIST_DELTOL)) THEN
                                  CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "DISTRICT_DELIVERY_FRACTION_TOLERANCE" KEYWORD,'//NL//'WHICH SPECIFIES NDIST FRACTIONS THAT REPRESENT THAT RATION OF DELIVERY/ORDER THAT MAKES THE ORDER ZERO'//NL//'BECAUSE THE DELIVERY CANNOT BE MADE.'//BLN//'IT WILL BE SET TO A DEFAULT VALUE OF 0.5'//BLN//'[NOTE THAT "DIST_DELTOL" KEYWORD WORKS AS WELL.]'//BLN)
                                  ALLOCATE(SWO%DIST_DELTOL(SWO%NDIST), SOURCE=FIFTH)
    END IF
    !
    IF(.NOT.ALLOCATED(SWO%MIN_DELORDER_RAT)) THEN
                                  CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "MINIMUM_BENEFICIARY_DELIVERY_ORDER" KEYWORD,'//NL//'WHICH SPECIFIES NFARM+NAUX MINIMUM DELIVERY ORDER SIZE BEFORE THE DELIVERY IS REFUSED (SET TO ZERO). THIS IS FOR DETERMINING WHEN THE REQUESTED WATER IS TOO SMALL TO BE WORTH DELIVERING.'//BLN//'IT WILL BE SET TO A DEFAULT VALUE OF 5 L^3 (MODEL LENGTH UNITS CUBED).'//BLN)
                                  ALLOCATE(SWO%MIN_DELORDER_RAT( SWO%NFARM+SWO%NAUXDEM ), SOURCE=DIEZ )
    END IF
    !
    IF(.NOT.ALLOCATED(SWO%MIN_PROJ_ORDER_RAT)) THEN
                                  CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "MINIMUM_PROJECT_DIVERSION_ORDER" KEYWORD,'//NL//'WHICH SPECIFIES NPROJ MINIMUM DELIVERY ORDER SIZE BEFORE THE DELIVERY IS REFUSED (SET TO ZERO). THIS IS FOR DETERMINING WHEN THE REQUESTED WATER IS TOO SMALL TO BE WORTH DELIVERING BY THE PROJECT.'//BLN//'IT WILL BE SET TO A DEFAULT VALUE OF 50 L^3 (MODEL LENGTH UNITS CUBED)'//BLN//'[NOTE THAT "MIN_PROJ_ORDER" KEYWORD WORKS AS WELL.]'//BLN)
                                  ALLOCATE(SWO%MIN_PROJ_ORDER_RAT(SWO%NPROJ), SOURCE = HECTO)
    END IF
    !
    IF(SWO%DEC_RUL_SIM%INUSE) THEN
        IF (.NOT. SWO%DEC_RUL_SIM%FOUND_BEGIN) CALL WARN_MSG2%ADD('SIMULATION BASED RULES DID NOT LOCATE     THE KEYWORD "BEGIN" THAT INTIATES     A BLOCK LOAD. INPUT WAS LOADED AS IF BEGIN WAS SPECIFED BEFORE ITS FIRST LINE (BEGIN FOR THIS INPUT IS OPTIONAL).'//BLN)
        IF (.NOT. SWO%DEC_RUL_SIM%FOUND_END  ) CALL WARN_MSG2%ADD('SIMULATION BASED RULES DID NOT LOCATE A CORRESPONDING "END"   THAT TERMINATES THE BLOCK LOAD. THE TERMINATOR WORD, END, IS OPTIONAL, SO THE ENTIRE FILE CONTENTS WAS LOADED INTO THE BLOCK INPUT.'//BLN)
    ELSE
        CALL WARN_MSG2%ADD('SIMULATION BASED RULES WERE NOT LOADED. THEY WILL BE SKIPPED/NOT PROCESSED (DID NOT FIND KEYWORD "DECISION_RULES SIMULATION").'//BLN)
    END IF
    !
    IF(SWO%DEC_RUL_SP%INUSE) THEN
        IF (.NOT. SWO%DEC_RUL_SP%FOUND_BEGIN) CALL WARN_MSG2%ADD('STRESS PERIOD BASED RULES DID NOT LOCATE     THE KEYWORD "BEGIN" THAT INTIATES     A BLOCK LOAD. INPUT WAS LOADED AS IF BEGIN WAS SPECIFED BEFORE ITS FIRST LINE (BEGIN FOR THIS INPUT IS OPTIONAL).'//BLN)
        IF (.NOT. SWO%DEC_RUL_SP%FOUND_END  ) CALL WARN_MSG2%ADD('STRESS PERIOD BASED RULES DID NOT LOCATE A CORRESPONDING "END"   THAT TERMINATES THE BLOCK LOAD. THE TERMINATOR WORD, END, IS OPTIONAL, SO THE ENTIRE FILE CONTENTS WAS LOADED INTO THE BLOCK INPUT.'//BLN)
    ELSE
        CALL WARN_MSG2%ADD('STRESS PERIOD BASED RULES WERE NOT LOADED. THEY WILL BE SKIPPED/NOT PROCESSED (DID NOT FIND KEYWORD "DECISION_RULES STRESS_PERIOD").'//BLN)
    END IF
    IF(SWO%DEC_RUL_TS%INUSE) THEN
        IF (.NOT. SWO%DEC_RUL_TS%FOUND_BEGIN) CALL WARN_MSG2%ADD('TIME STEP     BASED RULES DID NOT LOCATE     THE KEYWORD "BEGIN" THAT INTIATES     A BLOCK LOAD. INPUT WAS LOADED AS IF BEGIN WAS SPECIFED BEFORE ITS FIRST LINE (BEGIN FOR THIS INPUT IS OPTIONAL).'//BLN)
        IF (.NOT. SWO%DEC_RUL_TS%FOUND_END  ) CALL WARN_MSG2%ADD('TIME STEP     BASED RULES DID NOT LOCATE A CORRESPONDING "END"   THAT TERMINATES THE BLOCK LOAD. THE TERMINATOR WORD, END, IS OPTIONAL, SO THE ENTIRE FILE CONTENTS WAS LOADED INTO THE BLOCK INPUT.'//BLN)
    ELSE
        CALL WARN_MSG2%ADD('TIME STEP     BASED RULES WERE NOT LOADED. THEY WILL BE SKIPPED/NOT PROCESSED (DID NOT FIND KEYWORD "DECISION_RULES TIME_STEP").'//BLN)
    END IF
    IF(SWO%DEC_RUL_IT%INUSE) THEN
        IF (.NOT. SWO%DEC_RUL_IT%FOUND_BEGIN) CALL WARN_MSG2%ADD('ITERATION     BASED RULES DID NOT LOCATE     THE KEYWORD "BEGIN" THAT INTIATES     A BLOCK LOAD. INPUT WAS LOADED AS IF BEGIN WAS SPECIFED BEFORE ITS FIRST LINE (BEGIN FOR THIS INPUT IS OPTIONAL).'//BLN)
        IF (.NOT. SWO%DEC_RUL_IT%FOUND_END  ) CALL WARN_MSG2%ADD('ITERATION     BASED RULES DID NOT LOCATE A CORRESPONDING "END"   THAT TERMINATES THE BLOCK LOAD. THE TERMINATOR WORD, END, IS OPTIONAL, SO THE ENTIRE FILE CONTENTS WAS LOADED INTO THE BLOCK INPUT.'//BLN)
    ELSE
        CALL WARN_MSG2%ADD('ITERATION     BASED RULES WERE NOT LOADED. THEY WILL BE SKIPPED/NOT PROCESSED (DID NOT FIND KEYWORD "DECISION_RULES ITERATION").'//BLN)
    END IF
    !
    IF(NO_MAX_STORAGE ) CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_MAXIMUM_CAPACITY" KEYWORD,'//NL//'WHICH SPECIFIES THE MAXIMUM CAPACITY FOR EACH RESERVOIR.'//NL//'ONLY ONE VALUE PER RESERVOIR (NOTE SPLIT RESERVOIRS ARE TREATED AS ONE RESERVOIR SINCE THEY HAVE ONE RELEASE POINT AND LUMPED STORAGE).'//NL//'SWO WILL USE THE ELEVATION-AREA-STORAGE TABLE TO DETERMINE THE MAXIMUM CAPACITY (LAST ENTRY IN TABLE).'//NL//'[NOTE THAT "MAXIMUM_CAPACITY" KEYWORD WORKS AS WELL.]'//BLN)
    IF(NO_MAX_AREA    ) CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_MAXIMUM_AREA" KEYWORD,'//NL//'WHICH SPECIFIES THE MAXIMUM AREA FOR EACH RESERVOIR.'//NL//'ONLY ONE VALUE PER RESERVOIR (NOTE SPLIT RESERVOIRS ARE TREATED AS ONE RESERVOIR SINCE THEY HAVE ONE RELEASE POINT AND LUMPED STORAGE).'//NL//'SWO WILL USE THE ELEVATION-AREA-STORAGE TABLE TO DETERMINE THE MAXIMUM AREA (LAST ENTRY IN TABLE).'//NL//'[NOTE THAT "MAXIMUM_AREA" KEYWORD WORKS AS WELL.]'//BLN)
    IF(NO_RELEASE_FRAC) THEN
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_PROJECT_RELEASE_FRACTION" KEYWORD,'//NL//'WHICH SPECIFIES THE FRACTION OF THE TOTAL DEMANDED RELEASED WATER THAT IS SENT TO EACH RERVOIR.'//NL//'IT WILL BE AUTOMATICALLY SET TO EVENLY SPREAD THE DEMAND ACROSS ALL RESERVOIRS (SPLIT RESERVOIRS COUNT AS ONE).'//NL//'THIS FRACTION MAY BE OVER WRITTEN/SET IN THE S-LANGUAGE RETURN VARIABLE "PROJECT.ID.RES_RELEASE_FRAC.ResID"'//BLN)
        CALL SWO%RELEASE_DMD_FRAC%INIT('REL_FRAC',  DNEG, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
    END IF
    IF(NO_REQ_REL_FRAC) THEN
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_REQUIRED_RELEASE_FRACTION" KEYWORD,'//NL//'WHICH SPECIFIES THE FRACTION OF NECESSEARY RELEASES TO MEET A REQUIRE FLOW THAT IS ASSIGNED ETO REACH RESERVOIR.'//NL//'IT IS AUTOMATICALLY USE THOSE DEFINED BY "RESERVOIR_PROJECT_RELEASE_FRACTION" OR SET TO ONE, WHICH ASSUMES THAT THERE IS ONE RESERVOIR PER PROJECT (SPLIT RESERVOIRS COUNT AS ONE).'//NL//'THIS FRACTION MAY BE OVER WRITTEN/SET IN THE S-LANGUAGE RETURN VARIABLE "PROJECT.ID.RES_REQ_RELEASE_FRAC.ResID"'//BLN) !'//NL//'THIS FRACTION MAY BE OVER WRITTEN/SET IN THE S-LANGUAGE RETURN VARIABLE "PROJECT.ID.RELEASE_FRAC_RES.ResID"
        CALL SWO%RELEASE_REQ_FRAC%INIT('REL_REQ_FRAC',  DZ, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
    END IF
    IF(.NOT. SWO%PRECIP_AREA_FRAC%INUSE) THEN
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_PRECIPITATION_AREA_FRACTION" KEYWORD,'//NL//'WHICH SPECIFIES THE FRACTION OF THE MAX RESERVOIR AREA RELATIVE TO THE CURRENT SURFACE AREA AND THE CURRENT AREA THAT PRECIPITATION FALLS ON. IT WILL BE SET AUTOMATICALLY TO 0.0 TO USE THE CURRENT RESERVOIR AREA FOR WHAT PRECIP FALLS ON'//BLN)
        CALL SWO%PRECIP_AREA_FRAC%INIT('PRECIP_AREA',  DZ, BL%IOUT, BL%IU, NRES_BAL, ONE, Z, Z)
    END IF
    !IF(NO_ELEV_CHNG   ) CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "MAXIMUM_RESERVOIR_ELEVATION_CHANGE" KEYWORD,'//NL//'WHICH SPECIFIES THE FRACTION OF THE TOTAL DEMANDED RELEASED WATER THAT IS SENT TO EACH RERVOIR.'//NL//'IT IS AUTOMATICALLY SET TO ONE, WHICH ASSUMES THAT THERE IS ONE RESERVOIR PER PROJECT (SPLIT RESERVOIRS COUNT AS ONE).'//NL//'THIS FRACTION MAY BE OVER WRITTEN/SET IN THE S-LANGUAGE RETURN VARIABLE "PROJECT.ID.RELEASE_FRAC_RES.ResID"'//NL//'[NOTE THAT "MAX_ELEV_CHANGE" KEYWORD WORKS AS WELL.]'//BLN)
    !
    IF(SWO%DIST_ALLOC_FRAC_TYP == NEG)  THEN
        CALL WARN_MSG2%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "ALLOCATION_BENEFICIARY_SPLIT" KEYWORD,'//NL//'WHICH SPECIFIES HOW DISTRICT ALLOCATIONS ARE SPLIT AMONG THEIR CORRESPONDING FARMS AND AUXDEMS.'//NL//'UNLESS OVERRIDED BY THE S-LANGUAGE VARIABLES, IT WILL BE ASSUMED THAT THERE IS AN EQUAL SPLIT (e.g 3 FARMS MEANS EACH GETS ONE THIRD OF THE DISTRICT ALLOCATION)'//BLN)
        SWO%DIST_ALLOC_FRAC_TYP = Z
    END IF
    !
    CALL WARN_MSG2%CHECK(HED='SWO BLOCK MESSAGE:',INFILE=BL%IU,OUTPUT=BL%IOUT,INIT=TRUE)
    !
    IF(NO_ALLOCATION_DATE) THEN
                        !
                        DO I=ONE, SWO%NPROJ
                                           SWO%PROJ(I)%AllocDateFrac = DNEG
                                           SWO%PROJ(I)%AllocDate%DATE = 'NO_DATE'  ! DATE_OPERATOR('NO_DATE')
                        END DO
                       !IF(NO_ALLOCATION_DATE)              CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SWO BLOCK ERROR: FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "ALLOCATION_DATE" KEYWORD, WHICH SPECIFIES THE DAY OF THE YEAR THAT OPERATIONS START AS A FRACTION OF THE YEAR OR DATE OF THE FORM MM/DD (AllocDate).')
    END IF
    !
    IF(.NOT. SWO%UNIT_ACCOUNTING%INUSE) CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "UNIT_ACCOUNTING" KEYWORD,'//NL//'WHICH SPECIFIES THE ByPass AND Credit STREAM SEGMENTS.'//NL//'[NOTE THAT "ACCOUNTING" KEYWORD WORKS AS WELL.]'//BLN)
    IF(.NOT. SWO%SEG_TYP%INUSE)         CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "SFR_SEGMENT_TYPE" KEYWORD,'//NL//'WHICH SPECIFIES THE SFR STREAM SEGMENTS TYPE (1:NATURAL, 2:CONVEYANCE, 3:DELIVERY, 4:RETURN FLOW).'//NL//'[NOTE THAT "SFR_SEG_TYPE" KEYWORD WORKS AS WELL.]'//BLN)
    IF(.NOT. SWO%RESBAL_RELSEG%INUSE)   CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_RELEASE_SEGMENT" KEYWORD,'//NL//'WHICH SPECIFIES THE SFR SEGEMENT THAT EACH RESERVOIR MAKES RELEASES TOO. PLEASE FIX INPUT TO CONTINUE.'//BLN)
    IF(NO_INITIAL_STORAGE)              CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_INITIAL_STORAGE" KEYWORD,'//NL//'WHICH SPECIFIES THE STARTING STORAGE WITHIN ALL THE RESERVOIRS.'//BLN)
    !IF(NO_ACAP)                         CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "ELEVATION_AREA_CAPACITY_TABLE" KEYWORD,'//NL//'WHICH LOADS FOR ALL RESERVOIRS THEIR ELVATION AREA CAPACITY RELATIONSHIPS.'//NL//'[NOTE THAT "AREA_CAPACITY_TABLE" KEYWORD WORKS AS WELL.]'//BLN)
    IF(SWO%NAUXDEM > Z .AND. .NOT. SWO%AUX_SFR_DELV%INUSE) CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "AUX_SFR_DELIVERY" KEYWORD,'//NL//'WHICH SPECIFIES THE SEGMENT, REACH, AND EFFICIENCY OF EACH AUXILIARY DEMAND (NAUXDEM).'//NL//'EITHER ADD KEYWORD AND SPECIFY NAUXDEM PROPERTIES OR SET NAUXDEM TO ZERO. [NOTE THAT "AUX_SFR_DELIV" KEYWORD WORKS AS WELL.]'//BLN)
    IF(HAS_SPLITNAM    .AND. .NOT. HAS_SPLIT_FRAC        ) CALL WARN_MSG1%ADD('FAILED TO IDENTIFY/LOCATE WITHIN BLOCK "RESERVOIR_SPLIT_FRACTIONS" KEYWORD,'//NL//'WHICH IS REQUIRED WHEN THERE ARE SPLIT RESERVOIRS THAT ARE TREATED AS ONE LUMPED POOL STORAGE'//NL//'(viz. THERE WAS A PROJECT_RESERVOIR_COUNT < 0). [NOTE THAT "SPLIT_FRACTIONS" KEYWORD WORKS AS WELL.]'//BLN)
    !
    CALL WARN_MSG1%CHECK('SURFACE WATER OPERATIONS (SWO) FATAL ERRORS. THE FOLLOWING ARE ERROR MESSAGES THAT INDICATE WHY SWO CAN NOT CONTINUE:',BL%IU,BL%IOUT,KILL=TRUE,TAIL=BLN)
    !
    IF(SWO%HAS_OUTPUT) THEN
        CALL SWO%OUTPUT%FARM       %SET_HEADER('    KPER    KSTP   KITER  FarmID  UnitID  DistID  ProjID  DelSeg  DelRch             AreaTot             AreaIrr                TFDR           ALLOTMENT         DELIVRY_YTD             BALANCE            DELORDER            DELIVERY')
        CALL SWO%OUTPUT%AUXDEM     %SET_HEADER('    KPER    KSTP   KITER   AuxID  UnitID  DistID  ProjID  AuxSeg  AuxRch                AREA              DEMAND           ALLOTMENT         DELIVRY_YTD             BALANCE            DELORDER            DELIVERY')
        CALL SWO%OUTPUT%UNIT       %SET_HEADER('    KPER    KSTP   KITER  UnitID  DistID  ProjID  DivSeg  DivRch  ChgSeg  ChgRch  NBySeg NCrdSeg           DivFactor           ChgFactor             AreaTot             AreaIrr                TFDR            DELORDER        DELIVEFF(-1)            DIVORDER             DIVERSN             DELIVRY              BYPASS            DELIVEFF              CHARGE              CREDIT            CHGRATIO         NETCHGRATIO         DIVERSN_YTD         DELIVRY_YTD          BYPASS_YTD        DELIVEFF_YTD          CHARGE_YTD          CREDIT_YTD          CHGRAT_YTD         NETCHGRAT_YTD')
        CALL SWO%OUTPUT%DIST       %SET_HEADER('    KPER    KSTP   KITER  DistID  ProjID             AreaTot             AreaIrr           ALLOC_ANN            ALLOC_CO         ALLOC_TOTAL        DELIVEFF_YTD     NETCHGRATIO_YTD            EQ_ALLOT             BALANCE                TFDR            DELORDER        DELIVEFF(-1)            DIVORDER           DIVERSION            DELIVERY              BYPASS            DELIVEFF              CHARGE              CREDIT            CHGRATIO         NETCHGRATIO       DIVERSION_YTD        DELIVERY_YTD          BYPASS_YTD        DELIVEFF_YTD          CHARGE_YTD          CREDIT_YTD        CHGRATIO_YTD     NETCHGRATIO_YTD')
        CALL SWO%OUTPUT%PROJ       %SET_HEADER('    KPER    KSTP   KITER  ProjID             AreaTot             AreaIrr          ALLOCATION                TFDR            DELORDER            DIVORDER             RELEASE         RELEASE_YTD           DIVERSION       DIVERSION_YTD            DELIVERY        DELIVERY_YTD            DELIVEFF        DELIVEFF_YTD            DIVRATIO        DIVRATIO_YTD              BYPASS          BYPASS_YTD              CHARGE          CHARGE_YTD              CREDIT          CREDIT_YTD            CHGRATIO        CHGRATIO_YTD         NETCHGRATIO     NETCHGRATIO_YTD')
        CALL SWO%OUTPUT%DivSeg     %SET_HEADER('    KPER    KSTP   KITER   DivID  DivSeg  ProjID   NUnit                TFDR            DELORDER            DIVORDER           DIVERSION            DELIVERY            DELIVEFF               DSEEP            DFLOW_IN            DFLOW_IN         UP_DIVORDER            UP_DSEEP         UP_DFLOW_IN         UP_DFLOW_RT')
        CALL SWO%OUTPUT%SplitSeg   %SET_HEADER('    KPER    KSTP   KITER   DivID  DivSeg  SpltID  SptSeg SptType                TFDR            DELORDER            DIVORDER           DIVERSION            DELIVERY            DELIVEFF               DSEEP            DFLOW_IN            DFLOW_RT             SPT/BRC')
        CALL SWO%OUTPUT%Storage    %SET_HEADER('    KPER    KSTP   KITER  ProjID   ResID  RelSeg         STORAGE_SOS            AREA_SOS         AREA_DEADPL              INFLOW             PRCP_RT             EVAP_RT            AREA_END            AREA_AVG             PRCP_VL             EVAP_VL         STORAGE_NPR         STORAGE_PRJ         RELEASE_NPR         RELEASE_MAX         RELEASE_DEM         RELEASE_PROJ         RELEASE_FLOD         STORAGE_EOS')
        CALL SWO%OUTPUT%SFR        %SET_HEADER('    KPER    KSTP   KITER  ISEG  IRCH   ROW   COL LAYER              INFLOW             SEEPAGE              RUNOFF                PRCP                EVAP             OUTFLOW')
        !CALL SWO%OUTPUT%ALLOCATION %SET_HEADER('    KPER    KSTP   FINAL             STORAGE         STORAGE_NPJ         RELEASE_YTD          USABLE_TOT           CarryOver            DivRatio            ChgRatio          USABLE_AVL         RELEASE_EST           DroughtYr           DroughtCt          DroughtFct            D1_TOTAL           D1_MEXICO              D2_RAW            D2_TOTAL              D2_NET              D2_EP1             D2_EBID         DivRatio_AD              DivMax         DivRatio_DF          ALLC_EP1_A          ALLC_EP1_C          ALLC_EP1_T         ALLC_EBID_D         ALLC_EBID_A         ALLC_EBID_C         ALLC_EBID_T')
        CALL SWO%OUTPUT%Convergence%SET_HEADER('    KPER    KSTP   KITER IPROJ     MAXDEL(RELEASE)        DEL(RELEASE)   MAXDEL(DIVERSION)      DEL(DIVERSION)    MAXDEL(DELIVERY)       DEL(DELIVERY) MAXDIFF(DIV.v.ORDR)    DIFF(DIV.v.ORDR)        MAX(OUTFLOW)             OUTFLOW')
    END IF
    !
    IF(SWO%PRT_RESDAT_CMPCT %IS_OPEN) CALL SWO%PRT_RESDAT_CMPCT %SET_HEADER('DATE_START            DYEAR_START     DYEAR_END   SP   TS  RES           DELT          STAGE        STORAGE        RELEASE       SPILLWAY        OVERTOP         INFLOW         PRECIP         EVAPOR           AREA       TRANSFER    RELEASE_MAX')
    IF(SWO%PRT_RESDAT_DETAIL%IS_OPEN) CALL SWO%PRT_RESDAT_DETAIL%SET_HEADER('DATE_START            DYEAR_START     DYEAR_END   SP   TS  RES           DELT          STAGE        STORAGE   RELEASE_PROJ   RELEASE_REQF  RELEASE_FLOOD  RELEASE_EXTRA   RELEASE_SPEC    RELEASE_MIN       SPILLWAY        OVERTOP         INFLOW         PRECIP         EVAPOR           AREA       TRANSFER    RELEASE_MAX')
    !
    IF(SWO%PRT_RESDAT%IS_OPEN) THEN
        CUSTOM = 'DATE_START            DYEAR_START     DYEAR_END   SP   TS           DELT       STAGE_01     STORAGE_01     RELEASE_01    SPILLWAY_01     OVERTOP_01      INFLOW_01      PRECIP_01      EVAPOR_01        AREA_01    TRANSFER_01'
        DO I = TWO, NRES_BAL
                           CTMP   = NUM2STR(I,TWO,TRUE)
                           CUSTOM = CUSTOM//'       STAGE_'//CTMP//'     STORAGE_'//CTMP//'     RELEASE_'//CTMP//'    SPILLWAY_'//CTMP//'     OVERTOP_'//CTMP//'      INFLOW_'//CTMP//'      PRECIP_'//CTMP//'      EVAPOR_'//CTMP//'        AREA_'//CTMP//'    TRANSFER_'//CTMP
        END DO
        CALL SWO%PRT_RESDAT%SET_HEADER(CUSTOM)
    END IF
    !
    !
    ! Set Up Rervoir Names and Split Rervoir Names
    IF(HAS_RESNAM)    THEN
         !
         K=Z
         DO I = ONE, SWO%NPROJ
               DO J = ONE, SWO%NRES_BAL(I)
                   K = K + ONE
                   SWO%RESNAM(K)                   = TRIM(RESNAM(K))
                   SWO%RESDAT(I)%RESBAL(J)%RESNAME = SWO%RESNAM(K)%STR
               END DO
         END DO
         !
         DEALLOCATE(RESNAM)
    END IF
    !
    IF(HAS_SPLITNAM)  THEN
         !
         K=Z
         DO I = ONE, SWO%NPROJ
               DO J = ONE, SWO%NRES_SPT(I)
                   K = K + ONE
                   SWO%SPLITNAM(K)                   = TRIM(SPLITNAM(K))
                   SWO%RESDAT(I)%RESSPLIT(J)%RESNAME =  SWO%SPLITNAM(K)%STR
                   !
               END DO
         END DO
         !
         DEALLOCATE(SPLITNAM)
    END IF
    !
    ! Set up allotment multiplier limits
    DO I = ONE, SWO%NDIST
        !
        SWO%DIST(I)%S_ALLOTMENT_BYBEN = FALSE
        SWO%DIST(I)%S_ALLOTMENT       = DNEG  !WILL BE OVERRIDEN IS S USES VARIABLE
        SWO%DIST(I)%S_ALLOTMENT_VOL   = DNEG  !WILL BE OVERRIDEN IS S USES VARIABLE
        !
        IF    (ALLOT_LIM(I) < UNO) THEN
                                        SWO%DIST(I)%EQ_ALLOTMENT_LIMIT = UNO
        ELSEIF(ALLOT_LIM(I) < D29) THEN
                                        SWO%DIST(I)%EQ_ALLOTMENT_LIMIT = ALLOT_LIM(I)
        ELSE
                                        SWO%DIST(I)%EQ_ALLOTMENT_LIMIT = D50  !1E100
        END IF
    END DO
    !
    ! Intialize the reservoir variables
    !
    CALL SETUP_RESERVOIR_VARIABLES(SWO)  !-------------------------------------------------------------------
    !
    ! Make the Decision rules upper case to make case insentive
    !
    CALL SWO%DEC_RUL_SIM%PREPARE_RULES()
    !
    CALL SWO%DEC_RUL_SP%PREPARE_RULES()
    CALL SWO%DEC_RUL_TS%PREPARE_RULES()
    CALL SWO%DEC_RUL_IT%PREPARE_RULES()
    !
    CALL SWO%DEC_RUL_IT_END%PREPARE_RULES()
    CALL SWO%DEC_RUL_TS_END%PREPARE_RULES()
    CALL SWO%DEC_RUL_CLOSEOUT%PREPARE_RULES()
    !
    ! Check for any S specified diversions for SFR -------------------------------------------------------------------
    !
    CALL ILIST%INIT()
    !
    SWO%S_SFR_SPEC_DIV%N = Z
    DO I=ONE, SWO%DEC_VAR%NRET
      IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
        !
        IF(     'SFR' == SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)   ) THEN
        IF( ANY('DIV' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:)) ) THEN
            !
            SWO%S_SFR_SPEC_DIV%N = SWO%S_SFR_SPEC_DIV%N + ONE
            CALL ILIST%ADD(I)
            !
        END IF
        END IF
      END IF
    END DO
    !
    IF(SWO%S_SFR_SPEC_DIV%N > Z) THEN
        !
        CALL ILIST%TOARRAY(SWO%S_SFR_SPEC_DIV%POS)
        !ALLOCATE(SWO%S_SFR_SPEC_DIV%POS(SWO%S_SFR_SPEC_DIV%N))
        ALLOCATE(SWO%S_SFR_SPEC_DIV%SEG(SWO%S_SFR_SPEC_DIV%N))
        ALLOCATE(SWO%S_SFR_SPEC_DIV%DIV(SWO%S_SFR_SPEC_DIV%N), SOURCE=DZ)
        !
        DO J=ONE, SWO%S_SFR_SPEC_DIV%N
               !
               ! SFR.ID.DIV or SFR.33.DIV or SFR.NAME.DIV
               !
               ! [SFR].ID.[DIV]
               !
               I = SWO%S_SFR_SPEC_DIV%POS(J)   !Pos in PROP_RET
               !
               N = SWO%DEC_VAR%PROP_RET(I)%ID(ONE)    !COULD SPECIFY ISEG
               !
               IF( N > Z ) THEN
                           SWO%S_SFR_SPEC_DIV%SEG(J) = N
               ELSE
                   K = FDIM%SFR_ID%GET_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(TWO) )
                   !
                   IF( K > Z ) THEN
                       SWO%S_SFR_SPEC_DIV%SEG(J) = FDIM%SFR_ID%SEG_RCH(ONE,K)
                   ELSE
                       K = SWO%DEC_VAR%PROP_RET(I)%POS !Pos in DEC_VAR%NAM
                       CALL WARN_MSG1%ADD('FOUND S RETURN VARIABLE [SFR.ID.DIV] = '//SWO%DEC_VAR%NAM(K)//', BUT FAILED TO IDENTIFY .ID. AS EITHER A SEGMENT OR SFR_NAME.'//BLN)
                   END IF
               END IF
        END DO
    END IF
    !
    ! Check for any required flow variables -------------------------------------------------------------------
    !
    SWO%REQFLOW%N = Z
    !
    DO I=ONE, SWO%DEC_VAR%NRET
    IF ( SWO%DEC_VAR%PROP_RET(I)%N > ONE ) THEN
        !
        IF( 'REQ'  == SWO%DEC_VAR%PROP_RET(I)%PROP(ONE) .AND.   &
            'FLOW' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)       ) THEN
            !
            SWO%REQFLOW%N = SWO%REQFLOW%N + ONE
            !
        END IF
    END IF
    END DO
    !
    SWO%REQFLOW%HAS_REQ = SWO%REQFLOW%N > Z
    !
    IF(SWO%REQFLOW%HAS_REQ) THEN
        ALLOCATE(SWO%REQFLOW%NAM   (SWO%REQFLOW%N   ))
        ALLOCATE(SWO%REQFLOW%ISTRM (SWO%REQFLOW%N   ))
        ALLOCATE(SWO%REQFLOW%RES   (SWO%REQFLOW%N   ))
        ALLOCATE(SWO%REQFLOW%HEDSEG(SWO%REQFLOW%N   ))
        ALLOCATE(SWO%REQFLOW%SEG   (SWO%REQFLOW%N   ))
        ALLOCATE(SWO%REQFLOW%RCH   (SWO%REQFLOW%N   ))
        ALLOCATE(SWO%REQFLOW%REQ   (SWO%REQFLOW%N   ), SOURCE = DZ)
        ALLOCATE(SWO%REQFLOW%POS   (SWO%DEC_VAR%NRET), SOURCE = NEG)
        !
        ! SWO%REQFLOW%ORDER
        !
        ! REQ.FLOW.
        J = Z
        DO I=ONE, SWO%DEC_VAR%NRET
          IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
               !
               ! REQ.FLOW.NAME or REQ.FLOW.5.1
               !
               ! [REQ].[FLOW].[NAME] or [REQ].[FLOW.5].[1]
               !
               IF( 'REQ'  == SWO%DEC_VAR%PROP_RET(I)%PROP(ONE) .AND.   &
                   'FLOW' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)       ) THEN
                        J = J + ONE
                        !
                        SWO%REQFLOW%NAM(J) = TRIM(SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_RET(I)%POS ))
                        !
                        N = SWO%DEC_VAR%PROP_RET(I)%ID(TWO)    !COULD SPECIFY ISEG
                        !
                        IF(SWO%DEC_VAR%PROP_RET(I)%N > TWO) THEN    !CHECK IF SFR_NAME
                            !
                            K = FDIM%SFR_ID%GET_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(THREE) )
                        ELSE
                            K = Z
                        END IF                      !OTHERWISE IT IS A SPECIFIED SEGMENT
                        !
                        IF(K > Z) THEN
                                                 N = FDIM%SFR_ID%SEG_RCH(ONE,K)
                                                 K = FDIM%SFR_ID%SEG_RCH(TWO,K)
                                                 IF(K < ONE) K = ONE
                        ELSEIF(SWO%DEC_VAR%PROP_RET(I)%N > TWO) THEN
                                                 !
                                                 K = SWO%DEC_VAR%PROP_RET(I)%ID(THREE)
                        ELSE
                                                 K = ONE
                        END IF
                        !
                        SWO%REQFLOW%SEG(J) = N  !ISEG
                        SWO%REQFLOW%RCH(J) = K  !IRCH
                        !
                        SWO%REQFLOW%POS(I) = J  !SEG_NSTRM(ISEG) + IRCH
                        SWO%REQFLOW%ISTRM(J) =   SEG_NSTRM(   N) + K
               END IF
          END IF
        END DO
    END IF
    !
    ! Check for any Reservoir Transfer Variables -------------------------------------------------------------------
    !
    CALL CLIST%INIT()
    CALL ILIST%INIT()
    !
    DO I=ONE, SWO%DEC_VAR%NRET
      IF(SWO%DEC_VAR%PROP_RET(I)%N > TWO) THEN
        IF( 'TRAN'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF( ('RES' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)   .OR. ANY(SWO%RESNAM == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO))   ) .AND. &
                ('RES' == SWO%DEC_VAR%PROP_RET(I)%PROP(THREE) .OR. ANY(SWO%RESNAM == SWO%DEC_VAR%PROP_RET(I)%PROP(THREE)) )       ) THEN
                 !
                 CALL ILIST%ADD(I)
                 !
            END IF
        END IF
      END IF
    END DO
    !
    SWO%TRANS%N = ILIST%LEN()
    !
    SWO%TRANS%HAS_TRAN = SWO%TRANS%N > Z
    !
    IF(SWO%TRANS%HAS_TRAN) THEN
        !
        ALLOCATE(SWO%TRANS%POS(SWO%DEC_VAR%NRET), SOURCE = NEG) !JUST FOR SPEED ON THE VARIABLE_SET_RETURN_VALUES()
        ALLOCATE(SWO%TRANS%NAM(     SWO%TRANS%N))
        ALLOCATE(SWO%TRANS%RAT(     SWO%TRANS%N), SOURCE = DZ )
        ALLOCATE(SWO%TRANS%RES(TWO, SWO%TRANS%N), SOURCE = NEG)
        !
        CALL ILIST%START()
        !I = POSITION IN PROP_RET
        !J IS THE POSITION IN SWO%TRANS
        !PROP_RET(I)%POS IS THE POSITION IN THE DEC_VAR
        DO J=ONE, SWO%TRANS%N
            I = ILIST%INT()
            SWO%TRANS%POS(I) = J
            SWO%TRANS%NAM(J) = TRIM(SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_RET(I)%POS ))
            !
            IF('RES' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                N = SWO%DEC_VAR%PROP_RET(I)%ID(TWO)
            ELSE
                N = STR_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(TWO), SWO%RESNAM )
                IF(N == Z) THEN
                    CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%TRANS%NAM(J)%STR//'"'//NL//'BUT FAILED TO IDENTIFY THE FIRST (SENDING) RESERVOIR NUMBER OR NAME WITHIN VARIABLE'//NL//'PERHAPS CHECK IF NAME IS SPELLED CORRECT AND NO SPACES ARE ALLOWED OR IF YOU ARE USING THE RESERVOIR ID THE FORMAT SHOULD BE TRAN.RES.ID.RES.ID'//NL//'(eg TRAN.RES.1.RES.2)'//BLN)
                    CYCLE
                END IF
            END IF
            !
            SWO%TRANS%RES(ONE,J) = N
            !
            IF('RES' == SWO%DEC_VAR%PROP_RET(I)%PROP(THREE)) THEN
                N = SWO%DEC_VAR%PROP_RET(I)%ID(THREE)
            ELSE
                N = STR_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(THREE), SWO%RESNAM )
                IF(N == Z) THEN
                    CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%TRANS%NAM(J)%STR//'"'//NL//'BUT FAILED TO IDENTIFY THE SECOND (RECIEVING) RESERVOIR NUMBER OR NAME WITHIN VARIABLE'//NL//'PERHAPS CHECK IF NAME IS SPELLED CORRECT AND NO SPACES ARE ALLOWED OR IF YOU ARE USING THE RESERVOIR ID THE FORMAT SHOULD BE TRAN.RES.ID.RES.ID'//NL//'(eg TRAN.RES.1.RES.2)'//BLN)
                    CYCLE
                END IF
            END IF
            !
            SWO%TRANS%RES(TWO,J) = N
            !
            CALL ILIST%NEXT()
        END DO
    END IF
    !
    ! Check for S specified RELEASE_FRAC_RES or REQFLOW_FRAC -------------------------------------------------------------------
    !
    ALLOCATE(SWO%S_RELEASE_FRAC    %HAS_S(SWO%NPROJ), SOURCE = FALSE)
    !ALLOCATE(SWO%S_REQ_RELEASE_FRAC%HAS_S(SWO%NPROJ), SOURCE = FALSE)
    !
    DO I=ONE, SWO%DEC_VAR%NRET
      IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
        IF( 'PROJ'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF('RELEASE_FRAC_RES' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                 !
                          SWO%S_RELEASE_FRAC%HAS_FRAC = TRUE
                 ALLOCATE(SWO%S_RELEASE_FRAC%BACK_CALC(SWO%NPROJ), SOURCE = Z)
                 !ALLOCATE(SWO%S_RELEASE_FRAC%FRAC     (SWO%NPROJ))
                 ALLOCATE(SWO%S_RELEASE_FRAC%POS      (SWO%NPROJ))
                 EXIT
                 !
            END IF
        END IF
        IF( 'RES'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF('PROJ_REL_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                 !
                          SWO%S_RELEASE_FRAC%HAS_FRAC = TRUE
                 ALLOCATE(SWO%S_RELEASE_FRAC%BACK_CALC(SWO%NPROJ), SOURCE = Z)
                 !ALLOCATE(SWO%S_RELEASE_FRAC%FRAC     (SWO%NPROJ))
                 ALLOCATE(SWO%S_RELEASE_FRAC%POS      (SWO%NPROJ))
                 EXIT
                 !
            END IF
        END IF
      END IF
    END DO
    !
    IF(SWO%S_RELEASE_FRAC%HAS_FRAC) THEN
       DO I=ONE, SWO%DEC_VAR%NRET
         IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN                               ! Slang Var: PROJ.id.RELEASE_FRAC_RES.id2
           IF( 'PROJ'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
               IF('RELEASE_FRAC_RES' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                    !
                    K = SWO%DEC_VAR%PROP_RET(I)%ID(ONE)   !K = IPROJ
                    !IF(SWO%S_RELEASE_FRAC%FRAC(K)%N==Z) CALL SWO%S_RELEASE_FRAC%FRAC%ALLOC(SWO%NRES_BAL(K),  DZ)
                    IF(SWO%S_RELEASE_FRAC%POS (K)%N==Z) CALL SWO%S_RELEASE_FRAC%POS %ALLOC(SWO%NRES_BAL(K), Z)
                    !
                    N = SWO%DEC_VAR%PROP_RET(I)%ID(TWO)   !N = IRES
                    !
                    IF(N == Z .AND. SWO%DEC_VAR%PROP_RET(I)%N > TWO) THEN
                        N = STR_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(THREE), SWO%RESNAM )
                        SWO%DEC_VAR%PROP_RET(I)%ID(TWO) = N
                    END IF
                    !
                    IF(N == Z) THEN
                        CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_RET(I)%POS )//'"'//NL//'BUT FAILED TO IDENTIFY THE EITHER THE RESERVOIR NUMBER OR THE RESERVOIR NAME'//BLN)
                        CYCLE
                    END IF
                    !SWO%S_RELEASE_FRAC%FRAC(K)%VEC(N) = DZ
                    SWO%S_RELEASE_FRAC%POS (K)%VEC(N) = SWO%DEC_VAR%PROP_RET(I)%POS
                    !
               END IF
         ELSEIF( 'RES'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN                ! Slang Var: RES.id.PROJ_REL_FRAC
               !
               N = SWO%DEC_VAR%PROP_RET(I)%ID(ONE)   ! N = Absolute Res Number, not local
               !
               IF(N == Z .AND. SWO%DEC_VAR%PROP_RET(I)%N > TWO) THEN
                   N = STR_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(TWO), SWO%RESNAM )
                   !
                   IF(N == Z) THEN
                       CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_RET(I)%POS )//'"'//NL//'BUT FAILED TO IDENTIFY THE EITHER THE RESERVOIR NUMBER OR THE RESERVOIR NAME'//BLN)
                       CYCLE
                   END IF
                   !
                   SWO%DEC_VAR%PROP_RET(I)%ID(ONE) = N
                   DO J=3, SWO%DEC_VAR%PROP_RET(I)%N
                      SWO%DEC_VAR%PROP_RET(I)%PROP(J-1) = SWO%DEC_VAR%PROP_RET(I)%PROP(J)
                      SWO%DEC_VAR%PROP_RET(I)%ID  (J-1) = SWO%DEC_VAR%PROP_RET(I)%ID  (J)
                   END DO
                   SWO%DEC_VAR%PROP_RET(I)%N = SWO%DEC_VAR%PROP_RET(I)%N - ONE
               END IF
               !
               IF('PROJ_REL_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                    !
                    K = SWO%RESBAL2PROJ(ONE,N)         ! Get Proj Number
                    N = SWO%RESBAL2PROJ(TWO,N)         ! Get Res number in Proj (IRES)
                    !
                    IF(SWO%S_RELEASE_FRAC%POS (K)%N==Z) CALL SWO%S_RELEASE_FRAC%POS %ALLOC(SWO%NRES_BAL(K), Z)
                    !
                    SWO%S_RELEASE_FRAC%POS (K)%VEC(N) = SWO%DEC_VAR%PROP_RET(I)%POS
                    !
               END IF
           END IF
         END IF
       END DO
       !
       DO I=ONE, SWO%NPROJ
                          IF(SWO%S_RELEASE_FRAC%POS(K)%N > Z) THEN
                                N = COUNT(SWO%S_RELEASE_FRAC%POS (K)%VEC == Z)
                                SWO%S_RELEASE_FRAC%HAS_S(I) = TRUE
                                !
                                IF    (N  > ONE) THEN
                                    DO J = ONE, SWO%S_RELEASE_FRAC%POS(K)%N
                                        IF( SWO%S_RELEASE_FRAC%POS(K)%VEC(J) > Z) THEN
                                            SWO%S_RELEASE_FRAC%BACK_CALC(I)  =-J
                                            EXIT
                                        END IF
                                    END DO
                                ELSEIF(N == ONE) THEN
                                    DO J = ONE, SWO%S_RELEASE_FRAC%POS(K)%N
                                        IF(SWO%S_RELEASE_FRAC%POS(K)%VEC(J) == Z) THEN
                                            SWO%S_RELEASE_FRAC%BACK_CALC(I)  = J
                                            EXIT
                                        END IF
                                    END DO
                                END IF
                          END IF
       END DO
    END IF
    !
    !-----------------------------------------------------------------------------------------
    !
    DO I=ONE, SWO%DEC_VAR%NRET
      IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
        IF( 'RES'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF    ('REQFLOW_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                 !
                 !         SWO%S_REQ_RELEASE_FRAC%HAS_FRAC = TRUE
                 !ALLOCATE(SWO%S_REQ_RELEASE_FRAC%BACK_CALC(SWO%NPROJ), SOURCE = Z)
                 !ALLOCATE(SWO%S_REQ_RELEASE_FRAC%FRAC     (SWO%NPROJ))
                 !ALLOCATE(SWO%S_REQ_RELEASE_FRAC%POS      (SWO%NPROJ))
                 SWO%S_REQ_RELEASE_FRAC = TRUE
                 SWO%REQ_FRAC_USES_DMD_FRAC = FALSE
                 EXIT
                 !
            END IF
        END IF
      END IF
    END DO
    !
    IF(SWO%S_REQ_RELEASE_FRAC) THEN
          BLOCK
              TYPE(LOGICAL_VECTOR),DIMENSION(SWO%NPROJ):: CHK
              DO I=1,SWO%NPROJ
                  CALL CHK(I)%ALLOC(SWO%NRES_BAL(I), FALSE)
              END DO
              !
              DO I=ONE, SWO%DEC_VAR%NRET
                IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
                  IF( 'RES'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
                      !
                      IF    ('REQFLOW_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                           !
                           J = SWO%DEC_VAR%PROP_RET(I)%ID(ONE)
                           N = SWO%DEC_VAR%PROP_RET(I)%ID(TWO)   !N = IRES
                           IF(N == Z .AND. SWO%DEC_VAR%PROP_RET(I)%N > TWO) THEN
                               N = STR_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(THREE), SWO%RESNAM )
                               SWO%DEC_VAR%PROP_RET(I)%ID(TWO) = N
                               !
                           END IF
                           !
                           IF(J==Z .OR. N == Z) THEN
                               CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_RET(I)%POS )//'"'//NL//'BUT FAILED TO IDENTIFY THE EITHER THE RESERVOIR NUMBER OR THE RESERVOIR NAME'//BLN)
                               CYCLE
                           END IF
                           !
                           CHK(J)%VEC(N) = TRUE
                           !
                      END IF
                  END IF
                END IF
              END DO
              !
              DO I=1,SWO%NPROJ
                  IF(.NOT. ALL(CHK(I)%VEC)) THEN
                      CALL WARN_MSG1%ADD('IF YOU USE S VARIABLE RES.ID.REQFLOW_FRAC.ID THEN YOU MUST DEFINE A S VARIABLE FOR ALL RESERVOIRS IN USE FOR THE SIMULATION.'//BLN)
                      EXIT
                  END IF
              END DO
          END BLOCK
    END IF
    !
    !!!DO I=ONE, SWO%DEC_VAR%NRET
    !!!  IF(SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
    !!!    IF( 'PROJ'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
    !!!        !
    !!!        IF    ('REQFLOW_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
    !!!             !
    !!!             K = SWO%DEC_VAR%PROP_RET(I)%ID(ONE)   !K = IPROJ
    !!!             IF(SWO%S_REQ_RELEASE_FRAC%FRAC(K)%N==Z) CALL SWO%S_REQ_RELEASE_FRAC%FRAC%ALLOC(SWO%NRES_BAL(K),  DZ)
    !!!             IF(SWO%S_REQ_RELEASE_FRAC%POS (K)%N==Z) CALL SWO%S_REQ_RELEASE_FRAC%POS %ALLOC(SWO%NRES_BAL(K),   Z)
    !!!             !
    !!!             N = SWO%DEC_VAR%PROP_RET(I)%ID(TWO)   !N = IRES
    !!!             IF(N == Z .AND. SWO%DEC_VAR%PROP_RET(I)%N > ONE) THEN
    !!!                 N = STR_POS( SWO%DEC_VAR%PROP_RET(I)%PROP(THREE), SWO%RESNAM )
    !!!                 SWO%DEC_VAR%PROP_RET(I)%ID(TWO) = N
    !!!                 !
    !!!             END IF
    !!!             !
    !!!             IF(N == Z) THEN
    !!!                 CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_RET(I)%POS )//'"'//NL//'BUT FAILED TO IDENTIFY THE EITHER THE RESERVOIR NUMBER OR THE RESERVOIR NAME'//BLN)
    !!!                 CYCLE
    !!!             END IF
    !!!             SWO%S_REQ_RELEASE_FRAC%FRAC(K)%VEC(N) = DZ
    !!!             SWO%S_REQ_RELEASE_FRAC%POS (K)%VEC(N) = SWO%DEC_VAR%PROP_RET(I)%POS
    !!!             !
    !!!        END IF
    !!!    END IF
    !!!  END IF
    !!!END DO
    !!!!
    !!!DO I=ONE, SWO%NPROJ
    !!!                   IF(SWO%S_REQ_RELEASE_FRAC%POS(K)%N > Z) THEN
    !!!                         N = COUNT(SWO%S_REQ_RELEASE_FRAC%POS (K)%VEC == Z)
    !!!                         SWO%S_REQ_RELEASE_FRAC%HAS_S(I) = TRUE
    !!!                         !
    !!!                         IF    (N  > ONE) THEN
    !!!                             DO J = ONE, SWO%S_REQ_RELEASE_FRAC%POS(K)%N
    !!!                                 IF( SWO%S_REQ_RELEASE_FRAC%POS(K)%VEC(J) > Z) THEN
    !!!                                     SWO%S_REQ_RELEASE_FRAC%BACK_CALC(I)  =-J
    !!!                                     EXIT
    !!!                                 END IF
    !!!                             END DO
    !!!                         ELSEIF(N == ONE) THEN
    !!!                             DO J = ONE, SWO%S_REQ_RELEASE_FRAC%POS(K)%N
    !!!                                 IF(SWO%S_REQ_RELEASE_FRAC%POS(K)%VEC(J) == Z) THEN
    !!!                                     SWO%S_REQ_RELEASE_FRAC%BACK_CALC(I)  = J
    !!!                                     EXIT
    !!!                                 END IF
    !!!                             END DO
    !!!                         END IF
    !!!                   END IF
    !!!END DO
    !-----------------------------------------------------------------------------------------
    !
    ! CHECK FOR S VARIABLES THAT NEED FLAGS SET
    !
    SWO%HAS_FMP_SW_LIMIT_RULZ = FALSE
    SWO%HAS_REQ_DELIVERY      = FALSE
    !SWO%HAS_RELEASE_FRAC_S    = FALSE
    SWO%HAS_MIN_RELEASE_S     = FALSE
    SWO%HAS_ADD_RELEASE_S     = FALSE
    SWO%HAS_MAX_RELEASE_S     = FALSE
    SWO%HAS_MAX_STORAGE_S     = FALSE
    SWO%HAS_MIN_STORAGE_S     = FALSE
    SWO%HAS_MIN_STORAGE_TRAN_S= FALSE
    SWO%HAS_RELEASE_SPEC_S    = FALSE
    SWO%HAS_MAX_SPILL_S       = FALSE
    SWO%HAS_SPILL_S           = FALSE
    SWO%HAS_MAX_STAGE_S       = FALSE
    SWO%HAS_ELEV_CHNG_S       = FALSE
    SWO%HAS_ELEV_CHNG         = ANY(ELEV_CHNG > DZ)
    SWO%HAS_PROJ_RELEASE_ADD  = FALSE
    !
    DO I=ONE, SWO%DEC_VAR%NRET
    IF(SWO%DEC_VAR%PROP_RET(I)%N >= TWO) THEN
        !PROJECT.ID.RES_RELEASE_FRAC.ResID PROJECT.ID.RES_REQ_RELEASE_FRAC.ResID
        IF( 'WBS'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE) .OR. 'FARM'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF( ANY('DELIVERY_LIMIT' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 SWO%HAS_FMP_SW_LIMIT_RULZ = TRUE
                 !
            ELSEIF( ANY('ALLOC_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 SWO%DIST_ALLOC_FRAC_TYP = THREE
                 !
            ELSEIF( ANY('DELIVERY'     == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:)) .OR. &
                    ANY('DELIVERY_VOL' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))      ) THEN
                 !
                 SWO%HAS_REQ_DELIVERY = TRUE
                 !
            END IF
        ELSEIF( 'AUX'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE) ) THEN
            !
            IF( ANY('ALLOC_FRAC' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 SWO%DIST_ALLOC_FRAC_TYP = THREE
                 !
            END IF
        ELSEIF( 'PROJ'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF( ANY('ADD_RELEASE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 SWO%HAS_PROJ_RELEASE_ADD = TRUE
                 !
            END IF
        ELSEIF( 'REQ'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN  !CHECK FOR REQUIRED DELVIERY
            !
            IF( ANY('DELIVERY'     == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:)) .OR. &
                    ANY('DELIVERY_VOL' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))      ) THEN
                 !
                 SWO%HAS_REQ_DELIVERY = TRUE
                 !
            END IF
        ELSEIF('RES'== SWO%DEC_VAR%PROP_RET(I)%PROP(ONE)) THEN
            !
            IF( ANY('MAX_STAGE_DROP' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 IF(SWO%HAS_ELEV_CHNG .AND. .NOT. SWO%HAS_ELEV_CHNG_S) THEN
                     CALL WARN_MSG2%ADD('INPUT CONTAINED KEYWORD "MAXIMUM_RESERVOIR_ELEVATION_CHANGE" (or "MAX_ELEV_CHANGE"),'//NL//  &
                                       'BUT IT ALSO FOUND AN S RETURN VARIABLE DEFINED AS RES.ID.MAX_STAGE_DROP.'//NL//               &
                                       'SWO WILL GIVE HIGHER PRIORTY TO THE S VAIRABLE, BUT IT IS RECOMEDNED TO ONLY USE ONE OR THE OTHER.'//NL// &
                                       'THE VARIABLE FOUND THAT TRIGGERED THIS WARNING IS '//SWO%DEC_VAR%NAM(SWO%DEC_VAR%PROP_RET(I)%POS)//BLN)


                 END IF
                 SWO%HAS_ELEV_CHNG_S = TRUE
                 SWO%HAS_ELEV_CHNG   = TRUE
                 !
            ELSEIF( ANY('MIN_RELEASE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_MIN_RELEASE_S = TRUE
                 !
            ELSEIF( ANY('ADD_RELEASE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_ADD_RELEASE_S = TRUE
                 !
            ELSEIF( ANY('MAX_RELEASE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_MAX_RELEASE_S = TRUE
                 !
            ELSEIF( ANY('MAX_STAGE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_MAX_STAGE_S = TRUE
                 !
            ELSEIF( ANY('MAX_STORAGE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_MAX_STORAGE_S = TRUE
                 !
            ELSEIF( ANY('MIN_STAGE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_MIN_STORAGE_S = TRUE
                 !
            ELSEIF( ANY('MIN_STORAGE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_MIN_STORAGE_S = TRUE
                 !
            ELSEIF( ANY('MIN_STORAGE_TRAN' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 SWO%HAS_MIN_STORAGE_TRAN_S = TRUE
                 !
            ELSEIF( ANY('SPEC_RELEASE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                 SWO%HAS_RELEASE_SPEC_S = TRUE
                 !
            ELSEIF( ANY('SPILL_STAGE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_SPILL_S = TRUE
                 !
            ELSEIF( ANY('SPILL_STORAGE' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO:))) THEN
                 !
                SWO%HAS_SPILL_S = TRUE
                 !
            ELSEIF( 'MAX_SPILL' == SWO%DEC_VAR%PROP_RET(I)%PROP(TWO)) THEN
                 !
                 SWO%HAS_MAX_SPILL_S = TRUE
                 !
                 K =  SWO%DEC_VAR%PROP_RET(I)%ID(ONE)
                 !
                 ASSOCIATE( II => SWO%RESBAL2PROJ(ONE,K), JJ => SWO%RESBAL2PROJ(TWO,K) )  !II=IPROJ, JJ = IRES
                              !
                              SWO%RESDAT(II)%RESBAL(JJ)%NO_MAX_SPILL_S = FALSE
                              !
                 END ASSOCIATE
            END IF
        END IF
    END IF
    END DO
    CALL WARN_MSG2%CHECK(HED='SWO BLOCK MESSAGE:',INFILE=BL%IU,OUTPUT=BL%IOUT,INIT=TRUE)
    !
    !-----------------------------------------------------------------------------------------
    !
    ! Check if S requests SFR flow from previous time step SFR_OLD_FLO
    !
    SWO%NSFR_OLD_FLO = Z
    DO I=ONE, SWO%DEC_VAR%NSYS
      IF(SWO%DEC_VAR%PROP_PUL(I)%N > TWO) THEN
        IF( 'SFR'== SWO%DEC_VAR%PROP_PUL(I)%PROP(ONE)) THEN
            !
            IF    ( ANY('INFLOW' == SWO%DEC_VAR%PROP_PUL(I)%PROP(3:))) THEN
                  !
                  SWO%NSFR_OLD_FLO = SWO%NSFR_OLD_FLO + ONE
                  !
            ELSEIF( ANY('OUTFLOW' == SWO%DEC_VAR%PROP_PUL(I)%PROP(3:))) THEN
                  !
                  SWO%NSFR_OLD_FLO = SWO%NSFR_OLD_FLO + ONE
            END IF
        END IF
      END IF
    END DO
    !
    IF(SWO%NSFR_OLD_FLO > Z) THEN
        ALLOCATE(SWO%SFR_OLD_FLO(SWO%NSFR_OLD_FLO))
        !
        N = Z
        DO I=ONE, SWO%DEC_VAR%NSYS
          IF(SWO%DEC_VAR%PROP_PUL(I)%N > TWO) THEN
            IF( 'SFR'== SWO%DEC_VAR%PROP_PUL(I)%PROP(ONE)) THEN
                !
                IF    ( ANY('INFLOW' == SWO%DEC_VAR%PROP_PUL(I)%PROP(3:))) THEN
                      !
                      N = N + ONE
                      !
                      SWO%SFR_OLD_FLO(N)%IS_INFLOW = TRUE
                      SWO%SFR_OLD_FLO(N)%FLO       = DZ
                      !
                      SWO%SFR_OLD_FLO(N)%PROP_PUL_POS = I
                      !
                      IF(SWO%DEC_VAR%PROP_PUL(I)%ID(1) > Z) THEN
                          !
                          J = SWO%DEC_VAR%PROP_PUL(I)%ID(1)
                          !
                          IF(SWO%DEC_VAR%PROP_PUL(I)%ID(2) > Z) THEN
                              K = SWO%DEC_VAR%PROP_PUL(I)%ID(2)
                          ELSE
                              K = ONE
                          END IF
                      ELSE
                          CALL FDIM%SFR_ID%NAM2SEGRCH(SWO%DEC_VAR%PROP_PUL(I)%PROP(2), J, K)
                          IF( K < ONE) K = ONE
                      END IF
                      !
                      IF    ( J == NEG) THEN
                              J = ONE
                              K = ONE
                              CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_PUL(I)%POS )//'"'//NL//'BUT THE SFR NAME "'//SWO%DEC_VAR%PROP_PUL(I)%PROP(2)//'" DOES NOT MATCH ANY SFR NAME FROM THE FMP GLOBAL DIMENSION BLOCK SFR_NAMES.'//BLN)
                      ELSEIF    ( J < Z .OR. NSEG < J) THEN
                              J = ONE
                              K = ONE
                              CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_PUL(I)%POS )//'"'//NL//'BUT IT REFERANCED AN SFR SEGMENT THAT IS EITHER LESS THEN ONE OR GREATER THEN NSEG.'//BLN)
                      ELSEIF( K > SEG_NSTRM(J+1) - SEG_NSTRM(J)) THEN
                              J = ONE
                              K = ONE
                              CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_PUL(I)%POS )//'"'//NL//'BUT IT REFERANCED AN SFR REACH THAT IS GREATER THEN THE NUMBER OF REACHES DEFINED FOR THE SEGMENT.'//BLN)
                      END IF
                      !
                      SWO%SFR_OLD_FLO(N)%ISTRM = SEG_NSTRM(J) + K
                      !
                ELSEIF( ANY('OUTFLOW' == SWO%DEC_VAR%PROP_PUL(I)%PROP(3:))) THEN
                      !
                      N = N + ONE
                      !
                      SWO%SFR_OLD_FLO(N)%IS_INFLOW = FALSE
                      SWO%SFR_OLD_FLO(N)%FLO       = DZ
                      !
                      SWO%SFR_OLD_FLO(N)%PROP_PUL_POS = I
                      !
                      IF(SWO%DEC_VAR%PROP_PUL(I)%ID(1) > Z) THEN
                          !
                          J = SWO%DEC_VAR%PROP_PUL(I)%ID(1)
                          !
                          IF(SWO%DEC_VAR%PROP_PUL(I)%ID(2) > Z) THEN
                              K = SWO%DEC_VAR%PROP_PUL(I)%ID(2)
                          ELSE
                              K = Z
                          END IF
                      ELSE
                          CALL FDIM%SFR_ID%NAM2SEGRCH(SWO%DEC_VAR%PROP_PUL(I)%PROP(2), J, K)
                          IF( K < ONE) K = Z
                      END IF
                      !
                      IF    ( J == NEG) THEN
                              J = ONE
                              K = ONE
                              CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_PUL(I)%POS )//'"'//NL//'BUT THE SFR NAME "'//SWO%DEC_VAR%PROP_PUL(I)%PROP(2)//'" DOES NOT MATCH ANY SFR NAME FROM THE FMP GLOBAL DIMENSION BLOCK SFR_NAMES. PLEASE CHECK TO SEE IF YOUR INPUT.'//BLN)
                      ELSEIF    ( J < Z .OR. NSEG < J) THEN
                              J = ONE
                              K = ONE
                              CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_PUL(I)%POS )//'"'//NL//'BUT IT REFERANCED AN SFR SEGMENT THAT IS EITHER LESS THEN ONE OR GREATER THEN NSEG.'//BLN)
                      ELSEIF( K > SEG_NSTRM(J+1) - SEG_NSTRM(J)) THEN
                              J = ONE
                              K = ONE
                              CALL WARN_MSG1%ADD('FOUND VARIABLE NAME "'//SWO%DEC_VAR%NAM( SWO%DEC_VAR%PROP_PUL(I)%POS )//'"'//NL//'BUT IT REFERANCED AN SFR REACH THAT IS GREATER THEN THE NUMBER OF REACHES DEFINED FOR THE SEGMENT.'//BLN)
                      END IF
                      !
                      IF( K < ONE) K = SEG_NSTRM(J+1) - SEG_NSTRM(J)
                      !
                      SWO%SFR_OLD_FLO(N)%ISTRM = SEG_NSTRM(J) + K
                END IF
            END IF
          END IF
        END DO
        !
    END IF
    !
    !
    IF(SWO%INNER_ITERATION < ONE) SWO%INNER_ITERATION = ONE
    !
    ! Initialize Farm Allotment Fractions
    DO CONCURRENT (I=ONE:SWO%NFARM  ); SWO%FARM(I  )%DIST_ALLOC_FRAC = DZ
    END DO
    DO CONCURRENT (I=ONE:SWO%NAUXDEM); SWO%AUXDEM(I)%DIST_ALLOC_FRAC = DZ
    END DO
    !
    ! Initialize Farm S Specified Allotment Fractions
    DO CONCURRENT (I=ONE:SWO%NFARM  ); SWO%FARM(I  )%S_ALLOTMENT = DNEG
    END DO
    DO CONCURRENT (I=ONE:SWO%NAUXDEM)
        SWO%AUXDEM(I)%S_ALLOTMENT = DNEG
        SWO%AUXDEM(I)%S_DEMAND    = DNEG
        SWO%AUXDEM(I)%S_DEMAND_LIM= DNEG
        SWO%AUXDEM(I)%DEMAND_LIM  = INF
    END DO
    !
    ! CHECK FOR SKIPPED MINIMUMS
    DO CONCURRENT (I=ONE:SWO%NDIST)
        IF(SWO%DIST_DELTOL(I) .NE. SWO%DIST_DELTOL(I)) THEN
                                                            SWO%DIST_DELTOL(I) = DZ
        ELSEIF(SWO%DIST_DELTOL(I) < DZ) THEN
                                                            SWO%DIST_DELTOL(I) = DZ
        END IF
    END DO
    !
    DO CONCURRENT (I=ONE:SWO%NPROJ)
        IF(SWO%MIN_PROJ_ORDER_RAT(I) .NE. SWO%MIN_PROJ_ORDER_RAT(I)) THEN
                                                            SWO%MIN_PROJ_ORDER_RAT(I) = DZ
        ELSEIF(SWO%MIN_PROJ_ORDER_RAT(I) < NEARZERO_12) THEN
                                                            SWO%MIN_PROJ_ORDER_RAT(I) = NEARZERO_12
        END IF
    END DO
    !
    N = SWO%NFARM+SWO%NAUXDEM
    DO CONCURRENT (I=ONE:N)
        IF(SWO%MIN_DELORDER_RAT(I) .NE. SWO%MIN_DELORDER_RAT(I)) THEN
                                                            SWO%MIN_DELORDER_RAT(I) = DZ
        ELSEIF(SWO%MIN_DELORDER_RAT(I) < NEARZERO_12) THEN
                                                            SWO%MIN_DELORDER_RAT(I) = NEARZERO_12
        END IF
    END DO
    !
    K=Z
    DO I = ONE, SWO%NPROJ
       SWO%PROJ(I)%StorMax = DZ
       ASSOCIATE(RESBAL=>SWO%RESDAT(I)%RESBAL, ACAP=>SWO%RESDAT(I)%ACAP)
          DO J = ONE, SWO%NRES_BAL(I)
              K = K + ONE
              !
              !IF(INIT_AS_ELEV) THEN
              !                     TMP1 = RES_INITIAL_STORAGE(K) !ACTUAL VALUE IS ELEVATION
              !                     !
              !                     CALL ACAP_ELEV2STOR(SWO,I,J,TMP1,RES_INITIAL_STORAGE(K),WARN_MSG1%STR)  !NOT USING WARN_MSG1%STR AS A TEMP VARIABLE FOR ERROR CHECKING
              !                     !
              !                     IF(WARN_MSG1%STR.NE.NL) THEN
              !                         WARN_MSG1%RAISED = TRUE
              !                         WARN_MSG1%N = LEN(WARN_MSG1%STR)
              !                     END IF
              !END IF
              IF (SWO%NRES_SPT(I) > Z) THEN
                  JRES = RESBAL(1)%MAIN_SPLT
              ELSE
                  JRES = J
              END IF
              IMAX = ACAP(JRES)%ACAP_COUNT
              !
              RESBAL(J)%ACAP_MIN_ELEV = ACAP(JRES)%ACAP_ELEV(1)
              RESBAL(J)%ACAP_MAX_ELEV = ACAP(JRES)%ACAP_ELEV(IMAX)
              RESBAL(J)%ACAP_MIN_STOR = ACAP(JRES)%ACAP_STORAGE(1)
              RESBAL(J)%ACAP_MAX_STOR = ACAP(JRES)%ACAP_STORAGE(IMAX)
              !
              !
              RESBAL(J)%SPSTART = RES_START_SP(K)
              !
              RESBAL(J)%STORAGE = RES_INITIAL_STORAGE(K)
              !
              IF(RESBAL(J)%STORAGE < DZ) RESBAL(J)%STORAGE = DZ
              !
              RESBAL(J)%STORAGE_PREV = RESBAL(J)%STORAGE
              !
              RESBAL(J)%MAX_RELEASE_VOL = inf  !SET TO INITIAL VALUE THAT WILL NOT LIMIT RELEASES
              !
              RESBAL(J)%STORAGE_TRAN_FRAC = HALF
              !
              IF(MAX_RELEASE(K) .NE. MAX_RELEASE(K)) THEN
                                                         MAX_RELEASE(K) = inf !1D200  !NEAR inf BECAUSE MULTIPLIED BY DELT WOULD CAUSE FLOATING OVERFLOW
              ELSEIF(MAX_RELEASE(K) < DZ) THEN
                                                         MAX_RELEASE(K) = inf
              END IF
              !
              RESBAL(J)%MAX_RELEASE  = MAX_RELEASE(K)
              RESBAL(J)%ELEV_CHNG    = ELEV_CHNG(K)
              RESBAL(J)%ELEV_CHNG_S  = ninf
              !
              IF(STORAGE_CAPACITY(K) .NE. STORAGE_CAPACITY(K)) STORAGE_CAPACITY(K) = DNEG
              IF(STORAGE_CAPACITY(K) .LE. DZ) THEN
                  IF (SWO%NRES_SPT(I) > Z) THEN
                      STORAGE_CAPACITY(K)     = DZ
                      DO JRES=1, SWO%NRES_SPT(I)
                            IMAX                = SWO%RESDAT(I)%ACAP(JRES)%ACAP_COUNT
                            STORAGE_CAPACITY(K) = STORAGE_CAPACITY(K) + SWO%RESDAT(I)%ACAP(JRES)%ACAP_STORAGE(IMAX)
                      END DO
                  ELSE
                      STORAGE_CAPACITY(K) = RESBAL(J)%ACAP_MAX_STOR
                  END IF
              END IF
              !
              IF( STORAGE_CAPACITY(K) > RESBAL(J)%ACAP_MAX_STOR ) STORAGE_CAPACITY(K) = RESBAL(J)%ACAP_MAX_STOR
              !
              HAS_SPILLWAY = TRUE
              !
              IF(STORAGE_SPILL(K) .NE. STORAGE_SPILL(K)) THEN
                                       STORAGE_SPILL(K) = STORAGE_CAPACITY(K)
                                       HAS_SPILLWAY = FALSE
              END IF

              IF(STORAGE_SPILL(K) < DZ) THEN
                 STORAGE_SPILL(K) = STORAGE_CAPACITY(K)
                 HAS_SPILLWAY = FALSE
              END IF
              !
              IF(STORAGE_SPILL(K) > STORAGE_CAPACITY(K)) THEN
                 !
                 IF(STORAGE_SPILL(K) > STORAGE_CAPACITY(K) + STORAGE_CAPACITY(K)*NEARZERO_14 ) HAS_SPILLWAY = FALSE  !Spill storage beyond capacity
                 !
                 STORAGE_SPILL(K) = STORAGE_CAPACITY(K)
              END IF
              !
              RESBAL(J)%MAX_STORAGE_S = inf
              RESBAL(J)%MAX_STAGE_S   = inf
              RESBAL(J)%SPILL_STAGE_S = inf
              RESBAL(J)%SPILL_STORE_S = inf
              RESBAL(J)%MIN_STORAGE_S = ninf
              RESBAL(J)%MIN_STAGE_S   = ninf
              RESBAL(J)%MIN_RELEASE_S = ninf
              RESBAL(J)%MIN_STORAGE_TRAN_S = ninf
              RESBAL(J)%RELEASE_SPEC_S     = ninf
              RESBAL(J)%STORAGE_CAPACITY   = STORAGE_CAPACITY(K)
              RESBAL(J)%STORAGE_MAX        = STORAGE_SPILL(K) !POINT WHEN FLOOD RELEASES ARE MADE
              !
              IF(HAS_SPILLWAY) THEN
                  RESBAL(J)%STORAGE_SPILL      = STORAGE_SPILL(K)
                  RESBAL(J)%STORAGE_SPILL_INPUT= STORAGE_SPILL(K)
              ELSE
                  RESBAL(J)%STORAGE_SPILL      = DNEG
                  RESBAL(J)%STORAGE_SPILL_INPUT= DNEG
              END IF
              !
              SWO%PROJ(I)%StorMax = SWO%PROJ(I)%StorMax  + STORAGE_SPILL(K)
              !
              RESBAL(J)%SPILLWAY_PREF = SPILL_PREF(K).NE.Z
              !
              IF(MAX_AREA(K) .NE. MAX_AREA(K)) MAX_AREA(K) = DNEG
              IF(MAX_AREA(K) .LE. DZ) THEN
                    IF (SWO%NRES_SPT(I) > Z) THEN
                        MAX_AREA(K)     = DZ
                        DO JRES=1, SWO%NRES_SPT(I)
                              IMAX        = ACAP(JRES)%ACAP_COUNT
                              MAX_AREA(K) = MAX_AREA(K) + ACAP(JRES)%ACAP_AREA(IMAX)
                        END DO
                    ELSE
                        IMAX        = ACAP(J)%ACAP_COUNT
                        MAX_AREA(K) = ACAP(J)%ACAP_AREA(IMAX)
                    END IF
              END IF
              !
              RESBAL(J)%MAX_AREA = MAX_AREA(K)
              !
          END DO
       END ASSOCIATE
    END DO
    DEALLOCATE(RES_INITIAL_STORAGE, MAX_RELEASE, STORAGE_CAPACITY, MAX_AREA,ALLOT_LIM)
    CALL WARN_MSG1%CHECK('SURFACE WATER OPERATIONS FATAL ERRORS. THE FOLLOWING ARE ERROR MESSAGES THAT INDICATE WHY SWO CAN NOT CONTINUE:'//BLN,BL%IU,BL%IOUT,KILL=TRUE,TAIL=BLN)
    !
    ! SET UP DEAD POOL STORAGE
    K = Z
    DO I = ONE,SWO%NPROJ
    ASSOCIATE(RESBAL=>SWO%RESDAT(I)%RESBAL, ACAP=>SWO%RESDAT(I)%ACAP)
    DO J = ONE,SWO%NRES_BAL(I)
        K = K + ONE
        !
        IF(STORAGE_DPL(K) < DZ) THEN
           IF (SWO%NRES_SPT(I) > Z) THEN
               TMP1 = DZ
               DO K=ONE, SWO%NRES_SPT(I) 
                   TMP1 = TMP1 + ACAP(K)%ACAP_STORAGE(ONE)
               END DO
               RESBAL(J)%STORAGE_DPL = TMP1
           ELSE
               RESBAL(J)%STORAGE_DPL = ACAP(J)%ACAP_STORAGE(ONE)
           END IF
        ELSE
            RESBAL(J)%STORAGE_DPL = STORAGE_DPL(K)
        END IF
        !
        RESBAL(J)%STORAGE_MIN_TRAN = RESBAL(J)%STORAGE_DPL   !If not defined storage transfers can happen until deadpool storage
        !
    END DO; END ASSOCIATE; END DO
    !
    DEALLOCATE(STORAGE_DPL)
    !
    ! Set Up Elevation Variables
    !
    K = Z
    DO I = ONE,SWO%NPROJ
    DO J = ONE,SWO%NRES_BAL(I)
        K = K + ONE
        ASSOCIATE(STORAGE => SWO%RESDAT(I)%RESBAL(J)%STORAGE_PREV, POOLFLG => SWO%RESDAT(I)%RESBAL(J)%RESBAL_POOLFLG)
             !
             IF (POOLFLG.EQ.1) THEN
               ! loop over split reservoirs ...
               DO JRES = 1,SWO%NRES_SPT(I)
                 TMP2 = YEAR_FRACTION(RES_START(K)%DYEAR)
                 !
                 CALL FRAC_POOL2SPLIT( SWO,I,JRES, TMP2, STORAGE, TMP1, ISLEAPYEAR(RES_START(K)%YEAR) )  !TMP1 HOLDS SPLIT FRAC
                 !
                 CALL ACAP_STOR2ELEV(SWO,I,JRES,TMP1,TMP2)  ! TMP2 holds ELEV
                 !
                 SWO%RESDAT(I)%RESSPLIT(JRES)%ELEV_PREV = TMP2
                 SWO%RESDAT(I)%RESSPLIT(JRES)%ELEV      = TMP2
                 !
                 IF(SWO%RESDAT(I)%RESSPLIT(JRES)%MAIN_RES) THEN
                     SWO%RESDAT(I)%RESBAL(J)%ELEV_PREV = TMP2
                     SWO%RESDAT(I)%RESBAL(J)%ELEV      = TMP2
                 END IF
                 !
               END DO !(JRES)
             ELSE
               ! compute area for mass balance reservoir
               CALL ACAP_STOR2ELEV(SWO,I,J,STORAGE,TMP2)
               !
               SWO%RESDAT(I)%RESBAL(J)%ELEV_PREV = TMP2
               SWO%RESDAT(I)%RESBAL(J)%ELEV      = TMP2
               !
             END IF !(POOLFLG.EQ.1)
        END ASSOCIATE
    END DO !(IRES)
    END DO !(IPROJ)
    !
    ! CHECK IF NO MAX SPILL WAS SET
    IF(NO_MAX_SPILL) THEN
        DO I=ONE, SWO%NPROJ
        DO J=ONE, SWO%NRES_BAL(I)
                  CALL SWO%RESDAT(I)%MAX_SPILL(J)%ALLOC(ONE,ONE,INF)
        END DO
        END DO
    ELSE
        DO I=ONE, SWO%NPROJ
        DO J=ONE, SWO%NRES_BAL(I)
        IF ( SWO%RESDAT(I)%RESBAL(J)%NO_MAX_SPILL_S ) THEN
              !
              ASSOCIATE( DIM1=>SWO%RESDAT(I)%MAX_SPILL(J)%N, DIM2=>SWO%RESDAT(I)%MAX_SPILL(J)%M, MAT=>SWO%RESDAT(I)%MAX_SPILL(J)%MAT, STOR=>SWO%RESDAT(I)%MAX_SPILL(J)%MAT(:,ONE) )
                  !
                  NO_MAX_SPILL = FALSE
                  DO K = TWO, DIM1
                                  IF( STOR(K) < STOR(K-1) ) NO_MAX_SPILL = TRUE
                  END DO
                  IF(NO_MAX_SPILL) CALL SORT(MAT, SORT_INDEX=ONE) !STORT ON COLUMN 1
                  !
              END ASSOCIATE
        END IF
        END DO
        END DO
    END IF
    !
    IF(WARN_MSG1%STR.NE.NL) THEN
        WARN_MSG1%RAISED = TRUE
        CALL WARN_MSG1%CHECK('SURFACE WATER OPERATIONS FATAL ERRORS. THE FOLLOWING ARE ERROR MESSAGES THAT INDICATE WHY SWO CAN NOT CONTINUE:'//BLN,BL%IU,BL%IOUT,KILL=TRUE,TAIL=BLN)
    END IF
    !
    !
    !
    IF(.NOT. ANY(SWO%RES_STORAGE_MIN%OPENED())) THEN
        K = Z
        DO I = ONE,SWO%NPROJ
        DO J = ONE,SWO%NRES_BAL(I)
            K = K + ONE
            CALL SWO%RES_STORAGE_MIN(K)%INIT( SWO%RESDAT(I)%RESBAL(J)%STORAGE_DPL )
        END DO; END DO
    END IF
    !
    IF(HAS_RESNAM)    THEN
         K=Z
         DO I = ONE, SWO%NPROJ
               DO J = ONE, SWO%NRES_BAL(I)
                   K = K + ONE
                   !
                   ! Write reservoir names to list file
                   WRITE(SWO%IOUT,*) "   RESNAME  = ", SWO%RESNAM(K)%STR
                   WRITE(SWO%IOUT,*) "   PROJID   = ", SWO%RESDAT(I)%RESBAL(J)%RESBAL_PROJID
                   WRITE(SWO%IOUT,*) "   RESID    = ", SWO%RESDAT(I)%RESBAL(J)%RESBAL_RESID
                   WRITE(SWO%IOUT,*) "   RELSEG   = ", SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG
                   WRITE(SWO%IOUT,*) "   STOR_PREV= ", SWO%RESDAT(I)%RESBAL(J)%STORAGE_PREV
                   WRITE(SWO%IOUT,*) "   HEADCELL = ", SWO%RESDAT(I)%RESBAL(J)%HEADCELL
               END DO
         END DO
    END IF
    !
    IF(HAS_SPLITNAM)  THEN
         !
         DO I = ONE, SWO%NPROJ
               DO J = ONE, SWO%NRES_SPT(I)
                   !
                   ! Write reservoir names to list file
                   WRITE(SWO%IOUT,*) "   RESNAME  = ", SWO%RESDAT(I)%RESSPLIT(J)%RESNAME
                   WRITE(SWO%IOUT,*) "   PROJID   = ", SWO%RESDAT(I)%RESSPLIT(J)%RESSPLIT_PROJID
                   WRITE(SWO%IOUT,*) "   RESID    = ", SWO%RESDAT(I)%RESSPLIT(J)%RESSPLIT_RESID
                   WRITE(SWO%IOUT,*) "   HEADCELL = ", SWO%RESDAT(I)%RESSPLIT(J)%HEADCELL
               END DO
         END DO
    END IF
    !
    SWO%NUM_PRNT_VAR = PRNT_VAR%LEN()
    IF(SWO%NUM_PRNT_VAR > Z) THEN
                                 CALL PRNT_VAR%TOARRAY( SWO%PRNT_VAR_NAM )
                                 CALL PRNT_FLG%TOARRAY( SWO%PRNT_VAR_FLG )
                                 !
                                 CALL PRNT_VAR%DESTROY()
                                 CALL PRNT_FLG%DESTROY()
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_VOL_STAGE_TSF(SWO, NRES, DAT, LINE, LLOC, IU, SCRATCH, DATE, DATEVEC, TEXT, MSG)
    CLASS(SWO_DATA),                            INTENT(INOUT):: SWO
    INTEGER,             DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: NRES
    DOUBLE PRECISION,    DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: DAT
    CHARACTER(*),                                  INTENT(INOUT):: LINE
    INTEGER,                                       INTENT(INOUT):: LLOC
    INTEGER,                                       INTENT(IN   ):: IU, SCRATCH
    TYPE(DATE_OPERATOR),                 OPTIONAL, INTENT(IN   ):: DATE
    TYPE(DATE_OPERATOR), DIMENSION(:),   OPTIONAL, INTENT(IN   ):: DATEVEC
    CHARACTER(*),                        OPTIONAL, INTENT(IN   ):: TEXT, MSG
    !
    TYPE(TIME_SERIES_FILE), DIMENSION(:), ALLOCATABLE:: TSF !ONLY USED ONCE IF USER USES THAT TO SPECIFY INITIAL STORAGE
    DOUBLE PRECISION:: DTMP
    INTEGER:: I, J, K, N
    LOGICAL:: IS_TIME_SERIES, INIT_AS_ELEV
    CHARACTER(20):: KEY
    CHARACTER(:), ALLOCATABLE:: ERROR
    !
    INIT_AS_ELEV   = FALSE
    IS_TIME_SERIES = FALSE   !FLAG TO INDICATE A TIME SERIES FILE IS LOOKED UP TO GET STARTING STORAGE
    !
    DO I=ONE, TWO  ! COLLECT KEYWORDS
        !
        CALL GET_WORD(KEY,LINE,LLOC,N)
        !
        SELECT CASE ( KEY )
        CASE("BY_VOLUME", "BYVOLUME", 'VOLUME')
                                 WRITE(SWO%IOUT,'(A)') '   BY_VOLUME KEYWORD FOUND INPUT SPECIFIED AS A VOLUME IN MODEL UNITS'
                                 !
        CASE("BY_STAGE","BY_ELEVATION","BY_ELEV","BYELEVATION","BYELEV","STAGE","ELEVATION")
                                 WRITE(SWO%IOUT,'(A)') '   BY_ELEVATION (or BY_STAGE) KEYWORD FOUND INPUT IS SPECIFIED AS A STAGE ELEVATION (IN MODEL UNITS) AND CONVERTED TO A VOLUME FROM THE ELEVATION_AREA_CAPACITY_TABLE.'
                                 !
                                 IF(ANY(SWO%NRES_SPT > Z))  CALL STOP_ERROR( LINE, IU, SWO%IOUT, MSG=KEY//' KEYWORD FOUND, BUT THIS INPUT FORMAT IS NOT ALLOWED FOR SPLIT RESERVOIR SYSTEMS. THERE IS AT LEAST ONE PROJECT THAT HAS "PROJECT_RESERVOIR_COUNT" WITH AN NEGATIVE COUNT. PLEASE EITHER SPECIFY PROPERTY WITH "BY_VOLUME" OPTION INSTEAD.', MSG2=MSG)
                                 !
                                 INIT_AS_ELEV = TRUE
                                 !
        CASE("TIME_SERIES","TIME_SERIES_FILE","TIME_SERIES_FILES")
                                 WRITE(SWO%IOUT,'(A)') '   TIME_SERIES KEYWORD FOUND. THE INITIAL STORAGE WILL BE PULLED FROM THE LOADED TIME SERIES FILE AND THE INITIAL MODEL STARTING DATE.'

                                 IS_TIME_SERIES = TRUE
        CASE DEFAULT
                                 IF(I==ONE .OR. IS_TIME_SERIES) WRITE(SWO%IOUT,'(A)') '   "BY_VOLUME" OR "BY_ELEVATION" KEYWORD NOT FOUND. ASSUMING "BY_VOLUME" AND INPUT WILL BE LOADED AS A VOLUME IN MODEL UNITS'
                                 LLOC = N
                                 IF(I==ONE) EXIT
        END SELECT
    END DO
    !
    I = Z  !TEMP VARIABLE USED FOR UNIT NUMBER...MUST BE ZERO TO TELL ULOAD TO QUERY FILE UNIT
    !
    IF(IS_TIME_SERIES) THEN
                            N = SIZE(DAT)
                            !
                            ALLOCATE(  TSF(N) )
                            !
                            CALL ULOAD(TSF, LLOC, LINE, SWO%IOUT, IU, I, SCRATCH=SCRATCH, TEXT=TEXT, MSG=MSG) !USE TEXT (='STEP') FUNCTION BY DEFAULT IF NOT SPECIFIED
                            !
                            IF(PRESENT(DATEVEC)) THEN
                                DO I = ONE, N                      !STARTING DATE
                                      CALL TSF(I)%GET(DATEVEC(I), DAT(I))
                                END DO
                            ELSE
                                DO I = ONE, N                      !STARTING DATE
                                      CALL TSF(I)%GET(DATE, DAT(I))
                                END DO
                            END IF
                            !
                            DEALLOCATE(TSF)
    ELSE
                            CALL ULOAD(DAT, LLOC, LINE, SWO%IOUT, IU, I, SCRATCH=SCRATCH)
    END IF
    !
    IF(INIT_AS_ELEV) THEN
       ERROR = NL
       K=Z
       DO I = ONE, SWO%NPROJ
             DO J = ONE, NRES(I)
                 K = K + ONE
                 !
                 CALL ACAP_ELEV2STOR(SWO, I, J, DAT(K), DTMP)  !NOT USING WARN_MSG1%STR AS A TEMP VARIABLE FOR ERROR CHECKING
                 !
                 DAT(K) = DTMP !DTMP HOLDS STORAGE
             END DO
       END DO
       !
       IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR, MSG2=MSG)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE ALLOCATE_NRES_N_RESDAT_ARRAYS(SWO, I, J, N)
    CLASS(SWO_DATA), INTENT(INOUT):: SWO
    INTEGER,         INTENT(INOUT):: I, J, N  !SCRATCH VARIABLES -- JUST TO SAVE ON PASSED MEMOPRY
    !
    ! Determine number of reservoirs simulated...
    !    NRES_BAL --> number simulated explicitly
    !    NRES_SPT --> number simulated as fraction of lumped pool (lumped/split)
    !
    !SWO%NRES_BAL = 0; SWO%NRES_SPT = 0
    DO I = ONE, SWO%NPROJ
          ! IRESFL=0
          ! --> no reservoirs simulated explicitly or lumped/split
          ! --> usable water for project allocation provided as tabfile input
          IF (SWO%IRESFL(I).EQ.Z) THEN
                                           SWO%NRES_BAL(I) = Z
                                           SWO%NRES_SPT(I) = Z
          ! --> all reservoirs simulated explicitly
          ! --> no reservoir simulated as lumped/split
          ELSE IF (SWO%IRESFL(I).GT.Z) THEN
                                           SWO%NRES_BAL(I) = SWO%IRESFL(I)
                                           SWO%NRES_SPT(I) = Z
          ! --> no reservoirs simulated explicitly
          ! --> all reservoirs simulated as lumped pool, split by specified fractions (FRAC)
          ELSE
                                           SWO%NRES_BAL(I) = ONE      ! single lumped storage pool!
                                           SWO%NRES_SPT(I) =-SWO%IRESFL(I)
          END IF
    END DO

    !    Allocate reservoir derived types ...
    !    NRES_TOT    --> total number of reservoirs **for each project** (number of storage values, ACAP tables, etc.)
    !    NRES_TOTTOT --> total number of reservoirs **in model** (number of storage values, ACAP tables, etc.)
    !    NRES_BALTOT --> number of reservoir storages computed by mass balance, including lumped/pooled storage (size of RESBAL type)
    !    NRES_SPTTOT --> number of reservoir storages computed by fractional split (size of RESSPLIT type)
    !
    SWO%NRES_TOTTOT = Z; SWO%NRES_BALTOT = Z; SWO%NRES_SPTTOT = Z
    DO I = ONE,SWO%NPROJ
                        SWO%NRES_BALTOT = SWO%NRES_BALTOT + SWO%NRES_BAL(I)
                        SWO%NRES_SPTTOT = SWO%NRES_SPTTOT + SWO%NRES_SPT(I)
                        !
                        IF (SWO%IRESFL(I).GT.Z) THEN
                                                     SWO%NRES_TOT(I) = SWO%NRES_BAL(I)
                                                     SWO%NRES_TOTTOT = SWO%NRES_TOTTOT + SWO%NRES_BAL(I)
                        ELSEIF (SWO%IRESFL(I).LT.Z) THEN
                                                    SWO%NRES_TOT(I) = SWO%NRES_SPT(I)
                                                    SWO%NRES_TOTTOT = SWO%NRES_TOTTOT + SWO%NRES_SPT(I)
                        END IF
    END DO
    !
    ALLOCATE(SWO%RESBAL2PROJ(TWO,SWO%NRES_BALTOT))
    N = Z
    DO I=ONE, SWO%NPROJ
        DO J=ONE, SWO%NRES_BAL(I)
            N = N + ONE
            SWO%RESBAL2PROJ(ONE,N) = I
            SWO%RESBAL2PROJ(TWO,N) = J
        END DO
    END DO
    !
    !
    ! ALLOCATE RESBAL
    DO I = ONE,SWO%NPROJ
        !
        IF (SWO%IRESFL(I).EQ.Z) THEN
            ! RESBAL (Explicit Reservoir Mass Balance)
            ALLOCATE( SWO%RESDAT(I)%RESBAL(ONE) )
            SWO%RESDAT(I)%NRESBAL = Z
            !
            ALLOCATE( SWO%RESDAT(I)%MAX_SPILL(ONE))
            !
            ! ACAP (Area-Capacity-Elevation Table)
            ALLOCATE( SWO%RESDAT(I)%ACAP(ONE) )
            ALLOCATE( SWO%RESDAT(I)%ACAP(ONE)%ACAP_STORAGE(ONE) )
            ALLOCATE( SWO%RESDAT(I)%ACAP(ONE)%ACAP_AREA(   ONE) )
            ALLOCATE( SWO%RESDAT(I)%ACAP(ONE)%ACAP_ELEV(   ONE) )
            ! FRAC (Reservoir Storage Fraction Table)
            ALLOCATE( SWO%RESDAT(I)%FRAC(ONE) )
            !ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_LEAP( ONE) )
            ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_NOLEAP( ONE) )
            ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_FRAC(   ONE) )
            ! RESSPLIT (Reservoir Mass Balance)
            ! (No pooled reservoirs for IRESFL>0 --> dummy alloc/init)
            ALLOCATE( SWO%RESDAT(I)%RESSPLIT(ONE) )
            ! HEAD (Reservoir Head Cells)
            ALLOCATE( SWO%RESDAT(I)%HEAD(ONE) )
            ALLOCATE( SWO%RESDAT(I)%HEAD(ONE)%HEAD_CELLS(ONE,ONE) )
            !
            !ALLOCATE( SWO%RESDAT(I)%RELEASE_FRAC(ONE))
            !
        ELSEIF (SWO%IRESFL(I)>Z) THEN
            !
            N = SWO%NRES_BAL(I)
            SWO%RESDAT(I)%NRESBAL = N
            !
            ALLOCATE( SWO%RESDAT(I)%MAX_SPILL(N) )
            !
            ALLOCATE( SWO%RESDAT(I)%RESBAL(N) )
            ALLOCATE( SWO%RESDAT(I)%ACAP(  N) )
            ALLOCATE( SWO%RESDAT(I)%HEAD(  N) )
            !
            !ALLOCATE( SWO%RESDAT(I)%RELEASE_FRAC(N))
            !
            ! RESSPLIT (Reservoir Mass Balance)
            ! (No pooled reservoirs for IRESFL>0 --> dummy alloc/init)
            ALLOCATE( SWO%RESDAT(I)%RESSPLIT(ONE) )
            ! FRAC (Reservoir Storage Fraction Table)
            ! (No pooled reservoirs for IRESFL>0 --> dummy alloc/init)
            ALLOCATE( SWO%RESDAT(I)%FRAC(ONE) )
            !ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_LEAP(ONE))
            ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_NOLEAP(ONE))
            ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_FRAC(ONE))
            !
            !DO J=1, SWO%NRES_BAL(I)
            !   ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_INFLOW )
            !   ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_PRCP )
            !   ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_EVAP )
            !   ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_RELEASE_SPEC )
            !   ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_STORAGE_NPRJ )
            !END DO
            !
        ELSE
            !
            N = SWO%NRES_SPT(I)
            J = ONE             !=> NRES = 1 DO TO SPLIT RESERVOIRS
            SWO%RESDAT(I)%NRESBAL = ONE
            !
            ALLOCATE( SWO%RESDAT(I)%MAX_SPILL(ONE))
            !
            ALLOCATE( SWO%RESDAT(I)%RESBAL(ONE) )
            ALLOCATE( SWO%RESDAT(I)%RESSPLIT(N) )
            ALLOCATE( SWO%RESDAT(I)%ACAP(N) )
            ALLOCATE( SWO%RESDAT(I)%FRAC(N) )
            !
            !ALLOCATE( SWO%RESDAT(I)%RELEASE_FRAC(ONE), SOURCE=UNO)
            !ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_LEAP(:) )
            !ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_NOLEAP(:) )
            !ALLOCATE( SWO%RESDAT(I)%FRAC(ONE)%FRAC_FRAC(:) )
            ALLOCATE( SWO%RESDAT(I)%HEAD(N) )
            !
            !ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_INFLOW )
            !ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_PRCP )
            !ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_EVAP )
            !ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_RELEASE_SPEC )
            !ALLOCATE( SWO%RESDAT(I)%RESBAL(J)%TABIDX_STORAGE_NPRJ )
        END IF
    END DO
    !
  END SUBROUTINE
  !
!  SUBROUTINE LOAD_DATE_ALLOCATION(SWO, LLOC, LINE, IU, ERROR_IU)
!    CLASS(SWO_DATA), INTENT(INOUT):: SWO
!    INTEGER,         INTENT(INOUT):: LLOC
!    CHARACTER(*),    INTENT(INOUT):: LINE
!    INTEGER,         INTENT(IN   ):: IU, ERROR_IU
!    TYPE(GENERIC_INPUT_FILE):: FL
!    TYPE(DATE_OPERATOR):: DATE
!    INTEGER:: I, ISTART, ISTOP
!    !
!    CALL FL%OPEN(LLOC, LINE, SWO%IOUT, ERROR_IU, NOSTOP=TRUE, REQKEY=TRUE)
!    IF(.NOT. FL%ERROR .AND. FL%IU==Z) FL%IU = IU  !FOUND INTERNAL KEYWORD
!    !
!    DO I=ONE, SWO%NPROJ
!        SWO%PROJ(I)%AllocDateFrac = IEEE_VALUE(SWO%PROJ(I)%AllocDateFrac, IEEE_QUIET_NAN)
!        CALL SWO%PROJ(I)%AllocDate%INIT('NO_DATE')
!        IF(FL%ERROR) THEN
!                        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE)
!                        CALL DATE%INIT(LINE(ISTART:ISTOP)//'/0001')
!                        IF(DATE%YEAR == -999) THEN
!                            CALL DATE%INIT(LINE(ISTART:ISTOP-FOUR)//'0001')
!                        END IF
!                        !
!                        IF(DATE%YEAR == -999) THEN
!                            CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,ERROR_IU,SWO%PROJ(I)%AllocDateFrac,TRUE, MSG='FAILED TO LOAD AFTER KEYWORD "ALLOCATION_DATE" THE AllocDate FOR A PROJECT. THIS CAN EITHER BE A FRACTION FROM ZERO TO ONE TO INDICATE THE PART OF THE YEAR THAT ALLOCATIONS START OR DATE OF THE FORM MM/DD TO MATCH CALENDAR DATE. NOTE THIS IS READ NPROJ TIMES.')
!                        ELSE
!                            SWO%PROJ(I)%AllocDate = DATE
!                            IF(DATE_SP(1)%TS(0)%DATE.EQ.'NO_DATE') CALL STOP_ERROR(INFILE=IU, OUTPUT=SWO%IOUT,MSG='SWO INPUT ERROR FOR KEYWORD "ALLOCATION_DATE". A CALENDAR DATE WAS DETECTED AS INPUT (MM/DD), BUT YOU DID NOT SPECIFY IN THE DIS PACAKGE A STARTING CALENDARY DATE WITH THEY KEYWORD "STARTDATE". EITHER SPECIFY THE ALLOCATION_DATE AS BEIGN A FRACTION OF THE YEAR OR SPECIFY A STARTING CALENDARY DATE FOR THE MODEL.')
!                        END IF
!        ELSE
!                        CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!                        LLOC=ONE
!                        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)      !PASS PROJECT NUMBER
!                        CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP,TRUE) !LOAD DATE
!                        CALL DATE%INIT(LINE(ISTART:ISTOP)//'/0001')
!                        IF(DATE%YEAR == -999) THEN
!                            CALL DATE%INIT(LINE(ISTART:ISTOP-FOUR)//'0001')
!                        END IF
!                        !
!                        IF(DATE%YEAR == -999) THEN
!                            CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,ERROR_IU,SWO%PROJ(I)%AllocDateFrac,TRUE, MSG='FAILED TO LOAD AFTER KEYWORD "ALLOCATION_DATE" THE AllocDate FOR A PROJECT. THIS CAN EITHER BE A FRACTION FROM ZERO TO ONE TO INDICATE THE PART OF THE YEAR THAT ALLOCATIONS START OR DATE OF THE FORM MM/DD TO MATCH CALENDAR DATE. NOTE THIS IS READ NPROJ TIMES.')
!                        ELSE
!                            SWO%PROJ(I)%AllocDate = DATE
!                            IF(DATE_SP(1)%TS(0)%DATE.EQ.'NO_DATE') CALL STOP_ERROR(INFILE=IU, OUTPUT=SWO%IOUT,MSG='SWO INPUT ERROR FOR KEYWORD "ALLOCATION_DATE". A CALENDAR DATE WAS DETECTED AS INPUT (MM/DD), BUT YOU DID NOT SPECIFY IN THE DIS PACAKGE A STARTING CALENDARY DATE WITH THEY KEYWORD "STARTDATE". EITHER SPECIFY THE ALLOCATION_DATE AS BEIGN A FRACTION OF THE YEAR OR SPECIFY A STARTING CALENDARY DATE FOR THE MODEL.')
!                        END IF
!        END IF
!    END DO
!  END SUBROUTINE
  !
  SUBROUTINE LOAD_UNIT_CHARGE_CREDIT(SWO, LINE, IU, SFR_ID)
    CLASS(SWO_DATA),           INTENT(INOUT):: SWO
    CHARACTER(*),              INTENT(INOUT):: LINE
    INTEGER,                   INTENT(IN   ):: IU
    CLASS(SFR_NAMED_LOCATION), INTENT(IN   ):: SFR_ID
    INTEGER:: I, J, LLOC, ISTART, ISTOP
    !
    IF(IU .NE. Z) THEN   !IU = Z INDICATES THAT REPEAT KEYWORD FOUND.
          !
          CALL SWO%UNIT%DEALLOCATE() !DEALLOCATE INTERNAL ARRAYS THAT WILL BE REALLOCATED
          !
          DO I=ONE, SWO%NUNIT
              CALL READ_TO_DATA(LINE, IU, SWO%IOUT)
              LLOC = ONE
              CALL PARSE_WORD( LINE,LLOC,ISTART,ISTOP) !PASS UNIT ID
              CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,IU,SWO%UNIT(I)%NBySeg, MSG='FAILED TO LOAD NBySeg FOR UNIT')
              CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,IU,SWO%UNIT(I)%NCrdSeg,MSG='FAILED TO LOAD NCrdSeg FOR UNIT')
              IF(SWO%UNIT(I)%NBySeg > Z) THEN
                 ALLOCATE( SWO%UNIT(I)%BySeg    (SWO%UNIT(I)%NBySeg),  &
                           SWO%UNIT(I)%ByRch    (SWO%UNIT(I)%NBySeg),  &
                           SWO%UNIT(I)%ByFactor (SWO%UNIT(I)%NBySeg)   )
              END IF
              IF(SWO%UNIT(I)%NCrdSeg > Z) THEN
                 ALLOCATE( SWO%UNIT(I)%CrdSeg   (SWO%UNIT(I)%NCrdSeg), &
                           SWO%UNIT(I)%CrdRch   (SWO%UNIT(I)%NCrdSeg), &
                           SWO%UNIT(I)%CrdFactor(SWO%UNIT(I)%NCrdSeg)  )
              END IF
              !
              ! READ UNIT DIVERSION SEGMENT --------------------------------------------------------------------------------------------------
              !
              CALL READ_TO_DATA(LINE, IU, SWO%IOUT)
              LLOC = ONE
              CALL SFR_ID%GET(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%DivSeg, SWO%UNIT(I)%DivRch, TRUE)
              !
              ! READ UNIT CHARGE SEGMENT -----------------------------------------------------------------------------------------------------
              !
              CALL READ_TO_DATA(LINE, IU, SWO%IOUT)
              LLOC = ONE
              CALL SFR_ID%GET(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%ChgSeg, SWO%UNIT(I)%ChgRch, TRUE)
              CALL GET_NUMBER(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%ChgFactor, ERROR_VAL=UNO, MSG='FAILED TO LOAD ChgFactor FOR UNIT')
              !
              ! READ UNIT ByPass SEGMENT -----------------------------------------------------------------------------------------------------
              !
              DO J=ONE, SWO%UNIT(I)%NBySeg
                    CALL READ_TO_DATA(LINE, IU, SWO%IOUT)
                    LLOC = ONE
                    CALL SFR_ID%GET(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%BySeg(J), SWO%UNIT(I)%ByRch(J), TRUE)
                    CALL GET_NUMBER(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%ByFactor(J), ERROR_VAL=UNO, MSG='FAILED TO LOAD ByFactor FOR UNIT')
              END DO
              !
              ! READ UNIT CREDIT SEGMENT -----------------------------------------------------------------------------------------------------
              !
              DO J=ONE, SWO%UNIT(I)%NCrdSeg
                    CALL READ_TO_DATA(LINE, IU, SWO%IOUT)
                    LLOC = ONE
                    CALL SFR_ID%GET(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%CrdSeg(J), SWO%UNIT(I)%CrdRch(J), TRUE)
                    CALL GET_NUMBER(LINE, LLOC, ISTART, ISTOP, SWO%IOUT, IU, SWO%UNIT(I)%CrdFactor(J), ERROR_VAL=UNO, MSG='FAILED TO LOAD CrdFactor FOR UNIT')
              END DO
             !
          END DO
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE SETUP_NEXT_STRESS_PERIOD(SWO, WBS, SWF, KPER, IDIVAR, IOTSG, SEG, ISTRM, STRM)
    CLASS(SWO_DATA),                      INTENT(INOUT):: SWO
    TYPE(WBS_DATA),                       INTENT(IN   ):: WBS
    TYPE(SURFACE_WATER_DATA),             INTENT(IN   ):: SWF
    INTEGER,                              INTENT(IN   ):: KPER
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
    REAL,   DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: SEG
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: ISTRM
    REAL,   DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: STRM
    LOGICAL:: UNIT_OPTION, DIVSEG_ALLOC, DIVSEG_UnitID_ALLOC, SPTSEG_ALLOC
    !
    TYPE(INTEGER_LINKED_LIST):: LIST
    INTEGER:: I,J,K,F,NRES,ISEG,IRCH,TMP1,TMP2,TMP3,IRES,IPROJ,IUNIT,IDVS,COUNTER,JSEG,NTREE,NLIST,IBRC,TMPUNIT,IREQ
    DOUBLE PRECISION:: DTMP
    LOGICAL:: UPDATE
    CHARACTER(:), ALLOCATABLE:: ERROR
    !
    IF(SWO%TFR_READ) THEN
        !
        UPDATE = FALSE
        !
        CALL SWO%SEG_TYP%NEXT()
        CALL SWO%RESBAL_RELSEG    %NEXT(SWO%SFR_ID)
        CALL SWO%RESBAL_INFLOW_SEG%NEXT(SWO%SFR_ID)
        CALL SWO%RELEASE_DMD_FRAC%NEXT()
        CALL SWO%RELEASE_REQ_FRAC%NEXT()
        CALL SWO%PRECIP_AREA_FRAC%NEXT()
        !
        CALL SWO%AUX_SFR_DELV%NEXT(SWO%SFR_ID)
        CALL SWO%UNIT_ACCOUNTING%NEXT(SWO%SFR_ID)
        !
        CALL SWO%DEC_RUL_SP%NEXT()
        CALL SWO%DEC_RUL_TS%NEXT()
        CALL SWO%DEC_RUL_IT%NEXT()
        CALL SWO%DEC_RUL_IT_END  %NEXT()
        CALL SWO%DEC_RUL_TS_END  %NEXT()
        CALL SWO%DEC_RUL_CLOSEOUT%NEXT()
        !
        CALL SWO%DIST_ALLOC_FRAC%NEXT()
        !
        IF(SWO%UNIT_ACCOUNTING%RAN_LOAD) CALL LOAD_UNIT_CHARGE_CREDIT(SWO, SWO%UNIT_ACCOUNTING%TFR%LN, SWO%UNIT_ACCOUNTING%IU, SWO%SFR_ID)  !NEEDED TO FINISH THE LOADING BECAUSE UNIT IS ARRAY STRUCTURE THAT DOES NOT WORK WELL WITH THE UNIT_ACCOUNTING%LOAD()
        !
        ! Make the Decision rules upper case to make case insentive
        !
        CALL SWO%DEC_RUL_SP      %PREPARE_RULES()
        CALL SWO%DEC_RUL_TS      %PREPARE_RULES()
        CALL SWO%DEC_RUL_IT      %PREPARE_RULES()
        CALL SWO%DEC_RUL_IT_END  %PREPARE_RULES()
        CALL SWO%DEC_RUL_TS_END  %PREPARE_RULES()
        CALL SWO%DEC_RUL_CLOSEOUT%PREPARE_RULES()
        !
    ELSE
        !
        UPDATE = TRUE
        !
        SWO%TFR_READ = TRUE
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    DO IPROJ=ONE, SWO%NPROJ          
    DO IRES =ONE, SWO%NRES_BAL(IPROJ)
               IF( KPER < SWO%RESDAT(IPROJ)%RESBAL(IRES)%SPSTART) THEN
                   SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE = FALSE
               ELSE
                   SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE = TRUE
               END IF
    END DO
    END DO
    !
    IF(UPDATE .OR. WBS%HIERARCHY_TFR%TRANSIENT) THEN
        DO I=ONE, WBS%NUNIT
              SWO%UNIT(I)%UnitID = I
              SWO%UNIT(I)%DistID = WBS%UNIT(I)%DIST
              SWO%UNIT(I)%ProjID = WBS%UNIT(I)%PROJ
        END DO
        !
        DO I=ONE, WBS%NDIST
              SWO%DIST(I)%DistID = I
              SWO%DIST(I)%ProjID = WBS%DIST(I)%PROJ
              !
              IF(SWO%DIST(I)%NFARM .NE. WBS%DIST(I)%NFARM) THEN
                  IF(SWO%DIST(I)%NFARM > Z) DEALLOCATE(SWO%DIST(I)%FARM, STAT = J)
                  IF(WBS%DIST(I)%NFARM > Z)   ALLOCATE(SWO%DIST(I)%FARM, SOURCE = WBS%DIST(I)%FARM)
                  SWO%DIST(I)%NFARM = WBS%DIST(I)%NFARM
              ELSEIF(SWO%DIST(I)%NFARM > Z) THEN
                  SWO%DIST(I)%FARM = WBS%DIST(I)%FARM
              END IF
              !
              IF(SWO%DIST(I)%NAUXDEM .NE. WBS%DIST(I)%NAUX) THEN
                  IF(SWO%DIST(I)%NAUXDEM > Z) DEALLOCATE(SWO%DIST(I)%AUXDEM, STAT = J)
                  IF(WBS%DIST(I)%NAUX    > Z)   ALLOCATE(SWO%DIST(I)%AUXDEM, SOURCE = WBS%DIST(I)%AUX)
                  SWO%DIST(I)%NAUXDEM = WBS%DIST(I)%NAUX
              ELSEIF(SWO%DIST(I)%NAUXDEM > Z) THEN
                  SWO%DIST(I)%AUXDEM = WBS%DIST(I)%AUX
              END IF
               !
        END DO
        !
        DO I=ONE, WBS%NPROJ
              SWO%PROJ(I)%ProjID = I
        END DO
    END IF
    !
    IF(UPDATE .OR. SWO%SEG_TYP%TRANSIENT) THEN
          DO I=ONE, SWO%NSEG
                    SWO%SEGINFO(I)%SegType = SWO%SEG_TYP%LIST(I)
          END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    IF(UPDATE) THEN
          DO CONCURRENT (I=ONE:SWO%NSEG)
              SWO%SEGINFO(I)%SegID   = I
              SWO%SEGINFO(I)%ProjID  = Z
              SWO%SEGINFO(I)%DistID  = Z
              SWO%SEGINFO(I)%UnitID  = Z
              SWO%SEGINFO(I)%FarmID  = Z
              SWO%SEGINFO(I)%AuxID   = Z
          END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    IF(UPDATE .OR. WBS%HIERARCHY_TFR%TRANSIENT .OR. WBS%H2OSOURCE_TFR%TRANSIENT .OR. SWF%ISRD_TFR%TRANSIENT) THEN
        DO I=ONE, SWO%NSEG
            SWO%SEGINFO(I)%FarmID  = Z
            IF(SWO%SEGINFO(I)%AuxID==Z) THEN
                  SWO%SEGINFO(I)%ProjID  = Z
                  SWO%SEGINFO(I)%DistID  = Z
                  SWO%SEGINFO(I)%UnitID  = Z
            END IF
        END DO
        DO F=ONE, WBS%NFARM
              SWO%FARM(F)%FarmID = F
              SWO%FARM(F)%UnitID = WBS%FARM(F)%UNIT
              SWO%FARM(F)%DistID = WBS%FARM(F)%DIST
              SWO%FARM(F)%ProjID = WBS%FARM(F)%PROJ
              !
              IF(WBS%H2OSOURCE%SW(F) .AND. SWF%SRDLOC(F)%N > Z) THEN
                  !
                  SWO%FARM(F)%DelSeg = ISTRM(4,SWF%SRDLOC(F)%ISTRM(1))  !ONLY WILL DEVLIVER TO FIRST SRD
                  SWO%FARM(F)%DelRch = ISTRM(5,SWF%SRDLOC(F)%ISTRM(1))
                  !SWO%FARM(F)%DelSeg = ISRD(FOUR,F)
                  !SWO%FARM(F)%DelRch = ISRD(FIVE,F)
!!!          DO K = ONE, SWFL%SRDLOC(F)%N
!!!             ASSOCIATE(S  => ISTRM(4,SWF%SRDLOC(F)%ISTRM(K)),
!!!     +                 R  => ISTRM(5,SWF%SRDLOC(F)%ISTRM(K)),
!!!     +                 WT => SWFL%SRDLOC(F)%WT(K)            )
!!!                       !
!!!               IF(WT>-0.0001) THEN !0.123456
!!!                   WRITE(IOUT,'(3I5, 1x F9.6)') F, S, R, WT
!!!               ELSE
!!!                   WRITE(IOUT,'(3I5, 2x A)'   ) F, S, R, '--'
!!!               END IF
!!!               !
!!!             END ASSOCIATE
!!!          END DO
              ELSE
                  SWO%FARM(F)%DelSeg = Z
                  SWO%FARM(F)%DelRch = Z
              END IF
              !
              IF (SWO%FARM(F)%DelSeg > Z) THEN
                  SWO%SEGINFO(SWO%FARM(F)%DelSeg)%FarmID = F
                  SWO%SEGINFO(SWO%FARM(F)%DelSeg)%ProjID = WBS%FARM(F)%PROJ
                  SWO%SEGINFO(SWO%FARM(F)%DelSeg)%DistID = WBS%FARM(F)%DIST
                  SWO%SEGINFO(SWO%FARM(F)%DelSeg)%UnitID = WBS%FARM(F)%UNIT
              END IF
        END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    IF(UPDATE .OR. WBS%HIERARCHY_TFR%TRANSIENT .OR. SWO%AUX_SFR_DELV%TRANSIENT) THEN
        DO I=ONE, SWO%NSEG
            SWO%SEGINFO(I)%AuxID   = Z
            IF(SWO%SEGINFO(I)%FarmID==Z) THEN
                  SWO%SEGINFO(I)%ProjID  = Z
                  SWO%SEGINFO(I)%DistID  = Z
                  SWO%SEGINFO(I)%UnitID  = Z
            END IF
        END DO
        DO I=ONE, WBS%NAUXDEM
              !F = SWO%NFARM + I
              SWO%AUXDEM(I)%AuxID  = I
              SWO%AUXDEM(I)%UnitID = WBS%AUX_DEMAND(I)%UNIT
              SWO%AUXDEM(I)%DistID = WBS%AUX_DEMAND(I)%DIST
              SWO%AUXDEM(I)%ProjID = WBS%AUX_DEMAND(I)%PROJ
              SWO%AUXDEM(I)%AuxSeg = SWO%AUX_SFR_DELV%SEG(I)
              SWO%AUXDEM(I)%AuxRch = SWO%AUX_SFR_DELV%RCH(I)
              SWO%AUXDEM(I)%Factor = SWO%AUX_SFR_DELV%Factor(I)
              !
              IF (SWO%AUX_SFR_DELV%SEG(I) > Z) THEN
                  SWO%SEGINFO(SWO%AUX_SFR_DELV%SEG(I))%AuxID = I
                  SWO%SEGINFO(SWO%AUX_SFR_DELV%SEG(I))%ProjID = WBS%AUX_DEMAND(I)%PROJ
                  SWO%SEGINFO(SWO%AUX_SFR_DELV%SEG(I))%DistID = WBS%AUX_DEMAND(I)%DIST
                  SWO%SEGINFO(SWO%AUX_SFR_DELV%SEG(I))%UnitID = WBS%AUX_DEMAND(I)%UNIT
              END IF
        END DO
    END IF
    !
    DO I=ONE, WBS%NUNIT
    IF ( SWO%UNIT(I)%DivSeg > Z ) THEN
              SWO%SEGINFO(SWO%UNIT(I)%DivSeg)%ProjID = WBS%UNIT(I)%PROJ
              SWO%SEGINFO(SWO%UNIT(I)%DivSeg)%DistID = WBS%UNIT(I)%DIST
              SWO%SEGINFO(SWO%UNIT(I)%DivSeg)%UnitID = I
    END IF
    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    IF(UPDATE .OR. SWO%RESBAL_RELSEG%TRANSIENT) THEN
        !
        !!!DO I = ONE,SIZE(SWO%RESBAL_RELSEG%SEG) - ONE
        !!!    !
        !!!    IF(SWO%RESBAL_RELSEG%SEG(I) > Z .AND. ANY(SWO%RESBAL_RELSEG%SEG(I)==SWO%RESBAL_RELSEG%SEG(I+ONE:))) THEN
        !!!          !
        !!!          CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='RESERVOIR_RELEASE_SEGMENT INPUT MUST SPECIFY A UNIQUE RELEASE SEGMENT FOR EACH RESERVOIR (ZERO IS ALLOWED MORE THEN ONCE). THE FOLLOWING SEGMENT WAS SPECIFED MORE THAN ONCE: '//NUM2STR(SWO%RESBAL_RELSEG%SEG(I)))
        !!!    END IF
        !!!END DO
        !
        I=Z
        DO IPROJ = 1,SWO%NPROJ
          DO IRES = 1,SWO%NRES_BAL(IPROJ)
            I=I+1
            !
            IF(SWO%RESBAL_RELSEG%SEG(I) < Z) SWO%RESBAL_RELSEG%SEG(I) = Z
            !
            SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG = SWO%RESBAL_RELSEG%SEG(I)
            !
          END DO
        END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    IF(SWO%RESBAL_INFLOW_SEG%INUSE) THEN
        !
        IF(UPDATE .OR. SWO%RESBAL_INFLOW_SEG%TRANSIENT) THEN
            !
            DO I = ONE,SIZE(SWO%RESBAL_INFLOW_SEG%SEG) - ONE
                !
                IF(SWO%RESBAL_INFLOW_SEG%SEG(I) > Z .AND. ANY(SWO%RESBAL_INFLOW_SEG%SEG(I)==SWO%RESBAL_INFLOW_SEG%SEG(I+ONE:))) THEN
                      !
                      CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='RESERVOIR_SFR_INFLOW_SEGMENT INPUT MUST SPECIFY A UNIQUE SFR SEGMENT FOR EACH RESERVOIR (ZERO IS ALLOWED MORE THEN ONCE AND NEGATIVE IS AUTOMATICALY CHANGED TO ZERO). THE FOLLOWING SEGMENT WAS SPECIFED MORE THAN ONCE: '//NUM2STR(SWO%RESBAL_INFLOW_SEG%SEG(I)))
                END IF
            END DO
            !
            I=Z
            DO IPROJ = 1,SWO%NPROJ
              DO IRES = 1,SWO%NRES_BAL(IPROJ)
                I=I+1
                !
                IF( SWO%RESBAL_INFLOW_SEG%SEG(I) < Z) SWO%RESBAL_INFLOW_SEG%SEG(I) = Z
                !
                SWO%RESDAT(IPROJ)%RESBAL(IRES)%SFR_INFLOW_SEG = SWO%RESBAL_INFLOW_SEG%SEG(I)
                !
              END DO
            END DO
        END IF
        !
        DO IPROJ = 1,SWO%NPROJ
          DO IRES = 1,SWO%NRES_BAL(IPROJ)
            ASSOCIATE( RESBAL=>SWO%RESDAT(IPROJ)%RESBAL(IRES) )
                !
                IF( RESBAL%SFR_INFLOW_SEG > Z) THEN
                    RESBAL%SFR_INFLOW_ISTRM = SWO%SEGRCH_OUT( RESBAL%SFR_INFLOW_SEG )
                ELSE
                    RESBAL%SFR_INFLOW_ISTRM = Z
                END IF
            END ASSOCIATE
          END DO
        END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    !
    ! ****************************************************************
    ! UPDATE SFR INPUTS FOR CURRENT STRESS PERIOD
    ! (set all FLOWDATA vars from SFR input file at start of period)
    ! ****************************************************************

    DO ISEG = ONE, SWO%NSEG

        ! Check for segment numbering issue
        IF (SWO%SEGDATA(ISEG)%SegID.EQ.ISEG) THEN
          CONTINUE
        ELSE
          WRITE(SWO%IOUT,*) " "
          WRITE(SWO%IOUT,*) "ERROR:"
          WRITE(SWO%IOUT,*) "Mismatch between ISEG and SEGDATA(ISEG)%SegID"
          WRITE(SWO%IOUT,*) "May indicate change in SFR segments?"
          WRITE(SWO%IOUT,*) "Could be something else entirely..."
          WRITE(SWO%IOUT,*) "If you SFR file looks OK, then just blame the programmers."
          WRITE(SWO%IOUT,*) "[Argh! Those knucklehead programmers!]"
          WRITE(SWO%IOUT,*) " "
          WRITE(SWO%IOUT,*) "Stopping model..."
          CALL USTOP(' ')
        END IF !(SegID.EQ.ISEG)

        ! Grab values for current period
        SWO%SEGDATA(ISEG)%IUPSEG     = IDIVAR(1,ISEG)
        SWO%SEGDATA(ISEG)%OUTSEG     = IOTSG(ISEG)
        SWO%SEGDATA(ISEG)%IPRIOR     = IDIVAR(2,ISEG)
        SWO%SEGDATA(ISEG)%LENGTH     = SEG(1,ISEG)
        SWO%SEGDATA(ISEG)%FLOW       = SEG(2,ISEG)
        SWO%SEGDATA(ISEG)%RUNOFF     = SEG(3,ISEG)
        SWO%SEGDATA(ISEG)%STRM_ET    = SEG(4,ISEG)
        SWO%SEGDATA(ISEG)%STRM_PRCP  = SEG(5,ISEG)

        ! Check for changes in IUPSEG/OUTSEG/IPRIOR...
        IF (KPER.GT.1) THEN

          TMP1 = -99; TMP2 = -99; TMP3 = -99
          IF (SWO%SEGDATA(ISEG)%IUPSEG.NE.SWO%SEGDATA_PREV(ISEG)%IUPSEG) TMP1=1
          IF (SWO%SEGDATA(ISEG)%OUTSEG.NE.SWO%SEGDATA_PREV(ISEG)%OUTSEG) TMP2=1
          IF (SWO%SEGDATA(ISEG)%IPRIOR.NE.SWO%SEGDATA_PREV(ISEG)%IPRIOR) TMP3=1
          IF ((TMP1.GT.0).OR.(TMP2.GT.0).OR.(TMP3.GT.0)) THEN
            WRITE(SWO%IOUT,*) " "
            WRITE(SWO%IOUT,*) "WARNING:"
            WRITE(SWO%IOUT,*) "SFR network dependencies have changed..."
            WRITE(SWO%IOUT,*) "SEGMENT:", ISEG
            IF (TMP1.GT.0) WRITE(SWO%IOUT,*) "-> IUPSEG changed"
            IF (TMP2.GT.0) WRITE(SWO%IOUT,*) "-> OUTSEG changed"
            IF (TMP3.GT.0) WRITE(SWO%IOUT,*) "-> IPRIOR changed"
            WRITE(SWO%IOUT,*) "This may result bad conveyance efficiency values at initial iteration "
            WRITE(SWO%IOUT,*) "of initial timestep for period."
            WRITE(SWO%IOUT,*) " "
            WRITE(*,*)    " "
            WRITE(*,*)    "WARNING:"
            WRITE(*,*)    "SFR network dependencies have changed..."
            WRITE(*,*)    "SEGMENT:", ISEG
            IF (TMP1.GT.0) WRITE(*,*) "-> IUPSEG changed"
            IF (TMP2.GT.0) WRITE(*,*) "-> OUTSEG changed"
            IF (TMP3.GT.0) WRITE(*,*) "-> IPRIOR changed"
            WRITE(*,*)    "This may result bad conveyance efficiency values at initial iteration "
            WRITE(*,*)    "of initial timestep for period."
            WRITE(*,*)    " "
          END IF !(IUPSEG/OUTSEG/IPRIOR check)
        END IF !(KPER.GT.1)

        SWO%SEGDATA_PREV(ISEG)%IUPSEG    = IDIVAR(1,ISEG)
        SWO%SEGDATA_PREV(ISEG)%OUTSEG    = IOTSG(ISEG)
        SWO%SEGDATA_PREV(ISEG)%IPRIOR    = IDIVAR(2,ISEG)
        SWO%SEGDATA_PREV(ISEG)%LENGTH    = SEG(1,ISEG)
        SWO%SEGDATA_PREV(ISEG)%FLOW      = SEG(2,ISEG)
        SWO%SEGDATA_PREV(ISEG)%RUNOFF    = SEG(3,ISEG)
        SWO%SEGDATA_PREV(ISEG)%STRM_ET   = SEG(4,ISEG)
        SWO%SEGDATA_PREV(ISEG)%STRM_PRCP = SEG(5,ISEG)

        SWO%SEGDATA_SAVE(ISEG)%IUPSEG    = IDIVAR(1,ISEG)
        SWO%SEGDATA_SAVE(ISEG)%OUTSEG    = IOTSG(ISEG)
        SWO%SEGDATA_SAVE(ISEG)%IPRIOR    = IDIVAR(2,ISEG)
        SWO%SEGDATA_SAVE(ISEG)%LENGTH    = SEG(1,ISEG)
        SWO%SEGDATA_SAVE(ISEG)%FLOW      = SEG(2,ISEG)
        SWO%SEGDATA_SAVE(ISEG)%RUNOFF    = SEG(3,ISEG)
        SWO%SEGDATA_SAVE(ISEG)%STRM_ET   = SEG(4,ISEG)
        SWO%SEGDATA_SAVE(ISEG)%STRM_PRCP = SEG(5,ISEG)

      END DO !(ISEG)

      DO IRCH = 1,SWO%NSTRM
        IF ( (SWO%RCHDATA(IRCH)%RchID.EQ.IRCH) .AND. (SWO%RCHDATA(IRCH)%RchSeg.EQ.ISTRM(4,IRCH)) ) THEN
          CONTINUE
        ELSE
          WRITE(SWO%IOUT,*) " "
          WRITE(SWO%IOUT,*) "ERROR:"
          WRITE(SWO%IOUT,*) "Mismatch between RCHDATA(ISEG)%RchID and IRCH"
          WRITE(SWO%IOUT,*) "or"
          WRITE(SWO%IOUT,*) "Mismatch between RCHDATA(ISEG)%RchSeg and ISTRM(4,IRCH)"
          WRITE(SWO%IOUT,*) "May indicate change in SFR segments?"
          WRITE(SWO%IOUT,*) "Could be something else entirely..."
          WRITE(SWO%IOUT,*) "If you SFR file looks OK, then just blame the programmers."
          WRITE(SWO%IOUT,*) "[Argh! Those knucklehead programmers!]"
          WRITE(SWO%IOUT,*) " "
          WRITE(SWO%IOUT,*) "Stopping model..."
          CALL USTOP(' ')
        END IF !((RchID.EQ.ISEG).AND.(RchSeg.EQ.ISTRM(4,IRCH))

        SWO%RCHDATA(IRCH)%LENGTH        = STRM( 1,IRCH)
        SWO%RCHDATA(IRCH)%RUNOFF        = STRM(12,IRCH) + STRM(24,IRCH)
        SWO%RCHDATA(IRCH)%STRM_ET       = STRM(13,IRCH)
        SWO%RCHDATA(IRCH)%STRM_PRCP     = STRM(14,IRCH)

        SWO%RCHDATA_PREV(IRCH)%LENGTH   = STRM( 1,IRCH)
        SWO%RCHDATA_PREV(IRCH)%RUNOFF   = STRM(12,IRCH) + STRM(24,IRCH)
        SWO%RCHDATA_PREV(IRCH)%STRM_ET  = STRM(13,IRCH)
        SWO%RCHDATA_PREV(IRCH)%STRM_PRCP= STRM(14,IRCH)

        SWO%RCHDATA_SAVE(IRCH)%LENGTH   = STRM( 1,IRCH)
        SWO%RCHDATA_SAVE(IRCH)%RUNOFF   = STRM(12,IRCH) + STRM(24,IRCH)
!        RCHDATRA_SAVE(IRCH)%RUNOFF
!     +    SEG(3,ISTRM(4,IRCH))*(STRM(1,IRCH)/SEG(1,ISTRM(4,IRCH)))
        SWO%RCHDATA_SAVE(IRCH)%STRM_ET  = STRM(13,IRCH)
        SWO%RCHDATA_SAVE(IRCH)%STRM_PRCP= STRM(14,IRCH)

      END DO !(IRCH)
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1

      ! Set flag for release segment ...
      DO IPROJ=1, SWO%NPROJ
      DO IRES =1, SWO%NRES_BAL(IPROJ)
      IF ( SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE .AND. SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG > Z ) THEN
                 ISEG = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG
                 SWO%SEGDATA(ISEG)%RRFLAG      = 1
                 SWO%SEGDATA_PREV(ISEG)%RRFLAG = 1
                 SWO%SEGDATA_SAVE(ISEG)%RRFLAG = 1
      END IF
      END DO
      END DO 
    !
    !----------------------------------------------------------------------------------------------
    !
      !
      ! ****************************************************************
      ! MAP SURFACE NETWORK -- UPSTREAM+DOWNSTREAM SEGMENTS/TREES
      ! ****************************************************************

      ! NOTE --
      ! Mapping done at each stress period...
      ! Required for models where SFR network changes during simulation

      ! TODO --
      ! Move mapping inside IF statement so that it is only carried out
      ! at FIRST stress period when SFR network is constant or at *ALL*
      ! stress periods when SFR network varies

      ! TODO --
      ! Update mapping routines to pull from FLOWDATA_SAVE rather than
      ! directly from SFR arrays (for convenience and consistency in SWOPS)

      ! TODO --
      ! Update input format and AR routine to read UNIT_OPTION ...
      ! Hardwired for now ...
      UNIT_OPTION = .FALSE.

      ! Map immediate upstream and downstream segments ...
      CALL MAP_UPSEGS(SWO%UPSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_DNSEGS(SWO%DNSEG,SWO%NSEG,IDIVAR,IOTSG)

      ! IMF DEBUG --
      ! Check for segments with more than two downstream segs (e.g., two divsegs + outflow)
      DO ISEG = 1,SWO%NSEG
        IF (SWO%DNSEG(ISEG)%NLIST.GT.2) THEN
          WRITE(*,*) "WARNING: DNSEG%NSEG > 2"
          WRITE(*,*) "--> SEG =", ISEG
        END IF !(TMP.GT.2)
      END DO !(ISEG)
      ! END IMF DEBUG

      ! IMF DEBUG --
      ! Write downstream segments for ALL segs to test mapping routines...
!!!      ASSOCIATE(UPSEG=>SWO%UPSEG, DNSEG=>SWO%DNSEG)
!!!      OPEN(NEWUNIT=TMPUNIT,FILE="SWOPS.DownSegs.txt",STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')
!!!!!!      WRITE(TMPUNIT,'(A)') '  SEGMENT  |  DownSeg'
!!!!!!      DO ISEG = 1,SWO%NSEG
!!!!!!        I   = DNSEG(ISEG)%NLIST
!!!!!!        WRITE(TMPUNIT,*) " "
!!!!!!        WRITE(TMPUNIT,*) "--------------------"
!!!!!!        WRITE(TMPUNIT,'(I6,4x,A1,I4)') ISEG,"|",I
!!!!!!        DO JSEG = 1,I
!!!!!!          WRITE(TMPUNIT,'(11x,I8)') DNSEG(ISEG)%SEGLIST(JSEG)
!!!!!!        END DO !(JSEG)
!!!!!!      END DO !(ISEG)
!!!!!!      ! END IMF DEBUG
!!!
!!!
!!!      ! PRINT TEST -----------------------------
!!!          WRITE(TMPUNIT,*) " "
!!!          WRITE(TMPUNIT,*) "      TEST @ MAP_UPSEG:"
!!!          WRITE(TMPUNIT,*) " "
!!!          WRITE(TMPUNIT,*) "     SEGMENT |   NUMLIST | UPSEG LIST"
!!!          DO ISEG = 1,SWO%NSEG
!!!              WRITE(TMPUNIT,*) UPSEG(ISEG)%NSEG, UPSEG(ISEG)%NLIST,  (UPSEG(ISEG)%SEGLIST(J),J=1,UPSEG(ISEG)%NLIST)
!!!          END DO !(ISEG = 1,NSS)
!!!
!!!
!!!          WRITE(TMPUNIT,*) " "
!!!          WRITE(TMPUNIT,*) "      TEST @ MAP_DNSEG:"
!!!          WRITE(TMPUNIT,*) " "
!!!          WRITE(TMPUNIT,*) "     SEGMENT |   NUMLIST | DNSEG LIST"
!!!          DO ISEG = 1,SWO%NSEG
!!!              WRITE(TMPUNIT,*) DNSEG(ISEG)%NSEG, DNSEG(ISEG)%NLIST,(DNSEG(ISEG)%SEGLIST(J),J=1,DNSEG(ISEG)%NLIST)
!!!          END DO !(ISEG = 1,NSS)
!!!         CLOSE(TMPUNIT)
!!!      END ASSOCIATE

      ! Map complete upstream/downstream tree @ each segment type...
      ! NOTE -- in contrast to MAP_UPSEGS/MAP_DNSEGS, UPTREE type vars
      !         must be allocated *PRIOR* to calling MAP_UPTREE/MAP_DNTREE
      CALL MAP_UPTREE(SWO%UPTREE_NAT, 1,UNIT_OPTION,SWO%SEGINFO,SWO%UPSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_UPTREE(SWO%UPTREE_CON1,2,UNIT_OPTION,SWO%SEGINFO,SWO%UPSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_UPTREE(SWO%UPTREE_CON2,5,UNIT_OPTION,SWO%SEGINFO,SWO%UPSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_UPTREE(SWO%UPTREE_RET, 4,UNIT_OPTION,SWO%SEGINFO,SWO%UPSEG,SWO%NSEG,IDIVAR,IOTSG)
      !
      CALL MAP_DNTREE(SWO%DNTREE_NAT, 1,UNIT_OPTION,SWO%SEGINFO,SWO%DNSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_DNTREE(SWO%DNTREE_CON1,2,UNIT_OPTION,SWO%SEGINFO,SWO%DNSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_DNTREE(SWO%DNTREE_CON2,5,UNIT_OPTION,SWO%SEGINFO,SWO%DNSEG,SWO%NSEG,IDIVAR,IOTSG)
      CALL MAP_DNTREE(SWO%DNTREE_RET, 4,UNIT_OPTION,SWO%SEGINFO,SWO%DNSEG,SWO%NSEG,IDIVAR,IOTSG)

      ! IMF DEBUG --
      ! Write release segments, downstream trees to test mapping routines...
      ASSOCIATE(DNTREE_CON2=>SWO%DNTREE_CON2, UPTREE_CON2=>SWO%UPTREE_CON2, DNTREE_NAT=>SWO%DNTREE_NAT, RESDAT=>SWO%RESDAT )
      OPEN(NEWUNIT=TMPUNIT,FILE="SWOPS.RelSeg_DownTree.txt",STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')
      WRITE(TMPUNIT,'(A)') '  RELSEG  |  DownTree'
      DO IPROJ = 1,SWO%NPROJ
        DO IRES = 1,SWO%NRES_BAL(IPROJ)
          ISEG  = RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG
          IF( ISEG > Z) THEN
              NTREE = DNTREE_NAT(ISEG)%NTREE
          ELSE
              NTREE = Z
          END IF
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "--------------------"
          WRITE(TMPUNIT,'(I6,4x,A1,I4)') ISEG,"|",NTREE
          DO I = 1,NTREE
            WRITE(TMPUNIT,'(11x,I8)') DNTREE_NAT(ISEG)%SEGTREE(I)
          END DO !(ITREE)
        END DO !(IRES)
      END DO !(IPROJ)
      ! END IMF DEBUG

      ! PRINT TEST -----------------------------
      WRITE(TMPUNIT,*) " "
      WRITE(TMPUNIT,*) "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      WRITE(TMPUNIT,*) "DEBUG: DNTREE_CON2"
      WRITE(TMPUNIT,*) "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      DO ISEG = 1,SWO%NSEG
        NTREE = DNTREE_CON2(ISEG)%NTREE
        WRITE(TMPUNIT,*) " "
        WRITE(TMPUNIT,*) "SEGMENT:", ISEG
        WRITE(TMPUNIT,*) "NTREE:  ", NTREE
        DO I = 1,NTREE
          WRITE(TMPUNIT,*) "------->", DNTREE_CON2(ISEG)%SEGTREE(I)
        END DO !(ITREE)
      END DO !(ISEG)

          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ MAP_UPTREE:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | UPTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') UPTREE_CON2(ISEG)%NSEG,  &
                   UPTREE_CON2(ISEG)%NTREE,          &
                  (UPTREE_CON2(ISEG)%SEGTREE(J),J=1,UPTREE_CON2(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)

          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ MAP_DNTREE:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') DNTREE_CON2(ISEG)%NSEG, &
                   DNTREE_CON2(ISEG)%NTREE,         &
                  (DNTREE_CON2(ISEG)%SEGTREE(J), J=1,DNTREE_CON2(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
      CLOSE(TMPUNIT)
      END ASSOCIATE
      !
      !seb debug
      OPEN(NEWUNIT=TMPUNIT,FILE="SWOPS.TreeTypes.txt",STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ UPTREE_NAT:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%UPTREE_NAT(ISEG)%NSEG, SWO%UPTREE_NAT(ISEG)%NTREE, (SWO%UPTREE_NAT(ISEG)%SEGTREE(J), J=1,SWO%UPTREE_NAT(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ UPTREE_CON1:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%UPTREE_CON1(ISEG)%NSEG, SWO%UPTREE_CON1(ISEG)%NTREE, (SWO%UPTREE_CON1(ISEG)%SEGTREE(J), J=1,SWO%UPTREE_CON1(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ UPTREE_CON2:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%UPTREE_CON2(ISEG)%NSEG, SWO%UPTREE_CON2(ISEG)%NTREE, (SWO%UPTREE_CON2(ISEG)%SEGTREE(J), J=1,SWO%UPTREE_CON2(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ UPTREE_RET:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%UPTREE_RET(ISEG)%NSEG, SWO%UPTREE_RET(ISEG)%NTREE, (SWO%UPTREE_RET(ISEG)%SEGTREE(J), J=1,SWO%UPTREE_RET(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ DNTREE_NAT:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%DNTREE_NAT(ISEG)%NSEG, SWO%DNTREE_NAT(ISEG)%NTREE, (SWO%DNTREE_NAT(ISEG)%SEGTREE(J), J=1,SWO%DNTREE_NAT(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ DNTREE_CON1:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%DNTREE_CON1(ISEG)%NSEG, SWO%DNTREE_CON1(ISEG)%NTREE, (SWO%DNTREE_CON1(ISEG)%SEGTREE(J), J=1,SWO%DNTREE_CON1(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ DNTREE_CON2:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%DNTREE_CON2(ISEG)%NSEG, SWO%DNTREE_CON2(ISEG)%NTREE, (SWO%DNTREE_CON2(ISEG)%SEGTREE(J), J=1,SWO%DNTREE_CON2(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
          !
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "      TEST @ DNTREE_RET:"
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) " "
          WRITE(TMPUNIT,*) "     SEGMENT |   NUMTREE | DNTREE"
          DO ISEG = 1,SWO%NSEG
              WRITE(TMPUNIT,'(*(I6))') SWO%DNTREE_RET(ISEG)%NSEG, SWO%DNTREE_RET(ISEG)%NTREE, (SWO%DNTREE_RET(ISEG)%SEGTREE(J), J=1,SWO%DNTREE_RET(ISEG)%NTREE)
          END DO !(ISEG = 1,NSS)
      CLOSE(TMPUNIT)
    !
    !----------------------------------------------------------------------------------------------
    !
      ! ****************************************************************
      ! IDENTIFY NETWORK JUNCTIONS
      ! ****************************************************************

      ! Identify + allocate sub-units based on current network ...

      ! NOTE --
      ! Network junctions (splits) and corresponding sub-units are mapped
      ! by Diversion Segments, irrespective of district/unit, rather than
      ! based on individual units ...

      ! Compile unique diversion segments for each project
      ! NOTE: Each unit has *ONE* diversion segment (required)
      !       Code below compiles list of unique diversion segments
      !       (i.e., removes doubles where multiple units share point of diversion)

      ! (1) copy all diversion segments into a temporary array
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      CALL LIST%INIT()
      DO I = ONE, SWO%NUNIT
           IF(SWO%UNIT(I)%DivSeg.NE.Z) CALL LIST%ADD_UNIQUE(SWO%UNIT(I)%DivSeg)  !NOTE OLD ALGORITHM ALLOWED FOR ISEG=0
      END DO
      !
      !ALLOCATE( TMP_ARRAY1(NUNIT) ); TMP_ARRAY1 = -99
      !DO CONCURRENT(I=ONE:NUNIT); TMP_ARRAY1(I) = UNIT(I)%DivSeg
      !END DO
      !ITMP     = 0
      !DO IUNIT = 1,NUNIT
      !  ITMP   = ITMP + 1
      !  TMP_ARRAY1(ITMP) = UNIT(IUNIT)%DivSeg
      !END DO !(IUNIT)
      !
      !! (2) count number of unique diversion segments
      !ALLOCATE( TMP_ARRAY2(NUNIT) ); TMP_ARRAY2 = -99
      !TMP_ARRAY2(1) = TMP_ARRAY1(1)
      !DO IUNIT = 2,NUNIT
      !  TMP1   = TMP_ARRAY1(IUNIT)
      !  TEST   = 0
      !  DO JTMP = 1,IUNIT-1
      !    TMP2 = TMP_ARRAY1(JTMP)
      !    IF (TMP1.EQ.TMP2) TEST = TEST + 1
      !  END DO !(JTMP)
      !  IF (TEST.EQ.0) TMP_ARRAY2(IUNIT) = TMP1
      !END DO !(IUNIT)
      !COUNTER  = 0
      !DO IUNIT = 1,NUNIT
      !  IF (TMP_ARRAY2(IUNIT).NE.-99) COUNTER = COUNTER + 1
      !END DO !(IUNIT
      SWO%DIVCOUNT = LIST%LEN()                                        ! total number of UNIQUE diversion segments in model

      ! (3) compile unique diversion segments
      ! TODO --
      ! How to check allocation status of derived type (DIVSEG)
      ! and de/re-allocate if needed?
      !   COMPILE EROR --> IF (ALLOCATED(DIVSEG)) DEALLOCATE(DIVSEG)
      !
      IF(      ALLOCATED(SWO%DIVSEG) .AND. SIZE(SWO%DIVSEG).NE.SWO%DIVCOUNT) DEALLOCATE(SWO%DIVSEG)
      IF(.NOT. ALLOCATED(SWO%DIVSEG)) ALLOCATE(SWO%DIVSEG(SWO%DIVCOUNT))
      !
      DIVSEG_ALLOC = FALSE
      IF(      ALLOCATED(SWO%DIVSEG_PREV) .AND. SIZE(SWO%DIVSEG_PREV).NE.SWO%DIVCOUNT) DEALLOCATE(SWO%DIVSEG_PREV)
      IF(.NOT. ALLOCATED(SWO%DIVSEG_PREV))  THEN
          ALLOCATE(SWO%DIVSEG_PREV(SWO%DIVCOUNT))
          DIVSEG_ALLOC = TRUE
      END IF
      !
      !IF (KPER.EQ.1) ALLOCATE( SWO%DIVSEG_PREV(SWO%DIVCOUNT) )  !WHY ONLY SP 1?
      !
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      CALL LIST%START()
      DO I=ONE, SWO%DIVCOUNT
          SWO%DIVSEG(I)%NRES   = Z
          SWO%DIVSEG(I)%NUnit  = Z
          SWO%DIVSEG(I)%DivID  = I
          SWO%DIVSEG(I)%DivSeg = LIST%INT()  !RETURN CURRENT VALLUE IN LIST
          SWO%DIVSEG(I)%ProjID = SWO%SEGINFO(SWO%DIVSEG(I)%DivSeg)%ProjID
          CALL LIST%NEXT()
      END DO
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      !
      IF(DIVSEG_ALLOC) THEN
          DO CONCURRENT (I=ONE:SWO%DIVCOUNT)
              SWO%DIVSEG_PREV(I)%DivID  = SWO%DIVSEG(I)%DivID
              SWO%DIVSEG_PREV(I)%DivSeg = SWO%DIVSEG(I)%DivSeg
              SWO%DIVSEG_PREV(I)%ProjID = SWO%DIVSEG(I)%ProjID
          END DO
      END IF

      !
      !COUNTER  = 0
      !DO IUNIT = 1,NUNIT
      !  IF (TMP_ARRAY2(IUNIT).NE.-99) THEN
      !    COUNTER = COUNTER + 1
      !    SWO%DIVSEG(COUNTER)%DivID  = COUNTER
      !    SWO%DIVSEG(COUNTER)%DivSeg = TMP_ARRAY2(IUNIT)
      !    SWO%DIVSEG(COUNTER)%ProjID = SWO%SEGINFO(TMP_ARRAY2(IUNIT))%ProjID
      !    IF (KPER.EQ.1) THEN
      !      SWO%DIVSEG_PREV(COUNTER)%DivID  = COUNTER
      !      SWO%DIVSEG_PREV(COUNTER)%DivSeg = TMP_ARRAY2(IUNIT)
      !      SWO%DIVSEG_PREV(COUNTER)%ProjID = SWO%SEGINFO(TMP_ARRAY2(IUNIT))%ProjID
      !    END IF !(KPER.EQ.1)
      !  END IF !(TMP_ARRAY2.NE.-99)
      !END DO !(IUNIT)

      ! (4) count units served by each diversion segment
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      DO IDVS =ONE, SWO%DIVCOUNT
      DO IUNIT=ONE, SWO%NUNIT
      IF(SWO%DIVSEG(IDVS)%DivSeg == SWO%UNIT(IUNIT)%DivSeg) SWO%DIVSEG(IDVS)%NUnit = SWO%DIVSEG(IDVS)%NUnit + 1
      END DO
      END DO
      !
      DO CONCURRENT (I=ONE:SWO%DIVCOUNT); SWO%DIVSEG_PREV(I)%NUnit  = SWO%DIVSEG(I)%NUnit
      END DO
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      !
      DO IDVS=ONE, SWO%DIVCOUNT
          !
          IF(SWO%DIVSEG(IDVS)%NUnit > Z) THEN
              CALL ALLOC( SWO%DIVSEG(IDVS)%UnitID,      SWO%DIVSEG(IDVS)%NUnit )
              CALL ALLOC( SWO%DIVSEG_PREV(IDVS)%UnitID, SWO%DIVSEG(IDVS)%NUnit, NEW_ALLOC=DIVSEG_UnitID_ALLOC )
              !
              I=Z
              DO IUNIT=ONE, SWO%NUNIT
              IF ( SWO%DIVSEG(IDVS)%DivSeg == SWO%UNIT(IUNIT)%DivSeg ) THEN
                       I=I+ONE
                       SWO%DIVSEG(IDVS)%UnitID(I) = IUNIT
                       !
                       IF(DIVSEG_UnitID_ALLOC) SWO%DIVSEG_PREV(IDVS)%UnitID(I) = IUNIT
              END IF
              END DO
          !ELSE
          !    CALL ALLOC( SWO%DIVSEG(IDVS)%UnitID,      1, SRC=Z)
          !    CALL ALLOC( SWO%DIVSEG_PREV(IDVS)%UnitID, 1, SRC=Z)
          END IF
      END DO
      !
      !!!!DO CONCURRENT (IDVS=ONE:SWO%DIVCOUNT)
      !!!!    xxx
      !!!!END DO
      !DO IDVS = 1,SWO%DIVCOUNT
      !  ISEG = SWO%DIVSEG(IDVS)%DivSeg
      !  DO CONCURRENT( IUNIT = 1:NUNIT, SWO%UNIT(IUNIT)%DivSeg.EQ.ISEG);   SWO%DIVSEG(IDVS)%NUnit = SWO%DIVSEG(IDVS)%NUnit + 1
      !END DO !(IUNIT)
      !  !ALLOCATE( SWO%DIVSEG(IDVS)%UnitID(SWO%DIVSEG(IDVS)%NUnit) )
      !  !IF (KPER.EQ.1) THEN
      !  !  ALLOCATE( SWO%DIVSEG_PREV(IDVS)%UnitID(SWO%DIVSEG(IDVS)%NUnit) )
      !  !END IF !(KPER.EQ.1)
      !  TMP = 0
      !  DO IUNIT = 1,NUNIT
      !    IF (UNIT(IUNIT)%DivSeg.EQ.ISEG) THEN
      !      TMP = TMP + 1
      !      SWO%DIVSEG(IDVS)%UnitID(TMP) = IUNIT
      !      IF (KPER.EQ.1) THEN
      !        SWO%DIVSEG_PREV(IDVS)%UnitID(TMP) = IUNIT
      !      END IF !(KPER.EQ.1)
      !    END IF !(DivSeg.EQ.ISEG)
      !  END DO !(IUNIT)
      !END DO !(IDVS)

      ! (5) initialize remaining components of DIVSEG
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      DO IDVS = 1,SWO%DIVCOUNT
        SWO%DIVSEG(IDVS)%TFDR      = DZ
        SWO%DIVSEG(IDVS)%DELORDER  = DZ
        SWO%DIVSEG(IDVS)%DIVORDER  = DZ
        SWO%DIVSEG(IDVS)%DIVERSION = DZ
        SWO%DIVSEG(IDVS)%DELIVERY  = DZ
        SWO%DIVSEG(IDVS)%DELIVEFF  = UNO
        SWO%DIVSEG(IDVS)%DSEEP     = DZ
        SWO%DIVSEG(IDVS)%DFLOW_IN  = DZ
        SWO%DIVSEG(IDVS)%DFLOW_RT  = DZ
        SWO%DIVSEG(IDVS)%UP_DIVORDER = DZ
        SWO%DIVSEG(IDVS)%UP_DSEEP    = DZ
        SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DZ
        SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DZ
        SWO%DIVSEG(IDVS)%UP_DFLOW_RN = DZ
        SWO%DIVSEG(IDVS)%UP_DFLOW_ET = DZ
        IF (DIVSEG_ALLOC) THEN
          SWO%DIVSEG_PREV(IDVS)%TFDR      = DZ
          SWO%DIVSEG_PREV(IDVS)%DELORDER  = DZ
          SWO%DIVSEG_PREV(IDVS)%DIVORDER  = DZ
          SWO%DIVSEG_PREV(IDVS)%DIVERSION = DZ
          SWO%DIVSEG_PREV(IDVS)%DELIVERY  = DZ
          SWO%DIVSEG_PREV(IDVS)%DELIVEFF  = UNO
          SWO%DIVSEG_PREV(IDVS)%DSEEP     = DZ
          SWO%DIVSEG_PREV(IDVS)%DFLOW_IN  = DZ
          SWO%DIVSEG_PREV(IDVS)%DFLOW_RT  = DZ
          SWO%DIVSEG_PREV(IDVS)%UP_DIVORDER = DZ
          SWO%DIVSEG_PREV(IDVS)%UP_DSEEP    = DZ
          SWO%DIVSEG_PREV(IDVS)%UP_DFLOW_IN = DZ
          SWO%DIVSEG_PREV(IDVS)%UP_DFLOW_RT = DZ
          SWO%DIVSEG_PREV(IDVS)%UP_DFLOW_RN = DZ
          SWO%DIVSEG_PREV(IDVS)%UP_DFLOW_ET = DZ
        END IF !(KPER)
      END DO !(IDVS)

!      ! IMF DEBUG --
!      ! Write diversion segments, downstream trees to test mapping routines...
!      OPEN(NEWUNIT=TMPUNIT,FILE="SWOPS.DivSeg_DownTree.txt",STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')
!      WRITE(TMPUNIT,'(A)') '  DIVSEG  |  DownTree'
!      DO IDVS = 1,SWO%DIVCOUNT
!        ISEG  = SWO%DIVSEG(IDVS)%DivSeg
!        NTREE = DNTREE_CON2(ISEG)%NTREE
!        WRITE(TMPUNIT,*) " "
!        WRITE(TMPUNIT,*) "--------------------"
!        WRITE(TMPUNIT,'(I6,4x,A1,I8)') ISEG,"|",NTREE
!        DO ITREE = 1,NTREE
!          WRITE(TMPUNIT,'(11x,I8)') DNTREE_CON2(ISEG)%SEGTREE(ITREE)
!        END DO !(ITREE)
!      END DO !(IDVS)
!      CLOSE(TMPUNIT)
!      ! END IMF DEBUG

      ! Compile all splits within each DivSeg service area...
      ! NOTE: splits identified based on DNTREE_CON2 ...

      ! (1) count splits
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
      CALL ALLOC( SWO%NSPLIT, SWO%DIVCOUNT, SRC=Z )!; NSPLIT=0                            ! allocate NSPLIT @ (# Unique Diversion Segs)
      SWO%SPTCOUNT= Z
      DO IDVS = 1,SWO%DIVCOUNT
        JSEG  = SWO%DIVSEG(IDVS)%DivSeg
        ! Units w/o SW diversion ...
        IF (JSEG > Z) THEN
          ! Initialize counter
          COUNTER = 0
          ! Check if DivSeg is also a SptSeg
          IF (SWO%DNSEG(JSEG)%NLIST.GT.1) THEN
            COUNTER = COUNTER + 1
          END IF !(NLIST.GT.1)
          ! Loop over conveyance/delivery segments downstream of DivSeg
          NTREE   = SWO%DNTREE_CON2(JSEG)%NTREE
          DO I = 1,NTREE                                                ! loop over segments in downstream tree
            ISEG = SWO%DNTREE_CON2(JSEG)%SEGTREE(I)                         ! current segment in tree
            IF (SWO%DNSEG(ISEG)%NLIST.GT.1) THEN                            ! if current segment has more than one downstream neighbor,
              COUNTER = COUNTER + 1                                     ! then increment counter
            END IF !(DNSEG%NLIST>1)
          END DO !(I)
          SWO%NSPLIT(IDVS) = COUNTER
          SWO%SPTCOUNT     = SWO%SPTCOUNT + COUNTER
        END IF !(JSEG.EQ.0)
      END DO !(IDVS)

      ! (2) compile splits
      IF(      ALLOCATED(SWO%SPTSEG) .AND. SIZE(SWO%SPTSEG).NE.SWO%SPTCOUNT) DEALLOCATE(SWO%SPTSEG)
      IF(.NOT. ALLOCATED(SWO%SPTSEG))  ALLOCATE(SWO%SPTSEG    (SWO%SPTCOUNT))
      !
      SPTSEG_ALLOC = FALSE
      IF(      ALLOCATED(SWO%SPTSEG_PREV) .AND. SIZE(SWO%SPTSEG_PREV).NE.SWO%SPTCOUNT) DEALLOCATE(SWO%SPTSEG_PREV)
      IF(.NOT. ALLOCATED(SWO%SPTSEG_PREV))  THEN
          ALLOCATE(SWO%SPTSEG_PREV(SWO%SPTCOUNT))
          SPTSEG_ALLOC = TRUE
      END IF
      !
      !ALLOCATE( SPTSEG(SWO%SPTCOUNT) )
      !IF (KPER.EQ.1) ALLOCATE( SPTSEG_PREV(SWO%SPTCOUNT) )
    I     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1

      COUNTER = 0
      DO IDVS = 1,SWO%DIVCOUNT
        JSEG  = SWO%DIVSEG(IDVS)%DivSeg
        IPROJ = SWO%DIVSEG(IDVS)%ProjID
        ! Units w/ SW diversion ...
        IF (JSEG > Z) THEN
          ! Check if DivSeg is also a SptSeg
          IF (SWO%DNSEG(JSEG)%NLIST.GT.1) THEN
            COUNTER = COUNTER + 1                                     ! then increment counter
            NLIST   = SWO%DNSEG(JSEG)%NLIST
            SWO%SPTSEG(COUNTER)%DivID     =SWO%DIVSEG(IDVS)%DivID
            SWO%SPTSEG(COUNTER)%DivSeg    =SWO%DIVSEG(IDVS)%DivSeg
            SWO%SPTSEG(COUNTER)%ProjID    =SWO%DIVSEG(IDVS)%ProjID
            SWO%SPTSEG(COUNTER)%SplitID   = COUNTER
            SWO%SPTSEG(COUNTER)%SplitSeg  = JSEG
            SWO%SPTSEG(COUNTER)%NBRANCH   = NLIST
            SWO%SPTSEG(COUNTER)%TFDR      = DZ
            SWO%SPTSEG(COUNTER)%DELORDER  = DZ
            SWO%SPTSEG(COUNTER)%DIVORDER  = DZ
            SWO%SPTSEG(COUNTER)%DIVERSION = DZ
            SWO%SPTSEG(COUNTER)%DELIVERY  = DZ
            SWO%SPTSEG(COUNTER)%DELIVEFF  = UNO
            SWO%SPTSEG(COUNTER)%DSEEP     = DZ
            SWO%SPTSEG(COUNTER)%DFLOW_IN  = DZ
            SWO%SPTSEG(COUNTER)%DFLOW_RT  = DZ
            !
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcSeg,  NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcType, NLIST )
            !
            DO IBRC = ONE, NLIST ! = SWO%SPTSEG(COUNTER)%NBRANCH
              TMP1  = SWO%DNSEG(JSEG)%SEGLIST(IBRC)
              TMP2  = SWO%SEGINFO(TMP1)%SegType
              SWO%SPTSEG(COUNTER)%BrcSeg(IBRC)  = TMP1
              SWO%SPTSEG(COUNTER)%BrcType(IBRC) = TMP2
            END DO !(IBRC)
            !
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcTFDR,      NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDELORDER,  NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDIVORDER,  NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDIVERSION, NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDELIVERY,  NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDELIVEFF,  NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDSEEP,     NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDFLOW_IN,  NLIST )
            CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDFLOW_RT,  NLIST )
            SWO%SPTSEG(COUNTER)%BrcTFDR       = DZ
            SWO%SPTSEG(COUNTER)%BrcDELORDER   = DZ
            SWO%SPTSEG(COUNTER)%BrcDIVORDER   = DZ
            SWO%SPTSEG(COUNTER)%BrcDIVERSION  = DZ
            SWO%SPTSEG(COUNTER)%BrcDELIVERY   = DZ
            SWO%SPTSEG(COUNTER)%BrcDELIVEFF   = UNO
            SWO%SPTSEG(COUNTER)%BrcDSEEP      = DZ
            SWO%SPTSEG(COUNTER)%BrcDFLOW_IN   = DZ
            SWO%SPTSEG(COUNTER)%BrcDFLOW_RT   = DZ
            IF (SPTSEG_ALLOC) THEN
              SWO%SPTSEG_PREV(COUNTER)%DivID     = SWO%DIVSEG(IDVS)%DivID
              SWO%SPTSEG_PREV(COUNTER)%DivSeg    = SWO%DIVSEG(IDVS)%DivSeg
              SWO%SPTSEG_PREV(COUNTER)%ProjID    = SWO%DIVSEG(IDVS)%ProjID
              SWO%SPTSEG_PREV(COUNTER)%SplitID   = COUNTER
              SWO%SPTSEG_PREV(COUNTER)%SplitSeg  = JSEG
              SWO%SPTSEG_PREV(COUNTER)%NBRANCH   = SWO%DNSEG(JSEG)%NLIST
              SWO%SPTSEG_PREV(COUNTER)%DELIVEFF  = UNO
              SWO%SPTSEG_PREV(COUNTER)%DSEEP     = DZ
              SWO%SPTSEG_PREV(COUNTER)%DFLOW_IN  = DZ
              SWO%SPTSEG_PREV(COUNTER)%DFLOW_RT  = DZ
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcSeg,  NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcType, NLIST )
              DO IBRC = 1,SWO%SPTSEG_PREV(COUNTER)%NBRANCH
                TMP1  = SWO%DNSEG(JSEG)%SEGLIST(IBRC)
                TMP2  = SWO%SEGINFO(TMP1)%SegType
                SWO%SPTSEG_PREV(COUNTER)%BrcSeg(IBRC)  = TMP1
                SWO%SPTSEG_PREV(COUNTER)%BrcType(IBRC) = TMP2
              END DO !(IBRC)
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcTFDR,      NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDELORDER,  NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDIVORDER,  NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDIVERSION, NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDELIVERY,  NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDELIVEFF,  NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDSEEP,     NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_IN,  NLIST )
              CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_RT,  NLIST )
              SWO%SPTSEG_PREV(COUNTER)%BrcTFDR            = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDELORDER        = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDIVORDER        = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDIVERSION       = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDELIVERY        = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDELIVEFF        = UNO
              SWO%SPTSEG_PREV(COUNTER)%BrcDSEEP           = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_IN        = DZ
              SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_RT        = DZ
            END IF !(KPER.EQ.1)
          END IF !(DNSEG(JSEG)%NLIST.GT.1)
          ! Loop over conveyance/delivery segments downstream of DivSeg
          NTREE   = SWO%DNTREE_CON2(JSEG)%NTREE
          DO I = 1,NTREE                                                ! loop over segments in downstream tree
            ISEG = SWO%DNTREE_CON2(JSEG)%SEGTREE(I)                         ! current segment in tree
            IF (SWO%DNSEG(ISEG)%NLIST.GT.1) THEN                            ! if current segment has more than one downstream neighbor,
              COUNTER = COUNTER + 1                                     ! then increment counter
              NLIST   = SWO%DNSEG(ISEG)%NLIST
              SWO%SPTSEG(COUNTER)%DivID     = SWO%DIVSEG(IDVS)%DivID
              SWO%SPTSEG(COUNTER)%DivSeg    = SWO%DIVSEG(IDVS)%DivSeg
              SWO%SPTSEG(COUNTER)%ProjID    = SWO%DIVSEG(IDVS)%ProjID
              SWO%SPTSEG(COUNTER)%SplitID   = COUNTER
              SWO%SPTSEG(COUNTER)%SplitSeg  = ISEG
              SWO%SPTSEG(COUNTER)%NBRANCH   = NLIST
              SWO%SPTSEG(COUNTER)%TFDR      = DZ
              SWO%SPTSEG(COUNTER)%DELORDER  = DZ
              SWO%SPTSEG(COUNTER)%DIVORDER  = DZ
              SWO%SPTSEG(COUNTER)%DIVERSION = DZ
              SWO%SPTSEG(COUNTER)%DELIVERY  = DZ
              SWO%SPTSEG(COUNTER)%DELIVEFF  = UNO
              SWO%SPTSEG(COUNTER)%DSEEP     = DZ
              SWO%SPTSEG(COUNTER)%DFLOW_IN  = DZ
              SWO%SPTSEG(COUNTER)%DFLOW_RT  = DZ
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcSeg,  NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcType, NLIST )
              DO IBRC = 1,SWO%SPTSEG(COUNTER)%NBRANCH
                TMP1  = SWO%DNSEG(ISEG)%SEGLIST(IBRC)
                TMP2  = SWO%SEGINFO(TMP1)%SegType
                SWO%SPTSEG(COUNTER)%BrcSeg(IBRC)  = TMP1
                SWO%SPTSEG(COUNTER)%BrcType(IBRC) = TMP2
              END DO !(IBRC)
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcTFDR,      NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDELORDER,  NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDIVORDER,  NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDIVERSION, NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDELIVERY,  NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDELIVEFF,  NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDSEEP,     NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDFLOW_IN,  NLIST )
              CALL ALLOC( SWO%SPTSEG(COUNTER)%BrcDFLOW_RT,  NLIST )
              SWO%SPTSEG(COUNTER)%BrcTFDR            = DZ
              SWO%SPTSEG(COUNTER)%BrcDELORDER        = DZ
              SWO%SPTSEG(COUNTER)%BrcDIVORDER        = DZ
              SWO%SPTSEG(COUNTER)%BrcDIVERSION       = DZ
              SWO%SPTSEG(COUNTER)%BrcDELIVERY        = DZ
              SWO%SPTSEG(COUNTER)%BrcDELIVEFF        = UNO
              SWO%SPTSEG(COUNTER)%BrcDSEEP           = DZ
              SWO%SPTSEG(COUNTER)%BrcDFLOW_IN        = DZ
              SWO%SPTSEG(COUNTER)%BrcDFLOW_RT        = DZ
              IF (SPTSEG_ALLOC) THEN
                SWO%SPTSEG_PREV(COUNTER)%DivID    = SWO%DIVSEG(IDVS)%DivID
                SWO%SPTSEG_PREV(COUNTER)%DivSeg   = SWO%DIVSEG(IDVS)%DivSeg
                SWO%SPTSEG_PREV(COUNTER)%ProjID   = SWO%DIVSEG(IDVS)%ProjID
                SWO%SPTSEG_PREV(COUNTER)%SplitID  = COUNTER
                SWO%SPTSEG_PREV(COUNTER)%SplitSeg = ISEG
                SWO%SPTSEG_PREV(COUNTER)%NBRANCH  = SWO%DNSEG(ISEG)%NLIST
                SWO%SPTSEG_PREV(COUNTER)%DELIVEFF = UNO
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcSeg,  NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcType, NLIST )
                DO IBRC = 1,SWO%SPTSEG_PREV(COUNTER)%NBRANCH
                  TMP1  = SWO%DNSEG(ISEG)%SEGLIST(IBRC)
                  TMP2  = SWO%SEGINFO(TMP1)%SegType
                  SWO%SPTSEG_PREV(COUNTER)%BrcSeg(IBRC)  = TMP1
                  SWO%SPTSEG_PREV(COUNTER)%BrcType(IBRC) = TMP2
                END DO !(IBRC)
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcTFDR,      NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDELORDER,  NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDIVORDER,  NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDIVERSION, NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDELIVERY,  NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDELIVEFF,  NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDSEEP,     NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_IN,  NLIST )
                CALL ALLOC( SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_RT,  NLIST )
                SWO%SPTSEG_PREV(COUNTER)%BrcTFDR            = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDELORDER        = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDIVORDER        = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDIVERSION       = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDELIVERY        = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDELIVEFF        = UNO
                SWO%SPTSEG_PREV(COUNTER)%BrcDSEEP           = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_IN        = DZ
                SWO%SPTSEG_PREV(COUNTER)%BrcDFLOW_RT        = DZ
              END IF !(KPER.EQ.1)
            END IF !(DNSEG%NLIST>1)
          END DO !(I)
        END IF !(JSEG.EQ.0)
      END DO !(IDVS)

!      ! IMF DEBUG --
!      ! Write split segments, branches, downstream trees to test mapping routines...
!      OPEN(NEWUNIT=TMPUNIT,FILE="SWOPS.SplitSeg_DownTree.txt",STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')
!      DO ISPT = 1,SWO%SPTCOUNT
!        ISEG    = SWO%SPTSEG(ISPT)%SplitSeg
!        JSEG    = SWO%SPTSEG(ISPT)%DivSeg
!        NBRANCH = SWO%SPTSEG(ISPT)%NBRANCH
!        WRITE(TMPUNIT,'(A)') "========================================="
!        WRITE(TMPUNIT,*)     "SPLIT SEG:", ISEG
!        WRITE(TMPUNIT,*)     "DIV SEG:  ", JSEG
!        WRITE(TMPUNIT,*)     "NBRANCH:  ", NBRANCH
!        DO IBRC = 1,NBRANCH
!          KSEG  = SWO%SPTSEG(ISPT)%BrcSeg(IBRC)
!          NTREE = SWO%DNTREE_CON2(KSEG)%NTREE
!          WRITE(TMPUNIT,*)   " "
!          WRITE(TMPUNIT,*)   "   BrcSeg   |  NTree "
!          WRITE(TMPUNIT,'(I6,11x,I4)') KSEG,NTREE
!          DO ITREE = 1,NTREE
!            JSEG   = SWO%DNTREE_CON2(KSEG)%SEGTREE(ITREE)
!            WRITE(TMPUNIT,'(17x,I6)') JSEG
!          END DO !(ITREE)
!        END DO !(IBRC)
!      END DO !(ISPT)
!      CLOSE(TMPUNIT)
      ! END IMF DEBUG
    !
    ! SEARCH FOR ALL RESERVOIRS THAT SERVE DIVSEG
!!!!    DO IDVS = 1,SWO%DIVCOUNT
!!!!        ISEG  = SWO%DIVSEG(IDVS)%DivSeg
!!!!        !
!!!!        IF (ISEG > Z) THEN
!!!!             IPROJ = SWO%DIVSEG(IDVS)%ProjID
!!!!             !
!!!!             I = NEG
!!!!             DO J=ONE, SIZE(SWO%RESBAL2PROJ)             !FIND FIRST RESERVOIR ID FOR PROJECT
!!!!                 IF(IPROJ == SWO%RESBAL2PROJ(ONE,J)) THEN
!!!!                     I = J - ONE
!!!!                     EXIT
!!!!                 END IF
!!!!             END DO
!!!!             !
!!!!             CALL LIST%INIT()
!!!!             DO IRES  = 1, SWO%NRES_BAL(IPROJ)
!!!!                   I = I + ONE
!!!!                   ASSOCIATE(RES_INUSE => SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE, RELSEG => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG, DNTREE_NAT => SWO%DNTREE_NAT)
!!!!                      IF(RES_INUSE) THEN
!!!!                         IF(RELSEG == ISEG) THEN
!!!!                             CALL LIST%ADD(I)
!!!!                         ELSE
!!!!                             NTREE = DNTREE_NAT(RELSEG)%NTREE
!!!!                             DO J = 1,NTREE
!!!!                               IF(DNTREE_NAT(RELSEG)%SEGTREE(J) == ISEG) THEN
!!!!                                   CALL LIST%ADD(I)
!!!!                                   EXIT
!!!!                               END IF
!!!!                             END DO
!!!!                         END IF
!!!!                      END IF
!!!!                   END ASSOCIATE
!!!!             END DO
!!!!             !
!!!!             SWO%DIVSEG(IDVS)%NRES = LIST%LEN()
!!!!             CALL LIST%TOARRAY(SWO%DIVSEG(IDVS)%RES, TRUE)
!!!!        END IF
!!!!    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    ! SET UP COMMONT RESERVOIR FLOW SEGMENTS
    SWO%RESLINK = Z
    I = Z
    DO IPROJ = 1, SWO%NPROJ
    DO IRES  = 1, SWO%NRES_BAL(IPROJ)
        I = I + ONE
        SWO%RESLINK(I,I) = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG
    END DO
    END DO
    !
    I = Z
    DO I = ONE, SWO%NRES_BALTOT
      IF(SWO%RESLINK(I,I) > Z) THEN
         DO J = I+ONE, SWO%NRES_BALTOT
            IF(SWO%RESLINK(J,J) > Z ) THEN
                !
                ISEG = SWO%RESLINK(I,I) ! RES I RELEASE SEGMENT
                JSEG = SWO%RESLINK(J,J) ! RES J RELEASE SEGMENT
                !
                IF(ISEG == JSEG) THEN
                    !
                    SWO%RESLINK(I,J) = ISEG  !They release to the same segment
                    SWO%RESLINK(J,I) = ISEG
                ELSE
                    DO IRCH=ONE, SWO%DNTREE_NAT(ISEG)%NTREE
                        !                   RES 1 DNTREE_NAT SEG             ALL DNTREE_NAT SEGs
                        IDVS = INT_POS( SWO%DNTREE_NAT(ISEG)%SEGTREE(IRCH),  SWO%DNTREE_NAT(JSEG)%SEGTREE)
                        !
                        IF(IDVS > Z) THEN
                            SWO%RESLINK(I,J) = SWO%DNTREE_NAT(JSEG)%SEGTREE(IDVS) !FOUND COMMON POINT BETWEEN RESERVOIRS
                            SWO%RESLINK(J,I) = SWO%DNTREE_NAT(JSEG)%SEGTREE(IDVS)
                            CYCLE  !FOUND COMMON SEGMENT
                        END IF
                    END DO
                    !
                END IF
            END IF
         END DO
      END IF
    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    !INCORPORATE PRECIPITATION AREA FRACTIONS (0-1)
    K = Z
    DO I = ONE, SWO%NPROJ
       DO J = ONE, SWO%NRES_BAL(I)
           !
           K = K + ONE
           SWO%RESDAT(I)%RESBAL(J)%PRECIP_AREA_FRAC = SWO%PRECIP_AREA_FRAC%LIST(K)
           !
           IF(SWO%RESDAT(I)%RESBAL(J)%PRECIP_AREA_FRAC > SUB_ONE_3 ) SWO%RESDAT(I)%RESBAL(J)%PRECIP_AREA_FRAC = UNO
           IF(SWO%RESDAT(I)%RESBAL(J)%PRECIP_AREA_FRAC < NEARZERO_5) SWO%RESDAT(I)%RESBAL(J)%PRECIP_AREA_FRAC = DZ
       END DO
    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    ! SET AND CORRECT ANY BAD RESERVOIR RELEASE FRACTIONS -- PORTION OF TOTAL DOWNTSTREAM DEMANDED RELEASE SPREAD ACROSS MULTIPLE RESERVOIRS
    !
    K = Z
    DO I = ONE, SWO%NPROJ
          IF(    SWO%NRES_BAL(I) == Z  ) THEN; CONTINUE
          ELSEIF(SWO%NRES_BAL(I) == ONE) THEN;
              K = K + ONE
              SWO%RESDAT(I)%RESBAL(ONE)%RELEASE_DMD_FRAC_INI = UNO
          ELSE
              IF(SWO%S_RELEASE_FRAC%HAS_S(I)) CYCLE !DEFINED VIA S
              NRES=Z
              DO J = ONE, SWO%NRES_BAL(I)
                  K = K + ONE
                  IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
                      NRES = NRES + ONE !DETERMINE NUMBER OF ACTIVE RESERVOIRS
                      SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI = SWO%RELEASE_DMD_FRAC%LIST(K)
                  ELSE
                      SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI = DZ
                  END IF
              END DO
              !
              DTMP = DZ
              DO J = ONE, SWO%NRES_BAL(I)
                ASSOCIATE(RELEASE_DMD_FRAC_INI => SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI, RESBAL => SWO%RESDAT(I)%RESBAL)
                  !
                  IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
                     IF(RELEASE_DMD_FRAC_INI < -1.9D0) THEN
                         !
                         DO CONCURRENT (K = ONE:SWO%NRES_BAL(I), K.NE.J); RESBAL(K)%RELEASE_DMD_FRAC_INI = RELEASE_DMD_FRAC_INI
                         END DO
                         DTMP = inf
                         EXIT
                     ELSEIF(RELEASE_DMD_FRAC_INI < DZ) THEN
                         DTMP = DNEG
                         EXIT
                     ELSE
                         DTMP = DTMP + RELEASE_DMD_FRAC_INI
                     END IF
                  END IF
                END ASSOCIATE
              END DO
              !
              IF(    NRES == Z .OR. DTMP > NEAR_inf ) THEN  !PROCESS FRACTIOSN AT A LATER DATE
                  CONTINUE
              ELSEIF(DTMP < DZ) THEN
                  IF(    NRES == TWO  ) THEN; DTMP = HALF
                  ELSEIF(NRES == THREE) THEN; DTMP = THIRD
                  ELSEIF(NRES == FOUR ) THEN; DTMP = FOURTH
                  ELSEIF(NRES == FIVE ) THEN; DTMP = FIFTH
                  ELSE;                       DTMP = UNO / DBLE(NRES)
                  END IF
                  DO CONCURRENT (J = ONE:SWO%NRES_BAL(I));   SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI = DTMP
                  END DO
              ELSEIF(SNGL(DTMP).NE.1.0) THEN
                  ASSOCIATE(RESBAL => SWO%RESDAT(I)%RESBAL)
                     DO CONCURRENT (J = ONE:SWO%NRES_BAL(I)); RESBAL(J)%RELEASE_DMD_FRAC_INI = RESBAL(J)%RELEASE_DMD_FRAC_INI / DTMP
                     END DO
                  END ASSOCIATE
              END IF
          END IF
    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    ! SET UP REQUIRE FLOWS IF THEY ARE NECESSARY
    !
    IF(SWO%REQFLOW%HAS_REQ ) THEN
          IF( UPDATE .OR. SWO%SEG_TYP%TRANSIENT ) THEN
                ERROR = NL
                DO I=1, SWO%REQFLOW%N
                    ISEG = ISTRM(4,SWO%REQFLOW%ISTRM(I))
                    IF( SWO%SEGINFO(ISEG)%SegType.NE.ONE) ERROR = ERROR//SWO%REQFLOW%NAM(I)%STR//' REFERS TO SEGMENT '//NUM2STR(ISEG,SIX)//' WHICH HAS A SEGMENT TYPE OF '//NUM2STR(SWO%SEGINFO(ISEG)%SegType)//NL
                END DO
                IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='REQUIRED FLOW STREAM SEGMENT TYPE ERROR.'//NL//'REQUIRED FLOWS MAY ONLY BE ASSIGNED TO "NATURAL" SEGMENT TYPES, WHICH ARE DEFINED WITH A SEGTYPE = 1'//NL//'THE FOLLOWING ARE A LIST OF BAD REQUIRE FLOW VARIABLES:'//ERROR)
          END IF
          !
          IF ( SWO%REQFLOW%N == ONE) THEN
              IF(ALLOCATED(SWO%REQFLOW%ORDER)) THEN
                  IF(SIZE(SWO%REQFLOW%ORDER).NE.ONE) THEN
                      DEALLOCATE(SWO%REQFLOW%ORDER       )
                        ALLOCATE(SWO%REQFLOW%ORDER(ONE))
                      !
                      DEALLOCATE(SWO%REQFLOW%REL       )
                        ALLOCATE(SWO%REQFLOW%REL(ONE))
                      !
                      DEALLOCATE(SWO%REQFLOW%REL_OLD       )
                        ALLOCATE(SWO%REQFLOW%REL_OLD(ONE))
                  END IF
              ELSE
                  ALLOCATE(SWO%REQFLOW%ORDER  (ONE))
                  ALLOCATE(SWO%REQFLOW%REL    (ONE))
                  ALLOCATE(SWO%REQFLOW%REL_OLD(ONE))
              END IF
              !
              CALL SWO%REQFLOW%ORDER(ONE)%INIT([ONE])
          ELSE
              ! FIND MOST DOWNSTREAM REQUIRED FLOW SEGMENTS
              !
              CALL LIST%INIT()
              !
              DO IREQ=1,SWO%REQFLOW%N
                 !
                 ISEG = SWO%REQFLOW%SEG(IREQ)
                 NTREE = SWO%DNTREE_NAT(ISEG)%NTREE
                 !
                 IF(NTREE == Z) THEN
                     UPDATE = TRUE  !USED AS INDICATOR IF THERE ARE DOWN STREAM REQ SEGMENT
                 ELSE
                     UPDATE = TRUE  !USED AS INDICATOR IF THERE ARE DOWN STREAM REQ SEGMENT
                     !
                     DO I = ONE, SWO%REQFLOW%N
                         IF( I.NE.IREQ) THEN
                               IF( INT_POS(SWO%REQFLOW%SEG(I), SWO%DNTREE_NAT(ISEG)%SEGTREE) > Z ) THEN
                                  UPDATE = FALSE
                                  EXIT
                               END IF
                         END IF
                     END DO
                 END IF
                 !
                 IF(UPDATE) CALL LIST%ADD(IREQ)
                 !
              END DO
              !
              CALL LIST%DROP_DUPLICATES()
              CALL LIST%SORT()
              NLIST = LIST%LEN()  !Number of unique downstream most required flows
              !
              IF(ALLOCATED(SWO%REQFLOW%ORDER)) THEN
                  IF(SIZE(SWO%REQFLOW%ORDER).NE.NLIST) THEN
                      DEALLOCATE(SWO%REQFLOW%ORDER       )
                        ALLOCATE(SWO%REQFLOW%ORDER(NLIST))
                      !
                      DEALLOCATE(SWO%REQFLOW%REL       )
                        ALLOCATE(SWO%REQFLOW%REL(NLIST))
                      !
                      DEALLOCATE(SWO%REQFLOW%REL_OLD       )
                        ALLOCATE(SWO%REQFLOW%REL_OLD(NLIST))
                  END IF
              ELSE
                  ALLOCATE(SWO%REQFLOW%ORDER  (NLIST))
                  ALLOCATE(SWO%REQFLOW%REL    (NLIST))
                  ALLOCATE(SWO%REQFLOW%REL_OLD(NLIST))
              END IF
              !
              CALL LIST%START()
              DO I=ONE, NLIST
                             SWO%REQFLOW%ORDER(I)%N = LIST%INT()  !USE AS A PLACE HOLDER OF EACH REQUIRED ID
                             CALL LIST%NEXT()
              END DO
              !
              DO I=ONE, NLIST
                             CALL LIST%INIT()
                             IREQ  = SWO%REQFLOW%ORDER(I)%N
                             ISEG  = SWO%REQFLOW%SEG(IREQ)
                             NTREE = SWO%UPTREE_NAT(ISEG)%NTREE
                             !
                             CALL LIST%ADD(IREQ)
                             DO J=ONE, NTREE
                                 !
                                 JSEG = SWO%UPTREE_NAT(ISEG)%SEGTREE(J)  !NEXT UPSTREAM NATURAL SEGMENT
                                 IREQ = INT_POS( JSEG, SWO%REQFLOW%SEG)  !IS THE SEGMENT A REQUIRED FLOW SEGMENT
                                 !
                                 IF( IREQ > Z )  CALL LIST%ADD(IREQ)     !ADD UPSTREAM REQUIRED FLOW SEGMENT
                             END DO
                             !
                             SWO%REQFLOW%ORDER(I)%N = LIST%LEN()
                             !
                             CALL LIST%TOARRAY(SWO%REQFLOW%ORDER(I)%VEC)
              END DO
          END IF
          !
          DO IREQ=1,SWO%REQFLOW%N
             I = Z
             ISEG = SWO%REQFLOW%SEG(IREQ) !ISTRM(4,SWO%REQFLOW%ISTRM(IREQ))
             !
             CALL LIST%INIT()
             !
             DO IPROJ = 1, SWO%NPROJ
             DO IRES  = 1, SWO%NRES_BAL(IPROJ)
                   !
                   I = I + ONE
                   ASSOCIATE(RES_INUSE => SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE, RELSEG => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG, DNTREE_NAT => SWO%DNTREE_NAT)
                      IF(RES_INUSE .AND. RELSEG > Z) THEN
                         IF(RELSEG == ISEG) THEN
                             CALL LIST%ADD(I)
                         ELSE
                             NTREE = DNTREE_NAT(RELSEG)%NTREE
                             DO J = 1,NTREE
                               IF(DNTREE_NAT(RELSEG)%SEGTREE(J) == ISEG) THEN
                                   CALL LIST%ADD(I)
                                   EXIT
                               END IF
                             END DO
                         END IF
                      END IF
                   END ASSOCIATE
             END DO
             END DO
             !
             ASSOCIATE(REQRES => SWO%REQFLOW%RES(IREQ), HEDSEG => SWO%REQFLOW%HEDSEG(IREQ))
                REQRES%N = LIST%LEN()
                CALL LIST%TOARRAY(REQRES%VEC, TRUE)
                !
                IF(    REQRES%N == Z  ) THEN
                                            HEDSEG = Z
                ELSEIF(REQRES%N == ONE) THEN
                                            I = REQRES%VEC(ONE) !RESERVOR ID
                                            HEDSEG = SWO%RESLINK(I,I)
                ELSEIF(REQRES%N == TWO) THEN
                                            I = REQRES%VEC(ONE) !RESERVOR ID
                                            J = REQRES%VEC(TWO)
                                            !
                                            HEDSEG = SWO%RESLINK(I,J)
                ELSE
                    CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO CURRENTLY DOES NOT SUPPORT MORE THEN TWO BALANCE RESERVOIRS SERVING ONE REQURIED FLOW. PLEASE ADJUST NETWORK TO ONLY ALLOW FOR AT MOST TWO RESERVOIRS OR CONTACT DEVELOPERS ABOUT A POSSIBLE UPDATE TO THE BASE CODE')
                END IF
             END ASSOCIATE
             !
             CALL LIST%DESTROY()
          END DO
          !
          ! --------------------------------------------------------------------------------
          ! SET UP REQUIRED RELEASE FRACTIONS
          !
          IF(.NOT. SWO%S_REQ_RELEASE_FRAC) THEN  !INIT VALUES WHEN THERE IS MORE THEN ONE RES
                IF(SWO%REQ_FRAC_USES_DMD_FRAC) THEN
                    DO I=ONE, SWO%NPROJ
                    DO J=ONE, SWO%NRES_BAL(I)
                        SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI = SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI
                    END DO
                    END DO
                ELSE
                    K = Z
                    DO I = ONE, SWO%NPROJ
                    DO J = ONE, SWO%NRES_BAL(I)
                        K = K + ONE
                        !
                        !IF(SWO%S_REQ_RELEASE_FRAC%HAS_S(I)) CYCLE !DEFINED VIA S
                        !
                        IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
                            SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI = SWO%RELEASE_REQ_FRAC%LIST(K)
                        ELSE
                            SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI = DZ
                        END IF
                    END DO
                    END DO
                END IF
          END IF
          !
          REQF: DO IREQ = ONE, SWO%REQFLOW%N
             !
             ASSOCIATE(N => SWO%REQFLOW%RES(IREQ)%N, RES => SWO%REQFLOW%RES(IREQ)%VEC)
                IF(N < ONE) CYCLE REQF
                !
                IF(N == ONE) THEN
                    I = SWO%RESBAL2PROJ(ONE,RES(ONE))
                    J = SWO%RESBAL2PROJ(TWO,RES(ONE))
                    SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI = UNO
                ELSE
                    DO I = ONE, IREQ - ONE
                        IF (ALL(RES == SWO%REQFLOW%RES(I)%VEC)) CYCLE REQF !ALREADY DEFINED FRACTIONS FOR RES COMBINATION
                    END DO
                    !
                    NRES = Z
                    DTMP = DZ
                    DO K=ONE, N
                       I = SWO%RESBAL2PROJ(ONE,RES(K))
                       J = SWO%RESBAL2PROJ(TWO,RES(K))
                       !
                       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
                          !
                          NRES = NRES + ONE
                          !
                          ASSOCIATE(RELEASE_REQ_FRAC_INI => SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI)
                             !
                             IF(RELEASE_REQ_FRAC_INI < -1.9D0) THEN
                                 !
                                 DO F=ONE, N
                                 IF (F.NE.K) THEN
                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(F)), JJ => SWO%RESBAL2PROJ(TWO,RES(F)))
                                         !
                                         SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI = RELEASE_REQ_FRAC_INI
                                         !
                                       END ASSOCIATE
                                 END IF
                                 END DO
                                 DTMP = inf
                                 EXIT
                             ELSEIF(RELEASE_REQ_FRAC_INI < DZ) THEN
                                 DTMP = DNEG
                                 EXIT
                             ELSE
                                 DTMP = DTMP + RELEASE_REQ_FRAC_INI
                             END IF
                          END ASSOCIATE
                       END IF
                    END DO
                    !
                    IF(    NRES == Z .OR. DTMP > NEAR_inf ) THEN  !PROCESS FRACTIONS AT A LATER DATE
                        CONTINUE
                    ELSEIF(DTMP <= DZ) THEN
                        IF(    NRES == TWO  ) THEN; DTMP = HALF
                        ELSEIF(NRES == THREE) THEN; DTMP = THIRD
                        ELSEIF(NRES == FOUR ) THEN; DTMP = FOURTH
                        ELSEIF(NRES == FIVE ) THEN; DTMP = FIFTH
                        ELSE;                       DTMP = UNO / DBLE(NRES)
                        END IF
                        DO F=ONE, N
                              ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(F)), JJ => SWO%RESBAL2PROJ(TWO,RES(F)))
                                !
                                SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI = DTMP
                                !
                              END ASSOCIATE
                        END DO
                    ELSEIF(SNGL(DTMP).NE.1.0) THEN
                        DO F=ONE, N
                              ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(F)), JJ => SWO%RESBAL2PROJ(TWO,RES(F)))
                                !
                                SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI = SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI / DTMP
                                !
                              END ASSOCIATE
                        END DO
                    END IF
                END IF
             END ASSOCIATE
          END DO REQF
          !
    END IF ! (SWO%REQFLOW%HAS_REQ )
    !
  END SUBROUTINE !###################################################################################################
  !
  SUBROUTINE MAP_UPSEGS(UPSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_UPSEGS
!     ******************************************************************
!     OVERVIEW:
!     Return list of immediate upstream segments for each stream segment
!     (i.e., segments that contribute flow to a given segment either by
!            outflow or by diversion)
!
!     NOTES:
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: UPSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER ISEG,JSEG,COUNTER,NN,IDIV
!   ------------------------------------------------------------------
!
    ! Allocate SWOPS to dimension NSS (number of stream segments in model)
    !ALLOCATE()
!
     ! Loop over stream segments in model, count immediate upstream segments
     ! --> segments with OUTSEG = current segment (outflow to segment)
     ! --> IUPSEG of current segment              (diversion into seg)
    DO ISEG = ONE,NSS
!
        ! COUNT UPSTREAM SEGMENTS
        COUNTER = Z
        ! tick counter if segment has IUPSEG (i.e., segment receives diversion)
        IF (IDIVAR(1,ISEG).NE.Z) THEN
            IDIV = IDIVAR(1,ISEG)
            COUNTER = COUNTER + ONE
        ELSE
            IDIV = NINER
        END IF
        ! loop over all other segments, tick counter once for each segment
        ! with outseg equal to current segment
        DO JSEG = ONE,NSS
            IF (IOTSG(JSEG)==ISEG .AND. JSEG.NE.IDIV) THEN
                COUNTER = COUNTER + ONE
            END IF
        END DO !(JSEG = 1,NSS)
!
        ! ASSIGN SEGMENTS IN UPSEG
        UPSEG(ISEG)%NSEG  = ISEG
        UPSEG(ISEG)%NLIST = COUNTER
        !
        IF(COUNTER>Z) THEN
           CALL ALLOC( UPSEG(ISEG)%SEGLIST, COUNTER )
           ! add IUPSEG to list
           NN = ONE
           IF (IDIV > Z) THEN
               UPSEG(ISEG)%SEGLIST(NN) = IDIV
               NN = NN + ONE
           END IF
           ! loop again to fill UPSEG list
           IF(COUNTER > ONE .OR. NN==ONE) THEN
               DO JSEG = 1,NSS
                   IF (IOTSG(JSEG)==ISEG .AND. JSEG.NE.IDIV) THEN
                       UPSEG(ISEG)%SEGLIST(NN) = JSEG
                       NN = NN + 1
                   END IF
               END DO !(JSEG = 1,NSS)
           END IF
        END IF
        !
    END DO !(ISEG = 1,NSS)
    !
  END SUBROUTINE MAP_UPSEGS
  !
  SUBROUTINE MAP_DNSEGS(DNSEG,NSS,IDIVAR,IOTSG)
!-----VERSION X 2014.07.24 MAP_DNSEGS
!     ******************************************************************
!     OVERVIEW:
!     Return list of immediate upstream segments for each stream segment
!     (i.e., segments that contribute flow to a given segment either by
!            outflow or by diversion)
!
!     NOTES:
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    TYPE(SWOPS_SEGLIST), DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: DNSEG
    INTEGER,                          INTENT(IN):: NSS
    INTEGER,DIMENSION(:,:),CONTIGUOUS,INTENT(IN):: IDIVAR
    INTEGER,DIMENSION(:),  CONTIGUOUS,INTENT(IN):: IOTSG
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER ISEG,JSEG,COUNTER,NN,IDWN
!   ------------------------------------------------------------------
!
    ! Allocate SWOPS to dimension NSS (number of stream segments in model)
    !ALLOCATE( DNSEG(NSS) )
!    ! Loop over stream segments in model, count immediate downstream segments
    ! --> OUTSEG of current segment           (receives outflow from current seg)
    ! --> IUPSEG of segment = current segment (receives diversion from current seg)
    DO ISEG = ONE,NSS
!
        ! COUNT DOWNSTREAM SEGMENTS
        COUNTER = Z
        ! tick counter if segment has OUTSEG!=0 (i.e., segment receives diversion)
        IF (IOTSG(ISEG).NE.Z) THEN
            IDWN = IOTSG(ISEG)
            COUNTER = COUNTER + ONE
        ELSE
            IDWN = NINER
        END IF
        ! loop over all other segments, tick counter once for each segment
        ! with IUPSEG equal to current segment
        DO JSEG = ONE,NSS
            IF (IDIVAR(1,JSEG).EQ.ISEG .AND. JSEG.NE.IDWN) THEN
                COUNTER = COUNTER + ONE
            END IF
        END DO !(JSEG = 1,NSS)
!
        ! ASSIGN SEGMENTS IN DNSEG
        DNSEG(ISEG)%NSEG  = ISEG
        DNSEG(ISEG)%NLIST = COUNTER
        !
        IF(COUNTER > Z) THEN
           CALL ALLOC( DNSEG(ISEG)%SEGLIST, COUNTER )
           ! add OUTSEG to list
           NN = ONE
           IF (IDWN > Z) THEN
               DNSEG(ISEG)%SEGLIST(NN) = IDWN
               NN = NN + ONE
           END IF
           !
           IF(COUNTER > ONE .OR. NN==ONE) THEN
              ! loop again to fill DNSEG list
              DO JSEG = ONE,NSS
                  IF (IDIVAR(1,JSEG).EQ.ISEG .AND. JSEG.NE.IDWN) THEN
                      DNSEG(ISEG)%SEGLIST(NN) = JSEG
                      NN = NN + ONE
                  END IF
              END DO !(JSEG = 1,NSS)
           END IF
        END IF
!
    END DO !(ISEG = 1,NSS)
    !
  END SUBROUTINE MAP_DNSEGS
  !
  SUBROUTINE SETUP_RESERVOIR_VARIABLES(SWO)
    CLASS(SWO_DATA), INTENT(INOUT):: SWO
    INTEGER:: I, IAUX, IOUT, IPROJ, NRES, N
    DOUBLE PRECISION:: DTMP1, DTMP2
    CHARACTER(:), ALLOCATABLE:: ERROR
    !
    IOUT = SWO%IOUT
    ! (loop over aux demands)
    DO IAUX = 1,SWO%NAUXDEM
        !
        SWO%AUXDEM(IAUX)%DEMAND      = -99.D0
        SWO%AUXDEM_PREV(IAUX)%DEMAND = -99.D0
        SWO%AUXDEM(IAUX)%AREA      = -99.D0
        SWO%AUXDEM_PREV(IAUX)%AREA = -99.D0
    END DO !(IAUX = 1,NAUXDEM)
    !
    DO IPROJ = 1,SWO%NPROJ
        ! IRESFL=0
        ! --> No Reservoirs ...
        ! --> Allocate/initialize sub-types to -99
        IF (SWO%IRESFL(IPROJ).EQ.0) THEN

          ! RESBAL (Explicit Reservoir Mass Balance)
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_MAX           = -99
          SWO%RESDAT(IPROJ)%PROJ_STORAGE_TOT           = -99
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD           = -99
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD_PREV      = -99
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_ADD           = DZ
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_ADD_S         = DZ
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_FIN           = -99
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_PREV_FIN      = -99
          SWO%RESDAT(IPROJ)%RESBAL(1)%RESBAL_PROJID    = -99
          SWO%RESDAT(IPROJ)%RESBAL(1)%RESBAL_RESID     = -99
          SWO%RESDAT(IPROJ)%RESBAL(1)%RESBAL_POOLFLG   = -99
          SWO%RESDAT(IPROJ)%RESBAL(1)%STORAGE_PREV     = DZ
          SWO%RESDAT(IPROJ)%RESBAL(1)%AREA_PREV        = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%INFLOW           = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%PRCP             = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%EVAP             = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_MIN_INPUT= DZ
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_MIN      = DZ
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_SPEC     = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_ADDF      = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S = SWO%NaN
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S_VOL = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S_VOL = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_PROJ_ADD_INI = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_PROJ_ADD     = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_PROJ      = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_FLOD      = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%SPILL_WAY         = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%OVER_TOP          = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_REQF      = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_REQF_PREV = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%STORAGE          = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%AREA             = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%AREA_DPL         = -99.D0
          SWO%RESDAT(IPROJ)%RESBAL(1)%MAIN_SPLT        = 1

          ! RESSPLIT (Lumped/Pooled Reservoir Mass Balance)
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_PROJID= -99
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_RESID = -99
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%DOY            = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%STORAGE        = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%AREA           = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV           = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV_PREV      = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%MAIN_RES       = FALSE

          ! ACAP (Area-Capacity-Elevation Table)
          SWO%RESDAT(IPROJ)%ACAP(1)%RESNAME            = "N/A"
          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_PROJID        = -99
          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_RESID         = -99
          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_STORAGE       = -99.D0
          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_AREA          = -99.D0
          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_ELEV          = -99.D0

          ! FRAC (Reservoir Storage Fraction Table)
          SWO%RESDAT(IPROJ)%FRAC(1)%RESNAME            = "N/A"
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_PROJID        = -99
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_RESID         = -99
          !SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_LEAP          = -99.D0
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_NOLEAP        = -99.D0
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_FRAC          = -99.D0

          ! HEAD (Reservoir Head Cells)
          SWO%RESDAT(1)%HEAD(1)%HEAD_PROJID            = -99
          SWO%RESDAT(1)%HEAD(1)%HEAD_RESID             = -99
          SWO%RESDAT(1)%HEAD(1)%HEAD_NCELL             = -99
          SWO%RESDAT(1)%HEAD(1)%HEAD_CELLS             = -99

        ! IRESFL>0
        ! --> Individual reservoirs...
        ! --> Allocate/initialize RESBAL @ NRES_BAL(NP)
        ! --> Allocate/initialize RESSPLIT @ 0/-99
        ! --> Allocate/initialize ACAP @ NRES_BAL(NP)
        ! --> Allocate/initialize FRAC @ 0/-99
        ELSE IF (SWO%IRESFL(IPROJ).GT.0) THEN
          !
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD = DZ
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_ADD = DZ
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_ADD_S = DZ
          !
          ! Set pooled flag to zero (all reservoirs individual--not pooled)
          DO NRES=1,SWO%NRES_BAL(IPROJ)
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_POOLFLG = 0

            ! Compute initial area from initial storage
            DTMP1 = SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_PREV
            DTMP2 = DZ
            ERROR = NL
            CALL ACAP_STOR2AREA(SWO,IPROJ,NRES,DTMP1,DTMP2)
            IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_PREV = DTMP2
            ! Allocate/init remaining vars (computed vars -- not inputs)
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE         = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ_ADD_INI = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ_ADD     = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ    = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_FLOD    = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%SPILL_WAY       = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%OVER_TOP        = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_REQF    = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_REQF_PREV= DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA            = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_DPL        = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PMAX    = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_DMND    = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_ADDF     = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_MIN_INPUT= DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_MIN      = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S   = SWO%NaN
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S   = DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S_VOL= DZ
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S_VOL= inf
            SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAIN_SPLT        = 1
            !
          END DO
          !
          ! RESSPLIT (Reservoir Mass Balance)
          ! (No pooled reservoirs for IRESFL>0 --> dummy alloc/init)
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_PROJID= -99
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_RESID = -99
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%DOY            = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%STORAGE        = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%AREA           = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV           = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV_PREV      = -99.D0
          !SWO%RESDAT(IPROJ)%RESSPLIT(1)%MAX_ELEV       = -99.D0
          !SWO%RESDAT(IPROJ)%RESSPLIT(1)%MIN_ELEV       = -99.D0
          !SWO%RESDAT(IPROJ)%RESSPLIT(1)%STOR_SPEC_MIN_ELEV      = -99.D0
          SWO%RESDAT(IPROJ)%RESSPLIT(1)%MAIN_RES       = FALSE

          ! FRAC (Reservoir Storage Fraction Table)
          SWO%RESDAT(IPROJ)%FRAC(1)%RESNAME            = "N/A"
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_PROJID        = -99
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_RESID         = -99
          !SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_LEAP          = -99.D0
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_NOLEAP        = -99.D0
          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_FRAC          = -99.D0

        ! IRESFL<0
        ! --> One lumped/pooled storage, fractional split to individual reservoirs
        ! --> Allocate/initialize RESBAL @ 1 (only one explicit storage pool ... split into IRESFL pieces)
        ! --> Allocate/initialize RESSPLIT @ NRES_SPT(NP)
        ! --> Allocate/initialize ACAP @ NRES_SPT(NP)
        ! --> Allocate/initialize FRAC @ NRES_SPT(NP)
        ELSE
          ! --> Loop over projects to parse inputs, allocate/init input vars
          !
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD = DZ
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_ADD = DZ
          SWO%RESDAT(IPROJ)%PROJ_RELEASE_ADD_S = DZ
          ! RESBAL (Explicit Reservoir Mass Balance)
          ! NOTE -- NRES_BAL=1 if IRESFL<0 ...
          ! Set number of mass balance reservoirs to 1 (required for projects w/ split storage)
          ! Set pooled flag to zero (all reservoirs individual--not pooled)
          ! Set head flag to zero for mass balance reservoir (required for pojects w/ split storage)
          NRES = 1
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_POOLFLG = 1
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL       = 0

          ! Allocate/init remaining mass balance vars (computed vars -- not inputs)
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE         = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ_ADD_INI = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ_ADD     = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ    = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_FLOD    = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%SPILL_WAY       = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%OVER_TOP        = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_REQF    = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_REQF_PREV= DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA            = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_PREV       = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_DPL        = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PMAX    = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_DMND    = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_ADDF     = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_MIN_INPUT= DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_MIN      = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S   = SWO%NaN
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S   = DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S_VOL= DZ
          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S_VOL= inf
          !
          DO NRES = ONE, SWO%NRES_SPT(IPROJ)
             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%ELEV         = DZ
             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%ELEV_PREV    = DZ
             !SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%MAX_ELEV     = inf
             !SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%MIN_ELEV     = ninf
             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%MAIN_RES     = FALSE
          END DO
          !
          DTMP1 = ninf
          I = ONE
          DO NRES = 1, SWO%NRES_SPT(IPROJ)
                N  = SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_COUNT
                IF(DTMP1 < SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(N))THEN
                   DTMP1 = SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(N)
                   I = NRES
                END IF
          END DO
          SWO%RESDAT(IPROJ)%RESBAL(1)%MAIN_SPLT      = I
          SWO%RESDAT(IPROJ)%RESSPLIT(I)%MAIN_RES     = TRUE  !SET BIGGEST STORAGE TO TRUE
          !
        END IF !(IF (IRESFL.EQ.0)/ELSE IF(IRESFL.GT.0)/ELSE)
    END DO !(NP=1,SWO%NPROJ)
    !
  END SUBROUTINE
  !
!  SUBROUTINE LOAD_SWO_TABFILES(SWO, LLOC, LINE, IU, ERROR_IU)
!    CLASS(SWO_DATA), INTENT(INOUT):: SWO
!    INTEGER,         INTENT(INOUT):: LLOC
!    CHARACTER(*),    INTENT(INOUT):: LINE
!    INTEGER,         INTENT(IN   ):: IU, ERROR_IU
!    TYPE(GENERIC_INPUT_FILE):: FL
!    INTEGER:: I, ISTART, ISTOP, IAUX, IOUT, IPROJ, NRES, N, NTABVAR
!    INTEGER, DIMENSION(ONE):: TMPLST1D
!    REAL:: R
!    DOUBLE PRECISION:: DTMP1, DTMP2
!    CHARACTER(:), ALLOCATABLE:: ERROR
!    !
!    CALL FL%OPEN(LLOC, LINE, SWO%IOUT, ERROR_IU, REQKEY=TRUE)
!    IF(FL%IU==Z) FL%IU = IU  !FOUND INTERNAL KEYWORD
!    !
!    !LLOC=ONE
!    !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)      !PASS PROJECT NUMBER
!    !
!    TMPLST1D = ONE
!    IOUT = SWO%IOUT
!    ! (loop over aux demands)
!    DO IAUX = 1,SWO%NAUXDEM
!        !
!        CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!        ! (read TABFILE @ demand)
!        SWO%AUXDEM(IAUX)%DEMAND      = -99.D0
!        SWO%AUXDEM_PREV(IAUX)%DEMAND = -99.D0
!        !ALLOCATE( SWO%AUXDEM(IAUX)%TABIDX_DEMAND,SWO%AUXDEM_PREV(IAUX)%TABIDX_DEMAND )
!        CALL TABFILEPARSE( FL%IU,IOUT,LINE,SWO%AUXDEM(IAUX)%TABIDX_DEMAND, TRUE )
!        CALL TABFILELINKS( FL%IU,IOUT,LINE,SWO%AUXDEM(IAUX)%TABIDX_DEMAND )
!        IF (SWO%AUXDEM(IAUX)%TABIDX_DEMAND%NTAB.GT.0) THEN
!          CALL TABFILEPACKINDEX( SWO%AUXDEM(IAUX)%TABIDX_DEMAND, INDEXLST=TMPLST1D )
!        END IF !(NTAB.GT.0)
!        !
!        !CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!        ! (read TABFILE @ water righted area)
!        SWO%AUXDEM(IAUX)%AREA      = -99.D0
!        SWO%AUXDEM_PREV(IAUX)%AREA = -99.D0
!        !ALLOCATE( SWO%AUXDEM(IAUX)%TABIDX_AREA )
!        CALL TABFILEPARSE( FL%IU,IOUT,LINE, SWO%AUXDEM(IAUX)%TABIDX_AREA,TRUE )
!        CALL TABFILELINKS( FL%IU,IOUT,LINE, SWO%AUXDEM(IAUX)%TABIDX_AREA )
!        IF (SWO%AUXDEM(IAUX)%TABIDX_AREA%NTAB.GT.0) THEN
!          CALL TABFILEPACKINDEX( SWO%AUXDEM(IAUX)%TABIDX_AREA,INDEXLST=TMPLST1D )
!        END IF !(NTAB.GT.0)
!    END DO !(IAUX = 1,NAUXDEM)
!    !
!    DO IPROJ = 1,SWO%NPROJ
!        ! IRESFL=0
!        ! --> No Reservoirs ...
!        ! --> Allocate/initialize sub-types to -99
!        IF (SWO%IRESFL(IPROJ).EQ.0) THEN
!
!          ! RESBAL (Explicit Reservoir Mass Balance)
!          SWO%RESDAT(IPROJ)%PROJ_RELEASE_MAX           = -99
!          SWO%RESDAT(IPROJ)%PROJ_STORAGE_TOT           = -99
!          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD           = -99
!          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD_PREV      = -99
!          SWO%RESDAT(IPROJ)%RESBAL(1)%RESBAL_PROJID    = -99
!          SWO%RESDAT(IPROJ)%RESBAL(1)%RESBAL_RESID     = -99
!          SWO%RESDAT(IPROJ)%RESBAL(1)%RESBAL_POOLFLG   = -99
!          SWO%RESDAT(IPROJ)%RESBAL(1)%STORAGE_PREV     = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%AREA_PREV        = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%INFLOW           = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%PRCP             = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%EVAP             = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_SPEC     = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%ADD_RELEASE      = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_PROJ      = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%RELEASE_FLOD      = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%SPILL_WAY        = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%STORAGE          = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%AREA             = -99.D0
!          SWO%RESDAT(IPROJ)%RESBAL(1)%AREA_DPL         = -99.D0
!
!          ! RESSPLIT (Lumped/Pooled Reservoir Mass Balance)
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_PROJID= -99
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_RESID = -99
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%DOY            = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%STORAGE        = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%AREA           = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV           = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV_PREV      = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%MAIN_RES       = FALSE
!
!          ! ACAP (Area-Capacity-Elevation Table)
!          SWO%RESDAT(IPROJ)%ACAP(1)%RESNAME            = "N/A"
!          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_PROJID        = -99
!          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_RESID         = -99
!          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_STORAGE       = -99.D0
!          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_AREA          = -99.D0
!          SWO%RESDAT(IPROJ)%ACAP(1)%ACAP_ELEV          = -99.D0
!
!          ! FRAC (Reservoir Storage Fraction Table)
!          SWO%RESDAT(IPROJ)%FRAC(1)%RESNAME            = "N/A"
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_PROJID        = -99
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_RESID         = -99
!          !SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_LEAP          = -99.D0
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_NOLEAP        = -99.D0
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_FRAC          = -99.D0
!
!          ! HEAD (Reservoir Head Cells)
!          SWO%RESDAT(1)%HEAD(1)%HEAD_PROJID            = -99
!          SWO%RESDAT(1)%HEAD(1)%HEAD_RESID             = -99
!          SWO%RESDAT(1)%HEAD(1)%HEAD_NCELL             = -99
!          SWO%RESDAT(1)%HEAD(1)%HEAD_CELLS             = -99
!
!        ! IRESFL>0
!        ! --> Individual reservoirs...
!        ! --> Allocate/initialize RESBAL @ NRES_BAL(NP)
!        ! --> Allocate/initialize RESSPLIT @ 0/-99
!        ! --> Allocate/initialize ACAP @ NRES_BAL(NP)
!        ! --> Allocate/initialize FRAC @ 0/-99
!        ELSE IF (SWO%IRESFL(IPROJ).GT.0) THEN
!          !
!          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD = DZ
!          !
!          ! --> Loop over projects to parse inputs, allocate/init input vars
!          DO NRES=1,SWO%NRES_BAL(IPROJ)
!
!            ! Set pooled flag to zero (all reservoirs individual--not pooled)
!            SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_POOLFLG = 0
!
!            ! Read first line -- ProjID, ResID, STORAGE_PREV (initial storage), HEADCELL (keyword)
!            !IF (NRES.EQ.1) THEN
!            !  CONTINUE
!            !ELSE
!            !  CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!            !END IF !(NRES.EQ.1)
!            !LLOC=1
!            !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2, SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_PROJID,R,IOUT,FL%IU)
!            !LLOC=ISTOP+1
!            !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2, SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RESID,R,IOUT,FL%IU)
!            !LLOC=ISTOP+1
!            !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RELSEG,R,IOUT,FL%IU)
!            !LLOC=ISTOP+1
!            !CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_PREV,IOUT,FL%IU)
!            !LLOC=ISTOP+1
!            !CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,FL%IU)
!            !IF (LINE(ISTART:ISTOP).EQ."HEADCELL") THEN
!            !  SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL = 1
!            !ELSE
!            !  SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL = 0
!            !END IF !(HEADCELL)
!
!            ! Read name of reservoir
!            !!!CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!            !!!
!            !!!SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME =  trim(adjustl(LINE(1:65)))
!            !!!
!            !!!! Write reservoir names to list file
!            !!!  WRITE(IOUT,*) "   RESNAME  = ", trim(adjustl(SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME))
!            !!!  WRITE(IOUT,*) "   PROJID   = ",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_PROJID
!            !!!  WRITE(IOUT,*) "   RESID    = ",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RESID
!            !!!  WRITE(IOUT,*) "   RELSEG   = ",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RELSEG
!            !!!  WRITE(IOUT,*) "   STOR_PREV= ",SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_PREV
!            !!!  WRITE(IOUT,*) "   HEADCELL = ",SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL
!
!            ! Print warning re: tabfile links/indexing established only once at AR
!              WRITE(IOUT,*) " "
!              WRITE(IOUT,*) "   Setting up reservoir inputs as TABFILES"
!              WRITE(IOUT,*) "   ** Parsing  (TABFILEPARSE)"
!              WRITE(IOUT,*) "   ** Linking  (TABFILELINKS)"
!              WRITE(IOUT,*) "   ** Indexing (TABFILEPACKINDEX)"
!              WRITE(IOUT,*) "   NOTE: "
!              WRITE(IOUT,*) "   Indexing only done once in AR routine."
!              WRITE(IOUT,*) "   This assumes that each reservoir is tied to a set of"
!              WRITE(IOUT,*) "   inputs for the entire simulation period."
!
!            ! Assign tabfiles to mass balance terms
!
!            CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!
!            NTABVAR = 5                                                 ! inflow,prcp,evap,RELEASE_SPEC,storage_nprj
!            DO I = 1,NTABVAR
!
!              LLOC=1
!              CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,FL%IU)        ! read intput type
!
!              SELECT CASE( LINE(ISTART:ISTOP) )
!              CASE('RES_INFLOW')
!                CALL TABFILEPARSE(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW, .TRUE. )
!                CALL TABFILELINKS(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW )
!                IF (SWO%RESDAT(IPROJ)%RESBAL(NRES) %TABIDX_INFLOW%NTAB.GT.0) THEN
!                  CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW, INDEXLST=TMPLST1D )
!                ELSE
!                    WRITE(IOUT,*) " "
!                    WRITE(IOUT,*) "WHOA..."
!                    WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                    WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                    WRITE(IOUT,*) "Project:  ", IPROJ
!                    WRITE(IOUT,*) "Reservoir:", NRES
!                    WRITE(IOUT,*) "Res. Name:", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                    WRITE(IOUT,*) "TABFILE: RES_INFLOW"
!                    WRITE(IOUT,*) "STOPPING CODE..."
!                  CALL USTOP(' ')
!                END IF !(NTAB.GT.0)
!
!              CASE('RES_PRCP')
!                CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP, .TRUE. )
!                CALL TABFILELINKS(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP )
!                IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP%NTAB.GT.0) THEN
!                  CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP, INDEXLST=TMPLST1D )
!                ELSE
!                    WRITE(IOUT,*) " "
!                    WRITE(IOUT,*) "WHOA..."
!                    WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                    WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                    WRITE(IOUT,*) "Project:  ", IPROJ
!                    WRITE(IOUT,*) "Reservoir:", NRES
!                    WRITE(IOUT,*) "Res. Name:", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                    WRITE(IOUT,*) "TABFILE: RES_PRCP"
!                    WRITE(IOUT,*) "STOPPING CODE..."
!                  CALL USTOP(' ')
!                END IF !(NTAB.GT.0)
!
!              CASE('RES_EVAP')
!                CALL TABFILEPARSE(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP, .TRUE. )
!                CALL TABFILELINKS(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP )
!                IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP%NTAB.GT.0) THEN
!                  CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP, INDEXLST=TMPLST1D )
!                ELSE
!                    WRITE(IOUT,*) " "
!                    WRITE(IOUT,*) "WHOA..."
!                    WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                    WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                    WRITE(IOUT,*) "Project:  ", IPROJ
!                    WRITE(IOUT,*) "Reservoir:", NRES
!                    WRITE(IOUT,*) "Res. Name:",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                    WRITE(IOUT,*) "TABFILE: RES_EVAP"
!                    WRITE(IOUT,*) "STOPPING CODE..."
!                  CALL USTOP(' ')
!                END IF !(NTAB.GT.0)
!
!              CASE('RES_RELEASE_SPEC')
!                CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC, .TRUE. )
!                CALL TABFILELINKS(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC )
!                IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC%NTAB.GT.0) THEN
!                  CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC, INDEXLST=TMPLST1D )
!                ELSE
!                    WRITE(IOUT,*) " "
!                    WRITE(IOUT,*) "WHOA..."
!                    WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                    WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                    WRITE(IOUT,*) "Project:  ", IPROJ
!                    WRITE(IOUT,*) "Reservoir:", NRES
!                    WRITE(IOUT,*) "Res. Name:",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                    WRITE(IOUT,*) "TABFILE: RES_RELEASE_SPEC"
!                    WRITE(IOUT,*) "STOPPING CODE..."
!                  CALL USTOP(' ')
!                END IF !(NTAB.GT.0)
!
!              CASE('RES_STORAGE_NPRJ')
!                SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_NPRJ      = DZ      ! no conversion -- input as ft3, use as ft3
!                SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_NPRJ_PREV = DZ
!                CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ, .TRUE. )
!                CALL TABFILELINKS(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ )
!                IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ%NTAB.GT.0) THEN
!                  CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ, INDEXLST=TMPLST1D )
!                ELSE
!                    WRITE(IOUT,*) " "
!                    WRITE(IOUT,*) "WHOA..."
!                    WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                    WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                    WRITE(IOUT,*) "Project:  ", IPROJ
!                    WRITE(IOUT,*) "Reservoir:", NRES
!                    WRITE(IOUT,*) "Res. Name:",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                    WRITE(IOUT,*) "TABFILE: RES_STORAGE_NPRJ"
!                    WRITE(IOUT,*) "STOPPING CODE..."
!                  CALL USTOP(' ')
!                END IF !(NTAB.GT.0)
!
!              CASE DEFAULT
!                  WRITE(IOUT,*) "ERROR @ RESERVOIR TABFILE INPUTS"
!                  WRITE(IOUT,*) "KEYWORD NOT RECOGNIZED:", LINE(ISTART:ISTOP)
!                  WRITE(IOUT,*) "VALID KEYWORD OPTIONS ARE:"
!                  WRITE(IOUT,*) "** RES_INFLOW"
!                  WRITE(IOUT,*) "** RES_PRCP"
!                  WRITE(IOUT,*) "** RES_EVAP"
!                  WRITE(IOUT,*) "** RES_RELEASE_SPEC"
!                  WRITE(IOUT,*) "** RES_STORAGE_NPRJ"
!                CALL USTOP(' ')
!
!              END SELECT
!            END DO !(DO I = 1,NTABVAR)
!
!            !! ACAP (Area-Capacity-Elevation)
!            !SWO%RESDAT(IPROJ)%ACAP(NRES)%RESNAME = SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!            !SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_PROJID = SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_PROJID
!            !SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_RESID = SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RESID
!            !CALL SWOPS_READACAP( SWO,FL, LINE,IPROJ,NRES )
!            !!!
!            !!!! HEAD (Head Boundary Cells)
!            !!!IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL.EQ.1) THEN
!            !!!  SWO%RESDAT(IPROJ)%HEAD(NRES)%RESNAME = SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!            !!!  SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_PROJID = SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_PROJID
!            !!!  SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_RESID = SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RESID
!            !!!  CALL SWOPS_READHEAD( SWO,FL%IU,LINE, SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_NCELL,SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_CELLS)
!            !!!END IF !(HEADCELL.EQ.1)
!
!          END DO !(NRES=1,NRES_BAL(NP))
!
!          ! RESSPLIT (Reservoir Mass Balance)
!          ! (No pooled reservoirs for IRESFL>0 --> dummy alloc/init)
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_PROJID= -99
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%RESSPLIT_RESID = -99
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%DOY            = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%STORAGE        = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%AREA           = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV           = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%ELEV_PREV      = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%MAX_ELEV       = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%MIN_ELEV       = -99.D0
!          SWO%RESDAT(IPROJ)%RESSPLIT(1)%MAIN_RES       = FALSE
!
!          ! FRAC (Reservoir Storage Fraction Table)
!          SWO%RESDAT(IPROJ)%FRAC(1)%RESNAME            = "N/A"
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_PROJID        = -99
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_RESID         = -99
!          !SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_LEAP          = -99.D0
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_NOLEAP        = -99.D0
!          SWO%RESDAT(IPROJ)%FRAC(1)%FRAC_FRAC          = -99.D0
!
!          ! Compute initial area from initial storage
!          DTMP1 = SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_PREV
!          DTMP2 = DZ
!          ERROR = NL
!          CALL ACAP_STOR2AREA(SWO,IPROJ,NRES,DTMP1,DTMP2, ERROR)
!          IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_PREV = DTMP2
!
!          ! Allocate/init remaining vars (computed vars -- not inputs)
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE        = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ    = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_FLOD    = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%SPILL_WAY      = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA           = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_DPL       = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PMAX   = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%PROJ_RELEASE_DMD   = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE    = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S  = inf
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S  = DZ
!
!        ! IRESFL<0
!        ! --> One lumped/pooled storage, fractional split to individual reservoirs
!        ! --> Allocate/initialize RESBAL @ 1 (only one explicit storage pool ... split into IRESFL pieces)
!        ! --> Allocate/initialize RESSPLIT @ NRES_SPT(NP)
!        ! --> Allocate/initialize ACAP @ NRES_SPT(NP)
!        ! --> Allocate/initialize FRAC @ NRES_SPT(NP)
!        ELSE
!
!          ! --> Loop over projects to parse inputs, allocate/init input vars
!
!          ! RESBAL (Explicit Reservoir Mass Balance)
!          ! NOTE -- NRES_BAL=1 if IRESFL<0 ...
!          ! Set number of mass balance reservoirs to 1 (required for projects w/ split storage)
!          ! Set pooled flag to zero (all reservoirs individual--not pooled)
!          ! Set head flag to zero for mass balance reservoir (required for pojects w/ split storage)
!          NRES = 1
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_POOLFLG = 1
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL       = 0
!          SWO%RESDAT(IPROJ)%PROJ_RELEASE_DMD = DZ
!
!          !LLOC=1
!          !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_PROJID,R,IOUT,FL%IU)
!          !LLOC=ISTOP+1
!          !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RESID,R,IOUT,FL%IU)
!          !LLOC=ISTOP+1
!          !CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RELSEG,R,IOUT,FL%IU)
!          !LLOC=ISTOP+1
!          !CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_PREV,IOUT,FL%IU)
!
!          !!!! Read name of reservoir
!          !!!CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT, EOL=ISTOP)
!          !!!SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME = trim(LINE(:ISTOP))
!          !!!
!          !!!  WRITE(IOUT,*) "   RESNAME  = ", trim(adjustl(SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME))
!          !!!  WRITE(IOUT,*) "   PROJID   = ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_PROJID
!          !!!  WRITE(IOUT,*) "   RESID    = ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RESID
!          !!!  WRITE(IOUT,*) "   RELSEG   = ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RELSEG
!          !!!  WRITE(IOUT,*) "   STORAGE  = ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_PREV
!          !!!  WRITE(IOUT,*) "   HEADCELL = ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%HEADCELL
!
!          ! Assign tabfiles to mass balance terms
!
!          !CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!
!          NTABVAR = 5                                                   ! inflow,prcp,evap,RELEASE_SPEC,storage_nprj
!          DO I = 1,NTABVAR
!
!            LLOC=1
!            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,FL%IU)          ! read intput type
!
!            SELECT CASE( LINE(ISTART:ISTOP) )
!            CASE('RES_INFLOW')
!              CALL TABFILEPARSE(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW, .TRUE. )
!              CALL TABFILELINKS(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW )
!              IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW%NTAB.GT.0) THEN
!                CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_INFLOW, INDEXLST=TMPLST1D )
!              ELSE
!                  WRITE(IOUT,*) " "
!                  WRITE(IOUT,*) "WHOA..."
!                  WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                  WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                  WRITE(IOUT,*) "Project:  ", IPROJ
!                  WRITE(IOUT,*) "Reservoir:", NRES
!                  WRITE(IOUT,*) "Res. Name:",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                  WRITE(IOUT,*) "TABFILE: RES_INFLOW"
!                  WRITE(IOUT,*) "STOPPING CODE..."
!                CALL USTOP(' ')
!              END IF !(NTAB.GT.0)
!
!            CASE('RES_PRCP')
!              CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP, .TRUE. )
!              CALL TABFILELINKS(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP )
!              IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP%NTAB.GT.0) THEN
!                CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_PRCP, INDEXLST=TMPLST1D )
!              ELSE
!                  WRITE(IOUT,*) " "
!                  WRITE(IOUT,*) "WHOA..."
!                  WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                  WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                  WRITE(IOUT,*) "Project:  ", IPROJ
!                  WRITE(IOUT,*) "Reservoir:", NRES
!                  WRITE(IOUT,*) "Res. Name:", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                  WRITE(IOUT,*) "TABFILE: RES_PRCP"
!                  WRITE(IOUT,*) "STOPPING CODE..."
!                CALL USTOP(' ')
!              END IF !(NTAB.GT.0)
!
!            CASE('RES_EVAP')
!              CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP, .TRUE. )
!              CALL TABFILELINKS(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP )
!              IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP%NTAB.GT.0) THEN
!                CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_EVAP, INDEXLST=TMPLST1D )
!              ELSE
!                  WRITE(IOUT,*) " "
!                  WRITE(IOUT,*) "WHOA..."
!                  WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                  WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                  WRITE(IOUT,*) "Project:  ", IPROJ
!                  WRITE(IOUT,*) "Reservoir:", NRES
!                  WRITE(IOUT,*) "Res. Name:",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                  WRITE(IOUT,*) "TABFILE: RES_EVAP"
!                  WRITE(IOUT,*) "STOPPING CODE..."
!                CALL USTOP(' ')
!              END IF !(NTAB.GT.0)
!
!            CASE('RES_RELEASE_SPEC')
!              CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC, .TRUE. )
!              CALL TABFILELINKS(FL%IU,IOUT,LINE, SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC )
!              IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC%NTAB.GT.0) THEN
!                CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_RELEASE_SPEC, INDEXLST=TMPLST1D )
!              ELSE
!                  WRITE(IOUT,*) " "
!                  WRITE(IOUT,*) "WHOA..."
!                  WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                  WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                  WRITE(IOUT,*) "Project:  ", IPROJ
!                  WRITE(IOUT,*) "Reservoir:", NRES
!                  WRITE(IOUT,*) "Res. Name:", SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                  WRITE(IOUT,*) "TABFILE: RES_RELEASE_SPEC"
!                  WRITE(IOUT,*) "STOPPING CODE..."
!                CALL USTOP(' ')
!              END IF !(NTAB.GT.0)
!
!            CASE('RES_STORAGE_NPRJ')
!              CALL TABFILEPARSE(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ, .TRUE. )
!              CALL TABFILELINKS(FL%IU,IOUT,LINE,SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ )
!              IF (SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ%NTAB.GT.0) THEN
!                CALL TABFILEPACKINDEX(SWO%RESDAT(IPROJ)%RESBAL(NRES)%TABIDX_STORAGE_NPRJ, INDEXLST=TMPLST1D )
!              ELSE
!                  WRITE(IOUT,*) " "
!                  WRITE(IOUT,*) "WHOA..."
!                  WRITE(IOUT,*) "HOLD ON THERE, TIGER"
!                  WRITE(IOUT,*) "MassBalRes w/ NTAB<=0?"
!                  WRITE(IOUT,*) "Project:  ", IPROJ
!                  WRITE(IOUT,*) "Reservoir:", NRES
!                  WRITE(IOUT,*) "Res. Name:",SWO%RESDAT(IPROJ)%RESBAL(NRES)%RESNAME
!                  WRITE(IOUT,*) "TABFILE: RES_STORAGE_NPRJ"
!                  WRITE(IOUT,*) "STOPPING CODE..."
!                CALL USTOP(' ')
!              END IF !(NTAB.GT.0)
!
!            CASE DEFAULT
!                WRITE(IOUT,*) "ERROR @ RESERVOIR TABFILE INPUTS"
!                WRITE(IOUT,*) "KEYWORD NOT RECOGNIZED:",LINE(ISTART:ISTOP)
!                WRITE(IOUT,*) "VALID KEYWORD OPTIONS ARE:"
!                WRITE(IOUT,*) "** RES_INFLOW"
!                WRITE(IOUT,*) "** RES_PRCP"
!                WRITE(IOUT,*) "** RES_EVAP"
!                WRITE(IOUT,*) "** RES_RELEASE_SPEC"
!                WRITE(IOUT,*) "** RES_STORAGE_NPRJ"
!              CALL USTOP(' ')
!
!            END SELECT
!          END DO !(DO I = 1,NTABVAR)
!
!          ! Allocate/init remaining mass balance vars (computed vars -- not inputs)
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE        = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PROJ    = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_FLOD    = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%SPILL_WAY      = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA           = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_PREV      = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%AREA_DPL       = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_PMAX   = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%PROJ_RELEASE_DMD   = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE    = DZ
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%MAX_RELEASE_S  = inf
!          SWO%RESDAT(IPROJ)%RESBAL(NRES)%ADD_RELEASE_S  = DZ
!          !
!          DO NRES = ONE, SWO%NRES_SPT(IPROJ)
!             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%ELEV         = DZ
!             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%ELEV_PREV    = DZ
!             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%MAX_ELEV     = inf
!             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%MIN_ELEV     = ninf
!             SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%MAIN_RES     = FALSE
!          END DO
!          !
!          DTMP1 = ninf
!          I = ONE
!          DO NRES = 1, SWO%NRES_SPT(IPROJ)
!                N  = SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_COUNT
!                IF(DTMP1 < SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(N))THEN
!                   DTMP1 = SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(N)
!                   I = NRES
!                END IF
!          END DO
!          SWO%RESDAT(IPROJ)%RESSPLIT(I)%MAIN_RES     = TRUE  !SET BIGGEST STORAGE TO TRUE
!
!          ! Loop to allocate RESSPLIT
!          !!!DO NRES=1,SWO%NRES_SPT(IPROJ)
!          !!!
!          !!!  ! Read first line -- ProjID, ResID, HEADCELL (keyword)
!          !!!  LLOC=1
!          !!!  IF (NRES.GT.1) CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!          !!!  CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_PROJID,R,IOUT,FL%IU)
!          !!!  LLOC=ISTOP+1
!          !!!  CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_RESID,R,IOUT,FL%IU)
!          !!!  LLOC=ISTOP+1
!          !!!  CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,FL%IU)
!          !!!  IF (LINE(ISTART:ISTOP).EQ."HEADCELL") THEN
!          !!!      SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%HEADCELL = 1
!          !!!  ELSE
!          !!!      SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%HEADCELL = 0
!          !!!  END IF !(HEADCELL)
!          !!!
!          !!!  ! Read name of reservoir
!          !!!  !CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!          !!!  SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESNAME = trim(adjustl(LINE(1:65)))
!          !!!
!          !!!    WRITE(IOUT,*) "   RESNAME  = ", trim(adjustl(SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESNAME))
!          !!!    WRITE(IOUT,*) "   PROJID   = ", SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_PROJID
!          !!!    WRITE(IOUT,*) "   RESID    = ", SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_RESID
!          !!!    WRITE(IOUT,*) "   HEADCELL = ", SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%HEADCELL
!          !!!
!          !!!  ! ACAP (area-capacity-elevation table)
!          !!!  SWO%RESDAT(IPROJ)%ACAP(NRES)%RESNAME = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESNAME
!          !!!  SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_PROJID = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_PROJID
!          !!!  SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_RESID = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_RESID
!          !!!  CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!          !!!  CALL SWOPS_READACAP(SWO,FL, LINE,IPROJ,NRES)
!          !!!
!          !!!  ! FRAC (Reservoir Storage Fraction Table)
!          !!!  SWO%RESDAT(IPROJ)%FRAC(NRES)%RESNAME = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESNAME
!          !!!  SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_PROJID = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_PROJID
!          !!!  SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_RESID = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_RESID
!          !!!  CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
!          !!!  CALL SWOPS_READFRAC(SWO,FL%IU,LINE,IPROJ,NRES)
!          !!!
!          !!!  !!!! HEAD (Head Boundary Cells)
!          !!!  !!!IF (SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%HEADCELL.EQ.1) THEN
!          !!!  !!!  SWO%RESDAT(IPROJ)%HEAD(NRES)%RESNAME = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESNAME
!          !!!  !!!  SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_PROJID = SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_PROJID
!          !!!  !!!  SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_RESID  =  SWO%RESDAT(IPROJ)%RESSPLIT(NRES)%RESSPLIT_RESID
!          !!!  !!!  CALL SWOPS_READHEAD( SWO,FL%IU,LINE, SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_NCELL, SWO%RESDAT(IPROJ)%HEAD(NRES)%HEAD_CELLS)
!          !!!  !!!END IF !(HEADCELL.EQ.1)
!          !!!
!          !!!END DO !(NRES=1,NRES_SPT(NP))
!
!        END IF !(IF (IRESFL.EQ.0)/ELSE IF(IRESFL.GT.0)/ELSE)
!    END DO !(NP=1,SWO%NPROJ)
!      !
!    BACKSPACE(FL%IU)
!  END SUBROUTINE
  !
  SUBROUTINE SWOPS_READACAP(SWO, FL, LINE, IPROJ, NRES, SF)
!-----VERSION X 2014.07.11 SWOPS_READACAP
!     ******************************************************************
!     OVERVIEW:
!     READ ACAP TABLE FOR A SINGLE RESERVOIR
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    CLASS(SWO_DATA), INTENT(INOUT):: SWO
    TYPE(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),             INTENT(INOUT):: LINE
    INTEGER,                  INTENT(IN   ):: IPROJ, NRES
    TYPE(SFAC_DATA),          INTENT(IN   ):: SF
    TYPE(SFAC_DATA):: SFAC
    CHARACTER(5):: EXT
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER LLOC,ISTART,ISTOP,I,COUNTER
!   ------------------------------------------------------------------

    ! Open ACAP file...
    !LLOC=1
    !CALL FL%OPEN(LLOC, LINE, SWO%IOUT, IU, REQKEY=TRUE)
    !IF(FL%IU == Z) CALL STOP_ERROR(INFILE=IU, OUTPUT=SWO%IOUT,MSG='INTERNAL KEYWORD IS NOT ALLOWED FOR ACAP TABLE FOR A SINGLE RESERVOIR. PLEASE USE "EXTERNAL" OR "OPEN/CLOSE". NOTE THAT IF YOU USE "EXTERNAL", IT MUST BE THE ONLY DATA WITHIN THE FILE.')
    CALL FL%COUNT_LINES(COUNTER)
    !
    CALL FL%REWIND()
    SFAC = SF
    DO I = 1,COUNTER
         CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
         !
         LLOC = ONE
         CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,EXT,COM_STOP=TRUE)   !GET VALUE
         !
         IF (EXT == 'SFAC') THEN
             !
             COUNTER = COUNTER - ONE
             !
             CALL SFAC%LOAD(LINE, FL%IU, SWO%IOUT, EX1_WORD='CAPACITY', EX1_DIM=ONE, EX2_WORD='AREA', EX2_DIM=ONE, EX3_WORD='ELEVATION', EX3_DIM=ONE, NO_INTERNAL=TRUE)
         ELSE
             CALL FL%BACK()
             EXIT
         END IF
    END DO
    !
    WRITE(SWO%IOUT,'(2(A,I3),2A)') 'PROEJCT ',IPROJ,' RESERVOIR ',NRES, " ACAP COUNTER = ", NUM2STR(COUNTER)

    ! Allocate table vars, read table values
    WRITE(SWO%IOUT,*) "  --> Allocating Eevation Area Capacity look-up table arrays"
    SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_COUNT = COUNTER
    ALLOCATE( SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(COUNTER), &
              SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_AREA(COUNTER),    &
              SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_ELEV(COUNTER) )
    SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(COUNTER) = -99
    SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_AREA(COUNTER)    = -99
    SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_ELEV(COUNTER)    = -99

    WRITE(SWO%IOUT,*) "      ALLOCATION DONE."

    ! Read table values
    WRITE(SWO%IOUT,*) "  --> Reading look-up table values"
    !
    DO I = 1,COUNTER
      CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
      LLOC = 1
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_ELEV(I),   MSG='FAILED TO LOAD ACAP ELEVATION')
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_AREA(I),   MSG='FAILED TO LOAD ACAP AREA')
      CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(I),MSG='FAILED TO LOAD ACAP STORARGE')
      !CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(I),MSG='FAILED TO LOAD ACAPA STORARGE')
      !CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_AREA(I),   MSG='FAILED TO LOAD ACAPA AREA')
      !CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_ELEV(I),   MSG='FAILED TO LOAD ACAPA ELEVATION')
    END DO !(I = 1,COUNTER)
    !
    ASSOCIATE(ELEV => SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_ELEV, AREA => SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_AREA, STORAGE => SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE)
       IF(SFAC%HAS_ALL) THEN
                        ELEV    = SFAC%ALL * ELEV
                        AREA    = SFAC%ALL * AREA
                        STORAGE = SFAC%ALL * STORAGE
       END IF
       !
       IF(SFAC%HAS_EX1) STORAGE = SFAC%EX1(ONE) * STORAGE
       IF(SFAC%HAS_EX2) AREA    = SFAC%EX2(ONE) * AREA
       IF(SFAC%HAS_EX3) ELEV    = SFAC%EX3(ONE) * ELEV
    END ASSOCIATE
    !
    ! Print values
    !        WRITE(SWO%IOUT,*) "      ACAP TABLE:"
    !        WRITE(SWO%IOUT,*) " "
    !        WRITE(SWO%IOUT,102)
    !        DO I = 1,COUNTER
    !          WRITE(SWO%IOUT,103) SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_STORAGE(I),&
    !                          SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_AREA(I),   &
    !                          SWO%RESDAT(IPROJ)%ACAP(NRES)%ACAP_ELEV(I)
    !        END DO !(I = 1,COUNTER)
    !102     FORMAT("        STORAGE", &
    !               "           AREA", &
    !               "      ELEVATION")
    !103     FORMAT(3D15.4)
    !
  END SUBROUTINE SWOPS_READACAP
  !
  SUBROUTINE SWOPS_READFRAC(SWO,FL, LINE,IPROJ,NRES, INFILE)
    CLASS(SWO_DATA), INTENT(INOUT):: SWO
!-----VERSION X 2014.07.11 SWOPS_READFRAC
!     ******************************************************************
!     OVERVIEW:
!     READ RESERVOIR FRACTION TABLE FOR A SINGLE RESERVOIR
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    TYPE(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    INTEGER,                  INTENT(IN   ):: IPROJ,NRES,INFILE
    CHARACTER(*):: LINE
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER LLOC,ISTART,ISTOP,I,COUNTER
    TYPE(DATE_OPERATOR):: DATE
    DOUBLE PRECISION:: TMP
    LOGICAL:: FOUND
!   ------------------------------------------------------------------

    ! Open FRAC file...
    IF(FL%IS_CONSTANT) THEN
        COUNTER = ONE
        ALLOCATE( SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_NOLEAP( COUNTER), &
                  !SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_LEAP(   COUNTER), &
                  SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_FRAC(COUNTER) )
        SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_COUNT = COUNTER
        !
        SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_NOLEAP = DOS     !Ensures search always stops on first DOY check
        SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_FRAC   = FL%SCALE
    ELSE
        IF(FL%IU == Z .OR. FL%IS_INTERNAL) CALL STOP_ERROR(INFILE=INFILE, OUTPUT=SWO%IOUT,MSG='INTERNAL KEYWORD IS NOT ALLOWED FOR RESERVOIR FRACTION TABLE FOR A SINGLE RESERVOIR. PLEASE USE "EXTERNAL" OR "OPEN/CLOSE". NOTE THAT IF YOU USE "EXTERNAL", IT MUST BE THE ONLY DATA WITHIN THE FILE.')
        CALL FL%COUNT_LINES(COUNTER)
        WRITE(SWO%IOUT,'(2(A,I3),2A)') 'PROEJCT ',IPROJ,' RESERVOIR ',NRES, " RESERVOIR FRACTION = ", NUM2STR(COUNTER)

        ! Allocate table vars, read table values
        WRITE(SWO%IOUT,*) "  --> Allocating look-up table arrays"
        ALLOCATE( SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_NOLEAP( COUNTER), &
                    !SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_LEAP(   COUNTER), &
                    SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_FRAC(COUNTER) )
        SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_COUNT = COUNTER

        WRITE(SWO%IOUT,*) "      ALLOCATION DONE."

        ! Read table values
        WRITE(SWO%IOUT,*) "  --> Reading look-up table values"
        DO I = 1,COUNTER
          CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
          !
          LLOC = 1
          CALL GET_DOUBLE_DATE(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,TMP,DATE,FOUND_DATE=FOUND, MSG='FAILED TO LOAD FRACTION OF NON-LEAP YEAR OR CALENDAR DATE')
          IF(SWO%LEAP_YEAR) THEN
               IF(FOUND) THEN
                   SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_NOLEAP(I) = DATE%YEAR_FRACTION(FALSE)
                   !SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_LEAP  (I) = DATE%YEAR_FRACTION(TRUE )
               ELSE
                   SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_NOLEAP(I) = TMP * 365.25D0/365D0
                   !SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_LEAP  (I) = TMP * 365.25D0/366D0
               END IF
          ELSE
                   SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_NOLEAP(I) = TMP
                   !SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_LEAP  (I) = TMP
          END IF
          !
          CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_FRAC(I),MSG='FAILED TO LOAD Split Fraction')
        END DO !(I = 1,COUNTER)
        !
        !          ! Print values
        !              WRITE(SWO%IOUT,*) "      FRAC TABLE:"
        !              WRITE(SWO%IOUT,*) " "
        !              WRITE(SWO%IOUT,102)
        !              DO I = 1,COUNTER
        !                WRITE(SWO%IOUT,103) SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_DOY(I), &
        !                                SWO%RESDAT(IPROJ)%FRAC(NRES)%FRAC_FRAC(I)
        !              END DO !(I = 1,COUNTER)
        !102           FORMAT("    Day-Of-Year", &
        !                     "       Fraction")
        !103           FORMAT(2D15.4)
    END IF
    !
  END SUBROUTINE SWOPS_READFRAC
!
  SUBROUTINE SWOPS_READHEAD(SWO,FL,LINE,NCELL,CELLS,IPROJ,IRES)
!-----VERSION X 2014.07.11 SWOPS_READHEAD
!     ******************************************************************
!     OVERVIEW:
!     READ CELLS WHERE RESERVOIR CONTRIBUTES TO HEAD BOUNDARY
!
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!        ARGUMENTS:
!     ------------------------------------------------------------------
    CLASS(SWO_DATA),                   INTENT(INOUT):: SWO
    TYPE(GENERIC_INPUT_FILE),          INTENT(INOUT):: FL
    INTEGER,                           INTENT(  OUT):: NCELL
    CHARACTER(*),                      INTENT(INOUT):: LINE
    INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: CELLS
    INTEGER,                           INTENT(IN   ):: IPROJ, IRES
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    !CHARACTER(24)  :: ANAME
    INTEGER II,JJ,COUNTER,LLOC, IU
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: TMPGRID
!   ------------------------------------------------------------------
    !
    SWO%RESDAT(IPROJ)%HEAD(IRES)%HEAD_PROJID = IPROJ
    SWO%RESDAT(IPROJ)%HEAD(IRES)%HEAD_RESID  = IRES
    !
    IF(SWO%IRESFL(IPROJ) > Z) THEN
        IF(FL%NULL_FILE) THEN
            SWO%RESDAT(IPROJ)%RESBAL(IRES)%HEADCELL = Z
        ELSE
            SWO%RESDAT(IPROJ)%RESBAL(IRES)%HEADCELL = ONE
        END IF
    ELSE
        IF(FL%NULL_FILE) THEN
            SWO%RESDAT(IPROJ)%RESSPLIT(IRES)%HEADCELL = Z
        ELSE
            SWO%RESDAT(IPROJ)%RESSPLIT(IRES)%HEADCELL = ONE
        END IF
            SWO%RESDAT(IPROJ)%RESBAL  (1)%HEADCELL = Z
    END IF
    !
    ! Create temporary 2D array ...
    ALLOCATE( TMPGRID(SWO%NCOL,SWO%NROW) )
    LLOC = ONE
    IU = Z
    CALL ULOAD(TMPGRID, LLOC, LINE, SWO%IOUT, FL%IU, IU, NOID=TRUE, NO_INTERNAL=TRUE)

    ! Call U2DINT to read head indicator file ...
    !ANAME = 'Reservoir Head Boundary'
    !CALL U2DINT(TMPGRID,ANAME,NROW,NCOL,0,999,IOUT)

    ! Loop to count cells with non-zero value ...
    NCELL = 0
    DO II = 1, SWO%NROW
    DO JJ = 1, SWO%NCOL
    IF ( TMPGRID(JJ,II).NE.0 ) NCELL = NCELL + 1
    END DO
    END DO
    !
    ! Allocate CELLS array
    !
    IF(NCELL > Z) THEN
       ALLOCATE( CELLS(2,NCELL) )                                        ! CELLS(1,:) = Rows, CELLS(2,:) = Cols
       COUNTER = 0
       DO II = 1,SWO%NROW
         DO JJ = 1,SWO%NCOL
           IF (TMPGRID(JJ,II).NE.0) THEN
             COUNTER = COUNTER + 1
             CELLS(1,COUNTER) = II                                 ! Row...
             CELLS(2,COUNTER) = JJ                                 ! Col...
           END IF !(TMPGRID.NE.0)
         END DO !(JJ)
       END DO !(II)
    END IF
    !
    DEALLOCATE( TMPGRID )
    !
  END SUBROUTINE SWOPS_READHEAD
!
  SUBROUTINE SWO_READ_MAX_SPILL(SWO,FL,LINE,DAT,IPROJ,IRES,SF)
    CLASS(SWO_DATA),                   INTENT(INOUT):: SWO
    TYPE(GENERIC_INPUT_FILE),          INTENT(INOUT):: FL
    CHARACTER(*),                      INTENT(INOUT):: LINE
    TYPE(DOUBLE_MATRIX),               INTENT(INOUT):: DAT
    INTEGER,                           INTENT(IN   ):: IPROJ, IRES
    TYPE(SFAC_DATA),                   INTENT(IN   ):: SF
!
!   ------------------------------------------------------------------
    TYPE(SFAC_DATA):: SFAC
    CHARACTER(5):: EXT
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER LLOC,ISTART,ISTOP,I,DIM
!   ------------------------------------------------------------------
    !
    SFAC = SF
    !
    IF(FL%IS_CONSTANT) THEN
        CALL DAT%ALLOC(ONE,ONE,FL%SCALE)
        !
        IF(FL%SCALE < D100) THEN
                            IF(SFAC%HAS_ALL) DAT%MAT = DAT%MAT * SFAC%ALL
                            IF(SFAC%HAS_EX2) DAT%MAT = DAT%MAT * SFAC%EX2(ONE)
        END IF
    ELSEIF(FL%NULL_FILE) THEN
        CALL DAT%ALLOC(ONE,ONE,inf)
    ELSE
        CALL FL%COUNT_LINES(DIM)
        CALL FL%REWIND()
        !
        CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
        !
        DO I = 1, 10 !should never hav more then 10 SFAC
             !
             LLOC = ONE
             CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,EXT,COM_STOP=TRUE)   !GET VALUE
             !
             IF (EXT == 'SFAC') THEN
                 !
                 DIM = DIM - ONE
                 !
                 CALL SFAC%LOAD(LINE, FL%IU, SWO%IOUT, EX1_WORD='STORAGE', EX1_DIM=ONE, EX2_WORD='DISCHARGE', EX2_DIM=ONE, EX3_WORD='ELEVATION', NO_INTERNAL=TRUE)
                 !
                 CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
             ELSE
                 EXIT
             END IF
        END DO
        !
        CALL DAT%ALLOC(DIM,TWO)
        !
        DO I = 1, DIM
          !
          LLOC = ONE
          CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,DAT%MAT(I, ONE),   MSG='FAILED TO LOAD STORAGE OR ELEVATION PORTION OF SPILLWAY STAGE-DICHARGE TABLE')
          CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,SWO%IOUT,FL%IU,DAT%MAT(I, TWO),   MSG='FAILED TO LOAD DISCHARGE PORTION OF SPILLWAY STAGE-DICHARGE TABLE')
          !
          CALL READ_TO_DATA(LINE, FL%IU, SWO%IOUT)
        END DO
        !
        IF(SFAC%HAS_ALL) DAT%MAT = DAT%MAT * SFAC%ALL
        !
        IF(SFAC%HAS_EX1) DAT%MAT(:, ONE) = DAT%MAT(:, ONE) * SFAC%EX1(ONE)
        IF(SFAC%HAS_EX3) DAT%MAT(:, ONE) = DAT%MAT(:, ONE) * SFAC%EX3(ONE)
        IF(SFAC%HAS_EX2) DAT%MAT(:, TWO) = DAT%MAT(:, TWO) * SFAC%EX2(ONE)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SPLIT_RES_FRAC(SWO, IPROJ, IRES, DOY, FRAC, ISLEAP)
!-----VERSION X 2014.07.24 FRAC_POOL2SPLIT
!   ******************************************************************
!   OVERVIEW:
!   Return fraction of storage for specific reservoir in split
!   ------------------------------------------------------------------
    CLASS(SWO_DATA), INTENT(IN   ):: SWO
    INTEGER,         INTENT(IN   ):: IPROJ,IRES
    DOUBLE PRECISION,INTENT(IN   ):: DOY
    LOGICAL,         INTENT(IN   ):: ISLEAP
    DOUBLE PRECISION,INTENT(  OUT):: FRAC
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER             I,IMAX
    DOUBLE PRECISION :: DOY_LUT1,DOY_LUT2,  &
                        FRAC_LUT1,FRAC_LUT2,&
                        DFDD, LEAP
!   ------------------------------------------------------------------
!
    LEAP = UNO
    IF(SWO%LEAP_YEAR .AND. ISLEAP) LEAP = 365.D0 / 366.D0
    !
    DFDD = DZ; FRAC = DZ
    !
    IMAX = SWO%RESDAT(IPROJ)%FRAC(IRES)%FRAC_COUNT
    !
    ASSOCIATE(FRAC_DOY => SWO%RESDAT(IPROJ)%FRAC(IRES)%FRAC_NOLEAP, FRAC_FRAC => SWO%RESDAT(IPROJ)%FRAC(IRES)%FRAC_FRAC)
        !
         DO I = 1,IMAX
             IF (FRAC_DOY(I)*LEAP .GE. DOY) EXIT
         END DO !(I = 1,IMAX)

         IF (I.EQ.1) THEN
             FRAC           = FRAC_FRAC(I)
         ELSE IF (I.GE.IMAX) THEN
             FRAC           = FRAC_FRAC(IMAX)
         ELSE
             IF (DOY.EQ.FRAC_DOY(I)*LEAP) THEN
                 FRAC       = FRAC_FRAC(I)
             ELSE
                 DOY_LUT1  = FRAC_DOY(I-1)*LEAP
                 DOY_LUT2  = FRAC_DOY(I)  *LEAP
                 FRAC_LUT1 = FRAC_FRAC(I-1)
                 FRAC_LUT2 = FRAC_FRAC(I)
                 !
                 IF (DOY_LUT1.EQ.DOY_LUT2) THEN
                   FRAC     = (FRAC_LUT2-FRAC_LUT1)/2.
                 ELSE
                   DFDD    = (FRAC_LUT2-FRAC_LUT1)/(DOY_LUT2-DOY_LUT1)
                   FRAC    = FRAC_LUT1 + DFDD*(DOY-DOY_LUT1)               ! linear interpolation between neighboring table values...
                 END IF !(DOY_LUT1.EQ.DOY_LUT2)
             END IF !(DOY.EQ.FRAC_DOY)
         END IF !(I.EQ.1/I.EQ.IMAX/OTHER)
    END ASSOCIATE
  END SUBROUTINE
  !
  PURE SUBROUTINE ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE,AREA)
!-----VERSION X 2014.07.24 ACAP_STOR2AREA
!   ******************************************************************
!   OVERVIEW:
!   Compute reservoir surface area from reservoir storage based on
!   linear interpolation of ACAP table.
!
!   NOTES:
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),           INTENT(IN   ):: SWO
    INTEGER,                   INTENT(IN   ):: IPROJ,IRES
    DOUBLE PRECISION,          INTENT(IN   ):: STORAGE
    DOUBLE PRECISION,          INTENT(  OUT):: AREA
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER             I,IMAX
    DOUBLE PRECISION :: STOR_LUT1,AREA_LUT1, STOR_LUT2,AREA_LUT2, DADS
!   ------------------------------------------------------------------
!
    IMAX = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_COUNT
    DO I = 1,IMAX
        IF (SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I).GE.STORAGE) EXIT
    END DO !(I = 1,IMAX)
    IF(I>IMAX) I = IMAX

    IF (I == ONE) THEN
        AREA          = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_AREA(I)
    ELSE IF (I.EQ.IMAX) THEN
        AREA          = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_AREA(I)
    ELSE
        IF (STORAGE.EQ.SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)) THEN
            AREA      = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_AREA(I)
        ELSE
            STOR_LUT1 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I-1)
            STOR_LUT2 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)
            AREA_LUT1 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_AREA(I-1)
            AREA_LUT2 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_AREA(I)
            DADS      = (AREA_LUT2-AREA_LUT1)/(STOR_LUT2-STOR_LUT1)
            !
            AREA = AREA_LUT1 + DADS*(STORAGE-STOR_LUT1)          ! linear interpolation between neighboring table values...
        END IF !(STORAGE.EQ.ACAP_STORAGE)
    END IF !(I.EQ.1/I.EQ.IMAX/OTHER)
    !
  END SUBROUTINE ACAP_STOR2AREA
  !
  PURE SUBROUTINE ACAP_STOR2ELEV(SWO,IPROJ,IRES,STORAGE,ELEV)
!-----VERSION X 2014.07.24 ACAP_STOR2ELEV
!   ******************************************************************
!   OVERVIEW:
!   Compute reservoir surface ELEV from reservoir storage based on
!   linear interpolation of ACAP table.
!
!   NOTES:
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),           INTENT(IN   ):: SWO
    INTEGER,                   INTENT(IN   ):: IPROJ,IRES
    DOUBLE PRECISION,          INTENT(IN   ):: STORAGE
    DOUBLE PRECISION,          INTENT(  OUT):: ELEV
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER             I,IMAX
    DOUBLE PRECISION :: STOR_LUT1,ELEV_LUT1, STOR_LUT2,ELEV_LUT2, DADS
!   ------------------------------------------------------------------
!
    IMAX = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_COUNT
    DO I = 1,IMAX
        IF (SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I).GE.STORAGE) EXIT
    END DO !(I = 1,IMAX)
    IF(I>IMAX) I = IMAX

    IF (I == ONE) THEN
        ELEV          = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I)
    ELSE IF (I.EQ.IMAX) THEN
        ELEV          = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I)
    ELSE
        IF (STORAGE.EQ.SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)) THEN
            ELEV      = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I)
        ELSE
            STOR_LUT1 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I-1)
            STOR_LUT2 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)
            ELEV_LUT1 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I-1)
            ELEV_LUT2 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I)
            DADS      = (ELEV_LUT2-ELEV_LUT1)/(STOR_LUT2-STOR_LUT1)
            !
            ELEV = ELEV_LUT1 + DADS*(STORAGE-STOR_LUT1)          ! linear interpolation between neighboring table values...
            !
        END IF !(STORAGE.EQ.ACAP_STORAGE)
    END IF !(I.EQ.1/I.EQ.IMAX/OTHER)
    !
  END SUBROUTINE ACAP_STOR2ELEV
  !
  PURE SUBROUTINE ACAP_ELEV2STOR(SWO,IPROJ,IRES,ELEV,STORAGE)
!-----VERSION X 2014.07.24 ACAP_STOR2ELEV
!   ******************************************************************
!   OVERVIEW:
!   Compute reservoir storage from surface elevation based on
!   linear interpolation of ACAP table.
!
!   NOTES:
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),           INTENT(IN   ):: SWO
    INTEGER,                   INTENT(IN   ):: IPROJ,IRES
    DOUBLE PRECISION,          INTENT(IN   ):: ELEV
    DOUBLE PRECISION,          INTENT(  OUT):: STORAGE
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER             I,IMAX
    DOUBLE PRECISION :: STOR_LUT1,ELEV_LUT1, STOR_LUT2,ELEV_LUT2, DADS
!   ------------------------------------------------------------------
!
    IMAX = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_COUNT
    DO I = 1,IMAX
        IF (SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I).GE.ELEV) EXIT
    END DO !(I = 1,IMAX)
    IF(I>IMAX) I = IMAX

    IF (I == ONE) THEN
        STORAGE       = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)
    ELSE IF (I.EQ.IMAX) THEN
        STORAGE       = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)
    ELSE
        IF (STORAGE.EQ.SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)) THEN
            STORAGE       = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)
        ELSE
            ELEV_LUT1 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I-1)
            ELEV_LUT2 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_ELEV(I)
            STOR_LUT1 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I-1)
            STOR_LUT2 = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(I)
            DADS      = (STOR_LUT2-STOR_LUT1)/(ELEV_LUT2-ELEV_LUT1)
            !
            STORAGE = STOR_LUT1 + DADS*(ELEV-ELEV_LUT1)          ! linear interpolation between neighboring table values...
            !
        END IF !(STORAGE.EQ.ACAP_STORAGE)
    END IF !(I.EQ.1/I.EQ.IMAX/OTHER)
    !
  END SUBROUTINE ACAP_ELEV2STOR
  !
  SUBROUTINE ACAP_CHECK(SWO,IOUT,IN)
!   ******************************************************************
!   OVERVIEW:
!   Check to see if ACAP is properly formatted
!
!   NOTES:
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),           INTENT(IN):: SWO
    INTEGER,                   INTENT(IN):: IOUT,IN
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER             I,J,K, ICAP
    CHARACTER(:), ALLOCATABLE:: ERROR
!   ------------------------------------------------------------------
!
    ERROR = NL
    ICAP = Z
    DO I=ONE, SWO%NPROJ
    DO J=ONE, SWO%NRES_TOT(I)
      ICAP = ICAP + ONE
      !
      ASSOCIATE(ELEV => SWO%RESDAT(I)%ACAP(J)%ACAP_ELEV, AREA => SWO%RESDAT(I)%ACAP(J)%ACAP_AREA, STOR => SWO%RESDAT(I)%ACAP(J)%ACAP_STORAGE, IMAX => SWO%RESDAT(I)%ACAP(J)%ACAP_COUNT)
         DO K = TWO, IMAX
             IF(ELEV(K) <= ELEV(K-ONE)) ERROR = ERROR//'THE ELEVATION WAS NOT ORDERED FROM SMALLEST TO LARGEST FOR PROJECT '//NUM2STR(I,3)//' AND RES ID '//NUM2STR(J,3)//' AND ERROR HAPPENED AT THE THE Ith POSITION '//NUM2STR(K,4)//' FOR ACAP TABLE NUMBER '//NUM2STR(ICAP)//NL
         END DO
         DO K = TWO, IMAX
             IF( NEAR_ZERO( STOR(K) - STOR(K-ONE) )) THEN
                 ERROR = ERROR//'TWO STORAGE VALUES ARE THE SAME VALUE FOR PROJECT '//NUM2STR(I,3)//' AND RES ID '//NUM2STR(J,3)//' AND ERROR HAPPENED AT THE THE Ith POSITION '//NUM2STR(K,4)//' FOR ACAP TABLE NUMBER '//NUM2STR(ICAP)//NL
             ELSEIF( (ELEV(K)-ELEV(K-ONE))/(STOR(K)-STOR(K-ONE)) < DZ ) THEN
                 ERROR = ERROR//'THE CHANGE IN ELEVATION WITH RESPECT TO STORAGE (dELEV/dSTOR) IS NEGATIVE FOR PROJECT '//NUM2STR(I,3)//' AND RES ID '//NUM2STR(J,3)//' AND ERROR HAPPENED AT THE THE Ith POSITION '//NUM2STR(K,4)//' FOR ACAP TABLE NUMBER '//NUM2STR(ICAP)//NL
             END IF
         END DO
         DO K = TWO, IMAX
             IF( NEAR_ZERO( AREA(K) - AREA(K-ONE) )) THEN
                 ERROR = ERROR//'TWO AREA VALUES ARE THE SAME VALUE FOR PROJECT '//NUM2STR(I,3)//' AND RES ID '//NUM2STR(J,3)//' AND ERROR HAPPENED AT THE THE Ith POSITION '//NUM2STR(K,4)//' FOR ACAP TABLE NUMBER '//NUM2STR(ICAP)//NL
             ELSEIF( (ELEV(K)-ELEV(K-ONE))/(AREA(K)-AREA(K-ONE)) < DZ ) THEN
                 ERROR = ERROR//'THE CHANGE IN ELEVATION WITH RESPECT TO AREA (dELEV/dAREA) IS NEGATIVE FOR PROJECT '//NUM2STR(I,3)//' AND RES ID '//NUM2STR(J,3)//' AND ERROR HAPPENED AT THE THE Ith POSITION '//NUM2STR(K,4)//' FOR ACAP TABLE NUMBER '//NUM2STR(ICAP)//NL
             END IF
         END DO
         !
      END ASSOCIATE
    END DO
    END DO
    !
    IF(ERROR.NE.NL) CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT,MSG='SWO BLOCK ERROR: THE FOLLOWING ERRORS WERE PRESENT IN THE ELEVATION_AREA_CAPACITY TABLES:'//ERROR)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE FRAC_POOL2SPLIT(SWO,IPROJ,IRES,DOY,STOR_POOL,STOR_SPLIT, ISLEAP)
!-----VERSION X 2014.07.24 FRAC_POOL2SPLIT
!   ******************************************************************
!   OVERVIEW:
!   Compute storage in split reservoir from storage in lumped (pooled)
!   project storage based on linear interpolation reservoir FRAC table
!
!   NOTES:
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
!   ------------------------------------------------------------------
!      ARGUMENTS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA), INTENT(IN   ):: SWO
    INTEGER,         INTENT(IN   ):: IPROJ,IRES
    DOUBLE PRECISION,INTENT(IN   ):: STOR_POOL,DOY
    LOGICAL,         INTENT(IN   ):: ISLEAP
    DOUBLE PRECISION,INTENT(  OUT):: STOR_SPLIT
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
    INTEGER             I,IMAX
    DOUBLE PRECISION :: FRC,            &
                        DOY_LUT1,DOY_LUT2,  &
                        FRAC_LUT1,FRAC_LUT2,&
                        DFDD, LEAP
!   ------------------------------------------------------------------
!
    LEAP = UNO
    IF(SWO%LEAP_YEAR .AND. ISLEAP) LEAP = 365.D0 / 366.D0
    !
    DFDD = DZ; FRC = DZ
    !
    IMAX = SWO%RESDAT(IPROJ)%FRAC(IRES)%FRAC_COUNT
    !
    ASSOCIATE(FRAC_DOY => SWO%RESDAT(IPROJ)%FRAC(IRES)%FRAC_NOLEAP, FRAC_FRAC => SWO%RESDAT(IPROJ)%FRAC(IRES)%FRAC_FRAC)
        !
         DO I = 1,IMAX
             IF (FRAC_DOY(I)*LEAP .GE. DOY) EXIT
         END DO !(I = 1,IMAX)

         IF (I.EQ.1) THEN
             FRC           = FRAC_FRAC(I)
         ELSE IF (I.GE.IMAX) THEN
             FRC           = FRAC_FRAC(IMAX)
         ELSE
             IF (DOY.EQ.FRAC_DOY(I)*LEAP) THEN
                 FRC       = FRAC_FRAC(I)
             ELSE
                 DOY_LUT1  = FRAC_DOY(I-1)*LEAP
                 DOY_LUT2  = FRAC_DOY(I)  *LEAP
                 FRAC_LUT1 = FRAC_FRAC(I-1)
                 FRAC_LUT2 = FRAC_FRAC(I)
                 !
                 IF (DOY_LUT1.EQ.DOY_LUT2) THEN
                   FRC     = (FRAC_LUT2-FRAC_LUT1)/2.
                 ELSE
                   DFDD    = (FRAC_LUT2-FRAC_LUT1)/(DOY_LUT2-DOY_LUT1)
                   FRC     = FRAC_LUT1 + DFDD*(DOY-DOY_LUT1)               ! linear interpolation between neighboring table values...
                 END IF !(DOY_LUT1.EQ.DOY_LUT2)
             END IF !(DOY.EQ.FRAC_DOY)
         END IF !(I.EQ.1/I.EQ.IMAX/OTHER)
    END ASSOCIATE
    !
    STOR_SPLIT = STOR_POOL * FRC

    !WRITE(IOUT,*) " "
    !WRITE(IOUT,*) "PRINT CHECK @ FRAC_POOL2SPLIT"
    !WRITE(IOUT,*) "IPROJ          =", IPROJ
    !WRITE(IOUT,*) "IRES           =", IRES
    !WRITE(IOUT,*) "DOY            =", DOY
    !WRITE(IOUT,*) "POOLED STORAGE =", STOR_POOL
    !WRITE(IOUT,*) "DOY_LUT1       =", DOY_LUT1
    !WRITE(IOUT,*) "DOY_LUT2       =", DOY_LUT2
    !WRITE(IOUT,*) "FRAC_LUT1      =", FRAC_LUT1
    !WRITE(IOUT,*) "FRAC_LUT2      =", FRAC_LUT2
    !WRITE(IOUT,*) "DFDD           =", DFDD
    !WRITE(IOUT,*) "FRC            =", FRC
    !WRITE(IOUT,*) "STOR_SPLIT     =", STOR_SPLIT
    !
  END SUBROUTINE FRAC_POOL2SPLIT
  !
  SUBROUTINE ALLOCATE_INITIALIZE_ADDITIONAL_VARIABLES(SWO,ISTRM)  !MOSTLY ALLOCATES MODULE SWO_GLOBAL_VARIABLES
    CLASS(SWO_DATA),                     INTENT(INOUT):: SWO
    INTEGER, DIMENSION(:,:), CONTIGUOUS, INTENT(IN   ):: ISTRM
    INTEGER:: NSEG, NSTRM, NRCH, ISEG, IRCH
    INTEGER:: IFARM, IUNIT, IDIST, IPROJ, IAUX

    !
    NSEG  = SWO%NSEG
    NSTRM = SWO%NSTRM
    !
    ! Allocate and initialize regular variables...
    !SWO%TSTART  = DZ; SWO%TSTOP  = DZ
    !SWO%YRSTART = DZ; SWO%YRSTOP = DZ
    !
    SWO%TS_START = DZ
    SWO%TS_STOP  = REALTIM
    SWO%SP_START = DZ
    SWO%SP_STOP  = REALTIM_PER
    !
    SWO%FRSTART  = DZ
    SWO%FRSTOP      = YEAR_FRACTION(REALTIM)
    SWO%IS_LEAP_END = ISLEAPYEAR(INT(REALTIM))
    SWO%DIVCOUNT = ONE
    SWO%SPTCOUNT = ONE

    ! Allocate and initialize SEGDATA/RCHDATA types
    ALLOCATE(SWO%SEGRCH_IN(NSEG),SWO%SEGRCH_OUT(NSEG))
    ALLOCATE(SWO%RCHDATA(NSTRM),SWO%SEGDATA(NSEG))
    ALLOCATE(SWO%RCHDATA_PREV(NSTRM),SWO%SEGDATA_PREV(NSEG))
    ALLOCATE(SWO%RCHDATA_SAVE(NSTRM),SWO%SEGDATA_SAVE(NSEG))
    !
    ALLOCATE( SWO%SEGINFO(NSEG))
    !
    ALLOCATE( SWO%UPSEG(NSEG), SWO%DNSEG(NSEG) )
    ALLOCATE( SWO%UPTREE_NAT(NSEG), SWO%UPTREE_CON1(NSEG), SWO%UPTREE_CON2(NSEG), SWO%UPTREE_RET(NSEG) )
    ALLOCATE( SWO%DNTREE_NAT(NSEG), SWO%DNTREE_CON1(NSEG), SWO%DNTREE_CON2(NSEG), SWO%DNTREE_RET(NSEG) )
    !
    IF(SWO%HAS_FMP_SW_LIMIT_RULZ) ALLOCATE(SWO%FMP_SW_LIMIT_RULZ(SWO%NFARM), SOURCE=inf)
    !
    DO ISEG = 1,NSEG

      ! Count number of reaches in segment
      NRCH = 0
      DO IRCH = 1,NSTRM
        IF (ISTRM(4,IRCH).EQ.ISEG) THEN
          NRCH = NRCH + 1
          IF (ISTRM(5,IRCH).EQ.1) SWO%SEGRCH_IN(ISEG) = IRCH
        END IF !(ISTRM(4,IRCH).EQ.ISEG)
      END DO !(IRCH)
      SWO%SEGRCH_OUT(ISEG) = SWO%SEGRCH_IN(ISEG) + NRCH - 1

      ! Assign seg info, initialize remaining vars
      SWO%SEGDATA(ISEG)%SegID           = ISEG
      SWO%SEGDATA(ISEG)%RchIDX          = SWO%SEGRCH_IN(ISEG)
      SWO%SEGDATA(ISEG)%NRCH            = NRCH
      SWO%SEGDATA(ISEG)%IUPSEG          = -99
      SWO%SEGDATA(ISEG)%OUTSEG          = -99
      SWO%SEGDATA(ISEG)%IPRIOR          = -99
      SWO%SEGDATA(ISEG)%DDFLAG          = -99
      SWO%SEGDATA(ISEG)%RRFLAG          = -99
      SWO%SEGDATA(ISEG)%LENGTH          = -99.D0
      SWO%SEGDATA(ISEG)%FLOW            = -99.D0
      SWO%SEGDATA(ISEG)%INFLOW          = -99.D0
      !SWO%SEGDATA(ISEG)%OUTFLOW         = -99.D0
      SWO%SEGDATA(ISEG)%SEEPAGE         = -99.D0
      SWO%SEGDATA(ISEG)%RUNOFF          = -99.D0
      SWO%SEGDATA(ISEG)%STRM_ET         = -99.D0
      SWO%SEGDATA(ISEG)%STRM_PRCP       = -99.D0
      SWO%SEGDATA(ISEG)%EFFICIENCY      = -99.D0
      SWO%SEGDATA_PREV(ISEG)%SegID      = ISEG
      SWO%SEGDATA_PREV(ISEG)%RchIDX     = SWO%SEGRCH_IN(ISEG)
      SWO%SEGDATA_PREV(ISEG)%NRCH       = NRCH
      SWO%SEGDATA_PREV(ISEG)%IUPSEG     = -99
      SWO%SEGDATA_PREV(ISEG)%OUTSEG     = -99
      SWO%SEGDATA_PREV(ISEG)%IPRIOR     = -99
      SWO%SEGDATA_PREV(ISEG)%DDFLAG     = -99
      SWO%SEGDATA_PREV(ISEG)%RRFLAG     = -99
      SWO%SEGDATA_PREV(ISEG)%LENGTH     = -99.D0
      SWO%SEGDATA_PREV(ISEG)%FLOW       = -99.D0
      SWO%SEGDATA_PREV(ISEG)%INFLOW     = -99.D0
      !SWO%SEGDATA_PREV(ISEG)%OUTFLOW    = -99.D0
      SWO%SEGDATA_PREV(ISEG)%SEEPAGE    = -99.D0
      SWO%SEGDATA_PREV(ISEG)%RUNOFF     = -99.D0
      SWO%SEGDATA_PREV(ISEG)%STRM_ET    = -99.D0
      SWO%SEGDATA_PREV(ISEG)%STRM_PRCP  = -99.D0
      SWO%SEGDATA_PREV(ISEG)%EFFICIENCY = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%SegID      = ISEG
      SWO%SEGDATA_SAVE(ISEG)%RchIDX     = SWO%SEGRCH_IN(ISEG)
      SWO%SEGDATA_SAVE(ISEG)%NRCH       = NRCH
      SWO%SEGDATA_SAVE(ISEG)%IUPSEG     = -99
      SWO%SEGDATA_SAVE(ISEG)%OUTSEG     = -99
      SWO%SEGDATA_SAVE(ISEG)%IPRIOR     = -99
      SWO%SEGDATA_SAVE(ISEG)%DDFLAG     = -99
      SWO%SEGDATA_SAVE(ISEG)%RRFLAG     = -99
      SWO%SEGDATA_SAVE(ISEG)%LENGTH     = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%FLOW       = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%INFLOW     = -99.D0
      !SWO%SEGDATA_SAVE(ISEG)%OUTFLOW    = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%SEEPAGE    = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%RUNOFF     = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%STRM_ET    = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%STRM_PRCP  = -99.D0
      SWO%SEGDATA_SAVE(ISEG)%EFFICIENCY = -99.D0

      ! Assign rch info, initialize remaining vars
      NRCH    = 0
      DO IRCH = SWO%SEGRCH_IN(ISEG),SWO%SEGRCH_OUT(ISEG)
        NRCH  = NRCH + 1
        SWO%RCHDATA(IRCH)%RchID           = IRCH
        SWO%RCHDATA(IRCH)%RchSeg          = ISEG
        SWO%RCHDATA(IRCH)%RchCnt          = NRCH
        SWO%RCHDATA(IRCH)%LENGTH          = -99.D0
        SWO%RCHDATA(IRCH)%INFLOW          = -99.D0
        !SWO%RCHDATA(IRCH)%OUTFLOW         = -99.D0
        SWO%RCHDATA(IRCH)%SEEPAGE         = -99.D0
        SWO%RCHDATA(IRCH)%RUNOFF          = -99.D0
        SWO%RCHDATA(IRCH)%STRM_ET         = -99.D0
        SWO%RCHDATA(IRCH)%STRM_PRCP       = -99.D0
        SWO%RCHDATA(IRCH)%EFFICIENCY      = -99.D0
        SWO%RCHDATA_PREV(IRCH)%RchID      = IRCH
        SWO%RCHDATA_PREV(IRCH)%RchSeg     = ISEG
        SWO%RCHDATA_PREV(IRCH)%RchCnt     = NRCH
        SWO%RCHDATA_PREV(IRCH)%LENGTH     = -99.D0
        SWO%RCHDATA_PREV(IRCH)%INFLOW     = -99.D0
        !SWO%RCHDATA_PREV(IRCH)%OUTFLOW    = -99.D0
        SWO%RCHDATA_PREV(IRCH)%SEEPAGE    = -99.D0
        SWO%RCHDATA_PREV(IRCH)%RUNOFF     = -99.D0
        SWO%RCHDATA_PREV(IRCH)%STRM_ET    = -99.D0
        SWO%RCHDATA_PREV(IRCH)%STRM_PRCP  = -99.D0
        SWO%RCHDATA_PREV(IRCH)%EFFICIENCY = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%RchID      = IRCH
        SWO%RCHDATA_SAVE(IRCH)%RchSeg     = ISEG
        SWO%RCHDATA_SAVE(IRCH)%RchCnt     = NRCH
        SWO%RCHDATA_SAVE(IRCH)%LENGTH     = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%INFLOW     = -99.D0
        !SWO%RCHDATA_SAVE(IRCH)%OUTFLOW    = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%SEEPAGE    = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%RUNOFF     = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%STRM_ET    = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%STRM_PRCP  = -99.D0
        SWO%RCHDATA_SAVE(IRCH)%EFFICIENCY = -99.D0
      END DO !(IRCH)
    END DO !(ISEG)

    ! Initialize remaining components of FARM, UNIT, DIST, PROJ
    DO IFARM = 1, SWO%NFARM
      SWO%FARM(IFARM)%AreaTot           = DZ
      SWO%FARM(IFARM)%AreaIrr           = DZ
      SWO%FARM(IFARM)%TFDR              = DZ
      SWO%FARM(IFARM)%ALLOTMENT         = DZ
      SWO%FARM(IFARM)%BALANCE           = DZ
      SWO%FARM(IFARM)%DELORDER          = DZ
      SWO%FARM(IFARM)%DELIVERY          = DZ
      SWO%FARM(IFARM)%DELIVERY_YTD      = DZ
      SWO%FARM(IFARM)%REQ_DELIVERY_VOL  = DZ
    END DO !(IFARM)

    DO IUNIT = 1,SWO%NUNIT
      SWO%UNIT(IUNIT)%AreaTot            = DZ
      SWO%UNIT(IUNIT)%AreaIrr            = DZ
      SWO%UNIT(IUNIT)%DELORDER           = DZ
      SWO%UNIT(IUNIT)%DIVORDER           = DZ
      SWO%UNIT(IUNIT)%DIVERSION          = DZ
      SWO%UNIT(IUNIT)%DELIVERY           = DZ
      SWO%UNIT(IUNIT)%BYPASS             = DZ
      SWO%UNIT(IUNIT)%CHARGE             = DZ
      SWO%UNIT(IUNIT)%CREDIT             = DZ
      SWO%UNIT(IUNIT)%DELIVEFF           = UNO !0.75 !THIS WILL BE UPDATED BY CONVERGENCE CHECK
      SWO%UNIT(IUNIT)%CHGRATIO           = UNO
      SWO%UNIT(IUNIT)%NETCHGRATIO        = UNO
      SWO%UNIT(IUNIT)%DIVIN              = DZ
      SWO%UNIT(IUNIT)%SUMIN              = DZ
      SWO%UNIT(IUNIT)%SUMOUT             = DZ
      SWO%UNIT(IUNIT)%DIVERSION_YTD      = DZ
      SWO%UNIT(IUNIT)%DELIVERY_YTD       = DZ
      SWO%UNIT(IUNIT)%BYPASS_YTD         = DZ
      SWO%UNIT(IUNIT)%CHARGE_YTD         = DZ
      SWO%UNIT(IUNIT)%CREDIT_YTD         = DZ
      SWO%UNIT(IUNIT)%DELIVEFF_YTD       = UNO !0.75
      SWO%UNIT(IUNIT)%CHGRATIO_YTD       = UNO
      SWO%UNIT(IUNIT)%NETCHGRATIO_YTD    = UNO
      SWO%UNIT(IUNIT)%DIVIN_YTD          = DZ
      SWO%UNIT(IUNIT)%SUMIN_YTD          = DZ
      SWO%UNIT(IUNIT)%SUMOUT_YTD         = DZ
      SWO%UNIT(IUNIT)%ALLOTMENT          = DZ
      SWO%UNIT(IUNIT)%BALANCE            = DZ
    END DO !(IUNIT)

    DO IDIST = 1,SWO%NDIST
      SWO%DIST(IDIST)%NFARM              = Z
      SWO%DIST(IDIST)%NAUXDEM            = Z
      SWO%DIST(IDIST)%ALLOC_ANN          = DZ
      SWO%DIST(IDIST)%ALLOC_CO           = DZ
      SWO%DIST(IDIST)%ALLOC_TOTAL        = DZ
      SWO%DIST(IDIST)%EQ_ALLOTMENT       = DZ
      SWO%DIST(IDIST)%AreaTot            = DZ
      SWO%DIST(IDIST)%AreaIrr            = DZ
      SWO%DIST(IDIST)%AreaAlloc          = DZ
      SWO%DIST(IDIST)%DELORDER           = DZ
      SWO%DIST(IDIST)%DIVORDER           = DZ
      SWO%DIST(IDIST)%DIVERSION          = DZ
      SWO%DIST(IDIST)%DELIVERY           = DZ
      SWO%DIST(IDIST)%BYPASS             = DZ
      SWO%DIST(IDIST)%CHARGE             = DZ
      SWO%DIST(IDIST)%CREDIT             = DZ
      SWO%DIST(IDIST)%DELIVEFF           = UNO !0.75 !THIS WILL BE UPDATED BY CONVERGENCE CHECK
      SWO%DIST(IDIST)%CHGRATIO           = UNO
      SWO%DIST(IDIST)%NETCHGRATIO        = UNO
      SWO%DIST(IDIST)%DIVIN              = DZ
      SWO%DIST(IDIST)%SUMIN              = DZ
      SWO%DIST(IDIST)%SUMOUT             = DZ
      SWO%DIST(IDIST)%DIVERSION_YTD      = DZ
      SWO%DIST(IDIST)%DELIVERY_YTD       = DZ
      SWO%DIST(IDIST)%BYPASS_YTD         = DZ
      SWO%DIST(IDIST)%CHARGE_YTD         = DZ
      SWO%DIST(IDIST)%CREDIT_YTD         = DZ
      SWO%DIST(IDIST)%DELIVEFF_YTD       = UNO !0.75
      SWO%DIST(IDIST)%CHGRATIO_YTD       = UNO
      SWO%DIST(IDIST)%NETCHGRATIO_YTD    = UNO
      SWO%DIST(IDIST)%DIVIN_YTD          = DZ
      SWO%DIST(IDIST)%SUMIN_YTD          = DZ
      SWO%DIST(IDIST)%SUMOUT_YTD         = DZ
      SWO%DIST(IDIST)%BALANCE            = DZ
      SWO%DIST(IDIST)%TFDR               = DZ
    END DO !(IDIST)

    DO IPROJ = 1,SWO%NPROJ
      SWO%PROJ(IPROJ)%AreaTot            = DZ
      SWO%PROJ(IPROJ)%AreaIrr            = DZ
      SWO%PROJ(IPROJ)%DELORDER           = DZ
      SWO%PROJ(IPROJ)%DIVORDER           = DZ
      SWO%PROJ(IPROJ)%DIVERSION          = DZ
      !SWO%PROJ(IPROJ)%OUTFLOW            = DZ
      SWO%PROJ(IPROJ)%DELIVERY           = DZ
      SWO%PROJ(IPROJ)%BYPASS             = DZ
      SWO%PROJ(IPROJ)%CHARGE             = DZ
      SWO%PROJ(IPROJ)%CREDIT             = DZ
      SWO%PROJ(IPROJ)%DELIVEFF           = UNO
      SWO%PROJ(IPROJ)%RELEASE            = DZ
      SWO%PROJ(IPROJ)%DIVRATIO           = UNO
      SWO%PROJ(IPROJ)%CHGRATIO           = UNO
      SWO%PROJ(IPROJ)%NETCHGRATIO        = UNO
      SWO%PROJ(IPROJ)%DIVIN              = DZ
      SWO%PROJ(IPROJ)%SUMIN              = DZ
      SWO%PROJ(IPROJ)%SUMOUT             = DZ
      SWO%PROJ(IPROJ)%DIVERSION_YTD      = DZ
      !SWO%PROJ(IPROJ)%OUTFLOW_YTD        = DZ
      SWO%PROJ(IPROJ)%DELIVERY_YTD       = DZ
      SWO%PROJ(IPROJ)%BYPASS_YTD         = DZ
      SWO%PROJ(IPROJ)%CHARGE_YTD         = DZ
      SWO%PROJ(IPROJ)%CREDIT_YTD         = DZ
      SWO%PROJ(IPROJ)%DELIVEFF_YTD       = UNO
      SWO%PROJ(IPROJ)%RELEASE_YTD        = DZ
      SWO%PROJ(IPROJ)%DIVRATIO_YTD       = UNO
      SWO%PROJ(IPROJ)%CHGRATIO_YTD       = UNO
      SWO%PROJ(IPROJ)%NETCHGRATIO_YTD    = UNO
      SWO%PROJ(IPROJ)%DIVIN_YTD          = DZ
      SWO%PROJ(IPROJ)%SUMIN_YTD          = DZ
      SWO%PROJ(IPROJ)%SUMOUT_YTD         = DZ
    END DO !(IPROJ)

    DO IAUX = 1,SWO%NAUXDEM
      SWO%AUXDEM(IAUX)%AREA              = DZ
      SWO%AUXDEM(IAUX)%ALLOTMENT         = DZ
      SWO%AUXDEM(IAUX)%BALANCE           = DZ
      SWO%AUXDEM(IAUX)%DELORDER          = DZ
      SWO%AUXDEM(IAUX)%DELIVERY          = DZ
      SWO%AUXDEM(IAUX)%DELIVERY_YTD      = DZ
      SWO%AUXDEM_PREV(IAUX)%ALLOTMENT    = DZ
      SWO%AUXDEM_PREV(IAUX)%BALANCE      = DZ
      SWO%AUXDEM_PREV(IAUX)%DELORDER     = DZ
      SWO%AUXDEM_PREV(IAUX)%DELIVERY     = DZ
      SWO%AUXDEM_PREV(IAUX)%DELIVERY_YTD = DZ
    END DO !(IAUX)
    !
    ! SET PREVIOUS VARIABLS
    !
    SWO%FARM_PREV = SWO%FARM
    SWO%UNIT_PREV = SWO%UNIT
    SWO%DIST_PREV = SWO%DIST
    SWO%PROJ_PREV = SWO%PROJ
    !
    !
!###############################################################################################
!###############################################################################################
!###############################################################################################
!###############################################################################################
!###############################################################################################
!###############################################################################################
!!!
!!!
!!! ! TODO --
!!! ! Uncomment after updating SWOPS1PSV in SWOPS_GLOBAL.f
!!! ! Save pointers ...
!!! ! CALL SWOPS1PSV(IGRID)
  END SUBROUTINE
  !
  SUBROUTINE SETUP_NEXT_TIME_STEP(SWO,WBS,KPER,KSTP,SEG)
!-----VERSION X 2014.06.30 SWOPS1AD
!   ******************************************************************
!   OVERVIEW:
!   ADVANCE FOR TIMESTEP ...
!   Handles updates and calculations for timestep that do NOT change
!   during iterations.
!
!   NOTES:
!   - update TABFILE vars for timestep
!   - others to come...
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    USE TABLEFILE_INTERFACE
    IMPLICIT NONE
!   ------------------------------------------------------------------
!      ARGUMENTS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),                INTENT(INOUT):: SWO
    CLASS(WBS_DATA),                INTENT(IN   ):: WBS
    INTEGER,                        INTENT(IN   ):: KPER, KSTP
    REAL, DIMENSION(:,:),CONTIGUOUS,INTENT(INOUT):: SEG
    !DOUBLE PRECISION,INTENT(IN   ):: DELT
    !DOUBLE PRECISION,INTENT(IN   ):: REALTIM
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
!   ------------------------------------------------------------------
    INTEGER:: I,J,K,N,IRES,JRES,IPROJ,IDIST,IUNIT,IFARM,IAUX,ISEG,IRCH,IDVS,ISPT,IBRC,POOLFLG,NBRANCH, JSEG
    INTEGER:: NPROJ, NDIST, NUNIT, NFARM, ITMUNI
    DOUBLE PRECISION:: AREA_SOS,DTMP1,DTMP2,STORAGE_SOS, ELEV
    !DOUBLE PRECISION:: SIMTIM0, SIMTIM
    CHARACTER(:), ALLOCATABLE:: ERROR
    INTEGER:: YR_START, YR_STOP1, YR_STOP2
    INTEGER:: SP1,SP2,TS1, TS2
    LOGICAL:: HAS_DATE_SP
!
!   ------------------------------------------------------------------
    !DDELT = DBLE(DELT)
    ITMUNI= SWO%ITMUNI
    NFARM = SWO%NFARM
    NPROJ = SWO%NPROJ
    NDIST = SWO%NDIST
    NUNIT = SWO%NUNIT
    !
    DO CONCURRENT(I = 1:SWO%NFARM  ); SWO%FARM(I)  %DEL_TOL_CNT = Z
    END DO
    !
    DO CONCURRENT(I = 1:SWO%NAUXDEM); SWO%AUXDEM(I)%DEL_TOL_CNT = Z
    END DO
    !
    HAS_DATE_SP  = DATE_SP(1)%TS(0)%IS_SET()
    !
    IF(KSTP==ONE) THEN
        IF( HAS_DATE_SP) THEN
            I = NSTP(KPER)
            SWO%SP_START = SWO%SP_STOP
            SWO%SP_STOP  = DATE_SP(KPER)%TS(I)%DYEAR
        ELSE

               SWO%SP_START = SWO%SP_STOP
               SWO%SP_STOP  = REALTIM_PER
        END IF
    END IF
    !
    SWO%TS_START    = SWO%TS_STOP
    SWO%TS_STOP     = REALTIM
    !
    SWO%FRSTART     = SWO%FRSTOP
    SWO%FRSTOP      = YEAR_FRACTION(REALTIM)
    !
    SWO%IS_LEAP_START = SWO%IS_LEAP_END
    SWO%IS_LEAP_END   = ISLEAPYEAR(INT(REALTIM))
    !
    !
    SWO%DELT = SPTIM(KPER)%DT(KSTP)
    !
    IF(SWO%DELT < NEARZERO_30) THEN
        SWO%DELT_INV = DZ
    ELSE
        SWO%DELT_INV = UNO/SWO%DELT
    END IF
    !
    !SIMTIM  = SIMTIME
    !SIMTIM0 = SIMTIME - DELT
    !
    !DATE_SP(SP1)%TS(TS1-1) ! TIME STEP START
    !DATE_SP(SP1)%TS(TS1)   ! TIME STEP END
    !DATE_SP(SP2)%TS(TS2)   ! NEXT TIME STEP END
    SP1 = KPER
    TS1 = KSTP
    TS2 = ONE
    IF( KSTP+ONE > SIZE(SPTIM(KPER)%DT) ) THEN
        IF(KPER<NPER) THEN
            SWO%DELT2 = SPTIM(KPER+1)%DT(1)
            SP2 = KPER+1
        ELSE
            SWO%DELT2 = D100
            SP2 = 1
        END IF
    ELSE
            SP2 = KPER
            TS2 = KSTP+1
            SWO%DELT2 = SPTIM(KPER)%DT(KSTP+1)
    END IF
    !
    ! ****************************************************************
    ! Compute time values ... start/end of step
    ! Determine if timestep contains start/end of water year ...
    ! ****************************************************************
    !
    DO CONCURRENT(I = ONE:SWO%NPROJ);      SWO%PROJ(I)%AllocStart = 0
    END DO
    IF(SWO%DELT2.GE.1D99) THEN
        DO CONCURRENT(I = ONE:SWO%NPROJ);  SWO%PROJ(I)%AllocClose = 1
        END DO
    ELSE
        DO CONCURRENT(I = ONE:SWO%NPROJ);  SWO%PROJ(I)%AllocClose = 0
        END DO
    END IF
    !
    IF(SWO%DELT2.GE.1D99) THEN
        SWO%TS_STOP_NEXT = SWO%DELT2
    ELSEIF  (HAS_DATE_SP) THEN
        SWO%TS_STOP_NEXT = DATE_SP(SP2)%TS(TS2)%DYEAR
    ELSE
        SWO%TS_STOP_NEXT= SWO%TS_STOP
        CALL DECIMAL_YEAR(SWO%TS_STOP_NEXT,SWO%DELT2,ITMUNI,FALSE)
    END IF
    !
    IF(SWO%DELT2.GE.1D99) THEN
        SWO%FRSTOP_NEXT      = 0.0
        SWO%IS_LEAP_END_NEXT = FALSE
    ELSE
        SWO%FRSTOP_NEXT      = YEAR_FRACTION(SWO%TS_STOP_NEXT)
        SWO%IS_LEAP_END_NEXT = ISLEAPYEAR(INT(SWO%TS_STOP_NEXT))
    END IF
    !
    !DATE_SP(SP1)%TS(TS1-1) ! TIME STEP START
    !DATE_SP(SP1)%TS(TS1)   ! TIME STEP END
    !DATE_SP(SP2)%TS(TS2)   ! NEXT TIME STEP END
    DO I = ONE,SWO%NPROJ
       IF( SWO%PROJ(I)%AllocDate%IS_SET() ) THEN
         IF(SWO%DELT2.GE.1D99) THEN                                               !MODEL HAS ENDED
                                          SWO%PROJ(I)%AllocClose = 1
                                          !
         ELSEIF( HAS_DATE_SP) THEN
             !
             ASSOCIATE (AllocDate  => SWO%PROJ(I)%AllocDate,   AllocStart=> SWO%PROJ(I)%AllocStart, AllocClose    => SWO%PROJ(I)%AllocClose, &
                        DATE_START => DATE_SP(SP1)%TS(TS1-1),  DATE_END  => DATE_SP(SP1)%TS(TS1),   DATE_NEXT_END => DATE_SP(SP2)%TS(TS2))
                !
                IF( DATE_START%CONTAIN_MD(AllocDate, DATE_END) ) THEN         !TIME STEP CONTAINS ALLOCATION DATE
                                                                      AllocStart = 1
                ELSEIF( DATE_END%CONTAIN_MD(AllocDate, DATE_NEXT_END) ) THEN  !NEXT TIME STEP CONTAINS ALLOCATION DATE
                                                                      AllocClose = 1
                END IF
                !
                IF(AllocStart == ONE) THEN
                    SWO%PROJ(I)%AllocDate_At_TS = DATE_START
                    !
                    SWO%PROJ(I)%AllocDateFrac_At_TS = SWO%PROJ(I)%AllocDate_At_TS%DYEAR_FRACTION()
                    !
                END IF
                !
             END ASSOCIATE
         ELSE
             ASSOCIATE(FRAC => SWO%PROJ(I)%AllocDateFrac, AllocStart => SWO%PROJ(I)%AllocStart, AllocClose => SWO%PROJ(I)%AllocClose, &
                       TSTART => SWO%TS_START,            TSTOP => SWO%TS_STOP,                 TSTOP2 => SWO%TS_STOP_NEXT,           &
                       FSTART => SWO%FRSTART,             FSTOP => SWO%FRSTOP,                  FSTOP2 => SWO%FRSTOP_NEXT             )
                !
                YR_START = INT(TSTART)
                YR_STOP1 = INT(TSTOP)
                YR_STOP2 = INT(TSTOP2)
                !
                IF(YR_STOP1 - YR_START > ONE) THEN
                    AllocStart = 1
                ELSEIF(YR_START == YR_STOP1 .AND. FSTART <= FRAC .AND. FRAC < FSTOP) THEN
                    AllocStart = 1
                ELSEIF( FSTART <= FRAC .OR. FRAC < FSTOP) THEN
                    AllocStart = 1
                !------------------------------------------------
                ELSEIF(YR_STOP2 - YR_STOP1 > ONE) THEN
                    AllocClose = 1
                ELSEIF(YR_STOP1 == YR_STOP2 .AND. FSTOP <= FRAC .AND. FRAC < FSTOP2) THEN
                    AllocClose = 1
                ELSEIF( FSTOP <= FRAC .OR. FRAC < FSTOP2) THEN
                    AllocClose = 1
                END IF
                !
                IF(AllocStart == ONE)  SWO%PROJ(I)%AllocDateFrac_At_TS = YEAR_FRACTION(TSTART)
                !
             END ASSOCIATE
         END IF
       END IF
    END DO
    !
    !
    ! SET UP MINIM CONSTRAINT VOLUMES ------------------------------------------------------
    !
    DO CONCURRENT(I = ONE:SWO%NPROJ);  SWO%MIN_PROJ_ORDER(I) = SWO%MIN_PROJ_ORDER_RAT(I)*SWO%DELT
    END DO
    !
    N = SWO%NFARM+SWO%NAUXDEM
    DO CONCURRENT(I = ONE:N);  SWO%MIN_DELORDER(I) = SWO%MIN_DELORDER_RAT(I)*SWO%DELT
    END DO

    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1

    ! ****************************************************************
    ! Compute reservoir area at start of step
    ! (compute area corresponding to STORAGE_PREV)
    ! ****************************************************************
    !
    DO IPROJ= 1, NPROJ
    DO IRES = 1, SWO%NRES_BAL(IPROJ)
    IF ( SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE ) THEN      ! Later periods/steps, ensure that storage at start of step (STORAGE_PREV) is equal to storage at end of previous step (STORAGE)
        !
        IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE < DZ) SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE = DZ
        !
        SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE ! Storage @ start of current step... = Storage @ end of previous step...(from last CNVG)
        !
    END IF
    END DO
    END DO
    !
    ERROR = NL
    DO IPROJ = 1,NPROJ
      DO IRES = 1,SWO%NRES_BAL(IPROJ)
        IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
           !
           STORAGE_SOS = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV
           POOLFLG     = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_POOLFLG
           !
           IF (POOLFLG.EQ.1) THEN
             ! loop over split reservoirs ...
             AREA_SOS    = DZ
             DO JRES = 1,SWO%NRES_SPT(IPROJ)
               DTMP1 = DZ; DTMP2 = DZ
               CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,STORAGE_SOS,DTMP1,SWO%IS_LEAP_END)
               CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2)
               AREA_SOS = AREA_SOS + DTMP2
               !
               SWO%RESDAT(IPROJ)%RESSPLIT(JRES)%ELEV_PREV = SWO%RESDAT(IPROJ)%RESSPLIT(JRES)%ELEV
               !
               CALL ACAP_STOR2ELEV(SWO,IPROJ,JRES,DTMP1,ELEV)
               SWO%RESDAT(IPROJ)%RESSPLIT(JRES)%ELEV = ELEV
               !
               IF(SWO%RESDAT(IPROJ)%RESSPLIT(JRES)%MAIN_RES) THEN
                   SWO%RESDAT(IPROJ)%RESBAL(IRES)%ELEV_PREV = SWO%RESDAT(IPROJ)%RESBAL(IRES)%ELEV
                   SWO%RESDAT(IPROJ)%RESBAL(IRES)%ELEV      = ELEV
               END IF
               !
             END DO !(JRES)
           ELSE
             ! compute area for mass balance reservoir
             CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE_SOS,AREA_SOS)
             CALL ACAP_STOR2ELEV(SWO,IPROJ,IRES,STORAGE_SOS,ELEV)
             !
             SWO%RESDAT(IPROJ)%RESBAL(IRES)%ELEV_PREV = SWO%RESDAT(IPROJ)%RESBAL(IRES)%ELEV
             SWO%RESDAT(IPROJ)%RESBAL(IRES)%ELEV      = ELEV
             !
           END IF !(POOLFLG.EQ.1)
           SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_PREV = AREA_SOS
           !
        END IF
      END DO !(IRES)
    END DO !(IPROJ)
    !
    IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
    !
    ASSOCIATE(TS_START => SWO%TS_START, TS_STOP => SWO%TS_STOP)
       !
       !
       ! ****************************************************************
       ! Update TABFILES @ AUX DEMAND
       ! ****************************************************************
       !
       !DO I = 1,SWO%NAUXDEM
       !    IF(SWO%AUXDEM_TSF_SIMTIM) THEN
       !        CALL SWO%AUXDEM_TSF (I)%GET(SIMTIM,  SWO%AUXDEM(I)%DEMAND, SIMTIM0)
       !    ELSE
       !        CALL SWO%AUXDEM_TSF (I)%GET(TS_STOP, SWO%AUXDEM(I)%DEMAND, TS_START)
       !    END IF
       !    SWO%AUXDEM(I)%DEMAND = SWO%AUXDEM(I)%DEMAND * SWO%DELT
       !    !
       !    IF(SWO%AUXAREA_TSF_SIMTIM) THEN
       !      CALL SWO%AUXAREA_TSF(I)%GET(SIMTIM,  SWO%AUXDEM(I)%AREA, SIMTIM0  )
       !    ELSE
       !      CALL SWO%AUXAREA_TSF(I)%GET(TS_STOP, SWO%AUXDEM(I)%AREA, TS_START  )
       !    END IF
       !END DO
       DO I = 1,SWO%NAUXDEM
           !
           CALL SWO%AUXDEM_TSF (I)%GET(TS_STOP, SWO%AUXDEM(I)%DEMAND, TS_START)
           SWO%AUXDEM(I)%DEMAND = SWO%AUXDEM(I)%DEMAND * SWO%DELT
           !
           CALL SWO%AUXAREA_TSF(I)%GET(TS_STOP, SWO%AUXDEM(I)%AREA, TS_START  )
       END DO
       !
       ! ****************************************************************
       ! Update TABFILES @ Reservoirs
       ! (INFLOW, PRCP, EVAP, STORAGE_SPEC_MIN, RELEASE_SPEC)
       ! ****************************************************************

       ! NOTE --
       ! Updates to mass balance inputs carried out at timestep, not stress period!
       !
       I = Z
       DO IPROJ = 1,NPROJ
           DO IRES = 1,SWO%NRES_BAL(IPROJ)
               I = I + ONE
               IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
                  !
                  CALL SWO%RES_INFLOW(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW, TS_START)
                  !
                  CALL SWO%RES_PRECIP(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP, TS_START)
                  !
                  CALL SWO%RES_EVAP  (I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP, TS_START)
                  !
                  CALL SWO%RES_RELEASE_SPEC(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC, TS_START)
                  !
                  CALL SWO%RES_STORAGE_MIN(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN, TS_START)
                  !
                  CALL SWO%RES_RELEASE_MIN(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_MIN_INPUT, TS_START)
                  !
                  IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN < SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_DPL) SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_DPL
                  !
                  ! SWO OPERATORS WITH VOLUMES SO CONVERT RATES
                  SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW       = SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW       * SWO%DELT
                  SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP         * SWO%DELT
                  SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP         * SWO%DELT
                  SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC * SWO%DELT
                  !
                  SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_MIN_INPUT = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_MIN_INPUT * SWO%DELT
               END IF
           END DO
       END DO
       !DO IPROJ = 1,NPROJ
       !    DO IRES = 1,SWO%NRES_BAL(IPROJ)
       !        I = I + ONE
       !        IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
       !           !
       !           IF(SWO%RES_INFLOW_SIMTIM      ) THEN
       !               CALL SWO%RES_INFLOW(I)%GET(SIMTIM,  SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW, SIMTIM0)
       !           ELSE
       !               CALL SWO%RES_INFLOW(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW, TS_START)
       !           END IF
       !           !
       !           IF(SWO%RES_PRECIP_SIMTIM      ) THEN
       !               CALL SWO%RES_PRECIP(I)%GET(SIMTIM,  SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP, SIMTIM0)
       !           ELSE
       !               CALL SWO%RES_PRECIP(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP, TS_START)
       !           END IF
       !           !
       !           IF(SWO%RES_EVAP_SIMTIM        ) THEN
       !               CALL SWO%RES_EVAP  (I)%GET(SIMTIM,  SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP, SIMTIM0)
       !           ELSE
       !               CALL SWO%RES_EVAP  (I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP, TS_START)
       !           END IF
       !           !
       !           IF(SWO%RES_RELEASE_SPEC_SIMTIM) THEN
       !               CALL SWO%RES_RELEASE_SPEC(I)%GET(SIMTIM,  SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC, SIMTIM0)
       !           ELSE
       !               CALL SWO%RES_RELEASE_SPEC(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC, TS_START)
       !           END IF
       !           !
       !           IF(SWO%RES_STORAGE_SPEC_MIN_SIMTIM) THEN
       !               CALL SWO%RES_STORAGE_SPEC_MIN(I)%GET(SIMTIM,  SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN, SIMTIM0)
       !           ELSE
       !               CALL SWO%RES_STORAGE_SPEC_MIN(I)%GET(TS_STOP, SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN, TS_START)
       !           END IF
       !           !
       !           ! SWO OPERATORS WITH VOLUMES SO CONVERT RATES
       !           SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW       = SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW       * SWO%DELT
       !           SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP         * SWO%DELT
       !           SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP         * SWO%DELT
       !           SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC * SWO%DELT
       !        END IF
       !    END DO
       !END DO
    END ASSOCIATE
    !
    ! CALCULATE NPROJ AREA/DEAD POOL AREA IF IT IS THE ONLY STORAGE IN RESERVOIR
    !
    DO IPROJ = 1,NPROJ
        IF (SWO%IRESFL(IPROJ) > Z) THEN
            DO IRES = 1,SWO%NRES_BAL(IPROJ)
                IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
                  !
                  CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN, SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL)
                  !
                  CALL ACAP_STOR2ELEV(SWO,IPROJ,IRES,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN,ELEV)
                  SWO%RESDAT(IPROJ)%RESBAL(IRES)%STOR_SPEC_MIN_ELEV = ELEV
                END IF
            END DO
        ELSEIF(SWO%IRESFL(IPROJ) < Z) THEN
            IRES = 1
            IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
               !
               SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL = DZ
               DO JRES = 1,SWO%NRES_SPT(IPROJ)
                 ! Area @ non-project storage only
                 DTMP1 = DZ; DTMP2 = DZ
                 CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN,DTMP1,SWO%IS_LEAP_END)  ! DTMP1 = SPLIT_STORAGE
                 CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2)                                                                        ! DTMP2 = SPLIT_AREA
                 !
                 SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL = SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL + DTMP2
                 !
                 IF(SWO%RESDAT(IPROJ)%RESSPLIT(JRES)%MAIN_RES) THEN
                     CALL ACAP_STOR2ELEV(SWO,IPROJ,JRES,DTMP1,ELEV)
                     SWO%RESDAT(IPROJ)%RESBAL(IRES)%STOR_SPEC_MIN_ELEV = ELEV
                 END IF
               END DO
            END IF
        END IF
    END DO
    !
    IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//ERROR)


!    I     = -1
!    J     = -1
!    IPROJ = -1
!    IRES  = -1
!    ISEG  = -1
!    IRCH  = -1
!    IDVS  = -1
!    ISPT  = -1
!    IBRC  = -1
!    IRES  = -1
!    JRES  = -1
!    IPROJ = -1
!    IDIST = -1
!    IUNIT = -1
!    IFARM = -1
!    IAUX  = -1
!
!    DO IPROJ = 1,NPROJ
!
!        ! IRESFL=0 --> No reservoirs on project
!        IF (SWO%IRESFL(IPROJ).EQ.0) THEN
!
!            CONTINUE
!
!        ! IRESFL>0 --> project reservoirs treated explicitly...
!        ELSE IF (SWO%IRESFL(IPROJ).GT.0) THEN
!
!          IF (.NOT.ALLOCATED(TMPLST)) THEN
!            ALLOCATE( TMPLST(1) ); TMPLST = ONE
!          END IF
!
!          IF (.NOT.ALLOCATED(TMPDAT)) THEN
!            ALLOCATE(TMPDAT(1)); TMPDAT= -99.D0
!          END IF
!
!          DO IRES = 1,SWO%NRES_BAL(IPROJ)
!
!              WRITE(SWO%IOUT,*) ""
!              WRITE(SWO%IOUT,*) "SWOPS -- Updating TABFILE data"
!              WRITE(SWO%IOUT,*) "Project:  ", IPROJ
!              WRITE(SWO%IOUT,*) "Reservoir:", IRES
!
!            ! INFLOW
!            CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_INFLOW,"SWOPS_INFLOW",KSTP, TMPDAT )
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW = TMPDAT(1) * SWO%DELT        ! convert from step-avg ft3/d to step-total ft3
!
!            ! PRCP
!            CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_PRCP,"SWOPS_PRCP", KSTP, TMPDAT )
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP   = TMPDAT(1) * SWO%DELT        ! convert from step-avg ft/day to step-total ft
!
!            ! EVAP
!            CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_EVAP,"SWOPS_EVAP", KSTP, TMPDAT )
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP   = TMPDAT(1) * SWO%DELT        ! convert from step-avg ft/day to step-total ft
!
!            ! RELEASE_SPEC
!            CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)% TABIDX_RELEASE_SPEC, "SWOPS_RELEASE_SPEC",KSTP,TMPDAT )
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC =TMPDAT(1) * SWO%DELT   ! convert from step-avg ft3/day to step-total ft3
!
!            ! STORAGE_NPRJ
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ_PREV = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ
!            CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_STORAGE_NPRJ, "SWOPS_STORAGE_NPRJ",KSTP,TMPDAT )
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ =TMPDAT(1)          ! no conversion -- input as ft3, use as ft3
!            !
!            ! CALCULATE NPROJ AREA IF IT IS THE ONLY STORAGE IN RESERVOIR
!            !
!            CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ, SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL, ERROR)
!            !
!            CALL ACAP_STOR2ELEV(SWO,IPROJ,IRES,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ,ELEV,ERROR)
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%MIN_ELEV = ELEV
!            !
!!            ! IMF DEBUG
!!            WRITE(IOUT,*) " "
!!            WRITE(IOUT,*) " "
!!            WRITE(IOUT,*) "--------------------------------------------"
!!            WRITE(IOUT,*) "SWOPS: Reservoir Inputs Updated (SWOPS1AD):"
!!            WRITE(IOUT,*) "TSTART =", TSTART
!!            WRITE(IOUT,*) "TSTOP  =", TSTOP
!!            WRITE(IOUT,*) "KPER   =", KPER
!!            WRITE(IOUT,*) "KSTP   =", KSTP
!!            WRITE(IOUT,*) "INFLOW =",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%INFLOW / SWO%DELT
!!            WRITE(IOUT,*) "        ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%INFLOW
!!            WRITE(IOUT,*) "PRCP   =",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%PRCP / SWO%DELT
!!            WRITE(IOUT,*) "        ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%PRCP
!!            WRITE(IOUT,*) "EVAP   =",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%EVAP / SWO%DELT
!!            WRITE(IOUT,*) "        ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%EVAP
!!            WRITE(IOUT,*) "R_NPRJ =",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_SPEC / SWO%DELT
!!            WRITE(IOUT,*) "        ",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_SPEC
!!            WRITE(IOUT,*) "S_NPRJ =",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_NPRJ / SWO%DELT
!!            WRITE(IOUT,*) "        ",
!!     +                    SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_NPRJ
!!            ! END IMF DEBUG
!
!          END DO !(NRES=1,NRES_BAL(NP))
!
!        ! IRESFL<0 --> project reservoirs treated as lumped,
!        !              only one mass balance calculation at lumped storage
!        ELSE ! ( IRESFL(NP).LT.0 )
!
!          IRES = 1
!
!          IF (.NOT.ALLOCATED(TMPLST)) THEN
!            ALLOCATE( TMPLST(1) ); TMPLST = (/1/)
!          END IF
!
!          IF (.NOT.ALLOCATED(TMPDAT)) THEN
!            ALLOCATE(TMPDAT(1)); TMPDAT= (/-99.D0/)
!          END IF
!
!!          ! Print status
!!            WRITE(IOUT,*) ""
!!            WRITE(IOUT,*) "SWOPS -- Updating TABFILE data"
!!            WRITE(IOUT,*) "Project:  ", IPROJ
!!            WRITE(IOUT,*) "Reservoir:", IRES
!
!          ! INFLOW
!          CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_INFLOW,"SWOPS_INFLOW",KSTP, TMPDAT )
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW = TMPDAT(1) * SWO%DELT          ! convert from step-avg ft3/d to step-total ft3
!
!          ! PRCP
!          CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_PRCP,"SWOPS_PRCP",KSTP, TMPDAT )
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP   = TMPDAT(1) * SWO%DELT          ! convert from step-avg ft/day to step-total ft
!
!          ! EVAP
!          CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_EVAP,"SWOPS_EVAP", KSTP, TMPDAT )
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP   = TMPDAT(1) * SWO%DELT          ! convert from step-avg ft/day to step-total ft
!
!          ! RELEASE_SPEC
!          CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_RELEASE_SPEC, "SWOPS_RELEASE_SPEC",KSTP,TMPDAT )
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC =TMPDAT(1) * SWO%DELT     ! convert from step-avg ft3/day to step-total ft3
!
!          ! STORAGE_NPRJ
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ_PREV = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ
!          CALL TABFILEUPDATE(SWO%RESDAT(IPROJ)%RESBAL(IRES)%TABIDX_STORAGE_NPRJ, "SWOPS_STORAGE_NPRJ",KSTP,TMPDAT )
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ =TMPDAT(1)            ! no conversion -- input as ft3, use as ft3
!          !
!          IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ < SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_DPL) SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_DPL
!          !
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL = DZ
!          DO JRES = 1,SWO%NRES_SPT(IPROJ)
!            ! Area @ non-project storage only
!            DTMP1 = DZ; DTMP2 = DZ
!            CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ,DTMP1,SWO%IS_LEAP_END)  ! DTMP1 = SPLIT_STORAGE
!            CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)                                                             ! DTMP2 = SPLIT_AREA
!            !
!            SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL = SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL + DTMP2
!            !
!            IF(SWO%RESDAT(IPROJ)%RESSPLIT(JRES)%MAIN_RES) THEN
!                CALL ACAP_STOR2ELEV(SWO,IPROJ,JRES,DTMP1,ELEV,ERROR)
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%MIN_ELEV = ELEV
!            END IF
!          END DO !(JRES)
!          !
!!          ! IMF DEBUG
!!          WRITE(IOUT,*) " "
!!          WRITE(IOUT,*) " "
!!          WRITE(IOUT,*) "--------------------------------------------"
!!          WRITE(IOUT,*) "SWOPS: Reservoir Inputs Updated (SWOPS1AD):"
!!          WRITE(IOUT,*) "TSTART =", TSTART
!!          WRITE(IOUT,*) "TSTOP  =", TSTOP
!!          WRITE(IOUT,*) "KPER   =", KPER
!!          WRITE(IOUT,*) "KSTP   =", KSTP
!!          WRITE(IOUT,*) "INFLOW =",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%INFLOW / SWO%DELT
!!          WRITE(IOUT,*) "        ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%INFLOW
!!          WRITE(IOUT,*) "PRCP   =",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%PRCP / SWO%DELT
!!          WRITE(IOUT,*) "        ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%PRCP
!!          WRITE(IOUT,*) "EVAP   =",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%EVAP / SWO%DELT
!!          WRITE(IOUT,*) "        ", SWO%RESDAT(IPROJ)%RESBAL(NRES)%EVAP
!!          WRITE(IOUT,*) "R_NPRJ =",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_SPEC / SWO%DELT
!!          WRITE(IOUT,*) "        ",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%RELEASE_SPEC
!!          WRITE(IOUT,*) "S_NPRJ =",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_NPRJ / SWO%DELT
!!          WRITE(IOUT,*) "        ",
!!     +                  SWO%RESDAT(IPROJ)%RESBAL(NRES)%STORAGE_NPRJ
!!          ! END IMF DEBUG
!
!        END IF !(IRESFL /.EQ./.GT./.LT/ 0)
!
!      END DO !(IPROJ = 1,NPROJ)
!      !
!      IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//ERROR)
!
!
!      ! ****************************************************************
!      ! Update TABFILES @ Auxiliary + External Demands
!      ! (AUXDMND%AUX_DEMAND, AUXDMND%AREA_IRR, EXTDMND%EXT_DEMAND)
!      ! ****************************************************************
!    I     = -1
!    J     = -1
!    IPROJ = -1
!    IRES  = -1
!    ISEG  = -1
!    IRCH  = -1
!    IDVS  = -1
!    ISPT  = -1
!    IBRC  = -1
!    IRES  = -1
!    JRES  = -1
!    IPROJ = -1
!    IDIST = -1
!    IUNIT = -1
!    IFARM = -1
!    IAUX  = -1
!
!      DO IAUX = 1,SWO%NAUXDEM
!        CALL TABFILEUPDATE(SWO%AUXDEM(IAUX)%TABIDX_AREA, "SWOPS_AUXAREA",KSTP,TMPDAT )
!        SWO%AUXDEM(IAUX)%AREA   = TMPDAT(1)
!        CALL TABFILEUPDATE(SWO%AUXDEM(IAUX)%TABIDX_DEMAND, "SWOPS_AUXDEMAND",KSTP,TMPDAT )
!        SWO%AUXDEM(IAUX)%DEMAND = TMPDAT(1) * SWO%DELT
!
!      END DO !(IAUX)
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    !
    ! ****************************************************************
    ! COMPUTE AREAS @ FARMS, UNITS, DISTRICTS, PROJECTS ...
    ! ****************************************************************
    !
    ! Area @ Farms
    DO IFARM = 1,NFARM
      SWO%FARM(IFARM)%AreaTot = WBS%FID(IFARM)%AREA
      SWO%FARM(IFARM)%AreaIrr = WBS%IRR_AREA(IFARM)
      !
      IF(SWO%FARM(IFARM)%AreaTot < NEARZERO_5) SWO%FARM(IFARM)%AreaTot = DZ
      IF(SWO%FARM(IFARM)%AreaIrr < NEARZERO_5) SWO%FARM(IFARM)%AreaIrr = DZ
      !
      IF(SWO%FARM(IFARM)%DelSeg.NE.Z) THEN
          SWO%FARM(IFARM)%AreaSwIrr = WBS%IRR_AREA(IFARM)
          IF(SWO%FARM(IFARM)%AreaSwIrr < NEARZERO_5) SWO%FARM(IFARM)%AreaIrr = DZ
      ELSE
          SWO%FARM(IFARM)%AreaSwIrr = DZ
      END IF

!      DO I = 1,NCOL
!        DO J = 1,NROW
!          IF (FarmID(I,J).EQ.IFARM) THEN
!            FARM(IFARM)%AreaTot = FARM(IFARM)%AreaTot  + (DELR(I)*DELC(J))
!          END IF !(FarmID)
!          IF ((FarmID(I,J).EQ.IFARM).AND.(CropID(I,J).GT.0)) THEN
!            FARM(IFARM)%AreaIrr = FARM(IFARM)%AreaIrr
!   +                              + (DELR(I)*DELC(J))
!          END IF !((FarmID).AND.(CropID))
!        END DO !(J)
!      END DO !(I)
    END DO !(IFARM)

    ! Area @ Units (sum over farms/auxs in each unit)
    ! NOTE -- updated to exclude GW-only farms!
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    !
    DO IUNIT = 1,NUNIT
          SWO%UNIT(IUNIT)%AreaTot = DZ
          SWO%UNIT(IUNIT)%AreaIrr = DZ
          DO I=ONE, WBS%UNIT(IUNIT)%NFARM
                IFARM = WBS%UNIT(IUNIT)%FARM(I)
                IF(SWO%FARM(IFARM)%DelSeg.NE.Z) THEN                                     ! SCOTT WHAT ABOUT FARMS WITH OUT SFR SEG?
                   SWO%UNIT(IUNIT)%AreaTot = SWO%UNIT(IUNIT)%AreaTot + SWO%FARM(IFARM)%AreaTot
                   SWO%UNIT(IUNIT)%AreaIrr = SWO%UNIT(IUNIT)%AreaIrr + SWO%FARM(IFARM)%AreaSwIrr
                END IF
          END DO
          DO I=ONE, WBS%UNIT(IUNIT)%NAUX
                IAUX = WBS%UNIT(IUNIT)%AUX(I)
                IF(SWO%AUXDEM(IAUX)%AuxSeg.NE.Z) THEN
                   SWO%UNIT(IUNIT)%AreaTot = SWO%UNIT(IUNIT)%AreaTot + SWO%AUXDEM(IAUX)%AREA
                   SWO%UNIT(IUNIT)%AreaIrr = SWO%UNIT(IUNIT)%AreaIrr + SWO%AUXDEM(IAUX)%AREA
                END IF
          END DO
          !
          IF(SWO%UNIT(IUNIT)%AreaTot < NEARZERO_5) SWO%UNIT(IUNIT)%AreaTot = DZ
          IF(SWO%UNIT(IUNIT)%AreaIrr < NEARZERO_5) SWO%UNIT(IUNIT)%AreaIrr = DZ
    END DO
!    DO IUNIT = 1,NUNIT
!      UNIT(IUNIT)%AreaTot = DZ
!      UNIT(IUNIT)%AreaIrr = DZ
!      ! Farm area ...
!      DO IFARM = 1,NFARM
!        IF ((FARM(IFARM)%UnitID.EQ.IUNIT)
!   +        .AND.
!   +        (FARM(IFARM)%DelSeg.NE.0)) THEN
!          UNIT(IUNIT)%AreaTot = UNIT(IUNIT)%AreaTot
!   +                            + FARM(IFARM)%AreaTot
!          UNIT(IUNIT)%AreaIrr = UNIT(IUNIT)%AreaIrr
!   +                            + FARM(IFARM)%AreaIrr
!        END IF !(FARM%UnitID.EQ.IUNIT / FARM%DelSeg.NE.0)
!      END DO !(IFARM)
!      ! Aux area ...
!      DO IAUX = 1,NAUXDEM
!        IF ((SWO%AUXDEM(IAUX)%UnitID.EQ.IUNIT) .AND. (SWO%AUXDEM(IAUX)%AuxSeg.NE.0)) THEN
!          UNIT(IUNIT)%AreaIrr = UNIT(IUNIT)%AreaIrr + SWO%AUXDEM(IAUX)%AREA
!          UNIT(IUNIT)%AreaTot = UNIT(IUNIT)%AreaTot + SWO%AUXDEM(IAUX)%AREA
!        END IF !(AUX%UnitID.EQ.IUNIT / AUX%AuxSeg.NE.0)
!      END DO !(IAUX)
!    END DO !(IUNIT)
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1

    ! Area @ Districts (sum over units in each district)
    DO IDIST = 1,NDIST
      SWO%DIST(IDIST)%AreaTot = DZ
      SWO%DIST(IDIST)%AreaIrr = DZ
      DO I = 1, WBS%DIST(IDIST)%NUNIT
          IUNIT = WBS%DIST(IDIST)%UNIT(I)
          SWO%DIST(IDIST)%AreaTot = SWO%DIST(IDIST)%AreaTot + SWO%UNIT(IUNIT)%AreaTot
          SWO%DIST(IDIST)%AreaIrr = SWO%DIST(IDIST)%AreaIrr + SWO%UNIT(IUNIT)%AreaIrr
      END DO !(IUNIT)
      !
      IF(SWO%DIST(IDIST)%AreaTot < NEARZERO_5) SWO%DIST(IDIST)%AreaTot = DZ
      IF(SWO%DIST(IDIST)%AreaIrr < NEARZERO_5) SWO%DIST(IDIST)%AreaIrr = DZ
    END DO !(IDIST)
!    DO IDIST = 1,NDIST
!      DIST(IDIST)%AreaTot = DZ
!      DIST(IDIST)%AreaIrr = DZ
!      DO IUNIT = 1,NUNIT
!        IF (UNIT(IUNIT)%DistID.EQ.IDIST) THEN
!          DIST(IDIST)%AreaTot = DIST(IDIST)%AreaTot
!   +                            + UNIT(IUNIT)%AreaTot
!          DIST(IDIST)%AreaIrr = DIST(IDIST)%AreaIrr
!   +                            + UNIT(IUNIT)%AreaIrr
!        END IF !(UNIT%DistID.EQ.IDIST)
!      END DO !(IUNIT)
!    END DO !(IDIST)
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1

    ! Area @ Project (sum over districts in each project)
    DO IPROJ = 1,NPROJ
      SWO%PROJ(IPROJ)%AreaTot = DZ
      SWO%PROJ(IPROJ)%AreaIrr = DZ
      DO I = 1,WBS%PROJ(IPROJ)%NDIST
          IDIST = WBS%PROJ(IPROJ)%DIST(I)
          SWO%PROJ(IPROJ)%AreaTot = SWO%PROJ(IPROJ)%AreaTot + SWO%DIST(IDIST)%AreaTot
          SWO%PROJ(IPROJ)%AreaIrr = SWO%PROJ(IPROJ)%AreaIrr + SWO%DIST(IDIST)%AreaIrr
      END DO !(IDIST)
      !
      IF(SWO%PROJ(IPROJ)%AreaTot < NEARZERO_5) SWO%PROJ(IPROJ)%AreaTot = DZ
      IF(SWO%PROJ(IPROJ)%AreaIrr < NEARZERO_5) SWO%PROJ(IPROJ)%AreaIrr = DZ
    END DO !(IPROJ)
!    DO IPROJ = 1,NPROJ
!      SWO%PROJ(IPROJ)%AreaTot = DZ
!      SWO%PROJ(IPROJ)%AreaIrr = DZ
!      DO IDIST = 1,NDIST
!        IF (DIST(IDIST)%ProjID.EQ.IPROJ) THEN
!          SWO%PROJ(IPROJ)%AreaTot = SWO%PROJ(IPROJ)%AreaTot
!   +                            + DIST(IDIST)%AreaTot
!          SWO%PROJ(IPROJ)%AreaIrr = SWO%PROJ(IPROJ)%AreaIrr
!   +                            + DIST(IDIST)%AreaIrr
!        END IF !(DIST%ProjID.EQ.IPROJ)
!
!      END DO !(IDIST)
!    END DO !(IPROJ)
    !
    ! SET UP HOW DISTRICTS WILL SPLIT THEIR ALLOCATIONS ACROSS FARMS AND AUXDEMS
    !
    IF    (SWO%DIST_ALLOC_FRAC_TYP == Z) THEN  !Even Split
        !
        DO CONCURRENT (I=ONE:NFARM  ); SWO%FARM(I)%DIST_ALLOC_FRAC = DZ
        END DO
        DO CONCURRENT (I=ONE:SWO%NAUXDEM); SWO%AUXDEM(I)%DIST_ALLOC_FRAC = DZ
        END DO
        !
        DO IDIST = 1,NDIST
            I = WBS%DIST(IDIST)%NFARM + WBS%DIST(IDIST)%NAUX
            IF(I > Z) THEN
                DTMP1 = UNO/DBLE(I)
            ELSE
                CYCLE
            END IF
            !
            DO J =ONE, WBS%DIST(IDIST)%NFARM
               I = WBS%DIST(IDIST)%FARM(J)
               SWO%FARM(I)%DIST_ALLOC_FRAC = DTMP1
            END DO
            !
            DO J =ONE, WBS%DIST(IDIST)%NAUX
               I = WBS%DIST(IDIST)%AUX(J)
               SWO%AUXDEM(I)%DIST_ALLOC_FRAC = DTMP1
            END DO
        END DO
        !
    ELSEIF(SWO%DIST_ALLOC_FRAC_TYP == ONE) THEN  !BY IRRIGATED AREA
        !
        DO CONCURRENT (I=ONE:NFARM); SWO%FARM(I)%DIST_ALLOC_FRAC = DZ
        END DO
        DO CONCURRENT (I=ONE:SWO%NAUXDEM); SWO%AUXDEM(I)%DIST_ALLOC_FRAC = DZ
        END DO
        !
        DO IDIST = 1,NDIST
            I = WBS%DIST(IDIST)%NFARM + WBS%DIST(IDIST)%NAUX
            IF(I == Z .OR. SWO%DIST(IDIST)%AreaIrr < NEARZERO_5) CYCLE
            !
            !
            DO J =ONE, WBS%DIST(IDIST)%NFARM
               I = WBS%DIST(IDIST)%FARM(J)
               SWO%FARM(I)%DIST_ALLOC_FRAC = SWO%FARM(I)%AreaSwIrr / SWO%DIST(IDIST)%AreaIrr
            END DO
            !
            DO J =ONE, WBS%DIST(IDIST)%NAUX
               I = WBS%DIST(IDIST)%AUX(J)
               SWO%AUXDEM(I)%DIST_ALLOC_FRAC = SWO%AUXDEM(I)%AREA / SWO%DIST(IDIST)%AreaIrr
            END DO
        END DO
        !
    ELSEIF(SWO%DIST_ALLOC_FRAC_TYP==TWO) THEN ! Specified Fractions
        !
        DO IDIST = 1,NDIST
            !
            IF(WBS%DIST(IDIST)%NFARM + WBS%DIST(IDIST)%NAUX == Z) CYCLE
            !
            DTMP1 = DZ
            !
            DO J =ONE, WBS%DIST(IDIST)%NFARM
               I = WBS%DIST(IDIST)%FARM(J)
               SWO%FARM(I)%DIST_ALLOC_FRAC = SWO%DIST_ALLOC_FRAC%LIST(I)
               !
               DTMP1 = DTMP1 + SWO%FARM(I)%DIST_ALLOC_FRAC  !SUM CHECK
            END DO
            !
            DO J =ONE, WBS%DIST(IDIST)%NAUX
               I = WBS%DIST(IDIST)%AUX(J)
               SWO%AUXDEM(I)%DIST_ALLOC_FRAC = SWO%DIST_ALLOC_FRAC%LIST(I+NFARM)
               !
               DTMP1 = DTMP1 + SWO%FARM(I)%DIST_ALLOC_FRAC  !SUM CHECK
            END DO
            !
            IF(DTMP1 < SUB_ONE .OR. NEAR_ONE < DTMP1) THEN                 !Should sum to 1
                !
                DO J =ONE, WBS%DIST(IDIST)%NFARM
                   I = WBS%DIST(IDIST)%FARM(J)
                   SWO%FARM(I)%DIST_ALLOC_FRAC =     SWO%FARM(I)%DIST_ALLOC_FRAC / DTMP1
                END DO
                !
                DO J =ONE, WBS%DIST(IDIST)%NAUX
                   I = WBS%DIST(IDIST)%AUX(J)
                   SWO%AUXDEM(I)%DIST_ALLOC_FRAC = SWO%AUXDEM(I)%DIST_ALLOC_FRAC / DTMP1
                END DO
            END IF
        END DO
    END IF !IF SWO%DIST_ALLOC_FRAC_TYP == 3 then set by S-LANGUAGE


    ! ****************************************************************
    ! RESET CURRENT VALUES
    ! (reset current flow, efficiency, demand, etc., at start of timestep)
    ! ****************************************************************
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1

    ! TODO ...
    ! Is this really needed?
    ! And if so, is this being done correctly?

    ! FLOW DATA
    ! (only reset values computed/manipulated by SWOPS1FM ...
    !  update values computed outside of SWOPS1FM in SWOPS1BD routine)
    DO CONCURRENT (ISEG = ONE:SWO%NSEG); SWO%SEGDATA(ISEG)%EFFICIENCY = SWO%SEGDATA_PREV(ISEG)%EFFICIENCY
    END DO !(ISEG)

    DO CONCURRENT (IRCH = ONE:SWO%NSTRM); SWO%RCHDATA(IRCH)%EFFICIENCY = SWO%RCHDATA_PREV(IRCH)%EFFICIENCY
    END DO !(IRCH)

    ! SUPPLY / DEMAND / ACCOUNTING
    ! (set current flow/acct values to zero...)
    ! (set current conveyance/performance parameters to latest/greatest)
    ! (set current YTD values to end of previous step)
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO IPROJ = 1,NPROJ
      SWO%PROJ(IPROJ)%TFDR          = DZ
      SWO%PROJ(IPROJ)%DELORDER      = DZ
      SWO%PROJ(IPROJ)%DIVORDER      = DZ
      SWO%PROJ(IPROJ)%RELEASE       = DZ
      SWO%PROJ(IPROJ)%DIVERSION     = DZ
      !SWO%PROJ(IPROJ)%OUTFLOW       = DZ
      SWO%PROJ(IPROJ)%DELIVERY      = DZ
      SWO%PROJ(IPROJ)%BYPASS        = DZ
      SWO%PROJ(IPROJ)%CHARGE        = DZ
      SWO%PROJ(IPROJ)%CREDIT        = DZ
      SWO%PROJ(IPROJ)%DIVIN         = DZ
      SWO%PROJ(IPROJ)%SUMIN         = DZ
      SWO%PROJ(IPROJ)%SUMOUT        = DZ
      SWO%PROJ(IPROJ)%DELIVEFF      = SWO%PROJ_PREV(IPROJ)%DELIVEFF
      SWO%PROJ(IPROJ)%DIVRATIO      = SWO%PROJ_PREV(IPROJ)%DIVRATIO
      SWO%PROJ(IPROJ)%CHGRATIO      = SWO%PROJ_PREV(IPROJ)%CHGRATIO
      SWO%PROJ(IPROJ)%NETCHGRATIO   = SWO%PROJ_PREV(IPROJ)%NETCHGRATIO
      SWO%PROJ(IPROJ)%RELEASE_YTD   = SWO%PROJ_PREV(IPROJ)%RELEASE_YTD
      SWO%PROJ(IPROJ)%DIVERSION_YTD = SWO%PROJ_PREV(IPROJ)%DIVERSION_YTD
      !SWO%PROJ(IPROJ)%OUTFLOW_YTD   = SWO%PROJ_PREV(IPROJ)%OUTFLOW_YTD
      SWO%PROJ(IPROJ)%DELIVERY_YTD  = SWO%PROJ_PREV(IPROJ)%DELIVERY_YTD
      SWO%PROJ(IPROJ)%BYPASS_YTD    = SWO%PROJ_PREV(IPROJ)%BYPASS_YTD
      SWO%PROJ(IPROJ)%CHARGE_YTD    = SWO%PROJ_PREV(IPROJ)%CHARGE_YTD
      SWO%PROJ(IPROJ)%CREDIT_YTD    = SWO%PROJ_PREV(IPROJ)%CREDIT_YTD
      SWO%PROJ(IPROJ)%DELIVEFF_YTD  = SWO%PROJ_PREV(IPROJ)%DELIVEFF_YTD
      SWO%PROJ(IPROJ)%DIVRATIO_YTD  = SWO%PROJ_PREV(IPROJ)%DIVRATIO_YTD
      SWO%PROJ(IPROJ)%CHGRATIO_YTD  = SWO%PROJ_PREV(IPROJ)%CHGRATIO_YTD
      SWO%PROJ(IPROJ)%DIVIN_YTD     = SWO%PROJ_PREV(IPROJ)%DIVIN_YTD
      SWO%PROJ(IPROJ)%SUMIN_YTD     = SWO%PROJ_PREV(IPROJ)%SUMIN_YTD
      SWO%PROJ(IPROJ)%SUMOUT_YTD    = SWO%PROJ_PREV(IPROJ)%SUMOUT_YTD
      SWO%PROJ(IPROJ)%NETCHGRATIO_YTD= SWO%PROJ_PREV(IPROJ)%NETCHGRATIO_YTD
    END DO !(IPROJ)
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1

    DO IDIST = 1,NDIST
      SWO%DIST(IDIST)%TFDR          = DZ
      SWO%DIST(IDIST)%DELORDER      = DZ
      SWO%DIST(IDIST)%DIVORDER      = DZ
      SWO%DIST(IDIST)%DIVERSION     = DZ
      SWO%DIST(IDIST)%DELIVERY      = DZ
      SWO%DIST(IDIST)%BYPASS        = DZ
      SWO%DIST(IDIST)%CHARGE        = DZ
      SWO%DIST(IDIST)%CREDIT        = DZ
      SWO%DIST(IDIST)%DIVIN         = DZ
      SWO%DIST(IDIST)%SUMIN         = DZ
      SWO%DIST(IDIST)%SUMOUT        = DZ
      SWO%DIST(IDIST)%BALANCE       = SWO%DIST_PREV(IDIST)%BALANCE
      SWO%DIST(IDIST)%DELIVEFF      = SWO%DIST_PREV(IDIST)%DELIVEFF
      SWO%DIST(IDIST)%CHGRATIO      = SWO%DIST_PREV(IDIST)%CHGRATIO
      SWO%DIST(IDIST)%NETCHGRATIO   = SWO%DIST_PREV(IDIST)%NETCHGRATIO
      SWO%DIST(IDIST)%DIVERSION_YTD = SWO%DIST_PREV(IDIST)%DIVERSION_YTD
      SWO%DIST(IDIST)%DELIVERY_YTD  = SWO%DIST_PREV(IDIST)%DELIVERY_YTD
      SWO%DIST(IDIST)%BYPASS_YTD    = SWO%DIST_PREV(IDIST)%BYPASS_YTD
      SWO%DIST(IDIST)%CHARGE_YTD    = SWO%DIST_PREV(IDIST)%CHARGE_YTD
      SWO%DIST(IDIST)%CREDIT_YTD    = SWO%DIST_PREV(IDIST)%CREDIT_YTD
      SWO%DIST(IDIST)%DELIVEFF_YTD  = SWO%DIST_PREV(IDIST)%DELIVEFF_YTD
      SWO%DIST(IDIST)%CHGRATIO_YTD  = SWO%DIST_PREV(IDIST)%CHGRATIO_YTD
      SWO%DIST(IDIST)%DIVIN_YTD     = SWO%DIST_PREV(IDIST)%DIVIN_YTD
      SWO%DIST(IDIST)%SUMIN_YTD     = SWO%DIST_PREV(IDIST)%SUMIN_YTD
      SWO%DIST(IDIST)%SUMOUT_YTD    = SWO%DIST_PREV(IDIST)%SUMOUT_YTD
      SWO%DIST(IDIST)%NETCHGRATIO_YTD= SWO%DIST_PREV(IDIST)%NETCHGRATIO_YTD
    END DO !(IDIST)

    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO IUNIT = 1,NUNIT
      SWO%UNIT(IUNIT)%TFDR          = DZ
      SWO%UNIT(IUNIT)%DELORDER      = DZ
      SWO%UNIT(IUNIT)%DIVORDER      = DZ
      SWO%UNIT(IUNIT)%DIVERSION     = DZ
      SWO%UNIT(IUNIT)%DELIVERY      = DZ
      SWO%UNIT(IUNIT)%BYPASS        = DZ
      SWO%UNIT(IUNIT)%CHARGE        = DZ
      SWO%UNIT(IUNIT)%CREDIT        = DZ
      SWO%UNIT(IUNIT)%DIVIN         = DZ
      SWO%UNIT(IUNIT)%SUMIN         = DZ
      SWO%UNIT(IUNIT)%SUMOUT        = DZ
      SWO%UNIT(IUNIT)%DELIVEFF      = SWO%UNIT_PREV(IUNIT)%DELIVEFF
      SWO%UNIT(IUNIT)%CHGRATIO      = SWO%UNIT_PREV(IUNIT)%CHGRATIO
      SWO%UNIT(IUNIT)%NETCHGRATIO   = SWO%UNIT_PREV(IUNIT)%NETCHGRATIO
      SWO%UNIT(IUNIT)%DIVERSION_YTD = SWO%UNIT_PREV(IUNIT)%DIVERSION_YTD
      SWO%UNIT(IUNIT)%DELIVERY_YTD  = SWO%UNIT_PREV(IUNIT)%DELIVERY_YTD
      SWO%UNIT(IUNIT)%BYPASS_YTD    = SWO%UNIT_PREV(IUNIT)%BYPASS_YTD
      SWO%UNIT(IUNIT)%CHARGE_YTD    = SWO%UNIT_PREV(IUNIT)%CHARGE_YTD
      SWO%UNIT(IUNIT)%CREDIT_YTD    = SWO%UNIT_PREV(IUNIT)%CREDIT_YTD
      SWO%UNIT(IUNIT)%DELIVEFF_YTD  = SWO%UNIT_PREV(IUNIT)%DELIVEFF_YTD
      SWO%UNIT(IUNIT)%CHGRATIO_YTD  = SWO%UNIT_PREV(IUNIT)%CHGRATIO_YTD
      SWO%UNIT(IUNIT)%DIVIN_YTD     = SWO%UNIT_PREV(IUNIT)%DIVIN_YTD
      SWO%UNIT(IUNIT)%SUMIN_YTD     = SWO%UNIT_PREV(IUNIT)%SUMIN_YTD
      SWO%UNIT(IUNIT)%SUMOUT_YTD    = SWO%UNIT_PREV(IUNIT)%SUMOUT_YTD
      SWO%UNIT(IUNIT)%NETCHGRATIO_YTD= SWO%UNIT_PREV(IUNIT)%NETCHGRATIO_YTD
      SWO%UNIT(IUNIT)%ALLOTMENT     = SWO%UNIT_PREV(IUNIT)%ALLOTMENT
      SWO%UNIT(IUNIT)%BALANCE       = SWO%UNIT_PREV(IUNIT)%BALANCE
    END DO !(IUNIT)

    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO IFARM = 1,NFARM
      SWO%FARM(IFARM)%TFDR          = DZ
      SWO%FARM(IFARM)%ALLOTMENT     = DZ
      SWO%FARM(IFARM)%DELORDER      = DZ
      SWO%FARM(IFARM)%DELIVERY      = DZ
      SWO%FARM(IFARM)%BALANCE       = DZ !SWO%FARM_PREV(IFARM)%BALANCE
      SWO%FARM(IFARM)%DELIVERY_YTD  = SWO%FARM_PREV(IFARM)%DELIVERY_YTD
    END DO !(IFARM)

    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO IAUX = 1,SWO%NAUXDEM
      SWO%AUXDEM(IAUX)%ALLOTMENT    = DZ
      SWO%AUXDEM(IAUX)%DELORDER     = DZ
      SWO%AUXDEM(IAUX)%DELIVERY     = DZ
      SWO%AUXDEM(IAUX)%BALANCE      = SWO%AUXDEM_PREV(IAUX)%BALANCE
      SWO%AUXDEM(IAUX)%DELIVERY_YTD = SWO%AUXDEM_PREV(IAUX)%DELIVERY_YTD
    END DO !(IAUX)

    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO IDVS = 1,SWO%DIVCOUNT
      SWO%DIVSEG(IDVS)%TFDR        = DZ
      SWO%DIVSEG(IDVS)%DELORDER    = DZ
      SWO%DIVSEG(IDVS)%DIVORDER    = DZ
      SWO%DIVSEG(IDVS)%DIVERSION   = DZ
      SWO%DIVSEG(IDVS)%DELIVERY    = DZ
      SWO%DIVSEG(IDVS)%DELIVEFF    = SWO%DIVSEG_PREV(IDVS)%DELIVEFF
      SWO%DIVSEG(IDVS)%DSEEP       = DZ  !ORIGINALL ISPT
      SWO%DIVSEG(IDVS)%DFLOW_IN    = DZ
      SWO%DIVSEG(IDVS)%DFLOW_RT    = DZ
      SWO%DIVSEG(IDVS)%UP_DIVORDER = DZ
      SWO%DIVSEG(IDVS)%UP_DSEEP    = DZ
      SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DZ
      SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DZ
      SWO%DIVSEG(IDVS)%UP_DFLOW_RN = DZ
      SWO%DIVSEG(IDVS)%UP_DFLOW_ET = DZ
    END DO !(IDVS)

    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO ISPT = 1,SWO%SPTCOUNT
      SWO%SPTSEG(ISPT)%TFDR       = DZ
      SWO%SPTSEG(ISPT)%DELORDER   = DZ
      SWO%SPTSEG(ISPT)%DIVORDER   = DZ
      SWO%SPTSEG(ISPT)%DIVERSION  = DZ
      SWO%SPTSEG(ISPT)%DELIVERY   = DZ
      SWO%SPTSEG(ISPT)%DELIVEFF   = SWO%SPTSEG_PREV(ISPT)%DELIVEFF
      SWO%SPTSEG(ISPT)%DSEEP      = DZ
      SWO%SPTSEG(ISPT)%DFLOW_IN   = DZ
      SWO%SPTSEG(ISPT)%DFLOW_RT   = DZ
      DO IBRC = 1,SWO%SPTSEG(ISPT)%NBRANCH
        SWO%SPTSEG(ISPT)%BrcTFDR(IBRC)      = DZ
        SWO%SPTSEG(ISPT)%BrcDELORDER(IBRC)  = DZ
        SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC)  = DZ
        SWO%SPTSEG(ISPT)%BrcDIVERSION(IBRC) = DZ
        SWO%SPTSEG(ISPT)%BrcDELIVERY(IBRC)  = DZ
        SWO%SPTSEG(ISPT)%BrcDELIVEFF(IBRC)  = SWO%SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
        SWO%SPTSEG(ISPT)%BrcDSEEP(IBRC)     = DZ
        SWO%SPTSEG(ISPT)%BrcDFLOW_IN(IBRC)  = DZ
        SWO%SPTSEG(ISPT)%BrcDFLOW_RT(IBRC)  = DZ
      END DO !(IBRC)
    END DO !(ISPT)
    !
    ! INITIALIZE ANY REQUIRED FLOWS
    !
    IF(SWO%REQFLOW%HAS_REQ ) THEN
        !
        DO IPROJ=1, NPROJ
        DO IRES =1, SWO%NRES_BAL(IPROJ)
            SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF      = DZ
            SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF_PREV = DZ
        END DO
        END DO
        !
        SWO%REQFLOW%REL     = DZ
        SWO%REQFLOW%REL_OLD = DZ
    END IF
    !DO CONCURRENT ( IPROJ = 1:NPROJ, IRES = 1:SWO%NRES_BAL(IPROJ) )
    !    !
    !    IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE < NEAR_inf) THEN
    !        !
    !        SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_VOL = SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE * SWO%DELT
    !    ELSE
    !        SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_VOL = inf
    !    END IF
    !END DO
    !
    ! ****************************************************************
    ! COMPUTE MINIMUM ALLOWED RESERVOIR STORAGE STEP 1 (SPECIFIED OR DEAD POOL STORAGE IS FIRST MIN VALUE)
    !
    ! ****************************************************************
    !
    DO IPROJ= 1, NPROJ
    DO IRES = 1, SWO%NRES_BAL(IPROJ)
    IF ( SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE ) THEN
       SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_MIN = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN
       SWO%RESDAT(IPROJ)%RESBAL(IRES)%MIN_ELEV    = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STOR_SPEC_MIN_ELEV
    END IF
    END DO
    END DO
    !
    ! Initialize SFR related veriables. Set all diversions to zero---------------------------------------
    !
    DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT);  SWO%DIVSEG(IDVS)%DIVORDER = DZ
    END DO
    !
    DO ISPT=1, SWO%SPTCOUNT
      SWO%SPTSEG(ISPT)%DIVORDER = DZ
      DO CONCURRENT (J = 1:SWO%SPTSEG(ISPT)%NBRANCH);  SWO%SPTSEG(ISPT)%BrcDIVORDER(J) = DZ
      END DO
    END DO
    !
    !
    DO CONCURRENT   (I = ONE:NPROJ)
      DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG > Z)
          !
          SEG(2,SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG) = DZ
          !
      END DO
    END DO
    !
    ! (13) Loop over diversion segments ... zero diversion flow values
    DO IDVS=1, SWO%DIVCOUNT
    IF ( SWO%DIVSEG(IDVS)%DivSeg > Z ) THEN
      ISEG  = SWO%DIVSEG(IDVS)%DivSeg
      !
      SWO%SEGDATA(ISEG)%FLOW = DZ
      !
      SEG(2,ISEG)            = DZ
    END IF
    END DO
    !
    !
    ! (14) Loop over splits ... ZERO split values (fractional or gross)
    DO ISPT = 1,SWO%SPTCOUNT
      ISEG    = SWO%SPTSEG(ISPT)%SplitSeg
      NBRANCH = SWO%SPTSEG(ISPT)%NBRANCH
      DO IBRC = 1,NBRANCH
        JSEG  = SWO%SPTSEG(ISPT)%BrcSeg(IBRC)
        !
        SWO%SEGDATA(JSEG)%FLOW = DZ
        !
        SEG(2,JSEG)            = DZ
      END DO
    END DO
    !
    IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//'THE FOLLOWING ARE EITHER VARIABLE NAMES NOT FOUND WHEN PULLING AN OWHM PROPERTY FOR THE S-LANGUAGE'//NL//'OR SETTING AN OWHM PROPERTY WITH THE S-LANGUAGE'//NL//ERROR)
    !
  END SUBROUTINE
  !
  !########################################################################################
  !
  SUBROUTINE SET_ADJUSTIBLE_PROPERTIES(SWO, ERROR) !SET UP ANY PROPERTIES SET BY S
    CLASS(SWO_DATA),           INTENT(INOUT):: SWO
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: ERROR
    INTEGER:: I, J, K, F, N, IRES, JRES, NRES, IREQ, RES1a, RES2a
    INTEGER:: I1, I2, J1, J2
    DOUBLE PRECISION:: STOR, FRAC, ELEV_MIN, TRANS, DTMP, DTMP1, DTMP2
    !
    STOR    = DZ
    FRAC    = DZ
    ELEV_MIN= ninf
    TRANS   = DZ
    !
    ! RESET RESERVOIR STORAGE LIMITS ALLOWED STORAGE
    !
    DO CONCURRENT (I = 1:SWO%NPROJ)
    DO CONCURRENT (J = 1:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE )
       !
       SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL = SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL_INPUT
       SWO%RESDAT(I)%RESBAL(J)%STORAGE_MIN   = SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPEC_MIN
       SWO%RESDAT(I)%RESBAL(J)%MIN_ELEV      = SWO%RESDAT(I)%RESBAL(J)%STOR_SPEC_MIN_ELEV
       !
    END DO; END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    ! COMPUTE MAXIMUM RESERVOIR RELEASE VOLUME FOR CURRENT TIME STEP
    !
    DO CONCURRENT (I = 1:SWO%NPROJ)
    DO CONCURRENT (J = 1:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE )
        !
        IF(DZ < SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE .AND. SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE < D100) THEN
            !
            SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE_VOL = SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE * SWO%DELT
        ELSE
            SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE_VOL = inf
        END IF
        !
    END DO; END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CONVERT ANY S SPECIFIED MAXIMUM RELEASES TO VOLUMES
    !
    IF(SWO%HAS_MAX_RELEASE_S) THEN
       DO I=1, SWO%NPROJ
       DO J=1, SWO%NRES_BAL(I)
       IF ( SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE_S == SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE_S ) THEN
         ASSOCIATE(RESBAL => SWO%RESDAT(I)%RESBAL(J))
                     !
                     IF( RESBAL%MAX_RELEASE_S < D100 ) THEN
                          !
                          RESBAL%MAX_RELEASE = RESBAL%MAX_RELEASE_S   !Update input version
                          !
                          RESBAL%MAX_RELEASE_S_Vol = RESBAL%MAX_RELEASE_S * SWO%DELT
                          !
                          IF(RESBAL%MAX_RELEASE_S_Vol < DZ) RESBAL%MAX_RELEASE_S_Vol = DZ
                          !
                          RESBAL%MAX_RELEASE_VOL = RESBAL%MAX_RELEASE_S_Vol
                     ELSE
                         RESBAL%MAX_RELEASE       = inf
                         RESBAL%MAX_RELEASE_S     = inf
                         RESBAL%MAX_RELEASE_S_Vol = inf
                     END IF
         END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    !Check if S_ALLOTMENT is in use by farm and auxiliary
    DO I=ONE, SWO%NDIST
        K = Z
        DO J = ONE, SWO%DIST(I)%NFARM
            IF(SWO%FARM( SWO%DIST(I)%FARM(J)     )%S_ALLOTMENT .GE. DZ) K = K + ONE
        END DO
        !
        DO J=ONE, SWO%DIST(I)%NAUXDEM
            IF(SWO%AUXDEM( SWO%DIST(I)%AUXDEM(J) )%S_ALLOTMENT .GE. DZ) K = K + ONE
        END DO
        !
        IF( K.NE.Z .AND. K.NE. (SWO%DIST(I)%NFARM+SWO%DIST(I)%NAUXDEM) ) ERROR = ERROR//'FOUND FOR DISTRICT '//NUM2STR(I)//'FARM AND AUXILIARY DEMAND ALLOTMENTS SPECIFIED IN THE S LANGAUGE (e.g. FARM.fid.ALLOTMENT WAS SET >= 0.0),'//NL//'WHICH REQUIRES THAT ALL FARMS AND AUXILIARY DEMANDs ASSOCIATED WITH THE DISTRICT HAVE THEIR ALLOTMENT SPECIFIED IN S.'//NL//'THE FOLLOWING ARE A LIST OF THE FARMS ASSOCIATED WITH THE DISTRICT:'//NUM2STR(SWO%DIST(I)%FARM, 3, ', ')//NL//'THE FOLLOWING ARE A LIST OF THE AUXILIARY DEMAND IDs ASSOCIATED WITH THE DISTRICT:'//NUM2STR(SWO%DIST(I)%AUXDEM, 3, ', ')//NL
        !
        SWO%DIST(I)%S_ALLOTMENT_BYBEN = K > Z
    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    !Check if S_DEMAND is in use for auxiliary
    !
    DO I=ONE, SWO%NAUXDEM
        !
        IF(SWO%AUXDEM(I)%S_DEMAND > NEGNEARZERO_30) SWO%AUXDEM(I)%DEMAND = SWO%AUXDEM(I)%S_DEMAND * SWO%DELT  !Only true if defined in S
    END DO
    !
    !Check if AUXDEM EXCEEDS  is in use for auxiliary
    !
    DO I=ONE, SWO%NAUXDEM
        !
        IF(SWO%AUXDEM(I)%S_DEMAND_LIM > NEGNEARZERO_30) SWO%AUXDEM(I)%DEMAND_LIM = SWO%AUXDEM(I)%S_DEMAND_LIM * SWO%DELT  !Only true if defined in S
    END DO
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CONVERT ANY S SPECIFIED REQUIRED DELIVERIES
    !
    IF(SWO%HAS_REQ_DELIVERY) THEN
        !
        DO CONCURRENT (I = 1:SWO%NFARM); SWO%FARM(I)%REQ_DELIVERY_VOL = DZ
        END DO
        !
        DO CONCURRENT (I = 1:SWO%NFARM, SWO%FARM(I)%REQ_DELIVERY_S > NEARZERO_10)
            !
            SWO%FARM(I)%REQ_DELIVERY_VOL = SWO%FARM(I)%REQ_DELIVERY_S * SWO%DELT
            !
        END DO
        !
        DO CONCURRENT (I = 1:SWO%NFARM, SWO%FARM(I)%REQ_DELIVERY_VOL_S > NEARZERO_10)
            !
            SWO%FARM(I)%REQ_DELIVERY_VOL = SWO%FARM(I)%REQ_DELIVERY_VOL + SWO%FARM(I)%REQ_DELIVERY_VOL_S
            !
        END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS AN S OVERRIDE ON SPECIFIED RELEASES
    !
    !
    IF(SWO%HAS_RELEASE_SPEC_S) THEN
       DO I = 1, SWO%NPROJ
       DO J = 1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
          ASSOCIATE(RES => SWO%RESDAT(I)%RESBAL(J))
              !
              IF( NEAR_ninf < RES%RELEASE_SPEC_S ) THEN
                  !
                  RES%RELEASE_SPEC = RES%RELEASE_SPEC_S * SWO%DELT
                  !
              END IF
          END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CONVERT ANY S SPECIFIED MINIMUJM RELEASES TO VOLUMES
    !
    IF(SWO%HAS_MIN_RELEASE_S) THEN
       DO I = 1, SWO%NPROJ
       DO J = 1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%MIN_RELEASE_S > NEAR_ninf .AND. SWO%RESDAT(I)%RESBAL(J)%INUSE)  THEN !only true if S variable defined
           !
           IF(  SWO%RESDAT(I)%RESBAL(J)%MIN_RELEASE_S > NEARZERO_10  .AND. SWO%RESDAT(I)%RESBAL(J)%MIN_RELEASE_S < D100) THEN
                SWO%RESDAT(I)%RESBAL(J)%RELEASE_MIN_INPUT = SWO%RESDAT(I)%RESBAL(J)%MIN_RELEASE_S * SWO%DELT
           ELSE
                SWO%RESDAT(I)%RESBAL(J)%RELEASE_MIN_INPUT = DZ
           END IF
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CONVERT ANY S SPECIFIED ADDITIONAL RELEASES TO VOLUMES
    !
    IF(SWO%HAS_ADD_RELEASE_S) THEN
       DO CONCURRENT(I = 1:SWO%NPROJ)
       DO CONCURRENT(J = 1:SWO%NRES_BAL(I))
           IF( SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. SWO%RESDAT(I)%RESBAL(J)%ADD_RELEASE_S > NEARZERO_10  .AND. SWO%RESDAT(I)%RESBAL(J)%ADD_RELEASE_S < D100) THEN
                SWO%RESDAT(I)%RESBAL(J)%ADD_RELEASE_S_VOL = SWO%RESDAT(I)%RESBAL(J)%ADD_RELEASE_S * SWO%DELT
           ELSE
                SWO%RESDAT(I)%RESBAL(J)%ADD_RELEASE_S_VOL = DZ
           END IF
       END DO; END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS AN S LIMIT ON THE MININUM STORAGE - Overrides Main Input
    !
    !
    IF(SWO%HAS_MIN_STORAGE_S) THEN
       DO I=1, SWO%NPROJ
       DO J=1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
         ASSOCIATE(RES => SWO%RESDAT(I)%RESBAL(J))
           !
           IF( NEAR_ninf < RES%MIN_STORAGE_S ) THEN
               !
               RES%STORAGE_MIN = RES%MIN_STORAGE_S
               !
               IF(RES%STORAGE_MIN < RES%STORAGE_DPL) RES%STORAGE_MIN = RES%STORAGE_DPL
               !
               IF(RES%RESBAL_POOLFLG == ONE) THEN
                   JRES = RES%MAIN_SPLT
                   !
                   CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
                   !
                   STOR = RES%STORAGE_MIN*FRAC
                   !
                   CALL ACAP_STOR2ELEV(SWO,I,JRES,STOR,RES%MIN_ELEV) ! ELEV @ MAIN SPLIT RESERVOIR
                   !
               ELSE
                   CALL ACAP_STOR2ELEV(SWO,I,J,RES%STORAGE_MIN,RES%MIN_ELEV)
               END IF
           END IF
           !
           IF (NEAR_ninf < RES%MIN_STAGE_S) THEN
               !
               IF(RES%RESBAL_POOLFLG == ONE) THEN
                   JRES = RES%MAIN_SPLT
                   !
                   CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
                   !
                   IF( FRAC > NEARZERO_30) THEN
                         STOR = DZ
                         CALL ACAP_ELEV2STOR(SWO,I,JRES,RES%MIN_STAGE_S,STOR)
                         STOR = STOR/FRAC  !GET TOTAL STORAGE
                   ELSE
                         STOR = DZ
                   END IF
               ELSE
                   CALL ACAP_ELEV2STOR(SWO,I,J,RES%MIN_STAGE_S,STOR)
               END IF
               !
               RES%STORAGE_MIN = STOR
               !
               IF( RES%STORAGE_MIN < RES%STORAGE_DPL) THEN
                   RES%STORAGE_MIN = RES%STORAGE_DPL
                   !
                   IF(RES%RESBAL_POOLFLG == ONE) THEN
                       JRES = RES%MAIN_SPLT
                       !
                       CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
                       !
                       STOR = RES%STORAGE_MIN*FRAC
                       !
                       CALL ACAP_STOR2ELEV(SWO,I,JRES,STOR,RES%MIN_ELEV) ! ELEV @ MAIN SPLIT RESERVOIR
                       !
                   ELSE
                       CALL ACAP_STOR2ELEV(SWO,I,J,RES%STORAGE_MIN,RES%MIN_ELEV)
                   END IF
               END IF
           END IF
           !
         END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! APPLY ANY ELEVATION CONSTRAINTS
    !
    IF(SWO%HAS_ELEV_CHNG_S) THEN
      !
      DO CONCURRENT (I = ONE:SWO%NPROJ)                                                                                      !ninf is a flag to indicate its not in use
      DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. SWO%RESDAT(I)%RESBAL(J)%ELEV_CHNG_S > SNGL_ninf)
                !
                IF(SWO%RESDAT(I)%RESBAL(J)%ELEV_CHNG_S > NEARZERO_12) THEN
                    !
                    SWO%RESDAT(I)%RESBAL(J)%ELEV_CHNG = SWO%RESDAT(I)%RESBAL(J)%ELEV_CHNG_S
                ELSE
                    SWO%RESDAT(I)%RESBAL(J)%ELEV_CHNG = DNEG
                END IF
      END DO
      END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! COMPUTE MINIMUM ALLOWED RESERVOIR STORAGE STEP 2 (ANY ADDITIONAL CONSTRAINTS -- MAX ELEVATION CHANGE)
    !
    IF(SWO%HAS_ELEV_CHNG) THEN
       DO I=ONE, SWO%NPROJ
       DO J=ONE, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. SWO%RESDAT(I)%RESBAL(J)%ELEV_CHNG > DZ) THEN
         !
         ASSOCIATE( RES => SWO%RESDAT(I)%RESBAL(J), RESSPLIT => SWO%RESDAT(I)%RESSPLIT, STORAGE_MIN => SWO%RESDAT(I)%RESBAL(J)%STORAGE_MIN,  STORAGE_DPL=>SWO%RESDAT(I)%RESBAL(J)%STORAGE_DPL)
           !
           ELEV_MIN = RES%ELEV - RES%ELEV_CHNG*SWO%DELT
           IF(ELEV_MIN > RES%MIN_ELEV) THEN
               RES%MIN_ELEV = ELEV_MIN
               IF(RES%RESBAL_POOLFLG == ONE) THEN
                   JRES = RES%MAIN_SPLT
                   STOR = DZ; FRAC = DZ
                   CALL FRAC_POOL2SPLIT(SWO,I,JRES,SWO%FRSTOP,STORAGE_MIN,STOR,SWO%IS_LEAP_END)
                   FRAC = STOR / STORAGE_MIN  !STOR = SPLIT_STORAGE_SPEC_MIN
                   !                           !FRAC = Ratio of Split to Tot STORAGE_MIN
                   IF( FRAC > NEARZERO_30) THEN
                   !!!CALL ACAP_STOR2ELEV(SWO,I,JRES,STOR,ELEV,ERROR) ! ELEV @ SPLIT_STORAGE_SPEC_MIN
                   !!!!
                   !!!IF(ELEV < ELEV_MIN) THEN
                         STOR = DZ
                         CALL ACAP_ELEV2STOR(SWO,I,JRES,ELEV_MIN,STOR) !DTMP = STOR IS STORAGE AT MIN ELEVATION
                         STORAGE_MIN = STOR/FRAC
                   !!!END IF
                   END IF
               ELSE
                   CALL ACAP_ELEV2STOR(SWO,I,J,ELEV_MIN,STORAGE_MIN)  !OVERWRTE STORAGE_MIN WITH THE NEW LIMIT
               END IF
               !
               IF( STORAGE_MIN < STORAGE_DPL) THEN  !SHOULD NEVER BE TRUE
                   STORAGE_MIN = STORAGE_DPL
                   !
                   IF(RES%RESBAL_POOLFLG == ONE) THEN
                       JRES = RES%MAIN_SPLT
                       !
                       CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
                       !
                       STOR = STORAGE_MIN*FRAC
                       !
                       CALL ACAP_STOR2ELEV(SWO,I,JRES,STOR,RES%MIN_ELEV) ! ELEV @ MAIN SPLIT RESERVOIR
                       !
                   ELSE
                       CALL ACAP_STOR2ELEV(SWO,I,J,RES%STORAGE_MIN,RES%MIN_ELEV)
                   END IF
               END IF
           !ELSE
           !    STORAGE_MIN = STORAGE_SPEC_MIN  !ALREADY SET
           !    !
           END IF
           !
         END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS AN S LIMIT ON THE MININUM STORAGE FOR TRANSFERS
    !
    !
    IF(SWO%HAS_MIN_STORAGE_TRAN_S) THEN
       DO I=1, SWO%NPROJ
       DO J=1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
         ASSOCIATE(RES => SWO%RESDAT(I)%RESBAL(J))
           !
           IF( NEAR_ninf < RES%MIN_STORAGE_TRAN_S ) THEN
               !
               RES%STORAGE_MIN_TRAN = RES%MIN_STORAGE_TRAN_S
               !
               IF(RES%STORAGE_MIN_TRAN < DZ) RES%STORAGE_MIN_TRAN = DZ
               !
           END IF
           !
           !IF (NEAR_ninf < RES%MIN_STAGE_S) THEN
           !    !
           !    CALL ACAP_ELEV2STOR(SWO,I,J,RES%MIN_STAGE_S,STOR)
           !    !
           !    RES%STORAGE_MIN = STOR
           !    !
           !    IF( RES%STORAGE_MIN < RES%STORAGE_DPL) THEN
           !        RES%STORAGE_MIN = RES%STORAGE_DPL
           !        !
           !        IF(RES%RESBAL_POOLFLG == ONE) THEN
           !            JRES = RES%MAIN_SPLT
           !            !
           !            CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
           !            !
           !            STOR = RES%STORAGE_MIN*FRAC
           !            !
           !            CALL ACAP_STOR2ELEV(SWO,I,JRES,STOR,RES%MIN_ELEV) ! ELEV @ MAIN SPLIT RESERVOIR
           !            !
           !        ELSE
           !            CALL ACAP_STOR2ELEV(SWO,I,J,RES%STORAGE_MIN,RES%MIN_ELEV)
           !        END IF
           !    END IF
           !END IF
           !
         END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS A SPILLWAY ELEVATION SPECIFIED
    !
    IF(SWO%HAS_SPILL_S) THEN
       DO I=1, SWO%NPROJ
       DO J=1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
         ASSOCIATE(RES => SWO%RESDAT(I)%RESBAL(J))
           !
           IF( RES%SPILL_STORE_S < NEAR_inf ) THEN  !RES%STORAGE_DPL <= RES%SPILL_STORE_S .AND. RES%SPILL_STORE_S <= RES%ACAP_MAX_STOR) THEN   !MAX ALLOWED STORAGE FOR TS COMPARED TO MAX STRUCTURAL STORAGE
               !
               RES%STORAGE_SPILL = RES%SPILL_STORE_S
               !
           ELSEIF (RES%SPILL_STAGE_S < NEAR_inf ) THEN  !RES%ACAP_MIN_ELEV <= RES%SPILL_STAGE_S .AND. RES%SPILL_STAGE_S <= RES%ACAP_MAX_ELEV) THEN
               !
               IF(RES%RESBAL_POOLFLG == ONE) THEN
                   JRES = RES%MAIN_SPLT
                   !
                   CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
                   !
                   IF( FRAC > NEARZERO_30) THEN
                         STOR = DZ
                         CALL ACAP_ELEV2STOR(SWO,I,JRES,RES%SPILL_STAGE_S,STOR)
                         STOR = STOR/FRAC  !GET TOTAL STORAGE
                   ELSE
                         STOR = DZ
                   END IF
               ELSE
                   CALL ACAP_ELEV2STOR(SWO,I,J,RES%SPILL_STAGE_S,STOR)
               END IF
               !
               RES%STORAGE_SPILL = STOR
           END IF
           !
         END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS MAX STORAGE CONSTRAINTS -- FLOOD CONTROL
    !
    IF(SWO%HAS_MAX_STORAGE_S .OR. SWO%HAS_MAX_STAGE_S) THEN
       DO CONCURRENT (I = 1:SWO%NPROJ)
       DO CONCURRENT (J = 1:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE)
           !
           IF(SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL .GE. DZ .AND. SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL < SWO%RESDAT(I)%RESBAL(J)%STORAGE_CAPACITY) THEN
               SWO%RESDAT(I)%RESBAL(J)%STORAGE_MAX = SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL
           ELSE
               SWO%RESDAT(I)%RESBAL(J)%STORAGE_MAX = SWO%RESDAT(I)%RESBAL(J)%STORAGE_CAPACITY
           END IF
           !
       END DO; END DO
    END IF
    !
    IF(SWO%HAS_MAX_STORAGE_S) THEN
       DO I=1, SWO%NPROJ
       DO J=1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
         ASSOCIATE(RES => SWO%RESDAT(I)%RESBAL(J))
           !
           IF( DZ <= RES%MAX_STORAGE_S  .AND. RES%MAX_STORAGE_S < RES%ACAP_MAX_STOR ) THEN   !MAX ALLOWED STORAGE FOR TS COMPARED TO MAX STRUCTURAL STORAGE
               !
               RES%STORAGE_MAX = RES%MAX_STORAGE_S
               IF( RES%STORAGE_SPILL < RES%STORAGE_MAX .AND. RES%STORAGE_SPILL.GE.DZ ) RES%STORAGE_MAX = RES%STORAGE_SPILL
               IF( RES%STORAGE_MIN   > RES%STORAGE_MAX                               ) RES%STORAGE_MAX = RES%STORAGE_MIN
               !
           END IF
           !
         END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    IF(SWO%HAS_MAX_STAGE_S) THEN
       DO I=1, SWO%NPROJ
       DO J=1, SWO%NRES_BAL(I)
       IF(SWO%RESDAT(I)%RESBAL(J)%INUSE .AND.                                                &
          SWO%RESDAT(I)%RESBAL(J)%ACAP_MIN_ELEV <= SWO%RESDAT(I)%RESBAL(J)%MAX_STAGE_S .AND. &
          SWO%RESDAT(I)%RESBAL(J)%MAX_STAGE_S   <= SWO%RESDAT(I)%RESBAL(J)%ACAP_MAX_ELEV     ) THEN
           !
           ASSOCIATE(RES => SWO%RESDAT(I)%RESBAL(J), STORAGE_MIN => SWO%RESDAT(I)%RESBAL(J)%STORAGE_MIN)
               !
               IF(RES%RESBAL_POOLFLG == ONE) THEN
                   JRES = RES%MAIN_SPLT
                   !
                   CALL SPLIT_RES_FRAC(SWO,I,JRES,SWO%FRSTOP,FRAC,SWO%IS_LEAP_END)
                   !
                   IF( FRAC > NEARZERO_30) THEN
                         STOR = DZ
                         CALL ACAP_ELEV2STOR(SWO,I,JRES,RES%MAX_STAGE_S,STOR)
                         STOR = STOR/FRAC  !GET TOTAL STORAGE
                   ELSE
                         STOR = DZ
                   END IF
               ELSE
                   CALL ACAP_ELEV2STOR(SWO,I,J,RES%MAX_STAGE_S,STOR)
               END IF
               !
               RES%STORAGE_MAX = STOR
               IF( RES%STORAGE_MIN > RES%STORAGE_MAX) RES%STORAGE_MAX = RES%STORAGE_MIN
           END ASSOCIATE
       END IF
       END DO
       END DO
    END IF
    !
    ! Init any storage transfers
    DO CONCURRENT (I = 1:SWO%NPROJ)
    DO CONCURRENT (J = 1:SWO%NRES_BAL(I))
                                         SWO%RESDAT(I)%RESBAL(J)%STORAGE_TRANSFER = DZ
    END DO; END DO
    !
    IF(SWO%TRANS%HAS_TRAN) THEN
        DO K=ONE, SWO%TRANS%N
            ASSOCIATE(TRANSFER => SWO%TRANS%RAT(K), RES1 => SWO%TRANS%RES(ONE,K), RES2 => SWO%TRANS%RES(TWO,K))
             !
             IF( NOT_NEAR_ZERO(TRANSFER, NEARZERO_5) ) THEN ! THERE IS WATER TRANFERED
                !
                TRANS = TRANSFER*SWO%DELT  !NOTE TRANS IS A VOLUME AND NOT A RATE!!!
                !
                IRES = RES1
                IF(RES1 == Z) IRES = RES2    !PLACE HOLDER TO PREVENT INDEX ERROR -- THIS WILL NEVER BE USED BECAUSE RES1 = Z
                !
                JRES = RES2
                IF(RES2 == Z) JRES = RES1    !PLACE HOLDER TO PREVENT INDEX ERROR -- THIS WILL NEVER BE USED BECAUSE RES2 = Z
                !
                I1 = SWO%RESBAL2PROJ(ONE,IRES)  ! RES1 ID
                J1 = SWO%RESBAL2PROJ(TWO,IRES)  ! RES1 ID
                I2 = SWO%RESBAL2PROJ(ONE,JRES)  ! RES2 ID
                J2 = SWO%RESBAL2PROJ(TWO,JRES)  ! RES2 ID
                !
                ASSOCIATE(                                                                  &
                          STORAGE1      => SWO%RESDAT(I1)%RESBAL(J1)%STORAGE_PREV,          &   ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
                          STORAGE_MIN1  => SWO%RESDAT(I1)%RESBAL(J1)%STORAGE_MIN_TRAN,      &
                          TRANSFER1     => SWO%RESDAT(I1)%RESBAL(J1)%STORAGE_TRANSFER,      &
                          !
                          STORAGE2      => SWO%RESDAT(I2)%RESBAL(J2)%STORAGE_PREV,          &
                          STORAGE_MIN2  => SWO%RESDAT(I2)%RESBAL(J2)%STORAGE_MIN_TRAN,      &
                          TRANSFER2     => SWO%RESDAT(I2)%RESBAL(J2)%STORAGE_TRANSFER       &
                         )
                    IF(RES2 < ONE) THEN
                        !
                        IF    (TRANS > DZ .AND. STORAGE1 + TRANSFER1         < STORAGE_MIN1) CYCLE                                        !CANNOT TRANFER WATER DUE TO STORAGE+PREVIOUS TRANSFERS ALREADY BELOW DEAD POOL
                        IF    (TRANS > DZ .AND. STORAGE1 + TRANSFER1 - TRANS < STORAGE_MIN1) TRANS = STORAGE1 + TRANSFER1 - STORAGE_MIN1  !CANNOT TRANFER MORE WATER THEN WHAT IS AVAILIBLE
                        !
                        TRANSFER1 = TRANSFER1 - TRANS
                        !
                    ELSEIF(RES1 < ONE) THEN
                        !
                        IF(TRANS < DZ .AND. STORAGE2 + TRANSFER2         < STORAGE_MIN2) CYCLE                                              !CANNOT TRANFER WATER DUE TO STORAGE+PREVIOUS TRANSFERS ALREADY BELOW DEAD POOL
                        IF(TRANS < DZ .AND. STORAGE2 + TRANSFER2 + TRANS < STORAGE_MIN2) TRANS = DNEG*(STORAGE2 + TRANSFER2 - STORAGE_MIN2) !CANNOT TRANFER MORE WATER THEN WHAT IS AVAILIBLE
                        !
                        TRANSFER2 = TRANSFER2 + TRANS
                    ELSE
                          IF    (TRANS > DZ .AND. STORAGE1 + TRANSFER1 < STORAGE_MIN1) THEN; CYCLE !CANNOT TRANFER WATER DUE TO STORAGE+PREVIOUS TRANSFERS ALREADY BELOW DEAD POOL -- Norm
                          ELSEIF(TRANS < DZ .AND. STORAGE2 + TRANSFER2 < STORAGE_MIN2) THEN; CYCLE
                          END IF
                          !
                          IF    (TRANS > DZ .AND. STORAGE1 + TRANSFER1 - TRANS < STORAGE_MIN1) THEN; TRANS =       STORAGE1 + TRANSFER1 - STORAGE_MIN1  !CANNOT TRANFER MORE WATER THEN WHAT IS AVAILIBLE
                          ELSEIF(TRANS < DZ .AND. STORAGE2 + TRANSFER2 + TRANS < STORAGE_MIN2) THEN; TRANS = DNEG*(STORAGE2 + TRANSFER2 - STORAGE_MIN2) !CANNOT TRANFER MORE WATER THEN WHAT IS AVAILIBLE
                          END IF
                          !
                          TRANSFER1 = TRANSFER1 - TRANS
                          TRANSFER2 = TRANSFER2 + TRANS
                    END IF
                END ASSOCIATE
             END IF
            END ASSOCIATE
        END DO
    END IF
    !
    ! Any transfers cannot lower below th
    !
    !
    !IF(SWO%HAS_RELEASE_FRAC_S) THEN
    !   DO I = ONE, SWO%NPROJ
    !         IF(    SWO%NRES_BAL(I) == Z  ) THEN; CONTINUE
    !         ELSEIF(SWO%NRES_BAL(I) == ONE) THEN; SWO%RESDAT(I)%RESBAL(ONE)%RELEASE_FRAC = UNO
    !         ELSE
    !             NRES=Z
    !             DO J = ONE, SWO%NRES_BAL(I)
    !                 IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) NRES = NRES + ONE
    !             END DO
    !             !
    !             FRAC = DZ
    !             DO J = ONE, SWO%NRES_BAL(I)
    !                 IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
    !                    IF(SWO%RESDAT(I)%RESBAL(J)%RELEASE_FRAC < DZ) THEN
    !                        FRAC = DNEG
    !                        EXIT
    !                    ELSE
    !                        FRAC = FRAC + SWO%RESDAT(I)%RESBAL(J)%RELEASE_FRAC
    !                    END IF
    !                 END IF
    !             END DO
    !             !
    !             IF(    NRES == Z    ) THEN
    !                 CONTINUE
    !             ELSEIF(FRAC < DZ) THEN
    !                 IF(    NRES == TWO  ) THEN; FRAC = HALF
    !                 ELSEIF(NRES == THREE) THEN; FRAC = THIRD
    !                 ELSEIF(NRES == FOUR ) THEN; FRAC = FOURTH
    !                 ELSEIF(NRES == FIVE ) THEN; FRAC = FIFTH
    !                 ELSE;                       FRAC = UNO / DBLE(NRES)
    !                 END IF
    !                 DO CONCURRENT (J = ONE:SWO%NRES_BAL(I))
    !                     SWO%RESDAT(I)%RESBAL(J)%RELEASE_FRAC = FRAC
    !                 END DO
    !             ELSEIF(SNGL(FRAC).NE.1.0) THEN
    !                 DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE)
    !                     SWO%RESDAT(I)%RESBAL(J)%RELEASE_FRAC = SWO%RESDAT(I)%RESBAL(J)%RELEASE_FRAC / FRAC
    !                 END DO
    !             END IF
    !         END IF
    !   END DO
    !END IF
    !
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS S SPECIFED RELEASE FRACTIONS
    !
    !  MAIN DEMAND FRACTIONS ================================================
    !
    IF(SWO%S_RELEASE_FRAC%HAS_FRAC) THEN
       !
       DO I = ONE, SWO%NPROJ
         IF(SWO%S_RELEASE_FRAC%HAS_S(I)) THEN
             !
             IF(    SWO%NRES_BAL(I) == Z  ) THEN; CYCLE                !STUPID CHECKS
             ELSEIF(SWO%NRES_BAL(I) == ONE) THEN;
                 SWO%RESDAT(I)%RESBAL(ONE)%RELEASE_DMD_FRAC_INI = UNO
                 CYCLE
             END IF
             !
             K = SWO%S_RELEASE_FRAC%BACK_CALC(I)
             !
             IF    (K > Z) THEN     !K Points to missing value that should back calculated
                 DTMP = DZ
                 DO J = ONE, SWO%NRES_BAL(I)
                       IF(J.NE.K) THEN
                           IF(SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI < DZ) THEN
                                  DTMP = SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI
                                  EXIT
                           ELSE
                                  DTMP = DTMP + SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI
                           END IF
                       END IF
                 END DO
                 IF(DTMP < DZ .OR. 1.9D0 < DTMP) THEN
                        SWO%RESDAT(I)%RESBAL(K)%RELEASE_DMD_FRAC_INI = DTMP
                        EXIT
                 ELSE
                        SWO%RESDAT(I)%RESBAL(K)%RELEASE_DMD_FRAC_INI = UNO - DTMP
                 END IF
             ELSEIF(K < Z) THEN   !K is location of property that should be replicated
                 K = -K
                 DTMP = SWO%RESDAT(I)%RESBAL(K)%RELEASE_DMD_FRAC_INI
                 DO CONCURRENT(J = ONE:SWO%NRES_BAL(I))
                        SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI = DTMP
                 END DO
              END IF
              !
              NRES=Z
              DO J = ONE, SWO%NRES_BAL(I)
                  IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
                      NRES = NRES + ONE !DETERMINE NUMBER OF ACTIVE RESERVOIRS
                  END IF
              END DO
              !
              DTMP = DZ
              DO J = ONE, SWO%NRES_BAL(I)
                ASSOCIATE(RELEASE_DMD_FRAC_INI => SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI, RESBAL => SWO%RESDAT(I)%RESBAL)
                  !
                  IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) THEN
                     IF(RELEASE_DMD_FRAC_INI < DZ) THEN
                         DTMP = DNEG
                         EXIT
                     ELSEIF(RELEASE_DMD_FRAC_INI > 1.9D0) THEN
                         !
                         DO CONCURRENT (K = ONE:SWO%NRES_BAL(I), K.NE.J); RESBAL(K)%RELEASE_DMD_FRAC_INI = RELEASE_DMD_FRAC_INI
                         END DO
                         DTMP = inf
                         EXIT
                     ELSE
                         DTMP = DTMP + RELEASE_DMD_FRAC_INI
                     END IF
                  END IF
                END ASSOCIATE
              END DO
              !
              IF(    NRES == Z .OR. DTMP > NEAR_inf ) THEN  !PROCESS FRACTIOSN AT A LATER DATE
                  CONTINUE
              ELSEIF(DTMP < DZ) THEN
                  IF(    NRES == TWO  ) THEN; DTMP = HALF
                  ELSEIF(NRES == THREE) THEN; DTMP = THIRD
                  ELSEIF(NRES == FOUR ) THEN; DTMP = FOURTH
                  ELSEIF(NRES == FIVE ) THEN; DTMP = FIFTH
                  ELSE;                       DTMP = UNO / DBLE(NRES)
                  END IF
                  DO CONCURRENT (J = ONE:SWO%NRES_BAL(I));   SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI = DTMP
                  END DO
              ELSEIF(SNGL(DTMP).NE.1.0) THEN
                  ASSOCIATE(RESBAL => SWO%RESDAT(I)%RESBAL)
                     DO CONCURRENT (J = ONE:SWO%NRES_BAL(I)); RESBAL(J)%RELEASE_DMD_FRAC_INI = RESBAL(J)%RELEASE_DMD_FRAC_INI / DTMP
                     END DO
                  END ASSOCIATE
              END IF
         END IF
       END DO
    END IF
    !
    ! --------------------------------------------------------------------------------
    ! SET UP REQUIRED RELEASE FRACTIONS
    !
    !
    IF(SWO%S_REQ_RELEASE_FRAC .OR. (SWO%REQ_FRAC_USES_DMD_FRAC .AND. SWO%S_RELEASE_FRAC%HAS_FRAC)) THEN
       !
       IF(SWO%REQ_FRAC_USES_DMD_FRAC) THEN
           DO CONCURRENT(I = ONE:SWO%NPROJ      )
           DO CONCURRENT(J = ONE:SWO%NRES_BAL(I))
               SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI = SWO%RESDAT(I)%RESBAL(J)%RELEASE_DMD_FRAC_INI
           END DO; END DO
       END IF
       !
       REQF: DO IREQ = ONE, SWO%REQFLOW%N
          !
          ASSOCIATE(N => SWO%REQFLOW%RES(IREQ)%N, RES => SWO%REQFLOW%RES(IREQ)%VEC)
             !
             DO I = ONE, IREQ - ONE
                 IF (ALL(RES == SWO%REQFLOW%RES(I)%VEC)) CYCLE REQF !ALREADY DEFINED FRACTIONS FOR RES COMBINATION
             END DO
             !
             IF(N == ONE) THEN
                 I = SWO%RESBAL2PROJ(ONE,RES(ONE))
                 J = SWO%RESBAL2PROJ(TWO,RES(ONE))
                 SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI = UNO
             ELSE
                 NRES = Z
                 DTMP = DZ
                 DO K=ONE, N
                    I = SWO%RESBAL2PROJ(ONE,RES(K))
                    J = SWO%RESBAL2PROJ(TWO,RES(K))
                    !
                    IF(SWO%RESDAT(I)%RESBAL(J)%INUSE ) THEN
                       !
                       NRES = NRES + ONE
                       !
                       ASSOCIATE(RELEASE_REQ_FRAC_INI => SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI)
                          !
                          IF(RELEASE_REQ_FRAC_INI < -1.9D0) THEN
                              !
                              DO F=ONE, N
                                 IF(F.NE.K) THEN
                                    ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(F)), JJ => SWO%RESBAL2PROJ(TWO,RES(F)))
                                      !
                                      SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI = RELEASE_REQ_FRAC_INI
                                      !
                                    END ASSOCIATE
                                 END IF
                              END DO
                              DTMP = inf
                              EXIT
                          ELSEIF(RELEASE_REQ_FRAC_INI < DZ) THEN
                              DTMP = DNEG
                              EXIT
                          ELSE
                              DTMP = DTMP + RELEASE_REQ_FRAC_INI
                          END IF
                       END ASSOCIATE
                    END IF
                 END DO
                 !
                 IF(    NRES == Z .OR. DTMP > NEAR_inf ) THEN  !PROCESS FRACTIONS AT A LATER DATE
                     CONTINUE
                 ELSEIF(DTMP < DZ) THEN
                     IF(    NRES == TWO  ) THEN; DTMP = HALF
                     ELSEIF(NRES == THREE) THEN; DTMP = THIRD
                     ELSEIF(NRES == FOUR ) THEN; DTMP = FOURTH
                     ELSEIF(NRES == FIVE ) THEN; DTMP = FIFTH
                     ELSE;                       DTMP = UNO / DBLE(NRES)
                     END IF
                     DO F=ONE, N
                           ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(F)), JJ => SWO%RESBAL2PROJ(TWO,RES(F)))
                             !
                             SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI = DTMP
                             !
                           END ASSOCIATE
                     END DO
                 ELSEIF(SNGL(DTMP).NE.1.0) THEN
                     DO F=ONE, N
                           ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(F)), JJ => SWO%RESBAL2PROJ(TWO,RES(F)))
                             !
                             SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI = SWO%RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI / DTMP
                             !
                           END ASSOCIATE
                     END DO
                 END IF
             END IF
          END ASSOCIATE
       END DO REQF
       !
    END IF
    !
    !----------------------------------------------------------------------------------------------
    !
    ! CHECK IF THERE IS A SPILLWAY STAGE DISCHARGE AND SET UP MAX FLOW LIMIT
    !
    DO I = ONE, SWO%NPROJ
    DO J = ONE, SWO%NRES_BAL(I)
      IF(SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. SWO%RESDAT(I)%RESBAL(J)%NO_MAX_SPILL_S) THEN
        !
        ASSOCIATE( MAX_SPILL => SWO%RESDAT(I)%RESBAL(J)%MAX_SPILL, DIM=>SWO%RESDAT(I)%MAX_SPILL(J)%N )
            !
           IF(DIM == ONE) THEN
               MAX_SPILL = SWO%RESDAT(I)%MAX_SPILL(J)%MAT(ONE,ONE)
           ELSE
               ASSOCIATE( STOR_INI     => SWO%RESDAT(I)%RESBAL(J)%STORAGE,          &
                          STOR_TAB     => SWO%RESDAT(I)%MAX_SPILL(J)%MAT(:,ONE),    &
                          DIS          => SWO%RESDAT(I)%MAX_SPILL(J)%MAT(:,TWO),    &
                          !
                          AREA_MAX     => SWO%RESDAT(I)%RESBAL(J)%MAX_AREA,         &    ! Largest area of reservoir
                          AREA_SOS     => SWO%RESDAT(I)%RESBAL(J)%AREA_PREV,        &    ! Starting Area of reservoir
                          !
                          INFLOW       => SWO%RESDAT(I)%RESBAL(J)%INFLOW,           &    ! INFLOW       during current step (updated @ AD routine!)          [L3]
                          !
                          PRCP_FRAC    => SWO%RESDAT(I)%RESBAL(J)%PRECIP_AREA_FRAC, &
                          PRCP         => SWO%RESDAT(I)%RESBAL(J)%PRCP,             &    ! PRCP         during current step (updated @ AD routine!)          [L]
                          EVAP         => SWO%RESDAT(I)%RESBAL(J)%EVAP              &    ! EVAP         during current step (updated @ AD routine!)          [L]
                         )
                  DTMP1 = PRCP_FRAC*(AREA_MAX - AREA_SOS) + AREA_SOS    ! USE STARTING AREA FOR PRECXIP
                  DTMP1 = PRCP * DTMP1
                  !
                  DTMP2 = EVAP * AREA_SOS                               !USE STARTING AREA FOR EVAP
                  !
                  STOR = HALF*( INFLOW + DTMP1 - DTMP2 )   !INCLUDE HALF OF INFLOW, PRECIP, AND EVAP FOR SPILL ESTIMATE
                  IF( STOR > DZ ) THEN
                      STOR = STOR_INI + STOR
                  ELSE
                      STOR = STOR_INI
                  END IF
                  !
                  IF( STOR <= STOR_TAB(ONE) ) THEN
                      MAX_SPILL = DIS(ONE)
                  ELSE
                      N = DIM
                      DO K=TWO, DIM
                                   IF(STOR <= STOR_TAB(K)) THEN
                                       N = K - ONE
                                       EXIT
                                   END IF
                      END DO
                      !
                      IF( N == DIM ) THEN
                          MAX_SPILL = DIS(DIM)
                      ELSE
                          K = N+ONE
                          DTMP1 = ( DIS(K) - DIS(N) ) / ( STOR_TAB(K) - STOR_TAB(N) )
                          DTMP2 = STOR - STOR_TAB(N)
                          !
                          MAX_SPILL = DIS(N) + DTMP1*DTMP2
                      END IF
                  END IF
               END ASSOCIATE
           END IF
           !
           IF(MAX_SPILL < D100) MAX_SPILL = MAX_SPILL * SWO%DELT  !CONVERT DISCHARGE TO VOLUME
           !
        END ASSOCIATE
      END IF
    END DO
    END DO
    !
    ! --------------------------------------------------------------------------------
    ! SET UP ANY S RELATED LIMITS ON THE SPILLWAY FLOW
    !
    !
    IF(SWO%HAS_MAX_SPILL_S) THEN
       DO I = ONE, SWO%NPROJ
       DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. .NOT. SWO%RESDAT(I)%RESBAL(J)%NO_MAX_SPILL_S)
           IF(SWO%RESDAT(I)%RESBAL(J)%MAX_SPILL_RATE < D100) THEN
               SWO%RESDAT(I)%RESBAL(J)%MAX_SPILL = SWO%RESDAT(I)%RESBAL(J)%MAX_SPILL_RATE * SWO%DELT
           ELSE
               SWO%RESDAT(I)%RESBAL(J)%MAX_SPILL = D100
           END IF
       END DO; END DO
    END IF
    !
    ! --------------------------------------------------------------------------------
    ! Check if there is a spill way, if there is not one, then disable it.
    !
    DO I = ONE, SWO%NPROJ
    DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. &
                                           (SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL < DZ .OR. SWO%RESDAT(I)%RESBAL(J)%STORAGE_SPILL > SWO%RESDAT(I)%RESBAL(J)%STORAGE_CAPACITY*NEARZERO_14 ))
        SWO%RESDAT(I)%RESBAL(J)%MAX_SPILL = DZ
    END DO
    END DO
    !
    ! --------------------------------------------------------------------------------
    ! SET UP ALLOCATION BENEEFICIARY FRACTIONS IF SPECIFIED BY S
    !
    !
    IF(SWO%DIST_ALLOC_FRAC_TYP==THREE) THEN ! Specified Fractions BY S-LANGUAGE
        !
        DO K = ONE, SWO%NDIST
            !
            IF(SWO%DIST(K)%NFARM + SWO%DIST(K)%NAUXDEM == Z) CYCLE
            !
            DTMP = DZ
            !
            DO J=ONE, SWO%DIST(K)%NFARM
               DTMP = DTMP + SWO%FARM( SWO%DIST(K)%FARM(J) )%DIST_ALLOC_FRAC
            END DO
            !
            DO J=ONE, SWO%DIST(K)%NAUXDEM
               DTMP = DTMP + SWO%FARM( SWO%DIST(K)%AUXDEM(J) )%DIST_ALLOC_FRAC  !SUM CHECK
            END DO
            !
            IF(DTMP < SUB_ONE .OR. NEAR_ONE < DTMP) THEN                 !Should sum to 1
                !
                DO J = ONE, SWO%DIST(K)%NFARM
                   I = SWO%DIST(K)%FARM(J)
                   SWO%FARM(I)%DIST_ALLOC_FRAC =   SWO%FARM(I)%DIST_ALLOC_FRAC / DTMP
                END DO
                !
                DO J = ONE, SWO%DIST(K)%NAUXDEM
                   I = SWO%DIST(K)%AUXDEM(J)
                   SWO%AUXDEM(I)%DIST_ALLOC_FRAC = SWO%AUXDEM(I)%DIST_ALLOC_FRAC / DTMP
                END DO
            END IF
        END DO
    END IF

  END SUBROUTINE
  !
  SUBROUTINE COMPUTE_ALLOCATION_AND_RELEASE(SWO,KPER,KSTP,KITER,IDIVAR,STRM,SEG,SITER)   !FM ROUTINE
    IMPLICIT NONE
!   ******************************************************************
!   OVERVIEW:
!   FORMULATE+SOLVE TIMESTEP...
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    !USE SWOPSMODULE
    !USE SWOPSUTIL
    !USE GWFSFRMODULE, ONLY:IDIVAR,STRM,SEG
    !USE OPENSPEC
!   ------------------------------------------------------------------
!      ARGUMENTS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),                            INTENT(INOUT):: SWO
    INTEGER,          DIMENSION(:,:),CONTIGUOUS,INTENT(IN   ):: IDIVAR
    REAL,             DIMENSION(:,:),CONTIGUOUS,INTENT(IN   ):: STRM
    REAL,             DIMENSION(:,:),CONTIGUOUS,INTENT(INOUT):: SEG
    INTEGER,                                    INTENT(IN   ):: KPER,KSTP,KITER, SITER
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
!   ------------------------------------------------------------------
    INTEGER                                                             &
        :: I,J,K,F,P,ITYP,IREQ,IPROJ,IDIST,IUNIT,IFARM,IAUX,TMPSEG,     &
           ISEG,JSEG,KSEG,LSEG,NTREE,NLIST,TMP,ITREE,IBRC,ISPT,IDVS,    &
           IRES,JRES,ITER,NBRANCH,ISTR,JSTR,  &
           IRCH,JRCH,ILIST,JDVS,NUPSEG_IDVS,NUPSEG_JDVS,NSS
    DOUBLE PRECISION                                                    &
        :: DTMP,DTMP1,DTMP2,DTMP3,EFF,STOR_TRAN1,STOR_TRAN2,            &
           INFLOW,SPILL_WAY_PREV,                                       &
           STORAGE_END_PREV,AREA_AVG,PRCP_AREA,TOT_RELEASE,TOT_RELEASE_PREV, RTOL,   &
           UnderRelease,OverRelease,DFLOW,DFLOW_IN,DFLOW_RT,DFLOW_RN,DFLOW_ET,DSEEP,DDIVO, RATIO, ALLOTMENT,REQ_FLOW_RELEASE, CHARGE_CREDIT
    INTEGER:: NFARM, NPROJ, NDIST, NUNIT, NAUXDEM
    !TYPE(VAR_POINTER_LIST):: RES_RELEASES
    DOUBLE PRECISION, DIMENSION(SIX):: RES_RELEASES
    CHARACTER(:), ALLOCATABLE:: ERROR
    LOGICAL:: UPDATE, HAS_CONVERGED, EVEN_ITER, FIVE_ITER
    !
    ! SWOPS1OUT/XCHECK
    !CHARACTER(4) :: XOPT
    NSS = SWO%NSEG
    !
    !DDELT = DBLE(DELT)
    ERROR = NL
    !
    !IF(KPER>3)WRITE(123,'(3I4,*(1x F7.1))') KPER,KSTP,KITER,                                                                                                                                     &
    !                SWO%RESDAT(1)%RESBAL(1)%RELEASE_PROJ/86400./DELT,SWO%RESDAT(1)%RESBAL(1)%RELEASE_REQF/86400./DELT,SWO%RESDAT(1)%RESBAL(2)%RELEASE_PROJ/86400./DELT,SWO%RESDAT(1)%RESBAL(2)%RELEASE_REQF/86400./DELT, &
    !                STRM(10,3976 )/86400.,STRM(10,5171 )/86400.,STRM(10,6180 )/86400.,STRM(10,6885 )/86400.,STRM(10,6887 )/86400.,STRM(10,6890 )/86400.,STRM(10,6958 )/86400.,SWO%DIVSEG(1)%DIVORDER*DELT_INV/86400.,SWO%DIVSEG(1)%UP_DFLOW_IN*DELT_INV/86400.,SWO%DIVSEG(1)%UP_DFLOW_RT*DELT_INV/86400.,SWO%DIVSEG(1)%UP_DSEEP*DELT_INV/86400.,SWO%DIVSEG(1)%UP_DFLOW_RN*DELT_INV/86400.,SWO%DIVSEG(1)%UP_DFLOW_ET*DELT_INV/86400.

    !
    NAUXDEM= SWO%NAUXDEM
    NFARM = SWO%NFARM
    NPROJ = SWO%NPROJ
    NDIST = SWO%NDIST
    NUNIT = SWO%NUNIT
    !
    EVEN_ITER = MODULO(KITER,TWO) == Z
    !
    FIVE_ITER = MODULO(KITER,FIVE) == Z
    !
    !CALL RES_RELEASES%INIT(FIVE)  ! RELEASE_PROJ, RELEASE_ADD, RELEASE_REQF, RELEASE_SPEC, RELEASE_FLOD  --USED FOR REDUCTING TO MAX RELEASE AND GET SUM OF RELEASES
    !
    IF(.TRUE.) THEN !SITER == ONE

       ! SUPPLY / DEMAND / ACCOUNTING
       ! (set current flow/acct values to zero...)
       ! (set current conveyance/performance parameters to latest/greatest)
       ! (set current YTD values to end of previous step)

       ! ****************************************************************
       ! CURRENT VALUES      -- RESET TO ZERO
       ! YEAR-TO-DATE VALUES -- RESET TO END OF PREVIOUS STEP
       DO CONCURRENT (I = 1:NFARM)
         !SWO%FARM(I)%ALLOTMENT       = DZ
         SWO%FARM(I)%TFDR            = DZ
         SWO%FARM(I)%DELORDER        = DZ
         SWO%FARM(I)%DELIVERY        = DZ
         SWO%FARM(I)%DELIVERY_YTD    = SWO%FARM_PREV(I)%DELIVERY_YTD
         !SWO%FARM(I)%BALANCE         = SWO%FARM_PREV(I)%BALANCE
       END DO
       !
         DO I = 1,NAUXDEM
           SWO%AUXDEM(I)%DELORDER     = DZ
           SWO%AUXDEM(I)%DELIVERY     = DZ
           SWO%AUXDEM(I)%DELIVERY_YTD = SWO%AUXDEM_PREV(I)%DELIVERY_YTD
           !SWO%AUXDEM(I)%ALLOTMENT    = DZ
           !SWO%AUXDEM(I)%BALANCE      = SWO%AUXDEM_PREV(I)%BALANCE
         END DO

         DO I = 1,NUNIT
           SWO%UNIT(I)%TFDR          = DZ
           SWO%UNIT(I)%DELORDER      = DZ
           SWO%UNIT(I)%DIVORDER      = DZ
           SWO%UNIT(I)%DIVERSION     = DZ
           SWO%UNIT(I)%DELIVERY      = DZ
           SWO%UNIT(I)%BYPASS        = DZ
           SWO%UNIT(I)%CHARGE        = DZ
           SWO%UNIT(I)%CREDIT        = DZ
           SWO%UNIT(I)%DIVIN         = DZ
           SWO%UNIT(I)%SUMIN         = DZ
           SWO%UNIT(I)%SUMOUT        = DZ
           SWO%UNIT(I)%DELIVEFF      = SWO%UNIT_PREV(I)%DELIVEFF
           SWO%UNIT(I)%CHGRATIO      = SWO%UNIT_PREV(I)%CHGRATIO
           SWO%UNIT(I)%NETCHGRATIO   = SWO%UNIT_PREV(I)%NETCHGRATIO
           SWO%UNIT(I)%DIVERSION_YTD = SWO%UNIT_PREV(I)%DIVERSION_YTD
           SWO%UNIT(I)%DELIVERY_YTD  = SWO%UNIT_PREV(I)%DELIVERY_YTD
           SWO%UNIT(I)%BYPASS_YTD    = SWO%UNIT_PREV(I)%BYPASS_YTD
           SWO%UNIT(I)%CHARGE_YTD    = SWO%UNIT_PREV(I)%CHARGE_YTD
           SWO%UNIT(I)%CREDIT_YTD    = SWO%UNIT_PREV(I)%CREDIT_YTD
           SWO%UNIT(I)%DELIVEFF_YTD  = SWO%UNIT_PREV(I)%DELIVEFF_YTD
           SWO%UNIT(I)%CHGRATIO_YTD  = SWO%UNIT_PREV(I)%CHGRATIO_YTD
           SWO%UNIT(I)%DIVIN_YTD     = SWO%UNIT_PREV(I)%DIVIN_YTD
           SWO%UNIT(I)%SUMIN_YTD     = SWO%UNIT_PREV(I)%SUMIN_YTD
           SWO%UNIT(I)%SUMOUT_YTD    = SWO%UNIT_PREV(I)%SUMOUT_YTD
           SWO%UNIT(I)%NETCHGRATIO_YTD  = SWO%UNIT_PREV(I)%NETCHGRATIO_YTD
           SWO%UNIT(I)%ALLOTMENT     = SWO%UNIT_PREV(I)%ALLOTMENT
           SWO%UNIT(I)%BALANCE       = SWO%UNIT_PREV(I)%BALANCE
         END DO
         !
         DO I = 1,NDIST
           SWO%DIST(I)%TFDR          = DZ
           SWO%DIST(I)%DELORDER      = DZ
           SWO%DIST(I)%DIVORDER      = DZ
           SWO%DIST(I)%DIVERSION     = DZ
           SWO%DIST(I)%DELIVERY      = DZ
           SWO%DIST(I)%BYPASS        = DZ
           SWO%DIST(I)%CHARGE        = DZ
           SWO%DIST(I)%CREDIT        = DZ
           SWO%DIST(I)%DIVIN         = DZ
           SWO%DIST(I)%SUMIN         = DZ
           SWO%DIST(I)%SUMOUT        = DZ
           SWO%DIST(I)%BALANCE       = SWO%DIST_PREV(I)%BALANCE
           SWO%DIST(I)%DELIVEFF      = SWO%DIST_PREV(I)%DELIVEFF
           SWO%DIST(I)%CHGRATIO      = SWO%DIST_PREV(I)%CHGRATIO
           SWO%DIST(I)%NETCHGRATIO   = SWO%DIST_PREV(I)%NETCHGRATIO
           SWO%DIST(I)%DIVERSION_YTD = SWO%DIST_PREV(I)%DIVERSION_YTD
           SWO%DIST(I)%DELIVERY_YTD  = SWO%DIST_PREV(I)%DELIVERY_YTD
           SWO%DIST(I)%BYPASS_YTD    = SWO%DIST_PREV(I)%BYPASS_YTD
           SWO%DIST(I)%CHARGE_YTD    = SWO%DIST_PREV(I)%CHARGE_YTD
           SWO%DIST(I)%CREDIT_YTD    = SWO%DIST_PREV(I)%CREDIT_YTD
           SWO%DIST(I)%DELIVEFF_YTD  = SWO%DIST_PREV(I)%DELIVEFF_YTD
           SWO%DIST(I)%CHGRATIO_YTD  = SWO%DIST_PREV(I)%CHGRATIO_YTD
           SWO%DIST(I)%DIVIN_YTD     = SWO%DIST_PREV(I)%DIVIN_YTD
           SWO%DIST(I)%SUMIN_YTD     = SWO%DIST_PREV(I)%SUMIN_YTD
           SWO%DIST(I)%SUMOUT_YTD    = SWO%DIST_PREV(I)%SUMOUT_YTD
           SWO%DIST(I)%NETCHGRATIO_YTD = SWO%DIST_PREV(I)%NETCHGRATIO_YTD
         END DO
         !
         DO I = 1,NPROJ
           SWO%PROJ(I)%TFDR          = DZ
           SWO%PROJ(I)%DELORDER      = DZ
           SWO%PROJ(I)%DIVORDER      = DZ
           SWO%PROJ(I)%RELEASE       = DZ
           SWO%PROJ(I)%DIVERSION     = DZ
           !SWO%PROJ(I)%OUTFLOW       = DZ
           SWO%PROJ(I)%DELIVERY      = DZ
           SWO%PROJ(I)%BYPASS        = DZ
           SWO%PROJ(I)%CHARGE        = DZ
           SWO%PROJ(I)%CREDIT        = DZ
           SWO%PROJ(I)%DIVIN         = DZ
           SWO%PROJ(I)%SUMIN         = DZ
           SWO%PROJ(I)%SUMOUT        = DZ
           SWO%PROJ(I)%DELIVEFF      = SWO%PROJ_PREV(I)%DELIVEFF
           SWO%PROJ(I)%DIVRATIO      = SWO%PROJ_PREV(I)%DIVRATIO
           SWO%PROJ(I)%CHGRATIO      = SWO%PROJ_PREV(I)%CHGRATIO
           SWO%PROJ(I)%NETCHGRATIO   = SWO%PROJ_PREV(I)%NETCHGRATIO
           SWO%PROJ(I)%RELEASE_YTD   = SWO%PROJ_PREV(I)%RELEASE_YTD
           SWO%PROJ(I)%DIVERSION_YTD = SWO%PROJ_PREV(I)%DIVERSION_YTD
           !SWO%PROJ(I)%OUTFLOW_YTD   = SWO%PROJ_PREV(I)%OUTFLOW_YTD
           SWO%PROJ(I)%DELIVERY_YTD  = SWO%PROJ_PREV(I)%DELIVERY_YTD
           SWO%PROJ(I)%BYPASS_YTD    = SWO%PROJ_PREV(I)%BYPASS_YTD
           SWO%PROJ(I)%CHARGE_YTD    = SWO%PROJ_PREV(I)%CHARGE_YTD
           SWO%PROJ(I)%CREDIT_YTD    = SWO%PROJ_PREV(I)%CREDIT_YTD
           SWO%PROJ(I)%DELIVEFF_YTD  = SWO%PROJ_PREV(I)%DELIVEFF_YTD
           SWO%PROJ(I)%DIVRATIO_YTD  = SWO%PROJ_PREV(I)%DIVRATIO_YTD
           SWO%PROJ(I)%CHGRATIO_YTD  = SWO%PROJ_PREV(I)%CHGRATIO_YTD
           SWO%PROJ(I)%DIVIN_YTD     = SWO%PROJ_PREV(I)%DIVIN_YTD
           SWO%PROJ(I)%SUMIN_YTD     = SWO%PROJ_PREV(I)%SUMIN_YTD
           SWO%PROJ(I)%SUMOUT_YTD    = SWO%PROJ_PREV(I)%SUMOUT_YTD
           SWO%PROJ(I)%NETCHGRATIO_YTD = SWO%PROJ_PREV(I)%NETCHGRATIO_YTD
         END DO
         I = 1
         !
         ! Back up previous calcualte project demand
         !
         DO CONCURRENT (I=ONE:NPROJ)
                                    SWO%RESDAT(I)%PROJ_RELEASE_DMD_PREV = SWO%RESDAT(I)%PROJ_RELEASE_DMD  !BACK UP PROJECT DEMAND aka PROJ_PROJ_RELEASE_DMD
                                    SWO%RESDAT(I)%PROJ_RELEASE_DMD      = DZ
                                    !
                                    SWO%RESDAT(I)%PROJ_RELEASE_PREV_FIN = SWO%RESDAT(I)%PROJ_RELEASE_FIN  !BACK UP PROJECT DEMAND aka PROJ_PROJ_RELEASE_DMD
                                    SWO%RESDAT(I)%PROJ_RELEASE_FIN      = DZ
         END DO
         !
         DO IDVS = 1,SWO%DIVCOUNT
           SWO%DIVSEG(IDVS)%TFDR       = DZ
           SWO%DIVSEG(IDVS)%DELORDER   = DZ
           SWO%DIVSEG(IDVS)%DIVORDER   = DZ
           SWO%DIVSEG(IDVS)%DIVERSION  = DZ
           SWO%DIVSEG(IDVS)%DELIVERY   = DZ
           SWO%DIVSEG(IDVS)%DELIVEFF   = SWO%DIVSEG_PREV(IDVS)%DELIVEFF
           SWO%DIVSEG(IDVS)%DSEEP      = DZ
           SWO%DIVSEG(IDVS)%DFLOW_IN   = DZ
           SWO%DIVSEG(IDVS)%DFLOW_RT   = DZ
           SWO%DIVSEG(IDVS)%UP_DIVORDER = DZ
           SWO%DIVSEG(IDVS)%UP_DSEEP    = DZ
           SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DZ
           SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DZ
           SWO%DIVSEG(IDVS)%UP_DFLOW_RN = DZ
           SWO%DIVSEG(IDVS)%UP_DFLOW_ET = DZ
         END DO !(IDVS)
         !
         DO ISPT = 1,SWO%SPTCOUNT
           SWO%SPTSEG(ISPT)%TFDR       = DZ
           SWO%SPTSEG(ISPT)%DELORDER   = DZ
           SWO%SPTSEG(ISPT)%DIVORDER   = DZ
           SWO%SPTSEG(ISPT)%DIVERSION  = DZ
           SWO%SPTSEG(ISPT)%DELIVERY   = DZ
           SWO%SPTSEG(ISPT)%DELIVEFF   = SWO%SPTSEG_PREV(ISPT)%DELIVEFF
           SWO%SPTSEG(ISPT)%DSEEP      = DZ
           SWO%SPTSEG(ISPT)%DFLOW_IN   = DZ
           SWO%SPTSEG(ISPT)%DFLOW_RT   = DZ
           DO J = 1,SWO%SPTSEG(ISPT)%NBRANCH
             SWO%SPTSEG(ISPT)%BrcTFDR(J)      = DZ
             SWO%SPTSEG(ISPT)%BrcDELORDER(J)  = DZ
             SWO%SPTSEG(ISPT)%BrcDIVORDER(J)  = DZ
             SWO%SPTSEG(ISPT)%BrcDIVERSION(J) = DZ
             SWO%SPTSEG(ISPT)%BrcDELIVERY(J)  = DZ
             SWO%SPTSEG(ISPT)%BrcDELIVEFF(J)  = SWO%SPTSEG_PREV(ISPT)%BrcDELIVEFF(J)
             SWO%SPTSEG(ISPT)%BrcDSEEP(J)     = DZ
             SWO%SPTSEG(ISPT)%BrcDFLOW_IN(J)  = DZ
             SWO%SPTSEG(ISPT)%BrcDFLOW_RT(J)  = DZ
           END DO
         END DO
         !
         !!!! ****************************************************************
         !!!! COPY FARM DEMAND FROM FMP ...
         !!!! NOTE ... TFDR is defined/specified in FMP, USE'd here
         !!!!          TFDR is in units [L3/T] in FMP ...
         !!!!          Converted to [L3] here ...
         !!!! NOTE ... If first iteration, then use TFDR from end of previous step if
         !!!!          Otherwise, TFDR set to zero due to lag between FMP and SWOPS
         !!!IF (KITER.EQ.1) THEN
         !!!  DO IFARM = 1,NFARM
         !!!    SWO%FARM(IFARM)%TFDR = SWO%FARM_PREV(IFARM)%TFDR
         !!!  END DO
         !!!ELSE
         !!!  DO IFARM = 1,NFARM
         !!!    SWO%FARM(IFARM)%TFDR = TFDR(IFARM) * DDELT
         !!!  END DO !(IFARM)
         !!!END IF !(KITER.EQ.1)
         !
         DO CONCURRENT (I = 1:NFARM); SWO%FARM(I)%TFDR = SWO%FMP_SW_DEMAND(I) * SWO%DELT
         END DO
         !
    END IF !  SITER == ONE -----------------------------------------------------------------------------------------------------------------
    !
    ! SET UP ANY SFR INFLOWS
    !
    IF(KITER <= TEN) THEN !RELAXATION FACTOR FOR STREAM INFLOW
        DTMP = UNO
    ELSE
        DTMP = HALF  !0.5
    END IF
    !
    DO P = 1, SWO%NPROJ        !Check if RES has SFR Inflow to it
    DO J = 1, SWO%NRES_BAL(P) 
       ASSOCIATE( SFR_INFLOW => SWO%RESDAT(P)%RESBAL(J)%SFR_INFLOW,     &
                        ISTR => SWO%RESDAT(P)%RESBAL(J)%SFR_INFLOW_ISTRM )
           !
           IF( ISTR > Z) THEN
               !
               INFLOW = STRM(9, ISTR) * SWO%DELT  !Inflow to Reservoir from SFR
               !
               IF(DTMP == UNO) THEN
                   SFR_INFLOW = INFLOW                                 !NO RELAXATION
               ELSE
                   SFR_INFLOW = SFR_INFLOW + DTMP*(INFLOW-SFR_INFLOW)  !RELAX INFLOW BY DTMP
               END IF
           ELSE
                 SFR_INFLOW = DZ
           END IF
       END ASSOCIATE
    END DO
    END DO
    !
    !
    IF(TRUE) THEN !SITER == ONE
         !
         ! ****************************************************************
         ! ****************************************************************
         ! ****************************************************************
         ! ****************************************************************
         ! COMPUTE ALLOCATION
         ! (initial or updated allocation, depending on REALTIM of timestep)
         ! ****************************************************************
         ! ****************************************************************
         ! ****************************************************************
         ! ****************************************************************
         ! ****************************************************************

         ! NOTES:
         ! - Allocation based on total storage at END of PREVIOUS timestep ...
         ! - Allocation based on non-project storage for CURRENT timestep ...
         ! - Allocation based on year-to-date releases and conveyance parameters
         !   at END of PREVIOUS timestep
         ! **Allocations are made without any foresight or forecast information
         ! **Allocations are finalized retroactively at end-of-year close-out
         !
         ! - If timestep contains start/end of allocation year,
         !   ... computes allocation close-out for previous year
         !   ... computes initial allocation for current year
         !
         !
         UPDATE =               SWO%DEC_RUL_SP%INUSE .AND. KITER == TWO .AND. KSTP == ONE
         !
         UPDATE = UPDATE .OR.  (SWO%DEC_RUL_SIM%INUSE.AND. KITER == TWO .AND. KSTP==ONE .AND. KPER==ONE)
         !
         UPDATE = UPDATE .OR. ( KITER == TWO .AND. (SWO%DEC_RUL_TS%INUSE .OR. SWO%DEC_RUL_TS_END%INUSE .OR. SWO%DEC_RUL_CLOSEOUT%INUSE) )
         !
         UPDATE = UPDATE .OR. SWO%DEC_RUL_IT%INUSE .OR. SWO%DEC_RUL_IT_END%INUSE

         IF(SWO%PRT_RUL_SP%IS_OPEN .AND. KSTP==ONE .AND. KITER == TWO) WRITE(SWO%PRT_RUL_SP%IU,'(// A, // 16x, A,// 16x, A)') REPEAT('-', 150), '    FOR STRESS PERIOD '//NUM2STR(KPER),'WITH A STARTING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP-1)%PRETTYPRINT('  ')//' ('//DATE_SP(KPER)%TS(KSTP-1)%STR_DYEAR()//')'
         IF(SWO%PRT_RUL_TS%IS_OPEN                 .AND. KITER == TWO) WRITE(SWO%PRT_RUL_TS%IU,'(// A, // 16x, A,// 16x, A)') REPEAT('-', 150), '    FOR STRESS PERIOD '//NUM2STR(KPER,-6)//' AND TIME STEP '//NUM2STR(KSTP),'WITH A STARTING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP-1)%PRETTYPRINT('  ')//' AND ENDING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP)%PRETTYPRINT('  ')//'  ('//DATE_SP(KPER)%TS(KSTP-1)%STR_DYEAR()//' - '//DATE_SP(KPER)%TS(KSTP)%STR_DYEAR()//')'
         IF(SWO%PRT_RUL_IT%IS_OPEN                 .AND. KITER >= TWO) WRITE(SWO%PRT_RUL_TS%IU,'(// A, // 16x, A,// 16x, A)') REPEAT('-', 150), '    FOR STRESS PERIOD '//NUM2STR(KPER,-6)//' AND TIME STEP '//NUM2STR(KSTP)//' AND SOLVER ITERATION '//NUM2STR(KITER),'WITH A STARTING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP-1)%PRETTYPRINT('  ')//' AND ENDING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP)%PRETTYPRINT('  ')//'  ('//DATE_SP(KPER)%TS(KSTP-1)%STR_DYEAR()//' - '//DATE_SP(KPER)%TS(KSTP)%STR_DYEAR()//')'
         !
         IF(UPDATE) CALL VARIABLE_GET_GLOBAL_MODEL_PROPERTY(SWO%DEC_VAR, SWO%DEC_VAR%PROP_PUL, KPER, KSTP, KITER, Z, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES

         ! SET UP STRESS PERIOD BASED RULES -- MAYBE OVER RIDDEN BY TIME STEP OR ITERATION RULES
         !
         IF(SWO%DEC_RUL_SIM%INUSE.AND. KITER == TWO .AND. KSTP==ONE .AND. KPER==ONE)  CALL SWO%DEC_RUL_SIM%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_SIM%IU )
         IF(SWO%DEC_RUL_SP%INUSE .AND. KITER == TWO .AND. KSTP==ONE)  CALL SWO%DEC_RUL_SP%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_SP%IU )
         IF(SWO%DEC_RUL_TS%INUSE .AND. KITER == TWO                )  CALL SWO%DEC_RUL_TS%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_TS%IU )
         IF(SWO%DEC_RUL_IT%INUSE .AND. KITER >= TWO                )  CALL SWO%DEC_RUL_IT%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_IT%IU )
         !
         IF(UPDATE) CALL VARIABLE_SET_RETURN_VALUES(SWO%DEC_VAR, SWO%DEC_VAR%PROP_RET, KPER, KSTP, ERROR)
         !
         IF(SWO%PRT_RUL_SP%IS_OPEN) CALL SWO%PRT_RUL_SP%SIZE_CHECK()
         IF(SWO%PRT_RUL_TS%IS_OPEN) CALL SWO%PRT_RUL_TS%SIZE_CHECK()
         IF(SWO%PRT_RUL_IT%IS_OPEN) CALL SWO%PRT_RUL_IT%SIZE_CHECK()
         !
         !  SET UP ANY PARAMETERS THAT RELY ON DELT, S, CALCULATE POTENTIAL RESERVOIR RELEASES, AND SET UP HOW RELEASES ARE SPREAD ACROSS RESERVOIRS
         !
         UPDATE = UPDATE .OR. KITER == TWO
         !
         IF(UPDATE) THEN
             !
             CALL SET_ADJUSTIBLE_PROPERTIES(SWO, ERROR)  !HAVE TO CALL IT IRRELEVATNT OF S CAUSE DELT MIGHT CHANGE
             !
             IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//'THE FOLLOWING ARE EITHER VARIABLE NAMES NOT FOUND WHEN PULLING AN OWHM PROPERTY FOR THE S-LANGUAGE'//NL//'OR SETTING AN OWHM PROPERTY WITH THE S-LANGUAGE'//NL//ERROR)
             !
         END IF
         !
         IF( UPDATE .OR. (KITER > TWO .AND. (                              &
                                             SWO%RESBAL_INFLOW_SEG%INUSE   &
                                                                         ))&
           ) THEN
             !
             ! DETERMINE MAXIMUM POTENTIAL RELASE FROM RESERVOIR GIVEN CURRENT CONDITIONS
             ! (ASSUMING IT IS DRAINED TO NON-PROJECT STORAGE LEVELS)
             !
             !
             CALL SWO%RESDAT%SET_RELEASE_POT() !################################################
             !
             !
             !UPDATE ANY NECESSARY RESERVOIR RELEASE FRACTIONS --MAY DEPEND ON POTENTIAL RELEASE
             !
             DO P = ONE, SWO%NPROJ
                   IF(    SWO%NRES_BAL(P) < ONE  ) THEN !NO RES TO SET
                                                        CONTINUE
                   ELSEIF(SWO%NRES_BAL(P) < TWO  ) THEN
                                                        SWO%RESDAT(P)%RESBAL(ONE)%REL_DMD_FRAC = UNO
                   ELSE          ! BALANCE RESERVOIR PROPERTOES                   TOTAL FOR PROJECT P
                       ASSOCIATE(RESBAL=>SWO%RESDAT(P)%RESBAL, RELEASE_POT_TOT => SWO%RESDAT(P)%RELEASE_POT)
                           !
                           IF(RELEASE_POT_TOT > DZ) THEN
                               !
                               IF(RESBAL(ONE)%RELEASE_DMD_FRAC_INI < DZ) THEN
                                                                                  ITYP = NINT(RESBAL(ONE)%RELEASE_DMD_FRAC_INI)
                               ELSE
                                                                                  ITYP = Z
                               END IF
                               !
                               ITYP = NINT(RESBAL(ONE)%RELEASE_DMD_FRAC_INI)
                               !
                               SELECT CASE(ITYP)  !ONLY UPDATE IF = -2, -3, -4, or -5
                               CASE(-2) ! BY_POTENTIAL_RELEASE
                                           DO J = ONE, SWO%NRES_BAL(P)
                                                                     RESBAL(J)%REL_DMD_FRAC = RESBAL(J)%RELEASE_POT / RELEASE_POT_TOT
                                           END DO
                               CASE(-3) ! BY_AVAILABLE_RELEASE
                                           DTMP = RELEASE_POT_TOT
                                           !
                                           DO J = ONE, SWO%NRES_BAL(P)
                                                                     DTMP = DTMP - RESBAL(J)%RELEASE_SPEC
                                           END DO
                                           !
                                           IF( DTMP.LE.DZ ) THEN
                                                DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));    RESBAL(J)%REL_DMD_FRAC = DZ
                                                END DO
                                           ELSE
                                                DO J = ONE, SWO%NRES_BAL(P)
                                                                          RESBAL(J)%REL_DMD_FRAC = (RESBAL(J)%RELEASE_POT - RESBAL(J)%RELEASE_SPEC)/ DTMP
                                                END DO
                                           END IF
                                           !
                               CASE(-4 ) ! BY_STORAGE
                                           DTMP = DZ
                                           !
                                           DO J=ONE, SWO%NRES_BAL(P)
                                              IF( RESBAL(J)%STORAGE_PREV > DZ ) DTMP = DTMP + RESBAL(J)%STORAGE_PREV
                                           END DO
                                           !
                                           IF( DTMP.LE.DZ ) THEN
                                                DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));    RESBAL(J)%REL_DMD_FRAC = DZ
                                                END DO
                                           ELSE
                                                DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));    RESBAL(J)%REL_DMD_FRAC = RESBAL(J)%STORAGE_PREV/ DTMP
                                                END DO
                                           END IF
                               CASE(-5 ) ! BY_USABLE_STORAGE
                                           DTMP = DZ
                                           !
                                           DO J = ONE, SWO%NRES_BAL(P)
                                                                     DTMP = DTMP + ZERO_OR_GREATER( RESBAL(J)%STORAGE_PREV - RESBAL(J)%STORAGE_MIN )
                                           END DO
                                           !
                                           IF( DTMP.LE.DZ ) THEN
                                                DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));    RESBAL(J)%REL_DMD_FRAC = DZ
                                                END DO
                                           ELSE
                                                DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));    RESBAL(J)%REL_DMD_FRAC = (RESBAL(J)%STORAGE_PREV - RESBAL(J)%STORAGE_MIN)/ DTMP
                                                END DO
                                           END IF
                                 CASE(0) ! Release fractions are specified by user
                                           DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));    RESBAL(J)%REL_DMD_FRAC = RESBAL(J)%RELEASE_DMD_FRAC_INI
                                           END DO
                               END SELECT
                               !
                               DO CONCURRENT (J = ONE:SWO%NRES_BAL(P), RESBAL(J)%REL_DMD_FRAC < DZ);    RESBAL(J)%REL_DMD_FRAC = DZ
                               END DO
                           ELSE
                               DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));  RESBAL(J)%REL_DMD_FRAC = DZ
                               END DO
                           END IF
                       END ASSOCIATE
                   END IF
             END DO
             !
             !---------------------------------------------------------------------------------------------------------------
             !
             IF(SWO%REQFLOW%HAS_REQ ) THEN
                   !
                   DO IREQ = ONE, SWO%REQFLOW%N
                      !
                      ASSOCIATE(N => SWO%REQFLOW%RES(IREQ)%N, RES => SWO%REQFLOW%RES(IREQ)%VEC)
                         !
                         IF(    N == Z   ) THEN !NO RES TO SET
                                               CONTINUE
                         ELSEIF(N == ONE ) THEN
                             I = SWO%RESBAL2PROJ(ONE,RES(ONE))
                             J = SWO%RESBAL2PROJ(TWO,RES(ONE))
                             SWO%RESDAT(I)%RESBAL(J)%REL_REQ_FRAC = UNO
                         ELSE
                             ASSOCIATE(RESDAT=>SWO%RESDAT, RELEASE_POT_TOT => SWO%RESDAT(:)%RELEASE_POT)
                                 !
                                 I = SWO%RESBAL2PROJ(ONE,RES(ONE))
                                 J = SWO%RESBAL2PROJ(TWO,RES(ONE))
                                 !
                                 IF(RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI < DZ) THEN
                                                                                    ITYP = NINT(RESDAT(I)%RESBAL(J)%RELEASE_REQ_FRAC_INI)
                                 ELSE
                                                                                    ITYP = Z
                                 END IF
                                 !
                                 SELECT CASE(ITYP)  !ONLY UPDATE IF = 2, 3, 4, or 5
                                 !
                                 CASE(-2) ! BY_POTENTIAL_RELEASE
                                             DTMP = DZ
                                             !
                                             DO K=ONE, N
                                                   ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                     !
                                                     DTMP = DTMP + RESDAT(II)%RESBAL(JJ)%RELEASE_POT
                                                     !
                                                   END ASSOCIATE
                                             END DO
                                             !
                                             IF( DTMP.LE.DZ ) THEN
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = DZ
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             ELSE
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = RESDAT(II)%RESBAL(JJ)%RELEASE_POT / DTMP
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             END IF
                                 CASE(-3) ! BY_AVAILABLE_RELEASE
                                             DTMP = DZ
                                             !
                                             DO K=ONE, N
                                                   ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                     !
                                                     DTMP = DTMP +  ZERO_OR_GREATER( RESDAT(II)%RESBAL(JJ)%RELEASE_POT - RESDAT(II)%RESBAL(JJ)%RELEASE_SPEC )
                                                     !
                                                   END ASSOCIATE
                                             END DO
                                             !
                                             IF( DTMP.LE.DZ ) THEN
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = DZ
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             ELSE
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = ( RESDAT(II)%RESBAL(JJ)%RELEASE_POT - RESDAT(II)%RESBAL(JJ)%RELEASE_SPEC ) / DTMP
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             END IF
                                             !
                                 CASE(-4) ! BY_STORAGE
                                             DTMP = DZ
                                             !
                                             DO K=ONE, N
                                                   ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                     !
                                                     DTMP = DTMP +  RESDAT(II)%RESBAL(JJ)%STORAGE_PREV
                                                     !
                                                   END ASSOCIATE
                                             END DO
                                             !
                                             IF( DTMP.LE.DZ ) THEN
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = DZ
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             ELSE
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = RESDAT(II)%RESBAL(JJ)%STORAGE_PREV / DTMP
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             END IF
                                 CASE(-5) ! BY_USABLE_STORAGE
                                             DTMP = DZ
                                             !
                                             DO K=ONE, N
                                                   ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                     !
                                                     DTMP = DTMP +  ZERO_OR_GREATER( RESDAT(II)%RESBAL(JJ)%STORAGE_PREV - RESDAT(II)%RESBAL(JJ)%STORAGE_MIN )
                                                     !
                                                   END ASSOCIATE
                                             END DO
                                             !
                                             IF( DTMP.LE.DZ ) THEN
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = DZ
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             ELSE
                                                 DO K=ONE, N
                                                       ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                         !
                                                         RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = ( RESDAT(II)%RESBAL(JJ)%STORAGE_PREV - RESDAT(II)%RESBAL(JJ)%STORAGE_MIN ) / DTMP
                                                         !
                                                       END ASSOCIATE
                                                 END DO
                                             END IF
                                 CASE(0) ! Release fractions are specified by user
                                             DO K=ONE, N
                                                   ASSOCIATE(II => SWO%RESBAL2PROJ(ONE,RES(K)), JJ => SWO%RESBAL2PROJ(TWO,RES(K)))
                                                     !
                                                     RESDAT(II)%RESBAL(JJ)%REL_REQ_FRAC = RESDAT(II)%RESBAL(JJ)%RELEASE_REQ_FRAC_INI
                                                     !
                                                   END ASSOCIATE
                                             END DO
                                 END SELECT
                                 !
                             END ASSOCIATE
                             !
                         END IF
                      END ASSOCIATE
                   END DO
                   !
                   DO CONCURRENT (P = ONE:SWO%NPROJ)
                   DO CONCURRENT (J = ONE:SWO%NRES_BAL(P), SWO%RESDAT(P)%RESBAL(J)%REL_REQ_FRAC < DZ) 
                                                                                                      SWO%RESDAT(P)%RESBAL(J)%REL_REQ_FRAC = DZ
                   END DO
                   END DO
             END IF ! (SWO%REQFLOW%HAS_REQ )
             !
         END IF    ! (UPDATE .OR. KITER == TWO .OR. SWO%RESBAL_INFLOW_SEG%INUSE)
         !
    END IF !SITER == 1
    !
    !CHECK FOR REQUIRED DELIERVERY RATES
    !
    DO CONCURRENT (I = 1:NFARM, SWO%FARM(I)%TFDR < SWO%FARM(I)%REQ_DELIVERY_VOL)  !REQUESTED DEMAND IS LESS THEN REQURIED DELIVERY, INCREASE SWO DEMAND (TFDR) TO PUSH WATER THROUGH, WHICH FMP WILL NOT USE AND IT WILL BY PASS
          !
          SWO%FARM(I)%TFDR = SWO%FARM(I)%REQ_DELIVERY_VOL
    END DO
    !
    ! SET UP ALLOTMENT CONSTRAINTS
    !
    IF( SWO%HAS_FMP_SW_LIMIT_RULZ ) THEN
        DO CONCURRENT (I=ONE:NFARM)
           IF( SWO%FMP_SW_LIMIT_RULZ(I) < D100) THEN
             SWO%FMP_SW_LIMIT(I) = SWO%FMP_SW_LIMIT_RULZ(I)*SWO%DELT  !CONVERT TO VOLUME
           ELSE
             SWO%FMP_SW_LIMIT(I) = inf
           END IF
        END DO
    ELSE
             SWO%FMP_SW_LIMIT = inf
    END IF
    !
    DO CONCURRENT (I=ONE:NFARM, SWO%FMP_SW_ALLOTMENT(I) < SWO%FMP_SW_LIMIT(I));  SWO%FMP_SW_LIMIT(I) = SWO%FMP_SW_ALLOTMENT(I)
    END DO
    !
    ! CHECK FOR FORCED DELIVERY WHICH OVERRIDES ALLOWMENTS
    !
    DO CONCURRENT (I=ONE:NFARM, SWO%FMP_SW_LIMIT(I) < SWO%FARM(I)%REQ_DELIVERY_VOL);  SWO%FMP_SW_LIMIT(I) = SWO%FARM(I)%REQ_DELIVERY_VOL
    END DO
    !
    ! ****************************************************************
    ! ****************************************************************
    ! ****************************************************************
    ! ****************************************************************
    ! COMPUTE ALLOCATION
    ! (initial or updated allocation, depending on REALTIM of timestep)
    ! ****************************************************************
    ! ****************************************************************
    ! ****************************************************************
    ! ****************************************************************
    ! ****************************************************************
    !
    ! ****************************************************************
    ! COMPUTE DISTRIBUTION OF DISTRICT ALLOCATION ...
    ! (1) Compute allotment to lands
    !     Uniform allotment per unit area of district
    ! (2) Compute allotment to farms, aux demands
    !     Farm area x uniform allotment
    !     Aux area  x uniform allotment
    ! (3) Compute allotment balance of each farm, aux demand
    !     Balance = allotment - delivery_YTD
    ! (4) Compute delivery request to farms/aux, units, districts, projects
    !     DelReq = min( TFDR, Balance )
    ! (5) Compute diversion request to units, districts, projects
    !     DivReq = DelReq/DelivEff
    ! (6) Compute diversion request to diversion segments
    !     DivReq = sum(DelReq)/DelivEff
    !     NOTE -- Computed based on diversion segments,
    !             not computed for individual units ...
    !             Required for cases where bypass occurs
    !             (i.e., where multiple units served by common diversion segment)
    ! (7) Compute diversion requests throughout network
    ! ****************************************************************

    ! (1) Compute allotment to lands ...
    !     ALLOTMENT(DIST) = ALLOCATION(DIST)
    !                       x DelivEff(DIST)              --> account for losses or gains
    !                       x (1/NetChgRatio(DIST))       --> account for wet water vs. accounting
    !                       x (1/AreaIrr(DIST))           --> distribute equally to all IRRIGATED lands in district
    !                                                         (NOTE: AreaIrr(DIST) includes farm area + area associated with each aux demand!)
    !
    !
    ASSOCIATE( PROJ=>SWO%PROJ, DIST => SWO%DIST, UNIT=>SWO%UNIT, FARM=>SWO%FARM,                     &
               FARM_PREV=>SWO%FARM_PREV, AUXDEM=>SWO%AUXDEM, AUXDEM_PREV=>SWO%AUXDEM_PREV,           &
               SEGINFO=>SWO%SEGINFO, SEGDATA=> SWO%SEGDATA,                                          &
               DIVSEG=>SWO%DIVSEG,  DIVSEG_PREV=>SWO%DIVSEG_PREV, SPTSEG=>SWO%SPTSEG,                &
               DIST_DELTOL=>SWO%DIST_DELTOL, SEGRCH_IN=>SWO%SEGRCH_IN, SEGRCH_OUT=>SWO%SEGRCH_OUT )
      !
      IF(.TRUE.) THEN !SITER == ONE-----------------------------------------------------------------------------------------------------------------
        DO I = 1,NDIST
          !
          CHARGE_CREDIT = DIST(I)%CHARGE_YTD - DIST(I)%CREDIT_YTD
          !
          DIST(I)%BALANCE = DIST(I)%ALLOC_TOTAL - CHARGE_CREDIT  !<= DIST(I)%BALANCE = DIST(I)%ALLOC_TOTAL - DIST(IDIST)%CHARGE_YTD + DIST(I)%CREDIT_YTD
          !
          EFF = DIST(I)%DELIVEFF
          IF(EFF > UNO) EFF = UNO
          !
          IF      (DIST(I)%S_ALLOTMENT_BYBEN) THEN
                                                   DTMP = DZ
                                                   DO F = ONE, DIST(I)%NFARM
                                                             DTMP = DTMP + SWO%FARM( DIST(I)%FARM(F) )%S_ALLOTMENT
                                                   END DO
                                                   !
                                                   DO J = ONE, DIST(I)%NAUXDEM
                                                             DTMP = DTMP + SWO%AUXDEM( DIST(I)%AUXDEM(J) )%S_ALLOTMENT
                                                   END DO
                                                   !
                                                   ALLOTMENT = DTMP * SWO%DELT
                                                   !
                                                   DIST(I)%BALANCE      = ALLOTMENT
                                                   DIST(I)%EQ_ALLOTMENT = ALLOTMENT + DIST(I)%DELIVERY_YTD
                                                   DIST(I)%ALLOC_TOTAL  = ALLOTMENT + CHARGE_CREDIT
          ELSE IF(DIST(I)%S_ALLOTMENT.GE.DZ) THEN
                                                   ALLOTMENT = DIST(I)%S_ALLOTMENT * SWO%DELT ! S Allotment Override
                                                   !
                                                   DIST(I)%BALANCE      = ALLOTMENT
                                                   DIST(I)%EQ_ALLOTMENT = ALLOTMENT + DIST(I)%DELIVERY_YTD !* (UNO / DIST(I)%NETCHGRATIO) * EFF
                                                   DIST(I)%ALLOC_TOTAL  = ALLOTMENT + CHARGE_CREDIT
          ELSE IF(DIST(I)%S_ALLOTMENT_VOL.GE.DZ) THEN
                                                   ALLOTMENT = DIST(I)%S_ALLOTMENT_VOL   ! S Allotment Override
                                                   !
                                                   DIST(I)%BALANCE      = ALLOTMENT
                                                   DIST(I)%EQ_ALLOTMENT = ALLOTMENT + DIST(I)%DELIVERY_YTD   !* (UNO / DIST(I)%NETCHGRATIO) * EFF
                                                   DIST(I)%ALLOC_TOTAL  = ALLOTMENT + CHARGE_CREDIT
          ELSE IF (DIST(I)%ALLOC_TOTAL.LE.DZ) THEN                        ! No allocation --> no eq. allotment
                                                   DIST(I)%EQ_ALLOTMENT = DZ
          ELSE IF (DIST(I)%AreaIrr.EQ.DZ) THEN                       ! No irrigated acreage --> no eq. allotment
                                                   DIST(I)%EQ_ALLOTMENT = DZ
          ELSE IF (DIST(I)%BALANCE.LE.DZ) THEN                       ! District already used full diversion allocation --> use previous eq. allotment
                                                   DIST(I)%EQ_ALLOTMENT = SWO%DIST_PREV(I)%EQ_ALLOTMENT
          !ELSE IF (DIST(I)%DELIVEFF    > 0.99  .AND.  &
          !         DIST(I)%NETCHGRATIO < 0.25D0 ) THEN                ! System is too efficient, which is causing low charges and high credits -- Use total NetCHARg
          !                                         DIST(I)%EQ_ALLOTMENT = SWO%DIST_PREV(I)%EQ_ALLOTMENT
          ELSE
            !
            DIST(I)%EQ_ALLOTMENT = DIST(I)%DELIVERY_YTD                            &      ! water already delivered ...
                                     + ZERO_OR_GREATER(                            &      ! estimated additional water available for delivery, assuming current delivery efficiency ...
                                           ( DIST(I)%ALLOC_TOTAL - CHARGE_CREDIT ) &      ! Remaining allocation (alloc - charge + credit)
                                              * (UNO / DIST(I)%NETCHGRATIO)        &      ! x (1 / current-step net charge ratio)
                                              * EFF                             )         ! x (current-step delivery efficiency)
              !
            !!DIST(I)%EQ_ALLOTMENT = DIST(I)%DELIVERY_YTD / DIST(I)%AreaIrr    & ! water already delivered ...
            !!                         + ZERO_OR_GREATER(                                  &                                             ! estimated additional water available for delivery, assuming current delivery efficiency ...
            !!                               ( DIST(I)%ALLOC_TOTAL - DIST(I)%CHARGE_YTD + DIST(I)%CREDIT_YTD ) &    ! Remaining allocation (alloc - charge + credit)
            !!                                  * (UNO / DIST(I)%NETCHGRATIO) &         ! x (1 / current-step net charge ratio)
            !!                                  * DIST(I)%DELIVEFF            &         ! x (current-step delivery efficiency)
            !!                                  * (UNO / DIST(I)%AreaIrr) )             ! x (1 / area)
              !
              !IF(MAXALLOT < DIST(I)%EQ_ALLOTMENT) DIST(I)%EQ_ALLOTMENT = MAXALLOT! Constrain US allotment to MAXALLOT AS A HIEGHT (100 UNITS HIGH!)
              !
          END IF !(ALLOC_TOT.EQ.0 / AreaTot.EQ.0 / ELSE)
          !
          ! CHECK FOR ALLOTMENT LIMITS - EQ_ALLOTMENT_LIMIT IS A MULTIPLIER THAT DETMINERS THE MAX EQ_ALLOT WITH REGARDS TO ALLOC_TOT
          IF(UNO < SWO%DIST(I)%EQ_ALLOTMENT_LIMIT .AND. SWO%DIST(I)%EQ_ALLOTMENT_LIMIT < D29) THEN
                                                           DTMP = DIST(I)%ALLOC_TOTAL * SWO%DIST(I)%EQ_ALLOTMENT_LIMIT
                                                           !
                                                           IF(DIST(I)%EQ_ALLOTMENT > DTMP) THEN
                                                              DIST(I)%EQ_ALLOTMENT = DTMP
                                                           END IF
          END IF
        END DO !(IDIST)
        !
        ! RESET TOTAL ALLOTMENT ASSIGNED TO EACH UNIT -- Only for book keeping. Will sum from benicfiaries allotments
        !
        DO CONCURRENT (I=ONE:NUNIT)
          SWO%UNIT(I)%ALLOTMENT = DZ
          SWO%UNIT(I)%BALANCE   = DZ
        END DO
        !
        !
        ! (2) Compute allotment to farms, aux demands ...
        !     ALLOTMENT(FARM) = ALLOTMENT(DIST) * AreaTot(FARM)
        !     ALLOTMENT(AUX)  = ALLOTMENT(DIST) * AreaTot(Aux)
        DO F = ONE, NFARM
          IF(FARM(F)%DistID>Z .AND. FARM(F)%DelSeg>Z) THEN ! Must have delivery segment
              !
              I = FARM(F)%DistID
              J = FARM(F)%UnitID
              !
              IF(     DIST(I)%S_ALLOTMENT_BYBEN) THEN
                                                     FARM(F)%ALLOTMENT = SWO%FARM( F )%S_ALLOTMENT  * SWO%DELT + FARM(F)%DELIVERY_YTD
              ELSE IF(DIST(I)%S_ALLOTMENT.GE.DZ) THEN
                                                     FARM(F)%ALLOTMENT = DIST(I)%S_ALLOTMENT * SWO%DELT * FARM(F)%DIST_ALLOC_FRAC + FARM(F)%DELIVERY_YTD
              ELSE IF(DIST(I)%S_ALLOTMENT_VOL.GE.DZ) THEN
                                                     FARM(F)%ALLOTMENT = DIST(I)%S_ALLOTMENT_VOL    * FARM(F)%DIST_ALLOC_FRAC + FARM(F)%DELIVERY_YTD
              ELSE
                    FARM(F)%ALLOTMENT = DIST(I)%EQ_ALLOTMENT * FARM(F)%DIST_ALLOC_FRAC
              END IF
              !
              ! District already used full diversion allocation
              IF (DIST(I)%BALANCE.LE.DZ) FARM(F)%ALLOTMENT = FARM(F)%DELIVERY_YTD        ! --> Set allotment equal to deliveries year-to-date ... farms can't call for additional water.
              !
              UNIT(J)%ALLOTMENT = UNIT(J)%ALLOTMENT + FARM(F)%ALLOTMENT
          END IF
        END DO

        !DO IFARM = 1,NFARM
        !  IDIST = FARM(IFARM)%DistID
        !  IF (IDIST.EQ.0) THEN                                       ! No district
        !    FARM(IFARM)%ALLOTMENT = DZ                              ! --> Farm not in a district ... farm doesn't get SW delivery
        !  ELSE IF (DIST(IDIST)%BALANCE.LE.DZ) THEN                  ! District already used full diversion allocation
        !    FARM(IFARM)%ALLOTMENT = FARM(IFARM)%DELIVERY_YTD         ! --> Set allotment equal to deliveries year-to-date ... farms can't call for additional water.
        !  ELSE IF (FARM(IFARM)%DelSeg.EQ.0) THEN                     ! No farm delivery segment
        !    FARM(IFARM)%ALLOTMENT = DZ                              ! --> Set allotment to zero ... farm doesn't get SW delivery
        !  ELSE
        !    FARM(IFARM)%ALLOTMENT = DIST(IDIST)%EQ_ALLOTMENT * FARM(IFARM)%DIST_ALLOC_FRAC !FARM(IFARM)%AreaIrr
        !  END IF !(IDIST.NE.0)
        !END DO !(IFARM)
        !
        !
        DO IAUX=1, NAUXDEM
           IF(AUXDEM(IAUX)%DistID>Z .AND. AUXDEM(IAUX)%AuxSeg>Z) THEN
              I = AUXDEM(IAUX)%DistID
              J = AUXDEM(IAUX)%UnitID
              IF(     DIST(I)%S_ALLOTMENT_BYBEN) THEN
                   AUXDEM(IAUX)%ALLOTMENT = SWO%AUXDEM( IAUX )%S_ALLOTMENT * SWO%DELT + (AUXDEM(IAUX)%DELIVERY_YTD*AUXDEM(IAUX)%FACTOR)
              ELSEIF (DIST(I)%BALANCE.LE.0) THEN                           ! District already used full diversion allocation
                   AUXDEM(IAUX)%ALLOTMENT = AUXDEM(IAUX)%DELIVERY_YTD*AUXDEM(IAUX)%FACTOR      ! --> Set allotment equal to deliveries year-to-date ... auxs can't call for additional water.
              ELSE
                   AUXDEM(IAUX)%ALLOTMENT = DIST(I)%EQ_ALLOTMENT * SWO%AUXDEM(IAUX)%DIST_ALLOC_FRAC !AUXDEM(IAUX)%AREA
              END IF
              UNIT(J)%ALLOTMENT = UNIT(J)%ALLOTMENT + AUXDEM(IAUX)%ALLOTMENT
           END IF
        END DO
        !DO IAUX = 1,NAUXDEM
        !  IDIST = AUXDEM(IAUX)%DistID
        !  IF (IDIST.EQ.0) THEN                                      ! No district
        !    AUXDEM(IAUX)%ALLOTMENT = DZ                            ! --> Set allotment to zero ... aux doesn't get SW delivery
        !  ELSE IF (DIST(IDIST)%BALANCE.LE.0) THEN                   ! District already used full diversion allocation
        !    AUXDEM(IAUX)%ALLOTMENT = AUXDEM(IAUX)%DELIVERY_YTD      ! --> Set allotment equal to deliveries year-to-date ... auxs can't call for additional water.
        !  ELSE IF (AUXDEM(IAUX)%AuxSeg.EQ.0) THEN                   ! No aux segment
        !    AUXDEM(IAUX)%ALLOTMENT = DZ                            ! --> Set allotment to zero ... aux doesn't get SW delivery
        !  ELSE
        !    AUXDEM(IAUX)%ALLOTMENT = DIST(IDIST)%EQ_ALLOTMENT * SWO%AUXDEM(IAUX)%DIST_ALLOC_FRAC !AUXDEM(IAUX)%AREA
        !  END IF !(IDIST.NE.0)
        !END DO !(IAUX)
        !
        !
        !
        ! (3) Compute allotment balance for each farm, aux demand ...
        !     BALANCE(FARM) = ALLOTMENT(FARM) - DELIVERY_YTD(FARM)
        !     BALANCE(AUX)  = ALLOTMENT(AUX)  - DELIVERY_YTD(AUX)
        DO F=1, NFARM
        IF(FARM(F)%DistID>Z .AND. FARM(F)%DelSeg>Z) THEN
          !
          I = FARM(F)%DistID
          J = FARM(F)%UnitID
          !
          IF     (DIST(I)%S_ALLOTMENT_BYBEN) THEN
                                                   FARM(F)%BALANCE = SWO%FARM( DIST(I)%FARM(F) )%S_ALLOTMENT * SWO%DELT
          ELSE IF(DIST(I)%S_ALLOTMENT.GE.DZ) THEN
                                                   FARM(F)%BALANCE = DIST(I)%S_ALLOTMENT     * FARM(F)%DIST_ALLOC_FRAC * SWO%DELT
          ELSE IF(DIST(I)%S_ALLOTMENT_VOL.GE.DZ) THEN
                                                   FARM(F)%BALANCE = DIST(I)%S_ALLOTMENT_VOL * FARM(F)%DIST_ALLOC_FRAC
          ELSE
                                                   FARM(F)%BALANCE = FARM(F)%ALLOTMENT-FARM(F)%DELIVERY_YTD
          ENDIF
          !
          IF(FARM(F)%BALANCE < NEARZERO_5) FARM(F)%BALANCE = DZ
          !
          UNIT(J)%BALANCE = UNIT(J)%BALANCE + FARM(F)%BALANCE
        END IF
        END DO
        !
        !
        DO IAUX=1, NAUXDEM
          IF(AUXDEM(IAUX)%DistID>Z .AND. AUXDEM(IAUX)%AuxSeg>Z) THEN
             AUXDEM(IAUX)%BALANCE = AUXDEM(IAUX)%ALLOTMENT - (AUXDEM(IAUX)%DELIVERY_YTD*AUXDEM(IAUX)%FACTOR)
             IF(AUXDEM(IAUX)%BALANCE < NEARZERO_5) AUXDEM(IAUX)%BALANCE = DZ
             !
             UNIT(AUXDEM(IAUX)%UnitID)%BALANCE = UNIT(AUXDEM(IAUX)%UnitID)%BALANCE + AUXDEM(IAUX)%BALANCE
          END IF
        END DO
        !
        ! (4) Compute delivery request to farms/auxs, units, districts, projects
        !     DELREQ(FARM) = MIN( TFDR(FARM), BALANCE(FARM) )
        !     DELREQ(AUX)  = MIN( DEMAND(AUX),BALANCE(AUX) )
        DO F=1, NFARM
          IF(FARM(F)%DelSeg>Z .AND. FARM(F)%UnitID>Z .AND. FARM(F)%TFDR>NEARZERO_6 .AND.  FARM(F)%BALANCE>NEARZERO_6) THEN
               IUNIT = FARM(F)%UnitID
               IDIST = FARM(F)%DistID
               IPROJ = FARM(F)%ProjID
               !
               ! Iteration spin-up period ...
               IF (KITER.LE.SWO%SWMXITER(1) ) THEN            !.OR. SWO%FMP_SW_DEMAND(F) * DELT <= SWO%FARM(F)%REQ_DELIVERY_VOL
                 FARM(F)%DELORDER = MIN( FARM(F)%TFDR, FARM(F)%BALANCE, SWO%FMP_SW_LIMIT(F) )
                 !
               ! Iteration stagger or hold period ...
               ELSEIF(FARM(F)%DEL_TOL_CNT >= 25) THEN
                                                     FARM(F)%DELORDER = DZ
               ELSE
                 DTMP1 = FARM_PREV(F)%DELORDER
                 DTMP2 = FARM_PREV(F)%DELIVERY
                 IF (DTMP1.NE.DZ) THEN
                   DTMP3 = DTMP2/DTMP1
                 ELSE
                   DTMP3 = DZ
                 END IF !(DTMP2.EQ.DZ)
                 ! SCOTT THIS USED TO CHECK FOR IDIST.NE.3

                 IF ( DTMP3.LT.DIST_DELTOL(IDIST) .AND. FARM(F)%TFDR > SWO%FARM(F)%REQ_DELIVERY_VOL ) THEN  !Required Deliv not in effect and loses too much water
                   FARM(F)%DEL_TOL_CNT =  FARM(F)%DEL_TOL_CNT + ONE
                 ELSE
                   FARM(F)%DELORDER = MIN( FARM(F)%TFDR,  FARM(F)%BALANCE, SWO%FMP_SW_LIMIT(F) )
                 END IF !(DTMP1.EQ.0 | DTMP1/DTMP2.LT.0.5)
               END IF !(KITER.LE.SWMXITER(1))
               !
               ! Impose minimum delivery order ...
               IF ( FARM(F)%DELORDER.LT.SWO%MIN_DELORDER(F) ) FARM(F)%DELORDER = DZ
               !
               UNIT(IUNIT)%TFDR      = UNIT(IUNIT)%TFDR + FARM(F)%TFDR
               UNIT(IUNIT)%DELORDER  = UNIT(IUNIT)%DELORDER + FARM(F)%DELORDER
               !
               DIST(IDIST)%TFDR      = DIST(IDIST)%TFDR + FARM(F)%TFDR
               DIST(IDIST)%DELORDER  = DIST(IDIST)%DELORDER + FARM(F)%DELORDER
               !
               PROJ(IPROJ)%TFDR      = PROJ(IPROJ)%TFDR + FARM(F)%TFDR
               PROJ(IPROJ)%DELORDER  = PROJ(IPROJ)%DELORDER + FARM(F)%DELORDER
          ELSE
              FARM(F)%DELORDER = DZ
          END IF
        END DO
        !DO IFARM = 1,NFARM
        !  !
        !  IUNIT = FARM(IFARM)%UnitID
        !  IDIST = FARM(IFARM)%DistID
        !  IPROJ = FARM(IFARM)%ProjID
        !  IF (FARM(IFARM)%DelSeg.EQ.0) THEN
        !    FARM(IFARM)%DELORDER = DZ                                    ! GW only farm!
        !  ELSE
        !    ! Iteration spin-up period ...
        !    IF (KITER.LE.SWO%SWMXITER(1)) THEN
        !      FARM(IFARM)%DELORDER = MIN( FARM(IFARM)%TFDR, FARM(IFARM)%BALANCE, SWO%FMP_SW_LIMIT(IFARM) )
        !      !
        !    ! Iteration stagger or hold period ...
        !    ELSE
        !      DTMP1 = FARM_PREV(IFARM)%DELORDER
        !      DTMP2 = FARM_PREV(IFARM)%DELIVERY
        !      IF (DTMP1.NE.DZ) THEN
        !        DTMP3 = DTMP2/DTMP1
        !      ELSE
        !        DTMP3 = DZ
        !      END IF !(DTMP2.EQ.DZ)
        !      ! SCOTT THIS USED TO CHECK FOR IDIST.NE.3
        !      IF (DTMP1 < SWO%MIN_DELORDER(IFARM) .OR. DTMP3.LT.DIST_DELTOL(IDIST) ) THEN
        !        FARM(IFARM)%DELORDER = DZ
        !      ELSE
        !        FARM(IFARM)%DELORDER = MIN( FARM(IFARM)%TFDR,  FARM(IFARM)%BALANCE, SWO%FMP_SW_LIMIT(IFARM) )
        !      END IF !(DTMP1.EQ.0 | DTMP1/DTMP2.LT.0.5)
        !    END IF !(KITER.LE.SWMXITER(1))
        !    !
        !    ! Impose minimum delivery order ...
        !    IF ( FARM(IFARM)%DELORDER.LT.SWO%MIN_DELORDER(IFARM) ) FARM(IFARM)%DELORDER = DZ
        !  END IF !(DelSeg.EQ.0)
        !  !
        !  IF (IUNIT.NE.0) THEN
        !    UNIT(IUNIT)%TFDR      = UNIT(IUNIT)%TFDR + FARM(IFARM)%TFDR
        !    UNIT(IUNIT)%DELORDER  = UNIT(IUNIT)%DELORDER + FARM(IFARM)%DELORDER
        !  END IF !(IUNIT.NE.0)
        !
        !  IF (IDIST.NE.0) THEN
        !    DIST(IDIST)%TFDR      = DIST(IDIST)%TFDR + FARM(IFARM)%TFDR
        !    DIST(IDIST)%DELORDER  = DIST(IDIST)%DELORDER + FARM(IFARM)%DELORDER
        !  END IF !(IDIST.NE.0)
        !
        !  IF (IPROJ.NE.0) THEN
        !    PROJ(IPROJ)%TFDR      = PROJ(IPROJ)%TFDR + FARM(IFARM)%TFDR
        !    PROJ(IPROJ)%DELORDER  = PROJ(IPROJ)%DELORDER + FARM(IFARM)%DELORDER
        !  END IF !(IPROJ.NE.0)
        !  !
        !END DO !(IFARM)
        I     = -1
        J     = -1
        IPROJ = -1
        IRES  = -1
        ISEG  = -1
        IRCH  = -1
        IDVS  = -1
        ISPT  = -1
        IBRC  = -1
        IRES  = -1
        JRES  = -1
        IPROJ = -1
        IDIST = -1
        IUNIT = -1
        IFARM = -1
        IAUX  = -1
             !
             !
             DO IAUX=1, NAUXDEM
                 IF(AUXDEM(IAUX)%AuxSeg>Z .AND. AUXDEM(IAUX)%UnitID>Z) THEN
                    IUNIT = AUXDEM(IAUX)%UnitID
                    IDIST = AUXDEM(IAUX)%DistID
                    IPROJ = AUXDEM(IAUX)%ProjID
                    !
                    ! Iteration spin-up period ...
                    IF (KITER.LE.SWO%SWMXITER(1)) THEN
                      AUXDEM(IAUX)%DELORDER = MIN( AUXDEM(IAUX)%DEMAND, AUXDEM(IAUX)%BALANCE, AUXDEM(IAUX)%DEMAND_LIM ) / AUXDEM(IAUX)%FACTOR ! Apply factor here ... Delivery tracked at aux seg, balance tracked at off-grid aux location
                      !
                      ! Iteration stagger or hold period ...
                    ELSEIF(AUXDEM(IAUX)%DEL_TOL_CNT >= 25) THEN
                                                          AUXDEM(IAUX)%DELORDER = DZ
                    ELSE
                      DTMP1 = AUXDEM_PREV(IAUX)%DELORDER
                      DTMP2 = AUXDEM_PREV(IAUX)%DELIVERY
                      IF (DTMP1.NE.DZ) THEN
                        DTMP3 = DTMP2/DTMP1
                      ELSE
                        DTMP3 = DZ
                      END IF !(DTMP2.EQ.DZ)
                      ! SCOTT DO NOT CURTAIL MEXICO IDIST.NE.3
                      IF ( DTMP1.EQ.DZ .OR. DTMP3.LT.DIST_DELTOL(IDIST) ) THEN
                        AUXDEM(IAUX)%DEL_TOL_CNT = AUXDEM(IAUX)%DEL_TOL_CNT + ONE
                      ELSE
                        AUXDEM(IAUX)%DELORDER = MIN( AUXDEM(IAUX)%DEMAND, AUXDEM(IAUX)%BALANCE )
                        AUXDEM(IAUX)%DELORDER = AUXDEM(IAUX)%DELORDER / AUXDEM(IAUX)%FACTOR      ! Apply factor here ... Delivery tracked at aux seg, balance tracked at off-grid aux location
                        !
                      END IF !(DTMP1.EQ.0 | DTMP1/DTMP2.LT.0.5)
                    END IF !(KITER.LE.SWMXITER(1))
                    !
                    ! Impose minimu delivery order ...
                    IF ( AUXDEM(IAUX)%DELORDER.LT.SWO%MIN_DELORDER(SWO%NFARM+IAUX) )  AUXDEM(IAUX)%DELORDER = DZ
                    !
                    UNIT(IUNIT)%TFDR      = UNIT(IUNIT)%TFDR + AUXDEM(IAUX)%DEMAND
                    UNIT(IUNIT)%DELORDER  = UNIT(IUNIT)%DELORDER + (AUXDEM(IAUX)%DELORDER * AUXDEM(IAUX)%FACTOR)
                    !
                    DIST(IDIST)%TFDR      = DIST(IDIST)%TFDR + AUXDEM(IAUX)%DEMAND
                    DIST(IDIST)%DELORDER  = DIST(IDIST)%DELORDER + (AUXDEM(IAUX)%DELORDER * AUXDEM(IAUX)%FACTOR)
                    !
                    PROJ(IPROJ)%TFDR      = PROJ(IPROJ)%TFDR + AUXDEM(IAUX)%DEMAND
                    PROJ(IPROJ)%DELORDER  = PROJ(IPROJ)%DELORDER + (AUXDEM(IAUX)%DELORDER * AUXDEM(IAUX)%FACTOR)
                 ELSE
                     AUXDEM(IAUX)%DELORDER = DZ
                 END IF
             END DO
             !DO IAUX = 1,NAUXDEM
             !  IUNIT = AUXDEM(IAUX)%UnitID
             !  IDIST = AUXDEM(IAUX)%DistID
             !  IPROJ = AUXDEM(IAUX)%ProjID
             !  !
             !  IF (AUXDEM(IAUX)%AuxSeg.EQ.0) THEN
             !    AUXDEM(IAUX)%DELORDER = DZ                                   ! GW only demand!
             !  ELSE
             !    ! Iteration spin-up period ...
             !    IF (KITER.LE.SWO%SWMXITER(1)) THEN
             !      AUXDEM(IAUX)%DELORDER = MIN( AUXDEM(IAUX)%DEMAND, AUXDEM(IAUX)%BALANCE) / AUXDEM(IAUX)%FACTOR ! Apply factor here ... Delivery tracked at aux seg, balance tracked at off-grid aux location
             !      !
             !      ! Iteration stagger or hold period ...
             !    ELSE
             !      DTMP1 = AUXDEM_PREV(IAUX)%DELORDER
             !      DTMP2 = AUXDEM_PREV(IAUX)%DELIVERY
             !      IF (DTMP1.NE.DZ) THEN
             !        DTMP3 = DTMP2/DTMP1
             !      ELSE
             !        DTMP3 = DZ
             !      END IF !(DTMP2.EQ.DZ)
             !      ! SCOTT DO NOT CURTAIL MEXICO IDIST.NE.3
             !      IF ( DTMP1.EQ.DZ .OR. DTMP3.LT.DIST_DELTOL(IDIST) ) THEN
             !        AUXDEM(IAUX)%DELORDER = DZ
             !      ELSE
             !        AUXDEM(IAUX)%DELORDER = MIN( AUXDEM(IAUX)%DEMAND, AUXDEM(IAUX)%BALANCE )
             !        AUXDEM(IAUX)%DELORDER = AUXDEM(IAUX)%DELORDER / AUXDEM(IAUX)%FACTOR      ! Apply factor here ... Delivery tracked at aux seg, balance tracked at off-grid aux location
             !        !
             !      END IF !(DTMP1.EQ.0 | DTMP1/DTMP2.LT.0.5)
             !    END IF !(KITER.LE.SWMXITER(1))
             !  END IF !(AuxSeg.EQ.0)
             !  !
             !  ! Impose minimu delivery order ...
             !  IF ( AUXDEM(IAUX)%DELORDER.LT.SWO%MIN_DELORDER(SWO%NFARM+IAUX) )  AUXDEM(IAUX)%DELORDER = DZ
             !  !
             !  IF (IUNIT.NE.0) THEN
             !    UNIT(IUNIT)%TFDR      = UNIT(IUNIT)%TFDR + AUXDEM(IAUX)%DEMAND
             !    UNIT(IUNIT)%DELORDER  = UNIT(IUNIT)%DELORDER + (AUXDEM(IAUX)%DELORDER * AUXDEM(IAUX)%FACTOR)
             !  END IF !(IUNIT.NE.0)
             !  !
             !  IF (IDIST.NE.0) THEN
             !    DIST(IDIST)%TFDR      = DIST(IDIST)%TFDR + AUXDEM(IAUX)%DEMAND
             !    DIST(IDIST)%DELORDER  = DIST(IDIST)%DELORDER + (AUXDEM(IAUX)%DELORDER * AUXDEM(IAUX)%FACTOR)
             !  END IF !(IDIST.NE.0)
             !  !
             !  IF (IPROJ.NE.0) THEN
             !    PROJ(IPROJ)%TFDR      = PROJ(IPROJ)%TFDR + AUXDEM(IAUX)%DEMAND
             !    PROJ(IPROJ)%DELORDER  = PROJ(IPROJ)%DELORDER + (AUXDEM(IAUX)%DELORDER * AUXDEM(IAUX)%FACTOR)
             !  END IF !(IPROJ.NE.0)
             !  !
             !END DO !(IAUX)
       I     = -1
       J     = -1
       IPROJ = -1
       IRES  = -1
       ISEG  = -1
       IRCH  = -1
       IDVS  = -1
       ISPT  = -1
       IBRC  = -1
       IRES  = -1
       JRES  = -1
       IPROJ = -1
       IDIST = -1
       IUNIT = -1
       IFARM = -1
       IAUX  = -1
             !
             ! (5) Identify conveyance and delivery segments (SegType 2 or 3)
             !!     that have delivery demand downstream w/in their conveyance tree ...
             !DO ISEG = 1,NSS
             !  ! Reset DDFLAG to zero ...
             !  SEGDATA(ISEG)%DDFLAG = 0
             !  ! Check if ISEG is a delivery segment, check for delivery order ...
             !  IF (SEGINFO(ISEG)%SegType.EQ.3) THEN
             !    IFARM      = SEGINFO(ISEG)%FarmID
             !    IAUX       = SEGINFO(ISEG)%AuxID
             !    IF ( IFARM.NE.0 ) THEN; IF( FARM(IFARM)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = 1
             !    END IF !(IFARM.NE.0 / DELORDER.GT.0)
             !    IF ( IAUX.NE.0 ) THEN; IF( AUXDEM(IAUX)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = 1
             !    END IF
             !    !
             !  ELSEIF (SEGINFO(ISEG)%SegType.EQ.2) THEN ! If ISEG is a conveyance segment, check for downstream delivery orders ...
             !    NTREE      = DNTREE_CON2(ISEG)%NTREE
             !    DO ITREE   = 1,NTREE
             !      JSEG     = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
             !      IF (SEGINFO(JSEG)%SegType.EQ.3) THEN
             !        IFARM  = SEGINFO(JSEG)%FarmID
             !        IAUX   = SEGINFO(JSEG)%AuxID
             !        IF ( IFARM.NE.0 ) THEN; IF( FARM(IFARM)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = 1
             !        END IF !(IFARM.NE.0 / DELORDER.GT.0)
             !        IF ( IAUX.NE.0 ) THEN; IF( AUXDEM(IAUX)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = 1
             !        END IF
             !        IF(SEGDATA(ISEG)%DDFLAG == 1) EXIT
             !      END IF !(SEGINFO(JSEG)%SegType.EQ.3)
             !    END DO !(ITREE)
             !  END IF !(SEGINFO(ISEG)%SegType.EQ.2 or 3)
             !END DO !(ISEG)
             DO CONCURRENT (ISEG = 1:NSS)
                                         SEGDATA(ISEG)%DDFLAG = Z
             END DO
             !
             DO ISEG=1, NSS
             IF(SEGINFO(ISEG)%SegType.EQ.TWO .OR. SEGINFO(ISEG)%SegType.EQ.THREE) THEN
               ! Check if ISEG is a delivery segment, check for delivery order ...
               IF (SEGINFO(ISEG)%SegType.EQ.THREE) THEN
                 IFARM      = SEGINFO(ISEG)%FarmID
                 IAUX       = SEGINFO(ISEG)%AuxID
                 IF ( IFARM.NE.Z ) THEN; IF( SWO%FARM(IFARM)%DELORDER.GT.DZ  )  SEGDATA(ISEG)%DDFLAG  = ONE
                 END IF !(IFARM.NE.0 / DELORDER.GT.0)
                 IF ( IAUX.NE.Z  ) THEN; IF( SWO%AUXDEM(IAUX)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = ONE
                 END IF
                 !
               ELSEIF (SEGINFO(ISEG)%SegType.EQ.TWO) THEN ! If ISEG is a conveyance segment, check for downstream delivery orders ...
                 NTREE      = SWO%DNTREE_CON2(ISEG)%NTREE
                 DO ITREE   = 1,NTREE
                   JSEG     = SWO%DNTREE_CON2(ISEG)%SEGTREE(ITREE)
                   IF (SEGINFO(JSEG)%SegType.EQ.THREE) THEN
                     IFARM  = SEGINFO(JSEG)%FarmID
                     IAUX   = SEGINFO(JSEG)%AuxID
                     IF ( IFARM.NE.Z ) THEN; IF( SWO%FARM(IFARM)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = ONE
                     END IF !(IFARM.NE.0 / DELORDER.GT.0)
                     IF ( IAUX.NE.Z ) THEN; IF( SWO%AUXDEM(IAUX)%DELORDER.GT.DZ )  SEGDATA(ISEG)%DDFLAG  = ONE
                     END IF
                     IF(SEGDATA(ISEG)%DDFLAG == ONE) EXIT
                   END IF !(SEGINFO(JSEG)%SegType.EQ.3)
                 END DO !(ITREE)
               END IF !(SEGINFO(ISEG)%SegType.EQ.2 or 3)
             END IF
             END DO !(ISEG)
             !
      END IF !  SITER == ONE --------------------------------------------
      I     = -1
      J     = -1
      IPROJ = -1
      IRES  = -1
      ISEG  = -1
      IRCH  = -1
      IDVS  = -1
      ISPT  = -1
      IBRC  = -1
      IRES  = -1
      JRES  = -1
      IPROJ = -1
      IDIST = -1
      IUNIT = -1
      IFARM = -1
      IAUX  = -1
      !
      ! (5) Compute diversion request for each diversion segment
      !     DIVREQ(DIVSEG) = sum( DELREQ(FARM+AUX) ) - sum( gains ) - sum( inflows/input ) - sum( inflows/routed )
      DO IDVS = 1,SWO%DIVCOUNT
        !
        ISEG  = DIVSEG(IDVS)%DivSeg
        !
        ! Zero out sums...
        DIVSEG(IDVS)%TFDR      = DZ
        DIVSEG(IDVS)%DELORDER  = DZ
        DIVSEG(IDVS)%DIVORDER  = DZ
        DIVSEG(IDVS)%DSEEP     = DZ
        DIVSEG(IDVS)%DFLOW_IN  = DZ
        DIVSEG(IDVS)%DFLOW_RT  = DZ
        !
        ! Add @ diversion segment ...
        !
        ! -- TFDR / DELORDER
        IF (SEGINFO(ISEG)%SegType.EQ.THREE) THEN
          IFARM = SEGINFO(ISEG)%FarmID
          IAUX  = SEGINFO(ISEG)%AuxID
          IF (IFARM.NE.0) THEN
            DIVSEG(IDVS)%TFDR     = DIVSEG(IDVS)%TFDR + SWO%FARM(IFARM)%TFDR
            DIVSEG(IDVS)%DELORDER = DIVSEG(IDVS)%DELORDER + FARM(IFARM)%DELORDER
          END IF !(IFARM.NE.0)
          IF (IAUX.NE.0) THEN
            DIVSEG(IDVS)%TFDR     = DIVSEG(IDVS)%TFDR + AUXDEM(IAUX)%DEMAND
            DIVSEG(IDVS)%DELORDER = DIVSEG(IDVS)%DELORDER + AUXDEM(IAUX)%DELORDER
          END IF !(IAUX.NE.0)
        END IF !(SegType.EQ.3)
        !
        IF (SEGDATA(ISEG)%DDFLAG.NE.0) THEN
          !
          ! -- DSEEP
          IRCH    = SEGRCH_IN(ISEG)                                     ! reach index @ rch 1 of DIVSEG
          JRCH    = SEGRCH_OUT(ISEG)                                    ! reach index @ last reach of DIVSEG
          DO CONCURRENT (ISTR = IRCH:JRCH)
            DIVSEG(IDVS)%DSEEP     = DIVSEG(IDVS)%DSEEP - STRM(11,ISTR) * SWO%DELT             ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
          END DO !(ISTR)
          !
          ! -- DFLOW_IN  (inflow @ SFR input file)
          IF (IDIVAR(1,ISEG).EQ.0 .AND. SEGDATA(ISEG)%RRFLAG.NE.1) THEN
            DIVSEG(IDVS)%DFLOW_IN  = DIVSEG(IDVS)%DFLOW_IN + SEG(2,ISEG) * SWO%DELT               ! Inflow specified in SFR (applies only to non-diversion segments!)
          END IF !(IDIVAR(1).EQ.0)
          !
          ! -- DFLOW_RT (inflow @ SFR simulated)
          NLIST   = SWO%UPSEG(ISEG)%NLIST
          DO ILIST=1, NLIST
            KSEG   = SWO%UPSEG(ISEG)%SEGLIST(ILIST)
            JRCH   = SEGRCH_OUT(KSEG)
            IF (SEGINFO(KSEG)%SegType.NE.TWO .AND.  SEGINFO(KSEG)%SegType.NE.THREE .AND. SEGDATA(KSEG)%OUTSEG.EQ.ISEG) THEN
              DIVSEG(IDVS)%DFLOW_RT = DIVSEG(IDVS)%DFLOW_RT + STRM(9,JRCH) * SWO%DELT                                 ! Inflow simulated in SFR (i.e., simulated flow from upstream segment that is NOT in conveyance network)
            END IF !(SegType.NE.2 / OUTSEG.EQ.ISEG)
          END DO !(ILIST)
          !
        END IF !(SEGDATA(ISEG)%DDFLAG.NE.0)
        !
        ! Add downstream segments
        NTREE   = SWO%DNTREE_CON2(ISEG)%NTREE
        DO ITREE=1, NTREE
          JSEG  = SWO%DNTREE_CON2(ISEG)%SEGTREE(ITREE)
          !
          ! -- TFDR / DELORDER
          IF (SEGINFO(JSEG)%SegType.EQ.THREE) THEN
            IFARM = SEGINFO(JSEG)%FarmID
            IAUX  = SEGINFO(JSEG)%AuxID
            IF (IFARM.NE.0) THEN
              DIVSEG(IDVS)%TFDR     = DIVSEG(IDVS)%TFDR + FARM(IFARM)%TFDR
              DIVSEG(IDVS)%DELORDER = DIVSEG(IDVS)%DELORDER + FARM(IFARM)%DELORDER
            END IF !(IFARM.NE.0)
            IF (IAUX.NE.0) THEN
              DIVSEG(IDVS)%TFDR     = DIVSEG(IDVS)%TFDR + AUXDEM(IAUX)%DEMAND
              DIVSEG(IDVS)%DELORDER = DIVSEG(IDVS)%DELORDER + AUXDEM(IAUX)%DELORDER
            END IF !(IAUX.NE.0)
          END IF !(SegType.EQ.3)
          !
          IF (SEGDATA(JSEG)%DDFLAG.NE.0) THEN
            !
            ! -- DSEEP
            IRCH     = SEGRCH_IN(JSEG)                                  ! reach index @ rch 1 of DIVSEG
            JRCH     = SEGRCH_OUT(JSEG)                                 ! reach index @ last reach of DIVSEG
            DO CONCURRENT (JSTR  = IRCH:JRCH)
              DIVSEG(IDVS)%DSEEP    = DIVSEG(IDVS)%DSEEP - STRM(11,JSTR) * SWO%DELT            ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
            END DO !(JSTR)
            !
            ! -- DFLOW_IN  (inflow @ SFR input file)
            IF (IDIVAR(1,JSEG).EQ.0 .AND. SEGDATA(JSEG)%RRFLAG.NE.1) THEN
              DIVSEG(IDVS)%DFLOW_IN = DIVSEG(IDVS)%DFLOW_IN + SEG(2,JSEG) * SWO%DELT              ! Inflow specified in SFR (applies only to non-diversion segments!)
            END IF !(IDIVAR(1).EQ.0)
            !
            ! -- DFLOW_RT (inflow @ SFR simulated)
            NLIST    = SWO%UPSEG(JSEG)%NLIST
            DO ILIST=1, NLIST
              KSEG   = SWO%UPSEG(JSEG)%SEGLIST(ILIST)
              JRCH   = SEGRCH_OUT(KSEG)
              IF (SEGINFO(KSEG)%SegType.NE.2 .AND. SEGINFO(KSEG)%SegType.NE.3 .AND. SEGDATA(KSEG)%OUTSEG.EQ.JSEG) THEN
                DIVSEG(IDVS)%DFLOW_RT = DIVSEG(IDVS)%DFLOW_RT + STRM(9,JRCH) * SWO%DELT                               ! Inflow simulated in SFR (i.e., simulated flow from upstream segment that is NOT in conveyance network)
              END IF !(SegType.NE.2 / OUTSEG.EQ.JSEG)
            END DO !(ILIST)
          END IF !(SEGINFO(JSEG)%DDFLAG.NE.0)
        END DO !(ITREE/JSEG)
        !
        ! Compute diversion order ...
        ! DivOrder = DelOrder - Gains - Inflow(input) - Inflow(simulated)
        IF (DIVSEG(IDVS)%DELORDER.EQ.DZ) THEN
            DIVSEG(IDVS)%DIVORDER   = DZ
        ELSE
            DIVSEG(IDVS)%DIVORDER   = DIVSEG(IDVS)%DELORDER - DIVSEG(IDVS)%DSEEP - DIVSEG(IDVS)%DFLOW_IN - DIVSEG(IDVS)%DFLOW_RT
            IF(DIVSEG(IDVS)%DIVORDER < NEARZERO_5) DIVSEG(IDVS)%DIVORDER = DZ
        END IF !(DELORDER.EQ.0)
        !
        ! IMF DEBUG --
        ! Impose incremental update of diversion orders to reduce oscillation ...
        IF(KITER < THREE) THEN !NO ADJUSTMENT FOR FIRST DIVERSION REQUESTS
            CONTINUE
        ELSEIF (DIVSEG(IDVS)%TFDR > NEARZERO_6) THEN
            CALL RELAX_IT(DIVSEG(IDVS)%DIVORDER, DIVSEG_PREV(IDVS)%DIVORDER, SWO%WTFACTOR(1))
            !CALL DAMP_IT(DIVSEG(IDVS)%DIVORDER, DIVSEG_PREV(IDVS)%DIVORDER, A, 0.9D0)
        ELSE
            CALL RELAX_IT(DIVSEG(IDVS)%DIVORDER, DIVSEG_PREV(IDVS)%DIVORDER, SWO%WTFACTOR(2))
        END IF !(TFDR.GT.0)
      END DO !(IDVS)
      I     = -1
      J     = -1
      IPROJ = -1
      IRES  = -1
      ISEG  = -1
      IRCH  = -1
      IDVS  = -1
      ISPT  = -1
      IBRC  = -1
      IRES  = -1
      JRES  = -1
      IPROJ = -1
      IDIST = -1
      IUNIT = -1
      IFARM = -1
      IAUX  = -1
      !
      ! (6) Compute fractional diversions through distribution network ...
      !     Fractional diversion at each junction in the distribution network
      !     is based on total *delivery request*,
      !     accounting for cumulative gains/losses/inflows down each branch ...
      DO ISPT = 1,SWO%SPTCOUNT

        ISEG                    = SPTSEG(ISPT)%SplitSeg
        NBRANCH                 = SPTSEG(ISPT)%NBRANCH

        ! Zero out sums...
        SPTSEG(ISPT)%TFDR       = DZ
        SPTSEG(ISPT)%DELORDER   = DZ
        SPTSEG(ISPT)%DIVORDER   = DZ
        SPTSEG(ISPT)%DSEEP      = DZ
        SPTSEG(ISPT)%DFLOW_IN   = DZ
        SPTSEG(ISPT)%DFLOW_RT   = DZ
        !
        ! Add split segment
        !
        ! -- TFDR / DELORDER
        IF (SEGINFO(ISEG)%SegType.EQ.THREE) THEN
          IFARM = SEGINFO(ISEG)%FarmID
          IAUX  = SEGINFO(ISEG)%AuxID
          IF (IFARM.NE.0) THEN
            SPTSEG(ISPT)%TFDR     = SPTSEG(ISPT)%TFDR + FARM(IFARM)%TFDR
            SPTSEG(ISPT)%DELORDER = SPTSEG(ISPT)%DELORDER + FARM(IFARM)%DELORDER
          END IF !(IFARM.NE.0)
          IF (IAUX.NE.0) THEN
            SPTSEG(ISPT)%TFDR     = SPTSEG(ISPT)%TFDR + AUXDEM(IAUX)%DEMAND
            SPTSEG(ISPT)%DELORDER = SPTSEG(ISPT)%DELORDER + AUXDEM(IAUX)%DELORDER
          END IF !(IAUX.NE.0)
        END IF !(SegType.EQ.3)
        !
        IF (SEGDATA(ISEG)%DDFLAG.NE.0) THEN
          !
          ! -- DSEEP
          IRCH    = SEGRCH_IN(ISEG)                                     ! reach index @ rch 1 of SPTSEG
          JRCH    = SEGRCH_OUT(ISEG)                                    ! reach index @ last reach of SPTSEG
          DO CONCURRENT (ISTR = IRCH:JRCH)
            SPTSEG(ISPT)%DSEEP    = SPTSEG(ISPT)%DSEEP - STRM(11,ISTR) * SWO%DELT              ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
          END DO !(ISTR)
          !
          ! -- DFLOW_IN  (inflow @ SFR input file)
          IF (IDIVAR(1,ISEG).EQ.0 .AND. SEGDATA(ISEG)%RRFLAG.NE.1) THEN
            SPTSEG(ISPT)%DFLOW_IN  = SPTSEG(ISPT)%DFLOW_IN + SEG(2,ISEG) * SWO%DELT               ! Inflow specified in SFR (applies only to non-diversion segments!)
          END IF !(IDIVAR(1).EQ.0)
          !
          ! -- DFLOW_RT (inflow @ SFR simulated)
          NLIST    = SWO%UPSEG(ISEG)%NLIST
          DO ILIST=1, NLIST
            KSEG  = SWO%UPSEG(ISEG)%SEGLIST(ILIST)
            JRCH  = SEGRCH_OUT(KSEG)
            IF (SEGINFO(KSEG)%SegType.NE.2 .AND. SEGINFO(KSEG)%SegType.NE.3 .AND. SEGDATA(KSEG)%OUTSEG.EQ.ISEG) THEN
              SPTSEG(ISPT)%DFLOW_RT = SPTSEG(ISPT)%DFLOW_RT + STRM(9,JRCH) * SWO%DELT                                 ! Inflow simulated in SFR (i.e., simulated flow from upstream segment that is NOT in conveyance network)
            END IF !(SegType.NE.2 / OUTSEG.EQ.ISEG)
          END DO !(ILIST)
          !
        END IF !(SEGDATA(ISEG)%DDFLAG.NE.0)
        !
        ! Loop down branches
        DO J = 1,NBRANCH
          !
          JSEG  = SPTSEG(ISPT)%BrcSeg(J)
          SPTSEG(ISPT)%BrcTFDR(J)      = DZ
          SPTSEG(ISPT)%BrcDELORDER(J)  = DZ
          SPTSEG(ISPT)%BrcDIVORDER(J)  = DZ
          SPTSEG(ISPT)%BrcDSEEP(J)     = DZ
          SPTSEG(ISPT)%BrcDFLOW_IN(J)  = DZ
          SPTSEG(ISPT)%BrcDFLOW_RT(J)  = DZ
          !
          ! Add branch segment
          !
          ! -- TFDR / DELORDER
          IF (SEGINFO(JSEG)%SegType.EQ.THREE) THEN
            IFARM = SEGINFO(JSEG)%FarmID
            IAUX  = SEGINFO(JSEG)%AuxID
            IF (IFARM.NE.0) THEN
              SPTSEG(ISPT)%TFDR          = SPTSEG(ISPT)%TFDR          + FARM(IFARM)%TFDR
              SPTSEG(ISPT)%DELORDER      = SPTSEG(ISPT)%DELORDER      + FARM(IFARM)%DELORDER
              SPTSEG(ISPT)%BrcTFDR(J) = SPTSEG(ISPT)%BrcTFDR(J) + FARM(IFARM)%TFDR
              SPTSEG(ISPT)%BrcDELORDER(J) = SPTSEG(ISPT)%BrcDELORDER(J) + FARM(IFARM)%DELORDER
            END IF !(IFARM.NE.0)
            IF (IAUX.NE.0) THEN
              SPTSEG(ISPT)%TFDR     = SPTSEG(ISPT)%TFDR + AUXDEM(IAUX)%DEMAND
              SPTSEG(ISPT)%DELORDER = SPTSEG(ISPT)%DELORDER + AUXDEM(IAUX)%DELORDER
              SPTSEG(ISPT)%BrcTFDR(J) = SPTSEG(ISPT)%BrcTFDR(J) + AUXDEM(IAUX)%DEMAND
              SPTSEG(ISPT)%BrcDELORDER(J) = SPTSEG(ISPT)%BrcDELORDER(J) + AUXDEM(IAUX)%DELORDER
            END IF !(IAUX.NE.0)
          END IF !(SegType.EQ.3)
          !
          IF (SEGDATA(JSEG)%DDFLAG.NE.0) THEN
            !
            ! -- DSEEP
            IRCH   =SEGRCH_IN(JSEG)                                     ! reach index @ rch 1 of branch segment
            JRCH   =SEGRCH_OUT(JSEG)                                    ! reach index @ last reach of branch segment
            DO ISTR=IRCH, JRCH
              SPTSEG(ISPT)%DSEEP          = SPTSEG(ISPT)%DSEEP - STRM(11,ISTR) * SWO%DELT              ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
              SPTSEG(ISPT)%BrcDSEEP(J) = SPTSEG(ISPT)%BrcDSEEP(J) - STRM(11,ISTR) * SWO%DELT
            END DO !(ISTR)
            !
            ! -- DFLOW_IN  (inflow @ SFR input file)
            IF (IDIVAR(1,JSEG).EQ.0 .AND. SEGDATA(JSEG)%RRFLAG.NE.1) THEN
              SPTSEG(ISPT)%DFLOW_IN  = SPTSEG(ISPT)%DFLOW_IN + SEG(2,JSEG) * SWO%DELT               ! Inflow specified in SFR (applies only to non-diversion segments!)
              SPTSEG(ISPT)%BrcDFLOW_IN(J) = SPTSEG(ISPT)%BrcDFLOW_IN(J) + SEG(2,JSEG) * SWO%DELT
            END IF !(IDIVAR(1).EQ.0)
            !
            ! -- DFLOW_RT (inflow @ SFR simulated)
            NLIST    = SWO%UPSEG(JSEG)%NLIST
            DO ILIST=1, NLIST
              KSEG   = SWO%UPSEG(JSEG)%SEGLIST(ILIST)
              JRCH   = SEGRCH_OUT(KSEG)
              IF (SEGINFO(KSEG)%SegType.NE.2 .AND. SEGINFO(KSEG)%SegType.NE.3 .AND. SEGDATA(KSEG)%OUTSEG.EQ.JSEG) THEN
                SPTSEG(ISPT)%DFLOW_RT = SPTSEG(ISPT)%DFLOW_RT + STRM(9,JRCH) * SWO%DELT                                 ! Inflow simulated in SFR (i.e., simulated flow from upstream segment that is NOT in conveyance network)
                SPTSEG(ISPT)%BrcDFLOW_RT(J) = SPTSEG(ISPT)%BrcDFLOW_RT(J) + STRM(9,JRCH) * SWO%DELT
              END IF !(SegType.NE.2 / OUTSEG.EQ.JSEG)
            END DO !(ILIST)
            !
          END IF !(SEGDATA(JSEG)%DDFLAG.NE.0)
          !
          ! Add downstream tree below branch segment
          NTREE = SWO%DNTREE_CON2(JSEG)%NTREE
          DO ITREE=1, NTREE
            !
            KSEG   = SWO%DNTREE_CON2(JSEG)%SEGTREE(ITREE)
            !
            ! -- TFDR / DELORDER
            IF (SWO%SEGINFO(KSEG)%SegType.EQ.3) THEN
              IFARM = SEGINFO(KSEG)%FarmID
              IAUX  = SEGINFO(KSEG)%AuxID
              IF (IFARM.NE.0) THEN
                SPTSEG(ISPT)%TFDR     = SPTSEG(ISPT)%TFDR + FARM(IFARM)%TFDR
                SPTSEG(ISPT)%DELORDER = SPTSEG(ISPT)%DELORDER + FARM(IFARM)%DELORDER
                SPTSEG(ISPT)%BrcTFDR(J) = SPTSEG(ISPT)%BrcTFDR(J) + FARM(IFARM)%TFDR
                SPTSEG(ISPT)%BrcDELORDER(J) = SPTSEG(ISPT)%BrcDELORDER(J) + FARM(IFARM)%DELORDER
              END IF !(IFARM.NE.0)
              IF (IAUX.NE.0) THEN
                SPTSEG(ISPT)%TFDR     = SPTSEG(ISPT)%TFDR + AUXDEM(IAUX)%DEMAND
                SPTSEG(ISPT)%DELORDER = SPTSEG(ISPT)%DELORDER + AUXDEM(IAUX)%DELORDER
                SPTSEG(ISPT)%BrcTFDR(J) = SPTSEG(ISPT)%BrcTFDR(J) + AUXDEM(IAUX)%DEMAND
                SPTSEG(ISPT)%BrcDELORDER(J) = SPTSEG(ISPT)%BrcDELORDER(J) + AUXDEM(IAUX)%DELORDER
              END IF !(IAUX.NE.0)
            END IF !(SegType.EQ.3)

            IF (SEGDATA(KSEG)%DDFLAG.NE.0) THEN
              !
              ! -- DSEEP
              IRCH   =SEGRCH_IN(KSEG)                                   ! reach index @ rch 1 of branch segment
              JRCH   =SEGRCH_OUT(KSEG)                                  ! reach index @ last reach of branch segment
              DO ISTR=IRCH, JRCH
                SPTSEG(ISPT)%DSEEP       = SPTSEG(ISPT)%DSEEP - STRM(11,ISTR) * SWO%DELT            ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
                SPTSEG(ISPT)%BrcDSEEP(J) = SPTSEG(ISPT)%BrcDSEEP(J) - STRM(11,ISTR) * SWO%DELT
              END DO !(ISTR)
              !
              ! -- DFLOW_IN  (inflow @ SFR input file)
              IF (IDIVAR(1,KSEG).EQ.0 .AND. SEGDATA(KSEG)%RRFLAG.NE.1) THEN
                SPTSEG(ISPT)%DFLOW_IN = SPTSEG(ISPT)%DFLOW_IN + SEG(2,KSEG) * SWO%DELT            ! Inflow specified in SFR (applies only to non-diversion segments!)
                SPTSEG(ISPT)%BrcDFLOW_IN(J) = SPTSEG(ISPT)%BrcDFLOW_IN(J) + SEG(2,KSEG) * SWO%DELT
              END IF !(IDIVAR(1).EQ.0)
              !
              ! -- DFLOW_RT (inflow @ SFR simulated)
              NLIST   =SWO%UPSEG(KSEG)%NLIST
              DO ILIST=1, NLIST
                LSEG  = SWO%UPSEG(KSEG)%SEGLIST(ILIST)
                JRCH  = SEGRCH_OUT(LSEG)
                IF (SEGINFO(LSEG)%SegType.NE.2 .AND. SEGINFO(LSEG)%SegType.NE.3 .AND. SEGDATA(LSEG)%OUTSEG.EQ.KSEG) THEN
                  SPTSEG(ISPT)%DFLOW_RT = SPTSEG(ISPT)%DFLOW_RT + STRM(9,JRCH) * SWO%DELT                               ! Inflow simulated in SFR (i.e., simulated flow from upstream segment that is NOT in conveyance network)
                  SPTSEG(ISPT)%BrcDFLOW_RT(J) = SPTSEG(ISPT)%BrcDFLOW_RT(J) + STRM(9,JRCH) * SWO%DELT
                END IF !(SegType.NE.2 / OUTSEG.EQ.KSEG)
              END DO !(ILIST)
              !
            END IF !(SEGDATA(KSEG)%DDFLAG.NE.0)
            !
          END DO !(ITREE)
          !
          ! Compute diversion order @ branch
          ! DivOrder = DelOrder - Gains - Inflow(input) - Inflow(simulated)
          IF (SPTSEG(ISPT)%BrcDELORDER(J).EQ.DZ) THEN
            SPTSEG(ISPT)%BrcDIVORDER(J)   = DZ
          ELSE
            SPTSEG(ISPT)%BrcDIVORDER(J) = SPTSEG(ISPT)%BrcDELORDER(J) - SPTSEG(ISPT)%BrcDSEEP(J)   &
                                           - SPTSEG(ISPT)%BrcDFLOW_IN(J) - SPTSEG(ISPT)%BrcDFLOW_RT(J)
            IF(SPTSEG(ISPT)%BrcDIVORDER(J) < DZ) SPTSEG(ISPT)%BrcDIVORDER(J) = DZ
          END IF !(DELORDER.EQ.0)
          !
        END DO !(IBRC)
        !
        ! Compute diversion order @ split
        ! DivOrder = DelOrder - Gains - Inflow(input) - Inflow(simulated)
        IF (SPTSEG(ISPT)%DELORDER.EQ.DZ) THEN
          SPTSEG(ISPT)%DIVORDER   = DZ
        ELSE
          SPTSEG(ISPT)%DIVORDER = SPTSEG(ISPT)%DELORDER - SPTSEG(ISPT)%DSEEP     &
                                - SPTSEG(ISPT)%DFLOW_IN - SPTSEG(ISPT)%DFLOW_RT
          IF(SPTSEG(ISPT)%DIVORDER < DZ) SPTSEG(ISPT)%DIVORDER = DZ
        END IF !(DELORDER.EQ.0)

        ! Impose incremental update of diversion orders to reduce oscillation ...
        IF(KITER < THREE) THEN !NO ADJUSTMENT FOR FIRST DIVERSION REQUESTS
            CONTINUE
        ELSEIF (SPTSEG(ISPT)%TFDR.GT.DZ) THEN
            CALL RELAX_IT(SPTSEG(ISPT)%DIVORDER, SWO%SPTSEG_PREV(ISPT)%DIVORDER, SWO%WTFACTOR(1))
        ELSE
          CALL RELAX_IT(SPTSEG(ISPT)%DIVORDER, SWO%SPTSEG_PREV(ISPT)%DIVORDER, SWO%WTFACTOR(2))
        END IF !(TFDR.GT.0)
        DO J=1, NBRANCH
          IF(KITER < THREE) THEN !NO ADJUSTMENT FOR FIRST DIVERSION REQUESTS
            CONTINUE
          ELSEIF (SPTSEG(ISPT)%BrcTFDR(J).GT.DZ) THEN
              CALL RELAX_IT(SPTSEG(ISPT)%BrcDIVORDER(J), SWO%SPTSEG_PREV(ISPT)%BrcDIVORDER(J), SWO%WTFACTOR(1))
          ELSE
              CALL RELAX_IT(SPTSEG(ISPT)%BrcDIVORDER(J), SWO%SPTSEG_PREV(ISPT)%BrcDIVORDER(J), SWO%WTFACTOR(2))
          END IF !(TFDR.GT.0)
        END DO !(IBRC)
      END DO !(ISPT)
      I     = -1
      J     = -1
      IPROJ = -1
      IRES  = -1
      ISEG  = -1
      IRCH  = -1
      IDVS  = -1
      ISPT  = -1
      IBRC  = -1
      IRES  = -1
      JRES  = -1
      IPROJ = -1
      IDIST = -1
      IUNIT = -1
      IFARM = -1
      IAUX  = -1
      !
      ! (7) Compute diversion request to units, districts, projects
      !     DIVREQ = DELREQ / DELIVEFF
      ! Unit DivOrder not actually used in routing ...
      DO CONCURRENT (IUNIT = 1:NUNIT)
        UNIT(IUNIT)%DIVORDER  = UNIT(IUNIT)%DELORDER / UNIT(IUNIT)%DELIVEFF
      END DO !(IUNIT)
      !
      ! Distric DivOrder not actually used in routing ...
      DO CONCURRENT (IDIST = 1:NDIST)
        DIST(IDIST)%DIVORDER  = DIST(IDIST)%DELORDER / DIST(IDIST)%DELIVEFF
      END DO !(IDIST)
      !
      ! Project DivOrder used to estimate release ...
      ! Computed as sum of DivSeg DivOrders ...
      DO P=1, NPROJ
        PROJ(P)%DIVORDER     = DZ
        DO IDVS=1, SWO%DIVCOUNT
        IF(DIVSEG(IDVS)%ProjID.EQ.P) PROJ(P)%DIVORDER = PROJ(P)%DIVORDER + DIVSEG(IDVS)%DIVORDER
        END DO
      END DO
      !
    END ASSOCIATE
    !
    !
    !----------------------------------------------------------------------------------------------
    !
    ! DETERMINE MAXIMUM POTENTIAL RELASE FROM RESERVOIR GIVEN CURRENT CONDITIONS
    ! (ASSUMING IT IS DRAINED TO NON-PROJECT STORAGE LEVELS)
    !
    !CALL SWO%RESDAT%SET_RELEASE_POT() --CALLED IN SET_ADJUSTIBLE_PROPERTIES
    !
    !----------------------------------------------------------------------------------------------
    !DO CONCURRENT(IPROJ = 1:NPROJ)
    !DO CONCURRENT(IRES = 1:SWO%NRES_BAL(IPROJ))
    !    IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
    !       !
    !       ASSOCIATE(                                                                 &
    !                 STORAGE      => SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV    , &   ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
    !                 INFLOW       => SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW          , &   ! INFLOW       during current step (updated @ AD routine!)          [L3]
    !                 PRCP_LENTH   => SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP            , &   ! PRCP         during current step (updated @ AD routine!)          [L]
    !                 EVAP_LENTH   => SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP            , &   ! EVAP         during current step (updated @ AD routine!)          [L]
    !                 STORAGE_MIN  => SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_MIN     , &   ! Min allowed storage (either STORAGE_SPEC_MIN or greater) --Furthers point that can be released too
    !                 AREA_START   => SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_PREV       , &
    !                 AREA_END     => SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL        , &   ! AREA_END @ non-project storage only ...
    !                 AREA_MAX     => SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_AREA        , &   ! Maximum area the reservoir could have (also represents maximum area precep falls on and makes its way to the reservoir)
    !                 PRCP_FRAC    => SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRECIP_AREA_FRAC, &   ! FRACTION OF TOTAL AREA TO AVERAGE AREA THAT PRECIP FALLS OVER
    !                 !
    !                 RELEASE_LIMIT=> SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_VOL , &   !STRUCTURAL LIMIT TO RESERVOIR RELEASE
    !                 RELEASE_LIM_S=> SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_S   , &   ! ANY LIMIT IMPOSED FROM THE S-LANGAGE
    !                 !
    !                 RELEASE_POT  => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_POT       &   ! FINAL POTENTIAL RELEASE FOR RESERVOIR
    !                )
    !                !
    !                AREA_AVG  = (AREA_START + AREA_END) * HALF
    !                !
    !                PRCP_AREA = PRCP_FRAC*(AREA_MAX - AREA_AVG) + AREA_AVG
    !                !
    !                PRCP = PRCP_LENTH*PRCP_AREA;  EVAP = EVAP_LENTH*AREA_AVG
    !                !
    !                RELEASE_POT = STORAGE - STORAGE_MIN + INFLOW + PRCP - EVAP !CONTINUITY
    !                !
    !                IF(RELEASE_POT > RELEASE_LIMIT) RELEASE_POT = RELEASE_LIMIT
    !                IF(RELEASE_POT > RELEASE_LIM_S) RELEASE_POT = RELEASE_LIM_S
    !                IF(RELEASE_POT < DZ           ) RELEASE_POT = DZ
    !                !
    !       END ASSOCIATE
    !    ELSE
    !        SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_POT = DZ
    !    END IF
    !END DO
    !END DO
    !
    ! Set up any additional releases that maybe availible -------------------------------------------------------------------------
    !
    DO CONCURRENT(I = 1:NPROJ)
    DO CONCURRENT(J = 1:SWO%NRES_BAL(I))
        SWO%RESDAT(I)%RESBAL(J)%RELEASE_ADDF_INI = DZ
    END DO
    END DO
    !
    ! ADD ADDITIONAL RELEASES -----------------------------------------------------------------------------------------------------
    !
    IF(SWO%HAS_ADD_RELEASE_S) THEN
       DO I=1, NPROJ
       DO J=1, SWO%NRES_BAL(I)
         IF(SWO%RESDAT(I)%RESBAL(J)%INUSE) SWO%RESDAT(I)%RESBAL(J)%RELEASE_ADDF_INI = SWO%RESDAT(I)%RESBAL(J)%RELEASE_ADDF_INI + SWO%RESDAT(I)%RESBAL(J)%ADD_RELEASE_S_VOL !+...
       END DO
       END DO
    END IF
    !
    ! ESTABLISH THE INTIAL GUESS FOR ADDITIONAL RELEASES -------------------------------------------------------------------------
    !
    DO CONCURRENT(I = 1:NPROJ)
    DO CONCURRENT(J = 1:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%INUSE)
        !
        SWO%RESDAT(I)%RESBAL(J)%RELEASE_ADDF = SWO%RESDAT(I)%RESBAL(J)%RELEASE_ADDF_INI
        !
    END DO
    END DO
    !
    ! PROJECT releated additional releases - Reset
    !
    !
    IF(SWO%HAS_PROJ_RELEASE_ADD) THEN
       DO CONCURRENT (I = 1:NPROJ)
                           SWO%RESDAT(I)%PROJ_RELEASE_ADD = SWO%RESDAT(I)%PROJ_RELEASE_ADD_S * SWO%DELT  !Covert rate to volume
       END DO
       DO CONCURRENT (I = 1:NPROJ, SWO%RESDAT(I)%PROJ_RELEASE_ADD < NEARZERO_5)
                                   SWO%RESDAT(I)%PROJ_RELEASE_ADD = DZ
       END DO
    ELSE
        DO CONCURRENT (I = 1:NPROJ)
                           SWO%RESDAT(I)%PROJ_RELEASE_ADD = DZ
        END DO
    END IF

    !  --------------------------------------------------------------------- -----------------------------------------------------
    !
    !
    ! COMPUTE ANY REQURIED RELEASES TO MAINTAIN STREAM FLOWS
    !
    IF(SWO%REQFLOW%HAS_REQ) THEN
      !
      IF(KITER > NINE) THEN                ! START REQUIRED FLOWS ON 8th ITERATION
        !
        SWO%REQFLOW%REL = DZ  !NECESSARY RELEASE FOR REQUIRED FLOW --TOTAL SHORTFALL PER "ORDER" UNIQUE REQUIRED PATHWAYS
        !
        IF(TRUE) THEN               ! Have 8 iterations that use full network to determine release, then make adjustments afterwards
          !
          DO ITER=ONE, SIZE(SWO%REQFLOW%ORDER)  !NUMBER OF UNIQUE REQUIRED PATHWAYS
              UnderRelease = DZ   !LARGEST  SHORTFALL
              OverRelease = inf   !SMALLEST Overshoot of required flow
              !
              !!!DO TMP = SWO%REQFLOW%ORDER(ITER)%N, ONE, NEG  ! ONE, SWO%REQFLOW%ORDER(ITER)%N  !NUMBER OF REQUIRED FLOWS ALONG SAME STREAM
              !!!    !
              !!!    IREQ = SWO%REQFLOW%ORDER(ITER)%VEC(TMP)  !ITER = PATHWAY, TMP IS THE SPECIFIC "IREQ" IN THE PATHWAY
              !!!    !
              !!!    IF(SWO%REQFLOW%REQ(IREQ) > NEARZERO_5) THEN
              !!!        !
              !!!        ISTR = SWO%SEGRCH_IN(SWO%REQFLOW%HEDSEG(IREQ)) !Upper Most common Seg
              !!!        !
              !!!        JSTR = SWO%REQFLOW%ISTRM(IREQ)
              !!!        !
              !!!        DSEEP = STRM(10, ISTR) - STRM(10, JSTR) !Gains and Losses from Common Seg to Required Seg
              !!!        !
              !!!        IF(DSEEP < NEARZERO_5) DSEEP = DZ  !Only care if its losing
              !!!        !
              !!!        DFLOW = SWO%REQFLOW%REQ(IREQ) - STRM(10, JSTR)   !SHORTFALL OF CURRENT FLOW TO REQUIRED FLOW
              !!!        !
              !!!        IF(DFLOW > DZ) THEN !SHORTFALL
              !!!            !
              !!!            IF( DFLOW + DSEEP - SWO%REQFLOW%REL_OLD(ITER) > NEARZERO_5) THEN  !INclude seepage + previous release
              !!!                !
              !!!                DFLOW = DFLOW + DSEEP - SWO%REQFLOW%REL_OLD(ITER)
              !!!            END IF
              !!!            !
              !!!            IF(UnderRelease < DFLOW) UnderRelease = DFLOW
              !!!        ELSE
              !!!             DFLOW = DNEG*DFLOW  !It is an over release
              !!!             !
              !!!             IF ( DFLOW/SWO%REQFLOW%REQ(IREQ) > TENTH) THEN  !Over Release is greather than 10% of target required release
              !!!                  !
              !!!                  IF(OverRelease > DFLOW) OverRelease = DFLOW  !Store smallest over release for correction
              !!!             END IF
              !!!        END IF
              !!!    END IF
              !!!END DO
              !
              DO TMP = SWO%REQFLOW%ORDER(ITER)%N, ONE, NEG   ! Move from upper most required flow to downstream most
                  !
                  IREQ = SWO%REQFLOW%ORDER(ITER)%VEC(TMP)  !ITER = PATHWAY, TMP IS THE SPECIFIC "IREQ" IN THE PATHWAY
                  !
                  IF(SWO%REQFLOW%REQ(IREQ) > NEARZERO_5) THEN
                        !
                        INFLOW = DBLE( STRM(10,SWO%REQFLOW%ISTRM(IREQ)) ) !GET FLOW THAT COMES INTO REACH
                        !
                        DFLOW = SWO%REQFLOW%REQ(IREQ) - INFLOW            !SHORTFALL OF CURRENT FLOW TO REQUIRED FLOW
                        !
                        JSEG = SWO%REQFLOW%SEG(IREQ)  !Required Flow Seg
                        !
                        IF(DFLOW > DZ) THEN !SHORTFALL - Calc additional release
                           !
                           !!!!!!DSEEP = DZ
                           !!!!!!RATIO = DZ
                           !!!!!!!
                           !!!!!!IF(SWO%REQFLOW%RCH(IREQ).NE.ONE) THEN
                           !!!!!!    !
                           !!!!!!    !ISTR = SWO%SEGRCH_IN(JSEG)
                           !!!!!!    !JSTR = SWO%REQFLOW%ISTRM(IREQ)
                           !!!!!!    !
                           !!!!!!    !DSEEP = DSEEP + STRM(10,ISTR) - STRM(10,JSTR)  ! Seg Inflow - Flow at Required PointReach's IN - OUT
                           !!!!!!    DO ISTR = SWO%SEGRCH_IN(JSEG), SWO%REQFLOW%ISTRM(IREQ)-ONE !Add partial segment parts
                           !!!!!!       !
                           !!!!!!       RTOL = STRM(10,ISTR)
                           !!!!!!       IF(RTOL < NEGNEARZERO_12 .OR. NEGNEARZERO_12 < RTOL) THEN
                           !!!!!!           !
                           !!!!!!           RATIO = RATIO + STRM(1,ISTR)
                           !!!!!!           DSEEP = DSEEP + STRM(1,ISTR)*STRM(11,ISTR)/RTOL  ! NOTE: SFR specifies seepage LOSS as POSITIVE
                           !!!!!!       END IF
                           !!!!!!       !
                           !!!!!!    END DO
                           !!!!!!END IF
                           !!!!!!!
                           !!!!!!ISEG  = SWO%REQFLOW%HEDSEG(IREQ)  !Search from upper most common point to required flow segment
                           !!!!!!NTREE = SWO%DNTREE_NAT(ISEG)%NTREE
                           !!!!!!IF(ISEG .NE. JSEG) THEN
                           !!!!!!    !
                           !!!!!!    !ISTR = SWO%SEGRCH_IN (ISEG)
                           !!!!!!    !JSTR = SWO%SEGRCH_OUT(ISEG)
                           !!!!!!    !DSEEP = DSEEP + STRM(10,ISTR) - STRM(9, JSTR)
                           !!!!!!    DO CONCURRENT (ISTR = SWO%SEGRCH_IN(ISEG):SWO%SEGRCH_OUT(ISEG))
                           !!!!!!        !
                           !!!!!!        RTOL = STRM(10,ISTR)
                           !!!!!!        IF(RTOL < NEGNEARZERO_12 .OR. NEGNEARZERO_12 < RTOL) THEN
                           !!!!!!            !
                           !!!!!!            RATIO = RATIO + STRM(1,ISTR)
                           !!!!!!            DSEEP = DSEEP + STRM(1,ISTR)*STRM(11,ISTR)/RTOL  ! NOTE: SFR specifies seepage LOSS as POSITIVE
                           !!!!!!        END IF
                           !!!!!!    END DO
                           !!!!!!    !
                           !!!!!!    DO J=ONE, NTREE
                           !!!!!!        !
                           !!!!!!        KSEG = SWO%UPTREE_NAT(ISEG)%SEGTREE(J)  !NEXT DNSTREAM NATURAL SEGMENT
                           !!!!!!        !
                           !!!!!!        IF( KSEG == JSEG ) EXIT
                           !!!!!!        !
                           !!!!!!        !ISTR = SWO%SEGRCH_IN (KSEG)
                           !!!!!!        !JSTR = SWO%SEGRCH_OUT(KSEG)
                           !!!!!!        !DSEEP = DSEEP + STRM(10,ISTR) - STRM(9, JSTR)
                           !!!!!!        DO CONCURRENT (ISTR = SWO%SEGRCH_IN(KSEG):SWO%SEGRCH_OUT(KSEG))
                           !!!!!!            !
                           !!!!!!            RTOL = STRM(10,ISTR)
                           !!!!!!            IF(RTOL < NEGNEARZERO_12 .OR. NEGNEARZERO_12 < RTOL) THEN
                           !!!!!!                !
                           !!!!!!                RATIO = RATIO + STRM(1,ISTR)
                           !!!!!!                DSEEP = DSEEP + STRM(1,ISTR)*STRM(11,ISTR)/RTOL  ! NOTE: SFR specifies seepage LOSS as POSITIVE
                           !!!!!!            END IF
                           !!!!!!        END DO
                           !!!!!!    END DO
                           !!!!!!END IF
                           !!!!!!IF(RATIO < NEGNEARZERO_12 .OR. NEARZERO_12 < RATIO) THEN
                           !!!!!!    DSEEP = DSEEP/RATIO
                           !!!!!!ELSE
                           !!!!!!    DSEEP = UNO
                           !!!!!!END IF
                           !!!!!!IF(TENTH < DSEEP .AND. DSEEP < UNO ) THEN
                           !!!!!!                                      DFLOW = DFLOW / DSEEP  !Only care if its losing
                           !!!!!!ELSEIF(DSEEP <=TENTH ) THEN
                           !!!!!!                                      DFLOW = DFLOW / DIEZ  !Only care if its losing
                           !!!!!!END IF
                           !!!
                           !!IF(DSEEP < NEARZERO_5) DSEEP = DZ  !Only care if its losing
                           !!!
                           !!IF( DFLOW + DSEEP - SWO%REQFLOW%REL_OLD(ITER) > NEARZERO_5) THEN  !INclude seepage + previous release
                           !!    !
                           !!    DFLOW = DFLOW + DSEEP - SWO%REQFLOW%REL_OLD(ITER)
                           !!END IF
                           !!!
                           !
                           IF(UnderRelease < DFLOW) UnderRelease = DFLOW
                        ELSE
                           DFLOW = DNEG*DFLOW  !It is an over release
                           !
                           IF(OverRelease > DFLOW) OverRelease = DFLOW  !Store smallest over release for correction
                           !
                           !IF ( DFLOW > TENTH) THEN  !Over Release is greather than 0.1 L3/T
                           !     !
                           !     IF(OverRelease > DFLOW) OverRelease = DFLOW  !Store smallest over release for correction
                           !END IF
                        END IF
                        !DTMP=86400D0
                        !RELEASE_MAX =  DELT_INV * (SWO%RESDAT(1)%RESBAL(1)%RELEASE_TOT + SWO%RESDAT(1)%RESBAL(2)%RELEASE_TOT)/DTMP
                        !IF(OverRelease> 1D30) THEN
                        !WRITE(123,'(3I5,*(2x F9.3))') KPER, KSTP, KITER,RELEASE_MAX, SWO%REQFLOW%REL_OLD(ITER)/DTMP,INFLOW/DTMP,SWO%REQFLOW%REQ(IREQ)/DTMP, DSEEP/DTMP, UnderRelease/DTMP, 0.0, DELT_INV *SWO%RESDAT(1)%RESBAL(1)%RELEASE_TOT/DTMP, DELT_INV *SWO%RESDAT(1)%RESBAL(2)%RELEASE_TOT/DTMP
                        !ELSE
                        !WRITE(123,'(3I5,*(2x F9.3))') KPER, KSTP, KITER,RELEASE_MAX, SWO%REQFLOW%REL_OLD(ITER)/DTMP,INFLOW/DTMP,SWO%REQFLOW%REQ(IREQ)/DTMP, DSEEP/DTMP, UnderRelease/DTMP, OverRelease/DTMP, DELT_INV *SWO%RESDAT(1)%RESBAL(1)%RELEASE_TOT/DTMP, DELT_INV *SWO%RESDAT(1)%RESBAL(2)%RELEASE_TOT/DTMP
                        !END IF
                        !
                  END IF ! (SWO%REQFLOW%REQ(IREQ) > NEARZERO_5) THEN
              END DO     ! TMP = SWO%REQFLOW%ORDER(ITER)%N, ONE, NEG
              !
              !IF(OverRelease  < NEARZERO_5) OverRelease = DZ
              IF(OverRelease  > D100      ) OverRelease = DZ
              !
              I = ITER ! UNIQUE REQUIRED PATHWAYS GROUP
              !
              IF(UnderRelease > NEARZERO_5) THEN !HAVE TO RELEASE MORE WATER -- NO SHORTFALLS ACCEPTIBLE
                  !
                  !IF(KITER < 12) UnderRelease = UnderRelease * FOURTH
                  !
                  SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) + UnderRelease     !NOTE IS THE AMMOUNT OF SHORTFALL FROM THE PREIVOUS SET OF RELEASES
                  !
              ELSEIF(SWO%REQFLOW%REL_OLD(I) < NEARZERO_5 ) THEN                  !REQUIRED FLOW TOO SMALL TO BE WORTH RELEASING
                  !
                  SWO%REQFLOW%REL(I) = DZ
                  !
              ELSEIF(OverRelease > NEARZERO_5) THEN                 !OverRelease MAKES UP MORE THEN 0.1% OF OLD REQUIRED RELEASE
                  !
                  DFLOW = SWO%REQFLOW_RELAX*OverRelease                 ! DECREASE RELEASE BY SWO%REQFLOW_RELAX % OF THE OverRelease
                  !
                  IF(DFLOW > SWO%REQFLOW%REL_OLD(I)) THEN   ! Correction turns off required releases
                             SWO%REQFLOW%REL(I) = DZ
                  ELSE
                             SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) - DFLOW !Reduce release by Overrelease
                  END IF
              ELSE
                  SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) !REUSE RELEASE
              END IF
          END DO
          !
        ELSE !IF(KITER >= 12) THEN        ! Only make adjustments based on shortfall -------------------------------------
          !
          DO ITER=ONE, SIZE(SWO%REQFLOW%ORDER)  !NUMBER OF UNIQUE REQUIRED PATHWAYS
              UnderRelease = DZ   !LARGEST  SHORTFALL
              OverRelease = inf   !SMALLEST Overshoot of required flow
              DO TMP = ONE, SWO%REQFLOW%ORDER(ITER)%N  !NUMBER OF REQUIRED FLOWS ALONG SAME STREAM
                  !
                  IREQ = SWO%REQFLOW%ORDER(ITER)%VEC(TMP)  !ITER = PATHWAY, TMP IS THE SPECIFIC "IREQ" IN THE PATHWAY
                  !
                  IF(SWO%REQFLOW%REQ(IREQ) > NEARZERO_5) THEN
                        !
                        INFLOW = DBLE( STRM(10,SWO%REQFLOW%ISTRM(IREQ)) ) !GET FLOW THAT COMES INTO REACH
                        !
                        DFLOW = SWO%REQFLOW%REQ(IREQ) - INFLOW            !SHORTFALL OF CURRENT FLOW TO REQUIRED FLOW
                        !
                        IF(DFLOW > DZ) THEN !SHORTFALL
                             !RELEASE_REQF = RELEASE_REQF + DFLOW
                             IF(UnderRelease < DFLOW) UnderRelease = DFLOW
                             !
                        ELSEIF(FIVE_ITER) THEN
                             DFLOW = DNEG*DFLOW  !It is an over release
                             !
                             IF ( DFLOW/SWO%REQFLOW%REQ(IREQ) > TENTH) THEN  !Over Release is greather than 10% of target required release
                                  !
                                  IF(OverRelease > DFLOW) OverRelease = DFLOW  !Store smallest over release for correction
                             END IF
                        END IF
                  END IF
              END DO
              !
              IF(OverRelease < NEARZERO_5) OverRelease = DZ
              IF(OverRelease > D100      ) OverRelease = DZ
              !
              I = ITER ! UNIQUE REQUIRED PATHWAYS GROUP
              !
              IF(UnderRelease > NEARZERO_5) THEN !HAVE TO RELEASE MORE WATER -- NO SHORTFALLS ACCEPTIBLE
                  !
                  SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) + UnderRelease     !NOTE IS THE AMMOUNT OF SHORTFALL FROM THE PREIVOUS SET OF RELEASES
                  !
              ELSEIF(SWO%REQFLOW%REL_OLD(I) < NEARZERO_5 ) THEN                  !REQUIRED FLOW TOO SMALL TO BE WORTH RELEASING
                  !
                  SWO%REQFLOW%REL(I) = DZ
                  !
              ELSEIF(OverRelease/SWO%REQFLOW%REL_OLD(I) > FIFTH ) THEN                 !OverRelease MAKES UP MORE THEN 20% OF OLD REQUIRED RELEASE
                  !
                  DFLOW = SWO%REQFLOW_RELAX*OverRelease                 ! DECREASE RELEASE BY SWO%REQFLOW_RELAX % OF THE OverRelease
                  !
                  IF(DFLOW > SWO%REQFLOW%REL_OLD(I)) THEN   ! Correction turns off required releases
                             SWO%REQFLOW%REL(I) = DZ
                  ELSE
                             SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) - DFLOW !Reduce release by Overrelease
                  END IF
                  !
                  !!!!
                  !!!SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) - FIFTH*OverRelease !ONLY DECRESS RELEASE BY 20% OF THE OVER RELEASE
                  !!!!
                  !!!IF(SWO%REQFLOW%REL(I) < DZ) SWO%REQFLOW%REL(I) = FIFTH*SWO%REQFLOW%REL_OLD(I)  !NEGATIVE FLOW IS AN OVER CORRECTION, SO USE 20% OF PREVIOUS RELEASE
              ELSE
                  SWO%REQFLOW%REL(I) = SWO%REQFLOW%REL_OLD(I) !REUSE RELEASE
              END IF
          END DO
          !
        END IF  ! (KITER < 12) THEN
        !
        ! DETERMINE SPREAD OF REQUIRED FLOW RELEASES FROM RESERVOIRS (RELEASE_REQF)
        !
        DO CONCURRENT(I = 1:NPROJ)
        DO CONCURRENT (J = 1:SWO%NRES_BAL(I)); SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQF = DZ
        END DO;END DO
        !
        DO ITER=ONE, SIZE(SWO%REQFLOW%ORDER)  !NUMBER OF UNIQUE REQUIRED PATHWAYS
                !
                REQ_FLOW_RELEASE = SWO%REQFLOW%REL(ITER) * SWO%DELT   ! REQUIRED RELEASE DEMANDED --AS A VOLUME!!!
                !
                IREQ = SWO%REQFLOW%ORDER(ITER)%VEC(ONE)  !ITER = PATHWAY, ---EACH OF THE IREQ SHOULD HAVE THE SAME RESERVOIRS SO ONLY USE FURTHEST DOWNSTREAM "ONE"
                !
                DTMP = DZ                             !RESIDUAL RELEASE -- BECOMES ZERO WHEN REQ_FLOW_RELEASE IS FULLY MET
                I = Z                                 !NUMBER OF RES THAT ARE NOT AT RELEASE_POT
                DO TMP = ONE, SWO%REQFLOW%RES(IREQ)%N !NUMBER OF RES ASSOCIATED WITH IREQ
                       !
                       IPROJ = SWO%RESBAL2PROJ(ONE, SWO%REQFLOW%RES(IREQ)%VEC(TMP))  ! RES ID = SWO%REQFLOW%RES(IREQ)%VEC(TMP)
                       IRES  = SWO%RESBAL2PROJ(TWO, SWO%REQFLOW%RES(IREQ)%VEC(TMP))
                       !
                       ASSOCIATE(RESBAL => SWO%RESDAT(IPROJ)%RESBAL(IRES))
                              IF(RESBAL%INUSE) THEN
                                    !
                                    RESBAL%RELEASE_REQF = RESBAL%RELEASE_REQF + REQ_FLOW_RELEASE * RESBAL%REL_REQ_FRAC  !MAXED OUT THE RELEASES
                                    IF( RESBAL%RELEASE_REQF > RESBAL%RELEASE_POT) THEN
                                        DTMP = DTMP + RESBAL%RELEASE_REQF - RESBAL%RELEASE_POT
                                        RESBAL%RELEASE_REQF = RESBAL%RELEASE_POT
                                    ELSE
                                        I = I + ONE
                                    END IF
                              END IF
                       END ASSOCIATE
                END DO
                !
                DO WHILE (DTMP > NEARZERO_5 .AND. I > Z)
                    IF(I==ONE) THEN !ONLY ONE RERVOIR HAS ROOM FOR MORE RELEASE
                                  DO TMP = ONE, SWO%REQFLOW%RES(IREQ)%N
                                         !
                                         IPROJ = SWO%RESBAL2PROJ(ONE, SWO%REQFLOW%RES(IREQ)%VEC(TMP))
                                         IRES  = SWO%RESBAL2PROJ(TWO, SWO%REQFLOW%RES(IREQ)%VEC(TMP))
                                         !
                                         ASSOCIATE(RESBAL => SWO%RESDAT(IPROJ)%RESBAL(IRES))
                                            IF(RESBAL%RELEASE_REQF .NE. RESBAL%RELEASE_POT .AND. RESBAL%INUSE) THEN
                                                 !
                                                 RESBAL%RELEASE_REQF = RESBAL%RELEASE_REQF + DTMP
                                                 !
                                                 IF( RESBAL%RELEASE_REQF > RESBAL%RELEASE_POT) THEN
                                                     DTMP = RESBAL%RELEASE_REQF - RESBAL%RELEASE_POT
                                                     RESBAL%RELEASE_REQF = RESBAL%RELEASE_POT
                                                 END IF
                                            END IF
                                         END ASSOCIATE
                                  END DO
                                  I = Z    !EXIT LOOP
                    ELSE
                        DTMP3 = DZ  !PRORATE EXCESS BY OTHER RESERVOIRS MAX RELEASE
                        DO TMP = ONE, SWO%REQFLOW%RES(IREQ)%N
                               !
                               IPROJ = SWO%RESBAL2PROJ(ONE, SWO%REQFLOW%RES(IREQ)%VEC(TMP))
                               IRES  = SWO%RESBAL2PROJ(TWO, SWO%REQFLOW%RES(IREQ)%VEC(TMP))
                               !
                               ASSOCIATE(RESBAL => SWO%RESDAT(IPROJ)%RESBAL(IRES))
                                  IF(RESBAL%RELEASE_REQF .NE. RESBAL%RELEASE_POT .AND. RESBAL%INUSE) THEN
                                      DTMP3 = DTMP3 + RESBAL%REL_REQ_FRAC
                                  END IF
                               END ASSOCIATE
                        END DO
                        !
                        IF( DTMP3 > DZ) THEN
                            DTMP1 = DTMP
                            DTMP  = DZ
                            I = Z
                            DO TMP = ONE, SWO%REQFLOW%RES(IREQ)%N
                                   K     = SWO%REQFLOW%RES(IREQ)%VEC(TMP)
                                   IPROJ = SWO%RESBAL2PROJ(ONE,K)
                                   IRES  = SWO%RESBAL2PROJ(TWO,K)
                                   !
                                   ASSOCIATE(RESBAL => SWO%RESDAT(IPROJ)%RESBAL(IRES))
                                      IF(RESBAL%RELEASE_REQF .NE. RESBAL%RELEASE_POT .AND. RESBAL%INUSE) THEN
                                          !
                                          DTMP2 = RESBAL%REL_REQ_FRAC/DTMP3  !RATIO OF REL_REQ_FRAC/ SUM OF NOT MAXED RES RELEASE FRACs
                                          !
                                          RESBAL%RELEASE_REQF = RESBAL%RELEASE_REQF + (DTMP1*DTMP2)  ! DTMP1 = RELEASE SHORTFALL, DTMP2 = FRACTION OF RELEASE FOR RES
                                          !
                                          IF( RESBAL%RELEASE_REQF > RESBAL%RELEASE_POT) THEN
                                              DTMP = DTMP + RESBAL%RELEASE_REQF - RESBAL%RELEASE_POT
                                              RESBAL%RELEASE_REQF = RESBAL%RELEASE_POT
                                          ELSE
                                              I = I + ONE
                                          END IF
                                      END IF
                                   END ASSOCIATE
                            END DO
                        ELSE
                            I = Z  !EXIT LOOP --MAXED OUT RELEASES
                        END IF
                    END IF
                END DO
        END DO  ! ITER=ONE, SIZE(SWO%REQFLOW%ORDER)  !NUMBER OF UNIQUE REQUIRED PATHWAYS
      END IF  ! KITER > SEV
      END IF ! SWO%REQFLOW%HAS_REQ
    !!!!!IF(SWO%REQFLOW%HAS_REQ) THEN
    !!!!!  !
    !!!!!  IF(KITER > SEV) THEN   !START REQUIRED FLOWS ON 8th ITERATION
    !!!!!      !
    !!!!!      DO CONCURRENT(IPROJ = 1:NPROJ)
    !!!!!      DO CONCURRENT (IRES = 1:SWO%NRES_BAL(IPROJ)); SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF = DZ
    !!!!!      END DO;END DO
    !!!!!      !
    !!!!!      SWO%REQFLOW%REL = DZ  !NECESSARY RELEASE FOR REQUIRED FLOW
    !!!!!      !
    !!!!!      DO I=ONE, SWO%REQFLOW%N
    !!!!!         IF(SWO%REQFLOW%REQ(I) > NEARZERO_5) THEN
    !!!!!               !
    !!!!!               INFLOW = STRM(10,SWO%REQFLOW%ISTRM(I))  !GET FLOW THAT COMES INTO REACH
    !!!!!               !
    !!!!!               ISEG = SWO%REQFLOW%SEG(I)   !ISEG
    !!!!!               !IRCH = SWO%REQFLOW%SEGRCH(TWO,I)  !IRCH
    !!!!!               !
    !!!!!               DFLOW = INFLOW - SWO%REQFLOW%REQ(I)  !DEMANDED REQUIRED FLOW
    !!!!!               !
    !!!!!               IF(DFLOW < DZ) THEN !SHORTFALL
    !!!!!                   !
    !!!!!                   ! sum seepage over upstream river network ... to common point
    !!!!!                   ! ... divseg's iupseg ...
    !!!!!
    !!!!!                   DSEEP    = DZ
    !!!!!                   IRCH     = SWO%SEGRCH_IN(ISEG)                                ! reach index @ rch 1 of seg
    !!!!!                   JRCH     = SWO%REQFLOW%ISTRM(I)
    !!!!!                   !
    !!!!!                   IF(JRCH>ONE) JRCH = JRCH - ONE
    !!!!!                   !
    !!!!!                   DO CONCURRENT (ISTR  = IRCH:JRCH)
    !!!!!                     DSEEP  = DSEEP - STRM(11,ISTR)                          ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
    !!!!!                   END DO
    !!!!!                   ! ... uptree @ iupseg ...
    !!!!!                   !
    !!!!!                   DO ITREE = ONE, SWO%UPTREE_NAT(ISEG)%NTREE
    !!!!!                     !
    !!!!!                     KSEG   = SWO%UPTREE_NAT(ISEG)%SEGTREE(ITREE)
    !!!!!                     IRCH   = SWO%SEGRCH_IN(KSEG)
    !!!!!                     JRCH   = SWO%SEGRCH_OUT(KSEG)
    !!!!!                     DO CONCURRENT (ISTR = IRCH:JRCH);  DSEEP = DSEEP - STRM(11,ISTR)  ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so LOSS is NEGATIVE
    !!!!!                     END DO !(ISTR)
    !!!!!                     !
    !!!!!                     IF(KSEG == SWO%REQFLOW%HEDSEG(I)) EXIT  !REACHED COMMON POINT FROM RESERVOIR RELEASES
    !!!!!                   END DO !(ITREE)
    !!!!!                   !
    !!!!!                   ! sum specified inflows to upstream river network ...
    !!!!!                   !
    !!!!!                   DFLOW_IN = DZ
    !!!!!                   ! ... divseg ...              !RRFLAG = 1 if RES release seg
    !!!!!                   IF (IDIVAR(1,ISEG) == Z .AND. SWO%SEGDATA(ISEG)%RRFLAG.NE.ONE ) DFLOW_IN = DFLOW_IN + SEG(2,ISEG)
    !!!!!                   !
    !!!!!                   DO ITREE = ONE, SWO%UPTREE_NAT(ISEG)%NTREE
    !!!!!                     KSEG   = SWO%UPTREE_NAT(ISEG)%SEGTREE(ITREE)
    !!!!!                     IF ( IDIVAR(1,KSEG) == Z .AND. SWO%SEGDATA(KSEG)%RRFLAG.NE.ONE )  DFLOW_IN = DFLOW_IN + SEG(2,KSEG)
    !!!!!                     !
    !!!!!                     IF(KSEG == SWO%REQFLOW%HEDSEG(I)) EXIT  !REACHED COMMON POINT FROM RESERVOIR RELEASES
    !!!!!                   END DO !(ITREE)
    !!!!!                   !
    !!!!!                   ! sum routed inflows to upstream river network ...
    !!!!!                   !
    !!!!!                   DFLOW_RT = DZ
    !!!!!                   ! ... divseg's iupseg ...
    !!!!!                   NLIST    = SWO%UPSEG(ISEG)%NLIST
    !!!!!                   DO CONCURRENT (ILIST = 1:NLIST)
    !!!!!                     KSEG   = SWO%UPSEG(ISEG)%SEGLIST(ILIST)
    !!!!!                     JRCH   = SWO%SEGRCH_OUT(KSEG)
    !!!!!                     IF ( SWO%SEGDATA(KSEG)%OUTSEG.EQ.ISEG .AND. (SWO%SEGINFO(KSEG)%SegType.NE.1)) THEN  ! routed inflow to divseg's iupseg (ISEG) ... EXCLUDING inflow from upstream river segments!
    !!!!!                           DFLOW_RT = DFLOW_RT + STRM(9,JRCH)
    !!!!!                     END IF !(SegType.NE.1 / OUTSEG.EQ.ISEG)
    !!!!!                   END DO !(ILIST)
    !!!!!                   ! ... uptree @ iupseg ...
    !!!!!                   NTREE    = SWO%UPTREE_NAT(ISEG)%NTREE
    !!!!!                   DO ITREE = ONE, NTREE
    !!!!!                     KSEG   = SWO%UPTREE_NAT(ISEG)%SEGTREE(ITREE)
    !!!!!                     NLIST  = SWO%UPSEG(KSEG)%NLIST
    !!!!!                     DO CONCURRENT (ILIST = 1:NLIST)
    !!!!!                       LSEG   = SWO%UPSEG(KSEG)%SEGLIST(ILIST)
    !!!!!                       JRCH   = SWO%SEGRCH_OUT(LSEG)
    !!!!!                       IF ( SWO%SEGDATA(LSEG)%OUTSEG.EQ.KSEG .AND. SWO%SEGINFO(LSEG)%SegType.NE.1 ) THEN ! routed inflow to segment in uptree @ iupseg (JSEG) ... EXCLUDING inflow from upstream river segments!
    !!!!!                         DFLOW_RT = DFLOW_RT + STRM(9,JRCH)
    !!!!!                       END IF !(SegType.NE.1 / OUTSEG.EQ.KSEG)
    !!!!!                     END DO !(ILIST)
    !!!!!                     !
    !!!!!                     IF(KSEG == SWO%REQFLOW%HEDSEG(I)) EXIT  !REACHED COMMON POINT FROM RESERVOIR RELEASES
    !!!!!                   END DO !(ITREE)
    !!!!!                   !
    !!!!!                   DTMP  = (DFLOW - DSEEP - DFLOW_IN - DFLOW_RT)
    !!!!!                   !
    !!!!!               ELSEIF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF_PEV > DZ) THEN
    !!!!!                   SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF_PEV
    !!!!!               END IF
    !!!!!         END IF
    !!!!!      END DO
    !!!!!  END IF
    !!!!!END IF
    !
    !
    ! ****************************************************************
    ! COMPUTE RESERVOIR RELEASE, RESERVOIR MASS BALANCE ...
    ! (8)  Compute maximum possible release ...
    !      MaxRelease  = STORAGE[t]                   (end of prev step)
    !                    - STORAGE_NPROJ[t]           (current step)
    !                    - RELEASE_SPEC[t]            (current step)
    !                    + Inflow + Prcp*A - Evap*A   (current step, iterate @ A)
    ! (9)  Compute demand-driven release
    !      ReleaseDMND = PROJ_DIVREQ                  (demand-driven diversion request, constrained by allotments)
    !                    / PROJ_DIVRATIO              (project diversion ratio)
    ! (10) Compute estimated Project release
    !      ReleaseEst  = max( 0., min( MaxRelease, ReleaseDMND) )
    !      NOTE -- Diversion request should be constrained such that
    !              ReleaseDMND is always less than MaxRelease ...
    !              If ReleaseEst < ReleaseDMND, may indicate bug in code...!
    ! (11) Compute reservoir mass balance
    !      (initial guess, first update, then iterate to closure)
    !      (includes iterative calculation of reservoir spills)
    !      Storage[t+1]= STORAGE[t]                    (end of prev step)
    !                  + Inflow[t] + Prcp[t]*A[t]
    !                  - ReleasePRJ[t] - ReleaseNPRJ[t] - ReleaseFLD- Evap[t]*A[t]
    ! ****************************************************************
    !
    ! TODO
    ! Current release calculation doesn't account for multiple releases
    ! supporting a given project ... need to constrain release from each
    ! reservoir for general use. Currently allows release up to full
    ! allocation (or demand) from *EACH* reservoir, rather than from
    ! *ALL* reservoirs.
    !
    ! Loop over projects, mass-balance reservoirs
    !
    DO CONCURRENT (IDVS = ONE:SWO%DIVCOUNT)
          ! zero gain/loss and inflow components ...
          SWO%DIVSEG(IDVS)%UP_DIVORDER = DZ
          SWO%DIVSEG(IDVS)%UP_DSEEP    = DZ
          SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DZ
          SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DZ
          SWO%DIVSEG(IDVS)%UP_DFLOW_RN = DZ
          SWO%DIVSEG(IDVS)%UP_DFLOW_ET = DZ
    END DO
    !
    ! Calculate Total Project Storage and Max Possible Project Releases
    !
    DO P=ONE, NPROJ
       !
       ASSOCIATE(RESBAL           => SWO%RESDAT(P)%RESBAL,             &
                 PROJ_RELEASE_MAX => SWO%RESDAT(P)%PROJ_RELEASE_MAX,   &  ! MAX POTENTIAL RELEASE FOR PROEJECT
                 PROJ_STORAGE_TOT => SWO%RESDAT(P)%PROJ_STORAGE_TOT    &  ! TOTAL PROJECT STORAGE
                )
             !
             PROJ_RELEASE_MAX = DZ
             PROJ_STORAGE_TOT = DZ
             !
             ! Calculate Total Project Storage and Max Possible Project Releases
             !
             DO J=ONE, SWO%NRES_BAL(P)
             IF(SWO%RESDAT(P)%RESBAL(J)%INUSE) THEN
               !
               ASSOCIATE(STORAGE          => SWO%RESDAT(P)%RESBAL(J)%STORAGE_PREV,    &  ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
                         RELEASE_POT      => SWO%RESDAT(P)%RESBAL(J)%RELEASE_POT ,    &
                         RELEASE_ADDF      => SWO%RESDAT(P)%RESBAL(J)%RELEASE_ADDF ,  &
                         RELEASE_REQF     => SWO%RESDAT(P)%RESBAL(J)%RELEASE_REQF ,   &
                         RELEASE_SPEC     => SWO%RESDAT(P)%RESBAL(J)%RELEASE_SPEC,    &
                         RELEASE_PROJ_POT => SWO%RESDAT(P)%RESBAL(J)%RELEASE_PROJ_POT &  !POTENTIAL RELEASES AVAILBLE FOR PROJECT WATER
                        )
                        !
                         RELEASE_PROJ_POT = RELEASE_POT - RELEASE_ADDF - RELEASE_REQF - RELEASE_SPEC ! exclude non-project related releases ...
                         !
                         IF(RELEASE_PROJ_POT < DZ) RELEASE_PROJ_POT = DZ
                         !
                         PROJ_RELEASE_MAX = PROJ_RELEASE_MAX + RELEASE_PROJ_POT
                         !
                         PROJ_STORAGE_TOT = PROJ_STORAGE_TOT + STORAGE
               END ASSOCIATE
             END IF
             END DO
       END ASSOCIATE
    END DO
    !
    ! Calculate Total Project Storage and Max Possible Project Releases
    !
    DO P=ONE, SWO%NPROJ  !compiler error required changing I to P
       !
       ASSOCIATE(PROJ_DIV_ORDER   => SWO%PROJ  (P)%DIVORDER,         &  !DIVERSION ORDERS FOR PROJECT
                 RESBAL           => SWO%RESDAT(P)%RESBAL,           &
                 PROJ_RELEASE_MAX => SWO%RESDAT(P)%PROJ_RELEASE_MAX, &  ! MAX POTENTIAL RELEASE FOR PROEJECT
                 PROJ_RELEASE_DMD => SWO%RESDAT(P)%PROJ_RELEASE_DMD  &  ! DEMANDED PROJECT RELEASE TO MEET DIVERSION ORDERS --INCLUDES GAINS/LOSSES
                )
             !
             ! FIRST DETERMINE NECESSARY RELEASE FOR PROJECT TO MEET DEMANDS
             !
             ! Set release to zero if diversion order is less than 1 AF (43560 ft3)
             IF (PROJ_DIV_ORDER.LE.SWO%MIN_PROJ_ORDER(P)) THEN
                                       PROJ_RELEASE_DMD = DZ
             ELSEIF ( KITER.LE.THREE ) THEN
                                       PROJ_RELEASE_DMD = PROJ_DIV_ORDER
             ELSE  !KITER.GT.3
               !
               ! Loop over diversion segments ...
               PROJ_RELEASE_DMD = DZ
               DO IDVS = ONE,SWO%DIVCOUNT
                 !
                 IF(P.NE.SWO%DIVSEG(IDVS)%PROJID) CYCLE  !ONLY CALCULATE DEMANDED TOTAL RELEASE FOR EACH PROJECT
                 !
                 ! get diversion segment, its IUPSEG, and it's NUPSEG
                 ISEG        = SWO%DIVSEG(IDVS)%DivSeg
                 JSEG        = SWO%SEGDATA(ISEG)%IUPSEG
                 NUPSEG_IDVS = SWO%UPTREE_NAT(JSEG)%NTREE + ONE
                 !
                 !
                 ! sum diversion orders at and upstream of IDVS
                 DDIVO   = SWO%DIVSEG(IDVS)%DIVORDER * SWO%DELT_INV !/ DELT                    ! Convert order to rate!
                 DO JDVS=1, SWO%DIVCOUNT
                 IF(IDVS.NE.JDVS .AND. P==SWO%DIVSEG(JDVS)%PROJID)  THEN !THIS ASSUMES ALL DIVERSIONS ARE ON SAME RIVER AND FED FROM SAME RESERVOIR
                   KSEG        = SWO%DIVSEG(JDVS)%DivSeg
                   TMPSEG      = SWO%SEGDATA(KSEG)%IUPSEG
                   NUPSEG_JDVS = SWO%UPTREE_NAT(TMPSEG)%NTREE + ONE
                   IF (NUPSEG_JDVS.LT.NUPSEG_IDVS) THEN
                     DTMP  = SWO%DIVSEG(JDVS)%DIVORDER * SWO%DELT_INV !/ DELT                  ! Convert order to rate
                     DDIVO = DDIVO + DTMP
                   END IF
                 END IF
                 END DO
                 SWO%DIVSEG(IDVS)%UP_DIVORDER = DDIVO * SWO%DELT
                 !
                 ! sum seepage over upstream river network ...
                 ! ... divseg's iupseg ...
                 DSEEP  =DZ
                 IRCH   =SWO%SEGRCH_IN(JSEG)                                ! reach index @ rch 1 of seg
                 JRCH   =SWO%SEGRCH_OUT(JSEG)                               ! reach index @ last reach of seg
                 DO ISTR=IRCH, JRCH
                   DSEEP=DSEEP - STRM(11,ISTR)                          ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
                 END DO
                 ! ... uptree @ iupseg ...
                 NTREE   =SWO%UPTREE_NAT(JSEG)%NTREE
                 DO ITREE=1, NTREE
                   KSEG  =SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
                   IRCH  =SWO%SEGRCH_IN(KSEG)
                   JRCH  =SWO%SEGRCH_OUT(KSEG)
                   DO ISTR=IRCH, JRCH
                     DSEEP=DSEEP - STRM(11,ISTR)                         ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so LOSS is NEGATIVE
                   END DO
                 END DO
                 SWO%DIVSEG(IDVS)%UP_DSEEP = DSEEP * SWO%DELT
                 !
                 ! sum runoff into upstream river network ...
                 ! ... divseg's iupseg ...
                 DFLOW_RN=DZ
                 IRCH    =SWO%SEGRCH_IN(JSEG)                                ! reach index @ rch 1 of seg
                 JRCH    =SWO%SEGRCH_OUT(JSEG)                               ! reach index @ last reach of seg
                 DO ISTR =IRCH, JRCH
                          DFLOW_RN = DFLOW_RN + STRM(12,ISTR)
                          DFLOW_RN = DFLOW_RN + STRM(24,ISTR)
                 END DO
                 ! ... uptree @ iupseg ...
                 NTREE   =SWO%UPTREE_NAT(JSEG)%NTREE
                 DO ITREE=1, NTREE
                   KSEG  =SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
                   IRCH  =SWO%SEGRCH_IN(KSEG)
                   JRCH  =SWO%SEGRCH_OUT(KSEG)
                   DO ISTR=IRCH, JRCH
                           DFLOW_RN = DFLOW_RN + STRM(12,ISTR)
                           DFLOW_RN = DFLOW_RN + STRM(24,ISTR)
                   END DO
                 END DO
                 SWO%DIVSEG(IDVS)%UP_DFLOW_RN = DFLOW_RN * SWO%DELT
                 !
                 ! sum Precip and ET into upstream river network ...
                 ! ... divseg's iupseg ...
                 DFLOW_ET=DZ
                 IRCH    =SWO%SEGRCH_IN(JSEG)                                ! reach index @ rch 1 of seg
                 JRCH    =SWO%SEGRCH_OUT(JSEG)                               ! reach index @ last reach of seg
                 DO ISTR =IRCH, JRCH
                          DFLOW_ET = DFLOW_ET - STRM(13,ISTR) !ET Out
                          DFLOW_ET = DFLOW_ET + STRM(14,ISTR) !P  In
                 END DO
                 ! ... uptree @ iupseg ...
                 NTREE   =SWO%UPTREE_NAT(JSEG)%NTREE
                 DO ITREE=1, NTREE
                   KSEG  =SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
                   IRCH  =SWO%SEGRCH_IN(KSEG)
                   JRCH  =SWO%SEGRCH_OUT(KSEG)
                   DO ISTR=IRCH, JRCH
                           DFLOW_ET = DFLOW_ET - STRM(13,ISTR) !ET Out
                           DFLOW_ET = DFLOW_ET + STRM(14,ISTR) !P  In
                   END DO
                 END DO
                 SWO%DIVSEG(IDVS)%UP_DFLOW_ET = DFLOW_ET * SWO%DELT
                 !
                 ! sum specified inflows to upstream river network ...
                 DFLOW_IN = DZ
                 ! ... divseg ...
                 IF (IDIVAR(1,ISEG) == Z .AND. SWO%SEGDATA(ISEG)%RRFLAG.NE.ONE ) DFLOW_IN = DFLOW_IN + SEG(2,ISEG)
                 ! ... divseg's iupseg ...
                 IF (IDIVAR(1,JSEG) == Z .AND. SWO%SEGDATA(JSEG)%RRFLAG.NE.ONE ) DFLOW_IN = DFLOW_IN + SEG(2,JSEG) ! if NOT a diversion segment ... i.e., if IUPSEG=0  add FLOW value to inflows (if not diversion segment, FLOW is prescribed trib inflow)
                 ! ... uptree @ iupseg ...
                 NTREE   =SWO%UPTREE_NAT(JSEG)%NTREE
                 DO ITREE=1, NTREE
                   KSEG  =SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
                   IF ( IDIVAR(1,KSEG) == Z .AND. SWO%SEGDATA(KSEG)%RRFLAG.NE.ONE )  DFLOW_IN = DFLOW_IN + SEG(2,KSEG)
                 END DO
                 SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DFLOW_IN * SWO%DELT
                 !
                 ! sum routed inflows to upstream river network ...
                 DFLOW_RT = DZ
                 ! ... divseg ...
                 NLIST   =SWO%UPSEG(ISEG)%NLIST
                 DO ILIST=1, NLIST
                   KSEG  =SWO%UPSEG(ISEG)%SEGLIST(ILIST)
                   JRCH  =SWO%SEGRCH_OUT(KSEG)
                   IF ( SWO%SEGDATA(KSEG)%OUTSEG.EQ.ISEG  .AND.  &             ! routed inflow to divseg (ISEG) ...
                        SWO%SEGINFO(KSEG)%SegID.NE.JSEG ) THEN                 ! EXCLUDING inflow from divseg's iupseg
                     DFLOW_RT = DFLOW_RT + STRM(9,JRCH)
                   END IF
                 END DO
                 ! ... divseg's iupseg ...
                 NLIST   =SWO%UPSEG(JSEG)%NLIST
                 DO ILIST=1, NLIST
                   KSEG  =SWO%UPSEG(JSEG)%SEGLIST(ILIST)
                   JRCH  =SWO%SEGRCH_OUT(KSEG)
                   IF ( SWO%SEGDATA(KSEG)%OUTSEG.EQ.JSEG .AND. (SWO%SEGINFO(KSEG)%SegType.NE.ONE)) DFLOW_RT = DFLOW_RT + STRM(9,JRCH)  ! routed inflow to divseg's iupseg (JSEG) ... EXCLUDING inflow from upstream river segments!
                 END DO
                 ! ... uptree @ iupseg ...
                 NTREE   =SWO%UPTREE_NAT(JSEG)%NTREE
                 DO ITREE=1, NTREE
                   KSEG  = SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
                   NLIST = SWO%UPSEG(KSEG)%NLIST
                   DO ILIST=1, NLIST
                     LSEG  = SWO%UPSEG(KSEG)%SEGLIST(ILIST)
                     JRCH  = SWO%SEGRCH_OUT(LSEG)
                     IF ( SWO%SEGDATA(LSEG)%OUTSEG.EQ.KSEG .AND. SWO%SEGINFO(LSEG)%SegType.NE.ONE ) DFLOW_RT = DFLOW_RT + STRM(9,JRCH) ! routed inflow to segment in uptree @ iupseg (JSEG) ... EXCLUDING inflow from upstream river segments!
                   END DO
                 END DO
                 SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DFLOW_RT * SWO%DELT
                 !
                 ! Compute release required to meet demand at IDVS
                 ! accounting for upstream diversions, inflows, and seepage
                 ! (sum of diversion orders @ IDVS and upstream ...
                 !  minus seepage gains      (positive DSEEP=gain, negative DSEEP=loss)
                 !  minus inflows above IDVS (positive DFLOW=inflow, negative DFLOW=loss))
                 DTMP  = (DDIVO - DSEEP - DFLOW_IN - DFLOW_RT - DFLOW_RN - DFLOW_ET)*SWO%DELT
                 !
                 ! Reset release demand to highest value to meet all diversion orders
                 IF(PROJ_RELEASE_DMD < DTMP .AND. DDIVO > NEARZERO_5) PROJ_RELEASE_DMD = DTMP  !ONLY INCLUDE DEMAND FOR DIVERSION REQUESTS
                 !
               END DO ! IDVS = ONE,SWO%DIVCOUNT

             END IF ! (PROJ_DIV_ORDER.LE.SWO%MIN_PROJ_ORDER(I)) THEN
             !
             !IF(KITER > 4 .AND. SWO%RELEASE_RELAX < UNO) THEN
             !                    RELEASE_DMD = RELAXER(RELEASE_DMD, SWO%RESDAT(IPROJ)%RELEASE_DMD_PREV, SWO%RELEASE_RELAX)
             !END IF
             !
             IF(PROJ_RELEASE_DMD < NEARZERO_5) PROJ_RELEASE_DMD = DZ
             !
       END ASSOCIATE
    END DO
    !
    !
    ! (10) Compute actual release ...
    !     ** Actual release is minimum of max release and demand-driven release...
    !        adjusted for flood release if needed
    !     ** First compute demand-driven release (DivReq/DivRatio...)
    !     ** Then compute storage at end of period at demand-driven release
    !     ** Then check for flood release
    !
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    DO P=ONE, NPROJ
       !                                                                   RESBAL(IRES)%RELEASE_PROJ = Res IRES Final Release Ammount for Project
       ASSOCIATE(PROJ_DIV_ORDER   => SWO%PROJ  (P)%DIVORDER,         &
                 RESBAL           => SWO%RESDAT(P)%RESBAL,           &
                 PROJ_RELEASE_MAX => SWO%RESDAT(P)%PROJ_RELEASE_MAX, & ! MAX POTENTIAL RELEASE FOR PROEJECT
                 PROJ_RELEASE_DMD => SWO%RESDAT(P)%PROJ_RELEASE_DMD, & ! DEMANDED PROJECT RELEASE TO MEET DIVERSION ORDERS --INCLUDES GAINS/LOSSES
                 PROJ_RELEASE_FIN => SWO%RESDAT(P)%PROJ_RELEASE_FIN, & ! FINAL ADDJUSTED RELEASE VALUE BASED ON MAX RELEASE LIMITS
                 PROJ_RELEASE_ADD => SWO%RESDAT(P)%PROJ_RELEASE_ADD  & ! S-Langauge specified additional releases
                )
             ! (1) If RELEASE_EST < 150 cfs, no release                              !COULD ADD PRIORITY IN DIVERSIONS
             !     Ensures minimum release discharge of 150 cfs (9112.5 AF/period)
             !     NOTE: Allow diversion of return flows ...
             !     TODO: Revise diversions so that MX diversion takes priority over US
             !
             !IF (PROJ_RELEASE_MAX < PROJ_RELEASE_DMD) THEN
             !    RELEASE_EST = PROJ_RELEASE_MAX
             !ELSE
             !    RELEASE_EST = PROJ_RELEASE_DMD
             !END IF
             !
             RATIO = UNO
             DFLOW = PROJ_RELEASE_ADD  !HOLD BACKUP OF ADDITIONAL PROJECT RELEASE
             !
             IF(PROJ_RELEASE_ADD > DZ) THEN                                 !Check if PROJ_RELEASE_ADD needs to be corrected
                IF (PROJ_RELEASE_MAX < PROJ_RELEASE_DMD + PROJ_RELEASE_ADD) THEN
                  !
                  PROJ_RELEASE_ADD = PROJ_RELEASE_MAX - PROJ_RELEASE_DMD
                  !
                  IF(PROJ_RELEASE_ADD < NEARZERO_5) PROJ_RELEASE_ADD = DZ
                END IF
             END IF
             IF(PROJ_RELEASE_ADD < NEARZERO_5) THEN
               DO CONCURRENT (J = ONE:SWO%NRES_BAL(P))
                   RESBAL(J)%RELEASE_PROJ_ADD_INI = DZ
                   RESBAL(J)%RELEASE_PROJ_ADD     = DZ
               END DO
             END IF
             !
             !----------------------------------------------------------------------------------------------------------------
             !
             IF(PROJ_RELEASE_MAX < NEARZERO_5 .AND. PROJ_RELEASE_DMD >= NEARZERO_5) THEN  !NO WATER AVAILIBLE FOR PROJECT RELEASES TO MEET DEMANDED RELEASE, SO CLOSE OFF DIVERSIONS
                 !
                 PROJ_RELEASE_FIN = DZ
                 !
                 DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT, P==SWO%DIVSEG(IDVS)%PROJID);  SWO%DIVSEG(IDVS)%DIVORDER = DZ
                 END DO !(IDVS)
                 !
                 DO CONCURRENT (ISPT = 1:SWO%SPTCOUNT, P==SWO%SPTSEG(ISPT)%ProjID)
                   SWO%SPTSEG(ISPT)%DIVORDER = DZ
                   DO CONCURRENT (J = 1:SWO%SPTSEG(ISPT)%NBRANCH);  SWO%SPTSEG(ISPT)%BrcDIVORDER(J) = DZ
                   END DO
                 END DO
                 !
                 DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));
                     RESBAL(J)%RELEASE_PROJ = DZ
                     RESBAL(J)%RELEASE_PROJ_ADD_INI = DZ
                     RESBAL(J)%RELEASE_PROJ_ADD     = DZ
                 END DO
               !
             !----------------------------------------------------------------------------------------------------------------
             !
             ELSE IF (PROJ_RELEASE_DMD < NEARZERO_5 .AND. PROJ_RELEASE_ADD < NEARZERO_5) THEN  ! NO DEMANDED RELEASES ==> RELEASE DEMANDED IS NEAR ZERO, SO NATURAL FLOWS MEET ALL DOWNSTREAM DEMANDS.
                 !
                 PROJ_RELEASE_FIN = DZ
                 !
                 DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));
                     RESBAL(J)%RELEASE_PROJ = DZ
                     RESBAL(J)%RELEASE_PROJ_ADD_INI = DZ
                     RESBAL(J)%RELEASE_PROJ_ADD     = DZ
                 END DO
               !
             !----------------------------------------------------------------------------------------------------------------
             !
             ! (2) If RELEASE_MAX is less than RELEASE_DMD ...
             !     Reduce US diversion orders proportionately with respect to release shortage:
             !     DivOrder[new] = DivOrder[orig] * (RELEASE_MAX/RELEASE_DMD)
             !     NOTE: Reduction applies equally (by proportion) to all US diversions/splits
             !
             ELSE IF (PROJ_RELEASE_MAX < PROJ_RELEASE_DMD) THEN  ! RG -- LATER CHECK THAT  No reduction to MX diversion order!
               ! no water or no demand --> no release / no diversion
               ! NOTE -- Mexico diversion order not reduced -- only US orders.
               !         This is done simply by skipping the last divseg in loop.
               !         Not a general fix -- only works for RGP!
               ! water available, but less than demand
               ! NOTE -- Mexico diversion order not reduced -- only US orders.
               !         This is done simply by skipping the last divseg in loop.
               !         Not a general fix -- only works for RGP!
               !
               !PROJ_RELEASE_ADD = DZ --Already set to zero
               PROJ_RELEASE_FIN = PROJ_RELEASE_MAX
               !
               DO CONCURRENT (J = ONE:SWO%NRES_BAL(P))
                   IF(RESBAL(J)%INUSE) THEN
                       RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ_POT  !MAXED OUT THE RELEASES
                   ELSE
                       RESBAL(J)%RELEASE_PROJ = DZ
                   END IF
               END DO
               !
               RATIO = PROJ_RELEASE_MAX / PROJ_RELEASE_DMD
               DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT, P==SWO%DIVSEG(IDVS)%ProjID); SWO%DIVSEG(IDVS)%DIVORDER = SWO%DIVSEG(IDVS)%DIVORDER * RATIO
               END DO !(IDVS)
               !
               DO ISPT=1, SWO%SPTCOUNT
               IF(P==SWO%SPTSEG(ISPT)%ProjID) THEN
                     SWO%SPTSEG(ISPT)%DIVORDER = SWO%SPTSEG(ISPT)%DIVORDER * RATIO
                     DO CONCURRENT (J = 1:SWO%SPTSEG(ISPT)%NBRANCH)
                       SWO%SPTSEG(ISPT)%BrcDIVORDER(J) = SWO%SPTSEG(ISPT)%BrcDIVORDER(J) * RATIO
                     END DO
               END IF
               END DO
               !
               DO CONCURRENT (J = ONE:SWO%NRES_BAL(P));
                   RESBAL(J)%RELEASE_PROJ = DZ
                   RESBAL(J)%RELEASE_PROJ_ADD_INI = DZ
                   RESBAL(J)%RELEASE_PROJ_ADD     = DZ
               END DO
               !
             !----------------------------------------------------------------------------------------------------------------
             !
             ELSE
                 PROJ_RELEASE_FIN = PROJ_RELEASE_DMD  !CAN FULLY MEET DEMAND, SPREAD APPROPIATELY ACROSS RESERVOIRS
                 !
                 TOT_RELEASE = PROJ_RELEASE_DMD + PROJ_RELEASE_ADD
                 DTMP = DZ
                 I = Z
                 DO J=ONE, SWO%NRES_BAL(P)
                 IF(RESBAL(J)%INUSE) THEN
                           !
                           RESBAL(J)%RELEASE_PROJ = TOT_RELEASE * RESBAL(J)%REL_DMD_FRAC  !MAXED OUT THE RELEASES
                           !
                           IF( RESBAL(J)%RELEASE_PROJ > RESBAL(J)%RELEASE_PROJ_POT) THEN
                               DTMP = DTMP + RESBAL(J)%RELEASE_PROJ - RESBAL(J)%RELEASE_PROJ_POT
                               RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ_POT
                           ELSE
                               I = I + ONE
                           END IF
                 END IF
                 END DO
                 !
                 DO WHILE (DTMP > NEARZERO_5 .AND. I > Z)
                     IF(I==ONE) THEN
                                   DO J = ONE, SWO%NRES_BAL(P)
                                   IF(RESBAL(J)%RELEASE_PROJ .NE. RESBAL(J)%RELEASE_PROJ_POT .AND. RESBAL(J)%INUSE) THEN
                                       !
                                       RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ + DTMP
                                       !
                                       IF( RESBAL(J)%RELEASE_PROJ > RESBAL(J)%RELEASE_PROJ_POT) THEN
                                           DTMP = RESBAL(J)%RELEASE_PROJ - RESBAL(J)%RELEASE_PROJ_POT
                                           RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ_POT
                                       END IF
                                   END IF
                                   END DO
                                   I = Z    !EXIT LOOP
                     ELSE
                         DTMP3 = DZ  !PRORATE EXCESS BY OTHER RESERVOIRS MAX RELEASE
                         DO J = ONE, SWO%NRES_BAL(P)
                         IF(RESBAL(J)%RELEASE_PROJ .NE. RESBAL(J)%RELEASE_PROJ_POT .AND. RESBAL(J)%INUSE) DTMP3 = DTMP3 + RESBAL(J)%REL_DMD_FRAC
                         END DO
                         !
                         IF( DTMP3 > DZ) THEN
                             DTMP1 = DTMP
                             DTMP  = DZ
                             I = Z
                             DO J = ONE, SWO%NRES_BAL(P)
                             IF(RESBAL(J)%RELEASE_PROJ .NE. RESBAL(J)%RELEASE_PROJ_POT .AND. RESBAL(J)%INUSE) THEN
                                 !
                                 DTMP2 = RESBAL(J)%REL_DMD_FRAC/DTMP3  !RATIO OF REL_DMD_FRAC/ SUM OF NOT MAXED RES RELEASE FRACs
                                 !
                                 RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ + (DTMP1*DTMP2)  ! DTMP1 = RELEASE SHORTFALL, DTMP2 = FRACTION OF RELEASE FOR RES
                                 !
                                 IF( RESBAL(J)%RELEASE_PROJ > RESBAL(J)%RELEASE_PROJ_POT) THEN
                                     DTMP = DTMP + RESBAL(J)%RELEASE_PROJ - RESBAL(J)%RELEASE_PROJ_POT
                                     RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ_POT
                                 ELSE
                                     I = I + ONE
                                 END IF
                             END IF
                             END DO
                         ELSE
                             I = Z  !EXIT LOOP --MAXED OUT RELEASES
                         END IF
                     END IF
                 END DO
                 !
                 ! BACK CALCUALTE RELASE FROM PROJ.ADD
                 !
                 IF(PROJ_RELEASE_ADD > DZ .AND. TOT_RELEASE > DZ) THEN
                     TOT_RELEASE = UNO/TOT_RELEASE
                     !
                     DO J = ONE, SWO%NRES_BAL(P)
                         IF(RESBAL(J)%INUSE) THEN
                             !
                             DTMP = RESBAL(J)%RELEASE_PROJ * TOT_RELEASE
                             !
                             RESBAL(J)%RELEASE_PROJ_ADD_INI = DFLOW * DTMP              !Original added request
                             RESBAL(J)%RELEASE_PROJ_ADD     = PROJ_RELEASE_ADD * DTMP   !Final added request from project to specific reservoir
                         ELSE
                             RESBAL(J)%RELEASE_PROJ_ADD_INI = DZ
                             RESBAL(J)%RELEASE_PROJ_ADD     = DZ
                         END IF
                     END DO
                 ELSEIF(DFLOW > NEARZERO_5 .AND. TOT_RELEASE > DZ) THEN
                     TOT_RELEASE = UNO/TOT_RELEASE
                     !
                     DO J = ONE, SWO%NRES_BAL(P)
                         IF(RESBAL(J)%INUSE) THEN
                             !
                             RESBAL(J)%RELEASE_PROJ_ADD_INI = DFLOW * RESBAL(J)%RELEASE_PROJ * TOT_RELEASE        !Original added request
                             RESBAL(J)%RELEASE_PROJ_ADD     = DZ
                         ELSE
                             RESBAL(J)%RELEASE_PROJ_ADD_INI = DZ
                             RESBAL(J)%RELEASE_PROJ_ADD     = DZ
                         END IF
                     END DO
                 END IF
             END IF !(ad hoc corrections -- part 1)
             !
             ! STORE FINAL RELEASES THAT DO NOT TAKE INTO ACCOUNT STORAGE LEVEL
             DO CONCURRENT (J = 1:SWO%NRES_BAL(P))
                 IF(RESBAL(J)%INUSE) THEN
                    RESBAL(J)%RELEASE_PMAX = RESBAL(J)%RELEASE_PROJ
                 ELSE
                     RESBAL(J)%RELEASE_PMAX = DZ
                 END IF
             END DO
             !
          END ASSOCIATE
    END DO
    !
    !
    !*************************************************************************************************
    !  REQUESTED RELEASES CALCULATED - FIRST GUESS AT ACTUAL RESERVOIR RELEASE AND FINAL SURFACE AREA
    !*************************************************************************************************
    !
    RTOL = 0.0001D0  ! Relative Tolerance ==> .01% change....
    !ATOL = 0.01D0    ! Absolute Tolerance
    !
    IF(ALLOCATED(SWO%RES_CONV_ERROR)) THEN
        IF(SWO%RES_CONV_ERROR.NE.NL) THEN
           DEALLOCATE(SWO%RES_CONV_ERROR)
             ALLOCATE(SWO%RES_CONV_ERROR, SOURCE=NL)  !ERROR if SWO%RES_CONV_ERROR.NE.NL
        END IF
    ELSE
             ALLOCATE(SWO%RES_CONV_ERROR, SOURCE=NL)  !ERROR if SWO%RES_CONV_ERROR.NE.NL
    END IF
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    !
    ! Determine if release is greater or less than minimum release.
    !
    DO P = ONE, NPROJ
    DO J = ONE, SWO%NRES_BAL(P)
        !
        IF(SWO%RESDAT(P)%RESBAL(J)%INUSE) THEN
            !
            DTMP = SWO%RESDAT(P)%RESBAL(J)%RELEASE_PROJ + SWO%RESDAT(P)%RESBAL(J)%RELEASE_ADDF + SWO%RESDAT(P)%RESBAL(J)%RELEASE_REQF + SWO%RESDAT(P)%RESBAL(J)%RELEASE_SPEC
            !
            IF(SWO%RESDAT(P)%RESBAL(J)%RELEASE_MIN_INPUT > DTMP) THEN
                !
                SWO%RESDAT(P)%RESBAL(J)%RELEASE_MIN = SWO%RESDAT(P)%RESBAL(J)%RELEASE_MIN_INPUT - DTMP
            ELSE
                SWO%RESDAT(P)%RESBAL(J)%RELEASE_MIN = DZ
            END IF
        END IF
    END DO
    END DO
    !
    STORAGE_END_PREV = DZ   ! Will be overwritten with previous values -- No need to reset for each res
    TOT_RELEASE_PREV = DZ   !   need to initialize to prevent floating point errors
    SPILL_WAY_PREV   = DZ   !
    !
    K = Z
    DO P = ONE, NPROJ
    DO J = ONE, SWO%NRES_BAL(P)
       !
       K = K + ONE
       !
       SWO%RESDAT(P)%RESBAL(J)%RELEASE_FLOD = DZ  ! Initialize to zero
       SWO%RESDAT(P)%RESBAL(J)%SPILL_WAY    = DZ  !  ^= ditto
       SWO%RESDAT(P)%RESBAL(J)%OVER_TOP     = DZ  !  ^= ditto
       !
       RES_RELEASES = [SWO%RESDAT(P)%RESBAL(J)%RELEASE_MIN,     &  !1 LOWEST PRIORITY
                       SWO%RESDAT(P)%RESBAL(J)%RELEASE_FLOD,    &  !2
                       SWO%RESDAT(P)%RESBAL(J)%RELEASE_PROJ,    &  !3
                       SWO%RESDAT(P)%RESBAL(J)%RELEASE_ADDF,    &  !4
                       SWO%RESDAT(P)%RESBAL(J)%RELEASE_REQF,    &  !5
                       SWO%RESDAT(P)%RESBAL(J)%RELEASE_SPEC     &  !6 HIGHEST PRIORITY
                      ]
       ASSOCIATE(RESBAL       => SWO%RESDAT(P)%RESBAL(J),                  &
                 RES_INUSE    => SWO%RESDAT(P)%RESBAL(J)%INUSE,            &
                 POOLFLG      => SWO%RESDAT(P)%RESBAL(J)%RESBAL_POOLFLG,   &   ! =1 for split res, 0 or balance res
                 !
                 CAPACITY     => SWO%RESDAT(P)%RESBAL(J)%STORAGE_CAPACITY, &   ! Max Res Capacity before Spill Flow
                 STORAGE_SPL  => SWO%RESDAT(P)%RESBAL(J)%STORAGE_SPILL,    &   ! WHEN FLOOD RELEASES ARE MADE = CAPACITY OR USER SPECIFIED VALUE
                 STORAGE_FLD  => SWO%RESDAT(P)%RESBAL(J)%STORAGE_MAX,      &   ! WHEN FLOOD RELEASES ARE MADE = CAPACITY OR USER SPECIFIED VALUE
                 STORAGE      => SWO%RESDAT(P)%RESBAL(J)%STORAGE_PREV,     &   ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
                 STORAGE_MIN  => SWO%RESDAT(P)%RESBAL(J)%STORAGE_MIN,      &   ! Min allowed storage (either STORAGE_SPEC_MIN or greater)
                 STORAGE_END  => SWO%RESDAT(P)%RESBAL(J)%STORAGE,          &   ! Final Storage of Reservoir
                 !
                 SPILLWAY_PREF=> SWO%RESDAT(P)%RESBAL(J)%SPILLWAY_PREF,    &
                 !
                 TRANSFER     => SWO%RESDAT(P)%RESBAL(J)%STORAGE_TRANSFER, &
                 TRAN_FRAC    => SWO%RESDAT(P)%RESBAL(J)%STORAGE_TRAN_FRAC,&
                 !
                 AREA_MAX     => SWO%RESDAT(P)%RESBAL(J)%MAX_AREA,         &    ! Largest area of reservoir
                 AREA_DPL     => SWO%RESDAT(P)%RESBAL(J)%AREA_DPL,         &    ! Area at dead pool storage
                 AREA_SOS     => SWO%RESDAT(P)%RESBAL(J)%AREA_PREV,        &    ! Starting Area of reservoir
                 AREA_END     => SWO%RESDAT(P)%RESBAL(J)%AREA,             &    ! Final Area of Reservoir
                 !
                 TSF_INFLOW => SWO%RESDAT(P)%RESBAL(J)%INFLOW,             &    ! INFLOW       specified during current step (updated @ AD routine!)          [L3]
                 SFR_INFLOW => SWO%RESDAT(P)%RESBAL(J)%SFR_INFLOW,         &    ! SFR_INFLOW   that originated from an SFR outflow
                 !
                 PRCP_FRAC    => SWO%RESDAT(P)%RESBAL(J)%PRECIP_AREA_FRAC, &
                 PRCP         => SWO%RESDAT(P)%RESBAL(J)%PRCP,             &    ! PRCP         during current step (updated @ AD routine!)          [L]
                 EVAP         => SWO%RESDAT(P)%RESBAL(J)%EVAP,             &    ! EVAP         during current step (updated @ AD routine!)          [L]
                 PRCP_VOL     => SWO%RESDAT(P)%RESBAL(J)%PRCP_FIN,         &    ! PRCP         during current step (updated @ AD routine!)          [L3]
                 EVAP_VOL     => SWO%RESDAT(P)%RESBAL(J)%EVAP_FIN,         &    ! EVAP         during current step (updated @ AD routine!)          [L3]
                 !
                 RELEASE_LIMIT=> SWO%RESDAT(P)%RESBAL(J)%MAX_RELEASE_VOL , &    !LIMIT TO RESERVOIR RELEASES
                 !
                 RELEASE_TOT      => SWO%RESDAT(P)%RESBAL(J)%RELEASE_TOT,  &
                 RELEASE_ALL      => SWO%RESDAT(P)%RESBAL(J)%RELEASE_ALL,  &
                 !
                 MAX_SPILL        => SWO%RESDAT(P)%RESBAL(J)%MAX_SPILL,    &
                 SPILL_WAY        => SWO%RESDAT(P)%RESBAL(J)%SPILL_WAY,    &
                 OVER_TOP         => SWO%RESDAT(P)%RESBAL(J)%OVER_TOP,     &
                 !
                 RELEASE_MIN      => RES_RELEASES(ONE),                    &
                 RELEASE_FLOD     => RES_RELEASES(TWO),                    &
                 RELEASE_PROJ     => RES_RELEASES(THREE),                  &
                 RELEASE_ADDF     => RES_RELEASES(FOUR),                   &
                 RELEASE_REQF     => RES_RELEASES(FIVE),                   &
                 RELEASE_SPEC     => RES_RELEASES(SIX)                     &
                )
            IF(RES_INUSE) THEN
                 !
                 INFLOW = TSF_INFLOW + SFR_INFLOW !+... any additional inflows
                 !
                 IF(ANY(RES_RELEASES > NEARZERO_3)) THEN
                     AREA_END = AREA_DPL  ! FIRST ASSUME RESERVOIR GETS DRAINED TO DEAD POOL STORAGE FOR EVAP CALCUALTION
                 ELSE
                     AREA_END = AREA_SOS
                 END IF
                 !
                 IF    (TRANSFER > DZ) THEN
                                       STOR_TRAN1 = TRANSFER * TRAN_FRAC    ! AVAILIBLE FOR IMMEDIATE RELEASE, REMAINDER AVAILIBLE AFTER RELEASES -- DEFAULT IS TRAN_FRAC = 0.5 FOR HALF BEING AVAILBLE AND HALF HELD
                                       STOR_TRAN2 = TRANSFER - STOR_TRAN1   ! OTHER HALF ADDED AT END OF RELEASES
                 ELSEIF(TRANSFER < DZ) THEN
                                       STOR_TRAN1 = TRANSFER         !TRANFER REMOVES WATER, ENSURE IT SUPERCEDES RELEASES IN PRIORITY
                                       STOR_TRAN2 = DZ
                 ELSE
                                       STOR_TRAN1 = DZ
                                       STOR_TRAN2 = DZ
                 END IF
                 !
     RES_SOLVER: DO ITER=ONE, THOU  !Always solved at least 3  times
                   !
                   AREA_AVG  = ( AREA_SOS + AREA_END) * HALF
                   !
                   PRCP_AREA = PRCP_FRAC*(AREA_MAX - AREA_AVG) + AREA_AVG
                   !
                   CALL VEC_ADJUST_MAXSUM(RES_RELEASES, RELEASE_LIMIT)  !ENSURE NO STRUCTURAL RELEASE LIMITS
                   !
                   IF(SPILL_WAY > MAX_SPILL)  SPILL_WAY = MAX_SPILL
                   !
                   !CALL RES_RELEASES%NULL()
                   !CALL RES_RELEASES%ADD(RELEASE_FLOD)       !Flood Based Releases        -- Lowest Priority
                   !CALL RES_RELEASES%ADD(RELEASE_PROJ)       !Project Releated Release
                   !CALL RES_RELEASES%ADD(ADD_RELEASE_FIN)   !Additiona Releases
                   !CALL RES_RELEASES%ADD(RELEASE_REQF)       !Natural Flow Releases
                   !CALL RES_RELEASES%ADD(RELEASE_SPEC)      !NonProject/Regular Releases --Highest priority
                   !!
                   !CALL RES_RELEASES%ADJUST_MAXSUM(RELEASE_LIMIT)  !SCALE RELEASES SO THEY DO NOT EXCEDE THE MAX ALLOWED RELEASE
                   !
                   STORAGE_END   = STORAGE            &   !
                                 + STOR_TRAN1         &   ! Water transfered into reservoir
                                 + INFLOW             &   ! inflow ...
                                 + PRCP*PRCP_AREA     &   ! precip ...
                                 - EVAP*AREA_AVG      &   ! evap ...
                                 - SUM(RES_RELEASES)  &   ! Sum of each of the reservoir releases
                                 - SPILL_WAY              ! Spill release
                   !
                   IF( STORAGE_END < DZ ) STORAGE_END = DZ
                   !
                   IF( SPILLWAY_PREF) THEN
                       IF( STORAGE_END > STORAGE_SPL .AND. SPILL_WAY < MAX_SPILL ) THEN
                          SPILL_WAY   = SPILL_WAY   + (STORAGE_END - STORAGE_SPL)
                          STORAGE_END = STORAGE_SPL
                          !
                          IF(SPILL_WAY < NEARZERO_5) SPILL_WAY = DZ
                          !
                       ELSEIF(SPILL_WAY > UNO .AND. STORAGE_END < 0.995D0*STORAGE_SPL) THEN  !SPILL FLOW FROM A PREVIOUS ITERATION IS AN OVER ESTIMATE
                              !
                              DTMP = STORAGE_SPL - STORAGE_END    !NOTE SIGN IS BACKWARD --SHOULD BE NEG
                              !
                              IF(SPILL_WAY - DTMP < DZ) THEN
                                    STORAGE_END = STORAGE_END + SPILL_WAY
                                    SPILL_WAY = DZ
                              ELSE
                                    STORAGE_END = STORAGE_END + DTMP
                                    SPILL_WAY   = SPILL_WAY   - DTMP
                              END IF
                       END IF
                   END IF
                   !
                   IF(STORAGE_END > STORAGE_FLD) THEN
                       RELEASE_FLOD = RELEASE_FLOD + (STORAGE_END - STORAGE_FLD)
                       !
                       CALL VEC_ADJUST_MAXSUM(RES_RELEASES, RELEASE_LIMIT)
                       !
                       STORAGE_END   = STORAGE            &   !
                                     + STOR_TRAN1         &   ! Water transfered into reservoir
                                     + INFLOW             &   ! inflow ...
                                     + PRCP*PRCP_AREA     &   ! precip ...
                                     - EVAP*AREA_SOS      &   ! evap ...
                                     - SUM(RES_RELEASES)  &   ! Sum of each of the reservoir releases
                                     - SPILL_WAY              ! Spill release
                       !
                       IF( STORAGE_END > STORAGE_SPL .AND. SPILL_WAY < MAX_SPILL ) THEN                       !RELEASE_FLOD is maxed out, shift back to spill way
                           SPILL_WAY   = SPILL_WAY   + (STORAGE_END - STORAGE_SPL)
                           STORAGE_END = STORAGE_SPL
                           !
                           IF(SPILL_WAY < NEARZERO_5) SPILL_WAY = DZ
                           !
                       ELSEIF(SPILL_WAY > UNO .AND. STORAGE_END < 0.995D0*STORAGE_SPL) THEN  !SPILL FLOW FROM A PREVIOUS ITERATION IS AN OVER ESTIMATE by 0.5%
                              !
                              DTMP = STORAGE_SPL - STORAGE_END    !NOTE SIGN IS BACKWARD --SHOULD BE NEG
                              !
                              IF(SPILL_WAY - DTMP < DZ) THEN
                                    STORAGE_END = STORAGE_END + SPILL_WAY
                                    SPILL_WAY = DZ
                              ELSE
                                    STORAGE_END = STORAGE_END + DTMP
                                    SPILL_WAY   = SPILL_WAY   - DTMP
                              END IF
                       END IF
                       !
                   ELSEIF (STORAGE_END < STORAGE_MIN) THEN
                       !
                       ! REDUCE RELEASES TO MEET NON-PROJECT STORAGE LEVEL
                       !
                       CALL REDUCE_SUM_BY(RES_RELEASES, STORAGE_MIN - STORAGE_END) !SHIFT STORAGE BACK UP TO STORAGE_MIN
                       !
                       STORAGE_END   = STORAGE            &   !
                                     + STOR_TRAN1          &   ! Water transfered into reservoir
                                     + INFLOW             &   ! inflow ...
                                     + PRCP*PRCP_AREA     &   ! precip ...
                                     - EVAP*AREA_SOS      &   ! evap ...
                                     - SUM(RES_RELEASES)  &   ! Sum of each of the reservoir releases
                                     - SPILL_WAY              ! Spill release
                       !
                       IF( STORAGE_END < DZ ) STORAGE_END = DZ
                       ! TODO --
                       ! Add AREA_AVG to RESBAL type ...
                       ! Add adjustment to AREA_AVG if *evap* is greater than storage deficit
                       ! (i.e., if there isn't enough water in storage to make ANY release ... all goes to evap)
                       ! Be careful, though ... in reality, water would likely be released first so as not to evaporate!!
                   END IF
                   !
                   ! ADJUST SPILL WAY FLOW BASED ON INITIAL STORAGE
                   IF(SPILL_WAY > MAX_SPILL) THEN
                                                 STORAGE_END = STORAGE_END + (SPILL_WAY - MAX_SPILL)
                                                 SPILL_WAY = MAX_SPILL
                   END IF
                   !
                   !ADD OTHER HALF OF TRANSFER IF IT IS AN INFLOW
                   !
                   IF(STOR_TRAN2 > DZ) STORAGE_END = STORAGE_END + STOR_TRAN2  !WILL ONLY BE ZERO OR POSITIVE
                   !
                   IF(STORAGE_END > CAPACITY) THEN
                                              OVER_TOP = CAPACITY - STORAGE_END
                                              STORAGE_END = CAPACITY
                   END IF
                   !
                   ! ... update area@end based on first guess
                   IF (POOLFLG.EQ.1) THEN
                        AREA_END = DZ
                        DO JRES = 1,SWO%NRES_SPT(P)
                          CALL FRAC_POOL2SPLIT(SWO,P,JRES,SWO%FRSTOP,STORAGE_END,DTMP1,SWO%IS_LEAP_END)
                          CALL ACAP_STOR2AREA (SWO,P,JRES,DTMP1,DTMP2)
                          AREA_END = AREA_END + DTMP2
                        END DO
                   ELSE
                        CALL ACAP_STOR2AREA(SWO,P,J,STORAGE_END,AREA_END)
                   END IF
                   !
                   IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
                   !
                   HAS_CONVERGED = ITER > THREE !REQUIRES AT LEAST FOUR INTERATIONS BEFORE ALLOW REST OF THE CONVERGENCE CHECKS
                   !
                   ! STORAGE CHECK
                   IF    (STORAGE_END_PREV > NEARZERO_5 .AND. HAS_CONVERGED) THEN
                         !
                         HAS_CONVERGED = HAS_CONVERGED .AND. ABS( (STORAGE_END-STORAGE_END_PREV) / STORAGE_END_PREV ) < RTOL  ! Percent difference
                         !
                   ELSEIF(STORAGE_END      > NEARZERO_3) THEN;   HAS_CONVERGED = FALSE
                   END IF
                   !
                   ! Ensure that total releases are consistant
                   TOT_RELEASE = SUM(RES_RELEASES)
                   IF    (TOT_RELEASE_PREV > NEARZERO_5 .AND. HAS_CONVERGED) THEN
                         !
                         HAS_CONVERGED = HAS_CONVERGED .AND. ABS( (TOT_RELEASE-TOT_RELEASE_PREV) / TOT_RELEASE_PREV ) < RTOL    ! Percent difference
                         !
                   ELSEIF(TOT_RELEASE > NEARZERO_3) THEN;   HAS_CONVERGED = FALSE
                   END IF
                   !
                   ! If there is Spill Releases, then check it is not changing
                   IF (SPILL_WAY_PREV > NEARZERO_5 .AND. HAS_CONVERGED) THEN
                         !
                         HAS_CONVERGED = HAS_CONVERGED .AND. ABS( (SPILL_WAY - SPILL_WAY_PREV) / SPILL_WAY_PREV ) < RTOL    ! Percent difference
                         !
                   ELSEIF(SPILL_WAY > NEARZERO_3) THEN;   HAS_CONVERGED = FALSE
                   END IF
                   !
                   !------------------------------------------------
                   IF(HAS_CONVERGED) EXIT RES_SOLVER  !|||||||||
                   !------------------------------------------------
                   !
                   STORAGE_END_PREV = STORAGE_END
                   TOT_RELEASE_PREV = TOT_RELEASE
                   SPILL_WAY_PREV   = SPILL_WAY
                   !
                END DO RES_SOLVER
                !
                IF (.NOT. HAS_CONVERGED) SWO%RES_CONV_ERROR = SWO%RES_CONV_ERROR//NUM2STR(P,TEN)//NUM2STR(J,TEN)//NUM2STR(K,TEN)
                !
            ELSE
                PRCP_AREA = DZ
                AREA_AVG  = DZ
                PRCP      = DZ
                EVAP      = DZ
                !
                RELEASE_MIN     = DZ
                RELEASE_PROJ    = DZ
                RELEASE_SPEC    = DZ
                RELEASE_REQF    = DZ
                SPILL_WAY       = DZ
                RELEASE_FLOD    = DZ
                RELEASE_ADDF    = DZ
                OVER_TOP        = DZ
            END IF
            !
            !!!IF(.NOT. SPILLWAY_PREF .AND. SPILL_WAY > DZ) THEN !SHIFT SPILLWAY FLOW TO RELEASE FLOOD IF THERE IS ROOM
            !!!    !
            !!!    DTMP = RELEASE_LIMIT - SUM(RES_RELEASES)  !ADDITIONAL FLOW AVAILBLE FOR RELEASE
            !!!    !
            !!!    IF(DTMP > NEARZERO_5) THEN         !HAS ADDITIONAL FLOW
            !!!          IF( DTMP < SPILL_WAY) THEN
            !!!              RELEASE_FLOD = RELEASE_FLOD + DTMP
            !!!              SPILL_WAY    = SPILL_WAY   - DTMP
            !!!          ELSE
            !!!              RELEASE_FLOD = RELEASE_FLOD + SPILL_WAY
            !!!              SPILL_WAY    = DZ
            !!!          END IF
            !!!    END IF
            !!!END IF
            !
            PRCP_VOL = PRCP*PRCP_AREA
            EVAP_VOL = EVAP*AREA_AVG
            !
            IF(RELEASE_FLOD > NEARZERO_5 .AND. RELEASE_MIN > NEARZERO_5 ) THEN
                                                                          RELEASE_FLOD = RELEASE_FLOD + RELEASE_MIN
                                                                          RELEASE_MIN  = DZ
            END IF
            !
            RESBAL%RELEASE_MIN     = RELEASE_MIN
            RESBAL%RELEASE_FLOD    = RELEASE_FLOD
            RESBAL%RELEASE_PROJ    = RELEASE_PROJ
            RESBAL%RELEASE_ADDF    = RELEASE_ADDF
            RESBAL%RELEASE_REQF    = RELEASE_REQF
            RESBAL%RELEASE_SPEC    = RELEASE_SPEC
            !
            RELEASE_TOT = SUM(RES_RELEASES);    IF(RELEASE_TOT < NEARZERO_5) RELEASE_TOT = DZ
            !
            RELEASE_ALL = RELEASE_TOT + SPILL_WAY + OVER_TOP
            !
            IF(RELEASE_ALL < NEARZERO_5) RELEASE_ALL = DZ
            !
        END ASSOCIATE
    END DO
    END DO
    !
    !IF PROJECT RELEASES WERE REDUCED DUE TO STORAGE LIMITS, CHANGE DIVERSIONS TO ACCOUNT FOR NEW RELEASED AMMOUNT
    !
    DO I=ONE, NPROJ
    IF(SWO%RESDAT(I)%PROJ_RELEASE_FIN  > NEARZERO_5 )  THEN !Diversions were already adjusted if SWO%RESDAT(I)%PROJ_RELEASE_FIN = 0
       !
       ASSOCIATE(RESBAL           => SWO%RESDAT(I)%RESBAL,               &
                 PROJ_RELEASE_MAX => SWO%RESDAT(I)%PROJ_RELEASE_MAX,     &  ! MAX POTENTIAL RELEASE FOR PROEJECT
                 PROJ_RELEASE_FIN => SWO%RESDAT(I)%PROJ_RELEASE_FIN,     &
                 PROJ_RELEASE_ADD => SWO%RESDAT(I)%PROJ_RELEASE_ADD      & ! S-Langauge specified additional releases
                )
             !
             ! Calculate Water Released for Projects
             DTMP = DZ
             DO J = ONE, SWO%NRES_BAL(I)
               IF(RESBAL(J)%INUSE) DTMP = DTMP + RESBAL(J)%RELEASE_PROJ  !Get original sum
             END DO
             !
             ! REDUCE ADDITIONAL RELEASES IF NOT MET
             IF(PROJ_RELEASE_ADD > DZ) THEN
                 !
                 DO J = ONE, SWO%NRES_BAL(I)
                 IF(RESBAL(J)%INUSE .AND. RESBAL(J)%RELEASE_PMAX - RESBAL(J)%RELEASE_PROJ > NEARZERO_5) THEN ! Reduction in Res Release
                         !
                         RESBAL(J)%RELEASE_PROJ_ADD = RESBAL(J)%RELEASE_PROJ_ADD - RESBAL(J)%RELEASE_PMAX + RESBAL(J)%RELEASE_PROJ  !PROJ_ADD = PROJ_ADD - (PMAX-REL_PROJ)==> PMAX is original REL_PROJ before any reductions
                         IF(RESBAL(J)%RELEASE_PROJ_ADD < DZ) RESBAL(J)%RELEASE_PROJ_ADD = DZ
                 END IF
                 END DO
                 !
                 !!!PROJ_RELEASE_ADD = DZ
                 !!!DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), RESBAL(J)%INUSE )  !Corect Project Release only contain project releases
                 !!!        !
                 !!!        RESBAL(J)%RELEASE_PROJ = RESBAL(J)%RELEASE_PROJ - RESBAL(J)%RELEASE_PROJ_ADD
                 !!!        !
                 !!!        PROJ_RELEASE_ADD = PROJ_RELEASE_ADD + RESBAL(J)%RELEASE_PROJ_ADD
                 !!!END DO
                 !
                 IF( DTMP < PROJ_RELEASE_FIN + PROJ_RELEASE_ADD)  PROJ_RELEASE_ADD = PROJ_RELEASE_FIN - PROJ_RELEASE_ADD
                 !
                 IF(PROJ_RELEASE_ADD < NEARZERO_5) PROJ_RELEASE_ADD = DZ
             END IF
             !
             IF(DTMP < NEARZERO_5) THEN
                 RATIO = DZ
             ELSE
                 RATIO = DTMP / PROJ_RELEASE_FIN
             END IF
             !
             !More then 5% difference between PROJ_RELEASE_FIN AND ACTUAL RELEASES, SO ADJUST DIVERSIONS TO MEET THEM.
             !
             IF(RATIO < 0.95D0) THEN
               !
               DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT, IPROJ==SWO%DIVSEG(IDVS)%ProjID); SWO%DIVSEG(IDVS)%DIVORDER = SWO%DIVSEG(IDVS)%DIVORDER * RATIO
               END DO !(IDVS)
               !
               DO ISPT = 1, SWO%SPTCOUNT
               IF(IPROJ==SWO%SPTSEG(ISPT)%ProjID) THEN
                     SWO%SPTSEG(ISPT)%DIVORDER = SWO%SPTSEG(ISPT)%DIVORDER * RATIO
                     DO CONCURRENT (IBRC = 1:SWO%SPTSEG(ISPT)%NBRANCH)
                       SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC) = SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC) * RATIO
                     END DO
               END IF
               END DO
             END IF
       END ASSOCIATE
    END IF
    END DO
    !

!    DO IPROJ = 1,NPROJ !
!             !
!!!!!!      ASSOCIATE(DIVSEG=>SWO%DIVSEG)
!!!!!!! (3) If diversion to Mexico was shorted in last iteration,
!!!!!!!     either release additional water (if water available)
!!!!!!!     or curtail upstream diversions in proportion to their orders
!!!!!!          IF ( SWO%DIVSEG_PREV(SWO%DIVCOUNT)%DIVERSION < SWO%DIVSEG_PREV(SWO%DIVCOUNT)%DIVORDER) THEN !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!!!!!!            !
!!!!!!            !SHORTAGE    = DIVSEG_PREV(SWO%DIVCOUNT)%DIVORDER - DIVSEG_PREV(SWO%DIVCOUNT)%DIVERSION
!!!!!!            !IF(SHORTAGE < DZ) SHORTAGE = DZ
!!!!!!            SHORTAGE = ZERO_OR_GREATER( SWO%DIVSEG_PREV(SWO%DIVCOUNT)%DIVORDER - SWO%DIVSEG_PREV(SWO%DIVCOUNT)%DIVERSION )
!!!!!!            !
!!!!!!            IF ( RELEASE_MAX > RELEASE_EST+SHORTAGE ) THEN
!!!!!!              !RELEASE_EST = RELEASE_EST + SHORTAGE
!!!!!!              !IF( RELEASE_EST < DZ ) RELEASE_EST = DZ
!!!!!!              RELEASE_EST = ZERO_OR_GREATER( RELEASE_EST+SHORTAGE )
!!!!!!            ELSE IF ( RELEASE_MAX > RELEASE_EST ) THEN
!!!!!!              !SHORTAGE    = SHORTAGE-(RELEASE_MAX-RELEASE_EST)
!!!!!!              !IF(SHORTAGE < DZ) SHORTAGE = DZ
!!!!!!              SHORTAGE    = ZERO_OR_GREATER( SHORTAGE - RELEASE_MAX + RELEASE_EST )  !SHORTAGE - (RELEASE_MAX - RELEASE_EST)
!!!!!!              RELEASE_EST = ZERO_OR_GREATER( RELEASE_MAX)
!!!!!!              !
!!!!!!              DTMP        = DZ
!!!!!!              DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT-1); DTMP  = DTMP + DIVSEG(IDVS)%DIVORDER
!!!!!!              END DO !(IDVS)
!!!!!!              DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT-1)
!!!!!!                IF (DTMP.LE.UNO) THEN
!!!!!!                  DIVSEG(IDVS)%DIVORDER = DZ
!!!!!!                ELSE
!!!!!!                  DIVSEG(IDVS)%DIVORDER =  ZERO_OR_GREATER( DIVSEG(IDVS)%DIVORDER - SHORTAGE*(DIVSEG(IDVS)%DIVORDER/DTMP) )
!!!!!!                END IF !(DTMP.EQ.DZ)
!!!!!!              END DO !(IDVS)
!!!!!!            ELSE
!!!!!!              RELEASE_EST = ZERO_OR_GREATER( RELEASE_MAX )
!!!!!!              DTMP        = DZ
!!!!!!              DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT-1);  DTMP  = DTMP + DIVSEG(IDVS)%DIVORDER
!!!!!!              END DO !(IDVS)
!!!!!!              DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT-1)
!!!!!!                IF (DTMP.LE.UNO) THEN
!!!!!!                  DIVSEG(IDVS)%DIVORDER = DZ
!!!!!!                ELSE
!!!!!!                  DIVSEG(IDVS)%DIVORDER = ZERO_OR_GREATER( DIVSEG(IDVS)%DIVORDER - SHORTAGE*(DIVSEG(IDVS)%DIVORDER/DTMP) )
!!!!!!                END IF !(DTMP.EQ.0)
!!!!!!              END DO !(IDVS)
!!!!!!            END IF !(RELEASE_MAX.GT.RELEASE_EST+SHORTAGE / RELEASE_MAX.GT.RELEASE_EST )
!!!!!!          END IF !(ad hoc corrections -- part 2)
!!!!!!          END ASSOCIATE
!          !
!          ! (11) Compute reservoir release/mass balance
!          !
!          DO IRES = 1,SWO%NRES_BAL(IPROJ) !----------------------------------------------------------------------------
!             !
!             !ASSOCIATE(STORAGE      => SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV &   ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
!             !          RELEASE_POT  => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_POT  &
!             !          RELEASE_ADD  => SWO%RESDAT(IPROJ)%RESBAL(IRES)%ADD_RELEASE  &
!             !          RELEASE_REQF  => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF  &
!             !          RELEASE_SPEC => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC &
!             !          RELEASE_PROJ_POT => SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ_POT &  !POTENTIAL RELEASES AVAILBLE FOR PROJECT WATER
!             !         )
!            !
!            IF(.NOT. SWO%RESDAT(IPROJ)%RESBAL(IRES)%INUSE) THEN
!                !SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE         = DZ
!                !SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA            = DZ
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ     = DZ
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC    = DZ
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF     = DZ
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%SPILL_WAY       = DZ
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD     = DZ
!                SWO%RESDAT(IPROJ)%RESBAL(IRES)%ADD_RELEASE_FIN = DZ
!                CYCLE
!            END IF
!            !
!            ! Set values for local variables
!            CAPACITY     = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_CAPACITY
!            STORAGE_FLD  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_MAX         ! WHEN FLOOD RELEASES ARE MADE = CAPACITY OR USER SPECIFIED VALUE
!            STORAGE_SOS  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV        ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
!            INFLOW       = SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW              ! INFLOW       during current step (updated @ AD routine!)          [L3]
!            PRCP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP                ! PRCP         during current step (updated @ AD routine!)          [L]
!            EVAP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP                ! EVAP         during current step (updated @ AD routine!)          [L]
!            RELEASE_SPEC = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC        ! RELEASE_SPEC during current step (updated @ AD routine!)          [L3]
!            RELEASE_REQF  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF         ! Any releases to maintain required flows
!            STORAGE_SPEC_MIN = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_SPEC_MIN        ! STORAGE_SPEC_MIN during current step (updated @ AD routine!)
!            STORAGE_MIN  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_MIN         ! Min allowed storage (either STORAGE_SPEC_MIN or greater)
!            POOLFLG      = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_POOLFLG
!            AREA_SOS     = SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_PREV
!            RELEASE_PROJ  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ
!            AREA_MAX     = SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_AREA
!            RELEASE_ADD  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%ADD_RELEASE
!            PRCP_FRAC    = SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRECIP_AREA_FRAC
!            !
!            ! first guess ... assume area@end = area@start, neglect flood release
!            !
!            RELEASE_MAX = SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_VOL
!            IF(SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_S < D100) THEN
!                !
!                IF(RELEASE_MAX > SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_S) RELEASE_MAX = SWO%RESDAT(IPROJ)%RESBAL(IRES)%MAX_RELEASE_S  !STORED AS A RATE
!            END IF
!            !
!            IF( RELEASE_PROJ + RELEASE_ADD + RELEASE_REQF + RELEASE_SPEC > RELEASE_MAX) THEN
!                RELEASE_PROJ = ZERO_OR_GREATER(RELEASE_MAX - RELEASE_ADD - RELEASE_REQF - RELEASE_SPEC)
!                !
!                IF( RELEASE_ADD + RELEASE_REQF + RELEASE_SPEC > RELEASE_MAX) THEN
!                    RELEASE_ADD = ZERO_OR_GREATER(RELEASE_MAX - RELEASE_REQF - RELEASE_SPEC)
!                    !
!                    IF( RELEASE_REQF + RELEASE_SPEC > RELEASE_MAX) THEN
!                        RELEASE_REQF = ZERO_OR_GREATER(RELEASE_MAX - RELEASE_SPEC)
!                        IF(RELEASE_SPEC > RELEASE_MAX) RELEASE_SPEC = RELEASE_MAX
!                    END IF
!                END IF
!            END IF
!            !
!            !
!            !AREA_END      = AREA_SOS
!            RELEASE_FLOD   = DZ
!            SPILL_WAY     = DZ
!            !AREA_AVG      = AREA_SOS !(AREA_SOS+AREA_END) * HALF
!            PRCP_AREA     = PRCP_FRAC*(AREA_MAX - AREA_SOS) + AREA_SOS
!            !
!            STORAGE_END   = STORAGE_SOS    &
!                          - RELEASE_PROJ    &   ! estimated project release for reservoir ==> RELEASE_EST = MIN( RELEASE_MAX, RELEASE_DMND )
!                          - RELEASE_SPEC   &   ! non-project release
!                          - RELEASE_REQF    &   ! required flow releases
!                          - RELEASE_ADD    &   ! additional requested release
!                          - SPILL_WAY      &   ! Spill release
!                          - RELEASE_FLOD    &   ! flood release
!                          + INFLOW         &   ! inflow ...
!                          + PRCP*PRCP_AREA &   ! precip ...
!                          - EVAP*AREA_SOS      ! evap ...
!          !
!          ! ... update release@fld based on first guess
!          IF (STORAGE_END > CAPACITY) THEN
!              SPILL_WAY   = SPILL_WAY   + (STORAGE_END - CAPACITY)
!              STORAGE_END = CAPACITY
!          END IF
!          !
!          IF(STORAGE_END > STORAGE_FLD) THEN
!              RELEASE_FLOD = RELEASE_FLOD + (STORAGE_END - STORAGE_FLD)
!              STORAGE_END = STORAGE_FLD
!              IF(RELEASE_FLOD + RELEASE_PROJ + RELEASE_ADD + RELEASE_REQF + RELEASE_SPEC > RELEASE_MAX) THEN
!                 RELEASE_FLOD = RELEASE_MAX - RELEASE_PROJ - RELEASE_ADD - RELEASE_REQF - RELEASE_SPEC
!                 IF(RELEASE_FLOD < DZ) THEN
!                     STORAGE_END = STORAGE_END - RELEASE_FLOD  !ADD BACK IN THE RELEASE FLOOD OVER THE RELEASE GATE LIMIT
!                     RELEASE_FLOD = DZ
!                 END IF
!              END IF
!          ELSEIF (STORAGE_END < STORAGE_MIN) THEN
!              !
!              ! REDUCE RELEASES TO MEET NON-PROJECT STORAGE LEVEL
!              RELEASE_FLOD = RELEASE_FLOD  - (STORAGE_MIN - STORAGE_END) !RELEASE_PROJ = RELEASE_PROJ  - (STORAGE_SPEC_MIN - STORAGE_END)
!              !
!              IF (RELEASE_FLOD < DZ) RELEASE_PROJ = RELEASE_PROJ  + RELEASE_FLOD ! NOTE RELEASE_FLOD IS NEGATIVE
!              IF (RELEASE_PROJ < DZ) RELEASE_ADD = RELEASE_ADD  + RELEASE_PROJ
!              IF (RELEASE_ADD < DZ) RELEASE_REQF = RELEASE_REQF  + RELEASE_ADD
!              IF (RELEASE_REQF < DZ) RELEASE_SPEC= RELEASE_SPEC + RELEASE_REQF
!              !
!              ! CORRECT RELEASES THAT ARE NEGATIVE
!              !
!              IF( RELEASE_FLOD < NEARZERO_5 ) RELEASE_PROJ = DZ
!              IF( RELEASE_PROJ < NEARZERO_5 ) RELEASE_PROJ = DZ
!              IF( RELEASE_ADD < NEARZERO_5 ) RELEASE_ADD = DZ
!              IF( RELEASE_REQF < NEARZERO_5 ) RELEASE_REQF = DZ
!              IF( RELEASE_SPEC< NEARZERO_5 ) RELEASE_SPEC= DZ
!              !
!              STORAGE_END   = STORAGE_SOS    &
!                            - RELEASE_PROJ    &   ! estimated project release for reservoir ==> RELEASE_EST = MIN( RELEASE_MAX, RELEASE_DMND )
!                            - RELEASE_SPEC   &   ! non-project release
!                            - RELEASE_REQF    &   ! required flow releases
!                            - RELEASE_ADD    &   ! additional requested release
!                            - SPILL_WAY      &   ! Spill release
!                            - RELEASE_FLOD    &   ! flood release
!                            + INFLOW         &   ! inflow ...
!                            + PRCP*PRCP_AREA &   ! precip ...
!                            - EVAP*AREA_SOS      ! evap ...
!              !
!              IF( STORAGE_END < DZ ) STORAGE_END = DZ
!              ! TODO --
!              ! Add AREA_AVG to RESBAL type ...
!              ! Add adjustment to AREA_AVG if *evap* is greater than storage deficit
!              ! (i.e., if there isn't enough water in storage to make ANY release ... all goes to evap)
!              ! Be careful, though ... in reality, water would likely be released first so as not to evaporate!!
!          END IF !(STORAGE_END.LT.0)
!          !
!          ! ... update area@end based on first guess
!          IF (POOLFLG.EQ.1) THEN
!            AREA_END = DZ
!            DO JRES = 1,SWO%NRES_SPT(IPROJ)
!              CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,STORAGE_END,DTMP1,SWO%IS_LEAP_END)
!              CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)
!              AREA_END = AREA_END + DTMP2
!            END DO !(IRES)
!          ELSE
!            CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE_END,AREA_END, ERROR)
!          END IF !(POOLFLG.EQ.1)
!          !
!          AREA_AVG  = (AREA_SOS+AREA_END) * HALF
!          PRCP_AREA = PRCP_FRAC*(AREA_MAX - AREA_AVG) + AREA_AVG
!          !
!          ! second guess ...
!          ! ... recompute storage
!          STORAGE_END   = STORAGE_SOS    &
!                        - RELEASE_PROJ    &    ! estimated project release
!                        - RELEASE_SPEC   &    ! non-project release
!                        - RELEASE_REQF    &    ! required flow releases
!                        - RELEASE_ADD    &    ! additional requested release
!                        - SPILL_WAY      &    ! Spill release
!                        - RELEASE_FLOD    &    ! flood release
!                        + INFLOW         &    ! inflow ...
!                        + PRCP*PRCP_AREA &    ! precip ...
!                        - EVAP*AREA_AVG       ! evap ...
!
!          ! ... update release@fld based on second guess
!          IF (STORAGE_END > CAPACITY) THEN
!              SPILL_WAY   = SPILL_WAY   + (STORAGE_END - CAPACITY)
!              STORAGE_END = CAPACITY
!          END IF
!          !
!          IF(STORAGE_END > STORAGE_FLD) THEN
!              RELEASE_FLOD = RELEASE_FLOD + (STORAGE_END - STORAGE_FLD)
!              STORAGE_END = STORAGE_FLD
!              IF(RELEASE_FLOD + RELEASE_PROJ + RELEASE_ADD + RELEASE_REQF + RELEASE_SPEC > RELEASE_MAX) THEN
!                 RELEASE_FLOD = RELEASE_MAX - RELEASE_PROJ - RELEASE_ADD - RELEASE_REQF - RELEASE_SPEC
!                 IF(RELEASE_FLOD < DZ) THEN
!                     STORAGE_END = STORAGE_END - RELEASE_FLOD  !ADD BACK IN THE RELEASE FLOOD OVER THE RELEASE GATE LIMIT
!                     RELEASE_FLOD = DZ
!                 END IF
!              END IF
!          ELSEIF (STORAGE_END < STORAGE_MIN) THEN
!              !
!              ! REDUCE RELEASES TO MEET NON-PROJECT STORAGE LEVEL
!              RELEASE_FLOD = RELEASE_FLOD  - (STORAGE_MIN - STORAGE_END) !RELEASE_PROJ = RELEASE_PROJ  - (STORAGE_SPEC_MIN - STORAGE_END)
!              !
!              IF (RELEASE_FLOD < DZ) RELEASE_PROJ = RELEASE_PROJ  + RELEASE_FLOD ! NOTE RELEASE_FLOD IS NEGATIVE
!              IF (RELEASE_PROJ < DZ) RELEASE_ADD = RELEASE_ADD  + RELEASE_PROJ
!              IF (RELEASE_ADD < DZ) RELEASE_REQF = RELEASE_REQF  + RELEASE_ADD
!              IF (RELEASE_REQF < DZ) RELEASE_SPEC= RELEASE_SPEC + RELEASE_REQF
!              !
!              ! CORRECT RELEASES THAT ARE NEGATIVE
!              !
!              IF( RELEASE_FLOD < NEARZERO_5 ) RELEASE_PROJ = DZ
!              IF( RELEASE_PROJ < NEARZERO_5 ) RELEASE_PROJ = DZ
!              IF( RELEASE_ADD < NEARZERO_5 ) RELEASE_ADD = DZ
!              IF( RELEASE_REQF < NEARZERO_5 ) RELEASE_REQF = DZ
!              IF( RELEASE_SPEC< NEARZERO_5 ) RELEASE_SPEC= DZ
!              !
!              STORAGE_END   = STORAGE_SOS    &
!                            - RELEASE_PROJ    &   ! estimated project release for reservoir ==> RELEASE_EST = MIN( RELEASE_MAX, RELEASE_DMND )
!                            - RELEASE_SPEC   &   ! non-project release
!                            - RELEASE_REQF    &   ! required flow releases
!                            - RELEASE_ADD    &   ! additional requested release
!                            - SPILL_WAY      &   ! Spill release
!                            - RELEASE_FLOD    &   ! flood release
!                            + INFLOW         &   ! inflow ...
!                            + PRCP*PRCP_AREA &   ! precip ...
!                            - EVAP*AREA_AVG      ! evap ...
!              !
!              IF( STORAGE_END < DZ ) STORAGE_END = DZ
!              !
!              ! TODO --
!              ! Add AREA_AVG to RESBAL type ...
!              ! Add adjustment to AREA_AVG if *evap* is greater than storage deficit
!              ! (i.e., if there isn't enough water in storage to make ANY release ... all goes to evap)
!              ! Be careful, though ... in reality, water would likely be released first so as not to evaporate!!
!          END IF !(STORAGE_END.LT.0)
!
!          ! ... update area@end based on second guess
!          IF (POOLFLG.EQ.1) THEN
!            AREA_END = DZ
!            DO JRES = 1,SWO%NRES_SPT(IPROJ)
!              CALL FRAC_POOL2SPLIT(SWO, IPROJ,JRES,SWO%FRSTOP,STORAGE_END,DTMP1,SWO%IS_LEAP_END)
!              CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)
!              AREA_END = AREA_END + DTMP2
!            END DO !(IRES)
!          ELSE
!            CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE_END,DTMP2, ERROR)
!            AREA_END = DTMP2
!          END IF !(POOLFLG.EQ.1)
!          !AREA_AVG  = (AREA_SOS+AREA_END) * HALF
!          !PRCP_AREA = PRCP_FRAC*(AREA_MAX - AREA_AVG) + AREA_AVG
!
!          ! iterate to closure ...
!          DSTOR   = 0.0001D0  ! .01% change...
!          DRPRJ   = 0.0001D0
!          DRFLD   = 0.0001D0
!
!          ITER    = 0
!          MAXITER = 1000
!          TEST    = 0
!
!          STOREND_PREV    = STORAGE_END
!          AREAEND_PREV    = AREA_END
!          RELPRJ_PREV     = RELEASE_PROJ
!          RELFLD_PREV     = RELEASE_FLOD
!          !RELSPL_PREV     = SPILL_WAY
!
!          DO ITER=ONE, MAXITER !WHILE ( ITER.LE.MAXITER .AND. TEST.LT.3 )
!
!            ! update iteration counter
!            !ITER          = ITER + 1
!
!            ! recompute storage@end from updated area@end, release@flood
!            AREA_AVG      = (AREA_SOS+AREA_END) * HALF
!            PRCP_AREA     = PRCP_FRAC*(AREA_MAX - AREA_AVG) + AREA_AVG
!            !
!            STORAGE_END   = STORAGE_SOS     &
!                          - RELEASE_PROJ     &    ! estimated project release
!                          - RELEASE_SPEC    &    ! non-project release
!                          - RELEASE_REQF     &    ! required flow releases
!                          - RELEASE_ADD     &    ! additional requested release
!                          - SPILL_WAY       &    ! Spill release
!                          - RELEASE_FLOD     &    ! flood release
!                          + INFLOW          &    ! inflow ...
!                          + PRCP*PRCP_AREA  &    ! precip ...
!                          - EVAP*AREA_AVG        ! evap ...
!            !
!            ! ... update release@fld based on iteration
!            IF (STORAGE_END > CAPACITY) THEN
!                SPILL_WAY   = SPILL_WAY   + (STORAGE_END - CAPACITY)
!                STORAGE_END = CAPACITY
!            END IF
!            !
!            IF(STORAGE_END > STORAGE_FLD) THEN
!              RELEASE_FLOD = RELEASE_FLOD + (STORAGE_END - STORAGE_FLD)
!              STORAGE_END = STORAGE_FLD
!              IF(RELEASE_FLOD + RELEASE_PROJ + RELEASE_ADD + RELEASE_REQF + RELEASE_SPEC > RELEASE_MAX) THEN
!                 RELEASE_FLOD = RELEASE_MAX - RELEASE_PROJ - RELEASE_ADD - RELEASE_REQF - RELEASE_SPEC
!                 IF(RELEASE_FLOD < DZ) THEN
!                     STORAGE_END = STORAGE_END - RELEASE_FLOD  !ADD BACK IN THE RELEASE FLOOD OVER THE RELEASE GATE LIMIT
!                     RELEASE_FLOD = DZ
!                 END IF
!              END IF
!            ELSEIF (STORAGE_END < STORAGE_MIN) THEN
!                RELEASE_PROJ = RELEASE_PROJ  - (STORAGE_MIN - STORAGE_END)
!                IF (RELEASE_PROJ < DZ) THEN                        ! NOTE -- RELEASE_EST can be negative if storage deficit is larger than estimated project release ... if so, curtail non-project release
!                    RELEASE_ADD = RELEASE_ADD + RELEASE_PROJ
!                    IF (RELEASE_ADD < DZ) THEN
!                        RELEASE_REQF = RELEASE_REQF + RELEASE_ADD
!                        IF (RELEASE_ADD < DZ)  RELEASE_SPEC = RELEASE_SPEC + RELEASE_REQF
!                    END IF
!                END IF
!                !
!                IF( RELEASE_PROJ < NEARZERO_5 ) RELEASE_PROJ = DZ
!                IF( RELEASE_ADD < NEARZERO_5 ) RELEASE_ADD = DZ
!                IF( RELEASE_SPEC< NEARZERO_5 ) RELEASE_SPEC= DZ
!                !
!                STORAGE_END   = STORAGE_SOS    &
!                              - RELEASE_PROJ    &   ! estimated project release for reservoir ==> RELEASE_EST = MIN( RELEASE_MAX, RELEASE_DMND )
!                              - RELEASE_SPEC   &   ! non-project release
!                              - RELEASE_REQF    &   ! required flow releases
!                              - SPILL_WAY      &   ! Spill release
!                              - RELEASE_FLOD    &   ! flood release
!                              + INFLOW         &   ! inflow ...
!                              + PRCP*PRCP_AREA &   ! precip ...
!                              - EVAP*AREA_AVG      ! evap ...
!                ! TODO --
!                ! Add AREA_AVG to RESBAL type ...
!                ! Add adjustment to AREA_AVG if *evap* is greater than storage deficit
!                ! (i.e., if there isn't enough water in storage to make ANY release ... all goes to evap)
!                ! Be careful, though ... in reality, water would likely be released first so as not to evaporate!!
!            END IF !(STORAGE_END.LT.0)
!            !
!            IF(STORAGE_END < DZ) STORAGE_END = DZ
!
!            ! compute closure criteria
!            DTMP1 = 99.0D0; DTMP2 = 99.0D0; DTMP3 = 99.0D0
!
!            ! STORAGE
!            IF (STOREND_PREV > NEARZERO_5) THEN
!              DTMP1         = ABS( (STORAGE_END-STOREND_PREV) / STOREND_PREV )                   ! Percent difference
!            ELSE
!              IF (STORAGE_END < NEARZERO_5) THEN
!                  DTMP1 = DZ
!              ELSE
!                  DTMP1 = ABS( STORAGE_END-STOREND_PREV )          ! Absolute difference
!              END IF
!            END IF !(STOREND_PREV.NE.0)
!            !
!            ! RELEASE @ Project
!            IF (RELPRJ_PREV > NEARZERO_5) THEN
!              DTMP2         = ABS( (RELEASE_PROJ-RELPRJ_PREV) / RELPRJ_PREV )                     ! Percent difference
!            ELSE
!              IF (RELEASE_PROJ < NEARZERO_5) THEN
!                  DTMP2 = DZ
!              ELSE
!                  DTMP2 = ABS( RELEASE_PROJ-RELPRJ_PREV )             ! Absolute difference
!              END IF
!            END IF !(STOREND_PREV.NE.0)
!            !
!            ! RELEASE @ Flood
!            IF (RELFLD_PREV > NEARZERO_5) THEN
!              DTMP3         = ABS( (RELEASE_FLOD-RELFLD_PREV) / RELFLD_PREV )                    ! Percent difference
!            ELSE
!              IF (RELEASE_FLOD < NEARZERO_5) THEN
!                  DTMP3 = DZ
!              ELSE
!                  DTMP3 = ABS( RELEASE_FLOD-RELFLD_PREV )            ! Absolute difference
!              END IF
!            END IF !(RELFLD_PREV.NE.0)
!            !
!            ! RELEASE @ SPL
!            !IF (RELSPL_PREV > NEARZERO_5) THEN
!            !  DTMP4         = ABS( (SPILL_WAY  -RELSPL_PREV) / RELSPL_PREV )                    ! Percent difference
!            !ELSE
!            !  IF (SPILL_WAY   < NEARZERO_5) THEN
!            !      DTMP4 = DZ
!            !  ELSE
!            !      DTMP4 = ABS( SPILL_WAY  -RELSPL_PREV )            ! Absolute difference
!            !  END IF
!            !END IF
!            !
!            ! check for closure
!            !!!IF (DTMP1.LT.DSTOR) TEST = TEST + 1
!            !!!IF (DTMP2.LT.DRPRJ) TEST = TEST + 1
!            !!!IF (DTMP3.LT.DRFLD) TEST = TEST + 1
!            !
!            ! TEST
!            ! check for negative values
!            !!!IF ( RELEASE_RES < DZ .OR. RELEASE_FLOD < DZ  .OR. RELEASE_SPEC < DZ ) THEN
!            !!!  WRITE(SWO%IOUT,*) " "
!            !!!  WRITE(SWO%IOUT,*) "ERROR:"
!            !!!  WRITE(SWO%IOUT,*) "Cannot have negative reservoir release ..."
!            !!!  WRITE(SWO%IOUT,*) "RELEASE_PROJ =", RELEASE_RES
!            !!!  WRITE(SWO%IOUT,*) "RELEASE_FLOD =", RELEASE_FLOD
!            !!!  WRITE(SWO%IOUT,*) "RELEASE_SPEC=", RELEASE_SPEC
!            !!!  WRITE(SWO%IOUT,*) "Stopping Model."
!            !!!  CALL USTOP(' ')
!            !!!END IF !(RELEASE.LT.0)
!            !
!            IF (DTMP1.LT.DSTOR .AND. DTMP2.LT.DRPRJ .AND. DTMP3.LT.DRFLD) THEN
!              ! Flip convergence indicator
!              TEST = 99
!              ! Save final values
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_PREV    = AREA_SOS
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PMAX = RELEASE_MAX
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_DMND = RELEASE_DMND
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE      = STORAGE_END
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA         = AREA_END
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ  = RELEASE_PROJ
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC = RELEASE_SPEC    ! NOTE -- value may deviate from input if reservoir runs dry ... shouldn't happen, but possible
!              !
!! IMF Debug ...
!!
!! OPTION 1 -- Full updae of project and flood releases ...
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ  = RELEASE_PROJ
!              !SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD  = RELEASE_FLOD
!!
!! OPTION 2 -- Incremental update of project and flood releases to reduce oscillation ...
!!              IF (RELEASE_DMND.GT.DZ) THEN
!!                RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ
!!     +            = RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ
!!     +              + WTFACTOR(1)
!!     +                * (RELEASE_EST
!!     +                   - RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ )
!!                RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD
!!     +            = RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD
!!     +              + WTFACTOR(1)
!!     +                * (RELEASE_FLOD
!!     +                   - RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD )
!!              ELSE
!!                RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ
!!     +            = RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ
!!     +              + WTFACTOR(2)
!!     +                * (RELEASE_EST
!!     +                   - RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ )
!!                RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD
!!     +            = RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD
!!     +              + WTFACTOR(2)
!!     +                * (RELEASE_FLOD
!!     +                   - RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD )
!!              END IF !(RELEASE_DMND.GT.DZ)
!! END IMF Debug
!
!              EXIT !ITER LOOP
!
!            ELSE                                                        ! **NOT** CONVERGED
!              ! Reset convergence indicator
!              !TEST = 0
!              ! update area@end for iteration
!              IF (POOLFLG.EQ.1) THEN
!                AREA_END = DZ
!                DO JRES = 1,SWO%NRES_SPT(IPROJ)
!                  CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,STORAGE_END,DTMP1,SWO%IS_LEAP_END)
!                  CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)
!                  AREA_END = AREA_END + DTMP2
!                END DO !(IRES)
!              ELSE
!                CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE_END,DTMP2, ERROR)
!                AREA_END = DTMP2
!              END IF !(POOLFLG.EQ.1)
!              ! update saved vars for convergence check
!              STOREND_PREV    = STORAGE_END
!              AREAEND_PREV    = AREA_END
!              RELPRJ_PREV     = RELEASE_EST
!              RELFLD_PREV     = RELEASE_FLOD
!              !RELSPL_PREV     = SPILL_WAY
!            END IF !(convergence...)
!
!          END DO !(WHILE ...)
!
!          ! Stop model if storage does not converge ...
!          IF (TEST.EQ.0) THEN
!            WRITE(*,*)    " "
!            WRITE(*,*)    "============================================"
!            WRITE(*,*)    "WARNING:"
!            WRITE(*,*)    "SWO Storage calculation did not converge!"
!            WRITE(*,*)    "Continuing simulation ... "
!            WRITE(*,*)    "============================================"
!            WRITE(*,*)    " "
!
!            WRITE(SWO%IOUT,*) " "
!            WRITE(SWO%IOUT,*) "============================================"
!            WRITE(SWO%IOUT,*) "WARNING:"
!            WRITE(SWO%IOUT,*) "SWOPS Storage calculation did not converge!"
!            WRITE(SWO%IOUT,*) " "
!            WRITE(SWO%IOUT,*) "Latest Iteration Result:"
!            WRITE(SWO%IOUT,*) "STORAGE_SOS  =", STORAGE_SOS
!            WRITE(SWO%IOUT,*) "INFLOW       =", INFLOW
!            WRITE(SWO%IOUT,*) "PRCP[vol]    =", PRCP*PRCP_AREA
!            WRITE(SWO%IOUT,*) "EVAP[vol]    =", EVAP*AREA_AVG
!            WRITE(SWO%IOUT,*) "RELEASE_EST  =", RELEASE_EST
!            WRITE(SWO%IOUT,*) "RELEASE_SPEC =", RELEASE_SPEC
!            WRITE(SWO%IOUT,*) "RELEASE_REQF  =", RELEASE_REQF
!            WRITE(SWO%IOUT,*) "RELEASE_FLOD  =", RELEASE_FLOD
!            WRITE(SWO%IOUT,*) "STORAGE_EOS  =", STORAGE_END
!            WRITE(SWO%IOUT,*) " "
!            WRITE(SWO%IOUT,*) "Previous vs. Latest"
!            WRITE(SWO%IOUT,'(A,3F20.2)') "STORAGE_EOS:", STOREND_PREV, STORAGE_END, DTMP1
!            WRITE(SWO%IOUT,'(A,3F20.2)') "RELEASE_PROJ:", RELPRJ_PREV,  RELEASE_EST, DTMP2
!            WRITE(SWO%IOUT,'(A,3F20.2)') "RELEASE_FLOD:", RELFLD_PREV,  RELEASE_FLOD, DTMP3
!            WRITE(SWO%IOUT,*) " "
!            WRITE(SWO%IOUT,*) "Continuing simulation .... "
!            WRITE(SWO%IOUT,*) "============================================"
!            WRITE(SWO%IOUT,*) " "
!
!            ! CALL USTOP(' ')
!          END IF !(TEST)
!          !
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE         = STORAGE_END
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA            = AREA_END
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ     = RELEASE_PROJ
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC    = RELEASE_SPEC    ! NOTE -- value may deviate from input if reservoir runs dry ... shouldn't happen, but possible
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF     = RELEASE_REQF
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%SPILL_WAY       = SPILL_WAY
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD     = RELEASE_FLOD
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%ADD_RELEASE_FIN = RELEASE_ADD
!          !
!        END DO ! IRES = 1,SWO%NRES_BAL(IPROJ) !---------------------------------------------------------------------------------
!      END DO ! IPROJ = 1,NPROJ *************************************************************************************************
!    IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
    I     = -1
    J     = -1
    IPROJ = -1
    IRES  = -1
    ISEG  = -1
    IRCH  = -1
    IDVS  = -1
    ISPT  = -1
    IBRC  = -1
    IRES  = -1
    JRES  = -1
    IPROJ = -1
    IDIST = -1
    IUNIT = -1
    IFARM = -1
    IAUX  = -1
    !
    ! ****************************************************************
    ! COPY FLOW VALUES TO SFR ARRAYS ...
    ! (12) Reservoir releases     [L3/T]
    ! (13) Diversions to units    [L3/T]
    ! (14) Diversions to branches [frac]
    !
    ! ****************************************************************
    !
    ! (12) Reservoir releases ...

    DO CONCURRENT   (I = ONE:NPROJ)
      DO CONCURRENT (J = ONE:SWO%NRES_BAL(I), SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG > Z)
          !
          SWO%SEGDATA(SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG)%FLOW = DZ
          !
      END DO
    END DO

    DO I = ONE, NPROJ
    DO J = ONE, SWO%NRES_BAL(I)
    IF(SWO%RESDAT(I)%RESBAL(J)%INUSE .AND. SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG > Z) THEN
      ISEG = SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG
      !
      ! IMF DEBUG --
      ! Non-project release does NOT go to project RELSEG here ...
      ! Make non-project water disappear (per alloc committee) ...
      ! TODO --
      ! Klugey fix only works in cases like Rio Grande where
      ! non-project release does NOT go to river ...
      ! Revise for universal applicability ... e.g., specify non-project relseg...)
      ! --SCOTT ADDED RELEASE_SPEC
      SWO%SEGDATA(ISEG)%FLOW = SWO%SEGDATA(ISEG)%FLOW + SWO%RESDAT(I)%RESBAL(J)%RELEASE_ALL * SWO%DELT_INV !/ DELT
      !
    END IF
    END DO
    END DO
    !
    DO I = ONE, NPROJ
    DO J = ONE, SWO%NRES_BAL(I)
    IF(SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG > Z) THEN
        !
        ISEG = SWO%RESDAT(I)%RESBAL(J)%RESBAL_RELSEG
        !
        IF( SWO%SEGDATA(ISEG)%FLOW < NEARZERO_5) THEN
            SWO%SEGDATA(ISEG)%FLOW = DZ
            SEG(2,ISEG)            = 0.0  !Single Precision Number
        ELSE
            SEG(2,ISEG) = SNGL(SWO%SEGDATA(ISEG)%FLOW)
        END IF
        !
    END IF
    END DO
    END DO
    !
    ! (13) Loop over diversion segments ... copy diversion flow values
    DO IDVS = 1, SWO%DIVCOUNT
      ISEG  = SWO%DIVSEG(IDVS)%DivSeg
      !
      SWO%SEGDATA(ISEG)%FLOW = SWO%DIVSEG(IDVS)%DIVORDER * SWO%DELT_INV   !/DELT
      IF(SWO%SEGDATA(ISEG)%FLOW < NEARZERO_5) SWO%SEGDATA(ISEG)%FLOW = DZ
      !
      SEG(2,ISEG)        = SNGL(SWO%SEGDATA(ISEG)%FLOW)
    END DO
    !
    !
    ! (14) Loop over splits ... copy split values (fractional or gross)
    DO ISPT = 1,SWO%SPTCOUNT
      ISEG    = SWO%SPTSEG(ISPT)%SplitSeg
      NBRANCH = SWO%SPTSEG(ISPT)%NBRANCH
      DO IBRC = 1,NBRANCH
        JSEG  = SWO%SPTSEG(ISPT)%BrcSeg(IBRC)
        ! Error trap
        IF ( SWO%SEGDATA(JSEG)%IUPSEG.NE.Z .AND. SWO%SEGDATA(JSEG)%IUPSEG .NE. ISEG )THEN
          WRITE(SWO%IOUT,*) " "
          WRITE(SWO%IOUT,*) "ERROR:"
          WRITE(SWO%IOUT,*) "Branch segment has IUPSEG.NE.0,"
          WRITE(SWO%IOUT,*) "But IUPSEG does not equal Split Segment"
          WRITE(SWO%IOUT,*) "Check network ... ?"
          WRITE(SWO%IOUT,*) " "
          WRITE(SWO%IOUT,*) "Stopping Program..."
          CALL USTOP( ' ' )
        END IF !(IUPSEG.NE.0 .AND. IUPSEG.NE.ISEG)
        ! Compute diversion ...
        IF ( SWO%SEGDATA(JSEG)%IUPSEG == ISEG ) THEN
          ! if no diversion order, then no diversion
          IF (SWO%SPTSEG(ISPT)%DIVORDER.LE.NEARZERO_5) THEN
            SWO%SEGDATA(JSEG)%FLOW = DZ
          ! otherwise, compute diversion ...
          ELSE
            ! volumetric diversion
            IF (SWO%SEGDATA(JSEG)%IPRIOR.EQ.Z) THEN
                 SWO%SEGDATA(JSEG)%FLOW = SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC) * SWO%DELT_INV  !/ DELT
              IF(SWO%SEGDATA(JSEG)%FLOW < NEARZERO_5) SWO%SEGDATA(JSEG)%FLOW = DZ
            ! fractional diversion
            ELSE IF (SWO%SEGDATA(JSEG)%IPRIOR.EQ.-2) THEN
                 SWO%SEGDATA(JSEG)%FLOW = SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC) / SWO%SPTSEG(ISPT)%DIVORDER
              IF(SWO%SEGDATA(JSEG)%FLOW < NEARZERO_5) SWO%SEGDATA(JSEG)%FLOW = DZ
            ELSE
              WRITE(SWO%IOUT,*) " "
              WRITE(SWO%IOUT,*) "ERROR:"
              WRITE(SWO%IOUT,*) "SWO water distribution only supports diversions with IPRIOR=0 or IPRIOR=-2."
              WRITE(SWO%IOUT,*) "Sorry..."
              WRITE(SWO%IOUT,*) "Blame the developer."
              WRITE(SWO%IOUT,*) "Or perhaps he was just under a ridiculous and unrealistic deadline."
              WRITE(SWO%IOUT,*) " "
              WRITE(SWO%IOUT,*) "Stopping Program..."
              CALL USTOP( ' ' )
            END IF !(IPRIOR.EQ.0 / IPRIOR.EQ.-2)
          END IF !(DIVORDER.EQ.0)
          SEG(2,JSEG)        = SNGL(SWO%SEGDATA(JSEG)%FLOW)
        END IF !(IUPSEG.EQ.ISEG)
      END DO !(IBRC)
    END DO !(ISPT)
    !
    ! Check if S overrides diversions or inflows
    IF(SWO%S_SFR_SPEC_DIV%N > Z) THEN
        !
        DO I=ONE, SWO%S_SFR_SPEC_DIV%N
            !
            IF( SWO%S_SFR_SPEC_DIV%DIV(I) >= DZ ) THEN
                !
                ISEG = SWO%S_SFR_SPEC_DIV%SEG(I)
                !
                SWO%SEGDATA(ISEG)%FLOW = SWO%S_SFR_SPEC_DIV%DIV(I)
                !
                SEG(2,ISEG)            = SWO%S_SFR_SPEC_DIV%DIV(I)
            END IF
        END DO
    END IF
    !
  END SUBROUTINE
  !
  !!!!PURE FUNCTION STORAGE_TO_AREA(SWO,IPROJ,IRES,TIME, STORAGE, ERROR) RESULT(AREA)
  !!!!  CLASS(SWO_DATA),           INTENT(IN):: SWO
  !!!!  INTEGER,                   INTENT(IN):: IPROJ, IRES
  !!!!  DOUBLE PRECISION,          INTENT(IN):: TIME, STORAGE
  !!!!  CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: ERROR
  !!!!  DOUBLE PRECISION:: AREA
  !!!!  INTEGER:: JRES
  !!!!  !
  !!!!  IF     (SWO%IRESFL(IPROJ) > Z) THEN
  !!!!                                 AREA = DZ
  !!!!                                 DO JRES = 1, SWO%NRES_SPT(IPROJ)
  !!!!                                   CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,TIME,STORAGE,DTMP1,SWO%IS_LEAP_END)
  !!!!                                   CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)
  !!!!                                   AREA_END = AREA_END + DTMP2
  !!!!                                 END DO !(IRES)
  !!!!  ELSEIF (SWO%IRESFL(IPROJ) < Z) THEN
  !!!!    CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE, AREA, ERROR)
  !!!!  ELSE
  !!!!      AREA = DZ
  !!!!  END IF
  !!!!  !
  !!!!END FUNCTION
  !
  SUBROUTINE CONVERGENCE_CHECK(SWO,KPER,KSTP,KITER,STRM,SRDLOC,ICNVG)
!-----VERSION X 2014.06.30 SWOPS1FM
!   ******************************************************************
!   OVERVIEW:
!   COMPUTE CONVERGENCE CRITERIA @ TIMESTEP
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    !USE SWOPSMODULE
    !USE SWOPSUTIL
    !USE GWFSFRMODULE, ONLY: STRM
!   ------------------------------------------------------------------
!      ARGUMENTS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),                            INTENT(INOUT):: SWO
    INTEGER,                                    INTENT(INOUT):: ICNVG
    INTEGER,                                    INTENT(IN   ):: KPER,KSTP,KITER
    REAL,             DIMENSION(:,:),CONTIGUOUS,INTENT(IN   ):: STRM
    TYPE(SRD_LOC),    DIMENSION(:),  CONTIGUOUS,INTENT(IN   ):: SRDLOC
    ! SWOPS1OUT/XCHECK
    !CHARACTER(4) :: XOPT
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
!   ------------------------------------------------------------------
    INTEGER:: I,F,IPROJ,IDIST,IUNIT,IAUX,ISEG,JSEG,    &
              KSEG,NTREE,ITREE,IBRC,ISPT,IDVS,NRES,IRES,IRCH,JRCH,ISTR,JSTR,KRCH
    DOUBLE PRECISION:: DTMP, ACHK, RCHK, FACTOR,ORDER, DIVIN,SPTIN,BRCIN,SUMIN,SUMOUT
    CHARACTER(:), ALLOCATABLE:: ERROR
    !
    IF(KPER==ONE) THEN
      !
      IF( SWO%NSFR_OLD_FLO > Z .AND. KSTP==ONE .AND. KITER==ONE) THEN  !THERE ARE VARIABLES THAT SET TO SFR_INPUT_DATA_TYPES IN OR OUT FLOW
          !
          DO I=ONE, SWO%NSFR_OLD_FLO
              !
              IF(SWO%SFR_OLD_FLO(I)%IS_INFLOW) THEN
                  !
                  SWO%SFR_OLD_FLO(I)%FLO  = STRM(10,SWO%SFR_OLD_FLO(I)%ISTRM)
              ELSE
                  SWO%SFR_OLD_FLO(I)%FLO  = STRM( 9,SWO%SFR_OLD_FLO(I)%ISTRM)
              ENDIF
          END DO
      END IF
    END IF
!   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: DCNVG
    !
    !DDELT = DBLE(DELT)
!
!    ! XCHECK
!    CHARACTER(10),DIMENSION(:),ALLOCATABLE :: XCHKCHAR
!    INTEGER,DIMENSION(:),ALLOCATABLE :: IXCHECK
!    DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: DXCHECK
!
    ASSOCIATE(PROJ => SWO%PROJ,   DIST => SWO%DIST,    UNIT =>SWO%UNIT,  FARM => SWO%FARM,   AUXDEM => SWO%AUXDEM,                                            &
             !
             PROJ_PREV => SWO%PROJ_PREV, DIST_PREV => SWO%DIST_PREV, UNIT_PREV => SWO%UNIT_PREV, FARM_PREV => SWO%FARM_PREV,  AUXDEM_PREV => SWO%AUXDEM_PREV, &
             !
             NPROJ => SWO%NPROJ, NDIST=> SWO%NDIST, NUNIT=>SWO%NUNIT, NFARM => SWO%NFARM, NAUXDEM=>SWO%NAUXDEM, NSS => SWO%NSEG, DIVCOUNT => SWO%DIVCOUNT, SPTCOUNT => SWO%SPTCOUNT,   &
             !
             SEGDATA => SWO%SEGDATA, SEGINFO => SWO%SEGINFO, DIVSEG => SWO%DIVSEG, SPTSEG => SWO%SPTSEG, NRES_BAL => SWO%NRES_BAL, RESDAT => SWO%RESDAT,      &
             !
             UPTREE_CON2 => SWO%UPTREE_CON2, DNTREE_CON2 => SWO%DNTREE_CON2, DNTREE_NAT => SWO%DNTREE_NAT,                                                    &
             !
             SEGDATA_PREV => SWO%SEGDATA_PREV, DIVSEG_PREV => SWO%DIVSEG_PREV,  SPTSEG_PREV => SWO%SPTSEG_PREV,                                               &
             !
             RCHDATA_SAVE => SWO%RCHDATA_SAVE, ABS_CNVG => SWO%ABS_CNVG,  REL_CNVG => SWO%REL_CNVG,  ABS_CHK => SWO%ABS_CNVG_CHK,  REL_CHK => SWO%REL_CNVG_CHK, &
             !
             OUTPUT => SWO%OUTPUT, SEGRCH_IN => SWO%SEGRCH_IN, SEGRCH_OUT => SWO%SEGRCH_OUT )
!
!     ---------------------------------------------------------------------------------------------------------------------------------------------------
!
      ! Get info from other packages
      !
      ! ****************************************************************
      ! WET-WATER ACCOUNTING
      ! (compile wet-water flows
      !  @ FARM, AUX, UNIT, DIST, PROJ, DIVSEG, SPTSEG ...
      !  @ RELEASE, DIVERSION, DELIVERY, BYPASS )
      ! ****************************************************************
      !
      ! (1) Reset current-step and YTD values ...
      ! --- Project
      DO CONCURRENT (I = 1:NPROJ)
        PROJ(I)%RELEASE          = DZ
        PROJ(I)%DIVERSION        = DZ
        !PROJ(I)%OUTFLOW          = DZ
        PROJ(I)%DELIVERY         = DZ
        PROJ(I)%BYPASS           = DZ
        PROJ(I)%DELIVEFF         = UNO
        PROJ(I)%DIVRATIO         = DZ
        PROJ(I)%RELEASE_YTD      = PROJ_PREV(I)%RELEASE_YTD
        PROJ(I)%DIVERSION_YTD    = PROJ_PREV(I)%DIVERSION_YTD
        !PROJ(I)%OUTFLOW_YTD      = PROJ_PREV(I)%OUTFLOW_YTD
        PROJ(I)%DELIVERY_YTD     = DZ                              ! recomputed as sum over farm YTD deliveries
        PROJ(I)%BYPASS_YTD       = DZ                              ! recomputed as sum over unit YTD bypass
        PROJ(I)%DELIVEFF_YTD     = UNO                             ! recomputed from YTD flows
        PROJ(I)%DIVRATIO_YTD     = UNO                             ! recomputed from YTD flows
      END DO !(I)
      ! --- District
      DO CONCURRENT (IDIST = 1:NDIST)
        DIST(IDIST)%DIVERSION        = DZ
        DIST(IDIST)%DELIVERY         = DZ
        DIST(IDIST)%BYPASS           = DZ
        DIST(IDIST)%DELIVEFF         = UNO                             ! recomputed from current flows
        DIST(IDIST)%DIVERSION_YTD    = DZ
        DIST(IDIST)%DELIVERY_YTD     = DZ                              ! recomputed as sum over farm YTD deliveries
        DIST(IDIST)%BYPASS_YTD       = DZ                              ! recomputed as sum over unit YTD bypass
        DIST(IDIST)%DELIVEFF_YTD     = DZ                              ! recomputed from YTD flows
      END DO !(IDIST)
      ! --- Unit
      DO CONCURRENT (IUNIT = 1:NUNIT)
        UNIT(IUNIT)%DIVERSION        = DZ
        UNIT(IUNIT)%DELIVERY         = DZ
        UNIT(IUNIT)%BYPASS           = DZ
        UNIT(IUNIT)%DELIVEFF         = UNO
        UNIT(IUNIT)%DIVERSION_YTD    = UNIT_PREV(IUNIT)%DIVERSION_YTD
        UNIT(IUNIT)%DELIVERY_YTD     = DZ                              ! recomputed as sum over farm YTD deliveries
        UNIT(IUNIT)%BYPASS_YTD       = UNIT_PREV(IUNIT)%BYPASS_YTD
        UNIT(IUNIT)%DIVIN_YTD        = UNIT_PREV(IUNIT)%DIVIN_YTD
        UNIT(IUNIT)%SUMIN_YTD        = UNIT_PREV(IUNIT)%SUMIN_YTD
        UNIT(IUNIT)%SUMOUT_YTD       = UNIT_PREV(IUNIT)%SUMOUT_YTD
        UNIT(IUNIT)%DELIVEFF_YTD     = DZ                              ! recomputed from YTD flows
      END DO !(IUNIT)
      ! --- Farm
      DO CONCURRENT (F = 1:NFARM)
        FARM(F)%DELIVERY         = DZ
        FARM(F)%DELIVERY_YTD     = FARM_PREV(F)%DELIVERY_YTD
      END DO
      ! --- Aux
      DO IAUX = 1,NAUXDEM
        AUXDEM(IAUX)%DELIVERY        = DZ
        AUXDEM(IAUX)%DELIVERY_YTD    = AUXDEM_PREV(IAUX)%DELIVERY_YTD
      END DO !(IAUX)
      ! --- Segment
      DO CONCURRENT (ISEG = 1:NSS)
        SEGDATA(ISEG)%DELIVERY       = DZ
      END DO !(ISEG)
      ! --- DivSeg
      DO CONCURRENT (IDVS = 1:DIVCOUNT)
        DIVSEG(IDVS)%DIVERSION       = DZ
        DIVSEG(IDVS)%DELIVERY        = DZ
        DIVSEG(IDVS)%DELIVEFF        = UNO
      END DO !(IDVS)
      ! --- SptSeg
      DO CONCURRENT (ISPT = 1:SPTCOUNT)
        SPTSEG(ISPT)%DIVERSION       = DZ
        SPTSEG(ISPT)%DELIVERY        = DZ
        SPTSEG(ISPT)%DELIVEFF        = UNO
        !
        DO CONCURRENT (IBRC = 1:SPTSEG(ISPT)%NBRANCH)
          SPTSEG(ISPT)%BrcDIVERSION(IBRC) = DZ
          SPTSEG(ISPT)%BrcDELIVERY(IBRC)  = DZ
          SPTSEG(ISPT)%BrcDELIVEFF(IBRC)  = UNO
        END DO !(IBRC)
      END DO !(ISPT)
      !
      ! (2) Compute releases, diversions, deliveries, bypass ...
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! RELEASES -- FMP does NOT handle releases
      !             SWOPS computes release, passes value to SFR.
      !             Here, pull actual flow into release segment from SFR
      !             (this ensures that closure is based on actual flows
      !              computed in the SFR FM routine)
      !             (release = flow into first reach of release segment)
      ! IMF DEBUG --
      ! Revised to grab PROJECT release, rather than pull total release from SFR array
      !
      DO CONCURRENT (I = 1:NPROJ); PROJ(I)%RELEASE = DZ
      END DO
      !
      DO I    = 1, NPROJ
      DO NRES = 1, NRES_BAL(I)
      IF(RESDAT(I)%RESBAL(NRES)%INUSE)  PROJ(I)%RELEASE = PROJ(I)%RELEASE + RESDAT(I)%RESBAL(NRES)%RELEASE_PROJ
      END DO
      END DO
      ! IMF DEBUG --
      ! Old ... includes flood release ... not wanted for project parameters (divratio)!
!      DO IPROJ = 1,NPROJ
!        ! current step
!        DO IRES = 1,NRES_BAL(IPROJ)
!          ISEG = RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG
!          ISTR = SEGRCH_IN(ISEG)
!          PROJ(IPROJ)%RELEASE
!     +      = PROJ(IPROJ)%RELEASE
!     +        + STRM(10,ISTR)*DDELT
!        END DO !(IRES)
        ! YTD through end of current step
      DO I = 1, NPROJ
                PROJ(I)%RELEASE_YTD = PROJ(I)%RELEASE_YTD + PROJ(I)%RELEASE
      END DO
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! DIVERSIONS -- FMP doesn't deal with diversions ...
      !               SWOPS computes diversion at each point of diversion
      !               as the actual flow into the segment computed by the SFR FM routine

      ! Diversions aggregated to:
      ! - DivSeg   (service area)
      ! - SplitSeg (sub-area)
      ! - Project

      ! DIVERSION @ DIVSEG -- diversion = flow into segment
      DO IDVS = 1, DIVCOUNT
        !ISEG  = DIVSEG(IDVS)%DivSeg
        ISTR  = SEGRCH_IN( DIVSEG(IDVS)%DivSeg )
        DIVSEG(IDVS)%DIVERSION       = STRM(10,ISTR)*SWO%DELT
      END DO !(IDVS)
      !
      ! DIVERSION @ SPLITS/BRANCHES -- diversion = flow into split/branch segment
      DO ISPT = 1, SPTCOUNT
        ISTR  = SEGRCH_IN( SPTSEG(ISPT)%SplitSeg )
        SPTSEG(ISPT)%DIVERSION       = STRM(10,ISTR)*SWO%DELT
        DO IBRC = 1, SPTSEG(ISPT)%NBRANCH
          ISTR  = SEGRCH_IN( SPTSEG(ISPT)%BrcSeg(IBRC) )
          SPTSEG(ISPT)%BrcDIVERSION(IBRC) = STRM(10,ISTR)*SWO%DELT
        END DO !(IBRC)
      END DO !(ISPT)

      ! DIVERSION @ UNIT -- for unit scale, treat diversions identical to charges
      !                     but without appling charge factor...
      !                     i.e., diversion equals to flow into charge reach
      !                     OR flow into diversion reach, depending on situlation
      DO IUNIT = 1,NUNIT
        ! current step
        UNIT(IUNIT)%DIVERSION = DZ
        ISEG   = UNIT(IUNIT)%ChgSeg
        IRCH   = UNIT(IUNIT)%ChgRch
        JSEG   = UNIT(IUNIT)%DivSeg
        JRCH   = UNIT(IUNIT)%DivRch
        IF(ISEG < 1) CYCLE
        IF(JSEG < 1) CYCLE
        IF(IRCH < 1) IRCH = 1
        IF(JRCH < 1) IRCH = 1
        ISTR   = SEGRCH_IN(ISEG) + IRCH - 1
        JSTR   = SEGRCH_IN(JSEG) + JRCH - 1
        ! Charge @ Diversion Point
        IF (ISEG.EQ.JSEG) THEN
          UNIT(IUNIT)%DIVERSION = STRM(10,ISTR)*SWO%DELT
        ! Charge @ Delivery Point (i.e., unit supplied via bypass!)
        ! ... if orders between diversion and charge locations, charge = flow into charge segment
        ! ... if NO orders between diversion and charge locations, charge = flow into DIVERSION segment
        ELSE
          ORDER = DZ
          NTREE = UPTREE_CON2(ISEG)%NTREE
          DO ITREE = 1, NTREE
            KSEG  = UPTREE_CON2(ISEG)%SEGTREE(ITREE)
            IF (SEGINFO(KSEG)%SegType.EQ.3) THEN
              F = SEGINFO(KSEG)%FarmID
              IAUX  = SEGINFO(KSEG)%AuxID
              IF (F.NE.0) ORDER = ORDER + FARM(F)%DELORDER
              IF (IAUX.NE.0)  ORDER = ORDER + AUXDEM(IAUX)%DELORDER
            END IF !(SegType.EQ.3)
          END DO !(KSEG)
          IF (ORDER.GT.DZ) THEN
            UNIT(IUNIT)%DIVERSION = STRM(10,ISTR)*SWO%DELT
          ELSE
            UNIT(IUNIT)%DIVERSION = STRM(10,JSTR)*SWO%DELT
          END IF !(ORDER.GT.0)
        END IF !(ISEG.EQ.JSEG)
        ! YTD through end of current step
        UNIT(IUNIT)%DIVERSION_YTD = UNIT(IUNIT)%DIVERSION_YTD + UNIT(IUNIT)%DIVERSION
      END DO !(IUNIT)
      !
      ! DIVERSION @ DISTRICT -- for district scale, treat diversions identical to charges
      !                         but without appling charge factor...
      !                         district diversion equal to sum of unit diversions over district
      DO IUNIT = 1, NUNIT
        IDIST  = UNIT(IUNIT)%DistID
        ! current step
        DIST(IDIST)%DIVERSION = DIST(IDIST)%DIVERSION + UNIT(IUNIT)%DIVERSION
        ! YTD through end of current step
        DIST(IDIST)%DIVERSION_YTD = DIST(IDIST)%DIVERSION_YTD + UNIT(IUNIT)%DIVERSION_YTD
      END DO !(IUNIT)
      !
      !  DIVERSION @ PROJECT -- for project scale, treat diversions simply as wet-water diversions
      !                         project diversion is sum of diversions over project diversion segments.
      !  OUTFLOW @ PROJECT
      DO CONCURRENT (I = 1:NPROJ)
                             PROJ(I)%DIVERSION = DZ
      END DO
      !DO CONCURRENT (IPROJ = 1:NPROJ)
      !                       PROJ(IPROJ)%OUTFLOW   = DZ
      !END DO
      ! add diversions @ current step
      DO IDVS = 1, DIVCOUNT
         I = DIVSEG(IDVS)%ProjID
         PROJ(I)%DIVERSION  = PROJ(I)%DIVERSION + DIVSEG(IDVS)%DIVERSION
      END DO !(IDVS)
!! TODO --
!! Current code assumes one outflow per project release segment ...
!! Need to update for cases where project has multiple reservoirs releasing to same river, with same outflow segment
!! (otherwise, will double-count outflows...)
!      ! compute outflow from model @ current step ...
!      DO IPROJ = 1,NPROJ
!        DO NRES = 1,NRES_BAL(IPROJ)
!          ISEG  = RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RELSEG
!          NTREE = DNTREE_NAT(ISEG)%NTREE
!          DO ITREE = 1,NTREE
!            JSEG   = DNTREE_NAT(ISEG)%SEGTREE(ITREE)
!            KSEG   = SEGDATA(JSEG)%OUTSEG
!            IF (KSEG.EQ.0) THEN
!              JSTR = SEGRCH_OUT(JSEG)
!              PROJ(IPROJ)%OUTFLOW = PROJ(IPROJ)%OUTFLOW   + STRM(9,JSTR)*DELT
!            END IF !(KSEG.EQ.0)
!          END DO !(ITREE)
!        END DO !(IRES)
!      END DO !(IPROJ)
      ! YTD through end of current step
      !DO CONCURRENT (IPROJ = 1:NPROJ); PROJ(IPROJ)%OUTFLOW_YTD   = PROJ(IPROJ)%OUTFLOW_YTD   + PROJ(IPROJ)%OUTFLOW
      !END DO !(IPROJ)
      !
      DO IPROJ = 1, NPROJ
          PROJ(IPROJ)%DIVERSION_YTD = PROJ(IPROJ)%DIVERSION_YTD + PROJ(IPROJ)%DIVERSION
      END DO !(IPROJ)
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! DELIVERIES -- FMP implements farm deliveries as NEGATIVE RUNOFF ...
      !               SWOPS computes actual farm delivery as difference between
      !               runoff in SFR input (saved to SEGDATA_SAVE%RUNOFF) and
      !               actual runoff in SEG array (SEG(3,X)). Do the reverse here
      !               to back-out actual deliveries to farms.
      !
      ! Deliveries aggregated to:
      ! - Farm
      ! - Auxiliary
      ! - Unit
      ! - District
      ! - Project
      ! - Segment  (individual segments)
      ! - DivSeg   (service area)
      ! - SplitSeg (sub-area)
      !
      ! FARM DELIVERIES @ Farm/Unit/District/Project/Segment
      DO F = 1, NFARM
        ! Farm-Level
        ISEG = FARM(F)%DelSeg
        IRCH = FARM(F)%DelRch
        IUNIT = FARM(F)%UnitID
        IDIST = FARM(F)%DistID
        IPROJ = FARM(F)%ProjID
        !
        IF (ISEG.NE.0) THEN
          !ISTR = SEGRCH_IN(ISEG) + IRCH - 1
          !
          !!IF  (SRDLOC(F)%N == ONE) THEN
          !!    !
          !!    FARM(F)%DELIVERY = SRDLOC(F)%FLOW(ONE)
          !!    !
          !!ELSEIF(SRDLOC(F)%N >  ONE) THEN
          !!    !
          !!    FARM(F)%DELIVERY = SUM(SRDLOC(F)%FLOW)
          !!    !
          !!ENDIF
          !
          FARM(F)%DELIVERY = SRDLOC(F)%TOT_DMD_MET * SWO%DELT
          !
          !FARM(F)%DELIVERY = !( RCHDATA_SAVE(ISTR)%RUNOFF - STRM(12,ISTR) ) * DELT  ! input runoff - current runoff, after FMP applies delivery as negative runoff
          ! YTD through end of current step
          FARM(F)%DELIVERY_YTD   = FARM(F)%DELIVERY_YTD + FARM(F)%DELIVERY
        END IF !(ISEG)
        !
        ! Unit-Level
        IF (IUNIT.NE.0) THEN
          ! current step
          UNIT(IUNIT)%DELIVERY  = UNIT(IUNIT)%DELIVERY + FARM(F)%DELIVERY
          ! YTD through end of current step
          UNIT(IUNIT)%DELIVERY_YTD   = UNIT(IUNIT)%DELIVERY_YTD + FARM(F)%DELIVERY_YTD
        END IF !(IUNIT)
        !
        ! District-Level
        IF (IDIST.NE.0) THEN
          ! current step
          DIST(IDIST)%DELIVERY  = DIST(IDIST)%DELIVERY + FARM(F)%DELIVERY
          ! YTD through end of current step
          DIST(IDIST)%DELIVERY_YTD   = DIST(IDIST)%DELIVERY_YTD + FARM(F)%DELIVERY_YTD
        END IF !(IDIST)
        !
        ! Project-Level
        IF (IPROJ.NE.0) THEN
          ! current step
          PROJ(IPROJ)%DELIVERY = PROJ(IPROJ)%DELIVERY + FARM(F)%DELIVERY
          ! YTD through end of current step
          PROJ(IPROJ)%DELIVERY_YTD   = PROJ(IPROJ)%DELIVERY_YTD + FARM(F)%DELIVERY_YTD
        END IF !(IPROJ)
        !
        ! Segment-Level
        IF (ISEG.NE.0)   SEGDATA(ISEG)%DELIVERY     = SEGDATA(ISEG)%DELIVERY + FARM(F)%DELIVERY
        !
      END DO
      !
      ! AUXILIARY DELIVERIES @ Auxiliary/Unit/District/Project/Segment
      DO IAUX = 1, NAUXDEM
        ! Auxiliary-Level
        ISEG  = AUXDEM(IAUX)%AuxSeg
        IRCH  = AUXDEM(IAUX)%AuxRch
        IUNIT = AUXDEM(IAUX)%UnitID
        IDIST = AUXDEM(IAUX)%DistID
        IPROJ = AUXDEM(IAUX)%ProjID
        !
        IF (ISEG.NE.0) THEN
          ISTR = SEGRCH_IN(ISEG) + IRCH - 1
          ! current step
          AUXDEM(IAUX)%DELIVERY      = STRM(9,ISTR) * SWO%DELT              ! Aux Delivery is outflow from AuxSeg/AuxRch ... Apply factor when computing allotment balance!
          ! YTD through end of current step
          AUXDEM(IAUX)%DELIVERY_YTD  = AUXDEM(IAUX)%DELIVERY_YTD + AUXDEM(IAUX)%DELIVERY
        END IF !(ISEG.NE.0)
        !
        ! Unit-Level
        IF (IUNIT.NE.0) THEN
          ! current step
          UNIT(IUNIT)%DELIVERY       = UNIT(IUNIT)%DELIVERY     + (AUXDEM(IAUX)%DELIVERY * AUXDEM(IAUX)%FACTOR)
          ! YTD through end of current step
          UNIT(IUNIT)%DELIVERY_YTD   = UNIT(IUNIT)%DELIVERY_YTD + (AUXDEM(IAUX)%DELIVERY_YTD * AUXDEM(IAUX)%FACTOR)
        END IF !(IUNIT)
        !
        ! District-Level
        IF (IDIST.NE.0) THEN
          ! current step
          DIST(IDIST)%DELIVERY       = DIST(IDIST)%DELIVERY     + (AUXDEM(IAUX)%DELIVERY * AUXDEM(IAUX)%FACTOR)
          ! YTD through end of current step
          DIST(IDIST)%DELIVERY_YTD   = DIST(IDIST)%DELIVERY_YTD + (AUXDEM(IAUX)%DELIVERY_YTD * AUXDEM(IAUX)%FACTOR)
        END IF !(IDIST)
        !
        ! Project-Level
        IF (IPROJ.NE.0) THEN
          ! current step
          PROJ(IPROJ)%DELIVERY       = PROJ(IPROJ)%DELIVERY + (AUXDEM(IAUX)%DELIVERY * AUXDEM(IAUX)%FACTOR)
          ! YTD through end of current step
          PROJ(IPROJ)%DELIVERY_YTD   = PROJ(IPROJ)%DELIVERY_YTD + (AUXDEM(IAUX)%DELIVERY_YTD * AUXDEM(IAUX)%FACTOR)
        END IF !(IPROJ)
        ! Segment-Level
        IF (ISEG.NE.0) THEN
          SEGDATA(ISEG)%DELIVERY     = SEGDATA(ISEG)%DELIVERY + AUXDEM(IAUX)%DELIVERY
! REMOVE AUX FACTOR
!     +                                 + (AUXDEM(IAUX)%DELIVERY * AUXDEM(IAUX)%FACTOR)
        END IF !(ISEG)
      END DO !(IAUX)
      !
      ! ALL DELIVERY @ DIVERSION SEGMENT (aggregated over tree downstream of DivSeg)
      DO IDVS = 1, DIVCOUNT
        ISEG  = DIVSEG(IDVS)%DivSeg
        NTREE = DNTREE_CON2(ISEG)%NTREE
        DIVSEG(IDVS)%DELIVERY = SEGDATA(ISEG)%DELIVERY
        DO ITREE = 1, NTREE
          JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
          DIVSEG(IDVS)%DELIVERY        = DIVSEG(IDVS)%DELIVERY + SEGDATA(JSEG)%DELIVERY
        END DO !(ITREE)
      END DO !(IDVS)

      ! ALL DELIVERY @ SPLITS/BRANCHES (aggregate over tree downstream of divseg)
      DO ISPT = 1, SPTCOUNT
        ! full tree from top of split segment ...
        ISEG  = SPTSEG(ISPT)%SplitSeg
        SPTSEG(ISPT)%DELIVERY = SEGDATA(ISEG)%DELIVERY
        NTREE = DNTREE_CON2(ISEG)%NTREE
        DO ITREE = 1, NTREE
          JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
          SPTSEG(ISPT)%DELIVERY = SPTSEG(ISPT)%DELIVERY + SEGDATA(JSEG)%DELIVERY
        END DO !(TMP)
        !
        ! branches of split ...
        DO IBRC = 1, SPTSEG(ISPT)%NBRANCH
          ISEG  = SPTSEG(ISPT)%BrcSeg(IBRC)
          NTREE = DNTREE_CON2(ISEG)%NTREE
          SPTSEG(ISPT)%BrcDELIVERY(IBRC) = SEGDATA(ISEG)%DELIVERY
          DO ITREE = 1, NTREE
            JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
            SPTSEG(ISPT)%BrcDELIVERY(IBRC) = SPTSEG(ISPT)%BrcDELIVERY(IBRC) + SEGDATA(JSEG)%DELIVERY
          END DO !(TMP)
        END DO !(IBRC)
      END DO !(ISPT)
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! BYPASS  -- FMP doesn't deal with bypass ...
      !            SWOPS computes computes bypass for each unit/district/project

      ! Bypass aggregated to:
      ! - Unit
      ! - District
      ! - Project
      !
      ! BYPASS @ UNIT -- bypass equal to flow into bypass reach
      !
      DO IUNIT = 1, NUNIT
      DO I     = 1, UNIT(IUNIT)%NBySeg
        ! current step
          ISEG  = UNIT(IUNIT)%BySeg(I)
          IRCH  = UNIT(IUNIT)%ByRch(I)
          FACTOR= UNIT(IUNIT)%ByFactor(I)
          ISTR  = SEGRCH_IN(ISEG) + IRCH - 1
          UNIT(IUNIT)%BYPASS = UNIT(IUNIT)%BYPASS + STRM(10,ISTR)*SWO%DELT*FACTOR
      END DO; END DO !(IUNIT)
      !
      ! YTD through end of current step
      DO IUNIT = 1, NUNIT
          UNIT(IUNIT)%BYPASS_YTD = UNIT(IUNIT)%BYPASS_YTD + UNIT(IUNIT)%BYPASS
      END DO
      !
      ! BYPASS @ DISTRICT -- bypass equal to sum of unit bypass over district
      !
      DO IUNIT = 1, NUNIT
        IDIST  = UNIT(IUNIT)%DistID
        DIST(IDIST)%BYPASS =     DIST(IDIST)%BYPASS     + UNIT(IUNIT)%BYPASS
        DIST(IDIST)%BYPASS_YTD = DIST(IDIST)%BYPASS_YTD + UNIT(IUNIT)%BYPASS_YTD
      END DO !(IUNIT)
      !
      ! BYPASS @ PROJECT -- bypass equal to sum of district bypass over project
      !
      DO IUNIT = 1, NUNIT
        IPROJ  = UNIT(IUNIT)%ProjID
        PROJ(IPROJ)%BYPASS     = PROJ(IPROJ)%BYPASS     + UNIT(IUNIT)%BYPASS
        PROJ(IPROJ)%BYPASS_YTD = PROJ(IPROJ)%BYPASS_YTD + UNIT(IUNIT)%BYPASS_YTD
      END DO !(IUNIT)
      !
      ! (3) Recompute wet-water parameters ... delivery efficiency, etc.
      !
      ! PARAMETERS @ DIVERSION SEGMENTS

! IMF DEBUG -------------------------------------------------------------
! DELIVERY EFFICIENCY METHOD #1
!   DelivEff = (delivery) / (diversion)
!
!      DO IDVS = 1,DIVCOUNT
!        ! If no diversion, use previous value
!        IF ((DIVSEG(IDVS)%DIVORDER.EQ.DZ)
!     +      .OR.
!     +      (DIVSEG(IDVS)%DIVERSION.EQ.DZ)) THEN
!          DIVSEG(IDVS)%DELIVEFF    = DIVSEG_PREV(IDVS)%DELIVEFF
!        ! If diversion, compute efficiency
!        ELSE
!          ! Inflow to diversion area ...
!          ISEG   = DIVSEG(IDVS)%DivSeg
!          ISTR   = SEGRCH_IN(ISEG)
!          JSTR   = SEGRCH_OUT(ISEG)
!          DIVSEG(IDVS)%DELIVEFF = DIVSEG(IDVS)%DELIVERY
!     +                            / DIVSEG(IDVS)%DIVERSION
!        END IF !(DIVERSION.EQ.0)
!      END DO !(IDVS)

! IMF DEBUG -------------------------------------------------------------
! DELIVERY EFFICIENCY METHOD #2
!   DelivEff = 1.0 - (loss)/(diversion)
!            = 1.0 - (sum[segin]-sum[segout])/(diversion)
      DO IDVS = 1, DIVCOUNT
        ! If no diversion, use previous value
        IF (DIVSEG(IDVS)%DIVORDER < NEARZERO_6 .OR. DIVSEG(IDVS)%DIVERSION < NEARZERO_6 ) THEN
            DIVSEG(IDVS)%DELIVEFF    = DIVSEG_PREV(IDVS)%DELIVEFF
        ! If diversion, compute efficiency
        ELSE
          ! Inflow to diversion area ...
          ISEG   = DIVSEG(IDVS)%DivSeg
          ISTR   = SEGRCH_IN(ISEG)
          JSTR   = SEGRCH_OUT(ISEG)
          DIVIN  = STRM(10,ISTR)*SWO%DELT
          ! Inflow/Outflow from all segments in diversion area ...
          ! -- diversion segment...
          IF (SEGINFO(ISEG)%SegType.EQ.2) THEN
                SUMIN  = STRM(10,ISTR)*SWO%DELT
                SUMOUT = STRM(9,JSTR)*SWO%DELT
          ELSEIF (SEGINFO(ISEG)%SegType.EQ.3 .AND. SEGINFO(ISEG)%AuxID.NE.0) THEN
                IAUX    = SEGINFO(ISEG)%AuxID
                SUMIN   = DZ
                SUMOUT  = DZ
                JRCH = Z
                KRCH = AUXDEM(IAUX)%AuxID
                DO IRCH = ISTR, JSTR
                   JRCH = JRCH + ONE  !REACH NUMBER
                   IF(JRCH == KRCH) THEN
                       SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                       SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                   ELSE
                       SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                       SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                   END IF
                END DO
                !SUMIN  = STRM(10,ISTR)*SWO%DELT
                !SUMOUT = STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
          ELSE
                SUMIN  = DZ
                SUMOUT = DZ
          END IF !(SegType.EQ.2|3)
          !
          ! -- downstream segments (conveyance & aux delivery) ...
          !
          DO ITREE = 1, DNTREE_CON2(ISEG)%NTREE
            JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
            IF (SEGINFO(JSEG)%SegType.EQ.2) THEN
                  ISTR   = SEGRCH_IN(JSEG)
                  JSTR   = SEGRCH_OUT(JSEG)
                  SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                  SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT
            ELSEIF (SEGINFO(JSEG)%SegType.EQ.3 .AND. SEGINFO(JSEG)%AuxID.NE.0) THEN
                  IAUX   = SEGINFO(JSEG)%AuxID
                  ISTR   = SEGRCH_IN(JSEG)
                  JSTR   = SEGRCH_OUT(JSEG)
                  JRCH = Z
                  KRCH = AUXDEM(IAUX)%AuxID
                  DO IRCH = ISTR, JSTR
                     JRCH = JRCH + ONE  !REACH NUMBER
                     IF(JRCH == KRCH) THEN
                         SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                         SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                     ELSE
                         SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                         SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                     END IF
                  END DO
                  !IAUX   = SEGINFO(JSEG)%AuxID
                  !ISTR   = SEGRCH_IN(JSEG)
                  !JSTR   = SEGRCH_OUT(JSEG)
                  !SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                  !SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
            END IF !(SegType.EQ.2)
          END DO !(ITREE)
          !
          ! Conveyance efficiency for diversion area ...
          IF (DIVIN < NEARZERO_5) THEN
            DIVSEG(IDVS)%DELIVEFF = DIVSEG_PREV(IDVS)%DELIVEFF
          ELSE
            DIVSEG(IDVS)%DELIVEFF  = UNO - (SUMIN-SUMOUT)/DIVIN
          END IF !(DIVIN.EQ.0)
          ! IMF DEBUG
          ! Quick fix for efficiencies that blow-ups (positive or negative) ...
          ! Constrain efficiency to range (0.1,1.0)
          ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
          ! Blow-ups appears to be due to SFR network connectivity and lag between
          ! SFR, FMP, and SWOPS.
!          DIVSEG(IDVS)%DELIVEFF  = MAX( 0.1,
!     +                                  MIN(DIVSEG(IDVS)%DELIVEFF,1.0) )
          ! IMF DEBUG #2 --
          ! Reduce artificial constraint on delivery efficiency
          ! ... causes problems w/ low delivery efficiency in some cases
          ! ... Just constrain between (0.001,1.0)
          !IF( DIVSEG(IDVS)%DELIVEFF > UNO   ) DIVSEG(IDVS)%DELIVEFF = UNO  !ALLOW FOR >1 EFFICIENCIES CAUSE OF GW SEEPAGE seb
          IF    ( DIVSEG(IDVS)%DELIVEFF < CENTI) THEN
                  DIVSEG(IDVS)%DELIVEFF = CENTI
          ELSEIF( DIVSEG(IDVS)%DELIVEFF > 1D3) THEN !Efficiency gets too large
                  DIVSEG(IDVS)%DELIVEFF = 1D3
          END IF
          !
          !CALL RELAX_IT(DIVSEG(IDVS)%DELIVEFF,  DIVSEG_PREV(IDVS)%DELIVEFF, SWO%WTFACTOR(1))
          !
        END IF !(DIVERSION.EQ.0)
      END DO !(IDVS)
      !
      ! PARAMETERS @ SPLITS/BRANCHES
      !
! IMF DEBUG -------------------------------------------------------------
! DELIVERY EFFICIENCY METHOD #1
!   DelivEff = (delivery) / (diversion)!
!
!      DO ISPT = 1,SPTCOUNT
!        IDVS  = SPTSEG(ISPT)%DivID
!        ISEG  = SPTSEG(ISPT)%DivSeg
!        ! If no diversion into service area, use previous values for splits/branches
!        IF ((DIVSEG(IDVS)%DIVORDER.EQ.DZ)
!     +      .OR.
!     +      (DIVSEG(IDVS)%DIVERSION.EQ.DZ)) THEN
!          SPTSEG(ISPT)%DELIVEFF    = SPTSEG_PREV(ISPT)%DELIVEFF
!          NBRANCH = SPTSEG(ISPT)%NBRANCH
!          DO IBRC = 1,NBRANCH
!            SPTSEG(ISPT)%BrcDELIVEFF(IBRC)
!     +        = SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
!          END DO !(IBRC)
!        ! If no diversion into split, use previous values for split/branches
!        ELSE IF ((SPTSEG(ISPT)%DIVORDER.EQ.DZ)
!     +           .OR.
!     +           (SPTSEG(ISPT)%DIVERSION.EQ.DZ)) THEN
!          SPTSEG(ISPT)%DELIVEFF    = SPTSEG_PREV(ISPT)%DELIVEFF
!          NBRANCH = SPTSEG(ISPT)%NBRANCH
!          DO IBRC = 1,NBRANCH
!            SPTSEG(ISPT)%BrcDELIVEFF(IBRC)
!     +        = SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
!          END DO !(IBRC)
!        ! Otherwise, compute delivery efficiency for split/branches
!        ELSE
!          SPTSEG(ISPT)%DELIVEFF    = SPTSEG(ISPT)%DELIVERY
!     +                               / SPTSEG(ISPT)%DIVERSION
!          NBRANCH = SPTSEG(ISPT)%NBRANCH
!          DO IBRC = 1,NBRANCH
!            SPTSEG(ISPT)%BrcDELIVEFF(IBRC)
!     +        = SPTSEG(ISPT)%BrcDELIVERY(IBRC)
!     +          / SPTSEG(ISPT)%BrcDIVERSION(IBRC)
!          END DO !(IBRC)
!
!        END IF !(DIVORDER.EQ.0 or DIVERSION.EQ.0)
!      END DO !(ISPT)

! IMF DEBUG -------------------------------------------------------------
! DELIVERY EFFICIENCY METHOD #2
!   DelivEff = 1.0 - (loss)/(diversion)
!            = 1.0 - (sum[segin]-sum[segout])/(diversion)

      DO ISPT = 1,SPTCOUNT
        ! (1) If no diversion into DIVSEG (top of diversion network),
        !     then use previous value for split and all branches...
        IDVS  = SPTSEG(ISPT)%DivID
        !ISEG  = SPTSEG(ISPT)%DivSeg
        IF ( DIVSEG(IDVS)%DIVORDER < NEARZERO_6 .OR. DIVSEG(IDVS)%DIVERSION < NEARZERO_6 ) THEN
          SPTSEG(ISPT)%DELIVEFF    = SPTSEG_PREV(ISPT)%DELIVEFF
          !
          DO CONCURRENT (IBRC = 1:SPTSEG(ISPT)%NBRANCH)
            SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
          END DO !(IBRC)
        ! (2) If no diversion into SPTSEG (top of split),
        !     then use previous value for split and all branches ...
        ELSEIF ( SPTSEG(ISPT)%DIVORDER < NEARZERO_6 .OR. SPTSEG(ISPT)%DIVERSION < NEARZERO_6 ) THEN
          SPTSEG(ISPT)%DELIVEFF    = SPTSEG_PREV(ISPT)%DELIVEFF
          !
          DO CONCURRENT (IBRC = 1:SPTSEG(ISPT)%NBRANCH)
                                                SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
          END DO !(IBRC)
        ! (3) If diversion into DIVSEG and SPTSEG,
        !     then compute delivery efficiencies for split and branches ...
        ELSE
          ! ** SPLITS **
          ! Inflow to split ...
          ISEG   = SPTSEG(ISPT)%SplitSeg
          ISTR   = SEGRCH_IN(ISEG)
          JSTR   = SEGRCH_OUT(ISEG)
          SPTIN  = STRM(10,ISTR)*SWO%DELT
          ! Inflow/Outflow from all segments in diversion area ...
          ! -- split segment...
          IF (SEGINFO(ISEG)%SegType.EQ.2) THEN
                SUMIN  = STRM(10,ISTR)*SWO%DELT
                SUMOUT = STRM(9,JSTR)*SWO%DELT
          ELSEIF (SEGINFO(ISEG)%SegType.EQ.3 .AND. SEGINFO(ISEG)%AuxID.NE.0) THEN
                IAUX   = SEGINFO(ISEG)%AuxID
                SUMIN   = DZ
                SUMOUT  = DZ
                JRCH = Z
                KRCH = AUXDEM(IAUX)%AuxID
                DO IRCH = ISTR, JSTR
                   JRCH = JRCH + ONE  !REACH NUMBER
                   IF(JRCH == KRCH) THEN
                       SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                       SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                   ELSE
                       SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                       SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                   END IF
                END DO
                !SUMIN  = STRM(10,ISTR)*SWO%DELT
                !SUMOUT = STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
          ELSE
              SUMIN  = DZ
              SUMOUT = DZ
          END IF !(SegType.EQ.2|3)
          ! -- downstream segments (conveyance & aux delivery) ...
          !
          DO ITREE = 1, DNTREE_CON2(ISEG)%NTREE
            JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
            IF (SEGINFO(JSEG)%SegType.EQ.2) THEN
              ISTR   = SEGRCH_IN(JSEG)
              JSTR   = SEGRCH_OUT(JSEG)
              SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
              SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT
            ELSEIF (SEGINFO(JSEG)%SegType.EQ.3 .AND. SEGINFO(JSEG)%AuxID.NE.0) THEN
                  IAUX   = SEGINFO(JSEG)%AuxID
                  ISTR   = SEGRCH_IN(JSEG)
                  JSTR   = SEGRCH_OUT(JSEG)
                  JRCH = Z
                  KRCH = AUXDEM(IAUX)%AuxID
                  DO IRCH = ISTR, JSTR
                     JRCH = JRCH + ONE  !REACH NUMBER
                     IF(JRCH == KRCH) THEN
                         SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                         SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                     ELSE
                         SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                         SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                     END IF
                  END DO
                  !SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                  !SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
            END IF !(SegType.EQ.2|3)
          END DO !(ITREE)
          !
          IF (SPTIN < NEARZERO_5) THEN
            SPTSEG(ISPT)%DELIVEFF = SPTSEG_PREV(ISPT)%DELIVEFF
          ELSE
            SPTSEG(ISPT)%DELIVEFF = UNO - (SUMIN-SUMOUT)/SPTIN
          END IF !(SPTIN.EQ.0)
          ! IMF DEBUG
          ! Quick fix for efficiencies that blow-ups (positive or negative) ...
          ! Constrain efficiency to range (0.1,1.0)
          ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
          ! Blow-ups appears to be due to SFR network connectivity and lag between
          ! SFR, FMP, and SWOPS.
!          SPTSEG(ISPT)%DELIVEFF  = MAX( 0.1,
!     +                                  MIN(SPTSEG(ISPT)%DELIVEFF,1.0) )
          ! IMF DEBUG #2 --
          ! Reduce artificial constraint on delivery efficiency
          ! ... causes problems w/ low delivery efficiency in some cases
          ! ... Just constrain between (0.001,1.0)
          !IF( SPTSEG(ISPT)%DELIVEFF > UNO   ) SPTSEG(ISPT)%DELIVEFF  = UNO
          IF    ( SPTSEG(ISPT)%DELIVEFF < CENTI) THEN
                  SPTSEG(ISPT)%DELIVEFF = CENTI
          ELSEIF( SPTSEG(ISPT)%DELIVEFF > 1D3) THEN !Efficiency gets too large
                  SPTSEG(ISPT)%DELIVEFF = 1D3
          END IF
          !
          !CALL RELAX_IT(SPTSEG(ISPT)%DELIVEFF,  SPTSEG_PREV(ISPT)%DELIVEFF, SWO%WTFACTOR(1))
          !
          !
          ! ** BRANCHES **
          !
          DO IBRC = 1, SPTSEG(ISPT)%NBRANCH
            ISEG  = SPTSEG(ISPT)%BrcSeg(IBRC)
            NTREE = DNTREE_CON2(ISEG)%NTREE
            ! If branch is a single delivery segment, force efficiency to 1.0
            ! TODO -- This is a klugey fix that assumes all deliveries are made
            !         from dummy segments with HCOND1=HCOND2=DZ ...
            !         Need to revisit this and revise for generality...
            IF ( NTREE.EQ.0 .AND. SEGINFO(ISEG)%SegType.EQ.3 ) THEN
              SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = UNO
            ! If no flow into branch, use previous value
            ELSE IF ( SPTSEG(ISPT)%BrcDIVERSION(IBRC)  < NEARZERO_6) THEN
              SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
            ! If flow into branch, compute efficiency
            ELSE
              ISTR   = SEGRCH_IN(ISEG)
              JSTR   = SEGRCH_OUT(ISEG)
              BRCIN  = STRM(10,ISTR)*SWO%DELT
              ! Inflow/Outflow from all segments in diversion area ...
              ! -- branch segment...
              IF (SEGINFO(ISEG)%SegType.EQ.2) THEN
                    SUMIN  = STRM(10,ISTR)*SWO%DELT
                    SUMOUT = STRM(9,JSTR)*SWO%DELT
              ELSEIF (SEGINFO(ISEG)%SegType.EQ.3 .AND. SEGINFO(ISEG)%AuxID.NE.0) THEN
                    IAUX   = SEGINFO(ISEG)%AuxID
                    SUMIN   = DZ
                    SUMOUT  = DZ
                    JRCH = Z
                    KRCH = AUXDEM(IAUX)%AuxID
                    DO IRCH = ISTR, JSTR
                       JRCH = JRCH + ONE  !REACH NUMBER
                       IF(JRCH == KRCH) THEN
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                       ELSE
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                       END IF
                    END DO
                    !IAUX   = SEGINFO(ISEG)%AuxID
                    !SUMIN  = STRM(10,ISTR)*SWO%DELT
                    !SUMOUT = STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
              ELSE
                    SUMIN  = DZ
                    SUMOUT = DZ
              END IF !(SegType.EQ.2|3)
              ! -- downstream segments (conveyance & aux delivery) ...
              DO ITREE = 1, NTREE
                JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
                IF (SEGINFO(JSEG)%SegType.EQ.2) THEN
                      ISTR   = SEGRCH_IN(JSEG)
                      JSTR   = SEGRCH_OUT(JSEG)
                      SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                      SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT
                ELSEIF (SEGINFO(JSEG)%SegType.EQ.3 .AND. SEGINFO(JSEG)%AuxID.NE.0) THEN
                      IAUX   = SEGINFO(JSEG)%AuxID
                      ISTR   = SEGRCH_IN(JSEG)
                      JSTR   = SEGRCH_OUT(JSEG)
                      JRCH = Z
                      KRCH = AUXDEM(IAUX)%AuxID
                      DO IRCH = ISTR, JSTR
                         JRCH = JRCH + ONE  !REACH NUMBER
                         IF(JRCH == KRCH) THEN
                             SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                             SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                         ELSE
                             SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                             SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                         END IF
                      END DO
                      !IAUX   = SEGINFO(JSEG)%AuxID
                      !ISTR   = SEGRCH_IN(JSEG)
                      !JSTR   = SEGRCH_OUT(JSEG)
                      !SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                      !SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                END IF !(SegType.EQ.2)
              END DO !(ITREE)
              IF (BRCIN.EQ.DZ) THEN
                SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC)
              ELSE
                SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = UNO - (SUMIN-SUMOUT)/BRCIN
              END IF !(BRCIN.EQ.DZ)
              ! IMF DEBUG
              ! Quick fix for efficiencies that blow-ups (positive or negative) ...
              ! Constrain efficiency to range (0.1,1.0)
              ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
              ! Blow-ups appears to be due to SFR network connectivity and lag between
              ! SFR, FMP, and SWOPS.
!              SPTSEG(ISPT)%BrcDELIVEFF(IBRC)
!     +          = MAX( 0.1,
!     +                 MIN(SPTSEG(ISPT)%BrcDELIVEFF(IBRC),1.0) )
              ! IMF DEBUG #2 --
              ! Reduce artificial constraint on delivery efficiency
              ! ... causes problems w/ low delivery efficiency in some cases
              ! ... Just constrain between (0.001,1.0)
              !IF( SPTSEG(ISPT)%BrcDELIVEFF(IBRC) > UNO    ) SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = UNO
              !IF( SPTSEG(ISPT)%BrcDELIVEFF(IBRC) < CENTI ) SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = CENTI
              IF    ( SPTSEG(ISPT)%BrcDELIVEFF(IBRC) < CENTI) THEN
                      SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = CENTI
              ELSEIF( SPTSEG(ISPT)%BrcDELIVEFF(IBRC) > 1D3) THEN !Efficiency gets too large
                      SPTSEG(ISPT)%BrcDELIVEFF(IBRC) = 1D3
              END IF
            END IF !((NTREE.EQ.0).AND.(SEGTYPE.EQ.3) / (DIVERSION.EQ.0))
          END DO !(IBRC)
        END IF !(DIVERSION.EQ.0)
      END DO !(ISPT)
      !
      ! DELIVERY EFFICIENCY @ UNIT
      ! The flow taken as "diversion" to a given unit depends on whether it
      ! receives water as bypass through another unit/district, and whether
      ! the upstream unit/district has called for water during a given step.
      ! Case 1: Unit receives direct diversion from river ...
      !         Efficiency is simply the sum of segment inflows
      !         minus sum of segment outflows, divided by the unit diversion
      !         (sum taken over all segments of SEGINFO%SegType=2 and SEGINFO%UnitID=IUNIT)
      ! Case 2: Unit receives water as bypass, upstream unit/district called for water
      !         In this case, the upstream unit eats the conveyance losses...
      !         Efficiency is taken as the sum of segment inflows
      !         minus sum of segment outflows, divided by the unit diversion.
      !         (sum is again taken over all segments of SEGINFO%SegType=2 and SEGINFO%UnitID=IUNIT.
      !          diversion is flow entering the unit, taken as flow entering the unit's charge segment)
      ! Case 3: Unit receives water as bypass, upstream unit/district does NOT call for water
      !         In this case, the receiving unit must pay the conveyance losses...
      !         Efficiency is taken as the sum of segment inflows
      !         minus sum of segment outflows, divided by the unit diversion.
      !         (here, sum is taken over all segments of SEGINFO%SegType=2 and SEGINFO%UnitID=IUNIT,
      !          PLUS segments of SEGINFO%SegType=2 in charge segment upstream tree.
      !          diversion is flow entering diversion segment (remote from unit))

! IMF DEBUG -------------------------------------------------------------
! DELIVERY EFFICIENCY METHOD #1
!   DelivEff = (delivery+bypass) / (diversion)
!   NOTE: Remote diversion case already accounted for in DIVERSION calculation
!
!      DO IUNIT = 1,NUNIT
!        ! current step
!        IF ((UNIT(IUNIT)%DIVORDER.EQ.DZ)
!     +       OR
!     +      (UNIT(IUNIT)%DIVERSION.EQ.DZ)) THEN
!          UNIT(IUNIT)%DELIVEFF = UNIT_PREV(IUNIT)%DELIVEFF
!        ELSE
!          UNIT(IUNIT)%DELIVEFF
!     +      = (UNIT(IUNIT)%DELIVERY+UNIT(IUNIT)%BYPASS)
!     +        / UNIT(IUNIT)%DIVERSION
!        END IF !(DIVORDER.EQ.0 or DIVERSION.EQ.0)
!        ! year-to-date
!        IF (UNIT(IUNIT)%DIVERSION_YTD.EQ.DZ) THEN
!          UNIT(IUNIT)%DELIVEFF_YTD = UNIT_PREV(IUNIT)%DELIVEFF_YTD
!        ELSE
!          UNIT(IUNIT)%DELIVEFF_YTD
!     +      = (UNIT(IUNIT)%DELIVERY_YTD+UNIT(IUNIT)%BYPASS_YTD)
!     +        / UNIT(IUNIT)%DIVERSION_YTD
!        END IF !(DIVERSION.EQ.0)
!      END DO !(IUNIT)

! IMF DEBUG -------------------------------------------------------------
! DELIVERY EFFICIENCY METHOD #2
!   DelivEff = 1.0 - (loss)/(diversion)
!            = 1.0 - (sum[segin]-sum[segout])/(diversion)
      !
      DO IUNIT = 1,NUNIT
        !
        DIVIN = DZ
        SUMIN = DZ
        SUMOUT = DZ
        !
        ! If no diversion, use previous value
        IF ( UNIT(IUNIT)%DIVORDER.EQ.DZ .OR. UNIT(IUNIT)%DIVERSION.EQ.DZ ) THEN
          !
          UNIT(IUNIT)%DELIVEFF = UNIT_PREV(IUNIT)%DELIVEFF
          !
          ! If diversion, compute efficiency
        ELSE
          ISEG   = UNIT(IUNIT)%ChgSeg
          IRCH   = UNIT(IUNIT)%ChgRch
          JSEG   = UNIT(IUNIT)%DivSeg
          JRCH   = UNIT(IUNIT)%DivRch

          ! Diversion @ Unit
          IF (ISEG.EQ.JSEG) THEN
            ISTR = SEGRCH_IN(ISEG)
            JSTR = SEGRCH_OUT(ISEG)
            ! Inflow to unit ...
            DIVIN  = STRM(10,ISTR)*SWO%DELT
            ! Inflow/Outflow from all segments in diversion area ...
            ! -- diversion segment...
            IF (SEGINFO(ISEG)%SegType.EQ.2) THEN
              SUMIN  = STRM(10,ISTR)*SWO%DELT
              SUMOUT = STRM(9,JSTR)*SWO%DELT
            ELSE
              IF (SEGINFO(ISEG)%SegType.EQ.3) THEN
                IF (SEGINFO(ISEG)%AuxID.NE.0) THEN
                  IAUX   = SEGINFO(ISEG)%AuxID
                  SUMIN   = DZ
                  SUMOUT  = DZ
                  JRCH = Z
                  KRCH = AUXDEM(IAUX)%AuxID
                  DO IRCH = ISTR, JSTR
                     JRCH = JRCH + ONE  !REACH NUMBER
                     IF(JRCH == KRCH) THEN
                         SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                         SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                     ELSE
                         SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                         SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                     END IF
                  END DO
                  !IAUX   = SEGINFO(ISEG)%AuxID
                  !SUMIN  = STRM(10,ISTR)*SWO%DELT
                  !SUMOUT = STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                END IF !(AuxID.NE.0)
              END IF !(SegType.EQ.3)
            END IF !(SegType.EQ.2)
            ! -- downstream segments (conveyance & aux delivery) ...
            NTREE  = DNTREE_CON2(ISEG)%NTREE
            DO ITREE = 1,NTREE
              JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
              IF (SEGINFO(JSEG)%SegType.EQ.2) THEN
                ISTR   = SEGRCH_IN(JSEG)
                JSTR   = SEGRCH_OUT(JSEG)
                SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT
              ELSE
                IF (SEGINFO(JSEG)%SegType.EQ.3) THEN
                  IF (SEGINFO(JSEG)%AuxID.NE.0) THEN
                    IAUX   = SEGINFO(JSEG)%AuxID
                    ISTR   = SEGRCH_IN(JSEG)
                    JSTR   = SEGRCH_OUT(JSEG)
                    JRCH = Z
                    KRCH = AUXDEM(IAUX)%AuxID
                    DO IRCH = ISTR, JSTR
                       JRCH = JRCH + ONE  !REACH NUMBER
                       IF(JRCH == KRCH) THEN
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                       ELSE
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                       END IF
                    END DO
                    !IAUX   = SEGINFO(JSEG)%AuxID
                    !ISTR   = SEGRCH_IN(JSEG)
                    !JSTR   = SEGRCH_OUT(JSEG)
                    !SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                    !SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                  END IF !(AuxID.NE.0)
                END IF !(SegType.EQ.3)
              END IF !(SegType.EQ.2)
            END DO !(ITREE)

          ! Diversion @ Remote Point (receives water via bypass)
          ! ... sum delivery orders downstream of diversion seg and NOT in IUNIT
          ! ... if sum=0, unit efficiency includes carriage loss
          ! ... if sum>0, unit efficiency does NOT include carriage loss
          ELSE
            !
            ! loop over segments downstream of unit diversion point
            ! *AND* not in unit ... see if there are orders in system that are not in unit
            ORDER = DZ
            NTREE = DNTREE_CON2(JSEG)%NTREE
            DO ITREE = 1,NTREE
              KSEG  = DNTREE_CON2(JSEG)%SEGTREE(ITREE)
              IF ( SEGINFO(KSEG)%SegType.EQ.3 .AND. SEGINFO(KSEG)%UnitID.NE.IUNIT ) THEN
                F = SEGINFO(KSEG)%FarmID
                IAUX  = SEGINFO(KSEG)%AuxID
                IF (F.NE.0) ORDER = ORDER + FARM(F)%DELORDER
                IF (IAUX.NE.0)  ORDER = ORDER + AUXDEM(IAUX)%DELORDER
              END IF !(SegType.EQ.3)
            END DO !(ITREE)

            ! upstream order -- efficiency only includes unit segments
            ! (same as if ISEG=JSEG)
            IF (ORDER.GT.DZ) THEN

              ISTR = SEGRCH_IN(ISEG)
              JSTR = SEGRCH_OUT(ISEG)
              ! Inflow to unit ...
              DIVIN  = STRM(10,ISTR)*SWO%DELT
              ! Inflow/Outflow from all segments in diversion area ...
              ! -- diversion segment...
              IF (SEGINFO(ISEG)%SegType.EQ.2) THEN
                SUMIN  = STRM(10,ISTR)*SWO%DELT
                SUMOUT = STRM(9,JSTR)*SWO%DELT
              ELSE
                IF (SEGINFO(ISEG)%SegType.EQ.3) THEN
                  IF (SEGINFO(ISEG)%AuxID.NE.0) THEN
                    IAUX   = SEGINFO(ISEG)%AuxID
                    SUMIN   = DZ
                    SUMOUT  = DZ
                    JRCH = Z
                    KRCH = AUXDEM(IAUX)%AuxID
                    DO IRCH = ISTR, JSTR
                       JRCH = JRCH + ONE  !REACH NUMBER
                       IF(JRCH == KRCH) THEN
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                       ELSE
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                       END IF
                    END DO
                    !IAUX   = SEGINFO(ISEG)%AuxID
                    !SUMIN  = STRM(10,ISTR)*SWO%DELT
                    !SUMOUT = STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                  END IF !(AuxID.NE.0)
                END IF !(SegType.EQ.3)
              END IF !(SegType.EQ.2)
              ! -- downstream segments (conveyance & aux delivery) ...
              NTREE  = DNTREE_CON2(ISEG)%NTREE
              DO ITREE = 1,NTREE
                JSEG   = DNTREE_CON2(ISEG)%SEGTREE(ITREE)
                IF (SEGINFO(JSEG)%SegType.EQ.2) THEN
                  ISTR   = SEGRCH_IN(JSEG)
                  JSTR   = SEGRCH_OUT(JSEG)
                  SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                  SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT
                ELSE
                  IF (SEGINFO(JSEG)%SegType.EQ.3) THEN
                    IF (SEGINFO(JSEG)%AuxID.NE.0) THEN
                      IAUX   = SEGINFO(JSEG)%AuxID
                      ISTR   = SEGRCH_IN(JSEG)
                      JSTR   = SEGRCH_OUT(JSEG)
                      JRCH = Z
                      KRCH = AUXDEM(IAUX)%AuxID
                      DO IRCH = ISTR, JSTR
                         JRCH = JRCH + ONE  !REACH NUMBER
                         IF(JRCH == KRCH) THEN
                             SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                             SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                         ELSE
                             SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                             SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                         END IF
                      END DO
                      !IAUX   = SEGINFO(JSEG)%AuxID
                      !ISTR   = SEGRCH_IN(JSEG)
                      !JSTR   = SEGRCH_OUT(JSEG)
                      !SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                      !SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                    END IF !(AuxID.NE.0)
                  END IF !(SegType.EQ.3)
                END IF !(SegType.EQ.2)
              END DO !(ITREE)

            ! no upstream order -- efficiency includes carriage segments
            ! (i.e., same as above, but start at DivSeg (JSEG) instead of ChgSeg (ISEG))
            ELSE

              ISTR = SEGRCH_IN(JSEG)
              JSTR = SEGRCH_OUT(JSEG)
              ! Inflow to unit ...
              DIVIN  = STRM(10,ISTR)*SWO%DELT
              ! Inflow/Outflow from all segments in diversion area ...
              ! -- diversion segment...
              IF (SEGINFO(JSEG)%SegType.EQ.2) THEN
                SUMIN  = STRM(10,ISTR)*SWO%DELT
                SUMOUT = STRM(9,JSTR)*SWO%DELT
              ELSE
                IF (SEGINFO(JSEG)%SegType.EQ.3) THEN
                  IF (SEGINFO(JSEG)%AuxID.NE.0) THEN
                    IAUX   = SEGINFO(JSEG)%AuxID
                    SUMIN   = DZ
                    SUMOUT  = DZ
                    JRCH = Z
                    KRCH = AUXDEM(IAUX)%AuxID
                    DO IRCH = ISTR, JSTR
                       JRCH = JRCH + ONE  !REACH NUMBER
                       IF(JRCH == KRCH) THEN
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                       ELSE
                           SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                           SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                       END IF
                    END DO
                    !IAUX   = SEGINFO(JSEG)%AuxID
                    !SUMIN  = STRM(10,ISTR)*SWO%DELT
                    !SUMOUT = STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                  END IF !(AuxID.NE.0)
                END IF !(SegType.EQ.3)
              END IF !(SegType.EQ.2)
              ! -- downstream segments (conveyance & aux delivery) ...
              NTREE  = DNTREE_CON2(JSEG)%NTREE
              DO ITREE = 1,NTREE
                ISEG   = DNTREE_CON2(JSEG)%SEGTREE(ITREE)
                IF (SEGINFO(ISEG)%SegType.EQ.2) THEN
                  ISTR   = SEGRCH_IN(ISEG)
                  JSTR   = SEGRCH_OUT(ISEG)
                  SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                  SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT
                ELSE
                  IF (SEGINFO(ISEG)%SegType.EQ.3) THEN
                    IF (SEGINFO(ISEG)%AuxID.NE.0) THEN
                      IAUX   = SEGINFO(ISEG)%AuxID
                      ISTR   = SEGRCH_IN(ISEG)
                      JSTR   = SEGRCH_OUT(ISEG)
                      JRCH = Z
                      KRCH = AUXDEM(IAUX)%AuxID
                      DO IRCH = ISTR, JSTR
                         JRCH = JRCH + ONE  !REACH NUMBER
                         IF(JRCH == KRCH) THEN
                             SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                             SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                         ELSE
                             SUMIN  = SUMIN  + STRM(10,IRCH)*SWO%DELT
                             SUMOUT = SUMOUT + STRM( 9,IRCH)*SWO%DELT
                         END IF
                      END DO
                      !IAUX   = SEGINFO(ISEG)%AuxID
                      !ISTR   = SEGRCH_IN(ISEG)
                      !JSTR   = SEGRCH_OUT(ISEG)
                      !SUMIN  = SUMIN  + STRM(10,ISTR)*SWO%DELT
                      !SUMOUT = SUMOUT + STRM(9,JSTR)*SWO%DELT*AUXDEM(IAUX)%FACTOR
                    END IF !(AuxID.NE.0)
                  END IF !(SegType.EQ.3)
                END IF !(SegType.EQ.2)
              END DO !(ITREE)

            END IF !(ORDER.GT.0)
          END IF !(ISEG.EQ.JSEG)
          !
          UNIT(IUNIT)%DIVIN      = DIVIN
          UNIT(IUNIT)%DIVIN_YTD  = UNIT(IUNIT)%DIVIN_YTD + DIVIN
          UNIT(IUNIT)%SUMIN      = SUMIN
          UNIT(IUNIT)%SUMIN_YTD  = UNIT(IUNIT)%SUMIN_YTD + SUMIN
          UNIT(IUNIT)%SUMOUT     = SUMOUT
          UNIT(IUNIT)%SUMOUT_YTD = UNIT(IUNIT)%SUMOUT_YTD + SUMOUT
          !
          IF (DIVIN < NEARZERO_5) THEN
            UNIT(IUNIT)%DELIVEFF = UNIT_PREV(IUNIT)%DELIVEFF
            IF(DIST(IDIST)%DELIVEFF > UNO) DIST(IDIST)%DELIVEFF = UNO
          ELSE
            UNIT(IUNIT)%DELIVEFF  = UNO - (SUMIN-SUMOUT)/DIVIN
          END IF !(DIVIN.EQ.0)

          ! IMF DEBUG
          ! Quick fix for efficiencies that blow-ups (positive or negative) ...
          ! Constrain efficiency to range (0.1,1.0)
          ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
          ! Blow-ups appears to be due to SFR network connectivity and lag between
          ! SFR, FMP, and SWOPS.
!          UNIT(IUNIT)%DELIVEFF  = MAX( 0.1,
!     +                                 MIN(UNIT(IUNIT)%DELIVEFF,1.0) )
          ! IMF DEBUG #2 --
          ! Reduce artificial constraint on delivery efficiency
          ! ... causes problems w/ low delivery efficiency in some cases
          ! ... Just constrain between (0.001,1.0)
          !IF(UNIT(IUNIT)%DELIVEFF > UNO   ) UNIT(IUNIT)%DELIVEFF  = UNO
          IF    ( UNIT(IUNIT)%DELIVEFF < CENTI) THEN
                  UNIT(IUNIT)%DELIVEFF = CENTI
          ELSEIF( UNIT(IUNIT)%DELIVEFF > 1D3) THEN !Efficiency gets too large
                  UNIT(IUNIT)%DELIVEFF = 1D3
          END IF
          !
          !CALL RELAX_IT(UNIT(IUNIT)%DELIVEFF,  UNIT_PREV(IUNIT)%DELIVEFF, SWO%WTFACTOR(1))
          !
        END IF !(DIVORDER.EQ.0 or DIVERSION.EQ.0)

        IF (UNIT(IUNIT)%DIVIN_YTD.EQ.DZ) THEN
          UNIT(IUNIT)%DELIVEFF_YTD = UNIT_PREV(IUNIT)%DELIVEFF_YTD
        ELSE
          UNIT(IUNIT)%DELIVEFF_YTD = UNO - (UNIT(IUNIT)%SUMIN_YTD-UNIT(IUNIT)%SUMOUT_YTD) / UNIT(IUNIT)%DIVIN_YTD
      END IF
        ! IMF DEBUG
        ! Quick fix for efficiencies that blow-ups (positive or negative) ...
        ! Constrain efficiency to range (0.1,1.0)
        ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
        ! Blow-ups appears to be due to SFR network connectivity and lag between
        ! SFR, FMP, and SWOPS.
!        UNIT(IUNIT)%DELIVEFF_YTD
!     +    = MAX( 0.1,
!     +           MIN(UNIT(IUNIT)%DELIVEFF_YTD,1.0) )
        ! IMF DEBUG #2 --
        ! Reduce artificial constraint on delivery efficiency
        ! ... causes problems w/ low delivery efficiency in some cases
        ! ... Just constrain between (0.001,1.0)
        !IF(UNIT(IUNIT)%DELIVEFF_YTD > UNO   ) UNIT(IUNIT)%DELIVEFF_YTD = UNO
        IF(UNIT(IUNIT)%DELIVEFF_YTD < CENTI) UNIT(IUNIT)%DELIVEFF_YTD = CENTI
      END DO !(IUNIT)

      ! DELIVERY EFFICIENCY @ DISTRICT
      ! Zero out current values...
      DO CONCURRENT (IDIST = 1:NDIST )
        DIST(IDIST)%DIVIN      = DZ
        DIST(IDIST)%SUMIN      = DZ
        DIST(IDIST)%SUMOUT     = DZ
        DIST(IDIST)%DIVIN_YTD  = DZ
        DIST(IDIST)%SUMIN_YTD  = DZ
        DIST(IDIST)%SUMOUT_YTD = DZ
      END DO
      ! Compute current as sum over units
      DO IUNIT = 1, NUNIT
      IF(UNIT(IUNIT)%DistID.NE.Z) THEN
          IDIST  = UNIT(IUNIT)%DistID
          DIST(IDIST)%DIVIN      = DIST(IDIST)%DIVIN      + UNIT(IUNIT)%DIVIN
          DIST(IDIST)%SUMIN      = DIST(IDIST)%SUMIN      + UNIT(IUNIT)%SUMIN
          DIST(IDIST)%SUMOUT     = DIST(IDIST)%SUMOUT     + UNIT(IUNIT)%SUMOUT
          DIST(IDIST)%DIVIN_YTD  = DIST(IDIST)%DIVIN_YTD  + UNIT(IUNIT)%DIVIN_YTD
          DIST(IDIST)%SUMIN_YTD  = DIST(IDIST)%SUMIN_YTD  + UNIT(IUNIT)%SUMIN_YTD
          DIST(IDIST)%SUMOUT_YTD = DIST(IDIST)%SUMOUT_YTD + UNIT(IUNIT)%SUMOUT_YTD
      END IF
      END DO
      !
      ! Compute current efficiency
      DO IDIST = 1,NDIST
! IMF DEBUG ...
! Re-use efficiency if diversion ORDER is zero ...
! Diversions reduced incrementally, results in bad delivery efficiency
! at start of irrigation periods ...
        IF ( DIST(IDIST)%DIVIN < NEARZERO_6 .OR. DIST(IDIST)%DIVORDER < NEARZERO_6 ) THEN
          DIST(IDIST)%DELIVEFF = DIST_PREV(IDIST)%DELIVEFF
          IF(DIST(IDIST)%DELIVEFF > UNO) DIST(IDIST)%DELIVEFF = UNO
        ELSE
          DIST(IDIST)%DELIVEFF = UNO - (DIST(IDIST)%SUMIN-DIST(IDIST)%SUMOUT) / DIST(IDIST)%DIVIN
        END IF
        ! IMF DEBUG
        ! Quick fix for efficiencies that blow-ups (positive or negative) ...
        ! Constrain efficiency to range (0.1,1.0)
        ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
        ! Blow-ups appears to be due to SFR network connectivity and lag between
        ! SFR, FMP, and SWOPS.
!        DIST(IDIST)%DELIVEFF  = MAX( 0.1,
!     +                               MIN(DIST(IDIST)%DELIVEFF,1.0) )
        ! IMF DEBUG #2 --
        ! Reduce artificial constraint on delivery efficiency
        ! ... causes problems w/ low delivery efficiency in some cases
        ! ... Just constrain between (0.001,1.0)
         !IF(DIST(IDIST)%DELIVEFF > UNO   ) DIST(IDIST)%DELIVEFF  = UNO
          IF    ( DIST(IDIST)%DELIVEFF < CENTI) THEN
                  DIST(IDIST)%DELIVEFF = CENTI
          ELSEIF( DIST(IDIST)%DELIVEFF > 1D3) THEN !Efficiency gets too large
                  DIST(IDIST)%DELIVEFF = 1D3
          END IF
          !
          !CALL RELAX_IT(DIST(IDIST)%DELIVEFF, DIST_PREV(IDIST)%DELIVEFF, SWO%WTFACTOR(1))
          !
        ! Compute year-to-date sums/efficiency
! IMF DEBUG ...
! Re-use efficiency if diversion ORDER is zero ...
! Diversions reduced incrementally, results in bad delivery efficiency
! at start of irrigation periods ...
        IF ( DIST(IDIST)%DIVIN_YTD < NEARZERO_6 .OR. DIST(IDIST)%DIVORDER < NEARZERO_6 ) THEN
          DIST(IDIST)%DELIVEFF_YTD = DIST_PREV(IDIST)%DELIVEFF_YTD
        ELSE
          DIST(IDIST)%DELIVEFF_YTD = UNO - (DIST(IDIST)%SUMIN_YTD-DIST(IDIST)%SUMOUT_YTD) / DIST(IDIST)%DIVIN_YTD
        END IF
        ! IMF DEBUG
        ! Quick fix for efficiencies that blow-ups (positive or negative) ...
        ! Constrain efficiency to range (0.1,1.0)
        ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
        ! Blow-ups appears to be due to SFR network connectivity and lag between
        ! SFR, FMP, and SWOPS.
!        DIST(IDIST)%DELIVEFF_YTD
!     +    = MAX( 0.1,
!     +           MIN(DIST(IDIST)%DELIVEFF_YTD,1.0) )
        ! IMF DEBUG #2 --
        ! Reduce artificial constraint on delivery efficiency
        ! ... causes problems w/ low delivery efficiency in some cases
        ! ... Just constrain between (0.001,1.0)
        !IF( DIST(IDIST)%DELIVEFF_YTD > UNO   ) DIST(IDIST)%DELIVEFF_YTD =  UNO
        IF( DIST(IDIST)%DELIVEFF_YTD < CENTI) DIST(IDIST)%DELIVEFF_YTD = CENTI
        !
      END DO !(IDIST)

      ! DELIVERY EFFICIENCY @ PROJECT
      ! Zero out current values...
      DO CONCURRENT (IPROJ = 1:NPROJ )
        PROJ(IPROJ)%DIVIN      = DZ
        PROJ(IPROJ)%SUMIN      = DZ
        PROJ(IPROJ)%SUMOUT     = DZ
        PROJ(IPROJ)%DIVIN_YTD  = DZ
        PROJ(IPROJ)%SUMIN_YTD  = DZ
        PROJ(IPROJ)%SUMOUT_YTD = DZ
      END DO
      !
      ! Compute current as sum over units
      DO IUNIT = 1, NUNIT
      IF(UNIT(IUNIT)%ProjID.NE.Z) THEN
          IPROJ  = UNIT(IUNIT)%ProjID
          PROJ(IPROJ)%DIVIN      = PROJ(IPROJ)%DIVIN      + UNIT(IUNIT)%DIVIN
          PROJ(IPROJ)%SUMIN      = PROJ(IPROJ)%SUMIN      + UNIT(IUNIT)%SUMIN
          PROJ(IPROJ)%SUMOUT     = PROJ(IPROJ)%SUMOUT     + UNIT(IUNIT)%SUMOUT
          PROJ(IPROJ)%DIVIN_YTD  = PROJ(IPROJ)%DIVIN_YTD  + UNIT(IUNIT)%DIVIN_YTD
          PROJ(IPROJ)%SUMIN_YTD  = PROJ(IPROJ)%SUMIN_YTD  + UNIT(IUNIT)%SUMIN_YTD
          PROJ(IPROJ)%SUMOUT_YTD = PROJ(IPROJ)%SUMOUT_YTD + UNIT(IUNIT)%SUMOUT_YTD
      END IF
      END DO
      !
      DO IPROJ = 1, NPROJ
        ! Compute current efficiency
! IMF DEBUG ...
! Re-use efficiency if diversion ORDER is zero ...
! Diversions reduced incrementally, results in bad delivery efficiency
! at start of irrigation periods ...
        IF ( PROJ(IPROJ)%DIVIN < NEARZERO_6 .OR. PROJ(IPROJ)%DIVORDER < NEARZERO_6 ) THEN
          PROJ(IPROJ)%DELIVEFF = PROJ_PREV(IPROJ)%DELIVEFF
        ELSE
          PROJ(IPROJ)%DELIVEFF = UNO - (PROJ(IPROJ)%SUMIN-PROJ(IPROJ)%SUMOUT) / PROJ(IPROJ)%DIVIN
        END IF
        ! IMF DEBUG
        ! Quick fix for efficiencies that blow-ups (positive or negative) ...
        ! Constrain efficiency to range (0.1,1.0)
        ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
        ! Blow-ups appears to be due to SFR network connectivity and lag between
        ! SFR, FMP, and SWOPS.
!        PROJ(IPROJ)%DELIVEFF  = MAX( 0.1,
!     +                               MIN(PROJ(IPROJ)%DELIVEFF,1.0) )
        ! IMF DEBUG #2 --
        ! Reduce artificial constraint on delivery efficiency
        ! ... causes problems w/ low delivery efficiency in some cases
        ! ... Just constrain between (0.001,1.0)
        !IF( PROJ(IPROJ)%DELIVEFF > UNO    ) PROJ(IPROJ)%DELIVEFF = UNO
          IF    ( PROJ(IPROJ)%DELIVEFF < CENTI) THEN
                  PROJ(IPROJ)%DELIVEFF = CENTI
          ELSEIF( PROJ(IPROJ)%DELIVEFF > 1D3) THEN !Efficiency gets too large
                  PROJ(IPROJ)%DELIVEFF = 1D3
          END IF
          !
          !CALL RELAX_IT(PROJ(IPROJ)%DELIVEFF, PROJ_PREV(IPROJ)%DELIVEFF, SWO%WTFACTOR(1))
          !
        ! Compute year-to-date efficiency
! IMF DEBUG ...
! Re-use efficiency if diversion ORDER is zero ...
! Diversions reduced incrementally, results in bad delivery efficiency
! at start of irrigation periods ...
        IF ( PROJ(IPROJ)%DIVIN_YTD.EQ.DZ .OR. PROJ(IPROJ)%DIVORDER.EQ.DZ ) THEN
          PROJ(IPROJ)%DELIVEFF_YTD = PROJ_PREV(IPROJ)%DELIVEFF_YTD
        ELSE
          PROJ(IPROJ)%DELIVEFF_YTD = UNO - (PROJ(IPROJ)%SUMIN_YTD-PROJ(IPROJ)%SUMOUT_YTD) / PROJ(IPROJ)%DIVIN_YTD
        END IF
        ! IMF DEBUG
        ! Quick fix for efficiencies that blow-ups (positive or negative) ...
        ! Constrain efficiency to range (0.1,1.0)
        ! Lower end (0.1) is arbitrary ... but real systems shouldn't be any lower!
        ! Blow-ups appears to be due to SFR network connectivity and lag between
        ! SFR, FMP, and SWOPS.
!        PROJ(IPROJ)%DELIVEFF_YTD
!     +    = MAX( 0.1,
!     +           MIN(PROJ(IPROJ)%DELIVEFF_YTD,1.0) )
        ! IMF DEBUG #2 --
        ! Reduce artificial constraint on delivery efficiency
        ! ... causes problems w/ low delivery efficiency in some cases
        ! ... Just constrain between (0.001,1.0)
        !IF( PROJ(IPROJ)%DELIVEFF_YTD > UNO   ) PROJ(IPROJ)%DELIVEFF_YTD = UNO
        IF( PROJ(IPROJ)%DELIVEFF_YTD < CENTI) PROJ(IPROJ)%DELIVEFF_YTD = CENTI
      END DO !(IPROJ)
      !
      ! PARAMETERS @ PROJECT --
      ! (don't consider bypass ... project delivery and diversion
      !  computed by summing over diversion segments, which extend
      !  represent wet-water without consideration of units/districts)
      DO IPROJ = 1,NPROJ
        ! Diversion Ratio -- current step
! IMF DEBUG ...
! Was set to recompute divratio if any water released ...
! This resulted in blow-up of divratio for extremely small releases ... e.g., at non-irrigation periods (early itertions)
! Arbitrarily set cutoff at 0.5 AF release (21780 ft3)
!        IF ( (PROJ(IPROJ)%RELEASE.EQ.DZ)
!     +       .OR.
!     +       (PROJ(IPROJ)%DIVERSION.EQ.DZ) ) THEN
        !IF ( PROJ(IPROJ)%RELEASE.LT.21780.0 .OR. PROJ(IPROJ)%DIVERSION.LT.21780.0 ) THEN
          FACTOR = SWO%MIN_PROJ_ORDER(IPROJ) ! PROJ(IPROJ)%StorMax * NEARZERO_7
        IF ( PROJ(IPROJ)%RELEASE.LE.FACTOR .OR. PROJ(IPROJ)%DIVERSION.LE.FACTOR ) THEN
          PROJ(IPROJ)%DIVRATIO     = PROJ_PREV(IPROJ)%DIVRATIO
        ELSE
          PROJ(IPROJ)%DIVRATIO = PROJ(IPROJ)%DIVERSION / PROJ(IPROJ)%RELEASE
        END IF !(RELEASE.EQ.0)
        ! Diversion Ratio -- YTD through end of current step
! IMF DEBUG ...
! Was set to recompute divratio if any water released ...
! This resulted in blow-up of divratio for extremely small releases ... e.g., at non-irrigation periods (early itertions)
! Arbitrarily set cutoff at 0.5 AF release (21780 ft3)
!        IF ( (PROJ(IPROJ)%RELEASE_YTD.EQ.DZ)
!     +       .OR.
!     +       (PROJ(IPROJ)%DIVERSION_YTD.EQ.DZ) ) THEN
        IF ( PROJ(IPROJ)%RELEASE_YTD.LE.FACTOR .OR. PROJ(IPROJ)%DIVERSION_YTD.LE.FACTOR ) THEN
          PROJ(IPROJ)%DIVRATIO_YTD = PROJ_PREV(IPROJ)%DIVRATIO_YTD
        ELSE
          PROJ(IPROJ)%DIVRATIO_YTD = PROJ(IPROJ)%DIVERSION_YTD / PROJ(IPROJ)%RELEASE_YTD
        END IF !(RELEASE.EQ.0)
      END DO !(IPROJ)
      !
      ! ****************************************************************
      ! ALLOCATION/PAPER ACCOUNTING
      ! (compile charges, credits, balances...
      !  @ FARM, AUX, UNIT, DIST, PROJ ...
      !  @ BALANCE, CHARGE, CREDIT )
      ! ****************************************************************

      ! (1) Reset current-step and YTD values ...
      ! --- Project
      DO CONCURRENT (IPROJ = 1:NPROJ)
        PROJ(IPROJ)%ALLOCATION       = DZ
        PROJ(IPROJ)%CHARGE           = DZ
        PROJ(IPROJ)%CREDIT           = DZ
        PROJ(IPROJ)%CHGRATIO         = UNO
        PROJ(IPROJ)%NETCHGRATIO      = UNO
        PROJ(IPROJ)%CHARGE_YTD       = DZ                              ! recomputed as sum over unit YTD charges
        PROJ(IPROJ)%CREDIT_YTD       = DZ                              ! recomputed as sum over unit YTD credits
        PROJ(IPROJ)%CHGRATIO_YTD     = UNO
        PROJ(IPROJ)%NETCHGRATIO_YTD  = UNO
      END DO !(IPROJ)
      ! --- District
      DO CONCURRENT (IDIST = 1:NDIST)
        DIST(IDIST)%BALANCE          = DZ                              ! recomputed as sum over farm balances
        DIST(IDIST)%CHARGE           = DZ
        DIST(IDIST)%CREDIT           = DZ
        DIST(IDIST)%CHGRATIO         = UNO
        DIST(IDIST)%NETCHGRATIO      = UNO
        DIST(IDIST)%CHARGE_YTD       = DZ                              ! recomputed as sum over unit YTD charges
        DIST(IDIST)%CREDIT_YTD       = DZ                              ! recomputed as sum over unit YTD credits
        DIST(IDIST)%CHGRATIO_YTD     = UNO
        DIST(IDIST)%NETCHGRATIO_YTD  = UNO
      END DO !(IDIST)
      ! --- Unit
      DO CONCURRENT (IUNIT = 1:NUNIT)
        UNIT(IUNIT)%CHARGE           = DZ
        UNIT(IUNIT)%CREDIT           = DZ
        UNIT(IUNIT)%CHGRATIO         = UNO
        UNIT(IUNIT)%NETCHGRATIO      = UNO
        UNIT(IUNIT)%CHARGE_YTD       = UNIT_PREV(IUNIT)%CHARGE_YTD
        UNIT(IUNIT)%CREDIT_YTD       = UNIT_PREV(IUNIT)%CREDIT_YTD
        UNIT(IUNIT)%CHGRATIO_YTD     = UNIT_PREV(IUNIT)%CHGRATIO_YTD
        UNIT(IUNIT)%NETCHGRATIO_YTD  = UNIT_PREV(IUNIT)%NETCHGRATIO_YTD
      END DO !(IUNIT)
      ! --- Farm
      DO CONCURRENT (F = 1:NFARM)
        FARM(F)%BALANCE          = DZ
      END DO
      ! --- Aux
      DO CONCURRENT (IAUX = 1:NAUXDEM)
        AUXDEM(IAUX)%BALANCE         = DZ
      END DO !(IAUX)

      ! (2) Compute accounting releases, diversions, deliveries, bypass ...

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! ALLOCATION -- Total diversion allocation for project

      ! ALLOCATION @ DISTRICT
      DO IDIST = 1, NDIST
      IF(DIST(IDIST)%ProjID.NE.Z) THEN
        IPROJ  = DIST(IDIST)%ProjID
        PROJ(IPROJ)%ALLOCATION = PROJ(IPROJ)%ALLOCATION + DIST(IDIST)%ALLOC_TOTAL
      END IF
      END DO
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! CHARGES -- FMP doesn't deal with charges ...
      !            SWOPS computes computes charges for each unit/district/project

      ! Charges aggregated to:
      ! - Unit
      ! - District
      ! - Project

      ! CHARGE @ UNIT -- charge equal to flow into charge reach
      !                  OR flow into diversion reach, depending on situlation
      DO IUNIT = 1, NUNIT
        ! current step
        UNIT(IUNIT)%CHARGE = DZ
        ISEG   = UNIT(IUNIT)%ChgSeg
        IRCH   = UNIT(IUNIT)%ChgRch
        JSEG   = UNIT(IUNIT)%DivSeg
        JRCH   = UNIT(IUNIT)%DivRch
        FACTOR = UNIT(IUNIT)%ChgFactor
        IF(ISEG < 1) CYCLE
        IF(JSEG < 1) CYCLE
        IF(FACTOR < 1D-30) CYCLE
        IF(IRCH < 1) IRCH = 1
        IF(JRCH < 1) IRCH = 1
        ISTR   = SEGRCH_IN(ISEG) + IRCH - 1
        JSTR   = SEGRCH_IN(JSEG) + JRCH - 1
        ! Charge @ Diversion Point
        IF (ISEG.EQ.JSEG) THEN
          ! ... only charge if order made!
          IF (UNIT(IUNIT)%DIVORDER > DZ .AND. FACTOR > NEARZERO_30 ) THEN
            UNIT(IUNIT)%CHARGE = STRM(10,ISTR)*SWO%DELT*FACTOR;  IF(UNIT(IUNIT)%CHARGE < NEARZERO_6) UNIT(IUNIT)%CHARGE = DZ
          END IF !(DIVORDER.GT.0)
        ! Charge @ Delivery Point (i.e., unit supplied via remote diversion / bypass)
        ! ... if orders between diversion and charge locations, charge = flow into charge segment
        ! ... if NO orders between diversion and charge locations, charge = flow into DIVERSION segment
        ELSE
          ! only charge if order made!
          IF (UNIT(IUNIT)%DIVORDER > DZ) THEN
            ORDER = DZ
            NTREE = UPTREE_CON2(ISEG)%NTREE
            DO ITREE = 1, NTREE
            IF(SEGINFO( UPTREE_CON2(ISEG)%SEGTREE(ITREE) )%SegType == THREE) THEN
                KSEG  = UPTREE_CON2(ISEG)%SEGTREE(ITREE)
                F = SEGINFO(KSEG)%FarmID
                IAUX  = SEGINFO(KSEG)%AuxID
                IF (F.NE.0) ORDER = ORDER + FARM(F)%DELORDER
                IF (IAUX .NE.0) ORDER = ORDER + AUXDEM(IAUX)%DELORDER
            END IF
            END DO
            !
            IF (ORDER.GT.DZ) THEN
              UNIT(IUNIT)%CHARGE = STRM(10,ISTR)*SWO%DELT*FACTOR
            ELSE
              UNIT(IUNIT)%CHARGE = STRM(10,JSTR)*SWO%DELT*FACTOR
            END IF !(ORDER.GT.0)
            IF(UNIT(IUNIT)%CHARGE < NEARZERO_6) UNIT(IUNIT)%CHARGE = DZ
          ELSE
              UNIT(IUNIT)%CHARGE = DZ
          END IF !(DIVORDER.GT.0)
        END IF !(ISEG.EQ.JSEG)
      END DO !(IUNIT)
      !
      ! YTD through end of current step
      DO IUNIT=1, NUNIT
          UNIT(IUNIT)%CHARGE_YTD = UNIT(IUNIT)%CHARGE_YTD + UNIT(IUNIT)%CHARGE
      END DO
      !
      ! CHARGE @ DISTRICT -- charge equal to sum of unit charges over district
      DO IUNIT = 1, NUNIT
      IF(UNIT(IUNIT)%DistID.NE.Z) THEN
        IDIST  = UNIT(IUNIT)%DistID
        ! current step
        DIST(IDIST)%CHARGE = DIST(IDIST)%CHARGE + UNIT(IUNIT)%CHARGE
        ! YTD through end of current step
        DIST(IDIST)%CHARGE_YTD = DIST(IDIST)%CHARGE_YTD + UNIT(IUNIT)%CHARGE_YTD
      END IF
      END DO
      !
      ! CHARGE @ PROJECT -- charge equal to sum of district charges over project
      DO IUNIT = 1, NUNIT
      IF(UNIT(IUNIT)%ProjID.NE.Z) THEN
        IPROJ  = UNIT(IUNIT)%ProjID
        ! current step
        PROJ(IPROJ)%CHARGE = PROJ(IPROJ)%CHARGE + UNIT(IUNIT)%CHARGE
        ! YTD through end of current step
        PROJ(IPROJ)%CHARGE_YTD = PROJ(IPROJ)%CHARGE_YTD + UNIT(IUNIT)%CHARGE_YTD
      END IF
      END DO
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! CREDITS -- FMP doesn't deal with credits ...
      !            SWOPS computes computes credits for each unit/district/project

      ! Credits aggregated to:
      ! - DivSeg   (service area)
      ! - SplitSeg (sub-area)
      ! - Project

      ! CREDIT @ UNIT -- credit equal to flow out of credit reach
      !                  IF unit has incurred charge
      !                  (if unit has not incurred charge in current step,
      !                   then unit does not get credit for outflow ...
      !                   CAVEAT -- this assumes instantaneous flow through unit
      !                             i.e., no lag between charge/credit)
      DO IUNIT = 1, NUNIT
        UNIT(IUNIT)%CREDIT = DZ
        ! current step (sum over credit segments)
        ! ... if no charge, then no credit either
        IF (UNIT(IUNIT)%CHARGE < NEARZERO_6) THEN
          UNIT(IUNIT)%CREDIT = DZ
        ELSE
          DO I = 1, UNIT(IUNIT)%NCrdSeg
            ISEG   = UNIT(IUNIT)%CrdSeg(I)
            IRCH   = UNIT(IUNIT)%CrdRch(I)
            FACTOR = UNIT(IUNIT)%CrdFactor(I)
            ISTR   = SEGRCH_IN(ISEG) + IRCH - 1
            DTMP = STRM(9,ISTR)*SWO%DELT*FACTOR;     IF(DTMP < NEARZERO_6) DTMP = DZ
            !
            UNIT(IUNIT)%CREDIT = UNIT(IUNIT)%CREDIT + DTMP
          END DO !(TMP)
        END IF !(CHARGE.EQ.0)
        ! limit credit to charge
        IF(UNIT(IUNIT)%CREDIT > UNIT(IUNIT)%CHARGE) UNIT(IUNIT)%CREDIT = UNIT(IUNIT)%CHARGE

        ! YTD through end of current step
        UNIT(IUNIT)%CREDIT_YTD = UNIT(IUNIT)%CREDIT_YTD + UNIT(IUNIT)%CREDIT
      END DO !(IUNIT)
      !
      ! CREDIT @ DISTRICT -- credit equal to sum of unit credits over district
      DO IUNIT = 1, NUNIT
      IF(UNIT(IUNIT)%DistID.NE.Z) THEN
        IDIST  = UNIT(IUNIT)%DistID
        ! current step
        DIST(IDIST)%CREDIT     = DIST(IDIST)%CREDIT + UNIT(IUNIT)%CREDIT
        ! YTD through end of current step
        DIST(IDIST)%CREDIT_YTD = DIST(IDIST)%CREDIT_YTD + UNIT(IUNIT)%CREDIT_YTD
      END IF
      END DO

      ! CREDIT @ PROJECT -- credit equal to sum of district credits over project
      DO IUNIT = 1, NUNIT
      IF(UNIT(IUNIT)%ProjID.NE.Z) THEN
        IPROJ  = UNIT(IUNIT)%ProjID
        ! current step
        PROJ(IPROJ)%CREDIT     = PROJ(IPROJ)%CREDIT + UNIT(IUNIT)%CREDIT
        ! YTD through end of current step
        PROJ(IPROJ)%CREDIT_YTD = PROJ(IPROJ)%CREDIT_YTD + UNIT(IUNIT)%CREDIT_YTD
      END IF
      END DO

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! BALANCE    -- Farm and auxiliary balances are simply calculated as
      !               allotment (updated in FM) minus delivery_YTD (updated above)
      !
      ! BALANCE @ FARM
      DO F = 1, NFARM
          !
          I = FARM(F)%DistID
          !
          IF     (I == Z) THEN
                                                   FARM(F)%BALANCE = DZ
          ELSE
              IF(DIST(I)%S_ALLOTMENT_BYBEN) THEN
                                                   FARM(F)%BALANCE = SWO%FARM( DIST(I)%FARM(F) )%S_ALLOTMENT * SWO%DELT
              !ELSE IF(DIST(I)%S_ALLOTMENT.GE.DZ) THEN
              !                                         FARM(F)%BALANCE = DIST(I)%S_ALLOTMENT * FARM(F)%DIST_ALLOC_FRAC * SWO%DELT
              !ELSE IF(DIST(I)%S_ALLOTMENT_VOL.GE.DZ) THEN
              !                                         FARM(F)%BALANCE = DIST(I)%S_ALLOTMENT_VOL * FARM(F)%DIST_ALLOC_FRAC
              ELSE
                                                       FARM(F)%BALANCE = FARM(F)%ALLOTMENT-FARM(F)%DELIVERY_YTD
              ENDIF
              IF(FARM(F)%BALANCE < NEARZERO_6) FARM(F)%BALANCE = DZ
          END IF
      END DO

      ! BALANCE @ AUXILIARY
      DO IAUX = 1, NAUXDEM
                   I = AUXDEM(IAUX)%DistID
                   IF(     DIST(I)%S_ALLOTMENT_BYBEN) THEN
                        AUXDEM(IAUX)%BALANCE = SWO%AUXDEM( IAUX )%S_ALLOTMENT * SWO%DELT
                   ELSE
                        AUXDEM(IAUX)%BALANCE = AUXDEM(IAUX)%ALLOTMENT - (AUXDEM(IAUX)%DELIVERY_YTD*AUXDEM(IAUX)%FACTOR) ! Apply Aux delivery factor when computing balance!
                   END IF
                   IF(AUXDEM(IAUX)%BALANCE < NEARZERO_6) AUXDEM(IAUX)%BALANCE = DZ
      END DO

      ! BALANCE @ DISTRICT
      DO IDIST = 1, NDIST
                                     DIST(IDIST)%BALANCE = DIST(IDIST)%ALLOC_TOTAL - DIST(IDIST)%CHARGE_YTD + DIST(IDIST)%CREDIT_YTD
                                     IF(DIST(IDIST)%BALANCE < NEARZERO_6) DIST(IDIST)%BALANCE = DZ
      END DO
      !
      ! (3) Recompute accounting parameters ... charge efficiency, etc.
      !
      ! PARAMETERS @ PROJECT
! IMF DEBUG ...
! Was set to recompute chgratio and netchgratio if any water released ...
! This resulted in blow-up of chgratio for extremely small releases ... e.g., at non-irrigation periods (early itertions)
! Arbitrarily set cutoff at 0.5 AF release (21780 ft3)
      DO IPROJ = 1, NPROJ
        FACTOR = SWO%MIN_PROJ_ORDER(IPROJ) ! PROJ(IPROJ)%StorMax * NEARZERO_7
        ! Charge Ratio -- current step
        IF     (PROJ(IPROJ)%RELEASE.LE.FACTOR ) THEN
          PROJ(IPROJ)%CHGRATIO = UNO
        ELSEIF ( PROJ(IPROJ)%CHARGE.LE.FACTOR ) THEN
          PROJ(IPROJ)%CHGRATIO = PROJ_PREV(IPROJ)%CHGRATIO
        ELSE
          PROJ(IPROJ)%CHGRATIO = PROJ(IPROJ)%CHARGE / PROJ(IPROJ)%RELEASE
        END IF !(RELEASE.EQ.0)
        ! Charge Ratio -- YTD through end of current step
        IF ( PROJ(IPROJ)%RELEASE_YTD.LE.FACTOR ) THEN
          PROJ(IPROJ)%CHGRATIO_YTD = UNO
        ELSEIF( PROJ(IPROJ)%CHARGE_YTD.LE.FACTOR ) THEN
          PROJ(IPROJ)%CHGRATIO_YTD = PROJ_PREV(IPROJ)%CHGRATIO_YTD
        ELSE
          PROJ(IPROJ)%CHGRATIO_YTD = PROJ(IPROJ)%CHARGE_YTD / PROJ(IPROJ)%RELEASE_YTD
        END IF !(RELEASE.EQ.0)
        ! Net Charge Ratio -- current step
        IF ( PROJ(IPROJ)%RELEASE.LE.FACTOR) THEN
          PROJ(IPROJ)%NETCHGRATIO  = UNO
        ELSEIF(PROJ(IPROJ)%CHARGE-PROJ(IPROJ)%CREDIT.LE.FACTOR ) THEN
          PROJ(IPROJ)%NETCHGRATIO  = PROJ_PREV(IPROJ)%NETCHGRATIO
        ELSEIF(PROJ(IPROJ)%DELIVEFF > UNO) THEN
            PROJ(IPROJ)%NETCHGRATIO = (PROJ(IPROJ)%CHARGE - (PROJ(IPROJ)%CREDIT/PROJ(IPROJ)%DELIVEFF)) / PROJ(IPROJ)%RELEASE
        ELSE
          PROJ(IPROJ)%NETCHGRATIO = (PROJ(IPROJ)%CHARGE - PROJ(IPROJ)%CREDIT) / PROJ(IPROJ)%RELEASE
        END IF !(RELEASE.EQ.0)
        ! Net Charge Ratio -- YTD through end of current step
        IF ( PROJ(IPROJ)%RELEASE_YTD.LE.FACTOR) THEN
            PROJ(IPROJ)%NETCHGRATIO_YTD = UNO
        ELSEIF(PROJ(IPROJ)%CHARGE_YTD-PROJ(IPROJ)%CREDIT_YTD.LE.FACTOR) THEN
          PROJ(IPROJ)%NETCHGRATIO_YTD = PROJ_PREV(IPROJ)%NETCHGRATIO_YTD
        ELSE
          PROJ(IPROJ)%NETCHGRATIO_YTD = (PROJ(IPROJ)%CHARGE_YTD - PROJ(IPROJ)%CREDIT_YTD) / PROJ(IPROJ)%RELEASE_YTD
        END IF !RELEASE.EQ.0)
          !
          !CALL RELAX_IT(PROJ(IPROJ)%CHGRATIO, PROJ_PREV(IPROJ)%CHGRATIO, SWO%WTFACTOR(1))
          !
          !CALL RELAX_IT(PROJ(IPROJ)%NETCHGRATIO, PROJ_PREV(IPROJ)%NETCHGRATIO, SWO%WTFACTOR(1))
          !CALL DAMP_IT(PROJ(IPROJ)%NETCHGRATIO, PROJ_PREV(IPROJ)%NETCHGRATIO,PROJ_PREV(IPROJ)%NETCHGRATIO_PREV, SWO%WTFACTOR(1))
          !
      END DO !(IPROJ)
      !
      ! PARAMETERS @ DISTRICT
      DO IDIST = 1, NDIST
        ! Charge Ratio -- current step
        IF ( DIST(IDIST)%DIVERSION < UNO) THEN
            DIST(IDIST)%CHGRATIO  = UNO
        ELSEIF(DIST(IDIST)%CHARGE < NEARZERO_6 ) THEN
            DIST(IDIST)%CHGRATIO  = DIST_PREV(IDIST)%CHGRATIO
        ELSE
            DIST(IDIST)%CHGRATIO = DIST(IDIST)%CHARGE / DIST(IDIST)%DIVERSION
        END IF !(DIVERSION.EQ.0)
        ! Charge Ratio -- YTD through end of current step
        IF ( DIST(IDIST)%DIVERSION_YTD < UNO) THEN
          DIST(IDIST)%CHGRATIO_YTD = UNO
        ELSEIF(DIST(IDIST)%CHARGE_YTD < NEARZERO_6 ) THEN
          DIST(IDIST)%CHGRATIO_YTD = DIST_PREV(IDIST)%CHGRATIO_YTD
        ELSE
          DIST(IDIST)%CHGRATIO_YTD = DIST(IDIST)%CHARGE_YTD / DIST(IDIST)%DIVERSION_YTD
        END IF !(DIVERSION.EQ.0)
        ! Net Charge Ratio -- current step
        IF ( DIST(IDIST)%DIVERSION < UNO) THEN
          DIST(IDIST)%NETCHGRATIO  = UNO
        ELSEIF(IS_CLOSE(DIST(IDIST)%CHARGE, DIST(IDIST)%CREDIT) ) THEN
          DIST(IDIST)%NETCHGRATIO  = DIST_PREV(IDIST)%NETCHGRATIO
        !ELSEIF(DIST(IDIST)%DELIVEFF > UNO) THEN                      !EFF>1 INDICATES THAT THERE IS WATER GAINED IN THE SYSTEM WHILE IN TRANSIT
        !    DTMP = DIST(IDIST)%CHARGE/DIST(IDIST)%DELIVEFF
        !    IF(DTMP - DIST(IDIST)%CREDIT < DTMP*NEARZERO_5 ) THEN  !EFFECICIENY ADDS TOO MUCH WATER CAUSING LOW NETCHARGE
        !        DIST(IDIST)%NETCHGRATIO  = DIST_PREV(IDIST)%NETCHGRATIO
        !    ELSE
        !        DIST(IDIST)%NETCHGRATIO = (DIST(IDIST)%CHARGE-DIST(IDIST)%CREDIT) / DIST(IDIST)%DIVERSION
        !    END IF
        ELSE
                DIST(IDIST)%NETCHGRATIO = (DIST(IDIST)%CHARGE-DIST(IDIST)%CREDIT) / DIST(IDIST)%DIVERSION
        END IF !(DIVERSION.EQ.0)
        !
        DIST(IDIST)%NETCHGRATIO = DBLE(INT(DIST(IDIST)%NETCHGRATIO * HECTO))/HECTO !ONLY KEEP FIRST TWO DIGITS
        IF( DIST(IDIST)%NETCHGRATIO < TENTH) DIST(IDIST)%NETCHGRATIO = TENTH
        !
        !CALL RELAX_IT(DIST(IDIST)%NETCHGRATIO, DIST_PREV(IDIST)%NETCHGRATIO, SWO%WTFACTOR(1))
        !
        ! Charge Efficiency -- YTD through end of current step
        IF ( DIST(IDIST)%DIVERSION_YTD < UNO) THEN
          DIST(IDIST)%NETCHGRATIO_YTD = UNO
        ELSEIF( IS_CLOSE(DIST(IDIST)%CHARGE_YTD, DIST(IDIST)%CREDIT_YTD) ) THEN
          DIST(IDIST)%NETCHGRATIO_YTD = DIST_PREV(IDIST)%NETCHGRATIO_YTD
        ELSE
          DIST(IDIST)%NETCHGRATIO_YTD = (DIST(IDIST)%CHARGE_YTD-DIST(IDIST)%CREDIT_YTD) / DIST(IDIST)%DIVERSION_YTD
        END IF !(DIVERSION.EQ.0)
          !
          !CALL RELAX_IT(DIST(IDIST)%CHGRATIO, DIST_PREV(IDIST)%CHGRATIO, SWO%WTFACTOR(1))
          !
          !IF(KITER>FOUR) CALL RELAX_IT(DIST(IDIST)%NETCHGRATIO, DIST_PREV(IDIST)%NETCHGRATIO, 0.005D0)
          !IF(KITER>FOUR) CALL DAMP_IT(DIST(IDIST)%NETCHGRATIO, DIST_PREV(IDIST)%NETCHGRATIO,DIST_PREV(IDIST)%NETCHGRATIO_PREV,  0.015D0)
          !
      END DO !(IDIST)
      !WRITE(5555,'(3I6, *(1x F6.3))') KPER, KSTP, KITER, DIST(1)%DELIVEFF, DIST(1)%NETCHGRATIO, DIST(2)%DELIVEFF, DIST(2)%NETCHGRATIO

      ! PARAMETERS @ UNIT
      DO IUNIT = 1, NUNIT
        ! Charge Efficiency -- current step
        IF ( UNIT(IUNIT)%DIVERSION < UNO ) THEN
          UNIT(IUNIT)%CHGRATIO = UNO
        ELSEIF ( UNIT(IUNIT)%CHARGE < NEARZERO_6 ) THEN
          UNIT(IUNIT)%CHGRATIO = UNIT_PREV(IUNIT)%CHGRATIO
        ELSE
          UNIT(IUNIT)%CHGRATIO = UNIT(IUNIT)%CHARGE / UNIT(IUNIT)%DIVERSION
        END IF !(DIVERSION.EQ.0)
        ! Charge Efficiency -- YTD through end of current step
        IF ( UNIT(IUNIT)%DIVERSION_YTD < UNO ) THEN
            UNIT(IUNIT)%CHGRATIO_YTD = UNO
        ELSEIF(UNIT(IUNIT)%CHARGE_YTD < NEARZERO_6 ) THEN
          UNIT(IUNIT)%CHGRATIO_YTD = UNIT_PREV(IUNIT)%CHGRATIO_YTD
        ELSE
          UNIT(IUNIT)%CHGRATIO_YTD = UNIT(IUNIT)%CHARGE_YTD / UNIT(IUNIT)%DIVERSION_YTD
        END IF !(DIVERSION.EQ.0)
          !
          !CALL RELAX_IT(UNIT(IUNIT)%CHGRATIO, UNIT_PREV(IUNIT)%CHGRATIO,SWO%WTFACTOR(1))
          !
      END DO !(IUNIT)
      !
! IMF DEBUG ...
! Revised convergence criteria to encompass more of the key wet-water parameters,
! with less of the ratio-based metrics and accounting parameters ...
! NOTE -- still compute previous convergence parameters (DCNVG) ...
!         just don't use these to determine convergence ... use XCNVG.
!
! NEW ----
      !IF(KITER < TEN) ICNVG = Z  !WHEN SWO IS IN OPERATION REQUIRE 5 ITERATIONS
      !
      IF( ICNVG == ONE) THEN ! ONE-WATER HAS CONVERGED FOR GW-FLOW
          !
          ! ****************************************************************
          ! COMPUTE CONVERGENCE CRITERIA
          ! ... iteration change @ project release              WRT _CNVG(1)
          ! ... iteration change @ divseg  diversion            WRT _CNVG(1)
          ! ... iteration change @ divseg  delivery             WRT _CNVG(1)
          ! ... difference       @ divseg  diversion / divorder WRT _CNVG(2)
          ! ... total            @ outflow                      WRT _CNVG(3)
          ! ****************************************************************
          !
          ABS_CHK = DZ
          REL_CHK = DZ
          !
          ! release
          DO IPROJ = 1, NPROJ
            !
            ACHK = ABS(PROJ(IPROJ)%RELEASE - PROJ_PREV(IPROJ)%RELEASE)
            !
            IF (PROJ_PREV(IPROJ)%RELEASE.LE.UNO) THEN
                IF (PROJ(IPROJ)%RELEASE.LE.UNO) THEN
                    RCHK = DZ
                ELSE
                    RCHK = ACHK
                END IF !(RELEASE.EQ.0)
            ELSE
                    RCHK = ACHK / PROJ_PREV(IPROJ)%RELEASE
            END IF
            IF (ACHK > ABS_CHK(1)) ABS_CHK(1) = ACHK * SWO%DELT_INV  !convert to L3/T
            IF (RCHK > REL_CHK(1)) REL_CHK(1) = RCHK
          END DO
          !
          IF(SWO%REQFLOW%HAS_REQ) THEN
                DO I=ONE, SIZE(SWO%REQFLOW%REL)  !NUMBER OF UNIQUE REQUIRED PATHWAYS
                    !
                    ACHK = ABS(SWO%REQFLOW%REL(I) - SWO%REQFLOW%REL_OLD(I))
                    !
                    IF (SWO%REQFLOW%REL_OLD(I).LE.TENTH) THEN
                        IF (SWO%REQFLOW%REL(I).LE.TENTH) THEN
                            RCHK = DZ
                        ELSE
                            RCHK = ACHK
                        END IF
                    ELSE
                            RCHK = ACHK / SWO%REQFLOW%REL_OLD(I)
                    END IF
                    !
                    IF (ACHK > ABS_CHK(5)) ABS_CHK(5) = ACHK
                    IF (RCHK > REL_CHK(5)) REL_CHK(5) = RCHK
                END DO
          END IF
          !
          ! diversion
          DO IDVS = 1, DIVCOUNT
            !
            ACHK = ABS(DIVSEG(IDVS)%DIVERSION - DIVSEG_PREV(IDVS)%DIVERSION)
            !
            IF (DIVSEG_PREV(IDVS)%DIVERSION.LE.UNO) THEN
                IF (DIVSEG(IDVS)%DIVERSION.LE.UNO) THEN
                    RCHK = DZ
                ELSE
                    RCHK = ACHK
                END IF
            ELSE
                    RCHK = ACHK / DIVSEG_PREV(IDVS)%DIVERSION
            END IF
            !
            IF (ACHK > ABS_CHK(2)) ABS_CHK(2) = ACHK * SWO%DELT_INV
            IF (RCHK > REL_CHK(2)) REL_CHK(2) = RCHK
          END DO
          !
          ! delivery
          DO IDVS = 1, DIVCOUNT
            !
            ACHK = ABS(DIVSEG(IDVS)%DELIVERY - DIVSEG_PREV(IDVS)%DELIVERY)
            !
            IF (DIVSEG_PREV(IDVS)%DELIVERY.LE.UNO) THEN
                IF (DIVSEG(IDVS)%DELIVERY.LE.UNO) THEN
                    RCHK = DZ
                ELSE
                    RCHK = ACHK
                END IF
            ELSE
                RCHK = ACHK / DIVSEG_PREV(IDVS)%DELIVERY
            END IF
            !
            IF (ACHK > ABS_CHK(3)) ABS_CHK(3) = ACHK * SWO%DELT_INV
            IF (RCHK > REL_CHK(3)) REL_CHK(3) = RCHK
          END DO
          !
          ! SCOTT COMMENTED OUT TO ALL CONV AT FAILURE TO DELIVER WHAT IS ASKED
          ! diversion vs. divorder
          !!!DO CONCURRENT (IDVS = 1:DIVCOUNT)
          !!!  !
          !!!  ACHK = ABS(DIVSEG(IDVS)%DIVERSION - DIVSEG(IDVS)%DIVORDER)
          !!!  !
          !!!  IF (DIVSEG(IDVS)%DIVORDER.LE.UNO) THEN
          !!!      IF (DIVSEG(IDVS)%DIVERSION.LE.UNO) THEN
          !!!        RCHK = DZ
          !!!      ELSE
          !!!        RCHK = ACHK
          !!!      END IF
          !!!  ELSE
          !!!        RCHK = ACHK / DIVSEG(IDVS)%DIVORDER
          !!!  END IF
          !!!  !
          !!!  IF (ACHK > ABS_CHK(4)) ABS_CHK(4) = ACHK * SWO%DELT_INV
          !!!  IF (RCHK > REL_CHK(4)) REL_CHK(4) = RCHK
          !!!END DO !(IDVS)
          !
          ! outflow no longer a check in SWO
          !!!!!!!DO CONCURRENT (IPROJ = 1:NPROJ)
          !!!!!!!DO CONCURRENT (NRES = 1:NRES_BAL(IPROJ), PROJ(IPROJ)%RELEASE > DIEZ .AND. RESDAT(IPROJ)%RESBAL(NRES)%INUSE)
          !!!!!!!      ISEG  = RESDAT(IPROJ)%RESBAL(NRES)%RESBAL_RELSEG  ! release segment number
          !!!!!!!      NTREE = DNTREE_NAT(ISEG)%NTREE                    ! number of river segments downstream of release (in downstream tree)
          !!!!!!!      JSEG  = DNTREE_NAT(ISEG)%SEGTREE(NTREE)           ! segment number of LAST river segment in tree
          !!!!!!!      JSTR  = SEGRCH_OUT(JSEG)                          ! reach number of last reach of last river segment
          !!!!!!!      DTMP  = ABS(STRM(10,JSTR)) * DELT                 ! outflow from last reach of last river segment
          !!!!!!!      IF (DTMP > XCNVG(5)) XCNVG(5) = DTMP
          !!!!!!!END DO; END DO !(IPROJ)
!          !
!          ! ****************************************************************
!          ! COMPUTE STABILITY CHECK ...
!          ! ** NO LONGER USED AS CONVERGENCE CRITERIA -- JUST TO CHECK STABILITY!
!          ! -> wet-water
!          !    ... project release                         DCNVG(1)
!          !    ... project diversion                       DCNVG(2)
!          !    ... project diversion ratio                 DCNVG(3)
!          !    ... project delivery efficiency             DCNVG(4)
!          ! -> accounting
!          !    ... project allocation (sum over districts) DCNVG(5)
!          !    ... project charges                         DCNVG(6)
!          !    ... project charge ratio                    DCNVG(7)
!          !    ... project net charge ratio                DCNVG(8)
!          ! ****************************************************************
!
!          ! IMF DEBUG --
!          ! NOTE: Stability check parameters not used as closure criteria ...
!          !       See note above!
!
!          DCNVG=DZ
!          DO IPROJ = 1,NPROJ
!
!            ! RELEASE --> DCNVG(1)
!            IF (PROJ_PREV(IPROJ)%RELEASE.LE.UNO) THEN
!              IF (PROJ(IPROJ)%RELEASE.LE.UNO) DCNVG(1,IPROJ) = DZ
!              IF (PROJ(IPROJ)%RELEASE.GT.UNO) DCNVG(1,IPROJ) = UNO
!            ELSE
!              DCNVG(1,IPROJ) = ABS( PROJ(IPROJ)%RELEASE - PROJ_PREV(IPROJ)%RELEASE ) / PROJ_PREV(IPROJ)%RELEASE
!            END IF !(RELEASE.EQ.0)
!
!            ! DIVERSION --> DCNVG(2)
!            IF (PROJ_PREV(IPROJ)%DIVERSION.LE.UNO) THEN
!              IF (PROJ(IPROJ)%DIVERSION.LE.UNO) DCNVG(2,IPROJ) = DZ
!              IF (PROJ(IPROJ)%DIVERSION.GT.UNO) DCNVG(2,IPROJ) = UNO
!            ELSE
!              DCNVG(2,IPROJ) = ABS( PROJ(IPROJ)%DIVERSION - PROJ_PREV(IPROJ)%DIVERSION ) / PROJ_PREV(IPROJ)%DIVERSION
!            END IF !(DIVERSION.EQ.0)
!
!            ! DIVERSION RATIO --> DCNVG(3)
!            IF (PROJ_PREV(IPROJ)%DIVRATIO.EQ.DZ) THEN
!              IF (PROJ(IPROJ)%DIVRATIO.EQ.DZ) DCNVG(3,IPROJ) = DZ
!              IF (PROJ(IPROJ)%DIVRATIO.NE.DZ) DCNVG(3,IPROJ) = UNO
!            ELSE
!              DCNVG(3,IPROJ) = ABS( PROJ(IPROJ)%DIVRATIO - PROJ_PREV(IPROJ)%DIVRATIO ) / PROJ_PREV(IPROJ)%DIVRATIO
!            END IF !(DIVRATIO.EQ.0)
!
!            ! DELIVERY EFFICIENCY --> DCNVG(4)
!            IF (PROJ_PREV(IPROJ)%DELIVEFF.EQ.DZ) THEN
!              IF (PROJ(IPROJ)%DELIVEFF.EQ.DZ) DCNVG(4,IPROJ) = DZ
!              IF (PROJ(IPROJ)%DELIVEFF.NE.DZ) DCNVG(4,IPROJ) = UNO
!            ELSE
!              DCNVG(4,IPROJ) = ABS( PROJ(IPROJ)%DELIVEFF - PROJ_PREV(IPROJ)%DELIVEFF ) / PROJ_PREV(IPROJ)%DELIVEFF
!            END IF !(DELIVEFF.EQ.0)
!
!            ! ALLOCATION --> DCNVG(5)
!            IF (PROJ_PREV(IPROJ)%ALLOCATION.EQ.DZ) THEN
!              IF (PROJ(IPROJ)%ALLOCATION.EQ.DZ) DCNVG(5,IPROJ) = DZ
!              IF (PROJ(IPROJ)%ALLOCATION.NE.DZ) DCNVG(5,IPROJ) = UNO
!            ELSE
!              DCNVG(5,IPROJ) = ABS( PROJ(IPROJ)%ALLOCATION - PROJ_PREV(IPROJ)%ALLOCATION ) / PROJ_PREV(IPROJ)%ALLOCATION
!            END IF !(ALLOCATION.EQ.0)
!
!            ! CHARGE --> DCNVG(6)
!            IF (PROJ_PREV(IPROJ)%CHARGE.EQ.DZ) THEN
!              IF (PROJ(IPROJ)%CHARGE.EQ.DZ) DCNVG(6,IPROJ) = DZ
!              IF (PROJ(IPROJ)%CHARGE.NE.DZ) DCNVG(6,IPROJ) = UNO
!            ELSE
!              DCNVG(6,IPROJ) = ABS( PROJ(IPROJ)%CHARGE - PROJ_PREV(IPROJ)%CHARGE ) / PROJ_PREV(IPROJ)%CHARGE
!            END IF !(CHARGE.EQ.0)
!
!            ! CHGRATIO --> DCNVG(7)
!            IF (PROJ_PREV(IPROJ)%CHGRATIO.EQ.DZ) THEN
!              IF (PROJ(IPROJ)%CHGRATIO.EQ.DZ) DCNVG(7,IPROJ) = DZ
!              IF (PROJ(IPROJ)%CHGRATIO.NE.DZ) DCNVG(7,IPROJ) = UNO
!            ELSE
!              DCNVG(7,IPROJ) = ABS( PROJ(IPROJ)%CHGRATIO - PROJ_PREV(IPROJ)%CHGRATIO ) / PROJ_PREV(IPROJ)%CHGRATIO
!            END IF !(CHGRATIO.EQ.0)
!
!            ! NETCHGRATIO --> DCNVG(8)
!            IF (PROJ_PREV(IPROJ)%NETCHGRATIO.EQ.DZ) THEN
!              IF (PROJ(IPROJ)%NETCHGRATIO.EQ.DZ) DCNVG(8,IPROJ) = DZ
!              IF (PROJ(IPROJ)%NETCHGRATIO.NE.DZ) DCNVG(8,IPROJ) = UNO
!            ELSE
!              DCNVG(8,IPROJ) = ABS( PROJ(IPROJ)%NETCHGRATIO - PROJ_PREV(IPROJ)%NETCHGRATIO ) / PROJ_PREV(IPROJ)%NETCHGRATIO
!            END IF !(NETCHGRATIO.EQ.0)
!
!          END DO !(IPROJ)
          !
          ! ****************************************************************
          ! EVALUATE CONVERGENCE
          ! -> Any fractional change GREATER than MAXDCNVG results in NO convergence
          ! release    NOT converged WRT previous iteration SWMXCNVG(1)
          ! -- or --
          ! diversions NOT converged WRT previous iteration SWMXCNVG(2)
          ! -- or --
          ! deliveries NOT converged WRT previous iteration SWMXCNVG(3)
          ! -- or --
          ! diversions NOT converged WRT diversion orders   SWMXCNVG(4)
          ! -- or --
          ! required flow changing                          ABS_CNVG(5)
          ! ****************************************************************
          !
          IF ( ANY( ABS_CHK > ABS_CNVG ) .OR. ANY( REL_CHK > REL_CNVG ) ) THEN
            ! NOT converged ...
            ICNVG = 0
          ELSE
            ! converged ...
            ICNVG = 1
            !
            IF(SWO%RES_CONV_ERROR.NE.NL) CALL WARNING_MESSAGE(OUTPUT=SWO%IOUT, MSG= '   ---SWO Reservoir Final Storage and Area Calculation did NOT Converge---'//NL//'This warning was raised for the following reservoirs:'//NL//'   Project    Res_ID Balance_Res_ID')
            !WRITE(5556,'(*(1x F6.3))') DIST(1)%DELIVEFF, DIST(1)%NETCHGRATIO, DIST(2)%DELIVEFF, DIST(2)%NETCHGRATIO
          END IF !(XCNVG.GT.ABS_CNVG)
      END IF  !(ICNVG == ONE)
      !
      ! OUTPUT
      !
      IF(SWO%HAS_BY_ITERATION .AND. ( ICNVG == Z .OR. ICNVG == ONE )  ) THEN
          IF(SWO%NUM_PRNT_VAR > Z) THEN
              !
              ERROR = NL
              SWO%FMP_SW_LIMIT_RULZ = inf
              CALL VARIABLE_GET_GLOBAL_MODEL_PROPERTY(SWO%DEC_VAR, SWO%DEC_VAR%PROP_PUL, KPER, KSTP, KITER, TWO, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
              IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//'THE FOLLOWING ARE VARIABLE NAMES NOT FOUND WHEN PULLING AN OWHM PROPERTY FOR THE S-LANGUAGE OUTPUT'//NL//ERROR)
              !
              DO I=ONE, SWO%NUM_PRNT_VAR
                                        IF(KPER >= SWO%PRNT_VAR_FLG(I)) CALL SWO%DEC_VAR%PRINT( SWO%PRNT_VAR_NAM(I) )
              END DO
          END IF
          !
          IF (SWO%HAS_OUTPUT) THEN  !ALLOW FOR CALLING CHECK SUBROUTINE WITHOUT WRITING IO...SET ICNVG=-1
            IF(OUTPUT%FARM       %IS_OPEN .AND. OUTPUT%IT_FARM       ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%FARM       %IU, KPER, KSTP, KITER, "FARM")
            IF(OUTPUT%AUXDEM     %IS_OPEN .AND. OUTPUT%IT_AUXDEM     ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%AUXDEM     %IU, KPER, KSTP, KITER, "AUXD")
            IF(OUTPUT%UNIT       %IS_OPEN .AND. OUTPUT%IT_UNIT       ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%UNIT       %IU, KPER, KSTP, KITER, "UNIT")
            IF(OUTPUT%DIST       %IS_OPEN .AND. OUTPUT%IT_DIST       ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%DIST       %IU, KPER, KSTP, KITER, "DIST")
            IF(OUTPUT%PROJ       %IS_OPEN .AND. OUTPUT%IT_PROJ       ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%PROJ       %IU, KPER, KSTP, KITER, "PROJ")
            IF(OUTPUT%DivSeg     %IS_OPEN .AND. OUTPUT%IT_DivSeg     ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%DivSeg     %IU, KPER, KSTP, KITER, "DIVS")
            IF(OUTPUT%SplitSeg   %IS_OPEN .AND. OUTPUT%IT_SplitSeg   ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%SplitSeg   %IU, KPER, KSTP, KITER, "SPLT")
            IF(OUTPUT%Storage    %IS_OPEN .AND. OUTPUT%IT_Storage    ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%Storage    %IU, KPER, KSTP, KITER, "STOR")
            IF(OUTPUT%SFR        %IS_OPEN .AND. OUTPUT%IT_SFR        ) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%SFR        %IU, KPER, KSTP, KITER, "XSFR")
            IF(OUTPUT%Convergence%IS_OPEN .AND. OUTPUT%IT_Convergence) CALL SWO_OUTPUT_WRITE(SWO, OUTPUT%Convergence%IU, KPER, KSTP, KITER, "CNVG")
            !XOPT = ; CALL OUTPUT(IGRID,KPER,KSTP,KITER,"ALLC")
          END IF
      END IF
      !
      ! ****************************************************************
      ! SAVE LATEST VALUES TO *PREV*
      ! -> Regardless of convergence, save latest flow/accounting/parameter
      !    values for use in next iteration or next step ...
      ! -> Do *NOT* update year-to-date values here -- update in BD routine!
      ! ****************************************************************
      ! --- Project
      DO IPROJ = 1, NPROJ
        ! wet-water
        PROJ_PREV(IPROJ)%RELEASE         = PROJ(IPROJ)%RELEASE
        PROJ_PREV(IPROJ)%DIVERSION       = PROJ(IPROJ)%DIVERSION
        PROJ_PREV(IPROJ)%DELIVERY        = PROJ(IPROJ)%DELIVERY
        PROJ_PREV(IPROJ)%BYPASS          = PROJ(IPROJ)%BYPASS
        PROJ_PREV(IPROJ)%DELIVEFF        = PROJ(IPROJ)%DELIVEFF
        PROJ_PREV(IPROJ)%DIVRATIO        = PROJ(IPROJ)%DIVRATIO
        ! accounting
        PROJ_PREV(IPROJ)%ALLOCATION      = PROJ(IPROJ)%ALLOCATION
        PROJ_PREV(IPROJ)%TFDR            = PROJ(IPROJ)%TFDR
        PROJ_PREV(IPROJ)%DELORDER        = PROJ(IPROJ)%DELORDER
        PROJ_PREV(IPROJ)%DIVORDER        = PROJ(IPROJ)%DIVORDER
        PROJ_PREV(IPROJ)%CHARGE          = PROJ(IPROJ)%CHARGE
        PROJ_PREV(IPROJ)%CREDIT          = PROJ(IPROJ)%CREDIT
        PROJ_PREV(IPROJ)%CHGRATIO        = PROJ(IPROJ)%CHGRATIO
        !
        PROJ_PREV(IPROJ)%NETCHGRATIO_PREV= PROJ_PREV(IPROJ)%NETCHGRATIO
        PROJ_PREV(IPROJ)%NETCHGRATIO     = PROJ(IPROJ)%NETCHGRATIO
        PROJ(IPROJ)%NETCHGRATIO_PREV     = PROJ(IPROJ)%NETCHGRATIO
      END DO !(IPROJ)
      ! --- District
      DO IDIST = 1, NDIST
        ! wet-water
        DIST_PREV(IDIST)%DIVERSION       = DIST(IDIST)%DIVERSION
        DIST_PREV(IDIST)%DELIVERY        = DIST(IDIST)%DELIVERY
        DIST_PREV(IDIST)%BYPASS          = DIST(IDIST)%BYPASS
        DIST_PREV(IDIST)%DELIVEFF        = DIST(IDIST)%DELIVEFF
        ! accounting
        DIST_PREV(IDIST)%ALLOC_ANN       = DIST(IDIST)%ALLOC_ANN
        DIST_PREV(IDIST)%ALLOC_CO        = DIST(IDIST)%ALLOC_CO
        DIST_PREV(IDIST)%ALLOC_TOTAL     = DIST(IDIST)%ALLOC_TOTAL
        DIST_PREV(IDIST)%EQ_ALLOTMENT    = DIST(IDIST)%EQ_ALLOTMENT
        DIST_PREV(IDIST)%BALANCE         = DIST(IDIST)%BALANCE
        DIST_PREV(IDIST)%TFDR            = DIST(IDIST)%TFDR
        DIST_PREV(IDIST)%DELORDER        = DIST(IDIST)%DELORDER
        DIST_PREV(IDIST)%DIVORDER        = DIST(IDIST)%DIVORDER
        DIST_PREV(IDIST)%CHARGE          = DIST(IDIST)%CHARGE
        DIST_PREV(IDIST)%CREDIT          = DIST(IDIST)%CREDIT
        DIST_PREV(IDIST)%CHGRATIO        = DIST(IDIST)%CHGRATIO
        !
        DIST_PREV(IDIST)%NETCHGRATIO_PREV= DIST_PREV(IDIST)%NETCHGRATIO
        DIST_PREV(IDIST)%NETCHGRATIO     = DIST(IDIST)%NETCHGRATIO
        DIST(IDIST)%NETCHGRATIO_PREV     = DIST(IDIST)%NETCHGRATIO
      END DO !(IDIST)
      ! --- Unit
      DO I = 1, NUNIT
        ! wet-water
        UNIT_PREV(I)%DIVERSION       = UNIT(I)%DIVERSION
        UNIT_PREV(I)%DELIVERY        = UNIT(I)%DELIVERY
        UNIT_PREV(I)%BYPASS          = UNIT(I)%BYPASS
        UNIT_PREV(I)%DELIVEFF        = UNIT(I)%DELIVEFF
        ! accounting
        UNIT_PREV(I)%TFDR            = UNIT(I)%TFDR
        UNIT_PREV(I)%DELORDER        = UNIT(I)%DELORDER
        UNIT_PREV(I)%DIVORDER        = UNIT(I)%DIVORDER
        UNIT_PREV(I)%CHARGE          = UNIT(I)%CHARGE
        UNIT_PREV(I)%CREDIT          = UNIT(I)%CREDIT
        UNIT_PREV(I)%CHGRATIO        = UNIT(I)%CHGRATIO
        !
        UNIT_PREV(I)%NETCHGRATIO_PREV= UNIT_PREV(I)%NETCHGRATIO
        UNIT_PREV(I)%NETCHGRATIO     = UNIT(I)%NETCHGRATIO
        UNIT(I)%NETCHGRATIO_PREV     = UNIT(I)%NETCHGRATIO
        !
        SWO%UNIT_PREV(I)%ALLOTMENT   = SWO%UNIT(I)%ALLOTMENT
        SWO%UNIT_PREV(I)%BALANCE     = SWO%UNIT(I)%BALANCE
      END DO
      ! --- Farm
      DO F = 1, NFARM
        FARM_PREV(F)%TFDR            = FARM(F)%TFDR
        FARM_PREV(F)%ALLOTMENT       = FARM(F)%ALLOTMENT
        FARM_PREV(F)%BALANCE         = FARM(F)%BALANCE
        FARM_PREV(F)%DELORDER        = FARM(F)%DELORDER
        FARM_PREV(F)%DELIVERY        = FARM(F)%DELIVERY
      END DO
      ! --- Aux
      DO CONCURRENT (IAUX = 1:NAUXDEM)
        AUXDEM_PREV(IAUX)%DEMAND         = AUXDEM(IAUX)%DEMAND
        AUXDEM_PREV(IAUX)%ALLOTMENT      = AUXDEM(IAUX)%ALLOTMENT
        AUXDEM_PREV(IAUX)%BALANCE        = AUXDEM(IAUX)%BALANCE
        AUXDEM_PREV(IAUX)%DELORDER       = AUXDEM(IAUX)%DELORDER
        AUXDEM_PREV(IAUX)%DELIVERY       = AUXDEM(IAUX)%DELIVERY
      END DO
      ! --- Segment
      DO ISEG = 1, NSS
        SEGDATA_PREV(ISEG)%FLOW          = SEGDATA(ISEG)%FLOW
        SEGDATA_PREV(ISEG)%INFLOW        = SEGDATA(ISEG)%INFLOW
       !SEGDATA_PREV(ISEG)%OUTFLOW       = SEGDATA(ISEG)%OUTFLOW
        SEGDATA_PREV(ISEG)%SEEPAGE       = SEGDATA(ISEG)%SEEPAGE
        SEGDATA_PREV(ISEG)%RUNOFF        = SEGDATA(ISEG)%RUNOFF
        SEGDATA_PREV(ISEG)%STRM_ET       = SEGDATA(ISEG)%STRM_ET
        SEGDATA_PREV(ISEG)%STRM_PRCP     = SEGDATA(ISEG)%STRM_PRCP
        SEGDATA_PREV(ISEG)%DIVERSION     = SEGDATA(ISEG)%DIVERSION
        SEGDATA_PREV(ISEG)%DELIVERY      = SEGDATA(ISEG)%DELIVERY
        SEGDATA_PREV(ISEG)%EFFICIENCY    = SEGDATA(ISEG)%EFFICIENCY
      END DO !(ISEG)
      ! --- DivSeg
      DO IDVS = 1, DIVCOUNT
        DIVSEG_PREV(IDVS)%TFDR           = DIVSEG(IDVS)%TFDR
        DIVSEG_PREV(IDVS)%DELORDER       = DIVSEG(IDVS)%DELORDER
        DIVSEG_PREV(IDVS)%DIVORDER       = DIVSEG(IDVS)%DIVORDER
        DIVSEG_PREV(IDVS)%DIVERSION      = DIVSEG(IDVS)%DIVERSION
        DIVSEG_PREV(IDVS)%DELIVERY       = DIVSEG(IDVS)%DELIVERY
        DIVSEG_PREV(IDVS)%DELIVEFF       = DIVSEG(IDVS)%DELIVEFF
      END DO !(IDVS)
      ! --- SptSeg
      DO ISPT = 1, SPTCOUNT
        SPTSEG_PREV(ISPT)%TFDR           = SPTSEG(ISPT)%TFDR
        SPTSEG_PREV(ISPT)%DELORDER       = SPTSEG(ISPT)%DELORDER
        SPTSEG_PREV(ISPT)%DIVORDER       = SPTSEG(ISPT)%DIVORDER
        SPTSEG_PREV(ISPT)%DIVERSION      = SPTSEG(ISPT)%DIVERSION
        SPTSEG_PREV(ISPT)%DELIVERY       = SPTSEG(ISPT)%DELIVERY
        SPTSEG_PREV(ISPT)%DELIVEFF       = SPTSEG(ISPT)%DELIVEFF
        !
        DO IBRC = 1, SPTSEG(ISPT)%NBRANCH
          SPTSEG_PREV(ISPT)%BrcTFDR(IBRC)     = SPTSEG(ISPT)%BrcTFDR(IBRC)
          SPTSEG_PREV(ISPT)%BrcDELORDER(IBRC) = SPTSEG(ISPT)%BrcDELORDER(IBRC)
          SPTSEG_PREV(ISPT)%BrcDIVORDER(IBRC) = SPTSEG(ISPT)%BrcDIVORDER(IBRC)
          SPTSEG_PREV(ISPT)%BrcDIVERSION(IBRC)= SPTSEG(ISPT)%BrcDIVERSION(IBRC)
          SPTSEG_PREV(ISPT)%BrcDELIVERY(IBRC) = SPTSEG(ISPT)%BrcDELIVERY(IBRC)
          SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC) = SPTSEG(ISPT)%BrcDELIVEFF(IBRC)
        END DO !(IBRC)
      END DO !(ISPT)
      !
      ! -- Backup Required Releases
      !
      IF(SWO%REQFLOW%HAS_REQ) THEN
        DO IPROJ = 1, NPROJ
        DO IRES  = 1, SWO%NRES_BAL(IPROJ)
           RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF_PREV = RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_REQF
        END DO
        END DO
        !
        SWO%REQFLOW%REL_OLD = SWO%REQFLOW%REL
        !
      END IF
      !
    END ASSOCIATE !-------------------------------------------------------------------------------------------------------------------------------
    !
    IF(SWO%DEC_RUL_IT_END%INUSE) THEN
       IF( KITER >= TWO .AND. ICNVG .NE. NEG ) THEN
          !
          IF(SWO%PRT_RUL_IT_END%IS_OPEN) WRITE(SWO%PRT_RUL_TS%IU,'(// A, // 16x, A,// 16x, A)') REPEAT('-', 150), '    FOR STRESS PERIOD '//NUM2STR(KPER,-6)//' AND TIME STEP '//NUM2STR(KSTP)//' AND SOLVER ITERATION '//NUM2STR(KITER),'WITH A STARTING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP-1)%PRETTYPRINT('  ')//' AND ENDING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP)%PRETTYPRINT('  ')//'  ('//DATE_SP(KPER)%TS(KSTP-1)%STR_DYEAR()//' - '//DATE_SP(KPER)%TS(KSTP)%STR_DYEAR()//')'
          !
          CALL VARIABLE_GET_GLOBAL_MODEL_PROPERTY(SWO%DEC_VAR, SWO%DEC_VAR%PROP_PUL, KPER, KSTP, KITER, TWO, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
          !
          CALL SWO%DEC_RUL_IT_END%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_IT_END%IU )
          !
          CALL VARIABLE_SET_RETURN_VALUES(SWO%DEC_VAR, SWO%DEC_VAR%PROP_RET, KPER, KSTP, ERROR)
          !
          IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//'THE FOLLOWING ARE EITHER VARIABLE NAMES NOT FOUND WHEN PULLING AN OWHM PROPERTY FOR THE S-LANGUAGE'//NL//'OR SETTING AN OWHM PROPERTY WITH THE S-LANGUAGE'//NL//ERROR)
          !
          IF(SWO%PRT_RUL_IT_END%IS_OPEN) CALL SWO%PRT_RUL_IT_END%SIZE_CHECK()
       END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE SWO_FINALIZE_TIMESTEP(SWO,KPER,KSTP,KITER,STRM,SEG)
!-----VERSION X 2014.06.30 SWOPS1BD
!   ******************************************************************
!   OVERVIEW:
!   UPDATE SAVE CONVERGED VALUES TO PREV VARS...
!   WRITE OUTPUT
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    !USE SWOPSMODULE
    !USE SWOPSUTIL
    !USE GWFSFRMODULE, ONLY:SEG
!   ------------------------------------------------------------------
!      ARGUMENTS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA),                            INTENT(INOUT):: SWO
    INTEGER,                                    INTENT(IN   ):: KPER,KSTP,KITER
    REAL,             DIMENSION(:,:),CONTIGUOUS,INTENT(IN   ):: STRM
    REAL,             DIMENSION(:,:),CONTIGUOUS,INTENT(IN   ):: SEG
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
!   ------------------------------------------------------------------
    INTEGER:: IU, I,J,K,IPROJ,IDIST,IUNIT,IFARM,IAUX, ISEG,IBRC,ISPT,IDVS
    CHARACTER(:), ALLOCATABLE:: ERROR
    LOGICAL:: HAS_CLOSEOUT, HAS_TS_END
    DOUBLE PRECISION:: DYEAR, DYEAR2, STOR, AREA, ELEV, REL_TOT, REL_SPL, REL_OT, INFLOW, PRECIP, EVAP, TRANSF
    DOUBLE PRECISION:: REL_P, REL_R, REL_S, REL_M, REL_E, REL_F
    CHARACTER(15):: REL_MAX
    CHARACTER(19):: DATE
    LOGICAL:: BIN
!   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: DCNVG
!
    ! SWOPS1OUT/XCHECK
    !CHARACTER(4) :: XOPT
    !
    !BACKUP SFR FLOWS
    !
    IF( SWO%NSFR_OLD_FLO > Z ) THEN  !THERE ARE VARIABLES THAT SET TO SFR_INPUT_DATA_TYPES IN OR OUT FLOW
        !
        DO CONCURRENT (I=ONE:SWO%NSFR_OLD_FLO)
            !
            IF(SWO%SFR_OLD_FLO(I)%IS_INFLOW) THEN
                !
                SWO%SFR_OLD_FLO(I)%FLO  = STRM(10,SWO%SFR_OLD_FLO(I)%ISTRM)
            ELSE
                SWO%SFR_OLD_FLO(I)%FLO  = STRM( 9,SWO%SFR_OLD_FLO(I)%ISTRM)
            ENDIF
        END DO
    END IF
!
!   ------------------------------------------------------------------
!
    HAS_CLOSEOUT = ANY(SWO%PROJ(:)%AllocClose == ONE) .AND. SWO%DEC_RUL_CLOSEOUT%INUSE .AND. .NOT. SWO%DEC_RUL_CLOSEOUT%SKIP
    HAS_TS_END   = SWO%DEC_RUL_TS_END%INUSE .AND. .NOT. SWO%DEC_RUL_TS_END%SKIP
    !
    IF(HAS_STARTDATE) THEN  !SET SP STARTING DATE
        DATE = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
        DYEAR= DATE_SP(KPER)%TS(KSTP-1)%DYEAR
        DYEAR2=DATE_SP(KPER)%TS(KSTP  )%DYEAR
    ELSE
        DATE='   NaN'
        DYEAR = SIMTIME
        DYEAR2= SIMTIME
    END IF
    !
    ! OUTPUT
    IF (SWO%HAS_OUTPUT) THEN
      IF(SWO%OUTPUT%FARM       %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%FARM       %IU, KPER, KSTP, KITER, "FARM")
      IF(SWO%OUTPUT%AUXDEM     %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%AUXDEM     %IU, KPER, KSTP, KITER, "AUXD")
      IF(SWO%OUTPUT%UNIT       %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%UNIT       %IU, KPER, KSTP, KITER, "UNIT")
      IF(SWO%OUTPUT%DIST       %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%DIST       %IU, KPER, KSTP, KITER, "DIST")
      IF(SWO%OUTPUT%PROJ       %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%PROJ       %IU, KPER, KSTP, KITER, "PROJ")
      IF(SWO%OUTPUT%DivSeg     %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%DivSeg     %IU, KPER, KSTP, KITER, "DIVS")
      IF(SWO%OUTPUT%SplitSeg   %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%SplitSeg   %IU, KPER, KSTP, KITER, "SPLT")
      IF(SWO%OUTPUT%Storage    %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%Storage    %IU, KPER, KSTP, KITER, "STOR")
      IF(SWO%OUTPUT%SFR        %IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%SFR        %IU, KPER, KSTP, KITER, "XSFR")
      IF(SWO%OUTPUT%Convergence%IS_OPEN ) CALL SWO_OUTPUT_WRITE(SWO, SWO%OUTPUT%Convergence%IU, KPER, KSTP, KITER, "CNVG")
      !XOPT = ; CALL XCHECK(IGRID,KPER,KSTP,KITER,"ALLC")
    END IF
    !
    !RELEASE_MIN
    !RELEASE_FLOD
    !RELEASE_PROJ
    !RELEASE_ADDF
    !RELEASE_REQF
    !RELEASE_SPEC

    IF(SWO%PRT_RESDAT%IS_OPEN) THEN
        IU = SWO%PRT_RESDAT%IU
        BIN= SWO%PRT_RESDAT%BINARY
        !
        IF(BIN) THEN
            WRITE(IU) DATE,DYEAR,DYEAR2,KPER,KSTP,SWO%DELT
        ELSE
            WRITE(IU, '(A, 1x F13.7, 1x F13.7, 2I5, A15)', ADVANCE='NO') DATE,DYEAR,DYEAR2,KPER,KSTP,NUM2STR(SWO%DELT)
        END IF

        DO I = ONE,SWO%NPROJ
        DO J = ONE,SWO%NRES_BAL(I)
              STOR = SWO%RESDAT(I)%RESBAL(J)%STORAGE
              !
              CALL ACAP_STOR2AREA(SWO,I,J,STOR,AREA)
              CALL ACAP_STOR2ELEV(SWO,I,J,STOR,ELEV)
              STOR = STOR * SWO%STOR_CNVT
              AREA = AREA * SWO%AREA_CNVT
              ELEV = ELEV * SWO%LEN_CNVT
              !
              REL_TOT = SWO%RESDAT(I)%RESBAL(J)%RELEASE_TOT * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_SPL = SWO%RESDAT(I)%RESBAL(J)%SPILL_WAY   * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_OT  = SWO%RESDAT(I)%RESBAL(J)%OVER_TOP    * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              INFLOW = SWO%RESDAT(I)%RESBAL(J)%INFLOW + SWO%RESDAT(I)%RESBAL(J)%SFR_INFLOW
              INFLOW = INFLOW* SWO%DELT_INV * SWO%FLOW_CNVT
              !
              PRECIP = SWO%RESDAT(I)%RESBAL(J)%PRCP_FIN * SWO%DELT_INV * SWO%FLOW_CNVT
              EVAP   = SWO%RESDAT(I)%RESBAL(J)%EVAP_FIN * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              TRANSF = SWO%RESDAT(I)%RESBAL(J)%STORAGE_TRANSFER * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              IF(BIN) THEN
                  WRITE(IU) ELEV,STOR,REL_TOT,REL_SPL,REL_OT,INFLOW,PRECIP,EVAP,AREA,TRANSF
              ELSE
                  WRITE(IU, '(*(A15))', ADVANCE='NO') NUM2STR(ELEV), NUM2STR(STOR), NUM2STR(REL_TOT), NUM2STR(REL_SPL), NUM2STR(REL_OT), NUM2STR(INFLOW), NUM2STR(PRECIP), NUM2STR(EVAP), NUM2STR(AREA), NUM2STR(TRANSF)
              END IF
        END DO
        END DO
        !
        IF(.NOT. BIN) WRITE(IU, '(A)')
    END IF
    !
    IF(SWO%PRT_RESDAT_CMPCT%IS_OPEN) THEN
        IU = SWO%PRT_RESDAT_CMPCT%IU
        BIN= SWO%PRT_RESDAT_CMPCT%BINARY
        K = Z
        !'DATE_START           DYEAR_START      SP     TS RES_ID           DELT          STAGE        STORAGE        RELEASE       SPILLWAY        OVERTOP         INFLOW         PRECIP         EVAPOR           AREA       TRANSFER')

        DO I = ONE,SWO%NPROJ
        DO J = ONE,SWO%NRES_BAL(I)
              K = K + ONE
              !
              STOR = SWO%RESDAT(I)%RESBAL(J)%STORAGE
              !
              CALL ACAP_STOR2AREA(SWO,I,J,STOR,AREA)
              CALL ACAP_STOR2ELEV(SWO,I,J,STOR,ELEV)
              STOR = STOR * SWO%STOR_CNVT
              AREA = AREA * SWO%AREA_CNVT
              ELEV = ELEV * SWO%LEN_CNVT
              !
              REL_TOT = SWO%RESDAT(I)%RESBAL(J)%RELEASE_TOT * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_SPL = SWO%RESDAT(I)%RESBAL(J)%SPILL_WAY   * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_OT  = SWO%RESDAT(I)%RESBAL(J)%OVER_TOP    * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              INFLOW = SWO%RESDAT(I)%RESBAL(J)%INFLOW + SWO%RESDAT(I)%RESBAL(J)%SFR_INFLOW
              INFLOW = INFLOW* SWO%DELT_INV * SWO%FLOW_CNVT
              !
              PRECIP = SWO%RESDAT(I)%RESBAL(J)%PRCP_FIN * SWO%DELT_INV * SWO%FLOW_CNVT
              EVAP   = SWO%RESDAT(I)%RESBAL(J)%EVAP_FIN * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              TRANSF = SWO%RESDAT(I)%RESBAL(J)%STORAGE_TRANSFER * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              IF(DZ < SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE .AND. SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE < D100) THEN
                  REL_MAX = NUM2STR(SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE * SWO%FLOW_CNVT)
              ELSE
                  REL_MAX = 'inf'
              END IF
              REL_MAX = ADJUSTR(REL_MAX)
              !
              IF(BIN) THEN
                  WRITE(IU) DATE,DYEAR,DYEAR2,KPER,KSTP,K,SWO%DELT, ELEV, STOR, REL_TOT, REL_SPL, REL_OT, INFLOW, PRECIP, EVAP, AREA, TRANSF, SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE
              ELSE
                  WRITE(IU, '(A, 1x F13.7, 1x F13.7, 3I5, *(A15))') DATE,DYEAR,DYEAR2,KPER,KSTP,K,NUM2STR(SWO%DELT), NUM2STR(ELEV), NUM2STR(STOR), NUM2STR(REL_TOT), NUM2STR(REL_SPL), NUM2STR(REL_OT), NUM2STR(INFLOW), NUM2STR(PRECIP), NUM2STR(EVAP), NUM2STR(AREA), NUM2STR(TRANSF), REL_MAX
              END IF
        END DO
        END DO
    END IF
    !
    IF(SWO%PRT_RESDAT_DETAIL%IS_OPEN) THEN
        IU = SWO%PRT_RESDAT_DETAIL%IU
        BIN= SWO%PRT_RESDAT_DETAIL%BINARY
        K = Z
        !'DATE_START            DYEAR_START   SP   TS  RES           DELT          STAGE        STORAGE   RELEASE_PROJ   RELEASE_REQF  RELEASE_FLOOD  RELEASE_EXTRA   RELEASE_SPEC   RELEASE_MIN        SPILLWAY        OVERTOP         INFLOW         PRECIP         EVAPOR           AREA       TRANSFER')

        DO I = ONE,SWO%NPROJ
        DO J = ONE,SWO%NRES_BAL(I)
              K = K + ONE
              !
              STOR = SWO%RESDAT(I)%RESBAL(J)%STORAGE
              !
              CALL ACAP_STOR2AREA(SWO,I,J,STOR,AREA)
              CALL ACAP_STOR2ELEV(SWO,I,J,STOR,ELEV)
              STOR = STOR * SWO%STOR_CNVT
              AREA = AREA * SWO%AREA_CNVT
              ELEV = ELEV * SWO%LEN_CNVT
              !
              REL_P   = SWO%RESDAT(I)%RESBAL(J)%RELEASE_PROJ * SWO%DELT_INV * SWO%FLOW_CNVT - SWO%RESDAT(I)%RESBAL(J)%RELEASE_PROJ_ADD * SWO%DELT_INV * SWO%FLOW_CNVT  !Keep track of additional demand by project in EXTRA catagory
              REL_R   = SWO%RESDAT(I)%RESBAL(J)%RELEASE_REQF * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_S   = SWO%RESDAT(I)%RESBAL(J)%RELEASE_SPEC * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_E   = SWO%RESDAT(I)%RESBAL(J)%RELEASE_ADDF * SWO%DELT_INV * SWO%FLOW_CNVT + SWO%RESDAT(I)%RESBAL(J)%RELEASE_PROJ_ADD * SWO%DELT_INV * SWO%FLOW_CNVT  !+ ...
              REL_F   = SWO%RESDAT(I)%RESBAL(J)%RELEASE_FLOD * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_M   = SWO%RESDAT(I)%RESBAL(J)%RELEASE_MIN  * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              REL_SPL = SWO%RESDAT(I)%RESBAL(J)%SPILL_WAY   * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_OT  = SWO%RESDAT(I)%RESBAL(J)%OVER_TOP    * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              REL_SPL = SWO%RESDAT(I)%RESBAL(J)%SPILL_WAY   * SWO%DELT_INV * SWO%FLOW_CNVT
              REL_OT  = SWO%RESDAT(I)%RESBAL(J)%OVER_TOP    * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              INFLOW = SWO%RESDAT(I)%RESBAL(J)%INFLOW + SWO%RESDAT(I)%RESBAL(J)%SFR_INFLOW
              INFLOW = INFLOW* SWO%DELT_INV * SWO%FLOW_CNVT
              !
              PRECIP = SWO%RESDAT(I)%RESBAL(J)%PRCP_FIN * SWO%DELT_INV * SWO%FLOW_CNVT
              EVAP   = SWO%RESDAT(I)%RESBAL(J)%EVAP_FIN * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              TRANSF = SWO%RESDAT(I)%RESBAL(J)%STORAGE_TRANSFER * SWO%DELT_INV * SWO%FLOW_CNVT
              !
              IF(DZ < SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE .AND. SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE < D100) THEN
                  REL_MAX = NUM2STR(SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE * SWO%FLOW_CNVT)
              ELSE
                  REL_MAX = 'inf'
              END IF
              REL_MAX = ADJUSTR(REL_MAX)
              !
              IF(BIN) THEN   !                    STORAGE        RELEASE_PROJ  RELEASE_SPEC  RELEASE_FLOD  RELEASE_REQF  RELEASE_EXTRA       SPILLWAY        OVERTOP         INFLOW         PRECIP         EVAPOR           AREA       TRANSFER')
                  WRITE(IU) DATE,DYEAR,DYEAR2,KPER,KSTP,K,SWO%DELT, ELEV, STOR, REL_P, REL_R, REL_F, REL_E, REL_S, REL_M, REL_SPL, REL_OT, INFLOW, PRECIP, EVAP, AREA, TRANSF, SWO%RESDAT(I)%RESBAL(J)%MAX_RELEASE
              ELSE
                  WRITE(IU, '(A, 1x F13.7, 1x F13.7, 3I5, *(A15))') DATE,DYEAR,DYEAR2,KPER,KSTP,K,NUM2STR(SWO%DELT), NUM2STR(ELEV), NUM2STR(STOR), NUM2STR(REL_P), NUM2STR(REL_R), NUM2STR(REL_F), NUM2STR(REL_E), NUM2STR(REL_S), NUM2STR(REL_M), NUM2STR(REL_SPL), NUM2STR(REL_OT), NUM2STR(INFLOW), NUM2STR(PRECIP), NUM2STR(EVAP), NUM2STR(AREA), NUM2STR(TRANSF), REL_MAX
              END IF
        END DO
        END DO
    END IF
    !
    ! Save latest flow, accounting, and parameter values
    ! for use as initial value in next step
    ! --- Project
    DO CONCURRENT (IPROJ = 1:SWO%NPROJ)
      ! wet-water
      SWO%PROJ_PREV(IPROJ)%RELEASE         = SWO%PROJ(IPROJ)%RELEASE
      SWO%PROJ_PREV(IPROJ)%DIVERSION       = SWO%PROJ(IPROJ)%DIVERSION
      SWO%PROJ_PREV(IPROJ)%DELIVERY        = SWO%PROJ(IPROJ)%DELIVERY
      SWO%PROJ_PREV(IPROJ)%BYPASS          = SWO%PROJ(IPROJ)%BYPASS
      SWO%PROJ_PREV(IPROJ)%DELIVEFF        = SWO%PROJ(IPROJ)%DELIVEFF
      SWO%PROJ_PREV(IPROJ)%DIVRATIO        = SWO%PROJ(IPROJ)%DIVRATIO
      SWO%PROJ_PREV(IPROJ)%RELEASE_YTD     = SWO%PROJ(IPROJ)%RELEASE_YTD
      SWO%PROJ_PREV(IPROJ)%DIVERSION_YTD   = SWO%PROJ(IPROJ)%DIVERSION_YTD
      SWO%PROJ_PREV(IPROJ)%DELIVERY_YTD    = SWO%PROJ(IPROJ)%DELIVERY_YTD
      SWO%PROJ_PREV(IPROJ)%BYPASS_YTD      = SWO%PROJ(IPROJ)%BYPASS_YTD
      SWO%PROJ_PREV(IPROJ)%DELIVEFF_YTD    = SWO%PROJ(IPROJ)%DELIVEFF_YTD
      SWO%PROJ_PREV(IPROJ)%DIVRATIO_YTD    = SWO%PROJ(IPROJ)%DIVRATIO_YTD
      ! accounting
      SWO%PROJ_PREV(IPROJ)%ALLOCATION      = SWO%PROJ(IPROJ)%ALLOCATION
      SWO%PROJ_PREV(IPROJ)%TFDR            = SWO%PROJ(IPROJ)%TFDR
      SWO%PROJ_PREV(IPROJ)%DELORDER        = SWO%PROJ(IPROJ)%DELORDER
      SWO%PROJ_PREV(IPROJ)%DIVORDER        = SWO%PROJ(IPROJ)%DIVORDER
      SWO%PROJ_PREV(IPROJ)%CHARGE          = SWO%PROJ(IPROJ)%CHARGE
      SWO%PROJ_PREV(IPROJ)%CREDIT          = SWO%PROJ(IPROJ)%CREDIT
      SWO%PROJ_PREV(IPROJ)%CHGRATIO        = SWO%PROJ(IPROJ)%CHGRATIO
      SWO%PROJ_PREV(IPROJ)%NETCHGRATIO     = SWO%PROJ(IPROJ)%NETCHGRATIO
      SWO%PROJ_PREV(IPROJ)%CHARGE_YTD      = SWO%PROJ(IPROJ)%CHARGE_YTD
      SWO%PROJ_PREV(IPROJ)%CREDIT_YTD      = SWO%PROJ(IPROJ)%CREDIT_YTD
      SWO%PROJ_PREV(IPROJ)%CHGRATIO_YTD    = SWO%PROJ(IPROJ)%CHGRATIO_YTD
      SWO%PROJ_PREV(IPROJ)%NETCHGRATIO_YTD = SWO%PROJ(IPROJ)%NETCHGRATIO_YTD
    END DO !(IPROJ)
    ! --- District
    DO CONCURRENT (IDIST = 1:SWO%NDIST)
      ! wet-water
      SWO%DIST_PREV(IDIST)%DIVERSION       = SWO%DIST(IDIST)%DIVERSION
      SWO%DIST_PREV(IDIST)%DELIVERY        = SWO%DIST(IDIST)%DELIVERY
      SWO%DIST_PREV(IDIST)%BYPASS          = SWO%DIST(IDIST)%BYPASS
      SWO%DIST_PREV(IDIST)%DELIVEFF        = SWO%DIST(IDIST)%DELIVEFF
      SWO%DIST_PREV(IDIST)%DIVERSION_YTD   = SWO%DIST(IDIST)%DIVERSION_YTD
      SWO%DIST_PREV(IDIST)%DELIVERY_YTD    = SWO%DIST(IDIST)%DELIVERY_YTD
      SWO%DIST_PREV(IDIST)%BYPASS_YTD      = SWO%DIST(IDIST)%BYPASS_YTD
      SWO%DIST_PREV(IDIST)%DELIVEFF_YTD    = SWO%DIST(IDIST)%DELIVEFF_YTD
      ! accounting
      SWO%DIST_PREV(IDIST)%ALLOC_ANN       = SWO%DIST(IDIST)%ALLOC_ANN
      SWO%DIST_PREV(IDIST)%ALLOC_CO        = SWO%DIST(IDIST)%ALLOC_CO
      SWO%DIST_PREV(IDIST)%ALLOC_TOTAL     = SWO%DIST(IDIST)%ALLOC_TOTAL
      SWO%DIST_PREV(IDIST)%EQ_ALLOTMENT    = SWO%DIST(IDIST)%EQ_ALLOTMENT
      SWO%DIST_PREV(IDIST)%BALANCE         = SWO%DIST(IDIST)%BALANCE
      SWO%DIST_PREV(IDIST)%TFDR            = SWO%DIST(IDIST)%TFDR
      SWO%DIST_PREV(IDIST)%DELORDER        = SWO%DIST(IDIST)%DELORDER
      SWO%DIST_PREV(IDIST)%DIVORDER        = SWO%DIST(IDIST)%DIVORDER
      SWO%DIST_PREV(IDIST)%CHARGE          = SWO%DIST(IDIST)%CHARGE
      SWO%DIST_PREV(IDIST)%CREDIT          = SWO%DIST(IDIST)%CREDIT
      SWO%DIST_PREV(IDIST)%CHGRATIO        = SWO%DIST(IDIST)%CHGRATIO
      SWO%DIST_PREV(IDIST)%NETCHGRATIO     = SWO%DIST(IDIST)%NETCHGRATIO
      SWO%DIST_PREV(IDIST)%CHARGE_YTD      = SWO%DIST(IDIST)%CHARGE_YTD
      SWO%DIST_PREV(IDIST)%CREDIT_YTD      = SWO%DIST(IDIST)%CREDIT_YTD
      SWO%DIST_PREV(IDIST)%CHGRATIO_YTD    = SWO%DIST(IDIST)%CHGRATIO_YTD
      SWO%DIST_PREV(IDIST)%NETCHGRATIO_YTD = SWO%DIST(IDIST)%NETCHGRATIO_YTD
    END DO !(IDIST)
    ! --- Unit
    DO CONCURRENT (IUNIT = 1:SWO%NUNIT)
      ! wet-water
      SWO%UNIT_PREV(IUNIT)%DIVERSION       = SWO%UNIT(IUNIT)%DIVERSION
      SWO%UNIT_PREV(IUNIT)%DELIVERY        = SWO%UNIT(IUNIT)%DELIVERY
      SWO%UNIT_PREV(IUNIT)%BYPASS          = SWO%UNIT(IUNIT)%BYPASS
      SWO%UNIT_PREV(IUNIT)%DELIVEFF        = SWO%UNIT(IUNIT)%DELIVEFF
      SWO%UNIT_PREV(IUNIT)%DIVERSION_YTD   = SWO%UNIT(IUNIT)%DIVERSION_YTD
      SWO%UNIT_PREV(IUNIT)%DELIVERY_YTD    = SWO%UNIT(IUNIT)%DELIVERY_YTD
      SWO%UNIT_PREV(IUNIT)%BYPASS_YTD      = SWO%UNIT(IUNIT)%BYPASS_YTD
      SWO%UNIT_PREV(IUNIT)%DIVIN_YTD       = SWO%UNIT(IUNIT)%DIVIN_YTD
      SWO%UNIT_PREV(IUNIT)%SUMIN_YTD       = SWO%UNIT(IUNIT)%SUMIN_YTD
      SWO%UNIT_PREV(IUNIT)%SUMOUT_YTD      = SWO%UNIT(IUNIT)%SUMOUT_YTD
      SWO%UNIT_PREV(IUNIT)%DELIVEFF_YTD    = SWO%UNIT(IUNIT)%DELIVEFF_YTD
      ! accounting
      SWO%UNIT_PREV(IUNIT)%TFDR            = SWO%UNIT(IUNIT)%TFDR
      SWO%UNIT_PREV(IUNIT)%DELORDER        = SWO%UNIT(IUNIT)%DELORDER
      SWO%UNIT_PREV(IUNIT)%DIVORDER        = SWO%UNIT(IUNIT)%DIVORDER
      SWO%UNIT_PREV(IUNIT)%CHARGE          = SWO%UNIT(IUNIT)%CHARGE
      SWO%UNIT_PREV(IUNIT)%CREDIT          = SWO%UNIT(IUNIT)%CREDIT
      SWO%UNIT_PREV(IUNIT)%CHGRATIO        = SWO%UNIT(IUNIT)%CHGRATIO
      SWO%UNIT_PREV(IUNIT)%NETCHGRATIO     = SWO%UNIT(IUNIT)%NETCHGRATIO
      SWO%UNIT_PREV(IUNIT)%CHARGE_YTD      = SWO%UNIT(IUNIT)%CHARGE_YTD
      SWO%UNIT_PREV(IUNIT)%CREDIT_YTD      = SWO%UNIT(IUNIT)%CREDIT_YTD
      SWO%UNIT_PREV(IUNIT)%CHGRATIO_YTD    = SWO%UNIT(IUNIT)%CHGRATIO_YTD
      SWO%UNIT_PREV(IUNIT)%NETCHGRATIO_YTD = SWO%UNIT(IUNIT)%NETCHGRATIO_YTD
    END DO !(IUNIT)
    ! --- Farm
    DO CONCURRENT (IFARM = 1:SWO%NFARM)
      SWO%FARM_PREV(IFARM)%TFDR            = SWO%FARM(IFARM)%TFDR
      SWO%FARM_PREV(IFARM)%ALLOTMENT       = SWO%FARM(IFARM)%ALLOTMENT
      SWO%FARM_PREV(IFARM)%BALANCE         = SWO%FARM(IFARM)%BALANCE
      SWO%FARM_PREV(IFARM)%DELORDER        = SWO%FARM(IFARM)%DELORDER
      SWO%FARM_PREV(IFARM)%DELIVERY        = SWO%FARM(IFARM)%DELIVERY
      SWO%FARM_PREV(IFARM)%DELIVERY_YTD    = SWO%FARM(IFARM)%DELIVERY_YTD
    END DO !(IFARM)
    ! --- Aux
    DO CONCURRENT (IAUX = 1:SWO%NAUXDEM)
      SWO%AUXDEM_PREV(IAUX)%DEMAND         = SWO%AUXDEM(IAUX)%DEMAND
      SWO%AUXDEM_PREV(IAUX)%ALLOTMENT      = SWO%AUXDEM(IAUX)%ALLOTMENT
      SWO%AUXDEM_PREV(IAUX)%BALANCE        = SWO%AUXDEM(IAUX)%BALANCE
      SWO%AUXDEM_PREV(IAUX)%DELORDER       = SWO%AUXDEM(IAUX)%DELORDER
      SWO%AUXDEM_PREV(IAUX)%DELIVERY       = SWO%AUXDEM(IAUX)%DELIVERY
      SWO%AUXDEM_PREV(IAUX)%DELIVERY_YTD   = SWO%AUXDEM(IAUX)%DELIVERY_YTD
    END DO
    ! --- Segment
    DO CONCURRENT (ISEG = 1:SWO%NSEG)
      SWO%SEGDATA_PREV(ISEG)%FLOW          = SWO%SEGDATA(ISEG)%FLOW
      SWO%SEGDATA_PREV(ISEG)%INFLOW        = SWO%SEGDATA(ISEG)%INFLOW
      SWO%SEGDATA_PREV(ISEG)%OUTFLOW       = SWO%SEGDATA(ISEG)%OUTFLOW
      SWO%SEGDATA_PREV(ISEG)%SEEPAGE       = SWO%SEGDATA(ISEG)%SEEPAGE
      SWO%SEGDATA_PREV(ISEG)%RUNOFF        = SWO%SEGDATA(ISEG)%RUNOFF
      SWO%SEGDATA_PREV(ISEG)%STRM_ET       = SWO%SEGDATA(ISEG)%STRM_ET
      SWO%SEGDATA_PREV(ISEG)%STRM_PRCP     = SWO%SEGDATA(ISEG)%STRM_PRCP
      SWO%SEGDATA_PREV(ISEG)%DIVERSION     = SWO%SEGDATA(ISEG)%DIVERSION
      SWO%SEGDATA_PREV(ISEG)%DELIVERY      = SWO%SEGDATA(ISEG)%DELIVERY
      SWO%SEGDATA_PREV(ISEG)%EFFICIENCY    = SWO%SEGDATA(ISEG)%EFFICIENCY
    END DO !(ISEG)
    ! --- DivSeg
    DO CONCURRENT (IDVS = 1:SWO%DIVCOUNT)
      SWO%DIVSEG_PREV(IDVS)%TFDR           = SWO%DIVSEG(IDVS)%TFDR
      SWO%DIVSEG_PREV(IDVS)%DELORDER       = SWO%DIVSEG(IDVS)%DELORDER
      SWO%DIVSEG_PREV(IDVS)%DIVORDER       = SWO%DIVSEG(IDVS)%DIVORDER
      SWO%DIVSEG_PREV(IDVS)%DIVERSION      = SWO%DIVSEG(IDVS)%DIVERSION
      SWO%DIVSEG_PREV(IDVS)%DELIVERY       = SWO%DIVSEG(IDVS)%DELIVERY
      SWO%DIVSEG_PREV(IDVS)%DELIVEFF       = SWO%DIVSEG(IDVS)%DELIVEFF
    END DO !(IDVS)
    ! --- SptSeg
    DO CONCURRENT (ISPT = 1:SWO%SPTCOUNT)
      SWO%SPTSEG_PREV(ISPT)%TFDR           = SWO%SPTSEG(ISPT)%TFDR
      SWO%SPTSEG_PREV(ISPT)%DELORDER       = SWO%SPTSEG(ISPT)%DELORDER
      SWO%SPTSEG_PREV(ISPT)%DIVORDER       = SWO%SPTSEG(ISPT)%DIVORDER
      SWO%SPTSEG_PREV(ISPT)%DIVERSION      = SWO%SPTSEG(ISPT)%DIVERSION
      SWO%SPTSEG_PREV(ISPT)%DELIVERY       = SWO%SPTSEG(ISPT)%DELIVERY
      SWO%SPTSEG_PREV(ISPT)%DELIVEFF       = SWO%SPTSEG(ISPT)%DELIVEFF
      !
      DO CONCURRENT (IBRC = 1:SWO%SPTSEG(ISPT)%NBRANCH)
        SWO%SPTSEG_PREV(ISPT)%BrcTFDR(IBRC)     = SWO%SPTSEG(ISPT)%BrcTFDR(IBRC)
        SWO%SPTSEG_PREV(ISPT)%BrcDELORDER(IBRC) = SWO%SPTSEG(ISPT)%BrcDELORDER(IBRC)
        SWO%SPTSEG_PREV(ISPT)%BrcDIVORDER(IBRC) = SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC)
        SWO%SPTSEG_PREV(ISPT)%BrcDIVERSION(IBRC)= SWO%SPTSEG(ISPT)%BrcDIVERSION(IBRC)
        SWO%SPTSEG_PREV(ISPT)%BrcDELIVERY(IBRC) = SWO%SPTSEG(ISPT)%BrcDELIVERY(IBRC)
        SWO%SPTSEG_PREV(ISPT)%BrcDELIVEFF(IBRC) = SWO%SPTSEG(ISPT)%BrcDELIVEFF(IBRC)
      END DO !(IBRC)
    END DO !(ISPT)
    !
    ! UPDATE NECESSARY S-VARIABLES
    !
    ERROR = NL
    IF( SWO%NUM_PRNT_VAR > Z .OR. HAS_CLOSEOUT .OR. HAS_TS_END) THEN
        SWO%FMP_SW_LIMIT_RULZ = inf
        CALL VARIABLE_GET_GLOBAL_MODEL_PROPERTY(SWO%DEC_VAR, SWO%DEC_VAR%PROP_PUL, KPER, KSTP, KITER, TWO, ERROR) ! PULL VALUES FROM GLOBAL VARIABLES
    END IF
    IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG='SWO HAD FATAL ERRORS.'//NL//'THE FOLLOWING ARE VARIABLE NAMES NOT FOUND WHEN PULLING AN OWHM PROPERTY FOR THE S-LANGUAGE OUTPUT'//NL//ERROR)
    !
    ! CHECK FOR TIME STEP OUTPUT OPTIONS
    !
    IF(SWO%NUM_PRNT_VAR > Z) THEN
        IF(KSTP < NSTP(KPER)) THEN  !SWO%PRNT_VAR_FLG => TS 2  SP 1
            K = TWO
        ELSE
            K = ONE
        END IF
        DO I=ONE, SWO%NUM_PRNT_VAR
                                  IF(K .LE. SWO%PRNT_VAR_FLG(I)) CALL SWO%DEC_VAR%PRINT( SWO%PRNT_VAR_NAM(I) )
        END DO
    END IF
    !
    ! RUN END OF TIME STEP INSTRUCTIONS
    IF(HAS_TS_END) THEN
          !
          IF(SWO%PRT_RUL_TS_END%IS_OPEN) WRITE(SWO%PRT_RUL_TS_END%IU,'(// A, // 16x, A,// 16x, A)') REPEAT('-', 150), '    FOR STRESS PERIOD '//NUM2STR(KPER,-6)//' AND TIME STEP '//NUM2STR(KSTP),'WITH A STARTING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP-1)%PRETTYPRINT('  ')//' AND ENDING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP)%PRETTYPRINT('  ')//'  ('//DATE_SP(KPER)%TS(KSTP-1)%STR_DYEAR()//' - '//DATE_SP(KPER)%TS(KSTP)%STR_DYEAR()//')'
          !
          CALL SWO%DEC_RUL_TS_END%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_TS_END%IU )
          !
          CALL VARIABLE_SET_RETURN_VALUES(SWO%DEC_VAR, SWO%DEC_VAR%PROP_RET, KPER, KSTP, ERROR)
          !
          IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
          !
          IF(SWO%PRT_RUL_TS_END%IS_OPEN) CALL SWO%PRT_RUL_TS_END%SIZE_CHECK()
    END IF
    !
    !------------------------------------------------------------------------------------------------------------
    !
    ! CHECK FOR CLOSEOUT FLAG AND RUN CUSTOM CLOSE OUT INFO
    !
    IF(HAS_CLOSEOUT) THEN
          !
          IF(SWO%PRT_RUL_CLOSEOUT%IS_OPEN) WRITE(SWO%PRT_RUL_CLOSEOUT%IU,'(// A, // 16x A,// 16x A)') REPEAT('-', 150), '    CLOSE OUT FOR STRESS PERIOD '//NUM2STR(KPER,-6)//' AND TIME STEP '//NUM2STR(KSTP),'THAT HAS AN ENDING CALENDAR DATE '//DATE_SP(KPER)%TS(KSTP)%PRETTYPRINT('  ')//' ('//DATE_SP(KPER)%TS(KSTP)%STR_DYEAR()//')'
          !
          CALL SWO%DEC_RUL_CLOSEOUT%RUN_S_LANG( SWO%DEC_VAR, SWO%PRT_RUL_CLOSEOUT%IU )
          !
          CALL VARIABLE_SET_RETURN_VALUES(SWO%DEC_VAR, SWO%DEC_VAR%PROP_RET, KPER, KSTP, ERROR)
          !
          IF(ERROR.NE.NL) CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG=ERROR)
          !
          IF(SWO%PRT_RUL_CLOSEOUT%IS_OPEN) CALL SWO%PRT_RUL_CLOSEOUT%SIZE_CHECK()
    END IF
    !
    !------------------------------------------------------------------------------------------------------------
    !
    DO IPROJ = 1, SWO%NPROJ
       IF (SWO%PROJ(IPROJ)%AllocClose == ONE) THEN
           !
           ! RESET YEAR-TO-DATE VARIABLES
           SWO%PROJ(IPROJ)%DIVERSION_YTD        = DZ
           SWO%PROJ(IPROJ)%DELIVERY_YTD         = DZ
           SWO%PROJ(IPROJ)%BYPASS_YTD           = DZ
           SWO%PROJ(IPROJ)%CHARGE_YTD           = DZ
           SWO%PROJ(IPROJ)%CREDIT_YTD           = DZ
           SWO%PROJ(IPROJ)%RELEASE_YTD          = DZ
           SWO%PROJ_PREV(IPROJ)%DIVERSION_YTD   = DZ
           SWO%PROJ_PREV(IPROJ)%DELIVERY_YTD    = DZ
           SWO%PROJ_PREV(IPROJ)%BYPASS_YTD      = DZ
           SWO%PROJ_PREV(IPROJ)%CHARGE_YTD      = DZ
           SWO%PROJ_PREV(IPROJ)%CREDIT_YTD      = DZ
           SWO%PROJ_PREV(IPROJ)%RELEASE_YTD     = DZ
           SWO%PROJ(IPROJ)%DELIVEFF_YTD    = SWO%PROJ_PREV(IPROJ)%DELIVEFF_YTD
           SWO%PROJ(IPROJ)%DIVRATIO_YTD    = SWO%PROJ_PREV(IPROJ)%DIVRATIO_YTD
           SWO%PROJ(IPROJ)%CHGRATIO_YTD    = SWO%PROJ_PREV(IPROJ)%CHGRATIO_YTD
           SWO%PROJ(IPROJ)%NETCHGRATIO_YTD = SWO%PROJ_PREV(IPROJ)%NETCHGRATIO_YTD

           DO CONCURRENT (IDIST = 1:SWO%NDIST, SWO%DIST(IDIST)%ProjID == IPROJ)
               SWO%DIST(IDIST)%DIVERSION_YTD      = DZ
               SWO%DIST(IDIST)%DELIVERY_YTD       = DZ
               SWO%DIST(IDIST)%BYPASS_YTD         = DZ
               SWO%DIST(IDIST)%CHARGE_YTD         = DZ
               SWO%DIST(IDIST)%CREDIT_YTD         = DZ
               SWO%DIST(IDIST)%BALANCE            = DZ !DIST(IDIST)%ALLOC_TOTAL  ! If start of new allocation year, balance = allocation (no charges yet!)
               SWO%DIST_PREV(IDIST)%DIVERSION_YTD = DZ
               SWO%DIST_PREV(IDIST)%DELIVERY_YTD  = DZ
               SWO%DIST_PREV(IDIST)%BYPASS_YTD    = DZ
               SWO%DIST_PREV(IDIST)%CHARGE_YTD    = DZ
               SWO%DIST_PREV(IDIST)%CREDIT_YTD    = DZ
               SWO%DIST_PREV(IDIST)%BALANCE       = DZ !DIST(IDIST)%ALLOC_TOTAL  ! If start of new allocation year, balance = allocation (no charges yet!)
               SWO%DIST(IDIST)%DELIVEFF_YTD  = SWO%DIST_PREV(IDIST)%DELIVEFF_YTD
           END DO !(IDIST)

           DO CONCURRENT (IUNIT = 1:SWO%NUNIT, SWO%UNIT(IUNIT)%ProjID == IPROJ)
               SWO%UNIT(IUNIT)%DIVERSION_YTD      = DZ
               SWO%UNIT(IUNIT)%DELIVERY_YTD       = DZ
               SWO%UNIT(IUNIT)%BYPASS_YTD         = DZ
               SWO%UNIT(IUNIT)%CHARGE_YTD         = DZ
               SWO%UNIT(IUNIT)%CREDIT_YTD         = DZ
               SWO%UNIT_PREV(IUNIT)%DIVERSION_YTD = DZ
               SWO%UNIT_PREV(IUNIT)%DELIVERY_YTD  = DZ
               SWO%UNIT_PREV(IUNIT)%BYPASS_YTD    = DZ
               SWO%UNIT_PREV(IUNIT)%CHARGE_YTD    = DZ
               SWO%UNIT_PREV(IUNIT)%CREDIT_YTD    = DZ
               SWO%UNIT(IUNIT)%DELIVEFF_YTD       = SWO%UNIT_PREV(IUNIT)%DELIVEFF
           END DO !(IUNIT)

           DO CONCURRENT (IFARM = 1:SWO%NFARM, SWO%FARM(IFARM)%ProjID == IPROJ)
               SWO%FARM(IFARM)%BALANCE           = DZ
               SWO%FARM(IFARM)%DELIVERY_YTD      = DZ
               SWO%FARM_PREV(IFARM)%BALANCE      = DZ
               SWO%FARM_PREV(IFARM)%DELIVERY_YTD = DZ
           END DO !(IFARM)

           DO CONCURRENT (IAUX = 1:SWO%NAUXDEM, SWO%AUXDEM(IAUX)%ProjID == IPROJ)
               SWO%AUXDEM(IAUX)%BALANCE           = DZ
               SWO%AUXDEM(IAUX)%DELIVERY_YTD      = DZ
               SWO%AUXDEM_PREV(IAUX)%BALANCE      = DZ
               SWO%AUXDEM_PREV(IAUX)%DELIVERY_YTD = DZ
           END DO !(IAUX)
           !
       END IF
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE SWO_OUTPUT_WRITE(SWO,IU,KPER,KSTP,KITER,XOPT)
!-----VERSION X 2014.06.30 SWOPS1BD
!   ******************************************************************
!   OVERVIEW:
!   Write intensive output for debugging, etc.
!   XOPT:
!   INIT --> Initialize files
!   FARM --> Farm data                                      (1)
!   AUXD --> Aux data                                       (2)
!   EXTD --> Ext data                                       (3)
!   UNIT --> Unit data                                      (4)
!   DIST --> District data                                  (5)
!   PROJ --> Project data                                   (6)
!   DIVS --> Diversion segment (service area)               (7)
!   SPLT --> Split segment (sub-area), including branches   (8)
!   STOR --> Storage data (reservoir mass balance)          (9)
!   XSFR --> SFR data @ end of FM iteration                 (10)
!   ALLC --> Allocation (based on start-of-step conditions) (11)
!   CNVG --> Convergence output                             (12)
!
!   ******************************************************************
!      SPECIFICATIONS:
!   ------------------------------------------------------------------
    !USE SWOPSMODULE
    USE GWFSFRMODULE, ONLY:ISTRM,STRM
!   ------------------------------------------------------------------
!      ARGUMENTS:
!   ------------------------------------------------------------------
    CLASS(SWO_DATA), INTENT(INOUT):: SWO
    CHARACTER(*),    INTENT(IN   ):: XOPT
    INTEGER,         INTENT(IN   ):: IU,KPER,KSTP,KITER
!
!   ------------------------------------------------------------------
!      LOCAL VARIABLES:
!   ------------------------------------------------------------------
    INTEGER::IPROJ,IDIST,IUNTT,IFARM,IAUX,ISTR, IDVS,ISPT,IBRC,IRES
    DOUBLE PRECISION:: DTMP1,DTMP2
!
!   ------------------------------------------------------------------

    SELECT CASE(XOPT)
    ! XOPT='FARM' --> Write farm values for current period/step/iteration
    CASE ("FARM")
      DO IFARM = 1,SWO%NFARM
                         WRITE(IU, 1001) KPER, KSTP, KITER, IFARM, SWO%FARM(IFARM)%UnitID, SWO%FARM(IFARM)%DistID, SWO%FARM(IFARM)%ProjID, SWO%FARM(IFARM)%DelSeg, SWO%FARM(IFARM)%DelRch, SWO%FARM(IFARM)%AreaTot, SWO%FARM(IFARM)%AreaIrr, SWO%FARM(IFARM)%TFDR, SWO%FARM(IFARM)%ALLOTMENT, SWO%FARM_PREV(IFARM)%DELIVERY_YTD, SWO%FARM(IFARM)%ALLOTMENT-SWO%FARM_PREV(IFARM)%DELIVERY_YTD, SWO%FARM(IFARM)%DELORDER, SWO%FARM(IFARM)%DELIVERY
      END DO

    ! XOPT='AUXD' --> Write aux values for current period/step/iteration
    CASE ('AUXD')
      DO IAUX = 1,SWO%NAUXDEM
                         WRITE(IU, 2001) KPER,KSTP,KITER,IAUX,SWO%AUXDEM(IAUX)%UnitID,SWO%AUXDEM(IAUX)%DistID,SWO%AUXDEM(IAUX)%ProjID,SWO%AUXDEM(IAUX)%AuxSeg,SWO%AUXDEM(IAUX)%AuxRch,SWO%AUXDEM(IAUX)%AREA,SWO%AUXDEM(IAUX)%DEMAND,SWO%AUXDEM(IAUX)%ALLOTMENT,SWO%AUXDEM(IAUX)%DELIVERY_YTD,SWO%AUXDEM(IAUX)%BALANCE,SWO%AUXDEM(IAUX)%DELORDER,SWO%AUXDEM(IAUX)%DELIVERY
      END DO !(IAUX)

    ! XOPT='UNIT' --> Write unit values for current period/step/iteration
    CASE ("UNIT")
      DO IUNTT = 1,SWO%NUNIT
                        WRITE(IU,4001) KPER,KSTP,KITER,IUNTT,SWO%UNIT(IUNTT)%DistID,SWO%UNIT(IUNTT)%ProjID,SWO%UNIT(IUNTT)%DivSeg,SWO%UNIT(IUNTT)%DivRch,SWO%UNIT(IUNTT)%ChgSeg,SWO%UNIT(IUNTT)%ChgRch,SWO%UNIT(IUNTT)%NBySeg,SWO%UNIT(IUNTT)%NCrdSeg,SWO%UNIT(IUNTT)%DivFactor,SWO%UNIT(IUNTT)%ChgFactor,SWO%UNIT(IUNTT)%AreaTot,SWO%UNIT(IUNTT)%AreaIrr,SWO%UNIT(IUNTT)%TFDR,SWO%UNIT(IUNTT)%DELORDER,SWO%UNIT_PREV(IUNTT)%DELIVEFF,SWO%UNIT(IUNTT)%DIVORDER,SWO%UNIT(IUNTT)%DIVERSION,SWO%UNIT(IUNTT)%DELIVERY,SWO%UNIT(IUNTT)%BYPASS,SWO%UNIT(IUNTT)%DELIVEFF,SWO%UNIT(IUNTT)%CHARGE,SWO%UNIT(IUNTT)%CREDIT,SWO%UNIT(IUNTT)%CHGRATIO,SWO%UNIT(IUNTT)%NETCHGRATIO,SWO%UNIT(IUNTT)%DIVERSION_YTD,SWO%UNIT(IUNTT)%DELIVERY_YTD,SWO%UNIT(IUNTT)%BYPASS_YTD,SWO%UNIT(IUNTT)%DELIVEFF_YTD,SWO%UNIT(IUNTT)%CHARGE_YTD,SWO%UNIT(IUNTT)%CREDIT_YTD,SWO%UNIT(IUNTT)%CHGRATIO_YTD,SWO%UNIT(IUNTT)%NETCHGRATIO_YTD
      END DO !(IUNTT)

    ! XOPT='DIST' --> Write district values for current period/step/iteration
    CASE ("DIST")
      DO IDIST = 1,SWO%NDIST
                        WRITE(IU,5001) KPER,KSTP,KITER,IDIST,SWO%DIST(IDIST)%ProjID,SWO%DIST(IDIST)%AreaTot,SWO%DIST(IDIST)%AreaIrr,SWO%DIST(IDIST)%ALLOC_ANN,SWO%DIST(IDIST)%ALLOC_CO,SWO%DIST(IDIST)%ALLOC_TOTAL,SWO%DIST(IDIST)%DELIVEFF_YTD,SWO%DIST(IDIST)%NETCHGRATIO_YTD,SWO%DIST(IDIST)%EQ_ALLOTMENT,SWO%DIST(IDIST)%BALANCE,SWO%DIST(IDIST)%TFDR,SWO%DIST(IDIST)%DELORDER,SWO%DIST_PREV(IDIST)%DELIVEFF,SWO%DIST(IDIST)%DIVORDER,SWO%DIST(IDIST)%DIVERSION,SWO%DIST(IDIST)%DELIVERY,SWO%DIST(IDIST)%BYPASS,SWO%DIST(IDIST)%DELIVEFF,SWO%DIST(IDIST)%CHARGE,SWO%DIST(IDIST)%CREDIT,SWO%DIST(IDIST)%CHGRATIO,SWO%DIST(IDIST)%NETCHGRATIO,SWO%DIST(IDIST)%DIVERSION_YTD,SWO%DIST(IDIST)%DELIVERY_YTD,SWO%DIST(IDIST)%BYPASS_YTD,SWO%DIST(IDIST)%DELIVEFF_YTD,SWO%DIST(IDIST)%CHARGE_YTD,SWO%DIST(IDIST)%CREDIT_YTD,SWO%DIST(IDIST)%CHGRATIO_YTD,SWO%DIST(IDIST)%NETCHGRATIO_YTD
      END DO !(IDIST)

    ! XOPT='PROJ' --> Write project values for current period/step/iteration
    CASE ("PROJ")
      DO IPROJ = 1,SWO%NPROJ
                        WRITE(IU,6001) KPER,KSTP,KITER,IPROJ,SWO%PROJ(IPROJ)%AreaTot,SWO%PROJ(IPROJ)%AreaIrr,SWO%PROJ(IPROJ)%ALLOCATION,SWO%PROJ(IPROJ)%TFDR,SWO%PROJ(IPROJ)%DELORDER,SWO%PROJ(IPROJ)%DIVORDER,SWO%PROJ(IPROJ)%RELEASE,SWO%PROJ(IPROJ)%RELEASE_YTD,SWO%PROJ(IPROJ)%DIVERSION,SWO%PROJ(IPROJ)%DIVERSION_YTD,SWO%PROJ(IPROJ)%DELIVERY,SWO%PROJ(IPROJ)%DELIVERY_YTD,SWO%PROJ(IPROJ)%DELIVEFF,SWO%PROJ(IPROJ)%DELIVEFF_YTD,SWO%PROJ(IPROJ)%DIVRATIO,SWO%PROJ(IPROJ)%DIVRATIO_YTD,SWO%PROJ(IPROJ)%BYPASS,SWO%PROJ(IPROJ)%BYPASS_YTD,SWO%PROJ(IPROJ)%CHARGE,SWO%PROJ(IPROJ)%CHARGE_YTD,SWO%PROJ(IPROJ)%CREDIT,SWO%PROJ(IPROJ)%CREDIT_YTD,SWO%PROJ(IPROJ)%CHGRATIO,SWO%PROJ(IPROJ)%CHGRATIO_YTD,SWO%PROJ(IPROJ)%NETCHGRATIO,SWO%PROJ(IPROJ)%NETCHGRATIO_YTD
      END DO !(IPROJ)

    ! XOPT='DIVS' --> Write diversion segment values for current period/step/iteration
    CASE ("DIVS")
      DO IDVS = 1,SWO%DIVCOUNT
                         WRITE(IU,7001) KPER,KSTP,KITER,IDVS,SWO%DIVSEG(IDVS)%DivSeg,SWO%DIVSEG(IDVS)%ProjID,SWO%DIVSEG(IDVS)%NUnit,SWO%DIVSEG(IDVS)%TFDR,SWO%DIVSEG(IDVS)%DELORDER,SWO%DIVSEG(IDVS)%DIVORDER,SWO%DIVSEG(IDVS)%DIVERSION,SWO%DIVSEG(IDVS)%DELIVERY,SWO%DIVSEG(IDVS)%DELIVEFF,SWO%DIVSEG(IDVS)%DSEEP,SWO%DIVSEG(IDVS)%DFLOW_IN,SWO%DIVSEG(IDVS)%DFLOW_RT,SWO%DIVSEG(IDVS)%UP_DIVORDER,SWO%DIVSEG(IDVS)%UP_DSEEP,SWO%DIVSEG(IDVS)%UP_DFLOW_IN,SWO%DIVSEG(IDVS)%UP_DFLOW_RT
      END DO !(IDVS)

    ! XOPT='SPLT' --> Write split segment values for current period/step/iteration
    CASE ("SPLT")
      DO ISPT = 1,SWO%SPTCOUNT
          WRITE(IU,8101) KPER,KSTP,KITER,SWO%SPTSEG(ISPT)%DivID,SWO%SPTSEG(ISPT)%DivSeg,SWO%SPTSEG(ISPT)%SplitID,SWO%SPTSEG(ISPT)%SplitSeg,SWO%SEGINFO(SWO%SPTSEG(ISPT)%SplitSeg)%SegType,SWO%SPTSEG(ISPT)%TFDR,SWO%SPTSEG(ISPT)%DELORDER,SWO%SPTSEG(ISPT)%DIVORDER,SWO%SPTSEG(ISPT)%DIVERSION,SWO%SPTSEG(ISPT)%DELIVERY,SWO%SPTSEG(ISPT)%DELIVEFF,SWO%SPTSEG(ISPT)%DSEEP,SWO%SPTSEG(ISPT)%DFLOW_IN,SWO%SPTSEG(ISPT)%DFLOW_RT,'SPT'
        DO IBRC = 1,SWO%SPTSEG(ISPT)%NBRANCH
          WRITE(IU,8101) KPER,KSTP,KITER,SWO%SPTSEG(ISPT)%DivID,SWO%SPTSEG(ISPT)%DivSeg,SWO%SPTSEG(ISPT)%SplitID,SWO%SPTSEG(ISPT)%BrcSeg(IBRC),SWO%SPTSEG(ISPT)%BrcType(IBRC),SWO%SPTSEG(ISPT)%BrcTFDR(IBRC),SWO%SPTSEG(ISPT)%BrcDELORDER(IBRC),SWO%SPTSEG(ISPT)%BrcDIVORDER(IBRC),SWO%SPTSEG(ISPT)%BrcDIVERSION(IBRC),SWO%SPTSEG(ISPT)%BrcDELIVERY(IBRC),SWO%SPTSEG(ISPT)%BrcDELIVEFF(IBRC),SWO%SPTSEG(ISPT)%BrcDSEEP(IBRC),SWO%SPTSEG(ISPT)%BrcDFLOW_IN(IBRC),SWO%SPTSEG(ISPT)%BrcDFLOW_RT(IBRC),'BRC'
        END DO !(IBRC)
      END DO !(ISPT)

    ! XOPT='STOR' --> Write storage values for current period/step/iteration
    CASE ("STOR")
      DO  IPROJ = 1,SWO%NPROJ
        DO IRES = 1,SWO%NRES_BAL(IPROJ)
          DTMP1 = (SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_PREV    + SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA) * HALF
          DTMP2 = (SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV - SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_MIN)
          WRITE(IU,9001)KPER,KSTP,KITER,IPROJ,IRES,SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_RELSEG,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV,SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_PREV,SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL,SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW,SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP,SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP,SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA,DTMP1,SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP*DTMP1,SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP*DTMP1,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_MIN,DTMP2,SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC,SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PMAX,SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_DMND,SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_PROJ,SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_FLOD,SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE
        END DO !(IRES)
      END DO !(IPROJ)

!! REMOVE FOR LONGER TESTING ...
    ! XOPT='XSFR' --> Write SFR values for current period/step/iteration
    CASE ("XSFR")  !NO MORE XOUT_SFR
      DO ISTR = 1,SWO%NSTRM
        WRITE(IU,1901)KPER,KSTP,KITER,ISTRM(4,ISTR),ISTRM(5,ISTR),ISTRM(2,ISTR),ISTRM(3,ISTR),ISTRM(1,ISTR),STRM(10,ISTR),STRM(11,ISTR),STRM(12,ISTR)+STRM(24,ISTR),STRM(14,ISTR),STRM(13,ISTR),STRM(9,ISTR)
      END DO

    ! XOPT='ALLC' --> Write allocation components at end of current step
!    CASE ("ALLC")
!
!      ! TODO -- Currently, XOUT_ALLC and XOUT_ALLF aren't set up for multiple projects...
!      !         Need to add dimension for project.
!
!      DO IPROJ = 1,NPROJ
!
!        ! before first allocation ...
!        IF (PROJ(IPROJ)%AllocInit.EQ.0) THEN
!          IF ((KPER.EQ.1).AND.(KSTP.EQ.1).AND.(KITER.EQ.1.OR.2)) THEN
!            WRITE(XCHKOUT(11),2900)
!          END IF !(KPER.EQ.1 .AND. KSTP.EQ.1 .AND. KITER.EQ.1)
!          WRITE(XCHKOUT(11),2901) KPER,KSTP
!        END IF !(AllocInit.EQ.0)
!
!        ! allocation close-out / init
!        IF ((PROJ(IPROJ)%AllocInit.NE.0) .AND. (PROJ(IPROJ)%AllocStart.EQ.1)) THEN
!          WRITE(XCHKOUT(11),2902)                                       ! Allocation close-out
!   +        KPER,KSTP,1, (XOUT_ALLF(I),I=1,29)
!          WRITE(XCHKOUT(11),'(//)')
!          WRITE(XCHKOUT(11),2900)                                       ! Header
!          WRITE(XCHKOUT(11),2902)                                       ! Initial allocation of new year (start of first step)
!   +        KPER,KSTP,0,
!   +        (XOUT_ALLC(I),I=1,29)
!        END IF !(AllocInit.NE.0 .AND. AllocStart.EQ.1)
!
!        ! regular allocation updates
!        IF ((PROJ(IPROJ)%AllocInit.NE.0)
!   +         .AND. (PROJ(IPROJ)%AllocStart.NE.1)) THEN
!          WRITE(XCHKOUT(11),2902)
!   +        KPER,KSTP,0,
!   +        (XOUT_ALLC(I),I=1,29)
!        END IF !(AllocInit.NE.0 .AND. AllocStart.NE.1)
!      END DO !(IPROJ)

    ! XOPT='CNVG' --> Write convergence checks...
    CASE ("CNVG")
      DO IPROJ = 1,SWO%NPROJ
        WRITE(IU,2801) KPER,KSTP,KITER,IPROJ,SWO%ABS_CNVG_CHK(1),SWO%REL_CNVG_CHK(1),SWO%ABS_CNVG_CHK(2),SWO%REL_CNVG_CHK(2),SWO%ABS_CNVG_CHK(3),SWO%REL_CNVG_CHK(3),SWO%ABS_CNVG_CHK(4),SWO%REL_CNVG_CHK(4),SWO%ABS_CNVG_CHK(5),SWO%REL_CNVG_CHK(5)
      END DO !(IPROJ)

    CASE DEFAULT
      ERROR STOP 'XCHECK ERROR, BAD KEYWORD PASSED INTO SUBROUTINE...THIS SHOULD NOT HAPPEN!'
    END SELECT


      ! Format @ Farms ... SWOUT(1)
1001  FORMAT(9(I8),8(F20.2))

      ! Format @ Aux   ... SWOUT(2)
2001  FORMAT(9(I8),7(F20.2))

      ! Format @ Ext   ... SWOUT(3)
!3001  FORMAT(7(I8),6(F20.2))

      ! Format @ Units ... SWOUT(4)
4001  FORMAT(12(I8),24(F20.2))

      ! Format @ Districts ... SWOUT(5)
5001  FORMAT(5(I8),29(F20.2))

      ! Format @ Projects ... SWOUT(6)
6001  FORMAT(4(I8),26(F20.2))

      ! Format @ DivSegs ... SWOUT(7)
7001  FORMAT(7(I8),5(F20.2),1(F20.10),7(F20.2))
!      ! Format @ DivSegs ... SWOUT(7)

      ! Format @ SplitSegs ... SWOUT(8)
8101  FORMAT(8(I8),5(F20.2),1(F20.10),3(F20.2),A20)

      ! Format @ Storage ... SWOUT(9)
9001  FORMAT(6(I8),4(F20.2),2(F20.8),12(F20.2))

      ! Format @ SFR ... SWOUT(10)
1901  FORMAT(8(I6),6(F20.2))

      ! Format @ ALLC/ALLF ... SWOUT(11)
! 2901 FORMAT(2(I6),'    NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA','                  NA',
!     +       '                  NA' )
!2902  FORMAT(3(I6),29(F20.2))

      ! Format @ CNVG ... XCHKOUT(12)
2801  FORMAT(4(I6),F10.6,24(F20.4))
      !
  END SUBROUTINE
  !
END MODULE



!      ASSOCIATE (TSTART=>SWO%TSTART, TSTOP=>SWO%TSTOP, YRSTART=>SWO%YRSTART, YRSTOP=>SWO%YRSTOP, FRSTART=>SWO%FRSTART, FRSTOP=>SWO%FRSTOP)
!        IF( DATE_SP(1)%TS(0)%DATE.EQ.'NO_DATE') THEN
!            TSTOP = REALTIM
!            IF (TSTOP.GE.DZ) THEN
!               IF (ITMUNI.EQ.0 .OR. ITMUNI.EQ.4) THEN                         !DAYS
!                   TSTART = TSTOP - DELT*0.00273791D0
!               ELSE IF (ITMUNI.EQ.1) THEN                                     !SECONDS
!                   TSTART = TSTOP - DELT*3.16888D-8
!               ELSE IF (ITMUNI.EQ.2) THEN                                     !MINUTES
!                   TSTART = TSTOP - DELT*1.90133D-6
!               ELSE IF (ITMUNI.EQ.3) THEN                                     !HOURS
!                   TSTART = TSTOP - DELT*0.00011408D0
!               ELSE                                                           !YEARS
!                   TSTART = TSTOP - DELT
!               END IF
!            ELSE
!                CALL STOP_ERROR( OUTPUT=SWO%IOUT, MSG= 'SWO TIME CALCULATION ERROR: YOU MUST SPECIFY EITHER REALTIME OR DATETIME IN DIS PACKAGE TO MAKE SWO DATE AWARE.')
!            END IF
!        ELSE
!            DATE_START = DATE_SP(KPER)%TS(KSTP-1)
!            DATE_END   = DATE_SP(KPER)%TS(KSTP  )
!            TSTART = DATE_SP(KPER)%TS(KSTP-ONE)%DYEAR
!            TSTOP  = DATE_SP(KPER)%TS(KSTP    )%DYEAR
!        END IF
!      !
!      ! Break TSTART/TSTOP into YRSTART/FRSTART and YRSTOP/FRSTOP
!      !
!      FRSTART =  YEAR_FRACTION(TSTART)  !DMOD(TSTART,UNO)
!      !
!      YR      = INT(TSTART)
!      YRSTART = DBLE(YR) !TSTART - FRSTART
!      SWO%IS_LEAP_START = ISLEAPYEAR(YR)
!      !
!      FRSTOP = YEAR_FRACTION(TSTOP)  !DMOD(TSTOP,UNO)
!      YR     = INT(TSTOP)
!      YRSTOP = DBLE(YR) !TSTOP - FRSTOP
!      SWO%IS_LEAP_END = ISLEAPYEAR(YR)
!      !
!      IF(.NOT. SWO%LEAP_YEAR) THEN
!                                  SWO%IS_LEAP_START = FALSE
!                                  SWO%IS_LEAP_END   = FALSE
!      END IF
!      !
!      ! ****************************************************************
!      ! Determine if timestep contains start/end of water year ...
!      ! ****************************************************************
!
!      ! Case where TSTART/TSTOP have same year (so TSTART<TSTOP)
!    I     = -1
!    J     = -1
!    IPROJ = -1
!    IRES  = -1
!    ISEG  = -1
!    IRCH  = -1
!    IDVS  = -1
!    ISPT  = -1
!    IBRC  = -1
!    IRES  = -1
!    JRES  = -1
!    IPROJ = -1
!    IDIST = -1
!    IUNIT = -1
!    IFARM = -1
!    IAUX  = -1
!
!      DO IPROJ = 1,SWO%NPROJ
!        IF( SWO%PROJ(IPROJ)%AllocDate%DATE.NE.'NO_DATE') THEN
!            IF(DATE_START%YEAR .NE. SWO%PROJ(IPROJ)%AllocDate%YEAR) THEN
!                I=SWO%PROJ(IPROJ)%AllocDate%DAY
!                J=SWO%PROJ(IPROJ)%AllocDate%MONTH
!                CALL SWO%PROJ(IPROJ)%AllocDate%INIT(I,J,DATE_START%YEAR)  !UPDATE DATE TO INCLUDE CURRENT YEAR, BUT PRESEVER MONTH AND DAY
!            END IF
!            !
!            IF(DATE_START <= SWO%PROJ(IPROJ)%AllocDate .AND. SWO%PROJ(IPROJ)%AllocDate < DATE_END) THEN
!                SWO%PROJ(IPROJ)%AllocStart = 1
!            ELSE
!                SWO%PROJ(IPROJ)%AllocStart = 0
!            END IF
!            !
!        ELSEIF (YRSTART.EQ.YRSTOP) THEN
!          IF ( (FRSTART.LE.SWO%PROJ(IPROJ)%AllocDateFrac) .AND. (SWO%PROJ(IPROJ)%AllocDateFrac.LT.FRSTOP) ) THEN
!            SWO%PROJ(IPROJ)%AllocStart = 1
!          ELSE
!            SWO%PROJ(IPROJ)%AllocStart = 0
!          END IF
!
!      ! Case where TSTART/TSTOP do not have the same year...
!        ELSE IF (YRSTART.LT.YRSTOP) THEN
!          IF ( SWO%PROJ(IPROJ)%AllocDateFrac.LE.FRSTART ) THEN
!              IF ( (TSTART.LE.(YRSTOP+SWO%PROJ(IPROJ)%AllocDateFrac)) .AND. ((YRSTOP+SWO%PROJ(IPROJ)%AllocDateFrac).LT.TSTOP) ) THEN
!                  SWO%PROJ(IPROJ)%AllocStart = 1
!              ELSE
!                  SWO%PROJ(IPROJ)%AllocStart = 0
!              END IF
!          ELSE IF (SWO%PROJ(IPROJ)%AllocDateFrac.GT.FRSTART ) THEN
!              IF ( (TSTART.LE.(YRSTART+SWO%PROJ(IPROJ)%AllocDateFrac)) .AND. ((YRSTART+SWO%PROJ(IPROJ)%AllocDateFrac).LT.TSTOP) ) THEN
!                  SWO%PROJ(IPROJ)%AllocStart = 1
!              ELSE
!                  SWO%PROJ(IPROJ)%AllocStart = 0
!              END IF
!          END IF !(SWO%PROJ(IPROJ)%AllocDate LE/GT FRSTART)
!
!        ! End cases
!        END IF !(YRSTART EQ/LT YRSTOP)
!
!      END DO !(IPROJ)
!      !
!      END ASSOCIATE












             !
             ! (10) Compute actual release ...
             !     ** Actual release is minimum of max release and demand-driven release...
             !        adjusted for flood release if needed
             !     ** First compute demand-driven release (DivReq/DivRatio...)
             !     ** Then compute storage at end of period at demand-driven release
             !     ** Then check for flood release
             !
!-----------------------------------------------------------------------------------------------------------------------------------------
!      DO IPROJ = 1,NPROJ
!        DO IRES = 1,SWO%NRES_BAL(IPROJ)
!          !
!          IDX_RES = IDX_RES + ONE
!          !
!          ! Set values for local variables
!          STORAGE_SOS  = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_PREV        ! STORAGE_PREV = end of previous step ... = start of current step.  [L3]
!          INFLOW       = SWO%RESDAT(IPROJ)%RESBAL(IRES)%INFLOW              ! INFLOW       during current step (updated @ AD routine!)          [L3]
!          PRCP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%PRCP                ! PRCP         during current step (updated @ AD routine!)          [L]
!          EVAP         = SWO%RESDAT(IPROJ)%RESBAL(IRES)%EVAP                ! EVAP         during current step (updated @ AD routine!)          [L]
!          RELEASE_SPEC = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RELEASE_SPEC        ! RELEASE_SPEC during current step (updated @ AD routine!)          [L3]
!          STORAGE_NPRJ = SWO%RESDAT(IPROJ)%RESBAL(IRES)%STORAGE_NPRJ        ! STORAGE_NPRJ during current step (updated @ AD routine!)
!          POOLFLG      = SWO%RESDAT(IPROJ)%RESBAL(IRES)%RESBAL_POOLFLG
!          !
!          IF (POOLFLG.EQ.1) THEN
!
!            ! loop over split reservoirs ...
!            AREA_SOS = DZ                                             ! AREA_SOS = reservoir area at start of current step      [L2]
!            AREA_END = DZ                                             ! AREA_END = reservoir area if only NPRJ water in storage [L2]
!            DO JRES = 1,SWO%NRES_SPT(IPROJ)
!              ! Area @ start of step
!              DTMP1 = DZ; DTMP2 = DZ
!              CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,STORAGE_SOS,DTMP1,SWO%IS_LEAP_END)
!              CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)
!              AREA_SOS = AREA_SOS + DTMP2
!              ! Area @ non-project storage only
!              DTMP1 = DZ; DTMP2 = DZ
!              CALL FRAC_POOL2SPLIT(SWO,IPROJ,JRES,SWO%FRSTOP,STORAGE_NPRJ,DTMP1,SWO%IS_LEAP_END)
!              CALL ACAP_STOR2AREA(SWO,IPROJ,JRES,DTMP1,DTMP2, ERROR)
!              AREA_END = AREA_END + DTMP2
!            END DO !(JRES)
!
!          ELSE
!            ! compute area for mass balance reservoir
!            CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE_SOS,AREA_SOS, ERROR)
!            !
!            CALL ACAP_STOR2AREA(SWO,IPROJ,IRES,STORAGE_NPRJ,AREA_END, ERROR)
!            !
!          END IF !(POOLFLG.EQ.1)
!
!          ! save AREA_END @ non-project storage only ...
!          SWO%RESDAT(IPROJ)%RESBAL(IRES)%AREA_DPL = AREA_END
!
!          ! Compute max storage
!          IF (POOLFLG.EQ.1) THEN
!            STORAGE_MAX     = DZ
!            DO CONCURRENT (JRES = 1:SWO%NRES_SPT(IPROJ))
!              IMAX        = SWO%RESDAT(IPROJ)%ACAP(JRES)%ACAP_COUNT
!              STORAGE_MAX = STORAGE_MAX + SWO%RESDAT(IPROJ)%ACAP(JRES)%ACAP_STORAGE(IMAX)
!            END DO !(JRES)
!          ELSE
!            IMAX          = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_COUNT
!            STORAGE_MAX   = SWO%RESDAT(IPROJ)%ACAP(IRES)%ACAP_STORAGE(IMAX)
!          END IF !(POOLFLG)
!          !
!          ! (8) Compute maximum possible release ...
!          !     (release all project water in storage at start of step,
!          !      plus inflow and precip during step, minus evap during step)
!          !     ** Use current non-project release, non-project storage...
!          !     ** Use area @ non-project storage as final area...
!          AREA_AVG = (AREA_SOS+AREA_END) * HALF
!          RELEASE_MAX   = STORAGE_SOS     &
!                        - STORAGE_NPRJ    & ! exclude non-project storage ...
!                        - RELEASE_SPEC    & ! exclude non-project release ...
!                        + INFLOW          &
!                        + PRCP*AREA_AVG   &
!                        - EVAP*AREA_AVG
!          !
!          IF(RELEASE_MAX > SWO%MAX_RELEASE(IDX_RES)) RELEASE_MAX = SWO%MAX_RELEASE(IDX_RES)
!          !
!          ! Set release to zero if diversion order is less than 1 AF (43560 ft3)
!          IF (SWO%PROJ(IPROJ)%DIVORDER.LE.SWO%MIN_PROJ_ORDER(IPROJ)) THEN
!            RELEASE_DMND = DZ
!
!          ELSEIF ( KITER.LE.2 ) THEN
!                RELEASE_DMND = SWO%PROJ(IPROJ)%DIVORDER
!          ELSE  !KITER.GT.2
!            !
!            ! Loop over diversion segments ...
!            RELEASE_DMND = DZ
!            DO IDVS = ONE,SWO%DIVCOUNT
!              !
!              ! get diversion segment, its IUPSEG, and it's NUPSEG
!              ISEG        = SWO%DIVSEG(IDVS)%DivSeg
!              JSEG        = SWO%SEGDATA(ISEG)%IUPSEG
!              NUPSEG_IDVS = SWO%UPTREE_NAT(JSEG)%NTREE + ONE
!              !
!              ! zero gain/loss and inflow components ...
!              SWO%DIVSEG(IDVS)%UP_DIVORDER = DZ
!              SWO%DIVSEG(IDVS)%UP_DSEEP    = DZ
!              SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DZ
!              SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DZ
!              !
!              ! sum diversion orders at and upstream of IDVS
!              DDIVO   = SWO%DIVSEG(IDVS)%DIVORDER * SWO%DELT_INV !/ DELT                    ! Convert order to rate!
!              DO CONCURRENT (JDVS = 1:SWO%DIVCOUNT, IDVS.NE.JDVS)  !THIS ASSUMES ALL DIVERSIONS ARE ON SAME RIVER AND FED FROM SAME RESERVOIR
!                KSEG        = SWO%DIVSEG(JDVS)%DivSeg
!                TMPSEG      = SWO%SEGDATA(KSEG)%IUPSEG
!                NUPSEG_JDVS = SWO%UPTREE_NAT(TMPSEG)%NTREE + ONE
!                IF (NUPSEG_JDVS.LT.NUPSEG_IDVS) THEN
!                  DTMP  = SWO%DIVSEG(JDVS)%DIVORDER * SWO%DELT_INV !/ DELT                  ! Convert order to rate
!                  DDIVO = DDIVO + DTMP
!                END IF !(NUPSEG_JDVS.LT.NUPSEG_IDVS)
!              END DO !(JDVS)
!              SWO%DIVSEG(IDVS)%UP_DIVORDER = DDIVO * DDELT
!              !
!              ! sum seepage over upstream river network ...
!              ! ... divseg's iupseg ...
!              DSEEP    = DZ
!              IRCH     = SWO%SEGRCH_IN(JSEG)                                ! reach index @ rch 1 of seg
!              JRCH     = SWO%SEGRCH_OUT(JSEG)                               ! reach index @ last reach of seg
!              DO CONCURRENT (ISTR  = IRCH:JRCH)
!                DSEEP  = DSEEP - STRM(11,ISTR)                          ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so GAIN is POSITIVE
!              END DO !(ISTR)
!              ! ... uptree @ iupseg ...
!              NTREE    = SWO%UPTREE_NAT(JSEG)%NTREE
!              DO CONCURRENT (ITREE = 1:NTREE)
!                KSEG   = SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
!                IRCH   = SWO%SEGRCH_IN(KSEG)
!                JRCH   = SWO%SEGRCH_OUT(KSEG)
!                DO CONCURRENT (ISTR = IRCH:JRCH)
!                  DSEEP = DSEEP - STRM(11,ISTR)                         ! NOTE: SFR specifies seepage LOSS as POSITIVE ... reverse here so LOSS is NEGATIVE
!                END DO !(ISTR)
!              END DO !(ITREE)
!              SWO%DIVSEG(IDVS)%UP_DSEEP = DSEEP * DDELT
!              !
!              ! sum specified inflows to upstream river network ...
!              DFLOW_IN = DZ
!              ! ... divseg ...
!              IF (IDIVAR(1,ISEG) == Z .AND. SWO%SEGDATA(ISEG)%RRFLAG.NE.ONE ) DFLOW_IN = DFLOW_IN + SEG(2,ISEG)
!              ! ... divseg's iupseg ...
!              IF (IDIVAR(1,JSEG) == Z .AND. SWO%SEGDATA(JSEG)%RRFLAG.NE.ONE ) DFLOW_IN = DFLOW_IN + SEG(2,JSEG) ! if NOT a diversion segment ... i.e., if IUPSEG=0  add FLOW value to inflows (if not diversion segment, FLOW is prescribed trib inflow)
!              ! ... uptree @ iupseg ...
!              NTREE    = SWO%UPTREE_NAT(JSEG)%NTREE
!              DO CONCURRENT (ITREE = 1:NTREE)
!                KSEG   = SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
!                IF ( IDIVAR(1,KSEG) == Z .AND. SWO%SEGDATA(KSEG)%RRFLAG.NE.ONE )  DFLOW_IN = DFLOW_IN + SEG(2,KSEG)
!              END DO !(ITREE)
!              SWO%DIVSEG(IDVS)%UP_DFLOW_IN = DFLOW_IN * DDELT
!              !
!              ! sum routed inflows to upstream river network ...
!              DFLOW_RT = DZ
!              ! ... divseg ...
!              NLIST    = SWO%UPSEG(ISEG)%NLIST
!              DO CONCURRENT (ILIST = 1:NLIST)
!                KSEG   = SWO%UPSEG(ISEG)%SEGLIST(ILIST)
!                JRCH   = SWO%SEGRCH_OUT(KSEG)
!                IF ( SWO%SEGDATA(KSEG)%OUTSEG.EQ.ISEG  .AND.  &             ! routed inflow to divseg (ISEG) ...
!                     SWO%SEGINFO(KSEG)%SegID.NE.JSEG ) THEN                 ! EXCLUDING inflow from divseg's iupseg
!                  DFLOW_RT = DFLOW_RT + STRM(9,JRCH)
!                END IF !(SegID.NE.JSEG / OUTSEG.EQ.ISEG)
!              END DO !(ILIST)
!              ! ... divseg's iupseg ...
!              NLIST    = SWO%UPSEG(JSEG)%NLIST
!              DO CONCURRENT (ILIST = 1:NLIST)
!                KSEG   = SWO%UPSEG(JSEG)%SEGLIST(ILIST)
!                JRCH   = SWO%SEGRCH_OUT(KSEG)
!                IF ( SWO%SEGDATA(KSEG)%OUTSEG.EQ.JSEG .AND. (SWO%SEGINFO(KSEG)%SegType.NE.1)) THEN  ! routed inflow to divseg's iupseg (JSEG) ... EXCLUDING inflow from upstream river segments!
!                      DFLOW_RT = DFLOW_RT + STRM(9,JRCH)
!                END IF !(SegType.NE.1 / OUTSEG.EQ.JSEG)
!              END DO !(ILIST)
!              ! ... uptree @ iupseg ...
!              NTREE    = SWO%UPTREE_NAT(JSEG)%NTREE
!              DO CONCURRENT (ITREE = 1:NTREE)
!                KSEG   = SWO%UPTREE_NAT(JSEG)%SEGTREE(ITREE)
!                NLIST  = SWO%UPSEG(KSEG)%NLIST
!                DO CONCURRENT (ILIST = 1:NLIST)
!                  LSEG   = SWO%UPSEG(KSEG)%SEGLIST(ILIST)
!                  JRCH   = SWO%SEGRCH_OUT(LSEG)
!                  IF ( SWO%SEGDATA(LSEG)%OUTSEG.EQ.KSEG .AND. SWO%SEGINFO(LSEG)%SegType.NE.1 ) THEN ! routed inflow to segment in uptree @ iupseg (JSEG) ... EXCLUDING inflow from upstream river segments!
!                    DFLOW_RT = DFLOW_RT + STRM(9,JRCH)
!                  END IF !(SegType.NE.1 / OUTSEG.EQ.KSEG)
!                END DO !(ILIST)
!              END DO !(ITREE)
!              SWO%DIVSEG(IDVS)%UP_DFLOW_RT = DFLOW_RT * DDELT
!              !
!              ! Compute release required to meet demand at IDVS
!              ! accounting for upstream diversions, inflows, and seepage
!              ! (sum of diversion orders @ IDVS and upstream ...
!              !  minus seepage gains      (positive DSEEP=gain, negative DSEEP=loss)
!              !  minus inflows above IDVS (positive DFLOW=inflow, negative DFLOW=loss))
!              DTMP  = (DDIVO - DSEEP - DFLOW_IN - DFLOW_RT)*DDELT
!              !
!              ! Reset release demand to highest value to meet all diversion orders
!              IF(RELEASE_DMND < DTMP) RELEASE_DMND = DTMP
!              !RELEASE_DMND = MAX( RELEASE_DMND,DTMP )
!              !
!            END DO !(IDVS)
!
!          END IF !(DIVORDER.GT.0).AND.(KPER.LE/GT.2)
!          !
!          IF(RELEASE_DMND<DZ) = DZ
!
!          ! (10) Compute actual release ...
!          !     ** Actual release is minimum of max release and demand-driven release...
!          !        adjusted for flood release if needed
!          !     ** First compute demand-driven release (DivReq/DivRatio...)
!          !     ** Then compute storage at end of period at demand-driven release
!          !     ** Then check for flood release
!          RELEASE_EST   = MIN( RELEASE_MAX,RELEASE_DMND )
!          IF (RELEASE_EST < DZ) RELEASE_EST = DZ
!----------------------------------------------------------------------------------------------------------------
