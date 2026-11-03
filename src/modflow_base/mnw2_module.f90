MODULE GWFMNW2MODULE
  USE TABLEFILE_INTERFACE,             ONLY: TABFILETYPECHARIDX
  USE LINE_FEEDER,                     ONLY: LINE_FEED
  USE BUDGET_GROUP_INTERFACE,          ONLY: BUDGET_GROUP
  USE WARNING_TYPE_INSTRUCTION,        ONLY: WARNING_TYPE
  USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
  !
  IMPLICIT NONE
  !
  PRIVATE:: TABFILETYPECHARIDX, LINE_FEED, BUDGET_GROUP, WARNING_TYPE
  !
  DOUBLE PRECISION, PARAMETER:: MNW2_NULL = 1.0D30   ! null value of MNW2 arrays: set a value to it, then test "== MNW2_NULL" to see if it has been set
  !
  TYPE MNW_PPC_CACHE   ! partial penetration corrections of one well; components allocated on the first lookup of a well with PPFLAG > 0
     INTEGER,          DIMENSION(:),   ALLOCATABLE:: FLAG   ! (NNODES) -1 = nothing stored, else the ISOLNFLAG returned by PPC
     DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE:: KEY    ! (6,NNODES) BB, HKR, HKZ, RW, ZPD, ZPL the correction was computed for
     DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: DHP    ! (NNODES) drawdown per unit extraction rate
  END TYPE
  !
  LOGICAL, SAVE, POINTER:: HAS_NWT, SOLV_BY_HCOF, HAS_NONE_LOSSTYPE
  INTEGER, SAVE, POINTER:: IOUT
  INTEGER, SAVE, POINTER:: NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT
  INTEGER, SAVE, POINTER:: NODTOT,INTTOT,NTOTNOD
  INTEGER, SAVE, POINTER:: SOLV_FLG, HCOF_ONLY_ITER, HCOF_RHS_FLIP
  INTEGER, SAVE, POINTER:: PP_CON_ITER_LIM
  INTEGER, SAVE, POINTER:: MXNODE               !Holds largest number of NODES
  INTEGER, SAVE, POINTER:: LEN_WELLID           !LEN_TRIM WELLID or 12, which ever is bigger
  !
  DOUBLE PRECISION, SAVE, POINTER:: SMALL
  DOUBLE PRECISION, SAVE, POINTER:: MIN_PERF, NONE_PHIRAMP
  !
  CHARACTER(20), SAVE, DIMENSION(:), CONTIGUOUS, POINTER:: WELLID
  CHARACTER(16), SAVE, DIMENSION(:), CONTIGUOUS, POINTER:: MNWAUX
  !
  LOGICAL, SAVE,                           POINTER:: HAS_FMP_WELLS
  LOGICAL, SAVE, DIMENSION(:), CONTIGUOUS, POINTER:: MNW_FMP_LINK
  INTEGER, SAVE, DIMENSION(:), CONTIGUOUS, POINTER:: GRPNODE
  !
  INTEGER,          SAVE, DIMENSION(:,:),   CONTIGUOUS, POINTER:: LIMQ
  DOUBLE PRECISION, SAVE, DIMENSION(:,:),   CONTIGUOUS, POINTER:: MNW2
  DOUBLE PRECISION, SAVE, DIMENSION(:,:),   CONTIGUOUS, POINTER:: MNWNOD
  DOUBLE PRECISION, SAVE, DIMENSION(:,:),   CONTIGUOUS, POINTER:: MNWINT
  DOUBLE PRECISION, SAVE, DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CapTable
  DOUBLE PRECISION, SAVE, DIMENSION(:),     CONTIGUOUS, POINTER:: QDES_OLD
  TYPE(MNW_PPC_CACHE), SAVE, DIMENSION(:),  CONTIGUOUS, POINTER:: PPC_CACHE
  !
  TYPE(TABFILETYPECHARIDX),  SAVE, POINTER:: MNW2TABFILE
  TYPE(LINE_FEED),           SAVE, POINTER:: MNW_FEED
  TYPE(BUDGET_GROUP),        SAVE, POINTER:: MNW2BUD
  TYPE(WARNING_TYPE),        SAVE, POINTER:: MNW2_WARN
  TYPE(WARNING_TYPE),        SAVE, POINTER:: SMNW2COND_WARN
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2_Q_NODE
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2_Q_INOUT
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2_NODE
  !
  TYPE GWFMNWTYPE
     LOGICAL, POINTER:: HAS_NWT, SOLV_BY_HCOF, HAS_NONE_LOSSTYPE
     INTEGER, POINTER:: IOUT
     INTEGER, POINTER:: NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT
     INTEGER, POINTER:: NODTOT,INTTOT,NTOTNOD
     INTEGER, POINTER:: SOLV_FLG, HCOF_ONLY_ITER, HCOF_RHS_FLIP
     INTEGER, POINTER:: PP_CON_ITER_LIM
     INTEGER, POINTER:: MXNODE               !Holds largest number of NODES
     INTEGER, POINTER:: LEN_WELLID           !LEN_TRIM WELLID or 12, which ever is bigger
     !
     DOUBLE PRECISION, POINTER:: SMALL
     DOUBLE PRECISION, POINTER:: MIN_PERF, NONE_PHIRAMP
     !
     CHARACTER(20), DIMENSION(:), CONTIGUOUS, POINTER:: WELLID
     CHARACTER(16), DIMENSION(:), CONTIGUOUS, POINTER:: MNWAUX
     !
     LOGICAL,                           POINTER:: HAS_FMP_WELLS
     LOGICAL, DIMENSION(:), CONTIGUOUS, POINTER:: MNW_FMP_LINK
     INTEGER, DIMENSION(:), CONTIGUOUS, POINTER:: GRPNODE
     !
     INTEGER,          DIMENSION(:,:),   CONTIGUOUS, POINTER:: LIMQ
     DOUBLE PRECISION, DIMENSION(:,:),   CONTIGUOUS, POINTER:: MNW2
     DOUBLE PRECISION, DIMENSION(:,:),   CONTIGUOUS, POINTER:: MNWNOD
     DOUBLE PRECISION, DIMENSION(:,:),   CONTIGUOUS, POINTER:: MNWINT
     DOUBLE PRECISION, DIMENSION(:,:,:), CONTIGUOUS, POINTER:: CapTable
     DOUBLE PRECISION, DIMENSION(:),     CONTIGUOUS, POINTER:: QDES_OLD
     TYPE(MNW_PPC_CACHE), DIMENSION(:),  CONTIGUOUS, POINTER:: PPC_CACHE
     !
     TYPE(TABFILETYPECHARIDX),  POINTER:: MNW2TABFILE
     TYPE(LINE_FEED),           POINTER:: MNW_FEED
     TYPE(BUDGET_GROUP),        POINTER:: MNW2BUD
     TYPE(WARNING_TYPE),        POINTER:: MNW2_WARN
     TYPE(WARNING_TYPE),        POINTER:: SMNW2COND_WARN
     TYPE(GENERIC_OUTPUT_FILE), POINTER:: PRNT_MNW2
     TYPE(GENERIC_OUTPUT_FILE), POINTER:: PRNT_MNW2_Q_NODE
     TYPE(GENERIC_OUTPUT_FILE), POINTER:: PRNT_MNW2_Q_INOUT
     TYPE(GENERIC_OUTPUT_FILE), POINTER:: PRNT_MNW2_NODE
  END TYPE
  !
  TYPE(GWFMNWTYPE), SAVE:: GWFMNWDAT(10)
  !
  CONTAINS
  !------------------------------------------------------------------
  !     MNW2(NMNWVL, MNWMAX): one column per well, NMNWVL = 30 + NAUX rows
  !      Row #  = Description
  !------------------------------------------------------------------
  !         1   = IACTIV: 0 inactive this stress period, 1 active, -1 disabled for the simulation
  !         2   = NNODES as read: > 0 number of nodes (dataset 2d by cell), < 0 minus the number of open intervals (dataset 2d by elevation)
  !         3   = LOSSTYPE: 0 NONE, 1 THIEM, 2 SKIN, 3 GENERAL, 4 SPECIFYCWC
  !         4   = NODNUM: index in MNWNOD of the first node of the well (the nodes of a well are consecutive)
  !         5   = Qdes: desired rate of the stress period (< 0 extraction); with PUMPCAP the first negative Qdes is the table maximum
  !         6   = Qlimit: 0 no constraint, > 0 Hlim/QCUT constant (dataset 2f), < 0 read every stress period (dataset 4b)
  !         7   = Hlim: limiting well head (minimum for extraction, maximum for injection)
  !         8   = QCUT: 0 no shut-off, > 0 Qfrcmn/Qfrcmx are rates, < 0 they are fractions of Qdes
  !         9   = Qfrcmn: rate or fraction below which the well is shut off
  !        10   = Qfrcmx: rate or fraction above which a shut-off well restarts
  !        11   = PUMPLOC: 0 pump at the top node, > 0 cell given in dataset 2e, < 0 elevation Zpump given in dataset 2e
  !        12   = Cprime: concentration of injected water (GWT link only)
  !        13   = INTNUM: index in MNWINT of the first open interval of the well (NNODES < 0)
  !        14   = PUMPLAY: layer of the pump intake (dataset 2e, or the node that holds Zpump)
  !        15   = PUMPROW: row of the pump intake
  !        16   = PUMPCOL: column of the pump intake
  !        17   = hwell: water level in the well (the cell head for LOSSTYPE NONE)
  !        18   = qnet: rate applied in the current time step (Qdes, or the limited, restart or capacity rate; 0 while shut off)
  !        19   = PPFLAG: > 0 apply the partial penetration correction (reset to 0 for NONE, SPECIFYCWC, or when every node is fully penetrating)
  !        20   = QCUT state of the time step: 0 none, 1 restarted this step, -1 shut off this step, -2 shut off in an earlier step (may restart later this step); set by SMNW2SEEP/SMNW2NONE, cleared by AD
  !        21   = HWflag: 1 if the nodes are not all in one row and column (nonvertical well), else 0
  !        22   = PUMPCAP: number of entries of the head-capacity table (0 none)
  !        23   = Hlift: reference elevation of the discharge point of the pump
  !        24   = CapMult: multiplier of the head-capacity curve (<= 0 ignore the table, 1 use as read)
  !        25   = CapFlag: 1 while the capacity lookup is still done in this time step, 0 once the rate is locked in
  !        26   = last capacity rate looked up (used to limit the change between iterations to 25 percent)
  !        27   = CapFlag2: 0 not capacity-limited, 1 capacity-limited (AD carries the rate over), 2 capacity overrides the Hlim constraint
  !        28   = HWtol: well-head change below which the capacity rate is locked in for the time step
  !        29   = qactCap: capacity rate after the 25 percent and restart limits (applied through mnw2(18) when CapFlag2 > 0)
  !        30   = Qpot stored at a QCUT state change: the restart rate, or 0 when shut off (copied to mnw2(18))
  !        31+  = auxiliary variables (NAUX of them, names in MNWAUX)
  !------------------------------------------------------------------
  !
  ! MNWNOD(33, NODTOT): one column per node
  !
  !------------------------------------------------------------------
  !         1   = Layer
  !         2   = Row
  !         3   = Column
  !         4   = Qact: flow between the cell and the node (< 0 out of the cell)
  !         5   = Rw: well radius
  !         6   = Rskin: skin radius
  !         7   = Kskin: skin hydraulic conductivity
  !         8   = B: linear well-loss coefficient (GENERAL)
  !         9   = C: nonlinear well-loss coefficient (GENERAL)
  !        10   = P: exponent of the nonlinear term (GENERAL)
  !        11   = CWC: specified cell-to-well conductance (SPECIFYCWC)
  !        12   = first open interval (index in MNWINT) that the node represents (NNODES < 0)
  !        13   = last open interval that the node represents
  !        14   = CWC: cell-to-well conductance of the node as computed (0 for a deactivated node)
  !        15   = head at the node: hwell, the cell bottom for a seepage face, HDRY for a dry or inactive node, "inf" before SMNW2SEEP has set it
  !        16   = Txx: transmissivity of the cell in the row direction
  !        17   = Tyy: transmissivity of the cell in the column direction
  !        18   = dhp: additional drawdown per unit extraction rate due to partial penetration (added in series to 1/CWC)
  !        19   = PP: penetration fraction of the node given in dataset 2d (set to 1 when >= 0.99)
  !        20   = Ztop of the composite screen used for partial penetration; MNW2_NULL (1d30) until the screen position is computed
  !        21   = Zbotm of the composite screen, set with 20
  !        22   = node active flag: 1 active, 0 deactivated for the time step (inactive cell, CWC < 0, screen above the water table, failed correction)
  !        23   = nonvertical well: length of the borehole in the cell before the node (first segment)
  !        24   = nonvertical well: length of the borehole in the cell after the node (second segment)
  !        25   = nonvertical well: length of the borehole between this node's cell and the next node's cell (in no cell)
  !        26   = nonvertical well: elevation of the borehole at the node
  !        27   = QBH: flow in the borehole between this node and the previous one (saved at the upper face)
  !        28   = nonvertical well: omega, tilt angle of the borehole segment
  !        29   = nonvertical well: theta, map angle of the borehole segment
  !        30   = nonvertical well: CWC of the first segment (row 23)
  !        31   = nonvertical well: CWC of the second segment (row 24)
  !        32   = LOSSTYPE NONE constraint state (SMNW2NONE): 0 free, 1 pinned at Hlim, 2 restart probe; +10 when the state changed in this iteration
  !        33   = Kz: vertical hydraulic conductivity of the cell, for the partial penetration correction
  !
  !------------------------------------------------------------------
  !
  ! MNWINT(11, NODTOT): one column per open interval (NNODES < 0)
  !
  !------------------------------------------------------------------
  !         1   = Ztop: top elevation of the open interval
  !         2   = Zbotm: bottom elevation of the open interval
  !         3   = Row
  !         4   = Column
  !         5   = Rw: well radius
  !         6   = Rskin: skin radius
  !         7   = Kskin: skin hydraulic conductivity
  !         8   = B: linear well-loss coefficient (GENERAL)
  !         9   = C: nonlinear well-loss coefficient (GENERAL)
  !        10   = P: exponent of the nonlinear term (GENERAL)
  !        11   = CWC: specified cell-to-well conductance (SPECIFYCWC)
  !
  !------------------------------------------------------------------
  !
  SUBROUTINE GWF2MNW27DA(IGRID)
     ! Deallocate MNW MEMORY
     INTEGER, INTENT(IN):: IGRID
     !
     DEALLOCATE( GWFMNWDAT(IGRID)%HAS_NWT           )
     DEALLOCATE( GWFMNWDAT(IGRID)%SOLV_BY_HCOF      )
     DEALLOCATE( GWFMNWDAT(IGRID)%HAS_NONE_LOSSTYPE )
     DEALLOCATE( GWFMNWDAT(IGRID)%IOUT              )
     DEALLOCATE( GWFMNWDAT(IGRID)%NMNW2             )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNWMAX            )
     DEALLOCATE( GWFMNWDAT(IGRID)%NMNWVL            )
     DEALLOCATE( GWFMNWDAT(IGRID)%IWL2CB            )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNWPRNT           )
     DEALLOCATE( GWFMNWDAT(IGRID)%NODTOT            )
     DEALLOCATE( GWFMNWDAT(IGRID)%INTTOT            )
     DEALLOCATE( GWFMNWDAT(IGRID)%NTOTNOD           )
     DEALLOCATE( GWFMNWDAT(IGRID)%SOLV_FLG          )
     DEALLOCATE( GWFMNWDAT(IGRID)%HCOF_ONLY_ITER    )
     DEALLOCATE( GWFMNWDAT(IGRID)%HCOF_RHS_FLIP     )
     DEALLOCATE( GWFMNWDAT(IGRID)%PP_CON_ITER_LIM   )
     DEALLOCATE( GWFMNWDAT(IGRID)%MXNODE            )
     DEALLOCATE( GWFMNWDAT(IGRID)%LEN_WELLID        )
     DEALLOCATE( GWFMNWDAT(IGRID)%SMALL             )
     DEALLOCATE( GWFMNWDAT(IGRID)%MIN_PERF          )
     DEALLOCATE( GWFMNWDAT(IGRID)%NONE_PHIRAMP      )
     DEALLOCATE( GWFMNWDAT(IGRID)%WELLID            )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNWAUX            )
     DEALLOCATE( GWFMNWDAT(IGRID)%HAS_FMP_WELLS     )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW_FMP_LINK      )
     DEALLOCATE( GWFMNWDAT(IGRID)%GRPNODE           )
     DEALLOCATE( GWFMNWDAT(IGRID)%LIMQ              )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2              )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNWNOD            )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNWINT            )
     DEALLOCATE( GWFMNWDAT(IGRID)%CapTable          )
     DEALLOCATE( GWFMNWDAT(IGRID)%QDES_OLD          )
     DEALLOCATE( GWFMNWDAT(IGRID)%PPC_CACHE         )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2TABFILE       )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW_FEED          )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2BUD           )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2_WARN         )
     DEALLOCATE( GWFMNWDAT(IGRID)%SMNW2COND_WARN    )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2         )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2_Q_NODE  )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2_Q_INOUT )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2_NODE    )
     !
     ! NULLIFY THE LOCAL POINTERS
     IF(IGRID.EQ.1)THEN
       HAS_NWT           => NULL()
       SOLV_BY_HCOF      => NULL()
       HAS_NONE_LOSSTYPE => NULL()
       IOUT              => NULL()
       NMNW2             => NULL()
       MNWMAX            => NULL()
       NMNWVL            => NULL()
       IWL2CB            => NULL()
       MNWPRNT           => NULL()
       NODTOT            => NULL()
       INTTOT            => NULL()
       NTOTNOD           => NULL()
       SOLV_FLG          => NULL()
       HCOF_ONLY_ITER    => NULL()
       HCOF_RHS_FLIP     => NULL()
       PP_CON_ITER_LIM   => NULL()
       MXNODE            => NULL()
       LEN_WELLID        => NULL()
       SMALL             => NULL()
       MIN_PERF          => NULL()
       NONE_PHIRAMP      => NULL()
       WELLID            => NULL()
       MNWAUX            => NULL()
       HAS_FMP_WELLS     => NULL()
       MNW_FMP_LINK      => NULL()
       GRPNODE           => NULL()
       LIMQ              => NULL()
       MNW2              => NULL()
       MNWNOD            => NULL()
       MNWINT            => NULL()
       CapTable          => NULL()
       QDES_OLD          => NULL()
       PPC_CACHE         => NULL()
       MNW2TABFILE       => NULL()
       MNW_FEED          => NULL()
       MNW2BUD           => NULL()
       MNW2_WARN         => NULL()
       SMNW2COND_WARN    => NULL()
       PRNT_MNW2         => NULL()
       PRNT_MNW2_Q_NODE  => NULL()
       PRNT_MNW2_Q_INOUT => NULL()
       PRNT_MNW2_NODE    => NULL()
     END IF
     ! 
     !!! ! GFORTRAN compiler error work-around for pointer data type FINAL statement
     !!! MNW_FEED=>GWFMNWDAT(IGRID)%MNW_FEED
     !!! GWFMNWDAT(IGRID)%MNW_FEED=>NULL()
     !!! DEALLOCATE(MNW_FEED)
     !!! MNW_FEED=>NULL()
     !!! !
     !!! MNW2BUD=>GWFMNWDAT(IGRID)%MNW2BUD
     !!! GWFMNWDAT(IGRID)%MNW2BUD=>NULL()
     !!! DEALLOCATE(MNW2BUD)
     !!! MNW2BUD=>NULL()
     !!! !
     !!! MNW2_WARN=>GWFMNWDAT(IGRID)%MNW2_WARN
     !!! GWFMNWDAT(IGRID)%MNW2_WARN=>NULL()
     !!! DEALLOCATE(MNW2_WARN)
     !!! MNW2_WARN=>NULL()
     !
  END SUBROUTINE
  !
  SUBROUTINE SGWF2MNW2PNT(IGRID)
     ! Change MNW data to a different grid.
     INTEGER, INTENT(IN):: IGRID
     !
     HAS_NWT           => GWFMNWDAT(IGRID)%HAS_NWT
     SOLV_BY_HCOF      => GWFMNWDAT(IGRID)%SOLV_BY_HCOF
     HAS_NONE_LOSSTYPE => GWFMNWDAT(IGRID)%HAS_NONE_LOSSTYPE
     IOUT              => GWFMNWDAT(IGRID)%IOUT              
     NMNW2             => GWFMNWDAT(IGRID)%NMNW2             
     MNWMAX            => GWFMNWDAT(IGRID)%MNWMAX            
     NMNWVL            => GWFMNWDAT(IGRID)%NMNWVL            
     IWL2CB            => GWFMNWDAT(IGRID)%IWL2CB            
     MNWPRNT           => GWFMNWDAT(IGRID)%MNWPRNT           
     NODTOT            => GWFMNWDAT(IGRID)%NODTOT            
     INTTOT            => GWFMNWDAT(IGRID)%INTTOT            
     NTOTNOD           => GWFMNWDAT(IGRID)%NTOTNOD           
     SOLV_FLG          => GWFMNWDAT(IGRID)%SOLV_FLG          
     HCOF_ONLY_ITER    => GWFMNWDAT(IGRID)%HCOF_ONLY_ITER    
     HCOF_RHS_FLIP     => GWFMNWDAT(IGRID)%HCOF_RHS_FLIP     
     PP_CON_ITER_LIM   => GWFMNWDAT(IGRID)%PP_CON_ITER_LIM   
     MXNODE            => GWFMNWDAT(IGRID)%MXNODE            
     LEN_WELLID        => GWFMNWDAT(IGRID)%LEN_WELLID        
     SMALL             => GWFMNWDAT(IGRID)%SMALL             
     MIN_PERF          => GWFMNWDAT(IGRID)%MIN_PERF          
     NONE_PHIRAMP      => GWFMNWDAT(IGRID)%NONE_PHIRAMP
     WELLID            => GWFMNWDAT(IGRID)%WELLID            
     MNWAUX            => GWFMNWDAT(IGRID)%MNWAUX            
     HAS_FMP_WELLS     => GWFMNWDAT(IGRID)%HAS_FMP_WELLS     
     MNW_FMP_LINK      => GWFMNWDAT(IGRID)%MNW_FMP_LINK      
     GRPNODE           => GWFMNWDAT(IGRID)%GRPNODE           
     LIMQ              => GWFMNWDAT(IGRID)%LIMQ              
     MNW2              => GWFMNWDAT(IGRID)%MNW2              
     MNWNOD            => GWFMNWDAT(IGRID)%MNWNOD            
     MNWINT            => GWFMNWDAT(IGRID)%MNWINT            
     CapTable          => GWFMNWDAT(IGRID)%CapTable          
     QDES_OLD          => GWFMNWDAT(IGRID)%QDES_OLD          
     PPC_CACHE         => GWFMNWDAT(IGRID)%PPC_CACHE
     MNW2TABFILE       => GWFMNWDAT(IGRID)%MNW2TABFILE       
     MNW_FEED          => GWFMNWDAT(IGRID)%MNW_FEED          
     MNW2BUD           => GWFMNWDAT(IGRID)%MNW2BUD           
     MNW2_WARN         => GWFMNWDAT(IGRID)%MNW2_WARN         
     SMNW2COND_WARN    => GWFMNWDAT(IGRID)%SMNW2COND_WARN
     PRNT_MNW2         => GWFMNWDAT(IGRID)%PRNT_MNW2         
     PRNT_MNW2_Q_NODE  => GWFMNWDAT(IGRID)%PRNT_MNW2_Q_NODE  
     PRNT_MNW2_Q_INOUT => GWFMNWDAT(IGRID)%PRNT_MNW2_Q_INOUT 
     PRNT_MNW2_NODE    => GWFMNWDAT(IGRID)%PRNT_MNW2_NODE    
     !
  END SUBROUTINE
  !
  SUBROUTINE SGWF2MNW2PSV(IGRID)
     !  Save MNW2 data for a grid.
     INTEGER, INTENT(IN):: IGRID
     !
     GWFMNWDAT(IGRID)%HAS_NWT           => HAS_NWT           
     GWFMNWDAT(IGRID)%SOLV_BY_HCOF      => SOLV_BY_HCOF
     GWFMNWDAT(IGRID)%HAS_NONE_LOSSTYPE => HAS_NONE_LOSSTYPE
     GWFMNWDAT(IGRID)%IOUT              => IOUT              
     GWFMNWDAT(IGRID)%NMNW2             => NMNW2             
     GWFMNWDAT(IGRID)%MNWMAX            => MNWMAX            
     GWFMNWDAT(IGRID)%NMNWVL            => NMNWVL            
     GWFMNWDAT(IGRID)%IWL2CB            => IWL2CB            
     GWFMNWDAT(IGRID)%MNWPRNT           => MNWPRNT           
     GWFMNWDAT(IGRID)%NODTOT            => NODTOT            
     GWFMNWDAT(IGRID)%INTTOT            => INTTOT            
     GWFMNWDAT(IGRID)%NTOTNOD           => NTOTNOD           
     GWFMNWDAT(IGRID)%SOLV_FLG          => SOLV_FLG          
     GWFMNWDAT(IGRID)%HCOF_ONLY_ITER    => HCOF_ONLY_ITER    
     GWFMNWDAT(IGRID)%HCOF_RHS_FLIP     => HCOF_RHS_FLIP     
     GWFMNWDAT(IGRID)%PP_CON_ITER_LIM   => PP_CON_ITER_LIM   
     GWFMNWDAT(IGRID)%MXNODE            => MXNODE            
     GWFMNWDAT(IGRID)%LEN_WELLID        => LEN_WELLID        
     GWFMNWDAT(IGRID)%SMALL             => SMALL             
     GWFMNWDAT(IGRID)%MIN_PERF          => MIN_PERF          
     GWFMNWDAT(IGRID)%NONE_PHIRAMP      => NONE_PHIRAMP
     GWFMNWDAT(IGRID)%WELLID            => WELLID            
     GWFMNWDAT(IGRID)%MNWAUX            => MNWAUX            
     GWFMNWDAT(IGRID)%HAS_FMP_WELLS     => HAS_FMP_WELLS     
     GWFMNWDAT(IGRID)%MNW_FMP_LINK      => MNW_FMP_LINK      
     GWFMNWDAT(IGRID)%GRPNODE           => GRPNODE           
     GWFMNWDAT(IGRID)%LIMQ              => LIMQ              
     GWFMNWDAT(IGRID)%MNW2              => MNW2              
     GWFMNWDAT(IGRID)%MNWNOD            => MNWNOD            
     GWFMNWDAT(IGRID)%MNWINT            => MNWINT            
     GWFMNWDAT(IGRID)%CapTable          => CapTable          
     GWFMNWDAT(IGRID)%QDES_OLD          => QDES_OLD          
     GWFMNWDAT(IGRID)%PPC_CACHE         => PPC_CACHE
     GWFMNWDAT(IGRID)%MNW2TABFILE       => MNW2TABFILE       
     GWFMNWDAT(IGRID)%MNW_FEED          => MNW_FEED          
     GWFMNWDAT(IGRID)%MNW2BUD           => MNW2BUD           
     GWFMNWDAT(IGRID)%MNW2_WARN         => MNW2_WARN         
     GWFMNWDAT(IGRID)%SMNW2COND_WARN    => SMNW2COND_WARN
     GWFMNWDAT(IGRID)%PRNT_MNW2         => PRNT_MNW2         
     GWFMNWDAT(IGRID)%PRNT_MNW2_Q_NODE  => PRNT_MNW2_Q_NODE  
     GWFMNWDAT(IGRID)%PRNT_MNW2_Q_INOUT => PRNT_MNW2_Q_INOUT 
     GWFMNWDAT(IGRID)%PRNT_MNW2_NODE    => PRNT_MNW2_NODE    
     !
  END SUBROUTINE
  !
END MODULE GWFMNW2MODULE
