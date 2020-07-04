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
  LOGICAL, SAVE, POINTER:: HAS_NWT
  INTEGER, SAVE, POINTER:: IOUT
  INTEGER, SAVE, POINTER:: NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT
  INTEGER, SAVE, POINTER:: NODTOT,INTTOT,NTOTNOD
  INTEGER, SAVE, POINTER:: SOLV_FLG, HCOF_ONLY_ITER, HCOF_RHS_FLIP
  INTEGER, SAVE, POINTER:: PP_CON_ITER_LIM
  INTEGER, SAVE, POINTER:: MXNODE               !Holds largest number of NODES
  INTEGER, SAVE, POINTER:: LEN_WELLID           !LEN_TRIM WELLID or 12, which ever is bigger
  !
  DOUBLE PRECISION, SAVE, POINTER:: SMALL
  DOUBLE PRECISION, SAVE, POINTER:: MIN_PERF
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
  !
  TYPE(TABFILETYPECHARIDX),  SAVE, POINTER:: MNW2TABFILE
  TYPE(LINE_FEED),           SAVE, POINTER:: MNW_FEED
  TYPE(BUDGET_GROUP),        SAVE, POINTER:: MNW2BUD
  TYPE(WARNING_TYPE),        SAVE, POINTER:: MNW2_WARN
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2_Q_NODE
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2_Q_INOUT
  TYPE(GENERIC_OUTPUT_FILE), SAVE, POINTER:: PRNT_MNW2_NODE
  !
  TYPE GWFMNWTYPE
     LOGICAL, POINTER:: HAS_NWT
     INTEGER, POINTER:: IOUT
     INTEGER, POINTER:: NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT
     INTEGER, POINTER:: NODTOT,INTTOT,NTOTNOD
     INTEGER, POINTER:: SOLV_FLG, HCOF_ONLY_ITER, HCOF_RHS_FLIP
     INTEGER, POINTER:: PP_CON_ITER_LIM
     INTEGER, POINTER:: MXNODE               !Holds largest number of NODES
     INTEGER, POINTER:: LEN_WELLID           !LEN_TRIM WELLID or 12, which ever is bigger
     !
     DOUBLE PRECISION, POINTER:: SMALL
     DOUBLE PRECISION, POINTER:: MIN_PERF
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
     !
     TYPE(TABFILETYPECHARIDX),  POINTER:: MNW2TABFILE
     TYPE(LINE_FEED),           POINTER:: MNW_FEED
     TYPE(BUDGET_GROUP),        POINTER:: MNW2BUD
     TYPE(WARNING_TYPE),        POINTER:: MNW2_WARN
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
  !     The 11 rows of the MNW2 array store:
  !      Row #  = Description
  !------------------------------------------------------------------
  !         1   = IACTIV (0: inactive; 1: active, -1 disabled for simulation)
  !         2   = NNODES (number of nodes in this well)
  !         3   = LOSSTYPE (0: none; 1:THIEM; 2: SKIN; 3: GENERAL; 4:SPEC. COND)
  !         4   = NODNUM (number, in node list (MNWNOD), of first node of well)
  !         5   = QDES (desired flow rate for this stress period)
  !         6   = QLIMIT (pumpage constraint flag, QLIMIT>0 turns on constraint)
  !         7   = HLIM (limiting water level for pumpage constraint)
  !         8   = QCUT (pump cutoff flag: QCUT>0 limit by rate, QCUT<0 limit by 
  !                     fraction of QDES)
  !         9   = Qfrcmn (minimum rate for well to remain active)
  !        10   = Qfrcmx (maximum rate to reactivate)
  !        11   = PUMPLOC   WAS PREVIOUSLY DEFINED AS Cprime
  !        12   = Cprime (concentration of inflow)
  ! seb added definitions
  !        13   = First interval number in interval list that is the total intervals so far + 1. This is used to access the information in the interval array (MNWINT)
  !        14   = PUMPLAY from Data Set 2E
  !        15   = PUMPROW from Data Set 2E
  !        16   = PUMPCOL from Data Set 2E
  !        17   = Hwel WATER LEVEL IN WELL
  !        18   = qnet NET PUMPING RATE FOR WELL (USUALLY SET BY MNW2(30,:) WHICH HOLDS Qpot)
  !        19   = PPFLAG flag.  PPFLAG>0 means calculate partial penetration effect.
  !        20   = Flag to perform this q-limit check is mnw2(20,iw).  If > 0, limit has been met. Do not enforce q-limit oscillation-stopper until the 3rd iteration; hardwire=0. 
  !        21   = HWflag (horizontal well flag) to 0 as default; can be set to 1 (true) if NNODES>0 and any R,C is different than first; =1 Indicates non-vertical well
  !        22   = PUMPCAP: Pump capacity restraints
  !        23   = HLIFT: reference head (or elevation) corresponding to the discharge point for the well
  !        24   = CapMult
  !        25   = CapFlag 0 to discontinue doing capacity checks for current time step
  !        26   = Last Q that was looked up in table for next iteration
  !        27   = CapFlag2: If =1 then Q has been constrained, required for AD routine
  !        28   = HWTOL: Minimum absolute value of change in the computed water level in the well allowed between successive iterations
  !        29   = TEMP Storage for MISC values
  !        30   = USED TO STORE Qpot (Potential Pumping Rate) WHICH IS TRANSFERED TO MNW2(18,:)
  !        31   = CONC IN WELL
  !        31+  = USED FOR STORAGE OF AUXILIARY VARIABLES
  !------------------------------------------------------------------
  !
  ! MNWNOD
  !
  !------------------------------------------------------------------
  !         1   = Layer  
  !         2   = Row    
  !         3   = Column 
  !         4   = Qact - Flow rate in or out of node
  !         5   = Radius
  !         6   = Skin Radius
  !         7   = Skin hydraulic conductivity
  !         8   = B factor for Node
  !         9   = C factor for Node
  !        10   = P loss factor associated with C factor (Ploss)
  !        11   = Specified Conductance of Well when LOSSTYPE = SPECIFIEDCWC
  !        12   = First Interval that node represents
  !        13   = Last  Interval that node represents
  !        14   = Conductance that is calcualted for node
  !        15   = Set to Hwell, Hdry, Cell Bottom, or INF depending if Node is Wet, Dry, Seepage, or not yet calculated
  !        16   = Tranissivity in the X-direction
  !        17   = Tranissivity in the Y-direction
  !        18   = Drawdown correction due to partially penetrating well
  !        19   = Fraction of model cell for partial penetration flag when NOT using Ztop/Zbotm
  !        20   = Ztop  of composite screen used for partial penetration - set to 1d30 initially
  !        21   = Zbotm of composite screen used for partial penetration - set to 1d30 initially
  !        22   = Unknown use??? - seb taking over for 0 and 1 Flag to indicate if node is active or deactivated for time step
  !        23   = 
  !        24   = 
  !        25   = 
  !        26   = 
  !        27   = 
  !        28   = 
  !        29   = 
  !        30   = 
  !        31   = 
  !        32   = 
  !        33   = Vertical Hydraulic Concductivity of Model cell
  !        34   = Specific Storage of Model cell
  !
  !------------------------------------------------------------------
  !
  ! MNWINT
  !
  !------------------------------------------------------------------
  !         1   = Ztop
  !         2   = Zbotm
  !         3   = Row
  !         4   = Column
  !         5   = Radius
  !         6   = Skin Radius
  !         7   = Skin hydraulic conductivity
  !         8   = B factor for Node
  !         9   = C factor for Node
  !        10   = P loss factor associated with C factor (Ploss)
  !        11   = Specified Conductance of Well when LOSSTYPE = SPECIFIEDCWC
  !
  !------------------------------------------------------------------
  !
  SUBROUTINE GWF2MNW27DA(IGRID)
     ! Deallocate MNW MEMORY
     INTEGER, INTENT(IN):: IGRID
     !
     DEALLOCATE( GWFMNWDAT(IGRID)%HAS_NWT           )
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
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2TABFILE       )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW_FEED          )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2BUD           )
     DEALLOCATE( GWFMNWDAT(IGRID)%MNW2_WARN         )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2         )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2_Q_NODE  )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2_Q_INOUT )
     DEALLOCATE( GWFMNWDAT(IGRID)%PRNT_MNW2_NODE    )
     !
     ! NULLIFY THE LOCAL POINTERS
     IF(IGRID.EQ.1)THEN
       HAS_NWT           => NULL()
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
       MNW2TABFILE       => NULL()
       MNW_FEED          => NULL()
       MNW2BUD           => NULL()
       MNW2_WARN         => NULL()
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
     MNW2TABFILE       => GWFMNWDAT(IGRID)%MNW2TABFILE       
     MNW_FEED          => GWFMNWDAT(IGRID)%MNW_FEED          
     MNW2BUD           => GWFMNWDAT(IGRID)%MNW2BUD           
     MNW2_WARN         => GWFMNWDAT(IGRID)%MNW2_WARN         
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
     GWFMNWDAT(IGRID)%MNW2TABFILE       => MNW2TABFILE       
     GWFMNWDAT(IGRID)%MNW_FEED          => MNW_FEED          
     GWFMNWDAT(IGRID)%MNW2BUD           => MNW2BUD           
     GWFMNWDAT(IGRID)%MNW2_WARN         => MNW2_WARN         
     GWFMNWDAT(IGRID)%PRNT_MNW2         => PRNT_MNW2         
     GWFMNWDAT(IGRID)%PRNT_MNW2_Q_NODE  => PRNT_MNW2_Q_NODE  
     GWFMNWDAT(IGRID)%PRNT_MNW2_Q_INOUT => PRNT_MNW2_Q_INOUT 
     GWFMNWDAT(IGRID)%PRNT_MNW2_NODE    => PRNT_MNW2_NODE    
     !
  END SUBROUTINE
  !
END MODULE GWFMNW2MODULE