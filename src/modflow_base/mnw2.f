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
!        23   = Horizontal well define verticle distance
!        24   = Horizontal well define lengths with elevations
!        25   = Horizontal well define blank spaces in between
!        26   = Horizontal well z coords
!        27   = Flow between nodes--saved at upper face--in borehole (QBH)
!        28   = Horizontal well omega angle
!        29   = Horizontal well theta angle
!        30   = Horizontal well conductance 1
!        31   = Horizontal well conductance 2
!        32   = Unknown use??? - seb taking over for XYZ
!        33   = Vertical Hydraulic Concductivity of Model cell for Partial Penitration Calculation
!        34   = Specific Storage of Model cell                 for Partial Penitration Calculation
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
      SUBROUTINE GWF2MNW27AR(IN,IUNWT,IGRID)
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,        ONLY:LIST_OUT => IOUT, NLAY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                    CapTable,SMALL,NTOTNOD,WELLID,LIMQ,
     3               MNW2TABFILE, MNW_FEED,MNW2BUD, GRPNODE,IOUT,
     4               MNW_FMP_LINK, HAS_FMP_WELLS,SOLV_FLG,SOLV_BY_HCOF,
     5               HCOF_ONLY_ITER,HCOF_RHS_FLIP, QDES_OLD,
     6               HAS_NWT,MIN_PERF,PP_CON_ITER_LIM,MXNODE,LEN_WELLID,
     7               MNW2_WARN, SMNW2COND_WARN,
     8      PRNT_MNW2, PRNT_MNW2_Q_NODE,PRNT_MNW2_Q_INOUT,PRNT_MNW2_NODE
      
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PSV
      !
      USE TABLEFILE_INTERFACE,  ONLY: TABFILEPARSE,TABFILELINKS
      USE LINE_FEEDER,          ONLY: LINE_FEED                
      USE ERROR_INTERFACE,      ONLY: WARNING_MESSAGE
      USE FILE_IO_INTERFACE,    ONLY: READ_TO_DATA
      USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD_UP
      USE STRINGS,              ONLY: GET_INTEGER, GET_NUMBER, GET_WORD
      USE NUM2STR_INTERFACE,    ONLY: NUM2STR
      USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
      USE CONSTANTS,            ONLY: NL, TRUE, FALSE, BLNK, inf_I, 
     +                                NEG, Z, ONE, TWO, DZ
      IMPLICIT NONE
C
      INTEGER, INTENT(IN):: IN, IUNWT, IGRID
      INTEGER:: I, LLOC, ISTART, ISTOP
      INTEGER:: NAUX
      REAL:: R
      TYPE(GENERIC_BLOCK_READER):: BL
      CHARACTER(32):: KEY
      LOGICAL:: FOUND_BEGIN
      !CHARACTER(768):: LINE --NOTE THAT BL%LN IS ALREADY AVAILBLE, SO USE THAT SPACE
C     ------------------------------------------------------------------
C
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(NMNW2,MNWMAX,NTOTNOD,IWL2CB,MNWPRNT,NODTOT,INTTOT,SMALL,
     1 NMNWVL)
      ALLOCATE(MNW2BUD,IOUT,MNW2_WARN,SMNW2COND_WARN)
      ALLOCATE(SOLV_FLG,HCOF_ONLY_ITER,HCOF_RHS_FLIP,MIN_PERF)
      ALLOCATE(PRNT_MNW2, PRNT_MNW2_Q_NODE,PRNT_MNW2_Q_INOUT)
      ALLOCATE(PRNT_MNW2_NODE, SOLV_BY_HCOF)
      !
      ALLOCATE(HAS_NWT, PP_CON_ITER_LIM,MXNODE,LEN_WELLID)
      HAS_NWT = IUNWT.ne.Z
      PP_CON_ITER_LIM = 15
      MXNODE=Z
      LEN_WELLID = 12
      !
      IOUT     = LIST_OUT
      MIN_PERF = 0.01D0  !Default is to not allow perf if less than 1% of cell thickness
      SOLV_BY_HCOF = FALSE
      !
C
C2------IDENTIFY PACKAGE AND INITIALIZE NMNW2.
      WRITE(IOUT,1) IN
C---LFK--modify dates & version
    1 format(/,1x,'MNW2 -- MULTI-NODE WELL 2 PACKAGE, VERSION',
     +' OWHM.',/,4X,'INPUT READ FROM UNIT ',i3)
      NMNW2=0
      ntotnod=0
c-lfk-Dec 2012
      nodtot   = Z
      SOLV_FLG = Z
      HCOF_ONLY_ITER = inf_I
      HCOF_RHS_FLIP  = Z
C
C Allocate auxiliary variable space
      ALLOCATE (MNWAUX(20))
      NAUX   = Z
      MNWAUX = BLNK
C
C3------READ MAXIMUM NUMBER OF MNW2 WELLS, UNIT OR FLAG FOR
C3------CELL-BY-CELL FLOW TERMS, AND PRINT FLAG
      !CALL URDCOM(IN,IOUT,LINE)
      !
      !CALL BL%LOAD(IN,IOUT,LINE=LINE)
      CALL MNW2BUD%INIT('MNW2')
      !
      ALLOCATE(MNW_FEED)
      !
      CALL BL%LOAD(IN,IOUT,FOUND_BEGIN=FOUND_BEGIN, NO_BACKSPACE=TRUE)
      DO WHILE (FOUND_BEGIN)
        !
        IF(BL%NAME == 'BUDGET_GROUP'.OR.BL%NAME == 'BUDGET_GROUPS') THEN
               !
               IF(BL%NLINE > Z) CALL MNW2BUD%LOAD(BL)
               !
        ELSEIF(BL%NAME == 'LINEFEED') THEN
               !
               !ALLOCATE MNW_FEED VARIABLE AND OPTIONALLY READ IN FEED FILE LOCATIONS
               IF(BL%NLINE > Z) CALL MNW_FEED%INIT(BL)    !=>FEED_ALLOCATE(IN,IOUT,LINE)
               !
               !
        ELSEIF(BL%NAME == 'OPTION'.OR.BL%NAME == 'OPTIONS') THEN
        !
        WRITE(IOUT,'(/1X,A)') 'PROCESSING MNW2 OPTIONS'
        !
        CALL BL%START()
        DO I=1, BL%NLINE
        LLOC = ONE
        !CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
        !
        CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
        !
        IF(KEY == 'PRINT') THEN
          CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
          KEY = 'PRINT_'//KEY
        END IF
        !
        SELECT CASE (KEY)
        CASE('AUXILIARY','AUX')
         CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            MNWAUX(NAUX)=BL%LINE(ISTART:ISTOP)
            WRITE(IOUT,12) MNWAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY MNW2 VARIABLE: ',A)
         ELSE
           WRITE(IOUT,*)'MNW2 WARNING: TOO MANY AUX VARIABLES, MAX OF 5'
         END IF
        CASE('CELL_PERF_MIN_FRACTION')
                 CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +                            MIN_PERF,
     +   MSG='CELL_PERF_MIN_FRACTION FAILED TO LOAD THE MIN_FRAC')
        CASE('PP_CON_ITER_LIM') 
                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +                            PP_CON_ITER_LIM,
     +   MSG='PP_CON_ITER_LIM FAILED TO LOAD THE ITER_LIM')
                 IF(PP_CON_ITER_LIM < 5) PP_CON_ITER_LIM = 5
        CASE('RHS_ONLY','RHSONLY')
                 WRITE(IOUT,'(/A/)') 
     +         'RHS_ONLY: MNW2 PACKAGE WILL ONLY SOLVE USING RHS'
                 SOLV_FLG = 1
        CASE('HCOF_ONLY','HCOFONLY') 
                 WRITE(IOUT,'(/A/)') 
     +         'HCOF_ONLY: MNW2 PACKAGE WILL ONLY SOLVE USING HCOF'
                 SOLV_FLG = 2
                 
        CASE('ONLY_HCOF_AFTER') 
                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +                            HCOF_ONLY_ITER,
     +           MSG='ONLY_HCOF_AFTER FAILED TO LOAD THE ITER_START')
                 !
                 WRITE(IOUT,'(/4A/)') 'ONLY_HCOF_AFTER: MNW2 PACKAGE ',
     +          'WILL ONLY SOLVE USING HCOF AFTER ',
     +           NUM2STR(HCOF_ONLY_ITER), ' SOLVER ITERATIONS.'
                 
!        CASE('HCOF_RHS_FLIP')  !DOES NOT SEEM TO HELP
!                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
!     +                            HCOF_RHS_FLIP,
!     +           MSG='HCOF_RHS_FLIP FAILED TO LOAD THE ITER_FLIP')
!                 SOLV_FLG = 2
!                 !
!                 WRITE(IOUT,'(/4A/)') 'HCOF_RHS_FLIP: MNW2 PACKAGE ',
!     +          'WILL FLIP BETWEEN USING HCOF AND RHS EVERY ',
!     +          NUM2STR(HCOF_RHS_FLIP), ' SOLVER ITERATIONS.'
                 
        CASE('PRINT_WELL_PUMPING') 
            !
            CALL PRNT_MNW2%OPEN(BL%LINE,LLOC,IOUT,IN,
     +                                              SAVE_FNAME=TRUE)
            ! 
        CASE('PRINT_NODE_INFO') 
            !
            CALL PRNT_MNW2_NODE%OPEN(BL%LINE,LLOC,IOUT,IN,
     +                                 NO_BINARY=TRUE, SAVE_FNAME=TRUE)
            !      
        CASE('PRINT_WELL_NODE_FLOW') 
            !
            CALL PRNT_MNW2_Q_NODE%OPEN(BL%LINE,LLOC,IOUT,IN,
     +                                 NO_BINARY=TRUE, SAVE_FNAME=TRUE)
            !     
        CASE('PRINT_WELL_INOUT','PRINT_WELL_IN_OUT') 
            !
            CALL PRNT_MNW2_Q_INOUT%OPEN(BL%LINE,LLOC,IOUT,IN,
     +                                              SAVE_FNAME=TRUE)
            !
        CASE DEFAULT
            ! -- NO OPTIONS FOUND
            WRITE(IOUT,'(/2A,A/)')
     +             'MNW2 WARNING: FAILED TO IDENTIFY OPTION: ',
     +              BL%LINE(ISTART:ISTOP),
     +             'THIS OPTION IS IGNORED AND NOT APPLIED.'
        END SELECT
        CALL BL%NEXT()
        END DO
        !
        ELSE
              CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG=
     +        'MNW2 BLOCK NAME IS NOT RECOGNIZED. THE FOLLOWING BLOCK "'
     +        //BL%NAME//'" WILL BE SKIPPED AND NOT INCLUDED',
     +        INLINE=TRUE, CMD_PRINT=TRUE)
        END IF
        !
        CALL BL%LOAD(IN,IOUT,FOUND_BEGIN=FOUND_BEGIN,NO_BACKSPACE=TRUE)
      END DO
      !
      IF(MNW_FEED%NFEED < ONE) THEN
          WRITE(BL%IOUT,'(/ 2A /)') 
     +     ' NO LINEFEEDS ARE SPECIFIED FOR PACKAGE MNW2'
      END IF
      !
      IF( PRNT_MNW2%IU .ne. Z) THEN
         WRITE(IOUT,'(/3A/)') 
     +         'PRINT_WELL_INFO OPTION FOUND.',
     +         'EACH MNW2 PUMPING RATE WILL BE SAVED TO ',
     +          PRNT_MNW2%FNAME
         !
         IF(PRNT_MNW2%BINARY) THEN
          WRITE(IOUT,'(/2A,/A, /*(A))') 
     +    'PRINT_WELL_INFO IS SET TO ',
     +    'WRITTE TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE.',
     +    'EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',
     +    'DATE_START (19char), PER (int), STP (int), DELT (double), ',
     +    'WELLID (20char), ',
     +    'PUMPING_RATE_INI (double), PUMPING_RATE (double),  ',
     +    'HEAD_WELL (double)' 
        END IF
      END IF
      !
      IF( PRNT_MNW2_NODE%IU .ne. Z) THEN
         WRITE(IOUT,'(/3A/)') 
     +         'PRINT_NODE_INFO OPTION FOUND.',
     +    "EACH MNW2 WELL'S NODAL FLOW RATE AND HEAD WILL BE SAVED TO ",
     +          PRNT_MNW2_NODE%FNAME
      END IF
      !
      IF( PRNT_MNW2_Q_NODE%IU .ne. Z) THEN
         WRITE(IOUT,'(/3A/)') 
     +         'PRINT_WELL_FLOW OPTION FOUND.',
     +         "EACH MNW2 WELL'S NODAL FLOW RATE WILL BE SAVED TO ",
     +          PRNT_MNW2_Q_NODE%FNAME
      END IF
      !
      IF( PRNT_MNW2_Q_INOUT%IU .ne. Z) THEN
         WRITE(IOUT,'(/3A/)') 
     +         'PRINT_WELL_INOUT OPTION FOUND.',
     +         'EACH MNW2 PUMPING RATE WILL BE SAVED TO ',
     +          PRNT_MNW2_Q_INOUT%FNAME
         !
         IF(PRNT_MNW2_Q_INOUT%BINARY) THEN
          WRITE(IOUT,'(/2A,/A, /*(A))') 
     +    'PRINT_WELL_INOUT IS SET TO ',
     +    'WRITTE TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE.',
     +    'EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',
     +    'DATE_START (19char), PER (int), STP (int), DELT (double), ',
     +    'WELLID (20char), ',
     +    'PUMPING_RATE_INI (double), PUMPING_RATE (double), ',
     +    'RATE_IN (double),  RATE_OUT (double),  HEAD_WELL (double)' 
        END IF
      END IF
      !
      CALL MNW2BUD%RESET()
      ALLOCATE(GRPNODE(MNW2BUD%NGRP), SOURCE = Z)
      !
      !CALL READ_TO_DATA(BL%LN,IN,IOUT)
      !
      ALLOCATE(MNW2TABFILE)                                             !seb ADDED TABFILE SUPPORT FROM TABFILE_INTERFACE
      CALL TABFILEPARSE(IN,IOUT,BL%LN,MNW2TABFILE)
      CALL TABFILELINKS(IN,IOUT,BL%LN,MNW2TABFILE)
      !
      LLOC=1
      CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,2,MNWMAX,R,IOUT,IN)
c--LFK
      IF (MNWMAX < Z) THEN
           CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,2,NODTOT,R,IOUT,IN)
           MNWMAX = NEG * MNWMAX
      END IF
c--LFK
      CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,2,IWL2CB,R,IOUT,IN)
      CALL URWORD(BL%LN,LLOC,ISTART,ISTOP,2,MNWPRNT,R,IOUT,IN)
      !
      ! CHECK IF GLOBAL SHUTDOWN OF CBC IS IN EFFECT
      CALL CHECK_CBC_GLOBAL_UNIT(IWL2CB)
      !
      write(iout,3) NUM2STR(MNWMAX)
    3 format(1h ,'MAXIMUM OF ',A,' ACTIVE MULTI-NODE WELLS AT ONE TIME')
      write(iout,*) 
      if(IWL2CB > Z) write(iout,9) IWL2CB
    9 format(1x, 'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT', i3)
      if(IWL2CB < Z) write(iout,*) 'IWL2CB = ',IWL2CB
      if(IWL2CB < Z) write(iout,8)
    8 format(1x,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      write(iout,*) 'MNWPRNT = ',MNWPRNT
      !
      DO I=1, 10 
        CALL PARSE_WORD_UP(BL%LN,LLOC,ISTART,ISTOP)
        !
        SELECT CASE (BL%LN(ISTART:ISTOP))
        CASE('AUXILIARY','AUX')
         CALL PARSE_WORD_UP(BL%LN,LLOC,ISTART,ISTOP)
         IF(NAUX < 5) THEN
            NAUX=NAUX+1
            MNWAUX(NAUX)=BL%LN(ISTART:ISTOP)
            WRITE(IOUT,12) MNWAUX(NAUX)
!   12       FORMAT(1X,'AUXILIARY MNW2 VARIABLE: ',A)
         ELSE
           WRITE(IOUT,*)'MNW2 WARNING: TOO MANY AUX VARIABLES, MAX OF 5'
         END IF
        CASE('RHS_ONLY','RHSONLY')
                 WRITE(IOUT,'(/A/)') 
     +         'RHS_ONLY: MNW2 PACKAGE WILL ONLY SOLVE USING RHS'
                 SOLV_FLG = ONE
        CASE('HCOF_ONLY','HCOFONLY') 
                 WRITE(IOUT,'(/A/)') 
     +         'HCOF_ONLY: MNW2 PACKAGE WILL ONLY SOLVE USING HCOF'
                 SOLV_FLG = TWO
                 
        CASE('ONLY_HCOF_AFTER') 
                 WRITE(IOUT,'(/2A/)') 'ONLY_HCOF_AFTER: MNW2 PACKAGE ',
     +          'WILL ONLY SOLVE USING HCOF AFTER SPECIFIED ITERATIONS.'
                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +                            HCOF_ONLY_ITER,
     +           MSG='ONLY_HCOF_AFTER FAILED TO LOAD THE ITER_START')
                 
        CASE('HCOF_RHS_FLIP') 
                 WRITE(IOUT,'(/2A/)') 'HCOF_RHS_FLIP: MNW2 PACKAGE ',
     +          'WILL FLIP BETWEEN RHS TO HCOF AFTER EVER SET OF ',
     +          'SPECIFIED ITERATIONS.'
                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,IN,
     +                            HCOF_RHS_FLIP,
     +           MSG='HCOF_RHS_FLIP FAILED TO LOAD THE ITER_FLIP')
                 SOLV_FLG = ONE
        CASE DEFAULT
            ! -- NO OPTIONS FOUND
            EXIT
        END SELECT
       END DO
C
C5------ALLOCATE SPACE FOR MNW2 ARRAYS.
C5------FOR EACH WELL, THERE ARE 30 DATA VALUES PLUS THE AUXILIARY VARIABLES
      NMNWVL=30+NAUX
      ALLOCATE (MNW2(NMNWVL,MNWMAX))
      !
      IF (.NOT. MNW2BUD%BUDGET_GROUPS) CALL MNW2BUD%ADD( -MNWMAX )    !NO BUDGET GROUPS SO SET MNW2BUD TO THE SIZE OF THE MNW2WELLS
      !
C5------FOR EACH NODE, THERE ARE 31 DATA VALUES
c approximate number of nodes= max mnw wells * number of layers, this works well
c if all are mostly vertical wells.  add 10*nlay+25 for extra room.  ispmnwn is 
c passed out to RP routine to check allocation while reading actual # nodes used
C--LFK  Dec. 2012
      IF (NODTOT == Z) THEN
          NODTOT=(MNWMAX*NLAY)+(10*NLAY)+25
      END IF   
C-LFK
      ALLOCATE (MNWNOD(34,NODTOT))
C5------FOR EACH INTERVAL, THERE ARE 11 DATA VALUES
      ALLOCATE (MNWINT(11,NODTOT))
C5------FOR Capacity Table, 
c  27 is the hard-wired number of entries allowed in the Capacity Table (CapTable)
c  2 is the number of fields in the Capacity Table (CapTable): Lift (1) and Q (2)
      ALLOCATE (CapTable(mnwmax,27,2))
C5------FOR WELLID array, add an extra spot 
      ALLOCATE (WELLID(mnwmax+1))
      ALLOCATE(MNW_FMP_LINK(mnwmax+1), SOURCE = FALSE)
      ALLOCATE(HAS_FMP_WELLS, SOURCE=FALSE)
      ALLOCATE(QDES_OLD(MNWMAX), SOURCE=DZ)
C-LFK
      ALLOCATE(LIMQ(3,MNWMAX))
C
C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2MNW2PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2MNW27AR2(IN,kper,Iusip,Iude4,Iusor,Iupcg,
     +                    Iulmg,Iugmg,Iupcgn,igwtunit,Iunwt,IUFMP,IGRID)
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD, UPLAY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,WELLID,NTOTNOD,IOUT,
     3                       MNW2TABFILE, MNW_FEED, MNW2BUD, GRPNODE, !seb ADDED TABFILE
     4                       MNW_FMP_LINK,HAS_FMP_WELLS,MIN_PERF,
     5                       PRNT_MNW2_Q_NODE,PRNT_MNW2,
     6                       PRNT_MNW2_Q_INOUT,MXNODE,LEN_WELLID,
     7                       PRNT_MNW2_NODE
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE SIPMODULE,ONLY:HCLOSE
      USE DE4MODULE,ONLY:HCLOSEDE4
      USE PCGMODULE,ONLY:HCLOSEPCG
      USE GWFNWTMODULE,ONLY:Tol
      USE GMGMODULE,ONLY:HCLOSEGMG
      USE PCGN,ONLY:HCLOSEPCGN
      USE TABLEFILE_INTERFACE,ONLY: TABFILEPACKINDEX,MNW2TABFILE2QDES   !seb
      USE ERROR_INTERFACE,    ONLY: STOP_ERROR, WARNING_MESSAGE
      USE FILE_IO_INTERFACE,  ONLY: READ_TO_DATA
      USE STRINGS,            ONLY: JOIN_TXT
      USE NUM2STR_INTERFACE,  ONLY: NUM2STR, SEQ2STR
      USE BAS_UTIL, ONLY: GET_LOWEST_LAYER!(IR,IC,IBOUND) RESULT(IL)
      USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
      USE CONSTANTS, ONLY: NL,BLN,TRUE,FALSE,BLNK,
     +                     DNEG,DZ,UNO,DOS,TRES,QUAD,
     +                     NEARZERO_20, NEGNEARZERO_20,
     +                     ninf, NEG, Z, ONE
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: IN,kper,Iusip,Iude4,Iusor,Iupcg,Iulmg,
     +                      Iugmg,Iupcgn,igwtunit,Iunwt,IUFMP,IGRID 
      INTEGER:: Qlimit, QCUT, firstnode, lastnode
      INTEGER:: PUMPLAY, PUMPROW, PUMPCOL,PUMPLOC,PUMPCAP, PPFLAG
      INTEGER:: NNODES, INODE, nod, NODNUM, nodecount, NDALT, MNWID
      INTEGER:: INTNUM, NINTVL, IINT, intnodes, ITOP, IBOT
      INTEGER:: index, ifound, IPRT
      INTEGER:: IL, IR, IC, IRlast, IClast
      INTEGER:: I, J, K, LLOC, ISTART, ISTOP, IG, ID
      REAL:: R
      DOUBLE PRECISION:: CapMult
      DOUBLE PRECISION:: Rw,Rskin,Kskin,B,C,P,CWC,RwNode
      DOUBLE PRECISION:: RskinNode,KskinNode
      DOUBLE PRECISION:: BNode,CNode,PNode,CWCNode
      DOUBLE PRECISION:: Ztop,Zbotm,Zbotmlast,Zpump
      DOUBLE PRECISION:: Hlim,Qfrcmn,Qfrcmx,Qdes,Cprime,PP
      DOUBLE PRECISION:: Qtemp,Hlift,LIFTq0,LIFTqdes,LIFTn,Qn,HWtol
      DOUBLE PRECISION:: THCK, SCRN, BTM, TOP
      CHARACTER(20):: WELLNAME,LOSSTYPE
      CHARACTER(14):: cZtop, cZbot
      CHARACTER(768):: LINE
      LOGICAL:: CHECK
      !CHARACTER(:),ALLOCATABLE:: ERR
      TYPE(WARNING_TYPE):: WRN, WRN2,WRN3,ERR
C
c
      CALL SGWF2MNW2PNT(IGRID)
      
      CALL WRN%INIT()
      CALL ERR%INIT()
      CALL WRN2%INIT()
      CALL WRN3%INIT()
      
c     set defaults
      ntotnod = Z
      INTTOT  = Z
C-------SET SMALL DEPENDING ON CLOSURE CRITERIA OF THE SOLVER
      SMALL = DZ
      IF ( Iusip.NE.0 ) SMALL = HCLOSE
      IF ( Iude4.NE.0 ) SMALL = HCLOSEDE4
!     IF ( Iusor.NE.0 ) SMALL = HCLOSESOR
      IF ( Iupcg.NE.0 ) SMALL = HCLOSEPCG
      IF ( Iulmg.NE.0 ) SMALL = 0.0D0  !LMG SETS HCLOSE TO ZERO
      IF ( Iunwt.NE.0 ) SMALL = TOL
      IF ( Iugmg.NE.0 ) SMALL = HCLOSEGMG
      IF ( Iupcgn.NE.0 ) SMALL = HCLOSEPCGN
c     initialize
      WELLID = BLNK
      MNW2   = DZ
      MNWNOD = DZ
      MNWINT = DZ
c--lfk
      NDALT = Z
      write(iout,'(/A)') 'MNW2 Input:'
c0----read MNW info: wellid, nnodes, losstype, pumploc, Qlimit
      READ_MNW_WELLS: DO MNWID=1,MNWMAX
c  initialize CapFlag2 at beginning of simulation
        mnw2(27,MNWID)=DZ
c       
        IF(MNWPRNT>-1) write(iout,'(//A)') 
     +                  'WELLID             NNODES    LOSSTYPE',
     +                  ' PUMPLOC  Qlimit  PPFLAG PUMPCAP'
c     read Data Set 2a
        CALL READ_TO_DATA(LINE,IN,IOUT)
        LLOC=ONE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        WELLID(MNWID) = LINE(ISTART:ISTOP)
        WELLNAME = WELLID(MNWID)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NNODES,R,IOUT,IN)
        !
        IF (MNW2BUD%BUDGET_GROUPS) THEN
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
           CALL MNW2BUD%ADD( MNWID, LINE(ISTART:ISTOP), IG )
        ELSE
            IG = ONE
        END IF
!       read(in,*) WELLID(MNWID),NNODES
c     read Data Set 2b
        read(in,*) LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP
c     convert wellid and losstype to uppercase
        call UPCASE(LOSSTYPE)
c          write(iout,'(1x,A20,1x,I4,5x,A10,2x,I3,2(5x,I3))')
c     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG
c     write output with Format depending on LOSSTYPE
        if(LOSSTYPE.EQ.'NONE') then
         IF(MNWPRNT>-1) THEN
           write(iout,'(1x,A20,1x,I4,6x,A10,1x,I3,3(5x,I3))')
     + WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
     + PUMPCAP
         ENDIF
        elseif(LOSSTYPE.EQ.'THIEM') then
         IF(MNWPRNT>-1) THEN
           write(iout,'(1x,A20,1x,I4,6x,A10,1x,I3,3(5x,I3))')
     + WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
     + PUMPCAP
         ENDIF
        elseif(LOSSTYPE.EQ.'SKIN') then
         IF(MNWPRNT>-1) THEN
           write(iout,'(1x,A20,1x,I4,6x,A10,1x,I3,3(5x,I3))')
     + WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
     + PUMPCAP
         ENDIF
        elseif(LOSSTYPE.EQ.'GENERAL') then
         IF(MNWPRNT>-1) THEN
           write(iout,'(1x,A20,1x,I4,5x,A10,2x,I3,3(5x,I3))')
     + WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
     + PUMPCAP
         ENDIF
        elseif(LOSSTYPE.EQ.'SPECIFYCWC') then
         IF(MNWPRNT>-1) THEN
           write(iout,'(1x,A20,1x,I4,2x,A10,5x,I3,3(5x,I3))')
     + WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
     + PUMPCAP
         ENDIF
        else
          CALL ERR%ADD(WELLID(MNWID)//
     +            ' HAS THE FOLLOWING UNKNOWN LOSSTYPE" '//LOSSTYPE//NL)
        end if
c     check WELLID vs existing names
        DO ID=1,MNWID-1
          if(WELLID(MNWID).EQ.WELLID(ID)) then
              CALL ERR%ADD(WELLID(MNWID)//
     +                      '   IS A WELLID DEFINED MORE THEN ONCE'//NL)
          end if                
        END DO
c     set PUMPLOC and PUMPCAP
        MNW2(11,MNWID)=PUMPLOC
        MNW2(22,MNWID)=PUMPCAP
c     CapTable has max 27 entires, so PUMPCAP must not be > 25
        if(PUMPCAP > 25) then
           PUMPCAP = 25
           CALL ERR%ADD(WELLID(MNWID)//
     +               ' -- PUMPCAP CANNOT BE GREATER THAN 25'//NL)
        end if
c     set IACTIV=0, defaulting well to inactive
        MNW2(1,MNWID) = DZ
c     set number of nodes
        MNW2(2,MNWID)=NNODES
c     define LOSSTYPE using integers in MNW2 array
        if(LOSSTYPE == 'NONE') then
c     for none, NNODES must be 1
          if(NNODES /= ONE) then
                            CALL ERR%ADD(WELLID(MNWID)//
     +         ' -- LOSSTYPE = "NONE" REQUIRES NNODES=1 AND NOT >1'//NL)
          end if
          MNW2(3,MNWID) = DZ
        elseif(LOSSTYPE == 'THIEM') then
                                        MNW2(3,MNWID) = UNO 
        elseif(LOSSTYPE == 'SKIN') then
                                        MNW2(3,MNWID) = DOS
        elseif(LOSSTYPE.EQ.'GENERAL') then
                                        MNW2(3,MNWID) = TRES
        elseif(LOSSTYPE.EQ.'SPECIFYCWC') then
                                        MNW2(3,MNWID) = QUAD
        else
                                        MNW2(3,MNWID) = DNEG
        end if
c     initialize QDES=0
        MNW2(5,MNWID) = DZ
c     set Qlimit flag.  Qlimit.ne.0 means limit or constraint is on.  Qlimit<0 means
c          read it every stress period
        IF    (Qlimit > Z) THEN
                           MNW2(6,MNWID) = UNO
        ELSEIF(Qlimit < Z) THEN
                           MNW2(6,MNWID) = DNEG
        ELSE
                           MNW2(6,MNWID) = DZ
        END IF
        !
        ! set PPFLAG flag.  PPFLAG>0 means calculate partial penetration effect.
        IF    (PPFLAG > Z) THEN
                           MNW2(19,MNWID) = UNO
        ELSE
                           MNW2(19,MNWID) = DZ
        END IF
c     end read Data Set 2a and 2b
c
c     warning if LOSSTYPE=SPECIFYCWC and PPFLAG>0 (no PP correction done for this LOSSTYPE)
        if(LOSSTYPE.EQ.'SPECIFYCWC' .and. PPFLAG > Z) then
          CALL WRN%ADD(WELLID(MNWID)//
     +      ' -- PARTIAL PENETRATION NOT CALCULATED FOR '//
     +      'LOSSTYPE = "SPECIFYCWC", PPFLAG RESET TO 0'//NL)
!            CALL WARNING_MESSAGE(INFILE=IN,OUTPUT=IOUT,
!     +      MSG='MNW2: PARTIAL PENETRATION NOT CALCULATED FOR '//
!     +      'LOSSTYPE = SPECIFYCWC, PPFLAG RESET TO 0',INLINE=.TRUE.)
c-LFK (5/28/15) reset ppflag to 0; write note to output file
            !write(IOUT,*) '              PPFLAG reset to 0'
            !write(IOUT,*)
            PPFLAG = Z
            MNW2(19,MNWID) = DZ
        end if
c
c     read Data Set 2c, depending on LOSSTYPE (MNW2(3,MNWID)
        Rw=NEARZERO_20; Rskin = Rw; Kskin = Rw; B = Rw; C = Rw; P = UNO
        SELECT CASE (NINT(MNW2(3,MNWID)))
          CASE (1)
            READ(in,*) Rw
          CASE (2)
            READ(in,*) Rw,Rskin,Kskin
          CASE (3)
            READ(in,*) Rw,B,C,P
          CASE (4)
            READ(in,*) CWC
          CASE (-1)
            READ(in,'(A)') LINE  !ERROR FOUND JUST BYPASS LOADING
        END SELECT
c     don't allow Rw = 0
        if(NEGNEARZERO_20 <= Rw .AND. Rw < NEARZERO_20) then
          CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Rw = 0.0??? This is not allowed'//NL)
        endif
        if(NEGNEARZERO_20 <= Rskin .AND. Rskin < NEARZERO_20) then
          CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Rskin = 0.0??? This is not allowed'//NL)
        end if
        if(NEGNEARZERO_20 <= Kskin .AND. Kskin < NEARZERO_20) then
          CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Kskin = 0.0??? This is not allowed'//NL)
        endif
        if(P > DZ .AND. (P<1D0.or.P>3.5D0)) then
          CALL ERR%ADD(WELLID(MNWID)//
     +    ' -- P  MUST BE 1 <= P <=3.5, BUT FOUND: '//NUM2STR(P)//NL )
        endif
c     end read Data Set 2c
c
c     set HWflag (horizontal well flag) to 0 as default
c     can be set to 1 (true) if NNODES>0 and any R,C is different than first
        MNW2(21,MNWID) = DZ
c     read Data Set 2d, the list of nodes
c
c     if nnodes>0, read IL,IR,IC and LOSSTYPE variables set < 0
        IF(NNODES > Z) THEN
c     calculate first node number in nodal list= (total nodes so far) + 1
c     this will be used to access the information in the nodal array (MNWNOD)
          NODNUM = ntotnod + ONE
          MNW2(4,MNWID) = DBLE(NODNUM)
c     count nodes (will uses this to check vs. allocation)
          ntotnod = ntotnod + NNODES
          GRPNODE(IG) = GRPNODE(IG) + ABS(NNODES)   !IG SET BY MNWBUD
          !
          if(ntotnod > nodtot) CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,
     +       MSG='MNW2 ERROR: NODE ARRAY ALLOCATION INSUFFICIENT '//NL//
     +           'PLEASE INCREASE NODTOT BIGGER THAN '//NUM2STR(nodtot))
c     loop over nodes 
          DO INODE=1,NNODES
           !
           RwNode=NEARZERO_20
           RskinNode=RwNode; KskinNode=RwNode
           BNode=RwNode; CNode=RwNode; PNode=UNO
           !
c     If PPFLAG=0, don't read PP variable
           PPFLAG=NINT(MNW2(19,mnwid))
           IF(PPFLAG == Z) then           
c     access the LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
              CASE (0)
                READ(in,*) IL,IR,IC
c
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
              CASE (1)
                IF(Rw > DZ) THEN            
                  READ(in,*) IL,IR,IC
c     Rw at each node is the same if Rw>0.0  
                  RwNode=Rw         
                ELSE
c     If Rw<0, read in separate Rw for each node
                  READ(in,*) IL,IR,IC,RwNode
                END IF
c     Set Rw in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
c
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
                IF(Rw > DZ) THEN            
                  IF(Rskin > DZ) THEN
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC
                      RwNode=Rw             
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,KskinNode
                      RwNode=Rw             
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,RskinNode
                      RwNode=Rw             
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RskinNode,KskinNode
                      RwNode=Rw             
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(Rskin > DZ) THEN
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,RwNode
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,KskinNode
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,RwNode,RskinNode
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,RskinNode,KskinNode
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(6,NODNUM+INODE-1)=RskinNode             
                MNWNOD(7,NODNUM+INODE-1)=KskinNode             
c
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
              CASE (3)
                IF(Rw > DZ) THEN            
                  IF(B >= DZ) THEN
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,PNode
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,CNode
                        RwNode=Rw             
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,CNode,PNode
                        RwNode=Rw             
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,BNode
                        RwNode=Rw             
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,PNode
                        RwNode=Rw             
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,BNode,CNode
                        RwNode=Rw             
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,CNode,PNode
                        RwNode=Rw             
                      END IF
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(B >= DZ) THEN
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,RwNode
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,PNode
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,Rwnode,CNode
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,CNode,PNode
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,PNode
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode,PNode
                      END IF
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(8,NODNUM+INODE-1)=BNode             
                MNWNOD(9,NODNUM+INODE-1)=CNode            
                MNWNOD(10,NODNUM+INODE-1)=PNode             
c
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
              CASE (4)
                IF(CWC > DZ) THEN            
                  READ(in,*) IL,IR,IC
                  CWCNode=CWC         
                ELSE
                  READ(in,*) IL,IR,IC,CWCNode
                END IF
                MNWNOD(11,NODNUM+INODE-1)=CWCNode
              CASE (-1)
                READ(in,'(A)') LINE  !ERROR FOUND JUST BYPASS LOADING
            END SELECT
c    ELSE if PPFLAG NE 0, read PP flag
           ELSE
c     access the LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
              CASE (0)
                READ(in,*) IL,IR,IC,PP
c
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
              CASE (1)
                IF(Rw > DZ) THEN           
                  READ(in,*) IL,IR,IC,PP
c     Rw at each node is the same if Rw>0.0  
                  RwNode=Rw         
                ELSE
c     If Rw<0, read in separate Rw for each node
                  READ(in,*) IL,IR,IC,RwNode,PP
                END IF
c     Set Rw in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
c
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
                IF(Rw > DZ) THEN            
                  IF(Rskin > DZ) THEN
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,PP
                      RwNode=Rw             
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,KskinNode,PP
                      RwNode=Rw             
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,RskinNode,PP
                      RwNode=Rw             
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RskinNode,KskinNode,PP
                      RwNode=Rw             
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(Rskin > DZ) THEN
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,RwNode,PP
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,KskinNode,PP
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin > DZ) THEN
                      READ(in,*) IL,IR,IC,RwNode,RskinNode,PP
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,RskinNode,KskinNode,PP
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(6,NODNUM+INODE-1)=RskinNode             
                MNWNOD(7,NODNUM+INODE-1)=KskinNode             
c
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
              CASE (3)
                IF(Rw > DZ) THEN            
                  IF(B >= DZ) THEN
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,PP
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,PNode,PP
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,CNode,PP
                        RwNode=Rw             
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,CNode,PNode,PP
                        RwNode=Rw             
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,BNode,PP
                        RwNode=Rw             
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,PNode,PP
                        RwNode=Rw             
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,BNode,CNode,PP
                        RwNode=Rw             
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,CNode,PNode,PP
                        RwNode=Rw             
                      END IF
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(B >= DZ) THEN
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,RwNode,PP
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,PNode,PP
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,Rwnode,CNode,PP
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,CNode,PNode,PP
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode,PP
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,PNode,PP
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode,PP
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode,PNode,PP
                      END IF
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(8,NODNUM+INODE-1)=BNode             
                MNWNOD(9,NODNUM+INODE-1)=CNode            
                MNWNOD(10,NODNUM+INODE-1)=PNode             
c
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
              CASE (4)
                IF(CWC > DZ) THEN            
                  READ(in,*) IL,IR,IC,PP
                  CWCNode=CWC         
                ELSE
                  READ(in,*) IL,IR,IC,CWCNode,PP
                END IF
                MNWNOD(11,NODNUM+INODE-1)=CWCNode
              CASE (-1)
                READ(in,'(A)') LINE  !ERROR FOUND JUST BYPASS LOADING
            END SELECT            
           END IF
c     save node location, set Qdes=0.0, set PP, flag ZPD
            MNWNOD(1,NODNUM+INODE-1) = DBLE(IL)             
            MNWNOD(2,NODNUM+INODE-1) = DBLE(IR)            
            MNWNOD(3,NODNUM+INODE-1) = DBLE(IC)           
            MNWNOD(4,NODNUM+INODE-1) = DZ
            if(PPFLAG <= Z) then
              PP=DZ
            end if
            MNWNOD(19,NODNUM+INODE-1)=PP 
            !
            if(RwNode < NEARZERO_20 .AND. Rw <= DZ) then
                  CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Rw <= 0.0??? This is not allowed'//NL)
            endif
            if(RskinNode < NEARZERO_20 .AND. Rskin <= DZ) then
                  CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Rskin <= 0.0??? This is not allowed'//NL)
            end if
            if(KskinNode < NEARZERO_20 .AND. Kskin <= DZ) then
                  CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Kskin <= 0.0??? This is not allowed'//NL)
            endif
            if(P < DZ) then
              if(PNode > DZ .and. (PNode < UNO .or. PNode>3.5D0)) then
                    CALL ERR%ADD(WELLID(MNWID)//
     +              ' -- P  MUST BE 1 <= P <=3.5, BUT FOUND: '//
     +              NUM2STR(PNode)//NL)
              endif
            endif
            !
c     save IR and IC to check vs the subsequent nodes for vert/horiz
            IF(INODE == ONE) THEN
              IRlast=IR
              IClast=IC
            ELSE
c     if any node is a different R,C, this is a nonvertical well
              IF((IR.NE.IRlast).OR.(IC.NE.IClast)) THEN
c       set HWflag to true
                MNW2(21,MNWID) = UNO
              END IF
            END IF

c     if partial penetration ne 0, set ZPD to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well)
            if(pp /= DZ) MNWNOD(20,NODNUM+INODE-1)=1d30         
          END DO  !INODE=1,NNODES - loop over nodes 
c
c       end nnodes>0 read statements
c
c     if nnodes<0, read in Ztop and Zbot which define intervals
        ELSE
c
c     calculate first interval number in interval list= (total ints so far) + 1
c     this will be used to access the information in the interval array (MNWINT)
          INTNUM=INTTOT+1
          MNW2(13,MNWID)=INTNUM
c     the abs value of NNODES represents the number of intervals to de defined
          NINTVL=ABS(NNODES)
c     count intervals to check vs. allocation
          INTTOT=INTTOT+NINTVL
c     initialize interval node counter
          intnodes=0
c     loop over the intervals in this well
          DO 4 IINT=1,NINTVL
          !
        RwNode=NEARZERO_20; RskinNode=RwNode; KskinNode=RwNode
        BNode=RwNode; CNode=RwNode; PNode=UNO
c
c     access the LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read Ztop,Zbotm,IR,IC only
              CASE (0)
                READ(in,*) Ztop,Zbotm,IR,IC
c
c     LOSSTYPE=THIEM, read Ztop,Zbotm,IR,IC,{Rw}
              CASE (1)
                IF(Rw > DZ) THEN            
                  READ(in,*) Ztop,Zbotm,IR,IC
c     Rw at each node is the same if Rw>0.0  
                  RwNode=Rw         
                ELSE
c     If Rw<0, read in separate Rw for each node
                  READ(in,*) Ztop,Zbotm,IR,IC,RwNode
                END IF
c     Set Rw in interval list, spot is 1st int (INTNUM) + current step (IINT) - 1
                MNWINT(5,INTNUM+IINT-1)=RwNode             
c
c     LOSSTYPE=SKIN, read Ztop,Zbotm,IR,IC,{Rw Rskin Kskin}
              CASE (2)
                IF(Rw > DZ) THEN            
                  IF(Rskin > DZ) THEN
                    IF(Kskin > DZ) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC
                      RwNode=Rw             
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,KskinNode
                      RwNode=Rw             
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin > DZ) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC,RskinNode
                      RwNode=Rw             
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,RskinNode,KskinNode
                      RwNode=Rw             
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(Rskin > DZ) THEN
                    IF(Kskin > DZ) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode,KskinNode
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin > DZ) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode,RskinNode
                        KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode,RskinNode,
     &                  KskinNode
                    ENDIF
                  ENDIF
                END IF
c     Set vars for interval
                MNWINT(5,INTNUM+IINT-1)=RwNode             
                MNWINT(6,INTNUM+IINT-1)=RskinNode             
                MNWINT(7,INTNUM+IINT-1)=KskinNode             
c
c     LOSSTYPE=GENERAL, read Ztop,Zbotm,IR,IC,{Rw B C P}
              CASE (3)
                IF(Rw > DZ) THEN            
                  IF(B >= DZ) THEN
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,PNode
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,CNode
                        RwNode=Rw             
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,CNode,PNode
                        RwNode=Rw             
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode
                        RwNode=Rw             
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode,PNode
                        RwNode=Rw             
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode,CNode
                        RwNode=Rw             
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode,CNode,PNode
                        RwNode=Rw             
                      END IF
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(B >= DZ) THEN
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,RwNode
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,PNode
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,CNode
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,CNode,PNode
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C >= DZ) THEN
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode,PNode
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P >= DZ) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode,
     &                    PNode
                      END IF
                    ENDIF
                  ENDIF
                END IF
c     Set vars for interval
                MNWINT(5,INTNUM+IINT-1)=RwNode             
                MNWINT(8,INTNUM+IINT-1)=BNode             
                MNWINT(9,INTNUM+IINT-1)=CNode            
                MNWINT(10,INTNUM+IINT-1)=PNode             
c
c     LOSSTYPE=SPECIFYcwc, read Ztop,Zbotm,IR,IC,{CWC}
              CASE (4)
                IF(CWC > DZ) THEN            
                  READ(in,*) Ztop,Zbotm,IR,IC
                  CWCNode=CWC         
                ELSE
                  READ(in,*) Ztop,Zbotm,IR,IC,CWCNode
                END IF
c     Set var for interval
                MNWINT(11,INTNUM+IINT-1)=CWCNode             
            END SELECT
            !
            if(RwNode < NEARZERO_20 .AND. Rw <= DZ) then
                  CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Rw <= 0.0??? This is not allowed'//NL)
            endif
            if(RskinNode < NEARZERO_20 .AND. Rskin <= DZ) then
                  CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Rskin <= 0.0??? This is not allowed'//NL)
            end if
            if(KskinNode < NEARZERO_20 .AND. Kskin <= DZ) then
                  CALL ERR%ADD(WELLID(MNWID)//
     +            ' -- Kskin <= 0.0??? This is not allowed'//NL)
            endif
            if(P < DZ) then
              if(PNode > DZ .and. (PNode<UNO.or.PNode>3.5D0)) then
                    CALL ERR%ADD(WELLID(MNWID)//
     +              ' -- P  MUST BE 1 <= P <=3.5, BUT FOUND: '//
     +              NUM2STR(PNode)//NL)
              endif
            endif
            !
            IF(IINT > ONE) THEN
              IF(Ztop > Zbotmlast) THEN
                 CALL WRN%ADD(WELLID(MNWID)//
     +         ' -- Ztop > Zbotm from previous interval. Ztop is set '//
     +         'to Zbotm from previous interval with previous Ztop = '//
     +           NUM2STR(Ztop)//' AND Zbotm of previous interval = '//
     +           NUM2STR(Zbotmlast)//NL )
                 Ztop = Zbotmlast
              END IF
            END IF
            !
            IF(Ztop < Zbotm) THEN
               CALL WRN%ADD(WELLID(MNWID)//
     +         ' -- Ztop < Zbotm,  Ztop is set to Zbotm the previous '//
     +   'Ztop = '//NUM2STR(Ztop)//' and Zbotm  = '//NUM2STR(Zbotm)//NL)
                Ztop = Zbotm + (Zbotm*1D-10)
            END IF
            !
            IF(UPLAY(IC,IR) == Z) THEN  !No Active Cells, so kill search and move screen to bottom of model
                ITOP  = NLAY
                IBOT  = NLAY
                IL    = NLAY
                Ztop  = ninf
                Zbotm = ninf
            ELSE
               IL = NEG
               DO I=1, NLAY
                 !
                 IF(IBOUND(IC,IR,I) == Z ) CYCLE
                 !
                 BTM = BOTM(IC,IR,LBOTM(I))
                 !
                 IF( Ztop > BTM) THEN
                   !
                               TOP = BOTM(IC,IR,LBOTM(I)-1)
                   IF( Zbotm > TOP ) THEN                     !Bad interval => ignore
                                !
                                CALL WRN%ADD(WELLID(MNWID)//
     +   ' -- Failed to locate an active model cell within the '//
     +   'specified Ztop-Zbotm interval. This interval will be '//
     +   'ingnored for the entire simulation. Ztop = '// NUM2STR(Ztop)//
     +   ' and Zbotm  = '//NUM2STR(Zbotm)//NL )
                                Ztop  = TOP
                                Zbotm = TOP
                                IL    = I
                                EXIT
                                !
                   ELSEIF( Ztop > TOP ) THEN  !Shift Ztop to top of first active layer
                                !
                                CALL WRN2%ADD(WELLID(MNWID)//
     +   ' -- Ztop was lowered to the '//
     +   'top of the first active (not IBOUND=0) layer. Ztop = '//
     +   NUM2STR(Ztop)//' and new lowered Ztop  = '//
     +   NUM2STR(TOP)//' that is within Layer = '//NUM2STR(I)//NL )
                                Ztop = TOP
                                IL = I
                                EXIT
                   ELSE
                      ! Check if Ztop is enough of the layer to make it work simulating 
                      !
                      THCK = TOP  - BTM
                      SCRN = Ztop - BTM
                      !
                      IF (THCK < NEARZERO_20) THEN  !Skip layer cause its too thin
                          !
                          Ztop = TOP
                          CYCLE
                          !
                      ELSEIF(SCRN/THCK < MIN_PERF .AND. I < NLAY ) THEN  !Screen makes up less than 1% of layer, exclude
                        !
                        SCRN=BTM - MIN_PERF*(BTM-BOTM(IC,IR,LBOTM(I)+1))!1% of next layers thickness
                        !
                        IF(Zbotm < SCRN) THEN
                            CALL WRN2%ADD(WELLID(MNWID)//
     +   ' -- Ztop was lowered from '//NUM2STR(Ztop)//' to '//
     +   NUM2STR(BTM)//' because it only was perfed '//
     +   'for less than 1% into layer '//NUM2STR(I)//'.'//NL )
                           Ztop = BTM
                           CYCLE
                        END IF
                      END IF
                      !
                      IL = I
                      EXIT
                   END IF ! IF( Zbotm > TOP ) THEN;  ELSEIF( Ztop > TOP ) THEN; 
                 END IF ! IF( Ztop .GE. BTM) THEN
               END DO  ! DO I=1, NLAY
               !
               ITOP  = IL
            END IF
            !
            IF(IL==-1) THEN
                          IL    = NLAY
                          ITOP  = NLAY
                          IBOT  = NLAY
                          CALL WRN%ADD(WELLID(MNWID)//
     +   ' -- Failed to locate an active model cell within the '//
     +   'specified Ztop-Zbotm interval. This well be deactivated '//
     +   'for the entire simulation. Ztop = '// NUM2STR(Ztop)//
     +   ' and Zbotm  = '//NUM2STR(Zbotm)//NL )
                          Zbotm = Ztop
                          !
            ELSEIF(Zbotm < Ztop) THEN  !Only true if interval has some thickness. Any errors will zero out intervals thickness
               !
               J  = IL  !Current Top
               IL = NEG
               CHECK = .TRUE.
               DO I=NLAY, J, NEG
                 !
                 IF(IBOUND(IC,IR,I) == Z ) THEN
                     CHECK = TRUE 
                     CYCLE
                 END IF
                 !
                 TOP = BOTM(IC,IR,LBOTM(I)-1)
                 BTM = BOTM(IC,IR,LBOTM(I))
                 !
                 IF(CHECK) THEN  
                    CHECK = FALSE
                    IF(Zbotm < BTM) THEN   !Bottom below bottom of model
                       !
                       CALL WRN2%ADD(WELLID(MNWID)//
     +                      ' -- Zbotm was raised to the '//
     +   'bottom of the lowest active (not IBOUND=0) layer. Zbotm = '//
     +   NUM2STR(Zbotm)//' and new raised Zbotm  = '//
     +   NUM2STR(BTM)//' that is within Layer = '//NUM2STR(I)//NL )
                       Zbotm = BTM
                       IL = I
                       EXIT
                    END IF
                 END IF
                 !
                 IF( Zbotm < TOP) THEN
                   IF( Zbotm < BTM) THEN  !Shift Zbotm to bottom since it is perf below deepest active layer -- Should never be true
                                !
                                CALL WRN2%ADD(WELLID(MNWID)//
     +   ' -- Zbotm was raised to the '//
     +   'bottom of the lowest active (not IBOUND=0) layer. Zbotm = '//
     +   NUM2STR(Zbotm)//' and new raised Zbotm  = '//
     +   NUM2STR(BTM)//' that is within Layer = '//NUM2STR(I)//NL )
                                Zbotm = BTM
                                IL = I
                                EXIT
                   ELSE
                      ! Check if Zbotm is enough of the layer to make it work simulating 
                      !
                      THCK = TOP - BTM
                      SCRN = TOP - Zbotm
                      !
                      IF (THCK < NEARZERO_20) THEN  !Skip layer cause its too thin
                          !
                          Zbotm = TOP
                          CYCLE
                          !
                      ELSEIF(SCRN/THCK < MIN_PERF .AND. I<J ) THEN  !Screen makes up less than 1% of layer, exclude
                        !
                        SCRN=TOP + MIN_PERF*(BOTM(IC,IR,LBOTM(I)-1)-TOP)!1% of next layers thickness
                        !
                        IF(Ztop > SCRN) THEN
                            CALL WRN2%ADD(WELLID(MNWID)//
     +   ' -- Zbotm was raised from '//NUM2STR(Zbotm)//' to '//
     +   NUM2STR(TOP)//' because it only was perfed '//
     +   'for less than 1% into layer '//NUM2STR(I)//'.'//NL )
                           Ztop = BTM
                           CYCLE
                        END IF
                      END IF
                      !
                      IL = I
                      EXIT
                   END IF ! IF( Zbotm < BTM) THEN
                 END IF   ! IF( Zbotm < TOP) THEN
                END DO    ! DO I=NLAY, J, -1
                !
                IF(IL == NEG) THEN  ! This should not happen, deactivate node
                    IL = NLAY
                    Ztop  = BTM
                    Zbotm = BTM
                END IF
                !
                IBOT  = IL
                !
            END IF ! IF(IL==-1) THEN -- Way up high
            !
c     Set vars for interval
            MNWINT(1,INTNUM+IINT-1) = Ztop            
            MNWINT(2,INTNUM+IINT-1) = Zbotm            
            MNWINT(3,INTNUM+IINT-1) = IR            
            MNWINT(4,INTNUM+IINT-1) = IC            
c     Do error-checking on interval elevations
c     If beyond the first interval, do check on elevation
c     Save bottom of last interval for above check

            IF(Ztop >= Zbotm) THEN
                Zbotmlast=Zbotm
            ELSE
                Zbotmlast=Ztop  !?!?!?! should never happen ;)
            END IF
c
c     create nodes from interval information
c     MNWNOD will access MNWINT by pointing to the first and last interval
c       that intersects the node
c
c     set node counter for this interval
            nodecount = Z
c     save IR and IC to check vs the subsequent nodes
            IF(IINT == ONE) THEN
              IRlast=IR
              IClast=IC
            ELSEIF( IR /= IRlast .OR. IC /= IClast ) THEN
                  CALL ERR%ADD(WELLID(MNWID)//
     +    ' -- ROW AND COLUMN MUST BE THE SAME WHEN NNODES<0 '//
     +    '(ITS A VERTICAL ONLY WELL)'//NL )
      END IF
c
c     find first layer that the top of this interval penetrates, set = IL
c     botm(...k) points to the bottom of layer K
      ! IBOT HOLDS BOTTOM LAYER
          !!!IL=NLAY
          !!!IF( CHECK .AND. ITOP > 0 ) THEN
          !!!  !
          !!!  DO I=ITOP, NLAY
          !!!      IF(Ztop.GE.BOTM(IC,IR,LBOTM(I))) THEN
          !!!        IL=I
          !!!        EXIT
          !!!      END IF
          !!!  END DO
          !!!END IF
c
c     now that we have coordinates, start creating cells
c
c           Check if there is a screened iterval
            IF(Ztop <= Zbotm) THEN
                !
                IF(IINT == NINTVL .AND. intnodes.eq.0) THEN
                    MNW2(1,MNWID)=DNEG
                    CALL WRN%ADD(WELLID(MNWID)//
     +' -- Ztop <=Zbotm for all intervals causing '//
     +'cell-to-well cond = 0. Well deactivated '//
     +'for entire simulation.'//NL )
                END IF
                !
                GO TO 4  !Skip interval cause it has no lenght
                !
c     if we haven't create any cells, create the first
            ELSEIF(intnodes == Z) THEN
!              IF(IBOUND(IC,IR,IL).NE.0) THEN  seb
c     calculate first node number in nodal list= (total nodes so far) +1
c     this will be used to access the information in the nodal array (MNWNOD)
                NODNUM = ntotnod + ONE
c     increase total node count 
                ntotnod = ntotnod + ONE          
          if(ntotnod > nodtot) then
                 CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +           'MNW2 ERROR: NODE ARRAY ALLOCATION INSUFFICIENT '//NL//
     +           'PLEASE INCREASE NODTOT BIGGER THAN '//NUM2STR(nodtot))
          end if
c     increase count of nodes in this interval
                nodecount = nodecount + ONE   
c     increase count of nodes in this well
                intnodes = intnodes + ONE   
c     mark this as the first node in the node list for this well
                if(intnodes == ONE) MNW2(4,MNWID) = DBLE(NODNUM)
c     create node in node list from interval coordinates
                MNWNOD(1,NODNUM) = DBLE( ITOP )            
                MNWNOD(2,NODNUM) = DBLE( IR   )         
                MNWNOD(3,NODNUM) = DBLE( IC   )        
                MNWNOD(4,NODNUM) = DZ
c     set first and last intervals in this node= interval #1
c     INTNUM is location in int list of 1st interval for this well
c     IINT is loop counter for intervals
                MNWNOD(12,NODNUM)=INTNUM+IINT-1
                MNWNOD(13,NODNUM)=INTNUM+IINT-1
c     if partial penetration ne 0, set ZPD to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well)
                MNWNOD(20,NODNUM)=1d30     
c     if a node has been created (nodecount>0), then check to see if this interval
c     is still in that node (still NODNUM)
            ELSE
              IF( NINT(MNWNOD(1,NODNUM)).EQ.ITOP ) THEN
c     if interval is still in previous node, re-set "last int" for that node
c     do not increase nodecount, as it is in same node
                MNWNOD(13,NODNUM)=INTNUM+IINT-1
C--LFK
                NDALT=1
c     if top of this interval is in a new node, create a node in that layer
              ELSE
!                IF(IBOUND(IC,IR,IL).NE.0) THEN  seb
                  NODNUM=NODNUM+1   
c     increase total node count 
                  ntotnod=ntotnod+1          
          if(ntotnod > nodtot) then
                 CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +           'MNW2 ERROR: NODE ARRAY ALLOCATION INSUFFICIENT '//NL//
     +           'PLEASE INCREASE NODTOT BIGGER THAN '//NUM2STR(nodtot))
          end if
c     increase count of nodes in this interval
                  nodecount=nodecount+1   
c     increase count of nodes in this well
                  intnodes = intnodes + ONE   
                  MNWNOD(1,NODNUM) = DBLE( ITOP )            
                  MNWNOD(2,NODNUM) = DBLE( IR   )         
                  MNWNOD(3,NODNUM) = DBLE( IC   )        
                  MNWNOD(4,NODNUM) = DZ
c     set first and last intervals in this node= interval #1
c     INTNUM is location in int list of 1st interval for this well
c     IINT is loop counter for intervals
                  MNWNOD(12,NODNUM)=INTNUM+IINT-1
                  MNWNOD(13,NODNUM)=INTNUM+IINT-1
c     if partial penetration ne 0, set ZPD to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well)
                  MNWNOD(20,NODNUM)=1d30         
!                ELSE
!                 IF(IBOUND(IC,IR,IL)==0 .AND. CHECK) THEN
!                      CALL WRN%ADD(WELLID(MNWID)//
!     + " -- Has part of it's screened interval that passes throuh an "//
!     + 'IBOUND=0 cell at LAYER = '//NUM2STR(IL)//NL)
!                 END IF
              END IF
            END IF
c
c     check nodecount here before looping over lower layers, possibility that the
c     interval defines nothing is then caught
ccrth            IF(nodecount.gt.0) THEN
            IF(nodecount > Z .OR. NDALT > Z) THEN
c     add more nodes if the bottom of the interval penetrates below this layer,
c     as long as it isn't the last layer
C--LFK
              NDALT=Z
!              K=IL
              DO K = ITOP+1, IBOT
!              DO WHILE(Zbotm.lt.BOTM(IC,IR,LBOTM(K)) .and.
!     +                (K+1).LE.NLAY)                  
!                K=K+1
                !
               IF(IBOUND(IC,IR,K) == Z) THEN  !NODE IS IN IBOUND=0 SO DO NOT STORE IT!!!
                      CALL WRN%ADD(WELLID(MNWID)//
     +" -- Has part of it's screened interval that passes through an "//
     +'IBOUND=0 cell at LAYER = '//NUM2STR(K)//NL)
               ELSE
                IL=K
                NODNUM = NODNUM + ONE
c     increase total node count 
                ntotnod = ntotnod + ONE          
                if(ntotnod.gt.nodtot) then
                 CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +           'MNW2 ERROR: NODE ARRAY ALLOCATION INSUFFICIENT '//NL//
     +           'PLEASE INCREASE NODTOT BIGGER THAN '//NUM2STR(nodtot))
                end if
c     increase count of nodes in this interval
                nodecount = nodecount + ONE   
c     increase count of nodes in this well
                intnodes = intnodes + ONE   
                MNWNOD(1,NODNUM) = DBLE( IL )            
                MNWNOD(2,NODNUM) = DBLE( IR )           
                MNWNOD(3,NODNUM) = DBLE( IC )          
                MNWNOD(4,NODNUM) = DZ           
c     set first and last intervals in this node= interval number
                MNWNOD(12,NODNUM)=INTNUM+IINT-1
                MNWNOD(13,NODNUM)=INTNUM+IINT-1
c     if partial penetration ne 0, set ZPD (MNWNOD(20) to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well
                MNWNOD(20,NODNUM)=1d30   
                END IF
              END DO
            END IF
c
    4     CONTINUE
c         end loop over intervals
c     reset NUMNODES to -(number of nodes in interval) instead of -(# of intervals)
            MNW2(2,MNWID)= DNEG*DBLE(intnodes)
            GRPNODE(IG)  = GRPNODE(IG) + ABS(intnodes)   !IG SET BY MNWBUD
c
c     Print interval information
          if(MNWPRNT > Z) then
              write(iout,*)
c     write info line
          write(iout,'(/2A)') ' NNODES < 0: well defined using open',
     +                        ' intervals as described below'
c     write header depending on LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read Ztop,Zbotm,IR,IC only
              CASE (0)
             write(iout,'(2A)') ' Interval      Ztop       Zbotm    ',
     +                          '  Row  Col'
c     LOSSTYPE=THIEM, read Ztop,Zbotm,IR,IC,{Rw}
              CASE (1)
             write(iout,'(2A)') ' Interval      Ztop       Zbotm    ',
     +                          '  Row  Col      Rw     '

c     LOSSTYPE=SKIN, read Ztop,Zbotm,IR,IC,{Rw Rskin Kskin}
              CASE (2)
             write(iout,'(2A)') ' Interval      Ztop       Zbotm    ',
     +                          '  Row  Col      Rw     Rskin     Kskin'

c     LOSSTYPE=GENERAL, read Ztop,Zbotm,IR,IC,{Rw B C P}
              CASE (3)
             write(iout,'(2A)') ' Interval      Ztop       Zbotm    ',
     +             '  Row  Col      Rw     B         C         P'

c     LOSSTYPE=SPECIFYcwc, read Ztop,Zbotm,IR,IC,{CWC}
              CASE (4)
             write(iout,'(2A)') ' Interval      Ztop       Zbotm    ',
     +                          '  Row  Col      specCWC'
            END SELECT
c
c     get first interval in this well
            INTNUM=NINT(MNW2(13,MNWID))
c     write data depending on LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, write Ztop,Zbotm,IR,IC only
              CASE (0)
            do IINT=INTNUM,(INTNUM+NINTVL-1)
              write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4)')
     +IINT-intnum+1,(MNWINT(j,iint),j=1,2),(INT(MNWINT(j,iint)),j=3,4)
            end do
c     LOSSTYPE=THIEM, write Ztop,Zbotm,IR,IC,{Rw}
              CASE (1)
            do IINT=INTNUM,(INTNUM+NINTVL-1)
              write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P1G11.4)')
     +              IINT-intnum+1,(MNWINT(j,iint),j=1,2),
     +              (INT(MNWINT(j,iint)),j=3,4),(MNWINT(5,iint))
            end do
c     LOSSTYPE=SKIN, write Ztop,Zbotm,IR,IC,{Rw Rskin Kskin}
              CASE (2)
            do IINT=INTNUM,(INTNUM+NINTVL-1)
              write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P3G11.4)')
     +              IINT-intnum+1,(MNWINT(j,iint),j=1,2),
     +              (INT(MNWINT(j,iint)),j=3,4),(MNWINT(j,iint),j=5,7)
            end do
c     LOSSTYPE=GENERAL, write Ztop,Zbotm,IR,IC,{Rw B C P}
              CASE (3)
            do IINT=INTNUM,(INTNUM+NINTVL-1)
              write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P4G11.4)')
     +              IINT-intnum+1,(MNWINT(j,iint),j=1,2),
     +              (INT(MNWINT(j,iint)),j=3,4),(MNWINT(5,iint)),
     +              (MNWINT(j,iint),j=8,10)
            end do
c     LOSSTYPE=SPECIFYcwc, write Ztop,Zbotm,IR,IC,{CWC}
              CASE (4)
            do IINT=INTNUM,(INTNUM+NINTVL-1)
c-lfk              write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P1G11.4)')
              write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,4x,1P1G11.4)')
     +              IINT-intnum+1,(MNWINT(j,iint),j=1,2),
     +              (INT(MNWINT(j,iint)),j=3,4),(MNWINT(11,iint))
            end do
            END SELECT
          end if
c
        END IF
c       end read Data Set 2d
c
c     loop over wells to print out Node Info
c
c
      if(MNWPRNT > Z) then
        write(iout,*)
        firstnode= NINT( MNW2(4,MNWID) )
        lastnode = NINT( MNW2(4,MNWID) + ABS(MNW2(2,MNWID)) - UNO )
c     write info line
        if(nnodes < Z) then
c     for MNWs defined by intervals
         write(iout,'(2A)') ' The following nodes were assigned ',
     +           'to this well based on above open interval information'
         write(iout,'(A)') ' Node  Lay  Row  Col '
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
c  if more than one interval made up this node, write 'composite' 
          if(MNWNOD(12,INODE).ne.MNWNOD(13,INODE)) then
             write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,10A)')
     +             nod,(INT(MNWNOD(i,INODE)),i=1,3),' COMPOSITE'

          else
             write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4)')
     +             nod,(INT(MNWNOD(i,INODE)),i=1,3)       
          end if
         end do
c
        else
c     for MNWs defined by nodes
c
      if(PPFLAG == Z) then
c     write header depending on LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
              CASE (0)
        write(iout,'(A)') ' Node  Lay  Row  Col'
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
              CASE (1)
        write(iout,'(A)') ' Node  Lay  Row  Col      Rw     '
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
        write(iout,'(A)') ' Node  Lay  Row  Col      Rw     ',
     &'Rskin     Kskin'
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
              CASE (3)
        write(iout,'(2A)') ' Node  Lay  Row  Col      Rw     B
     &         C          P  '
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
              CASE (4)
        write(iout,'(A)') ' Node  Lay  Row  Col    specCWC'
            END SELECT
c
c     write data depending on LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, write IL,IR,IC only
              CASE (0)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4)')
     +            nod,(INT(MNWNOD(i,INODE)),i=1,3)       
         end do
c     LOSSTYPE=THIEM, write IL,IR,IC,{Rw}
              CASE (1)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P1G11.4)')
     +            nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(5,INODE))
         end do
c     LOSSTYPE=SKIN, write IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P3G11.4)')
     +         nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(j,INODE),j=5,7)
         end do
c     LOSSTYPE=GENERAL, write IL,IR,IC,{Rw B C P}
              CASE (3)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P4G11.4)')
     +       nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(5,INODE)),
     +       (MNWNOD(j,INODE),j=8,10)       
         end do
c     LOSSTYPE=SPECIFYcwc, write IL,IR,IC,{CWC}
              CASE (4)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P1G11.4)')
     +      nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(11,INODE))       
         end do
            END SELECT
c     If PPFLAG>0 print PP input
      else
c     write header depending on LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
              CASE (0)
        write(iout,'(A)') ' Node  Lay  Row  Col        PP'
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
              CASE (1)
        write(iout,'(A)') ' Node  Lay  Row  Col      Rw        PP'
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
        write(iout,'(2A)') ' Node  Lay  Row  Col      Rw     ',
     &'Rskin     Kskin        PP'
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
              CASE (3)
        write(iout,'(2A)') ' Node  Lay  Row  Col      Rw     B
     &         C          P        PP'
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
              CASE (4)
        write(iout,'(A)')' Node  Lay  Row  Col    specCWC        PP'
            END SELECT
c
c     write data depending on LOSSTYPE
            SELECT CASE (NINT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, write IL,IR,IC only
              CASE (0)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,G10.3)')
     +       nod,(INT(MNWNOD(i,INODE)),i=1,3),MNWNOD(19,INODE)       
         end do
c     LOSSTYPE=THIEM, write IL,IR,IC,{Rw}
              CASE (1)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P2G10.3)')
     +        nod,(INT(MNWNOD(i,INODE)),i=1,3),MNWNOD(5,INODE),
     +         MNWNOD(19,INODE)       
         end do
c     LOSSTYPE=SKIN, write IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P4G11.4)')
     +        nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(j,INODE),j=5,7),
     +         MNWNOD(19,INODE)      
         end do
c     LOSSTYPE=GENERAL, write IL,IR,IC,{Rw B C P}
              CASE (3)
         do INODE=firstnode,lastnode
            nod=INODE-firstnode+1
            write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P5G11.4)')
     +        nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(5,INODE)),
     +        (MNWNOD(j,INODE),j=8,10),MNWNOD(19,INODE)       
         end do
c     LOSSTYPE=SPECIFYcwc, write IL,IR,IC,{CWC}
              CASE (4)
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
c-lfk-11/27/2012
          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,ES11.4,2x,F5.2)')
     +      nod,(INT(MNWNOD(i,INODE)),i=1,3),MNWNOD(11,INODE),
     +      MNWNOD(19,INODE)      
         end do
            END SELECT
      end if      
        end if
      end if
c
c   check well nodes in grid
c   Loop over nodes in well
      firstnode= NINT( MNW2(4,MNWID) )
      lastnode = NINT( MNW2(4,MNWID) + ABS(MNW2(2,MNWID)) - UNO )
      do INODE=firstnode,lastnode
        il = NINT( MNWNOD(1,INODE) )             
        ir = NINT( MNWNOD(2,INODE) )             
        ic = NINT( MNWNOD(3,INODE) )

        if(il.lt.1.or.il.gt.nlay.or.
     +     ir.lt.1.or.ir.gt.nrow.or.
     +     ic.lt.1.or.ic.gt.ncol) then
          CALL ERR%ADD(WELLID(MNWID)//
     +       ' AT LAYER, ROW, COL: '//NUM2STR(IL)//', '//NUM2STR(IR)//
     +   ', '//NUM2STR(IC)//'IS LOCATED OUTSIDE OF THE MODEL GRID.'//NL)
        end if
      end do 
c
c     read Data Set 2e, PUMPLOC
c
        IF(PUMPLOC > DZ) THEN
c     if PUMPLOC>0, read PUMPLAY,PUMPROW,PUMPCOL 
          READ(in,*) PUMPLAY,PUMPROW,PUMPCOL    
          MNW2(14,MNWID) = PUMPLAY
          MNW2(15,MNWID) = PUMPROW
          MNW2(16,MNWID) = PUMPCOL 
C--lfk
          WRITE(IOUT,1106) PUMPLAY,PUMPROW,PUMPCOL
 1106 FORMAT(' PUMP LOCATION SET AT LAY = ',I3,
     1       ', ROW = ',I3,', COL = ',I3)
C
        ELSEIF(PUMPLOC.LT.0) THEN
c     if PUMPLOC<0, read Zpump and calulate PUMPLAY,PUMPROW,PUMPCOL
          READ(in,*) Zpump
c         loop over nodes in this well
          firstnode= NINT( MNW2(4,MNWID) )
          lastnode = NINT( MNW2(4,MNWID) + ABS(MNW2(2,MNWID)) - UNO )
          ifound = Z
          DO INODE=firstnode,lastnode
            IL=MNWNOD(1,INODE)
            IF(Zpump.LT.BOTM(IC,IR,LBOTM(IL)-1).AND.
     &         Zpump.GT.BOTM(IC,IR,LBOTM(IL))) THEN
              IR = NINT( MNWNOD(2,INODE) )
              IC = NINT( MNWNOD(3,INODE) )
              MNW2(14,MNWID) = DBLE( IL )
              MNW2(15,MNWID) = DBLE( IR )
              MNW2(16,MNWID) = DBLE( IC )
              ifound = ONE
            END IF
          END DO
c     if PUMPLOC not in a node, assume it is at top and print warning
          if(ifound == Z) then
          CALL WRN%ADD(WELLID(MNWID)//
     +         ' -- PUMP LOCATION SPECIFIED BUT NOT FOUND WITHIN A '//
     +         'MNW2 NODE. PUMP ASSUMED TO BE AT TOP NODE.'//NL)
!              write(IOUT,*) '***WARNING*** Pump location specified but
!     & not found within a MNW2 node'
!              write(IOUT,*) ' Pump assumed to be at top node'
            MNW2(11,MNWID)=0
          end if
C--lfk
          IF(ifound == ONE) THEN
            IL = NINT( MNW2(14,MNWID) )
            IR = NINT( MNW2(15,MNWID) )
            IC = NINT( MNW2(16,MNWID) )
            WRITE(IOUT,1108) zpump,IL,IR,IC
          END IF
 1108 FORMAT(' ZPUMP (Elev.) = ',f9.3,', CONVERTED TO NODE LOCATION: LAY
     1 = ',I3,', ROW = ',I3,', COL = ',I3)
C
        END IF
c
c     end read Data Set 2e
c
c     read data set 2f (if Qlimit > 0)
c
        Qfrcmn = DZ  ! This is used below when not set. RGN 10/1912
        Qfrcmx = DZ
        IF(Qlimit > Z) THEN
          READ(in,*) Hlim,QCUT
          BACKSPACE in
          IF(QCUT /= Z) THEN
            READ(in,*) Hlim,QCUT,Qfrcmn,Qfrcmx
          ELSE
            READ(in,*) Hlim,QCUT
          END IF
c     write info line
c              write(iout,*) 
            IF(MNWPRNT>-1) THEN
              write(iout,'(100A)') ' Qlimit > 0 : this well will ',
     & 'be constrained'
              !
              write(iout,'(A,ES13.5)') '     Hlim = ',Hlim 
              !
              write(iout,1111) QCUT
            ENDIF
 1111 FORMAT('     QCUT = ',I4)
           if(QCUT.lt.0) then 
           IF(MNWPRNT>-1) THEN
             write(iout,'(A,ES13.5,A)') '   Qfrcmn = ',Qfrcmn
             write(iout,'(A,ES13.5,A)') '   Qfrcmx = ',Qfrcmx
           ENDIF
           elseif(QCUT.gt.0) then
            IF(MNWPRNT>-1) THEN
              write(iout,'(A,ES13.5,A)') '   Qfrcmn = ',Qfrcmn, 
     1          ' L**3/T'
            ENDIF
            IF(MNWPRNT>-1) THEN
              write(iout,'(A,ES13.5,A)') '   Qfrcmx = ',Qfrcmx,
     1          ' L**3/T'
            ENDIF
           end if
              write(iout,*) 
c     process min and max Q's based on QCUT
          MNW2(7,MNWID) = Hlim
          MNW2(8,MNWID) = QCUT
          MNW2(9,MNWID) = Qfrcmn
          MNW2(10,MNWID)= Qfrcmx
c     error check Qfrcmn
C-LFK          if(QCUT.GT.0) then
          if(QCUT < Z) then
           if(Qfrcmn > UNO) then
              CALL ERR%ADD(WELLID(MNWID)//
     +        ' -- Qfrcmn MUST BE < 1, BUT FOUND '//NUM2STR(Qfrcmn)//NL)
           end if
c     error check Qfrcmx
           if(Qfrcmx > UNO) then
              CALL ERR%ADD(WELLID(MNWID)//
     +        ' -- Qfrcmx MUST BE < 1, BUT FOUND '//NUM2STR(Qfrcmx)//NL)
           end if
          end if
        END IF
c
c     end read Data Set 2f
c
c     read Data Sets 2g & 2h, PUMPCAP data
c
        IF(PUMPCAP > Z) THEN                                           !seb CHANGED (PUMPCAP.GT.0.0) to (PUMPCAP.GT.0)
c     if PUMPCAP>0, read Hlift, LIFTq0, LIFTqdes
          READ(in,*) Hlift,LIFTq0,LIFTqdes,HWtol   
          mnw2(23,MNWID)=Hlift
          mnw2(28,MNWID)=HWtol
c    CapTable(WELLID,index,type) where
c      index counts the number of values (PUMPCAP+2)
c      type 1 = Lift values
c      type 2 = Q values
          CapTable(MNWID,1,1) = LIFTq0
          CapTable(MNWID,1,2) = DZ
          CapTable(MNWID,PUMPCAP+2,1)=abs(LIFTqdes)
c  when we know qdes, set this
c          CapTable(MNWID,PUMPCAP+1,2)=qdes
          DO 382 index=2,PUMPCAP+1
            READ(in,*) Liftn,Qn              
            CapTable(MNWID,index,1) = LIFTn
            CapTable(MNWID,index,2) = abs(Qn)
 382      CONTINUE
        END IF
c     end read Data Sets 2g & 2h
c
c     check consistency of CapTable
        DO index=2,PUMPCAP
          if(CapTable(MNWID,index,1).GT.CapTable(MNWID,index-1,1)) then
          CALL ERR%ADD(WELLID(MNWID)//
     +  ' -- LIFT VALUES IN CAPACITY TABLE MUST BE IN DESCENDING ORDER'
     +    //NL)
          end if
          if(CapTable(MNWID,index,2).LT.CapTable(MNWID,index-1,2)) then
          CALL ERR%ADD(WELLID(MNWID)//
     +  ' -- Q VALUES IN CAPACITY TABLE MUST BE IN ASCENDING ORDER'//NL)
          end if
        END DO

c
c     end loop over MNWMAX
      END DO READ_MNW_WELLS
C
      !
      DO I=1, MNWMAX
                    J = LEN_TRIM(WELLID(I))
                    !
                    IF(LEN_WELLID < J)  LEN_WELLID = J
      END DO
      ! Get maximum number of nodes
      MXNODE = Z
      DO CONCURRENT (I=1:MNWMAX)
         IF(MNW2(1,I) > -0.5D0) THEN
             INODE = ONE + nint( mnw2(4,i) + abs(mnw2(2,i)) - uno )     !1 + lastnode - firstnode
     +                   - nint( mnw2(4,i) )
            !
            IF(MXNODE < INODE) MXNODE = INODE
         END IF
      END DO
      !
      ! CHECK TO SEE IF ENTIRE WELL IS SCREENED IN IBOUND=0
      DO CONCURRENT (I=1:MNWMAX)
         IF(MNW2(1,I) > -0.5D0) THEN
            FIRSTNODE=NINT( MNW2(4,I) )
            LASTNODE =NINT( MNW2(4,I) + ABS(MNW2(2,I)) - UNO )
            !
            DO J=FIRSTNODE,LASTNODE
                !
                IL = NINT( MNWNOD(1,J) )             
                IR = NINT( MNWNOD(2,J) )            
                IC = NINT( MNWNOD(3,J) )
                !
                IF(IBOUND(IC,IR,IL) /= Z) THEN
                    EXIT
                ELSEIF(J==LASTNODE) THEN
                    CALL WRN3%ADD(TRIM(WELLID(I))//NL)
                    MNW2(1, I) = DNEG
                    MNW2(5, I) = DZ
                    MNW2(18,I) = DZ
                    MNWNOD(4,FIRSTNODE:LASTNODE) = DZ
                END IF
            END DO
         ELSE
                CALL WRN3%ADD(TRIM(WELLID(I))//NL)
                MNW2(5, I) = DZ
                MNW2(18,I) = DZ
                MNWNOD(4,FIRSTNODE:LASTNODE) = DZ
         END IF
            
      END DO
      !
      IF(WRN3%RAISED) CALL WRN3%CHECK(
     +    'MNW2 DEACTIVATED THE FOLLOWING WELLS FOR THE ENTIRE '//
     +    'SIMULATION due to issues with their nodes.'//BLN//
     +    'WELLID', OUTPUT=IOUT)
      !
      IF(WRN%RAISED) CALL WRN%CHECK(
     +      'MNW2 HAD THE FOLLOWING WARNINGS:'//BLN//
     +      'WELLID                  COMMENT',
     +      OUTPUT=IOUT)
      !
      IF(WRN2%RAISED) CALL WRN2%CHECK(
     +      'MNW2 HAD THE FOLLOWING MINOR WARNINGS:'//BLN//
     +      'WELLID                  COMMENT',
     +      OUTPUT=IOUT)
      !
      IF(ERR%RAISED) CALL ERR%CHECK(
     +      'MNW2 HAD THE FOLLOWING FATAL ERRORS:'//BLN//
     +      'WELLID                  COMMENT',
     +      OUTPUT=IOUT, KILL=TRUE)
      !
      IF( PRNT_MNW2%IU.NE.Z .AND. .NOT. PRNT_MNW2%BINARY) THEN
          CALL PRNT_MNW2%SET_HEADER( 
     +     'DATE_START             PER    STP           DELT  '//
     +     'WELLID'//REPEAT(BLNK,LEN_WELLID-5)//
     +     'PUMPING_RATE_INI     PUMPING_RATE        HEAD_WELL')
      END IF
      !
      IF( PRNT_MNW2_Q_INOUT%IU.NE.Z .AND. 
     +                        .NOT. PRNT_MNW2_Q_INOUT%BINARY) THEN
          CALL PRNT_MNW2_Q_INOUT%SET_HEADER(
     +     'DATE_START             PER    STP           DELT  '//
     +     'WELLID'//REPEAT(BLNK,LEN_WELLID-5)//
     +     ' PUMPING_RATE_INI     '//
     +     'PUMPING_RATE          RATE_IN         RATE_OUT        '//
     +     'HEAD_WELL' )
      END IF
      !
      IF( PRNT_MNW2_NODE%IU.NE.Z .AND. 
     +                        .NOT. PRNT_MNW2_NODE%BINARY) THEN
        CALL PRNT_MNW2_NODE%SET_HEADER(
     +   'DATE_START             PER    STP           DELT  WELLID'//
     +   REPEAT(BLNK,LEN_WELLID-6)//
     +   'NODE             RATE        NODE_HEAD        '//
     +   'CELL_HEAD        CELL_BOTM        NODE_COND   '//
     +  'LAY   ROW   COL'  )
      END IF
      !
      IF( PRNT_MNW2_Q_NODE%IU.NE.Z) THEN
          !
          IF(MXNODE < 10     ) THEN
                                 I = 1  !pad size
          ELSEIF(MXNODE < 100) THEN
                                 I = 2
              
          ELSE
                                 I = 3
          END IF
         !
        CALL PRNT_MNW2_Q_NODE%SET_HEADER( 'DATE_START             '//
     +        'PER    STP           DELT  '//
     +        'WELLID'//REPEAT(BLNK,LEN_WELLID-5)//
     +        'PUMPING_RATE_INI'//SEQ2STR('NOD_', MXNODE,17, PAD=I))
      END IF
      !   
      CALL TABFILEPACKINDEX(MNW2TABFILE,NAMELST=WELLID)            !seb BUILD INDEX OF TABFILES TO THE ROW LOCATION OF MNW2 WELLS
      !
      ! LOAD MODEL CELLS THAT WILL BE DESCRIBED BY THE LINE FEED
      IPRT = 0
      IF (MNWPRNT>0) IPRT = 1
      CALL MNW_FEED%CELLS(Z,Z,Z,IPRT,WELLID)     !=>FEED_CELLS(LDIM,NPROP,NAUX,IPRT)
      !
      END SUBROUTINE
      !
      SUBROUTINE GWF2MNW27RP(IN,kper,Iusip,Iude4,Iusor,Iupcg,
     +                    Iulmg,Iugmg,Iupcgn,igwtunit,Iunwt,IUFMP,IGRID)
C     ******************************************************************
c     read mnw2 locations, stress rates, conc, well char., and limits
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD, UPLAY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,WELLID,NTOTNOD,IOUT,
     3                       MNW2TABFILE, MNW_FEED, MNW2BUD, GRPNODE, !seb ADDED TABFILE
     4                       MNW_FMP_LINK,HAS_FMP_WELLS,QDES_OLD
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE SIPMODULE,ONLY:HCLOSE
      USE DE4MODULE,ONLY:HCLOSEDE4
      USE PCGMODULE,ONLY:HCLOSEPCG
      USE GWFNWTMODULE,ONLY:Tol
      USE GMGMODULE,ONLY:HCLOSEGMG
      USE PCGN,ONLY:HCLOSEPCGN
      USE TABLEFILE_INTERFACE,ONLY: TABFILEPACKINDEX,MNW2TABFILE2QDES
      USE ERROR_INTERFACE,    ONLY: STOP_ERROR, WARNING_MESSAGE
      USE FILE_IO_INTERFACE,  ONLY: READ_TO_DATA
      USE STRINGS,            ONLY: JOIN_TXT
      USE NUM2STR_INTERFACE,  ONLY: NUM2STR
      USE CONSTANTS,          ONLY: NL, BLN, TRUE, FALSE, NEG, Z, ONE, 
     +                              DZ, HALF, UNO
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IN,kper,Iusip,Iude4,Iusor,Iupcg,
     +                    Iulmg,Iugmg,Iupcgn,igwtunit,Iunwt,Iufmp,IGRID
      INTEGER:: Qlimit,QCUT
      INTEGER:: firstnode,lastnode
      INTEGER:: PUMPLAY,PUMPROW,PUMPCOL,PUMPLOC,PPFLAG,PUMPCAP
      INTEGER:: IW, ITMP, IERR, MNWID, NAUX, IAUX
      INTEGER:: iread, ifound, index
      DOUBLE PRECISION:: CapMult
      DOUBLE PRECISION:: Rw,Rskin,Kskin,B,C,P,CWC,RwNode
      DOUBLE PRECISION:: RskinNode,KskinNode
      DOUBLE PRECISION:: BNode,CNode,PNode,CWCNode
      DOUBLE PRECISION:: Ztop,Zbotm,Zbotmlast,Zpump
      DOUBLE PRECISION:: Hlim,Qfrcmn,Qfrcmx,Qdes,Cprime,PP
      DOUBLE PRECISION:: Qtemp,Hlift,LIFTq0,LIFTqdes,LIFTn,Qn,HWtol
      CHARACTER(20):: WELLNAME,LOSSTYPE
      INTEGER:: TABFOUND                                                !seb
      !CHARACTER(768):: LINE
      LOGICAL:: EXIST
      LOGICAL, SAVE:: READ_ITMP = TRUE
C
      CALL SGWF2MNW2PNT(IGRID)
c
c     read Data Set 3, ITMP (number of wells or flag saying reuse well data)
c
c  Skip to here unless in first SP
      IF(KPER==1 .AND. IUFMP.NE.0) HAS_FMP_WELLS = ANY(MNW_FMP_LINK)
!  888 CONTINUE
      !
      IF(MNW2TABFILE%NTAB.GT.0)MNW2TABFILE%TAB%USEVAL= FALSE           !seb TELL TABFILEINTERP TO SEARCH FOR NEW TIME AND THEN START REUSING VALUES
      !
      IERR = Z
      ITMP = Z
      IF(READ_ITMP) READ(in,*, IOSTAT=IERR) ITMP
      !
      IF (IERR /= Z) THEN
          READ_ITMP = FALSE
          itmp = Z
          !
          IF(MNW_FEED%NFEED<1 .AND. .NOT. HAS_FMP_WELLS) THEN
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     + 'MNW2 ERROR:  FAILED TO READ STRESS PERIOD VARIABLE "ITMP".')
          END IF
          !
          WRITE(IOUT,'(/A)')
     +    "MNW2 PACKAGE WARNING (don't panic): FAILED TO READ "//
     +      'ITMP (STRESS PERIOD INPUT).'//NEW_LINE(' ')//
     +    'THIS MAY BE DUE TO THE END OF THE FILE BEING REACHED IN '//
     +      'THE MNW2 PACKAGE INPUT FILE.'//NEW_LINE(' ')//
     +    'ITMP IS SET TO ZERO FOR THE REST OF THE SIMULATION.'//
     +      NEW_LINE(' ')//
     +    'IF YOU ARE USING LINEFEED OR FMP LINK, THEN THOSE RATES'//
     +      ' ARE STILL APPLIED AND THE PACKAGE WILL OPERATE NORMALLY.' 
      END IF
      !
      !READ IN NEXT LINE IN LINE_FEED FILE WHICH CONTAINS THE CURRENT STRESS PERIODS DATA
      CALL MNW_FEED%NEXTLINE()
      !
        write(iout,*) 
c     return if there are no wells to read ........
      if( itmp >= Z ) then
c     reset # of wells and active well flag
         nmnw2 = Z
         do concurrent (iw=1:MNWMAX, .not. MNW_FMP_LINK(iw) 
     +                               .AND. MNW2(1,iw) > -0.5D0)
           MNW2(1,iw) = DZ
         end do
         itmp = itmp + MNW_FEED%NACT
         if(itmp == Z) return
      end if    
      if( itmp < Z ) then
c        if itmp less than zero, reuse data. print message and return.
        !
        ! Reset QDES to original values...in case other packages modified it.
        !
        DO iw=1, MNWMAX
              !
              firstnode= NINT( MNW2(4,iw) )
              lastnode = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
              !
              IF(MNW2(1,IW) > HALF) THEN
                  !
                  MNW2(5, iw) = QDES_OLD(iw)
                  MNW2(18,iw) = QDES_OLD(iw)
                  !
                  MNWNOD(4,firstnode:lastnode)=DZ
                  MNWNOD(4,firstnode)         =QDES_OLD(iw)
              ELSE
                  MNW2(5, iw) = DZ
                  MNW2(18,iw) = DZ
                  MNWNOD(4,firstnode:lastnode)=DZ
              END IF
        END DO
        !
        IF(MNW2TABFILE%NTAB > Z) THEN                                  !seb IF USING TABFILES UPDATE TABINFORMATION
          DO IW=1,MNWMAX
            IF(MNW2(1,IW) > HALF) THEN
              CALL MNW2TABFILE2QDES(MNW2TABFILE,IW,QDES,TABFOUND)       !seb IF TABFILES THEN CHECK IF QDES IS RELATED TO 1 TABFILE. IF IT IS THEN UPDATE IT WITH TABIFORMATION
              IF(TABFOUND > Z)THEN
                QDES_OLD(IW)= Qdes
                MNW2(5, IW) = Qdes
                MNW2(18,IW) = Qdes 
                firstnode= NINT( MNW2(4,iw) )
                lastnode = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
                MNWNOD(4,firstnode:lastnode)=DZ                         !seb ZERO OUT ALL NODES AND PUT DESIRED INTO FIRST NODE
                MNWNOD(4,firstnode)         =Qdes                                !ASSIGN FLOW TO TOP NODE
              END IF
            END IF
          END DO
        END IF
        !
        !CALL MNW_FEED%FEED_MNW2( MNW2,MNWNOD,CapTable,0)
        !
          write(iout,6)
    6   format('REUSING MNW2 INFORMATION FROM LAST STRESS PERIOD')
        !
        IF(MNW_FEED%NFEED>0) THEN
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     + 'MNW2 ERROR:  LINEFEED DOES NOT WORK WITH REUSING PREVIOUS '//
     + 'STRESS PERIODS INFORMATION (ITMP<0). FOUND ITMP = '
     + //NUM2STR(ITMP))
        END IF
        
        return
      else
c  If itmp > 0, read ITMP wells
c
       !
       !First set all pumping rates to zero, since they will be updated
       !
       QDES_OLD = DZ
       DO CONCURRENT(iw=1:MNWMAX)
                                 MNW2(5,iw) = DZ
       END DO
       DO CONCURRENT(iw=1:NODTOT)
                                 MNWNOD(4,iw) = DZ
       END DO
       !   
       if(itmp > ONE) then
          write(iout,*)'MNW2: ',ITMP, ' active wells in stress period ',
     &                  kper
       else
          write(iout,*) 'MNW2: ',ITMP, ' active well in stress period ',
     &                   kper
       end if
       write(iout,*) 
        !
        !CALL MNW_FEED%FEED_MNW2( MNW2,MNWNOD,CapTable,0)
        itmp = itmp - MNW_FEED%NACT
        nmnw2=nmnw2+MNW_FEED%NACT
        !
        do iread=1,ITMP
          Cprime   = DZ                                             !seb Initialize to a value
          TABFOUND = Z                                              !seb FLAG TO INDICATE IF WELL WAS DEFINED WITH A TABFILE
c  read data set 4a
c  read WELLNAME & Qdes and then backspace to check for PUMPCAP
c-lfk    read(in,*) WELLNAME
         read(in,*) WELLNAME,qdes
         backspace(in)
         call UPCASE(WELLNAME)
c  check for existence of well
         EXIST  = FALSE
         ifound = Z
         MNWID  = Z
         do iw=1,MNWMAX
           if(WELLID(iw) == WELLNAME) then
              ifound=ONE
c             set IACTIV=1 to turn well on for this SP
              IF(MNW2(1,iw) > -0.5D0) THEN
                  MNW2(1,iw)=UNO
                  nmnw2 = nmnw2 + ONE
                  EXIST = TRUE
              END IF
              MNWID=iw
C-LFK
             IF(MNWPRNT>-1) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,*)'WELLNAME = ',WELLNAME
             END IF
             EXIT
           end if
         end do
         if (ifound == Z) then
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +'MNW2 ERROR: FAILED TO LOCATE WELLID "'//TRIM(WELLNAME)//'"'//NL//
     +  'WITHIN THE LIST OF DEFINED MNW2 WELLIDs.'//BLN//
     +  'THE FOLLOWING ARE THE WELLIDs THAT WERE DEFINED:'//BLN//
     +   JOIN_TXT(WELLID(:MNWMAX),', '))
         end if
         !
         IF(MNW2TABFILE%NTAB > Z .AND. EXIST)                      ! IF TABFILES THEN CHECK IF QDES IS RELATED TO 1 TABFILE. IF IT IS THEN UPDATE IT WITH TABIFORMATION
     +            CALL MNW2TABFILE2QDES(MNW2TABFILE,MNWID,QDES,TABFOUND)
         !
c   continue reading data set 4a        
         PUMPCAP=MNW2(22,MNWID)
         NAUX=NMNWVL-30
         if(PUMPCAP == Z) then
          if(igwtunit == Z) then
            read(in,*) WELLNAME,Qdes,(MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
          else
c-lfk:  Only read Cprime for recharge/injection well (Qdes.gt.0.0)
            if (Qdes > DZ) then
              read(in,*) WELLNAME,Qdes,Cprime,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            else
              read(in,*) WELLNAME,Qdes,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            end if
           endif
          else
          if(igwtunit == Z) then
            read(in,*) WELLNAME,Qdes,CapMult,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
          else
c-lfk:  Only read Cprime for recharge/injection well (Qdes.gt.0.0)
            if (Qdes > DZ) then
              read(in,*) WELLNAME,Qdes,CapMult,Cprime,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            else
              read(in,*) WELLNAME,Qdes,CapMult,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            end if
         endif
         IF(MNW2TABFILE%NTAB > Z .AND. EXIST)   !seb IF TABFILES THEN CHECK IF QDES IS RELATED TO 1 TABFILE. IF IT IS THEN UPDATE IT WITH TABIFORMATION
     +            CALL MNW2TABFILE2QDES(MNW2TABFILE,MNWID,QDES,TABFOUND)
C-LFK (2/13) Check consistency of Qdes with CapTable values for Q
         if (CapMult > DZ .and. Qdes < DZ .and. EXIST) then
         index = pumpcap + ONE
          if(abs(Qdes) < CapTable(MNWID,index,2)*CapMult) then
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +'MNW2 ERROR: Qdes (Qmax) value "'//NUM2STR(abs(Qdes))//'" < '//
     +'CAPACITY TABLE VALUE "'//NUM2STR(CapTable(MNWID,index,2)*CapMult)
     +     //'".'//NL//'VALUES MUST BE IN ASCENDING ORDER.'//NL//
     +       'FOR WELLID: '//WELLID(MNWID))
          end if
         end if
          end if
C-LFK   write auxiliary variable info.
         if (naux > Z .and. mnwprnt > Z) then
              write(iout,1900) (mnwaux(iaux),iaux=1,naux)
              write(iout,1910) (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
 1900 format(2x,'Auxiliary Variable Information:  ',5(:(A16,2x)))
 1910 format(35x,5(:(f4.0,14x)))
         end if
c     read Data Set 4b, if Qlimit < 0
c
        Qlimit = NINT( MNW2(6,MNWID) )
        IF(Qlimit < Z) THEN
          READ(in,*) Hlim,QCUT
          BACKSPACE in
          IF(QCUT /= Z) THEN
            READ(in,*) Hlim,QCUT,Qfrcmn,Qfrcmx
          ELSE
            READ(in,*) Hlim,QCUT
            Qfrcmn=DZ
            Qfrcmx=DZ
          END IF
c     write info line
c              write(iout,*) 
            IF(MNWPRNT>-1) THEN
              write(iout,'(100A)') ' Qlimit < 0 : this well will
     & be constrained'
              write(iout,'(A,ES13.5)') '     Hlim = ',Hlim 
              write(iout,2111) QCUT
            ENDIF
 2111 FORMAT('     QCUT = ',I4)
           if(QCUT < Z) then 
           IF(MNWPRNT>-1) THEN
             write(iout,'(A,ES13.5,A)') '   Qfrcmn = ',Qfrcmn
             write(iout,'(A,ES13.5,A)') '   Qfrcmx = ',Qfrcmx
           ENDIF
           elseif(QCUT > Z) then
            IF(MNWPRNT>-1) THEN
              write(iout,'(A,ES13.5,A)') '   Qfrcmn = ',Qfrcmn,
     1          ' L**3/T'
              write(iout,'(A,ES13.5,A)') '   Qfrcmx = ',Qfrcmx,
     1          ' L**3/T'
            ENDIF
           end if
            IF(MNWPRNT>-1) THEN
              write(iout,*) 
            ENDIF
c     process min and max Q's based on QCUT
          MNW2(7,MNWID)=Hlim
          MNW2(8,MNWID)=QCUT
          if(QCUT == Z) then
            Qfrcmn = DZ
            Qfrcmx = DZ
          end if 
          MNW2(9,MNWID) =Qfrcmn
          MNW2(10,MNWID)=Qfrcmx
c     error check Qfrcmn
          if(QCUT < Z) then
           if(Qfrcmn > UNO) then
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +           'MNW2 ERROR: Qfrcmn MUST BE < 1, BUT FOUND '//
     +            NUM2STR(Qfrcmn)//NL//'FOR WELLID: '//WELLID(MNWID))
           end if
           if(Qfrcmn > Qfrcmx) then
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +           'MNW2 ERROR: Qfrcmn = '//NUM2STR(Qfrcmn)//NL//
     +           '  MUST BE < Qfrcmx = '//NUM2STR(Qfrcmx)//NL//
     +           'ERROR FOUND FOR WELLID: '//WELLID(MNWID))
           end if
c     error check Qfrcmx
           if(Qfrcmx > UNO) then
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +           'MNW2 ERROR: Qfrcmx MUST BE < 1, BUT FOUND '//
     +            NUM2STR(Qfrcmx)//NL//'FOR WELLID: '//WELLID(MNWID))
           end if
         end if
         END IF
c
c     end read Data Set 4b
         IF(.not. EXIST) THEN
             QDES_OLD(MNWID)= DZ
             MNW2(5, MNWID) = DZ
             MNW2(18,MNWID) = DZ
             MNW2(25,MNWID) = DZ
             MNW2(27,MNWID) = DZ  
             write(iout,2114) WELLNAME,kper
             CYCLE
         END IF
c
c        set desired flow rate for well
         QDES_OLD(MNWID)= Qdes
         MNW2(5, MNWID) = Qdes
         MNW2(18,MNWID) = Qdes
c        if transport, set cprime for well
         if(igwtunit.NE.0) MNW2(12,MNWID)=Cprime
c        set all Qact (actual flow rate at node) = Qdes/NNODES as launching point
c        Loop over nodes in well
         firstnode= NINT( MNW2(4,MNWID) )
         lastnode = NINT( MNW2(4,MNWID) + ABS(MNW2(2,MNWID)) - UNO )
c          put total Qdes in first node, allow it to go from there
         MNWNOD(4,firstnode:lastnode)=DZ                               !seb ZERO OUT ALL NODES AND PUT DESIRED INTO FIRST NODE
         MNWNOD(4,firstnode)         =Qdes                                       !seb CHANGED BACK TO FIRSTNODE, FIRST CHANGE WAS FROM MNWNOD(4,firstnode)=Qdes to MNWNOD(4,lastnode)=Qdes to prevent pumping from occuring in a dry cell
         !
         IF(TABFOUND.EQ.0 .AND. MNWPRNT>-1)THEN                         !seb ADDED ' [Q Derived from Tabfile]'
           write(iout,2112) WELLNAME,Qdes,kper
         ELSEIF(MNWPRNT>-1) THEN
           write(iout,2113) WELLNAME,Qdes,kper
         END IF
 2112 FORMAT('MNW2 Well ', A20,' active, desired Q =',
     + ES12.4,' for stress period ',I4)
 2113 FORMAT('MNW2 Well ', A20,' active, desired Q =',
     + ES12.4,' for stress period ',I4,' [Q Derived from Tabfile]')
 2114 FORMAT('MNW2 Well ', A20,' was specified, but a previous ',
     + 'stress period disabled it for the rest of the simulation, ',
     + 'so Q=NaN for stress period ',I4)
         if(PUMPCAP.GT.0) then
c  if Qdes is not negative for this stress period, do not apply pump capacity restraints
           if(Qdes > DZ) then                                        !seb (Qdes.GE.0.d0) CHANGED TO (Qdes.GT.0.d0)
              MNW2(25,MNWID)=DZ                                          !Initialize CapFlag2
              mnw2(27,MNWID)=DZ
           else
             IF(Qdes == DZ)THEN                                        !seb ALLOW HOLDING OF CAPMULT FOR Qdes=0 BUT DO NOT ENABLE CAPACITY CONTSTRAINTS TO FACILITATE LINK TO FMP
              MNW2(25,MNWID)=DZ
              MNW2(27,MNWID)=DZ                                          !Initialize CapFlag2
             ELSE
              MNW2(25,MNWID)=UNO
c  only set Qdes at upper end of CapTable if not set already
             if (CapTable(MNWID,PUMPCAP+2,2) <= DZ) then
               CapTable(MNWID,PUMPCAP+2,2)=abs(qdes)
             end if
             END IF
             MNW2(24,MNWID)=CapMult
             IF(MNWPRNT>-1) THEN
               write(iout,*) 
               write(iout,1114) mnw2(23,MNWID)
 1114 FORMAT('Reference head for calculating lift = ', ES12.4)
               write(iout,1113) CapMult
 1113 FORMAT('Pump Capacity Multiplier = ', ES12.4)
               if(mnwprnt.gt.0) write(iout,1112) mnw2(28,MNWID)
 1112 FORMAT('HWtol = ', ES12.4)
               if(mnwprnt.gt.1) write(iout,1115) 
             ENDIF
 1115 FORMAT(5x,'(Note: Solution may be sensitive to value of HWtol;',
     *' adjust value if solution fails to converge.)')
c   zero capacity use flag if CapMult=0
             if( CapMult == DZ) then
              MNW2(25,MNWID)=DZ
c   Initialize CapFlag2
              mnw2(27,MNWID)=DZ
           end if
c  now that we have Qdes, write Capacity table
            IF(Qdes < DZ)THEN
             IF(MNWPRNT>-1) THEN
               write(iout,*) 
               write(iout,*) 'Well Capacity Table'
               write(iout,*) ' Lift     Discharge'
             do index=1,PUMPCAP+2
                 write(iout,'(1x,ES12.5,G11.4)')
     &              CapTable(MNWID,index,1), CapTable(MNWID,index,2)
             end do
             ENDIF
            END IF
          end if
        end if
       end do
       !
       IF(MNWPRNT>-1) write(iout,*)
       !
       IF(MNW_FEED%NFEED > Z) THEN
                  write(iout,'(A)')'FEED FILES WILL NOW UPDATE Qdes'
       ENDIF
       !
       CALL MNW_FEED%FEED_MNW2( MNW2,MNWNOD,CapTable,Z)
       !
      end if
      return
c
      end
C
      SUBROUTINE GWF2MNW27AD(kstp,kper,IGRID)
C     ******************************************************************
c     Update Qact for wells that were constrained
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,DELR,DELC,CV,LAYHDT,
     2                       STRT,HNEW
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
C-LFK     2                       CapTable,SMALL,WELLID
     2                       CapTable,SMALL,WELLID,LIMQ,MNW2TABFILE,    !seb ADD TABFILE
     3                       IOUT,HAS_NWT
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE GWFBASMODULE, ONLY:TOTIM                                      !seb
      USE TABLEFILE_INTERFACE,ONLY: MNW2TABFILE2QDES                    !seb
      USE ERROR_INTERFACE, ONLY: WARNING_MESSAGE
      USE CONSTANTS,       ONLY: DZ, HALF, UNO, DNEG, Z, ONE, 
     +                           TRUE, FALSE, 
     +                           NEGNEARZERO_20, NEARZERO_20
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: KSTP, KPER, IGRID
      INTEGER:: firstnode, lastnode, nd
      DOUBLE PRECISION:: qoff, qon, qdes, qact, qpot, qnet, Qsmall
      DOUBLE PRECISION:: csum, chsum, cond, ratio, SGN
      DOUBLE PRECISION:: hwell, hlim, hmax, hsim, Hcell
      INTEGER:: TABFOUND,TABIDX
      INTEGER:: NNODES, INODE, IW, ITFLAG, kiterTMP
      INTEGER:: IL, IR, IC
      INTEGER:: Qlimit, QlimFlag, QCUT, CapFlag2
      LOGICAL:: CHKER, HASQ
C
      CALL SGWF2MNW2PNT(IGRID)
C
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
      !
      IF(MNW2TABFILE%NTAB > Z) THEN                                     !seb IF USING TABFILES UPDATE TABINFORMATION
        IF(.NOT. MNW2TABFILE%SPBASIS) MNW2TABFILE%TAB%USEVAL=FALSE      !TELL TABFILEINTERP TO SEARCH FOR NEW TIME AND THEN START REUSING VALUES --DO NOT SEARCH IF USING SAME VALUE FOR SP
        IF(MNW2TABFILE%TABPRINT)THEN                                    !PRINT OUT HEADER TO IOUT
       WRITE(IOUT,'(/A,ES16.9)')'MNW2 TABFILE PRINT OUT FOR TIME ',
     +                           TOTIM
       WRITE(IOUT,'(2A)')'     WELLNAME       Q                 ',
     +                   'TSFAC  TABNAM'
        END IF
        DO IW=1,MNWMAX
          IF(MNW2(1,IW) > HALF) THEN
            CALL MNW2TABFILE2QDES(MNW2TABFILE,IW,QDES,TABFOUND)         !IF TABFILES THEN CHECK IF QDES IS RELATED TO 1 TABFILE. IF IT IS THEN UPDATE IT WITH TABIFORMATION
            IF(TABFOUND > Z)THEN
              MNW2(5, IW)=Qdes
              MNW2(18,IW)=Qdes
              firstnode = NINT( MNW2(4,IW) )
              lastnode  = NINT( MNW2(4,IW) + ABS(MNW2(2,IW)) - UNO )
              MNWNOD(4,firstnode:lastnode)=0.0                          !ZERO OUT ALL NODES AND PUT DESIRED INTO FIRST NODE
              MNWNOD(4,firstnode)=Qdes                                  !ASSIGN FLOW TO TOP NODE
              IF(MNW2TABFILE%TABPRINT)THEN
                TABIDX=MNW2TABFILE%TABIDX(TABFOUND)
                WRITE(IOUT,'(A,ES16.9,ES15.8,2x,A)')WELLID(IW),
     +                       Qdes,MNW2TABFILE%TSFAC(TABFOUND),
     +                       MNW2TABFILE%TABNAM(TABIDX)
              END IF
            END IF
          END IF
        END DO
      END IF
      !
      ! Reset nodal flag
      !
      nd =NINT( MNW2(4,MNWMAX) + ABS(MNW2(2,MNWMAX)) - UNO )  !Get position of the last node simulated
      !
      DO CONCURRENT (INODE=1:nd)
                               MNWNOD(22,INODE) = UNO   !Set all nodes to active (note MNW2(1,:) supercedes this flag)
      END DO
c
c   Compute cell-to-well conductance for each well node
c
c   Send in ITFLAG=0, this means don't calculate partial penetration effects
c   because we are not currently iterating on a solution
      ITFLAG = Z
c   sending in kiter=0; this is before iter loop
      kiterTMP  = Z
      call SMNW2COND(IGRID,kstp,kper,kiterTMP,ITFLAG)
c
c
c   Allow constrained wells a new chance with the next time step
c
c   Loop over all wells
      do iw=1,MNWMAX                    !   Only operate on active wells (MNW2(1,iw)=1)
        NNODES = ABS(NINT(MNW2(2,iw)))
        if (MNW2(1,iw) > HALF .AND. NNODES > Z) then
          !
          firstnode = NINT( MNW2(4,iw) )
          lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
          hwell    = mnw2(17,iw)
          qdes     = mnw2(5,iw)
          qnet     = mnw2(5,iw)            !  Define qnet for well, which may be updated if well was restricted in previous time step
          Qlimit   = NINT(MNW2(6, iw))
          QCUT     = NINT(MNW2(8,iw))
          QlimFlag = NINT(MNW2(20,iw))
          CapFlag2 = NINT(MNW2(27,iw))
          !
          HasQ     = abs(qdes) > NEARZERO_20
C  SET Q
c  if qlimit was hit in a previous FM routine, set qnet to updated Q, then reset flags
          if(QlimFlag /= Z .or. CapFlag2 > Z) then
            qnet        = mnw2(18,iw)
            mnw2(20,iw) = DZ
            mnw2(27,iw) = DZ
c  default Q used = Qdes (this may be changed if limits kick in below)
          else
            mnw2(18,iw) = qdes
          end if
c
C-LFK Nov.2012
C   Initialize LIMQ flags
          LIMQ(1,IW) = Z
          LIMQ(2,IW) = Z
          LIMQ(3,IW) = Z
C        
c   Retrieve QCUT, Qfrcmn, Qfrcmx
          if(Qlimit /= Z) then
            qoff=DZ
            qon =DZ
            if(QCUT /= Z) then
             qoff = mnw2(9,iw)
             qon  = mnw2(10,iw)
             if(QCUT > Z) then           ! convert rate into fraction of Qdes (ratio is used to compare)
              if( HasQ ) then
                qoff = abs(qoff/Qdes)
                qon  = abs(qon/Qdes)
              else
                qoff=DZ
                qon =DZ
              end if
             end if
            end if
          end if
c
c   Compute hwell / Qpot for multi-node well (not single-cell wells)
          !
          csum  = DZ
          chsum = DZ
          qact  = DZ
          Qsmall = small*abs(qdes)
          !
c   Loop over nodes in well
          do INODE=firstnode, lastnode
            il = NINT( MNWNOD(1,INODE) )             
            ir = NINT( MNWNOD(2,INODE) )             
            ic = NINT( MNWNOD(3,INODE) )
            !
            Hcell = HNEW(IC,IR,IL)
            !
            IF(IBOUND(ic,ir,il) .ne. Z) THEN
              !
              IF(.NOT. HAS_NWT .OR. LAYHDT(IL) == Z) THEN
                      !
                      CHKER = .TRUE.
                      !
              ELSEIF( LAYHDT(IL) > Z .AND. 
     +                HNEW(IC,IR,IL) > BOTM(IC,IR,LBOTM(IL)) ) THEN
                      !
                      CHKER = .TRUE.
              ELSE
                      CHKER = .FALSE.
              END IF
              !
              IF(CHKER) THEN
                       csum  = csum  + MNWNOD(14,INODE)
                       chsum = chsum + MNWNOD(14,INODE)*Hcell
                       qact  = qact  + MNWNOD(4, INODE)
              END IF
            END IF
            !
C-LFK   Check for MNW & specified-head boundary condition in same cell; 
C       print warning if found
            if(ibound(ic,ir,il) < Z) then
               nd=INODE-firstnode+1
               CALL WARNING_MESSAGE(OUTPUT=IOUT,
     +      MSG='MNW2 WELLID '//WELLID(IW)//': SPECIFIED-HEAD '//
     +      'CONDITION (IBOUND<0) SHOULD NOT EXIST IN SAME CELL AS A '//
     +      'MULTI-NODE WELL.', INLINE=.TRUE.)
            end if
          end do
c---div0 ---  CSUM could go to zero if the entire well is dry
          if( csum .gt. NEARZERO_20 ) then   !  for limit procedure, use qnet here which may have been restricted in previous time stephwell = ( qdes + chsum ) / csum
            hwell = ( qnet + chsum ) / csum
          else
            hwell = Hcell
          endif
c      Test Hlim constraint if QLIMIT flag is set
          if(Qlimit /= Z) then
            !  
            hlim = mnw2(7,iw)
            !
            if    ( qdes < NEGNEARZERO_20) THEN
                                            SGN = DNEG
            ELSEif( qdes >    NEARZERO_20) THEN
                                            SGN = UNO
            ELSE
                                            SGN = DZ
            END IF
            !
            hmax = SGN * hlim 
            hsim = SGN * hwell 
c      Potential Q is...
            if( hsim > hmax )  hwell = hlim
            !
            qpot = hwell*csum - chsum
          end if
          !
          cond = csum
          !
          if(cond < DZ) then     !      check cond<0, reset to 0 and print warning
              CALL WARNING_MESSAGE(OUTPUT=IOUT,
     +    MSG='MNW2 WELLID '//WELLID(IW)//': CWC<0 IN WELL, '//
     +    'SETTING CWC TO ZERO', INLINE=.TRUE.)
              cond=0.d0
          end if
          !
c  Compute ratio of potential/desired flow rates
          if(QCUT /= Z) then
           ratio = UNO
           if( HasQ ) ratio =  qpot / qdes
           if( ratio .gt. 0.9999D0 ) then
                                     ratio =  UNO
                                     Qpot  = Qdes
           endif
           if( ratio .lt. Qoff) then
                  mnw2(30,iw) = DZ
                  MNWNOD(4,firstnode:lastnode) = DZ
           elseif( ratio.gt.Qon .and. abs(qact).lt.Qsmall ) then
                  mnw2(30,iw) = Qpot
                  MNWNOD(4,firstnode:lastnode) = DZ
                  MNWNOD(4,firstnode)          = Qpot
           endif
c  with QCUT=0 now, set q=qpot if neither situation holds?
           if(QCUT.EQ.0.and.ratio.gt.0.D0) then
                  mnw2(30,iw) = Qpot
                  MNWNOD(4,firstnode:lastnode) = DZ
                  MNWNOD(4,firstnode)          = Qpot
           end if
c  End if, active wells
          end if
        end if
c  End do, loop over all wells
      end do
c
      RETURN
      END SUBROUTINE
C
      SUBROUTINE GWF2MNW27FM(KITER,kstp,kper,mxiter,IGRID)
C     ******************************************************************
C     ADD MNW2 TERMS TO RHS AND HCOF
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,DELR,DELC,LAYHDT,RHS,HCOF,
     2                       HNEW,CV,STRT
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
C-LFK     2                       CapTable,SMALL,WELLID
     2                       CapTable,SMALL,WELLID,LIMQ,
     3                       IOUT,MNW2_WARN,SOLV_FLG,SOLV_BY_HCOF,
     4                       HCOF_ONLY_ITER,HCOF_RHS_FLIP,HAS_NWT
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE CONSTANTS,     ONLY: HALF, DZ, UNO, Z, ONE, TWO, NEARZERO_20
C     ------------------------------------------------------------------
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: KITER,kstp,kper,mxiter,IGRID
      !
      INTEGER:: NNODES, INODE
      INTEGER:: IQSLV
      INTEGER:: firstnode, lastnode, IW, IL, IR, IC
      INTEGER:: PUMPCAP, CapFlag 
      INTEGER:: ITFLAG                                  !=1, this means calculate partial penetration effects
      DOUBLE PRECISION:: qdes, qact, cond
      DOUBLE PRECISION:: hwell, Hcell, hlim
      DOUBLE PRECISION:: CapMult, lastQ, qtemp, qactCap
      DOUBLE PRECISION:: dhc2w, hmax, hsim, ratio, Hlift
      DOUBLE PRECISION:: temppct, lastH, htemp, HWtol
      LOGICAL:: EVEN_ITER, SKIP_NODE, QLIMIT, QEXTRACT, HasQ
C
      CALL SGWF2MNW2PNT(IGRID)
c
c                 CR( i, j, k)    ------>   CR  i + 1/2
c                 CC( i, j, k)    ------>   CC  j + 1/2
c                 CV( i, j, k)    ------>   CV  k + 1/2
c
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
      !
      EVEN_ITER = mod(kiter,2) == Z
C
      CALL MNW2_WARN%INIT()
      !
      !CHECK FOR SOLVER FLIP ITERATIONS
      !
      IF(HCOF_RHS_FLIP > Z .AND. KITER < HCOF_ONLY_ITER) THEN
          !
          IF(MOD(KITER,HCOF_RHS_FLIP) == Z) THEN
              IF( SOLV_FLG == ONE ) THEN
                  SOLV_FLG  = TWO
              ELSE
                  SOLV_FLG  = ONE
              END IF
          END IF
      END IF
      !
      ! ONLY USE HCOF FOR REST OF SIMUALTION
      !
      IF(HCOF_ONLY_ITER < KITER ) SOLV_FLG = TWO
c
c   Compute cell-to-well conductance for each well node
c
c   Send in ITFLAG=1, this means calculate partial penetration effects
c   because we are currently iterating on a solution
      ITFLAG = ONE
      call SMNW2COND(IGRID,kstp,kper,kiter,ITFLAG)
c
c   Prepare components and limits of a multi-node well
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
        if (MNW2(1,iw) > HALF) then
c   Check capacity calculation flag
         PUMPCAP  = NINT(MNW2(22,iw))
         CapMult  = MNW2(24,iw)
         qdes     = mnw2(5,iw)
         Qextract = qdes < DZ
         CapFlag  = NINT(mnw2(25,iw))
c   If CapFlag was turned off (e.g. with % Q check), turn back on at beginning of new time step (kiter=1)
         if(kiter == ONE .and. PUMPCAP > Z .and. CapMult .ne. DZ
     +     .and. Qextract) then
                           MNW2(25,iw) = UNO
                           CapFlag     = ONE
         end if
c  compute well capacity restraint
c         if capacity should be calculated, wait until after first iteration so
c         the lift can be computed with a new hwell (kiter>1)
          if(CapFlag > Z .and. kiter > ONE) then
            hwell = mnw2(17,iw)
c           save hwell before update in SEEP routine, to check vs HWtol later
            lastH = hwell
            Hlift = MNW2(23,iw)
c           call MNW2CAPACITY to look up Q based on capacity table (returns qactCap)
            call MNW2CAPACITY(IGRID,qactCap,iw)
c           get last Q that was looked up in table; this will be used to see if
c           new Q is less than 1% or more than 25% from new Q
            lastQ = mnw2(26,iw)
c           save last Q that was looked up in table for next iteration
            mnw2(26,iw) = qactCap
c           only do following if capacity Q is less than Qdes
            if (abs(qactCap) < abs(mnw2(5,iw))) then
c            if capacity Q from table is zero at beginning first time step, 
c            (kiter=2 is first time here due to kiter>1 check above)
c            set qpot to 0; "allow" to go to zero
             if(qactCap == DZ .and. kiter == TWO .and. kstp > 1) then
              mnw2(29,iw) = DZ
c             set CapFlag2 to true, this is used in AD routine to say that the
c             Q has been constrained
              mnw2(27,iw) = UNO
             else
c             only perform % checks after second iteration (lets soln get going)
              if(kiter > TWO) then
c              % check divides by lastQ, so avoid this if it is zero
               if(abs(lastQ) > small) then
c               this is the percentage change, below which the Q will be locked in
c               for the time step
                temppct=0.01d0
c               calculated % change in Q looked up in capacity tables
                qtemp=abs(qactCap-lastQ)/abs(lastQ)
c
c               if Q is changing more than 1%, continue
                if (qtemp > temppct) then
c                 if Q is changing more than 25% after 1st TS, constrain change
c                 in Q to 25% increase/decrease
                  if(qtemp > 0.25d0 .and. kstp > ONE) then
c                   check to see which way Q is moving; adjust accordingly
                    if(lastQ < qactCap) then
                      mnw2(29,iw)=lastQ*0.75d0
                    else
                      mnw2(29,iw)=lastQ*1.25d0
                    end if
c                   set CapFlag2 to true, this is used in AD routine to say that the
c                   Q has been constrained
                    mnw2(27,iw) = UNO
c                   save last Q
                    mnw2(26,iw)=mnw2(29,iw)
c  write message that Q was constrained
                    if(mnwprnt.gt.1) then
       write(iout,*) 'Capacity Q was changing more than 25%, constrained
     + to 25% increase/decrease for well ',WELLID(iw)
        write(iout,*) '   with Pump-Capacity Q = ',qactCap,' iter.no. ',
     +kiter
                    end if
                  else
c               if Q is changing less than 1%, set equal to Q just looked up
                    mnw2(29,iw) = qactCap
c                   set CapFlag2 to true, this is used in AD routine to say that the
c                   Q has been constrained
                    mnw2(27,iw) = UNO
                  end if
                else
c  if Qcap not changing more than 1%, stop doing capacity constraint
                  mnw2(29,iw) = qactCap
c  set CapFlag to false to discontinue doing capacity checks this TS
                  mnw2(25,iw) = DZ
c                 set CapFlag2 to true, this is used in AD routine to say that the
c                 Q has been constrained
                  mnw2(27,iw) = UNO
c  write message that Q was constrained
                  if(mnwprnt > ONE) then
      write(iout,*) 'Capacity Q was changing less than 1%; capacity calc
     +ulations stopped this TS for well ',WELLID(iw)
      write(iout,*)' with Pump-Capacity Q = ',qactCap,' iter.no. ',kiter
                  end if
                end if
c              else lastQ=0               
               else
c                if lastQ is zero, but new Q is not, and at first time this
c                happens in TS, only let it turn on at half the Q looked up
                 if(qactCap < DZ .and. kiter == 3) then
                   mnw2(29,iw)=SQRT(qactCap)
c                  set CapFlag2 to true, this is used in AD routine to say that the
c                  Q has been constrained
                   mnw2(27,iw) = UNO
c  write message that Q was constrained
                   if(mnwprnt.gt.1) then
        write(iout,*) 'Capacity Q went from zero to nonzero, new Q  
     + constrained to half of value looked up for well ',WELLID(iw)
        write(iout,*) '   with Pump-Capacity Q = ',qactCap
                   end if
                 end if
               end if
              end if
             end if
            end if
c
          end if
c         if NNODES>1, that is, this is a multi-node well
          NNODES = abs(NINT(MNW2(2,iw)))
C-LFK          if(NNODES.gt.1) then
          if(NNODES > Z) then
            CALL SMNW2SEEP(IGRID,KPER,KSTP,iw,kiter)
c  for PUMPCAP option, check hwell vs last H
            if(MNW2(25,iw) > Z .and. kiter > TWO) then
              hwell = mnw2(17,iw)
              htemp = abs(lastH-hwell)
              HWtol = mnw2(28,iw)
c  if htemp (head change) is less than closure criterion stop doing capacity constraint
              if (htemp <= HWtol) then
                mnw2(29,iw) = qactCap
                mnw2(25,iw) = DZ
                mnw2(27,iw) = UNO
c  write message that Q was constrained
                if(mnwprnt > ONE) then
c        write(iout,*) 'Capacity Q calculations stopped due to hwell
c     & change less than HWtol for well   ',WELLID(iw)
        write(iout,200) kiter,WELLID(iw)
  200 FORMAT (2X,'Capacity Q calculations stopped iter. no. ',I3,' due 
     +to hwell change less than HWtol for well ',20A)
        write(iout,*) ' with Pump-Capacity Q = ',qactCap
                end if
              end if
            end if
          end if
        end if
      end do
      !
      iw = -10                   ! Temp use as a subtractor to determine how close kitter is close to mxiter
      if(mxiter < 11) iw = -5
      iw = mxiter + iw           ! Is either 10 or 5 less than mxiter
      !
      IF    (KITER==1   ) THEN    !RHS ONLY
                          iqslv = Z
      ELSEIF(SOLV_FLG==1) THEN    !RHS ONLY
                          iqslv = Z
      ELSEIF(SOLV_FLG==2) THEN    !HCOF ONLY
                          iqslv = ONE
      ELSEIF(KITER  > iw) THEN    !RHS ONLY - Close to mxiter so stop using HCOF to stabilize budgets
                          iqslv = Z
      ELSEIF( EVEN_ITER ) THEN    !HCOF SOLVE
                          iqslv = ONE           
      ELSE                        !RHS SOLVE
                          iqslv = Z
      END IF
      !
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1 and IBOUND>0
        if (MNW2(1,iw) > HALF) then
          qdes     = mnw2(5,iw)
          Qextract = qdes < DZ
c   If Capacity restrictions are set, Qdes here is actually the retricted Qpot
          if(NINT(mnw2(27,iw)) /= Z) qdes = mnw2(29,iw)
          !
          !hwell = mnw2(17,iw)
          !
          firstnode = NINT( MNW2(4,iw) )
          lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
          !
          !QLIMIT = INT(MNW2(6,iw)) .NE. 0
          !
c-LFK Nov. 2012
          if(NINT(mnw2(27,iw)) /= Z) LIMQ(1,IW) = ONE
          !
          !WRITE(998,'(i6,1x,F11.5, 25x A)') kiter, hwell, REPEAT("-",50)
c   Loop over nodes in well
          do INODE=firstnode,lastnode
            il = NINT( MNWNOD(1,INODE) )              
            ir = NINT( MNWNOD(2,INODE) )              
            ic = NINT( MNWNOD(3,INODE) )
            !
            qact = MNWNOD(4, INODE)
            cond = MNWNOD(14,INODE)
            !
            hwell = MNWNOD(15,INODE)   !  Water level in wellbore, or bottom of cell if seepage face cell -> used for => qact = ( hlim - Hcell) * cond
            !IF ( QLIMIT )THEN
            !  hlim = MNW2(7,iw)
            !ELSE
            !  hlim = hwell
            !END IF
            !
            Hcell = HNEW(IC,IR,IL)
            !
            IF(iqslv /= Z) qact = (hwell - Hcell)*cond    ! .and. kiter > ONE <- implied
            !
            HasQ = abs(qact) > small
            !
            IF( cond < NEARZERO_20 ) THEN
                                   SKIP_NODE = .TRUE.
                                   !
            ELSEIF(IBOUND(ic,ir,il) /= Z .and. MNWNOD(22,INODE) > HALF)
     +                                                              THEN
              !
              IF(.NOT. HAS_NWT .OR. LAYHDT(IL) == Z) THEN
                      !
                      SKIP_NODE = .FALSE.
                      !
              ELSEIF( LAYHDT(IL) > Z .AND. 
     +                Hcell > BOTM(IC,IR,LBOTM(IL)) ) THEN
                      !
                      SKIP_NODE = .FALSE.
              ELSE
                      SKIP_NODE = .TRUE.
              END IF
            ELSE
                      SKIP_NODE = .TRUE.
            END IF
            !
            IF(SKIP_NODE) THEN
                MNWNOD(4,INODE) = DZ
            ELSE
c
c   Modify HCOF and RHS arrays
cdebug replace below line with this if debugging with No Mo Iter
cdebug              if(iqslv.ne.0.and.kiter.gt.1.and.kiter.lt.NoMoIter) then
              if(iqslv /= 0 .AND. HasQ) then
                 !qact  = ( hlim - Hcell) * cond
                 !
                 if (firstnode==lastnode) THEN
                 if (Qextract .and. qact > DZ) then  ! if desired pump is extraction but back calc results in injection
                       qact = DZ
                       cond = DZ
                       MNWNOD(15,INODE) = Hcell
                 end if
                 end if
                 !
                 hcof(ic,ir,il) = hcof(ic,ir,il) - cond
                 rhs(ic,ir,il)  = rhs(ic,ir,il)  - cond * hwell
                 SOLV_BY_HCOF = .TRUE.
              else
                 ! Specify Q and solve for head;  add Q to RHS accumulator.
                 rhs(ic,ir,il) = rhs(ic,ir,il) - qact
                 SOLV_BY_HCOF = .FALSE.
              endif
              !
              MNWNOD(4,INODE) = qact
            end if
            !WRITE(998,'(I6,1x,*(F11.5, :, 1x))')INODE,hwell,Hcell,qact
          end do
        end if
      end do
c
      return
      end
c
      SUBROUTINE GWF2MNW27BD(kstp,kper,IGRID)
C     ******************************************************************
c     calculate volumetric budget for multi-node wells
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,HNEW,BUFF,LAYHDT
      USE GWFBASMODULE, ONLY:HDRY,VBVL,VBNM,DELT,PERTIM,TOTIM,
     1                       MSUM,ICBCFL,HAS_STARTDATE, DATE_SP
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,
C-LFK     2                       SMALL,WELLID,NTOTNOD
     2                       SMALL,WELLID,NTOTNOD,LIMQ,MNW2BUD,GRPNODE,
     3                       IOUT,MNW2_WARN,SMNW2COND_WARN,HAS_NWT,
     4                       SOLV_FLG,HCOF_ONLY_ITER,MXNODE,LEN_WELLID,
     7                       PRNT_MNW2, PRNT_MNW2_Q_NODE,SOLV_BY_HCOF,
     +                       PRNT_MNW2_Q_INOUT,PRNT_MNW2_NODE
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE ERROR_INTERFACE,    ONLY: WARNING_MESSAGE
      USE CONSTANTS,         ONLY: NL, TRUE, FALSE, DZ, UNO, HALF,
     +                             Z, ONE, 
     +                             ninf, inf_I, NEARZERO_20
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE MNW2_OUPUT, ONLY: PRNT_MNW2_SUB, PRNT_MNW2_INOUT_SUB, 
     +                      PRNT_MNW2_NODE_SUB, PRNT_MNW2_NODE_INFO
C     ------------------------------------------------------------------
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: kstp, kper, IGRID
      INTEGER:: ibd, ioch, ioc, IG, NAUX, IDX, IW
      INTEGER:: IL, IC, IR, INODE, ND, R1, R2
      INTEGER:: firstnode, lastnode, iseepflg
      DOUBLE PRECISION:: ratin, ratout
      DOUBLE PRECISION:: q, qdes, qnet, qin, qout
      DOUBLE PRECISION:: hlim, hwell, Hcell
      DOUBLE PRECISION:: s, sNL, sL
      DOUBLE PRECISION:: cond, Bottom
      CHARACTER(19):: DATE
      CHARACTER(16) text
c-lfk  10/10/2012
      CHARACTER(25) ctext1
      CHARACTER(23) ctext2,ctext3
      CHARACTER(32) ctext4
      CHARACTER(15) ctext5
      CHARACTER(24) ctext6,CTEXT7
      LOGICAL:: ALLDRY, DRY_NODE, QLIMIT, Qextract
      REAL:: ZERO, realQ  ! Using default REAL for passing to routines
      ZERO = 0.0
C
      CALL SGWF2MNW2PNT(IGRID)
c             ----+----1----+-
      !text = '            MNW2'
c-lfk  10/10/2012
      ctext1 = '   Q  updated because of '
      ctext2 = 'Pump Capacity restraint'
      ctext3 = ' and/or Hlim constraint'
      ctext4 = ' and/or Seepage Face calculation'
      ctext5 = 'Hlim constraint'
      ctext6 = 'Seepage Face calculation'
      ctext7 = 'unidentified factors ***'
      !
      IF(HAS_STARTDATE) THEN  !SET SP STARTING DATE
          DATE = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
      ELSE
          DATE = NUM2STR(TOTIM-DELT)
          DATE = ADJUSTL(DATE)
      END IF
c
c     ------------------------------------------------------------------
c
      IF(HCOF_ONLY_ITER < inf_I) SOLV_FLG = Z  !RESET TO DEFAULT
      !
      ibd = Z
      IF(IWL2CB > Z) IBD=ICBCFL
      !
      if(MNWPRNT>-1) WRITE(iout,*)
      !
      IF(SMNW2COND_WARN%RAISED) CALL SMNW2COND_WARN%CHECK(
     + 'MNW2 subroutine that calculates the cell-to-well conductance '//
     + 'terms (SMNW2COND) '//NL//'Had the following warnings raised '//
     + 'while solving the time step:',OUTPUT=IOUT)
C
c -----print the header for individual rates if requested(IWL2CB<0).
      ioch = Z
      if( IWL2CB < Z .and. icbcfl /= Z  .AND. MNWPRNT>-1) then
          write(iout,'(/,1x,a,9h PERIOD =,i5,8h  STEP =,i5)')
     +           'Nodal information for all MNW2 wells,',  kper,kstp
             write(iout,'(101A)') 'WELLID                NODE   Lay ',
     +'  Row   Col        Totim        Q-node         Hnode         Hcel
     +l    Seepage elev.'
        ioch = ONE
      endif
      !
      GROUPS: DO IG=1, MNW2BUD%NGRP
         !
         RATIN =DZ               !clear ratin and ratout accumulators.
         RATOUT=DZ
         TEXT = MNW2BUD%GRP(IG)
         TEXT = ADJUSTR(TEXT)
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN  !swm: check if wells are active seb removed "nmnw2.gt.0.AND." which was added by swm
         NAUX=NMNWVL-30
C-LFK         NAUX = 0   !!   Set to zero -- Change order to dump
c         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,MNWAUX,IWL2CB,NCOL,NROW,NLAY,
     1          GRPNODE(IG),IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
c  clear the buffer.
      buff=DZ
c
c2------if there are no wells do not accumulate flow
      if(MNW2BUD%DIM(IG) > Z) then  !formully nmnw2
        !
c  Loop over all wells
        WELL_LOOP1: DO IDX=1, MNW2BUD%DIM(IG)
         IW = MNW2BUD%INDEX(IG,IDX)
c  active well check
         if(MNW2(1,iw) > HALF) then
          firstnode = NINT( MNW2(4,iw) )
          lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
          !
          qnet     = DZ
          qdes     = mnw2(5,iw)
          Qextract = qdes < DZ
          !
          QLIMIT = NINT(MNW2(6,iw)) .NE. Z
          !
          if (firstnode==lastnode .AND. Qextract) THEN  ! Single node well fix if desired pump is extraction but back calc results in injection
            INODE = firstnode
            cond  = MNWNOD(14,INODE)
            hwell = MNWNOD(15,INODE)        ! Water level in wellbore, or bottom of cell if seepage face cell -> used for => qact = ( hlim - Hcell) * cond
            IL    = NINT( MNWNOD(1,INODE) )             
            IR    = NINT( MNWNOD(2,INODE) )             
            IC    = NINT( MNWNOD(3,INODE) ) 
            Hcell = HNEW(IC,IR,IL)
            !
            IF(SOLV_BY_HCOF) THEN
               IF ( (hwell - Hcell)*cond > DZ ) THEN
                                             MNWNOD(4, INODE) = DZ
                                             MNWNOD(15,INODE) = Hcell
               END IF
            ELSEIF(MNWNOD(4, INODE) > DZ) THEN
                                             MNWNOD(4, INODE) = DZ
                                             MNWNOD(15,INODE) = Hcell
            END IF
          end if
          !
          ALLDRY=TRUE                                             !seb  CHECK IF ALL NODES ARE DRY
          DO INODE=firstnode,lastnode
              IL = NINT( MNWNOD(1,INODE) )             
              IR = NINT( MNWNOD(2,INODE) )             
              IC = NINT( MNWNOD(3,INODE) ) 
              IF(IBOUND(IC,IR,IL) == Z)  CYCLE
              IF(HNEW(IC,IR,IL).EQ.HDRY) CYCLE
              Bottom = BOTM(IC,IR,LBOTM(IL))
              IF(LAYHDT(IL).GT.0.AND.HAS_NWT.AND.
     +           HNEW(IC,IR,IL).LE.Bottom) CYCLE
              ALLDRY=FALSE  !FOUND ONE NONDRY CELL
              EXIT
          END DO
          !
c   Loop over nodes in well
          do INODE=firstnode,lastnode
            il = NINT( MNWNOD(1,INODE) )             
            ir = NINT( MNWNOD(2,INODE) )             
            ic = NINT( MNWNOD(3,INODE) )
            !
            nd = INODE - firstnode + ONE
            !
            Hcell = HNEW(ic,ir,il)
            !
            q     = MNWNOD(4, INODE)   ! May have to set to cond*(hwell-Hcell), if so then move to part of if(DRY_NODE) then block
            cond  = MNWNOD(14,INODE)
            hwell = MNWNOD(15,INODE)   ! Water level in wellbore, or bottom of cell if seepage face cell -> used for => qact = ( hlim - Hcell) * cond
            !
            DRY_NODE = IBOUND(IC,IR,IL) == Z .OR. 
     +                 Hcell == Hdry         .OR. 
     +                 cond  < NEARZERO_20
            !
            IF( HAS_NWT .AND. .NOT. DRY_NODE ) THEN
              IF( LAYHDT(IL) > Z .AND.
     +            Hcell <= BOTM(IC,IR,LBOTM(IL))  )  DRY_NODE = TRUE
            END IF
            !
            if(DRY_NODE) then
              if( MNWPRNT.gt.1 .AND. 
     +            ABS(MNWNOD(4,INODE)) > NEARZERO_20 ) then
                    write(IOUT,'(5A)') 
     +                    'Well: ',WELLID(iw),' Node: ',NUM2STR(ND,-3),
     +                    ' is in a dry cell, Q set to 0.0'
              end if
              MNWNOD(4,INODE) = DZ
              q = DZ
            elseif(SOLV_BY_HCOF) THEN  !Solved using HCOF so need to update qact to current HNEW since its not dry
                q = cond*(hwell-Hcell)
                !if (ABS(MNWNOD(4,INODE) - Q) > 20.0) THEN
                !    CONTINUE
                !end if
                !if(ABS(Q) > 1e-10) then
                !if (ABS((MNWNOD(4,INODE) - Q)/Q) > 1.0e-4) THEN
                !    CONTINUE
                !end if
                !end if
                MNWNOD(4, INODE) = q
            end if
c
c -----print the individual rates if requested(IWL2CB<0).
            if( ioch.eq.1 .AND. MNWPRNT>-1) then
                write(iout,'(A20,4i6,1x,1P5e14.6)')
     +            WELLID(iw),nd,il,ir,ic,totim,q,hwell,Hcell,
     +            MNWNOD(15,INODE)
            END IF
            !
c    Report all wells with production less than the desired rate......
            if(.NOT. DRY_NODE) then
              !
              qnet = qnet + q
              !
              buff(ic,ir,il) = buff(ic,ir,il) + q
              !
              if( q > DZ ) then      ! pumping rate is positive(recharge). add it to ratin.
                ratin = ratin + q
              else                   ! pumping rate is negative(discharge). add it to ratout.
                ratout = ratout - q
              endif
            endif
            enddo
            IF(ALLDRY)THEN !seb
                MNW2(17,iw) = Hdry
                qnet        = DZ
            END IF
            mnw2(18,iw) = qnet
c warn about dry well
!            if(MNWPRNT.gt.1.and.iweldry.eq.1) then
            if(MNWPRNT.gt.1.and.ALLDRY) then
!                write(IOUT,*)'Note-- the following MNW2 well went dry:',
!     +  WELLID(iw)                
            CALL MNW2_WARN%ADD(WELLID(IW)//'HAS GONE DRY '//
     +      'DUE TO THE HWEL < BOTTOM OF WELL/SCREEN'//NL)
!            CALL WARNING_MESSAGE(OUTPUT=IOUT,
!     +      MSG='MNW2 WELLID '//WELLID(IW)//': WENT DRY '//
!     +      '(HWEL < BOTTOM OF WELL/SCREEN)', INLINE=TRUE)
            end if
         end if
        enddo WELL_LOOP1
c
c   Sum components of  multi-node wells
c
c -----print the header for multi-node rates if requested(IWL2CB<0).
        if (ioch.eq.1) then
          IF(MNWPRNT>-1) THEN
            write(iout,'(/,5x,a )') 'Summary information for MNW2 wells'
             write(iout,'(200A)') 'WELLID                    Totim    
     +        Qin           Qout           Qnet          hwell'
           ENDIF
        endif
c  if Q is constrained, print message
c-LFK     
        if(mnwprnt.gt.0) then
        WELL_LOOP2: DO IDX=1, MNW2BUD%DIM(IG)
         IW = MNW2BUD%INDEX(IG,IDX)
c  active well check
         if(MNW2(1,iw) > HALF) then
          !
          firstnode = NINT( MNW2(4,iw) )
          lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
          !
          qdes = mnw2(5,iw)
          qnet = mnw2(18,iw) 
          !
          iseepflg = Z
          R1 = Z
          R2 = Z
          !
          qin  = DZ
          qout = DZ
          !
          do INODE=firstnode,lastnode
            !
            q  = MNWNOD(4, INODE)   ! May have to set to cond*(hwell-Hcell), if so then move to part of if(DRY_NODE) then block
            !
            if( q < DZ ) then
                         qout = qout + q
            else
                         qin  = qin  + q
            endif
          enddo
          !mnw2(18,iw) = qnet
C   if qnet<qdes, then Q was constrained
          if( abs(qdes-qnet) > small ) then
              !
              write(iout,'(/3A,2(G16.8,A))') ' MNW2 WELL: ',
     +              WELLID(iw),' Qnet = ',qnet, ' < ',Qdes,' = Qdes'
              !
              R1      =LIMQ(1,IW)
              R2      =LIMQ(2,IW)
              ISEEPFLG=LIMQ(3,IW)
              !
              if ( mnw2(27,iw) /= Z .and.
     +            (qnet-mnw2(29,iw)).lt.small )  R1=1
              !
              if( QLIMIT .AND.
     +            (abs(qnet)-abs(mnw2(30,iw))).lt.small )  R2=1
              !
              IF (R1.EQ.1.AND.R2.EQ.0.AND.ISEEPFLG.EQ.0) THEN
                WRITE(IOUT,'(A25,A23)') CTEXT1,CTEXT2
              ELSE IF (R1.EQ.1.AND.R2.EQ.1.AND.ISEEPFLG.EQ.0) THEN
                WRITE(IOUT,'(A25,A23,A23)') CTEXT1,CTEXT2,CTEXT3
              ELSE IF (R1.EQ.1.AND.R2.EQ.1.AND.ISEEPFLG.EQ.1) THEN
                WRITE(IOUT,'(A25,A23,A23,A32)') CTEXT1,CTEXT2,CTEXT3,
     +                CTEXT4
              ELSE IF (R1.EQ.0.AND.R2.EQ.1.AND.ISEEPFLG.EQ.0) THEN
                WRITE(IOUT,'(A25,A15)') CTEXT1,CTEXT5
              ELSE IF (R1.EQ.0.AND.R2.EQ.1.AND.ISEEPFLG.EQ.1) THEN
                WRITE(IOUT,'(A25,A15,A32)') CTEXT1,CTEXT5,CTEXT4
              ELSE IF (R1.EQ.0.AND.R2.EQ.0.AND.ISEEPFLG.EQ.1) THEN
                WRITE(IOUT,'(A25,A24)') CTEXT1,CTEXT6
              ELSE IF (R1.EQ.1.AND.R2.EQ.0.AND.ISEEPFLG.EQ.1) THEN
                WRITE(IOUT,'(A25,A23,A32)') CTEXT1,CTEXT2,CTEXT4
              ELSE IF (R1.EQ.0.AND.R2.EQ.0.AND.ISEEPFLG.EQ.0) THEN
                WRITE(IOUT,'(A25,A24)') CTEXT1,CTEXT7
              END IF
c-lfk  10/10/2012
            iseepflg = Z
            R1 = Z
            R2 = Z
c--lfk                     
            end if
c -----print the summed rates if requested(IWL2CB<0).
            hwell = mnw2(17,iw)
            if(  ioch.eq.1  ) then
c            if(qnet.lt.(small*qin)) qnet=0.d0
              write(iout,'(A20,5(1x,ES14.6))')
     +              WELLID(iw),totim,qin,qout,qnet,hwell
            endif
          endif
        enddo WELL_LOOP2
        endif
        !
        if(  ioch == ONE  ) write(iout,*)
c
c  ----- END  MULTI-NODE reporting section -------------
      endif
c6------if cell-by-cell flows will be saved call ubudsv to record them seb MOVED OUTSIDE OF PREVIOUS IF STATEMENT TO WRITE OUT EVEN WHEN THERE ARE NO MNW2 WELLS
        if( abs(IWL2CB) > Z .and. icbcfl /= Z ) then           
          ioc = abs(IWL2CB)
          if( ibd == 2 ) then   !!  Write COMPACT budget
c   Loop over all wells
        WELL_LOOP3: DO IDX=1, MNW2BUD%DIM(IG)
         IW = MNW2BUD%INDEX(IG,IDX)
c   Loop over nodes in well
         firstnode = NINT( MNW2(4,iw) )
         lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
         if(MNW2(1,iw) < HALF .or. NMNW2 == Z) THEN ! WELL IS INACTIVE
              do INODE=firstnode,lastnode
                ic = NINT( MNWNOD(3,INODE) )
                ir = NINT( MNWNOD(2,INODE) )
                il = NINT( MNWNOD(1,INODE) )
                call UBDSVB(ioc,ncol,nrow,IC,IR,IL,ZERO,
     +                    real(mnw2(:,iw)),
     +                    NMNWVL,NAUX,31,IBOUND,NLAY)
              end do
         else
              do INODE=firstnode,lastnode
                ic = NINT( MNWNOD(3,INODE) )
                ir = NINT( MNWNOD(2,INODE) )
                il = NINT( MNWNOD(1,INODE) )
                !Q  = MNWNOD(4,INODE)
                realQ = REAL(MNWNOD(4,INODE))
!                IF ( HAS_NWT ) THEN                                  !seb ADDED BLOCK TO MATCH WHAT IS WRITTEN TO BUDGET
!                  hwell = MNWNOD(15,INODE)
!                  IF ( LAYHDT(IL) > Z .AND. INT(MNW2(6,iw)) /= Z) THEN
!                    hlim=mnw2(7,iw)
!                    IF ( hlim < BOTM(ic,ir,lbotm(il)) ) 
!     +                   hlim = BOTM(ic,ir,lbotm(il))
!                    if ( hwell < hlim ) hwell = hlim
!                  END IF
!                  Hcell = hnew(ic,ir,il)
!                  cond  = MNWNOD(14,INODE)
!                  q     = cond*(hwell-Hcell)
!                  if (ABS(MNWNOD(4,INODE) - Q) > 1.0e-5) THEN
!                      CONTINUE
!                  END IF
!                END IF
c
                call UBDSVB(ioc,ncol,nrow,IC,IR,IL,realQ,
     +                    real(mnw2(:,iw)),
     +                    NMNWVL,NAUX,31,IBOUND,NLAY)
              end do
             end if ! (MNW2(1,iw) < HALF .or. NMNW2 == Z)
            enddo WELL_LOOP3
          else                  !!  Write full 3D array                ! IBD==1 case
            IF(NMNW2 == Z) buff = DZ                                   ! SET FLOW RATE TO 0 WHEN NOT MNW2 WELLS ARE IN USE
            call ubudsv(kstp,kper,text,ioc, buff,ncol,nrow,nlay,iout)
          endif
        endif
c
c7------move rates into vbvl for printing by module bas1ot.
      vbvl(3,msum)=ratin
      vbvl(4,msum)=ratout
c
c8------move rates times time step length into vbvl accumulators.
      vbvl(1,msum) = vbvl(1,msum) + ratin*delt
      vbvl(2,msum) = vbvl(2,msum) + ratout*delt
c
c9------move budget term labels into vbnm for printing.
      vbnm(msum) = text
c
c10-----increment budget term counter(msum).
      msum = msum + ONE
      !
      END DO GROUPS
      !
      ! Check if output files are requested
      IF(PRNT_MNW2        %IS_OPEN) CALL PRNT_MNW2_SUB(PRNT_MNW2, 
     +                              DATE, KPER, KSTP, NMNWVL, MNWMAX, 
     +                              NODTOT, MXNODE, LEN_WELLID,
     +                              MNW2, MNWNOD, WELLID)
      !
      IF(PRNT_MNW2_Q_NODE %IS_OPEN) CALL PRNT_MNW2_NODE_SUB(
     +                              PRNT_MNW2_Q_NODE, 
     +                              DATE, KPER, KSTP, NMNWVL, MNWMAX, 
     +                              NODTOT, MXNODE, LEN_WELLID,
     +                              MNW2, MNWNOD, WELLID)
      !
      IF(PRNT_MNW2_Q_INOUT%IS_OPEN) CALL PRNT_MNW2_INOUT_SUB(
     +                              PRNT_MNW2_Q_INOUT, 
     +                              DATE, KPER, KSTP, NMNWVL, MNWMAX, 
     +                              NODTOT, MXNODE, LEN_WELLID,
     +                              MNW2, MNWNOD, WELLID)
      !
      IF(PRNT_MNW2_NODE%IS_OPEN) CALL PRNT_MNW2_NODE_INFO(
     +                              PRNT_MNW2_NODE, 
     +                              DATE, KPER, KSTP, NMNWVL, MNWMAX, 
     +                              NODTOT, MXNODE, LEN_WELLID,
     +                              MNW2, MNWNOD, WELLID, 
     +                              HNEW, BOTM, LBOTM)
      !
      CALL MNW2_WARN%CHECK('MNW2 HAS THE FOLLOWING WARNINGS RAISED '//
     +          'WHILE SOLVING THE TIME STEP:',OUTPUT=IOUT)
      !
!      IF(MNW2_WARN%STR.NE.NL) THEN
!            CALL WARNING_MESSAGE(OUTPUT=IOUT,
!     +      MSG='MNW2 HAS THE FOLLOWING WARNINGS RAISED WHILE '//
!     +          'SOLVING THE CURRENT TIME STEP:'//MNW2_WARN%STR)
!      END IF
c
c11-----return
      return
      end
c
      SUBROUTINE MNW2_GET_COND(KPER,IGRID, IMNW, IBCF, ILPF, IUPW, IHUF)
      USE ERROR_INTERFACE, ONLY: STOP_ERROR
      USE CONSTANTS,       ONLY: Z
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: KPER,IGRID, IMNW, IBCF, ILPF, IUPW, IHUF
      !
      IF    (ILPF .NE. Z) THEN;  CALL GWF2MNW27LPF(KPER,IGRID)
      ELSEIF(IUPW .NE. Z) THEN;  CALL GWF2MNW27UPW(KPER,IGRID)
      ELSEIF(IBCF .NE. Z) THEN;  CALL GWF2MNW27BCF(KPER,IGRID)
      ELSEIF(IHUF .NE. Z) THEN;  CALL GWF2MNW27HUF(KPER,IGRID)
      ELSE
          CALL STOP_ERROR(INFILE=IMNW, MSG=
     +  'MNW2 only works with the flow packages BCF, LPF, UPW, and HUF.'
     +   //NEW_LINE(" ")//'The current flow package is not supported.')
      END IF
      !
      END SUBROUTINE
c
      SUBROUTINE GWF2MNW27BCF(KPER,IGRID)
C     ******************************************************************
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
C     Note: BCF, when LAYCON=0 or 2, does not save cell-by-cell
C     Transmissivity (T) values.  Instead, it converts the cell-by-cell
C     T values to branch conductances CR and CC, using harmonic
C     averaging.  When BCF is used, the method used in this routine to
C     generate cell-specific values of Tx and Ty is an approximation
C     based on CR and CC.  When LPF or HUF is used, cell-by-cell
C     hydraulic-conductivity values are stored, this approximation is
C     not needed, and the values generated for Tx and Ty are exact --
C     ERB 1/29/01.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,DELR,DELC,NBOTM,LBOTM,
     1                       BOTM,HNEW,LAYHDT,LAYCBD,CR,CC,ISSFLG
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFBCFMODULE, ONLY:HY,LAYCON,TRPY,SC1
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,
     1                       NODTOT,MNW2,MNWNOD,IOUT,
     2                       SMALL,WELLID
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE CONSTANTS,     ONLY: ONE, DZ, HALF, UNO, DOS, NEARZERO_20
C     ------------------------------------------------------------------
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: KPER,IGRID
      INTEGER:: firstnode,lastnode
      INTEGER:: ISS, ix, iy, iz, iw, INODE
      DOUBLE PRECISION::top,bot, SS
      DOUBLE PRECISION:: dx,dy,ah,dxp,Txp,dxm,Txm
      DOUBLE PRECISION:: dyp,Typ,dym,Tym,Txx,div,Tyy
      DOUBLE PRECISION:: upper,TempKX,thick
      REAL:: Kz
C
      ISS = ISSFLG(KPER)
      CALL SGWF2MNW2PNT(IGRID)
c
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
C
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw) > HALF) then
c   Loop over nodes in well
        firstnode = NINT( MNW2(4,iw) )
        lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
        do INODE=firstnode,lastnode
         ix  = NINT( MNWNOD(3,INODE) )
         iy  = NINT( MNWNOD(2,INODE) )
         iz  = NINT( MNWNOD(1,INODE) )
         dx  = delr(ix)
         dy  = delc(iy)
         top = BOTM(IX,IY,LBOTM(IZ)-1)
         bot = BOTM(IX,IY,LBOTM(IZ))
C
C     FIND HORIZONTAL ANISOTROPY, THE RATIO Ky/Kx
         AH = TRPY(IZ)
C
         if (LAYHDT(IZ).EQ.0) then
C       THICKNESS IS NOT HEAD-DEPENDENT
          dxp  = dx
          Txp  = DZ
          if( ix < ncol ) then
            dxp = delr(ix+1)
            Txp = cr(ix,iy,iz) * (dx+dxp) * HALF
          endif
          dxm = dx
          Txm  = Txp
          if( ix .gt. 1  ) then
            dxm = delr(ix-1)
            Txm  = cr(ix-1,iy,iz) * (dx+dxm) * HALF
          endif
          if( Txp.lt.small ) Txp = Txm
          if( Txm.lt.small ) Txm = Txp
c
          dyp  = dy
          Typ  = DZ
          if( iy .lt. nrow ) then
            dyp = delc(iy+1)
            Typ  = cc(ix,iy,iz) * (dy+dyp) * HALF
          endif
          dym = dy
          Tym  = Typ
          if( iy .gt. 1 ) then
            dym = delc(iy-1)
            Tym  = cc(ix,iy-1,iz) * (dy+dym) * HALF
          endif
          if( Typ.lt.small ) Typ = Tym
          if( Tym.lt.small ) Tym = Typ
          Txp = Txp / dy
          Txm = Txm / dy
          Typ = Typ / dx
          Tym = Tym / dx
c
c  Eliminate zero values .....
c
          if( Typ.lt.small .or. nrow.lt.2 )  then
            Typ = Txp
            Tym = Txm
          endif
c
          if( Txp.lt.small .or. ncol.lt.2 )  then
            Txp = Typ
            Txm = Tym
          endif
c
c   Assuming expansion of grid is slight, if present, & that Txx and Tyy of the adjacent
c   cells are about the same value.
          Txx = DZ
          div  = Txp + Txm
          if( abs(div).gt.small ) Txx  = DOS*Txp*Txm / div
          Tyy = DZ
          div  = Typ + Tym
          if( abs(div).gt.small ) Tyy  = DOS*Typ*Tym / div
          if( Txx.gt.small .and. Tyy.lt.small ) Tyy = Txx
          if( Tyy.gt.small .and. Txx.lt.small ) Txx = Tyy
         else
C       THICKNESS IS HEAD-DEPENDENT
c  Estimate T to well in an unconfined system
c
          upper = hnew(ix,iy,iz)
          if (LAYCON(IZ).EQ.3) then
           if( upper.gt.top ) upper = top
          endif
          TempKX = hy(ix,iy,iz)      !! BCF Hydraulic Conductivity array
          thick = upper - bot
c   set thickness / conductance to 0 if cell is dry
          if( (hnew(ix,iy,iz)-Hdry )**2 < NEARZERO_20 ) 
     &       thick = DZ
          Txx = TempKX * thick
          if( Txx < NEARZERO_20 ) Txx = DZ
          Tyy = Txx * AH
         endif
         MNWNOD(16,INODE)=Txx
         MNWNOD(17,INODE)=Tyy
C  FOR BCF, must assume Kh=Kz as only input is VCONT
         upper = hnew(ix,iy,iz)
         if( upper.gt.top ) upper = top
         thick = upper - bot
         if(thick > NEARZERO_20) then
           Kz=((Txx*Tyy)**0.5D0)/thick
         else
           Kz=0.d0
         end if
c-lfk         IF(ISS.NE.0) THEN
         IF(ISS.EQ.0 .and. thick > NEARZERO_20) THEN
           SS=SC1(IX,IY,IZ)/(thick*dx*dy)
         ELSE
           SS=1e-5
         END IF
         MNWNOD(33,INODE)=Kz
         MNWNOD(34,INODE)=SS
        end do 
       end if 
      end do 
      !
      END SUBROUTINE
c
c
      SUBROUTINE GWF2MNW27LPF(KPER,IGRID)
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,DELR,DELC,NBOTM,LBOTM,
     1                       BOTM,HNEW,LAYHDT,LAYCBD,CR,CC,ISSFLG,ITRSS
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFLPFMODULE, ONLY:CHANI,HANI,HK,LAYVKA,VKA,SC1
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,
     1                       NODTOT,MNW2,MNWNOD,IOUT,
     2                       WELLID
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE CONSTANTS,     ONLY: NEG, Z, ONE, UNO, DZ, HALF, NEARZERO_20
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: KPER,IGRID
      INTEGER:: firstnode, lastnode, iw, INODE, ISS
      INTEGER:: ix, iy, iz, KHANI
      DOUBLE PRECISION:: dx,dy,top,bot
      DOUBLE PRECISION:: AH, Txx, Tyy, upper, TempKX, thick
      DOUBLE PRECISION:: Kz, SS
C
      CALL SGWF2MNW2PNT(IGRID)
      ISS=ISSFLG(KPER)
c
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
C
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw) > HALF) then
c   Loop over nodes in well
        firstnode = NINT( MNW2(4,iw) )
        lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
        do INODE=firstnode,lastnode
         ix  = NINT( MNWNOD(3,INODE) )
         iy  = NINT( MNWNOD(2,INODE) )
         iz  = NINT( MNWNOD(1,INODE) )
         dx  = delr(ix)
         dy  = delc(iy)
         top = BOTM(IX,IY,LBOTM(IZ)-1)
         bot = BOTM(IX,IY,LBOTM(IZ))
C
C     FIND HORIZONTAL ANISOTROPY, THE RATIO Ky/Kx
         AH = UNO
         IF (CHANI(IZ) > DZ) THEN
             AH = CHANI(IZ)
         ELSE
             KHANI = NEG*NINT(CHANI(IZ))
             AH    = HANI(IX,IY,KHANI)
         ENDIF
C
         if (LAYHDT(IZ) == Z) then
C       THICKNESS IS NOT HEAD-DEPENDENT
          THICK = TOP-BOT
          TXX   = HK(ix,iy,iz)*THICK
          TYY   = TXX*AH
         else
C       THICKNESS IS HEAD-DEPENDENT
c  Estimate T to well in an unconfined system
c
          upper = hnew(ix,iy,iz)
          if( upper > top ) upper = top
          TempKX = hk(ix,iy,iz)       !!LPF Hydraulic Conductivity array
          thick  = upper - bot
c   set thickness / conductance to 0 if cell is dry
          if( (hnew(ix,iy,iz)-Hdry )**2 < NEARZERO_20 ) 
     +          thick = DZ
          Txx = TempKX * thick
          if( Txx < NEARZERO_20 ) Txx = DZ
          Tyy = Txx * AH
         endif
         MNWNOD(16,INODE) = Txx
         MNWNOD(17,INODE) = Tyy
         if(nlay == ONE) then
             Kz = DZ
         else
           IF(LAYVKA(iz) == Z) THEN
             Kz = VKA(ix,iy,iz)
           ELSE
c--LFK 10/4/12 modifications below to prevent zero-divide error if VKA.eq.0
            if (VKA(ix,iy,iz) > NEARZERO_20) then
                Kz = HK(ix,iy,iz)/VKA(ix,iy,iz)
            else
                Kz = DZ
c--lfk
c               write (iout,*) ' VKA = 0.0 for wellid,ix,iy,iz = ',
c     1                      wellid(iw),ix,iy,iz
            end if
           END IF
         end if
c-lfk
c          if (thick.le.0) then
c            if (LAYHDT(IZ).eq.0)then
c           write (iout,*) 'thck = ',thick,'iz,LAYHDT(IZ) = ',IZ,LAYHDT(IZ)
c           write (iout,*) ' confined; inode,ix,iy = ',inode,ix,iy
c           write (iout,*) 'top,bottom = ', top,bot
c            else
c           write (iout,*) 'thck = ',thick,'iz,LAYHDT(IZ) = ',IZ,LAYHDT(IZ)
c           write (iout,*) 'unconfined; inode,ix,iy = ',inode,ix,iy
c           write (iout,*) 'upper,bottom = ', upper,bot
c            end if
c          end if
c         continue
c lfk  4/21/11 modifications below to prevent zero-divide error if thick.eq.0       
         IF(thick > NEARZERO_20) THEN
          IF(ITRSS /= Z) THEN
              SS = SC1(IX,IY,IZ)/(thick*dx*dy)
          ELSE
              SS = 1e-5  
          END IF
         else
              SS = DZ
         end if
         MNWNOD(33,INODE) = Kz
         MNWNOD(34,INODE) = SS
        end do
       end if
      end do 
      !
      END SUBROUTINE 
c lfk
c
c
      SUBROUTINE GWF2MNW27HUF(KPER,IGRID)
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,DELR,DELC,NBOTM,LBOTM,
     1                       BOTM,HNEW,LAYHDT,LAYCBD,CR,CC,ISSFLG
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFHUFMODULE, ONLY:HK,HKCC,VKAH,SC1
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,
     1                       NODTOT,MNW2,MNWNOD,IOUT,
     2                       WELLID
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE CONSTANTS,     ONLY: Z, ONE, UNO, DZ, HALF, NEARZERO_20
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: KPER,IGRID
      INTEGER:: firstnode, lastnode, iw, INODE, ISS
      INTEGER:: ix, iy, iz
      DOUBLE PRECISION:: dx,dy,top,bot,ah
      DOUBLE PRECISION:: Txx,Tyy,upper,TempKX,thick,KY
      DOUBLE PRECISION:: Kz, SS
C
      CALL SGWF2MNW2PNT(IGRID)
      ISS=ISSFLG(KPER)
C
c
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
C
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw) > HALF) then
c   Loop over nodes in well
        firstnode = NINT( MNW2(4,iw) )
        lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
        do INODE=firstnode,lastnode
         ix  = NINT( MNWNOD(3,INODE) )
         iy  = NINT( MNWNOD(2,INODE) )
         iz  = NINT( MNWNOD(1,INODE) )
         dx  = delr(ix)
         dy  = delc(iy)
         top = BOTM(IX,IY,LBOTM(IZ)-1)
         bot = BOTM(IX,IY,LBOTM(IZ))
C
C     FIND HORIZONTAL ANISOTROPY, THE RATIO Ky/Kx
         TempKX = HK(ix,iy,iz)
         KY     = HKCC(IX,IY,IZ)
         AH     = DZ
         if ( TempKX > NEARZERO_20 ) AH = KY/TempKX
         !
         if (LAYHDT(IZ) == Z) then
C       THICKNESS IS NOT HEAD-DEPENDENT
          THICK = TOP-BOT
          TXX   = HK(ix,iy,iz)*THICK
          TYY   = TXX*AH
         else
C       THICKNESS IS HEAD-DEPENDENT
c  Estimate T to well in an unconfined system
c
          upper = hnew(ix,iy,iz)
          if( upper > top ) upper = top
          TempKX = hk(ix,iy,iz)      !!HUF Hydraulic Conductivity array
          thick  = upper - bot
c   set thickness / conductance to 0 if cell is dry
          if( (hnew(ix,iy,iz)-Hdry )**2 < NEARZERO_20 ) thick = DZ
          Txx = TempKX * thick
          if( Txx < NEARZERO_20 ) Txx = DZ
          Tyy = Txx * AH
         endif
         MNWNOD(16,INODE) = Txx
         MNWNOD(17,INODE) = Tyy
         if(nlay == ONE) then
             Kz = DZ
         else
             Kz = VKAH(ix,iy,iz)
         end if
C-LFK         IF(ISS.NE.0) THEN
         IF(ISS == Z .and. thick > NEARZERO_20) THEN
             SS = SC1(IX,IY,IZ)/(thick*dx*dy)
         ELSE
             SS =1e-5
         END IF
         MNWNOD(33,INODE) = Kz
         MNWNOD(34,INODE) = SS
        end do 
       end if 
      end do 
      !
      END SUBROUTINE
c
      SUBROUTINE GWF2MNW27UPW(KPER,IGRID)
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,DELR,DELC,NBOTM,LBOTM,
     1                       BOTM,HNEW,LAYHDT,LAYCBD,CR,CC,ISSFLG,ITRSS
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFUPWMODULE, ONLY:CHANI,HANI,HKUPW,LAYVKAUPW,VKAUPW,SC1
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,
     1                       NODTOT,MNW2,MNWNOD,IOUT,
     2                       WELLID
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE CONSTANTS,     ONLY: Z, NEG, ONE, UNO, DZ, HALF, NEARZERO_20
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN):: KPER,IGRID
      INTEGER:: firstnode,lastnode, iw, INODE, ISS, KHANI
      INTEGER:: ix, iy, iz
      DOUBLE PRECISION:: dx,dy,top,bot,ah
      DOUBLE PRECISION:: Txx,Tyy,upper,TempKX,thick
      DOUBLE PRECISION:: Kz, SS
C
      CALL SGWF2MNW2PNT(IGRID)
      ISS = ISSFLG(KPER)
c
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
C
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw) > HALF) then
c   Loop over nodes in well
        firstnode = NINT( MNW2(4,iw) )
        lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
        do INODE=firstnode,lastnode
         ix  = NINT( MNWNOD(3,INODE) )
         iy  = NINT( MNWNOD(2,INODE) )
         iz  = NINT( MNWNOD(1,INODE) )
         dx  = delr(ix)
         dy  = delc(iy)
         top = BOTM(IX,IY,LBOTM(IZ)-1)
         bot = BOTM(IX,IY,LBOTM(IZ))
C
C     FIND HORIZONTAL ANISOTROPY, THE RATIO Ky/Kx
         AH = UNO
         IF (CHANI(IZ) > DZ) THEN
             AH = CHANI(IZ)
         ELSE
             KHANI = NEG*NINT(CHANI(IZ))
             AH    = HANI(IX,IY,KHANI)
         ENDIF
C
         if (LAYHDT(IZ) == Z) then
C       THICKNESS IS NOT HEAD-DEPENDENT
          THICK = TOP-BOT
          TXX   = HKUPW(ix,iy,iz)*THICK
          TYY   = TXX*AH
         else
C       THICKNESS IS HEAD-DEPENDENT
c  Estimate T to well in an unconfined system
c
          upper = hnew(ix,iy,iz)
          if( upper > top ) upper = top
          TempKX = hkupw(ix,iy,iz)       !!LPF Hydraulic Conductivity array
          thick  = upper - bot
          !if ( thick.LT.1.0e-15 ) thick = 1.0e-15             !seb automatically set to zero if negative
c   set thickness / conductance to 0 if cell is dry
!          if( (hnew(ix,iy,iz)-Hdry )**2 .lt. NEARZERO_20 )      !seb THIS WILL NEVER HAPPEN IN UPW
!     &          thick = 0.0000000000D0
          Txx = TempKX * thick
          if( Txx < NEARZERO_20 ) Txx = DZ
          Tyy = Txx * AH
         endif
         MNWNOD(16,INODE) = Txx
         MNWNOD(17,INODE) = Tyy
         if(nlay == ONE) then
             Kz = DZ
         else
           IF(LAYVKAUPW(iz) == Z) THEN
               Kz = VKAUPW(ix,iy,iz)
           ELSE
               Kz = HKUPW(ix,iy,iz)/VKAUPW(ix,iy,iz)
           END IF
         end if
         IF(ITRSS /= Z) THEN
           IF (thick > NEARZERO_20) THEN
               SS = SC1(IX,IY,IZ)/(dx*dy)
           ELSE
               SS = DZ
           END IF
         ELSE
               SS = 1e-5  
         END IF
         MNWNOD(33,INODE) = Kz
         MNWNOD(34,INODE) = SS
        end do
       end if
      end do 
      !
      END SUBROUTINE
c
c
c_________________________________________________________________________________
c
      SUBROUTINE SMNW2COND(IGRID,kstp,kper,kiter,ITFLAG)
c----- MNW1 by K.J. Halford
c----- MNW2 by G.Z. Hornberger
c
c     ******************************************************************
c     Calculate all Cell-to-well conductance terms
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,DELR,DELC,LAYHDT,
     2                       HNEW,ISSFLG
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,MNWINT,INTTOT,
     1                       NODTOT,MNW2,MNWNOD,SMALL,WELLID,IOUT,
     2                       HAS_NWT,PP_CON_ITER_LIM,
     3                       MNW2_WARN,SMNW2COND_WARN
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE ERROR_INTERFACE, ONLY: WARNING_MESSAGE
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE CONSTANTS,   ONLY: NL, TRUE, FALSE, DNEG, DZ, UNO, HALF,
     +                       Z, ONE, TWO, NEARZERO_20
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID,kstp,kper,kiter,ITFLAG
      CHARACTER(10):: ctext
      INTEGER:: firstnode,lastnode,firstint,lastint
      INTEGER:: nd,nod,ipr,iw,irecalc,iint,ISS
      INTEGER:: LOSSTYPE, NNODES, INODE, PPFLAG, ISOLNFLAG
      INTEGER:: ix, iy, iz, ippc
      DOUBLE PRECISION:: cond,dx,dy,top,bot,thck
      DOUBLE PRECISION:: Txx,Tyy,rw,Qact,Rskin,Kskin,B,C,CF,PLoss
      DOUBLE PRECISION:: cel2wel2,Kz,totlength,lengthint,ratio
      DOUBLE PRECISION:: CWC,ztop,zbotm,dhp,SS,Skin
      DOUBLE PRECISION:: ZPD,ZPL,ABC,ABCD,lengthratio,T,Kh
      DOUBLE PRECISION:: QQ,dpp,topscreen,bottomscreen
      DOUBLE PRECISION:: hwell,alpha,alpha2,alpha3
      DOUBLE PRECISION:: t1, t2
      LOGICAL:: PPC_CHK, ConstT
C
      CALL SGWF2MNW2PNT(IGRID)
      ISS=ISSFLG(KPER)
C
      PPC_CHK = TRUE
      CALL SMNW2COND_WARN%INIT()
c
c1------if number of wells <= 0 then return.
      if(nmnw2 < ONE) return
c
c   set print flag for well output 
c   if transient, print every TS; if steady, every SP
      ipr = Z
      if(ISS == Z) then
        if(kiter == ONE) ipr = ONE
      else
        if(kstp == ONE.and.kiter == ONE) ipr = ONE
      end if
c   now check mnwprnt and SPs
      if(mnwprnt < ONE) ipr = Z
      if(kper > ONE .and. mnwprnt < TWO) ipr = Z
c   print header for well output
c   if transient, by kiter=1 , if not, by tstep=1 
      if(ipr == ONE) then
          write(iout,'(/2A)') 'MNW2 Well Conductance and Screen ',
     +                       '(Open Interval) Data'
          write(iout,'(3A)') 
     +    '                              M O D E L    ',
     +    '   L A Y E R     W E L L  S C R E E N   Penetration    ',
     +    'SKIN           CALCULATED'
        write(iout,'(120A)') 'WELLID        Node    CWC*    top_elev   
     &bott.elev     top_elev   bott.elev    fraction     COEFF.
     &          B'
      end if
c   Compute cell-to-well conductance for each well node
C
c   Loop over all wells
      do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw) > HALF) then
        SKIN     = DZ                         !seb set up SKIN so that it has a value when passed into CEL2WEL
        LOSSTYPE = NINT(MNW2(3,iw))
        NNODES   = NINT(MNW2(2,iw))
        firstnode= NINT( MNW2(4,iw) )
        lastnode = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
        alpha    = UNO
        PPFLAG   = NINT(MNW2(19,iw))
c   determine well characteristics for nonvertical wells
c--LFK 11/27/2012
c        if(MNW2(21,iw).GT.0) then
        if(MNW2(21,iw) > HALF .and. LOSSTYPE.NE.4) then
          CALL MNW2HORIZ(IGRID,LOSSTYPE,NNODES,firstnode,lastnode,
     &                   IW,kstp,kper,ipr,alpha)
        else
c   for all other wells, define CWC in node loop   
c   Loop over nodes in well
        do INODE=firstnode,lastnode
         nod = INODE-firstnode+1 
         iz  = NINT( MNWNOD(1,INODE)   ) ! Layer
         iy  = NINT( MNWNOD(2,INODE)   ) ! Row
         ix  = NINT( MNWNOD(3,INODE)   ) ! Col
         ConstT = LAYHDT(iz) == Z
c set flag for deciding whether to recalculate CWC (1=true)
         irecalc = ONE
c 2
         !if(ibound(ix,iy,iz).lt.1 ) irecalc=0                           !SCOTT WHY BYPASS SPECIFIED HEAD?
         !
         IF(HAS_NWT .AND. .NOT. ConstT) THEN
            IF(hnew(ix,iy,iz).LE.BOTM(IX,IY,LBOTM(IZ))) THEN
                                                              irecalc=Z         !seb FIX TO ENSURE THAT DRY CELLS OR THICK=0 DO NOT GET PROCESSED --SCOTT THIS MAY NEED TO ZERO OUT CWC AS WELL, but it should have a smoother later on
                                                     MNWNOD(14,INODE)=DZ        !seb if cell is inactive, then zero out conductance of node (CWC)
            END IF
         END IF
c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't recalculate CWC - But only if their is no PP
         if(ConstT .and. kiter > 1 .and. PPFLAG == Z) irecalc = Z
c
c if PP then always recalculate
         if(ConstT .and. kiter > PP_CON_ITER_LIM  ) irecalc=Z    !.and.PPFLAG>0 is implied
C
C only solve for active nodes
         if( MNWNOD(22,INODE) < HALF ) irecalc = Z
c
c if GENERAL, always recalculate
         if(LOSSTYPE.eq.3 .and. MNWNOD(9,INODE) > DZ) irecalc = ONE
c
c-----if the cell is inactive or specified then bypass processing.
         if(ibound(ix,iy,iz).eq.0 ) THEN                                !seb if cell is inactive, then zero out conductance of node (CWC)
             MNWNOD(14,INODE)=DZ
             MNWNOD(22,INODE)=DZ
             irecalc = Z
         elseif(ibound(ix,iy,iz) < Z .AND. kiter > ONE) THEN
             irecalc = Z
         END IF
c
         if(ConstT) then
c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't use hnew=top  --seb Moved above if(irecalc.eq.1) then
              top=BOTM(IX,IY,LBOTM(IZ)-1)
         else
              top = hnew(ix,iy,iz)
              if(top.gt.(BOTM(IX,IY,LBOTM(IZ)-1)))
     &          top=BOTM(IX,IY,LBOTM(IZ)-1)
         end if
         !
         bot = BOTM(IX,IY,LBOTM(IZ))
         thck = top-bot
         IF( thck<NEARZERO_20 ) thck=0.9D0*NEARZERO_20  !seb make it slightly smaller for checks
         !
         if(irecalc.eq.1) then
c-----if the cell is inactive or specified then bypass processing.
c         if(ibound(ix,iy,iz).ne.0 ) then
!            if(LAYHDT(IZ).EQ.0) then
!c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't use hnew=top
!              top=BOTM(IX,IY,LBOTM(IZ)-1)
!            else
!              top = hnew(ix,iy,iz)
!              if(top.gt.(BOTM(IX,IY,LBOTM(IZ)-1)))
!     &          top=BOTM(IX,IY,LBOTM(IZ)-1)
!            end if
!            bot = BOTM(IX,IY,LBOTM(IZ))
!            thck = top-bot
c     Check for SPECIFIED CONDUCTANCE option (LOSSTYPE=4) for node-defined well
          if(LOSSTYPE.EQ.4 .and. NNODES > Z) then
            cond = MNWNOD(11,INODE)
          else
            dx   = delr(ix)
            dy   = delc(iy)
            Txx = MNWNOD(16,INODE)
            Tyy = MNWNOD(17,INODE)
            Qact = MNWNOD(4,INODE)
c
c           If this is not a vertical well with intervals 
c           defined by elevations (i.e. NNODES>0)
            if(NNODES > Z) then
c
              rw    = MNWNOD(5,INODE)
              Rskin = MNWNOD(6,INODE)
              Kskin = MNWNOD(7,INODE)
              B     = MNWNOD(8,INODE)
              Cf    = MNWNOD(9,INODE)
              PLoss = MNWNOD(10,INODE)           
c   compute conductance term for node
              Skin = DZ
              cond = cel2wel2(LOSSTYPE,Txx,Tyy,dx,dy,                   !SCOTT SKIN IS BEING PASSED IN WITH NO VALUE SET, IT ALSO SERVES AS FLAG
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck,Qact,
     &                 WELLID(iw),Skin,iw,ix,iy,iz) !rgn added iw+
c   check cond<0, reset to 0 and print warning
              if(cond < DZ) then
                 CALL MNW2_WARN%ADD(WELLID(IW)//' HAS CWC<0 FOR NODE '//
     +                              NUM2STR(nod,2)//
     +'. NODE WILL BE DEACTIVATED FOR THE TIME STEP'//NL)
!            CALL WARNING_MESSAGE(OUTPUT=IOUT,
!     +      MSG='MNW2 WELLID '//WELLID(IW)//': CWC<0 IN WELL, '//
!     +      'SETTING CWC TO ZERO', INLINE=.TRUE.)
                cond=DZ
                MNWNOD(22,INODE)=DZ
              end if
c   check PPFLAG, if on, alpha defined for each node
              !PPFLAG=INT(MNW2(19,iw))
              if(PPFLAG > Z) then
                alpha   = MNWNOD(19,INODE)
                PPC_CHK = TRUE
              else
                alpha   = UNO
                PPC_CHK = FALSE
              end if
              !
            else
c   else this is a vertical well with intervals defined 
c     by elevations: process it
c   get first and last interval intersecting this node
              firstint= NINT( MNWNOD(12,INODE) )
              lastint = NINT( MNWNOD(13,INODE) )
c   initialize total length of borehole within cell
              totlength=DZ
c   initialize conductance; will be summed for multiple intervals
              cond = DZ
c   initialize specified conductance; will be summed for multiple intervals
              CWC  = DZ
              do iint=firstint,lastint
c   length of interval is ztop-zbotm     
                ztop  = MNWINT(1,iint)
                zbotm = MNWINT(2,iint)
c  LFK (11/30/15)
                MNWNOD(20,INODE) = ZTOP
                MNWNOD(21,INODE) = ZBOTM
c   check boundaries/saturated thickness
c LFK (6/3/15) check if bottom of well screen is above the water table.
c              If yes, well node is dry; set PPFLAG=0; deactivate node.
                if(zbotm.ge.top) then
                  cond            = DZ
                  alpha           = DZ
                  MNWNOD(14,INODE)= DZ
                  MNW2(19,iw)     = DZ
                  rw    = MNWINT(5,iint)
                  Rskin = MNWINT(6,iint)
                  Kskin = MNWINT(7,iint)
                  B     = MNWINT(8,iint)
                  Cf    = MNWINT(9,iint)
                  Ploss = MNWINT(10,iint)
C
                  if (mnwprnt.gt.0) CALL SMNW2COND_WARN%ADD(
     +                   'Open interval above water table; '//
     +                   'Deactivated node '//NUM2STR(nod,2)//
     +                   ' in well '//wellid(iw)//NL ) 
!                  WRITE(iout,83) wellid(iw),inode
!   83 FORMAT(1X,' Open interval above water table; MNW2 node deactivated
!     1',A20,' Node ',I4)
                  CYCLE
                end if
                if(ztop  > top) ztop =top
                if(zbotm < bot) zbotm=bot
                if(ztop  > zbotm) then
                                  lengthint = ztop-zbotm
                else
                                  lengthint = DZ
                endif
c   calculate total length of borehole within cell
                totlength = totlength + lengthint
                if(LOSSTYPE.EQ.4) then
                  if(totlength > NEARZERO_20) then
                    lengthratio = lengthint/totlength
                    CWC = CWC + lengthratio*(MNWINT(11,iint))
                  endif
                else
c   calculate weighting ratio based on full thickness of node
                  ratio = DZ
                  if(thck > NEARZERO_20) ratio=lengthint/thck
c   maximum ratio is 1.0
                  if(ratio > UNO) ratio = UNO
c   use length-weighted ratios for each interval to determine CWC of that interval
                  if(ratio > DZ) then
                    rw    = MNWINT(5,iint)
                    Rskin = MNWINT(6,iint)
                    Kskin = MNWINT(7,iint)
                    B     = MNWINT(8,iint)
                    Cf    = MNWINT(9,iint)
                    Ploss = MNWINT(10,iint)
c  calculate cond, weight it by length in cell (*ratio) and sum to get effective CWC
                    Skin = DZ
                    cond = cond + ratio*(cel2wel2(LOSSTYPE,Txx,Tyy,dx,
     &                 dy,rw,Rskin,Kskin,B,Cf,PLoss,thck,Qact,
     &                 WELLID(iw),Skin,iw,ix,iy,iz))  !RGN added iw+
c   check cond<0, reset to 0 and print warning
                    IF(cond < DZ) THEN
                       cond             = DZ
                       MNWNOD(22,INODE) = DZ
                       CALL MNW2_WARN%ADD(WELLID(IW)//
     +                 'HAS CWC<0 FOR NODE '//
     +                 NUM2STR(nod,2)//
     +              '. NODE WILL BE DEACTIVATED FOR THE TIME STEP'//NL)
!            CALL WARNING_MESSAGE(OUTPUT=IOUT,
!     +      MSG='MNW2 WELLID '//WELLID(IW)//': CWC<0 IN WELL, '//
!     +      'SETTING CWC TO ZERO', INLINE=.TRUE.)
                    END IF
                  else
                      MNWNOD(22,INODE) = DZ !No Screen Interval, deactivate
                  end if
                end if
              end do
c-LFK
             if(LOSSTYPE.EQ.4) cond=cwc
c
c   calculate alpha for partial penetration effect if PPFLAG is on
              !PPFLAG=INT(MNW2(19,iw))
              if(PPFLAG > Z) then
                  alpha = DZ
                if ( thck>NEARZERO_20 ) alpha = totlength/thck
                if(alpha > 0.99 .and. alpha < 1.0) then
                  if (MNWPRNT.gt.1.and.kiter.eq.1) then
                  CALL SMNW2COND_WARN%ADD(
     +               'Penetration fraction > 0.99; '//
     +               'Reset to 1.0 for node '//NUM2STR(nod,2)//
     +               ' in well '//wellid(iw)//NL ) 
!                    nd=INODE-firstnode+1
!                    write(iout,*) 'Penetration fraction > 0.99 for',
!     & ' node ',nd,' of well ',wellid(iw)
!                    write(iout,*) 'Value reset to 1.0 for this well'
                  end if
                  alpha = UNO
                end if
              else
                alpha = UNO
              endif
            end if  
c
c LFK (8/19/15) turn off PP calculation if convertible layer and hwell drops below (zbotm+0.01)
c             set COND=0.0
c              go to 567
            PPC_CHK = TRUE
            IF(.NOT. ConstT .AND. PPFLAG > Z .AND. KITER > 2) then
              hwell   = mnw2(17,iw)
              alpha2  = hwell-zbotm
              PPC_CHK = alpha2 > 0.01
              !
              IF(PPC_CHK  .AND.totlength > DZ) 
     +                               PPC_CHK = alpha2/totlength >= 0.1D0
              !
              IF(KITER > 50) THEN   !Check is not needed anymore, but keeping in just in case LFK had a reason for it -- Added KITER>50
              IF(alpha2 < 0.01D0) then
                PPFLAG = Z
                !MNW2(19,IW) = DZ - commented out to keep PP on for convertible layers
                COND  = DZ
                alpha = DZ
!                if(mnwprnt.gt.0) CALL SMNW2COND_WARN%ADD(
!     +               'hwell < (zbotm+0.01) in convertible layer; '//
!     +               'PPFLAG disabled, set to 0, for well '//
!     +               wellid(iw)//NL ) 
!                WRITE(iout,85) wellid(iw),iw
!   85 FORMAT(1X,' hwell <(zbotm+0.01) in convertible layer; PPFLAG reset
!     1 to 0; WELLNAME = ',A20,'  iw = ',I4)
c       write (iout,*) ' alpha reset to 0.0 '
              ELSE
                alpha3 = UNO
                if(totlength > DZ) alpha3=alpha2/totlength
                !
                if (alpha3 < 0.10) then
                  PPFLAG      = Z
                  MNW2(19,IW) = DZ
                  alpha       = DZ
                  if(mnwprnt.gt.0) CALL SMNW2COND_WARN%ADD(
     +               'hwell at <10% open interval in convertible '//
     +               'layer; PPFLAG disabled, set to 0, for well '//
     +               wellid(iw)//NL ) 
!                  WRITE(iout,87) wellid(iw)
!   87 FORMAT(1X,' MNW2 node in convertible layer & hwell at <10% open in
!     1terval; PPFLAG reset to 0; WELLNAME = ',A20)
c       write (iout,*) ' alpha reset to 0.0 '                
                end if
              END IF  !(alpha2 < 0.01D0) then
              END IF  !(KITER > 50     ) THEN
            END IF
c567    continue  
c
c     Correct conductance calculation for partial penetration effect
c
c     prepare variables for partial penetration calculation
c           only do partial penetration effect if PP>0 and alpha <1.0
            !PPFLAG=INT(MNW2(19,iw))
            IF(PPFLAG > Z .AND. alpha < UNO .AND. PPC_CHK) then    !seb PPC_CHK is used to keep PP on for entire simulation
c
c  use saved partial penetration effect if steady state and past 1st iter
              if(ISS == 1 .and. kiter > 1) then
                dhp = MNWNOD(18,INODE)
              else
c      if transient, update dhp
                T = (Txx*Tyy)**0.5D0
                Kh = DZ
                if ( thck > NEARZERO_20 ) Kh = T/thck
                QQ=Qact*(-1.D0)
                Kz=MNWNOD(33,INODE)
c
                SS = 1.0e-6
                if ( thck > NEARZERO_20 ) SS = MNWNOD(34,INODE) !/(thck*dx*dy) --seb MNWNOD(34,:) already is 1/L dimension
c
c    determine location of well screen in cell
c
c    only calculate this once for each well, then save topscreen and bottomscreen
c    topscreen (MNWNOD(20) is flagged as 1d30 until it is set
                if(MNWNOD(20,INODE).eq.1d30) then
c             if a vertical well
                 if(NNODES < Z) then
c    if firstint=lastint for this node, it is the only interval, so use exact
c    location of borehole for analytical calculation
                  if(firstint.eq.lastint ) then            
                    topscreen=ztop
                    bottomscreen=ztop-totlength
c    if multiple screens in a confined (constant thck) cell, assume in middle
c    (calculation: from the top, go down 1/2 the amount of "unscreened" aquifer
                  else  
                    IF(ConstT) then
                      topscreen=top-((thck-totlength)*HALF)
                      bottomscreen=topscreen-totlength
c    if multiple screens in an unconfined (WT) cell, assume at bottom of last screen
                    else
c    (zbotm works here as it is the last thing set in the interval loop above)
                      topscreen=zbotm+totlength 
                      bottomscreen=zbotm
                    end if
                  end if
c             save top and bottom of screen
                  MNWNOD(20,INODE)=topscreen
                  MNWNOD(21,INODE)=bottomscreen
c             else if not a vertical well
c          
                 else
c             alpha specified; calculate length of screen
                  totlength=thck*alpha
c                 if confined (constant thck), assume borehole in middle             
                  IF(ConstT) then
                    topscreen=top-((thck-totlength)*HALF)
                    bottomscreen=topscreen-totlength
c             if unconfined, assume borehole at bottom of cell             
                  else
                    topscreen=bot+totlength
                    bottomscreen=bot                    
                  end if                                          
c             save top and bottom of screen
                  MNWNOD(20,INODE)=topscreen
                  MNWNOD(21,INODE)=bottomscreen

                 end if
c             if topscreen and bottomscreen have been calculated, retrieve them
                else
                  topscreen   =MNWNOD(20,INODE)
                  bottomscreen=MNWNOD(21,INODE)
                end if
c
c from top and bottom of screen info, calculate ZPD and ZPL for PPC routine  
                ZPD=top-topscreen
                ZPL=top-bottomscreen
c if ZPD is less that zero, the screen is at the "top", so set ZPD=0
                if(ZPD < DZ) ZPD = DZ
c LFK (6/5/15) add check on negative ZPL
                if(ZPL < DZ) ZPL = DZ
c
C  LFK (5/28/15) don't calculate pp correction if alpha=0 or q-node reset to 0
C                  or cond reset to 0 or ppflag reset to 0
                ippc = Z
                if (alpha < 1D-3) ippc=1
                if (zpd==DZ .and. zpl==DZ) ippc=ippc+2
                if (cond==Z)     ippc=ippc+3               !seb changed mnwnod(14,inode) to cond
                if (ppflag==Z)   ippc=ippc+4
                if (zpd==zpl)    ippc=ippc+5
                if (ITFLAG==Z)   ippc=ippc+6               !seb since mnwnod(14,inode)=zero when ITFLAG=0, but now use COND for check
                if (MNWNOD(22,INODE) < HALF ) ippc=ippc+7 !seb node was deactivated
c calculate dhp (Delta-H due to Penetration) using analytical solution          
C
              if(ippc == Z) then
c
                CALL PPC(dhp,ISOLNFLAG,thck,Kh,Kz,SS,QQ,rw,ZPD,ZPL)
c          
              else
                  if(zpd == DZ .and. zpl == DZ) then
                    PPFLAG      = Z
                    MNW2(19,IW) = DZ
                  ! dhp=0.0
                  ! MNWNOD(18,INODE)=dhp
                  end if
                  ISOLNFLAG       = ONE         !Set to true only to keep code moving forward
                  dhp             = DZ
                  MNWNOD(18,INODE)= dhp
              end if
              if (PPFLAG == Z) GO TO 5
c  LFK  end code change
c          
c  if analytical solution failed, report no partial penetration and set dhp=0.0
                if(ISOLNFLAG == Z .AND. ITFLAG > Z .AND. QQ.ne.DZ) then
c  if alpha <= 0.2, shut well off if PPC did not converge
                  if(alpha < 0.2) then
                   if (MNWPRNT > 1) then
                       CALL SMNW2COND_WARN%ADD(
     +               'Partial penetration solution did not converge; '//
     +               'penetration fraction < 0.2; Resetting CWC= 0.0 '//
     +               'for node '//NUM2STR(nod,2)//
     +               ' in well '//wellid(iw)//NL ) 
!                    nd=INODE-firstnode+1
!                    write(IOUT,*) 'Partial penetration solution did not
!     & converge; penetration fraction < 0.2,      resetting CWC= 0.0 for
!     & node '
!     & ,nd,' of well ',wellid(iw)
                   end if
                   cond            = DZ
                   MNWNOD(22,INODE)= DZ 
                  else
c  if alpha > 0.2, set PPC effect = 0 if did not converge
                   if (MNWPRNT.gt.1) then
                       CALL SMNW2COND_WARN%ADD(
     +               'Partial penetration solution did not converge; '//
     +               'penetration fraction > 0.2; '//
     +               'PPFLAG is disabled, set to 0, for well '//
     +                wellid(iw)//NL ) 
!                     nd=INODE-firstnode+1
!                     write(IOUT,*) 'Partial penetration solution did not
!     & converge; penetration fraction > 0.2,      assume full 
!     & penetration for
!     & node ',nd,' of well ',wellid(iw)
                   end if
                   ppflag     = Z
                   mnw2(19,iw)= DZ
                   dhp        = DZ
                  endif
                end if
c  store partial penetration effect (dhp)         
                MNWNOD(18,INODE)=dhp
              end if              
c             end if recalc dhp
c
c  correct partially penetrating node-defined cells by ratio of screenlength/satthck
              if(NNODES > Z) then
                  ratio = DZ
                if ( thck > NEARZERO_20) ratio=(topscreen-bottomscreen)/
     &                                       thck
                cond=cond*ratio
              end if
c             
c  re-calculate conductance to include partial penetration
c    calculate dpp (partial penetration effect with specific Q) if Q and dhp "align" correctly 
c    eg if removing water (Q<0), dhp should be positive
c    (dhp>0 signifies drawdown).  Q is either <> 0 so no div 0 problem
              if(ITFLAG == ONE
cc     &          .AND.(Qact.lt.0.D0.AND.dhp.gt.0.D0)
cc     &          .OR.(Qact.gt.0.D0.AND.dhp.lt.0.D0)) then
c  LFK 6/2012  change check on dhp to include 0.0 to avoid unneeded warnings
     &          .AND.(Qact < DZ .AND. dhp >= DZ)
     &          .OR. (Qact > DZ .AND. dhp <= DZ) ) then
                  dpp = DZ
                  if (abs(Qact)>NEARZERO_20) dpp=dhp/(DNEG*Qact)
                if(cond > NEARZERO_20 .AND. ABS(dpp) > NEARZERO_20 )then
                  ABC=UNO/cond
                  ABCD=ABC+dpp
                  cond=UNO/ABCD
                end if
              else if (ITFLAG==ONE .and. Qact.ne.DZ .and. dhp > DZ) then
                dpp = DZ
                CALL MNW2_WARN%ADD(WELLID(IW)//
     +          'HAS PARTIAL PENETRATION '//
     +          'TERM (dpp) SET TO 0.0 DUE TO MISALIGNMENT OF DHP= '//
     +          NUM2STR(dhp)//' and Q='//NUM2STR(Qact)//NL)
!            CALL WARNING_MESSAGE(OUTPUT=IOUT,
!     +      MSG='MNW2 WELLID '//WELLID(IW)//': PARTIAL PENETRATION '//
!     +      'TERM (dpp) SET TO 0.0 DUE TO MISALIGNMENT OF DHP= '//
!     +      NUM2STR(dhp)//' and Q='//NUM2STR(Qact), INLINE=.TRUE.)
!                  write(IOUT,*) '***WARNING***  Partial penetration term
!     & (dpp) set to 0.0 due to misalignment of dhp= ',dhp,' and Q=',Qact
c LFK 6/2012  add well name to output
!     &, ' WELLNAME = ',wellid(iw)
              end if     
            END IF              
c           end if PP effect
C--LFK
    5       continue
c
          endif
c         endif LOSSTYP EQ 4 and NNODES GT 0
c        Save conductance of each node
          MNWNOD(14,INODE) = cond
c        endif irecalc=1
         else ! if irecalc=0, use saved cond
          cond = MNWNOD(14,INODE)
          B    = MNWNOD(8,INODE)         !Needed for some output
         endif   !(irecalc.eq.1) then
c        output node info  
c  if more than one interval made up this node, write composite 
         if(MNWNOD(12,INODE).ne.MNWNOD(13,INODE)) then
c--LFK
            ctext=' COMPOSITE'
         else
            ctext='          '
         end if        
c only write screen info for cells that have partial penetration
         if(ipr == 1) then
           !PPFLAG=INT(MNW2(19,iw))
           if(PPFLAG > Z .and. alpha < UNO) then
c LFK 12/1/15       Determine correct top and bottom of open interval for printing:
             if (nod > ONE) then
                if (topscreen > top) then
                  topscreen=top
                end if
                if (mnwnod(21,inode) < bot) then
                  bottomscreen=bot
                end if
             end if
c
             if(LOSSTYPE == 2) then
               write(iout,'(A15,I3,1P7G12.5,ES12.4,10A)') 
     & WELLID(iw),nod,cond,
     & top,bot,topscreen,bottomscreen,alpha,Skin,B,ctext
             else 
               write(iout,'(A15,I3,1P6G12.5,12A,12A,10A)') 
     & WELLID(iw),nod,cond,
     & top,bot,topscreen,bottomscreen,alpha,'     N/A    ',
     & '     N/A    ',ctext
             end if
           else
c for no partial penetration, just repeat top and bot of layer
c       unless open interval defined by elevations    (chng 8/24/15:LFK)
c
c LFK 12/1/15       Determine correct top and bottom of open interval for printing:
             t1=top
             t2=bot
             if (nnodes.lt.0.and.mnwnod(20,inode).lt.1e09) then
                if (mnwnod(20,inode).gt.top) then
                  t1=top
                else  
                  t1=mnwnod(20,inode)
                end if
                if (mnwnod(21,inode).lt.bot) then
                  t2=bot
                else  
                  t2=mnwnod(21,inode)
                end if
                if (mnwnod(21,inode).gt.top) then
                  t1=mnwnod(20,inode)
                  t2=mnwnod(21,inode)
                end if
             end if
c
             if(LOSSTYPE.eq.2) then
               if(nnodes.gt.0) then
                 write(iout,'(A15,I3,1P7G12.5,ES12.4,10A)') 
     &             WELLID(iw),nod,cond,   
     &             top,bot,top,bot,alpha,Skin,B,ctext      
c-LFK   change following line 8/24/2015
c     &top,bot,mnwnod(20,inode),mnwnod(21,inode),alpha,Skin,B,ctext   
               else
                 write(iout,'(A15,I3,ES12.4,1x,7G12.4,10A)') 
     &                  WELLID(iw),nod,cond,
     &                  top,bot,t1,t2,alpha,Skin,B,ctext
c     & top,bot,mnwnod(20,inode),mnwnod(21,inode),alpha,Skin,B,ctext
               end if
c LFK 11/30/15
             else if (losstype.eq.4) then
               if(nnodes.gt.0) then
                 write(iout,'(A15,I3,1PG12.4,1x,7G12.4,10A)') 
     &               WELLID(iw),nod,cond,top,bot,top,bot,
     &               '     N/A    ','     N/A    ','     N/A    ',ctext
               else
                 write(iout,'(A15,I3,1PG12.4,1x,7G12.4,10A)') 
     &               WELLID(iw),nod,cond,top,bot,t1,t2,'     N/A    ',
     &               '     N/A    ','     N/A    ',ctext
c     & top,bot,mnwnod(20,inode),mnwnod(21,inode),'     N/A    ',
               end if
c             
             else
c LFK 11/24/15
c             if(nnodes.gt.0) then
c             if(nnodes.gt.0.or.alpha.eq.1.0) then
              if(nnodes > Z .and. alpha == UNO) then
               if (ppflag.eq.1) then
                 write(iout,'(A15,I3,1PG12.4,1x,6G12.4,12A,12A,10A)') 
     &             WELLID(iw),nod,cond,top,bot,top,bot,alpha,
     &             '     N/A    ','     N/A    ',ctext
               else
                 write(iout,'(A15,I3,1PG12.4,1x,6G12.4,12A,12A,10A)') 
     &             WELLID(iw),nod,cond,top,bot,top,bot,
     &             '     N/A    ','     N/A    ','     N/A    ',ctext
c     &top,bot,top,bot,alpha,'     N/A    ',
               end if
              else
c             
               if (ppflag.eq.1) then
                 write(iout,'(A15,I3,1PG12.4,1x,6G12.4,12A,12A,10A)') 
     &              WELLID(iw),nod,cond,top,bot,t1,t2,alpha,
     &              '     N/A    ','     N/A    ',ctext
               else
                 write(iout,'(A15,I3,1PG12.4,1x,6G12.4,12A,12A,10A)') 
     &              WELLID(iw),nod,cond,top,bot,t1,t2,
     &              '     N/A    ','     N/A    ','     N/A    ',ctext
c     & top,bot,mnwnod(20,inode),mnwnod(21,inode),alpha,'     N/A    ',
c     & top,bot,t1,t2,alpha,'     N/A    ',
               end if
              end if
             end if
           end if
         end if
        enddo
c       enddo loop over nodes
       endif
c      endif horizontal well check
       endif
c      endif active node
      enddo
c     enddo loop over wells
c
c     write note about CWC values
c     if transient, by kiter=1 , if not, by tstep=1 
      if(ipr.eq.1) then
      write(iout,'(120A)') '* Cell-to-well conductance values (CWC) may 
     &change during the course of a stress period'
        write(iout,*) 
      end if
c
      return
      end
c
c
c_________________________________________________________________________________
c
      DOUBLE PRECISION function cel2wel2(LOSSTYPE,Txx,Tyy,dx,dy,
     +                 rw,Rskin,Kskin,B,Cf,PLoss,thck,Q,WELLNAME,Skin,
     +                 iw,C1,R1,L1)
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
c     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,          ONLY: BOTM,LBOTM,HNEW,LAYHDT
      USE GWFMNW2MODULE,   ONLY: mnw2,IOUT
      USE ERROR_INTERFACE, ONLY: WARNING_MESSAGE
      USE CONSTANTS,       ONLY: PI, TWOPI, ONE, UNO, DZ, NEARZERO_20
      IMPLICIT NONE
!External functions
!      DOUBLE PRECISION SMOOTH2  !seb replaced by new interface
!      EXTERNAL SMOOTH2
!
      CHARACTER(20) WELLNAME
      INTEGER LOSSTYPE,iw,C1,R1,L1
C--LFK
      INTEGER ISEGFLG
C
      DOUBLE PRECISION rw,Txx,Tyy,yx4,xy4,ro,dx,dy,Tpi2,A,
     + Ploss,B,Rskin,Kskin,C,Cf,Q,thck,T,Tskin,Skin,TOP,BOT,HED,SF,
     + RoRw, Tpi2_I!,h,dc 
      !!!INTERFACE
      !!!  DOUBLE PRECISION FUNCTION smooth2(h,dc)
      !!!    DOUBLE PRECISION h, dc
      !!!  END FUNCTION SMOOTH2      
      !!!END INTERFACE
C     ------------------------------------------------------------------
C
!!! RGN added next 3 lines.  seb commented out
!!!      h =  (mnw2(17,iw)-BOTM(C1,R1,L1))
!!      h =  (HNEW(C1,R1,lbotm(L1))-BOTM(C1,R1,lbotm(L1)))
!!      IF( h.LT.0.0 ) h = 0.0
C-LFK
      ISEGFLG=0
C      
      if (thck < NEARZERO_20) THEN
          cel2wel2 = DZ
      ELSEIF(LOSSTYPE.EQ.0) THEN   ! For the "NONE" option, multiply the Kh by 10000 to equivalate Hnew and hwell
          !
          cel2wel2=1.0D4*((Txx*Tyy)**0.5D0)/thck
          !
      !ELSEIF(LOSSTYPE.EQ.4) THEN   ! Should never be true, but here for completeness (SecifyCWC)
      !
      ELSEif( rw.lt.NEARZERO_20 ) THEN  !No flow cause well is super small
              cel2wel2 = DZ
      ELSEif( Txx.lt.NEARZERO_20.or.Tyy.lt.NEARZERO_20 )
     +  then
        cel2wel2 = ( Txx * Tyy )** 0.5D0
      else
        IF( Txx == Tyy ) THEN
            !yx4 = 1D0
            !xy4 = 1D0
            IF(dx == dy) THEN
                ro = 0.5D0 * 0.28D0 * 1.4142135623731D0 * dx   ! 1.414 = SQRT(2)
            ELSE
                ro = 0.5D0 * 0.28D0 * SQRT((dx*dx) + (dy*dy))  ! -> / (yx4+xy4) = *0.5
            END IF
            !
            Tpi2 = TWOPI * Txx    ! TWOPI = 2.D0 * pi
        ELSE
            yx4 = (Tyy/Txx)**0.25D0
            xy4 = (Txx/Tyy)**0.25D0
            ro = 0.28D0 *((yx4*dx)**2 +(xy4*dy)**2)**0.5D0 / (yx4+xy4)
            !
            Tpi2 = TWOPI * (Txx*Tyy)**0.5D0   ! TWOPI = 2.D0 * pi
        END IF
        !
        Tpi2_I = NEARZERO_20
        IF(Tpi2 > NEARZERO_20) Tpi2_I = UNO/Tpi2
c       if ro/rw is <1, 'A' term will be negative.  Warn user and cut off flow from this node
        !
        RoRw  =  ro/rw
        if (RoRw.lt.1.D0) then
            CALL WARNING_MESSAGE(OUTPUT=IOUT,
     +      MSG='MNW2 WELLID '//WELLNAME//': Ro/Rw < 1, CWC SET TO 0.0',
     +      INLINE=.TRUE.)
!            write(iout,*) 
!     +      '     Ro/Rw =  ',RoRw, 
!     +      '***WARNING*** Ro/Rw < 1, CWC set = 0.0 for well ',WELLNAME
          cel2wel2 = 0.D0
          GOTO 888
        end if
        !
        A = log(RoRw) * Tpi2_I  !log(ro/rw) / Tpi2
c--LFK--Dec 2012   IF Vert.Segment, Length & Cond. = 1/2 of calc. value.
          IF (SKIN.GE.HUGE(SKIN)) THEN
            A=A*2.0
            ISEGFLG=1
            SKIN=0.0
          END IF
c
c       THEIM option (LOSSTYPE.EQ.1) only needs A, so no need to calculate  B or C
c
c       SKIN (LINEAR) option, calculate B, C=0
        if(LOSSTYPE.EQ.2) then
c         average T in aquifer assumed to be sqrt of Txx*Tyy
          IF(Txx == Tyy) THEN
              T = Txx
          ELSE
              T = SQRT(Txx*Tyy)
          END IF
          Tskin = Kskin*thck
c--LFK--Dec 2012    Check for segment calculation
          IF (ISEGFLG.EQ.1) THEN
            Tskin=Tskin*0.5
          END IF
C
          if(Tskin.gt.NEARZERO_20.and.rw.gt.NEARZERO_20.and.
     +       Tpi2>NEARZERO_20) then
c         this is from eqs 3 and 5 in orig MNW report
            Skin = ((T/Tskin)-1)*(DLOG(Rskin/rw))
            B = Skin  * Tpi2_I !/ Tpi2
          else
            B = 0.D0
          end if
          C = 0.D0
c       GENERAL option, calculate B and C
       else if (LOSSTYPE.EQ.3) then
          if(ABS(Cf) < 1D-30) then   !if(Cf.NE.0.0) then  seb
            C = Cf * abs(Q)**(PLoss-1)
          else
            C = 0.D0
          end if
c--LFK--Dec 2012    Check for segment calculation
          IF (ISEGFLG.EQ.1) THEN
            C = C*2.0
          END IF
C
       else
          B = 0.D0
          C = 0.D0
       end if
        cel2wel2 = A + B + C 
        !if ( cel2wel2>NEARZERO_20 )cel2wel2 = 1.0D0 / cel2wel2
        IF ( cel2wel2 < NEARZERO_20 .AND. A > NEARZERO_20) THEN
                                           cel2wel2 = UNO / A  !SWITCH LOSS TYPE TO THIEM DUE TO VERY SMALL VALUE
        ELSEIF ( cel2wel2 < NEARZERO_20) THEN
                                           cel2wel2 = 1D25    !Small A so large conductance
        ELSE
                                           cel2wel2 = UNO / cel2wel2
        END IF
      endif
!      IF ( LAYHDT(L1).GT.0 .AND. Iuupw.NE.0 ) THEN  --NO SMOOTHING ONTOP OF A SEEPAGE FACE, WHICH ALREADY IS A SMOOTHED RESULT!
!         TOP=BOTM(C1,R1,lbotm(L1)-1)                                    ! seb ADDED ADDITIONAL CHECKS FOR WHEN HNEW>TOP, HNEW<BOT and TOP>HNEW>BOT
!         BOT=BOTM(C1,R1,lbotm(L1)  )
!         HED=HNEW(C1,R1,L1)
!         IF    ( HED.LT.BOT )THEN                                       !HNEW<BOT, SO SET THICKNESS TO 0
!           h=0D0                                                        
!         !ELSEIF( HED.GT.TOP )THEN                                       !HNEW>TOP, SO USE CELL THICKNESS
!         !  h=TOP-BOT                                                    
!         ELSE                                                           !TOP>HNEW>BOT, SO SET THICKNESS TO HNEW-BOT
!           h=HED-BOT
!         END IF
!         SF=smooth2(h,dc)
!         IF(SF<1D0)WRITE(IOUT,*)
!     +    'MNW2 NODE CONDUCTANCE REDUCED COND:',WELLNAME,SF
!         cel2wel2 = cel2wel2*SF
!      END IF
c
 888  return
      end
c
!      DOUBLE PRECISION FUNCTION smooth2(h,dc)
!! h is the depth 
!! dC is the derivative of well conductance with respect to well head
!      IMPLICIT NONE
!      DOUBLE PRECISION h, Bot, s, aa, ad, b, x, y, dc
!      smooth2 = 0.0D0
!      s = 1.0d-5
!      x = h
!      IF ( x-s.GT.-1.0d-14 ) THEN
!        y = 1.0D0
!        dc = 0.0
!      ELSEIF ( x.LE.0.0 ) THEN
!        y = 0.0D0
!        dc = 0.0
!      ELSE
!        aa = -1.0d0/(s**2.0d0)
!        ad = -2.0D0/(s**2.0d0)
!        b = 2.0d0/s
!        y = aa*x**2.0d0 + b*x
!        dc = (ad*x + b)
!      END IF
!      smooth2 = y
!      END FUNCTION smooth2
c
c
c_________________________________________________________________________________
c
c
c_________________________________________________________________________________
c
      subroutine SMNW2SEEP(IGRID,KPER,KSTP,iw,kiter)
c
c     ******************************************************************
c     determine q
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
     
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,HNEW,LAYHDT
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,IOUT,MNW2_WARN,
C-LFK     1                       NODTOT,MNW2,MNWNOD,SMALL,WELLID
     1                       NODTOT,MNW2,MNWNOD,SMALL,WELLID,LIMQ,
     2                       HAS_NWT
      USE NUM2STR_INTERFACE, ONLY: NUM2STR
      USE CONSTANTS,         ONLY: NL, DZ, UNO, DOS, DNEG, inf, HALF,
     +                             Z, ONE, TWO,
     +                             NEARZERO_20, NEGNEARZERO_20
      IMPLICIT NONE
      INTEGER, INTENT(IN):: IGRID, KPER, KSTP, iw, kiter
      INTEGER firstnode,lastnode,nd  
      DOUBLE PRECISION:: qdes, qact, Qseep, seepchk
      DOUBLE PRECISION:: Hcell, csum, chsum,
     & hwell,hlim,hmax,hsim,bottom,qoff,qon,qsmall,
     & qpot,cond,ratio, wel_bot
      DOUBLE PRECISION:: SGN
      INTEGER:: NNODES, INODE
      INTEGER:: QCUT, CapFlag2, QlimFlag
      INTEGER:: IL, IC, IR, kSeep
      LOGICAL:: CHKER, SOLV_NODE, QLIMIT, HasQ
C
      !verysmall = 1.D-25
C-LFK   initialize MNWNOD(15,IW) TO HIGH VALUE
C      INODE = MNW2(4,IW)                   !THis Inititialization commented out by Lenny Dec-2012??
      !iseepflg = Z
C      MNWNOD(15,INODE) = 1.0D31            !THis Inititialization commented out by Lenny Dec-2012??
      !
      NNODES   = abs(MNW2(2,iw))
      !
      firstnode = NINT( MNW2(4,iw) )
      lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
      !
      qdes   = mnw2(5,iw)
      HasQ   = ABS(qdes) > NEARZERO_20
      Qsmall = small*abs(qdes)
      qoff   = DZ
      qon    = DZ
      !
      QLIMIT = NINT(MNW2(6,iw)) .NE. Z
      hwell  = mnw2(17,iw)
      !
      CapFlag2 = NINT(mnw2(27,iw))
      !
c
c  Flag to perform this check is mnw2(20,iw).  If > 0, limit has been met
c  Do not enforce q-limit oscillation-stopper until the 3rd iteration; hardwire=0
c    for first two iterations
      IF(.NOT. QLIMIT) THEN
                             QlimFlag    = Z
      ELSEIF(kiter.le.2) THEN
                             QlimFlag    = Z
                             mnw2(20,iw) = DZ
      ELSE
c QFM  rechecking Q cutoff within FM routine
c skip this whole thing if 1st or 2nd iter
          !
c   Retrieve Qfrcmn, Qfrcmx, Qdes
          QCUT     = NINT(MNW2(8,iw))
          QlimFlag = NINT(mnw2(20,iw))
          if(QCUT .ne. Z) then
           qoff = mnw2(9,iw)
           qon  = mnw2(10,iw)
           if(QCUT > Z) then
c     convert rate into fraction of Qdes (ratio is used to compare)
            if(HasQ) then
              qoff=abs(qoff/Qdes)
              qon=abs(qon/Qdes)
c            else
c              qoff=0.0
c              qon=0.0
            end if
           end if
          end if
c
c   Compute hwell / Qpot for multi-node well (not single-cell wells)
c   
            csum  = DZ
            chsum = DZ
            qact  = DZ
c   Loop over nodes in well
            !firstnode=MNW2(4,iw)
            !lastnode=MNW2(4,iw)+NNODES-1
            !
            do INODE=firstnode,lastnode
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)                                         !SCOTT SHOULD HNEW<BOTM BE INCLUDED, THERE STILL IS POTENTIAL FLOW IF hwel>botm
              !
              Hcell = HNEW(IC,IR,IL)
              !
              IF(IBOUND(ic,ir,il) .ne. Z) THEN
                !
                IF(.NOT. HAS_NWT .OR. LAYHDT(IL) == Z) THEN
                        !
                        CHKER = .TRUE.
                        !
                ELSEIF( LAYHDT(IL) > Z .AND. 
     +                  HNEW(IC,IR,IL) > BOTM(IC,IR,LBOTM(IL)) ) THEN
                        !
                        CHKER = .TRUE.
                ELSE
                        CHKER = .FALSE.
                END IF
                !
                IF(CHKER) THEN
                         csum  = csum  + MNWNOD(14,INODE)
                         chsum = chsum + MNWNOD(14,INODE)*Hcell
                         qact  = qact  + MNWNOD(4,INODE)
                END IF
              END IF
            end do
c---div0 ---  CSUM could go to zero if the entire well is dry
            if( csum > NEARZERO_20 ) then
              hwell = ( qdes + chsum ) / csum
            else
              hwell = Hcell
            endif
c      Test Hlim constraint if QLIMIT flag is set
            if( QLIMIT ) then
              hlim = mnw2(7,iw)
              !
              if    ( qdes < NEGNEARZERO_20) THEN
                                              SGN = DNEG
              ELSEif( qdes >    NEARZERO_20) THEN
                                              SGN = UNO
              ELSE
                                              SGN = DZ
              END IF
              !
              hmax = SGN * hlim 
              hsim = SGN * hwell 
c      Potential Q is...
              if( hsim > hmax )  hwell = hlim
              !
              qpot = hwell*csum - chsum
            end if
            cond = csum
c    
c  Compute ratio of potential/desired flow rates
            if(QCUT /= Z) then
             ratio = UNO
             if( HasQ ) ratio =  qpot / qdes
             if( ratio .gt. 0.9999D0 ) then
                                       ratio =  UNO
                                       Qpot  = Qdes
             endif
c  Check if potential flow rate is below cutoff
c  If so, set Qact = 0.0
c  If q-limit condition is met, do not perform this check on subsequent iterations to
c    avoid oscillations
c  Flag to perform this check is mnw2(20,iw).  If > 0, limit has been met
c  Do not enforce q-limit oscillation-stopper until the 3rd iteration; hardwire=0
c    for first two iterations
             if( ratio .lt. Qoff . and. QlimFlag .ge. Z) then
c              mnw2(18,iw) = 0.D0
              mnw2(30,iw) = DZ
c  Set flag to avoid rechecking q-limit
              mnw2(20,iw) = DNEG
c  Check if potential flow rate is above restart threshold
c  If so, set Qact = Qpot
c  If q-limit condition is met, do not perform this check on subsequent iterations to
c    avoid oscillations
c  Flag to perform this check is mnw2(20,iw).  If > 0, limit has been met
             elseif( ratio.gt.Qon .and. abs(qact).lt.Qsmall .and. 
     &              mnw2(20,iw) .le. DZ ) then
c              mnw2(18,iw) = Qpot
              mnw2(30,iw) = Qpot
c  Set flag to avoid rechecking q-limit
              mnw2(20,iw) = UNO
             elseif( ratio.lt.Qon .and. ratio.gt.Qoff ) then
              mnw2(20,iw) = DOS
             else
c  Otherwise leave the flow rate alone
             endif
            endif
c  End if, QLimit>0
          QlimFlag = NINT(mnw2(20,iw))
      endif
      !
      qdes = mnw2(5,iw)
      qact = qdes
c   If Q in last TS was restricted, update qact
c      if(kstp.gt.1) then
        if( abs(mnw2(18,iw)-qdes).gt.Qsmall ) qact = mnw2(18,iw)
        !
c      end if
c   If restrictions were set this time step, update qact
       if( QlimFlag /= Z .or. CapFlag2 /= Z ) then
C-lfk         if((abs(mnw2(29,iw)).lt.abs(mnw2(18,iw)))) then   [subst.30 below]?
         if((abs(mnw2(29,iw)).lt.abs(mnw2(30,iw)))) then
           if(CapFlag2 /= Z) then
             mnw2(18,iw) = mnw2(29,iw)
             LIMQ(1,IW)  = ONE
           else
             mnw2(18,iw) = mnw2(30,iw)
             LIMQ(2,IW)  = ONE
           end if
        else
           if(CapFlag2 == Z) then
             mnw2(18,iw) = mnw2(30,iw)
             LIMQ(2,IW)  = ONE
           else
             mnw2(18,iw) = mnw2(29,iw)
             LIMQ(1,IW)  = ONE
           end if
        end if
        !
        if(CapFlag2 /= Z .and. QlimFlag /= Z) then
            if(abs(mnw2(29,iw)) < abs(mnw2(30,iw))) then
                 mnw2(18,iw)=mnw2(29,iw)
C--LFK           PUMP-CAPACITY constraint over-rides Hlimit constraint; 
C                   set flag to stop checking Hlim constraint this Time Step 
C                   and use spec. flux bdy. cond.
c-lfk temp hold                 mnw2(20,iw)=-2
                 CapFlag2    = TWO
                 mnw2(27,iw) = DOS
                 LIMQ(1,IW)  = ONE
                 LIMQ(2,IW)  = Z
            else
                 mnw2(18,iw) = mnw2(30,iw)
                 LIMQ(1,IW)  = Z
                 LIMQ(2,IW)  = ONE
            end if
         end if
         qact = mnw2(18,iw)
      end if
c
C-LFK   Skip seepage face calculations for single-node well
c--lfk
c     IF (NNODES.EQ.1) GO TO 100
      wel_bot = huge(wel_bot)
c  Make 2 passes to find seepage faces
      do kSeep = 1, 2          
        csum  = DZ
        chsum = DZ
        Qseep = DZ
        !
c  Loop over nodes in well
        do INODE=firstnode,lastnode
            il=MNWNOD(1,INODE)              
            ir=MNWNOD(2,INODE)              
            ic=MNWNOD(3,INODE)              
            nd=INODE-firstnode+1
            !
            Bottom = BOTM(ic,ir,LBOTM(il))
            Hcell  = Bottom
            
c  First time through, set the hwell_by_node to big; this will be used as a flag for 
c  whether or not there is a seepage face in the cell
            if( kSeep == ONE )  then
                         MNWNOD(15,INODE) = inf
                         if( wel_bot > Bottom ) wel_bot = Bottom
            end if
            !
            if(IBOUND(ic,ir,il) /= Z) then
              Hcell = HNEW(ic,ir,il)
c
c  only allow seepage or flow into a cell if the head in the cell is greater than the bottom of the cell
c
              IF(Hcell /= HDRY .AND. Hcell > bottom)  THEN
c  Second time through loop, now that we have a guess for hwell, check to see if there is a seepage face
c  If so, use the bottom (instead of the low hwell) as the gradient driver in the qact calc
c  Set the hwell_by_node to the bottom to flag this as a seepage face cell
c  Sum the seepage (there may be more than one node's worth
                if( kSeep > 1 .and. hwell < Bottom )then
                  MNWNOD(4,INODE)  = (bottom - hnew(ic,ir,il))
     +                                                * MNWNOD(14,INODE)
                  MNWNOD(15,INODE) = bottom
                  Qseep = Qseep + MNWNOD(4,INODE)
                else 
                  csum  = csum  + MNWNOD(14,INODE)
                  chsum = chsum + MNWNOD(14,INODE)*Hcell
                endif
              ELSE
                MNWNOD(4, INODE) = DZ
                MNWNOD(14,INODE) = DZ
                MNWNOD(15,INODE) = hdry
              END IF
            elseif(MNWNOD(22,INODE) < HALF ) THEN  !Node was deactivated
                MNWNOD(4, INODE) = DZ
                MNWNOD(15,INODE) = hdry
            else
                MNWNOD(4, INODE) = DZ
                MNWNOD(15,INODE) = hdry
                MNWNOD(22,INODE) = DZ
c  write message that inode was deactivated this time step
                if(kseep > ONE .and. mnwprnt > ONE) then
            CALL MNW2_WARN%ADD(WELLID(IW)//'HAS DEACTIVATED NODE '//
     +      NUM2STR(ND,-4)//' FOR THIS TIME STEP DUE TO IBOUND=0'//NL)
!        write(IOUT,210) ND,WELLID(iw)
C-LFK rev. format 11/2012
c 210 FORMAT ('Node no. ',I3,'  of Multi-Node Well ', A20)
  210 FORMAT ('MNW2: Node no. ',I3,'  of Multi-Node Well ', A20,
     +        '  deactivated this time step because IBOUND=0')
                end if
            end if
        end do           
c  End loop over nodes in well
c---div0 ---  CSUM could go to verysmall if the entire well is dry
        if( csum > NEARZERO_20 ) then
          hwell = ( qact - Qseep + chsum ) / csum  ! (Desired pumping, Seepage Flow, Intraborehol flow)/CSUM
        elseif(kSeep == ONE) then
          hwell = Hcell  ! hnew(ic,ir,il) -- Well is totally dry so set water level to bottom
        else
          hwell = wel_bot  ! Well is dry, use bottom elevation as well head.
        endif
c   Because the q/hwell may now be different due to seepage flow, need to re-check constraints
c   Test Hlim constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
        if( QLIMIT .and. csum > NEARZERO_20) then
          hlim = mnw2(7,iw)
          !
          if    ( qdes < NEGNEARZERO_20) THEN
                                          SGN = DNEG
          ELSEif( qdes >    NEARZERO_20) THEN
                                          SGN = UNO
          ELSE
                                          SGN = DZ
          END IF
          !
          hmax = SGN * hlim 
          hsim = SGN * hwell 
c
          if( hsim > hmax ) then
            hwell = hlim
            qact = hwell*csum - chsum + Qseep
            !
            LIMQ(2,IW) = ONE
c      Hlim constraints that stop production are not tested until after the 2nd iteration
            if( kiter > TWO ) then
              ratio = UNO
              if( HasQ ) ratio =  qact / qdes
              if( ratio < 0.00001D0 ) then
                qact  = DZ
                hwell = ( qact - Qseep + chsum ) / csum
              endif  
            endif   !! potentially stop all production after Kiter = 2 
          endif     !! End Hlim exceedence loop
        endif  !! Qlimit>0
      enddo  !!  kSeep -- End of seepage face corrector here
c
c  Loop over nodes in well, assign flow rates and water levels
c  use qdes to sum q's at end
      qdes = DZ
      csum = DZ
c-lfk  10/10/2012
      !iseepchk=0
      seepchk=DZ
      !
      do INODE=firstnode,lastnode
        il=MNWNOD(1,INODE)              
        ir=MNWNOD(2,INODE)              
        ic=MNWNOD(3,INODE)              
c  Qseep flag (MNWNOD(15): if still 1E31 here, there is no seepage face so use actual hwell to calc Q
c (if not set here, hwell < bottom and MNWNOD(15) stores value of bottom to calculate seepage, see above)
c                           
        SOLV_NODE = MNWNOD(22,INODE) > 0.5D0
        !
        IF(SOLV_NODE) THEN
            cond = MNWNOD(14,INODE)
        ELSE
            cond = DZ
        END IF
        !
        csum = csum + cond
        if(MNWNOD(15,INODE) == inf )then
          IF(cond > NEARZERO_20)THEN                !seb Added check to set qact=0 when COND=0
              qact = ( hwell - hnew(ic,ir,il) ) * cond
          ELSE
              qact = DZ
          END IF
          !
          MNWNOD(4,INODE) = qact
          !
          IF(SOLV_NODE) THEN
                                        MNWNOD(15,INODE) = hwell
          ELSE
                                        MNWNOD(15,INODE) = hdry
          END IF
          !
          qdes=qdes+qact
c-lfk  10/10/2012
          !iseepchk=1
c--lfk
        endif
c-lfk  10/10/2012
        seepchk=seepchk+mnwnod(4,inode)
c--lfk
      end do           
c-lfk  10/10/2012
      if (abs(seepchk).lt.abs(MNW2(18,IW)).and.qseep.ne.DZ) 
     +         LIMQ(3,IW)=ONE
        !if (abs(seepchk).lt.abs(MNW2(18,IW))) iseepflg=1
!C-LFK --seb SHOULD NOT DEACTIVATE A WELL FOR THE ENTIRE STRESS PERIOD BECAUSE IT GOES DRY
!            INSTEAD PREVENT IT FROM HAVING ANY FLOW
!C   DEACTIVATE WELL IF ALL NODES ARE INACTIVE (Check based on csum=0.0)
!       IF (CSUM.EQ.0.0.AND.Iuupw.NE.0) THEN   !RGN this was bad for smoothing conductance  !seb changed from .LE.
!          MNW2(1,IW)=0 
!            WRITE (IOUT,220) WELLID(IW)
!C-LFK rev. format 11/2012
!c 220 FORMAT (/,'Multi-Node Well ',A20,'DEACTIVATED THIS STRESS PERIOD
!  220 FORMAT (/,      'MNW2 Well ',A20,'DEACTIVATED THIS STRESS PERIOD
!     * BECAUSE ALL NODES WERE DEACTIVATED'/)
!       END IF
C-LFK--comment out single-node processing section below
c      GO TO 105
c  100 CONTINUE
C-LFK   for single-node wells, update values & check constraints
c            INODE=MNW2(4,IW)
c            il=MNWNOD(1,INODE)              
c            ir=MNWNOD(2,INODE)              
c            ic=MNWNOD(3,INODE)              
c            if(IBOUND(ic,ir,il).ne.0) then
c
c              csum  =  MNWNOD(14,INODE)
c              chsum =  MNWNOD(14,INODE)*hnew(ic,ir,il)
c            else
c              MNWNOD(4,INODE) = 0.0D0
c              MNWNOD(14,INODE) = 0.0D0
c              MNWNOD(15,INODE) = hdry
c              csum=0.0
c              chsum=0.0
c            end if
c---div0 ---  CSUM could go to verysmall if the entire well is dry
c        if( csum .gt. verysmall ) then
c          hwell = ( qact - Qseep + chsum ) / csum
c          hwell = ( qact + chsum ) / csum
c        else
c          hwell = hnew(ic,ir,il)
c        endif
c   Test Hlim constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
C-LFK        if(MNW2(6,iw).NE.0) then   !also skip if PUMPCAP overrode Hlim constraint
c        if(MNW2(6,iw).NE.0.and.MNW2(20,IW).GT.-2) then
c          hlim = mnw2(7,iw)
c          ipole = 0
c          if( abs(qdes).gt.verysmall ) ipole = qdes / abs(qdes)
c          hmax = ipole*( hlim )
c          hsim = ipole*( hwell )
c
c          if( hsim .gt. hmax ) then
c            hwell = hlim
c            qact = hwell*csum - chsum 
c      Hlim constraints that stop production are not tested until after the 2nd iteration
c            if( kiter.gt.2 ) then
c              ratio = 1.00D0
c              if( abs(qdes) .gt. small ) ratio =  qact / qdes
c              if( ratio .lt. 0.00001D0 ) then
c                qact  = 0.000D0
c                if (csum .gt. 0.0D0) then
C                  hwell = ( qact - Qseep + chsum ) / csum
c                  hwell = ( qact + chsum ) / csum
c                else
c                  hwell = hnew(ic,ir,il)
c                endif
c              endif  
c            endif   !! potentially stop all production after Kiter = 2 
c          endif     !! End Hlim exceedence loop
c        endif  !! Qlimit>0
c          MNWNOD(4,INODE) = qact
c          MNWNOD(15,INODE) = hwell
c  105 continue
c  Set hwell 
      MNW2(17,iw) = hwell
c                           
      return      
      end
c
c_________________________________________________________________________________
c
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF2MNW27BH(iw,IGRID)
C
c     ******************************************************************
c     compute borehole flow in mnw well 
c     ******************************************************************
c
      USE GWFMNW2MODULE, ONLY:MNWMAX,MNW2,MNWNOD,WELLID,IOUT
      USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
      USE ERROR_INTERFACE,ONLY: STOP_ERROR
      USE CONSTANTS, ONLY: NL, BLN, ONE, UNO, DZ, HALF
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: iw,IGRID
      INTEGER:: firstnode,lastnode,PUMPLOC
      INTEGER:: nodepump, ifound, inode
      INTEGER:: IL, IR, IC, ilp, irp, icp
      double precision Qnet,diff,q
C
      CALL SGWF2MNW2PNT(IGRID)
c   QBH (MNWNOD(27,m) is flow between nodes (saved at upper face) in borehole
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw) > HALF) then
        firstnode = NINT( MNW2(4,iw) )
        lastnode  = NINT( MNW2(4,iw) + ABS(MNW2(2,iw)) - UNO )
        PUMPLOC=MNW2(11,iw)
c
        if(PUMPLOC.eq.0) then
         nodepump=firstnode
        else
          ifound=0
C-RBW
          findpump: do inode=firstnode,lastnode
C Initialize
            MNWNOD(27,INODE)=DZ
c get node coordinates
            il=MNWNOD(1,INODE)              
            ir=MNWNOD(2,INODE)              
            ic=MNWNOD(3,INODE)              
c get pump coordinates
            ilp=MNW2(14,iw)              
            irp=MNW2(15,iw)              
            icp=MNW2(16,iw)              
            if(il.eq.ilp.and.ir.eq.irp.and.ic.eq.icp) then
              ifound=1
              nodepump=inode 
              exit findpump
            end if   
          enddo findpump
          if(ifound.eq.0) then
            CALL STOP_ERROR(OUTPUT=IOUT,MSG=
     + 'MNW2 ERROR: PUMP LOCATION LAYER, ROW, COL SPECIFIED,'//NL//
     +"BUT NO MATCHING WELL NODE'S LAYER, ROW, COL COULD BE FOUND???"//
     +NL//'FOR WELLID: '//WELLID(iw))
          end if
        end if
c get qnet
        Qnet=0.D0
        do INODE=firstnode,lastnode
          q=MNWNOD(4,INODE)
          Qnet=Qnet+q
        end do
          
c   Set flux at top of first node equal to Qnet (if pump at top)
c     or set to zero if pump is somewhere else
c
c   Set flux in between node 1 and node 2 (saved at top of node 2) equal to flux at 
c     that node and Qnet (if pump at top)
c     or just to flow if pump is somewhere else
        if(nodepump.eq.firstnode) then
          MNWNOD(27,firstnode)=-Qnet
        else
          MNWNOD(27,firstnode)=DZ
        end if
c
c   Loop over nodes in well
        do INODE=firstnode+1,lastnode
c   Loop over other nodes in this MNW to set QBH
c   QBH between successive nodes is Q at previous node - Q at node
C         if(nodepump.eq.inode) then
          if(nodepump.eq.inode.and.inode.ne.lastnode) then
            MNWNOD(27,INODE)=MNWNOD(27,INODE-1)+MNWNOD(4,INODE-1)-Qnet
          else          
            MNWNOD(27,INODE)=MNWNOD(27,INODE-1)+MNWNOD(4,INODE-1)
          end if
        enddo
       end if
C
      RETURN
C
      END
C
C
C MNW2HORIZ
C
C Process CWC for nonvertical MNWs
C
C     ******************************************************************
C
      SUBROUTINE MNW2HORIZ(IGRID,LOSSTYPE,NNODES,firstnode,lastnode,
     & IW,kstp,kper,ipr,alpha)
C
C     ******************************************************************
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,DELR,DELC,LAYHDT,
     2                       HNEW
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,IOUT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,WELLID
      USE CONSTANTS,     ONLY: PI, TWOPI, toDeg, Z, ONE, UNO, DZ, 
     +                         NEARZERO_20
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN):: IGRID,LOSSTYPE,NNODES
      INTEGER, INTENT(IN):: firstnode,lastnode,IW,kstp,kper,ipr
      DOUBLE PRECISION, INTENT(IN):: alpha
      !
      INTEGER,          DIMENSION(:), ALLOCATABLE:: ivert1, ivert2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: zseg1,  zseg2
      INTEGER:: Wel1flag, QSUMflag, BYNDflag
      INTEGER:: IMFLG, INODE
      INTEGER:: L1, R1, C1, L2, R2, C2, L, R, C
      INTEGER:: idone, nod, is_intersection
      DOUBLE PRECISION:: 
     + x1face1,x1face2,y1face1,y1face2,z1face1,z1face2,
     + x1face,y1face,z1face,
     + x2face1,x2face2,y2face1,y2face2,z2face1,z2face2,
     + x2face,y2face,z2face,
     + m,lxf,lyf,lzf,lbf,
     + zwt,ywt,xwt,
     + zi,yi,xi,zi2,yi2,xi2,t1,b1,za,ya,xa,zb,yb,xb
      DOUBLE PRECISION:: z1,y1,x1,z2,y2,x2,top1,bot1,top2,bot2,
     & betweennodes,omega_opp,omega,theta_opp,theta_hyp,
     & theta,thck1,thck2,lw,cel2wel2SEG,dx1,dx2,dy1,dy2,
     & cel2wel2,T,Kh,Kz,Txx1,Tyy1
      DOUBLE PRECISION:: 
     & Txx,Tyy,rw,Rskin,Kskin,B,Cf,PLoss,Qact,cond1,cond2,cond,Skin
      ALLOCATE(ivert1(NODTOT),ivert2(NODTOT),zseg1(NODTOT),
     & zseg2(NODTOT))
c convert degree trig func modified from http://techpubs.sgi.com
c     compute borehole length and screen orientation
c
c     compute length associated with each section 
c
c     compute CWC for each node
c
c   Initialize flags
      ivert1=Z
      ivert2=Z
C-LFK
      ZSEG1 = DZ
      ZSEG2 = DZ
      IMFLG = Z
      lxf = DZ
      lyf = DZ
      lzf = DZ
      lbf = DZ
c   Loop over "segments"
        do INODE=firstnode,lastnode-1
         nod=INODE-firstnode+1
c   Initialize flags
         is_intersection = Z
c   Define node and next node
         L1=NINT( MNWNOD(1,INODE)   )           
         R1=NINT( MNWNOD(2,INODE)   )           
         C1=NINT( MNWNOD(3,INODE)   )           
         L2=NINT( MNWNOD(1,INODE+1) )             
         R2=NINT( MNWNOD(2,INODE+1) )             
         C2=NINT( MNWNOD(3,INODE+1) )
         dx1=DELR(C1)             
         dx2=DELR(C2)             
         dy1=DELC(R1)             
         dy2=DELC(R2)             
C     convert to real coodinates
         x1=Z 
         do C=1,C1-1
           x1=x1+DELR(C)
         end do
         x1=x1+0.5D0*DELR(C1)
         x2=0 
         do C=1,C2-1
           x2=x2+DELR(C)
         end do
         x2=x2+0.5D0*DELR(C2)
         y1=0 
         do R=1,R1-1
           y1=y1+DELC(R)
         end do
         y1=y1+0.5D0*DELC(R1)
         y2=0 
         do R=1,R2-1
           y2=y2+DELC(R)
         end do
         y2=y2+0.5D0*DELC(R2)
          if(LAYHDT(L1).EQ.0) then
c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't use hnew=top
           top1=BOTM(C1,R1,LBOTM(L1)-1)
          else
           top1 = hnew(C1,R1,L1)                                       !SCOTT WHAT ABOUT HNEW<BOT OR HNEW=BOT WILL CAUSE CELL2WEL TO BLOW UP
           if(top1.gt.(BOTM(C1,R1,LBOTM(L1)-1)))
     &       top1=BOTM(C1,R1,LBOTM(L1)-1)
          end if
          bot1 = BOTM(C1,R1,LBOTM(L1))
          thck1 = (top1-bot1)/2.d0
          z1 = 0.5D0*(top1+bot1)
c
          if(LAYHDT(L2).EQ.0) then
c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't use hnew=top
           top2=BOTM(C2,R2,LBOTM(L2)-1)
          else
           top2 = hnew(C2,R2,L2)
           if(top2.gt.(BOTM(C2,R2,LBOTM(L2)-1)))
     &      top2=BOTM(C2,R2,LBOTM(L2)-1)
          end if
          bot2 = BOTM(C2,R2,LBOTM(L2))
          thck2 = (top2-bot2)/2.d0
          z2 = 0.5D0*(top2+bot2)
c   save z coords as we don't want z screen elevations to change for WT cases
c
c--LFK Dec 2012
         if(MNWNOD(26,INODE+1).eq.0.0)THEN
           IMFLG=1
         END IF
         if(kstp.eq.1.and.kper.eq.1.OR.(IMFLG.EQ.1.AND.KSTP.EQ.1)) then
C         if(kstp.eq.1.and.kper.eq.1) then
           IMFLG=0
C
           MNWNOD(26,INODE)=z1
           MNWNOD(26,INODE+1)=z2
         else
           z1=MNWNOD(26,INODE)
           z2=MNWNOD(26,INODE+1)
         end if
c     calculate distance between nodes
      betweennodes=SQRT(((x1-x2)**2)+((y1-y2)**2)+((z1-z2)**2))
c
c
c   estimate length of borehole segments
c
c   in first node, use vertical section up to top or WT for segment 1 
c--LFK
c      if(INODE.eq.1) then
      if(INODE.eq.firstnode) then
c
        MNWNOD(23,INODE) = DZ
        if(z1.lt.top1) MNWNOD(23,INODE)=top1-z1   
        ivert1(INODE)=1
      end if
c   if this is a vertical segment, define lengths with elevations, skip other calc
      if(x1.eq.x2.and.y1.eq.y2) then   
        if(top1.le.bot1) MNWNOD(24,INODE) = DZ         
        if(z1.gt.top1) then
          MNWNOD(24,INODE)=top1-bot1  
        else
          MNWNOD(24,INODE)=z1-bot1  
        endif       
        MNWNOD(23,INODE+1)=top2-z2         
c  if blank spaces in between, save that length
        if(bot1.ne.top2) MNWNOD(25,INODE)=bot1-top2
        ivert2(INODE  )=ONE
        ivert1(INODE+1)=ONE
c
c     if not vertical, calculate theta and omega for segment
c
      else        
c
      if(z1.eq.z2) then
C-LFK        omega=0.d0
        omega=90.d0
      else if(z1.gt.z2) then
c-lfk        omega_opp=SQRT(((x1-x2)**2)+((y1-y2)**2))
c-lfk        omega=DASIN((toRad * omega_opp)/betweennodes)
        omega=acos((z1-z2)/betweennodes)*toDeg
      else
        omega=asin((z2-z1)/betweennodes)*toDeg + 90.0D0
c-lfk        omega=dasind(dabs(z2-z1)/betweennodes)+90.0     
c-lfk
        write(iout,*) 'Note: z2>z1 & distal part of well is shallower.'
      end if
      MNWNOD(28,INODE)=omega
c
      theta_opp=dabs(y2-y1)
      theta_hyp=SQRT(((x1-x2)**2)+((y1-y2)**2))
c-lfk      theta=DASIN((toRad * theta_opp)/(toRad * theta_hyp))
      theta=ASIN((theta_opp)/(theta_hyp))*toDeg
c     correct for right quadrant
      if(y2.ge.y1) then
        if(x2.ge.x1) then
          theta=360.D0-theta
        else if (x2.le.x1) then
C-LFK          theta=270.D0-theta
          theta=180.D0+theta
        end if
      else if (y2.le.y1) then
        if (x2.le.x1) then
          theta=180.D0-theta
        end if        
      end if      
      MNWNOD(29,INODE)=theta
c   define first cell's limits to test for first intersection
c   only for nonvertical sections
          x1face1=x1-0.5D0*DELR(C1)
          x1face2=x1+0.5D0*DELR(C1)
          y1face1=y1-0.5D0*DELC(R1)
          y1face2=y1+0.5D0*DELC(R1)
        z1face1=z1-0.5D0*(BOTM(C1,R1,LBOTM(L1)-1)-BOTM(C1,R1,LBOTM(L1)))
        z1face2=z1+0.5D0*(BOTM(C1,R1,LBOTM(L1)-1)-BOTM(C1,R1,LBOTM(L1)))
c   define possible face of intersection in x direction, first cell
          if(x2.gt.x1) then
            x1face=x1face2
          else if(x2.lt.x1) then
            x1face=x1face1
          else
            x1face=0
          end if           
c   define possible face of intersection in y direction, first cell
          if(y2.gt.y1) then
            y1face=y1face2
          else if(y2.lt.y1) then
            y1face=y1face1
          else
            y1face=0
          end if           
c   define possible face of intersection in z direction, first cell
          if(z2.gt.z1) then
            z1face=z1face2
          else if(z2.lt.z1) then
            z1face=z1face1
          else
            z1face=0
          end if           
c   define second cell's limits to test for last intersection
          x2face1=x2-0.5D0*DELR(C2)
          x2face2=x2+0.5D0*DELR(C2)
          y2face1=y2-0.5D0*DELC(R2)
          y2face2=y2+0.5D0*DELC(R2)
        z2face1=z2-0.5D0*(BOTM(C2,R2,LBOTM(L2)-1)-BOTM(C2,R2,LBOTM(L2)))
        z2face2=z2+0.5D0*(BOTM(C2,R2,LBOTM(L2)-1)-BOTM(C2,R2,LBOTM(L2)))
c   define possible face of intersection in x direction, second cell
          if(x2.gt.x1) then
            x2face=x2face1
          else if(x2.lt.x1) then
            x2face=x2face2
          else
            x2face=0
          end if           
c   define possible face of intersection in y direction, second cell
          if(y2.gt.y1) then
            y2face=y2face1
          else if(y2.lt.y1) then
            y2face=y2face2
          else
            y2face=0
          end if           
c   define possible face of intersection in z direction, second cell
          if(z2.gt.z1) then
            z2face=z2face1
          else if(z2.lt.z1) then
            z2face=z2face2
          else        
            z2face=0
          end if           
c
c   if 1st z-coord is greater than the WT, start from intersection with WT
C-LFK          if(z1.gt.HNEW(C1,R1,L1)) then
          if(z1.gt.HNEW(C1,R1,L1).and.layhdt(L1).NE.0) then
            zwt=HNEW(C1,R1,L1)                
c   at wt face, determine intersection with line segment
c-lfk
            if (abs(z2-z1)>NEARZERO_20) then
              m=(zwt-z1)/(z2-z1)
            else
              m=0.0
            end if
            xwt=x1+m*(x2-x1)             
            ywt=y1+m*(y2-y1)
c   redefine 1st point
            x1=xwt
            y1=ywt
            z1=zwt
          end if
c   at x face, determine intersection with line segment
c   xi=intersection point for x face
c   m is "slope" in parameterization of 3d line segment,
c     define m for known x (at the face) and then use that m to solve for
c     other coordinates to give point of intersection
c   xi=x1 + (x2-x1)*m
c   xi-x1/(x2-x1)=m  
c
c-lfk Dec. 2012
          is_intersection=0
          idone=0
c
          if(x1face.ne.0) then
          m=(x1face-x1)/(x2-x1)
          yi=y1+m*(y2-y1)             
          zi=z1+m*(z2-z1)
          if(yi.ge.y1face1.and.yi.le.y1face2.and.
     &       zi.ge.z1face1.and.zi.le.z1face2) then
c       if x1face intersection point lies within cell, this is exit point
             xi=x1face
             lxf=SQRT(((x1-xi)**2)+((y1-yi)**2)+((z1-zi)**2))     
             MNWNOD(24,INODE)=lxf
             is_intersection=1
          end if
c       if exit point is on boundary with second cell, done with both segments
          if(is_intersection.eq.1) then
            if(x2face.eq.xi) then
              lxf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))         
              MNWNOD(23,INODE+1)=lxf
              idone=1
            end if
          end if
          else
            lxf=0.d0
          end if
c
c   at y face, determine intersection with line segment
          if(y1face.ne.0) then
          m=(y1face-y1)/(y2-y1)
          xi=x1+m*(x2-x1)             
          zi=z1+m*(z2-z1)
          if(xi.ge.x1face1.and.xi.le.x1face2.and.
     &       zi.ge.z1face1.and.zi.le.z1face2) then
c       if yface intersection point lies within cell, this is exit point
             yi=y1face
             lyf=SQRT(((x1-xi)**2)+((y1-yi)**2)+((z1-zi)**2))     
             MNWNOD(24,INODE)=lyf
             is_intersection=1
          end if
c       if exit point is on boundary with second cell, done with both segments
          if(is_intersection.eq.1) then
            if(y2face.eq.yi) then
              lyf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))         
              MNWNOD(23,INODE+1)=lyf
              idone=1
            end if
          end if
          else
            lyf=0.d0
          end if
c
c   at z face, determine intersection with line segment
          if(z1face.ne.0) then
          m=(z1face-z1)/(z2-z1)
          xi=x1+m*(x2-x1)             
          yi=y1+m*(y2-y1)
          if(xi.ge.x1face1.and.xi.le.x1face2.and.
     &       yi.ge.y1face1.and.yi.le.y1face2) then
c       if zface intersection point lies within cell, this is exit point
             zi=z1face
             lzf=SQRT(((x1-xi)**2)+((y1-yi)**2)+((z1-zi)**2))     
             MNWNOD(24,INODE)=lzf             
             is_intersection=1
          end if
c       if exit point is on boundary with second cell, done with both segments
          if(is_intersection.eq.1) then
            if(z2face.eq.zi) then
              lzf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))         
              MNWNOD(23,INODE+1)=lzf
              idone=1
            end if
          end if
          else
            lzf=0.d0
          end if
c   if idone still=0, then there are blank spaces in between nodes.  Calculate
c   length of that segment by getting intersection out of last node
          if(idone.eq.0) then
            is_intersection=0
c   at x face, determine intersection with line segment
            if(x2face.ne.0) then
            m=(x2face-x2)/(x2-x1)
            yi2=y2+m*(y2-y1)             
            zi2=z2+m*(z2-z1)
            if(yi2.ge.y2face1.and.yi2.le.y2face2.and.
     &       zi2.ge.z2face1.and.zi2.le.z2face2) then
c       if x2face intersection point lies within cell, this is exit point
             xi2=x2face 
             lxf=SQRT(((x2-xi2)**2)+((y2-yi2)**2)+((z2-zi2)**2))     
             MNWNOD(23,INODE+1)=lxf
             is_intersection=1
            end if
            else
             lxf=0.d0
            end if
c   at y face, determine intersection with line segment
            if(y2face.ne.0) then
            m=(y2face-y2)/(y2-y1)
            xi2=x2+m*(x2-x1)             
            zi2=z2+m*(z2-z1)
            if(xi2.ge.x2face1.and.xi2.le.x2face2.and.
     &       zi2.ge.z2face1.and.zi2.le.z2face2) then
c       if y2face intersection point lies within cell, this is exit point
             yi2=y2face 
             lyf=SQRT(((x2-xi2)**2)+((y2-yi2)**2)+((z2-zi2)**2))     
             MNWNOD(23,INODE+1)=lyf
             is_intersection=1
            end if
            else
             lyf=0.d0
            end if
c   at z face, determine intersection with line segment
            if(z2face.ne.0) then
             m=(z2face-z2)/(z2-z1)
             xi2=x2+m*(x2-x1)             
             yi2=y2+m*(y2-y1)
             if(xi2.ge.x2face1.and.xi2.le.x2face2.and.
     &        yi2.ge.y2face1.and.yi2.le.y2face2) then
c       if z2face intersection point lies within cell, this is exit point
              zi2=z2face 
              lzf=SQRT(((x2-xi2)**2)+((y2-yi2)**2)+((z2-zi2)**2)) 
              MNWNOD(23,INODE+1)=lzf
              is_intersection=1
             end if
            else
             lzf=0.d0
            end if
c  now that we have both node exit intersection points, blank distance is between
c  them.  Save in MNWNOD(25) of the first node between them            
c-LFK Dec 2012: correct calc. of length of blank casing
c           lbf=SQRT(((xi-xi2)**2)+((yi-yi2)**2)+((zi-zi2)**2))     
          if (x1.eq.x2) THEN
            xa=x1
            xb=x2
          ELSE 
            IF (xi.eq.xi2) then
              xa=x1face2
              xb=x2face1
            else
              xa=xi
              xb=xi2
            end if
          END IF  
          if (y1.eq.y2) THEN
            ya=y1
            yb=y2
          ELSE 
            IF (yi.eq.yi2) then
              ya=y1face2
              yb=y2face1
            else
              ya=yi
              yb=yi2
            end if
          END IF  
          if (z1.eq.z2) THEN
            za=z1
            zb=z2
          ELSE 
            IF (zi.eq.zi2) then
              za=z1face2
              zb=z2face1
            else
              za=zi
              zb=zi2
            end if
          END IF  
            lbf=SQRT(((xa-xb)**2)+((ya-yb)**2)+((za-zb)**2))     
c-LFK
            MNWNOD(25,INODE)=lbf 
          end if
C-LFK   Set vertical elev. limits for nonvertical segments
       if (ivert1(inode).eq.0) then
         zseg1(inode)=zseg2(inode-1)
       end if
       if (ivert2(inode).eq.0) then
         zseg2(inode)=zi
         zseg1(inode+1)=zi
       end if
c
c   For last segment, continue the line to the exit intersection of the last cell
c   Define possible face of intersection in x direction, second cell
         if(INODE.eq.(lastnode-1)) then
          if(x2.gt.x1) then
            x2face=x2face2
          else if(x2.lt.x1) then
            x2face=x2face1
          else
            x2face=0
          end if           
c   define possible face of intersection in y direction, second cell
          if(y2.gt.y1) then
            y2face=y2face2
          else if(y2.lt.y1) then
            y2face=y2face1
          else
            y2face=0
          end if           
c   define possible face of intersection in z direction, second cell
          if(z2.gt.z1) then
            z2face=z2face2
          else if(z2.lt.z1) then
            z2face=z2face1
          else        
            z2face=0
          end if  
          if(x2face.ne.0) then
          m=(x2face-x2)/(x2-x1)
          yi=y2+m*(y2-y1)             
          zi=z2+m*(z2-z1)
          if(yi.ge.y2face1.and.yi.le.y2face2.and.
     &       zi.ge.z2face1.and.zi.le.z2face2) then
c       if x2face intersection point lies within cell, this is exit point
             xi=x2face
             lxf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))     
             MNWNOD(24,INODE+1)=lxf
          end if
          else
            lxf=0.d0
          end if
c
c   at y face, determine intersection with line segment
          if(y2face.ne.0) then
          m=(y2face-y2)/(y2-y1)
          xi=x2+m*(x2-x1)             
          zi=z2+m*(z2-z1)
          if(xi.ge.x2face1.and.xi.le.x2face2.and.
     &       zi.ge.z2face1.and.zi.le.z2face2) then
c       if yface intersection point lies within cell, this is exit point
             yi=y2face
             lyf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))     
             MNWNOD(24,INODE+1)=lyf
          end if
          else
            lyf=0.d0
          end if
c
c   at z face, determine intersection with line segment
          if(z2face.ne.0) then
          m=(z2face-z2)/(z2-z1)
          xi=x2+m*(x2-x1)             
          yi=y2+m*(y2-y1)
          if(xi.ge.x2face1.and.xi.le.x2face2.and.
     &       yi.ge.y2face1.and.yi.le.y2face2) then
c       if zface intersection point lies within cell, this is exit point
             zi=z2face
             lzf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))     
C-LFK             MNWNOD(24,INODE+1)=lyf             
             MNWNOD(24,INODE+1)=lzf             
          end if
          else
            lzf=DZ
          end if
C-LFK   Set vertical elev. limit for final nonvertical segment
           if(ivert2(inode+1).eq.0) then
C                write(iout,*) 'ivert(inode+1).eq.0'
              zseg2(inode+1)=zi
           end if
c
        end if
      end if
c     
      lw=MNWNOD(23,INODE)
      if (lw.gt.0.D0) then
          Txx = MNWNOD(16,INODE)
          Tyy = MNWNOD(17,INODE)
          Txx1 = Txx*0.5d0
          Tyy1 = Tyy*0.5d0
          rw  = MNWNOD(5,INODE)
          Rskin = MNWNOD(6,INODE)  
          Kskin = MNWNOD(7,INODE)
          B = MNWNOD(8,INODE)
          Cf = MNWNOD(9,INODE)
          PLoss = MNWNOD(10,INODE)
          Qact = MNWNOD(4,INODE)
          L1=MNWNOD(1,INODE)       !RGN        
          R1=MNWNOD(2,INODE)       !RGN       
          C1=MNWNOD(3,INODE)       !RGN       
c   compute conductance term for segment
          if(ivert1(INODE).eq.0) then
             Kz=MNWNOD(33,INODE)
             cond1 = cel2wel2SEG(lw,theta,omega,LOSSTYPE,
c--LFK
     &         Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
c    &         Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
     &         WELLID(iw),Kz)
c   if a vertical segment, use original function
          else
c-LFK
c-LFK--use 'Skin' as a flag to denote calc. is for a segment
             Skin=HUGE(Skin)                                            !seb changed -999 to HUGE
c
c              cond1 = cel2wel2(LOSSTYPE,Txx1,Tyy1,dx1,dy1,
c     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
              cond1 = cel2wel2(LOSSTYPE,Txx,Tyy,dx1,dy1,
     +                 rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
     +                 WELLID(iw),Skin,iw,C1,R1,L1)               !rgn added iw
         end if
      else
         cond1=DZ 
      end if
      MNWNOD(30,INODE)=cond1
c calculate CWC of second segment of node
      lw=MNWNOD(24,INODE)
      if(lw.gt.0D0) then                      
          Txx = MNWNOD(16,INODE)
          Tyy = MNWNOD(17,INODE)
          Txx1 = Txx*0.5d0
          Tyy1 = Tyy*0.5d0
          rw  = MNWNOD(5,INODE)
          Rskin = MNWNOD(6,INODE)  
          Kskin = MNWNOD(7,INODE)
          B = MNWNOD(8,INODE)
          Cf = MNWNOD(9,INODE)
          PLoss = MNWNOD(10,INODE)
          Qact = MNWNOD(4,INODE)
          L1=MNWNOD(1,INODE)       !RGN        
          R1=MNWNOD(2,INODE)       !RGN       
          C1=MNWNOD(3,INODE)       !RGN      
c   compute conductance term for segment
          if(ivert2(INODE).eq.0) then
             Kz=MNWNOD(33,INODE)
c--LFK
             cond2 = cel2wel2SEG(lw,theta,omega,LOSSTYPE,
     &       Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
C    &         Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
     &         WELLID(iw),Kz)
c   if a vertical segment, use original function
          else
c-LFK
c-LFK--use 'Skin' as a flag to denote calc. is for a segment
             Skin=HUGE(Skin)                                            !seb changed -999 to HUGE
c
cc             cond2 = cel2wel2(LOSSTYPE,Txx1,Tyy1,dx1,dy1,
cc     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
cc     &                 WELLID(iw),Skin,iw,C1,R1,L1,Iuupw) !rgn added iw
             cond2 = cel2wel2(LOSSTYPE,Txx,Tyy,dx1,dy1,
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
     &                 WELLID(iw),Skin,iw,C1,R1,L1) !rgn added iw
          end if
      else
         cond2=0.D0 
      end if
      MNWNOD(31,INODE)=cond2
c sum cond for cell to get resultant CWC for node
      cond=cond1+cond2
c     Save conductance of each node
      MNWNOD(14,INODE) = cond
      if(ipr.eq.1) then 
        t1=top1
        b1=bot1
        if (ivert1(inode).eq.0) t1=zseg1(inode)
        if (ivert2(inode).eq.0) b1=zseg2(inode)
         write(iout,'(A15,I3,1P6G12.5,9A)') WELLID(iw),nod,cond,
     & top1,bot1,t1,b1,alpha,'         '
      end if
c
c process last node separately
c
      if(INODE.EQ.lastnode-1) then
c calculate CWC of first segment in node
      lw=MNWNOD(23,INODE+1)
      if (lw.gt.0.D0) then
          Txx = MNWNOD(16,INODE+1)
          Tyy = MNWNOD(17,INODE+1)
          Txx1 = Txx*0.5d0
          Tyy1 = Tyy*0.5d0
          rw  = MNWNOD(5,INODE+1)
          Rskin = MNWNOD(6,INODE+1)  
          Kskin = MNWNOD(7,INODE+1)
          B = MNWNOD(8,INODE+1)
          Cf = MNWNOD(9,INODE+1)
          PLoss = MNWNOD(10,INODE+1)
          Qact = MNWNOD(4,INODE+1)
          L1=MNWNOD(1,INODE)       !RGN        
          R1=MNWNOD(2,INODE)       !RGN       
          C1=MNWNOD(3,INODE)       !RGN      
c   compute conductance term for segment
          if(ivert1(INODE+1).eq.0) then
c--LFK
             Kz=MNWNOD(33,INODE+1)
c             Kz=MNWNOD(33,INODE)
             cond1 = cel2wel2SEG(lw,theta,omega,LOSSTYPE,
c--LFK
     &         Txx,Tyy,dx2,dy2,rw,Rskin,Kskin,B,Cf,PLoss,thck2*2,Qact,
cc     &         Txx,Tyy,dx2,dy2,rw,Rskin,Kskin,B,Cf,PLoss,thck2,Qact,
     &         WELLID(iw),Kz)
c   if a vertical segment, use original function
          else
c-LFK
c-LFK--use 'Skin' as a flag to denote calc. is for a segment
             Skin=HUGE(Skin)                                            !seb changed -999 to HUGE
c
c             cond1 = cel2wel2(LOSSTYPE,Txx1,Tyy1,dx2,dy2,
              cond1 = cel2wel2(LOSSTYPE,Txx,Tyy,dx2,dy2,
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck2,Qact,
     &                 WELLID(iw),Skin,iw,C1,R1,L1)  !rgn added iw
         end if
      else
         cond1=0.D0 
      end if
      MNWNOD(30,INODE+1)=cond1
c calculate CWC of second segment of node
c it is the same as the other segment in this node
      MNWNOD(24,INODE+1)=MNWNOD(23,INODE+1)
      cond2=cond1
      MNWNOD(31,INODE+1)=cond2
c sum cond for cell to get resultant CWC for node
      cond=cond1+cond2
c     Save conductance of each node
      MNWNOD(14,INODE+1) = cond      
      if(ipr.eq.1) then 
        t1=top2
        b1=bot2
        if (ivert1(inode+1).eq.0) t1=zseg1(inode+1)
        if (ivert2(inode+1).eq.0) b1=zseg2(inode+1)
         write(iout,'(A15,I3,1P6G12.5,9A)') WELLID(iw),nod+1,cond,
     & top2,bot2,t1,b1,alpha,'         '
      end if
      end if
c     end loop over "segments"
      end do
c     print segment info
      if(ipr.eq.1) then
         write(iout,*)
       write(iout,*) 'MNW2 Nonvertical Well:   Segment Information for W
     &ell ',WELLID(IW)
         write(iout,'(A)')  'Node   L   R   C   Segment   Length   
     &   DEG.TILT   MAP-ANGLE    CWC-segment'            
       do INODE=firstnode,lastnode
         L=MNWNOD(1,INODE)              
         R=MNWNOD(2,INODE)              
         C=MNWNOD(3,INODE)              
c segment 1
         lw=MNWNOD(23,INODE)
         if(inode.gt.1) then
           omega=MNWNOD(28,INODE-1)
           theta=MNWNOD(29,INODE-1)
         else
           omega=0.d0
           theta=0.d0
         end if
         cond1=MNWNOD(30,INODE)
           write(iout,'(4I4,I8,ES16.6,1p3G12.5)')
     & INODE,L,R,C,1,lw,omega,theta,cond1
c segment 2
         lw=MNWNOD(24,INODE)
         if(inode.lt.lastnode) then
           omega=MNWNOD(28,INODE)
           theta=MNWNOD(29,INODE)
         else
           omega=MNWNOD(28,INODE-1)
           theta=MNWNOD(29,INODE-1)        
         end if
         cond2=MNWNOD(31,INODE)
           write(iout,'(4I4,I8,ES16.6,1p3G12.5)') 
     & INODE,L,R,C,2,lw,omega,theta,cond2
c closed casings
         if(MNWNOD(25,INODE).GT.0) then
             write(iout,'(A,ES16.6)') '   Closed casing length = ',
     & MNWNOD(25,INODE)
         end if
C
       end do
         write(iout,*)
C-LFK   rewrite header for MNW well conductances if segment info was printed
C        (if there are any more MNW wells)
       if (iw.lt.nmnw2 .AND. MNWPRNT>-1) then
          write(iout,'(120A)') '                              M O D E L
     &  L A Y E R     W E L L  S C R E E N   Penetration    SKIN     
     &  CALCULATED'
         write(iout,'(120A)') 'WELLID        Node    CWC*    top elev   
     &bott.elev     top elev   bott.elev    fraction     COEFF.
     &          B'
       end if
      !
      end if
      !
      DEALLOCATE(ivert1,ivert2,zseg1,zseg2, stat=nod)
      !
      END SUBROUTINE
C
c
c_________________________________________________________________________________
c
      DOUBLE PRECISION function cel2wel2SEG(lw,theta,omega,LOSSTYPE,Txx,
     &  Tyy,dx,dy,rw,Rskin,Kskin,B,Cf,PLoss,thck,Q,WELLNAME,Kz)
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
c     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW2MODULE,  ONLY: IOUT
      USE ERROR_INTERFACE,ONLY: STOP_ERROR,WARNING_MESSAGE
      USE CONSTANTS, ONLY: NL, BLN, TWOPI, NEARZERO_20,
     +                     DZ, UNO, Z, toRad
      IMPLICIT NONE
      INTEGER:: LOSSTYPE,i
      CHARACTER(20):: WELLNAME
      DOUBLE PRECISION:: rw,Txx,Tyy,yx4,xy4,ro,dx,dy,Tpi2,A,
c-lfk     & Ploss,B,Rskin,Kskin,C,Cf,Q,thck,T,Tskin,x1,x2,x3,x4,
     & Ploss,B,Rskin,Kskin,C,Cf,Q,thck,T,Tskin,
     & roz,roy,rox,zx4,xz4,zy4,yz4,Az,Ay,Ax,Tpi2z,Tpi2y,Tpi2x,
     & theta,omega,kz,ky,kx,lw,bz,by,bx,clz,cly,clx,CLi,
     & numerator,denom1,denom2,lwz,lwy,lwx,
     & ct,st,cw,sw,omega0, theta_rad, omega_rad
c convert degree trig func modified from http://techpubs.sgi.com
c      dsind = sin(dgr_to_rad * dgr_argument)
c      dcosd = cos(dgr_to_rad * dgr_argument)
       
C     ------------------------------------------------------------------
c define parameters
c
C
c-lfk
      if (omega.gt.90.0) then
         omega0=omega
         omega=180.0-omega
      end if
      !
      if ( thck>NEARZERO_20 ) then
        Kx=Txx/thck
        Ky=Tyy/thck
      else
        Kx = NEARZERO_20
        Ky = NEARZERO_20
      end if
c    this makes conductance very small
      if( rw < NEARZERO_20 .or. Txx < NEARZERO_20 .or. 
     &                          Tyy < NEARZERO_20 )  then
        cel2wel2SEG = SQRT( Txx * Tyy )** 0.5D0
c       For the "NONE" option, multiply the Kh by 10000 to equivalent Hnew and hwell
      else if(LOSSTYPE == Z) then
        cel2wel2SEG=1.0D4*((Kx*Ky)**0.5D0)   
      else 
c
c    define ro (effective radius) for each direction 
        yx4 = (Ky/Kx)**0.25D0
        xy4 = (Kx/Ky)**0.25D0
        roz = 0.28D0 *((yx4*dx)**2 +(xy4*dy)**2)**0.5D0 / (yx4+xy4)
        Tpi2z = TWOPI * thck *(Kx*Ky)**0.5D0
c
        zx4 = (Kz/Kx)**0.25D0
        xz4 = (Kx/Kz)**0.25D0
        roy = 0.28D0 *((zx4*dx)**2 +(xz4*thck)**2)**0.5D0 / (zx4+xz4)
        Tpi2y = TWOPI * dy * (Kx*Kz)**0.5D0
c
        yz4 = (Kz/Ky)**0.25D0
        zy4 = (Ky/Kz)**0.25D0
        rox = 0.28D0 *((yz4*dy)**2 +(zy4*thck)**2)**0.5D0 / (yz4+zy4)
        Tpi2x = TWOPI * dx * (Kz*Ky)**0.5D0
c
c       if ro/rw is <1, 'A' term will be negative.  Warn user and cut off flow from this node
        if (rox/rw.lt.1.D0.or.roy/rw.lt.1.or.roz/rw.lt.1) then
            CALL WARNING_MESSAGE(OUTPUT=IOUT,
     +      MSG='MNW2 WELLID '//WELLNAME//': HAS ONE OF THE FOLLOWING'//
     +      ' => Ro_x/Rw < 1 OR Ro_y/Rw < 1 OR Ro_z/Rw < 1, '//
     +      'CWC SET TO 0.0', INLINE=.TRUE.)
!            write(IOUT,*) 
!     &      '     Ro_x/Rw =  ',Rox/Rw, 
!     &      '     Ro_y/Rw =  ',Roy/Rw, 
!     &      '     Ro_z/Rw =  ',Roz/Rw, 
!     &      '***WARNING*** At least one value of Ro/Rw < 1,
!     & CWC set = 0.0 for well '
          cel2wel2SEG = DZ
          GOTO 888
        end if
        Az = NEARZERO_20
        Ay = NEARZERO_20
        Ax = NEARZERO_20
        if ( rw > NEARZERO_20 ) then
          if ( abs(Tpi2z)>NEARZERO_20 ) Az = log(roz/rw) / Tpi2z
          if ( abs(Tpi2y)>NEARZERO_20 ) Ay = log(roy/rw) / Tpi2y
          if ( abs(Tpi2x)>NEARZERO_20 ) Ax = log(rox/rw) / Tpi2x
        end if
c
c       THEIM option (LOSSTYPE.EQ.1) only needs A, so no need to calculate  B or C
c
c       SKIN (LINEAR) option, calculate B, C=0
        if(LOSSTYPE.EQ.2) then
c         average T in aquifer assumed to be sqrt of Txx*Tyy
          Bx = DZ
          By = DZ
          Bz = DZ
          if(Kskin.gt.NEARZERO_20.and.rw.gt.NEARZERO_20) then
c         this is from eqs 3 and 5 in orig MNW report
            lwz=thck
            if ( abs(Tpi2z)>NEARZERO_20 ) 
     +      Bz=(thck*(Kx*Ky)**0.5D0/(Kskin*lw)-1)*(DLOG(Rskin/rw))/Tpi2z
            lwy=dy
            if ( abs(Tpi2y)>NEARZERO_20 ) 
     +      By = (dy*(Kx*Kz)**0.5D0/(Kskin*lw)-1)*(DLOG(Rskin/rw))/Tpi2y
            lwx=dx
            if ( abs(Tpi2x)>NEARZERO_20 )
     +      Bx = (dx*(Ky*Kz)**0.5D0/(Kskin*lw)-1)*(DLOG(Rskin/rw))/Tpi2x
          end if
          C = DZ
c       NONLINEAR option, calculate B and C
       else if (LOSSTYPE.EQ.3) then
c--LFK (9/10/15)  Define B terms for GENERAL losstype case.
c      Assumes no directional dependence of "B"; use specified value for Bx, By, & Bz.
          !if ( abs(Tpi2z)>NEARZERO_20 ) B = B / Tpi2z 
          Bx=B
          By=B
          Bz=B
          if(Cf /= DZ) then
            C = Cf * abs(Q)**(PLoss-1)
          else
            C = DZ
          end if
       else
          Bx = DZ
          By = DZ
          Bz = DZ
          C  = DZ
       end if
c       these are per length
       CLz = Az + Bz + C 
       if ( thck>NEARZERO_20 .and. abs(CLz)>NEARZERO_20) 
     +      CLz = UNO / CLz / thck
       CLy = Ay + By + C 
       if ( abs(CLy)>NEARZERO_20) CLy = UNO / CLy / dy
       CLx = Ax + Bx + C 
       if ( abs(CLx)>NEARZERO_20) CLx = UNO / CLx / dx
c calculate CWC for slanted well (from 2.45b in SUTRA doc)
       numerator=(CLz*CLy*CLx)
c      dsind = sin(dgr_to_rad * dgr_argument)
c      dcosd = cos(dgr_to_rad * dgr_argument)
c-lfk       x1=dcos(dgr_to_rad * theta)
c-lfk       x2=dsin(dgr_to_rad * theta)
c-lfk       x3=dcos(dgr_to_rad * omega)
c-lfk       x4=dsin(dgr_to_rad * omega)
       theta_rad = toRad*theta
       omega_rad = toRad*omega
       denom1=CLz*((CLy*(cos(theta_rad)**2))
     &              +CLx*(sin(theta_rad)**2))
     &              *sin(omega_rad)**2
       denom2=CLx*Cly*(cos(omega_rad)**2)
c 
       if(abs(denom1+denom2)<NEARZERO_20) then
            CALL STOP_ERROR(OUTPUT=IOUT,MSG=
     + 'MNW2 ERROR: SLANTED WELL NOT CONFIGURED CORRECTLY THE '//
     + 'FOLLOWING SUMS TO ZERO AND RESULTS IN A DIV-ZERO ERROR.'//BLN//
     +'CLz*((CLy*(cos(dgr_to_rad * theta)**2))+CLx*'//
     +'(sin(dgr_to_rad * theta)**2))*sin(dgr_to_rad * omega)**2  +  '//
     +'CLx*Cly*(cos(dgr_to_rad * omega)**2)'//BLN//    
     + 'FOR WELLID: '//WELLNAME)
       end if
       CLi=numerator/(denom1+denom2)
       cel2wel2SEG=lw*(numerator/(denom1+denom2))
      endif
c
 888  continue
c-lfk
      if (omega0.gt.90.0) omega=180.0-omega
      end
c
C
C
C MNW2CAPACITY
C
C Compute Qact restrained by pumping capacity
C
C     ******************************************************************
C
      SUBROUTINE MNW2CAPACITY(IGRID,qactCap,iw)
C
C     ******************************************************************
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,IOUT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,WELLID
      USE ERROR_INTERFACE,ONLY: STOP_ERROR
      USE CONSTANTS,      ONLY: NL, BLN
      INTEGER:: PUMPCAP
      DOUBLE PRECISION qactCap,LIFTact,Hlift,hwell,m,b,
     & L1,L2,Q1,Q2
      DOUBLE PRECISION CapMult,verysmall
C 
      verysmall = 1.0d-25
C
C     Compute lift
      PUMPCAP=MNW2(22,iw)
      hwell = mnw2(17,iw)
      Hlift=MNW2(23,iw)
      LIFTact=Hlift-hwell
      CapMult=MNW2(24,iw)
C     
      qactCap=0.d0
c     progress flag: idone=1 mean have interp points; idone=2 means have Q value
      idone=0
c     if actual lift is greater than first value in table, use Q for first value
      if(LIFTact.gt.CapTable(iw,1,1)) then
        qactCap=CapTable(iw,1,2)
        idone=2
      end if
c     if actual lift is less than final value in table, use Q for final value
      if(LIFTact.lt.CapTable(iw,PUMPCAP+2,1)) then
        qactCap=CapTable(iw,PUMPCAP+2,2)
        idone=2
      end if
C     Loop over CapTable to check for table entry matches or to find encompassing Lift values
      if(idone.eq.0) then
        do index=1,PUMPCAP+2
c     if actual lift equals one of the table entries, set Q and done
          if(LIFTact.eq.CapTable(iw,index,1)) then
            qactCap=CapTable(iw,index,2)
            idone=2
          end if
c     if LIFTact is an intermediate value, find first entry it is less than; this
c     will define which two value to use in interpolation
          if(idone.eq.0) then
           if(index.lt.(PUMPCAP+2)) then
            if(LIFTact.gt.CapTable(iw,index+1,1)) then
              ifirstL=index
              isecondL=index+1
              idone=1
            end if
           else
c     if table is constructed properly, this should never be executed (index=PUMPCAP+2)
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     + 'MNW2 ERROR:  CAPACITY TABLE READ ERROR'//NL//
     +'FOR WELLID: '//WELLID(iw))
           end if  
          end if
        end do
      end if
c     error check; idone should be set by now
      if(idone.eq.0) then
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     + 'MNW2 ERROR:  CAPACITY TABLE READ ERROR'//NL//
     +'FOR WELLID: '//WELLID(iw))
      end if      
C     Interpolate Q value from table
      if(idone.eq.1) then
c     define points
        L1=CapTable(iw,ifirstL,1)
        L2=CapTable(iw,isecondL,1)
        Q1=CapTable(iw,ifirstL,2)
        Q2=CapTable(iw,isecondL,2)
c     calculate slope and intercept of line between the two points
        m = 0.0d0
        if ( abs(L2-L2)>NEARZERO_20 ) m=(Q2-Q1)/(L2-L1)
        b=Q1-(m*L1)
c     interpolate by finding Q on the line segment for actual lift
        qactCap=m*LIFTact+b
      end if
c     convert discharge to MODFLOW sign convention
      qactCap=qactCap*(-1.d0)
c-LFK
      IF (CapMult.GT.0.0.and.CapMult.LT.1.0) THEN
         qactCap=qactCap*CapMult
      END IF
c      
      RETURN
      END
      SUBROUTINE PPC(DDPP,ISOLNFLAG,BB,HKR,HKZ,SS,QQ,RW,ZPD,ZPL)

C      ********************************************************
C      *                                                      *
C      *                    **** PPC ****                     *
C      *      COMPUTER PROGRAM FOR CALCULATING DRAWDOWN       *
C      *      IN A CONFINED AQUIFER WITH AXIAL-SYMMETRIC      *
C      *           FLOW TO A PARTIALLY PENETRATING,           *
C      *         INFINITESIMAL-DIAMETER PUMPED WELL           *
C      *          VERSION 3.0 CURRENT AS OF 01/25/08          *
C      *From Barlow, P.M., and Moench, A.F., 1999, WTAQ--A    *
C      *     computer program for calculating drawdowns and   *
C      *     estimating hydraulic properties for confined and *
C      *     water-table aquifers: U.S. Geological Survey     *
C      *     Water-Resources Investigations Report 99-4225    *
C      *                                                      *
C      ********************************************************
C
C---SPECIFICATIONS
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION GAMMA(5)
C
C---COMMON STATEMENTS
      COMMON /PAR1/ IPWD,IRUN,IPWS,NOBWC,IOWS,IDPR
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS
      COMMON /PAR6/ BETAW,SIGMA,GAMMA
      COMMON /PAR7/ RERRNR,RERRSUM,TDLAST,TLAST
      COMMON /PAR8/ R,ZP,Z1,Z2,WDP
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
      COMMON /PAR11/ XLD,XDD,WD,SW
C
c      OPEN(UNIT=94,FILE='PPC.out',STATUS='OLD')
C
C
C---THE FOLLOWING PARAMETERS NEED TO BE PASSED FROM MAIN PROGRAM
C      BB=AQUIFER THICKNESS
C      HKR=HORIZONTAL K (L/T)
C      HKZ=VERTICAL K (L/T)
C      SS=SPECIFIC STORAGE (1/L)
C
C      QQ=PUMPING RATE OF WELL (L**3/T)
C      RW=RADIUS OF THE SCREENED INTERVAL OF PUMPED WELL (L)
C      ZPD=DEPTH BELOW TOP OF AQUIFER TO THE TOP OF THE SCREENED 
C          INTERVAL OF THE PUMPED WELL (L)
C      ZPL=DEPTH BELOW TOP OF AQUIFER TO THE BOTTOM OF THE SCREENED
C          INTERVAL OF THE PUMPED WELL (L
C
C      TIMEDD=TIME FOR WHICH DRAWDOWNS WILL BE CALCULATED (T)--MUST BE
C       GREATER THAN 0.0D0
C
c  input for uncoupled code
c      QQ=96000.0
c      RW=0.99
c      ZPD=33.3 
c      ZPL=66.7
C     TIMEDD=100.
c      BB=100.0
c      HKR=140.0
c      HKZ=140.0
c      SS=0.000002
C
C   NOTE, PROGRAM ASSUMES THAT THE PUMPED WELL IS PARTIALLY PENETRATING.
C    THEREFORE, NEED A TEST IN THE MAIN PROGRAM THAT ENSURES THAT THE
C    PUMPED WELL IS PARTIALLY PENETRATING. IF THE PUMPED WELL IS FULLY
C    PENETRATING, DO NOT CALL THIS SUBROUTINE.
C
C1--SET PARAMETERS
C    CONFINED AQUIFER (IAQ=0)
C    DIMENSIONAL ANALYSIS (IFORMAT=1)
C    NO DRAINAGE FROM WT (IDRA=0), NALPHA=0
C    USER-SPECIFIED TIMES (ITS=1) AND NO MEASURED DATA (IMEAS=0)
C    BECAUSE ITS=1, TLAST=0.0D0, NLC=0, AND NOX=0
      IAQ=0
      IFORMAT=1
      IDRA=0
      NALPHA=0
      ITS=1
      TLAST=0.0D0
      NLC=0
      NOX=0
      IMEAS=0
C   PROGRAM SOLUTION VARIABLES
      RERRNR=0.0D0
      RERRSUM=1.D-07
      NMAX=200
      NTMS=0
      NS=8
C
C   PUMPED-WELL INFORMATION
C    WELL IS PARTIALLY PENETRATING (IPWS=0)
C    WELL HAS INFINITESIMAL DIAMETER (IPWD=0); THAT IS, NO WELLBORE 
C      STORAGE
C    WELL-BORE SKIN IS NOT ACCOUNTED FOR HERE (SW=0.0D0)
      IPWS=0
      IPWD=0
      RC=0.0D0
      SW=0.0D0
C    DRAWDOWNS WILL BE CALCULATED FOR A SINGLE TIME (NTSPW=1; IRUN=1)
      NTSPW=1
      IRUN=1
C      
C   OBSERVATION-WELL INFORMATION
C    NO DRAWDOWN CALCULATIONS ARE MADE FOR OBSERVATION WELLS
      NOBWC=0
C
C   CALCULATE AQUIFER PARAMETERS
       AT=HKR*BB
       IF ( abs(HKR).GT.1.0e-14 )THEN
         XKD=HKZ/HKR
       ELSE
         XKD=0.0
       END IF
       ASC=SS*BB
       SIGMA=0.0D0
C12d-CALCULATE PUMPING-WELL DIMENSIONLESS PARAMETERS 
      IF ( abs(BB).GT.1.0e-14 ) THEN
        RWD=RW/BB
      ELSE
        RWD = 0.0
      END IF
      BETAW=XKD*(RWD*RWD)
      IF(IPWD.EQ.0)WD=0.0D0
      IF(IPWS.EQ.0)THEN
       IF ( abs(BB).GT.1.0e-14 ) THEN
         XDD=ZPD/BB
         XLD=ZPL/BB
       ELSE
         XDD=0.0
         XLD=0.0
       END IF
      ENDIF
C
C3--DEFINE SELECTED PROGRAM PARAMETERS
      PI=3.141592653589793D0
      IF(IFORMAT.GE.1)THEN
       IF (abs(HKR).GT.1.0e-14)THEN
         F1=(SS*RW*RW)/HKR
       ELSE
         F1 = 0.0
       END IF
       IF ( abs(HKR*BB).GT.1.0e-14 ) THEN
         F2=QQ/(4.0D0*PI*HKR*BB)
       ELSE
         F2 = 0.0
       END IF
      ELSE
       F1 = 0.0
       F2 = 0.0
      ENDIF
C
C---EXPMAX IS THE MAXIMUM ALLOWABLE ABSOLUTE VALUE OF EXPONENTIAL ARGUMENTS
       EXPMAX=708.D0
C
C4--CALL LINVST TO CALCULATE COEFFICIENTS USED FOR THE STEHFEST
C     ALGORITHM
       CALL LINVST(V,NS)
       XLN2=DLOG(2.D0)
C
C7--SET KK=1 (NECESSARY FOR SUBROUTINE LTST2):
      KK=1
C
C7a-CACULATE DIMENSIONLESS VARIABLES TO PASS TO LAPLACE TRANSFORM
C    SOLUTION SUBROUTINES 
C
       IOWS=2
       RD=1.0D0
       RDSQ=RD*RD
       ZD=0.0D0
       WDP=0.0D0
C
C7b-DEFINE DIMENSIONLESS TIME (TD
       IF(IFORMAT.GE.1)RDSQ=1.0D0
C
C      DO 30 NT=1,NTS
C       HD=0.0D0
C
C---DETERMINE DIMENSIONLESS TIME OF CURRENT TIME STEP.
C      NTT=NTS-NT+1
C      TD=TIMEDD/F1
C
C---CALCULATE DRAWDOWNS
       DDPPOLD=0.0D0
       EPSILON=0.00001D0
       ISOLNFLAG=0
       TD=1.0D4
C       TD=1.0D-2
       DO 30 NT=1,10
        TD=TD*10.0D0
C---CALCULATE DIMENSIONLESS DRAWDOWN FOR PARTIALLY PENETRATING
C     WELL, CALL LTST2
         CALL LTST2(TD,HD)
         IF(HD.LT.1.D-02)HD=0.0D0
C
         RTD=F1*TD
         RHDPP=F2*HD
C
C---CALCULATE DIMENSIONLESS DRAWDOWN FOR FULLY PENETRATING
C     WELL, CALL LTST1
         CALL LTST1(TD,HD)
         IF(HD.LT.1.D-02)HD=0.0D0
C
         RHDFP=F2*HD
C
C---CALCULATE DRAWDOWN DUE TO PARTIALLY PENETRATING WELL
         DDPP=RHDPP-RHDFP
C
C---WRITE RESULTS
C
         IF(DABS(DDPP).LT.0.001D0)THEN
          DDPPOLD=0.0D0
          GO TO 30
         ENDIF
C
         IF ( abs(DDPP).GT.1.0e-14 ) THEN
           DDTEST=(DABS(DDPP-DDPPOLD))/DABS(DDPP)
         ELSE
           DDTEST=0.0
         END IF
         IF(DDTEST.LT.EPSILON)THEN
          ISOLNFLAG=1
          GO TO 10
         ELSE
          DDPPOLD=DDPP
          ISOLNFLAG=-1
         ENDIF
C---END TIME LOOP FOR DRAWDOWN CALCULATIONS
  30   CONTINUE
C
  10  CONTINUE
C
C---FORMAT STATEMENTS
   14 FORMAT(3X,' DIMENSIONLESS TIME = ',D12.1,' TIME= ',D12.5,
     2' PPHD= ',D12.5,' FPHD= ',D12.5,' DDPP= ',D12.5)
   15 FORMAT(3X,' VALUE RETURNED TO MAIN PROGRAM (DDPP) = ',D12.5)
      RETURN
      END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LINVST                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C15-SUBROUTINE LINVST CALCULATES COEFFICIENTS USED FOR THE
C   STEHFEST ALGORITHM
C
       SUBROUTINE LINVST(V,NS)
C---SPECIFICATIONS
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION G(20),V(20),HS(20)
C
       verysmall = 1.0d-25
       G(1)=1.D0
       NH=NS/2
        DO 1 IS=2,NS
1      G(IS)=G(IS-1)*IS
       HS(1) = 0.0d0
       if (abs(G(NH-1))>verysmall) HS(1)=2.D0/G(NH-1)
        DO 3 IS=2,NH
       FI=IS
       IF(IS.EQ.NH) GO TO 2
       if ( abs((G(NH-IS)*G(IS)*G(IS-1)))>verysmall )
     +   HS(IS)=FI**(NH)*G(2*IS)/(G(NH-IS)*G(IS)*G(IS-1))
       GO TO 3
2      if (abs(G(IS)*G(IS-1))>verysmall )
     +   HS(IS)=FI**(NH)*G(2*IS)/(G(IS)*G(IS-1))
3       CONTINUE
       SN=2*(NH-NH/2*2)-1
        DO 4 IS=1,NS
       V(IS)=0.D0
       K1=(IS+1)/2
       K2=IS
       IF(K2.GT.NH)K2=NH
        DO 5 KS=K1,K2
       IF(2*KS-IS.EQ.0) GO TO 6
       IF(IS.EQ.KS)GO TO 7
       if ( abs(G(IS-KS)*G(2*KS-IS))>verysmall ) 
     +      V(IS)=V(IS)+HS(KS)/(G(IS-KS)*G(2*KS-IS))
       GO TO 5
6      if ( abs(G(IS-KS))>verysmall ) V(IS)=V(IS)+HS(KS)/(G(IS-KS))
       GO TO 5
7      if ( abs(G(2*KS-IS))>verysmall) V(IS)=V(IS)+HS(KS)/G(2*KS-IS)
5       CONTINUE
       V(IS)=SN*V(IS)
       SN=-SN
4       CONTINUE
       RETURN
       END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST1                    *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C16-SUBROUTINE LTST1 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FULLY PENETRATING WELL OF INFINITESIMAL
C   DIAMETER IN A CONFINED AQUIFER (THEIS SOLUTION).
C
       SUBROUTINE LTST1(TD,HDT)
C---SPECIFICATIONS
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---COMMON STATEMENTS
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
C
      verysmall = 1.0d-25
       XP=0.D0
      DO 1 I=1,NS
       if ( abs(TD)>verysmall ) PP=XLN2*I/TD
C
       CA=RD*DSQRT(PP)
       IF(CA.GT.EXPMAX) CA=EXPMAX
       RE0=BESSK0(CA)
       if ( abs(PP)>verysmall ) PDL=RE0/PP
1     XP=XP+V(I)*PDL
       if ( abs(TD)>verysmall ) HDT=2.D0*XP*XLN2/TD
C
C---RETURN TO MAIN PROGRAM
       RETURN
       END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST2                    *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C17-SUBROUTINE LTST2 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FINITE DIAMETER, PARTIALLY PENETRATING
C   WELL IN A CONFINED AQUIFER (MODIFIED SOLUTION OF DOUGHERTY AND
C   BABU, 1984). DELAYED DRAWDOWN RESPONSE AT OBSERVATION WELLS
C   IS INCLUDED.
C
       SUBROUTINE LTST2(TD,HD)
C---SPECIFICATIONS
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GAMMA(5)
C---COMMON STATEMENTS
      COMMON /PAR1/ IPWD,IRUN,IPWS,NOBWC,IOWS,IDPR
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS
      COMMON /PAR6/ BETAW,SIGMA,GAMMA
      COMMON /PAR7/ RERRNR,RERRSUM,TDLAST,TLAST
      COMMON /PAR8/ R,ZP,Z1,Z2,WDP
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
      COMMON /PAR11/ XLD,XDD,WD,SW
C
       HD=0.D0
       verysmall = 1.0d-25
       IF(IRUN.EQ.0.AND.KK.EQ.1) RETURN
C
       PI=3.141592653589793D0
C
       IF(IPWS.EQ.1) THEN
        XDD=0.D0
        XLD=1.D0
       ENDIF
C
       XP=0.0D0
C
      DO 1 I=1,NS
       if ( abs(TD)>verysmall ) PP=XLN2*I/TD
       IF ( pp > verysmall ) THEN   !RGN 9/2/2016 changed 1e-14 to verysmall
         Q0=DSQRT(PP)
       ELSE
         Q0=0.0
       END IF
       Q0RD=Q0*RD
       IF(Q0.GT.EXPMAX) Q0=EXPMAX
       IF(Q0RD.GT.EXPMAX) Q0RD=EXPMAX
       RE0=BESSK0(Q0)
       RE1=BESSK1(Q0)
       RE0X=BESSK0(Q0RD)
       IF ( abs(Q0*RE1).GT.verysmall ) THEN
         A0=RE0*(XLD-XDD)/(Q0*RE1)
         E0=RE0X*(XLD-XDD)/(Q0*RE1)
       ELSE
         A0= 0.0
         E0= 0.0
       END IF
       A=0.D0
       E=0.D0
       IF(IPWS.EQ.1) GOTO 30
       IF(IOWS.EQ.1) GOTO 30
       SUMA=0.D0
       SUME=0.D0
C
       NNN=0
C
10     NNN=NNN+1
       IF(NNN.GE.NMAX) GOTO 40
       SUMTA=SUMA
       SUMTE=SUME
       XNPI=NNN*PI
       IF ( BETAW*XNPI*XNPI+PP.GT.1.0e-14 ) THEN
         QN=DSQRT(BETAW*XNPI*XNPI+PP)
       ELSE
         QN=0.0
       END IF
       IF(QN.GT.EXPMAX) QN=EXPMAX
       DB=DSIN(XNPI*(1.0D0-XDD))
       DA=DSIN(XNPI*(1.0D0-XLD))
       IF(IPWS.EQ.1) DA=0.D0
       SINES=DB-DA
       RE0=BESSK0(QN)
       RE1=BESSK1(QN)
       IF ( abs(XNPI*(XLD-XDD)).GT.1.0e-14 ) THEN
         XNUM=RE0*SINES*SINES/(XNPI*(XLD-XDD))
       ELSE
        XNUM=0.0
       END IF
       XDEN=0.5D0*QN*RE1*XNPI
       IF ( abs(XDEN).GT.1.0e-14 ) THEN
         A=XNUM/XDEN
       ELSE
         A = 0.0
       END IF
       SUMA=SUMTA+A
C
       IF(KK.GT.1)THEN
        QNRD=QN*RD
        IF(QNRD.GT.EXPMAX) QNRD=EXPMAX
        RE0X=BESSK0(QNRD)
        IF ( abs((XNPI*(ZD2-ZD1))).GT.1.0e-14) THEN
          IF(IOWS.EQ.0) 
     1   XNUM=RE0X*SINES*(DSIN(XNPI*ZD2)
     2   -DSIN(XNPI*ZD1))/(XNPI*(ZD2-ZD1))
        END IF
        IF(IOWS.EQ.2) 
     1   XNUM=RE0X*SINES*DCOS(XNPI*ZD)
        IF ( abs(XDEN).GT.1.0e-14 ) THEN
        E=XNUM/XDEN
        ELSE
        E=0.0
        END IF
        SUME=SUMTE+E
       ENDIF
C
       IF(IPWS.EQ.0.AND.NNN.LT.25) GOTO 10
       ERRA=DABS(SUMTA-SUMA)
       IF(KK.EQ.1)THEN
        IF(ERRA.LT.RERRSUM*SUMA) GOTO 40
       ENDIF
       IF(KK.GT.1)THEN
        ERRE=DABS(SUMTE-SUME)
        IF(ERRA.LT.RERRSUM*SUMA.AND.ERRE.LT.RERRSUM*SUME) GOTO 40
       ENDIF
C
       GOTO 10
C
40     CONTINUE
C
       A=SUMA
30     DENOM=(1.D0+WD*PP*(A0+A+SW))
       IF ( abs((PP*DENOM)).GT.1.0e-14 ) THEN
        IF(KK.EQ.1) PDL=(A0+A+SW)/(PP*DENOM)
        IF(KK.GT.1) THEN
         E=SUME
         IF(IDPR.EQ.0) PDL=(E0+E)/(PP*DENOM)
         IF(IDPR.EQ.1) THEN
            SLUGF=1.D0/(1.D0+WDP*PP)
            PDL=SLUGF*(E0+E)/(PP*DENOM)
         ENDIF
          END IF
       ELSE
         PDL = 0.0
         E = 0.0
         SLUGF= 0.0
       ENDIF
C
      XP=XP+V(I)*PDL
C
1     CONTINUE
C
       IF ( abs((TD*(XLD-XDD))).GT.1.0e-14 ) THEN
         HD=2.D0*XP*XLN2/(TD*(XLD-XDD))
       ELSE
         HD = 0.0
       END IF
C
C      IF(NNN.GE.NMAX) WRITE(IO,100)
C
C---FORMAT STATEMENTS
C100   FORMAT('PROGRAM CONTINUES TO NEXT TIME STEP BECAUSE NNN',
C    2' EXCEEDS NMAX.')
C
C---RETURN TO MAIN PROGRAM
       RETURN
       END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSK0                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C19-FUNCTION BESSK0 CALCULATES THE ZERO-ORDER MODIFIED BESSEL FUNCTION
C    OF THE SECOND KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSK0(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,BESSI0,Q
      DATA P1,P2,P3,P4,P5,P6,P7/-0.57721566D0,0.42278420D0,0.23069756D0,
     *    0.3488590D-1,0.262698D-2,0.10750D-3,0.74D-5/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,-0.7832358D-1,0.2189568D-1,
     *    -0.1062446D-1,0.587872D-2,-0.251540D-2,0.53208D-3/
C
      if ( X < 1.0E-14 ) THEN     !seb Added additional IF to account for near zero values that cause LOG(0) error
        Q = 1.0E-14
        Y=Q*Q/4.D0
        BESSK0=(-DLOG(Q/2.D0)*BESSI0(Q))+(P1+Y*(P2+Y*(P3+
     *        Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSEIF (X.LE.2.D0) THEN
        Y=X*X/4.D0
        BESSK0=(-DLOG(X/2.D0)*BESSI0(X))+(P1+Y*(P2+Y*(P3+
     *        Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        Y=(2.D0/X)
        BESSK0=(DEXP(-X)/DSQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *        Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSI0                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C20-FUNCTION BESSI0 CALCULATES THE ZERO-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE FIRST KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSI0(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,AX,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,3.5156229D0,3.0899424D0,1.2067492D
     *0,
     *    0.2659732D0,0.360768D-1,0.45813D-2/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,0.1328592D-1,
     *    0.225319D-2,-0.157565D-2,0.916281D-2,-0.2057706D-1,
     *    0.2635537D-1,-0.1647633D-1,0.392377D-2/
C
      IF (DABS(X).LT.3.75D0) THEN
        Y=(X/3.75D0)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=DABS(X)
        Y=3.75D0/AX
        BESSI0=(DEXP(AX)/DSQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4
     *      +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSK1                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C21-FUNCTION BESSK1 CALCULATES THE FIRST-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE SECOND KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSK1(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,BESSI1,Q
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,
     *    -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,
     *    0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/
C
      IF(X < 1.0E-14)THEN                                              !seb Added additional IF to account for near zero values that cause LOG(0) error
        Q = 1.0E-14  !Replaces X in normal equation
        Y=Q*Q/4.0
        BESSK1=(LOG(Q/2.0)*BESSI1(Q))+(1.0/Q)*(P1+Y*(P2+
     *      Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSEIF (X.LE.2.0) THEN
        Y=X*X/4.0
        BESSK1=(LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+
     *      Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        Y=2.0/X
        BESSK1=(EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *      Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSI1                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C22-FUNCTION BESSI1 CALCULATES THE FIRST-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE FIRST KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSI1(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,AX,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,
     *    0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,
     *    -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,
     *    -0.2895312D-1,0.1787654D-1,-0.420059D-2/
C
      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI1=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI1=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+
     *      Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C