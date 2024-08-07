!!!RDR   KNTR
!!!    FMP3RQ
!!!    TFDROLD UNRD  RNRD NRD
!!!    ISTRM(1,1),IDIVAR
!!!    ! Note that SWFL%MXNRD = -1 needs to be fixed   
!
! ###########################################################################################################################
!
MODULE FMP_MAIN_DRIVER
  USE NUM2STR_INTERFACE,      ONLY: NUM2STR
  USE GENERIC_OPEN_INTERFACE, ONLY: UTF8_BOM_OFFSET_REWIND
  USE FILE_IO_INTERFACE,      ONLY: READ_TO_DATA, MAX_LINE_LENGTH
  USE STRINGS,                ONLY: GET_WORD
  USE UTIL_INTERFACE,         ONLY: SET_NEAR_ZERO, ZERO_OR_GREATER
  USE ERROR_INTERFACE,        ONLY: STOP_ERROR, WARNING_MESSAGE, FILE_IO_ERROR
  !
  IMPLICIT NONE   
  !
  PRIVATE   
  !
  PUBLIC:: FMP_AR, FMP_RP, FMP_SUBLINK, FMP_AD, FMP_FM, FMP_CNVG, FMP_BD
  !
  CONTAINS
  !
  SUBROUTINE FMP_AR(IN_FMP,IUNITSFR,IUNITMNW1,IUNITMNW2,IUNITUZF,IUNITNWT,IUNITDRT,IGRID,ILGR,MXITER)
    !
    ! READ COMMENT RECORDS, PARAMETER DIMENSIONS, AND FLAGS;
    ! PRINT REPORT TO LIST FILE;
    ! ALLOCATE ARRAY STORAGE FOR FARM PROCESS.
    !
    USE CONSTANTS
    USE OPENSPEC
    USE FMP_GLOBAL
    USE GLOBAL, ONLY:IOUT, SPSTART, ITMUNI
    USE GLOBAL, ONLY:NCOL,NROW
    USE GWFMNW2MODULE, ONLY: SGWF2MNW2PNT
    USE GWFSFRMODULE, ONLY:NSTRM,ISTRM, NSS, SWO_ENABLED, SEG_NSTRM
    USE LGRMODULE,    ONLY:ISFRGRID,LGRDAT
    USE GENERIC_BLOCK_READER_INSTRUCTION,   ONLY: GENERIC_BLOCK_READER
    USE       WBS_DATA_FMP_MODULE,       ONLY: INITIALIZE_WBS_DATA
    USE      WELL_DATA_FMP_MODULE,       ONLY: INITIALIZE_FMP_WELL_DATA,FWEL_BASIC_ALLOCATE
    USE      CROP_DATA_FMP_MODULE,       ONLY: INITIALIZE_CROP_DATA
    USE   CLIMATE_DATA_FMP_MODULE,       ONLY: INITIALIZE_CLIMATE_DATA
    USE ALLOTMENT_DATA_FMP_MODULE,       ONLY: INITIALIZE_ALLOTMENT_DATA
    USE      SOIL_DATA_FMP_MODULE,       ONLY: INITIALIZE_SOIL_DATA
    USE   OPTIONS_DATA_FMP_MODULE,       ONLY: INITIALIZE_OPTIONS_DATA
    USE    OUTPUT_DATA_FMP_MODULE,       ONLY: INITIALIZE_OUTPUT_DATA
    USE   SALINITY_DATA_FMP_MODULE,      ONLY: INITIALIZE_SALINITY_DATA
    USE   SURFACE_WATER_DATA_FMP_MODULE, ONLY:INITIALIZE_SURFACE_WATER_DATA
    USE             SWO_DATA_FMP_MODULE, ONLY: INITIALIZE_SWO_DATA
    !
    INTEGER, INTENT(IN):: IN_FMP,IUNITSFR,IUNITMNW1,IUNITMNW2,IUNITUZF,IUNITNWT,IUNITDRT,IGRID,ILGR,MXITER
    !
    TYPE(GENERIC_BLOCK_READER):: BLK
    !
    INTEGER:: DIM, NSEG
    !
    CHARACTER(250):: LINE  !Only to hold "BEGIN NAME"
    CHARACTER(10):: WORD   !Used to get "BEGIN"
    !
    INTEGER:: LLOC, N, NF, NS, NC, NFARM
    !
    LOGICAL:: EOF, FOUND_BEGIN, NO_GLOBAL_FOUND
    !
    ! LGR Pointer Update
    !
    CALL FMP_LGR_PNT(IGRID)
    CALL SGWF2NWT1PNT(Igrid)   !seb lgr
    CALL SGWF2UPW1PNT(Igrid)   !seb lgr
    CALL SGWF2UZF1PNT(Igrid)   !seb lgr
    CALL SGWF2SFR7PNT(Igrid)   !seb lgr
    CALL SGWF2MNW2PNT(IGRID)   !seb lgr
    CALL SGWF2MNW1PNT(Igrid)   !seb lgr
    CALL SGWF2BAS7PNT(IGRID)   !Ensure that BAS Pointers are correct before using FDIM
    !
    ! ALLOCATE SPACE FOR CONSTANTS:
    !
    ALLOCATE(ISTARTFL)
    ISTARTFL=0 
    !
    ALLOCATE(FDIM, WBS)
    !
    ALLOCATE(FCROP,SWFL,CLIMATE,ALLOT,SOIL,FMPOPT,FMPOUT,SALT)
    ALLOCATE(SWODAT)
    !
    WBS%HAS_SFR  = IUNITSFR  .NE. Z
    WBS%HAS_UZF  = IUNITUZF  .NE. Z
    WBS%HAS_MNW1 = IUNITMNW1 .NE. Z
    WBS%HAS_MNW2 = IUNITMNW2 .NE. Z
    WBS%HAS_DRT  = IUNITDRT  .NE. Z
    WBS%HAS_NWT  = IUNITNWT  .NE. Z
    WBS%HAS_UPW  = IUNITNWT  .NE. Z
    !
    !1===== IDENTIFY PACKAGE AND INITIALIZE NFWELS ==============================================================
    WRITE(IOUT,'(//1x,A, I5/)')'FMP4 -- FARM PROCESS, VERSION 4.0 - OWHM, INPUT READ FROM UNIT ', IN_FMP
    !
    ! ====== SETTINGS FOR LOCAL GRID REFINEMENT IF SFR PACKAGE OF CURRENT GRID IS INACTIVE
    IF(ILGR.NE.0) THEN ! - Not sure why Wolfgang does this cause later he sets it to false
      IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) THEN
           WBS%HAS_SFR = TRUE
           ALLOCATE(NSTRM, NSS)
           NSTRM = Z
           NSS   = Z
           CALL SGWF2SFR7PSV(IGRID)
      ENDIF
    ENDIF
    !
    IF(WBS%HAS_SFR) THEN
        NSEG = NSS
    ELSE
        NSEG = Z
    END IF
    !
    !1===== BEGIN LOADING OF BLOCKS ===================
    !
    !FIRST DETERMINE MAX LINE LENTH
    N = MAX_LINE_LENGTH(IN_FMP, TRUE)
    IF(N < 200) N = 200
    !
    CALL BLK%SET_LINE_LEN(N) !SET BLOCK READER TO LOAD MAX LENTH
    !
    !  Search for Global Dimension Block
    !
    NO_GLOBAL_FOUND = TRUE 
    CALL READ_TO_DATA(LINE,IN_FMP,IOUT,EOF=EOF)
    DO WHILE (.NOT. EOF)
       LLOC=ONE
       CALL GET_WORD(WORD,LINE,LLOC)
       IF(WORD == 'BEGIN') THEN
                   CALL GET_WORD(WORD,LINE,LLOC)
                   IF (WORD == 'GLOBAL' .OR. WORD == 'DIMENSION') THEN
                         !
                         CALL BLK%LOAD(IN_FMP,IOUT,LINE)
                         CALL FDIM%INIT(BLK, NROW, NCOL, IUNITUZF, WBS%GSE)
                         NO_GLOBAL_FOUND = FALSE
                         EXIT
                   END IF
       END IF
       CALL READ_TO_DATA(LINE,IN_FMP,IOUT,EOF=EOF)
    END DO
    !
    CALL UTF8_BOM_OFFSET_REWIND(IN_FMP)
    !
    CALL SGWF2BAS7PSV(IGRID)  !Ensure that BAS/DIS is backed up due to changes in FDIM
    CALL SGWF2BAS7PNT(IGRID)  !Ensure that BAS Pointers are correct before using FDIM
    !
    IF(NO_GLOBAL_FOUND) CALL STOP_ERROR(BLNK,IN_FMP,IOUT,MSG=                                     &
            'FAILED TO LOCATE "BEGIN GLOBAL DIMENSION" WITHIN FMP INPUT FILE'//NL//               &
            'FMP AT REQUIRES AT THE MINUMUM SPECIFYING THE "GLOBAL DIMENSION" BLOCK INPUT.'//NL// &
            '-- PLEASE CHECK INPUT --')
    
    IF(FDIM%NFARM < Z) CALL STOP_ERROR(BLNK,IN_FMP,IOUT,MSG=                                        &
            'FOUND GLOBAL DIMENSION BLOCK,'//NL//                                                   &
            'BUT IT DID NOT SPECIFY THE NUMBER OF WATER BALANCE SUBREGIONS (NWBS).'//NL//                       &
            'IT IS REQUIRED AT A MINIMUM TO SPECIFY NWBS FOLLOWED BY AN INTEGER (eg "NWBS 1" or "NWBS 4").')
    !
    NFARM = FDIM%NFARM
    IF(NFARM < ONE) NFARM = ONE  ! Note FDIM%NFARM will be updated at end of subroutine
    !
    ! IF LOADED BUILD ISTRM INDEX
    IF( NSEG > 0 ) THEN
                   CALL FDIM%SFR_ID%BUILD_ISTRM(NSEG, SEG_NSTRM)  !Build ISTRM pointers for SFR_NAMES
    ELSE
                   CALL FDIM%SFR_ID%DESTROY()
    END IF
    !
    ! SET UP ANY SFR DELIVERY POINTS THAT NEED TO BE KEPT TRACK OF
    !
    DIM = FDIM%NSFR_DELIV ! +  ...
    !
    ALLOCATE(SFR_DELIV)
    IF(DIM > Z) THEN
        CALL SFR_DELIV%ALLOC(DIM)
    ELSE
        CALL SFR_DELIV%ALLOC(ONE)
    END IF
    !
    ALLOCATE(FWELL(NFARM))
    !
    WBS%NFARM = NEG
    !
    ! CROP BLOCK IS REQUIRED BEFORE SALINITY BLOCK IN CASE THERE ARE CROP FRACTIONS.
    ! CHECK FOR CROP BLOCK FIRST. THE REST DO NOT CARE ABOUT THE ORDER
    !
    CALL READ_TO_DATA(LINE,IN_FMP,IOUT,EOF=EOF)
    DO WHILE (.NOT. EOF)
       LLOC=ONE
       CALL GET_WORD(WORD,LINE,LLOC)
       IF(WORD == 'BEGIN') THEN
           CALL GET_WORD(WORD,LINE,LLOC)
           IF (WORD=='LAND_USE'.OR.WORD=='LAND'.OR.WORD=='CROP') THEN
              CALL BLK%LOAD(IN_FMP,IOUT,LINE)
              IF(BLK%NLINE>0) THEN
                IF (FDIM%HAS_FMP) THEN
                    !
                    WBS%HAS_CROP =  TRUE
                    CALL INITIALIZE_CROP_DATA(BLK,FCROP,LINE,FDIM,WBS%HAS_SFR)
                ELSE
                    CONTINUE
                END IF
              END IF
              EXIT
           END IF
       END IF
       CALL READ_TO_DATA(LINE,IN_FMP,IOUT,EOF=EOF)
    END DO
    !
    CALL UTF8_BOM_OFFSET_REWIND(IN_FMP)
    !
    BLK%EXTRA = 'GO'   !This ensures that loop starts
    !
    DO WHILE (BLK%EXTRA.NE.'EOF') !OLD CHECK BLK%NAME  == '', BUT NOW READ TO END OF FILE
      !
      CALL BLK%LOAD(IN_FMP,IOUT,FOUND_BEGIN=FOUND_BEGIN, SKIP=['GLOBAL   ','DIMENSION', 'CROP     ','LAND_USE '])
      !
      IF((BLK%NAME == 'WATER'                   .OR. BLK%NAME == 'WATER_BALANCE' .OR.   &  !Note WATER should later add checks for WATER BALANCE SUBREGION
          BLK%NAME == 'WATER_BALANCE_SUBREGION' .OR. BLK%NAME=='WBS')                   &
                                               .AND. BLK%NLINE>0) THEN
             !
             CALL INITIALIZE_WBS_DATA(BLK,WBS,LINE,FDIM)
             !
      ELSEIF((BLK%NAME == 'SUPPLY' .OR. BLK%NAME == 'SUPPLY_WELL' .OR. BLK%NAME == 'FMP_WELL') .AND. BLK%NLINE>0) THEN
             !
             WBS%HAS_WELL =  TRUE
             CALL INITIALIZE_FMP_WELL_DATA(BLK,FWELL,LINE)
             !
      ELSEIF((BLK%NAME=='SURFACE' .OR. BLK%NAME=='SURFACE_WATER' .OR. BLK%NAME=='SURFACEWATER') .AND. BLK%NLINE>0) THEN
             !
             CALL INITIALIZE_SURFACE_WATER_DATA(BLK, SWFL, LINE, FDIM, NSEG)
             !
      ELSEIF((BLK%NAME == 'SALINITY' .OR.                       &
              BLK%NAME == 'SALINITY_FLUSH_IRRIGATION' )         &
                                        .AND.BLK%NLINE>0) THEN
             !
             WBS%HAS_SALT = TRUE
             CALL INITIALIZE_SALINITY_DATA( BLK, SALT, LINE, FDIM, FCROP%MULTI_CROP_CELLS )
             !
      ELSEIF(BLK%NAME == 'SOIL' .AND. BLK%NLINE>0) THEN
             !
             WBS%HAS_SOIL =  TRUE
             CALL INITIALIZE_SOIL_DATA(BLK,SOIL,LINE,FDIM)
             !
      ELSEIF(BLK%NAME == 'CLIMATE' .AND. BLK%NLINE>0) THEN
             !
             WBS%HAS_CLIM =  TRUE
             CALL INITIALIZE_CLIMATE_DATA(BLK,CLIMATE,LINE,FDIM,WBS%HAS_CROP)
             !
      ELSEIF( (BLK%NAME == 'ALLOTMENT' .OR. BLK%NAME == 'ALLOTMENTS') .AND. BLK%NLINE>0) THEN
             WBS%HAS_ALLOT = TRUE
             CALL INITIALIZE_ALLOTMENT_DATA(BLK,ALLOT,LINE,FDIM)
             !
      ELSEIF(BLK%NAME == 'OUTPUT' .AND. BLK%NLINE>0) THEN
             !
             CALL INITIALIZE_OUTPUT_DATA( BLK, FMPOUT, ITMUNI )
             !
      ELSEIF((BLK%NAME=='OPTION' .OR. BLK%NAME=='OPTIONS').AND. BLK%NLINE>0) THEN
             !
             CALL INITIALIZE_OPTIONS_DATA( BLK, FMPOPT )
             !
      ELSEIF(       (BLK%NAME=='SWO' .OR.                             &
                     BLK%NAME=='SURFACE_WATER_OPERATIONS' .OR.        &
                     BLK%NAME=='SURFACEWATER_OPERATIONS') .AND.       &
                                                     BLK%NLINE>0) THEN
             !
             CALL INITIALIZE_SWO_DATA(BLK,SWODAT,LINE,FDIM,NSEG,NSTRM,SEG_NSTRM,SPSTART,MXITER)
             !
             IF(SWODAT%HAS_SWO .AND. WBS%HAS_SFR) THEN
                 CALL SWODAT%ALLOC_N_INIT(ISTRM)
                 SWO_ENABLED = TRUE  !DISABLES SFR BD POST FM ROUTINE WHICH THROWS MASS BALANCE
             END IF
             !
      ELSEIF(ANY( BLK%NAME==['GLOBAL   ','DIMENSION', 'CROP     ','LAND_USE ']))THEN
                 CONTINUE
      ELSEIF(BLK%NLINE>0) THEN  !TRUE IF "BEGIN" FOUND
             CALL WARNING_MESSAGE(OUTPUT=BLK%IOUT,MSG=               &
             'FMP BLOCK NAME IS NOT RECOGNIZED. THE FOLLOWING BLOCK "' &
             //BLK%NAME//'" WILL BE SKIPPED AND NOT INCLUDED',         &
             INLINE=TRUE, CMD_PRINT=TRUE)
             !
      ELSEIF(.NOT. FOUND_BEGIN .AND. BLK%EXTRA .NE. 'EOF') THEN
             CALL READ_TO_DATA(LINE,IN_FMP,BLK%IOUT,EOF=EOF)           !BLOCK READER DOES A BACKSPACE WHEN FAILS TO LOCATE "BEGIN"
             CALL WARNING_MESSAGE(OUTPUT=BLK%IOUT,MSG=                &
             'FAILED TO LOCATE ON NEXT UNCOMMENTED LINE THE FMP '//     &
             'BLOCK KEYWORD "BEGIN".'//BLN//'THE FOLLOWING LINE IS '//  &
             'WHAT WAS READ:'//NL//'"'//TRIM(LINE)//'"'//NL//           &
             '***THIS LINE WILL BE IGNORED***', INLINE=TRUE)
      END IF
      !
    END DO  
    CALL BLK%DESTROY()
    !                                                                                                                       
    IF(WBS%NFARM < Z .AND. FDIM%NFARM > Z) CALL STOP_ERROR(BLNK,IN_FMP,IOUT,MSG=                                                           &
                      'THE "GLOBAL DIMENSION" BLOCK SPECIFIED NWBS > 0, BUT FAILED TO LOCATE THE "WATER_BALANCE_SUBREGION" BLOCK.'//NL//   &
                      'IF NWBS > 0, THEN YOU MUST SPECIFY THE WATER_BALANCE_SUBREGION BLOCK AND SPECIFY IN IT THE "LOCATION" KEYWORD.'//NL//                                                        &
                      'OTHERWISE FMP DOES NOT KNOW HOW THE GEOGRAPHIC LOCATION OF EACH WBS.' )
    
    IF(WBS%NFARM < Z) THEN
        CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG='FMP FAILED TO LOCATE THE "WATER_BALANCE_SUBREGION" BLOCK AND NWBS < 1.'//NL//              &
                                             'FMP WILL SET NWBS = 1 AND SET THE ENTIRE MODEL GRID AS BEING ASSOCIATED WITH WBS1.'//NL//  &
                                             'That is, the following is assumed:'//BLN//  &
                                             'BEGIN GLOBAL DIMENSION'//NL//               &
                                             '           NWBS 1"'//NL//                   &
                                             'END  GLOBAL DIMENSION'//BLN//               &
                                             'BEGIN WATER_BALANCE_SUBREGION'//NL//        &
                                             '           LOCATION STATIC LIST CONSTANT 1"'//NL//  &
                                             'END WATER_BALANCE_SUBREGION')
        CALL WBS%SETUP_NO_WBS_DATA(FDIM, IN_FMP, IOUT)
    END IF
    !                  !
    IF(.NOT. WBS%HAS_SOIL) THEN
         CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG=BLN//                                                                                                  &
                       'FMP INPUT FAILED TO LOCATE THE SOIL BLOCK (BEGIN SOIL).'//BLN//                                                                &
                       'IT WILL AUTOMATICALLY SET THE SOIL ID TO ONE AND SET THE CAPILARY FRINGE DEPTH TO ZERO (0.0).'//BLN//                          &
                       'THIS WILL PREVENT ANY EVAPORATION OF GROUNDWATER.'//BLN//                                                                      &
                       'IF YOU ARE DEFINING CROPS (NCROP>0), THEN THERE IS A SMALL CHANCE (VERY SMALL) OF GETTING A DIV/0 FLOATING POINT ERROR.'//BLN, &
                       CMD_PRINT=TRUE)
     CALL SOIL%NO_SOIL(FDIM, TRUE)
    END IF
    !
    IF(.NOT. WBS%HAS_WELL) THEN
        CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG=                                                         &
                       'FMP INPUT FAILED TO LOCATE THE "SUPPLY_WELL" BLOCK (BEGIN SUPPLY_WELL).'//BLN// &
                        'NO FARM WELLS WILL BE EMPLOYED DURING SIMULATION'//BLN//                       &
                       '(i.e. NO AVAILIBLE PUMPAGE TO MEET DEMAND -- NO WELLS TO PUMP).')
        CALL FWEL_BASIC_ALLOCATE(FWELL, IOUT, NFARM)  ! SETS UP REQUIRED INITS WHEN BLOCK WAS NEVER CALLED
    ELSE
        WBS%HAS_WELL = ANY(ABS(FWELL%DIM) > Z) !THERE IS A CHANCE THERE ARE NO FARM WELLS LOADED  --NOTE TYPE 2 (FID FEED) SETS DIM TO ZERO INITIALLY TO REPRESENT THE TOTAL WELL COUNT
        !
        IF(WBS%HAS_WELL)  THEN
            CALL FWELL%MNW2_INDEX(WBS%HAS_MNW2)  !BUILD INDEX BETWEEN FMP AND MNW2
        ELSE
           CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG=                                                          &
                       'FMP INPUT FOUND THE FWELL BLOCK (BEGIN FWELL).'//BLN//                              &
                       'BUT NO FARM WELLS WERE SUCCESSFULLY LOADED.'//BLN//                                 &
                       'NO FARM WELLS WILL BE EMPLOYED DURING SIMULATION.'//BLN//                           &
                       'PLEASE CHECK FARM WELL INPUT IN THAT YOU MEANT NOT TO DEFINE ANY FARM WELLS'//BLN// &
                       '(NO AVAILIBLE PUMPAGE TO MEET DEMAND -- NO WELLS TO PUMP).',                        &
                       CMD_PRINT=TRUE)
                       !
           CALL FWEL_BASIC_ALLOCATE(FWELL, IOUT, NFARM)  ! SETS UP REQUIRED INITS WHEN BLOCK WAS NEVER CALLED
        END IF
    END IF
    !
    IF(SWODAT%HAS_SWO .AND. .NOT. WBS%HAS_SFR) THEN
        CALL STOP_ERROR(INFILE=IN_FMP, OUTPUT=IOUT,                                                                        &
                       MSG='FMP SURFACE_WATER_OPERATIONS BLOCK ERROR: '//NL//                                                    &
                       'FMP SURFACE WATER OPERATIONS REQUIRES THAT SFR PACKAGE BE IN USE AND DECLAIRED IN THE NAME FILE.'//NL//  &
                       'PLEASE SPECIFY THE SFR PACKAGE OR REMOVE THE SURFACE_WATER_OPERATIONS BLOCK FROM THE FMP INPUT'//NL//    &
                       '(OR JUST MAKE SURE THE BLOCK IS EMPTY).')
    END IF
    !
    IF(SWFL%REQ_SFR .AND. .NOT. WBS%HAS_SFR)  CALL STOP_ERROR(INFILE=IN_FMP, OUTPUT=IOUT,MSG= &
                       'FMP SURFACE_WATER BLOCK ERROR. FMP FEATURES THAT REQUIRE SFR '//            &
                       'WERE DECLAIRED (e.g. Semi-Routed Deliveries), BUT SFR IS NOT '//            &
                       'A PACKAGE DECLAIRED IN THE NAME FOR THIS SIMULATION.'//NL//                 &
                       'PLEASE CHECK INPUT AND REMOVE ANY CONNECTIONS TO SFR TO '//                 &
                       'CONTINUE')
    !
    IF(ALLOT%HAS_SW_ALLOTMENT .AND. .NOT. WBS%HAS_SFR)  &  !fix this
                                CALL STOP_ERROR(INFILE=IN_FMP, OUTPUT=IOUT,MSG=                       &
                               'FMP ALLOTMENT BLOCK ERROR. FMP SPECIFIED A SURFACE WATER ALLOTMENT,'//NL//  &
                               'BUT SFR IS NOT A PACKAGE DECLAIRED IN THE NAME FOR THIS SIMULATION.'//NL//  &
                               'PLEASE CHECK INPUT AND EITHER ENABLE SFR OR COMMENT OUT THE "SURFACE_WATER" ALLOTMENT TO CONTINUE.')
    !
    IF(.NOT. SWFL%HAS_SW) CALL SWFL%NO_SURFACE_WATER_DATA(FDIM, IOUT)
    !
    IF(.NOT. WBS%HAS_CLIM) CALL CLIMATE%SETUP_NO_CLIMATE_DATA(FDIM, IOUT)
    !
    CALL FCROP%SETUP_DEPENDENT_PARTS(FDIM, WBS, CLIMATE, SOIL, IOUT)
    !
    ! CHECK IF GLOBAL SHUTDOWN OF CBC IS IN EFFECT
    CALL CHECK_CBC_GLOBAL_UNIT( FMPOUT%WEL_CBC )
    CALL CHECK_CBC_GLOBAL_UNIT( FMPOUT%FNR_CBC )
    !
    !IF( NOCBC>0 .AND. FMPOUT%WEL_CBC > 1) FMPOUT%WEL_CBC = 0
    !IF( NOCBC>0 .AND. FMPOUT%FNR_CBC > 3) FMPOUT%FNR_CBC = 0
    !
    IF(FMPOUT%FWELLS%IS_OPEN) THEN
        !
        BLOCK 
             CHARACTER(14 ):: TIMEUNIT
             CHARACTER(120):: HEADER
             !
             IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
             IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
             IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
             IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
             IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
             !
             HEADER='  PER  STP'//TIMEUNIT//'                WELLID        WBS_ID  LAYER    ROW    COL             RATE'
             IF(WBS%HAS_MNW2) HEADER=TRIM(HEADER)//'  MNW2LINK'
             !
             CALL FMPOUT%FWELLS%SET_HEADER(TRIM(HEADER))
        END BLOCK
    END IF
    !
    !
    !4H-----Allocate space for the double precision array for the Total Delivery Requirement array (TDR, cell-by-cell) 
    !       and the Total Farm Delivery Requirement lists (TFDR,TFDROLD, farm-by-farm) and initialize 
    ALLOCATE(TFDROLD(NFARM), SOURCE=DZ)
    !
    !
    !4P-----Allocate space for the double precision lists of Unranked (UNRD), Ranked (RNRD), & actually used (NRD)
    !       Non-Routed Delivery Lists and initialize
    !
    ALLOCATE( SWFL%NRD(NFARM) )
    !
    IF(.NOT.SWFL%HAS_NRD) THEN
                        ALLOCATE(UNRD(4,NFARM)  , SOURCE=DZ)                                      !DUMMY 4       !DP
                        ALLOCATE(RNRD(2,1,NFARM), SOURCE=DZ)                                      !DUMMY 2,1     !DP
                        ALLOCATE(NRD(2,NFARM)   , SOURCE=DZ)                                      !DP
    ELSE
                        ALLOCATE(UNRD((SWFL%MXNRD*3+1),NFARM), SOURCE=DZ)                         !DP (1=FID; 3 per type = NRD-vol., NRD-rank, NRD-use-flag)
                        IF(ILGR.NE.0) THEN
                        ALLOCATE(RNRD(2,SWFL%MXNRD*2,NFARM)  , SOURCE=DZ)                         !DP (2 per ranked type = NRD-vol., NRD-use-flag) 2 times the MXNRDT to save old RNDR before
                        ELSE                                                                           !scaling it down as a result of prorating for parent and child farm NRDs.        
                        ALLOCATE(RNRD(2,SWFL%MXNRD,NFARM)    , SOURCE=DZ)                         !DP (2 per ranked type = NRD-vol., NRD-use-flag)
                        ENDIF
                        ALLOCATE(NRD(2,NFARM)                , SOURCE=DZ)                         !DP (2: NRD-new, NRD-old)
                        !
                        CALL SWFL%NRD%INIT(SWFL%MXNRD)
    ENDIF
    !
    IF(WBS%HAS_SFR) THEN
                       ALLOCATE(SFR_RUNOFF(NSTRM),         SOURCE = DZ)
                       ALLOCATE(FMP_MOD_SFR_RUNOFF(NSTRM), SOURCE = FALSE)
    END IF
    !
    !5===== ALLOCATE SPACE FOR INTEGER VARIABLES AND INITIALIZE ==============================================
    !
    !      
    !5F------Allocate space for ... lgr link  --Wolfgang hold over...not sure what to do with pointer codes
    ALLOCATE(IFA(NFARM))      
    IF(FDIM%NSOIL>Z) THEN
        ALLOCATE(ISA(FDIM%NSOIL))
        DO NS=1,FDIM%NSOIL
          ISA(NS)=NS
        ENDDO  
    ELSE
        ALLOCATE(ISA(ONE), SOURCE=Z)
    END IF
    !
    IF(FDIM%NCROP>Z) THEN
        ALLOCATE(ICA(FDIM%NCROP))
        DO NC=1,FDIM%NCROP
          ICA(NC)=NC
        ENDDO   
    ELSE
        ALLOCATE(ICA(ONE), SOURCE=Z)
    END IF
    !
    DO NF=1,NFARM
            IFA(NF)=NF   !FOR CHILD MODELS THIS GETS SET TO CHILD FARMS
    END DO     
    !
    !
    ! ALLOCATE DRT LOCATION AND RETURN FLOW TO FMP ARRAY
    ALLOCATE(DRTFLOW(NFARM))
    !
    IF(FDIM%NFARM < ONE) FDIM%NFARM = ONE  ! Override for outside packages to know there is 1 WBS
    !      
    !7===== SAVE POINTERS FOR GRID AND RETURN. ========================================================
    CALL FMP_LGR_PNT_SAV(IGRID)
    !
    CALL SGWF2NWT1PSV(Igrid)   !seb lgr
    CALL SGWF2UZF1PSV(Igrid)   !seb lgr
    !
  END SUBROUTINE
  !
  !     
  SUBROUTINE FMP_RP(KPER,IGRID)
    !
    !     READ NEW FARM WELL LOCATIONS AND STRESS RATES, AND
    !     READ OTHER INFORMATION FOR EACH STRESS PERIOD
    USE CONSTANTS
    USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
    USE FMP_GLOBAL,   ONLY:FWELL,NRD,UNRD,RNRD,IFA,ISTARTFL,FMPOUT,FMP_LGR_PNT,  &
                           SFR_DELIV,SFR_RUNOFF,FMP_MOD_SFR_RUNOFF,              &
                           FDIM, FCROP, WBS, CLIMATE, SOIL, ALLOT, SWFL, SALT, SWODAT
    USE GLOBAL,       ONLY:PERLEN,SPSTART
    USE GWFSFRMODULE, ONLY:ISTRM,STRM,NSTRM,IDIVAR,SEG,IOTSG,SEG_NSTRM
    USE GWFMNW2MODULE,ONLY: SGWF2MNW2PNT
    !
    INTEGER, INTENT(IN)::KPER,IGRID
    !
    CHARACTER(11):: LABEL
    INTEGER:: N,NF,IT, & !FORMERLY IMPLICIT INTEGER
     IRT, F
    DOUBLE PRECISION:: SP_LENGTH, DTMP
    LOGICAL:: FIRST_SP
    !
    INTEGER:: I, J, K, IOUT
    !TYPE(GENERIC_OUTPUT_FILE):: FL
    !
    CALL FMP_LGR_PNT(IGRID)
    CALL SGWF2UZF1PNT(IGRID) 
    CALL SGWF2MNW2PNT(IGRID) 
    CALL SGWF2SFR7PNT(IGRID)
    !
    FIRST_SP  = KPER == ONE
    SP_LENGTH = DBLE(PERLEN(KPER))
    IOUT      = WBS%IOUT
    !
    !2A-----READ FARM-ID (WATER BALANCE PROPERTIES)
    CALL  WBS%NEXT()
    !
    IF(SOIL%HAS_SOIL) THEN
       IF(FIRST_SP .OR. WBS%FID_TFR%TRANSIENT) CALL WBS%CHECK_SOIL_ID(SOIL%SID)
    END IF
    !
    !2A-----READ CLIMATE DATA (PRECIP, REF_ET)
    !
    CALL CLIMATE%NEXT(SOIL,WBS%FID_ARRAY, WBS%NEW_FID, WBS%AREA)
    !
    CALL WBS%SUM_WBS_PRECIP(CLIMATE%HAS_PRECIP, CLIMATE%PRECIP)
    !
    !-----READ SW DATA
    !
    IF( WBS%HAS_SFR ) THEN
        CALL SWFL%NEXT(FDIM, WBS%NEW_FID, WBS%H2OSOURCE%SW, SEG_NSTRM, STRM)
    ELSE
        CALL SWFL%NEXT(FDIM, WBS%NEW_FID, WBS%H2OSOURCE%SW)
    END IF
    !
    IF(SWFL%BUILD_FULLY_ROUTED_RETURN) THEN
        !
        CALL SWFL%BUILD_FULLY_ROUTED_RETURN_SRRLOC(WBS%FID_ARRAY, IDIVAR, ISTRM, STRM)
    END IF
    !
    !2A-----READ CROP PROPERTIES
    CALL FCROP%NEXT(WBS, CLIMATE, SOIL)
    !
    CALL FCROP%CALC_WBS_IRRIGATED_AREA(WBS)
    !
    IF(  FCROP%OUT_INPUT%IS_OPEN)  CALL FCROP%PRINT_OUT_INPUT(WBS,KPER,1)
    !
    !2A-----READ ALLOTMENT PROPERTIES -- MUST BE AFTER CALC_WBS_IRRIGATED_AREA
    IF(WBS%HAS_ALLOT) THEN
        CALL ALLOT%NEXT(WBS%IRR_AREA, SP_LENGTH)
    END IF
    !
    !2B-----READ ANY SALINITY DEMANDS
    !
    IF(WBS%HAS_SALT) CALL SALT%NEXT(FCROP,WBS)
    !
    !
    !3A-----READ FARM WELL INPUT --IF FIRST STRESS PERIOD SET UP MNW2 POITNERS. HAVE TO HAVE THIS HERE BECAUSE MNW2 DOES NOT READ IN WELLIDs TILL KPER==1
    !
    IF(WBS%HAS_WELL) THEN
      !
      !IF(FIRST_SP)  CALL FWELL%MNW2_INDEX(IUNITMNW2)  --MOVED TO AR
      !
      CALL FWELL%NEXT(KPER)        !LOAD NEXT STRESS PERIODS FARM WELLS
      CALL FWELL%NEXT_SETUP()      !CALC QMAX AND SET UP WHAT WELLS ARE ACTIVE --SEPARATE IN CASE THERE ARE EXTERNAL WELLS
      !
      IF(WBS%H2OSOURCE_TFR%INUSE) THEN
          DO CONCURRENT (NF=1:FDIM%NFARM, .NOT. WBS%H2OSOURCE%GW(NF) .AND. FWELL(NF)%N > Z)
                FWELL(NF)%N=0
                FWELL(NF)%ACT=.FALSE.
          END DO
      END IF
      !
      IF(ALLOT%HAS_GW_ALLOTMENT) CALL FWELL%SET_ALLOT(ALLOT%GW_RATE_LIM)
      CALL FWELL%APPLY_ALLOT()
      !
      !
      ! SET UP MNW2 PACKAGE IF WELLS ARE LINKED TO MNW2
      IF(WBS%HAS_MNW2)  CALL FWELL%MNW2_NEWSP()
      !
      WRITE (IOUT,'(/1x,2A)') 'NUMBER OF ACTIVE FARM WELLS: ', NUM2STR(SUM(FWELL%N))
      !
      ! PRINT OUT INPUT TRANSCRIPT IF REQUESTED
      CALL FWELL%PRINT_INPUT( KPER )
      !
    END IF
    !
    IF(ALLOT%HAS_SW_ALLOTMENT .AND. WBS%H2OSOURCE_TFR%INUSE) THEN
        DO CONCURRENT (NF=1:FDIM%NFARM, .NOT. WBS%H2OSOURCE%SW(NF))
              ALLOT%SW_ALLOTMENT(NF) = DZ
              ALLOT%SW_RATE_LIM (NF) = DZ
        END DO
    END IF
    !
    ! PRINT OUT SOME EXTRA INFORMATION:
    IF(WBS%H2OSOURCE_TFR%INUSE) THEN
            WRITE(IOUT,'(/A)') 'WATERSOURCE: FARM DELIVERY WATER SOURCES AVAILABLE FROM:'
            WRITE(IOUT,'(A)')  ' FARM-ID      SOURCES'
            DO NF=1,FDIM%NFARM
                LABEL(1:11)=''
                IF( WBS%H2OSOURCE%GW (NF) ) LABEL(1:2)='GW'
                IF( WBS%H2OSOURCE%SW (NF) ) LABEL(5:6)='SW'
                IF( WBS%H2OSOURCE%NRD(NF) ) LABEL(9:11)='NRD'
                WRITE(IOUT,'(I8, 5x, A)') NF, LABEL(1:11)
            END DO
    END IF
    !
    IF (WBS%HAS_WELL .AND. ALLOT%HAS_GW_ALLOTMENT) THEN
       !
       WRITE(IOUT,'(/1x,A)') 'FARM-ID,  NUMBER OF WELLS PER FARM, ORIGINAL PUMP CAPACIFTY, GROUNDWATER-ALLOTMENT,  FRACTION OF PUMPING CAPACITY PER FARM'
       !
       DO NF=1,FDIM%NFARM
          IF(.NOT. WBS%INUSE(NF)) THEN
                     !
                     WRITE(IOUT,'(4X,I4,20X,I6,9X,2F20.4,A)') NF, 0, 0.0, ALLOT%GW_RATE_LIM(NF), '                 0.0'
                     !
          ELSEIF(ALLOT%GW_RATE_LIM(NF) < DZ) THEN
                     !
                     WRITE(IOUT,'(4X,I4,20X,I6,9X,2A20)')     NF,FWELL(NF)%N, FWELL(NF)%QMAXini,' NaN', ' NaN'
                     !
          ELSEIF(FWELL(NF)%QMAXini <= ALLOT%GW_RATE_LIM(NF)) THEN  ! Not constrained via allotment
              IF( ALLOT%GW_RATE_LIM(NF) > DZ ) THEN
                     WRITE(IOUT,'(4X,I4,20X,I6,9X,2F20.4,A)') NF,FWELL(NF)%N, FWELL(NF)%QMAXini, ALLOT%GW_RATE_LIM(NF), '                 1.0'
              ELSE
                     WRITE(IOUT,'(4X,I4,20X,I6,9X,2F20.4,A)') NF,FWELL(NF)%N, FWELL(NF)%QMAXini, ALLOT%GW_RATE_LIM(NF), '                 0.0'
              ENDIF
          ELSEIF(FWELL(NF)%QMAXini > DZ) THEN
                     WRITE(IOUT,'(4X,I4,20X,I6,9X,3F20.4)')   NF,FWELL(NF)%N,FWELL(NF)%QMAXini, ALLOT%GW_RATE_LIM(NF), ALLOT%GW_RATE_LIM(NF)/FWELL(NF)%QMAXini
          ELSE
                     WRITE(IOUT,'(4X,I4,20X,I6,9X,2F20.4,A)') NF,FWELL(NF)%N, FWELL(NF)%QMAXini, ALLOT%GW_RATE_LIM(NF), '                 0.0'
          END IF
       END DO
    END IF
    !
    !8C1----READ EQUALLY APPROPRIATED FARM ALLOTMENT HEIGHT
    !
    IF(ALLOT%HAS_SW_ALLOTMENT) THEN
          WRITE(IOUT,'(/A)') 'FARM-ID  SURFACE-WATER FARM ALLOTMENT VOLUME [L3]'
          DO NF=1,FDIM%NFARM
             WRITE(IOUT,'(1X,I6,F19.4)') NF, ALLOT%SW_ALLOTMENT(NF)
          ENDDO
    ENDIF
    !
    !PRINT OUT WHAT WAS READ IN
    IF(FWELL(ONE)%LISTPRINT) THEN
          CALL FWELL%PRINT_WELL()
          WRITE(IOUT,*)
    END IF 
    !
    ! seb READ IN FARM WATER RETURN FLOWS IF RETURNFLOW OPTION IS REQUESTED
    !
    IF(SWFL%CHOICE%INUSE .AND. FIRST_SP .OR. SWFL%CHOICE%TRANSIENT)THEN
          !
          WRITE(IOUT,'(/A)') 'WATERSOURCE: FARM RETURN FLOW SPECIFIED AS:'
          WRITE(IOUT,'( A)') ' FARM-ID      ROUTED-RETURNS'
          !
          DO NF=1,FDIM%NFARM
              LABEL(1:11)=''
              IF( SWFL%H2ORETURN(1,NF).NE.Z ) LABEL(1:5)='FULLY'
              IF( SWFL%H2ORETURN(2,NF).NE.Z ) LABEL(8:11)='SEMI'
              WRITE(IOUT,'(I8, 5x, A)') NF, LABEL(1:11)
          END DO
    END IF
    !
    !7===== READ LIST OF NON-ROUTED DELIVERIES TO FARMS (NRDs) AND CREATE ARRAY OF RANKED NRDs ===================
    !
    IF(.NOT. SWFL%HAS_NRD) THEN
       DO NF=1,FDIM%NFARM
          NRD(1,NF) = DZ
          NRD(2,NF) = DZ
          RNRD(1,1,NF) = DZ
          RNRD(2,1,NF) = DZ
          DO N=1,4
                  UNRD(N,NF) = DZ
          ENDDO
       ENDDO
    ENDIF
    !
    !7A-----READ LIST OF YET UNRANKED NON-ROUTED DELIVERIES TO FARMS (VOLUME, RANK, USE-FLAG)
    IF(SWFL%HAS_NRD .AND. (FIRST_SP .OR. SWFL%NRD_TFR%TRANSIENT) ) THEN
        !
        !CALL FMP3DPLSTRD(UNRD,MXNRDT*3+1,FDIM%NFARM,IN,IOUT,0,0,MXNRDT*3+1)
        !
        CALL SWFL%SET_NRD_ARRAY(UNRD, FIRST_SP)
        !
        DTMP = UNO
        IF(.NOT. SWFL%NRD_IS_RAT) DTMP = UNO / SP_LENGTH
        DO J=ONE, FDIM%NFARM
                       CALL SWFL%NRD(J)%SETUP( UNRD(:,J), DTMP )
        END DO
        !
        IF(SWFL%NRD_IS_RAT) THEN
            IT = SWFL%MXNRD*3 + 1
            !
            DO CONCURRENT (J=ONE:FDIM%NFARM, I=2:IT:3)
                                             UNRD(I,J) = UNRD(I,J) * SP_LENGTH
            END DO
        END IF
        !
        WRITE(IOUT,'(/1x,A,/A,*(I12))') 'UNRANKED LIST OF NON-ROUTED DELIVERIES TO FARMS [L3]:','TYPE:  ', (IT, IT=1,SWFL%MXNRD)
        !
        WRITE(IOUT,'(/A)') 'FARM-ID  VOLUME:'
        !
        DO NF=1,FDIM%NFARM      
           IF(IFA(NF).NE.0) WRITE(IOUT,'(1x,I6,*(ES12.3))') IDINT(UNRD(1,NF)), (UNRD(IT*3-1,NF), IT=1,SWFL%MXNRD)
        END DO
        !
        WRITE(IOUT,'(/A)')'FARM-ID  PRIORITY RANK OF NRD-TYPE:'
        !
        DO NF=1,FDIM%NFARM
           IF(IFA(NF).NE.0)  WRITE(IOUT,'(1x,I6,*(I12))') IDINT(UNRD(1,NF)),(IDINT(UNRD(IT*3,NF)),IT=1,SWFL%MXNRD)
           !
           DO IT=1, SWFL%MXNRD
              IF (IDINT(UNRD(IT*3,NF)).LT.1)           CALL STOP_ERROR(OUTPUT=IOUT,MSG='ERROR: FMP REQUIRES THAT THE PRIORITY RANK OF NRD-TYPE (NRDR) BE 1 OR GREATER (ie > 0)'//NL//'A VALUE OF ZERO WAS FOUND FOR FARM: '//NUM2STR(NF))
              !
              IF (IDINT(UNRD(IT*3,NF)).GT.SWFL%MXNRD)  CALL STOP_ERROR(OUTPUT=IOUT,MSG='ERROR: FMP REQUIRES THAT THE PRIORITY RANK OF NRD-TYPE (NRDR) BE LESS THAN OR EQUAL TO MXNRDT.'//NL//'FOR FARM '//NUM2STR(NF)//' THERE IS A RANK OF '//NUM2STR(IDINT(UNRD(IT*3,NF)))//' BUT MXNRDT IS '//NUM2STR(SWFL%MXNRD))
              !
           END DO
        ENDDO
        !
        WRITE( IOUT, '(/A, /29x,A, /39x,A, /49x,A)' ) 'FARM-ID  USE-FLAG:', &
                                                      '0 = SUFFICIENT USE', &
                                                      '1 = ABSOLUTE USE (RECHARGE SURPLUS BACK INTO CANAL)', &
                                                      '2 = ABSOLUTE USE (INJECT SURPLUS INTO FARM-WELLS)'
        !
        DO NF=1,FDIM%NFARM
           IF(IFA(NF).NE.0)  write(IOUT,'(1x,I6,*(I12))') IDINT(UNRD(1,NF)),(IDINT(UNRD(IT*3+1,NF)),IT=1,SWFL%MXNRD)
        ENDDO
        !
        !7B-----CREATE ARRAY OF RANKED NON-ROUTED DELIVERIES TO FARMS (AND ARRAY OF THEIR NRD-USE FLAGS)
        !
        DO NF=1,FDIM%NFARM
           IF(IFA(NF).NE.0 .AND. WBS%INUSE(NF)) THEN
              DO IT=1,SWFL%MXNRD
                 RNRD(1,IDINT(UNRD(3*IT,NF)),NF)=UNRD(3*IT-1,NF)/SP_LENGTH
                 RNRD(2,IDINT(UNRD(3*IT,NF)),NF)=UNRD(3*IT+1,NF)
              ENDDO
           ENDIF
        ENDDO
        !
        WRITE(IOUT,'(/A)') 'LIST OF NON-ROUTED DELIVERIES IN RANKED SEQUENCE [L3/T]:'
        !
        WRITE(IOUT,'(/A)')'FARM-ID  RATE:'
        !
        DO NF=1,FDIM%NFARM
             IF(IFA(NF).NE.0)  WRITE(IOUT,'(1x,I6,*(ES12.3))') NF,(RNRD(1,IRT,NF), IRT=1,SWFL%MXNRD)
        ENDDO
        !
        WRITE(IOUT,'(/A, /29x,A, /39x,A, /49x,A)') 'FARM-ID  USE-FLAG:','0 = SUFFICIENT USE','1 = ABSOLUTE USE (RECHARGE SURPLUS BACK INTO CANAL)','2 = ABSOLUTE USE (INJECT SURPLUS INTO FARM-WELLS)'
        !
        DO NF=1,FDIM%NFARM
               IF(IFA(NF).NE.0) WRITE(IOUT,'(1x,I6,*(I12))') NF,(IDINT(RNRD(2,IRT,NF)), IRT=1,SWFL%MXNRD)
        ENDDO
        !
    ENDIF
    !
    !8===== READ INFORMATION IF LINKED TO STREAMFLOW ROUTING PACKAGE ============================================
    !
    IF(WBS%HAS_SFR) THEN
       !
       CALL SFR_DELIV%RESET()
       !
       CALL SWFL%ADD_SRD_TO_SFR_DELIV(SFR_DELIV)  !INCLUDE ANY SRDs IN GLOBAL FMP-SFR
       !
       DO CONCURRENT(I=1:NSTRM); FMP_MOD_SFR_RUNOFF(I) = FALSE
       END DO
       !
       DO CONCURRENT(I=1:NSTRM); SFR_RUNOFF(I) = DZ
       END DO
       !
       ! ASSUMES THAT SFR_DELIV IS FULLY MAPPED!!!!
       !
       DO CONCURRENT (NF=1:FDIM%NFARM)           !LOGICAL TO KEEP TRACK OF MODIFIED SFR RUNOFF REACHES
       DO CONCURRENT (I=ONE:SWFL%SRRLOC(NF)%N)
           IF(SWFL%SRRLOC(NF)%ISTRM(I) > Z) FMP_MOD_SFR_RUNOFF(SWFL%SRRLOC(NF)%ISTRM(I)) = TRUE
       END DO
       END DO
       !
       DO CONCURRENT(I=ONE:SFR_DELIV%N)           ! KEEP TRACK OF MODIFIED SFR DIVERSION REACHES
          FMP_MOD_SFR_RUNOFF(SFR_DELIV%ISTRM(I)) = TRUE
       END DO
       !
       DO I=1, NSTRM
       IF(FMP_MOD_SFR_RUNOFF(I)) THEN
           !
           STRM(12,I)=SEG(3,ISTRM(4,I))*(STRM(1,I)/SEG(1,ISTRM(4,I)))
           !
           SFR_RUNOFF(I)=DBLE(STRM(12,I))
       END IF
       END DO
    END IF
    !
    IF(SWFL%HAS_SRD) THEN  ! SWF%ISRD%ARRAY CHANGED SWF%ISRD%SEGRCH
      !
      IF(KPER==1 .OR. SWFL%ISRD_TFR%TRANSIENT) THEN 
          WRITE(IOUT,'(/1x,A, /A)') 'SEMI-ROUTED DELIVERIES TO FARMS (LOCATION OF DIVERSION-POINTS):', '  WBS  SEG  RCH  FRACTION'
          DO F=ONE, SWFL%NFARM
            DO K = ONE, SWFL%SRDLOC(F)%N                        
               ASSOCIATE(S  => ISTRM(4,SWFL%SRDLOC(F)%ISTRM(K)),    &
                         R  => ISTRM(5,SWFL%SRDLOC(F)%ISTRM(K)),    &
                         WT =>         SWFL%SRDLOC(F)%WT(K)     )
                         !
                         IF(WT>-0.0001) THEN !0.123456
                             WRITE(IOUT,'(3I5, 1x,F9.6)') F, S, R, WT
                         ELSE
                             WRITE(IOUT,'(3I5, 2x,A)'   ) F, S, R, '--'    
                         END IF
                         !
               END ASSOCIATE
            END DO
          END DO
      END IF
    END IF
    !
    IF(FMPOUT%HAS_ROUT == TWO .AND. KPER>SPSTART) THEN
        CALL FMPOUT%ROUTING_INFORMATION%CLOSE()
        FMPOUT%HAS_ROUT=Z
    END IF
    !
    ! SWO DATA INPUT FOR RP
    !
    IF(SWODAT%HAS_SWO) THEN
               WBS%HAS_SWO =  KPER >= SWODAT%BEGIN_SP
               !
               IF(WBS%HAS_SWO)  CALL SWODAT%NEXT(WBS, SWFL, KPER, IDIVAR, IOTSG, SEG, ISTRM, STRM)
    END IF
    !
  END SUBROUTINE
  !
  !
  SUBROUTINE FMP_SUBLINK(IGRID)
    !-----VERSION 2.01 07/29/2010 FMP_SUBLINK
    !*********************************************************************
    !     APPLY SUBSIDENCE DISPLACEMENT (DVZ) TO GROUND-SURFACE ELEVATION,
    !     RISE-OVER-RUN SLOPE, AND THE LOWEST ELEVATION WITHIN A FARM.
    !*********************************************************************
    !        SPECIFICATIONS:
    !     -----------------------------------------------------------------
    USE FMP_GLOBAL,      ONLY: FMP_LGR_PNT, WBS
    USE FMPBLK,       ONLY:ZER
    USE GLOBAL,       ONLY:NROW,NCOL,NLAY,IBOUND,SUBLNK  
    USE GWFSUBMODULE, ONLY:DVZ
    !
    INTEGER, INTENT(IN):: IGRID
    INTEGER:: IR,IC,IL
    !
    !CALL FMP_LGR_PNT(IGRID)  -- ALREADY CALLED BY FMP_AD
    !
    IF(SUBLNK)THEN
       !
       IF(ANY(DVZ.NE.ZER)) THEN
          !
          !1-----APPLY DISPLACEMENT TO GROUND-SURFACE ELEVATION OF UPPERMOST ACTIVE LAYER    
          ROW: DO IR=1,NROW
          COL: DO IC=1,NCOL
          LAY: DO IL=1,NLAY
                       IF(IBOUND(IC,IR,IL).EQ.0.AND.IL.NE.NLAY) CYCLE LAY
                       !
                       WBS%GSE(IC,IR)=WBS%GSE(IC,IR)-DVZ(IC,IR,IL)
                       CYCLE COL
          ENDDO LAY
          ENDDO COL
          ENDDO ROW 
          !
          !IROUT=FMPOUT%ROUTING_INFORMATION%IU
          !2-----RECOMPUTE RISE-OVER-RUN SLOPE
          !      IF(IIESWFL.EQ.0) CALL RISE_RUN()
          !
          !3-----RECHECK FOR LOWEST ELEVATION WITHIN ANY FARM AND NEAREST REACH
          !!!      IF(IRRFL.NE.0) THEN
          !!!      DO NF=1,NFARMS
          !!!       IF(LFID(NF))THEN                                                 !test for active farms  -- rth
          !!!         IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
          !!!           WRITE(IROUT,'(//,"RECHECK RETURNFLOW INFORMATION FOR FARM: ",
          !!!     1     I8,", PERIOD ",I4,/,A51)') NF, KPER,
          !!!     2     "---------------------------------------------------"
          !!!         ENDIF
          !!!         CALL RETURNFLOW(NF,IROUT)
          !!!       END IF
          !!!      ENDDO
          !!!      END IF
       END IF
    END IF !SUBLNK
    !
  END SUBROUTINE
  !
  !
  SUBROUTINE FMP_AD(KPER, KSTP, IGRID)
    !*********************************************************************
    !     RESET (REDUCED) DIVERSION RATE OF PREVIOUS TIME STEP,
    !     COMPUTE AVERAGE EVAPOTRANSPIRATION, ROOT DEPTHS AND EFFECTIVE
    !     PRECIPITATION PER TIME STEP, AND CALCULATE THE MAXIMUM ET FROM
    !     GROUNDWATER.
    !     RESET HEAD-DEPENDENT EFFICIENCIES TO SPECIFIED EFFICIENCY
    !     (ONLY CALLED IF ICUFL, IPFL, OR IRTFL = 3, OR IEBFL = 1 OR = 3)
    !*********************************************************************
    !        SPECIFICATIONS:
    !     -----------------------------------------------------------------
    USE CONSTANTS
    USE FMP_GLOBAL, ONLY:FDIM, WBS, SOIL, CLIMATE, FCROP, SALT, SWFL, ISTARTFL,  FMP_LGR_PNT, SFR_RUNOFF, FMP_MOD_SFR_RUNOFF, SWODAT
    USE GWFSFRMODULE, ONLY: IDIVAR, ISTRM, STRM, SEG, NSTRM
    USE GWFMNW2MODULE,ONLY: SGWF2MNW2PNT
    !
    INTEGER, INTENT(IN):: IGRID,KPER,KSTP
    !
    INTEGER:: NF, I
    LOGICAL:: RELOAD_CROP, SKIP_LOAD
    !
    CALL FMP_LGR_PNT(IGRID)
    CALL SGWF2NWT1PNT(IGRID)   !seb lgr
    CALL SGWF2UPW1PNT(IGRID)   !seb lgr
    CALL SGWF2UZF1PNT(IGRID)   !seb lgr
    CALL SGWF2SFR7PNT(IGRID)   !seb lgr
    CALL SGWF2MNW2PNT(IGRID)   !seb lgr
    CALL SGWF2MNW1PNT(IGRID)   !seb lgr 
    !
    !1===== SET ISTARTFL FLAG ============================================================================
    !
    !1A-----SET FLAG TO INDICATE START OF STRESS PERIOD AS START OF CHANGING CONDITIONS OF CONSUMPTIVE USE 
    !       (info: if consumptive use changes at each time-step, then istartfl is set zero in FMP_AD.)
    ISTARTFL=0
    !
    !1B-----FOR USE OF SFR1 AND (SEMI-)ROUTED DELIVERIES, SET FLAG TO SKIP FMP_FM IN 1ST ITERATION
    !
    !     MADE IT FOR ALL SCENARIOS
    !       skipping FMP_FM in 1st iteration is strictly not necessary if diversion rate doesn't change
    !       (as caused by "water-conservation pool"), but found better by standard for faster convergence!            
    !
    !
    IF(SWFL%HAS_SRD) istartfl=istartfl-1
!    IF(WBS%HAS_SFR) istartfl=istartfl-1
    !
    !1===== RESET SFR RUNOFF ============================================================================
    !
    
    IF(KSTP > ONE .AND. WBS%HAS_SFR) THEN
           DO CONCURRENT (I=1:NSTRM, FMP_MOD_SFR_RUNOFF(I))
               !
               STRM(12,I) = SFR_RUNOFF(I)
               !
           END DO
    END IF
    !
    RELOAD_CROP = FALSE
    !
    IF(KSTP > ONE .AND. FDIM%HAS_BY_TIMESTEP) THEN
      !
      SKIP_LOAD = .NOT. FDIM%CROP_BY_TIMESTEP !CROP MAY NEED TO RECALCULATE FID AND CLIM INFORMATION BUT NOT LOAD NEXT SP PROPERTIES (OR IN THIS CASE TS PROPERTIES)
      !
      IF(FDIM%WBS_BY_TIMESTEP) THEN
          !
          CALL  WBS%NEXT()
          !
          IF(WBS%NEW_FID) THEN
               !
               CALL WBS%CHECK_SOIL_ID(SOIL%SID)
               !
               DO CONCURRENT (NF=ONE:SWFL%NFARM)
                                           SWFL%BUILD_FRR(NF) = SWFL%SRRLOC(NF)%HAS_RETURN .AND. SWFL%SRRLOC(NF)%FULLY
               END DO
               !
               SWFL%BUILD_FULLY_ROUTED_RETURN = ANY(SWFL%BUILD_FRR)
               !
               IF(SWFL%BUILD_FULLY_ROUTED_RETURN) CALL SWFL%BUILD_FULLY_ROUTED_RETURN_SRRLOC(WBS%FID_ARRAY, IDIVAR, ISTRM, STRM)
          END IF
          !
          RELOAD_CROP = TRUE
      END IF
      !
      IF(FDIM%CLIMATE_BY_TIMESTEP) THEN
               CALL CLIMATE%NEXT(SOIL,WBS%FID_ARRAY, WBS%FID_TFR%TRANSIENT, WBS%AREA)
               RELOAD_CROP = TRUE
      END IF
      !
      IF(FDIM%WBS_BY_TIMESTEP .OR. FDIM%CLIMATE_BY_TIMESTEP)  CALL WBS%SUM_WBS_PRECIP(CLIMATE%HAS_PRECIP, CLIMATE%PRECIP)
      !
      IF(FDIM%CROP_BY_TIMESTEP .OR. RELOAD_CROP) THEN
          !
          CALL FCROP%NEXT(WBS, CLIMATE, SOIL, FDIM%CROP_BY_TIMESTEP)
          CALL FCROP%CALC_WBS_IRRIGATED_AREA(WBS)
          !
          IF(   FCROP%OUT_INPUT%IS_OPEN)  CALL FCROP%PRINT_OUT_INPUT(WBS,KPER,KSTP)
          !
          IF(WBS%HAS_SALT) CALL SALT%CROP_BY_TS(FCROP)
      END IF
    END IF
    !
    ! INITILIZE THE WATER SUPPLIES FOR THE WBS
    CALL WBS%SUPPLY%INIT()
    !
    ! INITIALIZE THE CROP IRRIGATION EFFICIENCIES. 
    ! THIS MAY CHANGE OVER ITERATIONS IF IMPROVEMENT IS ALLOWED
    !
    CALL FCROP%NEXT_TS(WBS)
    !
    IF(WBS%HAS_SALT) CALL SALT%NEXT_TS()  !INITIALIZE VARIBALES FOR NEXT TS
    !
    IF(SWFL%HAS_SW)  CALL SWFL%NEXT_TS()
    !
    IF(WBS%HAS_SWO)  CALL SWODAT%NEXT_TIME_STEP(WBS,KPER,KSTP,SEG)
    !
  END SUBROUTINE
  !
  !
  SUBROUTINE FMP_FM(KITER,KPER,KSTP,IGRID,NGRIDS,ILGR,LGRITER,DELT)  !added IUNITNWT,IUNITMNW2 by rth
    !     ******************************************************************
    !     COMPUTE FARM WELL DISCHARGE AND NET-RECHARGE AND ADD THE TERMS TO
    !     RHS OR THE RHS AND HCOF, RESPECTIVELY.
    !     Determine for each farm:
    !                * Total Farm Delivery Requirement; 
    !                * Actual Delivery from Non-Routed and (Semi-)Routed Surface-Water; &
    !                * Groundwater Pumping Requirement.
    !     If Demand > Supply:
    !                * Assume Zero Scenario; or
    !                * Apply Deficiency Scenarios (specified by user).
    !     Update for each Scenario:
    !                * Actual Delivery from Non-Routed and (Semi-)Routed Surface-Water:
    !                  optionally divert it from canals - if defined by SFR);
    !                * Discharge from each Farm Well:
    !                  add Q of FMP-farmwells to RHS, and
    !                  optionally use Q of MNW-farmwells as Q-desired in MNW package);
    !                * Net Recharge and add it to RHS and HCOF;
    !                * Surface-Water Runoff and route it to Drains (if defined by SFR).     
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE CONSTANTS
    USE FMP_GLOBAL, ONLY:  TFDROLD, NRD,  UNRD, RNRD, IFA, FMPDAT, FMP_LGR_PNT,         &
                           ISTARTFL, SFR_RUNOFF, DRTFLOW,                               &
                           WBS, FWELL, SWFL, FCROP, SOIL, FDIM, ALLOT, CLIMATE, SALT, SWODAT,  &
                           FMP_MOD_SFR_RUNOFF, SFR_DELIV, FMPOPT 
    USE FMPBLK
    !USE GWFBASMODULE, ONLY:DELT
    USE GLOBAL,          ONLY: RHS,HCOF,NCOL,NROW,PERLEN,UPLAY,WTABLE !,HNEW
    USE GWFMNW2MODULE,   ONLY: SGWF2MNW2PNT
    USE GWFSFRMODULE,    ONLY: STRM,NSTRM,SEG,IDIVAR
    USE GWFUZFMODULE,    ONLY: IUZFBND,FINF,EXCESPP,VKS
    USE LGRMODULE,       ONLY: LGRDAT
    USE GWFNWTMODULE,    ONLY: Heps
    USE FMPFUNCT,        ONLY: RTFUNC
    USE WEL_SUBROUTINES, ONLY: WEL_SMOOTH, WEL_DERIV
    USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
    !
    INTEGER,          INTENT(IN):: KITER,KPER,KSTP,IGRID,NGRIDS,ILGR,LGRITER
    DOUBLE PRECISION, INTENT(IN):: DELT
    !
    !TYPE(GENERIC_OUTPUT_FILE):: FL
    !
    INTEGER:: NF,IR,IC,N,IL,I,                   &
              NT,IDDELF,IT,ITTOT,IRT,ITU,K, &
              NFF
    !   
    INTEGER:: INRDR(SWFL%MXNRD),INRDU(SWFL%MXNRD) 
    !
    DOUBLE PRECISION:: TF, QR, RDEL, QTOL
    DOUBLE PRECISION:: ND, SURPLUS1,SURPLUS2, DSUM, SUFDSUM
    DOUBLE PRECISION:: FARMALLOT
    DOUBLE PRECISION:: CTFDR  !WELLFIELD
                     !HH,SS,XX,YY,PCTG,TF,ND,NW,QR,SW,CCIR,DSUM,                       &
                     !SUFDSUM,,RDEL,RDELCOR,AFDELSW,QTOL,TFOLD,EF,    &
                     !ADSW,QMXF,NRDE,ALLOTDP,,FLOWIN,GWSUM,SWSUM,NRSUM,AVI,   &
                     !TDROLD,CIRP,CIR,EFOLD,TMAX,TPOT,GSE,RRZ,TRZ,LXX,MXX,UXX,         &
                     !PSIA,MLT,DRZ,NEXP,PSI0,PSI1,PSI2,PSI3,PSIWET,PSIDRY,XWET,XDRY,   &
                     !THI,THD,TACT,EPOT,EHI,EHD,EACT,P,TP,EP,PET,CECT,RMIN,RMAX,RMAXP, &
                     !RIRR,RPREC,RHSS,HCOFF,HCOFFOLD,DIG1,DIG2,TI,SWRUNOFF,            &
                     !SWRUNSUM,SWRUNSUMC,AVGSUPPLYFLR,AVGSUPPLYFLUX,QSUMDF,QSUMEX,QDF, &
                     !QEX,THISAT1,THDSAT1,THISAT2,THDSAT2,TGWSAT1,TGWSAT2,ALPHA,       &
                     !QACT,QMAXMNW2,QMAXMNW3,FIESWI,FIESWP,TG,DRDC,                    &
                     !GWCOST,SWRCOST,SWNRCOST,GWPROFIT,SWRPROFIT,SWNRPROFIT,YIELD,     &
                     !BENEFIT,FLUX,CTFDR,SUPPLYFLR,TDRNONFAL,SUPPLYFLROLD,SUPPLYFLRNEW,&
                     !SUPPLYFLRNEWOLD,SWPREV,SWOLDPREV,SWREDPREV,CRDEL,TOT_DIV_FLOW
    !
    LOGICAL:: GO_SWO_ALLOC
    !
    INTEGER:: SITER                       !INTEGER VARIABLES FOR MNW2 LINK seb
    !
    CALL FMP_LGR_PNT(IGRID)
    CALL SGWF2LGR2PNT(IGRID) 
    !
    IF(WBS%HAS_SFR)  CALL SGWF2SFR7PNT(IGRID)               !necessary as FMP is called before SFR in FM loop
    IF(WBS%HAS_UZF)  CALL SGWF2UZF1PNT(Igrid)  
    IF(WBS%HAS_MNW2) CALL SGWF2MNW2PNT(IGRID)  
    IF(WBS%HAS_NWT)  CALL SGWF2NWT1PNT(Igrid)  
    IF(WBS%HAS_UPW)  CALL SGWF2UPW1PNT(Igrid)  
    IF(WBS%HAS_MNW1) CALL SGWF2MNW1PNT(Igrid) 
    !
    !
    !1===== ADVANCE ISTARTFL FLAG BY ONE =============================================================
    !
    !       1. ISTARTFL=-1 INDICATES NEW CONSUMPTIVE USE CONDITION IN FM:
    !
    !          ISTARFL=-1 is advanced to ISTARTFL=0 to skip 1st FMP_FM iteration (see comment C4 below) 
    !          and to solve MF2K5 without the FMP but with SFR in order to get good initial "pre-farming"
    !          streamflow values for the 2nd iteration that are not biased by previous head-dependent 
    !          consumptive use conditions, which can influence the initial streamflow.
    !
    !       2. ISTARTFL=0 INDICATES FIRST FMP_FM ITERATION:
    ! 
    !          If ISTARTFL=0 is advanced to ISTARTFL=1 then the 2nd MF2K5, but first FMP_FM iteration occurs.
    !          ISTARTFL=0 is advanced to ISTARTFL=1 and the 2nd MF2K5, but first FMP_FM iteration.
    !          Hence, ISTARTFL=0 tells when to reset some parameters at the beginning of the first FMP_FM
    !          iteration (e.g., to reset reduced cell-area percentages to allow a new acreage-optimization)
    !
    ISTARTFL=ISTARTFL+1
    !  write(iout,*) "TIMESTEP FLAG ... =",ISTARTFL,KPER,KSTP,KITER
    !      Moved to after SWO Calculation so it has record of previous iteration of runoff
    !IF(ISTARTFL .LE. Z) THEN
    !    IF(WBS%HAS_SFR) THEN !RESEST BACK TO ORIGINAL RUNOFF FROM SFR INPUT
    !       !
    !       DO CONCURRENT (I=1:NSTRM, FMP_MOD_SFR_RUNOFF(I))
    !           !
    !           STRM(12,I) = SFR_RUNOFF(I)
    !           !
    !       END DO
    !    END IF
    !END IF
    !
    !4===== SOME SETTINGS AND INITIALIZATIONS ======================================================================
    !
    !-----DISABLE PARENT FARMS AND ALL ASSOCIATED PROCESSES WHERE CHILD MODELS EXIST.
    !     (THIS COULD SIT IN SOME FORM IN FMP3RQ ... BUT REQUIRES FMP TO BE IN NAME FILE OF CHILD MODEL)
    !
    IF(ILGR.NE.0.AND.LGRITER.EQ.1.AND.IGRID.EQ.1) WBS%FID_ARRAY = WBS%FID_TFR%ARRAY
    !
    IF(ILGR.NE.0.AND.LGRITER.GT.1.AND.IGRID.EQ.1) THEN 
        DO IR=1,NROW
        DO IC=1,NCOL
           DO N=1,NGRIDS
             IF(IR.GE.LGRDAT(N)%NPRBEG.AND.IR.LE.LGRDAT(N)%NPREND.AND.IC.GE.LGRDAT(N)%NPCBEG.AND.IC.LE.LGRDAT(N)%NPCEND) THEN
                 !
                 WBS%FID_ARRAY(IC,IR)=0
             ENDIF
           ENDDO
        ENDDO
        ENDDO
        CALL WBS%SETUP_FID_RC()
    ENDIF
    !
    !
    IF(WBS%HAS_WATERSTACK) CALL FCROP%NOT_FALLOW_RESET()  !FEATURE NOT YET IMPLIMENTED
    !
    !4B-----INITIALIZE SOME VARIABLES 
    ND = DZ
    SURPLUS1 = DZ
    SURPLUS2 = DZ
    IF(( WBS%HAS_SFR .AND. SWFL%HAS_SRD        .AND. ISTARTFL.EQ.0 )  &
        .OR.                                                          &
       ((WBS%HAS_SFR  .OR. .NOT. SWFL%HAS_SRD) .AND. ISTARTFL.EQ.1  )   ) THEN
                                                                          TFDROLD=DZ
                                                                          NRD=DZ
                                                                          WBS%Q_DEMAND     = DZ
                                                                          WBS%Q_DEMAND_INI = DZ
    ENDIF 
    !
    !
    !-----MNW2 LINK SET QDES AND QACT FOR KITER=1
    IF(WBS%HAS_WELL .AND. WBS%HAS_MNW2 .AND. KITER.EQ.1) CALL FWELL%MNW2_ZERO_Q()      !SET QACT and QDES for MNW2 on first interation
    !
    IF(SWFL%HAS_NRD) CALL SWFL%NRD%INIT_CONSUME()
    !
    ! INITIALIZE SUPPLY VARIABLES TO ZERO  --CALLED IN AD ROUTINE
    !      IF(KITER.EQ.1) THEN
    !          CALL WBS%SUPPLY%INIT()
    !          CALL FCROP%SETUP_CROP_EFFICIENCY(WBS,TRUE) !SET UP EFFICIENCY 
    !      END IF
    !      
    !4C-----SKIP 1ST FMP_FM-ITERATION
    IF(ISTARTFL.LE.0) RETURN
    !
    !IF(WBS%HAS_SFR) THEN !RESEST BACK TO ORIGINAL RUNOFF FROM SFR INPUT -> seb, moved to after SWO computation block
    !   !
    !   DO CONCURRENT (I=1:NSTRM, FMP_MOD_SFR_RUNOFF(I))
    !       !
    !       STRM(12,I) = SFR_RUNOFF(I)
    !       !
    !   END DO
    !END IF
    !
    !GET POTENTIAL FLOW FROM SFR DELIVERY POINTS
    IF(WBS%HAS_SFR) CALL SFR_DELIV%SET_INFLOW(STRM)
    !
    !INITIALIZE Q FOR NEW FM LOOP  --CATCH ALL IN CASE THERE IS A NONZERO VALUE CARRIED OVER THAT IS NOT OVER WRITTEN
    CALL FWELL%FWELL_ZERO_Q()
    !
    !4D-----INITIALIZE SOME LOCAL VARIABLES
    FARMALLOT = DZ
    !
    !5===== UPDATE QMAXF WHEN QMAXRESET FLAG OCCURS ======================================= THIS USED TO BE FOR FOR MNW IF QMAX OF MNW-WELLS ITERATIVELY CHANGES NOW CAN APPLY TO ALL WHENEVER QMAXRESET IS IN USE FOR NWT SMOOTHED PUMPING
    !       (ONLY IN FM WHEN QMAXRESET FLAG IS SET TO TRUE. -- QMAX NORMALLY DONE IN RP)
    !
    IF(WBS%HAS_WELL) THEN
                     IF(ISTARTFL.EQ.1  .OR.  KITER.EQ.1)  CALL FWELL%AUX_QMAXRESET()
                     !
                     !-----MNW2 LINK CHECK IF MNW2 CAN PROVIDE DESIRED PUMPING FOR FARM WELLS FOR KITER>=3
                     IF(WBS%HAS_MNW2 .AND. KITER.GE.3 ) CALL FWELL%MNW2_UPDATE_CAP()
                     ! SET UP TOTAL WELL SUPPLY
                     CALL WBS%SUPPLY%SET_WEL_TOT(FWELL%QMAX) !ONE TO ONE ELEMENTAL SUBROUTINE MATCHED BY NFARM DIMENSION
    END IF
    !
    !6===== DETERMINE ACTUAL TRANSPIRATION FROM GROUNDWATER FOR EACH FARM CELL, AND               ===============
    !       DETERMINE CROP IRRIGATION REQUIREMENT & TOTAL DELIVERY REQUIREMENT FOR EACH FARM CELL
    !       =====================================================================================
    !
    !
    ! EFFICIENCY CHANGES WITHIN FM INTERATIONS, SO THEY NEED TO BE RESET
    !
    IF(WBS%EFF_IMPROVE) CALL FCROP%SETUP_CROP_EFFICIENCY(WBS,TRUE)
    !
    CALL FCROP%CALC_EGW_TGW(SOIL, KITER) !NEEDS WBS%AREA AND SOIL PROPERTIES
    CALL FCROP%CALC_CIR_DEMAND(.TRUE.)
    !
    CALL FCROP%INIT_EXTERNAL_DEMAND()  !SET TO ZERO OR ANY CROP ADDED DEMAND
    !
    ! ADD IN ADDITIONAL ADDED DEMANDS  --------
    !
    CALL FCROP%SET_WBS_ADDED_DEMAND(WBS)  !IF THERE ARE ADDED DEMANDS SPECIFIED BY WBS, ADD THEM
    !
    ! THIS MUST BE THE LAST TO GET DEEP PERC ESTIMATE CORRECT
    IF(WBS%HAS_SALT) CALL SALT%DEMAND_CALC(FCROP, WBS, CLIMATE)     !IF THERE IS ADDITIONAL IRRIGATION FOR SALINTY, ADD IT
    !
    ! END OF ADDING ADDITIONAL DEMANDS --------
    !
    CALL FCROP%SET_EXTERNAL_DEMAND_INI()  !BACK UP INITIAL ADDED DEMANDS IN CASE OF DEFICIT IRRIGATION
    !
    CALL FCROP%ADD_CROP_DEMAND_TO_ARRAY(NCOL,NROW,WBS%CROP_DEMAND_ARRAY)  !ADDS CROP DEMANDS TO WBS%CROP_DEMAND_ARRAY(NCOL,NROW)
    !
    IF(FCROP%HAS_DEMAND_EXT) THEN
        WBS%CROP_DEMAND_ADDED = DZ
        CALL FCROP%ADD_CROP_ADDED_DEMAND_BYWBS(WBS%NFARM, WBS%CROP_DEMAND_ADDED)
    END IF
    !
    CALL WBS%SUM_WBS_DEMAND()  !POPULATES WBS%WBS_DEMAND(NFARM) FROM WBS%DEMAND(NCOL,NROW)
    !
    IF(ISTARTFL.EQ.1) WBS%DEMAND_INI = WBS%DEMAND
    !
    WBS%DEMAND_POT = WBS%DEMAND
    !
    !
    !6F-----SET QMAX TO ZERO FOR WELLS WHOSE CELLS HAVE A ZERO CROP IRRIGATION REQUIREMENT
    !       AND RESET QMAX BACK TO DEFAULT VALUE AT FIRST ITERATION OF EACH TIME STEP
    !
    IF(WBS%HAS_WELL .AND. ISTARTFL.EQ.1) THEN !.OR.KITER.EQ.1  ---KITTER NEVER = 1
      DO NF=1,WBS%NFARM
      IF(FWELL(NF)%NOCIRNOQ) CALL FWELL(NF)%AUX_NOCIRNOQ(WBS%CROP_DEMAND_ARRAY, TRUE ) 
      END DO
      !
      ! CHECK IF ANY WELLS ARE NOW LOWCATED IN AN IBOUND = 0 CELL. IF SO THEY ARE REMOVED FOR THE REST OF THE STRESS PERIOD.
      CALL FWELL%IBOUND_CHECK()
      !
    END IF
    !
    !6G-----WELL-FIELD OPTION - ALLOWS A SERIES OF IRRIGATED FARMS TO RECEIVE THEIR CUMULATIVE IRRIGATION 
    !       DEMAND AS SIMULATED NON-ROUTED DELIVERIES FROM WELL FIELDS SIMULATED AS VIRTUAL FARMS.
    !
    IF(FMPOPT%WELLFIELD) THEN
       IF(KITER.GT.1)THEN        
          !
          !6G2-----FOR EACH WELL FIELD SIMULATED AS NON-ROUTED DELIVERY TYPE
          DO NT=1,SWFL%MXNRD      
             CTFDR = DZ
             !
             !6G2A----CALCULATE CUMULATIVE DELIVERY REQUIREMENT OF FARMS SUPPLIED BY A PARTICULAR WELL FIELD
             !        AND EVALUATE AVAILABLE SIMULATED "NON-ROUTED DELIVERY" FROM THAT WELL FIELD
             DO NF=1, WBS%NFARM
             IF(WBS%INUSE(NF)) THEN
                !
                IF(TFDROLD(NF) .EQ. DZ) TF=WBS%DEMAND(NF)
                IF(TFDROLD(NF) .GT. DZ) TF=TFDROLD(NF)           
                IF(IDINT(UNRD(NT*3+1,NF)).LT.0) THEN
                     IF(NT.EQ.1) THEN
                       CTFDR=CTFDR+TF
                     ELSE
                       IF(RNRD(1,NT-1,NF).LT.TF) THEN
                       CTFDR=CTFDR+(TF-RNRD(1,NT-1,NF))
                       ENDIF
                     ENDIF
                     IDDELF=IDINT(ABS(UNRD(NT*3+1,NF)))
                     RNRD(1,NT,IDDELF)=-MIN(CTFDR,FWELL(IDDELF)%QMAX)
                ENDIF
             END IF
             END DO
             !
             !6G2B----RE-DISTRIBUTE NON-ROUTED DELIVERY FROM WELL FIELD OVER TO RECCEIVING FARMS WEIGHTED BY EACH FARM'S 
             !        TOTAL DELIVERY REQUIREMENT (OR RESIDUAL DELIVERY REQUIRMENT FOR LOWER PRIORITY WELL FIELDS)
             DO NF=1, WBS%NFARM
             IF(WBS%INUSE(NF)) THEN
                !
                IF(TFDROLD(NF) .EQ. DZ) TF=WBS%DEMAND(NF)
                IF(TFDROLD(NF) .GT. DZ) TF=TFDROLD(NF)         
                IF(CTFDR.GT.0) THEN
                     IF(IDINT(UNRD(NT*3+1,NF)).LT.0)THEN
                         IF(NT.EQ.1)THEN
                         RNRD(1,NT,NF)=-RNRD(1,NT,IDDELF)*TF/CTFDR   
                         ELSE
                         RNRD(1,NT,NF)=-RNRD(1,NT,IDDELF)*(TF-RNRD(1,NT-1,NF))/CTFDR
                         ENDIF
                         RNRD(2,NT,NF) = DZ
                     ENDIF
                ENDIF
             END IF
             END DO
          ENDDO
       ENDIF
    ENDIF    !(FMPOPT%WELLFIELD) 
    !
    IF(WBS%HAS_SWO) THEN
        IF(KITER <= SWODAT%SWMXITER(1)) THEN
                                            GO_SWO_ALLOC = TRUE
        ELSEIF(KITER > SWODAT%SWMXITER(3)) THEN
                                            GO_SWO_ALLOC = FALSE
        ELSE
                                            GO_SWO_ALLOC = MOD(KITER,SWODAT%SWMXITER(2)) == Z
        END IF
    ELSE
        GO_SWO_ALLOC = FALSE
    END IF
    !
    IF(GO_SWO_ALLOC) THEN 
    !
    SWODAT%FMP_SW_DEMAND = WBS%DEMAND
    !
    ! ASSUME NON-ROUTED DELIVERIES ARE USED BEFORE MAKING A CALL ON RESERVOIR
    IF(SWFL%HAS_NRD) THEN
        ASSOCIATE(SWO_DMD => SWODAT%FMP_SW_DEMAND)
           !
           DO NF=1, WBS%NFARM
           IF(SWO_DMD(NF)> NEARZERO_10 .AND. WBS%H2OSOURCE%NRD(NF)) THEN
                 ITTOT = Z
                 DO IT=1,SWFL%MXNRD
                     N = IDINT(UNRD(3*IT,NF))                           !define nrd-ranks as array to find maximum rank for current farm
                     IF(ITTOT < N) ITTOT = N
                 END DO
                 !
                 DSUM=DZ
                 DO IRT=1,ITTOT
                    DSUM=DSUM+RNRD(1,IRT,NF)
                 ENDDO
                 !
                 SWO_DMD(NF) = SWO_DMD(NF)-DSUM
           END IF
           END DO
           !
        END ASSOCIATE
    END IF
    !
    DO CONCURRENT(NF=1:WBS%NFARM, SWODAT%FMP_SW_DEMAND(NF) < NEARZERO_10)
        SWODAT%FMP_SW_DEMAND(NF) = DZ
    END DO
    !
    !APPLY ANY FMP SW ALLOTMENTS
    !
    IF(ALLOT%HAS_SW_ALLOTMENT) THEN
        FARMALLOT = DELT/PERLEN(KPER)
        DO CONCURRENT (NF=1:WBS%NFARM)
            IF(ALLOT%SW_ALLOTMENT(NF) < D100) THEN
                                              SWODAT%FMP_SW_ALLOTMENT(NF) = ALLOT%SW_ALLOTMENT(NF)*FARMALLOT             ! FARMALLOT = DBLE(DELT)/DBLE(PERLEN(KPER))
            ELSE
                                              SWODAT%FMP_SW_ALLOTMENT(NF) = inf
            END IF
        END DO
    END IF
    !
    ! CALCULATE SURFACE WATER OPERATIONS RELEASES BASED ON DEMAND.  (SWO)
    N = -1  !PASSED AS ICNVG TO SWO_CONVERGENCE_CHECK. BY SETTING TO -1 PREVENTS OUTPUT 
    SITER = ONE
    !!!IF(SWODAT%INNER_ITERATION > ONE) THEN  -- Not yet implmented
    !!!  !
    !!!  DO SITER=1, SWODAT%INNER_ITERATION - 1
    !!!     !                                          KPER,KSTP,KITER,DELT,IDIVAR,STRM,SEG,SITER
    !!!     CALL SWODAT%COMPUTE_ALLOCATION_AND_RELEASE(KPER,KSTP,KITER,IDIVAR,STRM,SEG, SITER)
    !!!     !
    !!!    ERROR STOP 'SWO ERROR - INTERNAL_ITERATION > 1 IS NOT ALLOWED'
    !!!     !!!           CALL SWO_SOLVE_SFR_NETWORK(KITER,KPER,KSTP,IUNITLAK,IUNITNWT,
    !!!     !!!     +                                ILGR, LGRITER, NGRIDS, IGRID)
    !!!                !
    !!!                !CALL SFR_DELIV%SET_INFLOW(STRM)
    !!!                !!
    !!!                !DO CONCURRENT(F=1:WBS%NFARM, SWODAT%FMP_SW_DEMAND(NF) > DZ)
    !!!                !    !
    !!!                !    CALL SWFL%APPLY_SRD_DEMAND(NF,RDEL,SFR_DELIV)
    !!!                !END DO
    !!!                !
    !!!                CALL SWODAT%CONVERGENCE_CHECK(KPER,KSTP,KITER,STRM,SWFL%SRDLOC,N)
    !!!     !
    !!!  END DO
    !!!  !
    !!!  CALL SFR_DELIV%SET_INFLOW(STRM)  !UPDATE POTENTIAL FLOW FROM SFR DELIVERY POINTS AS A RESULT OF SWO
    !!!END IF
    !
    !
    CALL SWODAT%COMPUTE_ALLOCATION_AND_RELEASE(KPER,KSTP,KITER,IDIVAR,STRM,SEG, SITER)       ! FINAL SET OF RELEASES AFTER INNER, INNER ITERATIONS; SFR RUNOFF, STRM(12,:), is from previous iteration
    !
    END IF !WBS%HAS_SWO
    !
    IF(WBS%HAS_SFR) THEN !RESEST BACK TO ORIGINAL RUNOFF FROM SFR INPUT -> seb, moved to after SWO computation block
       !
       DO CONCURRENT (I=1:NSTRM, FMP_MOD_SFR_RUNOFF(I))
           !
           STRM(12,I) = SFR_RUNOFF(I)
           !
       END DO
    END IF
    !
    !
    !7===== START OF FARMS LOOP =================================================================================
    FARM_LOOP: DO NFF=1,WBS%NFARM
      !
      IF(.NOT.WBS%INUSE(NFF) .OR. IFA(NFF).EQ.0) CYCLE
      !
      NF=IFA(NFF)
      !
      TF=WBS%DEMAND(NF)
      QR=DZ
      !
      IF(ALLOT%HAS_SW_ALLOTMENT) THEN
          FARMALLOT=ALLOT%SW_ALLOTMENT(NF)/DBLE(PERLEN(KPER))
      ELSE
          FARMALLOT=inf
      END IF
      !
      !     
      !7E1----UPDATE NON-ROUTED DELIVERY
      !7E1A---UPDATE NON-ROUDED DELIVERY FOR NON-OPTIMIZATION CASES
      SUFDSUM=DZ
      SURPLUS1=DZ
      SURPLUS2=DZ
      ND=DZ                                                           !seb ALWAYS INITIALIZE TO ZERO
      IF(SWFL%HAS_NRD .AND. WBS%H2OSOURCE%NRD(NF)) THEN                  ! seb H2OSRC(1,:)=FID; H2OSRC(2,:)=GW; H2OSRC(3,:)=SW; H2OSRC(4,:)=NRD  
         !
         CALL SWFL%NRD(NF)%CONSUME(TF, ND)
         !
         !     FOR LGR, SCALE RANKED NON-ROUTED DELIVERIES TO PARENT OR CHILD MODEL FARM
         !     BY RATIO BETWEEN PARENT OR CHILD FARM DEMAND AND JOINT PARENT+CHILD DEMAND
         IF(ILGR.NE.0) THEN
           IF(LGRITER.EQ.2.AND.KITER.EQ.1) THEN
              !
              !     SCALE RANKED NON-ROUTED DELIVERIES TO PARENT MODEL FARM BY RATIO BETWEEN
              !     PARENT FARM DEMAND AND JOINT PARENT+CHILD DEMAND
              IF(IGRID.EQ.1) THEN                              ! SCOTT
                 do n=2,ngrids
                     if(N.EQ.LGRDAT(N)%IFMPGRID) then
                        IF(FMPDAT(N)%SWFL%MXNRD.EQ.-1) THEN
                           DO IT=1,SWFL%MXNRD   
                               IF(TF+FMPDAT(N)%WBS%DEMAND(NF).GT.0) THEN
                                   IF(KSTP.EQ.1) RNRD(1,SWFL%MXNRD+IT,NF)=RNRD(1,IT,NF)
                                   RNRD(1,IT,NF) = RNRD(1,SWFL%MXNRD+IT,NF)*TF/(TF+FMPDAT(N)%WBS%DEMAND(NF))
                               ENDIF
                           ENDDO
                        ENDIF
                     ENDIF
                 ENDDO
              ELSE
                 !     SCALE RANKED NON-ROUTED DELIVERIES TO CHILD MODEL FARM BY RATIO BETWEEN
                 !     CHILD FARM DEMAND AND JOINT PARENT+CHILD DEMAND AND
                 !     ADOPT NRD-USE FLAGS FROM PARENT MODEL NRD DATA INPUT.
                 IF(SWFL%MXNRD.EQ.-1) THEN
                     DO IT=1,SWFL%MXNRD   
                        IF(FMPDAT(1)%WBS%DEMAND(NF)+TF.GT.0) THEN
                          IF(KSTP.EQ.1) THEN
                            RNRD(1,SWFL%MXNRD+IT,NF)=FMPDAT(1)%RNRD(1,SWFL%MXNRD+IT,NF)
                          ENDIF
                          RNRD(1,IT,NF) = RNRD(1,SWFL%MXNRD+IT,NF)*TF/(TF+FMPDAT(1)%WBS%DEMAND(NF))
                          RNRD(2,IT,NF) = FMPDAT(1)%RNRD(2,IT,NF)
                          UNRD(3*IT,NF) = FMPDAT(1)%UNRD(3*IT,NF)
                        ENDIF
                     ENDDO
                 ENDIF        
              ENDIF     
           ENDIF   !IF(LGRITER.EQ.2.AND.KITER.EQ.1)
         ENDIF     !IF(ILGR.NE.0)
         !
         DO IT=1,SWFL%MXNRD
                          INRDR(IT)=IDINT(UNRD(3*IT,NF))     !define nrd-ranks as array to find maximum rank for current farm
                          INRDU(IT)=IDINT(RNRD(2,IT,NF))     !define nrd-use flags for a ranked array of nrd-types for current farm
         ENDDO
         ITTOT=MAXVAL(INRDR)                                            !find maximum rank for current farm (could be < max. number of nrd-types MXNRDT)
         !
         DSUM=DZ
         DO IRT=1,ITTOT
            DSUM=DSUM+RNRD(1,IRT,NF)
            ITU=ITTOT-IRT                                               !check for delivery types unnecessary to meet TFDR (ITU) in sequence of ranking 
            IF(TF-DSUM.LE.DZ) EXIT
         ENDDO
         IF(TF.LT.FPS) ITU=ITTOT
         !
         ND=DZ
         SUFDSUM=DZ
         SURPLUS1=DZ
         SURPLUS2=DZ
         DO IRT=1,ITTOT
            IF(TF.GT.DSUM) THEN
                ND=DSUM                                                 !non-routed delivery can't be greater than the sum of deliveries
            ELSE
               IF(IRT.LT.ITTOT-ITU)  SUFDSUM=SUFDSUM+RNRD(1,IRT,NF)                           !sum of deliveries that are used in their entirety to satisfy TF
               !
               IF(IRT.EQ.ITTOT-ITU) THEN
                  IF(INRDU(IRT).EQ.0) THEN
                                               ND=TF                                                 !as long as the current Ranked Type IRT exceeds TF, only part of IRT will be used & the total ND is equal to TF
                  ELSEIF(INRDU(IRT).GT.0) THEN
                                               ND=SUFDSUM+RNRD(1,IRT,NF)                             !if not only the sufficient but the abolute amount of IRT is to be used ...
                                               IF(INRDU(IRT).EQ.1) SURPLUS1=ND-TF                  !gather excess surplus component desired to be recharged into the canal
                                               IF(INRDU(IRT).EQ.2) SURPLUS2=ND-TF                  !gather excess surplus component desired to be injected into farm wells
                  ENDIF
               ENDIF
               !
               IF(IRT.GT.ITTOT-ITU.AND.INRDU(IRT).GT.0) THEN
                                               ND=ND+RNRD(1,IRT,NF)                                     !if IRT tyes are unnecessary, but nontheless specified to be used in their absolute amounts
                                               IF(INRDU(IRT).EQ.1) SURPLUS1=SURPLUS1+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be recharged into the canal
                                               IF(INRDU(IRT).EQ.2) SURPLUS2=SURPLUS2+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be injected into farm wells
               ENDIF
            ENDIF
         ENDDO
         !
         ! CALCAULATE TOTAL NRD AVAILIBLE
         ! SET UP THE NRD SUPPLY
         DSUM=DZ
         DO IRT=1,ITTOT
            DSUM=DSUM+RNRD(1,IRT,NF)
         ENDDO
         !
         CALL WBS%SUPPLY(NF)%SET_NRD_TOT(DSUM)
         CALL WBS%SUPPLY(NF)%SET_NRD(ND)
         !
      END IF   ! SWFL%HAS_NRD  .AND. WBS%H2OSOURCE%NRD(NF)
      !
      !7E2----UPDATE SW DELIVERY FROM HEAD-GATE REACH (ROUTED OR SEMI-ROUTED), AND FARM WELL DISCHARGE
      !
      !
      IF(WBS%H2OSOURCE%SW(NF)) THEN
         !
         RDEL=TF-ND-QR
         CALL SWFL%SET_DEMAND_INI(NF, RDEL)  ! Note negative demand is set to zero
         !
         IF(ND.LE.TF) THEN
             !
             IF(ALLOT%HAS_SW_ALLOTMENT) THEN
                 IF(RDEL > ALLOT%SW_RATE_LIM(NF)) THEN
                    RDEL = ALLOT%SW_RATE_LIM(NF)
                 END IF
             END IF
             !
             IF(WBS%HAS_SWO)THEN
                            IF(RDEL > SWODAT%FMP_SW_LIMIT(NF))  RDEL=SWODAT%FMP_SW_LIMIT(NF)
             END IF
             !
             CALL SWFL%APPLY_SRD_DEMAND(NF,RDEL,SFR_DELIV)
             RDEL = SWFL%SRDLOC(NF)%TOT_DMD_MET
         ELSE
             RDEL=-SURPLUS1       !recharge cumulative surplus of nrd (if desired to be sent to SW) into canal
             CALL SWFL%APPLY_SRD_SURPLUS(NF,SURPLUS1,STRM)
         END IF
         !
      ELSE
         RDEL = DZ
         CALL SWFL%SET_DEMAND_INI(NF, RDEL) 
      END IF
      !
      CALL WBS%SUPPLY(NF)%SET_SFR(SWFL%SRDLOC(NF)%TOT_DMD_MET)
      !
      !7E2B---DETERMINE PUMPING REQUIREMENT FOR "SW-DOMINATED" CASE
      !       PUMPING REQUIREMENT (QR) =
      !       TOTAL FARM DELIVERY REQUIREMENT MINUS ACUTAL FARM DELIVERY FROM SURFACE WATER!
      IF(ND.LE.TF) THEN
                       QR=TF-ND-SWFL%SRDLOC(NF)%TOT_DMD_MET
      ELSEIF(SURPLUS2 > NEARZERO_29) THEN
                       QR=-SURPLUS2                                   !inject cumulative surplus of nrd (desired to be sent to GW) into farm wells
      ELSE
                       QR = DZ
      END IF
      !
      !
      !7E5----SAVE BUDGET TERMS
      !
      !7E5A---SAVE BUDGET TERMS IN GENERAL IN ODER TO BE AVAILABLE IN FMP3WELBD
      !       In the regular case, budget terms are saved at each iteration. In case of deficiency,
      !       "pre-deficiency" terms are saved during the deficiency scenario execution (C7E6B,C7E7G)  
      !       under a different name in order to be still available in FMP3WELBD along with the final terms.
      NRD(1,NF)=ND
      WBS%Q_DEMAND(NF)=QR
      !
      !7E5B---SAVE BUDGET TERMS AS "PRE-DEFICIENCY" SCENARIO TERMS:
      !       For prior appropriation, the "to be optimized" "pre-deficiency" terms need to be corrected 
      !       for small numerical inaccuracies after the PIOR routine was carried out for all farms and
      !       after the farms loop is completed. That is, these terms need to be gathered at the end of 
      !       the farms loop while deficieny scenarios are not yet executed.
      !
      !7E5C---SAVE UNSATISFIED TOLERANCE OF PUMPING REQUIREMENT
      !
      QTOL = QR - FWELL(NF)%QMAX !
      !
      ! SET UP SUPPLY VARIABLES
      !
      IF(QTOL.GT.FPS .AND. QR.GT.FPS) THEN
         CALL WBS%SUPPLY(NF)%SET_WEL(FWELL(NF)%QMAX)
      ELSEIF(QR.GT.FPS) THEN
          CALL WBS%SUPPLY(NF)%SET_WEL(QR)
      ELSE
          QR=DZ
          WBS%Q_DEMAND(NF)=QR
          CALL WBS%SUPPLY(NF)%SET_WEL(QR)
      END IF
      !
      IF(WBS%DEFICIENCY%LIST(NF).EQ.0) THEN
            CALL WBS%SUPPLY(NF)%SET_MAGIC(TF,WBS%DEFICIENCY%LIST(NF))
      END IF
      !
      CALL WBS%SUPPLY(NF)%SET_USED()
      CALL WBS%SUPPLY(NF)%SET_TOTAL()
      !
      !
      IF(WBS%DEFICIENCY%LIST(NF) == ONE) THEN  !DEFICIT IRRIGATION SO NO MAGIC WATER PICKS UP THE SLACK, NO NEED FOR DEFICITY CALCULATIONS
         !
         ! TOTAL SUPPLY REQUIRED FOR DEFICIENCY SCENARIO
         !
         IF(QTOL.GT.FPS .AND. WBS%Q_DEMAND(NF).GT.FPS) THEN
             !!!AFDELSW=TF-QR-ND
             !!!IF(AFDELSW < NEARZERO_30) AFDELSW = DZ
             !
             CALL FCROP%APPLY_DEFICIENCY_SCENARIO(WBS, NF)
             !
             TF = WBS%CROP_DEMAND(NF)  !UPDATE DEMAND
             !
             QR=TF-ND-SWFL%SRDLOC(NF)%TOT_DMD_MET
             !
             !             store original pumping requirement before updates after acreage-optimization are done
             IF(.NOT. ( FMPOPT%WELLFIELD .AND. (KITER.LE.1.OR.ISTARTFL.LE.1))) THEN 
                IF(NRD(2,NF) .EQ. DZ) NRD(2,NF)=NRD(1,NF)
                IF(TFDROLD(NF) .EQ. DZ) TFDROLD(NF)=WBS%DEMAND(NF)
             END IF
             !
             WBS%Q_DEMAND(NF)=QR     
             WBS%DEMAND(NF) = TF
         END IF
      END IF
      !
   END DO FARM_LOOP !7===== END OF FARMS LOOP =================================================================================
   !
   !
   !       
   ! APPLY TO SFR ANY WATER REMVOED FROM NETWORK AS NEGATIVE RUNOFF.
   IF(WBS%HAS_SFR) CALL SFR_DELIV%APPLY_TO_SFR(STRM)
   !
   !       ======================================================================
   !9===== DETERMINE NET-RECHARGE FOR EACH FARM CELL, ADD IT TO RHS AND HCOF, AND ===============================
   !       DETERMINE SURFACE-WATER RUNOFF FOR EACH FARM CELL:
   !       ======================================================================
   !
   WBS%DPERC  = DZ  !THESE WILL BE POPULATED
   WBS%RUNOFF = DZ 
   CALL FCROP%CALC_INEFFICIENT_LOSSES(WBS, SOIL%SURF_VK)           !POPULATES  WBS%DPERC  and   WBS%RUNOFF(NCOL,NROW)
   !
   ! CHECK IF RUNOFF IS DISABLED
   !
   IF(SWFL%NORETURNFLOW) THEN                                !NO RUNOFF ALLOWED
       !
       DO CONCURRENT ( IC=1:NCOL, IR=1:NROW, WBS%RUNOFF(IC,IR) > DZ)
           !
           WBS%DPERC (IC,IR) = WBS%DPERC(IC,IR) + WBS%RUNOFF(IC,IR)
           WBS%RUNOFF(IC,IR) = DZ
           !
       END DO 
       !
   ELSEIF( SWFL%NORETURNFLOW_TFR_FLAG == 1 ) THEN  ! True if: ALL( SWFL%NORETURNFLOW_TFR%LIST == 1 )
                                        !
                                        DO CONCURRENT ( IC=1:NCOL, IR=1:NROW, WBS%RUNOFF(IC,IR) > DZ)
                                            !
                                            WBS%DPERC (IC,IR) = WBS%DPERC(IC,IR) + WBS%RUNOFF(IC,IR)
                                            WBS%RUNOFF(IC,IR) = DZ
                                            !
                                        END DO 
   ELSEIF( SWFL%NORETURNFLOW_TFR_FLAG == 2 ) THEN  ! True if: ANY( SWFL%NORETURNFLOW_TFR%LIST == 1 )
                                        !
                                        DO NF=1, WBS%NFARM
                                        IF(SWFL%NORETURNFLOW_TFR%LIST(NF)==1) THEN
                                            !
                                            DO I=1, WBS%FID(NF)%Count
                                              IR=WBS%FID(NF)%RC(1,I)
                                              IC=WBS%FID(NF)%RC(2,I)
                                              !
                                              IF(WBS%RUNOFF(IC,IR) > DZ) THEN
                                                !
                                                WBS%DPERC (IC,IR) = WBS%DPERC(IC,IR) + WBS%RUNOFF(IC,IR)
                                                WBS%RUNOFF(IC,IR) = DZ
                                                !
                                              END IF
                                            END DO
                                        END IF
                                        END DO
   END IF
   !
   IF(CLIMATE%HAS_RECHARGE) THEN
            CALL CLIMATE%ADD_DIRECT_RECHARGE(WBS%DPERC)                              ! ADDS ANY DIRECT RECHARGE TO DPERC
            CALL WBS%SUM_WBS_DIRECT_RECHARGE(CLIMATE%NDRCH, CLIMATE%DIRECT_RECHARGE) ! ACCUMULATE FARM BASED DIRECT RECHARGE
   END IF
   !
   !WBS%FNRCH = DZ !WBS%FNRCH - CDAT%TGWA - CDAT%EGWA
   !
   CALL WBS%SUM_WBS_RUNOFF_DPERC()                  !POPULATES  WBS%TOT_DPERC  and   WBS%TOT_RUNOFF(NFARM)   --CALLED IN BD ROUTINE
   !
   ! RESET UZF ARRAYS BECAUSE FMP HANDELS INFILTRATION
   !
   IF(WBS%UZF_LINK) THEN
       IF(CLIMATE%HAS_RECHARGE) THEN  !ENTIRE ARRAY OVER WRITTEN BY DIRECT RECHARGE
                         FINF   =0.0  !INFILTRATION WILL BE OVER WRITTEN BY DIRECT_RECAHRGE
                         EXCESPP=0.0
       ELSE
            WHERE ( WBS%FID_ARRAY > Z )
                         FINF   =0.0  
                         EXCESPP=0.0
            END WHERE
       END IF
   END IF
   !
   ! ADD DEEP PERCOLATION TO RECHARGE
   !
   IF(WBS%UZF_LINK) THEN
       !DO CONCURRENT (IR=1:FDIM%NROW, IC=1:FDIM%NCOL,      WBS%DPERC(IC,IR)>0.D0  &
       !                                              .AND. IUZFBND(IC,IR)<1       &
       !                                              .AND. UPLAY(IC,IR)>0         )
       DO IR=1, FDIM%NROW
       DO IC=1, FDIM%NCOL
       IF(      WBS%DPERC(IC,IR)>DZ  &
          .AND. IUZFBND(IC,IR)<1       &
          .AND. UPLAY(IC,IR)>0         ) THEN
                IL = UPLAY(IC,IR)
                RHS(IC,IR,IL) = RHS(IC,IR,IL) - WBS%DPERC(IC,IR)   !NEGATIVE ADDS DPERC TO GW
       END IF
       END DO
       END DO
   ELSE
       !DO CONCURRENT (IR=1:FDIM%NROW, IC=1:FDIM%NCOL,       WBS%DPERC(IC,IR)>0.D0  &
       !                                               .AND. UPLAY(IC,IR)>0         )
       DO IR=1, FDIM%NROW
       DO IC=1, FDIM%NCOL
       IF(      WBS%DPERC(IC,IR)>DZ  &
          .AND. UPLAY(IC,IR)>0         ) THEN
                IL = UPLAY(IC,IR)
                RHS(IC,IR,IL) = RHS(IC,IR,IL) - WBS%DPERC(IC,IR)   !NEGATIVE ADDS DPERC TO GW
       END IF
       END DO
       END DO
   END IF
   ! DO and DO CONCURRENT fails on O3 optimization, cannot figure out compiler directives to work except for separate sub
   !DIR$ NOINLINE
   IF(WBS%UZF_LINK) CALL UZF_LOOPS(NROW, NCOL, WBS%DPERC, WBS%AREA, IUZFBND, VKS, FINF, EXCESPP)
   !IF(WBS%UZF_LINK) THEN
   !    DO CONCURRENT (IR=1:NROW, IC=1:NCOL, IUZFBND(IC,IR)>0 .AND. WBS%DPERC(IC,IR)>0.D0 )
   !        !             !
   !        FINF(IC,IR)=SNGL(WBS%DPERC(IC,IR)/WBS%AREA(IC,IR)) !(DELR(IC)*DELC(IR))
   !        !
   !        IF(FINF(IC,IR).GT.VKS(IC,IR)) THEN
   !                                      EXCESPP(IC,IR) = (FINF(IC,IR)-VKS(IC,IR))*WBS%AREA(IC,IR)!(DELR(IC)*DELC(IR))
   !                                      FINF(IC,IR)    = VKS(IC,IR)
   !        ELSE
   !                                      EXCESPP(IC,IR)=0. 
   !        ENDIF
   !    END DO
   !END IF
   !
   ! WBS%FNRCH IS USED TO HOLD HCOF
   !
   WBS%FNRCH = DZ
   !
   ! ADD CROP TRANSPIRATION TO RHS AND HCOF
   DO I=ONE, FCROP%NCROP
      !
      ASSOCIATE( RC=>FCROP%CROP(I)%RC, GW_INTER=>FCROP%CROP(I)%GW_INTER, &
                THI=>FCROP%CROP(I)%THI,     THD=>FCROP%CROP(I)%THD,      &
                DIM=>FCROP%CROP(I)%N                                      )
                !
                !!! DO CONCURRENT ( K=ONE:DIM, GW_INTER(K)>ONE 
                !!!+                          .AND. (THI(K).NE.DZ .OR. THD(K).NE.DZ) )
                !!!   IR = RC(ONE,K);  IC = RC(TWO,K)
                !!!   IL = UPLAY(IC,IR)
                !!!   !
                !!!   IF (IL>0) THEN
                !!!        !
                !!!    WBS%FNRCH(IC,IR) =  WBS%FNRCH(IC,IR) + THD(K)
                !!!    !
                !!!    RHS( IC,IR,IL) = RHS( IC,IR,IL) + THI(K)
                !!!    !
                !!!   END IF
                !!! END DO
                !
                DO K=ONE, DIM         
                IF(THI(K) /= DZ) THEN                  ! THI(K) == DZ if UPLAY(IC,IR) == 0 cause there is no transpiration
                  IR = RC(ONE,K);  IC = RC(TWO,K)
                  IL = UPLAY(IC,IR)
                  !
                  RHS(IC,IR,IL) = RHS(IC,IR,IL) + THI(K)
                END IF
                END DO
                !
                DO K=ONE, DIM
                IF(THD(K) /= DZ ) THEN                  ! THD(K) == DZ if UPLAY(IC,IR) == 0 cause there is no transpiration
                  !
                  WBS%FNRCH(RC(2,K),RC(1,K)) = WBS%FNRCH(RC(2,K),RC(1,K)) + THD(K)   ! WBS%FNRCH IS USED TO HOLD HCOF
                END IF
                END DO
      END ASSOCIATE
   END DO
   !!!      DO CONCURRENT ( I=ONE:FCROP%NCROP, K=ONE:FCROP%CROP(I)%N,
   !!!     +                                   FCROP%CROP(I)%GW_INTER(K)>ONE )
   !!!         IR = FCROP%CROP(I)%RC(1,K)
   !!!         IC = FCROP%CROP(I)%RC(2,K)
   !!!         IL = UPLAY(IC,IR)
   !!!         !
   !!!         IF (IL>0) THEN
   !!!           RHS(IC,IR,IL) = RHS(IC,IR,IL) + FCROP%CROP(I)%THI(K)
   !!!           !
   !!!           IF( HCOF(IC,IR,IL) + FCROP%CROP(I)%THD(K)*HNEW(IC,IR,IL) 
   !!!     +                                                       > DNEG)THEN        !THERE IS A CHANCE THAT THD IS POSITIVE WHICH WILL WILL NO LONGER MAKE HCOF DIAGONALLY DOMINATE. MOVE TO RHS TO ALLOW SOLVER TO RUN FASTER/CONVERGE  -- DNEG ARBRTRARILY CHOOSEN AS LIMIT TO PREVENT NEARZERO HCOF
   !!!               !
   !!!               HH = RELAXER(HNEW(IC,IR,IL),HNEW_OLD(IC,IR,IL),
   !!!     +                                                      FCROP%RELAX)
   !!!               RHS(IC,IR,IL)  = RHS(IC,IR,IL) 
   !!!     +                        - FCROP%CROP(I)%THD(K)*HH    !MOVE TO OTHER SIDE (T = THI - THD*H)
   !!!           ELSE
   !!!               HCOF(IC,IR,IL) = HCOF(IC,IR,IL)
   !!!     +                        + FCROP%CROP(I)%THD(K)
   !!!           END IF
   !!!           !
   !!!         END IF
   !!!      END DO
   !
   ! ADD EVAPORATION OCCURING OVER CROPPED AREA TO RHS AND HCOF
   DO I=ONE, FCROP%NCROP
      ASSOCIATE( RC=>FCROP%CROP(I)%RC,   DIM=>FCROP%CROP(I)%N, &
                EHI=>FCROP%CROP(I)%EHI,  EHD=>FCROP%CROP(I)%EHD )
                !
                DO K=ONE, DIM
                IF(EHI(K) /= DZ) THEN  ! EHI(K) == DZ if UPLAY(IC,IR) == 0 cause there is no transpiration
                  IR = RC(1,K)
                  IC = RC(2,K)
                  IL = UPLAY(IC,IR)
                  !
                  RHS(IC,IR,IL)  = RHS(IC,IR,IL)  + EHI(K)
                END IF
                END DO
                !
                DO K=ONE, DIM
                IF(EHD(K) /= DZ) THEN   ! EHD(K) == DZ if UPLAY(IC,IR) == 0 cause there is no transpiration
                  !
                  WBS%FNRCH(RC(2,K),RC(1,K)) = WBS%FNRCH(RC(2,K),RC(1,K)) + EHD(K)  !WBS%FNRCH HOLDING HCOF
                END IF
                END DO
      END ASSOCIATE
   END DO
   !
   ! ADD EVAPORATION OCCURING OVER BAREN/FALLOW AREA TO RHS AND HCOF
   IF(FCROP%CHECK_BARE) THEN
                        !!!        DO CONCURRENT ( IC=1:NCOL, IR=1:NROW, UPLAY(IC,IR) > 0 
                        !!!     +                        .AND. FCROP%BARE_FRAC(IC,IR) > DZ)
                        !!!           !
                        !!!           RHS(IC,IR,UPLAY(IC,IR))   = RHS(IC,IR,UPLAY(IC,IR))
                        !!!     +                               + FCROP%BARE_EVAP_EHI(IC,IR)
                        !!!           !
                        !!!!           HCOF(IC,IR,UPLAY(IC,IR)) = HCOF(IC,IR,UPLAY(IC,IR))
                        !!!!     +                               + FCROP%BARE_EVAP_EHD(IC,IR)
                        !!!           WBS%FNRCH(IC,IR) =  WBS%FNRCH(IC,IR) 
                        !!!     +                               + FCROP%BARE_EVAP_EHD(IC,IR)
                        !!!        END DO
                        !
                        DO IC=1, NCOL
                        DO IR=1, NROW
                        IF(FCROP%BARE_EVAP_EHI(IC,IR) /= DZ) THEN
                           IL = UPLAY(IC,IR)
                           !
                           RHS(IC,IR,IL)   = RHS(IC,IR,IL) + FCROP%BARE_EVAP_EHI(IC,IR)
                        END IF
                        END DO
                        END DO
                        !
                        DO IC=1, NCOL
                        DO IR=1, NROW
                        IF(FCROP%BARE_EVAP_EHD(IC,IR) /= DZ) THEN
                           WBS%FNRCH(IC,IR) =  WBS%FNRCH(IC,IR) + FCROP%BARE_EVAP_EHD(IC,IR) ! WBS%FNRCH IS USED TO HOLD HCOF
                        END IF
                        END DO
                        END DO
   END IF
   !
   DO IR=1, NROW
   DO IC=1, NCOL
   IL = UPLAY(IC,IR)
   IF(IL > 0) THEN ! WBS%FNRCH IS USED TO HOLD HCOF
                   IF(WBS%FNRCH(IC,IR) < NEGNEARZERO_30 .OR. NEARZERO_30 < WBS%FNRCH(IC,IR)) THEN
                       !
                       IF    ( DZ < WBS%FNRCH(IC,IR) .OR. FDIM%NO_HCOF) THEN   ! -> Positive values can cause instability, so shift HCOF back to RHS
                             !
                             RHS(IC,IR,IL)  = RHS(IC,IR,IL) - WBS%FNRCH(IC,IR)*WTABLE(IC,IR)
                       ELSE 
                             HCOF(IC,IR,IL) = HCOF(IC,IR,IL) + WBS%FNRCH(IC,IR)
                       END IF
                   END IF
   END IF
   END DO
   END DO
   !
   !10==== DETERMINE SURFACE-WATER RUNOFF FOR EACH FARM: =======================================================
   !       =============================================
   !
   !10A----SUM UP ALL SWRUNOFF FOR EACH FARM AND ROUT IT TO THE RESPECTIVE FARM-DRAIN
   !       (if SFR is not linked to FMP, surface-water runoff is still calculated,
   !        but does not impact the GW-budget in any way.)
   !
   ! seb ADD CONENCTION OF DRT TO FMP BY SENDING DRAIN RETURN FLOW TO FARM RUN OFF.
   IF ( WBS%HAS_DRT)THEN
         DO CONCURRENT (NF=1:WBS%NFARM,DRTFLOW(NF)%HAS_FLO)
             !
             WBS%TOT_RUNOFF(NF)=WBS%TOT_RUNOFF(NF)+DRTFLOW(NF)%FLO !seb ADD DRTFLOW TO ALLOW ADDED FLOWS FROM DRT
             !
         END DO
   END IF
   !
   IF(WBS%HAS_SFR) THEN
       CALL SWFL%APPLY_RUNOFF_TO_SFR(WBS%TOT_RUNOFF, STRM)
   ELSE
       CALL SWFL%APPLY_RUNOFF_TO_SFR(WBS%TOT_RUNOFF)
   END IF
   !
   !13==== ADD FARM WELL DISCHARGE TO RHS: =============================================================================
   !
   !13B----NOCIRNOQ OPTION: SET QMAX TO ZERO FOR WELLS WHOSE CELLS HAVE A ZERO CROP IRRIGATION REQUIREMENT
   !
   IF(WBS%HAS_WELL) THEN
          DO NF=1, WBS%NFARM
          IF(FWELL(NF)%NOCIRNOQ) CALL FWELL(NF)%AUX_NOCIRNOQ( WBS%CROP_DEMAND_ARRAY, FALSE )
          END DO
          !
          !14A----CALCULATE PUMPING REQUIRED TO MEET DEMAND
          !
          CALL FWELL%CALCULATE_Q( WBS%Q_DEMAND )
          !
          !14A----APPLY TO RHS OR MNW2 THE CALCULATE PUMPING REQUIRED TO MEET DEMAND
          !          
          CALL FWELL%APPLY_Q()
          !
          ! STORE TOTAL WELL PUMPAGE USED
          !CALL WBS%SUPPLY%SET_WEL(QREQ)
   END IF
   !
  END SUBROUTINE
  !
  PURE SUBROUTINE UZF_LOOPS(NROW, NCOL, DPERC, AREA, IUZFBND, VKS, FINF, EXCESPP)
    USE CONSTANTS, ONLY: DZ
    INTEGER,                                INTENT(IN   ):: NROW, NCOL
    INTEGER,          DIMENSION(NCOL,NROW), INTENT(IN   ):: IUZFBND
    DOUBLE PRECISION, DIMENSION(NCOL,NROW), INTENT(IN   ):: DPERC, AREA
    REAL,             DIMENSION(NCOL,NROW), INTENT(IN   ):: VKS
    REAL,             DIMENSION(NCOL,NROW), INTENT(INOUT):: FINF, EXCESPP
    INTEGER:: IR, IC
    !
    DO IR=1, NROW
    DO IC=1, NCOL
        IF( IUZFBND(IC,IR)>0 .AND. DPERC(IC,IR)>DZ ) THEN
          !             !
          FINF(IC,IR)=SNGL(DPERC(IC,IR)/AREA(IC,IR)) !(DELR(IC)*DELC(IR))
          !
          IF(FINF(IC,IR).GT.VKS(IC,IR)) THEN
                                        EXCESPP(IC,IR) = (FINF(IC,IR)-VKS(IC,IR))*AREA(IC,IR)!(DELR(IC)*DELC(IR))
                                        FINF(IC,IR)    = VKS(IC,IR)
          ELSE
                                        EXCESPP(IC,IR)=0. 
          ENDIF
        END IF
    END DO
    END DO
  END SUBROUTINE
  !
  SUBROUTINE FMP_CNVG(IGRID, KPER, KSTP, KITER, ICNVG)
    USE FMP_GLOBAL,    ONLY: SWFL, WBS, FCROP, ISTARTFL, FMP_LGR_PNT, SWODAT
    USE GWFSFRMODULE, ONLY: STRM
    USE CONSTANTS,    ONLY: BLN, NL, ONE, Z, DZ, HALF, UNO, MILLI,NEARZERO_5
    !
    INTEGER, INTENT(IN   ):: IGRID, KPER, KSTP, KITER
    INTEGER, INTENT(INOUT):: ICNVG
    INTEGER::F,K
    !
    CALL FMP_LGR_PNT(IGRID)
    CALL SGWF2LGR2PNT(IGRID)  
    CALL SGWF2SFR7PNT(IGRID)  !necessary as FMP is called before SFR in FM loop
    !
    IF(ICNVG == 1) THEN
       !IF(    FCROP%RELAX <= 0.3D0) THEN
       !       ICNVG = 0 
       !       FCROP%RELAX  = 0.5D0
       IF(FCROP%RELAX <= HALF) THEN
              ICNVG = Z 
              FCROP%RELAX  = 0.8D0
       ELSEIF(FCROP%RELAX <  0.999D0) THEN
           ICNVG = Z 
           FCROP%RELAX = UNO
       END IF
    END IF
    !
    ! If FMP is active, 
    ! ISTARTFL   allows one initial iteration (ISTARTFL=0) prior to FMP-FM
    !            ... only solves after that initial iteration has converged
    IF(ISTARTFL < 1) ICNVG = Z
    IF(KITER    < 7) ICNVG = Z                    !ENSURE AT LEAST SIX ITERATIONS
    IF(KITER    < 13 .AND. WBS%HAS_SWO) ICNVG = Z !SWO REQUIRES A MIN OF 12 OUTER ITERACTIONS
    !
    IF(KITER    > 13 .AND. WBS%HAS_SWO) THEN      ! Required flow cnvg check
      !
      IF( KITER < 21 .AND. SWODAT%REQFLOW%HAS_REQ) THEN
          !
          IF( ANY( SWODAT%REQFLOW%REQ(:) > NEARZERO_5) ) ICNVG = Z
      END IF
    END IF
    !
    IF(SWFL%NSFR_DELIV > Z) THEN !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          IF(KITER == 7) SWFL%SRD_TOL_CNT = Z
          !
          IF(ICNVG==1) THEN
             ASSOCIATE(SW=>SWFL%SRDLOC)
                F = Z
                DO WHILE ( ICNVG==1 .AND. F < SWFL%NFARM )
                    !
                    F = F + ONE
                    !
                    DO K=1, SW(F)%N
                       !
                       IF(    SW(F)%FLOW_OLD(K) > SW(F)%FLOW(K) .AND.  SW(F)%FLOW_OLD(K) > MILLI)   THEN
                                !
                           IF( (SW(F)%FLOW_OLD(K)-SW(F)%FLOW(K))/SW(F)%FLOW_OLD(K) > SWFL%SRD_TOL ) THEN
                                 ICNVG = Z
                                 SWFL%SRD_TOL_CNT = SWFL%SRD_TOL_CNT + ONE
                           END IF
                           !
                       ELSEIF(SW(F)%FLOW(K)     > SW(F)%FLOW_OLD(K) .AND. SW(F)%FLOW(K)   > MILLI)   THEN
                                !
                                IF( (SW(F)%FLOW(K)-SW(F)%FLOW_OLD(K))/SW(F)%FLOW(K) > SWFL%SRD_TOL ) THEN
                                      ICNVG = Z
                                      SWFL%SRD_TOL_CNT = SWFL%SRD_TOL_CNT + ONE
                                END IF
                           !
                       END IF
                       !
                       IF(ICNVG == Z) EXIT 
                    END DO
                END DO
             END ASSOCIATE
          END IF
          !
          IF( SWFL%SRD_TOL_CNT == 3) THEN             !Only write warning once
              SWFL%SRD_TOL_CNT = SWFL%SRD_TOL_CNT + ONE
              !
           CALL WARNING_MESSAGE(OUTPUT=WBS%IOUT,MSG=                                                                                &
                                 'The number of solver iterations was increased due to the'//NL//                                        &
                                 'semi-routed deliveries changing between solver iterations.'//BLN//                                     &
                                 'FMP requires that the relative difference between the'//NL//                                           &
                                 'semi-routed deliveries from the previous solver iteration and the current'//NL//                       &
                                 'be less than '//NUM2STR(SWFL%SRD_TOL)//', even if the solver criteria is met.'//BLN//                  &
                                 'This warning was triggered because the solver was required to solve'//NL//                             &
                                 'at least 2 additional iterations before the semi-routed deliveries'//NL//                              &
                                 'were relatively the same between iterations.'//BLN//                                                   &
                                 'If it is desired to change this relative closure tolerance, add to the FMP SURFACE_WATER block'//NL//  &
                                 'the keyword SEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE followed by the new tolerance.'//BLN//              &
                                 '(Note the default is 0.02)')
          ENDIF
          !
          DO CONCURRENT   (F=1:SWFL%NFARM, SWFL%SRDLOC(F)%N > 0)
            DO CONCURRENT (K=1:SWFL%SRDLOC(F)%N)
                                               SWFL%SRDLOC(F)%FLOW_OLD(K) = SWFL%SRDLOC(F)%FLOW(K)
            END DO
          END DO
      END IF !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      IF(WBS%HAS_SWO) THEN
          !
          IF(KITER.LE.SWODAT%SWMXITER(3)) THEN 
                                          CALL SWODAT%CONVERGENCE_CHECK(KPER, KSTP, KITER, STRM,SWFL%SRDLOC,ICNVG)
          ELSEIF(ICNVG==1) THEN
                                          CALL WARNING_MESSAGE(OUTPUT=SWODAT%IOUT,MSG=                                                                                             &
                                                  'SWO WARNING: REACHED SWMXITER(3) ITERATION LIMIT BEFORE MODEL CONVERGED.'//NL//                                    &
                                                  'SWO DELIVERIES ASSUMED TO HAVE CONVERGED AFTER '//NUM2STR(SWODAT%SWMXITER(3))//' ITERATIONS [SWMXITER(3)].'//NL//  &
                                                  'THIS IS THE THIRD NUMBER AFTER THE KEYWORD "ITERATION_THRESHOLD" IN THE FMP-SWO BLOCK')
          END IF
      END IF
      !
  END SUBROUTINE
  !
  SUBROUTINE FMP_BD(KITER,KPER,KSTP,IGRID,NGRIDS,ILGR,LGRITER)
    USE GLOBAL,       ONLY: SPTIM, INPUT_CHECK,NCOL,NROW,NLAY
    USE GWFBASMODULE, ONLY: HAS_STARTDATE, DATE_SP, TOTIM
    USE FMP_GLOBAL,   ONLY: FDIM, WBS, CLIMATE, FCROP, FMPOUT,SALT,SOIL,SWFL,FMPOPT,FMP_LGR_PNT,DRTFLOW, SWODAT
    USE GWFSFRMODULE, ONLY: SEG, STRM
    USE GWFMNW2MODULE,ONLY: SGWF2MNW2PNT
    USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
    !
    INTEGER, INTENT(IN   ):: IGRID, KPER, KSTP, KITER
    INTEGER, INTENT(IN   ):: NGRIDS,ILGR,LGRITER
    !
    DOUBLE PRECISION:: DELT, TIM
    CHARACTER(10):: DATE
    CHARACTER(16):: TXT
    CHARACTER(19):: DATETIME
    INTEGER:: NF
    !
    INTERFACE
       SUBROUTINE UTILSAV2D(FL,COMPACT,BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,NROW,NLAY,DATE)
          ! ******************************************************************
          ! SAVE 1 LAYER ARRAY ON DISK
          ! ******************************************************************
          USE, INTRINSIC:: ISO_FORTRAN_ENV,    ONLY: REAL32, REAL64
          USE CONSTANTS,                       ONLY: ONE, Z, NEG
          USE GLOBAL,                          ONLY: BIN_REAL_KIND, RBUF
          USE UTIL_INTERFACE,                  ONLY: TO_SNGL, NOT_NEAR_ZERO
          USE NUM2STR_INTERFACE,               ONLY: NUM2STR
          USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
          IMPLICIT NONE
          !
          INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
          CLASS(GENERIC_OUTPUT_FILE),         INTENT(IN):: FL
          LOGICAL,                            INTENT(IN):: COMPACT
          REAL(REAL64), DIMENSION(NCOL,NROW), INTENT(IN):: BUF
          CHARACTER(16),                      INTENT(IN):: TEXT
          INTEGER,                            INTENT(IN):: KSTP,KPER
          REAL(REAL64),                       INTENT(IN):: PERTIM,TOTIM
          CHARACTER(10),                      INTENT(IN):: DATE
       END SUBROUTINE
    END INTERFACE
    !
    CALL FMP_LGR_PNT(IGRID)
    CALL SGWF2LGR2PNT(IGRID) 
    CALL SGWF2SFR7PNT(IGRID) 
    CALL SGWF2NWT1PNT(Igrid) 
    CALL SGWF2UPW1PNT(Igrid) 
    CALL SGWF2UZF1PNT(Igrid) 
    CALL SGWF2MNW2PNT(IGRID) 
    CALL SGWF2MNW1PNT(Igrid)
    !
    DELT = SPTIM(KPER)%DT(KSTP)
    !
    IF(HAS_STARTDATE) THEN  !SET SP STARTING DATE
        DATE     = DATE_SP(KPER)%TS(KSTP-1)%STR()
        DATETIME = DATE_SP(KPER)%TS(KSTP-1)%STR('T')
        TIM      = DATE_SP(KPER)%TS(KSTP  )%DYEAR
    ELSE
        DATE     ='   NaN'
        DATETIME ='   NaN'
        TIM      = TOTIM
    END IF
    !
    IF(FMPOPT%RECOMP_Q_BD) CALL FMP_FM(KITER,KPER,KSTP,IGRID,NGRIDS,ILGR,LGRITER,SPTIM(KPER)%DT(KSTP))
    !
    ! UPDATE HEAD DEPENDENT VECTORS (THEY MODIFIED HCOF SO THEY ARE OUT OF SYNC)
    IF(.NOT. FDIM%NO_HCOF) THEN
        !
        CALL FCROP%UPDATE_TWGA_EGWA_BARE_EVAP()
        !
        CALL WBS%SUM_WBS_RUNOFF_DPERC()                  !POPULATES  WBS%TOT_DPERC  and   WBS%TOT_RUNOFF(NFARM) --THIS REBUILDS TO ONLY CAPTURE THE RUNOFF FROM FMP AND NOT OTHER PACKAGES
        !
        IF ( WBS%HAS_DRT)THEN
              DO CONCURRENT (NF=1:WBS%NFARM,DRTFLOW(NF)%HAS_FLO)
                             WBS%TOT_RUNOFF(NF)=WBS%TOT_RUNOFF(NF)+DRTFLOW(NF)%FLO !seb ADD DRTFLOW TO ALLOW ADDED FLOWS FROM DRT
              END DO
        END IF
        !
        CALL FCROP%CALC_WBS_EFFICIENCY(WBS)              !POPULATES WBS%EFF
    END IF
    !
    CALL FCROP%CALC_TTOT_ETOT_TGWA_EGWA_TOTALS(WBS,INPUT_CHECK)   !POPULATES FCROP%TTOT, WBS%TTOT, FCROP%ETOT, WBS%ETOT, etc.
    !
    !      ...ALLOW CONVERGENCE CRITERIA FOR MNW WELL PUMPAGE LINKED TO FARM PROCESS
    !
    IF(WBS%HAS_MNW2 .AND. FMPOPT%HAS_MNWCLOSE)    CALL FMP4QCNVG() !FMP3QCNVG CALL ADDED BY SCHMID, added inuit for NWT by rth --seb removed passing MNW1/MNW2 IUNITS
    !
    CALL FMP3WELBD(KSTP,KPER,DELT,DATETIME,IGRID)!FMP3WELBD & FMP3FNRBD CALLS ADDED BY SCHMID, added inuit for NWT by rth
    !
    CALL FMP3FNRBD(KSTP,KPER,IGRID)
    !
    CALL FMP3ETPRT(KSTP,KPER,IGRID)
    !
    CALL SWFL%RUNOFF_LOST_CHECK()
    !
    IF(WBS%HAS_SWO) THEN
       ! Call SWO_FINALIZE_TIMESTEP to (1) update year-to-date values based on final budget, 
       !                               (2) write output
       CALL SWODAT%SWO_FINALIZE_TIMESTEP(KPER,KSTP,KITER,STRM,SEG) 
       !
    END IF
    !
    ! WRITE ANY ADDITIONAL OUTPUT-----------------------------------------------------------------------
    !
    !
    !  Crop Output
    !
    IF(FCROP%OUT_BYFARMCROP%IS_OPEN) CALL FCROP%PRINT_OUT_BYFARM_BYCROP(WBS, KPER, KSTP, DELT, TIM, DATETIME)
    IF(FCROP%OUT_BYFARM    %IS_OPEN) CALL FCROP%PRINT_OUT_BYFARM       (WBS, KPER, KSTP, DELT, TIM, DATETIME)
    IF(FCROP%OUT_BYCROP    %IS_OPEN) CALL FCROP%PRINT_OUT_BYCROP       (WBS, KPER, KSTP, DELT, TIM, DATETIME)
    IF(FCROP%OUT_ALL       %IS_OPEN) CALL FCROP%PRINT_OUT_ALL_CROP     (WBS, KPER, KSTP, DELT, TIM, DATETIME)
    IF(FCROP%OUT_DETAIL    %IS_OPEN) CALL FCROP%PRINT_OUT_DETAIL_CROP  (WBS, KPER, KSTP, DELT, TIM, DATETIME)
    IF(FCROP%OUT_BARE      %IS_OPEN) CALL FCROP%PRINT_OUT_BARE         (WBS, KPER, KSTP, DELT, TIM, DATETIME, SOIL%CAPILLARY_FRINGE)
    !
    IF(FCROP%OUT_ET        %IS_OPEN) CALL FCROP%PRINT_OUT_ET(WBS, CLIMATE, KPER, KSTP, DELT, TIM, DATETIME)
    !
    IF(ALLOCATED(FCROP%OUT_CROP)) CALL FCROP%PRINT_OUT_SPECIFIED(WBS, KPER, KSTP, DELT, TIM, DATETIME)
    !
    !  Salt Output-----------------------------------------------------------------------
    !
    IF(WBS%HAS_SALT) THEN 
            IF(SALT%OUT_BYFARM    %IS_OPEN) CALL SALT%PRINT_OUT_BYFARM       (FCROP, WBS, KPER, KSTP, DELT, TIM, DATETIME)
            IF(SALT%OUT_BYFARMCROP%IS_OPEN) CALL SALT%PRINT_OUT_BYFARM_BYCROP(FCROP, WBS, KPER, KSTP, DELT, TIM, DATETIME)
            IF(SALT%OUT_ALL       %IS_OPEN) CALL SALT%PRINT_OUT_ALL_CROP     (FCROP, WBS, KPER, KSTP, DELT, TIM, DATETIME)
    END IF
    !
    !  SURFACE WATER DELIVERY/RETURN OUTPUT-------------------------------------------------------
    !
    IF(SWFL%HAS_SW) THEN
             IF(SWFL%OUT_SFR_SRD      %IS_OPEN ) CALL SWFL%PRINT_OUT_SFR_SRD      (STRM, KPER, KSTP, DELT, TIM, DATETIME)
             IF(SWFL%OUT_SFR_SRD_BYWBS%IS_OPEN ) CALL SWFL%PRINT_OUT_SFR_SRD_BYWBS(STRM, KPER, KSTP, DELT, TIM, DATETIME)
             IF(SWFL%OUT_SFR_SRR      %IS_OPEN ) CALL SWFL%PRINT_OUT_SFR_SRR      (STRM, KPER, KSTP, DELT, TIM, DATETIME)
             IF(SWFL%OUT_SFR_RET      %IS_OPEN ) CALL SWFL%PRINT_OUT_SFR_RET      (STRM, KPER, KSTP, DELT, TIM, DATETIME)
    END IF
    !
    !  NON-ROUTED DELIVERY OUTPUT----------------------------------------------------------------
    !
    IF(SWFL%HAS_NRD) THEN
             IF(SWFL%OUT_NRD_BUD_WBS%IS_OPEN ) CALL SWFL%PRINT_OUT_NRD_BUD_BYWBS(KPER, KSTP, DELT, TIM, DATETIME)
             IF(SWFL%OUT_NRD_BUD    %IS_OPEN ) CALL SWFL%PRINT_OUT_NRD_BUD      (KPER, KSTP, DELT, TIM, DATETIME)
    END IF
    !
    !  Layer Based Output-----------------------------------------------------------------------
    !
    IF(FMPOUT%DPERC%IS_OPEN) THEN
      TXT = 'DPERC'               !USE NULL_FILE AS FLAG FOR COMPACT
      CALL UTILSAV2D(FMPOUT%DPERC,FMPOUT%DPERC%NULL_FILE, WBS%DPERC,TXT,KSTP,KPER,DELT,TIM,NCOL,NROW,NLAY,DATE)
      CALL FMPOUT%DPERC%SIZE_CHECK()
    END IF
    !
    IF(FMPOUT%RUNOFF%IS_OPEN) THEN
      TXT = 'RUNOFF'
      CALL UTILSAV2D(FMPOUT%RUNOFF,FMPOUT%RUNOFF%NULL_FILE,WBS%RUNOFF,TXT,KSTP,KPER,DELT,TIM,NCOL,NROW,NLAY,DATE)
      CALL FMPOUT%RUNOFF%SIZE_CHECK()
    END IF
    !
    !  PRINT WBS_WATER_USE IF REQUESTED -----------------------------------------------------------------------
    !
    IF(FMPOUT%WBS_WATER_USE%IS_OPEN) CALL PRINT_WBS_WATER_USE(FMPOUT%WBS_WATER_USE%IU, KPER, KSTP, DELT, TIM, DATETIME)
    !
    !
    !  Check if files need ot be split-----------------------------------------------------------------------
    !
    CALL FMPOUT%WBS_WATER_USE      %SIZE_CHECK()
    CALL FMPOUT%FDS                %SIZE_CHECK()
    CALL FMPOUT%FB_COMPACT         %SIZE_CHECK()
    CALL FMPOUT%FB_DETAILS         %SIZE_CHECK()
    CALL FMPOUT%ET_LIST            %SIZE_CHECK()
    CALL FMPOUT%FNRCH_LIST         %SIZE_CHECK()
    CALL FMPOUT%ET_ARRAY_SUM       %SIZE_CHECK()
    CALL FMPOUT%ET_ARRAY_SEP       %SIZE_CHECK()
    CALL FMPOUT%FNRCH_ARRAY        %SIZE_CHECK()
    CALL FMPOUT%FWELLS             %SIZE_CHECK()
    CALL FMPOUT%ROUTING_INFORMATION%SIZE_CHECK()
    !   
    IF(FDIM%SFR_CHECKER .AND. WBS%HAS_SFR) CALL SFR_CHECKS(KPER,KSTP,FDIM%SFR_CHECKER_DIR)
    !
  END SUBROUTINE
  !
  !
  SUBROUTINE SFR_CHECKS(KPER,KSTP,DIR)
      USE CONSTANTS
      USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
      USE NUM2STR_INTERFACE,ONLY: NUM2STR
      USE GWFSFRMODULE,     ONLY: NSS, SEG, SEG_NSTRM, IDIVAR,STRM, IOTSG, ISTRM
      USE FMP_GLOBAL,       ONLY: WBS,SWFL
      USE ARRAY_DATA_TYPES, ONLY: INTEGER_VECTOR
      !
      TYPE(GENERIC_OUTPUT_FILE):: FL
      TYPE(INTEGER_VECTOR):: IT
      INTEGER,      INTENT(IN):: KPER,KSTP
      CHARACTER(*), INTENT(IN):: DIR
      INTEGER:: NF,K,I,II,DN1,DN2,UP,UP1,UP2
      CHARACTER(12):: LINE
      LOGICAL:: IS_FIRST
      !
      IF(WBS%HAS_SFR) THEN
          !
         IS_FIRST = KPER==1 .AND. KSTP==1
         !
         IF( IS_FIRST .OR. SWFL%ISRD_TFR%TRANSIENT) THEN
             CALL FL%OPEN('DATAFILE  '//DIR//'./CHK_FMP_SRD_UPSEGS.txt ')
             !
             IF(IS_FIRST)  CALL FL%SET_HEADER('  WBS SRD_SEG   UPSEGS ==>')
             !
             DO NF=1,WBS%NFARM
               DO K=1, SWFL%SRDLOC(NF)%N
                  CALL IT%INIT()
                  I = ISTRM(4,SWFL%SRDLOC(NF)%ISTRM(K))  !ISEG
                  UP=IDIVAR(1,I)
                  IF(UP==0) THEN
                      DO II=1, NSS
                          IF(IOTSG(II)==I) THEN
                              CALL IT%ADD(II)
                          END IF
                      END DO
                  ELSE
                      CALL IT%ADD(UP)
                  END IF
                  IF(IT%N>Z) THEN
                  WRITE(FL%IU,'(I5,I6,100I7)') NF, I, (IT%VEC(II),II=1,IT%N)
                  END IF
               END DO
             END DO
             !
             CALL FL%CLOSE()
             !
         END IF
         !
         CALL FL%OPEN('DATAFILE  '//DIR//'./CHK_SFR_DIV_SEGS.txt BUFFER 16')
         !
         IF(IS_FIRST) CALL FL%SET_HEADER('   KPER   KSTP  UPSEG DIVSEG    UPFLOWOUT   SPEC_UPDIV    DIVFLOWIN   DIVFLOWOUT')
         !
         DO I=1, NSS
           IF(IDIVAR(1,I).NE.Z) THEN
              UP=IDIVAR(1,I)
              DN1 = SEG_NSTRM(I)+1
              DN2 = SEG_NSTRM(I+1)
              UP1 = SEG_NSTRM(UP)+1 
              UP2 = SEG_NSTRM(UP+1)
              !
              IF(SEG(2,I) > STRM(10,DN1)+0.01D0) THEN
                 LINE = 'NOT FULL DIV'
              ELSE
                  LINE = ''
              END IF
              WRITE(FL%IU,'(4I7, 4(1x,F12.1),1X,A)') KPER,KSTP,UP,I,  STRM(9,UP2), SEG(2,I), STRM(10,DN1), STRM(9, DN2), TRIM(LINE)
              !   
           END IF
         END DO
         !
         CALL FL%CLOSE()
         !
         CALL FL%OPEN('DATAFILE '//DIR//'./CHK_SFR_DIV_SEG_DEF.txt ')
         !
         IF(IS_FIRST)  CALL FL%SET_HEADER('  KPER   KSTP  UPSEG DIVSEG    UPFLOWOUT   SPEC_UPDIV    DIVFLOWIN   DIVFLOWOUT        DEFICIT')
         !
         DO I=1, NSS
           IF(IDIVAR(1,I).NE.Z) THEN
              UP=IDIVAR(1,I)
              DN1 = SEG_NSTRM(I)+1
              DN2 = SEG_NSTRM(I+1)
              UP1 = SEG_NSTRM(UP)+1 
              UP2 = SEG_NSTRM(UP+1)
              !
              IF(SEG(2,I) > STRM(10,DN1)+0.01D0)  WRITE(FL%IU,'(4I7, 4(1x,F12.1), 1x,A)') KPER,KSTP,UP,I,STRM(9,UP2),SEG(2,I),STRM(10,DN1),STRM(9, DN2),NUM2STR(SEG(2,I)-STRM(10,DN1),13)
              !
           END IF
         END DO
         !
         CALL FL%CLOSE()
         !
         CALL FL%OPEN('DATAFILE '//DIR//'./CHK_SFR_SRD_INOUT.txt BUFFER 0')
         !
         IF(IS_FIRST) CALL FL%SET_HEADER('   KPER   KSTP    WBS SRDSEG    SRDFLOWIN   SRDFLOWOUT        IN_OUT  FLOW_REMOVED_BY_WBS')
         !
         DO NF=1,WBS%NFARM
            IF(SWFL%SRDLOC(NF)%N == Z) THEN
                CONTINUE
                !!!          WRITE(FL%IU,'(4I7, 2(1x,F12.1), 2(1x,A13))') 
                !!!    +                           KPER,KSTP,NF,Z,
                !!!    +                           DZ,
                !!!    +                           DZ,
                !!!    +                           '0.0','0.0'
            ELSE
                DO K=1, SWFL%SRDLOC(NF)%N
                     UP= ISTRM(4,SWFL%SRDLOC(NF)%ISTRM(K)) !ISEG
                     UP1 = SEG_NSTRM(UP)+1 
                     UP2 = SEG_NSTRM(UP+1)
                     WRITE(FL%IU,'(4I7, 2(1x,F12.1), 2(1x,A))')KPER,KSTP,NF,UP,STRM(10,UP1),STRM(9, UP2),NUM2STR(STRM(10,UP1)-STRM(9, UP2),13),NUM2STR(SWFL%SRDLOC(NF)%FLOW(K),13)
                END DO
            END IF
         END DO
         !
         CALL FL%CLOSE()
         !
         CALL FL%OPEN('DATAFILE '//DIR//'./CHK_SFR_SRD_REACH_INOUT.txt BUFFER 0')
         !
         IF(IS_FIRST)  CALL FL%SET_HEADER('   KPER   KSTP    WBS SRDSEG    SRDFLOWIN   SRDFLOWOUT        IN_OUT  FLOW_REMOVED_BY_WBS')
         !
         DO NF=1,WBS%NFARM
             IF(SWFL%SRDLOC(NF)%N == Z) THEN
                 CONTINUE
                 !!!          WRITE(FL%IU,'(4I7, 2(1x,F12.1), 2(1x,A13))') 
                 !!!    +                           KPER,KSTP,NF,Z,
                 !!!    +                           DZ,
                 !!!    +                           DZ,
                 !!!    +                           '0.0','0.0'
             ELSE
                 DO K=1, SWFL%SRDLOC(NF)%N
                   UP= ISTRM(4,SWFL%SRDLOC(NF)%ISTRM(K)) !ISEG
                   UP1 = SWFL%SRDLOC(NF)%ISTRM(K) 
                   UP2 = SWFL%SRDLOC(NF)%ISTRM(K)
                   WRITE(FL%IU,'(4I7, 2(1x,F12.3), 2(1x,A))') KPER,KSTP,NF,UP,STRM(10,UP1),STRM(9, UP2),NUM2STR(STRM(10,UP1)-STRM(9, UP2),13),NUM2STR(SWFL%SRDLOC(NF)%FLOW(K),13)
                 END DO
             END IF
         END DO
         !
         CALL FL%CLOSE()
         !
         IF( IS_FIRST .OR. SWFL%ISRR_TFR%TRANSIENT) THEN
             CALL FL%OPEN('DATAFILE '//DIR//'./CHK_FMP_SRR_MAP_ISTRM.txt BUFFER 0 ')
             !
             IF(IS_FIRST) CALL FL%SET_HEADER('  WBS  TOT_LENGTH      SRR ==>')
             !
             DO NF=1,WBS%NFARM
               IF(SWFL%SRRLOC(NF)%N > Z) THEN
                   WRITE(FL%IU,'(I5, ES14.7,*(I7))') NF,SWFL%SRRLOC(NF)%TOTLENGTH, SWFL%SRRLOC(NF)%ISTRM
               ELSE
                   WRITE(FL%IU,'(I5, A)') NF, ' NaN'
               END IF
             END DO
             !
             CALL FL%CLOSE()
         END IF
         !
         !!!      CALL FL%OPEN('DATAFILE  ./CHK_SFR_FMP_DATA.txt')
         !!!      IF(KPER==1 .AND. KSTP==1) THEN
         !!!      CALL FL%SET_HEADER(
         !!!     +'KPER  KSTP  WBS   IUPSEG DELSEG UPSEG_OUTFLOW  DIVFLOW  '//
         !!!     +'DELSEG_INFLOW  SRD_INFLOW  DELSEG_OUTFLOW')
         !!!      END IF
         !!!      !
         !!!      DO NF=1,WBS%NFARM
         !!!        IF(    SWFL%ISRD%SEGRCH(1,NF) > 0) THEN
         !!!           I = SWFL%ISRD%SEGRCH(1,NF)
         !!!           II =SWFL%ISRD%SEGRCH(2,NF) 
         !!!           UP=IDIVAR(1,I)
         !!!           UP2 = SEG_NSTRM(UP+1)
         !!!           !
         !!!           II = SEG_NSTRM(I)+II
         !!!           DN1= SEG_NSTRM(I)+1 
         !!!           DN1= SEG_NSTRM(I+1)
         !!!           !
         !!!           IF(UP==0) THEN
         !!!               UPFLOW = 'IUPSEG=0???'
         !!!           ELSE
         !!!                UPFLOW = NUM2STR(STRM(9,UP2),12)
         !!!           END IF
         !!!           WRITE(FL%IU,'(*(A))') NUM2STR(KPER,        5),BLNK,
         !!!     +                           NUM2STR(KSTP,        5),BLNK,
         !!!     +                           NUM2STR(NF,          5),BLNK,
         !!!     +                           NUM2STR(UP,          5),BLNK,
         !!!     +                           NUM2STR(I,           5),BLNK,
         !!!     +                           UPFLOW                 ,BLNK,
         !!!     +                           NUM2STR(SEG(2,I),   12),BLNK,
         !!!     +                           NUM2STR(STRM(10,L), 12),BLNK,
         !!!     +                           NUM2STR(STRM(10,II),12),BLNK,
         !!!     +                           NUM2STR(STRM(9,LL),12)
         !!!       END IF
         !!!      END DO
      END IF
      !
  END SUBROUTINE
  !
  !
  SUBROUTINE FMP4QCNVG()
    !     ******************************************************************
    !     ADJUST CLOSURE CRITERIA TO ALLOW CONVERGENCE OF MNW-PUMPING TO 
    !     FMP-PUMPING REQUIREMENT (EXCLUDE MNW-WELLS NOT LINKED TO FMP)
    !     (still called within BD loop as net MNW rates are not yet computed
    !      within FM-routine - will be called within FM loop in next version)
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE CONSTANTS, ONLY: UNO
    USE FMP_GLOBAL,    ONLY:FWELL, WBS, FMPOPT 
    USE FMPBLK,       ONLY:FPS,ZERO
    USE GWFMNW2MODULE, ONLY:MNW2,MNWNOD
    USE GLOBAL,       ONLY:GW_SOLVER
    USE SIPMODULE,    ONLY:HCLOSE
    USE PCGMODULE,    ONLY:HCLOSEPCG,RCLOSEPCG
    USE GMGMODULE,    ONLY:HCLOSEGMG,RCLOSEGMG
    USE DE4MODULE,    ONLY:HCLOSEDE4
    USE PCGN,        ONLY: HCLOSEPCGN
    USE GWFNWTMODULE, ONLY:Tol,Ftol
    !
    INTEGER:: I,J, FID,FIRSTNODE,LASTNODE
    DOUBLE PRECISION:: HCL,RCL
    DOUBLE PRECISION:: Qdes,Qact
    LOGICAL:: UPDATE_CLOSURE
    !
    IF(.NOT. WBS%HAS_WELL) RETURN                                            !seb NO WELLS TO PROCESS
    !
    !
    !1======DEFINE HCLOSE AND RCLOSE OF VARIOUS SOLVERS
    SELECT CASE (GW_SOLVER)
    CASE('PCG')
                      HCL=DBLE(HCLOSEPCG)
                      RCL=DBLE(RCLOSEPCG)
    CASE('NWT')
                      HCL=Tol
                      RCL=Ftol
    CASE('PCGN')
                      HCL=HCLOSEPCGN
                      RCL=ZERO
    CASE('GMG')
                      HCL=DBLE(HCLOSEGMG)
                      RCL=DBLE(RCLOSEGMG)
    CASE('DE4')
                      HCL=DBLE(HCLOSEDE4)
                      RCL=ZERO
    CASE('SIP')
                      HCL=DBLE(HCLOSE)
                      RCL=ZERO
    END SELECT
    !C
    !3======LOOP THROUGH ALL MNW2-WELLS LINKED TO FMP
    10    FORMAT(1X,/,'HCLOSE & RCLOSE LOWERED BY ',F4.1,                    &
                 ' % & ',F4.1,' % TO ',ES17.3,' AND ',ES17.3,/,1X,           &
                 'FOR MNW2-PUMPING OF',F15.7,' TO CONVERGE TO FMP ',         &
                 'PUMPING REQUIREMENT OF ',F15.7,/,1X,' FOR FMP WELL ',      &
                 A,' LINKED TO MNW1 WELL OF THE SAME NAME LOCATED IN ROW ',  &
                 I5,', COL ',I5,'.',                                          &
                 /,1X,'SOLVER CONTINUES TO ITERATE WITH ADJUSTED ',          &
                 'HCLOSE AND RCLOSE',/)
    UPDATE_CLOSURE = .FALSE.
    !
    FARM_LOOP: DO FID=1, WBS%NFARM
       !
       IF(.NOT. FWELL(FID)%MNWLINK) CYCLE FARM_LOOP
       !
       WELL_LOOP: DO I=1, FWELL(FID)%DIM
           IF(FWELL(FID)%MNWLOC(I) > 0 .AND. FWELL(FID)%ACT(I)) THEN
              J=FWELL(FID)%MNWLOC(I)
              !
              FIRSTNODE=IDINT( MNW2(4,J) )
              LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
              !
              Qdes=MNW2(5,J)
              Qact=SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
              !
              IF(ABS(Qdes).GT.FPS.AND.ABS(Qact).GT.FPS.AND.ABS(Qdes/Qact-UNO).GT.FMPOPT%MNWCLOSE(1)) THEN
                  !
                  HCL=HCL*(UNO - FMPOPT%MNWCLOSE(2))
                  RCL=RCL*(UNO - FMPOPT%MNWCLOSE(3))
                  WRITE(WBS%IOUT,10) FMPOPT%MNWCLOSE(2)*100.D0,FMPOPT%MNWCLOSE(3)*100.D0,HCL,RCL,Qact,Qdes,FWELL(FID)%WELLID(I),FWELL(FID)%LRC(2,I),FWELL(FID)%LRC(3,I)
    
                  WRITE(*,10) FMPOPT%MNWCLOSE(2)*100.D0,FMPOPT%MNWCLOSE(3)*100.D0,HCL,RCL,Qact,Qdes,FWELL(FID)%WELLID(I),FWELL(FID)%LRC(2,I),FWELL(FID)%LRC(3,I)
                  !
                  UPDATE_CLOSURE =.TRUE.
                  EXIT FARM_LOOP
              END IF
              !
           END IF
       END DO WELL_LOOP
    END DO FARM_LOOP
    !
    SELECT CASE (GW_SOLVER)
    CASE('PCG')
                          HCLOSEPCG=SNGL(HCL)
                          RCLOSEPCG=SNGL(RCL)
    CASE('NWT')
                          Tol =SNGL(HCL)
                          Ftol=SNGL(RCL)
    CASE('PCGN')
                         HCLOSEPCGN=SNGL(HCL)
    CASE('GMG')
                          HCLOSEGMG=SNGL(HCL)
                          RCLOSEGMG=SNGL(RCL)
    CASE('DE4')
                          HCLOSEDE4=SNGL(HCL)
    CASE('SIP')
                          HCLOSE=SNGL(HCL)
    END SELECT
    !
  END SUBROUTINE
  !
  !
  SUBROUTINE FMP3WELBD(KSTP,KPER,DELT,DATE,IGRID)
    !     ******************************************************************
    !     CALCUALTE AND PRINT FARM DEMAND AND SUPPLY BUDGET
    !     CALCULATE AND PRINT COMPACT OR DETAILED FARM BUDGET
    !     CALCULATE AND PRINT VOLUMETRIC BUDGET FOR FARM WELLS
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE FMP_GLOBAL,    ONLY:TFDROLD,NRD,FMPDAT,                                  &
                           FMPOUT,FMP_LGR_PNT,WBS,FWELL,SWFL,DRTFLOW
    USE FMPBLK
    USE GWFMNW1MODULE, ONLY:IWL2CBMNW1=>IWL2CB          !IWL2CBMNW1 is local name of IWL2CB seb
    USE GWFMNW2MODULE, ONLY:IWL2CBMNW2=>IWL2CB          ! MNW2 LINK VARIABLES seb
    USE GWFUZFMODULE, ONLY:IRUNFLG,IUZFBND,IRUNBND,EXCESPP,SEEPOUT, REJ_INF !Added IRUNFLG to check for type of runoff accounted for by FMP from UZF
    USE GLOBAL,       ONLY:NCOL,NROW,NLAY,IBOUND,BUFF,ITMUNI                   !Added BOTM and LBOTM for NWT connection with Farm wells by rth
    USE GWFBASMODULE, ONLY:MSUM,ICBCFL,PERTIM,TOTIM,VBVL,VBNM, IAUXSV, REALTIM
    !crth Added additional function interface and module reference needed for NWT connection with Farm wells
    USE CONSTANTS, ONLY: TRUE, FALSE, Z, ONE, DZ, UNO, NEARZERO_10
    !USE WEL_SUBROUTINES, ONLY:WEL_SMOOTH
    !
    INTEGER, INTENT(IN):: KSTP,KPER,IGRID
    DOUBLE PRECISION, INTENT(IN):: DELT
    CHARACTER(*),     INTENT(IN):: DATE
    CHARACTER(16),DIMENSION(1):: FWLAUX
    !
    CHARACTER(16):: TEXT
    CHARACTER(14):: TIMEUNIT
    CHARACTER(14):: TIME
    INTEGER:: N,IR,IC,IL,I,NF,ISWFLAG,IBD,IBDLBL,NAUX,NFW
    REAL:: RIN,ROUT
    REAL:: Q, DT, PT, ST
    DOUBLE PRECISION:: RATIN, RATOUT
    DOUBLE PRECISION:: QR,SWOLD,SWFIN,QF, TF, ND, QQ,                 &
                       TG,EG,TT,ET,P,SR,DP,EXT,TOTIN,TOTOUT,DISC,DRT,SRD,  &
                       EP,TP,EI,TI,NDOUT,SRDOUT,RDOUT,QFOUT,DR          !added local variable Q for NWT connection to Farm Wells by rth
    !
    INTEGER:: FID, IWL2CB
    REAL,DIMENSION(5):: AUX_VEC
    !
    TEXT  = '      FARM WELLS'
    AUX_VEC = 0.0
    !
    !1===== ADJUST CLOSURE CRITERIA TO ALLOW CONVERGENCE OF MNW-PUMPING TO FMP-PUMPING REQUIREMENT
    !       (EXCLUDE MNW-WELLS NOT LINKED TO FMP)
    !
    ! PULL CORRECT CELL TO CELL FLAG FROM MNW1/MNW2
    IF(    WBS%HAS_MNW1) THEN
                             IWL2CB=IWL2CBMNW1
    ELSEIF(WBS%HAS_MNW2) THEN
                             IWL2CB=IWL2CBMNW2
    ELSE
                             IWL2CB=Z
    END IF
    !
    !2===== DEFINE TIME UNIT LABEL FOR "LABEL HEADER" ==========================================================
    IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
    IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
    IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
    IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
    IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
    IF(TOTIM.GE.TPU.OR.TOTIM.LT.TPL) THEN
                                     WRITE(TIME,'(ES14.7)') TOTIM
    ELSE
                                     WRITE(TIME,'(F14.2)') TOTIM
    END IF
    !
    !3===== PRINT HEADER FOR FARM DEMAND & SUPPLY LIST ==================================================
    !
    !
    IF(SWFL%HAS_SRD) THEN
        ISWFLAG=1
    ELSE
        ISWFLAG=Z
    END IF
    !
    !5D-----PRINT DEMAND AND SUPPLY PER FARM TO FILE
    !
    IF(FMPOUT%FDS%IS_OPEN) THEN !FDS OUTPUT ASCII OR BINARY --ONYL ASCII SUPORTED
        !
        DO NF=1,WBS%NFARM
          !
          IF(WBS%INUSE(NF)) THEN
              N=ONE
          ELSE
              N=Z
          END IF
          !
          TF=WBS%DEMAND(NF)
          ND=NRD(1,NF)
          QR=WBS%Q_DEMAND(NF)
          IF(TFDROLD(NF).EQ.DZ) THEN
              TFDROLD(NF)=TF
              NRD(2,NF)=ND
              WBS%Q_DEMAND_INI(NF) = QR
          ENDIF
          IF(SWFL%HAS_SRD_WBS(NF)) THEN
                                      SWOLD=TFDROLD(NF)-NRD(2,NF)-WBS%Q_DEMAND_INI(NF)
                                      SWFIN=SWFL%SRDLOC(NF)%TOT_DMD_MET
                                      !
                                      IF(SWOLD < NEARZERO_10) SWOLD=DZ
                                      IF(SWFIN < NEARZERO_10) SWFIN=DZ
          ELSE
                                      SWOLD=DZ
                                      SWFIN=DZ
          END IF
          !
          IF(QR.LT.FWELL(NF)%QMAX) QF=QR
          IF(QR.GE.FWELL(NF)%QMAX) QF=FWELL(NF)%QMAX
          !
          IF(FMPOUT%FDS%BINARY) THEN
              WRITE(FMPOUT%FDS%IU) DATE,N,KPER,KSTP,DELT,DBLE(TOTIM),NF,WBS%DEFICIENCY%LIST(NF),WBS%EFF(NF),TFDROLD(NF), NRD(2,NF), SWOLD, WBS%Q_DEMAND_INI(NF), TF, ND, SWFIN, QR, QF
          ELSE
             WRITE(FMPOUT%FDS%IU,'(2I5,A14,I7,F12.4,9(1x,ES21.8),I10,2x,A,3x,I1)') KPER,KSTP,TIME,NF,WBS%EFF(NF),                               &
                                                                                   TFDROLD(NF), NRD(2,NF), SWOLD, WBS%Q_DEMAND_INI(NF), TF, ND, SWFIN, QR, QF,    &
                                                                                   WBS%DEFICIENCY%LIST(NF),DATE,N
          END IF
        END DO
    END IF 
    !
    IF(FMPOUT%FB_COMPACT%IS_OPEN .OR. FMPOUT%FB_DETAILS%IS_OPEN) THEN
      DO NF=1,WBS%NFARM
         !
         SRD    = DZ
         !RD     = DZ  !Feature no longer used
         EXT    = DZ
         NDOUT  = DZ
         SRDOUT = DZ
         RDOUT  = DZ  !Feature no longer used
         TOTIN  = DZ
         TOTOUT = DZ
         DISC   = DZ
         !
         QF     = DZ
         QFOUT  = DZ
         !
         IF(WBS%INUSE(NF)) THEN
             N=ONE
             P = WBS%PRECIP(NF)
             TP= WBS%TPRECIP(NF)
             EP= WBS%EPRECIP(NF)
             TT= WBS%TTOT(NF)
             ET= WBS%ETOT(NF)
             TG= WBS%TGWA(NF)
             EG= WBS%EGWA(NF)
             EI= WBS%EIRR(NF)
             TI= WBS%TIRR(NF)
             SR= WBS%TOT_RUNOFF(NF)
             DP= WBS%TOT_DPERC(NF)
             DR= WBS%TOT_DIR_RCH(NF)
             !
             TF = WBS%DEMAND(NF)
             ND = NRD(1,NF)
             QR = WBS%Q_DEMAND(NF)
             !
             IF ( WBS%HAS_DRT)THEN
                   DRT = DRTFLOW(NF)%FLO !seb ADD DRTFLOW TO ALLOW ADDED FLOWS FROM DRT
             ELSE
                   DRT = DZ
             END IF
             !
             DO I=1, FWELL(NF)%DIM
                IF(FWELL(NF)%ACT(I)) THEN  
                    if(FWELL(NF)%Q(I) < DZ) then
                          QF = QF + FWELL(NF)%Q(I)
                    else
                          QFOUT = QFOUT + FWELL(NF)%Q(I)
                    end if
                END IF
             END DO
             QF = ABS(QF)
             !
             IF(SWFL%HAS_SRD_WBS(NF).OR.(IGRID.GT.1.AND.FMPDAT(1)%SWFL%HAS_SRD_WBS(NF))) SRD=TF-ND-QR
             !
             IF(WBS%UZF_LINK) THEN
                 DO I=1, WBS%FID(NF)%Count
                     IR=WBS%FID(NF)%RC(1,I)
                     IC=WBS%FID(NF)%RC(2,I)
                     IF(IUZFBND(IC,IR).GT.0.AND.IRUNFLG.GT.0)THEN
                         IF(IRUNBND(IC,IR).GT.0)THEN
                             QQ = EXCESPP(IC,IR)+SEEPOUT(IC,IR)+REJ_INF(IC,IR)
                             SR = SR + QQ
                             DP = DP - QQ
                         END IF
                     END IF
                 END DO
             END IF
             !
             IF(ND < DZ) THEN
                             NDOUT=-ND
                             ND=DZ
             ENDIF
             !
             IF(SRD < DZ) THEN
                          SRDOUT=-SRD
                          SRD=DZ
             ENDIF
             IF(SRD < NEARZERO_10) SRD=DZ
         ELSE
             N=Z
             P = DZ;  TP= DZ;  EP= DZ
             TT= DZ;  ET= DZ;  TG= DZ
             EG= DZ;  EI= DZ;  TI= DZ
             SR= DZ;  DP= DZ;  DR= DZ
             DRT=DZ
             !
             TF= DZ;  ND= DZ;  QR= DZ
         END IF
         !
         TOTIN  = P+ND+SRD+DRT+QF+EG+TG+DR
         !
         TOTOUT = ET+TT+SR+DP+NDOUT+SRDOUT+RDOUT+QFOUT
         !
         If(WBS%DEFICIENCY%LIST(NF).EQ.Z.AND.TOTIN.LT.TOTOUT) THEN
                                                              EXT   = TOTOUT-TOTIN
                                                              TOTIN = TOTIN+EXT
         ENDIF
         !
         IF(ABS(TOTIN+TOTOUT).GT.FPS) DISC=((TOTIN-TOTOUT)/((TOTIN+TOTOUT)/2.D0))*100.D0
         !
         IF(FMPOUT%FB_COMPACT%IS_OPEN) THEN
             !
             WRITE(FMPOUT%FB_COMPACT%IU,'(2I12,A14,I12,1x,11(ES17.9,1x), F18.3, 2x,A, 3x,I1)') KPER,KSTP,TIME,NF, &
                                                                                P, ND+SRD+DRT, QF+EG+TG, EXT, TOTIN-DR, TT+ET, SR+DP-DR, NDOUT+SRDOUT+RDOUT, QFOUT+EG+TG, TOTOUT-DR, TOTIN-TOTOUT, DISC,   &
                                                                                DATE,N
         END IF
         !
         IF(FMPOUT%FB_DETAILS%IS_OPEN) THEN
             !
             IF(FMPOUT%FB_DETAILS%BINARY) THEN
                 WRITE(FMPOUT%FB_DETAILS%IU) DATE,N,KPER,KSTP,DELT,DBLE(TOTIM),NF, P, ND, SRD, DRT, QF, EG, TG, DR, EXT, EI, EP,  EG, TI, TP, TG, SR, DP, NDOUT, SRDOUT, RDOUT, QFOUT
             ELSE
                 WRITE(FMPOUT%FB_DETAILS%IU,'(2I12,A14,I12,1x,24(ES17.9,1x), F18.3,2x,A, 3x,I1)')                                                          &
                                                                                   KPER,KSTP,TIME,NF,                                                    &
                                                                                   P,  ND, SRD, DRT, QF, EG, TG, DR,   EXT, TOTIN,                        &
                                                                                   EI, EP,  EG, TI, TP, TG, SR, DP, NDOUT, SRDOUT, RDOUT, QFOUT, TOTOUT, &
                                                                                   TOTIN-TOTOUT, DISC, DATE, N
             END IF
         END IF
         !
      END DO
    END IF
    !
    !8===== CALCULATE AND PRINT VOLUMETRIC BUDGET FOR FARM WELLS ================================================
    !
    !8A-----CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL BUDGET FLAG.
    !
    RATIN=DZ
    RATOUT=DZ
    IBD=Z
    !
    !8-----IF THERE ARE NO FARM WELLS, DO NOT ACCUMULATE FLOW.
    IF(WBS%HAS_WELL) THEN
           !
           IF(FMPOUT%WEL_CBC.LT.Z .AND. ICBCFL.NE.Z) IBD=-1
           IF(FMPOUT%WEL_CBC.GT.1) IBD=ICBCFL
           !
           IBDLBL=Z
           !
           !8B-----IF FINAL PUMPING RATES WILL BE SAVED AS A LIST TO ASCII FILE, WRITE HEADER
           !       (TO BUILD TIME SERIES FOR FARM WELLS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM WELL)
           !
           !
           !8C----IF FINAL PUMPING RATES WILL BE SAVED AS A LIST TO COMPACT BUDGET BINARY FILE, WRITE HEADER.
           !
           !  Note FWELLS are included in the CBC even if they are not in use
           IF(IBD.EQ.2) THEN
               !
               NAUX = FWELL(1)%NAUX    !SAME VALUE ACROSS ALL WELLS
               IF(IAUXSV.EQ.Z) NAUX=Z
               !
               IF(NAUX > Z) ERROR STOP 'CODE ERROR PLEASE SET UP "FWLAUX"'
               ! 
               NFW = Z
               !
               DO NF=1,WBS%NFARM
                  IF(WBS%INUSE(NF) .and. FWELL(NF)%DIM > Z) THEN
                     !
                     DO I =1, FWELL(NF)%DIM
                        IF(FWELL(NF)%ACT(I) .AND. FWELL(NF)%MNWLOC(I) == Z ) NFW = NFW + ONE
                     END DO
                     !
                     IF(FWELL(NF)%MNWLINK) THEN
                         DO I=1, FWELL(NF)%DIM
                           IF(FWELL(NF)%ACT(I) .AND. FWELL(NF)%MNWLOC(I) > Z) THEN
                              IF(FWELL(NF)%LISTPRINT.AND.KPER.EQ.1.AND.IWL2CB.EQ.Z)THEN
                                                                                   WRITE(WBS%IOUT,67) FID, FWELL(FID)%WELLID(I), FWELL(FID)%LRC(2:3,I)
                                                                                              67 FORMAT(1X,'CUMULATIVE PUMPING RATE OF FARM WELL,',I8,'LINKED TO MNW2 WELL ',A,' THAT IS LOCATED IN ROW ',I8,'COL ', I8, ' IS LISTED IN BINARY FILE' )
                              ENDIF
                              !
                              IF(FWELL(NF)%LISTPRINT.AND.KPER.GT.1.AND.IWL2CB.EQ.Z) WRITE(WBS%IOUT,'(1x,A)') '(ROW & COLUMN LOCATION AND LAYER OF TOP NODES ARE ONLY PRINTED TO LISTING FILE ONCE FOR STRESS PERIOD 1)'
                           ENDIF
                         END DO
                     END IF
                  END IF
               END DO
               !
               DT = REAL( DELT  , kind(DT))
               PT = REAL( PERTIM, kind(PT))
               ST = REAL( TOTIM , kind(ST))
               CALL UBDSV4(KSTP,KPER,TEXT,NAUX,FWLAUX,FMPOUT%WEL_CBC,NCOL,NROW,NLAY,NFW,WBS%IOUT,DT,PT,ST,IBOUND)
               !
           ENDIF
           !  
           !8F-----LOOP THROUGH EACH FARM WELL CALCULATING FLOW AND PRINTING
           !
           !
           WHERE (TFDROLD==DZ) 
                  TFDROLD = WBS%DEMAND
           END WHERE
           !
           CALL FWELL%PRINT_SMOOTHED_PUMPING( KPER, KSTP, TOTIM, DATE )
           CALL FWELL%PRINT_BYWELL( KPER,KSTP,DELT,REALTIM,DATE )
           CALL FWELL%PRINT_BYFARM( KPER,KSTP,DELT,REALTIM,DATE,TFDROLD,WBS%DEMAND )
           CALL FWELL%PRINT_BYMNW ( KPER,KSTP,DELT,REALTIM,DATE )
           !
           IF(FWELL(1)%OUT_BYLAYER%IS_OPEN) CALL FWELL%PRINT_BYLAYER( KPER,KSTP,DELT,REALTIM,DATE,WBS )
           !
           !8F4----PRINT FLOW RATE PER FARM-WELL TO LIST-FILE IF REQUESTED
           !
           IF(IBD.LT.Z) CALL FWELL%PRINT_TO_LIST(KPER,KSTP,TOTIM,DATE)
           !
           !8F5----PRINT FLOW RATE TO ASCII FILE
           IF(FMPOUT%FWELLS%IS_OPEN) THEN
              DO NF=1,WBS%NFARM
              IF(WBS%INUSE(NF)) THEN
                 DO I=1, FWELL(NF)%DIM
                     IF(FWELL(NF)%ACT(I)) THEN
                         !
                         WRITE(FMPOUT%FWELLS%IU,'(2I5,A14,I7,A,I9,3I7,ES17.8)', ADVANCE='NO') KPER,KSTP,TIME,IGRID,FWELL(NF)%WELLID(I),NF,FWELL(NF)%LRC(:,I),FWELL(NF)%Q(I)
                         !
                         IF(.NOT. WBS%HAS_MNW2) THEN 
                                                    WRITE(FMPOUT%FWELLS%IU,'(A)')
                         ELSEIF(FWELL(NF)%MNWLOC(I) == Z) THEN 
                                                    WRITE(FMPOUT%FWELLS%IU,'(3x,A)') 'NO'
                         ELSE
                                                    WRITE(FMPOUT%FWELLS%IU,'(2x,A)') 'YES'
                         END IF
                     END IF
                 END DO
              END IF
              END DO
           ENDIF
           !
           !8F7----SEE IF FLOW IS POSITIVE OR NEGATIVE.
           DO NF=1, WBS%NFARM
           IF(WBS%INUSE(NF)) THEN
              DO I =1, FWELL(NF)%DIM
              IF( FWELL(NF)%ACT(I) .AND. FWELL(NF)%MNWLOC(I) == Z ) THEN !Only include wells that are NOT linked to MNW2
                  IF ( FWELL(NF)%Q(I) < DZ ) THEN
                                                  RATOUT = RATOUT - FWELL(NF)%Q(I)
                  ELSE
                                                  RATIN  = RATIN  + FWELL(NF)%Q(I)
                  END IF
              END IF
              END DO
           END IF
           END DO
           !
           !8G-----CELL-BY-CELL FLOWS (PUMPING RATES) WILL BE SAVED AS A 3-D ARRAY IN A BINARY FILE, 
           !       IF "COMPACT BUDGET" IS NOT SPECIFIED IN OUTPUT CONTROL (ANALOGOUS TO WEL6),
           !       AND PRINT INFO TELLING THAT MULTI-AQUIFER WELL PUMPAGE IN 3-D ARRAY IS
           !       ASSOCIATED WITH NODE OF TOP LAYER OF MULTI-LAYER MNW-WELL.
           !
           !
           IF(IBD.EQ.1)  THEN
               !
               DO CONCURRENT (IC=1:NCOL,IR=1:NROW,IL=1:NLAY) 
                                                            BUFF(IC,IR,IL)=0.0  
               END DO
               !
               DO NF=1, WBS%NFARM
               IF(WBS%INUSE(NF)) THEN
                  DO I =1, FWELL(NF)%DIM
                  IF(FWELL(NF)%ACT(I) .AND. FWELL(NF)%MNWLOC(I) == Z ) THEN
                      IL = FWELL(NF)%LRC(1,I) 
                      IR = FWELL(NF)%LRC(2,I) 
                      IC = FWELL(NF)%LRC(3,I)
                      !
                      BUFF(IC,IR,IL) = BUFF(IC,IR,IL) + FWELL(NF)%Q(I)
                  END IF
                  END DO
               END IF
               END DO
               CALL UBUDSV(KSTP,KPER,TEXT,FMPOUT%WEL_CBC,BUFF,NCOL,NROW,NLAY,WBS%IOUT)
           END IF
           !
           !8F10---CELL-BY-CELL FLOWS (PUMPING RATES) WILL BE SAVED AS A LIST IN A BINARY FILE, 
           !       IF "COMPACT BUDGET" IS SPECIFIED IN OUTPUT CONTROL. (ANALOGOUS TO WEL6)
           !       OR IF RETURNING THE FLOW IN THE FARM-WELL ARRAY, COPY FLOW TO FARM-WELL.
           IF(IBD.EQ.2) THEN
               DO NF=1, WBS%NFARM
               IF(WBS%INUSE(NF)) THEN
                  DO I=1, FWELL(NF)%DIM
                     IF(FWELL(NF)%ACT(I) .AND. FWELL(NF)%MNWLOC(I) == Z) THEN  !Only include wells that are NOT linked to MNW2
                         Q  = FWELL(NF)%Q(I)
                         IL = FWELL(NF)%LRC(1,I) 
                         IR = FWELL(NF)%LRC(2,I) 
                         IC = FWELL(NF)%LRC(3,I)
                         IF(NAUX > Z) AUX_VEC(:NAUX) = REAL( FWELL(NF)%AUX(I,:),KIND(AUX_VEC))
                         !
                         CALL UBDSVB(FMPOUT%WEL_CBC,NCOL,NROW,IC,IR,IL,Q,AUX_VEC,5,NAUX,1,IBOUND,NLAY)
                     END IF
                  END DO
               END IF
               END DO
           END IF
           !
           !8H-----MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
           RIN         =SNGL(RATIN)
           ROUT        =SNGL(RATOUT)
           VBVL(3,MSUM)=RIN
           VBVL(4,MSUM)=ROUT
           VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
           VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
           VBNM(MSUM)  =TEXT
           !
           !8I-----INCREMENT BUDGET TERM COUNTER(MSUM).
           !
           MSUM=MSUM+1
           !
    END IF !(WBS%HAS_WELL) 
    !
    ! SCOTT WHY ARE THESE SET TO ZERO NOW?
    !5E-----RESET SOME VARIABLES  
    TFDROLD  = DZ                                                 !NOTE THAT TFDROLD, NRD(2 & QREQOLD 
    NRD(2,:) = DZ  
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_WBS_WATER_USE(IU, KPER, KSTP, DELT, DYEAR, DATE)
    USE CONSTANTS,  ONLY: TRUE, FALSE, Z, ONE, TWO, DZ, UNO, NEARZERO_10
    USE FMP_GLOBAL, ONLY: NRD, FCROP, WBS, SWFL, FWELL
    INTEGER,         INTENT(IN):: IU, KPER, KSTP
    DOUBLE PRECISION,INTENT(IN):: DELT
    DOUBLE PRECISION,INTENT(IN):: DYEAR
    CHARACTER(*),    INTENT(IN):: DATE
    INTEGER:: F, I, J, K, R, C
    DOUBLE PRECISION:: AREA
    DOUBLE PRECISION:: PET_NO_IRR, PET_IRR, P_NI, P, ETgw_NI, ETp_NI, ETgw, ETp, ETi
    DOUBLE PRECISION:: NRD_USE, SRD_USE, Q_USE
    CHARACTER(17):: DT
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    !
    DO F=1, WBS%NFARM
         !
         IF(.NOT. WBS%INUSE(F)) CYCLE
         !
         AREA = DZ
         DO K=ONE, WBS%FID(F)%Count
                   AREA =  AREA + WBS%AREA( WBS%FID(F)%RC(TWO,K), WBS%FID(F)%RC(ONE,K) )
         END DO
         !
         PET_NO_IRR = DZ
         PET_IRR    = DZ
         ETgw_NI    = DZ
         ETp_NI     = DZ
         ETgw       = DZ
         ETp        = DZ
         ETi        = DZ
         P_NI       = DZ
         P          = DZ
         !
         DO J=ONE, WBS%CROP(F)%N
             I=WBS%CROP(F)%PNT(ONE,J)
             K=WBS%CROP(F)%PNT(TWO,J)
             !
             IF(FCROP%CROP(I)%IRR(K) > Z) THEN             !Note does not include OFE losses
                 PET_IRR    = PET_IRR    + FCROP%CROP(I)%CU(K)
             ELSE
                 PET_NO_IRR = PET_NO_IRR + FCROP%CROP(I)%CU(K)
             END IF
             !
             !IF(FCROP%CROP(I)%IRR(K) > Z) THEN
             !    PET_LOSS_IRR    = PET_LOSS_IRR    + FCROP%CROP(I)%ANOX_LOSS(K) + FCROP%CROP(I)%SOIL_LOSS(K)
             !ELSE
             !    PET_LOSS_NO_IRR = PET_LOSS_NO_IRR + FCROP%CROP(I)%ANOX_LOSS(K) + FCROP%CROP(I)%SOIL_LOSS(K)
             !END IF
             !
             !IF(FCROP%CROP(I)%EFF(K)>DZ) DMDI= DMDI + FCROP%CROP(I)%CIR_INI(K)/FCROP%CROP(I)%EFF(K) + FCROP%CROP(I)%DEMAND_EXT_INI(K)
             !SUP = SUP  + FCROP%CROP(I)%DEMAND (K)
             !
             IF(FCROP%CROP(I)%IRR(K) > Z) THEN
                 !
                                   P = P + FCROP%CROP(I)%PRECIP(K)
                 IF(FCROP%HAS_Pe)  P = P + FCROP%CROP(I)%RNOFF_Peff(K)
                 !
                 ETgw    = ETgw    + FCROP%CROP(I)%EGWA(K) + FCROP%CROP(I)%TGWA(K)
                 ETp     = ETp     + FCROP%CROP(I)%EP(K) + FCROP%CROP(I)%TP(K)
                 ETi     = ETi     + FCROP%CROP(I)%TI(K) + (FCROP%CROP(I)%TI(K) * FCROP%CROP(I)%CECT(K))
             ELSE
                 P_NI    = P_NI    + FCROP%CROP(I)%PRECIP(K)
                 ETgw_NI = ETgw_NI + FCROP%CROP(I)%EGWA(K) + FCROP%CROP(I)%TGWA(K)
                 ETp_NI  = ETp_NI  + FCROP%CROP(I)%EP(K) + FCROP%CROP(I)%TP(K)
             END IF
             !
         END DO
         !
         IF(FCROP%CHECK_BARE) THEN
            DO K=ONE, WBS%FID(F)%Count
                  R = WBS%FID(F)%RC(ONE,K)
                  C = WBS%FID(F)%RC(TWO,K)
                  IF(FCROP%BARE_FRAC(C,R) > DZ) THEN
                     ETgw_NI = ETgw_NI + FCROP%BARE_EVAP(C,R)
                     ETp_NI  = ETp_NI  + FCROP%BARE_EVAP_PRECIP(C,R)
                     P_NI    = P_NI    + FCROP%BARE_TOT_PRECEP(C,R)
                  END IF
            END DO
         END IF
         !
         !NRD_SUP, Q_SUP, SRD_SUP
         !
         NRD_USE  = NRD(1,F)
         !
            Q_USE = WBS%Q_DEMAND(F)
         IF(Q_USE.GE.FWELL(F)%QMAX) Q_USE=FWELL(F)%QMAX
         !
         SRD_USE = DZ
         IF(SWFL%SRDLOC(F)%N > Z) THEN
             DO I = ONE, SWFL%SRDLOC(F)%N
              IF(SWFL%SRDLOC(F)%ISTRM(I) > Z) THEN
                SRD_USE = SRD_USE + SWFL%SRDLOC(F)%FLOW(I)
              END IF
             END DO
         END IF
         !
         WRITE(IU, '(3I7, 17A17, 2x,F13.7, 2x,A)') KPER, KSTP, F, NUM2STR(AREA), NUM2STR(WBS%IRR_AREA(F)), NUM2STR(PET_NO_IRR), NUM2STR(ETgw_NI), NUM2STR(ETp_NI), NUM2STR(P_NI), NUM2STR(PET_IRR), NUM2STR(ETgw), NUM2STR(ETp), NUM2STR(ETi), NUM2STR(P), NUM2STR(WBS%DEMAND_POT(F)), NUM2STR(WBS%EFF(F)), NUM2STR(NRD_USE), NUM2STR(SRD_USE), NUM2STR(Q_USE), DT, DYEAR, DATE
       END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE FMP3FNRBD(KSTP,KPER,IGRID)
      !
      !  CALCULATE AND PRINT VOLUMETRIC BUDGET FOR FARM NET RECHARGE
      !
      USE FMP_GLOBAL,  ONLY:IFA,FMPOUT,FMP_LGR_PNT, WBS,FCROP 
      USE FMPBLK,      ONLY:TPL,TPU,ZERO,ZER
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,BUFF,ITMUNI, UPLAY,UPLAY,INPUT_CHECK
      USE GWFBASMODULE,ONLY:DELT,VBVL,VBNM,MSUM,ICBCFL,PERTIM,TOTIM
      USE GWFUZFMODULE, ONLY: IUZFBND
      USE CONSTANTS, ONLY: TRUE, FALSE, Z, ONE, DZ, UNO
      !
      INTEGER, INTENT(IN):: KSTP,KPER,IGRID
      !
      CHARACTER(16):: TEXT
      CHARACTER(14):: TIMEUNIT
      CHARACTER(14):: TIME
      INTEGER IRCH(NCOL,NROW)
      INTEGER I,IL,IR,IC,IBD,J,NF,NOPT                                  !FORMERLY IMPLICIT INTEGER
      REAL ROUT, RIN                                                    !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION RATIN,RATOUT
      !
      CALL FMP_LGR_PNT(IGRID)
      TEXT = 'FARM  NET  RECH.'
      !
      ! CLEAR THE RATE ACCUMULATORS
      !
      RATIN  = DZ
      RATOUT = DZ
      !
      !1===== DEFINE TIME UNIT LABEL FOR "LABEL HEADER" ==========================================================
      IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
      IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
      IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
      IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
      IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
      IF(TOTIM.GE.TPU.OR.TOTIM.GT.TPL) THEN
         WRITE(TIME,'(ES14.7)') TOTIM
      ELSE
         WRITE(TIME,'(F14.2)') TOTIM
      END IF
      !
      !2A2----CLEAR THE BUFFER & SET FLAG FOR SAVING CELL-BY-CELL FLOW TERMS.
      DO CONCURRENT (IC=1:NCOL,IR=1:NROW,IL=1:NLAY) 
                                                   BUFF(IC,IR,IL)=ZER  
      END DO
      !
      DO CONCURRENT (IR=1:NROW, IC=1:NCOL) 
                                          WBS%FNRCH(IC,IR) = DZ
      END DO
      !
      DO CONCURRENT (IR=1:NROW, IC=1:NCOL) 
                                IF(UPLAY(IC,IR)>Z) THEN
                                                   IRCH(IC,IR) = UPLAY(IC,IR)
                                ELSE
                                                   IRCH(IC,IR) = ONE
                                END IF
      END DO
      !
      IF(.NOT. INPUT_CHECK) THEN
         IF(WBS%UZF_LINK) THEN
             !DO CONCURRENT (IR=ONE:WBS%NROW, IC=ONE:WBS%NCOL, WBS%DPERC(IC,IR)>DZ       &
             !                                                .AND. IUZFBND(IC,IR)<ONE   &
             !                                                .AND. UPLAY(IC,IR)>Z)
             DO IR=ONE, WBS%NROW
             DO IC=ONE, WBS%NCOL
             IL = UPLAY(IC,IR) 
             IF(      WBS%DPERC(IC,IR)>DZ  &
                .AND. IUZFBND(IC,IR)<ONE   &
                .AND. IL>Z                  ) THEN
                   WBS%FNRCH(IC,IR) = WBS%DPERC(IC,IR)
                   !
                   BUFF(IC,IR,IL) = SNGL(WBS%DPERC(IC,IR))
                   RATIN = RATIN + WBS%DPERC(IC,IR)
             END IF
             END DO
             END DO
         ELSE
             DO IR=ONE, WBS%NROW
             DO IC=ONE, WBS%NCOL
             IL = UPLAY(IC,IR) 
             IF(WBS%DPERC(IC,IR)>DZ .AND. IL>Z) THEN
                   !
                   WBS%FNRCH(IC,IR) = WBS%DPERC(IC,IR)
                   !
                   BUFF(IC,IR,IL) = SNGL(WBS%DPERC(IC,IR))
                   RATIN = RATIN + WBS%DPERC(IC,IR)
             END IF
             END DO
             END DO
         END IF
         !
         IF(FCROP%NCROP > Z) THEN
             DO IR=ONE, WBS%NROW
             DO IC=ONE, WBS%NCOL
             IL = UPLAY(IC,IR) 
             IF(FCROP%TGWA(IC,IR)>DZ .AND. IL>Z) THEN
                             !
                             BUFF(IC,IR,IL)   = BUFF(IC,IR,IL)   - SNGL(FCROP%TGWA(IC,IR))
                             !
                             WBS%FNRCH(IC,IR) = WBS%FNRCH(IC,IR) - FCROP%TGWA(IC,IR)
                             !
                             RATOUT = RATOUT + FCROP%TGWA(IC,IR)
             END IF
             END DO
             END DO
         END IF
         !
         IF(FCROP%NCROP > Z .OR. FCROP%CHECK_BARE) THEN
             DO IR=ONE, WBS%NROW
             DO IC=ONE, WBS%NCOL
             IL = UPLAY(IC,IR) 
             IF(FCROP%EGWA(IC,IR)>DZ .AND. IL>Z) THEN
                             !
                             BUFF(IC,IR,IL)   = BUFF(IC,IR,IL) - SNGL(FCROP%EGWA(IC,IR))
                             !
                             RATOUT = RATOUT + FCROP%EGWA(IC,IR)
                             !
                             WBS%FNRCH(IC,IR) = WBS%FNRCH(IC,IR) - FCROP%EGWA(IC,IR)
             END IF
             END DO
             END DO
         END IF
         !
      END IF !INPUT_CHECK
      !
      CALL WBS%SUM_WBS_FNRCH()  ! SUM UP TOTAL WBS FNRCH -- POPULATES WBS%TOT_FNRCH
      !
      !2===== CALCULATE VOLUMETRIC BUDGET OF FARM NET RECHARGE ===================================================
      !
      IBD=Z
      IF(FMPOUT%FNR_CBC.LT.Z .AND. ICBCFL.NE.Z) IBD=-1
      IF(FMPOUT%FNR_CBC.GT.3) IBD=ICBCFL
      !
      !3===== PRINT 2D-ARRAY OF CELL-BY-CELL NET RECHARGE FLOW RATES TO LIST FILE ===============================
      !       (FOR TIME STEPS SPECIFIED BY OUTPUT CONTROL)
      !
      !3A-----WRITE HEADER TO LIST FILE.
      IF(IBD.LT.Z) THEN
             WRITE(WBS%IOUT,"(//,1X,A,'   PERIOD',I3,'   STEP',I3,',',I4,' COLUMNS,',I4,' ROWS,',I3,' LAYERS',7X,'ELAPSED TIME',A14,1X,A14,/)") TEXT,KPER,KSTP,NCOL,NROW,NLAY,TIME,TIMEUNIT
             !
             !3B-----WRITE DATA AS ONE OR TWO RECORDS CONTAINING ONE VALUE PER LAYER.
             IF(NLAY.EQ.1) THEN
                 !
                 !3B1----WRITE ONE RECORD OF FLOW VALUES IF ONLY ONE LAYER
                 !
                 DO I=1,NROW
                           WRITE(WBS%IOUT,'(*(ES15.7))') BUFF(:,I,1)
                 ENDDO
             ELSE
                 !3B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS RECEIVE NET-RECHARGE.
                 !       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS FLOW VALUES).
                 !
                 DO I=1,NROW
                            WRITE(WBS%IOUT,'(*(I12))') IRCH(:,I)
                 ENDDO
                 !
                 DO I=1,NROW
                            WRITE(WBS%IOUT,'(*(ES15.7))') ( BUFF(J,I,IRCH(J,I)), J=1, NCOL )
                 ENDDO
                 !
             ENDIF
      ENDIF
      !
      !
      !4===== PRINT NET RECHARGE FLOW RATES TO ASCII FILES ========================================================
      !
      !4A-----PRINT 2D-ARRAY OF CELL-BY-CELL NET RECHARGE FLOW RATE TO ASCII FILE
      !       (FOR EACH TIME STEP)
      !
      !4A1----WRITE HEADER TO ASCII FILE.
      IF(FMPOUT%FNRCH_ARRAY%IS_OPEN) THEN
          !
          WRITE(FMPOUT%FNRCH_ARRAY%IU,"(//,1X,A,'   PERIOD',I3,'   STEP',I3,',',I4,' COLUMNS,',1I4,' ROWS,',I3,' LAYERS',7X,'ELAPSED TIME',A14,1X,A14,/)") TEXT,KPER,KSTP,NCOL,NROW,NLAY,TIME,TIMEUNIT
          !
          !4A2----WRITE DATA AS ONE OR TWO RECORDS CONTAINING ONE VALUE PER LAYER.
          !
          IF(NLAY.EQ.1) THEN
              !
              DO I=1,NROW
                        WRITE(FMPOUT%FNRCH_ARRAY%IU,'(*(ES15.7))') BUFF(:,I,1)  !4A2A---WRITE ONE RECORD OF FLOW VALUES IF ONLY ONE LAYER
              ENDDO
          ELSE
              !4A2B---WRITE TWO RECORDS WHEN MULTIPLE LAYERS RECEIVE NET-RECHARGE.
              !       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS FLOW VALUES).
              !
              DO I=1,NROW
                        WRITE(FMPOUT%FNRCH_ARRAY%IU,FMT='(*(I12))') IRCH(:,I)
              ENDDO
              !
              DO I=1,NROW
                        WRITE(FMPOUT%FNRCH_ARRAY%IU,'(*(ES15.7))') ( BUFF(J,I,IRCH(J,I)), J=1, NCOL )
              ENDDO
          ENDIF
      ENDIF
      !
      !4B---- PRINT CUMULATIVE FARM NET RECHARGE FLOW RATE FOR EACH FARM TO ASCII FILE
      !       (TO BUILD TIME SERIES FOR FARM WELLS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM WELL)
      !       (FOR EACH TIME STEP)
      !
      !4B1----WRITE HEADER TO ASCII FILE
      IF(FMPOUT%FNRCH_LIST%IS_OPEN) THEN
          DO NF=1,WBS%NFARM
                  WRITE(FMPOUT%FNRCH_LIST%IU,'(2I5,A14,I11,G17.8)') KPER,KSTP,TIME,IFA(NF), WBS%TOT_FNRCH(NF)
          ENDDO
      ENDIF      
      !
      !6B-----CELL-BY-CELL NET RECHARGE FLOW RATES WILL BE SAVED AS 2-D ARRAY IN A BINARY FILE, IF
      !       "COMPACT BUDGET" IS SPECIFIED IN OUTPUT CONTROL.
      IF(FMPOUT%FNR_CBC.GT.3.AND.IBD.EQ.1) THEN
                                      CALL UBUDSV(KSTP,KPER,TEXT,FMPOUT%FNR_CBC,BUFF,NCOL,NROW,NLAY,WBS%IOUT)
      ELSEIF(IBD.EQ.2) THEN
                          !6B1----WRITE ONE RECORD OF FLOW VALUES IF ONLY ONE LAYER
                          IF(NLAY.EQ.1) THEN
                            NOPT=1
                          ELSE
                            !
                            !6B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS RECEIVE NET-RECHARGE.
                            !       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS FLOW VALUES).
                            NOPT=2
                          ENDIF
                          !     
                          CALL UBDSV3(KSTP,KPER,TEXT,FMPOUT%FNR_CBC,BUFF,IRCH,NOPT,NCOL,NROW,NLAY,WBS%IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
      !
      !7===== UPDATE VOLUMETRIC BUDGET FOR FARM NET RECHARGE ======================================================
      !
      ROUT=SNGL(RATOUT)
      RIN=SNGL(RATIN)
      VBVL(4,MSUM)=ROUT
      VBVL(3,MSUM)=RIN
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      !
      !7C-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS_OT.
      VBNM(MSUM)=TEXT
      !
      !7D-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
      !
  END SUBROUTINE
  !
  !
  SUBROUTINE FMP3ETPRT(KSTP,KPER,IGRID)                             !seb NEW PRINT OUTPUT SUBROUTINE
    !     ******************************************************************
    !     PRINT TOTAL EVAPORATION AND TRANSPIRATION FOR MODEL CELLS
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE CONSTANTS, ONLY: DZ
    USE FMP_GLOBAL,    ONLY:IFA,FMPOUT,FMP_LGR_PNT, WBS, CRP=>FCROP
    USE FMPBLK,       ONLY:TPL,TPU,ZERO,ZER
    USE GLOBAL,       ONLY:NCOL,NROW,NLAY,IBOUND,ITMUNI,HNEW, BOTM
    USE GWFBASMODULE, ONLY:TOTIM
    !
    INTEGER, INTENT(IN):: KSTP,KPER,IGRID
    !
    CHARACTER(12):: FMT
    CHARACTER(39):: TEXTET,TEXTETS
    CHARACTER(14):: TIMEUNIT
    CHARACTER(14):: TIME
    DATA TEXTET  /'EVAPORATION AND TRANSIPIRATION TOTALS  '/
    DATA TEXTETS /'EVAPORATION AND TRANSIPIRATION COMBINED'/
    INTEGER:: IRCH(NCOL,NROW)
    INTEGER:: I,IL,IR,IC,J,NF
    DOUBLE PRECISION:: EVAP,TRAN
    !
    FMT='(*(G15.6))'                                                  !SET NUMBER FORMAT  *(Gw.d) means format as many columns as required  CHANGED FROM G12 TO G15 TO PREVENT OVERWRITING
    !
    CALL FMP_LGR_PNT(IGRID)
    !
    ! OUTPUT NOT REQUESTED RETURN TO MAIN FILE
    !
    IF(.NOT.FMPOUT%ET_ARRAY_SUM%IS_OPEN .AND. .NOT.FMPOUT%ET_ARRAY_SEP%IS_OPEN .AND. .NOT.FMPOUT%ET_LIST%IS_OPEN) RETURN
    !
    !1===== DEFINE TIME UNIT LABEL FOR "LABEL HEADER" ==========================================================
    IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
    IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
    IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
    IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
    IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
    IF(TOTIM.GE.TPU.OR.TOTIM.GT.TPL) THEN
       WRITE(TIME,'(ES14.7)') TOTIM
    ELSE
       WRITE(TIME,'(F14.2)') TOTIM
    END IF
    !
    !2B-----LOOK FOR HIGHEST LAYER THAT ETOT AND TTOT ORIGINATE FROM
    IF (NLAY.GT.1)THEN    !if only 1 layer no need to find top layer
        ROW_LP: DO IR=1,NROW
        COL_LP: DO IC=1,NCOL
                !
                !2B1----LOOP THROUGH CELLS IN A VERTICAL COLUMN TO FIND WHERE TO PLACE FARM NET RECHARGE.
                IRCH(IC,IR)=1
                LAY_LP: DO IL=1,NLAY
                                !2B2----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
                                IF(IBOUND(IC,IR,IL).LT.0) EXIT LAY_LP
                                !
                                !2B3----IF CELL IS INACTIVE MOVE DOWN TO NEXT CELL.
                                IF(IBOUND(IC,IR,IL).EQ.0 .OR. HNEW(IC,IR,IL).LE.BOTM(IC,IR,IL))  CYCLE LAY_LP
                                IRCH(IC,IR)=IL
                                EXIT LAY_LP
                END DO LAY_LP
        END DO COL_LP
        END DO ROW_LP
    END IF
    !
    !3===== PRINT 2D-ARRAY OF CELL-BY-CELL COMBINED EVAPORATION AND TRANSPIRATION  ===============================
    !
    !3A-----WRITE HEADER
    IF(FMPOUT%ET_ARRAY_SUM%IS_OPEN) THEN
        WRITE(FMPOUT%ET_ARRAY_SUM%IU,"(//,1X,A,'   PERIOD',I5,'   STEP',I5,',',I5,' COLUMNS,',1I5,' ROWS,',I3,' LAYERS',7X,'ELAPSED TIME',A14,1X,A14,/)") TEXTETS,KPER,KSTP,NCOL,NROW,NLAY,TIME,TIMEUNIT
        !
        !3B-----WRITE DATA AS TWO OR THREE RECORDS CONTAINING ONE VALUE PER LAYER.
        IF(NLAY.EQ.1) THEN
           !
           !-------WRITE ONE RECORD OF EVAPORATION+TRANSPIRATION VALUES IF ONLY ONE LAYER
           DO I=1,NROW
             WRITE(FMPOUT%ET_ARRAY_SUM%IU,FMT) (CRP%ETOT(J,I)+CRP%TTOT(J,I),J=1,NCOL)!CRP=>FCROP TO SAVE ON SPACE
           END DO
        ELSE
           !3B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS HAVE ET.
           !       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS EVAPORATION+TRANSPIRATION).
           DO I=1,NROW
                  WRITE(FMPOUT%ET_ARRAY_SUM%IU,FMT)(IRCH(J,I),J=1,NCOL)
           END DO
           !-------WRITE ONE RECORD OF EVAPORATION+TRANSPIRATION VALUES IF ONLY ONE LAYER
           DO I=1,NROW
                  WRITE(FMPOUT%ET_ARRAY_SUM%IU,FMT) (CRP%ETOT(J,I)+CRP%TTOT(J,I),J=1,NCOL)!CRP=>FCROP TO SAVE ON SPACE
           END DO
           !
        END IF
    END IF
    !3===== PRINT 2D-ARRAY OF CELL-BY-CELL SEPARATE EVAPORATION AND TRANSPIRATION  ===============================
    !
    !3A-----WRITE HEADER
    IF(FMPOUT%ET_ARRAY_SEP%IS_OPEN) THEN
        WRITE(FMPOUT%ET_ARRAY_SEP%IU,"(//,1X,A,'   PERIOD',I5,'   STEP',I5,',',I5,' COLUMNS,',1I5,' ROWS,',I3,' LAYERS',7X,'ELAPSED TIME',A14,1X,A14,/)") TEXTET,KPER,KSTP,NCOL,NROW,NLAY,TIME,TIMEUNIT
        !
        !3B-----WRITE DATA AS TWO OR THREE RECORDS CONTAINING ONE VALUE PER LAYER.
        IF(NLAY.EQ.1) THEN
           !-------WRITE ONE RECORD OF EVAPORATION VALUES IF ONLY ONE LAYER
           DO I=1,NROW
             WRITE(FMPOUT%ET_ARRAY_SEP%IU,FMT)(CRP%ETOT(J,I),J=1,NCOL)!CRP=>FCROP TO SAVE ON SPACE
           END DO
           !------WRITE ONE RECORD OF TRANSIPIRATION VALUES IF ONLY ONE LAYER
           DO I=1,NROW
             WRITE(FMPOUT%ET_ARRAY_SEP%IU,FMT)(CRP%TTOT(J,I),J=1,NCOL)!CRP=>FCROP TO SAVE ON SPACE
           END DO
        ELSE
           !3B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS HAVE ET.
           !       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS EVAPORATION; THIRD RECORD HAS TRANSPIRATION).
           DO I=1,NROW
             WRITE(FMPOUT%ET_ARRAY_SEP%IU,FMT)(IRCH(J,I),J=1,NCOL)
           END DO
           !-------WRITE ONE RECORD OF EVAPORATION VALUES IF ONLY ONE LAYER
           DO I=1,NROW
             WRITE(FMPOUT%ET_ARRAY_SEP%IU,FMT)(CRP%ETOT(J,I),J=1,NCOL)!CRP=>FCROP TO SAVE ON SPACE
           END DO
           !------WRITE ONE RECORD OF TRANSIPIRATION VALUES IF ONLY ONE LAYER
           DO I=1,NROW
             WRITE(FMPOUT%ET_ARRAY_SEP%IU,FMT)(CRP%TTOT(J,I),J=1,NCOL)!CRP=>FCROP TO SAVE ON SPACE
           END DO
        END IF
    END IF
    !
    !4B---- PRINT CUMULATIVE EVAPOTRANSPIRATION FLOW RATE FOR EACH FARM TO ASCII FILE
    !       (TO BUILD TIME SERIES FOR FARM WELLS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM WELL)
    !       (FOR EACH TIME STEP)
    !
    !4B1----WRITE HEADER TO ASCII FILE
    IF(FMPOUT%ET_LIST%IS_OPEN) THEN
        !
        !4B2----WRITE LIST OF CUMULATIVE EVAPORATION AND TRANSPIRATION FOR EACH FARM
        DO NF=1,WBS%NFARM
                      EVAP = DZ
                      TRAN = DZ        
                      DO IR=1,NROW
                      DO IC=1,NCOL
                              IF(WBS%FID_ARRAY(IC,IR).EQ.IFA(NF)) THEN
                                                                   EVAP=EVAP+CRP%ETOT(IC,IR)!CRP=>FCROP TO SAVE ON SPACE
                                                                   TRAN=TRAN+CRP%TTOT(IC,IR)           
                              ENDIF
                      ENDDO
                      ENDDO
                      WRITE(FMPOUT%ET_LIST%IU,'(2I5,A14,I11,3G23.8)') KPER,KSTP,TIME,IFA(NF), EVAP,TRAN,EVAP+TRAN   
        END DO
    END IF 
    !
  END SUBROUTINE
  !
END MODULE FMP_MAIN_DRIVER
!
!  Wolfgang LGR Subroutines - Unsure how they were used -- not called anywhere
!
MODULE FMP_LGR_LINK
  !
  IMPLICIT NONE
  !
  CONTAINS
  !
  SUBROUTINE FMP3PRTOCH_BILINEAR(P,F)
    USE CONSTANTS, ONLY: DZ
    !
    USE GLOBAL,      ONLY:NCOL,NROW,GLOBALDAT
    USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NCPP,NPL,NCPPL
    INTEGER:: X,Y
    DOUBLE PRECISION, DIMENSION(GLOBALDAT(1)%NCOL,GLOBALDAT(1)%NROW):: P
    DOUBLE PRECISION, DIMENSION(NCOL,NROW):: F
    !
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE:: R
    !
    INTEGER:: ICEND, JCEND, KCEND, NROWEXT, NCOLEXT, KCLAY, KPLAY
    INTEGER:: ICR, IPR, JCC, JPC, IM1, JM1, KM1, IR, IC
    !
    !-----IF PARENT ARRAY IS CONSTANT, RETURN CONSTANT FOR CHILD ARRAY
    IF(MAXVAL(P).EQ.MINVAL(P)) THEN
      F=MAXVAL(P)
      RETURN
    ENDIF
    !
    ICEND=NCPP
    JCEND=NCPP     
    NROWEXT=NROW+2*ICEND
    NCOLEXT=NCOL+2*JCEND
    ALLOCATE(R(NCOLEXT,NROWEXT))
    R = DZ
    F = DZ
    KCLAY=0
    DO KPLAY = 1,NPL
      KCEND=NCPPL(KPLAY)       
      ICR=0
      DO IPR = NPRBEG-1,NPREND+1
        JCC=0
        DO JPC = NPCBEG-1,NPCEND+1
          KM1=INT(KCEND/2)+1
          IM1=INT(ICEND/2)+1
          JM1=INT(JCEND/2)+1
          DO Y=1,ICEND
          DO X=1,JCEND
            IF(JCC+X.EQ.JCC+JM1.AND.ICR+Y.EQ.ICR+IM1) THEN
              R(JCC+X,ICR+Y)=P(JPC,IPR)
            ENDIF
            IF(JCC+X.EQ.JCC+JM1) THEN
              IF(ICR+Y.LT.ICR+IM1) THEN
                R(JCC+X,ICR+Y)=(P(JPC,IPR)-P(JPC,IPR-1))/ICEND*(IM1-1+Y)+P(JPC,IPR-1)
              ENDIF
              IF(ICR+Y.GT.ICR+IM1) THEN
                R(JCC+X,ICR+Y)=(P(JPC,IPR+1)-P(JPC,IPR))/ICEND*(Y-IM1)+P(JPC,IPR)
              ENDIF
            ENDIF
            IF(ICR+Y.EQ.ICR+IM1) THEN
              IF(JCC+X.LT.JCC+JM1) THEN
                R(JCC+X,ICR+Y)=(P(JPC,IPR)-P(JPC-1,IPR))/JCEND*(JM1-1+X)+P(JPC-1,IPR)
              ENDIF
              IF(JCC+X.GT.JCC+JM1) THEN
                R(JCC+X,ICR+Y)=(P(JPC+1,IPR)-P(JPC,IPR))/JCEND*(X-JM1)+P(JPC,IPR)
              ENDIF
            ENDIF
          ENDDO
          ENDDO
          JCC=JCC+JCEND
        ENDDO 
        ICR=ICR+ICEND
      ENDDO
      KCLAY=KCLAY+KCEND
    ENDDO
    !
    !      DO IR=1,NROWEXT
    !      WRITE(1239,'(<NCOLEXT>F15.7)') (R(IC,IR),IC=1,NCOLEXT)
    !      ENDDO
    !      WRITE(1239,'(/)')      
    !
    ICR=0
    DO WHILE (ICR.LE.NROWEXT)
      JCC=0
      DO WHILE (JCC.LE.NCOLEXT)
        IM1=INT(ICEND/2)+1
        JM1=INT(JCEND/2)+1
          IF(JCC.GE.JCEND.AND.ICR.GE.ICEND.AND.JCC.LT.NCOLEXT-JCEND.AND.ICR.LT.NROWEXT-ICEND) THEN
          DO Y=1,ICEND
          DO X=1,JCEND
            IF(JCC+X.LT.JCC+JM1.OR.JCC+X.GT.JCC+JM1) THEN
              IF(ICR+Y.LT.ICR+IM1) THEN                
                R(JCC+X,ICR+Y)= (R(JCC+X,ICR+IM1)-R(JCC+X,ICR-ICEND+IM1)) / ICEND*(IM1-1+Y)+R(JCC+X,ICR-ICEND+IM1)
              ENDIF
              IF(ICR+Y.GT.ICR+IM1) THEN
                R(JCC+X,ICR+Y)= (R(JCC+X,ICR+ICEND+IM1)-R(JCC+X,ICR+IM1)) / ICEND*(Y-IM1)+R(JCC+X,ICR+IM1)
              ENDIF
            ENDIF
          ENDDO
          ENDDO
          ENDIF
        JCC=JCC+JCEND
      ENDDO 
      ICR=ICR+ICEND
    ENDDO
    !
    !      DO IR=1,NROWEXT
    !      WRITE(1239,'(<NCOLEXT>F15.7)') (R(IC,IR),IC=1,NCOLEXT)
    !      ENDDO
    !      WRITE(1239,'(/)')      
    !            
    DO IR=1,NROWEXT
    DO IC=1,NCOLEXT
    IF(IC.GT.JCEND.AND.IR.GT.ICEND.AND.IC.LE.NCOLEXT-JCEND.AND.IR.LE.NROWEXT-ICEND) THEN
       F(IC-JCEND,IR-ICEND)=R(IC,IR)
    ENDIF
    ENDDO
    ENDDO
    !
    !      DO IR=1,NROW
    !      WRITE(1239,'(<NCOL>F15.7)') (F(IC,IR),IC=1,NCOL)
    !      ENDDO
    !      WRITE(1239,'(/)') 
    !
    DEALLOCATE(R)
    !
  END SUBROUTINE
  !
  SUBROUTINE FMP3PRTOCH(ID)
      !
      !   SPECIFICATIONS:
      !   WHEN 'P' FLAG OCCURS PULL FROM PARENT MODEL PROPERTIES FROM ID
      !        ID = 1 IS IFA
      !        ID = 2 IS ISA
      !        ID = 3 IS ICA
      !
      USE FMP_GLOBAL,   ONLY:WBS,FCROP,SOIL,FMPDAT,IFA,ISA,ICA
      USE FMP_GLOBAL,   ONLY:IFA,ISA,ICA
      USE GLOBAL,      ONLY:NCOL,NROW
      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NCPP,NPL,NCPPL,IBFLG,ICBOUND
      INTEGER, INTENT(IN):: ID
      !
      INTEGER:: I, J, K, IM1, JM1, KM1
      INTEGER:: IR, IC
      INTEGER:: ICEND, JCEND, KCEND
      INTEGER:: KCLAY, KPLAY
      INTEGER:: IPR, ICR, JCC, JPC
      INTEGER:: NCF, NPF, NCS, NPS, NCC, NPC
      !
      ICEND=NCPP
      JCEND=NCPP     
      KCLAY=1
      DO KPLAY = 1,NPL
        KCEND=NCPPL(KPLAY)       
        ICR=1
        DO IPR = NPRBEG,NPREND
          JCC=1
          DO JPC = NPCBEG,NPCEND
            DO K=1,KCEND
              KM1=K-1
              DO I=1,ICEND
                IM1=I-1
                DO J=1,JCEND
                  JM1=J-1
                  IF(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1) .GT. 0 .OR. ABS(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1)).EQ.IBFLG)THEN
                      IF(ID.EQ.1) WBS%FID_ARRAY(JCC+JM1,ICR+IM1)=FMPDAT(1)%WBS%FID_ARRAY(JPC,IPR)
                      IF(ID.EQ.2) SOIL%SID(JCC+JM1,ICR+IM1)=FMPDAT(1)%SOIL%SID(JPC,IPR)
                      IF(ID.EQ.3) FCROP%CRPID(JCC+JM1,ICR+IM1)=FMPDAT(1)%FCROP%CRPID(JPC,IPR)
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
            JCC=JCC+JCEND
          ENDDO 
          ICR=ICR+ICEND
        ENDDO
        KCLAY=KCLAY+KCEND
      ENDDO
      !
      IF(ID.EQ.1) THEN
      NCF=0
      IFA=0
      DO NPF=1,FMPDAT(1)%WBS%NFARM
      DO IR=1,NROW
      DO IC=1,NCOL
        IF(NPF.EQ.WBS%FID_ARRAY(IC,IR).AND.NPF.GT.NCF) THEN
          IFA(NPF)=WBS%FID_ARRAY(IC,IR)
          NCF=NPF
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
      !      
      IF(ID.EQ.2) THEN
      NCS=0
      ISA=0
      DO NPS=1,FMPDAT(1)%FDIM%NSOIL
      DO IR=1,NROW
      DO IC=1,NCOL
        IF(NPS.EQ.SOIL%SID(IC,IR).AND.NPS.GT.NCS) THEN
          ISA(NPS)=SOIL%SID(IC,IR)
          NCS=NPS
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
      !      
      IF(ID.EQ.3) THEN
      NCC=0
      ICA=0
      DO NPC=1,FMPDAT(1)%FDIM%NCROP
      DO IR=1,NROW
      DO IC=1,NCOL
        IF(NPC.EQ.FCROP%CRPID(IC,IR).AND.NPC.GT.NCC) THEN
          ICA(NPC)=FCROP%CRPID(IC,IR)
          NCC=NPC
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
      !      
      !      DO IR=1,NROW
      !        IF(ID.EQ.1) WRITE(1256,'(12I7)') (IFID(IC,IR),IC=1,NCOL)
      !        IF(ID.EQ.2) WRITE(1256,'(12I7)') (ISID(IC,IR),IC=1,NCOL)      
      !        IF(ID.EQ.3) WRITE(1256,'(12I7)') (ICID(IC,IR),IC=1,NCOL)      
      !      ENDDO
      !
      !
  END SUBROUTINE
!      
!C     ******************************************************************
!      SUBROUTINE FMP3PWELTOCWEL(ID,MM,IBEG,IPFWEL,IUNITMNW2)
!C     ******************************************************************
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      USE FMP_GLOBAL,   ONLY:MXPFW,NNPFWL,FWELL,FMPDAT,NFWLVL,MXFWEL,NAUX
!     1                       ,FWLAUX,FWLAUXORDER,AUXV,MNW2NAM,AUXIDX
!      USE GLOBAL,      ONLY:IOUT
!      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NPLEND,NCPP,NPL,
!     1                      NCPPL,IBFLG,ICBOUND
!C     ------------------------------------------------------------------
!C
!      IF(ID.EQ.1) NPFWELS=FMPDAT(1)%NNPFWL
!      IF(ID.EQ.2) NPFWELS=FMPDAT(1)%MXFWEL      
!      M=MM
!      DO L=1,NPFWELS       
!      ICEND=NCPP
!      JCEND=NCPP     
!      KCLAY=0
!      DO KPLAY = 1,NPL
!        KCEND=NCPPL(KPLAY)       
!        ICR=0
!        DO IPR = NPRBEG,NPREND
!          JCC=0
!          DO JPC = NPCBEG,NPCEND
!C            DO K=1,KCEND
!              KM1=INT(KCEND/2)+1
!C              DO I=1,ICEND
!                IM1=INT(ICEND/2)+1
!C                DO J=1,JCEND
!                  JM1=INT(JCEND/2)+1
!                  IF(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1) .GT. 0.OR.
!     +            ABS(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1)).EQ.IBFLG)THEN
!                     IF(FMPDAT(1)%FWELL(1,L).EQ.KPLAY.AND.
!     1                  FMPDAT(1)%FWELL(2,L).EQ.IPR.AND.
!     2                  FMPDAT(1)%FWELL(3,L).EQ.JPC) THEN
!C
!C                       EXCLUDE PARENT WELLS FROM BEING USED FOR A CHILD MODEL FARM, IF EVEN ONE WELL IS SPECIFIED FOR THAT CHILD MODEL FARM
!                        DO N=IBEG,MM-1
!                         IF(FMPDAT(1)%FWELL(5,L).EQ.FWELL(5,N)) GOTO 206
!                        ENDDO
!C                        
!                        FWELL(1,M)=KCLAY+KM1
!                        FWELL(2,M)=ICR+IM1
!                        FWELL(3,M)=JCC+JM1
!                        DO N=4,FMPDAT(1)%NFWLVL
!                          FWELL(N,M)=FMPDAT(1)%FWELL(N,L)
!C                          WRITE(1234,*) FWELL(N,M),FMPDAT(1)%FWELL(N,L)
!                        ENDDO
!                        AUXV(:,M)=FMPDAT(1)%AUXV(:,M)                   !TRANSFER AUX VARIABLES
!                        IF(IUNITMNW2.NE.0)THEN
!                          MNW2NAM(M)=FMPDAT(1)%MNW2NAM(M)               !TRANSFER MNW2 LINKED NAMES
!                        END IF
!                        !
!                        IF(FWLAUXORDER(3).EQ."LGRGRID") THEN
!                          FWELL(AUXIDX(3),M)=1                                  !LGR FLAG ON, SET FWELL(9,M) TO 1 SCOTT THIS SEEMS TO DO NOTHING
!                          AUXV(3,M)=1
!                        END IF
!C                        
!C                       FOR NON-PARAMETER WELLS PULLED FROM NON-PAR. PARENT WELLS OR PARAMETER WELLS PULLED FORM PARAMETER PARENT WELLS,
!C                       APPEND INFO TO LIST IN LIST FILE
!                        IF(ID.EQ.1) NN=M
!                        IF(ID.EQ.2) NN=M-MM+2
!                        IF(LSTCHK(3)) THEN
!                         IF(IPFWEL.EQ.1 .AND. IUNITMNW2.EQ.0)    THEN    !ADDED IPFWEL AND MNW2 UNIT CHECKS FOR PRINT OUT seb
!                           WRITE(IOUT,204) NN,(IDINT(FWELL(N,M)),N=1,5),
!     1                      FWELL(6,M),(AUXV(N,M),N=1,NAUX)
!                         ELSEIF(IPFWEL.EQ.1 .AND. IUNITMNW2.NE.0)THEN
!                           WRITE(IOUT,205) NN,(IDINT(FWELL(N,M)),N=1,5),
!     1                      FWELL(6,M),MNW2NAM(M),(AUXV(N,M),N=1,NAUX)
!                         ENDIF
! 204                     FORMAT(1X,4I7,2I11,ES16.4,1x,5I5)
! 205                     FORMAT(1X,4I7,2I11,ES16.4,2x,A,1x,5I5) 
!                        END IF
!                     M=M+1
!                     ENDIF
!                  ENDIF
!C                ENDDO
!C              ENDDO
!C            ENDDO
!            JCC=JCC+JCEND
! 206        ENDDO 
!          ICR=ICR+ICEND
!        ENDDO
!        KCLAY=KCLAY+KCEND
!      ENDDO
!      ENDDO
!      MM=M
!C      DO M=1,MXFWEL
!C       WRITE(1234,'(10F10.3)') (FWELL(N,M),N=1,NFWLVL)
!C      ENDDO
!C
!      RETURN
!      END SUBROUTINE
!
!     ******************************************************************
!     ******************************************************************
!     ******************************************************************
    END MODULE FMP_LGR_LINK
    
    
    
    
    
! OLD FULLY-ROUTED RETURN SEARCH ALGORITHM

!
!
!!!!      SUBROUTINE RISE_RUN()
!!!!C-----VERSION 2.01 07/28/2010 RISERUN
!!!!C     ******************************************************************
!!!!C     CALCULATE SLOPE AS RISE-RUN TO RELATE FIESWP AND FIESWI IN FM TO SLOPE.
!!!!C     METHOD: THIRD-ORDER FINITE DIFFERENCE ESTIMATOR USING ALL 8 OUTHER 
!!!!C     POINTS OF NINE ELEVATION POINTS (3X3 WINDOW) - AFTER HORNE (1981).
!!!!C     EDGES: 6 ELEVATION POINTS (3X2 WINDOW).
!!!!C     CORNERS: 4 ELEVATION POINTS (2X2 WINDOW).
!!!!C     ******************************************************************
!!!!C        SPECIFICATIONS:
!!!!C     ------------------------------------------------------------------
!!!!      USE FMP_GLOBAL,    ONLY:WBS,RISERUN
!!!!      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,DELR,DELC
!!!!      IMPLICIT NONE
!!!!C     ------------------------------------------------------------------
!!!!C        ARGUMENTS:
!!!!C     ------------------------------------------------------------------
!!!!C
!!!!C     ------------------------------------------------------------------
!!!!C        LOCAL VARIABLES:
!!!!c     ------------------------------------------------------------------
!!!!      INTEGER IR,IC,IR1,IR2,IR3,IC1,IC2,IC3,IMX,IMY,ID
!!!!      DOUBLE PRECISION DZDX,DZDY
!!!!C     ------------------------------------------------------------------
!!!!      DO IR=1,NROW
!!!!      DO IC=1,NCOL
!!!!      IR1=0
!!!!      IR2=0
!!!!      IR3=0
!!!!      IC1=0
!!!!      IC3=0
!!!!      IC2=0
!!!!      IMX=0
!!!!      IMY=0
!!!!      ID=0 
!!!!C1------CORNERS
!!!!      if(ir.eq.1.or.ic.eq.1.or.ir.eq.nrow.or.ic.eq.ncol) then
!!!!      if(ir.eq.1.and.ic.eq.1) then
!!!!      ic1=ic
!!!!      ic3=ic+1
!!!!      ir1=ir
!!!!      ir3=ir+1
!!!!      elseif(ir.eq.1.and.ic.eq.ncol) then
!!!!      ic1=ic-1
!!!!      ic3=ic
!!!!      ir1=ir
!!!!      ir3=ir+1
!!!!      elseif(ir.eq.nrow.and.ic.eq.1) then
!!!!      ic1=ic
!!!!      ic3=ic+1
!!!!      ir1=ir-1
!!!!      ir3=ir
!!!!      elseif(ir.eq.nrow.and.ic.eq.ncol) then
!!!!      ic1=ic-1
!!!!      ic3=ic
!!!!      ir1=ir-1
!!!!      ir3=ir
!!!!      endif
!!!!      ic2=1
!!!!      ir2=1
!!!!      imx=0
!!!!      imy=0
!!!!      id=2
!!!!      endif
!!!!c2------EDGES
!!!!      if(ir.eq.1.and.(ic.ne.1.and.ic.ne.ncol)) then
!!!!      ic1=ic-1
!!!!      ic2=ic
!!!!      ic3=ic+1
!!!!      ir1=ir
!!!!      ir2=1
!!!!      ir3=ir+1
!!!!      imx=0
!!!!      imy=2
!!!!      id=4
!!!!      elseif(ir.eq.nrow.and.(ic.ne.1.and.ic.ne.ncol)) then
!!!!      ic1=ic-1
!!!!      ic2=ic
!!!!      ic3=ic+1
!!!!      ir1=ir-1
!!!!      ir2=1
!!!!      ir3=ir
!!!!      imx=0
!!!!      imy=2
!!!!      id=4
!!!!      elseif(ic.eq.1.and.(ir.ne.1.and.ir.ne.nrow)) then
!!!!      ic1=ic
!!!!      ic2=1
!!!!      ic3=ic+1
!!!!      ir1=ir-1
!!!!      ir2=ir
!!!!      ir3=ir+1
!!!!      imx=2
!!!!      imy=0
!!!!      id=4
!!!!      elseif(ic.eq.ncol.and.(ir.ne.1.and.ir.ne.nrow)) then
!!!!      ic1=ic-1
!!!!      ic2=1
!!!!      ic3=ic
!!!!      ir1=ir-1
!!!!      ir2=ir
!!!!      ir3=ir+1
!!!!      imx=2
!!!!      imy=0
!!!!      id=4
!!!!C3------GENERAL
!!!!      elseif(ic.ne.1.and.ir.ne.1.and.ic.ne.ncol.and.ir.ne.nrow) then
!!!!      ic1=ic-1
!!!!      ic2=ic
!!!!      ic3=ic+1
!!!!      ir1=ir-1
!!!!      ir2=ir
!!!!      ir3=ir+1
!!!!      imx=2
!!!!      imy=2
!!!!      id=8
!!!!      endif
!!!!C4------THIRD ORDER FINITE DIFFERENCE ESTIMATOR
!!!!      DZDX=WBS%GSE(IC1,IR1)+DBLE(IMX)*WBS%GSE(IC1,IR2)+WBS%GSE(IC1,IR3)
!!!!     1   -(WBS%GSE(IC3,IR1)+DBLE(IMX)*WBS%GSE(IC3,IR2)+WBS%GSE(IC3,IR3))
!!!!      DZDY=WBS%GSE(IC1,IR1)+DBLE(IMY)*WBS%GSE(IC2,IR1)+WBS%GSE(IC3,IR1)
!!!!     1   -(WBS%GSE(IC1,IR3)+DBLE(IMY)*WBS%GSE(IC2,IR3)+WBS%GSE(IC3,IR3))
!!!!      DZDX=DZDX/(DBLE(ID)*DBLE(DELR(IC)))
!!!!      DZDY=DZDY/(DBLE(ID)*DBLE(DELC(IR)))
!!!!      RISERUN(IC,IR)=DSQRT(DZDX**2+DZDY**2)                             !RISE-RUN SLOPE IN PERCENT
!!!!      ENDDO
!!!!      ENDDO
!!!!C
!!!!      RETURN
!!!!      END SUBROUTINE
!
!!!      SUBROUTINE RETURNFLOW(NF,IROUT)
!!!C-----VERSION 2.01 07//28/2010 LOWESTCELL_NEARESTREACH
!!!C     ******************************************************************
!!!C     IF DRAIN IS NOT LOCATED WITHIN A FARM AND IF NO RETURNFLOW REACH IS SPECIFIED,
!!!C     DETERMINE CELL WITH LOWEST ELEVATION IN A FARM, AND
!!!C     CLOSEST DRAIN REACH (IRRFL.EQ.1) OR CLOSEST "ANY-TYPE" REACH (IRRFL=-1) FROM THAT CELL,
!!!C     AND DEFINE THAT REACH TO BE A "FARM DRAIN REACH."
!!!C     (RECHARGE CUMULATIVE RUNOFF INTO THAT DRAIN REACH IN FMP_FM-C10B1)
!!!C     ******************************************************************
!!!C        SPECIFICATIONS:
!!!C     -------------------------------------------------------------------
!!!      USE CONSTANTS
!!!      USE FMP_GLOBAL, ONLY:IRTPFL,FDSEGL,ISRRFL,IRRFL,ISRR,IFDRID,
!!!     1                    SFR_RUNOFF,WBS,LFID, H2ORETURN,WBS,FMPOUT,SWFL
!!!      USE GLOBAL, ONLY:NCOL,NROW,DELR,DELC
!!!      USE GWFSFRMODULE, ONLY: NSTRM,IDIVAR,ISTRM,STRM,SEG,SEG_NSTRM
!!!      IMPLICIT NONE
!!!C     -------------------------------------------------------------------
!!!C        ARGUMENTS:
!!!C     -------------------------------------------------------------------
!!!      INTEGER NF,IROUT
!!!C     -------------------------------------------------------------------
!!!C        LOCAL VARIABLES:
!!!      INTEGER LOLD2,NRCH,ICNT,L,IR,IC,NR,NC,N,IA,IB, I
!!!      DOUBLE PRECISION FDRAINSEGL,GSURFOLD,COLD,COUNT,ASUM,BSUM,C
!!!C     -------------------------------------------------------------------
!!!      IF(.NOT. LFID(NF)) RETURN !DO NOT PROCESS RETURN FLOW FOR INACTIVE FARMS  --REDUNDANT CHECK, BUT KEPT INCASED CALLED ELSEWHERE IN FMP
!!!      !IF(H2ORETURN(1,NF)==0 .AND. H2ORETURN(2,NF)==0) RETURN
!!!      
!!!C8D4----DETERMINE FULLY-ROUTED OR SEMI-ROUTED SURFACE-WATER RUNOFF-RETURNFLOWS:
!!!C       - DETERMINE DRAIN SEGEMENT LENGTH WITHIN A FARM.
!!!C         (STRM(12,L)=RUNOFF PER REACH, SEG(3,ISTRM(4,L))=RUNOFF PER SEGMENT, SEG(1,ISTRM(4,L))=SEGMENT LENGTH)
!!!C       - DEFINE REACHES WITHIN A FARM TO BE FARM DRAIN REACHES.
!!!C       - DETERMINE PREEXISTING RUNOFF PER DRAIN REACH FROM PRESPECIFIED RUNOFF PER SEGMENT.
!!!C       - LOCK PREEXISTING RUNOFF PER DRAIN REACH, SO THAT IT CAN'T BE OVERWRITTEN.
!!!C       - WRITE LIST OF FOUND DRAIN REACHES AND FARM DRAIN SEGMENT LENGTH TO LIST FILE.
!!!      IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!      WRITE(IROUT,
!!!     +           '(/," RETURNFLOWS:",/,2X,"FULLY-ROUTED RETURNFLOWS:")')
!!!      ENDIF
!!!      FDRAINSEGL=0.D0
!!!      IF(H2ORETURN(1,NF).NE.0 .AND. H2ORETURN(2,NF)==0) THEN
!!!!      IF( H2ORETURN(1,NF).NE.0  .AND. IRRFL.NE.0 .AND.
!!!!     +   (  ISRRFL.EQ.0.OR.(ISRRFL.GT.0.AND.ALL(ISRR(2:5,NF).EQ.0))  )
!!!!     +  ) THEN
!!!        IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!        IF(IRRFL.EQ.1) THEN
!!!        WRITE(IROUT,'(3X,"ACTIVATED SEARCH FOR REACHES OF ",
!!!     1               "NON-DIVERSION SEGMENTS THAT ARE WITHIN A FARM.")')
!!!        ELSEIF(IRRFL.EQ.-1) THEN
!!!        WRITE(IROUT,'(3X,"ACTIVATED SEARCH FOR REACHES OF ANY STREAM",
!!!     1  " SEGMENTS THAT ARE WITHIN A FARM.")')       
!!!        ENDIF
!!!        ENDIF
!!!        LOLD2=0
!!!        DO I=1,WBS%FID(NF)%Count                                        !seb removed double do loop NOTE THAT FOR INACTIVE FORMS FMLOC(NF)%Count=0 SO LOOP WILL NOT OCCUR
!!!          IR=WBS%FID(NF)%RC(ONE,I)
!!!          IC=WBS%FID(NF)%RC(TWO,I)
!!!        DO L=1,NSTRM
!!!          !DO IR=1,NROW
!!!          !DO IC=1,NCOL
!!!            !IF(IFID(IC,IR).EQ.NF.and.LFID(NF)) THEN                     !include test for returnflow only in active farm id's--rth
!!!             IF(( (IRRFL.EQ.1.AND.IDIVAR(1,ISTRM(4,L)).EQ.0).OR.
!!!     1            (IRRFL.EQ.-1) ).AND.
!!!     2             ISTRM(2,L).EQ.IR.AND.ISTRM(3,L).EQ.IC ) THEN
!!!              IF(L.NE.LOLD2) THEN                                       !don't count reaches twice if they are intercalated into an "inner" corner of the farm boundary!
!!!              FDRAINSEGL=FDRAINSEGL+DBLE(STRM(1,L))
!!!              IFDRID(NF,L)=NF                                           !CREATE ID OF REACHES WITHIN A FARM EQUAL TO ID OF FARM
!!!              STRM(12,L)=SEG(3,ISTRM(4,L))*(STRM(1,L)/SEG(1,ISTRM(4,L)))!RUNOFF PER REACH MUST BE RESET: OTHERWISE FOR ITMP<0 IN SFR (DATA REUSED FOR NEW STRESS PERIODS), THE RUNOFF PER REACH WOULD NOT BE OVERWRITTEN, AND THE LAST 'RUNOFF MINUS REACH DEL.'-VALUE WOULD FALSLY REPRESENT THE RUNOFF PER REACH VALUE FOR THE NEW STRESS PERIOD!
!!!              SFR_RUNOFF(L)=DBLE(STRM(12,L))                                !-------> !RENAME, SO THAT FM AT SECOND AND FOLLOWING ITERATIONS DOESN'T START UP WITH STRM(12,L)=STRM(12,L)+RDR(L), BUT WITH STRM(12,L)=RUNOFF(L)+RDR(L)
!!!              IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!              IF(LOLD2.EQ.0) WRITE(IROUT,220)                           !However note that reuse stress period (-1) in input file won't work. Each stress period's inflow must be specified separately, since if -1 is specified the last stored value (i.e. seg(2,n)) will be re-used.
!!!  220         FORMAT(3X,'FULLY ROUTED RUNOFF-RETURNFLOW PRORATED ',
!!!     1        'OVER REACHES WITHIN THE FARM AT:
!!!     2        ',/6X,'ROW',2X,'COLUMN',2X,'SEGMENT_NO.',2X,'REACH_NO.',
!!!     3        2X,'REACH_LENGTH')
!!!              WRITE(IROUT,221)ISTRM(2,L),ISTRM(3,L),ISTRM(4,L),
!!!     1        ISTRM(5,L),STRM(1,L)
!!!  221         FORMAT(3X,I6,I8,I12,I11,3X,G15.8)
!!!              ENDIF
!!!              ENDIF
!!!              LOLD2=L
!!!             ENDIF
!!!            !ENDIF
!!!          !ENDDO
!!!          ENDDO
!!!        ENDDO
!!!      ELSEIF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!        IF(IRRFL.EQ.1) THEN
!!!        WRITE(IROUT,'(3X,"DEACTIVATED SEARCH FOR REACHES OF ",
!!!     1  "NON-DIVERSION SEGMENTS THAT ARE WITHIN A FARM.")')
!!!        ELSEIF(IRRFL.EQ.-1) THEN
!!!        WRITE(IROUT,'(3X,"DEACTIVATED SEARCH FOR REACHES OF ANY STREAM "
!!!     1             ,"SEGMENTS THAT ARE WITHIN A FARM.")')        
!!!        ENDIF
!!!      ENDIF      
!!!      FDSEGL(NF)=FDRAINSEGL      
!!!      IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!      IF(FDSEGL(NF).GT.0.D0) THEN
!!!        WRITE(IROUT,'
!!!     1 (3X,"ACTIVE FARM DRAIN SEGMENT LENGTH: ",G15.8)') FDSEGL(NF)
!!!      ELSE
!!!        WRITE(IROUT,'
!!!     1  (3X,"NO ACTIVE FARM DRAIN REACHES ARE WITHIN ",
!!!     2  "THE FARM: NO FULLY-ROUTED RETURNFLOW POSSIBLE.")')
!!!      ENDIF
!!!      ENDIF
!!!C
!!!C8D5----IF DRAIN IS NOT LOCATED WITHIN A FARM AND IF NO RETURNFLOW REACH IS SPECIFIED,
!!!C       DETERMINE CELL WITH LOWEST ELEVATION IN A FARM, AND
!!!C       CLOSEST DRAIN REACH (IRRFL.EQ.1) OR CLOSEST "ANY-TYPE" REACH (IRRFL=-1) FROM THAT CELL,
!!!C       AND DEFINE THAT REACH TO BE A "FARM DRAIN REACH."
!!!C       (RECHARGE CUMULATIVE RUNOFF INTO THAT DRAIN REACH IN FMP_FM-C10B1)
!!!C       (FMP_RP-C8D5 WAS FORMERLY (PRIOR TO 07/02/08) LOCATED IN FMP_FM-C10B2)
!!!      IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!      WRITE(IROUT,'(/,2X,"SEMI-ROUTED RETURNFLOWS:")')
!!!      ENDIF
!!!      NRCH=0
!!!      IF(H2ORETURN(1,NF).NE.0 .AND. H2ORETURN(2,NF)==0
!!!     +                        .AND. FDSEGL(NF)==0.D0) THEN
!!!!      IF( H2ORETURN(1,NF).NE.0.AND.IRRFL.NE.0.AND.FDSEGL(NF).EQ.0 .AND.
!!!!     +   ( ISRRFL.EQ.0 .OR. (ISRRFL.GT.0.AND.ALL(ISRR(2:5,NF).EQ.0))
!!!!     +   ) )THEN 
!!!        ICNT=0
!!!        GSURFOLD=0.D0
!!!        COLD=0.D0
!!!        DO I=1,WBS%FID(NF)%Count                                        !seb removed double do loop NOTE THAT FOR INACTIVE FORMS FMLOC(NF)%Count=0 SO LOOP WILL NOT OCCUR
!!!          IR=WBS%FID(NF)%RC(ONE,I)
!!!          IC=WBS%FID(NF)%RC(TWO,I)
!!!        !DO IR=1,NROW
!!!        !DO IC=1,NCOL
!!!        !IF(FDSEGL(NF).EQ.0.AND.IFID(IC,IR).EQ.NF.and.LFID(NF)) THEN     !include test for returnflow only in active farm id's--rth
!!!         ICNT=ICNT+1
!!!        IF(ICNT.EQ.1.OR.(ICNT.GT.1.AND.WBS%GSE(IC,IR).LE.GSURFOLD))
!!!     1   GOTO 200
!!!         IF(WBS%GSE(IC,IR).GT.GSURFOLD) GO TO 210
!!!200      COUNT=0.D0                     
!!!         DO L=1,NSTRM
!!!           IF( (IRRFL.EQ.1.AND.IDIVAR(1,ISTRM(4,L)).EQ.0.
!!!     1          AND.ISTRM(4,L).NE.1) .OR.  IRRFL.EQ.-1  ) THEN
!!!           COUNT=COUNT+1
!!!           ASUM=DBLE(DELC(ISTRM(2,L))/2.+DELC(IR)/2.)
!!!           BSUM=DBLE(DELR(ISTRM(3,L))/2.+DELR(IC)/2.)
!!!           NR=IABS(ISTRM(2,L)-IR)                                        !NUMBER OF ROWS BETWEEN STREAM REACH AND CELL
!!!           NC=IABS(ISTRM(3,L)-IC)                                        !NUMBER OF COLUMNS BETWEEN STREAM REACH AND CELL
!!!             IF(NR.EQ.0) ASUM=0.D0
!!!             IF(NR.GT.1) THEN
!!!             DO N=2,NR
!!!               IF(ISTRM(2,L).LT.IR) IA=ISTRM(2,L)+(N-1)                 !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM STREAM REACH!
!!!               IF(ISTRM(2,L).GT.IR) IA=IR+(N-1)                         !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
!!!               ASUM=ASUM+DBLE(DELC(IA))
!!!             ENDDO
!!!             ENDIF
!!!             IF(NC.EQ.0) BSUM=0.D0
!!!             IF(NC.GT.1) THEN
!!!             DO N=2,NC  
!!!               IF(ISTRM(3,L).LT.IC) IB=ISTRM(3,L)+(N-1)                 !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM STREAM REACH!
!!!               IF(ISTRM(3,L).GT.IC) IB=IC+(N-1)                         !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
!!!               BSUM=BSUM+DBLE(DELR(IB))
!!!             ENDDO
!!!             ENDIF
!!!           C=DSQRT(ASUM**2+BSUM**2)
!!!             IF(COUNT.EQ.1 .OR. (COUNT.GT.1 .AND. C.LT.COLD)) THEN
!!!             COLD=C
!!!             NRCH=L
!!!             ENDIF
!!!           ENDIF
!!!         ENDDO
!!!         GSURFOLD=WBS%GSE(IC,IR)
!!!210     ENDDO
!!!        !ENDDO
!!!        IF(NRCH.gt.0) THEN                        !seb broke into if statment to include IRTPFL OR WOULD REFER TO ISTRM(:,0)
!!!            IFDRID(NF,NRCH)=NF                    !include test for returnflow only in active farm id's--rth
!!!            IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) WRITE(IROUT,118)        
!!!     1  ISTRM(2,NRCH),ISTRM(3,NRCH),ISTRM(4,NRCH),ISTRM(5,NRCH)
!!!  118   FORMAT(3X,'SEMI-ROUTED RUNOFF-RETURNFLOW TO A STREAM REACH',
!!!     1  ' FOUND NEAREST TO THE LOWEST FARM ELEVATION AT:',/,6X,
!!!     2  'ROW',2X,'COLUMN',2X,'SEGMENT NO.',2X,'REACH NO.',/,
!!!     3  3X,I6,I8,I12,I11)
!!!        ENDIF
!!!      ENDIF
!!!C
!!!C8D6---FOR SEMI-ROUTED RUNOFF-RETURNFLOW TO SPECIFIED REACH:
!!!C       - DEFINE A SPECIFIED REACH (BY COORDINATES) TO BE A "REMOTE" FARM 'DRAIN' REACH
!!!C         (NOTE: UNLIKE 'DRAIN REACHES' INSIDE A FARM, THIS RETURNFLOW REACH CAN BE ON ANY TYPE OF SEGMENT)
!!!C       - DETERMINE RUNOFF INTO "REMOTE" FARM RETURNFLOW REACH FROM PRESPECIFIED RUNOFF PER SEGMENT.
!!!C       - LOCK RUNOFF INTO "REMOTE" FARM RETURNFLOW REACH, SO THAT IT CAN'T BE OVERWRITTEN.
!!!      IF(H2ORETURN(2,NF).NE.0) THEN
!!!          I = SWFL%ISRR%SEGRCH(ONE,NF)
!!!          !
!!!          L = SEG_NSTRM(I) + SWFL%ISRR%SEGRCH(TWO,NF)
!!!          IFDRID(NF,L)=NF
!!!          STRM(12,L)=SEG(3,ISTRM(4,L))*(STRM(1,L)/SEG(1,ISTRM(4,L)))
!!!          SFR_RUNOFF(L)=DBLE(STRM(12,L))
!!!          IF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!          WRITE(IROUT,119) ISTRM(2,L),ISTRM(3,L),ISTRM(4,L),ISTRM(5,L)
!!!  119     FORMAT(3X,'SEMI-ROUTED RUNOFF-RETURNFLOW TO A SPECIFED ',
!!!     1    'STREAM REACH AT:',/,6X,'ROW',2X,'COLUMN',2X,'SEGMENT NO.',
!!!     2    2X,'REACH NO.',/,3X,I6,I8,I12,I11)
!!!          ENDIF
!!!      ELSEIF(FMPOUT%ROUTING_INFORMATION%IS_OPEN) THEN
!!!                                    WRITE(FMPOUT%ROUTING_INFORMATION%IU,
!!!     1 '(3X,"NO POINT OF DIVERSION FOR SEMI-ROUTED DELIVERY SPECIFIED:",
!!!     2 " NO SEMI-ROUTED DIVERSION POSSIBLE.")')
!!!      END IF
!!!C
!!!      RETURN
!!!      END SUBROUTINE
    