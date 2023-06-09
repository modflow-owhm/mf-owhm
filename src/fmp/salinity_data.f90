!
!#########################################################################################################
! 
MODULE SALINITY_DATA_FMP_MODULE!, ONLY: SALINITY_DATA, INITIALIZE_SALINITY_DATA
  !
  USE    FMP_DIMENSION_MODULE, ONLY: FMP_DIMENSION
  USE     WBS_DATA_FMP_MODULE, ONLY:     WBS_DATA
  USE    CROP_DATA_FMP_MODULE, ONLY:    CROP_DATA
  USE CLIMATE_DATA_FMP_MODULE, ONLY: CLIMATE_DATA
  !
  USE CONSTANTS
  USE ULOAD_AND_SFAC_INTERFACE
  USE ARRAY_DATA_TYPES,                 ONLY: DOUBLE_VECTOR,LOGICAL_VECTOR
  USE IS_ROUTINES,                      ONLY: IS_NUMBER
  USE ERROR_INTERFACE,                  ONLY: STOP_ERROR, WARNING_MESSAGE, FILE_IO_ERROR
  USE PARSE_WORD_INTERFACE,             ONLY: PARSE_WORD, PARSE_WORD_UP
  USE STRINGS,                          ONLY: GET_INTEGER, GET_NUMBER, UPPER
  USE UTIL_INTERFACE,                   ONLY: NEAR_ZERO
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR
  USE ALLOC_INTERFACE,                  ONLY: ALLOC
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,  ONLY: GENERIC_OUTPUT_FILE
  USE LIST_ARRAY_INPUT_INTERFACE,       ONLY: LIST_ARRAY_INPUT, LIST_ARRAY_INPUT_INT, LIST_ARRAY_INPUT_STR
  USE WARNING_TYPE_INSTRUCTION,         ONLY: WARNING_TYPE
  USE EquationParser,                   ONLY: EVAL
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: SALINITY_DATA, INITIALIZE_SALINITY_DATA
  !
  TYPE SALINITY_EXPRESSION
      INTEGER:: EVAL = Z
      DOUBLE PRECISION:: VAL=DZ
      CHARACTER(:),ALLOCATABLE:: EXP
      DOUBLE PRECISION:: SF=UNO
  END TYPE
  !
  !TYPE SOURCE_SALINITY
  !    DOUBLE PRECISION:: NRD=DZ, SW=DZ, GW=DZ
  !    DOUBLE PRECISION:: ECw
  !    !
  !    CONTAINS
  !    !
  !    PROCEDURE, PASS(SRC):: SET_ECw  => CALC_ECw_FROM_SOURCES
  !END TYPE
  !
  TYPE IRRIGATION_RATIO
      DOUBLE PRECISION:: NRD=DZ, SW=DZ, GW=DZ
      CONTAINS
      !PROCEDURE, PASS(RAT):: SET_RATIO  => CALC_IRRIGATION_RATIO_FROM_SOURCES
  END TYPE
  !
  TYPE SALINITY_DATA
      INTEGER:: IOUT=Z, LOUT=Z
      INTEGER:: NFARM, NCROP, NIRRG, NROW, NCOL, cNROW
      LOGICAL:: SKIP_SALINITY = TRUE
      LOGICAL:: TFR_READ      = FALSE
      LOGICAL:: CROP_FRACTION = FALSE
      LOGICAL:: HAS_CUSTOM_LR = FALSE
      LOGICAL:: HAS_CUSTOM_AW = FALSE
      LOGICAL:: HAS_NON_CUSTOM= FALSE
      TYPE(SALINITY_EXPRESSION),DIMENSION(:),  ALLOCATABLE:: LR_EXP
      TYPE(SALINITY_EXPRESSION),DIMENSION(:),  ALLOCATABLE:: AW_EXP
      TYPE(DOUBLE_VECTOR),      DIMENSION(:),  ALLOCATABLE:: LR
      TYPE(DOUBLE_VECTOR),      DIMENSION(:),  ALLOCATABLE:: AW
      TYPE(LOGICAL_VECTOR),     DIMENSION(:),  ALLOCATABLE:: INUSE
      DOUBLE PRECISION,         DIMENSION(:),  ALLOCATABLE:: ECe
      DOUBLE PRECISION,         DIMENSION(:),  ALLOCATABLE:: ECw
      DOUBLE PRECISION,         DIMENSION(:),  ALLOCATABLE:: MAX_LR
      DOUBLE PRECISION,         DIMENSION(:,:),ALLOCATABLE:: IRR_UNI
      DOUBLE PRECISION,         DIMENSION(:,:),ALLOCATABLE:: SUP_PPM
      DOUBLE PRECISION:: MIN_VARIABLE
      !
      TYPE(GENERIC_OUTPUT_FILE):: OUT_BYFARMCROP
      TYPE(GENERIC_OUTPUT_FILE):: OUT_BYFARM
      TYPE(GENERIC_OUTPUT_FILE):: OUT_ALL
      TYPE(GENERIC_OUTPUT_FILE):: OUT_INPUT
      !
      TYPE(LIST_ARRAY_INPUT_STR):: LR_TFR
      TYPE(LIST_ARRAY_INPUT_STR):: AW_TFR
      TYPE(LIST_ARRAY_INPUT    ):: MAX_LR_TFR
      TYPE(LIST_ARRAY_INPUT    ):: AW_PPM_TFR
      TYPE(LIST_ARRAY_INPUT    ):: STOL
      TYPE(LIST_ARRAY_INPUT    ):: IRR_UNI_TFR
      TYPE(LIST_ARRAY_INPUT_INT):: USED
      !
      CONTAINS
      !
      PROCEDURE, PASS(SALT):: NEXT        => SETUP_NEXT_STRESS_PERIOD!(CDAT,WBS)
      PROCEDURE, PASS(SALT):: NEXT_TS     => SETUP_NEXT_TIME_STEP
      PROCEDURE, PASS(SALT):: CROP_BY_TS  => SETUP_CROP_BY_TIME_STEP
      PROCEDURE, PASS(SALT):: DEMAND_CALC => CALCULATE_SALINTY_DEMAND!(CDAT)
      PROCEDURE, PASS(SALT):: PRINT_OUT_BYFARM
      PROCEDURE, PASS(SALT):: PRINT_OUT_BYFARM_BYCROP
      PROCEDURE, PASS(SALT):: PRINT_OUT_ALL_CROP
      FINAL:: DEALLOCATE_SALINITY_FINAL
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE DEALLOCATE_SALINITY_FINAL(SALT)
    TYPE(SALINITY_DATA)::SALT
    CALL DEALLOCATE_SALINITY(SALT)
  END SUBROUTINE
  !
  SUBROUTINE DEALLOCATE_SALINITY(SALT)
    CLASS(SALINITY_DATA), INTENT(INOUT)::SALT
       !
       SALT%IOUT  = Z
       SALT%LOUT  = Z
       SALT%TFR_READ    = FALSE
       !IF(ALLOCATED(SALT%XXX)) DEALLOCATE(SALT%XXX)
       !
  END SUBROUTINE
  !  
  SUBROUTINE INITIALIZE_SALINITY_DATA( BL, SALT, LINE, FDIM, CROP_FRACTION )
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(SALINITY_DATA),        INTENT(INOUT):: SALT
    CHARACTER(*),                INTENT(INOUT):: LINE
    TYPE(FMP_DIMENSION),         INTENT(IN   ):: FDIM
    LOGICAL,                     INTENT(IN   ):: CROP_FRACTION
    !INTEGER,                     INTENT(IN   ):: LENUNI
    !
    CHARACTER(:),ALLOCATABLE:: ERR
    CHARACTER(5):: ERROR
    CHARACTER(6):: BYCROP, BYFARM
    LOGICAL:: EOF
    INTEGER:: I, LLOC, ISTART, ISTOP, LINELEN, cNROW
    LOGICAL:: BINARY
    TYPE(WARNING_TYPE):: WARN_MSG
    !
    CALL WARN_MSG%INIT()
    !
    WRITE(BL%IOUT,'(/A/)') 'SALINITY BLOCK FOUND AND NOW LOADING PROPERTIES'
    !
    IF(FDIM%NCROP == Z .OR. FDIM%NIRRG==Z) RETURN
    !
    LINELEN = NEG
    !SALT%HAS_SALINITY = TRUE
    SALT%IOUT = BL%IOUT
    SALT%LOUT = BL%IOUT
    SALT%NCROP = FDIM%NCROP
    SALT%NIRRG = FDIM%NIRRG
    !
    SALT%NFARM = FDIM%NFARM
    IF(SALT%NFARM < ONE) SALT%NFARM = ONE
    !
    SALT%NCOL = FDIM%NCOL
    SALT%NROW = FDIM%NROW
    !
    SALT%CROP_FRACTION = CROP_FRACTION
    !
    !SELECT CASE (LENUNI)
    !CASE(Z);     SUP%TO_LITER = 1000.D0
    !CASE(ONE);   SUP%TO_LITER = 28.316846592D0
    !CASE(TWO);   SUP%TO_LITER = 1000.D0
    !CASE(THREE); SUP%TO_LITER = 0.001D0
    !END SELECT
    !
    cNROW = FDIM%NROW
    IF(CROP_FRACTION) cNROW = cNROW * SALT%NCROP !LOAD IN NCROP SETS OF NROWS
    SALT%cNROW = cNROW
    !
    ALLOCATE(SALT%ECw   (SALT%NFARM)      )
    !
    ALLOCATE(SALT%INUSE (SALT%NCROP)      )
    ALLOCATE(SALT%LR_EXP(SALT%NCROP)      )
    ALLOCATE(SALT%AW_EXP(SALT%NCROP)      )
    ALLOCATE(SALT%LR    (SALT%NCROP)      )
    ALLOCATE(SALT%AW    (SALT%NCROP)      )
    ALLOCATE(SALT%ECe   (SALT%NCROP)      )
    ALLOCATE(SALT%MAX_LR(SALT%NCROP)      )
    !
    ALLOCATE(SALT%SUP_PPM(FOUR,SALT%NCROP))
    !
    ALLOCATE(SALT%IRR_UNI(SALT%NIRRG,SALT%NFARM))
    !
    !ULOAD_LINE = BLNK
    !
    ERROR  = 'ERROR'
    BYFARM = 'BYWBS'
    BYCROP = 'BYCROP'
    !
    SALT%MIN_VARIABLE = DZ
    !
    ! LOOK FOT LINELEN
    CALL BL%START()
    DO I=ONE, BL%NLINE
                    LLOC=ONE
                    CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                    !
                    SELECT CASE(BL%LINE(ISTART:ISTOP))
                    CASE("EXPRESSION_LINE_LENGTH","LINELEN"); CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,LINELEN,MSG='SALINITY BLOCK KEYWORD "EXPRESSION_LINE_LENGTH"; FAILED TO LOAD THE MAXIMUM LENGTH OF THE EXPRESSION LINES.')
                    END SELECT
                    !
                    CALL BL%NEXT()
    END DO
    !
    IF(LINELEN == NEG) THEN
        CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SALINITY BLOCK FAILED TO IDENTIFY KEYWORD "EXPRESSION_LINE_LENGTH" (LINELEN).'//NL//'--- IT WILL BE AUTOMATICALLY SET TO 20 ---'//NL//'IT IS RECOMMENDED TO SPECIFY THE MAXIMUM LENGTH OF LINES USED FOR KEYWORDS "CROP_LEACHING_REQUIREMENT" AND "CROP_SALINITY_DEMAND".'//NL//'NOTE THIS IS ONLY SPACE REQUIRED FOR WHAT IS LOADED AFTER THE RECORD ID,'//NL//'SO IF YOU ONLY USE "RHOADES" OPTION THEN YOU CAN SET IT TO 7 OR GREATER. IF YOU SPECIFY JUST A NUMBER THEN IT SHOULD BE LARGE ENOUGH TO CAPTURE THE NUMBER (e.g. "1.2345E21" SHOULD HAVE A LENGTH GREATER THAN 9'//NL//'IF YOU DO NOT KNOW THE LENGTH, THEN JUST PICK A LARGE NUMBER (SAY 250).', INLINE=TRUE)
        LINELEN = 19
    END IF
    !
    IF(LINELEN < ONE) THEN
        CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='SALINITY BLOCK FOUND KEYWORD "EXPRESSION_LINE_LENGTH" (LINELEN) BUT ITS LENTH IS LESS THAN OR EQUAL TO ZERO.'//NL//'--- IT WILL BE AUTOMATICALLY SET TO 20 ---'//NL//'IT IS RECOMMENDED TO SPECIFY THE MAXIMUM LENGTH OF LINES USED FOR KEYWORDS "CROP_LEACHING_REQUIREMENT" AND "CROP_SALINITY_DEMAND".'//NL//'NOTE THIS IS ONLY SPACE REQUIRED FOR WHAT IS LOADED AFTER THE RECORD ID,'//NL//'SO IF YOU ONLY USE "RHOADES" OPTION THEN YOU CAN SET IT TO 7 OR GREATER. IF YOU SPECIFY JUST A NUMBER THEN IT SHOULD BE LARGE ENOUGH TO CAPTURE THE NUMBER (e.g. "1.2345E21" SHOULD HAVE A LENGTH GREATER THAN 9'//NL//'IF YOU DO NOT KNOW THE LENGTH, THEN JUST PICK A LARGE NUMBER (SAY 250).', INLINE=TRUE)
        LINELEN = 19
    END IF
    !
    IF(LINELEN > ONE)  WRITE(BL%IOUT,'(2A)') 'EXPRESSION_LINE_LENGTH (LINELEN) KEYWORD FOUND AND WILL HAVE A MAX EXPRESSION LENGTH OF ', NUM2STR(LINELEN)
    !
    LINELEN = LINELEN + ONE
    IF(LINELEN<20) LINELEN=20
    !
    CALL BL%MAKE_SCRATCH_FILE()  !OR CALL BL%START AND DO I=ONE, BL%NLINE WITH BL%NEXT FOR BL%LINE
    !
    !READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    CALL BL%READ_SCRATCH(EOF, LINE)
    !
    DO WHILE (.NOT. EOF)
      !
      LLOC=ONE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      !
      SELECT CASE ( LINE(ISTART:ISTOP) )
      !
      CASE("EXPRESSION_LINE_LENGTH","LINELEN"); CONTINUE
      !
      CASE("EXPRESSION_VARIABLE_NEARZERO")
                       CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SALT%MIN_VARIABLE,  MSG='FMP SALINTY BLOCK "EXPRESSION_VARIABLE_NEARZERO" FAILED TO LOAD THE ACTUAL VALUE.')
      !
      CASE ("CROP_LEACHING_REQUIREMENT")
                        WRITE(BL%IOUT,'(A)') '   CROP_LEACHING_REQUIREMENT                    KEYWORD FOUND.'
                        CALL SALT%LR_TFR%INIT('LR',LLOC, LINE, BL%IOUT, BL%IU, SALT%NCROP, ONE, Z, Z, LINELEN, SALT%NCROP, BYCROP, SCRATCH=BL%SCRATCH, ENTIRE_LINE=TRUE)                  
                        IF(SALT%LR_TFR%TRUNCATED) CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP SALINTY BLOCK LIST-ARRAY INPUT OF "CROP_LEACHING_REQUIREMENT"'  //NL//'INDICATED TO OneWater THAT THE ENTIRE LINE MAY HAVE NOT BEEN LOADED.'//BLN//'THIS MAY BE THE RESULT OF THE KEYWORD "EXPRESSION_LINE_LENGTH" NOT SPECIFYING A LARGE ENOUGH LINELEN'//NL//'   (LINELEN = NUMBER OF CHARACTERS/SPACE TO USE FOR LOADING THE LINE)'//BLN//'THE FOLLOWING IS THE INPUT THAT WAS LOADED,'//NL//'THE "" INDICATE THE START AND END OF WHAT EACH LINE LOADED.'//NL//SALT%LR_TFR%GET_LINES())
      CASE ("CROP_SALINITY_APPLIED_WATER")
                        WRITE(BL%IOUT,'(A)') '   CROP_SALINITY_APPLIED_WATER                    KEYWORD FOUND.'
                        CALL SALT%AW_TFR%INIT('AW',LLOC, LINE, BL%IOUT, BL%IU, SALT%NCROP, ONE, Z, Z, LINELEN, SALT%NCROP, BYCROP, SCRATCH=BL%SCRATCH, ENTIRE_LINE=TRUE)
                        IF(SALT%AW_TFR%TRUNCATED) CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP SALINTY BLOCK LIST-ARRAY INPUT OF "CROP_SALINITY_APPLIED_WATER"'//NL//'INDICATED TO OneWater THAT THE ENTIRE LINE MAY HAVE NOT BEEN LOADED.'//BLN//'THIS MAY BE THE RESULT OF THE KEYWORD "EXPRESSION_LINE_LENGTH" NOT SPECIFYING A LARGE ENOUGH LINELEN'//NL//'   (LINELEN = NUMBER OF CHARACTERS/SPACE TO USE FOR LOADING THE LINE)'//BLN//'THE FOLLOWING IS THE INPUT THAT WAS LOADED,'//NL//'THE "" INDICATE THE START AND END OF WHAT EACH LINE LOADED.'//NL//SALT%AW_TFR%GET_LINES())
      CASE ("CROP_MAX_LEACHING_REQUIREMENT", "MAX_LEACHING_REQUIREMENT")
                        WRITE(BL%IOUT,'(A)') '   CROP_MAX_LEACHING_REQUIREMENT                  KEYWORD FOUND.'
                        CALL SALT%MAX_LR_TFR%INIT('MAX_LR',LLOC, LINE, BL%IOUT, BL%IU, SALT%NCROP, ONE, Z, Z, SALT%NCROP, BYCROP, SCRATCH=BL%SCRATCH)
      CASE ("WBS_SUPPLY_SALT_CONCENTRATION")
                        WRITE(BL%IOUT,'(A)') '   WBS_SUPPLY_SALT_CONCENTRATION  KEYWORD FOUND.'
                        CALL SALT%AW_PPM_TFR%INIT('AW_PPM', LLOC, LINE, BL%IOUT, BL%IU, SALT%NFARM, FOUR, Z, Z, SALT%NFARM, BYFARM, FOUR, 'BYSOURCE', SCRATCH=BL%SCRATCH)
                        !
      CASE ("CROP_SALINITY_TOLERANCE")
                        WRITE(BL%IOUT,'(A)') '   CROP_TOLERANCE                 KEYWORD FOUND.'
                        CALL SALT%STOL%INIT('SAL_TOL', LLOC, LINE, BL%IOUT, BL%IU, SALT%NCROP, ONE, Z, Z, SALT%NCROP, BYCROP, SCRATCH=BL%SCRATCH)
                        !
      CASE ("CROP_HAS_SALINITY_DEMAND", "HAS_CROP_SALINITY_DEMAND")
                        WRITE(BL%IOUT,'(A)') '   CROP_HAS_SALINITY_DEMAND       KEYWORD FOUND.  READ EITHER NCROP LIST, IF USING CROP ID THEN ARRAY IS "NROW by NCOL" AND IF USING CROP FRACTIONS THEN ARRAY IS "NROW*NCROP by NCOL"'
                        CALL SALT%USED%INIT('SAL_CHECK', LLOC, LINE, BL%IOUT, BL%IU, SALT%NCROP, ONE, cNROW, SALT%NCOL, SCRATCH=BL%SCRATCH) !!!---NOTE ITS cNROW---!!!
                        !
      CASE ("WBS_IRRIGATION_UNIFORMITY")
                        WRITE(BL%IOUT,'(A)') '   WBS_IRRIGATION_UNIFORMITY                    KEYWORD FOUND.'
                        CALL SALT%IRR_UNI_TFR%INIT('IRR_UNI', LLOC, LINE, BL%IOUT, BL%IU, SALT%NFARM, SALT%NIRRG, Z, Z, SALT%NFARM, BYFARM, FDIM%NIRRG, 'BYIRRIGATE', SCRATCH=BL%SCRATCH, LISTARRAY=TRUE)
                        !

      CASE ("PRINT")
                        BINARY = FALSE
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        IF(LINE(ISTART:ISTOP) == 'BINARY') THEN
                            BINARY = TRUE
                            CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        END IF
                        !
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE ("BYWBS","BYFARM")
                                          CALL SALT%OUT_BYFARM%OPEN(LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        CASE ("BYWBSBYCROP","BYWBSCROP","BYWBS_BYCROP","BYFARMBYCROP","BYFARMCROP","BYFARM_BYCROP")
                                          CALL SALT%OUT_BYFARMCROP%OPEN(LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        CASE ("ALL")
                                          CALL SALT%OUT_ALL%OPEN(LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        !CASE ("DETAIL")
                        !                  CALL SALT%OUT_ALL%OPEN(LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        CASE ("INPUT")
                                          CALL SALT%OUT_INPUT%OPEN(LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        !
                        CASE DEFAULT;     CALL STOP_ERROR(LINE,BL%IU,BL%IOUT,'FMP SALINTY BLOCK KEYWORD ERROR. IDENTIFIED KEYWORD "PRINT", BUT THE NEXT WORD WAS NOT IDENTIFIED.'//NL//'WORDS EXPECTED ARE: "BYWBS", "BYWBSBYCROP", "ALL", "INPUT"')
                        END SELECT
      CASE DEFAULT
                        CALL WARN_MSG%ADD('FOUND UNKNOWN KEYWORD "'//LINE(ISTART:ISTOP)//'" ***IT WILL BE IGNORED***'//BLN)
                        
      END SELECT
      !
      !READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
      CALL BL%READ_SCRATCH(EOF, LINE)
      !
    END DO
    !
    CALL WARN_MSG%CHECK(HED='FMP SALINITY BLOCK'//NL,INFILE=BL%IU,OUTPUT=BL%IOUT,INLINE=TRUE,CMD_PRINT=TRUE,TAIL=NL)
    !
    SALT%SKIP_SALINITY = FALSE
    !
    IF(.NOT. SALT%USED%INUSE) THEN
        CALL SALT%USED%INIT('GWRT', ONE, BL%IOUT, BL%IU, SALT%NCROP, ONE, Z, Z)
    END IF
    !
    IF( .NOT. SALT%AW_PPM_TFR%INUSE ) THEN
        CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='FMP SALINITY_FLUSH_IRRIGATION Block: Failed to find keyword "WBS_SUPPLY_SALT_CONCENTRATION".'//NL//'All source water supply salt concentrations are set to 0.0 PPM (mg/L).',INLINE=TRUE)
        CALL SALT%AW_PPM_TFR%INIT('AW_PPM', DZ, BL%IOUT, BL%IU, SALT%NFARM, FOUR, Z, Z) 
    END IF
    !
    IF(.NOT. SALT%IRR_UNI_TFR%INUSE) THEN
        CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='FMP SALINITY_FLUSH_IRRIGATION Block:  "WBS_IRRIGATION_UNIFORMITY".'//NL//'Irrigation is assumed to be perfectly uniform with a value of 1.0 for all Water Balance Subregions (WBS).',INLINE=TRUE)
        CALL SALT%IRR_UNI_TFR%INIT('IRR_UNI', UNO, BL%IOUT, BL%IU, SALT%NFARM, SALT%NIRRG, Z, Z)
    END IF
    !
    ERR = NL
    IF( .NOT. SALT%LR_TFR%INUSE ) ERR = ERR//'      CROP_LEACHING_REQUIREMENT'//NL
    IF( .NOT. SALT%AW_TFR%INUSE ) ERR = ERR//'      CROP_SALINITY_APPLIED_WATER'//NL
    IF( .NOT. SALT%STOL%INUSE   ) ERR = ERR//'      CROP_SALINITY_TOLERANCE'//NL
    !
    IF(ERR .NE. NL) CALL STOP_ERROR(INFILE=BL%IU,OUTPUT=BL%IOUT,MSG='FMP SALINITY_FLUSH_IRRIGATION Block:'//NL//NL//'   Failed to find the following required keywords:'//NL//TRIM(ERR)//NL)
    !
  END SUBROUTINE 
  !
  SUBROUTINE SETUP_NEXT_STRESS_PERIOD(SALT,CDAT,WBS)
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    CLASS(CROP_DATA),     INTENT(IN   ):: CDAT
    CLASS(WBS_DATA),      INTENT(IN   ):: WBS
    LOGICAL:: UPDATE
    INTEGER:: I, J, K, R, C, LLOC,ISTART,ISTOP
    CHARACTER(:),ALLOCATABLE:: ERROR
    LOGICAL, DIMENSION(SALT%NCROP):: PRNT
    !
    !IF(SALT%SPKIP_SALINITY) RETURN
    !
    IF(SALT%TFR_READ) THEN
        !
        UPDATE = CDAT%CID%TRANSIENT .OR. (CDAT%MULTI_CROP_CELLS .AND. CDAT%FRAC%TRANSIENT) .OR. CDAT%IRR%TRANSIENT .OR. WBS%NEW_FID
        !
        CALL SALT%LR_TFR     %NEXT()
        CALL SALT%AW_TFR     %NEXT()
        CALL SALT%AW_PPM_TFR %NEXT()
        CALL SALT%STOL       %NEXT()
        CALL SALT%IRR_UNI_TFR%NEXT()
        CALL SALT%USED       %NEXT()
        !
    ELSE
        !
        UPDATE = TRUE
        !
        SALT%TFR_READ = TRUE
    END IF
    !
    IF(UPDATE) CALL SALT%INUSE(:)%ALLOC(CDAT%CROP(:)%N)
    !
    IF( UPDATE ) THEN
            !
            IF(SALT%USED%LISTLOAD) THEN
                DO CONCURRENT (I=ONE:SALT%NCROP)
                      SALT%INUSE(I)%VEC = SALT%USED%LIST(I) .NE. Z
                END DO
            ELSE
                ASSOCIATE (CPR=>CDAT%CROP, TF=>SALT%INUSE, ARRAY=>SALT%USED%ARRAY)
                    DO I=ONE, SALT%NCROP
                       if(CPR(I)%N > 0) then
                          CALL CROP_INPUT_ARRAY_TO_LOGICAL_PROP(CPR(I)%N, CPR(I)%RC, TF(I)%VEC, ARRAY(:,CPR(I)%LD:) )
                       end if
                    END DO
                END ASSOCIATE
            END IF
            !
            SALT%SKIP_SALINITY = TRUE
            DO I=ONE,SALT%NCROP
                IF(ANY(SALT%INUSE(I)%VEC)) THEN
                   SALT%SKIP_SALINITY = FALSE
                   EXIT
                END IF
            END DO
    END IF
    !
    IF(SALT%SKIP_SALINITY) RETURN !---------------------------------------------
    !
    ERROR = NL
    !
    IF( UPDATE .OR. SALT%USED%TRANSIENT .OR. CDAT%IRR%TRANSIENT) THEN
        !
        IF( CDAT%IRR%LISTLOAD) THEN
            PRNT = TRUE
            DO CONCURRENT (I=ONE:CDAT%NCROP)
            DO CONCURRENT (K=ONE:CDAT%CROP(I)%N, SALT%INUSE(I)%VEC(K) .AND. CDAT%CROP(I)%IRR(K) == Z )
                                                                    SALT%INUSE(I)%VEC(K) = FALSE
                                                                    IF(PRNT(I)) THEN
                                                                                    ERROR = ERROR//BLNK//NUM2STR(I,-8)//NL
                                                                                    PRNT(I) = FALSE
                                                                    END IF
            END DO; END DO

            IF (ERROR.NE.NL) ERROR='FMP SALINTY BLOCK -- FOUND A CROP THAT HAS SALINITY DEMAND,'//NL//                                  &
                                   'BUT IT HAS AN IRRIGATION FLAG EQUAL TO ZERO,'//NL//                                                 &
                                   'SO THERE IS NO WAY TO IRRIGATE CROP TO MEET ADDITINAL SALINITY IRRIGATION DEMAND.'//NL//            &
                                   'THE FOLLOWING IS A LIST OF CROPS THAT HAD THEIR'//NL//                                              &
                                   '"HAS_CROP_SALINITY_DEMAND" FLAG SET FROM TRUE TO FALSE (i.e. 1 BECOMES 0).'//BLN//                  &
                                   ' CROP_ID'//ERROR
        ELSE
            DO CONCURRENT (I=ONE:CDAT%NCROP)
            DO CONCURRENT (K=ONE:CDAT%CROP(I)%N, SALT%INUSE(I)%VEC(K) .AND. CDAT%CROP(I)%IRR(K) == Z )
                                                                    SALT%INUSE(I)%VEC(K) = FALSE
                                                                    R = CDAT%CROP(I)%RC(ONE,K)
                                                                    C = CDAT%CROP(I)%RC(TWO,K)
                                                                    ERROR = ERROR//BLNK//NUM2STR(I,-8)//NUM2STR(R,-10)//' '//NUM2STR(C,-10)//NL
            END DO; END DO
            IF (ERROR.NE.NL) ERROR='FMP SALINTY BLOCK -- FOUND A CROP THAT HAS SALINITY DEMAND,'//NL//                                   &
                                   'BUT IT HAS AN IRRIGATION FLAG EQUAL TO ZERO,'//NL//                                                 &
                                   'SO THERE IS NO WAY TO IRRIGATE CROP TO MEET ADDITINAL SALINITY IRRIGATION DEMAND.'//NL//            &
                                   'THE FOLLOWING IS A LIST OF CROPS THAT HAD THEIR'//NL//                                              &
                                   '"HAS_CROP_SALINITY_DEMAND" FLAG SET FROM TRUE TO FALSE (i.e. 1 BECOMES 0).'//BLN//                  &
                                   ' CROP_ID ROW        COLUMN'//ERROR
        END IF
        !
        IF (ERROR.NE.NL) THEN
            CALL WARNING_MESSAGE(OUTPUT=SALT%LOUT,MSG=ERROR)
            ERROR = NL
        END IF
    END IF
    !
    IF(UPDATE) THEN
               CALL SALT%LR   (:)%ALLOC(CDAT%CROP(:)%N)
               CALL SALT%AW   (:)%ALLOC(CDAT%CROP(:)%N)
    END IF
    !    
    IF( UPDATE .OR. SALT%LR_TFR%TRANSIENT) THEN
        !
        SALT%HAS_CUSTOM_LR = FALSE
        !
        DO CONCURRENT (I=ONE:SALT%NCROP); SALT%LR_EXP(I)%SF = UNO
        END DO
        !
        CALL UPPER(SALT%LR_TFR%LIST)  !ENSURE THAT ALL EXPRESIONS ARE UPPER CASE - ELEMENTAL FUNCTION
        !
        ASSOCIATE (LR=>SALT%LR_EXP, SFAC=>SALT%LR_TFR%SFAC, EXPR=>SALT%LR_TFR%LIST, IN =>SALT%LR_TFR%TFR%IU)
            !
            IF(SFAC%HAS_ALL) THEN
                             DO I=ONE, SALT%NCROP
                                                LR(I)%SF = LR(I)%SF * SFAC%ALL
                             END DO
            END IF
            !
            IF(SFAC%HAS_EX1) THEN
                             DO I=ONE, SALT%NCROP
                                                LR(I)%SF = LR(I)%SF * SFAC%EX1(I)
                             END DO
            END IF
            !
            DO I=ONE, SALT%NCROP
                !
                IF(IS_NUMBER(EXPR(I))) THEN
                    LR(I)%EVAL = ONE
                    LLOC = ONE
                    CALL GET_NUMBER(EXPR(I),LLOC,ISTART,ISTOP,SALT%IOUT,IN,LR(I)%VAL,  MSG='FMP SALINITY BLOCK LEACHING REQUIREMENT EXPRESSION ERROR; DETECTED SINGLE NUMBER IN EXPRESSION BUT FAILED TO CONVERT IT TO A NUMBER.')
                ELSE
                    LLOC=ONE
                    CALL PARSE_WORD(EXPR(I),LLOC,ISTART,ISTOP)
                    SELECT CASE(EXPR(I)(ISTART:ISTOP))
                    CASE("RHOADES");    LR(I)%EVAL = TWO  !Rhoades Equation (Rhoades, 1974 and Rhoades and Merrill 1976) cited in FAO pub by  Ayers and Wescott (1985)
                    CASE("SKIP","NONE");LR(I)%EVAL = Z  
                    CASE DEFAULT
                                        LR(I)%EVAL = NEG
                                        IF(ALLOCATED(LR(I)%EXP)) DEALLOCATE(LR(I)%EXP, STAT=LLOC)
                                        !
                                        ALLOCATE(LR(I)%EXP, SOURCE=TRIM(EXPR(I)))
                                        SALT%HAS_CUSTOM_LR = TRUE
                    END SELECT
                END IF
            END DO  
        END ASSOCIATE
        IF(SALT%LR_TFR%TRUNCATED .AND. SALT%LR_TFR%TRANSIENT) CALL WARNING_MESSAGE(OUTPUT=SALT%IOUT,MSG='FMP SALINTY BLOCK LIST-ARRAY INPUT OF "CROP_LEACHING_REQUIREMENT"'  //NL//'INDICATED TO OneWater THAT THE ENTIRE LINE MAY HAVE NOT BEEN LOADED.'//BLN//'THIS MAY BE THE RESULT OF THE KEYWORD "EXPRESSION_LINE_LENGTH" NOT SPECIFYING A LARGE ENOUGH LINELEN'//NL//'   (LINELEN = NUMBER OF CHARACTERS/SPACE TO USE FOR LOADING THE LINE)'//BLN//'THE FOLLOWING IS THE INPUT THAT WAS LOADED,'//NL//'THE "" INDICATE THE START AND END OF WHAT EACH LINE LOADED.'//NL//SALT%LR_TFR%GET_LINES())
    END IF
    !
    IF( UPDATE .OR. SALT%AW_TFR%TRANSIENT) THEN
        !
        SALT%HAS_CUSTOM_AW = FALSE
        !
        DO CONCURRENT (I=ONE:SALT%NCROP); SALT%AW_EXP(I)%SF = UNO
        END DO
        !
        CALL UPPER(SALT%AW_TFR%LIST)  !ENSURE THAT ALL EXPRESIONS ARE UPPER CASE - ELEMENTAL FUNCTION
        !
        ASSOCIATE (AW=>SALT%AW_EXP, SFAC=>SALT%AW_TFR%SFAC, EXPR=>SALT%AW_TFR%LIST, IN =>SALT%AW_TFR%TFR%IU)
            !
            IF(SFAC%HAS_ALL) THEN
                             DO I=ONE, SALT%NCROP
                                                AW(I)%SF = AW(I)%SF * SFAC%ALL
                             END DO
            END IF
            !
            IF(SFAC%HAS_EX1) THEN
                             DO I=ONE, SALT%NCROP
                                                AW(I)%SF = AW(I)%SF * SFAC%EX1(I)
                             END DO
            END IF          
            !
            DO I=ONE, SALT%NCROP
                !
                IF(IS_NUMBER(EXPR(I))) THEN
                    AW(I)%EVAL = ONE
                    LLOC = ONE
                    CALL GET_NUMBER(EXPR(I),LLOC,ISTART,ISTOP,SALT%IOUT,IN,AW(I)%VAL,  MSG='FMP SALINITY BLOCK LEACHING REQUIREMENT EXPRESSION ERROR; DETECTED SINGLE NUMBER IN EXPRESSION BUT FAILED TO CONVERT IT TO A NUMBER.')
                ELSE
                    LLOC=ONE
                    CALL PARSE_WORD(EXPR(I),LLOC,ISTART,ISTOP)
                    SELECT CASE(EXPR(I)(ISTART:ISTOP))
                    CASE("RHOADES");    AW(I)%EVAL = TWO
                    CASE("SKIP","NONE");AW(I)%EVAL = Z  
                    CASE DEFAULT
                                        AW(I)%EVAL = NEG
                                        IF(ALLOCATED(AW(I)%EXP)) DEALLOCATE(AW(I)%EXP, STAT=LLOC)
                                        !
                                        ALLOCATE(AW(I)%EXP, SOURCE=TRIM(EXPR(I)))
                                        SALT%HAS_CUSTOM_AW = TRUE
                    END SELECT
                END IF
            END DO
        END ASSOCIATE
        IF(SALT%AW_TFR%TRUNCATED .AND. SALT%LR_TFR%TRANSIENT) CALL WARNING_MESSAGE(OUTPUT=SALT%IOUT,MSG='FMP SALINTY BLOCK LIST-ARRAY INPUT OF "CROP_SALINITY_APPLIED_WATER"'//NL//'INDICATED TO OneWater THAT THE ENTIRE LINE MAY HAVE NOT BEEN LOADED.'//BLN//'THIS MAY BE THE RESULT OF THE KEYWORD "EXPRESSION_LINE_LENGTH" NOT SPECIFYING A LARGE ENOUGH LINELEN'//NL//'   (LINELEN = NUMBER OF CHARACTERS/SPACE TO USE FOR LOADING THE LINE)'//BLN//'THE FOLLOWING IS THE INPUT THAT WAS LOADED,'//NL//'THE "" INDICATE THE START AND END OF WHAT EACH LINE LOADED.'//NL//SALT%AW_TFR%GET_LINES())
    END IF
    !
    IF( UPDATE .OR. SALT%LR_TFR%TRANSIENT .OR. SALT%AW_TFR%TRANSIENT)  SALT%HAS_NON_CUSTOM = ANY(SALT%LR_EXP(:)%EVAL > Z) .OR. ANY(SALT%AW_EXP(:)%EVAL > Z)
    !
    IF( UPDATE .OR. SALT%AW_PPM_TFR%TRANSIENT) THEN
        SALT%SUP_PPM = SALT%AW_PPM_TFR%ARRAY  !NOTE IT IS STORED AS A LISTARRAY
        !
        ASSOCIATE (SFAC=>SALT%AW_PPM_TFR%SFAC)
            !
            IF(SFAC%HAS_ALL) SALT%SUP_PPM = SALT%SUP_PPM * SFAC%ALL
            !
            IF(SFAC%HAS_EX1) THEN 
                             DO I=ONE, SALT%NFARM
                                                SALT%SUP_PPM(:,I) = SALT%SUP_PPM(:,I) * SFAC%EX1(I)
                             END DO
            END IF
            !
            IF(SFAC%HAS_EX2) THEN 
                             DO I=ONE, SALT%NFARM
                             DO J=ONE, FOUR
                                                SALT%SUP_PPM(J,I) = SALT%SUP_PPM(J,I) * SFAC%EX2(J)
                             END DO
                             END DO
            END IF
        END ASSOCIATE
        !
        DO CONCURRENT ( I=ONE:SALT%NFARM,J=ONE:FOUR, SALT%SUP_PPM(J,I) < DZ) 
                       SALT%SUP_PPM(J,I) = DZ
                       SELECT CASE(J)
                       CASE(ONE);   ERROR = ERROR//BLNK//NUM2STR(I,-8)//' NRD      SOURCE WATER'//NL
                       CASE(TWO);   ERROR = ERROR//BLNK//NUM2STR(I,-8)//' SW       SOURCE WATER'//NL
                       CASE(THREE); ERROR = ERROR//BLNK//NUM2STR(I,-8)//' GW       SOURCE WATER'//NL
                       CASE(FOUR);  ERROR = ERROR//BLNK//NUM2STR(I,-8)//' EXTERNAL SOURCE WATER'//NL
                       END SELECT
                       
        END DO
        IF (ERROR.NE.NL) THEN
            ERROR='FMP SALINTY BLOCK -- "WBS_SUPPLY_SALT_CONCENTRATION" IS LESS THAN 0,'//NL// &
            'IT IS RESET TO 0.0 ppm [mg/L] (i.e. NO SALT CONCENTRATION) FOR THE FOLLOWING FARMS AND SOURCE WATER TYPES.'//NL//       &
            ' FARM_ID  WATER_SOURCE'//ERROR
            CALL WARNING_MESSAGE(OUTPUT=SALT%LOUT,MSG=ERROR, INLINE=TRUE)
            ERROR = NL
        END IF
    END IF
    !
    IF( UPDATE .OR. SALT%MAX_LR_TFR%TRANSIENT) THEN
        IF(SALT%MAX_LR_TFR%INUSE) THEN
            !
            SALT%MAX_LR = SALT%MAX_LR_TFR%LIST
            !
            ASSOCIATE (SFAC=>SALT%MAX_LR_TFR%SFAC)
              !
              IF(SFAC%HAS_ALL) SALT%MAX_LR = SALT%MAX_LR * SFAC%ALL
              IF(SFAC%HAS_EX1) SALT%MAX_LR = SALT%MAX_LR * SFAC%EX1
              !
            END ASSOCIATE
            !
            DO CONCURRENT ( I=ONE:CDAT%NCROP, SALT%MAX_LR(I) > 0.9999D0) 
                           IF(SALT%MAX_LR(I) > 0.99991D0) ERROR = ERROR//BLNK//NUM2STR(I,-8)//NL
                           SALT%MAX_LR(I) = 0.9999D0
            END DO
            IF (ERROR.NE.NL) THEN
                ERROR='FMP SALINTY BLOCK -- "MAX_LEACHING_REQUIREMENT" IS GREATER THAN 0.9999,'//NL//          &
                'THIS COULD RESULT IN A FLOATING POINT OVERFLOW DUE TO DIVISION BY ZERO.'//NL//               &
                'THE "MAX_LEACHING_REQUIREMENT" IS RESET TO 0.9999 FOR THE FOLLOWING CROPS.'//NL//            &
                ' CROP_ID'//ERROR
                CALL WARNING_MESSAGE(OUTPUT=SALT%LOUT,MSG=ERROR, INLINE=TRUE)
                ERROR = NL
            END IF
        ELSE
            SALT%MAX_LR = 0.99D0
        END IF
    END IF
    !
    IF( UPDATE .OR. SALT%STOL%TRANSIENT) THEN
        SALT%ECe = SALT%STOL%LIST
        !
        IF(SALT%STOL%SFAC%HAS_ALL) SALT%ECe = SALT%ECe * SALT%STOL%SFAC%ALL
        IF(SALT%STOL%SFAC%HAS_EX1) SALT%ECe = SALT%ECe * SALT%STOL%SFAC%EX1
        !
        DO CONCURRENT ( I=ONE:CDAT%NCROP, SALT%ECe(I) < NEARZERO_12) 
                       SALT%ECe(I) = NEARZERO_12
                       ERROR = ERROR//BLNK//NUM2STR(I,-8)//NL
        END DO
        IF (ERROR.NE.NL) THEN
            ERROR='FMP SALINTY BLOCK -- "CROP_SALINITY_TOLERANCE" IS LESS THAN 1E-12,'//NL//                                  &
            'THIS IS TOO CLOSE TO ZERO TO COMPUTE A SALINTY IRRIGATION REQUIREMENT (CROP HAS ZERO TOLERANCE TO SALT).'//NL//  &
            'THE "CROP_SALINITY_TOLERANCE" IS RESET TO 1E-12 FOR THE FOLLOWING CROPS.'//NL//                                  &
            ' CROP_ID'//ERROR
            CALL WARNING_MESSAGE(OUTPUT=SALT%LOUT,MSG=ERROR, INLINE=TRUE)
            ERROR = NL
        END IF
    END IF
    !
    IF( UPDATE .OR. SALT%IRR_UNI_TFR%TRANSIENT) THEN
        !
        SALT%IRR_UNI = SALT%IRR_UNI_TFR%ARRAY   !NOTE IT IS STORED AS A LISTARRAY
        !
        ASSOCIATE (SFAC=>SALT%IRR_UNI_TFR%SFAC)
            !
            IF(SFAC%HAS_ALL) SALT%IRR_UNI = SALT%IRR_UNI * SFAC%ALL
            !
            IF(SFAC%HAS_EX1) THEN 
                             DO CONCURRENT (                  I=ONE:SALT%NFARM); SALT%IRR_UNI(:,I) = SALT%IRR_UNI(:,I) * SFAC%EX1(I)
                             END DO
            END IF
            !
            IF(SFAC%HAS_EX2) THEN 
                             DO CONCURRENT (J=ONE:SALT%NIRRG, I=ONE:SALT%NFARM); SALT%IRR_UNI(J,I) = SALT%IRR_UNI(J,I) * SFAC%EX2(J)
                             END DO
            END IF
        END ASSOCIATE
        !
        DO I=ONE, SALT%NFARM
            IF( ANY(SALT%IRR_UNI(:,I)<NEARZERO_30) ) CALL STOP_ERROR(OUTPUT=SALT%LOUT,MSG='IRRIGATION UNIFORMITY (DU) MUST BE GREATER THAN ZER0 (>0). TO PREVENT ANY DIV/0 ERRORS.')
        END DO
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE SETUP_NEXT_TIME_STEP(SALT)
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    INTEGER:: I
    !
    DO CONCURRENT(I=ONE:SALT%NCROP, SALT%LR(I)%N > Z); SALT%LR(I)%VEC = DZ
    END DO
    DO CONCURRENT(I=ONE:SALT%NCROP, SALT%AW(I)%N > Z); SALT%AW(I)%VEC = DZ
    END DO
    SALT%ECw = DZ
    !
  END SUBROUTINE
  !
  SUBROUTINE SETUP_CROP_BY_TIME_STEP(SALT,CDAT) !CROP LOADS BY TIME STEP SO IT COULD CHANGE IRRIGATION FLAGS
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    CLASS(CROP_DATA),     INTENT(IN   ):: CDAT
    INTEGER:: I, K, R, C
    CHARACTER(:),ALLOCATABLE:: ERROR
    LOGICAL, DIMENSION(SALT%NCROP):: PRNT
    !
    IF(SALT%USED%LISTLOAD) THEN
        DO CONCURRENT (I=ONE:SALT%NCROP)
              SALT%INUSE(I)%VEC = SALT%USED%LIST(I) .NE. Z
        END DO
    ELSE
        ASSOCIATE (CPR=>CDAT%CROP, TF=>SALT%INUSE, ARRAY=>SALT%USED%ARRAY)
            DO I=ONE, SALT%NCROP
               if(CPR(I)%N > 0) then
                  CALL CROP_INPUT_ARRAY_TO_LOGICAL_PROP(CPR(I)%N, CPR(I)%RC, TF(I)%VEC, ARRAY(:,CPR(I)%LD:) )
               end if
            END DO
        END ASSOCIATE
    END IF
    !
    SALT%SKIP_SALINITY = TRUE
    DO I=ONE,SALT%NCROP
        IF(ANY(SALT%INUSE(I)%VEC)) THEN
           SALT%SKIP_SALINITY = FALSE
           EXIT
        END IF
    END DO
    !
    IF(SALT%SKIP_SALINITY) RETURN !---------------------------------------------
    !
    ERROR = NL
    !
    IF( CDAT%IRR%LISTLOAD) THEN
        PRNT = TRUE
        DO CONCURRENT (I=ONE:CDAT%NCROP)
        DO CONCURRENT (K=ONE:CDAT%CROP(I)%N, SALT%INUSE(I)%VEC(K) .AND. CDAT%CROP(I)%IRR(K) == Z )
                                                                SALT%INUSE(I)%VEC(K) = FALSE
                                                                IF(PRNT(I)) THEN
                                                                                ERROR = ERROR//BLNK//NUM2STR(I,-8)//NL
                                                                                PRNT(I) = FALSE
                                                                END IF
        END DO; END DO

        IF (ERROR.NE.NL) ERROR='FMP SALINTY BLOCK -- FOUND A CROP THAT HAS SALINITY DEMAND,'//NL//                                  &
                               'BUT IT HAS AN IRRIGATION FLAG EQUAL TO ZERO,'//NL//                                                 &
                               'SO THERE IS NO WAY TO IRRIGATE CROP TO MEET ADDITINAL SALINITY IRRIGATION DEMAND.'//NL//            &
                               'THE FOLLOWING IS A LIST OF CROPS THAT HAD THEIR'//NL//                                              &
                               '"HAS_CROP_SALINITY_DEMAND" FLAG SET FROM TRUE TO FALSE (i.e. 1 BECOMES 0).'//BLN//                  &
                               ' CROP_ID'//ERROR
    ELSE
        DO CONCURRENT (I=ONE:CDAT%NCROP)
        DO CONCURRENT (K=ONE:CDAT%CROP(I)%N, SALT%INUSE(I)%VEC(K) .AND. CDAT%CROP(I)%IRR(K) == Z )
                                                                SALT%INUSE(I)%VEC(K) = FALSE
                                                                R = CDAT%CROP(I)%RC(ONE,K)
                                                                C = CDAT%CROP(I)%RC(TWO,K)
                                                                ERROR = ERROR//BLNK//NUM2STR(I,-8)//NUM2STR(R,-10)//' '//NUM2STR(C,-10)//NL
        END DO; END DO
        IF (ERROR.NE.NL) ERROR='FMP SALINTY BLOCK -- FOUND A CROP THAT HAS SALINITY DEMAND,'//NL//                                   &
                               'BUT IT HAS AN IRRIGATION FLAG EQUAL TO ZERO,'//NL//                                                 &
                               'SO THERE IS NO WAY TO IRRIGATE CROP TO MEET ADDITINAL SALINITY IRRIGATION DEMAND.'//NL//            &
                               'THE FOLLOWING IS A LIST OF CROPS THAT HAD THEIR'//NL//                                              &
                               '"HAS_CROP_SALINITY_DEMAND" FLAG SET FROM TRUE TO FALSE (i.e. 1 BECOMES 0).'//BLN//                  &
                               ' CROP_ID ROW        COLUMN'//ERROR
    END IF
    !
    IF (ERROR.NE.NL) CALL WARNING_MESSAGE(OUTPUT=SALT%LOUT,MSG=ERROR)
    !
    DEALLOCATE(ERROR)
    !
  END SUBROUTINE
  !
  SUBROUTINE CALCULATE_SALINTY_DEMAND(SALT,CDAT,WBS,CLIM) !<== ASSUMES CIR HAS BEEN CALCULATED
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    TYPE(CROP_DATA),      INTENT(INOUT):: CDAT
    TYPE(WBS_DATA),       INTENT(IN   ):: WBS
    TYPE(CLIMATE_DATA),   INTENT(IN   ):: CLIM
    INTEGER:: I, K, N
    DOUBLE PRECISION:: ECw, ETc, DU, MIN_VAR
    CHARACTER(4),     DIMENSION(:), ALLOCATABLE:: NAM
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: VAL
    !
    ! CALCULATE SOURCE WATER SALINITY (ECw) --INCLUDES WATER FROM PREVIOUS SALINITY APPLICATION
    ! SW (mg/L)   GW (mg/L)   NRD (mg/L)   MAGIC (mg/L)       (1dS/m = 640ppm = 640 mg/L)
    !
    DO CONCURRENT (I=ONE:SALT%NFARM)
        IF(WBS%SUPPLY(I)%USED > DZ) THEN
            SALT%ECw(I) =                                             &
                        ( WBS%SUPPLY(I)%SFR   *  SALT%SUP_PPM(1,I) +  &
                          WBS%SUPPLY(I)%WEL   *  SALT%SUP_PPM(2,I) +  &
                          WBS%SUPPLY(I)%NRD   *  SALT%SUP_PPM(3,I) +  &
                          WBS%SUPPLY(I)%MAGIC *  SALT%SUP_PPM(4,I)   ) / WBS%SUPPLY(I)%USED
        ELSEIF(WBS%SUPPLY(I)%HAS_MAGIC) THEN
            SALT%ECw(I) = ( SALT%SUP_PPM(1,I) + SALT%SUP_PPM(2,I) + SALT%SUP_PPM(3,I) + SALT%SUP_PPM(4,I) ) / 4.D0
            
        ELSE
            SALT%ECw(I) = ( SALT%SUP_PPM(1,I) + SALT%SUP_PPM(2,I) + SALT%SUP_PPM(3,I) ) / 3.D0
        END IF
    END DO
    !
    SALT%ECw = SALT%ECw / 640.D0 !mg/l  => dS/m
    !
    IF(SALT%SKIP_SALINITY) RETURN  !NOTHING HAS SALINTIY
    !
    ! CONSTRUCT CUSTOM EQUATION VARIABLES IF NECESSARY
    !
    IF(SALT%HAS_CUSTOM_LR .OR. SALT%HAS_CUSTOM_AW) THEN
        N = 24
        ALLOCATE(VAL(N))
        ALLOCATE(NAM,   SOURCE =  &
        [       &  
        'LR  ', &  ! 1 
        'DU  ', &  ! 2
        'CIR ', &  ! 3  
        'ECW ', &  ! 4 
        'ECE ', &  ! 5 
        'CU  ', &  ! 6 
        'DMD ', &  ! 7 
        'ETP ', &  ! 8 
        'ETC ', &  ! 9 
        'P   ', &  ! 10
        'AREA', &  ! 11
        'TGW ', &  ! 12
        'TP  ', &  ! 13
        'TI  ', &  ! 14
        'EFF ', &  ! 15
        'ROOT', &  ! 16
        'DP_P', &  ! 17
        'DP_I', &  ! 18
        'DP  ', &  ! 19
        'ADRS', &  ! 20
        'ETI ', &  ! 21
        'ADMD', &  ! 22
        'ETR ', &  ! 23
        'CAPF'  &  ! 24
        ])
        MIN_VAR = SALT%MIN_VARIABLE
        IF(MIN_VAR>DZ) MIN_VAR = DNEG*MIN_VAR
    END IF
    !
    ! CALCULATE LEACHING REQUIREMENT (LR)
    !
    !!!DO I=ONE, CDAT%NCROP
    !!!DO K=ONE, CDAT%CROP(I)%N
    !!!!
    !!!IRR = CDAT%CROP(I)%IRR(K)
    !!!IF(SALT%INUSE(I)%VEC(K) .AND. IRR > Z) THEN
    !!!    !
    !!!    SALT%LR(I)%VEC(K) = DZ
    !!!    !
    !!!    SELECT CASE(SALT%LR_EXP(I)%EVAL)
    !!!    !CASE(Z)  ! SKIP
    !!!    !         SALT%LR(I)%VEC(K) = DZ
    !!!    CASE(ONE) ! SPECIFIC VALUE
    !!!              !
    !!!              SALT%LR(I)%VEC(K) = SALT%LR_EXP(I)%VAL
    !!!              !
    !!!    CASE(TWO) ! RHOADES EQUATION => LR = ECw/(5*ECe - ECw)
    !!!              !                    
    !!!              ECw = SALT%ECw( CDAT%CROP(I)%FID(K) )
    !!!              !
    !!!              SALT%LR(I)%VEC(K) = ECw / (FIVE*SALT%ECe(I) - ECw)
    !!!    CASE(NEG) ! CUSTOM EQUATION
    !!!              ! 
    !!!              CALL SET_VAL(N, VAL, I, K, SALT, CDAT, WBS, MIN_VAR, SALT%MIN_VARIABLE)
    !!!              ! 
    !!!              SALT%LR(I)%VEC(K) = EVAL(SALT%LR_EXP(I)%EXP,NAM,VAL,FALSE,FALSE) 
    !!!    END SELECT
    !!!    !
    !!!END IF
    !!!END DO
    !!!END DO
    !
    DO CONCURRENT (I=ONE:CDAT%NCROP, CDAT%CROP(I)%N > Z); SALT%LR(I)%VEC = DZ
    END DO
    !
    IF(SALT%HAS_NON_CUSTOM) THEN
          DO CONCURRENT (I=ONE:CDAT%NCROP    )
          DO CONCURRENT (K=ONE:CDAT%CROP(I)%N)
              !
              IF(SALT%INUSE(I)%VEC(K)) THEN
                  !
                  SELECT CASE(SALT%LR_EXP(I)%EVAL)
                  !CASE(Z)  ! SKIP
                  !         SALT%LR(I)%VEC(K) = DZ
                  CASE(ONE) ! SPECIFIC VALUE
                            !
                            SALT%LR(I)%VEC(K) = SALT%LR_EXP(I)%VAL
                            !
                  CASE(TWO) ! RHOADES EQUATION => LR = ECw/(5*ECe - ECw)
                            !                    
                            ECw = SALT%ECw( CDAT%CROP(I)%FID(K) )
                            !
                            SALT%LR(I)%VEC(K) = ECw / (FIVE*SALT%ECe(I) - ECw)
                  END SELECT
              END IF
              !
          END DO; END DO
    END IF
    !
    IF (SALT%HAS_CUSTOM_LR) THEN
       DO I=ONE,CDAT%NCROP
          IF(SALT%LR_EXP(I)%EVAL==NEG .AND. CDAT%CROP(I)%N > Z) THEN
              DO K=ONE, CDAT%CROP(I)%N
                                      IF(SALT%INUSE(I)%VEC(K)) THEN
                                            CALL SET_VAL(N, VAL, I, K, SALT, CDAT, WBS, CLIM, MIN_VAR, SALT%MIN_VARIABLE)
                                            ! 
                                            SALT%LR(I)%VEC(K) = EVAL(SALT%LR_EXP(I)%EXP,NAM,VAL) 
                                      END IF
              END DO
          END IF
       END DO
    END IF
    !
    DO CONCURRENT (I=ONE:CDAT%NCROP, SALT%LR_EXP(I)%SF.NE.UNO); SALT%LR(I)%VEC = SALT%LR(I)%VEC * SALT%LR_EXP(I)%SF
    END DO
    !
    DO CONCURRENT (I=ONE:CDAT%NCROP)
    DO CONCURRENT (K=ONE:CDAT%CROP(I)%N)
        !
        IF(SALT%LR(I)%VEC(K) > SALT%MAX_LR(I)) SALT%LR(I)%VEC(K) = SALT%MAX_LR(I)
        IF(SALT%LR(I)%VEC(K) < DZ            ) SALT%LR(I)%VEC(K) = DZ
        !
        !SALT%LR(I)%VEC(K) = UNO - SALT%LR(I)%VEC(K)  !NOTE THIS IS STORED AS 1 - LR
    END DO; END DO
    !
    ! CALCULATE ADDITIPONAL APPLIED WATER (AW)
    !
    !!!DO I=ONE, CDAT%NCROP
    !!!DO K=ONE, CDAT%CROP(I)%N
    !!!!
    !!!IRR = CDAT%CROP(I)%IRR(K)
    !!!IF(SALT%INUSE(I)%VEC(K) .AND. IRR > Z) THEN
    !!!    !
    !!!    SELECT CASE(SALT%AW_EXP(I)%EVAL)
    !!!    CASE(Z)   ! SKIP
    !!!              SALT%AW(I)%VEC(K) = DZ
    !!!    CASE(ONE) ! SPECIFIC VALUE
    !!!              !
    !!!              SALT%AW(I)%VEC(K) = SALT%AW_EXP(I)%VAL
    !!!              !
    !!!    CASE(TWO) ! RHOADES EQUATION => AW = (ETc/DU)/(1-LR)
    !!!              !
    !!!              IF(CDAT%CROP(I)%TI(K) > NEARZERO_12) THEN
    !!!                   ETc = CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K))  !ONLY IRRIGATION ET
    !!!                   DU  = SALT%IRR_UNI(IRR,CDAT%CROP(I)%FID(K))
    !!!                   !
    !!!                   SALT%AW(I)%VEC(K) = ETc / ( DU*(UNO-SALT%LR(I)%VEC(K)) ) - ETc  
    !!!              ELSE
    !!!                  SALT%AW(I)%VEC(K) = DZ
    !!!              END IF
    !!!    CASE(NEG) ! CUSTOM EQUATION
    !!!              !
    !!!              CALL SET_VAL(N, VAL, I, K, SALT, CDAT, WBS, MIN_VAR, SALT%MIN_VARIABLE)
    !!!              !
    !!!              SALT%AW(I)%VEC(K) = EVAL(SALT%AW_EXP(I)%EXP,NAM,VAL,FALSE,FALSE) 
    !!!    END SELECT
    !!!    !
    !!!END IF
    !!!END DO
    !!!END DO
    DO CONCURRENT (I=ONE:CDAT%NCROP, CDAT%CROP(I)%N > Z); SALT%AW(I)%VEC = DZ
    END DO
    !
    IF(SALT%HAS_NON_CUSTOM) THEN
       DO CONCURRENT (I=ONE:CDAT%NCROP    )
       DO CONCURRENT (K=ONE:CDAT%CROP(I)%N)
           !
           IF(SALT%INUSE(I)%VEC(K)) THEN
                !
               SELECT CASE(SALT%AW_EXP(I)%EVAL)
               !CASE(Z)   ! SKIP
               !          SALT%AW(I)%VEC(K) = DZ
               CASE(ONE) ! SPECIFIC VALUE
                         !
                         SALT%AW(I)%VEC(K) = SALT%AW_EXP(I)%VAL
                         !
               CASE(TWO) ! RHOADES EQUATION => AW = (ETc/DU)/(1-LR)
                         !
                         IF(CDAT%CROP(I)%TI(K) > NEARZERO_12) THEN
                              ETc = CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K))  !ONLY IRRIGATION ET
                              DU  = SALT%IRR_UNI(CDAT%CROP(I)%IRR(K),CDAT%CROP(I)%FID(K))
                              !
                              SALT%AW(I)%VEC(K) = ETc / ( DU*(UNO-SALT%LR(I)%VEC(K)) ) - ETc  
                         ELSE
                             SALT%AW(I)%VEC(K) = DZ
                         END IF
               END SELECT
           END IF
           !
       END DO; END DO
    END IF
    !
    IF (SALT%HAS_CUSTOM_AW) THEN
       DO I=ONE,CDAT%NCROP
          IF(SALT%AW_EXP(I)%EVAL==NEG .AND. CDAT%CROP(I)%N > Z) THEN
              DO K=ONE, CDAT%CROP(I)%N
                                      IF(SALT%INUSE(I)%VEC(K)) THEN
                                            !
                                            CALL SET_VAL(N, VAL, I, K, SALT, CDAT, WBS, CLIM, MIN_VAR, SALT%MIN_VARIABLE)
                                            !
                                            SALT%AW(I)%VEC(K) = EVAL(SALT%AW_EXP(I)%EXP,NAM,VAL) 
                                      END IF
              END DO
          END IF
       END DO
    END IF
    !
    DO CONCURRENT (I=ONE:CDAT%NCROP, SALT%AW_EXP(I)%SF.NE.UNO); SALT%AW(I)%VEC = SALT%AW(I)%VEC * SALT%AW_EXP(I)%SF
    END DO
    !
    DO CONCURRENT (I=ONE:CDAT%NCROP)
    DO CONCURRENT (K=ONE:CDAT%CROP(I)%N, SALT%INUSE(I)%VEC(K) .AND. SALT%AW(I)%VEC(K) > NEARZERO_12)
            !
            CALL CDAT%ADD_EXTERNAL_DEMAND(SALT%AW(I)%VEC(K),I,K,Z,FALSE)
    END DO; END DO
    !    
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_VAL(N, VAL, I, K, SALT, CDAT, WBS, CLIM, MIN1, MIN2) ! Populates array VAL
    INTEGER,                        INTENT(IN ):: N
    DOUBLE PRECISION, DIMENSION(N), INTENT(OUT):: VAL
    INTEGER,                        INTENT(IN ):: I, K
    CLASS(SALINITY_DATA),           INTENT(IN ):: SALT
    TYPE(CROP_DATA),                INTENT(IN ):: CDAT
    TYPE(WBS_DATA),                 INTENT(IN ):: WBS
    TYPE(CLIMATE_DATA),             INTENT(IN ):: CLIM
    DOUBLE PRECISION,               INTENT(IN ):: MIN1, MIN2
    INTEGER:: R, C
    !
    R = CDAT%CROP(I)%RC(ONE,K)
    C = CDAT%CROP(I)%RC(TWO,K)
    !
    VAL(1 ) = SALT%LR(I)%VEC(K)                                    ! LR  
    VAL(2 ) = SALT%IRR_UNI(CDAT%CROP(I)%IRR(K),CDAT%CROP(I)%FID(K))! DU
    VAL(3 ) = CDAT%CROP(I)%CIR(K)                                  ! CIR 
    VAL(4 ) = SALT%ECw( CDAT%CROP(I)%FID(K) )                      ! ECw 
    VAL(5 ) = SALT%ECe(I)                                          ! ECe 
    VAL(6 ) = CDAT%CROP(I)%CU(K)                                   ! CU  
    VAL(7 ) = CDAT%CROP(I)%DEMAND(K)                               ! DMD 
    VAL(8 ) = CDAT%CROP(I)%EP(K) + CDAT%CROP(I)%TP(K)              ! ETp 
    VAL(9 ) = VAL(3) + VAL(8)                                      ! ETc 
    VAL(10) = CDAT%CROP(I)%PRECIP(K)                               ! P
    IF(CDAT%HAS_Pe) VAL(10) = VAL(10) + CDAT%CROP(I)%RNOFF_Peff(K) !    <-- Account for Precip removed cause of potential consumption limits
    VAL(11) = CDAT%CROP(I)%AREA(K)                                 ! AREA
    VAL(12) = CDAT%CROP(I)%TGWA(K)                                 ! TGW 
    VAL(13) = CDAT%CROP(I)%TP(K)                                   ! TP  
    VAL(14) = CDAT%CROP(I)%TI(K)                                   ! TI  
    VAL(15) = CDAT%CROP(I)%EFF(K)                                  ! EFF 
    VAL(16) = CDAT%CROP(I)%ROOT(K)                                 ! ROOT
    !                                                              
    VAL(17) = CDAT%CROP(I)%PRECIP(K) - VAL(8)                      ! DP_P
    VAL(17) = VAL(17) * (UNO - CDAT%CROP(I)%FIESWP(K))             ! 
    IF(VAL(17) < NEARZERO_15) VAL(17) = DZ                         ! DP_P
    !
    VAL(18) = CDAT%CROP(I)%DEMAND(K) * (UNO - CDAT%CROP(I)%EFF(K)) * (UNO - CDAT%CROP(I)%FIESWI(K)) ! DP_I  --Inefficient losses to dp
    IF(CDAT%CROP(I)%DEMAND_EXT(K) > DZ) THEN                       !
             VAL(18) = VAL(18)  &
                     + CDAT%CROP(I)%DEMAND_EXT(K) * (UNO - CDAT%CROP(I)%EFF(K)) * (UNO - CDAT%CROP(I)%FIESWI(K))  & ! --Inefficient extra losses to dp
                     + CDAT%CROP(I)%DEMAND_EXT(K) *        CDAT%CROP(I)%EFF(K)  * (UNO - CDAT%CROP(I)%ADRF(K))      ! --Extra losses to dp
    END IF                                                         !
    IF(VAL(18) < NEARZERO_15) VAL(18) = DZ                         ! DP_I
    !
    VAL(19) = VAL(17) + VAL(18)                                    ! DP
    !
    VAL(20) = CDAT%CROP(I)%ADRF(K)                                 ! ADRS
    VAL(21) = VAL(7 ) * VAL(15)                                    ! ETI 
    VAL(22) = CDAT%CROP(I)%ADMD(K)                                 ! ADMD
    IF(CLIM%HAS_REF_ET) THEN
         VAL(23) = CLIM%REF_ET(C,R)                                ! ETR
    ELSE
         VAL(23) = MIN2
    END IF    
    VAL(24) = CDAT%CROP(I)%SS(K)-CDAT%CROP(I)%LXX(K)               ! CAPF
    ! 
    !
    IF(MIN2 > NEARZERO_30) THEN
       DO CONCURRENT(R=ONE:N, MIN1 < VAL(R) .AND. VAL(R) < MIN2)
           IF(VAL(R)<DZ) THEN
               VAL(R) = MIN1
           ELSE
               VAL(R) = MIN2
           END IF
       END DO
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE CROP_INPUT_ARRAY_TO_LOGICAL_PROP(N, RC, PROP, ARRAY)  !CLONE OF CROP_INPUT_ARRAY_TO_PROP ROUTINE LOCATED IN CROP MODULE
    INTEGER,                   INTENT(IN ):: N
    INTEGER, DIMENSION(TWO,N), INTENT(IN ):: RC
    LOGICAL, DIMENSION(N),     INTENT(OUT):: PROP
    INTEGER, DIMENSION(:,:),   INTENT(IN ):: ARRAY
    CONTIGUOUS:: ARRAY
    INTEGER:: K
    !
    DO CONCURRENT (K=ONE:N)
                         PROP(K) = ARRAY( RC(TWO,K), RC(ONE,K) ).NE.Z  !NOTE ARRAY IS STORED AS NCOL, NROW
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_OUT_BYFARM(SALT,CDAT, WBS, KPER, KSTP, DELT, DYEAR, DATE)
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    CLASS(CROP_DATA),     INTENT(IN   ):: CDAT
    TYPE(WBS_DATA),       INTENT(IN   ):: WBS
    INTEGER,              INTENT(IN   ):: KPER, KSTP
    DOUBLE PRECISION,     INTENT(IN   ):: DELT
    DOUBLE PRECISION,     INTENT(IN   ):: DYEAR
    CHARACTER(*),         INTENT(IN   ):: DATE
    INTEGER:: IU, F, I, J, K, IRR
    DOUBLE PRECISION:: AREA, CU, CIR, DMD, CIRI, DMDI, LR, ECe, ECw, AW, LF, P, DP, DP_TOT, SALT_AREA, ETi, DMD_FRAC, DU, CDMD, PIN
    CHARACTER(17):: ZER, DT
    CHARACTER(14):: PC

    !
    IF(SALT%OUT_BYFARM%IU == Z) RETURN  !NOTHING TO PRINT OUT
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    ZER = '0.0'; ZER = ADJUSTR(ZER)
    !DDT = DBLE(DELT)
    !
    CALL SALT%OUT_BYFARM%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IU = SALT%OUT_BYFARM%IU
    !
    IF( (KPER==ONE .AND. KSTP==ONE) .OR. SALT%IOUT==IU )  THEN
        IF(SALT%OUT_BYFARM%BINARY) THEN          
            WRITE(SALT%IOUT,'(A,/A)')'SALINITY IRRIGATION INFORMATION BY WBS OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',"DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), WBS ID (int), PERCENT INCREASE (double), WBS AREA (double), CROP AREA (double), IRRIGATED AREA (double), SALINITY AREA (double), EFFICIENCY (double), PRECIPITATION (double), TOTAL IRRIGATION (double), TOTAL DEEP PERCOLOTION (double), LEACH FRACTION (double), CU (double), ET FROM IRRIGATION (double), SALT_REQ_IRR (double), SALT_IRR (double), SALT_REQ_DEMAND (double), SALT_DEMAND (double), LEACH REQUIREMENT (double), ECe (double), ECw (double), IRRIGATION UNIFORMITY (double)"
        ELSE
            !
            IF (SALT%IOUT==IU) WRITE(IU,*)
            !
            CALL SALT%OUT_BYFARM%SET_HEADER( '    PER    STP    WBS  DEMAND%CHANGE       FARM_AREA        CROP_AREA   IRRIGATED_AREA    SALINITY_AREA       EFFICIENCY    PRECIPITATION TOTAL_IRRIGATION  TOTAL_DEEP_PERC   LEACH_FRACTION               CU           ET_IRR     SALT_REQ_IRR         SALT_IRR  SALT_REQ_DEMAND      SALT_DEMAND        LEACH_REQ              ECe              ECw   IRR_UNIFORMITY             DELT   DYEAR            DATE_START' )
        END IF
    END IF
    !
    DO F=ONE, WBS%NFARM
       AREA= DZ
       CU  = DZ
       CIR = DZ
       DMD = DZ
       CIRI= DZ
       DMDI= DZ
       LR = DZ
       ECe =DZ
       ECw =DZ
       AW  =DZ
       LF  =DZ
       P   =DZ
       DP_TOT=DZ
       SALT_AREA = DZ
       ETi = DZ
       DMD_FRAC = UNO
       DU = DZ
       CDMD=DZ
       DO CONCURRENT(J=ONE:WBS%CROP(F)%N) 
           I=WBS%CROP(F)%PNT(ONE,J)
           K=WBS%CROP(F)%PNT(TWO,J)
           IRR = CDAT%CROP(I)%IRR(K)
           !
           AREA= AREA + CDAT%CROP(I)%AREA(K)
           CU  = CU   + CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K)) + CDAT%CROP(I)%TP(K) + CDAT%CROP(I)%EP(K) + CDAT%CROP(I)%TGWA(K) + CDAT%CROP(I)%EGWA(K) !TRAN*CDAT%CROP(I)%FTR(K) + TRAN*(UNO - CDAT%CROP(I)%FTR(K))
           ETi = ETi  + CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K))  !ONLY IRRIGATION ET
           !
           P   = P    + CDAT%CROP(I)%PRECIP(K)
           IF(CDAT%HAS_Pe)  P = P + CDAT%CROP(I)%RNOFF_Peff(K)
           !
           AW  = AW   + CDAT%CROP(I)%DEMAND(K) + CDAT%CROP(I)%DEMAND_EXT(K)  
           CDMD= CDMD + CDAT%CROP(I)%DEMAND(K)
           !DMD_FRAC, LF, LR, AW, ECe, ECw, DU
           ! 
           IF(SALT%INUSE(I)%VEC(K)) THEN
               !
               IF(CDAT%HAS_DEMAND_EXT) THEN
                   IF(NEAR_ZERO(CDAT%CROP(I)%DEMAND_EXT_INI(K))) THEN
                       DMD_FRAC=DZ
                   ELSE
                       DMD_FRAC = CDAT%CROP(I)%DEMAND_EXT(K) / CDAT%CROP(I)%DEMAND_EXT_INI(K)
                       IF(NEAR_ZERO(DMD_FRAC)) DMD_FRAC=DZ
                   END IF
               END IF
               !
               SALT_AREA = SALT_AREA + CDAT%CROP(I)%AREA(K)
               LR = LR + SALT%LR(I)%VEC(K) * CDAT%CROP(I)%AREA(K)
               !
               CIRI= CIRI + SALT%AW(I)%VEC(K)
               CIR = CIR  + SALT%AW(I)%VEC(K)*DMD_FRAC
               !
               ECe = ECe + SALT%ECe(I)*CDAT%CROP(I)%AREA(K)
               ECw = ECw + SALT%ECw(I)*CDAT%CROP(I)%AREA(K)
               DU  = DU  + SALT%IRR_UNI(IRR,CDAT%CROP(I)%FID(K))*CDAT%CROP(I)%AREA(K)
               !
               IF(CDAT%CROP(I)%EFF(K)>DZ) THEN
                   DMDI = DMDI + SALT%AW(I)%VEC(K)/CDAT%CROP(I)%EFF(K)
                   DMD  = DMD  + (SALT%AW(I)%VEC(K)/CDAT%CROP(I)%EFF(K))*DMD_FRAC
               END IF
           END IF
           !
           DP = CDAT%CROP(I)%PRECIP(K) - CDAT%CROP(I)%TP(K) - CDAT%CROP(I)%EP(K)  ! 'DP_P'
           DP = DP * (UNO - CDAT%CROP(I)%FIESWP(K))         ! 
           IF(DP < NEARZERO_12) DP = DZ                     ! 'DP_P'
           !
           DP_TOT = DP_TOT + DP
           !
           DP = CDAT%CROP(I)%DEMAND(K) * (UNO - CDAT%CROP(I)%EFF(K)) * (UNO - CDAT%CROP(I)%FIESWI(K)) ! 'DP_I'  --Inefficient losses to dp
           IF(CDAT%CROP(I)%DEMAND_EXT(K) > DZ) THEN                       !
                    DP = DP  &
                            + CDAT%CROP(I)%DEMAND_EXT(K) * (UNO - CDAT%CROP(I)%EFF(K)) * (UNO - CDAT%CROP(I)%FIESWI(K))  & ! --Inefficient extra losses to dp
                            + CDAT%CROP(I)%DEMAND_EXT(K) *        CDAT%CROP(I)%EFF(K)  * (UNO - CDAT%CROP(I)%ADRF(K))      ! --Extra losses to dp
           END IF                                                         !
           IF(DP < NEARZERO_12) DP = DZ                         ! 'DP_I'
           DP_TOT = DP_TOT + DP
           !
       END DO
       !
       IF(SALT_AREA > DZ) THEN
               LR  = LR  / SALT_AREA
               ECe = ECe / SALT_AREA
               ECw = ECw / SALT_AREA
               DU  = DU  / SALT_AREA
       END IF
       !
       IF(AW+P>DZ) THEN
           LF = DP_TOT/(AW+P)
       ELSE
           LF = DZ
       END IF
       !
       IF(CDMD>NEARZERO_10 .AND. DMD>UNO) THEN
           !
           PIN = (DMD/CDMD)*100.D0
           !
           IF(PIN < 100000.D0) THEN
                WRITE(PC,'(1x,F12.2)') PIN
           ELSE
               WRITE(PC,'(1x,ES12.2)') PIN
           END IF
       ELSE
           PC='         0.00'
           PIN=DZ
       END IF
       !
       IF(SALT%OUT_BYFARM%BINARY) THEN
           IF(WBS%FID(F)%Count == Z) THEN
               WRITE(IU) DATE, DYEAR, DELT, KPER, KSTP, F, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ, DZ
           ELSE
               WRITE(IU) DATE, DYEAR, DELT, KPER, KSTP, F,PIN, WBS%FID(F)%AREA, AREA, WBS%IRR_AREA(F), SALT_AREA, WBS%EFF(F), P, AW, DP_TOT, LF, CU, ETi, CIRI, CIR, DMDI, DMD, LR, ECe, ECw, DU
           END IF   
       ELSEIF(WBS%FID(F)%Count == Z) THEN
               !
               PC='        0.00'
               WRITE(IU, '(3I7, A, 20A17, 2x,F13.7, 2x,A)') KPER, KSTP, F, PC, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, ZER, DT, DYEAR, DATE
       ELSE
               WRITE(IU, '(3I7, A, 20A17, 2x,F13.7, 2x,A)') KPER, KSTP, F, PC, NUM2STR(WBS%FID(F)%AREA), NUM2STR(AREA), NUM2STR(WBS%IRR_AREA(F)), NUM2STR(SALT_AREA), NUM2STR(WBS%EFF(F)), NUM2STR(P), NUM2STR(AW), NUM2STR(DP_TOT), NUM2STR(LF), NUM2STR(CU), NUM2STR(ETi), NUM2STR(CIRI), NUM2STR(CIR), NUM2STR(DMDI), NUM2STR(DMD), NUM2STR(LR), NUM2STR(ECe), NUM2STR(ECw), NUM2STR(DU), DT, DYEAR, DATE
       END IF
       
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_OUT_BYFARM_BYCROP(SALT, CDAT, WBS, KPER, KSTP, DELT, DYEAR, DATE)
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    CLASS(CROP_DATA),     INTENT(IN   ):: CDAT
    TYPE(WBS_DATA),       INTENT(IN   ):: WBS
    INTEGER,              INTENT(IN   ):: KPER, KSTP
    DOUBLE PRECISION,     INTENT(IN   ):: DELT
    DOUBLE PRECISION,     INTENT(IN   ):: DYEAR
    CHARACTER(*),         INTENT(IN   ):: DATE
    INTEGER:: IU, F, I, J, K, L, IRR
    DOUBLE PRECISION::  AREA, AREA_IRR, CU, CIR, DMD, CIRI, DMDI, LR, ECe, ECw, AW, LF, P, DP, DP_TOT, SALT_AREA, ETi, DMD_FRAC, DU, PIN, CDMD
    CHARACTER(17):: ZER, DT
    CHARACTER(14):: PC
    LOGICAL:: FOUND
    !
    IF(SALT%OUT_BYFARMCROP%IU == Z) RETURN  !NOTHING TO PRINT OUT
    !
    L  = CDAT%CROP_NAME_LEN
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    ZER = '0.0'; ZER = ADJUSTR(ZER)
    !DDT = DBLE(DELT)
    !
    CALL SALT%OUT_BYFARMCROP%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IU = SALT%OUT_BYFARMCROP%IU
    !
    IF( (KPER==ONE .AND. KSTP==ONE) .OR. SALT%IOUT==IU )  THEN
        IF(SALT%OUT_BYFARMCROP%BINARY) THEN          
            WRITE(SALT%IOUT,'(A,/A)')'SALINITY IRRIGATION BY WBS BY CROP OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',"DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), WBS ID (int), CROP ID (int), CROP_NAME (20 char), PERCENT INCREASE (double), CROP AREA (double), IRRIGATED AREA (double), SALINITY AREA (double), PRECIPITATION (double), TOTAL IRRIGATION (double), TOTAL DEEP PERCOLOTION (double), LEACH FRACTION (double), CU (double), ET FROM IRRIGATION (double), SALT_REQ_IRR (double), SALT_IRR (double), SALT_REQ_DEMAND (double), SALT_DEMAND (double), LEACH REQUIREMENT (double), ECe (double), ECw (double), IRRIGATION UNIFORMITY (double)"
        ELSE
            !
            IF (SALT%IOUT==IU) WRITE(IU,*)
            !                                                                                  
            CALL SALT%OUT_BYFARMCROP%SET_HEADER( '    PER    STP    WBS   CROP  CROP_NAME'//REPEAT(' ',L-7)//'DEMAND%CHANGE      CROP_AREA   IRRIGATED_AREA    SALINITY_AREA    PRECIPITATION TOTAL_IRRIGATION  TOTAL_DEEP_PERC   LEACH_FRACTION               CU           ET_IRR     SALT_REQ_IRR         SALT_IRR  SALT_REQ_DEMAND      SALT_DEMAND        LEACH_REQ              ECe              ECw   IRR_UNIFORMITY             DELT   DYEAR            DATE_START' )
        END IF
    END IF
    !
    DO F=ONE, WBS%NFARM
    DO I=ONE, CDAT%NCROP
       AREA= DZ
       AREA_IRR=DZ
       SALT_AREA = DZ
       CU  = DZ
       CIR = DZ
       DMD = DZ
       CIRI= DZ
       DMDI= DZ
       LR = DZ
       ECe =DZ
       ECw =DZ
       AW  =DZ
       LF  =DZ
       P   =DZ
       DP_TOT=DZ
       ETi = DZ
       DMD_FRAC = UNO
       DU = DZ
       CDMD=DZ
       FOUND = FALSE
       DO CONCURRENT(J=ONE:WBS%CROP(F)%N, WBS%CROP(F)%PNT(ONE,J) == I)
           K=WBS%CROP(F)%PNT(TWO,J)
           FOUND = TRUE
           IRR = CDAT%CROP(I)%IRR(K)
           !
           AREA= AREA + CDAT%CROP(I)%AREA(K)
           IF(IRR>Z) AREA_IRR = AREA_IRR + CDAT%CROP(I)%AREA(K)
           CU  = CU   + CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K)) + CDAT%CROP(I)%TP(K) + CDAT%CROP(I)%EP(K) + CDAT%CROP(I)%TGWA(K) + CDAT%CROP(I)%EGWA(K) !TRAN*CDAT%CROP(I)%FTR(K) + TRAN*(UNO - CDAT%CROP(I)%FTR(K))
           ETi = ETi  + CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K))  !ONLY IRRIGATION ET
           !
           P   = P    + CDAT%CROP(I)%PRECIP(K)
           IF(CDAT%HAS_Pe)  P = P + CDAT%CROP(I)%RNOFF_Peff(K)
           !
           AW  = AW   + CDAT%CROP(I)%DEMAND(K) + CDAT%CROP(I)%DEMAND_EXT(K)  
           CDMD= CDMD + CDAT%CROP(I)%DEMAND(K)
           !DMD_FRAC, LF, LR, AW, ECe, ECw, DU
           ! 
           IF(SALT%INUSE(I)%VEC(K)) THEN
               !
               IF(CDAT%HAS_DEMAND_EXT) THEN
                   IF(NEAR_ZERO(CDAT%CROP(I)%DEMAND_EXT_INI(K))) THEN
                       DMD_FRAC=DZ
                   ELSE
                       DMD_FRAC = CDAT%CROP(I)%DEMAND_EXT(K) / CDAT%CROP(I)%DEMAND_EXT_INI(K)
                       IF(NEAR_ZERO(DMD_FRAC)) DMD_FRAC=DZ
                   END IF
               END IF
               !
               SALT_AREA = SALT_AREA + CDAT%CROP(I)%AREA(K)
               LR = LR + SALT%LR(I)%VEC(K) * CDAT%CROP(I)%AREA(K)
               !
               CIRI= CIRI + SALT%AW(I)%VEC(K)
               CIR = CIR  + SALT%AW(I)%VEC(K)*DMD_FRAC
               !
               ECe = ECe + SALT%ECe(I)*CDAT%CROP(I)%AREA(K)
               ECw = ECw + SALT%ECw(I)*CDAT%CROP(I)%AREA(K)
               DU  = DU  + SALT%IRR_UNI(IRR,CDAT%CROP(I)%FID(K))*CDAT%CROP(I)%AREA(K)
               !
               IF(CDAT%CROP(I)%EFF(K)>DZ) THEN
                   DMDI = DMDI + SALT%AW(I)%VEC(K)/CDAT%CROP(I)%EFF(K)
                   DMD  = DMD  + (SALT%AW(I)%VEC(K)/CDAT%CROP(I)%EFF(K))*DMD_FRAC
               END IF
           END IF
           !
           DP = CDAT%CROP(I)%PRECIP(K) - CDAT%CROP(I)%TP(K) - CDAT%CROP(I)%EP(K)  ! 'DP_P'
           DP = DP * (UNO - CDAT%CROP(I)%FIESWP(K))         ! 
           IF(DP < NEARZERO_12) DP = DZ                     ! 'DP_P'
           !
           DP_TOT = DP_TOT + DP
           !
           DP = CDAT%CROP(I)%DEMAND(K) * (UNO - CDAT%CROP(I)%EFF(K)) * (UNO - CDAT%CROP(I)%FIESWI(K)) ! 'DP_I'  --Inefficient losses to dp
           IF(CDAT%CROP(I)%DEMAND_EXT(K) > DZ) THEN                       !
                    DP = DP  &
                            + CDAT%CROP(I)%DEMAND_EXT(K) * (UNO - CDAT%CROP(I)%EFF(K)) * (UNO - CDAT%CROP(I)%FIESWI(K))  & ! --Inefficient extra losses to dp
                            + CDAT%CROP(I)%DEMAND_EXT(K) *        CDAT%CROP(I)%EFF(K)  * (UNO - CDAT%CROP(I)%ADRF(K))      ! --Extra losses to dp
           END IF                                                         !
           IF(DP < NEARZERO_12) DP = DZ                         ! 'DP_I'
           DP_TOT = DP_TOT + DP
           !
       END DO
       !
       IF(FOUND) THEN
               IF(SALT_AREA > DZ) THEN
                       LR  = LR  / SALT_AREA
                       ECe = ECe / SALT_AREA
                       ECw = ECw / SALT_AREA
                       DU  = DU  / SALT_AREA
               END IF
               !
               IF(AW+P>DZ) THEN
                   LF = DP_TOT/(AW+P)
               ELSE
                   LF = DZ
               END IF
               !
               IF(CDMD>NEARZERO_10 .AND. DMD>UNO) THEN
                   !
                   PIN = (DMD/CDMD)*100.D0
                   !
                   IF(PIN < 100000.D0) THEN
                        WRITE(PC,'(1x,F12.2)') PIN
                   ELSE
                       WRITE(PC,'(1x,ES12.2)') PIN
                   END IF
               ELSE
                   PC='         0.00'
                   PIN=DZ
               END IF
               !
               IF(SALT%OUT_BYFARMCROP%BINARY) THEN
                   WRITE(IU) DATE, DYEAR, DELT, KPER, KSTP, F, I, CDAT%CROP_NAME(I), PIN, AREA, WBS%IRR_AREA(F), SALT_AREA, P, AW, DP_TOT, LF, CU, ETi, CIRI, CIR, DMDI, DMD, LR, ECe, ECw, DU 
               ELSE
                       WRITE(IU, '(4I7, 2x,A, A, 18A17, 2x,F13.7, 2x,A)') KPER, KSTP, F, I, CDAT%CROP_NAME(I)(:L), PC, NUM2STR(AREA), NUM2STR(WBS%IRR_AREA(F)), NUM2STR(SALT_AREA), NUM2STR(P), NUM2STR(AW), NUM2STR(DP_TOT), NUM2STR(LF), NUM2STR(CU), NUM2STR(ETi), NUM2STR(CIRI), NUM2STR(CIR), NUM2STR(DMDI), NUM2STR(DMD), NUM2STR(LR), NUM2STR(ECe), NUM2STR(ECw), NUM2STR(DU), DT, DYEAR, DATE
               END IF
       END IF
    END DO
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_OUT_ALL_CROP(SALT,CDAT, WBS, KPER, KSTP, DELT, DYEAR, DATE)
    CLASS(SALINITY_DATA), INTENT(INOUT):: SALT
    CLASS(CROP_DATA),     INTENT(IN   ):: CDAT
    TYPE(WBS_DATA),       INTENT(IN   ):: WBS
    INTEGER,              INTENT(IN   ):: KPER, KSTP
    DOUBLE PRECISION,     INTENT(IN   ):: DELT
    DOUBLE PRECISION,     INTENT(IN   ):: DYEAR
    CHARACTER(*),         INTENT(IN   ):: DATE
    INTEGER:: IU, F, I, K, L, R, C, IRR
    DOUBLE PRECISION::  AREA, CU, CIR, DMD, CIRI, DMDI, LR, ECe, ECw, AW, LF, P, DP, DP_TOT, ETi, DMD_FRAC, DU, CDMD, PIN, EFF
    CHARACTER(17):: ZER, DT
    CHARACTER(13):: PC
    !
    IF(SALT%OUT_ALL%IU == Z) RETURN  !NOTHING TO PRINT OUT
    !
    L  = CDAT%CROP_NAME_LEN
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    ZER = '0.0'; ZER = ADJUSTR(ZER)
    !DDT = DBLE(DELT)
    !
    CALL SALT%OUT_ALL%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IU = SALT%OUT_ALL%IU
    !
    IF( (KPER==ONE .AND. KSTP==ONE) .OR. SALT%IOUT==IU )  THEN
        IF(SALT%OUT_ALL%BINARY) THEN          
            WRITE(SALT%IOUT,'(A,/A)')'SALINITY IRRIGATION INFORMATION BY WBS OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',"DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), CROP ID (int), CROP_NAME (20 char), WBS ID (int), ROW (int), COL (int), PERCENT INCREASE (double), WBS AREA (double), CROP AREA (double), IRRIGATED AREA (double), SALINITY AREA (double), EFFICIENCY (double), PRECIPITATION (double), TOTAL IRRIGATION (double), TOTAL DEEP PERCOLOTION (double), LEACH FRACTION (double), CU (double), ET FROM IRRIGATION (double), SALT_REQ_IRR (double), SALT_IRR (double), SALT_REQ_DEMAND (double), SALT_DEMAND (double), LEACH REQUIREMENT (double), ECe (double), ECw (double), IRRIGATION UNIFORMITY (double)"
        ELSE
            !
            IF (SALT%IOUT==IU) WRITE(IU,*)
            !                                       
            CALL SALT%OUT_ALL%SET_HEADER( '    PER    STP   CROP  CROP_NAME'//REPEAT(' ',L-6)//' WBS    ROW    COL    DEMAND%CHANGE        CROP_AREA       EFFICIENCY    PRECIPITATION TOTAL_IRRIGATION  TOTAL_DEEP_PERC   LEACH_FRACTION               CU           ET_IRR     SALT_REQ_IRR         SALT_IRR  SALT_REQ_DEMAND      SALT_DEMAND        LEACH_REQ              ECe              ECw   IRR_UNIFORMITY             DELT   DYEAR            DATE_START' )
        END IF
    END IF
    !
    DO I=ONE, CDAT%NCROP
     IF(CDAT%PRINT_CROP%LIST(I).NE.Z) THEN
        DO K=ONE, CDAT%CROP(I)%N
          !
          IF(SALT%INUSE(I)%VEC(K)) THEN
              !
              IRR = CDAT%CROP(I)%IRR(K)
              R   = CDAT%CROP(I)%RC(ONE,K)
              C   = CDAT%CROP(I)%RC(TWO,K)
              F   = CDAT%CROP(I)%FID(K) 
              !
              EFF=DZ
              IF(CDAT%CROP(I)%EFF(K)>DZ) EFF = CDAT%CROP(I)%EFF(K)
              !
              AREA= CDAT%CROP(I)%AREA(K)
              CU  = CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K)) + CDAT%CROP(I)%TP(K) + CDAT%CROP(I)%EP(K) + CDAT%CROP(I)%TGWA(K) + CDAT%CROP(I)%EGWA(K) !TRAN*CDAT%CROP(I)%FTR(K) + TRAN*(UNO - CDAT%CROP(I)%FTR(K))
              ETi = CDAT%CROP(I)%TI(K)*(UNO + CDAT%CROP(I)%CECT(K))  !ONLY IRRIGATION ET
              !
              P   = CDAT%CROP(I)%PRECIP(K)
              IF(CDAT%HAS_Pe)  P = P + CDAT%CROP(I)%RNOFF_Peff(K)
              !
              AW  = CDAT%CROP(I)%DEMAND(K) + CDAT%CROP(I)%DEMAND_EXT(K)  
              CDMD= CDAT%CROP(I)%DEMAND(K)
              !
              IF(NEAR_ZERO(CDAT%CROP(I)%DEMAND_EXT_INI(K))) THEN
                  DMD_FRAC=DZ
              ELSE
                  DMD_FRAC = CDAT%CROP(I)%DEMAND_EXT(K) / CDAT%CROP(I)%DEMAND_EXT_INI(K)
                  IF(NEAR_ZERO(DMD_FRAC)) DMD_FRAC=DZ
              END IF
              !
              LR = SALT%LR(I)%VEC(K)
              !
              CIRI= SALT%AW(I)%VEC(K)
              CIR = SALT%AW(I)%VEC(K)*DMD_FRAC
              !
              ECe = SALT%ECe(I)
              ECw = SALT%ECw(I)
              DU  = SALT%IRR_UNI(IRR,CDAT%CROP(I)%FID(K))
              !
              IF(EFF>DZ) THEN
                  DMDI =  CIRI/EFF
                  DMD  = (CIRI/EFF)*DMD_FRAC
              ELSE
                  DMDI = DZ
                  DMD  = DZ
              END IF
              !
              P = CDAT%CROP(I)%PRECIP(K) - CDAT%CROP(I)%TP(K) - CDAT%CROP(I)%EP(K)  ! 'DP_P'
              DP = DP * (UNO - CDAT%CROP(I)%FIESWP(K))         ! 
              IF(DP < NEARZERO_12) DP = DZ                     ! 'DP_P'
              !
              DP_TOT = DP
              !
              DP = CDAT%CROP(I)%DEMAND(K) * (UNO - EFF) * (UNO - CDAT%CROP(I)%FIESWI(K)) ! 'DP_I'  --Inefficient losses to dp
              IF(CDAT%CROP(I)%DEMAND_EXT(K) > DZ) THEN                       !
                       DP = DP  &
                               + CDAT%CROP(I)%DEMAND_EXT(K) * (UNO - EFF) * (UNO - CDAT%CROP(I)%FIESWI(K))  & ! --Inefficient extra losses to dp
                               + CDAT%CROP(I)%DEMAND_EXT(K) *        EFF  * (UNO - CDAT%CROP(I)%ADRF(K))      ! --Extra losses to dp
              END IF                                                         !
              IF(DP < NEARZERO_12) DP = DZ                         ! 'DP_I'
              DP_TOT = DP_TOT + DP
              !
              !
              IF(AW+P>DZ) THEN
                  LF = DP_TOT/(AW+P)
              ELSE
                  LF = DZ
              END IF
              !
              IF(CDMD>NEARZERO_10 .AND. DMD>UNO) THEN
                  !
                  PIN = (DMD/CDMD)*100.D0
                  !
                  IF(PIN < 100000.D0) THEN
                       WRITE(PC,'(1x,F12.2)') PIN
                  ELSE
                      WRITE(PC,'(1x,ES12.2)') PIN
                  END IF
              ELSE
                  PC='         0.00'
                  PIN=DZ
              END IF
              !
              IF(SALT%OUT_ALL%BINARY) THEN
                      WRITE(IU) DATE, DYEAR, DELT, KPER, KSTP, I, CDAT%CROP_NAME(I), F, R, C, PIN, AREA, EFF, P, AW, DP_TOT, LF, CU, ETi, CIRI, CIR, DMDI, DMD, LR, ECe, ECw, DU
              ELSE
                      WRITE(IU, '(3I7, 2x,A, 3I7, 18A17, 2x,F13.7, 2x,A)') KPER, KSTP, I, CDAT%CROP_NAME(I)(:L),  F, R, C, PC, NUM2STR(AREA), NUM2STR(EFF), NUM2STR(P), NUM2STR(AW), NUM2STR(DP_TOT), NUM2STR(LF), NUM2STR(CU), NUM2STR(ETi), NUM2STR(CIRI), NUM2STR(CIR), NUM2STR(DMDI), NUM2STR(DMD), NUM2STR(LR), NUM2STR(ECe), NUM2STR(ECw), NUM2STR(DU), DT, DYEAR, DATE
              END IF
          END IF
        END DO
     END IF
    END DO
    !
  END SUBROUTINE
  !
END MODULE 
!
!#########################################################################################################
!
