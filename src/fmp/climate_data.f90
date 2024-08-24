!
!#########################################################################################################
!
MODULE CLIMATE_DATA_FMP_MODULE
  !
  USE FMP_DIMENSION_MODULE, ONLY: FMP_DIMENSION
  USE SOIL_DATA_FMP_MODULE, ONLY: SOIL_DATA
  !
  USE CONSTANTS
  USE ULOAD_AND_SFAC_INTERFACE
  USE ERROR_INTERFACE,                  ONLY: STOP_ERROR, WARNING_MESSAGE, FILE_IO_ERROR
  USE PARSE_WORD_INTERFACE,             ONLY: PARSE_WORD_UP
  USE STRINGS,                          ONLY: GET_NUMBER
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR
  USE ALLOC_INTERFACE,                  ONLY: ALLOC
  USE ARRAY_DATA_TYPES,                 ONLY: COMPRESSED_VALUE_STORAGE
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  USE LIST_ARRAY_INPUT_INTERFACE,       ONLY: LIST_ARRAY_INPUT, LIST_ARRAY_INPUT_INT
  USE WARNING_TYPE_INSTRUCTION,         ONLY: WARNING_TYPE
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: CLIMATE_DATA, INITIALIZE_CLIMATE_DATA
  !
  TYPE CLIMATE_DATA
      INTEGER:: IOUT=Z, LOUT=Z
      INTEGER:: NROW, NCOL, NFARM
      !
      INTEGER:: NDRCH = Z           ! Counter for how many times DIRECT_RECHARGE keyword is specified.
      !
      INTEGER:: HAS_Pe_TFR = Z     !0 = NO Pe, 1 = Pe, 2 =Pe as Fraction
      LOGICAL:: HAS_PRECIP = FALSE
      LOGICAL:: HAS_Pe     = FALSE  !SET TO TRUE IF PRECIP_EFFECTIVE IS ALLOCATED
      !
      LOGICAL:: HAS_REF_ET = FALSE
      LOGICAL:: HAS_BARE_PET = FALSE
      LOGICAL:: BARE_PET_USE_REF_ET = FALSE
      !
      LOGICAL:: HAS_RECHARGE    = FALSE
      LOGICAL:: TPOT_SHIFT_EPOT = TRUE
      LOGICAL:: TFR_READ   = FALSE
      DOUBLE PRECISION:: REF_ET_TO_BARE = HALF
      DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: REF_ET
      DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: PRECIP
      DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: PRECIP_EFFECTIVE
      DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: BARE_POT_EVAP
      !
      !!!DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: EGW_POT, TGW_POT
      !!!DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: EGW_ACT, TGW_ACT
      !!!DOUBLE PRECISION,    DIMENSION(:,:),ALLOCATABLE:: PRECIP_POT_TRAN, PRECIP_POT_EVAP
      !
      TYPE(COMPRESSED_VALUE_STORAGE), DIMENSION(:), ALLOCATABLE:: DIRECT_RECHARGE  ! Allocated to the size of NDRCH
      LOGICAL,                        DIMENSION(:), ALLOCATABLE:: RECHARGE_AS_FLUX
      TYPE(LIST_ARRAY_INPUT),         DIMENSION(:), ALLOCATABLE:: RCH_TFR
      !
      TYPE(LIST_ARRAY_INPUT):: REF_ET_BARE_TFR
      TYPE(LIST_ARRAY_INPUT):: REF_ET_TFR
      TYPE(LIST_ARRAY_INPUT):: PRECIP_TFR
      TYPE(LIST_ARRAY_INPUT):: PRECIP_EFF_TFR
      !
      CONTAINS
      !
      PROCEDURE, PASS(CLIM):: NEXT     => SETUP_NEXT_STRESS_PERIOD
      PROCEDURE, PASS(CLIM):: ADD_DIRECT_RECHARGE
      PROCEDURE, PASS(CLIM):: SETUP_NO_CLIMATE_DATA
      FINAL:: DEALLOCATE_CLIMATE_FINAL
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE DEALLOCATE_CLIMATE_FINAL(CLIM)
    TYPE(CLIMATE_DATA)::CLIM
    CALL DEALLOCATE_CLIMATE(CLIM)
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE DEALLOCATE_CLIMATE(CLIM)
    CLASS(CLIMATE_DATA), INTENT(INOUT)::CLIM
    !
    IF(ALLOCATED(CLIM%REF_ET))           DEALLOCATE(CLIM%REF_ET)
    IF(ALLOCATED(CLIM%PRECIP))           DEALLOCATE(CLIM%PRECIP)
    IF(ALLOCATED(CLIM%PRECIP_EFFECTIVE)) DEALLOCATE(CLIM%PRECIP_EFFECTIVE)
    IF(ALLOCATED(CLIM%BARE_POT_EVAP))    DEALLOCATE(CLIM%BARE_POT_EVAP)
    IF(ALLOCATED(CLIM%DIRECT_RECHARGE))  DEALLOCATE(CLIM%DIRECT_RECHARGE)
    IF(ALLOCATED(CLIM%RECHARGE_AS_FLUX)) DEALLOCATE(CLIM%RECHARGE_AS_FLUX)
    IF(ALLOCATED(CLIM%RCH_TFR))          DEALLOCATE(CLIM%RCH_TFR)
  END SUBROUTINE
  !
  SUBROUTINE SETUP_BASIC_VAR_CLIMATE_DATA( CLIM, FDIM, IOUT )  
    CLASS(CLIMATE_DATA), INTENT(INOUT):: CLIM
    TYPE(FMP_DIMENSION), INTENT(IN   ):: FDIM
    INTEGER,             INTENT(IN   ):: IOUT
    !
    CLIM%REF_ET_TO_BARE = HALF  !Default value
    !
    CLIM%HAS_PRECIP      = FALSE
    CLIM%HAS_Pe          = FALSE
    CLIM%HAS_REF_ET      = FALSE
    CLIM%HAS_BARE_PET    = FALSE
    CLIM%BARE_PET_USE_REF_ET = FALSE
    CLIM%RECHARGE_AS_FLUX= FALSE
    CLIM%TPOT_SHIFT_EPOT = TRUE
    CLIM%TFR_READ        = FALSE
    !
    CLIM%IOUT = IOUT
    CLIM%LOUT = IOUT
    CLIM%NROW = FDIM%NROW 
    CLIM%NCOL = FDIM%NCOL
    CLIM%NFARM = FDIM%NFARM
    !
    IF(CLIM%NFARM < ONE) CLIM%NFARM = ONE
    CLIM%HAS_Pe_TFR = Z
    !
    CLIM%HAS_RECHARGE = FALSE
    CLIM%NDRCH        = Z
    !
    !
  END SUBROUTINE
  !  
  SUBROUTINE INITIALIZE_CLIMATE_DATA( BL, CLIM, LINE, FDIM, HAS_CROP )
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(CLIMATE_DATA),         INTENT(INOUT):: CLIM
    CHARACTER(*),                INTENT(INOUT):: LINE
    TYPE(FMP_DIMENSION),         INTENT(IN   ):: FDIM
    LOGICAL,                     INTENT(IN   ):: HAS_CROP
    CHARACTER(5):: ERROR, BYWBS
    LOGICAL:: EOF, READ_REF_ET_TO_BARE
    INTEGER:: I, LLOC, ISTART, ISTOP, NROW, NCOL
    INTEGER, DIMENSION(3):: CDIM
    TYPE(WARNING_TYPE):: WARN_MSG
    !
    CALL WARN_MSG%INIT()
    !
    WRITE(BL%IOUT,'(/A/)') 'CLIMATE BLOCK FOUND AND NOW LOADING PROPERTIES'
    !
    CALL SETUP_BASIC_VAR_CLIMATE_DATA( CLIM, FDIM, BL%IOUT )  
    !
    NROW = FDIM%NROW
    NCOL = FDIM%NCOL
    ERROR = 'ERROR'
    BYWBS = 'BYWBS'
    READ_REF_ET_TO_BARE = FALSE
    !
    CDIM = [2,1,0]
    !
    CALL BL%START()                   ! First scan for multiple copies of DIRECT_RECHARGE, allocate the required size
    !
    CLIM%NDRCH =  Z
    DO I=ONE, BL%NLINE
                    LLOC=ONE
                    CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                    IF (BL%LINE(ISTART:ISTOP)=='DIRECT_RECHARGE' .OR.   &
                        BL%LINE(ISTART:ISTOP)=='DIRECTRECHARGE'  .OR.   &
                        BL%LINE(ISTART:ISTOP)=='DRCH') THEN
                                                           CLIM%NDRCH = CLIM%NDRCH + ONE
                    END IF
                    !
                    CALL BL%NEXT()
    END DO 
    !
    CLIM%HAS_RECHARGE = CLIM%NDRCH > Z
    !
    IF(CLIM%HAS_RECHARGE) THEN
        ALLOCATE(CLIM%RECHARGE_AS_FLUX(CLIM%NDRCH))
        ALLOCATE(CLIM%DIRECT_RECHARGE (CLIM%NDRCH))
        ALLOCATE(CLIM%RCH_TFR         (CLIM%NDRCH))
        CLIM%NDRCH = Z
    END IF
    !
    CALL BL%MAKE_SCRATCH_FILE()
    CALL BL%READ_SCRATCH(EOF, LINE)  ! -> READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
    !
    DO WHILE (.NOT. EOF)
      !
      LLOC=ONE
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
      !
      SELECT CASE ( LINE(ISTART:ISTOP) )
      CASE ("REFERENCE_ET","ETR")
                        WRITE(BL%IOUT,'(A)') '   REFERENCE_ET (ETR)                  KEYWORD FOUND. NOW LOADING STATIC/TRANSIENT KEYWORD AND THEN ARRAY OF REFERENCE ET VALUES.'
                        CLIM%HAS_REF_ET = TRUE
                        CALL CLIM%REF_ET_TFR%INIT('ETR',  LLOC, LINE, BL%IOUT, BL%IU, Z, Z, NROW, NCOL, CLIM%NFARM, BYWBS, SCRATCH=BL%SCRATCH, CDIM=CDIM)
                        !CALL CLIM%REF_ET_TFR%NEXT()  --AUTOCALLED!
                        !
                        ALLOCATE(CLIM%REF_ET(NCOL,NROW))
      CASE ("REFERENCE_ET_TO_BARE")
                        WRITE(BL%IOUT,'(A)') '   REFERENCE_ET_TO_BARE                  KEYWORD FOUND. NOW LOADING STATIC/TRANSIENT KEYWORD AND THEN ARRAY OF REFERENCE ET VALUES.'
                        READ_REF_ET_TO_BARE = TRUE
                        CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,CLIM%REF_ET_TO_BARE,  MSG='FMP CLIMATE BLOCK: "REFERENCE_ET_TO_BARE" FAILED TO LOAD THE NUMBER SPECIFIED AFTER THE KEYWORD.')
      CASE ("POTENTIAL_EVAPORATION_BARE","ETR_BARE")
                        WRITE(BL%IOUT,'(A)') '   POTENTIAL_EVAPORATION_BARE (ETR_BARE) KEYWORD FOUND. NOW LOADING STATIC/TRANSIENT KEYWORD AND THEN ARRAY OF REFERENCE EVAPORATION VALUES FOR BARE SOIL.'
                        CLIM%HAS_BARE_PET = TRUE
                        CALL CLIM%REF_ET_BARE_TFR%INIT('ETR_BARE',  LLOC, LINE, BL%IOUT, BL%IU, Z, Z, NROW, NCOL, CLIM%NFARM, BYWBS, SCRATCH=BL%SCRATCH, CDIM=CDIM)
                        !
      CASE ("PRECIPITATION", "PRECIP")
                        WRITE(BL%IOUT,'(A)') '   PRECIPITATION (PRECIP)                KEYWORD FOUND. NOW LOADING STATIC/TRANSIENT KEYWORD AND THEN ARRAY OF PRECIPITATION VALUES.'
                        CLIM%HAS_PRECIP = TRUE
                        CALL CLIM%PRECIP_TFR%INIT('PRECIP', LLOC, LINE, BL%IOUT, BL%IU, Z, Z, NROW, NCOL, CLIM%NFARM, BYWBS, SCRATCH=BL%SCRATCH, CDIM=CDIM)
                        !
      CASE ("PRECIPITATION_POTENTIAL_CONSUMPTION")
                        WRITE(BL%IOUT,'(A)') '   PRECIPITATION_POTENTIAL_CONSUMPTION KEYWORD FOUND. FIRST CHECKING "BY_LENGTH" OR "BY_FRACTION" KEYWORD. IF NOT PRESENT THEN AN ERROR IS  RAISED.'
                        !
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        !
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE('BYLENGTH', 'BY_LENGTH','BYHEIGHT',"BY_HEIGHT");  CLIM%HAS_Pe_TFR = ONE
                        CASE("BYFRACTION","BY_FRACTION",'BYFRAC',"BY_FRAC" );  CLIM%HAS_Pe_TFR = TWO
                        CASE DEFAULT
                                        !WRITE(BL%IOUT,'(3x,3A)') '"BY_LENGTH" OR "BY_FRAC" FLAG NOT FOUND, ASSUMING IT IS NOT SPECIFIED AND WILL USE "BY_FRAC".'
                                        CALL STOP_ERROR(LINE=LINE, INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='FMP CLIMATE ERROR. KEYWORD "PRECIPITATION_POTENTIAL_CONSUMPTION" MUST BE FOLLOWED BY THE KEYWORD "BY_LENGTH" OR "BY_FRACTION"')
                        END SELECT
                        !
                        CALL CLIM%PRECIP_EFF_TFR%INIT('Peff', LLOC, LINE, BL%IOUT, BL%IU, Z, Z, NROW, NCOL, CLIM%NFARM, BYWBS, SCRATCH=BL%SCRATCH, CDIM=CDIM)
                        !
      CASE ("DIRECT_RECHARGE", "DIRECTRECHARGE", "DRCH")
                        WRITE(BL%IOUT,'(A)') '   DIRECT_RECHARGE                       KEYWORD FOUND. NOW LOADING STATIC/TRANSIENT KEYWORD AND THEN ARRAY OF PRECIPITATION VALUES.'
                        !
                        CLIM%NDRCH = CLIM%NDRCH + ONE
                        !
                        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( LINE(ISTART:ISTOP) )
                        CASE("LENGTH","FLUX"); CLIM%RECHARGE_AS_FLUX(CLIM%NDRCH) = TRUE
                        CASE("RATE");          CLIM%RECHARGE_AS_FLUX(CLIM%NDRCH) = FALSE
                        CASE DEFAULT;          CALL STOP_ERROR(LINE=LINE, OUTPUT=CLIM%LOUT, MSG='FMP CLIMATE BLOCK ERROR. IF YOU SPECIFY KEYWORD "DIRECT_RECHARGE",'//NL//'YOU MUST FOLLOW IT WITH THE KEYWORD "LENGTH" OR "RATE".'//NL//' --Note error line maybe have been case shifted and stripped of comments.')
                        END SELECT
                        !
                        CALL CLIM%RCH_TFR(CLIM%NDRCH)%INIT('DRCH', LLOC, LINE, BL%IOUT, BL%IU, Z, Z, NROW, NCOL, CLIM%NFARM, BYWBS, SCRATCH=BL%SCRATCH, CDIM=CDIM)
      CASE ("NO_TPOT_SHIFT_TO_EPOT")
                        WRITE(BL%IOUT,'(A)') '   NO_TPOT_SHIFT_TO_EPOT                KEYWORD FOUND. UNUSES POTENTIAL TRANSPIRATION WILL NOT BE ADDED TO POTENTIAL EVAPORATION.'
                        CLIM%TPOT_SHIFT_EPOT = FALSE
                        
      CASE DEFAULT
                        CALL WARN_MSG%ADD('FOUND UNKNOWN KEYWORD "'//LINE(ISTART:ISTOP)//'" ***IT WILL BE IGNORED***'//BLN)
                        
      END SELECT
      !
      !READ(BL%SCRATCH, '(A)', IOSTAT=IERR) LINE
      CALL BL%READ_SCRATCH(EOF, LINE)
      !
    END DO
    !
    CALL WARN_MSG%CHECK(HED='FMP SOIL BLOCK'//NL,INFILE=BL%IU,OUTPUT=BL%IOUT,INLINE=TRUE,CMD_PRINT=TRUE,TAIL=NL)
    !
    IF    (CLIM%HAS_PRECIP .AND. .NOT. CLIM%PRECIP_TFR%TRANSIENT) THEN
        IF(ALL(CLIM%PRECIP_TFR%ARRAY < NEARZERO_30)) THEN
            CLIM%HAS_PRECIP = FALSE
            CALL CLIM%PRECIP_TFR%DESTROY()
        END IF
    ELSEIF(.NOT. CLIM%HAS_PRECIP) THEN
        CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP CLIMATE BLOCK: DID NOT FIND "PRECIPITATION" KEYWORD.'//BLN//'PRECIPITATION IS ASSUMED TO BE ZERO FOR ENTIRE MODEL.'//BLN//'TO DISABLE THIS WARNING YOU MAY WANT TO SPECIFY IN THE CLIMATE BLOCK "PRECIPITATION CONSTANT 0.0".')
    END IF
    !
    IF( .not. CLIM%HAS_BARE_PET .and. CLIM%HAS_REF_ET ) then
        CLIM%HAS_BARE_PET        = CLIM%HAS_REF_ET
        CLIM%BARE_PET_USE_REF_ET = CLIM%HAS_REF_ET
    end if
    !
    IF(CLIM%REF_ET_BARE_TFR%INUSE) THEN
                                   CLIM%REF_ET_TO_BARE = UNO  ! Value never used because user defined PotE for Bare
    ELSEIF (CLIM%HAS_BARE_PET .AND. .NOT. HAS_CROP .AND. .NOT. READ_REF_ET_TO_BARE) THEN
        CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP CLIMATE BLOCK: Input did not specify a LAND_USE block, but did specify REFERENCE_ET.'//NL//'The land surface has potential evaporation (PE) set to REFERENCE_ET (ETref).'//NL//'If you do not want to use the full ETref or disable this, add to the CLIMATE block the keyword REFERENCE_ET_TO_BARE followed by a floating point number.'//NL//'For example, "REFERENCE_ET_TO_BARE 0.5", will set PE = 0.5*ETref')
        CLIM%REF_ET_TO_BARE = UNO  ! No LAND_USE block, so any ETref will be just evaporation
    END IF
    !
    IF(  CLIM%HAS_REF_ET .AND. .NOT. CLIM%HAS_BARE_PET) THEN
          !
          CALL WARNING_MESSAGE(OUTPUT=CLIM%IOUT,MSG='FMP CLIMATE BLOCK: DID NOT FIND "POTENTIAL_EVAPORATION_BARE" KEYWORD.'//NL//'FALLOW/BARE LAND POTENTIAL EVAPORATION IS ASSUMED TO BE EQUAL TO '//NUM2STR(CLIM%REF_ET_TO_BARE)//NL//' TIMES THE SPECIFIED REFERENCE ET FROM "REFERENCE_ET" KEYWORD.', INLINE=TRUE)
          !
    ELSEIF(.NOT. CLIM%HAS_BARE_PET) THEN
          !
          CALL WARNING_MESSAGE(OUTPUT=CLIM%IOUT,MSG='FMP CLIMATE BLOCK: DID NOT FIND "POTENTIAL_EVAPORATION_BARE" KEYWORD OR "REFERENCE_ET" KEYWORD.'//NL//'PROGRAM WILL TERMINATE IF THERE ARE ANY CROPS THAT BECOME FALLOW OR BARE LAND AS A RESULT OF FRACTIONS OF CROPS WITHIN A CELL.', INLINE=TRUE)
    END IF
    !
    IF (CLIM%HAS_BARE_PET) ALLOCATE(CLIM%BARE_POT_EVAP(CLIM%NCOL,CLIM%NROW))
    !
    IF(CLIM%HAS_PRECIP) ALLOCATE(CLIM%PRECIP( NCOL, NROW ))
    !
  END SUBROUTINE 
  !
  SUBROUTINE SETUP_NO_CLIMATE_DATA(CLIM, FDIM, IOUT)
    !
    CLASS(CLIMATE_DATA), INTENT(INOUT):: CLIM
    TYPE(FMP_DIMENSION), INTENT(IN   ):: FDIM
    INTEGER,             INTENT(IN   ):: IOUT
    !
    CALL SETUP_BASIC_VAR_CLIMATE_DATA( CLIM, FDIM, IOUT )
    !
  END SUBROUTINE
  !
  SUBROUTINE SETUP_NEXT_STRESS_PERIOD(CLIM, SOIL, FID_ARRAY, NEW_FID, AREA)
    !
    CLASS(CLIMATE_DATA),                 INTENT(INOUT):: CLIM
    TYPE(SOIL_DATA),                     INTENT(IN   ):: SOIL
    INTEGER, DIMENSION(:,:), CONTIGUOUS, INTENT(IN   ):: FID_ARRAY
    LOGICAL,                             INTENT(IN   ):: NEW_FID
    DOUBLE PRECISION, DIMENSION(:,:), CONTIGUOUS, INTENT(IN   ):: AREA
    INTEGER:: I, R, C
    LOGICAL:: UPDATE, ERROR, LTMP
    !
    IF(CLIM%TFR_READ) THEN
        !
        UPDATE = FALSE
        !
        CALL CLIM%REF_ET_TFR%NEXT()
        CALL CLIM%PRECIP_TFR%NEXT()
        CALL CLIM%PRECIP_EFF_TFR%NEXT()
        CALL CLIM%REF_ET_BARE_TFR%NEXT()
        !
        IF(CLIM%HAS_RECHARGE) THEN
            DO I=ONE, CLIM%NDRCH
                    CALL CLIM%RCH_TFR(I)%NEXT()
            END DO
        END IF
        !
    ELSE
        UPDATE        = TRUE
        CLIM%TFR_READ = TRUE
        CLIM%HAS_Pe   = CLIM%HAS_PRECIP .AND. (CLIM%HAS_Pe_TFR > Z .OR. SOIL%HAS_Pe_TABLE > Z)
        !
        IF(CLIM%HAS_Pe) ALLOCATE(CLIM%PRECIP_EFFECTIVE(CLIM%NCOL,CLIM%NROW))
    END IF
    !
    IF(CLIM%HAS_RECHARGE) THEN
        DO I=ONE, CLIM%NDRCH
           IF( UPDATE                    .OR.            &
               CLIM%RCH_TFR(I)%TRANSIENT .OR.            &
              (CLIM%RCH_TFR(I)%SFAC%HAS_EX1.AND.NEW_FID) &
             ) THEN
              !
              IF(CLIM%RCH_TFR(I)%HAS_IXJ) THEN
                  CALL CLIM%RCH_TFR(I)%IXJ%TO_CVS(TWO, ONE, ONE, CLIM%DIRECT_RECHARGE(I))  !(IDIM1, IDIM2, VPOS, CVS)
              ELSE
                  CALL CLIM%DIRECT_RECHARGE(I)%BUILD_NONZERO( CLIM%RCH_TFR(I)%ARRAY )
              END IF
              !
              CALL DIRECT_RECHARGE_SFAC_AND_FLUX(CLIM%DIRECT_RECHARGE(I), CLIM%RCH_TFR(I), CLIM%RECHARGE_AS_FLUX(I), AREA, FID_ARRAY)
           END IF
        END DO
    END IF
    !
    IF(CLIM%HAS_PRECIP .AND. (CLIM%PRECIP_TFR%TRANSIENT .OR. UPDATE .OR. (CLIM%PRECIP_TFR%SFAC%HAS_EX1.AND.NEW_FID))) THEN
        IF(CLIM%PRECIP_TFR%HAS_IXJ) THEN
                                             CALL CLIM%PRECIP_TFR%IXJ%TO_ARRAY(TWO, ONE, ONE, CLIM%NCOL, CLIM%NROW, CLIM%PRECIP, ERROR)  !(IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR)
                                             !
                                             IF(ERROR) CALL STOP_ERROR(OUTPUT=CLIM%LOUT, MSG='FMP CLIMATE BLOCK ERROR. PRECIPITATION ARRAY SPECIFED WITH IXJ INPUT, BUT IT CONTAINED A ROW/COLUMN THAT WAS IN INPUT IS EITHER LESS THAN ZERO OR GREATER THEN NROW/NCOL.')
        ELSE
                                             DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                       CLIM%PRECIP(C,R) = CLIM%PRECIP_TFR%ARRAY(C,R)
                                             END DO
        END IF
        !
        IF(CLIM%PRECIP_TFR%SFAC%HAS_ALL) THEN
                                             DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                       CLIM%PRECIP(C,R) = CLIM%PRECIP(C,R) * CLIM%PRECIP_TFR%SFAC%ALL
                                             END DO
        END IF
        !
        IF(CLIM%PRECIP_TFR%SFAC%HAS_EX1) THEN
              DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL, FID_ARRAY(C,R)>Z)
                    !
                    CLIM%PRECIP(C,R) = CLIM%PRECIP(C,R) * CLIM%PRECIP_TFR%SFAC%EX1(FID_ARRAY(C,R))
              END DO
        END IF
        !
        DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL, FID_ARRAY(C,R)<ONE .OR. FID_ARRAY(C,R)>CLIM%NFARM .OR. (CLIM%PRECIP(C,R)<NEARZERO_30 .AND. CLIM%PRECIP(C,R).NE.DZ))
              CLIM%PRECIP(C,R) = DZ
        END DO
        !WHERE(FID_ARRAY<ONE) CLIM%PRECIP = DZ
        !
        IF(CLIM%HAS_Pe) THEN
                IF(CLIM%HAS_Pe_TFR > Z) THEN
                  ASSOCIATE(PRECIP => CLIM%PRECIP, Peff => CLIM%PRECIP_EFFECTIVE )
                    IF(CLIM%PRECIP_EFF_TFR%HAS_IXJ) THEN
                                                         CALL CLIM%PRECIP_EFF_TFR%IXJ%TO_ARRAY(TWO, ONE, ONE, CLIM%NCOL, CLIM%NROW, Peff, ERROR, D100)  !(IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR, NULL_VALUE)
                                                         !
                                                         IF(ERROR) CALL STOP_ERROR(OUTPUT=CLIM%LOUT, MSG='FMP CLIMATE BLOCK ERROR. PRECIPITATION ARRAY SPECIFED WITH IXJ INPUT, BUT IT CONTAINED A ROW/COLUMN THAT WAS IN INPUT IS EITHER LESS THAN ZERO OR GREATER THEN NROW/NCOL.')
                    ELSE
                                                         DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                                   Peff(C,R) = CLIM%PRECIP_EFF_TFR%ARRAY(C,R)
                                                         END DO
                    END IF
                    !
                    IF(CLIM%PRECIP_EFF_TFR%SFAC%HAS_ALL) THEN
                                                         DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                                   Peff(C,R) = Peff(C,R) * CLIM%PRECIP_EFF_TFR%SFAC%ALL
                                                         END DO
                    END IF
                    !
                    IF(CLIM%PRECIP_EFF_TFR%SFAC%HAS_EX1) THEN
                          DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL, FID_ARRAY(C,R)>Z)
                                !
                                Peff(C,R) = Peff(C,R) * CLIM%PRECIP_EFF_TFR%SFAC%EX1(FID_ARRAY(C,R))
                          END DO
                    END IF
                    !
                    IF(CLIM%HAS_Pe_TFR == TWO) THEN      !INPUT IS AS A FRACTION
                                                         DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                                   Peff(C,R) = Peff(C,R) * PRECIP(C,R)
                                                         END DO
                        
                    END IF
                    !
                    DO CONCURRENT (R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                        IF    (Peff(C,R) < DZ .OR. PRECIP(C,R) < NEARZERO_30) THEN
                                                                               Peff(C,R) = DZ
                        ELSEIF(Peff(C,R) > PRECIP(C,R)) THEN
                                                                               Peff(C,R) = PRECIP(C,R)
                        END IF
                    END DO
                  END ASSOCIATE
                ELSE !-----------------------------------------------------------------------
                  ASSOCIATE(Pe_TABLE => SOIL%Pe_TABLE, SID=>SOIL%SID, PRECIP => CLIM%PRECIP, Peff => CLIM%PRECIP_EFFECTIVE )
                      !
                    DO CONCURRENT (R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                        !
                        IF( PRECIP(C,R) < NEARZERO_30) THEN
                            !
                            Peff(C,R) = DZ
                            !
                        ELSEIF(SID(C,R) < ONE) THEN
                            !
                            Peff(C,R) = PRECIP(C,R)
                        ELSE
                            CALL Pe_TABLE( SID(C,R) )%LOOKUP( PRECIP(C,R), Peff(C,R) )  !Peff set to table result
                            !
                            IF(SOIL%HAS_Pe_TABLE == TWO) Peff(C,R) = Peff(C,R) * PRECIP(C,R) !INPUT IS AS A FRACTION
                            !
                            IF    (Peff(C,R) .NE. Peff(C,R)) THEN
                                                                 Peff(C,R) = PRECIP(C,R)
                            ELSEIF(Peff(C,R) < NEARZERO_30 ) THEN
                                                                 Peff(C,R) = DZ
                            ELSEIF(Peff(C,R) > PRECIP(C,R) ) THEN
                                                                 Peff(C,R) = PRECIP(C,R)
                            END IF
                        END IF
                    END DO
                  END ASSOCIATE
                END IF
        END IF
    END IF
    !
    IF(CLIM%HAS_REF_ET .and. (                     &
       CLIM%REF_ET_TFR%TRANSIENT .or.              &
       UPDATE .or.                                 &
       (CLIM%REF_ET_TFR%SFAC%HAS_EX1.and.NEW_FID)  &
                             ) ) THEN
        IF(CLIM%REF_ET_TFR%HAS_IXJ) THEN
                                             CALL CLIM%REF_ET_TFR%IXJ%TO_ARRAY(TWO, ONE, ONE, CLIM%NCOL, CLIM%NROW, CLIM%REF_ET, ERROR)  !(IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR)
                                             !
                                             IF(ERROR) CALL STOP_ERROR(OUTPUT=CLIM%LOUT, MSG='FMP CLIMATE BLOCK ERROR. REFERENCE_ET ARRAY SPECIFED WITH IXJ INPUT, BUT IT CONTAINED A ROW/COLUMN THAT WAS IN INPUT IS EITHER LESS THAN ZERO OR GREATER THEN NROW/NCOL.')
        ELSE
                                             DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                       CLIM%REF_ET(C,R) = CLIM%REF_ET_TFR%ARRAY(C,R)
                                             END DO
        END IF
        !
        IF(CLIM%REF_ET_TFR%SFAC%HAS_ALL) THEN
                                             DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                       CLIM%REF_ET(C,R) = CLIM%REF_ET(C,R) * CLIM%REF_ET_TFR%SFAC%ALL
                                             END DO
        END IF
        !
        IF(CLIM%REF_ET_TFR%SFAC%HAS_EX1) THEN
              !
              DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL, FID_ARRAY(C,R)>Z)
                    !
                    CLIM%REF_ET(C,R) = CLIM%REF_ET(C,R) * CLIM%REF_ET_TFR%SFAC%EX1(FID_ARRAY(C,R))
              END DO
        END IF
    END IF
    !
    ! SET UP POTENTIAL EVAP OF PRECIP
    IF(CLIM%HAS_BARE_PET) THEN  !ONLY CALCULATE POTENTIAL PRECIP EVAP IF THERE IS A REFERNCE ET OR BARE EVAP AVAILIBLE
        LTMP = UPDATE
        LTMP = LTMP .or.  CLIM%REF_ET_BARE_TFR%TRANSIENT
        LTMP = LTMP .or. (CLIM%REF_ET_BARE_TFR%SFAC%HAS_EX1 .and. NEW_FID)
        LTMP = LTMP .or. (CLIM%BARE_PET_USE_REF_ET .and. CLIM%REF_ET_TFR%TRANSIENT)
        LTMP = LTMP .or. (CLIM%BARE_PET_USE_REF_ET .and. CLIM%REF_ET_TFR%SFAC%HAS_EX1 .and. NEW_FID)
        IF(LTMP) THEN
              !              
              IF(CLIM%REF_ET_BARE_TFR%INUSE) THEN
                                                     IF(CLIM%REF_ET_BARE_TFR%HAS_IXJ) THEN
                                                                                          CALL CLIM%REF_ET_BARE_TFR%IXJ%TO_ARRAY(TWO, ONE, ONE, CLIM%NCOL, CLIM%NROW, CLIM%BARE_POT_EVAP, ERROR)  !(IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR)
                                                                                          !
                                                                                          IF(ERROR) CALL STOP_ERROR(OUTPUT=CLIM%LOUT, MSG='FMP CLIMATE BLOCK ERROR. POTENTIAL_EVAPORATION_BARE ARRAY SPECIFED WITH IXJ INPUT, BUT IT CONTAINED A ROW/COLUMN THAT WAS IN INPUT IS EITHER LESS THAN ZERO OR GREATER THEN NROW/NCOL.')
                                                     ELSE
                                                                                          DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                                                                    CLIM%BARE_POT_EVAP(C,R) = CLIM%REF_ET_BARE_TFR%ARRAY(C,R)
                                                                                          END DO
                                                     END IF
                                                     !
                                                     IF(CLIM%REF_ET_BARE_TFR%SFAC%HAS_ALL) THEN
                                                                                          DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)
                                                                                                    CLIM%BARE_POT_EVAP(C,R) = CLIM%BARE_POT_EVAP(C,R) * CLIM%REF_ET_BARE_TFR%SFAC%ALL
                                                                                          END DO
                                                     END IF
                                                     !
                                                     IF(CLIM%REF_ET_BARE_TFR%SFAC%HAS_EX1) THEN
                                                           !
                                                           DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL, FID_ARRAY(C,R)>Z)
                                                                 !
                                                                 CLIM%BARE_POT_EVAP(C,R) = CLIM%BARE_POT_EVAP(C,R) * CLIM%REF_ET_BARE_TFR%SFAC%EX1(FID_ARRAY(C,R))
                                                           END DO
                                                     END IF
              ELSE!IF(CLIM%REF_ET_TFR%INUSE) THEN
                                                         DO CONCURRENT(R=ONE:CLIM%NROW, C=ONE:CLIM%NCOL)           !USE REF ET VALUES WHEN IF FALLOW POTENTIAL EVAP NOT SPECIFIED AND PRECIP EXCEEDS REF ET
                                                                   CLIM%BARE_POT_EVAP(C,R) = CLIM%REF_ET(C,R) * CLIM%REF_ET_TO_BARE
                                                         END DO
              END IF
              !
              !CLIM%BARE_POT_EVAP = CLIM%PRECIP
              !
              !IF(CLIM%REF_ET_BARE_TFR%INUSE .AND. CLIM%REF_ET_BARE_TFR%SFAC%HAS_ALL) THEN
              !                                       !
              !                                       DO CONCURRENT (I=ONE:SIZE(CLIM%BARE_POT_EVAP,ONE), J=ONE:SIZE(CLIM%BARE_POT_EVAP,TWO), CLIM%BARE_POT_EVAP(I,J)>CLIM%REF_ET_BARE_TFR%ARRAY(I,J)*CLIM%REF_ET_BARE_TFR%SFAC%ALL)
              !                                             !
              !                                             CLIM%BARE_POT_EVAP(I,J) = CLIM%REF_ET_BARE_TFR%ARRAY(I,J)*CLIM%REF_ET_BARE_TFR%SFAC%ALL
              !                                       END DO
              !ELSEIF(CLIM%REF_ET_BARE_TFR%INUSE) THEN
              !                                       WHERE (CLIM%BARE_POT_EVAP>CLIM%REF_ET_BARE_TFR%ARRAY) 
              !                                           CLIM%BARE_POT_EVAP = CLIM%REF_ET_BARE_TFR%ARRAY
              !                                       END WHERE
              !ELSE!IF(CLIM%REF_ET_TFR%INUSE) THEN
              !                                       WHERE (CLIM%BARE_POT_EVAP>CLIM%REF_ET) 
              !                                           CLIM%BARE_POT_EVAP = CLIM%REF_ET  !USE REF ET VALUES WHEN IF FALLOW POTENTIAL EVAP NOT SPECIFIED AND PRECIP EXCEEDS REF ET
              !                                       END WHERE
              !END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE ADD_DIRECT_RECHARGE(CLIM, DPERC)
    CLASS(CLIMATE_DATA),                          INTENT(INOUT):: CLIM
    DOUBLE PRECISION, DIMENSION(:,:), CONTIGUOUS, INTENT(INOUT):: DPERC
    INTEGER:: DIM1, DIM2, I, K
    !
    IF (CLIM%HAS_RECHARGE) THEN
                 DO I=ONE, CLIM%NDRCH
                           DO K=ONE, CLIM%DIRECT_RECHARGE(I)%N
                                DIM1 = CLIM%DIRECT_RECHARGE(I)%DIM(ONE,K)
                                DIM2 = CLIM%DIRECT_RECHARGE(I)%DIM(TWO,K)
                                !
                                DPERC(DIM1,DIM2) = DPERC(DIM1,DIM2) + CLIM%DIRECT_RECHARGE(I)%VAL(K)
                                !
                           END DO
                 END DO
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE DIRECT_RECHARGE_SFAC_AND_FLUX(CVS, LAI, RECHARGE_AS_FLUX, AREA, FID_ARRAY)
    CLASS (COMPRESSED_VALUE_STORAGE),             INTENT(INOUT):: CVS
    CLASS (LIST_ARRAY_INPUT),                     INTENT(IN   ):: LAI
    LOGICAL,                                      INTENT(IN   ):: RECHARGE_AS_FLUX
    DOUBLE PRECISION, DIMENSION(:,:), CONTIGUOUS, INTENT(IN   ):: AREA
    INTEGER, DIMENSION(:,:),          CONTIGUOUS, INTENT(IN   ):: FID_ARRAY
    INTEGER:: I,J,K,F
    !
    IF (CVS%N > Z) THEN
                       !
                       IF(RECHARGE_AS_FLUX) THEN
                                                DO CONCURRENT (K=ONE:CVS%N)
                                                    I = CVS%DIM(ONE,K)
                                                    J = CVS%DIM(TWO,K) 
                                                    CVS%VAL(K) = CVS%VAL(K) * AREA(I,J)
                                                END DO
                       END IF
                       !
                       IF(LAI%SFAC%HAS_ALL) CVS%VAL = CVS%VAL * LAI%SFAC%ALL
                       IF(LAI%SFAC%HAS_EX1) THEN
                                                DO K=ONE, CVS%N
                                                        F = FID_ARRAY( CVS%DIM(ONE,K), CVS%DIM(TWO,K) )
                                                        !
                                                        IF(F > Z) CVS%VAL(K) = CVS%VAL(K) * LAI%SFAC%EX1(F)
                                                END DO
                       END IF
    END IF
    !
  END SUBROUTINE
  !
END MODULE
!
!#########################################################################################################
!
