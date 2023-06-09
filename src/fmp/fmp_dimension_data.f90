!
!#########################################################################################################
!
MODULE FMP_DIMENSION_MODULE!, ONLY: FMP_DIMENSION
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: DBL => REAL64
  USE GLOBAL, ONLY: DIS_GSE => GSE, BOTM, LBOTM, NLAY, IBOUND, ITMUNI, KND
  !
  USE CONSTANTS
  USE ULOAD_AND_SFAC_INTERFACE,          ONLY: ULOAD
  USE GENERIC_BLOCK_READER_INSTRUCTION,  ONLY: GENERIC_BLOCK_READER
  USE PATH_INTERFACE,                    ONLY: ADD_DIR_SLASH_ALLOC
  USE FILE_IO_INTERFACE,                 ONLY: READ_TO_DATA
  USE PARSE_WORD_INTERFACE,              ONLY: PARSE_WORD, PARSE_WORD_UP, RET_WORD
  USE STRINGS,                           ONLY: GET_INTEGER, GET_NUMBER
  USE ERROR_INTERFACE,                   ONLY: STOP_ERROR, WARNING_MESSAGE
  USE SFR_INPUT_DATA_TYPES,              ONLY: SFR_NAMED_LOCATION
  USE NUM2STR_INTERFACE,                 ONLY: NUM2STR
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: FMP_DIMENSION
  !
  TYPE FMP_DIMENSION
      INTEGER:: NFARM=NEG, NCROP, NSOIL, NIRRG, NSFR_DELIV, NSFR_RETURN, NSFR_MAR_DELIV, NMAR_ID, MXNRD, NCROP_ELEV
      INTEGER:: NPROJ, NDIST, NUNIT, NAUXDEM
      INTEGER:: NROW, NCOL, NROW_GW, NCOL_GW, ITMUNI
      LOGICAL:: HAS_FMP             = FALSE
      LOGICAL:: HAS_HIERARCHY       = FALSE
      LOGICAL:: UZF_LINK            = FALSE
      LOGICAL:: HAS_BY_TIMESTEP     = FALSE
      LOGICAL:: WBS_BY_TIMESTEP     = FALSE
      LOGICAL:: CROP_BY_TIMESTEP    = FALSE
      LOGICAL:: CLIMATE_BY_TIMESTEP = FALSE
      LOGICAL:: SFR_CHECKER         = FALSE
      LOGICAL:: NO_HCOF             = FALSE
      REAL(DBL):: HNEW_FACTOR       = UNO
      CHARACTER(:), ALLOCATABLE:: SFR_CHECKER_DIR
      TYPE(SFR_NAMED_LOCATION):: SFR_ID
      !CHARACTER(:), ALLOCATABLE:: OUTDIR
      CONTAINS
      PROCEDURE, PASS(FDIM):: INIT => INITIALIZE_FMP_DIMENSION
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE INITIALIZE_FMP_DIMENSION( FDIM, BL, NROW, NCOL, IUNITUZF, GSE )  !, DELR, DELC   
    CLASS(FMP_DIMENSION),        INTENT(INOUT):: FDIM
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL   !DATA BLOCK
    INTEGER,                     INTENT(IN   ):: NROW, NCOL, IUNITUZF
    !DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: DELR, DELC
    DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: GSE
    INTEGER:: I, J, K, N, LLOC, ISTART, ISTOP
    LOGICAL:: HAS_HIERARCHY, POP_LINE, EOF!, NO_OUTDIR
    !
    WRITE(BL%IOUT,'(/A/)') 'GLOBAL BLOCK FOUND AND NOW LOADING PROPERTIES'
    !
    FDIM%NROW_GW= NROW   !THIS IS INCASE THE MF GRID HAS A DIFFERENT DISCRETIZATION THAN THE FMP GRID
    FDIM%NCOL_GW= NCOL
    FDIM%NROW   = NROW
    FDIM%NCOL   = NCOL
    FDIM%ITMUNI = ITMUNI
    FDIM%NFARM  = NEG
    FDIM%NCROP  = Z
    FDIM%NSOIL  = NEG
    FDIM%NIRRG  = Z
    FDIM%NPROJ  = Z
    FDIM%NDIST  = Z
    FDIM%NUNIT  = Z
    FDIM%MXNRD  = Z
    FDIM%NCROP_ELEV  = Z
    FDIM%NSFR_DELIV  = Z
    FDIM%NSFR_RETURN = Z
    FDIM%NSFR_RETURN = Z
    FDIM%NAUXDEM = Z
    FDIM%SFR_ID%N = Z
    HAS_HIERARCHY = FALSE
    !NO_OUTDIR = TRUE
    !
    IF(ALLOCATED(GSE)) DEALLOCATE(GSE)
    !IF(ALLOCATED(DELR)) DEALLOCATE(DELR)
    !IF(ALLOCATED(DELC)) DEALLOCATE(DELC)
    !
    CALL BL%START()  !FIRST PASS TO GET GLOBAL DIMS -- SINGLE LINE LOADING VARIABLES ONLY!!!
    !
    DO I=ONE, BL%NLINE
                    POP_LINE = TRUE
                    LLOC=ONE
                    CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                    !
                    SELECT CASE(BL%LINE(ISTART:ISTOP))
                    CASE("NFARM","NWBS");      CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NFARM,        MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NWBS (aka NFARM).')
                    CASE("NCROP");             CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NCROP,        MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NCROP.')
                    CASE("NSOIL");             CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NSOIL,        MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NSOIL.')
                    CASE("NIRRIGATE","NIRR");  CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NIRRG, MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NIRRIGATE.') 
                    !
                    CASE("NAUX_DEMAND","NAUXDEM");    CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NAUXDEM,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NAUX_DEMAND.')
                    !
                    CASE("NPROJ") 
                                      CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NPROJ,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NPROJ.')
                                      HAS_HIERARCHY = HAS_HIERARCHY .OR. FDIM%NPROJ > Z
                    CASE("NDIST") 
                                      CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NDIST,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NDIST.')
                                      HAS_HIERARCHY = HAS_HIERARCHY .OR. FDIM%NDIST > Z
                    CASE("NUNIT") 
                                      CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NUNIT,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NUNIT.')
                                      HAS_HIERARCHY = HAS_HIERARCHY .OR. FDIM%NUNIT > Z
                    !
                    CASE("MXNRD","NRD_TYPES","NRD_TYPE","MXNRDT"); CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%MXNRD,     MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NRD_TYPES (MXNRD).') 
                    !
                    CASE("NSFR_DELIV");                            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NSFR_DELIV,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NSFR_DELIV.')
                    !
                    CASE("NSFR_RETURN");                           CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NSFR_RETURN,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NSFR_RETURN.')
                    !
                    !
                    CASE("NCROP_SPECIFIED_ELEVATIONS");            CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NCROP_ELEV,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR NCROP_SPECIFIED_ELEVATIONS.')
                    !                                               
                    CASE("UZF_LINK", "UZF", "UZFLINK")
                                                      IF(IUNITUZF == Z ) THEN
                                                          CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FOUND "'//BL%LINE(ISTART:ISTOP)//'" KEYWORD'//NL//'WHICH LINKES FMP TO UZF, BUT UZF PACKAGE IS NOT ENABLED (DECLAIRED IN NAME FILE).'//NL//'THIS OPTION WILL BE DISABLED DUE TO NO UZF BEING AVAILBLE TO SEND WATER TOO'//NL//'DEEP PERCOLATION WILL INSTEAD GO TO THE WATER TABLE.')
                                                      ELSE
                                                          FDIM%UZF_LINK = TRUE
                                                      END IF
                    !
                    CASE("NOHCOF", "NO_HCOF"); FDIM%NO_HCOF = TRUE
                    !
                    CASE('HEAD_PREDICTOR_FACTOR')
                                                      CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%HNEW_FACTOR,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD THE NUMBER FOR HEAD_PREDICTOR_FACTOR.')
                                                      !
                                                      IF (FDIM%HNEW_FACTOR > 0.99D0) FDIM%HNEW_FACTOR = UNO
                                                      IF (FDIM%HNEW_FACTOR < 0.01D0) FDIM%HNEW_FACTOR = DZ
                    !
                    CASE("BY_TIMESTEP", "TIMESTEP", "BYTIMESTEP") 
                                      CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                                      !
                                      SELECT CASE(BL%LINE(ISTART:ISTOP))
                                      CASE("WBS",  "WATER_BALANCE_SUBREGION"); FDIM%WBS_BY_TIMESTEP     = TRUE  
                                      CASE("CROP", "LAND_USE"               ); FDIM%CROP_BY_TIMESTEP    = TRUE  
                                      CASE("CLIMATE"                        ); FDIM%CLIMATE_BY_TIMESTEP = TRUE
                                      CASE DEFAULT;      CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FOUND "BY_TIMESTEP" (OR "TIMESTEP") KEYWORD.'//NL//'THIS MUST BE FOLLOWED ONE OF: "WBS", "LAND_USE", "CLIMATE", BUT INSTEAD FOUND "'//BL%LINE(ISTART:ISTOP)//'" KEYWORD.'//NL//'PLEASE REMOVE/FIX TO CONTINUE.')
                                      END SELECT 
                                      !
                                      FDIM%HAS_BY_TIMESTEP = TRUE
                    !
                    CASE("SFR_CHECK_FILES")
                                      FDIM%SFR_CHECKER_DIR = RET_WORD(BL%LINE,LLOC,COM_STOP=TRUE)
                                      CALL ADD_DIR_SLASH_ALLOC(FDIM%SFR_CHECKER_DIR)
                                      FDIM%SFR_CHECKER = TRUE
                    !
                    CASE("SCOTT","BOYCE") 
                                      WRITE(BL%IOUT,'(//A//, A//, A)') 'YOU SPECIFIED A MOST WONDERFUL NAME AND KEYWORD AVAILIBLE IN OneWater','OneWater WILL NOW SHOW ITS APPRECIATION WITH THE FOLLOWING:',REPEAT('   SCOTT E. BOYCE IS AWESOME!!!'//NL,100), 'THANKS FOR YOUR SUPPORT BY SELECTIONG OPTION '//BL%LINE(ISTART:ISTOP)
                                      WRITE(*,      '(//A//, A//, A)') 'YOU SPECIFIED A MOST WONDERFUL NAME AND KEYWORD AVAILIBLE IN OneWater','OneWater WILL NOW SHOW ITS APPRECIATION WITH THE FOLLOWING:',REPEAT('   SCOTT E. BOYCE IS AWESOME!!!'//NL,10), 'THANKS FOR YOUR SUPPORT BY SELECTIONG OPTION '//BL%LINE(ISTART:ISTOP)
                    !
                    !CASE('NROW');     CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NROW,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD NROW.')
                    !CASE('NCOL');     CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,FDIM%NCOL,MSG='FMP GLOBAL DIMENSION ERROR; FAILED TO LOAD NCOL.')
                    CASE DEFAULT
                                CALL BL%NEXT() !MOVE TO NEXT  LINE
                                POP_LINE = FALSE
                    END SELECT
                    !
                    IF(POP_LINE) CALL BL%DELETE_LINE() ! REMOVE LINE CAUSE ITS NO LONGER NEEDED
    END DO
    !
    CALL BL%START()
    !
    DO WHILE (BL%NOT_AT_END())
                    LLOC=ONE
                    CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                    !
                    SELECT CASE(BL%LINE(ISTART:ISTOP))
                    CASE("SFR_NAME","SFR_NAMES") 
                                      CALL FDIM%SFR_ID%LOAD(BL, LLOC, ISTART, ISTOP)
                                      EXIT
                    END SELECT
                    !
                    CALL BL%NEXT()
    END DO
    !
    !  LOAD GSE --MIGHT HAVE INTERNAL
    !
    CALL BL%START()
    !
    CALL BL%MAKE_SCRATCH_FILE()
    !
    CALL BL%READ_SCRATCH(EOF)
    !
    DO WHILE (.NOT. EOF)
                    !
                    LLOC=ONE
                    CALL PARSE_WORD_UP(BL%LN,LLOC,ISTART,ISTOP)
                    !
                    SELECT CASE(BL%LN(ISTART:ISTOP))
                    CASE("SURFACE_ELEVATION", "GSE") 
                                      ALLOCATE(GSE(FDIM%NCOL,FDIM%NROW))
                                      N = Z
                                      CALL ULOAD(GSE, LLOC, BL%LN, BL%IOUT, BL%IU, N, NOID=TRUE, SCRATCH=BL%SCRATCH)  !IU = N
                    !CASE("OUTPUT_DIRECTORY","OUTDIR") 
                    !                  CALL GET_WORD(BL%LN,LLOC,ISTART,ISTOP,FDIM%OUTDIR,IS_ALLOC=TRUE,NO_UPCASE=TRUE)
                    !                  NO_OUTDIR = FALSE
                    !CASE("DELR") 
                    !                  I = Z
                    !                  CALL ULOAD(DELR, LLOC, BL%LN, BL%IOUT, BL%IU, I, NO_INTERNAL=TRUE)
                    !CASE("DELC") 
                    !                  I = Z
                    !                  CALL ULOAD(DELC, LLOC, BL%LN, BL%IOUT, BL%IU, I, NO_INTERNAL=TRUE)
                    !!!CASE ("HIERARCHY")
                    !!!                  WRITE(BL%IOUT,'(A)') '   HIERARCHY                        KEYWORD FOUND. NOW LOADING STATIC/TRANSIENT KEYWORD AND FIRST LIST OF HIERARCHYS.'
                    !!!                  IF(.NOT. WBS%HAS_HIERARCHY) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='FOUND "HIERARCHY" KEYWORD, WHICH REQUIRES REQUIRES THAT "NPROJ", "NDIST", AND "NUNIT" BE SPECIFIED AND SET TO GREATER THAN ZERO IN THE GLOBAL DIMENSION BLOCK. PLEASE DOUBLE CHECK BLOCK SET UP.')
                    !!!                  CALL WBS%HIERARCHY_TFR%INIT('HIERARCHY', LLOC, LINE, BL%IOUT, BL%IU, FDIM%NFARM+FDIM%NAUXDEM, THREE, Z, Z, SCRATCH=BL%SCRATCH)  !, FDIM%NFARM, 'BYWBS', FDIM%NCROP, 'BYCROP'
                    CASE DEFAULT
                                CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP GLOBAL BLOCK FOUND UNKNOWN KEYWORD "'//BL%LN(ISTART:ISTOP)//'" ***IT WILL BE IGNORED***',INLINE=TRUE,CMD_PRINT=TRUE)
                    END SELECT
                    !
                    CALL BL%READ_SCRATCH(EOF)
    END DO
    !
    IF(.NOT. ALLOCATED(GSE)) THEN
        !
        IF( FDIM%NROW.NE.NROW .OR. FDIM%NCOL.NE.NCOL) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='GLOBAL DIMENSION BLOCK ERROR: YOU MUST SPECIFY THE GROND SURFACE ELEVATION WITH KEYWORD "SURFACE_ELEVATION" IF THE FMP GRID HAS AN NROW/NCOL THAT IS DIFFERENT FROM MODFLOW MODEL NROW/NCOL.')
        !
        ALLOCATE(GSE(FDIM%NCOL,FDIM%NROW))
        !
        !CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='GLOBAL DIMENSION BLOCK ERROR: YOU MUST SPECIFY THE GROND SURFACE ELEVATION EITHER IN THE DIS PACKAGE OR SPECIFY IT HERE WITH KEYWORD "SURFACE_ELEVATION".')
        !
        IF( DIS_GSE(1,1).NE.DIS_GSE(1,1) ) THEN
            CALL WARNING_MESSAGE(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='GLOBAL DIMENSION BLOCK: THE KEYWORD "SURFACE_ELEVATION" WAS NOT FOUND NOR WAS THE GROUND SURFACE ELEVATION DEFINED IN THE DIS PACKAGE.'//BLN//'FMP WILL ASSUME THAT THE GROUND SURFACE ELEVATION IS EQUAL TO THE TOP ELEVATION OF THE UPPER MOST ACTIVE CELL.')
            !
            DEALLOCATE(DIS_GSE)          !DIS GSE IGRID pointer must be updated outside of subroutine with call to: CALL SGWF2BAS7PSV(IGRID)
            ALLOCATE(DIS_GSE(NCOL,NROW))
            !
            DO I = ONE, NROW
            DO J = ONE, NCOL
                  DIS_GSE(J,I) = D100
                  DO K=ONE, NLAY
                               IF(IBOUND(J,I,K).NE.Z) THEN
                                   !
                                   DIS_GSE(J,I) = BOTM(J,I,LBOTM(K)-1)
                                   EXIT
                                   !
                               END IF
                  END DO
            END DO
            END DO
            !
        END IF
        !
        GSE = DIS_GSE              !REDUNDANT FROM BAS BECAUSE THERE MAYBE A TIME TO HAVE FMP GRID BE INDEPENDENT OF MODEL GRID.
        !
    ELSEIF( DIS_GSE(1,1).NE.DIS_GSE(1,1) .AND. FDIM%NROW==NROW .AND. FDIM%NCOL==NCOL ) THEN
                 DEALLOCATE(DIS_GSE)
                   ALLOCATE(DIS_GSE(NCOL,NROW), SOURCE=GSE)   !DIS GSE IGRID pointer must be updated outside of subroutine with call to: CALL SGWF2BAS7PSV(IGRID)
    END IF
    !
    FDIM%HAS_FMP = FDIM%NFARM > Z
    !
    IF( FDIM%NFARM < Z )   CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FAILED TO LOCATE KEYWORD "NWBS" OR ITS VALUE IS LESS THAN ZERO. PLEASE DOUBLE CHECK BLOCK SET UP.')
    !IF( FDIM%NCROP < ONE ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FAILED TO LOCATE KEYWORD "NCROP" OR ITS VALUE IS LESS THAN ONE. PLEASE DOUBLE CHECK BLOCK SET UP.')
    IF( FDIM%NSOIL < Z )   CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FAILED TO LOCATE KEYWORD "NSOIL" OR ITS VALUE IS LESS THAN ZERO. PLEASE DOUBLE CHECK BLOCK SET UP.')
    !
    IF( HAS_HIERARCHY .AND. FDIM%NPROJ < ONE ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FAILED TO LOCATE KEYWORD "NPROJ" WHEN NDIST OR NUNIT WAS FOUND AND GREATER THAN ZERO. YOU MUST EITHER SPECIFIY ALL "NPROJ", "NDIST", AND "NUNIT" OR SPECIFIY NONE OF THEM (OR SET THEM ALL TO ZERO). PLEASE DOUBLE CHECK BLOCK SET UP.')
    IF( HAS_HIERARCHY .AND. FDIM%NDIST < ONE ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FAILED TO LOCATE KEYWORD "NDIST" WHEN NPROJ OR NUNIT WAS FOUND AND GREATER THAN ZERO. YOU MUST EITHER SPECIFIY ALL "NPROJ", "NDIST", AND "NUNIT" OR SPECIFIY NONE OF THEM (OR SET THEM ALL TO ZERO). PLEASE DOUBLE CHECK BLOCK SET UP.')
    IF( HAS_HIERARCHY .AND. FDIM%NUNIT < ONE ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='WITHIN THE GLOBAL DIMENSION BLOCK FAILED TO LOCATE KEYWORD "NNUIT" WHEN NPROJ OR NDIST WAS FOUND AND GREATER THAN ZERO. YOU MUST EITHER SPECIFIY ALL "NPROJ", "NDIST", AND "NUNIT" OR SPECIFIY NONE OF THEM (OR SET THEM ALL TO ZERO). PLEASE DOUBLE CHECK BLOCK SET UP.')
    !
    FDIM%HAS_HIERARCHY = HAS_HIERARCHY
    !
    WRITE(BL%IOUT,'(A/)')'FMP GLOBAL BLOCK LOADED'
    WRITE(BL%IOUT,'(2A)')  'NWBS:          ', Num2Str( FDIM%NFARM       )
    WRITE(BL%IOUT,'(2A)')  'NCROP:         ', Num2Str( FDIM%NCROP       )
    WRITE(BL%IOUT,'(2A)')  'NSOIL:         ', Num2Str( FDIM%NSOIL       )
    WRITE(BL%IOUT,'(2A)')  'NIRRG:         ', Num2Str( FDIM%NIRRG       )
    WRITE(BL%IOUT,'(2A)')  'NRD_TYPES:     ', Num2Str( FDIM%MXNRD       )
    WRITE(BL%IOUT,'(2A)')  'NSFR_DELIVERY: ', Num2Str( FDIM%NSFR_DELIV  )
    WRITE(BL%IOUT,'(2A)')  'NSFR_RETURN:   ', Num2Str( FDIM%NSFR_RETURN )
    !WRITE(BL%IOUT,'(A,I10/)') 'NSFR_RETURN:   ',FDIM%NSFR_RETURN
    IF(FDIM%NCROP_ELEV > Z) WRITE(BL%IOUT,'(A,I10,10x,A)')  'NCROP_SPECIFIED_ELEVATIONS: ',FDIM%NCROP_ELEV, "-- ONLY USED BY CROP BLOCK KEYWORDS THAT ALTER THE CROP'S SURFACE ELEVATION FROM USING THE GLOBAL DIMENSION'S SURFACE_ELEVATION --"
    !
    WRITE(BL%IOUT,'(/A/)')'FMP-SWO HIERARCHY PARAMETERS ARE:'
    WRITE(BL%IOUT,'(A,I10)')  'NWBS:        ',FDIM%NFARM
    WRITE(BL%IOUT,'(A,I10)')  'NAUX_DEMAND: ',FDIM%NAUXDEM
    WRITE(BL%IOUT,'(A,I10)')  'NUNIT:       ',FDIM%NUNIT
    WRITE(BL%IOUT,'(A,I10)')  'NDIST:       ',FDIM%NDIST
    WRITE(BL%IOUT,'(A,I10/)') 'NPROJ:       ',FDIM%NPROJ
    !
    IF(FDIM%HAS_BY_TIMESTEP) THEN
        WRITE(BL%IOUT,'(A/,A/)')'BY TIME STEP KEYWORD FOUND','THE FOLLOWING BLOCKS WILL LOAD ALL "TRANSIENT" PROPERTIES EVERY TIME STEP.'
        IF(FDIM%WBS_BY_TIMESTEP    ) WRITE(BL%IOUT,'(A)' ) 'WBS     BLOCK'
        IF(FDIM%CROP_BY_TIMESTEP   ) WRITE(BL%IOUT,'(A)' ) 'CROP    BLOCK'
        IF(FDIM%CLIMATE_BY_TIMESTEP) WRITE(BL%IOUT,'(A/)') 'CLIMATE BLOCK'
    END IF
    !
    !IF(.NOT. HAS_HIERARCHY) THEN  !ASSUME EACH FARM IS ITS OWN PROJECT, DISTRICT, UNIT
    !    FDIM%NPROJ = FDIM%NFARM
    !    FDIM%NDIST = FDIM%NFARM
    !    FDIM%NUNIT = FDIM%NFARM
    !END IF
    !
    WRITE(BL%IOUT,'(/2A/, *(3x,A/),/)')                                                                         &
        'FMP IS USING A HEAD_PREDICTOR_FACTOR OF ',NUM2STR(FDIM%HNEW_FACTOR),                                   &
        'THIS FACTOR IS APPLIED FOR DETERMINING THE WATER TABLE ELEVATION AS A COMBINATION OF HNEW AND HOLD.',  &
        '   FOR EXAMPLE, FOR DETERMING THE WATER TABLE (WT) FOR EVAPORATION OF GROUNDWATER WITH THE FACTOR X:', &
        '   WT = HNEW*X + HOLD*(1-X)',                                                                          &
        '                            -> IT IS RECOMMENDED TO ONLY SET THIS VALUE (X) TO 0, 0.5, or 1.     ***NOTE THAT X = 1 IS THE DEFAULT OPTION'
    !
    IF(FDIM%UZF_LINK) THEN
        WRITE(BL%IOUT,'(/A/)')  'FMP-UZF LINK ENABLED.  DEEP PERCOLATION IS PASSED TO UZF OR TREATED AS RECHARGE.'
    ELSE 
        WRITE(BL%IOUT,'(/A/)')  'FMP IS --NOT-- LINKED TO UZF.  DEEP PERCOLATION IS TREATED AS RECHARGE EVERYWHERE.'
    END IF
    !
    IF (FDIM%SFR_ID%N > Z) THEN
        WRITE(BL%IOUT,'(/A,/ A20,1x,A7,1x,A7)' )  ' SFR_NAMES HAVE BEEN SPECIFIED WITH THE FOLLOWING NAMES HAVE BEEN ASSOCIATED WITH THE FOLLOWING SEGMENT AND REACHES:','SFR_NAME','SEGMENT', 'REACH'
        DO I=ONE, FDIM%SFR_ID%N
            WRITE(BL%IOUT,'(A20,1x,I7,1x,I7)' ) FDIM%SFR_ID%NAM(I), FDIM%SFR_ID%SEG_RCH(ONE,I), FDIM%SFR_ID%SEG_RCH(TWO,I)
        END DO
        WRITE(BL%IOUT,'(A)' )
    ELSE
        WRITE(BL%IOUT,'(/A/)')  'SFR_NAMES KEYWORD WAS NOT LOCATED, SO ALL FMP RELATED SEGMENT AND REACHES MUST BE SPECIFED DIRECTLY (NO SFR NAME BUT THEIR SEGEMETN AND REACH INSTEAD).'
    END IF
    !
    !IF(NO_OUTDIR) ALLOCATE(FDIM%OUTDIR, SOURCE='')
    !
  END SUBROUTINE
    !
!  SUBROUTINE INITIALIZE_OFE(OFE, LLOC, LINE, IOUT, IN, NCROP, NFARM, NROW, NCOL, LIST_DIM)
!  ! LISTARRAY   => TRUE indicates that only LIST keyword is allowed, but it is a multicolumn list
!  ! ONLY_ARRAY  => TRUE indicates that only ARRAY keyword is allowed
!  ! LIST_DIM    => When present and > 1 indicates the column dimension of the list htat is loaded, viz. multicolumn list
!  CLASS(CROP_PROP_INPUT_INT):: CPI
!  CHARACTER(*),           INTENT(INOUT):: LINE
!  INTEGER,                INTENT(INOUT):: LLOC
!  INTEGER,                INTENT(IN   ):: IOUT, IN, NCROP, NFARM, NROW, NCOL
!  INTEGER,                INTENT(IN   ):: LIST_DIM
!  REAL:: R
!  INTEGER:: IU,ISTART,ISTOP,ONE,Z,I,N
!  LOGICAL:: TRUE, FALSE, LISTARRAY, NOID, ONLYARRAY
!  !
!  Z = 0
!  ONE = 1
!  TRUE =.TRUE.
!  FALSE=.FALSE.
!  IU = Z
!  ALLOCATE(OFE%TYP, SOURCE = 'OFE')
!  !
!  N = LLOC
!  CALL URWORD(LINE,LLOC,ISTART,ISTOP,ONE,I,R,IOUT,IN)
!  SELECT CASE (LINE(ISTART:ISTOP))
!                                  CASE('LIST' );  OFE%LISTARRAY = TRUE
!                                  CASE('ARRAY');  OFE%LISTARRAY = FALSE
!                                  CASE DEFAULT
!                                               CALL STOP_ERROR(LINE,IN,IOUT,'FMP WBS BLOCK ERROR. EXPECTED TO FIND KEYWORD "LIST" OR "ARRAY" AFTER KEYWORD, BUT IT WAS NOT FOUND.')
!                                               LLOC = N
!  END SELECT
!  !
!  IF(LISTARRAY .OR. ONLYARRAY) CPI%LISTLOAD = FALSE
!  !
!  CALL URWORD(LINE,LLOC,ISTART,ISTOP,ONE,I,R,IOUT,IN)
!  SELECT CASE (LINE(ISTART:ISTOP))
!                                 CASE('TRANSIENT'); CPI%TRANSIENT = TRUE
!                                 CASE('STATIC');    CPI%TRANSIENT = FALSE
!                                 CASE DEFAULT; CALL STOP_ERROR(LINE,IN,IOUT,'FMP CROP BLOCK ERROR. FOUND KEYWORD THAT SHOULD BE FOLLOWED BY EITHER "TRANSIENT" OR "STATIC" KEYWORD')
!  END SELECT
!  !
!  IF     (CPI%LISTLOAD .AND. PRESENT(LIST_DIM)) THEN
!                              IF(LIST_DIM > ONE) THEN
!                                                     CPI%LISTLOAD  = FALSE
!                                                     CPI%LISTARRAY = TRUE
!                                                         LISTARRAY = TRUE
!                                                     ALLOCATE(CPI%ARRAY(LIST_DIM,NCROP))
!                              ELSE
!                                                     ALLOCATE(CPI%LIST(NCROP))
!                              END IF
!  ELSEIF (CPI%LISTLOAD ) THEN
!                              ALLOCATE(CPI%LIST(NCROP))
!  ELSE
!       IF(.NOT. FRACTION) THEN              
!                              ALLOCATE(CPI%ARRAY(NCOL,NROW))
!       ELSE
!                              ALLOCATE(CPI%ARRAY(NCOL,NROW*NCROP))
!       END IF
!  END IF
!  !
!  NOID = .NOT. LISTARRAY  !FOR READING OF ID WHEN READING LIST2D
!  IF(ONLYARRAY) NOID = TRUE
!  !
!  IF (.NOT. CPI%TRANSIENT ) THEN  
!      IF (CPI%LISTLOAD ) THEN
!                             CALL ULOAD(CPI%LIST,  LLOC, LINE, IOUT, IN, IU, NOID=FALSE)
!      ELSE
!                             CALL ULOAD(CPI%ARRAY, LLOC, LINE, IOUT, IN, IU, NOID=NOID)
!      END IF
!  ELSE
!      IF (CPI%LISTLOAD ) THEN
!                             CALL CPI%TFR%INIT(LLOC,LINE,IOUT,IN, NOID=FALSE)
!      ELSE
!                             CALL CPI%TFR%INIT(LLOC,LINE,IOUT,IN, NOID=NOID)
!      END IF
!  END IF
!  !
!  END SUBROUTINE
  !
END MODULE
!
!#########################################################################################################
!
