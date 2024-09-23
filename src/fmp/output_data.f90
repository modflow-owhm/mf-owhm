!
!#########################################################################################################
!
MODULE OUTPUT_DATA_FMP_MODULE!, ONLY: OUTPUT_DATA, INITIALIZE_OUTPUT_DATA
  !
  USE FMP_DIMENSION_MODULE, ONLY: FMP_DIMENSION
  !
  USE CONSTANTS
  USE ERROR_INTERFACE,                   ONLY: STOP_ERROR, FILE_IO_ERROR, WARNING_MESSAGE
  USE PARSE_WORD_INTERFACE,              ONLY: PARSE_WORD_UP
  USE STRINGS,                           ONLY: GET_INTEGER, GET_WORD
  USE NUM2STR_INTERFACE,                 ONLY: NUM2STR
  USE ALLOC_INTERFACE,                   ONLY: ALLOC
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,   ONLY: GENERIC_OUTPUT_FILE
  USE GENERIC_BLOCK_READER_INSTRUCTION,  ONLY: GENERIC_BLOCK_READER
  USE WARNING_TYPE_INSTRUCTION,          ONLY: WARNING_TYPE
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: OUTPUT_DATA, INITIALIZE_OUTPUT_DATA
  !
  TYPE OUTPUT_DATA
      INTEGER:: WEL_CBC = Z
      INTEGER:: FNR_CBC = Z
      !INTEGER:: ISDPFL  = Z
      !INTEGER:: IFBPFL  = Z
      INTEGER:: HAS_ROUT= Z  ! 1 = STATIC, 2 = TRANSIENT 
      TYPE(GENERIC_OUTPUT_FILE):: WBS_WATER_USE
      TYPE(GENERIC_OUTPUT_FILE):: FDS
      TYPE(GENERIC_OUTPUT_FILE):: FB_COMPACT
      TYPE(GENERIC_OUTPUT_FILE):: FB_DETAILS
      TYPE(GENERIC_OUTPUT_FILE):: ET_ARRAY_SUM
      TYPE(GENERIC_OUTPUT_FILE):: ET_ARRAY_SEP
      TYPE(GENERIC_OUTPUT_FILE):: ET_LIST
      TYPE(GENERIC_OUTPUT_FILE):: ROUTING_INFORMATION
      TYPE(GENERIC_OUTPUT_FILE):: FNRCH_ARRAY
      TYPE(GENERIC_OUTPUT_FILE):: FNRCH_LIST
      TYPE(GENERIC_OUTPUT_FILE):: FWELLS
      TYPE(GENERIC_OUTPUT_FILE):: EGWA
      TYPE(GENERIC_OUTPUT_FILE):: TGWA
      TYPE(GENERIC_OUTPUT_FILE):: TI
      TYPE(GENERIC_OUTPUT_FILE):: EI
      TYPE(GENERIC_OUTPUT_FILE):: TP
      TYPE(GENERIC_OUTPUT_FILE):: EP
      TYPE(GENERIC_OUTPUT_FILE):: ETGW
      TYPE(GENERIC_OUTPUT_FILE):: ETI
      TYPE(GENERIC_OUTPUT_FILE):: ETP
      TYPE(GENERIC_OUTPUT_FILE):: RUNOFF
      TYPE(GENERIC_OUTPUT_FILE):: DPERC
      !MAYBE: 'RED_FMP_PMP.out' - FMPOUT%NAME(10)
      !
      CONTAINS
      !
      FINAL:: DEALLOCATE_OUTPUT_FINAL
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE DEALLOCATE_OUTPUT_FINAL(OFL)
    TYPE(OUTPUT_DATA)::OFL
    CALL DEALLOCATE_OUTPUT(OFL)
  END SUBROUTINE
  !
  SUBROUTINE DEALLOCATE_OUTPUT(OFL)
    CLASS(OUTPUT_DATA), INTENT(INOUT)::OFL
    !
    OFL%WEL_CBC = Z
    OFL%FNR_CBC = Z
    !OFL%ISDPFL  = Z
    !OFL%IFBPFL  = Z
    OFL%HAS_ROUT= Z
    !
    CALL OFL%WBS_WATER_USE      %CLOSE()
    CALL OFL%FDS                %CLOSE()
    CALL OFL%FB_COMPACT         %CLOSE()
    CALL OFL%FB_DETAILS         %CLOSE()
    CALL OFL%ET_ARRAY_SUM       %CLOSE()
    CALL OFL%ET_ARRAY_SEP       %CLOSE()
    CALL OFL%ET_LIST            %CLOSE()
    CALL OFL%FNRCH_ARRAY        %CLOSE()
    CALL OFL%FNRCH_LIST         %CLOSE()
    CALL OFL%FWELLS             %CLOSE()
    CALL OFL%ROUTING_INFORMATION%CLOSE()
    CALL OFL%EGWA               %CLOSE()
    CALL OFL%TGWA               %CLOSE()
    CALL OFL%TI                 %CLOSE()
    CALL OFL%EI                 %CLOSE()
    CALL OFL%TP                 %CLOSE()
    CALL OFL%EP                 %CLOSE()
    CALL OFL%ETGW               %CLOSE()
    CALL OFL%ETI                %CLOSE()
    CALL OFL%ETP                %CLOSE()
    CALL OFL%RUNOFF             %CLOSE()
    CALL OFL%DPERC              %CLOSE()
  END SUBROUTINE
  !  
  SUBROUTINE INITIALIZE_OUTPUT_DATA( BL, OFL, ITMUNI)
    CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(OUTPUT_DATA),          INTENT(INOUT):: OFL
    INTEGER,                     INTENT(IN   ):: ITMUNI
    CHARACTER(14):: TIMEUNIT
    CHARACTER( 8):: KEY
    CHARACTER( 5):: ERROR
    INTEGER:: I, LLOC, ISTART, ISTOP!, IERR, NROW, NCOL
    TYPE(WARNING_TYPE):: WARN_MSG1, WARN_MSG2
    !
    WRITE(BL%IOUT,'(/A/)') 'OUTPUT BLOCK FOUND AND NOW LOADING OUTPUT OPTIONS'
    !
    !
    ERROR='ERROR'
    !
    CALL WARN_MSG1%INIT()
    CALL WARN_MSG2%INIT()
    !
    CALL BL%START()
    !
    DO I=ONE, BL%NLINE
      !
      LLOC=ONE
      CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
      !
      SELECT CASE ( BL%LINE(ISTART:ISTOP) )
      CASE ("FWEL_CBC","FWELL_CBC","FARM_WELL_CBC")
                        WRITE(BL%IOUT,'(A)') '   FARM_WELL_CBC (FWEL_CBC)               OUTPUT KEYWORD FOUND. NOW READING UNIT NUMBER (MUST BE 0 OR >1)'
                        CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,OFL%WEL_CBC,  MSG='FMP OUTPUT BLOCK ERROR; OUTPUT WEL_CBC FAILED TO LOAD ITS ASSOCIATED UNIT NUMBER.')
                        IF( OFL%WEL_CBC<Z .OR. OFL%WEL_CBC==ONE) THEN
                                 CALL WARN_MSG2%ADD('FOUND KEYWORD "'//BL%LINE(ISTART:ISTOP)//'", IT MUST BE >1 OR 0, BUT WAS SET TO '//NUM2STR(OFL%WEL_CBC)//' IT IS RESET TO ZERO'//BLN)
                                 OFL%WEL_CBC = Z
                        END IF
                        !
      CASE ("FNR_CBC","FARM_NET_RECHARGE_CBC")
                        WRITE(BL%IOUT,'(A)') '   FARM_NET_RECHARGE_CBC (FNR_CBC)        OUTPUT KEYWORD FOUND. NOW READING UNIT NUMBER (MUST BE 0 OR >3)'
                        CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,OFL%FNR_CBC,  MSG='FMP OUTPUT BLOCK ERROR; OUTPUT FNR_CBC FAILED TO LOAD ITS ASSOCIATED UNIT NUMBER.')
                        IF( OFL%FNR_CBC<=THREE .AND. OFL%FNR_CBC.NE.Z) THEN
                                 CALL WARN_MSG2%ADD('FOUND KEYWORD "'//BL%LINE(ISTART:ISTOP)//'", IT MUST BE >3 OR 0, BUT WAS SET TO '//NUM2STR(OFL%FNR_CBC)//' IT IS RESET TO ZERO'//BLN)
                                 OFL%FNR_CBC = Z
                        END IF
                        !
!      CASE ("ISDPFL")
!                        WRITE(BL%IOUT,'(A)') '   ISDPFL              OUTPUT KEYWORD FOUND. NOW READING THE INTEGER FLAG.'
!                        CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,OFL%ISDPFL,  MSG='FMP OUTPUT BLOCK ERROR; OUTPUT ISDPFL FAILED TO LOAD ITS INTEGER FLAG.')
!                        !
!      CASE ("IFBPFL")
!                        WRITE(BL%IOUT,'(A)') '   IFBPFL              OUTPUT KEYWORD FOUND. NOW READING THE INTEGER FLAG.'
!                        CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,OFL%IFBPFL,  MSG='FMP OUTPUT BLOCK ERROR; OUTPUT IFBPFL FAILED TO LOAD ITS INTEGER FLAG.')
!                        !
      CASE ("FWELLS","FARM_WELL_SUMMARY")
                        WRITE(BL%IOUT,'(A)') '   FARM_WELL_SUMMARY (FWELLS)             OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FWELLS.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%FWELLS%OPEN("FWELLS.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%FWELLS%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%FWELLS%IU==Z) CALL OFL%FWELLS%OPEN("FWELLS.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
      CASE ("FNRCH_ARRAY","FARM_NET_RECHARGE_ARRAY")
                        WRITE(BL%IOUT,'(A)') '   FARM_NET_RECHARGE_ARRAY (FNRCH_ARRAY)  OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FNRCH_ARRAY.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%FNRCH_ARRAY%OPEN("FNRCH_ARRAY.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%FNRCH_ARRAY%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%FNRCH_ARRAY%IU==Z) CALL OFL%FNRCH_ARRAY%OPEN("FNRCH_ARRAY.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
      CASE ("FNRCH_LIST","FARM_NET_RECHARGE_LIST")
                        WRITE(BL%IOUT,'(A)') '   FARM_NET_RECHARGE_LIST (FNRCH_LIST)    OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FNRCH_LIST.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%FNRCH_LIST%OPEN("FNRCH_LIST.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%FNRCH_LIST%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%FNRCH_LIST%IU==Z) CALL OFL%FNRCH_LIST%OPEN("FNRCH_LIST.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
      CASE ("WBS_WATER_USE")
                        WRITE(BL%IOUT,'(A)') '   WBS_WATER_USE                          OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FMP_WBS_WATER_USE.txt"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%WBS_WATER_USE%OPEN("FMP_WBS_WATER_USE.txt",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%WBS_WATER_USE%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%WBS_WATER_USE%IU==Z) CALL OFL%WBS_WATER_USE%OPEN("FMP_WBS_WATER_USE.txt",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
      CASE ("FDS","FARM_DEMAND_SUPPLY_SUMMARY")
                        WRITE(BL%IOUT,'(A)') '   FARM_DEMAND_SUPPLY_SUMMARY (FDS)       OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FDS.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%FDS%OPEN("FDS.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%FDS%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11)
                                !
                                IF(OFL%FDS%IU==Z) CALL OFL%FDS%OPEN("FDS.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("FB_COMPACT","FARM_BUDGET_COMPACT")
                        WRITE(BL%IOUT,'(A)') '   FARM_BUDGET_COMPACT  (FB_COMPACT)      OUTPUT FOUND. NOW OPENING GENERIC_OUTPUT FILE. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FB_COMPACT.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%FB_COMPACT%OPEN("FB_COMPACT.out",LLOC,BL%IOUT,BL%IU)
                        ELSE
                                CALL OFL%FB_COMPACT%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11)
                                !
                                IF(OFL%FB_COMPACT%IU==Z)  CALL OFL%FB_COMPACT%OPEN("FB_COMPACT.out",LLOC,BL%IOUT,BL%IU)
                        END IF
                        
                        !
      CASE ("FB_DETAILS","FB_DETAIL","FARM_BUDGET")
                        WRITE(BL%IOUT,'(A)') '   FARM_BUDGET (FB_DETAILS)               OUTPUT FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "FB_DETAILS.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%FB_DETAILS%OPEN("FB_DETAILS.out",LLOC,BL%IOUT,BL%IU)
                        ELSE
                                CALL OFL%FB_DETAILS%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11)
                                !
                                IF(OFL%FB_DETAILS%IU==Z)  CALL OFL%FB_DETAILS%OPEN("FB_DETAILS.out",LLOC,BL%IOUT,BL%IU)
                        END IF
                        !
      CASE ("ET_ARRAY","EVAPOTRANSPIRATION_SUMMARY")
                        WRITE(BL%IOUT,'(A)') '   EVAPOTRANSPIRATION_SUMMARY (ET_ARRAY)  OUTPUT KEYWORD FOUND. NOW LOADING KEYWORD "SUM" OR "SEPARATE" AND OPENING GENERIC_OUTPUT FILE.'
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE("SUM")      
                                           WRITE(BL%IOUT,'(42x,A)') 'FOUND SUM KEYWORD, NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "ET_ARRAY.out"'
                                           IF(BL%LINE(LLOC:) == BLNK) THEN
                                                   LLOC=ONE
                                                   CALL OFL%ET_ARRAY_SUM%OPEN("ET_ARRAY.out",LLOC,BL%IOUT,BL%IU)
                                           ELSE
                                                   CALL OFL%ET_ARRAY_SUM%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                                   !
                                                   IF(OFL%ET_ARRAY_SUM%IU==Z)  CALL OFL%ET_ARRAY_SUM%OPEN("ET_ARRAY.out",LLOC,BL%IOUT,BL%IU)
                                           END IF
                        CASE("SEPARATE")      
                                           WRITE(BL%IOUT,'(42x,A)') 'FOUND SEPARATE KEYWORD, NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "E_n_T_ARRAY.out"'
                                           IF(BL%LINE(LLOC:) == BLNK) THEN
                                                   LLOC=ONE
                                                   CALL OFL%ET_ARRAY_SEP%OPEN("E_n_T_ARRAY.out",LLOC,BL%IOUT,BL%IU)
                                           ELSE
                                                   CALL OFL%ET_ARRAY_SEP%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                                   !
                                                   IF(OFL%ET_ARRAY_SEP%IU==Z)  CALL OFL%ET_ARRAY_SEP%OPEN("E_n_T_ARRAY.out",LLOC,BL%IOUT,BL%IU)
                                           END IF
                        CASE DEFAULT;     CALL STOP_ERROR(OUTPUT=BL%IOUT, MSG='FMP OUTPUT BLOCK ERROR. IF YOU SPECIFY KEYWORD "ET_ARRAY",'//NL//'YOU MUST FOLLOW IT WITH THE KEYWORD "SUM" OR "SEPARATE".')
                        END SELECT
      CASE ("ET_LIST","EVAPOTRANSPIRATION_LIST")
                        WRITE(BL%IOUT,'(A)') '   EVAPOTRANSPIRATION_LIST (ET_LIST)      OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "ET_LIST.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%ET_LIST%OPEN("ET_LIST.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%ET_LIST%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%ET_LIST%IU==Z) CALL OFL%ET_LIST%OPEN("ET_LIST.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("ROUTING_INFORMATION")
                        WRITE(BL%IOUT,'(A)') '   ROUTING_INFORMATION                    OUTPUT KEYWORD FOUND. NOW LOADING KEYWORD "STATIC" OR "TRANSIENT" AND OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "ROUT.out"'
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) ) ! HAS_ROUT => 1 = STATIC, 2 = TRANSIENT 
                        CASE("STATIC");    OFL%HAS_ROUT = TWO
                        CASE("TRANSIENT"); OFL%HAS_ROUT = ONE
                        CASE DEFAULT;      CALL STOP_ERROR(OUTPUT=BL%IOUT, MSG='FMP OUTPUT BLOCK ERROR. IF YOU SPECIFY KEYWORD "ROUTING_INFORMATION", YOU MUST FOLLOW IT WITH THE KEYWORD "STATIC" OR "TRANSIENT".')
                        END SELECT
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%ROUTING_INFORMATION%OPEN("ROUT.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%ROUTING_INFORMATION%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%ROUTING_INFORMATION%IU==Z) CALL OFL%ROUTING_INFORMATION%OPEN("ROUT.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("GROUNDWATER_EVAPORATION", "EGWA")
                        WRITE(BL%IOUT,'(A)') '   GROUNDWATER_EVAPORATION (EGWA)         OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "EGWA.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%EGWA%OPEN("EGWA.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%EGWA%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%EGWA%IU==Z) CALL OFL%EGWA%OPEN("EGWA.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("GROUNDWATER_TRANSPIRATION", "TGWA")
                        WRITE(BL%IOUT,'(A)') '   GROUNDWATER_TRANSPIRATION (TGWA)       OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "TGWA.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN 
                                LLOC=ONE
                                CALL OFL%TGWA%OPEN("TGWA.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%TGWA%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%TGWA%IU==Z) CALL OFL%TGWA%OPEN("TGWA.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("GROUNDWATER_EVAPOTRANSPIRATION", "ETGW")
                        WRITE(BL%IOUT,'(A)') '   GROUNDWATER_EVAPOTRANSPIRATION (ETGW)  OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "ETGW.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%ETGW%OPEN("ETGW.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%ETGW%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%ETGW%IU==Z) CALL OFL%ETGW%OPEN("ETGW.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("IRRIGATION_EVAPORATION", "EI")
                        WRITE(BL%IOUT,'(A)') '   IRRIGATION_EVAPORATION (EI)            OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "EI.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%EI%OPEN("EI.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%EI%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%EI%IU==Z) CALL OFL%EI%OPEN("EI.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("IRRIGATION_TRANSPIRATION", "TI")
                        WRITE(BL%IOUT,'(A)') '   IRRIGATION_TRANSPIRATION (TI)          OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "TI.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%TI%OPEN("TI.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%TI%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%TI%IU==Z) CALL OFL%TI%OPEN("TI.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("IRRIGATION_EVAPOTRANSPIRATION", "ETI")
                        WRITE(BL%IOUT,'(A)') '   IRRIGATION_EVAPOTRANSPIRATION (ETI)    OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "ETI.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%ETI%OPEN("ETI.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%ETI%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%ETI%IU==Z) CALL OFL%ETI%OPEN("ETI.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("PRECIPITATION_EVAPORATION", "EP")
                        WRITE(BL%IOUT,'(A)') '   PRECIPITATION_EVAPORATION (EP)         OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "EP.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%EP%OPEN("EP.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%EP%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%EP%IU==Z) CALL OFL%EP%OPEN("EP.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("PRECIPITATION_TRANSPIRATION", "TP")
                        WRITE(BL%IOUT,'(A)') '   PRECIPITATION_TRANSPIRATION (TP)       OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "TP.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%TP%OPEN("TP.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%TP%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%TP%IU==Z) CALL OFL%TP%OPEN("TP.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("PRECIPITATION_EVAPOTRANSPIRATION", "ETP")
                        WRITE(BL%IOUT,'(A)') '   PRECIPITATION_EVAPOTRANSPIRATION (ETP) OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "ETP.out"'
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%ETP%OPEN("ETP.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%ETP%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%ETP%IU==Z) CALL OFL%ETP%OPEN("ETP.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
      CASE ("LANDSCAPE_RUNOFF", "RUNOFF")
                        WRITE(BL%IOUT,'(A)') '   LANDSCAPE_RUNOFF (RUNOFF)              OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "RUNOFF.out"'
                        !
                        CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY) 
                        IF(KEY.NE.'COMPACT') LLOC = ISTART
                        !
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%RUNOFF%OPEN("RUNOFF.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%RUNOFF%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%RUNOFF%IU==Z) CALL OFL%RUNOFF%OPEN("RUNOFF.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
                        IF(OFL%RUNOFF%NULL_FILE) CALL OFL%RUNOFF%CLOSE()
                        !
                        IF(OFL%RUNOFF%IS_OPEN .AND. KEY=='COMPACT') OFL%RUNOFF%NULL_FILE = TRUE
                        !
      CASE ("DEEP_PERCOLATION", "INFILTRATION")
                        WRITE(BL%IOUT,'(A)') '   DEEP_PERCOLATION (INFILTRATION)        OUTPUT KEYWORD FOUND. NOW OPENING GENERIC_OUTPUT FILE. IF NOTHING SPECIFIED THEN OPENING FILE: "DPERC.out"'
                        !
                        CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY) 
                        IF(KEY.NE.'COMPACT') LLOC = ISTART
                        !
                        IF(BL%LINE(LLOC:) == BLNK) THEN
                                LLOC=ONE
                                CALL OFL%DPERC%OPEN("DPERC.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        ELSE
                                CALL OFL%DPERC%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE)
                                !
                                IF(OFL%DPERC%IU==Z) CALL OFL%DPERC%OPEN("DPERC.out",LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11,NO_BINARY=TRUE, NO_INTERNAL=TRUE)
                        END IF
                        !
                        IF(OFL%DPERC%NULL_FILE) CALL OFL%DPERC%CLOSE()
                        !
                        IF(OFL%DPERC%IS_OPEN .AND. KEY=='COMPACT') OFL%DPERC%NULL_FILE = TRUE
      CASE DEFAULT
                        CALL WARN_MSG1%ADD('FOUND UNKNOWN KEYWORD "'//BL%LINE(ISTART:ISTOP)//'" ***IT WILL BE IGNORED***'//BLN)
                        
      END SELECT
      !
      CALL BL%NEXT()
      CALL WARN_MSG1%CHECK(HED='FMP OUTPUT BLOCK'//NL,INFILE=BL%IU,OUTPUT=BL%IOUT,INLINE=TRUE,CMD_PRINT=TRUE,TAIL=NL)
      CALL WARN_MSG2%CHECK(INFILE=BL%IU,OUTPUT=BL%IOUT)
      !
    END DO
    !
    SELECT CASE(ITMUNI)
    CASE(1); TIMEUNIT='       SECONDS'
    CASE(2); TIMEUNIT='       MINUTES'
    CASE(3); TIMEUNIT='         HOURS'
    CASE(4); TIMEUNIT='          DAYS'
    CASE(5); TIMEUNIT='         YEARS'
    CASE DEFAULT
            TIMEUNIT='       UNKNOWN'
    END SELECT
    !
    IF(OFL%FDS       %IS_OPEN .AND. .NOT. OFL%FDS       %BINARY) CALL OFL%FDS       %SET_HEADER('  PER  STP'//TIMEUNIT//'    FID         OFE              TFDR-INI            NR-SWD-INI             R-SWD-INI              QREQ-INI              TFDR-FIN            NR-SWD-FIN             R-SWD-FIN              QREQ-FIN                 Q-FIN  DEF-FLAG           DATE_START   ACTIVE')
    IF(OFL%FB_COMPACT%IS_OPEN .AND. .NOT. OFL%FB_COMPACT%BINARY) CALL OFL%FB_COMPACT%SET_HEADER('         PER         STP'//TIMEUNIT//'         FID            Q-p-in           Q-sw-in           Q-gw-in          Q-ext-in          Q-tot-in          Q-et-out       Q-ineff-out          Q-sw-out          Q-gw-out         Q-tot-out          Q-in-out  Q-Discrepancy[%]        DATE_START            ACTIVE')
    IF(OFL%FB_DETAILS%IS_OPEN .AND. .NOT. OFL%FB_DETAILS%BINARY) CALL OFL%FB_DETAILS%SET_HEADER('         PER         STP'//TIMEUNIT//'         FID            Q-p-in          Q-nrd-in          Q-srd-in          Q-drt-in        Q-wells-in          Q-egw-in          Q-tgw-in         Q-drch-in          Q-ext-in          Q-tot-in          Q-ei-out          Q-ep-out         Q-egw-out          Q-ti-out          Q-tp-out         Q-tgw-out         Q-run-out          Q-dp-out         Q-nrd-out         Q-srd-out          Q-rd-out       Q-wells-out         Q-tot-out          Q-in-out  Q-Discrepancy[%]   DATE_START            ACTIVE')
    IF(OFL%ET_LIST   %IS_OPEN .AND. .NOT. OFL%ET_LIST   %BINARY) CALL OFL%ET_LIST   %SET_HEADER('  PER  STP'//TIMEUNIT//'    FARM_ID           EVAPOARATION          TRANSPIRATION     EVAPOTRANSPIRATION')
    IF(OFL%FNRCH_LIST%IS_OPEN .AND. .NOT. OFL%FNRCH_LIST%BINARY) CALL OFL%FNRCH_LIST%SET_HEADER('  PER  STP'//TIMEUNIT//'    FARM_ID         RATE')
    !
    IF(OFL%WBS_WATER_USE%IS_OPEN .AND. .NOT. OFL%WBS_WATER_USE%BINARY) &
       CALL OFL%WBS_WATER_USE%SET_HEADER('    PER    STP    WBS             AREA   IRRIGATED_AREA          PET_NAT         ETgw_NAT          ETp_NAT            P_NAT          PET_IRR         ETgw_IRR          ETp_IRR          ETi_IRR            P_IRR       DEMAND_POT       EFFICIENCY          NRD_USE          SRD_USE            Q_USE             DELT   DYEAR            DATE_START')
    !
    !IF(OFL%ET_ARRAY_SUM       %IS_OPEN) CALL OFL%ET_ARRAY_SUM       %SET_HEADER()
    !IF(OFL%ET_ARRAY_SEP       %IS_OPEN) CALL OFL%ET_ARRAY_SEP       %SET_HEADER()
    !IF(OFL%FNRCH_ARRAY        %IS_OPEN) CALL OFL%FNRCH_ARRAY        %SET_HEADER()
    !IF(OFL%FWELLS             %IS_OPEN) CALL OFL%FWELLS             %SET_HEADER()
    !IF(OFL%ROUTING_INFORMATION%IS_OPEN) CALL OFL%ROUTING_INFORMATION%SET_HEADER()
    !
    ! NOT YET IMPLIMENTED
    !
    !IF(OFL%EGWA               %IS_OPEN) CALL OFL%EGWA               %SET_HEADER()
    !IF(OFL%TGWA               %IS_OPEN) CALL OFL%TGWA               %SET_HEADER()
    !IF(OFL%TI                 %IS_OPEN) CALL OFL%TI                 %SET_HEADER()
    !IF(OFL%EI                 %IS_OPEN) CALL OFL%EI                 %SET_HEADER()
    !IF(OFL%TP                 %IS_OPEN) CALL OFL%TP                 %SET_HEADER()
    !IF(OFL%EP                 %IS_OPEN) CALL OFL%EP                 %SET_HEADER()
    !IF(OFL%ETGW               %IS_OPEN) CALL OFL%ETGW               %SET_HEADER()
    !IF(OFL%ETI                %IS_OPEN) CALL OFL%ETI                %SET_HEADER()
    !IF(OFL%ETP                %IS_OPEN) CALL OFL%ETP                %SET_HEADER()
    !IF(OFL%RUNOFF             %IS_OPEN) CALL OFL%RUNOFF             %SET_HEADER()
    !IF(OFL%DPERC              %IS_OPEN) CALL OFL%DPERC              %SET_HEADER()
    !
  END SUBROUTINE 
  !
END MODULE
!
!#########################################################################################################
!
