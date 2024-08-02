! IDEA GIVE EACH FEED FILE ITS OWN BUDGET...ALLOW OPTION FOR BUDGET EXPORT
!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LINE_FEEDER
!  MODULE DESIGN AS ALTERNATIVE INPUT TO MODFLOW PACKAGES
!  CREATES A FORTRAN OBJECT THAT LOADS ALL PROPERTIES THEN
!  READS FOR EVERY STRESS PERIOD THE TIME VARARING PROPERTIES
!
!  VERSION 1.0 [4/23/2015] ORIGINAL VERSION THAT RETURNS A LINE_FEED DATATYPE
!
MODULE LINE_FEEDER
  USE CONSTANTS
  USE GLOBAL,                           ONLY: IUNIT, CUNIT, XYGRID, BOTM, LBOTM, NLAY
  USE ERROR_INTERFACE,                  ONLY: STOP_ERROR, FILE_IO_ERROR
  USE FILE_IO_INTERFACE,                ONLY: READ_TO_DATA
  USE PARSE_WORD_INTERFACE,             ONLY: PARSE_WORD_UP, GET_WORD
  USE STRINGS,                          ONLY: GET_INTEGER, GET_NUMBER
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR
  USE GENERIC_INPUT_FILE_INSTRUCTION,   ONLY: GENERIC_INPUT_FILE
  USE PAK_PROP_INTERFACE,               ONLY: PAK_PROP, PAK_PROPTAB
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  USE GENERIC_OPEN_INTERFACE,           ONLY: UTF8_BOM_OFFSET_REWIND
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: LINE_FEED                                               ! LINE_FEED serves both as allocation routine and data type definition
  !
  TYPE:: FEED_DATA
      !INTEGER:: IN                                                ! Holds  unit number of line_feed
      TYPE(GENERIC_INPUT_FILE):: FL
      TYPE(GENERIC_INPUT_FILE), ALLOCATABLE:: BIN                  ! Only allocated if feed file is a binary
      DOUBLE PRECISION:: PROP_SFAC = UNO                           ! Global scale factor
      INTEGER:: NDAT                                               ! Size of DAT
      !INTEGER:: NACT                                              ! Size of currently active cells (in use for current stress period)
      INTEGER,         DIMENSION(:,:),ALLOCATABLE:: NUMIDX         ! SIZE(NUMIDX,1)>0 when active
      CHARACTER(20),   DIMENSION(:),  ALLOCATABLE:: CHARID          
      INTEGER,         DIMENSION(:),  ALLOCATABLE:: CHARIDX        ! SIZE(CHARIDX)>0 when active
      !CLASS(*),        DIMENSION(:),  ALLOCATABLE:: WILD          ! Can be any user defined derived datatype (its a wild card)
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: PROP           ! Time invariant properties (like drain elevation)
      INTEGER,         DIMENSION(:,:),ALLOCATABLE:: AUX            ! Auxiliary information (like NAUX/AUX from normal reads)
      CHARACTER(16),   DIMENSION(:),  ALLOCATABLE:: GRP  
      DOUBLE PRECISION,DIMENSION(:),  ALLOCATABLE:: DAT            ! Current stress periods data as float
      INTEGER,         DIMENSION(:),  ALLOCATABLE:: IDAT           ! Current stress periods data as int
  END TYPE 
  !
  TYPE:: LINE_FEED
      LOGICAL:: BINARY = FALSE                                     ! Determines if second half of feedfile is binary
      LOGICAL:: INT_DAT= FALSE                                     ! Determines if dat or idat is in use
      INTEGER:: NFEED  = NEG                                       ! The size of fd  --init at -1, set to zero if not found
      INTEGER:: TOTDAT = Z                                         ! Total count of all feed points ( count of FD(:)%DAT(:) )  -note if NFEED=0 then TOTDAT=0
      INTEGER:: NACT   = Z                                         ! Total count of all feed points in use for current stress period ( count of FD(:)%NACT )
      INTEGER:: IPRT   = Z                                         ! Print flag 0 means no printing
      INTEGER,                     ALLOCATABLE:: LDIM              ! Leading dimension of NUMIDX (0, 1, or >3), if equal to zero then charid is allocated and NUMIDX is not used
      INTEGER,                     ALLOCATABLE:: NPROP             ! Number of properties associated with line feed. for example GHB has NPROP=1 because a GHB cell has a conductance assigned to it
      INTEGER,                     ALLOCATABLE:: IOUT              ! Shadow copy of list unit number
      CHARACTER(5),                ALLOCATABLE:: PAK               ! Encompansing package
      CHARACTER(16),DIMENSION(:),  ALLOCATABLE:: CAUX              ! List of additional AUX variables
      LOGICAL,                     ALLOCATABLE:: XYCOORD           ! Logical the determines if X,Y,Layer is read in rather than Layer, Row, Col
      LOGICAL,                     ALLOCATABLE:: XYZCOORD          ! Logical the determines if X,Y,Z     is read in rather than Layer, Row, Col
      TYPE(FEED_DATA),DIMENSION(:),ALLOCATABLE:: FD                ! Holds each of the line feeds
      !
      CONTAINS
      !
      GENERIC::               INIT      => FEED_ALLOCATE, FEED_ALLOCATE_BLOCK!(IN,IOUT,LINE,NOREAD), (BL)
      PROCEDURE, PASS(FEED):: CELLS     => FEED_CELLS
      PROCEDURE, PASS(FEED):: NEXTLINE  => LINEFEED_LINE_READ
      !
      PROCEDURE, PASS(FEED):: PAKPROP_APPLY => FEED_APPLYLINE_PAKPROP
      PROCEDURE, PASS(FEED):: FEED_MNW2     => FEED_APPLYLINE_MNW2
      PROCEDURE, PASS(FEED):: FEED_SFR      => FEED_APPLYLINE_SFR
      PROCEDURE, PASS(FEED):: FEED_ALLOCATE
      PROCEDURE, PASS(FEED):: FEED_ALLOCATE_BLOCK
      !
      !GENERIC:: APPLYLINE => FEED_APPLYLINE_PAKPROP
      !
      FINAL:: FINAL_DEALLOCATE_LINEFEED
  END TYPE
  !
  !INTERFACE LINE_FEED
  !  MODULE PROCEDURE FEED_ALLOCATE
  !END INTERFACE
  !
CONTAINS
 !
 SUBROUTINE FEED_ALLOCATE_BLOCK(FEED,BL, EX1_WORD, EX1_POS)
  !     ******************************************************************
  !     GENERIC LINE_FEED INTERFACE AND READIN DEVELOPED BY SCOTT E. BOYCE 
  !     ALLOCATE AND READ LINE_FEEDS IF LINE_FEED FLAG IS FOUND
  !     IF LINE_FEED IS **NOT** PRESENT THEN THE DERIVED DATA TYPE, LINE_FEEDS,
  !     IS ALLOCATED AND LINE_FEED%NFEEDS IS SET TO 0
  !     
  !     ******************************************************************
  !
  !        SPECIFICATIONS:
  !     ------------------------------------------------------------------
  !
  CLASS(LINE_FEED),            INTENT(INOUT):: FEED
  CLASS(GENERIC_BLOCK_READER), INTENT(INOUT):: BL
  CHARACTER(*),      OPTIONAL, INTENT(IN   ):: EX1_WORD
  INTEGER,           OPTIONAL, INTENT(IN   ):: EX1_POS
  !
  ! LOCAL VARIABLES
  !
  INTEGER,PARAMETER:: MXAUX=2                                        !MAX NUMBER OF AUX STRINGS TO READ IN (CURRENTLY NOT USED, SO ITS KEPT SMALL)
  CHARACTER(16),DIMENSION(MXAUX):: CAUX
  CHARACTER(4) :: WORD
  INTEGER::LLOC,ISTART,ISTOP,FEEDAUX
  INTEGER::I,F,N,NFEED,PACKID, POS
  LOGICAL:: XYCOORD,XYZCOORD
  DOUBLE PRECISION:: SF, SF1, SF2
  ! ------------------------------------------------------------------
  !
  IF(PRESENT(EX1_POS)) THEN  !CODE MOST ALSO INCLUDE EX1_WORD if TRUE
      POS = EX1_POS
  ELSE
      POS = Z
  END IF
  !
  ! FIND CALLING PACKAGE
  PACKID=Z
  DO I=1,UBOUND(IUNIT,1)
    PACKID=PACKID+1
    IF(IUNIT(I) == BL%IU) EXIT
  END DO
  IF(PACKID == Z)THEN
      PACKID=UBOUND(IUNIT,1)
  END IF
  !
  IF(BL%NAME == 'LINEFEED') THEN
    FEEDAUX=Z
    CAUX=''
    XYCOORD =FALSE
    XYZCOORD=FALSE
    FEED%IPRT = Z
    !
    LLOC=1
    DO
      CALL PARSE_WORD_UP(BL%EXTRA,LLOC,ISTART,ISTOP)
      !
      IF    (BL%EXTRA(ISTART:ISTOP) == 'XY')   THEN
          XYCOORD =TRUE
      ELSEIF(BL%EXTRA(ISTART:ISTOP) == 'XYZ')  THEN
          XYZCOORD=TRUE
      ELSEIF(BL%EXTRA(ISTART:ISTOP) == 'PRINT')  THEN
          FEED%IPRT = 1
      ELSEIF(BL%EXTRA(ISTART:ISTOP) == 'BINARY')  THEN
          FEED%BINARY=TRUE
      ELSEIF(BL%EXTRA(ISTART:ISTOP) /= ' ' .AND. FEEDAUX < MXAUX ) THEN     !REACHED THE END OF THE BL%EXTRA THAT IS READ IN
          FEEDAUX=FEEDAUX+1
          CAUX(FEEDAUX)=BL%EXTRA(ISTART:ISTOP)
      ELSE
          EXIT
      END IF
    END DO
    !
  END IF
  !
  NFEED = Z
  !
  CALL BL%START()
  DO I=1, BL%NLINE
      !
      LLOC=1
      CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,WORD)
      !
      IF(WORD /= 'SFAC') NFEED = NFEED + ONE
      !
      CALL BL%NEXT()
  END DO
  !
  FEED%NFEED=NFEED
  !
  IF(FEED%NFEED <= Z)THEN
     WRITE(BL%IOUT,'(/ 2A /)') ' NO LINEFEEDS ARE SPECIFIED FOR PACKAGE ',CUNIT(PACKID)
     FEED%NFEED=Z
     FEED%TOTDAT=Z
     FEED%NACT=Z
     FEED%IPRT=Z
     RETURN      
  END IF
  !
  WRITE(BL%IOUT,'(//2A, /A, /)')  '"LINEFEED" BLOCK SPECIFIED IN PACKAGE ',CUNIT(PACKID), &
                                            ' LINEFEED INFORMATION WILL BE APPLIED TO EACH STRESS PERIOD'
  !
  ! ALLOCATE LINE_FEED DATA TYPE AND ITS PARTS
  ALLOCATE( FEED%FD(FEED%NFEED) )
  !
  ALLOCATE(FEED%IOUT,  FEED%PAK,  FEED%XYCOORD,  FEED%XYZCOORD )
  !
  IF(FEEDAUX > Z) THEN
      ALLOCATE( FEED%CAUX, SOURCE=CAUX(1:FEEDAUX) )
      !ALLOCATE(FEED%CAUX(FEEDAUX))
      !FEED%CAUX=CAUX(1:FEEDAUX)
  END IF
  !
  FEED%IOUT=BL%IOUT
  FEED%PAK = CUNIT(PACKID)
  FEED%TOTDAT=Z
  FEED%NACT=Z
  FEED%XYCOORD =XYCOORD
  FEED%XYZCOORD=XYZCOORD
  !
  WORD = BLNK
  SF1  = UNO
  SF2  = UNO
  !
  I = Z
  CALL BL%START()
  DO F=1, BL%NLINE
      !
      LLOC=1                                       !Check for SFAC
      CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,WORD)
      !
      IF(WORD == 'SFAC') THEN
          !
          IF(POS > Z) THEN
              N = LLOC
              CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)  !CHECK FOR DimKey
              !
              IF(BL%LINE(ISTART:ISTOP) == EX1_WORD)  THEN
                  WORD = 'EX1'                              !Flag to indicate found DimKey 
              ELSE
                  LLOC = N
                  WORD = BLNK
              END IF
          ELSE
              WORD = BLNK
          END IF
          !
          CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SF,MSG='LINEFEED FOUND KEYWORD SFAC, BUT FAILED TO LOAD THE SCALE FACTOR NUMBER. IF YOU ARE SPECIFYING "SFAC DimKey", CHECK TO SEE IF YOUR DimKey IS CORRECT.')
          !
          IF(WORD == BLNK) THEN
                               SF1 = SF1*SF
          ELSE
                               SF2 = SF2*SF
          END IF
          !
      ELSE
          I = I + ONE
          !
          IF(FEED%BINARY) THEN
              N = INDEX(BL%LINE,' BINARY ')
              IF(N < ONE) CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='LINEFEED CONTAINED "BINARY" KEYWORD WHICH INDICATES THAT ALL FEEDFILES MUST BE SPECIFIED WITH TWO FILES WITH THE WORD " BINARY " BETWEEN THEM (e.g FILE1.txt BINARY FILE2.bin).'//NL//'FAILED TO LOCATE ON LINE THE KEYWORD " BINARY ",'//NL//'EITHER CHECK IF THE WORD IS THERE (CASE SENSITIVE) OR REMOVE "BINARY" FROM'//NL//'"BEGIN  LINEFEED  '//BL%EXTRA//'".')
          ELSE
              N = LEN_TRIM(BL%LINE)
          END IF
          !
          LLOC = ONE
          CALL FEED%FD(I)%FL%OPEN(BL%LINE(:N),LLOC,BL%IOUT,BL%IU, NO_INTERNAL=TRUE, NO_CONSTANT=TRUE)
          !
          IF( FEED%FD(I)%FL%BINARY ) CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='WHEN OPENING LINEFEED FILE, FOUND "BINARY" KEYWORD, HOWEVER BINARY FORMAT IS NOT SUPPORTED FOR LINEFEED.'//NL//'(UNLESS YOU HAVE "BEGIN LINEFEED BINARY", THEN THE SECOND FILE IS BINARY FORMAT AND THE FIRST MUST BE TEXT.)'//NL//'PLEASE REMOVE "BINARY" KEYWORD AND USE TEXT INPUT.')
          !
          IF(FEED%BINARY) THEN
              ALLOCATE(FEED%FD(I)%BIN)
              LLOC = N
              CALL FEED%FD(I)%BIN%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,NO_INTERNAL=TRUE, NO_CONSTANT=TRUE)
          END IF
          !
          IF( SF1 /= UNO ) FEED%FD(I)%FL%SCALE = FEED%FD(I)%FL%SCALE * SF1
          !
          IF( SF2 /= UNO ) THEN
                             FEED%FD(I)%NDAT = POS   !Use NDAT to Temporarily Hold Scale Position
          ELSE
                             FEED%FD(I)%NDAT = Z
          END IF
          !
          FEED%FD(I)%PROP_SFAC = SF2
          !
          SF1 = UNO
          SF2 = UNO
      END IF
      !
      CALL BL%NEXT()
  END DO
  !
 END SUBROUTINE
 !
 SUBROUTINE FEED_ALLOCATE(FEED,IN,IOUT,LINE,NOREAD)
  !     ******************************************************************
  !     GENERIC LINE_FEED INTERFACE AND READIN DEVELOPED BY SCOTT E. BOYCE 
  !     ALLOCATE AND READ LINE_FEEDS IF LINE_FEED FLAG IS FOUND
  !     IF LINE_FEED IS **NOT** PRESENT THEN THE DERIVED DATA TYPE, LINE_FEEDS,
  !     IS ALLOCATED AND LINE_FEED%NFEEDS IS SET TO 0
  !     
  !     ******************************************************************
  !
  !        SPECIFICATIONS:
  !     ------------------------------------------------------------------
  !
  CLASS(LINE_FEED),   INTENT(INOUT):: FEED
  !
  INTEGER,           INTENT(IN   ):: IN, IOUT
  CHARACTER(*),      INTENT(INOUT):: LINE
  LOGICAL, OPTIONAL, INTENT(IN   ):: NOREAD
  !
  ! LOCAL VARIABLES
  !
  INTEGER,PARAMETER:: MXAUX=2                                        !MAX NUMBER OF AUX STRINGS TO READ IN (CURRENTLY NOT USED, SO ITS KEPT SMALL)
  CHARACTER(16),DIMENSION(MXAUX):: CAUX
  INTEGER::LLOC,ISTART,ISTOP,FEEDAUX
  INTEGER::I,NFEED,PACKID
  LOGICAL:: XYCOORD,XYZCOORD, READ_NEXT_LINE, KEY_FOUND
! ------------------------------------------------------------------
  ! 
  READ_NEXT_LINE = TRUE 
  IF(PRESENT(NOREAD)) READ_NEXT_LINE = .NOT. NOREAD
  !
  ! FIND CALLING PACKAGE
  PACKID=Z
  DO I=1,UBOUND(IUNIT,1)
    PACKID=PACKID+1
    IF(IUNIT(I) == IN) EXIT
  END DO
  IF(PACKID == Z)THEN
      PACKID=UBOUND(IUNIT,1)
  END IF
  !
  ! CHECK IF TABFILE IS SPECIFIED FOR SPECIFIC PACKAGE
  ! IF NOT SPECIFIED ALLOCATE AND RETURN
  LLOC=1
  CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
  !
  KEY_FOUND = FALSE
  NFEED=Z
  IF(LINE(ISTART:ISTOP) == 'LINEFEED') THEN
    KEY_FOUND = TRUE
    CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,NFEED,MSG='FOUND LINEFEED KEYWORD, BUT FAILED TO LOAD THE NUMBER OF FEED FILES, "NFEED".')
    FEEDAUX=Z
    CAUX=''
    XYCOORD =FALSE
    XYZCOORD=FALSE
    FEED%IPRT = Z
    DO
      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP,TRUE)
      !
      IF    (LINE(ISTART:ISTOP) == 'XY')   THEN
          XYCOORD =TRUE
      ELSEIF(LINE(ISTART:ISTOP) == 'XYZ')  THEN
          XYZCOORD=TRUE
      ELSEIF(LINE(ISTART:ISTOP) == 'PRINT')  THEN
          FEED%IPRT = 1
      ELSEIF(LINE(ISTART:ISTOP) /= ' ' .AND. FEEDAUX < MXAUX ) THEN     !REACHED THE END OF THE LINE THAT IS READ IN
          FEEDAUX=FEEDAUX+1
          CAUX(FEEDAUX)=LINE(ISTART:ISTOP)
      ELSE
          EXIT
      END IF
    END DO
    !
  END IF
  !
  FEED%NFEED=NFEED
  !
  IF(FEED%NFEED <= Z)THEN
     WRITE(IOUT,'(/ 2A /)') ' NO LINEFEEDS ARE SPECIFIED FOR PACKAGE ',CUNIT(PACKID)
     FEED%NFEED=Z
     FEED%TOTDAT=Z
     FEED%NACT=Z
     FEED%IPRT=Z
     IF(KEY_FOUND.AND.READ_NEXT_LINE) CALL READ_TO_DATA(LINE,IN,IOUT,IOUT)
     RETURN      
  END IF
  !
  WRITE(IOUT,'(//2A, /A, /)')  '"LINEFEED" FLAG SPECIFIED IN PACKAGE ',CUNIT(PACKID), &
                                            ' LINEFEED INFORMATION WILL BE APPLIED TO EACH STRESS PERIOD'
  !
  ! ALLOCATE LINE_FEED DATA TYPE AND ITS PARTS
  ALLOCATE( FEED%FD(FEED%NFEED) )
  !
  ALLOCATE(FEED%IOUT,  FEED%PAK,  FEED%XYCOORD,  FEED%XYZCOORD )
  !
  IF(FEEDAUX > Z) THEN
      ALLOCATE( FEED%CAUX, SOURCE=CAUX(1:FEEDAUX) )
      !ALLOCATE(FEED%CAUX(FEEDAUX))
      !FEED%CAUX=CAUX(1:FEEDAUX)
  END IF
  !
  FEED%IOUT=IOUT
  FEED%PAK = CUNIT(PACKID)
  FEED%TOTDAT=Z
  FEED%NACT=Z
  FEED%XYCOORD =XYCOORD
  FEED%XYZCOORD=XYZCOORD
  !
  DO I=1, FEED%NFEED
      !
      CALL READ_TO_DATA(LINE,IN,IOUT,IOUT)
      !
      LLOC=1
      CALL FEED%FD(I)%FL%OPEN(LINE,LLOC,IOUT,IN,NO_INTERNAL=TRUE, NO_CONSTANT=TRUE)
      !
      IF( FEED%FD(I)%FL%BINARY ) CALL STOP_ERROR(LINE,IN,IOUT,MSG='WHEN OPENING LINEFEED FILE, FOUND "BINARY" KEYWORD, HOWEVER BINARY FORMAT IS NOT SUPPORTED FOR LINEFEED. PLEASE REMOVE "BINARY" KEYWORD AND USE TEXT INPUT.')
      !CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
      !!
      !IF(ISTOP-ISTART+1 == 8) THEN
      !    EXT=LINE(ISTART:ISTOP)                                        !COULD CONTAIN EXTERNAL KEY WORD
      !    CALL UPCASE(EXT)
      !ELSE
      !    EXT=' '
      !END IF
      !!
      !IF (EXT == 'EXTERNAL')THEN     
      !     CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,INFEED,MSG='LINEFEED ERROR: FOUND KEYWORD "EXTERNAL" BUT FAILED TO LOAD THE UNIT NUMBER.') !EXTERNAL FILE, READ UNIT NUMBER
      !ELSE
      !     !
      !     OPEN(NEWUNIT=INFEED, FILE=LINE(ISTART:ISTOP), ACTION='READ', IOSTAT=IERR)
      !     !
      !     IF(IERR /= Z) CALL FILE_IO_ERROR(IERR, FNAME=LINE(ISTART:ISTOP), LINE=LINE, INFILE=IN, OUTPUT=IOUT)
      !END IF
      !!
      !REWIND(INFEED)
      !FEED%FD(I)%IN=INFEED
  END DO
  !
  IF(KEY_FOUND.AND.READ_NEXT_LINE) CALL READ_TO_DATA(LINE,IN,IOUT,IOUT)
  !
 END SUBROUTINE
 !
 SUBROUTINE FEED_CELLS(FEED,LDIM,NPROP,NAUX,IPRT,NML,BUDGET_GROUPS, INT_DAT)!,WILD)
  ! ******************************************************************
  ! READ IN CELLS THAT ARE ASSOCIATED WITH THE COLLUMNS OF THE FEED
  ! ******************************************************************
  !
  CLASS(LINE_FEED):: FEED
  !
  INTEGER,                   INTENT(IN):: LDIM,NPROP,NAUX, IPRT
  CHARACTER(*), DIMENSION(:),INTENT(IN),OPTIONAL:: NML
  LOGICAL,                   INTENT(IN),OPTIONAL:: BUDGET_GROUPS
  LOGICAL,                   INTENT(IN),OPTIONAL:: INT_DAT
  CONTIGUOUS:: NML
  ! LOCAL VARIABLES
  CHARACTER(768)::LINE,FNAM
  CHARACTER(95)::SLINE        !SHORT LINE, USED TO FOR DUMMY LINE READS FOR SPEED
  DOUBLE PRECISION:: X,Y,ZZ
  INTEGER::LLOC,ISTART,ISTOP
  INTEGER::I,J,K,N,NDAT,IN,IERR,IOUT,LAY,ROW,COL,POS
  INTEGER:: a,b,c
  LOGICAL:: ERROR, USE_IDAT
  CHARACTER(:),ALLOCATABLE::ELINE
  ! ------------------------------------------------------------------
  !
  IF (FEED%NFEED < ONE) RETURN  !NO FEED FILES
  !
  ALLOCATE(FEED%LDIM,  SOURCE=LDIM)
  ALLOCATE(FEED%NPROP, SOURCE=NPROP)
  !
  USE_IDAT =FALSE
  IF(PRESENT(INT_DAT)) USE_IDAT = INT_DAT
  FEED%INT_DAT = USE_IDAT
  !
  IOUT= FEED%IOUT
  IF(FEED%IPRT==Z .AND. IPRT>Z) FEED%IPRT = 1
  !
  WRITE(IOUT,'(/2A/)') TRIM(FEED%PAK),' LINEFEED LOADING MODEL CELLS THAT WILL BE FED STRESS PERIOD INFORMATION'
  !
  IF(FEED%IPRT /= Z.AND.FEED%XYCOORD) WRITE(IOUT,'(2A//A)')               &
       TRIM(FEED%PAK),' LINEFEED COORDINATES WILL BE TRANSLATED TO LAY, ROW, COL',       &
       '       X               Y             LAY         =>     LAY    ROW    COL'
  !
  IF(FEED%IPRT /= Z.AND.FEED%XYZCOORD) WRITE(IOUT,'(2A//A)')               &
       TRIM(FEED%PAK),' LINEFEED COORDINATES WILL BE TRANSLATED TO LAY, ROW, COL',       &
       '       X               Y               Z         =>     LAY    ROW    COL'
  !
  ! COUNT AND ALLOCATE LOOPS
  ERROR=FALSE
  ELINE=''
  FEED_FILE: DO I=1, FEED%NFEED
    !
    POS = FEED%FD(I)%NDAT    !Used NDAT to Temporarily Hold Scale Position - POS > Z if Prop Scale Factor
    !
    IN=FEED%FD(I)%FL%IU !FEED%FD(I)%IN
    NDAT=Z
    DO    !READ UNTIL "STRESS PERIOD" FLAG IS REACHED
      DO     !READ UNTIL FIRT NON_COMMENT
        READ(IN,'(A)',IOSTAT=IERR) SLINE
        !
        IF(IERR /= Z) THEN
          INQUIRE(IN,NAME=FNAM)
          LINE=TRIM(FEED%PAK)//' LINEFEED ERROR WHILE READING IN CELLS FROM LINEFEED FILE:'//NL//      &
               TRIM(FNAM)//NL//                                                                        &
               'THIS MOST LIKELY HAS OCCURED BECAUSE THE KEYWORD "STRESS PERIOD" WAS NOT FOUND'//NL//  &
               'OR THERE WAS A FAILURE TO READ THE FILE ITSELF.'//NL//                                 &
               'THE LAST LINE READ BEFORE FAILURE WAS:'//NL//                                          &
               TRIM(SLINE)
          CALL STOP_ERROR(INFILE=IN,MSG=LINE,OUTPUT=IOUT)
        END IF
        !
        SLINE = ADJUSTL(SLINE)
        IF(SLINE(1:1) /= '#') EXIT
        !
      END DO
      !
      LLOC = ONE
      CALL PARSE_WORD_UP(SLINE,LLOC,ISTART,ISTOP,TRUE)
      IF(SLINE(ISTART:ISTOP)=='TEMPORAL') THEN !Temporal Input
          EXIT
      ELSEIF(SLINE(ISTART:ISTOP)=='STRESS' ) THEN
          CALL PARSE_WORD_UP(SLINE,LLOC,ISTART,ISTOP,TRUE)
          IF(SLINE(ISTART:ISTOP)=='PERIOD' ) EXIT
      END IF
      !
      NDAT=NDAT+1
    END DO
    !
    FEED%FD(I)%NDAT=NDAT
    FEED%TOTDAT=FEED%TOTDAT+NDAT
    !
    IF (LDIM == Z) THEN
        ALLOCATE(FEED%FD(I)%CHARID(NDAT))
        IF(PRESENT(NML)) ALLOCATE(FEED%FD(I)%CHARIDX(NDAT), SOURCE=-1)
    ELSEIF(LDIM>Z) THEN
        ALLOCATE(FEED%FD(I)%NUMIDX(LDIM,NDAT))
    !ELSEIF(LDIM == -1) THEN
    !    ALLOCATE(FEED%FD(I)%WILD(NDAT), SOURCE=WILD)
    END IF  
    !
    IF(USE_IDAT) THEN
        ALLOCATE(FEED%FD(I)%IDAT(NDAT))
        POS = Z                        !Cannot Scale INT
    ELSE
        ALLOCATE(FEED%FD(I)%DAT(NDAT))
    END IF
    !
    IF(NPROP>Z)  ALLOCATE(FEED%FD(I)%PROP(NPROP, NDAT))
    IF(NAUX >Z)  ALLOCATE(FEED%FD(I)%AUX(NAUX, NDAT))
    !
    IF(PRESENT(BUDGET_GROUPS)) THEN
            IF(BUDGET_GROUPS) ALLOCATE(FEED%FD(I)%GRP(NDAT))
    END IF
    !
    CALL UTF8_BOM_OFFSET_REWIND(IN)                                                       !NOW THAT SIZE OF DATA IS ESTABLISED GO TO FIRST LINE AND READ DATA IN
    !
    ! READ IN THE DATA
    INQUIRE(IN,NAME=FNAM)
    DATA_LOAD: DO J=1, NDAT
      CALL READ_TO_DATA(LINE,IN)
      LLOC=1
      !          
      SELECT CASE(LDIM)
       CASE(Z)
          CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
          FEED%FD(I)%CHARID(J) = LINE(ISTART:ISTOP)
          !
          IF(PRESENT(NML)) THEN
                               !FEED%FD(I)%CHARIDX(J) = -1  ---PRESET BY SOURCE=-1
                               DO K=1, SIZE(NML)
                                         IF ( NML(K) == FEED%FD(I)%CHARID(J) ) THEN
                                             FEED%FD(I)%CHARIDX(J) = K
                                             EXIT
                                         END IF
                               END DO
                               !
                               IF(FEED%FD(I)%CHARIDX(J) == -1) THEN   !DID NOT FIND CHARACTER NAME
                                    !WRITE(IOUT,'(A)') 'LINEFEED WARNING: FAILED TO LOCATE CHARACTER NAME "'//FEED%FD(I)%CHARID(J)//'" IN LIST OF PROVIDED NAMES. THIS COLUMN IN THE FEEDFILE WILL BE IGNORED (ie NOT APPLIED/SKIPPED).   ***IF YOU ARE USING MNW2 THEN THIS OCCURRED BECAUSE THE WELLID WAS NOT LOCATED AND THE WELL WILL NEVER BE IN USE VIA THE LINEFEED FILE.'
                                    ELINE=ELINE//'FEEDFILE '//TRIM(FNAM)//' FAILED TO LOCATE CHARACTER NAME "'//FEED%FD(I)%CHARID(J)//'" IN LIST OF PROVIDED NAMES. THIS COLUMN IN THE FEEDFILE SHOULD BE DELETED IN ORDER TO RUN PROGRAM.   ***IF YOU ARE USING MNW2 THEN THIS OCCURRED BECAUSE THE WELLID WAS NOT LOCATED/DEFINED.'//NL
                                    ERROR = TRUE
                                    CYCLE
                               END IF
          END IF
          !
       CASE(1)
          CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,N,MSG='LINEFEED ERROR: FAILED TO LOAD ID NUMBER.')
          FEED%FD(I)%NUMIDX(1,J) = N
          !
       CASE(3,6)
          DO K=3, LDIM, 3                                           !DO LOOP TWICE IF LDIM IS 6
            a=K-2   !=1 or 4
            b=K-1   !=2 or 5
            c=K     !=3 or 6
            IF(FEED%XYCOORD .OR. FEED%XYZCOORD) THEN
              CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,X,MSG='LINEFEED ERROR: FAILED TO LOAD X COORDINATE.')
              CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,Y,MSG='LINEFEED ERROR: FAILED TO LOAD Y COORDINATE.')
              IF (FEED%XYZCOORD) THEN   !FLAG TO READ IN Z COORDINATE OR LAYER
                CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,ZZ,MSG='LINEFEED ERROR: FAILED TO LOAD Z COORDINATE.')
              ELSE
                CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,LAY,MSG='LINEFEED ERROR: FAILED TO LOAD LAYER NUMBER.')
              END IF
              !
              ! SEARCH FOR ROW AND COL
              !
              CALL XYGRID%XY2RC(X,Y,ROW,COL)
              !
              IF(ROW==Z .OR. COL==Z) THEN
                 ELINE=ELINE//'FEEDFILE '//TRIM(FNAM)//' FAILED TO LOCATE ROW AND COL FROM GIVEN X AND Y COORDINATES WHEN PROCESSING LINE:'//TRIM(LINE)//NL
                 ERROR = TRUE
                 CYCLE
              END IF
              !
              ! SEARCH FOR LAYER LOCATION
              IF (FEED%XYZCOORD) THEN
                 LAY=1
                 DO WHILE ( ZZ <= BOTM(COL,ROW,LBOTM(LAY)) )
                   LAY=LAY+1
                   IF(LAY > NLAY) THEN
                      ELINE=ELINE//'FEEDFILE '//TRIM(FNAM)//' SPECIFIED Z ELEVATION THAT IS BELOW THE BOTTOM OF MODEL WHEN PROCESSING LINE: '//TRIM(LINE)
                      ERROR = TRUE
                      CYCLE
                   END IF
                 END DO
                 !
                 IF(FEED%IPRT /= Z) WRITE(IOUT,'(3G16.8,A,3I7)') X,Y,ZZ,' => ',LAY,ROW,COL   ! THIS IF IS INCLUDED IN IF (FEED%XYZCOORD)
                 !
              ELSEIF(FEED%IPRT /= Z) THEN                                                    ! LAY ALREADY SPECIFIED, CHECK IF PRINT OUT IS REQUESTED
                 WRITE(IOUT,'(2G16.8,I16,A,3I7)') X,Y,LAY,' => ',LAY,ROW,COL
              END IF                  
              !
              !
            ELSE
              CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,LAY,MSG='LINEFEED ERROR: FAILED TO LOAD LAYER NUMBER.')
              CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,ROW,MSG='LINEFEED ERROR: FAILED TO LOAD ROW NUMBER.')
              CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,COL,MSG='LINEFEED ERROR: FAILED TO LOAD COLUMN NUMBER.')
            END IF
            !
            FEED%FD(I)%NUMIDX(a,J)=LAY
            FEED%FD(I)%NUMIDX(b,J)=ROW
            FEED%FD(I)%NUMIDX(c,J)=COL
            !
          END DO
       !CASE(-1)
       !   SELECT TYPE(WILD)
       !   TYPE IS(XXX)
       !       
       !   END SELECT
      END SELECT
      !
      IF(NPROP > Z) THEN
        FEED%FD(I)%PROP(:, J) = 0.D0
        DO K=1, NPROP
           CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,FEED%FD(I)%PROP(K, J),MSG='LINEFEED ERROR: FAILED TO LOAD MODEL FEATURE PROPERTY. THIS PACKAGE EXPECTS '//NUM2STR(NPROP)//' PROPERTIES TO BE LOADED. (AN EXAMPLE PROPTER WOULD BE GHB CONDUCTANCE IF YOU ARE USING THE GHB PACKAGE)')
        END DO
      END IF
      !
      IF (ALLOCATED(FEED%FD(I)%GRP)) THEN
          CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
          FEED%FD(I)%GRP(J) = LINE(ISTART:ISTOP)
      END IF
      !
      IF(NAUX > Z) THEN
        FEED%FD(I)%AUX(:, J) = Z
        DO K=1, NAUX
          CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,FEED%FD(I)%AUX(K,J),MSG='LINEFEED ERROR: FAILED TO LOAD AUXILIARY VARIABLE, THIS MUST BE EITHER 0 OR 1. GIVEN CURRENT INPUT SET UP, LINEFEED EXPECTS '//NUM2STR(NAUX)//' AUXILIARY NUMBERS TO BE LOADED')
        END DO
      END IF
    END DO DATA_LOAD
    !
    IF(POS > Z) THEN
      DO CONCURRENT (J=1:NDAT)
                              FEED%FD(I)%PROP(POS, J) = FEED%FD(I)%PROP(POS, J) * FEED%FD(I)%PROP_SFAC
      END DO 
    END IF
    !
    !CALL URDCOM ONE MORE TIME TO BY PASS STRESS PERIOD FLAG
    IF( (FEED%XYCOORD.OR.FEED%XYZCOORD) )WRITE(IOUT,*)
    CALL READ_TO_DATA(LINE,IN)
    !
  END DO FEED_FILE
  !
  IF (ERROR) THEN
      ELINE=NL//TRIM(FEED%PAK)//' LINEFEED ERRORS:'//NL//ELINE//NL
      WRITE(IOUT,'(A)') ELINE
      !CALL STOP_ERROR(INFILE=IN,MSG=LINE,OUTPUT=IOUT)
  END IF
  !
  IF(ALLOCATED(ELINE)) DEALLOCATE(ELINE)
 END SUBROUTINE
 !
 SUBROUTINE LINEFEED_LINE_READ(FEED)
  CLASS(LINE_FEED):: FEED
  !
  ! LOCAL VARIABLES
  CHARACTER(256)::LINE
  INTEGER:: I, J, IERR, NACT, INFEED
  LOGICAL:: EOF
  !
  IF(FEED%NFEED <= Z) RETURN
  !
  WRITE(FEED%IOUT,*)                                       !CARRAGE RETURN WHEN USING LINE FEED
  !
  IF(FEED%BINARY) LINE='UNREADABLE BINARY DATA -- NO LINE TO RETURN'
  !
  NACT=Z
  EOF = FALSE
  !
  IF( .NOT. FEED%INT_DAT .AND. .NOT. FEED%BINARY ) THEN  !DBLE AND ASCII
      DO I=1, FEED%NFEED
        INFEED=FEED%FD(I)%FL%IU
        CALL READ_TO_DATA(LINE, INFEED, EOF=EOF, BACK_UP=TRUE) !Read until first uncommented line, then backspace so that READ() will read the next uncommented line
        !
        IF(EOF) THEN
            IERR = NEG
        ELSE
            READ(INFEED,*,IOSTAT=IERR) FEED%FD(I)%DAT
        END IF
        IF(IERR /= Z) EXIT
        !
        IF(FEED%FD(I)%FL%SCALE /= UNO) THEN
              DO CONCURRENT(J=ONE:FEED%FD(I)%NDAT, FEED%FD(I)%DAT(J) == FEED%FD(I)%DAT(J));  FEED%FD(I)%DAT(J) = FEED%FD(I)%DAT(J) * FEED%FD(I)%FL%SCALE
              END DO
        END IF
        !
        NACT = NACT + COUNT(FEED%FD(I)%DAT == FEED%FD(I)%DAT)            !NaN CAN NOT EQUAL ITSELF
      END DO
  ELSEIF(   FEED%INT_DAT .AND. .NOT. FEED%BINARY ) THEN  !INT AND ASCII
      DO I=1, FEED%NFEED
        INFEED=FEED%FD(I)%FL%IU
        CALL READ_TO_DATA(LINE, INFEED, EOF=EOF, BACK_UP=TRUE) !Read until first uncommented line, then backspace so that READ() will read the next uncommented line
        !
        IF(EOF) THEN
            IERR = NEG
        ELSE
            READ(INFEED,*,IOSTAT=IERR) FEED%FD(I)%IDAT
        END IF
        IF(IERR /= Z) EXIT
        !
        NACT = NACT + COUNT(FEED%FD(I)%IDAT /= Z)                      !IF ZERO THEN ASSUMED INACTIVE
      END DO
  ELSEIF(.NOT. FEED%INT_DAT .AND. FEED%BINARY ) THEN  !DBL AND BIN
      DO I=1, FEED%NFEED
        INFEED=FEED%FD(I)%FL%IU
        !
        READ(INFEED,IOSTAT=IERR) FEED%FD(I)%DAT
        IF(IERR /= Z) EXIT
        !
        IF(FEED%FD(I)%FL%SCALE /= UNO) THEN
              DO CONCURRENT(J=ONE:FEED%FD(I)%NDAT, FEED%FD(I)%DAT(J) == FEED%FD(I)%DAT(J));  FEED%FD(I)%DAT = FEED%FD(I)%DAT * FEED%FD(I)%FL%SCALE
              END DO
        END IF
        !
        NACT = NACT + COUNT(FEED%FD(I)%DAT == FEED%FD(I)%DAT)            !NaN CAN NOT EQUAL ITSELF
      END DO
  ELSE!IF(     FEED%INT_DAT .AND. FEED%BINARY ) THEN  !INT AND BIN
      DO I=1, FEED%NFEED
        INFEED=FEED%FD(I)%FL%IU
        !
        READ(INFEED,IOSTAT=IERR) FEED%FD(I)%IDAT
        IF(IERR /= Z) EXIT
        !
        NACT = NACT + COUNT(FEED%FD(I)%IDAT /= Z)                      !IF ZERO THEN ASSUMED INACTIVE
      END DO
  END IF
  !
  IF(IERR /= Z) THEN
     IF(FEED%INT_DAT) THEN
         NACT = SIZE(FEED%FD(I)%IDAT)
     ELSE
         NACT = SIZE(FEED%FD(I)%DAT)
     END IF
     CALL FILE_IO_ERROR(IERR,INFEED,LINE=LINE,INFILE=INFEED,OUTPUT=FEED%IOUT,MSG=TRIM(FEED%PAK)//' LINEFEED ERROR WHILE READING STRESS PERIOD FEED INFORMATION.'//BLN//'NOTE THAT ERROR<0 CAN ALSO MEAN NOT ENOUGH NUMBERS OF LINE.'//NL//'THIS FEEDFILE EXPECTED TO LOAD '//NUM2STR(NACT)//' NUMBERS ON EACH LINE.'//NL//'**NOTE THAT REPLICATED ERROR LINE ONLY SHOWS THE FIRST 256 CHARACTERS.')
  END IF
  !
  FEED%NACT = NACT
  WRITE(FEED%IOUT,'(/2A,I7,A/)') TRIM(FEED%PAK),' LINEFEED HAS A TOTAL ACTIVE COUNT OF ',   &
                                          FEED%NACT,' FOR CURRENT STRESS PERIOD'
  !
 END SUBROUTINE
 !
 SUBROUTINE FEED_APPLYLINE_PAKPROP(FEED,PAK,IPVL,NTOT,LABEL,CAUX,IPRT)
  ! ******************************************************************
  ! APPLY CURRENT LINE FEED TO PACKAGE ARRAY FOR PACKAGES THAT USE
  ! PAK_PROP_INTERFACE
  ! ******************************************************************
  !
  CLASS(LINE_FEED):: FEED
  CLASS(PAK_PROP ),DIMENSION(:):: PAK
  INTEGER,      INTENT(INOUT):: NTOT
  INTEGER,      INTENT(IN   ):: IPVL, IPRT
  CHARACTER(*), INTENT(IN   ):: LABEL
  CHARACTER(16),INTENT(IN   ), DIMENSION(:):: CAUX
  CONTIGUOUS:: PAK, CAUX
  ! LOCAL VARIABLES
  INTEGER:: I, J, K, NPROP, IPROP, LDIM, NAUX, NCAUX, IOUT
  INTEGER:: ISTART
  CHARACTER(40)::  FMT1,FMT2
  !
  IF (FEED%NFEED <= Z) RETURN
  !
  IOUT = FEED%IOUT
  IF(FEED%IPRT==Z .AND. IPRT>Z) FEED%IPRT = 1
  WRITE(IOUT,*)                                       !CARRAGE RETURN WHEN USING LINE FEED
  !
  LDIM  = FEED%LDIM
  NPROP = FEED%NPROP
  NAUX  = SIZE(PAK(1)%AUX,1)   
  NCAUX = SIZE(CAUX)
  ISTART=NTOT+1
  !
  !Write a label for the list if the list will be printed.
  IF(FEED%IPRT == 1) THEN
     WRITE(IOUT,'(A/)') TRIM(FEED%PAK)//' LINEFEED STRESS PERIOD INFORMATION'
     CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
  END IF
  !
  DO I=1, FEED%NFEED
    IF(FEED%IPRT == 1) WRITE(IOUT,'(A,I2)')'FEED NUMBER ',I
    !
    DO J=1, FEED%FD(I)%NDAT
      IF( FEED%FD(I)%DAT(J) /= FEED%FD(I)%DAT(J) ) CYCLE          !SKIP PROCESSING NaN VALUES
      NTOT=NTOT+1
      IPROP=1
      PAK(NTOT)%LAY=FEED%FD(I)%NUMIDX(1,J)
      PAK(NTOT)%ROW=FEED%FD(I)%NUMIDX(2,J)
      PAK(NTOT)%COL=FEED%FD(I)%NUMIDX(3,J)
      DO K=1, NPROP+1
        IF     (K==IPVL) THEN
                              PAK(NTOT)%VAL(K) = FEED%FD(I)%DAT(J)
        ELSEIF (NPROP>Z) THEN
                              PAK(NTOT)%VAL(K) = FEED%FD(I)%PROP(IPROP,J)
                              IPROP=IPROP+1
        END IF
      END DO
      !
      IF (ALLOCATED(PAK(NTOT)%BUDGET_GROUP)) PAK(NTOT)%BUDGET_GROUP = FEED%FD(I)%GRP(J)
      !
      IF (NAUX > Z ) PAK(NTOT)%AUX = FEED%FD(I)%AUX(:,J)
      !
      IF(FEED%IPRT == 1)THEN
        WRITE(FMT1,*)LDIM
        WRITE(FMT2,*)NAUX
        FMT1=ADJUSTL(FMT1);  FMT2=ADJUSTL(FMT2)
        FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8,'//TRIM(FMT2)//'I10)'
        FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8)'
        IF(NAUX > Z)THEN   
         WRITE(IOUT,FMT2) NTOT, PAK(NTOT)%LAY, PAK(NTOT)%ROW, PAK(NTOT)%COL, PAK(NTOT)%VAL, PAK(NTOT)%AUX(1:NAUX)
        ELSE
         WRITE(IOUT,FMT1) NTOT, PAK(NTOT)%LAY, PAK(NTOT)%ROW, PAK(NTOT)%COL, PAK(NTOT)%VAL
        END IF
      END IF
    END DO
  END DO
  !
  SELECT TYPE (PAK) !ENSURE THAT TABFILES ARE NOT USED BY LINEFEED FILES
  CLASS IS (PAK_PROPTAB)
      IF( ALLOCATED(PAK(ISTART)%TABNAM) ) THEN
            DO CONCURRENT (I=ISTART:NTOT)
                  PAK(I)%TABIDX=Z
                  PAK(I)%TABNAM='     NO_TABFILE     '
            END DO
      END IF
  END SELECT
  !
 END SUBROUTINE
 !
 SUBROUTINE FEED_APPLYLINE_DRNPACK(FEED,PAK,IPVL,NTOT,LABEL,CAUX,IPRT)
  ! ******************************************************************
  ! APPLY CURRENT LINE FEED TO PACKAGE ARRAY FOR PACKAGES THAT USE
  ! PAK_PROP_INTERFACE
  ! ******************************************************************
  !
  CLASS(LINE_FEED):: FEED
  CLASS(PAK_PROP ),DIMENSION(:):: PAK
  INTEGER,      INTENT(INOUT):: NTOT
  INTEGER,      INTENT(IN   ):: IPVL, IPRT
  CHARACTER(*), INTENT(IN   ):: LABEL
  CHARACTER(16),INTENT(IN   ), DIMENSION(:):: CAUX
  CONTIGUOUS:: PAK, CAUX
  ! LOCAL VARIABLES
  INTEGER:: I, J, K, IPROP, LDIM, NAUX, NCAUX, IOUT
  CHARACTER(40)::  FMT1,FMT2
  !
  IF (FEED%NFEED <= Z) RETURN
  !
  IOUT = FEED%IOUT
  IF(FEED%IPRT==Z .AND. IPRT>Z) FEED%IPRT = 1
  WRITE(IOUT,*)                                       !CARRAGE RETURN WHEN USING LINE FEED
  !
  LDIM  = FEED%LDIM                                      !THERE MUST BE AT LEAST ONE VALUE ALLOCATED OR PACKAGE WOULD NOT BE IN USE. THE CONTENTS DO NOT MATTER ONLY ITS SIZE
  NAUX  = SIZE(PAK(1)%AUX,1)
  NCAUX = SIZE(CAUX)
  !
  !Write a label for the list if the list will be printed.
  IF(FEED%IPRT == 1) THEN
     WRITE(IOUT,'(A/)') TRIM(FEED%PAK)//' LINEFEED STRESS PERIOD INFORMATION'
     CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
  END IF
  !
  DO I=1, FEED%NFEED
    IF(FEED%IPRT == 1) WRITE(IOUT,'(A,I2)')'FEED NUMBER ',I
    !
    DO J=1, FEED%FD(I)%NDAT
      IF( FEED%FD(I)%DAT(J) /= FEED%FD(I)%DAT(J) ) CYCLE          !SKIP PROCESSING NaN VALUES
      NTOT=NTOT+1
      IPROP=1
      PAK(NTOT)%LAY=FEED%FD(I)%NUMIDX(1,J)
      PAK(NTOT)%ROW=FEED%FD(I)%NUMIDX(2,J)
      PAK(NTOT)%COL=FEED%FD(I)%NUMIDX(3,J)
      DO K=1, LDIM
        IF (K==IPVL) THEN
            PAK(NTOT)%VAL(K) = FEED%FD(I)%DAT(J)
        ELSE
            PAK(NTOT)%VAL(K) = FEED%FD(I)%PROP(IPROP,J)
            IPROP=IPROP+1
        END IF
      END DO
      !
      IF (NAUX > Z ) PAK(NTOT)%AUX = FEED%FD(I)%AUX(:,J)
      !
      IF(FEED%IPRT == 1)THEN
        WRITE(FMT1,*)LDIM
        WRITE(FMT2,*)NAUX
        FMT1=ADJUSTL(FMT1);  FMT2=ADJUSTL(FMT2)
   FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8,'//TRIM(FMT2)//'I10)'
        FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8)'
        IF(NAUX > Z)THEN   
         WRITE(IOUT,FMT2) NTOT, PAK(NTOT)%LAY, PAK(NTOT)%ROW, PAK(NTOT)%COL, PAK(NTOT)%VAL, PAK(NTOT)%AUX(1:NAUX)
        ELSE
         WRITE(IOUT,FMT1) NTOT, PAK(NTOT)%LAY, PAK(NTOT)%ROW, PAK(NTOT)%COL, PAK(NTOT)%VAL
        END IF
      END IF
    END DO
  END DO
  !
  END SUBROUTINE
 !
 SUBROUTINE FEED_APPLYLINE_MNW2(FEED,MNW2,MNWNOD,CapTable,IPRT)
  ! ******************************************************************
  ! APPLY CURRENT LINE FEED TO PACKAGE ARRAY FOR PACKAGES THAT USE
  ! PAK_PROP_INTERFACE
  ! ******************************************************************
  !
  CLASS(LINE_FEED):: FEED
  DOUBLE PRECISION, DIMENSION(:,:),  INTENT(INOUT):: MNW2, MNWNOD
  DOUBLE PRECISION, DIMENSION(:,:,:),INTENT(INOUT):: CapTable
  INTEGER,                           INTENT(IN   ):: IPRT
  !CHARACTER(*),                      INTENT(IN   ):: LABEL
  CONTIGUOUS:: MNW2, MNWNOD, CapTable
  ! LOCAL VARIABLES
  DOUBLE PRECISION:: Qdes
  INTEGER:: I, J, MNWID, QLOC, IACT, IOUT
  INTEGER:: PUMPCAP, firstnode, lastnode, QLOCNOD, NNODES, NODNUM
  !
  IF (FEED%NFEED <= Z) RETURN
  !
  QLOC=5     !LOCATION OF Qdes
  IACT=1     !LOCATION OF FLAG FOR WELL IN USE
  NNODES=2   !LOCATION FOR NUMBER OF NODES
  NODNUM=4   !LOCATION OF STARTING NODE
  !
  QLOCNOD = 4
  !
  IOUT = FEED%IOUT
  IF(FEED%IPRT==Z .AND. IPRT>Z) FEED%IPRT = 1
  WRITE(IOUT,*)                                       !CARRAGE RETURN WHEN USING LINE FEED
  !
  !Write a label for the list if the list will be printed.
  IF(FEED%IPRT == 1) THEN
     WRITE(IOUT,'(A/)') TRIM(FEED%PAK)//' LINEFEED STRESS PERIOD INFORMATION'
     !CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
  END IF
  !
  DO I=1, FEED%NFEED
    IF(FEED%IPRT == 1) WRITE(IOUT,'(A,I2)')'FEED NUMBER ',I
    !
    DO J=1, FEED%FD(I)%NDAT
      !
      IF( FEED%FD(I)%DAT(J) == FEED%FD(I)%DAT(J) .AND. FEED%FD(I)%CHARIDX(J) > Z) THEN            ! If set to NaN then it will never equal itself. Also if it is not found then skip
          !
          MNWID = FEED%FD(I)%CHARIDX(J)
          !
          IF(MNW2(IACT,MNWID) > -0.5D0) THEN
              Qdes  = FEED%FD(I)%DAT(J)
              MNW2(IACT,MNWID) = UNO
          ELSE
              MNW2(QLOC,MNWID) = DZ
              CYCLE
          END IF
          !
          MNW2(QLOC,MNWID) = Qdes
          !
          firstnode=INT( MNW2(NODNUM,MNWID) )
          lastnode =INT( MNW2(NODNUM,MNWID) + ABS(MNW2(NNODES,MNWID)) - UNO )
          !
          MNWNOD(QLOCNOD,firstnode:lastnode)=DZ                                !seb ZERO OUT ALL NODES AND PUT DESIRED INTO FIRST NODE
          MNWNOD(QLOCNOD,firstnode)=Qdes                                       !seb CHANGED BACK TO FIRSTNODE, FIRST CHANGE WAS FROM MNWNOD(4,firstnode)=Qdes to MNWNOD(4,lastnode)=Qdes to prevent pumping from occuring in a dry cell
          !
          PUMPCAP=INT( MNW2(22,MNWID) )
          !
          if(PUMPCAP > Z) then
           if(Qdes > DZ) then                                        ! (Qdes.GE.0.d0) CHANGED TO (Qdes > 0.d0)
                MNW2(25,MNWID)=DZ                                     !Initialize CapFlag2
                MNW2(27,MNWID)=DZ
           else
             IF(Qdes == DZ)THEN                                       ! ALLOW HOLDING OF CAPMULT FOR Qdes=0 BUT DO NOT ENABLE CAPACITY CONTSTRAINTS TO FACILITATE LINK TO FMP
                MNW2(25,MNWID)=DZ
                MNW2(27,MNWID)=DZ                                     !Initialize CapFlag2
             ELSE
                MNW2(25,MNWID)=UNO
                if (CapTable(MNWID,PUMPCAP+2,2).LE.DZ) CapTable(MNWID,PUMPCAP+2,2)=abs(qdes)
             END IF
             MNW2(24,MNWID)=UNO  !CapMult
           end if
          end if
          IF(FEED%IPRT == 1)THEN
            !WRITE(FMT1,*)LDIM
            !WRITE(FMT2,*)NAUX
            !FMT1=ADJUSTL(FMT1);  FMT2=ADJUSTL(FMT2)
            !FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8,'//TRIM(FMT2)//'I10)'
            !FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8)'
            !IF(NAUX > 0)THEN   
            ! WRITE(IOUT,FMT2) NTOT, PAK(NTOT)%LAY, PAK(NTOT)%ROW, PAK(NTOT)%COL, PAK(NTOT)%VAL, PAK(NTOT)%AUX(1:NAUX)
            !ELSE
            ! WRITE(IOUT,FMT1) NTOT, PAK(NTOT)%LAY, PAK(NTOT)%ROW, PAK(NTOT)%COL, PAK(NTOT)%VAL
            !END IF
          END IF
      END IF
    END DO
  END DO
  !
 END SUBROUTINE
  !
 SUBROUTINE FEED_APPLYLINE_SFR(FEED,PAK,IPVL,LABEL)
  ! ******************************************************************
  ! APPLY CURRENT LINE FEED TO SFR2 PACKAGE
  ! ******************************************************************
  !
  CLASS(LINE_FEED):: FEED
  REAL,         INTENT(INOUT), DIMENSION(:,:):: PAK
  INTEGER,      INTENT(IN   ):: IPVL
  CHARACTER(*), INTENT(IN   ):: LABEL
  CONTIGUOUS:: PAK
  ! LOCAL VARIABLES
  INTEGER:: I, J, IOUT, ISEG, NSEG
  CHARACTER(16),DIMENSION(1):: CAUX
  !
  IF (FEED%NFEED <= Z) RETURN
  !
  NSEG = SIZE(PAK, 2) + 1  !Expects passed array to be correct dimension; +1 isfor < rather than <= with checks
  !
  IOUT = FEED%IOUT
  WRITE(IOUT,*)                                       !CARRAGE RETURN WHEN USING LINE FEED
  !
  !Write a label for the list if the list will be printed.
  IF(FEED%IPRT == 1) THEN
     WRITE(IOUT,'(A/)') TRIM(FEED%PAK)//' LINEFEED STRESS PERIOD INFORMATION'
     CALL ULSTLB(IOUT,LABEL,CAUX,1,Z)
  ELSE
     WRITE(IOUT,'(A/)') TRIM(FEED%PAK)//' LINEFEED STRESS PERIOD INFORMATION NOW BEING APPLIED'
  END IF
  !
  DO I=1, FEED%NFEED
    DO J=1, FEED%FD(I)%NDAT
      IF( FEED%FD(I)%DAT(J) /= FEED%FD(I)%DAT(J) ) FEED%FD(I)%DAT(J)=0.0D0          !NaN VALUES ARE SET TO ZERO
      !
      ISEG = FEED%FD(I)%NUMIDX(1,J)
      IF(0 < ISEG .AND. ISEG < NSEG) PAK(IPVL,ISEG) = 0.0
    END DO
  END DO
  !
  DO I=1, FEED%NFEED
    DO J=1, FEED%FD(I)%NDAT
      ISEG = FEED%FD(I)%NUMIDX(1,J)
      IF(0 < ISEG .AND. ISEG < NSEG) PAK(IPVL,ISEG) = PAK(IPVL,ISEG) + FEED%FD(I)%DAT(J)
    END DO
  END DO
  !
  IF(FEED%IPRT == 1) THEN
     DO I=1, FEED%NFEED
       WRITE(IOUT,'(A,I2)')'FEED NUMBER ',I
       DO J=1, FEED%FD(I)%NDAT
         !
         ISEG = FEED%FD(I)%NUMIDX(1,J)
         IF(0 < ISEG .AND. ISEG < NSEG) THEN
           WRITE(IOUT,'(1X,I7,1x,G16.8)') ISEG, PAK(IPVL,ISEG)
         ELSE
           WRITE(IOUT,'(1X,I7,1x,G16.8,2x,A)') ISEG, PAK(IPVL,ISEG), '<-- WARNING BAD SEGMENT NUMBER, SKIPPING THIS ASSIGNMENT'
         END IF
       END DO
     END DO
  END IF
  !
 END SUBROUTINE
  !
 SUBROUTINE FINAL_DEALLOCATE_LINEFEED(FEED)
  TYPE(LINE_FEED), INTENT(INOUT):: FEED
  !
  INTEGER:: I, LDIM
  LOGICAL:: PROP,AUX
  !
  IF(FEED%NFEED.LE.Z) RETURN
  !
  LDIM=FEED%LDIM
  PROP = FEED%NPROP > Z
  AUX  = SIZE(FEED%FD(1)%AUX, 1) > Z        !AT A MINIMUM THERE IS ONE FEED. THIS RETURNS TRUE IF THE FIRST DIMENSION OF AUX IS GREATER THAN ZERO (ie ITS ALLOCATED)
  !
  DO I=1, FEED%NFEED
    IF (LDIM == Z) THEN
        DEALLOCATE(FEED%FD(I)%CHARID)
        IF (ALLOCATED(FEED%FD(I)%CHARIDX))  DEALLOCATE(FEED%FD(I)%CHARIDX)
    ELSE
        DEALLOCATE(FEED%FD(I)%NUMIDX)
    END IF
    !
    IF(ALLOCATED(FEED%FD(I)%DAT) ) DEALLOCATE(FEED%FD(I)%DAT )
    IF(ALLOCATED(FEED%FD(I)%IDAT)) DEALLOCATE(FEED%FD(I)%IDAT)
    !
    !IF(ALLOCATED(FEED%FD(I)%BIN))  DEALLOCATE(FEED%FD(I)%BIN)  -- gfortran does not like this statement -- does not matter cause following deallocate of FD should trigger autocleanup
    !
    IF(PROP)  DEALLOCATE(FEED%FD(I)%PROP)
    IF(AUX )  DEALLOCATE(FEED%FD(I)%AUX)
  END DO
  !
  DEALLOCATE( FEED%FD )
  !
  DEALLOCATE(FEED%IOUT,    FEED%LDIM,    FEED%NPROP,   FEED%PAK)
  DEALLOCATE(FEED%XYCOORD, FEED%XYZCOORD)
  IF(ALLOCATED(FEED%CAUX)) DEALLOCATE(FEED%CAUX)
  !
  !
  END SUBROUTINE
  !
  !!!PURE SUBROUTINE LINEFEED_DEALLOCATE(FL)
  !!!  TYPE(LINE_FEED),ALLOCATABLE,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  CALL FINAL_DEALLOCATE_LINEFEED(FL)
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!PURE SUBROUTINE LINEFEED_DEPOINT(FL)
  !!!  TYPE(LINE_FEED),POINTER,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  CALL FINAL_DEALLOCATE_LINEFEED(FL)
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !
  END MODULE  