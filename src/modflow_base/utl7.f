      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
C     ******************************************************************
C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
C     CONVERT THE WORD TO A NUMBER.
C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
C          ENDING CHARACTER POSITIONS OF THE WORD.
C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
C          A QUOTE CHARACTER.
C        IF NCODE IS 0, THE WORD IS UNMODIFIED AND RETURNED.          seb REQUIRED FOR PROPER FUNCTION FOR OPEN CLOSE OF LINUX FILES [CASE SENSITIVE]
C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
C          POSITIVE; No Long Don ==>ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0
C          PROGRAM IS NOT STOPPED FOR IOUT=0.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,         ONLY:LOUT=>IOUT
      USE UTIL_INTERFACE, ONLY: STOP_ERROR
      CHARACTER(*) :: LINE
      CHARACTER(20):: STRING
      CHARACTER(30):: RW
      CHARACTER(1) :: TAB
      !CHARACTER(:),ALLOCATABLE:: FNAME                                  !seb ADDED FILE NAME TO URWORD 
      CHARACTER:: OLD_ENDING
C     ------------------------------------------------------------------
      TAB=CHAR(9)
C
C1------Set last char in LINE to blank and set ISTART and ISTOP to point
C1------to this blank as a default situation when no word is found.  If
C1------starting location in LINE is out of bounds, do not look for a
C1------word.
      LINLEN=LEN(LINE)
      !
      IF(LINE(LINLEN:LINLEN) .NE. '#') LINE(LINLEN:LINLEN)=' '
      !
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
C
C2------Find start of word, which is indicated by first character that
C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.','
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
C
C3------Found start of word.  Look for end.
C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ."'") THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ."'") GO TO 40
25          CONTINUE
         END IF
C
C3A-----When word is double quoted, only a double quote can terminate it. seb
      ELSEIF(LINE(I:I).EQ.'"') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 26 J=I,LINLEN
            IF(LINE(J:J).EQ.'"') GO TO 40
26          CONTINUE
         END IF
C
C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.','
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
C
C3C-----End of line without finding end of word; set end of word to
C3C-----end of line.
      J=LINLEN+1
C
C4------Found end of word; set J to point to last character in WORD and
C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
C
C4.5------WORD COLLECTED RETURN WITHOUT CONVERTING TO UPPER CASE if NCODE is 0.  seb
      IF(NCODE.EQ.0) THEN
         RETURN
      END IF
C
C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')
     1             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
C
C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
         IF(RW.EQ.' ' ) GO TO 200                                       !seb ADDED CHECK FOR WHEN THERE IS A FAILED READ. NOTE SOME COMPILERS TO NOT FLAG A READ OF AN EMPTY LINE AS AN ERROR
      END IF
      RETURN  !NCODE>3 MEANS DO THE SAME AS 0
C
C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
C
C7A-----If output unit is zero, set last character of string to 'E'.  --CHANGED FROM NEGATIVE
      IF(IOUT.EQ.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
C
C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.NE.0) THEN
          RW = ADJUSTL(RW)
          CALL STOP_ERROR(LINE,IN,IOUT,MSG=
     +    'URWORD FAILED TO LOAD "'//TRIM(RW)//
     +    '" AS '//STRING(1:L))
!           IF(IN.NE.0) THEN
!             ALLOCATE(CHARACTER(700)::FNAME)
!             INQUIRE(IN,NAME=FNAME)
!      WRITE(LOUT,201) IN,TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),
!     +                                                        TRIM(LINE)!seb ADDED FNAME
!      WRITE(*,   201) IN,TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),
!     +                                                        TRIM(LINE)!seb ADDED FNAME
!           ELSE
!             WRITE(LOUT,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
!             WRITE(*,   202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
!           END IF
!201      FORMAT(1X,/1X,'FILE UNIT ',I4,' WITH FILE NAME ',A,
!     +    ' : ERROR CONVERTING " ',A,                                    !seb ADDED FILE NAME TO ERROR REPORTING
!     +       ' " TO ',A,' IN LINE:',/,'" ',A,' "')
!202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,
!     1       '" TO ',A,' IN LINE:',/1X,A)
!C
!C7C-----If output unit is 0; write a message to default output.
!      ELSE
!         IF(IN.NE.0) THEN
!          ALLOCATE(CHARACTER(700)::FNAME)
!          INQUIRE(IN,NAME=FNAME)
!      WRITE(*,201) IN,TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),
!     +                                                     TRIM(LINE)   !seb ADDED FNAME
!         ELSE
!            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
!         END IF
      END IF
C
C7D-----STOP after writing message.
      CALL USTOP(' ')
      END
      SUBROUTINE UPCASE(WORD)
C     ******************************************************************
C     CONVERT A CHARACTER STRING TO ALL UPPER CASE
C     ******************************************************************
C       SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER WORD*(*)
C
C1------Compute the difference between lowercase and uppercase.
      L = LEN_TRIM(WORD)                                                !seb CHANGED FROM LEN TO LEN_TRIM
      IDIFF=ICHAR('a')-ICHAR('A')
C
C2------Loop through the string and convert any lowercase characters.
      DO 10 K=1,L
      IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z')
     1   WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
10    CONTINUE
C
C3------return.
      RETURN
      END
      SUBROUTINE URDCOM(IN,IOUT,LINE)
      USE CONSTANTS,      ONLY: NL
      USE UTIL_INTERFACE, ONLY: SPECIAL_BLANK_STRIP, FILE_IO_ERROR
C     ******************************************************************
C     READ COMMENTS FROM A FILE AND PRINT THEM.  RETURN THE FIRST LINE
C     THAT IS NOT A COMMENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
      CHARACTER(:),ALLOCATABLE::FNAME                                   !seb
C     ------------------------------------------------------------------
C
C1------Read a line
   10 READ(IN,'(A)',IOSTAT=L) LINE
      !
      IF(L>0) THEN                                                      !seb
         CALL FILE_IO_ERROR(L,IN,INFILE=IN,OUTPUT=IOUT,
     +   MSG='URDCOM FAILED TO READ LINE FROM FILE.'//NL//
     +       'THIS MAYBE THE ERROR LINE OR THE PRECEDING LINE: '//NL//
     +       TRIM(LINE))
      ELSEIF(L<0)THEN                                                   !END OF FILE RETURN BLANK LINE
          LINE=''
          RETURN
      END IF
C
C2------If the line does not start with "#", return.
      IF(LINE(1:1).NE.'#') THEN
          CALL SPECIAL_BLANK_STRIP(LINE)
          RETURN
      END IF
C
C3------Find the last non-blank character.
      L=LEN(LINE)
      DO 20 I=L,1,-1
      IF(LINE(I:I).NE.' ') GO TO 30
   20 CONTINUE
C
C4------Print the line up to the last non-blank character if IOUT.NE.0.
   30 IF (IOUT.NE.0) THEN
        WRITE(IOUT,'(1X,A)') LINE(1:I)
      ENDIF
      GO TO 10
C
      END
      SUBROUTINE ULSTRD(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,IOUT,
     1     LABEL,CAUX,NCAUX,NAUX,IFREFM,NCOL,NROW,NLAY,ISCLOC1,ISCLOC2,
     2     IPRFLG,GROUP)
C     ******************************************************************
C     Read and print a list.  NAUX of the values in the list are
C     optional -- auxiliary data.
C     ******************************************************************
      USE UTIL_INTERFACE,         ONLY: READ_TO_DATA, FILE_IO_ERROR,
     +                                  STOP_ERROR
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE OPENSPEC
      CHARACTER(20),DIMENSION(*):: GROUP  !IS EITHER GROUP(1) OR GROUP(MXLIST)
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      DIMENSION RLIST(LDIM,MXLIST)
      CHARACTER*700 LINE,FNAME
      !CHARACTER*20 FMTARG, ACCARG
      CHARACTER*20 FILEFMT
      LOGICAL LVAL
      !!!DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1------If the list is empty, return.
      IF (NLIST.EQ.0) RETURN
C
C2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      IBINARY=0
      CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=.TRUE.) !seb originally => READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
C          TEST IF EXTERNAL FILE IS OPEN
         INQUIRE( UNIT=IN, OPENED=LVAL )
         IF ( LVAL.EQV. .FALSE. ) THEN
             BLOCK
               CHARACTER(64):: CERR
               WRITE ( CERR,110 ) IN
  110  FORMAT('ULSTRD: EXTERNAL Unit ', I4,' is not open in NAME file.')
               !WRITE ( IOUT,'(1X,A)' ) CERR
               !CALL USTOP(CERR)
               CALL STOP_ERROR(LINE,IN,IOUT,MSG=CERR)
           END BLOCK
         END IF
C          TEST IF OPEN EXTERNAL FILE IS FORMATTED OR UNFORMATTED/BINARY
C          SEE FORM VARIABLE IN openspec.inc
         !FMTARG=FORM
         INQUIRE( UNIT=IN, FORM=FILEFMT )
         IF ( FILEFMT.EQ.FORM ) THEN
           IBINARY=1
         END IF
         IF(IPRFLG.EQ.1) THEN
           IF ( IBINARY.NE.1 ) THEN
             WRITE(IOUT,111) IN
  111        FORMAT(1X,'Reading list on unit ',I4)
           ELSE
             WRITE(IOUT,1111) IN
 1111        FORMAT(1X,'Reading list on binary unit ',I4)
           END IF
         END IF
        IF (IBINARY.NE.1) CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=.TRUE.) !seb originally => READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         !!!IN=NUNOPN
!C          TEST IF OPEN\CLOSE FILE EXISTS
!         INQUIRE( FILE=FNAME, EXIST=LVAL )
!         IF ( LVAL.EQV. .FALSE. ) THEN
!           WRITE ( IOUT,112 ) LINE(ISTART:ISTOP)
!  112      FORMAT('Specified OPEN/CLOSE file ',(A),' does not exist')
!           CALL USTOP('Specified OPEN/CLOSE file does not exist')
!         END IF
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IN = 0
         IF (LINE(ISTART:ISTOP).EQ.'(BINARY)') THEN
           IBINARY=1
           !FMTARG=FORM
           !ACCARG=ACCESS
           CALL GENERIC_OPEN(FNAME, IN, IOUT, ACTION(1), FORM,
     +                       ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384,  !Buffer = 16kB*2
     +                       LINE=LINE, INFILE=INPACK)
!           OPEN(NEWUNIT=IN,FILE=FNAME,ACTION=ACTION(1),FORM=FMTARG,
!     1          ACCESS=ACCARG,STATUS='OLD')
           IF(IPRFLG.EQ.1)WRITE(IOUT,1115) IN,FNAME
 1115      FORMAT(1X,/1X,'OPENING BINARY FILE ON UNIT ',I4,':',/1X,A)
         ELSE
           CALL GENERIC_OPEN(FNAME, IN, IOUT, ACTION(1), 'FORMATTED',
     +                       ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384,  !Buffer = 16kB*2
     +                       LINE=LINE, INFILE=INPACK)
         !OPEN(NEWUNIT=IN,FILE=FNAME,ACTION=ACTION(1))
           IF(IPRFLG.EQ.1) WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         END IF
         ICLOSE=1
        IF (IBINARY.NE.1) CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=.TRUE.) !seb originally => READ(IN,'(A)') LINE
      END IF
C
C3------Check for SFAC record in ascii records.
      IF (IBINARY.NE.1) THEN
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
          IF(IPRFLG.EQ.1) THEN
              WRITE(IOUT,116) SFAC
  116       FORMAT(1X,'LIST SCALING FACTOR=',ES12.5)
            IF(ISCLOC1.EQ.ISCLOC2) THEN
                 WRITE(IOUT,113) ISCLOC1
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
            ELSE
                 WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114          FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',
     1            I2,'-',I2,')')
            END IF
          ENDIF
          CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=.TRUE.) !seb originally => READ(IN,'(A)') LINE
        END IF
      END IF
C
C3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
           WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
      END IF
C
C4------Setup indices for reading the list
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
C
C4A-----READ THE LIST -- BINARY OR ASCII
      IF (IBINARY.NE.0) THEN
        READ(IN) ((RLIST(JJ,II),JJ=1,NREAD2),II=LSTBEG,N)
      ELSE
C
C5------CHECK FOR AUXILIARY VARIABLE "AUXSFAC" AND STORE LOCATION 
C5------READ AN ASCII LIST
        JAUX = 0
        DO 230 JJ=1,NAUX
          IF(CAUX(JJ) .EQ. "AUXSFAC") THEN
            JAUX=JJ+NREAD1
            EXIT
          END IF
230     CONTINUE
C          
        DO 240 II=LSTBEG,N
C
C5A-----Read a line into the buffer.  (The first line has already been
C5A-----read to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) CALL READ_TO_DATA(LINE,IN,IOUT,NOSHIFT=.TRUE.) !seb originally => READ(IN,'(A)') LINE
C
C5B-----Get the non-optional values from the line.
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(3I10,9F10.0)',IOSTAT=IE) K,I,J,
     +                                        (RLIST(JJ,II),JJ=4,NREAD1)
         IF(IE.NE.0) CALL FILE_IO_ERROR(IE,IN,LINE=LINE,INFILE=IN,
     +         OUTPUT=IOUT,MSG=
     +        'ULSTRD: FIX FORMATTED READ FAILED TO READ LINE')
         LLOC=10*NREAD1+1
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
         DO 200 JJ=4,NREAD1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),
     +                IOUT,IN)
200      CONTINUE
      END IF
      RLIST(1,II)=K
      RLIST(2,II)=I
      RLIST(3,II)=J
       !
       IF(GROUP(1).NE.'NOGROUP') THEN
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
           GROUP(II) = LINE(ISTART:ISTOP)
       END IF
       !
C
C5E-----Get the optional values from the line
      IF(NAUX.GT.0) THEN
         DO 210 JJ=NREAD1+1,NREAD2
           RLIST(JJ,II)=0.                                              !seb ADDED 0 TO PREVENT READ FAILURE WHEN NO AUX IS PROVIDED
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),
     1                 0,IN)
210      CONTINUE
      END IF
C
240     CONTINUE
      ENDIF 
C
C6----SCALE THE DATA AND CHECK 
      DO 250 II=LSTBEG,N
      K=RLIST(1,II)
      I=RLIST(2,II)
      J=RLIST(3,II)
C
C6A------Scale fields ISCLOC1-ISCLOC2 by SFAC and AUXSFAC (if present)
      DO 204 ILOC=ISCLOC1,ISCLOC2
        RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
        IF (JAUX .NE. 0) RLIST(ILOC,II)=RLIST(ILOC,II)*RLIST(JAUX,II)
204   CONTINUE
C
C6C-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IPRFLG.EQ.1)
     1      WRITE(IOUT,205) NN,K,I,J,(RLIST(JJ,II),JJ=4,NREAD2)
205   FORMAT(1X,I6,I7,I7,I7,26G16.4)
C
C6D-----Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         !CALL USTOP(' Layer number in list is outside of the grid')
         CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +        'ULSTRD: Layer number in list is outside of the grid')
      END IF
      IF(I.LT.1 .OR. I.GT.NROW) THEN
         !CALL USTOP(' Row number in list is outside of the grid')
         CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +        'ULSTRD: Row number in list is outside of the grid')
      END IF
      IF(J.LT.1 .OR. J.GT.NCOL) THEN
         !CALL USTOP(' Column number in list is outside of the grid')
         CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=
     +        'ULSTRD: Column number in list is outside of the grid')
      END IF
  250 CONTINUE
C
C7------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
      RETURN
      END
      SUBROUTINE ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
C     ******************************************************************
C     PRINT A LABEL FOR A LIST
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      CHARACTER(LEN(LABEL)+9+NAUX*16):: BUF,DASH                        !seb CHANGED FROM *400 THIS CAUSED INDEX FAILURES WHEN LABEL WAS GREATER THAN 400
!      CHARACTER*1 DASH(NBUF)                                            !REMOVED ARRAY
!      DATA DASH/400*'-'/
C     ------------------------------------------------------------------
C
C1------Construct the complete label in BUF.  Start with BUF=LABEL.
      BUF=LABEL
C
C2------Add auxiliary data names if there are any.
      NBUF=LEN(LABEL)+9
      IF(NAUX.GT.0) THEN
         DO 10 I=1,NAUX
         N1=NBUF+1
         NBUF=NBUF+16
         BUF(N1:NBUF)=CAUX(I)
10       CONTINUE
      END IF
      DASH=REPEAT('-',NBUF)
C
C3------Write the label.
        WRITE(IOUT,103) BUF(1:NBUF)
  103 FORMAT(1X,A)
C
C4------Add a line of dashes.
        WRITE(IOUT,104) DASH !(DASH(J),J=1,NBUF)
  104 FORMAT(1X,A)
C
C5------Return.
      RETURN
      END
      !
      SUBROUTINE BACKSPACE_STOP_MSG(IN, IOUT, MSG)
      USE UTIL_INTERFACE,  ONLY: STOP_ERROR
      INTEGER,      INTENT(IN):: IN, IOUT
      CHARACTER(*), INTENT(IN):: MSG
      CHARACTER(700):: LINE
      !
      BACKSPACE(IN)
      READ(IN, '(A)') LINE
      !
      CALL STOP_ERROR(LINE,IN,IOUT,MSG)
      !
      END SUBROUTINE
      !
      SUBROUTINE BACKSPACE_IO_STOP_MSG(IN, IOUT, IERR, MSG)
      USE UTIL_INTERFACE,  ONLY: FILE_IO_ERROR
      INTEGER,      INTENT(IN):: IN, IOUT, IERR
      CHARACTER(*), INTENT(IN):: MSG
      INTEGER:: I
      CHARACTER(700):: LINE
      !
      BACKSPACE(IN)
      READ(IN, '(A)', IOSTAT=I) LINE
      !
      IF(I == 0) THEN;
          CALL FILE_IO_ERROR(IERR,IN,LINE=LINE,INFILE=IN,OUTPUT=IOUT,
     +                   MSG=MSG)
      ELSE
          CALL FILE_IO_ERROR(IERR,IN,INFILE=IN,OUTPUT=IOUT,MSG=MSG)
      END IF
      !
      END SUBROUTINE
      !
      SUBROUTINE U1DREL(A,ANAME,JJ,IN,IOUT)
C     ******************************************************************
C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       JJ IS NO. OF ELEMENTS
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,              ONLY: NL, BLN, TRUE
      USE UTIL_INTERFACE,         ONLY: READ_TO_DATA, 
     +                                  STOP_ERROR
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE OPENSPEC
      CHARACTER*24 ANAME
      DIMENSION A(JJ)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*700 FNAME
      !DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      CALL READ_TO_DATA(CNTRL,IN,IOUT,NOSHIFT=TRUE) !seb originally => READ(IN,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         !LOCAT=NUNOPN
         !OPEN(NEWUNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1),IOSTAT=IE)
         LOCAT = 0
         CALL GENERIC_OPEN(FNAME, LOCAT, IOUT, ACTION(1), 'FORMATTED',
     +                     ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384,  !Buffer = 16kB*2
     +                     LINE=CNTRL, INFILE=INPACK)
           WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         !
!         IF(IE.NE.0)  CALL FILE_IO_ERROR(IE,FNAME=FNAME,LINE=CNTRL,
!     +                                   INFILE=IN,OUTPUT=IOUT)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,IOSTAT=IE) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
         !
         IF(IE.NE.0) CALL BACKSPACE_STOP_MSG(IN, IOUT, 
     +              'U1DREL - ERROR READING FOR "'//ANAME//'"'//
     +              'THE ARRAY CONTROL RECORD "'//CNTRL//'"')
        WRITE(IOUT,'(1X,A)') CNTRL
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.NE.0) GO TO 90
C
C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 J=1,JJ
   80 A(J)=CNSTNT
        WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
        WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/
     1       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      CALL READ_TO_DATA(CNTRL,LOCAT,IOUT,NOSHIFT=TRUE,BACK_UP=TRUE)
      IF(FMTIN.EQ.'(FREE)') THEN
         READ(LOCAT,*,IOSTAT=IE) A(1:JJ)  !(A(J),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN,IOSTAT=IE) A(1:JJ)  !(A(J),J=1,JJ)
      END IF
      !
      IF(IE.NE.0) CALL BACKSPACE_IO_STOP_MSG(LOCAT, IOUT, IE, 
     +     'U1DREL - ERROR READING ON CURRENT INPUT LINE A SET OF '//
     +     'NUMBERS FOR "'//TRIM(ADJUSTL(ANAME))//'"'//BLN//
     +     '**NOTE ERROR LINE IS THE BEST GUESS FOR ERROR LOCATION.')
      !
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
C
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 A(J)=A(J)*CNSTNT
C
C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
           WRITE(IOUT,1001) (A(J),J=1,JJ)
1001     FORMAT((1X,ES12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
           WRITE(IOUT,1002) (A(J),J=1,JJ)
1002     FORMAT((1X,ES12.5,4(1X,G12.5)))
      END IF
C
C7------RETURN
      RETURN
C
C8------CONTROL RECORD ERROR.
500   WRITE(FNAME,502) ANAME,CNTRL
502   FORMAT(/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':',/1x A)
          !CALL USTOP(' ')  
          CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=FNAME)
      END
      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)
C     ******************************************************************
C     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
C       IA IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF IA
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,              ONLY: NL, BLN, TRUE
      USE UTIL_INTERFACE,         ONLY: READ_TO_DATA,
     +                                  STOP_ERROR
      USE NUM2STR_INTERFACE,      ONLY: NUM2STR
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE OPENSPEC
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*700 FNAME
      !DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
      LOGICAL:: FORMATTED
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      CALL READ_TO_DATA(CNTRL,IN,IOUT,NOSHIFT=TRUE) !seb originally => READ(IN,'(A)') CNTRL
      FORMATTED=TRUE
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=-1 !NUNOPN
         IF(IOUT.NE.0) THEN                             !seb Added IOUT.NE.0
           WRITE(IOUT,15) FNAME
         ENDIF
   15    FORMAT(1X,/1X,'OPENING FILE :',1X,A)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,IOSTAT=IE) LOCAT,ICONST,FMTIN,IPRN
    1    FORMAT(I10,I10,A20,I10)
         !
         IF(IE.NE.0) THEN
           IF(K.GT.0) THEN
             CALL BACKSPACE_STOP_MSG(IN, IOUT, 
     +              'U2DINT - ERROR READING FOR "'//ANAME//'"'//
     +              'THE ARRAY CONTROL RECORD "'//
     +               CNTRL//'" FOR LAYER '//NUM2STR(K))
           ELSE
             CALL BACKSPACE_STOP_MSG(IN, IOUT, 
     +              'U2DINT - ERROR READING FOR "'//ANAME//'"'//
     +              'THE ARRAY CONTROL RECORD "'//
     +               CNTRL//'"')
           END IF
         END IF
         !
         WRITE(IOUT,'(1X,A)') CNTRL
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
             LOCAT = 0
             IF(FMTIN.EQ.'(BINARY)') THEN
                  FORMATTED=.FALSE.
!                  OPEN(NEWUNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,
!     &                 ACTION=ACTION(1),IOSTAT=IE)
                  CALL GENERIC_OPEN(FNAME, LOCAT, IOUT, ACTION(1), FORM,
     +                           ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384, !Buffer = 16kB*2
     +                           LINE=CNTRL, INFILE=IN)
             ELSE
               !OPEN(NEWUNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1),IOSTAT=IE)
               CALL GENERIC_OPEN(FNAME,LOCAT,IOUT,ACTION(1),'FORMATTED',
     +                     ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384,  !Buffer = 16kB*2
     +                     LINE=CNTRL, INFILE=IN)
             END IF
!          IF(IE.NE.0) CALL FILE_IO_ERROR(IE,FNAME=FNAME,LINE=CNTRL,
!     +                                    INFILE=IN,OUTPUT=IOUT)
            END IF
            IF(LOCAT.NE.0 .AND. FMTIN.EQ.'(BINARY)') FORMATTED=.FALSE. !LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   IA(J,I)=ICONST
        IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
          IF(K.GT.0) WRITE(IOUT,82) ANAME,ICONST,K
   82   FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
          IF(K.LE.0) WRITE(IOUT,83) ANAME,ICONST
        ENDIF
   83   FORMAT(1X,/1X,A,' =',I15)
        RETURN
      ELSE IF(LOCAT.NE.0 .AND. FORMATTED) THEN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
             WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
           ENDIF
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
             WRITE(IOUT,95) ANAME,LOCAT,FMTIN
           ENDIF
   95      FORMAT(1X,///11X,A,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
             WRITE(IOUT,96) ANAME,LOCAT,FMTIN
           ENDIF
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        CALL READ_TO_DATA(CNTRL,LOCAT,IOUT,NOSHIFT=TRUE,BACK_UP=TRUE)
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,    *,IOSTAT=IE) IA(1:JJ,I) !(IA(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN,IOSTAT=IE) IA(1:JJ,I) !(IA(J,I),J=1,JJ)
        END IF
           !
           IF(IE.NE.0) CALL BACKSPACE_IO_STOP_MSG(LOCAT, IOUT, IE,
     +     'U2DINT - ERROR READING ON CURRENT INPUT LINE A SET OF '//
     +     'INTEGERS FOR "'//TRIM(ADJUSTL(ANAME))//'"'//BLN//
     +     '**NOTE ERROR LINE IS THE BEST GUESS FOR ERROR LOCATION.')
  100   CONTINUE
      ELSE
C
C4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
        !LOCAT=-LOCAT
        IF(K.GT.0) THEN
           IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
             WRITE(IOUT,201) ANAME,K,LOCAT
           ENDIF
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
             WRITE(IOUT,202) ANAME,LOCAT
           ENDIF
  202      FORMAT(1X,///11X,A,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           IF(IOUT.NE.0) THEN                              !seb Added IOUT.NE.0
             WRITE(IOUT,203) ANAME,LOCAT
           ENDIF
  203      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT)
        READ(LOCAT) IA
      END IF
C
C5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      IF(ICONST.EQ.0) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      IA(J,I)=IA(J,I)*ICONST
  310 CONTINUE
C
C6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  320 IF(IPRN.LT.0 .OR. IOUT.EQ.0) RETURN                                  !seb ADDED IOUT.EQ.0
C
C7------PRINT COLUMN NUMBERS AT TOP OF PAGE.
      IF(IPRN.GT.9 .OR. IPRN.EQ.0) IPRN=6
      GO TO(401,402,403,404,405,406,407,408,409), IPRN
401   CALL UCOLNO(1,JJ,4,60,2,IOUT)
      GO TO 500
402   CALL UCOLNO(1,JJ,4,40,3,IOUT)
      GO TO 500
403   CALL UCOLNO(1,JJ,4,30,4,IOUT)
      GO TO 500
404   CALL UCOLNO(1,JJ,4,25,5,IOUT)
      GO TO 500
405   CALL UCOLNO(1,JJ,4,20,6,IOUT)
      GO TO 500
406   CALL UCOLNO(1,JJ,4,10,12,IOUT)
      GO TO 500
407   CALL UCOLNO(1,JJ,4,25,3,IOUT)
      GO TO 500
408   CALL UCOLNO(1,JJ,4,15,5,IOUT)
      GO TO 500
409   CALL UCOLNO(1,JJ,4,10,7,IOUT)
C
C8------PRINT EACH ROW IN THE ARRAY.
500   DO 510 I=1,II
      GO TO(501,502,503,504,505,506,507,508,509), IPRN
C
C----------------FORMAT 60I1
  501 WRITE(IOUT,551) I,(IA(J,I),J=1,JJ)
  551 FORMAT(1X,I3,1X,60(1X,I1):/(5X,60(1X,I1)))
      GO TO 510
C
C----------------FORMAT 40I2
  502 WRITE(IOUT,552) I,(IA(J,I),J=1,JJ)
  552 FORMAT(1X,I3,1X,40(1X,I2):/(5X,40(1X,I2)))
      GO TO 510
C
C----------------FORMAT 30I3
  503 WRITE(IOUT,553) I,(IA(J,I),J=1,JJ)
  553 FORMAT(1X,I3,1X,30(1X,I3):/(5X,30(1X,I3)))
      GO TO 510
C
C----------------FORMAT 25I4
  504 WRITE(IOUT,554) I,(IA(J,I),J=1,JJ)
  554 FORMAT(1X,I3,1X,25(1X,I4):/(5X,25(1X,I4)))
      GO TO 510
C
C----------------FORMAT 20I5
  505 WRITE(IOUT,555) I,(IA(J,I),J=1,JJ)
  555 FORMAT(1X,I3,1X,20(1X,I5):/(5X,20(1X,I5)))
      GO TO 510
C
C----------------FORMAT 10I11
  506 WRITE(IOUT,556) I,(IA(J,I),J=1,JJ)
  556 FORMAT(1X,I3,1X,10(1X,I11):/(5X,10(1X,I11)))
      GO TO 510
C
C----------------FORMAT 25I2
  507 WRITE(IOUT,557) I,(IA(J,I),J=1,JJ)
  557 FORMAT(1X,I3,1X,25(1X,I2):/(5X,25(1X,I2)))
      GO TO 510
C
C----------------FORMAT 15I4
  508 WRITE(IOUT,558) I,(IA(J,I),J=1,JJ)
  558 FORMAT(1X,I3,1X,15(1X,I4):/(5X,10(1X,I4)))
      GO TO 510
C
C----------------FORMAT 10I6
  509 WRITE(IOUT,559) I,(IA(J,I),J=1,JJ)
  559 FORMAT(1X,I3,1X,10(1X,I6):/(5X,10(1X,I6)))
C
  510 CONTINUE
C
C9------RETURN
      RETURN
C
C10-----CONTROL RECORD ERROR.
  600 IF(K.GT.0) THEN
         WRITE(FNAME,601) ANAME,K,CNTRL
  601    FORMAT(/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,
     1     ' FOR LAYER',I4,':',/1x A)
      ELSE
         WRITE(FNAME,602) ANAME,CNTRL
  602  FORMAT(/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':',/1x A)
      END IF
          !CALL USTOP(' ')
          CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=FNAME)
      END
      SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)
C     ******************************************************************
C     ROUTINE TO INPUT 2-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,              ONLY: NL, BLN, inf_I, TRUE
      USE UTIL_INTERFACE,         ONLY: READ_TO_DATA,
     +                                  STOP_ERROR
      USE NUM2STR_INTERFACE,      ONLY: NUM2STR
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE OPENSPEC
      CHARACTER*24 ANAME
      DIMENSION A(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*700 FNAME
      !DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
      LOGICAL:: FORMATTED
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      CALL READ_TO_DATA(CNTRL,IN,IOUT,NOSHIFT=TRUE) !seb originally => READ(IN,'(A)') CNTRL
      FORMATTED=TRUE
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=inf_I
           WRITE(IOUT,15) FNAME
   15    FORMAT(1X,/1X,'OPENING FILE:',/1X,A)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,IOSTAT=IE) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
         !
         IF(IE.NE.0) THEN
           IF(K.GT.0) THEN
             CALL BACKSPACE_STOP_MSG(IN, IOUT, 
     +              'U2DREL - ERROR READING FOR "'//ANAME//'"'//
     +              'THE ARRAY CONTROL RECORD "'//
     +               CNTRL//'" FOR LAYER '//NUM2STR(K))
           ELSE
             CALL BACKSPACE_STOP_MSG(IN, IOUT, 
     +              'U2DREL - ERROR READING FOR "'//ANAME//'"'//
     +              'THE ARRAY CONTROL RECORD "'//
     +               CNTRL//'"')
           END IF
         END IF
         !
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(LOCAT == inf_I) LOCAT=0
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  FORMATTED=.FALSE. 
!                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,
!     &                 ACTION=ACTION(1),IOSTAT=IE)
                  IF(LOCAT == inf_I) LOCAT=0
                  CALL GENERIC_OPEN(FNAME, LOCAT, IOUT, ACTION(1), FORM,
     +                           ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384, !Buffer = 16kB*2
     +                           LINE=CNTRL, INFILE=IN)
               ELSE
                !OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1),IOSTAT=IE)
               CALL GENERIC_OPEN(FNAME,LOCAT,IOUT,ACTION(1),'FORMATTED',
     +                           ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384, !Buffer = 16kB*2
     +                           LINE=CNTRL, INFILE=IN)
               END IF
!           IF(IE.NE.0) CALL FILE_IO_ERROR(IE,FNAME=FNAME,LINE=CNTRL,
!     +                                     INFILE=IN,OUTPUT=IOUT)
            END IF
            IF(LOCAT.NE.0 .AND. FMTIN.EQ.'(BINARY)') FORMATTED=.FALSE. !LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   A(J,I)=CNSTNT
          IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
          IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3   FORMAT(1X,/1X,A,' =',1P,G14.6)
        RETURN
      ELSE IF(LOCAT.NE.0 .AND. FORMATTED) THEN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
             WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
             WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
             WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        CALL READ_TO_DATA(CNTRL,LOCAT,IOUT,NOSHIFT=TRUE,BACK_UP=TRUE)
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*,IOSTAT=IE) A(1:JJ,I)  !(A(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN,IOSTAT=IE) A(1:JJ,I)  !(A(J,I),J=1,JJ)
        END IF
           !
           IF(IE.NE.0) CALL BACKSPACE_IO_STOP_MSG(LOCAT, IOUT, IE,
     +     'U2DREL - ERROR READING ON CURRENT INPUT LINE A SET OF '//
     +     'NUMBERS FOR "'//TRIM(ADJUSTL(ANAME))//'"'//BLN//
     +     '**NOTE ERROR LINE IS THE BEST GUESS FOR ERROR LOCATION.')
  100   CONTINUE
      ELSE
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        !LOCAT=-LOCAT
        IF(K.GT.0) THEN
             WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
             WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE
             WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/
     1      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
        READ(LOCAT) A
      END IF
C
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
C
C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
C
C7------RETURN
      RETURN
C
C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
           WRITE(FNAME,501) ANAME,K,CNTRL
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,
     1     ' FOR LAYER',I4,':',/1x A)
      ELSE
           WRITE(FNAME,502) ANAME,CNTRL
  502  FORMAT(/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':',/1x A)
      END IF
          !CALL USTOP(' ')
          CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=FNAME)
      END
      SUBROUTINE UCOLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT)
C     ******************************************************************
C     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT
C        NLBL1 IS THE START COLUMN LABEL (NUMBER)
C        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)
C        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
C        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE
C        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
C        IOUT IS OUTPUT CHANNEL
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*1 DOT,SPACE,DG,BF
      DIMENSION BF(130),DG(10)
C
      DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/
     1         '0','1','2','3','4','5','6','7','8','9'/
      DATA DOT,SPACE/'.',' '/
C     ------------------------------------------------------------------
C
C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
        WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL=NLBL2-NLBL1+1
      N=NLBL
      IF(NLBL.GT.NCPL) N=NCPL
      NTOT=NSPACE+N*NDIG
      IF(NTOT.GT.130) GO TO 50
      NWRAP=(NLBL-1)/NCPL + 1
      J1=NLBL1-NCPL
      J2=NLBL1-1
C
C2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
C
C3------CLEAR THE BUFFER (BF).
      DO 20 I=1,130
      BF(I)=SPACE
   20 CONTINUE
      NBF=NSPACE
C
C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
      J1=J1+NCPL
      J2=J2+NCPL
      IF(J2.GT.NLBL2) J2=NLBL2
C
C5------LOAD THE COLUMN #'S INTO THE BUFFER.
      DO 30 J=J1,J2
      NBF=NBF+NDIG
      I2=J/10
      I1=J-I2*10+1
      BF(NBF)=DG(I1)
      IF(I2.EQ.0) GO TO 30
      I3=I2/10
      I2=I2-I3*10+1
      BF(NBF-1)=DG(I2)
      IF(I3.EQ.0) GO TO 30
      I4=I3/10
      I3=I3-I4*10+1
      BF(NBF-2)=DG(I3)
      IF(I4.EQ.0) GO TO 30
      IF(I4.GT.9) THEN
C5A-----If more than 4 digits, use "X" for 4th digit.
         BF(NBF-3)='X'
      ELSE
         BF(NBF-3)=DG(I4+1)
      END IF
   30 CONTINUE
C
C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
        WRITE(IOUT,31) (BF(I),I=1,NBF)
   31 FORMAT(1X,130A1)
C
   40 CONTINUE
C
C7------PRINT A LINE OF DOTS (FOR AESTHETIC PURPOSES ONLY).
   50 NTOT=NTOT
      IF(NTOT.GT.130) NTOT=130
        WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,130A1)
C
C8------RETURN
      RETURN
      END
      SUBROUTINE ULAPRS(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)
C     ******************************************************************
C     PRINT A 1 LAYER ARRAY IN STRIPS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DIMENSION BUF(NCOL,NROW)
C     ------------------------------------------------------------------
C
C1------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS BETWEEN 1
C1------AND 21.
      IP=IPRN
      IF(IP.LT.1 .OR. IP.GT.21) IP=12
C
C2------DETERMINE THE NUMBER OF VALUES (NCAP) PRINTED ON ONE LINE.
      NCAP=10
      IF(IP.EQ.1) NCAP=11
      IF(IP.EQ.2) NCAP=9
      IF(IP.GT.2 .AND. IP.LT.7) NCAP=15
      IF(IP.GT.6 .AND. IP.LT.12) NCAP=20
      IF(IP.EQ.19) NCAP=5
      IF(IP.EQ.20) NCAP=6
      IF(IP.EQ.21) NCAP=7
C
C3------CALCULATE THE NUMBER OF STRIPS (NSTRIP).
      NCPF=129/NCAP
      IF(IP.GE.13 .AND. IP.LE.18) NCPF=7
      IF(IP.EQ.19) NCPF=13
      IF(IP.EQ.20) NCPF=12
      IF(IP.EQ.21) NCPF=10
      ISP=0
      IF(NCAP.GT.12 .OR. IP.GE.13) ISP=3
      NSTRIP=(NCOL-1)/NCAP + 1
      J1=1-NCAP
      J2=0
C
C4------LOOP THROUGH THE STRIPS.
      DO 2000 N=1,NSTRIP
C
C5------CALCULATE THE FIRST(J1) & THE LAST(J2) COLUMNS FOR THIS STRIP
      J1=J1+NCAP
      J2=J2+NCAP
      IF(J2.GT.NCOL) J2=NCOL
C
C6-------PRINT TITLE ON EACH STRIP DEPENDING ON ILAY
      IF(ILAY.GT.0) THEN
           WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER
    1    FORMAT('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I3,
     1     ' IN STRESS PERIOD ',I4/2X,75('-'))
      ELSE IF(ILAY.LT.0) THEN
           WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3,
     1     ' IN STRESS PERIOD ',I4/1X,79('-'))
      END IF
C
C7------PRINT COLUMN NUMBERS ABOVE THE STRIP
      CALL UCOLNO(J1,J2,ISP,NCAP,NCPF,IOUT)
C
C8------LOOP THROUGH THE ROWS PRINTING COLS J1 THRU J2 WITH FORMAT IP
      DO 1000 I=1,NROW
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
     1      180,190,200,210), IP
C
C------------FORMAT 11G10.3
   10 WRITE(IOUT,11) I,(BUF(J,I),J=J1,J2)
   11 FORMAT(1X,I3,2X,ES10.3,10(1X,G10.3))
      GO TO 1000
C
C------------FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I),J=J1,J2)
   21 FORMAT(1X,I3,2X,ES13.6,8(1X,G13.6))
      GO TO 1000
C
C------------FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I),J=J1,J2)
   31 FORMAT(1X,I3,1X,15(1X,F7.1))
      GO TO 1000
C
C------------FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I),J=J1,J2)
   41 FORMAT(1X,I3,1X,15(1X,F7.2))
      GO TO 1000
C
C------------FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I),J=J1,J2)
   51 FORMAT(1X,I3,1X,15(1X,F7.3))
      GO TO 1000
C
C------------FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I),J=J1,J2)
   61 FORMAT(1X,I3,1X,15(1X,F7.4))
      GO TO 1000
C
C------------FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I),J=J1,J2)
   71 FORMAT(1X,I3,1X,20(1X,F5.0))
      GO TO 1000
C
C------------FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I),J=J1,J2)
   81 FORMAT(1X,I3,1X,20(1X,F5.1))
      GO TO 1000
C
C------------FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I),J=J1,J2)
   91 FORMAT(1X,I3,1X,20(1X,F5.2))
      GO TO 1000
C
C------------FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I),J=J1,J2)
  101 FORMAT(1X,I3,1X,20(1X,F5.3))
      GO TO 1000
C
C------------FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I),J=J1,J2)
  111 FORMAT(1X,I3,1X,20(1X,F5.4))
      GO TO 1000
C
C------------FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I),J=J1,J2)
  121 FORMAT(1X,I3,2X,ES11.4,9(1X,G11.4))
      GO TO 1000
C
C------------FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I),J=J1,J2)
  131 FORMAT(1X,I3,1X,10(1X,F6.0))
      GO TO 1000
C
C------------FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I),J=J1,J2)
  141 FORMAT(1X,I3,1X,10(1X,F6.1))
      GO TO 1000
C
C------------FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I),J=J1,J2)
  151 FORMAT(1X,I3,1X,10(1X,F6.2))
      GO TO 1000
C
C------------FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I),J=J1,J2)
  161 FORMAT(1X,I3,1X,10(1X,F6.3))
      GO TO 1000
C
C------------FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I),J=J1,J2)
  171 FORMAT(1X,I3,1X,10(1X,F6.4))
      GO TO 1000
C
C------------FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I),J=J1,J2)
  181 FORMAT(1X,I3,1X,10(1X,F6.5))
C
C------------FORMAT 5G12.5
  190 WRITE(IOUT,191) I,(BUF(J,I),J=J1,J2)
  191 FORMAT(1X,I3,1X,ES12.5,4(1X,G12.5))
C
C------------FORMAT 6G11.4
  200 WRITE(IOUT,201) I,(BUF(J,I),J=J1,J2)
  201 FORMAT(1X,I3,1X,ES11.4,5(1X,G11.4))
C
C------------FORMAT 7G9.2
  210 WRITE(IOUT,211) I,(BUF(J,I),J=J1,J2)
  211 FORMAT(1X,I3,1X,ES9.2,6(1X,G9.2))
C
 1000 CONTINUE
 2000 CONTINUE
C
C9------RETURN
      RETURN
      END
      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)
C     ******************************************************************
C     PRINT 1 LAYER ARRAY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DIMENSION BUF(NCOL,NROW)
C     ------------------------------------------------------------------
C
C0------SKIP PRINTING IF IOUT IS NEGATIVE seb
      IF(IOUT.EQ.0) RETURN
C1------PRINT A HEADER DEPENDING ON ILAY
      IF(ILAY.GT.0) THEN
           WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER
    1    FORMAT('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I3,
     1     ' IN STRESS PERIOD ',I4/2X,75('-'))
      ELSE IF(ILAY.LT.0) THEN
           WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3,
     1     ' IN STRESS PERIOD ',I4/1X,79('-'))
      END IF
C
C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
C2------BETWEEN 1 AND 21.
    5 IP=IPRN
      IF(IP.LT.1 .OR. IP.GT.21) IP=12
C
C3------CALL THE UTILITY MODULE UCOLNO TO PRINT COLUMN NUMBERS.
      IF(IP.EQ.1) CALL UCOLNO(1,NCOL,0,11,11,IOUT)
      IF(IP.EQ.2) CALL UCOLNO(1,NCOL,0,9,14,IOUT)
      IF(IP.GE.3 .AND. IP.LE.6) CALL UCOLNO(1,NCOL,3,15,8,IOUT)
      IF(IP.GE.7 .AND. IP.LE.11) CALL UCOLNO(1,NCOL,3,20,6,IOUT)
      IF(IP.EQ.12) CALL UCOLNO(1,NCOL,0,10,12,IOUT)
      IF(IP.GE.13 .AND. IP.LE.18) CALL UCOLNO(1,NCOL,3,10,7,IOUT)
      IF(IP.EQ.19) CALL UCOLNO(1,NCOL,0,5,13,IOUT)
      IF(IP.EQ.20) CALL UCOLNO(1,NCOL,0,6,12,IOUT)
      IF(IP.EQ.21) CALL UCOLNO(1,NCOL,0,7,10,IOUT)
C
C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO 1000 I=1,NROW
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
     1      180,190,200,210), IP
C
C------------ FORMAT 11G10.3
   10 WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)
   11 FORMAT(1X,I3,2X,ES10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      GO TO 1000
C
C------------ FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)
   21 FORMAT(1X,I3,2X,ES13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
      GO TO 1000
C
C------------ FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)
   31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
      GO TO 1000
C
C------------ FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)
   41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
      GO TO 1000
C
C------------ FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)
   51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
      GO TO 1000
C
C------------ FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)
   61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
      GO TO 1000
C
C------------ FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)
   71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
      GO TO 1000
C
C------------ FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)
   81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
      GO TO 1000
C
C------------ FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)
   91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
      GO TO 1000
C
C------------ FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)
  101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
      GO TO 1000
C
C------------ FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)
  111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
      GO TO 1000
C
C------------ FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)
  121 FORMAT(1X,I3,2X,ES11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
      GO TO 1000
C
C------------ FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I),J=1,NCOL)
  131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
      GO TO 1000
C
C------------ FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I),J=1,NCOL)
  141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
      GO TO 1000
C
C------------ FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I),J=1,NCOL)
  151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
      GO TO 1000
C
C------------ FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I),J=1,NCOL)
  161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
      GO TO 1000
C
C------------ FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I),J=1,NCOL)
  171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
      GO TO 1000
C
C------------ FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I),J=1,NCOL)
  181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
      GO TO 1000
C
C------------FORMAT 5G12.5
  190 WRITE(IOUT,191) I,(BUF(J,I),J=1,NCOL)
  191 FORMAT(1X,I3,2X,ES12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))
      GO TO 1000
C
C------------FORMAT 6G11.4
  200 WRITE(IOUT,201) I,(BUF(J,I),J=1,NCOL)
  201 FORMAT(1X,I3,2X,ES11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))
      GO TO 1000
C
C------------FORMAT 7G9.2
  210 WRITE(IOUT,211) I,(BUF(J,I),J=1,NCOL)
  211 FORMAT(1X,I3,2X,ES9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))
C
 1000 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE ULAPRWC(A,NCOL,NROW,ILAY,IOUT,IPRN,ANAME)
C     ******************************************************************
C     WRITE A TWO-DIMENSIONAL REAL ARRAY.  IF THE ARRAY IS CONSTANT,
C     PRINT JUST THE CONSTANT VALUE.  IF THE ARRAY IS NOT CONSTANT, CALL
C     ULAPRW TO PRINT IT.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION A(NCOL,NROW)
      CHARACTER*(*) ANAME
C     ------------------------------------------------------------------
C
C  Check to see if entire array is a constant.
      TMP=A(1,1)
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
      IF(A(J,I).NE.TMP) GO TO 400
  300 CONTINUE
      IF(ILAY.GT.0) THEN
           WRITE(IOUT,302) ANAME,TMP,ILAY
  302    FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
           WRITE(IOUT,303) ANAME,TMP
  303    FORMAT(1X,/1X,A,' =',1P,G14.6)
      ELSE
           WRITE(IOUT,304) ANAME,TMP
  304    FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR CROSS SECTION')
      END IF
      RETURN
C
C  Print the array.
  400 IF(ILAY.GT.0) THEN
           WRITE(IOUT,494) ANAME,ILAY
  494    FORMAT(1X,//11X,A,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
           WRITE(IOUT,495) ANAME
  495    FORMAT(1X,//11X,A)
      ELSE
           WRITE(IOUT,496) ANAME
  496    FORMAT(1X,//11X,A,' FOR CROSS SECTION')
      END IF
      IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,NCOL,NROW,0,IPRN,IOUT)
C
      RETURN
      END
!      SUBROUTINE ULASAV(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
!     1                   NROW,ILAY,ICHN)
!C     ******************************************************************
!C     SAVE 1 LAYER ARRAY ON DISK
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      CHARACTER*16 TEXT
!      REAL BUF
!      DIMENSION BUF(NCOL,NROW)
!C     ------------------------------------------------------------------
!C
!C1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING
!C1------INFORMATION.
!      WRITE(ICHN) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
!C
!C2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES
!C2------THE ARRAY IS DIMENSIONED (NCOL,NROW)
!      WRITE(ICHN) ((BUF(IC,IR),IC=1,NCOL),IR=1,NROW)
!C
!C3------RETURN
!      RETURN
!      END
      SUBROUTINE ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                   NROW,ILAY,ICHN,FMTOUT,LBLSAV,IBOUND)
C     ******************************************************************
C     SAVE 1 LAYER ARRAY ON DISK USING FORMATTED OUTPUT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DIMENSION BUFF(NCOL,NROW),IBOUND(NCOL,NROW)
      CHARACTER*20 FMTOUT
C     ------------------------------------------------------------------
C
C1------WRITE A LABEL IF LBLSAV IS NOT 0.
      IF(LBLSAV.NE.0) WRITE(ICHN,5) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,
     1                 NROW,ILAY,FMTOUT
5     FORMAT(1X,2I5,1P,2E15.6,1X,A,3I6,1X,A)
C
C2------WRITE THE ARRAY USING THE SPECIFIED FORMAT.
      DO 10 IR=1,NROW
      WRITE(ICHN,FMTOUT) (BUFF(IC,IR),IC=1,NCOL)
10    CONTINUE
C
C3------RETURN
      RETURN
      END
      SUBROUTINE ULASV3(IDATA,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                   NROW,ILAY,ICHN,FMTOUT,LBLSAV)
C     ******************************************************************
C     SAVE 2-D (LAYER) INTEGER ARRAY ON DISK USING FORMATTED OUTPUT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DIMENSION IDATA(NCOL,NROW)
      CHARACTER*20 FMTOUT
C     ------------------------------------------------------------------
C
C1------WRITE A LABEL IF LBLSAV IS NOT 0.
      IF(LBLSAV.NE.0) WRITE(ICHN,5) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,
     1                 NROW,ILAY,FMTOUT
5     FORMAT(1X,2I5,1P,2E15.6,1X,A,3I6,1X,A)
C
C2------WRITE THE ARRAY USING THE SPECIFIED FORMAT.
      DO 10 IR=1,NROW
      WRITE(ICHN,FMTOUT) (IDATA(IC,IR),IC=1,NCOL)
10    CONTINUE
C
C3------RETURN.
      RETURN
      END
!      SUBROUTINE UBUDSV(KSTP,KPER,TEXT,IBDCHN,BUFF,NCOL,NROW,NLAY,IOUT)
!C     ******************************************************************
!C     RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      CHARACTER*16 TEXT
!      DIMENSION BUFF(NCOL,NROW,NLAY)
!C     ------------------------------------------------------------------
!C
!C1------WRITE AN UNFORMATTED RECORD IDENTIFYING DATA.
!        WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER
!    1 FORMAT(1X,'UBUDSV SAVING "',A16,'" ON UNIT',I3,
!     1     ' AT TIME STEP',I5,', STRESS PERIOD ',I4)
!      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,NLAY
!C
!C2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
!C2------EACH CELL IN THE GRID.
!      WRITE(IBDCHN) BUFF
!C
!C3------RETURN
!      RETURN
!      END
!      SUBROUTINE UBDSV1(KSTP,KPER,TEXT,IBDCHN,BUFF,NCOL,NROW,NLAY,IOUT,
!     1          DELT,PERTIM,TOTIM,IBOUND)
!C     ******************************************************************
!C     RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW AS A 3-D
!C     ARRAY WITH EXTRA RECORD TO INDICATE DELT, PERTIM, AND TOTIM.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      CHARACTER*16 TEXT
!      DIMENSION BUFF(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
!C     ------------------------------------------------------------------
!C
!C1------WRITE TWO UNFORMATTED RECORDS IDENTIFYING DATA.
!      IF(IOUT.NE.0) WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER
!    1 FORMAT(1X,'UBDSV1 SAVING "',A16,'" ON UNIT',I4,
!     1     ' AT TIME STEP',I5,', STRESS PERIOD',I4)
!      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
!      WRITE(IBDCHN) 1,DELT,PERTIM,TOTIM
!C
!C2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
!C2------EACH CELL IN THE GRID.
!      WRITE(IBDCHN) BUFF
!C
!C3------RETURN
!      RETURN
!      END
!      SUBROUTINE UBDSV2(KSTP,KPER,TEXT,IBDCHN,NCOL,NROW,NLAY,
!     1          NLIST,IOUT,DELT,PERTIM,TOTIM,IBOUND)
!C     ******************************************************************
!C     WRITE HEADER RECORDS FOR CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT
!C     OF FLOW USING A LIST STRUCTURE.  EACH ITEM IN THE LIST IS WRITTEN
!C     BY MODULE UBDSVA
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      CHARACTER*16 TEXT
!      DIMENSION IBOUND(NCOL,NROW,NLAY)
!C     ------------------------------------------------------------------
!C
!C1------WRITE THREE UNFORMATTED RECORDS IDENTIFYING DATA.
!      IF(IOUT.NE.0) WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER
!    1 FORMAT(1X,'UBDSV2 SAVING "',A16,'" ON UNIT',I4,
!     1     ' AT TIME STEP',I5,', STRESS PERIOD',I4)
!      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
!      WRITE(IBDCHN) 2,DELT,PERTIM,TOTIM
!      WRITE(IBDCHN) NLIST
!C
!C2------RETURN
!      RETURN
!      END
!      SUBROUTINE UBDSVA(IBDCHN,NCOL,NROW,J,I,K,Q,IBOUND,NLAY)
!C     ******************************************************************
!C     WRITE ONE VALUE OF CELL-BY-CELL FLOW USING A LIST STRUCTURE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      DIMENSION IBOUND(NCOL,NROW,NLAY)
!C     ------------------------------------------------------------------
!C
!C1------CALCULATE CELL NUMBER
!      ICRL= (K-1)*NROW*NCOL + (I-1)*NCOL + J
!C
!C2------WRITE CELL NUMBER AND FLOW RATE
!      WRITE(IBDCHN) ICRL,Q
!C
!C3------RETURN
!      RETURN
!      END
!      SUBROUTINE UBDSV3(KSTP,KPER,TEXT,IBDCHN,BUFF,IBUFF,NOPT,
!     1              NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
!C     ******************************************************************
!C     RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW AS A 2-D
!C     ARRAY OF FLOW VALUES AND OPTIONALLY A 2-D ARRAY OF LAYER NUMBERS
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      CHARACTER*16 TEXT
!      DIMENSION BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW),
!     1          IBOUND(NCOL,NROW,NLAY)
!C     ------------------------------------------------------------------
!C
!C1------WRITE TWO UNFORMATTED RECORDS IDENTIFYING DATA.
!      IF(IOUT.NE.0) WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER
!    1 FORMAT(1X,'UBDSV3 SAVING "',A16,'" ON UNIT',I4,
!     1     ' AT TIME STEP',I5,', STRESS PERIOD',I4)
!      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
!      IMETH=3
!      IF(NOPT.EQ.1) IMETH=4
!      WRITE(IBDCHN) IMETH,DELT,PERTIM,TOTIM
!C
!C2------WRITE DATA AS ONE OR TWO UNFORMATTED RECORDS CONTAINING ONE
!C2------VALUE PER LAYER.
!      IF(NOPT.EQ.1) THEN
!C2A-----WRITE ONE RECORD WHEN NOPT IS 1.  THE VALUES ARE FLOW VALUES
!C2A-----FOR LAYER 1.
!         WRITE(IBDCHN) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
!      ELSE
!C2B-----WRITE TWO RECORDS WHEN NOPT IS NOT 1.  FIRST RECORD CONTAINS
!C2B-----LAYER NUMBERS;  SECOND RECORD CONTAINS FLOW VALUES.
!         WRITE(IBDCHN) ((IBUFF(J,I),J=1,NCOL),I=1,NROW)
!         WRITE(IBDCHN) ((BUFF(J,I,IBUFF(J,I)),J=1,NCOL),I=1,NROW)
!      END IF
!C
!C3------RETURN
!      RETURN
!      END
!      SUBROUTINE UBDSV4(KSTP,KPER,TEXT,NAUX,AUXTXT,IBDCHN,
!     1          NCOL,NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM,IBOUND)
!C     ******************************************************************
!C     WRITE HEADER RECORDS FOR CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT
!C     OF FLOW PLUS AUXILIARY DATA USING A LIST STRUCTURE.  EACH ITEM IN
!C     THE LIST IS WRITTEN BY MODULE UBDSVB
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      CHARACTER*16 TEXT,AUXTXT(*)
!      DIMENSION IBOUND(NCOL,NROW,NLAY)
!C     ------------------------------------------------------------------
!C
!C1------WRITE UNFORMATTED RECORDS IDENTIFYING DATA.
!      IF(IOUT.NE.0) WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER
!    1 FORMAT(1X,'UBDSV4 SAVING "',A16,'" ON UNIT',I4,
!     1     ' AT TIME STEP',I5,', STRESS PERIOD',I4)
!      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
!      WRITE(IBDCHN) 5,DELT,PERTIM,TOTIM
!      WRITE(IBDCHN) NAUX+1
!      IF(NAUX.GT.0) WRITE(IBDCHN) (AUXTXT(N),N=1,NAUX)
!      WRITE(IBDCHN) NLIST
!C
!C2------RETURN
!      RETURN
!      END
!      SUBROUTINE UBDSVB(IBDCHN,NCOL,NROW,J,I,K,Q,VAL,NVL,NAUX,LAUX,
!     1                  IBOUND,NLAY)
!C     ******************************************************************
!C     WRITE ONE VALUE OF CELL-BY-CELL FLOW PLUS AUXILIARY DATA USING
!C     A LIST STRUCTURE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
!      DIMENSION IBOUND(NCOL,NROW,NLAY),VAL(NVL)
!C     ------------------------------------------------------------------
!C
!C1------CALCULATE CELL NUMBER
!      ICRL= (K-1)*NROW*NCOL + (I-1)*NCOL + J
!C
!C2------WRITE CELL NUMBER AND FLOW RATE
!      IF(NAUX.GT.0) THEN
!         N2=LAUX+NAUX-1
!         WRITE(IBDCHN) ICRL,Q,(VAL(N),N=LAUX,N2)
!      ELSE
!         WRITE(IBDCHN) ICRL,Q
!      END IF
!C
!C3------RETURN
!      RETURN
!      END
      SUBROUTINE UMESPR(TEXT1,TEXT2,IOUT)
C     ******************************************************************
C     PRINT A LINE CONSISTING OF TWO TEXT VARIABLES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) TEXT1,TEXT2
C     ------------------------------------------------------------------
        WRITE(IOUT,*)
        WRITE(IOUT,'(1X,2A)') TEXT1,TEXT2
C
      RETURN
      END
C=======================================================================
      SUBROUTINE USTOP(STOPMESS)
      USE UTIL_INTERFACE, ONLY: STOP_ERROR
      USE GLOBAL,         ONLY: IOUT
C     ******************************************************************
C     STOP PROGRAM, WITH OPTION TO PRINT MESSAGE BEFORE STOPPING
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER(*), INTENT(IN):: STOPMESS
C     ------------------------------------------------------------------
      !
      CALL STOP_ERROR(OUTPUT=IOUT,MSG=STOPMESS)
      !!!IF (STOPMESS.NE.' ') WRITE(*,'(/ 1x,A)') TRIM(ADJUSTL(STOPMESS))
      !!!!
      !!!ERROR STOP      !seb CHANGED TO NEWER STANDARD TO ALLOW PASSING NON-ZERO EXIT STATUS
      !
      END SUBROUTINE
C
C
!*********!To call the subroutine you need to emulate the following code in gwf2lpf7.f 
!*********!
!*********!C8------PREPARE AND CHECK LPF DATA.
!*********!      CALL SGWF2LPF7N()
!*********!C seb added output of final array      
!*********!      CALL PRINTARRAY(IGRID,1)
!*********!C
!*********!C9------RETURN
!*********!      CALL GWF2LPF7PSV(IGRID)
!*********!      RETURN
!*********!      END
!*********!
!*********!and in gwf2upw1.f
!*********!
!*********!...
!*********!        CALL SGWF2UPW1VCOND(K)
!*********!      END DO
!*********!C seb      
!*********!      CALL PRINTARRAY(IGRID,2)
!*********!      
!*********!!10-----SAVE POINTERS FOR GRID AND RETURN
!*********!      CALL SGWF2UPW1PSV(Igrid)
!*********!!
!*********!!11-----RETURN
!*********!      END SUBROUTINE GWF2UPW1AR
      SUBROUTINE PRINTARRAY(IGRID,ORIGIN,DIROUT,IOUT)  !COULD ADD FLAG FOR WHAT ARRAY TO PRINT OUT
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE GLOBAL,        ONLY: NCOL,NROW,NLAY,DELR,DELC,BOTM,LBOTM,
     +                         ISSFLG,LAYHDT,IBOUND
      USE GWFLPFMODULE,  ONLY: HK1    =>HK,
     +                         VKCB1  =>VKCB,
     +                         LAYVKA1=>LAYVKA,
     +                         VKA1   =>VKA,
     +                         CHANI1 =>CHANI,
     +                         HANI1  =>HANI,
     +                         LAYTYP1=>LAYTYP,
     +                         SC11   =>SC1,
     +                         SC21   =>SC2
       USE GWFUPWMODULE, ONLY: HKUPW2     =>HKUPW,
     +                         VKCB2      =>VKCB,
     +                         LAYVKAUPW2 =>LAYVKAUPW,
     +                         VKAUPW2    =>VKAUPW,
     +                         CHANI2     =>CHANI,
     +                         HANI2      =>HANI,
     +                         LAYTYPUPW2 =>LAYTYPUPW,
     +                         SC12       =>SC1,
     +                         SC2UPW2    =>SC2UPW
      USE UTIL_INTERFACE, ONLY: GET_WARN
      IMPLICIT NONE
      INTEGER:: IGRID                                                   !INCOMING GRID ID
      INTEGER:: ORIGIN                                                  !1=LPF, 2=UPW
      CHARACTER(*)::DIROUT                                              !DIRECTORY TO PLACE OUTPUT FILES
      INTEGER:: IOUT                                                    !OUTPUT DIRECTORY
      CHARACTER(:),ALLOCATABLE::DIR                                     !DIRECTORY TO PRINT TOO
      INTEGER:: IU                                                      !TEMPORAY UNIT NUMBER
      INTEGER::I,J,K,N,KHANI
      !PRINTING VARIABLES
      INTEGER, POINTER, DIMENSION(:)    ,CONTIGUOUS ::T_LAYTYP
      REAL,    POINTER, DIMENSION(:)    ,CONTIGUOUS ::T_CHANI
      INTEGER, POINTER, DIMENSION(:)    ,CONTIGUOUS ::T_LAYVKA
      REAL,    POINTER, DIMENSION(:,:,:),CONTIGUOUS ::T_VKA
      REAL,    POINTER, DIMENSION(:,:,:),CONTIGUOUS ::T_HANI
      REAL,    POINTER, DIMENSION(:,:,:),CONTIGUOUS ::T_SC1
      REAL,    POINTER, DIMENSION(:,:,:),CONTIGUOUS ::T_SC2
      REAL,    POINTER, DIMENSION(:,:,:),CONTIGUOUS ::T_HK
      REAL,ALLOCATABLE, DIMENSION(:,:,:) ::BUFF

      CHARACTER(250)::FN
      CHARACTER(32)::SGRID,SLAY,SCOL,SFMT
      
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
      
      IF (ORIGIN.EQ.1)THEN                                              !USING LPF
        T_LAYTYP => LAYTYP1
        T_CHANI  => CHANI1
        T_LAYVKA => LAYVKA1
        T_VKA    => VKA1
        T_HANI   => HANI1
        T_SC1    => SC11
        T_SC2    => SC21
        T_HK     => HK1
      ELSEIF(ORIGIN.EQ.2)THEN                                           !USING UPW
        T_LAYTYP => LAYTYPUPW2
        T_CHANI  => CHANI2
        T_LAYVKA => LAYVKAUPW2
        T_VKA    => VKAUPW2
        T_HANI   => HANI2
        T_SC1    => SC12
        T_SC2    => SC2UPW2
        T_HK     => HKUPW2
      ELSE
        WRITE(*,'(A)') 'PRINTARRAY() ORIGIN VAIRABLE MUST BE 1 or 2'
        ERROR STOP
      END IF
      
      !DIR='.\'                                                          !CAN BE A READ IN VARIABLE TO SPECIFY LOCATION OF FILES
      DIR=TRIM(ADJUSTL(DIROUT))//'PARAM_'                                  !ADDED FIRST PART OF FILE NAME TO KEEP ALL FILES IN SAME ALPHABETICAL LOCATION
      !NL=(/'HKR_G','HKC_G','HKV_G','SC1_G','SC2_G'/)                   !Note that interally MF does a wierd averaging between SS and SY between timesteps
      !
      WRITE(SGRID,'(I32)')IGRID
      WRITE(SFMT,'(I32)')NCOL
      SGRID=ADJUSTL(SGRID)
      SFMT='('//TRIM(ADJUSTL(SFMT))//'ES20.10)'
      !
      DO K=1, NLAY
        WRITE(SLAY,'(I32)')K
        FN=DIR//'HKR_G'//TRIM(SGRID)//'_L'//TRIM(ADJUSTL(SLAY))//'.txt'
        IU = 0
        CALL GENERIC_OPEN(FN, IU, IOUT,
     +           ACTION='WRITE', FORM='FORMATTED',
     +           ACCESS='SEQUENTIAL', STATUS='REPLACE',
     +           BUFFER_BLOCKSIZE=16384, BUFFER_COUNT=1)
!        OPEN(NEWUNIT=IU,FILE=FN,WARN=
!     +         STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE') 
        WRITE(IU,'(4I10,A,10x,A)')NROW,NCOL,K,IGRID,'  HKR',
     +                         'NROW,NCOL,LAY,IGRID' !HEADER INFORMATION
        DO I=1,NROW
          WRITE(IU,SFMT)T_HK(:,I,K)                  !REPLACES(HK(J,I,K),J=1,NCOL) !PRINT OUT ARRAYS
        END DO
        CLOSE(IU)
      END DO
      
      DO K=1, NLAY
        WRITE(SLAY,'(I32)')K
        FN=DIR//'HKC_G'//TRIM(SGRID)//'_L'//TRIM(ADJUSTL(SLAY))//'.txt'
        OPEN(NEWUNIT=IU,FILE=FN,
     +         STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE') 
        WRITE(IU,'(4I10,A,10x,A)')NROW,NCOL,K,IGRID,'  HKC',
     +                         'NROW,NCOL,LAY,IGRID' !HEADER INFORMATION
        IF(T_CHANI(K).LE.0.) THEN
          KHANI=-T_CHANI(K)
          DO I=1,NROW
            WRITE(IU,SFMT)T_HK(:,I,K)*T_HANI(:,I,KHANI)
          END DO
        ELSE
          DO I=1,NROW
            WRITE(IU,SFMT)T_HK(:,I,K)*T_CHANI(K)
          END DO
        END IF
        CLOSE(IU)
      END DO
      
      IF(ANY(T_LAYVKA.NE.0)) THEN  !THERE WILL BE VKA=HK/HANI
          WHERE (T_VKA .GT. 0.0 .AND. IBOUND.NE.0)    !NOTE THAT THIS BLINDLY DOES ALL LAYERS, BUT ONLY THE ONES THAT HAVE T_LAYVKA.NE.0 ARE ACTUALLY USED
             BUFF=T_HK/T_VKA
          ELSEWHERE
             BUFF=0.0
          END WHERE
      END IF
      DO K=1, NLAY
        WRITE(SLAY,'(I32)')K
        FN=DIR//'VKA_G'//TRIM(SGRID)//'_L'//TRIM(ADJUSTL(SLAY))//'.txt'
        OPEN(NEWUNIT=IU,FILE=FN,
     +         STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE') 
        WRITE(IU,'(4I10,A,10x,A)')NROW,NCOL,K,IGRID,'  VKA',
     +                         'NROW,NCOL,LAY,IGRID' !HEADER INFORMATION
        IF(T_LAYVKA(K).EQ.0) THEN
          DO I=1,NROW
            WRITE(IU,SFMT)T_VKA(:,I,K)
          END DO
        ELSE
          DO I=1,NROW
            WRITE(IU,SFMT)BUFF(:,I,K)  !BUFF IS CALCULATED BEFORE LOOP
          END DO
        END IF
        CLOSE(IU)
      END DO
      !      
      IF(ANY(ISSFLG.EQ.0))THEN
       BUFF=T_SC1                                                       !SET BUFFER TO STORACE COEFICIENT 1
       WHERE(IBOUND.EQ.0)BUFF=0E0
       IF    (ORIGIN.EQ.1) THEN                                         !USING LPF
!         FORALL(I=1:NROW,J=1:NCOL,K=1:NLAY,IBOUND(J,I,K).NE.0)          !Ss=SC/(dr*dc*thick)       
!     +     BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I)            
!     +                    *(BOTM(J,I,LBOTM(K)-1)-BOTM(J,I,LBOTM(K))) )
         DO I=1,NROW
         DO J=1,NCOL
         DO K=1,NLAY
           IF(IBOUND(J,I,K).NE.0) THEN
          BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I)                     !Ss=SC/(dr*dc*thick)   
     +                    *(BOTM(J,I,LBOTM(K)-1)-BOTM(J,I,LBOTM(K))) )
          END IF
         END DO
         END DO
         END DO
       ELSEIF(ORIGIN.EQ.2) THEN                                         !USING UPW
!         FORALL(I=1:NROW,J=1:NCOL,K=1:NLAY,IBOUND(J,I,K).NE.0)          !Ss=SC/(dr*dc)
!     +     BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I) )
         DO I=1,NROW
         DO J=1,NCOL
         DO K=1,NLAY
           IF(IBOUND(J,I,K).NE.0) THEN                                  !Ss=SC/(dr*dc)
            BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I) )
          END IF
         END DO
         END DO
         END DO
       END IF
       !
       DO K=1, NLAY
         WRITE(SLAY,'(I32)')K
         FN=DIR//'Ss_G'//TRIM(SGRID)//'_L'//TRIM(ADJUSTL(SLAY))//'.txt'
         OPEN(NEWUNIT=IU,FILE=FN,
     +          STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE') 
         WRITE(IU,'(4I10,A,10x,A)')NROW,NCOL,K,IGRID,'  Ss ',
     +                          'NROW,NCOL,LAY,IGRID' !HEADER INFORMATION
         DO I=1,NROW
           WRITE(IU,SFMT)BUFF(:,I,K)
         END DO
         CLOSE(IU)
       END DO
       !
       IF(ANY(LAYHDT.NE.0)) THEN                                        !THERE IS ATLEAST ONE CONVERTABLE LAYER
        N=MAXVAL(T_LAYTYP)
        BUFF(:,:,1:N)=T_SC2                                             !SET BUFFER TO STORACE COEFICIENT 2
        IF    (ORIGIN.EQ.1) THEN                                        !USING LPF
!         FORALL(I=1:NROW,J=1:NCOL,K=1:N)                                !Sy=SC/(dr*dc)
!     +                       BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I) )
         DO I=1,NROW
         DO J=1,NCOL
         DO K=1,NLAY
           IF(IBOUND(J,I,K).NE.0) THEN                                  !Sy=SC/(dr*dc)
             BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I) )
          END IF
         END DO
         END DO
         END DO
        ELSEIF(ORIGIN.EQ.2) THEN                                        !USING UPW
!         FORALL(I=1:NROW,J=1:NCOL,K=1:N)                                !Sy=SC/(dr*dc)
!     +                       BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I) )
         DO I=1,NROW
         DO J=1,NCOL
         DO K=1,NLAY
           IF(IBOUND(J,I,K).NE.0) THEN                                  !Sy=SC/(dr*dc)
             BUFF(J,I,K)=BUFF(J,I,K)/( DELR(J)*DELC(I) )
           END IF
         END DO
         END DO
         END DO
        END IF
        !
        DO K=1, NLAY
         IF(T_LAYTYP(K).NE.0) THEN
          WRITE(SLAY,'(I32)')K
          FN=DIR//'Sy_G'//TRIM(SGRID)//'_L'//TRIM(ADJUSTL(SLAY))//'.txt'
          OPEN(NEWUNIT=IU,FILE=FN,
     +           STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE') 
          WRITE(IU,'(4I10,A,10x,A)')NROW,NCOL,K,IGRID,'  Sy ',
     +                           'NROW,NCOL,LAY,IGRID' !HEADER INFORMATION
          DO I=1,NROW
            WRITE(IU,SFMT)BUFF(:,I,T_LAYTYP(K))
          END DO
          CLOSE(IU)
         END IF
        END DO
       END IF
      END IF
      
      T_LAYTYP =>NULL()
      T_CHANI  =>NULL()
      T_LAYVKA =>NULL()
      T_VKA    =>NULL()
      T_HANI   =>NULL()
      T_SC1    =>NULL()
      T_SC2    =>NULL()
      T_HK     =>NULL()
      DEALLOCATE(BUFF)
      IF(ALLOCATED(DIR)) DEALLOCATE(DIR)
      END SUBROUTINE
      ! 
      SUBROUTINE DATE_STR_TO_SP(DATE_STR,SP)
      ! SP = -1 MEANS DAT_STR FAILED TO BE CONVERTED TO DATE
      ! SP = -2 MEANS THAT DIS START_DATE WAS NOT DEFINED
      USE GWFBASMODULE,              ONLY: HAS_STARTDATE, DATE_SP
      USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN ):: DATE_STR
      INTEGER,      INTENT(OUT):: SP
      TYPE(DATE_OPERATOR):: DATE
      INTEGER:: N, NPER, I
      !
      CALL DATE%INIT( DATE_STR, 0.001D0 )          !Add a slight shift to ensure that date is either within SP or just beyond.
      !
      IF (DATE%NOT_SET()) THEN
          SP = -1
          RETURN
      END IF
      !
      IF(.NOT. HAS_STARTDATE .AND. DATE%IS_SET()) THEN
          SP = -2
          RETURN
      END IF
      !
      NPER = SIZE(DATE_SP)
      N = UBOUND( DATE_SP(1)%TS,1 )
      SP=NPER+1                       !IF DATE IS BEYOND SIMFRAME SET TO NPER+1
      !
      IF ( DATE <  DATE_SP(1)%TS(N) ) THEN
                                                  SP=1
      !ELSEIF ( DATE >= DATE_SP(NPER)%TS(0) ) THEN
      !                                            SP=NPER
      ELSE
         DO I=2, NPER
           N = UBOUND( DATE_SP(I)%TS,1 )
           IF(DATE_SP(I)%TS(0)<=DATE .AND. DATE<DATE_SP(I)%TS(N) )THEN
                                                   SP=I
                                                   EXIT
           END IF
         END DO
      END IF
      !
      END SUBROUTINE
      ! 
      SUBROUTINE DATE_TO_SP(DATE,SP)
      ! SP = -1 MEANS DAT_STR FAILED TO BE CONVERTED TO DATE
      ! SP = -2 MEANS THAT DIS START_DATE WAS NOT DEFINED
      USE GWFBASMODULE,              ONLY: HAS_STARTDATE, DATE_SP
      USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
      IMPLICIT NONE
      TYPE(DATE_OPERATOR), INTENT(IN ):: DATE
      INTEGER,             INTENT(OUT):: SP
      INTEGER:: N, NPER, I
      !
      IF (DATE%NOT_SET()) THEN
          SP = -1
          RETURN
      END IF
      !
      IF(.NOT. HAS_STARTDATE .AND. DATE%IS_SET()) THEN
          SP = -2
          RETURN
      END IF
      !
      NPER = SIZE(DATE_SP)
      N = UBOUND( DATE_SP(1)%TS,1 )
      SP=NPER+1                       !IF DATE IS BEYOND SIMFRAME SET TO NPER+1
      !
      IF ( DATE <  DATE_SP(1)%TS(N) ) THEN
                                                  SP=1
      !ELSEIF ( DATE >= DATE_SP(NPER)%TS(0) ) THEN
      !                                            SP=NPER
      ELSE
         DO I=2, NPER
           N = UBOUND( DATE_SP(I)%TS,1 )
           IF(DATE_SP(I)%TS(0)<=DATE .AND. DATE<DATE_SP(I)%TS(N) )THEN
                                                   SP=I
                                                   EXIT
           END IF
         END DO
      END IF
      !
      END SUBROUTINE
      !
      !FUNCTION THAT DETERMINES IF CELL IS WET OR DRY BASED ON LAY, ROW, COL
      PURE ELEMENTAL FUNCTION WET_CELL(LAY,ROW,COL)
      USE CONSTANTS,     ONLY: Z
      USE GLOBAL,        ONLY: BOTM,LBOTM,LAYHDT,IBOUND,HNEW
      IMPLICIT NONE
      INTEGER, INTENT(IN):: LAY,ROW,COL
      LOGICAL:: WET_CELL
      !
      IF(LAYHDT(LAY)==Z) THEN !CONFINE LAYER
          WET_CELL = IBOUND(COL,ROW,LAY) .NE. Z
      ELSE
          WET_CELL = IBOUND(COL,ROW,LAY) .NE. Z .AND. 
     +               HNEW(COL,ROW,LAY) > BOTM(COL,ROW,LBOTM(LAY))
      END IF
      !
      END FUNCTION
!
      SUBROUTINE U2DDP(A,ANAME,II,JJ,K,IN,IOUT)
C-----VERSION 2 09/21/09 U2DDP (U2DREL MODIFIED BY WSCHMID)
C     ******************************************************************
C     ROUTINE TO INPUT 2-D DOUBLE PRECISION DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CONSTANTS,              ONLY: TRUE
      USE UTIL_INTERFACE,         ONLY: FILE_IO_ERROR,  READ_TO_DATA,
     +                                  PARSE_WORD, PARSE_WORD_UP, 
     +                                  GET_INTEGER, GET_NUMBER,
     +                                  STOP_ERROR
      USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
      USE OPENSPEC
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      INTEGER II,JJ,K,IN,IOUT
      DOUBLE PRECISION A(JJ,II)
C     ------------------------------------------------------------------      
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*700 FNAME
      !DATA NUNOPN/99/NUNOPN,
      INTEGER I,ICLOSE,IFREE,ICOL,ISTART,ISTOP,N,LOCAT,IPRN,J,
     +        KSTP,KPER,NCOL,NROW,ILAY,IERR
      REAL R,PERTIM,TOTIM
      DOUBLE PRECISION CNSTNT,ZERO
      LOGICAL:: FORMATTED
C     ------------------------------------------------------------------      
      !INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------

C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      CALL URDCOM(IN,IOUT,CNTRL) ! seb originally => READ(IN,'(A)') CNTRL
      FORMATTED=TRUE
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL PARSE_WORD_UP(CNTRL,ICOL,ISTART,ISTOP)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,IN,LOCAT)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL PARSE_WORD(CNTRL,ICOL,ISTART,ISTOP)
         FNAME=CNTRL(ISTART:ISTOP)
         !LOCAT=NUNOPN
         LOCAT = -1
         IF(IOUT.NE.0) WRITE(IOUT,15) FNAME
         !
   15    FORMAT(1X,/1X,'OPENING FILE :',1X,A)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL GET_NUMBER(CNTRL,ICOL,ISTART,ISTOP,IOUT,IN,CNSTNT)
         IF(LOCAT.NE.0) THEN
            CALL PARSE_WORD_UP(CNTRL,ICOL,ISTART,ISTOP)
            FMTIN=CNTRL(ISTART:ISTOP)
            LOCAT = 0
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  FORMATTED=.FALSE.
!                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,
!     &                 ACTION=ACTION(1),IOSTAT=IERR)
                  CALL GENERIC_OPEN(FNAME, LOCAT, IOUT, ACTION(1), FORM,
     +                           ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384, !Buffer = 16kB*2
     +                           LINE=CNTRL, INFILE=IN)
               ELSE
               !OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1),IOSTAT=IERR)
               CALL GENERIC_OPEN(FNAME,LOCAT,IOUT,ACTION(1),'FORMATTED',
     +                           ACCESS, 'OLD', BUFFER_BLOCKSIZE=16384, !Buffer = 16kB*2
     +                           LINE=CNTRL, INFILE=IN)
               END IF
!         IF(IERR.NE.0) 
!     +         CALL FILE_IO_ERROR(IERR,FNAME=FNAME,LINE=CNTRL,
!     +                            INFILE=IN,OUTPUT=IOUT)
            END IF
            IF(LOCAT.NE.0 .AND. FMTIN.EQ.'(BINARY)') FORMATTED=.FALSE.!LOCAT=-LOCAT
            CALL GET_INTEGER(CNTRL,ICOL,ISTART,ISTOP,IOUT,IN,IPRN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        DO I=1,II
        DO J=1,JJ
           A(J,I)=CNSTNT
        ENDDO
        ENDDO
        IF(IOUT.NE.0) THEN                                 !seb ADDED IOUT.NE.0
          IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
        ENDIF
    2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
        IF(IOUT.NE.0) THEN                                 !seb ADDED IOUT.NE.0
          IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
        ENDIF
    3   FORMAT(1X,/1X,A,' =',1P,G14.6)
        RETURN
      ELSE IF(LOCAT.NE.0 .AND. FORMATTED) THEN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           IF(IOUT.NE.0) THEN                              !seb ADDED IOUT.NE.0
             WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
           ENDIF
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           IF(IOUT.NE.0) THEN                              !seb ADDED IOUT.NE.0
             WRITE(IOUT,95) ANAME,LOCAT,FMTIN
           ENDIF
   95      FORMAT(1X,///11X,A,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           IF(IOUT.NE.0) THEN                              !seb ADDED IOUT.NE.0
             WRITE(IOUT,96) ANAME,LOCAT,FMTIN
           ENDIF
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO I=1,II
        CALL READ_TO_DATA(CNTRL,LOCAT,IOUT,NOSHIFT=TRUE,BACK_UP=TRUE)
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) A(1:JJ,I)  !(A(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) A(1:JJ,I)  !(A(J,I),J=1,JJ)
        END IF
        ENDDO
      ELSE
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        !LOCAT=-LOCAT
        IF(K.GT.0) THEN
           IF(IOUT.NE.0) THEN                              !seb ADDED IOUT.NE.0
             WRITE(IOUT,201) ANAME,K,LOCAT
           ENDIF
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           IF(IOUT.NE.0) THEN                              !seb ADDED IOUT.NE.0
             WRITE(IOUT,202) ANAME,LOCAT
           ENDIF
  202      FORMAT(1X,///1X,A,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           IF(IOUT.NE.0) THEN                              !seb ADDED IOUT.NE.0
             WRITE(IOUT,203) ANAME,LOCAT
           ENDIF
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/
     1      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
        READ(LOCAT) A
      END IF
C
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.D0
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO I=1,II
      DO J=1,JJ
         A(J,I)=A(J,I)*CNSTNT
      ENDDO
      ENDDO
C
C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(SNGL(A),ANAME,0,0,JJ,II,0,IPRN,IOUT)
C
C7------RETURN
      RETURN
C
C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
           WRITE(FNAME,501) ANAME,K,CNTRL
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,
     1     ' FOR LAYER',I4,':',/1x A)
      ELSE
           WRITE(FNAME,502) ANAME,CNTRL
  502  FORMAT(/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':',/1x A)
      END IF
        !WRITE(IOUT,'(1X,A)') CNTRL
        !CALL USTOP(' ')
      CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG=FNAME)
C
C9===== END ==============================================================================================      
      END SUBROUTINE