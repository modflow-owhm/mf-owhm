!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!   UTIL_INTERFACE
!                           FUNCTIONS
!                                    IS_ASCII                     (LN)
!                           SUBROUTINES
!                                    ASCII_CHECK(LN, OUTPUT, [ERRMSG]) 
!
!    
!
!
MODULE IS_ASCII_INTERFACE
  !
  USE CONSTANTS,      ONLY: ONE, BLNK, NL, BLN, TRUE, FALSE, FOUR
  USE ERROR_INTERFACE,ONLY: STOP_ERROR
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: IS_ASCII    !(LN)
  PUBLIC:: ASCII_CHECK
  !
  INTERFACE  ASCII_CHECK
    MODULE PROCEDURE ASCII_CHECK_LOGICAL!(LN, IS_ASCII) 
    MODULE PROCEDURE ASCII_CHECK_STOP   !(LN, IOUT    ) 
    MODULE PROCEDURE ASCII_CHECK_CHAR   !(LN, NON_ASCII)
  END INTERFACE
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  ASCII routines
  !
  PURE ELEMENTAL FUNCTION IS_ASCII(LN)
      CHARACTER(*),INTENT(IN):: LN
      LOGICAL:: IS_ASCII
      INTEGER::I
      !
      IS_ASCII = TRUE
      !
      DO I = ONE, LEN_TRIM(LN)  !CHECK FOR BAD ASCII CODES
          IF ( ICHAR( LN(I:I) ) > 126 ) THEN
              !
              IS_ASCII = FALSE
              !
              EXIT
          END IF
      END DO
      !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ASCII_CHECK_LOGICAL(LN, IS_ASCII)
      CHARACTER(*),INTENT(IN   ):: LN
      LOGICAL,     INTENT(INOUT):: IS_ASCII
      INTEGER::I
      !
      IS_ASCII = TRUE
      !
      DO I = ONE, LEN_TRIM(LN)  !CHECK FOR BAD ASCII CODES
          IF ( ICHAR( LN(I:I) ) > 126 ) THEN
              !
              IS_ASCII = FALSE
              !
              EXIT
          END IF
      END DO
      !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE ASCII_CHECK_STOP(LN, IOUT, MSG) !CLONE OF UTIL_INTERFACE SUBROUTINE OF SAME NAME  --ASSUME ALL IS CASE CORRECT
      CHARACTER(*),           INTENT(IN):: LN
      INTEGER,                INTENT(IN):: IOUT
      CHARACTER(*), OPTIONAL, INTENT(IN):: MSG  ! Supplemental messages to write in error
      LOGICAL:: ASCII_ERROR
      INTEGER::I
      !
      ASCII_ERROR = FALSE
      !
      DO I = ONE, LEN_TRIM(LN)  !CHECK FOR BAD ASCII CODES
          IF ( ICHAR( LN(I:I) ) > 126 ) THEN
              !
              ASCII_ERROR = TRUE
              !
              EXIT
          END IF
      END DO
      !
      IF(ASCII_ERROR) CALL STOP_ERROR( LINE=LN, OUTPUT=IOUT, MSG= 'FOUND NON-ASCII CHARACTER IN LINE.'//BLN//'THIS INPUT ONLY ALLOWS FOR STANDARD ENGLISH ASCII CHARACTERS (ASCII CODES 1 TO 126)'//NL//'THE CHARACTER THAT IS A PROBLEM IS "'//LN(I:I)//'".'//BLN//'THIS CAN HAPPEN IF YOU COPY/PAST FROM AN ADVANCED EDITOR LIKE MS-WORD.'//NL//'AN EASY FIX IS TO REWRITE YOUR EQUATION IN AN ASCII/UNICODE BASIC TEXT EDITOR.', MSG2=MSG )
      !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE ASCII_CHECK_CHAR(LN, NON_ASCII) !CLONE OF UTIL_INTERFACE SUBROUTINE OF SAME NAME  --ASSUME ALL IS CASE CORRECT
      CHARACTER(*),                        INTENT(IN   ):: LN
      CHARACTER(:),           ALLOCATABLE, INTENT(INOUT):: NON_ASCII
      LOGICAL:: NOT_ASCII
      INTEGER::I
      !
      IF(ALLOCATED(NON_ASCII)) DEALLOCATE(NON_ASCII)
      NON_ASCII = BLNK 
      !
      NOT_ASCII = FALSE
      !
      DO I = ONE, LEN_TRIM(LN)  !CHECK FOR BAD ASCII CODES
          IF ( ICHAR( LN(I:I) ) > 126 ) THEN
              !
              NOT_ASCII = TRUE
              !
              NON_ASCII = NON_ASCII//', "'//LN(I:I)//'"'
              !
          END IF
      END DO
      !
      IF(NOT_ASCII) THEN
          NON_ASCII = NON_ASCII(FOUR:)
      ELSE
          DEALLOCATE(NON_ASCII)
      END IF
      !
  END SUBROUTINE
  !
END MODULE 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!