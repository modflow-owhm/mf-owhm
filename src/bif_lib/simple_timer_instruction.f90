!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE SIMPLE_TIMER_INSTRUCTION
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: DBL => REAL64
  PRIVATE
  PUBLIC:: SIMPLE_TIMER
  !
  TYPE SIMPLE_TIMER
      INTEGER  :: EPOCH     = 0
      INTEGER  :: FINISH    = 0
      REAL(DBL):: ClockRate = -999_DBL
      !
      CONTAINS
      !
      PROCEDURE, PASS(TIMER):: INIT     =>    INIT_SIMPLE_TIMER   !()       - Optional, sets up ClockRate
      PROCEDURE, PASS(TIMER):: START    =>   START_SIMPLE_TIMER   !()       - Start the timer
      PROCEDURE, PASS(TIMER):: LAP      =>     LAP_SIMPLE_TIMER   !(SEC)    - SEC is set to the number of sections since start of last lap
      PROCEDURE, PASS(TIMER):: ELAPSED  => ELAPSED_SIMPLE_TIMER   !(SEC)    - SEC is set to the number of sections since %START()
      PROCEDURE, PASS(TIMER):: PRINT_LAP=> PRINT_LAP_SIMPLE_TIMER !([TEXT]) - write to prompt elapsed time in seconds since start of last lap
      !
  END TYPE 
  !
  CONTAINS
  !
  SUBROUTINE INIT_SIMPLE_TIMER(TIMER)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    INTEGER:: I, ClockRate
    !
    TIMER%EPOCH  = 0
    TIMER%FINISH = 0
    !
    CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
    !
    IF(ClockRate.LE.0) THEN
                             DO I=1, 1000                                     !  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
                                     CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)  ! Find the rate  
                                     IF(ClockRate > 0) EXIT
                             END DO
                             IF(ClockRate.LE.0) CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
    END IF
    !
    IF(ClockRate > 0 ) THEN
                       TIMER%ClockRate = 1_DBL / REAL(ClockRate, DBL)
    ELSE
                       TIMER%ClockRate = 0_DBL
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE GET_SYSTEM_CLOCK_COUNT(TIMER, IVAL)
    CLASS(SIMPLE_TIMER), INTENT(IN   ):: TIMER
    INTEGER,             INTENT(INOUT):: IVAL
    INTEGER:: I
    !
    IF(TIMER%ClockRate < 1.0E-250_DBL) THEN
                                   IVAL = TIMER%EPOCH
    ELSE
        CALL SYSTEM_CLOCK(COUNT=IVAL)
        !
        IF(IVAL.LE.0) THEN
                      DO I=1, 1000                                   !  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
                              CALL SYSTEM_CLOCK(COUNT=IVAL)  ! Find the rate  
                              IF(IVAL > 0) EXIT
                      END DO
                      IF(IVAL.LE.0) CALL SYSTEM_CLOCK(COUNT=IVAL)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE START_SIMPLE_TIMER(TIMER)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    !
    IF(TIMER%ClockRate < 1.0E-250_DBL) CALL INIT_SIMPLE_TIMER(TIMER)
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, TIMER%EPOCH)
    !
    TIMER%FINISH = TIMER%EPOCH
    !
  END SUBROUTINE
  !
  SUBROUTINE ELAPSED_SIMPLE_TIMER(TIMER, SEC)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    REAL(DBL),           INTENT(INOUT):: SEC
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, TIMER%FINISH)
    !
    IF(TIMER%FINISH .LE. TIMER%EPOCH) THEN
        SEC = 0_DBL
    ELSE
        SEC = REAL( TIMER%FINISH-TIMER%EPOCH, DBL ) * TIMER%ClockRate
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE LAP_SIMPLE_TIMER(TIMER, SEC)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    REAL(DBL),           INTENT(INOUT):: SEC
    INTEGER:: LAP
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, LAP)
    !
    IF(LAP .LE. TIMER%EPOCH) THEN
                SEC = 0_DBL
                TIMER%FINISH = TIMER%EPOCH
    ELSE
                SEC = REAL( LAP-TIMER%FINISH, DBL ) * TIMER%ClockRate
                TIMER%FINISH = LAP
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_LAP_SIMPLE_TIMER(TIMER, TXT)
    CLASS(SIMPLE_TIMER),           INTENT(INOUT):: TIMER
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: TXT
    REAL(DBL):: SEC
    !
    CALL LAP_SIMPLE_TIMER(TIMER, SEC)
    !
    IF(PRESENT(TXT)) THEN
                     WRITE(*,'(A,F16.3)') TXT, SEC
    ELSE
                     WRITE(*,'(F16.3)') SEC
    END IF
    !
  END SUBROUTINE
  !
END MODULE