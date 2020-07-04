!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
MODULE RANDOM_ROUTINES!, ONLY: RANDOM_GENERATOR, SHUFFLE, SET_FORTRAN_SEED, COIN_TOSS, BINARY_TOSS
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i8 => INT8,   i16 => INT16,  &
                                         i32 => INT32,  i64 => INT64,  &
                                         SNG => REAL32, DBL => REAL64, &
                                                        QAD => REAL128
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: RANDOM_GENERATOR
  PUBLIC:: SET_FORTRAN_SEED!([SEED])
  PUBLIC:: SHUFFLE         !(VEC, [SEED])
  PUBLIC:: COIN_TOSS       !()
  PUBLIC:: BINARY_TOSS     !()
  !
  !
  ! Constants used internally to module ----------------------------------------------------
  !
  INTEGER(i32),  PARAMETER:: NEG   = -1
  INTEGER(i32),  PARAMETER:: Z     = 0
  INTEGER(i32),  PARAMETER:: ONE   = 1
  INTEGER(i32),  PARAMETER:: TWO   = 2
  INTEGER(i32),  PARAMETER:: EIGHT = 8
  !                                 
  REAL(DBL),     PARAMETER:: DZ   =   0_dbl
  REAL(DBL),     PARAMETER:: HALF = 0.5_dbl
  REAL(DBL),     PARAMETER:: UNO  =   1_dbl
  !
  INTEGER(i64),  PARAMETER:: LONG_NEG    = -1_i64
  INTEGER(i64),  PARAMETER:: LONG_ZER    =  0_i64
  INTEGER(i64),  PARAMETER:: LONG_ONE    =  1_i64
  INTEGER(i64),  PARAMETER:: LONG_TWO    =  2_i64
  INTEGER(i64),  PARAMETER:: LONG_inf_I  =  INT( HUGE(Z), i64)
  !
  ! ----------------------------------------------------------------------------------------
  !
  INTERFACE SHUFFLE
    MODULE PROCEDURE  INT_SHUFFLE  !(VEC,SEED)
    MODULE PROCEDURE DBLE_SHUFFLE  !(VEC,SEED)
    MODULE PROCEDURE REAL_SHUFFLE  !(VEC,SEED)
  END INTERFACE
  !  
  TYPE RANDOM_GENERATOR                      ! Uses a LINEAR_CONGRUENTIAL_GENERATOR to make random numbers
      !
      INTEGER(i64):: m    = 2147483647_i64   !Values from C18 standard  - Must be a power of 2 minus 1, eg 2^31-1
      INTEGER(i64):: a    = 1103515245_i64
      INTEGER(i64):: c    = 12345_i64
      INTEGER(i64):: seed = LONG_NEG         !Flag to indicate seed is not set
      !
      CONTAINS
      !
      PROCEDURE, PASS(LCG):: INIT       => INITIALIZE_LCG!([ID], [SEED])
      GENERIC             :: GEN        => GENERATE_RANDOM_INT_SUB_LCG, &  !GEN(RAND)
                                           GENERATE_RANDOM_DBL_SUB_LCG, &    
                                           GENERATE_RANDOM_SGL_SUB_LCG
      GENERIC             :: PURE_GEN   => GENERATE_RANDOM_INT_PURE_SUB_LCG,  &  !GEN(RAND)
                                           GENERATE_RANDOM_DBL_PURE_SUB_LCG,  &    
                                           GENERATE_RANDOM_SNG_PURE_SUB_LCG
      PROCEDURE, PASS(LCG):: GEN_INT    => GENERATE_RANDOM_INT_LCG             ! GEN_INT   ()
      PROCEDURE, PASS(LCG):: GEN_DOUBLE => GENERATE_RANDOM_DBL_LCG             ! GEN_DOUBLE() 
      PROCEDURE, PASS(LCG):: GEN_SINGLE => GENERATE_RANDOM_SNG_LCG             ! GEN_SINGLE() 
      PROCEDURE, PASS(LCG):: GEN_BINARY => GENERATE_RANDOM_BINARY_LCG          ! GEN_BINARY() 
      PROCEDURE, PASS(LCG):: FLIP_COIN  => GENERATE_RANDOM_COIN_LCG            ! FLIP_COIN () 
      PROCEDURE, PASS(LCG):: SET_PROP   => SET_SEED_LCG                        ! SET_PROP([ID], [SEED], [M], [A], [C])
      !
      GENERIC:: ASSIGNMENT(=)           => COPY_RANDOM_GENERATOR
      !
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_INT_SUB_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_DBL_SUB_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_SGL_SUB_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_INT_PURE_SUB_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_DBL_PURE_SUB_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_SNG_PURE_SUB_LCG 
      PROCEDURE,            PRIVATE:: COPY_RANDOM_GENERATOR
      !
      FINAL:: FINAL_DESTROY_LCG
      !
  END TYPE
  !
  TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_COIN
  TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BINARY
  !
  INTRINSIC:: RANDOM_SEED, RANDOM_NUMBER, DATE_AND_TIME
  INTRINSIC:: INT
  INTRINSIC:: REAL
  !
  INTEGER(i64), PARAMETER:: Default_seed =   105155045_i64  !Random number I made up  -- Only used by PURE routines if the SEED is not yet initialized
  !	
  CONTAINS
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SET_FORTRAN_SEED(SEED)
     INTEGER, OPTIONAL, INTENT(IN):: SEED
     !
     TYPE(RANDOM_GENERATOR):: LCG
     !
     INTEGER, DIMENSION(:),ALLOCATABLE:: S
     INTEGER:: I, DIM
     !
     CALL RANDOM_SEED (SIZE = DIM)
     ALLOCATE(S(DIM))
     !
     IF(PRESENT(SEED)) THEN
                       S(:) = SEED
     ELSE
                       S(1) = NEG
     END IF
     !
     IF(S(1) < ONE) THEN
         !
         DO I=ONE, DIM;  S(I) = LCG%GEN_INT()
         END DO
     END IF
     !
     CALL RANDOM_SEED (PUT = S)
     !
     DEALLOCATE(S)
     !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE INITIALIZE_LCG(LCG, ID, SEED)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    CLASS(*), OPTIONAL,      INTENT(IN   ):: ID    ! If returned then makes SEED unique to ID (Disables odd-even random shift)
    CLASS(*), OPTIONAL,      INTENT(IN   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    !
    CALL DESTROY_LCG(LCG)
    !
    CALL SET_SEED_LCG(LCG, ID, SEED)
    !  
  END SUBROUTINE
  ! 
  PURE ELEMENTAL SUBROUTINE DESTROY_LCG(LCG)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    !
    LCG%m    = 2147483647_i64
    LCG%a    = 1103515245_i64
    LCG%c    =      12345_i64
    LCG%seed = LONG_NEG         !Flag to indicate seed is not set
    !  
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE COPY_RANDOM_GENERATOR(LCG_OUT,LCG_IN)
    CLASS(RANDOM_GENERATOR), INTENT(IN   ):: LCG_IN
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG_OUT
    !
    LCG_OUT%m    = LCG_IN%m   
    LCG_OUT%a    = LCG_IN%a   
    LCG_OUT%c    = LCG_IN%c   
    LCG_OUT%seed = LCG_IN%seed
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    INTEGER(i32),            INTENT(  OUT):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) LCG%seed = Default_seed
    !
    ASSOCIATE(x => LCG%seed, m => LCG%m, a  => LCG%a, c => LCG%c)
        !
        !X1 = MODULO(X*a + c, m+LONG_ONE)
        X = IAND(X*a + c, m)             !Because M is a power of 2 minus 1, can use IAND in place of MOD()
        !
        IF(X < LONG_ZER) X = LONG_NEG*X
        !
        
        IF       (X > LONG_inf_I) THEN
        DO WHILE (X > LONG_inf_I)
                            X = IAND(X*a + c, m)
                            !
                            IF(X < LONG_ZER) X = LONG_NEG*X
        END DO
        END IF
        !
        IRAND = INT(X, i32)
        !                          seed    *     c     -   a
        IF(X <= LONG_ONE) X = Default_seed * 12345_i64 - 1103515245_i64 !Re-init random seed
        !
    END ASSOCIATE
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE GENERATE_RANDOM_INT_SUB_LCG(LCG, IRAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    INTEGER(i32),            INTENT(  OUT):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !  
  END SUBROUTINE
  !	
  FUNCTION GENERATE_RANDOM_INT_LCG(LCG)  RESULT(IRAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    INTEGER:: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_DBL_PURE_SUB_LCG(LCG, RAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    REAL(DBL),               INTENT(  OUT):: RAND
    INTEGER(i32):: IRAND
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    RAND = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
    !
    IF(RAND > UNO) RAND = UNO
    IF(RAND <  DZ) RAND = DZ
    !  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE GENERATE_RANDOM_DBL_SUB_LCG(LCG, RAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    REAL(DBL),               INTENT(  OUT):: RAND
    INTEGER(i32):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    RAND = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
    !
    IF(RAND > UNO) RAND = UNO
    IF(RAND <  DZ) RAND = DZ
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_DBL_LCG(LCG)  RESULT(RAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    REAL(DBL):: RAND
    !
    INTEGER(i32):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    RAND = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
    !
    IF(RAND > UNO) RAND = UNO
    IF(RAND <  DZ) RAND = DZ
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_SNG_PURE_SUB_LCG(LCG, RAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    REAL(SNG),               INTENT(  OUT):: RAND
    REAL(DBL)     :: DRAND
    INTEGER(i32):: IRAND
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    DRAND = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
    !
    IF(DRAND > UNO) DRAND = UNO
    IF(DRAND <  DZ) DRAND = DZ
    !
    RAND = REAL(DRAND, SNG)
    !  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE GENERATE_RANDOM_SGL_SUB_LCG(LCG, RAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    REAL(SNG),               INTENT(  OUT):: RAND
    REAL(DBL)     :: DRAND
    INTEGER(i32):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    DRAND = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
    !
    IF(DRAND > UNO) DRAND = UNO
    IF(DRAND <  DZ) DRAND = DZ
    !
    RAND = REAL(DRAND, SNG)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_SNG_LCG(LCG)  RESULT(RAND)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    REAL(SNG):: RAND
    REAL(DBL)     :: DRAND
    INTEGER(i32):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    DRAND = REAL(IRAND, SNG) / REAL( LCG%m+LONG_ONE, SNG )
    !
    IF(DRAND > UNO) DRAND = UNO
    IF(DRAND <  DZ) DRAND = DZ
    !
    RAND = REAL(DRAND, SNG)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_COIN_LCG(LCG)  RESULT(COIN)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    LOGICAL:: COIN
    REAL(DBL)   :: DRAND
    INTEGER(i32):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    DRAND = REAL(IRAND, SNG) / REAL( LCG%m+LONG_ONE, SNG )
    !
    IF(DRAND > UNO) DRAND = UNO
    IF(DRAND <  DZ) DRAND = DZ
    !
    COIN = DRAND <  HALF
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_BINARY_LCG(LCG)  RESULT(BIN)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    INTEGER:: BIN
    REAL(DBL)   :: DRAND
    INTEGER(i32):: IRAND
    !
    IF(LCG%seed <= LONG_ONE) CALL SET_SEED_LCG(LCG) 
    !
    CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
    !
    DRAND = REAL(IRAND, SNG) / REAL( LCG%m+LONG_ONE, SNG )
    !
    IF(DRAND > UNO) DRAND = UNO
    IF(DRAND <  DZ) DRAND = DZ
    !
    IF( DRAND <  HALF ) THEN; BIN = ONE
    ELSE;                     BIN = Z
    END IF
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  !!!PURE SUBROUTINE SHUFFLE_INT_LCG(LCG, DIM, VEC)  
  !!! !Modified SATTOLO Shuffle Method  
  !!!  CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
  !!!  INTEGER,                 INTENT(IN   ):: DIM
  !!!  INTEGER,DIMENSION(DIM), INTENT(INOUT):: VEC
  !!!  INTEGER:: I,J
  !!!  INTEGER:: TMP
  !!!  REAL(SNG):: RND 
  !!!  !double x = (int)( Math.random()*((max-min)+1) )+min;
  !!!  DO I=DIM, TWO, NEG
  !!!      !
  !!!      CALL GENERATE_RANDOM_INT_PURE_SUB_LCG(LCG, IRAND)
  !!!      !
  !!!      RND = RND * REAL(I-ONE, SNG) + 1_SNG
  !!!      !
  !!!      J = NINT(RND) 
  !!!      !
  !!!      TMP    = VEC(I)
  !!!      VEC(I) = VEC(J)
  !!!      VEC(J) = TMP
  !!!      !
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SET_SEED_LCG(LCG, ID, SEED, M, A, C)
    CLASS(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    CLASS(*), OPTIONAL,      INTENT(IN   ):: ID    ! If returned then makes SEED unique to ID (Disables odd-even random shift)
    CLASS(*), OPTIONAL,      INTENT(IN   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    CLASS(*), OPTIONAL,      INTENT(IN   ):: M     ! Set range of random numbers, must be a power of 2 minus one (default is 2^31 - 1)
    CLASS(*), OPTIONAL,      INTENT(IN   ):: A, C  ! Overrudes default va
    !
    INTEGER(i64):: YER, SEC, MS, CNT, PID
    !
    INTEGER, DIMENSION(EIGHT):: T
    INTEGER:: I, J
    !
    CHARACTER(EIGHT):: BYT
    CHARACTER(TWO  ):: TXT
    !
    IF(PRESENT(ID)) THEN
         SELECT TYPE(ID)
         TYPE IS (INTEGER(i32)); PID = INT(ID,i64)
         TYPE IS (INTEGER(i64)); PID = ID
         END SELECT
         !
         IF(PID < LONG_ONE) PID = LONG_ONE
    ELSE
            PID = LONG_ONE
    END IF
    !
    IF(PRESENT(M)) THEN
         SELECT TYPE(M)
         TYPE IS (INTEGER(i32)); LCG%M = INT(M,i64)
         TYPE IS (INTEGER(i64)); LCG%M = M
         END SELECT
         !
         !CNT = LOG2(LCG%M)
         !
         CNT = 1024_i64;   IF(LCG%M < CNT) LCG%M = CNT  !should be at least 2^10 = 1024
         !
         LCG%M = LCG%M + LONG_ONE
         !
         CNT   = LONG_NEG
         DO WHILE (LCG%M > 0)
                           CNT = CNT + LONG_ONE
                           LCG%M = SHIFTR( LCG%M, LONG_ONE )  !Drop out the right most bit
         ENDDO
         !
         LCG%M = LONG_TWO**CNT - LONG_ONE   !Ensure it is a power of 2
    END IF
    !
    IF(PRESENT(A)) THEN
         SELECT TYPE(A)
         TYPE IS (INTEGER(i32)); LCG%A = INT(A,i64)
         TYPE IS (INTEGER(i64)); LCG%A = A
         END SELECT
    END IF
    !
    IF(PRESENT(C)) THEN
         SELECT TYPE(C)
         TYPE IS (INTEGER(i32)); LCG%C = INT(C,i64)
         TYPE IS (INTEGER(i64)); LCG%C = C
         END SELECT
    END IF
    !
    LCG%seed = LONG_NEG
    !
    IF(PRESENT(SEED)) THEN
         SELECT TYPE(SEED)
         TYPE IS (INTEGER(i32)); LCG%seed = INT(SEED,i64)
         TYPE IS (INTEGER(i64)); LCG%seed = SEED
         END SELECT
    END IF
    !
    IF(LCG%seed <= LONG_ONE) THEN
        ASSOCIATE( X => LCG%seed )
           CALL DATE_AND_TIME(VALUES = T)  !GET CURRENT TIME VALUES
           !
           YER =       INT(T(1), i64)
           CNT =       INT(T(2), i64)*25920000_i64   !MON
           CNT = CNT + INT(T(3), i64)*8640000_i64    !DAY
           CNT = CNT + INT(T(5), i64)*3600000_i64    !HR 
           CNT = CNT + INT(T(6), i64)*60000_i64      !MIN
           SEC =       INT(T(7), i64)*1000_i64
           MS  =       INT(T(8), i64)
           !
           X = YER*SEC*PID + SEC*MS + MS + PID
           !
           IF( BTEST(T(8), Z) .AND. PID > LONG_ONE) THEN      !MS is odd
               BYT = TRANSFER(X, BYT)                         !Shift Bytes around for fun
               !
               TXT  = BYT(1:2)
               BYT(1:2) = BYT(6:7)
               BYT(6:7) = TXT
               !
               TXT  = BYT(5:6)
               BYT(5:6) = BYT(7:8)
               BYT(7:8) = TXT
               !
               DO J=TWO, EIGHT, TWO
                   I = J - ONE
                   !
                   TXT(1:1)  = BYT(J:J)
                   BYT(J:J) = BYT(I:I)
                   BYT(I:I) = TXT(1:1)
               END DO
               !
               X = TRANSFER(BYT, X)
           END IF
           !
           IF(X < LONG_ZER) X = LONG_NEG*X
           !
           IF(X < 10000_i64) X = X*10000_i64
           !
        END ASSOCIATE
    END IF
    !  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE ELEMENTAL SUBROUTINE FINAL_DESTROY_LCG(LCG)
    TYPE(RANDOM_GENERATOR), INTENT(INOUT):: LCG
    !
    CALL DESTROY_LCG(LCG)
    ! 
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION COIN_TOSS(SEED)  RESULT(TIS_TRUE)
     !SEED<0 IS RANDOM SEED
     INTEGER,  OPTIONAL, INTENT(IN):: SEED
     LOGICAL:: TIS_TRUE
     !
     IF( .NOT. ALLOCATED(LCG_COIN) ) ALLOCATE( LCG_COIN )
     !
     IF(PRESENT(SEED)) CALL LCG_COIN%SET_PROP(SEED=SEED)
     !
     TIS_TRUE = LCG_COIN%GEN_DOUBLE() < HALF
     !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION BINARY_TOSS(SEED)  RESULT(BINARY_RESULT)
     !SEED<0 IS RANDOM SEED
     INTEGER,  OPTIONAL, INTENT(IN):: SEED
     INTEGER:: BINARY_RESULT
     !
     IF( .NOT. ALLOCATED(LCG_BINARY) ) ALLOCATE( LCG_BINARY )
     !
     IF(PRESENT(SEED)) CALL LCG_BINARY%SET_PROP(SEED=SEED)
     !
     IF( LCG_BINARY%GEN_DOUBLE() < HALF ) THEN
                      BINARY_RESULT = ONE
     ELSE
                      BINARY_RESULT = Z
     END IF
     !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE INT_SHUFFLE(VEC,SEED)  
     !Modified SATTOLO Shuffle Method  
     !SEED<0 IS RANDOM SEED
     INTEGER,DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: VEC
     INTEGER,              OPTIONAL, INTENT(IN   ):: SEED
     INTEGER:: I,J,DIM
     INTEGER:: TMP
     REAL(SNG):: RND 
     !
     IF(PRESENT(SEED)) THEN; CALL SET_FORTRAN_SEED(SEED)
     END IF
     !
     DIM  = SIZE(VEC)
     !
     DO I=DIM, TWO, NEG
         !
         CALL RANDOM_NUMBER(RND)
         !
         RND = RND * REAL(I-ONE, SNG) + 1_SNG
         !
         J = NINT(RND) 
         !
         TMP    = VEC(I)
         VEC(I) = VEC(J)
         VEC(J) = TMP
         !
     END DO
     !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE DBLE_SHUFFLE(VEC,SEED)  
     !Modified SATTOLO Shuffle Method  
     !SEED<0 IS RANDOM SEED
     REAL(DBL),DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: VEC
     INTEGER,                   OPTIONAL, INTENT(IN   ):: SEED
     INTEGER:: I,J,DIM
     REAL(DBL):: TMP
     REAL(SNG):: RND 
     !
     IF(PRESENT(SEED)) THEN; CALL SET_FORTRAN_SEED(SEED)
     END IF
     !
     DIM  = SIZE(VEC)
     !
     DO I=DIM, TWO, NEG
         !
         CALL RANDOM_NUMBER(RND)
         !
         RND = RND * REAL(I-ONE, SNG) + 1_SNG
         !
         J = NINT(RND) 
         !
         TMP    = VEC(I)
         VEC(I) = VEC(J)
         VEC(J) = TMP
         !
     END DO
     !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE REAL_SHUFFLE(VEC,SEED)  
     !Modified SATTOLO Shuffle Method  
     !SEED<0 IS RANDOM SEED
     REAL(SNG),DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: VEC
     INTEGER,                   OPTIONAL, INTENT(IN   ):: SEED
     INTEGER:: I,J,DIM
     REAL(SNG):: TMP
     REAL(SNG):: RND 
     !
     IF(PRESENT(SEED)) THEN; CALL SET_FORTRAN_SEED(SEED)
     END IF
     !
     DIM  = SIZE(VEC)
     !
     DO I=DIM, TWO, NEG
         !
         CALL RANDOM_NUMBER(RND)
         !
         RND = RND * REAL(I-ONE, SNG) + 1_SNG
         !
         J = NINT(RND) 
         !
         TMP    = VEC(I)
         VEC(I) = VEC(J)
         VEC(J) = TMP
         !
     END DO
     !
  END SUBROUTINE
  !
END MODULE
!