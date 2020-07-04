! 
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
MODULE SORT_INTERFACE ! SORT(DIM1,ARR) or SORT(DIM1,DIM2,ARR,COL)
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC SORT, SORTED, REVERSE_ORDER, SORT_BY_DIM1  !SORT IS A SUBROUTINE THAT MODFIES IN PLACE AND SORTED IS FUNCTION THAT RETURNS A NEW VECTOR/ARRAY
  !
  ! SORTING IS FROM SMALLEST TO LARGEST VALUE. 
  !                             --IF SORTING AN ARRAY SORTING IS DONE SHIFTING ALONG DIM2 (COLUMN)
  !                             --TO SORT BY ROW/DIM1 THEN USE SORT_BY_DIM1
  !
  ! SORT OPTIONS
  ! GIVEN A VECTOR X(DIM1) 
  ! SORT(DIM1, X)
  !
  ! GIVEN AN ARRAY Y(DIM1,DIM2)  THAT IS SORTED ON COLUMN "COL", WHICH IS FROM 1 to DIM2
  ! SORT(DIM1, DIM2, Y, COL)
  ! or
  ! SORT(DIM1, DIM2, Y, [COL1,COL2,...COLN])  -- SORT BY SPECIFIED COLUMNS -- NOTE: SORT(DIM1, DIM2, Y, COL)  <==> SORT(DIM1, DIM2, Y, [COL1]) WHEN COL = COL1
  !
  ! MIXED SORTING
  ! GIVEN AN VECTOR X(DIM1), Z(DIM1) 
  ! SORT(DIM1, X, Z)                  --SORTS ON X AND INCLUDES SORTING OPERATIONS ON Z, NOTE SORT(DIM1, X, Z) <==> SORT(DIM1, DIM2, Y, COL=1) WHEN Y=[X; Z]
  !
  ! MIXED SORTING
  ! GIVEN AN INTEGER VECTOR A(DIM1) AND DOUBLE PERCISION ARRAYS B(DIM1) AND C(DIM1,DIM2)
  ! WILL SORT USING INTEGER VECTOR THE DOUBLE PRECISION PART
  ! SORT(DIM1,      A, B)
  ! SORT(DIM1,DIM2, A, C)
  !
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ! TO REVERSE RESULTING ORDER USE THE FOLLOWING SUBROUTINES ARE AVAIBLE:
  !    CALL REVERSE_ORDER(DIM1, VEC)
  !    CALL REVERSE_ORDER(DIM1, DIM2, ARR)
  ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !
  ! FORMAL INTERFACES:
  !
  INTERFACE REVERSE_ORDER
    MODULE PROCEDURE REVERSE_ORDER_INT_VECTOR       !(DIM1,IVEC                )
    MODULE PROCEDURE REVERSE_ORDER_REAL_VECTOR      !(DIM1,RVEC                )
    MODULE PROCEDURE REVERSE_ORDER_DBLE_VECTOR      !(DIM1,DVEC                )
    MODULE PROCEDURE REVERSE_ORDER_INT_ARRAY_COL    !(DIM1,DIM2,IARR,[ByColumn])
    MODULE PROCEDURE REVERSE_ORDER_REAL_ARRAY_COL   !(DIM1,DIM2,RARR,[ByColumn])
    MODULE PROCEDURE REVERSE_ORDER_DBLE_ARRAY_COL   !(DIM1,DIM2,DARR,[ByColumn])
  END INTERFACE
  !
  ! MAGNITUDE is an optional LOGICAL that when present and true indicates sort is based on magnitude (ABS) of numbers
  !  
  INTERFACE SORT
    MODULE PROCEDURE SORT_INT_VECTOR       !(DIM1,IVEC,         [MAGNITUDE])
    MODULE PROCEDURE SORT_REAL_VECTOR      !(DIM1,RVEC,         [MAGNITUDE])
    MODULE PROCEDURE SORT_DBLE_VECTOR      !(DIM1,DVEC,         [MAGNITUDE])
    MODULE PROCEDURE SORT_INT_ARRAY_COL    !(DIM1,DIM2,IARR,COL,[MAGNITUDE])
    MODULE PROCEDURE SORT_REAL_ARRAY_COL   !(DIM1,DIM2,RARR,COL,[MAGNITUDE])
    MODULE PROCEDURE SORT_DBLE_ARRAY_COL   !(DIM1,DIM2,DARR,COL,[MAGNITUDE])
    MODULE PROCEDURE SORT_INT_ARRAY_COLDIM !(DIM1,DIM2,IARR,COLDIM)   -- COLDIM is vec of columns to sort by in order of preference
    MODULE PROCEDURE SORT_DBLE_ARRAY_COLDIM!(DIM1,DIM2,DARR,COLDIM)   -- COLDIM is vec of columns to sort by in order of preference
    MODULE PROCEDURE SORT_DBLE_2VECTOR     !(DIM1,DVEC,DVEC2)          -- SORT ON DVEC COLUMN AS IF MERGED ARRAY WITH ARRAY = [DVEC; DVEC2]
    MODULE PROCEDURE SORT_INT_2VECTOR      !(DIM1,IVEC,IVEC2)
    MODULE PROCEDURE SORT_INT_VECTOR_DBLE_VECTOR!(DIM1,IVEC,DVEC     ,[MAGNITUDE])
    MODULE PROCEDURE SORT_INT_VECTOR_DBLE_ARRAY !(DIM1,DIM2,IVEC,DARR,[MAGNITUDE])
    MODULE PROCEDURE SORT_DBLE_VECTOR_INT_VECTOR!(DIM1,DVEC,IVEC     ,[MAGNITUDE])
    MODULE PROCEDURE SORT_DBLE_VECTOR_INT_ARRAY !(DIM1,DIM2,DVEC,IARR,[MAGNITUDE])
    !
    MODULE PROCEDURE SORT_DBLE_VECTOR_INT_ARRAY_SORT_DIM         !(DIM1,DIM2,ARR,ARR2,SORT_DIM,[MAGNITUDE]) -- SORT_DIM IS THE MATCHING DIM FROM ARR TO ARR2 -- SORT_DIM = 1  CALLS SORT_DBLE_VECTOR_INT_ARRAY
    MODULE PROCEDURE SORT_DBLE_VECTOR_INT_ARRAY_SORT_MATCHING_DIM!(ARR,ARR2,MAGNITUDE) !GUESS SORT_DIM, IF DIM1==DIM2 THEN TREAT AS SORT_DIM = 1
    
  END INTERFACE
  !
  INTERFACE SORTED
    MODULE PROCEDURE SORTED_INT_VECTOR       !(DIM1,IVEC,         [MAGNITUDE])
    MODULE PROCEDURE SORTED_REAL_VECTOR      !(DIM1,RVEC,         [MAGNITUDE])
    MODULE PROCEDURE SORTED_DBLE_VECTOR      !(DIM1,DVEC,         [MAGNITUDE])
    MODULE PROCEDURE SORTED_INT_ARRAY_COL    !(DIM1,DIM2,IARR,COL,[MAGNITUDE])
    MODULE PROCEDURE SORTED_REAL_ARRAY_COL   !(DIM1,DIM2,RARR,COL,[MAGNITUDE])
    MODULE PROCEDURE SORTED_DBLE_ARRAY_COL   !(DIM1,DIM2,DARR,COL,[MAGNITUDE])
    MODULE PROCEDURE SORTED_INT_ARRAY_COLDIM !(DIM1,DIM2,IARR,COLDIM)   -- COLDIM is vec of columns to sort by in order of preference
    MODULE PROCEDURE SORTED_DBLE_ARRAY_COLDIM!(DIM1,DIM2,DARR,COLDIM)   -- COLDIM is vec of columns to sort by in order of preference
  END INTERFACE
  !
  INTERFACE SORT_BY_DIM1
    MODULE PROCEDURE SORT_INT_ARRAY_ROW    !(DIM1,DIM2,ARR,ROW,MAGNITUDE)
    MODULE PROCEDURE SORT_INT_ARRAY_ROWDIM !(DIM1,DIM2,ARR,ROWDIM)
  END INTERFACE
  !
  CONTAINS
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_VECTOR(DIM1,ARR,MAGNITUDE)
    INTEGER,                 INTENT(IN   ):: DIM1
    INTEGER, DIMENSION(DIM1),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,       INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, VAL, AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL=ARR(I)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J) = ARR(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
            END DO
        ELSE
            DO I=2, DIM1
                VAL =ARR(I)
                AVAL=ABS(VAL)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J) = ARR(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
            END DO
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_REAL_VECTOR(DIM1,ARR,MAGNITUDE)
    INTEGER,              INTENT(IN   ):: DIM1
    REAL(REAL32), DIMENSION(DIM1),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,    INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J
    REAL(REAL32):: VAL, AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL=ARR(I)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J) = ARR(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
            END DO
        ELSE
            DO I=2, DIM1
                VAL=ARR(I)
                AVAL = ABS(VAL)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J) = ARR(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_VECTOR(DIM1,ARR,MAGNITUDE)
    INTEGER,                          INTENT(IN   ):: DIM1
    REAL(REAL64), DIMENSION(DIM1),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,       INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J
    REAL(REAL64):: VAL, AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL=ARR(I)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J) = ARR(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
            END DO
        ELSE
            DO I=2, DIM1
                VAL  = ARR(I)
                AVAL = ABS(VAL)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J) = ARR(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_ARRAY_COL(DIM1,DIM2,ARR,COL,MAGNITUDE)
    INTEGER,                      INTENT(IN   ):: DIM1, DIM2, COL
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, K
    INTEGER, DIMENSION(DIM2):: VAL
    INTEGER:: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
                END DO
                !
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1,COL) < VAL(COL)) EXIT
                                   DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
                END DO
            END DO
        ELSE
            DO I=2, DIM1
                DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
                END DO
                AVAL = ABS(VAL(COL))
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1,COL)) < AVAL) EXIT
                                   DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
                END DO
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_ARRAY_COLDIM(DIM1,DIM2,ARR,COLDIM)
    INTEGER,                      INTENT(IN   ):: DIM1, DIM2
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    INTEGER, DIMENSION(:),        INTENT(IN   ):: COLDIM
    INTEGER:: I, J, K, M, N, P, COL
    INTEGER, DIMENSION(DIM2):: VAL
    !
    IF( DIM1 > 1 ) THEN
        COL = COLDIM(1)
        DO I=2, DIM1
            DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
            END DO
            !
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1,COL) < VAL(COL)) EXIT
                               DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                               END DO
                               J=J-1
            END DO
            !
            DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
            END DO
        END DO
        !
        CDIM: DO P=2, SIZE(COLDIM) 
            COL = COLDIM(P)
            I=1
            J=2
            DIM_SEARCH: DO WHILE (I <= DIM1)
               JSEARCH: DO WHILE (J <= DIM1)
                   DO K=1, P-1
                       IF (ARR(I,COLDIM(K)) .NE. ARR(J,COLDIM(K))) EXIT JSEARCH
                   END DO
                   J = J + 1
               END DO JSEARCH
               !
               J = J - 1
               !
               IF(J - I == 1) THEN
                                  IF (ARR(I,COL) > ARR(J,COL)) THEN
                                      DO CONCURRENT(K=1:DIM2) 
                                                       VAL(1)   = ARR(I,K)   !JUST USE VAL(1) AS TEMP VARIABLE
                                                       ARR(I,K) = ARR(J,K)
                                                       ARR(J,K) = VAL(1)
                                      END DO
                                  END IF
               ELSEIF(I.NE.J) THEN
                                  DO M=I+1, J
                                      DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(M,K)
                                      END DO
                                      N = M
                                      DO WHILE ( N > I ) !I IS THE LOWER LIMIT OF SEARCH
                                                         IF (ARR(N-1,COL) < VAL(COL)) EXIT
                                                         DO CONCURRENT(K=1:DIM2); ARR(N,K) = ARR(N-1,K)
                                                         END DO
                                                         N=N-1
                                      END DO
                                      DO CONCURRENT(K=1:DIM2); ARR(N,K) = VAL(K)
                                      END DO
                                  END DO
               END IF
               I = J + 1
               J = I + 1
            END DO DIM_SEARCH
        END DO CDIM
    END IF
    !
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_REAL_ARRAY_COL(DIM1,DIM2,ARR,COL,MAGNITUDE)
    INTEGER,                       INTENT(IN   ):: DIM1, DIM2, COL
    REAL(REAL32),    DIMENSION(DIM1,DIM2), INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,             INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, K
    REAL(REAL32), DIMENSION(DIM2):: VAL
    REAL(REAL32):: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
                END DO
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1,COL) < VAL(COL)) EXIT
                                   DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
                END DO
            END DO
        ELSE
            DO I=2, DIM1
                DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
                END DO
                AVAL = ABS(VAL(COL))
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1,COL)) < AVAL) EXIT
                                   DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
                END DO
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_ARRAY_COL(DIM1,DIM2,ARR,COL,MAGNITUDE)
    INTEGER,                               INTENT(IN   ):: DIM1, DIM2, COL
    REAL(REAL64), DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    LOGICAL,          OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, K
    REAL(REAL64), DIMENSION(DIM2):: VAL
    REAL(REAL64):: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
                END DO
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1,COL) < VAL(COL)) EXIT
                                   DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
                END DO
            END DO
        ELSE
            DO I=2, DIM1
                DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
                END DO
                AVAL = ABS(VAL(COL))
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1,COL)) < AVAL) EXIT
                                   DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
                END DO
            END DO
        END IF
    END IF
  END SUBROUTINE   
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_ARRAY_COLDIM(DIM1,DIM2,ARR,COLDIM)
    INTEGER,                               INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    INTEGER, DIMENSION(:),                 INTENT(IN   ):: COLDIM
    INTEGER:: I, J, K, M, N, P, COL
    REAL(REAL64), DIMENSION(DIM2):: VAL
    !
    IF( DIM1 > 1 ) THEN
        COL = COLDIM(1)
        DO I=2, DIM1
            DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(I,K)
            END DO
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1,COL) < VAL(COL)) EXIT
                               DO CONCURRENT(K=1:DIM2); ARR(J,K) = ARR(J-1,K)
                               END DO
                               J=J-1
            END DO
            DO CONCURRENT(K=1:DIM2); ARR(J,K) = VAL(K)
            END DO
        END DO
        !
        CDIM: DO P=2, SIZE(COLDIM) 
            COL = COLDIM(P)
            I=1
            J=2
            DIM_SEARCH: DO WHILE (I <= DIM1)
               JSEARCH: DO WHILE (J <= DIM1)
                   DO K=1, P-1
                       IF (ARR(I,COLDIM(K)) .NE. ARR(J,COLDIM(K))) EXIT JSEARCH
                   END DO
                   J = J + 1
               END DO JSEARCH
               !
               J = J - 1
               !
               IF(J - I == 1) THEN
                                  IF (ARR(I,COL) > ARR(J,COL)) THEN
                                      DO CONCURRENT(K=1:DIM2) 
                                                       VAL(1)   = ARR(I,K)   !JUST USE VAL(1) AS TEMP VARIABLE
                                                       ARR(I,K) = ARR(J,K)
                                                       ARR(J,K) = VAL(1)
                                      END DO
                                  END IF
               ELSEIF(I.NE.J) THEN
                                  DO M=I+1, J
                                      DO CONCURRENT(K=1:DIM2); VAL(K)=ARR(M,K)
                                      END DO
                                      N = M
                                      DO WHILE ( N > I ) !I IS THE LOWER LIMIT OF SEARCH
                                                         IF (ARR(N-1,COL) < VAL(COL)) EXIT
                                                         DO CONCURRENT(K=1:DIM2); ARR(N,K) = ARR(N-1,K)
                                                         END DO
                                                         N=N-1
                                      END DO
                                      DO CONCURRENT(K=1:DIM2); ARR(N,K) = VAL(K)
                                      END DO
                                  END DO
               END IF
               I = J + 1
               J = I + 1
            END DO DIM_SEARCH
        END DO CDIM
    END IF
    !
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_2VECTOR(DIM1,ARR,ARR2)
    INTEGER,              INTENT(IN):: DIM1
    REAL(REAL64), DIMENSION(DIM1),INTENT(INOUT):: ARR,ARR2
    INTEGER:: I, J
    REAL(REAL64):: VAL, VAL2
    !
    IF( DIM1 > 1 ) THEN
        DO I=2, DIM1
            VAL =ARR(I)
            VAL2=ARR2(I)
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1) < VAL) EXIT
                               ARR(J)  = ARR(J-1)
                               ARR2(J) = ARR2(J-1)
                               J=J-1
            END DO
            ARR(J) = VAL
            ARR2(J) = VAL2
        END DO
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_2VECTOR(DIM1,ARR,ARR2)
    INTEGER,                 INTENT(IN   ):: DIM1
    INTEGER, DIMENSION(DIM1),INTENT(INOUT):: ARR,ARR2
    INTEGER:: I, J
    INTEGER:: VAL, VAL2
    !
    IF( DIM1 > 1 ) THEN
        DO I=2, DIM1
            VAL =ARR(I)
            VAL2=ARR2(I)
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1) < VAL) EXIT
                               ARR(J)  = ARR(J-1)
                               ARR2(J) = ARR2(J-1)
                               J=J-1
            END DO
            ARR(J) = VAL
            ARR2(J) = VAL2
        END DO
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_VECTOR_DBLE_VECTOR(DIM1,ARR,ARR2,MAGNITUDE)
    INTEGER,                      INTENT(IN   ):: DIM1
    INTEGER,      DIMENSION(DIM1),INTENT(INOUT):: ARR
    REAL(REAL64), DIMENSION(DIM1),INTENT(INOUT):: ARR2
    LOGICAL,      OPTIONAL,       INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J
    INTEGER:: VAL
    REAL(REAL64):: VAL2
    INTEGER:: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL =ARR(I)
                VAL2=ARR2(I)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   ARR2(J) = ARR2(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
                ARR2(J) = VAL2
            END DO
        ELSE
            DO I=2, DIM1
                VAL =ARR(I)
                VAL2=ARR2(I)
                !
                AVAL = ABS(VAL)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   ARR2(J) = ARR2(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
                ARR2(J) = VAL2
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_VECTOR_DBLE_ARRAY(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    INTEGER,                           INTENT(IN   ):: DIM1,DIM2
    INTEGER,      DIMENSION(DIM1),     INTENT(INOUT):: ARR
    REAL(REAL64), DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR2
    LOGICAL,      OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, K
    INTEGER:: VAL
    REAL(REAL64), DIMENSION(DIM2):: VAL2
    INTEGER:: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL = ARR(I)
                DO CONCURRENT(K=1:DIM2); VAL2(K)=ARR2(I,K)
                END DO
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   DO CONCURRENT(K=1:DIM2); ARR2(J,K) = ARR2(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                ARR(J) = VAL
                DO CONCURRENT(K=1:DIM2); ARR2(J,K) = VAL2(K)
                END DO
            END DO
        ELSE
            DO I=2, DIM1
                VAL  = ARR(I)
                AVAL = ABS(VAL)
                DO CONCURRENT(K=1:DIM2); VAL2(K)=ARR2(I,K)
                END DO
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   DO CONCURRENT(K=1:DIM2); ARR2(J,K) = ARR2(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                ARR(J) = VAL
                DO CONCURRENT(K=1:DIM2); ARR2(J,K) = VAL2(K)
                END DO
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_VECTOR_INT_VECTOR(DIM1,ARR,ARR2,MAGNITUDE)
    INTEGER,                      INTENT(IN   ):: DIM1
    REAL(REAL64), DIMENSION(DIM1),INTENT(INOUT):: ARR
    INTEGER,      DIMENSION(DIM1),INTENT(INOUT):: ARR2
    LOGICAL,      OPTIONAL,       INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J
    REAL(REAL64):: VAL
    INTEGER:: VAL2
    REAL(REAL64):: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL =ARR(I)
                VAL2=ARR2(I)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   ARR2(J) = ARR2(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
                ARR2(J) = VAL2
            END DO
        ELSE
            DO I=2, DIM1
                VAL =ARR(I)
                VAL2=ARR2(I)
                !
                AVAL = ABS(VAL)
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   ARR2(J) = ARR2(J-1)
                                   J=J-1
                END DO
                ARR(J) = VAL
                ARR2(J) = VAL2
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_VECTOR_INT_ARRAY(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    INTEGER,                           INTENT(IN   ):: DIM1,DIM2
    REAL(REAL64), DIMENSION(DIM1),     INTENT(INOUT):: ARR
    INTEGER,      DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR2
    LOGICAL,      OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, K
    REAL(REAL64):: VAL
    INTEGER, DIMENSION(DIM2):: VAL2
    REAL(REAL64):: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM1 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM1
                VAL = ARR(I)
                DO CONCURRENT(K=1:DIM2); VAL2(K)=ARR2(I,K)
                END DO
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(J-1) < VAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   DO CONCURRENT(K=1:DIM2); ARR2(J,K) = ARR2(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                ARR(J) = VAL
                DO CONCURRENT(K=1:DIM2); ARR2(J,K) = VAL2(K)
                END DO
            END DO
        ELSE
            DO I=2, DIM1
                VAL  = ARR(I)
                AVAL = ABS(VAL)
                DO CONCURRENT(K=1:DIM2); VAL2(K)=ARR2(I,K)
                END DO
                J=I
                DO WHILE ( J > 1 )
                                   IF (ABS(ARR(J-1)) < AVAL) EXIT
                                   ARR(J)  = ARR(J-1)
                                   DO CONCURRENT(K=1:DIM2); ARR2(J,K) = ARR2(J-1,K)
                                   END DO
                                   J=J-1
                END DO
                ARR(J) = VAL
                DO CONCURRENT(K=1:DIM2); ARR2(J,K) = VAL2(K)
                END DO
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_DBLE_VECTOR_INT_ARRAY_SORT_DIM(DIM1,DIM2,ARR,ARR2,SORT_DIM, MAGNITUDE)
    ! SORT_DIM = 1 => ARR DIM IS SAME AS ARR2 FIRST DIM  --SHIFT FIRST DIM WITH ARR
    ! SORT_DIM = 2 => ARR DIM IS SAME AS ARR2 SECOND DIM --SHIFT   2nd DIM WITH ARR
    INTEGER,                           INTENT(IN   ):: DIM1,DIM2
    REAL(REAL64), DIMENSION(DIM1),     INTENT(INOUT):: ARR
    INTEGER,      DIMENSION(:,:),      INTENT(INOUT):: ARR2
    INTEGER,                           INTENT(IN   ):: SORT_DIM
    LOGICAL,      OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    CONTIGUOUS:: ARR2
    !
    IF(SORT_DIM == 1) THEN
        CALL SORT_DBLE_VECTOR_INT_ARRAY(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    ELSE
        CALL SORT_DBLE_VECTOR_INT_ARRAY_SORT_DIM2(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    END IF
  END SUBROUTINE 
  !
  PURE SUBROUTINE SORT_DBLE_VECTOR_INT_ARRAY_SORT_MATCHING_DIM(ARR,ARR2,MAGNITUDE)
    ! AUTO-DETERMINE MATCHING ARR WITH ARR2 DIM. IF TIME THEN USE SORT_DIM = 1
    REAL(REAL64), DIMENSION(:),  CONTIGUOUS, INTENT(INOUT):: ARR
    INTEGER,      DIMENSION(:,:),CONTIGUOUS, INTENT(INOUT):: ARR2
    LOGICAL,      OPTIONAL,                  INTENT(IN   ):: MAGNITUDE
    INTEGER:: DIM1,DIM2
    !
    DIM1 = SIZE(ARR, 1)
    DIM2 = SIZE(ARR2,1)
    !
    IF(DIM1 == DIM2) THEN
        DIM2 = SIZE(ARR2,2)
        CALL SORT_DBLE_VECTOR_INT_ARRAY(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    ELSE
        CALL SORT_DBLE_VECTOR_INT_ARRAY_SORT_DIM2(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    END IF
  END SUBROUTINE
  !
  PURE SUBROUTINE SORT_DBLE_VECTOR_INT_ARRAY_SORT_DIM2(DIM1,DIM2,ARR,ARR2,MAGNITUDE)
    INTEGER,                           INTENT(IN   ):: DIM1,DIM2
    REAL(REAL64), DIMENSION(DIM1),     INTENT(INOUT):: ARR
    INTEGER,      DIMENSION(DIM2, DIM1),INTENT(INOUT):: ARR2
    LOGICAL,      OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    !
    INTEGER:: I, J, K
    REAL(REAL64):: VAL
    INTEGER, DIMENSION(DIM2):: VAL2
    REAL(REAL64):: AVAL
    LOGICAL:: NO_ABS
    !
    NO_ABS = .TRUE.
    IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
    !
    IF(NO_ABS) THEN
        DO I=2, DIM1
            VAL = ARR(I)
            DO CONCURRENT(K=1:DIM2); VAL2(K)=ARR2(K,I)
            END DO
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1) < VAL) EXIT
                               ARR(J)  = ARR(J-1)
                               DO CONCURRENT(K=1:DIM2); ARR2(K,J) = ARR2(K,J-1)
                               END DO
                               J=J-1
            END DO
            ARR(J) = VAL
            DO CONCURRENT(K=1:DIM2); ARR2(K,J) = VAL2(K)
            END DO
        END DO
    ELSE
        DO I=2, DIM1
            VAL  = ARR(I)
            AVAL = ABS(VAL)
            DO CONCURRENT(K=1:DIM2); VAL2(K)=ARR2(K,I)
            END DO
            J=I
            DO WHILE ( J > 1 )
                               IF (ABS(ARR(J-1)) < AVAL) EXIT
                               ARR(J)  = ARR(J-1)
                               DO CONCURRENT(K=1:DIM2); ARR2(K,J) = ARR2(K,J-1)
                               END DO
                               J=J-1
            END DO
            ARR(J) = VAL
            DO CONCURRENT(K=1:DIM2); ARR2(K,J) = VAL2(K)
            END DO
        END DO
    END IF
  END SUBROUTINE 
  !
  !######################################################################   
  !## SORT_BY_DIM1 FUNCTIONS ==> SORTED 
  !######################################################################  
  !
  PURE SUBROUTINE SORT_INT_ARRAY_ROW(DIM1,DIM2,ARR,ROW,MAGNITUDE)
    INTEGER,                      INTENT(IN   ):: DIM1, DIM2, ROW
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,            INTENT(IN   ):: MAGNITUDE
    INTEGER:: I, J, K
    INTEGER, DIMENSION(DIM1):: VAL
    INTEGER:: AVAL
    LOGICAL:: NO_ABS
    !
    IF( DIM2 > 1 ) THEN
        NO_ABS = .TRUE.
        IF(PRESENT(MAGNITUDE)) NO_ABS = .NOT. MAGNITUDE
        !
        IF(NO_ABS) THEN
            DO I=2, DIM2
                VAL=ARR(:,I)
                !
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(ROW,J-1) < VAL(ROW)) EXIT
                                   ARR(:,J) = ARR(:,J-1)
                                   J=J-1
                END DO
                ARR(:,J) = VAL
            END DO
        ELSE
            DO I=2, DIM2
                VAL=ARR(:,I)
                AVAL = ABS(VAL(ROW))
                !
                J=I
                DO WHILE ( J > 1 )
                                   IF (ARR(ROW,J-1) < AVAL) EXIT
                                   ARR(:,J) = ARR(:,J-1)
                                   J=J-1
                END DO
                ARR(:,J) = VAL
            END DO
        END IF
    END IF
  END SUBROUTINE 
  !
  !######################################################################    
  !
  PURE SUBROUTINE SORT_INT_ARRAY_ROWDIM(DIM1,DIM2,ARR,ROWDIM)
    INTEGER,                      INTENT(IN   ):: DIM1, DIM2
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    INTEGER, DIMENSION(:),        INTENT(IN   ):: ROWDIM
    INTEGER:: I, J, K, M, N, P, ROW
    INTEGER, DIMENSION(DIM1):: VAL
    !
    IF( DIM2 > 1 ) THEN
        ROW = ROWDIM(1)
        DO I=2, DIM1
            VAL=ARR(:,I)
            !
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(ROW,J-1) < VAL(ROW)) EXIT
                               ARR(:,J) = ARR(:,J-1)
                               J=J-1
            END DO
            !
            ARR(:,J) = VAL
        END DO
        !
        CDIM: DO P=2, SIZE(ROWDIM) 
            ROW = ROWDIM(P)
            I=1
            J=2
            DIM_SEARCH: DO WHILE (I <= DIM2)
               JSEARCH: DO WHILE (J <= DIM2)
                   DO K=1, P-1
                       IF (ARR(ROWDIM(K),I) .NE. ARR(ROWDIM(K),J)) EXIT JSEARCH
                   END DO
                   J = J + 1
               END DO JSEARCH
               !
               J = J - 1
               !
               IF(J - I == 1) THEN
                                  IF (ARR(ROW,I) > ARR(ROW,J)) THEN
                                      DO CONCURRENT(K=1:DIM1) 
                                                       VAL(1)   = ARR(K,I)   !JUST USE VAL(1) AS TEMP VARIABLE
                                                       ARR(K,I) = ARR(K,J)
                                                       ARR(K,J) = VAL(1)
                                      END DO
                                  END IF
               ELSEIF(I.NE.J) THEN
                                  DO M=I+1, J
                                      VAL=ARR(:,M)
                                      N = M
                                      DO WHILE ( N > I ) !I IS THE LOWER LIMIT OF SEARCH
                                                         IF (ARR(ROW,N-1) < VAL(ROW)) EXIT
                                                         ARR(:,N) = ARR(:,N-1)
                                                         N=N-1
                                      END DO
                                      ARR(:,N) = VAL
                                  END DO
               END IF
               I = J + 1
               J = I + 1
            END DO DIM_SEARCH
        END DO CDIM
    END IF
    !
  END SUBROUTINE 
  !
  !######################################################################   
  !## SORT FUNCTIONS ==> SORTED 
  !######################################################################  
  !
  PURE FUNCTION SORTED_INT_VECTOR(DIM1,IVEC,MAGNITUDE) RESULT(SRT)
    INTEGER,                 INTENT(IN):: DIM1
    INTEGER, DIMENSION(DIM1),INTENT(IN):: IVEC
    LOGICAL, OPTIONAL,       INTENT(IN):: MAGNITUDE
    INTEGER, DIMENSION(DIM1):: SRT
    !
    SRT = IVEC
    CALL SORT_INT_VECTOR(DIM1, SRT, MAGNITUDE)
    !
  END FUNCTION
  !
  PURE FUNCTION SORTED_REAL_VECTOR(DIM1,RVEC,MAGNITUDE) RESULT(SRT) 
    INTEGER,                  INTENT(IN):: DIM1
    REAL(REAL32),    DIMENSION(DIM1), INTENT(IN):: RVEC
    LOGICAL, OPTIONAL,        INTENT(IN):: MAGNITUDE
    REAL(REAL32), DIMENSION(DIM1):: SRT
    !
    SRT = RVEC
    CALL SORT_REAL_VECTOR(DIM1, SRT, MAGNITUDE)
    !
  END FUNCTION
  !
  PURE FUNCTION SORTED_DBLE_VECTOR(DIM1,DVEC,MAGNITUDE) RESULT(SRT) 
    INTEGER,                      INTENT(IN):: DIM1
    REAL(REAL64), DIMENSION(DIM1),INTENT(IN):: DVEC
    LOGICAL,      OPTIONAL,       INTENT(IN):: MAGNITUDE
    REAL(REAL64), DIMENSION(DIM1):: SRT
    !
    SRT = DVEC
    CALL SORT_DBLE_VECTOR(DIM1, SRT, MAGNITUDE)
    !
  END FUNCTION
  !
  PURE FUNCTION SORTED_INT_ARRAY_COL(DIM1,DIM2,IARR,COL,MAGNITUDE) RESULT(SRT) 
    INTEGER,                      INTENT(IN):: DIM1, DIM2, COL
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(IN):: IARR
    LOGICAL, OPTIONAL,            INTENT(IN):: MAGNITUDE
    INTEGER, DIMENSION(DIM1,DIM2):: SRT
    !
    SRT = IARR
    CALL SORT_INT_ARRAY_COL(DIM1,DIM2,SRT,COL,MAGNITUDE)
  END FUNCTION
  !
  PURE FUNCTION SORTED_REAL_ARRAY_COL(DIM1,DIM2,RARR,COL,MAGNITUDE) RESULT(SRT) 
    INTEGER,                      INTENT(IN):: DIM1, DIM2, COL
    REAL(REAL32),    DIMENSION(DIM1,DIM2),INTENT(IN):: RARR
    LOGICAL, OPTIONAL,            INTENT(IN):: MAGNITUDE
    REAL(REAL32), DIMENSION(DIM1,DIM2):: SRT
    !
    SRT = RARR
    CALL SORT_REAL_ARRAY_COL(DIM1,DIM2,SRT,COL,MAGNITUDE)
    !
  END FUNCTION
  !
  PURE FUNCTION SORTED_DBLE_ARRAY_COL(DIM1,DIM2,DARR,COL,MAGNITUDE) RESULT(SRT) 
    INTEGER,                           INTENT(IN):: DIM1, DIM2, COL
    REAL(REAL64), DIMENSION(DIM1,DIM2),INTENT(IN):: DARR
    LOGICAL,      OPTIONAL,            INTENT(IN):: MAGNITUDE
    REAL(REAL64), DIMENSION(DIM1,DIM2):: SRT
    !
    SRT = DARR
    CALL SORT_DBLE_ARRAY_COL(DIM1,DIM2,SRT,COL,MAGNITUDE)
    !
  END FUNCTION
  !
  PURE FUNCTION SORTED_INT_ARRAY_COLDIM(DIM1,DIM2,IARR,COLDIM) RESULT(SRT) !-- COLDIM is vec of columns to sort by in order of preference
    INTEGER,                      INTENT(IN):: DIM1, DIM2
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(IN):: IARR
    INTEGER, DIMENSION(:),        INTENT(IN):: COLDIM
    INTEGER, DIMENSION(DIM1,DIM2):: SRT
    !
    SRT = IARR
    CALL SORT_INT_ARRAY_COLDIM(DIM1,DIM2,SRT,COLDIM)
    !
  END FUNCTION
  !
  PURE FUNCTION SORTED_DBLE_ARRAY_COLDIM(DIM1,DIM2,DARR,COLDIM) RESULT(SRT) !-- COLDIM is vec of columns to sort by in order of preference
    INTEGER,                           INTENT(IN):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2),INTENT(IN):: DARR
    INTEGER, DIMENSION(:),             INTENT(IN):: COLDIM
    REAL(REAL64), DIMENSION(DIM1,DIM2):: SRT
    !
    SRT = DARR
    CALL SORT_DBLE_ARRAY_COLDIM(DIM1,DIM2,SRT,COLDIM)
    !
  END FUNCTION
  !
  !######################################################################   
  !## REVERSE ORDER SUBROUTINES
  !######################################################################  
  !
  PURE SUBROUTINE REVERSE_ORDER_INT_VECTOR(DIM1,ARR)
    INTEGER,                 INTENT(IN   ):: DIM1
    INTEGER, DIMENSION(DIM1),INTENT(INOUT):: ARR
    INTEGER:: I, J, HALF_DIM
    INTEGER:: VAL
    !
    IF(DIM1 > 1) THEN
       !
       J = DIM1
       HALF_DIM = DIM1/2
       !
       DO I=1, HALF_DIM
           VAL    = ARR(I)
           ARR(I) = ARR(J)
           ARR(J) = VAL
           J = J - 1
       END DO
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_REAL_VECTOR(DIM1,ARR)
    INTEGER,                INTENT(IN   ):: DIM1
    REAL(REAL32),   DIMENSION(DIM1),INTENT(INOUT):: ARR
    INTEGER:: I, J, HALF_DIM
    REAL(REAL32):: VAL
    !
    IF(DIM1 > 1) THEN
       !
       J = DIM1
       HALF_DIM = DIM1/2
       !
       DO I=1, HALF_DIM
           VAL    = ARR(I)
           ARR(I) = ARR(J)
           ARR(J) = VAL
           J = J - 1
       END DO
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_DBLE_VECTOR(DIM1,ARR)
    INTEGER,                          INTENT(IN   ):: DIM1
    REAL(REAL64), DIMENSION(DIM1),INTENT(INOUT):: ARR
    INTEGER:: I, J, HALF_DIM
    REAL(REAL64):: VAL
    !
    IF(DIM1 > 1) THEN
       !
       J = DIM1
       HALF_DIM = DIM1/2
       !
       DO I=1, HALF_DIM
           VAL    = ARR(I)
           ARR(I) = ARR(J)
           ARR(J) = VAL
           J = J - 1
       END DO
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_INT_ARRAY_COL (DIM1,DIM2,ARR,BYCOLUMN)
    INTEGER,                      INTENT(IN   ):: DIM1, DIM2
    INTEGER, DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,            INTENT(IN   ):: BYCOLUMN
    INTEGER:: I, J, K, HALF_DIM
    INTEGER:: VAL
    LOGICAL:: ByROW
    !
    ByROW = .TRUE.
    IF(PRESENT(BYCOLUMN)) ByROW = .NOT. BYCOLUMN
    !
    IF(BYROW) THEN
        J = DIM1
        HALF_DIM = DIM1/2
        !
        DO K=1, DIM2
            DO I=1, HALF_DIM
                VAL      = ARR(I,K)
                ARR(I,K) = ARR(J,K)
                ARR(J,K) = VAL
                J = J - 1
            END DO
        END DO
    ELSE
        BLOCK
            INTEGER,DIMENSION(DIM1):: TMP
            J = DIM2
            HALF_DIM = DIM2/2
            !
            DO I=1, HALF_DIM
                TMP = ARR(:,I)
                ARR(:,I) = ARR(:,J)
                ARR(:,J) = TMP
                J = J - 1
            END DO
        END BLOCK
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_REAL_ARRAY_COL(DIM1,DIM2,ARR,BYCOLUMN)
    INTEGER,                      INTENT(IN   ):: DIM1, DIM2
    REAL(REAL32),    DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,            INTENT(IN   ):: BYCOLUMN
    INTEGER:: I, J, K, HALF_DIM
    REAL(REAL32):: VAL
    LOGICAL:: ByROW
    !
    ByROW = .TRUE.
    IF(PRESENT(BYCOLUMN)) ByROW = .NOT. BYCOLUMN
    !
    IF(BYROW) THEN
        J = DIM1
        HALF_DIM = DIM1/2
        !
        DO K=1, DIM2
            DO I=1, HALF_DIM
                VAL      = ARR(I,K)
                ARR(I,K) = ARR(J,K)
                ARR(J,K) = VAL
                J = J - 1
            END DO
        END DO
    ELSE
        BLOCK
            REAL(REAL32),DIMENSION(DIM1):: TMP
            J = DIM2
            HALF_DIM = DIM2/2
            !
            DO I=1, HALF_DIM
                TMP = ARR(:,I)
                ARR(:,I) = ARR(:,J)
                ARR(:,J) = TMP
                J = J - 1
            END DO
        END BLOCK
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_DBLE_ARRAY_COL(DIM1,DIM2,ARR,BYCOLUMN)
    INTEGER,                               INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2),INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL,                     INTENT(IN   ):: BYCOLUMN
    INTEGER:: I, J, K, HALF_DIM
    REAL(REAL64):: VAL
    LOGICAL:: ByROW
    !
    ByROW = .TRUE.
    IF(PRESENT(BYCOLUMN)) ByROW = .NOT. BYCOLUMN
    !
    IF(BYROW) THEN
        J = DIM1
        HALF_DIM = DIM1/2
        !
        DO K=1, DIM2
            DO I=1, HALF_DIM
                VAL      = ARR(I,K)
                ARR(I,K) = ARR(J,K)
                ARR(J,K) = VAL
                J = J - 1
            END DO
        END DO
    ELSE
        BLOCK
            REAL(REAL64),DIMENSION(DIM1):: TMP
            J = DIM2
            HALF_DIM = DIM2/2
            !
            DO I=1, HALF_DIM
                TMP = ARR(:,I)
                ARR(:,I) = ARR(:,J)
                ARR(:,J) = TMP
                J = J - 1
            END DO
        END BLOCK
    END IF
    !
  END SUBROUTINE
  !
END MODULE
!
!