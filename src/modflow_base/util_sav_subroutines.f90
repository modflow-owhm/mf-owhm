!
SUBROUTINE ULASAV(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,NROW,ILAY,ICHN)
  ! ******************************************************************
  ! SAVE 1 LAYER ARRAY ON DISK
  ! ******************************************************************
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND, RBUF
  IMPLICIT NONE
  !
  INTEGER,                    INTENT(IN):: NCOL,NROW
  REAL, DIMENSION(NCOL,NROW), INTENT(IN):: BUF
  CHARACTER(16),              INTENT(IN):: TEXT
  INTEGER,                    INTENT(IN):: KSTP,KPER,ILAY,ICHN
  REAL,                       INTENT(IN):: PERTIM,TOTIM
  INTEGER:: I,J
  REAL(REAL32):: rPERTIM,rTOTIM
  !
  !1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING INFORMATION.
  !2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES THE ARRAY IS DIMENSIONED (NCOL,NROW)
  !
  SELECT CASE(KIND(BUF))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         WRITE(ICHN) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
                         WRITE(ICHN) BUF  !((BUF(IC,IR),IC=1,NCOL),IR=1,NROW)
             CASE(REAL64)
                         WRITE(ICHN) KSTP,KPER,REAL(PERTIM,REAL64),REAL(TOTIM,REAL64),TEXT,NCOL,NROW,ILAY
                         DO I=1,NROW;  DO J=1,NCOL
                                                               WRITE(ICHN) REAL(BUF(J,I),REAL64)
                         END DO; END DO
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         CALL COPY_SNGL(PERTIM, rPERTIM)
                         CALL COPY_SNGL(TOTIM,  rTOTIM)
                         WRITE(ICHN) KSTP,KPER,rPERTIM,rTOTIM,TEXT,NCOL,NROW,ILAY
                         DO I=1,NROW;  DO J=1,NCOL
                                                  CALL COPY_SNGL(BUF(J,I), RBUF(J,I,1))
                         END DO; END DO
                         WRITE(ICHN) RBUF(:,:,1)
             CASE(REAL64)
                         WRITE(ICHN) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
                         WRITE(ICHN) BUF  
             END SELECT
  END SELECT
  !
END SUBROUTINE
!
SUBROUTINE UBUDSV(KSTP,KPER,TEXT,IBDCHN,BUF,NCOL,NROW,NLAY,IOUT)
  ! ******************************************************************
  ! RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW.
  ! ******************************************************************
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND, RBUF
  USE NUM2STR_INTERFACE,            ONLY: NUM2STR
  IMPLICIT NONE
  !
  INTEGER,                         INTENT(IN):: NCOL,NROW,NLAY
  REAL, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: BUF
  CHARACTER(16),                   INTENT(IN):: TEXT
  INTEGER,                         INTENT(IN):: KSTP,KPER,IOUT,IBDCHN
  INTEGER:: I,J,K
  !
  !1------WRITE AN UNFORMATTED RECORD IDENTIFYING DATA.
  WRITE(IOUT,'(A)') ' UBUDSV SAVING "'//TEXT//'" ON UNIT '//NUM2STR(IBDCHN)//' AT TIME STEP '//NUM2STR(KSTP)//', STRESS PERIOD '//NUM2STR(KPER)
  !
  WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,NLAY
  !
  !2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
  !2------EACH CELL IN THE GRID.
  SELECT CASE(KIND(BUF))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32); WRITE(IBDCHN) BUF
             CASE(REAL64)
                           DO K=1,NLAY;  DO I=1,NROW;  DO J=1,NCOL
                                                               WRITE(IBDCHN) REAL(BUF(J,I,K),REAL64)
                           END DO; END DO; END DO
             END SELECT
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                           DO K=1,NLAY;  DO I=1,NROW;  DO J=1,NCOL
                                                       CALL COPY_SNGL(BUF(J,I,K), RBUF(J,I,K))
                           END DO; END DO; END DO
                           WRITE(IBDCHN) RBUF
             CASE(REAL64); WRITE(IBDCHN) BUF
             END SELECT
  END SELECT
  !
END SUBROUTINE
!
SUBROUTINE UBDSV1(KSTP,KPER,TEXT,IBDCHN,BUF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
  ! ******************************************************************
  ! RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW AS A 3-D
  ! ARRAY WITH EXTRA RECORD TO INDICATE DELT, PERTIM, AND TOTIM.
  ! ******************************************************************
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND,RBUF
  USE NUM2STR_INTERFACE,            ONLY: NUM2STR
  IMPLICIT NONE
  !
  INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
  REAL,    DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: BUF
  INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: IBOUND
  CHARACTER(16),                      INTENT(IN):: TEXT
  INTEGER,                            INTENT(IN):: KSTP,KPER,IOUT,IBDCHN
  REAL,                               INTENT(IN):: PERTIM,TOTIM,DELT
  INTEGER:: I,J,K
  REAL(REAL32):: rDELT, rPERTIM, rTOTIM
  !     ------------------------------------------------------------------
  !
  !1------WRITE TWO UNFORMATTED RECORDS IDENTIFYING DATA.
  IF(IOUT.NE.0) WRITE(IOUT,'(A)') ' UBDSV1 SAVING "'//TEXT//'" ON UNIT '//NUM2STR(IBDCHN)//' AT TIME STEP '//NUM2STR(KSTP)//', STRESS PERIOD '//NUM2STR(KPER)
  !
  WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
  !
  !2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
  !2------EACH CELL IN THE GRID.
  !
  SELECT CASE(KIND(BUF))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         WRITE(IBDCHN) 1,DELT,PERTIM,TOTIM
                         WRITE(IBDCHN) BUF
             CASE(REAL64)
                         WRITE(IBDCHN) 1,REAL(DELT,REAL64),REAL(PERTIM,REAL64),REAL(TOTIM,REAL64)
                         DO K=1,NLAY;  DO I=1,NROW;  DO J=1,NCOL
                                                               WRITE(IBDCHN) REAL(BUF(J,I,K),REAL64)
                         END DO; END DO; END DO
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         CALL COPY_SNGL(DELT,   rDELT)
                         CALL COPY_SNGL(PERTIM, rPERTIM)
                         CALL COPY_SNGL(TOTIM,  rTOTIM)
                         WRITE(IBDCHN) 1, rDELT, rPERTIM, rTOTIM
                         DO K=1,NLAY;  DO I=1,NROW;  DO J=1,NCOL
                                                       CALL COPY_SNGL(BUF(J,I,K), RBUF(J,I,K))
                         END DO; END DO; END DO
                         WRITE(IBDCHN) RBUF
             CASE(REAL64)
                         WRITE(IBDCHN) 1,DELT,PERTIM,TOTIM
                         WRITE(IBDCHN) BUF
             END SELECT
  END SELECT
  !
END SUBROUTINE
!
SUBROUTINE UBDSV2(KSTP,KPER,TEXT,IBDCHN,NCOL,NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM,IBOUND)
  ! ******************************************************************
  ! WRITE HEADER RECORDS FOR CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT
  ! OF FLOW USING A LIST STRUCTURE.  EACH ITEM IN THE LIST IS WRITTEN
  ! BY MODULE UBDSVA
  ! ******************************************************************
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND
  USE NUM2STR_INTERFACE,            ONLY: NUM2STR
  IMPLICIT NONE
  !
  INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
  INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: IBOUND
  CHARACTER(16),                      INTENT(IN):: TEXT
  INTEGER,                            INTENT(IN):: KSTP,KPER,IBDCHN,NLIST,IOUT
  REAL,                               INTENT(IN):: PERTIM,TOTIM,DELT
  REAL(REAL32):: rDELT, rPERTIM, rTOTIM
  !
  !1------WRITE THREE UNFORMATTED RECORDS IDENTIFYING DATA.
  IF(IOUT.NE.0) WRITE(IOUT,'(A)') ' UBDSV2 SAVING "'//TEXT//'" ON UNIT '//NUM2STR(IBDCHN)//' AT TIME STEP '//NUM2STR(KSTP)//', STRESS PERIOD '//NUM2STR(KPER)
  !
  WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
      
  !
  SELECT CASE(KIND(PERTIM))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32); WRITE(IBDCHN) 2,DELT,PERTIM,TOTIM
             CASE(REAL64); WRITE(IBDCHN) 2,REAL(DELT,REAL64),REAL(PERTIM,REAL64),REAL(TOTIM,REAL64)
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                           CALL COPY_SNGL(DELT,   rDELT)
                           CALL COPY_SNGL(PERTIM, rPERTIM)
                           CALL COPY_SNGL(TOTIM,  rTOTIM) 
                           WRITE(IBDCHN) 2, rDELT, rPERTIM, rTOTIM
             CASE(REAL64); WRITE(IBDCHN) 2,  DELT,  PERTIM,  TOTIM
             END SELECT
  END SELECT
  !
  WRITE(IBDCHN) NLIST
  !
END SUBROUTINE
!
SUBROUTINE UBDSVA(IBDCHN,NCOL,NROW,J,I,K,Q,IBOUND,NLAY)
  ! ******************************************************************
  ! WRITE ONE VALUE OF CELL-BY-CELL FLOW USING A LIST STRUCTURE.
  ! ******************************************************************
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND
  IMPLICIT NONE
  !
  REAL,                               INTENT(IN):: Q
  INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
  INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: IBOUND
  INTEGER,                            INTENT(IN):: IBDCHN,J,I,K
  INTEGER:: ICRL
  REAL(REAL32):: rQ
  !
  !1------CALCULATE CELL NUMBER
  ICRL = (K-1)*NROW*NCOL + (I-1)*NCOL + J
  !
  !2------WRITE CELL NUMBER AND FLOW RATE
  !
  SELECT CASE(KIND(Q))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32); WRITE(IBDCHN) ICRL,Q
             CASE(REAL64); WRITE(IBDCHN) ICRL,REAL(Q,REAL64)
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32); 
                           CALL COPY_SNGL(Q,  rQ)
                           WRITE(IBDCHN) ICRL,rQ
             CASE(REAL64); WRITE(IBDCHN) ICRL, Q
             END SELECT
  END SELECT
  !
END SUBROUTINE
!
SUBROUTINE UBDSV3(KSTP,KPER,TEXT,IBDCHN,BUF,IBUF,NOPT,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
  !     ******************************************************************
  !     RECORD CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT OF FLOW AS A 2-D
  !     ARRAY OF FLOW VALUES AND OPTIONALLY A 2-D ARRAY OF LAYER NUMBERS
  !     ******************************************************************
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND, RBUF
  USE NUM2STR_INTERFACE,            ONLY: NUM2STR
  IMPLICIT NONE
  !
  INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
  REAL,    DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: BUF
  INTEGER, DIMENSION(NCOL,NROW),      INTENT(IN):: IBUF
  INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: IBOUND
  CHARACTER(16),                      INTENT(IN):: TEXT
  INTEGER,                            INTENT(IN):: KSTP,KPER,NOPT,IOUT,IBDCHN
  REAL,                               INTENT(IN):: DELT,PERTIM,TOTIM
  INTEGER:: IMETH, I, J
  REAL(REAL32):: rDELT, rPERTIM, rTOTIM
  !
  IMETH=3
  IF(NOPT.EQ.1) IMETH=4
  !
  !1------WRITE TWO UNFORMATTED RECORDS IDENTIFYING DATA.
  IF(IOUT.NE.0) WRITE(IOUT,'(A)')' UBDSV3 SAVING "'//TEXT//'" ON UNIT '//NUM2STR(IBDCHN)//' AT TIME STEP '//NUM2STR(KSTP)//', STRESS PERIOD '//NUM2STR(KPER)
  !
  WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
  !
  !2------WRITE DATA AS ONE OR TWO UNFORMATTED RECORDS CONTAINING ONE
  !2------VALUE PER LAYER.
  !  
  !2A-----WRITE ONE RECORD WHEN NOPT IS 1.  THE VALUES ARE FLOW VALUES
  !2A-----FOR LAYER 1.
  !
  !2B-----WRITE TWO RECORDS WHEN NOPT IS NOT 1.  FIRST RECORD CONTAINS
  !2B-----LAYER NUMBERS;  SECOND RECORD CONTAINS FLOW VALUES.
  SELECT CASE(KIND(BUF))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         WRITE(IBDCHN) IMETH,DELT,PERTIM,TOTIM
                         IF(NOPT.EQ.1) THEN
                             WRITE(IBDCHN) BUF(:,:,1) !((BUF(J,I,1),J=1,NCOL),I=1,NROW)
                         ELSE
                             WRITE(IBDCHN) IBUF !((IBUF(J,I),J=1,NCOL),I=1,NROW)
                             DO I=1, NROW
                             DO J=1, NCOL
                                          WRITE(IBDCHN) BUF(J,I,IBUF(J,I))
                             END DO
                             END DO
                         END IF
             CASE(REAL64)
                         WRITE(IBDCHN) IMETH,REAL(DELT,REAL64),REAL(PERTIM,REAL64),REAL(TOTIM,REAL64)
                         IF(NOPT.EQ.1) THEN
                           DO I=1,NROW;  DO J=1,NCOL
                                                    WRITE(IBDCHN) REAL(BUF(J,I,1),REAL64)
                           END DO; END DO
                         ELSE
                             WRITE(IBDCHN) IBUF
                             DO I=1, NROW
                             DO J=1, NCOL
                                          WRITE(IBDCHN) REAL(BUF(J,I,IBUF(J,I)),REAL64)
                             END DO
                             END DO
                         END IF
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         CALL COPY_SNGL(DELT,   rDELT)
                         CALL COPY_SNGL(PERTIM, rPERTIM)
                         CALL COPY_SNGL(TOTIM,  rTOTIM)
                         WRITE(IBDCHN) IMETH, rDELT, rPERTIM, rTOTIM
                         IF(NOPT.EQ.1) THEN
                             DO I=1,NROW;  DO J=1,NCOL
                                                      CALL COPY_SNGL(BUF(J,I,1), RBUF(J,I,1))
                             END DO; END DO
                             WRITE(IBDCHN) RBUF(:,:,1)
                         ELSE
                             WRITE(IBDCHN) IBUF
                             !
                             DO I=1,NROW;  DO J=1,NCOL
                                                      CALL COPY_SNGL(BUF(J,I,IBUF(J,I)), RBUF(J,I,1))
                             END DO; END DO
                             WRITE(IBDCHN) RBUF(:,:,1)
                         END IF
             CASE(REAL64)
                         WRITE(IBDCHN) IMETH,DELT,PERTIM,TOTIM
                         IF(NOPT.EQ.1) THEN
                             WRITE(IBDCHN) BUF(:,:,1) 
                         ELSE
                             WRITE(IBDCHN) IBUF
                             DO I=1, NROW
                             DO J=1, NCOL
                                          WRITE(IBDCHN) BUF(J,I,IBUF(J,I))
                             END DO
                             END DO
                         END IF
             END SELECT
  END SELECT
  !
END SUBROUTINE
!
SUBROUTINE UBDSV4(KSTP,KPER,TEXT,NAUX,AUXTXT,IBDCHN,NCOL,NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM,IBOUND)
  !     ******************************************************************
  !     WRITE HEADER RECORDS FOR CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT
  !     OF FLOW PLUS AUXILIARY DATA USING A LIST STRUCTURE.  EACH ITEM IN
  !     THE LIST IS WRITTEN BY MODULE UBDSVB
  !     ******************************************************************
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND
  USE NUM2STR_INTERFACE,            ONLY: NUM2STR
  IMPLICIT NONE
  !
  INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
  INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: IBOUND
  CHARACTER(16),                      INTENT(IN):: TEXT
  CHARACTER(16), DIMENSION(*),        INTENT(IN):: AUXTXT
  INTEGER,                            INTENT(IN):: IBDCHN,KSTP,KPER,NAUX,NLIST,IOUT
  REAL,                               INTENT(IN):: DELT,PERTIM,TOTIM
  REAL(REAL32):: rDELT, rPERTIM, rTOTIM
  !
  !1------WRITE UNFORMATTED RECORDS IDENTIFYING DATA.
  IF(IOUT.NE.0) WRITE(IOUT,'(A)') ' UBDSV4 SAVING "'//TEXT//'" ON UNIT '//NUM2STR(IBDCHN)//' AT TIME STEP '//NUM2STR(KSTP)//', STRESS PERIOD '//NUM2STR(KPER)
  !
  WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
  !
  SELECT CASE(KIND(PERTIM))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32); WRITE(IBDCHN) 5,DELT,PERTIM,TOTIM
             CASE(REAL64); WRITE(IBDCHN) 5,REAL(DELT,REAL64),REAL(PERTIM,REAL64),REAL(TOTIM,REAL64)
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                           CALL COPY_SNGL(DELT,   rDELT)
                           CALL COPY_SNGL(PERTIM, rPERTIM)
                           CALL COPY_SNGL(TOTIM,  rTOTIM)
                           WRITE(IBDCHN) 5, rDELT, rPERTIM, rTOTIM
             CASE(REAL64); WRITE(IBDCHN) 5,  DELT,  PERTIM,  TOTIM
             END SELECT
  END SELECT
  !
  WRITE(IBDCHN) NAUX+1
  IF(NAUX.GT.0) WRITE(IBDCHN) AUXTXT(1:NAUX)
  WRITE(IBDCHN) NLIST
!
END SUBROUTINE
!
SUBROUTINE UBDSVB(IBDCHN,NCOL,NROW,J,I,K,Q,VAL,NVL,NAUX,LAUX,IBOUND,NLAY)
  ! ******************************************************************
  ! WRITE ONE VALUE OF CELL-BY-CELL FLOW PLUS AUXILIARY DATA USING
  ! A LIST STRUCTURE.
  ! ******************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE GLOBAL,                       ONLY: BIN_REAL_KIND
  IMPLICIT NONE
  !
  INTEGER,                            INTENT(IN):: NVL,NCOL,NROW,NLAY
  REAL,                               INTENT(IN):: Q
  REAL,    DIMENSION(NVL),            INTENT(IN):: VAL
  INTEGER, DIMENSION(NCOL,NROW,NLAY), INTENT(IN):: IBOUND
  INTEGER,                            INTENT(IN):: IBDCHN,J,I,K,NAUX,LAUX
  INTEGER:: ICRL, N2, lp
  REAL(REAL32):: rQ
  REAL(REAL32), dimension(:), allocatable:: rVAL
  !
  !1------CALCULATE CELL NUMBER
  ICRL = (K-1)*NROW*NCOL + (I-1)*NCOL + J
  !
  !2------WRITE CELL NUMBER AND FLOW RATE
  !
  SELECT CASE(KIND(Q))
  CASE(REAL32)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         IF(NAUX.GT.0) THEN
                            N2=LAUX+NAUX-1
                            WRITE(IBDCHN) ICRL,Q,VAL(LAUX:N2)
                         ELSE
                            WRITE(IBDCHN) ICRL,Q
                         END IF
             CASE(REAL64)
                         IF(NAUX.GT.0) THEN
                            N2=LAUX+NAUX-1
                            WRITE(IBDCHN) ICRL,REAL(Q,REAL64),REAL(VAL(LAUX:N2),REAL64)
                         ELSE
                            WRITE(IBDCHN) ICRL,REAL(Q,REAL64)
                         END IF
             END SELECT
             
  CASE(REAL64)
             SELECT CASE(BIN_REAL_KIND)
             CASE(REAL32)
                         CALL COPY_SNGL(Q,  rQ)
                         IF(NAUX.GT.0) THEN
                            N2=LAUX+NAUX-1
                            !
                            allocate(rVAL(NVL))
                            do lp=LAUX, N2
                                         CALL COPY_SNGL(VAL(lp),  rVAL(lp))
                            end do
                            !
                            WRITE(IBDCHN) ICRL, rQ, rVAL(LAUX:N2)
                         ELSE
                            WRITE(IBDCHN) ICRL, rQ
                         END IF
             CASE(REAL64)
                         IF(NAUX.GT.0) THEN
                            N2=LAUX+NAUX-1
                            WRITE(IBDCHN) ICRL,Q,VAL(LAUX:N2)
                         ELSE
                            WRITE(IBDCHN) ICRL,Q
                         END IF
             END SELECT
  END SELECT
  !
END SUBROUTINE
  !
SUBROUTINE UTILSAV2D(FL,COMPACT,BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,NROW,NLAY,DATE)
  ! ******************************************************************
  ! SAVE 1 LAYER ARRAY ON DISK
  ! ******************************************************************
  USE, INTRINSIC:: ISO_FORTRAN_ENV,    ONLY: REAL32, REAL64
  USE CONSTANTS,                       ONLY: ONE, Z, NEG, NEGNEARZERO_30, NEARZERO_30
  USE GLOBAL,                          ONLY: BIN_REAL_KIND, RBUF
  USE UTIL_INTERFACE,                  ONLY: NOT_NEAR_ZERO
  USE NUM2STR_INTERFACE,               ONLY: NUM2STR
  USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
  IMPLICIT NONE
  !
  INTEGER,                            INTENT(IN):: NCOL,NROW,NLAY
  CLASS(GENERIC_OUTPUT_FILE),         INTENT(IN):: FL
  LOGICAL,                            INTENT(IN):: COMPACT
  REAL(REAL64), DIMENSION(NCOL,NROW), INTENT(IN):: BUF
  CHARACTER(16),                      INTENT(IN):: TEXT
  INTEGER,                            INTENT(IN):: KSTP,KPER
  REAL(REAL64),                       INTENT(IN):: PERTIM,TOTIM
  CHARACTER(10),                      INTENT(IN):: DATE
  INTEGER:: CNT,I,J,IU,ICRL
  REAL(REAL32):: rPERTIM,rTOTIM, rB
  !
  IF(.NOT. FL%IS_OPEN) RETURN
  !
  SELECT CASE(KIND(PERTIM))
  CASE(REAL32)
              rPERTIM = PERTIM
              rTOTIM  = TOTIM
  CASE(REAL64)
              CALL COPY_SNGL(PERTIM, rPERTIM)
              CALL COPY_SNGL(TOTIM,  rTOTIM)
  END SELECT
  !
  IU = FL%IU
  !
  IF(COMPACT) THEN
    CNT = Z
    DO I=ONE, NROW
    DO J=ONE, NCOL
            IF( NOT_NEAR_ZERO(BUF(J,I)) ) CNT = CNT + ONE
    END DO
    END DO
    !
    IF(FL%BINARY) THEN
        !
        SELECT CASE(BIN_REAL_KIND)
        CASE(REAL32)
                    WRITE(IU) CNT, KSTP,KPER,rPERTIM,rTOTIM,TEXT,NCOL,NROW,NLAY
                    !
                    DO I=ONE, NROW
                    DO J=ONE, NCOL
                    IF(NOT_NEAR_ZERO(BUF(J,I))) THEN
                        !
                        ICRL= (I-1)*NCOL + J
                        CALL COPY_SNGL(BUF(J,I), rB)
                        !
                        WRITE(IU) ICRL, rB
                    END IF
                    END DO
                    END DO
        CASE(REAL64)
                    WRITE(IU) CNT, KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,NLAY
                    !
                    DO I=ONE, NROW
                    DO J=ONE, NCOL
                    IF(NOT_NEAR_ZERO(BUF(J,I))) THEN
                        !
                        ICRL= (I-1)*NCOL + J
                        WRITE(IU) ICRL, BUF(J,I)
                    END IF
                    END DO
                    END DO
        END SELECT
    ELSE
        WRITE(IU,'(2I7,3(1x,A),3I7,3(1x,A))') KSTP,KPER,NUM2STR(PERTIM,10),NUM2STR(TOTIM,10),TEXT,NCOL,NROW,NLAY, DATE, ' NTERM ', NUM2STR(CNT)
        !
        DO I=ONE, NROW
        DO J=ONE, NCOL
        IF(NOT_NEAR_ZERO(BUF(J,I)))  WRITE(IU,'(2I7, ES15.7)') I, J, BUF(J,I)
        END DO
        END DO
    END IF
  ELSE
    IF(FL%BINARY) THEN
        CNT = NEG
        SELECT CASE(BIN_REAL_KIND)
        CASE(REAL32)
                    WRITE(IU) CNT, KSTP,KPER,rPERTIM,rTOTIM,TEXT,NCOL,NROW,NLAY
                    DO I=1,NROW;  DO J=1,NCOL
                                             CALL COPY_SNGL(BUF(J,I), RBUF(J,I,ONE))
                    END DO; END DO
                    WRITE(IU) RBUF(:,:,ONE)
        CASE(REAL64)
                    WRITE(IU) CNT, KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,NLAY
                    WRITE(IU) BUF  
        END SELECT
    ELSE
        WRITE(IU,'(2I7,3(1x,A),3I7,1x,A)') KSTP,KPER,NUM2STR(PERTIM,10),NUM2STR(TOTIM,10),TEXT,NCOL,NROW,NLAY, DATE
        !
        DO I=ONE, NROW
                 WRITE(IU,'(*(ES15.7))') BUF(:,I)
        END DO
    END IF
  END IF
  !
END SUBROUTINE
!
PURE SUBROUTINE COPY_SNGL(IN_VAL, OUT_VAL)
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  REAL(REAL64), INTENT(IN ):: IN_VAL
  REAL(REAL32), INTENT(OUT):: OUT_VAL
  !
  IF    (IN_VAL /= IN_VAL       ) THEN                ! Set to NaN
                            OUT_VAL = IEEE_VALUE(OUT_VAL, IEEE_QUIET_NAN)
  ELSEIF(IN_VAL < -3.4e38_real64) THEN                ! Lower limit of Single Precision
                            OUT_VAL = -3.4E38_real32
  ELSEIF(IN_VAL >  3.4e38_real64) THEN
                            OUT_VAL =  3.4E38_real32  ! Upper limit of Single Precision
  ELSE
                            OUT_VAL = REAL(IN_VAL, REAL32)
  END IF
  !
END SUBROUTINE
!
!SUBROUTINE WRITE_STREAM_2D_REAL32_AYNC(IU,DIM1,DIM2,BUF)
!  INTEGER, INTENT(IN):: IU,DIM1,DIM2
!  REAL(REAL32), DIMENSION(DIM1,DIM2), ASYNCHRONOUS, INTENT(IN):: BUF
!  !
!  WRITE(IU,ASYNCHRONOUS='YES') BUF
!  !
!  WAIT(IU)
!  !
!END SUBROUTINE
!SUBROUTINE WRITE_STREAM_3D_REAL32_AYNC(IU,DIM1,DIM2,DIM3,BUF)
!  INTEGER, INTENT(IN):: IU,DIM1,DIM2,DIM3
!  REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), ASYNCHRONOUS, INTENT(IN):: BUF
!  !
!  WRITE(IU,ASYNCHRONOUS='YES') BUF
!  !
!  WAIT(IU)
!  !
!END SUBROUTINE
!!
!SUBROUTINE WRITE_STREAM_2D_REAL64_AYNC(IU,DIM1,DIM2,BUF)
!  INTEGER, INTENT(IN):: IU,DIM1,DIM2
!  REAL(REAL64), DIMENSION(DIM1,DIM2), ASYNCHRONOUS, INTENT(IN):: BUF
!  !
!  WRITE(IU,ASYNCHRONOUS='YES') BUF
!  !
!  WAIT(IU)
!  !
!END SUBROUTINE
!!
!SUBROUTINE WRITE_STREAM_3D_REAL64_AYNC(IU,DIM1,DIM2,DIM3,BUF)
!  INTEGER, INTENT(IN):: IU,DIM1,DIM2,DIM3
!  REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), ASYNCHRONOUS, INTENT(IN):: BUF
!  !
!  WRITE(IU,ASYNCHRONOUS='YES') BUF
!  !
!  WAIT(IU)
!  !
!END SUBROUTINE
    