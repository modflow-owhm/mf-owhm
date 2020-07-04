C
C
C
C
C
      SUBROUTINE CFPBCF7BDCH(KSTP, KPER, IGRID)
C***********************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C***********************************************************************
C
      USE CONSTANTS, ONLY: DZ, Z
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND, HNEW, BUFF, CR, CC, CV,
     +                 BOTM, LBOTM, IOUT
      USE GWFBASMODULE, ONLY:MSUM, VBVL, VBNM, DELT, PERTIM, TOTIM,     
     +                       ICBCFL, ICHFLG
      USE GWFBCFMODULE, ONLY:IBCFCB, LAYCON
      USE CFPMODULE, ONLY:MODE, MOD2, NCOND, B, NODEBOT
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KSTP, KPER, IGRID
C
C--LOCAL VARIABLES
      DOUBLE PRECISION CHCH1, CHCH2, CHCH3, CHCH4, CHCH5, CHCH6
      DOUBLE PRECISION CIN, COUT, HDIFF, RATE, TMP
      REAL RRATE
C
C--BARC**MOVE RATE HERE
      DOUBLE PRECISION X1, X2, X3, X4, X5, X6, X7, XX7
      DOUBLE PRECISION HD, CHIN, CHOUT, XX1, XX2, XX3, XX4, XX5, XX6
C
C--BARC**ADD XX7 FOR CONDUIT FLOW RATE
      INTEGER I, IBD, IBDLBL, J, K, LC, NCH, NNCOND
      CHARACTER(LEN=16) TEXT
      DATA TEXT/'   CONSTANT HEAD'/
C-----------------------------------------------------------------------
      IF ( MODE.EQ.2 ) THEN 
	  CALL GWF2BCF7BDCH(KSTP,KPER,IGRID)
	  RETURN
	ENDIF
C
      CALL SGWF2BCF7PNT(IGRID)
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD = Z
      IF ( IBCFCB.LT.Z .AND. ICBCFL.NE.Z ) IBD = -1
      IF ( IBCFCB.GT.Z ) IBD = ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      CHIN = DZ
      CHOUT = DZ
      IBDLBL = Z
C
C3------CLEAR BUFFER.
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            BUFF(J, I, K) = DZ
          ENDDO
        ENDDO
      ENDDO
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF ( IBD.EQ.2 ) THEN
        NCH = Z
        DO K = 1, NLAY
          DO I = 1, NROW
            DO J = 1, NCOL
              IF ( IBOUND(J,I,K).LT.Z ) NCH = NCH + 1
            ENDDO
          ENDDO
        ENDDO
        CALL UBDSV2(KSTP, KPER, TEXT, IBCFCB, NCOL, NROW, NLAY, NCH,    
     +              IOUT, DELT, PERTIM, TOTIM, IBOUND)
      ENDIF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO K = 1, NLAY
        LC = LAYCON(K)
        DO I = 1, NROW
          DO J = 1, NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF ( IBOUND(J,I,K).LT.Z ) THEN
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
              X1 = DZ
              X2 = DZ
              X3 = DZ
              X4 = DZ
              X5 = DZ
              X6 = DZ
C
C--BARC**ADD X7 FOR CONDUIT
              X7 = DZ
              XX7 = DZ
              CHCH1 = DZ
              CHCH2 = DZ
              CHCH3 = DZ
              CHCH4 = DZ
              CHCH5 = DZ
              CHCH6 = DZ
C
C--BARC**ADD X7 FOR CONDUIT
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
              IF ( J.NE.1 ) THEN
                IF ( IBOUND(J-1,I,K).NE.Z ) THEN
                  IF ( IBOUND(J-1,I,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
                    HDIFF = HNEW(J, I, K) - HNEW(J-1, I, K)
                    CHCH1 = HDIFF*CR(J-1, I, K)
                    IF ( IBOUND(J-1,I,K).GE.Z ) THEN
                      X1 = CHCH1
                      XX1 = X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
                      IF ( X1.LT.DZ ) THEN
                        CHOUT = CHOUT - XX1
                      ELSE
                        CHIN = CHIN + XX1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
              IF ( J.NE.NCOL ) THEN
                IF ( IBOUND(J+1,I,K).NE.Z ) THEN
                  IF ( IBOUND(J+1,I,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HDIFF = HNEW(J, I, K) - HNEW(J+1, I, K)
                    CHCH2 = HDIFF*CR(J, I, K)
                    IF ( IBOUND(J+1,I,K).GE.Z ) THEN
                      X2 = CHCH2
                      XX2 = X2
                      IF ( X2.LT.DZ ) THEN
                        CHOUT = CHOUT - XX2
                      ELSE
                        CHIN = CHIN + XX2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
              IF ( I.NE.1 ) THEN
                IF ( IBOUND(J,I-1,K).NE.Z ) THEN
                  IF ( IBOUND(J,I-1,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HDIFF = HNEW(J, I, K) - HNEW(J, I-1, K)
                    CHCH3 = HDIFF*CC(J, I-1, K)
                    IF ( IBOUND(J,I-1,K).GE.Z ) THEN
                      X3 = CHCH3
                      XX3 = X3
                      IF ( X3.LT.DZ ) THEN
                        CHOUT = CHOUT - XX3
                      ELSE
                        CHIN = CHIN + XX3
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
              IF ( I.NE.NROW ) THEN
                IF ( IBOUND(J,I+1,K).NE.Z ) THEN
                  IF ( IBOUND(J,I+1,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HDIFF = HNEW(J, I, K) - HNEW(J, I+1, K)
                    CHCH4 = HDIFF*CC(J, I, K)
                    IF ( IBOUND(J,I+1,K).GE.Z ) THEN
                      X4 = CHCH4
                      XX4 = X4
                      IF ( X4.LT.DZ ) THEN
                        CHOUT = CHOUT - XX4
                      ELSE
                        CHIN = CHIN + XX4
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
              IF ( K.NE.1 ) THEN
                IF ( IBOUND(J,I,K-1).NE.Z ) THEN
                  IF ( IBOUND(J,I,K-1).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HD = HNEW(J, I, K)
                    IF ( LC.EQ.3 .OR. LC.EQ.2 ) THEN
                      TMP = HD
                      IF ( TMP.LT.BOTM(J,I,LBOTM(K)-1) )                
     +                     HD = BOTM(J, I, LBOTM(K)-1)
                    ENDIF
                    HDIFF = HD - HNEW(J, I, K-1)
                    CHCH5 = HDIFF*CV(J, I, K-1)
                    IF ( IBOUND(J,I,K-1).GE.Z ) THEN
                      X5 = CHCH5
                      XX5 = X5
                      IF ( X5.LT.DZ ) THEN
                        CHOUT = CHOUT - XX5
                      ELSE
                        CHIN = CHIN + XX5
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
              IF ( K.NE.NLAY ) THEN
                IF ( IBOUND(J,I,K+1).NE.Z ) THEN
                  IF ( IBOUND(J,I,K+1).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HD = HNEW(J, I, K+1)
                    IF ( LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2 ) THEN
                      TMP = HD
                      IF ( TMP.LT.BOTM(J,I,LBOTM(K+1)-1) )              
     +                     HD = BOTM(J, I, LBOTM(K+1)-1)
                    ENDIF
                    HDIFF = HNEW(J, I, K) - HD
                    CHCH6 = HDIFF*CV(J, I, K)
                    IF ( IBOUND(J,I,K+1).GE.Z ) THEN
                      X6 = CHCH6
                      XX6 = X6
                      IF ( X6.LT.DZ ) THEN
                        CHOUT = CHOUT - XX6
                      ELSE
                        CHIN = CHIN + XX6
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
              RATE = CHCH1 + CHCH2 + CHCH3 + CHCH4 + CHCH5 + CHCH6
C
C--BIRK  CALCULATE FLOW TO OR FROM CONDUIT
              IF ( NCOND(J,I,K).NE.Z ) THEN
                NNCOND = NCOND(J, I, K)
                HDIFF = B(NNCOND) - HNEW(J, I, K)
C
C--SEBA--INCLUDE DRY NODES. DON'T CALCULATE RATE IF NODE IS DRY
CB      PRINT *, B(NNCOND), NODEBOT(NNCOND)
CB      IF(B(NNCOND).LT.NODEBOT(NNCOND)) GO TO 190
                X7 = -MOD2(NNCOND)*HDIFF
                XX7 = X7
                IF ( XX7.LT.DZ ) THEN
                  CHOUT = CHOUT - XX7
                ELSEIF ( XX7.GT.DZ ) THEN
                  CHIN = CHIN + XX7
                ENDIF
              ENDIF
C
C--BIRK ADD CONDUIT FLOW X7 TO RATE
              RATE = RATE + XX7
              !
              RRATE = SNGL(RATE)
              BUFF(J, I, K) = RRATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
              IF ( IBD.LT.Z ) THEN
                IF ( IBDLBL.EQ.Z ) WRITE (IOUT, 9001) TEXT, KPER, KSTP
                WRITE (IOUT, 9002) K, I, J, RATE
                IBDLBL = 1
              ENDIF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
CBARC**PASS RATE AS SINGLE PRECISION
              IF ( IBD.EQ.2 ) CALL UBDSVA(IBCFCB, NCOL, NROW, J, I, K,  
     +                                    RRATE, IBOUND, NLAY)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF ( IBD.EQ.1 ) CALL UBUDSV(KSTP, KPER, TEXT, IBCFCB, BUFF, NCOL, 
     +                            NROW, NLAY, IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN = CHIN
      COUT = CHOUT
      VBVL(1, MSUM) = VBVL(1, MSUM) + CIN*DELT
      VBVL(2, MSUM) = VBVL(2, MSUM) + COUT*DELT
      VBVL(3, MSUM) = CIN
      VBVL(4, MSUM) = COUT
      VBNM(MSUM) = TEXT
      MSUM = MSUM + 1
C 
 9001 FORMAT (1X, /, 1X, A, '   PERIOD', I3, '   STEP', I3)
 9002 FORMAT (1X, 'LAYER', I3, '   ROW', I4, '   COL', I4, '   RATE',   
     +        1PG15.6)
C
C18-----RETURN.
      END SUBROUTINE CFPBCF7BDCH
C
C
C
C
C
      SUBROUTINE CFPLPF7BDCH(KSTP, KPER, IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
      USE CONSTANTS, ONLY: DZ, Z
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND, HNEW, BUFF, CR, CC, CV,
     +    BOTM, LBOTM, IOUT
      USE GWFBASMODULE, ONLY:MSUM, VBVL, VBNM, DELT, PERTIM, TOTIM,     
     +    ICBCFL, ICHFLG
      USE GWFLPFMODULE, ONLY:ILPFCB, LAYTYP
C
C--BARC**USE CFP MODULE 
      USE CFPMODULE, ONLY:MOD2, NCOND, B, MODE
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KSTP, KPER, IGRID
C
C--LOCAL VARIABLES
      DOUBLE PRECISION CHCH1, CHCH2, CHCH3, CHCH4, CHCH5, CHCH6
      DOUBLE PRECISION CIN, COUT, HDIFF, RATE, TMP, TOP
      DOUBLE PRECISION X1, X2, X3, X4, X5, X6, X7, XX7
      DOUBLE PRECISION HD, CHIN, CHOUT, XX1, XX2, XX3, XX4, XX5, XX6
      INTEGER I, IBD, IBDLBL, J, K, NCH, NNCOND
      REAL:: RRATE
C
C--BARC**ADD XX7 FOR CONDUIT FLOW RATE      
C
      CHARACTER(LEN=16) TEXT
C
      DATA TEXT/'   CONSTANT HEAD'/
C-----------------------------------------------------------------------
      IF ( MODE.EQ.2 ) THEN 
	 CALL GWF2LPF7BDCH(KSTP,KPER,IGRID)
	 RETURN
      ENDIF
C
C--BARC**
      CALL SGWF2LPF7PNT(IGRID)
CB    CALL GWF2LPF7PNT(IGRID)
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD = Z
      IF ( ILPFCB.LT.Z .AND. ICBCFL.NE.Z ) IBD = -1
      IF ( ILPFCB.GT.Z ) IBD = ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      CHIN = DZ
      CHOUT = DZ
      IBDLBL = Z
C
C3------CLEAR BUFFER.
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            BUFF(J, I, K) = DZ
          ENDDO
        ENDDO
      ENDDO
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF ( IBD.EQ.2 ) THEN
        NCH = Z
        DO K = 1, NLAY
          DO I = 1, NROW
            DO J = 1, NCOL
              IF ( IBOUND(J,I,K).LT.Z ) NCH = NCH + 1
            ENDDO
          ENDDO
        ENDDO
        CALL UBDSV2(KSTP, KPER, TEXT, ILPFCB, NCOL, NROW, NLAY, NCH,    
     +              IOUT, DELT, PERTIM, TOTIM, IBOUND)
      ENDIF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF ( IBOUND(J,I,K).LT.Z ) THEN
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
              X1 = DZ
              X2 = DZ
              X3 = DZ
              X4 = DZ
              X5 = DZ
              X6 = DZ
CBARC**ADD X7 FOR CONDUIT
              X7 = DZ
              XX7 = DZ
C 
              CHCH1 = DZ
              CHCH2 = DZ
              CHCH3 = DZ
              CHCH4 = DZ
              CHCH5 = DZ
              CHCH6 = DZ
C
C--BARC**ADD X7 FOR CONDUIT
!             CHCH7 = DZ              
C
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
              IF ( J.NE.1 ) THEN
                IF ( IBOUND(J-1,I,K).NE.Z ) THEN
                  IF ( IBOUND(J-1,I,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
                    HDIFF = HNEW(J, I, K) - HNEW(J-1, I, K)
                    CHCH1 = HDIFF*CR(J-1, I, K)
                    IF ( IBOUND(J-1,I,K).GE.Z ) THEN
                      X1 = CHCH1
                      XX1 = X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
                      IF ( X1.LT.DZ ) THEN
                        CHOUT = CHOUT - XX1
                      ELSE
                        CHIN = CHIN + XX1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
              IF ( J.NE.NCOL ) THEN
                IF ( IBOUND(J+1,I,K).NE.Z ) THEN
                  IF ( IBOUND(J+1,I,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HDIFF = HNEW(J, I, K) - HNEW(J+1, I, K)
                    CHCH2 = HDIFF*CR(J, I, K)
                    IF ( IBOUND(J+1,I,K).GE.Z ) THEN
                      X2 = CHCH2
                      XX2 = X2
                      IF ( X2.LT.DZ ) THEN
                        CHOUT = CHOUT - XX2
                      ELSE
                        CHIN = CHIN + XX2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
              IF ( I.NE.1 ) THEN
                IF ( IBOUND(J,I-1,K).NE.Z ) THEN
                  IF ( IBOUND(J,I-1,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HDIFF = HNEW(J, I, K) - HNEW(J, I-1, K)
                    CHCH3 = HDIFF*CC(J, I-1, K)
                    IF ( IBOUND(J,I-1,K).GE.Z ) THEN
                      X3 = CHCH3
                      XX3 = X3
                      IF ( X3.LT.DZ ) THEN
                        CHOUT = CHOUT - XX3
                      ELSE
                        CHIN = CHIN + XX3
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
              IF ( I.NE.NROW ) THEN
                IF ( IBOUND(J,I+1,K).NE.Z ) THEN
                  IF ( IBOUND(J,I+1,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HDIFF = HNEW(J, I, K) - HNEW(J, I+1, K)
                    CHCH4 = HDIFF*CC(J, I, K)
                    IF ( IBOUND(J,I+1,K).GE.Z ) THEN
                      X4 = CHCH4
                      XX4 = X4
                      IF ( X4.LT.DZ ) THEN
                        CHOUT = CHOUT - XX4
                      ELSE
                        CHIN = CHIN + XX4
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
              IF ( K.NE.1 ) THEN
                IF ( IBOUND(J,I,K-1).NE.Z ) THEN
                  IF ( IBOUND(J,I,K-1).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HD = HNEW(J, I, K)
                    IF ( LAYTYP(K).NE.Z ) THEN
                      TMP = HD
                      TOP = BOTM(J, I, LBOTM(K)-1)
                      IF ( TMP.LT.TOP ) HD = TOP
                    ENDIF
                    HDIFF = HD - HNEW(J, I, K-1)
                    CHCH5 = HDIFF*CV(J, I, K-1)
                    IF ( IBOUND(J,I,K-1).GE.Z ) THEN
                      X5 = CHCH5
                      XX5 = X5
                      IF ( X5.LT.DZ ) THEN
                        CHOUT = CHOUT - XX5
                      ELSE
                        CHIN = CHIN + XX5
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
              IF ( K.NE.NLAY ) THEN
                IF ( IBOUND(J,I,K+1).NE.Z ) THEN
                  IF ( IBOUND(J,I,K+1).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HD = HNEW(J, I, K+1)
                    IF ( LAYTYP(K+1).NE.Z ) THEN
                      TMP = HD
                      TOP = BOTM(J, I, LBOTM(K+1)-1)
                      IF ( TMP.LT.TOP ) HD = TOP
                    ENDIF
                    HDIFF = HNEW(J, I, K) - HD
                    CHCH6 = HDIFF*CV(J, I, K)
                    IF ( IBOUND(J,I,K+1).GE.Z ) THEN
                      X6 = CHCH6
                      XX6 = X6
                      IF ( X6.LT.DZ ) THEN
                        CHOUT = CHOUT - XX6
                      ELSE
                        CHIN = CHIN + XX6
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
              RATE = CHCH1 + CHCH2 + CHCH3 + CHCH4 + CHCH5 + CHCH6
 
C
C--BIRK  CALCULATE FLOW TO OR FROM CONDUIT
              IF ( NCOND(J,I,K).NE.Z ) THEN
                NNCOND = NCOND(J, I, K)
                HDIFF = B(NNCOND) - HNEW(J, I, K)
C
C--SEBA--INCLUDE DRY NODES. DON'T CALCULATE RATE IF NODE IS DRY
C      IF(NODEWD(NNCOND).LT.Z) GO TO 190
CB      IF(B(NNCOND).LT.NODEBOT(NNCOND)) GO TO 190
 
                X7 = -MOD2(NNCOND)*HDIFF
                XX7 = X7
 
                IF ( XX7.LT.DZ ) THEN
                  CHOUT = CHOUT - XX7
                ELSEIF ( XX7.GT.DZ ) THEN
                  CHIN = CHIN + XX7
                ENDIF
              ENDIF
C
C--BIRK ADD CONDUIT FLOW X7 TO RATE
              RATE = RATE + XX7
C 
              RRATE = RATE
              BUFF(J, I, K) = RRATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
              IF ( IBD.LT.Z ) THEN
                IF ( IBDLBL.EQ.Z ) WRITE (IOUT, 9001) TEXT, KPER, KSTP
                WRITE (IOUT, 9002) K, I, J, RATE
                IBDLBL = 1
              ENDIF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
CBARC**PASS RATE AS SINGLE PRECISION
              IF ( IBD.EQ.2 ) CALL UBDSVA(ILPFCB, NCOL, NROW, J, I, K,  
     +                                    RRATE, IBOUND, NLAY)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF ( IBD.EQ.1 ) CALL UBUDSV(KSTP, KPER, TEXT, ILPFCB, BUFF, NCOL, 
     +                            NROW, NLAY, IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN = CHIN
      COUT = CHOUT
      VBVL(1, MSUM) = VBVL(1, MSUM) + CIN*DELT
      VBVL(2, MSUM) = VBVL(2, MSUM) + COUT*DELT
      VBVL(3, MSUM) = CIN
      VBVL(4, MSUM) = COUT
      VBNM(MSUM) = TEXT
      MSUM = MSUM + 1
C 
 9001 FORMAT (1X, /, 1X, A, '   PERIOD', I5, '   STEP', I4)
 9002 FORMAT (' LAYER', I4, '   ROW', I6, '   COL', I6, '   RATE',      
     +        1PG16.6)
C
C18-----RETURN.
      END SUBROUTINE CFPLPF7BDCH
C
C
C
C
C
      SUBROUTINE CFPHUF7BDCH(KSTP, KPER, ILVDA, IGRID)
C
C--BARC**MODIFIED TO WORK WITH CFP MODE 1 PIPES
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
      USE CONSTANTS, ONLY: DZ, Z
      USE GLOBAL, ONLY:NCOL, NROW, NLAY, IBOUND, HNEW, BUFF, CR, CC, CV,
     +    BOTM, LBOTM, IOUT, KND
      USE GWFBASMODULE, ONLY:MSUM, VBVL, VBNM, DELT, PERTIM, TOTIM,     
     +    ICBCFL, ICHFLG
      USE GWFHUFMODULE, ONLY:LTHUF, IHUFCB, VDHT
C
C--BARC**USE CFP MODULE      
      USE CFPMODULE, ONLY:MODE, MOD2, NCOND, B
C
      IMPLICIT NONE
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: IGRID, ILVDA, KPER, KSTP
C
C--LOCAL VARIABLES
      REAL(KND):: CHCH1, CHCH2, CHCH3, CHCH4, CHCH5, CHCH6, CIN
      REAL(KND):: COUT, HDIFF, RATE, TMP, TOP
      REAL(KND):: X1, X2, X3, X4, X5, X6, X7
      INTEGER I, IBD, IBDLBL, J, K, NCH, NNCOND
C
C--BARC**MODIFIED TO WORK WITH CFP MODE 1 PIPES
      REAL(KND):: HD, CHIN, CHOUT, XX1, XX2, XX3, XX4, XX5, XX6
      REAL(KND):: DFL, DFR, DFT, DFB, XX7
      REAL:: RRATE
C
C--BARC**ADD XX7 FOR CONDUIT FLOW RATE
C
      CHARACTER(LEN=16) TEXT
C
      DATA TEXT/'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
      IF ( MODE.EQ.2 ) THEN 
       CALL GWF2HUF7BDCH(KSTP,KPER,ILVDA,IGRID)
	 RETURN
      ENDIF
C
      CALL SGWF2HUF7PNT(IGRID)
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD = Z
      IF ( IHUFCB.LT.Z .AND. ICBCFL.NE.Z ) IBD = -1
      IF ( IHUFCB.GT.Z ) IBD = ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      CHIN = DZ
      CHOUT = DZ
      IBDLBL = Z
C
C3------CLEAR BUFFER.
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            BUFF(J, I, K) = DZ
          ENDDO
        ENDDO
      ENDDO
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF ( IBD.EQ.2 ) THEN
        NCH = Z
        DO K = 1, NLAY
          DO I = 1, NROW
            DO J = 1, NCOL
              IF ( IBOUND(J,I,K).LT.Z ) NCH = NCH + 1
            ENDDO
          ENDDO
        ENDDO
        CALL UBDSV2(KSTP, KPER, TEXT, IHUFCB, NCOL, NROW, NLAY, NCH,    
     +              IOUT, DELT, PERTIM, TOTIM, IBOUND)
      ENDIF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF ( IBOUND(J,I,K).LT.Z ) THEN
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
              X1 = DZ
              X2 = DZ
              X3 = DZ
              X4 = DZ
              X5 = DZ
              X6 = DZ
CBARC**ADD X7 FOR CONDUIT
              X7 = DZ
              XX7 = DZ
              CHCH1 = DZ
              CHCH2 = DZ
              CHCH3 = DZ
              CHCH4 = DZ
              CHCH5 = DZ
              CHCH6 = DZ
C 
              IF ( ILVDA.GT.Z ) CALL SGWF2HUF7VDF9(I, J, K, VDHT, HNEW, 
     +             IBOUND, NLAY, NROW, NCOL, DFL, DFR, DFT, DFB)
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
              IF ( J.NE.1 ) THEN
                IF ( IBOUND(J-1,I,K).NE.Z ) THEN
                  IF ( IBOUND(J-1,I,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
C
C7B-----CALCULATE FLOW THROUGH THE LEFT FACE.
                    IF ( ILVDA.GT.Z ) THEN
                      CHCH1 = -DFL
                    ELSE
                      HDIFF = HNEW(J, I, K) - HNEW(J-1, I, K)
                      CHCH1 = HDIFF*CR(J-1, I, K)
                    ENDIF
                    IF ( IBOUND(J-1,I,K).GE.Z ) THEN
                      X1 = CHCH1
                      XX1 = X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
                      IF ( X1.LT.DZ ) THEN
                        CHOUT = CHOUT - XX1
!RSR                  ELSEIF ( X1.NE.DZ ) THEN
                      ELSE
                        CHIN = CHIN + XX1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
              IF ( J.NE.NCOL ) THEN
                IF ( IBOUND(J+1,I,K).NE.Z ) THEN
                  IF ( IBOUND(J+1,I,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    IF ( ILVDA.GT.Z ) THEN
                      CHCH2 = DFR
                    ELSE
                      HDIFF = HNEW(J, I, K) - HNEW(J+1, I, K)
                      CHCH2 = HDIFF*CR(J, I, K)
                    ENDIF
                    IF ( IBOUND(J+1,I,K).GE.Z ) THEN
                      X2 = CHCH2
                      XX2 = X2
                      IF ( X2.LT.DZ ) THEN
                        CHOUT = CHOUT - XX2
!RSR                  ELSEIF ( X2.NE.DZ ) THEN
                      ELSE
                        CHIN = CHIN + XX2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
              IF ( I.NE.1 ) THEN
                IF ( IBOUND(J,I-1,K).NE.Z ) THEN
                  IF ( IBOUND(J,I-1,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    IF ( ILVDA.GT.Z ) THEN
                      CHCH3 = -DFT
                    ELSE
                      HDIFF = HNEW(J, I, K) - HNEW(J, I-1, K)
                      CHCH3 = HDIFF*CC(J, I-1, K)
                    ENDIF
                    IF ( IBOUND(J,I-1,K).GE.Z ) THEN
                      X3 = CHCH3
                      XX3 = X3
                      IF ( X3.LT.DZ ) THEN
                        CHOUT = CHOUT - XX3
!RSR                  ELSEIF ( X3.NE.DZ ) THEN
                      ELSE
                        CHIN = CHIN + XX3
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
              IF ( I.NE.NROW ) THEN
                IF ( IBOUND(J,I+1,K).NE.Z ) THEN
                  IF ( IBOUND(J,I+1,K).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    IF ( ILVDA.GT.Z ) THEN
                      CHCH4 = DFB
                    ELSE
                      HDIFF = HNEW(J, I, K) - HNEW(J, I+1, K)
                      CHCH4 = HDIFF*CC(J, I, K)
                    ENDIF
                    IF ( IBOUND(J,I+1,K).GE.Z ) THEN
                      X4 = CHCH4
                      XX4 = X4
                      IF ( X4.LT.DZ ) THEN
                        CHOUT = CHOUT - XX4
!RSR                  ELSEIF ( X4.NE.DZ ) THEN
                      ELSE
                        CHIN = CHIN + XX4
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
              IF ( K.NE.1 ) THEN
                IF ( IBOUND(J,I,K-1).NE.Z ) THEN
                  IF ( IBOUND(J,I,K-1).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HD = HNEW(J, I, K)
                    IF ( LTHUF(K).NE.Z ) THEN
                      TMP = HD
                      TOP = BOTM(J, I, LBOTM(K)-1)
                      IF ( TMP.LT.TOP ) HD = TOP
                    ENDIF
                    HDIFF = HD - HNEW(J, I, K-1)
                    CHCH5 = HDIFF*CV(J, I, K-1)
                    IF ( IBOUND(J,I,K-1).GE.Z ) THEN
                      X5 = CHCH5
                      XX5 = X5
                      IF ( X5.LT.DZ ) THEN
                        CHOUT = CHOUT - XX5
!RSR                  ELSEIF ( X5.NE.DZ ) THEN
                      ELSE
                        CHIN = CHIN + XX5
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
              IF ( K.NE.NLAY ) THEN
                IF ( IBOUND(J,I,K+1).NE.Z ) THEN
                  IF ( IBOUND(J,I,K+1).GE.Z .OR. ICHFLG.NE.Z ) THEN
                    HD = HNEW(J, I, K+1)
                    IF ( LTHUF(K+1).NE.Z ) THEN
                      TMP = HD
                      TOP = BOTM(J, I, LBOTM(K+1)-1)
                      IF ( TMP.LT.TOP ) HD = TOP
                    ENDIF
                    HDIFF = HNEW(J, I, K) - HD
                    CHCH6 = HDIFF*CV(J, I, K)
                    IF ( IBOUND(J,I,K+1).GE.Z ) THEN
                      X6 = CHCH6
                      XX6 = X6
                      IF ( X6.LT.DZ ) THEN
                        CHOUT = CHOUT - XX6
!RSR                  ELSEIF ( X6.NE.DZ ) THEN
                      ELSE
                        CHIN = CHIN + XX6
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
              RATE = CHCH1 + CHCH2 + CHCH3 + CHCH4 + CHCH5 + CHCH6
 
CBIRK  CALCULATE FLOW TO OR FROM CONDUIT
              IF ( NCOND(J,I,K).NE.Z ) THEN
                NNCOND = NCOND(J, I, K)
                HDIFF = B(NNCOND) - HNEW(J, I, K)
                X7 = -MOD2(NNCOND)*HDIFF
                XX7 = X7
                IF ( XX7.LT.DZ ) THEN
                  CHOUT = CHOUT - XX7
                ELSEIF ( XX7.GT.DZ ) THEN
                  CHIN = CHIN + XX7
                ENDIF
              ENDIF
C
C--BIRK ADD CONDUIT FLOW X7 TO RATE
              RATE = RATE + XX7
              RRATE = RATE
              BUFF(J, I, K) = RRATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
              IF ( IBD.LT.Z ) THEN
                IF ( IBDLBL.EQ.Z ) WRITE (IOUT, 9001) TEXT, KPER, KSTP
                WRITE (IOUT, 9002) K, I, J, RATE
                IBDLBL = 1
              ENDIF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
C
C--BARC**PASS RATE AS SINGLE PRECISION
              IF ( IBD.EQ.2 ) CALL UBDSVA(IHUFCB, NCOL, NROW, J, I, K,  
     +                                    RRATE, IBOUND, NLAY)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF ( IBD.EQ.1 ) CALL UBUDSV(KSTP, KPER, TEXT, IHUFCB, BUFF, NCOL, 
     +                            NROW, NLAY, IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN = CHIN
      COUT = CHOUT
      VBVL(1, MSUM) = VBVL(1, MSUM) + CIN*DELT
      VBVL(2, MSUM) = VBVL(2, MSUM) + COUT*DELT
      VBVL(3, MSUM) = CIN
      VBVL(4, MSUM) = COUT
      VBNM(MSUM) = TEXT
      MSUM = MSUM + 1
C 
 9001 FORMAT (1X, /, 1X, A, '   PERIOD', I3, '   STEP', I3)
 9002 FORMAT (1X, 'LAYER', I3, '   ROW', I4, '   COL', I4, '   RATE',   
     +        1PG15.6)
C
C18-----RETURN.
      END SUBROUTINE CFPHUF7BDCH
C
C
C
C
C
      SUBROUTINE GWF2CFP1BD(KPER, KSTP, INCOC, MSUM)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR CONDUITS
C
C     THIS SUBROUTINE CALCULATES RATES AND VOLUMES TRANSFERRED BETWEEN 
C     THE FISSURED SYSTEM (MODFLOW) AND THE CONDUIT SYSTEM (CONDUIT).
C
C     !TR: CADS (CONDUIT ASSOCIATED DRAINABLE STORAGE) FLOW COMPUTED
C     !TR: HERE; CADS IS PART OF THE CONDUIT SYSTEM AND NOT CONSIDERED
C     !TR: HERE
C
C     ******************************************************************
C     20JAN1998 CON5BD
C
      USE CONSTANTS, ONLY: DZ, Z
      USE CFPMODULE, ONLY:MXNODE, NBR, B, MOD2, MATFLOW, NODEBOT,  
     +    QMAT, MODE, KTSNO, KTSTU, B_MAT, B_MAT_O, CADSFLOW, CADS2,    !TR: 2012 04 26 ADDED CADSFLOW, CADS2
     +    PFPSFLOW, PFPS2, ICY, CONDCY, HCY, CYFLOW,TSA_FLG,CADS_FLG,
     +    CADSML_FLG, CADSML2, B_MAT_OTR,                               !TR: 2012 07 16 ADDED PFPSFLOW, PFPS2 / 2013 03 18 CAUCHY // 2013 05 13 TSA OUTPUT // 2013 06 28 CADSML
     +    NO_L_NTS, L_NTS                                               !TR: 2018 05 22 LISTING OUTPUT

      USE GLOBAL, ONLY:HNEW, NCOL, NROW, NLAY, IOUT, IBOUND, BUFF

      USE GWFBASMODULE, ONLY:DELT, PERTIM, TOTIM, VBVL, VBNM, ICBCFL

      IMPLICIT NONE
      EXTERNAL CONBUD, NODEBUD, FLOW_OUTPUT, BUDGET_OUTPUT, NWBOUT
      EXTERNAL TS1OUT
C
C--INPUT:
C     DELT                      LENGTH OF THE CURRENT TIME STEP
C     HCON(MXNODE)              HEAD IN THE CONDUIT SYSTEM [L]
C     HNEW(NCOL,NROW,NLAY)      HEAD IN THE FISSURED SYSTEM [L]
C     IBOUND(NCOL,NROW,NLAY)    STATUS OF EACH CELL (SEE MODFLOW DOCUMENTATION)
C     ICBCFL                    FLAG FOR SAVING OR PRINTING CELL-BY-CELL FLOW TERMS (SEE
C		                        MODFLOW DOCUMENTATION: OUTPUT CONTROL)
C     ICONCB                    FLAG AND A UNIT NUMBER:
C		>0                      UNIT NUMBER ON WHICH CELL-BY-CELL FLOW TERMS
C		                        WILL BE RECORDED WHENEVER ICBCFL IS SET
C		=0                      CELL-BY-CELL FLOW TERMS WILL NOT BE PRINTED
C		                        OR RECORDED
C		<0                      EXCHANGE BETWEEN FISSURED AND CONDUIT SYSTEM
C		                        WILL BE PRINTED WHENEVER ICBCFL IS SET
C     IOUT                      UNIT NUMBER FOR WRITING THE LISTING FILE
C     KPER                      STRESS PERIOD COUNTER
C     KSTP                      TIME STEP COUNTER
C     MOD(MXNODE)               EXCHANGE COEFFICIENT FOR EXCHANGE BETWEEN CONDUIT AND
C                               FISSURED SYSTEM [L**2/T]
C     MXNODE                    NUMBER OF NODES (CONDUIT SYSTEM)
C     NCOL                      NUMBER OF COLUMNS
C     NLAY                      NUMBER OF LAYERS
C     NROW                      NUMBER OF ROWS
C
C--OUTPUT:
C     MATFLOW(MXNODE)           ARRAY IN WHICH EXCHANGE RATES ARE SAVED [L**3/T]
C     MSUM                      COUNTER FOR BUDGET TERMS
C     VBNM(MSUM)                LABELS FOR BUDGET TERMS
C     VBVL(4,MSUM)              FLOWS FOR THE VOLUMETRIC BUDGET TERM N:
C         (1,N)                 TOTAL FLOW INTO THE FISSURED SYSTEM DURING THE SIMULATION
C         (2,N)                 TOTAL FLOW OUT OF THE FISSURED SYSTEM DURING THE SIMULATION
C         (3,N)                 FLOW RATE INTO THE FISSURED SYSTEM FOR THE CURRENT TIME STEP
C         (4,N)                 FLOW RATE OUT OF THE FISSURED SYSTEM FOR THE CURRENT TIME STEP
C
C--INTERNAL:
C     BUFF(NCOL,NROW,NLAY)      BUFFER USED TO ACCUMULATEM INFORMATION
C     HHCON                     HCON(IC,IR,IL)
C     HHNEW                     HNEW(IC,IR,IL)
C     I                         INDEX FOR NODES
C     IBD                       CELL-BY-CELL BUDGET FLAG
C     IBDLBL                    FLAG USED WHEN PRINTING CELL-BY-CELL BUDGET VALUES IN THE
C                               LISTING FILE SO THAT THE LABEL IS PRINTED ONLY ONCE
C     IC                        INDEX FOR COLUMNS
C     IL                        INDEX FOR LAYERS
C     IR                        INDEX FOR ROWS
C     RATE                      EXCHANGE RATE INTO THE CELL OF THE FISSURED SYSTEM
C                               FROM THE NODE OF THE CONDUIT SYSTEM [L**3/T]
C     RATIN                     ACCUMULATOR TO WHICH INFLOWS ARE ADDED [L**3/T]
C     RATOUT                    ACCUMULATOR TO WHICH OUTFLOWS ARE ADDED [L**3/T]
C     TEXT                      LABEL THAT IDENTIFIES CONDUIT DATA
C
C--ARGUMENTS
      INTEGER, INTENT(IN) :: KPER, KSTP, INCOC
      INTEGER, INTENT(INOUT) :: MSUM
C
C--LOCAL VARIABLES
      INTEGER I, IBD, IBDLBL, IC, IL, IR, ICONCB, J
      DOUBLE PRECISION RATE, RATIN, RATOUT, HHNEW, HHCON
      CHARACTER(LEN=16) TEXT
      REAL:: RRATE
C-----------------------------------------------------------------------
      DATA TEXT/'           PIPES'/
C
      IF ( MODE.EQ.2 ) RETURN
C
C--BARC**FOR NOW SET ICONCB TO 0, LATER THIS WILL BE READ FROM FILE
      ICONCB = Z
C 
C--INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD),IBDLBL AND ACCUMULATORS
C  (RATIN AND RATOUT)
      RATIN = DZ
      RATOUT = DZ
      IBDLBL = Z
      IBD = Z
      IF ( ICBCFL.NE.Z .AND. ICONCB.LT.Z ) IBD = -1
      IF ( ICONCB.GT.Z ) IBD = ICBCFL
C      
C---IF CELL-BY-CELL FLOW TERMS ARE SAVED AS A LIST, WRITE HEADER
      IF ( IBD.EQ.2 ) CALL UBDSV2(KSTP, KPER, TEXT, ICONCB, NCOL, NROW, 
     +                            NLAY, MXNODE, IOUT, DELT, PERTIM,     
     +                            TOTIM, IBOUND)
C---CLEAR BUFFER
      DO IL = 1, NLAY
        DO IR = 1, NROW
          DO IC = 1, NCOL
            BUFF(IC, IR, IL) = Z
          ENDDO
        ENDDO
      ENDDO
C
C---FOR EACH CONDUIT NODE ACCUMULATE EXCHANGE FLOW
      DO I = 1, MXNODE
C
C---GET LAYER, ROW, COLUMN OF CELL CONTAINING CONDUIT NODE
        IC = NBR(I, 2)
        IR = NBR(I, 3)
        IL = NBR(I, 4)
C        
C---IF CELL IS NO FLOW MOVE TO NEXT NODE
        IF ( IBOUND(IC,IR,IL).NE.Z ) THEN
C
C---GET HEADS OF FISSURED AND CONDUIT SYSTEM
 
          HHNEW = HNEW(IC, IR, IL)
C
C--BARC**FOR PERCHED CASES**
          IF ( HHNEW.LT.NODEBOT(I) ) HHNEW = NODEBOT(I)
C
C--BARC**FOR DRY CONDUIT CASES**
          HHCON = B(I)
          IF ( HHCON.LT.NODEBOT(I) ) HHCON = NODEBOT(I)
          RATE = DZ
C
C---CALCULATE EXCHANGE RATE
C--BARC**NEED SOMETHING IN HERE FOR PERCHED CASES, AND OTHERS ?
          RATE = MOD2(I)*(HHCON-HHNEW) 
CB          PRINT*, RATE,MOD2(I),HHCON,HHNEW
C
C--BARC***ADD QMAT FOR BUDGET OUTPUT
          QMAT(I) = RATE
C
C---SAVE EXCHANGE RATE IN MATFLOW
          MATFLOW(I) = RATE
C
C--CALCULATE CADS FLOW RATE AND SAVE IN CADSFLOW           
          IF(CADS_FLG.EQ.1)CADSFLOW(I) = CADS2(I)*(B_MAT(I)-B_MAT_O(I)) !TR: 2012 04 26 CADS / CADSFLOW COMPUTED HERE; SUBSEQUENTLY, B_MAT IS SAVED AS B_MAT_O / 2013 06 28 
          IF(CADSML_FLG.EQ.1)CADSFLOW(I) = CADSML2(I)*(B_MAT(I)-
     +                                     B_MAT_O(I))                  !TR: 2013 06 28 CADSML / CADSFLOW COMPUTED HERE; SUBSEQUENTLY, B_MAT IS SAVED AS B_MAT_O
C
C--CALCULATE PFPS FLOW RATE AND SAVE IN PFPSFLOW
          PFPSFLOW(I) = PFPS2(I)*(B_MAT(I)-B_MAT_O(I))                  !TR: 2012 07 16 PFPS / PFPSFLOW COMPUTED HERE;
C
C--CALCULATE CAUCHY FLOW RATE AND SAVE IN CYFLOW
          DO J=1, MXNODE                                                !TR: 2012 07 16 CAUCHY / CYFLOW COMPUTED HERE
            IF(ICY(J,1).EQ.I) THEN                                      !TR: 2012 07 16 CAUCHY / CYFLOW COMPUTED HERE
              CYFLOW(I) = CONDCY(J)*(B_MAT(I)-HCY(J))                   !TR: 2012 07 16 CAUCHY / CYFLOW COMPUTED HERE
            ENDIF                                                       !TR: 2012 07 16 CAUCHY / CYFLOW COMPUTED HERE
          ENDDO                                                         !TR: 2012 07 16 CAUCHY / CYFLOW COMPUTED HERE
C
C---PRINT THE INDIVIDUAL RATES IF REQUESTED (ICONCB<0)
          IF ( IBD.LT.Z ) THEN
            IF ( IBDLBL.EQ.Z ) WRITE (IOUT, 9001) TEXT, KPER, KSTP
            WRITE (IOUT, 9002) I, IL, IR, IC, RATE
            IBDLBL = 1
          ENDIF
C
C---ADD RATE TO BUFFER
          BUFF(IC, IR, IL) = BUFF(IC, IR, IL) + RATE
C
C---ADD RATE TO RATIN IF FLOW IS INTO FISSURED SYSTEM
          IF ( RATE.GT.DZ ) THEN
            RATIN = RATIN + RATE
C
C---SUBTRACT RATE FROM RATOUT IF FLOW IS INTO CONDUIT SYSTEM
          ELSEIF ( RATE.LT.DZ ) THEN
            RATOUT = RATOUT - RATE
          ENDIF
        ENDIF
        RRATE = RATE
C
C--IF SAVING CELL-BY-CELL FLOWS IN LIST, WRITE FLOW
C--BARC**PASS RATE AS SINGLE
        IF ( IBD.EQ.2 ) CALL UBDSVA(ICONCB, NCOL, NROW, IC, IR, IL,     
     +                              RRATE, IBOUND, NLAY)
      ENDDO
C
C---IF CELL-BY-CELL FLOW TERMS WILL BE SAVED AS 3-D ARRAY
C---CALL UBUDSV TO RECORD THEM
      IF ( IBD.EQ.1 ) CALL UBUDSV(KSTP, KPER, TEXT, ICONCB, BUFF, NCOL, 
     +                            NROW, NLAY, IOUT)
C
C---MOVE RATES, VOLUMES, LABELS INTO ARRAYS FOR PRINTING
      VBVL(3, MSUM) = RATIN
      VBVL(4, MSUM) = RATOUT
      VBVL(1, MSUM) = VBVL(1, MSUM) + RATIN*DELT
      VBVL(2, MSUM) = VBVL(2, MSUM) + RATOUT*DELT
      VBNM(MSUM) = TEXT
C
C---INCREMENT BUDGET TERM COUNTER
      MSUM = MSUM + 1
C
C--BIRK----BUDGET OF THE CONDUIT SYSTEM
      CALL CONBUD
C
C--BIRK----BUDGET OF EVERY NODE
      CALL NODEBUD
C
C--RESET LISTING OUTPUT COUNTER FOR EACH NEW PERIOD
      IF (KSTP.EQ.1) NO_L_NTS = Z                                       !TR: 2018 05 22 
C
C--OUTPUT COUNTER FOR LISTING                                           !TR: 2018 05 22 
      NO_L_NTS = NO_L_NTS + 1
C
C--BIRK----CONDUIT PIPE FLOW
      IF (NO_L_NTS .EQ. L_NTS) CALL FLOW_OUTPUT(KPER, KSTP)             !TR: 2018 05 22 
C
C--OUTPUT OF CONDUIT BUDGET
      CALL BUDGET_OUTPUT(KPER, KSTP)
C
C--WRITE NODE WATER BUDGET AND RESET COUNTER NO_L_NTS
      IF (NO_L_NTS .EQ. L_NTS) THEN                                     !TR: 2018 05 22 
        CALL NWBOUT(KPER, KSTP)
        NO_L_NTS = Z                                                    !TR: 2018 05 22 
      ENDIF                                                             !TR: 2018 05 22 
C
C--TIME SERIES OUTPUT
      IF ( INCOC.NE.Z ) CALL TS1OUT
C
C--TIME SERIES ANALYSIS OUTPUT
      IF (INCOC.NE.Z) THEN
          IF(TSA_FLG) CALL TSA_OUT                        !TR: 2013 05 13 TSA OUTPUT // 2013 06 04 MODIFIED
      END IF
C
C--BARC**PRESERVE OLD HEADS IN CONDUIT.
      B_MAT_OTR = B_MAT_O                                               !TR: 2014 02 28 KEEP OLD HEADS FOR TRANSPORT
      B_MAT_O = B_MAT
C
 9001 FORMAT (1X, /1X, A, '   PERIOD', I3, '   STEP', I3)
 9002 FORMAT (1X, 'NODE', I4, '   LAYER', I3, '   ROW', I4, '   COL',   
     +        I4, '   RATE', 1PG15.6)
C
      END SUBROUTINE GWF2CFP1BD
C
C
C
C
C
      SUBROUTINE CONBUD
C     *****************************************************************
C     CALCULATE THE CONDUIT BUDGET
C     *****************************************************************
C     VERSION 2 10DEZ1998 BUDGET
C     CHANGES BY THOMAS.REIMANN@TU-DRESDEN.DE // 2012 05 11: SPLIT
C     QBDIR / CONSIDER CAD STORAGE / FHLQ BC)// 2014 02 20 CADS 
C     RECHARGE
C                                   
C
      USE CONSTANTS, ONLY: TRUE, FALSE,Z,DZ, DOS, NEARZERO_30,TWOPI
      USE CFPMODULE, ONLY:NNOD, QBDIR, MXNODE, NBR, BEGHEAD,      
     +  MATFLOW, BUD_SOURCE, BUD_SINK, BUD_MATIN, BUD_MATOUT, B,      
     +  TOTAL_BUDGET, BUD_TOTAL_STEP, TS_IN_MAT, TS_OUT, TS_OUT_MAT,    !TR: 2012 04 24 BUD_QBDIR REMOVED
     +  TS_BUD_SINK, TS_BUD_SOURCE, TS_IN,DIFF_TOT, DIFF_TS, TOT_FLOW,  !TR: 2012 04 24 TS_QBDIR REMOVED
     +  TS_TOT_FLOW, ITUBE, DTUBE, GEOHIGHT, CON_DATA, B_MAT_O, B_MAT,  !TR: 2012 04 24 B_MAT ADDED
     +  BUD_STORAGE_IN, BUD_STORAGE_OUT,TS_IN_PFPS, TS_OUT_PFPS,        !TR: 2012 07 16 REMOVED TS_BUD_STORAGE
     +  NODE_LOCA,TORTUOS, ONE8TH, BUD_QBDIRIN,
     +  BUD_QBDIROUT, TS_IN_QBDIR, TS_OUT_QBDIR, BUD_CADSIN,            !TR: 2012 04 24 VARIABLES ADDED TO DISTIGUISH RECHARGE IN- AND OUT / VARIABLES TO DISTIGUISH RECHARGE IN- AND OUT
     +  BUD_CADSOUT ,TS_IN_CADS, TS_OUT_CADS, CADSFLOW, BUD_FHLQIN,     !TR: 2012 04 24 CADS
     +  BUD_CDSCONIN,BUD_CDSCONOUT ,TS_IN_CDSCON, TS_OUT_CDSCON,        !TR: 2014 04 16 CADS
     +  BUD_FHLQOUT, TS_IN_FHLQ, TS_OUT_FHLQ, QFHLQ, BUD_WELLIN,        !TR: 2012 05 11 FHLQ
     +  BUD_WELLOUT, TS_IN_WELL, TS_OUT_WELL, QWELL, PFPSFLOW, KU_IN,
     +  KU_OUT, BUD_CYIN, BUD_CYOUT, TS_IN_CY, TS_OUT_CY, QCYLQ, CYFLOW,!TR: 2012 06 08 WELLS // 2012 07 16 PFPS // 2012 07 17 KUMULATIVE TERMS // 2013 03 14 CAUCHY
     +  BUD_CYLQIN, BUD_CYLQOUT, TS_IN_CYLQ, TS_OUT_CYLQ,LH_FLG,ILH,    !TR: 2013 03 23 LH
     +  TS_IN_LH, TS_OUT_LH, BUD_LHIN, BUD_LHOUT, BUD_QSDIROUT,         !TR: 2014 02 20 CADS RECHARGE
     +  BUD_QSDIRIN, TS_IN_QSDIR, TS_OUT_QSDIR, QSDIR                   !TR: 2014 02 20 CADS RECHARGE
C
      USE GWFBASMODULE, ONLY:TOTIM, DELT                                !TR: 2012 04 24 ADDED DELT FOR MASS BALANCES
C
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT, ASIN, SIN
C
C--INPUT
C     MXNODE  	        NUMBER OF NODES
C     MAT_DIR	        STORE EXCHANGE COMING IN (> 0) OR OUT (< 0)
C		                OF THE MATRIX
C     QBDIR  	        FLOW RATE DIRECTLY IN CONDUIT (DIRECT RECHARGE IN CONDUIT)
C     BEG_HEAD 	        CONTAINING THE HEADS READ FROM FILE
C     TUBE  	        MATRIX WITH TUBE NR., NODE_BEGIN, NODE_END
C        	            Q M/S (IF NODE_BEGIN PRESSURE > NODE_END PRESSURE THEN Q >= 0),
C     CHEMISTRY:        0 NOT CALCULATED, 1 CALCULATED
C     GEOTIMES	        DURATION OF THE 'CHEMICAL TIMESTEP' OF EVERY STRESS
C		                PERIOD
C--OUTPUT
C     KU_QBDIR          CUMULATIVE FLOW RATE DIRECTLY IN CONDUIT
C     BUD_SOURCE        FLOW OF SOURCE
C     BUD_SINK          FLOW OF SINK = FLOW OUT OF THE CONDUIT SYSTEM
C     BUD_MATIN         TOTAL FLOW IN CONDUIT SYSTEM COMING FROM MATRIX
C     BUD_MATOUT        TOTAL FLOW IN MATRIX
C     BUD_QBDIR         TOTAL DIRECT INFLOW IN CONDUIT SYSTEM
C     TOTAL_BUDGET      TOTAL BUDGET DIFFERENCE
C     BUD_TOTAL_STEP    TOTAL BUDGET DIFFERENCE IN THE LAST TIMESTEP
C
C--LOCAL VARIABLES
      INTEGER I, ITEST, J, FIX_NODE, FLOW_TUBE
      LOGICAL LH_NODE                                                   !TR: 2013 03 23 LH; LH_NODE = TRUE IF THE NODE IS A LH BOUNDARY
      DOUBLE PRECISION WETTDNEW, WETTDOLD, VNEW, VOLD, MODTOURT         !TR: 2012 07 17 REMOVED VCHANGE
      DOUBLE PRECISION DKNOT, THETA, FIXBUDGET
      DOUBLE PRECISION TUBETOP1, TUBEBOT1, LTOURT
      !DOUBLE PRECISION X, Y, Z, LENGTH
C-----------------------------------------------------------------------
C
C--INITIALIZE
      LH_NODE = FALSE
C
C--SEBA--CALCULATE RATES FOR TIMESTEP
      TS_IN_MAT = DZ
      TS_OUT_MAT = DZ
      TS_IN_QBDIR = DZ                                                !TR: 2012 04 24 REPLACED TS_QBDIR TO DISTIGUISH RECHARGE IN- AND OUT
      TS_OUT_QBDIR = DZ                                               !TR: 2012 04 24 REPLACED TS_QBDIR TO DISTIGUISH RECHARGE IN- AND OUT
      TS_IN_QSDIR = DZ                                                !TR: 2014 02 20 CADS RECHARGE
      TS_OUT_QSDIR = DZ                                               !TR: 2014 02 20 CADS RECHARGE
      TS_BUD_SINK = DZ
      TS_BUD_SOURCE = DZ
      TS_IN_PFPS = DZ                                                 !TR: 2012 07 16 PFPS
      TS_OUT_PFPS = DZ                                                !TR: 2012 07 16 PFPS
      TS_IN_CADS = DZ                                                 !TR: 2012 04 26 CADS
      TS_OUT_CADS = DZ                                                !TR: 2012 04 26 CADS
      TS_IN_CDSCON = DZ                                               !TR: 2014 04 16 CADS RCH
      TS_OUT_CDSCON = DZ                                              !TR: 2014 04 16 CADS RCH
      TS_IN_FHLQ = DZ                                                 !TR: 2012 05 11 FHLQ
      TS_OUT_FHLQ = DZ                                                !TR: 2012 05 11 FHLQ
      TS_IN_WELL = DZ                                                 !TR: 2012 06 08 WELLS
      TS_OUT_WELL = DZ                                                !TR: 2012 06 08 WELLS
      TS_IN_CY = DZ                                                   !TR: 2013 03 14 CAUCHY
      TS_OUT_CY = DZ                                                  !TR: 2013 03 14 CAUCHY
      TS_IN_CYLQ = DZ                                                 !TR: 2013 03 14 CAUCHY LQ
      TS_OUT_CYLQ = DZ                                                !TR: 2013 03 14 CAUCHY LQ
      TS_IN_LH = DZ                                                 !TR: 2013 03 25 LH
      TS_OUT_LH = DZ                                                !TR: 2013 03 25 LH
C
C--GO THROUGH ALL NODES
      DO I = 1, MXNODE
C
C--CHECK IF NODE IS LH
        IF (LH_FLG) THEN    
          LH_NODE = FALSE
            DO J=1, MXNODE
              IF((ILH(J,1).EQ.NBR(I,1)).AND.(ILH(J,2).EQ.1))
     +          LH_NODE = TRUE
            ENDDO   
        ENDIF 
C
C--DIRECT RECHARGE IN CONDUIT SYSTEM
!       TS_QBDIR = TS_QBDIR + QBDIR(I)
        IF (QBDIR(I).LT.DZ) THEN                                      !TR: 2012 04 26 DISTINGUISH IN RECHARGE IN- AND OUT
          TS_OUT_QBDIR = TS_OUT_QBDIR + QBDIR(I)
        ELSE
          TS_IN_QBDIR = TS_IN_QBDIR + QBDIR(I)
        ENDIF
C
C--DIRECT RECHARGE IN CADS
        IF (QSDIR(I).LT.DZ) THEN                                      !TR: 2012 04 26 DISTINGUISH IN RECHARGE IN- AND OUT
          TS_OUT_QSDIR = TS_OUT_QSDIR + QSDIR(I)
        ELSE
          TS_IN_QSDIR = TS_IN_QSDIR + QSDIR(I)
        ENDIF        
C
C--MATRIX IN- AND OUT-FLOW
        IF ( MATFLOW(I).LT.DZ ) THEN
          TS_IN_MAT = TS_IN_MAT + (-1.D0*MATFLOW(I))
        ELSE
          TS_OUT_MAT = TS_OUT_MAT + (-1.D0*MATFLOW(I))
        ENDIF
C
C--CAD STORAGE CHANGE                                                   !TR: 2012 04 26 CADS / 
        IF ( CADSFLOW(I).LT.DZ ) THEN                                 !TR: 2012 04 26 CADS /
          TS_IN_CADS = TS_IN_CADS + (-1.D0*CADSFLOW(I))                 !TR: 2012 04 26 CADS /
        ELSE                                                            !TR: 2012 04 26 CADS /
          TS_OUT_CADS = TS_OUT_CADS + (-1.D0*CADSFLOW(I))               !TR: 2012 04 26 CADS /
        ENDIF                                                           !TR: 2012 04 26 CADS /
C
C--CADS <-> CONDUIT TRANSFER                                            !TR: 2014 04 16 CADS RCH
        IF ( CADSFLOW(I)-QSDIR(I).LT.DZ ) THEN                        !TR: 2014 04 16 CADS RCH
          TS_IN_CDSCON = TS_IN_CDSCON + (-1.D0*(CADSFLOW(I)-QSDIR(I)))  !TR: 2014 04 16 CADS RCH
        ELSE                                                            !TR: 2014 04 16 CADS RCH
          TS_OUT_CDSCON = TS_OUT_CDSCON + (-1.D0*(CADSFLOW(I)-QSDIR(I)))!TR: 2014 04 16 CADS RCH
        ENDIF                                                           !TR: 2014 04 16 CADS RCH
C
C--ADD PFPSFLOW FOR BUDGET OUTPUT
        IF ( PFPSFLOW(I).LT.DZ ) THEN                                 !TR: 2012 07 16 PFPS /
          TS_IN_PFPS = TS_IN_PFPS + (-1.D0*PFPSFLOW(I))                 !TR: 2012 07 16 PFPS /
        ELSE                                                            !TR: 2012 07 16 PFPS /
          TS_OUT_PFPS = TS_OUT_PFPS + (-1.D0*PFPSFLOW(I))               !TR: 2012 07 16 PFPS /
        ENDIF           
C
C--CAUCHY IN- AND OUT-FLOW
        IF ( CYFLOW(I).LT.DZ ) THEN
          TS_IN_CY = TS_IN_CY + (-1.D0*CYFLOW(I))
        ELSE
          TS_OUT_CY = TS_OUT_CY + (-1.D0*CYFLOW(I))
        ENDIF        
C
C--FHLQ IN- AND OUT-FLOW                                                !TR: 2012 05 11 ADDED FHLQ
        IF ( QFHLQ(I).LT.DZ ) THEN                                    !TR: 2012 05 16 FHLQ
          TS_OUT_FHLQ = TS_OUT_FHLQ + QFHLQ(I)                          !TR: 2012 05 16 FHLQ
        ELSE                                                            !TR: 2012 05 16 FHLQ
          TS_IN_FHLQ = TS_IN_FHLQ + QFHLQ(I)                            !TR: 2012 05 16 FHLQ
        ENDIF                                                           !TR: 2012 05 16 FHLQ
C
C--WELL IN- AND OUT-FLOW                                                !TR: 2012 06 08 WELLS
        IF ( QWELL(I).LT.DZ ) THEN                                    !TR: 2012 06 08 WELLS
          TS_OUT_WELL = TS_OUT_WELL + QWELL(I)                          !TR: 2012 06 08 WELLS
        ELSE                                                            !TR: 2012 06 08 WELLS
          TS_IN_WELL = TS_IN_WELL + QWELL(I)                            !TR: 2012 06 08 WELLS
        ENDIF                                                           !TR: 2012 06 08 WELLS
C
C--CAUCHY LQ IN- AND OUT-FLOW                                           !TR: 2013 03 14 CAUCHY
        IF ( QCYLQ(I).LT.DZ ) THEN                                    !TR: 2013 03 14 CAUCHY
          TS_OUT_CYLQ = TS_OUT_CYLQ + QCYLQ(I)                          !TR: 2013 03 14 CAUCHY
        ELSE                                                            !TR: 2013 03 14 CAUCHY
          TS_IN_CYLQ = TS_IN_CYLQ + QCYLQ(I)                            !TR: 2013 03 14 CAUCHY
        ENDIF                                                           !TR: 2013 03 14 CAUCHY
!C
!C--BARC**STORAGE CHANGES FOR PARTIALLY-FILLED CONDUITS
!        DO J = 1, NNOD
!          ITEST = NBR(I, J+4+NNOD)
!C
!C--BARC**ITEST IS A TUBE NUMBER !!!
!          IF ( ITEST.NE.0 ) THEN
!            TUBETOP1 = GEOHIGHT(I) + CON_DATA(ITEST, 2)/DOS
!            TUBEBOT1 = GEOHIGHT(I) - CON_DATA(ITEST, 2)/DOS
!C
!C--BARC**ONLY IN PARTIALLY FILLED CASES // !TR: CHECK ALSO FOR PARTIALLY 
!C  FILLED PIPES IN THE PREVIOUS TS
!            IF ((B_MAT(I).LT.TUBETOP1 .AND. B_MAT(I).GT.TUBEBOT1).OR.   !TR: 2012 06 08 REPLACED B BY B_MAT
!     +          (B_MAT_O(I).LT.TUBETOP1.AND.B_MAT_O(I).GT.TUBEBOT1))THEN!TR: 2012 06 08 CONSIDER REFILLING 
!C
!C--BARC**DETERMINE L (LENGTH)
!              X = DABS(NODE_LOCA(I,2)-NODE_LOCA(NBR(I,J+4),2))          !TR: 2012 06 08 J+4 INSTEAD OF J
!              Y = DABS(NODE_LOCA(I,3)-NODE_LOCA(NBR(I,J+4),3))          !TR: 2012 06 08 J+4 INSTEAD OF J
!              Z = DABS(NODE_LOCA(I,4)-NODE_LOCA(NBR(I,J+4),4))          !TR: 2012 06 08 J+4 INSTEAD OF J
!              LENGTH = 1.D0
!C
!C--BARC**FOR CASES OF Z=0
!              IF ( Z.LT.NEARZERO ) THEN
!                IF ( X.LT.NEARZERO .AND. Y.GT.DZ ) THEN
!                  LENGTH = Y/DOS
!                ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO ) THEN
!                  LENGTH = X/DOS
!                ELSE
!                  LENGTH = DSQRT(X**DOS+Y**DOS)/DOS
!                ENDIF
!C
!C--BARC**FOR CASES OF Z>0
!              ELSEIF ( X.LT.NEARZERO .AND. Y.GT.DZ ) THEN
!                LENGTH = DSQRT(Y**DOS+Z**DOS)/DOS
!              ELSEIF ( X.GT.DZ .AND. Y.LT.NEARZERO ) THEN
!                LENGTH = DSQRT(X**DOS+Z**DOS)/DOS
!              ELSE
!                LENGTH = Z
!              ENDIF
!C  
!              MODTOURT = TORTUOS(ITEST)                                 !TR: 2012 06 08 NOT SURE ABOUT THIS / NOT USED
!              LTOURT = (MODTOURT-1.D0)/DOS + 1.0D0                      !TR: 2012 06 08 NOT SURE ABOUT THIS / NOT USED
!              LENGTH = LENGTH * TORTUOS(ITEST)                          !TR: 2012 06 08 COMPUTE LENGTH
!              WETTDNEW = B_MAT(I) - TUBEBOT1
!              DKNOT = CON_DATA(ITEST, 2)
!              THETA = DOS*ASIN(DOS*DSQRT((DKNOT/DOS)**DOS               
!     +                -(WETTDNEW-DKNOT/DOS)**DOS)/DKNOT)
!              IF ( B_MAT(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA     !TR: 2012 06 08 REPLACED B BY B_MAT
!              IF (WETTDNEW.GE.CON_DATA(ITEST,2)) THETA = TWOPI         !TR: 2012 06 08 CONSIDER FILLED PIPE / PREVENT 'NAN' = FULL CASE
!              IF (WETTDNEW.LE.DZ) THETA = DZ                        !TR: 2012 06 11 CONSIDER DRY PIPE / PREVENT 'NAN' = DRY CASE
!              VNEW = ONE8TH*(THETA-SIN(THETA))*DKNOT**DOS*LENGTH        !TR: 2012 06 08 REPLACED LTOURT BY LENGTH
!C
!C--BARC**LAST ITERATION.
!              WETTDOLD = B_MAT_O(I) - TUBEBOT1
!              THETA = DOS*ASIN(DOS*DSQRT((DKNOT/DOS)**DOS               
!     +                -(WETTDOLD-DKNOT/DOS)**DOS)/DKNOT)
!              IF ( B_MAT_O(I).GE.GEOHIGHT(I) ) THETA = TWOPI - THETA
!              IF (WETTDOLD.GE.CON_DATA(ITEST,2)) THETA = TWOPI         !TR: 2012 06 08 CONSIDER FILLED PIPE / PREVENT 'NAN'
!              IF (WETTDOLD.LE.DZ) THETA = DZ                        !TR: 2012 06 11 CONSIDER DRY PIPE / PREVENT 'NAN' = DRY CASE              
!              VOLD = ONE8TH*(THETA-SIN(THETA))*DKNOT**DOS*LENGTH        !TR: 2012 06 08 REPLACED LTOURT BY LENGTH
!              VCHANGE = VNEW - VOLD
!              TS_BUD_STORAGE = TS_BUD_STORAGE + VCHANGE / DELT          !TR: 2012 06 08 DIVIDE VCHANGE BY DELT TO GET FLOW PER TIME
!C
!C--BARC**ENDIF FOR PARTIALLY FILLED STATEMENT
!            ENDIF
!C
!C--BARC**ENDIF FOR ITEST STATEMENT (ALL TUBES COMING TO THE NODE)
!          ENDIF
!C
!C--BARC**ENDDO FOR NNOD DO
!        ENDDO
!C
!C--BARC**ADD PFPSFLOW (PREVIOUS VERSIONS = QSTOR) FOR BUDGET OUTPUT
!        IF ( PFPSFLOW(I).GT.DZ ) THEN
!          TS_OUT_PFPS = TS_OUT_PFPS + PFPSFLOW(I)
!        ELSE
!          TS_IN_PFPS = TS_IN_PFPS + PFPSFLOW(I)
!        ENDIF                
C
C--BARC**WHAT ABOUT DRY CASES?
C 
C--BUDGET OF SOURCES AND SINKS
!RSR    IF ( BEGHEAD(I).NE.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(I)+1.D0).GT.NEARZERO_30 ) THEN
C
          FIX_NODE = I
!         QMIXBUD - 0.D0
C
C--FLOW INTO THE FIXPOTENTIAL FROM FISSURED SYSTEM
          FIXBUDGET = 1.D0*MATFLOW(I)
C
C--FLOW INTO THE FIXPOTENTIAL FROM CAUCHY                               !TR: 2013 03 18 CAUCHY / CHECK - REMOVE?
          FIXBUDGET = FIXBUDGET + CYFLOW(I)                             !TR: 2013 03 18 CAUCHY / CHECK - REMOVE?
C
C--ADD FLOW OF DIRECT RECHARGE INTO FIXED POTENTIAL
          FIXBUDGET = FIXBUDGET - QBDIR(I)
C
C--ADD FLOW OF DIRECT CADS RECHARGE INTO FIXED POTENTIAL                !TR: 2014 04 16 CADS RCH
          FIXBUDGET = FIXBUDGET - QSDIR(I)                              !TR: 2014 04 16 CADS RCH
C
C--ADD CADS FLOW INTO FIXED POTENTIAL                                   !TR: 2012 05 11 CADS /
          FIXBUDGET = FIXBUDGET + CADSFLOW(I)                           !TR: 2012 04 26 CADS /
C
C--ADD PFPS FLOW INTO FIXED POTENTIAL                                   !TR: 2012 07 16 PFPS /
          FIXBUDGET = FIXBUDGET + PFPSFLOW(I)                           !TR: 2012 04 26 PFPS / CONSIDER PFPSFLOW TOO
C
C--ADD FLOW OF FHLQ INTO FIXED POTENTIAL                                !TR: 2012 05 11 FHLQ
          FIXBUDGET = FIXBUDGET - QFHLQ(I)                              !TR: 2012 05 11 FHLQ 
C
C--ADD FLOW OF WELL INTO FIXED POTENTIAL                                !TR: 2012 06 08 WELL
          FIXBUDGET = FIXBUDGET - QWELL(I)                              !TR: 2012 06 08 WELL
C
C--ADD FLOW OF CAUCHY RB INTO FIXED POTENTIAL                           !TR: 2013 03 14 CAUCHY / CHECK - REMOVE?
          FIXBUDGET = FIXBUDGET - QCYLQ(I)                              !TR: 2013 03 14 CAUCHY / CHECK - REMOVE?
C
C--BARC**ACOUNT FOR TUBE FLOW
          DO J = 1, NNOD
            IF ( NBR(FIX_NODE,J+4+NNOD).NE.Z ) THEN
              FLOW_TUBE = NBR(FIX_NODE, J+4+NNOD)
              IF ( ITUBE(FLOW_TUBE,2).EQ.FIX_NODE ) THEN
                FIXBUDGET = FIXBUDGET + DTUBE(FLOW_TUBE, 1)
              ELSE
                FIXBUDGET = FIXBUDGET - DTUBE(FLOW_TUBE, 1)
              ENDIF
            ENDIF
          ENDDO
C
C--IF NODE IS LH (AND NOT A FIXED HEAD)
          IF (LH_NODE) THEN                                             !TR: 2013 03 25 LH
            IF (FIXBUDGET.GT.Z) THEN                                    !TR: 2013 03 25 LH
              TS_IN_LH = FIXBUDGET                                      !TR: 2013 03 25 LH
              FIXBUDGET = Z                                             !TR: 2013 03 25 LH
            ELSE                                                        !TR: 2013 03 25 LH
              TS_OUT_LH = FIXBUDGET                                     !TR: 2013 03 25 LH
              FIXBUDGET = Z                                             !TR: 2013 03 25 LH
            ENDIF                                                       !TR: 2013 03 25 LH
          ENDIF                                                         !TR: 2013 03 25 LH
C
C--ADD SINK OR SOURCE TO THE BUDGET
C--BARC**RESIDUAL BEING ASSIGNED AS SOURCE OR SINK
C 
          IF ( FIXBUDGET.GT.Z ) THEN
C--THE FIXED HEAD NODE IS A SOURCE
            TS_BUD_SOURCE = TS_BUD_SOURCE + FIXBUDGET
          ELSE
C--THE FIXED HEAD NODE IS A SINK
            TS_BUD_SINK = TS_BUD_SINK + FIXBUDGET
          ENDIF
C
C--BARC**ENDIF FOR BEGHEAD STATEMENT
        ENDIF
C
C--BARC**ENDDO FOR MXNODE DO
      ENDDO
C--TOTAL INPUT THIS TIMESTEP
!     TS_IN = TS_BUD_SOURCE + TS_QBDIR + TS_IN_MAT +                    
      TS_IN = TS_BUD_SOURCE + TS_IN_QBDIR + TS_IN_MAT                   !TR: 2012 04 24 REPLACED TS_QBDIR TO DISTIGUISH RECHARGE IN- AND OUT
     +        + TS_IN_PFPS                                              
!     +        + TS_IN_CADS                                              !TR: 2012 04 26 CADS
     +        + TS_IN_FHLQ                                              !TR: 2012 05 11 FHLQ
     +        + TS_IN_WELL                                              !TR: 2012 06 08 WELL
     +        + TS_IN_CY                                                !TR: 2013 03 14 CAUCHY
     +        + TS_IN_CYLQ                                              !TR: 2013 03 14 CAUCHY LQ
     +        + TS_IN_LH                                                !TR: 2013 03 25 LH
!     +        + TS_IN_QSDIR                                             !TR: 2014 02 20 CADS RECHARGE
     +        + TS_IN_CDSCON                                            !TR: 2014 04 16 CDS RCH
C 
C--TOTAL OUTPUT THIS TIMESTEP
      TS_OUT = DABS(TS_BUD_SINK) + DABS(TS_OUT_MAT)                     
     +         + DABS(TS_OUT_PFPS) + DABS(TS_OUT_QBDIR)                 !TR: 2012 04 24 ADDED TS_OUT_QBDIR TO DISTIGUISH RECHARGE IN- AND OUT
!     +         + DABS(TS_OUT_CADS)                                      !TR: 2012 04 26 CADS
     +         + DABS(TS_OUT_FHLQ)                                      !TR: 2012 05 11 FHLQ
     +         + DABS(TS_OUT_WELL)                                      !TR: 2012 06 08 WELL
     +         + DABS(TS_OUT_CY)                                        !TR: 2013 03 14 CAUCHY
     +         + DABS(TS_OUT_CYLQ)                                      !TR: 2013 03 14 CAUCHY LQ
     +         + DABS(TS_OUT_LH)                                        !TR: 2013 03 25 LH
!     +         + DABS(TS_OUT_QSDIR)                                     !TR: 2014 02 20 CADS RECHARGE
     +         + DABS(TS_OUT_CDSCON)
C
C--CALCULATE CUMMULATIVE BUDGET:                                        !TR: 2012 06 08 BUD_TOTAL_STEP USED?
C     BUD_TOTAL_STEP = 0.D0
      DO I = 1, MXNODE
C
C--CHECK IF NODE IS LH
        IF (LH_FLG) THEN    
          LH_NODE = FALSE
            DO J=1, MXNODE
              IF((ILH(J,1).EQ.NBR(I,1)).AND.(ILH(J,2).EQ.1))
     +          LH_NODE = TRUE
            ENDDO   
        ENDIF       
C
C DIRECT RECHARGE IN THE CONDUIT SYSTEM
!       BUD_QBDIR = BUD_QBDIR + (QBDIR(I)*DELT)                         !TR: 2012 04 24 REPLACED TOTIM BY DELT
!       BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QBDIR(I)*DELT)               !TR: 2012 04 24 REPLACED TOTIM BY DELT
        IF (QBDIR(I).LT.DZ) THEN                                      !TR: 2012 04 24 TO DISTIGUISH RECHARGE IN- AND OUT // HERE OUTFLOW
          BUD_QBDIROUT = BUD_QBDIROUT + (QBDIR(I)*DELT)                 !TR: 2012 04 24 REPLACED TOTIM BY DELT
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QBDIR(I)*DELT)             !TR: 2012 04 24 REPLACED TOTIM BY DELT
        ELSE                                                            !TR: 2012 04 24 TO DISTIGUISH RECHARGE IN- AND OUT // HERE INFLOW
          BUD_QBDIRIN = BUD_QBDIRIN + (QBDIR(I)*DELT)                   !TR: 2012 04 24 REPLACED TOTIM BY DELT
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QBDIR(I)*DELT)             !TR: 2012 04 24 REPLACED TOTIM BY DELT
        ENDIF        
C
C DIRECT RECHARGE IN CADS
        IF (QSDIR(I).LT.DZ) THEN                                      !TR: 2014 02 20 CADS RECHARGE
          BUD_QSDIROUT = BUD_QSDIROUT + (QSDIR(I)*DELT)                 !TR: 2014 02 20 CADS RECHARGE
!          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QSDIR(I)*DELT)             !TR: 2014 02 20 CADS RECHARGE
        ELSE                                                            !TR: 2014 02 20 CADS RECHARGE
          BUD_QSDIRIN = BUD_QSDIRIN + (QSDIR(I)*DELT)                   !TR: 2014 02 20 CADS RECHARGE
!          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QSDIR(I)*DELT)             !TR: 2014 02 20 CADS RECHARGE
        ENDIF          
C
C DISTINGUISH BETWEEN IN- OR OUTFLOW OF THE MATRIX
        IF ( MATFLOW(I).LT.DZ ) THEN
          BUD_MATIN = BUD_MATIN + (-1.D0*MATFLOW(I)*DELT)               !TR: 2012 04 24 REPLACED TOTIM BY DELT
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*MATFLOW(I)*DELT)     !TR: 2012 04 24 REPLACED TOTIM BY DELT
        ELSE
          BUD_MATOUT = BUD_MATOUT + (-1.D0*MATFLOW(I)*DELT)             !TR: 2012 04 24 REPLACED TOTIM BY DELT
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*MATFLOW(I)*DELT)     !TR: 2012 04 24 REPLACED TOTIM BY DELT
        ENDIF
C
C DISTINGUISH BETWEEN IN- OR OUTFLOW OF THE CAD STORAGE                 !TR: 2012 04 26 CADS /
        IF ( CADSFLOW(I).LT.DZ ) THEN                                 !TR: 2012 04 26 CADS /
          BUD_CADSIN = BUD_CADSIN + (-1.D0*CADSFLOW(I)*DELT)            !TR: 2012 04 24 REPLACED TOTIM BY DELT
!          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*CADSFLOW(I)*DELT)    !TR: 2012 04 24 REPLACED TOTIM BY DELT
        ELSE
          BUD_CADSOUT = BUD_CADSOUT + (-1.D0*CADSFLOW(I)*DELT)          !TR: 2012 04 24 REPLACED TOTIM BY DELT
!          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*CADSFLOW(I)*DELT)    !TR: 2012 04 24 REPLACED TOTIM BY DELT
        ENDIF    
C
C DISTINGUISH BETWEEN IN- OR OUTFLOW OF THE CAD STORAGE                 !TR: 2014 04 16 CDS RCH
        IF ( (CADSFLOW(I)-QSDIR(I)).LT.DZ ) THEN                      !TR: 2014 04 16 CDS RCH
          BUD_CDSCONIN = BUD_CDSCONIN
     +                   + (-1.D0*(CADSFLOW(I)-QSDIR(I))*DELT)          !TR: 2014 04 16 CDS RCH
          BUD_TOTAL_STEP = BUD_TOTAL_STEP                               !TR: 2014 04 16 CDS RCH
     +                     + (-1.D0*(CADSFLOW(I)-QSDIR(I))*DELT)        !TR: 2014 04 16 CDS RCH
        ELSE                                                            !TR: 2014 04 16 CDS RCH
          BUD_CDSCONOUT = BUD_CDSCONOUT
     +                    +(-1.D0*(CADSFLOW(I)-QSDIR(I))*DELT)          !TR: 2014 04 16 CDS RCH
          BUD_TOTAL_STEP = BUD_TOTAL_STEP                               !TR: 2014 04 16 CDS RCH
     +                     + (-1.D0*(CADSFLOW(I)-QSDIR(I))*DELT)        !TR: 2014 04 16 CDS RCH
        ENDIF            
C
C--BARC*STORAGE CHANGES HERE**
C--BARC***CORRECT FOR STORAGE CHANGES IN PARTIALLY-FILLED CONDUITS
        IF ( PFPSFLOW(I).LT.DZ ) THEN                                 !TR: 2012 06 08 REPLACED BUD_STORAGE BY QSTOR / PFPSFLOW
          BUD_STORAGE_IN = BUD_STORAGE_IN + (-1.D0*PFPSFLOW(I)*DELT)
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*PFPSFLOW(I)*DELT)
        ELSE
          BUD_STORAGE_OUT = BUD_STORAGE_OUT + (-1.D0*PFPSFLOW(I)*DELT)
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*PFPSFLOW(I)*DELT)
        ENDIF        
C
C DISTINGUISH BETWEEN IN- OR OUTFLOW OF THE CAUCHY BC
        IF ( CYFLOW(I).LT.DZ ) THEN
          BUD_CYIN = BUD_CYIN + (-1.D0*CYFLOW(I)*DELT)                  !TR: 2013 03 18 CAUCHY
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*CYFLOW(I)*DELT)      !TR: 2013 03 18 CAUCHY
        ELSE
          BUD_CYOUT = BUD_CYOUT + (-1.D0*CYFLOW(I)*DELT)                !TR: 2013 03 18 CAUCHY
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (-1.D0*CYFLOW(I)*DELT)      !TR: 2013 03 18 CAUCHY
        ENDIF        
C
C--INFLOW FROM FHLQ (FOR LQ CASES)                                      !TR: 2012 05 11 HERE IN- AND OUTFLOW IS CONSIDERED / INITIALLY, ONLY INFLOW IS INTENDED
        IF (QFHLQ(I).LT.DZ) THEN                                      !TR: 2012 05 11 / FHLQ; TO DISTIGUISH QFHLQ IN- AND OUT // HERE OUTFLOW
          BUD_FHLQOUT = BUD_FHLQOUT + (QFHLQ(I)*DELT)                   !TR: 2012 05 11 / FHLQ; REPLACED TOTIM BY DELT
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QFHLQ(I)*DELT)             !TR: 2012 05 11 / FHLQ; REPLACED TOTIM BY DELT
        ELSE                                                            !TR: 2012 05 11 / FHLQ; TO DISTIGUISH RECHARGE IN- AND OUT // HERE INFLOW
          BUD_FHLQIN = BUD_FHLQIN + (QFHLQ(I)*DELT)                     !TR: 2012 05 11 / FHLQ; REPLACED TOTIM BY DELT
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QFHLQ(I)*DELT)             !TR: 2012 05 11 / FHLQ; REPLACED TOTIM BY DELT
        ENDIF 
C
C--INFLOW FROM WELL                                                     !TR: 2012 06 08 WELL
        IF (QWELL(I).LT.DZ) THEN                                      !TR: 2012 06 08 WELL
          BUD_WELLOUT = BUD_WELLOUT + (QWELL(I)*DELT)                   !TR: 2012 06 08 WELL
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QWELL(I)*DELT)             !TR: 2012 06 08 WELL
        ELSE                                                            !TR: 2012 06 08 WELL
          BUD_WELLIN = BUD_WELLIN + (QWELL(I)*DELT)                     !TR: 2012 06 08 WELL
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QWELL(I)*DELT)             !TR: 2012 06 08 WELL
        ENDIF      
C
C--INFLOW FROM CAUCHY LQ                                                !TR: 2013 03 14 CAUCHY
        IF (QCYLQ(I).LT.DZ) THEN                                      !TR: 2013 03 14 CAUCHY
          BUD_CYLQOUT = BUD_CYLQOUT + (QCYLQ(I)*DELT)                   !TR: 2013 03 14 CAUCHY
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QCYLQ(I)*DELT)             !TR: 2013 03 14 CAUCHY
        ELSE                                                            !TR: 2013 03 14 CAUCHY
          BUD_CYLQIN = BUD_CYLQIN + (QCYLQ(I)*DELT)                     !TR: 2013 03 14 CAUCHY
          BUD_TOTAL_STEP = BUD_TOTAL_STEP + (QCYLQ(I)*DELT)             !TR: 2013 03 14 CAUCHY
        ENDIF                   
C
C--BUDGET OF SOURCES AND SINKS
CB----------------------------------------------------
!RSR    IF ( BEGHEAD(I).NE.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(I)+1.D0).GT.NEARZERO_30 ) THEN
C
          FIX_NODE = I
!         QMIXBUD - 0.D0
C--FLOW INTO THE FIXPOTENTIAL FROM FISSURED SYSTEM
          FIXBUDGET = (1)*MATFLOW(I)*DELT                               !TR: 2012 04 24 REPLACED TOTIM BY DELT
C          
C--DIRECT RECHARGE CONDUIT
          FIXBUDGET = FIXBUDGET - QBDIR(I)*DELT                         !TR: 2012 04 24 REPLACED TOTIM BY DELT
C          
C--DIRECT RECHARGE CADS
          FIXBUDGET = FIXBUDGET - QSDIR(I)*DELT                         !TR: 2014 02 20 CADS RECHARGE
C
C--FLOW INTO THE FIXPOTENTIAL FROM CADS
          FIXBUDGET = FIXBUDGET + CADSFLOW(I)*DELT                      !TR: 2012 05 11 CADS
C
C--FHLQ                                                                 !TR: 2012 05 11 
          FIXBUDGET = FIXBUDGET - QFHLQ(I)*DELT                         !TR: 2012 05 11 FHLQ / 2012 04 24 REPLACED TOTIM BY DELT
C
C--WELL                                                                 !TR: 2012 06 08 
          FIXBUDGET = FIXBUDGET - QWELL(I)*DELT                         !TR: 2012 06 08 WELL / 2012 04 24 REPLACED TOTIM BY DELT
C
C--CAUCHY                                                               !TR: 2013 03 14 
          FIXBUDGET = FIXBUDGET + CYFLOW(I)*DELT                        !TR: 2013 03 14 CAUCHY / CHECK - REMOVE?
C
C--CAUCHY LQ                                                            !TR: 2013 03 14 
          FIXBUDGET = FIXBUDGET - QCYLQ(I)*DELT                         !TR: 2013 03 14 CAUCHY / CHECK - REMOVE?
C
C--BARC***ADD STORAGE CHANGES FROM PARTIALLY FILLED PIPES (PFPS)
          FIXBUDGET = FIXBUDGET + (PFPSFLOW(I)*DELT)                    !TR: 2012 06 08 CHECK THIS  // TODO
C          
C--FLOW IN CONDUITS
          DO J = 1, NNOD
            IF ( NBR(FIX_NODE,J+4+NNOD).NE.Z ) THEN
              FLOW_TUBE = NBR(FIX_NODE, J+4+NNOD)
              IF ( ITUBE(FLOW_TUBE,2).EQ.FIX_NODE ) THEN
                FIXBUDGET = FIXBUDGET + (DTUBE(FLOW_TUBE,1)*DELT)       !TR: 2012 04 24 REPLACED TOTIM BY DELT
              ELSE
                FIXBUDGET = FIXBUDGET - (DTUBE(FLOW_TUBE,1)*DELT)       !TR: 2012 04 24 REPLACED TOTIM BY DELT
              ENDIF
            ENDIF
          ENDDO
C
C--IF NODE IS LH (NOT FIXED HEAD)
          IF (LH_NODE) THEN
            IF (FIXBUDGET.GT.Z) THEN
              BUD_LHIN = BUD_LHIN + FIXBUDGET
              BUD_TOTAL_STEP = BUD_TOTAL_STEP + FIXBUDGET
              FIXBUDGET = 0.0
            ELSE
              BUD_LHOUT = BUD_LHOUT + FIXBUDGET
              BUD_TOTAL_STEP = BUD_TOTAL_STEP + FIXBUDGET
              FIXBUDGET = 0.0            
            ENDIF
          ENDIF          
C
C--ADD SINK OR SOURCE TO THE BUDGET
          IF ( FIXBUDGET.GT.Z ) THEN
C
C--THE FIXED HEAD NODE IS A SOURCE
            BUD_SOURCE = BUD_SOURCE + FIXBUDGET
            BUD_TOTAL_STEP = BUD_TOTAL_STEP + FIXBUDGET
          ELSE
C
C--THE FIXED HEAD NODE IS A SINK
            BUD_SINK = BUD_SINK + FIXBUDGET
            BUD_TOTAL_STEP = BUD_TOTAL_STEP + FIXBUDGET
          ENDIF
C
C--BARC**ENDIF FOR BEGHEAD STATEMENT
        ENDIF
C
      ENDDO
C
C--ADD BUDGET DIFFERENCE OF THE TIMESTEP TO TOTAL BUDGET DIFFERENCE
      TOTAL_BUDGET = TOTAL_BUDGET + DABS(BUD_TOTAL_STEP)
C
C--CALCULATE ACCURRACY OF BUDGET
!      DIFF_TOT = BUD_SOURCE + BUD_QBDIR + BUD_MATIN +                   
!     +           DABS(BUD_STORAGE_IN) - DABS(BUD_SINK)                  
!     +           - DABS(BUD_MATOUT) - BUD_STORAGE_OUT
!      DIFF_TS = TS_BUD_SOURCE + TS_QBDIR + TS_IN_MAT +                  
!     +          DABS(TS_BUD_STORAGE_IN) - DABS(TS_BUD_SINK)             
!     +          - DABS(TS_OUT_MAT) - TS_BUD_STORAGE_OUT
!      TOT_FLOW = BUD_SOURCE + BUD_QBDIR + BUD_MATIN + BUD_STORAGE_IN +  
!     +           DABS(BUD_SINK) + DABS(BUD_MATOUT)                      
!     +           + DABS(BUD_STORAGE_OUT)
!      TS_TOT_FLOW = TS_BUD_SOURCE + TS_QBDIR + TS_IN_MAT +              
!     +              DABS(TS_BUD_STORAGE_IN) + DABS(TS_BUD_SINK)         
!     +              + DABS(TS_OUT_MAT) + TS_BUD_STORAGE_OUT
!TR: 2012 04 24 MODIFIED TO CONSIDER RECHARGE IN- AND OUT               !TR: 2012 04 24 SUBSEQUENT STATEMENT MODIFIED
      DIFF_TOT = BUD_SOURCE + BUD_QBDIRIN + BUD_MATIN                   
     +           + BUD_STORAGE_IN - DABS(BUD_SINK) - DABS(BUD_MATOUT)   
     +           - DABS(BUD_STORAGE_OUT) - DABS(BUD_QBDIROUT)           
!     +           + BUD_CADSIN - DABS(BUD_CADSOUT)                       !TR: 2012 04 26 CADS
     +           + BUD_CDSCONIN - DABS(BUD_CDSCONOUT)                   !TR: 2014 04 16 CADS
     +           + BUD_FHLQIN - DABS(BUD_FHLQOUT)                       !TR: 2012 05 11 FHLQ
     +           + BUD_WELLIN - DABS(BUD_WELLOUT)                       !TR: 2012 06 08 WELL
     +           + BUD_CYIN   - DABS(BUD_CYOUT)                         !TR: 2013 03 14 CAUCHY
     +           + BUD_CYLQIN - DABS(BUD_CYLQOUT)                       !TR: 2013 03 14 CAUCHY LQ
     +           + BUD_LHIN   - DABS(BUD_LHOUT)                         !TR: 2013 03 25 LH
!     +           + BUD_QSDIRIN- DABS(BUD_QSDIROUT)                      !TR: 2014 02 20 CADS RECHARGE
      DIFF_TS = TS_BUD_SOURCE + TS_IN_QBDIR + TS_IN_MAT +               
     +          TS_IN_PFPS - DABS(TS_BUD_SINK)                          
     +          - DABS(TS_OUT_MAT) - DABS(TS_OUT_PFPS)                  
     +          - DABS(TS_OUT_QBDIR)                                    
!     +          + TS_IN_CADS - DABS(TS_OUT_CADS)                        !TR: 2012 04 26 CADS
     +          + TS_IN_CDSCON - DABS(TS_OUT_CDSCON)                    !TR: 2012 04 26 CADS
     +          + TS_IN_FHLQ - DABS(TS_OUT_FHLQ)                        !TR: 2012 05 11 FHLQ
     +          + TS_IN_WELL - DABS(TS_OUT_WELL)                        !TR: 2012 06 08 WELL
     +          + TS_IN_CY   - DABS(TS_OUT_CY)                          !TR: 2013 03 14 CAUCHY
     +          + TS_IN_CYLQ - DABS(TS_OUT_CYLQ)                        !TR: 2013 03 14 CAUCHY LQ
     +          + TS_IN_LH   - DABS(TS_OUT_LH)                          !TR: 2013 03 25 LH
!     +          + TS_IN_QSDIR- DABS(TS_OUT_QSDIR)                       !TR: 2014 02 20 CADS RECHARGE
      TOT_FLOW = BUD_SOURCE + BUD_QBDIRIN + BUD_MATIN + BUD_STORAGE_IN  
     +           + DABS(BUD_SINK) + DABS(BUD_MATOUT)                    
     +           + DABS(BUD_STORAGE_OUT) + DABS(BUD_QBDIROUT)           
!     +           + BUD_CADSIN + DABS(BUD_CADSOUT)                       !TR: 2012 04 26 CADS
     +           + BUD_CDSCONIN + DABS(BUD_CDSCONOUT)                   !TR: 2014 04 16 CADS
     +           + BUD_FHLQIN + DABS(BUD_FHLQOUT)                       !TR: 2012 05 11 FHLQ
     +           + BUD_WELLIN + DABS(BUD_WELLOUT)                       !TR: 2012 06 08 WELL
     +           + BUD_CYIN   + DABS(BUD_CYOUT)                         !TR: 2013 03 14 CAUCHY
     +           + BUD_CYLQIN + DABS(BUD_CYLQOUT)                       !TR: 2013 03 14 CAUCHY LQ
     +           + BUD_LHIN   + DABS(BUD_LHOUT)                         !TR: 2013 03 25 LH
!     +           + BUD_QSDIRIN+ DABS(BUD_QSDIROUT)                      !TR: 2014 02 20 CADS RECHARGE
     
      TS_TOT_FLOW = TS_BUD_SOURCE + TS_IN_QBDIR + TS_IN_MAT +           
     +              TS_IN_PFPS + DABS(TS_BUD_SINK)                      
     +              + DABS(TS_OUT_MAT) + DABS(TS_OUT_PFPS)              
     +              + DABS(TS_OUT_QBDIR)                                
!     +              + TS_IN_CADS + DABS(TS_OUT_CADS)                    !TR: 2012 04 26 CADS
     +              + TS_IN_CDSCON + DABS(TS_OUT_CDSCON)                !TR: 2014 04 16 CADS
     +              + TS_IN_FHLQ + DABS(TS_OUT_FHLQ)                    !TR: 2012 05 11 FHLQ
     +              + TS_IN_WELL + DABS(TS_OUT_WELL)                    !TR: 2012 06 08 WELL
     +              + TS_IN_CY   + DABS(TS_OUT_CY)                      !TR: 2013 03 14 CAUCHY
     +              + TS_IN_CYLQ + DABS(TS_OUT_CYLQ)                    !TR: 2013 03 14 CAUCHY LQ
     +              + TS_IN_LH   + DABS(TS_OUT_LH)                      !TR: 2013 03 25 LH
!     +              + TS_IN_QSDIR+ DABS(TS_OUT_QSDIR)                   !TR: 2014 02 20 CADS RECHARGE
C
C--KUMULATIVE TERMS
      KU_IN = BUD_SOURCE + BUD_QBDIRIN + BUD_MATIN + BUD_STORAGE_IN     
!     +      + BUD_CADSIN + BUD_FHLQIN + BUD_WELLIN + BUD_CYIN           !TR: 2013 03 18 CAUCHY
!     +      + BUD_CYLQIN + BUD_LHIN + BUD_QSDIRIN                       !TR: 2013 03 18 CAUCHY / 2013 03 25 LH // 2014 02 20 CADS RECHARGE
     +      + BUD_CDSCONIN + BUD_FHLQIN + BUD_WELLIN + BUD_CYIN         !TR: 2013 03 18 CAUCHY // 2014 04 16 CADS
     +      + BUD_CYLQIN + BUD_LHIN                                     !TR: 2013 03 18 CAUCHY / 2013 03 25 LH // 
      KU_OUT = DABS(BUD_SINK)+ DABS(BUD_MATOUT)+ DABS(BUD_STORAGE_OUT)  
!     +       + DABS(BUD_QBDIROUT)+ DABS(BUD_CADSOUT) + DABS(BUD_WELLOUT)
     +     + DABS(BUD_QBDIROUT)+ DABS(BUD_CDSCONOUT) + DABS(BUD_WELLOUT)     
     +       + DABS(BUD_FHLQOUT) + DABS(BUD_CYOUT) + DABS(BUD_CYLQOUT)  !TR: 2013 03 18 CAUCHY
!     +       + DABS(BUD_LHOUT) + DABS(BUD_QSDIROUT)                     !TR: 2013 03 25 LH // 2014 02 20 CADS RECHARGE
     +       + DABS(BUD_LHOUT)                                          !TR: 2013 03 25 LH
C
      END SUBROUTINE CONBUD
C
C
C
C
C
      SUBROUTINE NODEBUD
C
C--VERSION 2                   SUBROUTINE NODEBD                      19071999
C******************************************************************************
C                   CALCULATE WATER BUDGET FOR EVERY NODE
C******************************************************************************
C
      USE CONSTANTS, ONLY: TRUE, FALSE, Z, DZ, DOS, NEARZERO_30, TWOPI
      USE CFPMODULE, ONLY:NNOD, MXNODE, NBR, BEGHEAD, QMAT, QTUB, QFIX, 
     +    NODIN, NODOUT, QTUBIN, QTUBOUT, B_MAT, B_MAT_O, NSTOR, ITUBE, 
     +    NODETOP, NODEBOT, GEOHIGHT, CON_DATA, QBDIR, DTUBE,   
     +    ONE8TH,                                   
     +    CADSFLOW, QFHLQ, QWELL, PFPSFLOW, QCYLQ, CYFLOW, QLH, ILH,    !TR: 2012 05 11 CADS / FHLQ / WELL // ADDED PFPSFLOW // 2013 03 14 CAUCHY / 2013 03 25 LH
     +    LH_FLG, QSDIR                                                 !TR: 2014 02 20 CADS RECHARGE
C
CBARC**WHAT ABOUT TUBE      
CBARC**WHAT ABOUT TUBE, RSR TUBE(I,1), TUBE(I,2), TUBE(I,3) IN ITUBE
CBARC**WHAT ABOUT TUBE, RSR TUBE(I,4), TUBE(I,4) IN DTUBE
C
      IMPLICIT NONE
      INTRINSIC DSQRT, SIN
C
C--LOCAL VARIABLES
      INTEGER I, ITEST, J, N, T, BNOD, ENOD
      DOUBLE PRECISION WETTDNEW, WETTDOLD, VNEW, VOLD                   !TR: 2012 07 17 REMOVED VCHANGE
      DOUBLE PRECISION DKNOT, THETA, QT
      LOGICAL LH_NODE
C
C     NODIN  SUM OF INFLOW TO NODE, DIMENSION(MXNODE)
C     NODOUT SUM OF OUTFLOW FROM NODE, DIMENSION(MXNODE)
C     QDIR   FLOW FROM DIRECT RECHARGE INTO NODE, DIMENSION(MXNODE)
C     QFIX   FLOW FROM FIXED HEAD INTO NODE, DIMENSION(MXNODE)
C     QMAT   FLOW FROM NODE INTO FISSURED SYSTEM, DIMENSION(MXNODE)
C     QTUB   FLOW FROM TUBES INTO NODE, DIMENSION(MXNODE,NNOD)
C     QTUBIN SUM OF INFLOW FROM TUBES INTO NODE, DIMENSION(MXNODE)
C     QTUBOUT SUM OF OUTFLOW FROM NODES INTO TUBES, DIMENSION(MXNODE)
C     QWD    DRAINFLOW DUE TO DRY NODES WITH EXCHANGE
C     QWDBUD BUDGET OF DRAININFLOW FOR EACH NODE
C
C--CLEAR QTUB,QFIX,NODIN,NODOUT,QTUBIN,QTUBOUT
      DO I = 1, MXNODE
        DO J = 1, NNOD
          QTUB(I, J) = DZ
        ENDDO
        QFIX(I) = DZ
        NODIN(I) = DZ
        NODOUT(I) = DZ
        QTUBIN(I) = DZ
        QTUBOUT(I) = DZ
      ENDDO
      LH_NODE = FALSE
C
C--LOOP OVER NODES
      DO N = 1, MXNODE
C
C--CHECK IF NODE IS LH
        IF (LH_FLG) THEN    
          LH_NODE = FALSE
            DO J=1, MXNODE
              IF((ILH(J,1).EQ.NBR(N,1)).AND.(ILH(J,2).EQ.1))
     +          LH_NODE = TRUE
            ENDDO   
        ENDIF       
C
C--ADD DIRECT RECHARGE CONDUIT
        IF ( QBDIR(N).GT.DZ ) THEN
          NODIN(N) = NODIN(N) + QBDIR(N)
        ELSE
          NODOUT(N) = NODOUT(N) - QBDIR(N)
        ENDIF
C
C--ADD DIRECT RECHARGE CADS
!        IF ( QSDIR(N).GT.DZ ) THEN                                    !TR: 2014 02 20 CADS RECHARGE
!          NODIN(N) = NODIN(N) + QSDIR(N)                                !TR: 2014 02 20 CADS RECHARGE
!        ELSE                                                            !TR: 2014 02 20 CADS RECHARGE
!          NODOUT(N) = NODOUT(N) - QSDIR(N)                              !TR: 2014 02 20 CADS RECHARGE
!        ENDIF                                                           !TR: 2014 02 20 CADS RECHARGE
C
C--ADD EXCHANGE WITH MATRIX
        IF ( QMAT(N).LT.DZ ) THEN
          NODIN(N) = NODIN(N) - QMAT(N)
        ELSE
          NODOUT(N) = NODOUT(N) + QMAT(N)
        ENDIF
C
C--ADD CADS
        IF ((CADSFLOW(N)-QSDIR(N)).LT.DZ ) THEN                       !TR: 2014 04 16 CADS
          NODIN(N) = NODIN(N) - CADSFLOW(N) + QSDIR(N)                  !TR: 2014 04 16 CADS
        ELSE                                                            !TR: 2014 04 16 CADS
          NODOUT(N) = NODOUT(N) + CADSFLOW(N) - QSDIR(N)                !TR: 2014 04 16 CADS
        ENDIF                                                           !TR: 2014 04 16 CADS
C
C--ADD FHLQ
        IF ( QFHLQ(N).GT.DZ ) THEN                                    !TR: 2012 05 11 / FHLQ
          NODIN(N) = NODIN(N) + QFHLQ(N)                                !TR: 2012 05 11 / FHLQ
        ELSE                                                            !TR: 2012 05 11 / FHLQ
          NODOUT(N) = NODOUT(N) - QFHLQ(N)                              !TR: 2012 05 11 / FHLQ
        ENDIF                                                           !TR: 2012 05 11 / FHLQ
C
C--ADD WELL
        IF ( QWELL(N).GT.DZ ) THEN                                    !TR: 2012 06 08 WELL /
          NODIN(N) = NODIN(N) + QWELL(N)                                !TR: 2012 06 08 WELL /
        ELSE                                                            !TR: 2012 06 08 WELL /
          NODOUT(N) = NODOUT(N) - QWELL(N)                              !TR: 2012 06 08 WELL /
        ENDIF                                                           !TR: 2012 06 08 WELL /
C
C--ADD CAUCHY
        IF ( CYFLOW(N).LT.DZ ) THEN                                   !TR: 2013 03 18 CAUCHY
          NODIN(N) = NODIN(N) - CYFLOW(N)                               !TR: 2013 03 18 CAUCHY
        ELSE                                                            !TR: 2013 03 18 CAUCHY
          NODOUT(N) = NODOUT(N) + CYFLOW(N)                             !TR: 2013 03 18 CAUCHY
        ENDIF                                                           !TR: 2013 03 18 CAUCHY
C
C--ADD CAUCHY LQ
        IF ( QCYLQ(N).GT.DZ ) THEN                                    !TR: 2013 03 14 CAUCHY
          NODIN(N) = NODIN(N) + QCYLQ(N)                                !TR: 2013 03 14 CAUCHY
        ELSE                                                            !TR: 2013 03 14 CAUCHY
          NODOUT(N) = NODOUT(N) - QCYLQ(N)                              !TR: 2013 03 14 CAUCHY
        ENDIF                                                           !TR: 2013 03 14 CAUCHY
C
C--BARC--------ADD STORAGE TERM-----
        IF ( PFPSFLOW(N).LT.Z ) THEN                                    !TR: 2012 07 16 PFPS
          NODIN(N) = NODIN(N) - PFPSFLOW(N)                             !TR: 2012 07 16 PFPS
        ELSE                                                            !TR: 2012 07 16 PFPS
          NODOUT(N) = NODOUT(N) + PFPSFLOW(N)                           !TR: 2012 07 16 PFPS
        ENDIF                                                           !TR: 2012 07 16 PFPS
C
C--SAVE FLOW FROM TUBES IN QTUB AND ADD IT TO NODIN OR NODOUT RESP.
C--LOOP OVER TUBES WHICH ARE CONNECTED WITH THE NODE
        DO T = 1, NNOD
          IF ( NBR(N,T+4+NNOD).NE.Z ) THEN
            QT = DTUBE(NBR(N,T+4+NNOD), 1)
            BNOD = ITUBE(NBR(N,T+4+NNOD), 2)
            ENOD = ITUBE(NBR(N,T+4+NNOD), 3)
            IF ( QT.LT.DZ .AND. N.EQ.BNOD ) THEN
              QTUB(N, T) = -QT
              NODIN(N) = NODIN(N) + QTUB(N, T)
              QTUBIN(N) = QTUBIN(N) + QTUB(N, T)
            ELSEIF ( QT.GT.DZ .AND. N.EQ.ENOD ) THEN
              QTUB(N, T) = QT
              NODIN(N) = NODIN(N) + QTUB(N, T)
              QTUBIN(N) = QTUBIN(N) + QTUB(N, T)
            ELSEIF ( QT.LT.DZ .AND. N.EQ.ENOD ) THEN
              QTUB(N, T) = QT
              NODOUT(N) = NODOUT(N) - QTUB(N, T)
              QTUBOUT(N) = QTUBOUT(N) + QTUB(N, T)
            ELSEIF ( QT.GT.DZ .AND. N.EQ.BNOD ) THEN
              QTUB(N, T) = -QT
              NODOUT(N) = NODOUT(N) - QTUB(N, T)
              QTUBOUT(N) = QTUBOUT(N) + QTUB(N, T)
            ENDIF
          ENDIF
        ENDDO
C
C--FIXED HEAD NODE: DIFFERENCE BETWEEN INFLOW AND OUTFLOW IS SINK OR SOURCE
C--BARC**CHANGE SO THIS WORKS WITH FIXED HEAD  BELOW SEALEVEL
CB          IF(BEGHEAD(N).GE.0)THEN
!RSR    IF ( BEGHEAD(N).NE.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(N)+1.D0).GT.NEARZERO_30 ) THEN
          QFIX(N) = NODOUT(N) - NODIN(N)
C
C--IF NODE IS LH, ACCOUNT FOR LH FLOW
          IF (LH_NODE) THEN
            QLH(N)= QFIX(N)
            QFIX(N) = DZ
            IF (QLH(N).GT.DZ)NODIN(N) = NODIN(N) + QLH(N)
            IF (QLH(N).LT.DZ)NODOUT(N) = NODOUT(N) - QLH(N)
          ENDIF
C
C--FIXED HEAD IS A SOURCE
          IF ( QFIX(N).GT.DZ ) NODIN(N) = NODIN(N) + QFIX(N)
C
C--FIXED HEAD IS A SINK
          IF ( QFIX(N).LT.DZ ) NODOUT(N) = NODOUT(N) - QFIX(N)
C 
        ENDIF
      ENDDO
C
      END SUBROUTINE NODEBUD
C
C
C
C
C
      SUBROUTINE QFIX_INTERMEDIATE
C     *************************************************************************
C     CALCULATE INTERMEDIATE VALUE FOR QFIX FOR FHLQ BC
C     *************************************************************************
C
      USE CONSTANTS, ONLY: Z, DZ, DOS, NEARZERO_30, TWOPI
      USE CFPMODULE, ONLY:NNOD, MXNODE, NBR, BEGHEAD, QMAT, QTUB, QFIX, 
     +    NODIN, NODOUT, QTUBIN, QTUBOUT, B_MAT, B_MAT_O, NSTOR, ITUBE, 
     +    NODETOP, NODEBOT, GEOHIGHT, CON_DATA, QBDIR, DTUBE,   
     +    ONE8TH, B, MOD2, CADSFLOW, QFHLQ,                    
     +    PFPSFLOW, QWELL, QCYLQ, CYFLOW, CADSML2, CADSML_FLG, QSDIR    !TR: 2013 03 14 CAUCHY // 2013 07 05 CADSML // 2014 02 20 CADS RECHARGE
      USE GLOBAL, ONLY:HNEW, IBOUND     
C
      IMPLICIT NONE
      INTRINSIC DSQRT, SIN
C
C--LOCAL VARIABLES
      INTEGER I, ITEST, J, N, T, BNOD, ENOD, IC, IR, IL
      DOUBLE PRECISION WETTDNEW, WETTDOLD, VNEW, VOLD                   !TR: 2012 07 17 REMOVED VCHANGE
      DOUBLE PRECISION DKNOT, THETA, QT, HHNEW, HHCON, RATE
C
C     NODIN  SUM OF INFLOW TO NODE, DIMENSION(MXNODE)
C     NODOUT SUM OF OUTFLOW FROM NODE, DIMENSION(MXNODE)
C     QBDIR  FLOW FROM DIRECT RECHARGE INTO NODE (CONDUIT), DIMENSION(MXNODE)
C     QSDIR  FLOW FROM DIRECT RECHARGE INTO NODE (CADS), DIMENSION(MXNODE)
C     QFIX   FLOW FROM FIXED HEAD INTO NODE, DIMENSION(MXNODE)
C     QMAT   FLOW FROM NODE INTO FISSURED SYSTEM, DIMENSION(MXNODE)
C     QTUB   FLOW FROM TUBES INTO NODE, DIMENSION(MXNODE,NNOD)
C     QTUBIN SUM OF INFLOW FROM TUBES INTO NODE, DIMENSION(MXNODE)
C     QTUBOUT SUM OF OUTFLOW FROM NODES INTO TUBES, DIMENSION(MXNODE)
C     QWD    DRAINFLOW DUE TO DRY NODES WITH EXCHANGE
C     QWDBUD BUDGET OF DRAININFLOW FOR EACH NODE
C
C--CLEAR QTUB,QFIX,NODIN,NODOUT,QTUBIN,QTUBOUT
      DO I = 1, MXNODE
C
C---GET LAYER, ROW, COLUMN OF CELL CONTAINING CONDUIT NODE
        IC = NBR(I, 2)
        IR = NBR(I, 3)
        IL = NBR(I, 4)
C        
C---IF CELL IS NO FLOW MOVE TO NEXT NODE
        IF ( IBOUND(IC,IR,IL).NE.Z ) THEN
C
C---GET HEADS OF FISSURED AND CONDUIT SYSTEM
          HHNEW = HNEW(IC, IR, IL)
C
C--BARC**FOR PERCHED CASES**
          IF ( HHNEW.LT.NODEBOT(I) ) HHNEW = NODEBOT(I)
C
C--BARC**FOR DRY CONDUIT CASES**
          HHCON = B(I)
          IF ( HHCON.LT.NODEBOT(I) ) HHCON = NODEBOT(I)
          RATE = DZ
C
C---CALCULATE EXCHANGE RATE
C--BARC**NEED SOMETHING IN HERE FOR PERCHED CASES, AND OTHERS ?
          RATE = MOD2(I)*(HHCON-HHNEW) 
C
C--BARC***ADD QMAT FOR BUDGET OUTPUT
          QMAT(I) = RATE
        ENDIF
C
C--COMPUTE CADSML FLOW HERE        
        IF(CADSML_FLG.EQ.1)CADSFLOW(I) = CADSML2(I)*(B_MAT(I)-          !TR: 2013 07 05 ADD CADSML HERE
     +                                     B_MAT_O(I))                  !TR: 2013 06 28 CADSML / CADSFLOW COMPUTED HERE; SUBSEQUENTLY, B_MAT IS SAVED AS B_MAT_O        
C              
        DO J = 1, NNOD
          QTUB(I, J) = DZ
        ENDDO
        QFIX(I) = DZ
        NODIN(I) = DZ
        NODOUT(I) = DZ
        QTUBIN(I) = DZ
        QTUBOUT(I) = DZ
      ENDDO
C
C--LOOP OVER NODES
      DO N = 1, MXNODE
C
C--ADD DIRECT RECHARGE CONDUIT
        IF ( QBDIR(N).GT.DZ ) THEN
          NODIN(N) = NODIN(N) + QBDIR(N)
        ELSE
          NODOUT(N) = NODOUT(N) - QBDIR(N)
        ENDIF
!C
!C--ADD DIRECT RECHARGE CADS
!        IF ( QSDIR(N).GT.DZ ) THEN                                    !TR: 2014 02 20 CADS RECHARGE
!          NODIN(N) = NODIN(N) + QSDIR(N)                                !TR: 2014 02 20 CADS RECHARGE
!        ELSE                                                            !TR: 2014 02 20 CADS RECHARGE
!          NODOUT(N) = NODOUT(N) - QSDIR(N)                              !TR: 2014 02 20 CADS RECHARGE
!        ENDIF                                                           !TR: 2014 02 20 CADS RECHARGE
C
C--ADD EXCHANGE WITH MATRIX
        IF ( QMAT(N).LT.DZ ) THEN
          NODIN(N) = NODIN(N) - QMAT(N)
        ELSE
          NODOUT(N) = NODOUT(N) + QMAT(N)
        ENDIF
!C
!C--ADD CADS
!        IF ( CADSFLOW(N).LT.DZ ) THEN                                 !TR: 2012 05 11 CADS
!          NODIN(N) = NODIN(N) - CADSFLOW(N)                             !TR: 2012 05 11 CADS
!        ELSE                                                            !TR: 2012 05 11 CADS
!          NODOUT(N) = NODOUT(N) + CADSFLOW(N)                           !TR: 2012 05 11 CADS
!        ENDIF                                                           !TR: 2012 05 11 CADS
C
C--ADD CADS
        IF ((CADSFLOW(N)-QSDIR(N)).LT.DZ ) THEN                       !TR: 2014 04 16 CADS
          NODIN(N) = NODIN(N) - CADSFLOW(N) + QSDIR(N)                  !TR: 2014 04 16 CADS
        ELSE                                                            !TR: 2014 04 16 CADS
          NODOUT(N) = NODOUT(N) + CADSFLOW(N) - QSDIR(N)                !TR: 2014 04 16 CADS
        ENDIF                                                           !TR: 2014 04 16 CADS
C
C--ADD PFPS
        IF ( PFPSFLOW(N).LT.DZ ) THEN                                 !TR: 2012 07 16 PFPS
          NODIN(N) = NODIN(N) - PFPSFLOW(N)                             !TR: 2012 07 16 PFPS
        ELSE                                                            !TR: 2012 07 16 PFPS
          NODOUT(N) = NODOUT(N) + PFPSFLOW(N)                           !TR: 2012 07 16 PFPS
        ENDIF                                                           !TR: 2012 07 16 PFPS
C
C--ADD FHLQ                                                  
        IF ( QFHLQ(N).GT.DZ ) THEN                                    !TR: 2012 05 11 FHLQ
          NODIN(N) = NODIN(N) + QFHLQ(N)                                !TR: 2012 05 11 FHLQ
        ELSE                                                            !TR: 2012 05 11 FHLQ
          NODOUT(N) = NODOUT(N) - QFHLQ(N)                              !TR: 2012 05 11 FHLQ
        ENDIF                                                           !TR: 2012 05 11 FHLQ
C
C--ADD WELL                                                             !TR: 2012 06 08 WELL /
        IF ( QWELL(N).GT.DZ ) THEN                                    !TR: 2012 06 08 WELL /
          NODIN(N) = NODIN(N) + QWELL(N)                                !TR: 2012 06 08 WELL /
        ELSE                                                            !TR: 2012 06 08 WELL /
          NODOUT(N) = NODOUT(N) - QWELL(N)                              !TR: 2012 06 08 WELL /
        ENDIF                                                           !TR: 2012 06 08 WELL /
C
C--ADD CAUCHY
        IF ( CYFLOW(N).LT.DZ ) THEN                                   !TR: 2013 03 18 CAUCHY
          NODIN(N) = NODIN(N) - CYFLOW(N)                               !TR: 2013 03 18 CAUCHY
        ELSE                                                            !TR: 2013 03 18 CAUCHY
          NODOUT(N) = NODOUT(N) + CYFLOW(N)                             !TR: 2013 03 18 CAUCHY
        ENDIF                                                           !TR: 2013 03 18 CAUCHY
C
C--ADD CAUCHY LQ                                                        !TR: 2013 03 14 CAUCHY
        IF ( QCYLQ(N).GT.DZ ) THEN                                    !TR: 2013 03 14 CAUCHY
          NODIN(N) = NODIN(N) + QCYLQ(N)                                !TR: 2013 03 14 CAUCHY
        ELSE                                                            !TR: 2013 03 14 CAUCHY
          NODOUT(N) = NODOUT(N) - QCYLQ(N)                              !TR: 2013 03 14 CAUCHY
        ENDIF                                                           !TR: 2013 03 14 CAUCHY
C        
C--SAVE FLOW FROM TUBES IN QTUB AND ADD IT TO NODIN OR NODOUT RESP.
C--LOOP OVER TUBES WHICH ARE CONNECTED WITH THE NODE
        DO T = 1, NNOD
          IF ( NBR(N,T+4+NNOD).NE.Z ) THEN
            QT = DTUBE(NBR(N,T+4+NNOD), 1)
            BNOD = ITUBE(NBR(N,T+4+NNOD), 2)
            ENOD = ITUBE(NBR(N,T+4+NNOD), 3)
            IF ( QT.LT.DZ .AND. N.EQ.BNOD ) THEN
              QTUB(N, T) = -QT
              NODIN(N) = NODIN(N) + QTUB(N, T)
              QTUBIN(N) = QTUBIN(N) + QTUB(N, T)
            ELSEIF ( QT.GT.DZ .AND. N.EQ.ENOD ) THEN
              QTUB(N, T) = QT
              NODIN(N) = NODIN(N) + QTUB(N, T)
              QTUBIN(N) = QTUBIN(N) + QTUB(N, T)
            ELSEIF ( QT.LT.DZ .AND. N.EQ.ENOD ) THEN
              QTUB(N, T) = QT
              NODOUT(N) = NODOUT(N) - QTUB(N, T)
              QTUBOUT(N) = QTUBOUT(N) + QTUB(N, T)
            ELSEIF ( QT.GT.DZ .AND. N.EQ.BNOD ) THEN
              QTUB(N, T) = -QT
              NODOUT(N) = NODOUT(N) - QTUB(N, T)
              QTUBOUT(N) = QTUBOUT(N) + QTUB(N, T)
            ENDIF
          ENDIF
        ENDDO
C
C--FIXED HEAD NODE: DIFFERENCE BETWEEN INFLOW AND OUTFLOW IS SINK OR SOURCE
C
C--BARC**CHANGE SO THIS WORKS WITH FIXED HEAD  BELOW SEALEVEL
CB          IF(BEGHEAD(N).GE.0)THEN
!RSR    IF ( BEGHEAD(N).NE.-1.D0 ) THEN
        IF ( DABS(BEGHEAD(N)+1.D0).GT.NEARZERO_30 ) THEN
          QFIX(N) = NODOUT(N) - NODIN(N)
        ENDIF
      ENDDO
C
      END SUBROUTINE QFIX_INTERMEDIATE