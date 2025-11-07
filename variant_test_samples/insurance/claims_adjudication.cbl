       IDENTIFICATION DIVISION.
       PROGRAM-ID. ClaimsAdjudication.
      *INSURANCE - CLAIMS ADJUDICATION SYSTEM
      *Processes insurance claims and determines coverage

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-FILE ASSIGN TO 'claims.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLAIM-NUMBER
               ALTERNATE RECORD KEY IS CLAIM-POLICY-NUM
               FILE STATUS IS WS-CLAIM-FILE-STATUS.

           SELECT POLICY-FILE ASSIGN TO 'policies.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS POLICY-NUMBER
               FILE STATUS IS WS-POLICY-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIM-FILE.
       01  CLAIM-RECORD.
           05  CLAIM-NUMBER            PIC X(12).
           05  CLAIM-POLICY-NUM        PIC X(15).
           05  CLAIM-DATE-OF-LOSS      PIC 9(8).
           05  CLAIM-DATE-FILED        PIC 9(8).
           05  CLAIM-TYPE              PIC X(20).
           05  CLAIM-AMOUNT            PIC 9(9)V99 COMP-3.
           05  CLAIM-STATUS            PIC X(15).
           05  CLAIM-APPROVED-AMT      PIC 9(9)V99 COMP-3.
           05  CLAIM-DEDUCTIBLE        PIC 9(6)V99 COMP-3.
           05  CLAIM-ADJUSTER-ID       PIC X(10).

       FD  POLICY-FILE.
       01  POLICY-RECORD.
           05  POLICY-NUMBER           PIC X(15).
           05  POLICY-HOLDER-NAME      PIC X(50).
           05  POLICY-TYPE             PIC X(20).
           05  POLICY-COVERAGE-LIMIT   PIC 9(9)V99 COMP-3.
           05  POLICY-DEDUCTIBLE       PIC 9(6)V99 COMP-3.
           05  POLICY-PREMIUM          PIC 9(6)V99 COMP-3.
           05  POLICY-EFFECTIVE-DATE   PIC 9(8).
           05  POLICY-EXPIRY-DATE      PIC 9(8).
           05  POLICY-STATUS           PIC X.
           05  POLICY-YTD-CLAIMS       PIC 9(9)V99 COMP-3.

       WORKING-STORAGE SECTION.

       01  WS-CLAIM-FILE-STATUS        PIC XX.
       01  WS-POLICY-FILE-STATUS       PIC XX.

      *Adjudication decision
       01  WS-ADJUDICATION.
           05  WS-DECISION             PIC X(15).
           05  WS-APPROVED-AMOUNT      PIC 9(9)V99 COMP-3.
           05  WS-REASON               PIC X(60).
           05  WS-COVERAGE-AVAILABLE   PIC 9(9)V99 COMP-3.

      *Processing statistics
       01  WS-STATISTICS.
           05  WS-CLAIMS-PROCESSED     PIC 9(6) VALUE ZERO.
           05  WS-CLAIMS-APPROVED      PIC 9(6) VALUE ZERO.
           05  WS-CLAIMS-DENIED        PIC 9(6) VALUE ZERO.
           05  WS-TOTAL-PAID           PIC 9(11)V99 COMP-3 VALUE ZERO.
           05  WS-TOTAL-RESERVED       PIC 9(11)V99 COMP-3 VALUE ZERO.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-DATE             PIC 9999/99/99.

       01  WS-CLAIM-COUNT              PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ADJUDICATION-SYSTEM.
           PERFORM SETUP-TEST-POLICIES.
           PERFORM SETUP-TEST-CLAIMS.
           PERFORM ADJUDICATE-ALL-CLAIMS.
           PERFORM DISPLAY-ADJUDICATION-SUMMARY.
           PERFORM CLEANUP-ADJUDICATION-SYSTEM.
           STOP RUN.

       INITIALIZE-ADJUDICATION-SYSTEM SECTION.
       INIT-SYSTEM.
           DISPLAY '========================================'.
           DISPLAY 'Claims Adjudication System'.
           DISPLAY 'Insurance Processing Division'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

           OPEN OUTPUT POLICY-FILE.
           OPEN OUTPUT CLAIM-FILE.

           IF WS-POLICY-FILE-STATUS NOT = '00' OR
              WS-CLAIM-FILE-STATUS NOT = '00'
               DISPLAY 'Error initializing system'
               STOP RUN
           END-IF.

           DISPLAY 'System initialized'.
           DISPLAY SPACE.

       SETUP-TEST-POLICIES SECTION.
       SETUP-POLICIES.
           DISPLAY 'Setting up test policies...'.

      *Auto policy 1
           MOVE 'AUTO-2024-001' TO POLICY-NUMBER.
           MOVE 'John Smith' TO POLICY-HOLDER-NAME.
           MOVE 'AUTO-COMPREHENSIVE' TO POLICY-TYPE.
           MOVE 50000.00 TO POLICY-COVERAGE-LIMIT.
           MOVE 500.00 TO POLICY-DEDUCTIBLE.
           MOVE 1200.00 TO POLICY-PREMIUM.
           MOVE 20240101 TO POLICY-EFFECTIVE-DATE.
           MOVE 20241231 TO POLICY-EXPIRY-DATE.
           MOVE 'A' TO POLICY-STATUS.
           MOVE 0.00 TO POLICY-YTD-CLAIMS.
           WRITE POLICY-RECORD.

      *Home policy 1
           MOVE 'HOME-2024-001' TO POLICY-NUMBER.
           MOVE 'Jane Doe' TO POLICY-HOLDER-NAME.
           MOVE 'HOMEOWNER' TO POLICY-TYPE.
           MOVE 300000.00 TO POLICY-COVERAGE-LIMIT.
           MOVE 1000.00 TO POLICY-DEDUCTIBLE.
           MOVE 1800.00 TO POLICY-PREMIUM.
           MOVE 20240101 TO POLICY-EFFECTIVE-DATE.
           MOVE 20241231 TO POLICY-EXPIRY-DATE.
           MOVE 'A' TO POLICY-STATUS.
           MOVE 0.00 TO POLICY-YTD-CLAIMS.
           WRITE POLICY-RECORD.

      *Health policy 1
           MOVE 'HLTH-2024-001' TO POLICY-NUMBER.
           MOVE 'Bob Johnson' TO POLICY-HOLDER-NAME.
           MOVE 'HEALTH-FAMILY' TO POLICY-TYPE.
           MOVE 1000000.00 TO POLICY-COVERAGE-LIMIT.
           MOVE 2500.00 TO POLICY-DEDUCTIBLE.
           MOVE 15000.00 TO POLICY-PREMIUM.
           MOVE 20240101 TO POLICY-EFFECTIVE-DATE.
           MOVE 20241231 TO POLICY-EXPIRY-DATE.
           MOVE 'A' TO POLICY-STATUS.
           MOVE 0.00 TO POLICY-YTD-CLAIMS.
           WRITE POLICY-RECORD.

           DISPLAY 'Test policies created'.
           DISPLAY SPACE.

           CLOSE POLICY-FILE.
           OPEN I-O POLICY-FILE.

       SETUP-TEST-CLAIMS SECTION.
       SETUP-CLAIMS.
           DISPLAY 'Setting up test claims...'.

      *Claim 1 - Auto accident
           ADD 1 TO WS-CLAIM-COUNT.
           STRING 'CLM-' WS-CLAIM-COUNT '-2024'
               DELIMITED BY SIZE INTO CLAIM-NUMBER.
           MOVE 'AUTO-2024-001' TO CLAIM-POLICY-NUM.
           MOVE 20241015 TO CLAIM-DATE-OF-LOSS.
           MOVE 20241017 TO CLAIM-DATE-FILED.
           MOVE 'COLLISION' TO CLAIM-TYPE.
           MOVE 8500.00 TO CLAIM-AMOUNT.
           MOVE 'PENDING' TO CLAIM-STATUS.
           MOVE ZERO TO CLAIM-APPROVED-AMT.
           MOVE ZERO TO CLAIM-DEDUCTIBLE.
           MOVE SPACES TO CLAIM-ADJUSTER-ID.
           WRITE CLAIM-RECORD.

      *Claim 2 - Home fire damage
           ADD 1 TO WS-CLAIM-COUNT.
           STRING 'CLM-' WS-CLAIM-COUNT '-2024'
               DELIMITED BY SIZE INTO CLAIM-NUMBER.
           MOVE 'HOME-2024-001' TO CLAIM-POLICY-NUM.
           MOVE 20241001 TO CLAIM-DATE-OF-LOSS.
           MOVE 20241003 TO CLAIM-DATE-FILED.
           MOVE 'FIRE DAMAGE' TO CLAIM-TYPE.
           MOVE 45000.00 TO CLAIM-AMOUNT.
           MOVE 'PENDING' TO CLAIM-STATUS.
           MOVE ZERO TO CLAIM-APPROVED-AMT.
           MOVE ZERO TO CLAIM-DEDUCTIBLE.
           MOVE SPACES TO CLAIM-ADJUSTER-ID.
           WRITE CLAIM-RECORD.

      *Claim 3 - Health medical
           ADD 1 TO WS-CLAIM-COUNT.
           STRING 'CLM-' WS-CLAIM-COUNT '-2024'
               DELIMITED BY SIZE INTO CLAIM-NUMBER.
           MOVE 'HLTH-2024-001' TO CLAIM-POLICY-NUM.
           MOVE 20241020 TO CLAIM-DATE-OF-LOSS.
           MOVE 20241022 TO CLAIM-DATE-FILED.
           MOVE 'HOSPITAL SURGERY' TO CLAIM-TYPE.
           MOVE 125000.00 TO CLAIM-AMOUNT.
           MOVE 'PENDING' TO CLAIM-STATUS.
           MOVE ZERO TO CLAIM-APPROVED-AMT.
           MOVE ZERO TO CLAIM-DEDUCTIBLE.
           MOVE SPACES TO CLAIM-ADJUSTER-ID.
           WRITE CLAIM-RECORD.

      *Claim 4 - Fraudulent claim
           ADD 1 TO WS-CLAIM-COUNT.
           STRING 'CLM-' WS-CLAIM-COUNT '-2024'
               DELIMITED BY SIZE INTO CLAIM-NUMBER.
           MOVE 'AUTO-2024-999' TO CLAIM-POLICY-NUM.
           MOVE 20241010 TO CLAIM-DATE-OF-LOSS.
           MOVE 20241012 TO CLAIM-DATE-FILED.
           MOVE 'THEFT' TO CLAIM-TYPE.
           MOVE 25000.00 TO CLAIM-AMOUNT.
           MOVE 'PENDING' TO CLAIM-STATUS.
           MOVE ZERO TO CLAIM-APPROVED-AMT.
           MOVE ZERO TO CLAIM-DEDUCTIBLE.
           MOVE SPACES TO CLAIM-ADJUSTER-ID.
           WRITE CLAIM-RECORD.

           DISPLAY 'Test claims created: ' WS-CLAIM-COUNT.
           DISPLAY SPACE.

           CLOSE CLAIM-FILE.
           OPEN I-O CLAIM-FILE.

       ADJUDICATE-ALL-CLAIMS SECTION.
       ADJ-ALL.
           DISPLAY 'Processing claims adjudication...'.
           DISPLAY '--------------------------------------------'.

           MOVE SPACES TO CLAIM-NUMBER.

           START CLAIM-FILE KEY >= CLAIM-NUMBER
               INVALID KEY
                   DISPLAY 'No claims to process'
                   GO TO ADJ-ALL-EXIT
           END-START.

           PERFORM UNTIL WS-CLAIM-FILE-STATUS = '10'
               READ CLAIM-FILE NEXT RECORD
                   AT END
                       CONTINUE
                   NOT AT END
                       PERFORM ADJUDICATE-SINGLE-CLAIM
               END-READ
           END-PERFORM.

       ADJ-ALL-EXIT.
           DISPLAY SPACE.
           EXIT.

       ADJUDICATE-SINGLE-CLAIM SECTION.
       ADJ-SINGLE.
           ADD 1 TO WS-CLAIMS-PROCESSED.

           DISPLAY 'Claim: ' CLAIM-NUMBER.
           DISPLAY '  Policy: ' CLAIM-POLICY-NUM.
           MOVE CLAIM-AMOUNT TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Amount: ' WS-DISPLAY-AMOUNT.
           DISPLAY '  Type: ' CLAIM-TYPE.

           PERFORM VERIFY-POLICY-COVERAGE.

           IF WS-DECISION = 'APPROVED'
               PERFORM CALCULATE-PAYMENT
               PERFORM UPDATE-CLAIM-APPROVED
               PERFORM UPDATE-POLICY-YTD
               ADD 1 TO WS-CLAIMS-APPROVED
               ADD WS-APPROVED-AMOUNT TO WS-TOTAL-PAID
           ELSE
               PERFORM UPDATE-CLAIM-DENIED
               ADD 1 TO WS-CLAIMS-DENIED
           END-IF.

           PERFORM DISPLAY-ADJUDICATION-DECISION.
           DISPLAY SPACE.

       VERIFY-POLICY-COVERAGE SECTION.
       VERIFY-POLICY.
           MOVE CLAIM-POLICY-NUM TO POLICY-NUMBER.

           READ POLICY-FILE KEY IS POLICY-NUMBER
               INVALID KEY
                   MOVE 'DENIED' TO WS-DECISION
                   MOVE 'POLICY NOT FOUND' TO WS-REASON
                   GO TO VERIFY-POLICY-EXIT
           END-READ.

      *Check policy is active
           IF POLICY-STATUS NOT = 'A'
               MOVE 'DENIED' TO WS-DECISION
               MOVE 'POLICY NOT ACTIVE' TO WS-REASON
               GO TO VERIFY-POLICY-EXIT
           END-IF.

      *Check loss date within policy period
           IF CLAIM-DATE-OF-LOSS < POLICY-EFFECTIVE-DATE OR
              CLAIM-DATE-OF-LOSS > POLICY-EXPIRY-DATE
               MOVE 'DENIED' TO WS-DECISION
               MOVE 'LOSS DATE OUTSIDE POLICY PERIOD' TO WS-REASON
               GO TO VERIFY-POLICY-EXIT
           END-IF.

      *Check coverage limit
           COMPUTE WS-COVERAGE-AVAILABLE =
               POLICY-COVERAGE-LIMIT - POLICY-YTD-CLAIMS.

           IF WS-COVERAGE-AVAILABLE <= ZERO
               MOVE 'DENIED' TO WS-DECISION
               MOVE 'COVERAGE LIMIT EXHAUSTED' TO WS-REASON
               GO TO VERIFY-POLICY-EXIT
           END-IF.

      *Check claim filed timely (within 30 days)
           IF CLAIM-DATE-FILED > CLAIM-DATE-OF-LOSS + 30
               MOVE 'DENIED' TO WS-DECISION
               MOVE 'CLAIM FILED LATE' TO WS-REASON
               GO TO VERIFY-POLICY-EXIT
           END-IF.

      *All checks passed
           MOVE 'APPROVED' TO WS-DECISION.
           MOVE 'COVERAGE VERIFIED' TO WS-REASON.

       VERIFY-POLICY-EXIT.
           EXIT.

       CALCULATE-PAYMENT SECTION.
       CALC-PAYMENT.
      *Apply deductible
           IF CLAIM-AMOUNT > POLICY-DEDUCTIBLE
               COMPUTE WS-APPROVED-AMOUNT =
                   CLAIM-AMOUNT - POLICY-DEDUCTIBLE
           ELSE
               MOVE ZERO TO WS-APPROVED-AMOUNT
           END-IF.

      *Check against remaining coverage
           IF WS-APPROVED-AMOUNT > WS-COVERAGE-AVAILABLE
               MOVE WS-COVERAGE-AVAILABLE TO WS-APPROVED-AMOUNT
           END-IF.

      *Store deductible applied
           MOVE POLICY-DEDUCTIBLE TO CLAIM-DEDUCTIBLE.

       UPDATE-CLAIM-APPROVED SECTION.
       UPD-APPROVED.
           MOVE 'APPROVED' TO CLAIM-STATUS.
           MOVE WS-APPROVED-AMOUNT TO CLAIM-APPROVED-AMT.
           MOVE 'ADJ001' TO CLAIM-ADJUSTER-ID.
           REWRITE CLAIM-RECORD.

       UPDATE-CLAIM-DENIED SECTION.
       UPD-DENIED.
           MOVE 'DENIED' TO CLAIM-STATUS.
           MOVE ZERO TO CLAIM-APPROVED-AMT.
           MOVE 'ADJ001' TO CLAIM-ADJUSTER-ID.
           REWRITE CLAIM-RECORD.

       UPDATE-POLICY-YTD SECTION.
       UPD-POLICY.
           MOVE CLAIM-POLICY-NUM TO POLICY-NUMBER.
           READ POLICY-FILE KEY IS POLICY-NUMBER.

           ADD WS-APPROVED-AMOUNT TO POLICY-YTD-CLAIMS.
           REWRITE POLICY-RECORD.

       DISPLAY-ADJUDICATION-DECISION SECTION.
       SHOW-DECISION.
           DISPLAY '  Decision: ' WS-DECISION.
           DISPLAY '  Reason: ' WS-REASON.

           IF WS-DECISION = 'APPROVED'
               MOVE WS-APPROVED-AMOUNT TO WS-DISPLAY-AMOUNT
               DISPLAY '  Approved Amount: ' WS-DISPLAY-AMOUNT
               MOVE CLAIM-DEDUCTIBLE TO WS-DISPLAY-AMOUNT
               DISPLAY '  Deductible: ' WS-DISPLAY-AMOUNT
           END-IF.

       DISPLAY-ADJUDICATION-SUMMARY SECTION.
       SHOW-SUMMARY.
           DISPLAY '========================================'.
           DISPLAY 'Adjudication Summary'.
           DISPLAY '========================================'.
           DISPLAY 'Claims Processed: ' WS-CLAIMS-PROCESSED.
           DISPLAY 'Claims Approved:  ' WS-CLAIMS-APPROVED.
           DISPLAY 'Claims Denied:    ' WS-CLAIMS-DENIED.

           MOVE WS-TOTAL-PAID TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Paid:       ' WS-DISPLAY-AMOUNT.

       CLEANUP-ADJUDICATION-SYSTEM SECTION.
       CLEANUP.
           CLOSE CLAIM-FILE.
           CLOSE POLICY-FILE.
           DISPLAY 'Adjudication complete.'.
