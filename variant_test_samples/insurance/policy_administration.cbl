       IDENTIFICATION DIVISION.
       PROGRAM-ID. PolicyAdministration.
      *INSURANCE - POLICY ADMINISTRATION SYSTEM
      *Manages insurance policy lifecycle and renewals

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICY-MASTER ASSIGN TO 'policymas.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PM-POLICY-NUMBER
               ALTERNATE RECORD KEY IS PM-CUSTOMER-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  POLICY-MASTER.
       01  POLICY-MASTER-RECORD.
           05  PM-POLICY-NUMBER        PIC X(15).
           05  PM-CUSTOMER-ID          PIC 9(10).
           05  PM-CUSTOMER-NAME        PIC X(50).
           05  PM-POLICY-TYPE          PIC X(20).
           05  PM-COVERAGE-AMOUNT      PIC 9(10)V99 COMP-3.
           05  PM-PREMIUM-ANNUAL       PIC 9(7)V99 COMP-3.
           05  PM-PREMIUM-FREQUENCY    PIC X(10).
           05  PM-ISSUE-DATE           PIC 9(8).
           05  PM-EFFECTIVE-DATE       PIC 9(8).
           05  PM-EXPIRATION-DATE      PIC 9(8).
           05  PM-STATUS               PIC X(10).
           05  PM-RENEWAL-DATE         PIC 9(8).
           05  PM-LAPSE-DATE           PIC 9(8).
           05  PM-PAID-TO-DATE         PIC 9(8).
           05  PM-PREMIUM-PAID-YTD     PIC 9(7)V99 COMP-3.
           05  PM-CLAIM-COUNT-YTD      PIC 9(4).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS              PIC XX.

      *Processing control
       01  WS-CONTROL.
           05  WS-PROCESSING-DATE      PIC 9(8) VALUE 20241107.
           05  WS-CURRENT-YEAR         PIC 9(4) VALUE 2024.
           05  WS-RENEWAL-THRESHOLD    PIC 9(3) VALUE 30.

      *Policy actions
       01  WS-POLICY-ACTION.
           05  WS-ACTION-TYPE          PIC X(15).
           05  WS-ACTION-REASON        PIC X(60).
           05  WS-NEW-PREMIUM          PIC 9(7)V99 COMP-3.
           05  WS-PREMIUM-INCREASE     PIC S9(5)V99 COMP-3.
           05  WS-INCREASE-PCT         PIC S9(3)V99 COMP-3.

      *Statistics
       01  WS-STATISTICS.
           05  WS-POLICIES-PROCESSED   PIC 9(6) VALUE ZERO.
           05  WS-POLICIES-RENEWED     PIC 9(6) VALUE ZERO.
           05  WS-POLICIES-LAPSED      PIC 9(6) VALUE ZERO.
           05  WS-POLICIES-CANCELLED   PIC 9(6) VALUE ZERO.
           05  WS-RENEWAL-NOTICES-SENT PIC 9(6) VALUE ZERO.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-DATE             PIC 9999/99/99.
       01  WS-DISPLAY-PERCENT          PIC ZZ9.99.

       01  WS-POLICY-COUNT             PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-POLICY-SYSTEM.
           PERFORM SETUP-TEST-POLICIES.
           PERFORM PROCESS-POLICY-RENEWALS.
           PERFORM PROCESS-POLICY-LAPSES.
           PERFORM CALCULATE-PREMIUM-ADJUSTMENTS.
           PERFORM DISPLAY-POLICY-STATISTICS.
           PERFORM CLEANUP-POLICY-SYSTEM.
           STOP RUN.

       INITIALIZE-POLICY-SYSTEM SECTION.
       INIT-SYSTEM.
           DISPLAY '========================================'.
           DISPLAY 'Policy Administration System'.
           DISPLAY 'Insurance Operations Division'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

           OPEN OUTPUT POLICY-MASTER.

           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening policy file'
               STOP RUN
           END-IF.

           MOVE WS-PROCESSING-DATE TO WS-DISPLAY-DATE.
           DISPLAY 'Processing Date: ' WS-DISPLAY-DATE.
           DISPLAY SPACE.

       SETUP-TEST-POLICIES SECTION.
       SETUP-POLICIES.
           DISPLAY 'Setting up test policies...'.

      *Policy 1 - Active, renewing soon
           ADD 1 TO WS-POLICY-COUNT.
           STRING 'POL-AUTO-' WS-POLICY-COUNT
               DELIMITED BY SIZE INTO PM-POLICY-NUMBER.
           MOVE 1000000001 TO PM-CUSTOMER-ID.
           MOVE 'Alice Thompson' TO PM-CUSTOMER-NAME.
           MOVE 'AUTO-STANDARD' TO PM-POLICY-TYPE.
           MOVE 25000.00 TO PM-COVERAGE-AMOUNT.
           MOVE 1200.00 TO PM-PREMIUM-ANNUAL.
           MOVE 'ANNUAL' TO PM-PREMIUM-FREQUENCY.
           MOVE 20231207 TO PM-ISSUE-DATE.
           MOVE 20240101 TO PM-EFFECTIVE-DATE.
           MOVE 20241231 TO PM-EXPIRATION-DATE.
           MOVE 'ACTIVE' TO PM-STATUS.
           MOVE 20241201 TO PM-RENEWAL-DATE.
           MOVE ZERO TO PM-LAPSE-DATE.
           MOVE 20241201 TO PM-PAID-TO-DATE.
           MOVE 1200.00 TO PM-PREMIUM-PAID-YTD.
           MOVE 1 TO PM-CLAIM-COUNT-YTD.
           WRITE POLICY-MASTER-RECORD.

      *Policy 2 - Active, good record
           ADD 1 TO WS-POLICY-COUNT.
           STRING 'POL-HOME-' WS-POLICY-COUNT
               DELIMITED BY SIZE INTO PM-POLICY-NUMBER.
           MOVE 1000000002 TO PM-CUSTOMER-ID.
           MOVE 'Robert Garcia' TO PM-CUSTOMER-NAME.
           MOVE 'HOMEOWNER' TO PM-POLICY-TYPE.
           MOVE 350000.00 TO PM-COVERAGE-AMOUNT.
           MOVE 1800.00 TO PM-PREMIUM-ANNUAL.
           MOVE 'ANNUAL' TO PM-PREMIUM-FREQUENCY.
           MOVE 20220615 TO PM-ISSUE-DATE.
           MOVE 20240101 TO PM-EFFECTIVE-DATE.
           MOVE 20241231 TO PM-EXPIRATION-DATE.
           MOVE 'ACTIVE' TO PM-STATUS.
           MOVE 20241201 TO PM-RENEWAL-DATE.
           MOVE ZERO TO PM-LAPSE-DATE.
           MOVE 20241201 TO PM-PAID-TO-DATE.
           MOVE 1800.00 TO PM-PREMIUM-PAID-YTD.
           MOVE 0 TO PM-CLAIM-COUNT-YTD.
           WRITE POLICY-MASTER-RECORD.

      *Policy 3 - Payment lapsed
           ADD 1 TO WS-POLICY-COUNT.
           STRING 'POL-AUTO-' WS-POLICY-COUNT
               DELIMITED BY SIZE INTO PM-POLICY-NUMBER.
           MOVE 1000000003 TO PM-CUSTOMER-ID.
           MOVE 'Maria Rodriguez' TO PM-CUSTOMER-NAME.
           MOVE 'AUTO-PREMIUM' TO PM-POLICY-TYPE.
           MOVE 50000.00 TO PM-COVERAGE-AMOUNT.
           MOVE 2400.00 TO PM-PREMIUM-ANNUAL.
           MOVE 'MONTHLY' TO PM-PREMIUM-FREQUENCY.
           MOVE 20230101 TO PM-ISSUE-DATE.
           MOVE 20240101 TO PM-EFFECTIVE-DATE.
           MOVE 20241231 TO PM-EXPIRATION-DATE.
           MOVE 'ACTIVE' TO PM-STATUS.
           MOVE 20241101 TO PM-RENEWAL-DATE.
           MOVE ZERO TO PM-LAPSE-DATE.
           MOVE 20240815 TO PM-PAID-TO-DATE.
           MOVE 1600.00 TO PM-PREMIUM-PAID-YTD.
           MOVE 3 TO PM-CLAIM-COUNT-YTD.
           WRITE POLICY-MASTER-RECORD.

      *Policy 4 - Renewal eligible
           ADD 1 TO WS-POLICY-COUNT.
           STRING 'POL-LIFE-' WS-POLICY-COUNT
               DELIMITED BY SIZE INTO PM-POLICY-NUMBER.
           MOVE 1000000004 TO PM-CUSTOMER-ID.
           MOVE 'James Wilson' TO PM-CUSTOMER-NAME.
           MOVE 'LIFE-TERM20' TO PM-POLICY-TYPE.
           MOVE 500000.00 TO PM-COVERAGE-AMOUNT.
           MOVE 3600.00 TO PM-PREMIUM-ANNUAL.
           MOVE 'ANNUAL' TO PM-PREMIUM-FREQUENCY.
           MOVE 20190101 TO PM-ISSUE-DATE.
           MOVE 20240101 TO PM-EFFECTIVE-DATE.
           MOVE 20241231 TO PM-EXPIRATION-DATE.
           MOVE 'ACTIVE' TO PM-STATUS.
           MOVE 20241115 TO PM-RENEWAL-DATE.
           MOVE ZERO TO PM-LAPSE-DATE.
           MOVE 20241115 TO PM-PAID-TO-DATE.
           MOVE 3600.00 TO PM-PREMIUM-PAID-YTD.
           MOVE 0 TO PM-CLAIM-COUNT-YTD.
           WRITE POLICY-MASTER-RECORD.

           DISPLAY 'Created ' WS-POLICY-COUNT ' test policies'.
           DISPLAY SPACE.

           CLOSE POLICY-MASTER.
           OPEN I-O POLICY-MASTER.

       PROCESS-POLICY-RENEWALS SECTION.
       PROC-RENEWALS.
           DISPLAY 'Processing policy renewals...'.
           DISPLAY '--------------------------------------------'.

           MOVE SPACES TO PM-POLICY-NUMBER.

           START POLICY-MASTER KEY >= PM-POLICY-NUMBER
               INVALID KEY
                   DISPLAY 'No policies to process'
                   GO TO PROC-RENEWALS-EXIT
           END-START.

           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ POLICY-MASTER NEXT RECORD
                   AT END
                       CONTINUE
                   NOT AT END
                       IF PM-STATUS = 'ACTIVE'
                           PERFORM CHECK-RENEWAL-ELIGIBILITY
                       END-IF
               END-READ
           END-PERFORM.

       PROC-RENEWALS-EXIT.
           DISPLAY SPACE.
           EXIT.

       CHECK-RENEWAL-ELIGIBILITY SECTION.
       CHECK-RENEWAL.
           ADD 1 TO WS-POLICIES-PROCESSED.

      *Check if renewal date is within threshold
           IF PM-RENEWAL-DATE <= WS-PROCESSING-DATE + 30 AND
              PM-RENEWAL-DATE >= WS-PROCESSING-DATE
               PERFORM PROCESS-RENEWAL
           END-IF.

       PROCESS-RENEWAL SECTION.
       PROC-RENEWAL.
           DISPLAY 'Policy: ' PM-POLICY-NUMBER.
           DISPLAY '  Customer: ' PM-CUSTOMER-NAME.
           DISPLAY '  Type: ' PM-POLICY-TYPE.

           MOVE PM-PREMIUM-ANNUAL TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Current Premium: ' WS-DISPLAY-AMOUNT.

           PERFORM CALCULATE-RENEWAL-PREMIUM.

           IF WS-NEW-PREMIUM > ZERO
               PERFORM GENERATE-RENEWAL-NOTICE
               ADD 1 TO WS-POLICIES-RENEWED
           END-IF.

           DISPLAY SPACE.

       CALCULATE-RENEWAL-PREMIUM SECTION.
       CALC-RENEWAL.
      *Base premium increase of 3%
           COMPUTE WS-INCREASE-PCT = 3.00.

      *Add surcharge for claims
           IF PM-CLAIM-COUNT-YTD > 0
               COMPUTE WS-INCREASE-PCT =
                   WS-INCREASE-PCT + (PM-CLAIM-COUNT-YTD * 5)
               DISPLAY '  Surcharge applied: '
                       PM-CLAIM-COUNT-YTD ' claims'
           END-IF.

      *Discount for no claims
           IF PM-CLAIM-COUNT-YTD = 0
               COMPUTE WS-INCREASE-PCT = WS-INCREASE-PCT - 5
               DISPLAY '  Discount applied: No claims'
           END-IF.

      *Calculate new premium
           COMPUTE WS-NEW-PREMIUM =
               PM-PREMIUM-ANNUAL *
               (1 + (WS-INCREASE-PCT / 100))
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.

           COMPUTE WS-PREMIUM-INCREASE =
               WS-NEW-PREMIUM - PM-PREMIUM-ANNUAL.

           MOVE WS-NEW-PREMIUM TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Renewal Premium: ' WS-DISPLAY-AMOUNT.
           MOVE WS-INCREASE-PCT TO WS-DISPLAY-PERCENT.
           DISPLAY '  Increase: ' WS-DISPLAY-PERCENT '%'.

       GENERATE-RENEWAL-NOTICE SECTION.
       GEN-NOTICE.
           ADD 1 TO WS-RENEWAL-NOTICES-SENT.
           DISPLAY '  *** RENEWAL NOTICE GENERATED ***'.

      *In real system, would generate letter/email
           MOVE 'RENEW-PENDING' TO PM-STATUS.
           REWRITE POLICY-MASTER-RECORD.

       PROCESS-POLICY-LAPSES SECTION.
       PROC-LAPSES.
           DISPLAY 'Checking for policy lapses...'.
           DISPLAY '--------------------------------------------'.

           MOVE SPACES TO PM-POLICY-NUMBER.

           START POLICY-MASTER KEY >= PM-POLICY-NUMBER.

           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ POLICY-MASTER NEXT RECORD
                   AT END
                       CONTINUE
                   NOT AT END
                       IF PM-STATUS = 'ACTIVE'
                           PERFORM CHECK-PAYMENT-LAPSE
                       END-IF
               END-READ
           END-PERFORM.

           DISPLAY SPACE.

       CHECK-PAYMENT-LAPSE SECTION.
       CHECK-LAPSE.
      *Check if payment is more than 60 days overdue
           IF PM-PAID-TO-DATE < WS-PROCESSING-DATE - 60
               PERFORM LAPSE-POLICY
           END-IF.

       LAPSE-POLICY SECTION.
       DO-LAPSE.
           DISPLAY 'LAPSE: ' PM-POLICY-NUMBER.
           DISPLAY '  Customer: ' PM-CUSTOMER-NAME.
           DISPLAY '  Last payment: ' PM-PAID-TO-DATE.

           MOVE 'LAPSED' TO PM-STATUS.
           MOVE WS-PROCESSING-DATE TO PM-LAPSE-DATE.
           REWRITE POLICY-MASTER-RECORD.

           ADD 1 TO WS-POLICIES-LAPSED.
           DISPLAY '  *** POLICY LAPSED ***'.
           DISPLAY SPACE.

       CALCULATE-PREMIUM-ADJUSTMENTS SECTION.
       CALC-ADJ.
           DISPLAY 'Premium adjustment analysis complete.'.

       DISPLAY-POLICY-STATISTICS SECTION.
       SHOW-STATS.
           DISPLAY '========================================'.
           DISPLAY 'Policy Administration Summary'.
           DISPLAY '========================================'.
           DISPLAY 'Policies Processed:      ' WS-POLICIES-PROCESSED.
           DISPLAY 'Policies Renewed:        ' WS-POLICIES-RENEWED.
           DISPLAY 'Renewal Notices Sent:    ' WS-RENEWAL-NOTICES-SENT.
           DISPLAY 'Policies Lapsed:         ' WS-POLICIES-LAPSED.
           DISPLAY 'Policies Cancelled:      ' WS-POLICIES-CANCELLED.

       CLEANUP-POLICY-SYSTEM SECTION.
       CLEANUP.
           CLOSE POLICY-MASTER.
           DISPLAY 'Policy administration complete.'.
