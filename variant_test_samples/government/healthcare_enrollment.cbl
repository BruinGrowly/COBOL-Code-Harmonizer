       IDENTIFICATION DIVISION.
       PROGRAM-ID. HealthcareEnrollment.
      *GOVERNMENT - HEALTHCARE ENROLLMENT SYSTEM
      *Processes Medicare and Medicaid enrollment applications

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APPLICATION-FILE ASSIGN TO 'applications.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS APP-NUMBER
               ALTERNATE RECORD KEY IS APP-SSN
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  APPLICATION-FILE.
       01  APPLICATION-RECORD.
           05  APP-NUMBER              PIC X(12).
           05  APP-SSN                 PIC 9(9).
           05  APP-APPLICANT-NAME      PIC X(50).
           05  APP-BIRTH-DATE          PIC 9(8).
           05  APP-AGE                 PIC 9(3).
           05  APP-PROGRAM             PIC X(15).
           05  APP-HOUSEHOLD-SIZE      PIC 99.
           05  APP-ANNUAL-INCOME       PIC 9(9)V99.
           05  APP-DISABILITY-FLAG     PIC X.
           05  APP-STATUS              PIC X(20).
           05  APP-ELIGIBILITY-DATE    PIC 9(8).
           05  APP-BENEFIT-TIER        PIC X(10).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS              PIC XX.

      *Eligibility criteria
       01  WS-ELIGIBILITY-RULES.
           05  WS-MEDICARE-AGE         PIC 99 VALUE 65.
           05  WS-FEDERAL-POVERTY-LEVEL PIC 9(6) VALUE 14580.
           05  WS-MEDICAID-INCOME-PCT  PIC 9(3) VALUE 138.
           05  WS-CHIP-INCOME-PCT      PIC 9(3) VALUE 200.

      *Application processing
       01  WS-APPLICATION-DATA.
           05  WS-APPL-COUNT           PIC 9(6) VALUE ZERO.
           05  WS-APPROVED-COUNT       PIC 9(6) VALUE ZERO.
           05  WS-DENIED-COUNT         PIC 9(6) VALUE ZERO.
           05  WS-PENDING-COUNT        PIC 9(6) VALUE ZERO.

      *Eligibility determination
       01  WS-ELIGIBILITY-CHECK.
           05  WS-ELIGIBLE             PIC X VALUE 'N'.
           05  WS-PROGRAM-ASSIGNED     PIC X(15).
           05  WS-REASON               PIC X(60).
           05  WS-INCOME-LIMIT         PIC 9(7)V99.
           05  WS-INCOME-AS-PCT-FPL    PIC 9(3).

      *Date calculations
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-YEAR         PIC 9(4).
           05  WS-CURRENT-MONTH        PIC 99.
           05  WS-CURRENT-DAY          PIC 99.

       01  WS-CALCULATED-AGE           PIC 9(3).

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-PERCENT          PIC ZZ9.

       01  WS-APPLICATION-NUM          PIC 9(4) VALUE 1.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ENROLLMENT-SYSTEM.
           PERFORM SETUP-TEST-APPLICATIONS.
           PERFORM PROCESS-ALL-APPLICATIONS.
           PERFORM DISPLAY-ENROLLMENT-SUMMARY.
           PERFORM CLEANUP-ENROLLMENT-SYSTEM.
           STOP RUN.

       INITIALIZE-ENROLLMENT-SYSTEM SECTION.
       INIT-SYSTEM.
           DISPLAY '========================================'.
           DISPLAY 'Healthcare Enrollment System'.
           DISPLAY 'Department of Health & Human Services'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

           OPEN OUTPUT APPLICATION-FILE.

           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening application file'
               STOP RUN
           END-IF.

      *Get current date
           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE YYYYMMDD.

           DISPLAY 'System initialized successfully'.
           DISPLAY SPACE.

       SETUP-TEST-APPLICATIONS SECTION.
       SETUP-APPS.
           DISPLAY 'Creating test applications...'.

      *Application 1 - Medicare eligible by age
           STRING 'APP-' WS-APPLICATION-NUM
               DELIMITED BY SIZE INTO APP-NUMBER.
           MOVE 123456789 TO APP-SSN.
           MOVE 'Mary Johnson' TO APP-APPLICANT-NAME.
           MOVE 19580215 TO APP-BIRTH-DATE.
           MOVE 66 TO APP-AGE.
           MOVE 'MEDICARE' TO APP-PROGRAM.
           MOVE 1 TO APP-HOUSEHOLD-SIZE.
           MOVE 28000.00 TO APP-ANNUAL-INCOME.
           MOVE 'N' TO APP-DISABILITY-FLAG.
           MOVE 'PENDING' TO APP-STATUS.
           MOVE ZERO TO APP-ELIGIBILITY-DATE.
           MOVE SPACES TO APP-BENEFIT-TIER.
           WRITE APPLICATION-RECORD.
           ADD 1 TO WS-APPLICATION-NUM.

      *Application 2 - Medicaid eligible by income
           STRING 'APP-' WS-APPLICATION-NUM
               DELIMITED BY SIZE INTO APP-NUMBER.
           MOVE 987654321 TO APP-SSN.
           MOVE 'Carlos Rodriguez' TO APP-APPLICANT-NAME.
           MOVE 19850520 TO APP-BIRTH-DATE.
           MOVE 39 TO APP-AGE.
           MOVE 'MEDICAID' TO APP-PROGRAM.
           MOVE 3 TO APP-HOUSEHOLD-SIZE.
           MOVE 18000.00 TO APP-ANNUAL-INCOME.
           MOVE 'N' TO APP-DISABILITY-FLAG.
           MOVE 'PENDING' TO APP-STATUS.
           MOVE ZERO TO APP-ELIGIBILITY-DATE.
           MOVE SPACES TO APP-BENEFIT-TIER.
           WRITE APPLICATION-RECORD.
           ADD 1 TO WS-APPLICATION-NUM.

      *Application 3 - Disability Medicare
           STRING 'APP-' WS-APPLICATION-NUM
               DELIMITED BY SIZE INTO APP-NUMBER.
           MOVE 555667788 TO APP-SSN.
           MOVE 'James Wilson' TO APP-APPLICANT-NAME.
           MOVE 19780310 TO APP-BIRTH-DATE.
           MOVE 46 TO APP-AGE.
           MOVE 'MEDICARE' TO APP-PROGRAM.
           MOVE 2 TO APP-HOUSEHOLD-SIZE.
           MOVE 22000.00 TO APP-ANNUAL-INCOME.
           MOVE 'Y' TO APP-DISABILITY-FLAG.
           MOVE 'PENDING' TO APP-STATUS.
           MOVE ZERO TO APP-ELIGIBILITY-DATE.
           MOVE SPACES TO APP-BENEFIT-TIER.
           WRITE APPLICATION-RECORD.
           ADD 1 TO WS-APPLICATION-NUM.

      *Application 4 - CHIP eligible child
           STRING 'APP-' WS-APPLICATION-NUM
               DELIMITED BY SIZE INTO APP-NUMBER.
           MOVE 111222333 TO APP-SSN.
           MOVE 'Emma Davis (minor)' TO APP-APPLICANT-NAME.
           MOVE 20150815 TO APP-BIRTH-DATE.
           MOVE 9 TO APP-AGE.
           MOVE 'CHIP' TO APP-PROGRAM.
           MOVE 4 TO APP-HOUSEHOLD-SIZE.
           MOVE 45000.00 TO APP-ANNUAL-INCOME.
           MOVE 'N' TO APP-DISABILITY-FLAG.
           MOVE 'PENDING' TO APP-STATUS.
           MOVE ZERO TO APP-ELIGIBILITY-DATE.
           MOVE SPACES TO APP-BENEFIT-TIER.
           WRITE APPLICATION-RECORD.
           ADD 1 TO WS-APPLICATION-NUM.

      *Application 5 - Income too high
           STRING 'APP-' WS-APPLICATION-NUM
               DELIMITED BY SIZE INTO APP-NUMBER.
           MOVE 444555666 TO APP-SSN.
           MOVE 'Patricia Brown' TO APP-APPLICANT-NAME.
           MOVE 19750622 TO APP-BIRTH-DATE.
           MOVE 49 TO APP-AGE.
           MOVE 'MEDICAID' TO APP-PROGRAM.
           MOVE 2 TO APP-HOUSEHOLD-SIZE.
           MOVE 75000.00 TO APP-ANNUAL-INCOME.
           MOVE 'N' TO APP-DISABILITY-FLAG.
           MOVE 'PENDING' TO APP-STATUS.
           MOVE ZERO TO APP-ELIGIBILITY-DATE.
           MOVE SPACES TO APP-BENEFIT-TIER.
           WRITE APPLICATION-RECORD.
           ADD 1 TO WS-APPLICATION-NUM.

           SUBTRACT 1 FROM WS-APPLICATION-NUM.
           DISPLAY 'Created ' WS-APPLICATION-NUM ' applications'.
           DISPLAY SPACE.

           CLOSE APPLICATION-FILE.
           OPEN I-O APPLICATION-FILE.

       PROCESS-ALL-APPLICATIONS SECTION.
       PROC-ALL.
           DISPLAY 'Processing enrollment applications...'.
           DISPLAY '--------------------------------------------'.

           MOVE SPACES TO APP-NUMBER.

           START APPLICATION-FILE KEY >= APP-NUMBER
               INVALID KEY
                   DISPLAY 'No applications to process'
                   GO TO PROC-ALL-EXIT
           END-START.

           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ APPLICATION-FILE NEXT RECORD
                   AT END
                       CONTINUE
                   NOT AT END
                       IF APP-STATUS = 'PENDING'
                           PERFORM PROCESS-SINGLE-APPLICATION
                       END-IF
               END-READ
           END-PERFORM.

       PROC-ALL-EXIT.
           DISPLAY SPACE.
           EXIT.

       PROCESS-SINGLE-APPLICATION SECTION.
       PROC-SINGLE.
           ADD 1 TO WS-APPL-COUNT.

           DISPLAY 'Application: ' APP-NUMBER.
           DISPLAY '  Applicant: ' APP-APPLICANT-NAME.
           DISPLAY '  Program: ' APP-PROGRAM.
           DISPLAY '  Age: ' APP-AGE.
           MOVE APP-ANNUAL-INCOME TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Income: ' WS-DISPLAY-AMOUNT.

           EVALUATE APP-PROGRAM
               WHEN 'MEDICARE'
                   PERFORM CHECK-MEDICARE-ELIGIBILITY
               WHEN 'MEDICAID'
                   PERFORM CHECK-MEDICAID-ELIGIBILITY
               WHEN 'CHIP'
                   PERFORM CHECK-CHIP-ELIGIBILITY
               WHEN OTHER
                   MOVE 'N' TO WS-ELIGIBLE
                   MOVE 'UNKNOWN PROGRAM' TO WS-REASON
           END-EVALUATE.

           IF WS-ELIGIBLE = 'Y'
               PERFORM APPROVE-APPLICATION
               ADD 1 TO WS-APPROVED-COUNT
           ELSE
               PERFORM DENY-APPLICATION
               ADD 1 TO WS-DENIED-COUNT
           END-IF.

           PERFORM DISPLAY-DECISION.
           DISPLAY SPACE.

       CHECK-MEDICARE-ELIGIBILITY SECTION.
       CHECK-MEDICARE.
           MOVE 'N' TO WS-ELIGIBLE.

      *Check age-based eligibility
           IF APP-AGE >= WS-MEDICARE-AGE
               MOVE 'Y' TO WS-ELIGIBLE
               MOVE 'MEDICARE' TO WS-PROGRAM-ASSIGNED
               MOVE 'AGE 65 OR OLDER' TO WS-REASON
               GO TO CHECK-MEDICARE-EXIT
           END-IF.

      *Check disability-based eligibility
           IF APP-DISABILITY-FLAG = 'Y' AND
              APP-AGE >= 18
               MOVE 'Y' TO WS-ELIGIBLE
               MOVE 'MEDICARE' TO WS-PROGRAM-ASSIGNED
               MOVE 'QUALIFIED DISABILITY' TO WS-REASON
               GO TO CHECK-MEDICARE-EXIT
           END-IF.

      *Not eligible
           MOVE 'DOES NOT MEET AGE/DISABILITY CRITERIA' TO WS-REASON.

       CHECK-MEDICARE-EXIT.
           EXIT.

       CHECK-MEDICAID-ELIGIBILITY SECTION.
       CHECK-MEDICAID.
           MOVE 'N' TO WS-ELIGIBLE.

      *Calculate adjusted poverty level for household size
           COMPUTE WS-INCOME-LIMIT =
               WS-FEDERAL-POVERTY-LEVEL +
               ((APP-HOUSEHOLD-SIZE - 1) * 5140) *
               (WS-MEDICAID-INCOME-PCT / 100).

      *Calculate income as percentage of FPL
           COMPUTE WS-INCOME-AS-PCT-FPL =
               (APP-ANNUAL-INCOME /
                (WS-FEDERAL-POVERTY-LEVEL +
                 ((APP-HOUSEHOLD-SIZE - 1) * 5140))) * 100.

      *Check income eligibility
           IF APP-ANNUAL-INCOME <= WS-INCOME-LIMIT
               MOVE 'Y' TO WS-ELIGIBLE
               MOVE 'MEDICAID' TO WS-PROGRAM-ASSIGNED
               STRING
                   'INCOME AT ' WS-INCOME-AS-PCT-FPL
                   '% OF FPL (LIMIT: ' WS-MEDICAID-INCOME-PCT '%)'
                   DELIMITED BY SIZE INTO WS-REASON
           ELSE
               STRING
                   'INCOME TOO HIGH: ' WS-INCOME-AS-PCT-FPL
                   '% OF FPL (LIMIT: ' WS-MEDICAID-INCOME-PCT '%)'
                   DELIMITED BY SIZE INTO WS-REASON
           END-IF.

       CHECK-CHIP-ELIGIBILITY SECTION.
       CHECK-CHIP.
           MOVE 'N' TO WS-ELIGIBLE.

      *CHIP is for children under 19
           IF APP-AGE >= 19
               MOVE 'CHILD OVER AGE 19' TO WS-REASON
               GO TO CHECK-CHIP-EXIT
           END-IF.

      *Calculate income limit for CHIP
           COMPUTE WS-INCOME-LIMIT =
               WS-FEDERAL-POVERTY-LEVEL +
               ((APP-HOUSEHOLD-SIZE - 1) * 5140) *
               (WS-CHIP-INCOME-PCT / 100).

           COMPUTE WS-INCOME-AS-PCT-FPL =
               (APP-ANNUAL-INCOME /
                (WS-FEDERAL-POVERTY-LEVEL +
                 ((APP-HOUSEHOLD-SIZE - 1) * 5140))) * 100.

      *Check income eligibility
           IF APP-ANNUAL-INCOME <= WS-INCOME-LIMIT
               MOVE 'Y' TO WS-ELIGIBLE
               MOVE 'CHIP' TO WS-PROGRAM-ASSIGNED
               STRING
                   'INCOME AT ' WS-INCOME-AS-PCT-FPL
                   '% OF FPL (LIMIT: ' WS-CHIP-INCOME-PCT '%)'
                   DELIMITED BY SIZE INTO WS-REASON
           ELSE
               STRING
                   'INCOME TOO HIGH: ' WS-INCOME-AS-PCT-FPL
                   '% OF FPL (LIMIT: ' WS-CHIP-INCOME-PCT '%)'
                   DELIMITED BY SIZE INTO WS-REASON
           END-IF.

       CHECK-CHIP-EXIT.
           EXIT.

       APPROVE-APPLICATION SECTION.
       APPROVE-APP.
           MOVE 'APPROVED' TO APP-STATUS.
           MOVE WS-PROGRAM-ASSIGNED TO APP-PROGRAM.

      *Set eligibility date (first of next month)
           IF WS-CURRENT-MONTH = 12
               COMPUTE WS-CURRENT-YEAR = WS-CURRENT-YEAR + 1
               MOVE 1 TO WS-CURRENT-MONTH
           ELSE
               ADD 1 TO WS-CURRENT-MONTH
           END-IF.

           STRING
               WS-CURRENT-YEAR
               WS-CURRENT-MONTH
               '01'
               DELIMITED BY SIZE INTO APP-ELIGIBILITY-DATE.

      *Determine benefit tier based on income
           IF WS-INCOME-AS-PCT-FPL < 100
               MOVE 'FULL' TO APP-BENEFIT-TIER
           ELSE
               IF WS-INCOME-AS-PCT-FPL < 150
                   MOVE 'ENHANCED' TO APP-BENEFIT-TIER
               ELSE
                   MOVE 'STANDARD' TO APP-BENEFIT-TIER
               END-IF
           END-IF.

           REWRITE APPLICATION-RECORD.

       DENY-APPLICATION SECTION.
       DENY-APP.
           MOVE 'DENIED' TO APP-STATUS.
           REWRITE APPLICATION-RECORD.

       DISPLAY-DECISION SECTION.
       SHOW-DECISION.
           DISPLAY '  Decision: ' APP-STATUS.
           DISPLAY '  Reason: ' WS-REASON.

           IF APP-STATUS = 'APPROVED'
               DISPLAY '  Program: ' APP-PROGRAM
               DISPLAY '  Benefit Tier: ' APP-BENEFIT-TIER
               DISPLAY '  Effective Date: ' APP-ELIGIBILITY-DATE
           END-IF.

       DISPLAY-ENROLLMENT-SUMMARY SECTION.
       SHOW-SUMMARY.
           DISPLAY '========================================'.
           DISPLAY 'Enrollment Summary'.
           DISPLAY '========================================'.
           DISPLAY 'Applications Processed: ' WS-APPL-COUNT.
           DISPLAY 'Applications Approved:  ' WS-APPROVED-COUNT.
           DISPLAY 'Applications Denied:    ' WS-DENIED-COUNT.
           DISPLAY 'Applications Pending:   ' WS-PENDING-COUNT.

           IF WS-APPL-COUNT > ZERO
               COMPUTE WS-DISPLAY-PERCENT =
                   (WS-APPROVED-COUNT / WS-APPL-COUNT) * 100
               DISPLAY 'Approval Rate:          '
                   WS-DISPLAY-PERCENT '%'
           END-IF.

       CLEANUP-ENROLLMENT-SYSTEM SECTION.
       CLEANUP.
           CLOSE APPLICATION-FILE.
           DISPLAY 'Enrollment processing complete.'.
