       IDENTIFICATION DIVISION.
       PROGRAM-ID. SocialSecurityBenefits.
      *GOVERNMENT - SOCIAL SECURITY BENEFITS CALCULATOR
      *Calculates retirement, disability, and survivor benefits

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *Beneficiary information
       01  WS-BENEFICIARY-INFO.
           05  WS-SSN                  PIC 9(9).
           05  WS-NAME                 PIC X(50).
           05  WS-BIRTH-DATE           PIC 9(8).
           05  WS-CURRENT-AGE          PIC 9(3).
           05  WS-FULL-RETIREMENT-AGE  PIC 9(2)V99.
           05  WS-GENDER               PIC X.

      *Work history
       01  WS-WORK-HISTORY.
           05  WS-YEARS-WORKED         PIC 99.
           05  WS-TOTAL-EARNINGS       PIC 9(11)V99.
           05  WS-AVG-INDEXED-EARNINGS PIC 9(9)V99.
           05  WS-HIGHEST-35-YEARS-TOT PIC 9(11)V99.

      *Earnings by year (simplified - last 10 years)
       01  WS-EARNINGS-TABLE.
           05  WS-YEAR-EARNINGS OCCURS 10 TIMES.
               10  WS-YEAR             PIC 9(4).
               10  WS-EARNINGS         PIC 9(7)V99.
               10  WS-INDEXED-EARNINGS PIC 9(7)V99.

      *Benefit calculation
       01  WS-BENEFIT-CALCULATION.
           05  WS-PRIMARY-INS-AMOUNT   PIC 9(6)V99.
           05  WS-RETIREMENT-AGE       PIC 9(2)V99.
           05  WS-EARLY-RETIRE-REDUCT  PIC 9V9999.
           05  WS-DELAYED-RETIRE-INC   PIC 9V9999.
           05  WS-MONTHLY-BENEFIT      PIC 9(6)V99.
           05  WS-ANNUAL-BENEFIT       PIC 9(7)V99.

      *Benefit bend points (2024)
       01  WS-BEND-POINTS.
           05  WS-BEND-POINT-1         PIC 9(5) VALUE 1115.
           05  WS-BEND-POINT-2         PIC 9(5) VALUE 6721.

      *Maximum earnings subject to tax (2024)
       01  WS-MAX-TAXABLE-EARNINGS     PIC 9(7) VALUE 168600.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-YEARS            PIC ZZ9.
       01  WS-DISPLAY-AGE              PIC Z9.99.

       01  WS-SCENARIO-NUM             PIC 9 VALUE 1.
       01  WS-LOOP-INDEX               PIC 99.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM DISPLAY-SYSTEM-HEADER.
           PERFORM CALCULATE-SCENARIO-1.
           PERFORM CALCULATE-SCENARIO-2.
           PERFORM CALCULATE-SCENARIO-3.
           DISPLAY 'Benefit calculations complete.'.
           STOP RUN.

       DISPLAY-SYSTEM-HEADER SECTION.
       SHOW-HEADER.
           DISPLAY '========================================'.
           DISPLAY 'Social Security Benefits Calculator'.
           DISPLAY 'Social Security Administration'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

       CALCULATE-SCENARIO-1 SECTION.
       CALC-SCENARIO-1.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Full Retirement Age'.
           DISPLAY '--------------------------------------------'.

      *Beneficiary info
           MOVE 123456789 TO WS-SSN.
           MOVE 'Robert Johnson' TO WS-NAME.
           MOVE 19580315 TO WS-BIRTH-DATE.
           MOVE 66 TO WS-CURRENT-AGE.
           MOVE 66.67 TO WS-FULL-RETIREMENT-AGE.
           MOVE 'M' TO WS-GENDER.

      *Work history (high earner)
           MOVE 40 TO WS-YEARS-WORKED.

      *Setup earnings for last 10 years
           PERFORM SETUP-HIGH-EARNINGS.

           PERFORM CALCULATE-AVERAGE-INDEXED-EARNINGS.
           PERFORM CALCULATE-PRIMARY-INSURANCE-AMOUNT.

      *Calculate benefit at full retirement age
           MOVE WS-FULL-RETIREMENT-AGE TO WS-RETIREMENT-AGE.
           MOVE 0.0 TO WS-EARLY-RETIRE-REDUCT.
           MOVE 0.0 TO WS-DELAYED-RETIRE-INC.

           PERFORM CALCULATE-MONTHLY-BENEFIT.
           PERFORM DISPLAY-BENEFIT-DETAILS.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-SCENARIO-2 SECTION.
       CALC-SCENARIO-2.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Early Retirement (Age 62)'.
           DISPLAY '--------------------------------------------'.

      *Beneficiary info
           MOVE 987654321 TO WS-SSN.
           MOVE 'Linda Martinez' TO WS-NAME.
           MOVE 19630528 TO WS-BIRTH-DATE.
           MOVE 61 TO WS-CURRENT-AGE.
           MOVE 67.00 TO WS-FULL-RETIREMENT-AGE.
           MOVE 'F' TO WS-GENDER.

      *Work history (average earner)
           MOVE 35 TO WS-YEARS-WORKED.

      *Setup earnings
           PERFORM SETUP-AVERAGE-EARNINGS.

           PERFORM CALCULATE-AVERAGE-INDEXED-EARNINGS.
           PERFORM CALCULATE-PRIMARY-INSURANCE-AMOUNT.

      *Calculate benefit at age 62 (early)
           MOVE 62.00 TO WS-RETIREMENT-AGE.
           COMPUTE WS-EARLY-RETIRE-REDUCT =
               (WS-FULL-RETIREMENT-AGE - WS-RETIREMENT-AGE) * 0.0067.

           PERFORM CALCULATE-MONTHLY-BENEFIT.
           PERFORM DISPLAY-BENEFIT-DETAILS.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-SCENARIO-3 SECTION.
       CALC-SCENARIO-3.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Delayed Retirement (Age 70)'.
           DISPLAY '--------------------------------------------'.

      *Beneficiary info
           MOVE 555667788 TO WS-SSN.
           MOVE 'David Chen' TO WS-NAME.
           MOVE 19540110 TO WS-BIRTH-DATE.
           MOVE 70 TO WS-CURRENT-AGE.
           MOVE 66.00 TO WS-FULL-RETIREMENT-AGE.
           MOVE 'M' TO WS-GENDER.

      *Work history (maximum earner)
           MOVE 42 TO WS-YEARS-WORKED.

      *Setup earnings
           PERFORM SETUP-MAXIMUM-EARNINGS.

           PERFORM CALCULATE-AVERAGE-INDEXED-EARNINGS.
           PERFORM CALCULATE-PRIMARY-INSURANCE-AMOUNT.

      *Calculate benefit at age 70 (delayed)
           MOVE 70.00 TO WS-RETIREMENT-AGE.
           COMPUTE WS-DELAYED-RETIRE-INC =
               (WS-RETIREMENT-AGE - WS-FULL-RETIREMENT-AGE) * 0.08.

           PERFORM CALCULATE-MONTHLY-BENEFIT.
           PERFORM DISPLAY-BENEFIT-DETAILS.

           DISPLAY SPACE.

       SETUP-HIGH-EARNINGS SECTION.
       SETUP-HIGH.
      *High earner - consistently earning near maximum
           MOVE 2024 TO WS-YEAR(1).
           MOVE 155000.00 TO WS-EARNINGS(1).
           MOVE 2023 TO WS-YEAR(2).
           MOVE 150000.00 TO WS-EARNINGS(2).
           MOVE 2022 TO WS-YEAR(3).
           MOVE 145000.00 TO WS-EARNINGS(3).
           MOVE 2021 TO WS-YEAR(4).
           MOVE 140000.00 TO WS-EARNINGS(4).
           MOVE 2020 TO WS-YEAR(5).
           MOVE 137000.00 TO WS-EARNINGS(5).
           MOVE 2019 TO WS-YEAR(6).
           MOVE 132900.00 TO WS-EARNINGS(6).
           MOVE 2018 TO WS-YEAR(7).
           MOVE 128400.00 TO WS-EARNINGS(7).
           MOVE 2017 TO WS-YEAR(8).
           MOVE 127200.00 TO WS-EARNINGS(8).
           MOVE 2016 TO WS-YEAR(9).
           MOVE 118500.00 TO WS-EARNINGS(9).
           MOVE 2015 TO WS-YEAR(10).
           MOVE 118500.00 TO WS-EARNINGS(10).

      *Copy to indexed earnings (simplified)
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > 10
               MOVE WS-EARNINGS(WS-LOOP-INDEX) TO
                   WS-INDEXED-EARNINGS(WS-LOOP-INDEX)
           END-PERFORM.

       SETUP-AVERAGE-EARNINGS SECTION.
       SETUP-AVG.
      *Average earner - consistent middle income
           MOVE 2024 TO WS-YEAR(1).
           MOVE 55000.00 TO WS-EARNINGS(1).
           MOVE 2023 TO WS-YEAR(2).
           MOVE 52000.00 TO WS-EARNINGS(2).
           MOVE 2022 TO WS-YEAR(3).
           MOVE 50000.00 TO WS-EARNINGS(3).
           MOVE 2021 TO WS-YEAR(4).
           MOVE 48000.00 TO WS-EARNINGS(4).
           MOVE 2020 TO WS-YEAR(5).
           MOVE 47000.00 TO WS-EARNINGS(5).
           MOVE 2019 TO WS-YEAR(6).
           MOVE 45000.00 TO WS-EARNINGS(6).
           MOVE 2018 TO WS-YEAR(7).
           MOVE 43000.00 TO WS-EARNINGS(7).
           MOVE 2017 TO WS-YEAR(8).
           MOVE 42000.00 TO WS-EARNINGS(8).
           MOVE 2016 TO WS-YEAR(9).
           MOVE 40000.00 TO WS-EARNINGS(9).
           MOVE 2015 TO WS-YEAR(10).
           MOVE 38000.00 TO WS-EARNINGS(10).

           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > 10
               MOVE WS-EARNINGS(WS-LOOP-INDEX) TO
                   WS-INDEXED-EARNINGS(WS-LOOP-INDEX)
           END-PERFORM.

       SETUP-MAXIMUM-EARNINGS SECTION.
       SETUP-MAX.
      *Maximum earner - always at or above cap
           MOVE 2024 TO WS-YEAR(1).
           MOVE 168600.00 TO WS-EARNINGS(1).
           MOVE 2023 TO WS-YEAR(2).
           MOVE 160200.00 TO WS-EARNINGS(2).
           MOVE 2022 TO WS-YEAR(3).
           MOVE 147000.00 TO WS-EARNINGS(3).
           MOVE 2021 TO WS-YEAR(4).
           MOVE 142800.00 TO WS-EARNINGS(4).
           MOVE 2020 TO WS-YEAR(5).
           MOVE 137700.00 TO WS-EARNINGS(5).
           MOVE 2019 TO WS-YEAR(6).
           MOVE 132900.00 TO WS-EARNINGS(6).
           MOVE 2018 TO WS-YEAR(7).
           MOVE 128400.00 TO WS-EARNINGS(7).
           MOVE 2017 TO WS-YEAR(8).
           MOVE 127200.00 TO WS-EARNINGS(8).
           MOVE 2016 TO WS-YEAR(9).
           MOVE 118500.00 TO WS-EARNINGS(9).
           MOVE 2015 TO WS-YEAR(10).
           MOVE 118500.00 TO WS-EARNINGS(10).

           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > 10
               MOVE WS-EARNINGS(WS-LOOP-INDEX) TO
                   WS-INDEXED-EARNINGS(WS-LOOP-INDEX)
           END-PERFORM.

       CALCULATE-AVERAGE-INDEXED-EARNINGS SECTION.
       CALC-AIME.
      *Calculate average of highest 35 years
      *Simplified: use last 10 years only for demo
           MOVE ZERO TO WS-HIGHEST-35-YEARS-TOT.

           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > 10
               ADD WS-INDEXED-EARNINGS(WS-LOOP-INDEX) TO
                   WS-HIGHEST-35-YEARS-TOT
           END-PERFORM.

      *Average monthly earnings (10 years = 120 months)
           COMPUTE WS-AVG-INDEXED-EARNINGS =
               WS-HIGHEST-35-YEARS-TOT / 120.

       CALCULATE-PRIMARY-INSURANCE-AMOUNT SECTION.
       CALC-PIA.
      *Apply bend point formula (2024)
      *90% of first $1,115
      *32% of amount between $1,115 and $6,721
      *15% of amount over $6,721

           MOVE ZERO TO WS-PRIMARY-INS-AMOUNT.

      *First bend point
           IF WS-AVG-INDEXED-EARNINGS >= WS-BEND-POINT-1
               COMPUTE WS-PRIMARY-INS-AMOUNT =
                   WS-BEND-POINT-1 * 0.90
           ELSE
               COMPUTE WS-PRIMARY-INS-AMOUNT =
                   WS-AVG-INDEXED-EARNINGS * 0.90
               GO TO CALC-PIA-EXIT
           END-IF.

      *Second bend point
           IF WS-AVG-INDEXED-EARNINGS >= WS-BEND-POINT-2
               COMPUTE WS-PRIMARY-INS-AMOUNT =
                   WS-PRIMARY-INS-AMOUNT +
                   ((WS-BEND-POINT-2 - WS-BEND-POINT-1) * 0.32)
           ELSE
               COMPUTE WS-PRIMARY-INS-AMOUNT =
                   WS-PRIMARY-INS-AMOUNT +
                   ((WS-AVG-INDEXED-EARNINGS - WS-BEND-POINT-1) *
                    0.32)
               GO TO CALC-PIA-EXIT
           END-IF.

      *Amount over second bend point
           COMPUTE WS-PRIMARY-INS-AMOUNT =
               WS-PRIMARY-INS-AMOUNT +
               ((WS-AVG-INDEXED-EARNINGS - WS-BEND-POINT-2) * 0.15).

       CALC-PIA-EXIT.
           EXIT.

       CALCULATE-MONTHLY-BENEFIT SECTION.
       CALC-MONTHLY.
           MOVE WS-PRIMARY-INS-AMOUNT TO WS-MONTHLY-BENEFIT.

      *Apply early retirement reduction
           IF WS-EARLY-RETIRE-REDUCT > ZERO
               COMPUTE WS-MONTHLY-BENEFIT =
                   WS-MONTHLY-BENEFIT *
                   (1 - WS-EARLY-RETIRE-REDUCT)
           END-IF.

      *Apply delayed retirement increase
           IF WS-DELAYED-RETIRE-INC > ZERO
               COMPUTE WS-MONTHLY-BENEFIT =
                   WS-MONTHLY-BENEFIT *
                   (1 + WS-DELAYED-RETIRE-INC)
           END-IF.

      *Calculate annual benefit
           COMPUTE WS-ANNUAL-BENEFIT = WS-MONTHLY-BENEFIT * 12.

       DISPLAY-BENEFIT-DETAILS SECTION.
       SHOW-DETAILS.
           DISPLAY 'Beneficiary: ' WS-NAME.
           DISPLAY 'SSN: ' WS-SSN.
           DISPLAY 'Current Age: ' WS-CURRENT-AGE.
           MOVE WS-YEARS-WORKED TO WS-DISPLAY-YEARS.
           DISPLAY 'Years Worked: ' WS-DISPLAY-YEARS.
           DISPLAY SPACE.

           DISPLAY 'Earnings Summary:'.
           MOVE WS-AVG-INDEXED-EARNINGS TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Average Indexed Monthly: ' WS-DISPLAY-AMOUNT.
           DISPLAY SPACE.

           DISPLAY 'Benefit Calculation:'.
           MOVE WS-PRIMARY-INS-AMOUNT TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Primary Insurance Amt:   ' WS-DISPLAY-AMOUNT.
           MOVE WS-FULL-RETIREMENT-AGE TO WS-DISPLAY-AGE.
           DISPLAY '  Full Retirement Age:     ' WS-DISPLAY-AGE.
           MOVE WS-RETIREMENT-AGE TO WS-DISPLAY-AGE.
           DISPLAY '  Claiming Age:            ' WS-DISPLAY-AGE.

           IF WS-EARLY-RETIRE-REDUCT > ZERO
               DISPLAY '  Early Retirement Penalty: '
                   WS-EARLY-RETIRE-REDUCT * 100 '%'
           END-IF.

           IF WS-DELAYED-RETIRE-INC > ZERO
               DISPLAY '  Delayed Retirement Credit: '
                   WS-DELAYED-RETIRE-INC * 100 '%'
           END-IF.

           DISPLAY SPACE.
           MOVE WS-MONTHLY-BENEFIT TO WS-DISPLAY-AMOUNT.
           DISPLAY 'MONTHLY BENEFIT:         ' WS-DISPLAY-AMOUNT.
           MOVE WS-ANNUAL-BENEFIT TO WS-DISPLAY-AMOUNT.
           DISPLAY 'ANNUAL BENEFIT:          ' WS-DISPLAY-AMOUNT.
