       IDENTIFICATION DIVISION.
       PROGRAM-ID. TaxCalculation.
      *GOVERNMENT - TAX CALCULATION SYSTEM
      *Processes individual income tax returns

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *Taxpayer information
       01  WS-TAXPAYER-INFO.
           05  WS-SSN                  PIC 9(9).
           05  WS-TAXPAYER-NAME        PIC X(50).
           05  WS-FILING-STATUS        PIC X(20).
           05  WS-DEPENDENTS           PIC 99.
           05  WS-TAX-YEAR             PIC 9(4) VALUE 2024.

      *Income information
       01  WS-INCOME.
           05  WS-WAGES                PIC 9(9)V99.
           05  WS-INTEREST             PIC 9(7)V99.
           05  WS-DIVIDENDS            PIC 9(7)V99.
           05  WS-BUSINESS-INCOME      PIC S9(9)V99.
           05  WS-CAPITAL-GAINS        PIC S9(9)V99.
           05  WS-OTHER-INCOME         PIC S9(9)V99.
           05  WS-TOTAL-INCOME         PIC S9(10)V99.

      *Adjustments to income
       01  WS-ADJUSTMENTS.
           05  WS-IRA-DEDUCTION        PIC 9(6)V99.
           05  WS-STUDENT-LOAN-INT     PIC 9(5)V99.
           05  WS-HSA-CONTRIBUTION     PIC 9(5)V99.
           05  WS-SELF-EMPLOYMENT-TAX  PIC 9(6)V99.
           05  WS-TOTAL-ADJUSTMENTS    PIC 9(7)V99.

      *Deductions
       01  WS-DEDUCTIONS.
           05  WS-STANDARD-DEDUCTION   PIC 9(6)V99.
           05  WS-ITEMIZED-DEDUCTION   PIC 9(7)V99.
           05  WS-DEDUCTION-USED       PIC 9(7)V99.

      *Tax calculation
       01  WS-TAX-CALCULATION.
           05  WS-ADJUSTED-GROSS-INC   PIC S9(10)V99.
           05  WS-TAXABLE-INCOME       PIC S9(10)V99.
           05  WS-TAX-BEFORE-CREDITS   PIC 9(8)V99.
           05  WS-CHILD-TAX-CREDIT     PIC 9(5)V99.
           05  WS-EARNED-INCOME-CREDIT PIC 9(5)V99.
           05  WS-OTHER-CREDITS        PIC 9(5)V99.
           05  WS-TOTAL-TAX            PIC S9(8)V99.

      *Payments and refund
       01  WS-PAYMENTS.
           05  WS-FEDERAL-WITHHOLDING  PIC 9(8)V99.
           05  WS-ESTIMATED-PAYMENTS   PIC 9(7)V99.
           05  WS-TOTAL-PAYMENTS       PIC 9(8)V99.
           05  WS-REFUND-OR-OWED       PIC S9(8)V99.

      *Tax bracket tables (2024)
       01  WS-TAX-BRACKETS.
           05  WS-BRACKET OCCURS 7 TIMES.
               10  WS-BRACKET-MIN      PIC 9(9)V99.
               10  WS-BRACKET-MAX      PIC 9(9)V99.
               10  WS-BRACKET-RATE     PIC 99V99.
               10  WS-BRACKET-BASE-TAX PIC 9(7)V99.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-RATE             PIC Z9.99.
       01  WS-SCENARIO-NUM             PIC 9 VALUE 1.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM DISPLAY-SYSTEM-HEADER.
           PERFORM INITIALIZE-TAX-TABLES.
           PERFORM CALCULATE-SCENARIO-1.
           PERFORM CALCULATE-SCENARIO-2.
           PERFORM CALCULATE-SCENARIO-3.
           DISPLAY 'Tax calculations complete.'.
           STOP RUN.

       DISPLAY-SYSTEM-HEADER SECTION.
       SHOW-HEADER.
           DISPLAY '========================================'.
           DISPLAY 'Federal Income Tax Calculation System'.
           DISPLAY 'Internal Revenue Service'.
           DISPLAY 'Tax Year: ' WS-TAX-YEAR.
           DISPLAY '========================================'.
           DISPLAY SPACE.

       INITIALIZE-TAX-TABLES SECTION.
       INIT-TABLES.
      *2024 Tax brackets for Single filers (simplified)
           MOVE 0.00 TO WS-BRACKET-MIN(1).
           MOVE 11600.00 TO WS-BRACKET-MAX(1).
           MOVE 10.00 TO WS-BRACKET-RATE(1).
           MOVE 0.00 TO WS-BRACKET-BASE-TAX(1).

           MOVE 11600.01 TO WS-BRACKET-MIN(2).
           MOVE 47150.00 TO WS-BRACKET-MAX(2).
           MOVE 12.00 TO WS-BRACKET-RATE(2).
           MOVE 1160.00 TO WS-BRACKET-BASE-TAX(2).

           MOVE 47150.01 TO WS-BRACKET-MIN(3).
           MOVE 100525.00 TO WS-BRACKET-MAX(3).
           MOVE 22.00 TO WS-BRACKET-RATE(3).
           MOVE 5426.00 TO WS-BRACKET-BASE-TAX(3).

           MOVE 100525.01 TO WS-BRACKET-MIN(4).
           MOVE 191950.00 TO WS-BRACKET-MAX(4).
           MOVE 24.00 TO WS-BRACKET-RATE(4).
           MOVE 17168.50 TO WS-BRACKET-BASE-TAX(4).

           MOVE 191950.01 TO WS-BRACKET-MIN(5).
           MOVE 243725.00 TO WS-BRACKET-MAX(5).
           MOVE 32.00 TO WS-BRACKET-RATE(5).
           MOVE 39110.50 TO WS-BRACKET-BASE-TAX(5).

           MOVE 243725.01 TO WS-BRACKET-MIN(6).
           MOVE 609350.00 TO WS-BRACKET-MAX(6).
           MOVE 35.00 TO WS-BRACKET-RATE(6).
           MOVE 55678.50 TO WS-BRACKET-BASE-TAX(6).

           MOVE 609350.01 TO WS-BRACKET-MIN(7).
           MOVE 99999999.99 TO WS-BRACKET-MAX(7).
           MOVE 37.00 TO WS-BRACKET-RATE(7).
           MOVE 183647.25 TO WS-BRACKET-BASE-TAX(7).

       CALCULATE-SCENARIO-1 SECTION.
       CALC-SCENARIO-1.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Middle Income Single Filer'.
           DISPLAY '--------------------------------------------'.

      *Taxpayer info
           MOVE 123456789 TO WS-SSN.
           MOVE 'John Smith' TO WS-TAXPAYER-NAME.
           MOVE 'SINGLE' TO WS-FILING-STATUS.
           MOVE 0 TO WS-DEPENDENTS.

      *Income
           MOVE 75000.00 TO WS-WAGES.
           MOVE 150.00 TO WS-INTEREST.
           MOVE 300.00 TO WS-DIVIDENDS.
           MOVE 0.00 TO WS-BUSINESS-INCOME.
           MOVE 0.00 TO WS-CAPITAL-GAINS.
           MOVE 0.00 TO WS-OTHER-INCOME.

      *Adjustments
           MOVE 3500.00 TO WS-IRA-DEDUCTION.
           MOVE 1200.00 TO WS-STUDENT-LOAN-INT.
           MOVE 0.00 TO WS-HSA-CONTRIBUTION.
           MOVE 0.00 TO WS-SELF-EMPLOYMENT-TAX.

      *Deductions
           MOVE 14600.00 TO WS-STANDARD-DEDUCTION.
           MOVE 0.00 TO WS-ITEMIZED-DEDUCTION.

      *Payments
           MOVE 12000.00 TO WS-FEDERAL-WITHHOLDING.
           MOVE 0.00 TO WS-ESTIMATED-PAYMENTS.

           PERFORM CALCULATE-TAX-RETURN.
           PERFORM DISPLAY-TAX-RETURN.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-SCENARIO-2 SECTION.
       CALC-SCENARIO-2.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': High Income with Dependents'.
           DISPLAY '--------------------------------------------'.

      *Taxpayer info
           MOVE 987654321 TO WS-SSN.
           MOVE 'Jane Williams' TO WS-TAXPAYER-NAME.
           MOVE 'HEAD-OF-HOUSEHOLD' TO WS-FILING-STATUS.
           MOVE 2 TO WS-DEPENDENTS.

      *Income
           MOVE 150000.00 TO WS-WAGES.
           MOVE 2500.00 TO WS-INTEREST.
           MOVE 5000.00 TO WS-DIVIDENDS.
           MOVE 25000.00 TO WS-BUSINESS-INCOME.
           MOVE 8000.00 TO WS-CAPITAL-GAINS.
           MOVE 0.00 TO WS-OTHER-INCOME.

      *Adjustments
           MOVE 7000.00 TO WS-IRA-DEDUCTION.
           MOVE 0.00 TO WS-STUDENT-LOAN-INT.
           MOVE 4150.00 TO WS-HSA-CONTRIBUTION.
           MOVE 3532.50 TO WS-SELF-EMPLOYMENT-TAX.

      *Deductions
           MOVE 21900.00 TO WS-STANDARD-DEDUCTION.
           MOVE 35000.00 TO WS-ITEMIZED-DEDUCTION.

      *Payments
           MOVE 35000.00 TO WS-FEDERAL-WITHHOLDING.
           MOVE 8000.00 TO WS-ESTIMATED-PAYMENTS.

           PERFORM CALCULATE-TAX-RETURN.
           PERFORM DISPLAY-TAX-RETURN.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-SCENARIO-3 SECTION.
       CALC-SCENARIO-3.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Low Income with EITC'.
           DISPLAY '--------------------------------------------'.

      *Taxpayer info
           MOVE 555667777 TO WS-SSN.
           MOVE 'Maria Garcia' TO WS-TAXPAYER-NAME.
           MOVE 'HEAD-OF-HOUSEHOLD' TO WS-FILING-STATUS.
           MOVE 1 TO WS-DEPENDENTS.

      *Income
           MOVE 28000.00 TO WS-WAGES.
           MOVE 50.00 TO WS-INTEREST.
           MOVE 0.00 TO WS-DIVIDENDS.
           MOVE 0.00 TO WS-BUSINESS-INCOME.
           MOVE 0.00 TO WS-CAPITAL-GAINS.
           MOVE 0.00 TO WS-OTHER-INCOME.

      *Adjustments
           MOVE 0.00 TO WS-IRA-DEDUCTION.
           MOVE 0.00 TO WS-STUDENT-LOAN-INT.
           MOVE 0.00 TO WS-HSA-CONTRIBUTION.
           MOVE 0.00 TO WS-SELF-EMPLOYMENT-TAX.

      *Deductions
           MOVE 21900.00 TO WS-STANDARD-DEDUCTION.
           MOVE 0.00 TO WS-ITEMIZED-DEDUCTION.

      *Payments
           MOVE 2100.00 TO WS-FEDERAL-WITHHOLDING.
           MOVE 0.00 TO WS-ESTIMATED-PAYMENTS.

           PERFORM CALCULATE-TAX-RETURN.
           PERFORM DISPLAY-TAX-RETURN.

           DISPLAY SPACE.

       CALCULATE-TAX-RETURN SECTION.
       CALC-RETURN.
      *Step 1: Calculate total income
           COMPUTE WS-TOTAL-INCOME =
               WS-WAGES +
               WS-INTEREST +
               WS-DIVIDENDS +
               WS-BUSINESS-INCOME +
               WS-CAPITAL-GAINS +
               WS-OTHER-INCOME.

      *Step 2: Calculate adjustments
           COMPUTE WS-TOTAL-ADJUSTMENTS =
               WS-IRA-DEDUCTION +
               WS-STUDENT-LOAN-INT +
               WS-HSA-CONTRIBUTION +
               WS-SELF-EMPLOYMENT-TAX.

      *Step 3: Calculate adjusted gross income
           COMPUTE WS-ADJUSTED-GROSS-INC =
               WS-TOTAL-INCOME - WS-TOTAL-ADJUSTMENTS.

      *Step 4: Determine deduction to use
           IF WS-ITEMIZED-DEDUCTION > WS-STANDARD-DEDUCTION
               MOVE WS-ITEMIZED-DEDUCTION TO WS-DEDUCTION-USED
           ELSE
               MOVE WS-STANDARD-DEDUCTION TO WS-DEDUCTION-USED
           END-IF.

      *Step 5: Calculate taxable income
           COMPUTE WS-TAXABLE-INCOME =
               WS-ADJUSTED-GROSS-INC - WS-DEDUCTION-USED.

           IF WS-TAXABLE-INCOME < ZERO
               MOVE ZERO TO WS-TAXABLE-INCOME
           END-IF.

      *Step 6: Calculate tax using brackets
           PERFORM CALCULATE-TAX-FROM-BRACKETS.

      *Step 7: Calculate credits
           PERFORM CALCULATE-TAX-CREDITS.

      *Step 8: Calculate total tax
           COMPUTE WS-TOTAL-TAX =
               WS-TAX-BEFORE-CREDITS -
               WS-CHILD-TAX-CREDIT -
               WS-EARNED-INCOME-CREDIT -
               WS-OTHER-CREDITS.

           IF WS-TOTAL-TAX < ZERO
               MOVE ZERO TO WS-TOTAL-TAX
           END-IF.

      *Step 9: Calculate payments
           COMPUTE WS-TOTAL-PAYMENTS =
               WS-FEDERAL-WITHHOLDING +
               WS-ESTIMATED-PAYMENTS.

      *Step 10: Calculate refund or owed
           COMPUTE WS-REFUND-OR-OWED =
               WS-TOTAL-PAYMENTS - WS-TOTAL-TAX.

       CALCULATE-TAX-FROM-BRACKETS SECTION.
       CALC-BRACKETS.
           MOVE ZERO TO WS-TAX-BEFORE-CREDITS.

      *Find applicable tax bracket
           PERFORM VARYING WS-SCENARIO-NUM FROM 1 BY 1
               UNTIL WS-SCENARIO-NUM > 7 OR
                     WS-TAXABLE-INCOME <= WS-BRACKET-MAX(WS-SCENARIO-NUM)

               IF WS-TAXABLE-INCOME >= WS-BRACKET-MIN(WS-SCENARIO-NUM)
                   COMPUTE WS-TAX-BEFORE-CREDITS =
                       WS-BRACKET-BASE-TAX(WS-SCENARIO-NUM) +
                       ((WS-TAXABLE-INCOME -
                         WS-BRACKET-MIN(WS-SCENARIO-NUM)) *
                        (WS-BRACKET-RATE(WS-SCENARIO-NUM) / 100))
               END-IF
           END-PERFORM.

       CALCULATE-TAX-CREDITS SECTION.
       CALC-CREDITS.
           MOVE ZERO TO WS-CHILD-TAX-CREDIT.
           MOVE ZERO TO WS-EARNED-INCOME-CREDIT.
           MOVE ZERO TO WS-OTHER-CREDITS.

      *Child Tax Credit ($2000 per child)
           IF WS-DEPENDENTS > 0
               COMPUTE WS-CHILD-TAX-CREDIT = WS-DEPENDENTS * 2000
           END-IF.

      *Earned Income Tax Credit (simplified)
           IF WS-ADJUSTED-GROSS-INC < 60000 AND
              WS-DEPENDENTS > 0
               COMPUTE WS-EARNED-INCOME-CREDIT = 3000
           END-IF.

       DISPLAY-TAX-RETURN SECTION.
       SHOW-RETURN.
           DISPLAY 'Taxpayer: ' WS-TAXPAYER-NAME.
           DISPLAY 'Filing Status: ' WS-FILING-STATUS.
           DISPLAY 'Dependents: ' WS-DEPENDENTS.
           DISPLAY SPACE.

           DISPLAY 'Income:'.
           MOVE WS-TOTAL-INCOME TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Total Income:       ' WS-DISPLAY-AMOUNT.
           MOVE WS-TOTAL-ADJUSTMENTS TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Adjustments:        ' WS-DISPLAY-AMOUNT.
           MOVE WS-ADJUSTED-GROSS-INC TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Adjusted Gross:     ' WS-DISPLAY-AMOUNT.
           MOVE WS-DEDUCTION-USED TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Deductions:         ' WS-DISPLAY-AMOUNT.
           MOVE WS-TAXABLE-INCOME TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Taxable Income:     ' WS-DISPLAY-AMOUNT.
           DISPLAY SPACE.

           DISPLAY 'Tax:'.
           MOVE WS-TAX-BEFORE-CREDITS TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Tax Before Credits: ' WS-DISPLAY-AMOUNT.

           IF WS-CHILD-TAX-CREDIT > ZERO
               MOVE WS-CHILD-TAX-CREDIT TO WS-DISPLAY-AMOUNT
               DISPLAY '  Child Tax Credit:   ' WS-DISPLAY-AMOUNT
           END-IF.

           IF WS-EARNED-INCOME-CREDIT > ZERO
               MOVE WS-EARNED-INCOME-CREDIT TO WS-DISPLAY-AMOUNT
               DISPLAY '  Earned Inc Credit:  ' WS-DISPLAY-AMOUNT
           END-IF.

           MOVE WS-TOTAL-TAX TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Total Tax:          ' WS-DISPLAY-AMOUNT.
           DISPLAY SPACE.

           DISPLAY 'Payments:'.
           MOVE WS-TOTAL-PAYMENTS TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Total Payments:     ' WS-DISPLAY-AMOUNT.
           DISPLAY SPACE.

           IF WS-REFUND-OR-OWED >= ZERO
               MOVE WS-REFUND-OR-OWED TO WS-DISPLAY-AMOUNT
               DISPLAY 'REFUND:               ' WS-DISPLAY-AMOUNT
           ELSE
               COMPUTE WS-DISPLAY-AMOUNT = - WS-REFUND-OR-OWED
               DISPLAY 'AMOUNT OWED:          ' WS-DISPLAY-AMOUNT
           END-IF.
