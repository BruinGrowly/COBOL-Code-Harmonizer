       IDENTIFICATION DIVISION.
       PROGRAM-ID. FinancialCalculator.
      *COBOL-2014 IEEE FLOATING-POINT DEMONSTRATION
      *Demonstrates IEEE 754 floating-point arithmetic

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *IEEE Floating-Point Variables (COBOL-2014)
       01  WS-PRINCIPAL           COMP-2.
       01  WS-INTEREST-RATE       COMP-2.
       01  WS-YEARS               PIC 9(2).
       01  WS-MONTHLY-PAYMENT     COMP-2.
       01  WS-TOTAL-INTEREST      COMP-2.
       01  WS-FUTURE-VALUE        COMP-2.

      *Investment Portfolio Data
       01  WS-PORTFOLIO.
           05  WS-STOCK-VALUE     COMP-2.
           05  WS-BOND-VALUE      COMP-2.
           05  WS-CASH-VALUE      COMP-2.
           05  WS-TOTAL-VALUE     COMP-2.
           05  WS-RETURN-PCT      COMP-2.

      *Calculation Workspace
       01  WS-TEMP-CALC           COMP-2.
       01  WS-MONTHLY-RATE        COMP-2.
       01  WS-NUM-PAYMENTS        PIC 9(4).
       01  WS-COMPOUND-FACTOR     COMP-2.

      *Display Variables
       01  WS-DISPLAY-AMOUNT      PIC $$,$$$,$$9.99.
       01  WS-DISPLAY-RATE        PIC 9.9999.
       01  WS-DISPLAY-PERCENT     PIC ZZ9.99.

      *Control Variables
       01  WS-SCENARIO-NUM        PIC 9 VALUE 1.
       01  WS-CONTINUE-FLAG       PIC X VALUE 'Y'.

       PROCEDURE DIVISION.

       MAIN-LOGIC SECTION.
       MAIN-PROCEDURE.
           DISPLAY '======================================'.
           DISPLAY 'Financial Calculator (IEEE 754)'.
           DISPLAY 'COBOL-2014 Floating-Point Demo'.
           DISPLAY '======================================'.
           DISPLAY ' '.

           PERFORM RUN-SCENARIO-1.
           PERFORM RUN-SCENARIO-2.
           PERFORM RUN-SCENARIO-3.
           PERFORM RUN-SCENARIO-4.

           DISPLAY ' '.
           DISPLAY 'All calculations complete.'.
           STOP RUN.

       RUN-SCENARIO-1 SECTION.
       SCENARIO-1.
      *Mortgage Payment Calculation
           DISPLAY 'Scenario 1: Mortgage Payment Calculator'.
           DISPLAY '----------------------------------------'.

           MOVE 250000.00 TO WS-PRINCIPAL.
           MOVE 0.0375 TO WS-INTEREST-RATE.
           MOVE 30 TO WS-YEARS.

           PERFORM CALCULATE-MORTGAGE-PAYMENT.
           PERFORM DISPLAY-MORTGAGE-RESULTS.
           DISPLAY ' '.

       CALCULATE-MORTGAGE-PAYMENT SECTION.
       CALC-MORTGAGE.
      *Calculate monthly mortgage payment using formula:
      *M = P * [r(1+r)^n] / [(1+r)^n - 1]

           COMPUTE WS-MONTHLY-RATE = WS-INTEREST-RATE / 12.
           COMPUTE WS-NUM-PAYMENTS = WS-YEARS * 12.

      *Calculate (1 + r)^n
           COMPUTE WS-COMPOUND-FACTOR =
               (1 + WS-MONTHLY-RATE) ** WS-NUM-PAYMENTS
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.

      *Calculate monthly payment
           COMPUTE WS-MONTHLY-PAYMENT =
               WS-PRINCIPAL *
               (WS-MONTHLY-RATE * WS-COMPOUND-FACTOR) /
               (WS-COMPOUND-FACTOR - 1)
               ROUNDED MODE IS NEAREST-TOWARD-ZERO
               ON SIZE ERROR
                   DISPLAY 'Error: Payment calculation overflow'
           END-COMPUTE.

      *Calculate total interest paid
           COMPUTE WS-TOTAL-INTEREST =
               (WS-MONTHLY-PAYMENT * WS-NUM-PAYMENTS) - WS-PRINCIPAL.

       DISPLAY-MORTGAGE-RESULTS SECTION.
       SHOW-MORTGAGE.
           MOVE WS-PRINCIPAL TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Principal: ' WS-DISPLAY-AMOUNT.

           MOVE WS-INTEREST-RATE TO WS-DISPLAY-RATE.
           DISPLAY 'Annual Rate: ' WS-DISPLAY-RATE.

           DISPLAY 'Term: ' WS-YEARS ' years'.

           MOVE WS-MONTHLY-PAYMENT TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Monthly Payment: ' WS-DISPLAY-AMOUNT.

           MOVE WS-TOTAL-INTEREST TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Interest: ' WS-DISPLAY-AMOUNT.

       RUN-SCENARIO-2 SECTION.
       SCENARIO-2.
      *Investment Growth Calculation
           DISPLAY 'Scenario 2: Investment Growth (Compound Interest)'.
           DISPLAY '------------------------------------------------'.

           MOVE 50000.00 TO WS-PRINCIPAL.
           MOVE 0.07 TO WS-INTEREST-RATE.
           MOVE 20 TO WS-YEARS.

           PERFORM CALCULATE-INVESTMENT-GROWTH.
           PERFORM DISPLAY-INVESTMENT-RESULTS.
           DISPLAY ' '.

       CALCULATE-INVESTMENT-GROWTH SECTION.
       CALC-INVESTMENT.
      *Calculate future value: FV = P * (1 + r)^t

           COMPUTE WS-FUTURE-VALUE =
               WS-PRINCIPAL * ((1 + WS-INTEREST-RATE) ** WS-YEARS)
               ROUNDED MODE IS NEAREST-TOWARD-ZERO
               ON SIZE ERROR
                   DISPLAY 'Error: Future value overflow'
           END-COMPUTE.

           COMPUTE WS-TOTAL-INTEREST =
               WS-FUTURE-VALUE - WS-PRINCIPAL.

       DISPLAY-INVESTMENT-RESULTS SECTION.
       SHOW-INVESTMENT.
           MOVE WS-PRINCIPAL TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Initial Investment: ' WS-DISPLAY-AMOUNT.

           COMPUTE WS-DISPLAY-PERCENT = WS-INTEREST-RATE * 100.
           DISPLAY 'Annual Return: ' WS-DISPLAY-PERCENT '%'.

           DISPLAY 'Time Period: ' WS-YEARS ' years'.

           MOVE WS-FUTURE-VALUE TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Future Value: ' WS-DISPLAY-AMOUNT.

           MOVE WS-TOTAL-INTEREST TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Gain: ' WS-DISPLAY-AMOUNT.

       RUN-SCENARIO-3 SECTION.
       SCENARIO-3.
      *Portfolio Analysis
           DISPLAY 'Scenario 3: Investment Portfolio Analysis'.
           DISPLAY '-----------------------------------------'.

           MOVE 125000.00 TO WS-STOCK-VALUE.
           MOVE 75000.00 TO WS-BOND-VALUE.
           MOVE 25000.00 TO WS-CASH-VALUE.

           PERFORM ANALYZE-PORTFOLIO.
           PERFORM DISPLAY-PORTFOLIO-RESULTS.
           DISPLAY ' '.

       ANALYZE-PORTFOLIO SECTION.
       ANALYZE-PORT.
           COMPUTE WS-TOTAL-VALUE =
               WS-STOCK-VALUE + WS-BOND-VALUE + WS-CASH-VALUE.

      *Calculate weighted return
      *Assuming: Stocks 8%, Bonds 4%, Cash 1%
           COMPUTE WS-RETURN-PCT =
               ((WS-STOCK-VALUE * 0.08) +
                (WS-BOND-VALUE * 0.04) +
                (WS-CASH-VALUE * 0.01)) / WS-TOTAL-VALUE
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.

      *Calculate one-year projected growth
           COMPUTE WS-FUTURE-VALUE =
               WS-TOTAL-VALUE * (1 + WS-RETURN-PCT).

       DISPLAY-PORTFOLIO-RESULTS SECTION.
       SHOW-PORTFOLIO.
           DISPLAY 'Asset Allocation:'.

           MOVE WS-STOCK-VALUE TO WS-DISPLAY-AMOUNT.
           COMPUTE WS-DISPLAY-PERCENT =
               (WS-STOCK-VALUE / WS-TOTAL-VALUE) * 100.
           DISPLAY '  Stocks: ' WS-DISPLAY-AMOUNT
                   ' (' WS-DISPLAY-PERCENT '%)'.

           MOVE WS-BOND-VALUE TO WS-DISPLAY-AMOUNT.
           COMPUTE WS-DISPLAY-PERCENT =
               (WS-BOND-VALUE / WS-TOTAL-VALUE) * 100.
           DISPLAY '  Bonds:  ' WS-DISPLAY-AMOUNT
                   ' (' WS-DISPLAY-PERCENT '%)'.

           MOVE WS-CASH-VALUE TO WS-DISPLAY-AMOUNT.
           COMPUTE WS-DISPLAY-PERCENT =
               (WS-CASH-VALUE / WS-TOTAL-VALUE) * 100.
           DISPLAY '  Cash:   ' WS-DISPLAY-AMOUNT
                   ' (' WS-DISPLAY-PERCENT '%)'.

           MOVE WS-TOTAL-VALUE TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Portfolio Value: ' WS-DISPLAY-AMOUNT.

           COMPUTE WS-DISPLAY-PERCENT = WS-RETURN-PCT * 100.
           DISPLAY 'Expected Annual Return: ' WS-DISPLAY-PERCENT '%'.

           MOVE WS-FUTURE-VALUE TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Projected Value (1 year): ' WS-DISPLAY-AMOUNT.

       RUN-SCENARIO-4 SECTION.
       SCENARIO-4.
      *Loan Amortization
           DISPLAY 'Scenario 4: Auto Loan Amortization'.
           DISPLAY '-----------------------------------'.

           MOVE 35000.00 TO WS-PRINCIPAL.
           MOVE 0.0499 TO WS-INTEREST-RATE.
           MOVE 5 TO WS-YEARS.

           PERFORM CALCULATE-AUTO-LOAN.
           PERFORM DISPLAY-LOAN-RESULTS.

       CALCULATE-AUTO-LOAN SECTION.
       CALC-AUTO.
           COMPUTE WS-MONTHLY-RATE = WS-INTEREST-RATE / 12.
           COMPUTE WS-NUM-PAYMENTS = WS-YEARS * 12.

           COMPUTE WS-COMPOUND-FACTOR =
               (1 + WS-MONTHLY-RATE) ** WS-NUM-PAYMENTS.

           COMPUTE WS-MONTHLY-PAYMENT =
               WS-PRINCIPAL * WS-MONTHLY-RATE * WS-COMPOUND-FACTOR /
               (WS-COMPOUND-FACTOR - 1)
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.

           COMPUTE WS-TOTAL-INTEREST =
               (WS-MONTHLY-PAYMENT * WS-NUM-PAYMENTS) - WS-PRINCIPAL.

           COMPUTE WS-FUTURE-VALUE =
               WS-MONTHLY-PAYMENT * WS-NUM-PAYMENTS.

       DISPLAY-LOAN-RESULTS SECTION.
       SHOW-LOAN.
           MOVE WS-PRINCIPAL TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Loan Amount: ' WS-DISPLAY-AMOUNT.

           COMPUTE WS-DISPLAY-PERCENT = WS-INTEREST-RATE * 100.
           DISPLAY 'Interest Rate: ' WS-DISPLAY-PERCENT '%'.

           DISPLAY 'Term: ' WS-YEARS ' years (' WS-NUM-PAYMENTS
                   ' payments)'.

           MOVE WS-MONTHLY-PAYMENT TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Monthly Payment: ' WS-DISPLAY-AMOUNT.

           MOVE WS-TOTAL-INTEREST TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Interest Paid: ' WS-DISPLAY-AMOUNT.

           MOVE WS-FUTURE-VALUE TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Amount Paid: ' WS-DISPLAY-AMOUNT.
