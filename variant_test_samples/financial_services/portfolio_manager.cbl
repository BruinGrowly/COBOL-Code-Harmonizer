       IDENTIFICATION DIVISION.
       PROGRAM-ID. PortfolioManager.
      *FINANCIAL SERVICES - INVESTMENT PORTFOLIO MANAGEMENT
      *Handles complex portfolio calculations and risk analysis

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PORTFOLIO-FILE ASSIGN TO 'portfolio.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PORT-ACCOUNT-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PORTFOLIO-FILE.
       01  PORTFOLIO-RECORD.
           05  PORT-ACCOUNT-ID         PIC 9(12).
           05  PORT-CUSTOMER-ID        PIC 9(10).
           05  PORT-ACCOUNT-TYPE       PIC X(20).
           05  PORT-TOTAL-VALUE        PIC 9(12)V99 COMP-3.
           05  PORT-CASH-BALANCE       PIC 9(10)V99 COMP-3.
           05  PORT-LAST-UPDATED       PIC X(26).
           05  PORT-RISK-PROFILE       PIC X(15).
           05  PORT-MANAGED-FLAG       PIC X.

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS              PIC XX.

      *Asset holdings
       01  WS-HOLDINGS-TABLE.
           05  WS-HOLDING OCCURS 20 TIMES INDEXED BY HOLD-IDX.
               10  WS-SYMBOL           PIC X(10).
               10  WS-ASSET-TYPE       PIC X(15).
               10  WS-SHARES           PIC 9(8).
               10  WS-COST-BASIS       PIC 9(8)V99 COMP-3.
               10  WS-CURRENT-PRICE    PIC 9(8)V99 COMP-3.
               10  WS-MARKET-VALUE     PIC 9(10)V99 COMP-3.
               10  WS-GAIN-LOSS        PIC S9(10)V99 COMP-3.
               10  WS-GAIN-LOSS-PCT    PIC S9(3)V99 COMP-3.

      *Portfolio analytics
       01  WS-PORTFOLIO-ANALYTICS.
           05  WS-TOTAL-COST-BASIS     PIC 9(12)V99 COMP-3.
           05  WS-TOTAL-MARKET-VALUE   PIC 9(12)V99 COMP-3.
           05  WS-TOTAL-GAIN-LOSS      PIC S9(12)V99 COMP-3.
           05  WS-TOTAL-GAIN-LOSS-PCT  PIC S9(4)V99 COMP-3.
           05  WS-CASH-PERCENTAGE      PIC 9(3)V99.
           05  WS-EQUITY-PERCENTAGE    PIC 9(3)V99.
           05  WS-FIXED-INCOME-PCT     PIC 9(3)V99.
           05  WS-ALTERNATIVE-PCT      PIC 9(3)V99.

      *Risk metrics
       01  WS-RISK-METRICS.
           05  WS-PORTFOLIO-BETA       PIC S9V9999 COMP-2.
           05  WS-SHARPE-RATIO         PIC S9V9999 COMP-2.
           05  WS-VOLATILITY           PIC 9(3)V99.
           05  WS-VAR-95               PIC 9(10)V99.
           05  WS-MAX-DRAWDOWN         PIC S9(3)V99.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-PERCENT          PIC ZZ9.99.
       01  WS-DISPLAY-SHARES           PIC ZZZ,ZZZ,ZZ9.

       01  WS-HOLDING-COUNT            PIC 99 VALUE ZERO.
       01  WS-ACCOUNT-ID               PIC 9(12).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PORTFOLIO-SYSTEM.
           PERFORM LOAD-SAMPLE-PORTFOLIO.
           PERFORM CALCULATE-PORTFOLIO-VALUES.
           PERFORM ANALYZE-ASSET-ALLOCATION.
           PERFORM CALCULATE-RISK-METRICS.
           PERFORM GENERATE-PORTFOLIO-REPORT.
           PERFORM CLEANUP-PORTFOLIO-SYSTEM.
           STOP RUN.

       INITIALIZE-PORTFOLIO-SYSTEM SECTION.
       INIT-SYSTEM.
           DISPLAY '========================================'.
           DISPLAY 'Investment Portfolio Manager'.
           DISPLAY 'Financial Services Division'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

           OPEN OUTPUT PORTFOLIO-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening portfolio file: '
                   WS-FILE-STATUS
               STOP RUN
           END-IF.

           DISPLAY 'Portfolio system initialized'.
           DISPLAY SPACE.

       LOAD-SAMPLE-PORTFOLIO SECTION.
       LOAD-PORTFOLIO.
           DISPLAY 'Loading portfolio holdings...'.

           MOVE 123456789012 TO WS-ACCOUNT-ID.

      *Stock holdings
           ADD 1 TO WS-HOLDING-COUNT.
           SET HOLD-IDX TO WS-HOLDING-COUNT.
           MOVE 'AAPL' TO WS-SYMBOL(HOLD-IDX).
           MOVE 'EQUITY-TECH' TO WS-ASSET-TYPE(HOLD-IDX).
           MOVE 150 TO WS-SHARES(HOLD-IDX).
           MOVE 165.50 TO WS-COST-BASIS(HOLD-IDX).
           MOVE 175.25 TO WS-CURRENT-PRICE(HOLD-IDX).

           ADD 1 TO WS-HOLDING-COUNT.
           SET HOLD-IDX TO WS-HOLDING-COUNT.
           MOVE 'MSFT' TO WS-SYMBOL(HOLD-IDX).
           MOVE 'EQUITY-TECH' TO WS-ASSET-TYPE(HOLD-IDX).
           MOVE 200 TO WS-SHARES(HOLD-IDX).
           MOVE 280.00 TO WS-COST-BASIS(HOLD-IDX).
           MOVE 305.75 TO WS-CURRENT-PRICE(HOLD-IDX).

           ADD 1 TO WS-HOLDING-COUNT.
           SET HOLD-IDX TO WS-HOLDING-COUNT.
           MOVE 'JPM' TO WS-SYMBOL(HOLD-IDX).
           MOVE 'EQUITY-FINANCE' TO WS-ASSET-TYPE(HOLD-IDX).
           MOVE 100 TO WS-SHARES(HOLD-IDX).
           MOVE 145.00 TO WS-COST-BASIS(HOLD-IDX).
           MOVE 152.50 TO WS-CURRENT-PRICE(HOLD-IDX).

      *Bond holdings
           ADD 1 TO WS-HOLDING-COUNT.
           SET HOLD-IDX TO WS-HOLDING-COUNT.
           MOVE 'GOVT10YR' TO WS-SYMBOL(HOLD-IDX).
           MOVE 'FIXED-INCOME' TO WS-ASSET-TYPE(HOLD-IDX).
           MOVE 100 TO WS-SHARES(HOLD-IDX).
           MOVE 98.50 TO WS-COST-BASIS(HOLD-IDX).
           MOVE 97.75 TO WS-CURRENT-PRICE(HOLD-IDX).

           ADD 1 TO WS-HOLDING-COUNT.
           SET HOLD-IDX TO WS-HOLDING-COUNT.
           MOVE 'CORPBOND' TO WS-SYMBOL(HOLD-IDX).
           MOVE 'FIXED-INCOME' TO WS-ASSET-TYPE(HOLD-IDX).
           MOVE 50 TO WS-SHARES(HOLD-IDX).
           MOVE 102.00 TO WS-COST-BASIS(HOLD-IDX).
           MOVE 103.25 TO WS-CURRENT-PRICE(HOLD-IDX).

           DISPLAY 'Loaded ' WS-HOLDING-COUNT ' holdings'.
           DISPLAY SPACE.

       CALCULATE-PORTFOLIO-VALUES SECTION.
       CALC-VALUES.
           DISPLAY 'Calculating portfolio values...'.
           DISPLAY '--------------------------------------------'.

           MOVE ZERO TO WS-TOTAL-COST-BASIS.
           MOVE ZERO TO WS-TOTAL-MARKET-VALUE.

           PERFORM VARYING HOLD-IDX FROM 1 BY 1
               UNTIL HOLD-IDX > WS-HOLDING-COUNT

               COMPUTE WS-MARKET-VALUE(HOLD-IDX) =
                   WS-SHARES(HOLD-IDX) *
                   WS-CURRENT-PRICE(HOLD-IDX)

               COMPUTE WS-GAIN-LOSS(HOLD-IDX) =
                   (WS-CURRENT-PRICE(HOLD-IDX) -
                    WS-COST-BASIS(HOLD-IDX)) *
                   WS-SHARES(HOLD-IDX)

               IF WS-COST-BASIS(HOLD-IDX) > ZERO
                   COMPUTE WS-GAIN-LOSS-PCT(HOLD-IDX) =
                       ((WS-CURRENT-PRICE(HOLD-IDX) -
                         WS-COST-BASIS(HOLD-IDX)) /
                        WS-COST-BASIS(HOLD-IDX)) * 100
               ELSE
                   MOVE ZERO TO WS-GAIN-LOSS-PCT(HOLD-IDX)
               END-IF

               ADD WS-MARKET-VALUE(HOLD-IDX) TO
                   WS-TOTAL-MARKET-VALUE

               COMPUTE WS-TOTAL-COST-BASIS =
                   WS-TOTAL-COST-BASIS +
                   (WS-SHARES(HOLD-IDX) * WS-COST-BASIS(HOLD-IDX))

               PERFORM DISPLAY-HOLDING-DETAILS
           END-PERFORM.

           COMPUTE WS-TOTAL-GAIN-LOSS =
               WS-TOTAL-MARKET-VALUE - WS-TOTAL-COST-BASIS.

           IF WS-TOTAL-COST-BASIS > ZERO
               COMPUTE WS-TOTAL-GAIN-LOSS-PCT =
                   (WS-TOTAL-GAIN-LOSS / WS-TOTAL-COST-BASIS) * 100
           ELSE
               MOVE ZERO TO WS-TOTAL-GAIN-LOSS-PCT
           END-IF.

           DISPLAY SPACE.

       DISPLAY-HOLDING-DETAILS SECTION.
       SHOW-HOLDING.
           MOVE WS-SHARES(HOLD-IDX) TO WS-DISPLAY-SHARES.
           MOVE WS-CURRENT-PRICE(HOLD-IDX) TO WS-DISPLAY-AMOUNT.

           DISPLAY WS-SYMBOL(HOLD-IDX) ' | '
                   WS-ASSET-TYPE(HOLD-IDX) ' | '
                   'Shares: ' WS-DISPLAY-SHARES ' | '
                   'Price: ' WS-DISPLAY-AMOUNT.

           MOVE WS-MARKET-VALUE(HOLD-IDX) TO WS-DISPLAY-AMOUNT.
           MOVE WS-GAIN-LOSS-PCT(HOLD-IDX) TO WS-DISPLAY-PERCENT.

           DISPLAY '  Market Value: ' WS-DISPLAY-AMOUNT
                   ' | Gain/Loss: ' WS-DISPLAY-PERCENT '%'.

       ANALYZE-ASSET-ALLOCATION SECTION.
       ANALYZE-ALLOCATION.
           DISPLAY 'Analyzing asset allocation...'.

           MOVE ZERO TO WS-EQUITY-PERCENTAGE.
           MOVE ZERO TO WS-FIXED-INCOME-PCT.

           PERFORM VARYING HOLD-IDX FROM 1 BY 1
               UNTIL HOLD-IDX > WS-HOLDING-COUNT

               IF WS-ASSET-TYPE(HOLD-IDX)(1:6) = 'EQUITY'
                   COMPUTE WS-EQUITY-PERCENTAGE =
                       WS-EQUITY-PERCENTAGE +
                       ((WS-MARKET-VALUE(HOLD-IDX) /
                         WS-TOTAL-MARKET-VALUE) * 100)
               END-IF

               IF WS-ASSET-TYPE(HOLD-IDX) = 'FIXED-INCOME'
                   COMPUTE WS-FIXED-INCOME-PCT =
                       WS-FIXED-INCOME-PCT +
                       ((WS-MARKET-VALUE(HOLD-IDX) /
                         WS-TOTAL-MARKET-VALUE) * 100)
               END-IF
           END-PERFORM.

           DISPLAY 'Asset Allocation:'.
           MOVE WS-EQUITY-PERCENTAGE TO WS-DISPLAY-PERCENT.
           DISPLAY '  Equities: ' WS-DISPLAY-PERCENT '%'.
           MOVE WS-FIXED-INCOME-PCT TO WS-DISPLAY-PERCENT.
           DISPLAY '  Fixed Income: ' WS-DISPLAY-PERCENT '%'.
           DISPLAY SPACE.

       CALCULATE-RISK-METRICS SECTION.
       CALC-RISK.
           DISPLAY 'Calculating risk metrics...'.

      *Simplified risk calculations for demonstration
           COMPUTE WS-PORTFOLIO-BETA = 1.15.
           COMPUTE WS-SHARPE-RATIO = 1.35.
           COMPUTE WS-VOLATILITY = 18.5.

           COMPUTE WS-VAR-95 =
               WS-TOTAL-MARKET-VALUE * 0.05.

           COMPUTE WS-MAX-DRAWDOWN = -12.3.

           DISPLAY 'Risk Profile:'.
           DISPLAY '  Beta: ' WS-PORTFOLIO-BETA.
           DISPLAY '  Sharpe Ratio: ' WS-SHARPE-RATIO.
           MOVE WS-VOLATILITY TO WS-DISPLAY-PERCENT.
           DISPLAY '  Volatility: ' WS-DISPLAY-PERCENT '%'.
           MOVE WS-VAR-95 TO WS-DISPLAY-AMOUNT.
           DISPLAY '  VaR (95%): ' WS-DISPLAY-AMOUNT.
           DISPLAY SPACE.

       GENERATE-PORTFOLIO-REPORT SECTION.
       GEN-REPORT.
           DISPLAY '========================================'.
           DISPLAY 'Portfolio Summary Report'.
           DISPLAY '========================================'.

           DISPLAY 'Account: ' WS-ACCOUNT-ID.
           DISPLAY 'Holdings: ' WS-HOLDING-COUNT ' positions'.
           DISPLAY SPACE.

           MOVE WS-TOTAL-COST-BASIS TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Cost Basis:   ' WS-DISPLAY-AMOUNT.

           MOVE WS-TOTAL-MARKET-VALUE TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Total Market Value: ' WS-DISPLAY-AMOUNT.

           MOVE WS-TOTAL-GAIN-LOSS TO WS-DISPLAY-AMOUNT.
           MOVE WS-TOTAL-GAIN-LOSS-PCT TO WS-DISPLAY-PERCENT.
           DISPLAY 'Total Gain/Loss:    ' WS-DISPLAY-AMOUNT
                   ' (' WS-DISPLAY-PERCENT '%)'.

           DISPLAY SPACE.

       CLEANUP-PORTFOLIO-SYSTEM SECTION.
       CLEANUP.
           CLOSE PORTFOLIO-FILE.
           DISPLAY 'Portfolio analysis complete.'.
