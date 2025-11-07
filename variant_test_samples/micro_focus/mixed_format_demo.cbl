      $SET SOURCEFORMAT"VARIABLE"
      $SET DIALECT"MF"
      $SET PERFORM-TYPE"MF"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MixedFormatDemo.
      *MICRO FOCUS - MIXED FORMAT DEMONSTRATION
      *Combines fixed and variable format in single program

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
               ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TRANS-ID                PIC 9(10).
           05  TRANS-TYPE              PIC X(10).
           05  TRANS-AMOUNT            PIC S9(9)V99.
           05  TRANS-DATE              PIC X(10).
           05  TRANS-DESCRIPTION       PIC X(50).

       WORKING-STORAGE SECTION.

      *File status
       01  WS-FILE-STATUS              PIC XX.
       01  WS-EOF-FLAG                 PIC X VALUE "N".

      *Transaction processing
       01  WS-TRANSACTION-DATA.
           05  WS-TRANS-COUNT          PIC 9(6) VALUE ZERO.
           05  WS-DEBIT-TOTAL          PIC S9(11)V99 VALUE ZERO.
           05  WS-CREDIT-TOTAL         PIC S9(11)V99 VALUE ZERO.
           05  WS-NET-BALANCE          PIC S9(11)V99 VALUE ZERO.

      *Display formatting
       01  WS-DISPLAY-AMOUNT           PIC $$,$$$,$$9.99-.
       01  WS-DISPLAY-COUNT            PIC ZZZ,ZZ9.

      *Date handling
       01  WS-CURRENT-DATE.
           05  WS-YEAR                 PIC 9(4).
           05  WS-MONTH                PIC 99.
           05  WS-DAY                  PIC 99.
           05  WS-TIME                 PIC 9(6).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM DISPLAY-HEADER.
           PERFORM INITIALIZE-PROCESSING.
           PERFORM GENERATE-TRANSACTIONS.
           PERFORM PROCESS-ALL-TRANSACTIONS.
           PERFORM DISPLAY-SUMMARY.
           PERFORM CLEANUP-PROCESSING.
           STOP RUN.

       DISPLAY-HEADER SECTION.
       SHOW-HEADER.
           DISPLAY "========================================".
           DISPLAY "Micro Focus Mixed Format Demo".
           DISPLAY "Transaction Processing System".
           DISPLAY "========================================".
           DISPLAY SPACE.

       INITIALIZE-PROCESSING SECTION.
       INIT-PROC.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

           DISPLAY "Initializing transaction processing...".
           OPEN OUTPUT TRANSACTION-FILE.

           IF WS-FILE-STATUS NOT = "00" THEN
               DISPLAY "Error opening transaction file: "
                   WS-FILE-STATUS
               STOP RUN
           END-IF.

           DISPLAY "Transaction file opened successfully".
           DISPLAY SPACE.

       GENERATE-TRANSACTIONS SECTION.
       GEN-TRANS.
           DISPLAY "Generating sample transactions...".

      *Transaction 1 - Deposit
           MOVE 1001 TO TRANS-ID.
           MOVE "DEPOSIT" TO TRANS-TYPE.
           MOVE +1500.00 TO TRANS-AMOUNT.
           STRING WS-YEAR "/" WS-MONTH "/" WS-DAY
               DELIMITED BY SIZE INTO TRANS-DATE.
           MOVE "Payroll deposit" TO TRANS-DESCRIPTION.
           PERFORM WRITE-TRANSACTION.

      *Transaction 2 - Withdrawal
           MOVE 1002 TO TRANS-ID.
           MOVE "WITHDRAWAL" TO TRANS-TYPE.
           MOVE -250.00 TO TRANS-AMOUNT.
           STRING WS-YEAR "/" WS-MONTH "/" WS-DAY
               DELIMITED BY SIZE INTO TRANS-DATE.
           MOVE "ATM withdrawal" TO TRANS-DESCRIPTION.
           PERFORM WRITE-TRANSACTION.

      *Transaction 3 - Payment
           MOVE 1003 TO TRANS-ID.
           MOVE "PAYMENT" TO TRANS-TYPE.
           MOVE -89.95 TO TRANS-AMOUNT.
           STRING WS-YEAR "/" WS-MONTH "/" WS-DAY
               DELIMITED BY SIZE INTO TRANS-DATE.
           MOVE "Online purchase" TO TRANS-DESCRIPTION.
           PERFORM WRITE-TRANSACTION.

      *Transaction 4 - Deposit
           MOVE 1004 TO TRANS-ID.
           MOVE "DEPOSIT" TO TRANS-TYPE.
           MOVE +3200.00 TO TRANS-AMOUNT.
           STRING WS-YEAR "/" WS-MONTH "/" WS-DAY
               DELIMITED BY SIZE INTO TRANS-DATE.
           MOVE "Wire transfer received" TO TRANS-DESCRIPTION.
           PERFORM WRITE-TRANSACTION.

      *Transaction 5 - Fee
           MOVE 1005 TO TRANS-ID.
           MOVE "FEE" TO TRANS-TYPE.
           MOVE -15.00 TO TRANS-AMOUNT.
           STRING WS-YEAR "/" WS-MONTH "/" WS-DAY
               DELIMITED BY SIZE INTO TRANS-DATE.
           MOVE "Monthly service fee" TO TRANS-DESCRIPTION.
           PERFORM WRITE-TRANSACTION.

      *Transaction 6 - Interest
           MOVE 1006 TO TRANS-ID.
           MOVE "INTEREST" TO TRANS-TYPE.
           MOVE +42.55 TO TRANS-AMOUNT.
           STRING WS-YEAR "/" WS-MONTH "/" WS-DAY
               DELIMITED BY SIZE INTO TRANS-DATE.
           MOVE "Interest earned" TO TRANS-DESCRIPTION.
           PERFORM WRITE-TRANSACTION.

           DISPLAY "Generated " WS-TRANS-COUNT " transactions".
           DISPLAY SPACE.

           CLOSE TRANSACTION-FILE.
           OPEN INPUT TRANSACTION-FILE.

       WRITE-TRANSACTION SECTION.
       WRITE-TRANS.
           WRITE TRANSACTION-RECORD.

           IF WS-FILE-STATUS = "00" THEN
               ADD 1 TO WS-TRANS-COUNT
           ELSE
               DISPLAY "Write error: " WS-FILE-STATUS
           END-IF.

       PROCESS-ALL-TRANSACTIONS SECTION.
       PROC-ALL.
           DISPLAY "Processing transactions...".
           DISPLAY "--------------------------------------------".

           MOVE "N" TO WS-EOF-FLAG.

           PERFORM UNTIL WS-EOF-FLAG = "Y"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-SINGLE-TRANSACTION
               END-READ
           END-PERFORM.

           DISPLAY SPACE.

       PROCESS-SINGLE-TRANSACTION SECTION.
       PROC-SINGLE.
      *Categorize transaction
           IF TRANS-AMOUNT > ZERO THEN
               ADD TRANS-AMOUNT TO WS-CREDIT-TOTAL
           ELSE
               ADD TRANS-AMOUNT TO WS-DEBIT-TOTAL
           END-IF.

      *Display transaction details
           MOVE TRANS-AMOUNT TO WS-DISPLAY-AMOUNT.

           DISPLAY TRANS-ID " | "
                   TRANS-TYPE " | "
                   WS-DISPLAY-AMOUNT " | "
                   TRANS-DATE " | "
                   TRANS-DESCRIPTION(1:30).

      *Validate transaction
           PERFORM VALIDATE-TRANSACTION.

       VALIDATE-TRANSACTION SECTION.
       VALIDATE.
      *Business rule validations
           IF TRANS-TYPE = "WITHDRAWAL" AND
              TRANS-AMOUNT >= ZERO THEN
               DISPLAY "  WARNING: Withdrawal should be negative"
           END-IF.

           IF TRANS-TYPE = "DEPOSIT" AND
              TRANS-AMOUNT < ZERO THEN
               DISPLAY "  WARNING: Deposit should be positive"
           END-IF.

           IF TRANS-AMOUNT > 10000.00 OR
              TRANS-AMOUNT < -5000.00 THEN
               DISPLAY "  ALERT: Large transaction - review required"
           END-IF.

       DISPLAY-SUMMARY SECTION.
       SHOW-SUMMARY.
           COMPUTE WS-NET-BALANCE =
               WS-CREDIT-TOTAL + WS-DEBIT-TOTAL.

           DISPLAY "========================================".
           DISPLAY "Transaction Summary".
           DISPLAY "========================================".

           MOVE WS-TRANS-COUNT TO WS-DISPLAY-COUNT.
           DISPLAY "Total Transactions: " WS-DISPLAY-COUNT.

           MOVE WS-CREDIT-TOTAL TO WS-DISPLAY-AMOUNT.
           DISPLAY "Total Credits:      " WS-DISPLAY-AMOUNT.

           MOVE WS-DEBIT-TOTAL TO WS-DISPLAY-AMOUNT.
           DISPLAY "Total Debits:       " WS-DISPLAY-AMOUNT.

           MOVE WS-NET-BALANCE TO WS-DISPLAY-AMOUNT.
           DISPLAY "Net Balance:        " WS-DISPLAY-AMOUNT.

           DISPLAY SPACE.

           IF WS-NET-BALANCE > ZERO THEN
               DISPLAY "Status: Positive balance"
           ELSE IF WS-NET-BALANCE < ZERO THEN
               DISPLAY "Status: Negative balance - overdraft"
           ELSE
               DISPLAY "Status: Zero balance"
           END-IF.

           DISPLAY SPACE.

       CLEANUP-PROCESSING SECTION.
       CLEANUP.
           CLOSE TRANSACTION-FILE.
           DISPLAY "Processing complete.".

       END PROGRAM MixedFormatDemo.
