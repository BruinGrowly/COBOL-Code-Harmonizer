       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISHARMONIOUS-EXAMPLE.
      *****************************************************************
      * Example of DISHARMONIOUS COBOL code where procedure names    *
      * LIE about their actual behavior - SEMANTIC BUGS!             *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUST-ID              PIC 9(6).
           05 CUST-NAME            PIC X(30).
           05 CUST-BALANCE         PIC 9(7)V99.
           05 CUST-STATUS          PIC X.
           05 LAST-ACCESS-DATE     PIC 9(8).

       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05 WS-CURRENT-DATE      PIC 9(8).
           05 WS-RESULT            PIC X(20).

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           OPEN I-O CUSTOMER-FILE.
           PERFORM GET-CUSTOMER-BALANCE.
           PERFORM CHECK-ACCOUNT-STATUS.
           PERFORM VALIDATE-CUSTOMER-RECORD.
           CLOSE CUSTOMER-FILE.
           STOP RUN.

      *****************************************************************
      * CRITICAL DISHARMONY 💥                                        *
      * Intent: Wisdom (GET = retrieve) - Score: ~0.8                *
      * Execution: Power (DELETE!) - Score: ~0.7                     *
      * Disharmony: 1.12 - CRITICAL BUG! 💥                          *
      * BUG: Name says "GET" but code DELETES the record!            *
      *****************************************************************
       GET-CUSTOMER-BALANCE.
           MOVE 123456 TO CUST-ID.
           READ CUSTOMER-FILE
               KEY IS CUST-ID
               INVALID KEY MOVE ZEROS TO CUST-BALANCE
           END-READ.

           DISPLAY 'Balance: ' CUST-BALANCE.

      *    WAIT, WE'RE DELETING?!
           DELETE CUSTOMER-FILE RECORD
               INVALID KEY DISPLAY 'Delete failed'
           END-DELETE.

           MOVE CUST-BALANCE TO WS-RESULT.

      *****************************************************************
      * SIGNIFICANT DISHARMONY 🔴                                     *
      * Intent: Justice (CHECK = validate) - Score: ~0.7             *
      * Execution: Power (REWRITE, COMPUTE) - Score: ~0.6            *
      * Disharmony: 0.95 - SIGNIFICANT BUG! 🔴                       *
      * BUG: Name says "CHECK" but actually MODIFIES data!           *
      *****************************************************************
       CHECK-ACCOUNT-STATUS.
           MOVE 123456 TO CUST-ID.
           READ CUSTOMER-FILE
               KEY IS CUST-ID
           END-READ.

      *    This is NOT checking, this is UPDATING!
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE TO LAST-ACCESS-DATE.
           COMPUTE CUST-BALANCE = CUST-BALANCE * 1.05.

           REWRITE CUSTOMER-RECORD.

           IF CUST-BALANCE > 10000
               MOVE 'VIP' TO CUST-STATUS
               REWRITE CUSTOMER-RECORD
           END-IF.

      *****************************************************************
      * SIGNIFICANT DISHARMONY 🔴                                     *
      * Intent: Justice (VALIDATE = verify) - Score: ~0.8            *
      * Execution: Power (WRITE, MOVE) - Score: ~0.6                 *
      * Disharmony: 0.87 - SIGNIFICANT BUG! 🔴                       *
      * BUG: Validator has massive side effects!                     *
      *****************************************************************
       VALIDATE-CUSTOMER-RECORD.
           IF CUST-ID = SPACES OR ZEROS
               MOVE 'INVALID' TO WS-RESULT
      *        Wait, we're creating records during validation?
               MOVE 999999 TO CUST-ID
               MOVE 'DEFAULT CUSTOMER' TO CUST-NAME
               MOVE 0 TO CUST-BALANCE
               WRITE CUSTOMER-RECORD
           ELSE
               MOVE 'VALID' TO WS-RESULT
      *        And updating last-access during validation?
               ACCEPT LAST-ACCESS-DATE FROM DATE YYYYMMDD
               REWRITE CUSTOMER-RECORD
           END-IF.

      *****************************************************************
      * CONCERNING DISHARMONY ⚠️                                      *
      * Intent: Wisdom (DISPLAY = show) - Score: ~0.6                *
      * Execution: Power (MOVE, REWRITE) - Score: ~0.5               *
      * Disharmony: 0.62 - CONCERNING ⚠️                             *
      * BUG: Display function modifies state as side effect          *
      *****************************************************************
       DISPLAY-CUSTOMER-INFO.
           DISPLAY 'Customer: ' CUST-NAME.
           DISPLAY 'Balance: ' CUST-BALANCE.

      *    Side effect: updating last-access
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE TO LAST-ACCESS-DATE.
           REWRITE CUSTOMER-RECORD.

      *****************************************************************
      * MINOR DISHARMONY (Acceptable pattern) ⚠️                      *
      * Intent: Mixed (PROCESS = vague) - Score: ~0.4                *
      * Execution: Power + Wisdom - Score: ~0.45                     *
      * Disharmony: 0.42 - MINOR DRIFT ⚠️                            *
      * Issue: Name is too vague, but execution is acceptable        *
      *****************************************************************
       PROCESS-CUSTOMER.
           READ CUSTOMER-FILE.
           IF CUST-STATUS = 'ACTIVE'
               PERFORM CALCULATE-INTEREST
               REWRITE CUSTOMER-RECORD
           END-IF.

       CALCULATE-INTEREST.
           COMPUTE CUST-BALANCE = CUST-BALANCE * 1.03.
