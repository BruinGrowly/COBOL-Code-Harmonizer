       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCERNING-EXAMPLE.
      *****************************************************************
      * This file demonstrates CONCERNING drift (score 0.5-0.8)
      * Names don't match implementations well
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-RECORD.
           05  WS-CUST-ID       PIC 9(6).
           05  WS-CUST-NAME     PIC X(30).
           05  WS-BALANCE       PIC 9(7)V99.
           05  WS-STATUS        PIC X(10).

       PROCEDURE DIVISION.

      *****************************************************************
      * CONCERNING: Name says CALCULATE but code WRITES
      * Score: ~0.65
      *****************************************************************
       CALCULATE-TOTAL-BALANCE.
           EXEC SQL
               INSERT INTO BALANCE_HISTORY
               VALUES (:WS-CUST-ID, :WS-BALANCE, CURRENT_DATE)
           END-EXEC.

      *****************************************************************
      * CONCERNING: Name says GET but code VALIDATES and MODIFIES
      * Score: ~0.72
      *****************************************************************
       GET-CUSTOMER-DATA.
           READ CUSTOMER-FILE
               INTO WS-CUSTOMER-RECORD
           END-READ.

           IF WS-BALANCE < 0
               MOVE 'OVERDRAWN' TO WS-STATUS
               REWRITE CUSTOMER-RECORD
           END-IF.

      *****************************************************************
      * CONCERNING: Name says DISPLAY but code UPDATES
      * Score: ~0.55
      *****************************************************************
       DISPLAY-ACCOUNT-INFO.
           DISPLAY 'Processing account: ' WS-CUST-ID.

           EXEC SQL
               UPDATE LAST_ACCESS
               SET ACCESS_DATE = CURRENT_DATE
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.

       STOP RUN.
