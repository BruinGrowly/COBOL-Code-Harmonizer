       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGNIFICANT-EXAMPLE.
      *****************************************************************
      * This file demonstrates SIGNIFICANT disharmony (score 0.8-1.2)
      * Names contradict implementations - refactoring needed
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
      * SIGNIFICANT: Name says RETRIEVE but code DELETES!
      * Score: ~0.95
      *****************************************************************
       RETRIEVE-CUSTOMER-ACCOUNT.
           EXEC SQL
               DELETE FROM CUSTOMERS
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.

      *****************************************************************
      * SIGNIFICANT: Name says VALIDATE but code CREATES records
      * Score: ~0.88
      *****************************************************************
       VALIDATE-TRANSACTION.
           IF WS-CUST-ID = SPACES
               MOVE '999999' TO WS-CUST-ID
           END-IF.

           EXEC SQL
               INSERT INTO TRANSACTIONS
               VALUES (:WS-CUST-ID, CURRENT_DATE, :WS-BALANCE)
           END-EXEC.

      *****************************************************************
      * SIGNIFICANT: Name says READ but code WRITES and DELETES
      * Score: ~1.05
      *****************************************************************
       READ-ACCOUNT-BALANCE.
           EXEC SQL
               DELETE FROM TEMP_BALANCES
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.

           WRITE AUDIT-RECORD
               FROM WS-CUSTOMER-RECORD
           END-WRITE.

       STOP RUN.
