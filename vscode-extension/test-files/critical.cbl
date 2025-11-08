       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRITICAL-EXAMPLE.
      *****************************************************************
      * This file demonstrates CRITICAL disharmony (score 1.2+)
      * Names completely contradict implementations - BUGS!
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-RECORD.
           05  WS-ACCT-ID       PIC 9(8).
           05  WS-ACCT-NAME     PIC X(30).
           05  WS-AMOUNT        PIC 9(9)V99.

       PROCEDURE DIVISION.

      *****************************************************************
      * CRITICAL: Name says DISPLAY but code DELETES entire table!
      * Score: ~1.5
      *****************************************************************
       DISPLAY-ACCOUNT-INFO.
           EXEC SQL
               DELETE FROM ACCOUNTS
               WHERE ACCT_ID = :WS-ACCT-ID
           END-EXEC.

      *****************************************************************
      * CRITICAL: Name says READ but code DELETES and UPDATES
      * Score: ~1.35
      *****************************************************************
       READ-TRANSACTION-HISTORY.
           EXEC SQL
               DELETE FROM TRANSACTIONS
               WHERE ACCT_ID = :WS-ACCT-ID
           END-EXEC.

           EXEC SQL
               UPDATE ACCOUNTS
               SET STATUS = 'DELETED'
               WHERE ACCT_ID = :WS-ACCT-ID
           END-EXEC.

      *****************************************************************
      * CRITICAL: Name says GET (retrieve) but code PURGES data!
      * Score: ~1.45
      *****************************************************************
       GET-CUSTOMER-BALANCE.
           DELETE CUSTOMER-FILE RECORD
               INVALID KEY DISPLAY 'Record not found'
           END-DELETE.

      *****************************************************************
      * CRITICAL: Name says VALIDATE but code DESTROYS database!
      * Score: ~1.8
      *****************************************************************
       VALIDATE-INPUT-DATA.
           EXEC SQL
               TRUNCATE TABLE CUSTOMER_DATA
           END-EXEC.

           EXEC SQL
               DROP INDEX IDX_CUSTOMERS
           END-EXEC.

       STOP RUN.
