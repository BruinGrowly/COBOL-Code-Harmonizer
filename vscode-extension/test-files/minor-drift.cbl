       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINOR-DRIFT-EXAMPLE.
      *****************************************************************
      * This file demonstrates MINOR DRIFT (score 0.3-0.5)
      * Names mostly match, but slight semantic mismatch
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-RECORD.
           05  WS-CUST-ID       PIC 9(6).
           05  WS-CUST-NAME     PIC X(30).
           05  WS-STATUS        PIC X(10).

       01  WS-RESULT            PIC X(20).

       PROCEDURE DIVISION.

      *****************************************************************
      * MINOR DRIFT: Name says VALIDATE but also READS
      * Score: ~0.4
      *****************************************************************
       VALIDATE-CUSTOMER.
           EXEC SQL
               SELECT STATUS INTO :WS-STATUS
               FROM CUSTOMERS
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.

           IF WS-STATUS NOT = 'ACTIVE'
               MOVE 'INVALID' TO WS-RESULT
           END-IF.

      *****************************************************************
      * MINOR DRIFT: Name says CHECK but also DISPLAYS
      * Score: ~0.35
      *****************************************************************
       CHECK-ACCOUNT-STATUS.
           EVALUATE WS-STATUS
               WHEN 'ACTIVE'
                   DISPLAY 'Account is active'
               WHEN 'SUSPENDED'
                   DISPLAY 'Account suspended'
               WHEN OTHER
                   DISPLAY 'Unknown status'
           END-EVALUATE.

       STOP RUN.
