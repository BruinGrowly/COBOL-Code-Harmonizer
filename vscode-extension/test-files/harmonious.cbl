       IDENTIFICATION DIVISION.
       PROGRAM-ID. HARMONIOUS-EXAMPLE.
      *****************************************************************
      * This file demonstrates HARMONIOUS code (score 0.0-0.3)
      * Names match implementations perfectly
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-RECORD.
           05  WS-CUST-ID       PIC 9(6).
           05  WS-CUST-NAME     PIC X(30).
           05  WS-BALANCE       PIC 9(7)V99.

       01  WS-MONTHLY-TOTAL     PIC 9(9)V99.
       01  WS-DAILY-TOTAL       PIC 9(7)V99.
       01  WS-DAYS              PIC 99 VALUE 30.

       PROCEDURE DIVISION.

      *****************************************************************
      * HARMONIOUS: Name says CALCULATE, code CALCULATES
      *****************************************************************
       CALCULATE-MONTHLY-TOTAL.
           COMPUTE WS-MONTHLY-TOTAL = WS-DAILY-TOTAL * WS-DAYS.

      *****************************************************************
      * HARMONIOUS: Name says READ, code READS
      *****************************************************************
       READ-CUSTOMER-RECORD.
           READ CUSTOMER-FILE
               INTO WS-CUSTOMER-RECORD
               AT END DISPLAY 'EOF REACHED'
           END-READ.

      *****************************************************************
      * HARMONIOUS: Name says DISPLAY, code DISPLAYS
      *****************************************************************
       DISPLAY-CUSTOMER-INFO.
           DISPLAY 'Customer ID: ' WS-CUST-ID.
           DISPLAY 'Customer Name: ' WS-CUST-NAME.
           DISPLAY 'Balance: ' WS-BALANCE.

      *****************************************************************
      * HARMONIOUS: Name says UPDATE, code UPDATES
      *****************************************************************
       UPDATE-CUSTOMER-BALANCE.
           EXEC SQL
               UPDATE CUSTOMERS
               SET BALANCE = :WS-BALANCE
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.

       STOP RUN.
