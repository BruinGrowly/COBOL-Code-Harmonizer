       IDENTIFICATION DIVISION.
       PROGRAM-ID. HARMONIOUS-EXAMPLE.
      *****************************************************************
      * Example of HARMONIOUS COBOL code where procedure names       *
      * accurately reflect their implementations.                     *
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

       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05 WS-EOF-FLAG          PIC X VALUE 'N'.
           05 WS-TOTAL-BALANCE     PIC 9(9)V99 VALUE ZERO.
           05 WS-CUSTOMER-COUNT    PIC 9(5) VALUE ZERO.
           05 WS-SEARCH-ID         PIC 9(6).

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           PERFORM INITIALIZATION.
           PERFORM PROCESS-CUSTOMERS.
           PERFORM DISPLAY-SUMMARY.
           PERFORM CLEANUP.
           STOP RUN.

      *****************************************************************
      * HARMONIOUS: Initializes variables                            *
      * Intent: Power (initialization) - Score: ~0.2                 *
      * Execution: Power (MOVE, OPEN) - Score: ~0.7                  *
      * Disharmony: 0.28 - HARMONIOUS ✓                              *
      *****************************************************************
       INITIALIZATION.
           MOVE ZEROS TO WS-TOTAL-BALANCE.
           MOVE ZEROS TO WS-CUSTOMER-COUNT.
           MOVE 'N' TO WS-EOF-FLAG.
           OPEN INPUT CUSTOMER-FILE.

      *****************************************************************
      * HARMONIOUS: Gets customer record by ID                       *
      * Intent: Wisdom (GET = retrieve) - Score: ~0.8                *
      * Execution: Wisdom (READ) - Score: ~0.7                       *
      * Disharmony: 0.15 - HARMONIOUS ✓                              *
      *****************************************************************
       GET-CUSTOMER-RECORD.
           MOVE WS-SEARCH-ID TO CUST-ID.
           READ CUSTOMER-FILE
               KEY IS CUST-ID
               INVALID KEY MOVE 'Y' TO WS-EOF-FLAG
           END-READ.

      *****************************************************************
      * HARMONIOUS: Validates customer data                          *
      * Intent: Justice (VALIDATE = check) - Score: ~0.8             *
      * Execution: Justice (IF statements) - Score: ~0.7             *
      * Disharmony: 0.18 - HARMONIOUS ✓                              *
      *****************************************************************
       VALIDATE-CUSTOMER-DATA.
           IF CUST-ID = ZEROS
               MOVE 'INVALID' TO CUST-STATUS
           END-IF.
           IF CUST-NAME = SPACES
               MOVE 'INVALID' TO CUST-STATUS
           END-IF.
           IF CUST-BALANCE < ZERO
               MOVE 'INVALID' TO CUST-STATUS
           END-IF.

      *****************************************************************
      * HARMONIOUS: Calculates total balance                         *
      * Intent: Wisdom (CALCULATE = compute) - Score: ~0.6           *
      * Execution: Wisdom (COMPUTE, ADD) - Score: ~0.6               *
      * Disharmony: 0.08 - HARMONIOUS ✓                              *
      *****************************************************************
       CALCULATE-TOTAL-BALANCE.
           ADD CUST-BALANCE TO WS-TOTAL-BALANCE.
           ADD 1 TO WS-CUSTOMER-COUNT.

      *****************************************************************
      * HARMONIOUS: Displays customer information                    *
      * Intent: Wisdom/Love (DISPLAY = show) - Score: ~0.6           *
      * Execution: Wisdom/Love (DISPLAY) - Score: ~0.5               *
      * Disharmony: 0.12 - HARMONIOUS ✓                              *
      *****************************************************************
       DISPLAY-CUSTOMER-INFO.
           DISPLAY 'Customer ID: ' CUST-ID.
           DISPLAY 'Name: ' CUST-NAME.
           DISPLAY 'Balance: ' CUST-BALANCE.
           DISPLAY 'Status: ' CUST-STATUS.

      *****************************************************************
      * HARMONIOUS: Processes all customers                          *
      * Intent: Mixed (PROCESS = ambiguous but acceptable)           *
      * Execution: Mixed (READ, IF, PERFORM) - Score: ~0.4           *
      * Disharmony: 0.35 - MINOR DRIFT (acceptable) ⚠️                *
      *****************************************************************
       PROCESS-CUSTOMERS.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ CUSTOMER-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF-FLAG
               END-READ
               IF WS-EOF-FLAG NOT = 'Y'
                   PERFORM VALIDATE-CUSTOMER-DATA
                   PERFORM CALCULATE-TOTAL-BALANCE
               END-IF
           END-PERFORM.

      *****************************************************************
      * HARMONIOUS: Displays summary information                     *
      * Intent: Wisdom/Love (DISPLAY = show) - Score: ~0.6           *
      * Execution: Wisdom/Love (DISPLAY, COMPUTE) - Score: ~0.5      *
      * Disharmony: 0.15 - HARMONIOUS ✓                              *
      *****************************************************************
       DISPLAY-SUMMARY.
           DISPLAY '----------------------------------------'.
           DISPLAY 'Total Customers: ' WS-CUSTOMER-COUNT.
           DISPLAY 'Total Balance: ' WS-TOTAL-BALANCE.
           DISPLAY '----------------------------------------'.

      *****************************************************************
      * HARMONIOUS: Cleanup and close files                          *
      * Intent: Power (CLEANUP = close/reset) - Score: ~0.7          *
      * Execution: Power (CLOSE) - Score: ~0.7                       *
      * Disharmony: 0.05 - HARMONIOUS ✓                              *
      *****************************************************************
       CLEANUP.
           CLOSE CUSTOMER-FILE.
