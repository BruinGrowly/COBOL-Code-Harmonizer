       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMController.
      *FINANCIAL SERVICES - ATM TRANSACTION CONTROLLER
      *Handles cash withdrawals, deposits, and balance inquiries

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'accounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER             PIC 9(10).
           05  ACCT-PIN                PIC 9(4).
           05  ACCT-TYPE               PIC X(10).
           05  ACCT-BALANCE            PIC 9(9)V99 COMP-3.
           05  ACCT-DAILY-LIMIT        PIC 9(5) COMP-3.
           05  ACCT-DAILY-WITHDRAWN    PIC 9(5) COMP-3.
           05  ACCT-STATUS             PIC X.
           05  ACCT-FAILED-ATTEMPTS    PIC 9 VALUE ZERO.

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS              PIC XX.

      *ATM session data
       01  WS-ATM-SESSION.
           05  WS-ATM-ID               PIC X(10) VALUE 'ATM-NYC-01'.
           05  WS-SESSION-ACTIVE       PIC X VALUE 'N'.
           05  WS-LOGGED-IN-ACCT       PIC 9(10).
           05  WS-PIN-ATTEMPTS         PIC 9 VALUE ZERO.

      *Transaction data
       01  WS-TRANSACTION.
           05  WS-TRANS-TYPE           PIC X(15).
           05  WS-TRANS-AMOUNT         PIC 9(7)V99.
           05  WS-TRANS-STATUS         PIC X(10).
           05  WS-TRANS-MESSAGE        PIC X(60).

      *ATM cash inventory
       01  WS-CASH-INVENTORY.
           05  WS-BILLS-100            PIC 9(4) VALUE 100.
           05  WS-BILLS-50             PIC 9(4) VALUE 200.
           05  WS-BILLS-20             PIC 9(4) VALUE 500.
           05  WS-BILLS-10             PIC 9(4) VALUE 300.
           05  WS-BILLS-5              PIC 9(4) VALUE 200.
           05  WS-TOTAL-CASH           PIC 9(7)V99.

      *Dispensed cash
       01  WS-DISPENSED-BILLS.
           05  WS-DISP-100             PIC 9(3) VALUE ZERO.
           05  WS-DISP-50              PIC 9(3) VALUE ZERO.
           05  WS-DISP-20              PIC 9(3) VALUE ZERO.
           05  WS-DISP-10              PIC 9(3) VALUE ZERO.
           05  WS-DISP-5               PIC 9(3) VALUE ZERO.

      *User input
       01  WS-USER-INPUT.
           05  WS-INPUT-ACCOUNT        PIC 9(10).
           05  WS-INPUT-PIN            PIC 9(4).
           05  WS-INPUT-AMOUNT         PIC 9(7)V99.
           05  WS-MENU-CHOICE          PIC 9.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-NEW-BALANCE              PIC 9(9)V99 COMP-3.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ATM-SYSTEM.
           PERFORM SETUP-TEST-ACCOUNTS.
           PERFORM ATM-MAIN-LOOP.
           PERFORM SHUTDOWN-ATM-SYSTEM.
           STOP RUN.

       INITIALIZE-ATM-SYSTEM SECTION.
       INIT-ATM.
           DISPLAY '========================================'.
           DISPLAY 'ATM TRANSACTION CONTROLLER'.
           DISPLAY 'Location: ' WS-ATM-ID.
           DISPLAY '========================================'.
           DISPLAY SPACE.

           OPEN OUTPUT ACCOUNT-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error initializing ATM system'
               STOP RUN
           END-IF.

           PERFORM CALCULATE-CASH-INVENTORY.

           MOVE WS-TOTAL-CASH TO WS-DISPLAY-AMOUNT.
           DISPLAY 'Cash available: ' WS-DISPLAY-AMOUNT.
           DISPLAY SPACE.

       CALCULATE-CASH-INVENTORY SECTION.
       CALC-CASH.
           COMPUTE WS-TOTAL-CASH =
               (WS-BILLS-100 * 100) +
               (WS-BILLS-50 * 50) +
               (WS-BILLS-20 * 20) +
               (WS-BILLS-10 * 10) +
               (WS-BILLS-5 * 5).

       SETUP-TEST-ACCOUNTS SECTION.
       SETUP-ACCTS.
      *Test account 1
           MOVE 1234567890 TO ACCT-NUMBER.
           MOVE 1234 TO ACCT-PIN.
           MOVE 'CHECKING' TO ACCT-TYPE.
           MOVE 5000.00 TO ACCT-BALANCE.
           MOVE 500.00 TO ACCT-DAILY-LIMIT.
           MOVE 0.00 TO ACCT-DAILY-WITHDRAWN.
           MOVE 'A' TO ACCT-STATUS.
           WRITE ACCOUNT-RECORD.

      *Test account 2
           MOVE 9876543210 TO ACCT-NUMBER.
           MOVE 5678 TO ACCT-PIN.
           MOVE 'SAVINGS' TO ACCT-TYPE.
           MOVE 15000.00 TO ACCT-BALANCE.
           MOVE 1000.00 TO ACCT-DAILY-LIMIT.
           MOVE 0.00 TO ACCT-DAILY-WITHDRAWN.
           MOVE 'A' TO ACCT-STATUS.
           WRITE ACCOUNT-RECORD.

           CLOSE ACCOUNT-FILE.
           OPEN I-O ACCOUNT-FILE.

       ATM-MAIN-LOOP SECTION.
       ATM-LOOP.
           DISPLAY 'Simulating ATM transactions...'.
           DISPLAY '--------------------------------------------'.

      *Session 1: Successful withdrawal
           MOVE 1234567890 TO WS-INPUT-ACCOUNT.
           MOVE 1234 TO WS-INPUT-PIN.
           PERFORM AUTHENTICATE-USER.

           IF WS-SESSION-ACTIVE = 'Y'
               MOVE 200.00 TO WS-INPUT-AMOUNT
               PERFORM PROCESS-WITHDRAWAL
               PERFORM END-SESSION
           END-IF.

      *Session 2: Balance inquiry
           MOVE 9876543210 TO WS-INPUT-ACCOUNT.
           MOVE 5678 TO WS-INPUT-PIN.
           PERFORM AUTHENTICATE-USER.

           IF WS-SESSION-ACTIVE = 'Y'
               PERFORM PROCESS-BALANCE-INQUIRY
               PERFORM END-SESSION
           END-IF.

      *Session 3: Deposit
           MOVE 1234567890 TO WS-INPUT-ACCOUNT.
           MOVE 1234 TO WS-INPUT-PIN.
           PERFORM AUTHENTICATE-USER.

           IF WS-SESSION-ACTIVE = 'Y'
               MOVE 500.00 TO WS-INPUT-AMOUNT
               PERFORM PROCESS-DEPOSIT
               PERFORM END-SESSION
           END-IF.

      *Session 4: Over daily limit
           MOVE 1234567890 TO WS-INPUT-ACCOUNT.
           MOVE 1234 TO WS-INPUT-PIN.
           PERFORM AUTHENTICATE-USER.

           IF WS-SESSION-ACTIVE = 'Y'
               MOVE 400.00 TO WS-INPUT-AMOUNT
               PERFORM PROCESS-WITHDRAWAL
               PERFORM END-SESSION
           END-IF.

      *Session 5: Wrong PIN
           MOVE 9876543210 TO WS-INPUT-ACCOUNT.
           MOVE 0000 TO WS-INPUT-PIN.
           PERFORM AUTHENTICATE-USER.

           DISPLAY SPACE.

       AUTHENTICATE-USER SECTION.
       AUTH-USER.
           DISPLAY 'Authentication attempt...'.

           MOVE WS-INPUT-ACCOUNT TO ACCT-NUMBER.
           READ ACCOUNT-FILE KEY IS ACCT-NUMBER
               INVALID KEY
                   MOVE 'FAILED' TO WS-TRANS-STATUS
                   MOVE 'ACCOUNT NOT FOUND' TO WS-TRANS-MESSAGE
                   MOVE 'N' TO WS-SESSION-ACTIVE
                   PERFORM DISPLAY-TRANSACTION-RESULT
           END-READ.

           IF WS-FILE-STATUS = '00'
               PERFORM VERIFY-PIN
           END-IF.

       VERIFY-PIN SECTION.
       VERIFY.
           IF ACCT-STATUS NOT = 'A'
               MOVE 'FAILED' TO WS-TRANS-STATUS
               MOVE 'ACCOUNT INACTIVE' TO WS-TRANS-MESSAGE
               MOVE 'N' TO WS-SESSION-ACTIVE
               PERFORM DISPLAY-TRANSACTION-RESULT
               GO TO VERIFY-EXIT
           END-IF.

           IF WS-INPUT-PIN NOT = ACCT-PIN
               ADD 1 TO ACCT-FAILED-ATTEMPTS
               MOVE 'FAILED' TO WS-TRANS-STATUS
               MOVE 'INCORRECT PIN' TO WS-TRANS-MESSAGE
               MOVE 'N' TO WS-SESSION-ACTIVE

               IF ACCT-FAILED-ATTEMPTS >= 3
                   MOVE 'I' TO ACCT-STATUS
                   MOVE 'ACCOUNT LOCKED' TO WS-TRANS-MESSAGE
                   REWRITE ACCOUNT-RECORD
               END-IF

               PERFORM DISPLAY-TRANSACTION-RESULT
           ELSE
               MOVE ZERO TO ACCT-FAILED-ATTEMPTS
               MOVE 'Y' TO WS-SESSION-ACTIVE
               MOVE WS-INPUT-ACCOUNT TO WS-LOGGED-IN-ACCT
               DISPLAY '  Authentication successful'
               DISPLAY '  Account: ' ACCT-NUMBER
               DISPLAY SPACE
           END-IF.

       VERIFY-EXIT.
           EXIT.

       PROCESS-WITHDRAWAL SECTION.
       PROC-WITHDRAWAL.
           MOVE 'WITHDRAWAL' TO WS-TRANS-TYPE.
           DISPLAY 'Processing withdrawal...'.
           MOVE WS-INPUT-AMOUNT TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Amount requested: ' WS-DISPLAY-AMOUNT.

      *Check daily limit
           COMPUTE WS-NEW-BALANCE =
               ACCT-DAILY-WITHDRAWN + WS-INPUT-AMOUNT.

           IF WS-NEW-BALANCE > ACCT-DAILY-LIMIT
               MOVE 'DECLINED' TO WS-TRANS-STATUS
               MOVE 'EXCEEDS DAILY LIMIT' TO WS-TRANS-MESSAGE
               PERFORM DISPLAY-TRANSACTION-RESULT
               GO TO WITHDRAWAL-EXIT
           END-IF.

      *Check account balance
           IF WS-INPUT-AMOUNT > ACCT-BALANCE
               MOVE 'DECLINED' TO WS-TRANS-STATUS
               MOVE 'INSUFFICIENT FUNDS' TO WS-TRANS-MESSAGE
               PERFORM DISPLAY-TRANSACTION-RESULT
               GO TO WITHDRAWAL-EXIT
           END-IF.

      *Check ATM cash availability
           IF WS-INPUT-AMOUNT > WS-TOTAL-CASH
               MOVE 'DECLINED' TO WS-TRANS-STATUS
               MOVE 'ATM OUT OF CASH' TO WS-TRANS-MESSAGE
               PERFORM DISPLAY-TRANSACTION-RESULT
               GO TO WITHDRAWAL-EXIT
           END-IF.

      *Process withdrawal
           PERFORM DISPENSE-CASH.

           IF WS-TRANS-STATUS = 'SUCCESS'
               COMPUTE ACCT-BALANCE = ACCT-BALANCE - WS-INPUT-AMOUNT
               COMPUTE ACCT-DAILY-WITHDRAWN =
                   ACCT-DAILY-WITHDRAWN + WS-INPUT-AMOUNT
               REWRITE ACCOUNT-RECORD
               PERFORM CALCULATE-CASH-INVENTORY
           END-IF.

           PERFORM DISPLAY-TRANSACTION-RESULT.

       WITHDRAWAL-EXIT.
           EXIT.

       DISPENSE-CASH SECTION.
       DISPENSE.
           MOVE ZERO TO WS-DISPENSED-BILLS.

      *Simplified cash dispensing algorithm
           IF WS-INPUT-AMOUNT = 200.00
               MOVE 2 TO WS-DISP-100
               SUBTRACT 2 FROM WS-BILLS-100
               MOVE 'SUCCESS' TO WS-TRANS-STATUS
               MOVE 'CASH DISPENSED' TO WS-TRANS-MESSAGE
           ELSE
               IF WS-INPUT-AMOUNT = 400.00
                   MOVE 4 TO WS-DISP-100
                   SUBTRACT 4 FROM WS-BILLS-100
                   MOVE 'SUCCESS' TO WS-TRANS-STATUS
                   MOVE 'CASH DISPENSED' TO WS-TRANS-MESSAGE
               ELSE
                   MOVE 'DECLINED' TO WS-TRANS-STATUS
                   MOVE 'INVALID AMOUNT' TO WS-TRANS-MESSAGE
               END-IF
           END-IF.

           IF WS-TRANS-STATUS = 'SUCCESS'
               DISPLAY '  Dispensing:'
               IF WS-DISP-100 > ZERO
                   DISPLAY '    $100 bills: ' WS-DISP-100
               END-IF
               IF WS-DISP-50 > ZERO
                   DISPLAY '    $50 bills: ' WS-DISP-50
               END-IF
               IF WS-DISP-20 > ZERO
                   DISPLAY '    $20 bills: ' WS-DISP-20
               END-IF
           END-IF.

       PROCESS-DEPOSIT SECTION.
       PROC-DEPOSIT.
           MOVE 'DEPOSIT' TO WS-TRANS-TYPE.
           DISPLAY 'Processing deposit...'.
           MOVE WS-INPUT-AMOUNT TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Amount: ' WS-DISPLAY-AMOUNT.

           COMPUTE ACCT-BALANCE = ACCT-BALANCE + WS-INPUT-AMOUNT.
           REWRITE ACCOUNT-RECORD.

           MOVE 'SUCCESS' TO WS-TRANS-STATUS.
           MOVE 'DEPOSIT ACCEPTED' TO WS-TRANS-MESSAGE.
           PERFORM DISPLAY-TRANSACTION-RESULT.

       PROCESS-BALANCE-INQUIRY SECTION.
       PROC-INQUIRY.
           MOVE 'BALANCE' TO WS-TRANS-TYPE.
           DISPLAY 'Balance inquiry...'.

           MOVE ACCT-BALANCE TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Current balance: ' WS-DISPLAY-AMOUNT.
           DISPLAY '  Account type: ' ACCT-TYPE.

           MOVE 'SUCCESS' TO WS-TRANS-STATUS.
           MOVE 'INQUIRY COMPLETE' TO WS-TRANS-MESSAGE.
           DISPLAY SPACE.

       DISPLAY-TRANSACTION-RESULT SECTION.
       SHOW-RESULT.
           DISPLAY '  Status: ' WS-TRANS-STATUS.
           DISPLAY '  ' WS-TRANS-MESSAGE.
           DISPLAY SPACE.

       END-SESSION SECTION.
       END-SESS.
           MOVE 'N' TO WS-SESSION-ACTIVE.
           MOVE ZERO TO WS-LOGGED-IN-ACCT.
           DISPLAY 'Session ended'.
           DISPLAY '--------------------------------------------'.
           DISPLAY SPACE.

       SHUTDOWN-ATM-SYSTEM SECTION.
       SHUTDOWN.
           CLOSE ACCOUNT-FILE.
           DISPLAY 'ATM system shutdown complete.'.
