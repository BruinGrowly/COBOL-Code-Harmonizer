       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIXSQLCS.
      *IBM MAINFRAME - MIXED SQL AND CICS PROCESSING
      *Demonstrates both EXEC SQL and EXEC CICS in one program

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *SQL Communication Area
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      *CICS Communication Area
       01  DFHCOMMAREA.
           05  CA-FUNCTION            PIC X(4).
           05  CA-CUSTOMER-ID         PIC 9(10).
           05  CA-ACCOUNT-NUMBER      PIC 9(12).
           05  CA-AMOUNT              PIC S9(9)V99 COMP-3.
           05  CA-RETURN-CODE         PIC 99.
           05  CA-MESSAGE             PIC X(60).

      *SQL Host Variables
       01  SQL-HOST-VARS.
           05  :HV-CUSTOMER-ID        PIC S9(9) COMP.
           05  :HV-CUSTOMER-NAME      PIC X(50).
           05  :HV-ACCOUNT-NUMBER     PIC S9(11) COMP.
           05  :HV-ACCOUNT-TYPE       PIC X(20).
           05  :HV-CURRENT-BALANCE    PIC S9(13)V99 COMP-3.
           05  :HV-AVAILABLE-CREDIT   PIC S9(13)V99 COMP-3.
           05  :HV-LAST-ACTIVITY      PIC X(26).
           05  :HV-STATUS-CODE        PIC X.

      *Account Transaction Record
       01  TRANSACTION-RECORD.
           05  TRANS-ID               PIC 9(15).
           05  TRANS-ACCOUNT          PIC 9(12).
           05  TRANS-TYPE             PIC X(10).
           05  TRANS-AMOUNT           PIC S9(9)V99 COMP-3.
           05  TRANS-TIMESTAMP        PIC X(26).
           05  TRANS-USER-ID          PIC X(8).
           05  TRANS-TERMINAL         PIC X(4).

      *Working Variables
       01  WS-NEW-BALANCE             PIC S9(13)V99 COMP-3.
       01  WS-HOLD-AMOUNT             PIC S9(13)V99 COMP-3.
       01  WS-TRANSACTION-COUNT       PIC 9(6) VALUE ZERO.
       01  WS-TIMESTAMP               PIC X(26).
       01  WS-RESP                    PIC S9(8) COMP.
       01  WS-SQLCODE-SAVE            PIC S9(9) COMP.

      *CICS File Names
       01  WS-TRANS-FILE              PIC X(8) VALUE 'TRANSACT'.
       01  WS-HOLD-FILE               PIC X(8) VALUE 'HOLDINGS'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      *Initialize CICS error handling
           EXEC CICS HANDLE CONDITION
               ERROR(CICS-ERROR-HANDLER)
               NOTFND(CICS-NOT-FOUND)
           END-EXEC.

      *Check communication area
           IF EIBCALEN = ZERO
               MOVE 'No communication area provided' TO CA-MESSAGE
               MOVE 99 TO CA-RETURN-CODE
               EXEC CICS RETURN END-EXEC
           END-IF.

      *Process based on function code
           EVALUATE CA-FUNCTION
               WHEN 'INQR'
                   PERFORM INQUIRY-PROCESS
               WHEN 'DEPO'
                   PERFORM DEPOSIT-PROCESS
               WHEN 'WDRW'
                   PERFORM WITHDRAWAL-PROCESS
               WHEN 'HOLD'
                   PERFORM HOLD-PROCESS
               WHEN 'HIST'
                   PERFORM HISTORY-PROCESS
               WHEN OTHER
                   MOVE 'Invalid function code' TO CA-MESSAGE
                   MOVE 98 TO CA-RETURN-CODE
           END-EVALUATE.

           EXEC CICS RETURN
               TRANSID(EIBTRNID)
               COMMAREA(DFHCOMMAREA)
               LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.

           GOBACK.

       INQUIRY-PROCESS SECTION.
       DO-INQUIRY.
      *Query account information using SQL
           MOVE CA-CUSTOMER-ID TO :HV-CUSTOMER-ID.
           MOVE CA-ACCOUNT-NUMBER TO :HV-ACCOUNT-NUMBER.

           EXEC SQL
               SELECT CUSTOMER_NAME, ACCOUNT_TYPE,
                      CURRENT_BALANCE, AVAILABLE_CREDIT,
                      LAST_ACTIVITY_DATE, STATUS
               INTO :HV-CUSTOMER-NAME, :HV-ACCOUNT-TYPE,
                    :HV-CURRENT-BALANCE, :HV-AVAILABLE-CREDIT,
                    :HV-LAST-ACTIVITY, :HV-STATUS-CODE
               FROM ACCOUNTS
               WHERE CUSTOMER_ID = :HV-CUSTOMER-ID
                 AND ACCOUNT_NUMBER = :HV-ACCOUNT-NUMBER
           END-EXEC.

           IF SQLCODE = 0
               MOVE :HV-CURRENT-BALANCE TO CA-AMOUNT
               MOVE 'Account inquiry successful' TO CA-MESSAGE
               MOVE 0 TO CA-RETURN-CODE
           ELSE
               IF SQLCODE = 100
                   MOVE 'Account not found' TO CA-MESSAGE
                   MOVE 10 TO CA-RETURN-CODE
               ELSE
                   MOVE 'Database error occurred' TO CA-MESSAGE
                   MOVE 20 TO CA-RETURN-CODE
               END-IF
           END-IF.

       DEPOSIT-PROCESS SECTION.
       DO-DEPOSIT.
      *Start SQL transaction
           EXEC SQL
               BEGIN WORK
           END-EXEC.

      *Read current balance
           MOVE CA-CUSTOMER-ID TO :HV-CUSTOMER-ID.
           MOVE CA-ACCOUNT-NUMBER TO :HV-ACCOUNT-NUMBER.

           EXEC SQL
               SELECT CURRENT_BALANCE, STATUS
               INTO :HV-CURRENT-BALANCE, :HV-STATUS-CODE
               FROM ACCOUNTS
               WHERE CUSTOMER_ID = :HV-CUSTOMER-ID
                 AND ACCOUNT_NUMBER = :HV-ACCOUNT-NUMBER
               FOR UPDATE OF CURRENT_BALANCE
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Cannot access account' TO CA-MESSAGE
               MOVE 21 TO CA-RETURN-CODE
               GO TO DEPOSIT-EXIT
           END-IF.

      *Check account status
           IF :HV-STATUS-CODE NOT = 'A'
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Account not active' TO CA-MESSAGE
               MOVE 22 TO CA-RETURN-CODE
               GO TO DEPOSIT-EXIT
           END-IF.

      *Calculate new balance
           COMPUTE WS-NEW-BALANCE =
               :HV-CURRENT-BALANCE + CA-AMOUNT.

      *Update balance in database
           MOVE WS-NEW-BALANCE TO :HV-CURRENT-BALANCE.

           EXEC SQL
               UPDATE ACCOUNTS
               SET CURRENT_BALANCE = :HV-CURRENT-BALANCE,
                   LAST_ACTIVITY_DATE = CURRENT TIMESTAMP
               WHERE CUSTOMER_ID = :HV-CUSTOMER-ID
                 AND ACCOUNT_NUMBER = :HV-ACCOUNT-NUMBER
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Update failed' TO CA-MESSAGE
               MOVE 23 TO CA-RETURN-CODE
               GO TO DEPOSIT-EXIT
           END-IF.

      *Write transaction log using CICS
           PERFORM BUILD-TRANSACTION-RECORD.
           MOVE 'DEPOSIT' TO TRANS-TYPE.

           EXEC CICS WRITE
               FILE(WS-TRANS-FILE)
               FROM(TRANSACTION-RECORD)
               RIDFLD(TRANS-ID)
               LENGTH(LENGTH OF TRANSACTION-RECORD)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               EXEC SQL COMMIT WORK END-EXEC
               MOVE 'Deposit successful' TO CA-MESSAGE
               MOVE 0 TO CA-RETURN-CODE
           ELSE
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Transaction log error' TO CA-MESSAGE
               MOVE 24 TO CA-RETURN-CODE
           END-IF.

       DEPOSIT-EXIT.
           EXIT.

       WITHDRAWAL-PROCESS SECTION.
       DO-WITHDRAWAL.
      *Similar to deposit but with balance check
           EXEC SQL BEGIN WORK END-EXEC.

           MOVE CA-CUSTOMER-ID TO :HV-CUSTOMER-ID.
           MOVE CA-ACCOUNT-NUMBER TO :HV-ACCOUNT-NUMBER.

           EXEC SQL
               SELECT CURRENT_BALANCE, AVAILABLE_CREDIT, STATUS
               INTO :HV-CURRENT-BALANCE, :HV-AVAILABLE-CREDIT,
                    :HV-STATUS-CODE
               FROM ACCOUNTS
               WHERE CUSTOMER_ID = :HV-CUSTOMER-ID
                 AND ACCOUNT_NUMBER = :HV-ACCOUNT-NUMBER
               FOR UPDATE OF CURRENT_BALANCE
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Cannot access account' TO CA-MESSAGE
               MOVE 31 TO CA-RETURN-CODE
               GO TO WITHDRAWAL-EXIT
           END-IF.

      *Check sufficient funds (balance + credit)
           COMPUTE WS-HOLD-AMOUNT =
               :HV-CURRENT-BALANCE + :HV-AVAILABLE-CREDIT.

           IF CA-AMOUNT > WS-HOLD-AMOUNT
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Insufficient funds' TO CA-MESSAGE
               MOVE 32 TO CA-RETURN-CODE
               GO TO WITHDRAWAL-EXIT
           END-IF.

      *Update balance
           COMPUTE WS-NEW-BALANCE =
               :HV-CURRENT-BALANCE - CA-AMOUNT.
           MOVE WS-NEW-BALANCE TO :HV-CURRENT-BALANCE.

           EXEC SQL
               UPDATE ACCOUNTS
               SET CURRENT_BALANCE = :HV-CURRENT-BALANCE,
                   LAST_ACTIVITY_DATE = CURRENT TIMESTAMP
               WHERE CUSTOMER_ID = :HV-CUSTOMER-ID
                 AND ACCOUNT_NUMBER = :HV-ACCOUNT-NUMBER
           END-EXEC.

           IF SQLCODE = 0
               PERFORM BUILD-TRANSACTION-RECORD
               MOVE 'WITHDRAWAL' TO TRANS-TYPE
               EXEC CICS WRITE
                   FILE(WS-TRANS-FILE)
                   FROM(TRANSACTION-RECORD)
                   RIDFLD(TRANS-ID)
                   LENGTH(LENGTH OF TRANSACTION-RECORD)
                   RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   EXEC SQL COMMIT WORK END-EXEC
                   MOVE 'Withdrawal successful' TO CA-MESSAGE
                   MOVE 0 TO CA-RETURN-CODE
               ELSE
                   EXEC SQL ROLLBACK WORK END-EXEC
                   MOVE 'Transaction log error' TO CA-MESSAGE
                   MOVE 33 TO CA-RETURN-CODE
               END-IF
           ELSE
               EXEC SQL ROLLBACK WORK END-EXEC
               MOVE 'Update failed' TO CA-MESSAGE
               MOVE 34 TO CA-RETURN-CODE
           END-IF.

       WITHDRAWAL-EXIT.
           EXIT.

       HOLD-PROCESS SECTION.
       DO-HOLD.
      *Place hold on funds using CICS file
           PERFORM BUILD-TRANSACTION-RECORD.
           MOVE 'HOLD' TO TRANS-TYPE.

           EXEC CICS WRITE
               FILE(WS-HOLD-FILE)
               FROM(TRANSACTION-RECORD)
               RIDFLD(CA-ACCOUNT-NUMBER)
               LENGTH(LENGTH OF TRANSACTION-RECORD)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Hold placed successfully' TO CA-MESSAGE
               MOVE 0 TO CA-RETURN-CODE
           ELSE
               MOVE 'Hold placement failed' TO CA-MESSAGE
               MOVE 40 TO CA-RETURN-CODE
           END-IF.

       HISTORY-PROCESS SECTION.
       DO-HISTORY.
      *Query transaction history using SQL cursor
           MOVE CA-ACCOUNT-NUMBER TO :HV-ACCOUNT-NUMBER.

           EXEC SQL
               DECLARE TRANS_CURSOR CURSOR FOR
               SELECT TRANSACTION_ID, TRANS_TYPE, TRANS_AMOUNT,
                      TRANS_TIMESTAMP
               FROM TRANSACTIONS
               WHERE ACCOUNT_NUMBER = :HV-ACCOUNT-NUMBER
               ORDER BY TRANS_TIMESTAMP DESC
               FETCH FIRST 10 ROWS ONLY
           END-EXEC.

           EXEC SQL OPEN TRANS_CURSOR END-EXEC.

           IF SQLCODE = 0
               MOVE 'Transaction history retrieved' TO CA-MESSAGE
               MOVE 0 TO CA-RETURN-CODE
           ELSE
               MOVE 'History query failed' TO CA-MESSAGE
               MOVE 50 TO CA-RETURN-CODE
           END-IF.

           EXEC SQL CLOSE TRANS_CURSOR END-EXEC.

       BUILD-TRANSACTION-RECORD SECTION.
       BUILD-TRANS.
      *Generate transaction ID
           ADD 1 TO WS-TRANSACTION-COUNT.
           COMPUTE TRANS-ID =
               FUNCTION CURRENT-DATE(1:8) * 1000000 +
               WS-TRANSACTION-COUNT.

           MOVE CA-ACCOUNT-NUMBER TO TRANS-ACCOUNT.
           MOVE CA-AMOUNT TO TRANS-AMOUNT.
           MOVE EIBTRMID TO TRANS-TERMINAL.
           MOVE EIBUSERID TO TRANS-USER-ID.

           EXEC CICS ASKTIME ABSTIME(WS-TIMESTAMP) END-EXEC.
           MOVE WS-TIMESTAMP TO TRANS-TIMESTAMP.

       CICS-ERROR-HANDLER SECTION.
       CICS-ERROR.
           MOVE 'CICS system error' TO CA-MESSAGE.
           MOVE 90 TO CA-RETURN-CODE.
           EXEC SQL ROLLBACK WORK END-EXEC.
           EXEC CICS RETURN END-EXEC.

       CICS-NOT-FOUND SECTION.
       NOT-FOUND.
           MOVE 'CICS record not found' TO CA-MESSAGE.
           MOVE 91 TO CA-RETURN-CODE.
