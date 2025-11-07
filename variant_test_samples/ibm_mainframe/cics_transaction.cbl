       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSTRAN.
      *IBM MAINFRAME - CICS TRANSACTION PROCESSING
      *Demonstrates EXEC CICS blocks for online transactions

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *CICS Communication Areas
       01  DFHCOMMAREA.
           05  CA-TRANS-CODE          PIC X(4).
           05  CA-ACCOUNT-ID          PIC 9(10).
           05  CA-AMOUNT              PIC S9(9)V99 COMP-3.
           05  CA-STATUS              PIC X.
           05  CA-ERROR-MSG           PIC X(50).

      *Account Master Record
       01  ACCOUNT-RECORD.
           05  ACCT-ID                PIC 9(10).
           05  ACCT-NAME              PIC X(50).
           05  ACCT-BALANCE           PIC S9(11)V99 COMP-3.
           05  ACCT-TYPE              PIC X(10).
           05  ACCT-STATUS            PIC X.
           05  ACCT-LAST-TRANS        PIC X(26).

      *Screen Layout Definition
       01  TRANSACTION-SCREEN.
           05  FILLER                 PIC X(20) VALUE
               'Account Transaction'.
           05  FILLER                 PIC X(60) VALUE SPACES.
           05  SCREEN-ACCT-ID         PIC 9(10).
           05  FILLER                 PIC X(10) VALUE 'Balance: $'.
           05  SCREEN-BALANCE         PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(10) VALUE 'Amount: $'.
           05  SCREEN-AMOUNT          PIC ZZZ,ZZZ,ZZ9.99.
           05  SCREEN-MESSAGE         PIC X(60).

      *CICS Control Variables
       01  WS-RESP                    PIC S9(8) COMP.
       01  WS-RESP2                   PIC S9(8) COMP.
       01  WS-EIBTIME                 PIC S9(7) COMP-3.
       01  WS-EIBDATE                 PIC S9(7) COMP-3.
       01  WS-EIBTRNID                PIC X(4).

      *File Control
       01  WS-FILE-NAME               PIC X(8) VALUE 'ACCOUNTS'.
       01  WS-QUEUE-NAME              PIC X(8) VALUE 'TRANLOG'.

      *Transaction Control
       01  WS-NEW-BALANCE             PIC S9(11)V99 COMP-3.
       01  WS-TRANS-TIMESTAMP         PIC X(26).
       01  WS-PROGRAM-NAME            PIC X(8) VALUE 'CICSTRAN'.

       LINKAGE SECTION.
       01  DFHCOMMAREA-LINK.
           05  LINK-DATA              PIC X(100).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      *Check if this is first time through
           EXEC CICS HANDLE CONDITION
               ERROR(GENERAL-ERROR)
               NOTFND(RECORD-NOT-FOUND)
               NOTOPEN(FILE-NOT-OPEN)
           END-EXEC.

           IF EIBCALEN = ZERO
               PERFORM DISPLAY-INITIAL-SCREEN
           ELSE
               PERFORM PROCESS-TRANSACTION-REQUEST
           END-IF.

           EXEC CICS RETURN
           END-EXEC.

           GOBACK.

       DISPLAY-INITIAL-SCREEN SECTION.
       SHOW-SCREEN.
           MOVE SPACES TO TRANSACTION-SCREEN.
           MOVE 'Enter Account ID and Transaction Details'
               TO SCREEN-MESSAGE.

           EXEC CICS SEND
               MAP('TRANMAP')
               MAPSET('TRANSET')
               FROM(TRANSACTION-SCREEN)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID(EIBTRNID)
               COMMAREA(DFHCOMMAREA)
               LENGTH(100)
           END-EXEC.

       PROCESS-TRANSACTION-REQUEST SECTION.
       PROCESS-REQUEST.
      *Receive transaction input
           EXEC CICS RECEIVE
               MAP('TRANMAP')
               MAPSET('TRANSET')
               INTO(TRANSACTION-SCREEN)
           END-EXEC.

      *Move screen data to working storage
           MOVE SCREEN-ACCT-ID TO CA-ACCOUNT-ID.

      *Determine transaction type
           EXEC CICS ASSIGN
               TRANID(WS-EIBTRNID)
           END-EXEC.

           EVALUATE WS-EIBTRNID
               WHEN 'DEPO'
                   MOVE 'D' TO CA-TRANS-CODE
                   PERFORM PROCESS-DEPOSIT
               WHEN 'WDRW'
                   MOVE 'W' TO CA-TRANS-CODE
                   PERFORM PROCESS-WITHDRAWAL
               WHEN 'INQR'
                   MOVE 'I' TO CA-TRANS-CODE
                   PERFORM PROCESS-INQUIRY
               WHEN 'XFER'
                   MOVE 'X' TO CA-TRANS-CODE
                   PERFORM PROCESS-TRANSFER
               WHEN OTHER
                   MOVE 'Invalid transaction type' TO CA-ERROR-MSG
                   PERFORM DISPLAY-ERROR-SCREEN
           END-EVALUATE.

           PERFORM DISPLAY-RESULT-SCREEN.

       PROCESS-DEPOSIT SECTION.
       PROC-DEPOSIT.
           PERFORM READ-ACCOUNT-RECORD.

           IF ACCT-STATUS = 'A'
               COMPUTE WS-NEW-BALANCE =
                   ACCT-BALANCE + CA-AMOUNT
               MOVE WS-NEW-BALANCE TO ACCT-BALANCE
               PERFORM UPDATE-ACCOUNT-RECORD
               PERFORM WRITE-TRANSACTION-LOG
               MOVE 'S' TO CA-STATUS
               MOVE 'Deposit processed successfully' TO CA-ERROR-MSG
           ELSE
               MOVE 'E' TO CA-STATUS
               MOVE 'Account is not active' TO CA-ERROR-MSG
           END-IF.

       PROCESS-WITHDRAWAL SECTION.
       PROC-WITHDRAWAL.
           PERFORM READ-ACCOUNT-RECORD.

           IF ACCT-STATUS = 'A'
               COMPUTE WS-NEW-BALANCE =
                   ACCT-BALANCE - CA-AMOUNT
               IF WS-NEW-BALANCE >= ZERO
                   MOVE WS-NEW-BALANCE TO ACCT-BALANCE
                   PERFORM UPDATE-ACCOUNT-RECORD
                   PERFORM WRITE-TRANSACTION-LOG
                   MOVE 'S' TO CA-STATUS
                   MOVE 'Withdrawal processed' TO CA-ERROR-MSG
               ELSE
                   MOVE 'E' TO CA-STATUS
                   MOVE 'Insufficient funds' TO CA-ERROR-MSG
               END-IF
           ELSE
               MOVE 'E' TO CA-STATUS
               MOVE 'Account is not active' TO CA-ERROR-MSG
           END-IF.

       PROCESS-INQUIRY SECTION.
       PROC-INQUIRY.
           PERFORM READ-ACCOUNT-RECORD.

           MOVE ACCT-BALANCE TO SCREEN-BALANCE.
           MOVE 'S' TO CA-STATUS.
           MOVE 'Balance inquiry completed' TO CA-ERROR-MSG.

       PROCESS-TRANSFER SECTION.
       PROC-TRANSFER.
           PERFORM READ-ACCOUNT-RECORD.

      *Transfer requires calling another program
           EXEC CICS LINK
               PROGRAM('XFERPROC')
               COMMAREA(DFHCOMMAREA)
               LENGTH(100)
           END-EXEC.

           MOVE 'Transfer initiated' TO CA-ERROR-MSG.

       READ-ACCOUNT-RECORD SECTION.
       READ-ACCOUNT.
           EXEC CICS READ
               FILE(WS-FILE-NAME)
               INTO(ACCOUNT-RECORD)
               RIDFLD(CA-ACCOUNT-ID)
               LENGTH(LENGTH OF ACCOUNT-RECORD)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'E' TO CA-STATUS
               MOVE 'Account not found' TO CA-ERROR-MSG
               PERFORM DISPLAY-ERROR-SCREEN
               EXEC CICS RETURN END-EXEC
           END-IF.

       UPDATE-ACCOUNT-RECORD SECTION.
       UPDATE-ACCOUNT.
      *Get current timestamp
           EXEC CICS ASKTIME
               ABSTIME(WS-EIBTIME)
           END-EXEC.

           EXEC CICS FORMATTIME
               ABSTIME(WS-EIBTIME)
               YYDDMM(WS-TRANS-TIMESTAMP)
               TIME(WS-TRANS-TIMESTAMP(9:8))
               DATESEP('/')
           END-EXEC.

           MOVE WS-TRANS-TIMESTAMP TO ACCT-LAST-TRANS.

      *Update account record
           EXEC CICS REWRITE
               FILE(WS-FILE-NAME)
               FROM(ACCOUNT-RECORD)
               LENGTH(LENGTH OF ACCOUNT-RECORD)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'E' TO CA-STATUS
               MOVE 'Error updating account' TO CA-ERROR-MSG
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       WRITE-TRANSACTION-LOG SECTION.
       WRITE-LOG.
      *Write to transient data queue for audit log
           EXEC CICS WRITEQ TD
               QUEUE(WS-QUEUE-NAME)
               FROM(ACCOUNT-RECORD)
               LENGTH(LENGTH OF ACCOUNT-RECORD)
           END-EXEC.

       DISPLAY-RESULT-SCREEN SECTION.
       SHOW-RESULT.
           MOVE CA-ACCOUNT-ID TO SCREEN-ACCT-ID.
           MOVE ACCT-BALANCE TO SCREEN-BALANCE.
           MOVE CA-AMOUNT TO SCREEN-AMOUNT.
           MOVE CA-ERROR-MSG TO SCREEN-MESSAGE.

           EXEC CICS SEND
               MAP('TRANMAP')
               MAPSET('TRANSET')
               FROM(TRANSACTION-SCREEN)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID(EIBTRNID)
               COMMAREA(DFHCOMMAREA)
               LENGTH(100)
           END-EXEC.

       DISPLAY-ERROR-SCREEN SECTION.
       SHOW-ERROR.
           MOVE CA-ERROR-MSG TO SCREEN-MESSAGE.

           EXEC CICS SEND
               MAP('ERRMAP')
               MAPSET('TRANSET')
               FROM(TRANSACTION-SCREEN)
               ERASE
               ALARM
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID(EIBTRNID)
               COMMAREA(DFHCOMMAREA)
               LENGTH(100)
           END-EXEC.

       RECORD-NOT-FOUND SECTION.
       NOT-FOUND.
           MOVE 'E' TO CA-STATUS.
           MOVE 'Account record not found' TO CA-ERROR-MSG.
           PERFORM DISPLAY-ERROR-SCREEN.

       FILE-NOT-OPEN SECTION.
       NOT-OPEN.
           MOVE 'E' TO CA-STATUS.
           MOVE 'Account file not available' TO CA-ERROR-MSG.
           PERFORM DISPLAY-ERROR-SCREEN.

       GENERAL-ERROR SECTION.
       GEN-ERROR.
           MOVE 'E' TO CA-STATUS.
           MOVE 'System error occurred' TO CA-ERROR-MSG.
           PERFORM DISPLAY-ERROR-SCREEN.

           EXEC CICS ABEND
               ABCODE('CTER')
           END-EXEC.
