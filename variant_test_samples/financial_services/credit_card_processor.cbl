       IDENTIFICATION DIVISION.
       PROGRAM-ID. CreditCardProcessor.
      *FINANCIAL SERVICES - CREDIT CARD TRANSACTION PROCESSING
      *Real-time authorization and fraud detection

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILE ASSIGN TO 'cards.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CARD-NUMBER
               ALTERNATE RECORD KEY IS CARD-CUSTOMER-ID
               FILE STATUS IS WS-CARD-FILE-STATUS.

           SELECT TRANSACTION-LOG ASSIGN TO 'cc_trans.log'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE.
       01  CARD-RECORD.
           05  CARD-NUMBER             PIC 9(16).
           05  CARD-CUSTOMER-ID        PIC 9(10).
           05  CARD-TYPE               PIC X(15).
           05  CARD-CREDIT-LIMIT       PIC 9(7)V99 COMP-3.
           05  CARD-CURRENT-BALANCE    PIC 9(7)V99 COMP-3.
           05  CARD-AVAILABLE-CREDIT   PIC 9(7)V99 COMP-3.
           05  CARD-STATUS             PIC X.
           05  CARD-EXPIRY-DATE        PIC 9(6).
           05  CARD-CVV                PIC 9(3).
           05  CARD-LAST-TRANS-DATE    PIC X(10).

       FD  TRANSACTION-LOG.
       01  LOG-RECORD                  PIC X(200).

       WORKING-STORAGE SECTION.

       01  WS-CARD-FILE-STATUS         PIC XX.
       01  WS-LOG-FILE-STATUS          PIC XX.

      *Transaction request
       01  WS-TRANSACTION-REQUEST.
           05  WS-REQ-CARD-NUMBER      PIC 9(16).
           05  WS-REQ-AMOUNT           PIC 9(7)V99.
           05  WS-REQ-MERCHANT-ID      PIC X(15).
           05  WS-REQ-MERCHANT-NAME    PIC X(40).
           05  WS-REQ-MERCHANT-CATEGORY PIC X(20).
           05  WS-REQ-LOCATION         PIC X(30).
           05  WS-REQ-CURRENCY         PIC XXX.
           05  WS-REQ-CVV              PIC 9(3).

      *Transaction response
       01  WS-TRANSACTION-RESPONSE.
           05  WS-RESP-AUTH-CODE       PIC X(6).
           05  WS-RESP-STATUS          PIC X(20).
           05  WS-RESP-DECLINE-REASON  PIC X(50).
           05  WS-RESP-TIMESTAMP       PIC X(26).
           05  WS-RESP-AVAILABLE-CREDIT PIC 9(7)V99.

      *Fraud detection
       01  WS-FRAUD-INDICATORS.
           05  WS-LARGE-AMOUNT-FLAG    PIC X VALUE 'N'.
           05  WS-FOREIGN-TRANS-FLAG   PIC X VALUE 'N'.
           05  WS-RAPID-TRANS-FLAG     PIC X VALUE 'N'.
           05  WS-UNUSUAL-MERCHANT-FLAG PIC X VALUE 'N'.
           05  WS-FRAUD-SCORE          PIC 9(3) VALUE ZERO.

      *Processing variables
       01  WS-NEW-BALANCE              PIC 9(7)V99 COMP-3.
       01  WS-TRANS-COUNT              PIC 9(6) VALUE ZERO.
       01  WS-AUTH-CODE-COUNTER        PIC 9(6) VALUE 100000.
       01  WS-CURRENT-DATE-TIME        PIC X(26).

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-CARD             PIC 9999-9999-9999-9999.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SYSTEM.
           PERFORM SETUP-TEST-CARDS.
           PERFORM PROCESS-TRANSACTION-BATCH.
           PERFORM DISPLAY-PROCESSING-SUMMARY.
           PERFORM CLEANUP-SYSTEM.
           STOP RUN.

       INITIALIZE-SYSTEM SECTION.
       INIT-SYS.
           DISPLAY '========================================'.
           DISPLAY 'Credit Card Processing System'.
           DISPLAY 'Real-Time Authorization Engine'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

           OPEN OUTPUT CARD-FILE.
           OPEN OUTPUT TRANSACTION-LOG.

           IF WS-CARD-FILE-STATUS NOT = '00' OR
              WS-LOG-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening files'
               STOP RUN
           END-IF.

           DISPLAY 'System initialized successfully'.
           DISPLAY SPACE.

       SETUP-TEST-CARDS SECTION.
       SETUP-CARDS.
           DISPLAY 'Setting up test credit cards...'.

      *Card 1 - Good standing
           MOVE 4532123456789012 TO CARD-NUMBER.
           MOVE 1000000001 TO CARD-CUSTOMER-ID.
           MOVE 'VISA PLATINUM' TO CARD-TYPE.
           MOVE 10000.00 TO CARD-CREDIT-LIMIT.
           MOVE 2350.50 TO CARD-CURRENT-BALANCE.
           COMPUTE CARD-AVAILABLE-CREDIT =
               CARD-CREDIT-LIMIT - CARD-CURRENT-BALANCE.
           MOVE 'A' TO CARD-STATUS.
           MOVE 202612 TO CARD-EXPIRY-DATE.
           MOVE 123 TO CARD-CVV.
           WRITE CARD-RECORD.

      *Card 2 - Near limit
           MOVE 5424987654321098 TO CARD-NUMBER.
           MOVE 1000000002 TO CARD-CUSTOMER-ID.
           MOVE 'MASTERCARD GOLD' TO CARD-TYPE.
           MOVE 5000.00 TO CARD-CREDIT-LIMIT.
           MOVE 4850.00 TO CARD-CURRENT-BALANCE.
           COMPUTE CARD-AVAILABLE-CREDIT =
               CARD-CREDIT-LIMIT - CARD-CURRENT-BALANCE.
           MOVE 'A' TO CARD-STATUS.
           MOVE 202509 TO CARD-EXPIRY-DATE.
           MOVE 456 TO CARD-CVV.
           WRITE CARD-RECORD.

      *Card 3 - Inactive
           MOVE 3782123456789012 TO CARD-NUMBER.
           MOVE 1000000003 TO CARD-CUSTOMER-ID.
           MOVE 'AMEX CORPORATE' TO CARD-TYPE.
           MOVE 25000.00 TO CARD-CREDIT-LIMIT.
           MOVE 0.00 TO CARD-CURRENT-BALANCE.
           MOVE 25000.00 TO CARD-AVAILABLE-CREDIT.
           MOVE 'I' TO CARD-STATUS.
           MOVE 202503 TO CARD-EXPIRY-DATE.
           MOVE 789 TO CARD-CVV.
           WRITE CARD-RECORD.

           DISPLAY 'Test cards created'.
           DISPLAY SPACE.

           CLOSE CARD-FILE.
           OPEN I-O CARD-FILE.

       PROCESS-TRANSACTION-BATCH SECTION.
       PROC-BATCH.
           DISPLAY 'Processing transaction batch...'.
           DISPLAY '--------------------------------------------'.

      *Transaction 1 - Normal purchase
           MOVE 4532123456789012 TO WS-REQ-CARD-NUMBER.
           MOVE 89.99 TO WS-REQ-AMOUNT.
           MOVE 'MERCH001' TO WS-REQ-MERCHANT-ID.
           MOVE 'Amazon.com' TO WS-REQ-MERCHANT-NAME.
           MOVE 'RETAIL' TO WS-REQ-MERCHANT-CATEGORY.
           MOVE 'New York, NY' TO WS-REQ-LOCATION.
           MOVE 'USD' TO WS-REQ-CURRENCY.
           MOVE 123 TO WS-REQ-CVV.
           PERFORM AUTHORIZE-TRANSACTION.

      *Transaction 2 - Large purchase
           MOVE 4532123456789012 TO WS-REQ-CARD-NUMBER.
           MOVE 2500.00 TO WS-REQ-AMOUNT.
           MOVE 'MERCH002' TO WS-REQ-MERCHANT-ID.
           MOVE 'Best Buy' TO WS-REQ-MERCHANT-NAME.
           MOVE 'ELECTRONICS' TO WS-REQ-MERCHANT-CATEGORY.
           MOVE 'Los Angeles, CA' TO WS-REQ-LOCATION.
           MOVE 'USD' TO WS-REQ-CURRENCY.
           MOVE 123 TO WS-REQ-CVV.
           PERFORM AUTHORIZE-TRANSACTION.

      *Transaction 3 - Over limit
           MOVE 5424987654321098 TO WS-REQ-CARD-NUMBER.
           MOVE 500.00 TO WS-REQ-AMOUNT.
           MOVE 'MERCH003' TO WS-REQ-MERCHANT-ID.
           MOVE 'Shell Gas Station' TO WS-REQ-MERCHANT-NAME.
           MOVE 'FUEL' TO WS-REQ-MERCHANT-CATEGORY.
           MOVE 'Dallas, TX' TO WS-REQ-LOCATION.
           MOVE 'USD' TO WS-REQ-CURRENCY.
           MOVE 456 TO WS-REQ-CVV.
           PERFORM AUTHORIZE-TRANSACTION.

      *Transaction 4 - Inactive card
           MOVE 3782123456789012 TO WS-REQ-CARD-NUMBER.
           MOVE 150.00 TO WS-REQ-AMOUNT.
           MOVE 'MERCH004' TO WS-REQ-MERCHANT-ID.
           MOVE 'Starbucks' TO WS-REQ-MERCHANT-NAME.
           MOVE 'RESTAURANT' TO WS-REQ-MERCHANT-CATEGORY.
           MOVE 'Seattle, WA' TO WS-REQ-LOCATION.
           MOVE 'USD' TO WS-REQ-CURRENCY.
           MOVE 789 TO WS-REQ-CVV.
           PERFORM AUTHORIZE-TRANSACTION.

      *Transaction 5 - Foreign transaction
           MOVE 4532123456789012 TO WS-REQ-CARD-NUMBER.
           MOVE 250.00 TO WS-REQ-AMOUNT.
           MOVE 'MERCH005' TO WS-REQ-MERCHANT-ID.
           MOVE 'London Hotel' TO WS-REQ-MERCHANT-NAME.
           MOVE 'LODGING' TO WS-REQ-MERCHANT-CATEGORY.
           MOVE 'London, UK' TO WS-REQ-LOCATION.
           MOVE 'GBP' TO WS-REQ-CURRENCY.
           MOVE 123 TO WS-REQ-CVV.
           PERFORM AUTHORIZE-TRANSACTION.

           DISPLAY SPACE.

       AUTHORIZE-TRANSACTION SECTION.
       AUTH-TRANS.
           ADD 1 TO WS-TRANS-COUNT.

           DISPLAY 'Transaction #' WS-TRANS-COUNT ':'.
           MOVE WS-REQ-AMOUNT TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Amount: ' WS-DISPLAY-AMOUNT
                   ' at ' WS-REQ-MERCHANT-NAME.

           PERFORM READ-CARD-RECORD.

           IF WS-CARD-FILE-STATUS = '00'
               PERFORM VALIDATE-CARD
               IF WS-RESP-STATUS = 'VALIDATED'
                   PERFORM CHECK-FRAUD-INDICATORS
                   IF WS-FRAUD-SCORE < 50
                       PERFORM CHECK-CREDIT-AVAILABILITY
                       IF WS-RESP-STATUS = 'APPROVED'
                           PERFORM UPDATE-CARD-BALANCE
                       END-IF
                   ELSE
                       MOVE 'DECLINED' TO WS-RESP-STATUS
                       MOVE 'FRAUD DETECTED' TO
                           WS-RESP-DECLINE-REASON
                   END-IF
               END-IF
           ELSE
               MOVE 'DECLINED' TO WS-RESP-STATUS
               MOVE 'CARD NOT FOUND' TO WS-RESP-DECLINE-REASON
           END-IF.

           PERFORM LOG-TRANSACTION.
           PERFORM DISPLAY-AUTHORIZATION-RESULT.
           DISPLAY SPACE.

       READ-CARD-RECORD SECTION.
       READ-CARD.
           MOVE WS-REQ-CARD-NUMBER TO CARD-NUMBER.
           READ CARD-FILE KEY IS CARD-NUMBER
               INVALID KEY
                   MOVE '23' TO WS-CARD-FILE-STATUS
           END-READ.

       VALIDATE-CARD SECTION.
       VALIDATE.
           IF CARD-STATUS NOT = 'A'
               MOVE 'DECLINED' TO WS-RESP-STATUS
               MOVE 'CARD INACTIVE' TO WS-RESP-DECLINE-REASON
           ELSE
               IF CARD-EXPIRY-DATE < 202511
                   MOVE 'DECLINED' TO WS-RESP-STATUS
                   MOVE 'CARD EXPIRED' TO WS-RESP-DECLINE-REASON
               ELSE
                   IF WS-REQ-CVV NOT = CARD-CVV
                       MOVE 'DECLINED' TO WS-RESP-STATUS
                       MOVE 'INVALID CVV' TO WS-RESP-DECLINE-REASON
                   ELSE
                       MOVE 'VALIDATED' TO WS-RESP-STATUS
                   END-IF
               END-IF
           END-IF.

       CHECK-FRAUD-INDICATORS SECTION.
       CHECK-FRAUD.
           MOVE ZERO TO WS-FRAUD-SCORE.

      *Check for large transaction
           IF WS-REQ-AMOUNT > 2000.00
               MOVE 'Y' TO WS-LARGE-AMOUNT-FLAG
               ADD 25 TO WS-FRAUD-SCORE
               DISPLAY '  ALERT: Large transaction detected'
           END-IF.

      *Check for foreign transaction
           IF WS-REQ-CURRENCY NOT = 'USD'
               MOVE 'Y' TO WS-FOREIGN-TRANS-FLAG
               ADD 15 TO WS-FRAUD-SCORE
               DISPLAY '  ALERT: Foreign transaction detected'
           END-IF.

      *Check unusual merchant category
           IF WS-REQ-MERCHANT-CATEGORY = 'GAMBLING' OR
              WS-REQ-MERCHANT-CATEGORY = 'WIRE TRANSFER'
               MOVE 'Y' TO WS-UNUSUAL-MERCHANT-FLAG
               ADD 30 TO WS-FRAUD-SCORE
               DISPLAY '  ALERT: High-risk merchant category'
           END-IF.

           DISPLAY '  Fraud Score: ' WS-FRAUD-SCORE.

       CHECK-CREDIT-AVAILABILITY SECTION.
       CHECK-CREDIT.
           COMPUTE WS-NEW-BALANCE =
               CARD-CURRENT-BALANCE + WS-REQ-AMOUNT.

           IF WS-NEW-BALANCE > CARD-CREDIT-LIMIT
               MOVE 'DECLINED' TO WS-RESP-STATUS
               MOVE 'INSUFFICIENT CREDIT' TO WS-RESP-DECLINE-REASON
               MOVE CARD-AVAILABLE-CREDIT TO
                   WS-RESP-AVAILABLE-CREDIT
           ELSE
               MOVE 'APPROVED' TO WS-RESP-STATUS
               ADD 1 TO WS-AUTH-CODE-COUNTER
               MOVE WS-AUTH-CODE-COUNTER TO WS-RESP-AUTH-CODE
           END-IF.

       UPDATE-CARD-BALANCE SECTION.
       UPDATE-BALANCE.
           MOVE WS-NEW-BALANCE TO CARD-CURRENT-BALANCE.
           COMPUTE CARD-AVAILABLE-CREDIT =
               CARD-CREDIT-LIMIT - CARD-CURRENT-BALANCE.

           MOVE FUNCTION CURRENT-DATE TO CARD-LAST-TRANS-DATE.

           REWRITE CARD-RECORD
               INVALID KEY
                   DISPLAY '  Error updating card balance'
           END-REWRITE.

       LOG-TRANSACTION SECTION.
       LOG-TRANS.
           MOVE FUNCTION CURRENT-DATE TO WS-RESP-TIMESTAMP.

           STRING
               WS-RESP-TIMESTAMP DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-REQ-CARD-NUMBER DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-REQ-AMOUNT DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-REQ-MERCHANT-NAME DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-RESP-STATUS DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-RESP-AUTH-CODE DELIMITED BY SIZE
               INTO LOG-RECORD
           END-STRING.

           WRITE LOG-RECORD.

       DISPLAY-AUTHORIZATION-RESULT SECTION.
       SHOW-RESULT.
           DISPLAY '  Status: ' WS-RESP-STATUS.

           IF WS-RESP-STATUS = 'APPROVED'
               DISPLAY '  Authorization Code: ' WS-RESP-AUTH-CODE
           ELSE
               DISPLAY '  Decline Reason: ' WS-RESP-DECLINE-REASON
           END-IF.

       DISPLAY-PROCESSING-SUMMARY SECTION.
       SHOW-SUMMARY.
           DISPLAY '========================================'.
           DISPLAY 'Processing Summary'.
           DISPLAY '========================================'.
           DISPLAY 'Total Transactions: ' WS-TRANS-COUNT.

       CLEANUP-SYSTEM SECTION.
       CLEANUP.
           CLOSE CARD-FILE.
           CLOSE TRANSACTION-LOG.
           DISPLAY 'System shutdown complete.'.
