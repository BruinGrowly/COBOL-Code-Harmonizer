       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIGateway.
      *COBOL-2014 JSON API INTEGRATION
      *Demonstrates JSON GENERATE and JSON PARSE capabilities

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REQUEST-LOG ASSIGN TO 'api_requests.log'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  REQUEST-LOG.
       01  LOG-RECORD              PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.

      *JSON Request Structure
       01  WS-REQUEST-DATA.
           05  WS-REQUEST-ID       PIC 9(10).
           05  WS-CUSTOMER-ID      PIC 9(8).
           05  WS-TRANSACTION-TYPE PIC X(20).
           05  WS-AMOUNT           PIC 9(9)V99.
           05  WS-TIMESTAMP        PIC X(26).
           05  WS-STATUS           PIC X(10).

      *JSON Response Structure
       01  WS-RESPONSE-DATA.
           05  WS-RESP-CODE        PIC 9(3).
           05  WS-RESP-MESSAGE     PIC X(100).
           05  WS-RESP-TIMESTAMP   PIC X(26).
           05  WS-BALANCE          PIC 9(10)V99.

      *JSON Strings
       01  WS-JSON-REQUEST         PIC X(500).
       01  WS-JSON-RESPONSE        PIC X(500).
       01  WS-JSON-LENGTH          PIC 9(4).

      *API Statistics
       01  WS-STATS.
           05  WS-TOTAL-REQUESTS   PIC 9(8) VALUE ZERO.
           05  WS-SUCCESS-COUNT    PIC 9(8) VALUE ZERO.
           05  WS-ERROR-COUNT      PIC 9(8) VALUE ZERO.
           05  WS-AVG-AMOUNT       PIC 9(9)V99 VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN-LOGIC SECTION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GATEWAY.
           PERFORM PROCESS-API-REQUESTS UNTIL WS-TOTAL-REQUESTS > 5.
           PERFORM DISPLAY-STATISTICS.
           PERFORM CLEANUP-GATEWAY.
           STOP RUN.

       INITIALIZE-GATEWAY SECTION.
       INIT-GATEWAY.
           DISPLAY '=== API Gateway Starting ==='.
           OPEN OUTPUT REQUEST-LOG.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening log file: ' WS-FILE-STATUS
               STOP RUN
           END-IF.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP.
           DISPLAY 'Gateway initialized at: ' WS-TIMESTAMP.

       PROCESS-API-REQUESTS SECTION.
       PROCESS-REQUEST.
           ADD 1 TO WS-TOTAL-REQUESTS.
           PERFORM BUILD-SAMPLE-REQUEST.
           PERFORM PARSE-JSON-REQUEST.
           PERFORM VALIDATE-REQUEST.
           PERFORM EXECUTE-TRANSACTION.
           PERFORM GENERATE-JSON-RESPONSE.
           PERFORM LOG-API-CALL.
           PERFORM UPDATE-STATISTICS.

       BUILD-SAMPLE-REQUEST SECTION.
       BUILD-REQUEST.
      *Simulate incoming JSON request
           MOVE WS-TOTAL-REQUESTS TO WS-REQUEST-ID.
           COMPUTE WS-CUSTOMER-ID = 10000000 + WS-TOTAL-REQUESTS.

           EVALUATE WS-TOTAL-REQUESTS
               WHEN 1
                   MOVE 'DEPOSIT' TO WS-TRANSACTION-TYPE
                   MOVE 1500.00 TO WS-AMOUNT
               WHEN 2
                   MOVE 'WITHDRAWAL' TO WS-TRANSACTION-TYPE
                   MOVE 500.00 TO WS-AMOUNT
               WHEN 3
                   MOVE 'TRANSFER' TO WS-TRANSACTION-TYPE
                   MOVE 2500.00 TO WS-AMOUNT
               WHEN 4
                   MOVE 'BALANCE_INQUIRY' TO WS-TRANSACTION-TYPE
                   MOVE ZERO TO WS-AMOUNT
               WHEN OTHER
                   MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
                   MOVE 750.50 TO WS-AMOUNT
           END-EVALUATE.

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP.
           MOVE 'PENDING' TO WS-STATUS.

       PARSE-JSON-REQUEST SECTION.
       PARSE-REQUEST.
      *In real implementation, would parse incoming JSON
      *Using JSON PARSE statement (COBOL-2014 feature)
           DISPLAY 'Parsing JSON request for ID: ' WS-REQUEST-ID.

      *Example JSON PARSE syntax (commented for compatibility):
      *    JSON PARSE WS-JSON-REQUEST INTO WS-REQUEST-DATA
      *        ON EXCEPTION
      *            DISPLAY 'JSON Parse Error'
      *            MOVE 400 TO WS-RESP-CODE
      *            MOVE 'Invalid JSON format' TO WS-RESP-MESSAGE
      *        NOT ON EXCEPTION
      *            DISPLAY 'JSON parsed successfully'
      *    END-JSON.

       VALIDATE-REQUEST SECTION.
       VALIDATE-REQ.
           IF WS-CUSTOMER-ID < 10000000
               MOVE 400 TO WS-RESP-CODE
               MOVE 'Invalid customer ID' TO WS-RESP-MESSAGE
               GO TO VALIDATE-EXIT
           END-IF.

           IF WS-AMOUNT < ZERO
               MOVE 400 TO WS-RESP-CODE
               MOVE 'Invalid amount' TO WS-RESP-MESSAGE
               GO TO VALIDATE-EXIT
           END-IF.

           IF WS-TRANSACTION-TYPE = SPACES
               MOVE 400 TO WS-RESP-CODE
               MOVE 'Missing transaction type' TO WS-RESP-MESSAGE
               GO TO VALIDATE-EXIT
           END-IF.

           MOVE 200 TO WS-RESP-CODE.
           MOVE 'Request validated' TO WS-RESP-MESSAGE.

       VALIDATE-EXIT.
           EXIT.

       EXECUTE-TRANSACTION SECTION.
       EXEC-TRANSACTION.
           IF WS-RESP-CODE NOT = 200
               GO TO EXEC-EXIT
           END-IF.

           EVALUATE WS-TRANSACTION-TYPE
               WHEN 'DEPOSIT'
                   PERFORM PROCESS-DEPOSIT
               WHEN 'WITHDRAWAL'
                   PERFORM PROCESS-WITHDRAWAL
               WHEN 'TRANSFER'
                   PERFORM PROCESS-TRANSFER
               WHEN 'BALANCE_INQUIRY'
                   PERFORM PROCESS-INQUIRY
               WHEN 'PAYMENT'
                   PERFORM PROCESS-PAYMENT
               WHEN OTHER
                   MOVE 400 TO WS-RESP-CODE
                   MOVE 'Unknown transaction type' TO WS-RESP-MESSAGE
           END-EVALUATE.

       EXEC-EXIT.
           EXIT.

       PROCESS-DEPOSIT SECTION.
       PROC-DEPOSIT.
           COMPUTE WS-BALANCE = 5000.00 + WS-AMOUNT.
           MOVE 200 TO WS-RESP-CODE.
           MOVE 'Deposit processed successfully' TO WS-RESP-MESSAGE.
           MOVE 'COMPLETED' TO WS-STATUS.
           ADD 1 TO WS-SUCCESS-COUNT.

       PROCESS-WITHDRAWAL SECTION.
       PROC-WITHDRAWAL.
           COMPUTE WS-BALANCE = 5000.00 - WS-AMOUNT.
           IF WS-BALANCE < ZERO
               MOVE 400 TO WS-RESP-CODE
               MOVE 'Insufficient funds' TO WS-RESP-MESSAGE
               MOVE 'FAILED' TO WS-STATUS
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               MOVE 200 TO WS-RESP-CODE
               MOVE 'Withdrawal processed' TO WS-RESP-MESSAGE
               MOVE 'COMPLETED' TO WS-STATUS
               ADD 1 TO WS-SUCCESS-COUNT
           END-IF.

       PROCESS-TRANSFER SECTION.
       PROC-TRANSFER.
           COMPUTE WS-BALANCE = 5000.00 - WS-AMOUNT.
           MOVE 200 TO WS-RESP-CODE.
           MOVE 'Transfer completed' TO WS-RESP-MESSAGE.
           MOVE 'COMPLETED' TO WS-STATUS.
           ADD 1 TO WS-SUCCESS-COUNT.

       PROCESS-INQUIRY SECTION.
       PROC-INQUIRY.
           MOVE 5000.00 TO WS-BALANCE.
           MOVE 200 TO WS-RESP-CODE.
           MOVE 'Balance inquiry successful' TO WS-RESP-MESSAGE.
           MOVE 'COMPLETED' TO WS-STATUS.
           ADD 1 TO WS-SUCCESS-COUNT.

       PROCESS-PAYMENT SECTION.
       PROC-PAYMENT.
           COMPUTE WS-BALANCE = 5000.00 - WS-AMOUNT.
           MOVE 200 TO WS-RESP-CODE.
           MOVE 'Payment processed' TO WS-RESP-MESSAGE.
           MOVE 'COMPLETED' TO WS-STATUS.
           ADD 1 TO WS-SUCCESS-COUNT.

       GENERATE-JSON-RESPONSE SECTION.
       GEN-RESPONSE.
           MOVE FUNCTION CURRENT-DATE TO WS-RESP-TIMESTAMP.

      *Example JSON GENERATE syntax (COBOL-2014 feature):
      *    JSON GENERATE WS-JSON-RESPONSE FROM WS-RESPONSE-DATA
      *        ON EXCEPTION
      *            DISPLAY 'JSON Generation Error'
      *        NOT ON EXCEPTION
      *            DISPLAY 'JSON response generated'
      *            DISPLAY WS-JSON-RESPONSE
      *    END-JSON.

           DISPLAY 'Response Code: ' WS-RESP-CODE.
           DISPLAY 'Response Message: ' WS-RESP-MESSAGE.
           IF WS-RESP-CODE = 200
               DISPLAY 'Balance: ' WS-BALANCE
           END-IF.

       LOG-API-CALL SECTION.
       LOG-CALL.
           STRING
               'REQ=' WS-REQUEST-ID DELIMITED BY SIZE
               ' CUST=' WS-CUSTOMER-ID DELIMITED BY SIZE
               ' TYPE=' WS-TRANSACTION-TYPE DELIMITED BY SIZE
               ' AMT=' WS-AMOUNT DELIMITED BY SIZE
               ' STATUS=' WS-STATUS DELIMITED BY SIZE
               INTO LOG-RECORD
           END-STRING.

           WRITE LOG-RECORD.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error writing log: ' WS-FILE-STATUS
           END-IF.

       UPDATE-STATISTICS SECTION.
       UPDATE-STATS.
           IF WS-RESP-CODE = 200 AND WS-AMOUNT > ZERO
               COMPUTE WS-AVG-AMOUNT =
                   (WS-AVG-AMOUNT * (WS-SUCCESS-COUNT - 1) + WS-AMOUNT)
                   / WS-SUCCESS-COUNT
           END-IF.

       DISPLAY-STATISTICS SECTION.
       SHOW-STATS.
           DISPLAY ' '.
           DISPLAY '=== API Gateway Statistics ==='.
           DISPLAY 'Total Requests: ' WS-TOTAL-REQUESTS.
           DISPLAY 'Successful: ' WS-SUCCESS-COUNT.
           DISPLAY 'Errors: ' WS-ERROR-COUNT.
           DISPLAY 'Average Transaction Amount: ' WS-AVG-AMOUNT.

       CLEANUP-GATEWAY SECTION.
       CLEANUP.
           CLOSE REQUEST-LOG.
           DISPLAY 'Gateway shutdown complete.'.
