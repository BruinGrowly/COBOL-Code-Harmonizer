       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2CUSTQRY.
      *IBM MAINFRAME - DB2 EMBEDDED SQL DEMONSTRATION
      *Demonstrates EXEC SQL blocks for database operations

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *SQL Communication Area
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      *Customer Table Structure
       EXEC SQL DECLARE CUSTOMER TABLE
           (CUSTOMER_ID     INTEGER NOT NULL,
            CUSTOMER_NAME   CHAR(50) NOT NULL,
            ACCOUNT_TYPE    CHAR(20),
            BALANCE         DECIMAL(15,2),
            CREDIT_LIMIT    DECIMAL(15,2),
            BRANCH_CODE     CHAR(6),
            LAST_TRANS_DATE DATE,
            STATUS          CHAR(1))
       END-EXEC.

      *Host Variables for SQL Operations
       01  SQL-HOST-VARIABLES.
           05  :WS-CUSTOMER-ID        PIC S9(9) COMP.
           05  :WS-CUSTOMER-NAME      PIC X(50).
           05  :WS-ACCOUNT-TYPE       PIC X(20).
           05  :WS-BALANCE            PIC S9(13)V99 COMP-3.
           05  :WS-CREDIT-LIMIT       PIC S9(13)V99 COMP-3.
           05  :WS-BRANCH-CODE        PIC X(6).
           05  :WS-LAST-TRANS-DATE    PIC X(10).
           05  :WS-STATUS             PIC X.

      *Query Parameters
       01  :WS-MIN-BALANCE            PIC S9(13)V99 COMP-3.
       01  :WS-SEARCH-BRANCH          PIC X(6).

      *Cursor Control
       01  WS-END-OF-FETCH            PIC X VALUE 'N'.
       01  WS-ROW-COUNT               PIC 9(6) VALUE ZERO.
       01  WS-SQLCODE-SAVE            PIC S9(9) COMP.

      *Display Variables
       01  WS-BALANCE-DISPLAY         PIC $$$,$$$,$$9.99.
       01  WS-LIMIT-DISPLAY           PIC $$$,$$$,$$9.99.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DB-CONNECTION.
           PERFORM QUERY-HIGH-VALUE-CUSTOMERS.
           PERFORM QUERY-BY-BRANCH.
           PERFORM UPDATE-CREDIT-LIMITS.
           PERFORM INSERT-NEW-CUSTOMER.
           PERFORM DELETE-INACTIVE-ACCOUNTS.
           PERFORM TERMINATE-DB-CONNECTION.
           STOP RUN.

       INITIALIZE-DB-CONNECTION SECTION.
       INIT-DB.
           DISPLAY '*** DB2 Customer Query System ***'.
           DISPLAY ' '.

      *Connect to DB2
           EXEC SQL
               CONNECT TO DB2BANK
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'Error connecting to DB2: ' SQLCODE
               STOP RUN
           ELSE
               DISPLAY 'Connected to DB2 successfully'
           END-IF.

       QUERY-HIGH-VALUE-CUSTOMERS SECTION.
       QUERY-HIGH-VALUE.
           DISPLAY ' '.
           DISPLAY 'Querying high-value customers...'.
           DISPLAY '------------------------------------'.

           MOVE 100000.00 TO :WS-MIN-BALANCE.

      *Declare cursor for high-value customers
           EXEC SQL
               DECLARE HIGH_VALUE_CUST CURSOR FOR
               SELECT CUSTOMER_ID, CUSTOMER_NAME, ACCOUNT_TYPE,
                      BALANCE, CREDIT_LIMIT, BRANCH_CODE, STATUS
               FROM CUSTOMER
               WHERE BALANCE >= :WS-MIN-BALANCE
                 AND STATUS = 'A'
               ORDER BY BALANCE DESC
           END-EXEC.

      *Open cursor
           EXEC SQL
               OPEN HIGH_VALUE_CUST
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'Error opening cursor: ' SQLCODE
               GO TO QUERY-HIGH-VALUE-EXIT
           END-IF.

           MOVE 'N' TO WS-END-OF-FETCH.
           MOVE ZERO TO WS-ROW-COUNT.

           PERFORM FETCH-HIGH-VALUE-RECORD
               UNTIL WS-END-OF-FETCH = 'Y'.

      *Close cursor
           EXEC SQL
               CLOSE HIGH_VALUE_CUST
           END-EXEC.

           DISPLAY 'Total high-value customers: ' WS-ROW-COUNT.

       QUERY-HIGH-VALUE-EXIT.
           EXIT.

       FETCH-HIGH-VALUE-RECORD SECTION.
       FETCH-RECORD.
           EXEC SQL
               FETCH HIGH_VALUE_CUST
               INTO :WS-CUSTOMER-ID, :WS-CUSTOMER-NAME,
                    :WS-ACCOUNT-TYPE, :WS-BALANCE,
                    :WS-CREDIT-LIMIT, :WS-BRANCH-CODE,
                    :WS-STATUS
           END-EXEC.

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-ROW-COUNT
                   PERFORM DISPLAY-CUSTOMER-INFO
               WHEN 100
                   MOVE 'Y' TO WS-END-OF-FETCH
               WHEN OTHER
                   DISPLAY 'Fetch error: ' SQLCODE
                   MOVE 'Y' TO WS-END-OF-FETCH
           END-EVALUATE.

       DISPLAY-CUSTOMER-INFO SECTION.
       DISPLAY-INFO.
           MOVE WS-BALANCE TO WS-BALANCE-DISPLAY.
           MOVE WS-CREDIT-LIMIT TO WS-LIMIT-DISPLAY.

           DISPLAY 'Customer ID: ' WS-CUSTOMER-ID.
           DISPLAY '  Name: ' WS-CUSTOMER-NAME.
           DISPLAY '  Type: ' WS-ACCOUNT-TYPE.
           DISPLAY '  Balance: ' WS-BALANCE-DISPLAY.
           DISPLAY '  Credit Limit: ' WS-LIMIT-DISPLAY.
           DISPLAY '  Branch: ' WS-BRANCH-CODE.
           DISPLAY ' '.

       QUERY-BY-BRANCH SECTION.
       QUERY-BRANCH.
           DISPLAY 'Querying customers by branch...'.
           DISPLAY '--------------------------------'.

           MOVE 'NYC001' TO :WS-SEARCH-BRANCH.

      *Single row query using SELECT INTO
           EXEC SQL
               SELECT COUNT(*), SUM(BALANCE)
               INTO :WS-ROW-COUNT, :WS-BALANCE
               FROM CUSTOMER
               WHERE BRANCH_CODE = :WS-SEARCH-BRANCH
                 AND STATUS = 'A'
           END-EXEC.

           IF SQLCODE = 0
               MOVE WS-BALANCE TO WS-BALANCE-DISPLAY
               DISPLAY 'Branch: ' WS-SEARCH-BRANCH
               DISPLAY 'Active Customers: ' WS-ROW-COUNT
               DISPLAY 'Total Deposits: ' WS-BALANCE-DISPLAY
           ELSE
               DISPLAY 'Query error: ' SQLCODE
           END-IF.

           DISPLAY ' '.

       UPDATE-CREDIT-LIMITS SECTION.
       UPDATE-LIMITS.
           DISPLAY 'Updating credit limits for premium customers...'.
           DISPLAY '-----------------------------------------------'.

      *Update credit limits based on balance
           EXEC SQL
               UPDATE CUSTOMER
               SET CREDIT_LIMIT = BALANCE * 1.5
               WHERE BALANCE >= 50000.00
                 AND ACCOUNT_TYPE = 'PREMIUM'
                 AND STATUS = 'A'
           END-EXEC.

           MOVE SQLCODE TO WS-SQLCODE-SAVE.

           IF WS-SQLCODE-SAVE = 0
               EXEC SQL COMMIT WORK END-EXEC
               DISPLAY 'Credit limits updated successfully'
               DISPLAY 'Rows affected: ' SQLERRD(3)
           ELSE
               IF WS-SQLCODE-SAVE = 100
                   DISPLAY 'No rows found to update'
               ELSE
                   EXEC SQL ROLLBACK WORK END-EXEC
                   DISPLAY 'Update failed: ' WS-SQLCODE-SAVE
                   DISPLAY 'Transaction rolled back'
               END-IF
           END-IF.

           DISPLAY ' '.

       INSERT-NEW-CUSTOMER SECTION.
       INSERT-CUSTOMER.
           DISPLAY 'Inserting new customer record...'.
           DISPLAY '--------------------------------'.

           MOVE 99999999 TO :WS-CUSTOMER-ID.
           MOVE 'SMITH, JOHN M.' TO :WS-CUSTOMER-NAME.
           MOVE 'CHECKING' TO :WS-ACCOUNT-TYPE.
           MOVE 5000.00 TO :WS-BALANCE.
           MOVE 10000.00 TO :WS-CREDIT-LIMIT.
           MOVE 'NYC001' TO :WS-BRANCH-CODE.
           MOVE 'A' TO :WS-STATUS.

           EXEC SQL
               INSERT INTO CUSTOMER
                   (CUSTOMER_ID, CUSTOMER_NAME, ACCOUNT_TYPE,
                    BALANCE, CREDIT_LIMIT, BRANCH_CODE,
                    LAST_TRANS_DATE, STATUS)
               VALUES
                   (:WS-CUSTOMER-ID, :WS-CUSTOMER-NAME,
                    :WS-ACCOUNT-TYPE, :WS-BALANCE,
                    :WS-CREDIT-LIMIT, :WS-BRANCH-CODE,
                    CURRENT DATE, :WS-STATUS)
           END-EXEC.

           IF SQLCODE = 0
               EXEC SQL COMMIT WORK END-EXEC
               DISPLAY 'Customer inserted successfully'
               DISPLAY 'Customer ID: ' WS-CUSTOMER-ID
           ELSE
               EXEC SQL ROLLBACK WORK END-EXEC
               DISPLAY 'Insert failed: ' SQLCODE
           END-IF.

           DISPLAY ' '.

       DELETE-INACTIVE-ACCOUNTS SECTION.
       DELETE-INACTIVE.
           DISPLAY 'Deleting inactive zero-balance accounts...'.
           DISPLAY '-------------------------------------------'.

      *Delete customers with zero balance and inactive status
           EXEC SQL
               DELETE FROM CUSTOMER
               WHERE BALANCE = 0
                 AND STATUS = 'I'
                 AND LAST_TRANS_DATE < CURRENT DATE - 2 YEARS
           END-EXEC.

           IF SQLCODE = 0
               EXEC SQL COMMIT WORK END-EXEC
               DISPLAY 'Inactive accounts deleted'
               DISPLAY 'Rows deleted: ' SQLERRD(3)
           ELSE
               IF SQLCODE = 100
                   DISPLAY 'No inactive accounts found'
               ELSE
                   EXEC SQL ROLLBACK WORK END-EXEC
                   DISPLAY 'Delete failed: ' SQLCODE
               END-IF
           END-IF.

           DISPLAY ' '.

       TERMINATE-DB-CONNECTION SECTION.
       TERM-DB.
           EXEC SQL
               COMMIT WORK
           END-EXEC.

           EXEC SQL
               CONNECT RESET
           END-EXEC.

           DISPLAY 'DB2 connection closed.'.
           DISPLAY 'Program complete.'.
