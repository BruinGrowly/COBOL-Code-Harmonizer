       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKACCT.
       AUTHOR. BANKING-SYSTEM.
       DATE-WRITTEN. 2025-07-27.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMERS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO 'TRANSACTIONS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 ACCT-ID     PIC X(10).
           05 NAME        PIC X(30).
           05 BALANCE     PIC 9(7)V99.
           05 ACCT-TYPE   PIC X(1).

       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANS-ACCT-ID    PIC X(10).
           05 TRANS-TYPE       PIC X(1).
           05 TRANS-AMOUNT     PIC 9(7)V99.
           05 TRANS-DATE       PIC X(10).
           05 TRANS-TIME       PIC X(8).

       WORKING-STORAGE SECTION.
       01 CHOICE           PIC 9.
       01 WS-DONE          PIC X VALUE 'N'.
       01 FILE-STATUS      PIC XX.

       01 WS-ACCT-ID       PIC X(10).
       01 WS-NAME          PIC X(30).
       01 WS-BALANCE       PIC 9(7)V99.
       01 WS-TYPE          PIC X(1).

       01 WS-SEARCH-ID     PIC X(10).
       01 WS-AMOUNT        PIC 9(7)V99.
       01 WS-FOUND         PIC X VALUE 'N'.
       01 WS-NEW-BALANCE   PIC 9(7)V99.

       01 WS-CURRENT-DATE.
           05 WS-YEAR      PIC 9999.
           05 WS-MONTH     PIC 99.
           05 WS-DAY       PIC 99.
       01 WS-CURRENT-TIME.
           05 WS-HOUR      PIC 99.
           05 WS-MINUTE    PIC 99.
           05 WS-SECOND    PIC 99.
       01 WS-DATE-STRING   PIC X(10).
       01 WS-TIME-STRING   PIC X(8).
       01 WS-STMT-COUNT    PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=============================================="
           DISPLAY "🏦 COBOL BANKING SYSTEM"
           DISPLAY "=============================================="
           PERFORM UNTIL WS-DONE = 'Y'
               DISPLAY " "
               DISPLAY "📋 MAIN MENU:"
               DISPLAY "  1. Create New Account"
               DISPLAY "  2. View All Accounts"
               DISPLAY "  3. Deposit Money"
               DISPLAY "  4. Withdraw Money"
               DISPLAY "  5. Mini Statement"
               DISPLAY "  6. Apply Interest (Savings)"
               DISPLAY "  7. Exit System"
               DISPLAY " "
               DISPLAY "Enter your choice (1-7): " WITH NO ADVANCING
               ACCEPT CHOICE
               EVALUATE CHOICE
                   WHEN 1
                       PERFORM CREATE-ACCOUNT
                   WHEN 2
                       PERFORM VIEW-ACCOUNTS
                   WHEN 3
                       PERFORM DEPOSIT-MONEY
                   WHEN 4
                       PERFORM WITHDRAW-MONEY
                   WHEN 5
                       PERFORM MINI-STATEMENT
                   WHEN 6
                       PERFORM APPLY-INTEREST
                   WHEN 7
                       DISPLAY "👋 Thank you for using COBOL Banking System!"
                       MOVE 'Y' TO WS-DONE
                   WHEN OTHER
                       DISPLAY "❌ Invalid option. Please enter 1-7."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       CREATE-ACCOUNT.
           DISPLAY " "
           DISPLAY "💳 CREATE NEW ACCOUNT"
           DISPLAY "====================="

           DISPLAY "Enter Account ID (max 10 chars): " WITH NO ADVANCING
           ACCEPT WS-ACCT-ID

           DISPLAY "Enter Customer Name (max 30 chars): " WITH NO ADVANCING
           ACCEPT WS-NAME

           DISPLAY "Enter Initial Balance: $" WITH NO ADVANCING
           ACCEPT WS-BALANCE

           DISPLAY "Enter Account Type (S=Savings, C=Checking): "
               WITH NO ADVANCING
           ACCEPT WS-TYPE

           PERFORM WRITE-CUSTOMER-RECORD

           DISPLAY " "
           DISPLAY "✅ Account created successfully!"
           DISPLAY "   Account ID: " WS-ACCT-ID
           DISPLAY "   Name: " WS-NAME
           DISPLAY "   Balance: $" WS-BALANCE
           DISPLAY "   Type: " WS-TYPE.

       VIEW-ACCOUNTS.
           DISPLAY " "
           DISPLAY "👥 ALL CUSTOMER ACCOUNTS"
           DISPLAY "========================"

           OPEN INPUT CUSTOMER-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
               DISPLAY "   No accounts found or file cannot be read."
           ELSE
               DISPLAY "Account ID | Customer Name              | Balance    | Type"
               DISPLAY "-----------|----------------------------|------------|-----"

               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       DISPLAY ACCT-ID " | " NAME " | $" BALANCE " | " ACCT-TYPE
                   END-IF
               END-PERFORM

               IF FILE-STATUS NOT = "10" AND FILE-STATUS NOT = "00"
                   DISPLAY "❌ Error reading customer file: " FILE-STATUS
               END-IF
           END-IF

           CLOSE CUSTOMER-FILE.

       DEPOSIT-MONEY.
           DISPLAY " "
           DISPLAY "💰 DEPOSIT MONEY"
           DISPLAY "================"

           DISPLAY "Enter Account ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID

           DISPLAY "Enter deposit amount: $" WITH NO ADVANCING
           ACCEPT WS-AMOUNT

           PERFORM UPDATE-BALANCE-ADD

           IF WS-FOUND = 'Y'
               DISPLAY " "
               DISPLAY "✅ Deposit successful!"
               DISPLAY "   Account ID: " WS-SEARCH-ID
               DISPLAY "   Amount deposited: $" WS-AMOUNT
               DISPLAY "   New balance: $" WS-NEW-BALANCE
           ELSE
               DISPLAY " "
               DISPLAY "❌ Account not found: " WS-SEARCH-ID
           END-IF.

       WITHDRAW-MONEY.
           DISPLAY " "
           DISPLAY "💸 WITHDRAW MONEY"
           DISPLAY "================="

           DISPLAY "Enter Account ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID

           DISPLAY "Enter withdrawal amount: $" WITH NO ADVANCING
           ACCEPT WS-AMOUNT

           PERFORM UPDATE-BALANCE-SUBTRACT

           IF WS-FOUND = 'Y'
               DISPLAY " "
               DISPLAY "✅ Withdrawal successful!"
               DISPLAY "   Account ID: " WS-SEARCH-ID
               DISPLAY "   Amount withdrawn: $" WS-AMOUNT
               DISPLAY "   New balance: $" WS-NEW-BALANCE
           ELSE
               DISPLAY " "
               DISPLAY "❌ Account not found: " WS-SEARCH-ID
           END-IF.

       UPDATE-BALANCE-ADD.
           MOVE 'N' TO WS-FOUND
           OPEN I-O CUSTOMER-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       IF ACCT-ID = WS-SEARCH-ID
                           ADD WS-AMOUNT TO BALANCE
                           MOVE BALANCE TO WS-NEW-BALANCE
                           REWRITE CUSTOMER-RECORD
                           MOVE 'Y' TO WS-FOUND
                           PERFORM LOG-TRANSACTION-DEPOSIT
                           MOVE "10" TO FILE-STATUS
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

           CLOSE CUSTOMER-FILE.

       UPDATE-BALANCE-SUBTRACT.
           MOVE 'N' TO WS-FOUND
           OPEN I-O CUSTOMER-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       IF ACCT-ID = WS-SEARCH-ID
                           IF BALANCE >= WS-AMOUNT
                               SUBTRACT WS-AMOUNT FROM BALANCE
                               MOVE BALANCE TO WS-NEW-BALANCE
                               REWRITE CUSTOMER-RECORD
                               MOVE 'Y' TO WS-FOUND
                               PERFORM LOG-TRANSACTION-WITHDRAW
                           ELSE
                               DISPLAY " "
                               DISPLAY "❌ Insufficient funds!"
                               DISPLAY "   Current balance: $" BALANCE
                               DISPLAY "   Requested amount: $" WS-AMOUNT
                               MOVE 'N' TO WS-FOUND
                           END-IF
                           MOVE "10" TO FILE-STATUS
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

           CLOSE CUSTOMER-FILE.

       WRITE-CUSTOMER-RECORD.
           OPEN EXTEND CUSTOMER-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               MOVE WS-ACCT-ID TO ACCT-ID
               MOVE WS-NAME TO NAME
               MOVE WS-BALANCE TO BALANCE
               MOVE WS-TYPE TO ACCT-TYPE
               WRITE CUSTOMER-RECORD

               IF FILE-STATUS NOT = "00"
                   DISPLAY "❌ Error writing to customer file: " FILE-STATUS
               END-IF
           END-IF

           CLOSE CUSTOMER-FILE.

       MINI-STATEMENT.
           DISPLAY " "
           DISPLAY "📊 MINI STATEMENT"
           DISPLAY "================"

           DISPLAY "Enter Account ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID

           DISPLAY " "
           DISPLAY "Last 5 transactions for Account: " WS-SEARCH-ID
           DISPLAY "Date       | Time     | Type | Amount     "
           DISPLAY "-----------|----------|------|------------"

           MOVE 0 TO WS-STMT-COUNT
           OPEN INPUT TRANSACTION-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ No transaction history found."
           ELSE
               PERFORM UNTIL FILE-STATUS = "10" OR WS-STMT-COUNT >= 5
                   READ TRANSACTION-FILE
                   IF FILE-STATUS = "00"
                       IF TRANS-ACCT-ID = WS-SEARCH-ID
                           ADD 1 TO WS-STMT-COUNT
                           IF TRANS-TYPE = 'D'
                               DISPLAY TRANS-DATE " | " TRANS-TIME " | DEP  | $" TRANS-AMOUNT
                           ELSE
                               DISPLAY TRANS-DATE " | " TRANS-TIME " | WTH  | $" TRANS-AMOUNT
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

               IF WS-STMT-COUNT = 0
                   DISPLAY "No transactions found for this account."
               END-IF
           END-IF

           CLOSE TRANSACTION-FILE.

       APPLY-INTEREST.
           DISPLAY " "
           DISPLAY "💰 APPLY INTEREST TO SAVINGS ACCOUNTS"
           DISPLAY "===================================="
           DISPLAY "Applying 2% annual interest to all savings accounts..."

           MOVE 0 TO WS-STMT-COUNT
           OPEN I-O CUSTOMER-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       IF ACCT-TYPE = 'S'
                           COMPUTE WS-AMOUNT = BALANCE * 0.02
                           ADD WS-AMOUNT TO BALANCE
                           REWRITE CUSTOMER-RECORD
                           ADD 1 TO WS-STMT-COUNT
                           MOVE ACCT-ID TO WS-SEARCH-ID
                           PERFORM LOG-TRANSACTION-INTEREST
                           DISPLAY "Interest applied to " ACCT-ID ": $" WS-AMOUNT
                       END-IF
                   END-IF
               END-PERFORM

               DISPLAY " "
               DISPLAY "✅ Interest applied to " WS-STMT-COUNT " savings accounts."
           END-IF

           CLOSE CUSTOMER-FILE.

       LOG-TRANSACTION-DEPOSIT.
           PERFORM GET-CURRENT-DATETIME
           OPEN EXTEND TRANSACTION-FILE
           MOVE WS-SEARCH-ID TO TRANS-ACCT-ID
           MOVE 'D' TO TRANS-TYPE
           MOVE WS-AMOUNT TO TRANS-AMOUNT
           MOVE WS-DATE-STRING TO TRANS-DATE
           MOVE WS-TIME-STRING TO TRANS-TIME
           WRITE TRANSACTION-RECORD
           CLOSE TRANSACTION-FILE.

       LOG-TRANSACTION-WITHDRAW.
           PERFORM GET-CURRENT-DATETIME
           OPEN EXTEND TRANSACTION-FILE
           MOVE WS-SEARCH-ID TO TRANS-ACCT-ID
           MOVE 'W' TO TRANS-TYPE
           MOVE WS-AMOUNT TO TRANS-AMOUNT
           MOVE WS-DATE-STRING TO TRANS-DATE
           MOVE WS-TIME-STRING TO TRANS-TIME
           WRITE TRANSACTION-RECORD
           CLOSE TRANSACTION-FILE.

       LOG-TRANSACTION-INTEREST.
           PERFORM GET-CURRENT-DATETIME
           OPEN EXTEND TRANSACTION-FILE
           MOVE WS-SEARCH-ID TO TRANS-ACCT-ID
           MOVE 'I' TO TRANS-TYPE
           MOVE WS-AMOUNT TO TRANS-AMOUNT
           MOVE WS-DATE-STRING TO TRANS-DATE
           MOVE WS-TIME-STRING TO TRANS-TIME
           WRITE TRANSACTION-RECORD
           CLOSE TRANSACTION-FILE.

       GET-CURRENT-DATETIME.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           STRING WS-YEAR '/' WS-MONTH '/' WS-DAY DELIMITED BY SIZE
               INTO WS-DATE-STRING
           STRING WS-HOUR ':' WS-MINUTE ':' WS-SECOND DELIMITED BY SIZE
               INTO WS-TIME-STRING.
