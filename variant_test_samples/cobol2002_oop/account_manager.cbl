       IDENTIFICATION DIVISION.
       PROGRAM-ID. AccountManager.
      *COBOL-2002 ACCOUNT MANAGER USING OOP

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Customer IS "Customer".

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CustomerObj          OBJECT REFERENCE Customer.
       01 WS-CustomerId        PIC 9(10).
       01 WS-CustomerName      PIC X(50).
       01 WS-TransAmount       PIC S9(9)V99.
       01 WS-Balance           PIC S9(11)V99.
       01 WS-Success           PIC X.
       01 WS-Valid             PIC X.
       01 WS-Choice            PIC 9.
       01 WS-Done              PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY 'COBOL-2002 OOP Account Manager'.
           PERFORM CREATE-CUSTOMER.
           PERFORM MENU-LOOP UNTIL WS-Done = 'Y'.
           PERFORM CLEANUP.
           STOP RUN.

       CREATE-CUSTOMER.
           DISPLAY 'Enter Customer ID: ' WITH NO ADVANCING.
           ACCEPT WS-CustomerId.
           DISPLAY 'Enter Customer Name: ' WITH NO ADVANCING.
           ACCEPT WS-CustomerName.

           INVOKE Customer "NEW"
               USING WS-CustomerId WS-CustomerName
               RETURNING CustomerObj
           END-INVOKE.

           IF CustomerObj NOT = NULL
               DISPLAY 'Customer object created successfully'
           ELSE
               DISPLAY 'Error creating customer object'
               STOP RUN
           END-IF.

       MENU-LOOP.
           DISPLAY ' '.
           DISPLAY 'ACCOUNT MENU:'.
           DISPLAY '1. Process Transaction'.
           DISPLAY '2. Check Balance'.
           DISPLAY '3. Validate Customer'.
           DISPLAY '4. Set Credit Limit'.
           DISPLAY '5. Get Customer Info'.
           DISPLAY '6. Exit'.
           DISPLAY 'Choice: ' WITH NO ADVANCING.
           ACCEPT WS-Choice.

           EVALUATE WS-Choice
               WHEN 1
                   PERFORM PROCESS-TRANSACTION
               WHEN 2
                   PERFORM CHECK-BALANCE
               WHEN 3
                   PERFORM VALIDATE-CUSTOMER
               WHEN 4
                   PERFORM SET-CREDIT-LIMIT
               WHEN 5
                   PERFORM GET-CUSTOMER-INFO
               WHEN 6
                   MOVE 'Y' TO WS-Done
               WHEN OTHER
                   DISPLAY 'Invalid choice'
           END-EVALUATE.

       PROCESS-TRANSACTION.
           DISPLAY 'Enter transaction amount: ' WITH NO ADVANCING.
           DISPLAY '(positive for credit, negative for debit)'.
           ACCEPT WS-TransAmount.

           INVOKE CustomerObj "ProcessTransaction"
               USING WS-TransAmount
               RETURNING WS-Success
           END-INVOKE.

           IF WS-Success = 'Y'
               DISPLAY 'Transaction completed successfully'
           ELSE
               DISPLAY 'Transaction failed'
           END-IF.

       CHECK-BALANCE.
           INVOKE CustomerObj "GetBalance"
               RETURNING WS-Balance
           END-INVOKE.

           DISPLAY 'Current Balance: ' WS-Balance.

       VALIDATE-CUSTOMER.
           INVOKE CustomerObj "ValidateCustomer"
               RETURNING WS-Valid
           END-INVOKE.

           IF WS-Valid = 'Y'
               DISPLAY 'Customer is valid and active'
           ELSE
               DISPLAY 'Customer validation failed'
           END-IF.

       SET-CREDIT-LIMIT.
           DISPLAY 'Enter new credit limit: ' WITH NO ADVANCING.
           ACCEPT WS-TransAmount.

           INVOKE CustomerObj "SetCreditLimit"
               USING WS-TransAmount
           END-INVOKE.

       GET-CUSTOMER-INFO.
           INVOKE CustomerObj "GetCustomerInfo"
               RETURNING CustomerInfo
           END-INVOKE.

           DISPLAY 'Customer Information:'.
           DISPLAY '  ID: ' CustId OF CustomerInfo.
           DISPLAY '  Name: ' CustName OF CustomerInfo.
           DISPLAY '  Balance: ' CustBalance OF CustomerInfo.
           DISPLAY '  Status: ' CustStatus OF CustomerInfo.

       01 CustomerInfo.
           05 CustId           PIC 9(10).
           05 CustName         PIC X(50).
           05 CustBalance      PIC S9(11)V99.
           05 CustStatus       PIC X(10).

       CLEANUP.
           IF CustomerObj NOT = NULL
               INVOKE CustomerObj "FINALIZE"
               END-INVOKE
               SET CustomerObj TO NULL
           END-IF.
           DISPLAY 'Account Manager terminating'.
