       IDENTIFICATION DIVISION.
       CLASS-ID. Customer.
      *COBOL-2002 OBJECT-ORIENTED CUSTOMER CLASS

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Customer.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CustomerData.
           05 CustomerId       PIC 9(10) PRIVATE.
           05 CustomerName     PIC X(50) PRIVATE.
           05 CustomerEmail    PIC X(100) PRIVATE.
           05 AccountBalance   PIC S9(11)V99 COMP-3 PRIVATE.
           05 CustomerStatus   PIC X(10) PRIVATE.
           05 CreditLimit      PIC 9(9)V99 PRIVATE.

       OBJECT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TempAmount       PIC S9(11)V99.
       01 WS-CreditAvailable  PIC S9(11)V99.

       PROCEDURE DIVISION.

       METHOD-ID. NEW.
      *CONSTRUCTOR METHOD
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-CustomerId       PIC 9(10).
       01 LS-Name             PIC X(50).
       PROCEDURE DIVISION USING LS-CustomerId LS-Name.
           MOVE LS-CustomerId TO CustomerId.
           MOVE LS-Name TO CustomerName.
           MOVE ZERO TO AccountBalance.
           MOVE 'ACTIVE' TO CustomerStatus.
           MOVE 5000.00 TO CreditLimit.
           EXIT METHOD.
       END METHOD NEW.

       METHOD-ID. SetCreditLimit.
      *SET CUSTOMER CREDIT LIMIT
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NewLimit         PIC 9(9)V99.
       PROCEDURE DIVISION USING LS-NewLimit.
           IF LS-NewLimit >= ZERO AND LS-NewLimit <= 999999.99
               MOVE LS-NewLimit TO CreditLimit
               DISPLAY 'Credit limit updated to: ' CreditLimit
           ELSE
               DISPLAY 'Invalid credit limit: ' LS-NewLimit
           END-IF.
           EXIT METHOD.
       END METHOD SetCreditLimit.

       METHOD-ID. ProcessTransaction.
      *PROCESS CUSTOMER TRANSACTION
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-TransactionAmount PIC S9(9)V99.
       01 LS-Success           PIC X VALUE 'N'.
       PROCEDURE DIVISION USING LS-TransactionAmount
                          RETURNING LS-Success.
           IF LS-TransactionAmount > ZERO
               PERFORM ProcessCredit
           ELSE
               PERFORM ProcessDebit
           END-IF.
           EXIT METHOD.

       ProcessCredit.
           ADD LS-TransactionAmount TO AccountBalance.
           MOVE 'Y' TO LS-Success.
           DISPLAY 'Credit processed: ' LS-TransactionAmount.

       ProcessDebit.
           COMPUTE WS-CreditAvailable = AccountBalance + CreditLimit.
           IF WS-CreditAvailable >= ABS(LS-TransactionAmount)
               ADD LS-TransactionAmount TO AccountBalance
               MOVE 'Y' TO LS-Success
               DISPLAY 'Debit processed: ' LS-TransactionAmount
           ELSE
               MOVE 'N' TO LS-Success
               DISPLAY 'Transaction declined - insufficient funds'
           END-IF.

       END METHOD ProcessTransaction.

       METHOD-ID. GetBalance.
      *RETURN CURRENT BALANCE
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-Balance          PIC S9(11)V99.
       PROCEDURE DIVISION RETURNING LS-Balance.
           MOVE AccountBalance TO LS-Balance.
           EXIT METHOD.
       END METHOD GetBalance.

       METHOD-ID. ValidateCustomer.
      *VALIDATE CUSTOMER STATUS
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-IsValid          PIC X.
       PROCEDURE DIVISION RETURNING LS-IsValid.
           IF CustomerStatus = 'ACTIVE'
               IF CustomerId > ZERO
                   MOVE 'Y' TO LS-IsValid
               ELSE
                   MOVE 'N' TO LS-IsValid
               END-IF
           ELSE
               MOVE 'N' TO LS-IsValid
           END-IF.
           EXIT METHOD.
       END METHOD ValidateCustomer.

       METHOD-ID. GetCustomerInfo.
      *RETRIEVE CUSTOMER INFORMATION
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-CustomerData.
           05 LS-Id            PIC 9(10).
           05 LS-Name          PIC X(50).
           05 LS-Balance       PIC S9(11)V99.
           05 LS-Status        PIC X(10).
       PROCEDURE DIVISION RETURNING LS-CustomerData.
           MOVE CustomerId TO LS-Id.
           MOVE CustomerName TO LS-Name.
           MOVE AccountBalance TO LS-Balance.
           MOVE CustomerStatus TO LS-Status.
           EXIT METHOD.
       END METHOD GetCustomerInfo.

       METHOD-ID. FINALIZE.
      *DESTRUCTOR
       PROCEDURE DIVISION.
           DISPLAY 'Finalizing customer object: ' CustomerId.
           EXIT METHOD.
       END METHOD FINALIZE.

       END OBJECT.
       END CLASS Customer.
