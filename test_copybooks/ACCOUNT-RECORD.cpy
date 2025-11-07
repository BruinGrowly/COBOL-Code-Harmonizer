      *****************************************************************
      * Account Record Copybook
      *****************************************************************
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER             PIC 9(12).
           05  ACCT-CUSTOMER-ID        PIC 9(10).
           05  ACCT-TYPE               PIC X(10).
           05  ACCT-BALANCE            PIC S9(11)V99 COMP-3.
           05  ACCT-INTEREST-RATE      PIC 9V9999 COMP-2.
           05  ACCT-OPEN-DATE          PIC 9(8).
           05  ACCT-LAST-ACTIVITY      PIC 9(8).
           05  ACCT-STATUS             PIC X.
