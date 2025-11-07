      *****************************************************************
      * Customer Record Copybook
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUST-ID                 PIC 9(10).
           05  CUST-NAME               PIC X(50).
           05  CUST-ADDRESS.
               10  CUST-STREET         PIC X(50).
               10  CUST-CITY           PIC X(30).
               10  CUST-STATE          PIC XX.
               10  CUST-ZIP            PIC X(10).
           05  CUST-PHONE              PIC X(15).
           05  CUST-EMAIL              PIC X(100).
           05  CUST-BALANCE            PIC S9(9)V99 COMP-3.
           05  CUST-STATUS             PIC X.
               88  CUST-ACTIVE         VALUE 'A'.
               88  CUST-INACTIVE       VALUE 'I'.
               88  CUST-SUSPENDED      VALUE 'S'.
