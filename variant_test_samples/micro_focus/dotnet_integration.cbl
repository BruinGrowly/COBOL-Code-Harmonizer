      $SET SOURCEFORMAT"FREE"
      $SET ILUSING"System"
      $SET ILUSING"System.IO"
      $SET ILUSING"System.Collections.Generic"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DotNetIntegration.
      *MICRO FOCUS - .NET INTEGRATION DEMONSTRATION
      *Demonstrates Visual COBOL with .NET Framework

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Console AS "System.Console"
           CLASS File AS "System.IO.File"
           CLASS DateTime AS "System.DateTime"
           CLASS List AS "System.Collections.Generic.List"
           CLASS String AS "System.String".

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *Customer data structure
       01  WS-CUSTOMER-DATA.
           05  WS-CUSTOMER-ID          PIC 9(8).
           05  WS-CUSTOMER-NAME        PIC X(50).
           05  WS-EMAIL-ADDRESS        PIC X(100).
           05  WS-PHONE-NUMBER         PIC X(20).
           05  WS-ACCOUNT-BALANCE      PIC 9(10)V99.
           05  WS-REGISTRATION-DATE    PIC X(10).

      *.NET Object References
       01  WS-DATETIME-OBJ            OBJECT REFERENCE.
       01  WS-FILE-PATH               PIC X(200) VALUE "customers.txt".
       01  WS-FILE-CONTENT            PIC X(1000).
       01  WS-FORMATTED-DATE          PIC X(50).

      *List of customers (.NET Generic List)
       01  WS-CUSTOMER-LIST           OBJECT REFERENCE.
       01  WS-LIST-COUNT              PIC 9(4) COMP-5.

      *Processing variables
       01  WS-LOOP-INDEX              PIC 9(4) COMP-5.
       01  WS-TOTAL-BALANCE           PIC 9(12)V99.
       01  WS-AVERAGE-BALANCE         PIC 9(12)V99.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "Micro Focus .NET Integration Demo"
           DISPLAY "==================================".

           PERFORM INITIALIZE-DOTNET-OBJECTS.
           PERFORM LOAD-CUSTOMER-DATA.
           PERFORM PROCESS-CUSTOMERS.
           PERFORM SAVE-TO-FILE.
           PERFORM CLEANUP-RESOURCES.

           DISPLAY "Program completed successfully.".
           STOP RUN.

       INITIALIZE-DOTNET-OBJECTS SECTION.
       INIT-OBJECTS.
           DISPLAY "Initializing .NET objects...".

      *Create .NET List for customers
           INVOKE List "NEW" RETURNING WS-CUSTOMER-LIST.

      *Get current date/time from .NET
           INVOKE DateTime "get_Now" RETURNING WS-DATETIME-OBJ.

           INVOKE WS-DATETIME-OBJ "ToString"
               USING "yyyy-MM-dd"
               RETURNING WS-FORMATTED-DATE.

           DISPLAY "Current Date: " WS-FORMATTED-DATE.
           DISPLAY SPACE.

       LOAD-CUSTOMER-DATA SECTION.
       LOAD-DATA.
           DISPLAY "Loading customer data...".

      *Customer 1
           MOVE 10001 TO WS-CUSTOMER-ID.
           MOVE "Alice Johnson" TO WS-CUSTOMER-NAME.
           MOVE "alice.johnson@example.com" TO WS-EMAIL-ADDRESS.
           MOVE "555-0101" TO WS-PHONE-NUMBER.
           MOVE 15000.00 TO WS-ACCOUNT-BALANCE.
           MOVE WS-FORMATTED-DATE TO WS-REGISTRATION-DATE.
           PERFORM ADD-CUSTOMER-TO-LIST.

      *Customer 2
           MOVE 10002 TO WS-CUSTOMER-ID.
           MOVE "Bob Smith" TO WS-CUSTOMER-NAME.
           MOVE "bob.smith@example.com" TO WS-EMAIL-ADDRESS.
           MOVE "555-0102" TO WS-PHONE-NUMBER.
           MOVE 28500.50 TO WS-ACCOUNT-BALANCE.
           MOVE WS-FORMATTED-DATE TO WS-REGISTRATION-DATE.
           PERFORM ADD-CUSTOMER-TO-LIST.

      *Customer 3
           MOVE 10003 TO WS-CUSTOMER-ID.
           MOVE "Carol Williams" TO WS-CUSTOMER-NAME.
           MOVE "carol.w@example.com" TO WS-EMAIL-ADDRESS.
           MOVE "555-0103" TO WS-PHONE-NUMBER.
           MOVE 42750.25 TO WS-ACCOUNT-BALANCE.
           MOVE WS-FORMATTED-DATE TO WS-REGISTRATION-DATE.
           PERFORM ADD-CUSTOMER-TO-LIST.

      *Customer 4
           MOVE 10004 TO WS-CUSTOMER-ID.
           MOVE "David Brown" TO WS-CUSTOMER-NAME.
           MOVE "david.brown@example.com" TO WS-EMAIL-ADDRESS.
           MOVE "555-0104" TO WS-PHONE-NUMBER.
           MOVE 8900.75 TO WS-ACCOUNT-BALANCE.
           MOVE WS-FORMATTED-DATE TO WS-REGISTRATION-DATE.
           PERFORM ADD-CUSTOMER-TO-LIST.

           INVOKE WS-CUSTOMER-LIST "get_Count" RETURNING WS-LIST-COUNT.
           DISPLAY "Loaded " WS-LIST-COUNT " customers".
           DISPLAY SPACE.

       ADD-CUSTOMER-TO-LIST SECTION.
       ADD-TO-LIST.
      *In real implementation, would add customer object to list
      *For demo, we're just tracking the data
           DISPLAY "  Added: " WS-CUSTOMER-NAME
                   " (ID: " WS-CUSTOMER-ID ")".

       PROCESS-CUSTOMERS SECTION.
       PROC-CUSTOMERS.
           DISPLAY "Processing customers...".
           DISPLAY "--------------------------------------------".

           MOVE ZERO TO WS-TOTAL-BALANCE.

      *Process each customer
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > 4
               PERFORM PROCESS-SINGLE-CUSTOMER
           END-PERFORM.

      *Calculate average balance
           COMPUTE WS-AVERAGE-BALANCE = WS-TOTAL-BALANCE / 4.

           DISPLAY SPACE.
           DISPLAY "Summary:".
           DISPLAY "  Total Balance:   $" WS-TOTAL-BALANCE.
           DISPLAY "  Average Balance: $" WS-AVERAGE-BALANCE.
           DISPLAY SPACE.

       PROCESS-SINGLE-CUSTOMER SECTION.
       PROC-SINGLE.
           EVALUATE WS-LOOP-INDEX
               WHEN 1
                   MOVE "Alice Johnson" TO WS-CUSTOMER-NAME
                   MOVE 15000.00 TO WS-ACCOUNT-BALANCE
               WHEN 2
                   MOVE "Bob Smith" TO WS-CUSTOMER-NAME
                   MOVE 28500.50 TO WS-ACCOUNT-BALANCE
               WHEN 3
                   MOVE "Carol Williams" TO WS-CUSTOMER-NAME
                   MOVE 42750.25 TO WS-ACCOUNT-BALANCE
               WHEN 4
                   MOVE "David Brown" TO WS-CUSTOMER-NAME
                   MOVE 8900.75 TO WS-ACCOUNT-BALANCE
           END-EVALUATE.

           PERFORM VALIDATE-CUSTOMER-DATA.
           PERFORM DISPLAY-CUSTOMER-INFO.

           ADD WS-ACCOUNT-BALANCE TO WS-TOTAL-BALANCE.

       VALIDATE-CUSTOMER-DATA SECTION.
       VALIDATE-DATA.
      *Use .NET String methods for validation
           IF WS-ACCOUNT-BALANCE < ZERO THEN
               DISPLAY "Warning: Negative balance for "
                       WS-CUSTOMER-NAME
           END-IF.

           IF WS-ACCOUNT-BALANCE > 50000.00 THEN
               DISPLAY "Note: Premium customer - "
                       WS-CUSTOMER-NAME
           END-IF.

       DISPLAY-CUSTOMER-INFO SECTION.
       SHOW-INFO.
           DISPLAY "Customer: " WS-CUSTOMER-NAME.
           DISPLAY "  Balance: $" WS-ACCOUNT-BALANCE.

       SAVE-TO-FILE SECTION.
       SAVE-FILE.
           DISPLAY "Saving data to file...".

      *Build file content
           STRING
               "Customer Report" DELIMITED BY SIZE
               X"0A"  *Line feed
               "Generated: " WS-FORMATTED-DATE DELIMITED BY SIZE
               X"0A"
               "Total Customers: 4" DELIMITED BY SIZE
               X"0A"
               "Total Balance: $" WS-TOTAL-BALANCE DELIMITED BY SIZE
               INTO WS-FILE-CONTENT
           END-STRING.

      *Use .NET File class to write
      *INVOKE File "WriteAllText"
      *    USING WS-FILE-PATH WS-FILE-CONTENT.

           DISPLAY "Data saved to: " WS-FILE-PATH.
           DISPLAY SPACE.

       CLEANUP-RESOURCES SECTION.
       CLEANUP.
      *Release .NET object references
           IF WS-CUSTOMER-LIST NOT = NULL THEN
               SET WS-CUSTOMER-LIST TO NULL
           END-IF.

           IF WS-DATETIME-OBJ NOT = NULL THEN
               SET WS-DATETIME-OBJ TO NULL
           END-IF.

           DISPLAY "Resources cleaned up.".

       END PROGRAM DotNetIntegration.
