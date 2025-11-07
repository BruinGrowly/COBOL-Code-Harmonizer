      $SET SOURCEFORMAT"VARIABLE"
      $SET ANS85
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ScreenHandler.
      *MICRO FOCUS - ENHANCED SCREEN SECTION
      *Demonstrates Micro Focus screen handling extensions

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *Menu data
       01  WS-MENU-CHOICE              PIC 9 VALUE ZERO.
       01  WS-CONTINUE-FLAG            PIC X VALUE "Y".

      *Customer input data
       01  WS-CUSTOMER-INPUT.
           05  WS-INPUT-ID             PIC 9(8).
           05  WS-INPUT-NAME           PIC X(40).
           05  WS-INPUT-ADDRESS        PIC X(50).
           05  WS-INPUT-CITY           PIC X(30).
           05  WS-INPUT-STATE          PIC XX.
           05  WS-INPUT-ZIP            PIC X(10).
           05  WS-INPUT-PHONE          PIC X(14).

      *Display data
       01  WS-DISPLAY-DATA.
           05  WS-DISP-MESSAGE         PIC X(60).
           05  WS-DISP-STATUS          PIC X(20).
           05  WS-RECORD-COUNT         PIC 9(6) VALUE ZERO.

      *Screen colors (Micro Focus extensions)
       01  WS-COLOR-CODES.
           05  COLOR-NORMAL            PIC 9 VALUE 7.
           05  COLOR-HIGHLIGHT         PIC 9 VALUE 15.
           05  COLOR-ERROR             PIC 9 VALUE 12.
           05  COLOR-SUCCESS           PIC 9 VALUE 10.

       SCREEN SECTION.

      *Main menu screen
       01  MAIN-MENU-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COLUMN 25 VALUE
               "╔════════════════════════════════╗"
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 3 COLUMN 25 VALUE
               "║  CUSTOMER MANAGEMENT SYSTEM   ║"
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 4 COLUMN 25 VALUE
               "╚════════════════════════════════╝"
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 6 COLUMN 25 VALUE "1. Add New Customer".
           05  LINE 7 COLUMN 25 VALUE "2. Search Customer".
           05  LINE 8 COLUMN 25 VALUE "3. Update Customer".
           05  LINE 9 COLUMN 25 VALUE "4. Delete Customer".
           05  LINE 10 COLUMN 25 VALUE "5. List All Customers".
           05  LINE 11 COLUMN 25 VALUE "6. Exit".
           05  LINE 13 COLUMN 25 VALUE "Enter choice (1-6): ".
           05  LINE 13 COLUMN 45 PIC 9 TO WS-MENU-CHOICE
               FOREGROUND-COLOR COLOR-HIGHLIGHT
               BACKGROUND-COLOR 1
               REVERSE-VIDEO.

      *Customer entry screen
       01  CUSTOMER-ENTRY-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COLUMN 20 VALUE
               "═══ ADD NEW CUSTOMER ═══"
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 4 COLUMN 10 VALUE "Customer ID:".
           05  LINE 4 COLUMN 25 PIC 9(8) TO WS-INPUT-ID
               REQUIRED.
           05  LINE 6 COLUMN 10 VALUE "Name:".
           05  LINE 6 COLUMN 25 PIC X(40) TO WS-INPUT-NAME
               REQUIRED.
           05  LINE 8 COLUMN 10 VALUE "Address:".
           05  LINE 8 COLUMN 25 PIC X(50) TO WS-INPUT-ADDRESS.
           05  LINE 10 COLUMN 10 VALUE "City:".
           05  LINE 10 COLUMN 25 PIC X(30) TO WS-INPUT-CITY.
           05  LINE 12 COLUMN 10 VALUE "State:".
           05  LINE 12 COLUMN 25 PIC XX TO WS-INPUT-STATE
               UPPER.
           05  LINE 12 COLUMN 35 VALUE "ZIP:".
           05  LINE 12 COLUMN 40 PIC X(10) TO WS-INPUT-ZIP.
           05  LINE 14 COLUMN 10 VALUE "Phone:".
           05  LINE 14 COLUMN 25 PIC X(14) TO WS-INPUT-PHONE.
           05  LINE 18 COLUMN 10 VALUE
               "Press ENTER to save, ESC to cancel"
               FOREGROUND-COLOR COLOR-NORMAL.

      *Search screen
       01  SEARCH-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COLUMN 20 VALUE
               "═══ SEARCH CUSTOMER ═══"
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 5 COLUMN 10 VALUE
               "Enter Customer ID to search:".
           05  LINE 5 COLUMN 40 PIC 9(8) TO WS-INPUT-ID
               REQUIRED
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 8 COLUMN 10 VALUE
               "Press ENTER to search, ESC to cancel".

      *Results display screen
       01  RESULTS-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COLUMN 20 VALUE
               "═══ SEARCH RESULTS ═══"
               FOREGROUND-COLOR COLOR-HIGHLIGHT.
           05  LINE 5 COLUMN 10 VALUE "Customer ID:".
           05  LINE 5 COLUMN 25 PIC 9(8) FROM WS-INPUT-ID.
           05  LINE 6 COLUMN 10 VALUE "Name:".
           05  LINE 6 COLUMN 25 PIC X(40) FROM WS-INPUT-NAME.
           05  LINE 7 COLUMN 10 VALUE "Address:".
           05  LINE 7 COLUMN 25 PIC X(50) FROM WS-INPUT-ADDRESS.
           05  LINE 8 COLUMN 10 VALUE "City:".
           05  LINE 8 COLUMN 25 PIC X(30) FROM WS-INPUT-CITY.
           05  LINE 9 COLUMN 10 VALUE "State:".
           05  LINE 9 COLUMN 25 PIC XX FROM WS-INPUT-STATE.
           05  LINE 9 COLUMN 35 VALUE "ZIP:".
           05  LINE 9 COLUMN 40 PIC X(10) FROM WS-INPUT-ZIP.
           05  LINE 10 COLUMN 10 VALUE "Phone:".
           05  LINE 10 COLUMN 25 PIC X(14) FROM WS-INPUT-PHONE.
           05  LINE 14 COLUMN 10 PIC X(60) FROM WS-DISP-MESSAGE
               FOREGROUND-COLOR COLOR-SUCCESS.
           05  LINE 16 COLUMN 10 VALUE
               "Press any key to continue...".

      *Status message screen
       01  STATUS-MESSAGE-SCREEN.
           05  LINE 20 COLUMN 10 PIC X(60) FROM WS-DISP-MESSAGE.
           05  LINE 21 COLUMN 10 VALUE
               "Press any key to continue...".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SYSTEM.
           PERFORM MENU-LOOP UNTIL WS-MENU-CHOICE = 6.
           PERFORM CLEANUP-SYSTEM.
           STOP RUN.

       INITIALIZE-SYSTEM SECTION.
       INIT-SYS.
           MOVE "System initialized" TO WS-DISP-MESSAGE.
           MOVE "Ready" TO WS-DISP-STATUS.

       MENU-LOOP SECTION.
       SHOW-MENU.
           DISPLAY MAIN-MENU-SCREEN.
           ACCEPT MAIN-MENU-SCREEN.

           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   PERFORM ADD-CUSTOMER-PROCESS
               WHEN 2
                   PERFORM SEARCH-CUSTOMER-PROCESS
               WHEN 3
                   PERFORM UPDATE-CUSTOMER-PROCESS
               WHEN 4
                   PERFORM DELETE-CUSTOMER-PROCESS
               WHEN 5
                   PERFORM LIST-CUSTOMERS-PROCESS
               WHEN 6
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid selection. Try again." TO
                       WS-DISP-MESSAGE
                   PERFORM SHOW-STATUS-MESSAGE
           END-EVALUATE.

       ADD-CUSTOMER-PROCESS SECTION.
       ADD-CUST.
           MOVE SPACES TO WS-CUSTOMER-INPUT.
           DISPLAY CUSTOMER-ENTRY-SCREEN.
           ACCEPT CUSTOMER-ENTRY-SCREEN.

           IF WS-INPUT-ID > ZERO AND WS-INPUT-NAME NOT = SPACES THEN
               PERFORM SAVE-CUSTOMER-RECORD
               MOVE "Customer added successfully!" TO
                   WS-DISP-MESSAGE
               ADD 1 TO WS-RECORD-COUNT
           ELSE
               MOVE "Invalid data. Customer not added." TO
                   WS-DISP-MESSAGE
           END-IF.

           PERFORM SHOW-STATUS-MESSAGE.

       SAVE-CUSTOMER-RECORD SECTION.
       SAVE-CUST.
      *In real application, would write to file/database
           DISPLAY "Saving customer record..." AT LINE 22 COLUMN 10.

      *Simulate save operation
           CALL "C$SLEEP" USING BY CONTENT 1.

       SEARCH-CUSTOMER-PROCESS SECTION.
       SEARCH-CUST.
           MOVE ZERO TO WS-INPUT-ID.
           DISPLAY SEARCH-SCREEN.
           ACCEPT SEARCH-SCREEN.

           IF WS-INPUT-ID > ZERO THEN
               PERFORM FETCH-CUSTOMER-RECORD
               PERFORM DISPLAY-SEARCH-RESULTS
           ELSE
               MOVE "Invalid Customer ID" TO WS-DISP-MESSAGE
               PERFORM SHOW-STATUS-MESSAGE
           END-IF.

       FETCH-CUSTOMER-RECORD SECTION.
       FETCH-CUST.
      *Simulate database lookup
           MOVE "John Smith" TO WS-INPUT-NAME.
           MOVE "123 Main Street" TO WS-INPUT-ADDRESS.
           MOVE "New York" TO WS-INPUT-CITY.
           MOVE "NY" TO WS-INPUT-STATE.
           MOVE "10001" TO WS-INPUT-ZIP.
           MOVE "(212)555-1234" TO WS-INPUT-PHONE.

           MOVE "Customer found" TO WS-DISP-MESSAGE.

       DISPLAY-SEARCH-RESULTS SECTION.
       SHOW-RESULTS.
           DISPLAY RESULTS-SCREEN.
           ACCEPT STATUS-MESSAGE-SCREEN.

       UPDATE-CUSTOMER-PROCESS SECTION.
       UPDATE-CUST.
           MOVE "Update functionality not yet implemented" TO
               WS-DISP-MESSAGE.
           PERFORM SHOW-STATUS-MESSAGE.

       DELETE-CUSTOMER-PROCESS SECTION.
       DELETE-CUST.
           MOVE "Delete functionality not yet implemented" TO
               WS-DISP-MESSAGE.
           PERFORM SHOW-STATUS-MESSAGE.

       LIST-CUSTOMERS-PROCESS SECTION.
       LIST-CUST.
           MOVE "List functionality not yet implemented" TO
               WS-DISP-MESSAGE.
           PERFORM SHOW-STATUS-MESSAGE.

       SHOW-STATUS-MESSAGE SECTION.
       SHOW-STATUS.
           DISPLAY STATUS-MESSAGE-SCREEN.
           ACCEPT STATUS-MESSAGE-SCREEN.

       CLEANUP-SYSTEM SECTION.
       CLEANUP.
           DISPLAY "Exiting system..." AT LINE 23 COLUMN 25.
           CALL "C$SLEEP" USING BY CONTENT 1.

       END PROGRAM ScreenHandler.
