       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY.
       DATE-WRITTEN. 1977-11-20.
      *COBOL-74 INVENTORY CONTROL SYSTEM
      *CLASSIC MAINFRAME BATCH PROCESSING

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-MASTER ASSIGN TO INVMAST
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS ITEM-CODE.
           SELECT ORDER-FILE ASSIGN TO ORDFILE.
           SELECT REPORT-FILE ASSIGN TO RPTFILE.

       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-MASTER.
       01 INV-RECORD.
           05 ITEM-CODE        PIC X(8).
           05 ITEM-DESC        PIC X(30).
           05 ITEM-QTY         PIC 9(6).
           05 ITEM-REORDER     PIC 9(6).
           05 ITEM-COST        PIC 9(5)V99.

       FD ORDER-FILE.
       01 ORDER-RECORD.
           05 ORD-ITEM         PIC X(8).
           05 ORD-QTY          PIC 9(6).
           05 ORD-TYPE         PIC X.

       FD REPORT-FILE.
       01 REPORT-LINE         PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FLAGS.
           05 WS-EOF           PIC X VALUE 'N'.
           05 WS-FOUND         PIC X.
       01 WS-COUNTS.
           05 WS-ORDERS        PIC 9(5) VALUE ZERO.
           05 WS-REORDERS      PIC 9(5) VALUE ZERO.
       01 WS-NEW-QTY          PIC 9(6).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM PROCESS-ORDERS
               UNTIL WS-EOF = 'Y'.
           PERFORM GENERATE-REPORT.
           PERFORM TERMINATE-PROGRAM.
           STOP RUN.

       INITIALIZE-PROGRAM.
           OPEN I-O INVENTORY-MASTER.
           OPEN INPUT ORDER-FILE.
           OPEN OUTPUT REPORT-FILE.
           PERFORM READ-ORDER.

       READ-ORDER.
           READ ORDER-FILE
               AT END MOVE 'Y' TO WS-EOF.

       PROCESS-ORDERS.
      *NO SCOPE TERMINATORS - CLASSIC COBOL-74
           IF WS-EOF = 'N'
               ADD 1 TO WS-ORDERS
               PERFORM FIND-ITEM
               IF WS-FOUND = 'Y'
                   IF ORD-TYPE = 'R'
                       PERFORM RECEIVE-ITEM
                   ELSE
                       PERFORM SHIP-ITEM
               ELSE
                   PERFORM ITEM-NOT-FOUND
               PERFORM READ-ORDER.

       FIND-ITEM.
           MOVE 'N' TO WS-FOUND.
           MOVE ORD-ITEM TO ITEM-CODE.
           READ INVENTORY-MASTER
               INVALID KEY CONTINUE
               NOT INVALID KEY MOVE 'Y' TO WS-FOUND.

       RECEIVE-ITEM.
           ADD ORD-QTY TO ITEM-QTY.
           REWRITE INV-RECORD.
           DISPLAY 'RECEIVED: ' ITEM-CODE
                   ' QTY: ' ORD-QTY
                   ' NEW BAL: ' ITEM-QTY.

       SHIP-ITEM.
           IF ITEM-QTY >= ORD-QTY
               SUBTRACT ORD-QTY FROM ITEM-QTY
               REWRITE INV-RECORD
               PERFORM CHECK-REORDER
               DISPLAY 'SHIPPED: ' ITEM-CODE
                       ' QTY: ' ORD-QTY
                       ' NEW BAL: ' ITEM-QTY
           ELSE
               PERFORM INSUFFICIENT-STOCK.

       CHECK-REORDER.
           IF ITEM-QTY < ITEM-REORDER
               ADD 1 TO WS-REORDERS
               DISPLAY 'REORDER NEEDED: ' ITEM-CODE
                       ' QTY: ' ITEM-QTY
                       ' REORDER: ' ITEM-REORDER.

       INSUFFICIENT-STOCK.
           DISPLAY 'INSUFFICIENT STOCK: ' ITEM-CODE
                   ' AVAILABLE: ' ITEM-QTY
                   ' REQUESTED: ' ORD-QTY.

       ITEM-NOT-FOUND.
           DISPLAY 'ITEM NOT FOUND: ' ORD-ITEM.

       GENERATE-REPORT.
           MOVE 'INVENTORY CONTROL REPORT' TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO REPORT-LINE.
           WRITE REPORT-LINE.
           STRING 'ORDERS PROCESSED: ' WS-ORDERS
               DELIMITED BY SIZE INTO REPORT-LINE.
           WRITE REPORT-LINE.
           STRING 'REORDERS NEEDED: ' WS-REORDERS
               DELIMITED BY SIZE INTO REPORT-LINE.
           WRITE REPORT-LINE.

       TERMINATE-PROGRAM.
           CLOSE INVENTORY-MASTER.
           CLOSE ORDER-FILE.
           CLOSE REPORT-FILE.
