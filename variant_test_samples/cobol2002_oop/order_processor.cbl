       IDENTIFICATION DIVISION.
       CLASS-ID. OrderProcessor INHERITS Base.
      *COBOL-2002 ORDER PROCESSING WITH INHERITANCE

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS OrderProcessor
           CLASS Base.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OrderData PRIVATE.
           05 OrderId          PIC 9(12).
           05 CustomerId       PIC 9(10).
           05 OrderDate        PIC X(10).
           05 TotalAmount      PIC 9(9)V99.
           05 OrderStatus      PIC X(20).
           05 ItemCount        PIC 9(4).

       OBJECT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TaxRate          PIC V9999 VALUE .0825.
       01 WS-ShippingCost     PIC 9(5)V99.
       01 WS-GrandTotal       PIC 9(10)V99.

       PROCEDURE DIVISION.

       METHOD-ID. InitializeOrder.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-OrderId          PIC 9(12).
       01 LS-CustomerId       PIC 9(10).
       PROCEDURE DIVISION USING LS-OrderId LS-CustomerId.
           MOVE LS-OrderId TO OrderId.
           MOVE LS-CustomerId TO CustomerId.
           MOVE FUNCTION CURRENT-DATE(1:8) TO OrderDate.
           MOVE ZERO TO TotalAmount.
           MOVE 'PENDING' TO OrderStatus.
           MOVE ZERO TO ItemCount.
           EXIT METHOD.
       END METHOD InitializeOrder.

       METHOD-ID. AddItem.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-ItemPrice        PIC 9(7)V99.
       01 LS-Quantity         PIC 9(4).
       PROCEDURE DIVISION USING LS-ItemPrice LS-Quantity.
       LOCAL-STORAGE SECTION.
       01 LS-LineTotal        PIC 9(8)V99.
           COMPUTE LS-LineTotal = LS-ItemPrice * LS-Quantity
               ON SIZE ERROR
                   DISPLAY 'Error: Item price overflow'
               NOT ON SIZE ERROR
                   ADD LS-LineTotal TO TotalAmount
                   ADD LS-Quantity TO ItemCount
           END-COMPUTE.
           EXIT METHOD.
       END METHOD AddItem.

       METHOD-ID. CalculateTotal.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-FinalTotal       PIC 9(10)V99.
       PROCEDURE DIVISION RETURNING LS-FinalTotal.
       LOCAL-STORAGE SECTION.
       01 LS-TaxAmount        PIC 9(8)V99.
           PERFORM CalculateShipping.
           COMPUTE LS-TaxAmount = TotalAmount * WS-TaxRate
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.
           COMPUTE WS-GrandTotal =
               TotalAmount + LS-TaxAmount + WS-ShippingCost.
           MOVE WS-GrandTotal TO LS-FinalTotal.
           EXIT METHOD.
       END METHOD CalculateTotal.

       METHOD-ID. CalculateShipping.
           IF TotalAmount < 50.00
               MOVE 9.99 TO WS-ShippingCost
           ELSE
               IF TotalAmount < 100.00
                   MOVE 5.99 TO WS-ShippingCost
               ELSE
                   MOVE ZERO TO WS-ShippingCost
               END-IF
           END-IF.
       END METHOD CalculateShipping.

       METHOD-ID. ProcessOrder.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-Success          PIC X.
       PROCEDURE DIVISION RETURNING LS-Success.
           PERFORM ValidateOrder.
           IF OrderStatus = 'VALIDATED'
               PERFORM ApproveOrder
               MOVE 'Y' TO LS-Success
           ELSE
               DISPLAY 'Order validation failed'
               MOVE 'N' TO LS-Success
           END-IF.
           EXIT METHOD.
       END METHOD ProcessOrder.

       METHOD-ID. ValidateOrder.
           IF ItemCount > ZERO AND TotalAmount > ZERO
               IF CustomerId > ZERO
                   MOVE 'VALIDATED' TO OrderStatus
               ELSE
                   MOVE 'INVALID_CUSTOMER' TO OrderStatus
               END-IF
           ELSE
               MOVE 'INVALID_ITEMS' TO OrderStatus
           END-IF.
       END METHOD ValidateOrder.

       METHOD-ID. ApproveOrder.
           MOVE 'APPROVED' TO OrderStatus.
           DISPLAY 'Order ' OrderId ' approved for customer '
                   CustomerId.
           DISPLAY 'Total amount: ' TotalAmount.
           DISPLAY 'Item count: ' ItemCount.
       END METHOD ApproveOrder.

       METHOD-ID. CancelOrder.
       PROCEDURE DIVISION.
           IF OrderStatus = 'PENDING' OR OrderStatus = 'VALIDATED'
               MOVE 'CANCELLED' TO OrderStatus
               DISPLAY 'Order ' OrderId ' has been cancelled'
           ELSE
               DISPLAY 'Cannot cancel order in status: ' OrderStatus
           END-IF.
           EXIT METHOD.
       END METHOD CancelOrder.

       METHOD-ID. GetOrderStatus.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-Status           PIC X(20).
       PROCEDURE DIVISION RETURNING LS-Status.
           MOVE OrderStatus TO LS-Status.
           EXIT METHOD.
       END METHOD GetOrderStatus.

       END OBJECT.
       END CLASS OrderProcessor.
