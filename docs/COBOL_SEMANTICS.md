# COBOL Semantic Analysis - LJPW Framework Application

**Version**: 1.0
**Date**: 2025-11-07
**Status**: Research & Design Document

---

## Table of Contents

1. [Introduction](#introduction)
2. [COBOL Language Structure](#cobol-language-structure)
3. [COBOL Verbs and LJPW Mapping](#cobol-verbs-and-ljpw-mapping)
4. [COBOL-Specific Semantic Patterns](#cobol-specific-semantic-patterns)
5. [Procedure Naming Conventions](#procedure-naming-conventions)
6. [Real-World Examples](#real-world-examples)
7. [Edge Cases and Challenges](#edge-cases-and-challenges)

---

## Introduction

This document details how the LJPW (Love, Justice, Power, Wisdom) semantic framework applies specifically to COBOL programming language constructs.

**Goal**: Map every COBOL verb and naming pattern to semantic coordinates, enabling detection of semantic disharmony in legacy COBOL codebases.

---

## COBOL Language Structure

### The Four Divisions

```cobol
IDENTIFICATION DIVISION.
    PROGRAM-ID. CUSTOMER-MANAGER.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT CUSTOMER-FILE...

DATA DIVISION.
    FILE SECTION.
    FD CUSTOMER-FILE.
    01 CUSTOMER-RECORD.
        05 CUST-ID          PIC 9(6).
        05 CUST-NAME        PIC X(30).

    WORKING-STORAGE SECTION.
    01 WS-VARIABLES.
        05 WS-COUNTER       PIC 9(3) VALUE 0.

PROCEDURE DIVISION.
    MAIN-LOGIC.
        PERFORM INITIALIZATION.
        PERFORM PROCESS-CUSTOMERS.
        PERFORM CLEANUP.
        STOP RUN.
```

**Semantic Analysis Focus**: We analyze the **PROCEDURE DIVISION** only, where actual logic resides.

---

## COBOL Verbs and LJPW Mapping

### Complete Verb Taxonomy (120+ Verbs)

#### WISDOM-Dominant Verbs (Information/Knowledge)

**Pure Retrieval** - (L=0.1, J=0.2, P=0.0, W=0.7)
```cobol
READ CUSTOMER-FILE INTO CUSTOMER-RECORD
    AT END MOVE 'Y' TO EOF-FLAG
END-READ
```

**User Input** - (L=0.1, J=0.1, P=0.0, W=0.8)
```cobol
ACCEPT WS-USER-INPUT FROM CONSOLE
ACCEPT WS-DATE FROM DATE YYYYMMDD
```

**Calculation** - (L=0.0, J=0.3, P=0.1, W=0.6)
```cobol
COMPUTE TOTAL-AMOUNT = PRICE * QUANTITY + TAX
ADD ITEM-PRICE TO RUNNING-TOTAL
SUBTRACT DISCOUNT FROM TOTAL-PRICE
MULTIPLY QUANTITY BY UNIT-PRICE GIVING LINE-TOTAL
DIVIDE TOTAL-SALES BY DAYS-COUNT GIVING AVERAGE
```

**String Analysis** - (L=0.0, J=0.2, P=0.0, W=0.8)
```cobol
INSPECT CUSTOMER-NAME TALLYING WS-COUNT FOR ALL 'A'
INSPECT INPUT-STRING REPLACING ALL SPACES BY '-'
STRING FIRST-NAME DELIMITED BY SIZE
       " " DELIMITED BY SIZE
       LAST-NAME DELIMITED BY SIZE
       INTO FULL-NAME
END-STRING
UNSTRING FULL-NAME DELIMITED BY SPACE
    INTO FIRST-NAME LAST-NAME
END-UNSTRING
```

**Display/Output** - (L=0.3, J=0.1, P=0.1, W=0.5)
```cobol
DISPLAY "Customer ID: " CUST-ID
DISPLAY "Processing complete" UPON CONSOLE
```

#### JUSTICE-Dominant Verbs (Validation/Correctness)

**Conditional Logic** - (L=0.0, J=0.8, P=0.1, W=0.1)
```cobol
IF CUSTOMER-STATUS = 'ACTIVE'
    PERFORM PROCESS-ACTIVE-CUSTOMER
ELSE
    PERFORM ARCHIVE-CUSTOMER
END-IF

EVALUATE TRUE
    WHEN BALANCE < 0
        PERFORM HANDLE-OVERDRAWN
    WHEN BALANCE = 0
        PERFORM HANDLE-ZERO-BALANCE
    WHEN BALANCE > 1000000
        PERFORM HANDLE-HIGH-BALANCE
    WHEN OTHER
        PERFORM HANDLE-NORMAL-BALANCE
END-EVALUATE
```

**Searching** - (L=0.0, J=0.7, P=0.0, W=0.3)
```cobol
SEARCH CUSTOMER-TABLE
    AT END MOVE 'NOT-FOUND' TO WS-STATUS
    WHEN CUST-ID(IDX) = WS-SEARCH-ID
        MOVE CUST-NAME(IDX) TO WS-RESULT
END-SEARCH

SEARCH ALL SORTED-TABLE
    WHEN KEY-FIELD(IDX) = SEARCH-KEY
        PERFORM FOUND-LOGIC
END-SEARCH
```

**Validation** - (L=0.0, J=0.9, P=0.0, W=0.1)
```cobol
*> Enterprise COBOL XML validation
XML VALIDATE WS-XML-DOC
    ON EXCEPTION
        DISPLAY 'Validation failed'
END-XML
```

#### POWER-Dominant Verbs (State Modification)

**Assignment** - (L=0.0, J=0.1, P=0.6, W=0.3)
```cobol
MOVE CUSTOMER-ID TO WS-CURRENT-ID
MOVE SPACES TO CUSTOMER-NAME
MOVE ZEROS TO ACCOUNT-BALANCE
MOVE 'PROCESSED' TO RECORD-STATUS
```

**File Writing** - (L=0.1, J=0.1, P=0.7, W=0.1)
```cobol
WRITE CUSTOMER-RECORD
    INVALID KEY DISPLAY 'Write failed'
END-WRITE

REWRITE CUSTOMER-RECORD
    INVALID KEY DISPLAY 'Update failed'
END-REWRITE

DELETE CUSTOMER-FILE RECORD
    INVALID KEY DISPLAY 'Delete failed'
END-DELETE
```

**File Management** - (L=0.1, J=0.1, P=0.7, W=0.1)
```cobol
OPEN INPUT CUSTOMER-FILE
OPEN OUTPUT REPORT-FILE
OPEN I-O MASTER-FILE
OPEN EXTEND LOG-FILE

CLOSE CUSTOMER-FILE
CLOSE REPORT-FILE WITH LOCK
```

**Data Initialization** - (L=0.0, J=0.1, P=0.7, W=0.2)
```cobol
INITIALIZE CUSTOMER-RECORD
INITIALIZE WS-COUNTERS REPLACING NUMERIC BY 0
SET WS-POINTER TO ADDRESS OF CUSTOMER-RECORD
SET WS-INDEX TO 1
```

**Release/Free** - (L=0.0, J=0.1, P=0.8, W=0.1)
```cobol
RELEASE SORT-RECORD
FREE DYNAMIC-TABLE
CANCEL 'SUBPROGRAM-NAME'
```

#### LOVE-Dominant Verbs (Connection/Communication)

**Program Linkage** - (L=0.5, J=0.1, P=0.3, W=0.1)
```cobol
CALL 'VALIDATION-MODULE' USING CUSTOMER-RECORD WS-STATUS
CALL WS-PROGRAM-NAME USING BY REFERENCE DATA-AREA
CALL 'LOGGER' USING BY CONTENT LOG-MESSAGE

*> Dynamic call
CALL PROGRAM-NAME USING PARAMETERS
    ON EXCEPTION DISPLAY 'Call failed'
    NOT ON EXCEPTION CONTINUE
END-CALL
```

**Object-Oriented** - (L=0.6, J=0.1, P=0.2, W=0.1)
```cobol
*> COBOL-2002 OO features
INVOKE CUSTOMER-OBJECT "getName" RETURNING WS-NAME
INVOKE SELF "calculateTotal"
```

**File Merging** - (L=0.5, J=0.2, P=0.2, W=0.1)
```cobol
MERGE SORT-FILE
    ON ASCENDING KEY CUSTOMER-ID
    USING INPUT-FILE-1 INPUT-FILE-2
    OUTPUT PROCEDURE IS MERGE-PROCESSING
```

**Sorting** - (L=0.4, J=0.2, P=0.2, W=0.2)
```cobol
SORT SORT-FILE
    ON ASCENDING KEY SORT-KEY
    INPUT PROCEDURE IS SORT-INPUT-PROC
    OUTPUT PROCEDURE IS SORT-OUTPUT-PROC
```

#### BALANCED/CONTROL-FLOW Verbs

**Procedure Invocation** - (L=0.2, J=0.2, P=0.4, W=0.2)
```cobol
PERFORM CALCULATE-TOTALS
PERFORM PROCESS-RECORD 10 TIMES
PERFORM PROCESS-LOOP UNTIL EOF-FLAG = 'Y'
PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 100
    DISPLAY CUSTOMER-NAME(IDX)
END-PERFORM

PERFORM SECTION-NAME THRU SECTION-END
```

**Continuation** - (L=0.0, J=0.5, P=0.5, W=0.0)
```cobol
CONTINUE
GO TO PARAGRAPH-NAME
GO TO SECTION-END DEPENDING ON WS-OPTION
EXIT
EXIT PARAGRAPH
EXIT SECTION
EXIT PROGRAM
```

**Termination** - (L=0.0, J=0.2, P=0.6, W=0.2)
```cobol
STOP RUN
STOP RUN RETURNING 0
GOBACK
```

---

## COBOL-Specific Semantic Patterns

### Pattern 1: File I/O Sequences

**Common Pattern**:
```cobol
PROCESS-FILE-RECORDS.
    OPEN INPUT CUSTOMER-FILE          *> Power: 0.7
    PERFORM UNTIL EOF-FLAG = 'Y'      *> Justice: 0.8
        READ CUSTOMER-FILE            *> Wisdom: 0.7
            AT END MOVE 'Y' TO EOF-FLAG
        END-READ
        IF NOT EOF-FLAG = 'Y'
            PERFORM PROCESS-RECORD    *> Depends on content
        END-IF
    END-PERFORM
    CLOSE CUSTOMER-FILE               *> Power: 0.7
```

**Semantic Profile**: Mixed (P=0.5, W=0.3, J=0.2)
- Power operations (OPEN, CLOSE)
- Wisdom operations (READ)
- Justice operations (IF, PERFORM UNTIL)

### Pattern 2: Validation Sequences

**Common Pattern**:
```cobol
VALIDATE-CUSTOMER-DATA.
    IF CUST-ID = SPACES OR ZEROS      *> Justice: 0.8
        MOVE 'INVALID-ID' TO WS-ERROR
        MOVE 'N' TO WS-VALID-FLAG
    END-IF
    IF CUST-NAME = SPACES             *> Justice: 0.8
        MOVE 'INVALID-NAME' TO WS-ERROR
        MOVE 'N' TO WS-VALID-FLAG
    END-IF
    IF ACCOUNT-BALANCE < 0            *> Justice: 0.8
        MOVE 'NEGATIVE-BAL' TO WS-ERROR
        MOVE 'N' TO WS-VALID-FLAG
    END-IF
```

**Semantic Profile**: Justice-dominant (J=0.7, P=0.2, W=0.1)

### Pattern 3: Calculation Sequences

**Common Pattern**:
```cobol
CALCULATE-ORDER-TOTAL.
    COMPUTE SUBTOTAL = QUANTITY * UNIT-PRICE     *> Wisdom: 0.6
    COMPUTE TAX-AMOUNT = SUBTOTAL * TAX-RATE     *> Wisdom: 0.6
    COMPUTE TOTAL = SUBTOTAL + TAX-AMOUNT        *> Wisdom: 0.6
    MOVE TOTAL TO ORDER-TOTAL                    *> Power: 0.6
```

**Semantic Profile**: Wisdom-dominant with Power (W=0.5, P=0.3, J=0.2)

### Pattern 4: Database Operations (SQL COBOL)

```cobol
UPDATE-CUSTOMER-RECORD.
    EXEC SQL                                      *> Power: 0.8
        UPDATE CUSTOMERS
        SET LAST_UPDATED = CURRENT_TIMESTAMP,
            STATUS = :WS-NEW-STATUS
        WHERE CUST_ID = :WS-CUST-ID
    END-EXEC

    IF SQLCODE = 0                                *> Justice: 0.8
        MOVE 'SUCCESS' TO WS-STATUS
    ELSE
        MOVE 'FAILED' TO WS-STATUS
        PERFORM LOG-SQL-ERROR
    END-IF
```

**Semantic Profile**: Power + Justice (P=0.6, J=0.3, W=0.1)

---

## Procedure Naming Conventions

### Intent-Revealing Keywords in COBOL

#### High-Wisdom Keywords
```
GET-*           → (0.1, 0.1, 0.0, 0.8)
FETCH-*         → (0.1, 0.1, 0.0, 0.8)
READ-*          → (0.1, 0.2, 0.0, 0.7)
RETRIEVE-*      → (0.1, 0.1, 0.0, 0.8)
CALCULATE-*     → (0.0, 0.3, 0.1, 0.6)
COMPUTE-*       → (0.0, 0.3, 0.1, 0.6)
DISPLAY-*       → (0.3, 0.1, 0.0, 0.6)
SHOW-*          → (0.2, 0.1, 0.0, 0.7)
PRINT-*         → (0.2, 0.1, 0.2, 0.5)
FORMAT-*        → (0.1, 0.2, 0.1, 0.6)
```

**Examples**:
- `GET-CUSTOMER-RECORD` → Should READ, not DELETE
- `CALCULATE-TOTAL-AMOUNT` → Should COMPUTE, not WRITE
- `DISPLAY-REPORT-LINE` → Should DISPLAY, not UPDATE

#### High-Justice Keywords
```
VALIDATE-*      → (0.0, 0.8, 0.1, 0.1)
VERIFY-*        → (0.0, 0.8, 0.1, 0.1)
CHECK-*         → (0.0, 0.7, 0.1, 0.2)
TEST-*          → (0.0, 0.8, 0.1, 0.1)
AUDIT-*         → (0.0, 0.7, 0.1, 0.2)
CONFIRM-*       → (0.0, 0.7, 0.2, 0.1)
INSPECT-*       → (0.0, 0.6, 0.0, 0.4)
SEARCH-*        → (0.0, 0.7, 0.0, 0.3)
FIND-*          → (0.0, 0.5, 0.0, 0.5)
```

**Examples**:
- `VALIDATE-INPUT-DATA` → Should IF/EVALUATE, not DELETE
- `CHECK-ACCOUNT-STATUS` → Should validate, not modify
- `VERIFY-BALANCE` → Should check, not update

#### High-Power Keywords
```
UPDATE-*        → (0.0, 0.2, 0.7, 0.1)
DELETE-*        → (0.0, 0.2, 0.8, 0.0)
REMOVE-*        → (0.0, 0.2, 0.8, 0.0)
CREATE-*        → (0.1, 0.2, 0.6, 0.1)
INSERT-*        → (0.0, 0.2, 0.7, 0.1)
WRITE-*         → (0.1, 0.1, 0.7, 0.1)
SET-*           → (0.0, 0.1, 0.8, 0.1)
INITIALIZE-*    → (0.0, 0.1, 0.7, 0.2)
RESET-*         → (0.0, 0.1, 0.7, 0.2)
CLEAR-*         → (0.0, 0.1, 0.8, 0.1)
OPEN-*          → (0.1, 0.1, 0.7, 0.1)
CLOSE-*         → (0.1, 0.1, 0.7, 0.1)
PURGE-*         → (0.0, 0.1, 0.9, 0.0)
```

**Examples**:
- `UPDATE-CUSTOMER-BALANCE` → Should MOVE/REWRITE, not just READ
- `DELETE-OLD-RECORDS` → Should DELETE, not just mark as deleted
- `CREATE-NEW-ACCOUNT` → Should WRITE, not just validate

#### High-Love Keywords
```
LINK-*          → (0.6, 0.1, 0.2, 0.1)
CONNECT-*       → (0.7, 0.1, 0.1, 0.1)
MERGE-*         → (0.5, 0.2, 0.2, 0.1)
COMBINE-*       → (0.5, 0.1, 0.3, 0.1)
JOIN-*          → (0.6, 0.1, 0.2, 0.1)
ASSOCIATE-*     → (0.6, 0.1, 0.2, 0.1)
CALL-*          → (0.5, 0.1, 0.3, 0.1)
```

**Examples**:
- `LINK-CUSTOMER-ACCOUNT` → Should CALL or establish relationship
- `MERGE-TRANSACTION-FILES` → Should MERGE, not just READ

#### Ambiguous/Vague Keywords (Should be Avoided)
```
PROCESS-*       → (0.1, 0.2, 0.4, 0.3) - Too vague!
HANDLE-*        → (0.1, 0.2, 0.4, 0.3) - Too vague!
MANAGE-*        → (0.2, 0.2, 0.3, 0.3) - Too vague!
EXECUTE-*       → (0.1, 0.2, 0.5, 0.2) - Too vague!
DO-*            → (0.1, 0.1, 0.6, 0.2) - Too vague!
```

**These indicate code smell** - procedure likely does too many things.

---

## Real-World Examples

### Example 1: Semantic Harmony ✓

```cobol
GET-CUSTOMER-DETAILS.
    *> Intent: (L=0.1, J=0.1, P=0.0, W=0.8) - GET = Wisdom

    MOVE SPACES TO CUSTOMER-RECORD.
    READ CUSTOMER-FILE
        KEY IS CUST-ID
        INVALID KEY MOVE 'NOT-FOUND' TO WS-STATUS
    END-READ.

    IF WS-STATUS = 'NOT-FOUND'
        DISPLAY 'Customer not found'
    ELSE
        MOVE CUSTOMER-RECORD TO WS-OUTPUT-RECORD
    END-IF.

    *> Execution: (L=0.1, J=0.3, P=0.1, W=0.5) - Mostly Wisdom
    *> Disharmony Score: 0.35 - HARMONIOUS ✓
```

### Example 2: Minor Disharmony ⚠️

```cobol
VALIDATE-CUSTOMER-RECORD.
    *> Intent: (L=0.0, J=0.8, P=0.1, W=0.1) - VALIDATE = Justice

    IF CUST-ID = SPACES
        MOVE 'INVALID' TO WS-STATUS
        MOVE 0 TO RETURN-CODE
    ELSE
        MOVE 'VALID' TO WS-STATUS
        MOVE 1 TO RETURN-CODE
        *> Side effect: updating last-validated timestamp
        ACCEPT WS-TIMESTAMP FROM TIME
        MOVE WS-TIMESTAMP TO LAST-VALIDATED
    END-IF.

    *> Execution: (L=0.0, J=0.6, P=0.3, W=0.1) - Justice + Power
    *> Disharmony Score: 0.43 - MINOR DRIFT ⚠️
    *> Issue: Validator should not modify state
```

### Example 3: Significant Disharmony 🔴

```cobol
GET-CUSTOMER-BALANCE.
    *> Intent: (L=0.1, J=0.1, P=0.0, W=0.8) - GET = Wisdom

    READ CUSTOMER-FILE
        KEY IS CUST-ID
        INVALID KEY MOVE ZEROS TO ACCOUNT-BALANCE
    END-READ.

    *> Wait, we're DELETING?!
    DELETE CUSTOMER-FILE RECORD
        INVALID KEY DISPLAY 'Delete failed'
    END-DELETE.

    MOVE ACCOUNT-BALANCE TO WS-RESULT.

    *> Execution: (L=0.0, J=0.2, P=0.6, W=0.2) - Power-dominant!
    *> Disharmony Score: 1.12 - SIGNIFICANT DISHARMONY 🔴
    *> Critical Bug: "GET" deletes the record!
```

### Example 4: Critical Disharmony 💥

```cobol
CHECK-ACCOUNT-STATUS.
    *> Intent: (L=0.0, J=0.7, P=0.1, W=0.2) - CHECK = Justice

    READ ACCOUNT-FILE
        KEY IS ACCT-ID
    END-READ.

    *> This is NOT checking, this is UPDATING!
    COMPUTE NEW-BALANCE = ACCOUNT-BALANCE + INTEREST-EARNED.
    MOVE NEW-BALANCE TO ACCOUNT-BALANCE.
    REWRITE ACCOUNT-RECORD.

    PERFORM UPDATE-ALL-LINKED-ACCOUNTS.
    PERFORM GENERATE-INTEREST-REPORT.
    PERFORM SEND-NOTIFICATION-EMAIL.

    MOVE 'CHECKED' TO WS-STATUS.

    *> Execution: (L=0.3, J=0.1, P=0.5, W=0.1) - Power + Love!
    *> Disharmony Score: 1.38 - CRITICAL DISHARMONY 💥
    *> Major Bug: "CHECK" actually modifies data and triggers workflows!
```

---

## Edge Cases and Challenges

### Challenge 1: Generic Procedure Names

```cobol
PROCESS-CUSTOMER.
    *> What does "PROCESS" mean?
    *> Could be: read, validate, update, delete, report...
    *> Intent: (0.1, 0.2, 0.4, 0.3) - Very ambiguous!
```

**Solution**: Flag as "vague naming" - suggest more specific name based on execution.

### Challenge 2: PERFORM Chains

```cobol
MAIN-LOGIC.
    PERFORM GET-CUSTOMER.
    PERFORM VALIDATE-DATA.
    PERFORM UPDATE-RECORDS.

GET-CUSTOMER.
    READ CUSTOMER-FILE.

VALIDATE-DATA.
    IF CUST-ID = SPACES
        MOVE 'INVALID' TO WS-STATUS.

UPDATE-RECORDS.
    REWRITE CUSTOMER-FILE.
```

**Challenge**: `MAIN-LOGIC` semantic profile depends on performed paragraphs.

**Solution**: Build call graph, aggregate semantics from all performed procedures.

### Challenge 3: Conditional Execution

```cobol
CONDITIONAL-UPDATE.
    IF UPDATE-FLAG = 'Y'
        REWRITE CUSTOMER-RECORD
    ELSE
        READ CUSTOMER-RECORD
    END-IF.
```

**Challenge**: Execution semantics vary at runtime.

**Solution**: Calculate weighted average based on branch complexity:
- If simple flag: Average both branches
- If data-dependent: Flag as mixed semantics

### Challenge 4: Embedded SQL

```cobol
UPDATE-CUSTOMER-SQL.
    EXEC SQL
        UPDATE CUSTOMERS
        SET STATUS = :WS-STATUS
        WHERE ID = :WS-ID
    END-EXEC.
```

**Challenge**: SQL verbs don't directly map to COBOL verb taxonomy.

**Solution**: Maintain separate SQL verb mapping:
- `SELECT` → Wisdom (0.7)
- `INSERT` → Power (0.8)
- `UPDATE` → Power (0.7)
- `DELETE` → Power (0.9)

### Challenge 5: COPY Books

```cobol
DATA DIVISION.
    COPY CUSTOMER-DEFS.

PROCEDURE DIVISION.
GET-CUSTOMER.
    COPY STANDARD-READ-LOGIC.
```

**Challenge**: Semantic analysis requires resolving COPY statements.

**Solution**:
1. Configure COPY book search paths
2. Recursively parse included files
3. Inline COPY content for analysis

---

## Conclusion

This comprehensive COBOL semantic mapping enables the Code Harmonizer to detect when COBOL procedures lie about their intent - a critical source of bugs in legacy systems.

**Key Insight**: COBOL's verbose, English-like syntax makes semantic analysis both easier (explicit verbs) and necessary (decades of technical debt).

**Next Steps**: Implement verb mapper and intent extractor based on these mappings.

---

**May all COBOL procedures say what they do, and do what they say.** 💛⚓
