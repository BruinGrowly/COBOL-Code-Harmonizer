>>SOURCE FORMAT IS FREE
>>SET CONSTANT VERSION AS "1.5.2"
>>IF DEFINE-TRACE-LEVEL NOT DEFINED
>>SET CONSTANT DEFINE-TRACE-LEVEL AS 1
>>END-IF
IDENTIFICATION DIVISION.
PROGRAM-ID. StringProcessor.
AUTHOR. GnuCOBOL Demo.
*> Demonstrates advanced string processing with intrinsic functions

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> Input strings for processing
01  WS-INPUT-STRINGS.
    05  WS-STRING-1             PIC X(100) VALUE
        "  Hello, World! This is COBOL.  ".
    05  WS-STRING-2             PIC X(100) VALUE
        "john.doe@example.com".
    05  WS-STRING-3             PIC X(100) VALUE
        "The quick brown fox jumps over the lazy dog".
    05  WS-STRING-4             PIC X(100) VALUE
        "COBOL,Programming,Language,Legacy,Modernization".

*> Working storage for string operations
01  WS-PROCESSED-STRING         PIC X(200).
01  WS-TEMP-STRING              PIC X(200).
01  WS-OUTPUT-STRING            PIC X(200).

*> String analysis results
01  WS-STRING-STATS.
    05  WS-LENGTH               PIC 9(4).
    05  WS-TRIMMED-LENGTH       PIC 9(4).
    05  WS-WORD-COUNT           PIC 9(3).
    05  WS-UPPERCASE-COUNT      PIC 9(3).
    05  WS-LOWERCASE-COUNT      PIC 9(3).
    05  WS-DIGIT-COUNT          PIC 9(3).
    05  WS-SPACE-COUNT          PIC 9(3).

*> Substring extraction
01  WS-EXTRACTED-PARTS.
    05  WS-PART-1               PIC X(50).
    05  WS-PART-2               PIC X(50).
    05  WS-PART-3               PIC X(50).
    05  WS-PART-4               PIC X(50).

*> Control variables
01  WS-INDEX                    PIC 9(4).
01  WS-CHAR                     PIC X.
01  WS-DELIMITER-POS            PIC 9(4).
01  WS-START-POS                PIC 9(4).
01  WS-END-POS                  PIC 9(4).

PROCEDURE DIVISION.

MAIN-PROCEDURE.
    DISPLAY "=========================================="
    DISPLAY "GnuCOBOL String Processor"
    DISPLAY "Version: " VERSION
    DISPLAY "Trace Level: " DEFINE-TRACE-LEVEL
    DISPLAY "=========================================="
    DISPLAY SPACE

    PERFORM PROCESS-STRING-1
    PERFORM PROCESS-STRING-2
    PERFORM PROCESS-STRING-3
    PERFORM PROCESS-STRING-4

    DISPLAY "All string processing complete."
    STOP RUN.

PROCESS-STRING-1.
    DISPLAY "Processing String 1: Basic Transformations"
    DISPLAY "--------------------------------------------"
    DISPLAY "Original: [" TRIM(WS-STRING-1) "]"

    *> Trim whitespace
    MOVE FUNCTION TRIM(WS-STRING-1) TO WS-PROCESSED-STRING
    DISPLAY "Trimmed:  [" TRIM(WS-PROCESSED-STRING) "]"

    *> Convert to uppercase
    MOVE FUNCTION UPPER-CASE(WS-PROCESSED-STRING) TO WS-TEMP-STRING
    DISPLAY "Upper:    [" TRIM(WS-TEMP-STRING) "]"

    *> Convert to lowercase
    MOVE FUNCTION LOWER-CASE(WS-PROCESSED-STRING) TO WS-TEMP-STRING
    DISPLAY "Lower:    [" TRIM(WS-TEMP-STRING) "]"

    *> Reverse string
    MOVE FUNCTION REVERSE(WS-PROCESSED-STRING) TO WS-TEMP-STRING
    DISPLAY "Reversed: [" TRIM(WS-TEMP-STRING) "]"

    *> Calculate length
    COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-STRING-1)
    COMPUTE WS-TRIMMED-LENGTH =
        FUNCTION LENGTH(FUNCTION TRIM(WS-STRING-1))
    DISPLAY "Length: " WS-LENGTH
        " (trimmed: " WS-TRIMMED-LENGTH ")"

    DISPLAY SPACE.

PROCESS-STRING-2.
    DISPLAY "Processing String 2: Email Parsing"
    DISPLAY "--------------------------------------------"
    DISPLAY "Email: " TRIM(WS-STRING-2)

    *> Find @ symbol position
    COMPUTE WS-DELIMITER-POS =
        FUNCTION STORED-CHAR-LENGTH(WS-STRING-2, '@')

    IF WS-DELIMITER-POS > 0 THEN
        *> Extract username (before @)
        MOVE WS-STRING-2(1:WS-DELIMITER-POS - 1) TO WS-PART-1

        *> Extract domain (after @)
        COMPUTE WS-START-POS = WS-DELIMITER-POS + 1
        COMPUTE WS-LENGTH =
            FUNCTION LENGTH(FUNCTION TRIM(WS-STRING-2))
        COMPUTE WS-END-POS = WS-LENGTH - WS-DELIMITER-POS
        MOVE WS-STRING-2(WS-START-POS:WS-END-POS) TO WS-PART-2

        DISPLAY "Username: " TRIM(WS-PART-1)
        DISPLAY "Domain:   " TRIM(WS-PART-2)
    ELSE
        DISPLAY "Invalid email format"
    END-IF

    DISPLAY SPACE.

PROCESS-STRING-3.
    DISPLAY "Processing String 3: Word Analysis"
    DISPLAY "--------------------------------------------"
    DISPLAY "Text: " TRIM(WS-STRING-3)

    PERFORM ANALYZE-STRING-CONTENT

    DISPLAY "Statistics:"
    DISPLAY "  Total chars:    " WS-LENGTH
    DISPLAY "  Uppercase:      " WS-UPPERCASE-COUNT
    DISPLAY "  Lowercase:      " WS-LOWERCASE-COUNT
    DISPLAY "  Digits:         " WS-DIGIT-COUNT
    DISPLAY "  Spaces:         " WS-SPACE-COUNT
    DISPLAY "  Word count:     " WS-WORD-COUNT

    *> Extract specific words
    MOVE FUNCTION WORD(WS-STRING-3, 1) TO WS-PART-1
    MOVE FUNCTION WORD(WS-STRING-3, 3) TO WS-PART-2
    MOVE FUNCTION WORD(WS-STRING-3, 5) TO WS-PART-3

    DISPLAY "First word:  " TRIM(WS-PART-1)
    DISPLAY "Third word:  " TRIM(WS-PART-2)
    DISPLAY "Fifth word:  " TRIM(WS-PART-3)

    DISPLAY SPACE.

ANALYZE-STRING-CONTENT.
    MOVE FUNCTION TRIM(WS-STRING-3) TO WS-TEMP-STRING
    COMPUTE WS-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(WS-STRING-3))

    MOVE ZERO TO WS-UPPERCASE-COUNT
    MOVE ZERO TO WS-LOWERCASE-COUNT
    MOVE ZERO TO WS-DIGIT-COUNT
    MOVE ZERO TO WS-SPACE-COUNT

    PERFORM VARYING WS-INDEX FROM 1 BY 1
        UNTIL WS-INDEX > WS-LENGTH

        MOVE WS-TEMP-STRING(WS-INDEX:1) TO WS-CHAR

        EVALUATE TRUE
            WHEN WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                ADD 1 TO WS-UPPERCASE-COUNT
            WHEN WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                ADD 1 TO WS-LOWERCASE-COUNT
            WHEN WS-CHAR >= '0' AND WS-CHAR <= '9'
                ADD 1 TO WS-DIGIT-COUNT
            WHEN WS-CHAR = SPACE
                ADD 1 TO WS-SPACE-COUNT
        END-EVALUATE
    END-PERFORM

    *> Count words (spaces + 1)
    COMPUTE WS-WORD-COUNT = WS-SPACE-COUNT + 1.

PROCESS-STRING-4.
    DISPLAY "Processing String 4: CSV Parsing"
    DISPLAY "--------------------------------------------"
    DISPLAY "CSV: " TRIM(WS-STRING-4)

    PERFORM PARSE-CSV-STRING

    DISPLAY "Extracted fields:"
    DISPLAY "  1: " TRIM(WS-PART-1)
    DISPLAY "  2: " TRIM(WS-PART-2)
    DISPLAY "  3: " TRIM(WS-PART-3)
    DISPLAY "  4: " TRIM(WS-PART-4)

    *> Reconstruct with different delimiter
    STRING
        TRIM(WS-PART-1) " | "
        TRIM(WS-PART-2) " | "
        TRIM(WS-PART-3) " | "
        TRIM(WS-PART-4)
        DELIMITED BY SIZE
        INTO WS-OUTPUT-STRING
    END-STRING

    DISPLAY "Reformatted: " TRIM(WS-OUTPUT-STRING)

    DISPLAY SPACE.

PARSE-CSV-STRING.
    MOVE SPACES TO WS-EXTRACTED-PARTS

    *> Use UNSTRING to parse CSV
    UNSTRING WS-STRING-4
        DELIMITED BY ","
        INTO
            WS-PART-1
            WS-PART-2
            WS-PART-3
            WS-PART-4
    END-UNSTRING.

END PROGRAM StringProcessor.
