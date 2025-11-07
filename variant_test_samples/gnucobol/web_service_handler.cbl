>>SOURCE FORMAT IS FREE
>>SET CONSTANT API-VERSION AS "2.1.0"
>>SET CONSTANT MAX-CONNECTIONS AS 100
IDENTIFICATION DIVISION.
PROGRAM-ID. WebServiceHandler.
*> GnuCOBOL Web Service Request Handler
*> Demonstrates C function integration and modern features

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> HTTP Request Structure
01  WS-HTTP-REQUEST.
    05  WS-METHOD               PIC X(10).
    05  WS-URI                  PIC X(200).
    05  WS-PROTOCOL             PIC X(10).
    05  WS-CONTENT-TYPE         PIC X(50).
    05  WS-CONTENT-LENGTH       PIC 9(8).
    05  WS-BODY                 PIC X(2000).

*> HTTP Response Structure
01  WS-HTTP-RESPONSE.
    05  WS-STATUS-CODE          PIC 999.
    05  WS-STATUS-TEXT          PIC X(30).
    05  WS-RESPONSE-BODY        PIC X(4000).
    05  WS-RESPONSE-LENGTH      PIC 9(8).

*> Request Statistics
01  WS-STATS.
    05  WS-TOTAL-REQUESTS       PIC 9(8) VALUE ZERO.
    05  WS-SUCCESS-COUNT        PIC 9(8) VALUE ZERO.
    05  WS-ERROR-COUNT          PIC 9(8) VALUE ZERO.
    05  WS-GET-COUNT            PIC 9(8) VALUE ZERO.
    05  WS-POST-COUNT           PIC 9(8) VALUE ZERO.
    05  WS-PUT-COUNT            PIC 9(8) VALUE ZERO.
    05  WS-DELETE-COUNT         PIC 9(8) VALUE ZERO.

*> Resource Data
01  WS-RESOURCE-ID              PIC 9(10).
01  WS-RESOURCE-NAME            PIC X(100).
01  WS-RESOURCE-VALUE           PIC X(500).

*> Control Variables
01  WS-CONTINUE-FLAG            PIC X VALUE "Y".
01  WS-ROUTE-MATCHED            PIC X VALUE "N".
01  WS-TIMESTAMP                PIC X(26).

*> Parsed URI Components
01  WS-URI-PARTS.
    05  WS-ENDPOINT             PIC X(50).
    05  WS-RESOURCE-PATH        PIC X(100).
    05  WS-QUERY-STRING         PIC X(200).

>>IF DEBUG-MODE DEFINED
01  WS-DEBUG-MESSAGE            PIC X(200).
>>END-IF

PROCEDURE DIVISION.

MAIN-PROCEDURE.
    DISPLAY "=========================================="
    DISPLAY "GnuCOBOL Web Service Handler"
    >>IF DEBUG-MODE DEFINED
    DISPLAY "*** DEBUG MODE ENABLED ***"
    >>END-IF
    DISPLAY "API Version: " API-VERSION
    DISPLAY "Max Connections: " MAX-CONNECTIONS
    DISPLAY "=========================================="
    DISPLAY SPACE

    PERFORM INITIALIZE-SERVICE
    PERFORM SIMULATE-REQUESTS
    PERFORM DISPLAY-STATISTICS
    PERFORM SHUTDOWN-SERVICE

    STOP RUN.

INITIALIZE-SERVICE.
    DISPLAY "Initializing web service..."
    MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
    DISPLAY "Service started at: " WS-TIMESTAMP
    DISPLAY "Ready to accept connections"
    DISPLAY SPACE.

SIMULATE-REQUESTS.
    DISPLAY "Processing simulated HTTP requests..."
    DISPLAY "--------------------------------------------"
    DISPLAY SPACE

    *> Simulate various HTTP requests
    PERFORM PROCESS-GET-REQUEST
    PERFORM PROCESS-POST-REQUEST
    PERFORM PROCESS-PUT-REQUEST
    PERFORM PROCESS-DELETE-REQUEST
    PERFORM PROCESS-INVALID-REQUEST.

PROCESS-GET-REQUEST.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Processing GET request"
    >>END-IF

    MOVE "GET" TO WS-METHOD
    MOVE "/api/users/12345" TO WS-URI
    MOVE "HTTP/1.1" TO WS-PROTOCOL
    MOVE "application/json" TO WS-CONTENT-TYPE
    MOVE ZERO TO WS-CONTENT-LENGTH
    MOVE SPACES TO WS-BODY

    PERFORM ROUTE-REQUEST
    PERFORM LOG-REQUEST.

PROCESS-POST-REQUEST.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Processing POST request"
    >>END-IF

    MOVE "POST" TO WS-METHOD
    MOVE "/api/users" TO WS-URI
    MOVE "HTTP/1.1" TO WS-PROTOCOL
    MOVE "application/json" TO WS-CONTENT-TYPE
    MOVE 150 TO WS-CONTENT-LENGTH
    MOVE '{"name":"John Doe","email":"john@example.com"}' TO WS-BODY

    PERFORM ROUTE-REQUEST
    PERFORM LOG-REQUEST.

PROCESS-PUT-REQUEST.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Processing PUT request"
    >>END-IF

    MOVE "PUT" TO WS-METHOD
    MOVE "/api/users/12345" TO WS-URI
    MOVE "HTTP/1.1" TO WS-PROTOCOL
    MOVE "application/json" TO WS-CONTENT-TYPE
    MOVE 120 TO WS-CONTENT-LENGTH
    MOVE '{"name":"Jane Smith","email":"jane@example.com"}' TO WS-BODY

    PERFORM ROUTE-REQUEST
    PERFORM LOG-REQUEST.

PROCESS-DELETE-REQUEST.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Processing DELETE request"
    >>END-IF

    MOVE "DELETE" TO WS-METHOD
    MOVE "/api/users/12345" TO WS-URI
    MOVE "HTTP/1.1" TO WS-PROTOCOL
    MOVE "application/json" TO WS-CONTENT-TYPE
    MOVE ZERO TO WS-CONTENT-LENGTH
    MOVE SPACES TO WS-BODY

    PERFORM ROUTE-REQUEST
    PERFORM LOG-REQUEST.

PROCESS-INVALID-REQUEST.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Processing invalid request"
    >>END-IF

    MOVE "PATCH" TO WS-METHOD
    MOVE "/invalid/endpoint" TO WS-URI
    MOVE "HTTP/1.1" TO WS-PROTOCOL
    MOVE "text/plain" TO WS-CONTENT-TYPE
    MOVE ZERO TO WS-CONTENT-LENGTH
    MOVE SPACES TO WS-BODY

    PERFORM ROUTE-REQUEST
    PERFORM LOG-REQUEST.

ROUTE-REQUEST.
    ADD 1 TO WS-TOTAL-REQUESTS
    MOVE "N" TO WS-ROUTE-MATCHED

    PERFORM PARSE-URI

    *> Route to appropriate handler
    EVALUATE TRUE
        WHEN WS-ENDPOINT = "/api/users" AND WS-METHOD = "GET"
            PERFORM HANDLE-GET-USERS
            MOVE "Y" TO WS-ROUTE-MATCHED
        WHEN WS-ENDPOINT = "/api/users" AND WS-METHOD = "POST"
            PERFORM HANDLE-CREATE-USER
            MOVE "Y" TO WS-ROUTE-MATCHED
        WHEN WS-ENDPOINT = "/api/users" AND WS-METHOD = "PUT"
            PERFORM HANDLE-UPDATE-USER
            MOVE "Y" TO WS-ROUTE-MATCHED
        WHEN WS-ENDPOINT = "/api/users" AND WS-METHOD = "DELETE"
            PERFORM HANDLE-DELETE-USER
            MOVE "Y" TO WS-ROUTE-MATCHED
        WHEN WS-ENDPOINT = "/api/health"
            PERFORM HANDLE-HEALTH-CHECK
            MOVE "Y" TO WS-ROUTE-MATCHED
        WHEN OTHER
            PERFORM HANDLE-NOT-FOUND
    END-EVALUATE.

PARSE-URI.
    *> Extract endpoint from URI
    IF WS-URI(1:10) = "/api/users" THEN
        MOVE "/api/users" TO WS-ENDPOINT
        IF LENGTH(TRIM(WS-URI)) > 10 THEN
            MOVE WS-URI(11:) TO WS-RESOURCE-PATH
        END-IF
    ELSE IF WS-URI(1:11) = "/api/health" THEN
        MOVE "/api/health" TO WS-ENDPOINT
    ELSE
        MOVE SPACES TO WS-ENDPOINT
    END-IF.

HANDLE-GET-USERS.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Handling GET /api/users"
    >>END-IF

    ADD 1 TO WS-GET-COUNT

    IF WS-RESOURCE-PATH NOT = SPACES THEN
        *> Get specific user
        MOVE 12345 TO WS-RESOURCE-ID
        MOVE "John Doe" TO WS-RESOURCE-NAME
        STRING
            '{"id":' WS-RESOURCE-ID
            ',"name":"' TRIM(WS-RESOURCE-NAME)
            '","email":"john@example.com"}'
            DELIMITED BY SIZE
            INTO WS-RESPONSE-BODY
        END-STRING
        MOVE 200 TO WS-STATUS-CODE
        MOVE "OK" TO WS-STATUS-TEXT
    ELSE
        *> Get all users
        STRING
            '[{"id":12345,"name":"John Doe"},'
            '{"id":67890,"name":"Jane Smith"}]'
            DELIMITED BY SIZE
            INTO WS-RESPONSE-BODY
        END-STRING
        MOVE 200 TO WS-STATUS-CODE
        MOVE "OK" TO WS-STATUS-TEXT
    END-IF

    ADD 1 TO WS-SUCCESS-COUNT
    PERFORM BUILD-RESPONSE.

HANDLE-CREATE-USER.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Handling POST /api/users"
    >>END-IF

    ADD 1 TO WS-POST-COUNT

    *> Simulate user creation
    COMPUTE WS-RESOURCE-ID = FUNCTION RANDOM(1000000)

    STRING
        '{"id":' WS-RESOURCE-ID
        ',"status":"created",'
        '"message":"User created successfully"}'
        DELIMITED BY SIZE
        INTO WS-RESPONSE-BODY
    END-STRING

    MOVE 201 TO WS-STATUS-CODE
    MOVE "Created" TO WS-STATUS-TEXT
    ADD 1 TO WS-SUCCESS-COUNT
    PERFORM BUILD-RESPONSE.

HANDLE-UPDATE-USER.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Handling PUT /api/users"
    >>END-IF

    ADD 1 TO WS-PUT-COUNT

    STRING
        '{"id":12345,'
        '"status":"updated",'
        '"message":"User updated successfully"}'
        DELIMITED BY SIZE
        INTO WS-RESPONSE-BODY
    END-STRING

    MOVE 200 TO WS-STATUS-CODE
    MOVE "OK" TO WS-STATUS-TEXT
    ADD 1 TO WS-SUCCESS-COUNT
    PERFORM BUILD-RESPONSE.

HANDLE-DELETE-USER.
    >>IF DEBUG-MODE DEFINED
    DISPLAY "DEBUG: Handling DELETE /api/users"
    >>END-IF

    ADD 1 TO WS-DELETE-COUNT

    STRING
        '{"id":12345,'
        '"status":"deleted",'
        '"message":"User deleted successfully"}'
        DELIMITED BY SIZE
        INTO WS-RESPONSE-BODY
    END-STRING

    MOVE 200 TO WS-STATUS-CODE
    MOVE "OK" TO WS-STATUS-TEXT
    ADD 1 TO WS-SUCCESS-COUNT
    PERFORM BUILD-RESPONSE.

HANDLE-HEALTH-CHECK.
    STRING
        '{"status":"healthy",'
        '"version":"' API-VERSION '",'
        '"timestamp":"' FUNCTION CURRENT-DATE '"}'
        DELIMITED BY SIZE
        INTO WS-RESPONSE-BODY
    END-STRING

    MOVE 200 TO WS-STATUS-CODE
    MOVE "OK" TO WS-STATUS-TEXT
    ADD 1 TO WS-SUCCESS-COUNT
    PERFORM BUILD-RESPONSE.

HANDLE-NOT-FOUND.
    ADD 1 TO WS-ERROR-COUNT

    STRING
        '{"error":"Not Found",'
        '"message":"The requested resource was not found",'
        '"path":"' TRIM(WS-URI) '"}'
        DELIMITED BY SIZE
        INTO WS-RESPONSE-BODY
    END-STRING

    MOVE 404 TO WS-STATUS-CODE
    MOVE "Not Found" TO WS-STATUS-TEXT
    PERFORM BUILD-RESPONSE.

BUILD-RESPONSE.
    COMPUTE WS-RESPONSE-LENGTH = FUNCTION LENGTH(TRIM(WS-RESPONSE-BODY)).

LOG-REQUEST.
    DISPLAY WS-METHOD " " TRIM(WS-URI)
        " - " WS-STATUS-CODE " " TRIM(WS-STATUS-TEXT)
    >>IF DEBUG-MODE DEFINED
    DISPLAY "  Response: " TRIM(WS-RESPONSE-BODY)
    >>END-IF
    DISPLAY SPACE.

DISPLAY-STATISTICS.
    DISPLAY "=========================================="
    DISPLAY "Request Statistics"
    DISPLAY "=========================================="
    DISPLAY "Total Requests:    " WS-TOTAL-REQUESTS
    DISPLAY "Successful:        " WS-SUCCESS-COUNT
    DISPLAY "Errors:            " WS-ERROR-COUNT
    DISPLAY SPACE
    DISPLAY "By Method:"
    DISPLAY "  GET:             " WS-GET-COUNT
    DISPLAY "  POST:            " WS-POST-COUNT
    DISPLAY "  PUT:             " WS-PUT-COUNT
    DISPLAY "  DELETE:          " WS-DELETE-COUNT
    DISPLAY SPACE.

SHUTDOWN-SERVICE.
    MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
    DISPLAY "Service stopped at: " WS-TIMESTAMP
    DISPLAY "Shutdown complete.".

END PROGRAM WebServiceHandler.
