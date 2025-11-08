"""
SQL/CICS Semantic Analyzer

Analyzes EXEC SQL and EXEC CICS statements in COBOL programs.
Maps SQL and CICS operations to LJPW semantic coordinates.

Supports:
- EXEC SQL (DB2, Oracle, Informix, etc.)
- EXEC CICS (Customer Information Control System)
- IBM Enterprise COBOL extensions
"""

import re
from typing import List, Tuple, Optional, Dict
from dataclasses import dataclass

# Type alias for LJPW coordinates
LJPWCoords = Tuple[float, float, float, float]


@dataclass
class SQLStatement:
    """Represents an EXEC SQL statement"""

    operation: str  # SELECT, INSERT, UPDATE, DELETE, etc.
    tables: List[str]  # Table names
    is_update: bool  # True if modifies data
    is_transactional: bool  # True if COMMIT/ROLLBACK
    full_text: str  # Original statement


@dataclass
class CICSCommand:
    """Represents an EXEC CICS command"""

    command: str  # READ, WRITE, SEND, RECEIVE, etc.
    resource_type: str  # FILE, QUEUE, TERMINAL, etc.
    is_update: bool  # True if modifies data
    is_transactional: bool  # True if SYNCPOINT
    full_text: str  # Original command


class SQLCICSAnalyzer:
    """Analyze SQL and CICS statements for semantic meaning"""

    # SQL verb to LJPW coordinate mapping
    SQL_COORDINATES: Dict[str, LJPWCoords] = {
        # WISDOM-Dominant (Retrieval/Query)
        "SELECT": (0.1, 0.2, 0.0, 0.7),
        "FETCH": (0.1, 0.2, 0.0, 0.7),
        "EXPLAIN": (0.0, 0.3, 0.0, 0.7),
        "DESCRIBE": (0.0, 0.2, 0.0, 0.8),
        # POWER-Dominant (Modification)
        "INSERT": (0.1, 0.2, 0.7, 0.0),
        "UPDATE": (0.0, 0.2, 0.7, 0.1),
        "DELETE": (0.0, 0.2, 0.8, 0.0),
        "TRUNCATE": (0.0, 0.1, 0.9, 0.0),
        "DROP": (0.0, 0.1, 0.9, 0.0),
        "CREATE": (0.1, 0.2, 0.6, 0.1),
        "ALTER": (0.0, 0.2, 0.7, 0.1),
        # JUSTICE-Dominant (Transactional Control)
        "COMMIT": (0.1, 0.6, 0.3, 0.0),
        "ROLLBACK": (0.0, 0.6, 0.4, 0.0),
        "SAVEPOINT": (0.0, 0.5, 0.3, 0.2),
        # LOVE-Dominant (Connection/Session)
        "CONNECT": (0.7, 0.1, 0.1, 0.1),
        "DISCONNECT": (0.6, 0.1, 0.2, 0.1),
        "SET": (0.1, 0.2, 0.5, 0.2),
        # BALANCED (Cursor Operations)
        "DECLARE": (0.1, 0.3, 0.2, 0.4),
        "OPEN": (0.1, 0.2, 0.4, 0.3),
        "CLOSE": (0.1, 0.2, 0.4, 0.3),
        # DB2-Specific
        "CALL": (0.5, 0.1, 0.3, 0.1),  # Stored procedure
        "PREPARE": (0.0, 0.3, 0.2, 0.5),
        "EXECUTE": (0.1, 0.2, 0.5, 0.2),
        "LOCK": (0.0, 0.5, 0.4, 0.1),
        "UNLOCK": (0.0, 0.4, 0.5, 0.1),
    }

    # CICS command to LJPW coordinate mapping
    CICS_COORDINATES: Dict[str, LJPWCoords] = {
        # WISDOM-Dominant (Read/Retrieve)
        "READ": (0.1, 0.2, 0.0, 0.7),
        "READNEXT": (0.1, 0.2, 0.0, 0.7),
        "READPREV": (0.1, 0.2, 0.0, 0.7),
        "RETRIEVE": (0.1, 0.2, 0.0, 0.7),
        "INQUIRE": (0.0, 0.3, 0.0, 0.7),
        # POWER-Dominant (Write/Modify)
        "WRITE": (0.1, 0.1, 0.7, 0.1),
        "REWRITE": (0.0, 0.2, 0.7, 0.1),
        "DELETE": (0.0, 0.2, 0.8, 0.0),
        "DELETEQ": (0.0, 0.2, 0.8, 0.0),
        "WRITEQ": (0.1, 0.1, 0.7, 0.1),
        "UPDATE": (0.0, 0.2, 0.7, 0.1),
        # LOVE-Dominant (Communication)
        "SEND": (0.6, 0.1, 0.2, 0.1),
        "RECEIVE": (0.5, 0.1, 0.1, 0.3),
        "CONVERSE": (0.7, 0.1, 0.1, 0.1),
        "LINK": (0.6, 0.1, 0.2, 0.1),
        "XCTL": (0.5, 0.1, 0.3, 0.1),  # Transfer control
        "START": (0.4, 0.1, 0.4, 0.1),
        "RETURN": (0.3, 0.2, 0.3, 0.2),
        # JUSTICE-Dominant (Transactional)
        "SYNCPOINT": (0.1, 0.6, 0.3, 0.0),
        "SYNCPOINT ROLLBACK": (0.0, 0.6, 0.4, 0.0),
        "ENQ": (0.0, 0.6, 0.3, 0.1),  # Enqueue (lock)
        "DEQ": (0.0, 0.5, 0.4, 0.1),  # Dequeue (unlock)
        "HANDLE": (0.1, 0.5, 0.3, 0.1),
        # BALANCED (Session/Control)
        "GETMAIN": (0.0, 0.1, 0.6, 0.3),  # Allocate storage
        "FREEMAIN": (0.0, 0.1, 0.7, 0.2),  # Free storage
        "DELAY": (0.0, 0.2, 0.5, 0.3),
        "SUSPEND": (0.0, 0.2, 0.5, 0.3),
        "WAIT": (0.0, 0.3, 0.4, 0.3),
        "POST": (0.2, 0.3, 0.4, 0.1),
        # Terminal I/O
        "SEND MAP": (0.4, 0.1, 0.3, 0.2),
        "RECEIVE MAP": (0.3, 0.1, 0.1, 0.5),
        "SEND TEXT": (0.5, 0.1, 0.2, 0.2),
        "RECEIVE TEXT": (0.4, 0.1, 0.1, 0.4),
        # Queue Operations
        "READQ TS": (0.1, 0.2, 0.0, 0.7),  # Temporary storage
        "WRITEQ TS": (0.1, 0.1, 0.7, 0.1),
        "READQ TD": (0.1, 0.2, 0.0, 0.7),  # Transient data
        "WRITEQ TD": (0.1, 0.1, 0.7, 0.1),
    }

    # Default coordinate for unknown operations
    DEFAULT_COORDS: LJPWCoords = (0.25, 0.25, 0.25, 0.25)

    def __init__(self):
        pass

    def extract_sql_statements(self, cobol_source: str) -> List[SQLStatement]:
        """
        Extract all EXEC SQL statements from COBOL source.

        Args:
            cobol_source: COBOL source code

        Returns:
            List of SQLStatement objects
        """
        statements = []

        # Pattern to match EXEC SQL ... END-EXEC
        # Handles both single-line and multi-line statements
        pattern = r"EXEC\s+SQL\s+(.*?)\s+END-EXEC"
        matches = re.finditer(pattern, cobol_source, re.IGNORECASE | re.DOTALL)

        for match in matches:
            sql_text = match.group(1).strip()

            # Extract operation (first keyword)
            operation_match = re.match(r"(\w+)", sql_text)
            if not operation_match:
                continue

            operation = operation_match.group(1).upper()

            # Extract table names (simplified - looks for FROM, INTO, UPDATE keywords)
            tables = self._extract_table_names(sql_text)

            # Determine if it's an update operation
            is_update = operation in {
                "INSERT",
                "UPDATE",
                "DELETE",
                "TRUNCATE",
                "DROP",
                "ALTER",
                "CREATE",
            }

            # Determine if it's transactional
            is_transactional = operation in {"COMMIT", "ROLLBACK", "SAVEPOINT"}

            statements.append(
                SQLStatement(
                    operation=operation,
                    tables=tables,
                    is_update=is_update,
                    is_transactional=is_transactional,
                    full_text=sql_text,
                )
            )

        return statements

    def extract_cics_commands(self, cobol_source: str) -> List[CICSCommand]:
        """
        Extract all EXEC CICS commands from COBOL source.

        Args:
            cobol_source: COBOL source code

        Returns:
            List of CICSCommand objects
        """
        commands = []

        # Pattern to match EXEC CICS ... END-EXEC
        pattern = r"EXEC\s+CICS\s+(.*?)\s+END-EXEC"
        matches = re.finditer(pattern, cobol_source, re.IGNORECASE | re.DOTALL)

        for match in matches:
            cics_text = match.group(1).strip()

            # Extract command (first keyword or keyword phrase)
            command = self._extract_cics_command(cics_text)

            # Extract resource type
            resource_type = self._extract_resource_type(cics_text)

            # Determine if it's an update operation
            is_update = command in {"WRITE", "REWRITE", "DELETE", "DELETEQ", "WRITEQ", "UPDATE"}

            # Determine if it's transactional
            is_transactional = command in {"SYNCPOINT", "SYNCPOINT ROLLBACK"}

            commands.append(
                CICSCommand(
                    command=command,
                    resource_type=resource_type,
                    is_update=is_update,
                    is_transactional=is_transactional,
                    full_text=cics_text,
                )
            )

        return commands

    def analyze_sql_semantics(self, sql_statements: List[SQLStatement]) -> LJPWCoords:
        """
        Analyze SQL statements and return aggregate LJPW coordinates.

        Args:
            sql_statements: List of SQL statements

        Returns:
            Aggregate LJPW coordinates (centroid)
        """
        if not sql_statements:
            return self.DEFAULT_COORDS

        coords_list = []
        for stmt in sql_statements:
            coords = self.SQL_COORDINATES.get(stmt.operation, self.DEFAULT_COORDS)
            coords_list.append(coords)

        return self._calculate_centroid(coords_list)

    def analyze_cics_semantics(self, cics_commands: List[CICSCommand]) -> LJPWCoords:
        """
        Analyze CICS commands and return aggregate LJPW coordinates.

        Args:
            cics_commands: List of CICS commands

        Returns:
            Aggregate LJPW coordinates (centroid)
        """
        if not cics_commands:
            return self.DEFAULT_COORDS

        coords_list = []
        for cmd in cics_commands:
            coords = self.CICS_COORDINATES.get(cmd.command, self.DEFAULT_COORDS)
            coords_list.append(coords)

        return self._calculate_centroid(coords_list)

    def get_sql_verb_coords(self, sql_operation: str) -> LJPWCoords:
        """
        Get LJPW coordinates for a specific SQL operation.

        Args:
            sql_operation: SQL operation (SELECT, INSERT, etc.)

        Returns:
            LJPW coordinates
        """
        return self.SQL_COORDINATES.get(sql_operation.upper(), self.DEFAULT_COORDS)

    def get_cics_command_coords(self, cics_command: str) -> LJPWCoords:
        """
        Get LJPW coordinates for a specific CICS command.

        Args:
            cics_command: CICS command (READ, WRITE, etc.)

        Returns:
            LJPW coordinates
        """
        return self.CICS_COORDINATES.get(cics_command.upper(), self.DEFAULT_COORDS)

    def has_sql(self, cobol_source: str) -> bool:
        """Check if COBOL source contains EXEC SQL statements"""
        return bool(re.search(r"EXEC\s+SQL", cobol_source, re.IGNORECASE))

    def has_cics(self, cobol_source: str) -> bool:
        """Check if COBOL source contains EXEC CICS commands"""
        return bool(re.search(r"EXEC\s+CICS", cobol_source, re.IGNORECASE))

    def _extract_table_names(self, sql_text: str) -> List[str]:
        """Extract table names from SQL statement (simplified)"""
        tables = []

        # Look for FROM clause
        from_match = re.search(r"FROM\s+([\w.-]+)", sql_text, re.IGNORECASE)
        if from_match:
            tables.append(from_match.group(1))

        # Look for INTO clause (INSERT)
        into_match = re.search(r"INTO\s+([\w.-]+)", sql_text, re.IGNORECASE)
        if into_match:
            tables.append(into_match.group(1))

        # Look for UPDATE clause
        update_match = re.search(r"UPDATE\s+([\w.-]+)", sql_text, re.IGNORECASE)
        if update_match:
            tables.append(update_match.group(1))

        return tables

    def _extract_cics_command(self, cics_text: str) -> str:
        """Extract CICS command from command text"""
        # Handle multi-word commands
        multi_word_commands = [
            "SYNCPOINT ROLLBACK",
            "SEND MAP",
            "RECEIVE MAP",
            "SEND TEXT",
            "RECEIVE TEXT",
            "READQ TS",
            "WRITEQ TS",
            "READQ TD",
            "WRITEQ TD",
        ]

        cics_upper = cics_text.upper()
        for cmd in multi_word_commands:
            if cics_upper.startswith(cmd):
                return cmd

        # Single-word command (first word)
        match = re.match(r"(\w+)", cics_text)
        if match:
            return match.group(1).upper()

        return "UNKNOWN"

    def _extract_resource_type(self, cics_text: str) -> str:
        """Extract resource type from CICS command"""
        # Look for FILE(), QUEUE(), TERMINAL(), etc.
        if "FILE(" in cics_text.upper():
            return "FILE"
        elif "QUEUE(" in cics_text.upper():
            return "QUEUE"
        elif "TERMINAL(" in cics_text.upper() or "TERMID(" in cics_text.upper():
            return "TERMINAL"
        elif "PROGRAM(" in cics_text.upper():
            return "PROGRAM"
        elif "MAP(" in cics_text.upper():
            return "MAP"
        elif " TS" in cics_text.upper():
            return "TEMP_STORAGE"
        elif " TD" in cics_text.upper():
            return "TRANSIENT_DATA"
        else:
            return "UNKNOWN"

    def _calculate_centroid(self, coords_list: List[LJPWCoords]) -> LJPWCoords:
        """
        Calculate centroid (average) of multiple coordinate sets.

        Args:
            coords_list: List of LJPW coordinates

        Returns:
            Centroid coordinates
        """
        if not coords_list:
            return self.DEFAULT_COORDS

        n = len(coords_list)
        l_avg = sum(c[0] for c in coords_list) / n
        j_avg = sum(c[1] for c in coords_list) / n
        p_avg = sum(c[2] for c in coords_list) / n
        w_avg = sum(c[3] for c in coords_list) / n

        return (l_avg, j_avg, p_avg, w_avg)

    def analyze_combined_semantics(
        self,
        cobol_coords: LJPWCoords,
        sql_statements: List[SQLStatement],
        cics_commands: List[CICSCommand],
    ) -> LJPWCoords:
        """
        Combine COBOL, SQL, and CICS semantics into aggregate coordinates.

        Strategy:
        - Weight COBOL verbs: 50%
        - Weight SQL statements: 30%
        - Weight CICS commands: 20%

        Args:
            cobol_coords: LJPW coordinates from COBOL verbs
            sql_statements: List of SQL statements
            cics_commands: List of CICS commands

        Returns:
            Combined LJPW coordinates
        """
        weights = []
        coords = []

        # COBOL verbs (base weight: 0.5)
        weights.append(0.5)
        coords.append(cobol_coords)

        # SQL statements (weight: 0.3)
        if sql_statements:
            weights.append(0.3)
            coords.append(self.analyze_sql_semantics(sql_statements))

        # CICS commands (weight: 0.2)
        if cics_commands:
            weights.append(0.2)
            coords.append(self.analyze_cics_semantics(cics_commands))

        # Normalize weights
        total_weight = sum(weights)
        normalized_weights = [w / total_weight for w in weights]

        # Calculate weighted average
        l = sum(c[0] * w for c, w in zip(coords, normalized_weights))
        j = sum(c[1] * w for c, w in zip(coords, normalized_weights))
        p = sum(c[2] * w for c, w in zip(coords, normalized_weights))
        w_dim = sum(c[3] * w for c, w in zip(coords, normalized_weights))

        return (l, j, p, w_dim)

    def get_sql_impact_description(self, sql_statements: List[SQLStatement]) -> str:
        """
        Generate human-readable description of SQL impact.

        Args:
            sql_statements: List of SQL statements

        Returns:
            Description string
        """
        if not sql_statements:
            return "No SQL operations"

        selects = sum(1 for s in sql_statements if s.operation == "SELECT")
        inserts = sum(1 for s in sql_statements if s.operation == "INSERT")
        updates = sum(1 for s in sql_statements if s.operation == "UPDATE")
        deletes = sum(1 for s in sql_statements if s.operation == "DELETE")

        parts = []
        if selects:
            parts.append(f"{selects} SELECT(s)")
        if inserts:
            parts.append(f"{inserts} INSERT(s)")
        if updates:
            parts.append(f"{updates} UPDATE(s)")
        if deletes:
            parts.append(f"{deletes} DELETE(s)")

        return "SQL: " + ", ".join(parts) if parts else "SQL operations"

    def get_cics_impact_description(self, cics_commands: List[CICSCommand]) -> str:
        """
        Generate human-readable description of CICS impact.

        Args:
            cics_commands: List of CICS commands

        Returns:
            Description string
        """
        if not cics_commands:
            return "No CICS operations"

        reads = sum(1 for c in cics_commands if "READ" in c.command)
        writes = sum(1 for c in cics_commands if "WRITE" in c.command)
        sends = sum(1 for c in cics_commands if "SEND" in c.command)
        receives = sum(1 for c in cics_commands if "RECEIVE" in c.command)

        parts = []
        if reads:
            parts.append(f"{reads} READ(s)")
        if writes:
            parts.append(f"{writes} WRITE(s)")
        if sends:
            parts.append(f"{sends} SEND(s)")
        if receives:
            parts.append(f"{receives} RECEIVE(s)")

        return "CICS: " + ", ".join(parts) if parts else "CICS operations"
