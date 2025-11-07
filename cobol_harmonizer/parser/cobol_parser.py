"""
COBOL Parser

Parses COBOL source files and extracts procedure definitions for semantic analysis.
Supports both fixed-format and free-format COBOL.
"""

import re
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum


class COBOLFormat(Enum):
    """COBOL source format types"""
    FIXED = "fixed"      # COBOL-85 standard (columns 7-72)
    FREE = "free"        # COBOL-2002+ free format
    MIXED = "mixed"      # Mixed fixed and free (Micro Focus)


class COBOLStandard(Enum):
    """COBOL standard versions"""
    COBOL_74 = "74"
    COBOL_85 = "85"
    COBOL_2002 = "2002"
    COBOL_2014 = "2014"
    COBOL_2023 = "2023"
    UNKNOWN = "unknown"


class COBOLDialect(Enum):
    """COBOL implementation dialects"""
    STANDARD = "standard"      # ISO/ANSI standard
    IBM = "ibm"               # IBM Enterprise COBOL
    MICRO_FOCUS = "microfocus" # Micro Focus COBOL
    GNU = "gnu"               # GnuCOBOL
    ACUCOBOL = "acucobol"     # ACUCOBOL-GT
    FUJITSU = "fujitsu"       # Fujitsu NetCOBOL


@dataclass
class COBOLStatement:
    """Represents a single COBOL statement"""
    verb: str
    line_number: int
    full_text: str
    operands: List[str] = field(default_factory=list)


@dataclass
class Procedure:
    """Represents a COBOL SECTION or PARAGRAPH"""
    name: str
    type: str  # 'SECTION' or 'PARAGRAPH'
    line_number: int
    statements: List[COBOLStatement] = field(default_factory=list)
    performed_procedures: List[str] = field(default_factory=list)


@dataclass
class COBOLProgram:
    """Represents a parsed COBOL program"""
    program_id: str
    procedures: List[Procedure] = field(default_factory=list)
    source_format: COBOLFormat = COBOLFormat.FIXED
    standard: COBOLStandard = COBOLStandard.UNKNOWN
    dialect: COBOLDialect = COBOLDialect.STANDARD
    has_oop_features: bool = False
    has_exec_sql: bool = False
    has_exec_cics: bool = False
    has_xml_json: bool = False


class COBOLParser:
    """Parse COBOL source code and extract semantic structures"""

    # COBOL verbs for statement identification
    COBOL_VERBS = {
        'ACCEPT', 'ADD', 'ALTER', 'CALL', 'CANCEL', 'CLOSE', 'COMPUTE',
        'CONTINUE', 'DELETE', 'DISPLAY', 'DIVIDE', 'ENABLE', 'DISABLE',
        'EVALUATE', 'EXIT', 'FREE', 'GENERATE', 'GO', 'GOBACK', 'IF',
        'INITIALIZE', 'INITIATE', 'INSPECT', 'INVOKE', 'MERGE', 'MOVE',
        'MULTIPLY', 'OPEN', 'PERFORM', 'PURGE', 'READ', 'RECEIVE',
        'RELEASE', 'RETURN', 'REWRITE', 'SEARCH', 'SEND', 'SET',
        'SORT', 'START', 'STOP', 'STRING', 'SUBTRACT', 'SUPPRESS',
        'TERMINATE', 'UNSTRING', 'VALIDATE', 'WRITE', 'XML', 'JSON',
        # SQL verbs (embedded SQL)
        'SELECT', 'INSERT', 'UPDATE', 'CREATE', 'DROP', 'COMMIT', 'ROLLBACK'
    }

    def __init__(self):
        self.current_format = COBOLFormat.FIXED

    def detect_format(self, source: str) -> COBOLFormat:
        """
        Auto-detect COBOL source format (fixed vs free).

        Args:
            source: COBOL source code

        Returns:
            COBOLFormat enum value
        """
        lines = source.split('\n')

        # Check for explicit format directives
        for line in lines[:20]:
            line_upper = line.upper()
            if '>>SOURCE' in line_upper and 'FREE' in line_upper:
                return COBOLFormat.FREE
            if '$SET' in line_upper and 'SOURCEFORMAT' in line_upper and 'FREE' in line_upper:
                return COBOLFormat.FREE

        # Check for free-format indicators
        for line in lines[:50]:  # Check first 50 lines
            # Free format typically doesn't have sequence numbers in cols 1-6
            # and doesn't respect column positioning
            if len(line) > 0 and not line[0].isdigit():
                # Check if it looks like free format
                if any(keyword in line.upper() for keyword in ['IDENTIFICATION', 'PROGRAM-ID']):
                    if not line.startswith(' ' * 7):  # Not in area A/B
                        return COBOLFormat.FREE

        return COBOLFormat.FIXED

    def detect_standard(self, source: str) -> COBOLStandard:
        """
        Detect COBOL standard version based on language features.

        Args:
            source: COBOL source code

        Returns:
            COBOLStandard enum value
        """
        source_upper = source.upper()

        # COBOL-2023 indicators
        if 'JSON GENERATE' in source_upper or 'JSON PARSE' in source_upper:
            return COBOLStandard.COBOL_2014  # JSON added in 2014

        # COBOL-2014 indicators
        if 'METHOD-ID' in source_upper and 'OVERLOAD' in source_upper:
            return COBOLStandard.COBOL_2014

        # COBOL-2002 indicators
        if any(kw in source_upper for kw in ['CLASS-ID', 'METHOD-ID', 'INVOKE', 'OBJECT REFERENCE']):
            return COBOLStandard.COBOL_2002

        if 'XML GENERATE' in source_upper or 'XML PARSE' in source_upper:
            return COBOLStandard.COBOL_2002

        # COBOL-85 indicators
        if any(kw in source_upper for kw in ['END-IF', 'END-PERFORM', 'END-READ', 'EVALUATE']):
            return COBOLStandard.COBOL_85

        # COBOL-74 (no scope terminators, no EVALUATE)
        if 'PROGRAM-ID' in source_upper:
            return COBOLStandard.COBOL_74

        return COBOLStandard.UNKNOWN

    def detect_dialect(self, source: str) -> COBOLDialect:
        """
        Detect COBOL implementation dialect.

        Args:
            source: COBOL source code

        Returns:
            COBOLDialect enum value
        """
        source_upper = source.upper()

        # IBM Enterprise COBOL indicators
        if 'EXEC SQL' in source_upper or 'EXEC CICS' in source_upper:
            return COBOLDialect.IBM

        if 'EXEC DLI' in source_upper:
            return COBOLDialect.IBM

        # Micro Focus indicators
        if '$SET' in source_upper or 'SOURCEFORMAT' in source_upper:
            return COBOLDialect.MICRO_FOCUS

        # GnuCOBOL indicators
        if '>>' in source_upper and ('SOURCE' in source_upper or 'IF' in source_upper):
            return COBOLDialect.GNU

        # ACUCOBOL indicators
        if 'C$' in source_upper:
            return COBOLDialect.ACUCOBOL

        return COBOLDialect.STANDARD

    def detect_features(self, source: str) -> Dict[str, bool]:
        """
        Detect advanced COBOL features in source.

        Args:
            source: COBOL source code

        Returns:
            Dictionary of feature flags
        """
        source_upper = source.upper()

        return {
            'has_oop_features': any(kw in source_upper for kw in ['CLASS-ID', 'METHOD-ID', 'INVOKE']),
            'has_exec_sql': 'EXEC SQL' in source_upper,
            'has_exec_cics': 'EXEC CICS' in source_upper,
            'has_xml_json': any(kw in source_upper for kw in ['XML GENERATE', 'XML PARSE', 'JSON GENERATE', 'JSON PARSE'])
        }

    def normalize_fixed_format(self, line: str) -> str:
        """
        Normalize fixed-format COBOL line.

        Args:
            line: Fixed-format COBOL line

        Returns:
            Normalized line (columns 8-72 only)
        """
        if len(line) < 7:
            return line

        # Check indicator column (column 7)
        indicator = line[6] if len(line) > 6 else ' '

        # Skip comment lines
        if indicator in ['*', '/']:
            return ''

        # Skip debug lines (D in column 7)
        if indicator == 'D':
            return ''

        # Extract code area (columns 8-72)
        if len(line) >= 72:
            code = line[7:72]
        else:
            code = line[7:] if len(line) > 7 else ''

        return code.rstrip()

    def preprocess_source(self, source: str) -> List[str]:
        """
        Preprocess COBOL source code.

        Args:
            source: Raw COBOL source

        Returns:
            List of normalized lines
        """
        lines = source.split('\n')
        normalized = []

        for line in lines:
            if self.current_format == COBOLFormat.FIXED:
                normalized_line = self.normalize_fixed_format(line)
            else:
                # For free format, just strip and remove comments
                normalized_line = line.strip()
                if normalized_line.startswith('*>'):
                    normalized_line = ''

            if normalized_line:  # Skip empty lines
                normalized.append(normalized_line)

        return normalized

    def extract_program_id(self, lines: List[str]) -> Optional[str]:
        """
        Extract PROGRAM-ID from source.

        Args:
            lines: Normalized source lines

        Returns:
            Program ID or None
        """
        for line in lines:
            if 'PROGRAM-ID' in line.upper():
                # Extract program name after PROGRAM-ID
                match = re.search(r'PROGRAM-ID\.\s+(\S+)', line.upper())
                if match:
                    return match.group(1)
        return None

    def is_procedure_division_start(self, line: str) -> bool:
        """Check if line marks start of PROCEDURE DIVISION"""
        return 'PROCEDURE' in line.upper() and 'DIVISION' in line.upper()

    def is_section_or_paragraph(self, line: str) -> Optional[Tuple[str, str]]:
        """
        Check if line is a SECTION or PARAGRAPH definition.

        Args:
            line: Source line

        Returns:
            Tuple of (name, type) or None
        """
        line_upper = line.upper().strip()

        # Check for SECTION
        if line_upper.endswith('SECTION.') or ' SECTION.' in line_upper:
            # Extract section name
            name = line_upper.replace('SECTION.', '').strip()
            return (name, 'SECTION')

        # Check for PARAGRAPH (ends with period, no SECTION keyword)
        if line_upper.endswith('.') and not any(kw in line_upper for kw in ['DIVISION', 'SECTION']):
            # Check if it's a paragraph (not a statement)
            # Paragraphs typically start at beginning of line or after whitespace
            # and contain hyphenated names
            words = line_upper.strip().rstrip('.').split()
            if len(words) == 1 and '-' in words[0]:
                return (words[0], 'PARAGRAPH')
            # Also handle single-word paragraphs
            if len(words) == 1 and words[0].replace('-', '').isalnum():
                return (words[0], 'PARAGRAPH')

        return None

    def extract_verb(self, line: str) -> Optional[str]:
        """
        Extract COBOL verb from statement.

        Args:
            line: Statement line

        Returns:
            Verb or None
        """
        line_upper = line.upper().strip()
        words = line_upper.split()

        if not words:
            return None

        # Check first word (most common case)
        if words[0] in self.COBOL_VERBS:
            return words[0]

        # Handle special cases
        # IF ... THEN
        if words[0] == 'IF':
            return 'IF'

        # GO TO
        if words[0] == 'GO' and len(words) > 1 and words[1] == 'TO':
            return 'GO'

        # EXEC SQL (embedded SQL)
        if words[0] == 'EXEC' and len(words) > 1:
            if words[1] == 'SQL':
                # Find SQL verb
                for word in words[2:]:
                    if word in self.COBOL_VERBS:
                        return word
            return 'EXEC'

        return None

    def parse_procedure(self, lines: List[str], start_idx: int, name: str, proc_type: str) -> Procedure:
        """
        Parse a single procedure (SECTION or PARAGRAPH).

        Args:
            lines: Source lines
            start_idx: Starting line index
            name: Procedure name
            proc_type: 'SECTION' or 'PARAGRAPH'

        Returns:
            Parsed Procedure object
        """
        procedure = Procedure(
            name=name,
            type=proc_type,
            line_number=start_idx + 1
        )

        # Parse statements until next procedure or end
        idx = start_idx + 1
        while idx < len(lines):
            line = lines[idx].strip()

            # Check if we've hit another procedure
            if self.is_section_or_paragraph(line):
                break

            # Extract verb and create statement
            verb = self.extract_verb(line)
            if verb:
                statement = COBOLStatement(
                    verb=verb,
                    line_number=idx + 1,
                    full_text=line
                )
                procedure.statements.append(statement)

                # Track PERFORM targets
                if verb == 'PERFORM':
                    # Extract performed procedure name
                    match = re.search(r'PERFORM\s+([A-Z0-9-]+)', line.upper())
                    if match:
                        procedure.performed_procedures.append(match.group(1))

            idx += 1

        return procedure

    def parse_file(self, filepath: str) -> COBOLProgram:
        """
        Parse COBOL source file.

        Args:
            filepath: Path to COBOL source file

        Returns:
            Parsed COBOLProgram object
        """
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            source = f.read()

        return self.parse_source(source)

    def parse_source(self, source: str) -> COBOLProgram:
        """
        Parse COBOL source code.

        Args:
            source: COBOL source code

        Returns:
            Parsed COBOLProgram object
        """
        # Detect format, standard, and dialect
        self.current_format = self.detect_format(source)
        standard = self.detect_standard(source)
        dialect = self.detect_dialect(source)
        features = self.detect_features(source)

        # Preprocess
        lines = self.preprocess_source(source)

        # Extract program ID
        program_id = self.extract_program_id(lines) or 'UNKNOWN'

        # Find PROCEDURE DIVISION
        proc_div_idx = None
        for idx, line in enumerate(lines):
            if self.is_procedure_division_start(line):
                proc_div_idx = idx
                break

        if proc_div_idx is None:
            # No PROCEDURE DIVISION found
            return COBOLProgram(
                program_id=program_id,
                source_format=self.current_format,
                standard=standard,
                dialect=dialect,
                **features
            )

        # Parse procedures
        procedures = []
        idx = proc_div_idx + 1

        while idx < len(lines):
            line = lines[idx]
            result = self.is_section_or_paragraph(line)

            if result:
                name, proc_type = result
                procedure = self.parse_procedure(lines, idx, name, proc_type)
                procedures.append(procedure)

                # Skip to next procedure
                idx += len(procedure.statements) + 1
            else:
                idx += 1

        return COBOLProgram(
            program_id=program_id,
            procedures=procedures,
            source_format=self.current_format,
            standard=standard,
            dialect=dialect,
            **features
        )

    def extract_procedures(self, program: COBOLProgram) -> List[Procedure]:
        """
        Extract all procedures from parsed program.

        Args:
            program: Parsed COBOLProgram

        Returns:
            List of Procedure objects
        """
        return program.procedures
