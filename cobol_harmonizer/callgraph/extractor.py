"""
Call Graph Extractor

Extracts call relationships from COBOL source code:
- CALL statements (inter-program calls)
- PERFORM statements (intra-program calls)
"""

import re
from typing import List, Set, Optional
from pathlib import Path

from ..parser import COBOLParser, COBOLProgram
from .models import CallSite, CallType


class CallExtractor:
    """
    Extracts call relationships from COBOL source code

    Handles:
    - CALL 'PROGRAM' (static inter-program calls)
    - CALL WS-VAR (dynamic inter-program calls)
    - PERFORM PARAGRAPH (intra-program calls)
    - PERFORM SECTION (section calls)
    - PERFORM THRU constructs
    """

    # Pattern for CALL statements
    # Matches: CALL 'PROG', CALL "PROG", CALL WS-PROGRAM-NAME
    CALL_PATTERN = re.compile(
        r"\bCALL\s+"  # CALL keyword
        r"(?:"  # Group for either:
        r'[\'"]([A-Z0-9\-]+)[\'"]'  #   Quoted program name
        r"|"  # OR
        r"([A-Z0-9\-]+)"  #   Variable name (dynamic)
        r")",
        re.IGNORECASE,
    )

    # Pattern for PERFORM statements
    # Matches: PERFORM PARA, PERFORM 100-PROCESS, PERFORM PARA THRU PARA-END
    PERFORM_PATTERN = re.compile(
        r"\bPERFORM\s+"  # PERFORM keyword
        r"([A-Z0-9\-]+)"  # Paragraph/section name
        r"(?:\s+THRU\s+([A-Z0-9\-]+))?",  # Optional THRU clause
        re.IGNORECASE,
    )

    # Pattern to identify procedure division sections and paragraphs
    # Allow leading whitespace for COBOL's fixed-format columns 1-6
    PARAGRAPH_PATTERN = re.compile(
        r"^\s*([A-Z0-9\-]+)\s*\.",  # Paragraph name with optional leading spaces
        re.IGNORECASE | re.MULTILINE,
    )

    SECTION_PATTERN = re.compile(
        r"^\s*([A-Z0-9\-]+)\s+SECTION\s*\.",  # Section name with optional leading spaces
        re.IGNORECASE | re.MULTILINE,
    )

    def __init__(self, parser: Optional[COBOLParser] = None):
        """Initialize extractor with optional parser"""
        self.parser = parser or COBOLParser()

    def extract_from_file(self, file_path: str) -> List[CallSite]:
        """
        Extract all call sites from a COBOL file

        Args:
            file_path: Path to COBOL source file

        Returns:
            List of CallSite objects
        """
        with open(file_path, "r", encoding="utf-8", errors="replace") as f:
            source = f.read()

        # Parse the program
        program = self.parser.parse_source(source)

        return self.extract_from_source(
            source, program.program_id, Path(file_path).name
        )

    def extract_from_source(
        self, source: str, program_name: str, source_file: str
    ) -> List[CallSite]:
        """
        Extract all call sites from COBOL source code

        Args:
            source: COBOL source code
            program_name: Program name
            source_file: Source file name

        Returns:
            List of CallSite objects
        """
        call_sites = []

        # Find all procedures (paragraphs and sections) to use as callers
        procedures = self._extract_procedures(source)

        # Extract CALL statements (inter-program)
        call_sites.extend(
            self._extract_call_statements(source, program_name, source_file, procedures)
        )

        # Extract PERFORM statements (intra-program)
        call_sites.extend(
            self._extract_perform_statements(
                source, program_name, source_file, procedures
            )
        )

        return call_sites

    def _extract_procedures(self, source: str) -> dict:
        """
        Extract all procedure names (paragraphs and sections) from source - Enhanced

        Improvements:
        - Only extracts procedures after PROCEDURE DIVISION
        - Better filtering of false positives
        - Distinguishes sections from paragraphs
        - Tracks procedure types

        Returns dict mapping line numbers to (procedure_name, is_section) tuples
        """
        procedures = {}  # line_number -> (procedure_name, is_section)
        in_procedure_division = False

        lines = source.split("\n")
        for line_num, line in enumerate(lines, 1):
            # Skip comments and empty lines
            if not line.strip() or line.strip().startswith("*"):
                continue

            stripped = line.strip()

            # Check if we've entered PROCEDURE DIVISION
            if "PROCEDURE" in stripped.upper() and "DIVISION" in stripped.upper():
                in_procedure_division = True
                continue

            # Only extract procedures after PROCEDURE DIVISION
            if not in_procedure_division:
                continue

            # Check for section first (more specific pattern)
            section_match = self.SECTION_PATTERN.match(line)
            if section_match:
                section_name = section_match.group(1)
                # Filter out DIVISION keywords and other false positives
                if not any(
                    keyword in section_name.upper()
                    for keyword in [
                        "DIVISION",
                        "IDENTIFICATION",
                        "ENVIRONMENT",
                        "DATA",
                        "WORKING-STORAGE",
                        "FILE",
                        "LINKAGE",
                        "LOCAL-STORAGE",
                    ]
                ):
                    procedures[line_num] = (section_name, True)  # is_section=True
                continue

            # Check for paragraph
            para_match = self.PARAGRAPH_PATTERN.match(line)
            if para_match:
                para_name = para_match.group(1)

                # Filter out false positives:
                # - DIVISION keywords
                # - Level numbers (01, 05, etc.)
                # - COBOL reserved words at start of line
                if (
                    not para_name.endswith("DIVISION")
                    and not para_name.isdigit()
                    and not any(
                        keyword == para_name.upper()
                        for keyword in [
                            "IF",
                            "ELSE",
                            "THEN",
                            "END-IF",
                            "PERFORM",
                            "CALL",
                            "EVALUATE",
                            "END-EVALUATE",
                            "WHEN",
                            "SEARCH",
                            "END-SEARCH",
                            "READ",
                            "WRITE",
                            "OPEN",
                            "CLOSE",
                            "MOVE",
                            "ADD",
                            "SUBTRACT",
                            "MULTIPLY",
                            "DIVIDE",
                            "COMPUTE",
                            "DISPLAY",
                            "ACCEPT",
                            "STOP",
                            "GOBACK",
                            "EXIT",
                        ]
                    )
                ):
                    procedures[line_num] = (para_name, False)  # is_section=False

        return procedures

    def _find_containing_procedure(self, line_number: int, procedures: dict) -> str:
        """
        Find which procedure contains a given line number

        Args:
            line_number: Line number to check
            procedures: Dict of line_number -> (procedure_name, is_section)

        Returns:
            Name of containing procedure or "MAIN"
        """
        # Find the latest procedure before this line
        containing_proc = "MAIN"
        for proc_line, proc_info in sorted(procedures.items()):
            if proc_line <= line_number:
                # Extract procedure name from tuple
                proc_name = proc_info[0] if isinstance(proc_info, tuple) else proc_info
                containing_proc = proc_name
            else:
                break

        return containing_proc

    def _extract_call_statements(
        self, source: str, program_name: str, source_file: str, procedures: dict
    ) -> List[CallSite]:
        """Extract CALL statements from source"""
        call_sites = []
        lines = source.split("\n")

        for line_num, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith("*"):
                continue

            # Find CALL statements
            for match in self.CALL_PATTERN.finditer(line):
                # Determine if static or dynamic call
                static_name = match.group(1)  # Quoted name
                dynamic_name = match.group(2)  # Variable name

                if static_name:
                    # Static call: CALL 'SUBPROG'
                    callee = static_name
                    is_dynamic = False
                    call_type = CallType.PROGRAM_CALL
                else:
                    # Dynamic call: CALL WS-PROGRAM-NAME
                    callee = dynamic_name
                    is_dynamic = True
                    call_type = CallType.DYNAMIC_CALL

                # Find containing procedure
                caller = self._find_containing_procedure(line_num, procedures)

                call_sites.append(
                    CallSite(
                        caller=f"{program_name}.{caller}",
                        callee=callee,
                        call_type=call_type,
                        line_number=line_num,
                        source_file=source_file,
                        is_dynamic=is_dynamic,
                    )
                )

        return call_sites

    def _extract_perform_statements(
        self, source: str, program_name: str, source_file: str, procedures: dict
    ) -> List[CallSite]:
        """Extract PERFORM statements from source"""
        call_sites = []
        lines = source.split("\n")

        # Build set of known procedures for validation
        # Extract procedure names from tuples (procedure_name, is_section)
        known_procedures = set()
        procedure_types = {}  # procedure_name -> is_section
        for proc_info in procedures.values():
            if isinstance(proc_info, tuple):
                proc_name, is_section = proc_info
                known_procedures.add(proc_name)
                procedure_types[proc_name] = is_section
            else:
                known_procedures.add(proc_info)
                procedure_types[proc_info] = False

        for line_num, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith("*"):
                continue

            # Find PERFORM statements
            for match in self.PERFORM_PATTERN.finditer(line):
                target_para = match.group(1)
                thru_para = match.group(2)

                # Skip PERFORM with inline code (UNTIL, TIMES, VARYING)
                if any(
                    keyword in line.upper() for keyword in ["UNTIL", "TIMES", "VARYING"]
                ):
                    # Could still be "PERFORM PARA UNTIL" - check if target is a known procedure
                    if target_para not in known_procedures:
                        continue

                # Find containing procedure
                caller = self._find_containing_procedure(line_num, procedures)

                # Determine if it's a section or paragraph (already tracked in procedure_types)
                call_type = (
                    CallType.SECTION_PERFORM
                    if procedure_types.get(target_para, False)
                    else CallType.PROCEDURE_PERFORM
                )

                call_sites.append(
                    CallSite(
                        caller=f"{program_name}.{caller}",
                        callee=target_para,
                        call_type=call_type,
                        line_number=line_num,
                        source_file=source_file,
                        is_dynamic=False,
                    )
                )

                # If THRU clause, add the end paragraph as well
                if thru_para:
                    call_sites.append(
                        CallSite(
                            caller=f"{program_name}.{caller}",
                            callee=thru_para,
                            call_type=call_type,
                            line_number=line_num,
                            source_file=source_file,
                            is_dynamic=False,
                        )
                    )

        return call_sites

    def get_statistics(self, call_sites: List[CallSite]) -> dict:
        """
        Get statistics about extracted call sites

        Args:
            call_sites: List of call sites

        Returns:
            Dictionary of statistics
        """
        stats = {
            "total_calls": len(call_sites),
            "program_calls": sum(
                1 for cs in call_sites if cs.call_type == CallType.PROGRAM_CALL
            ),
            "dynamic_calls": sum(
                1 for cs in call_sites if cs.call_type == CallType.DYNAMIC_CALL
            ),
            "perform_statements": sum(
                1 for cs in call_sites if cs.call_type == CallType.PROCEDURE_PERFORM
            ),
            "section_calls": sum(
                1 for cs in call_sites if cs.call_type == CallType.SECTION_PERFORM
            ),
            "unique_callers": len(set(cs.caller for cs in call_sites)),
            "unique_callees": len(set(cs.callee for cs in call_sites)),
        }

        return stats
