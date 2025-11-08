"""
Main copybook resolver

Orchestrates copybook resolution process
"""

import re
import time
import logging
import hashlib
from pathlib import Path
from typing import List, Set, Optional
from .models import (
    CopyStatement,
    ReplacingClause,
    Copybook,
    ResolvedSource,
    SourceMap,
    SourceLocation,
    CopybookConfig,
    CopybookNotFoundError,
    CircularCopybookError,
)
from .finder import CopybookFinder
from .cache import CopybookCache

logger = logging.getLogger(__name__)


class CopybookResolver:
    """
    Main copybook resolution system

    Finds, parses, and inlines copybooks into COBOL source code.

    Example:
        config = CopybookConfig(search_paths=['/copybooks'])
        resolver = CopybookResolver(config)
        resolved = resolver.resolve_file('PROGRAM.CBL')
    """

    # Regex pattern for finding COPY statements
    # Matches: COPY CUSTOMER-RECORD.
    #          COPY SQLCA
    #          COPY CUSTOMER REPLACING ==:TAG:== BY ==CUST==.
    COPY_PATTERN = re.compile(
        r"^\s*COPY\s+([A-Z0-9\-_]+)"  # COPY followed by name
        r"(?:\s+REPLACING\s+(.+?))?"  # Optional REPLACING clause
        r"\s*\.",  # Terminating period
        re.IGNORECASE | re.MULTILINE,
    )

    # Pattern for REPLACING clause with pseudo-text (==text==)
    # Handles both: ==PREFIX== BY ==CUSTOMER== and ==PREFIX== BY CUSTOMER-
    REPLACING_PSEUDO_PATTERN = re.compile(
        r"==(.+?)==\s+BY\s+(?:==(.+?)==|([A-Z0-9\-]+))", re.IGNORECASE
    )

    # Pattern for LEADING/TRAILING REPLACING
    REPLACING_LEADING_TRAILING_PATTERN = re.compile(
        r'(LEADING|TRAILING)\s+(==|"")(.+?)\2\s+BY\s+(==|"")(.+?)\4', re.IGNORECASE
    )

    def __init__(self, config: CopybookConfig):
        """
        Initialize copybook resolver

        Args:
            config: Copybook configuration
        """
        self.config = config
        self.finder = CopybookFinder(config)
        self.cache = CopybookCache(config)

        # Track resolution to detect circular dependencies
        self._resolution_stack: Set[str] = set()

    def resolve_file(self, file_path: str) -> ResolvedSource:
        """
        Resolve all copybooks in a COBOL source file

        Args:
            file_path: Path to COBOL source file

        Returns:
            ResolvedSource with copybooks inlined

        Raises:
            CopybookNotFoundError: If a copybook cannot be found
            CircularCopybookError: If circular dependencies detected
        """
        start_time = time.time()

        # Read source file
        with open(file_path, "r", encoding="utf-8") as f:
            source_content = f.read()

        # Resolve copybooks
        resolved = self.resolve_source(file_path, source_content)

        # Calculate timing
        resolved.resolution_time_ms = (time.time() - start_time) * 1000

        logger.info(
            f"Resolved {len(resolved.copybooks_used)} copybooks in "
            f"{resolved.resolution_time_ms:.1f}ms for {file_path}"
        )

        return resolved

    def resolve_source(self, source_path: str, source_content: str) -> ResolvedSource:
        """
        Resolve copybooks in source content

        Args:
            source_path: Path to source file (for reference)
            source_content: COBOL source code

        Returns:
            ResolvedSource with copybooks inlined
        """
        # Find all COPY statements
        copy_statements = self.find_copy_statements(source_content)

        if not copy_statements:
            # No copybooks to resolve
            return ResolvedSource(
                original_path=source_path,
                resolved_content=source_content,
                copybooks_used=[],
                source_map=SourceMap(),
                total_lines=source_content.count("\n") + 1,
                total_lines_from_copybooks=0,
            )

        # Resolve each copybook
        copybooks_used = []
        for copy_stmt in copy_statements:
            try:
                copybook = self.resolve_copybook(copy_stmt)
                copy_stmt.content = copybook.content
                copy_stmt.resolved_path = copybook.path
                copybooks_used.append(copybook)
            except CopybookNotFoundError as e:
                if self.config.fail_on_not_found:
                    raise
                else:
                    logger.warning(str(e))
                    # Leave COPY statement as-is
                    copy_stmt.content = (
                        f"      *> COPYBOOK NOT FOUND: {copy_stmt.copybook_name}\n"
                    )

        # Inline copybooks
        resolved_content, source_map = self.inline_copybooks(
            source_path, source_content, copy_statements
        )

        # Calculate statistics
        total_lines = resolved_content.count("\n") + 1
        original_lines = source_content.count("\n") + 1
        copybook_lines = total_lines - original_lines

        return ResolvedSource(
            original_path=source_path,
            resolved_content=resolved_content,
            copybooks_used=copybooks_used,
            source_map=source_map,
            total_lines=total_lines,
            total_lines_from_copybooks=copybook_lines,
        )

    def find_copy_statements(self, source: str) -> List[CopyStatement]:
        """
        Find all COPY statements in source code

        Args:
            source: COBOL source code

        Returns:
            List of CopyStatement objects
        """
        statements = []
        lines = source.split("\n")

        for line_num, line in enumerate(lines, 1):
            # Skip comments and blank lines
            if len(line) < 7 or line[6] == "*":
                continue

            # Look for COPY statement
            match = self.COPY_PATTERN.search(line)
            if match:
                copybook_name = match.group(1).strip()
                replacing_text = match.group(2)

                # Parse REPLACING clause if present
                replacing_clauses = []
                if replacing_text:
                    replacing_clauses = self._parse_replacing_clause(replacing_text)

                statements.append(
                    CopyStatement(
                        copybook_name=copybook_name,
                        line_number=line_num,
                        column_start=match.start(),
                        column_end=match.end(),
                        replacing_clauses=replacing_clauses,
                    )
                )

                logger.debug(f"Found COPY {copybook_name} at line {line_num}")

        return statements

    def _parse_replacing_clause(self, replacing_text: str) -> List[ReplacingClause]:
        """
        Parse REPLACING clause - Enhanced version

        Supports:
        - Standard pseudo-text: ==OLD== BY ==NEW==
        - LEADING: LEADING ==OLD== BY ==NEW==
        - TRAILING: TRAILING ==OLD== BY ==NEW==
        - Multiple clauses

        Args:
            replacing_text: Text of REPLACING clause

        Returns:
            List of ReplacingClause objects
        """
        clauses = []

        # Try LEADING/TRAILING pattern first
        for match in self.REPLACING_LEADING_TRAILING_PATTERN.finditer(replacing_text):
            mode = match.group(1).upper()  # LEADING or TRAILING
            original = match.group(3).strip()
            replacement = match.group(5).strip()

            clauses.append(
                ReplacingClause(
                    original=original,
                    replacement=replacement,
                    is_leading=(mode == "LEADING"),
                    is_trailing=(mode == "TRAILING"),
                )
            )

        # If no LEADING/TRAILING found, try standard pseudo-text
        if not clauses:
            for match in self.REPLACING_PSEUDO_PATTERN.finditer(replacing_text):
                original = match.group(1).strip()
                # Group 2: replacement with delimiters (==TEXT==)
                # Group 3: replacement without delimiters (TEXT-)
                replacement = (match.group(2) or match.group(3)).strip()

                clauses.append(
                    ReplacingClause(
                        original=original,
                        replacement=replacement,
                    )
                )

        return clauses

    def resolve_copybook(self, copy_stmt: CopyStatement, depth: int = 0) -> Copybook:
        """
        Resolve a single copybook (recursive for nested copybooks)

        Args:
            copy_stmt: COPY statement to resolve
            depth: Current recursion depth

        Returns:
            Resolved Copybook

        Raises:
            CopybookNotFoundError: If copybook file not found
            CircularCopybookError: If circular dependency detected
        """
        start_time = time.time()

        # Check for circular dependencies
        if copy_stmt.copybook_name in self._resolution_stack:
            chain = list(self._resolution_stack) + [copy_stmt.copybook_name]
            raise CircularCopybookError(chain)

        # Check depth limit
        if depth >= self.config.max_depth:
            logger.warning(
                f"Max resolution depth ({self.config.max_depth}) reached "
                f"for {copy_stmt.copybook_name}"
            )
            return Copybook(
                name=copy_stmt.copybook_name,
                path="",
                content=f"      *> MAX DEPTH REACHED\n",
                hash="",
            )

        # Find copybook file
        file_path = self.finder.find(copy_stmt.copybook_name)
        if not file_path:
            raise CopybookNotFoundError(
                copy_stmt.copybook_name, self.config.search_paths
            )

        # Check cache
        cached = self.cache.get(copy_stmt.copybook_name, file_path)
        if cached:
            return cached

        # Mark as being resolved
        self._resolution_stack.add(copy_stmt.copybook_name)

        try:
            # Read copybook content
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()

            # Apply REPLACING clauses
            if copy_stmt.replacing_clauses:
                content = self._apply_replacing(content, copy_stmt.replacing_clauses)

            # Resolve nested COPY statements (if enabled)
            nested_copies = []
            if self.config.resolve_nested:
                nested_copies = self.find_copy_statements(content)

                for nested_copy in nested_copies:
                    nested_copybook = self.resolve_copybook(nested_copy, depth + 1)
                    # Inline the nested copybook
                    content = self._inline_single_copy(
                        content, nested_copy, nested_copybook.content
                    )

            # Compute hash
            content_hash = hashlib.sha256(content.encode()).hexdigest()

            # Create copybook object
            copybook = Copybook(
                name=copy_stmt.copybook_name,
                path=file_path,
                content=content,
                nested_copies=nested_copies,
                hash=content_hash,
                resolution_time_ms=(time.time() - start_time) * 1000,
            )

            # Cache it
            self.cache.put(copy_stmt.copybook_name, copybook)

            return copybook

        finally:
            # Remove from resolution stack
            self._resolution_stack.discard(copy_stmt.copybook_name)

    def _apply_replacing(
        self, content: str, replacing_clauses: List[ReplacingClause]
    ) -> str:
        """
        Apply REPLACING clauses to copybook content - Enhanced version

        Handles:
        - Standard replacement (anywhere in text)
        - LEADING replacement (only at start of words/lines)
        - TRAILING replacement (only at end of words/lines)
        - Preserves COBOL formatting and structure

        Args:
            content: Copybook content
            replacing_clauses: List of replacements to apply

        Returns:
            Content with replacements applied
        """
        for clause in replacing_clauses:
            if clause.is_leading:
                # LEADING: Replace at start of words
                # Match original at word boundary (start of line or after whitespace/punctuation)
                pattern = r"(^|\s|[^A-Z0-9\-_])(" + re.escape(clause.original) + r")"
                replacement = r"\1" + clause.replacement
                content = re.sub(pattern, replacement, content, flags=re.MULTILINE)

            elif clause.is_trailing:
                # TRAILING: Replace at end of words
                # Match original at word boundary (end of line or before whitespace/punctuation)
                pattern = r"(" + re.escape(clause.original) + r")($|\s|[^A-Z0-9\-_])"
                replacement = clause.replacement + r"\2"
                content = re.sub(pattern, replacement, content, flags=re.MULTILINE)

            else:
                # Standard replacement: Replace all occurrences
                # In COBOL REPLACING, we need to match the pseudo-text with delimiters
                # ==PREFIX== in copybook matches ==PREFIX== in REPLACING clause

                # Try with pseudo-text delimiters first (==TEXT==)
                pseudo_pattern = r"==" + re.escape(clause.original) + r"=="
                if re.search(pseudo_pattern, content):
                    # Replace ==PREFIX== with REPLACEMENT (no delimiters in replacement)
                    content = re.sub(pseudo_pattern, clause.replacement, content)
                else:
                    # Fallback: try without delimiters (word boundary)
                    pattern = r"\b" + re.escape(clause.original) + r"\b"
                    try:
                        content = re.sub(pattern, clause.replacement, content)
                    except:
                        # Final fallback: simple replace
                        content = content.replace(clause.original, clause.replacement)

        return content

    def inline_copybooks(
        self,
        source_path: str,
        source_content: str,
        copy_statements: List[CopyStatement],
    ) -> tuple[str, SourceMap]:
        """
        Inline all copybooks into source

        Args:
            source_path: Path to original source file
            source_content: Original source content
            copy_statements: List of COPY statements to inline

        Returns:
            Tuple of (resolved_content, source_map)
        """
        lines = source_content.split("\n")
        source_map = SourceMap()
        resolved_line_num = 1

        # Map original line numbers to COPY statements
        copy_by_line = {copy.line_number: copy for copy in copy_statements}

        # Build resolved source
        resolved_lines = []

        for line_num, line in enumerate(lines, 1):
            if line_num in copy_by_line:
                copy_stmt = copy_by_line[line_num]

                # Add comment showing what was copied
                comment = f"      *> BEGIN COPY {copy_stmt.copybook_name}"
                if copy_stmt.resolved_path:
                    comment += f" FROM {copy_stmt.resolved_path}"
                resolved_lines.append(comment)

                # Add source mapping for comment
                source_map.add_mapping(
                    resolved_line_num, SourceLocation(source_path, line_num, False)
                )
                resolved_line_num += 1

                # Inline copybook content
                if copy_stmt.content:
                    copybook_lines = copy_stmt.content.split("\n")
                    for cb_line_num, cb_line in enumerate(copybook_lines, 1):
                        resolved_lines.append(cb_line)

                        # Add source mapping for copybook line
                        source_map.add_mapping(
                            resolved_line_num,
                            SourceLocation(
                                copy_stmt.resolved_path or copy_stmt.copybook_name,
                                cb_line_num,
                                True,
                                copy_stmt.copybook_name,
                            ),
                        )
                        resolved_line_num += 1

                # Add end comment
                resolved_lines.append(f"      *> END COPY {copy_stmt.copybook_name}")
                source_map.add_mapping(
                    resolved_line_num, SourceLocation(source_path, line_num, False)
                )
                resolved_line_num += 1

            else:
                # Regular line (not a COPY statement)
                resolved_lines.append(line)

                # Add source mapping
                source_map.add_mapping(
                    resolved_line_num, SourceLocation(source_path, line_num, False)
                )
                resolved_line_num += 1

        return "\n".join(resolved_lines), source_map

    def _inline_single_copy(
        self, content: str, copy_stmt: CopyStatement, copybook_content: str
    ) -> str:
        """
        Inline a single copybook (for nested copies)

        Args:
            content: Content containing COPY statement
            copy_stmt: COPY statement to replace
            copybook_content: Content to inline

        Returns:
            Content with COPY statement replaced
        """
        lines = content.split("\n")

        if 0 < copy_stmt.line_number <= len(lines):
            # Replace the COPY statement line
            lines[copy_stmt.line_number - 1] = (
                f"      *> BEGIN COPY {copy_stmt.copybook_name}\n"
                f"{copybook_content}\n"
                f"      *> END COPY {copy_stmt.copybook_name}"
            )

        return "\n".join(lines)

    def get_stats(self) -> dict:
        """
        Get resolver statistics

        Returns:
            Dictionary with resolution statistics
        """
        return {
            "finder": self.finder.get_search_summary(),
            "cache": self.cache.get_stats(),
            "config": {
                "search_paths": len(self.config.search_paths),
                "extensions": len(self.config.extensions),
                "max_depth": self.config.max_depth,
                "cache_enabled": self.config.enable_cache,
            },
        }

    def __repr__(self) -> str:
        return f"CopybookResolver(config={self.config})"
