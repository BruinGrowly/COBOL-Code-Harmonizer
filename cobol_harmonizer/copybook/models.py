"""
Data models for copybook resolution system
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional
from pathlib import Path


# ===== Exceptions =====


class CopybookError(Exception):
    """Base exception for copybook-related errors"""

    pass


class CopybookNotFoundError(CopybookError):
    """Raised when a copybook file cannot be found"""

    def __init__(self, copybook_name: str, search_paths: List[str]):
        self.copybook_name = copybook_name
        self.search_paths = search_paths
        super().__init__(
            f"Copybook '{copybook_name}' not found in paths: {', '.join(search_paths)}"
        )


class CircularCopybookError(CopybookError):
    """Raised when circular copybook dependencies are detected"""

    def __init__(self, chain: List[str]):
        self.chain = chain
        super().__init__(f"Circular copybook dependency detected: {' → '.join(chain)}")


# ===== Core Models =====


@dataclass
class ReplacingClause:
    """
    Represents a REPLACING clause in a COPY statement

    Example:
        COPY CUSTOMER-RECORD REPLACING ==:TAG:== BY ==CUST==.
        COPY TEMPLATE REPLACING LEADING ==PRE==  BY ==CUST==.
        COPY TEMPLATE REPLACING TRAILING ==SUF== BY ==ACCT==.
    """

    original: str  # What to replace (e.g., ":TAG:")
    replacement: str  # What to replace with (e.g., "CUST")
    is_leading: bool = False  # LEADING replacement (match at start)
    is_trailing: bool = False  # TRAILING replacement (match at end)

    def __str__(self) -> str:
        prefix = (
            "LEADING " if self.is_leading else "TRAILING " if self.is_trailing else ""
        )
        return f"REPLACING {prefix}{self.original} BY {self.replacement}"


@dataclass
class CopyStatement:
    """
    Represents a COPY statement found in COBOL source

    Example:
        COPY CUSTOMER-RECORD.
        COPY SQLCA.
        COPY CUSTOMER REPLACING ==:PREFIX:== BY ==CUST==.
    """

    copybook_name: str
    line_number: int
    column_start: int
    column_end: int
    replacing_clauses: List[ReplacingClause] = field(default_factory=list)

    # Filled in during resolution
    resolved_path: Optional[str] = None
    content: Optional[str] = None
    nested_copies: List["CopyStatement"] = field(default_factory=list)

    def __str__(self) -> str:
        result = f"COPY {self.copybook_name}"
        if self.replacing_clauses:
            result += " " + " ".join(str(r) for r in self.replacing_clauses)
        return result


@dataclass
class SourceLocation:
    """
    Location in original source file (before copybook resolution)
    """

    file_path: str
    line_number: int
    is_copybook: bool
    copybook_name: Optional[str] = None

    def __str__(self) -> str:
        if self.is_copybook:
            return f"{self.copybook_name}:{self.line_number}"
        return f"{self.file_path}:{self.line_number}"


@dataclass
class SourceMap:
    """
    Maps resolved source line numbers back to original files

    This allows error messages to reference the original source location
    even after copybooks have been inlined.
    """

    mappings: Dict[int, SourceLocation] = field(default_factory=dict)

    def add_mapping(self, resolved_line: int, location: SourceLocation):
        """Add a line number mapping"""
        self.mappings[resolved_line] = location

    def get_location(self, resolved_line: int) -> Optional[SourceLocation]:
        """Get original location for a resolved line number"""
        return self.mappings.get(resolved_line)

    def __str__(self) -> str:
        return f"SourceMap({len(self.mappings)} mappings)"


@dataclass
class Copybook:
    """
    Resolved copybook information
    """

    name: str
    path: str
    content: str
    nested_copies: List[CopyStatement] = field(default_factory=list)
    hash: str = ""  # For cache invalidation
    source_map: SourceMap = field(default_factory=SourceMap)
    resolution_time_ms: float = 0.0

    def __str__(self) -> str:
        return f"Copybook(name={self.name}, path={self.path}, size={len(self.content)})"


@dataclass
class ResolvedSource:
    """
    COBOL source with all copybooks resolved and inlined
    """

    original_path: str
    resolved_content: str
    copybooks_used: List[Copybook] = field(default_factory=list)
    source_map: SourceMap = field(default_factory=SourceMap)
    total_lines: int = 0
    total_lines_from_copybooks: int = 0
    resolution_time_ms: float = 0.0

    def __str__(self) -> str:
        return (
            f"ResolvedSource(path={self.original_path}, "
            f"lines={self.total_lines}, "
            f"copybooks={len(self.copybooks_used)})"
        )


@dataclass
class CopybookConfig:
    """
    Configuration for copybook resolution

    Example:
        config = CopybookConfig(
            search_paths=['/copybooks', './copy'],
            extensions=['.cpy', '.CPY', '.cbl'],
            enable_cache=True
        )
    """

    # Search paths (in order of priority)
    search_paths: List[str] = field(
        default_factory=lambda: [
            "./copybooks",
            "./copy",
            "./COPY",
            "../copybooks",
            "../copy",
        ]
    )

    # File extensions to try (in order)
    extensions: List[str] = field(
        default_factory=lambda: [
            ".cpy",
            ".CPY",
            ".cbl",
            ".CBL",
            ".cob",
            ".COB",
            "",  # No extension (common on mainframe)
        ]
    )

    # Naming conventions to try
    # Some systems use C$COPY or COPY$ prefix
    prefixes: List[str] = field(
        default_factory=lambda: [
            "",
            "C$",
            "COPY$",
        ]
    )

    # Cache settings
    enable_cache: bool = True
    cache_dir: str = ".harmonizer-cache/copybooks"
    max_cache_size_mb: int = 100
    cache_ttl_hours: int = 24

    # Resolution settings
    max_depth: int = 10  # Prevent infinite recursion
    preserve_comments: bool = True
    resolve_nested: bool = True
    fail_on_not_found: bool = False  # If False, emit warning instead

    # Performance settings
    parallel_resolution: bool = False
    max_workers: int = 4

    def __post_init__(self):
        """Validate and normalize configuration"""
        # Convert all paths to Path objects
        self.search_paths = [str(Path(p).resolve()) for p in self.search_paths]

        # Ensure cache directory exists
        if self.enable_cache:
            cache_path = Path(self.cache_dir)
            cache_path.mkdir(parents=True, exist_ok=True)


@dataclass
class CopybookResolutionStats:
    """
    Statistics about copybook resolution
    """

    total_copybooks_found: int = 0
    total_copybooks_resolved: int = 0
    total_copybooks_cached: int = 0
    total_lines_inlined: int = 0
    total_time_ms: float = 0.0
    cache_hit_rate: float = 0.0

    def __str__(self) -> str:
        return (
            f"CopybookStats(resolved={self.total_copybooks_resolved}, "
            f"cached={self.total_copybooks_cached}, "
            f"cache_hit_rate={self.cache_hit_rate:.1%})"
        )
