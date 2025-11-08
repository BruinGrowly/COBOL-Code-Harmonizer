"""
Copybook resolution system for COBOL Code Harmonizer

This module handles COPY statement resolution, including:
- Finding copybook files in search paths
- Parsing and inlining copybook content
- Handling nested COPY statements
- Supporting REPLACING clauses
- Caching for performance
"""

from .models import (
    CopyStatement,
    ReplacingClause,
    Copybook,
    SourceMap,
    SourceLocation,
    CopybookConfig,
    ResolvedSource,
    CopybookNotFoundError,
    CircularCopybookError,
)

from .finder import CopybookFinder
from .cache import CopybookCache
from .resolver import CopybookResolver

__all__ = [
    # Models
    "CopyStatement",
    "ReplacingClause",
    "Copybook",
    "SourceMap",
    "SourceLocation",
    "CopybookConfig",
    "ResolvedSource",
    # Exceptions
    "CopybookNotFoundError",
    "CircularCopybookError",
    # Main components
    "CopybookFinder",
    "CopybookCache",
    "CopybookResolver",
]
