"""
Copybook file finder

Locates copybook files in configured search paths
"""

import logging
from pathlib import Path
from typing import Optional, List
from .models import CopybookConfig

logger = logging.getLogger(__name__)


class CopybookFinder:
    """
    Finds copybook files in configured search paths

    Handles various naming conventions and file extensions common in
    COBOL environments (mainframe, Unix, Windows).
    """

    def __init__(self, config: CopybookConfig):
        """
        Initialize copybook finder

        Args:
            config: Copybook configuration including search paths
        """
        self.config = config
        self._cache: dict[str, Optional[str]] = {}  # name → path cache

    def find(self, copybook_name: str) -> Optional[str]:
        """
        Find a copybook file by name

        Searches through configured paths and tries various combinations
        of prefixes and extensions.

        Args:
            copybook_name: Name of copybook (e.g., "CUSTOMER-RECORD")

        Returns:
            Absolute path to copybook file, or None if not found

        Example:
            finder = CopybookFinder(config)
            path = finder.find("CUSTOMER-RECORD")
            # Might find: /copybooks/CUSTOMER-RECORD.cpy
        """
        # Check cache first
        if copybook_name in self._cache:
            return self._cache[copybook_name]

        # Normalize copybook name
        normalized_name = self._normalize_name(copybook_name)

        # Try to find the file
        path = self._search_for_copybook(normalized_name)

        # Cache result (even if None)
        self._cache[copybook_name] = path

        if path:
            logger.debug(f"Found copybook '{copybook_name}' at: {path}")
        else:
            logger.warning(f"Copybook '{copybook_name}' not found in search paths")

        return path

    def _normalize_name(self, name: str) -> str:
        """
        Normalize copybook name

        Removes quotes, strips whitespace, handles common variations.

        Args:
            name: Raw copybook name from COPY statement

        Returns:
            Normalized name
        """
        # Remove quotes (single or double)
        name = name.strip().strip('"').strip("'")

        # Remove extension if present (will try all extensions anyway)
        for ext in self.config.extensions:
            if ext and name.endswith(ext):
                name = name[: -len(ext)]
                break

        return name

    def _search_for_copybook(self, name: str) -> Optional[str]:
        """
        Search for copybook in all configured paths

        Tries all combinations of:
        - Search paths
        - Prefixes
        - Extensions
        - Case variations (if on case-insensitive filesystem)

        Args:
            name: Normalized copybook name

        Returns:
            Path to file if found, None otherwise
        """
        for search_path in self.config.search_paths:
            path = Path(search_path)

            if not path.exists():
                logger.debug(f"Search path does not exist: {search_path}")
                continue

            # Try all prefix + extension combinations
            for prefix in self.config.prefixes:
                for extension in self.config.extensions:
                    candidate = self._build_candidate_path(
                        path, prefix, name, extension
                    )

                    if candidate.exists() and candidate.is_file():
                        return str(candidate.resolve())

                    # Try case-insensitive match
                    case_insensitive = self._try_case_insensitive(
                        path, prefix, name, extension
                    )
                    if case_insensitive:
                        return str(case_insensitive.resolve())

        return None

    def _build_candidate_path(
        self, search_path: Path, prefix: str, name: str, extension: str
    ) -> Path:
        """
        Build a candidate file path

        Args:
            search_path: Base search directory
            prefix: Prefix to try (e.g., "C$")
            name: Copybook name
            extension: Extension to try (e.g., ".cpy")

        Returns:
            Candidate path
        """
        filename = f"{prefix}{name}{extension}"
        return search_path / filename

    def _try_case_insensitive(
        self, search_path: Path, prefix: str, name: str, extension: str
    ) -> Optional[Path]:
        """
        Try to find file with case-insensitive match

        This is useful when code was written on mainframe (case-insensitive)
        but is now on Unix (case-sensitive).

        Args:
            search_path: Directory to search
            prefix: Prefix to try
            name: Copybook name
            extension: Extension to try

        Returns:
            Path if found, None otherwise
        """
        if not search_path.exists():
            return None

        target_name = f"{prefix}{name}{extension}".lower()

        try:
            for file_path in search_path.iterdir():
                if file_path.is_file() and file_path.name.lower() == target_name:
                    return file_path
        except PermissionError:
            logger.warning(f"Permission denied accessing: {search_path}")

        return None

    def clear_cache(self):
        """Clear the find cache"""
        self._cache.clear()
        logger.debug("Copybook finder cache cleared")

    def get_search_summary(self) -> dict:
        """
        Get summary of search configuration

        Returns:
            Dictionary with search statistics and configuration
        """
        return {
            "search_paths": self.config.search_paths,
            "extensions": self.config.extensions,
            "prefixes": self.config.prefixes,
            "cache_size": len(self._cache),
            "cache_hits": sum(1 for v in self._cache.values() if v is not None),
            "cache_misses": sum(1 for v in self._cache.values() if v is None),
        }

    def __repr__(self) -> str:
        return f"CopybookFinder(paths={len(self.config.search_paths)})"
