"""
Copybook caching system

Caches parsed copybooks to avoid re-parsing the same files
"""

import json
import hashlib
import logging
from pathlib import Path
from typing import Optional, Dict
from datetime import datetime, timedelta
from .models import Copybook, CopybookConfig, SourceMap

logger = logging.getLogger(__name__)


class CopybookCache:
    """
    Two-tier cache for copybooks

    1. In-memory cache (fast, limited size)
    2. Disk cache (persistent, larger)

    Cache invalidation based on file modification time and content hash.
    """

    def __init__(self, config: CopybookConfig):
        """
        Initialize copybook cache

        Args:
            config: Copybook configuration
        """
        self.config = config
        self._memory_cache: Dict[str, Copybook] = {}
        self._cache_dir = Path(config.cache_dir) if config.enable_cache else None

        if self._cache_dir:
            self._cache_dir.mkdir(parents=True, exist_ok=True)
            logger.debug(f"Copybook cache directory: {self._cache_dir}")

    def get(
        self, copybook_name: str, file_path: Optional[str] = None
    ) -> Optional[Copybook]:
        """
        Get copybook from cache

        Args:
            copybook_name: Name of copybook
            file_path: Path to copybook file (for validation)

        Returns:
            Cached Copybook if valid, None if cache miss or invalid
        """
        if not self.config.enable_cache:
            return None

        # Try memory cache first
        if copybook_name in self._memory_cache:
            cached = self._memory_cache[copybook_name]

            # Validate cache entry
            if self._is_valid_cache_entry(cached, file_path):
                logger.debug(f"Memory cache HIT: {copybook_name}")
                return cached
            else:
                # Invalid entry, remove it
                del self._memory_cache[copybook_name]

        # Try disk cache
        if self._cache_dir:
            cached = self._load_from_disk(copybook_name)
            if cached and self._is_valid_cache_entry(cached, file_path):
                logger.debug(f"Disk cache HIT: {copybook_name}")
                # Promote to memory cache
                self._memory_cache[copybook_name] = cached
                return cached

        logger.debug(f"Cache MISS: {copybook_name}")
        return None

    def put(self, copybook_name: str, copybook: Copybook):
        """
        Store copybook in cache

        Args:
            copybook_name: Name of copybook
            copybook: Parsed copybook to cache
        """
        if not self.config.enable_cache:
            return

        # Store in memory cache
        self._memory_cache[copybook_name] = copybook
        logger.debug(f"Cached in memory: {copybook_name}")

        # Store in disk cache
        if self._cache_dir:
            try:
                self._save_to_disk(copybook_name, copybook)
                logger.debug(f"Cached on disk: {copybook_name}")
            except Exception as e:
                logger.warning(f"Failed to cache {copybook_name} to disk: {e}")

    def _is_valid_cache_entry(self, cached: Copybook, file_path: Optional[str]) -> bool:
        """
        Check if cached copybook is still valid

        Args:
            cached: Cached copybook
            file_path: Path to original file (for validation)

        Returns:
            True if cache entry is still valid
        """
        # If no file path provided, assume valid
        if not file_path:
            return True

        try:
            # Check if file still exists
            path = Path(file_path)
            if not path.exists():
                return False

            # Check if file has been modified
            current_mtime = path.stat().st_mtime
            cached_mtime = Path(cached.path).stat().st_mtime

            if current_mtime != cached_mtime:
                logger.debug(f"Cache invalid: file modified - {file_path}")
                return False

            # Check content hash (if available)
            if cached.hash:
                current_hash = self._compute_file_hash(path)
                if current_hash != cached.hash:
                    logger.debug(f"Cache invalid: content changed - {file_path}")
                    return False

            return True

        except Exception as e:
            logger.warning(f"Cache validation failed for {file_path}: {e}")
            return False

    def _compute_file_hash(self, file_path: Path) -> str:
        """
        Compute SHA-256 hash of file content

        Args:
            file_path: Path to file

        Returns:
            Hex digest of file content
        """
        sha256 = hashlib.sha256()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                sha256.update(chunk)
        return sha256.hexdigest()

    def _get_cache_file_path(self, copybook_name: str) -> Path:
        """
        Get path to cache file for a copybook

        Args:
            copybook_name: Name of copybook

        Returns:
            Path to cache file
        """
        # Create safe filename from copybook name
        safe_name = copybook_name.replace("/", "_").replace("\\", "_")
        return self._cache_dir / f"{safe_name}.cache.json"

    def _save_to_disk(self, copybook_name: str, copybook: Copybook):
        """
        Save copybook to disk cache

        Args:
            copybook_name: Name of copybook
            copybook: Copybook to save
        """
        cache_file = self._get_cache_file_path(copybook_name)

        # Serialize to JSON
        cache_data = {
            "version": "1.0",
            "timestamp": datetime.now().isoformat(),
            "copybook": {
                "name": copybook.name,
                "path": copybook.path,
                "content": copybook.content,
                "hash": copybook.hash,
                "resolution_time_ms": copybook.resolution_time_ms,
                # Note: nested_copies and source_map not cached (would be too large)
            },
        }

        with open(cache_file, "w", encoding="utf-8") as f:
            json.dump(cache_data, f, indent=2)

    def _load_from_disk(self, copybook_name: str) -> Optional[Copybook]:
        """
        Load copybook from disk cache

        Args:
            copybook_name: Name of copybook

        Returns:
            Cached Copybook or None if not found/invalid
        """
        cache_file = self._get_cache_file_path(copybook_name)

        if not cache_file.exists():
            return None

        try:
            # Check cache age
            cache_age = datetime.now() - datetime.fromtimestamp(
                cache_file.stat().st_mtime
            )
            max_age = timedelta(hours=self.config.cache_ttl_hours)

            if cache_age > max_age:
                logger.debug(f"Cache expired: {copybook_name}")
                cache_file.unlink()
                return None

            # Load from JSON
            with open(cache_file, "r", encoding="utf-8") as f:
                cache_data = json.load(f)

            # Reconstruct Copybook object
            cb_data = cache_data["copybook"]
            copybook = Copybook(
                name=cb_data["name"],
                path=cb_data["path"],
                content=cb_data["content"],
                hash=cb_data.get("hash", ""),
                resolution_time_ms=cb_data.get("resolution_time_ms", 0.0),
                source_map=SourceMap(),  # Empty source map
                nested_copies=[],  # Not cached
            )

            return copybook

        except Exception as e:
            logger.warning(f"Failed to load cache for {copybook_name}: {e}")
            # Delete corrupted cache file
            try:
                cache_file.unlink()
            except:
                pass
            return None

    def clear(self):
        """Clear all caches (memory and disk)"""
        # Clear memory cache
        self._memory_cache.clear()
        logger.info("Memory cache cleared")

        # Clear disk cache
        if self._cache_dir and self._cache_dir.exists():
            try:
                for cache_file in self._cache_dir.glob("*.cache.json"):
                    cache_file.unlink()
                logger.info("Disk cache cleared")
            except Exception as e:
                logger.warning(f"Failed to clear disk cache: {e}")

    def get_stats(self) -> Dict:
        """
        Get cache statistics

        Returns:
            Dictionary with cache statistics
        """
        stats = {
            "memory_cache_size": len(self._memory_cache),
            "disk_cache_enabled": self._cache_dir is not None,
        }

        if self._cache_dir and self._cache_dir.exists():
            cache_files = list(self._cache_dir.glob("*.cache.json"))
            total_size = sum(f.stat().st_size for f in cache_files)

            stats.update(
                {
                    "disk_cache_files": len(cache_files),
                    "disk_cache_size_bytes": total_size,
                    "disk_cache_size_mb": round(total_size / (1024 * 1024), 2),
                }
            )

        return stats

    def __repr__(self) -> str:
        return (
            f"CopybookCache(memory={len(self._memory_cache)}, disk={self._cache_dir})"
        )
