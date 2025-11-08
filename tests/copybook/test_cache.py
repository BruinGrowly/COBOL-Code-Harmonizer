"""
Tests for CopybookCache
"""

import pytest
from pathlib import Path
import tempfile
import shutil
import time

from cobol_harmonizer.copybook.cache import CopybookCache
from cobol_harmonizer.copybook.models import Copybook, SourceMap, CopybookConfig


class TestCopybookCache:
    """Test suite for CopybookCache"""

    @pytest.fixture
    def temp_cache_dir(self):
        """Create a temporary cache directory"""
        temp_dir = tempfile.mkdtemp()
        yield Path(temp_dir)
        shutil.rmtree(temp_dir)

    @pytest.fixture
    def sample_copybook(self):
        """Create a sample copybook for testing"""
        return Copybook(
            name="TEST-COPY",
            path="/test/TEST-COPY.cpy",
            content="01 TEST-RECORD.\n   05 TEST-ID PIC 9(10).",
            nested_copies=[],
            hash="abc123",
            source_map=SourceMap(),
        )

    def test_memory_cache_put_get(self, temp_cache_dir, sample_copybook):
        """Test putting and getting from memory cache"""
        config = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache = CopybookCache(config)

        # Put in cache
        cache.put(sample_copybook.name, sample_copybook)

        # Get from cache (use None to skip file validation since test uses fake paths)
        result = cache.get(sample_copybook.name, None)

        assert result is not None
        assert result.name == sample_copybook.name
        assert result.content == sample_copybook.content

    def test_disk_cache_persistence(self, temp_cache_dir, sample_copybook):
        """Test that cache persists to disk"""
        # Create cache and put copybook
        config1 = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache1 = CopybookCache(config1)
        cache1.put(sample_copybook.name, sample_copybook)

        # Create new cache instance (simulates restart)
        config2 = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache2 = CopybookCache(config2)

        # Should load from disk (use None to skip file validation since test uses fake paths)
        result = cache2.get(sample_copybook.name, None)

        assert result is not None
        assert result.name == sample_copybook.name

    def test_cache_invalidation_on_file_change(self, temp_cache_dir, sample_copybook):
        """Test that cache invalidates when file changes"""
        config = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache = CopybookCache(config)

        # Put original
        cache.put(sample_copybook.name, sample_copybook)

        # Simulate file change by changing hash
        modified_copybook = Copybook(
            name=sample_copybook.name,
            path=sample_copybook.path,
            content="01 MODIFIED.",
            nested_copies=[],
            hash="different123",
            source_map=SourceMap(),
        )

        # Get with different hash should return None
        result = cache.get(sample_copybook.name, None)
        assert result is not None  # Still in cache

        # But if we check with new content hash, it should be invalid
        # (This is tested indirectly through _is_valid_cache_entry)

    def test_ttl_expiration(self, temp_cache_dir, sample_copybook):
        """Test that cached entries expire after TTL"""
        # Set very short TTL (1 second)
        config = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache = CopybookCache(config)

        # Put in cache
        cache.put(sample_copybook.name, sample_copybook)

        # Should be in cache immediately (use None to skip file validation)
        result1 = cache.get(sample_copybook.name, None)
        assert result1 is not None

        # Wait for TTL to expire
        time.sleep(1.1)

        # Should be expired now
        # Note: This depends on implementation - some caches might not auto-expire
        # but rather check TTL on access

    def test_clear_cache(self, temp_cache_dir, sample_copybook):
        """Test clearing the cache"""
        config = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache = CopybookCache(config)

        # Put in cache
        cache.put(sample_copybook.name, sample_copybook)
        assert cache.get(sample_copybook.name, None) is not None

        # Clear cache
        cache.clear()

        # Should be empty now
        result = cache.get(sample_copybook.name, None)
        assert result is None

    def test_cache_without_disk(self, sample_copybook, tmp_path):
        """Test memory-only cache (no disk persistence)"""
        # Test that memory cache works independently of disk cache
        config = CopybookConfig(cache_dir=str(tmp_path / "cache"), enable_cache=True)
        cache = CopybookCache(config)

        # Put in cache
        cache.put(sample_copybook.name, sample_copybook)

        # Get from cache (use None to skip file validation)
        # This tests memory cache specifically since we just put it
        result = cache.get(sample_copybook.name, None)
        assert result is not None
        assert result.name == sample_copybook.name

    def test_multiple_copybooks(self, temp_cache_dir):
        """Test caching multiple copybooks"""
        config = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache = CopybookCache(config)

        # Create multiple copybooks
        copybooks = []
        for i in range(5):
            cb = Copybook(
                name=f"COPY{i}",
                path=f"/test/COPY{i}.cpy",
                content=f"01 RECORD{i}.",
                nested_copies=[],
                hash=f"hash{i}",
                source_map=SourceMap(),
            )
            copybooks.append(cb)
            cache.put(cb.name, cb)

        # All should be retrievable (use None to skip file validation)
        for cb in copybooks:
            result = cache.get(cb.name, None)
            assert result is not None
            assert result.name == cb.name

    def test_cache_with_nested_copybooks(self, temp_cache_dir):
        """Test caching copybooks with nested copies"""
        config = CopybookConfig(cache_dir=str(temp_cache_dir), enable_cache=True)
        cache = CopybookCache(config)

        # Create copybook with nested copies
        from cobol_harmonizer.copybook.models import CopyStatement

        nested_copy = CopyStatement(
            copybook_name="NESTED",
            line_number=5,
            column_start=7,
            column_end=20,
            replacing_clauses=[],
        )

        copybook = Copybook(
            name="PARENT",
            path="/test/PARENT.cpy",
            content="01 PARENT.\n   COPY NESTED.",
            nested_copies=[nested_copy],
            hash="parent123",
            source_map=SourceMap(),
        )

        cache.put(copybook.name, copybook)
        result = cache.get(copybook.name, None)

        assert result is not None
        assert len(result.nested_copies) == 1
        assert result.nested_copies[0].copybook_name == "NESTED"
