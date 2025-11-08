"""
Tests for CopybookFinder
"""

import pytest
from pathlib import Path
import tempfile
import shutil

from cobol_harmonizer.copybook.finder import CopybookFinder
from cobol_harmonizer.copybook.models import CopybookConfig


class TestCopybookFinder:
    """Test suite for CopybookFinder"""

    @pytest.fixture
    def temp_dir(self):
        """Create a temporary directory for test files"""
        temp_dir = tempfile.mkdtemp()
        yield Path(temp_dir)
        shutil.rmtree(temp_dir)

    @pytest.fixture
    def finder_with_fixtures(self):
        """Create a finder with test fixtures directory"""
        fixtures_dir = Path(__file__).parent.parent / "fixtures"
        config = CopybookConfig(search_paths=[str(fixtures_dir)])
        return CopybookFinder(config)

    def test_find_existing_copybook(self, finder_with_fixtures):
        """Test finding an existing copybook"""
        result = finder_with_fixtures.find("TEST-RECORD")
        assert result is not None
        assert "TEST-RECORD.cpy" in result

    def test_find_nested_copybook(self, finder_with_fixtures):
        """Test finding a nested copybook"""
        result = finder_with_fixtures.find("NESTED-RECORD")
        assert result is not None
        assert "NESTED-RECORD.cpy" in result

    def test_find_nonexistent_copybook(self, finder_with_fixtures):
        """Test finding a copybook that doesn't exist"""
        result = finder_with_fixtures.find("NONEXISTENT")
        assert result is None

    def test_case_insensitive_search(self, temp_dir):
        """Test that search is case-insensitive"""
        # Create copybook with lowercase name
        copybook_file = temp_dir / "test-record.cpy"
        copybook_file.write_text("01 TEST.")

        config = CopybookConfig(search_paths=[str(temp_dir)])
        finder = CopybookFinder(config)

        # Should find with uppercase name
        result = finder.find("TEST-RECORD")
        assert result is not None

    def test_multiple_search_paths(self, temp_dir):
        """Test searching multiple paths in order"""
        # Create two directories
        dir1 = temp_dir / "dir1"
        dir2 = temp_dir / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        # Create same copybook in both (different content)
        (dir1 / "COPY1.cpy").write_text("01 DIR1.")
        (dir2 / "COPY1.cpy").write_text("01 DIR2.")

        # First path should be checked first
        config = CopybookConfig(search_paths=[str(dir1), str(dir2)])
        finder = CopybookFinder(config)
        result = finder.find("COPY1")
        assert "dir1" in result

        # Reverse order
        config2 = CopybookConfig(search_paths=[str(dir2), str(dir1)])
        finder = CopybookFinder(config2)
        result = finder.find("COPY1")
        assert "dir2" in result

    def test_different_extensions(self, temp_dir):
        """Test finding copybooks with different extensions"""
        # Create copybooks with various extensions
        (temp_dir / "COPY1.cpy").write_text("01 CPY.")
        (temp_dir / "COPY2.cbl").write_text("01 CBL.")
        (temp_dir / "COPY3.COB").write_text("01 COB.")

        config = CopybookConfig(search_paths=[str(temp_dir)])
        finder = CopybookFinder(config)

        assert finder.find("COPY1") is not None
        assert finder.find("COPY2") is not None
        assert finder.find("COPY3") is not None

    def test_cache_functionality(self, finder_with_fixtures):
        """Test that results are cached"""
        # First call
        result1 = finder_with_fixtures.find("TEST-RECORD")

        # Second call should return cached result
        result2 = finder_with_fixtures.find("TEST-RECORD")

        assert result1 == result2

    def test_normalize_name(self, finder_with_fixtures):
        """Test name normalization"""
        # Should handle names with or without extensions
        result1 = finder_with_fixtures.find("TEST-RECORD")
        result2 = finder_with_fixtures.find("TEST-RECORD.cpy")

        assert result1 == result2

    def test_empty_search_paths(self):
        """Test finder with no search paths"""
        config = CopybookConfig(search_paths=[])
        finder = CopybookFinder(config)
        result = finder.find("ANYTHING")
        assert result is None

    def test_nonexistent_search_path(self):
        """Test finder with nonexistent search path"""
        config = CopybookConfig(search_paths=["/nonexistent/path"])
        finder = CopybookFinder(config)
        result = finder.find("ANYTHING")
        assert result is None
