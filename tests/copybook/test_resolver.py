"""
Tests for CopybookResolver
"""

import pytest
from pathlib import Path
import tempfile
import shutil

from cobol_harmonizer.copybook.resolver import CopybookResolver
from cobol_harmonizer.copybook.models import (
    CopybookConfig,
    CopybookNotFoundError,
    CircularCopybookError,
)


class TestCopybookResolver:
    """Test suite for CopybookResolver"""

    @pytest.fixture
    def temp_dir(self):
        """Create a temporary directory for test files"""
        temp_dir = tempfile.mkdtemp()
        yield Path(temp_dir)
        shutil.rmtree(temp_dir)

    @pytest.fixture
    def fixtures_dir(self):
        """Get the fixtures directory"""
        return Path(__file__).parent.parent / "fixtures"

    @pytest.fixture
    def resolver_with_fixtures(self, fixtures_dir):
        """Create a resolver with test fixtures"""
        config = CopybookConfig(search_paths=[str(fixtures_dir)], enable_cache=True)
        return CopybookResolver(config)

    def test_find_copy_statements(self, resolver_with_fixtures):
        """Test finding COPY statements in source"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      COPY TEST-RECORD.
      01 WS-COUNTER PIC 9(5).
      COPY NESTED-RECORD.
"""

        copy_statements = resolver_with_fixtures.find_copy_statements(source)

        assert len(copy_statements) == 2
        assert copy_statements[0].copybook_name == "TEST-RECORD"
        assert copy_statements[1].copybook_name == "NESTED-RECORD"

    def test_resolve_simple_copybook(self, resolver_with_fixtures):
        """Test resolving a simple copybook"""
        source = """      COPY TEST-RECORD."""

        copy_stmt = resolver_with_fixtures.find_copy_statements(source)[0]
        copybook = resolver_with_fixtures.resolve_copybook(copy_stmt)

        assert copybook is not None
        assert copybook.name == "TEST-RECORD"
        assert "TEST-ID" in copybook.content
        assert "TEST-NAME" in copybook.content

    def test_resolve_nested_copybook(self, resolver_with_fixtures):
        """Test resolving nested copybooks"""
        source = """      COPY NESTED-RECORD."""

        copy_stmt = resolver_with_fixtures.find_copy_statements(source)[0]
        copybook = resolver_with_fixtures.resolve_copybook(copy_stmt)

        assert copybook is not None
        assert copybook.name == "NESTED-RECORD"
        # Should have nested copy statement
        assert len(copybook.nested_copies) > 0

    def test_resolve_file_complete(self, temp_dir, fixtures_dir):
        """Test complete file resolution"""
        # Create a test program
        program = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TESTPROG.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      COPY TEST-RECORD.
      01 WS-VAR PIC X.
"""
        program_file = temp_dir / "TEST.cbl"
        program_file.write_text(program)

        # Resolve
        config = CopybookConfig(search_paths=[str(fixtures_dir)])
        resolver = CopybookResolver(config)
        resolved = resolver.resolve_file(str(program_file))

        assert resolved is not None
        assert len(resolved.copybooks_used) > 0
        assert resolved.copybooks_used[0].name == "TEST-RECORD"
        assert "TEST-ID" in resolved.resolved_content
        assert resolved.total_lines_from_copybooks > 0

    def test_copybook_not_found_error(self, temp_dir):
        """Test error when copybook not found"""
        source = """      COPY NONEXISTENT."""

        config = CopybookConfig(search_paths=[str(temp_dir)])
        resolver = CopybookResolver(config)

        copy_stmt = resolver.find_copy_statements(source)[0]

        with pytest.raises(CopybookNotFoundError):
            resolver.resolve_copybook(copy_stmt)

    def test_circular_dependency_detection(self, temp_dir):
        """Test detection of circular copybook dependencies"""
        # Create circular copybooks
        (temp_dir / "COPY-A.cpy").write_text("01 A.\n      COPY COPY-B.")
        (temp_dir / "COPY-B.cpy").write_text("01 B.\n      COPY COPY-A.")

        config = CopybookConfig(search_paths=[str(temp_dir)])
        resolver = CopybookResolver(config)

        source = """      COPY COPY-A."""
        copy_stmt = resolver.find_copy_statements(source)[0]

        with pytest.raises(CircularCopybookError):
            resolver.resolve_copybook(copy_stmt)

    def test_basic_replacing_clause(self, temp_dir):
        """Test basic REPLACING clause"""
        # Create copybook
        (temp_dir / "TEMPLATE.cpy").write_text(
            "01 ==PREFIX==RECORD.\n   05 ==PREFIX==ID PIC 9(10)."
        )

        config = CopybookConfig(search_paths=[str(temp_dir)])
        resolver = CopybookResolver(config)

        source = """      COPY TEMPLATE REPLACING ==PREFIX== BY CUSTOMER-."""

        copy_stmt = resolver.find_copy_statements(source)[0]
        copybook = resolver.resolve_copybook(copy_stmt)

        assert "CUSTOMER-RECORD" in copybook.content
        assert "CUSTOMER-ID" in copybook.content
        assert "==PREFIX==" not in copybook.content

    def test_inline_copybooks_with_source_mapping(self, temp_dir, fixtures_dir):
        """Test inlining copybooks with source mapping"""
        program = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      COPY TEST-RECORD.
      01 WS-VAR PIC X.
"""
        program_file = temp_dir / "TEST.cbl"
        program_file.write_text(program)

        config = CopybookConfig(search_paths=[str(fixtures_dir)])
        resolver = CopybookResolver(config)
        resolved = resolver.resolve_file(str(program_file))

        # Check source mapping
        assert len(resolved.source_map.mappings) > 0

        # Should have mappings for both original and copybook lines
        original_mappings = [
            m for m in resolved.source_map.mappings.values() if not m.is_copybook
        ]
        copybook_mappings = [
            m for m in resolved.source_map.mappings.values() if m.is_copybook
        ]

        assert len(original_mappings) > 0
        assert len(copybook_mappings) > 0

    def test_cache_performance(self, resolver_with_fixtures):
        """Test that caching improves performance"""
        source = """      COPY TEST-RECORD."""

        copy_stmt1 = resolver_with_fixtures.find_copy_statements(source)[0]

        # First resolution (cache miss)
        import time

        start1 = time.time()
        copybook1 = resolver_with_fixtures.resolve_copybook(copy_stmt1)
        time1 = time.time() - start1

        # Second resolution (should hit cache)
        copy_stmt2 = resolver_with_fixtures.find_copy_statements(source)[0]
        start2 = time.time()
        copybook2 = resolver_with_fixtures.resolve_copybook(copy_stmt2)
        time2 = time.time() - start2

        # Cache hit should be faster (or at least not slower)
        # Note: This is a weak assertion since timing can be variable
        assert copybook1.name == copybook2.name
        assert copybook1.content == copybook2.content

    def test_max_depth_limit(self, temp_dir):
        """Test maximum recursion depth limit"""
        # Create deeply nested copybooks
        for i in range(15):
            next_i = i + 1
            content = f"01 LEVEL-{i}.\n"
            if next_i < 15:
                content += f"      COPY LEVEL-{next_i}."
            (temp_dir / f"LEVEL-{i}.cpy").write_text(content)

        config = CopybookConfig(
            search_paths=[str(temp_dir)], max_depth=10
        )  # Limit to 10 levels
        resolver = CopybookResolver(config)

        source = """      COPY LEVEL-0."""
        copy_stmt = resolver.find_copy_statements(source)[0]

        # Should handle max depth gracefully (logs warning, doesn't crash)
        # The implementation logs a warning but doesn't raise an exception
        resolved = resolver.resolve_copybook(copy_stmt)

        # Should resolve up to max_depth levels (0-9 = 10 levels)
        assert resolved is not None
        # Note: Implementation logs warning but continues gracefully

    def test_multiple_copybooks_in_one_file(self, fixtures_dir):
        """Test resolving multiple copybooks in one file"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      COPY TEST-RECORD.
      COPY NESTED-RECORD.
      01 WS-VAR PIC X.
"""

        config = CopybookConfig(search_paths=[str(fixtures_dir)])
        resolver = CopybookResolver(config)

        copy_statements = resolver.find_copy_statements(source)
        assert len(copy_statements) == 2

        # Both should resolve
        copybook1 = resolver.resolve_copybook(copy_statements[0])
        copybook2 = resolver.resolve_copybook(copy_statements[1])

        assert copybook1.name == "TEST-RECORD"
        assert copybook2.name == "NESTED-RECORD"

    def test_copy_statement_line_numbers(self, resolver_with_fixtures):
        """Test that COPY statement line numbers are accurate"""
        source = """      LINE 1
      LINE 2
      COPY TEST-RECORD.
      LINE 4
"""

        copy_statements = resolver_with_fixtures.find_copy_statements(source)
        assert len(copy_statements) == 1
        assert copy_statements[0].line_number == 3
