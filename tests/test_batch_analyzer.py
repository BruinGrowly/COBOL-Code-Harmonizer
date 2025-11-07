"""
Tests for BatchAnalyzer
"""

import pytest
import tempfile
from pathlib import Path
from cobol_harmonizer.batch_analyzer import BatchAnalyzer


class TestBatchAnalyzer:
    """Test suite for BatchAnalyzer"""

    def setup_method(self):
        """Set up test fixtures"""
        self.analyzer = BatchAnalyzer(max_workers=2)

    def test_initialization(self):
        """Test BatchAnalyzer initialization"""
        assert self.analyzer.max_workers == 2
        assert self.analyzer.parser is not None
        assert self.analyzer.intent_extractor is not None
        assert self.analyzer.execution_analyzer is not None
        assert self.analyzer.calculator is not None

    def test_find_cobol_files(self):
        """Test finding COBOL files in directory"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create some test files
            (tmppath / "test1.cbl").touch()
            (tmppath / "test2.CBL").touch()
            (tmppath / "test3.cob").touch()
            (tmppath / "readme.txt").touch()

            # Create subdirectory
            subdir = tmppath / "subdir"
            subdir.mkdir()
            (subdir / "test4.cbl").touch()

            # Find files non-recursively
            files = self.analyzer._find_cobol_files(str(tmppath), "*.cbl", recursive=False)
            # Should find test1.cbl and test2.CBL (case variations)
            assert len(files) >= 2

            # Find files recursively
            files_recursive = self.analyzer._find_cobol_files(
                str(tmppath), "*.cbl", recursive=True
            )
            # Should find more files including subdirectory
            assert len(files_recursive) >= len(files)

    def test_analyze_single_file(self):
        """Test analyzing a single COBOL file"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test COBOL file
            test_file = Path(tmpdir) / "test.cbl"
            test_file.write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       PROCEDURE DIVISION.
       GET-DATA.
           READ CUSTOMER-FILE.
           STOP RUN.
    """)

            result = self.analyzer._analyze_single_file(str(test_file))

            # Check result structure
            assert result["status"] == "success"
            assert result["file_path"] == str(test_file)
            assert result["program_id"] == "TEST-PROG"
            assert "results" in result

    def test_analyze_file_not_found(self):
        """Test analyzing non-existent file"""
        result = self.analyzer._analyze_single_file("/nonexistent/file.cbl")

        assert result["status"] == "error"
        assert "error" in result

    def test_analyze_file_no_procedures(self):
        """Test file with no procedures"""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test.cbl"
            test_file.write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
    """)

            result = self.analyzer._analyze_single_file(str(test_file))

            assert result["status"] == "no_procedures"

    def test_analyze_files(self):
        """Test analyzing multiple files"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create test files
            file1 = tmppath / "test1.cbl"
            file1.write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1.
       PROCEDURE DIVISION.
       GET-DATA.
           READ CUSTOMER-FILE.
    """)

            file2 = tmppath / "test2.cbl"
            file2.write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.
       PROCEDURE DIVISION.
       VALIDATE-DATA.
           IF CUSTOMER-STATUS = 'ACTIVE'
               CONTINUE
           END-IF.
    """)

            results = self.analyzer.analyze_files([str(file1), str(file2)])

            # Check results
            assert results["status"] == "completed"
            assert results["files_analyzed"] == 2
            assert results["total_files"] == 2
            assert "statistics" in results
            assert "file_results" in results

    def test_analyze_directory(self):
        """Test analyzing entire directory"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create test files
            (tmppath / "test1.cbl").write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY 'HELLO'.
    """)

            (tmppath / "test2.cbl").write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.
       PROCEDURE DIVISION.
       PROCESS.
           COMPUTE X = Y + Z.
    """)

            results = self.analyzer.analyze_directory(str(tmppath))

            assert results["status"] == "completed"
            assert results["files_analyzed"] >= 2

    def test_analyze_directory_empty(self):
        """Test analyzing empty directory"""
        with tempfile.TemporaryDirectory() as tmpdir:
            results = self.analyzer.analyze_directory(tmpdir)

            assert results["status"] == "no_files"
            assert results["files_analyzed"] == 0

    def test_calculate_batch_statistics(self):
        """Test batch statistics calculation"""
        file_results = [
            {
                "file_path": "file1.cbl",
                "total_procedures": 3,
                "results": [
                    {"is_harmonious": True, "requires_action": False,
                     "severity_level": "harmonious", "disharmony_score": 0.1},
                    {"is_harmonious": False, "requires_action": True,
                     "severity_level": "critical", "disharmony_score": 1.2}
                ]
            },
            {
                "file_path": "file2.cbl",
                "total_procedures": 2,
                "results": [
                    {"is_harmonious": True, "requires_action": False,
                     "severity_level": "harmonious", "disharmony_score": 0.2}
                ]
            }
        ]

        stats = self.analyzer._calculate_batch_statistics(file_results, threshold=0.5)

        assert stats["total_procedures"] == 5  # 3 + 2
        assert stats["harmonious_count"] == 2
        assert stats["disharmonious_count"] == 1
        assert stats["requires_action_count"] == 1
        assert "severity_breakdown" in stats
        assert stats["severity_breakdown"]["harmonious"] == 2
        assert stats["severity_breakdown"]["critical"] == 1

    def test_get_worst_offenders(self):
        """Test getting worst offenders"""
        batch_results = {
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "results": [
                        {"procedure_name": "PROC1", "disharmony_score": 0.5},
                        {"procedure_name": "PROC2", "disharmony_score": 1.5}
                    ]
                },
                {
                    "file_path": "file2.cbl",
                    "results": [
                        {"procedure_name": "PROC3", "disharmony_score": 1.0}
                    ]
                }
            ]
        }

        worst = self.analyzer.get_worst_offenders(batch_results, limit=2)

        assert len(worst) == 2
        assert worst[0]["procedure_name"] == "PROC2"
        assert worst[0]["disharmony_score"] == 1.5
        assert worst[1]["procedure_name"] == "PROC3"

    def test_get_files_by_severity(self):
        """Test filtering files by severity"""
        batch_results = {
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "program_id": "PROG1",
                    "total_procedures": 3,
                    "results": [
                        {"severity_level": "critical"},
                        {"severity_level": "significant"}
                    ]
                },
                {
                    "file_path": "file2.cbl",
                    "program_id": "PROG2",
                    "total_procedures": 2,
                    "results": [
                        {"severity_level": "minor_drift"}
                    ]
                }
            ]
        }

        # Get files with concerning or higher
        files = self.analyzer.get_files_by_severity(batch_results, min_severity="concerning")

        assert len(files) == 1
        assert files[0]["file_path"] == "file1.cbl"
        assert files[0]["issue_count"] == 2

    def test_progress_callback(self):
        """Test progress callback functionality"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create test files
            (tmppath / "test1.cbl").write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY 'TEST'.
    """)

            progress_calls = []

            def progress_callback(current, total, file_path):
                progress_calls.append((current, total, file_path))

            self.analyzer.analyze_directory(
                str(tmppath),
                progress_callback=progress_callback
            )

            # Verify callback was called
            assert len(progress_calls) > 0
            # First call should have current=1, total=num_files
            assert progress_calls[0][0] == 1
