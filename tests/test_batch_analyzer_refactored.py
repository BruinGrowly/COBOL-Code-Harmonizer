"""
Tests for Refactored BatchAnalyzer (v0.5.0+)

The BatchAnalyzer was refactored in v0.5.0 to use a generic analyzer function
instead of having built-in semantic analysis. This allows it to be used for
any type of batch COBOL analysis (semantic, compliance, callgraph, etc.)

For legacy tests of the old API, see test_batch_analyzer_legacy.py (skipped).
"""

import pytest
import tempfile
from pathlib import Path
from cobol_harmonizer.batch_analyzer import BatchAnalyzer, FileAnalysisResult, BatchAnalysisResults


# Sample analyzer function for testing
def simple_analyzer(file_path: str) -> dict:
    """Simple analyzer that counts lines"""
    with open(file_path, 'r') as f:
        lines = f.readlines()
    return {
        'line_count': len(lines),
        'file_name': Path(file_path).name,
        'analysis_type': 'simple'
    }


def failing_analyzer(file_path: str) -> dict:
    """Analyzer that always fails"""
    raise ValueError(f"Intentional failure for testing: {file_path}")


class TestRefactoredBatchAnalyzer:
    """Test suite for refactored BatchAnalyzer"""

    def test_initialization_default(self):
        """Test default initialization"""
        analyzer = BatchAnalyzer()
        assert analyzer.max_workers >= 1
        assert analyzer.enable_incremental == True
        assert analyzer.cache_dir == Path('.harmonizer-cache/batch')

    def test_initialization_custom(self):
        """Test custom initialization"""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = BatchAnalyzer(
                max_workers=4,
                enable_incremental=False,
                cache_dir=tmpdir
            )
            assert analyzer.max_workers == 4
            assert analyzer.enable_incremental == False
            assert analyzer.cache_dir == Path(tmpdir)

    def test_analyze_single_file_success(self):
        """Test successful single file analysis"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test file
            test_file = Path(tmpdir) / "test.cbl"
            test_file.write_text("       IDENTIFICATION DIVISION.\n" * 5)

            analyzer = BatchAnalyzer(max_workers=1)
            result = analyzer._analyze_single_file(str(test_file), simple_analyzer)

            assert result.success == True
            assert result.error_message is None
            assert result.metrics['line_count'] == 5
            assert result.metrics['file_name'] == 'test.cbl'
            assert result.analysis_time_ms > 0

    def test_analyze_single_file_failure(self):
        """Test single file analysis with error"""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test.cbl"
            test_file.write_text("IDENTIFICATION DIVISION.")

            analyzer = BatchAnalyzer(max_workers=1)
            result = analyzer._analyze_single_file(str(test_file), failing_analyzer)

            assert result.success == False
            assert "Intentional failure" in result.error_message
            assert result.metrics is None

    def test_analyze_files_multiple(self):
        """Test analyzing multiple files"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create test files
            for i in range(5):
                (tmppath / f"test{i}.cbl").write_text(f"Line\n" * (i + 1))

            files = [str(f) for f in tmppath.glob("*.cbl")]

            analyzer = BatchAnalyzer(max_workers=2, cache_dir=tmpdir)
            results = analyzer.analyze_files(files, simple_analyzer)

            assert results.total_files == 5
            assert results.successful == 5
            assert results.failed == 0
            assert results.skipped == 0
            assert len(results.results) == 5
            assert results.avg_time_per_file_ms > 0

    def test_analyze_files_with_failures(self):
        """Test analyzing files with some failures"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create test files
            for i in range(3):
                (tmppath / f"test{i}.cbl").write_text("Line\n")

            files = [str(f) for f in tmppath.glob("*.cbl")]

            analyzer = BatchAnalyzer(max_workers=1, cache_dir=tmpdir)
            results = analyzer.analyze_files(files, failing_analyzer)

            assert results.total_files == 3
            assert results.successful == 0
            assert results.failed == 3
            assert results.skipped == 0

    def test_incremental_analysis(self):
        """Test incremental analysis (skip unchanged files)"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            test_file = tmppath / "test.cbl"
            test_file.write_text("Line 1\n")

            analyzer = BatchAnalyzer(
                max_workers=1,
                enable_incremental=True,
                cache_dir=tmpdir
            )

            # First analysis - should process file
            results1 = analyzer.analyze_files([str(test_file)], simple_analyzer)
            assert results1.successful == 1
            assert results1.skipped == 0

            # Second analysis without changes - should skip
            results2 = analyzer.analyze_files([str(test_file)], simple_analyzer)
            assert results2.skipped == 1
            assert results2.successful == 0

            # Modify file - should process again
            test_file.write_text("Line 1\nLine 2\n")
            results3 = analyzer.analyze_files([str(test_file)], simple_analyzer)
            assert results3.successful == 1
            assert results3.skipped == 0

    def test_incremental_disabled(self):
        """Test with incremental analysis disabled"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            test_file = tmppath / "test.cbl"
            test_file.write_text("Line 1\n")

            analyzer = BatchAnalyzer(
                max_workers=1,
                enable_incremental=False,
                cache_dir=tmpdir
            )

            # First analysis
            results1 = analyzer.analyze_files([str(test_file)], simple_analyzer)
            assert results1.successful == 1

            # Second analysis - should still process (not skip)
            results2 = analyzer.analyze_files([str(test_file)], simple_analyzer)
            assert results2.successful == 1
            assert results2.skipped == 0

    def test_progress_callback(self):
        """Test progress callback functionality"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create test files
            for i in range(5):
                (tmppath / f"test{i}.cbl").write_text("Line\n")

            files = [str(f) for f in tmppath.glob("*.cbl")]

            # Track progress
            progress_updates = []

            def progress_callback(completed, total):
                progress_updates.append({
                    'completed': completed,
                    'total': total
                })

            analyzer = BatchAnalyzer(max_workers=1, cache_dir=tmpdir)
            results = analyzer.analyze_files(
                files,
                simple_analyzer,
                progress_callback=progress_callback
            )

            # Should have received progress updates
            assert len(progress_updates) == 5
            assert progress_updates[0]['completed'] == 1
            assert progress_updates[-1]['completed'] == 5
            assert all(u['total'] == 5 for u in progress_updates)

    def test_empty_file_list(self):
        """Test analyzing empty file list"""
        analyzer = BatchAnalyzer(max_workers=1)
        results = analyzer.analyze_files([], simple_analyzer)

        assert results.total_files == 0
        assert results.successful == 0
        assert results.failed == 0
        assert results.skipped == 0
        assert results.total_time_ms >= 0
        assert len(results.results) == 0

    def test_file_analysis_result_to_dict(self):
        """Test FileAnalysisResult serialization"""
        result = FileAnalysisResult(
            file_path="/path/to/file.cbl",
            success=True,
            error_message=None,
            analysis_time_ms=123.45,
            file_hash="abc123",
            metrics={'count': 42}
        )

        result_dict = result.to_dict()

        assert result_dict['file_path'] == "/path/to/file.cbl"
        assert result_dict['success'] == True
        assert result_dict['metrics']['count'] == 42

    def test_batch_analysis_results_to_dict(self):
        """Test BatchAnalysisResults serialization"""
        file_result = FileAnalysisResult(
            file_path="/test.cbl",
            success=True,
            analysis_time_ms=100.0,
            metrics={'lines': 10}
        )

        batch_results = BatchAnalysisResults(
            total_files=1,
            successful=1,
            failed=0,
            skipped=0,
            total_time_ms=150.0,
            avg_time_per_file_ms=150.0,
            results=[file_result]
        )

        results_dict = batch_results.to_dict()

        assert results_dict['total_files'] == 1
        assert results_dict['successful'] == 1
        assert len(results_dict['results']) == 1
        assert results_dict['results'][0]['metrics']['lines'] == 10

    def test_parallel_processing(self):
        """Test parallel processing with multiple workers"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create many files to benefit from parallelism
            for i in range(20):
                (tmppath / f"test{i}.cbl").write_text(f"Line\n" * (i + 1))

            files = [str(f) for f in tmppath.glob("*.cbl")]

            analyzer = BatchAnalyzer(max_workers=4, cache_dir=tmpdir)
            results = analyzer.analyze_files(files, simple_analyzer)

            assert results.total_files == 20
            assert results.successful == 20
            assert results.failed == 0

            # Verify all files were analyzed
            analyzed_files = {r.file_path for r in results.results}
            assert len(analyzed_files) == 20

    def test_nonexistent_file(self):
        """Test handling of nonexistent file"""
        analyzer = BatchAnalyzer(max_workers=1)
        result = analyzer._analyze_single_file("/nonexistent/file.cbl", simple_analyzer)

        assert result.success == False
        assert result.error_message is not None
        assert "FileNotFoundError" in result.error_message or "No such file" in result.error_message


# Integration test with real semantic analysis
class TestBatchAnalyzerIntegration:
    """Integration tests with actual COBOL analysis"""

    def test_with_semantic_analysis(self):
        """Test batch analyzer with semantic analysis"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create a simple COBOL file
            cobol_file = tmppath / "test.cbl"
            cobol_file.write_text("""
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       CALCULATE-TOTAL.
           COMPUTE WS-TOTAL = WS-A + WS-B.
       STOP RUN.
            """)

            # Define semantic analyzer function
            def semantic_analyzer(file_path: str) -> dict:
                from cobol_harmonizer.parser.cobol_parser import COBOLParser
                from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
                from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
                from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator

                parser = COBOLParser()
                intent_extractor = IntentExtractor()
                execution_analyzer = ExecutionAnalyzer()
                calculator = DisharmonyCalculator()

                program = parser.parse_file(file_path)

                results = {
                    'procedures': [],
                    'total_procedures': len(program.procedures)
                }

                for proc in program.procedures:
                    intent = intent_extractor.extract_intent(proc.name)
                    execution = execution_analyzer.analyze_procedure(proc)
                    score = calculator.calculate(intent, execution)

                    results['procedures'].append({
                        'name': proc.name,
                        'disharmony_score': score
                    })

                return results

            analyzer = BatchAnalyzer(max_workers=1, cache_dir=tmpdir)
            results = analyzer.analyze_files([str(cobol_file)], semantic_analyzer)

            assert results.successful == 1
            assert results.failed == 0

            # Check semantic analysis results
            file_result = results.results[0]
            assert file_result.metrics['total_procedures'] == 1
            assert file_result.metrics['procedures'][0]['name'] == 'CALCULATE-TOTAL'
            assert file_result.metrics['procedures'][0]['disharmony_score'] < 0.5  # Should be harmonious or minor drift


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
