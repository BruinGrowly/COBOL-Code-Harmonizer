"""
Tests for Baseline
"""

import pytest
import json
import tempfile
from pathlib import Path
from cobol_harmonizer.baseline import Baseline, BaselineManager, format_comparison_summary


class TestBaseline:
    """Test suite for Baseline"""

    def setup_method(self):
        """Set up test fixtures"""
        self.sample_batch_results = {
            "statistics": {"total_procedures": 5, "harmonious_count": 3, "disharmonious_count": 2},
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "program_id": "PROG1",
                    "total_procedures": 3,
                    "results": [
                        {
                            "procedure_name": "PROC1",
                            "disharmony_score": 0.2,
                            "severity_level": "harmonious",
                            "is_harmonious": True,
                            "requires_action": False,
                            "intent_coords": {
                                "love": 0.1,
                                "justice": 0.2,
                                "power": 0.1,
                                "wisdom": 0.6,
                            },
                            "execution_coords": {
                                "love": 0.1,
                                "justice": 0.3,
                                "power": 0.0,
                                "wisdom": 0.6,
                            },
                        },
                        {
                            "procedure_name": "PROC2",
                            "disharmony_score": 1.0,
                            "severity_level": "significant",
                            "is_harmonious": False,
                            "requires_action": True,
                            "intent_coords": {
                                "love": 0.0,
                                "justice": 0.0,
                                "power": 0.0,
                                "wisdom": 1.0,
                            },
                            "execution_coords": {
                                "love": 0.0,
                                "justice": 0.0,
                                "power": 1.0,
                                "wisdom": 0.0,
                            },
                        },
                    ],
                }
            ],
        }

    def test_initialization(self):
        """Test Baseline initialization"""
        baseline = Baseline()

        assert "version" in baseline.data
        assert "created_at" in baseline.data
        assert "files" in baseline.data

    def test_from_analysis_results(self):
        """Test creating baseline from analysis results"""
        baseline = Baseline.from_analysis_results(
            self.sample_batch_results, description="Test baseline"
        )

        assert baseline.data["description"] == "Test baseline"
        assert "file1.cbl" in baseline.data["files"]

        file_data = baseline.data["files"]["file1.cbl"]
        assert file_data["program_id"] == "PROG1"
        assert file_data["total_procedures"] == 3
        assert "PROC1" in file_data["procedures"]
        assert "PROC2" in file_data["procedures"]

    def test_save_and_load(self):
        """Test saving and loading baseline"""
        with tempfile.TemporaryDirectory() as tmpdir:
            baseline_path = Path(tmpdir) / "baseline.json"

            # Create and save baseline
            baseline = Baseline.from_analysis_results(self.sample_batch_results)
            baseline.save(str(baseline_path))

            # Load baseline
            loaded = Baseline.from_file(str(baseline_path))

            assert "file1.cbl" in loaded.data["files"]

    def test_compare_identical(self):
        """Test comparison with identical results"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)
        comparison = baseline.compare(self.sample_batch_results)

        summary = comparison["summary"]
        assert summary["regressions"] == 0
        assert summary["improvements"] == 0
        assert summary["unchanged"] >= 0

    def test_compare_regression(self):
        """Test detecting regressions"""
        # Create baseline
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        # Create worse results (regression)
        worse_results = {
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "program_id": "PROG1",
                    "total_procedures": 3,
                    "results": [
                        {
                            "procedure_name": "PROC1",
                            "disharmony_score": 0.9,  # Much worse!
                            "severity_level": "concerning",
                            "is_harmonious": False,
                            "requires_action": True,
                            "intent_coords": {},
                            "execution_coords": {},
                        }
                    ],
                }
            ]
        }

        comparison = baseline.compare(worse_results)

        # Should detect regression
        assert comparison["summary"]["regressions"] > 0
        assert len(comparison["regressions"]) > 0

        regression = comparison["regressions"][0]
        assert regression["procedure_name"] == "PROC1"
        assert regression["delta"] > 0  # Positive delta = worse

    def test_compare_improvement(self):
        """Test detecting improvements"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        # Create better results (improvement)
        better_results = {
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "program_id": "PROG1",
                    "total_procedures": 3,
                    "results": [
                        {
                            "procedure_name": "PROC2",
                            "disharmony_score": 0.2,  # Much better!
                            "severity_level": "harmonious",
                            "is_harmonious": True,
                            "requires_action": False,
                            "intent_coords": {},
                            "execution_coords": {},
                        }
                    ],
                }
            ]
        }

        comparison = baseline.compare(better_results)

        # Should detect improvement
        assert comparison["summary"]["improvements"] > 0
        assert len(comparison["improvements"]) > 0

        improvement = comparison["improvements"][0]
        assert improvement["procedure_name"] == "PROC2"
        assert improvement["delta"] < 0  # Negative delta = better

    def test_compare_new_file(self):
        """Test detecting new files"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        # Add new file
        new_results = {
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "program_id": "PROG1",
                    "total_procedures": 2,
                    "results": [],
                },
                {
                    "file_path": "file2.cbl",  # New file!
                    "program_id": "PROG2",
                    "total_procedures": 1,
                    "results": [
                        {
                            "procedure_name": "NEW-PROC",
                            "disharmony_score": 0.8,
                            "severity_level": "concerning",
                            "is_harmonious": False,
                            "requires_action": True,
                        }
                    ],
                },
            ]
        }

        comparison = baseline.compare(new_results)

        assert comparison["summary"]["new_files"] == 1
        assert len(comparison["new_issues"]) > 0

    def test_compare_removed_file(self):
        """Test detecting removed files"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        # Remove file
        empty_results = {"file_results": []}

        comparison = baseline.compare(empty_results)

        assert comparison["summary"]["removed_files"] == 1

    def test_get_statistics(self):
        """Test getting baseline statistics"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        stats = baseline.get_statistics()

        assert stats["total_procedures"] == 5
        assert stats["harmonious_count"] == 3

    def test_get_file_count(self):
        """Test getting file count"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        assert baseline.get_file_count() == 1

    def test_get_procedure_count(self):
        """Test getting procedure count"""
        baseline = Baseline.from_analysis_results(self.sample_batch_results)

        assert baseline.get_procedure_count() == 3


class TestBaselineManager:
    """Test suite for BaselineManager"""

    def test_initialization(self):
        """Test BaselineManager initialization"""
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = BaselineManager(tmpdir)

            assert manager.baseline_dir.exists()

    def test_save_baseline(self):
        """Test saving baseline"""
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = BaselineManager(tmpdir)

            batch_results = {
                "file_results": [
                    {
                        "file_path": "test.cbl",
                        "program_id": "TEST",
                        "total_procedures": 1,
                        "results": [],
                    }
                ]
            }

            path = manager.save_baseline(batch_results, name="test", description="Test baseline")

            assert Path(path).exists()

            # Check latest was also created
            latest_path = manager.baseline_dir / "test_latest.json"
            assert latest_path.exists()

    def test_load_baseline(self):
        """Test loading baseline"""
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = BaselineManager(tmpdir)

            # Save baseline
            batch_results = {"file_results": []}
            manager.save_baseline(batch_results, name="test")

            # Load baseline
            loaded = manager.load_baseline("test_latest")

            assert loaded is not None
            assert isinstance(loaded, Baseline)

    def test_list_baselines(self):
        """Test listing baselines"""
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = BaselineManager(tmpdir)

            # Create multiple baselines
            batch_results = {"file_results": []}

            manager.save_baseline(batch_results, name="baseline1")
            manager.save_baseline(batch_results, name="baseline2")

            baselines = manager.list_baselines()

            assert len(baselines) >= 2

    def test_compare_to_baseline(self):
        """Test comparing to baseline"""
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = BaselineManager(tmpdir)

            # Save baseline
            baseline_results = {
                "file_results": [
                    {
                        "file_path": "test.cbl",
                        "program_id": "TEST",
                        "total_procedures": 1,
                        "results": [
                            {
                                "procedure_name": "PROC1",
                                "disharmony_score": 0.5,
                                "severity_level": "minor_drift",
                                "is_harmonious": False,
                                "requires_action": False,
                                "intent_coords": {},
                                "execution_coords": {},
                            }
                        ],
                    }
                ]
            }

            manager.save_baseline(baseline_results, name="test")

            # Compare to current results
            current_results = {
                "file_results": [
                    {
                        "file_path": "test.cbl",
                        "program_id": "TEST",
                        "total_procedures": 1,
                        "results": [
                            {
                                "procedure_name": "PROC1",
                                "disharmony_score": 0.8,  # Worse
                                "severity_level": "concerning",
                                "is_harmonious": False,
                                "requires_action": True,
                                "intent_coords": {},
                                "execution_coords": {},
                            }
                        ],
                    }
                ]
            }

            comparison = manager.compare_to_baseline(current_results, "test_latest")

            assert comparison["summary"]["regressions"] > 0

    def test_delete_baseline(self):
        """Test deleting baseline"""
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = BaselineManager(tmpdir)

            # Create baseline
            batch_results = {"file_results": []}
            manager.save_baseline(batch_results, name="test")

            baseline_path = manager.baseline_dir / "test_latest.json"
            assert baseline_path.exists()

            # Delete baseline
            manager.delete_baseline("test_latest")

            assert not baseline_path.exists()


def test_format_comparison_summary():
    """Test formatting comparison summary"""
    comparison = {
        "baseline_created": "2024-01-01T00:00:00Z",
        "comparison_time": "2024-01-02T00:00:00Z",
        "summary": {
            "total_files_baseline": 5,
            "total_files_current": 6,
            "new_files": 1,
            "removed_files": 0,
            "regressions": 2,
            "improvements": 3,
            "unchanged": 10,
            "new_procedures": 1,
            "removed_procedures": 0,
        },
        "regressions": [
            {
                "procedure_name": "BAD-PROC",
                "file_path": "bad.cbl",
                "baseline_score": 0.5,
                "current_score": 1.2,
                "delta": 0.7,
            }
        ],
        "improvements": [
            {
                "procedure_name": "GOOD-PROC",
                "file_path": "good.cbl",
                "baseline_score": 1.0,
                "current_score": 0.3,
                "delta": -0.7,
            }
        ],
    }

    summary_text = format_comparison_summary(comparison)

    assert "Baseline Comparison Summary" in summary_text
    assert "Regressions" in summary_text
    assert "Improvements" in summary_text
    assert "BAD-PROC" in summary_text
    assert "GOOD-PROC" in summary_text
