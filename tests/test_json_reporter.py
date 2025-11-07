"""
Tests for JSONReporter
"""

import pytest
import json
import tempfile
from pathlib import Path
from cobol_harmonizer.reporter.json_reporter import JSONReporter


class TestJSONReporter:
    """Test suite for JSONReporter"""

    def setup_method(self):
        """Set up test fixtures"""
        self.reporter = JSONReporter()
        self.sample_results = [
            {
                "procedure_name": "GET-CUSTOMER-RECORD",
                "disharmony_score": 0.15,
                "severity_level": "harmonious",
                "is_harmonious": True,
                "requires_action": False
            },
            {
                "procedure_name": "DELETE-CUSTOMER-DATA",
                "disharmony_score": 1.12,
                "severity_level": "critical",
                "is_harmonious": False,
                "requires_action": True
            }
        ]

    def test_generate_report(self):
        """Test basic report generation"""
        report_json = self.reporter.generate_report(
            file_path="test.cbl",
            program_id="TEST-PROG",
            results=self.sample_results
        )

        # Parse JSON
        report = json.loads(report_json)

        # Check structure
        assert "harmonizer_version" in report
        assert "generated_at" in report
        assert "file" in report
        assert "analysis" in report
        assert "procedures" in report

        # Check file info
        assert report["file"]["path"] == "test.cbl"
        assert report["file"]["program_id"] == "TEST-PROG"

        # Check analysis summary
        analysis = report["analysis"]
        assert analysis["total_procedures"] == 2
        assert analysis["harmonious_count"] == 1
        assert analysis["disharmonious_count"] == 1
        assert analysis["requires_action_count"] == 1

    def test_generate_report_with_threshold(self):
        """Test report generation with threshold"""
        report_json = self.reporter.generate_report(
            file_path="test.cbl",
            program_id="TEST-PROG",
            results=self.sample_results,
            threshold=0.5
        )

        report = json.loads(report_json)

        assert report["analysis"]["threshold"] == 0.5

    def test_generate_report_with_metadata(self):
        """Test report with custom metadata"""
        metadata = {"branch": "main", "commit": "abc123"}

        report_json = self.reporter.generate_report(
            file_path="test.cbl",
            program_id="TEST-PROG",
            results=self.sample_results,
            metadata=metadata
        )

        report = json.loads(report_json)

        assert "metadata" in report
        assert report["metadata"]["branch"] == "main"
        assert report["metadata"]["commit"] == "abc123"

    def test_severity_breakdown(self):
        """Test severity breakdown calculation"""
        results = [
            {"severity_level": "harmonious", "is_harmonious": True, "requires_action": False},
            {"severity_level": "harmonious", "is_harmonious": True, "requires_action": False},
            {"severity_level": "critical", "is_harmonious": False, "requires_action": True},
            {"severity_level": "significant", "is_harmonious": False, "requires_action": True}
        ]

        report_json = self.reporter.generate_report(
            file_path="test.cbl",
            program_id="TEST-PROG",
            results=results
        )

        report = json.loads(report_json)
        severity_breakdown = report["analysis"]["severity_breakdown"]

        assert severity_breakdown["harmonious"] == 2
        assert severity_breakdown["critical"] == 1
        assert severity_breakdown["significant"] == 1

    def test_generate_batch_report(self):
        """Test batch report generation"""
        file_results = [
            {
                "file_path": "file1.cbl",
                "program_id": "PROG1",
                "results": [
                    {"severity_level": "harmonious", "is_harmonious": True, "requires_action": False}
                ]
            },
            {
                "file_path": "file2.cbl",
                "program_id": "PROG2",
                "results": [
                    {"severity_level": "critical", "is_harmonious": False, "requires_action": True},
                    {"severity_level": "significant", "is_harmonious": False, "requires_action": True}
                ]
            }
        ]

        report_json = self.reporter.generate_batch_report(file_results)
        report = json.loads(report_json)

        # Check structure
        assert "batch_analysis" in report
        assert "files" in report

        # Check batch analysis
        batch = report["batch_analysis"]
        assert batch["files_analyzed"] == 2
        assert batch["total_procedures"] == 3
        assert batch["harmonious_count"] == 1
        assert batch["disharmonious_count"] == 2

    def test_save_report(self):
        """Test saving report to file"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "report.json"

            report_json = self.reporter.generate_report(
                file_path="test.cbl",
                program_id="TEST-PROG",
                results=self.sample_results
            )

            self.reporter.save_report(report_json, str(output_path))

            # Verify file exists
            assert output_path.exists()

            # Verify content
            with open(output_path, 'r') as f:
                loaded_report = json.load(f)

            assert loaded_report["file"]["program_id"] == "TEST-PROG"

    def test_format_for_ci(self):
        """Test CI/CD format"""
        results = [
            {"disharmony_score": 0.2, "severity_level": "harmonious",
             "procedure_name": "PROC1", "explanation": "Good"},
            {"disharmony_score": 0.9, "severity_level": "critical",
             "procedure_name": "PROC2", "explanation": "Bad"}
        ]

        ci_format = self.reporter.format_for_ci(results, threshold=0.8)

        # Check structure
        assert "status" in ci_format
        assert "total_procedures" in ci_format
        assert "critical_issues" in ci_format
        assert "threshold" in ci_format
        assert "issues" in ci_format

        # Check values
        assert ci_format["status"] == "failed"
        assert ci_format["total_procedures"] == 2
        assert ci_format["critical_issues"] == 1
        assert ci_format["threshold"] == 0.8
        assert len(ci_format["issues"]) == 1
        assert ci_format["issues"][0]["procedure"] == "PROC2"

    def test_format_for_ci_passed(self):
        """Test CI/CD format with passing results"""
        results = [
            {"disharmony_score": 0.2, "severity_level": "harmonious",
             "procedure_name": "PROC1", "explanation": "Good"}
        ]

        ci_format = self.reporter.format_for_ci(results, threshold=0.8)

        assert ci_format["status"] == "passed"
        assert ci_format["critical_issues"] == 0
        assert len(ci_format["issues"]) == 0

    def test_empty_results(self):
        """Test with empty results"""
        report_json = self.reporter.generate_report(
            file_path="test.cbl",
            program_id="TEST-PROG",
            results=[]
        )

        report = json.loads(report_json)

        assert report["analysis"]["total_procedures"] == 0
        assert report["analysis"]["harmonious_count"] == 0
        assert len(report["procedures"]) == 0
