"""
JSON Reporter

Exports analysis results in JSON format for programmatic consumption.
"""

import json
from typing import List, Dict, Optional
from datetime import datetime
from pathlib import Path


class JSONReporter:
    """Generate JSON reports from analysis results"""

    def __init__(self):
        self.version = "0.1.0"

    def generate_report(
        self,
        file_path: str,
        program_id: str,
        results: List[Dict],
        threshold: Optional[float] = None,
        metadata: Optional[Dict] = None,
    ) -> str:
        """
        Generate JSON report from analysis results.

        Args:
            file_path: Path to analyzed COBOL file
            program_id: COBOL program identifier
            results: List of analysis result dictionaries
            threshold: Disharmony threshold used (optional)
            metadata: Additional metadata (optional)

        Returns:
            JSON string
        """
        # Calculate summary statistics
        summary = self._calculate_summary(results, threshold)

        # Build report structure
        report = {
            "harmonizer_version": self.version,
            "generated_at": datetime.utcnow().isoformat() + "Z",
            "file": {"path": str(file_path), "program_id": program_id},
            "analysis": {
                "threshold": threshold,
                "total_procedures": summary["total"],
                "harmonious_count": summary["harmonious"],
                "disharmonious_count": summary["disharmonious"],
                "requires_action_count": summary["requires_action"],
                "severity_breakdown": summary["severity_breakdown"],
            },
            "procedures": results,
        }

        # Add optional metadata
        if metadata:
            report["metadata"] = metadata

        return json.dumps(report, indent=2)

    def generate_batch_report(
        self, file_results: List[Dict], metadata: Optional[Dict] = None
    ) -> str:
        """
        Generate JSON report for batch analysis of multiple files.

        Args:
            file_results: List of per-file results
                Each item should have: file_path, program_id, results
            metadata: Additional metadata (optional)

        Returns:
            JSON string
        """
        # Aggregate statistics across all files
        total_procedures = 0
        total_harmonious = 0
        total_disharmonious = 0
        total_requires_action = 0
        all_severity_counts = {}

        for file_result in file_results:
            results = file_result.get("results", [])
            summary = self._calculate_summary(results, None)

            total_procedures += summary["total"]
            total_harmonious += summary["harmonious"]
            total_disharmonious += summary["disharmonious"]
            total_requires_action += summary["requires_action"]

            # Aggregate severity counts
            for severity, count in summary["severity_breakdown"].items():
                all_severity_counts[severity] = all_severity_counts.get(severity, 0) + count

        # Build batch report
        report = {
            "harmonizer_version": self.version,
            "generated_at": datetime.utcnow().isoformat() + "Z",
            "batch_analysis": {
                "files_analyzed": len(file_results),
                "total_procedures": total_procedures,
                "harmonious_count": total_harmonious,
                "disharmonious_count": total_disharmonious,
                "requires_action_count": total_requires_action,
                "severity_breakdown": all_severity_counts,
            },
            "files": file_results,
        }

        if metadata:
            report["metadata"] = metadata

        return json.dumps(report, indent=2)

    def save_report(self, report_json: str, output_path: str):
        """
        Save JSON report to file.

        Args:
            report_json: JSON string
            output_path: Output file path
        """
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        with open(output_file, "w", encoding="utf-8") as f:
            f.write(report_json)

    def _calculate_summary(self, results: List[Dict], threshold: Optional[float]) -> Dict:
        """Calculate summary statistics from results"""

        total = len(results)
        harmonious = sum(1 for r in results if r.get("is_harmonious", False))
        disharmonious = total - harmonious
        requires_action = sum(1 for r in results if r.get("requires_action", False))

        # Count by severity
        severity_breakdown = {}
        for result in results:
            severity = result.get("severity_level", "unknown")
            severity_breakdown[severity] = severity_breakdown.get(severity, 0) + 1

        # Calculate above threshold if provided
        above_threshold = 0
        if threshold is not None:
            above_threshold = sum(1 for r in results if r.get("disharmony_score", 0) >= threshold)

        return {
            "total": total,
            "harmonious": harmonious,
            "disharmonious": disharmonious,
            "requires_action": requires_action,
            "above_threshold": above_threshold if threshold else None,
            "severity_breakdown": severity_breakdown,
        }

    def format_for_ci(self, results: List[Dict], threshold: float = 0.8) -> Dict:
        """
        Format results for CI/CD consumption.

        Returns a simplified structure with pass/fail status.

        Args:
            results: Analysis results
            threshold: Threshold for failure

        Returns:
            CI-friendly dictionary
        """
        critical_issues = [r for r in results if r.get("disharmony_score", 0) >= threshold]

        passed = len(critical_issues) == 0

        return {
            "status": "passed" if passed else "failed",
            "total_procedures": len(results),
            "critical_issues": len(critical_issues),
            "threshold": threshold,
            "issues": [
                {
                    "procedure": issue["procedure_name"],
                    "score": issue["disharmony_score"],
                    "severity": issue["severity_level"],
                    "explanation": issue["explanation"],
                }
                for issue in critical_issues
            ],
        }
