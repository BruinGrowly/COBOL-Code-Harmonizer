"""
Baseline Comparison

Enables regression testing by comparing current analysis against saved baselines.
Tracks improvements and regressions over time.
"""

import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from datetime import datetime


class Baseline:
    """Manage baseline snapshots for regression testing"""

    def __init__(self, baseline_data: Optional[Dict] = None):
        """
        Initialize baseline.

        Args:
            baseline_data: Optional baseline data dictionary
        """
        self.data = baseline_data or {
            "version": "0.1.0",
            "created_at": datetime.utcnow().isoformat() + "Z",
            "files": {},
        }

    @classmethod
    def from_file(cls, baseline_path: str) -> "Baseline":
        """
        Load baseline from file.

        Args:
            baseline_path: Path to baseline JSON file

        Returns:
            Baseline instance
        """
        baseline_file = Path(baseline_path)

        if not baseline_file.exists():
            raise FileNotFoundError(f"Baseline file not found: {baseline_path}")

        with open(baseline_file, "r", encoding="utf-8") as f:
            baseline_data = json.load(f)

        return cls(baseline_data)

    @classmethod
    def from_analysis_results(
        cls, batch_results: Dict, description: Optional[str] = None
    ) -> "Baseline":
        """
        Create baseline from batch analysis results.

        Args:
            batch_results: Results from BatchAnalyzer
            description: Optional description of this baseline

        Returns:
            Baseline instance
        """
        baseline_data = {
            "version": "0.1.0",
            "created_at": datetime.utcnow().isoformat() + "Z",
            "description": description or "Baseline snapshot",
            "statistics": batch_results.get("statistics", {}),
            "files": {},
        }

        # Store results by file
        for file_result in batch_results.get("file_results", []):
            file_path = file_result.get("file_path", "unknown")

            # Store procedure results indexed by name
            procedures = {}
            for result in file_result.get("results", []):
                proc_name = result.get("procedure_name", "unknown")
                procedures[proc_name] = {
                    "disharmony_score": result.get("disharmony_score", 0.0),
                    "severity_level": result.get("severity_level", "harmonious"),
                    "is_harmonious": result.get("is_harmonious", True),
                    "requires_action": result.get("requires_action", False),
                    "intent_coords": result.get("intent_coords", {}),
                    "execution_coords": result.get("execution_coords", {}),
                }

            baseline_data["files"][file_path] = {
                "program_id": file_result.get("program_id", "unknown"),
                "total_procedures": file_result.get("total_procedures", 0),
                "procedures": procedures,
            }

        return cls(baseline_data)

    def save(self, output_path: str):
        """
        Save baseline to file.

        Args:
            output_path: Output file path
        """
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        with open(output_file, "w", encoding="utf-8") as f:
            json.dump(self.data, f, indent=2)

    def compare(self, current_results: Dict) -> Dict:
        """
        Compare current analysis results against this baseline.

        Args:
            current_results: Current batch analysis results

        Returns:
            Comparison results dictionary
        """
        comparison = {
            "baseline_created": self.data.get("created_at"),
            "comparison_time": datetime.utcnow().isoformat() + "Z",
            "summary": {
                "total_files_baseline": len(self.data.get("files", {})),
                "total_files_current": len(current_results.get("file_results", [])),
                "new_files": 0,
                "removed_files": 0,
                "regressions": 0,
                "improvements": 0,
                "unchanged": 0,
                "new_procedures": 0,
                "removed_procedures": 0,
            },
            "file_comparisons": [],
            "regressions": [],
            "improvements": [],
            "new_issues": [],
        }

        baseline_files = set(self.data.get("files", {}).keys())
        current_files = {fr.get("file_path"): fr for fr in current_results.get("file_results", [])}
        current_file_paths = set(current_files.keys())

        # Find new and removed files
        new_files = current_file_paths - baseline_files
        removed_files = baseline_files - current_file_paths
        common_files = baseline_files & current_file_paths

        comparison["summary"]["new_files"] = len(new_files)
        comparison["summary"]["removed_files"] = len(removed_files)

        # Compare common files
        for file_path in common_files:
            file_comparison = self._compare_file(
                file_path, self.data["files"][file_path], current_files[file_path]
            )

            comparison["file_comparisons"].append(file_comparison)

            # Aggregate counts
            comparison["summary"]["regressions"] += file_comparison["regressions"]
            comparison["summary"]["improvements"] += file_comparison["improvements"]
            comparison["summary"]["unchanged"] += file_comparison["unchanged"]
            comparison["summary"]["new_procedures"] += file_comparison["new_procedures"]
            comparison["summary"]["removed_procedures"] += file_comparison["removed_procedures"]

            # Collect regression details
            comparison["regressions"].extend(file_comparison["regression_details"])
            comparison["improvements"].extend(file_comparison["improvement_details"])

        # Handle new files
        for file_path in new_files:
            file_result = current_files[file_path]
            for result in file_result.get("results", []):
                if not result.get("is_harmonious", True):
                    comparison["new_issues"].append(
                        {
                            "file_path": file_path,
                            "procedure_name": result.get("procedure_name"),
                            "disharmony_score": result.get("disharmony_score"),
                            "severity_level": result.get("severity_level"),
                        }
                    )

        return comparison

    def _compare_file(self, file_path: str, baseline_file: Dict, current_file: Dict) -> Dict:
        """Compare a single file against baseline"""
        comparison = {
            "file_path": file_path,
            "regressions": 0,
            "improvements": 0,
            "unchanged": 0,
            "new_procedures": 0,
            "removed_procedures": 0,
            "regression_details": [],
            "improvement_details": [],
        }

        baseline_procs = baseline_file.get("procedures", {})
        current_results = {r.get("procedure_name"): r for r in current_file.get("results", [])}

        baseline_proc_names = set(baseline_procs.keys())
        current_proc_names = set(current_results.keys())

        # Find new and removed procedures
        new_procs = current_proc_names - baseline_proc_names
        removed_procs = baseline_proc_names - current_proc_names
        common_procs = baseline_proc_names & current_proc_names

        comparison["new_procedures"] = len(new_procs)
        comparison["removed_procedures"] = len(removed_procs)

        # Compare common procedures
        for proc_name in common_procs:
            baseline_proc = baseline_procs[proc_name]
            current_proc = current_results[proc_name]

            baseline_score = baseline_proc.get("disharmony_score", 0.0)
            current_score = current_proc.get("disharmony_score", 0.0)

            # Calculate delta (positive = regression, negative = improvement)
            delta = current_score - baseline_score

            # Threshold for significant change (0.1 points)
            if abs(delta) < 0.1:
                comparison["unchanged"] += 1
            elif delta > 0:
                # Regression: score got worse
                comparison["regressions"] += 1
                comparison["regression_details"].append(
                    {
                        "file_path": file_path,
                        "procedure_name": proc_name,
                        "baseline_score": baseline_score,
                        "current_score": current_score,
                        "delta": delta,
                        "baseline_severity": baseline_proc.get("severity_level"),
                        "current_severity": current_proc.get("severity_level"),
                    }
                )
            else:
                # Improvement: score got better
                comparison["improvements"] += 1
                comparison["improvement_details"].append(
                    {
                        "file_path": file_path,
                        "procedure_name": proc_name,
                        "baseline_score": baseline_score,
                        "current_score": current_score,
                        "delta": delta,
                        "baseline_severity": baseline_proc.get("severity_level"),
                        "current_severity": current_proc.get("severity_level"),
                    }
                )

        return comparison

    def get_statistics(self) -> Dict:
        """Get baseline statistics"""
        return self.data.get("statistics", {})

    def get_file_count(self) -> int:
        """Get number of files in baseline"""
        return len(self.data.get("files", {}))

    def get_procedure_count(self) -> int:
        """Get total number of procedures in baseline"""
        total = 0
        for file_data in self.data.get("files", {}).values():
            total += file_data.get("total_procedures", 0)
        return total


class BaselineManager:
    """Manage multiple baselines and track history"""

    def __init__(self, baseline_dir: str = ".harmonizer/baselines"):
        """
        Initialize baseline manager.

        Args:
            baseline_dir: Directory to store baselines
        """
        self.baseline_dir = Path(baseline_dir)
        self.baseline_dir.mkdir(parents=True, exist_ok=True)

    def save_baseline(
        self, batch_results: Dict, name: str = "baseline", description: Optional[str] = None
    ) -> str:
        """
        Save a new baseline.

        Args:
            batch_results: Batch analysis results
            name: Baseline name
            description: Optional description

        Returns:
            Path to saved baseline file
        """
        # Create baseline
        baseline = Baseline.from_analysis_results(batch_results, description)

        # Generate filename with timestamp
        timestamp = datetime.utcnow().strftime("%Y%m%d_%H%M%S")
        filename = f"{name}_{timestamp}.json"
        output_path = self.baseline_dir / filename

        # Save
        baseline.save(str(output_path))

        # Also save as "latest"
        latest_path = self.baseline_dir / f"{name}_latest.json"
        baseline.save(str(latest_path))

        return str(output_path)

    def load_baseline(self, name: str = "baseline_latest") -> Baseline:
        """
        Load a baseline by name.

        Args:
            name: Baseline name (without .json extension)

        Returns:
            Baseline instance
        """
        baseline_path = self.baseline_dir / f"{name}.json"
        return Baseline.from_file(str(baseline_path))

    def list_baselines(self) -> List[Dict]:
        """
        List all available baselines.

        Returns:
            List of baseline info dictionaries
        """
        baselines = []

        for baseline_file in sorted(self.baseline_dir.glob("*.json")):
            try:
                with open(baseline_file, "r", encoding="utf-8") as f:
                    data = json.load(f)

                baselines.append(
                    {
                        "name": baseline_file.stem,
                        "path": str(baseline_file),
                        "created_at": data.get("created_at"),
                        "description": data.get("description"),
                        "file_count": len(data.get("files", {})),
                    }
                )
            except Exception:
                continue

        return baselines

    def compare_to_baseline(
        self, current_results: Dict, baseline_name: str = "baseline_latest"
    ) -> Dict:
        """
        Compare current results to a named baseline.

        Args:
            current_results: Current batch analysis results
            baseline_name: Name of baseline to compare against

        Returns:
            Comparison results
        """
        baseline = self.load_baseline(baseline_name)
        return baseline.compare(current_results)

    def delete_baseline(self, name: str):
        """
        Delete a baseline.

        Args:
            name: Baseline name (without .json extension)
        """
        baseline_path = self.baseline_dir / f"{name}.json"
        if baseline_path.exists():
            baseline_path.unlink()


def format_comparison_summary(comparison: Dict) -> str:
    """
    Format comparison results as human-readable text.

    Args:
        comparison: Comparison results from Baseline.compare()

    Returns:
        Formatted summary string
    """
    summary = comparison.get("summary", {})

    lines = [
        "Baseline Comparison Summary",
        "=" * 50,
        f"Baseline created: {comparison.get('baseline_created', 'unknown')}",
        f"Comparison time: {comparison.get('comparison_time', 'unknown')}",
        "",
        "Files:",
        f"  Baseline: {summary.get('total_files_baseline', 0)}",
        f"  Current:  {summary.get('total_files_current', 0)}",
        f"  New:      {summary.get('new_files', 0)}",
        f"  Removed:  {summary.get('removed_files', 0)}",
        "",
        "Procedures:",
        f"  Regressions:  {summary.get('regressions', 0)} 📈",
        f"  Improvements: {summary.get('improvements', 0)} 📉",
        f"  Unchanged:    {summary.get('unchanged', 0)}",
        f"  New:          {summary.get('new_procedures', 0)}",
        f"  Removed:      {summary.get('removed_procedures', 0)}",
        "",
    ]

    # Show top regressions
    regressions = comparison.get("regressions", [])
    if regressions:
        lines.append("Top Regressions:")
        for reg in sorted(regressions, key=lambda x: x.get("delta", 0), reverse=True)[:5]:
            lines.append(
                f"  • {reg.get('procedure_name')} in {reg.get('file_path')}: "
                f"{reg.get('baseline_score', 0):.3f} → {reg.get('current_score', 0):.3f} "
                f"(+{reg.get('delta', 0):.3f})"
            )
        lines.append("")

    # Show top improvements
    improvements = comparison.get("improvements", [])
    if improvements:
        lines.append("Top Improvements:")
        for imp in sorted(improvements, key=lambda x: x.get("delta", 0))[:5]:
            lines.append(
                f"  • {imp.get('procedure_name')} in {imp.get('file_path')}: "
                f"{imp.get('baseline_score', 0):.3f} → {imp.get('current_score', 0):.3f} "
                f"({imp.get('delta', 0):.3f})"
            )
        lines.append("")

    return "\n".join(lines)
