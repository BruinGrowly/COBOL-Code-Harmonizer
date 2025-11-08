"""
Legacy Codebase Mapper

Analyzes and visualizes large legacy COBOL codebases.
Identifies hotspots, patterns, and refactoring priorities.
"""

from typing import Dict, List, Optional, Tuple
from collections import defaultdict
import statistics


class CodebaseMapper:
    """Map and analyze legacy COBOL codebases"""

    def __init__(self):
        """Initialize codebase mapper"""
        self.severity_weights = {
            "harmonious": 0,
            "minor_drift": 1,
            "concerning": 2,
            "significant": 3,
            "critical": 4,
        }

    def analyze_codebase(self, batch_results: Dict) -> Dict:
        """
        Analyze entire codebase and generate comprehensive map.

        Args:
            batch_results: Results from BatchAnalyzer

        Returns:
            Codebase analysis map
        """
        file_results = batch_results.get("file_results", [])

        if not file_results:
            return {"status": "empty", "message": "No files to analyze"}

        return {
            "overview": self._generate_overview(batch_results),
            "health_metrics": self._calculate_health_metrics(batch_results),
            "hotspots": self._identify_hotspots(batch_results),
            "semantic_patterns": self._analyze_semantic_patterns(batch_results),
            "recommendations": self._generate_recommendations(batch_results),
            "file_rankings": self._rank_files(batch_results),
            "procedure_rankings": self._rank_procedures(batch_results),
            "dimension_analysis": self._analyze_dimensions(batch_results),
        }

    def _generate_overview(self, batch_results: Dict) -> Dict:
        """Generate high-level overview"""
        stats = batch_results.get("statistics", {})
        file_results = batch_results.get("file_results", [])

        return {
            "total_files": len(file_results),
            "total_procedures": stats.get("total_procedures", 0),
            "harmonious_count": stats.get("harmonious_count", 0),
            "disharmonious_count": stats.get("disharmonious_count", 0),
            "requires_action_count": stats.get("requires_action_count", 0),
            "files_with_issues": stats.get("files_with_issues", 0),
            "files_clean": stats.get("files_clean", 0),
            "severity_breakdown": stats.get("severity_breakdown", {}),
        }

    def _calculate_health_metrics(self, batch_results: Dict) -> Dict:
        """Calculate codebase health metrics"""
        stats = batch_results.get("statistics", {})
        total_procs = stats.get("total_procedures", 0)

        if total_procs == 0:
            return {
                "health_score": 0.0,
                "harmony_rate": 0.0,
                "action_required_rate": 0.0,
                "grade": "N/A",
            }

        # Calculate harmony rate (percentage of harmonious procedures)
        harmonious = stats.get("harmonious_count", 0)
        harmony_rate = harmonious / total_procs

        # Calculate action required rate
        requires_action = stats.get("requires_action_count", 0)
        action_rate = requires_action / total_procs

        # Calculate weighted health score (0-100)
        # Based on severity distribution
        severity_breakdown = stats.get("severity_breakdown", {})
        weighted_issues = 0

        for severity, count in severity_breakdown.items():
            weight = self.severity_weights.get(severity.lower(), 0)
            weighted_issues += count * weight

        # Health score: lower weighted_issues = higher health
        max_possible_issues = total_procs * 4  # Max weight is 4 (critical)
        health_score = max(0, 100 - (weighted_issues / max_possible_issues * 100))

        # Assign grade
        if health_score >= 90:
            grade = "A"
        elif health_score >= 80:
            grade = "B"
        elif health_score >= 70:
            grade = "C"
        elif health_score >= 60:
            grade = "D"
        else:
            grade = "F"

        return {
            "health_score": round(health_score, 2),
            "harmony_rate": round(harmony_rate * 100, 2),
            "action_required_rate": round(action_rate * 100, 2),
            "grade": grade,
            "interpretation": self._interpret_health_score(health_score),
        }

    def _interpret_health_score(self, score: float) -> str:
        """Interpret health score"""
        if score >= 90:
            return "Excellent - Very low technical debt"
        elif score >= 80:
            return "Good - Manageable technical debt"
        elif score >= 70:
            return "Fair - Moderate technical debt, consider refactoring"
        elif score >= 60:
            return "Poor - High technical debt, refactoring recommended"
        else:
            return "Critical - Severe technical debt, immediate action needed"

    def _identify_hotspots(self, batch_results: Dict) -> Dict:
        """Identify problematic hotspots"""
        file_results = batch_results.get("file_results", [])

        # Calculate file-level scores
        file_scores = []
        for file_result in file_results:
            file_path = file_result.get("file_path", "unknown")
            results = file_result.get("results", [])

            if results:
                avg_score = statistics.mean(r.get("disharmony_score", 0) for r in results)
                max_score = max(r.get("disharmony_score", 0) for r in results)
                critical_count = sum(
                    1 for r in results if r.get("severity_level", "") == "critical"
                )

                file_scores.append(
                    {
                        "file_path": file_path,
                        "avg_disharmony": avg_score,
                        "max_disharmony": max_score,
                        "critical_count": critical_count,
                        "total_procedures": file_result.get("total_procedures", 0),
                        "issue_count": len(results),
                    }
                )

        # Sort by average disharmony (descending)
        file_scores.sort(key=lambda x: x["avg_disharmony"], reverse=True)

        return {
            "worst_files": file_scores[:10],  # Top 10 worst files
            "critical_files": [f for f in file_scores if f["critical_count"] > 0],
        }

    def _analyze_semantic_patterns(self, batch_results: Dict) -> Dict:
        """Analyze semantic shift patterns"""
        file_results = batch_results.get("file_results", [])

        # Track semantic shifts
        shift_patterns = defaultdict(int)
        dimension_drifts = {"Love": 0, "Justice": 0, "Power": 0, "Wisdom": 0}

        for file_result in file_results:
            for result in file_result.get("results", []):
                shift = result.get("dominant_shift", {})
                from_dim = shift.get("from", "")
                to_dim = shift.get("to", "")

                if from_dim and to_dim and from_dim != to_dim:
                    pattern = f"{from_dim} → {to_dim}"
                    shift_patterns[pattern] += 1

                    # Track which dimension is being drifted away from
                    if from_dim in dimension_drifts:
                        dimension_drifts[from_dim] += 1

        # Sort patterns by frequency
        sorted_patterns = sorted(shift_patterns.items(), key=lambda x: x[1], reverse=True)

        return {
            "common_shifts": sorted_patterns[:10],
            "dimension_drifts": dimension_drifts,
            "most_unstable_dimension": (
                max(dimension_drifts.items(), key=lambda x: x[1])[0] if dimension_drifts else None
            ),
        }

    def _generate_recommendations(self, batch_results: Dict) -> List[Dict]:
        """Generate refactoring recommendations"""
        recommendations = []

        stats = batch_results.get("statistics", {})
        severity_breakdown = stats.get("severity_breakdown", {})

        # Critical issues
        critical_count = severity_breakdown.get("critical", 0)
        if critical_count > 0:
            recommendations.append(
                {
                    "priority": "CRITICAL",
                    "category": "Semantic Bugs",
                    "title": f"Fix {critical_count} Critical Semantic Bug(s)",
                    "description": (
                        f"Found {critical_count} procedure(s) with critical disharmony. "
                        "These procedures have names that severely contradict their "
                        "implementations and likely represent bugs."
                    ),
                    "action": "Review and either rename or refactor these procedures immediately",
                }
            )

        # Significant issues
        significant_count = severity_breakdown.get("significant", 0)
        if significant_count > 0:
            recommendations.append(
                {
                    "priority": "HIGH",
                    "category": "Code Quality",
                    "title": f"Address {significant_count} Significant Disharmony Issue(s)",
                    "description": (
                        f"Found {significant_count} procedure(s) with significant semantic "
                        "drift. These should be refactored for better maintainability."
                    ),
                    "action": "Plan refactoring sprint to address these procedures",
                }
            )

        # Concerning issues
        concerning_count = severity_breakdown.get("concerning", 0)
        if concerning_count > 0:
            recommendations.append(
                {
                    "priority": "MEDIUM",
                    "category": "Maintenance",
                    "title": f"Review {concerning_count} Concerning Procedure(s)",
                    "description": (
                        f"Found {concerning_count} procedure(s) with concerning semantic "
                        "drift. Review for potential improvements."
                    ),
                    "action": "Add to technical debt backlog",
                }
            )

        # Health score recommendations
        health_metrics = self._calculate_health_metrics(batch_results)
        health_score = health_metrics.get("health_score", 0)

        if health_score < 70:
            recommendations.append(
                {
                    "priority": "HIGH",
                    "category": "Codebase Health",
                    "title": f"Improve Codebase Health (Current: {health_score}/100)",
                    "description": (
                        f"Overall codebase health score is {health_metrics.get('grade')}. "
                        "Focus on reducing technical debt systematically."
                    ),
                    "action": "Establish baseline and track improvements over time",
                }
            )

        return recommendations

    def _rank_files(self, batch_results: Dict) -> List[Dict]:
        """Rank files by overall quality"""
        file_results = batch_results.get("file_results", [])

        file_rankings = []
        for file_result in file_results:
            results = file_result.get("results", [])
            total_procs = file_result.get("total_procedures", 0)

            if total_procs == 0:
                continue

            # Calculate file metrics
            avg_score = (
                statistics.mean(r.get("disharmony_score", 0) for r in results) if results else 0
            )

            harmonious_count = sum(1 for r in results if r.get("is_harmonious", True))

            file_rankings.append(
                {
                    "file_path": file_result.get("file_path"),
                    "program_id": file_result.get("program_id"),
                    "total_procedures": total_procs,
                    "avg_disharmony": round(avg_score, 3),
                    "harmonious_count": harmonious_count,
                    "harmony_rate": round(harmonious_count / total_procs * 100, 2),
                }
            )

        # Sort by harmony rate (ascending) - worst first
        file_rankings.sort(key=lambda x: (x["harmony_rate"], -x["avg_disharmony"]))

        return file_rankings

    def _rank_procedures(self, batch_results: Dict) -> List[Dict]:
        """Rank individual procedures by disharmony"""
        file_results = batch_results.get("file_results", [])

        all_procedures = []
        for file_result in file_results:
            file_path = file_result.get("file_path")

            for result in file_result.get("results", []):
                all_procedures.append(
                    {
                        "file_path": file_path,
                        "procedure_name": result.get("procedure_name"),
                        "disharmony_score": result.get("disharmony_score", 0),
                        "severity_level": result.get("severity_level"),
                        "dominant_shift": result.get("dominant_shift"),
                    }
                )

        # Sort by disharmony score (descending)
        all_procedures.sort(key=lambda x: x["disharmony_score"], reverse=True)

        return all_procedures[:50]  # Top 50 worst procedures

    def _analyze_dimensions(self, batch_results: Dict) -> Dict:
        """Analyze LJPW dimension usage across codebase"""
        file_results = batch_results.get("file_results", [])

        # Aggregate intent and execution distributions
        intent_totals = {"Love": 0, "Justice": 0, "Power": 0, "Wisdom": 0}
        execution_totals = {"Love": 0, "Justice": 0, "Power": 0, "Wisdom": 0}
        total_procedures = 0

        for file_result in file_results:
            for result in file_result.get("results", []):
                total_procedures += 1

                # Sum intent coordinates
                intent_coords = result.get("intent_coords", {})
                for dim in intent_totals:
                    intent_totals[dim] += intent_coords.get(dim.lower(), 0)

                # Sum execution coordinates
                exec_coords = result.get("execution_coords", {})
                for dim in execution_totals:
                    execution_totals[dim] += exec_coords.get(dim.lower(), 0)

        # Calculate averages
        if total_procedures > 0:
            intent_avg = {
                dim: round(total / total_procedures, 3) for dim, total in intent_totals.items()
            }
            execution_avg = {
                dim: round(total / total_procedures, 3) for dim, total in execution_totals.items()
            }
        else:
            intent_avg = {dim: 0 for dim in intent_totals}
            execution_avg = {dim: 0 for dim in execution_totals}

        # Identify dominant dimensions
        dominant_intent = max(intent_avg.items(), key=lambda x: x[1])[0]
        dominant_execution = max(execution_avg.items(), key=lambda x: x[1])[0]

        return {
            "intent_distribution": intent_avg,
            "execution_distribution": execution_avg,
            "dominant_intent_dimension": dominant_intent,
            "dominant_execution_dimension": dominant_execution,
            "alignment": dominant_intent == dominant_execution,
        }

    def generate_summary_report(self, analysis: Dict) -> str:
        """
        Generate human-readable summary report.

        Args:
            analysis: Analysis from analyze_codebase()

        Returns:
            Formatted report string
        """
        lines = ["=" * 70, "COBOL CODEBASE ANALYSIS REPORT", "=" * 70, ""]

        # Overview
        overview = analysis.get("overview", {})
        lines.extend(
            [
                "OVERVIEW",
                "-" * 70,
                f"Total Files:       {overview.get('total_files', 0)}",
                f"Total Procedures:  {overview.get('total_procedures', 0)}",
                f"Harmonious:        {overview.get('harmonious_count', 0)}",
                f"Disharmonious:     {overview.get('disharmonious_count', 0)}",
                f"Requires Action:   {overview.get('requires_action_count', 0)}",
                "",
            ]
        )

        # Health Metrics
        health = analysis.get("health_metrics", {})
        lines.extend(
            [
                "CODEBASE HEALTH",
                "-" * 70,
                f"Health Score:      {health.get('health_score', 0)}/100 (Grade: {health.get('grade', 'N/A')})",
                f"Harmony Rate:      {health.get('harmony_rate', 0)}%",
                f"Action Required:   {health.get('action_required_rate', 0)}%",
                f"Assessment:        {health.get('interpretation', 'Unknown')}",
                "",
            ]
        )

        # Hotspots
        hotspots = analysis.get("hotspots", {})
        worst_files = hotspots.get("worst_files", [])[:5]

        if worst_files:
            lines.extend(["TOP 5 PROBLEM FILES", "-" * 70])
            for i, file_info in enumerate(worst_files, 1):
                lines.append(
                    f"{i}. {file_info.get('file_path')} "
                    f"(avg: {file_info.get('avg_disharmony', 0):.3f}, "
                    f"critical: {file_info.get('critical_count', 0)})"
                )
            lines.append("")

        # Recommendations
        recommendations = analysis.get("recommendations", [])
        if recommendations:
            lines.extend(["RECOMMENDATIONS", "-" * 70])
            for rec in recommendations:
                lines.extend(
                    [
                        f"[{rec.get('priority')}] {rec.get('title')}",
                        f"  {rec.get('description')}",
                        f"  → {rec.get('action')}",
                        "",
                    ]
                )

        lines.append("=" * 70)

        return "\n".join(lines)
