"""
Batch Analyzer

Analyzes multiple COBOL files in directories or from file lists.
Supports parallel processing and aggregated reporting.
"""

import os
from pathlib import Path
from typing import List, Dict, Optional, Callable
from concurrent.futures import ThreadPoolExecutor, as_completed
import time

from cobol_harmonizer.parser.cobol_parser import COBOLParser
from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator


class BatchAnalyzer:
    """Analyze multiple COBOL files in batch"""

    def __init__(self, max_workers: int = 4):
        """
        Initialize batch analyzer.

        Args:
            max_workers: Maximum number of parallel workers
        """
        self.max_workers = max_workers
        self.parser = COBOLParser()
        self.intent_extractor = IntentExtractor()
        self.execution_analyzer = ExecutionAnalyzer()
        self.calculator = DisharmonyCalculator()

    def analyze_directory(
        self,
        directory: str,
        pattern: str = "*.cbl",
        recursive: bool = True,
        threshold: Optional[float] = None,
        progress_callback: Optional[Callable] = None
    ) -> Dict:
        """
        Analyze all COBOL files in a directory.

        Args:
            directory: Directory path to analyze
            pattern: File pattern to match (default: *.cbl)
            recursive: Search subdirectories (default: True)
            threshold: Disharmony threshold for filtering
            progress_callback: Optional callback(current, total, file_path)

        Returns:
            Dictionary with batch analysis results
        """
        # Find all COBOL files
        files = self._find_cobol_files(directory, pattern, recursive)

        if not files:
            return {
                "status": "no_files",
                "message": f"No COBOL files found in {directory}",
                "files_analyzed": 0,
                "results": []
            }

        # Analyze files
        return self.analyze_files(files, threshold, progress_callback)

    def analyze_files(
        self,
        file_paths: List[str],
        threshold: Optional[float] = None,
        progress_callback: Optional[Callable] = None
    ) -> Dict:
        """
        Analyze a list of COBOL files.

        Args:
            file_paths: List of file paths to analyze
            threshold: Disharmony threshold for filtering
            progress_callback: Optional callback(current, total, file_path)

        Returns:
            Dictionary with batch analysis results
        """
        start_time = time.time()
        file_results = []
        errors = []

        total_files = len(file_paths)

        # Use ThreadPoolExecutor for parallel processing
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            # Submit all files for processing
            future_to_file = {
                executor.submit(self._analyze_single_file, file_path, threshold): file_path
                for file_path in file_paths
            }

            # Process completed futures
            for i, future in enumerate(as_completed(future_to_file), 1):
                file_path = future_to_file[future]

                if progress_callback:
                    progress_callback(i, total_files, file_path)

                try:
                    result = future.result()
                    if result["status"] == "success":
                        file_results.append(result)
                    else:
                        errors.append(result)
                except Exception as e:
                    errors.append({
                        "status": "error",
                        "file_path": file_path,
                        "error": str(e)
                    })

        end_time = time.time()

        # Calculate aggregate statistics
        stats = self._calculate_batch_statistics(file_results, threshold)

        return {
            "status": "completed",
            "execution_time": end_time - start_time,
            "files_analyzed": len(file_results),
            "files_with_errors": len(errors),
            "total_files": total_files,
            "statistics": stats,
            "file_results": file_results,
            "errors": errors
        }

    def analyze_from_list_file(
        self,
        list_file: str,
        threshold: Optional[float] = None,
        progress_callback: Optional[Callable] = None
    ) -> Dict:
        """
        Analyze files listed in a text file (one path per line).

        Args:
            list_file: Path to file containing list of COBOL files
            threshold: Disharmony threshold for filtering
            progress_callback: Optional callback(current, total, file_path)

        Returns:
            Dictionary with batch analysis results
        """
        with open(list_file, 'r', encoding='utf-8') as f:
            file_paths = [
                line.strip()
                for line in f
                if line.strip() and not line.strip().startswith('#')
            ]

        return self.analyze_files(file_paths, threshold, progress_callback)

    def _analyze_single_file(
        self,
        file_path: str,
        threshold: Optional[float] = None
    ) -> Dict:
        """
        Analyze a single COBOL file.

        Args:
            file_path: Path to COBOL file
            threshold: Disharmony threshold for filtering

        Returns:
            Dictionary with file analysis results
        """
        try:
            # Parse file
            program = self.parser.parse_file(file_path)

            if not program.procedures:
                return {
                    "status": "no_procedures",
                    "file_path": file_path,
                    "program_id": program.program_id,
                    "message": "No procedures found"
                }

            # Analyze all procedures
            results = []
            for procedure in program.procedures:
                # Extract intent from name
                intent = self.intent_extractor.extract_intent(procedure.name)

                # Analyze execution from body
                execution = self.execution_analyzer.analyze_procedure(procedure)

                # Calculate disharmony
                analysis = self.calculator.calculate_detailed_analysis(
                    procedure.name,
                    intent,
                    execution
                )

                # Add line number if available
                if hasattr(procedure, 'line_number'):
                    analysis['line_number'] = procedure.line_number

                # Add suggestions for disharmonious code
                if not analysis['is_harmonious']:
                    suggestions = self.intent_extractor.suggest_better_names(
                        execution,
                        procedure.name
                    )
                    analysis['suggestions'] = suggestions

                # Filter by threshold if provided
                if threshold is None or analysis['disharmony_score'] >= threshold:
                    results.append(analysis)

            return {
                "status": "success",
                "file_path": file_path,
                "program_id": program.program_id,
                "total_procedures": len(program.procedures),
                "results": results
            }

        except FileNotFoundError:
            return {
                "status": "error",
                "file_path": file_path,
                "error": "File not found"
            }
        except Exception as e:
            return {
                "status": "error",
                "file_path": file_path,
                "error": str(e)
            }

    def _find_cobol_files(
        self,
        directory: str,
        pattern: str,
        recursive: bool
    ) -> List[str]:
        """Find all COBOL files in directory"""
        dir_path = Path(directory)

        if not dir_path.exists():
            return []

        if recursive:
            # Use rglob for recursive search
            files = list(dir_path.rglob(pattern))
        else:
            # Use glob for non-recursive search
            files = list(dir_path.glob(pattern))

        # Also search for common COBOL extensions
        cobol_extensions = ['*.cbl', '*.CBL', '*.cob', '*.COB', '*.cobol', '*.COBOL']
        if pattern not in cobol_extensions:
            for ext in cobol_extensions:
                if recursive:
                    files.extend(dir_path.rglob(ext))
                else:
                    files.extend(dir_path.glob(ext))

        # Remove duplicates and convert to strings
        unique_files = sorted(set(str(f) for f in files))

        return unique_files

    def _calculate_batch_statistics(
        self,
        file_results: List[Dict],
        threshold: Optional[float]
    ) -> Dict:
        """Calculate aggregate statistics across all files"""
        total_procedures = 0
        total_harmonious = 0
        total_disharmonious = 0
        total_requires_action = 0
        total_above_threshold = 0

        severity_counts = {}
        files_with_issues = 0

        for file_result in file_results:
            results = file_result.get("results", [])
            total_procs = file_result.get("total_procedures", 0)

            total_procedures += total_procs

            if results:
                files_with_issues += 1

            for result in results:
                # Count by harmony
                if result.get("is_harmonious", False):
                    total_harmonious += 1
                else:
                    total_disharmonious += 1

                # Count by action required
                if result.get("requires_action", False):
                    total_requires_action += 1

                # Count by severity
                severity = result.get("severity_level", "unknown")
                severity_counts[severity] = severity_counts.get(severity, 0) + 1

                # Count above threshold
                if threshold is not None:
                    if result.get("disharmony_score", 0) >= threshold:
                        total_above_threshold += 1

        return {
            "total_procedures": total_procedures,
            "harmonious_count": total_harmonious,
            "disharmonious_count": total_disharmonious,
            "requires_action_count": total_requires_action,
            "above_threshold_count": total_above_threshold if threshold else None,
            "severity_breakdown": severity_counts,
            "files_with_issues": files_with_issues,
            "files_clean": len(file_results) - files_with_issues
        }

    def get_worst_offenders(
        self,
        batch_results: Dict,
        limit: int = 10
    ) -> List[Dict]:
        """
        Get the procedures with highest disharmony scores.

        Args:
            batch_results: Results from analyze_directory or analyze_files
            limit: Maximum number of results to return

        Returns:
            List of worst offenders sorted by disharmony score
        """
        all_procedures = []

        for file_result in batch_results.get("file_results", []):
            file_path = file_result.get("file_path", "unknown")

            for result in file_result.get("results", []):
                all_procedures.append({
                    **result,
                    "file_path": file_path
                })

        # Sort by disharmony score (descending)
        sorted_procedures = sorted(
            all_procedures,
            key=lambda x: x.get("disharmony_score", 0),
            reverse=True
        )

        return sorted_procedures[:limit]

    def get_files_by_severity(
        self,
        batch_results: Dict,
        min_severity: str = "concerning"
    ) -> List[Dict]:
        """
        Get files that contain procedures above minimum severity.

        Args:
            batch_results: Results from analyze_directory or analyze_files
            min_severity: Minimum severity level
                (harmonious, minor_drift, concerning, significant, critical)

        Returns:
            List of file paths with issue counts
        """
        severity_order = {
            "harmonious": 0,
            "minor_drift": 1,
            "concerning": 2,
            "significant": 3,
            "critical": 4
        }

        min_level = severity_order.get(min_severity.lower(), 0)
        files_with_issues = []

        for file_result in batch_results.get("file_results", []):
            file_path = file_result.get("file_path", "unknown")
            results = file_result.get("results", [])

            # Count procedures at or above minimum severity
            count = sum(
                1 for r in results
                if severity_order.get(r.get("severity_level", "harmonious").lower(), 0) >= min_level
            )

            if count > 0:
                files_with_issues.append({
                    "file_path": file_path,
                    "program_id": file_result.get("program_id", "unknown"),
                    "issue_count": count,
                    "total_procedures": file_result.get("total_procedures", 0)
                })

        # Sort by issue count (descending)
        files_with_issues.sort(key=lambda x: x["issue_count"], reverse=True)

        return files_with_issues
