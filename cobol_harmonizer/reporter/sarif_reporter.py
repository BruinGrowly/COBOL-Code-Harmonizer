"""
SARIF Reporter

Exports analysis results in SARIF format for CI/CD integration.
Supports GitHub Code Scanning, GitLab, Azure DevOps, and other SARIF-compatible tools.

SARIF (Static Analysis Results Interchange Format) 2.1.0
"""

import json
from typing import List, Dict, Optional
from datetime import datetime
from pathlib import Path


class SARIFReporter:
    """Generate SARIF reports from analysis results"""

    def __init__(self):
        self.version = "0.1.0"
        self.sarif_version = "2.1.0"

    def generate_report(
        self,
        file_path: str,
        results: List[Dict],
        metadata: Optional[Dict] = None
    ) -> str:
        """
        Generate SARIF report from analysis results.

        Args:
            file_path: Path to analyzed COBOL file
            results: List of analysis result dictionaries
            metadata: Additional metadata (optional)

        Returns:
            SARIF JSON string
        """
        # Build SARIF structure
        sarif = {
            "version": self.sarif_version,
            "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
            "runs": [
                {
                    "tool": self._build_tool_metadata(),
                    "results": self._convert_results_to_sarif(file_path, results),
                    "artifacts": self._build_artifacts(file_path),
                    "invocations": [
                        {
                            "executionSuccessful": True,
                            "endTimeUtc": datetime.utcnow().isoformat() + "Z"
                        }
                    ]
                }
            ]
        }

        # Add optional properties
        if metadata:
            sarif["runs"][0]["properties"] = metadata

        return json.dumps(sarif, indent=2)

    def generate_batch_report(
        self,
        file_results: List[Dict],
        metadata: Optional[Dict] = None
    ) -> str:
        """
        Generate SARIF report for batch analysis of multiple files.

        Args:
            file_results: List of per-file results
                Each item should have: file_path, results
            metadata: Additional metadata (optional)

        Returns:
            SARIF JSON string
        """
        all_results = []
        all_artifacts = []

        for file_result in file_results:
            file_path = file_result.get("file_path", "unknown")
            results = file_result.get("results", [])

            all_results.extend(self._convert_results_to_sarif(file_path, results))
            all_artifacts.extend(self._build_artifacts(file_path))

        # Remove duplicate artifacts
        unique_artifacts = {a["location"]["uri"]: a for a in all_artifacts}
        all_artifacts = list(unique_artifacts.values())

        sarif = {
            "version": self.sarif_version,
            "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
            "runs": [
                {
                    "tool": self._build_tool_metadata(),
                    "results": all_results,
                    "artifacts": all_artifacts,
                    "invocations": [
                        {
                            "executionSuccessful": True,
                            "endTimeUtc": datetime.utcnow().isoformat() + "Z"
                        }
                    ]
                }
            ]
        }

        if metadata:
            sarif["runs"][0]["properties"] = metadata

        return json.dumps(sarif, indent=2)

    def save_report(self, report_json: str, output_path: str):
        """
        Save SARIF report to file.

        Args:
            report_json: SARIF JSON string
            output_path: Output file path
        """
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(report_json)

    def _build_tool_metadata(self) -> Dict:
        """Build SARIF tool metadata"""
        return {
            "driver": {
                "name": "COBOL Code Harmonizer",
                "version": self.version,
                "informationUri": "https://github.com/BruinGrowly/COBOL-Code-Harmonizer",
                "semanticVersion": self.version,
                "organization": "Code Harmonizer Project",
                "shortDescription": {
                    "text": "Semantic analysis tool for COBOL using LJPW framework"
                },
                "fullDescription": {
                    "text": "Detects semantic disharmony between COBOL procedure names and their implementations using the Love, Justice, Power, Wisdom (LJPW) semantic framework."
                },
                "rules": self._build_rules()
            }
        }

    def _build_rules(self) -> List[Dict]:
        """Build SARIF rule definitions"""
        return [
            {
                "id": "COBOL-HARMONIZER-CRITICAL",
                "name": "CriticalDisharmony",
                "shortDescription": {
                    "text": "Critical semantic disharmony detected"
                },
                "fullDescription": {
                    "text": "The procedure name severely contradicts its implementation. This indicates a critical semantic bug that will confuse developers and likely cause maintenance issues."
                },
                "defaultConfiguration": {
                    "level": "error"
                },
                "helpUri": "https://github.com/BruinGrowly/COBOL-Code-Harmonizer/blob/main/docs/PHILOSOPHY.md",
                "help": {
                    "text": "Critical disharmony (score >= 1.2) indicates the procedure name lies about its behavior. For example, a procedure named GET-CUSTOMER-BALANCE that actually deletes records. This is a severe semantic bug that requires immediate attention.",
                    "markdown": "**Critical Semantic Disharmony**\n\nThe procedure name severely contradicts its implementation. This typically occurs when:\n- A READ/GET procedure modifies or deletes data\n- A DELETE procedure creates or reads data\n- A VALIDATE procedure performs destructive operations\n\nThis is a critical bug that will confuse developers and cause maintenance issues."
                },
                "properties": {
                    "tags": ["semantic", "maintainability", "security"],
                    "precision": "high"
                }
            },
            {
                "id": "COBOL-HARMONIZER-SIGNIFICANT",
                "name": "SignificantDisharmony",
                "shortDescription": {
                    "text": "Significant semantic disharmony detected"
                },
                "fullDescription": {
                    "text": "The procedure name significantly contradicts its implementation. This indicates a serious semantic issue that should be addressed."
                },
                "defaultConfiguration": {
                    "level": "error"
                },
                "helpUri": "https://github.com/BruinGrowly/COBOL-Code-Harmonizer/blob/main/docs/PHILOSOPHY.md",
                "help": {
                    "text": "Significant disharmony (score 0.8-1.2) indicates substantial contradiction between the procedure name and its behavior. This requires refactoring.",
                    "markdown": "**Significant Semantic Disharmony**\n\nThe procedure name and implementation are substantially misaligned. Consider:\n- Renaming the procedure to match its actual behavior\n- Refactoring the procedure to match its name\n- Splitting the procedure into multiple well-named procedures"
                },
                "properties": {
                    "tags": ["semantic", "maintainability"],
                    "precision": "high"
                }
            },
            {
                "id": "COBOL-HARMONIZER-CONCERNING",
                "name": "ConcerningDisharmony",
                "shortDescription": {
                    "text": "Concerning semantic disharmony detected"
                },
                "fullDescription": {
                    "text": "The procedure name noticeably contradicts its implementation. Consider refactoring for clarity."
                },
                "defaultConfiguration": {
                    "level": "warning"
                },
                "helpUri": "https://github.com/BruinGrowly/COBOL-Code-Harmonizer/blob/main/docs/PHILOSOPHY.md",
                "help": {
                    "text": "Concerning disharmony (score 0.5-0.8) indicates noticeable semantic drift. Review and consider refactoring.",
                    "markdown": "**Concerning Semantic Disharmony**\n\nThe procedure shows noticeable semantic drift between name and implementation. While not critical, this should be reviewed for potential refactoring."
                },
                "properties": {
                    "tags": ["semantic", "maintainability"],
                    "precision": "medium"
                }
            },
            {
                "id": "COBOL-HARMONIZER-MINOR",
                "name": "MinorDrift",
                "shortDescription": {
                    "text": "Minor semantic drift detected"
                },
                "fullDescription": {
                    "text": "The procedure shows minor semantic drift. This is often acceptable but worth reviewing."
                },
                "defaultConfiguration": {
                    "level": "note"
                },
                "helpUri": "https://github.com/BruinGrowly/COBOL-Code-Harmonizer/blob/main/docs/PHILOSOPHY.md",
                "help": {
                    "text": "Minor drift (score 0.3-0.5) indicates slight semantic misalignment. This is often acceptable but worth reviewing for clarity.",
                    "markdown": "**Minor Semantic Drift**\n\nThe procedure shows slight semantic drift. This is often acceptable for complex procedures that perform multiple operations, but consider if the name could be more accurate."
                },
                "properties": {
                    "tags": ["semantic", "maintainability"],
                    "precision": "medium"
                }
            }
        ]

    def _convert_results_to_sarif(
        self,
        file_path: str,
        results: List[Dict]
    ) -> List[Dict]:
        """Convert analysis results to SARIF result format"""
        sarif_results = []

        for result in results:
            # Skip harmonious results unless they're edge cases
            if result.get("is_harmonious", True) and not result.get("requires_action", False):
                continue

            # Determine rule ID based on severity
            severity_level = result.get("severity_level", "harmonious")
            rule_id = self._map_severity_to_rule_id(severity_level)
            sarif_level = self._map_severity_to_sarif_level(severity_level)

            # Build SARIF result
            sarif_result = {
                "ruleId": rule_id,
                "level": sarif_level,
                "message": {
                    "text": self._build_result_message(result)
                },
                "locations": [
                    {
                        "physicalLocation": {
                            "artifactLocation": {
                                "uri": self._normalize_path(file_path)
                            },
                            "region": {
                                "startLine": result.get("line_number", 1),
                                "snippet": {
                                    "text": result.get("procedure_name", "unknown")
                                }
                            }
                        }
                    }
                ],
                "properties": {
                    "procedure_name": result.get("procedure_name", "unknown"),
                    "disharmony_score": result.get("disharmony_score", 0.0),
                    "severity_level": severity_level,
                    "intent_coords": result.get("intent_coords", {}),
                    "execution_coords": result.get("execution_coords", {}),
                    "dominant_shift": result.get("dominant_shift", {}),
                    "ljpw_framework": True
                }
            }

            # Add suggestions if available
            if "suggestions" in result and result["suggestions"]:
                sarif_result["fixes"] = self._build_fixes(result)

            sarif_results.append(sarif_result)

        return sarif_results

    def _build_result_message(self, result: Dict) -> str:
        """Build human-readable message for SARIF result"""
        procedure_name = result.get("procedure_name", "unknown")
        score = result.get("disharmony_score", 0.0)
        severity = result.get("severity_level", "harmonious").upper()
        shift = result.get("dominant_shift", {})

        message = f"Procedure '{procedure_name}' shows {severity} disharmony (score: {score:.3f})"

        if shift.get("from") and shift.get("to"):
            message += f" - semantic shift from {shift['from']} to {shift['to']}"

        explanation = result.get("explanation", "")
        if explanation:
            message += f". {explanation}"

        return message

    def _build_fixes(self, result: Dict) -> List[Dict]:
        """Build SARIF fix suggestions"""
        suggestions = result.get("suggestions", [])
        if not suggestions:
            return []

        fixes = []
        for i, suggestion in enumerate(suggestions[:3]):  # Limit to 3 suggestions
            fix = {
                "description": {
                    "text": f"Rename to '{suggestion}' to match actual behavior"
                },
                "artifactChanges": [
                    {
                        "artifactLocation": {
                            "uri": "file:///" + result.get("file_path", "unknown")
                        },
                        "replacements": [
                            {
                                "deletedRegion": {
                                    "startLine": result.get("line_number", 1),
                                    "startColumn": 1
                                },
                                "insertedContent": {
                                    "text": f"       {suggestion}."
                                }
                            }
                        ]
                    }
                ]
            }
            fixes.append(fix)

        return fixes

    def _build_artifacts(self, file_path: str) -> List[Dict]:
        """Build SARIF artifact entries"""
        return [
            {
                "location": {
                    "uri": self._normalize_path(file_path)
                },
                "sourceLanguage": "COBOL"
            }
        ]

    def _normalize_path(self, file_path: str) -> str:
        """Normalize file path for SARIF"""
        # Convert to relative path if possible
        try:
            path = Path(file_path)
            if path.is_absolute():
                # Try to make relative to current directory
                try:
                    return str(path.relative_to(Path.cwd()))
                except ValueError:
                    return str(path)
            return str(path)
        except Exception:
            return str(file_path)

    def _map_severity_to_rule_id(self, severity: str) -> str:
        """Map severity level to SARIF rule ID"""
        severity_map = {
            "critical": "COBOL-HARMONIZER-CRITICAL",
            "significant": "COBOL-HARMONIZER-SIGNIFICANT",
            "concerning": "COBOL-HARMONIZER-CONCERNING",
            "minor_drift": "COBOL-HARMONIZER-MINOR"
        }
        return severity_map.get(severity.lower(), "COBOL-HARMONIZER-MINOR")

    def _map_severity_to_sarif_level(self, severity: str) -> str:
        """Map severity level to SARIF level"""
        severity_map = {
            "critical": "error",
            "significant": "error",
            "concerning": "warning",
            "minor_drift": "note",
            "harmonious": "none"
        }
        return severity_map.get(severity.lower(), "note")
