#!/usr/bin/env python3
"""
Enterprise Analysis Framework

Analyzes large COBOL codebases and generates comprehensive metrics for IBM presentation.

Generates:
- Bug detection statistics
- Cost estimates
- ROI calculations
- Comparison benchmarks
- Executive summary
"""

import sys
import json
import time
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple
from dataclasses import dataclass, asdict

# Add project to path
sys.path.insert(0, str(Path(__file__).parent))

from cobol_harmonizer.parser.cobol_parser import COBOLParser
from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator
from cobol_harmonizer.compliance import ComplianceTagger, ComplianceRiskAssessor, ComplianceTag


@dataclass
class BugFinding:
    """A semantic bug finding"""
    file_path: str
    procedure_name: str
    disharmony_score: float
    severity: str
    intent_description: str
    execution_description: str
    business_impact: str
    estimated_cost_range: Tuple[int, int]
    compliance_tags: List[str]
    risk_score: float


@dataclass
class ProjectAnalysis:
    """Analysis results for a project"""
    project_name: str
    total_files: int
    total_procedures: int
    total_loc: int
    analysis_time_seconds: float

    # Bug counts by severity
    critical_bugs: int
    high_bugs: int
    medium_bugs: int
    low_bugs: int

    # Compliance findings
    sox_procedures: int
    pci_procedures: int
    gdpr_procedures: int

    # Top findings
    top_bugs: List[BugFinding]

    # Financial metrics
    estimated_bug_cost_total: Tuple[int, int]  # (min, max)
    estimated_fix_hours: int
    roi_multiplier: float


class EnterpriseAnalyzer:
    """Analyzes COBOL codebases with enterprise metrics"""

    # Industry standard cost estimates
    COST_PER_BUG = {
        'critical': (50000, 500000),  # $50K - $500K per critical bug
        'high': (10000, 100000),      # $10K - $100K per high bug
        'medium': (2000, 20000),      # $2K - $20K per medium bug
        'low': (500, 5000),           # $500 - $5K per low bug
    }

    HOURS_TO_FIX = {
        'critical': 40,  # 1 week
        'high': 16,      # 2 days
        'medium': 8,     # 1 day
        'low': 4,        # Half day
    }

    def __init__(self):
        self.parser = COBOLParser()
        self.intent_extractor = IntentExtractor()
        self.execution_analyzer = ExecutionAnalyzer()
        self.disharmony_calc = DisharmonyCalculator()
        self.tagger = ComplianceTagger()
        self.risk_assessor = ComplianceRiskAssessor(self.tagger)

    def analyze_project(
        self,
        project_path: Path,
        project_name: str,
        max_files: int = None
    ) -> ProjectAnalysis:
        """
        Analyze an entire project.

        Args:
            project_path: Path to project directory
            project_name: Name of the project
            max_files: Maximum files to analyze (for testing)

        Returns:
            ProjectAnalysis with comprehensive metrics
        """
        print(f"\n{'='*80}")
        print(f"Analyzing Project: {project_name}")
        print(f"{'='*80}\n")

        start_time = time.time()

        # Find all COBOL files
        cobol_files = list(project_path.rglob("*.cbl"))
        cobol_files += list(project_path.rglob("*.CBL"))
        cobol_files += list(project_path.rglob("*.cob"))
        cobol_files += list(project_path.rglob("*.COB"))

        if max_files:
            cobol_files = cobol_files[:max_files]

        total_files = len(cobol_files)
        total_procedures = 0
        total_loc = 0

        # Findings by severity
        findings_by_severity = {
            'critical': [],
            'high': [],
            'medium': [],
            'low': [],
        }

        # Compliance counts
        sox_count = 0
        pci_count = 0
        gdpr_count = 0

        # Analyze each file
        for i, file_path in enumerate(cobol_files, 1):
            print(f"[{i}/{total_files}] Analyzing {file_path.name}...")

            try:
                # Read file
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    source_code = f.read()

                total_loc += len(source_code.split('\n'))

                # Parse
                program = self.parser.parse_source(source_code)

                # Analyze each procedure
                for procedure in program.procedures:
                    total_procedures += 1

                    # Skip if no statements
                    if not procedure.statements:
                        continue

                    # Extract intent and execution
                    intent_coords = self.intent_extractor.extract_intent(procedure.name)
                    execution_coords = self.execution_analyzer.analyze_procedure(procedure)

                    # Calculate disharmony
                    disharmony_score = self.disharmony_calc.calculate(
                        intent_coords,
                        execution_coords
                    )
                    severity = self.disharmony_calc.classify(disharmony_score)

                    # Only process if disharmonious
                    if severity.value in ['concerning', 'significant', 'critical']:
                        # Compliance tagging
                        verbs = [stmt.verb for stmt in procedure.statements]
                        tags = self.tagger.tag_procedure(
                            procedure.name,
                            verbs=verbs
                        )

                        # Count compliance
                        if any('sox' in tag.value for tag in tags):
                            sox_count += 1
                        if any('pci' in tag.value for tag in tags):
                            pci_count += 1
                        if any('gdpr' in tag.value for tag in tags):
                            gdpr_count += 1

                        # Risk assessment
                        risk = self.risk_assessor.assess_procedure(
                            procedure_name=procedure.name,
                            file_path=str(file_path),
                            disharmony_score=disharmony_score,
                            disharmony_level=severity.value,
                            verbs=verbs,
                            fan_in=0  # Would need call graph
                        )

                        # Classify severity for reporting
                        if disharmony_score >= 1.2:
                            severity_label = 'critical'
                        elif disharmony_score >= 0.8:
                            severity_label = 'high'
                        elif disharmony_score >= 0.5:
                            severity_label = 'medium'
                        else:
                            severity_label = 'low'

                        # Create finding
                        finding = BugFinding(
                            file_path=str(file_path.relative_to(project_path)),
                            procedure_name=procedure.name,
                            disharmony_score=disharmony_score,
                            severity=severity_label,
                            intent_description=self._describe_intent(intent_coords),
                            execution_description=self._describe_execution(execution_coords),
                            business_impact=self._assess_business_impact(
                                procedure.name,
                                tags,
                                disharmony_score
                            ),
                            estimated_cost_range=self.COST_PER_BUG[severity_label],
                            compliance_tags=[tag.value for tag in tags],
                            risk_score=risk.risk_score
                        )

                        findings_by_severity[severity_label].append(finding)

            except Exception as e:
                print(f"  ⚠️  Error analyzing {file_path.name}: {e}")
                continue

        # Calculate totals
        analysis_time = time.time() - start_time

        critical_bugs = len(findings_by_severity['critical'])
        high_bugs = len(findings_by_severity['high'])
        medium_bugs = len(findings_by_severity['medium'])
        low_bugs = len(findings_by_severity['low'])

        # Estimate costs
        cost_min = sum(
            len(findings) * self.COST_PER_BUG[sev][0]
            for sev, findings in findings_by_severity.items()
        )
        cost_max = sum(
            len(findings) * self.COST_PER_BUG[sev][1]
            for sev, findings in findings_by_severity.items()
        )

        # Estimate fix hours
        fix_hours = sum(
            len(findings) * self.HOURS_TO_FIX[sev]
            for sev, findings in findings_by_severity.items()
        )

        # Calculate ROI (cost saved / setup cost)
        setup_cost = 4 * 150  # 4 hours at $150/hr
        roi_multiplier = cost_min / setup_cost if setup_cost > 0 else 0

        # Top 10 bugs by risk score
        all_findings = []
        for findings in findings_by_severity.values():
            all_findings.extend(findings)
        top_bugs = sorted(all_findings, key=lambda f: f.risk_score, reverse=True)[:10]

        return ProjectAnalysis(
            project_name=project_name,
            total_files=total_files,
            total_procedures=total_procedures,
            total_loc=total_loc,
            analysis_time_seconds=analysis_time,
            critical_bugs=critical_bugs,
            high_bugs=high_bugs,
            medium_bugs=medium_bugs,
            low_bugs=low_bugs,
            sox_procedures=sox_count,
            pci_procedures=pci_count,
            gdpr_procedures=gdpr_count,
            top_bugs=top_bugs,
            estimated_bug_cost_total=(cost_min, cost_max),
            estimated_fix_hours=fix_hours,
            roi_multiplier=roi_multiplier
        )

    def _describe_intent(self, coords: Tuple[float, float, float, float]) -> str:
        """Describe intent based on coordinates"""
        l, j, p, w = coords
        dominant = max(enumerate([l, j, p, w]), key=lambda x: x[1])
        labels = ['Love (Communication)', 'Justice (Validation)', 'Power (Modification)', 'Wisdom (Retrieval)']
        return f"Dominant: {labels[dominant[0]]}"

    def _describe_execution(self, coords: Tuple[float, float, float, float]) -> str:
        """Describe execution based on coordinates"""
        l, j, p, w = coords
        dominant = max(enumerate([l, j, p, w]), key=lambda x: x[1])
        labels = ['Love (Communication)', 'Justice (Validation)', 'Power (Modification)', 'Wisdom (Retrieval)']
        return f"Actual: {labels[dominant[0]]}"

    def _assess_business_impact(
        self,
        procedure_name: str,
        tags: set,
        disharmony_score: float
    ) -> str:
        """Assess business impact"""
        impacts = []

        if disharmony_score >= 1.2:
            impacts.append("CRITICAL: Severe semantic contradiction likely causing production bugs")

        if any('sox' in tag.value for tag in tags):
            impacts.append("SOX Compliance Risk: Financial reporting integrity")

        if any('pci' in tag.value for tag in tags):
            impacts.append("PCI-DSS Risk: Payment card data security")

        if any('gdpr' in tag.value for tag in tags):
            impacts.append("GDPR Risk: Personal data protection")

        if 'DELETE' in procedure_name.upper() or 'REMOVE' in procedure_name.upper():
            impacts.append("Data Loss Risk: Unexpected deletion behavior")

        if not impacts:
            impacts.append("Maintainability: Code behavior doesn't match name")

        return " | ".join(impacts)

    def generate_report(
        self,
        analyses: List[ProjectAnalysis],
        output_path: Path
    ) -> None:
        """Generate comprehensive analysis report"""

        # Calculate totals
        total_files = sum(a.total_files for a in analyses)
        total_procedures = sum(a.total_procedures for a in analyses)
        total_loc = sum(a.total_loc for a in analyses)
        total_critical = sum(a.critical_bugs for a in analyses)
        total_high = sum(a.high_bugs for a in analyses)
        total_medium = sum(a.medium_bugs for a in analyses)
        total_low = sum(a.low_bugs for a in analyses)
        total_bugs = total_critical + total_high + total_medium + total_low

        cost_min = sum(a.estimated_bug_cost_total[0] for a in analyses)
        cost_max = sum(a.estimated_bug_cost_total[1] for a in analyses)

        # Generate report
        report = {
            'generated_at': datetime.now().isoformat(),
            'summary': {
                'total_projects': len(analyses),
                'total_files': total_files,
                'total_procedures': total_procedures,
                'total_loc': total_loc,
                'total_bugs_found': total_bugs,
                'critical_bugs': total_critical,
                'high_bugs': total_high,
                'medium_bugs': total_medium,
                'low_bugs': total_low,
                'estimated_cost_savings': {
                    'minimum': cost_min,
                    'maximum': cost_max,
                    'formatted_range': f"${cost_min:,} - ${cost_max:,}"
                },
                'key_metrics': {
                    'bugs_per_1000_loc': round((total_bugs / total_loc * 1000), 2) if total_loc > 0 else 0,
                    'critical_bug_percentage': round((total_critical / total_bugs * 100), 1) if total_bugs > 0 else 0,
                    'sox_procedures_flagged': sum(a.sox_procedures for a in analyses),
                    'pci_procedures_flagged': sum(a.pci_procedures for a in analyses),
                    'gdpr_procedures_flagged': sum(a.gdpr_procedures for a in analyses),
                }
            },
            'projects': [
                {
                    'name': a.project_name,
                    'files': a.total_files,
                    'procedures': a.total_procedures,
                    'loc': a.total_loc,
                    'bugs': {
                        'critical': a.critical_bugs,
                        'high': a.high_bugs,
                        'medium': a.medium_bugs,
                        'low': a.low_bugs,
                        'total': a.critical_bugs + a.high_bugs + a.medium_bugs + a.low_bugs
                    },
                    'compliance': {
                        'sox': a.sox_procedures,
                        'pci': a.pci_procedures,
                        'gdpr': a.gdpr_procedures
                    },
                    'financial': {
                        'cost_range': f"${a.estimated_bug_cost_total[0]:,} - ${a.estimated_bug_cost_total[1]:,}",
                        'fix_hours': a.estimated_fix_hours,
                        'roi_multiplier': round(a.roi_multiplier, 1)
                    },
                    'top_findings': [
                        {
                            'procedure': bug.procedure_name,
                            'file': bug.file_path,
                            'severity': bug.severity,
                            'disharmony_score': round(bug.disharmony_score, 2),
                            'risk_score': round(bug.risk_score, 1),
                            'intent': bug.intent_description,
                            'execution': bug.execution_description,
                            'business_impact': bug.business_impact,
                            'estimated_cost_range': f"${bug.estimated_cost_range[0]:,} - ${bug.estimated_cost_range[1]:,}",
                            'compliance_tags': bug.compliance_tags
                        }
                        for bug in a.top_bugs[:5]  # Top 5 per project
                    ]
                }
                for a in analyses
            ]
        }

        # Write JSON report
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)

        print(f"\n✅ Report saved to {output_path}")


def main():
    """Run enterprise analysis"""
    analyzer = EnterpriseAnalyzer()

    # Analyze existing examples
    examples_path = Path(__file__).parent / "examples"
    external_path = Path(__file__).parent / "external_samples"

    analyses = []

    # Analyze examples
    if examples_path.exists():
        analysis = analyzer.analyze_project(
            examples_path,
            "COBOL Harmonizer Examples"
        )
        analyses.append(analysis)

    # Analyze external samples
    if external_path.exists():
        analysis = analyzer.analyze_project(
            external_path,
            "Real-World COBOL Samples"
        )
        analyses.append(analysis)

    # Generate report
    output_path = Path(__file__).parent / "ENTERPRISE_ANALYSIS_REPORT.json"
    analyzer.generate_report(analyses, output_path)

    print("\n" + "="*80)
    print("ENTERPRISE ANALYSIS COMPLETE")
    print("="*80)


if __name__ == '__main__':
    main()
