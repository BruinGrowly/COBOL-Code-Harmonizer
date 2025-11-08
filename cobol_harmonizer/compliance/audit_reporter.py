"""
Compliance Reporter

Generates audit reports for compliance and regulatory purposes.
"""

import json
from datetime import datetime
from typing import List, Dict, Optional
from pathlib import Path
from enum import Enum
import uuid

from .models import AuditReport, ComplianceViolation, AuditEntry, ComplianceFramework
from .risk_assessor import RiskAssessment, RiskLevel


class ReportFormat(Enum):
    """Report output formats"""
    JSON = "json"
    HTML = "html"
    PDF = "pdf"
    CSV = "csv"
    MARKDOWN = "markdown"


class ComplianceReporter:
    """
    Generates compliance and audit reports.

    Supports multiple output formats:
    - JSON for programmatic consumption
    - HTML for human review
    - Markdown for documentation
    - CSV for spreadsheet analysis
    """

    def __init__(self):
        pass

    def generate_report(
        self,
        assessments: List[RiskAssessment],
        audit_entries: Optional[List[AuditEntry]] = None,
        baseline_deviations: Optional[List[Dict]] = None,
        report_type: str = "compliance",
        period_start: Optional[datetime] = None,
        period_end: Optional[datetime] = None,
        generated_by: Optional[str] = None
    ) -> AuditReport:
        """
        Generate comprehensive audit report.

        Args:
            assessments: List of risk assessments
            audit_entries: List of audit log entries
            baseline_deviations: List of baseline deviations
            report_type: Type of report
            period_start: Start of reporting period
            period_end: End of reporting period
            generated_by: Who generated the report

        Returns:
            AuditReport
        """
        # Generate report ID
        report_id = f"AUDIT-{datetime.now():%Y%m%d-%H%M%S}-{uuid.uuid4().hex[:8]}"

        # Count files analyzed
        unique_files = len(set(a.file_path for a in assessments))

        # Find violations (critical and high risk procedures)
        violations = self._extract_violations(assessments)

        # Determine framework status
        framework_status = self._assess_framework_status(assessments)

        # Extract high-risk changes
        high_risk_changes = self._extract_high_risk_changes(assessments)

        # Create report
        report = AuditReport(
            report_id=report_id,
            generated_at=datetime.now(),
            report_type=report_type,
            period_start=period_start or datetime.now(),
            period_end=period_end or datetime.now(),
            total_files_analyzed=unique_files,
            total_procedures_analyzed=len(assessments),
            total_violations=len(violations),
            critical_violations=sum(1 for v in violations if v.severity == 'critical'),
            high_violations=sum(1 for v in violations if v.severity == 'high'),
            violations=violations,
            audit_entries=audit_entries or [],
            framework_status=framework_status,
            baseline_deviations=baseline_deviations or [],
            high_risk_changes=high_risk_changes,
            generated_by=generated_by,
        )

        return report

    def _extract_violations(self, assessments: List[RiskAssessment]) -> List[ComplianceViolation]:
        """Extract compliance violations from assessments"""
        violations = []

        for assessment in assessments:
            # Critical risk = violation
            if assessment.risk_level == RiskLevel.CRITICAL:
                # Create a pseudo-rule for critical risk
                from .models import ComplianceRule

                rule = ComplianceRule(
                    rule_id="RISK-001",
                    framework=ComplianceFramework.CUSTOM,
                    name="Critical Risk Procedure",
                    description="Procedure has critical compliance risk",
                    severity="critical"
                )

                violation = ComplianceViolation(
                    rule=rule,
                    procedure_name=assessment.procedure_name,
                    file_path=assessment.file_path,
                    severity="critical",
                    description=(
                        f"Risk score: {assessment.risk_score}/100. "
                        f"Disharmony: {assessment.disharmony_score:.2f}. "
                        f"Compliance level: {assessment.compliance_level.value}. "
                        f"Fan-in: {assessment.fan_in}."
                    ),
                    recommendation="\n".join(assessment.recommendations),
                    evidence={
                        'disharmony_score': assessment.disharmony_score,
                        'risk_score': assessment.risk_score,
                        'compliance_tags': [tag.value for tag in assessment.compliance_tags],
                        'fan_in': assessment.fan_in,
                    }
                )
                violations.append(violation)

            # High risk with compliance tags = violation
            elif assessment.risk_level == RiskLevel.HIGH and assessment.compliance_tags:
                from .models import ComplianceRule

                rule = ComplianceRule(
                    rule_id="RISK-002",
                    framework=ComplianceFramework.CUSTOM,
                    name="High Risk Compliance Procedure",
                    description="Compliance-tagged procedure with high risk",
                    severity="high"
                )

                violation = ComplianceViolation(
                    rule=rule,
                    procedure_name=assessment.procedure_name,
                    file_path=assessment.file_path,
                    severity="high",
                    description=(
                        f"Risk score: {assessment.risk_score}/100. "
                        f"Compliance tags: {', '.join(tag.value for tag in assessment.compliance_tags)}. "
                        f"Requires review."
                    ),
                    recommendation="\n".join(assessment.recommendations[:3]),  # Top 3 recommendations
                    evidence={
                        'disharmony_score': assessment.disharmony_score,
                        'risk_score': assessment.risk_score,
                        'compliance_tags': [tag.value for tag in assessment.compliance_tags],
                    }
                )
                violations.append(violation)

        return violations

    def _assess_framework_status(self, assessments: List[RiskAssessment]) -> Dict[ComplianceFramework, str]:
        """Assess compliance status for each framework"""
        status = {}

        # Count critical/high risk procedures by framework-relevant tags
        from .compliance_tagger import ComplianceTag

        framework_tag_mapping = {
            ComplianceFramework.SOX: [
                ComplianceTag.SOX_FINANCIAL_REPORTING,
                ComplianceTag.SOX_ACCESS_CONTROL,
                ComplianceTag.SOX_AUDIT_TRAIL,
            ],
            ComplianceFramework.PCI_DSS: [
                ComplianceTag.PCI_CARDHOLDER_DATA,
                ComplianceTag.PCI_ENCRYPTION,
                ComplianceTag.PCI_ACCESS_CONTROL,
            ],
            ComplianceFramework.GDPR: [
                ComplianceTag.GDPR_PERSONAL_DATA,
                ComplianceTag.GDPR_DATA_DELETION,
                ComplianceTag.GDPR_DATA_EXPORT,
            ],
        }

        for framework, relevant_tags in framework_tag_mapping.items():
            # Find assessments relevant to this framework
            relevant = [
                a for a in assessments
                if any(tag in a.compliance_tags for tag in relevant_tags)
            ]

            if not relevant:
                status[framework] = "Not Applicable"
                continue

            # Check if any are critical or high risk
            critical_count = sum(1 for a in relevant if a.risk_level == RiskLevel.CRITICAL)
            high_count = sum(1 for a in relevant if a.risk_level == RiskLevel.HIGH)

            if critical_count > 0:
                status[framework] = f"Non-Compliant ({critical_count} critical issues)"
            elif high_count > 0:
                status[framework] = f"Needs Review ({high_count} high-risk issues)"
            else:
                status[framework] = "Compliant"

        return status

    def _extract_high_risk_changes(self, assessments: List[RiskAssessment]) -> List[Dict]:
        """Extract high-risk procedures that would impact compliance if changed"""
        high_risk = []

        for assessment in assessments:
            if assessment.risk_level in [RiskLevel.CRITICAL, RiskLevel.HIGH]:
                high_risk.append({
                    'procedure_name': assessment.procedure_name,
                    'file_path': assessment.file_path,
                    'risk_level': assessment.risk_level.value,
                    'risk_score': assessment.risk_score,
                    'compliance_level': assessment.compliance_level.value,
                    'compliance_tags': [tag.value for tag in assessment.compliance_tags],
                    'impact': f"{assessment.fan_in} dependent procedures",
                })

        return high_risk

    def export_json(self, report: AuditReport, output_path: str) -> None:
        """
        Export report as JSON.

        Args:
            report: Audit report
            output_path: Path to output file
        """
        with open(output_path, 'w') as f:
            json.dump(report.to_dict(), f, indent=2)

    def export_markdown(self, report: AuditReport, output_path: str) -> None:
        """
        Export report as Markdown.

        Args:
            report: Audit report
            output_path: Path to output file
        """
        md = []
        md.append(f"# Compliance Audit Report")
        md.append(f"")
        md.append(f"**Report ID:** {report.report_id}")
        md.append(f"**Generated:** {report.generated_at:%Y-%m-%d %H:%M:%S}")
        md.append(f"**Period:** {report.period_start:%Y-%m-%d} to {report.period_end:%Y-%m-%d}")
        if report.generated_by:
            md.append(f"**Generated By:** {report.generated_by}")
        md.append(f"")

        # Executive Summary
        md.append(f"## Executive Summary")
        md.append(f"")
        md.append(f"- **Total Files Analyzed:** {report.total_files_analyzed}")
        md.append(f"- **Total Procedures Analyzed:** {report.total_procedures_analyzed}")
        md.append(f"- **Total Violations:** {report.total_violations}")
        md.append(f"  - Critical: {report.critical_violations}")
        md.append(f"  - High: {report.high_violations}")
        md.append(f"")

        # Framework Status
        if report.framework_status:
            md.append(f"## Compliance Framework Status")
            md.append(f"")
            for framework, status in report.framework_status.items():
                icon = "✅" if status == "Compliant" else ("⚠️" if "Review" in status else "❌")
                md.append(f"- **{framework.value.upper()}:** {icon} {status}")
            md.append(f"")

        # Violations
        if report.violations:
            md.append(f"## Compliance Violations")
            md.append(f"")

            # Group by severity
            critical = [v for v in report.violations if v.severity == 'critical']
            high = [v for v in report.violations if v.severity == 'high']

            if critical:
                md.append(f"### Critical Violations ({len(critical)})")
                md.append(f"")
                for v in critical:
                    md.append(f"#### {v.procedure_name}")
                    md.append(f"")
                    md.append(f"- **File:** `{v.file_path}`")
                    md.append(f"- **Rule:** {v.rule.name} ({v.rule.rule_id})")
                    md.append(f"- **Description:** {v.description}")
                    md.append(f"- **Recommendation:**")
                    md.append(f"  ```")
                    md.append(f"  {v.recommendation}")
                    md.append(f"  ```")
                    md.append(f"")

            if high:
                md.append(f"### High Priority Violations ({len(high)})")
                md.append(f"")
                for v in high:
                    md.append(f"#### {v.procedure_name}")
                    md.append(f"")
                    md.append(f"- **File:** `{v.file_path}`")
                    md.append(f"- **Description:** {v.description}")
                    md.append(f"")

        # High-risk changes
        if report.high_risk_changes:
            md.append(f"## High-Risk Procedures ({len(report.high_risk_changes)})")
            md.append(f"")
            md.append(f"Procedures that require extra scrutiny before modification:")
            md.append(f"")

            for change in report.high_risk_changes[:10]:  # Top 10
                md.append(f"### {change['procedure_name']}")
                md.append(f"")
                md.append(f"- **Risk Level:** {change['risk_level'].upper()}")
                md.append(f"- **Risk Score:** {change['risk_score']}/100")
                md.append(f"- **Compliance Level:** {change['compliance_level']}")
                md.append(f"- **Impact:** {change['impact']}")
                if change['compliance_tags']:
                    md.append(f"- **Tags:** {', '.join(change['compliance_tags'])}")
                md.append(f"")

        # Attestation
        md.append(f"---")
        md.append(f"")
        md.append(f"*This report was automatically generated by COBOL Code Harmonizer*")
        md.append(f"")

        # Write to file
        with open(output_path, 'w') as f:
            f.write('\n'.join(md))

    def export_csv(self, report: AuditReport, output_path: str) -> None:
        """
        Export violations as CSV.

        Args:
            report: Audit report
            output_path: Path to output file
        """
        import csv

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)

            # Header
            writer.writerow([
                'Procedure Name',
                'File Path',
                'Severity',
                'Rule ID',
                'Rule Name',
                'Description',
                'Recommendation'
            ])

            # Violations
            for v in report.violations:
                writer.writerow([
                    v.procedure_name,
                    v.file_path,
                    v.severity,
                    v.rule.rule_id,
                    v.rule.name,
                    v.description,
                    v.recommendation
                ])

    def export_html(self, report: AuditReport, output_path: str) -> None:
        """
        Export report as HTML.

        Args:
            report: Audit report
            output_path: Path to output file
        """
        html = f"""
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Compliance Audit Report - {report.report_id}</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 40px; background: #f5f5f5; }}
        .container {{ max-width: 1200px; margin: 0 auto; background: white; padding: 30px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }}
        h1 {{ color: #333; border-bottom: 3px solid #4CAF50; padding-bottom: 10px; }}
        h2 {{ color: #555; margin-top: 30px; }}
        .metadata {{ background: #f9f9f9; padding: 15px; border-left: 4px solid #4CAF50; margin: 20px 0; }}
        .summary {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin: 20px 0; }}
        .summary-card {{ background: #fff; border: 1px solid #ddd; padding: 20px; border-radius: 5px; }}
        .summary-card h3 {{ margin: 0 0 10px 0; color: #666; font-size: 14px; }}
        .summary-card .value {{ font-size: 32px; font-weight: bold; color: #4CAF50; }}
        .violation {{ border: 1px solid #ddd; padding: 15px; margin: 10px 0; border-radius: 5px; }}
        .violation.critical {{ border-left: 5px solid #f44336; background: #ffebee; }}
        .violation.high {{ border-left: 5px solid #ff9800; background: #fff3e0; }}
        .framework-status {{ margin: 20px 0; }}
        .framework-item {{ padding: 10px; margin: 5px 0; background: #f9f9f9; border-left: 4px solid #4CAF50; }}
        .framework-item.compliant {{ border-left-color: #4CAF50; }}
        .framework-item.needs-review {{ border-left-color: #ff9800; }}
        .framework-item.non-compliant {{ border-left-color: #f44336; }}
        code {{ background: #f5f5f5; padding: 2px 5px; border-radius: 3px; }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Compliance Audit Report</h1>

        <div class="metadata">
            <p><strong>Report ID:</strong> {report.report_id}</p>
            <p><strong>Generated:</strong> {report.generated_at:%Y-%m-%d %H:%M:%S}</p>
            <p><strong>Period:</strong> {report.period_start:%Y-%m-%d} to {report.period_end:%Y-%m-%d}</p>
            {f'<p><strong>Generated By:</strong> {report.generated_by}</p>' if report.generated_by else ''}
        </div>

        <h2>Executive Summary</h2>
        <div class="summary">
            <div class="summary-card">
                <h3>Files Analyzed</h3>
                <div class="value">{report.total_files_analyzed}</div>
            </div>
            <div class="summary-card">
                <h3>Procedures Analyzed</h3>
                <div class="value">{report.total_procedures_analyzed}</div>
            </div>
            <div class="summary-card">
                <h3>Total Violations</h3>
                <div class="value">{report.total_violations}</div>
            </div>
            <div class="summary-card">
                <h3>Critical Violations</h3>
                <div class="value" style="color: #f44336;">{report.critical_violations}</div>
            </div>
        </div>

        {self._generate_html_framework_status(report) if report.framework_status else ''}

        {self._generate_html_violations(report) if report.violations else ''}

        {self._generate_html_high_risk(report) if report.high_risk_changes else ''}

        <hr style="margin-top: 40px;">
        <p style="text-align: center; color: #999; font-size: 12px;">
            This report was automatically generated by COBOL Code Harmonizer
        </p>
    </div>
</body>
</html>
"""

        with open(output_path, 'w') as f:
            f.write(html)

    def _generate_html_framework_status(self, report: AuditReport) -> str:
        """Generate HTML for framework status section"""
        html = "<h2>Compliance Framework Status</h2>"
        html += '<div class="framework-status">'

        for framework, status in report.framework_status.items():
            css_class = "compliant"
            if "Non-Compliant" in status:
                css_class = "non-compliant"
            elif "Needs Review" in status:
                css_class = "needs-review"

            icon = "✅" if css_class == "compliant" else ("⚠️" if css_class == "needs-review" else "❌")

            html += f'<div class="framework-item {css_class}">'
            html += f'<strong>{framework.value.upper()}:</strong> {icon} {status}'
            html += '</div>'

        html += '</div>'
        return html

    def _generate_html_violations(self, report: AuditReport) -> str:
        """Generate HTML for violations section"""
        html = f"<h2>Compliance Violations ({len(report.violations)})</h2>"

        for v in report.violations:
            html += f'<div class="violation {v.severity}">'
            html += f'<h3>{v.procedure_name}</h3>'
            html += f'<p><strong>File:</strong> <code>{v.file_path}</code></p>'
            html += f'<p><strong>Rule:</strong> {v.rule.name} ({v.rule.rule_id})</p>'
            html += f'<p><strong>Severity:</strong> {v.severity.upper()}</p>'
            html += f'<p><strong>Description:</strong> {v.description}</p>'
            html += f'<p><strong>Recommendation:</strong></p>'
            html += f'<pre style="background: #f5f5f5; padding: 10px; border-radius: 3px;">{v.recommendation}</pre>'
            html += '</div>'

        return html

    def _generate_html_high_risk(self, report: AuditReport) -> str:
        """Generate HTML for high-risk procedures section"""
        html = f"<h2>High-Risk Procedures ({len(report.high_risk_changes)})</h2>"
        html += "<p>Procedures that require extra scrutiny before modification:</p>"

        for change in report.high_risk_changes[:10]:
            html += f'<div class="violation {change["risk_level"]}">'
            html += f'<h3>{change["procedure_name"]}</h3>'
            html += f'<p><strong>Risk Score:</strong> {change["risk_score"]}/100</p>'
            html += f'<p><strong>Compliance Level:</strong> {change["compliance_level"]}</p>'
            html += f'<p><strong>Impact:</strong> {change["impact"]}</p>'
            if change['compliance_tags']:
                html += f'<p><strong>Tags:</strong> {", ".join(change["compliance_tags"])}</p>'
            html += '</div>'

        return html
