#!/usr/bin/env python3
"""
Compliance Features Demo

Demonstrates audit logging, compliance tagging, risk assessment,
and report generation capabilities.
"""

import sys
from pathlib import Path
from datetime import datetime, timedelta

# Add project to path
sys.path.insert(0, str(Path(__file__).parent))

from cobol_harmonizer.compliance import (
    AuditLogger,
    AuditEvent,
    AuditAction,
    ComplianceTagger,
    ComplianceTag,
    ComplianceRiskAssessor,
    ComplianceReporter,
    ComplianceConfig,
    ComplianceRule,
    ComplianceFramework,
)


def print_section(title: str):
    """Print section header"""
    print()
    print("=" * 80)
    print(title)
    print("=" * 80)
    print()


def demo_audit_logging():
    """Demonstrate audit logging"""
    print_section("DEMO 1: Audit Logging")

    # Create audit logger
    config = ComplianceConfig(
        enabled=True,
        audit_log_path=".demo_audit_log",
        track_user=True,
        track_environment=True
    )
    logger = AuditLogger(config)

    print("✅ Audit logger initialized")
    print(f"   Log path: {config.audit_log_path}")
    print()

    # Log some actions
    actions = [
        ("ANALYSIS", "examples/harmonious_example.cbl", "MAIN-PROCEDURE", {"disharmony_score": 0.12}),
        ("ANALYSIS", "examples/disharmonious_example.cbl", "GET-CUSTOMER-BALANCE", {"disharmony_score": 1.12}),
        ("BASELINE_SAVE", "examples/", None, {"files": 2, "baseline_name": "2025-Q4"}),
    ]

    for action_type, file_path, procedure, details in actions:
        event = AuditEvent(AuditAction[action_type])
        event.with_file(file_path)
        if procedure:
            event.with_procedure(procedure)
        event.with_details(**details)
        event.with_justification("Compliance demo")

        logger.log(event)

        print(f"✅ Logged: {action_type} - {file_path}")
        if procedure:
            print(f"   Procedure: {procedure}")
        print(f"   Details: {details}")
        print()

    # Query audit log
    print_section("QUERYING AUDIT LOG")

    recent_entries = logger.query(
        start_date=datetime.now() - timedelta(minutes=5),
        limit=10
    )

    print(f"Found {len(recent_entries)} recent audit entries:")
    print()

    for entry in recent_entries:
        print(f"[{entry.timestamp:%Y-%m-%d %H:%M:%S}] {entry.action.value}")
        print(f"  User: {entry.user}")
        print(f"  File: {entry.file_path}")
        if entry.procedure_name:
            print(f"  Procedure: {entry.procedure_name}")
        print(f"  Details: {entry.details}")
        print()

    logger.close()


def demo_compliance_tagging():
    """Demonstrate compliance tagging"""
    print_section("DEMO 2: Compliance Tagging")

    tagger = ComplianceTagger()

    # Test procedures
    procedures = [
        ("PROCESS-CREDIT-CARD-PAYMENT", ["READ", "WRITE", "ENCRYPT"], ["CARD-FILE"]),
        ("GENERATE-FINANCIAL-REPORT", ["READ", "COMPUTE", "DISPLAY"], ["LEDGER-FILE"]),
        ("DELETE-CUSTOMER-PROFILE", ["READ", "DELETE"], ["CUSTOMER-FILE"]),
        ("VALIDATE-USER-LOGIN", ["READ", "IF"], ["USER-FILE"]),
        ("CALCULATE-INTEREST-RATE", ["READ", "COMPUTE"], ["ACCOUNT-FILE"]),
    ]

    for proc_name, verbs, files in procedures:
        tags = tagger.tag_procedure(proc_name, verbs=verbs, files_accessed=files)
        level = tagger.get_compliance_level(tags)

        print(f"📋 Procedure: {proc_name}")
        print(f"   Compliance Level: {level.value.upper()}")
        print(f"   Tags: {', '.join(tag.value for tag in sorted(tags, key=lambda t: t.value))}")
        print()

        # Show applicable rules
        rules = tagger.get_applicable_rules(proc_name)
        if rules:
            print(f"   Applicable Rules:")
            for rule in rules:
                print(f"     • {rule.name} ({rule.framework.value.upper()}) - {rule.severity}")
        print()


def demo_risk_assessment():
    """Demonstrate risk assessment"""
    print_section("DEMO 3: Risk Assessment")

    assessor = ComplianceRiskAssessor()

    # Test procedures with different risk profiles
    test_cases = [
        {
            'procedure_name': 'PROCESS-CREDIT-CARD-PAYMENT',
            'file_path': 'payment_processing.cbl',
            'disharmony_score': 0.85,
            'disharmony_level': 'concerning',
            'verbs': ['READ', 'WRITE', 'ENCRYPT'],
            'fan_in': 15,  # High impact
        },
        {
            'procedure_name': 'DISPLAY-ACCOUNT-INFO',
            'file_path': 'account_display.cbl',
            'disharmony_score': 0.25,
            'disharmony_level': 'harmonious',
            'verbs': ['READ', 'DISPLAY'],
            'fan_in': 3,
        },
        {
            'procedure_name': 'UPDATE-CUSTOMER-ADDRESS',
            'file_path': 'customer_mgmt.cbl',
            'disharmony_score': 0.45,
            'disharmony_level': 'minor_drift',
            'verbs': ['READ', 'UPDATE', 'WRITE'],
            'fan_in': 5,
        },
    ]

    assessments = []

    for test_case in test_cases:
        assessment = assessor.assess_procedure(**test_case)
        assessments.append(assessment)

        print(f"🔍 Procedure: {assessment.procedure_name}")
        print(f"   Risk Level: {assessment.risk_level.value.upper()}")
        print(f"   Risk Score: {assessment.risk_score}/100")
        print(f"   Disharmony: {assessment.disharmony_score:.2f} ({assessment.disharmony_level})")
        print(f"   Compliance Level: {assessment.compliance_level.value.upper()}")
        print(f"   Call Impact: {assessment.fan_in} callers")
        print()

        if assessment.compliance_tags:
            print(f"   Compliance Tags:")
            for tag in sorted(assessment.compliance_tags, key=lambda t: t.value):
                print(f"     • {tag.value}")
            print()

        if assessment.warnings:
            print(f"   ⚠️  WARNINGS:")
            for warning in assessment.warnings:
                print(f"     {warning}")
            print()

        if assessment.recommendations:
            print(f"   📝 RECOMMENDATIONS:")
            for i, rec in enumerate(assessment.recommendations[:3], 1):  # Top 3
                print(f"     {i}. {rec}")
            print()

        print()

    # Show summary
    print_section("RISK SUMMARY")
    summary = assessor.get_compliance_summary(assessments)

    print(f"Total Procedures Analyzed: {summary['total_procedures']}")
    print()
    print(f"Risk Distribution:")
    for level, count in summary['risk_distribution'].items():
        print(f"  {level.upper()}: {count}")
    print()
    print(f"Compliance Distribution:")
    for level, count in summary['compliance_distribution'].items():
        print(f"  {level.upper()}: {count}")
    print()
    print(f"Critical Risk: {summary['critical_risk_percentage']:.1f}%")
    print(f"High Risk: {summary['high_risk_percentage']:.1f}%")

    return assessments


def demo_report_generation(assessments):
    """Demonstrate report generation"""
    print_section("DEMO 4: Compliance Report Generation")

    reporter = ComplianceReporter()

    # Generate report
    report = reporter.generate_report(
        assessments=assessments,
        report_type="compliance",
        period_start=datetime.now() - timedelta(days=30),
        period_end=datetime.now(),
        generated_by="Compliance Demo"
    )

    print(f"📊 Report Generated")
    print(f"   Report ID: {report.report_id}")
    print(f"   Type: {report.report_type}")
    print(f"   Period: {report.period_start:%Y-%m-%d} to {report.period_end:%Y-%m-%d}")
    print()

    print(f"Summary:")
    print(f"  Files Analyzed: {report.total_files_analyzed}")
    print(f"  Procedures Analyzed: {report.total_procedures_analyzed}")
    print(f"  Total Violations: {report.total_violations}")
    print(f"    - Critical: {report.critical_violations}")
    print(f"    - High: {report.high_violations}")
    print()

    if report.framework_status:
        print(f"Framework Status:")
        for framework, status in report.framework_status.items():
            icon = "✅" if status == "Compliant" else ("⚠️" if "Review" in status else "❌")
            print(f"  {framework.value.upper()}: {icon} {status}")
        print()

    # Export to files
    output_dir = Path("compliance_reports")
    output_dir.mkdir(exist_ok=True)

    json_file = output_dir / "compliance_report.json"
    md_file = output_dir / "compliance_report.md"
    html_file = output_dir / "compliance_report.html"
    csv_file = output_dir / "compliance_violations.csv"

    reporter.export_json(report, str(json_file))
    reporter.export_markdown(report, str(md_file))
    reporter.export_html(report, str(html_file))
    reporter.export_csv(report, str(csv_file))

    print(f"✅ Reports exported:")
    print(f"   JSON:     {json_file}")
    print(f"   Markdown: {md_file}")
    print(f"   HTML:     {html_file}")
    print(f"   CSV:      {csv_file}")
    print()

    print(f"💡 TIP: Open {html_file} in your browser for a visual report!")


def demo_custom_rules():
    """Demonstrate custom compliance rules"""
    print_section("DEMO 5: Custom Compliance Rules")

    # Define custom rules for a bank
    custom_rules = [
        ComplianceRule(
            rule_id="BANK-001",
            framework=ComplianceFramework.CUSTOM,
            name="Wire Transfer Procedures",
            description="All wire transfer procedures require dual approval",
            severity="critical",
            applies_to=["*WIRE*", "*TRANSFER*", "*ACH*"]
        ),
        ComplianceRule(
            rule_id="BANK-002",
            framework=ComplianceFramework.CUSTOM,
            name="Interest Calculation",
            description="Interest calculations must be audited quarterly",
            severity="high",
            applies_to=["*INTEREST*", "*CALCULATE-RATE*"]
        ),
        ComplianceRule(
            rule_id="BANK-003",
            framework=ComplianceFramework.CUSTOM,
            name="Overdraft Processing",
            description="Overdraft handling requires special monitoring",
            severity="high",
            applies_to=["*OVERDRAFT*", "*NSF*"]
        ),
    ]

    tagger = ComplianceTagger(rules=custom_rules)

    print("Custom compliance rules defined:")
    for rule in custom_rules:
        print(f"  • {rule.rule_id}: {rule.name} ({rule.severity})")
    print()

    # Test procedures against custom rules
    test_procedures = [
        "PROCESS-WIRE-TRANSFER",
        "CALCULATE-INTEREST-RATE",
        "HANDLE-OVERDRAFT-FEE",
        "DISPLAY-ACCOUNT-BALANCE",
    ]

    for proc_name in test_procedures:
        applicable = tagger.get_applicable_rules(proc_name)

        print(f"📋 Procedure: {proc_name}")
        if applicable:
            print(f"   Applicable Custom Rules:")
            for rule in applicable:
                print(f"     • {rule.name} ({rule.severity})")
                print(f"       {rule.description}")
        else:
            print(f"   No custom rules apply")
        print()


def main():
    """Run all demos"""
    print()
    print("=" * 80)
    print("COBOL CODE HARMONIZER - COMPLIANCE FEATURES DEMO")
    print("=" * 80)
    print()
    print("This demo showcases:")
    print("  1. Audit Logging - Track who did what, when, and why")
    print("  2. Compliance Tagging - Auto-detect SOX, PCI, GDPR-relevant code")
    print("  3. Risk Assessment - Combine semantic analysis with compliance")
    print("  4. Report Generation - Create audit-ready reports")
    print("  5. Custom Rules - Define organization-specific compliance rules")
    print()

    try:
        # Run demos
        demo_audit_logging()
        demo_compliance_tagging()
        assessments = demo_risk_assessment()
        demo_report_generation(assessments)
        demo_custom_rules()

        # Final summary
        print_section("DEMO COMPLETE")
        print("✅ All compliance features demonstrated successfully!")
        print()
        print("Next steps:")
        print("  1. Review generated reports in ./compliance_reports/")
        print("  2. Check audit log in ./.demo_audit_log/")
        print("  3. Read docs/COMPLIANCE_FEATURES.md for full documentation")
        print("  4. Integrate into your COBOL analysis workflow")
        print()
        print("🎉 Compliance features are production-ready for:")
        print("   • Financial institutions (SOX, PCI-DSS)")
        print("   • Healthcare organizations (HIPAA)")
        print("   • EU operations (GDPR)")
        print("   • Government/enterprise (FISMA, ISO 27001)")
        print()

    except Exception as e:
        print(f"❌ Demo failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()
