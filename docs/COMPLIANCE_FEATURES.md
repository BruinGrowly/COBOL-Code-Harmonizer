# Compliance and Audit Features

**Version:** 1.0
**Date:** 2025-11-08
**Status:** Production-Ready

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Overview](#overview)
3. [Features](#features)
4. [Quick Start](#quick-start)
5. [Compliance Frameworks](#compliance-frameworks)
6. [Configuration](#configuration)
7. [Usage Examples](#usage-examples)
8. [Reports](#reports)
9. [Best Practices](#best-practices)
10. [API Reference](#api-reference)

---

## Executive Summary

The COBOL Code Harmonizer now includes comprehensive **audit and compliance features** designed for financial institutions, healthcare organizations, and enterprises that must meet regulatory requirements like **SOX, PCI-DSS, GDPR, HIPAA, and ISO 27001**.

### Key Capabilities

✅ **Audit Logging** - Track all code analysis activities with full traceability
✅ **Compliance Tagging** - Automatically identify SOX, PCI, GDPR-relevant procedures
✅ **Risk Assessment** - Combine semantic analysis with compliance criticality
✅ **Audit Reports** - Generate compliance-ready reports in multiple formats
✅ **Change Tracking** - Monitor changes to critical compliance procedures
✅ **Non-Intrusive** - Opt-in features that don't interfere with core analysis

---

## Overview

### Why Compliance Features?

COBOL systems often handle:
- Financial transactions (SOX requirements)
- Credit card data (PCI-DSS requirements)
- Personal data (GDPR requirements)
- Healthcare information (HIPAA requirements)

These regulations require:
1. **Audit trails** - Who changed what, when, and why
2. **Change control** - Documented review and approval
3. **Risk assessment** - Understanding impact of changes
4. **Evidence of compliance** - Reports for auditors

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Compliance Module                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│  │ Audit Logger │───▶│  Compliance  │───▶│     Risk     │      │
│  │              │    │    Tagger    │    │   Assessor   │      │
│  └──────────────┘    └──────────────┘    └──────────────┘      │
│         │                    │                     │             │
│         │                    │                     │             │
│         ▼                    ▼                     ▼             │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│  │  Audit Log   │    │  Compliance  │    │    Audit     │      │
│  │  (JSONL)     │    │    Tags      │    │   Reports    │      │
│  └──────────────┘    └──────────────┘    └──────────────┘      │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

---

## Features

### 1. Audit Logging

**What:** Persistent, tamper-evident log of all analysis activities

**Tracks:**
- Who performed the action
- When it was performed
- What files/procedures were analyzed
- Results of the analysis
- Justification for changes (optional)
- Environment information (hostname, platform)

**Storage:** JSONL format (one JSON object per line) for easy parsing

**Retention:** Configurable (default: 365 days)

**Example Audit Entry:**
```json
{
  "timestamp": "2025-11-08T10:30:00",
  "action": "analysis",
  "user": "jsmith",
  "file_path": "/cobol/src/BANKACCT.cbl",
  "procedure_name": "VALIDATE-TRANSACTION",
  "details": {
    "disharmony_score": 0.45,
    "compliance_tags": ["pci_cardholder_data", "financial_transaction"]
  },
  "result": "success",
  "justification": "Monthly code review",
  "environment": {
    "hostname": "dev-server-01",
    "platform": "Linux"
  }
}
```

### 2. Compliance Tagging

**What:** Automatically identifies procedures relevant to regulations

**Supported Frameworks:**
- **SOX** (Sarbanes-Oxley) - Financial reporting, access control, audit trails
- **PCI-DSS** - Payment card data, encryption, access control
- **GDPR** - Personal data, data deletion, data export
- **HIPAA** - Protected health information, encryption
- **ISO 27001** - Information security management
- **NIST, FISMA, COBIT** - Government and enterprise standards

**Detection Methods:**
1. **Name-based** - Procedure names (e.g., `PROCESS-PAYMENT` → PCI tag)
2. **Verb-based** - COBOL verbs used (e.g., `DELETE` → data deletion tag)
3. **File-based** - Files accessed (e.g., `CUSTOMER-FILE` → GDPR tag)

**Example Tags:**
- `sox_financial_reporting` - SOX-relevant financial procedures
- `pci_cardholder_data` - Handles credit card data
- `gdpr_personal_data` - Processes personal information
- `authentication` - User authentication procedures
- `encryption` - Encryption/decryption operations

### 3. Risk Assessment

**What:** Combines semantic disharmony with compliance criticality

**Risk Score Calculation (0-100):**
```
Risk Score = (40% × Semantic Disharmony) +
             (30% × Compliance Criticality) +
             (20% × Call Graph Impact) +
             (10% × Compliance Tagging)
```

**Risk Levels:**
- **CRITICAL** (75-100) - Immediate action required
- **HIGH** (50-74) - Priority review needed
- **MEDIUM** (25-49) - Standard review
- **LOW** (0-24) - Minimal concern

**Example:**
```
Procedure: PROCESS-CREDIT-CARD-PAYMENT
- Disharmony Score: 0.85 (concerning)
- Compliance Tags: [pci_cardholder_data, payment_processing]
- Fan-in: 15 (called by 15 other procedures)
→ Risk Score: 82/100 (CRITICAL)
```

### 4. Audit Reports

**What:** Compliance-ready reports for auditors and management

**Formats:**
- **JSON** - Programmatic consumption, CI/CD integration
- **Markdown** - Documentation, version control
- **HTML** - Executive review, visual presentation
- **CSV** - Spreadsheet analysis

**Report Contents:**
1. Executive summary (files analyzed, violations found)
2. Compliance framework status (SOX compliant? PCI compliant?)
3. Critical violations with recommendations
4. High-risk procedures requiring extra scrutiny
5. Audit trail of recent activities
6. Baseline deviations (changes to critical code)

---

## Quick Start

### Installation

The compliance module is included in COBOL Code Harmonizer. No additional installation needed.

### Basic Usage

```python
from cobol_harmonizer.compliance import (
    AuditLogger, AuditEvent, AuditAction,
    ComplianceTagger, ComplianceRiskAssessor,
    ComplianceReporter
)

# 1. Enable audit logging
logger = AuditLogger()

# 2. Log an analysis
event = AuditEvent(AuditAction.ANALYSIS)
event.with_file("BANKACCT.cbl")
event.with_procedure("PROCESS-TRANSACTION")
event.with_details(disharmony_score=0.65)
event.with_justification("Monthly code review")

logger.log(event)

# 3. Tag a procedure
tagger = ComplianceTagger()
tags = tagger.tag_procedure(
    "PROCESS-CREDIT-CARD",
    verbs=["READ", "WRITE", "ENCRYPT"],
    files_accessed=["CARD-FILE"]
)
# Result: {pci_cardholder_data, payment_processing, encryption}

# 4. Assess risk
assessor = ComplianceRiskAssessor()
risk = assessor.assess_procedure(
    procedure_name="PROCESS-CREDIT-CARD",
    file_path="payment.cbl",
    disharmony_score=0.65,
    disharmony_level="concerning",
    verbs=["READ", "WRITE"],
    fan_in=10
)

print(f"Risk Level: {risk.risk_level.value}")
print(f"Risk Score: {risk.risk_score}/100")
print(f"Tags: {', '.join(tag.value for tag in risk.compliance_tags)}")

# 5. Generate report
reporter = ComplianceReporter()
report = reporter.generate_report(
    assessments=[risk],
    report_type="compliance",
    generated_by="J. Smith"
)

# Export as HTML
reporter.export_html(report, "compliance_report.html")
```

---

## Compliance Frameworks

### SOX (Sarbanes-Oxley Act)

**Purpose:** Financial reporting integrity

**COBOL Relevance:**
- Financial report generation procedures
- General ledger updates
- Access control to financial data
- Audit trail requirements

**Auto-Detected Patterns:**
- Procedures with `REPORT`, `FINANCIAL`, `BALANCE`, `STATEMENT`, `GL`
- Access control: `AUTH`, `LOGIN`, `ACCESS`, `PERMISSION`
- Audit logging: `AUDIT`, `LOG`

**Requirements:**
- Documented change control
- Segregation of duties
- Audit trail of all changes

### PCI-DSS (Payment Card Industry Data Security Standard)

**Purpose:** Protect cardholder data

**COBOL Relevance:**
- Payment processing procedures
- Credit card data handling
- Encryption of sensitive data

**Auto-Detected Patterns:**
- Procedures with `CARD`, `PAYMENT`, `PAN`, `CVV`, `CHARGE`
- Encryption: `ENCRYPT`, `DECRYPT`, `HASH`, `SECURE`

**Requirements:**
- Encrypt cardholder data at rest and in transit
- Restrict access to cardholder data
- Maintain audit trails
- Data retention limits

### GDPR (General Data Protection Regulation)

**Purpose:** Protect personal data of EU residents

**COBOL Relevance:**
- Customer data processing
- Data deletion (right to erasure)
- Data export (right to portability)

**Auto-Detected Patterns:**
- Procedures with `CUSTOMER`, `PERSONAL`, `PROFILE`, `CONTACT`
- Data deletion: `DELETE-CUSTOMER`, `REMOVE-PERSONAL`, `PURGE`
- Data export: `EXPORT`, `EXTRACT`, `DOWNLOAD`

**Requirements:**
- Lawful basis for processing
- Data minimization
- Right to access, rectification, erasure
- Breach notification (72 hours)

### HIPAA (Health Insurance Portability and Accountability Act)

**Purpose:** Protect health information

**COBOL Relevance:**
- Patient data handling
- Medical records processing
- Healthcare claims

**Auto-Detected Patterns:**
- Procedures with `PATIENT`, `MEDICAL`, `HEALTH`, `CLAIM`
- PHI (Protected Health Information) handling

**Requirements:**
- Encryption of PHI
- Access controls and audit logs
- Breach notification

---

## Configuration

### Compliance Config File

Create `.compliance_config.json` in your project:

```json
{
  "enabled": true,
  "frameworks": ["sox", "pci_dss", "gdpr"],
  "audit_log_path": ".audit_log",
  "audit_retention_days": 365,
  "require_justification": true,
  "track_user": true,
  "track_environment": true,
  "auto_tag_critical": true,
  "critical_disharmony_threshold": 1.2,
  "high_disharmony_threshold": 0.8,
  "high_fan_in_threshold": 10,
  "notify_on_critical": true,
  "notify_on_baseline_deviation": true,
  "rules": [
    {
      "rule_id": "CUSTOM-001",
      "framework": "custom",
      "name": "Critical Business Procedures",
      "description": "Procedures that handle critical business logic",
      "severity": "high",
      "applies_to": ["*CALCULATE-INTEREST*", "*PROCESS-LOAN*"]
    }
  ]
}
```

### Loading Configuration

```python
from cobol_harmonizer.compliance import ComplianceConfig
import json

# Load from file
with open('.compliance_config.json') as f:
    config_data = json.load(f)

config = ComplianceConfig(**config_data)
```

---

## Usage Examples

### Example 1: Audit a Batch Analysis

```python
from cobol_harmonizer.compliance import AuditLogger, AuditEvent, AuditAction
from cobol_harmonizer.batch_analyzer import BatchAnalyzer

# Setup
logger = AuditLogger()
analyzer = BatchAnalyzer()

# Log start of batch
event = AuditEvent(AuditAction.BATCH_ANALYSIS)
event.with_details(directory="/cobol/src", files_count=50)
event.with_justification("Quarterly compliance review")
logger.log(event)

# Run batch analysis
results = analyzer.analyze_directory("/cobol/src", recursive=True)

# Log completion
event = AuditEvent(AuditAction.BATCH_ANALYSIS)
event.with_details(
    files_analyzed=len(results),
    critical_issues=sum(1 for r in results if r['severity'] == 'critical')
)
event.with_result("success")
logger.log(event)
```

### Example 2: Generate Compliance Report

```python
from cobol_harmonizer.compliance import (
    ComplianceRiskAssessor,
    ComplianceReporter,
    ComplianceTagger
)
from datetime import datetime, timedelta

# Analyze procedures
assessor = ComplianceRiskAssessor()
procedures_data = [
    {
        'procedure_name': 'PROCESS-PAYMENT',
        'file_path': 'payment.cbl',
        'disharmony_score': 0.65,
        'disharmony_level': 'concerning',
        'verbs': ['READ', 'WRITE', 'ENCRYPT'],
        'fan_in': 12
    },
    # ... more procedures
]

assessments = assessor.assess_batch(procedures_data)

# Generate report
reporter = ComplianceReporter()
report = reporter.generate_report(
    assessments=assessments,
    report_type="compliance",
    period_start=datetime.now() - timedelta(days=30),
    period_end=datetime.now(),
    generated_by="Compliance Team"
)

# Export in multiple formats
reporter.export_json(report, "compliance_report.json")
reporter.export_html(report, "compliance_report.html")
reporter.export_markdown(report, "compliance_report.md")
reporter.export_csv(report, "violations.csv")
```

### Example 3: Query Audit Log

```python
from cobol_harmonizer.compliance import AuditLogger, AuditAction
from datetime import datetime, timedelta

logger = AuditLogger()

# Get all analysis actions in the last 30 days
start_date = datetime.now() - timedelta(days=30)
entries = logger.query(
    start_date=start_date,
    action=AuditAction.ANALYSIS,
    limit=100
)

# Find who analyzed a specific file
file_history = logger.get_file_history("BANKACCT.cbl", limit=50)

for entry in file_history:
    print(f"{entry.timestamp}: {entry.user} - {entry.action.value}")
    print(f"  Details: {entry.details}")
    print()

# Get user activity
user_activity = logger.get_user_activity("jsmith", days=7)
```

### Example 4: Custom Compliance Rules

```python
from cobol_harmonizer.compliance import (
    ComplianceTagger,
    ComplianceRule,
    ComplianceFramework
)

# Define custom rules
rules = [
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
    )
]

# Use custom rules
tagger = ComplianceTagger(rules=rules)

# Check which rules apply
applicable_rules = tagger.get_applicable_rules("PROCESS-WIRE-TRANSFER")
for rule in applicable_rules:
    print(f"Rule: {rule.name} ({rule.severity})")
```

---

## Reports

### Report Types

1. **Compliance Report** - Focus on regulatory compliance
2. **Audit Trail Report** - Focus on who did what
3. **Risk Assessment Report** - Focus on risk scoring

### Sample Compliance Report (Markdown)

```markdown
# Compliance Audit Report

**Report ID:** AUDIT-20251108-103000-a1b2c3d4
**Generated:** 2025-11-08 10:30:00
**Period:** 2025-10-01 to 2025-11-08
**Generated By:** Compliance Team

## Executive Summary

- **Total Files Analyzed:** 45
- **Total Procedures Analyzed:** 312
- **Total Violations:** 8
  - Critical: 2
  - High: 6

## Compliance Framework Status

- **SOX:** ✅ Compliant
- **PCI-DSS:** ⚠️ Needs Review (6 high-risk issues)
- **GDPR:** ✅ Compliant

## Critical Violations (2)

### PROCESS-CREDIT-CARD-PAYMENT

- **File:** `payment_processing.cbl`
- **Rule:** Critical Risk Procedure (RISK-001)
- **Description:** Risk score: 82/100. Disharmony: 0.85. Compliance level: critical. Fan-in: 15.
- **Recommendation:**
  ```
  CRITICAL: Semantic disharmony score 0.85. Rename or refactor immediately.
  CRITICAL compliance procedure. Requires documented code review and approval before changes.
  PCI-DSS cardholder data: Ensure data is encrypted at rest and in transit.
  ```

## High-Risk Procedures (8)

### VALIDATE-CARD-NUMBER
- **Risk Level:** HIGH
- **Risk Score:** 68/100
- **Compliance Level:** critical
- **Impact:** 8 dependent procedures
- **Tags:** pci_cardholder_data, encryption

---

*This report was automatically generated by COBOL Code Harmonizer*
```

### Viewing HTML Reports

HTML reports provide a visual, executive-friendly view:
- Color-coded severity levels (red=critical, orange=high)
- Summary cards with key metrics
- Expandable violation details
- Framework status indicators
- Responsive design for mobile viewing

---

## Best Practices

### 1. Enable Audit Logging from Day One

```python
# In your analysis script
from cobol_harmonizer.compliance import get_audit_logger, AuditEvent, AuditAction

logger = get_audit_logger()

# Log every analysis
event = AuditEvent(AuditAction.ANALYSIS)
event.with_file(file_path)
event.with_justification("Reason for analysis")
logger.log(event)
```

### 2. Regular Compliance Reports

Generate monthly or quarterly compliance reports:

```bash
# Monthly compliance report
python -m cobol_harmonizer.cli compliance-report \
  --period 30d \
  --format html \
  --output monthly_compliance_report.html
```

### 3. Baseline Critical Procedures

For SOX/PCI-critical procedures, create baselines:

```bash
# Save baseline
python -m cobol_harmonizer.cli save-baseline ./cobol/src \
  --name "2025-Q4-baseline" \
  --description "Q4 2025 SOX compliance baseline"

# Compare monthly
python -m cobol_harmonizer.cli compare-baseline ./cobol/src \
  --baseline "2025-Q4-baseline" \
  --compliance-only
```

### 4. Pre-Commit Hooks for Critical Code

Prevent changes to critical procedures without review:

```python
# pre-commit hook
from cobol_harmonizer.compliance import ComplianceTagger

tagger = ComplianceTagger()
tags = tagger.tag_procedure(changed_procedure)

if ComplianceTag.SOX_FINANCIAL_REPORTING in tags:
    print("ERROR: SOX-critical procedure. Requires compliance review.")
    sys.exit(1)
```

### 5. Quarterly Audit Log Review

```python
# Review last quarter
from datetime import datetime, timedelta
from cobol_harmonizer.compliance import get_audit_logger

logger = get_audit_logger()
entries = logger.query(
    start_date=datetime.now() - timedelta(days=90)
)

# Analyze patterns
users = {}
for entry in entries:
    users[entry.user] = users.get(entry.user, 0) + 1

print("Activity by user:")
for user, count in sorted(users.items(), key=lambda x: x[1], reverse=True):
    print(f"  {user}: {count} actions")
```

### 6. Document Compliance Procedures

Create a `COMPLIANCE.md` in your repository:

```markdown
# Compliance Procedures

## SOX-Critical Code
The following procedures handle financial reporting and require:
- Dual approval for changes
- Quarterly code review
- Comprehensive testing before deployment

List of SOX procedures: [see compliance_tags.json]

## Change Control Process
1. Analyze code: `python -m cobol_harmonizer.cli analyze`
2. Check compliance tags
3. If SOX/PCI-tagged: Request compliance review
4. Document approval in audit log
5. Commit with justification
```

---

## API Reference

### AuditLogger

```python
class AuditLogger:
    def __init__(self, config: Optional[ComplianceConfig] = None)
    def log(self, event: AuditEvent) -> None
    def query(
        self,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        action: Optional[AuditAction] = None,
        user: Optional[str] = None,
        file_path: Optional[str] = None,
        limit: int = 1000
    ) -> List[AuditEntry]
    def get_file_history(self, file_path: str, limit: int = 50) -> List[AuditEntry]
    def get_user_activity(self, user: str, days: int = 30) -> List[AuditEntry]
    def cleanup_old_logs(self) -> int
```

### ComplianceTagger

```python
class ComplianceTagger:
    def __init__(self, rules: Optional[List[ComplianceRule]] = None)
    def tag_procedure(
        self,
        procedure_name: str,
        verbs: Optional[List[str]] = None,
        files_accessed: Optional[List[str]] = None
    ) -> Set[ComplianceTag]
    def get_compliance_level(self, tags: Set[ComplianceTag]) -> ComplianceLevel
    def get_applicable_rules(self, procedure_name: str) -> List[ComplianceRule]
```

### ComplianceRiskAssessor

```python
class ComplianceRiskAssessor:
    def __init__(self, tagger: Optional[ComplianceTagger] = None)
    def assess_procedure(
        self,
        procedure_name: str,
        file_path: str,
        disharmony_score: float,
        disharmony_level: str,
        verbs: Optional[List[str]] = None,
        fan_in: int = 0,
        fan_out: int = 0,
        max_depth: int = 0,
    ) -> RiskAssessment
    def assess_batch(self, procedures_data: List[Dict]) -> List[RiskAssessment]
    def get_critical_procedures(self, assessments: List[RiskAssessment]) -> List[RiskAssessment]
```

### ComplianceReporter

```python
class ComplianceReporter:
    def generate_report(
        self,
        assessments: List[RiskAssessment],
        audit_entries: Optional[List[AuditEntry]] = None,
        baseline_deviations: Optional[List[Dict]] = None,
        report_type: str = "compliance",
        period_start: Optional[datetime] = None,
        period_end: Optional[datetime] = None,
        generated_by: Optional[str] = None
    ) -> AuditReport
    def export_json(self, report: AuditReport, output_path: str) -> None
    def export_markdown(self, report: AuditReport, output_path: str) -> None
    def export_html(self, report: AuditReport, output_path: str) -> None
    def export_csv(self, report: AuditReport, output_path: str) -> None
```

---

## Conclusion

The compliance module provides enterprise-grade audit and compliance features that are:

✅ **Non-intrusive** - Optional, doesn't affect core analysis
✅ **Standards-based** - Supports SOX, PCI-DSS, GDPR, HIPAA, ISO 27001
✅ **Automatic** - Tags procedures based on patterns
✅ **Auditable** - Complete audit trail with user tracking
✅ **Reportable** - Multiple export formats for stakeholders

Perfect for financial institutions, healthcare organizations, and enterprises that need to demonstrate compliance with regulatory requirements.

---

**Made with 💛 for the COBOL community**
