"""
Compliance Data Models

Defines data structures for audit and compliance tracking.
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import List, Dict, Optional, Set
from enum import Enum


class ComplianceFramework(Enum):
    """Regulatory frameworks and standards"""
    SOX = "sox"  # Sarbanes-Oxley Act (financial reporting)
    PCI_DSS = "pci_dss"  # Payment Card Industry Data Security Standard
    GDPR = "gdpr"  # General Data Protection Regulation
    HIPAA = "hipaa"  # Health Insurance Portability and Accountability Act
    FISMA = "fisma"  # Federal Information Security Management Act
    ISO_27001 = "iso_27001"  # Information Security Management
    COBIT = "cobit"  # Control Objectives for Information Technologies
    NIST = "nist"  # National Institute of Standards and Technology
    CUSTOM = "custom"  # Organization-specific requirements


class AuditAction(Enum):
    """Types of auditable actions"""
    ANALYSIS = "analysis"  # Code analysis performed
    BASELINE_SAVE = "baseline_save"  # Baseline snapshot created
    BASELINE_COMPARE = "baseline_compare"  # Baseline comparison
    BATCH_ANALYSIS = "batch_analysis"  # Batch processing
    REPORT_GENERATED = "report_generated"  # Report created
    COMPLIANCE_TAG_ADDED = "compliance_tag_added"  # Compliance tag applied
    COMPLIANCE_TAG_REMOVED = "compliance_tag_removed"  # Compliance tag removed
    RISK_ASSESSMENT = "risk_assessment"  # Risk assessment performed
    CODE_REVIEW = "code_review"  # Code review conducted


@dataclass
class ComplianceRule:
    """A compliance rule that code must satisfy"""
    rule_id: str
    framework: ComplianceFramework
    name: str
    description: str
    severity: str  # 'critical', 'high', 'medium', 'low'
    check_function: Optional[str] = None  # Name of function to check compliance
    applies_to: List[str] = field(default_factory=list)  # Procedure name patterns

    def matches_procedure(self, procedure_name: str) -> bool:
        """Check if rule applies to a procedure"""
        if not self.applies_to:
            return False

        # Simple pattern matching (supports wildcards)
        for pattern in self.applies_to:
            if '*' in pattern:
                # Convert wildcard pattern to regex-like matching
                import re
                regex_pattern = pattern.replace('*', '.*')
                if re.match(regex_pattern, procedure_name):
                    return True
            elif pattern in procedure_name:
                return True

        return False


@dataclass
class ComplianceConfig:
    """Configuration for compliance features"""
    enabled: bool = True
    frameworks: List[ComplianceFramework] = field(default_factory=list)
    audit_log_path: str = ".audit_log"
    audit_retention_days: int = 365  # Keep audit logs for 1 year
    require_justification: bool = True  # Require reason for changes
    track_user: bool = True  # Track who performs actions
    track_environment: bool = True  # Track environment info
    auto_tag_critical: bool = True  # Auto-tag high-risk procedures
    rules: List[ComplianceRule] = field(default_factory=list)

    # Thresholds for automatic risk flagging
    critical_disharmony_threshold: float = 1.2
    high_disharmony_threshold: float = 0.8
    high_fan_in_threshold: int = 10  # Procedures called by many others

    # Notification settings
    notify_on_critical: bool = True
    notify_on_baseline_deviation: bool = True


@dataclass
class AuditEntry:
    """Single audit log entry"""
    timestamp: datetime
    action: AuditAction
    user: Optional[str]
    file_path: Optional[str]
    procedure_name: Optional[str]
    details: Dict[str, any]
    result: str  # 'success', 'failure', 'warning'
    justification: Optional[str] = None
    environment: Optional[Dict[str, str]] = None

    def to_dict(self) -> Dict:
        """Convert to dictionary for JSON serialization"""
        return {
            'timestamp': self.timestamp.isoformat(),
            'action': self.action.value,
            'user': self.user,
            'file_path': self.file_path,
            'procedure_name': self.procedure_name,
            'details': self.details,
            'result': self.result,
            'justification': self.justification,
            'environment': self.environment,
        }


@dataclass
class ComplianceViolation:
    """A compliance rule violation"""
    rule: ComplianceRule
    procedure_name: str
    file_path: str
    severity: str
    description: str
    recommendation: str
    evidence: Dict[str, any]


@dataclass
class AuditReport:
    """Comprehensive audit report for compliance"""
    report_id: str
    generated_at: datetime
    report_type: str  # 'compliance', 'audit_trail', 'risk_assessment'
    period_start: datetime
    period_end: datetime

    # Summary statistics
    total_files_analyzed: int = 0
    total_procedures_analyzed: int = 0
    total_violations: int = 0
    critical_violations: int = 0
    high_violations: int = 0

    # Detailed findings
    violations: List[ComplianceViolation] = field(default_factory=list)
    audit_entries: List[AuditEntry] = field(default_factory=list)

    # Compliance status by framework
    framework_status: Dict[ComplianceFramework, str] = field(default_factory=dict)

    # Change tracking
    baseline_deviations: List[Dict] = field(default_factory=list)
    high_risk_changes: List[Dict] = field(default_factory=list)

    # Metadata
    generated_by: Optional[str] = None
    attestation: Optional[str] = None  # Digital signature or attestation

    def to_dict(self) -> Dict:
        """Convert to dictionary for JSON serialization"""
        return {
            'report_id': self.report_id,
            'generated_at': self.generated_at.isoformat(),
            'report_type': self.report_type,
            'period_start': self.period_start.isoformat(),
            'period_end': self.period_end.isoformat(),
            'summary': {
                'total_files_analyzed': self.total_files_analyzed,
                'total_procedures_analyzed': self.total_procedures_analyzed,
                'total_violations': self.total_violations,
                'critical_violations': self.critical_violations,
                'high_violations': self.high_violations,
            },
            'violations': [
                {
                    'rule_id': v.rule.rule_id,
                    'framework': v.rule.framework.value,
                    'procedure_name': v.procedure_name,
                    'file_path': v.file_path,
                    'severity': v.severity,
                    'description': v.description,
                    'recommendation': v.recommendation,
                    'evidence': v.evidence,
                }
                for v in self.violations
            ],
            'audit_trail': [entry.to_dict() for entry in self.audit_entries],
            'framework_status': {
                f.value: status for f, status in self.framework_status.items()
            },
            'baseline_deviations': self.baseline_deviations,
            'high_risk_changes': self.high_risk_changes,
            'metadata': {
                'generated_by': self.generated_by,
                'attestation': self.attestation,
            }
        }
