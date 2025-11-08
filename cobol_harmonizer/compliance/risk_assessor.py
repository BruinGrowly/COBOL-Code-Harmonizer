"""
Compliance Risk Assessor

Assesses compliance risk by combining semantic disharmony analysis
with compliance tagging and call graph impact.
"""

from typing import Dict, List, Optional, Set
from enum import Enum
from dataclasses import dataclass

from .compliance_tagger import ComplianceTagger, ComplianceTag, ComplianceLevel


class RiskLevel(Enum):
    """Overall risk level"""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class RiskAssessment:
    """Risk assessment for a procedure"""

    procedure_name: str
    file_path: str

    # Semantic analysis
    disharmony_score: float
    disharmony_level: str

    # Compliance tagging
    compliance_tags: Set[ComplianceTag]
    compliance_level: ComplianceLevel

    # Call graph metrics
    fan_in: int = 0  # How many procedures call this one
    fan_out: int = 0  # How many procedures this one calls
    max_depth: int = 0  # Depth in call graph

    # Overall risk
    risk_level: RiskLevel = RiskLevel.LOW
    risk_score: float = 0.0  # 0-100

    # Recommendations
    recommendations: List[str] = None
    warnings: List[str] = None

    def __post_init__(self):
        if self.recommendations is None:
            self.recommendations = []
        if self.warnings is None:
            self.warnings = []


class ComplianceRiskAssessor:
    """
    Assesses compliance risk for COBOL procedures.

    Combines multiple risk factors:
    - Semantic disharmony (does code match its name?)
    - Compliance criticality (SOX, PCI-DSS, GDPR relevance)
    - Call graph impact (how many procedures depend on this?)
    - Change frequency (from audit logs)
    """

    def __init__(self, tagger: Optional[ComplianceTagger] = None):
        self.tagger = tagger or ComplianceTagger()

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
    ) -> RiskAssessment:
        """
        Assess compliance risk for a procedure.

        Args:
            procedure_name: Name of the procedure
            file_path: Path to file containing procedure
            disharmony_score: Semantic disharmony score
            disharmony_level: Disharmony classification
            verbs: COBOL verbs used in procedure
            fan_in: Number of callers
            fan_out: Number of callees
            max_depth: Depth in call graph

        Returns:
            RiskAssessment
        """
        # Get compliance tags
        tags = self.tagger.tag_procedure(procedure_name, verbs=verbs)
        compliance_level = self.tagger.get_compliance_level(tags)

        # Calculate risk score (0-100)
        risk_score = self._calculate_risk_score(
            disharmony_score=disharmony_score,
            compliance_level=compliance_level,
            fan_in=fan_in,
            has_tags=len(tags) > 0,
        )

        # Determine overall risk level
        risk_level = self._classify_risk(risk_score)

        # Create assessment
        assessment = RiskAssessment(
            procedure_name=procedure_name,
            file_path=file_path,
            disharmony_score=disharmony_score,
            disharmony_level=disharmony_level,
            compliance_tags=tags,
            compliance_level=compliance_level,
            fan_in=fan_in,
            fan_out=fan_out,
            max_depth=max_depth,
            risk_level=risk_level,
            risk_score=risk_score,
        )

        # Generate recommendations and warnings
        self._add_recommendations(assessment)
        self._add_warnings(assessment)

        return assessment

    def _calculate_risk_score(
        self,
        disharmony_score: float,
        compliance_level: ComplianceLevel,
        fan_in: int,
        has_tags: bool,
    ) -> float:
        """
        Calculate overall risk score (0-100).

        Formula:
        - 40% semantic disharmony
        - 30% compliance criticality
        - 20% call graph impact
        - 10% tagged vs untagged

        Returns:
            Risk score 0-100
        """
        # Disharmony component (0-40 points)
        # Map disharmony score (0-1.5) to 0-40
        disharmony_component = min(40, (disharmony_score / 1.5) * 40)

        # Compliance component (0-30 points)
        compliance_points = {
            ComplianceLevel.CRITICAL: 30,
            ComplianceLevel.HIGH: 22,
            ComplianceLevel.MEDIUM: 15,
            ComplianceLevel.LOW: 5,
        }
        compliance_component = compliance_points.get(compliance_level, 0)

        # Impact component (0-20 points)
        # High fan-in means many procedures depend on this
        impact_component = min(20, (fan_in / 20) * 20)

        # Tagged component (0-10 points)
        # Procedures with compliance tags need extra scrutiny
        tagged_component = 10 if has_tags else 0

        total = disharmony_component + compliance_component + impact_component + tagged_component

        return round(total, 1)

    def _classify_risk(self, risk_score: float) -> RiskLevel:
        """Classify risk score into risk level"""
        if risk_score >= 75:
            return RiskLevel.CRITICAL
        elif risk_score >= 50:
            return RiskLevel.HIGH
        elif risk_score >= 25:
            return RiskLevel.MEDIUM
        else:
            return RiskLevel.LOW

    def _add_recommendations(self, assessment: RiskAssessment) -> None:
        """Add recommendations based on assessment"""
        recs = assessment.recommendations

        # Disharmony recommendations
        if assessment.disharmony_score > 1.2:
            recs.append(
                f"CRITICAL: Semantic disharmony score {assessment.disharmony_score:.2f}. "
                f"Rename or refactor immediately."
            )
        elif assessment.disharmony_score > 0.8:
            recs.append(
                f"HIGH: Semantic disharmony score {assessment.disharmony_score:.2f}. "
                f"Consider renaming to match implementation."
            )
        elif assessment.disharmony_score > 0.5:
            recs.append(
                f"MEDIUM: Semantic drift detected ({assessment.disharmony_score:.2f}). "
                f"Review procedure name for clarity."
            )

        # Compliance recommendations
        if assessment.compliance_level == ComplianceLevel.CRITICAL:
            recs.append(
                "CRITICAL compliance procedure. Requires documented code review "
                "and approval before changes."
            )
            recs.append("Consider adding comprehensive audit logging to this procedure.")

        if assessment.compliance_level in [ComplianceLevel.CRITICAL, ComplianceLevel.HIGH]:
            recs.append("Ensure this procedure has comprehensive unit tests and integration tests.")

        # Impact recommendations
        if assessment.fan_in > 10:
            recs.append(
                f"HIGH IMPACT: Called by {assessment.fan_in} other procedures. "
                f"Changes here ripple widely through the system."
            )

        # Tag-specific recommendations
        if ComplianceTag.AUTHENTICATION in assessment.compliance_tags:
            recs.append(
                "Authentication procedure: Ensure multi-factor authentication is supported "
                "and credentials are never logged."
            )

        if ComplianceTag.ENCRYPTION in assessment.compliance_tags:
            recs.append(
                "Encryption procedure: Verify use of approved encryption algorithms "
                "(AES-256 or equivalent)."
            )

        if ComplianceTag.DATA_DELETION in assessment.compliance_tags:
            recs.append(
                "Data deletion procedure: Ensure deletion is irreversible and audit-logged."
            )

        if ComplianceTag.SOX_FINANCIAL_REPORTING in assessment.compliance_tags:
            recs.append(
                "SOX-relevant financial reporting: Ensure proper authorization controls "
                "and change management."
            )

        if ComplianceTag.PCI_CARDHOLDER_DATA in assessment.compliance_tags:
            recs.append(
                "PCI-DSS cardholder data: Ensure data is encrypted at rest and in transit. "
                "Limit data retention to business necessity."
            )

        if ComplianceTag.GDPR_PERSONAL_DATA in assessment.compliance_tags:
            recs.append(
                "GDPR personal data: Ensure proper consent tracking and data minimization. "
                "Support right to access and right to erasure."
            )

    def _add_warnings(self, assessment: RiskAssessment) -> None:
        """Add warnings based on assessment"""
        warnings = assessment.warnings

        # Critical disharmony + critical compliance = highest risk
        if (
            assessment.disharmony_score > 0.8
            and assessment.compliance_level == ComplianceLevel.CRITICAL
        ):
            warnings.append(
                "⚠️  CRITICAL: High semantic disharmony in compliance-critical procedure. "
                "This represents significant regulatory risk."
            )

        # High impact + high disharmony
        if assessment.fan_in > 10 and assessment.disharmony_score > 0.5:
            warnings.append(
                "⚠️  HIGH: Widely-used procedure with semantic drift. "
                "Many procedures may be misunderstanding this procedure's actual behavior."
            )

        # PCI + data deletion = special concern
        if (
            ComplianceTag.PCI_CARDHOLDER_DATA in assessment.compliance_tags
            and ComplianceTag.DATA_DELETION in assessment.compliance_tags
        ):
            warnings.append(
                "⚠️  PCI-DSS: Cardholder data deletion must follow secure deletion standards. "
                "Verify compliance with PCI-DSS requirement 3.1."
            )

        # SOX + modification = audit concern
        if (
            ComplianceTag.SOX_FINANCIAL_REPORTING in assessment.compliance_tags
            and ComplianceTag.DATA_MODIFICATION in assessment.compliance_tags
        ):
            warnings.append(
                "⚠️  SOX: Financial data modification requires segregation of duties "
                "and comprehensive audit trail."
            )

    def assess_batch(self, procedures_data: List[Dict]) -> List[RiskAssessment]:
        """
        Assess multiple procedures.

        Args:
            procedures_data: List of dicts with procedure info

        Returns:
            List of risk assessments
        """
        assessments = []

        for data in procedures_data:
            assessment = self.assess_procedure(
                procedure_name=data.get("procedure_name"),
                file_path=data.get("file_path"),
                disharmony_score=data.get("disharmony_score", 0.0),
                disharmony_level=data.get("disharmony_level", "harmonious"),
                verbs=data.get("verbs"),
                fan_in=data.get("fan_in", 0),
                fan_out=data.get("fan_out", 0),
                max_depth=data.get("max_depth", 0),
            )
            assessments.append(assessment)

        return assessments

    def get_critical_procedures(self, assessments: List[RiskAssessment]) -> List[RiskAssessment]:
        """
        Get procedures with critical risk.

        Args:
            assessments: List of risk assessments

        Returns:
            Filtered list of critical risk procedures
        """
        return [a for a in assessments if a.risk_level == RiskLevel.CRITICAL]

    def get_compliance_summary(self, assessments: List[RiskAssessment]) -> Dict:
        """
        Generate summary of compliance status.

        Args:
            assessments: List of risk assessments

        Returns:
            Summary dictionary
        """
        total = len(assessments)
        risk_counts = {
            RiskLevel.CRITICAL: 0,
            RiskLevel.HIGH: 0,
            RiskLevel.MEDIUM: 0,
            RiskLevel.LOW: 0,
        }

        compliance_counts = {
            ComplianceLevel.CRITICAL: 0,
            ComplianceLevel.HIGH: 0,
            ComplianceLevel.MEDIUM: 0,
            ComplianceLevel.LOW: 0,
        }

        all_tags = set()

        for assessment in assessments:
            risk_counts[assessment.risk_level] += 1
            compliance_counts[assessment.compliance_level] += 1
            all_tags.update(assessment.compliance_tags)

        return {
            "total_procedures": total,
            "risk_distribution": {level.value: count for level, count in risk_counts.items()},
            "compliance_distribution": {
                level.value: count for level, count in compliance_counts.items()
            },
            "unique_compliance_tags": len(all_tags),
            "critical_risk_percentage": (
                (risk_counts[RiskLevel.CRITICAL] / total * 100) if total > 0 else 0
            ),
            "high_risk_percentage": (risk_counts[RiskLevel.HIGH] / total * 100) if total > 0 else 0,
        }
