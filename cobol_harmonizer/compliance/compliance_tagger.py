"""
Compliance Tagger

Identifies and tags COBOL procedures that are relevant for compliance/regulatory purposes.
"""

import re
from typing import List, Set, Dict, Optional
from enum import Enum
from pathlib import Path
import json

from .models import ComplianceFramework, ComplianceRule


class ComplianceTag(Enum):
    """Compliance tags for procedures"""

    # Financial regulations
    SOX_FINANCIAL_REPORTING = "sox_financial_reporting"
    SOX_ACCESS_CONTROL = "sox_access_control"
    SOX_AUDIT_TRAIL = "sox_audit_trail"

    # Payment security
    PCI_CARDHOLDER_DATA = "pci_cardholder_data"
    PCI_ENCRYPTION = "pci_encryption"
    PCI_ACCESS_CONTROL = "pci_access_control"

    # Data protection
    GDPR_PERSONAL_DATA = "gdpr_personal_data"
    GDPR_DATA_DELETION = "gdpr_data_deletion"
    GDPR_DATA_EXPORT = "gdpr_data_export"

    # Healthcare
    HIPAA_PHI = "hipaa_phi"  # Protected Health Information
    HIPAA_ENCRYPTION = "hipaa_encryption"

    # Security
    AUTHENTICATION = "authentication"
    AUTHORIZATION = "authorization"
    ENCRYPTION = "encryption"
    AUDIT_LOGGING = "audit_logging"

    # Data operations
    DATA_DELETION = "data_deletion"
    DATA_MODIFICATION = "data_modification"
    DATA_EXPORT = "data_export"
    DATA_IMPORT = "data_import"

    # Critical business functions
    FINANCIAL_TRANSACTION = "financial_transaction"
    CUSTOMER_DATA = "customer_data"
    PAYMENT_PROCESSING = "payment_processing"


class ComplianceLevel(Enum):
    """Compliance criticality level"""

    CRITICAL = "critical"  # SOX, PCI, highly regulated
    HIGH = "high"  # Important compliance requirements
    MEDIUM = "medium"  # Standard compliance
    LOW = "low"  # Informational


class ComplianceTagger:
    """
    Identifies procedures that require compliance tracking.

    Uses pattern matching and heuristics to tag procedures based on:
    - Procedure names
    - COBOL verbs used
    - Data files accessed
    - Called programs
    """

    def __init__(self, rules: Optional[List[ComplianceRule]] = None):
        self.rules = rules or self._get_default_rules()
        self._tag_cache: Dict[str, Set[ComplianceTag]] = {}

    def _get_default_rules(self) -> List[ComplianceRule]:
        """Get default compliance rules"""
        return [
            # Financial reporting (SOX)
            ComplianceRule(
                rule_id="SOX-001",
                framework=ComplianceFramework.SOX,
                name="Financial Reporting Procedures",
                description="Procedures that generate financial reports",
                severity="critical",
                applies_to=["*REPORT*", "*FINANCIAL*", "*BALANCE*", "*STATEMENT*", "*GL*"],
            ),
            ComplianceRule(
                rule_id="SOX-002",
                framework=ComplianceFramework.SOX,
                name="Access Control",
                description="Procedures that control access to financial data",
                severity="critical",
                applies_to=["*AUTH*", "*LOGIN*", "*ACCESS*", "*PERMISSION*"],
            ),
            # Payment processing (PCI-DSS)
            ComplianceRule(
                rule_id="PCI-001",
                framework=ComplianceFramework.PCI_DSS,
                name="Cardholder Data Handling",
                description="Procedures that process credit card data",
                severity="critical",
                applies_to=["*CARD*", "*PAYMENT*", "*PAN*", "*CVV*", "*CHARGE*"],
            ),
            ComplianceRule(
                rule_id="PCI-002",
                framework=ComplianceFramework.PCI_DSS,
                name="Encryption Requirements",
                description="Procedures that encrypt sensitive data",
                severity="high",
                applies_to=["*ENCRYPT*", "*DECRYPT*", "*HASH*", "*SECURE*"],
            ),
            # Data protection (GDPR)
            ComplianceRule(
                rule_id="GDPR-001",
                framework=ComplianceFramework.GDPR,
                name="Personal Data Processing",
                description="Procedures that handle personal data",
                severity="high",
                applies_to=["*CUSTOMER*", "*PERSONAL*", "*PROFILE*", "*CONTACT*"],
            ),
            ComplianceRule(
                rule_id="GDPR-002",
                framework=ComplianceFramework.GDPR,
                name="Right to Erasure",
                description="Procedures that delete personal data",
                severity="high",
                applies_to=["*DELETE-CUSTOMER*", "*REMOVE-PERSONAL*", "*PURGE*"],
            ),
            # General security
            ComplianceRule(
                rule_id="SEC-001",
                framework=ComplianceFramework.ISO_27001,
                name="Authentication",
                description="Procedures that authenticate users",
                severity="critical",
                applies_to=["*LOGIN*", "*AUTH*", "*VALIDATE-USER*", "*PASSWORD*"],
            ),
            ComplianceRule(
                rule_id="SEC-002",
                framework=ComplianceFramework.ISO_27001,
                name="Data Modification",
                description="Procedures that modify critical data",
                severity="high",
                applies_to=["*UPDATE*", "*MODIFY*", "*DELETE*", "*WRITE*"],
            ),
        ]

    def tag_procedure(
        self,
        procedure_name: str,
        verbs: Optional[List[str]] = None,
        files_accessed: Optional[List[str]] = None,
    ) -> Set[ComplianceTag]:
        """
        Tag a procedure based on its characteristics.

        Args:
            procedure_name: Name of the procedure
            verbs: COBOL verbs used in the procedure
            files_accessed: Files accessed by the procedure

        Returns:
            Set of applicable compliance tags
        """
        # Check cache
        cache_key = f"{procedure_name}|{verbs}|{files_accessed}"
        if cache_key in self._tag_cache:
            return self._tag_cache[cache_key]

        tags = set()

        # Tag based on procedure name patterns
        tags.update(self._tag_by_name(procedure_name))

        # Tag based on COBOL verbs
        if verbs:
            tags.update(self._tag_by_verbs(verbs))

        # Tag based on files accessed
        if files_accessed:
            tags.update(self._tag_by_files(files_accessed))

        # Cache result
        self._tag_cache[cache_key] = tags

        return tags

    def _tag_by_name(self, procedure_name: str) -> Set[ComplianceTag]:
        """Tag based on procedure name"""
        tags = set()
        name_upper = procedure_name.upper()

        # Financial patterns
        if any(
            kw in name_upper
            for kw in ["REPORT", "FINANCIAL", "BALANCE", "STATEMENT", "GL", "LEDGER"]
        ):
            tags.add(ComplianceTag.SOX_FINANCIAL_REPORTING)

        if any(kw in name_upper for kw in ["TRANSACTION", "DEBIT", "CREDIT", "TRANSFER"]):
            tags.add(ComplianceTag.FINANCIAL_TRANSACTION)

        # Payment patterns
        if any(kw in name_upper for kw in ["CARD", "PAYMENT", "PAN", "CVV", "CHARGE"]):
            tags.add(ComplianceTag.PCI_CARDHOLDER_DATA)
            tags.add(ComplianceTag.PAYMENT_PROCESSING)

        # Security patterns
        if any(kw in name_upper for kw in ["LOGIN", "AUTH", "PASSWORD", "CREDENTIAL"]):
            tags.add(ComplianceTag.AUTHENTICATION)
            tags.add(ComplianceTag.SOX_ACCESS_CONTROL)

        if any(kw in name_upper for kw in ["ENCRYPT", "DECRYPT", "HASH", "SECURE"]):
            tags.add(ComplianceTag.ENCRYPTION)
            tags.add(ComplianceTag.PCI_ENCRYPTION)

        if any(kw in name_upper for kw in ["AUDIT", "LOG"]):
            tags.add(ComplianceTag.AUDIT_LOGGING)
            tags.add(ComplianceTag.SOX_AUDIT_TRAIL)

        # Data protection patterns
        if any(kw in name_upper for kw in ["CUSTOMER", "PERSONAL", "PROFILE", "CONTACT"]):
            tags.add(ComplianceTag.CUSTOMER_DATA)
            tags.add(ComplianceTag.GDPR_PERSONAL_DATA)

        # Data operations
        if any(kw in name_upper for kw in ["DELETE", "REMOVE", "PURGE"]):
            tags.add(ComplianceTag.DATA_DELETION)
            if "CUSTOMER" in name_upper or "PERSONAL" in name_upper:
                tags.add(ComplianceTag.GDPR_DATA_DELETION)

        if any(kw in name_upper for kw in ["EXPORT", "EXTRACT", "DOWNLOAD"]):
            tags.add(ComplianceTag.DATA_EXPORT)
            if "CUSTOMER" in name_upper or "PERSONAL" in name_upper:
                tags.add(ComplianceTag.GDPR_DATA_EXPORT)

        if any(kw in name_upper for kw in ["UPDATE", "MODIFY", "CHANGE"]):
            tags.add(ComplianceTag.DATA_MODIFICATION)

        return tags

    def _tag_by_verbs(self, verbs: List[str]) -> Set[ComplianceTag]:
        """Tag based on COBOL verbs used"""
        tags = set()
        verbs_upper = [v.upper() for v in verbs]

        # Data deletion operations
        if "DELETE" in verbs_upper:
            tags.add(ComplianceTag.DATA_DELETION)

        # Data modification operations
        if any(v in verbs_upper for v in ["WRITE", "REWRITE", "UPDATE"]):
            tags.add(ComplianceTag.DATA_MODIFICATION)

        # Audit trail operations
        if any(v in verbs_upper for v in ["DISPLAY", "WRITE"]) and "AUDIT" in " ".join(verbs_upper):
            tags.add(ComplianceTag.AUDIT_LOGGING)

        return tags

    def _tag_by_files(self, files: List[str]) -> Set[ComplianceTag]:
        """Tag based on files accessed"""
        tags = set()

        for file_name in files:
            file_upper = file_name.upper()

            # Customer data files
            if any(kw in file_upper for kw in ["CUSTOMER", "CLIENT", "ACCOUNT"]):
                tags.add(ComplianceTag.CUSTOMER_DATA)
                tags.add(ComplianceTag.GDPR_PERSONAL_DATA)

            # Financial files
            if any(kw in file_upper for kw in ["TRANSACTION", "LEDGER", "GL", "BALANCE"]):
                tags.add(ComplianceTag.FINANCIAL_TRANSACTION)
                tags.add(ComplianceTag.SOX_FINANCIAL_REPORTING)

            # Payment files
            if any(kw in file_upper for kw in ["CARD", "PAYMENT", "CHARGE"]):
                tags.add(ComplianceTag.PAYMENT_PROCESSING)
                tags.add(ComplianceTag.PCI_CARDHOLDER_DATA)

            # Audit files
            if "AUDIT" in file_upper or "LOG" in file_upper:
                tags.add(ComplianceTag.AUDIT_LOGGING)

        return tags

    def get_compliance_level(self, tags: Set[ComplianceTag]) -> ComplianceLevel:
        """
        Determine overall compliance level based on tags.

        Args:
            tags: Set of compliance tags

        Returns:
            ComplianceLevel
        """
        # Critical if SOX or PCI tags present
        critical_tags = {
            ComplianceTag.SOX_FINANCIAL_REPORTING,
            ComplianceTag.SOX_ACCESS_CONTROL,
            ComplianceTag.PCI_CARDHOLDER_DATA,
            ComplianceTag.AUTHENTICATION,
        }

        if tags & critical_tags:
            return ComplianceLevel.CRITICAL

        # High if security or GDPR tags
        high_tags = {
            ComplianceTag.ENCRYPTION,
            ComplianceTag.GDPR_PERSONAL_DATA,
            ComplianceTag.GDPR_DATA_DELETION,
            ComplianceTag.PAYMENT_PROCESSING,
            ComplianceTag.AUDIT_LOGGING,
        }

        if tags & high_tags:
            return ComplianceLevel.HIGH

        # Medium if any data operation tags
        medium_tags = {
            ComplianceTag.DATA_DELETION,
            ComplianceTag.DATA_MODIFICATION,
            ComplianceTag.DATA_EXPORT,
            ComplianceTag.CUSTOMER_DATA,
        }

        if tags & medium_tags:
            return ComplianceLevel.MEDIUM

        # Low otherwise
        return ComplianceLevel.LOW

    def get_applicable_rules(self, procedure_name: str) -> List[ComplianceRule]:
        """
        Get compliance rules that apply to a procedure.

        Args:
            procedure_name: Name of the procedure

        Returns:
            List of applicable rules
        """
        applicable = []

        for rule in self.rules:
            if rule.matches_procedure(procedure_name):
                applicable.append(rule)

        return applicable

    def save_tags(self, file_path: str, tags_by_procedure: Dict[str, Set[ComplianceTag]]) -> None:
        """
        Save compliance tags to a file.

        Args:
            file_path: Path to save tags file
            tags_by_procedure: Dictionary mapping procedure names to tags
        """
        data = {
            "generated_at": str(Path(file_path).stat().st_mtime),
            "procedures": {
                proc: [tag.value for tag in tags] for proc, tags in tags_by_procedure.items()
            },
        }

        with open(file_path, "w") as f:
            json.dump(data, f, indent=2)

    def load_tags(self, file_path: str) -> Dict[str, Set[ComplianceTag]]:
        """
        Load compliance tags from a file.

        Args:
            file_path: Path to tags file

        Returns:
            Dictionary mapping procedure names to tags
        """
        with open(file_path, "r") as f:
            data = json.load(f)

        tags_by_procedure = {}
        for proc, tag_values in data.get("procedures", {}).items():
            tags_by_procedure[proc] = {ComplianceTag(tag_val) for tag_val in tag_values}

        return tags_by_procedure
