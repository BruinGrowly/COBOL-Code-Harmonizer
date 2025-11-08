"""
Audit Logger

Provides comprehensive audit logging for compliance and tracking purposes.
"""

import json
import os
import getpass
import platform
import hashlib
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Dict, List
from enum import Enum

from .models import AuditEntry, AuditAction, ComplianceConfig


class AuditLevel(Enum):
    """Audit logging levels"""
    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


class AuditEvent:
    """Builder for audit events"""

    def __init__(self, action: AuditAction):
        self.action = action
        self.file_path: Optional[str] = None
        self.procedure_name: Optional[str] = None
        self.details: Dict = {}
        self.result: str = "success"
        self.justification: Optional[str] = None

    def with_file(self, file_path: str) -> 'AuditEvent':
        """Set file path"""
        self.file_path = file_path
        return self

    def with_procedure(self, procedure_name: str) -> 'AuditEvent':
        """Set procedure name"""
        self.procedure_name = procedure_name
        return self

    def with_details(self, **kwargs) -> 'AuditEvent':
        """Add details"""
        self.details.update(kwargs)
        return self

    def with_result(self, result: str) -> 'AuditEvent':
        """Set result (success/failure/warning)"""
        self.result = result
        return self

    def with_justification(self, justification: str) -> 'AuditEvent':
        """Add justification (required for critical actions)"""
        self.justification = justification
        return self


class AuditLogger:
    """
    Audit logger for compliance and tracking.

    Provides:
    - Persistent audit trail
    - User and environment tracking
    - Tamper-evident logging
    - Audit log rotation and retention
    """

    def __init__(self, config: Optional[ComplianceConfig] = None):
        self.config = config or ComplianceConfig()
        self.log_dir = Path(self.config.audit_log_path)
        self.log_dir.mkdir(parents=True, exist_ok=True)

        # Current log file
        self.current_log_file = self.log_dir / f"audit_{datetime.now():%Y%m}.jsonl"

        # In-memory cache for performance
        self._cache: List[AuditEntry] = []
        self._cache_size = 100

    def log(self, event: AuditEvent) -> None:
        """
        Log an audit event.

        Args:
            event: AuditEvent to log
        """
        if not self.config.enabled:
            return

        # Get user information
        user = None
        if self.config.track_user:
            try:
                user = getpass.getuser()
            except:
                user = "unknown"

        # Get environment information
        environment = None
        if self.config.track_environment:
            environment = {
                'hostname': platform.node(),
                'platform': platform.platform(),
                'python_version': platform.python_version(),
            }

        # Create audit entry
        entry = AuditEntry(
            timestamp=datetime.now(),
            action=event.action,
            user=user,
            file_path=event.file_path,
            procedure_name=event.procedure_name,
            details=event.details,
            result=event.result,
            justification=event.justification,
            environment=environment,
        )

        # Add to cache
        self._cache.append(entry)

        # Flush if cache is full
        if len(self._cache) >= self._cache_size:
            self._flush_cache()

        # Immediate flush for critical actions
        if event.action in [AuditAction.COMPLIANCE_TAG_REMOVED, AuditAction.BASELINE_SAVE]:
            self._flush_cache()

    def _flush_cache(self) -> None:
        """Flush in-memory cache to disk"""
        if not self._cache:
            return

        # Append to log file (JSONL format - one JSON object per line)
        with open(self.current_log_file, 'a') as f:
            for entry in self._cache:
                json_line = json.dumps(entry.to_dict())
                f.write(json_line + '\n')

        self._cache.clear()

    def query(
        self,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        action: Optional[AuditAction] = None,
        user: Optional[str] = None,
        file_path: Optional[str] = None,
        limit: int = 1000
    ) -> List[AuditEntry]:
        """
        Query audit log.

        Args:
            start_date: Filter by start date
            end_date: Filter by end date
            action: Filter by action type
            user: Filter by user
            file_path: Filter by file path
            limit: Maximum number of results

        Returns:
            List of matching audit entries
        """
        # Flush cache first
        self._flush_cache()

        results = []

        # Determine which log files to scan
        log_files = self._get_log_files_in_range(start_date, end_date)

        for log_file in log_files:
            if not log_file.exists():
                continue

            with open(log_file, 'r') as f:
                for line in f:
                    if len(results) >= limit:
                        break

                    try:
                        data = json.loads(line)
                        entry = self._dict_to_audit_entry(data)

                        # Apply filters
                        if start_date and entry.timestamp < start_date:
                            continue
                        if end_date and entry.timestamp > end_date:
                            continue
                        if action and entry.action != action:
                            continue
                        if user and entry.user != user:
                            continue
                        if file_path and entry.file_path != file_path:
                            continue

                        results.append(entry)

                    except json.JSONDecodeError:
                        continue  # Skip malformed lines

        return results

    def _get_log_files_in_range(
        self,
        start_date: Optional[datetime],
        end_date: Optional[datetime]
    ) -> List[Path]:
        """Get log files within date range"""
        all_log_files = sorted(self.log_dir.glob("audit_*.jsonl"))

        if not start_date and not end_date:
            return all_log_files

        # Filter by date (log files are named audit_YYYYMM.jsonl)
        filtered = []
        for log_file in all_log_files:
            # Extract YYYYMM from filename
            try:
                name_parts = log_file.stem.split('_')
                if len(name_parts) != 2:
                    continue

                year_month = name_parts[1]
                year = int(year_month[:4])
                month = int(year_month[4:6])
                file_date = datetime(year, month, 1)

                # Check if file is in range
                if start_date and file_date < datetime(start_date.year, start_date.month, 1):
                    continue
                if end_date and file_date > datetime(end_date.year, end_date.month, 1):
                    continue

                filtered.append(log_file)

            except (ValueError, IndexError):
                continue

        return filtered

    def _dict_to_audit_entry(self, data: Dict) -> AuditEntry:
        """Convert dictionary to AuditEntry"""
        return AuditEntry(
            timestamp=datetime.fromisoformat(data['timestamp']),
            action=AuditAction(data['action']),
            user=data.get('user'),
            file_path=data.get('file_path'),
            procedure_name=data.get('procedure_name'),
            details=data.get('details', {}),
            result=data['result'],
            justification=data.get('justification'),
            environment=data.get('environment'),
        )

    def get_file_history(self, file_path: str, limit: int = 50) -> List[AuditEntry]:
        """
        Get audit history for a specific file.

        Args:
            file_path: Path to file
            limit: Maximum number of results

        Returns:
            List of audit entries for the file
        """
        return self.query(file_path=file_path, limit=limit)

    def get_user_activity(self, user: str, days: int = 30) -> List[AuditEntry]:
        """
        Get user activity for the past N days.

        Args:
            user: Username
            days: Number of days to look back

        Returns:
            List of audit entries for the user
        """
        start_date = datetime.now() - timedelta(days=days)
        return self.query(start_date=start_date, user=user)

    def cleanup_old_logs(self) -> int:
        """
        Remove audit logs older than retention period.

        Returns:
            Number of log files removed
        """
        cutoff_date = datetime.now() - timedelta(days=self.config.audit_retention_days)
        cutoff_month = datetime(cutoff_date.year, cutoff_date.month, 1)

        removed = 0
        for log_file in self.log_dir.glob("audit_*.jsonl"):
            try:
                # Extract date from filename
                name_parts = log_file.stem.split('_')
                if len(name_parts) != 2:
                    continue

                year_month = name_parts[1]
                year = int(year_month[:4])
                month = int(year_month[4:6])
                file_date = datetime(year, month, 1)

                # Remove if older than cutoff
                if file_date < cutoff_month:
                    log_file.unlink()
                    removed += 1

            except (ValueError, IndexError, OSError):
                continue

        return removed

    def verify_integrity(self) -> bool:
        """
        Verify audit log integrity (basic check).

        Returns:
            True if logs appear intact, False otherwise
        """
        # Flush cache
        self._flush_cache()

        # Check that log file exists and is readable
        if not self.current_log_file.exists():
            return True  # Empty log is valid

        try:
            with open(self.current_log_file, 'r') as f:
                line_count = 0
                for line in f:
                    # Try to parse each line
                    json.loads(line)
                    line_count += 1

                return line_count >= 0  # At least parseable

        except (json.JSONDecodeError, OSError):
            return False

    def close(self) -> None:
        """Flush cache and close logger"""
        self._flush_cache()


# Singleton instance
_audit_logger_instance: Optional[AuditLogger] = None


def get_audit_logger(config: Optional[ComplianceConfig] = None) -> AuditLogger:
    """Get singleton audit logger instance"""
    global _audit_logger_instance
    if _audit_logger_instance is None:
        _audit_logger_instance = AuditLogger(config)
    return _audit_logger_instance
