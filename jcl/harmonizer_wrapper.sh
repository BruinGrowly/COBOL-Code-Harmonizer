#!/bin/sh
#
# COBOL Code Harmonizer - JCL Wrapper Script
#
# Purpose: Wrapper script called by JCL to run COBOL Harmonizer
#
# Environment Variables (set by JCL STDENV):
#   HARMONIZER_HOME          - Installation directory
#   HARMONIZER_COPYBOOK_PATH - Copybook search path
#   HARMONIZER_CACHE_DIR     - Cache directory
#   HARMONIZER_LOG_DIR       - Log directory
#   HARMONIZER_COMPLIANCE    - Compliance frameworks (comma-separated)
#   HARMONIZER_AUDIT_LOG     - Audit log directory
#
# DD Names (from JCL):
#   COBOLSRC - Input COBOL source directory
#   COPYLIB  - Copybook library
#   REPORT   - Output report file
#   CONFIG   - Configuration JSON
#
# Author: COBOL Code Harmonizer Team
# Version: 0.5.0
# Date: 2025-11-08
#

set -e  # Exit on error
set -u  # Exit on undefined variable

#-----------------------------------------------------------------------
# Configuration
#-----------------------------------------------------------------------

SCRIPT_DIR="$(dirname "$0")"
HARMONIZER_HOME="${HARMONIZER_HOME:-/u/cobol-harmonizer}"
PYTHON="${PYTHON:-python3}"

# DD names (z/OS specific)
COBOLSRC_PATH="${COBOLSRC_PATH:-/u/prod/cobol}"
COPYLIB_PATH="${COPYLIB_PATH:-/u/prod/copybook}"
REPORT_PATH="${REPORT_PATH:-/u/reports/harmonizer_report.json}"
CONFIG_PATH="${CONFIG_PATH:-/tmp/harmonizer_config.json}"

# Read configuration from stdin if CONFIG DD is allocated
if [ -n "${CONFIG:-}" ]; then
    cat > "$CONFIG_PATH"
fi

#-----------------------------------------------------------------------
# Logging
#-----------------------------------------------------------------------

LOG_DIR="${HARMONIZER_LOG_DIR:-$HARMONIZER_HOME/logs}"
mkdir -p "$LOG_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="$LOG_DIR/harmonizer_${TIMESTAMP}.log"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG_FILE"
}

error() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $*" | tee -a "$LOG_FILE" >&2
}

#-----------------------------------------------------------------------
# Environment Validation
#-----------------------------------------------------------------------

log "COBOL Code Harmonizer - Batch Analysis"
log "========================================"
log ""
log "Environment:"
log "  HARMONIZER_HOME: $HARMONIZER_HOME"
log "  Python: $PYTHON"
log "  COBOL Source: $COBOLSRC_PATH"
log "  Copybook Path: $COPYLIB_PATH"
log "  Report Path: $REPORT_PATH"
log "  Config Path: $CONFIG_PATH"
log ""

# Validate Python installation
if ! command -v "$PYTHON" >/dev/null 2>&1; then
    error "Python not found. Please install Python 3.9+ on z/OS USS."
    error "IBM Python installation: /usr/lpp/IBM/cyp/v3r9/pyz/bin/python3"
    exit 1
fi

PYTHON_VERSION=$($PYTHON --version 2>&1 | awk '{print $2}')
log "Python version: $PYTHON_VERSION"

# Validate COBOL Harmonizer installation
if [ ! -d "$HARMONIZER_HOME" ]; then
    error "COBOL Harmonizer not found at $HARMONIZER_HOME"
    exit 1
fi

if [ ! -f "$HARMONIZER_HOME/cobol_harmonizer/__init__.py" ]; then
    error "COBOL Harmonizer installation appears incomplete"
    exit 1
fi

# Validate input directory
if [ ! -d "$COBOLSRC_PATH" ]; then
    error "COBOL source directory not found: $COBOLSRC_PATH"
    exit 1
fi

#-----------------------------------------------------------------------
# Run Analysis
#-----------------------------------------------------------------------

log "Starting semantic analysis..."
log ""

# Build command
CMD="$PYTHON $HARMONIZER_HOME/cobol_harmonizer.py analyze"
CMD="$CMD --input $COBOLSRC_PATH"
CMD="$CMD --output $REPORT_PATH"

# Add copybook path if specified
if [ -n "$COPYLIB_PATH" ] && [ -d "$COPYLIB_PATH" ]; then
    CMD="$CMD --copybook-path $COPYLIB_PATH"
fi

# Add compliance frameworks if specified
if [ -n "${HARMONIZER_COMPLIANCE:-}" ]; then
    CMD="$CMD --compliance $HARMONIZER_COMPLIANCE"
fi

# Add configuration file if exists
if [ -f "$CONFIG_PATH" ]; then
    CMD="$CMD --config $CONFIG_PATH"
fi

# Add cache directory
CACHE_DIR="${HARMONIZER_CACHE_DIR:-$HARMONIZER_HOME/.cache}"
mkdir -p "$CACHE_DIR"
CMD="$CMD --cache-dir $CACHE_DIR"

# Add audit log if specified
if [ -n "${HARMONIZER_AUDIT_LOG:-}" ]; then
    mkdir -p "$HARMONIZER_AUDIT_LOG"
    CMD="$CMD --audit-log $HARMONIZER_AUDIT_LOG"
fi

log "Command: $CMD"
log ""

# Execute analysis
START_TIME=$(date +%s)

if $CMD 2>&1 | tee -a "$LOG_FILE"; then
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))

    log ""
    log "========================================"
    log "Analysis completed successfully"
    log "Duration: ${DURATION} seconds"
    log "Report: $REPORT_PATH"
    log "Log: $LOG_FILE"
    log "========================================"

    # Print summary if report exists
    if [ -f "$REPORT_PATH" ]; then
        log ""
        log "Summary:"

        # Extract key metrics from JSON report
        if command -v jq >/dev/null 2>&1; then
            TOTAL_BUGS=$(jq -r '.summary.total_bugs_found // 0' "$REPORT_PATH")
            CRITICAL=$(jq -r '.summary.critical_bugs // 0' "$REPORT_PATH")
            HIGH=$(jq -r '.summary.high_bugs // 0' "$REPORT_PATH")
            MEDIUM=$(jq -r '.summary.medium_bugs // 0' "$REPORT_PATH")
            LOW=$(jq -r '.summary.low_bugs // 0' "$REPORT_PATH")
            COST_MIN=$(jq -r '.summary.estimated_cost_savings.minimum // 0' "$REPORT_PATH")
            COST_MAX=$(jq -r '.summary.estimated_cost_savings.maximum // 0' "$REPORT_PATH")

            log "  Total bugs found: $TOTAL_BUGS"
            log "    Critical: $CRITICAL"
            log "    High: $HIGH"
            log "    Medium: $MEDIUM"
            log "    Low: $LOW"
            log "  Estimated cost savings: \$${COST_MIN} - \$${COST_MAX}"
        else
            log "  (Install jq for detailed summary)"
            log "  Report generated successfully"
        fi
    fi

    exit 0
else
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))

    error ""
    error "========================================"
    error "Analysis failed after ${DURATION} seconds"
    error "Check log for details: $LOG_FILE"
    error "========================================"

    exit 1
fi
