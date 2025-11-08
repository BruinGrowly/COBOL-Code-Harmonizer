# JCL Integration for IBM z/OS

This directory contains JCL (Job Control Language) templates for running COBOL Code Harmonizer as batch jobs on IBM z/OS mainframes.

## Files

| File | Purpose |
|------|---------|
| `HARMONIZ.jcl` | Main JCL template with full configuration options |
| `SINGLE.jcl` | Analyze a single COBOL program |
| `SOXAUDIT.jcl` | SOX compliance audit (quarterly) |
| `NIGHTLY.jcl` | Scheduled nightly codebase scan |
| `harmonizer_wrapper.sh` | Shell script wrapper called by JCL |

## Prerequisites

### 1. Python on z/OS USS

COBOL Harmonizer requires Python 3.9+ on z/OS Unix System Services (USS).

**IBM Python Installation:**
```bash
# IBM Python is typically installed at:
/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3

# Verify installation:
/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3 --version
```

**Alternative: Rocket Software Python**
```bash
# Install from Rocket Software:
# https://www.rocketsoftware.com/zos-open-source

# Verify:
python3 --version
```

### 2. COBOL Harmonizer Installation

```bash
# SSH to z/OS USS
ssh userid@mainframe.company.com

# Create installation directory
mkdir -p /u/cobol-harmonizer
cd /u/cobol-harmonizer

# Clone repository (if git available)
git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git .

# OR transfer via FTP (binary mode)
ftp mainframe.company.com
> binary
> cd /u/cobol-harmonizer
> mput *.py
> mput -r cobol_harmonizer
```

### 3. Directory Structure

Create the following directories on USS:

```bash
mkdir -p /u/cobol-harmonizer/logs
mkdir -p /u/cobol-harmonizer/.cache
mkdir -p /u/cobol-harmonizer/.audit_log
mkdir -p /u/cobol-harmonizer/baselines
mkdir -p /u/reports/nightly
mkdir -p /u/reports/sox
mkdir -p /u/reports/trends
```

### 4. Make Scripts Executable

```bash
chmod +x /u/cobol-harmonizer/jcl/*.sh
```

## Quick Start

### Option 1: Analyze Single File

1. Edit `SINGLE.jcl`:
   ```jcl
   //STDPARM  DD *
   SH cd /u/cobol-harmonizer;
      python3 cobol_harmonizer.py analyze \
      --input /u/prod/cobol/YOUR-PROGRAM.cbl \
      --output /u/reports/your_analysis.json \
      --format json \
      --verbose
   ```

2. Submit:
   ```
   SUBMIT 'YOUR.JCL.LIB(SINGLE)'
   ```

3. Check output:
   ```
   ST  (Status)
   OUTPUT userid
   ```

### Option 2: Full Codebase Analysis

1. Edit `HARMONIZ.jcl`:
   - Update `COBOLSRC` DD to your COBOL library
   - Update `COPYLIB` DD to your copybook library
   - Update `REPORT` DD for output location

2. Submit:
   ```
   SUBMIT 'YOUR.JCL.LIB(HARMONIZ)'
   ```

3. Review results:
   ```bash
   # USS
   cat /u/reports/harmonizer_*.json
   ```

### Option 3: SOX Compliance Audit

1. Schedule `SOXAUDIT.jcl` quarterly (Jan, Apr, Jul, Oct)

2. Submit:
   ```
   SUBMIT 'YOUR.JCL.LIB(SOXAUDIT)'
   ```

3. Output files:
   - `/u/reports/sox/Q4_2025_sox_analysis.json` (detailed data)
   - `/u/reports/sox/Q4_2025_sox_dashboard.html` (for auditors)
   - `/u/reports/sox/Q4_2025_sox_procedures.csv` (for Excel)

### Option 4: Nightly Scheduled Scan

1. Add `NIGHTLY.jcl` to TWS (Tivoli Workload Scheduler) or JES scheduler

2. Schedule for 02:00 AM daily

3. Automatic actions:
   - Full codebase analysis
   - Baseline comparison
   - Trend analysis
   - Alert on critical bugs
   - Archive to DB2
   - Email summary

## Configuration

### Environment Variables (STDENV DD)

```bash
# Required
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON  # Auto-convert ASCII/EBCDIC

# Optional
export HARMONIZER_HOME=/u/cobol-harmonizer
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
export HARMONIZER_LOG_DIR=/u/cobol-harmonizer/logs
export HARMONIZER_COMPLIANCE=SOX,PCI_DSS,GDPR,HIPAA
export HARMONIZER_AUDIT_LOG=/u/cobol-harmonizer/.audit_log
```

### Config File (CONFIG DD)

```json
{
  "analysis_type": "batch",
  "output_format": "json",
  "compliance_frameworks": ["sox", "pci_dss", "gdpr", "hipaa"],
  "thresholds": {
    "disharmony_min": 0.5,
    "risk_score_min": 40
  },
  "features": {
    "semantic_analysis": true,
    "compliance_tagging": true,
    "risk_assessment": true,
    "call_graph": true,
    "baseline_comparison": false
  },
  "output_options": {
    "include_harmonious": false,
    "max_findings": 100,
    "sort_by": "risk_score"
  }
}
```

## Common Patterns

### Pattern 1: Analyze Changed Programs Only

```jcl
//CHANGED  EXEC PGM=BPXBATCH
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   # Get list of changed programs from git
   git diff --name-only HEAD~1 HEAD | grep '\.cbl$' | \
   while read file; do
     python3 cobol_harmonizer.py analyze \
       --input "$file" \
       --output "/u/reports/changed/$(basename $file .cbl).json"
   done
/*
```

### Pattern 2: Alert on High-Risk Changes

```jcl
//ALERT    EXEC PGM=BPXBATCH
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 cobol_harmonizer.py analyze \
     --input /u/prod/cobol/CRITICAL-PROGRAM.cbl \
     --output /tmp/analysis.json \
     --alert-threshold 70 \
     --alert-command "/u/cobol-harmonizer/jcl/send_alert.sh" \
     --alert-email "security@company.com"
/*
```

### Pattern 3: Compare Before/After Change

```jcl
//COMPARE  EXEC PGM=BPXBATCH
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   # Analyze baseline (before change)
   python3 cobol_harmonizer.py analyze \
     --input /u/prod/cobol/PROGRAM.cbl.baseline \
     --output /tmp/before.json

   # Analyze current (after change)
   python3 cobol_harmonizer.py analyze \
     --input /u/prod/cobol/PROGRAM.cbl \
     --output /tmp/after.json

   # Compare
   python3 -m cobol_harmonizer.baseline.compare \
     --baseline /tmp/before.json \
     --current /tmp/after.json \
     --output /u/reports/change_impact.json
/*
```

## Integration with IBM Tools

### IBM Developer for z/OS (RAD)

1. Export analysis to CSV:
   ```bash
   python3 -m cobol_harmonizer.cli export \
     --input /u/reports/analysis.json \
     --output /u/reports/analysis.csv \
     --format csv
   ```

2. Download CSV to workstation

3. Import to RAD:
   - File → Import → CSV
   - Select `/u/reports/analysis.csv`
   - Map columns to RAD markers

### DB2 Integration

See `HARMONIZ.jcl` STEP 3 for example DB2 archival.

**Create DB2 Table:**
```sql
CREATE TABLE HARMONIZER.ANALYSIS_RESULTS (
    ANALYSIS_ID INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
    ANALYSIS_DATE DATE NOT NULL,
    USER_ID VARCHAR(8) NOT NULL,
    PROJECT_NAME VARCHAR(50),
    TOTAL_FILES INTEGER,
    TOTAL_PROCEDURES INTEGER,
    BUGS_FOUND INTEGER,
    CRITICAL_BUGS INTEGER,
    HIGH_BUGS INTEGER,
    MEDIUM_BUGS INTEGER,
    LOW_BUGS INTEGER,
    ESTIMATED_COST_MIN DECIMAL(12,2),
    ESTIMATED_COST_MAX DECIMAL(12,2),
    REPORT_PATH VARCHAR(255),
    PRIMARY KEY (ANALYSIS_ID)
);

CREATE INDEX HARMONIZER.IX_ANALYSIS_DATE
    ON HARMONIZER.ANALYSIS_RESULTS (ANALYSIS_DATE DESC);
```

**Query Historical Trends:**
```sql
-- Bug trends over last 90 days
SELECT
    ANALYSIS_DATE,
    TOTAL_PROCEDURES,
    BUGS_FOUND,
    CRITICAL_BUGS,
    ESTIMATED_COST_MIN
FROM HARMONIZER.ANALYSIS_RESULTS
WHERE ANALYSIS_DATE >= CURRENT_DATE - 90 DAYS
ORDER BY ANALYSIS_DATE DESC;

-- Project comparison
SELECT
    PROJECT_NAME,
    AVG(BUGS_FOUND) AS AVG_BUGS,
    MAX(CRITICAL_BUGS) AS MAX_CRITICAL,
    SUM(ESTIMATED_COST_MIN) AS TOTAL_COST_SAVED
FROM HARMONIZER.ANALYSIS_RESULTS
GROUP BY PROJECT_NAME
ORDER BY TOTAL_COST_SAVED DESC;
```

## Troubleshooting

### Error: "Python not found"

**Solution:**
```jcl
//STDENV   DD *
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
/*
```

### Error: "Permission denied"

**Solution:**
```bash
# Make scripts executable
chmod +x /u/cobol-harmonizer/jcl/*.sh

# Check file permissions
ls -la /u/cobol-harmonizer
```

### Error: "Module not found"

**Solution:**
```jcl
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
/*
```

### Error: "EBCDIC/ASCII conversion issues"

**Solution:**
```jcl
//STDENV   DD *
export _BPXK_AUTOCVT=ON  # Auto-convert
export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG)"
/*
```

### Error: "Copybook not found"

**Solution:**
```jcl
//STDENV   DD *
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook:/u/test/copybook
/*
```

## Performance Tips

### 1. Enable Caching

Copybook resolution is expensive. Enable caching:

```bash
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
```

Cache speeds up subsequent runs by **95%**.

### 2. Parallel Processing

For large codebases, analyze in parallel:

```bash
# Split files into chunks
find /u/prod/cobol -name '*.cbl' | split -l 100 - chunk_

# Process each chunk in parallel (separate JCL jobs)
# Job 1: chunk_aa
# Job 2: chunk_ab
# Job 3: chunk_ac

# Merge results
python3 -m cobol_harmonizer.cli merge \
  --inputs /u/reports/chunk_*.json \
  --output /u/reports/merged.json
```

### 3. Incremental Analysis

Only analyze changed files:

```bash
# Last nightly scan: /u/reports/nightly/analysis_20251107.json
# Current scan: /u/reports/nightly/analysis_20251108.json

# Compare and identify changed procedures
python3 -m cobol_harmonizer.baseline.compare \
  --baseline /u/reports/nightly/analysis_20251107.json \
  --current /u/reports/nightly/analysis_20251108.json \
  --output-changed-only
```

## Best Practices

### 1. Baseline Management

Establish semantic baseline after initial analysis:

```bash
# After first full analysis
cp /u/reports/full_analysis.json /u/cobol-harmonizer/baselines/production.json

# Update baseline quarterly (after code review)
```

### 2. Change Control Integration

Add to your change control process:

1. **Before Change:** Analyze current version
2. **After Change:** Analyze modified version
3. **Compare:** Generate change impact report
4. **Review:** Approve if risk score acceptable
5. **Deploy:** Promote to production

### 3. Audit Trail

Enable audit logging for compliance:

```bash
export HARMONIZER_AUDIT_LOG=/u/cobol-harmonizer/.audit_log
export HARMONIZER_USER=$LOGNAME
export HARMONIZER_JUSTIFICATION="Quarterly SOX audit"
```

Audit log retention: **365 days** (configurable)

### 4. Scheduled Scans

Run scans during off-peak hours:

- Nightly: 02:00 AM (full codebase)
- Weekly: Sunday 01:00 AM (trend analysis)
- Quarterly: First of month (SOX audit)

## Support

### Documentation

- Main README: `/u/cobol-harmonizer/README.md`
- IBM Proof of Value: `/u/cobol-harmonizer/IBM_PROOF_OF_VALUE.md`
- Benchmark Comparison: `/u/cobol-harmonizer/BENCHMARK_COMPARISON.md`
- Customer Case Studies: `/u/cobol-harmonizer/IBM_CUSTOMER_CASE_STUDIES.md`

### GitHub

- Repository: https://github.com/BruinGrowly/COBOL-Code-Harmonizer
- Issues: https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues

### IBM Resources

- z/OS USS Documentation: https://www.ibm.com/docs/en/zos
- IBM Python for z/OS: https://www.ibm.com/products/python-for-zos
- BPXBATCH: https://www.ibm.com/docs/en/zos/2.4.0?topic=services-bpxbatch-run-shell-command-program

---

**Version:** 0.5.0
**Last Updated:** 2025-11-08
**Platform:** IBM z/OS with Unix System Services (USS)
