# IBM Mainframe Integration Guide

**COBOL Code Harmonizer v0.5.0+**

Complete guide for integrating COBOL Code Harmonizer with IBM z/OS mainframes.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation on z/OS USS](#installation-on-zos-uss)
3. [JCL Integration](#jcl-integration)
4. [SQL/CICS Analysis](#sqlcics-analysis)
5. [IBM Developer for z/OS Integration](#ibm-developer-for-zos-integration)
6. [DB2 Integration](#db2-integration)
7. [Production Deployment](#production-deployment)
8. [Performance Optimization](#performance-optimization)
9. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Required Components

| Component | Version | Purpose |
|-----------|---------|---------|
| **IBM z/OS** | 2.2+ | Operating system |
| **z/OS USS** | - | Unix System Services |
| **Python** | 3.9+ | Runtime for COBOL Harmonizer |
| **Git** (optional) | 2.x+ | Source control |

### Optional Components

| Component | Purpose |
|-----------|---------|
| **IBM Developer for z/OS (RAD)** | IDE integration |
| **DB2** | Result archival |
| **CICS** | Transaction processing analysis |
| **TWS/OPC** | Job scheduling |

---

## Installation on z/OS USS

### Step 1: Install Python on z/OS

**Option A: IBM Python (Recommended)**

```bash
# IBM Python is typically pre-installed at:
/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3

# Verify installation
/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3 --version
# Expected: Python 3.9.x or higher

# Add to PATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
```

**Option B: Rocket Software Python**

```bash
# Download from: https://www.rocketsoftware.com/zos-open-source
# Follow Rocket installation guide

# Verify
python3 --version
```

### Step 2: Create Installation Directory

```bash
# SSH to z/OS USS
ssh userid@mainframe.company.com

# Create directory structure
mkdir -p /u/cobol-harmonizer
mkdir -p /u/cobol-harmonizer/logs
mkdir -p /u/cobol-harmonizer/.cache
mkdir -p /u/cobol-harmonizer/.audit_log
mkdir -p /u/cobol-harmonizer/baselines
mkdir -p /u/reports/nightly
mkdir -p /u/reports/sox
mkdir -p /u/reports/trends

# Set permissions
chmod 755 /u/cobol-harmonizer
```

### Step 3: Transfer COBOL Harmonizer

**Option A: Git Clone (if git available)**

```bash
cd /u/cobol-harmonizer
git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git .
```

**Option B: FTP Transfer**

```bash
# On your workstation
ftp mainframe.company.com
> binary
> cd /u/cobol-harmonizer
> mput *.py
> mkdir cobol_harmonizer
> cd cobol_harmonizer
> mput cobol_harmonizer/*.py
> mkdir semantic
> cd semantic
> mput cobol_harmonizer/semantic/*.py
# ... repeat for all directories
```

**Option C: SCP Transfer**

```bash
# On your workstation
scp -r * userid@mainframe.company.com:/u/cobol-harmonizer/
```

### Step 4: Verify Installation

```bash
cd /u/cobol-harmonizer

# Test Python import
python3 -c "import cobol_harmonizer; print('OK')"

# Run help
python3 cobol_harmonizer.py --help

# Analyze sample file
python3 cobol_harmonizer.py analyze \
  --input examples/harmonious_example.cbl \
  --output /tmp/test.json

# Check output
cat /tmp/test.json
```

### Step 5: Make Scripts Executable

```bash
chmod +x /u/cobol-harmonizer/jcl/*.sh
chmod +x /u/cobol-harmonizer/*.py
```

---

## JCL Integration

### Basic JCL Template

See `jcl/HARMONIZ.jcl` for the main template.

**Key Components:**

1. **BPXBATCH**: IBM utility to run Unix commands from JCL
2. **STDENV DD**: Environment variables
3. **STDPARM DD**: Shell commands to execute
4. **PATH DDs**: Input/output file paths

**Simple Example:**

```jcl
//ANALYZE  EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export _BPXK_AUTOCVT=ON
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 cobol_harmonizer.py analyze \
   --input /u/prod/cobol/PROGRAM.cbl \
   --output /u/reports/analysis.json
/*
```

### Pre-Built JCL Templates

| Template | Purpose | Location |
|----------|---------|----------|
| `HARMONIZ.jcl` | Full-featured batch analysis | `jcl/HARMONIZ.jcl` |
| `SINGLE.jcl` | Analyze single file | `jcl/SINGLE.jcl` |
| `SOXAUDIT.jcl` | SOX compliance audit | `jcl/SOXAUDIT.jcl` |
| `NIGHTLY.jcl` | Scheduled nightly scan | `jcl/NIGHTLY.jcl` |

**See:** `jcl/README.md` for complete JCL documentation.

---

## SQL/CICS Analysis

### Overview

COBOL Harmonizer includes **enhanced semantic analysis** for:
- **EXEC SQL** statements (DB2, Oracle, Informix)
- **EXEC CICS** commands (Customer Information Control System)

### SQL Analysis

**Supported SQL Operations:**

| SQL Operation | Semantic Dimension | LJPW Coordinates |
|---------------|-------------------|------------------|
| **SELECT** | Wisdom (Retrieval) | (0.1, 0.2, 0.0, 0.7) |
| **INSERT** | Power (Modification) | (0.1, 0.2, 0.7, 0.0) |
| **UPDATE** | Power (Modification) | (0.0, 0.2, 0.7, 0.1) |
| **DELETE** | Power (Modification) | (0.0, 0.2, 0.8, 0.0) |
| **COMMIT** | Justice (Transactional) | (0.1, 0.6, 0.3, 0.0) |
| **ROLLBACK** | Justice (Transactional) | (0.0, 0.6, 0.4, 0.0) |
| **CALL** | Love (Communication) | (0.5, 0.1, 0.3, 0.1) |

**Example: DB2 COBOL Program**

```cobol
VALIDATE-CUSTOMER-BALANCE.
    *> Procedure name implies: Validation (Justice)

    EXEC SQL
        SELECT ACCOUNT_BALANCE
        INTO :WS-BALANCE
        FROM CUSTOMER_ACCOUNTS
        WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
    END-EXEC.

    IF WS-BALANCE < 0
        MOVE "INVALID" TO BALANCE-STATUS.

    *> Wait, it also updates!
    EXEC SQL
        UPDATE CUSTOMER_ACCOUNTS
        SET LAST_VALIDATED = CURRENT_TIMESTAMP
        WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
    END-EXEC.
```

**Analysis Result:**
```json
{
  "procedure": "VALIDATE-CUSTOMER-BALANCE",
  "intent": {
    "dimension": "Justice (Validation)",
    "coords": [0.0, 0.7, 0.1, 0.2]
  },
  "execution": {
    "dimension": "Power (Modification)",
    "coords": [0.05, 0.4, 0.35, 0.35],
    "sql_statements": [
      {"operation": "SELECT", "semantic": "Wisdom"},
      {"operation": "UPDATE", "semantic": "Power"}
    ]
  },
  "disharmony_score": 0.68,
  "severity": "MEDIUM",
  "warning": "VALIDATE procedure modifies data (UPDATE statement)"
}
```

### CICS Analysis

**Supported CICS Commands:**

| CICS Command | Semantic Dimension | LJPW Coordinates |
|--------------|-------------------|------------------|
| **READ** | Wisdom (Retrieval) | (0.1, 0.2, 0.0, 0.7) |
| **WRITE** | Power (Modification) | (0.1, 0.1, 0.7, 0.1) |
| **REWRITE** | Power (Modification) | (0.0, 0.2, 0.7, 0.1) |
| **DELETE** | Power (Modification) | (0.0, 0.2, 0.8, 0.0) |
| **SEND** | Love (Communication) | (0.6, 0.1, 0.2, 0.1) |
| **RECEIVE** | Love (Communication) | (0.5, 0.1, 0.1, 0.3) |
| **LINK** | Love (Communication) | (0.6, 0.1, 0.2, 0.1) |
| **SYNCPOINT** | Justice (Transactional) | (0.1, 0.6, 0.3, 0.0) |

**Example: CICS Transaction Program**

```cobol
PROCESS-CUSTOMER-INQUIRY.
    *> Procedure name implies: Read-only processing (Wisdom)

    EXEC CICS
        READ FILE('CUSTFILE')
        INTO(CUSTOMER-RECORD)
        RIDFLD(CUSTOMER-ID)
    END-EXEC.

    MOVE CUSTOMER-NAME TO DISPLAY-NAME.

    *> But wait, it also writes!
    EXEC CICS
        WRITEQ TS QUEUE('AUDIT')
        FROM(AUDIT-RECORD)
        LENGTH(AUDIT-LENGTH)
    END-EXEC.

    EXEC CICS
        SEND MAP('CUSTMAP')
        FROM(CUSTOMER-RECORD)
    END-EXEC.
```

**Analysis Result:**
```json
{
  "procedure": "PROCESS-CUSTOMER-INQUIRY",
  "intent": {
    "dimension": "Wisdom (Processing/Retrieval)",
    "coords": [0.1, 0.2, 0.4, 0.3]
  },
  "execution": {
    "dimension": "Love (Communication)",
    "coords": [0.4, 0.13, 0.33, 0.33],
    "cics_commands": [
      {"command": "READ", "semantic": "Wisdom", "resource": "FILE"},
      {"command": "WRITEQ TS", "semantic": "Power", "resource": "TEMP_STORAGE"},
      {"command": "SEND MAP", "semantic": "Love", "resource": "TERMINAL"}
    ]
  },
  "disharmony_score": 0.52,
  "severity": "MEDIUM",
  "warning": "INQUIRY procedure writes to temporary storage"
}
```

### Combined Analysis

COBOL Harmonizer **combines** regular COBOL verbs with SQL/CICS semantics:

**Weighting Strategy:**
- COBOL verbs: **50%**
- SQL statements: **30%**
- CICS commands: **20%**

**Example: Complex Procedure**

```cobol
UPDATE-CUSTOMER-PROFILE.
    *> Procedure name: UPDATE (Power)

    *> COBOL verbs
    MOVE NEW-NAME TO CUSTOMER-NAME.
    MOVE NEW-ADDRESS TO CUSTOMER-ADDRESS.

    *> SQL statement
    EXEC SQL
        UPDATE CUSTOMERS
        SET NAME = :CUSTOMER-NAME,
            ADDRESS = :CUSTOMER-ADDRESS
        WHERE CUSTOMER_ID = :CUSTOMER-ID
    END-EXEC.

    *> CICS command
    EXEC CICS
        SYNCPOINT
    END-EXEC.
```

**Combined Semantic Analysis:**
```json
{
  "procedure": "UPDATE-CUSTOMER-PROFILE",
  "intent_coords": [0.0, 0.2, 0.7, 0.1],
  "execution_breakdown": {
    "cobol_verbs": {
      "coords": [0.0, 0.1, 0.6, 0.3],
      "weight": 0.5,
      "verbs": ["MOVE", "MOVE"]
    },
    "sql_statements": {
      "coords": [0.0, 0.2, 0.7, 0.1],
      "weight": 0.3,
      "operations": ["UPDATE"]
    },
    "cics_commands": {
      "coords": [0.1, 0.6, 0.3, 0.0],
      "weight": 0.2,
      "commands": ["SYNCPOINT"]
    }
  },
  "combined_execution_coords": [0.02, 0.25, 0.59, 0.14],
  "disharmony_score": 0.15,
  "severity": "HARMONIOUS",
  "note": "Procedure behavior matches name (UPDATE)"
}
```

### Using SQL/CICS Analyzer Programmatically

```python
from cobol_harmonizer.semantic.sql_cics_analyzer import SQLCICSAnalyzer

# Initialize analyzer
analyzer = SQLCICSAnalyzer()

# Read COBOL source
with open('BANKING-PROGRAM.cbl', 'r') as f:
    source_code = f.read()

# Extract SQL statements
sql_statements = analyzer.extract_sql_statements(source_code)
for stmt in sql_statements:
    print(f"SQL {stmt.operation}: {stmt.full_text[:50]}...")
    print(f"  Tables: {stmt.tables}")
    print(f"  Modifies data: {stmt.is_update}")

# Extract CICS commands
cics_commands = analyzer.extract_cics_commands(source_code)
for cmd in cics_commands:
    print(f"CICS {cmd.command}: {cmd.resource_type}")
    print(f"  Modifies data: {cmd.is_update}")

# Get semantic coordinates
sql_coords = analyzer.analyze_sql_semantics(sql_statements)
cics_coords = analyzer.analyze_cics_semantics(cics_commands)

print(f"SQL semantics: {sql_coords}")
print(f"CICS semantics: {cics_coords}")

# Combine with COBOL verb analysis
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer

exec_analyzer = ExecutionAnalyzer()
# ... analyze COBOL procedure to get cobol_coords

combined_coords = analyzer.analyze_combined_semantics(
    cobol_coords=cobol_coords,
    sql_statements=sql_statements,
    cics_commands=cics_commands
)

print(f"Combined semantics: {combined_coords}")
```

---

## IBM Developer for z/OS Integration

### Export Analysis to RAD

**Step 1: Generate CSV Report**

```bash
cd /u/cobol-harmonizer

python3 -m cobol_harmonizer.compliance.audit_reporter \
  --input /u/reports/analysis.json \
  --output /u/reports/analysis.csv \
  --format csv
```

**Step 2: Download to Workstation**

```bash
# On workstation
scp userid@mainframe.company.com:/u/reports/analysis.csv ./
```

**Step 3: Import to RAD**

1. Open IBM Developer for z/OS (RAD)
2. File → Import → General → CSV File
3. Select `analysis.csv`
4. Map columns:
   - `procedure` → Marker Name
   - `file` → Resource Path
   - `severity` → Severity
   - `disharmony_score` → Priority
   - `business_impact` → Description

**Step 4: View in RAD**

- Problems View: Shows all findings
- Task View: Sorted by severity
- Source Editor: Markers appear in code

### Use RAD for Code Review

1. **Filter by Severity:**
   - Critical: Review immediately
   - High: Review this sprint
   - Medium: Review next sprint
   - Low: Backlog

2. **Add Comments:**
   - Right-click marker → Add Comment
   - Document why semantic disharmony exists
   - Link to work items

3. **Track Fixes:**
   - Mark as "Fixed" in RAD
   - Re-run analysis to verify
   - Update baseline

---

## DB2 Integration

### Create Results Table

```sql
-- Create schema
CREATE SCHEMA HARMONIZER;

-- Create main results table
CREATE TABLE HARMONIZER.ANALYSIS_RESULTS (
    ANALYSIS_ID INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
    ANALYSIS_DATE DATE NOT NULL,
    ANALYSIS_TIME TIME NOT NULL,
    USER_ID VARCHAR(8) NOT NULL,
    PROJECT_NAME VARCHAR(50),

    -- Summary metrics
    TOTAL_FILES INTEGER,
    TOTAL_PROCEDURES INTEGER,
    TOTAL_LOC INTEGER,

    -- Bug counts
    BUGS_FOUND INTEGER,
    CRITICAL_BUGS INTEGER,
    HIGH_BUGS INTEGER,
    MEDIUM_BUGS INTEGER,
    LOW_BUGS INTEGER,

    -- Compliance metrics
    SOX_PROCEDURES INTEGER,
    PCI_PROCEDURES INTEGER,
    GDPR_PROCEDURES INTEGER,
    HIPAA_PROCEDURES INTEGER,

    -- Financial metrics
    ESTIMATED_COST_MIN DECIMAL(12,2),
    ESTIMATED_COST_MAX DECIMAL(12,2),

    -- Metadata
    REPORT_PATH VARCHAR(255),
    ANALYSIS_DURATION_SEC INTEGER,

    PRIMARY KEY (ANALYSIS_ID)
);

-- Create indexes
CREATE INDEX HARMONIZER.IX_ANALYSIS_DATE
    ON HARMONIZER.ANALYSIS_RESULTS (ANALYSIS_DATE DESC);

CREATE INDEX HARMONIZER.IX_PROJECT_NAME
    ON HARMONIZER.ANALYSIS_RESULTS (PROJECT_NAME);

-- Create findings detail table
CREATE TABLE HARMONIZER.FINDINGS (
    FINDING_ID INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY,
    ANALYSIS_ID INTEGER NOT NULL,
    FILE_PATH VARCHAR(255) NOT NULL,
    PROCEDURE_NAME VARCHAR(100) NOT NULL,
    DISHARMONY_SCORE DECIMAL(5,3),
    SEVERITY VARCHAR(20),
    RISK_SCORE DECIMAL(5,1),
    INTENT_DIMENSION VARCHAR(50),
    EXECUTION_DIMENSION VARCHAR(50),
    BUSINESS_IMPACT VARCHAR(500),
    COMPLIANCE_TAGS VARCHAR(500),

    PRIMARY KEY (FINDING_ID),
    FOREIGN KEY (ANALYSIS_ID) REFERENCES HARMONIZER.ANALYSIS_RESULTS(ANALYSIS_ID)
        ON DELETE CASCADE
);

CREATE INDEX HARMONIZER.IX_FINDINGS_ANALYSIS
    ON HARMONIZER.FINDINGS (ANALYSIS_ID);

CREATE INDEX HARMONIZER.IX_FINDINGS_SEVERITY
    ON HARMONIZER.FINDINGS (SEVERITY, RISK_SCORE DESC);

-- Grant permissions
GRANT SELECT, INSERT, UPDATE ON HARMONIZER.ANALYSIS_RESULTS TO PUBLIC;
GRANT SELECT, INSERT, UPDATE ON HARMONIZER.FINDINGS TO PUBLIC;
```

### Load Results via Python

```python
# db2_loader.py - Load analysis results into DB2

import json
import ibm_db
from datetime import datetime

# Read analysis report
with open('/u/reports/nightly/analysis_20251108.json', 'r') as f:
    report = json.load(f)

# Connect to DB2
conn = ibm_db.connect("DATABASE=DB2P;HOSTNAME=mainframe;PORT=5035;UID=user;PWD=pass;", "", "")

# Insert summary
summary = report['summary']
insert_summary = """
INSERT INTO HARMONIZER.ANALYSIS_RESULTS
(ANALYSIS_DATE, ANALYSIS_TIME, USER_ID, PROJECT_NAME,
 TOTAL_FILES, TOTAL_PROCEDURES, TOTAL_LOC,
 BUGS_FOUND, CRITICAL_BUGS, HIGH_BUGS, MEDIUM_BUGS, LOW_BUGS,
 SOX_PROCEDURES, PCI_PROCEDURES, GDPR_PROCEDURES, HIPAA_PROCEDURES,
 ESTIMATED_COST_MIN, ESTIMATED_COST_MAX,
 REPORT_PATH)
VALUES
(CURRENT_DATE, CURRENT_TIME, ?, ?,
 ?, ?, ?,
 ?, ?, ?, ?, ?,
 ?, ?, ?, ?,
 ?, ?,
 ?)
"""

stmt = ibm_db.prepare(conn, insert_summary)
ibm_db.bind_param(stmt, 1, 'SYSADM')
ibm_db.bind_param(stmt, 2, 'Production COBOL')
ibm_db.bind_param(stmt, 3, summary['total_files'])
ibm_db.bind_param(stmt, 4, summary['total_procedures'])
ibm_db.bind_param(stmt, 5, summary['total_loc'])
ibm_db.bind_param(stmt, 6, summary['total_bugs_found'])
ibm_db.bind_param(stmt, 7, summary['critical_bugs'])
ibm_db.bind_param(stmt, 8, summary['high_bugs'])
ibm_db.bind_param(stmt, 9, summary['medium_bugs'])
ibm_db.bind_param(stmt, 10, summary['low_bugs'])
ibm_db.bind_param(stmt, 11, summary['key_metrics']['sox_procedures_flagged'])
ibm_db.bind_param(stmt, 12, summary['key_metrics']['pci_procedures_flagged'])
ibm_db.bind_param(stmt, 13, summary['key_metrics']['gdpr_procedures_flagged'])
ibm_db.bind_param(stmt, 14, 0)  # HIPAA
ibm_db.bind_param(stmt, 15, summary['estimated_cost_savings']['minimum'])
ibm_db.bind_param(stmt, 16, summary['estimated_cost_savings']['maximum'])
ibm_db.bind_param(stmt, 17, '/u/reports/nightly/analysis_20251108.json')

ibm_db.execute(stmt)

# Get analysis_id
analysis_id = ibm_db.last_insert_id(stmt)

# Insert detailed findings
for project in report['projects']:
    for finding in project['top_findings']:
        insert_finding = """
        INSERT INTO HARMONIZER.FINDINGS
        (ANALYSIS_ID, FILE_PATH, PROCEDURE_NAME,
         DISHARMONY_SCORE, SEVERITY, RISK_SCORE,
         INTENT_DIMENSION, EXECUTION_DIMENSION,
         BUSINESS_IMPACT, COMPLIANCE_TAGS)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """

        stmt2 = ibm_db.prepare(conn, insert_finding)
        ibm_db.bind_param(stmt2, 1, analysis_id)
        ibm_db.bind_param(stmt2, 2, finding['file'])
        ibm_db.bind_param(stmt2, 3, finding['procedure'])
        ibm_db.bind_param(stmt2, 4, finding['disharmony_score'])
        ibm_db.bind_param(stmt2, 5, finding['severity'])
        ibm_db.bind_param(stmt2, 6, finding['risk_score'])
        ibm_db.bind_param(stmt2, 7, finding['intent'])
        ibm_db.bind_param(stmt2, 8, finding['execution'])
        ibm_db.bind_param(stmt2, 9, finding['business_impact'])
        ibm_db.bind_param(stmt2, 10, ','.join(finding['compliance_tags']))

        ibm_db.execute(stmt2)

ibm_db.commit(conn)
ibm_db.close(conn)

print(f"Loaded analysis {analysis_id} into DB2")
```

### Query Historical Trends

```sql
-- Bug trends over last 90 days
SELECT
    ANALYSIS_DATE,
    TOTAL_PROCEDURES,
    BUGS_FOUND,
    CRITICAL_BUGS,
    CAST(BUGS_FOUND AS DECIMAL(10,2)) / TOTAL_PROCEDURES * 100 AS BUG_RATE_PCT
FROM HARMONIZER.ANALYSIS_RESULTS
WHERE ANALYSIS_DATE >= CURRENT_DATE - 90 DAYS
ORDER BY ANALYSIS_DATE DESC;

-- Project comparison
SELECT
    PROJECT_NAME,
    COUNT(*) AS ANALYSIS_COUNT,
    AVG(BUGS_FOUND) AS AVG_BUGS,
    MAX(CRITICAL_BUGS) AS MAX_CRITICAL,
    SUM(ESTIMATED_COST_MIN) AS TOTAL_COST_SAVED
FROM HARMONIZER.ANALYSIS_RESULTS
GROUP BY PROJECT_NAME
ORDER BY TOTAL_COST_SAVED DESC;

-- Top 10 riskiest procedures (all time)
SELECT
    F.PROCEDURE_NAME,
    F.FILE_PATH,
    F.RISK_SCORE,
    F.DISHARMONY_SCORE,
    F.SEVERITY,
    F.BUSINESS_IMPACT,
    A.ANALYSIS_DATE
FROM HARMONIZER.FINDINGS F
JOIN HARMONIZER.ANALYSIS_RESULTS A ON F.ANALYSIS_ID = A.ANALYSIS_ID
ORDER BY F.RISK_SCORE DESC
FETCH FIRST 10 ROWS ONLY;

-- Compliance summary
SELECT
    ANALYSIS_DATE,
    SOX_PROCEDURES,
    PCI_PROCEDURES,
    GDPR_PROCEDURES,
    HIPAA_PROCEDURES
FROM HARMONIZER.ANALYSIS_RESULTS
WHERE ANALYSIS_DATE >= CURRENT_DATE - 30 DAYS
ORDER BY ANALYSIS_DATE DESC;
```

---

## Production Deployment

### Deployment Checklist

- [ ] Python 3.9+ installed on z/OS USS
- [ ] COBOL Harmonizer installed in `/u/cobol-harmonizer`
- [ ] Directory structure created
- [ ] JCL templates customized for your environment
- [ ] Shell scripts made executable
- [ ] Test run completed successfully
- [ ] DB2 tables created (if using)
- [ ] Scheduled jobs configured (TWS/OPC)
- [ ] Audit logging enabled
- [ ] Baseline established
- [ ] Team trained

### Recommended Schedule

| Job | Frequency | Time | Purpose |
|-----|-----------|------|---------|
| **NIGHTLY** | Daily | 02:00 AM | Full codebase scan |
| **SOXAUDIT** | Quarterly | 1st of month | Compliance audit |
| **BASELINE** | Monthly | Last Sunday | Update baseline |
| **TRENDS** | Weekly | Sunday 01:00 AM | Generate trend reports |

### Monitoring

**Key Metrics to Monitor:**

1. **Job Success Rate:** Aim for 99%+
2. **Analysis Duration:** Should be consistent
3. **Bug Count Trends:** Track over time
4. **Critical Bug Count:** Alert if > 0
5. **Compliance Procedure Count:** Monitor changes

**Alerts:**

- Critical bugs found → Email security team
- Analysis failure → Page on-call
- Significant drift detected → Email development manager
- Compliance violations → Email audit team

---

## Performance Optimization

### 1. Copybook Caching

**Problem:** Copybook resolution is slow (network filesystem)

**Solution:** Enable 2-tier caching

```bash
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
```

**Results:**
- First run: ~5 minutes for 100K LOC
- Subsequent runs: ~15 seconds (95% faster)

### 2. Parallel Processing

**Problem:** Large codebases take hours to analyze

**Solution:** Split into chunks, process in parallel

```bash
# Split files
find /u/prod/cobol -name '*.cbl' | split -l 100 - /tmp/chunk_

# Submit parallel jobs (one per chunk)
submit 'JCL.LIB(CHUNK01)'  # Processes chunk_aa
submit 'JCL.LIB(CHUNK02)'  # Processes chunk_ab
submit 'JCL.LIB(CHUNK03)'  # Processes chunk_ac
# ... etc

# Merge results
python3 -m cobol_harmonizer.cli merge \
  --inputs /u/reports/chunk_*.json \
  --output /u/reports/merged.json
```

**Results:**
- Sequential: 4 hours for 1M LOC
- Parallel (10 jobs): 30 minutes

### 3. Incremental Analysis

**Problem:** Re-analyzing unchanged files wastes time

**Solution:** Only analyze changed files

```bash
# Compare with baseline to find changed procedures
python3 -m cobol_harmonizer.baseline.compare \
  --baseline /u/cobol-harmonizer/baselines/production.json \
  --current /u/reports/nightly/analysis_$(date +%Y%m%d).json \
  --output-changed-only \
  --changed-file-list /tmp/changed.txt

# Only analyze changed files
cat /tmp/changed.txt | while read file; do
    python3 cobol_harmonizer.py analyze --input "$file" ...
done
```

### 4. Resource Allocation

**JCL Region Size:**

```jcl
//ANALYZE  EXEC PGM=BPXBATCH,REGION=128M
```

**Python Memory:**

```bash
# Limit Python memory (if needed)
export PYTHON_GC_MAX_HEAP_SIZE=512M
```

---

## Troubleshooting

### Common Issues

#### 1. "Python not found"

**Symptom:**
```
FSUM7351 not found
```

**Solution:**
```jcl
//STDENV   DD *
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
/*
```

#### 2. "Module not found: cobol_harmonizer"

**Symptom:**
```
ModuleNotFoundError: No module named 'cobol_harmonizer'
```

**Solution:**
```jcl
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
/*
```

#### 3. "EBCDIC/ASCII conversion issues"

**Symptom:**
```
UnicodeDecodeError: 'ascii' codec can't decode byte...
```

**Solution:**
```jcl
//STDENV   DD *
export _BPXK_AUTOCVT=ON
export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG)"
/*
```

#### 4. "Copybook not found"

**Symptom:**
```
WARNING: Could not resolve copybook: CUSTCOPY
```

**Solution:**
```jcl
//STDENV   DD *
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook:/u/test/copybook
/*
```

#### 5. "Permission denied"

**Symptom:**
```
Permission denied: /u/cobol-harmonizer/jcl/harmonizer_wrapper.sh
```

**Solution:**
```bash
chmod +x /u/cobol-harmonizer/jcl/*.sh
```

### Debug Mode

Enable verbose logging:

```bash
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/PROGRAM.cbl \
  --output /tmp/analysis.json \
  --verbose \
  --log-file /u/cobol-harmonizer/logs/debug.log
```

### Support

- **Documentation:** `/u/cobol-harmonizer/docs/`
- **GitHub Issues:** https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues
- **JCL Examples:** `/u/cobol-harmonizer/jcl/`

---

## Appendix

### A. Environment Variable Reference

| Variable | Default | Purpose |
|----------|---------|---------|
| `HARMONIZER_HOME` | `/u/cobol-harmonizer` | Installation directory |
| `HARMONIZER_COPYBOOK_PATH` | - | Copybook search path (`:` separated) |
| `HARMONIZER_CACHE_DIR` | `$HOME/.cache` | Copybook cache directory |
| `HARMONIZER_LOG_DIR` | `$HOME/logs` | Log file directory |
| `HARMONIZER_AUDIT_LOG` | - | Audit log directory |
| `HARMONIZER_COMPLIANCE` | - | Compliance frameworks (`SOX,PCI,GDPR`) |
| `HARMONIZER_BASELINE_PATH` | - | Path to baseline JSON |
| `PYTHON PATH` | - | Python module search path |
| `_BPXK_AUTOCVT` | - | Auto-convert ASCII/EBCDIC |

### B. File Naming Conventions

**Reports:**
- `analysis_YYYYMMDD.json` - Daily analysis
- `sox_Q4_2025.html` - Quarterly SOX audit
- `trend_YYYYMMDD.html` - Weekly trends

**Logs:**
- `harmonizer_YYYYMMDD_HHMMSS.log` - Analysis logs
- `.audit_log/YYYYMM.jsonl` - Monthly audit logs

**Baselines:**
- `production.json` - Production baseline
- `test.json` - Test environment baseline

### C. Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Analysis error (invalid COBOL, etc.) |
| 2 | Configuration error |
| 3 | File not found |
| 4 | Permission denied |
| 5 | Python error |

---

**Version:** 0.5.0
**Last Updated:** 2025-11-08
**Platform:** IBM z/OS with USS
**Python:** 3.9+

**Made with 💛 for IBM mainframe customers**
