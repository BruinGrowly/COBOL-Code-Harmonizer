# IBM Quick Start Guide
## COBOL Code Harmonizer for IBM z/OS

**Get from zero to analyzing COBOL in under 5 minutes.**

---

## Table of Contents

1. [Prerequisites Check](#prerequisites-check) (30 seconds)
2. [Installation](#installation) (2 minutes)
3. [First Analysis](#first-analysis) (1 minute)
4. [Next Steps](#next-steps)

---

## Prerequisites Check

**Already have these? [Skip to Installation](#installation)**

### ✅ Checklist

```bash
# SSH to your z/OS USS environment
ssh your-userid@mainframe.company.com

# Check Python version (need 3.9+)
python3 --version
# ✓ Should show: Python 3.9.x or higher

# Check you can write to /u directory
cd /u
mkdir -p test-cobol-harmonizer
cd test-cobol-harmonizer
pwd
# ✓ Should show: /u/test-cobol-harmonizer

# You're ready!
```

**Don't have Python 3.9+?**
```bash
# IBM Python is usually at:
/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3 --version

# Set it as default:
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
echo 'export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH' >> ~/.profile
```

---

## Installation

### Option A: Quick Install (Recommended)

```bash
# 1. Create directory
mkdir -p /u/cobol-harmonizer
cd /u/cobol-harmonizer

# 2. Download (choose one method):

## Method 1: Git (if available)
git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git .

## Method 2: FTP from your workstation
# On workstation:
# - Download zip from GitHub
# - Extract locally
# - FTP to mainframe (binary mode)

## Method 3: Direct download (if curl/wget available)
curl -L https://github.com/BruinGrowly/COBOL-Code-Harmonizer/archive/main.zip -o harmonizer.zip
unzip harmonizer.zip
mv COBOL-Code-Harmonizer-main/* .

# 3. Make executable
chmod +x cobol_harmonizer.py
chmod +x jcl/*.sh

# 4. Verify installation
python3 cobol_harmonizer.py --version
# ✓ Should show: COBOL Code Harmonizer v0.5.0

# Done! Takes ~2 minutes
```

### Option B: Minimal Install (Core Only)

If you only need core analysis (no compliance, no JCL):

```bash
# Just copy these files:
# - cobol_harmonizer/ (entire directory)
# - cobol_harmonizer.py
# - examples/ (for testing)

# Verify
python3 cobol_harmonizer.py --help
```

---

## First Analysis

### Step 1: Analyze a Sample Program (30 seconds)

```bash
cd /u/cobol-harmonizer

# Analyze the included example
python3 cobol_harmonizer.py analyze \
  --input examples/disharmonious_example.cbl \
  --output /tmp/my-first-analysis.json \
  --verbose
```

**Expected Output:**
```
🔍 COBOL Code Harmonizer v0.5.0
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📂 Analyzing: examples/disharmonious_example.cbl
⏱️  Parsed in 0.023s

📊 Analysis Results
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ File: examples/disharmonious_example.cbl
  Procedures analyzed: 5
  Semantic issues found: 3

⚠️  MEDIUM: VALIDATE-CUSTOMER-RECORD
    Disharmony: 0.75
    Intent: Justice (Validation)
    Execution: Power (Modification)
    → Procedure name implies validation but modifies data!

⚠️  MEDIUM: END-READ
    Disharmony: 0.64
    Intent: Wisdom (Retrieval)
    Execution: Power (Modification)
    → Procedure name implies read but deletes records!

✅ Report saved: /tmp/my-first-analysis.json
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Step 2: View the Results

```bash
# View JSON report
cat /tmp/my-first-analysis.json | head -50

# Or convert to HTML for visual dashboard
python3 cobol_harmonizer.py convert-report \
  --input /tmp/my-first-analysis.json \
  --output /tmp/my-first-analysis.html \
  --format html

# Download HTML to your workstation to view in browser
```

### Step 3: Analyze YOUR COBOL Program

```bash
# Point to your actual COBOL file
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/YOUR-PROGRAM.cbl \
  --output /u/reports/analysis-$(date +%Y%m%d).json \
  --verbose

# With copybook resolution
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/YOUR-PROGRAM.cbl \
  --copybook-path /u/prod/copybook \
  --output /u/reports/analysis-$(date +%Y%m%d).json

# With compliance checking
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/YOUR-PROGRAM.cbl \
  --compliance sox,pci_dss,gdpr \
  --output /u/reports/analysis-$(date +%Y%m%d).json
```

**🎉 Congratulations! You've analyzed your first COBOL program!**

---

## Next Steps

### For Development Teams

**Integrate with Your Workflow:**

```bash
# 1. Add to git pre-commit hook
# .git/hooks/pre-commit
#!/bin/bash
python3 /u/cobol-harmonizer/cobol_harmonizer.py analyze \
  --input . \
  --threshold 0.5 \
  --exit-code

# 2. Add to CI/CD pipeline
# See .github/workflows/ci.yml for examples
```

### For Compliance Teams

**Run SOX Audit:**

```bash
# Submit JCL for quarterly audit
# Edit jcl/SOXAUDIT.jcl with your paths
# Then submit:
submit 'YOUR.JCL.LIB(SOXAUDIT)'

# Or run manually:
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/financial \
  --compliance sox \
  --output /u/reports/sox-audit-Q4-2025.json \
  --format html
```

### For Management

**Generate Executive Dashboard:**

```bash
# Run enterprise analysis
python3 enterprise_analyzer.py \
  --input /u/prod/cobol \
  --output /u/reports/enterprise-analysis.json

# View metrics
cat /u/reports/enterprise-analysis.json | grep -A 5 "summary"

# Get ROI estimate
python3 -c "
import json
with open('/u/reports/enterprise-analysis.json') as f:
    data = json.load(f)
    cost_min = data['summary']['estimated_cost_savings']['minimum']
    print(f'Estimated bugs prevented: \${cost_min:,}')
"
```

---

## Common Tasks

### Task 1: Analyze Entire Directory

```bash
# Analyze all COBOL files in directory
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --output /u/reports/batch-analysis.json \
  --recursive

# Takes ~2 seconds per 1,000 LOC
```

### Task 2: Compare Before/After Changes

```bash
# 1. Save baseline before changes
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/PROGRAM.cbl \
  --output /tmp/baseline.json

# 2. Make your changes to PROGRAM.cbl

# 3. Analyze again
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol/PROGRAM.cbl \
  --output /tmp/current.json

# 4. Compare
python3 cobol_harmonizer.py compare \
  --baseline /tmp/baseline.json \
  --current /tmp/current.json \
  --output /tmp/change-impact.json
```

### Task 3: Schedule Nightly Scans

```bash
# 1. Edit jcl/NIGHTLY.jcl with your paths

# 2. Add to TWS/OPC scheduler
# Schedule: Daily at 02:00 AM

# 3. Submit test run
submit 'YOUR.JCL.LIB(NIGHTLY)'

# 4. Check results
cat /u/reports/nightly/analysis-$(date +%Y%m%d).json
```

### Task 4: Generate Reports for Auditors

```bash
# JSON report (machine-readable)
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --compliance sox,pci_dss \
  --output /u/reports/audit-report.json

# HTML dashboard (human-readable)
python3 cobol_harmonizer.py convert-report \
  --input /u/reports/audit-report.json \
  --output /u/reports/audit-dashboard.html \
  --format html

# CSV for Excel (data analysis)
python3 cobol_harmonizer.py convert-report \
  --input /u/reports/audit-report.json \
  --output /u/reports/audit-data.csv \
  --format csv

# Download all three to your workstation
```

---

## Troubleshooting

### Issue: "Python not found"

```bash
# Find Python installation
find /usr -name python3 2>/dev/null

# Common locations:
# - /usr/lpp/IBM/cyp/v3r9/pyz/bin/python3 (IBM Python)
# - /usr/bin/python3 (system Python)

# Add to PATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
```

### Issue: "Module not found"

```bash
# Set PYTHONPATH
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH

# Add to profile
echo 'export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH' >> ~/.profile
```

### Issue: "Permission denied"

```bash
# Make scripts executable
cd /u/cobol-harmonizer
chmod +x cobol_harmonizer.py
chmod +x jcl/*.sh

# Check ownership
ls -la cobol_harmonizer.py
# If not owned by you: contact system admin
```

### Issue: "Copybook not found"

```bash
# Specify copybook search paths (colon-separated)
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook:/u/test/copybook

# Or use command-line flag
python3 cobol_harmonizer.py analyze \
  --input program.cbl \
  --copybook-path /u/prod/copybook:/u/test/copybook \
  --output analysis.json
```

### Issue: "Analysis is slow"

```bash
# Enable caching (95% speedup on subsequent runs)
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
mkdir -p /u/cobol-harmonizer/.cache

# Use parallel processing for batch analysis
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --max-workers 4 \
  --output analysis.json
```

### Issue: "Out of memory"

```bash
# Analyze in smaller batches
# Instead of entire directory, analyze 100 files at a time

# Find first 100 files
find /u/prod/cobol -name "*.cbl" | head -100 > /tmp/batch1.txt

# Analyze batch
python3 cobol_harmonizer.py analyze \
  --file-list /tmp/batch1.txt \
  --output /u/reports/batch1.json
```

---

## JCL Quick Start

### Quick JCL Template

Copy this to `YOUR.JCL.LIB(HARMONIZ)`:

```jcl
//HARMONIZ JOB (ACCT),'COBOL ANALYSIS',CLASS=A,MSGCLASS=X
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
   --input /u/prod/cobol/YOUR-PROGRAM.cbl \
   --output /u/reports/analysis-$(date +%Y%m%d).json \
   --verbose
/*
//
```

**Submit:**
```
submit 'YOUR.JCL.LIB(HARMONIZ)'
```

**Check output:**
```
ST
OUTPUT your-userid
```

---

## Integration Examples

### With IBM Developer for z/OS (RAD)

```bash
# 1. Generate CSV report
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --output /u/reports/analysis.csv \
  --format csv

# 2. Download to workstation
scp userid@mainframe:/u/reports/analysis.csv ./

# 3. In RAD: File → Import → CSV
# Map columns: procedure → Name, severity → Severity, etc.

# 4. Markers appear in RAD source editor
```

### With DB2

```bash
# 1. Create DB2 tables (one-time setup)
# See docs/IBM_MAINFRAME_INTEGRATION.md for schema

# 2. Run analysis with DB2 archival
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --output /u/reports/analysis.json \
  --db2-archive

# 3. Query historical data
db2 "SELECT * FROM HARMONIZER.ANALYSIS_RESULTS
     WHERE ANALYSIS_DATE >= CURRENT_DATE - 90 DAYS
     ORDER BY ANALYSIS_DATE DESC"
```

### With Git

```bash
# 1. Add pre-commit hook
cd /u/your-git-repo
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Analyze changed COBOL files
changed_cobol=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.cbl$|\.cob$')

if [ -n "$changed_cobol" ]; then
    echo "Analyzing changed COBOL files..."
    for file in $changed_cobol; do
        python3 /u/cobol-harmonizer/cobol_harmonizer.py analyze \
            --input "$file" \
            --threshold 0.8 \
            --exit-code || exit 1
    done
fi
EOF

chmod +x .git/hooks/pre-commit

# 2. Now commits will be blocked if critical bugs found
```

---

## Performance Expectations

### What to Expect

| Codebase Size | Analysis Time | Throughput |
|--------------|---------------|------------|
| 100 LOC | <0.1 seconds | Instant |
| 1,000 LOC | ~2 seconds | 500 LOC/sec |
| 10,000 LOC | ~20 seconds | 500 LOC/sec |
| 100,000 LOC | ~3 minutes | 555 LOC/sec |
| 1,000,000 LOC | ~30 minutes | 555 LOC/sec |

**With Caching Enabled:** 95% faster on subsequent runs

**With Parallel Processing:** Linear speedup (4 workers = 4x faster)

### Optimization Tips

```bash
# 1. Enable caching (first time)
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache

# 2. Use parallel processing
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --max-workers 4 \
  --output analysis.json

# 3. Incremental analysis (only changed files)
python3 cobol_harmonizer.py analyze \
  --input /u/prod/cobol \
  --incremental \
  --baseline /u/cobol-harmonizer/baselines/production.json \
  --output analysis.json
```

---

## Getting Help

### Documentation

- **Full Documentation:** [IBM_MAINFRAME_INTEGRATION.md](docs/IBM_MAINFRAME_INTEGRATION.md)
- **JCL Examples:** [jcl/README.md](jcl/README.md)
- **Proof of Value:** [IBM_PROOF_OF_VALUE.md](IBM_PROOF_OF_VALUE.md)
- **Customer Case Studies:** [IBM_CUSTOMER_CASE_STUDIES.md](IBM_CUSTOMER_CASE_STUDIES.md)

### Command-Line Help

```bash
# General help
python3 cobol_harmonizer.py --help

# Command-specific help
python3 cobol_harmonizer.py analyze --help
python3 cobol_harmonizer.py compare --help
python3 cobol_harmonizer.py convert-report --help
```

### Support

- **GitHub Issues:** https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues
- **GitHub Discussions:** https://github.com/BruinGrowly/COBOL-Code-Harmonizer/discussions

---

## Quick Reference Card

### Essential Commands

```bash
# Analyze single file
python3 cobol_harmonizer.py analyze -i FILE.cbl -o report.json

# Analyze directory
python3 cobol_harmonizer.py analyze -i /u/prod/cobol -o report.json

# With compliance
python3 cobol_harmonizer.py analyze -i FILE.cbl --compliance sox,pci -o report.json

# Convert to HTML
python3 cobol_harmonizer.py convert-report -i report.json -o report.html -f html

# Compare versions
python3 cobol_harmonizer.py compare --baseline old.json --current new.json -o diff.json
```

### Environment Variables

```bash
# Required
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH

# Optional but recommended
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook:/u/test/copybook
export _BPXK_AUTOCVT=ON  # Auto EBCDIC/ASCII conversion

# For compliance
export HARMONIZER_COMPLIANCE=SOX,PCI_DSS,GDPR,HIPAA
export HARMONIZER_AUDIT_LOG=/u/cobol-harmonizer/.audit_log
```

### File Locations

```
/u/cobol-harmonizer/              # Installation directory
├── cobol_harmonizer.py           # Main script
├── cobol_harmonizer/             # Core modules
├── jcl/                          # JCL templates
├── examples/                     # Sample COBOL programs
├── docs/                         # Documentation
└── .cache/                       # Performance cache

/u/reports/                       # Your analysis reports
├── nightly/                      # Nightly scans
├── sox/                          # SOX audits
└── trends/                       # Trend analysis
```

---

## Success Checklist

After following this guide, you should be able to:

- ✅ Run `python3 cobol_harmonizer.py --version`
- ✅ Analyze a sample COBOL program
- ✅ Generate JSON/HTML/CSV reports
- ✅ Submit JCL batch jobs
- ✅ Integrate with your development workflow
- ✅ Understand the results and take action

**If you can do all of the above, you're ready for production!** 🎉

---

## What's Next?

### Week 1: Pilot

- [ ] Analyze 3-5 critical programs manually
- [ ] Review findings with development team
- [ ] Establish baseline for those programs
- [ ] Calculate ROI from bugs found

### Week 2: Integration

- [ ] Set up JCL batch jobs
- [ ] Configure compliance rules (if needed)
- [ ] Enable audit logging
- [ ] Integrate with CI/CD pipeline

### Week 3: Automation

- [ ] Schedule nightly scans
- [ ] Set up DB2 archival (optional)
- [ ] Create executive dashboards
- [ ] Train team on results interpretation

### Week 4: Production

- [ ] Full codebase analysis
- [ ] Establish semantic baselines
- [ ] Set up alerting for new issues
- [ ] Document process for ongoing use

---

**Questions? Issues? Feedback?**

Open a GitHub issue: https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues

**Happy analyzing!** 🚀

---

*COBOL Code Harmonizer - Making COBOL codebases understandable, one procedure at a time.*

**Version:** 0.5.0
**Platform:** IBM z/OS with USS
**License:** Free for IBM customers
**Last Updated:** 2025-11-08
