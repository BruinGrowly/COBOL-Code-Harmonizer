# COBOL Code Harmonizer

**Semantic analysis for COBOL using the LJPW framework**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![IBM Z Verified](https://img.shields.io/badge/IBM_Z-Verified-blue?logo=ibm)](ibm/ZOS_QUICKSTART.md)
[![z/OS USS Ready](https://img.shields.io/badge/z/OS-USS_Ready-green?logo=ibm)](docs/IBM_MAINFRAME_INTEGRATION.md)
[![SARIF for ADDI](https://img.shields.io/badge/ADDI-SARIF_Compatible-purple?logo=ibm)](ibm/ADDI_PLUGIN.md)
[![Works with Wazi](https://img.shields.io/badge/IBM_Wazi-Compatible-darkblue?logo=visualstudiocode)](ibm/WAZI_DEMO.md)

> *"Does your COBOL procedure DO what its name SAYS it does?"*

---

## What Is This?

COBOL Code Harmonizer detects **semantic bugs** in COBOL codebases - when procedure names contradict their actual behavior.

### The Problem

```cobol
GET-CUSTOMER-RECORD.
    READ CUSTOMER-FILE.
    DELETE CUSTOMER-FILE RECORD.  *> Wait, we're DELETING?!
    MOVE CUSTOMER-DATA TO WS-RESULT.
```

**The Bug**: Function name says "GET" (retrieve) but code actually DELETES the record. This is a **semantic bug** - the code lies about its purpose, causing production issues that traditional tools miss.

### The Solution

COBOL Code Harmonizer uses the **LJPW framework** - a mathematically proven semantic model - to analyze your COBOL code and detect these contradictions.

---

## Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git
cd COBOL-Code-Harmonizer

# Install dependencies
pip install -r requirements.txt

# Install the package
pip install -e .
```

### Basic Usage

```bash
# Analyze a COBOL file
python -m cobol_harmonizer.cli analyze examples/disharmonious_example.cbl

# Example output:
# ┌───────────────────────────────────────────────────────────────┐
# │ GET-CUSTOMER-BALANCE                          💥 CRITICAL    │
# ├───────────────────────────────────────────────────────────────┤
# │ Disharmony Score: 1.12                                        │
# │                                                                │
# │ Intent (from name):     Wisdom-dominant (retrieval)          │
# │ Execution (actual):     Power-dominant (deletion)            │
# │                                                                │
# │ ⚠️  CRITICAL: Function name says GET but actually DELETES!    │
# │                                                                │
# │ Suggestions:                                                   │
# │   → DELETE-CUSTOMER-BALANCE (95% match)                       │
# │   → REMOVE-CUSTOMER-BALANCE (92% match)                       │
# └───────────────────────────────────────────────────────────────┘
```

---

## VS Code Extension (NEW! v0.1.0) 🚀

**Real-time semantic analysis directly in your editor!**

The COBOL Harmonizer VS Code extension brings semantic analysis into your development workflow with:

- ✅ **Real-time Analysis** - Analyze on file save
- ✅ **Inline Diagnostics** - See disharmony warnings directly in your code
- ✅ **Color-coded Severity** - Error (≥1.2), Warning (0.8-1.2), Info (0.5-0.8)
- ✅ **Workspace Analysis** - Analyze entire COBOL codebase with one command
- ✅ **IBM z/OS Ready** - Works with USS environments
- ✅ **Copybook Support** - Configurable copybook paths

### Quick Install

```bash
cd vscode-extension
npm install -g vsce
vsce package
code --install-extension cobol-harmonizer-0.1.0.vsix
```

**Documentation:**
- [Extension README](vscode-extension/README.md) - Features and usage
- [Installation Guide](vscode-extension/INSTALL.md) - Complete setup instructions
- [Testing Guide](vscode-extension/TESTING.md) - Quality assurance

---

## IBM Enterprise Ready 🔵

**Deploy to IBM z/OS USS in 5 minutes.** Fully integrated with IBM toolchain.

### Quick Deploy to z/OS

```bash
# 1. Upload z/OS bundle to USS
ftp your.zos.host
> bin
> put cobol-harmonizer-zos-*.tar.gz

# 2. Extract and install
gunzip cobol-harmonizer-zos-*.tar.gz
tar -xf cobol-harmonizer-zos-*.tar
cd cobol-harmonizer-zos-*
./install_zos.sh

# 3. Analyze COBOL
python3 -m cobol_harmonizer.cli.commands analyze MYPROG.cbl --verbose
```

📚 **Complete Guides:**
- **[5-Minute z/OS Quick Start](ibm/ZOS_QUICKSTART.md)** - Get running on USS now
- **[IBM ADDI Integration](ibm/ADDI_PLUGIN.md)** - Plug into ADDI in minutes
- **[IBM Wazi Demo Script](ibm/WAZI_DEMO.md)** - Live demo for sales
- **[Full Mainframe Integration](docs/IBM_MAINFRAME_INTEGRATION.md)** - Complete deployment guide

### IBM Tool Integrations

| Tool | Status | Quick Link |
|------|--------|------------|
| **IBM z/OS USS** | ✅ Production Ready | [Quick Start](ibm/ZOS_QUICKSTART.md) |
| **IBM ADDI** | ✅ Plugin Available | [ADDI Guide](ibm/ADDI_PLUGIN.md) |
| **IBM Wazi Developer** | ✅ VS Code Compatible | [Wazi Demo](ibm/WAZI_DEMO.md) |
| **IBM Debug for z/OS** | ✅ SARIF Import | [Integration Docs](docs/IBM_MAINFRAME_INTEGRATION.md) |
| **SonarQube for z/OS** | ✅ SARIF Compatible | Output: `--format sarif` |

### JCL Templates Included

```jcl
// Submit directly to z/OS
//HARMONIZ JOB (ACCT),'COBOL ANALYSIS'
//ANALYZE  EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH /u/harmonizer/jcl/harmonizer_wrapper.sh \
   /u/prod/cobol/MYPROG.cbl
/*
```

📦 **Pre-built JCL**: `jcl/HARMONIZ.jcl`, `jcl/NIGHTLY.jcl`, `jcl/SOXAUDIT.jcl`

### Container Support

```bash
# Build z/OS-compatible container
docker build -f Dockerfile.zos -t cobol-harmonizer:zos .

# Run analysis
docker run --rm \
  -v /u/prod/cobol:/input:ro \
  -v /u/reports:/output \
  cobol-harmonizer:zos analyze /input/MYPROG.cbl
```

### Release Automation

Every GitHub release includes:
- ✅ z/OS USS deployment bundle (`.tar.gz`)
- ✅ ZIP format for IBM shops
- ✅ SHA-256 checksums
- ✅ Install script (`install_zos.sh`)
- ✅ Quick start guide

Download latest: **[Releases](https://github.com/BruinGrowly/COBOL-Code-Harmonizer/releases)**

---

## Enterprise Features (v0.4.0 - Phase 4A) 🚀

### Copybook Resolution

Automatically resolves and inlines COBOL copybooks:

```python
from cobol_harmonizer.copybook import CopybookConfig, CopybookResolver

config = CopybookConfig(
    search_paths=['./copybooks', './lib'],
    enable_cache=True,
)

resolver = CopybookResolver(config)
resolved = resolver.resolve_file('CUSTOMER-PROC.cbl')

print(f"✓ Resolved {len(resolved.copybooks_used)} copybooks in {resolved.resolution_time_ms:.1f}ms")
```

**Features:**
- ⚡ Fast resolution (2-4ms for typical programs)
- 💾 Two-tier caching (memory + disk)
- 🔄 Recursive resolution (handles nested COPY statements)
- 📍 Source mapping (tracks original line numbers)
- 🔍 Circular dependency detection

### Call Graph Analysis

Analyzes program dependencies and call relationships:

```python
from cobol_harmonizer.callgraph import CallExtractor, CallGraphBuilder, CallGraphAnalyzer

# Extract calls
extractor = CallExtractor()
call_sites = extractor.extract_from_file('ACCOUNT-PROC.cbl')

# Build graph
builder = CallGraphBuilder()
graph = builder.build(call_sites)

# Analyze impact
analyzer = CallGraphAnalyzer(graph)
analysis = analyzer.analyze_impact('PROGRAM.VALIDATE-ACCOUNT')

print(f"Risk: {analysis.risk_level} ({analysis.risk_score:.1f}/100)")
print(f"Impact: {analysis.total_impact} nodes affected")
```

**Features:**
- 📞 CALL statement extraction (inter-program)
- 🔄 PERFORM statement extraction (intra-program)
- 🎯 Impact analysis (what's affected by changes)
- ⚠️ Risk assessment (LOW, MEDIUM, HIGH, CRITICAL)
- 💀 Dead code detection
- 🔁 Circular dependency detection
- 🔥 Hot spot analysis
- 📊 Visualization (GraphViz, JSON, ASCII)

**Demo:**
```bash
# Copybook resolution demo
python demo_copybook_resolution.py

# Call graph analysis demo
python demo_callgraph.py

# Integrated demo (both features)
python demo_integrated.py
```

---

## Compliance & Audit Features (v0.5.0 - NEW!) 🔒

### For Financial Institutions, Healthcare, and Regulated Industries

**Comprehensive audit and compliance tracking for SOX, PCI-DSS, GDPR, HIPAA, and more.**

#### Audit Logging

Track all analysis activities with full traceability:

```python
from cobol_harmonizer.compliance import AuditLogger, AuditEvent, AuditAction

logger = AuditLogger()

# Log analysis with justification
event = AuditEvent(AuditAction.ANALYSIS)
event.with_file("BANKACCT.cbl")
event.with_procedure("PROCESS-TRANSACTION")
event.with_justification("Q4 2025 SOX compliance review")

logger.log(event)
```

**Features:**
- 📝 Persistent audit trail (JSONL format)
- 👤 User and environment tracking
- 🔍 Query capabilities (by user, date, file, action)
- 🗄️ Configurable retention (default: 365 days)

#### Compliance Tagging

Automatically identifies SOX, PCI-DSS, GDPR-relevant procedures:

```python
from cobol_harmonizer.compliance import ComplianceTagger

tagger = ComplianceTagger()
tags = tagger.tag_procedure(
    "PROCESS-CREDIT-CARD",
    verbs=["READ", "WRITE", "ENCRYPT"],
    files_accessed=["CARD-FILE"]
)
# Result: {pci_cardholder_data, payment_processing, encryption}
```

**Supported Frameworks:**
- **SOX** - Financial reporting, access control, audit trails
- **PCI-DSS** - Payment card data, encryption
- **GDPR** - Personal data, right to erasure, data export
- **HIPAA** - Protected health information
- **ISO 27001, NIST, FISMA** - Security standards

#### Risk Assessment

Combines semantic disharmony with compliance criticality:

```python
from cobol_harmonizer.compliance import ComplianceRiskAssessor

assessor = ComplianceRiskAssessor()
risk = assessor.assess_procedure(
    procedure_name="PROCESS-PAYMENT",
    file_path="payment.cbl",
    disharmony_score=0.65,
    disharmony_level="concerning",
    fan_in=10
)

print(f"Risk Level: {risk.risk_level.value}")  # CRITICAL/HIGH/MEDIUM/LOW
print(f"Risk Score: {risk.risk_score}/100")
# Compliance tags, warnings, and recommendations included
```

**Risk Calculation (0-100):**
- 40% Semantic disharmony
- 30% Compliance criticality
- 20% Call graph impact
- 10% Compliance tagging

#### Compliance Reports

Generate audit-ready reports in multiple formats:

```python
from cobol_harmonizer.compliance import ComplianceReporter

reporter = ComplianceReporter()
report = reporter.generate_report(
    assessments=risk_assessments,
    report_type="compliance",
    generated_by="Compliance Team"
)

# Export to multiple formats
reporter.export_html(report, "compliance_report.html")
reporter.export_json(report, "compliance_report.json")
reporter.export_markdown(report, "compliance_report.md")
reporter.export_csv(report, "violations.csv")
```

**Report Contents:**
- Executive summary
- Compliance framework status (SOX compliant? PCI compliant?)
- Critical violations with recommendations
- High-risk procedures
- Audit trail
- Baseline deviations

**Demo:**
```bash
# Run compliance features demo
python demo_compliance.py
```

**Documentation:**
- See [docs/COMPLIANCE_FEATURES.md](docs/COMPLIANCE_FEATURES.md) for complete guide
- Example configuration in `.compliance_config.json`

---

## Advanced Features (v0.3.0)

### Batch Analysis

Analyze entire directories of COBOL files:

```bash
# Analyze all COBOL files in a directory
python -m cobol_harmonizer.cli.commands batch-analyze /path/to/cobol/source

# Recursive analysis with progress tracking
python -m cobol_harmonizer.cli.commands batch-analyze ./mainframe --recursive

# Filter by threshold (only show issues >= 0.8)
python -m cobol_harmonizer.cli.commands batch-analyze ./src --threshold 0.8

# Generate JSON report for entire codebase
python -m cobol_harmonizer.cli.commands batch-analyze ./src --output report.json --format json
```

**Features:**
- Multi-threaded parallel processing
- Progress tracking for large codebases
- Aggregated statistics across all files
- Identification of worst offenders
- File-level and procedure-level rankings

### CI/CD Integration

Export results in industry-standard formats:

```bash
# Generate SARIF report for GitHub Code Scanning
python -m cobol_harmonizer.cli.commands analyze program.cbl \
  --format sarif \
  --output results.sarif

# Upload to GitHub (via Actions)
- uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: results.sarif

# JSON format for custom tooling
python -m cobol_harmonizer.cli.commands analyze program.cbl \
  --format json \
  --output results.json
```

**SARIF Features:**
- GitHub Code Scanning integration
- GitLab Security Dashboard support
- Azure DevOps compatibility
- Automatic severity mapping
- Fix suggestions included

### Configuration Files

Create a `.harmonizerrc.json` in your project:

```json
{
  "analysis": {
    "threshold": 0.5,
    "show_harmonious": false
  },
  "batch": {
    "max_workers": 8,
    "recursive": true,
    "file_patterns": ["*.cbl", "*.CBL", "*.cob"],
    "exclude_patterns": [
      "**/test/**",
      "**/vendor/**"
    ]
  },
  "reporting": {
    "default_format": "json",
    "include_suggestions": true,
    "max_suggestions": 5
  }
}
```

**Configuration Features:**
- JSON or YAML format support
- Automatic discovery (searches parent directories)
- Override via command-line arguments
- Validation with helpful error messages

### Baseline Comparison

Track improvements and detect regressions:

```bash
# Save current state as baseline
python -m cobol_harmonizer.cli.commands save-baseline ./src \
  --name production \
  --description "Production release v1.2.3"

# Compare current state to baseline
python -m cobol_harmonizer.cli.commands compare-baseline ./src \
  --baseline production

# Example output:
# Baseline Comparison Summary
# ══════════════════════════════════════════════════════
# Regressions:  3 📈 (procedures got worse)
# Improvements: 12 📉 (procedures got better)
# Unchanged:    45
#
# Top Regressions:
#   • VALIDATE-INPUT in validate.cbl: 0.3 → 0.9 (+0.6)
#   • PROCESS-DATA in process.cbl: 0.5 → 0.8 (+0.3)
```

**Baseline Features:**
- Save snapshots of codebase health
- Compare current vs. baseline
- Detect regressions (code got worse)
- Identify improvements (code got better)
- Track new/removed procedures
- Generate regression reports

### Codebase Health Mapping

Get a comprehensive view of your legacy codebase:

```bash
# Generate codebase health report
python -m cobol_harmonizer.cli.commands map-codebase ./mainframe

# Example output:
# ══════════════════════════════════════════════════════════════
# COBOL CODEBASE ANALYSIS REPORT
# ══════════════════════════════════════════════════════════════
#
# CODEBASE HEALTH
# ──────────────────────────────────────────────────────────────
# Health Score:      68/100 (Grade: D)
# Harmony Rate:      45.2%
# Action Required:   23.8%
# Assessment:        Poor - High technical debt, refactoring recommended
#
# TOP 5 PROBLEM FILES
# ──────────────────────────────────────────────────────────────
# 1. customer-mgmt.cbl (avg: 1.23, critical: 5)
# 2. billing.cbl (avg: 0.95, critical: 3)
# 3. reports.cbl (avg: 0.87, critical: 2)
#
# RECOMMENDATIONS
# ──────────────────────────────────────────────────────────────
# [CRITICAL] Fix 8 Critical Semantic Bug(s)
#   These procedures have names that severely contradict their
#   implementations and likely represent bugs.
#   → Review and either rename or refactor immediately
#
# [HIGH] Address 15 Significant Disharmony Issue(s)
#   These should be refactored for better maintainability.
#   → Plan refactoring sprint to address these procedures
```

**Codebase Mapper Features:**
- Overall health score (0-100 with letter grade)
- Hotspot identification (worst files/procedures)
- Semantic pattern analysis (common drift patterns)
- Refactoring recommendations with priorities
- Dimension analysis (LJPW usage across codebase)
- File and procedure rankings

---

## How It Works

### The LJPW Framework

Every programming operation can be mapped to four semantic dimensions:

| Dimension | Symbol | Represents | COBOL Examples |
|-----------|--------|------------|----------------|
| **Love** | L | Connection, communication | `CALL`, `MERGE`, `DISPLAY` |
| **Justice** | J | Validation, verification | `IF`, `EVALUATE`, `VALIDATE` |
| **Power** | P | State modification, action | `WRITE`, `DELETE`, `UPDATE` |
| **Wisdom** | W | Information, knowledge | `READ`, `COMPUTE`, `ACCEPT` |

### Analysis Process

1. **Extract Intent** - Parse procedure name (e.g., "GET-CUSTOMER-RECORD")
   - "GET" → Wisdom-dominant (0.1, 0.1, 0.0, 0.8)

2. **Analyze Execution** - Parse procedure body
   - `READ` → Wisdom (0.1, 0.2, 0.0, 0.7)
   - `DELETE` → Power (0.0, 0.2, 0.8, 0.0)
   - Average → (0.05, 0.2, 0.4, 0.35)

3. **Calculate Disharmony** - Euclidean distance in LJPW space
   - Distance = √[(0.1-0.05)² + (0.1-0.2)² + (0.0-0.4)² + (0.8-0.35)²]
   - Score = 0.65 → **CONCERNING**

4. **Report Issues** - Explain the contradiction
   - "Function name suggests Wisdom operations (retrieval) but execution performs Power operations (deletion)"

---

## Real-World Impact

### Why This Matters for COBOL

COBOL runs **billions of lines** of critical business logic in:
- Banking systems
- Insurance companies
- Government agencies
- Airline reservations
- Healthcare systems

**The Technical Debt Problem**:
- Procedures written decades ago
- Evolved beyond original purpose
- Names never updated
- No one dares to refactor
- New developers get confused
- Bugs hide in plain sight

### Example Scenarios

#### Scenario 1: The Hidden Side Effect
```cobol
*> Developer thinks this just displays data
DISPLAY-ACCOUNT-INFO.
    DISPLAY 'Account: ' ACCOUNT-NUMBER.
    MOVE CURRENT-DATE TO LAST-ACCESS-DATE.  *> Side effect!
    REWRITE ACCOUNT-RECORD.                  *> Modifying state!
```
**Impact**: Harmless display function actually modifies database, breaks caching assumptions, causes race conditions.

#### Scenario 2: The Validator That Creates
```cobol
*> Developer thinks this validates existing data
VALIDATE-CUSTOMER-RECORD.
    IF CUST-ID = SPACES
        MOVE DEFAULT-ID TO CUST-ID
        WRITE CUSTOMER-RECORD   *> Creating records?!
    END-IF.
```
**Impact**: Validation creates data, violates separation of concerns, causes duplicate records.

#### Scenario 3: The Getter That Deletes
```cobol
*> Developer thinks this retrieves data
GET-TRANSACTION-HISTORY.
    READ TRANSACTION-FILE.
    DELETE TRANSACTION-FILE RECORD.  *> Data loss!
    MOVE TRANSACTION-DATA TO WS-BUFFER.
```
**Impact**: Critical data loss bug. "GET" suggests read-only, but actually deletes records.

---

## Features

### Current (v0.3.0)

#### Core Analysis ✅
- ✅ Fixed-format COBOL parsing (COBOL-85 standard)
- ✅ 120+ COBOL verbs mapped to LJPW coordinates
- ✅ Intent extraction from procedure names
- ✅ Execution analysis from procedure bodies
- ✅ Disharmony calculation with severity classification
- ✅ Example COBOL programs (harmonious & disharmonious)

#### CLI & Reporting ✅
- ✅ Command-line interface (`analyze`, `report`, `version`, `examples`)
- ✅ Rich console output with color-coded severity levels
- ✅ JSON export for programmatic consumption
- ✅ SARIF export for CI/CD integration (GitHub, GitLab, Azure DevOps)
- ✅ Suggestion engine for better procedure names

#### Enterprise Features ✅
- ✅ **Batch analysis** - Analyze entire directories of COBOL files
- ✅ **Parallel processing** - Multi-threaded analysis for large codebases
- ✅ **Configuration files** - `.harmonizerrc` support (JSON/YAML)
- ✅ **Baseline comparison** - Track improvements and detect regressions
- ✅ **Codebase mapper** - Visualize health metrics and identify hotspots
- ✅ **Progress tracking** - Real-time feedback for long-running analyses

### Roadmap

#### Phase 4A (v0.4.0) - Enterprise Essentials ✅ COMPLETE
- [x] **Copybook resolution** (with caching and source mapping)
- [x] **Call graph generation** (CALL and PERFORM analysis)
- [x] **Impact analysis** (what's affected by changes)
- [x] **Dead code detection** (unreachable procedures)
- [x] **Visualization** (GraphViz DOT, JSON, ASCII)

#### Phase 4B (v0.4.5) - Enhanced Features
- [ ] Enhanced REPLACING clause support
- [ ] Comprehensive test suite
- [ ] Batch performance optimizations
- [ ] Data flow analysis
- [ ] Free-format COBOL support

#### Phase 5 (v0.6.0) - IDE Integration ✅ IN PROGRESS
- [x] **VS Code extension MVP** (v0.1.0 released!)
- [ ] Language Server Protocol (LSP) implementation
- [ ] Real-time as-you-type analysis
- [ ] Quick fixes and code actions
- [ ] IntelliJ IDEA plugin

#### Phase 6 (v1.0.0) - Production Ready
- [ ] Web dashboard
- [ ] Git integration with pre-commit hooks
- [ ] Comprehensive API documentation
- [ ] Performance optimizations for massive codebases
- [ ] Enterprise SSO integration

---

## Project Structure

```
cobol-harmonizer/
├── cobol_harmonizer/           # Main package
│   ├── parser/                 # COBOL parsing
│   │   └── cobol_parser.py     # Fixed-format COBOL parser
│   ├── semantic/               # Semantic analysis
│   │   ├── verb_mapper.py      # COBOL verb → LJPW mapping (120+ verbs)
│   │   ├── intent_extractor.py # Extract intent from procedure names
│   │   ├── execution_analyzer.py # Analyze procedure bodies
│   │   └── disharmony.py       # Calculate semantic distance & severity
│   ├── compliance/             # Compliance & audit (v0.5.0)
│   │   ├── audit_logger.py     # Tamper-evident audit trail
│   │   ├── compliance_tagger.py # SOX/PCI/GDPR/HIPAA tagging
│   │   └── risk_assessor.py    # Risk scoring (0-100)
│   ├── reporter/               # Output generation
│   │   ├── console_reporter.py # Rich terminal output
│   │   ├── json_reporter.py    # JSON export
│   │   └── sarif_reporter.py   # SARIF export for CI/CD
│   ├── cli/                    # Command-line interface
│   │   └── commands.py         # CLI commands (analyze, report, etc.)
│   ├── batch_analyzer.py       # Batch analysis for directories
│   ├── config.py               # Configuration file support
│   ├── baseline.py             # Baseline comparison & regression tracking
│   ├── codebase_mapper.py      # Legacy codebase health mapping
│   └── utils/                  # Utilities
├── vscode-extension/           # VS Code extension (v0.1.0)
│   ├── package.json            # Extension manifest
│   ├── extension.js            # Main extension code
│   ├── README.md              # Extension documentation
│   ├── INSTALL.md             # Installation guide
│   └── TESTING.md             # Testing procedures
├── jcl/                        # IBM mainframe JCL templates
│   ├── HARMONIZ.jcl           # Batch analysis JCL
│   ├── SOXAUDIT.jcl           # SOX compliance audit
│   ├── NIGHTLY.jcl            # Scheduled scans
│   └── harmonizer_wrapper.sh  # Shell wrapper
├── tests/                      # Test suite (pytest)
│   ├── test_verb_mapper.py
│   ├── test_disharmony.py
│   ├── test_json_reporter.py
│   ├── test_batch_analyzer.py
│   ├── test_performance.py    # Performance benchmarks
│   └── test_compliance.py     # Compliance feature tests
├── examples/                   # Example COBOL programs
│   ├── harmonious_example.cbl  # Well-aligned procedures
│   └── disharmonious_example.cbl # Semantic bugs demonstration
├── docs/                       # Comprehensive documentation
│   ├── ARCHITECTURE.md         # System architecture diagrams
│   ├── IBM_QUICK_START.md      # 5-minute IBM guide
│   ├── IBM_MAINFRAME_INTEGRATION.md # z/OS USS integration
│   ├── IBM_PROOF_OF_VALUE.md   # ROI and proof of value
│   ├── COMPLIANCE_FEATURES.md  # Compliance documentation
│   ├── COBOL_SEMANTICS.md      # Complete COBOL-LJPW mapping
│   ├── PHILOSOPHY.md           # LJPW framework philosophy
│   ├── MATHEMATICAL_FOUNDATION.md # Mathematical proofs
│   └── PRACTICAL_GUIDE.md      # Real-world usage patterns
├── requirements.txt            # Python dependencies
├── setup.py                    # Package setup
├── demo.py                     # Interactive demonstration
├── demo_compliance.py          # Compliance features demo
├── simple_test.py              # Quick smoke test
└── README.md                   # This file
```

---

## Sister Projects

COBOL Code Harmonizer is part of the **Code Harmonizer** family:

- **[Python Code Harmonizer](https://github.com/BruinGrowly/Python-Code-Harmonizer)** - Semantic analysis for Python
- **[JavaScript Code Harmonizer](https://github.com/BruinGrowly/JavaScript-Code-Harmonizer)** - Semantic analysis for JavaScript/TypeScript
- **COBOL Code Harmonizer** (this project) - Semantic analysis for COBOL

All built on the same **LJPW mathematical framework**.

---

## Documentation

### For Users
- **[Quick Start Guide](docs/USER_GUIDE.md)** - Get started in 5 minutes (coming soon)
- **[Practical Guide](docs/PRACTICAL_GUIDE.md)** - Real-world usage patterns
- **[COBOL Semantics](docs/COBOL_SEMANTICS.md)** - COBOL verb mappings

### For Developers
- **[Architecture](docs/ARCHITECTURE.md)** - System design
- **[Contributing](CONTRIBUTING.md)** - How to contribute (coming soon)
- **[API Reference](docs/API_REFERENCE.md)** - Code documentation (coming soon)

### Theory
- **[Philosophy](docs/PHILOSOPHY.md)** - LJPW framework philosophy
- **[Mathematical Foundation](docs/MATHEMATICAL_FOUNDATION.md)** - Proofs and theory
- **[Programming Language Semantics](docs/PROGRAMMING_LANGUAGE_SEMANTICS.md)** - Why this works

---

## Contributing

We welcome contributions! This is a **free, open-source** project addressing a critical gap in COBOL tooling.

### Ways to Contribute

1. **Report Bugs** - Found an issue? [Open an issue](https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues)
2. **Suggest Features** - Have ideas? We'd love to hear them
3. **Improve Documentation** - Help us make docs clearer
4. **Add COBOL Verb Mappings** - Know COBOL dialects we're missing?
5. **Write Tests** - Help us improve coverage
6. **Submit Pull Requests** - Fix bugs or add features

### Development Setup

```bash
# Clone repo
git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git
cd COBOL-Code-Harmonizer

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dev dependencies
pip install -r requirements.txt
pip install -e ".[dev]"

# Run tests
pytest tests/

# Run code quality checks
black cobol_harmonizer/
flake8 cobol_harmonizer/
mypy cobol_harmonizer/
```

---

## License

MIT License - see [LICENSE](LICENSE) file for details.

**Free and open source forever.** This tool exists to help the COBOL community address technical debt.

---

## Acknowledgments

- **LJPW Framework** - Developed by Wellington Taureka
- **Sister Projects** - Python and JavaScript Code Harmonizers for inspiration
- **COBOL Community** - For decades of maintaining critical systems

---

## Contact & Support

- **GitHub Issues**: [Report bugs or request features](https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues)
- **Discussions**: [Join the conversation](https://github.com/BruinGrowly/COBOL-Code-Harmonizer/discussions)
- **Sister Projects**: Check out Python and JavaScript harmonizers for more examples

---

## Why "Harmonizer"?

**Harmony** = alignment between intent and execution

When code is **harmonious**, it says what it does and does what it says. When code is **disharmonious**, it lies about its purpose, causing bugs.

This tool helps you find and fix those lies.

---

**Let's harmonize COBOL, one procedure at a time.** 💛⚓

---

## Quick Reference

### Disharmony Severity Levels

| Score | Level | Symbol | Action |
|-------|-------|--------|--------|
| 0.0 - 0.3 | Harmonious | ✓ | None needed |
| 0.3 - 0.5 | Minor Drift | ⚠️ | Review for clarity |
| 0.5 - 0.8 | Concerning | ⚠️ | Consider renaming |
| 0.8 - 1.2 | Significant | 🔴 | Rename or refactor |
| 1.2+ | Critical | 💥 | Immediate action required |

### LJPW Quick Reference

```
LJPW = (Love, Justice, Power, Wisdom)

Love (L)     = Connection    → CALL, MERGE, DISPLAY
Justice (J)  = Validation    → IF, EVALUATE, VALIDATE
Power (P)    = Modification  → WRITE, DELETE, UPDATE
Wisdom (W)   = Information   → READ, COMPUTE, ACCEPT
```

---

**Made with 💛 for the COBOL community**
