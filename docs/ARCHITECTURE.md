# COBOL Code Harmonizer - Architecture

Complete architecture diagrams for understanding, integrating, and deploying COBOL Code Harmonizer in IBM environments.

---

## System Architecture

### Component Layers

```
┌─────────────────────────────────────────────────────────────┐
│                  COBOL Code Harmonizer v0.5.0                │
├─────────────────────────────────────────────────────────────┤
│ Layer 1: Parser                                             │
│  • COBOL Parser (AST generation)                            │
│  • SQL/CICS Analyzer (embedded statements)                  │
│  • Copybook Resolver (2-tier caching)                       │
├─────────────────────────────────────────────────────────────┤
│ Layer 2: Semantic Analysis                                  │
│  • Intent Extractor (procedure name → LJPW)                 │
│  • Execution Analyzer (COBOL verbs → LJPW)                  │
│  • Disharmony Calculator (Euclidean distance)               │
├─────────────────────────────────────────────────────────────┤
│ Layer 3: Compliance & Risk                                  │
│  • Compliance Tagger (SOX, PCI, GDPR, HIPAA)               │
│  • Risk Assessor (0-100 scoring)                            │
│  • Audit Logger (tamper-evident JSONL)                      │
├─────────────────────────────────────────────────────────────┤
│ Layer 4: Reporting                                          │
│  • Report Engine (JSON, HTML, Markdown, CSV)                │
│  • Baseline Manager (semantic drift detection)              │
│  • Batch Analyzer (parallel processing)                     │
└─────────────────────────────────────────────────────────────┘
```

## Integration Architecture

### IBM z/OS Integration

```
┌──────────────────────────────────────────────────────────┐
│                   IBM z/OS Mainframe                     │
├──────────────────────────────────────────────────────────┤
│  z/OS USS                                                │
│  ├─ Python 3.9+ Runtime                                  │
│  ├─ COBOL Harmonizer (/u/cobol-harmonizer/)             │
│  └─ Analysis Reports (/u/reports/)                       │
├──────────────────────────────────────────────────────────┤
│  JCL Integration (BPXBATCH)                              │
│  ├─ HARMONIZ.jcl (full analysis)                         │
│  ├─ SOXAUDIT.jcl (compliance)                            │
│  └─ NIGHTLY.jcl (scheduled scans)                        │
├──────────────────────────────────────────────────────────┤
│  DB2 Integration                                         │
│  ├─ HARMONIZER.ANALYSIS_RESULTS                          │
│  └─ HARMONIZER.FINDINGS                                  │
└──────────────────────────────────────────────────────────┘
                      ↓
         ┌────────────────────────┐
         │  Developer Workstation │
         ├────────────────────────┤
         │  IBM RAD (CSV import)  │
         │  Web Browser (HTML)    │
         │  Excel (CSV analysis)  │
         └────────────────────────┘
```

## Performance Characteristics

| Component | Operation | Performance |
|-----------|-----------|-------------|
| Parser | Small program (10 LOC) | 19μs |
| Parser | Medium program (50 LOC) | 80μs |
| Parser | Large program (100 LOC) | 233μs |
| Semantic | Intent extraction | 4.3μs |
| Semantic | Execution analysis | 2.6μs |
| Semantic | Disharmony calculation | 0.5μs |
| Compliance | Tagging | 0.86μs |
| Compliance | Risk assessment | 8.6μs |
| **Total** | **Per procedure** | **~100μs** |

**Throughput:** 10,000+ procedures/second (single-threaded)

## Deployment Architecture

### Production Environment

```
/u/cobol-harmonizer/              # Installation
├── cobol_harmonizer/             # Core modules
│   ├── parser/                   # COBOL parsing
│   ├── semantic/                 # Semantic analysis  
│   ├── compliance/               # Compliance module
│   └── reporting/                # Report generation
├── jcl/                          # JCL templates
├── .cache/                       # Performance cache (95% speedup)
└── .audit_log/                   # Compliance audit trail

/u/prod/cobol/                    # Source code (read-only)
├── *.cbl                         # COBOL programs
└── copybook/                     # Copybooks

/u/reports/                       # Analysis results
├── nightly/                      # Daily scans
├── sox/                          # Compliance audits
└── trends/                       # Historical trends
```

## Data Flow

```
COBOL Source
    ↓
Parser Layer
    ├→ AST
    ├→ SQL/CICS Statements
    └→ Resolved Copybooks
    ↓
Semantic Analysis
    ├→ Intent (from name)
    ├→ Execution (from code)
    └→ Disharmony Score
    ↓
Compliance & Risk
    ├→ Compliance Tags
    ├→ Risk Score (0-100)
    └→ Audit Log Entry
    ↓
Report Generation
    ├→ JSON (machine-readable)
    ├→ HTML (dashboard)
    ├→ CSV (Excel)
    └→ Markdown (docs)
    ↓
Output
    ├→ File System
    ├→ DB2 Database
    ├→ Email/Alerts
    └→ CI/CD Pipeline
```

## Security

- **Input Validation:** All file paths sanitized
- **Access Control:** Respects z/OS RACF permissions
- **Audit Trail:** Complete who/what/when/why logging
- **No Code Execution:** Pure analysis, no code generation
- **Data Protection:** No sensitive data in logs

## Scalability

- **Linear Scaling:** Performance scales with codebase size
- **Parallel Processing:** 4x speedup with 4 workers
- **Caching:** 95% speedup on subsequent runs
- **Memory Efficient:** 100MB typical, 500MB maximum
- **Disk Efficient:** 5MB install + 100MB cache

---

**Version:** 0.5.0  
**Platform:** IBM z/OS with USS  
**License:** Free for IBM customers  
**Last Updated:** 2025-11-08
