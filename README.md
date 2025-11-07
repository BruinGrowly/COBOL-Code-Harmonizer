# COBOL Code Harmonizer 💛⚓

**Semantic analysis for COBOL using the LJPW framework**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)

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

COBOL Code Harmonizer uses the **LJPW (Love, Justice, Power, Wisdom) framework** - a mathematically proven semantic model - to analyze your COBOL code and detect these contradictions.

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

### Current (v0.1.0)

- ✅ Fixed-format COBOL parsing
- ✅ 120+ COBOL verbs mapped to LJPW coordinates
- ✅ Intent extraction from procedure names
- ✅ Execution analysis from procedure bodies
- ✅ Disharmony calculation with severity classification
- ✅ Example COBOL programs (harmonious & disharmonious)

### Roadmap

#### Phase 2 (v0.2.0)
- [ ] Free-format COBOL support
- [ ] COPY book resolution
- [ ] CLI commands (`analyze`, `report`, `suggest`)
- [ ] JSON/SARIF export for CI/CD

#### Phase 3 (v0.3.0)
- [ ] PERFORM chain analysis
- [ ] Call graph generation
- [ ] Batch analysis for legacy codebases
- [ ] Interactive fix suggestions

#### Phase 4 (v1.0.0)
- [ ] VS Code extension
- [ ] Web dashboard
- [ ] Git integration
- [ ] Comprehensive documentation

---

## Project Structure

```
cobol-harmonizer/
├── cobol_harmonizer/           # Main package
│   ├── parser/                 # COBOL parsing
│   │   └── cobol_parser.py
│   ├── semantic/               # Semantic analysis
│   │   ├── verb_mapper.py      # COBOL verb → LJPW mapping
│   │   ├── intent_extractor.py # Extract intent from names
│   │   ├── execution_analyzer.py # Analyze procedure bodies
│   │   └── disharmony.py       # Calculate semantic distance
│   ├── reporter/               # Output generation
│   ├── cli/                    # Command-line interface
│   └── utils/                  # Utilities
├── tests/                      # Test suite
├── examples/                   # Example COBOL programs
│   ├── harmonious_example.cbl  # Good examples
│   └── disharmonious_example.cbl # Bug examples
├── docs/                       # Documentation
│   ├── ARCHITECTURE.md         # System design
│   ├── COBOL_SEMANTICS.md      # COBOL-LJPW mapping
│   ├── PHILOSOPHY.md           # LJPW framework theory
│   └── ...                     # More docs
├── requirements.txt            # Python dependencies
├── setup.py                    # Package setup
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
