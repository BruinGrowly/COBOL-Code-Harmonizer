# COBOL Code Harmonizer - Architecture

**Version**: 1.0
**Date**: 2025-11-07
**Status**: Design Document

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [System Architecture](#system-architecture)
3. [Core Components](#core-components)
4. [COBOL-Specific Challenges](#cobol-specific-challenges)
5. [Data Flow](#data-flow)
6. [LJPW Mapping for COBOL](#ljpw-mapping-for-cobol)
7. [Implementation Phases](#implementation-phases)
8. [Technology Stack](#technology-stack)

---

## Executive Summary

The COBOL Code Harmonizer applies the proven LJPW (Love, Justice, Power, Wisdom) semantic framework to COBOL codebases, detecting when procedure names contradict their implementations.

**Core Innovation**: COBOL has massive technical debt with decades-old procedures that evolved beyond their original purpose. This tool identifies semantic mismatches that traditional linters miss.

**Mission**: Free, open-source semantic analysis for COBOL - addressing a critical gap no one else is tackling.

---

## System Architecture

### High-Level Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    COBOL Code Harmonizer                     │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │   COBOL     │───▶│   Semantic   │───▶│  Disharmony   │  │
│  │   Parser    │    │   Analyzer   │    │  Calculator   │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│         │                   │                     │          │
│         ▼                   ▼                     ▼          │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │   AST       │    │ Intent/Exec  │    │   Reporting   │  │
│  │ Generator   │    │  Extractor   │    │    Engine     │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│                                                               │
└─────────────────────────────────────────────────────────────┘
         │                       │                      │
         ▼                       ▼                      ▼
   COBOL Source            LJPW Coords           Reports/Fixes
```

### Architecture Layers

**Layer 1: Input Processing**
- COBOL source file reader
- Fixed/free format detector
- Dialect identifier (COBOL-85, Enterprise COBOL, etc.)
- COPY book resolver

**Layer 2: Parsing & AST**
- Lexical analysis (tokenization)
- Syntax parsing
- AST generation
- PROCEDURE DIVISION extraction

**Layer 3: Semantic Analysis**
- Intent extraction (from PARAGRAPH/SECTION names)
- Execution extraction (from COBOL verbs in code)
- LJPW coordinate calculation
- Disharmony scoring

**Layer 4: Reporting & Remediation**
- Console output
- JSON/SARIF export
- Interactive fix suggestions
- Batch analysis for legacy codebases

---

## Core Components

### 1. COBOL Parser

**Responsibility**: Parse COBOL source into analyzable structure

**Key Features**:
- Support for fixed-format (columns 7-72)
- Support for free-format COBOL
- Handle IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions
- Extract SECTION and PARAGRAPH definitions
- Resolve COPY/INCLUDE statements

**Implementation**:
```python
class COBOLParser:
    def parse_file(self, filepath: str) -> COBOLProgram:
        """Parse COBOL source file into structured representation"""

    def extract_procedures(self, program: COBOLProgram) -> List[Procedure]:
        """Extract all SECTION and PARAGRAPH definitions"""

    def tokenize(self, source: str) -> List[Token]:
        """Convert COBOL source to tokens"""
```

### 2. Verb Mapper

**Responsibility**: Map COBOL verbs to LJPW semantic coordinates

**COBOL Verb Categories**:

**Wisdom-Dominant** (Information retrieval/processing):
- `READ` (file/database input)
- `ACCEPT` (user input)
- `INSPECT` (string analysis)
- `STRING/UNSTRING` (parsing)
- `COMPUTE` (calculation)
- `DISPLAY` (output - also has Love component)

**Justice-Dominant** (Validation/verification):
- `IF/EVALUATE` (conditional logic)
- `SEARCH` (finding with conditions)
- `TEST` (condition checking)
- `VALIDATE` (Enterprise COBOL)

**Power-Dominant** (State modification):
- `MOVE` (assignment)
- `WRITE` (output to file)
- `REWRITE` (update record)
- `DELETE` (remove record)
- `OPEN/CLOSE` (file operations)
- `PERFORM` (execution - depends on target)
- `CALL` (subroutine invocation - also has Love)
- `INITIALIZE` (reset data)
- `SET` (modify values)

**Love-Dominant** (Connection/communication):
- `CALL` (program linkage)
- `MERGE` (combining files)
- `SORT` (organizing relationships)
- `COPY` (including shared code)
- `INVOKE` (OO COBOL method calls)

**Implementation**:
```python
class VerbMapper:
    # Semantic coordinates for each COBOL verb
    VERB_COORDINATES = {
        'READ': (0.1, 0.2, 0.0, 0.7),      # Wisdom-focused
        'WRITE': (0.1, 0.1, 0.7, 0.1),     # Power-focused
        'IF': (0.0, 0.8, 0.1, 0.1),        # Justice-focused
        'CALL': (0.5, 0.1, 0.3, 0.1),      # Love-focused (connection)
        'MOVE': (0.0, 0.1, 0.6, 0.3),      # Power + Wisdom
        'COMPUTE': (0.0, 0.3, 0.2, 0.5),   # Wisdom + Justice
        # ... 100+ more verbs
    }

    def map_verb(self, verb: str) -> Tuple[float, float, float, float]:
        """Return LJPW coordinates for COBOL verb"""
```

### 3. Intent Extractor

**Responsibility**: Analyze procedure names to determine intended semantics

**Strategy**:
```
PARAGRAPH name: "GET-CUSTOMER-RECORD"
  ↓ Parse name
  ["GET", "CUSTOMER", "RECORD"]
  ↓ Map keywords
  GET → Wisdom (0.7)
  CUSTOMER → Domain term (neutral)
  RECORD → Domain term (neutral)
  ↓ Result
  Intent coordinates: (0.1, 0.2, 0.0, 0.7)
```

**Implementation**:
```python
class IntentExtractor:
    INTENT_KEYWORDS = {
        'GET': (0.1, 0.1, 0.0, 0.8),
        'SET': (0.0, 0.1, 0.8, 0.1),
        'UPDATE': (0.0, 0.2, 0.7, 0.1),
        'DELETE': (0.0, 0.2, 0.8, 0.0),
        'VALIDATE': (0.0, 0.8, 0.1, 0.1),
        'CHECK': (0.0, 0.7, 0.1, 0.2),
        'CALCULATE': (0.0, 0.3, 0.1, 0.6),
        'PROCESS': (0.1, 0.2, 0.4, 0.3),  # Vague/mixed
        # ... more
    }

    def extract_intent(self, name: str) -> Tuple[float, float, float, float]:
        """Derive intended semantics from procedure name"""
```

### 4. Execution Analyzer

**Responsibility**: Analyze procedure body to determine actual semantics

**Strategy**:
```
Procedure body contains:
  - READ CUSTOMER-FILE (Wisdom: 0.7)
  - IF CUSTOMER-STATUS = "ACTIVE" (Justice: 0.8)
  - DELETE CUSTOMER-RECORD (Power: 0.9)
  - MOVE "DELETED" TO WS-STATUS (Power: 0.6)

  ↓ Weighted average
  Execution coordinates: (0.0, 0.3, 0.7, 0.1)
```

**Implementation**:
```python
class ExecutionAnalyzer:
    def analyze_procedure_body(
        self,
        procedure: Procedure
    ) -> Tuple[float, float, float, float]:
        """Analyze verbs in procedure to get execution semantics"""

        verb_coords = []
        for statement in procedure.statements:
            verb = statement.verb
            coords = self.verb_mapper.map_verb(verb)
            verb_coords.append(coords)

        # Weighted average (or other mixing strategy)
        return self.calculate_centroid(verb_coords)
```

### 5. Disharmony Calculator

**Responsibility**: Calculate semantic distance between intent and execution

**Formula**:
```
Disharmony = √[(Li - Le)² + (Ji - Je)² + (Pi - Pe)² + (Wi - We)²]

Where:
  (Li, Ji, Pi, Wi) = Intent coordinates
  (Le, Je, Pe, We) = Execution coordinates
```

**Thresholds**:
- `0.0 - 0.3`: Harmonious ✓
- `0.3 - 0.5`: Minor drift (worth reviewing)
- `0.5 - 0.8`: Concerning contradiction ⚠️
- `0.8 - 1.2`: Significant disharmony 🔴
- `1.2+`: Critical semantic bug 💥

**Implementation**:
```python
class DisharmonyCalculator:
    def calculate(
        self,
        intent: Tuple[float, float, float, float],
        execution: Tuple[float, float, float, float]
    ) -> float:
        """Euclidean distance in LJPW space"""
        return math.sqrt(
            (intent[0] - execution[0])**2 +  # Love
            (intent[1] - execution[1])**2 +  # Justice
            (intent[2] - execution[2])**2 +  # Power
            (intent[3] - execution[3])**2    # Wisdom
        )
```

---

## COBOL-Specific Challenges

### Challenge 1: Fixed vs Free Format

**Fixed Format** (COBOL-85 standard):
```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. CUSTMGMT.
000300 PROCEDURE DIVISION.
000400 GET-CUSTOMER-RECORD.
```
- Columns 1-6: Sequence numbers (ignore)
- Column 7: Indicator (*, -, D for comments/continuation)
- Columns 8-11: Area A (divisions, sections, paragraphs)
- Columns 12-72: Area B (statements)
- Columns 73-80: Program identification (ignore)

**Free Format** (COBOL-2002+):
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. CUSTMGMT.
PROCEDURE DIVISION.
GET-CUSTOMER-RECORD.
    READ CUSTOMER-FILE.
```

**Solution**: Auto-detect format and parse accordingly.

### Challenge 2: PERFORM Statements

PERFORM can invoke other paragraphs/sections:

```cobol
MAIN-LOGIC.
    PERFORM GET-CUSTOMER.      *> Calls another paragraph
    PERFORM VALIDATE-CUSTOMER. *> Calls another paragraph
    PERFORM UPDATE-DATABASE.   *> Calls another paragraph
```

**Challenge**: Semantic analysis must follow PERFORM chains to understand full execution path.

**Solution**: Build call graph, recursively analyze performed procedures.

### Challenge 3: COPY Books

Shared code via COPY/INCLUDE:

```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY CUSTOMER-RECORD.   *> External file
```

**Challenge**: Need to resolve and parse COPY books to get complete picture.

**Solution**: COPY book resolver with configurable search paths.

---

## LJPW Mapping for COBOL

### Complete Verb Taxonomy

#### High Wisdom (W > 0.6)
```cobol
READ            *> (0.1, 0.2, 0.0, 0.7) - Read from file
ACCEPT          *> (0.1, 0.1, 0.0, 0.8) - Accept user input
COMPUTE         *> (0.0, 0.3, 0.1, 0.6) - Calculate value
INSPECT         *> (0.0, 0.2, 0.0, 0.8) - Examine string
STRING          *> (0.0, 0.2, 0.1, 0.7) - Parse/compose strings
```

#### High Justice (J > 0.6)
```cobol
IF              *> (0.0, 0.8, 0.1, 0.1) - Conditional logic
EVALUATE        *> (0.0, 0.8, 0.1, 0.1) - Switch/case logic
SEARCH          *> (0.0, 0.7, 0.0, 0.3) - Find with conditions
```

#### High Power (P > 0.6)
```cobol
WRITE           *> (0.1, 0.1, 0.7, 0.1) - Write to file
REWRITE         *> (0.0, 0.2, 0.7, 0.1) - Update record
DELETE          *> (0.0, 0.2, 0.8, 0.0) - Delete record
MOVE            *> (0.0, 0.1, 0.6, 0.3) - Assign value
SET             *> (0.0, 0.1, 0.7, 0.2) - Set value/pointer
INITIALIZE      *> (0.0, 0.1, 0.7, 0.2) - Reset data
```

#### High Love (L > 0.5)
```cobol
CALL            *> (0.5, 0.1, 0.3, 0.1) - Call program
MERGE           *> (0.5, 0.2, 0.2, 0.1) - Merge files
```

---

## Implementation Phases

### Phase 1: Core Foundation (Weeks 1-3)
- Project structure setup
- Documentation
- Basic COBOL parser (fixed format)
- Verb mapper with 50+ verbs
- Basic intent extractor
- Disharmony calculator
- Console reporter

**Deliverable**: CLI tool that can analyze simple COBOL programs

### Phase 2: Enhanced Parsing (Weeks 4-6)
- Free-format support
- COPY book resolver
- PERFORM chain analysis
- Nested program support

**Deliverable**: Can handle real-world COBOL programs

### Phase 3: Advanced Features (Weeks 7-10)
- Interactive fix suggestions
- JSON/SARIF export
- Legacy codebase mapper
- CI/CD integration

**Deliverable**: Production-ready tool

### Phase 4: Polish & Community (Weeks 11-12)
- Comprehensive documentation
- Example programs
- Community guidelines

**Deliverable**: Ready for open-source release

---

## Technology Stack

### Core Language
**Python 3.8+**

### Key Dependencies
```python
# Parsing
pyparsing>=3.0.0       # Parser combinator library

# Math
numpy>=1.21.0          # Numerical operations

# CLI/UX
click>=8.0.0           # CLI framework
rich>=10.0.0           # Beautiful terminal output

# Testing
pytest>=7.0.0          # Testing framework
```

### Project Structure
```
cobol-harmonizer/
├── cobol_harmonizer/           # Main package
│   ├── parser/                 # COBOL parsing
│   ├── semantic/               # Semantic analysis
│   ├── reporter/               # Output generation
│   ├── cli/                    # Command-line interface
│   └── utils/                  # Utilities
├── tests/                      # Test suite
├── examples/                   # Example COBOL programs
├── docs/                       # Documentation
├── setup.py                    # Package setup
├── requirements.txt            # Dependencies
├── README.md                   # Project README
└── LICENSE                     # MIT License
```

---

## Conclusion

The COBOL Code Harmonizer brings modern semantic analysis to the world's most critical legacy codebases.

**This is not just a tool - it's a movement to address technical debt in the COBOL ecosystem.**

**Let's harmonize COBOL.** 💛⚓
