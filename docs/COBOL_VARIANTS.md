# COBOL Variants and Dialects Support

## Overview

The COBOL Code Harmonizer is designed to support all major COBOL standards and dialects used in production environments worldwide.

---

## COBOL Standards (ISO/ANSI)

### 1. COBOL-68 (ANSI X3.23-1968)
**Status:** Historical - Rarely used today
**Key Features:**
- First standardized version
- Basic file handling
- Simple data structures
- No scope terminators

### 2. COBOL-74 (ANSI X3.23-1974)
**Status:** Legacy - Still in production
**Key Features:**
- Added subprograms
- ACCEPT DATE/TIME functionality
- File organization clauses
- Improved debugging facilities

**Common In:**
- Very old mainframe systems
- Banking systems from 1970s-1980s
- Government legacy applications

### 3. COBOL-85 (ANSI X3.23-1985) ⭐ **MOST COMMON**
**Status:** Widespread production use
**Key Features:**
- **Scope terminators** (END-IF, END-PERFORM, END-READ, etc.)
- **EVALUATE statement** (multi-way branch)
- **Inline PERFORM** (structured loops)
- Nested subprograms
- Reference modification
- Improved structured programming

**Common In:**
- 70%+ of all production COBOL code
- Banking, insurance, government systems
- Most mainframe applications

**Parser Considerations:**
- Scope terminators change parsing significantly
- EVALUATE requires special handling
- Inline PERFORM affects control flow analysis

### 4. COBOL-2002 (ISO/IEC 1989:2002)
**Status:** Increasing adoption
**Key Features:**
- **Object-Oriented Programming** (classes, methods, inheritance)
- User-defined functions
- User-defined data types
- Pointers
- **XML facilities** (GENERATE, PARSE)
- Floating-point support (COMP-1, COMP-2)
- Calling conventions to C, Java, .NET
- Dynamic tables (OCCURS DEPENDING)

**Common In:**
- Modernized applications
- New COBOL development
- Integration projects

**Parser Considerations:**
- CLASS definition support
- METHOD procedures vs paragraphs
- INVOKE statement for method calls
- XML GENERATE/PARSE statements

### 5. COBOL-2014 (ISO/IEC 1989:2014)
**Status:** Modern standard
**Key Features:**
- Method overloading
- **IEEE 754 floating-point** compliance
- Dynamic capacity tables
- Enhanced pointer handling
- Improved Unicode support
- JSON support (some implementations)

**Common In:**
- Cutting-edge modernization projects
- Cloud-native COBOL
- Micro Focus Visual COBOL

**Parser Considerations:**
- JSON GENERATE/PARSE statements
- Enhanced numeric types
- Method overloading resolution

### 6. COBOL-2023 (ISO/IEC 1989:2023)
**Status:** Latest standard
**Key Features:**
- Further JSON enhancements
- Improved interoperability
- Modern language features
- Enhanced portability

**Common In:**
- Very recent implementations
- GnuCOBOL 3.2+
- Not yet widely adopted

---

## COBOL Dialects and Implementations

### 1. IBM Enterprise COBOL (z/OS)
**Platform:** IBM Mainframes (z/OS, z/VSE, z/VM)
**Market Share:** ~60% of mainframe COBOL
**Standards:** COBOL-85 base + extensions, supports 2002/2014 features

**Unique Features:**
- **EXEC SQL** (embedded DB2)
- **EXEC CICS** (transaction processing)
- **EXEC DLI** (IMS database)
- **SQL TYPE IS** declarations
- COMP-3 (packed decimal) - IBM specific
- COMPUTATIONAL-3 optimization
- PGMNAME directive
- SKIP1/SKIP2/SKIP3 for printer control

**Common Patterns:**
```cobol
       EXEC SQL
           SELECT CUSTOMER_NAME
           INTO :WS-CUSTOMER-NAME
           FROM CUSTOMERS
           WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
       END-EXEC

       EXEC CICS
           READ DATASET('CUSTFILE')
           INTO(CUSTOMER-RECORD)
           RIDFLD(WS-CUSTOMER-KEY)
       END-EXEC
```

**Parser Requirements:**
- EXEC SQL/CICS/DLI block handling
- Colon-prefixed host variables (:WS-VAR)
- COPY/INCLUDE for copybooks
- Compiler directives (CBL, PROCESS)

### 2. Micro Focus COBOL
**Platform:** Windows, Linux, Unix, Cloud
**Market Share:** Dominant in non-mainframe COBOL
**Standards:** COBOL-85, COBOL-2002, COBOL-2014

**Unique Features:**
- **Visual COBOL** (IDE integration)
- **.NET integration** (managed COBOL)
- **Java interoperability**
- **Screen Section** enhancements
- **ACUCOBOL compatibility mode**
- File handling extensions
- Enhanced debugging directives

**Common Patterns:**
```cobol
       $SET SOURCEFORMAT"FREE"
       CLASS CustomerRecord
           METHOD-ID. CalculateDiscount.
           01 discount PIC 99V99.
           PROCEDURE DIVISION.
               COMPUTE discount = price * 0.10
               GOBACK.
           END METHOD CalculateDiscount.
       END CLASS CustomerRecord.
```

**Parser Requirements:**
- $SET compiler directives
- Free-format source support
- CLASS/METHOD syntax
- Mixed fixed/free format

### 3. GnuCOBOL (formerly OpenCOBOL)
**Platform:** Cross-platform (compiles to C)
**Market Share:** Growing in open-source
**Standards:** COBOL-85, COBOL-2002, COBOL-2014, COBOL-2023

**Unique Features:**
- **Open source** (GPL/LGPL)
- Compiles to native C code
- Excellent standards compliance
- Multiple dialect emulation (IBM, MF, ACUCOBOL)
- C function calling
- Screen handling extensions

**Common Patterns:**
```cobol
       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. modern-program.

       >>IF DEBUG-MODE DEFINED
           DISPLAY "Debug mode enabled"
       >>END-IF
```

**Parser Requirements:**
- >> compiler directives
- Multiple dialect modes
- Free and fixed format
- C function interfaces

### 4. ACUCOBOL-GT (now Micro Focus Extend)
**Platform:** Cross-platform
**Market Share:** Niche but significant
**Standards:** COBOL-85 with extensions

**Unique Features:**
- **Runtime-based** (not compiled)
- **Screen Section** with GUI
- **Vision indexed files** (proprietary)
- Web enablement (AcuXML)
- C$COPY runtime library

**Parser Requirements:**
- C$ runtime routines
- Extended SCREEN SECTION
- Vision file syntax

### 5. Fujitsu NetCOBOL
**Platform:** Windows, Linux
**Market Share:** Strong in Asia-Pacific
**Standards:** COBOL-85, COBOL-2002

**Unique Features:**
- **.NET Framework integration**
- Windows GUI support
- Japanese language support
- Excel/Office integration
- Strong Windows API access

**Parser Requirements:**
- .NET class integration
- Windows-specific extensions

### 6. COBOL.NET / Visual COBOL
**Platform:** .NET Framework, .NET Core
**Market Share:** Growing for modernization
**Standards:** COBOL-2002, COBOL-2014

**Unique Features:**
- Full .NET integration
- Object-oriented features
- LINQ support (some)
- ASP.NET web development
- Modern IDE integration

**Parser Requirements:**
- .NET namespace references
- Attribute syntax
- USING directives

---

## Format Variations

### Fixed-Format (Column-Sensitive)
**Columns:**
- 1-6: Sequence number area
- 7: Indicator area (*, -, /, D, comment markers)
- 8-11: Area A (division, section, paragraph, 01/77 levels)
- 12-72: Area B (statements, continued entries)
- 73-80: Program identification area (ignored)

**Used In:** COBOL-74, COBOL-85, traditional mainframe

### Free-Format
**Features:**
- No column restrictions
- More like modern languages
- Requires directive: `>>SOURCE FORMAT IS FREE`
- Supported in COBOL-2002+

**Used In:** Modern COBOL, GnuCOBOL, Micro Focus

---

## Regional Variations

### North America
- Heavy IBM mainframe focus
- COBOL-85 dominant
- DB2, CICS integration common

### Europe
- Mix of IBM and Micro Focus
- More modernization activity
- Cloud migration projects

### Asia-Pacific
- Strong Fujitsu presence
- Japanese business practices reflected in code
- Government systems in COBOL

---

## Harmonizer Support Matrix

| Feature | COBOL-74 | COBOL-85 | COBOL-2002 | COBOL-2014 | IBM | Micro Focus | GnuCOBOL |
|---------|----------|----------|------------|------------|-----|-------------|----------|
| **Fixed Format** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Free Format** | ❌ | ❌ | ✅ | ✅ | Partial | ✅ | ✅ |
| **Scope Terminators** | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **EVALUATE** | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **OOP (Classes)** | ❌ | ❌ | ✅ | ✅ | Partial | ✅ | ✅ |
| **XML Support** | ❌ | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **JSON Support** | ❌ | ❌ | ❌ | Partial | ✅ | ✅ | ✅ |
| **EXEC SQL** | N/A | N/A | N/A | N/A | ✅ | ✅ | ✅ |
| **EXEC CICS** | N/A | N/A | N/A | N/A | ✅ | ✅ | Emulation |

---

## Testing Strategy

### Priority 1: Core Standards (COBOL-85)
- 3 samples from banking
- 3 samples from insurance
- 3 samples from government

### Priority 2: Modern Standards (COBOL-2002/2014)
- 3 OOP samples
- 3 XML/JSON samples
- 3 modernization projects

### Priority 3: IBM Mainframe
- 3 EXEC SQL samples
- 3 EXEC CICS samples
- 3 mixed samples

### Priority 4: Cross-Platform
- 3 GnuCOBOL samples
- 3 Micro Focus samples
- 3 free-format samples

**Total:** 30 samples across all major variants

---

## Parser Enhancement Plan

### Phase 1: Format Support
- [x] Fixed-format COBOL-85
- [ ] Free-format COBOL-2002+
- [ ] Mixed format detection
- [ ] Auto-format detection

### Phase 2: Statement Support
- [x] Basic COBOL-85 verbs
- [ ] COBOL-2002 OOP (CLASS, METHOD, INVOKE)
- [ ] XML GENERATE/PARSE
- [ ] JSON GENERATE/PARSE
- [ ] EXEC SQL blocks
- [ ] EXEC CICS blocks

### Phase 3: Dialect Support
- [ ] IBM compiler directives
- [ ] Micro Focus extensions
- [ ] GnuCOBOL directives
- [ ] Copybook resolution

### Phase 4: Advanced Features
- [ ] Object-oriented analysis
- [ ] SQL statement analysis
- [ ] CICS transaction flow
- [ ] Cross-program call graphs

---

## Current Support Status

✅ **Fully Supported:**
- COBOL-85 fixed-format
- Standard PROCEDURE DIVISION
- 120+ COBOL-85 verbs
- Paragraph-level analysis

⚠️ **Partial Support:**
- COBOL-2002 features (detection but limited analysis)
- Free-format (can parse but may have issues)
- EXEC blocks (treated as single statements)

🔄 **Planned:**
- Full COBOL-2002 OOP support
- EXEC SQL/CICS detailed analysis
- Free-format optimization
- Dialect-specific verb mappings

---

## Recommended Next Steps

1. **Enhance Parser:**
   - Add free-format support
   - Implement COBOL-2002 class parsing
   - Handle EXEC blocks properly

2. **Expand Verb Library:**
   - Add dialect-specific verbs
   - IBM-specific operations
   - Micro Focus extensions

3. **Test Comprehensively:**
   - 3 samples per standard
   - 3 samples per major dialect
   - Real-world production code

4. **Document:**
   - Variant-specific guides
   - Migration recommendations
   - Best practices per dialect

---

**Last Updated:** 2025-11-07
**Version:** 0.3.0
**Status:** In Progress
