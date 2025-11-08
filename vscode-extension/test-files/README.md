# COBOL Harmonizer - VS Code Extension Test Files

This directory contains test COBOL files that demonstrate all severity levels of semantic disharmony. Use these to test the VS Code extension and understand what each classification means.

## Files Overview

| File | Severity | Score Range | Description |
|------|----------|-------------|-------------|
| `harmonious.cbl` | ✅ Harmonious | 0.0-0.3 | Perfect alignment - names match implementations |
| `minor-drift.cbl` | ℹ️ Minor Drift | 0.3-0.5 | Slight mismatch - review recommended |
| `concerning.cbl` | ⚠️ Concerning | 0.5-0.8 | Notable issues - refactoring recommended |
| `significant.cbl` | 🔴 Significant | 0.8-1.2 | Major problems - rename or refactor required |
| `critical.cbl` | 💥 Critical | 1.2+ | Severe bugs - immediate action required |

## Quick Start

### 1. Install the Extension

```bash
cd ..
vsce package
code --install-extension cobol-harmonizer-0.1.0.vsix
```

### 2. Configure Extension

Open VS Code settings (Ctrl+,) and configure:

```json
{
  "cobolHarmonizer.pythonPath": "python3",
  "cobolHarmonizer.harmonizerPath": "/path/to/cobol_harmonizer.py",
  "cobolHarmonizer.disharmonyThreshold": 0.3
}
```

### 3. Test the Extension

Open each test file and watch the extension detect semantic disharmony:

1. **Open `harmonious.cbl`**
   - Save file (Ctrl+S)
   - Should see: "✓ No semantic disharmony detected!"
   - No underlines or warnings

2. **Open `minor-drift.cbl`**
   - Save file
   - Should see blue ℹ️ info squiggles under procedure names
   - Hover to see disharmony score and LJPW coordinates

3. **Open `concerning.cbl`**
   - Save file
   - Should see blue ℹ️ info squiggles
   - Message: "Found X procedure(s) with semantic disharmony"

4. **Open `significant.cbl`**
   - Save file
   - Should see yellow 🟡 warning squiggles
   - Clear warning about naming contradictions

5. **Open `critical.cbl`**
   - Save file
   - Should see red 🔴 error squiggles
   - Critical warnings about severe mismatches

## What Each File Demonstrates

### harmonious.cbl - Perfect Code

All procedure names accurately describe their implementations:

```cobol
CALCULATE-MONTHLY-TOTAL.
    COMPUTE WS-MONTHLY-TOTAL = WS-DAILY-TOTAL * WS-DAYS.
```

✅ **Harmonious** - Name says CALCULATE, code CALCULATES

### minor-drift.cbl - Slight Mismatches

Procedures that do slightly more than the name suggests:

```cobol
VALIDATE-CUSTOMER.
    EXEC SQL
        SELECT STATUS INTO :WS-STATUS  *> Also READS!
        FROM CUSTOMERS
    END-EXEC.
    IF WS-STATUS NOT = 'ACTIVE'
        MOVE 'INVALID' TO WS-RESULT
    END-IF.
```

ℹ️ **Minor Drift** - Name suggests validation only, but also reads data

### concerning.cbl - Notable Problems

Procedures that do something notably different:

```cobol
CALCULATE-TOTAL-BALANCE.
    EXEC SQL
        INSERT INTO BALANCE_HISTORY  *> CALCULATES or INSERTS?
        VALUES (:WS-CUST-ID, :WS-BALANCE, CURRENT_DATE)
    END-EXEC.
```

⚠️ **Concerning** - Name says CALCULATE, but code INSERTS

### significant.cbl - Major Issues

Procedures that contradict their names:

```cobol
RETRIEVE-CUSTOMER-ACCOUNT.
    EXEC SQL
        DELETE FROM CUSTOMERS  *> RETRIEVES or DELETES?!
        WHERE CUST_ID = :WS-CUST-ID
    END-EXEC.
```

🔴 **Significant** - Name says RETRIEVE, but code DELETES!

### critical.cbl - Severe Bugs

Procedures with completely opposite implementations:

```cobol
DISPLAY-ACCOUNT-INFO.
    EXEC SQL
        DELETE FROM ACCOUNTS  *> DISPLAYS or DELETES?!!
        WHERE ACCT_ID = :WS-ACCT-ID
    END-EXEC.
```

💥 **CRITICAL** - Name says DISPLAY, but code DELETES entire records!

## Testing Checklist

- [ ] Extension activates when opening .cbl files
- [ ] Auto-analysis triggers on save
- [ ] Harmonious file shows no warnings
- [ ] Minor drift shows blue info squiggles
- [ ] Concerning shows blue info squiggles
- [ ] Significant shows yellow warning squiggles
- [ ] Critical shows red error squiggles
- [ ] Hover tooltips show LJPW coordinates
- [ ] Problems panel shows all issues
- [ ] Workspace analysis command works
- [ ] Clear diagnostics command works

## Manual Analysis

You can also analyze files manually:

1. Right-click in any COBOL file
2. Select "COBOL Harmonizer: Analyze Current File"
3. View results in Problems panel (Ctrl+Shift+M)

## Adjusting Threshold

Test different threshold settings to see how it affects reporting:

```json
{
  // More sensitive - reports more issues
  "cobolHarmonizer.disharmonyThreshold": 0.3,

  // Standard - good balance
  "cobolHarmonizer.disharmonyThreshold": 0.5,

  // Less sensitive - only major issues
  "cobolHarmonizer.disharmonyThreshold": 0.8
}
```

## Expected Results

With default threshold (0.5), you should see:

- **harmonious.cbl**: 0 diagnostics
- **minor-drift.cbl**: 0 diagnostics (below threshold)
- **concerning.cbl**: 3 diagnostics (Info level)
- **significant.cbl**: 3 diagnostics (Warning level)
- **critical.cbl**: 4 diagnostics (Error level)

## Troubleshooting

### No diagnostics appear

1. Check Python path in settings
2. Verify harmonizer path is correct
3. Check Output panel → COBOL Harmonizer for errors
4. Ensure threshold isn't too high

### Extension not activating

1. Check file extension is .cbl, .cob, or .cobol
2. Reload VS Code (Ctrl+Shift+P → "Reload Window")
3. Check Extensions panel to verify installation

### Analysis takes too long

1. Check Python installation
2. Verify harmonizer.py is accessible
3. Try with smaller files first

## Performance Expectations

Typical analysis times for these test files:

- Small files (<100 LOC): < 1 second
- Medium files (100-500 LOC): 1-3 seconds
- Large files (500+ LOC): 3-10 seconds

## Next Steps

After testing with these files:

1. Try analyzing your own COBOL codebase
2. Experiment with different threshold settings
3. Use the Workspace Analysis command for batch processing
4. Configure copybook paths if needed
5. Provide feedback on GitHub Issues

## Support

- **Extension README**: `../README.md`
- **Installation Guide**: `../INSTALL.md`
- **Main Project**: `../../README.md`
- **Issues**: https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues

---

**Happy Testing!** 🚀

These test files cover all severity levels and demonstrate the power of semantic analysis for COBOL code.
