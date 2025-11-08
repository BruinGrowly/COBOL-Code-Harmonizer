# VS Code Extension Testing Guide

Comprehensive testing procedures for the COBOL Harmonizer extension.

## Manual Testing Checklist

### Phase 1: Installation Testing

- [ ] Extension installs from VSIX without errors
- [ ] Extension appears in Extensions list
- [ ] Extension version shown correctly (0.1.0)
- [ ] Extension icon displays properly
- [ ] All three commands appear in Command Palette:
  - [ ] "COBOL Harmonizer: Analyze Current File"
  - [ ] "COBOL Harmonizer: Analyze Workspace"
  - [ ] "COBOL Harmonizer: Clear Diagnostics"

### Phase 2: Configuration Testing

- [ ] Settings appear in VS Code settings UI
- [ ] `pythonPath` accepts custom path
- [ ] `harmonizerPath` accepts custom path
- [ ] `harmonizerPath` auto-detects when empty
- [ ] `disharmonyThreshold` accepts values 0.0-1.5
- [ ] `analyzeOnSave` toggle works
- [ ] `copybookPaths` accepts array of paths

### Phase 3: Analysis Testing

#### Test 1: Single File Analysis (Harmonious)

**File:** `test-harmonious.cbl`
```cobol
CALCULATE-TOTAL.
    COMPUTE WS-TOTAL = WS-A + WS-B.
```

**Expected:**
- Analysis completes successfully
- No diagnostics shown
- Message: "✓ No semantic disharmony detected!"

#### Test 2: Single File Analysis (Minor Drift)

**File:** `test-minor-drift.cbl`
```cobol
VALIDATE-CUSTOMER.
    EXEC SQL
        SELECT STATUS INTO :WS-STATUS
        FROM CUSTOMERS
        WHERE CUST-ID = :WS-CUST-ID
    END-EXEC.
```

**Expected:**
- Info diagnostic on `VALIDATE-CUSTOMER`
- Disharmony score: 0.5-0.8
- Hover shows LJPW coordinates

#### Test 3: Single File Analysis (Critical)

**File:** `test-critical.cbl`
```cobol
DISPLAY-ACCOUNT.
    EXEC SQL
        DELETE FROM ACCOUNTS
        WHERE ACCT-ID = :WS-ACCT-ID
    END-EXEC.
```

**Expected:**
- Error diagnostic on `DISPLAY-ACCOUNT`
- Disharmony score: ≥1.2
- Red underline
- Message: "Found 1 procedure(s) with semantic disharmony"

### Phase 4: Workflow Testing

#### Test 4: Analyze on Save

**Steps:**
1. Open COBOL file with disharmony
2. Save file (Ctrl+S)
3. Verify analysis runs automatically

**Expected:**
- "Analyzing COBOL file..." notification appears
- Diagnostics update after save
- No manual command needed

#### Test 5: Workspace Analysis

**Setup:** Create workspace with 5 COBOL files

**Steps:**
1. Run "COBOL Harmonizer: Analyze Workspace"
2. Observe progress notification

**Expected:**
- Progress bar shows "X/5 files"
- All files analyzed
- Summary: "Analyzed 5 COBOL files"
- Diagnostics shown for all files

#### Test 6: Clear Diagnostics

**Steps:**
1. Analyze file with diagnostics
2. Run "COBOL Harmonizer: Clear Diagnostics"

**Expected:**
- All underlines removed
- Problems panel cleared
- Message: "COBOL Harmonizer diagnostics cleared"

### Phase 5: Integration Testing

#### Test 7: Context Menu

**Steps:**
1. Right-click in COBOL file
2. Find "COBOL Harmonizer: Analyze Current File"

**Expected:**
- Command appears in context menu
- Only appears for COBOL files (.cbl, .cob, .cobol)
- Not shown for other file types

#### Test 8: Copybook Resolution

**Setup:**
```json
{
  "cobolHarmonizer.copybookPaths": ["/path/to/copybooks"]
}
```

**File:** Uses `COPY CUSTOMER-RECORD.`

**Expected:**
- Copybook resolved from configured path
- No copybook errors in output
- Analysis completes successfully

### Phase 6: Error Handling

#### Test 9: Missing Python

**Setup:** Set invalid Python path
```json
{
  "cobolHarmonizer.pythonPath": "/invalid/python"
}
```

**Expected:**
- Error: "Analysis failed: ..."
- Graceful error message
- No crash

#### Test 10: Missing Harmonizer

**Setup:** Set invalid harmonizer path
```json
{
  "cobolHarmonizer.harmonizerPath": "/invalid/path.py"
}
```

**Expected:**
- Error: "Could not find cobol_harmonizer.py..."
- Suggestion to configure path
- No crash

#### Test 11: Invalid COBOL Syntax

**File:** Broken COBOL syntax

**Expected:**
- Analysis attempts but fails gracefully
- Error message shown
- No VS Code crash

### Phase 7: Performance Testing

#### Test 12: Small File (<100 LOC)

**Expected:**
- Analysis completes in < 1 second
- UI remains responsive

#### Test 13: Large File (>2000 LOC)

**Expected:**
- Analysis completes in < 10 seconds
- Progress indication shown
- UI remains responsive

#### Test 14: Large Workspace (100+ files)

**Expected:**
- Progress bar updates smoothly
- Can cancel operation (if implemented)
- Memory usage stays reasonable (<500MB)

## Automated Testing

### Unit Tests (Future)

Create `test/suite/extension.test.js`:

```javascript
const assert = require('assert');
const vscode = require('vscode');

suite('Extension Test Suite', () => {
    test('Extension should activate', async () => {
        const ext = vscode.extensions.getExtension('cobol-harmonizer.cobol-harmonizer');
        assert.ok(ext);
        await ext.activate();
        assert.ok(ext.isActive);
    });

    test('Commands should be registered', async () => {
        const commands = await vscode.commands.getCommands(true);
        assert.ok(commands.includes('cobolHarmonizer.analyzeFile'));
        assert.ok(commands.includes('cobolHarmonizer.analyzeWorkspace'));
        assert.ok(commands.includes('cobolHarmonizer.clearDiagnostics'));
    });
});
```

Run: `npm test`

## Regression Testing

Before each release:

1. Run all manual tests above
2. Test on all platforms:
   - [ ] Windows 10/11
   - [ ] macOS (Intel)
   - [ ] macOS (Apple Silicon)
   - [ ] Linux (Ubuntu)
   - [ ] z/OS USS (if available)

3. Test VS Code versions:
   - [ ] Latest stable
   - [ ] Previous major version
   - [ ] Insiders build

4. Test Python versions:
   - [ ] Python 3.9
   - [ ] Python 3.10
   - [ ] Python 3.11
   - [ ] Python 3.12

## Test Artifacts

### Sample Test Files

Create `test-files/` directory:

1. **harmonious.cbl** - No disharmony (0.0-0.3)
2. **minor-drift.cbl** - Info level (0.3-0.5)
3. **concerning.cbl** - Info level (0.5-0.8)
4. **significant.cbl** - Warning level (0.8-1.2)
5. **critical.cbl** - Error level (≥1.2)
6. **with-copybook.cbl** - Uses copybooks
7. **large-file.cbl** - 2000+ LOC
8. **invalid-syntax.cbl** - Broken COBOL

### Test Results Template

```markdown
## Test Run: [Date]

**Environment:**
- OS: [Windows/macOS/Linux]
- VS Code: [version]
- Python: [version]
- Extension: [version]

**Results:**
- Phase 1: [Pass/Fail] - [Notes]
- Phase 2: [Pass/Fail] - [Notes]
- Phase 3: [Pass/Fail] - [Notes]
- Phase 4: [Pass/Fail] - [Notes]
- Phase 5: [Pass/Fail] - [Notes]
- Phase 6: [Pass/Fail] - [Notes]
- Phase 7: [Pass/Fail] - [Notes]

**Issues Found:**
1. [Issue description]
2. [Issue description]

**Overall:** [Pass/Fail]
```

## Bug Report Template

When issues are found:

```markdown
**Environment:**
- OS: [Windows/macOS/Linux/z/OS]
- VS Code: [version]
- Python: [version]
- Extension: [version]

**Steps to Reproduce:**
1. [Step 1]
2. [Step 2]
3. [Step 3]

**Expected Behavior:**
[What should happen]

**Actual Behavior:**
[What actually happened]

**Logs:**
```
[Paste relevant logs from Output → COBOL Harmonizer]
```

**Screenshots:**
[If applicable]
```

## Continuous Testing

For CI/CD integration:

```yaml
# .github/workflows/extension-test.yml
name: Extension Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        vscode: [stable, insiders]
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
      - run: npm install
      - run: npm test
```

## Acceptance Criteria

Extension is ready for release when:

- [ ] All Phase 1-7 tests pass
- [ ] No critical bugs
- [ ] Performance meets targets (<10s for 2000 LOC)
- [ ] Documentation complete
- [ ] Works on Windows, macOS, Linux
- [ ] IBM z/OS USS testing completed (if available)

---

**Testing Status:** Ready for initial manual testing
**Last Updated:** 2025-11-08
**Next Review:** After first user feedback
