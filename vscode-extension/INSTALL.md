# VS Code Extension Installation Guide

Complete installation instructions for the COBOL Code Harmonizer VS Code extension.

## Prerequisites

### 1. Install Python 3.9+

**Linux/z/OS USS:**
```bash
python3 --version  # Should show 3.9 or higher
```

**Windows:**
```cmd
python --version  # Should show 3.9 or higher
```

### 2. Install COBOL Code Harmonizer CLI

See [../docs/IBM_QUICK_START.md](../docs/IBM_QUICK_START.md) for CLI installation.

**Quick verification:**
```bash
python3 /path/to/cobol_harmonizer.py --version
```

## Installation Methods

### Method 1: Install from VSIX (Recommended)

#### Step 1: Package the Extension

```bash
cd vscode-extension

# Install packaging tool (one-time)
npm install -g vsce

# Install dependencies
npm install

# Create VSIX package
vsce package
```

This creates `cobol-harmonizer-0.1.0.vsix`

#### Step 2: Install in VS Code

**GUI Method:**
1. Open VS Code
2. Click Extensions (Ctrl+Shift+X)
3. Click "..." menu → "Install from VSIX..."
4. Select `cobol-harmonizer-0.1.0.vsix`

**Command Line:**
```bash
code --install-extension cobol-harmonizer-0.1.0.vsix
```

### Method 2: Development Mode

For testing and development:

```bash
cd vscode-extension
npm install

# Open VS Code
code .

# Press F5 to launch Extension Development Host
```

## Configuration

### Step 1: Open Settings

- File → Preferences → Settings (Ctrl+,)
- Search for "COBOL Harmonizer"

### Step 2: Configure Paths

**Option A: Automatic (if installed in standard location)**
```json
{
  "cobolHarmonizer.pythonPath": "python3"
  // harmonizerPath auto-detects
}
```

**Option B: Manual Configuration**
```json
{
  "cobolHarmonizer.pythonPath": "/usr/bin/python3",
  "cobolHarmonizer.harmonizerPath": "/u/cobol-harmonizer/cobol_harmonizer.py"
}
```

**Option C: IBM z/OS USS**
```json
{
  "cobolHarmonizer.pythonPath": "/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3",
  "cobolHarmonizer.harmonizerPath": "/u/cobol-harmonizer/cobol_harmonizer.py",
  "cobolHarmonizer.copybookPaths": [
    "/u/prod/cobol/copybook",
    "/SYS1.COPYLIB"
  ]
}
```

### Step 3: Test Installation

1. Open a COBOL file (.cbl, .cob, .cobol)
2. Right-click → "COBOL Harmonizer: Analyze Current File"
3. Should see "Analyzing COBOL file..." notification
4. Results appear as inline diagnostics

## Verification

### Quick Test

Create a test file `test-disharmony.cbl`:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DISHARMONY.

       PROCEDURE DIVISION.

       VALIDATE-CUSTOMER.
           EXEC SQL
               UPDATE CUSTOMERS
               SET STATUS = 'APPROVED'
               WHERE CUST-ID = :WS-CUST-ID
           END-EXEC.

       STOP RUN.
```

Save the file - should see a warning on `VALIDATE-CUSTOMER` (name suggests validation, code does update).

## Troubleshooting

### Extension Not Activating

**Symptom:** No "COBOL Harmonizer" commands in Command Palette

**Solution:**
1. Check VS Code version: Help → About (requires 1.60.0+)
2. Reload VS Code: Ctrl+Shift+P → "Reload Window"
3. Check extension installed: Extensions → Search "COBOL"

### "Could not find cobol_harmonizer.py"

**Symptom:** Error when running analysis

**Solution:** Set explicit path in settings:
```json
{
  "cobolHarmonizer.harmonizerPath": "/full/path/to/cobol_harmonizer.py"
}
```

**Verify path:**
```bash
ls -l /full/path/to/cobol_harmonizer.py
```

### "Analysis failed: Python not found"

**Symptom:** Error about Python executable

**Solution:** Find Python path and configure:

**Linux/macOS:**
```bash
which python3
# Use output in settings
```

**Windows:**
```cmd
where python
# Use output in settings
```

### No Diagnostics Shown

**Possible causes:**

1. **Threshold too high** - Lower it:
```json
{
  "cobolHarmonizer.disharmonyThreshold": 0.3
}
```

2. **Analyze on save disabled** - Enable it:
```json
{
  "cobolHarmonizer.analyzeOnSave": true
}
```

3. **File not recognized as COBOL** - Check extension (.cbl, .cob, .cobol)

4. **Code is actually harmonious!** - No problems found ✓

### Copybooks Not Resolved

**Symptom:** Warnings about missing copybooks

**Solution:** Add copybook paths:
```json
{
  "cobolHarmonizer.copybookPaths": [
    "/u/prod/cobol/copybook",
    "./local/copybooks",
    "../shared/copybooks"
  ]
}
```

### Performance Issues

**Symptom:** Analysis takes too long

**Solutions:**

1. **Increase threshold** (analyze fewer procedures):
```json
{
  "cobolHarmonizer.disharmonyThreshold": 0.8
}
```

2. **Disable auto-analyze** (manual only):
```json
{
  "cobolHarmonizer.analyzeOnSave": false
}
```

3. **Check cache** - First run is slow, subsequent runs are 95% faster

## Uninstallation

### From VS Code GUI

1. Extensions → Search "COBOL Harmonizer"
2. Click gear icon → Uninstall

### From Command Line

```bash
code --uninstall-extension cobol-harmonizer.cobol-harmonizer
```

## Updates

To update to a new version:

1. Uninstall current version
2. Install new VSIX file
3. Reload VS Code

## Support

- **Documentation**: [README.md](README.md)
- **CLI Guide**: [../docs/IBM_QUICK_START.md](../docs/IBM_QUICK_START.md)
- **Issues**: [GitHub Issues](https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues)

---

**Next Steps:**
- Read [README.md](README.md) for feature overview
- Try analyzing your COBOL codebase
- Configure copybook paths
- Adjust threshold to your needs
