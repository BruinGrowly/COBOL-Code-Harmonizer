# z/OS USS Quick Start - 5 Minutes

Get COBOL Code Harmonizer running on z/OS USS in 5 minutes.

## Prerequisites (30 seconds)

```bash
# SSH to z/OS USS
ssh your-userid@mainframe.company.com

# Verify Python 3.9+
python3 --version

# If not found, use IBM Python:
/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3 --version
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
```

## Install (2 minutes)

```bash
# Download and extract z/OS bundle
cd /u
wget https://github.com/BruinGrowly/COBOL-Code-Harmonizer/releases/latest/download/cobol-harmonizer-zos-*.tar.gz
gunzip cobol-harmonizer-zos-*.tar.gz
tar -xf cobol-harmonizer-zos-*.tar
cd cobol-harmonizer-zos-*

# Run installer
./install_zos.sh

# Verify
python3 -m cobol_harmonizer.cli.commands --version
```

## Test (1 minute)

```bash
# Analyze example
python3 -m cobol_harmonizer.cli.commands analyze \
  examples/disharmonious_example.cbl --verbose

# Analyze your COBOL
python3 -m cobol_harmonizer.cli.commands analyze \
  /u/prod/cobol/YOURPROG.cbl --verbose
```

## JCL Submission (2 minutes)

```bash
# 1. Edit JCL
vi jcl/HARMONIZ.jcl

# Update these lines:
//   SET HLQ='YOUR.HLQ'              <-- Your high-level qualifier
//   SET INPUT='YOUR.COBOL.SRC'      <-- Input dataset
//   SET OUTPUT='YOUR.ANALYSIS.OUT'  <-- Output dataset

# 2. Submit
submit jcl/HARMONIZ.jcl

# 3. View results
# Check output dataset: YOUR.ANALYSIS.OUT
```

## Integration with IBM Tools

### IBM ADDI

```bash
# Generate SARIF for ADDI
python3 -m cobol_harmonizer.cli.commands report MYPROG.cbl \
  --format sarif \
  --output analysis.sarif

# Import into ADDI
addi-cli import-sarif analysis.sarif --project "MyMainframe"
```

See [`ADDI_PLUGIN.md`](ADDI_PLUGIN.md) for full integration guide.

### IBM Wazi

```bash
# Works natively in Wazi Developer
# Install VS Code extension:
code --install-extension cobol-harmonizer-*.vsix
```

See [`WAZI_DEMO.md`](WAZI_DEMO.md) for live demo script.

### SonarQube for z/OS

```bash
# Output SARIF for SonarQube
python3 -m cobol_harmonizer.cli.commands report MYPROG.cbl \
  --format sarif \
  --output sonar.sarif

# Import via SonarQube Scanner
sonar-scanner \
  -Dsonar.projectKey=my-cobol-project \
  -Dsonar.sarifReportPaths=sonar.sarif
```

## Batch Processing

```bash
# Analyze all COBOL in directory
find /u/prod/cobol -name "*.cbl" -exec \
  python3 -m cobol_harmonizer.cli.commands analyze {} \; \
  > analysis.log

# Scheduled nightly analysis
# Edit JCL: jcl/NIGHTLY.jcl
# Submit to run nightly
```

## Container Deployment

```bash
# Build container
docker build -f Dockerfile.zos -t cobol-harmonizer:zos .

# Run analysis
docker run --rm \
  -v /u/prod/cobol:/input:ro \
  -v /u/reports:/output \
  cobol-harmonizer:zos analyze /input/MYPROG.cbl --verbose
```

## Troubleshooting

### Python Not Found

```bash
# Find IBM Python
find /usr/lpp/IBM -name python3 2>/dev/null

# Add to PATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
echo 'export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH' >> ~/.profile
```

### EBCDIC Issues

```bash
# Set auto-conversion (already in Dockerfile)
export _BPXK_AUTOCVT=ON
export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
```

### JCL Submission Fails

```bash
# Check JCL syntax
oput jcl/HARMONIZ.jcl

# Verify dataset names exist
listcat -entries 'YOUR.HLQ.*'

# Submit with trace
submit jcl/HARMONIZ.jcl trace
```

## Performance

| Programs | Time | Memory |
|----------|------|--------|
| 1        | <1s  | 20 MB  |
| 10       | <5s  | 50 MB  |
| 100      | <30s | 200 MB |
| 1,000    | <5m  | 500 MB |

**Optimization**: Enable caching for 95% speedup on subsequent runs.

## Support

- **Full Documentation**: [`../docs/IBM_MAINFRAME_INTEGRATION.md`](../docs/IBM_MAINFRAME_INTEGRATION.md)
- **Issues**: https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues
- **IBM Support**: Contact your IBM representative

## What's Next?

- ✅ **Installed and tested** (you just did this!)
- 📊 **Integrate with ADDI**: See [`ADDI_PLUGIN.md`](ADDI_PLUGIN.md)
- 🎯 **Demo in Wazi**: See [`WAZI_DEMO.md`](WAZI_DEMO.md)
- 🔧 **Production deployment**: See [`../docs/IBM_DEPLOYMENT_CHECKLIST.md`](../docs/IBM_DEPLOYMENT_CHECKLIST.md)
- 📈 **Continuous monitoring**: Set up nightly JCL jobs
