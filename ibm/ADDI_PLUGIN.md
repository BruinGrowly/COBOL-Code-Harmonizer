# IBM ADDI Integration - 5 Minute Setup

**IBM Application Discovery and Delivery Intelligence (ADDI)** integration for COBOL Code Harmonizer.

## Quick Install

### Step 1: Deploy Plugin (2 minutes)

```bash
# On your ADDI server
cd /opt/ibm/addi/plugins

# Copy plugin manifest
cp /path/to/cobol-harmonizer/ibm/addi-plugin.xml ./cobol-harmonizer/

# Install COBOL Harmonizer
pip install cobol-harmonizer

# Restart ADDI
systemctl restart ibm-addi
```

### Step 2: Configure in ADDI UI (2 minutes)

1. Open ADDI web console: `https://your-addi-server:9443/addi`
2. Navigate to **Settings → Analyzers**
3. Enable **COBOL Semantic Harmony Analyzer**
4. Configure thresholds:
   - **Critical**: 1.0 (procedure name completely contradicts implementation)
   - **Significant**: 0.7 (major semantic shift)
   - **Concerning**: 0.5 (notable contradiction)
   - **Minor**: 0.3 (slight drift)

### Step 3: Run Analysis (1 minute)

```bash
# Via ADDI UI
1. Create new project
2. Import COBOL source
3. Run "COBOL Semantic Harmony Analyzer"
4. View results in ADDI dashboard

# Via CLI
addi-cli analyze \
  --project "MyMainframe" \
  --analyzer "com.cobol.harmonizer.semantic" \
  --input /path/to/cobol \
  --output /path/to/results
```

## SARIF Output for ADDI

COBOL Code Harmonizer outputs SARIF 2.1.0 format, fully compatible with ADDI:

```bash
# Generate SARIF report
python -m cobol_harmonizer.cli.commands report MYPROG.cbl \
  --format sarif \
  --output analysis.sarif

# Import into ADDI
addi-cli import-sarif analysis.sarif --project "MyMainframe"
```

## ADDI Dashboard Views

Once integrated, you'll see:

### 1. **Disharmony Heatmap**
- Color-coded procedures by disharmony score
- Red = Critical (1.0+), Orange = Significant (0.7+), Yellow = Concerning (0.5+)

### 2. **Semantic Flow Diagram**
- Visualize LJPW coordinate trajectories
- Shows intent → execution shifts

### 3. **Risk Assessment**
- Compliance tags (SOX, PCI-DSS, GDPR, HIPAA)
- Priority recommendations

### 4. **Trend Analysis**
- Track disharmony over time
- Identify deteriorating code quality

## Integration Points

### Works With

- ✅ **IBM ADDI 6.x** - Full integration
- ✅ **IBM Debug for z/OS** - SARIF import
- ✅ **SonarQube for z/OS** - Quality gates
- ✅ **IBM Wazi** - VSCode extension compatibility

### Output Formats

| Format | ADDI Support | Use Case |
|--------|--------------|----------|
| SARIF 2.1.0 | ✅ Native | Import into ADDI dashboard |
| JSON | ✅ Via adapter | Custom integrations |
| Console | ❌ Manual only | Quick CLI analysis |

## Advanced Configuration

### Custom Rules in ADDI

```xml
<!-- In ADDI project settings -->
<quality-gate>
  <rule analyzer="com.cobol.harmonizer.semantic">
    <condition>
      <metric>disharmony_score</metric>
      <operator>greater_than</operator>
      <threshold>0.7</threshold>
      <severity>blocker</severity>
    </condition>
  </rule>

  <rule analyzer="com.cobol.harmonizer.semantic">
    <condition>
      <metric>compliance_risk_level</metric>
      <operator>equals</operator>
      <value>critical</value>
      <severity>blocker</severity>
    </condition>
  </rule>
</quality-gate>
```

### Continuous Integration

```yaml
# Jenkins pipeline for ADDI
stages:
  - stage: Analyze COBOL
    steps:
      - sh: |
          python -m cobol_harmonizer.cli.commands report \
            ${WORKSPACE}/cobol \
            --format sarif \
            --output harmonizer.sarif

      - sh: |
          addi-cli import-sarif harmonizer.sarif \
            --project "${JOB_NAME}" \
            --fail-on-critical

  - stage: ADDI Quality Gate
    steps:
      - sh: addi-cli check-quality-gate --project "${JOB_NAME}"
```

## Troubleshooting

### Plugin Not Loading

```bash
# Check ADDI logs
tail -f /var/log/ibm/addi/analyzer.log

# Verify Python path
which python3
python3 -m cobol_harmonizer.cli.commands --version

# Re-register plugin
addi-cli register-plugin /path/to/addi-plugin.xml
```

### SARIF Import Fails

```bash
# Validate SARIF format
python -m sarif validate harmonizer.sarif

# Check ADDI import logs
grep "cobol.harmonizer" /var/log/ibm/addi/import.log
```

### Performance Issues

```bash
# Enable caching in ADDI
# In addi-plugin.xml:
<parameter name="enable_cache" type="boolean" default="true"/>

# Increase worker threads
<parameter name="max_workers" type="int" default="4"/>
```

## Support

- **ADDI Documentation**: See IBM ADDI User Guide
- **COBOL Harmonizer Issues**: https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues
- **IBM Support**: Contact your IBM representative

## Next Steps

- ✅ Install plugin (above)
- 📊 Run on sample COBOL: `examples/`
- 🔧 Configure quality gates
- 📈 Set up continuous monitoring
- 📚 Read full docs: `../docs/IBM_MAINFRAME_INTEGRATION.md`
