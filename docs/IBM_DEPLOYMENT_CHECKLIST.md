# IBM Deployment Checklist

**COBOL Code Harmonizer - Enterprise Deployment Guide for IBM z/OS**

Version: 1.0
Date: 2025-11-08
Target Platforms: IBM z/OS USS, Linux on Z, Windows/Mac (for development)

---

## Pre-Deployment Assessment

### ☑ Business Readiness

- [ ] **Executive Approval**
  - [ ] Budget approved for deployment
  - [ ] Business case reviewed (see IBM_PROOF_OF_VALUE.md)
  - [ ] ROI calculator completed (see ROI_CALCULATOR.md)
  - [ ] Stakeholders identified and informed

- [ ] **Team Readiness**
  - [ ] COBOL development team trained
  - [ ] System administrators briefed
  - [ ] Support team prepared
  - [ ] Change management process followed

- [ ] **Environment Assessment**
  - [ ] Python 3.9+ available on z/OS USS
  - [ ] Git available (or alternative transfer method)
  - [ ] Network connectivity verified
  - [ ] Storage capacity confirmed (minimum 500MB)

---

## Phase 1: Pilot Installation (Week 1-2)

### ☑ 1.1 Development Environment Setup

- [ ] **Install on Development Workstation**
  ```bash
  # Windows/Mac/Linux workstation
  git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git
  cd COBOL-Code-Harmonizer
  pip install -r requirements.txt
  python3 cobol_harmonizer.py --version
  ```

- [ ] **Verify Installation**
  ```bash
  # Run example analysis
  python3 cobol_harmonizer.py analyze \
    examples/disharmonious_example.cbl \
    --output test-results.json
  ```

- [ ] **Run Test Suite**
  ```bash
  pytest tests/ -v
  # Target: 85%+ tests passing
  ```

### ☑ 1.2 Sample Analysis

- [ ] **Analyze 5-10 Sample COBOL Programs**
  - [ ] Select representative programs
  - [ ] Run analysis and review results
  - [ ] Validate findings with COBOL team
  - [ ] Document false positives (if any)

- [ ] **Baseline Metrics**
  - [ ] Total procedures analyzed: _______
  - [ ] Bugs found: _______
  - [ ] Average disharmony score: _______
  - [ ] Critical issues: _______

---

## Phase 2: z/OS USS Installation (Week 2-3)

### ☑ 2.1 USS Environment Preparation

- [ ] **Verify Python Installation**
  ```bash
  # SSH to z/OS USS
  python3 --version  # Should be 3.9+
  which python3      # Note path: _______________
  ```

- [ ] **Create Installation Directory**
  ```bash
  mkdir -p /u/cobol-harmonizer
  cd /u/cobol-harmonizer
  chmod 755 /u/cobol-harmonizer
  ```

- [ ] **Transfer Files to z/OS**

  **Option A: Git** (Preferred)
  ```bash
  git clone https://github.com/BruinGrowly/COBOL-Code-Harmonizer.git .
  ```

  **Option B: FTP**
  ```bash
  # From workstation:
  ftp zos.company.com
  binary
  cd /u/cobol-harmonizer
  put cobol_harmonizer.py
  put -r cobol_harmonizer/
  ```

  **Option C: SCP**
  ```bash
  scp -r COBOL-Code-Harmonizer/ user@zos:/u/cobol-harmonizer/
  ```

- [ ] **Set File Permissions**
  ```bash
  chmod +x cobol_harmonizer.py
  chmod +x jcl/*.sh
  find . -type f -name "*.py" -exec chmod 644 {} \;
  ```

### ☑ 2.2 USS Installation Verification

- [ ] **Test CLI**
  ```bash
  python3 /u/cobol-harmonizer/cobol_harmonizer.py --version
  ```

- [ ] **Test Analysis**
  ```bash
  python3 /u/cobol-harmonizer/cobol_harmonizer.py analyze \
    /u/prod/cobol/SAMPLE.cbl \
    --output /tmp/test-analysis.json
  ```

- [ ] **Review Output**
  ```bash
  cat /tmp/test-analysis.json | grep disharmony_score
  ```

---

## Phase 3: JCL Integration (Week 3-4)

### ☑ 3.1 JCL Template Customization

- [ ] **Customize HARMONIZ.jcl**
  - [ ] Update Python path: `_______________`
  - [ ] Update installation path: `_______________`
  - [ ] Update COBOL source path: `_______________`
  - [ ] Update output path: `_______________`
  - [ ] Test with sample program

- [ ] **Customize SOXAUDIT.jcl** (if using compliance features)
  - [ ] Configure email notifications
  - [ ] Set audit report paths
  - [ ] Configure DB2 archival (optional)

- [ ] **Customize NIGHTLY.jcl** (for scheduled scans)
  - [ ] Set schedule in job scheduler
  - [ ] Configure baseline comparison
  - [ ] Set retention policy

### ☑ 3.2 JCL Testing

- [ ] **Submit Test Job**
  ```jcl
  //TESTJOB  JOB (ACCT),'HARMONIZER TEST',
  //         CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
  //STEP1    EXEC PGM=BPXBATCH
  //STDOUT   DD  SYSOUT=*
  //STDERR   DD  SYSOUT=*
  //STDENV   DD *
  export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
  export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
  export _BPXK_AUTOCVT=ON
  /*
  //STDPARM  DD *
  SH cd /u/cobol-harmonizer;
     python3 cobol_harmonizer.py --version
  /*
  ```

- [ ] **Verify Job Completion**
  - [ ] RC = 0
  - [ ] Output shows version number
  - [ ] No error messages

---

## Phase 4: Integration with Development Workflow (Week 4-5)

### ☑ 4.1 VS Code Extension Deployment

- [ ] **Package Extension**
  ```bash
  cd vscode-extension
  npm install -g vsce
  vsce package
  ```

- [ ] **Distribute to Developers**
  - [ ] Upload VSIX to internal repository
  - [ ] Send installation instructions
  - [ ] Schedule training session

- [ ] **Developer Setup**
  - [ ] Install extension
  - [ ] Configure settings
  - [ ] Test with sample files
  - [ ] Provide feedback

### ☑ 4.2 CI/CD Integration

- [ ] **GitHub Actions** (if using GitHub)
  - [ ] Copy `.github/workflows/ci.yml`
  - [ ] Customize for internal environment
  - [ ] Test workflow

- [ ] **Jenkins** (if using Jenkins)
  ```groovy
  stage('COBOL Analysis') {
    steps {
      sh '''
        python3 /u/cobol-harmonizer/cobol_harmonizer.py analyze \
          --input ${WORKSPACE}/cobol \
          --output analysis.json \
          --threshold 0.8
      '''
    }
  }
  ```

- [ ] **GitLab CI** (if using GitLab)
  ```yaml
  cobol_analysis:
    script:
      - python3 cobol_harmonizer.py analyze --input src/ --output analysis.json
    artifacts:
      reports:
        codequality: analysis.json
  ```

### ☑ 4.3 IBM RAD Integration

- [ ] **Configure External Tool**
  - [ ] Tool Name: COBOL Harmonizer
  - [ ] Location: `/u/cobol-harmonizer/cobol_harmonizer.py`
  - [ ] Arguments: `analyze --input ${selected_resource_loc}`
  - [ ] Test integration

---

## Phase 5: Production Deployment (Week 5-6)

### ☑ 5.1 Production Environment Setup

- [ ] **Create Production Directory**
  ```bash
  mkdir -p /u/prod/cobol-harmonizer
  chmod 755 /u/prod/cobol-harmonizer
  ```

- [ ] **Install Production Instance**
  ```bash
  cp -r /u/cobol-harmonizer/* /u/prod/cobol-harmonizer/
  ```

- [ ] **Set Production Permissions**
  ```bash
  chown -R produser:prodgroup /u/prod/cobol-harmonizer
  chmod -R 555 /u/prod/cobol-harmonizer  # Read-only
  ```

### ☑ 5.2 Batch Processing Setup

- [ ] **Schedule Nightly Scans**
  - [ ] Submit NIGHTLY.jcl to job scheduler
  - [ ] Frequency: Daily at 2 AM
  - [ ] Retention: 90 days

- [ ] **Configure Alerts**
  - [ ] Email notifications for critical issues
  - [ ] Slack/Teams integration (optional)
  - [ ] Dashboard integration (optional)

### ☑ 5.3 Compliance Integration

- [ ] **SOX Compliance** (if applicable)
  - [ ] Schedule quarterly audits
  - [ ] Configure audit logging
  - [ ] Set up report archival

- [ ] **PCI-DSS Compliance** (if applicable)
  - [ ] Tag payment-related procedures
  - [ ] Schedule monthly scans
  - [ ] Configure security reports

---

## Phase 6: Training & Documentation (Week 6-7)

### ☑ 6.1 Team Training

- [ ] **COBOL Developers (2-hour session)**
  - [ ] Introduction to semantic analysis
  - [ ] VS Code extension demo
  - [ ] Interpreting results
  - [ ] Best practices
  - [ ] Q&A session

- [ ] **System Administrators (1-hour session)**
  - [ ] Installation and maintenance
  - [ ] JCL jobs management
  - [ ] Troubleshooting
  - [ ] Backup and recovery

- [ ] **Management (30-minute presentation)**
  - [ ] ROI metrics
  - [ ] Success stories
  - [ ] Reporting dashboards
  - [ ] Future roadmap

### ☑ 6.2 Documentation

- [ ] **Create Internal Wiki**
  - [ ] Installation guide
  - [ ] User guide
  - [ ] Troubleshooting guide
  - [ ] FAQ

- [ ] **Distribute Reference Materials**
  - [ ] Quick start guide
  - [ ] JCL templates
  - [ ] VS Code extension guide
  - [ ] Compliance guide

---

## Phase 7: Monitoring & Optimization (Ongoing)

### ☑ 7.1 Performance Monitoring

- [ ] **Track Metrics**
  - [ ] Analysis time per file: _______ ms
  - [ ] Batch job duration: _______ minutes
  - [ ] False positive rate: _______ %
  - [ ] User satisfaction: _______ /10

- [ ] **Optimize Performance**
  - [ ] Adjust max_workers setting
  - [ ] Enable caching
  - [ ] Tune threshold values

### ☑ 7.2 Quality Assurance

- [ ] **Weekly Reviews**
  - [ ] Review critical findings
  - [ ] Validate against manual reviews
  - [ ] Track resolution rate

- [ ] **Monthly Audits**
  - [ ] Codebase health trends
  - [ ] ROI measurement
  - [ ] Team feedback
  - [ ] Process improvements

### ☑ 7.3 Continuous Improvement

- [ ] **Quarterly Updates**
  - [ ] Check for new releases
  - [ ] Review changelog
  - [ ] Test in dev environment
  - [ ] Deploy to production

- [ ] **Feature Requests**
  - [ ] Collect user feedback
  - [ ] Submit GitHub issues
  - [ ] Participate in discussions

---

## Success Criteria

### ✅ Technical Success

- [ ] 95%+ job completion rate
- [ ] < 5% false positive rate
- [ ] Analysis time < 10 seconds per file
- [ ] Zero production incidents
- [ ] 85%+ test coverage maintained

### ✅ Business Success

- [ ] ROI > 100x within 6 months
- [ ] 10+ critical bugs found and fixed
- [ ] 80%+ developer adoption
- [ ] Compliance audit findings = 0
- [ ] Technical debt reduced by 20%

### ✅ User Satisfaction

- [ ] Developer satisfaction > 8/10
- [ ] Management satisfaction > 8/10
- [ ] Support ticket volume < 5/month
- [ ] Training completion rate > 90%

---

## Rollback Plan

### If Issues Occur

1. **Stop Scheduled Jobs**
   ```bash
   # Cancel nightly jobs
   # Notify stakeholders
   ```

2. **Revert to Previous Process**
   - [ ] Disable VS Code extension
   - [ ] Stop JCL jobs
   - [ ] Document issues

3. **Root Cause Analysis**
   - [ ] Collect logs
   - [ ] Review error messages
   - [ ] Identify failure point

4. **Fix and Retry**
   - [ ] Address root cause
   - [ ] Test in dev environment
   - [ ] Gradual rollout

---

## Support Contacts

### Internal Contacts

- **Project Owner**: _____________________
- **Technical Lead**: _____________________
- **System Administrator**: _____________________
- **Support Team**: _____________________

### External Resources

- **GitHub Issues**: https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues
- **Documentation**: See `docs/` directory
- **Community**: GitHub Discussions

---

## Appendix A: Configuration Templates

### `.harmonizerrc.json`

```json
{
  "version": "0.5.0",
  "analysis": {
    "threshold": 0.5,
    "show_harmonious": false
  },
  "batch": {
    "max_workers": 8,
    "recursive": true,
    "file_patterns": ["*.cbl", "*.CBL", "*.cob", "*.COB"]
  },
  "reporting": {
    "default_format": "json",
    "include_suggestions": true
  }
}
```

### VS Code Settings

```json
{
  "cobolHarmonizer.pythonPath": "/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3",
  "cobolHarmonizer.harmonizerPath": "/u/cobol-harmonizer/cobol_harmonizer.py",
  "cobolHarmonizer.disharmonyThreshold": 0.5,
  "cobolHarmonizer.analyzeOnSave": true,
  "cobolHarmonizer.copybookPaths": [
    "/u/prod/cobol/copybook",
    "/SYS1.COPYLIB"
  ]
}
```

---

## Appendix B: Troubleshooting Quick Reference

| Issue | Solution |
|-------|----------|
| Python not found | Check PATH, verify Python 3.9+ installed |
| Permission denied | Check file ownership and permissions (chmod 755) |
| Module not found | Verify PYTHONPATH includes installation directory |
| EBCDIC errors | Ensure `_BPXK_AUTOCVT=ON` is set |
| Slow analysis | Increase max_workers, enable caching |
| False positives | Adjust threshold, review procedure logic |
| JCL job fails | Check RC, review STDOUT/STDERR, verify paths |

---

## Appendix C: Metrics Dashboard Template

Track these KPIs monthly:

| Metric | Target | Actual | Trend |
|--------|--------|--------|-------|
| Files Analyzed | 10,000+ | _____ | _____ |
| Bugs Found | 100+ | _____ | _____ |
| Critical Issues | < 10 | _____ | _____ |
| Avg Disharmony | < 0.4 | _____ | _____ |
| Developer Adoption | > 80% | _____ | _____ |
| ROI | > 100x | _____ | _____ |

---

**Deployment Status**: ☐ Not Started  ☐ In Progress  ☐ Complete
**Deployment Date**: _______________
**Sign-off**: _______________

---

*This checklist is based on COBOL Code Harmonizer v0.5.0+*
*For latest version, see: https://github.com/BruinGrowly/COBOL-Code-Harmonizer*
