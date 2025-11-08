//NIGHTLY  JOB (ACCT),'NIGHTLY COBOL SCAN',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//*
//*********************************************************************
//* COBOL Code Harmonizer - Nightly Batch Analysis
//*
//* Purpose: Scheduled nightly scan of production COBOL codebase
//*
//* Schedule: Daily at 02:00 AM (via JES scheduler or TWS)
//*
//* Features:
//*   - Full codebase analysis
//*   - Baseline comparison (detect semantic drift)
//*   - Trend analysis (bugs over time)
//*   - Alert on new high-risk findings
//*   - Archive results to DB2
//*
//* Notification:
//*   - Email on completion
//*   - Alert if critical bugs found
//*   - Dashboard update for development team
//*
//* Retention:
//*   - Keep last 90 days of reports
//*   - Archive summary to DB2 indefinitely
//*
//*********************************************************************
//*
//*-------------------------------------------------------------------
//* STEP 1: Full Codebase Analysis
//*-------------------------------------------------------------------
//ANALYZE  EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON

# Harmonizer configuration
export HARMONIZER_HOME=/u/cobol-harmonizer
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook:/u/test/copybook
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
export HARMONIZER_AUDIT_LOG=/u/cobol-harmonizer/.audit_log

# Nightly scan settings
export HARMONIZER_COMPLIANCE=SOX,PCI_DSS,GDPR,HIPAA
export HARMONIZER_BASELINE_PATH=/u/cobol-harmonizer/baselines/production.json
export HARMONIZER_COMPARE_BASELINE=true
export HARMONIZER_ALERT_THRESHOLD=70
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   # Run enterprise analyzer
   python3 enterprise_analyzer.py \
   --input /u/prod/cobol \
   --output /u/reports/nightly/analysis_$(date +%Y%m%d).json \
   --compliance sox,pci_dss,gdpr,hipaa \
   --baseline /u/cobol-harmonizer/baselines/production.json \
   --compare-baseline \
   --alert-on-new-critical \
   --cache-dir /u/cobol-harmonizer/.cache \
   --verbose
/*
//*
//*-------------------------------------------------------------------
//* STEP 2: Compare with Baseline (Detect Drift)
//*-------------------------------------------------------------------
//COMPARE  EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 -m cobol_harmonizer.baseline.compare \
   --current /u/reports/nightly/analysis_$(date +%Y%m%d).json \
   --baseline /u/cobol-harmonizer/baselines/production.json \
   --output /u/reports/nightly/drift_$(date +%Y%m%d).json \
   --threshold 0.1 \
   --alert-on-drift
/*
//*
//*-------------------------------------------------------------------
//* STEP 3: Generate Trend Report (Last 30 Days)
//*-------------------------------------------------------------------
//TREND    EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 -m cobol_harmonizer.trends.analyzer \
   --input-dir /u/reports/nightly \
   --days 30 \
   --output /u/reports/trends/trend_$(date +%Y%m%d).html \
   --format html \
   --include-charts
/*
//*
//*-------------------------------------------------------------------
//* STEP 4: Archive Summary to DB2
//*-------------------------------------------------------------------
//DB2LOAD  EXEC PGM=IKJEFT01,COND=(0,NE,ANALYZE)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
DSN SYSTEM(DB2P)
RUN PROGRAM(DSNTIAD) PLAN(DSNTIA91) -
  LIB('SYS1.DB2.RUNLIB.LOAD')
END
//SYSIN    DD *
-- Archive nightly analysis summary
INSERT INTO HARMONIZER.NIGHTLY_SCANS
(SCAN_DATE, TOTAL_FILES, TOTAL_PROCEDURES,
 BUGS_FOUND, CRITICAL_BUGS, HIGH_BUGS, MEDIUM_BUGS, LOW_BUGS,
 SOX_PROCEDURES, PCI_PROCEDURES, GDPR_PROCEDURES,
 DRIFT_DETECTED, NEW_CRITICAL_BUGS,
 REPORT_PATH)
SELECT
    CURRENT_DATE,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    'N', 0,
    '/u/reports/nightly/analysis_' || VARCHAR_FORMAT(CURRENT_DATE,'YYYYMMDD') || '.json'
FROM SYSIBM.SYSDUMMY1;

-- Note: Replace 0 values with actual data parsed from JSON
-- Consider using external stored procedure or Python script to parse JSON

COMMIT;
/*
//*
//*-------------------------------------------------------------------
//* STEP 5: Check for Critical Bugs (Alert if Found)
//*-------------------------------------------------------------------
//ALERT    EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 -m cobol_harmonizer.alerts.checker \
   --report /u/reports/nightly/analysis_$(date +%Y%m%d).json \
   --critical-threshold 70 \
   --alert-command "/u/cobol-harmonizer/jcl/send_alert.sh" \
   --alert-recipients "dev-team@company.com,security@company.com"
/*
//*
//*-------------------------------------------------------------------
//* STEP 6: Cleanup Old Reports (Keep Last 90 Days)
//*-------------------------------------------------------------------
//CLEANUP  EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH cd /u/reports/nightly;
   # Delete reports older than 90 days
   find . -name 'analysis_*.json' -type f -mtime +90 -delete;
   find . -name 'drift_*.json' -type f -mtime +90 -delete;
   echo "Cleanup complete: Deleted reports older than 90 days"
/*
//*
//*-------------------------------------------------------------------
//* STEP 7: Send Summary Email
//*-------------------------------------------------------------------
//EMAIL    EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH /u/cobol-harmonizer/jcl/nightly_summary_email.sh
/*
//
