//SOXAUDIT JOB (ACCT),'SOX COMPLIANCE AUDIT',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//*
//*********************************************************************
//* COBOL Code Harmonizer - SOX Compliance Audit
//*
//* Purpose: Generate SOX compliance report for quarterly audit
//*
//* Features:
//*   - Identifies SOX-critical procedures automatically
//*   - Risk assessment for financial reporting code
//*   - Audit trail logging (who/what/when/why)
//*   - Multiple output formats (JSON, HTML, CSV)
//*
//* Schedule: Run quarterly (Jan, Apr, Jul, Oct)
//*
//* Output:
//*   - JSON report for archival
//*   - HTML dashboard for auditors
//*   - CSV export for Excel analysis
//*   - Audit log entries
//*
//*********************************************************************
//*
//*-------------------------------------------------------------------
//* STEP 1: Run SOX Compliance Analysis
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
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook
export HARMONIZER_AUDIT_LOG=/u/cobol-harmonizer/.audit_log

# SOX-specific settings
export HARMONIZER_COMPLIANCE=SOX
export HARMONIZER_RISK_THRESHOLD=40
export HARMONIZER_USER=$LOGNAME
export HARMONIZER_JUSTIFICATION="Q4 2025 SOX 404 Audit"
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 -m cobol_harmonizer.compliance.sox_audit \
   --input /u/prod/cobol/financial \
   --output /u/reports/sox/Q4_2025_sox_analysis.json \
   --compliance sox \
   --risk-threshold 40 \
   --include-audit-trail \
   --justification "Q4 2025 SOX 404 Audit" \
   --user "$LOGNAME"
/*
//*
//*-------------------------------------------------------------------
//* STEP 2: Generate HTML Dashboard for Auditors
//*-------------------------------------------------------------------
//HTMLRPT  EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 -m cobol_harmonizer.compliance.audit_reporter \
   --input /u/reports/sox/Q4_2025_sox_analysis.json \
   --output /u/reports/sox/Q4_2025_sox_dashboard.html \
   --format html \
   --title "Q4 2025 SOX 404 Compliance Audit" \
   --include-executive-summary
/*
//*
//*-------------------------------------------------------------------
//* STEP 3: Generate CSV Export for Excel
//*-------------------------------------------------------------------
//CSVRPT   EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 -m cobol_harmonizer.compliance.audit_reporter \
   --input /u/reports/sox/Q4_2025_sox_analysis.json \
   --output /u/reports/sox/Q4_2025_sox_procedures.csv \
   --format csv
/*
//*
//*-------------------------------------------------------------------
//* STEP 4: Email Report to Audit Team
//*-------------------------------------------------------------------
//EMAIL    EXEC PGM=BPXBATCH,COND=(0,NE,ANALYZE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH /u/cobol-harmonizer/jcl/email_report.sh \
   --to audit-team@company.com \
   --subject "Q4 2025 SOX Compliance Report - COBOL Analysis" \
   --body "Please find attached the quarterly SOX compliance analysis." \
   --attach /u/reports/sox/Q4_2025_sox_dashboard.html \
   --attach /u/reports/sox/Q4_2025_sox_procedures.csv
/*
//*
//*-------------------------------------------------------------------
//* STEP 5: Archive to Long-Term Storage
//*-------------------------------------------------------------------
//ARCHIVE  EXEC PGM=IEBGENER,COND=(0,NE,ANALYZE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD PATH='/u/reports/sox/Q4_2025_sox_analysis.json',
//         PATHOPTS=(ORDONLY)
//SYSUT2   DD DSN=AUDIT.SOX.Q4.2025.JSON,
//         DISP=(NEW,CATA,DELETE),
//         SPACE=(TRK,(10,5)),
//         DCB=(RECFM=VB,LRECL=32760,BLKSIZE=32764)
//
