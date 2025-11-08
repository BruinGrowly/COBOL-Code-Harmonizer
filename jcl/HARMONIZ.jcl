//HARMONIZ JOB (ACCT),'COBOL HARMONIZER',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//*
//*********************************************************************
//* COBOL Code Harmonizer - Semantic Analysis Batch Job
//*
//* Purpose: Analyze COBOL programs for semantic disharmony
//*
//* Input:
//*   COBOLSRC - COBOL source files to analyze
//*   COPYLIB  - Copybook library (optional)
//*   CONFIG   - Configuration file (optional)
//*
//* Output:
//*   REPORT   - Analysis report (JSON format)
//*   STDOUT   - Console output
//*   STDERR   - Error messages
//*
//* Parameters:
//*   PARM1 - Analysis type: SINGLE, BATCH, COMPLIANCE
//*   PARM2 - Output format: JSON, HTML, MARKDOWN, CSV
//*   PARM3 - Compliance frameworks: SOX,PCI,GDPR,HIPAA (comma-sep)
//*
//* Example:
//*   //HARMONIZ JOB ...
//*   // PARM='BATCH,JSON,SOX'
//*
//* Installation:
//*   1. Install Python 3.9+ on z/OS USS
//*   2. Install COBOL Harmonizer in /u/cobol-harmonizer
//*   3. Update paths below to match your environment
//*   4. Submit this JCL
//*
//* Author: COBOL Code Harmonizer Team
//* Version: 0.5.0
//* Date: 2025-11-08
//*********************************************************************
//*
//*-------------------------------------------------------------------
//* STEP 1: Run Semantic Analysis via BPXBATCH
//*-------------------------------------------------------------------
//ANALYZE  EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
# Environment variables for COBOL Harmonizer
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON

# Harmonizer configuration
export HARMONIZER_HOME=/u/cobol-harmonizer
export HARMONIZER_COPYBOOK_PATH=/u/prod/copybook
export HARMONIZER_CACHE_DIR=/u/cobol-harmonizer/.cache
export HARMONIZER_LOG_DIR=/u/cobol-harmonizer/logs

# Optional: Set compliance frameworks
export HARMONIZER_COMPLIANCE=SOX,PCI_DSS,GDPR,HIPAA

# Optional: Enable audit logging
export HARMONIZER_AUDIT_LOG=/u/cobol-harmonizer/.audit_log
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   /u/cobol-harmonizer/jcl/harmonizer_wrapper.sh
/*
//COBOLSRC DD PATH='/u/prod/cobol',
//         PATHOPTS=(ORDONLY),
//         PATHDISP=(KEEP,KEEP)
//COPYLIB  DD PATH='/u/prod/copybook',
//         PATHOPTS=(ORDONLY),
//         PATHDISP=(KEEP,KEEP)
//REPORT   DD PATH='/u/reports/harmonizer_&SYSUID._&DATE._&TIME..json',
//         PATHOPTS=(OWRONLY,OCREAT,OTRUNC),
//         PATHMODE=(SIRUSR,SIWUSR,SIRGRP),
//         PATHDISP=(KEEP,DELETE)
//CONFIG   DD *,SYMBOLS=EXECSYS
{
  "analysis_type": "batch",
  "output_format": "json",
  "compliance_frameworks": ["sox", "pci_dss", "gdpr", "hipaa"],
  "thresholds": {
    "disharmony_min": 0.5,
    "risk_score_min": 40
  },
  "features": {
    "semantic_analysis": true,
    "compliance_tagging": true,
    "risk_assessment": true,
    "call_graph": true,
    "baseline_comparison": false
  },
  "output_options": {
    "include_harmonious": false,
    "max_findings": 100,
    "sort_by": "risk_score"
  }
}
/*
//*
//*-------------------------------------------------------------------
//* STEP 2: Optional - Generate HTML Report
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
   python3 -m cobol_harmonizer.cli convert-report \
   --input /u/reports/harmonizer_&SYSUID._&DATE._&TIME..json \
   --output /u/reports/harmonizer_&SYSUID._&DATE._&TIME..html \
   --format html
/*
//*
//*-------------------------------------------------------------------
//* STEP 3: Optional - Archive Results to DB2
//*-------------------------------------------------------------------
//ARCHIVE  EXEC PGM=IKJEFT01,COND=(0,NE,ANALYZE)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
DSN SYSTEM(DB2P)
RUN PROGRAM(DSNTIAD) PLAN(DSNTIA91) -
  LIB('SYS1.DB2.RUNLIB.LOAD')
END
//SYSIN    DD *
-- Load analysis results into DB2
-- Table: HARMONIZER.ANALYSIS_RESULTS

INSERT INTO HARMONIZER.ANALYSIS_RESULTS
(ANALYSIS_DATE, USER_ID, PROJECT_NAME,
 TOTAL_FILES, TOTAL_PROCEDURES, BUGS_FOUND,
 CRITICAL_BUGS, HIGH_BUGS, MEDIUM_BUGS, LOW_BUGS,
 ESTIMATED_COST_MIN, ESTIMATED_COST_MAX,
 REPORT_PATH)
VALUES
(CURRENT_DATE, CURRENT_SQLID, 'PRODUCTION_COBOL',
 0, 0, 0, 0, 0, 0, 0, 0, 0,
 '/u/reports/harmonizer_&SYSUID._&DATE._&TIME..json');

-- Note: Replace 0 values with actual counts from report
-- This is a template - actual values should be parsed from JSON
/*
//
