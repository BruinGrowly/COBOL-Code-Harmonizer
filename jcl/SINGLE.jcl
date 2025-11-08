//SINGLE   JOB (ACCT),'ANALYZE SINGLE FILE',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//*
//*********************************************************************
//* COBOL Code Harmonizer - Single File Analysis
//*
//* Purpose: Analyze a single COBOL program
//*
//* Usage:
//*   1. Update COBOLSRC DD to point to your COBOL file
//*   2. Update REPORT DD for output location
//*   3. Submit this JCL
//*
//* Example:
//*   Analyze: MY.COBOL.LIB(CUSTOMER)
//*   Output:  /u/reports/customer_analysis.json
//*
//*********************************************************************
//*
//ANALYZE  EXEC PGM=BPXBATCH
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *
export PYTHONPATH=/u/cobol-harmonizer:$PYTHONPATH
export PATH=/usr/lpp/IBM/cyp/v3r9/pyz/bin:$PATH
export _BPXK_AUTOCVT=ON
export HARMONIZER_HOME=/u/cobol-harmonizer
/*
//STDPARM  DD *
SH cd /u/cobol-harmonizer;
   python3 cobol_harmonizer.py analyze \
   --input /u/prod/cobol/CUSTOMER.cbl \
   --output /u/reports/customer_analysis.json \
   --format json \
   --verbose
/*
//
