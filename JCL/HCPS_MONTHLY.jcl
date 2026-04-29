//HCPSMNTH JOB (HCPS,PROD,M001),
//             'HCPS MONTHLY BATCH',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             REGION=0M,
//             TIME=1440,
//             NOTIFY=&SYSUID,
//             TYPRUN=SCAN,
//             RESTART=*
//*
//*================================================================*
//*  HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)                    *
//*  MONTHLY BATCH CYCLE                                           *
//*                                                                *
//*  SCHEDULE: 1ST BUSINESS DAY OF MONTH - 02:00 AM EST           *
//*  ON-CALL : HCPS BATCH SUPPORT (EXT 4400)                      *
//*  RUNBOOK : HCPS-OPS-002                                        *
//*                                                                *
//*  STEP SUMMARY:                                                 *
//*    STEP010 - HCPRVMNT  Provider File Maintenance (OIG/SAM)     *
//*    STEP020 - HCACCRCN  Benefit Accumulator Reconciliation      *
//*    STEP030 - HCMNTHCL  Monthly Close Processing                *
//*    STEP040 - HCCAPGEN  Capitation Payment Generation           *
//*    STEP050 - HCPREMRC  Premium Reconciliation                  *
//*    STEP060 - HC1099AC  1099 Accrual Update                     *
//*    STEP070 - HCARCHIV  Data Archive (Aged Claims)              *
//*    STEP080 - HCRPTGEN  Monthly Management Reports              *
//*                                                                *
//*  DEPENDENCIES:                                                 *
//*    - ALL DAILY BATCHES FOR MONTH MUST COMPLETE                 *
//*    - OIG/SAM EXCLUSION FILES REFRESHED                         *
//*    - ENROLLMENT CENSUS FINALIZED                               *
//*                                                                *
//*  CHANGE LOG:                                                   *
//*    05/1995 R.PETERSON  INITIAL JCL CREATION                    *
//*    06/1999 M.CHAMBERS  Y2K DATE REMEDIATION                    *
//*    01/2004 S.NAKAMURA  ADD ARCHIVE/PURGE STEPS                 *
//*    09/2008 K.PATEL     ADD REORG STEP                          *
//*    06/2014 D.CHEN      ICD-10 PROVIDER UPDATES                 *
//*    02/2020 A.JOHNSON   ADD TELEHEALTH PROVIDER TYPES           *
//*    04/2022 S.CHEN      ADD CAPITATION PROCESSING              *
//*    07/2023 J.WILLIAMS  ADD OIG/SAM EXCLUSION MATCH            *
//*    01/2025 A.PATEL     ADD PREMIUM RECONCILIATION             *
//*================================================================*
//*
//         JCLLIB ORDER=(HCPS.PROD.PROCLIB,
//                       HCPS.PROD.JCLLIB,
//                       SYS1.PROCLIB)
//*
//JOBLIB   DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//         DD DSN=SYS1.DSNLOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//*================================================================*
//*  STEP010 - PROVIDER FILE MAINTENANCE (OIG/SAM EXCLUSION)       *
//*  Program: HCPRVMNT                                             *
//*  Purpose: Match provider file against OIG exclusion list       *
//*           and SAM (System for Award Management) database.      *
//*           Flag excluded providers, update status,              *
//*           process credentialing updates, validate NPI.         *
//*================================================================*
//*
//STEP010  EXEC PGM=HCPRVMNT,
//             REGION=512M,
//             TIME=60,
//             PARM='MONTHLY,PROD,EXCL=FULL'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: OIG EXCLUSION LIST (UPDATED MONTHLY FROM OIG.GOV) ---
//OIGFILE  DD DSN=HCPS.PROD.REFERENCE.OIG.EXCLUSION,
//            DISP=SHR
//*
//*--- INPUT: SAM EXCLUSION LIST ---
//SAMFILE  DD DSN=HCPS.PROD.REFERENCE.SAM.EXCLUSION,
//            DISP=SHR
//*
//*--- INPUT: NPPES FULL REPLACEMENT FILE ---
//NPPESFIL DD DSN=HCPS.PROD.REFERENCE.NPPES.FULL,
//            DISP=SHR
//*
//*--- INPUT: PROVIDER MAINTENANCE TRANSACTIONS ---
//PROVTRAN DD DSN=HCPS.PROD.PROVIDER.MONTHLY.TRANS,
//            DISP=OLD
//*
//*--- VSAM PROVIDER FILES ---
//PRVMASTR DD DSN=HCPS.PROD.VSAM.PROVMASTER,
//            DISP=SHR
//PRVCONTR DD DSN=HCPS.PROD.VSAM.PROVCONTRACT,
//            DISP=SHR
//NTWKTBL  DD DSN=HCPS.PROD.VSAM.NETWORKTBL,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//EXCLRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//EXCLFLAG DD DSN=HCPS.PROD.PROVIDER.EXCLUDED.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//PROVERR  DD DSN=HCPS.PROD.PROVIDER.MONTHLY.ERRORS(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//PRVAUDIT DD DSN=HCPS.PROD.PROVIDER.AUDIT.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//PROVRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSABEND DD SYSOUT=*
//*
//SYSIN    DD *
 PROCESS-TYPE=MONTHLY
 VALIDATE-OIG=Y
 VALIDATE-SAM=Y
 CRED-EXPIRY-DAYS=90
 NPI-VALIDATE=Y
 FLAG-SANCTIONED=Y
 HOLD-PAYMENTS-EXCLUDED=Y
 NOTIFICATION-COMPLIANCE=Y
/*
//*
//*================================================================*
//*  STEP020 - BENEFIT ACCUMULATOR RECONCILIATION                  *
//*  Program: HCACCRCN                                             *
//*  Purpose: Reconcile benefit accumulators against paid           *
//*           claims totals. Identify and correct                  *
//*           discrepancies. Reset monthly counters.               *
//*================================================================*
//*
//STEP020  EXEC PGM=HCACCRCN,
//             REGION=256M,
//             TIME=30,
//             PARM='MONTHLY,PROD',
//             COND=(4,LT,STEP010)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- VSAM FILES ---
//BENACC   DD DSN=HCPS.PROD.VSAM.BENACCUM,
//            DISP=SHR
//MBRMASTR DD DSN=HCPS.PROD.VSAM.MEMBERMASTER,
//            DISP=SHR
//*
//*--- INPUT/OUTPUT: MEMBER ACCUMULATORS ---
//ACCUFILE DD DSN=HCPS.PROD.MEMBER.ACCUMULATORS,
//            DISP=OLD
//*
//*--- BACKUP BEFORE RECONCILIATION ---
//ACCUBKUP DD DSN=HCPS.PROD.MEMBER.ACCUM.BACKUP(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//*--- BENEFIT PLAN REFERENCE ---
//BENEPLAN DD DSN=HCPS.PROD.BENEFIT.PLAN.MASTER,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//ACCMRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//ACCDISCP DD DSN=HCPS.PROD.ACCUM.DISCREPANCY.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//ACCCORR  DD DSN=HCPS.PROD.ACCUM.CORRECTIONS.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 RESET-TYPE=MONTHLY
 RESET-VISIT-LIMITS=Y
 RESET-DOLLAR-MAX=Y
 RESET-RX-COUNTS=Y
 RESET-THERAPY-VISITS=Y
 BACKUP-BEFORE-RESET=Y
 TOLERANCE-PCT=0.01
/*
//*
//*================================================================*
//*  STEP030 - MONTHLY CLOSE PROCESSING                            *
//*  Program: HCMNTHCL                                             *
//*  Purpose: Close out the monthly accounting period.             *
//*           Finalize IBNR estimates. Generate GL journal         *
//*           entries. Lock period from further posting.           *
//*================================================================*
//*
//STEP030  EXEC PGM=HCMNTHCL,
//             REGION=256M,
//             TIME=30,
//             PARM='MONTHLY,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//GLENTRIES DD DSN=HCPS.PROD.GL.ENTRIES.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//IBNRRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//CLOSERPT DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSIN    DD *
 CLOSE-PERIOD=CURRENT
 IBNR-METHOD=CHAIN-LADDER
 IBNR-DEVELOPMENT-MONTHS=24
 GL-INTERFACE=SAP
 LOCK-AFTER-CLOSE=Y
 RECONCILE-PAYMENTS=Y
 RECONCILE-RESERVES=Y
/*
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*================================================================*
//*  STEP040 - CAPITATION PAYMENT GENERATION                       *
//*  Program: HCCAPGEN                                             *
//*  Purpose: Generate monthly capitation payments for             *
//*           contracted providers. Calculate PMPM payments        *
//*           based on enrolled membership counts by               *
//*           risk category and age/sex band.                      *
//*================================================================*
//*
//STEP040  EXEC PGM=HCCAPGEN,
//             REGION=256M,
//             TIME=30,
//             PARM='MONTHLY,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- VSAM REFERENCE FILES ---
//CAPRTTBL DD DSN=HCPS.PROD.VSAM.CAPRATESTBL,
//            DISP=SHR
//PRVCONTR DD DSN=HCPS.PROD.VSAM.PROVCONTRACT,
//            DISP=SHR
//MBRMASTR DD DSN=HCPS.PROD.VSAM.MEMBERMASTER,
//            DISP=SHR
//NTWKTBL  DD DSN=HCPS.PROD.VSAM.NETWORKTBL,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//CAPPAYMT DD DSN=HCPS.PROD.CAPITATION.PAYMENTS.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//CAPEFT   DD DSN=HCPS.PROD.CAPITATION.EFT.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=94,BLKSIZE=0)
//CAPRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*================================================================*
//*  STEP050 - PREMIUM RECONCILIATION                              *
//*  Program: HCPREMRC                                             *
//*  Purpose: Reconcile premium payments received against          *
//*           expected premiums based on enrollment census.        *
//*           Identify variances. Generate billing adjustments.    *
//*================================================================*
//*
//STEP050  EXEC PGM=HCPREMRC,
//             REGION=256M,
//             TIME=30,
//             PARM='MONTHLY,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT FILES ---
//PREMPAID DD DSN=HCPS.PROD.PREMIUM.PAYMENTS.M&LYYMMDD,
//            DISP=SHR
//PREMEXPD DD DSN=HCPS.PROD.PREMIUM.EXPECTED.M&LYYMMDD,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//PREMVAR  DD DSN=HCPS.PROD.PREMIUM.VARIANCE.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//PREMADJ  DD DSN=HCPS.PROD.PREMIUM.ADJUSTMENTS.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//PREMRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*================================================================*
//*  STEP060 - 1099 ACCRUAL UPDATE                                 *
//*  Program: HC1099AC                                             *
//*  Purpose: Update 1099-NEC accrual totals for provider          *
//*           payments. Track year-to-date payments by TIN.        *
//*           Flag providers approaching $600 threshold.           *
//*================================================================*
//*
//STEP060  EXEC PGM=HC1099AC,
//             REGION=256M,
//             TIME=30,
//             PARM='MONTHLY,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- VSAM FILES ---
//ACC1099  DD DSN=HCPS.PROD.VSAM.1099ACCUM,
//            DISP=SHR
//PRVMASTR DD DSN=HCPS.PROD.VSAM.PROVMASTER,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//ACCRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//THRSHOLD DD DSN=HCPS.PROD.1099.THRESHOLD.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//*
//SYSIN    DD *
 TAX-YEAR=CURRENT
 THRESHOLD-AMOUNT=600.00
 ALERT-PERCENT=80
 INCLUDE-CAPITATION=Y
 INCLUDE-FFS=Y
 INCLUDE-SETTLEMENTS=Y
/*
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*================================================================*
//*  STEP070 - DATA ARCHIVE (AGED CLAIMS TO ARCHIVE TABLES)        *
//*  Program: HCARCHIV                                             *
//*  Purpose: Move fully adjudicated claims older than             *
//*           rolling archive window to archive tables/tape.       *
//*           Maintain online access for recent claims only.       *
//*================================================================*
//*
// IF (STEP030.RC <= 4) THEN
//*
//STEP070  EXEC PGM=HCARCHIV,
//             REGION=256M,
//             TIME=120,
//             PARM='MONTHLY,PROD'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT: ARCHIVED CLAIMS (TAPE) ---
//ARCHOUT  DD DSN=HCPS.PROD.ARCHIVE.CLAIMS.MONTHLY(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=CART,
//            SPACE=(CYL,(500,100),RLSE),
//            DCB=(RECFM=FB,LRECL=1200,BLKSIZE=27600),
//            RETPD=2555
//*
//*--- OUTPUT: ARCHIVED REMITTANCES (TAPE) ---
//ARCHREM  DD DSN=HCPS.PROD.ARCHIVE.REMIT.MONTHLY(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=CART,
//            SPACE=(CYL,(200,50),RLSE),
//            DCB=(RECFM=FB,LRECL=800,BLKSIZE=24000),
//            RETPD=2555
//*
//ARCHRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSIN    DD *
 ARCHIVE-WINDOW-MONTHS=24
 ARCHIVE-STATUS=FINALIZED
 EXCLUDE-OPEN-CLAIMS=Y
 EXCLUDE-LITIGATION-HOLDS=Y
 COMPRESS-OUTPUT=Y
 RETENTION-YEARS=7
 ARCHIVE-CLAIMS=Y
 ARCHIVE-REMIT=Y
 ARCHIVE-AUTHS=Y
 VERIFY-COUNTS=Y
/*
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
// ENDIF
//*
//*================================================================*
//*  STEP080 - MONTHLY MANAGEMENT REPORTS                          *
//*  Program: HCRPTGEN                                             *
//*  Purpose: Generate monthly management, financial, and          *
//*           regulatory compliance reports. MLR tracking,         *
//*           utilization analysis, fraud indicators.              *
//*================================================================*
//*
//STEP080  EXEC PGM=HCRPTGEN,
//             REGION=512M,
//             TIME=60,
//             PARM='MONTHLY,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- REPORT CONTROL CARD ---
//RPTCTRL  DD *
 RPT01=MONTHLY-FINANCIAL-SUMMARY
 RPT02=CLAIMS-INVENTORY-STATUS
 RPT03=MEDICAL-LOSS-RATIO
 RPT04=PROVIDER-UTILIZATION
 RPT05=MEMBER-UTILIZATION
 RPT06=PAYER-PERFORMANCE
 RPT07=DENIAL-ANALYSIS
 RPT08=CAPITATION-RECONCILIATION
 RPT09=PREMIUM-VARIANCE
 RPT10=REGULATORY-COMPLIANCE
 RPT11=FRAUD-WASTE-ABUSE-INDICATORS
 RPT12=NETWORK-ADEQUACY
 MLR-TARGET=85.00
 REPORT-PERIOD=CURRENT-MONTH
 COMPARISON-PERIOD=PRIOR-YEAR-SAME-MONTH
 OUTPUT-FORMAT=PDF,CSV,EXCEL
 INCLUDE-TELEHEALTH=Y
 INCLUDE-COVID-CLAIMS=Y
 TREND-ANALYSIS=Y
/*
//*
//*--- REPORT OUTPUT DDs ---
//RPT01    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT02    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT03    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT04    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT05    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT06    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT07    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT08    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT09    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT10    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT11    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPT12    DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//CSVOUT   DD DSN=HCPS.PROD.REPORTS.MONTHLY.CSV.M&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=VB,LRECL=4096,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*================================================================*
//*  END OF JOB - HCPS MONTHLY BATCH CYCLE                         *
//*================================================================*
//
