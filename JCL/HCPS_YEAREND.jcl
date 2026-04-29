//HCPSYEND JOB (HCPS,PROD,Y001),
//             'HCPS YEAR-END BATCH',
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
//*  YEAR-END BATCH CYCLE                                          *
//*                                                                *
//*  SCHEDULE: JANUARY 1ST - 04:00 AM EST (MANUAL RELEASE)        *
//*  ON-CALL : HCPS BATCH SUPPORT + MANAGEMENT (EXT 4400)         *
//*  RUNBOOK : HCPS-OPS-003                                        *
//*  APPROVAL: CFO + CIO SIGN-OFF REQUIRED                        *
//*                                                                *
//*  STEP SUMMARY:                                                 *
//*    STEP010 - HCYRROLL   Plan Year Rollover Processing          *
//*    STEP020 - HCACCRST   Accumulator Reset (Preserve Carryover) *
//*    STEP030 - HC1099GN   1099-NEC Generation and Reporting      *
//*    STEP040 - HCREGEXT   Annual Regulatory Filing Data Extract  *
//*    STEP050 - HCPURGE    Archive and Purge (7-Year Retention)   *
//*    STEP060 - HCFEELOAD  New Year Fee Schedule Load             *
//*    STEP070 - HCDRGUPD   DRG Weight Table Update                *
//*    STEP080 - HCCOLAUP   COLA/Inflation Rate Table Update       *
//*    STEP090 - IDCAMS     Year-End VSAM Reorg and Verify         *
//*                                                                *
//*  SPECIAL INSTRUCTIONS:                                         *
//*    - EXCLUSIVE DATABASE ACCESS REQUIRED                        *
//*    - ONLINE SYSTEM MUST BE FULLY DOWN                          *
//*    - DBA ON-CALL MUST BE AVAILABLE                             *
//*    - FULL BACKUP VERIFIED BEFORE EXECUTION (JOB HCPSBKUP)     *
//*                                                                *
//*  DEPENDENCIES:                                                 *
//*    - DECEMBER MONTHLY BATCH MUST COMPLETE                      *
//*    - FULL DATABASE BACKUP VERIFIED                             *
//*    - OIG/SAM FILES UPDATED FOR NEW YEAR                        *
//*    - NEW YEAR FEE SCHEDULES RECEIVED FROM CMS                  *
//*    - NEW DRG WEIGHTS RECEIVED FROM CMS                         *
//*                                                                *
//*  CHANGE LOG:                                                   *
//*    12/1995 R.PETERSON  INITIAL JCL CREATION                    *
//*    12/1998 M.CHAMBERS  Y2K PREPARATION UPDATES                 *
//*    12/2001 S.NAKAMURA  ADD 1099 GENERATION STEP                *
//*    12/2005 T.WILLIAMS  ADD VERIFICATION STEP                   *
//*    12/2009 K.PATEL     ENHANCE ARCHIVAL PROCESS                *
//*    12/2013 D.CHEN      ICD-10 TRANSITION YEAR-END              *
//*    12/2017 A.JOHNSON   ADD TELEHEALTH ACCUMULATORS             *
//*    12/2021 A.JOHNSON   SURPRISE BILLING ACT UPDATES            *
//*    12/2023 S.CHEN      ADD DRG/FEE SCHEDULE LOAD STEPS        *
//*    12/2025 A.PATEL     ADD COLA/INFLATION UPDATE               *
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
//*  STEP010 - PLAN YEAR ROLLOVER PROCESSING                       *
//*  Program: HCYRROLL                                             *
//*  Purpose: Roll plan year forward. Create new plan year         *
//*           records. Copy benefit structures. Update             *
//*           effective dates. Handle multi-year plans.            *
//*           Activate new plans, deactivate expired.              *
//*================================================================*
//*
//STEP010  EXEC PGM=HCYRROLL,
//             REGION=512M,
//             TIME=60,
//             PARM='YEAREND,PROD'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: NEW YEAR PLAN CONFIGURATION ---
//NEWPLANS DD DSN=HCPS.PROD.BENEFIT.NEWYEAR.CONFIG,
//            DISP=SHR
//*
//*--- INPUT: NEW YEAR CONTRACT RATES ---
//NEWRATES DD DSN=HCPS.PROD.CONTRACT.NEWYEAR.RATES,
//            DISP=SHR
//*
//*--- VSAM FILES ---
//PLANTBL  DD DSN=HCPS.PROD.VSAM.PLANCFG,
//            DISP=SHR
//BENTBL   DD DSN=HCPS.PROD.VSAM.BENCONFIG,
//            DISP=SHR
//MBRMASTR DD DSN=HCPS.PROD.VSAM.MEMBERMASTER,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- BACKUP: CURRENT PLAN YEAR DATA (TAPE) ---
//PLANBKUP DD DSN=HCPS.PROD.BENEFIT.PLAN.BACKUP(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=CART,
//            SPACE=(CYL,(100,20),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=27500),
//            RETPD=2555
//*
//*--- OUTPUT FILES ---
//ROLLRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//ROLLAUDT DD DSN=HCPS.PROD.YEAREND.ROLLOVER.AUDIT.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSABEND DD SYSOUT=*
//*
//SYSIN    DD *
 PRIOR-PLAN-YEAR=2025
 NEW-PLAN-YEAR=2026
 ROLLOVER-MODE=COPY-AND-UPDATE
 HANDLE-CARRYOVER=Y
 PRESERVE-GRANDFATHERED=Y
 OE-EFFECTIVE-DATE=20260101
 GRACE-PERIOD-DAYS=90
 ACTIVATE-NEW-PLANS=Y
 DEACTIVATE-EXPIRED=Y
 ROLLOVER-NETWORKS=Y
 ROLLOVER-CONTRACTS=Y
 BACKUP-CURRENT=Y
/*
//*
//*================================================================*
//*  STEP020 - ACCUMULATOR RESET (PRESERVE CARRYOVER)              *
//*  Program: HCACCRST                                             *
//*  Purpose: Reset annual benefit accumulators (deductible,       *
//*           out-of-pocket max, visit limits). Preserve           *
//*           carryover credits from Q4. Handle HSA/FSA            *
//*           rollover amounts. Preserve lifetime maximums.        *
//*================================================================*
//*
//STEP020  EXEC PGM=HCACCRST,
//             REGION=512M,
//             TIME=60,
//             PARM='YEAREND,PROD',
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
//PLANTBL  DD DSN=HCPS.PROD.VSAM.PLANCFG,
//            DISP=SHR
//*
//*--- INPUT/OUTPUT: MEMBER ACCUMULATORS ---
//ACCUFILE DD DSN=HCPS.PROD.MEMBER.ACCUMULATORS,
//            DISP=OLD
//*
//*--- REFERENCE: BENEFIT PLAN MASTER (NEW YEAR) ---
//BENEPLAN DD DSN=HCPS.PROD.BENEFIT.PLAN.MASTER,
//            DISP=SHR
//*
//*--- REFERENCE: MEMBER ENROLLMENT ---
//MEMBENRL DD DSN=HCPS.PROD.MEMBER.ENROLLMENT.MASTER,
//            DISP=SHR
//*
//*--- BACKUP: YEAR-END ACCUMULATOR SNAPSHOT (TAPE) ---
//ACCBKUP  DD DSN=HCPS.PROD.MEMBER.ACCUM.YEAREND(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=CART,
//            SPACE=(CYL,(200,50),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=27500),
//            RETPD=2555
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT FILES ---
//RSTRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//CARYRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//CARRYOVR DD DSN=HCPS.PROD.MEMBER.DEDUCT.CARRYOVER(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(20,5),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//RSTAUDIT DD DSN=HCPS.PROD.YEAREND.ACCRESET.AUDIT.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 RESET-TYPE=ANNUAL
 RESET-DEDUCTIBLE=Y
 RESET-OOP-MAX=Y
 RESET-VISIT-LIMITS=Y
 RESET-ANNUAL-MAX=Y
 RESET-LIFETIME-MAX=N
 PRESERVE-LIFETIME-MAX=Y
 CARRYOVER-Q4-DEDUCTIBLE=Y
 CARRYOVER-WINDOW-DAYS=90
 HSA-ROLLOVER=Y
 FSA-ROLLOVER-MAX=640.00
 FSA-GRACE-PERIOD-DAYS=75
 PRESERVE-DISABILITY-WAIVER=Y
 BACKUP-BEFORE-RESET=Y
/*
//*
//*================================================================*
//*  STEP030 - 1099-NEC GENERATION AND REPORTING                   *
//*  Program: HC1099GN                                             *
//*  Purpose: Generate 1099-NEC forms for all providers            *
//*           paid $600+ during the tax year. Create IRS           *
//*           electronic filing (FIRE system format).              *
//*           Generate 1096 transmittal. State filing.             *
//*================================================================*
//*
//STEP030  EXEC PGM=HC1099GN,
//             REGION=512M,
//             TIME=60,
//             PARM='YEAREND,PROD',
//             COND=((4,LT,STEP010),(4,LT,STEP020))
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
//*--- REFERENCE: PROVIDER 1099 TRACKING ---
//PROV1099 DD DSN=HCPS.PROD.PROVIDER.1099.TRACKING,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT: 1099-NEC FORMS ---
//NEC1099  DD DSN=HCPS.PROD.TAX.1099NEC.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(20,5),RLSE),
//            DCB=(RECFM=FB,LRECL=750,BLKSIZE=0)
//*
//*--- OUTPUT: IRS ELECTRONIC FILING (FIRE FORMAT) ---
//IRSFILE  DD DSN=HCPS.PROD.TAX.IRS.FIRE.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=750,BLKSIZE=0)
//*
//*--- OUTPUT: STATE FILING ---
//STFILE   DD DSN=HCPS.PROD.TAX.STATE.FILING.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=750,BLKSIZE=0)
//*
//*--- OUTPUT: 1096 TRANSMITTAL ---
//TRANS1096 DD DSN=HCPS.PROD.TAX.1096.TRANSMIT.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*--- REPORTS ---
//TAXRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//TAXEXCP  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 TAX-YEAR=2025
 REPORTING-THRESHOLD=600.00
 PAYER-TIN=XX-XXXXXXX
 PAYER-NAME=HEALTHCARE CLAIMS PROCESSING INC
 TRANSMITTER-CONTROL-CODE=XXXXX
 TCC-CODE=22AAA
 IRS-FIRE-FORMAT=2025
 COMBINED-FEDERAL-STATE=Y
 GENERATE-1099NEC=Y
 GENERATE-1099MISC=Y
 GENERATE-EFILE=Y
 GENERATE-1096=Y
 GENERATE-CORRECTIONS=Y
 TEST-FILING=N
 INCLUDE-STATE-FILING=Y
 CONTACT-NAME=TAX DEPARTMENT
 CONTACT-PHONE=5551234500
/*
//*
//*================================================================*
//*  STEP040 - ANNUAL REGULATORY FILING DATA EXTRACT               *
//*  Program: HCREGEXT                                             *
//*  Purpose: Extract data for annual regulatory filings:          *
//*           CMS MLR reporting, state DOI filings,                *
//*           NAIC annual statement, ACA reporting,                *
//*           risk adjustment data submission.                     *
//*================================================================*
//*
//STEP040  EXEC PGM=HCREGEXT,
//             REGION=512M,
//             TIME=120,
//             PARM='YEAREND,PROD',
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
//*--- OUTPUT: MLR REPORTING DATA ---
//MLRDATA  DD DSN=HCPS.PROD.REGULATORY.MLR.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=1000,BLKSIZE=0)
//*
//*--- OUTPUT: NAIC ANNUAL STATEMENT ---
//NAICDATA DD DSN=HCPS.PROD.REGULATORY.NAIC.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=1000,BLKSIZE=0)
//*
//*--- OUTPUT: STATE DOI FILINGS ---
//DOIDATA  DD DSN=HCPS.PROD.REGULATORY.DOI.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=1000,BLKSIZE=0)
//*
//*--- OUTPUT: ACA REPORTING ---
//ACADATA  DD DSN=HCPS.PROD.REGULATORY.ACA.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=1000,BLKSIZE=0)
//*
//*--- OUTPUT: ANNUAL MANAGEMENT REPORTS ---
//RPTFIN   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPTPAYER DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPTDENY  DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//RPTHIDOL DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//REGRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 REPORT-YEAR=2025
 MLR-TARGET=85.00
 MLR-REBATE-CALC=Y
 NAIC-STATEMENT-TYPE=ANNUAL
 DOI-STATES=ALL-LICENSED
 ACA-SECTION-1332-WAIVER=N
 RISK-ADJUSTMENT-DATA=Y
 RISK-CORRIDOR-CALC=N
 YEAR-OVER-YEAR=Y
 PRIOR-YEAR=2024
 TREND-ANALYSIS=Y
/*
//*
//*================================================================*
//*  STEP050 - ARCHIVE AND PURGE (7-YEAR RETENTION)                *
//*  Program: HCPURGE                                              *
//*  Purpose: Archive and purge data exceeding 7-year              *
//*           retention period. HIPAA requires 6 years,            *
//*           company policy is 7 years. Write to tape.            *
//*           Generate purge audit trail. Check litigation holds.  *
//*================================================================*
//*
// IF (STEP010.RC <= 4 AND STEP020.RC <= 4) THEN
//*
//STEP050  EXEC PGM=HCPURGE,
//             REGION=512M,
//             TIME=240,
//             PARM='YEAREND,PROD'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- ARCHIVE OUTPUT TO TAPE (10-YEAR EXPIRY) ---
//ARCHTAPE DD DSN=HCPS.PROD.ARCHIVE.ANNUAL.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=TAPE,
//            DCB=(RECFM=VB,LRECL=32760,BLKSIZE=0),
//            LABEL=(1,SL),
//            EXPDT=2033/365
//*
//*--- ARCHIVE INDEX ---
//ARCHIDX  DD DSN=HCPS.PROD.ARCHIVE.INDEX.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//*
//*--- ARCHIVE AUDIT TRAIL ---
//ARCHAUDT DD DSN=HCPS.PROD.LTARCHIVE.AUDIT(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=CART,
//            SPACE=(CYL,(300,50),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=27600),
//            RETPD=3650
//*
//*--- REPORTS ---
//PURGRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//PURGAUDT DD DSN=HCPS.PROD.YEAREND.PURGE.AUDIT.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 RETENTION-YEARS=7
 PURGE-CUTOFF-DATE=20190101
 ARCHIVE-CLAIMS=Y
 ARCHIVE-ELIGIBILITY=Y
 ARCHIVE-PROVIDERS=N
 ARCHIVE-PAYMENTS=Y
 ARCHIVE-REMITTANCE=Y
 ARCHIVE-AUTHORIZATIONS=Y
 ARCHIVE-AUDIT-TRAIL=Y
 LITIGATION-HOLD-CHECK=Y
 AUDIT-TRAIL=FULL
 DRY-RUN=N
 HIPAA-COMPLIANCE=Y
 COMPRESS-ARCHIVE=Y
 GENERATE-MANIFEST=Y
 DELETE-AFTER-ARCHIVE=Y
 VERIFY-COUNTS=Y
/*
//*
// ENDIF
//*
//*================================================================*
//*  STEP060 - NEW YEAR FEE SCHEDULE LOAD                          *
//*  Program: HCFEELOAD                                            *
//*  Purpose: Load new calendar year fee schedules:                *
//*           Medicare MPFS (Physician Fee Schedule),              *
//*           Medicare OPPS, Medicaid fee schedules,               *
//*           commercial contracted rates.                         *
//*================================================================*
//*
//STEP060  EXEC PGM=HCFEELOAD,
//             REGION=256M,
//             TIME=30,
//             PARM='YEAREND,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: NEW FEE SCHEDULES ---
//MPFSFILE DD DSN=HCPS.PROD.REFERENCE.MPFS.CY2026,
//            DISP=SHR
//OPPSFILE DD DSN=HCPS.PROD.REFERENCE.OPPS.CY2026,
//            DISP=SHR
//MCDFEES  DD DSN=HCPS.PROD.REFERENCE.MEDICAID.CY2026,
//            DISP=SHR
//COMFEES  DD DSN=HCPS.PROD.REFERENCE.COMMERCIAL.CY2026,
//            DISP=SHR
//NEWFEES  DD DSN=HCPS.PROD.PRICING.NEWYEAR.FEES,
//            DISP=SHR
//*
//*--- VSAM FEE SCHEDULE ---
//FEESCHED DD DSN=HCPS.PROD.VSAM.FEESCHEDULE,
//            DISP=SHR
//*
//*--- BACKUP CURRENT FEE SCHEDULE ---
//FEEBKUP  DD DSN=HCPS.PROD.PRICING.FEE.BACKUP(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=CART,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=27600),
//            RETPD=2555
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT ---
//FEERPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//FEEAUDIT DD DSN=HCPS.PROD.YEAREND.FEESCHED.AUDIT.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 EFFECTIVE-DATE=20260101
 BACKUP-CURRENT=Y
 LOAD-MPFS=Y
 LOAD-OPPS=Y
 LOAD-MEDICAID=Y
 LOAD-COMMERCIAL=Y
 CONVERSION-FACTOR=32.7442
 GPCI-UPDATE=Y
 LOCALITY-ADJUSTMENT=Y
 APPLY-RATE-CHANGES=Y
/*
//*
//*================================================================*
//*  STEP070 - DRG WEIGHT TABLE UPDATE                             *
//*  Program: HCDRGUPD                                             *
//*  Purpose: Load new fiscal year DRG weights and                 *
//*           relative value units. Update MS-DRG grouper          *
//*           tables. Apply wage index updates.                    *
//*================================================================*
//*
//STEP070  EXEC PGM=HCDRGUPD,
//             REGION=256M,
//             TIME=30,
//             PARM='YEAREND,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: NEW DRG TABLES ---
//DRGWGHTS DD DSN=HCPS.PROD.REFERENCE.DRG.WEIGHTS.FY2026,
//            DISP=SHR
//WAGEIDX  DD DSN=HCPS.PROD.REFERENCE.WAGE.INDEX.FY2026,
//            DISP=SHR
//GRPRTBL  DD DSN=HCPS.PROD.REFERENCE.MSDRG.GROUPER.V41,
//            DISP=SHR
//*
//*--- VSAM DRG TABLE ---
//DRGTBL   DD DSN=HCPS.PROD.VSAM.DRGWEIGHTS,
//            DISP=SHR
//*
//*--- BACKUP ---
//DRGBKUP  DD DSN=HCPS.PROD.DRG.BACKUP.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=VB,LRECL=32760,BLKSIZE=0)
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT ---
//DRGRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 EFFECTIVE-DATE=20261001
 DRG-VERSION=41
 BACKUP-CURRENT=Y
 BASE-RATE-UPDATE=Y
 NATIONAL-BASE-RATE=6378.41
 OUTLIER-THRESHOLD=31471.00
 WAGE-INDEX-RECLASSIFY=Y
 REBUILD-GROUPER=Y
/*
//*
//*================================================================*
//*  STEP080 - COLA / INFLATION RATE TABLE UPDATE                  *
//*  Program: HCCOLAUP                                             *
//*  Purpose: Update cost-of-living adjustment tables,             *
//*           inflation multipliers, ACA premium tax credit        *
//*           thresholds, OOP maximum limits, HSA/FSA limits.      *
//*================================================================*
//*
//STEP080  EXEC PGM=HCCOLAUP,
//             REGION=256M,
//             TIME=30,
//             PARM='YEAREND,PROD',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: NEW RATE TABLES ---
//COLAINPT DD DSN=HCPS.PROD.REFERENCE.COLA.RATES.CY2026,
//            DISP=SHR
//ACALIMTS DD DSN=HCPS.PROD.REFERENCE.ACA.LIMITS.CY2026,
//            DISP=SHR
//*
//*--- VSAM TABLES ---
//RATETBL  DD DSN=HCPS.PROD.VSAM.RATETABLES,
//            DISP=SHR
//PLANTBL  DD DSN=HCPS.PROD.VSAM.PLANCFG,
//            DISP=SHR
//*
//*--- BACKUP ---
//RATEBKUP DD DSN=HCPS.PROD.RATETBL.BACKUP.Y&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=VB,LRECL=32760,BLKSIZE=0)
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT ---
//COLARPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 EFFECTIVE-DATE=20260101
 COLA-RATE=3.20
 CPI-MEDICAL=4.10
 ACA-OOP-MAX-INDIVIDUAL=9450.00
 ACA-OOP-MAX-FAMILY=18900.00
 ACA-DEDUCTIBLE-LIMIT=8300.00
 HSA-CONTRIBUTION-INDIVIDUAL=4300.00
 HSA-CONTRIBUTION-FAMILY=8550.00
 HSA-CATCHUP-55PLUS=1000.00
 FSA-CONTRIBUTION-LIMIT=3200.00
 FSA-ROLLOVER-LIMIT=640.00
 BACKUP-CURRENT=Y
 UPDATE-PLAN-LIMITS=Y
 UPDATE-PREMIUM-BANDS=Y
/*
//*
//*================================================================*
//*  STEP090 - YEAR-END VSAM REORG AND VERIFY                     *
//*  Utility: IDCAMS                                               *
//*  Purpose: Reorganize all VSAM clusters after year-end          *
//*           updates. Verify integrity of all clusters.           *
//*================================================================*
//*
//STEP090  EXEC PGM=IDCAMS,
//             COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//*
//SYSIN    DD *
 /* ----------------------------------------------------------- */
 /* VERIFY ALL VSAM CLUSTERS AFTER YEAR-END UPDATES             */
 /* ----------------------------------------------------------- */
 VERIFY DATASET(HCPS.PROD.VSAM.FEESCHEDULE)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.DRGWEIGHTS)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.BENACCUM)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.PLANCFG)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.RATETABLES)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.1099ACCUM)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.BENCONFIG)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.MEMBERMASTER)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 VERIFY DATASET(HCPS.PROD.VSAM.PROVMASTER)
 IF LASTCC > 0 THEN -
   SET MAXCC = 8
 /* ----------------------------------------------------------- */
 /* LIST ALL VSAM CLUSTER STATISTICS FOR AUDIT                  */
 /* ----------------------------------------------------------- */
 LISTCAT ENTRIES(HCPS.PROD.VSAM.*) -
         ALL
/*
//*
//*================================================================*
//*  STEP100 - YEAR-END VERIFICATION AND SIGN-OFF                  *
//*  Program: HCYRVRFY                                             *
//*  Purpose: Verify all year-end processing completed             *
//*           successfully. Generate sign-off report.              *
//*================================================================*
//*
//STEP100  EXEC PGM=HCYRVRFY,
//             REGION=256M,
//             TIME=30,
//             PARM='YEAREND,PROD',
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
//*--- OUTPUT: VERIFICATION AND SIGN-OFF REPORT ---
//VRFYRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*--- OUTPUT: SYSTEM READINESS STATUS ---
//SYSREADY DD DSN=HCPS.PROD.CONTROL.SYSTEM.STATUS,
//            DISP=OLD
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 VERIFY-TYPE=YEAREND
 CHECK-PLAN-ROLLOVER=Y
 CHECK-ACCUM-RESET=Y
 CHECK-1099-TOTALS=Y
 CHECK-ARCHIVE-COUNTS=Y
 CHECK-DB-INTEGRITY=Y
 CHECK-INDEX-STATUS=Y
 CHECK-FEE-SCHEDULE=Y
 CHECK-DRG-WEIGHTS=Y
 CHECK-COLA-RATES=Y
 SET-SYSTEM-READY=Y
 NEW-YEAR=2026
/*
//*
//*================================================================*
//*  END OF JOB - HCPS YEAR-END BATCH CYCLE                        *
//*================================================================*
//
