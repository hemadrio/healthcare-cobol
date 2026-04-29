//HCPSDALY JOB (HCPS,PROD,D001),
//             'HCPS DAILY BATCH',
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
//*  DAILY BATCH CYCLE                                             *
//*                                                                *
//*  SCHEDULE: DAILY - 01:00 AM EST (CA-7/TWS)                    *
//*            WEEKDAYS: MON-FRI                                   *
//*            WEEKENDS: SAT-SUN AT 06:00 AM EST                   *
//*                                                                *
//*  ON-CALL : HCPS BATCH SUPPORT (EXT 4400)                      *
//*  RUNBOOK : HCPS-OPS-001                                        *
//*                                                                *
//*  STEP SUMMARY:                                                 *
//*    STEP010 - HCCLMVAL  Claims Validation                       *
//*    STEP020 - HCELIGVR  Eligibility Verification Batch          *
//*    STEP030 - SORT      Sort Validated Claims by Payer          *
//*    STEP040 - HCCLMADJ  Claims Adjudication                    *
//*    STEP050 - HCREMIT   Remittance / EOB Generation             *
//*    STEP060 - HCRPTGEN  Daily Report Generation                 *
//*    STEP070 - BPXBATCH  FTP 835 Files to Clearinghouse          *
//*    STEP080 - IDCAMS    GDG Roll-Forward and Cleanup            *
//*    STEP090 - IDCAMS    VSAM Maintenance                        *
//*                                                                *
//*  DEPENDENCIES:                                                 *
//*    - EDI GATEWAY FEED MUST COMPLETE (JOB HCPSEDI0)             *
//*    - ONLINE SYSTEM QUIESCE CONFIRMED                           *
//*                                                                *
//*  CHANGE LOG:                                                   *
//*    03/1995 R.PETERSON  INITIAL JCL CREATION                    *
//*    06/1999 M.CHAMBERS  Y2K DATE REMEDIATION                    *
//*    09/2003 S.NAKAMURA  ADD GDG REFERENCES                      *
//*    04/2007 K.PATEL     NPI MANDATE CHANGES                     *
//*    11/2010 J.MARTINEZ  ADD REMITTANCE STEP                     *
//*    08/2015 D.CHEN      ICD-10 CONVERSION UPDATES               *
//*    03/2020 A.JOHNSON   COVID EMERGENCY PROCESSING              *
//*    01/2022 A.JOHNSON   NO SURPRISES ACT EDITS                 *
//*    06/2023 S.CHEN      TELEHEALTH PROCESSING                  *
//*    01/2025 K.JOHNSON   OPIOID MONITORING EDITS                *
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
//*  STEP010 - CLAIMS VALIDATION                                   *
//*  Program: HCCLMVAL                                             *
//*  Purpose: Validate incoming claims against edit rules,         *
//*           code tables, and business rules. Split into          *
//*           validated and rejected claim files.                  *
//*           Validates 837I/837P formats, NPI, ICD-10,            *
//*           CPT/HCPCS codes, place of service, and              *
//*           timely filing compliance.                            *
//*================================================================*
//*
//STEP010  EXEC PGM=HCCLMVAL,
//             REGION=256M,
//             TIME=30,
//             PARM='DAILY,PROD,EDITLVL=FULL'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: INCOMING CLAIMS FROM EDI GATEWAY (GDG) ---
//CLMINPUT DD DSN=HCPS.PROD.CLAIMS.DAILY.INPUT(+0),
//            DISP=OLD
//*
//*--- INPUT: CLAIM HISTORY FOR DUPLICATE CHECK ---
//CLMHIST  DD DSN=HCPS.PROD.CLAIMS.HISTORY(+0),
//            DISP=SHR
//*
//*--- VSAM EDIT RULE CLUSTER ---
//EDITRULE DD DSN=HCPS.PROD.VSAM.EDITRULES,
//            DISP=SHR
//*
//*--- VSAM CODE TABLES ---
//ICDTBL   DD DSN=HCPS.PROD.VSAM.ICD10CODES,
//            DISP=SHR
//CPTTBL   DD DSN=HCPS.PROD.VSAM.CPTCODES,
//            DISP=SHR
//HCPCSTBL DD DSN=HCPS.PROD.VSAM.HCPCSCODES,
//            DISP=SHR
//NPITBL   DD DSN=HCPS.PROD.VSAM.NPIREG,
//            DISP=SHR
//POSTBL   DD DSN=HCPS.PROD.VSAM.PLACEOFSERV,
//            DISP=SHR
//REVTBL   DD DSN=HCPS.PROD.VSAM.REVCODES,
//            DISP=SHR
//MODTBL   DD DSN=HCPS.PROD.VSAM.MODIFIERS,
//            DISP=SHR
//NCCITBL  DD DSN=HCPS.PROD.VSAM.NCCIEDITS,
//            DISP=SHR
//*
//*--- OUTPUT: VALIDATED CLAIMS (GDG +1) ---
//CLMVALID DD DSN=HCPS.PROD.CLAIMS.VALIDATED(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=2000,BLKSIZE=0)
//*
//*--- OUTPUT: REJECTED CLAIMS ---
//CLMREJCT DD DSN=HCPS.PROD.CLAIMS.REJECTED.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=2200,BLKSIZE=0)
//*
//*--- OUTPUT: DUPLICATE CLAIMS ---
//CLMDUPES DD DSN=HCPS.PROD.CLAIMS.DUPLICATES.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=2200,BLKSIZE=0)
//*
//*--- OUTPUT: VALIDATION ERROR REPORT ---
//ERRRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*--- OUTPUT: VALIDATION SUMMARY REPORT ---
//VALRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSABEND DD SYSOUT=*
//*
//*--- CONTROL CARD: PARM OVERRIDES ---
//SYSIN    DD *
 EDIT-LEVEL=FULL
 PROCESS-DATE=CURRENT
 DUPLICATE-WINDOW-DAYS=365
 TIMELY-FILING-COMMERCIAL=365
 TIMELY-FILING-MEDICARE=365
 TIMELY-FILING-MEDICAID=180
 NSA-EFFECTIVE-DATE=20220101
 TELEHEALTH-ALLOWED=Y
 OPIOID-MONITORING=Y
 COVID-NO-COST-SHARE=Y
 MAX-CLAIM-AMOUNT=9999999.99
 VALIDATE-NPI=Y
 VALIDATE-ICD10=Y
 VALIDATE-CPT=Y
 VALIDATE-NCCI=Y
 MAX-ERRORS=9999
 TRACE-LEVEL=0
/*
//*
//*================================================================*
//*  STEP020 - ELIGIBILITY VERIFICATION BATCH                      *
//*  Program: HCELIGVR                                             *
//*  Purpose: Process enrollment feed, verify active coverage      *
//*           for validated claims, apply COB rules,               *
//*           check pre-existing conditions, verify                *
//*           benefit plan assignment.                             *
//*================================================================*
//*
//STEP020  EXEC PGM=HCELIGVR,
//             REGION=256M,
//             TIME=30,
//             PARM='DAILY,PROD',
//             COND=(4,LT,STEP010)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: ENROLLMENT FEED ---
//ENRLFEED DD DSN=HCPS.PROD.ENROLLMENT.DAILY.FEED(+0),
//            DISP=SHR
//*
//*--- INPUT: VALIDATED CLAIMS FROM STEP010 ---
//CLMVALID DD DSN=HCPS.PROD.CLAIMS.VALIDATED(+1),
//            DISP=SHR
//*
//*--- INPUT: COB MASTER FILE ---
//COBFILE  DD DSN=HCPS.PROD.MEMBER.COB.MASTER,
//            DISP=SHR
//*
//*--- INPUT: BENEFIT PLAN MASTER ---
//BENEPLAN DD DSN=HCPS.PROD.BENEFIT.PLAN.MASTER,
//            DISP=SHR
//*
//*--- VSAM MEMBER MASTER ---
//MBRMASTR DD DSN=HCPS.PROD.VSAM.MEMBERMASTER,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT: ELIGIBILITY VERIFICATION REPORT ---
//ELIGRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*--- OUTPUT: INELIGIBLE CLAIMS ---
//INELICLM DD DSN=HCPS.PROD.CLAIMS.INELIGIBLE.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=2200,BLKSIZE=0)
//*
//*--- OUTPUT: ELIGIBLE CLAIMS ---
//ELIGCLM  DD DSN=HCPS.PROD.CLAIMS.ELIGIBLE.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=2200,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 PROCESS-DATE=CURRENT
 VERIFY-COB=Y
 CHECK-ACCUMULATORS=Y
 RETRO-ELIG-DAYS=90
 VERIFY-BENEFIT-PLAN=Y
/*
//*
//*================================================================*
//*  STEP030 - SORT VALIDATED CLAIMS BY PAYER                      *
//*  Utility: DFSORT (ICEMAN) / SYNCSORT                           *
//*  Purpose: Sort eligible claims by payer ID, plan code,         *
//*           provider NPI, date of service for efficient           *
//*           adjudication processing.                             *
//*================================================================*
//*
//STEP030  EXEC PGM=SORT,
//             COND=(4,LT)
//SORTIN   DD DSN=HCPS.PROD.CLAIMS.ELIGIBLE.D&LYYMMDD,
//            DISP=SHR
//*
//SORTOUT  DD DSN=HCPS.PROD.CLAIMS.SORTED.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=2200,BLKSIZE=0)
//*
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(20,5))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(20,5))
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(20,5))
//*
//SYSOUT   DD SYSOUT=*
//*
//SYSIN    DD *
  SORT FIELDS=(15,10,CH,A,       PAYER-ID
               25,5,CH,A,        PLAN-CODE
               50,10,CH,A,       PROVIDER-NPI
               100,8,CH,A),      DATE-OF-SERVICE
       FORMAT=CH
  INCLUDE COND=(5,2,CH,EQ,C'01',   CLAIM-TYPE = PROFESSIONAL
               OR,
               5,2,CH,EQ,C'02',    CLAIM-TYPE = INSTITUTIONAL
               OR,
               5,2,CH,EQ,C'03')    CLAIM-TYPE = DENTAL
  SUM FIELDS=NONE
  OPTION DYNALLOC=(SYSDA,5),FILSZ=E50000
/*
//*
//*================================================================*
//*  STEP040 - CLAIMS ADJUDICATION                                 *
//*  Program: HCCLMADJ                                             *
//*  Purpose: Price claims, apply benefit plan rules, calculate    *
//*           member responsibility (deductible, copay,            *
//*           coinsurance), determine payment amounts,             *
//*           verify authorizations, apply NCCI edits,             *
//*           update benefit accumulators.                         *
//*================================================================*
//*
//STEP040  EXEC PGM=HCCLMADJ,
//             REGION=512M,
//             TIME=60,
//             PARM='DAILY,PROD,PRICELVL=FULL',
//             COND=(4,LT)
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: SORTED CLAIMS ---
//SRTDCLMS DD DSN=HCPS.PROD.CLAIMS.SORTED.D&LYYMMDD,
//            DISP=SHR
//*
//*--- VSAM REFERENCE FILES ---
//FEESCHED DD DSN=HCPS.PROD.VSAM.FEESCHEDULE,
//            DISP=SHR
//DRGTBL   DD DSN=HCPS.PROD.VSAM.DRGWEIGHTS,
//            DISP=SHR
//BENACC   DD DSN=HCPS.PROD.VSAM.BENACCUM,
//            DISP=SHR
//AUTHREG  DD DSN=HCPS.PROD.VSAM.AUTHREGISTER,
//            DISP=SHR
//PRVCONTR DD DSN=HCPS.PROD.VSAM.PROVCONTRACT,
//            DISP=SHR
//NTWKTBL  DD DSN=HCPS.PROD.VSAM.NETWORKTBL,
//            DISP=SHR
//EDTTBL   DD DSN=HCPS.PROD.VSAM.EDITRULES,
//            DISP=SHR
//NCCITBL  DD DSN=HCPS.PROD.VSAM.NCCIEDITS,
//            DISP=SHR
//*
//*--- FLAT FILE REFERENCE ---
//DUPREF   DD DSN=HCPS.PROD.CLAIMS.HISTORY.INDEX,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT: PAID CLAIMS ---
//ADJPAID  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.PAID.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=2500,BLKSIZE=0)
//*
//*--- OUTPUT: DENIED CLAIMS ---
//ADJDENY  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.DENY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=2500,BLKSIZE=0)
//*
//*--- OUTPUT: PENDED CLAIMS (MANUAL REVIEW) ---
//ADJPEND  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.PEND.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=2500,BLKSIZE=0)
//*
//*--- OUTPUT: SUSPENDED CLAIMS ---
//ADJSUSPS DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.SUSP.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=2500,BLKSIZE=0)
//*
//*--- OUTPUT: ACCUMULATOR UPDATES ---
//ACCMUPDT DD DSN=HCPS.PROD.ACCUM.UPDATES.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//*--- OUTPUT: MEMBER ACCUMULATORS (UPDATED IN PLACE) ---
//ACCUOUT  DD DSN=HCPS.PROD.MEMBER.ACCUMULATORS,
//            DISP=OLD
//*
//*--- REPORTS ---
//ADJRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//PRICRPT  DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//EXCRPT   DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 PROCESS-DATE=CURRENT
 PRICING-METHOD=CONTRACT
 DUP-CHECK=Y
 NCCI-EDITS=Y
 AUTH-REQUIRED=Y
 HIGH-DOLLAR-THRESHOLD=50000.00
 PEND-THRESHOLD=25000.00
 AUTO-ADJUD-PCT=85
 NSA-ENFORCEMENT=Y
 OPIOID-REVIEW-THRESHOLD=90
 COVID-NO-COSTSHARE=Y
/*
//*
//*================================================================*
//*  CONDITIONAL CHECK - SKIP REMITTANCE IF ADJUDICATION FAILED    *
//*================================================================*
//*
// IF (STEP040.RC <= 4) THEN
//*
//*================================================================*
//*  STEP050 - REMITTANCE / EOB GENERATION                         *
//*  Program: HCREMIT                                              *
//*  Purpose: Generate 835 electronic remittance advice,           *
//*           paper checks, EFT/ACH files, positive pay,           *
//*           and member Explanation of Benefits.                  *
//*================================================================*
//*
//STEP050  EXEC PGM=HCREMIT,
//             REGION=256M,
//             TIME=30,
//             PARM='DAILY,PROD'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT: ADJUDICATED CLAIMS ---
//ADJPAID  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.PAID.D&LYYMMDD,
//            DISP=SHR
//ADJDENY  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.DENY.D&LYYMMDD,
//            DISP=SHR
//*
//*--- VSAM REFERENCE FILES ---
//PRVMASTR DD DSN=HCPS.PROD.VSAM.PROVMASTER,
//            DISP=SHR
//MBRMASTR DD DSN=HCPS.PROD.VSAM.MEMBERMASTER,
//            DISP=SHR
//BANKTBL  DD DSN=HCPS.PROD.VSAM.BANKACCOUNT,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- OUTPUT: 835 ELECTRONIC REMITTANCE ADVICE ---
//EDI835   DD DSN=HCPS.PROD.EDI.835.DAILY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(20,5),RLSE),
//            DCB=(RECFM=FB,LRECL=128,BLKSIZE=0)
//*
//*--- OUTPUT: PAPER CHECK FILE ---
//CHKFILE  DD DSN=HCPS.PROD.CHECKS.DAILY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//*
//*--- OUTPUT: EFT/ACH FILE ---
//EFTFILE  DD DSN=HCPS.PROD.EFT.DAILY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=94,BLKSIZE=0)
//*
//*--- OUTPUT: POSITIVE PAY ---
//POSPAY   DD DSN=HCPS.PROD.POSPAY.DAILY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//*
//*--- OUTPUT: MEMBER EOB ---
//EOBRPT   DD DSN=HCPS.PROD.EOB.DAILY.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*--- OUTPUT: PAPER REMITTANCE PRINT ---
//REMPRINT DD DSN=HCPS.PROD.REMIT.DAILY.PRINT.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*--- REPORT ---
//REMITRPT DD SYSOUT=*,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//SYSIN    DD *
 ERA-FORMAT=005010X221A1
 GENERATE-EOB=Y
 EFT-ORIGINATOR=HCPS001
 CHECK-START-NUM=AUTO
 POSITIVE-PAY=Y
/*
//*
//*================================================================*
//*  STEP060 - DAILY REPORT GENERATION                             *
//*  Program: HCRPTGEN                                             *
//*  Purpose: Generate daily batch processing reports for          *
//*           operations, management, and compliance.              *
//*================================================================*
//*
//STEP060  EXEC PGM=HCRPTGEN,
//             REGION=512M,
//             TIME=45,
//             PARM='DAILY,PROD'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//         DD DSN=HCPS.PROD.LOADLIB.SYBASE,DISP=SHR
//         DD DSN=HCPS.PROD.DBRMLIB,DISP=SHR
//*
//*--- INPUT FILES ---
//ADJPAID  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.PAID.D&LYYMMDD,
//            DISP=SHR
//ADJDENY  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.DENY.D&LYYMMDD,
//            DISP=SHR
//ADJPEND  DD DSN=HCPS.PROD.CLAIMS.ADJUDICATED.PEND.D&LYYMMDD,
//            DISP=SHR
//CLMREJCT DD DSN=HCPS.PROD.CLAIMS.REJECTED.D&LYYMMDD,
//            DISP=SHR
//*
//*--- SYBASE DATABASE CONNECTION ---
//SYSDBOUT DD DSN=HCPS.PROD.SYBASE.DBPARMS(HCPSPROD),
//            DISP=SHR
//SYSDBLOG DD SYSOUT=*
//*
//*--- REPORT CONTROL CARD ---
//RPTCTRL  DD *
 RPT01=DAILY-CLAIMS-SUMMARY
 RPT02=PAYER-MIX-ANALYSIS
 RPT03=DENIAL-REASON-BREAKDOWN
 RPT04=HIGH-DOLLAR-CLAIMS
 RPT05=PENDED-CLAIMS-AGING
 RPT06=PROVIDER-PAYMENT-SUMMARY
 RPT07=CLAIMS-TURNAROUND-TIME
 RPT08=DUPLICATE-CLAIMS-REPORT
 RPT09=OPIOID-MONITORING-REPORT
 RPT10=TELEHEALTH-UTILIZATION
 HIGH-DOLLAR-THRESHOLD=25000.00
 AGING-BUCKETS=30,60,90,120,180
 OUTPUT-FORMAT=PDF,CSV
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
//*
//CSVOUT   DD DSN=HCPS.PROD.REPORTS.DAILY.CSV.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=VB,LRECL=4096,BLKSIZE=0)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
// ELSE
//*
//*--- ADJUDICATION FAILED - GENERATE ERROR NOTIFICATION ---
//STEPERR  EXEC PGM=HCNOTIFY,
//             PARM='ADJFAIL,CRITICAL'
//STEPLIB  DD DSN=HCPS.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 NOTIFY-GROUP=HCPS-ONCALL
 NOTIFY-METHOD=EMAIL,PAGER
 MESSAGE=DAILY ADJUDICATION STEP FAILED - REMITTANCE SKIPPED
/*
//*
// ENDIF
//*
//*================================================================*
//*  STEP070 - FTP 835 FILES TO CLEARINGHOUSE                      *
//*  Utility: BPXBATCH (USS)                                       *
//*  Purpose: Transmit 835 remittance files to clearinghouse       *
//*           partners via secure FTP (SFTP).                      *
//*================================================================*
//*
// IF (STEP050.RC <= 4) THEN
//*
//STEP070  EXEC PGM=BPXBATCH,
//             PARM='SH /hcps/prod/scripts/sftp_835.sh &LYYMMDD'
//*
//STDIN    DD DSN=HCPS.PROD.FTP.SCRIPTS(XMIT835),
//            DISP=SHR
//*
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//FTPLOG   DD DSN=HCPS.PROD.FTP.LOG.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=(RECFM=VB,LRECL=256,BLKSIZE=0)
//*
//FTPCTRL  DD *
 PARTNER=AVAILITY
   HOST=sftp.availity.com
   PORT=22
   USERID=HCPS_PROD
   KEYFILE=/hcps/prod/.ssh/availity_rsa
   REMOTE-DIR=/inbound/835
   LOCAL-DSN=HCPS.PROD.EDI.835.DAILY.D&LYYMMDD
 PARTNER=CHANGEHC
   HOST=sftp.changehealthcare.com
   PORT=22
   USERID=HCPS_PROD
   KEYFILE=/hcps/prod/.ssh/changehc_rsa
   REMOTE-DIR=/inbound/remittance
   LOCAL-DSN=HCPS.PROD.EDI.835.DAILY.D&LYYMMDD
 PARTNER=WAYSTAR
   HOST=sftp.waystar.com
   PORT=22
   USERID=HCPS_PROD
   KEYFILE=/hcps/prod/.ssh/waystar_rsa
   REMOTE-DIR=/claims/835
   LOCAL-DSN=HCPS.PROD.EDI.835.DAILY.D&LYYMMDD
/*
//*
// ENDIF
//*
//*================================================================*
//*  STEP080 - GDG ROLL-FORWARD AND CLEANUP                        *
//*  Utility: IDCAMS                                               *
//*  Purpose: Maintain GDG base entries, delete expired             *
//*           generations, catalog new generations.                 *
//*================================================================*
//*
//STEP080  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//*
//SYSIN    DD *
 /* ----------------------------------------------------------- */
 /* ROLL FORWARD DAILY CLAIMS INPUT GDG                         */
 /* RETAIN LAST 30 GENERATIONS                                  */
 /* ----------------------------------------------------------- */
 ALTER HCPS.PROD.CLAIMS.DAILY.INPUT -
       LIMIT(30) -
       SCRATCH
 /* ----------------------------------------------------------- */
 /* ROLL FORWARD VALIDATED CLAIMS GDG                           */
 /* RETAIN LAST 30 GENERATIONS                                  */
 /* ----------------------------------------------------------- */
 ALTER HCPS.PROD.CLAIMS.VALIDATED -
       LIMIT(30) -
       SCRATCH
 /* ----------------------------------------------------------- */
 /* ROLL FORWARD HISTORY GDG                                    */
 /* RETAIN LAST 90 GENERATIONS                                  */
 /* ----------------------------------------------------------- */
 ALTER HCPS.PROD.CLAIMS.HISTORY -
       LIMIT(90) -
       SCRATCH
 /* ----------------------------------------------------------- */
 /* ROLL FORWARD EDI 835 GDG                                    */
 /* RETAIN LAST 60 GENERATIONS                                  */
 /* ----------------------------------------------------------- */
 ALTER HCPS.PROD.EDI.835.DAILY -
       LIMIT(60) -
       SCRATCH
 /* ----------------------------------------------------------- */
 /* DELETE WORK FILES OLDER THAN 7 DAYS                         */
 /* ----------------------------------------------------------- */
 SET MAXCC=0
 DELETE HCPS.PROD.CLAIMS.SORTED.D* -
        PURGE -
        NOSCRATCH
 SET MAXCC=0
 DELETE HCPS.PROD.CLAIMS.ELIGIBLE.D* -
        PURGE -
        NOSCRATCH
 SET MAXCC=0
/*
//*
//*================================================================*
//*  STEP090 - VSAM MAINTENANCE                                    *
//*  Utility: IDCAMS                                               *
//*  Purpose: Verify VSAM cluster integrity, backup                *
//*           critical clusters, gather statistics.                *
//*================================================================*
//*
// IF (STEP040.RC <= 4) THEN
//*
//STEP090  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//*
//*--- BACKUP BENEFIT ACCUMULATOR BEFORE VERIFY ---
//BACKUP   DD DSN=HCPS.PROD.VSAM.BENACCUM.BACKUP.D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(20,5),RLSE),
//            DCB=(RECFM=VB,LRECL=32760,BLKSIZE=0)
//*
//BENACC   DD DSN=HCPS.PROD.VSAM.BENACCUM,
//            DISP=SHR
//*
//SYSIN    DD *
 /* ----------------------------------------------------------- */
 /* VERIFY BENEFIT ACCUMULATOR VSAM CLUSTER                     */
 /* ----------------------------------------------------------- */
 VERIFY FILE(BENACC)
 /* ----------------------------------------------------------- */
 /* EXPORT (BACKUP) BENEFIT ACCUMULATOR                         */
 /* ----------------------------------------------------------- */
 REPRO INDATASET(HCPS.PROD.VSAM.BENACCUM) -
       OUTFILE(BACKUP)
 /* ----------------------------------------------------------- */
 /* VERIFY EDIT RULES CLUSTER                                   */
 /* ----------------------------------------------------------- */
 VERIFY DATASET(HCPS.PROD.VSAM.EDITRULES)
 /* ----------------------------------------------------------- */
 /* VERIFY MEMBER MASTER CLUSTER                                */
 /* ----------------------------------------------------------- */
 VERIFY DATASET(HCPS.PROD.VSAM.MEMBERMASTER)
 /* ----------------------------------------------------------- */
 /* VERIFY PROVIDER MASTER CLUSTER                              */
 /* ----------------------------------------------------------- */
 VERIFY DATASET(HCPS.PROD.VSAM.PROVMASTER)
 /* ----------------------------------------------------------- */
 /* VERIFY FEE SCHEDULE CLUSTER                                 */
 /* ----------------------------------------------------------- */
 VERIFY DATASET(HCPS.PROD.VSAM.FEESCHEDULE)
 /* ----------------------------------------------------------- */
 /* VERIFY NPI REGISTRY CLUSTER                                 */
 /* ----------------------------------------------------------- */
 VERIFY DATASET(HCPS.PROD.VSAM.NPIREG)
 /* ----------------------------------------------------------- */
 /* LIST CATALOG ENTRIES FOR AUDIT                              */
 /* ----------------------------------------------------------- */
 LISTCAT ENTRIES(HCPS.PROD.VSAM.*) -
         ALL
 /* ----------------------------------------------------------- */
 /* PRINT VSAM STATISTICS (ZERO RECORDS FOR STATS ONLY)        */
 /* ----------------------------------------------------------- */
 PRINT INDATASET(HCPS.PROD.VSAM.BENACCUM) -
       CHARACTER -
       COUNT(0)
/*
//*
// ENDIF
//*
//*================================================================*
//*  END OF JOB - HCPS DAILY BATCH CYCLE                           *
//*================================================================*
//
