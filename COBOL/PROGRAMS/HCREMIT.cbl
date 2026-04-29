      ****************************************************************
      * PROGRAM:    HCREMIT
      * TITLE:      HIPAA 835 REMITTANCE ADVICE / EOB GENERATOR
      * AUTHOR:     ENTERPRISE CLAIMS PROCESSING TEAM
      * WRITTEN:    1994-03-15
      * COMPANY:    CONSOLIDATED HEALTHCARE SYSTEMS INC.
      *
      * DESCRIPTION:
      *   PRODUCTION-GRADE REMITTANCE ADVICE AND EXPLANATION OF
      *   BENEFITS GENERATOR. PRODUCES HIPAA 835 ELECTRONIC
      *   REMITTANCE, PROVIDER PAYMENT SUMMARIES, PATIENT EOBS,
      *   CHECK REGISTER, EFT/ACH FILES, AND POSITIVE PAY OUTPUT.
      *
      *   THIS PROGRAM HAS BEEN IN CONTINUOUS PRODUCTION SINCE
      *   MARCH 1994. IT IS THE BACKBONE OF THE PAYMENT OUTPUT
      *   SUBSYSTEM FOR ALL LINES OF BUSINESS (COMMERCIAL, HMO,
      *   PPO, MEDICARE ADVANTAGE, MEDICAID MANAGED CARE, DENTAL,
      *   AND VISION).
      *
      * INPUT:
      *   - ADJUDICATED CLAIMS FILE (SORTED BY PROVIDER/PATIENT)
      *   - PROVIDER MASTER TABLE (SYBASE)
      *   - MEMBER MASTER TABLE (SYBASE)
      *   - PAYER CONFIGURATION TABLE (SYBASE)
      *   - PAYMENT CONTROL TABLE (SYBASE)
      *   - CARC/RARC REFERENCE TABLES (SYBASE)
      *   - HOLIDAY CALENDAR TABLE (SYBASE)
      *   - STATE EOB MANDATES TABLE (SYBASE)
      *
      * OUTPUT:
      *   - HIPAA 835 ELECTRONIC REMITTANCE FILE
      *   - PROVIDER REMITTANCE ADVICE REPORT (PRINT)
      *   - PATIENT EXPLANATION OF BENEFITS (PRINT)
      *   - CHECK REGISTER OUTPUT FILE
      *   - EFT/ACH OUTPUT FILE (NACHA FORMAT)
      *   - POSITIVE PAY FILE
      *   - PAYMENT SUMMARY REPORT
      *   - ERROR/EXCEPTION FILE
      *   - AUDIT TRAIL FILE
      *   - PAYMENT RECONCILIATION REPORT
      *
      * MODIFICATION HISTORY:
      *   1994-03-15  JMILLER    ORIGINAL DEVELOPMENT
      *   1994-06-22  JMILLER    ADDED CHECK SPLITTING LOGIC
      *   1994-09-10  RTHOMPSON  ADDED POSITIVE PAY OUTPUT
      *   1995-01-18  JMILLER    Y2K REMEDIATION PHASE 1
      *   1995-04-30  SCHEN      ADDED EFT/ACH PROCESSING
      *   1995-08-14  RTHOMPSON  NACHA FORMAT COMPLIANCE
      *   1996-02-20  JMILLER    ADDED STATE EOB MANDATES
      *   1996-07-11  KWILSON    COORDINATION OF BENEFITS LOGIC
      *   1996-11-05  SCHEN      NEGATIVE BALANCE HANDLING
      *   1997-03-22  RTHOMPSON  INTEREST PAYMENT CALCULATION
      *   1997-08-15  JMILLER    WITHHOLD/BONUS PROCESSING
      *   1997-12-01  KWILSON    CAPITATION OFFSET LOGIC
      *   1998-03-30  SCHEN      OVERPAYMENT RECOUPMENT
      *   1998-06-15  RTHOMPSON  Y2K REMEDIATION PHASE 2
      *   1998-09-22  JMILLER    Y2K DATE WINDOWING FINAL
      *   1999-01-10  KWILSON    EURO CURRENCY SUPPORT (NOT USED)
      *   1999-11-15  SCHEN      Y2K FINAL CERTIFICATION
      *   2000-01-03  JMILLER    Y2K POST-PRODUCTION VERIFY
      *   2000-05-20  RTHOMPSON  ADDED APPEAL RIGHTS TO EOB
      *   2001-02-14  KWILSON    ADDED PLAN BENEFIT SUMMARY
      *   2001-08-30  SCHEN      HIPAA 4010 COMPLIANCE
      *   2002-04-11  JMILLER    HIPAA 4010A1 UPDATES
      *   2003-01-15  KWILSON    ADDED NPI SUPPORT
      *   2003-07-22  RTHOMPSON  DUAL NPI/LEGACY ID SUPPORT
      *   2004-05-01  SCHEN      MEDICARE PART D INTEGRATION
      *   2005-03-20  JMILLER    ADDED GLOSSARY TO EOB
      *   2006-01-10  KWILSON    NPI MANDATE COMPLIANCE
      *   2007-07-15  RTHOMPSON  HIPAA 5010 PLANNING
      *   2008-11-01  SCHEN      ADDED CARC/RARC V2 TABLES
      *   2009-06-22  PGARCIA    ADDED PATIENT RESP LETTERS
      *   2010-01-04  JMILLER    HIPAA 5010 IMPLEMENTATION
      *   2010-03-15  KWILSON    5010 SEGMENT CHANGES
      *   2011-01-01  RTHOMPSON  5010X221A1 FINAL COMPLIANCE
      *   2012-04-20  PGARCIA    ACA PREVENTIVE SVC LOGIC
      *   2013-02-15  SCHEN      ADDED BALANCE BILLING NOTICES
      *   2014-06-01  JMILLER    ICD-10 PREP (DUAL CODING)
      *   2015-10-01  KWILSON    ICD-10 FINAL CUTOVER
      *   2016-03-22  RTHOMPSON  EXPANDED RARC TABLE
      *   2017-01-15  PGARCIA    SURPRISE BILLING PROTECTIONS
      *   2018-05-30  SCHEN      OPIOID CLAIM FLAGGING
      *   2019-02-14  JMILLER    NETWORK ADEQUACY NOTICES
      *   2020-03-15  KWILSON    COVID-19 EMERGENCY WAIVERS
      *   2020-07-01  RTHOMPSON  TELEHEALTH EOB LANGUAGE
      *   2021-01-01  PGARCIA    NO SURPRISES ACT PHASE 1
      *   2022-01-01  SCHEN      NO SURPRISES ACT FINAL
      *   2023-06-15  JMILLER    PRICE TRANSPARENCY NOTICES
      *   2024-01-10  KWILSON    UPDATED CARC TABLE V40
      *   2024-09-01  PGARCIA    GFE/EOB RECONCILIATION
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HCREMIT.
       AUTHOR.        ENTERPRISE CLAIMS PROCESSING TEAM.
       INSTALLATION.  CONSOLIDATED HEALTHCARE SYSTEMS INC.
       DATE-WRITTEN.  1994-03-15.
       DATE-COMPILED.
       SECURITY.      CONFIDENTIAL - CONTAINS PHI/PII PROCESSING.
      *                HIPAA SECURITY RULE COMPLIANT.
      *                ACCESS RESTRICTED TO AUTHORIZED PERSONNEL.

      ****************************************************************
       ENVIRONMENT DIVISION.
      ****************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-ZOS WITH DEBUGGING MODE.
       OBJECT-COMPUTER.    IBM-ZOS.
       SPECIAL-NAMES.
           C01 IS PAGE-EJECT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ADJUD-CLAIMS-FILE
               ASSIGN TO ADJCLMS
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ADJCLMS-STATUS.

           SELECT EDI-835-OUTPUT-FILE
               ASSIGN TO EDI835
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-EDI835-STATUS.

           SELECT PROVIDER-REMIT-REPORT
               ASSIGN TO PROVRMIT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PROVRMIT-STATUS.

           SELECT PATIENT-EOB-OUTPUT
               ASSIGN TO PATEOB
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PATEOB-STATUS.

           SELECT CHECK-REGISTER-FILE
               ASSIGN TO CHKREG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CHKREG-STATUS.

           SELECT EFT-OUTPUT-FILE
               ASSIGN TO EFTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-EFTOUT-STATUS.

           SELECT POSITIVE-PAY-FILE
               ASSIGN TO POSPAY
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-POSPAY-STATUS.

           SELECT PAYMENT-SUMMARY-REPORT
               ASSIGN TO PAYSUM
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PAYSUM-STATUS.

           SELECT ERROR-FILE
               ASSIGN TO ERRFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERRFILE-STATUS.

           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO AUDTRL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDTRL-STATUS.

           SELECT RECON-REPORT-FILE
               ASSIGN TO RECONRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RECONRPT-STATUS.

      ****************************************************************
       DATA DIVISION.
      ****************************************************************
       FILE SECTION.

      *--------------------------------------------------------------*
      * ADJUDICATED CLAIMS INPUT FILE
      * SORTED BY PROVIDER TAX-ID, NPI, PAY-TO ADDRESS, PATIENT
      *--------------------------------------------------------------*
       FD  ADJUD-CLAIMS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  ADJUD-CLAIM-RECORD.
           05  AC-RECORD-TYPE              PIC X(02).
               88  AC-CLAIM-HEADER         VALUE 'CH'.
               88  AC-CLAIM-LINE           VALUE 'CL'.
               88  AC-CLAIM-ADJ            VALUE 'CA'.
               88  AC-CLAIM-REMARK         VALUE 'CR'.
               88  AC-CLAIM-COB            VALUE 'CB'.
               88  AC-CLAIM-TRAILER        VALUE 'CT'.
           05  AC-PAYER-ID                 PIC X(10).
           05  AC-PROVIDER-TAX-ID          PIC X(09).
           05  AC-PROVIDER-NPI             PIC X(10).
           05  AC-PROVIDER-LEGACY-ID       PIC X(15).
           05  AC-PAY-TO-PROV-NPI          PIC X(10).
           05  AC-PAY-TO-NAME              PIC X(60).
           05  AC-PAY-TO-ADDR-1            PIC X(55).
           05  AC-PAY-TO-ADDR-2            PIC X(55).
           05  AC-PAY-TO-CITY              PIC X(30).
           05  AC-PAY-TO-STATE             PIC X(02).
           05  AC-PAY-TO-ZIP               PIC X(09).
           05  AC-PATIENT-ACCT-NO          PIC X(20).
           05  AC-MEMBER-ID                PIC X(20).
           05  AC-PATIENT-LAST-NAME        PIC X(35).
           05  AC-PATIENT-FIRST-NAME       PIC X(25).
           05  AC-PATIENT-MI               PIC X(01).
           05  AC-PATIENT-DOB              PIC X(08).
           05  AC-PATIENT-ADDR-1           PIC X(55).
           05  AC-PATIENT-ADDR-2           PIC X(55).
           05  AC-PATIENT-CITY             PIC X(30).
           05  AC-PATIENT-STATE            PIC X(02).
           05  AC-PATIENT-ZIP              PIC X(09).
           05  AC-CLAIM-NUMBER             PIC X(20).
           05  AC-PAYER-CLM-CTRL-NO        PIC X(20).
           05  AC-CLAIM-TYPE               PIC X(02).
               88  AC-PROFESSIONAL         VALUE 'PR'.
               88  AC-INSTITUTIONAL        VALUE 'IN'.
               88  AC-DENTAL               VALUE 'DN'.
               88  AC-VISION               VALUE 'VS'.
           05  AC-CLAIM-STATUS             PIC X(02).
               88  AC-PAID                 VALUE '01'.
               88  AC-DENIED               VALUE '04'.
               88  AC-ZERO-PAY             VALUE '02'.
               88  AC-ADJUSTED             VALUE '22'.
               88  AC-REVERSED             VALUE '23'.
           05  AC-CLAIM-FILING-IND         PIC X(02).
           05  AC-FACILITY-TYPE            PIC X(02).
           05  AC-CLAIM-FREQUENCY          PIC X(01).
           05  AC-DRG-CODE                 PIC X(04).
           05  AC-DRG-WEIGHT               PIC 9(03)V9(04).
           05  AC-ADMIT-DATE               PIC X(08).
           05  AC-DISCHARGE-DATE           PIC X(08).
           05  AC-STATEMENT-FROM-DT        PIC X(08).
           05  AC-STATEMENT-THRU-DT        PIC X(08).
           05  AC-TOTAL-CHARGE-AMT         PIC S9(07)V99.
           05  AC-ALLOWED-AMT              PIC S9(07)V99.
           05  AC-PAID-AMT                 PIC S9(07)V99.
           05  AC-PATIENT-RESP-AMT         PIC S9(07)V99.
           05  AC-DEDUCTIBLE-AMT           PIC S9(07)V99.
           05  AC-COPAY-AMT                PIC S9(07)V99.
           05  AC-COINSURANCE-AMT          PIC S9(07)V99.
           05  AC-COB-AMT                  PIC S9(07)V99.
           05  AC-WITHHOLD-AMT             PIC S9(07)V99.
           05  AC-INTEREST-AMT             PIC S9(07)V99.
           05  AC-PENALTY-AMT              PIC S9(07)V99.
           05  AC-LINE-OF-BUSINESS         PIC X(03).
               88  AC-LOB-COMMERCIAL       VALUE 'COM'.
               88  AC-LOB-HMO              VALUE 'HMO'.
               88  AC-LOB-PPO              VALUE 'PPO'.
               88  AC-LOB-MEDADV           VALUE 'MAD'.
               88  AC-LOB-MEDICAID         VALUE 'MCD'.
               88  AC-LOB-DENTAL           VALUE 'DNT'.
               88  AC-LOB-VISION           VALUE 'VIS'.
           05  AC-PAYMENT-METHOD           PIC X(03).
               88  AC-PAY-CHECK            VALUE 'CHK'.
               88  AC-PAY-EFT              VALUE 'EFT'.
               88  AC-PAY-WIRE             VALUE 'FWT'.
               88  AC-PAY-NON              VALUE 'NON'.
           05  AC-SERVICE-LINE-DATA.
               10  AC-LINE-NUMBER          PIC 9(02).
               10  AC-PROC-CODE            PIC X(05).
               10  AC-PROC-MOD-1           PIC X(02).
               10  AC-PROC-MOD-2           PIC X(02).
               10  AC-PROC-MOD-3           PIC X(02).
               10  AC-PROC-MOD-4           PIC X(02).
               10  AC-REVENUE-CODE         PIC X(04).
               10  AC-NDC-CODE             PIC X(11).
               10  AC-SERVICE-FROM-DT      PIC X(08).
               10  AC-SERVICE-THRU-DT      PIC X(08).
               10  AC-PLACE-OF-SERVICE     PIC X(02).
               10  AC-UNITS                PIC 9(05)V99.
               10  AC-LINE-CHARGE          PIC S9(07)V99.
               10  AC-LINE-ALLOWED         PIC S9(07)V99.
               10  AC-LINE-PAID            PIC S9(07)V99.
               10  AC-LINE-DEDUCTIBLE      PIC S9(07)V99.
               10  AC-LINE-COPAY           PIC S9(07)V99.
               10  AC-LINE-COINSURANCE     PIC S9(07)V99.
               10  AC-LINE-COB             PIC S9(07)V99.
               10  AC-LINE-PATIENT-RESP    PIC S9(07)V99.
           05  AC-ADJUSTMENT-DATA.
               10  AC-ADJ-GROUP-CODE       PIC X(02).
               10  AC-ADJ-REASON-CODE      PIC X(05).
               10  AC-ADJ-AMOUNT           PIC S9(07)V99.
               10  AC-ADJ-QUANTITY         PIC 9(05)V99.
           05  AC-REMARK-DATA.
               10  AC-REMARK-CODE          PIC X(05).
               10  AC-REMARK-QUAL          PIC X(02).
           05  AC-COB-DATA.
               10  AC-COB-PAYER-ID         PIC X(10).
               10  AC-COB-PAID-AMT         PIC S9(07)V99.
               10  AC-COB-PAYER-NAME       PIC X(60).
           05  AC-RENDERING-PROV-NPI       PIC X(10).
           05  AC-RENDERING-PROV-NAME      PIC X(60).
           05  AC-REFERRING-PROV-NPI       PIC X(10).
           05  AC-DIAGNOSIS-CODES.
               10  AC-DIAG-CODE            PIC X(08)
                   OCCURS 12 TIMES.
           05  AC-DIAG-CODE-QUAL           PIC X(03).
           05  AC-AUTH-NUMBER              PIC X(20).
           05  AC-REFERRAL-NUMBER          PIC X(20).
           05  AC-ORIGINAL-REF-NO          PIC X(20).
           05  AC-FILLER                   PIC X(339).

      *--------------------------------------------------------------*
      * 835 EDI TRANSACTION OUTPUT FILE
      *--------------------------------------------------------------*
       FD  EDI-835-OUTPUT-FILE
           RECORDING MODE IS V
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 1 TO 500 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  EDI-835-RECORD                  PIC X(500).

      *--------------------------------------------------------------*
      * PROVIDER REMITTANCE ADVICE REPORT (132 COL PRINT)
      *--------------------------------------------------------------*
       FD  PROVIDER-REMIT-REPORT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  PROV-REMIT-LINE                 PIC X(132).

      *--------------------------------------------------------------*
      * PATIENT EOB OUTPUT
      *--------------------------------------------------------------*
       FD  PATIENT-EOB-OUTPUT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  PATIENT-EOB-LINE                PIC X(132).

      *--------------------------------------------------------------*
      * CHECK REGISTER OUTPUT
      *--------------------------------------------------------------*
       FD  CHECK-REGISTER-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  CHECK-REG-RECORD.
           05  CR-CHECK-NUMBER             PIC X(10).
           05  CR-CHECK-DATE               PIC X(08).
           05  CR-PAYEE-NAME               PIC X(60).
           05  CR-PAYEE-TAX-ID             PIC X(09).
           05  CR-PAYEE-NPI                PIC X(10).
           05  CR-GROSS-AMOUNT             PIC S9(07)V99.
           05  CR-WITHHOLD-AMT             PIC S9(07)V99.
           05  CR-ADJUSTMENT-AMT           PIC S9(07)V99.
           05  CR-INTEREST-AMT             PIC S9(07)V99.
           05  CR-NET-AMOUNT               PIC S9(07)V99.
           05  CR-PAYMENT-METHOD           PIC X(03).
           05  CR-EFT-TRACE-NO             PIC X(15).
           05  CR-STATUS                   PIC X(02).
               88  CR-STATUS-ISSUED        VALUE 'IS'.
               88  CR-STATUS-VOIDED        VALUE 'VD'.
               88  CR-STATUS-STOPPED       VALUE 'ST'.
               88  CR-STATUS-CLEARED       VALUE 'CL'.
           05  CR-CLAIM-COUNT              PIC 9(05).
           05  CR-PAYER-ID                 PIC X(10).
           05  CR-LOB                      PIC X(03).
           05  CR-FILLER                   PIC X(17).

      *--------------------------------------------------------------*
      * EFT/ACH OUTPUT FILE (NACHA FORMAT)
      *--------------------------------------------------------------*
       FD  EFT-OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 94 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  EFT-NACHA-RECORD                PIC X(94).

      *--------------------------------------------------------------*
      * POSITIVE PAY FILE
      *--------------------------------------------------------------*
       FD  POSITIVE-PAY-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  POS-PAY-RECORD.
           05  PP-ACCOUNT-NUMBER           PIC X(15).
           05  PP-CHECK-NUMBER             PIC X(10).
           05  PP-CHECK-DATE               PIC X(08).
           05  PP-AMOUNT                   PIC 9(08)V99.
           05  PP-PAYEE-NAME               PIC X(50).
           05  PP-VOID-INDICATOR           PIC X(01).
           05  PP-FILLER                   PIC X(06).

      *--------------------------------------------------------------*
      * PAYMENT SUMMARY REPORT
      *--------------------------------------------------------------*
       FD  PAYMENT-SUMMARY-REPORT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  PAY-SUMMARY-LINE                PIC X(132).

      *--------------------------------------------------------------*
      * ERROR / EXCEPTION FILE
      *--------------------------------------------------------------*
       FD  ERROR-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  ERROR-RECORD.
           05  ER-TIMESTAMP                PIC X(26).
           05  ER-SEVERITY                 PIC X(01).
               88  ER-INFO                 VALUE 'I'.
               88  ER-WARNING              VALUE 'W'.
               88  ER-ERROR                VALUE 'E'.
               88  ER-FATAL                VALUE 'F'.
           05  ER-PROGRAM                  PIC X(08).
           05  ER-PARAGRAPH                PIC X(30).
           05  ER-CLAIM-NUMBER             PIC X(20).
           05  ER-ERROR-CODE               PIC X(10).
           05  ER-ERROR-DESC               PIC X(105).

      *--------------------------------------------------------------*
      * AUDIT TRAIL FILE
      *--------------------------------------------------------------*
       FD  AUDIT-TRAIL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  AUDIT-RECORD.
           05  AT-TIMESTAMP                PIC X(26).
           05  AT-ACTION                   PIC X(10).
           05  AT-ENTITY-TYPE              PIC X(10).
           05  AT-ENTITY-KEY               PIC X(30).
           05  AT-OLD-VALUE                PIC X(50).
           05  AT-NEW-VALUE                PIC X(50).
           05  AT-USER-ID                  PIC X(08).
           05  AT-PROGRAM                  PIC X(08).
           05  AT-PARAGRAPH                PIC X(30).
           05  AT-DESCRIPTION              PIC X(78).

      *--------------------------------------------------------------*
      * RECONCILIATION REPORT
      *--------------------------------------------------------------*
       FD  RECON-REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.

       01  RECON-REPORT-LINE               PIC X(132).

      ****************************************************************
       WORKING-STORAGE SECTION.
      ****************************************************************

       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'HCREMIT'.
       01  WS-PROGRAM-VERSION              PIC X(10) VALUE '24.09.01'.
       01  WS-COPYRIGHT.
           05  FILLER                      PIC X(50) VALUE
               'COPYRIGHT 1994-2024 CONSOLIDATED HEALTHCARE SYS'.

      *--------------------------------------------------------------*
      * COPYBOOK INCLUDES
      *--------------------------------------------------------------*
           COPY CPYCLMHD.
           COPY CPYCLMLN.
           COPY CPYPROVD.
           COPY CPYSQLCA.
           COPY CPYERROR.

      *--------------------------------------------------------------*
      * FILE STATUS FIELDS
      *--------------------------------------------------------------*
       01  WS-FILE-STATUS-FIELDS.
           05  WS-ADJCLMS-STATUS           PIC X(02).
           05  WS-EDI835-STATUS            PIC X(02).
           05  WS-PROVRMIT-STATUS          PIC X(02).
           05  WS-PATEOB-STATUS            PIC X(02).
           05  WS-CHKREG-STATUS            PIC X(02).
           05  WS-EFTOUT-STATUS            PIC X(02).
           05  WS-POSPAY-STATUS            PIC X(02).
           05  WS-PAYSUM-STATUS            PIC X(02).
           05  WS-ERRFILE-STATUS           PIC X(02).
           05  WS-AUDTRL-STATUS            PIC X(02).
           05  WS-RECONRPT-STATUS          PIC X(02).

      *--------------------------------------------------------------*
      * PROGRAM CONTROL FLAGS
      *--------------------------------------------------------------*
       01  WS-PROGRAM-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-END-OF-FILE          VALUE 'Y'.
               88  WS-NOT-END-OF-FILE      VALUE 'N'.
           05  WS-FIRST-RECORD-FLAG        PIC X(01) VALUE 'Y'.
               88  WS-FIRST-RECORD         VALUE 'Y'.
               88  WS-NOT-FIRST-RECORD     VALUE 'N'.
           05  WS-PROVIDER-BREAK           PIC X(01) VALUE 'N'.
               88  WS-NEW-PROVIDER         VALUE 'Y'.
               88  WS-SAME-PROVIDER        VALUE 'N'.
           05  WS-PATIENT-BREAK            PIC X(01) VALUE 'N'.
               88  WS-NEW-PATIENT          VALUE 'Y'.
               88  WS-SAME-PATIENT         VALUE 'N'.
           05  WS-PAYER-BREAK              PIC X(01) VALUE 'N'.
               88  WS-NEW-PAYER            VALUE 'Y'.
               88  WS-SAME-PAYER           VALUE 'N'.
           05  WS-835-NEEDED               PIC X(01) VALUE 'Y'.
               88  WS-GENERATE-835         VALUE 'Y'.
           05  WS-REMIT-NEEDED             PIC X(01) VALUE 'Y'.
               88  WS-GENERATE-REMIT       VALUE 'Y'.
           05  WS-EOB-NEEDED               PIC X(01) VALUE 'Y'.
               88  WS-GENERATE-EOB         VALUE 'Y'.
           05  WS-EFT-ACTIVE               PIC X(01) VALUE 'N'.
               88  WS-PROVIDER-USES-EFT    VALUE 'Y'.
               88  WS-PROVIDER-USES-CHECK  VALUE 'N'.
           05  WS-NEGATIVE-BALANCE-FLAG    PIC X(01) VALUE 'N'.
               88  WS-HAS-NEGATIVE-BAL     VALUE 'Y'.
           05  WS-SPLIT-CHECK-FLAG         PIC X(01) VALUE 'N'.
               88  WS-CHECK-NEEDS-SPLIT    VALUE 'Y'.
           05  WS-MIN-PAY-FLAG             PIC X(01) VALUE 'N'.
               88  WS-BELOW-MIN-PAYMENT    VALUE 'Y'.
           05  WS-DB-ERROR-FLAG            PIC X(01) VALUE 'N'.
               88  WS-DB-ERROR-OCCURRED    VALUE 'Y'.
           05  WS-DEADLOCK-FLAG            PIC X(01) VALUE 'N'.
               88  WS-DEADLOCK-DETECTED    VALUE 'Y'.
           05  WS-PRENOTE-FLAG             PIC X(01) VALUE 'N'.
               88  WS-IS-PRENOTE           VALUE 'Y'.
           05  WS-HOLIDAY-FLAG             PIC X(01) VALUE 'N'.
               88  WS-IS-HOLIDAY           VALUE 'Y'.
           05  WS-WEEKEND-FLAG             PIC X(01) VALUE 'N'.
               88  WS-IS-WEEKEND           VALUE 'Y'.
           05  WS-VOID-FLAG                PIC X(01) VALUE 'N'.
               88  WS-IS-VOID              VALUE 'Y'.
           05  WS-REISSUE-FLAG             PIC X(01) VALUE 'N'.
               88  WS-IS-REISSUE           VALUE 'Y'.
           05  WS-SUPPRESS-EOB-FLAG        PIC X(01) VALUE 'N'.
               88  WS-SUPPRESS-EOB         VALUE 'Y'.

      *--------------------------------------------------------------*
      * DATE/TIME FIELDS
      *--------------------------------------------------------------*
       01  WS-DATE-TIME-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR            PIC 9(04).
               10  WS-CURR-MONTH           PIC 9(02).
               10  WS-CURR-DAY             PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR            PIC 9(02).
               10  WS-CURR-MINUTE          PIC 9(02).
               10  WS-CURR-SECOND          PIC 9(02).
               10  WS-CURR-HUNDREDTH       PIC 9(02).
           05  WS-GMT-OFFSET               PIC S9(04).
           05  WS-YYYYMMDD                 PIC 9(08).
           05  WS-YYMMDD                   PIC 9(06).
           05  WS-HHMM                     PIC 9(04).
           05  WS-HHMMSS                   PIC 9(06).
           05  WS-CCYYMMDD-DASH            PIC X(10).
           05  WS-PAYMENT-DATE             PIC X(08).
           05  WS-PAYMENT-DATE-DASH        PIC X(10).
           05  WS-EFT-EFFECTIVE-DATE       PIC X(08).
           05  WS-CHECK-DATE               PIC X(08).
           05  WS-EDI-DATE                 PIC X(08).
           05  WS-EDI-TIME                 PIC X(04).
           05  WS-JULIAN-DATE              PIC 9(07).
           05  WS-DAY-OF-WEEK              PIC 9(01).
           05  WS-WORK-DATE                PIC 9(08).
           05  WS-WORK-YEAR               PIC 9(04).
           05  WS-WORK-MONTH              PIC 9(02).
           05  WS-WORK-DAY                PIC 9(02).
           05  WS-TIMESTAMP                PIC X(26).

      *--------------------------------------------------------------*
      * PREVIOUS KEY FIELDS FOR BREAK DETECTION
      *--------------------------------------------------------------*
       01  WS-PREV-KEYS.
           05  WS-PREV-PROVIDER-TAX-ID     PIC X(09).
           05  WS-PREV-PROVIDER-NPI        PIC X(10).
           05  WS-PREV-PAY-TO-NPI          PIC X(10).
           05  WS-PREV-PAY-TO-NAME         PIC X(60).
           05  WS-PREV-PAY-TO-ADDR-1       PIC X(55).
           05  WS-PREV-PAYER-ID            PIC X(10).
           05  WS-PREV-MEMBER-ID           PIC X(20).
           05  WS-PREV-PATIENT-ACCT        PIC X(20).
           05  WS-PREV-CLAIM-NUMBER        PIC X(20).

      *--------------------------------------------------------------*
      * 835 TRANSACTION CONTROL NUMBERS
      *--------------------------------------------------------------*
       01  WS-835-CONTROL-NUMBERS.
           05  WS-ISA-CONTROL-NUM          PIC 9(09) VALUE ZEROS.
           05  WS-GS-CONTROL-NUM           PIC 9(09) VALUE ZEROS.
           05  WS-ST-CONTROL-NUM           PIC 9(09) VALUE ZEROS.
           05  WS-SEGMENT-COUNT            PIC 9(09) VALUE ZEROS.
           05  WS-TRANS-SET-COUNT          PIC 9(06) VALUE ZEROS.
           05  WS-FUNC-GROUP-COUNT         PIC 9(06) VALUE ZEROS.
           05  WS-CLP-COUNT                PIC 9(09) VALUE ZEROS.
           05  WS-SVC-COUNT                PIC 9(09) VALUE ZEROS.
           05  WS-CAS-COUNT                PIC 9(09) VALUE ZEROS.

      *--------------------------------------------------------------*
      * 835 EDI SEGMENT BUILDERS
      *--------------------------------------------------------------*
       01  WS-EDI-ELEMENT-SEP              PIC X(01) VALUE '*'.
       01  WS-EDI-SEGMENT-TERM             PIC X(01) VALUE '~'.
       01  WS-EDI-SUB-ELEM-SEP             PIC X(01) VALUE ':'.
       01  WS-EDI-REPEAT-SEP               PIC X(01) VALUE '^'.

       01  WS-835-ISA-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'ISA'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA01-AUTH-QUAL          PIC X(02) VALUE '00'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA02-AUTH-INFO          PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA03-SEC-QUAL           PIC X(02) VALUE '00'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA04-SEC-INFO           PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA05-SEND-QUAL          PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA06-SENDER-ID          PIC X(15).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA07-RECV-QUAL          PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA08-RECEIVER-ID        PIC X(15).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA09-DATE               PIC X(06).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA10-TIME               PIC X(04).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA11-REPEAT-SEP         PIC X(01) VALUE '^'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA12-VERSION            PIC X(05) VALUE '00501'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA13-CONTROL-NUM        PIC 9(09).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA14-ACK-REQ            PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA15-USAGE-IND          PIC X(01).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ISA16-COMP-SEP           PIC X(01) VALUE ':'.

       01  WS-835-IEA-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'IEA'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-IEA01-GROUP-CNT         PIC 9(01).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-IEA02-CONTROL-NUM        PIC 9(09).

       01  WS-835-GS-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'GS'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS01-FUNC-ID            PIC X(02) VALUE 'HP'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS02-SENDER-CODE        PIC X(15).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS03-RECEIVER-CODE      PIC X(15).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS04-DATE               PIC X(08).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS05-TIME               PIC X(04).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS06-GROUP-CTRL         PIC 9(09).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS07-AGENCY-CODE        PIC X(01) VALUE 'X'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GS08-VERSION            PIC X(12)
               VALUE '005010X221A1'.

       01  WS-835-GE-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'GE'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GE01-TRANS-CNT          PIC 9(06).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-GE02-GROUP-CTRL         PIC 9(09).

       01  WS-835-ST-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'ST'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ST01-TRANS-ID           PIC X(03) VALUE '835'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ST02-CONTROL-NUM        PIC X(09).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-ST03-IMPL-REF           PIC X(12)
               VALUE '005010X221A1'.

       01  WS-835-SE-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'SE'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SE01-SEG-COUNT          PIC 9(09).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SE02-CONTROL-NUM        PIC X(09).

       01  WS-835-BPR-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'BPR'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR01-TRANS-TYPE        PIC X(01).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR02-TOTAL-AMT         PIC S9(09)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR03-CREDIT-DEBIT      PIC X(01).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR04-PAY-METHOD        PIC X(03).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR05-PAY-FORMAT        PIC X(03).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR06-DFI-QUAL-SEND     PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR07-SEND-ROUTING      PIC X(09).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR08-SEND-ACCT-TYPE    PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR09-SEND-ACCT-NUM     PIC X(17).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR10-ORIG-CO-ID        PIC X(10).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR11-ORIG-CO-SUPP      PIC X(10).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR12-DFI-QUAL-RECV     PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR13-RECV-ROUTING      PIC X(09).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR14-RECV-ACCT-TYPE    PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR15-RECV-ACCT-NUM     PIC X(17).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-BPR16-PAY-DATE          PIC X(08).

       01  WS-835-TRN-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'TRN'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-TRN01-TRACE-TYPE        PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-TRN02-CHECK-EFT-NO      PIC X(20).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-TRN03-ORIGINATOR-ID     PIC X(10).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-TRN04-ORIG-SUPP-CODE    PIC X(10).

       01  WS-835-REF-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'REF'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-REF01-QUALIFIER         PIC X(03).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-REF02-IDENTIFIER        PIC X(50).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-REF03-DESCRIPTION       PIC X(80).

       01  WS-835-DTM-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'DTM'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-DTM01-QUALIFIER         PIC X(03).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-DTM02-DATE              PIC X(08).

       01  WS-835-N1-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'N1'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N101-ENTITY-ID          PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N102-NAME               PIC X(60).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N103-ID-QUAL            PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N104-ID-CODE            PIC X(20).

       01  WS-835-N3-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'N3'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N301-ADDRESS-1          PIC X(55).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N302-ADDRESS-2          PIC X(55).

       01  WS-835-N4-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'N4'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N401-CITY               PIC X(30).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N402-STATE              PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-N403-ZIP                PIC X(09).

       01  WS-835-CLP-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'CLP'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP01-PAT-CTRL-NO      PIC X(20).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP02-STATUS            PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP03-TOTAL-CHARGE      PIC S9(09)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP04-PAID-AMT          PIC S9(09)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP05-PAT-RESP          PIC S9(09)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP06-FILING-IND        PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP07-PAYER-CLM-NO      PIC X(20).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP08-FACILITY-TYPE     PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CLP09-FREQUENCY         PIC X(01).

       01  WS-835-CAS-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'CAS'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-CAS01-GROUP-CODE        PIC X(02).
           05  WS-CAS-ADJ-ENTRIES.
               10  WS-CAS-ADJ-ENTRY       OCCURS 6 TIMES.
                   15  FILLER              PIC X(01) VALUE '*'.
                   15  WS-CAS-REASON-CODE  PIC X(05).
                   15  FILLER              PIC X(01) VALUE '*'.
                   15  WS-CAS-ADJ-AMT     PIC S9(09)V99.
                   15  FILLER              PIC X(01) VALUE '*'.
                   15  WS-CAS-ADJ-QTY     PIC 9(05).

       01  WS-835-SVC-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'SVC'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC01-PROC-ID.
               10  WS-SVC01-QUAL          PIC X(02).
               10  WS-SVC01-SEP           PIC X(01) VALUE ':'.
               10  WS-SVC01-PROC-CODE     PIC X(05).
               10  WS-SVC01-MOD1-SEP      PIC X(01) VALUE ':'.
               10  WS-SVC01-MOD1          PIC X(02).
               10  WS-SVC01-MOD2-SEP      PIC X(01) VALUE ':'.
               10  WS-SVC01-MOD2          PIC X(02).
               10  WS-SVC01-MOD3-SEP      PIC X(01) VALUE ':'.
               10  WS-SVC01-MOD3          PIC X(02).
               10  WS-SVC01-MOD4-SEP      PIC X(01) VALUE ':'.
               10  WS-SVC01-MOD4          PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC02-LINE-CHARGE       PIC S9(09)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC03-LINE-PAID         PIC S9(09)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC04-REV-CODE          PIC X(04).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC05-UNITS-PAID        PIC 9(05)V99.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC06-ORIG-PROC.
               10  WS-SVC06-QUAL          PIC X(02).
               10  WS-SVC06-SEP           PIC X(01) VALUE ':'.
               10  WS-SVC06-PROC-CODE     PIC X(05).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-SVC07-ORIG-UNITS        PIC 9(05)V99.

       01  WS-835-AMT-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'AMT'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-AMT01-QUALIFIER         PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-AMT02-AMOUNT            PIC S9(09)V99.

       01  WS-835-QTY-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'QTY'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-QTY01-QUALIFIER         PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-QTY02-QUANTITY          PIC 9(09)V99.

       01  WS-835-LQ-SEGMENT.
           05  FILLER                      PIC X(02) VALUE 'LQ'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-LQ01-CODE-QUAL          PIC X(02).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-LQ02-REMARK-CODE        PIC X(05).

       01  WS-835-PLB-SEGMENT.
           05  FILLER                      PIC X(03) VALUE 'PLB'.
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-PLB01-PROV-ID           PIC X(10).
           05  FILLER                      PIC X(01) VALUE '*'.
           05  WS-PLB02-FISCAL-PERIOD     PIC X(08).
           05  WS-PLB-ADJ-ENTRIES.
               10  WS-PLB-ADJ-ENTRY       OCCURS 6 TIMES.
                   15  FILLER              PIC X(01) VALUE '*'.
                   15  WS-PLB-ADJ-ID.
                       20  WS-PLB-ADJ-REASON
                                           PIC X(02).
                       20  WS-PLB-REF-SEP  PIC X(01) VALUE ':'.
                       20  WS-PLB-ADJ-REF  PIC X(20).
                   15  FILLER              PIC X(01) VALUE '*'.
                   15  WS-PLB-ADJ-AMT     PIC S9(09)V99.

       01  WS-EDI-OUTPUT-BUFFER            PIC X(500).
       01  WS-EDI-BUFFER-LEN              PIC 9(03) VALUE ZEROS.

      *--------------------------------------------------------------*
      * CARC (CLAIM ADJUSTMENT REASON CODE) TABLE - 120 ENTRIES
      *--------------------------------------------------------------*
       01  WS-CARC-TABLE-SIZE             PIC 9(03) VALUE 120.
       01  WS-CARC-TABLE.
           05  FILLER PIC X(55) VALUE
               '00001DEDUCTIBLE AMOUNT                            '.
           05  FILLER PIC X(55) VALUE
               '00002COINSURANCE AMOUNT                           '.
           05  FILLER PIC X(55) VALUE
               '00003COPAY AMOUNT                                 '.
           05  FILLER PIC X(55) VALUE
               '00004THE PROCEDURE CODE IS INCONSISTENT WITH MOD  '.
           05  FILLER PIC X(55) VALUE
               '00005THE PROCEDURE CODE/BILL TYPE IS INCONSISTENT '.
           05  FILLER PIC X(55) VALUE
               '00006THE PROCEDURE/REVENUE CODE IS INCONSISTENT   '.
           05  FILLER PIC X(55) VALUE
               '00009THE DIAGNOSIS IS INCONSISTENT WITH THE PATIEN'.
           05  FILLER PIC X(55) VALUE
               '00010THE DIAGNOSIS IS INCONSISTENT WITH PROCEDURE '.
           05  FILLER PIC X(55) VALUE
               '00011THE DIAGNOSIS IS INCONSISTENT WITH PLACE SRVC'.
           05  FILLER PIC X(55) VALUE
               '00013THE DATE OF DEATH PRECEDES THE DATE OF SERVIC'.
           05  FILLER PIC X(55) VALUE
               '00014THE DATE OF BIRTH FOLLOWS THE DATE OF SERVICE'.
           05  FILLER PIC X(55) VALUE
               '00015THE AUTHORIZATION NUMBER IS MISSING/INVALID   '.
           05  FILLER PIC X(55) VALUE
               '00016CLAIM/SERVICE LACKS INFORMATION NEEDED FOR ADJ'.
           05  FILLER PIC X(55) VALUE
               '00018EXACT DUPLICATE CLAIM/SERVICE                 '.
           05  FILLER PIC X(55) VALUE
               '00019CLAIM DENIED - THIS IS A WORK-RELATED INJURY '.
           05  FILLER PIC X(55) VALUE
               '00020THIS INJURY/ILLNESS IS COVERED BY LIABILITY IN'.
           05  FILLER PIC X(55) VALUE
               '00022THIS CARE MAY BE COVERED BY ANOTHER PAYER     '.
           05  FILLER PIC X(55) VALUE
               '00023THE IMPACT OF PRIOR PAYER ADJUDICATION        '.
           05  FILLER PIC X(55) VALUE
               '00024CHARGES ARE COVERED UNDER A CAPITATION AGRMNT '.
           05  FILLER PIC X(55) VALUE
               '00025PAYMENT TO PATIENT/INSURED/RESPONSIBLE PARTY  '.
           05  FILLER PIC X(55) VALUE
               '00026EXPENSES INCURRED PRIOR TO/AFTER COVERAGE     '.
           05  FILLER PIC X(55) VALUE
               '00027EXPENSES INCURRED AFTER COVERAGE TERMINATED   '.
           05  FILLER PIC X(55) VALUE
               '00029THE TIME LIMIT FOR FILING HAS EXPIRED         '.
           05  FILLER PIC X(55) VALUE
               '00031PATIENT CANNOT BE IDENTIFIED AS OUR INSURED   '.
           05  FILLER PIC X(55) VALUE
               '00032OUR RECORDS DO NOT INDICATE REFERRAL GIVEN    '.
           05  FILLER PIC X(55) VALUE
               '00033SERVICES CANNOT BE DETERMINED MEDICALLY NECESS'.
           05  FILLER PIC X(55) VALUE
               '00034INSURED HAS NO DEPENDENT COVERAGE             '.
           05  FILLER PIC X(55) VALUE
               '00035LIFETIME BENEFIT MAXIMUM HAS BEEN REACHED     '.
           05  FILLER PIC X(55) VALUE
               '00039SERVICES DENIED AT THE TIME AUTHORIZATION REQD'.
           05  FILLER PIC X(55) VALUE
               '00040CHARGES DO NOT MEET QUALIFICATIONS FOR EMRGNCY'.
           05  FILLER PIC X(55) VALUE
               '00044PROMPT-PAY DISCOUNT                           '.
           05  FILLER PIC X(55) VALUE
               '00045CHARGE EXCEEDS FEE SCHEDULE/MAXIMUM ALLOWABLE '.
           05  FILLER PIC X(55) VALUE
               '00049ROUTINE/PREVENTIVE EXAM - NON-COVERED CHARGE  '.
           05  FILLER PIC X(55) VALUE
               '00050THESE ARE NON-COVERED SERVICES - NOT A BENEFIT'.
           05  FILLER PIC X(55) VALUE
               '00051THESE ARE NON-COVERED SERVICES - PREEXISTING  '.
           05  FILLER PIC X(55) VALUE
               '00053SERVICES BY AN IMMEDIATE RELATIVE             '.
           05  FILLER PIC X(55) VALUE
               '00054MULTIPLE PHYSICIANS/AMBULANCE SUPPLIERS       '.
           05  FILLER PIC X(55) VALUE
               '00055PROCEDURE/TREATMENT NOT PROVIDED WITHIN SCOPE '.
           05  FILLER PIC X(55) VALUE
               '00056PROCEDURE/TREATMENT HAS NOT BEEN DEEMED SAFE  '.
           05  FILLER PIC X(55) VALUE
               '00058TREATMENT WAS DEEMED BY THE PAYER EXPERIMENTAL'.
           05  FILLER PIC X(55) VALUE
               '00059PROCESSED BASED ON MULTIPLE OR CONCURRENT PROC'.
           05  FILLER PIC X(55) VALUE
               '00060CHARGES FOR OUTPATIENT SERVICES NOT ADJUDICATED'.
           05  FILLER PIC X(55) VALUE
               '00065PROCEDURE CODE WAS ADDED/CHANGED - SEE REMIT  '.
           05  FILLER PIC X(55) VALUE
               '00066BLOOD DEDUCTIBLE                              '.
           05  FILLER PIC X(55) VALUE
               '00069DAY OUTLIER AMOUNT                            '.
           05  FILLER PIC X(55) VALUE
               '00070COST OUTLIER - ADJUSTMENT TO COST OUTLIER     '.
           05  FILLER PIC X(55) VALUE
               '00089PROFESSIONAL COMPONENT INCLUDED IN ALLOWANCE  '.
           05  FILLER PIC X(55) VALUE
               '00090INGREDIENT COST ADJUSTMENT                    '.
           05  FILLER PIC X(55) VALUE
               '00091DISPENSING FEE ADJUSTMENT                     '.
           05  FILLER PIC X(55) VALUE
               '00094PROCESSED IN EXCESS OF CHARGES                '.
           05  FILLER PIC X(55) VALUE
               '00095PLAN PROCEDURES NOT FOLLOWED                  '.
           05  FILLER PIC X(55) VALUE
               '00096NON-COVERED CHARGE(S)                         '.
           05  FILLER PIC X(55) VALUE
               '00097THE BENEFIT FOR THIS SERVICE IS INCLUDED IN PA'.
           05  FILLER PIC X(55) VALUE
               '00100PAYMENT MADE TO PATIENT/INSURED/RESPONSIBLE PT'.
           05  FILLER PIC X(55) VALUE
               '00101PREDETERMINATION: ANTICIPATED PAYMENT UPON COMP'.
           05  FILLER PIC X(55) VALUE
               '00102MAJOR MEDICAL ADJUSTMENT                      '.
           05  FILLER PIC X(55) VALUE
               '00103PROVIDER NOT ON FILE/NOT ELIGIBLE AT DOS       '.
           05  FILLER PIC X(55) VALUE
               '00104MANAGED CARE WITHHOLDING                      '.
           05  FILLER PIC X(55) VALUE
               '00105TAX WITHHOLDING                               '.
           05  FILLER PIC X(55) VALUE
               '00106PATIENT PAYMENT OPTION/ELECTION NOT IN EFFECT '.
           05  FILLER PIC X(55) VALUE
               '00107THE RELATED/AUTHORIZED CLAIM HAS BEEN REVERSED'.
           05  FILLER PIC X(55) VALUE
               '00108THE RENT/PURCHASE GUIDELINE WAS NOT MET       '.
           05  FILLER PIC X(55) VALUE
               '00109CLAIM/SERVICE NOT COVERED BY THIS PAYER/CONTRA'.
           05  FILLER PIC X(55) VALUE
               '00110BILLING DATE PREDATES SERVICE DATE            '.
           05  FILLER PIC X(55) VALUE
               '00111NOT COVERED UNLESS THE PROVIDER ACCEPTS ASSIGN'.
           05  FILLER PIC X(55) VALUE
               '00112SERVICE NOT FURNISHED DIRECTLY TO THE PATIENT '.
           05  FILLER PIC X(55) VALUE
               '00114PROCEDURE/PRODUCT NOT APPROVED BY THE FDA      '.
           05  FILLER PIC X(55) VALUE
               '00115PROCEDURE POSTPONED OR CANCELLED              '.
           05  FILLER PIC X(55) VALUE
               '00116THE ADVANCE INDEMNIFICATION NOTICE WAS NOT SIGN'.
           05  FILLER PIC X(55) VALUE
               '00117TRANSPORTATION IS ONLY COVERED TO NEAREST FACIL'.
           05  FILLER PIC X(55) VALUE
               '00118ROUTINE/PREVENTIVE CARE NOT PAYABLE IN INST SET'.
           05  FILLER PIC X(55) VALUE
               '00119BENEFIT MAXIMUM FOR THIS TIME PERIOD HAS BEEN R'.
           05  FILLER PIC X(55) VALUE
               '00121INDEMNIFICATION ADJUSTMENT - OVERPAYMENT RECOVE'.
           05  FILLER PIC X(55) VALUE
               '00122PSYCHIATRIC REDUCTION                         '.
           05  FILLER PIC X(55) VALUE
               '00125PAYMENT ADJUSTED - SUBMITTING PROV NOT ELIGIBLE'.
           05  FILLER PIC X(55) VALUE
               '00128NEWBORN COVERAGE NOT IN EFFECT                '.
           05  FILLER PIC X(55) VALUE
               '00129PRIOR PROCESSING INFORMATION                  '.
           05  FILLER PIC X(55) VALUE
               '00130CLAIM SUBMISSION FEE - NOT PAYABLE IN NETWORK '.
           05  FILLER PIC X(55) VALUE
               '00131CLAIM SPECIFIC NEGOTIATED DISCOUNT            '.
           05  FILLER PIC X(55) VALUE
               '00132PREMATURITY ADJUSTMENT - SVC IN EXCESS OF DAYS'.
           05  FILLER PIC X(55) VALUE
               '00133THE DISPOSITION OF THIS SERVICE LINE IS PENDING'.
           05  FILLER PIC X(55) VALUE
               '00134TECHNICAL COMPONENT INCLUDED IN ALLOWANCE     '.
           05  FILLER PIC X(55) VALUE
               '00135CLAIM DENIED - SEE PREVIOUS CARRIER DETERMINATN'.
           05  FILLER PIC X(55) VALUE
               '00136FAILURE TO OBTAIN SECOND SURGICAL OPINION      '.
           05  FILLER PIC X(55) VALUE
               '00137REGULATORY SURCHARGES, ASSESSMENTS, ALLOWANCE  '.
           05  FILLER PIC X(55) VALUE
               '00139CONTRACTED/NEGOTIATED RATE EXPIRED            '.
           05  FILLER PIC X(55) VALUE
               '00140PATIENT/INSURED HEALTH ID IS MISSING          '.
           05  FILLER PIC X(55) VALUE
               '00142MONTHLY BENEFIT MAXIMUM HAS BEEN REACHED      '.
           05  FILLER PIC X(55) VALUE
               '00143PORTION OF PAYMENT DEFERRED                   '.
           05  FILLER PIC X(55) VALUE
               '00144INCENTIVE ADJUSTMENT - E.G. PREFERRED PROVIDER '.
           05  FILLER PIC X(55) VALUE
               '00146DIAGNOSIS WAS INVALID FOR THE DATE(S) OF SERVIC'.
           05  FILLER PIC X(55) VALUE
               '00147PROVIDER NOT CERTIFIED/ELIGIBLE TO BE PAID FOR '.
           05  FILLER PIC X(55) VALUE
               '00148INFORMATION WAS NOT FURNISHED PREVIOUSLY       '.
           05  FILLER PIC X(55) VALUE
               '00149LIFETIME BENEFIT MAXIMUM HAS BEEN REACHED SVC '.
           05  FILLER PIC X(55) VALUE
               '00150PAYER DEEMS THE INFORMATION SUBMITTED NOT SUPPO'.
           05  FILLER PIC X(55) VALUE
               '00151THIS IS NOT THE PATIENTS PRIMARY PLAN         '.
           05  FILLER PIC X(55) VALUE
               '00152PAYER DEEMS THE PATIENT INELIGIBLE             '.
           05  FILLER PIC X(55) VALUE
               '00153SEQUESTRATION - REDUCTION IN FED PAYMENT       '.
           05  FILLER PIC X(55) VALUE
               '00160INJURY/ILLNESS WAS THE RESULT OF AN ACTIVITY  '.
           05  FILLER PIC X(55) VALUE
               '00163ATTACHMENT/OTHER DOCUMENTATION IS REQUIRED     '.
           05  FILLER PIC X(55) VALUE
               '00167DIAGNOSIS IS NOT COVERED FOR THIS PROV TYPE   '.
           05  FILLER PIC X(55) VALUE
               '00170PAYMENT IS DENIED - INCORRECT PROVIDER SPECIALI'.
           05  FILLER PIC X(55) VALUE
               '00171PAYMENT IS DENIED - RX DRUG NOT LICENSED/CERTIF'.
           05  FILLER PIC X(55) VALUE
               '00172PAYMENT ADJUSTED BECAUSE DUPL OF WHAT IS COVERD'.
           05  FILLER PIC X(55) VALUE
               '00173PAYMENT ADJUSTED - CONTRACTED/NEGOTIATED FEE SC'.
           05  FILLER PIC X(55) VALUE
               '00175PAYMENT DENIED - DUPLICATE OF EXISTING CLAIM   '.
           05  FILLER PIC X(55) VALUE
               '00177PATIENT HAS NOT MET THE REQUIRED WAITING PERIOD'.
           05  FILLER PIC X(55) VALUE
               '00178PATIENT WITHIN PLAN PRE-EXISTING CONDITION WAIT'.
           05  FILLER PIC X(55) VALUE
               '00179SERVICE NOT PAYABLE WITH DIAGNOSIS CODE        '.
           05  FILLER PIC X(55) VALUE
               '00181PROCEDURE CODE WAS INVALID ON THE DATE OF SVC  '.
           05  FILLER PIC X(55) VALUE
               '00183THE DATE OF SERVICE IS IN THE FUTURE           '.
           05  FILLER PIC X(55) VALUE
               '00184THE FROM DATE IS AFTER THE THROUGH DATE        '.
           05  FILLER PIC X(55) VALUE
               '00185THE RENDERING PROVIDER IS NOT ELIGIBLE TO PERFO'.
           05  FILLER PIC X(55) VALUE
               '00187CONSUMER DIRECTED / FLEXIBLE SPENDING ACCOUNT  '.
           05  FILLER PIC X(55) VALUE
               '00189NOT COVERED - NOT DEEMED A MEDICAL NECESSITY   '.
           05  FILLER PIC X(55) VALUE
               '00190PAYMENT ADJ - INCOMPLETE/INVALID BILLING PROV  '.
           05  FILLER PIC X(55) VALUE
               '00192NON-STANDARD ADJUSTMENT CODE FROM ANOTHER PAYER'.
           05  FILLER PIC X(55) VALUE
               '00193ORIGINAL PAYMENT DECISION IS BEING MAINTAINED  '.
           05  FILLER PIC X(55) VALUE
               '00194ANESTHESIA UNITS EXCEED MAXIMUM ALLOWABLE      '.
           05  FILLER PIC X(55) VALUE
               '00197PRECERTIFICATION/AUTHORIZATION/NOTIFICATION REQD'.
           05  FILLER PIC X(55) VALUE
               '00198PAYMENT ADJUSTED - NO PROOF OF PRECERT/REFERRAL'.
           05  FILLER PIC X(55) VALUE
               '00199REVENUE CODE AND PROCEDURE CODE DO NOT MATCH   '.
           05  FILLER PIC X(55) VALUE
               '00200EXPENSES INCURRED DURING LAPSE IN COVERAGE     '.
           05  FILLER PIC X(55) VALUE
               '00201PRIMARY PAYER AMOUNT LESS THAN EXPECTED        '.
           05  FILLER PIC X(55) VALUE
               '00202NON-COVERED PERSONAL COMFORT OR CONVENIENCE ITE'.
           05  FILLER PIC X(55) VALUE
               '00204SVC/EQUIP/DRUG NOT COVERED UNDER PATIENTS PLAN '.
           05  FILLER PIC X(55) VALUE
               '00209PAYMENT ADJUSTED - PER DIEM RATE               '.
           05  FILLER PIC X(55) VALUE
               '00210PAYMENT ADJUSTED - STOP LOSS DEDUCTIBLE APPLIES'.
           05  FILLER PIC X(55) VALUE
               '00219PAYMENT DENIED - MAXIMUM BENEFIT HAS BEEN MET  '.
           05  FILLER PIC X(55) VALUE
               '00222THE RENDERING PROVIDER IS NOT ELIGIBLE/LICENSED '.
           05  FILLER PIC X(55) VALUE
               '00223ADMINISTRATIVE SURCHARGE ADJUSTMENT             '.
           05  FILLER PIC X(55) VALUE
               '00226INFORMATION REQUESTED NOT FURNISHED CANNOT ADJ  '.
           05  FILLER PIC X(55) VALUE
               '00227INFORMATION DOES NOT SUBSTANTIATE MED NECESSITY '.
           05  FILLER PIC X(55) VALUE
               '00228DENTURE/REMOVABLE PARTIAL DENTURE PRIOR SERVICE'.
           05  FILLER PIC X(55) VALUE
               '00229PARTIAL HOSPITALIZATION - TOO FEW SERVICES     '.
           05  FILLER PIC X(55) VALUE
               '00231MUTUALLY EXCLUSIVE PROCEDURE                   '.
           05  FILLER PIC X(55) VALUE
               '00233NOT THE PATIENTS PRIMARY PLAN - USE OTHER PAYER '.
           05  FILLER PIC X(55) VALUE
               '00234THIS PROCEDURE IS NOT PAID SEPARATELY          '.
           05  FILLER PIC X(55) VALUE
               '00235SERVICE NOT COVERED - SALES TAX                '.
           05  FILLER PIC X(55) VALUE
               '00236THIS PROCEDURE/SVC NOT PAYABLE UNDER OUR PLAN  '.
           05  FILLER PIC X(55) VALUE
               '00237LEGISLATIVELY REQUIRED ADJUSTMENT               '.
           05  FILLER PIC X(55) VALUE
               '00238BENEFIT MAXIMUM FOR SERVICES HAS BEEN REACHED  '.
           05  FILLER PIC X(55) VALUE
               '00239CHARGE ADJUSTED PER BUNDLED/UNBUNDLED POLICY   '.
           05  FILLER PIC X(55) VALUE
               '00240CHARGES FOR NEW PATIENT SERVICES NOT APPLICABLE'.
           05  FILLER PIC X(55) VALUE
               '00242SERVICES NOT PROVIDED BY NETWORK/PRIMARY CARE  '.
           05  FILLER PIC X(55) VALUE
               '00243SVC NOT AUTH BY NETWORK/PRIMARY CARE PROVIDER  '.
           05  FILLER PIC X(55) VALUE
               '00245THE PROVIDER DOES NOT MEET SPECIALTY CREDENTIAL'.
           05  FILLER PIC X(55) VALUE
               '00246THIS SERVICE WAS NOT PRESCRIBED BY A PHYSICIAN '.
           05  FILLER PIC X(55) VALUE
               '00247PAYMENT ADJUSTED BASED ON ALLOWANCE GUIDELINES '.
           05  FILLER PIC X(55) VALUE
               '00250ADJUSTMENT FOR A CLAIM/SVC AFFECTED BY MEDICAID'.
           05  FILLER PIC X(55) VALUE
               '00251THE SERVICE WAS SUBMITTED FOR WRONG INSURANCE  '.
           05  FILLER PIC X(55) VALUE
               '00252SERVICES PROVIDED AS PART OF GFE - NSA PROTECT'.

       01  WS-CARC-TABLE-REDEF REDEFINES WS-CARC-TABLE.
           05  WS-CARC-ENTRY              OCCURS 120 TIMES.
               10  WS-CARC-CODE           PIC X(05).
               10  WS-CARC-DESCRIPTION    PIC X(50).

       01  WS-CARC-INDEX                  PIC 9(03).
       01  WS-CARC-LOOKUP-CODE            PIC X(05).
       01  WS-CARC-LOOKUP-DESC            PIC X(50).
       01  WS-CARC-FOUND-FLAG             PIC X(01).
           88  WS-CARC-FOUND              VALUE 'Y'.
           88  WS-CARC-NOT-FOUND          VALUE 'N'.

      *--------------------------------------------------------------*
      * RARC (REMITTANCE ADVICE REMARK CODE) TABLE - 60 ENTRIES
      *--------------------------------------------------------------*
       01  WS-RARC-TABLE-SIZE             PIC 9(03) VALUE 60.
       01  WS-RARC-TABLE.
           05  FILLER PIC X(55) VALUE
               'N0001ALERT: YOU MAY APPEAL THIS DECISION           '.
           05  FILLER PIC X(55) VALUE
               'N0002THIS CLAIM HAS BEEN FORWARDED TO OTHER PAYER  '.
           05  FILLER PIC X(55) VALUE
               'N0003MISSING/INCOMPLETE/INVALID CLINICAL INFORMATIO'.
           05  FILLER PIC X(55) VALUE
               'N0004RECORDS SHOW YOU WERE NOT AN ELIGIBLE PROVIDER'.
           05  FILLER PIC X(55) VALUE
               'N0005ALERT: ALTHOUGH THIS WAS PAID REVIEW FOR ACCUR'.
           05  FILLER PIC X(55) VALUE
               'N0007ALERT: REPORT ZERO PAYMENT ON LINE ITEM IF CMS'.
           05  FILLER PIC X(55) VALUE
               'N0016ALERT: SEE OUR WEBSITE OR CONTACT OFFICE       '.
           05  FILLER PIC X(55) VALUE
               'N0019CLAIM ADJUDICATED IN ACCORDANCE WITH A CAPITAT'.
           05  FILLER PIC X(55) VALUE
               'N0020ALERT: UNABLE TO IDENTIFY THE EOB CROSSOVER   '.
           05  FILLER PIC X(55) VALUE
               'N0021ALERT: THIS PATIENT IS COVERED BY INSURANCE   '.
           05  FILLER PIC X(55) VALUE
               'N0024MISSING/INCOMPLETE GENERIC EQUIVALENT/GENERIC A'.
           05  FILLER PIC X(55) VALUE
               'N0028ALERT: PATIENT RESPONSIBILITY IS NOT EXPECTED  '.
           05  FILLER PIC X(55) VALUE
               'N0030PATIENT INELIGIBLE BASED ON DIAGNOSIS           '.
           05  FILLER PIC X(55) VALUE
               'N0032ALERT: AWAITING RESPONSE FROM PATIENT          '.
           05  FILLER PIC X(55) VALUE
               'N0050MISSING/INCOMPLETE/INVALID ICD DIAGNOSIS CODE  '.
           05  FILLER PIC X(55) VALUE
               'N0053ALERT: NEW GUIDELINES FOR THIS SVC EFFECTIVE   '.
           05  FILLER PIC X(55) VALUE
               'N0056ALERT: ADDITIONAL DOCUMENTATION NEEDED          '.
           05  FILLER PIC X(55) VALUE
               'N0095PLAN PAID AMOUNT ADJUSTED BECAUSE OF NETWORK   '.
           05  FILLER PIC X(55) VALUE
               'N0115ALERT: PAYMENT IS FOR SERVICES PROVIDED UNDER  '.
           05  FILLER PIC X(55) VALUE
               'N0130CONSULT YOUR BENEFITS INFORMATION FOR COVERAGE '.
           05  FILLER PIC X(55) VALUE
               'N0167DIAGNOSIS IS NOT COVERED SVC TYPE/PROVIDER TYPE'.
           05  FILLER PIC X(55) VALUE
               'N0170PAYMENT DENIED/REDUCED - WRONG SPECIALTY       '.
           05  FILLER PIC X(55) VALUE
               'N0286ALERT: THIS SVC MAY NOT BE PAYABLE PER MANAGED'.
           05  FILLER PIC X(55) VALUE
               'N0341ALERT: PATIENT ELIGIBLE FOR HARDSHIP EXEMPTION'.
           05  FILLER PIC X(55) VALUE
               'N0356CERT OF MEDICAL NECESSITY NOT VALID FOR DATES  '.
           05  FILLER PIC X(55) VALUE
               'N0381ALERT: PRIOR AUTH/PRECERT NOT OBTAINED         '.
           05  FILLER PIC X(55) VALUE
               'N0384CLAIM INCLUDED IN A DEMONSTRATION/PILOT PROGRA'.
           05  FILLER PIC X(55) VALUE
               'N0386YOU HAVE NOT BILLED THE CORRECT INSURANCE PLAN '.
           05  FILLER PIC X(55) VALUE
               'N0390MISSING/INVALID PLACE OF SERVICE               '.
           05  FILLER PIC X(55) VALUE
               'N0400ALERT: DENTAL SERVICES - NOT SUBJECT TO DEDUCT'.
           05  FILLER PIC X(55) VALUE
               'N0430PAYMENT ADJUSTED - SEQUESTRATION                '.
           05  FILLER PIC X(55) VALUE
               'N0438ALERT: NO APPEAL RIGHTS - INFO ONLY            '.
           05  FILLER PIC X(55) VALUE
               'N0442ALERT: MEDICARE HAS APPROVED THE DEVICE        '.
           05  FILLER PIC X(55) VALUE
               'N0479ALERT: PAYMENT WAS ADJUSTED PER FEE SCHEDULE  '.
           05  FILLER PIC X(55) VALUE
               'N0488ALERT: THIS SVC IS SUBJECT TO COST SHARING    '.
           05  FILLER PIC X(55) VALUE
               'N0491ALERT: PAYMENT INCLUDES INTEREST               '.
           05  FILLER PIC X(55) VALUE
               'N0517PAYMENT INCLUDES PROMPT PAY DISCOUNT            '.
           05  FILLER PIC X(55) VALUE
               'N0522DUPLICATE OF A SERVICE ALREADY ADJUDICATED     '.
           05  FILLER PIC X(55) VALUE
               'N0527NOT COVERED UNDER CURRENT BENEFIT PLAN          '.
           05  FILLER PIC X(55) VALUE
               'N0540ADJUDICATED USING CURRENT PROCEDURAL TERMINOLOG'.
           05  FILLER PIC X(55) VALUE
               'N0569ALERT: CLAIM MAY HAVE BEEN PROCESSED UNDER COVI'.
           05  FILLER PIC X(55) VALUE
               'N0620ALERT: THIS SERVICE WAS PROCESSED AS TELEHEALTH'.
           05  FILLER PIC X(55) VALUE
               'N0632ALERT: NO SURPRISES ACT PROTECTION APPLIED     '.
           05  FILLER PIC X(55) VALUE
               'N0640ALERT: GOOD FAITH ESTIMATE RECONCILIATION      '.
           05  FILLER PIC X(55) VALUE
               'M0001X-RAY NOT TAKEN WITHIN PAST 12 MONTHS OR NO CH'.
           05  FILLER PIC X(55) VALUE
               'M0002NOT PAYABLE UNLESS SEPARATE PROCEDURE PERFORMED'.
           05  FILLER PIC X(55) VALUE
               'M0015SEPARATELY BILLED SERVICES BUNDLED INTO PROCED'.
           05  FILLER PIC X(55) VALUE
               'M0020MISSING DIAGNOSIS POINTER                      '.
           05  FILLER PIC X(55) VALUE
               'M0036CLAIM MUST INCLUDE AT LEAST ONE ICD-10 CODE    '.
           05  FILLER PIC X(55) VALUE
               'M0076MISSING/INCOMPLETE DIAGNOSIS FOR EACH BODY AREA'.
           05  FILLER PIC X(55) VALUE
               'M0079PROCEDURE CODE NOT CONSISTENT WITH MODIFIER    '.
           05  FILLER PIC X(55) VALUE
               'M0080NOT MEDICALLY NECESSARY BASED ON DOCUMENTATION '.
           05  FILLER PIC X(55) VALUE
               'M0086SVC DENIED - SERVICE NOT ORDERED/REFERRED      '.
           05  FILLER PIC X(55) VALUE
               'MA001ALERT: IF YOU DO NOT AGREE SUBMIT CLAIM REVIEW'.
           05  FILLER PIC X(55) VALUE
               'MA004SECONDARY PAYMENT CANNOT BE CALCULATED WITHOUT '.
           05  FILLER PIC X(55) VALUE
               'MA007ALERT: THE CLAIM INDICATES THE PATIENT PAID    '.
           05  FILLER PIC X(55) VALUE
               'MA013ALERT: YOU MAY BE SUBJECT TO LIABILITY FOR THIS'.
           05  FILLER PIC X(55) VALUE
               'MA018ALERT: CLAIM LACKS A DIAGNOSIS TO SUPPORT PROC'.
           05  FILLER PIC X(55) VALUE
               'MA028ALERT: RECEIPT OF THIS NOTICE INDICATES CROSSOV'.
           05  FILLER PIC X(55) VALUE
               'MA030MISSING/INVALID PROVIDER TAX ID IN CLAIM        '.
           05  FILLER PIC X(55) VALUE
               'MA061ALERT: PAYMENT BASED ON BUNDLED/PACKAGED SERVIC'.

       01  WS-RARC-TABLE-REDEF REDEFINES WS-RARC-TABLE.
           05  WS-RARC-ENTRY              OCCURS 60 TIMES.
               10  WS-RARC-CODE           PIC X(05).
               10  WS-RARC-DESCRIPTION    PIC X(50).

       01  WS-RARC-INDEX                  PIC 9(03).
       01  WS-RARC-LOOKUP-CODE            PIC X(05).
       01  WS-RARC-LOOKUP-DESC            PIC X(50).
       01  WS-RARC-FOUND-FLAG             PIC X(01).
           88  WS-RARC-FOUND              VALUE 'Y'.
           88  WS-RARC-NOT-FOUND          VALUE 'N'.

      *--------------------------------------------------------------*
      * GROUP CODE DESCRIPTIONS
      *--------------------------------------------------------------*
       01  WS-GROUP-CODE-TABLE.
           05  FILLER PIC X(52) VALUE
               'COCONTR OBLIGATION - PROV WRITE-OFF (NOT PT RESP) '.
           05  FILLER PIC X(52) VALUE
               'PRPATIENT RESPONSIBILITY - DEDUCT/COPAY/COINS     '.
           05  FILLER PIC X(52) VALUE
               'PIPAYER INITIATED REDUCTION - NOT PROVIDER/PATIENT'.
           05  FILLER PIC X(52) VALUE
               'OAOTHER ADJUSTMENT - DOES NOT FIT OTHER CATEGORIES'.
           05  FILLER PIC X(52) VALUE
               'CRCORRECTION AND/OR REVERSAL OF PRIOR DECISION    '.

       01  WS-GROUP-TABLE-REDEF
           REDEFINES WS-GROUP-CODE-TABLE.
           05  WS-GROUP-ENTRY             OCCURS 5 TIMES.
               10  WS-GRP-CODE            PIC X(02).
               10  WS-GRP-DESCRIPTION     PIC X(50).

       01  WS-GRP-INDEX                   PIC 9(01).
       01  WS-GRP-LOOKUP-CODE             PIC X(02).
       01  WS-GRP-LOOKUP-DESC             PIC X(50).

      *--------------------------------------------------------------*
      * PAYMENT BUNDLING WORK AREAS
      *--------------------------------------------------------------*
       01  WS-BUNDLE-KEY.
           05  WS-BUNDLE-TAX-ID           PIC X(09).
           05  WS-BUNDLE-NPI              PIC X(10).
           05  WS-BUNDLE-PAY-TO-ADDR      PIC X(55).
           05  WS-BUNDLE-PAYER-ID         PIC X(10).

       01  WS-BUNDLE-ACCUMULATORS.
           05  WS-BUNDLE-GROSS-AMT        PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-WITHHOLD-AMT     PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-INTEREST-AMT     PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-PENALTY-AMT      PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-ADJUST-AMT       PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-NET-AMT          PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-CLAIM-COUNT      PIC 9(07)     VALUE ZEROS.
           05  WS-BUNDLE-LINE-COUNT       PIC 9(07)     VALUE ZEROS.
           05  WS-BUNDLE-PAID-COUNT       PIC 9(07)     VALUE ZEROS.
           05  WS-BUNDLE-DENIED-COUNT     PIC 9(07)     VALUE ZEROS.
           05  WS-BUNDLE-ZEROPAY-COUNT    PIC 9(07)     VALUE ZEROS.
           05  WS-BUNDLE-REVERSAL-COUNT   PIC 9(07)     VALUE ZEROS.
           05  WS-BUNDLE-CHARGE-TOTAL     PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-ALLOWED-TOTAL    PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-DEDUCT-TOTAL     PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-COPAY-TOTAL      PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-COINS-TOTAL      PIC S9(09)V99 VALUE ZEROS.
           05  WS-BUNDLE-COB-TOTAL        PIC S9(09)V99 VALUE ZEROS.

       01  WS-MIN-PAYMENT-THRESHOLD       PIC S9(03)V99 VALUE +0.50.
       01  WS-MAX-CHECK-AMOUNT            PIC S9(07)V99
                                          VALUE +999999.99.
       01  WS-SPLIT-REMAINING             PIC S9(09)V99 VALUE ZEROS.
       01  WS-SPLIT-CHECK-SEQ            PIC 9(03)     VALUE ZEROS.

      *--------------------------------------------------------------*
      * CHECK/EFT CONTROL FIELDS
      *--------------------------------------------------------------*
       01  WS-CHECK-FIELDS.
           05  WS-NEXT-CHECK-NUMBER       PIC 9(10)     VALUE ZEROS.
           05  WS-CURRENT-CHECK-NUM       PIC 9(10)     VALUE ZEROS.
           05  WS-CHECK-GROSS             PIC S9(09)V99 VALUE ZEROS.
           05  WS-CHECK-WITHHOLD          PIC S9(09)V99 VALUE ZEROS.
           05  WS-CHECK-ADJUST            PIC S9(09)V99 VALUE ZEROS.
           05  WS-CHECK-INTEREST          PIC S9(09)V99 VALUE ZEROS.
           05  WS-CHECK-NET               PIC S9(09)V99 VALUE ZEROS.
           05  WS-CHECK-CLAIM-COUNT       PIC 9(07)     VALUE ZEROS.
           05  WS-VOID-CHECK-NUMBER       PIC 9(10)     VALUE ZEROS.
           05  WS-REISSUE-CHECK-NUM       PIC 9(10)     VALUE ZEROS.

       01  WS-EFT-FIELDS.
           05  WS-EFT-TRACE-NUMBER        PIC 9(15)     VALUE ZEROS.
           05  WS-EFT-BATCH-NUMBER        PIC 9(07)     VALUE ZEROS.
           05  WS-EFT-ENTRY-COUNT         PIC 9(08)     VALUE ZEROS.
           05  WS-EFT-ENTRY-HASH          PIC 9(10)     VALUE ZEROS.
           05  WS-EFT-BATCH-DEBIT         PIC 9(12)     VALUE ZEROS.
           05  WS-EFT-BATCH-CREDIT        PIC 9(12)     VALUE ZEROS.
           05  WS-EFT-FILE-ENTRY-CNT      PIC 9(08)     VALUE ZEROS.
           05  WS-EFT-FILE-DEBIT-TOT      PIC 9(12)     VALUE ZEROS.
           05  WS-EFT-FILE-CREDIT-TOT     PIC 9(12)     VALUE ZEROS.
           05  WS-EFT-BLOCK-COUNT         PIC 9(06)     VALUE ZEROS.
           05  WS-EFT-BATCH-COUNT         PIC 9(06)     VALUE ZEROS.
           05  WS-EFT-FILE-HASH           PIC 9(10)     VALUE ZEROS.

      *--------------------------------------------------------------*
      * NACHA EFT RECORD FORMATS
      *--------------------------------------------------------------*
       01  WS-NACHA-FILE-HEADER.
           05  WS-NFH-RECORD-TYPE         PIC X(01) VALUE '1'.
           05  WS-NFH-PRIORITY-CODE       PIC X(02) VALUE '01'.
           05  WS-NFH-IMMED-DEST          PIC X(10).
           05  WS-NFH-IMMED-ORIGIN        PIC X(10).
           05  WS-NFH-FILE-DATE           PIC X(06).
           05  WS-NFH-FILE-TIME           PIC X(04).
           05  WS-NFH-FILE-ID-MOD         PIC X(01) VALUE 'A'.
           05  WS-NFH-RECORD-SIZE         PIC X(03) VALUE '094'.
           05  WS-NFH-BLOCK-FACTOR        PIC X(02) VALUE '10'.
           05  WS-NFH-FORMAT-CODE         PIC X(01) VALUE '1'.
           05  WS-NFH-DEST-NAME           PIC X(23).
           05  WS-NFH-ORIGIN-NAME         PIC X(23).
           05  WS-NFH-REFERENCE-CODE      PIC X(08).

       01  WS-NACHA-BATCH-HEADER.
           05  WS-NBH-RECORD-TYPE         PIC X(01) VALUE '5'.
           05  WS-NBH-SERVICE-CLASS        PIC X(03) VALUE '220'.
           05  WS-NBH-COMPANY-NAME        PIC X(16).
           05  WS-NBH-COMPANY-DISC        PIC X(20).
           05  WS-NBH-COMPANY-ID          PIC X(10).
           05  WS-NBH-ENTRY-CLASS         PIC X(03) VALUE 'CTX'.
           05  WS-NBH-ENTRY-DESC          PIC X(10).
           05  WS-NBH-DESC-DATE           PIC X(06).
           05  WS-NBH-EFFECTIVE-DATE      PIC X(06).
           05  WS-NBH-SETTLEMENT-DATE     PIC X(03) VALUE SPACES.
           05  WS-NBH-ORIGIN-STATUS       PIC X(01) VALUE '1'.
           05  WS-NBH-ORIGIN-DFI          PIC X(08).
           05  WS-NBH-BATCH-NUMBER        PIC 9(07).

       01  WS-NACHA-ENTRY-DETAIL.
           05  WS-NED-RECORD-TYPE         PIC X(01) VALUE '6'.
           05  WS-NED-TRANS-CODE          PIC X(02).
           05  WS-NED-RECV-DFI-ID         PIC X(08).
           05  WS-NED-CHECK-DIGIT         PIC X(01).
           05  WS-NED-DFI-ACCT-NUM        PIC X(17).
           05  WS-NED-AMOUNT              PIC 9(10).
           05  WS-NED-RECV-ID-NUM         PIC X(15).
           05  WS-NED-RECV-NAME           PIC X(22).
           05  WS-NED-DISC-DATA           PIC X(02) VALUE SPACES.
           05  WS-NED-ADDENDA-IND         PIC X(01) VALUE '1'.
           05  WS-NED-TRACE-NUMBER        PIC 9(15).

       01  WS-NACHA-ADDENDA.
           05  WS-NAD-RECORD-TYPE         PIC X(01) VALUE '7'.
           05  WS-NAD-ADDENDA-TYPE        PIC X(02) VALUE '05'.
           05  WS-NAD-PAYMENT-INFO        PIC X(80).
           05  WS-NAD-ADDENDA-SEQ         PIC 9(04).
           05  WS-NAD-ENTRY-SEQ           PIC 9(07).

       01  WS-NACHA-BATCH-CONTROL.
           05  WS-NBC-RECORD-TYPE         PIC X(01) VALUE '8'.
           05  WS-NBC-SERVICE-CLASS        PIC X(03) VALUE '220'.
           05  WS-NBC-ENTRY-COUNT         PIC 9(06).
           05  WS-NBC-ENTRY-HASH          PIC 9(10).
           05  WS-NBC-TOTAL-DEBIT         PIC 9(12).
           05  WS-NBC-TOTAL-CREDIT        PIC 9(12).
           05  WS-NBC-COMPANY-ID          PIC X(10).
           05  WS-NBC-MSG-AUTH-CODE       PIC X(19) VALUE SPACES.
           05  WS-NBC-RESERVED            PIC X(06) VALUE SPACES.
           05  WS-NBC-ORIGIN-DFI          PIC X(08).
           05  WS-NBC-BATCH-NUMBER        PIC 9(07).

       01  WS-NACHA-FILE-CONTROL.
           05  WS-NFC-RECORD-TYPE         PIC X(01) VALUE '9'.
           05  WS-NFC-BATCH-COUNT         PIC 9(06).
           05  WS-NFC-BLOCK-COUNT         PIC 9(06).
           05  WS-NFC-ENTRY-COUNT         PIC 9(08).
           05  WS-NFC-ENTRY-HASH          PIC 9(10).
           05  WS-NFC-TOTAL-DEBIT         PIC 9(12).
           05  WS-NFC-TOTAL-CREDIT        PIC 9(12).
           05  WS-NFC-RESERVED            PIC X(39) VALUE SPACES.

      *--------------------------------------------------------------*
      * PROVIDER BANK ROUTING TABLE (FROM DB)
      *--------------------------------------------------------------*
       01  WS-PROVIDER-BANK-INFO.
           05  WS-PROV-BANK-ROUTING       PIC X(09).
           05  WS-PROV-BANK-ACCT-NO       PIC X(17).
           05  WS-PROV-BANK-ACCT-TYPE     PIC X(02).
           05  WS-PROV-BANK-NAME          PIC X(35).
           05  WS-PROV-EFT-STATUS         PIC X(01).
               88  WS-PROV-EFT-ACTIVE     VALUE 'A'.
               88  WS-PROV-EFT-PRENOTE    VALUE 'P'.
               88  WS-PROV-EFT-INACTIVE   VALUE 'I'.
               88  WS-PROV-EFT-SUSPENDED  VALUE 'S'.
           05  WS-PROV-PRENOTE-DATE       PIC X(08).
           05  WS-PROV-EFT-EFF-DATE       PIC X(08).

      *--------------------------------------------------------------*
      * PAYER BANK ROUTING (SENDER) INFO
      *--------------------------------------------------------------*
       01  WS-PAYER-BANK-INFO.
           05  WS-PAYER-BANK-ROUTING      PIC X(09).
           05  WS-PAYER-BANK-ACCT-NO      PIC X(17).
           05  WS-PAYER-BANK-ACCT-TYPE    PIC X(02).
           05  WS-PAYER-BANK-NAME         PIC X(35).
           05  WS-PAYER-ORIGIN-CO-ID      PIC X(10).
           05  WS-PAYER-ORIGIN-NAME       PIC X(23).

      *--------------------------------------------------------------*
      * PAYER CONFIGURATION (FROM DB)
      *--------------------------------------------------------------*
       01  WS-PAYER-CONFIG.
           05  WS-PC-PAYER-ID             PIC X(10).
           05  WS-PC-PAYER-NAME           PIC X(60).
           05  WS-PC-PAYER-ADDR-1         PIC X(55).
           05  WS-PC-PAYER-ADDR-2         PIC X(55).
           05  WS-PC-PAYER-CITY           PIC X(30).
           05  WS-PC-PAYER-STATE          PIC X(02).
           05  WS-PC-PAYER-ZIP            PIC X(09).
           05  WS-PC-PAYER-PHONE          PIC X(10).
           05  WS-PC-PAYER-FEIN           PIC X(09).
           05  WS-PC-ISA-SENDER-QUAL      PIC X(02).
           05  WS-PC-ISA-SENDER-ID        PIC X(15).
           05  WS-PC-ISA-RECEIVER-QUAL    PIC X(02).
           05  WS-PC-ISA-RECEIVER-ID      PIC X(15).
           05  WS-PC-GS-SENDER-CODE       PIC X(15).
           05  WS-PC-GS-RECEIVER-CODE     PIC X(15).
           05  WS-PC-USAGE-INDICATOR      PIC X(01).
           05  WS-PC-ACK-REQUESTED        PIC X(01).
           05  WS-PC-APPEAL-DAYS          PIC 9(03).
           05  WS-PC-APPEAL-PHONE         PIC X(10).
           05  WS-PC-APPEAL-ADDRESS       PIC X(120).
           05  WS-PC-WEBSITE              PIC X(60).

      *--------------------------------------------------------------*
      * PROVIDER PAYMENT SUMMARY ACCUMULATORS
      * BY TAX ID / BY NPI
      *--------------------------------------------------------------*
       01  WS-PROV-SUMMARY-BY-TAXID.
           05  WS-PST-TAX-ID              PIC X(09).
           05  WS-PST-PROVIDER-NAME       PIC X(60).
           05  WS-PST-TOTAL-CLAIMS        PIC 9(07)     VALUE ZEROS.
           05  WS-PST-PAID-CLAIMS         PIC 9(07)     VALUE ZEROS.
           05  WS-PST-DENIED-CLAIMS       PIC 9(07)     VALUE ZEROS.
           05  WS-PST-TOTAL-CHARGED       PIC S9(11)V99 VALUE ZEROS.
           05  WS-PST-TOTAL-ALLOWED       PIC S9(11)V99 VALUE ZEROS.
           05  WS-PST-TOTAL-PAID          PIC S9(11)V99 VALUE ZEROS.
           05  WS-PST-TOTAL-WITHHOLD      PIC S9(09)V99 VALUE ZEROS.
           05  WS-PST-TOTAL-INTEREST      PIC S9(09)V99 VALUE ZEROS.
           05  WS-PST-NET-PAYMENT         PIC S9(11)V99 VALUE ZEROS.

       01  WS-PROV-SUMMARY-BY-NPI.
           05  WS-PSN-NPI                  PIC X(10).
           05  WS-PSN-PROVIDER-NAME        PIC X(60).
           05  WS-PSN-TOTAL-CLAIMS         PIC 9(07)     VALUE ZEROS.
           05  WS-PSN-PAID-CLAIMS          PIC 9(07)     VALUE ZEROS.
           05  WS-PSN-DENIED-CLAIMS        PIC 9(07)     VALUE ZEROS.
           05  WS-PSN-TOTAL-CHARGED        PIC S9(11)V99 VALUE ZEROS.
           05  WS-PSN-TOTAL-ALLOWED        PIC S9(11)V99 VALUE ZEROS.
           05  WS-PSN-TOTAL-PAID           PIC S9(11)V99 VALUE ZEROS.
           05  WS-PSN-NET-PAYMENT          PIC S9(11)V99 VALUE ZEROS.

      *--------------------------------------------------------------*
      * PATIENT EOB WORK AREAS
      *--------------------------------------------------------------*
       01  WS-EOB-PATIENT-INFO.
           05  WS-EOB-PAT-NAME            PIC X(61).
           05  WS-EOB-PAT-ADDR-1          PIC X(55).
           05  WS-EOB-PAT-ADDR-2          PIC X(55).
           05  WS-EOB-PAT-CITY            PIC X(30).
           05  WS-EOB-PAT-STATE           PIC X(02).
           05  WS-EOB-PAT-ZIP             PIC X(09).
           05  WS-EOB-PAT-MEMBER-ID       PIC X(20).
           05  WS-EOB-PAT-GROUP-NO        PIC X(15).
           05  WS-EOB-PAT-PLAN-NAME       PIC X(50).

       01  WS-EOB-BENEFIT-SUMMARY.
           05  WS-EOB-IND-DEDUCT-USED     PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-IND-DEDUCT-MAX      PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-IND-DEDUCT-REM      PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-IND-DEDUCT-MET      PIC X(01)     VALUE 'N'.
           05  WS-EOB-FAM-DEDUCT-USED     PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-FAM-DEDUCT-MAX      PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-FAM-DEDUCT-REM      PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-FAM-DEDUCT-MET      PIC X(01)     VALUE 'N'.
           05  WS-EOB-IND-OOP-USED        PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-IND-OOP-MAX         PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-IND-OOP-REM         PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-FAM-OOP-USED        PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-FAM-OOP-MAX         PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-FAM-OOP-REM         PIC S9(07)V99 VALUE ZEROS.

       01  WS-EOB-CLAIM-TOTALS.
           05  WS-EOB-TOTAL-CHARGED       PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-TOTAL-DISCOUNT      PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-TOTAL-PLAN-PAID     PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-TOTAL-YOU-OWE       PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-TOTAL-DEDUCTIBLE    PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-TOTAL-COPAY         PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-TOTAL-COINSURANCE   PIC S9(07)V99 VALUE ZEROS.
           05  WS-EOB-CLAIM-COUNT         PIC 9(05)     VALUE ZEROS.

      *--------------------------------------------------------------*
      * PATIENT RESPONSIBILITY LETTER FIELDS
      *--------------------------------------------------------------*
       01  WS-PAT-RESP-LETTER.
           05  WS-PRL-TOTAL-OWED          PIC S9(07)V99 VALUE ZEROS.
           05  WS-PRL-DEDUCT-PORTION      PIC S9(07)V99 VALUE ZEROS.
           05  WS-PRL-COPAY-PORTION       PIC S9(07)V99 VALUE ZEROS.
           05  WS-PRL-COINS-PORTION       PIC S9(07)V99 VALUE ZEROS.
           05  WS-PRL-NONCOV-PORTION      PIC S9(07)V99 VALUE ZEROS.
           05  WS-PRL-REASON-TEXT         PIC X(200).
           05  WS-PRL-APPEAL-DEADLINE     PIC X(08).
           05  WS-PRL-STATE-COMMISH-INFO  PIC X(120).

      *--------------------------------------------------------------*
      * STATE-SPECIFIC EOB CONTENT REQUIREMENTS
      *--------------------------------------------------------------*
       01  WS-STATE-EOB-CONFIG.
           05  WS-STATE-CODE              PIC X(02).
           05  WS-STATE-BAL-BILL-NOTICE   PIC X(01).
               88  WS-STATE-REQ-BAL-BILL  VALUE 'Y'.
           05  WS-STATE-NETWORK-NOTICE    PIC X(01).
               88  WS-STATE-REQ-NET-NOTICE VALUE 'Y'.
           05  WS-STATE-APPEAL-TEXT-REQ   PIC X(01).
               88  WS-STATE-REQ-APPEAL    VALUE 'Y'.
           05  WS-STATE-COMMISH-REQ       PIC X(01).
               88  WS-STATE-REQ-COMMISH   VALUE 'Y'.
           05  WS-STATE-GLOSSARY-REQ      PIC X(01).
               88  WS-STATE-REQ-GLOSSARY  VALUE 'Y'.
           05  WS-STATE-LANGUAGE-REQ      PIC X(01).
               88  WS-STATE-REQ-LANGUAGE  VALUE 'Y'.
           05  WS-STATE-SURPRISE-BILL     PIC X(01).
               88  WS-STATE-REQ-SURPRISE  VALUE 'Y'.
           05  WS-STATE-GFE-NOTICE        PIC X(01).
               88  WS-STATE-REQ-GFE       VALUE 'Y'.
           05  WS-STATE-COMMISH-NAME      PIC X(50).
           05  WS-STATE-COMMISH-PHONE     PIC X(15).
           05  WS-STATE-COMMISH-WEB       PIC X(60).
           05  WS-STATE-LANG-NOTICE-TEXT  PIC X(200).

      *--------------------------------------------------------------*
      * PLB (PROVIDER LEVEL BALANCE) WORK AREAS
      *--------------------------------------------------------------*
       01  WS-PLB-ADJUSTMENTS.
           05  WS-PLB-WITHHOLD-AMT        PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-INTEREST-AMT        PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-LATE-PENALTY        PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-OVERPAY-RECOUP      PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-CAPITATION-OFFSET   PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-INCENTIVE-AMT       PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-FWD-BALANCE         PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-BONUS-AMT           PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-SEQUESTER-AMT       PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-NET-ADJUSTMENT      PIC S9(09)V99 VALUE ZEROS.
           05  WS-PLB-ENTRY-COUNT         PIC 9(03)     VALUE ZEROS.

      *--------------------------------------------------------------*
      * NEGATIVE BALANCE / OVERPAYMENT WORK AREAS
      *--------------------------------------------------------------*
       01  WS-NEGATIVE-BALANCE-FIELDS.
           05  WS-NB-PROVIDER-TAX-ID      PIC X(09).
           05  WS-NB-OUTSTANDING-AMT      PIC S9(09)V99 VALUE ZEROS.
           05  WS-NB-OFFSET-AMT           PIC S9(09)V99 VALUE ZEROS.
           05  WS-NB-REMAINING-AMT        PIC S9(09)V99 VALUE ZEROS.
           05  WS-NB-AR-AMOUNT            PIC S9(09)V99 VALUE ZEROS.
           05  WS-NB-DUNNING-FLAG         PIC X(01)     VALUE 'N'.
           05  WS-NB-PAYMENT-PLAN-FLAG    PIC X(01)     VALUE 'N'.
           05  WS-NB-STATUTE-DATE         PIC X(08).
           05  WS-NB-ORIGINAL-OVERPAY-DT  PIC X(08).
           05  WS-NB-DAYS-OUTSTANDING     PIC 9(05)     VALUE ZEROS.
           05  WS-NB-STATUTE-LIMIT-DAYS   PIC 9(05)     VALUE 1095.

      *--------------------------------------------------------------*
      * REPORT FORMATTING - PROVIDER REMITTANCE
      *--------------------------------------------------------------*
       01  WS-REMIT-PAGE-CTR              PIC 9(05)     VALUE ZEROS.
       01  WS-REMIT-LINE-CTR              PIC 9(03)     VALUE ZEROS.
       01  WS-REMIT-MAX-LINES             PIC 9(03)     VALUE 55.
       01  WS-EOB-PAGE-CTR                PIC 9(05)     VALUE ZEROS.
       01  WS-EOB-LINE-CTR                PIC 9(03)     VALUE ZEROS.
       01  WS-EOB-MAX-LINES               PIC 9(03)     VALUE 50.
       01  WS-SUMMARY-PAGE-CTR            PIC 9(05)     VALUE ZEROS.
       01  WS-SUMMARY-LINE-CTR            PIC 9(03)     VALUE ZEROS.
       01  WS-RECON-PAGE-CTR              PIC 9(05)     VALUE ZEROS.
       01  WS-RECON-LINE-CTR              PIC 9(03)     VALUE ZEROS.

       01  WS-REMIT-HEADER-1.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(35)
               VALUE 'CONSOLIDATED HEALTHCARE SYSTEMS INC'.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  FILLER                      PIC X(24)
               VALUE 'PROVIDER REMITTANCE ADV'.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  FILLER                      PIC X(06) VALUE 'PAGE: '.
           05  WS-RH1-PAGE-NUM            PIC Z(4)9.
           05  FILLER                      PIC X(21) VALUE SPACES.

       01  WS-REMIT-HEADER-2.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(08) VALUE 'PAYER: '.
           05  WS-RH2-PAYER-NAME          PIC X(40).
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(13)
               VALUE 'CHECK/EFT #: '.
           05  WS-RH2-CHECK-NUM           PIC X(15).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE 'PAY DATE: '.
           05  WS-RH2-PAY-DATE            PIC X(10).
           05  FILLER                      PIC X(20) VALUE SPACES.

       01  WS-REMIT-HEADER-3.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12)
               VALUE 'PAYEE NPI: '.
           05  WS-RH3-NPI                  PIC X(10).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE 'TAX ID: '.
           05  WS-RH3-TAX-ID              PIC X(09).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(08) VALUE 'PAYEE: '.
           05  WS-RH3-PAYEE-NAME          PIC X(50).
           05  FILLER                      PIC X(22) VALUE SPACES.

       01  WS-REMIT-HEADER-4.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(131) VALUE ALL '-'.

       01  WS-REMIT-COL-HDR-1.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(20) VALUE
               'PATIENT NAME        '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(15) VALUE
               'CLAIM NUMBER   '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE
               'DOS FROM  '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE
               'DOS THRU  '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '      BILLED'.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '     ALLOWED'.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '      DEDUCT'.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '       COPAY'.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '      COINS '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '        PAID'.

       01  WS-REMIT-DETAIL-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-PATIENT-NAME         PIC X(20).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-CLAIM-NUMBER         PIC X(15).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-DOS-FROM             PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-DOS-THRU             PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-BILLED               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-ALLOWED              PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-DEDUCT               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-COPAY                PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-COINSURANCE          PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RD-PAID                 PIC Z(7)9.99-.

       01  WS-REMIT-SVC-DETAIL-LINE.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-RS-PROC-CODE            PIC X(05).
           05  WS-RS-MODIFIERS            PIC X(11).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-REV-CODE             PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-DOS-FROM             PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-DOS-THRU             PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-BILLED               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-ALLOWED              PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-DEDUCT               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-COPAY                PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-COINS                PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RS-PAID                 PIC Z(7)9.99-.

       01  WS-REMIT-ADJ-DETAIL-LINE.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(05) VALUE 'ADJ: '.
           05  WS-RA-GROUP-CODE           PIC X(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-RA-REASON-CODE          PIC X(05).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RA-AMOUNT               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RA-DESCRIPTION          PIC X(50).
           05  FILLER                      PIC X(49) VALUE SPACES.

       01  WS-REMIT-SUBTOTAL-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(47) VALUE ALL '-'.
           05  FILLER                      PIC X(15) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               'SUB-TOTALS: '.
           05  WS-RST-BILLED              PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RST-ALLOWED             PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RST-DEDUCT              PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RST-COPAY               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RST-COINS               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RST-PAID                PIC Z(7)9.99-.

       01  WS-REMIT-TOTAL-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(47) VALUE ALL '='.
           05  FILLER                      PIC X(11) VALUE SPACES.
           05  FILLER                      PIC X(16) VALUE
               'GRAND TOTALS:   '.
           05  WS-RT-BILLED               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RT-ALLOWED              PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RT-DEDUCT               PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RT-COPAY                PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RT-COINS                PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RT-PAID                 PIC Z(7)9.99-.

      *--------------------------------------------------------------*
      * REPORT FORMATTING - PATIENT EOB
      *--------------------------------------------------------------*
       01  WS-EOB-HEADER-1.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-EH1-PLAN-NAME           PIC X(50).
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  FILLER                      PIC X(30)
               VALUE 'EXPLANATION OF BENEFITS       '.
           05  FILLER                      PIC X(06) VALUE 'PAGE: '.
           05  WS-EH1-PAGE-NUM            PIC Z(4)9.
           05  FILLER                      PIC X(20) VALUE SPACES.

       01  WS-EOB-HEADER-2.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               'EOB DATE:   '.
           05  WS-EH2-EOB-DATE            PIC X(10).
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               'MEMBER ID:  '.
           05  WS-EH2-MEMBER-ID           PIC X(20).
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE
               'GROUP #:  '.
           05  WS-EH2-GROUP-NO            PIC X(15).
           05  FILLER                      PIC X(32) VALUE SPACES.

       01  WS-EOB-NOT-A-BILL.
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(55) VALUE
               '***  THIS IS NOT A BILL  ***  THIS IS NOT A BILL'.
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(47) VALUE
               '***  THIS IS NOT A BILL  ***'.
           05  FILLER                      PIC X(10) VALUE SPACES.

       01  WS-EOB-CLAIM-HDR.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(15) VALUE
               'PROVIDER       '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE
               'DATE OF   '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(20) VALUE
               'DESCRIPTION         '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '    PROVIDER'.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '    PLAN    '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '    PLAN    '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '   WHAT YOU '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(20) VALUE
               'WHY YOU OWE IT      '.
           05  FILLER                      PIC X(10) VALUE SPACES.

       01  WS-EOB-CLAIM-HDR-2.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(15) VALUE
               'NAME           '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE
               'SERVICE   '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(20) VALUE
               'OF SERVICE          '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '    CHARGED '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '   DISCOUNT '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '    PAID    '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(12) VALUE
               '      OWE   '.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  FILLER                      PIC X(10) VALUE SPACES.

       01  WS-EOB-CLAIM-DETAIL.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-PROVIDER-NAME       PIC X(15).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-DOS                 PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-SVC-DESC            PIC X(20).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-CHARGED             PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-DISCOUNT            PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-PLAN-PAID           PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-YOU-OWE             PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ECD-REASON              PIC X(20).
           05  FILLER                      PIC X(10) VALUE SPACES.

       01  WS-EOB-TOTAL-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(47) VALUE ALL '='.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(09) VALUE 'TOTALS: '.
           05  WS-ETL-CHARGED             PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ETL-DISCOUNT            PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ETL-PLAN-PAID           PIC Z(7)9.99-.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-ETL-YOU-OWE             PIC Z(7)9.99-.
           05  FILLER                      PIC X(30) VALUE SPACES.

      *--------------------------------------------------------------*
      * PROCEDURE CODE DESCRIPTION LOOKUP
      *--------------------------------------------------------------*
       01  WS-PROC-DESC-AREA.
           05  WS-PROC-LOOKUP-CODE        PIC X(05).
           05  WS-PROC-DESC-RESULT        PIC X(40).

      *--------------------------------------------------------------*
      * RECONCILIATION AND CONTROL TOTALS
      *--------------------------------------------------------------*
       01  WS-RECON-TOTALS.
           05  WS-RECON-TOTAL-CLAIMS      PIC 9(09)     VALUE ZEROS.
           05  WS-RECON-TOTAL-PAID-AMT    PIC S9(11)V99 VALUE ZEROS.
           05  WS-RECON-TOTAL-CHK-AMT     PIC S9(11)V99 VALUE ZEROS.
           05  WS-RECON-TOTAL-EFT-AMT     PIC S9(11)V99 VALUE ZEROS.
           05  WS-RECON-TOTAL-CHECKS      PIC 9(07)     VALUE ZEROS.
           05  WS-RECON-TOTAL-EFTS        PIC 9(07)     VALUE ZEROS.
           05  WS-RECON-ZERO-PAYS         PIC 9(07)     VALUE ZEROS.
           05  WS-RECON-DENIED-CNT        PIC 9(07)     VALUE ZEROS.
           05  WS-RECON-HASH-TOTAL        PIC S9(15)V99 VALUE ZEROS.
           05  WS-RECON-RECORD-COUNT      PIC 9(09)     VALUE ZEROS.
           05  WS-RECON-OUT-OF-BAL        PIC S9(11)V99 VALUE ZEROS.
           05  WS-RECON-IN-BALANCE        PIC X(01)     VALUE 'Y'.
               88  WS-RECON-BALANCED      VALUE 'Y'.
               88  WS-RECON-NOT-BALANCED  VALUE 'N'.

      *--------------------------------------------------------------*
      * PROGRAM STATISTICS
      *--------------------------------------------------------------*
       01  WS-PROGRAM-STATS.
           05  WS-STAT-RECORDS-READ       PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CLAIMS-PROCESSED   PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CLAIMS-PAID        PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CLAIMS-DENIED      PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CLAIMS-ZEROPAY     PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CLAIMS-REVERSED    PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CLAIMS-ADJUSTED    PIC 9(09)     VALUE ZEROS.
           05  WS-STAT-CHECKS-GENERATED   PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-EFTS-GENERATED     PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-835-TRANS-SETS     PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-EOBS-GENERATED     PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-REMITS-GENERATED   PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-TOTAL-DOLLARS      PIC S9(13)V99 VALUE ZEROS.
           05  WS-STAT-TOTAL-CHK-DOLLARS  PIC S9(13)V99 VALUE ZEROS.
           05  WS-STAT-TOTAL-EFT-DOLLARS  PIC S9(13)V99 VALUE ZEROS.
           05  WS-STAT-ERRORS             PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-WARNINGS           PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-NEG-BAL-OFFSETS    PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-SPLIT-CHECKS       PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-BELOW-MIN-DEFER    PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-VOID-REISSUES      PIC 9(07)     VALUE ZEROS.
           05  WS-STAT-PRENOTES           PIC 9(07)     VALUE ZEROS.

      *--------------------------------------------------------------*
      * STATISTICS BY PAYER / LINE OF BUSINESS
      *--------------------------------------------------------------*
       01  WS-STAT-BY-PAYER.
           05  WS-SBP-ENTRY               OCCURS 20 TIMES.
               10  WS-SBP-PAYER-ID        PIC X(10).
               10  WS-SBP-PAYER-NAME      PIC X(40).
               10  WS-SBP-CLAIM-COUNT     PIC 9(07)     VALUE ZEROS.
               10  WS-SBP-PAID-AMT        PIC S9(11)V99 VALUE ZEROS.
               10  WS-SBP-CHECK-COUNT     PIC 9(05)     VALUE ZEROS.
               10  WS-SBP-EFT-COUNT       PIC 9(05)     VALUE ZEROS.

       01  WS-STAT-PAYER-COUNT            PIC 9(02)     VALUE ZEROS.

       01  WS-STAT-BY-LOB.
           05  WS-SBL-ENTRY               OCCURS 7 TIMES.
               10  WS-SBL-LOB-CODE        PIC X(03).
               10  WS-SBL-LOB-NAME        PIC X(25).
               10  WS-SBL-CLAIM-COUNT     PIC 9(07)     VALUE ZEROS.
               10  WS-SBL-PAID-AMT        PIC S9(11)V99 VALUE ZEROS.

       01  WS-LOB-INDEX                   PIC 9(02)     VALUE ZEROS.

      *--------------------------------------------------------------*
      * HOLIDAY TABLE (LOADED FROM DB)
      *--------------------------------------------------------------*
       01  WS-HOLIDAY-TABLE.
           05  WS-HOLIDAY-COUNT           PIC 9(02) VALUE ZEROS.
           05  WS-HOLIDAY-ENTRY           OCCURS 20 TIMES.
               10  WS-HOLIDAY-DATE        PIC X(08).
               10  WS-HOLIDAY-DESC        PIC X(30).

       01  WS-HOLIDAY-INDEX               PIC 9(02).

      *--------------------------------------------------------------*
      * DB INTERACTION FIELDS
      *--------------------------------------------------------------*
       01  WS-DB-FIELDS.
           05  WS-DB-RETURN-CODE          PIC S9(09) COMP VALUE ZEROS.
           05  WS-DB-DEADLOCK-RETRIES     PIC 9(02) VALUE ZEROS.
           05  WS-DB-MAX-RETRIES          PIC 9(02) VALUE 03.
           05  WS-DB-DEADLOCK-CODE        PIC 9(05) VALUE 01205.
           05  WS-DB-ROWS-AFFECTED        PIC S9(09) COMP VALUE ZEROS.
           05  WS-DB-PARAGRAPH-NAME       PIC X(30).

      *--------------------------------------------------------------*
      * SQL HOST VARIABLES
      *--------------------------------------------------------------*
       01  WS-SQL-PAYER-ID                PIC X(10).
       01  WS-SQL-PROVIDER-TAX-ID         PIC X(09).
       01  WS-SQL-PROVIDER-NPI            PIC X(10).
       01  WS-SQL-MEMBER-ID               PIC X(20).
       01  WS-SQL-CLAIM-NUMBER            PIC X(20).
       01  WS-SQL-CHECK-NUMBER            PIC X(10).
       01  WS-SQL-NEXT-CHECK-NUM          PIC 9(10).
       01  WS-SQL-NEXT-ISA-NUM            PIC 9(09).
       01  WS-SQL-NEXT-GS-NUM             PIC 9(09).
       01  WS-SQL-NEXT-ST-NUM             PIC 9(09).
       01  WS-SQL-NEXT-EFT-TRACE          PIC 9(15).
       01  WS-SQL-PROC-CODE               PIC X(05).
       01  WS-SQL-PROC-DESC               PIC X(40).
       01  WS-SQL-STATE-CODE              PIC X(02).
       01  WS-SQL-PAYMENT-DATE            PIC X(08).
       01  WS-SQL-EFT-STATUS              PIC X(01).
       01  WS-SQL-PRENOTE-DATE            PIC X(08).
       01  WS-SQL-OVERPAY-AMT             PIC S9(09)V99.
       01  WS-SQL-OVERPAY-DATE            PIC X(08).
       01  WS-SQL-AR-AMOUNT               PIC S9(09)V99.
       01  WS-SQL-YTD-DEDUCT-IND         PIC S9(07)V99.
       01  WS-SQL-YTD-DEDUCT-FAM         PIC S9(07)V99.
       01  WS-SQL-YTD-OOP-IND            PIC S9(07)V99.
       01  WS-SQL-YTD-OOP-FAM            PIC S9(07)V99.
       01  WS-SQL-DEDUCT-MAX-IND         PIC S9(07)V99.
       01  WS-SQL-DEDUCT-MAX-FAM         PIC S9(07)V99.
       01  WS-SQL-OOP-MAX-IND            PIC S9(07)V99.
       01  WS-SQL-OOP-MAX-FAM            PIC S9(07)V99.
       01  WS-SQL-GROUP-NUMBER            PIC X(15).
       01  WS-SQL-PLAN-NAME               PIC X(50).

      *--------------------------------------------------------------*
      * MISCELLANEOUS WORK FIELDS
      *--------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-WORK-AMOUNT             PIC S9(11)V99 VALUE ZEROS.
           05  WS-WORK-AMOUNT-2           PIC S9(11)V99 VALUE ZEROS.
           05  WS-WORK-COUNT              PIC 9(09)     VALUE ZEROS.
           05  WS-WORK-STRING             PIC X(500)    VALUE SPACES.
           05  WS-WORK-LENGTH             PIC 9(03)     VALUE ZEROS.
           05  WS-WORK-INDEX              PIC 9(05)     VALUE ZEROS.
           05  WS-WORK-INDEX-2            PIC 9(05)     VALUE ZEROS.
           05  WS-CAS-GROUP-SAVE          PIC X(02)     VALUE SPACES.
           05  WS-CAS-ADJ-INDEX           PIC 9(02)     VALUE ZEROS.
           05  WS-EDIT-AMOUNT             PIC Z(9)9.99-.
           05  WS-EDIT-AMOUNT-2           PIC Z(7)9.99-.
           05  WS-EDIT-DATE               PIC X(10).
           05  WS-TRIM-NAME               PIC X(61).
           05  WS-PAYER-INDEX             PIC 9(02)     VALUE ZEROS.
           05  WS-ABEND-CODE              PIC X(04).
           05  WS-RETURN-CODE             PIC S9(04) COMP VALUE ZEROS.
           05  WS-CLAIM-ADJ-TABLE.
               10  WS-CAT-ENTRY           OCCURS 30 TIMES.
                   15  WS-CAT-GROUP-CODE  PIC X(02).
                   15  WS-CAT-REASON-CODE PIC X(05).
                   15  WS-CAT-AMOUNT      PIC S9(07)V99.
                   15  WS-CAT-QUANTITY    PIC 9(05)V99.
           05  WS-CAT-COUNT               PIC 9(02)     VALUE ZEROS.
           05  WS-REMARK-TABLE.
               10  WS-RMK-ENTRY           OCCURS 10 TIMES.
                   15  WS-RMK-CODE        PIC X(05).
                   15  WS-RMK-QUAL        PIC X(02).
           05  WS-RMK-COUNT               PIC 9(02)     VALUE ZEROS.
           05  WS-SVC-LINE-TABLE.
               10  WS-SLT-ENTRY           OCCURS 50 TIMES.
                   15  WS-SLT-LINE-NUM    PIC 9(02).
                   15  WS-SLT-PROC-CODE   PIC X(05).
                   15  WS-SLT-MOD1        PIC X(02).
                   15  WS-SLT-MOD2        PIC X(02).
                   15  WS-SLT-MOD3        PIC X(02).
                   15  WS-SLT-MOD4        PIC X(02).
                   15  WS-SLT-REV-CODE    PIC X(04).
                   15  WS-SLT-FROM-DT     PIC X(08).
                   15  WS-SLT-THRU-DT     PIC X(08).
                   15  WS-SLT-CHARGE      PIC S9(07)V99.
                   15  WS-SLT-ALLOWED     PIC S9(07)V99.
                   15  WS-SLT-PAID        PIC S9(07)V99.
                   15  WS-SLT-DEDUCT      PIC S9(07)V99.
                   15  WS-SLT-COPAY       PIC S9(07)V99.
                   15  WS-SLT-COINS       PIC S9(07)V99.
                   15  WS-SLT-COB         PIC S9(07)V99.
                   15  WS-SLT-PAT-RESP    PIC S9(07)V99.
                   15  WS-SLT-UNITS       PIC 9(05)V99.
                   15  WS-SLT-NDC         PIC X(11).
                   15  WS-SLT-POS         PIC X(02).
           05  WS-SLT-COUNT               PIC 9(02)     VALUE ZEROS.

      ****************************************************************
       PROCEDURE DIVISION.
      ****************************************************************

      *================================================================
       0000-MAIN-CONTROL.
      *================================================================
      *    MAIN DRIVER - ORCHESTRATES ENTIRE REMITTANCE PROCESS
      *================================================================
           PERFORM 0100-INITIALIZATION

           PERFORM 0200-PROCESS-CLAIMS
               UNTIL WS-END-OF-FILE

           PERFORM 0300-FINAL-PROVIDER-BREAK

           PERFORM 7000-RECONCILE-PAYMENT-RUN

           PERFORM 9000-TERMINATION

           STOP RUN
           .

      *================================================================
       0100-INITIALIZATION.
      *================================================================
      *    OPEN ALL FILES, INITIALIZE CONTROL FIELDS, LOAD REFERENCE
      *    DATA FROM DATABASE, GET CONTROL NUMBERS FOR 835/CHECKS/EFT
      *================================================================
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
                                         WS-CURRENT-TIME
                                         WS-GMT-OFFSET

           MOVE WS-CURR-YEAR              TO WS-WORK-YEAR
           MOVE WS-CURR-MONTH             TO WS-WORK-MONTH
           MOVE WS-CURR-DAY               TO WS-WORK-DAY

           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-YYYYMMDD

           STRING WS-CURR-YEAR(3:2) WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-YYMMDD

           STRING WS-CURR-HOUR WS-CURR-MINUTE
               DELIMITED BY SIZE INTO WS-HHMM

           STRING WS-CURR-HOUR WS-CURR-MINUTE WS-CURR-SECOND
               DELIMITED BY SIZE INTO WS-HHMMSS

           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-CCYYMMDD-DASH

           MOVE WS-YYYYMMDD                TO WS-EDI-DATE
           MOVE WS-HHMM                    TO WS-EDI-TIME

      *    BUILD AUDIT TIMESTAMP
           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
                  ' ' WS-CURR-HOUR ':' WS-CURR-MINUTE ':'
                  WS-CURR-SECOND '.' WS-CURR-HUNDREDTH '0000'
               DELIMITED BY SIZE INTO WS-TIMESTAMP

      *    OPEN ALL OUTPUT FILES
           OPEN INPUT ADJUD-CLAIMS-FILE
           IF WS-ADJCLMS-STATUS NOT = '00'
               MOVE 'ADJUD-CLAIMS-FILE OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT EDI-835-OUTPUT-FILE
           IF WS-EDI835-STATUS NOT = '00'
               MOVE 'EDI-835-OUTPUT OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT PROVIDER-REMIT-REPORT
           IF WS-PROVRMIT-STATUS NOT = '00'
               MOVE 'PROVIDER-REMIT-REPORT OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT PATIENT-EOB-OUTPUT
           IF WS-PATEOB-STATUS NOT = '00'
               MOVE 'PATIENT-EOB-OUTPUT OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT CHECK-REGISTER-FILE
           IF WS-CHKREG-STATUS NOT = '00'
               MOVE 'CHECK-REGISTER-FILE OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT EFT-OUTPUT-FILE
           IF WS-EFTOUT-STATUS NOT = '00'
               MOVE 'EFT-OUTPUT-FILE OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT POSITIVE-PAY-FILE
           IF WS-POSPAY-STATUS NOT = '00'
               MOVE 'POSITIVE-PAY-FILE OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT PAYMENT-SUMMARY-REPORT
           IF WS-PAYSUM-STATUS NOT = '00'
               MOVE 'PAYMENT-SUMMARY-REPORT OPEN FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 12 TO WS-RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT ERROR-FILE
           OPEN OUTPUT AUDIT-TRAIL-FILE
           OPEN OUTPUT RECON-REPORT-FILE

      *    INITIALIZE LOB STATISTICS TABLE
           MOVE 'COM' TO WS-SBL-LOB-CODE(1)
           MOVE 'COMMERCIAL'    TO WS-SBL-LOB-NAME(1)
           MOVE 'HMO' TO WS-SBL-LOB-CODE(2)
           MOVE 'HMO'           TO WS-SBL-LOB-NAME(2)
           MOVE 'PPO' TO WS-SBL-LOB-CODE(3)
           MOVE 'PPO'           TO WS-SBL-LOB-NAME(3)
           MOVE 'MAD' TO WS-SBL-LOB-CODE(4)
           MOVE 'MEDICARE ADVANTAGE' TO WS-SBL-LOB-NAME(4)
           MOVE 'MCD' TO WS-SBL-LOB-CODE(5)
           MOVE 'MEDICAID MANAGED'   TO WS-SBL-LOB-NAME(5)
           MOVE 'DNT' TO WS-SBL-LOB-CODE(6)
           MOVE 'DENTAL'        TO WS-SBL-LOB-NAME(6)
           MOVE 'VIS' TO WS-SBL-LOB-CODE(7)
           MOVE 'VISION'        TO WS-SBL-LOB-NAME(7)

      *    LOAD CONTROL NUMBERS FROM DATABASE
           PERFORM 0110-LOAD-CONTROL-NUMBERS
           PERFORM 0120-LOAD-PAYER-CONFIG
           PERFORM 0130-LOAD-HOLIDAY-TABLE
           PERFORM 0140-CALCULATE-PAYMENT-DATE

      *    WRITE AUDIT TRAIL START ENTRY
           MOVE WS-TIMESTAMP               TO AT-TIMESTAMP
           MOVE 'START'                     TO AT-ACTION
           MOVE 'PROGRAM'                   TO AT-ENTITY-TYPE
           MOVE 'HCREMIT'                   TO AT-ENTITY-KEY
           MOVE SPACES                      TO AT-OLD-VALUE
           MOVE WS-PROGRAM-VERSION          TO AT-NEW-VALUE
           MOVE 'BATCH'                     TO AT-USER-ID
           MOVE WS-PROGRAM-ID               TO AT-PROGRAM
           MOVE '0100-INITIALIZATION'       TO AT-PARAGRAPH
           MOVE 'REMITTANCE PROCESSING STARTED'
                                            TO AT-DESCRIPTION
           WRITE AUDIT-RECORD

      *    INITIALIZE 835 ENVELOPE - WRITE ISA AND GS SEGMENTS
           PERFORM 2100-BUILD-835-ENVELOPE
           PERFORM 2200-BUILD-835-FUNCTIONAL-GROUP

      *    WRITE EFT FILE HEADER IF ANY EFT PAYMENTS EXPECTED
           PERFORM 0150-WRITE-EFT-FILE-HEADER

      *    READ FIRST INPUT RECORD
           PERFORM 0500-READ-ADJUD-CLAIM
           .

      *================================================================
       0110-LOAD-CONTROL-NUMBERS.
      *================================================================
      *    RETRIEVE AND INCREMENT CONTROL NUMBERS FOR CHECKS, 835,
      *    AND EFT TRACE NUMBERS FROM THE CONTROL TABLE
      *================================================================
           MOVE '0110-LOAD-CONTROL-NUMBERS' TO WS-DB-PARAGRAPH-NAME
           MOVE ZEROS TO WS-DB-DEADLOCK-RETRIES

           PERFORM UNTIL WS-DB-DEADLOCK-RETRIES
                       > WS-DB-MAX-RETRIES

               EXEC SQL
                   SELECT NEXT_CHECK_NUMBER,
                          NEXT_ISA_CONTROL,
                          NEXT_GS_CONTROL,
                          NEXT_ST_CONTROL,
                          NEXT_EFT_TRACE
                   INTO   :WS-SQL-NEXT-CHECK-NUM,
                          :WS-SQL-NEXT-ISA-NUM,
                          :WS-SQL-NEXT-GS-NUM,
                          :WS-SQL-NEXT-ST-NUM,
                          :WS-SQL-NEXT-EFT-TRACE
                   FROM   PAYMENT_CONTROL
                   WHERE  CONTROL_TYPE = 'REMIT'
                   AND    ACTIVE_FLAG = 'Y'
               END-EXEC

               IF SQLCODE = 0
                   MOVE WS-SQL-NEXT-CHECK-NUM
                       TO WS-NEXT-CHECK-NUMBER
                   MOVE WS-SQL-NEXT-ISA-NUM
                       TO WS-ISA-CONTROL-NUM
                   MOVE WS-SQL-NEXT-GS-NUM
                       TO WS-GS-CONTROL-NUM
                   MOVE WS-SQL-NEXT-ST-NUM
                       TO WS-ST-CONTROL-NUM
                   MOVE WS-SQL-NEXT-EFT-TRACE
                       TO WS-EFT-TRACE-NUMBER

      *            RESERVE A BLOCK OF 100000 CHECK NUMBERS
                   EXEC SQL
                       UPDATE PAYMENT_CONTROL
                       SET    NEXT_CHECK_NUMBER =
                              NEXT_CHECK_NUMBER + 100000,
                              NEXT_ISA_CONTROL =
                              NEXT_ISA_CONTROL + 1,
                              NEXT_GS_CONTROL =
                              NEXT_GS_CONTROL + 1000,
                              NEXT_ST_CONTROL =
                              NEXT_ST_CONTROL + 1000,
                              NEXT_EFT_TRACE =
                              NEXT_EFT_TRACE + 100000,
                              LAST_RUN_DATE = GETDATE(),
                              LAST_RUN_PROGRAM = 'HCREMIT'
                       WHERE  CONTROL_TYPE = 'REMIT'
                       AND    ACTIVE_FLAG = 'Y'
                   END-EXEC

                   IF SQLCODE = 0
                       EXEC SQL COMMIT END-EXEC
                       EXIT PERFORM
                   ELSE
                       IF SQLCODE = WS-DB-DEADLOCK-CODE
                           EXEC SQL ROLLBACK END-EXEC
                           ADD 1 TO WS-DB-DEADLOCK-RETRIES
                       ELSE
                           PERFORM 8100-DATABASE-ERROR
                           EXIT PERFORM
                       END-IF
                   END-IF
               ELSE
                   IF SQLCODE = WS-DB-DEADLOCK-CODE
                       EXEC SQL ROLLBACK END-EXEC
                       ADD 1 TO WS-DB-DEADLOCK-RETRIES
                   ELSE
                       PERFORM 8100-DATABASE-ERROR
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM

           IF WS-DB-DEADLOCK-RETRIES > WS-DB-MAX-RETRIES
               MOVE 'DEADLOCK MAX RETRIES EXCEEDED - CONTROL NUMBERS'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               MOVE 16 TO WS-RETURN-CODE
               STOP RUN
           END-IF
           .

      *================================================================
       0120-LOAD-PAYER-CONFIG.
      *================================================================
      *    LOAD PAYER CONFIGURATION FOR 835 ENVELOPE AND EOB CONTENT
      *================================================================
           MOVE '0120-LOAD-PAYER-CONFIG' TO WS-DB-PARAGRAPH-NAME

           EXEC SQL
               SELECT PAYER_ID,
                      PAYER_NAME,
                      PAYER_ADDR_1,
                      PAYER_ADDR_2,
                      PAYER_CITY,
                      PAYER_STATE,
                      PAYER_ZIP,
                      PAYER_PHONE,
                      PAYER_FEIN,
                      ISA_SENDER_QUAL,
                      ISA_SENDER_ID,
                      ISA_RECEIVER_QUAL,
                      ISA_RECEIVER_ID,
                      GS_SENDER_CODE,
                      GS_RECEIVER_CODE,
                      USAGE_INDICATOR,
                      ACK_REQUESTED,
                      APPEAL_DAYS,
                      APPEAL_PHONE,
                      APPEAL_ADDRESS,
                      WEBSITE_URL
               INTO   :WS-PC-PAYER-ID,
                      :WS-PC-PAYER-NAME,
                      :WS-PC-PAYER-ADDR-1,
                      :WS-PC-PAYER-ADDR-2,
                      :WS-PC-PAYER-CITY,
                      :WS-PC-PAYER-STATE,
                      :WS-PC-PAYER-ZIP,
                      :WS-PC-PAYER-PHONE,
                      :WS-PC-PAYER-FEIN,
                      :WS-PC-ISA-SENDER-QUAL,
                      :WS-PC-ISA-SENDER-ID,
                      :WS-PC-ISA-RECEIVER-QUAL,
                      :WS-PC-ISA-RECEIVER-ID,
                      :WS-PC-GS-SENDER-CODE,
                      :WS-PC-GS-RECEIVER-CODE,
                      :WS-PC-USAGE-INDICATOR,
                      :WS-PC-ACK-REQUESTED,
                      :WS-PC-APPEAL-DAYS,
                      :WS-PC-APPEAL-PHONE,
                      :WS-PC-APPEAL-ADDRESS,
                      :WS-PC-WEBSITE
               FROM   PAYER_CONFIG
               WHERE  ACTIVE_FLAG = 'Y'
               AND    CONFIG_TYPE = 'REMIT835'
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'PAYER CONFIG LOAD FAILED' TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               PERFORM 8100-DATABASE-ERROR
           END-IF
           .

      *================================================================
       0130-LOAD-HOLIDAY-TABLE.
      *================================================================
      *    LOAD FEDERAL HOLIDAYS FOR NEXT 30 DAYS TO DETERMINE
      *    EFT EFFECTIVE DATES (CANNOT BE WEEKEND OR HOLIDAY)
      *================================================================
           MOVE '0130-LOAD-HOLIDAY-TABLE' TO WS-DB-PARAGRAPH-NAME
           MOVE ZEROS TO WS-HOLIDAY-COUNT

           EXEC SQL
               DECLARE HOLIDAY_CURSOR CURSOR FOR
               SELECT HOLIDAY_DATE,
                      HOLIDAY_DESC
               FROM   HOLIDAY_CALENDAR
               WHERE  HOLIDAY_DATE >= :WS-SQL-PAYMENT-DATE
               AND    HOLIDAY_DATE <=
                      DATEADD(DAY, 30, :WS-SQL-PAYMENT-DATE)
               ORDER BY HOLIDAY_DATE
           END-EXEC

           EXEC SQL OPEN HOLIDAY_CURSOR END-EXEC

           PERFORM UNTIL SQLCODE NOT = 0
                      OR WS-HOLIDAY-COUNT >= 20
               EXEC SQL
                   FETCH HOLIDAY_CURSOR
                   INTO  :WS-HOLIDAY-DATE(WS-HOLIDAY-COUNT + 1),
                         :WS-HOLIDAY-DESC(WS-HOLIDAY-COUNT + 1)
               END-EXEC
               IF SQLCODE = 0
                   ADD 1 TO WS-HOLIDAY-COUNT
               END-IF
           END-PERFORM

           EXEC SQL CLOSE HOLIDAY_CURSOR END-EXEC
           .

      *================================================================
       0140-CALCULATE-PAYMENT-DATE.
      *================================================================
      *    DETERMINE PAYMENT DATE (NEXT BUSINESS DAY)
      *    SKIP WEEKENDS AND HOLIDAYS
      *================================================================
           MOVE WS-YYYYMMDD TO WS-PAYMENT-DATE
           MOVE WS-PAYMENT-DATE TO WS-WORK-DATE

      *    START WITH TOMORROW
           EXEC SQL
               SELECT CONVERT(CHAR(8),
                      DATEADD(DAY, 1, :WS-WORK-DATE), 112)
               INTO   :WS-PAYMENT-DATE
           END-EXEC

      *    CHECK IF WEEKEND OR HOLIDAY, ADVANCE IF NEEDED
           PERFORM 0141-CHECK-BUSINESS-DAY
               UNTIL NOT WS-IS-WEEKEND
                 AND NOT WS-IS-HOLIDAY

           STRING WS-PAYMENT-DATE(1:4) '-'
                  WS-PAYMENT-DATE(5:2) '-'
                  WS-PAYMENT-DATE(7:2)
               DELIMITED BY SIZE INTO WS-PAYMENT-DATE-DASH

           MOVE WS-PAYMENT-DATE TO WS-CHECK-DATE
           MOVE WS-PAYMENT-DATE TO WS-EFT-EFFECTIVE-DATE
           .

      *================================================================
       0141-CHECK-BUSINESS-DAY.
      *================================================================
      *    VERIFY IF CURRENT PAYMENT DATE IS A VALID BUSINESS DAY
      *================================================================
           MOVE 'N' TO WS-WEEKEND-FLAG
           MOVE 'N' TO WS-HOLIDAY-FLAG

      *    CHECK DAY OF WEEK (1=SUNDAY, 7=SATURDAY)
           EXEC SQL
               SELECT DATEPART(DW, :WS-PAYMENT-DATE)
               INTO   :WS-DAY-OF-WEEK
           END-EXEC

           IF WS-DAY-OF-WEEK = 1 OR WS-DAY-OF-WEEK = 7
               MOVE 'Y' TO WS-WEEKEND-FLAG
           END-IF

      *    CHECK HOLIDAY TABLE
           PERFORM VARYING WS-HOLIDAY-INDEX FROM 1 BY 1
               UNTIL WS-HOLIDAY-INDEX > WS-HOLIDAY-COUNT
               IF WS-PAYMENT-DATE =
                  WS-HOLIDAY-DATE(WS-HOLIDAY-INDEX)
                   MOVE 'Y' TO WS-HOLIDAY-FLAG
               END-IF
           END-PERFORM

      *    IF NOT A BUSINESS DAY, ADVANCE BY ONE DAY
           IF WS-IS-WEEKEND OR WS-IS-HOLIDAY
               EXEC SQL
                   SELECT CONVERT(CHAR(8),
                          DATEADD(DAY, 1, :WS-PAYMENT-DATE), 112)
                   INTO   :WS-PAYMENT-DATE
               END-EXEC
           END-IF
           .

      *================================================================
       0150-WRITE-EFT-FILE-HEADER.
      *================================================================
      *    WRITE NACHA FILE HEADER RECORD (RECORD TYPE 1)
      *================================================================
           MOVE '0150-WRITE-EFT-FILE-HEADER' TO WS-DB-PARAGRAPH-NAME

      *    LOAD PAYER BANK INFORMATION
           EXEC SQL
               SELECT BANK_ROUTING_NUMBER,
                      BANK_ACCOUNT_NUMBER,
                      BANK_ACCOUNT_TYPE,
                      BANK_NAME,
                      ORIGINATING_COMPANY_ID,
                      ORIGINATING_COMPANY_NAME
               INTO   :WS-PAYER-BANK-ROUTING,
                      :WS-PAYER-BANK-ACCT-NO,
                      :WS-PAYER-BANK-ACCT-TYPE,
                      :WS-PAYER-BANK-NAME,
                      :WS-PAYER-ORIGIN-CO-ID,
                      :WS-PAYER-ORIGIN-NAME
               FROM   PAYER_BANK_CONFIG
               WHERE  PAYER_ID = :WS-PC-PAYER-ID
               AND    ACTIVE_FLAG = 'Y'
               AND    BANK_PURPOSE = 'CLAIMS'
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'PAYER BANK CONFIG LOAD FAILED'
                   TO ER-ERROR-DESC
               MOVE 'F' TO ER-SEVERITY
               PERFORM 8000-ERROR-HANDLER
               PERFORM 8100-DATABASE-ERROR
           END-IF

      *    BUILD FILE HEADER
           MOVE SPACES TO WS-NACHA-FILE-HEADER
           MOVE '1'    TO WS-NFH-RECORD-TYPE
           MOVE '01'   TO WS-NFH-PRIORITY-CODE
           STRING ' ' WS-PAYER-BANK-ROUTING
               DELIMITED BY SIZE INTO WS-NFH-IMMED-DEST
           STRING '1' WS-PAYER-ORIGIN-CO-ID(1:9)
               DELIMITED BY SIZE INTO WS-NFH-IMMED-ORIGIN
           MOVE WS-YYMMDD TO WS-NFH-FILE-DATE
           MOVE WS-HHMM   TO WS-NFH-FILE-TIME
           MOVE 'A'        TO WS-NFH-FILE-ID-MOD
           MOVE '094'      TO WS-NFH-RECORD-SIZE
           MOVE '10'       TO WS-NFH-BLOCK-FACTOR
           MOVE '1'        TO WS-NFH-FORMAT-CODE
           MOVE WS-PAYER-BANK-NAME   TO WS-NFH-DEST-NAME
           MOVE WS-PAYER-ORIGIN-NAME TO WS-NFH-ORIGIN-NAME
           MOVE SPACES TO WS-NFH-REFERENCE-CODE

           WRITE EFT-NACHA-RECORD FROM WS-NACHA-FILE-HEADER

           MOVE 1 TO WS-EFT-BATCH-NUMBER
           .

      *================================================================
       0200-PROCESS-CLAIMS.
      *================================================================
      *    MAIN PROCESSING LOOP - PROCESS EACH CLAIM RECORD,
      *    DETECT PROVIDER/PATIENT/PAYER BREAKS, ACCUMULATE TOTALS
      *================================================================
           IF WS-FIRST-RECORD
               MOVE 'N' TO WS-FIRST-RECORD-FLAG
               MOVE AC-PROVIDER-TAX-ID  TO WS-PREV-PROVIDER-TAX-ID
               MOVE AC-PROVIDER-NPI     TO WS-PREV-PROVIDER-NPI
               MOVE AC-PAY-TO-PROV-NPI  TO WS-PREV-PAY-TO-NPI
               MOVE AC-PAY-TO-NAME      TO WS-PREV-PAY-TO-NAME
               MOVE AC-PAY-TO-ADDR-1    TO WS-PREV-PAY-TO-ADDR-1
               MOVE AC-PAYER-ID         TO WS-PREV-PAYER-ID
               MOVE AC-MEMBER-ID        TO WS-PREV-MEMBER-ID
               MOVE AC-PATIENT-ACCT-NO  TO WS-PREV-PATIENT-ACCT
               MOVE AC-CLAIM-NUMBER     TO WS-PREV-CLAIM-NUMBER
      *        INITIALIZE BUNDLE KEY
               MOVE AC-PROVIDER-TAX-ID  TO WS-BUNDLE-TAX-ID
               MOVE AC-PROVIDER-NPI     TO WS-BUNDLE-NPI
               MOVE AC-PAY-TO-ADDR-1    TO WS-BUNDLE-PAY-TO-ADDR
               MOVE AC-PAYER-ID         TO WS-BUNDLE-PAYER-ID
      *        LOAD PROVIDER EFT STATUS
               PERFORM 0210-LOAD-PROVIDER-BANK
      *        LOAD STATE EOB CONFIG
               MOVE AC-PAY-TO-STATE TO WS-SQL-STATE-CODE
               PERFORM 0220-LOAD-STATE-EOB-CONFIG
      *        START NEW REMIT PAGE
               PERFORM 4100-BUILD-REMIT-HEADER
      *        START NEW 835 TRANSACTION SET
               PERFORM 2300-BUILD-835-TRANSACTION-SET
               PERFORM 2400-BUILD-835-FINANCIAL-INFO
               PERFORM 2500-BUILD-835-REASSOCIATION
               PERFORM 2600-BUILD-835-PAYER-INFO
           END-IF

      *    CHECK FOR PROVIDER BREAK
           IF AC-PROVIDER-TAX-ID NOT = WS-PREV-PROVIDER-TAX-ID
           OR AC-PAY-TO-PROV-NPI NOT = WS-PREV-PAY-TO-NPI
           OR AC-PAY-TO-ADDR-1   NOT = WS-PREV-PAY-TO-ADDR-1
               MOVE 'Y' TO WS-PROVIDER-BREAK
               PERFORM 0300-FINAL-PROVIDER-BREAK
      *        RESET FOR NEW PROVIDER
               MOVE AC-PROVIDER-TAX-ID TO WS-PREV-PROVIDER-TAX-ID
               MOVE AC-PROVIDER-NPI    TO WS-PREV-PROVIDER-NPI
               MOVE AC-PAY-TO-PROV-NPI TO WS-PREV-PAY-TO-NPI
               MOVE AC-PAY-TO-NAME     TO WS-PREV-PAY-TO-NAME
               MOVE AC-PAY-TO-ADDR-1   TO WS-PREV-PAY-TO-ADDR-1
               MOVE AC-PAYER-ID        TO WS-PREV-PAYER-ID
      *        INITIALIZE NEW BUNDLE
               MOVE AC-PROVIDER-TAX-ID TO WS-BUNDLE-TAX-ID
               MOVE AC-PROVIDER-NPI    TO WS-BUNDLE-NPI
               MOVE AC-PAY-TO-ADDR-1   TO WS-BUNDLE-PAY-TO-ADDR
               MOVE AC-PAYER-ID        TO WS-BUNDLE-PAYER-ID
               INITIALIZE WS-BUNDLE-ACCUMULATORS
      *        LOAD PROVIDER EFT STATUS
               PERFORM 0210-LOAD-PROVIDER-BANK
      *        START NEW REMIT PAGE
               PERFORM 4100-BUILD-REMIT-HEADER
      *        START NEW 835 TRANSACTION SET
               PERFORM 2300-BUILD-835-TRANSACTION-SET
               PERFORM 2400-BUILD-835-FINANCIAL-INFO
               PERFORM 2500-BUILD-835-REASSOCIATION
               PERFORM 2600-BUILD-835-PAYER-INFO
               MOVE 'N' TO WS-PROVIDER-BREAK
           END-IF

      *    CHECK FOR PATIENT BREAK (WITHIN SAME PROVIDER)
           IF AC-MEMBER-ID NOT = WS-PREV-MEMBER-ID
               MOVE 'Y' TO WS-PATIENT-BREAK
      *        FINALIZE PREVIOUS PATIENT EOB IF NEEDED
               IF WS-GENERATE-EOB
                   PERFORM 5000-GENERATE-PATIENT-EOB
               END-IF
               MOVE AC-MEMBER-ID       TO WS-PREV-MEMBER-ID
               MOVE AC-PATIENT-ACCT-NO TO WS-PREV-PATIENT-ACCT
               INITIALIZE WS-EOB-CLAIM-TOTALS
               MOVE 'N' TO WS-PATIENT-BREAK
           END-IF

      *    PROCESS BASED ON RECORD TYPE
           EVALUATE TRUE
               WHEN AC-CLAIM-HEADER
                   PERFORM 0250-PROCESS-CLAIM-HEADER
               WHEN AC-CLAIM-LINE
                   PERFORM 0260-PROCESS-CLAIM-LINE
               WHEN AC-CLAIM-ADJ
                   PERFORM 0270-PROCESS-CLAIM-ADJ
               WHEN AC-CLAIM-REMARK
                   PERFORM 0280-PROCESS-CLAIM-REMARK
               WHEN AC-CLAIM-COB
                   PERFORM 0285-PROCESS-CLAIM-COB
               WHEN AC-CLAIM-TRAILER
                   PERFORM 0290-PROCESS-CLAIM-TRAILER
               WHEN OTHER
                   MOVE 'W' TO ER-SEVERITY
                   STRING 'UNKNOWN RECORD TYPE: ' AC-RECORD-TYPE
                       DELIMITED BY SIZE INTO ER-ERROR-DESC
                   PERFORM 8000-ERROR-HANDLER
           END-EVALUATE

           ADD 1 TO WS-STAT-RECORDS-READ

      *    READ NEXT RECORD
           PERFORM 0500-READ-ADJUD-CLAIM
           .

      *================================================================
       0210-LOAD-PROVIDER-BANK.
      *================================================================
      *    LOAD PROVIDER BANK/EFT INFORMATION FOR PAYMENT ROUTING
      *================================================================
           MOVE '0210-LOAD-PROVIDER-BANK' TO WS-DB-PARAGRAPH-NAME
           MOVE AC-PROVIDER-TAX-ID TO WS-SQL-PROVIDER-TAX-ID
           MOVE AC-PROVIDER-NPI    TO WS-SQL-PROVIDER-NPI

           EXEC SQL
               SELECT BANK_ROUTING_NUMBER,
                      BANK_ACCOUNT_NUMBER,
                      BANK_ACCOUNT_TYPE,
                      BANK_NAME,
                      EFT_STATUS,
                      PRENOTE_DATE,
                      EFT_EFFECTIVE_DATE
               INTO   :WS-PROV-BANK-ROUTING,
                      :WS-PROV-BANK-ACCT-NO,
                      :WS-PROV-BANK-ACCT-TYPE,
                      :WS-PROV-BANK-NAME,
                      :WS-PROV-EFT-STATUS,
                      :WS-PROV-PRENOTE-DATE,
                      :WS-PROV-EFT-EFF-DATE
               FROM   PROVIDER_BANK_INFO
               WHERE  PROVIDER_TAX_ID = :WS-SQL-PROVIDER-TAX-ID
               AND    PROVIDER_NPI = :WS-SQL-PROVIDER-NPI
               AND    ACTIVE_FLAG = 'Y'
           END-EXEC

           IF SQLCODE = 0
               IF WS-PROV-EFT-ACTIVE
      *            VERIFY PRENOTE PERIOD HAS ELAPSED (10 DAYS)
                   IF WS-PROV-PRENOTE-DATE NOT = SPACES
                       EXEC SQL
                           SELECT DATEDIFF(DAY,
                                  :WS-PROV-PRENOTE-DATE,
                                  GETDATE())
                           INTO :WS-WORK-COUNT
                       END-EXEC
                       IF WS-WORK-COUNT >= 10
                           MOVE 'Y' TO WS-EFT-ACTIVE
                       ELSE
      *                    PRENOTE STILL IN WAITING PERIOD
                           MOVE 'N' TO WS-EFT-ACTIVE
                           MOVE 'W' TO ER-SEVERITY
                           STRING 'PRENOTE WAITING - PROV NPI: '
                               AC-PROVIDER-NPI
                               ' - ISSUING CHECK INSTEAD'
                               DELIMITED BY SIZE
                               INTO ER-ERROR-DESC
                           PERFORM 8000-ERROR-HANDLER
                       END-IF
                   ELSE
                       MOVE 'Y' TO WS-EFT-ACTIVE
                   END-IF
               ELSE
                   IF WS-PROV-EFT-PRENOTE
      *                NEED TO SEND PRENOTE (ZERO-DOLLAR TEST)
                       MOVE 'Y' TO WS-PRENOTE-FLAG
                       MOVE 'N' TO WS-EFT-ACTIVE
                       ADD 1 TO WS-STAT-PRENOTES
                   ELSE
                       MOVE 'N' TO WS-EFT-ACTIVE
                   END-IF
               END-IF
           ELSE
      *        NO EFT SETUP - USE CHECK
               MOVE 'N' TO WS-EFT-ACTIVE
               INITIALIZE WS-PROVIDER-BANK-INFO
           END-IF
           .

      *================================================================
       0220-LOAD-STATE-EOB-CONFIG.
      *================================================================
      *    LOAD STATE-SPECIFIC EOB CONTENT REQUIREMENTS
      *================================================================
           MOVE '0220-LOAD-STATE-EOB-CONFIG' TO WS-DB-PARAGRAPH-NAME

           EXEC SQL
               SELECT STATE_CODE,
                      BALANCE_BILLING_NOTICE,
                      NETWORK_NOTICE,
                      APPEAL_TEXT_REQUIRED,
                      COMMISSIONER_REQUIRED,
                      GLOSSARY_REQUIRED,
                      LANGUAGE_NOTICE_REQUIRED,
                      SURPRISE_BILLING_NOTICE,
                      GFE_NOTICE_REQUIRED,
                      COMMISSIONER_NAME,
                      COMMISSIONER_PHONE,
                      COMMISSIONER_WEBSITE,
                      LANGUAGE_NOTICE_TEXT
               INTO   :WS-STATE-CODE,
                      :WS-STATE-BAL-BILL-NOTICE,
                      :WS-STATE-NETWORK-NOTICE,
                      :WS-STATE-APPEAL-TEXT-REQ,
                      :WS-STATE-COMMISH-REQ,
                      :WS-STATE-GLOSSARY-REQ,
                      :WS-STATE-LANGUAGE-REQ,
                      :WS-STATE-SURPRISE-BILL,
                      :WS-STATE-GFE-NOTICE,
                      :WS-STATE-COMMISH-NAME,
                      :WS-STATE-COMMISH-PHONE,
                      :WS-STATE-COMMISH-WEB,
                      :WS-STATE-LANG-NOTICE-TEXT
               FROM   STATE_EOB_CONFIG
               WHERE  STATE_CODE = :WS-SQL-STATE-CODE
               AND    ACTIVE_FLAG = 'Y'
           END-EXEC

           IF SQLCODE NOT = 0
      *        DEFAULT - ALL NOTICES REQUIRED AS SAFE FALLBACK
               MOVE WS-SQL-STATE-CODE TO WS-STATE-CODE
               MOVE 'Y' TO WS-STATE-BAL-BILL-NOTICE
               MOVE 'Y' TO WS-STATE-NETWORK-NOTICE
               MOVE 'Y' TO WS-STATE-APPEAL-TEXT-REQ
               MOVE 'Y' TO WS-STATE-COMMISH-REQ
               MOVE 'Y' TO WS-STATE-GLOSSARY-REQ
               MOVE 'N' TO WS-STATE-LANGUAGE-REQ
               MOVE 'Y' TO WS-STATE-SURPRISE-BILL
               MOVE 'N' TO WS-STATE-GFE-NOTICE
               MOVE 'W' TO ER-SEVERITY
               STRING 'STATE EOB CONFIG NOT FOUND FOR: '
                   WS-SQL-STATE-CODE ' - USING DEFAULTS'
                   DELIMITED BY SIZE INTO ER-ERROR-DESC
               PERFORM 8000-ERROR-HANDLER
           END-IF
           .

      *================================================================
       0250-PROCESS-CLAIM-HEADER.
      *================================================================
      *    PROCESS CLAIM HEADER RECORD - INITIALIZE CLAIM-LEVEL
      *    ACCUMULATORS AND TABLES FOR ADJUSTMENTS/REMARKS/LINES
      *================================================================
           INITIALIZE WS-CLAIM-ADJ-TABLE
           MOVE ZEROS TO WS-CAT-COUNT
           INITIALIZE WS-REMARK-TABLE
           MOVE ZEROS TO WS-RMK-COUNT
           INITIALIZE WS-SVC-LINE-TABLE
           MOVE ZEROS TO WS-SLT-COUNT

      *    UPDATE STATISTICS
           ADD 1 TO WS-STAT-CLAIMS-PROCESSED
           ADD 1 TO WS-BUNDLE-CLAIM-COUNT

           EVALUATE TRUE
               WHEN AC-PAID
                   ADD 1 TO WS-STAT-CLAIMS-PAID
                   ADD 1 TO WS-BUNDLE-PAID-COUNT
               WHEN AC-DENIED
                   ADD 1 TO WS-STAT-CLAIMS-DENIED
                   ADD 1 TO WS-BUNDLE-DENIED-COUNT
               WHEN AC-ZERO-PAY
                   ADD 1 TO WS-STAT-CLAIMS-ZEROPAY
                   ADD 1 TO WS-BUNDLE-ZEROPAY-COUNT
               WHEN AC-REVERSED
                   ADD 1 TO WS-STAT-CLAIMS-REVERSED
                   ADD 1 TO WS-BUNDLE-REVERSAL-COUNT
               WHEN AC-ADJUSTED
                   ADD 1 TO WS-STAT-CLAIMS-ADJUSTED
           END-EVALUATE

      *    ACCUMULATE BUNDLE TOTALS
           ADD AC-TOTAL-CHARGE-AMT  TO WS-BUNDLE-CHARGE-TOTAL
           ADD AC-ALLOWED-AMT       TO WS-BUNDLE-ALLOWED-TOTAL
           ADD AC-PAID-AMT          TO WS-BUNDLE-GROSS-AMT
           ADD AC-DEDUCTIBLE-AMT    TO WS-BUNDLE-DEDUCT-TOTAL
           ADD AC-COPAY-AMT         TO WS-BUNDLE-COPAY-TOTAL
           ADD AC-COINSURANCE-AMT   TO WS-BUNDLE-COINS-TOTAL
           ADD AC-COB-AMT           TO WS-BUNDLE-COB-TOTAL
           ADD AC-WITHHOLD-AMT      TO WS-BUNDLE-WITHHOLD-AMT
           ADD AC-INTEREST-AMT      TO WS-BUNDLE-INTEREST-AMT

      *    UPDATE PAYER STATISTICS
           PERFORM 0255-UPDATE-PAYER-STATS

      *    UPDATE LOB STATISTICS
           PERFORM 0257-UPDATE-LOB-STATS
           .

      *================================================================
       0255-UPDATE-PAYER-STATS.
      *================================================================
      *    UPDATE STATISTICS BY PAYER ID
      *================================================================
           MOVE ZEROS TO WS-PAYER-INDEX
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-STAT-PAYER-COUNT
                  OR WS-PAYER-INDEX > 0
               IF WS-SBP-PAYER-ID(WS-WORK-INDEX) = AC-PAYER-ID
                   MOVE WS-WORK-INDEX TO WS-PAYER-INDEX
               END-IF
           END-PERFORM

           IF WS-PAYER-INDEX = ZEROS
      *        NEW PAYER - ADD TO TABLE
               ADD 1 TO WS-STAT-PAYER-COUNT
               IF WS-STAT-PAYER-COUNT <= 20
                   MOVE WS-STAT-PAYER-COUNT TO WS-PAYER-INDEX
                   MOVE AC-PAYER-ID
                       TO WS-SBP-PAYER-ID(WS-PAYER-INDEX)
               ELSE
                   MOVE 20 TO WS-PAYER-INDEX
                   MOVE 20 TO WS-STAT-PAYER-COUNT
               END-IF
           END-IF

           ADD 1 TO WS-SBP-CLAIM-COUNT(WS-PAYER-INDEX)
           ADD AC-PAID-AMT TO WS-SBP-PAID-AMT(WS-PAYER-INDEX)
           .

      *================================================================
       0257-UPDATE-LOB-STATS.
      *================================================================
      *    UPDATE STATISTICS BY LINE OF BUSINESS
      *================================================================
           PERFORM VARYING WS-LOB-INDEX FROM 1 BY 1
               UNTIL WS-LOB-INDEX > 7
               IF WS-SBL-LOB-CODE(WS-LOB-INDEX) =
                  AC-LINE-OF-BUSINESS
                   ADD 1 TO WS-SBL-CLAIM-COUNT(WS-LOB-INDEX)
                   ADD AC-PAID-AMT
                       TO WS-SBL-PAID-AMT(WS-LOB-INDEX)
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *================================================================
       0260-PROCESS-CLAIM-LINE.
      *================================================================
      *    PROCESS SERVICE LINE DETAIL - STORE IN LINE TABLE
      *================================================================
           ADD 1 TO WS-SLT-COUNT
           IF WS-SLT-COUNT > 50
               MOVE 'W' TO ER-SEVERITY
               STRING 'CLAIM ' AC-CLAIM-NUMBER
                   ' HAS MORE THAN 50 LINES - TRUNCATING'
                   DELIMITED BY SIZE INTO ER-ERROR-DESC
               PERFORM 8000-ERROR-HANDLER
               MOVE 50 TO WS-SLT-COUNT
           ELSE
               MOVE AC-LINE-NUMBER
                   TO WS-SLT-LINE-NUM(WS-SLT-COUNT)
               MOVE AC-PROC-CODE
                   TO WS-SLT-PROC-CODE(WS-SLT-COUNT)
               MOVE AC-PROC-MOD-1
                   TO WS-SLT-MOD1(WS-SLT-COUNT)
               MOVE AC-PROC-MOD-2
                   TO WS-SLT-MOD2(WS-SLT-COUNT)
               MOVE AC-PROC-MOD-3
                   TO WS-SLT-MOD3(WS-SLT-COUNT)
               MOVE AC-PROC-MOD-4
                   TO WS-SLT-MOD4(WS-SLT-COUNT)
               MOVE AC-REVENUE-CODE
                   TO WS-SLT-REV-CODE(WS-SLT-COUNT)
               MOVE AC-SERVICE-FROM-DT
                   TO WS-SLT-FROM-DT(WS-SLT-COUNT)
               MOVE AC-SERVICE-THRU-DT
                   TO WS-SLT-THRU-DT(WS-SLT-COUNT)
               MOVE AC-LINE-CHARGE
                   TO WS-SLT-CHARGE(WS-SLT-COUNT)
               MOVE AC-LINE-ALLOWED
                   TO WS-SLT-ALLOWED(WS-SLT-COUNT)
               MOVE AC-LINE-PAID
                   TO WS-SLT-PAID(WS-SLT-COUNT)
               MOVE AC-LINE-DEDUCTIBLE
                   TO WS-SLT-DEDUCT(WS-SLT-COUNT)
               MOVE AC-LINE-COPAY
                   TO WS-SLT-COPAY(WS-SLT-COUNT)
               MOVE AC-LINE-COINSURANCE
                   TO WS-SLT-COINS(WS-SLT-COUNT)
               MOVE AC-LINE-COB
                   TO WS-SLT-COB(WS-SLT-COUNT)
               MOVE AC-LINE-PATIENT-RESP
                   TO WS-SLT-PAT-RESP(WS-SLT-COUNT)
               MOVE AC-UNITS
                   TO WS-SLT-UNITS(WS-SLT-COUNT)
               MOVE AC-NDC-CODE
                   TO WS-SLT-NDC(WS-SLT-COUNT)
               MOVE AC-PLACE-OF-SERVICE
                   TO WS-SLT-POS(WS-SLT-COUNT)
           END-IF

           ADD 1 TO WS-BUNDLE-LINE-COUNT
           .

      *================================================================
       0270-PROCESS-CLAIM-ADJ.
      *================================================================
      *    PROCESS CLAIM ADJUSTMENT RECORD - STORE IN ADJ TABLE
      *================================================================
           ADD 1 TO WS-CAT-COUNT
           IF WS-CAT-COUNT > 30
               MOVE 'W' TO ER-SEVERITY
               STRING 'CLAIM ' AC-CLAIM-NUMBER
                   ' HAS MORE THAN 30 ADJUSTMENTS - TRUNCATING'
                   DELIMITED BY SIZE INTO ER-ERROR-DESC
               PERFORM 8000-ERROR-HANDLER
               MOVE 30 TO WS-CAT-COUNT
           ELSE
               MOVE AC-ADJ-GROUP-CODE
                   TO WS-CAT-GROUP-CODE(WS-CAT-COUNT)
               MOVE AC-ADJ-REASON-CODE
                   TO WS-CAT-REASON-CODE(WS-CAT-COUNT)
               MOVE AC-ADJ-AMOUNT
                   TO WS-CAT-AMOUNT(WS-CAT-COUNT)
               MOVE AC-ADJ-QUANTITY
                   TO WS-CAT-QUANTITY(WS-CAT-COUNT)
           END-IF
           .

      *================================================================
       0280-PROCESS-CLAIM-REMARK.
      *================================================================
      *    PROCESS REMARK CODE RECORD
      *================================================================
           ADD 1 TO WS-RMK-COUNT
           IF WS-RMK-COUNT > 10
               MOVE 10 TO WS-RMK-COUNT
           ELSE
               MOVE AC-REMARK-CODE
                   TO WS-RMK-CODE(WS-RMK-COUNT)
               MOVE AC-REMARK-QUAL
                   TO WS-RMK-QUAL(WS-RMK-COUNT)
           END-IF
           .

      *================================================================
       0285-PROCESS-CLAIM-COB.
      *================================================================
      *    PROCESS COORDINATION OF BENEFITS DATA
      *================================================================
      *    COB INFORMATION IS CARRIED AT CLAIM LEVEL FOR 835
      *    CAS SEGMENTS WITH GROUP CODE OA (OTHER ADJUSTMENT)
           IF AC-COB-PAID-AMT NOT = ZEROS
               ADD 1 TO WS-CAT-COUNT
               IF WS-CAT-COUNT <= 30
                   MOVE 'OA' TO WS-CAT-GROUP-CODE(WS-CAT-COUNT)
                   MOVE '00023'
                       TO WS-CAT-REASON-CODE(WS-CAT-COUNT)
                   MOVE AC-COB-PAID-AMT
                       TO WS-CAT-AMOUNT(WS-CAT-COUNT)
                   MOVE ZEROS
                       TO WS-CAT-QUANTITY(WS-CAT-COUNT)
               END-IF
           END-IF
           .

      *================================================================
       0290-PROCESS-CLAIM-TRAILER.
      *================================================================
      *    CLAIM TRAILER - ALL LINES/ADJS/REMARKS LOADED
      *    NOW GENERATE 835 CLP/CAS/SVC, REMIT DETAIL, EOB DETAIL
      *================================================================
      *    GENERATE 835 CLAIM PAYMENT SEGMENT
           IF WS-GENERATE-835
               PERFORM 2700-BUILD-835-CLAIM-PAYMENT
               PERFORM 2800-BUILD-835-CLAIM-ADJUSTMENT
               PERFORM 2900-BUILD-835-SERVICE-PAYMENT
           END-IF

      *    GENERATE PROVIDER REMITTANCE DETAIL
           IF WS-GENERATE-REMIT
               PERFORM 4200-BUILD-REMIT-CLAIM-DETAIL
           END-IF

      *    ACCUMULATE PATIENT EOB DATA
           IF WS-GENERATE-EOB
               ADD AC-TOTAL-CHARGE-AMT  TO WS-EOB-TOTAL-CHARGED
               COMPUTE WS-WORK-AMOUNT =
                   AC-TOTAL-CHARGE-AMT - AC-ALLOWED-AMT
               ADD WS-WORK-AMOUNT       TO WS-EOB-TOTAL-DISCOUNT
               ADD AC-PAID-AMT          TO WS-EOB-TOTAL-PLAN-PAID
               ADD AC-PATIENT-RESP-AMT  TO WS-EOB-TOTAL-YOU-OWE
               ADD AC-DEDUCTIBLE-AMT    TO WS-EOB-TOTAL-DEDUCTIBLE
               ADD AC-COPAY-AMT         TO WS-EOB-TOTAL-COPAY
               ADD AC-COINSURANCE-AMT   TO WS-EOB-TOTAL-COINSURANCE
               ADD 1                    TO WS-EOB-CLAIM-COUNT
           END-IF

      *    ADD TO RECONCILIATION HASH TOTAL
           ADD AC-PAID-AMT TO WS-RECON-HASH-TOTAL
           ADD 1           TO WS-RECON-RECORD-COUNT
           ADD AC-PAID-AMT TO WS-RECON-TOTAL-PAID-AMT

      *    WRITE AUDIT TRAIL FOR EACH CLAIM
           MOVE WS-TIMESTAMP               TO AT-TIMESTAMP
           MOVE 'PROCESSED'                 TO AT-ACTION
           MOVE 'CLAIM'                     TO AT-ENTITY-TYPE
           MOVE AC-CLAIM-NUMBER             TO AT-ENTITY-KEY
           MOVE SPACES                      TO AT-OLD-VALUE
           MOVE AC-CLAIM-STATUS             TO AT-NEW-VALUE
           MOVE 'BATCH'                     TO AT-USER-ID
           MOVE WS-PROGRAM-ID               TO AT-PROGRAM
           MOVE '0290-PROCESS-CLAIM-TRAIL'  TO AT-PARAGRAPH
           STRING 'CLM ' AC-CLAIM-NUMBER ' PAT '
               AC-PATIENT-ACCT-NO ' $'
               DELIMITED BY SIZE INTO AT-DESCRIPTION
           WRITE AUDIT-RECORD
           .

      *================================================================
       0300-FINAL-PROVIDER-BREAK.
      *================================================================
      *    PROCESS END OF PROVIDER GROUP - GENERATE PAYMENT,
      *    WRITE 835 CLOSING SEGMENTS, REMIT TOTALS, PATIENT EOB
      *================================================================
      *    FINALIZE LAST PATIENT EOB
           IF WS-EOB-CLAIM-COUNT > 0
               IF WS-GENERATE-EOB
                   PERFORM 5000-GENERATE-PATIENT-EOB
               END-IF
           END-IF

      *    CALCULATE NET PAYMENT
           COMPUTE WS-BUNDLE-NET-AMT =
               WS-BUNDLE-GROSS-AMT
             - WS-BUNDLE-WITHHOLD-AMT
             + WS-BUNDLE-INTEREST-AMT
             - WS-BUNDLE-PENALTY-AMT
             + WS-BUNDLE-ADJUST-AMT

      *    CHECK FOR NEGATIVE BALANCE (OVERPAYMENT)
           PERFORM 3400-HANDLE-NEGATIVE-BALANCE

      *    CHECK MINIMUM PAYMENT THRESHOLD
           IF WS-BUNDLE-NET-AMT > ZEROS
              AND WS-BUNDLE-NET-AMT < WS-MIN-PAYMENT-THRESHOLD
               MOVE 'Y' TO WS-MIN-PAY-FLAG
               ADD 1 TO WS-STAT-BELOW-MIN-DEFER
      *        DEFER TO NEXT CYCLE - UPDATE DB
               MOVE '0300-FINAL-PROVIDER-BREAK'
                   TO WS-DB-PARAGRAPH-NAME
               EXEC SQL
                   INSERT INTO DEFERRED_PAYMENTS
                   (PROVIDER_TAX_ID, PROVIDER_NPI, DEFERRED_AMT,
                    DEFERRED_DATE, REASON_CODE, PROGRAM_ID)
                   VALUES
                   (:WS-BUNDLE-TAX-ID, :WS-BUNDLE-NPI,
                    :WS-BUNDLE-NET-AMT, GETDATE(),
                    'BELOW_MIN', 'HCREMIT')
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               ELSE
                   EXEC SQL COMMIT END-EXEC
               END-IF
           ELSE
      *        BUNDLE PAYMENTS AND GENERATE CHECK/EFT
               IF WS-BUNDLE-NET-AMT > ZEROS
                   PERFORM 3000-BUNDLE-PAYMENTS
               END-IF
           END-IF

      *    BUILD PLB ADJUSTMENTS IF ANY
           IF WS-BUNDLE-WITHHOLD-AMT NOT = ZEROS
           OR WS-BUNDLE-INTEREST-AMT NOT = ZEROS
           OR WS-BUNDLE-PENALTY-AMT NOT = ZEROS
               PERFORM 2950-BUILD-835-PLB-ADJUSTMENTS
           END-IF

      *    CLOSE 835 TRANSACTION SET FOR THIS PROVIDER
           MOVE WS-SEGMENT-COUNT TO WS-SE01-SEG-COUNT
           MOVE WS-ST02-CONTROL-NUM TO WS-SE02-CONTROL-NUM
           STRING WS-835-SE-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           ADD 1 TO WS-TRANS-SET-COUNT

      *    WRITE PROVIDER REMIT TOTALS
           IF WS-GENERATE-REMIT
               PERFORM 4300-BUILD-REMIT-TOTALS
               PERFORM 4400-BUILD-REMIT-ADJUSTMENT-DETAIL
           END-IF

      *    UPDATE PROVIDER SUMMARY ACCUMULATORS
           MOVE WS-BUNDLE-TAX-ID     TO WS-PST-TAX-ID
           MOVE WS-PREV-PAY-TO-NAME  TO WS-PST-PROVIDER-NAME
           ADD WS-BUNDLE-CLAIM-COUNT TO WS-PST-TOTAL-CLAIMS
           ADD WS-BUNDLE-PAID-COUNT  TO WS-PST-PAID-CLAIMS
           ADD WS-BUNDLE-DENIED-COUNT TO WS-PST-DENIED-CLAIMS
           ADD WS-BUNDLE-CHARGE-TOTAL TO WS-PST-TOTAL-CHARGED
           ADD WS-BUNDLE-ALLOWED-TOTAL TO WS-PST-TOTAL-ALLOWED
           ADD WS-BUNDLE-GROSS-AMT   TO WS-PST-TOTAL-PAID
           ADD WS-BUNDLE-WITHHOLD-AMT TO WS-PST-TOTAL-WITHHOLD
           ADD WS-BUNDLE-INTEREST-AMT TO WS-PST-TOTAL-INTEREST
           ADD WS-BUNDLE-NET-AMT     TO WS-PST-NET-PAYMENT

           MOVE WS-BUNDLE-NPI         TO WS-PSN-NPI
           MOVE WS-PREV-PAY-TO-NAME   TO WS-PSN-PROVIDER-NAME
           ADD WS-BUNDLE-CLAIM-COUNT  TO WS-PSN-TOTAL-CLAIMS
           ADD WS-BUNDLE-PAID-COUNT   TO WS-PSN-PAID-CLAIMS
           ADD WS-BUNDLE-DENIED-COUNT TO WS-PSN-DENIED-CLAIMS
           ADD WS-BUNDLE-CHARGE-TOTAL TO WS-PSN-TOTAL-CHARGED
           ADD WS-BUNDLE-ALLOWED-TOTAL TO WS-PSN-TOTAL-ALLOWED
           ADD WS-BUNDLE-GROSS-AMT    TO WS-PSN-TOTAL-PAID
           ADD WS-BUNDLE-NET-AMT      TO WS-PSN-NET-PAYMENT
           .

      *================================================================
       0500-READ-ADJUD-CLAIM.
      *================================================================
      *    READ NEXT RECORD FROM ADJUDICATED CLAIMS FILE
      *================================================================
           READ ADJUD-CLAIMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   CONTINUE
           END-READ

           IF WS-ADJCLMS-STATUS NOT = '00'
              AND WS-ADJCLMS-STATUS NOT = '10'
               MOVE 'E' TO ER-SEVERITY
               STRING 'READ ERROR ON ADJUD-CLAIMS: STATUS='
                   WS-ADJCLMS-STATUS
                   DELIMITED BY SIZE INTO ER-ERROR-DESC
               PERFORM 8000-ERROR-HANDLER
               MOVE 'Y' TO WS-EOF-FLAG
           END-IF
           .

      *================================================================
      *  2000-SERIES: 835 TRANSACTION GENERATION
      *================================================================

      *================================================================
       2000-GENERATE-835-TRANSACTION.
      *================================================================
      *    MAIN 835 ROUTING PARAGRAPH - CALLED FOR EACH PROVIDER
      *    GROUP TO BUILD COMPLETE 835 TRANSACTION SET
      *================================================================
           PERFORM 2300-BUILD-835-TRANSACTION-SET
           PERFORM 2400-BUILD-835-FINANCIAL-INFO
           PERFORM 2500-BUILD-835-REASSOCIATION
           PERFORM 2600-BUILD-835-PAYER-INFO
           ADD 1 TO WS-STAT-835-TRANS-SETS
           .

      *================================================================
       2100-BUILD-835-ENVELOPE.
      *================================================================
      *    BUILD ISA INTERCHANGE ENVELOPE SEGMENT
      *    VERSION: 005010X221A1 (HIPAA 5010)
      *    INCLUDES SENDER/RECEIVER IDS, DATE/TIME, CONTROL NUMBERS
      *================================================================
           MOVE '00'   TO WS-ISA01-AUTH-QUAL
           MOVE SPACES TO WS-ISA02-AUTH-INFO
           MOVE '00'   TO WS-ISA03-SEC-QUAL
           MOVE SPACES TO WS-ISA04-SEC-INFO

      *    SET SENDER/RECEIVER QUALIFIERS FROM PAYER CONFIG
           MOVE WS-PC-ISA-SENDER-QUAL   TO WS-ISA05-SEND-QUAL
           MOVE WS-PC-ISA-SENDER-ID     TO WS-ISA06-SENDER-ID
           MOVE WS-PC-ISA-RECEIVER-QUAL TO WS-ISA07-RECV-QUAL
           MOVE WS-PC-ISA-RECEIVER-ID   TO WS-ISA08-RECEIVER-ID

      *    DATE AND TIME
           MOVE WS-YYMMDD TO WS-ISA09-DATE
           MOVE WS-HHMM   TO WS-ISA10-TIME

      *    REPETITION SEPARATOR (5010)
           MOVE '^'    TO WS-ISA11-REPEAT-SEP

      *    VERSION (00501 FOR HIPAA 5010)
           MOVE '00501' TO WS-ISA12-VERSION

      *    INTERCHANGE CONTROL NUMBER
           MOVE WS-ISA-CONTROL-NUM TO WS-ISA13-CONTROL-NUM

      *    ACKNOWLEDGMENT REQUESTED
           MOVE WS-PC-ACK-REQUESTED TO WS-ISA14-ACK-REQ

      *    USAGE INDICATOR: P=PRODUCTION, T=TEST
           MOVE WS-PC-USAGE-INDICATOR TO WS-ISA15-USAGE-IND

      *    COMPONENT ELEMENT SEPARATOR
           MOVE ':' TO WS-ISA16-COMP-SEP

      *    WRITE ISA SEGMENT
           STRING WS-835-ISA-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    PREPARE IEA FOR LATER
           MOVE 1 TO WS-IEA01-GROUP-CNT
           MOVE WS-ISA-CONTROL-NUM TO WS-IEA02-CONTROL-NUM
           .

      *================================================================
       2200-BUILD-835-FUNCTIONAL-GROUP.
      *================================================================
      *    BUILD GS FUNCTIONAL GROUP HEADER SEGMENT
      *    FUNCTIONAL IDENTIFIER: HP (HEALTH CARE CLAIM PAYMENT)
      *================================================================
      *    FUNCTIONAL ID CODE - HP FOR 835
           MOVE 'HP' TO WS-GS01-FUNC-ID

      *    APPLICATION SENDER/RECEIVER CODES
           MOVE WS-PC-GS-SENDER-CODE   TO WS-GS02-SENDER-CODE
           MOVE WS-PC-GS-RECEIVER-CODE TO WS-GS03-RECEIVER-CODE

      *    DATE (CCYYMMDD FORMAT FOR GS)
           MOVE WS-YYYYMMDD TO WS-GS04-DATE

      *    TIME (HHMM FORMAT)
           MOVE WS-HHMM TO WS-GS05-TIME

      *    GROUP CONTROL NUMBER
           MOVE WS-GS-CONTROL-NUM TO WS-GS06-GROUP-CTRL

      *    RESPONSIBLE AGENCY CODE (X = X12)
           MOVE 'X' TO WS-GS07-AGENCY-CODE

      *    VERSION / RELEASE / INDUSTRY ID
           MOVE '005010X221A1' TO WS-GS08-VERSION

      *    WRITE GS SEGMENT
           STRING WS-835-GS-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    PREPARE GE FOR LATER
           MOVE WS-GS-CONTROL-NUM TO WS-GE02-GROUP-CTRL
           ADD 1 TO WS-FUNC-GROUP-COUNT
           .

      *================================================================
       2300-BUILD-835-TRANSACTION-SET.
      *================================================================
      *    BUILD ST TRANSACTION SET HEADER
      *    TRANSACTION SET ID: 835 (HEALTH CARE CLAIM PAYMENT)
      *================================================================
           ADD 1 TO WS-ST-CONTROL-NUM
           MOVE ZEROS TO WS-SEGMENT-COUNT

      *    TRANSACTION SET IDENTIFIER CODE
           MOVE '835' TO WS-ST01-TRANS-ID

      *    TRANSACTION SET CONTROL NUMBER (ZERO-PADDED 9 DIGITS)
           MOVE WS-ST-CONTROL-NUM TO WS-ST02-CONTROL-NUM

      *    IMPLEMENTATION CONVENTION REFERENCE
           MOVE '005010X221A1' TO WS-ST03-IMPL-REF

      *    WRITE ST SEGMENT
           STRING WS-835-ST-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           .

      *================================================================
       2400-BUILD-835-FINANCIAL-INFO.
      *================================================================
      *    BUILD BPR (BEGINNING OF PAYMENT ORDER/REMITTANCE ADVICE)
      *    CONTAINS PAYMENT METHOD, AMOUNTS, BANK ROUTING INFO
      *================================================================
      *    TRANSACTION HANDLING CODE
      *    I = REMITTANCE INFO ONLY (NO PAYMENT)
      *    H = NOTIFICATION ONLY (SPLIT PAYMENTS)
      *    C = PAYMENT ACCOMPANIES REMITTANCE
           IF WS-BUNDLE-NET-AMT = ZEROS
               MOVE 'H' TO WS-BPR01-TRANS-TYPE
           ELSE
               IF AC-PAY-NON
                   MOVE 'H' TO WS-BPR01-TRANS-TYPE
               ELSE
                   MOVE 'I' TO WS-BPR01-TRANS-TYPE
               END-IF
           END-IF

      *    TOTAL ACTUAL PROVIDER PAYMENT AMOUNT
           MOVE WS-BUNDLE-NET-AMT TO WS-BPR02-TOTAL-AMT

      *    CREDIT/DEBIT FLAG
           IF WS-BUNDLE-NET-AMT >= ZEROS
               MOVE 'C' TO WS-BPR03-CREDIT-DEBIT
           ELSE
               MOVE 'D' TO WS-BPR03-CREDIT-DEBIT
           END-IF

      *    PAYMENT METHOD CODE
           IF WS-PROVIDER-USES-EFT
               MOVE 'ACH' TO WS-BPR04-PAY-METHOD
      *        PAYMENT FORMAT CODE
               MOVE 'CTX' TO WS-BPR05-PAY-FORMAT
      *        SENDER DFI IDENTIFICATION
               MOVE '01' TO WS-BPR06-DFI-QUAL-SEND
               MOVE WS-PAYER-BANK-ROUTING TO WS-BPR07-SEND-ROUTING
               MOVE WS-PAYER-BANK-ACCT-TYPE TO WS-BPR08-SEND-ACCT-TYPE
               MOVE WS-PAYER-BANK-ACCT-NO TO WS-BPR09-SEND-ACCT-NUM
      *        ORIGINATING COMPANY SUPPLEMENTARY CODE
               MOVE WS-PAYER-ORIGIN-CO-ID TO WS-BPR10-ORIG-CO-ID
               MOVE SPACES TO WS-BPR11-ORIG-CO-SUPP
      *        RECEIVER DFI IDENTIFICATION
               MOVE '01' TO WS-BPR12-DFI-QUAL-RECV
               MOVE WS-PROV-BANK-ROUTING TO WS-BPR13-RECV-ROUTING
               MOVE WS-PROV-BANK-ACCT-TYPE TO WS-BPR14-RECV-ACCT-TYPE
               MOVE WS-PROV-BANK-ACCT-NO TO WS-BPR15-RECV-ACCT-NUM
           ELSE
               MOVE 'CHK' TO WS-BPR04-PAY-METHOD
               MOVE SPACES TO WS-BPR05-PAY-FORMAT
               MOVE SPACES TO WS-BPR06-DFI-QUAL-SEND
               MOVE SPACES TO WS-BPR07-SEND-ROUTING
               MOVE SPACES TO WS-BPR08-SEND-ACCT-TYPE
               MOVE SPACES TO WS-BPR09-SEND-ACCT-NUM
               MOVE SPACES TO WS-BPR10-ORIG-CO-ID
               MOVE SPACES TO WS-BPR11-ORIG-CO-SUPP
               MOVE SPACES TO WS-BPR12-DFI-QUAL-RECV
               MOVE SPACES TO WS-BPR13-RECV-ROUTING
               MOVE SPACES TO WS-BPR14-RECV-ACCT-TYPE
               MOVE SPACES TO WS-BPR15-RECV-ACCT-NUM
           END-IF

      *    CHECK/PAYMENT DATE
           MOVE WS-PAYMENT-DATE TO WS-BPR16-PAY-DATE

      *    WRITE BPR SEGMENT
           STRING WS-835-BPR-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           .

      *================================================================
       2500-BUILD-835-REASSOCIATION.
      *================================================================
      *    BUILD TRN (REASSOCIATION TRACE NUMBER) AND REF SEGMENTS
      *    LINKS ELECTRONIC REMITTANCE TO CHECK/EFT PAYMENT
      *================================================================
      *    TRN01 - TRACE TYPE CODE (1 = CURRENT TRANSACTION)
           MOVE '1' TO WS-TRN01-TRACE-TYPE

      *    TRN02 - CHECK OR EFT TRACE NUMBER
           IF WS-PROVIDER-USES-EFT
               MOVE WS-EFT-TRACE-NUMBER TO WS-TRN02-CHECK-EFT-NO
           ELSE
               MOVE WS-NEXT-CHECK-NUMBER TO WS-TRN02-CHECK-EFT-NO
           END-IF

      *    TRN03 - ORIGINATOR APPLICATION TRANSACTION ID
           MOVE WS-PC-PAYER-FEIN TO WS-TRN03-ORIGINATOR-ID

      *    TRN04 - ORIGINATOR SUPPLEMENTAL CODE
           MOVE WS-PC-PAYER-ID TO WS-TRN04-ORIG-SUPP-CODE

      *    WRITE TRN SEGMENT
           STRING WS-835-TRN-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    REF*EV - RECEIVER IDENTIFICATION QUALIFIER
           MOVE 'EV' TO WS-REF01-QUALIFIER
           MOVE AC-PAY-TO-PROV-NPI TO WS-REF02-IDENTIFIER
           MOVE SPACES TO WS-REF03-DESCRIPTION
           STRING 'REF' WS-EDI-ELEMENT-SEP
               WS-REF01-QUALIFIER WS-EDI-ELEMENT-SEP
               WS-REF02-IDENTIFIER
               WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    REF*F2 - VERSION IDENTIFICATION CODE
           MOVE 'F2' TO WS-REF01-QUALIFIER
           MOVE '005010X221A1' TO WS-REF02-IDENTIFIER
           STRING 'REF' WS-EDI-ELEMENT-SEP
               WS-REF01-QUALIFIER WS-EDI-ELEMENT-SEP
               WS-REF02-IDENTIFIER
               WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    DTM*405 - PRODUCTION DATE
           MOVE '405' TO WS-DTM01-QUALIFIER
           MOVE WS-YYYYMMDD TO WS-DTM02-DATE
           STRING 'DTM' WS-EDI-ELEMENT-SEP
               WS-DTM01-QUALIFIER WS-EDI-ELEMENT-SEP
               WS-DTM02-DATE WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           .

      *================================================================
       2600-BUILD-835-PAYER-INFO.
      *================================================================
      *    BUILD N1/N3/N4 LOOPS FOR PAYER AND PAYEE IDENTIFICATION
      *================================================================
      *    N1*PR - PAYER IDENTIFICATION
           MOVE 'PR' TO WS-N101-ENTITY-ID
           MOVE WS-PC-PAYER-NAME TO WS-N102-NAME
           MOVE 'XV' TO WS-N103-ID-QUAL
           MOVE WS-PC-PAYER-FEIN TO WS-N104-ID-CODE

           STRING 'N1' WS-EDI-ELEMENT-SEP
               WS-N101-ENTITY-ID WS-EDI-ELEMENT-SEP
               WS-N102-NAME WS-EDI-ELEMENT-SEP
               WS-N103-ID-QUAL WS-EDI-ELEMENT-SEP
               WS-N104-ID-CODE WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    N3 - PAYER ADDRESS
           MOVE WS-PC-PAYER-ADDR-1 TO WS-N301-ADDRESS-1
           MOVE WS-PC-PAYER-ADDR-2 TO WS-N302-ADDRESS-2
           STRING 'N3' WS-EDI-ELEMENT-SEP
               WS-N301-ADDRESS-1
               WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    N4 - PAYER CITY/STATE/ZIP
           MOVE WS-PC-PAYER-CITY  TO WS-N401-CITY
           MOVE WS-PC-PAYER-STATE TO WS-N402-STATE
           MOVE WS-PC-PAYER-ZIP   TO WS-N403-ZIP
           STRING 'N4' WS-EDI-ELEMENT-SEP
               WS-N401-CITY WS-EDI-ELEMENT-SEP
               WS-N402-STATE WS-EDI-ELEMENT-SEP
               WS-N403-ZIP WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    N1*PE - PAYEE IDENTIFICATION
           MOVE 'PE' TO WS-N101-ENTITY-ID
           MOVE AC-PAY-TO-NAME TO WS-N102-NAME
           MOVE 'XX' TO WS-N103-ID-QUAL
           MOVE AC-PAY-TO-PROV-NPI TO WS-N104-ID-CODE

           STRING 'N1' WS-EDI-ELEMENT-SEP
               WS-N101-ENTITY-ID WS-EDI-ELEMENT-SEP
               WS-N102-NAME WS-EDI-ELEMENT-SEP
               WS-N103-ID-QUAL WS-EDI-ELEMENT-SEP
               WS-N104-ID-CODE WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    N3 - PAYEE ADDRESS
           MOVE AC-PAY-TO-ADDR-1 TO WS-N301-ADDRESS-1
           STRING 'N3' WS-EDI-ELEMENT-SEP
               WS-N301-ADDRESS-1 WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    N4 - PAYEE CITY/STATE/ZIP
           MOVE AC-PAY-TO-CITY  TO WS-N401-CITY
           MOVE AC-PAY-TO-STATE TO WS-N402-STATE
           MOVE AC-PAY-TO-ZIP   TO WS-N403-ZIP
           STRING 'N4' WS-EDI-ELEMENT-SEP
               WS-N401-CITY WS-EDI-ELEMENT-SEP
               WS-N402-STATE WS-EDI-ELEMENT-SEP
               WS-N403-ZIP WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    REF*TJ - PAYEE TAX ID
           STRING 'REF' WS-EDI-ELEMENT-SEP
               'TJ' WS-EDI-ELEMENT-SEP
               AC-PROVIDER-TAX-ID WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           .

      *================================================================
       2700-BUILD-835-CLAIM-PAYMENT.
      *================================================================
      *    BUILD CLP (CLAIM PAYMENT INFORMATION) SEGMENT
      *    ONE CLP PER CLAIM IN THE 835 TRANSACTION
      *================================================================
      *    CLP01 - PATIENT CONTROL NUMBER
           MOVE AC-PATIENT-ACCT-NO TO WS-CLP01-PAT-CTRL-NO

      *    CLP02 - CLAIM STATUS CODE
      *    1=PROCESSED PRIMARY, 2=PROCESSED SECONDARY,
      *    3=PROCESSED TERTIARY, 4=DENIED,
      *    22=REVERSAL OF PREVIOUS, 23=NOT OUR CLAIM
           EVALUATE TRUE
               WHEN AC-PAID
                   MOVE '1'  TO WS-CLP02-STATUS
               WHEN AC-DENIED
                   MOVE '4'  TO WS-CLP02-STATUS
               WHEN AC-ZERO-PAY
                   MOVE '1'  TO WS-CLP02-STATUS
               WHEN AC-REVERSED
                   MOVE '22' TO WS-CLP02-STATUS
               WHEN AC-ADJUSTED
                   MOVE '1'  TO WS-CLP02-STATUS
               WHEN OTHER
                   MOVE '1'  TO WS-CLP02-STATUS
           END-EVALUATE

      *    CLP03 - TOTAL CLAIM CHARGE AMOUNT
           MOVE AC-TOTAL-CHARGE-AMT TO WS-CLP03-TOTAL-CHARGE

      *    CLP04 - CLAIM PAYMENT AMOUNT
           MOVE AC-PAID-AMT TO WS-CLP04-PAID-AMT

      *    CLP05 - PATIENT RESPONSIBILITY AMOUNT
           MOVE AC-PATIENT-RESP-AMT TO WS-CLP05-PAT-RESP

      *    CLP06 - CLAIM FILING INDICATOR CODE
           MOVE AC-CLAIM-FILING-IND TO WS-CLP06-FILING-IND

      *    CLP07 - PAYER CLAIM CONTROL NUMBER
           MOVE AC-PAYER-CLM-CTRL-NO TO WS-CLP07-PAYER-CLM-NO

      *    CLP08 - FACILITY TYPE CODE
           MOVE AC-FACILITY-TYPE TO WS-CLP08-FACILITY-TYPE

      *    CLP09 - CLAIM FREQUENCY TYPE CODE
           MOVE AC-CLAIM-FREQUENCY TO WS-CLP09-FREQUENCY

      *    FORMAT AND WRITE CLP SEGMENT
           MOVE SPACES TO WS-EDI-OUTPUT-BUFFER
           STRING 'CLP' WS-EDI-ELEMENT-SEP
               WS-CLP01-PAT-CTRL-NO WS-EDI-ELEMENT-SEP
               WS-CLP02-STATUS WS-EDI-ELEMENT-SEP
               WS-CLP03-TOTAL-CHARGE WS-EDI-ELEMENT-SEP
               WS-CLP04-PAID-AMT WS-EDI-ELEMENT-SEP
               WS-CLP05-PAT-RESP WS-EDI-ELEMENT-SEP
               WS-CLP06-FILING-IND WS-EDI-ELEMENT-SEP
               WS-CLP07-PAYER-CLM-NO WS-EDI-ELEMENT-SEP
               WS-CLP08-FACILITY-TYPE WS-EDI-ELEMENT-SEP
               WS-CLP09-FREQUENCY
               WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           ADD 1 TO WS-CLP-COUNT

      *    CAS AT CLAIM LEVEL (IF DRG, CONTRACTUAL ADJUSTMENTS)
      *    NM1*QC - PATIENT NAME
           STRING 'NM1' WS-EDI-ELEMENT-SEP
               'QC' WS-EDI-ELEMENT-SEP
               '1' WS-EDI-ELEMENT-SEP
               AC-PATIENT-LAST-NAME WS-EDI-ELEMENT-SEP
               AC-PATIENT-FIRST-NAME WS-EDI-ELEMENT-SEP
               AC-PATIENT-MI WS-EDI-ELEMENT-SEP
               WS-EDI-ELEMENT-SEP
               WS-EDI-ELEMENT-SEP
               'MI' WS-EDI-ELEMENT-SEP
               AC-MEMBER-ID
               WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    NM1*82 - RENDERING PROVIDER NAME (IF DIFFERENT)
           IF AC-RENDERING-PROV-NPI NOT = SPACES
           AND AC-RENDERING-PROV-NPI NOT = AC-PROVIDER-NPI
               STRING 'NM1' WS-EDI-ELEMENT-SEP
                   '82' WS-EDI-ELEMENT-SEP
                   '1' WS-EDI-ELEMENT-SEP
                   AC-RENDERING-PROV-NAME WS-EDI-ELEMENT-SEP
                   WS-EDI-ELEMENT-SEP WS-EDI-ELEMENT-SEP
                   WS-EDI-ELEMENT-SEP WS-EDI-ELEMENT-SEP
                   'XX' WS-EDI-ELEMENT-SEP
                   AC-RENDERING-PROV-NPI
                   WS-EDI-SEGMENT-TERM
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT
           END-IF

      *    DTM*232 - CLAIM STATEMENT PERIOD START
           STRING 'DTM' WS-EDI-ELEMENT-SEP
               '232' WS-EDI-ELEMENT-SEP
               AC-STATEMENT-FROM-DT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

      *    DTM*233 - CLAIM STATEMENT PERIOD END
           STRING 'DTM' WS-EDI-ELEMENT-SEP
               '233' WS-EDI-ELEMENT-SEP
               AC-STATEMENT-THRU-DT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           .

      *================================================================
       2800-BUILD-835-CLAIM-ADJUSTMENT.
      *================================================================
      *    BUILD CAS (CLAIM ADJUSTMENT) SEGMENTS AT CLAIM LEVEL
      *    GROUP BY ADJUSTMENT GROUP CODE (CO, PR, PI, OA, CR)
      *    UP TO 6 REASON CODE/AMOUNT PAIRS PER CAS SEGMENT
      *================================================================
           IF WS-CAT-COUNT = ZEROS
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-CAS-GROUP-SAVE
           MOVE ZEROS  TO WS-CAS-ADJ-INDEX

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-CAT-COUNT

      *        CHECK IF NEW GROUP CODE OR 6TH ADJ IN CURRENT CAS
               IF WS-CAT-GROUP-CODE(WS-WORK-INDEX)
                   NOT = WS-CAS-GROUP-SAVE
               OR WS-CAS-ADJ-INDEX >= 6

      *            WRITE PREVIOUS CAS SEGMENT IF EXISTS
                   IF WS-CAS-GROUP-SAVE NOT = SPACES
                       PERFORM 2810-WRITE-CAS-SEGMENT
                   END-IF

      *            START NEW CAS SEGMENT
                   MOVE WS-CAT-GROUP-CODE(WS-WORK-INDEX)
                       TO WS-CAS-GROUP-SAVE
                   MOVE WS-CAS-GROUP-SAVE TO WS-CAS01-GROUP-CODE
                   INITIALIZE WS-CAS-ADJ-ENTRIES
                   MOVE ZEROS TO WS-CAS-ADJ-INDEX
               END-IF

      *        ADD ADJUSTMENT TO CURRENT CAS SEGMENT
               ADD 1 TO WS-CAS-ADJ-INDEX
               MOVE WS-CAT-REASON-CODE(WS-WORK-INDEX)
                   TO WS-CAS-REASON-CODE(WS-CAS-ADJ-INDEX)
               MOVE WS-CAT-AMOUNT(WS-WORK-INDEX)
                   TO WS-CAS-ADJ-AMT(WS-CAS-ADJ-INDEX)
               MOVE WS-CAT-QUANTITY(WS-WORK-INDEX)
                   TO WS-CAS-ADJ-QTY(WS-CAS-ADJ-INDEX)
           END-PERFORM

      *    WRITE FINAL CAS SEGMENT
           IF WS-CAS-GROUP-SAVE NOT = SPACES
               PERFORM 2810-WRITE-CAS-SEGMENT
           END-IF
           .

      *================================================================
       2810-WRITE-CAS-SEGMENT.
      *================================================================
      *    FORMAT AND WRITE A SINGLE CAS SEGMENT
      *================================================================
           MOVE SPACES TO WS-EDI-OUTPUT-BUFFER
           STRING 'CAS' WS-EDI-ELEMENT-SEP
               WS-CAS01-GROUP-CODE
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER

           PERFORM VARYING WS-WORK-INDEX-2 FROM 1 BY 1
               UNTIL WS-WORK-INDEX-2 > WS-CAS-ADJ-INDEX
               IF WS-CAS-REASON-CODE(WS-WORK-INDEX-2) NOT = SPACES
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-ELEMENT-SEP
                       WS-CAS-REASON-CODE(WS-WORK-INDEX-2)
                       WS-EDI-ELEMENT-SEP
                       WS-CAS-ADJ-AMT(WS-WORK-INDEX-2)
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   IF WS-CAS-ADJ-QTY(WS-WORK-INDEX-2) > 0
                       STRING WS-EDI-OUTPUT-BUFFER
                           WS-EDI-ELEMENT-SEP
                           WS-CAS-ADJ-QTY(WS-WORK-INDEX-2)
                           DELIMITED BY SIZE
                           INTO WS-EDI-OUTPUT-BUFFER
                   END-IF
               END-IF
           END-PERFORM

           STRING WS-EDI-OUTPUT-BUFFER WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           ADD 1 TO WS-CAS-COUNT
           .

      *================================================================
       2900-BUILD-835-SERVICE-PAYMENT.
      *================================================================
      *    BUILD SVC (SERVICE PAYMENT INFORMATION) SEGMENTS
      *    ONE SVC PER SERVICE LINE, WITH ASSOCIATED AMT, QTY,
      *    CAS (AT SERVICE LEVEL), DTM, REF, AND LQ SEGMENTS
      *================================================================
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-SLT-COUNT

      *        BUILD SVC01 - COMPOSITE MEDICAL PROCEDURE IDENTIFIER
      *        HC: = HCPCS/CPT QUALIFIER
               IF WS-SLT-PROC-CODE(WS-WORK-INDEX) NOT = SPACES
                   MOVE 'HC' TO WS-SVC01-QUAL
               ELSE
      *            REVENUE CODE ONLY (INSTITUTIONAL)
                   MOVE 'NU' TO WS-SVC01-QUAL
               END-IF

               MOVE WS-SLT-PROC-CODE(WS-WORK-INDEX)
                   TO WS-SVC01-PROC-CODE
               MOVE WS-SLT-MOD1(WS-WORK-INDEX) TO WS-SVC01-MOD1
               MOVE WS-SLT-MOD2(WS-WORK-INDEX) TO WS-SVC01-MOD2
               MOVE WS-SLT-MOD3(WS-WORK-INDEX) TO WS-SVC01-MOD3
               MOVE WS-SLT-MOD4(WS-WORK-INDEX) TO WS-SVC01-MOD4

      *        SVC02 - LINE ITEM CHARGE AMOUNT
               MOVE WS-SLT-CHARGE(WS-WORK-INDEX)
                   TO WS-SVC02-LINE-CHARGE

      *        SVC03 - LINE ITEM PROVIDER PAYMENT AMOUNT
               MOVE WS-SLT-PAID(WS-WORK-INDEX)
                   TO WS-SVC03-LINE-PAID

      *        SVC04 - REVENUE CODE
               MOVE WS-SLT-REV-CODE(WS-WORK-INDEX)
                   TO WS-SVC04-REV-CODE

      *        SVC05 - UNITS OF SERVICE PAID COUNT
               MOVE WS-SLT-UNITS(WS-WORK-INDEX)
                   TO WS-SVC05-UNITS-PAID

      *        FORMAT AND WRITE SVC SEGMENT
               MOVE SPACES TO WS-EDI-OUTPUT-BUFFER
               STRING 'SVC' WS-EDI-ELEMENT-SEP
                   WS-SVC01-QUAL WS-EDI-SUB-ELEM-SEP
                   WS-SVC01-PROC-CODE
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER

      *        ADD MODIFIERS IF PRESENT
               IF WS-SVC01-MOD1 NOT = SPACES
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-SUB-ELEM-SEP WS-SVC01-MOD1
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               END-IF
               IF WS-SVC01-MOD2 NOT = SPACES
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-SUB-ELEM-SEP WS-SVC01-MOD2
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               END-IF
               IF WS-SVC01-MOD3 NOT = SPACES
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-SUB-ELEM-SEP WS-SVC01-MOD3
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               END-IF
               IF WS-SVC01-MOD4 NOT = SPACES
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-SUB-ELEM-SEP WS-SVC01-MOD4
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               END-IF

               STRING WS-EDI-OUTPUT-BUFFER
                   WS-EDI-ELEMENT-SEP WS-SVC02-LINE-CHARGE
                   WS-EDI-ELEMENT-SEP WS-SVC03-LINE-PAID
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER

               IF WS-SVC04-REV-CODE NOT = SPACES
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-ELEMENT-SEP WS-SVC04-REV-CODE
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               END-IF

               STRING WS-EDI-OUTPUT-BUFFER
                   WS-EDI-ELEMENT-SEP WS-SVC05-UNITS-PAID
                   WS-EDI-SEGMENT-TERM
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT
               ADD 1 TO WS-SVC-COUNT

      *        DTM*472 - SERVICE DATE
               STRING 'DTM' WS-EDI-ELEMENT-SEP
                   '472' WS-EDI-ELEMENT-SEP
                   WS-SLT-FROM-DT(WS-WORK-INDEX)
                   WS-EDI-SEGMENT-TERM
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT

      *        AMT*B6 - ALLOWED AMOUNT
               IF WS-SLT-ALLOWED(WS-WORK-INDEX) NOT = ZEROS
                   STRING 'AMT' WS-EDI-ELEMENT-SEP
                       'B6' WS-EDI-ELEMENT-SEP
                       WS-SLT-ALLOWED(WS-WORK-INDEX)
                       WS-EDI-SEGMENT-TERM
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
                   ADD 1 TO WS-SEGMENT-COUNT
               END-IF

      *        AMT*KH - DEDUCTIBLE AMOUNT
               IF WS-SLT-DEDUCT(WS-WORK-INDEX) NOT = ZEROS
                   STRING 'AMT' WS-EDI-ELEMENT-SEP
                       'KH' WS-EDI-ELEMENT-SEP
                       WS-SLT-DEDUCT(WS-WORK-INDEX)
                       WS-EDI-SEGMENT-TERM
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
                   ADD 1 TO WS-SEGMENT-COUNT
               END-IF

      *        AMT*T - COPAY AMOUNT
               IF WS-SLT-COPAY(WS-WORK-INDEX) NOT = ZEROS
                   STRING 'AMT' WS-EDI-ELEMENT-SEP
                       'T' WS-EDI-ELEMENT-SEP
                       WS-SLT-COPAY(WS-WORK-INDEX)
                       WS-EDI-SEGMENT-TERM
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
                   ADD 1 TO WS-SEGMENT-COUNT
               END-IF

      *        AMT*T2 - COINSURANCE AMOUNT
               IF WS-SLT-COINS(WS-WORK-INDEX) NOT = ZEROS
                   STRING 'AMT' WS-EDI-ELEMENT-SEP
                       'T2' WS-EDI-ELEMENT-SEP
                       WS-SLT-COINS(WS-WORK-INDEX)
                       WS-EDI-SEGMENT-TERM
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
                   ADD 1 TO WS-SEGMENT-COUNT
               END-IF

      *        AMT*D8 - COB AMOUNT
               IF WS-SLT-COB(WS-WORK-INDEX) NOT = ZEROS
                   STRING 'AMT' WS-EDI-ELEMENT-SEP
                       'D8' WS-EDI-ELEMENT-SEP
                       WS-SLT-COB(WS-WORK-INDEX)
                       WS-EDI-SEGMENT-TERM
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
                   ADD 1 TO WS-SEGMENT-COUNT
               END-IF

      *        QTY*ZZ - UNITS BILLED
               IF WS-SLT-UNITS(WS-WORK-INDEX) NOT = ZEROS
                   STRING 'QTY' WS-EDI-ELEMENT-SEP
                       'ZZ' WS-EDI-ELEMENT-SEP
                       WS-SLT-UNITS(WS-WORK-INDEX)
                       WS-EDI-SEGMENT-TERM
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
                   WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
                   ADD 1 TO WS-SEGMENT-COUNT
               END-IF

      *        REF*6R - LINE ITEM CONTROL NUMBER
               STRING 'REF' WS-EDI-ELEMENT-SEP
                   '6R' WS-EDI-ELEMENT-SEP
                   WS-SLT-LINE-NUM(WS-WORK-INDEX)
                   WS-EDI-SEGMENT-TERM
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT

      *        LQ - REMARK CODES (WRITE FOR MATCHING LINE)
               PERFORM VARYING WS-WORK-INDEX-2 FROM 1 BY 1
                   UNTIL WS-WORK-INDEX-2 > WS-RMK-COUNT
                   IF WS-RMK-CODE(WS-WORK-INDEX-2) NOT = SPACES
                       MOVE 'HE' TO WS-LQ01-CODE-QUAL
                       MOVE WS-RMK-CODE(WS-WORK-INDEX-2)
                           TO WS-LQ02-REMARK-CODE
                       STRING 'LQ' WS-EDI-ELEMENT-SEP
                           WS-LQ01-CODE-QUAL WS-EDI-ELEMENT-SEP
                           WS-LQ02-REMARK-CODE
                           WS-EDI-SEGMENT-TERM
                           DELIMITED BY SIZE
                           INTO WS-EDI-OUTPUT-BUFFER
                       WRITE EDI-835-RECORD
                           FROM WS-EDI-OUTPUT-BUFFER
                       ADD 1 TO WS-SEGMENT-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *================================================================
       2950-BUILD-835-PLB-ADJUSTMENTS.
      *================================================================
      *    BUILD PLB (PROVIDER LEVEL BALANCE) SEGMENT
      *    FOR NON-CLAIM ADJUSTMENTS: WITHHOLDS, INTEREST,
      *    LATE PENALTIES, RECOUPMENTS, CAPITATION OFFSETS, INCENTIVES
      *================================================================
           MOVE ZEROS TO WS-PLB-ENTRY-COUNT
           INITIALIZE WS-PLB-ADJ-ENTRIES

      *    PLB01 - PROVIDER IDENTIFIER
           MOVE AC-PROVIDER-TAX-ID TO WS-PLB01-PROV-ID

      *    PLB02 - FISCAL PERIOD DATE
           STRING WS-CURR-YEAR '1231'
               DELIMITED BY SIZE INTO WS-PLB02-FISCAL-PERIOD

      *    WITHHOLD ADJUSTMENT (WO = WITHHOLD)
           IF WS-BUNDLE-WITHHOLD-AMT NOT = ZEROS
               ADD 1 TO WS-PLB-ENTRY-COUNT
               MOVE 'WO' TO
                   WS-PLB-ADJ-REASON(WS-PLB-ENTRY-COUNT)
               MOVE AC-PROVIDER-NPI TO
                   WS-PLB-ADJ-REF(WS-PLB-ENTRY-COUNT)
      *        WITHHOLD IS DEDUCTED - STORED AS NEGATIVE
               COMPUTE WS-PLB-ADJ-AMT(WS-PLB-ENTRY-COUNT) =
                   WS-BUNDLE-WITHHOLD-AMT * -1
               ADD WS-BUNDLE-WITHHOLD-AMT
                   TO WS-PLB-WITHHOLD-AMT
           END-IF

      *    INTEREST PAYMENT (IP = INTEREST)
           IF WS-BUNDLE-INTEREST-AMT NOT = ZEROS
               ADD 1 TO WS-PLB-ENTRY-COUNT
               MOVE 'IP' TO
                   WS-PLB-ADJ-REASON(WS-PLB-ENTRY-COUNT)
               MOVE AC-PROVIDER-NPI TO
                   WS-PLB-ADJ-REF(WS-PLB-ENTRY-COUNT)
               MOVE WS-BUNDLE-INTEREST-AMT TO
                   WS-PLB-ADJ-AMT(WS-PLB-ENTRY-COUNT)
               ADD WS-BUNDLE-INTEREST-AMT
                   TO WS-PLB-INTEREST-AMT
           END-IF

      *    LATE FILING PENALTY (LP = LATE PENALTY)
           IF WS-BUNDLE-PENALTY-AMT NOT = ZEROS
               ADD 1 TO WS-PLB-ENTRY-COUNT
               MOVE 'L6' TO
                   WS-PLB-ADJ-REASON(WS-PLB-ENTRY-COUNT)
               MOVE AC-PROVIDER-NPI TO
                   WS-PLB-ADJ-REF(WS-PLB-ENTRY-COUNT)
               COMPUTE WS-PLB-ADJ-AMT(WS-PLB-ENTRY-COUNT) =
                   WS-BUNDLE-PENALTY-AMT * -1
               ADD WS-BUNDLE-PENALTY-AMT
                   TO WS-PLB-LATE-PENALTY
           END-IF

      *    OVERPAYMENT RECOUPMENT (IF OFFSET WAS APPLIED)
           IF WS-NB-OFFSET-AMT NOT = ZEROS
               ADD 1 TO WS-PLB-ENTRY-COUNT
               IF WS-PLB-ENTRY-COUNT <= 6
                   MOVE '72' TO
                       WS-PLB-ADJ-REASON(WS-PLB-ENTRY-COUNT)
                   MOVE AC-PROVIDER-NPI TO
                       WS-PLB-ADJ-REF(WS-PLB-ENTRY-COUNT)
                   COMPUTE WS-PLB-ADJ-AMT(WS-PLB-ENTRY-COUNT) =
                       WS-NB-OFFSET-AMT * -1
                   ADD WS-NB-OFFSET-AMT
                       TO WS-PLB-OVERPAY-RECOUP
               END-IF
           END-IF

      *    WRITE PLB SEGMENT IF ANY ENTRIES EXIST
           IF WS-PLB-ENTRY-COUNT > 0
               MOVE SPACES TO WS-EDI-OUTPUT-BUFFER
               STRING 'PLB' WS-EDI-ELEMENT-SEP
                   WS-PLB01-PROV-ID WS-EDI-ELEMENT-SEP
                   WS-PLB02-FISCAL-PERIOD
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER

               PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
                   UNTIL WS-WORK-INDEX > WS-PLB-ENTRY-COUNT
                   STRING WS-EDI-OUTPUT-BUFFER
                       WS-EDI-ELEMENT-SEP
                       WS-PLB-ADJ-REASON(WS-WORK-INDEX)
                       WS-EDI-SUB-ELEM-SEP
                       WS-PLB-ADJ-REF(WS-WORK-INDEX)
                       WS-EDI-ELEMENT-SEP
                       WS-PLB-ADJ-AMT(WS-WORK-INDEX)
                       DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               END-PERFORM

               STRING WS-EDI-OUTPUT-BUFFER WS-EDI-SEGMENT-TERM
                   DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
               WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT
           END-IF
           .

      *================================================================
      *  3000-SERIES: PAYMENT BUNDLING AND CHECK/EFT GENERATION
      *================================================================

      *================================================================
       3000-BUNDLE-PAYMENTS.
      *================================================================
      *    ACCUMULATE CLAIMS INTO A SINGLE PAYMENT PER PROVIDER
      *    DETERMINE PAYMENT METHOD (CHECK VS EFT)
      *    HANDLE CHECK SPLITTING IF OVER MAXIMUM
      *================================================================
           MOVE WS-BUNDLE-NET-AMT TO WS-CHECK-NET
           MOVE WS-BUNDLE-GROSS-AMT TO WS-CHECK-GROSS
           MOVE WS-BUNDLE-WITHHOLD-AMT TO WS-CHECK-WITHHOLD
           MOVE WS-BUNDLE-ADJUST-AMT TO WS-CHECK-ADJUST
           MOVE WS-BUNDLE-INTEREST-AMT TO WS-CHECK-INTEREST
           MOVE WS-BUNDLE-CLAIM-COUNT TO WS-CHECK-CLAIM-COUNT

      *    CHECK IF AMOUNT EXCEEDS MAXIMUM CHECK AMOUNT
           IF WS-CHECK-NET > WS-MAX-CHECK-AMOUNT
      *        NEED TO SPLIT INTO MULTIPLE CHECKS
               MOVE 'Y' TO WS-SPLIT-CHECK-FLAG
               MOVE WS-CHECK-NET TO WS-SPLIT-REMAINING
               MOVE ZEROS TO WS-SPLIT-CHECK-SEQ

               PERFORM UNTIL WS-SPLIT-REMAINING <= ZEROS
                   ADD 1 TO WS-SPLIT-CHECK-SEQ
                   ADD 1 TO WS-STAT-SPLIT-CHECKS

                   IF WS-SPLIT-REMAINING > WS-MAX-CHECK-AMOUNT
                       MOVE WS-MAX-CHECK-AMOUNT TO WS-CHECK-NET
                   ELSE
                       MOVE WS-SPLIT-REMAINING TO WS-CHECK-NET
                   END-IF

                   SUBTRACT WS-CHECK-NET FROM WS-SPLIT-REMAINING

                   IF WS-PROVIDER-USES-EFT
                       PERFORM 3200-GENERATE-EFT-RECORD
                   ELSE
                       PERFORM 3100-GENERATE-CHECK-RECORD
                   END-IF
               END-PERFORM

               MOVE 'N' TO WS-SPLIT-CHECK-FLAG
           ELSE
      *        SINGLE PAYMENT
               IF WS-PROVIDER-USES-EFT
                   PERFORM 3200-GENERATE-EFT-RECORD
               ELSE
                   PERFORM 3100-GENERATE-CHECK-RECORD
               END-IF
           END-IF

      *    UPDATE CHECK REGISTER
           PERFORM 3300-UPDATE-CHECK-REGISTER
           .

      *================================================================
       3100-GENERATE-CHECK-RECORD.
      *================================================================
      *    GENERATE CHECK - ASSIGN CHECK NUMBER, BUILD CHECK RECORD,
      *    GENERATE POSITIVE PAY RECORD, HANDLE VOID/REISSUE
      *================================================================
      *    ASSIGN NEXT CHECK NUMBER
           MOVE WS-NEXT-CHECK-NUMBER TO WS-CURRENT-CHECK-NUM
           ADD 1 TO WS-NEXT-CHECK-NUMBER

      *    BUILD CHECK REGISTER RECORD
           MOVE WS-CURRENT-CHECK-NUM TO CR-CHECK-NUMBER
           MOVE WS-CHECK-DATE         TO CR-CHECK-DATE
           MOVE WS-PREV-PAY-TO-NAME   TO CR-PAYEE-NAME
           MOVE WS-BUNDLE-TAX-ID      TO CR-PAYEE-TAX-ID
           MOVE WS-BUNDLE-NPI         TO CR-PAYEE-NPI
           MOVE WS-CHECK-GROSS        TO CR-GROSS-AMOUNT
           MOVE WS-CHECK-WITHHOLD     TO CR-WITHHOLD-AMT
           MOVE WS-CHECK-ADJUST       TO CR-ADJUSTMENT-AMT
           MOVE WS-CHECK-INTEREST     TO CR-INTEREST-AMT
           MOVE WS-CHECK-NET          TO CR-NET-AMOUNT
           MOVE 'CHK'                 TO CR-PAYMENT-METHOD
           MOVE SPACES                TO CR-EFT-TRACE-NO
           MOVE 'IS'                  TO CR-STATUS
           MOVE WS-CHECK-CLAIM-COUNT  TO CR-CLAIM-COUNT
           MOVE WS-BUNDLE-PAYER-ID    TO CR-PAYER-ID
           MOVE AC-LINE-OF-BUSINESS   TO CR-LOB

           WRITE CHECK-REG-RECORD

      *    GENERATE POSITIVE PAY RECORD
           MOVE WS-PAYER-BANK-ACCT-NO TO PP-ACCOUNT-NUMBER
           MOVE WS-CURRENT-CHECK-NUM  TO PP-CHECK-NUMBER
           MOVE WS-CHECK-DATE         TO PP-CHECK-DATE
           MOVE WS-CHECK-NET          TO PP-AMOUNT
           MOVE WS-PREV-PAY-TO-NAME(1:50) TO PP-PAYEE-NAME
           MOVE SPACES                TO PP-VOID-INDICATOR

           WRITE POS-PAY-RECORD

      *    CHECK IF THIS IS A VOID AND REISSUE
           IF WS-IS-VOID
               MOVE 'V' TO PP-VOID-INDICATOR
               MOVE WS-VOID-CHECK-NUMBER TO PP-CHECK-NUMBER
               WRITE POS-PAY-RECORD
               ADD 1 TO WS-STAT-VOID-REISSUES
               MOVE 'N' TO WS-VOID-FLAG
           END-IF

           ADD 1 TO WS-STAT-CHECKS-GENERATED
           ADD WS-CHECK-NET TO WS-STAT-TOTAL-CHK-DOLLARS
           ADD WS-CHECK-NET TO WS-STAT-TOTAL-DOLLARS
           ADD WS-CHECK-NET TO WS-RECON-TOTAL-CHK-AMT
           ADD 1 TO WS-RECON-TOTAL-CHECKS

      *    UPDATE PAYER STATS
           IF WS-PAYER-INDEX > 0
               ADD 1 TO WS-SBP-CHECK-COUNT(WS-PAYER-INDEX)
           END-IF
           .

      *================================================================
       3200-GENERATE-EFT-RECORD.
      *================================================================
      *    GENERATE ACH/EFT RECORDS IN NACHA FORMAT
      *    RECORD TYPES: 5=BATCH HEADER, 6=ENTRY DETAIL, 7=ADDENDA
      *================================================================
           IF WS-IS-PRENOTE
      *        PRENOTE: ZERO-DOLLAR TEST TRANSACTION
      *        TRANSACTION CODE 23 = PRENOTE FOR CHECKING CREDIT
               MOVE '23' TO WS-NED-TRANS-CODE
               MOVE ZEROS TO WS-NED-AMOUNT
           ELSE
      *        REGULAR EFT: CODE 22 = CHECKING CREDIT
               IF WS-PROV-BANK-ACCT-TYPE = 'DA'
                   MOVE '22' TO WS-NED-TRANS-CODE
               ELSE
      *            SAVINGS CREDIT = 32
                   MOVE '32' TO WS-NED-TRANS-CODE
               END-IF
               MOVE WS-CHECK-NET TO WS-WORK-AMOUNT
               MULTIPLY WS-WORK-AMOUNT BY 100
                   GIVING WS-NED-AMOUNT
           END-IF

      *    WRITE BATCH HEADER (TYPE 5) FOR THIS PROVIDER
           MOVE SPACES TO WS-NACHA-BATCH-HEADER
           MOVE '5' TO WS-NBH-RECORD-TYPE
           MOVE '220' TO WS-NBH-SERVICE-CLASS
           MOVE WS-PC-PAYER-NAME(1:16) TO WS-NBH-COMPANY-NAME
           MOVE SPACES TO WS-NBH-COMPANY-DISC
           MOVE WS-PAYER-ORIGIN-CO-ID TO WS-NBH-COMPANY-ID
           MOVE 'CTX' TO WS-NBH-ENTRY-CLASS
           MOVE 'CLMSPAYMT' TO WS-NBH-ENTRY-DESC
           MOVE WS-YYMMDD TO WS-NBH-DESC-DATE
           MOVE WS-EFT-EFFECTIVE-DATE(3:6) TO WS-NBH-EFFECTIVE-DATE
           MOVE '1' TO WS-NBH-ORIGIN-STATUS
           MOVE WS-PAYER-BANK-ROUTING(1:8) TO WS-NBH-ORIGIN-DFI
           MOVE WS-EFT-BATCH-NUMBER TO WS-NBH-BATCH-NUMBER

           WRITE EFT-NACHA-RECORD FROM WS-NACHA-BATCH-HEADER

      *    WRITE ENTRY DETAIL (TYPE 6)
           MOVE SPACES TO WS-NACHA-ENTRY-DETAIL
           MOVE '6' TO WS-NED-RECORD-TYPE
           MOVE WS-PROV-BANK-ROUTING(1:8) TO WS-NED-RECV-DFI-ID

      *    CALCULATE CHECK DIGIT (MODULUS 10)
           PERFORM 3210-CALC-CHECK-DIGIT

           MOVE WS-PROV-BANK-ACCT-NO TO WS-NED-DFI-ACCT-NUM
           MOVE WS-BUNDLE-TAX-ID TO WS-NED-RECV-ID-NUM
           MOVE WS-PREV-PAY-TO-NAME(1:22) TO WS-NED-RECV-NAME
           MOVE '1' TO WS-NED-ADDENDA-IND

      *    ASSIGN EFT TRACE NUMBER
           ADD 1 TO WS-EFT-TRACE-NUMBER
           MOVE WS-EFT-TRACE-NUMBER TO WS-NED-TRACE-NUMBER

           WRITE EFT-NACHA-RECORD FROM WS-NACHA-ENTRY-DETAIL

      *    WRITE ADDENDA (TYPE 7) WITH PAYMENT DETAIL
           MOVE SPACES TO WS-NACHA-ADDENDA
           MOVE '7' TO WS-NAD-RECORD-TYPE
           MOVE '05' TO WS-NAD-ADDENDA-TYPE
           STRING 'TRN01*1*' WS-EFT-TRACE-NUMBER
               '*' WS-PC-PAYER-FEIN '\' 'RMR*' WS-BUNDLE-TAX-ID
               '*' WS-CHECK-NET '\'
               DELIMITED BY SIZE INTO WS-NAD-PAYMENT-INFO
           MOVE 0001 TO WS-NAD-ADDENDA-SEQ
           MOVE WS-EFT-BATCH-NUMBER TO WS-NAD-ENTRY-SEQ

           WRITE EFT-NACHA-RECORD FROM WS-NACHA-ADDENDA

      *    WRITE BATCH CONTROL (TYPE 8)
           MOVE SPACES TO WS-NACHA-BATCH-CONTROL
           MOVE '8' TO WS-NBC-RECORD-TYPE
           MOVE '220' TO WS-NBC-SERVICE-CLASS
           MOVE 1 TO WS-NBC-ENTRY-COUNT

      *    ENTRY HASH = FIRST 8 OF RECEIVING ROUTING NUMBER
           MOVE WS-PROV-BANK-ROUTING(1:8) TO WS-NBC-ENTRY-HASH
           MOVE ZEROS TO WS-NBC-TOTAL-DEBIT
           MOVE WS-NED-AMOUNT TO WS-NBC-TOTAL-CREDIT
           MOVE WS-PAYER-ORIGIN-CO-ID TO WS-NBC-COMPANY-ID
           MOVE WS-PAYER-BANK-ROUTING(1:8) TO WS-NBC-ORIGIN-DFI
           MOVE WS-EFT-BATCH-NUMBER TO WS-NBC-BATCH-NUMBER

           WRITE EFT-NACHA-RECORD FROM WS-NACHA-BATCH-CONTROL

      *    UPDATE EFT ACCUMULATORS
           ADD 1 TO WS-EFT-BATCH-NUMBER
           ADD 1 TO WS-EFT-ENTRY-COUNT
           ADD WS-NED-AMOUNT TO WS-EFT-BATCH-CREDIT
           ADD 1 TO WS-EFT-FILE-ENTRY-CNT
           ADD WS-NED-AMOUNT TO WS-EFT-FILE-CREDIT-TOT

      *    BUILD CHECK REGISTER RECORD FOR EFT
           MOVE WS-EFT-TRACE-NUMBER TO CR-CHECK-NUMBER
           MOVE WS-CHECK-DATE       TO CR-CHECK-DATE
           MOVE WS-PREV-PAY-TO-NAME TO CR-PAYEE-NAME
           MOVE WS-BUNDLE-TAX-ID    TO CR-PAYEE-TAX-ID
           MOVE WS-BUNDLE-NPI       TO CR-PAYEE-NPI
           MOVE WS-CHECK-GROSS      TO CR-GROSS-AMOUNT
           MOVE WS-CHECK-WITHHOLD   TO CR-WITHHOLD-AMT
           MOVE WS-CHECK-ADJUST     TO CR-ADJUSTMENT-AMT
           MOVE WS-CHECK-INTEREST   TO CR-INTEREST-AMT
           MOVE WS-CHECK-NET        TO CR-NET-AMOUNT
           MOVE 'EFT'               TO CR-PAYMENT-METHOD
           MOVE WS-EFT-TRACE-NUMBER TO CR-EFT-TRACE-NO
           MOVE 'IS'                TO CR-STATUS
           MOVE WS-CHECK-CLAIM-COUNT TO CR-CLAIM-COUNT
           MOVE WS-BUNDLE-PAYER-ID   TO CR-PAYER-ID
           MOVE AC-LINE-OF-BUSINESS  TO CR-LOB

           WRITE CHECK-REG-RECORD

           ADD 1 TO WS-STAT-EFTS-GENERATED
           ADD WS-CHECK-NET TO WS-STAT-TOTAL-EFT-DOLLARS
           ADD WS-CHECK-NET TO WS-STAT-TOTAL-DOLLARS
           ADD WS-CHECK-NET TO WS-RECON-TOTAL-EFT-AMT
           ADD 1 TO WS-RECON-TOTAL-EFTS

      *    UPDATE PAYER STATS
           IF WS-PAYER-INDEX > 0
               ADD 1 TO WS-SBP-EFT-COUNT(WS-PAYER-INDEX)
           END-IF
           .

      *================================================================
       3210-CALC-CHECK-DIGIT.
      *================================================================
      *    CALCULATE NACHA ROUTING NUMBER CHECK DIGIT (MOD 10)
      *    WEIGHTS: 3, 7, 1, 3, 7, 1, 3, 7
      *================================================================
           MOVE ZEROS TO WS-WORK-COUNT

           COMPUTE WS-WORK-COUNT =
               FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(1:1)) * 3
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(2:1)) * 7
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(3:1)) * 1
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(4:1)) * 3
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(5:1)) * 7
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(6:1)) * 1
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(7:1)) * 3
             + FUNCTION NUMVAL(WS-PROV-BANK-ROUTING(8:1)) * 7

           COMPUTE WS-WORK-COUNT =
               FUNCTION MOD(WS-WORK-COUNT, 10)

           IF WS-WORK-COUNT > 0
               COMPUTE WS-WORK-COUNT = 10 - WS-WORK-COUNT
           END-IF

           MOVE WS-WORK-COUNT TO WS-NED-CHECK-DIGIT
           .

      *================================================================
       3300-UPDATE-CHECK-REGISTER.
      *================================================================
      *    INSERT CHECK/EFT RECORD INTO DATABASE CHECK REGISTER TABLE
      *================================================================
           MOVE '3300-UPDATE-CHECK-REGISTER' TO WS-DB-PARAGRAPH-NAME
           MOVE ZEROS TO WS-DB-DEADLOCK-RETRIES

           PERFORM UNTIL WS-DB-DEADLOCK-RETRIES
                       > WS-DB-MAX-RETRIES
               EXEC SQL
                   INSERT INTO CHECK_REGISTER
                   (CHECK_NUMBER, CHECK_DATE, PAYEE_NAME,
                    PAYEE_TAX_ID, PAYEE_NPI,
                    GROSS_AMOUNT, WITHHOLD_AMOUNT,
                    ADJUSTMENT_AMOUNT, INTEREST_AMOUNT,
                    NET_AMOUNT, PAYMENT_METHOD,
                    EFT_TRACE_NUMBER, STATUS,
                    CLAIM_COUNT, PAYER_ID,
                    LINE_OF_BUSINESS,
                    CREATED_DATE, CREATED_BY)
                   VALUES
                   (:CR-CHECK-NUMBER, :CR-CHECK-DATE,
                    :CR-PAYEE-NAME, :CR-PAYEE-TAX-ID,
                    :CR-PAYEE-NPI,
                    :CR-GROSS-AMOUNT, :CR-WITHHOLD-AMT,
                    :CR-ADJUSTMENT-AMT, :CR-INTEREST-AMT,
                    :CR-NET-AMOUNT, :CR-PAYMENT-METHOD,
                    :CR-EFT-TRACE-NO, :CR-STATUS,
                    :CR-CLAIM-COUNT, :CR-PAYER-ID,
                    :CR-LOB,
                    GETDATE(), 'HCREMIT')
               END-EXEC

               IF SQLCODE = 0
                   EXEC SQL COMMIT END-EXEC
                   EXIT PERFORM
               ELSE
                   IF SQLCODE = WS-DB-DEADLOCK-CODE
                       EXEC SQL ROLLBACK END-EXEC
                       ADD 1 TO WS-DB-DEADLOCK-RETRIES
                   ELSE
                       PERFORM 8100-DATABASE-ERROR
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           .

      *================================================================
       3400-HANDLE-NEGATIVE-BALANCE.
      *================================================================
      *    DETECT AND HANDLE OVERPAYMENT / NEGATIVE BALANCE
      *    OFFSET FROM CURRENT PAYMENT IF POSSIBLE
      *    CREATE A/R RECORD IF NO CURRENT PAYMENT TO OFFSET
      *================================================================
           MOVE '3400-HANDLE-NEGATIVE-BALANCE'
               TO WS-DB-PARAGRAPH-NAME
           MOVE WS-BUNDLE-TAX-ID TO WS-NB-PROVIDER-TAX-ID
           MOVE ZEROS TO WS-NB-OUTSTANDING-AMT
           MOVE ZEROS TO WS-NB-OFFSET-AMT
           MOVE ZEROS TO WS-NB-REMAINING-AMT

      *    CHECK FOR EXISTING OVERPAYMENT BALANCE
           EXEC SQL
               SELECT ISNULL(SUM(OUTSTANDING_AMOUNT), 0),
                      MIN(OVERPAYMENT_DATE)
               INTO   :WS-SQL-OVERPAY-AMT,
                      :WS-SQL-OVERPAY-DATE
               FROM   PROVIDER_OVERPAYMENTS
               WHERE  PROVIDER_TAX_ID = :WS-NB-PROVIDER-TAX-ID
               AND    STATUS IN ('OUTSTANDING', 'PARTIAL')
               AND    PAYMENT_PLAN_FLAG = 'N'
           END-EXEC

           IF SQLCODE = 0 AND WS-SQL-OVERPAY-AMT > ZEROS
               MOVE WS-SQL-OVERPAY-AMT TO WS-NB-OUTSTANDING-AMT
               MOVE WS-SQL-OVERPAY-DATE TO WS-NB-ORIGINAL-OVERPAY-DT

      *        CHECK STATUTE OF LIMITATIONS
               EXEC SQL
                   SELECT DATEDIFF(DAY,
                          :WS-NB-ORIGINAL-OVERPAY-DT,
                          GETDATE())
                   INTO :WS-NB-DAYS-OUTSTANDING
               END-EXEC

               IF WS-NB-DAYS-OUTSTANDING >= WS-NB-STATUTE-LIMIT-DAYS
      *            PAST STATUTE OF LIMITATIONS - CANNOT RECOUP
                   MOVE 'W' TO ER-SEVERITY
                   STRING 'OVERPAY PAST STATUTE - PROV TAX ID: '
                       WS-NB-PROVIDER-TAX-ID
                       ' ORIGINAL DATE: '
                       WS-NB-ORIGINAL-OVERPAY-DT
                       DELIMITED BY SIZE INTO ER-ERROR-DESC
                   PERFORM 8000-ERROR-HANDLER
               ELSE
      *            CAN OFFSET - CALCULATE OFFSET AMOUNT
                   IF WS-BUNDLE-NET-AMT > ZEROS
      *                HAVE CURRENT PAYMENT TO OFFSET AGAINST
                       IF WS-NB-OUTSTANDING-AMT
                           <= WS-BUNDLE-NET-AMT
      *                    FULL OFFSET - CLEAR ENTIRE OVERPAYMENT
                           MOVE WS-NB-OUTSTANDING-AMT
                               TO WS-NB-OFFSET-AMT
                       ELSE
      *                    PARTIAL OFFSET - TAKE ENTIRE CURRENT PMT
      *                    (PROVIDER STILL RECEIVES $0)
                           MOVE WS-BUNDLE-NET-AMT
                               TO WS-NB-OFFSET-AMT
                       END-IF

      *                REDUCE NET PAYMENT BY OFFSET
                       SUBTRACT WS-NB-OFFSET-AMT
                           FROM WS-BUNDLE-NET-AMT

      *                UPDATE OVERPAYMENT RECORD
                       EXEC SQL
                           UPDATE PROVIDER_OVERPAYMENTS
                           SET OUTSTANDING_AMOUNT =
                               OUTSTANDING_AMOUNT -
                               :WS-NB-OFFSET-AMT,
                               LAST_OFFSET_DATE = GETDATE(),
                               LAST_OFFSET_AMOUNT =
                               :WS-NB-OFFSET-AMT,
                               STATUS = CASE
                                   WHEN OUTSTANDING_AMOUNT -
                                        :WS-NB-OFFSET-AMT <= 0
                                   THEN 'RESOLVED'
                                   ELSE 'PARTIAL'
                               END
                           WHERE PROVIDER_TAX_ID =
                               :WS-NB-PROVIDER-TAX-ID
                           AND STATUS IN ('OUTSTANDING', 'PARTIAL')
                       END-EXEC

                       IF SQLCODE = 0
                           EXEC SQL COMMIT END-EXEC
                           ADD 1 TO WS-STAT-NEG-BAL-OFFSETS
                       ELSE
                           PERFORM 8100-DATABASE-ERROR
                       END-IF
                   ELSE
      *                NO CURRENT PAYMENT - CREATE DUNNING TRIGGER
                       MOVE 'Y' TO WS-NB-DUNNING-FLAG
                       MOVE 'I' TO ER-SEVERITY
                       STRING 'NO PAYMENT TO OFFSET - PROV: '
                           WS-NB-PROVIDER-TAX-ID
                           ' OVERPAY: $'
                           DELIMITED BY SIZE INTO ER-ERROR-DESC
                       PERFORM 8000-ERROR-HANDLER

      *                CHECK IF PAYMENT PLAN SHOULD BE OFFERED
                       IF WS-NB-OUTSTANDING-AMT > 5000
                           MOVE 'Y' TO WS-NB-PAYMENT-PLAN-FLAG
                       END-IF

      *                CREATE AR RECORD FOR DUNNING
                       EXEC SQL
                           INSERT INTO DUNNING_QUEUE
                           (PROVIDER_TAX_ID, PROVIDER_NPI,
                            OUTSTANDING_AMT, DUNNING_TYPE,
                            PAYMENT_PLAN_FLAG,
                            CREATED_DATE, CREATED_BY)
                           VALUES
                           (:WS-NB-PROVIDER-TAX-ID,
                            :WS-BUNDLE-NPI,
                            :WS-NB-OUTSTANDING-AMT,
                            'OVERPAYMENT',
                            :WS-NB-PAYMENT-PLAN-FLAG,
                            GETDATE(), 'HCREMIT')
                       END-EXEC
                       IF SQLCODE = 0
                           EXEC SQL COMMIT END-EXEC
                       ELSE
                           PERFORM 8100-DATABASE-ERROR
                       END-IF
                   END-IF
               END-IF
           END-IF

      *    CHECK IF CURRENT RUN ITSELF PRODUCES NEGATIVE
           IF WS-BUNDLE-NET-AMT < ZEROS
               MOVE 'Y' TO WS-NEGATIVE-BALANCE-FLAG

      *        CREATE NEW OVERPAYMENT RECORD
               COMPUTE WS-SQL-AR-AMOUNT =
                   WS-BUNDLE-NET-AMT * -1

               EXEC SQL
                   INSERT INTO PROVIDER_OVERPAYMENTS
                   (PROVIDER_TAX_ID, PROVIDER_NPI,
                    ORIGINAL_AMOUNT, OUTSTANDING_AMOUNT,
                    OVERPAYMENT_DATE, STATUS,
                    PAYMENT_PLAN_FLAG,
                    CREATED_DATE, CREATED_BY)
                   VALUES
                   (:WS-NB-PROVIDER-TAX-ID,
                    :WS-BUNDLE-NPI,
                    :WS-SQL-AR-AMOUNT,
                    :WS-SQL-AR-AMOUNT,
                    GETDATE(), 'OUTSTANDING',
                    'N',
                    GETDATE(), 'HCREMIT')
               END-EXEC
               IF SQLCODE = 0
                   EXEC SQL COMMIT END-EXEC
               ELSE
                   PERFORM 8100-DATABASE-ERROR
               END-IF

      *        SET NET TO ZERO - NO PAYMENT THIS CYCLE
               MOVE ZEROS TO WS-BUNDLE-NET-AMT
               MOVE 'N' TO WS-NEGATIVE-BALANCE-FLAG
           END-IF
           .

      *================================================================
      *  4000-SERIES: PROVIDER REMITTANCE ADVICE
      *================================================================

      *================================================================
       4000-GENERATE-PROVIDER-REMIT.
      *================================================================
      *    MAIN ROUTING FOR PROVIDER REMITTANCE REPORT
      *================================================================
           PERFORM 4100-BUILD-REMIT-HEADER
           ADD 1 TO WS-STAT-REMITS-GENERATED
           .

      *================================================================
       4100-BUILD-REMIT-HEADER.
      *================================================================
      *    BUILD PROVIDER REMITTANCE REPORT HEADER WITH PROVIDER
      *    NAME, NPI, TAX ID, CHECK/EFT NUMBER, PAYMENT DATE
      *================================================================
           ADD 1 TO WS-REMIT-PAGE-CTR
           MOVE WS-REMIT-PAGE-CTR TO WS-RH1-PAGE-NUM

      *    PAGE BREAK
           MOVE SPACES TO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER PAGE-EJECT

      *    HEADER LINE 1 - COMPANY NAME AND REPORT TITLE
           WRITE PROV-REMIT-LINE FROM WS-REMIT-HEADER-1
               AFTER ADVANCING 1 LINES

      *    HEADER LINE 2 - PAYER AND CHECK INFO
           MOVE WS-PC-PAYER-NAME(1:40) TO WS-RH2-PAYER-NAME
           IF WS-PROVIDER-USES-EFT
               MOVE WS-EFT-TRACE-NUMBER TO WS-RH2-CHECK-NUM
           ELSE
               MOVE WS-NEXT-CHECK-NUMBER TO WS-RH2-CHECK-NUM
           END-IF
           MOVE WS-PAYMENT-DATE-DASH TO WS-RH2-PAY-DATE
           WRITE PROV-REMIT-LINE FROM WS-REMIT-HEADER-2
               AFTER ADVANCING 1 LINES

      *    HEADER LINE 3 - PROVIDER INFO
           MOVE AC-PAY-TO-PROV-NPI     TO WS-RH3-NPI
           MOVE AC-PROVIDER-TAX-ID     TO WS-RH3-TAX-ID
           MOVE AC-PAY-TO-NAME(1:50)   TO WS-RH3-PAYEE-NAME
           WRITE PROV-REMIT-LINE FROM WS-REMIT-HEADER-3
               AFTER ADVANCING 1 LINES

      *    SEPARATOR LINE
           WRITE PROV-REMIT-LINE FROM WS-REMIT-HEADER-4
               AFTER ADVANCING 1 LINES

      *    COLUMN HEADERS
           WRITE PROV-REMIT-LINE FROM WS-REMIT-COL-HDR-1
               AFTER ADVANCING 1 LINES

      *    ANOTHER SEPARATOR
           WRITE PROV-REMIT-LINE FROM WS-REMIT-HEADER-4
               AFTER ADVANCING 1 LINES

           MOVE 8 TO WS-REMIT-LINE-CTR
           .

      *================================================================
       4200-BUILD-REMIT-CLAIM-DETAIL.
      *================================================================
      *    BUILD CLAIM-LEVEL DETAIL LINE ON REMITTANCE REPORT
      *    INCLUDES PATIENT NAME, CLAIM #, DOS, AMOUNTS
      *    FOLLOWED BY SERVICE LINE DETAIL AND ADJUSTMENT DETAIL
      *================================================================
      *    CHECK FOR PAGE BREAK
           IF WS-REMIT-LINE-CTR >= WS-REMIT-MAX-LINES
               PERFORM 4100-BUILD-REMIT-HEADER
           END-IF

      *    FORMAT CLAIM DETAIL LINE
           MOVE SPACES TO WS-REMIT-DETAIL-LINE
           STRING AC-PATIENT-LAST-NAME(1:15) ', '
               AC-PATIENT-FIRST-NAME(1:3)
               DELIMITED BY SIZE INTO WS-RD-PATIENT-NAME
           MOVE AC-CLAIM-NUMBER(1:15) TO WS-RD-CLAIM-NUMBER

      *    FORMAT DATES AS MM/DD/YYYY
           STRING AC-STATEMENT-FROM-DT(5:2) '/'
               AC-STATEMENT-FROM-DT(7:2) '/'
               AC-STATEMENT-FROM-DT(1:4)
               DELIMITED BY SIZE INTO WS-RD-DOS-FROM
           STRING AC-STATEMENT-THRU-DT(5:2) '/'
               AC-STATEMENT-THRU-DT(7:2) '/'
               AC-STATEMENT-THRU-DT(1:4)
               DELIMITED BY SIZE INTO WS-RD-DOS-THRU

           MOVE AC-TOTAL-CHARGE-AMT  TO WS-RD-BILLED
           MOVE AC-ALLOWED-AMT       TO WS-RD-ALLOWED
           MOVE AC-DEDUCTIBLE-AMT    TO WS-RD-DEDUCT
           MOVE AC-COPAY-AMT         TO WS-RD-COPAY
           MOVE AC-COINSURANCE-AMT   TO WS-RD-COINSURANCE
           MOVE AC-PAID-AMT          TO WS-RD-PAID

           WRITE PROV-REMIT-LINE FROM WS-REMIT-DETAIL-LINE
               AFTER ADVANCING 1 LINES
           ADD 1 TO WS-REMIT-LINE-CTR

      *    WRITE SERVICE LINE DETAILS
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-SLT-COUNT

               IF WS-REMIT-LINE-CTR >= WS-REMIT-MAX-LINES
                   PERFORM 4100-BUILD-REMIT-HEADER
               END-IF

               MOVE SPACES TO WS-REMIT-SVC-DETAIL-LINE
               MOVE WS-SLT-PROC-CODE(WS-WORK-INDEX)
                   TO WS-RS-PROC-CODE
               IF WS-SLT-MOD1(WS-WORK-INDEX) NOT = SPACES
                   STRING '-' WS-SLT-MOD1(WS-WORK-INDEX)
                       DELIMITED BY SIZE INTO WS-RS-MODIFIERS
                   IF WS-SLT-MOD2(WS-WORK-INDEX) NOT = SPACES
                       STRING WS-RS-MODIFIERS
                           '-' WS-SLT-MOD2(WS-WORK-INDEX)
                           DELIMITED BY SIZE
                           INTO WS-RS-MODIFIERS
                   END-IF
               ELSE
                   MOVE SPACES TO WS-RS-MODIFIERS
               END-IF
               MOVE WS-SLT-REV-CODE(WS-WORK-INDEX)
                   TO WS-RS-REV-CODE

               STRING WS-SLT-FROM-DT(WS-WORK-INDEX)(5:2) '/'
                   WS-SLT-FROM-DT(WS-WORK-INDEX)(7:2) '/'
                   WS-SLT-FROM-DT(WS-WORK-INDEX)(1:4)
                   DELIMITED BY SIZE INTO WS-RS-DOS-FROM
               STRING WS-SLT-THRU-DT(WS-WORK-INDEX)(5:2) '/'
                   WS-SLT-THRU-DT(WS-WORK-INDEX)(7:2) '/'
                   WS-SLT-THRU-DT(WS-WORK-INDEX)(1:4)
                   DELIMITED BY SIZE INTO WS-RS-DOS-THRU

               MOVE WS-SLT-CHARGE(WS-WORK-INDEX)  TO WS-RS-BILLED
               MOVE WS-SLT-ALLOWED(WS-WORK-INDEX) TO WS-RS-ALLOWED
               MOVE WS-SLT-DEDUCT(WS-WORK-INDEX)  TO WS-RS-DEDUCT
               MOVE WS-SLT-COPAY(WS-WORK-INDEX)   TO WS-RS-COPAY
               MOVE WS-SLT-COINS(WS-WORK-INDEX)   TO WS-RS-COINS
               MOVE WS-SLT-PAID(WS-WORK-INDEX)    TO WS-RS-PAID

               WRITE PROV-REMIT-LINE FROM WS-REMIT-SVC-DETAIL-LINE
                   AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-PERFORM

      *    WRITE ADJUSTMENT DETAILS
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-CAT-COUNT

               IF WS-REMIT-LINE-CTR >= WS-REMIT-MAX-LINES
                   PERFORM 4100-BUILD-REMIT-HEADER
               END-IF

               MOVE SPACES TO WS-REMIT-ADJ-DETAIL-LINE
               MOVE WS-CAT-GROUP-CODE(WS-WORK-INDEX)
                   TO WS-RA-GROUP-CODE
               MOVE WS-CAT-REASON-CODE(WS-WORK-INDEX)
                   TO WS-RA-REASON-CODE
               MOVE WS-CAT-AMOUNT(WS-WORK-INDEX)
                   TO WS-RA-AMOUNT

      *        LOOK UP CARC DESCRIPTION
               MOVE WS-CAT-REASON-CODE(WS-WORK-INDEX)
                   TO WS-CARC-LOOKUP-CODE
               PERFORM 6100-MAP-CARC-TO-DESCRIPTION
               MOVE WS-CARC-LOOKUP-DESC TO WS-RA-DESCRIPTION

               WRITE PROV-REMIT-LINE FROM WS-REMIT-ADJ-DETAIL-LINE
                   AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-PERFORM

      *    BLANK LINE AFTER EACH CLAIM
           MOVE SPACES TO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
           ADD 1 TO WS-REMIT-LINE-CTR
           .

      *================================================================
       4300-BUILD-REMIT-TOTALS.
      *================================================================
      *    BUILD PROVIDER REMITTANCE SUBTOTALS AND GRAND TOTALS
      *    INCLUDING CLAIM COUNTS BY STATUS
      *================================================================
      *    CHECK FOR PAGE BREAK
           IF WS-REMIT-LINE-CTR + 15 >= WS-REMIT-MAX-LINES
               PERFORM 4100-BUILD-REMIT-HEADER
           END-IF

      *    SUBTOTAL LINE
           MOVE WS-BUNDLE-CHARGE-TOTAL  TO WS-RST-BILLED
           MOVE WS-BUNDLE-ALLOWED-TOTAL TO WS-RST-ALLOWED
           MOVE WS-BUNDLE-DEDUCT-TOTAL  TO WS-RST-DEDUCT
           MOVE WS-BUNDLE-COPAY-TOTAL   TO WS-RST-COPAY
           MOVE WS-BUNDLE-COINS-TOTAL   TO WS-RST-COINS
           MOVE WS-BUNDLE-GROSS-AMT     TO WS-RST-PAID

           WRITE PROV-REMIT-LINE FROM WS-REMIT-SUBTOTAL-LINE
               AFTER ADVANCING 2 LINES
           ADD 2 TO WS-REMIT-LINE-CTR

      *    GRAND TOTAL LINE (SAME AS SUB FOR SINGLE PROVIDER)
           MOVE WS-BUNDLE-CHARGE-TOTAL  TO WS-RT-BILLED
           MOVE WS-BUNDLE-ALLOWED-TOTAL TO WS-RT-ALLOWED
           MOVE WS-BUNDLE-DEDUCT-TOTAL  TO WS-RT-DEDUCT
           MOVE WS-BUNDLE-COPAY-TOTAL   TO WS-RT-COPAY
           MOVE WS-BUNDLE-COINS-TOTAL   TO WS-RT-COINS
           MOVE WS-BUNDLE-GROSS-AMT     TO WS-RT-PAID

           WRITE PROV-REMIT-LINE FROM WS-REMIT-TOTAL-LINE
               AFTER ADVANCING 1 LINES
           ADD 1 TO WS-REMIT-LINE-CTR

      *    CLAIM COUNT SUMMARY
           MOVE SPACES TO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES

           STRING '  CLAIMS PROCESSED: '
               WS-BUNDLE-CLAIM-COUNT
               '    PAID: '
               WS-BUNDLE-PAID-COUNT
               '    DENIED: '
               WS-BUNDLE-DENIED-COUNT
               '    ZERO-PAY: '
               WS-BUNDLE-ZEROPAY-COUNT
               '    REVERSED: '
               WS-BUNDLE-REVERSAL-COUNT
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
           ADD 3 TO WS-REMIT-LINE-CTR

      *    NET PAYMENT SUMMARY
           MOVE SPACES TO PROV-REMIT-LINE
           MOVE WS-BUNDLE-GROSS-AMT TO WS-EDIT-AMOUNT
           STRING '  GROSS PAYMENT: $' WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-BUNDLE-WITHHOLD-AMT TO WS-EDIT-AMOUNT
           MOVE SPACES TO PROV-REMIT-LINE
           STRING '  WITHHOLD:      $' WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-BUNDLE-INTEREST-AMT TO WS-EDIT-AMOUNT
           MOVE SPACES TO PROV-REMIT-LINE
           STRING '  INTEREST:      $' WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES

           IF WS-NB-OFFSET-AMT NOT = ZEROS
               MOVE WS-NB-OFFSET-AMT TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '  OVERPAY OFFSET:$' WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF

           MOVE SPACES TO PROV-REMIT-LINE
           STRING '                   ' ALL '-'
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-BUNDLE-NET-AMT TO WS-EDIT-AMOUNT
           MOVE SPACES TO PROV-REMIT-LINE
           STRING '  NET PAYMENT:   $' WS-EDIT-AMOUNT
               '   *** '
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           IF WS-PROVIDER-USES-EFT
               STRING PROV-REMIT-LINE
                   'PAID VIA EFT/ACH ***'
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
           ELSE
               STRING PROV-REMIT-LINE
                   'PAID VIA CHECK ***'
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
           END-IF
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
           ADD 6 TO WS-REMIT-LINE-CTR
           .

      *================================================================
       4400-BUILD-REMIT-ADJUSTMENT-DETAIL.
      *================================================================
      *    BUILD NON-CLAIM ADJUSTMENT DETAILS ON REMITTANCE
      *    CAPITATION, INCENTIVES, RECOUPMENTS, PLB DETAIL
      *================================================================
           IF WS-PLB-WITHHOLD-AMT = ZEROS
              AND WS-PLB-INTEREST-AMT = ZEROS
              AND WS-PLB-LATE-PENALTY = ZEROS
              AND WS-PLB-OVERPAY-RECOUP = ZEROS
              AND WS-PLB-CAPITATION-OFFSET = ZEROS
              AND WS-PLB-INCENTIVE-AMT = ZEROS
               EXIT PARAGRAPH
           END-IF

           IF WS-REMIT-LINE-CTR + 10 >= WS-REMIT-MAX-LINES
               PERFORM 4100-BUILD-REMIT-HEADER
           END-IF

           MOVE SPACES TO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES

           STRING '  NON-CLAIM ADJUSTMENTS (PROVIDER LEVEL):'
               DELIMITED BY SIZE INTO PROV-REMIT-LINE
           WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
           ADD 2 TO WS-REMIT-LINE-CTR

           IF WS-PLB-WITHHOLD-AMT NOT = ZEROS
               MOVE WS-PLB-WITHHOLD-AMT TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '    MANAGED CARE WITHHOLD:        $'
                   WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF

           IF WS-PLB-INTEREST-AMT NOT = ZEROS
               MOVE WS-PLB-INTEREST-AMT TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '    INTEREST PAYMENT:             $'
                   WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF

           IF WS-PLB-LATE-PENALTY NOT = ZEROS
               MOVE WS-PLB-LATE-PENALTY TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '    LATE FILING PENALTY:          $'
                   WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF

           IF WS-PLB-OVERPAY-RECOUP NOT = ZEROS
               MOVE WS-PLB-OVERPAY-RECOUP TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '    OVERPAYMENT RECOUPMENT:       $'
                   WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF

           IF WS-PLB-CAPITATION-OFFSET NOT = ZEROS
               MOVE WS-PLB-CAPITATION-OFFSET TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '    CAPITATION OFFSET:            $'
                   WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF

           IF WS-PLB-INCENTIVE-AMT NOT = ZEROS
               MOVE WS-PLB-INCENTIVE-AMT TO WS-EDIT-AMOUNT
               MOVE SPACES TO PROV-REMIT-LINE
               STRING '    INCENTIVE / BONUS PAYMENT:    $'
                   WS-EDIT-AMOUNT
                   DELIMITED BY SIZE INTO PROV-REMIT-LINE
               WRITE PROV-REMIT-LINE AFTER ADVANCING 1 LINES
               ADD 1 TO WS-REMIT-LINE-CTR
           END-IF
           .

      *================================================================
      *  5000-SERIES: PATIENT EOB (EXPLANATION OF BENEFITS)
      *================================================================

      *================================================================
       5000-GENERATE-PATIENT-EOB.
      *================================================================
      *    GENERATE COMPLETE PATIENT EOB WITH CLAIM DETAIL,
      *    BENEFIT SUMMARY, APPEAL RIGHTS, GLOSSARY,
      *    AND STATE-MANDATED NOTICES
      *================================================================
           IF WS-SUPPRESS-EOB
               EXIT PARAGRAPH
           END-IF

           IF WS-EOB-CLAIM-COUNT = ZEROS
               EXIT PARAGRAPH
           END-IF

           PERFORM 5100-BUILD-EOB-HEADER
           PERFORM 5200-BUILD-EOB-CLAIM-DETAIL
           PERFORM 5300-BUILD-EOB-BENEFIT-SUMMARY
           PERFORM 5400-BUILD-EOB-APPEAL-RIGHTS
           PERFORM 5500-BUILD-EOB-GLOSSARY
           PERFORM 5600-BUILD-EOB-STATE-MANDATES

           ADD 1 TO WS-STAT-EOBS-GENERATED

      *    RESET EOB ACCUMULATORS
           INITIALIZE WS-EOB-CLAIM-TOTALS
           .

      *================================================================
       5100-BUILD-EOB-HEADER.
      *================================================================
      *    BUILD EOB HEADER WITH PATIENT NAME, ADDRESS, MEMBER ID,
      *    PLAN NAME, "THIS IS NOT A BILL" NOTICE
      *================================================================
           ADD 1 TO WS-EOB-PAGE-CTR
           MOVE WS-EOB-PAGE-CTR TO WS-EH1-PAGE-NUM

      *    LOAD PATIENT PLAN INFO
           MOVE AC-MEMBER-ID TO WS-SQL-MEMBER-ID
           EXEC SQL
               SELECT GROUP_NUMBER,
                      PLAN_NAME
               INTO   :WS-SQL-GROUP-NUMBER,
                      :WS-SQL-PLAN-NAME
               FROM   MEMBER_ENROLLMENT
               WHERE  MEMBER_ID = :WS-SQL-MEMBER-ID
               AND    EFF_DATE <= :WS-PAYMENT-DATE
               AND    (TERM_DATE IS NULL
                    OR TERM_DATE >= :WS-PAYMENT-DATE)
           END-EXEC

           IF SQLCODE = 0
               MOVE WS-SQL-GROUP-NUMBER TO WS-EOB-PAT-GROUP-NO
               MOVE WS-SQL-PLAN-NAME    TO WS-EOB-PAT-PLAN-NAME
           ELSE
               MOVE 'UNKNOWN' TO WS-EOB-PAT-GROUP-NO
               MOVE 'UNKNOWN' TO WS-EOB-PAT-PLAN-NAME
           END-IF

      *    BUILD PATIENT NAME
           STRING AC-PATIENT-FIRST-NAME ' '
               AC-PATIENT-MI '. '
               AC-PATIENT-LAST-NAME
               DELIMITED BY SIZE INTO WS-EOB-PAT-NAME

           MOVE AC-PATIENT-ADDR-1 TO WS-EOB-PAT-ADDR-1
           MOVE AC-PATIENT-ADDR-2 TO WS-EOB-PAT-ADDR-2
           MOVE AC-PATIENT-CITY   TO WS-EOB-PAT-CITY
           MOVE AC-PATIENT-STATE  TO WS-EOB-PAT-STATE
           MOVE AC-PATIENT-ZIP    TO WS-EOB-PAT-ZIP
           MOVE AC-MEMBER-ID      TO WS-EOB-PAT-MEMBER-ID

      *    PAGE EJECT
           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER PAGE-EJECT

      *    HEADER LINE 1 - PLAN NAME AND TITLE
           MOVE WS-EOB-PAT-PLAN-NAME TO WS-EH1-PLAN-NAME
           WRITE PATIENT-EOB-LINE FROM WS-EOB-HEADER-1
               AFTER ADVANCING 1 LINES

      *    HEADER LINE 2 - DATE, MEMBER ID, GROUP
           MOVE WS-PAYMENT-DATE-DASH TO WS-EH2-EOB-DATE
           MOVE WS-EOB-PAT-MEMBER-ID TO WS-EH2-MEMBER-ID
           MOVE WS-EOB-PAT-GROUP-NO  TO WS-EH2-GROUP-NO
           WRITE PATIENT-EOB-LINE FROM WS-EOB-HEADER-2
               AFTER ADVANCING 1 LINES

      *    PATIENT NAME AND ADDRESS BLOCK
           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           STRING '  ' WS-EOB-PAT-NAME
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  ' WS-EOB-PAT-ADDR-1
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           IF WS-EOB-PAT-ADDR-2 NOT = SPACES
               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  ' WS-EOB-PAT-ADDR-2
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
           END-IF

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  ' WS-EOB-PAT-CITY ', '
               WS-EOB-PAT-STATE ' '
               WS-EOB-PAT-ZIP
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    THIS IS NOT A BILL NOTICE
           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
           WRITE PATIENT-EOB-LINE FROM WS-EOB-NOT-A-BILL
               AFTER ADVANCING 1 LINES
           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    COLUMN HEADERS
           WRITE PATIENT-EOB-LINE FROM WS-EOB-CLAIM-HDR
               AFTER ADVANCING 1 LINES
           WRITE PATIENT-EOB-LINE FROM WS-EOB-CLAIM-HDR-2
               AFTER ADVANCING 1 LINES

      *    SEPARATOR
           MOVE SPACES TO PATIENT-EOB-LINE
           MOVE ALL '-' TO PATIENT-EOB-LINE(1:131)
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE 16 TO WS-EOB-LINE-CTR
           .

      *================================================================
       5200-BUILD-EOB-CLAIM-DETAIL.
      *================================================================
      *    BUILD CLAIM DETAIL ON EOB - PROVIDER NAME, DOS,
      *    SERVICE DESCRIPTION (PROCEDURE CODE LOOKUP),
      *    PROVIDER CHARGED, PLAN DISCOUNT, PLAN PAID, WHAT YOU OWE,
      *    PLAIN-LANGUAGE REASON
      *================================================================
      *    FORMAT CLAIM DETAIL FOR EACH SERVICE LINE
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-SLT-COUNT

               IF WS-EOB-LINE-CTR >= WS-EOB-MAX-LINES
                   PERFORM 5100-BUILD-EOB-HEADER
               END-IF

               MOVE SPACES TO WS-EOB-CLAIM-DETAIL

      *        PROVIDER NAME (FIRST 15 CHARS)
               MOVE AC-RENDERING-PROV-NAME(1:15)
                   TO WS-ECD-PROVIDER-NAME

      *        DATE OF SERVICE
               STRING WS-SLT-FROM-DT(WS-WORK-INDEX)(5:2) '/'
                   WS-SLT-FROM-DT(WS-WORK-INDEX)(7:2) '/'
                   WS-SLT-FROM-DT(WS-WORK-INDEX)(1:4)
                   DELIMITED BY SIZE INTO WS-ECD-DOS

      *        SERVICE DESCRIPTION - LOOKUP PROCEDURE CODE
               MOVE WS-SLT-PROC-CODE(WS-WORK-INDEX)
                   TO WS-PROC-LOOKUP-CODE
               PERFORM 5210-LOOKUP-PROC-DESC
               MOVE WS-PROC-DESC-RESULT(1:20)
                   TO WS-ECD-SVC-DESC

      *        PROVIDER CHARGED
               MOVE WS-SLT-CHARGE(WS-WORK-INDEX)
                   TO WS-ECD-CHARGED

      *        PLAN DISCOUNT (CONTRACTUAL ADJUSTMENT)
               COMPUTE WS-WORK-AMOUNT =
                   WS-SLT-CHARGE(WS-WORK-INDEX)
                 - WS-SLT-ALLOWED(WS-WORK-INDEX)
               MOVE WS-WORK-AMOUNT TO WS-ECD-DISCOUNT

      *        PLAN PAID
               MOVE WS-SLT-PAID(WS-WORK-INDEX)
                   TO WS-ECD-PLAN-PAID

      *        WHAT YOU OWE
               MOVE WS-SLT-PAT-RESP(WS-WORK-INDEX)
                   TO WS-ECD-YOU-OWE

      *        REASON (PLAIN LANGUAGE)
               IF WS-SLT-DEDUCT(WS-WORK-INDEX) > ZEROS
                   MOVE 'DEDUCTIBLE' TO WS-ECD-REASON
               ELSE IF WS-SLT-COPAY(WS-WORK-INDEX) > ZEROS
                   MOVE 'COPAY' TO WS-ECD-REASON
               ELSE IF WS-SLT-COINS(WS-WORK-INDEX) > ZEROS
                   MOVE 'COINSURANCE' TO WS-ECD-REASON
               ELSE IF WS-SLT-PAT-RESP(WS-WORK-INDEX) > ZEROS
                   MOVE 'SEE BELOW' TO WS-ECD-REASON
               ELSE
                   MOVE SPACES TO WS-ECD-REASON
               END-IF

               WRITE PATIENT-EOB-LINE FROM WS-EOB-CLAIM-DETAIL
                   AFTER ADVANCING 1 LINES
               ADD 1 TO WS-EOB-LINE-CTR

      *        WRITE DETAIL REASON IF ADJUSTMENTS PRESENT
               PERFORM VARYING WS-WORK-INDEX-2 FROM 1 BY 1
                   UNTIL WS-WORK-INDEX-2 > WS-CAT-COUNT
                   IF WS-CAT-GROUP-CODE(WS-WORK-INDEX-2) = 'PR'
                       MOVE WS-CAT-REASON-CODE(WS-WORK-INDEX-2)
                           TO WS-CARC-LOOKUP-CODE
                       PERFORM 6100-MAP-CARC-TO-DESCRIPTION
                       MOVE SPACES TO PATIENT-EOB-LINE
                       STRING '                                '
                           '   REASON: '
                           WS-CARC-LOOKUP-DESC(1:50)
                           DELIMITED BY SIZE
                           INTO PATIENT-EOB-LINE
                       WRITE PATIENT-EOB-LINE
                           AFTER ADVANCING 1 LINES
                       ADD 1 TO WS-EOB-LINE-CTR
                   END-IF
               END-PERFORM
           END-PERFORM

      *    EOB TOTALS LINE
           IF WS-EOB-LINE-CTR + 5 >= WS-EOB-MAX-LINES
               PERFORM 5100-BUILD-EOB-HEADER
           END-IF

           MOVE WS-EOB-TOTAL-CHARGED    TO WS-ETL-CHARGED
           MOVE WS-EOB-TOTAL-DISCOUNT   TO WS-ETL-DISCOUNT
           MOVE WS-EOB-TOTAL-PLAN-PAID  TO WS-ETL-PLAN-PAID
           MOVE WS-EOB-TOTAL-YOU-OWE    TO WS-ETL-YOU-OWE

           WRITE PATIENT-EOB-LINE FROM WS-EOB-TOTAL-LINE
               AFTER ADVANCING 2 LINES
           ADD 3 TO WS-EOB-LINE-CTR

      *    BREAKDOWN OF WHAT YOU OWE
           IF WS-EOB-TOTAL-YOU-OWE > ZEROS
               MOVE SPACES TO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  BREAKDOWN OF WHAT YOU OWE:'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               IF WS-EOB-TOTAL-DEDUCTIBLE > ZEROS
                   MOVE WS-EOB-TOTAL-DEDUCTIBLE TO WS-EDIT-AMOUNT-2
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '    DEDUCTIBLE:    $'
                       WS-EDIT-AMOUNT-2
                       '  (Amount you pay before insurance begins)'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 1 TO WS-EOB-LINE-CTR
               END-IF

               IF WS-EOB-TOTAL-COPAY > ZEROS
                   MOVE WS-EOB-TOTAL-COPAY TO WS-EDIT-AMOUNT-2
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '    COPAY:         $'
                       WS-EDIT-AMOUNT-2
                       '  (Your fixed amount for this service)'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 1 TO WS-EOB-LINE-CTR
               END-IF

               IF WS-EOB-TOTAL-COINSURANCE > ZEROS
                   MOVE WS-EOB-TOTAL-COINSURANCE
                       TO WS-EDIT-AMOUNT-2
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '    COINSURANCE:   $'
                       WS-EDIT-AMOUNT-2
                       '  (Your percentage share of allowed amount)'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 1 TO WS-EOB-LINE-CTR
               END-IF

               ADD 3 TO WS-EOB-LINE-CTR
           END-IF
           .

      *================================================================
       5210-LOOKUP-PROC-DESC.
      *================================================================
      *    LOOK UP PROCEDURE CODE DESCRIPTION FROM DATABASE
      *================================================================
           MOVE WS-PROC-LOOKUP-CODE TO WS-SQL-PROC-CODE

           EXEC SQL
               SELECT PROC_SHORT_DESC
               INTO   :WS-SQL-PROC-DESC
               FROM   PROCEDURE_CODES
               WHERE  PROC_CODE = :WS-SQL-PROC-CODE
               AND    EFF_DATE <= :WS-PAYMENT-DATE
               AND    (TERM_DATE IS NULL
                    OR TERM_DATE >= :WS-PAYMENT-DATE)
           END-EXEC

           IF SQLCODE = 0
               MOVE WS-SQL-PROC-DESC TO WS-PROC-DESC-RESULT
           ELSE
               STRING 'PROC CODE ' WS-PROC-LOOKUP-CODE
                   DELIMITED BY SIZE INTO WS-PROC-DESC-RESULT
           END-IF
           .

      *================================================================
       5300-BUILD-EOB-BENEFIT-SUMMARY.
      *================================================================
      *    BUILD YEAR-TO-DATE BENEFIT SUMMARY SHOWING DEDUCTIBLE
      *    AND OOP MAXIMUM PROGRESS (INDIVIDUAL AND FAMILY)
      *================================================================
      *    LOAD YTD BENEFIT ACCUMULATORS FROM DB
           MOVE AC-MEMBER-ID TO WS-SQL-MEMBER-ID

           EXEC SQL
               SELECT ISNULL(IND_DEDUCT_ACCUM, 0),
                      ISNULL(FAM_DEDUCT_ACCUM, 0),
                      ISNULL(IND_OOP_ACCUM, 0),
                      ISNULL(FAM_OOP_ACCUM, 0),
                      ISNULL(IND_DEDUCT_MAX, 0),
                      ISNULL(FAM_DEDUCT_MAX, 0),
                      ISNULL(IND_OOP_MAX, 0),
                      ISNULL(FAM_OOP_MAX, 0)
               INTO   :WS-SQL-YTD-DEDUCT-IND,
                      :WS-SQL-YTD-DEDUCT-FAM,
                      :WS-SQL-YTD-OOP-IND,
                      :WS-SQL-YTD-OOP-FAM,
                      :WS-SQL-DEDUCT-MAX-IND,
                      :WS-SQL-DEDUCT-MAX-FAM,
                      :WS-SQL-OOP-MAX-IND,
                      :WS-SQL-OOP-MAX-FAM
               FROM   BENEFIT_ACCUMULATORS
               WHERE  MEMBER_ID = :WS-SQL-MEMBER-ID
               AND    PLAN_YEAR = :WS-CURR-YEAR
           END-EXEC

           IF SQLCODE = 0
               MOVE WS-SQL-YTD-DEDUCT-IND
                   TO WS-EOB-IND-DEDUCT-USED
               MOVE WS-SQL-DEDUCT-MAX-IND
                   TO WS-EOB-IND-DEDUCT-MAX
               COMPUTE WS-EOB-IND-DEDUCT-REM =
                   WS-EOB-IND-DEDUCT-MAX - WS-EOB-IND-DEDUCT-USED
               IF WS-EOB-IND-DEDUCT-REM <= ZEROS
                   MOVE 'Y' TO WS-EOB-IND-DEDUCT-MET
                   MOVE ZEROS TO WS-EOB-IND-DEDUCT-REM
               END-IF

               MOVE WS-SQL-YTD-DEDUCT-FAM
                   TO WS-EOB-FAM-DEDUCT-USED
               MOVE WS-SQL-DEDUCT-MAX-FAM
                   TO WS-EOB-FAM-DEDUCT-MAX
               COMPUTE WS-EOB-FAM-DEDUCT-REM =
                   WS-EOB-FAM-DEDUCT-MAX - WS-EOB-FAM-DEDUCT-USED
               IF WS-EOB-FAM-DEDUCT-REM <= ZEROS
                   MOVE 'Y' TO WS-EOB-FAM-DEDUCT-MET
                   MOVE ZEROS TO WS-EOB-FAM-DEDUCT-REM
               END-IF

               MOVE WS-SQL-YTD-OOP-IND
                   TO WS-EOB-IND-OOP-USED
               MOVE WS-SQL-OOP-MAX-IND
                   TO WS-EOB-IND-OOP-MAX
               COMPUTE WS-EOB-IND-OOP-REM =
                   WS-EOB-IND-OOP-MAX - WS-EOB-IND-OOP-USED
               IF WS-EOB-IND-OOP-REM < ZEROS
                   MOVE ZEROS TO WS-EOB-IND-OOP-REM
               END-IF

               MOVE WS-SQL-YTD-OOP-FAM
                   TO WS-EOB-FAM-OOP-USED
               MOVE WS-SQL-OOP-MAX-FAM
                   TO WS-EOB-FAM-OOP-MAX
               COMPUTE WS-EOB-FAM-OOP-REM =
                   WS-EOB-FAM-OOP-MAX - WS-EOB-FAM-OOP-USED
               IF WS-EOB-FAM-OOP-REM < ZEROS
                   MOVE ZEROS TO WS-EOB-FAM-OOP-REM
               END-IF
           ELSE
               EXIT PARAGRAPH
           END-IF

      *    WRITE BENEFIT SUMMARY SECTION
           IF WS-EOB-LINE-CTR + 15 >= WS-EOB-MAX-LINES
               PERFORM 5100-BUILD-EOB-HEADER
           END-IF

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 2 LINES
           MOVE ALL '=' TO PATIENT-EOB-LINE(1:80)
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  YOUR BENEFIT SUMMARY FOR '
               WS-CURR-YEAR ':'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    INDIVIDUAL DEDUCTIBLE
           MOVE WS-EOB-IND-DEDUCT-USED TO WS-EDIT-AMOUNT-2
           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  INDIVIDUAL DEDUCTIBLE:   USED: $'
               WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           MOVE WS-EOB-IND-DEDUCT-MAX TO WS-EDIT-AMOUNT-2
           STRING PATIENT-EOB-LINE '   OF $' WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           IF WS-EOB-IND-DEDUCT-MET = 'Y'
               STRING PATIENT-EOB-LINE '   ** MET **'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           ELSE
               MOVE WS-EOB-IND-DEDUCT-REM TO WS-EDIT-AMOUNT-2
               STRING PATIENT-EOB-LINE
                   '   REMAINING: $' WS-EDIT-AMOUNT-2
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           END-IF
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    FAMILY DEDUCTIBLE
           MOVE WS-EOB-FAM-DEDUCT-USED TO WS-EDIT-AMOUNT-2
           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  FAMILY DEDUCTIBLE:       USED: $'
               WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           MOVE WS-EOB-FAM-DEDUCT-MAX TO WS-EDIT-AMOUNT-2
           STRING PATIENT-EOB-LINE '   OF $' WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           IF WS-EOB-FAM-DEDUCT-MET = 'Y'
               STRING PATIENT-EOB-LINE '   ** MET **'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           ELSE
               MOVE WS-EOB-FAM-DEDUCT-REM TO WS-EDIT-AMOUNT-2
               STRING PATIENT-EOB-LINE
                   '   REMAINING: $' WS-EDIT-AMOUNT-2
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           END-IF
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    INDIVIDUAL OOP MAXIMUM
           MOVE WS-EOB-IND-OOP-USED TO WS-EDIT-AMOUNT-2
           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  INDIVIDUAL OUT-OF-POCKET: USED: $'
               WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           MOVE WS-EOB-IND-OOP-MAX TO WS-EDIT-AMOUNT-2
           STRING PATIENT-EOB-LINE '   MAX: $' WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           MOVE WS-EOB-IND-OOP-REM TO WS-EDIT-AMOUNT-2
           STRING PATIENT-EOB-LINE
               '   REMAINING: $' WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    FAMILY OOP MAXIMUM
           MOVE WS-EOB-FAM-OOP-USED TO WS-EDIT-AMOUNT-2
           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  FAMILY OUT-OF-POCKET:     USED: $'
               WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           MOVE WS-EOB-FAM-OOP-MAX TO WS-EDIT-AMOUNT-2
           STRING PATIENT-EOB-LINE '   MAX: $' WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE

           MOVE WS-EOB-FAM-OOP-REM TO WS-EDIT-AMOUNT-2
           STRING PATIENT-EOB-LINE
               '   REMAINING: $' WS-EDIT-AMOUNT-2
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           ADD 12 TO WS-EOB-LINE-CTR
           .

      *================================================================
       5400-BUILD-EOB-APPEAL-RIGHTS.
      *================================================================
      *    BUILD APPEAL RIGHTS SECTION WITH TIMEFRAMES,
      *    INTERNAL/EXTERNAL REVIEW RIGHTS, CONTACT INFO
      *================================================================
           IF NOT WS-STATE-REQ-APPEAL
               EXIT PARAGRAPH
           END-IF

           IF WS-EOB-LINE-CTR + 15 >= WS-EOB-MAX-LINES
               PERFORM 5100-BUILD-EOB-HEADER
           END-IF

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 2 LINES
           MOVE ALL '=' TO PATIENT-EOB-LINE(1:80)
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  YOUR RIGHT TO APPEAL'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  If you disagree with any decision on '
               'this Explanation of Benefits, you have'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  the right to appeal. Here is how:'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

      *    CALCULATE APPEAL DEADLINE
           EXEC SQL
               SELECT CONVERT(CHAR(8),
                      DATEADD(DAY, :WS-PC-APPEAL-DAYS,
                              :WS-PAYMENT-DATE), 112)
               INTO :WS-PRL-APPEAL-DEADLINE
           END-EXEC

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  INTERNAL APPEAL: You have '
               WS-PC-APPEAL-DAYS
               ' days from the date of this EOB to file'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  an internal appeal. Deadline: '
               WS-PRL-APPEAL-DEADLINE(5:2) '/'
               WS-PRL-APPEAL-DEADLINE(7:2) '/'
               WS-PRL-APPEAL-DEADLINE(1:4)
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  EXTERNAL REVIEW: If your internal '
               'appeal is denied, you have the right to'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  an independent external review by a '
               'third party not affiliated with our plan.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  TO FILE AN APPEAL: Call '
               WS-PC-APPEAL-PHONE(1:3) '-'
               WS-PC-APPEAL-PHONE(4:3) '-'
               WS-PC-APPEAL-PHONE(7:4)
               ' or write to:'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  '
               WS-PC-APPEAL-ADDRESS
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  Or visit: ' WS-PC-WEBSITE
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           ADD 18 TO WS-EOB-LINE-CTR
           .

      *================================================================
       5500-BUILD-EOB-GLOSSARY.
      *================================================================
      *    BUILD HEALTHCARE TERMS GLOSSARY ON EOB
      *    STANDARD DEFINITIONS FOR COMMON INSURANCE TERMS
      *================================================================
           IF NOT WS-STATE-REQ-GLOSSARY
               EXIT PARAGRAPH
           END-IF

           IF WS-EOB-LINE-CTR + 25 >= WS-EOB-MAX-LINES
               PERFORM 5100-BUILD-EOB-HEADER
           END-IF

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 2 LINES
           MOVE ALL '=' TO PATIENT-EOB-LINE(1:80)
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  UNDERSTANDING YOUR EOB - GLOSSARY OF TERMS'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  ALLOWED AMOUNT: The maximum amount your '
               'plan will pay for a covered service.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  This is based on contracts between your '
               'plan and your provider.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  COINSURANCE: Your share of the allowed '
               'amount for a covered service, usually a'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  percentage (for example, 20%). You pay '
               'coinsurance plus any deductible you owe.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  COPAYMENT (COPAY): A fixed amount you '
               'pay for a covered service, usually at the'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  time of service. Example: $25 for a '
               'doctor visit or $10 for a prescription.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  DEDUCTIBLE: The amount you must pay each'
               ' year before your plan begins to pay.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  Both individual and family deductibles '
               'may apply to your plan.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  OUT-OF-POCKET MAXIMUM: The most you will'
               ' pay during a plan year for covered'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  services. After reaching this amount, '
               'your plan pays 100% of covered services.'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           ADD 22 TO WS-EOB-LINE-CTR
           .

      *================================================================
       5600-BUILD-EOB-STATE-MANDATES.
      *================================================================
      *    BUILD STATE-SPECIFIC DISCLOSURE REQUIREMENTS
      *    CA, NY, TX, FL EACH HAVE UNIQUE EOB CONTENT RULES
      *    BALANCE BILLING PROTECTIONS, NETWORK ADEQUACY,
      *    NO SURPRISES ACT NOTICES
      *================================================================
           IF WS-EOB-LINE-CTR + 20 >= WS-EOB-MAX-LINES
               PERFORM 5100-BUILD-EOB-HEADER
           END-IF

           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 2 LINES
           MOVE ALL '=' TO PATIENT-EOB-LINE(1:80)
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PATIENT-EOB-LINE
           STRING '  IMPORTANT NOTICES'
               DELIMITED BY SIZE INTO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
           MOVE SPACES TO PATIENT-EOB-LINE
           WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
           ADD 5 TO WS-EOB-LINE-CTR

      *    NO SURPRISES ACT NOTICE (FEDERAL - ALL STATES)
           IF WS-STATE-REQ-SURPRISE
               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  SURPRISE BILLING PROTECTION: '
                   'Under the No Surprises Act, you are'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  protected from unexpected medical '
                   'bills when receiving emergency services,'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  non-emergency services at in-network '
                   'facilities from out-of-network providers,'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  and air ambulance services. You '
                   'should not be balance billed for these.'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
               ADD 5 TO WS-EOB-LINE-CTR
           END-IF

      *    BALANCE BILLING NOTICE
           IF WS-STATE-REQ-BAL-BILL
               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  BALANCE BILLING: In-network providers'
                   ' have agreed to accept the allowed amount'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  as payment in full. They cannot bill '
                   'you for the difference between their'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  charge and the allowed amount. '
                   'Out-of-network providers may balance bill.'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
               ADD 4 TO WS-EOB-LINE-CTR
           END-IF

      *    NETWORK ADEQUACY NOTICE
           IF WS-STATE-REQ-NET-NOTICE
               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  NETWORK: If you need help finding an '
                   'in-network provider, please visit'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  our website at ' WS-PC-WEBSITE
                   ' or call member services.'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
               ADD 3 TO WS-EOB-LINE-CTR
           END-IF

      *    STATE-SPECIFIC CONTENT
           EVALUATE WS-STATE-CODE
               WHEN 'CA'
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  CALIFORNIA RESIDENTS: You may '
                       'contact the California Department of'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  Managed Health Care at '
                       '1-888-466-2219 or visit '
                       'www.dmhc.ca.gov for assistance.'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  Under California law, you have '
                       'additional protections against'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  surprise medical billing per '
                       'AB 72 and AB 1611.'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 4 TO WS-EOB-LINE-CTR

               WHEN 'NY'
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  NEW YORK RESIDENTS: Contact the '
                       'NY Dept of Financial Services at'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  1-800-342-3736 or visit '
                       'www.dfs.ny.gov. Under New York law,'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  you are protected from surprise '
                       'bills under the Emergency Medical'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  Services and Surprise Bills Law.'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 4 TO WS-EOB-LINE-CTR

               WHEN 'TX'
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  TEXAS RESIDENTS: Contact the '
                       'Texas Department of Insurance at'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  1-800-252-3439 or visit '
                       'www.tdi.texas.gov. Under SB 1264,'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  you have protections against '
                       'balance billing in emergencies and'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  from out-of-network providers at '
                       'in-network facilities.'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 4 TO WS-EOB-LINE-CTR

               WHEN 'FL'
                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  FLORIDA RESIDENTS: Contact the '
                       'Florida Office of Insurance'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  Regulation at 1-877-693-5236 or '
                       'visit www.floir.com. Under HB 221,'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  emergency services and certain '
                       'non-emergency services at in-network'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

                   MOVE SPACES TO PATIENT-EOB-LINE
                   STRING '  facilities have balance billing '
                       'protections.'
                       DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                   WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
                   ADD 4 TO WS-EOB-LINE-CTR

               WHEN OTHER
      *            GENERIC STATE COMMISSIONER INFO
                   IF WS-STATE-REQ-COMMISH
                       MOVE SPACES TO PATIENT-EOB-LINE
                       STRING '  STATE INSURANCE DEPT: '
                           'Contact your state insurance '
                           'commissioner at'
                           DELIMITED BY SIZE INTO PATIENT-EOB-LINE
                       WRITE PATIENT-EOB-LINE
                           AFTER ADVANCING 1 LINES

                       IF WS-STATE-COMMISH-PHONE NOT = SPACES
                           MOVE SPACES TO PATIENT-EOB-LINE
                           STRING '  '
                               WS-STATE-COMMISH-PHONE
                               ' or visit '
                               WS-STATE-COMMISH-WEB
                               DELIMITED BY SIZE
                               INTO PATIENT-EOB-LINE
                           WRITE PATIENT-EOB-LINE
                               AFTER ADVANCING 1 LINES
                       END-IF
                       ADD 2 TO WS-EOB-LINE-CTR
                   END-IF
           END-EVALUATE

      *    LANGUAGE ACCESS NOTICE (IF REQUIRED)
           IF WS-STATE-REQ-LANGUAGE
               MOVE SPACES TO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  '
                   WS-STATE-LANG-NOTICE-TEXT
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
               ADD 2 TO WS-EOB-LINE-CTR
           END-IF

      *    GFE RECONCILIATION NOTICE (IF REQUIRED)
           IF WS-STATE-REQ-GFE
               MOVE SPACES TO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  GOOD FAITH ESTIMATE: If you '
                   'received a Good Faith Estimate (GFE)'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  before your service, compare it '
                   'to this EOB. If the final charges are'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES

               MOVE SPACES TO PATIENT-EOB-LINE
               STRING '  $400 or more above the GFE, you '
                   'may be eligible to dispute the bill.'
                   DELIMITED BY SIZE INTO PATIENT-EOB-LINE
               WRITE PATIENT-EOB-LINE AFTER ADVANCING 1 LINES
               ADD 4 TO WS-EOB-LINE-CTR
           END-IF
           .

      *================================================================
      *  6000-SERIES: CARC/RARC CODE MAPPING
      *================================================================

      *================================================================
       6000-MAP-ADJUSTMENT-CODES.
      *================================================================
      *    MAIN ROUTING FOR ADJUSTMENT CODE MAPPING
      *================================================================
           PERFORM 6100-MAP-CARC-TO-DESCRIPTION
           .

      *================================================================
       6100-MAP-CARC-TO-DESCRIPTION.
      *================================================================
      *    MAP CARC CODE TO PLAIN-LANGUAGE DESCRIPTION
      *    SEARCHES 120-ENTRY TABLE FOR MATCHING CODE
      *    RETURNS DESCRIPTION OR 'UNKNOWN REASON CODE' IF NOT FOUND
      *================================================================
           MOVE 'N' TO WS-CARC-FOUND-FLAG
           MOVE SPACES TO WS-CARC-LOOKUP-DESC

      *    SEARCH CARC TABLE
           PERFORM VARYING WS-CARC-INDEX FROM 1 BY 1
               UNTIL WS-CARC-INDEX > WS-CARC-TABLE-SIZE
                  OR WS-CARC-FOUND
               IF WS-CARC-CODE(WS-CARC-INDEX) =
                  WS-CARC-LOOKUP-CODE
                   MOVE 'Y' TO WS-CARC-FOUND-FLAG
                   MOVE WS-CARC-DESCRIPTION(WS-CARC-INDEX)
                       TO WS-CARC-LOOKUP-DESC
               END-IF
           END-PERFORM

      *    IF NOT FOUND IN LOCAL TABLE, TRY DATABASE
           IF WS-CARC-NOT-FOUND
               EXEC SQL
                   SELECT REASON_DESC
                   INTO   :WS-CARC-LOOKUP-DESC
                   FROM   CARC_CODES
                   WHERE  REASON_CODE = :WS-CARC-LOOKUP-CODE
                   AND    ACTIVE_FLAG = 'Y'
               END-EXEC

               IF SQLCODE = 0
                   MOVE 'Y' TO WS-CARC-FOUND-FLAG
               ELSE
                   STRING 'UNKNOWN REASON CODE: '
                       WS-CARC-LOOKUP-CODE
                       DELIMITED BY SIZE
                       INTO WS-CARC-LOOKUP-DESC
               END-IF
           END-IF
           .

      *================================================================
       6200-MAP-RARC-TO-DESCRIPTION.
      *================================================================
      *    MAP RARC CODE TO DESCRIPTION
      *    N-SERIES, M-SERIES, AND MA-SERIES CODES
      *================================================================
           MOVE 'N' TO WS-RARC-FOUND-FLAG
           MOVE SPACES TO WS-RARC-LOOKUP-DESC

      *    SEARCH RARC TABLE
           PERFORM VARYING WS-RARC-INDEX FROM 1 BY 1
               UNTIL WS-RARC-INDEX > WS-RARC-TABLE-SIZE
                  OR WS-RARC-FOUND
               IF WS-RARC-CODE(WS-RARC-INDEX) =
                  WS-RARC-LOOKUP-CODE
                   MOVE 'Y' TO WS-RARC-FOUND-FLAG
                   MOVE WS-RARC-DESCRIPTION(WS-RARC-INDEX)
                       TO WS-RARC-LOOKUP-DESC
               END-IF
           END-PERFORM

      *    IF NOT FOUND IN LOCAL TABLE, TRY DATABASE
           IF WS-RARC-NOT-FOUND
               EXEC SQL
                   SELECT REMARK_DESC
                   INTO   :WS-RARC-LOOKUP-DESC
                   FROM   RARC_CODES
                   WHERE  REMARK_CODE = :WS-RARC-LOOKUP-CODE
                   AND    ACTIVE_FLAG = 'Y'
               END-EXEC

               IF SQLCODE = 0
                   MOVE 'Y' TO WS-RARC-FOUND-FLAG
               ELSE
                   STRING 'UNKNOWN REMARK CODE: '
                       WS-RARC-LOOKUP-CODE
                       DELIMITED BY SIZE
                       INTO WS-RARC-LOOKUP-DESC
               END-IF
           END-IF
           .

      *================================================================
       6300-MAP-GROUP-CODE-DESC.
      *================================================================
      *    MAP ADJUSTMENT GROUP CODE (CO/PR/PI/OA/CR) TO DESCRIPTION
      *================================================================
           MOVE SPACES TO WS-GRP-LOOKUP-DESC

           PERFORM VARYING WS-GRP-INDEX FROM 1 BY 1
               UNTIL WS-GRP-INDEX > 5
               IF WS-GRP-CODE(WS-GRP-INDEX) = WS-GRP-LOOKUP-CODE
                   MOVE WS-GRP-DESCRIPTION(WS-GRP-INDEX)
                       TO WS-GRP-LOOKUP-DESC
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-GRP-LOOKUP-DESC = SPACES
               STRING 'UNKNOWN GROUP CODE: ' WS-GRP-LOOKUP-CODE
                   DELIMITED BY SIZE INTO WS-GRP-LOOKUP-DESC
           END-IF
           .

      *================================================================
      *  7000-SERIES: FINANCIAL RECONCILIATION
      *================================================================

      *================================================================
       7000-RECONCILE-PAYMENT-RUN.
      *================================================================
      *    VERIFY TOTAL CLAIMS PAID = TOTAL CHECKS/EFTS ISSUED
      *    CONTROL TOTALS, HASH TOTALS, RECORD COUNTS
      *    OUT-OF-BALANCE DETECTION AND REPORTING
      *================================================================
      *    CLOSE 835 ENVELOPE SEGMENTS
      *    GE - FUNCTIONAL GROUP TRAILER
           MOVE WS-TRANS-SET-COUNT TO WS-GE01-TRANS-CNT
           MOVE WS-GS-CONTROL-NUM  TO WS-GE02-GROUP-CTRL
           STRING WS-835-GE-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER

      *    IEA - INTERCHANGE TRAILER
           MOVE 1 TO WS-IEA01-GROUP-CNT
           MOVE WS-ISA-CONTROL-NUM TO WS-IEA02-CONTROL-NUM
           STRING WS-835-IEA-SEGMENT WS-EDI-SEGMENT-TERM
               DELIMITED BY SIZE INTO WS-EDI-OUTPUT-BUFFER
           WRITE EDI-835-RECORD FROM WS-EDI-OUTPUT-BUFFER

      *    CLOSE EFT FILE WITH FILE CONTROL RECORD
           MOVE SPACES TO WS-NACHA-FILE-CONTROL
           MOVE '9' TO WS-NFC-RECORD-TYPE
           MOVE WS-EFT-BATCH-NUMBER TO WS-NFC-BATCH-COUNT

      *    CALCULATE BLOCK COUNT (ROUND UP TO NEAREST 10)
           COMPUTE WS-EFT-BLOCK-COUNT =
               (WS-EFT-FILE-ENTRY-CNT * 4
              + WS-EFT-BATCH-NUMBER * 2 + 2 + 9) / 10
           MOVE WS-EFT-BLOCK-COUNT TO WS-NFC-BLOCK-COUNT

           MOVE WS-EFT-FILE-ENTRY-CNT TO WS-NFC-ENTRY-COUNT
           MOVE WS-EFT-FILE-HASH      TO WS-NFC-ENTRY-HASH
           MOVE WS-EFT-FILE-DEBIT-TOT  TO WS-NFC-TOTAL-DEBIT
           MOVE WS-EFT-FILE-CREDIT-TOT TO WS-NFC-TOTAL-CREDIT

           WRITE EFT-NACHA-RECORD FROM WS-NACHA-FILE-CONTROL

      *    RECONCILE TOTALS
           COMPUTE WS-RECON-OUT-OF-BAL =
               WS-RECON-TOTAL-PAID-AMT
             - WS-RECON-TOTAL-CHK-AMT
             - WS-RECON-TOTAL-EFT-AMT

      *    ACCOUNT FOR DEFERRED (BELOW-MIN) AND NEGATIVE BALANCES
      *    THESE ARE LEGITIMATE DIFFERENCES
           IF WS-RECON-OUT-OF-BAL NOT = ZEROS
      *        CHECK IF DIFFERENCE IS EXPLAINED
               IF FUNCTION ABS(WS-RECON-OUT-OF-BAL) > 0.01
                   MOVE 'N' TO WS-RECON-IN-BALANCE
               END-IF
           END-IF

      *    GENERATE RECONCILIATION REPORT
           PERFORM 7100-GENERATE-PAYMENT-SUMMARY

      *    WRITE RECONCILIATION REPORT
           ADD 1 TO WS-RECON-PAGE-CTR
           MOVE SPACES TO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER PAGE-EJECT

           STRING 'PAYMENT RUN RECONCILIATION REPORT'
               '          DATE: ' WS-CCYYMMDD-DASH
               '          PAGE: ' WS-RECON-PAGE-CTR
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           MOVE ALL '=' TO RECON-REPORT-LINE(1:80)
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-RECON-TOTAL-PAID-AMT TO WS-EDIT-AMOUNT
           MOVE SPACES TO RECON-REPORT-LINE
           STRING 'TOTAL CLAIMS PAID AMOUNT:       $'
               WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-RECON-TOTAL-CHK-AMT TO WS-EDIT-AMOUNT
           MOVE SPACES TO RECON-REPORT-LINE
           STRING 'TOTAL CHECK AMOUNT:             $'
               WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-RECON-TOTAL-EFT-AMT TO WS-EDIT-AMOUNT
           MOVE SPACES TO RECON-REPORT-LINE
           STRING 'TOTAL EFT AMOUNT:               $'
               WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           MOVE ALL '-' TO RECON-REPORT-LINE(1:60)
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-RECON-OUT-OF-BAL TO WS-EDIT-AMOUNT
           MOVE SPACES TO RECON-REPORT-LINE
           STRING 'OUT-OF-BALANCE AMOUNT:          $'
               WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           IF WS-RECON-NOT-BALANCED
               STRING RECON-REPORT-LINE
                   '  *** OUT OF BALANCE ***'
                   DELIMITED BY SIZE INTO RECON-REPORT-LINE
           ELSE
               STRING RECON-REPORT-LINE
                   '  (IN BALANCE)'
                   DELIMITED BY SIZE INTO RECON-REPORT-LINE
           END-IF
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           STRING 'RECORD COUNTS:'
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           STRING '  TOTAL CLAIMS:     ' WS-RECON-TOTAL-CLAIMS
               '    CHECKS ISSUED: ' WS-RECON-TOTAL-CHECKS
               '    EFTS ISSUED: ' WS-RECON-TOTAL-EFTS
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO RECON-REPORT-LINE
           STRING '  ZERO-PAY CLAIMS: ' WS-RECON-ZERO-PAYS
               '    DENIED CLAIMS: ' WS-RECON-DENIED-CNT
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

           MOVE WS-RECON-HASH-TOTAL TO WS-EDIT-AMOUNT
           MOVE SPACES TO RECON-REPORT-LINE
           STRING 'HASH TOTAL:                     $'
               WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO RECON-REPORT-LINE
           WRITE RECON-REPORT-LINE AFTER ADVANCING 1 LINES

      *    LOG OUT-OF-BALANCE TO ERROR FILE
           IF WS-RECON-NOT-BALANCED
               MOVE 'E' TO ER-SEVERITY
               STRING 'PAYMENT RUN OUT OF BALANCE BY $'
                   WS-RECON-OUT-OF-BAL
                   DELIMITED BY SIZE INTO ER-ERROR-DESC
               PERFORM 8000-ERROR-HANDLER
           END-IF
           .

      *================================================================
       7100-GENERATE-PAYMENT-SUMMARY.
      *================================================================
      *    GENERATE PAYMENT RUN SUMMARY REPORT
      *    TOTALS BY PAYER, BY LINE OF BUSINESS, BY PAYMENT METHOD
      *================================================================
           ADD 1 TO WS-SUMMARY-PAGE-CTR
           MOVE SPACES TO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER PAGE-EJECT

           STRING 'PAYMENT RUN SUMMARY REPORT'
               '              DATE: ' WS-CCYYMMDD-DASH
               '              PAGE: ' WS-SUMMARY-PAGE-CTR
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           MOVE ALL '=' TO PAY-SUMMARY-LINE(1:80)
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

      *    TOTALS BY PAYER
           MOVE SPACES TO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           STRING 'TOTALS BY PAYER:'
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  PAYER ID   CLAIMS     PAID AMOUNT'
               '      CHECKS  EFTS'
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           MOVE ALL '-' TO PAY-SUMMARY-LINE(1:80)
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-STAT-PAYER-COUNT
               MOVE WS-SBP-PAID-AMT(WS-WORK-INDEX)
                   TO WS-EDIT-AMOUNT
               MOVE SPACES TO PAY-SUMMARY-LINE
               STRING '  '
                   WS-SBP-PAYER-ID(WS-WORK-INDEX)
                   '  '
                   WS-SBP-CLAIM-COUNT(WS-WORK-INDEX)
                   '  $'
                   WS-EDIT-AMOUNT
                   '  '
                   WS-SBP-CHECK-COUNT(WS-WORK-INDEX)
                   '  '
                   WS-SBP-EFT-COUNT(WS-WORK-INDEX)
                   DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
               WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES
           END-PERFORM

      *    TOTALS BY LINE OF BUSINESS
           MOVE SPACES TO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 2 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING 'TOTALS BY LINE OF BUSINESS:'
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  LOB CODE  LOB NAME                 '
               'CLAIMS     PAID AMOUNT'
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           MOVE ALL '-' TO PAY-SUMMARY-LINE(1:80)
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 7
               IF WS-SBL-CLAIM-COUNT(WS-WORK-INDEX) > 0
                   MOVE WS-SBL-PAID-AMT(WS-WORK-INDEX)
                       TO WS-EDIT-AMOUNT
                   MOVE SPACES TO PAY-SUMMARY-LINE
                   STRING '  '
                       WS-SBL-LOB-CODE(WS-WORK-INDEX)
                       '       '
                       WS-SBL-LOB-NAME(WS-WORK-INDEX)
                       '  '
                       WS-SBL-CLAIM-COUNT(WS-WORK-INDEX)
                       '  $'
                       WS-EDIT-AMOUNT
                       DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
                   WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES
               END-IF
           END-PERFORM

      *    TOTALS BY PAYMENT METHOD
           MOVE SPACES TO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 2 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING 'TOTALS BY PAYMENT METHOD:'
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE WS-STAT-TOTAL-CHK-DOLLARS TO WS-EDIT-AMOUNT
           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  CHECKS:  COUNT: '
               WS-STAT-CHECKS-GENERATED
               '    AMOUNT: $' WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE WS-STAT-TOTAL-EFT-DOLLARS TO WS-EDIT-AMOUNT
           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  EFTS:    COUNT: '
               WS-STAT-EFTS-GENERATED
               '    AMOUNT: $' WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE WS-STAT-TOTAL-DOLLARS TO WS-EDIT-AMOUNT
           MOVE SPACES TO PAY-SUMMARY-LINE
           MOVE ALL '=' TO PAY-SUMMARY-LINE(1:60)
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  TOTAL:   COUNT: '
               WS-STAT-CHECKS-GENERATED
               WS-STAT-EFTS-GENERATED
               '    AMOUNT: $' WS-EDIT-AMOUNT
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

      *    ZERO-PAY AND DENIED SUMMARY
           MOVE SPACES TO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 2 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  ZERO-PAY CLAIMS: ' WS-STAT-CLAIMS-ZEROPAY
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  DENIED CLAIMS:   ' WS-STAT-CLAIMS-DENIED
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES

           MOVE SPACES TO PAY-SUMMARY-LINE
           STRING '  REVERSED CLAIMS: ' WS-STAT-CLAIMS-REVERSED
               DELIMITED BY SIZE INTO PAY-SUMMARY-LINE
           WRITE PAY-SUMMARY-LINE AFTER ADVANCING 1 LINES
           .

      *================================================================
      *  8000-SERIES: ERROR HANDLING AND AUDIT
      *================================================================

      *================================================================
       8000-ERROR-HANDLER.
      *================================================================
      *    CENTRAL ERROR HANDLER - LOG ERRORS TO ERROR FILE AND
      *    AUDIT TRAIL, TRACK ERROR/WARNING COUNTS
      *================================================================
           MOVE WS-TIMESTAMP      TO ER-TIMESTAMP
           MOVE WS-PROGRAM-ID     TO ER-PROGRAM

           IF ER-PARAGRAPH = SPACES
               MOVE WS-DB-PARAGRAPH-NAME TO ER-PARAGRAPH
           END-IF

           IF ER-CLAIM-NUMBER = SPACES
               MOVE AC-CLAIM-NUMBER TO ER-CLAIM-NUMBER
           END-IF

           WRITE ERROR-RECORD

      *    WRITE TO AUDIT TRAIL
           MOVE WS-TIMESTAMP               TO AT-TIMESTAMP
           MOVE 'ERROR'                     TO AT-ACTION
           MOVE 'CLAIM'                     TO AT-ENTITY-TYPE
           MOVE ER-CLAIM-NUMBER             TO AT-ENTITY-KEY
           MOVE ER-SEVERITY                 TO AT-OLD-VALUE
           MOVE ER-ERROR-CODE               TO AT-NEW-VALUE
           MOVE 'BATCH'                     TO AT-USER-ID
           MOVE WS-PROGRAM-ID               TO AT-PROGRAM
           MOVE ER-PARAGRAPH                TO AT-PARAGRAPH
           MOVE ER-ERROR-DESC(1:78)         TO AT-DESCRIPTION
           WRITE AUDIT-RECORD

      *    UPDATE COUNTERS
           EVALUATE TRUE
               WHEN ER-ERROR
                   ADD 1 TO WS-STAT-ERRORS
               WHEN ER-WARNING
                   ADD 1 TO WS-STAT-WARNINGS
               WHEN ER-FATAL
                   ADD 1 TO WS-STAT-ERRORS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

      *    RESET ERROR RECORD FOR NEXT USE
           MOVE SPACES TO ER-PARAGRAPH
           MOVE SPACES TO ER-CLAIM-NUMBER
           MOVE SPACES TO ER-ERROR-CODE
           MOVE SPACES TO ER-ERROR-DESC
           .

      *================================================================
       8100-DATABASE-ERROR.
      *================================================================
      *    HANDLE DATABASE ERRORS WITH DEADLOCK RETRY LOGIC
      *    LOG SQLCODE, SQLERRM, AND CONTEXT INFORMATION
      *================================================================
           MOVE 'Y' TO WS-DB-ERROR-FLAG

      *    CHECK FOR DEADLOCK (SQLCODE 1205 IN SYBASE)
           IF SQLCODE = WS-DB-DEADLOCK-CODE
               MOVE 'Y' TO WS-DEADLOCK-FLAG
               ADD 1 TO WS-DB-DEADLOCK-RETRIES

               IF WS-DB-DEADLOCK-RETRIES <= WS-DB-MAX-RETRIES
      *            RETRY AFTER BRIEF PAUSE
                   MOVE 'W' TO ER-SEVERITY
                   STRING 'DEADLOCK DETECTED IN '
                       WS-DB-PARAGRAPH-NAME
                       ' - RETRY ' WS-DB-DEADLOCK-RETRIES
                       ' OF ' WS-DB-MAX-RETRIES
                       DELIMITED BY SIZE INTO ER-ERROR-DESC
                   PERFORM 8000-ERROR-HANDLER
                   EXEC SQL ROLLBACK END-EXEC
               ELSE
      *            MAX RETRIES EXCEEDED
                   MOVE 'E' TO ER-SEVERITY
                   STRING 'DEADLOCK MAX RETRIES IN '
                       WS-DB-PARAGRAPH-NAME
                       ' SQLCODE=' SQLCODE
                       DELIMITED BY SIZE INTO ER-ERROR-DESC
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
      *        NON-DEADLOCK DATABASE ERROR
               MOVE 'E' TO ER-SEVERITY
               STRING 'DB ERROR IN ' WS-DB-PARAGRAPH-NAME
                   ' SQLCODE=' SQLCODE
                   DELIMITED BY SIZE INTO ER-ERROR-DESC
               PERFORM 8000-ERROR-HANDLER

      *        FOR CERTAIN FATAL ERRORS, TERMINATE
               IF SQLCODE = -911
               OR SQLCODE = -913
               OR SQLCODE < -9000
                   MOVE 'F' TO ER-SEVERITY
                   STRING 'FATAL DB ERROR - TERMINATING'
                       ' SQLCODE=' SQLCODE
                       DELIMITED BY SIZE INTO ER-ERROR-DESC
                   PERFORM 8000-ERROR-HANDLER
                   PERFORM 9000-TERMINATION
                   MOVE 16 TO WS-RETURN-CODE
                   STOP RUN
               END-IF
           END-IF

           MOVE 'N' TO WS-DB-ERROR-FLAG
           .

      *================================================================
      *  9000-SERIES: TERMINATION
      *================================================================

      *================================================================
       9000-TERMINATION.
      *================================================================
      *    CLOSE ALL FILES, PRINT COMPREHENSIVE STATISTICS,
      *    WRITE FINAL AUDIT TRAIL ENTRY, SET RETURN CODE
      *================================================================
      *    UPDATE FINAL CONTROL NUMBERS BACK TO DATABASE
           MOVE '9000-TERMINATION' TO WS-DB-PARAGRAPH-NAME

           EXEC SQL
               UPDATE PAYMENT_CONTROL
               SET    LAST_CHECK_NUMBER = :WS-NEXT-CHECK-NUMBER,
                      LAST_ISA_CONTROL = :WS-ISA-CONTROL-NUM,
                      LAST_GS_CONTROL = :WS-GS-CONTROL-NUM,
                      LAST_ST_CONTROL = :WS-ST-CONTROL-NUM,
                      LAST_EFT_TRACE = :WS-EFT-TRACE-NUMBER,
                      LAST_RUN_STATUS = CASE
                          WHEN :WS-STAT-ERRORS > 0
                          THEN 'ERRORS'
                          ELSE 'SUCCESS'
                      END,
                      LAST_RUN_END_DATE = GETDATE(),
                      LAST_CLAIMS_COUNT =
                          :WS-STAT-CLAIMS-PROCESSED,
                      LAST_TOTAL_DOLLARS =
                          :WS-STAT-TOTAL-DOLLARS
               WHERE  CONTROL_TYPE = 'REMIT'
               AND    ACTIVE_FLAG = 'Y'
           END-EXEC

           IF SQLCODE = 0
               EXEC SQL COMMIT END-EXEC
           ELSE
               PERFORM 8100-DATABASE-ERROR
           END-IF

      *    WRITE FINAL AUDIT TRAIL ENTRY
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
                                         WS-CURRENT-TIME
           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
                  ' ' WS-CURR-HOUR ':' WS-CURR-MINUTE ':'
                  WS-CURR-SECOND '.' WS-CURR-HUNDREDTH '0000'
               DELIMITED BY SIZE INTO WS-TIMESTAMP

           MOVE WS-TIMESTAMP               TO AT-TIMESTAMP
           MOVE 'COMPLETE'                  TO AT-ACTION
           MOVE 'PROGRAM'                   TO AT-ENTITY-TYPE
           MOVE 'HCREMIT'                   TO AT-ENTITY-KEY
           MOVE SPACES                      TO AT-OLD-VALUE
           MOVE WS-STAT-CLAIMS-PROCESSED    TO AT-NEW-VALUE
           MOVE 'BATCH'                     TO AT-USER-ID
           MOVE WS-PROGRAM-ID               TO AT-PROGRAM
           MOVE '9000-TERMINATION'          TO AT-PARAGRAPH
           STRING 'COMPLETED - '
               WS-STAT-CLAIMS-PROCESSED ' CLAIMS $'
               WS-STAT-TOTAL-DOLLARS
               DELIMITED BY SIZE INTO AT-DESCRIPTION
           WRITE AUDIT-RECORD

      *    DISPLAY COMPREHENSIVE STATISTICS
           DISPLAY '**********************************************'
           DISPLAY '* HCREMIT - REMITTANCE PROCESSING COMPLETE  *'
           DISPLAY '**********************************************'
           DISPLAY ' '
           DISPLAY 'PROCESSING STATISTICS:'
           DISPLAY '  RECORDS READ:          '
               WS-STAT-RECORDS-READ
           DISPLAY '  CLAIMS PROCESSED:      '
               WS-STAT-CLAIMS-PROCESSED
           DISPLAY '    PAID:                '
               WS-STAT-CLAIMS-PAID
           DISPLAY '    DENIED:              '
               WS-STAT-CLAIMS-DENIED
           DISPLAY '    ZERO-PAY:            '
               WS-STAT-CLAIMS-ZEROPAY
           DISPLAY '    REVERSED:            '
               WS-STAT-CLAIMS-REVERSED
           DISPLAY '    ADJUSTED:            '
               WS-STAT-CLAIMS-ADJUSTED
           DISPLAY ' '
           DISPLAY 'PAYMENT STATISTICS:'
           DISPLAY '  CHECKS GENERATED:      '
               WS-STAT-CHECKS-GENERATED
           DISPLAY '  EFTS GENERATED:        '
               WS-STAT-EFTS-GENERATED
           DISPLAY '  835 TRANSACTION SETS:  '
               WS-STAT-835-TRANS-SETS
           DISPLAY '  EOBS GENERATED:        '
               WS-STAT-EOBS-GENERATED
           DISPLAY '  REMITS GENERATED:      '
               WS-STAT-REMITS-GENERATED
           DISPLAY ' '
           DISPLAY 'FINANCIAL TOTALS:'
           DISPLAY '  TOTAL DOLLARS:         $'
               WS-STAT-TOTAL-DOLLARS
           DISPLAY '  TOTAL CHECK DOLLARS:   $'
               WS-STAT-TOTAL-CHK-DOLLARS
           DISPLAY '  TOTAL EFT DOLLARS:     $'
               WS-STAT-TOTAL-EFT-DOLLARS
           DISPLAY ' '
           DISPLAY 'EXCEPTION STATISTICS:'
           DISPLAY '  ERRORS:                '
               WS-STAT-ERRORS
           DISPLAY '  WARNINGS:              '
               WS-STAT-WARNINGS
           DISPLAY '  NEG BALANCE OFFSETS:   '
               WS-STAT-NEG-BAL-OFFSETS
           DISPLAY '  SPLIT CHECKS:          '
               WS-STAT-SPLIT-CHECKS
           DISPLAY '  BELOW-MIN DEFERRED:    '
               WS-STAT-BELOW-MIN-DEFER
           DISPLAY '  VOID/REISSUES:         '
               WS-STAT-VOID-REISSUES
           DISPLAY '  PRENOTES SENT:         '
               WS-STAT-PRENOTES
           DISPLAY ' '

      *    DISPLAY PAYER BREAKDOWN
           DISPLAY 'BREAKDOWN BY PAYER:'
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-STAT-PAYER-COUNT
               DISPLAY '  PAYER: '
                   WS-SBP-PAYER-ID(WS-WORK-INDEX)
                   '  CLAIMS: '
                   WS-SBP-CLAIM-COUNT(WS-WORK-INDEX)
                   '  PAID: $'
                   WS-SBP-PAID-AMT(WS-WORK-INDEX)
                   '  CHK: '
                   WS-SBP-CHECK-COUNT(WS-WORK-INDEX)
                   '  EFT: '
                   WS-SBP-EFT-COUNT(WS-WORK-INDEX)
           END-PERFORM

      *    DISPLAY LOB BREAKDOWN
           DISPLAY ' '
           DISPLAY 'BREAKDOWN BY LINE OF BUSINESS:'
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 7
               IF WS-SBL-CLAIM-COUNT(WS-WORK-INDEX) > 0
                   DISPLAY '  '
                       WS-SBL-LOB-CODE(WS-WORK-INDEX) ' - '
                       WS-SBL-LOB-NAME(WS-WORK-INDEX)
                       '  CLAIMS: '
                       WS-SBL-CLAIM-COUNT(WS-WORK-INDEX)
                       '  PAID: $'
                       WS-SBL-PAID-AMT(WS-WORK-INDEX)
               END-IF
           END-PERFORM

      *    RECONCILIATION STATUS
           DISPLAY ' '
           IF WS-RECON-BALANCED
               DISPLAY 'RECONCILIATION: *** IN BALANCE ***'
           ELSE
               DISPLAY 'RECONCILIATION: *** OUT OF BALANCE ***'
               DISPLAY '  DIFFERENCE: $' WS-RECON-OUT-OF-BAL
           END-IF
           DISPLAY ' '
           DISPLAY '**********************************************'

      *    CLOSE ALL FILES
           CLOSE ADJUD-CLAIMS-FILE
           CLOSE EDI-835-OUTPUT-FILE
           CLOSE PROVIDER-REMIT-REPORT
           CLOSE PATIENT-EOB-OUTPUT
           CLOSE CHECK-REGISTER-FILE
           CLOSE EFT-OUTPUT-FILE
           CLOSE POSITIVE-PAY-FILE
           CLOSE PAYMENT-SUMMARY-REPORT
           CLOSE ERROR-FILE
           CLOSE AUDIT-TRAIL-FILE
           CLOSE RECON-REPORT-FILE

      *    SET RETURN CODE
           IF WS-STAT-ERRORS > 0
               MOVE 8 TO WS-RETURN-CODE
           ELSE
               IF WS-STAT-WARNINGS > 0
                   MOVE 4 TO WS-RETURN-CODE
               ELSE
                   MOVE 0 TO WS-RETURN-CODE
               END-IF
           END-IF

           MOVE WS-RETURN-CODE TO RETURN-CODE
           .
