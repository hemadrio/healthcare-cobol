       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HCCLMADJ.
       AUTHOR.        CLAIMS ADJUDICATION TEAM.
       INSTALLATION.  NATIONAL HEALTH SYSTEMS INC.
       DATE-WRITTEN.  1994-03-15.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM:  HCCLMADJ                                            *
      * TITLE:    HEALTHCARE CLAIMS ADJUDICATION ENGINE                *
      * SYSTEM:   CLAIMS PROCESSING SYSTEM (CPS) - BATCH MODULE       *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   CORE BATCH CLAIMS ADJUDICATION ENGINE FOR HEALTHCARE         *
      *   PAYER PROCESSING. READS INBOUND CLAIMS, APPLIES PRICING,    *
      *   BENEFIT DETERMINATION, COORDINATION OF BENEFITS, AND        *
      *   GENERATES PAYMENT OR PEND DISPOSITIONS.                     *
      *                                                                *
      * INPUT FILES:                                                   *
      *   CLMINFL  - INBOUND CLAIMS (FORMATTED FROM 837 TRANSLATOR)   *
      *   REFDATA  - REFERENCE DATA FLAT FILE (BACKUP IF DB DOWN)     *
      *                                                                *
      * OUTPUT FILES:                                                  *
      *   ADJOUTFL - ADJUDICATED CLAIMS OUTPUT                        *
      *   PENDFL   - PENDED CLAIMS FOR MANUAL REVIEW                  *
      *   APPEALFL - APPEAL TRACKING FILE                             *
      *   PAYFL    - PAYMENT REMITTANCE FILE (835 INPUT)              *
      *   ERRFL    - ERROR/REJECT FILE                                *
      *   RPTFL    - BATCH PROCESSING REPORT                          *
      *   AUDITFL  - AUDIT TRAIL FILE                                 *
      *                                                                *
      * DATABASE:                                                      *
      *   SYBASE ASE 15.7 - CLAIMS PROCESSING DATABASE (CLMPROCDB)   *
      *                                                                *
      * MODIFICATION HISTORY:                                          *
      * ------------------------------------------------------------ *
      * DATE       AUTHOR       CHG#     DESCRIPTION                  *
      * ---------- ------------ -------- ---------------------------- *
      * 1994-03-15 R.MORRISON   CPS-001  INITIAL CREATION             *
      * 1994-07-20 R.MORRISON   CPS-018  ADD FEE SCHEDULE PRICING     *
      * 1994-11-05 T.NAKAMURA   CPS-032  ADD DRG PRICING MODULE       *
      * 1995-02-14 R.MORRISON   CPS-045  ADD COB PROCESSING           *
      * 1995-06-30 S.PATEL      CPS-067  ADD CAPITATION DETECTION     *
      * 1995-09-12 T.NAKAMURA   CPS-078  MEDICARE SECONDARY PAYER     *
      * 1996-01-22 D.WASHINGTON CPS-091  ADD PER DIEM PRICING         *
      * 1996-05-18 S.PATEL      CPS-104  PROMPT PAY INTEREST CALC     *
      * 1996-08-30 R.MORRISON   CPS-112  FAMILY DEDUCTIBLE LOGIC      *
      * 1997-01-15 D.WASHINGTON CPS-129  ADD CASE RATE PRICING        *
      * 1997-04-22 T.NAKAMURA   CPS-138  OUTLIER PAYMENT CALC         *
      * 1997-09-10 S.PATEL      CPS-152  WITHHOLD CALCULATION         *
      * 1998-03-05 R.MORRISON   CPS-171  Y2K DATE REMEDIATION         *
      * 1998-07-20 D.WASHINGTON CPS-185  ADD APPEAL FILE OUTPUT       *
      * 1999-01-12 T.NAKAMURA   CPS-198  CONCURRENT ACCUM RESERVE     *
      * 1999-06-30 S.PATEL      CPS-210  OOP MAXIMUM PROCESSING       *
      * 2000-02-15 R.MORRISON   CPS-225  ADD AUDIT TRAIL FILE         *
      * 2000-08-22 D.WASHINGTON CPS-241  DEADLOCK RETRY LOGIC         *
      * 2001-03-10 T.NAKAMURA   CPS-258  MODIFIER 59 PROCESSING      *
      * 2001-09-18 S.PATEL      CPS-274  STEP-DOWN PER DIEM           *
      * 2002-04-05 R.MORRISON   CPS-290  DSH ADJUSTMENT               *
      * 2002-10-20 D.WASHINGTON CPS-305  IME ADJUSTMENT               *
      * 2003-03-15 T.NAKAMURA   CPS-318  EMBEDDED DEDUCTIBLE          *
      * 2003-08-28 S.PATEL      CPS-332  CARRYOVER DEDUCTIBLE Q4      *
      * 2004-02-12 R.MORRISON   CPS-348  NEW TECH ADD-ON PAYMENTS     *
      * 2004-07-30 D.WASHINGTON CPS-362  TRANSFER DRG ADJUSTMENT      *
      * 2005-01-18 J.CHEN       CPS-379  MENTAL HEALTH PARITY         *
      * 2005-06-22 T.NAKAMURA   CPS-394  BILATERAL PROCEDURE MOD      *
      * 2006-02-10 S.PATEL      CPS-410  STATE PROMPT PAY UPDATES     *
      * 2006-09-15 J.CHEN       CPS-428  ASSISTANT SURGEON PRICING    *
      * 2007-04-20 R.MORRISON   CPS-445  COBRA SECONDARY RULE         *
      * 2007-11-08 D.WASHINGTON CPS-462  SITE OF SERVICE DIFF         *
      * 2008-05-30 T.NAKAMURA   CPS-478  ESRD COORDINATION PERIOD     *
      * 2008-12-15 J.CHEN       CPS-495  UNLISTED PROCEDURE CODES     *
      * 2009-06-22 S.PATEL      CPS-512  NAIC COB MODEL UPDATE        *
      * 2010-01-04 R.MORRISON   CPS-530  ACA COMPLIANCE - NO LIFE MAX *
      * 2010-07-18 D.WASHINGTON CPS-548  PREVENTIVE CARE NO COST SHR  *
      * 2011-03-10 J.CHEN       CPS-565  ESSENTIAL HEALTH BENEFITS    *
      * 2012-08-22 T.NAKAMURA   CPS-582  ICD-10 CONVERSION SUPPORT    *
      * 2013-04-15 S.PATEL      CPS-601  PEDIATRIC DENTAL/VISION      *
      * 2014-01-20 J.CHEN       CPS-618  MARKETPLACE PLAN TIERS       *
      * 2015-06-30 R.MORRISON   CPS-635  READMISSION PENALTY LOGIC    *
      * 2016-11-12 D.WASHINGTON CPS-652  MACRA/MIPS ADJUSTMENTS       *
      * 2017-05-28 T.NAKAMURA   CPS-670  TELEHEALTH PRICING           *
      * 2018-09-14 J.CHEN       CPS-688  SURPRISE BILLING PROTECT     *
      * 2019-03-20 S.PATEL      CPS-705  GENDER AFFIRMING CARE COV    *
      * 2020-04-01 R.MORRISON   CPS-722  COVID-19 EMERGENCY RULES     *
      * 2020-12-15 J.CHEN       CPS-738  COVID VACCINE NO COST SHARE  *
      * 2021-06-30 D.WASHINGTON CPS-755  NO SURPRISES ACT PHASE 1     *
      * 2022-01-15 T.NAKAMURA   CPS-772  NO SURPRISES ACT PHASE 2     *
      * 2022-08-20 S.PATEL      CPS-789  MENTAL HEALTH PARITY UPDT    *
      * 2023-03-10 J.CHEN       CPS-806  PRICE TRANSPARENCY FINAL     *
      * 2023-09-15 R.MORRISON   CPS-823  ANNUAL MAINTENANCE 2024      *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-3090 WITH DEBUGGING MODE.
       OBJECT-COMPUTER.  IBM-3090.
       SPECIAL-NAMES.
           C01 IS PAGE-TOP.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLAIM-INPUT-FILE
               ASSIGN TO CLMINFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CLMIN-STATUS.

           SELECT REFERENCE-DATA-FILE
               ASSIGN TO REFDATA
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS REF-DATA-KEY
               FILE STATUS IS WS-REFDATA-STATUS.

           SELECT ADJUDICATED-OUTPUT-FILE
               ASSIGN TO ADJOUTFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ADJOUT-STATUS.

           SELECT PEND-FILE
               ASSIGN TO PENDFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PEND-STATUS.

           SELECT APPEAL-FILE
               ASSIGN TO APPEALFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-APPEAL-STATUS.

           SELECT PAYMENT-FILE
               ASSIGN TO PAYFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PAY-STATUS.

           SELECT ERROR-FILE
               ASSIGN TO ERRFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERR-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO RPTFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO AUDITFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CLAIM-INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  CLAIM-INPUT-RECORD.
           05  CI-RECORD-TYPE              PIC X(02).
               88  CI-IS-HEADER            VALUE 'HD'.
               88  CI-IS-DETAIL            VALUE 'DT'.
               88  CI-IS-TRAILER           VALUE 'TR'.
           05  CI-CLAIM-ID                 PIC X(15).
           05  CI-CLAIM-DATA               PIC X(983).

       FD  REFERENCE-DATA-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  REFERENCE-DATA-RECORD.
           05  REF-DATA-KEY.
               10  REF-TABLE-CODE          PIC X(03).
               10  REF-LOOKUP-KEY          PIC X(20).
           05  REF-DATA-VALUE              PIC X(477).

       FD  ADJUDICATED-OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  ADJUDICATED-OUTPUT-RECORD       PIC X(1500).

       FD  PEND-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  PEND-OUTPUT-RECORD              PIC X(800).

       FD  APPEAL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  APPEAL-OUTPUT-RECORD            PIC X(600).

       FD  PAYMENT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  PAYMENT-OUTPUT-RECORD           PIC X(500).

       FD  ERROR-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  ERROR-OUTPUT-RECORD             PIC X(400).

       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  REPORT-OUTPUT-RECORD            PIC X(133).

       FD  AUDIT-TRAIL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  AUDIT-TRAIL-RECORD              PIC X(500).

       WORKING-STORAGE SECTION.

      *================================================================*
      * COPYBOOK INCLUDES                                              *
      *================================================================*
           COPY CPYCLMHD.
           COPY CPYCLMLN.
           COPY CPYPATIN.
           COPY CPYPROVD.
           COPY CPYELIG.
           COPY CPYSQLCA.
           COPY CPYERROR.

      *================================================================*
      * FILE STATUS FIELDS                                             *
      *================================================================*
       01  WS-FILE-STATUS-FIELDS.
           05  WS-CLMIN-STATUS             PIC X(02).
               88  CLMIN-OK                VALUE '00'.
               88  CLMIN-EOF               VALUE '10'.
               88  CLMIN-ERROR             VALUE '30' '35' '37'
                                                 '41' '42' '43'
                                                 '44' '46' '47' '48'.
           05  WS-REFDATA-STATUS           PIC X(02).
               88  REFDATA-OK              VALUE '00'.
               88  REFDATA-NOT-FOUND       VALUE '23'.
               88  REFDATA-ERROR           VALUE '30' '35' '37'.
           05  WS-ADJOUT-STATUS            PIC X(02).
               88  ADJOUT-OK               VALUE '00'.
               88  ADJOUT-ERROR            VALUE '30' '35' '37'
                                                 '41' '42' '48'.
           05  WS-PEND-STATUS              PIC X(02).
               88  PEND-OK                 VALUE '00'.
               88  PEND-ERROR              VALUE '30' '35' '37'.
           05  WS-APPEAL-STATUS            PIC X(02).
               88  APPEAL-OK               VALUE '00'.
               88  APPEAL-ERROR            VALUE '30' '35' '37'.
           05  WS-PAY-STATUS               PIC X(02).
               88  PAY-OK                  VALUE '00'.
               88  PAY-ERROR               VALUE '30' '35' '37'.
           05  WS-ERR-STATUS               PIC X(02).
               88  ERR-OK                  VALUE '00'.
               88  ERR-ERROR               VALUE '30' '35' '37'.
           05  WS-RPT-STATUS               PIC X(02).
               88  RPT-OK                  VALUE '00'.
               88  RPT-ERROR               VALUE '30' '35' '37'.
           05  WS-AUDIT-STATUS             PIC X(02).
               88  AUDIT-OK               VALUE '00'.
               88  AUDIT-ERROR             VALUE '30' '35' '37'.

      *================================================================*
      * PROGRAM CONTROL FLAGS                                          *
      *================================================================*
       01  WS-PROGRAM-FLAGS.
           05  WS-END-OF-FILE-SW           PIC X(01) VALUE 'N'.
               88  END-OF-FILE             VALUE 'Y'.
               88  NOT-END-OF-FILE         VALUE 'N'.
           05  WS-CLAIM-VALID-SW           PIC X(01) VALUE 'Y'.
               88  CLAIM-IS-VALID          VALUE 'Y'.
               88  CLAIM-IS-INVALID        VALUE 'N'.
           05  WS-PEND-CLAIM-SW            PIC X(01) VALUE 'N'.
               88  CLAIM-SHOULD-PEND       VALUE 'Y'.
               88  CLAIM-SHOULD-NOT-PEND   VALUE 'N'.
           05  WS-DB-CONNECTED-SW          PIC X(01) VALUE 'N'.
               88  DB-IS-CONNECTED         VALUE 'Y'.
               88  DB-NOT-CONNECTED        VALUE 'N'.
           05  WS-PRICING-FOUND-SW         PIC X(01) VALUE 'N'.
               88  PRICING-FOUND           VALUE 'Y'.
               88  PRICING-NOT-FOUND       VALUE 'N'.
           05  WS-COB-REQUIRED-SW          PIC X(01) VALUE 'N'.
               88  COB-IS-REQUIRED         VALUE 'Y'.
               88  COB-NOT-REQUIRED        VALUE 'N'.
           05  WS-AUTH-REQUIRED-SW         PIC X(01) VALUE 'N'.
               88  AUTH-IS-REQUIRED        VALUE 'Y'.
               88  AUTH-NOT-REQUIRED       VALUE 'N'.
           05  WS-AUTH-FOUND-SW            PIC X(01) VALUE 'N'.
               88  AUTH-WAS-FOUND          VALUE 'Y'.
               88  AUTH-NOT-FOUND          VALUE 'N'.
           05  WS-CAPITATED-SW             PIC X(01) VALUE 'N'.
               88  SERVICE-IS-CAPITATED    VALUE 'Y'.
               88  SERVICE-NOT-CAPITATED   VALUE 'N'.
           05  WS-DEDUCT-MET-SW            PIC X(01) VALUE 'N'.
               88  DEDUCTIBLE-IS-MET       VALUE 'Y'.
               88  DEDUCTIBLE-NOT-MET      VALUE 'N'.
           05  WS-OOP-MET-SW              PIC X(01) VALUE 'N'.
               88  OOP-MAX-IS-MET          VALUE 'Y'.
               88  OOP-MAX-NOT-MET         VALUE 'N'.
           05  WS-FAMILY-DEDUCT-MET-SW    PIC X(01) VALUE 'N'.
               88  FAMILY-DEDUCT-MET       VALUE 'Y'.
               88  FAMILY-DEDUCT-NOT-MET   VALUE 'N'.
           05  WS-FAMILY-OOP-MET-SW       PIC X(01) VALUE 'N'.
               88  FAMILY-OOP-MET          VALUE 'Y'.
               88  FAMILY-OOP-NOT-MET      VALUE 'N'.
           05  WS-LIFETIME-MAX-SW         PIC X(01) VALUE 'N'.
               88  LIFETIME-MAX-EXCEEDED   VALUE 'Y'.
               88  LIFETIME-MAX-OK         VALUE 'N'.
           05  WS-ANNUAL-MAX-SW           PIC X(01) VALUE 'N'.
               88  ANNUAL-MAX-EXCEEDED     VALUE 'Y'.
               88  ANNUAL-MAX-OK           VALUE 'N'.
           05  WS-OUTLIER-SW              PIC X(01) VALUE 'N'.
               88  IS-OUTLIER-CASE         VALUE 'Y'.
               88  NOT-OUTLIER-CASE        VALUE 'N'.
           05  WS-TRANSFER-DRG-SW         PIC X(01) VALUE 'N'.
               88  IS-TRANSFER-DRG         VALUE 'Y'.
               88  NOT-TRANSFER-DRG        VALUE 'N'.
           05  WS-GLOBAL-PERIOD-SW        PIC X(01) VALUE 'N'.
               88  WITHIN-GLOBAL-PERIOD    VALUE 'Y'.
               88  NOT-IN-GLOBAL-PERIOD    VALUE 'N'.
           05  WS-CLEAN-CLAIM-SW          PIC X(01) VALUE 'Y'.
               88  IS-CLEAN-CLAIM          VALUE 'Y'.
               88  NOT-CLEAN-CLAIM         VALUE 'N'.
           05  WS-COB-PRIMARY-SW          PIC X(01) VALUE 'Y'.
               88  WE-ARE-PRIMARY          VALUE 'Y'.
               88  WE-ARE-SECONDARY        VALUE 'N'.
           05  WS-PREVENTIVE-SW           PIC X(01) VALUE 'N'.
               88  IS-PREVENTIVE-CARE      VALUE 'Y'.
               88  NOT-PREVENTIVE-CARE     VALUE 'N'.
           05  WS-COVID-RELATED-SW        PIC X(01) VALUE 'N'.
               88  IS-COVID-RELATED        VALUE 'Y'.
               88  NOT-COVID-RELATED       VALUE 'N'.
           05  WS-TELEHEALTH-SW           PIC X(01) VALUE 'N'.
               88  IS-TELEHEALTH           VALUE 'Y'.
               88  NOT-TELEHEALTH          VALUE 'N'.
           05  WS-NO-SURPRISE-SW          PIC X(01) VALUE 'N'.
               88  NO-SURPRISE-APPLIES     VALUE 'Y'.
               88  NO-SURPRISE-NA          VALUE 'N'.
           05  WS-DEADLOCK-RETRY-SW       PIC X(01) VALUE 'N'.
               88  DEADLOCK-OCCURRED       VALUE 'Y'.
               88  NO-DEADLOCK             VALUE 'N'.
           05  WS-COPAY-BEFORE-DEDUCT-SW  PIC X(01) VALUE 'N'.
               88  COPAY-BEFORE-DEDUCTIBLE VALUE 'Y'.
               88  COPAY-AFTER-DEDUCTIBLE  VALUE 'N'.
           05  WS-EMBEDDED-DEDUCT-SW      PIC X(01) VALUE 'N'.
               88  IS-EMBEDDED-DEDUCT      VALUE 'Y'.
               88  NOT-EMBEDDED-DEDUCT     VALUE 'N'.
           05  WS-CARRYOVER-DEDUCT-SW     PIC X(01) VALUE 'N'.
               88  CARRYOVER-APPLIES       VALUE 'Y'.
               88  CARRYOVER-NA            VALUE 'N'.
           05  WS-ER-ADMITTED-SW          PIC X(01) VALUE 'N'.
               88  ER-WAS-ADMITTED         VALUE 'Y'.
               88  ER-NOT-ADMITTED         VALUE 'N'.
           05  WS-COPAY-TO-OOP-SW        PIC X(01) VALUE 'Y'.
               88  COPAY-COUNTS-TO-OOP    VALUE 'Y'.
               88  COPAY-NOT-TO-OOP       VALUE 'N'.
           05  WS-GRANDFATHERED-SW       PIC X(01) VALUE 'N'.
               88  IS-GRANDFATHERED-PLAN  VALUE 'Y'.
               88  NOT-GRANDFATHERED      VALUE 'N'.

      *================================================================*
      * CURRENT DATE AND TIME FIELDS                                   *
      *================================================================*
       01  WS-CURRENT-DATE-TIME.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR        PIC 9(04).
               10  WS-CURRENT-MONTH       PIC 9(02).
               10  WS-CURRENT-DAY         PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR        PIC 9(02).
               10  WS-CURRENT-MINUTE      PIC 9(02).
               10  WS-CURRENT-SECOND      PIC 9(02).
               10  WS-CURRENT-HUNDREDTH   PIC 9(02).
           05  WS-GMT-OFFSET              PIC X(05).

       01  WS-PROCESS-START-TIME          PIC 9(08).
       01  WS-PROCESS-END-TIME            PIC 9(08).
       01  WS-CLAIM-START-TIME            PIC 9(08).
       01  WS-ELAPSED-SECONDS             PIC 9(08) COMP.
       01  WS-BATCH-RUN-DATE              PIC 9(08).
       01  WS-BATCH-RUN-DATE-X            PIC X(10).
       01  WS-JULIAN-DATE                 PIC 9(07).

      *================================================================*
      * CLAIM HEADER WORKING FIELDS                                    *
      *================================================================*
       01  WS-CLAIM-HEADER.
           05  WS-CLM-ID                  PIC X(15).
           05  WS-CLM-TYPE                PIC X(02).
               88  CLM-TYPE-PROFESSIONAL  VALUE 'PR'.
               88  CLM-TYPE-INSTITUTIONAL VALUE 'IN'.
               88  CLM-TYPE-DENTAL        VALUE 'DN'.
               88  CLM-TYPE-PHARMACY      VALUE 'RX'.
           05  WS-CLM-SUB-TYPE            PIC X(02).
               88  CLM-INPATIENT          VALUE 'IP'.
               88  CLM-OUTPATIENT         VALUE 'OP'.
               88  CLM-EMERGENCY          VALUE 'ER'.
               88  CLM-OFFICE-VISIT       VALUE 'OV'.
               88  CLM-LAB                VALUE 'LB'.
               88  CLM-RADIOLOGY          VALUE 'RD'.
               88  CLM-MENTAL-HEALTH      VALUE 'MH'.
               88  CLM-SUBSTANCE-ABUSE    VALUE 'SA'.
               88  CLM-SNF                VALUE 'SN'.
               88  CLM-HOME-HEALTH        VALUE 'HH'.
               88  CLM-DME                VALUE 'DM'.
               88  CLM-AMBULANCE          VALUE 'AM'.
               88  CLM-TELEHEALTH         VALUE 'TH'.
           05  WS-CLM-FREQUENCY-CODE      PIC X(01).
               88  CLM-ORIGINAL           VALUE '1'.
               88  CLM-VOID               VALUE '8'.
               88  CLM-REPLACEMENT        VALUE '7'.
               88  CLM-ADJUSTMENT         VALUE '5'.
           05  WS-CLM-RECEIVED-DATE       PIC 9(08).
           05  WS-CLM-FROM-DATE           PIC 9(08).
           05  WS-CLM-THRU-DATE           PIC 9(08).
           05  WS-CLM-ADMIT-DATE          PIC 9(08).
           05  WS-CLM-DISCHARGE-DATE      PIC 9(08).
           05  WS-CLM-DISCHARGE-STATUS    PIC X(02).
               88  CLM-DISCHARGED-HOME    VALUE '01'.
               88  CLM-TRANSFERRED        VALUE '02' '05' '06'
                                                '65' '66'.
               88  CLM-LEFT-AMA           VALUE '07'.
               88  CLM-EXPIRED            VALUE '20'.
               88  CLM-STILL-PATIENT      VALUE '30'.
           05  WS-CLM-TOTAL-CHARGE        PIC S9(09)V99 COMP-3.
           05  WS-CLM-DRG-CODE            PIC X(04).
           05  WS-CLM-ADMIT-DIAG          PIC X(07).
           05  WS-CLM-PRINCIPAL-DIAG      PIC X(07).
           05  WS-CLM-DIAG-TABLE.
               10  WS-CLM-DIAG-ENTRY OCCURS 25 TIMES.
                   15  WS-CLM-DIAG-CODE   PIC X(07).
                   15  WS-CLM-DIAG-POA    PIC X(01).
                       88  DIAG-POA-YES   VALUE 'Y'.
                       88  DIAG-POA-NO    VALUE 'N'.
                       88  DIAG-POA-UNK   VALUE 'U' 'W'.
           05  WS-CLM-PROC-TABLE.
               10  WS-CLM-PROC-ENTRY OCCURS 25 TIMES.
                   15  WS-CLM-ICD-PROC    PIC X(07).
                   15  WS-CLM-PROC-DATE   PIC 9(08).
           05  WS-CLM-TYPE-OF-BILL        PIC X(04).
           05  WS-CLM-PLACE-OF-SERVICE    PIC X(02).
               88  POS-OFFICE             VALUE '11'.
               88  POS-HOME               VALUE '12'.
               88  POS-INPATIENT          VALUE '21'.
               88  POS-OUTPATIENT         VALUE '22'.
               88  POS-EMERGENCY          VALUE '23'.
               88  POS-ASC                VALUE '24'.
               88  POS-SNF                VALUE '31' '32'.
               88  POS-TELEHEALTH         VALUE '02'.
               88  POS-LAB                VALUE '81'.
           05  WS-CLM-LINE-COUNT          PIC 9(03) COMP-3.
           05  WS-CLM-STATUS              PIC X(02).
               88  CLM-STATUS-RECEIVED    VALUE 'RC'.
               88  CLM-STATUS-ADJUDICATED VALUE 'AJ'.
               88  CLM-STATUS-PENDED      VALUE 'PD'.
               88  CLM-STATUS-DENIED      VALUE 'DN'.
               88  CLM-STATUS-PAID        VALUE 'PA'.
               88  CLM-STATUS-VOID        VALUE 'VD'.
           05  WS-CLM-SOURCE              PIC X(02).
               88  CLM-FROM-EDI           VALUE 'ED'.
               88  CLM-FROM-PAPER         VALUE 'PP'.
               88  CLM-FROM-PORTAL        VALUE 'WB'.

      *================================================================*
      * CLAIM DETAIL/LINE WORKING FIELDS                               *
      *================================================================*
       01  WS-CLAIM-DETAIL.
           05  WS-DTL-LINE-NUMBER         PIC 9(03) COMP-3.
           05  WS-DTL-CPT-CODE            PIC X(05).
           05  WS-DTL-HCPCS-CODE          PIC X(05).
           05  WS-DTL-REVENUE-CODE        PIC X(04).
           05  WS-DTL-MODIFIER-TABLE.
               10  WS-DTL-MODIFIER        PIC X(02)
                                           OCCURS 4 TIMES.
           05  WS-DTL-FROM-DATE           PIC 9(08).
           05  WS-DTL-THRU-DATE           PIC 9(08).
           05  WS-DTL-UNITS               PIC S9(05)V99 COMP-3.
           05  WS-DTL-BILLED-AMOUNT       PIC S9(07)V99 COMP-3.
           05  WS-DTL-ALLOWED-AMOUNT      PIC S9(07)V99 COMP-3.
           05  WS-DTL-DEDUCTIBLE-AMT      PIC S9(07)V99 COMP-3.
           05  WS-DTL-COPAY-AMOUNT        PIC S9(07)V99 COMP-3.
           05  WS-DTL-COINSURANCE-AMT     PIC S9(07)V99 COMP-3.
           05  WS-DTL-COB-AMOUNT          PIC S9(07)V99 COMP-3.
           05  WS-DTL-PAID-AMOUNT         PIC S9(07)V99 COMP-3.
           05  WS-DTL-WITHHOLD-AMT        PIC S9(07)V99 COMP-3.
           05  WS-DTL-INTEREST-AMT        PIC S9(07)V99 COMP-3.
           05  WS-DTL-NET-PAYMENT         PIC S9(07)V99 COMP-3.
           05  WS-DTL-PATIENT-RESP        PIC S9(07)V99 COMP-3.
           05  WS-DTL-PRICING-METHOD      PIC X(02).
               88  PRICE-BY-FEE-SCHED     VALUE 'FS'.
               88  PRICE-BY-DRG           VALUE 'DG'.
               88  PRICE-BY-PER-DIEM      VALUE 'PD'.
               88  PRICE-BY-CASE-RATE     VALUE 'CR'.
               88  PRICE-BY-PCT-CHARGE    VALUE 'PC'.
               88  PRICE-BY-CAPITATION    VALUE 'CP'.
               88  PRICE-BY-CONTRACT      VALUE 'CT'.
           05  WS-DTL-ADJUD-STATUS        PIC X(02).
               88  DTL-APPROVED           VALUE 'AP'.
               88  DTL-DENIED             VALUE 'DN'.
               88  DTL-PENDED             VALUE 'PD'.
               88  DTL-PARTIAL            VALUE 'PT'.
           05  WS-DTL-DENY-REASON         PIC X(05).
           05  WS-DTL-REMARK-CODES.
               10  WS-DTL-REMARK          PIC X(05)
                                           OCCURS 5 TIMES.
           05  WS-DTL-ADJUSTMENT-CODES.
               10  WS-DTL-ADJ-GROUP       PIC X(02)
                                           OCCURS 6 TIMES.
               10  WS-DTL-ADJ-REASON      PIC X(05)
                                           OCCURS 6 TIMES.
               10  WS-DTL-ADJ-AMOUNT      PIC S9(07)V99 COMP-3
                                           OCCURS 6 TIMES.

      *================================================================*
      * MEMBER/PATIENT INFORMATION                                     *
      *================================================================*
       01  WS-MEMBER-INFO.
           05  WS-MBR-ID                  PIC X(12).
           05  WS-MBR-LAST-NAME           PIC X(25).
           05  WS-MBR-FIRST-NAME          PIC X(15).
           05  WS-MBR-DOB                 PIC 9(08).
           05  WS-MBR-GENDER              PIC X(01).
               88  MBR-MALE               VALUE 'M'.
               88  MBR-FEMALE             VALUE 'F'.
               88  MBR-NONBINARY          VALUE 'X'.
           05  WS-MBR-RELATIONSHIP        PIC X(02).
               88  MBR-IS-SUBSCRIBER      VALUE '18'.
               88  MBR-IS-SPOUSE          VALUE '01'.
               88  MBR-IS-CHILD           VALUE '19'.
               88  MBR-IS-OTHER-DEPEND    VALUE '20' '21' '29'
                                                '32' '33'.
           05  WS-MBR-GROUP-ID            PIC X(10).
           05  WS-MBR-PLAN-CODE           PIC X(06).
           05  WS-MBR-PLAN-TYPE           PIC X(03).
               88  PLAN-TYPE-HMO          VALUE 'HMO'.
               88  PLAN-TYPE-PPO          VALUE 'PPO'.
               88  PLAN-TYPE-POS          VALUE 'POS'.
               88  PLAN-TYPE-EPO          VALUE 'EPO'.
               88  PLAN-TYPE-IND          VALUE 'IND'.
               88  PLAN-TYPE-HDHP         VALUE 'HDP'.
               88  PLAN-TYPE-MCARE        VALUE 'MCR'.
               88  PLAN-TYPE-MCAID        VALUE 'MCD'.
           05  WS-MBR-EFF-DATE            PIC 9(08).
           05  WS-MBR-TERM-DATE           PIC 9(08).
           05  WS-MBR-COBRA-SW            PIC X(01).
               88  MBR-ON-COBRA           VALUE 'Y'.
               88  MBR-NOT-COBRA          VALUE 'N'.
           05  WS-MBR-MEDICARE-SW         PIC X(01).
               88  MBR-HAS-MEDICARE       VALUE 'Y'.
               88  MBR-NO-MEDICARE        VALUE 'N'.
           05  WS-MBR-MEDICARE-REASON     PIC X(02).
               88  MCR-REASON-AGED        VALUE 'AG'.
               88  MCR-REASON-DISABILITY  VALUE 'DS'.
               88  MCR-REASON-ESRD        VALUE 'ES'.
           05  WS-MBR-ESRD-START-DATE     PIC 9(08).
           05  WS-MBR-EMPLOYER-SIZE       PIC 9(05) COMP-3.
           05  WS-MBR-AGE                 PIC 9(03) COMP-3.
           05  WS-MBR-SUBSCRIBER-DOB      PIC 9(08).
           05  WS-MBR-SPOUSE-DOB          PIC 9(08).
           05  WS-MBR-NETWORK-STATUS      PIC X(02).
               88  MBR-IN-NETWORK         VALUE 'IN'.
               88  MBR-OUT-OF-NETWORK     VALUE 'ON'.
               88  MBR-TIER-2             VALUE 'T2'.

      *================================================================*
      * PROVIDER INFORMATION                                           *
      *================================================================*
       01  WS-PROVIDER-INFO.
           05  WS-PROV-NPI                PIC X(10).
           05  WS-PROV-TAX-ID             PIC X(09).
           05  WS-PROV-LAST-NAME          PIC X(25).
           05  WS-PROV-FIRST-NAME         PIC X(15).
           05  WS-PROV-SPECIALTY          PIC X(03).
               88  PROV-IS-PCP            VALUE '001' '008' '011'
                                                '038' '084'.
               88  PROV-IS-SPECIALIST     VALUE '003' '004' '005'
                                                '006' '007' '010'
                                                '012' '013' '014'
                                                '016' '020' '022'
                                                '024' '025' '026'
                                                '028' '029' '033'
                                                '034' '035' '036'
                                                '037' '039' '040'
                                                '041' '046' '066'
                                                '070' '076' '077'
                                                '078' '079' '082'
                                                '083' '085' '086'
                                                '091' '098'.
               88  PROV-IS-FACILITY       VALUE 'FAC'.
           05  WS-PROV-TYPE               PIC X(02).
               88  PROV-TYPE-MD           VALUE 'MD'.
               88  PROV-TYPE-DO           VALUE 'DO'.
               88  PROV-TYPE-NP           VALUE 'NP'.
               88  PROV-TYPE-PA           VALUE 'PA'.
               88  PROV-TYPE-HOSP         VALUE 'HP'.
               88  PROV-TYPE-ASC          VALUE 'AS'.
               88  PROV-TYPE-SNF          VALUE 'SF'.
           05  WS-PROV-PAR-STATUS         PIC X(02).
               88  PROV-IS-PAR            VALUE 'PA'.
               88  PROV-IS-NON-PAR        VALUE 'NP'.
               88  PROV-IS-OON            VALUE 'OO'.
           05  WS-PROV-CONTRACT-ID        PIC X(10).
           05  WS-PROV-CONTRACT-TYPE      PIC X(02).
               88  CONTR-FEE-SCHEDULE     VALUE 'FS'.
               88  CONTR-DRG              VALUE 'DG'.
               88  CONTR-PER-DIEM         VALUE 'PD'.
               88  CONTR-CASE-RATE        VALUE 'CR'.
               88  CONTR-PCT-CHARGE       VALUE 'PC'.
               88  CONTR-CAPITATION       VALUE 'CP'.
           05  WS-PROV-WITHHOLD-PCT       PIC S9(03)V99 COMP-3.
           05  WS-PROV-CBSA-CODE          PIC X(05).
           05  WS-PROV-STATE              PIC X(02).
           05  WS-PROV-ZIP                PIC X(09).
           05  WS-PROV-DSH-PCT            PIC S9(03)V9(04) COMP-3.
           05  WS-PROV-IME-PCT            PIC S9(03)V9(06) COMP-3.
           05  WS-PROV-COST-TO-CHARGE     PIC S9(01)V9(06) COMP-3.
           05  WS-PROV-TEACHING-SW        PIC X(01).
               88  PROV-IS-TEACHING       VALUE 'Y'.
               88  PROV-NOT-TEACHING      VALUE 'N'.
           05  WS-PROV-DSH-ELIGIBLE-SW    PIC X(01).
               88  PROV-DSH-ELIGIBLE      VALUE 'Y'.
               88  PROV-DSH-NOT-ELIGIBLE  VALUE 'N'.

      *================================================================*
      * BENEFIT PLAN INFORMATION                                       *
      *================================================================*
       01  WS-BENEFIT-PLAN.
           05  WS-BEN-PLAN-CODE           PIC X(06).
           05  WS-BEN-PLAN-YEAR           PIC 9(04).
           05  WS-BEN-INN-DEDUCTIBLE.
               10  WS-BEN-INN-IND-DEDUCT PIC S9(07)V99 COMP-3.
               10  WS-BEN-INN-FAM-DEDUCT PIC S9(07)V99 COMP-3.
           05  WS-BEN-OON-DEDUCTIBLE.
               10  WS-BEN-OON-IND-DEDUCT PIC S9(07)V99 COMP-3.
               10  WS-BEN-OON-FAM-DEDUCT PIC S9(07)V99 COMP-3.
           05  WS-BEN-INN-COINSURANCE     PIC S9(03)V99 COMP-3.
           05  WS-BEN-OON-COINSURANCE     PIC S9(03)V99 COMP-3.
           05  WS-BEN-INN-OOP-MAX.
               10  WS-BEN-INN-IND-OOP    PIC S9(07)V99 COMP-3.
               10  WS-BEN-INN-FAM-OOP    PIC S9(07)V99 COMP-3.
           05  WS-BEN-OON-OOP-MAX.
               10  WS-BEN-OON-IND-OOP    PIC S9(07)V99 COMP-3.
               10  WS-BEN-OON-FAM-OOP    PIC S9(07)V99 COMP-3.
           05  WS-BEN-LIFETIME-MAX        PIC S9(09)V99 COMP-3.
           05  WS-BEN-ANNUAL-MAX          PIC S9(09)V99 COMP-3.
           05  WS-BEN-COPAY-TABLE.
               10  WS-BEN-COPAY-PCP      PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-SPEC     PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-ER       PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-URGENT   PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-INPT     PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-OUTPT    PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-LAB      PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-RAD      PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-MH       PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-PT       PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-RX-GEN   PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-RX-BRAND PIC S9(05)V99 COMP-3.
               10  WS-BEN-COPAY-RX-SPEC  PIC S9(05)V99 COMP-3.
           05  WS-BEN-EMBEDDED-IND        PIC X(01).
               88  BEN-EMBEDDED-DEDUCT    VALUE 'Y'.
               88  BEN-NOT-EMBEDDED       VALUE 'N'.
           05  WS-BEN-COPAY-BEFORE-DED    PIC X(01).
               88  BEN-COPAY-BEFORE-DED   VALUE 'Y'.
               88  BEN-COPAY-AFTER-DED    VALUE 'N'.
           05  WS-BEN-COPAY-TO-OOP        PIC X(01).
               88  BEN-COPAY-TO-OOP       VALUE 'Y'.
               88  BEN-COPAY-NOT-OOP      VALUE 'N'.
           05  WS-BEN-CARRYOVER-IND       PIC X(01).
               88  BEN-HAS-CARRYOVER      VALUE 'Y'.
               88  BEN-NO-CARRYOVER       VALUE 'N'.
           05  WS-BEN-CARRYOVER-MONTHS    PIC 9(02) COMP-3.
           05  WS-BEN-GRANDFATHERED       PIC X(01).
               88  BEN-IS-GRANDFATHERED   VALUE 'Y'.
               88  BEN-NOT-GRANDFATHERED  VALUE 'N'.
           05  WS-BEN-PREVENTIVE-IND      PIC X(01).
               88  BEN-COVERS-PREVENTIVE  VALUE 'Y'.
               88  BEN-NO-PREVENTIVE      VALUE 'N'.
           05  WS-BEN-MH-PARITY-IND      PIC X(01).
               88  BEN-MH-PARITY         VALUE 'Y'.
               88  BEN-NO-MH-PARITY      VALUE 'N'.
           05  WS-BEN-FAM-DEDUCT-RULE     PIC X(01).
               88  FAM-DEDUCT-AGGREGATE   VALUE 'A'.
               88  FAM-DEDUCT-2-OF-3      VALUE '2'.
               88  FAM-DEDUCT-EACH        VALUE 'E'.

      *================================================================*
      * BENEFIT ACCUMULATORS                                           *
      *================================================================*
       01  WS-ACCUMULATORS.
           05  WS-ACC-INN-IND-DEDUCT-USED PIC S9(07)V99 COMP-3.
           05  WS-ACC-INN-FAM-DEDUCT-USED PIC S9(07)V99 COMP-3.
           05  WS-ACC-OON-IND-DEDUCT-USED PIC S9(07)V99 COMP-3.
           05  WS-ACC-OON-FAM-DEDUCT-USED PIC S9(07)V99 COMP-3.
           05  WS-ACC-INN-IND-OOP-USED    PIC S9(07)V99 COMP-3.
           05  WS-ACC-INN-FAM-OOP-USED    PIC S9(07)V99 COMP-3.
           05  WS-ACC-OON-IND-OOP-USED    PIC S9(07)V99 COMP-3.
           05  WS-ACC-OON-FAM-OOP-USED    PIC S9(07)V99 COMP-3.
           05  WS-ACC-LIFETIME-USED        PIC S9(09)V99 COMP-3.
           05  WS-ACC-ANNUAL-USED          PIC S9(09)V99 COMP-3.
           05  WS-ACC-RESERVED-AMT         PIC S9(07)V99 COMP-3.
           05  WS-ACC-FAMILY-MEMBER-CNT    PIC 9(02) COMP-3.
           05  WS-ACC-FAM-MEMBERS-MET      PIC 9(02) COMP-3.

       01  WS-FAMILY-DEDUCT-TABLE.
           05  WS-FAM-MEMBER-ENTRY OCCURS 10 TIMES.
               10  WS-FAM-MBR-ID          PIC X(12).
               10  WS-FAM-MBR-IND-USED    PIC S9(07)V99 COMP-3.
               10  WS-FAM-MBR-IND-MET-SW  PIC X(01).
                   88  FAM-MBR-DEDUCT-MET  VALUE 'Y'.
                   88  FAM-MBR-DEDUCT-OPEN VALUE 'N'.

      *================================================================*
      * PRICING CALCULATION FIELDS                                     *
      *================================================================*
       01  WS-PRICING-FIELDS.
           05  WS-PRC-WORK-RVU            PIC S9(05)V9(04) COMP-3.
           05  WS-PRC-PE-RVU              PIC S9(05)V9(04) COMP-3.
           05  WS-PRC-MP-RVU              PIC S9(05)V9(04) COMP-3.
           05  WS-PRC-WORK-GPCI           PIC S9(03)V9(04) COMP-3.
           05  WS-PRC-PE-GPCI             PIC S9(03)V9(04) COMP-3.
           05  WS-PRC-MP-GPCI             PIC S9(03)V9(04) COMP-3.
           05  WS-PRC-CONVERSION-FACTOR    PIC S9(05)V9(04) COMP-3.
           05  WS-PRC-TOTAL-RVU           PIC S9(07)V9(04) COMP-3.
           05  WS-PRC-ALLOWED-CALC        PIC S9(09)V99 COMP-3.
           05  WS-PRC-FEE-SCHEDULE-AMT    PIC S9(07)V99 COMP-3.
           05  WS-PRC-MODIFIER-ADJ-PCT    PIC S9(03)V99 COMP-3.
           05  WS-PRC-SOS-REDUCTION       PIC S9(03)V99 COMP-3.
           05  WS-PRC-MULTIPLE-PROC-PCT   PIC S9(03)V99 COMP-3.
           05  WS-PRC-BILATERAL-PCT       PIC S9(03)V99 COMP-3.
           05  WS-PRC-ASSIST-SURG-PCT     PIC S9(03)V99 COMP-3.
           05  WS-PRC-UCR-AMOUNT          PIC S9(07)V99 COMP-3.
           05  WS-PRC-MAC-AMOUNT          PIC S9(07)V99 COMP-3.

      *================================================================*
      * DRG PRICING FIELDS                                             *
      *================================================================*
       01  WS-DRG-PRICING.
           05  WS-DRG-WEIGHT              PIC S9(03)V9(04) COMP-3.
           05  WS-DRG-RELATIVE-WEIGHT     PIC S9(03)V9(04) COMP-3.
           05  WS-DRG-FEDERAL-RATE        PIC S9(09)V99 COMP-3.
           05  WS-DRG-WAGE-INDEX          PIC S9(03)V9(04) COMP-3.
           05  WS-DRG-LABOR-SHARE         PIC S9(01)V9(04) COMP-3.
           05  WS-DRG-NON-LABOR-SHARE     PIC S9(01)V9(04) COMP-3.
           05  WS-DRG-BASE-RATE           PIC S9(09)V99 COMP-3.
           05  WS-DRG-CASE-MIX-INDEX      PIC S9(03)V9(04) COMP-3.
           05  WS-DRG-OUTLIER-THRESHOLD    PIC S9(09)V99 COMP-3.
           05  WS-DRG-OUTLIER-PAYMENT     PIC S9(09)V99 COMP-3.
           05  WS-DRG-COST-AMOUNT         PIC S9(09)V99 COMP-3.
           05  WS-DRG-DSH-AMOUNT          PIC S9(09)V99 COMP-3.
           05  WS-DRG-IME-AMOUNT          PIC S9(09)V99 COMP-3.
           05  WS-DRG-TRANSFER-PER-DIEM   PIC S9(09)V99 COMP-3.
           05  WS-DRG-TRANSFER-DAYS       PIC 9(03) COMP-3.
           05  WS-DRG-GEOM-LOS            PIC S9(03)V99 COMP-3.
           05  WS-DRG-ARITH-LOS           PIC S9(03)V99 COMP-3.
           05  WS-DRG-TOTAL-PAYMENT       PIC S9(09)V99 COMP-3.
           05  WS-DRG-NEW-TECH-AMOUNT     PIC S9(09)V99 COMP-3.
           05  WS-DRG-NEW-TECH-SW         PIC X(01).
               88  HAS-NEW-TECH-ADD-ON    VALUE 'Y'.
               88  NO-NEW-TECH-ADD-ON     VALUE 'N'.
           05  WS-DRG-READMIT-SW         PIC X(01).
               88  IS-READMISSION         VALUE 'Y'.
               88  NOT-READMISSION        VALUE 'N'.
           05  WS-DRG-READMIT-PENALTY-PCT PIC S9(01)V9(04) COMP-3.

      *================================================================*
      * PER DIEM PRICING FIELDS                                        *
      *================================================================*
       01  WS-PER-DIEM-PRICING.
           05  WS-PDM-BASE-RATE           PIC S9(07)V99 COMP-3.
           05  WS-PDM-ICU-RATE            PIC S9(07)V99 COMP-3.
           05  WS-PDM-ANCILLARY-RATE      PIC S9(07)V99 COMP-3.
           05  WS-PDM-LOS-LIMIT           PIC 9(03) COMP-3.
           05  WS-PDM-ACTUAL-LOS          PIC 9(03) COMP-3.
           05  WS-PDM-COVERED-DAYS        PIC 9(03) COMP-3.
           05  WS-PDM-ICU-DAYS            PIC 9(03) COMP-3.
           05  WS-PDM-REGULAR-DAYS        PIC 9(03) COMP-3.
           05  WS-PDM-STEP-DOWN-DAY       PIC 9(03) COMP-3.
           05  WS-PDM-STEP-DOWN-PCT       PIC S9(01)V9(04) COMP-3.
           05  WS-PDM-MAX-PER-DIEM        PIC S9(07)V99 COMP-3.
           05  WS-PDM-TOTAL-ROOM-BOARD    PIC S9(09)V99 COMP-3.
           05  WS-PDM-TOTAL-ANCILLARY     PIC S9(09)V99 COMP-3.
           05  WS-PDM-TOTAL-PAYMENT       PIC S9(09)V99 COMP-3.
           05  WS-PDM-SERVICE-TYPE        PIC X(03).
               88  PDM-MED-SURG           VALUE 'MSR'.
               88  PDM-ICU                VALUE 'ICU'.
               88  PDM-PSYCH              VALUE 'PSY'.
               88  PDM-REHAB              VALUE 'REH'.
               88  PDM-SNF                VALUE 'SNF'.
               88  PDM-MATERNITY          VALUE 'MAT'.
               88  PDM-NICU               VALUE 'NIC'.
               88  PDM-STEP-DOWN-ICU      VALUE 'SDI'.

      *================================================================*
      * CASE RATE PRICING FIELDS                                       *
      *================================================================*
       01  WS-CASE-RATE-PRICING.
           05  WS-CSR-RATE-AMOUNT         PIC S9(07)V99 COMP-3.
           05  WS-CSR-GLOBAL-DAYS         PIC 9(03) COMP-3.
           05  WS-CSR-GLOBAL-START-DATE   PIC 9(08).
           05  WS-CSR-GLOBAL-END-DATE     PIC 9(08).
           05  WS-CSR-CARVE-OUT-AMT       PIC S9(07)V99 COMP-3.
           05  WS-CSR-TOTAL-PAYMENT       PIC S9(07)V99 COMP-3.
           05  WS-CSR-BUNDLE-SW           PIC X(01).
               88  CSR-IS-BUNDLED         VALUE 'Y'.
               88  CSR-NOT-BUNDLED        VALUE 'N'.

      *================================================================*
      * PERCENT OF CHARGES FIELDS                                      *
      *================================================================*
       01  WS-PCT-CHARGE-PRICING.
           05  WS-PCT-CONTRACT-PCT        PIC S9(03)V99 COMP-3.
           05  WS-PCT-UCR-LIMIT           PIC S9(07)V99 COMP-3.
           05  WS-PCT-MAX-ALLOW           PIC S9(07)V99 COMP-3.
           05  WS-PCT-CALC-AMOUNT         PIC S9(09)V99 COMP-3.
           05  WS-PCT-TOTAL-PAYMENT       PIC S9(09)V99 COMP-3.

      *================================================================*
      * COORDINATION OF BENEFITS FIELDS                                *
      *================================================================*
       01  WS-COB-FIELDS.
           05  WS-COB-ORDER               PIC X(01).
               88  COB-ORDER-PRIMARY      VALUE 'P'.
               88  COB-ORDER-SECONDARY    VALUE 'S'.
               88  COB-ORDER-TERTIARY     VALUE 'T'.
           05  WS-COB-METHOD              PIC X(02).
               88  COB-TRADITIONAL        VALUE 'TR'.
               88  COB-MAINTENANCE        VALUE 'MB'.
               88  COB-NON-DUPLICATION    VALUE 'ND'.
               88  COB-CARVE-OUT          VALUE 'CO'.
               88  COB-STANDARD           VALUE 'ST'.
           05  WS-COB-OTHER-CARRIER.
               10  WS-COB-OTHER-PLAN      PIC X(20).
               10  WS-COB-OTHER-PAID      PIC S9(07)V99 COMP-3.
               10  WS-COB-OTHER-ALLOWED   PIC S9(07)V99 COMP-3.
               10  WS-COB-OTHER-DEDUCT    PIC S9(07)V99 COMP-3.
               10  WS-COB-OTHER-COPAY     PIC S9(07)V99 COMP-3.
               10  WS-COB-OTHER-COINS     PIC S9(07)V99 COMP-3.
           05  WS-COB-OUR-ALLOWED         PIC S9(07)V99 COMP-3.
           05  WS-COB-OUR-BENEFIT         PIC S9(07)V99 COMP-3.
           05  WS-COB-BALANCE-AFTER-PRI   PIC S9(07)V99 COMP-3.
           05  WS-COB-SECONDARY-PAY       PIC S9(07)V99 COMP-3.
           05  WS-COB-SAVINGS             PIC S9(07)V99 COMP-3.
           05  WS-COB-RULE-USED           PIC X(03).
               88  COB-RULE-BIRTHDAY      VALUE 'BDY'.
               88  COB-RULE-GENDER        VALUE 'GDR'.
               88  COB-RULE-ACTIVE        VALUE 'ACT'.
               88  COB-RULE-COBRA         VALUE 'CBR'.
               88  COB-RULE-LONGEST       VALUE 'LNG'.
               88  COB-RULE-COURT         VALUE 'CRT'.
               88  COB-RULE-MSP           VALUE 'MSP'.
           05  WS-COB-MSP-TYPE            PIC X(02).
               88  MSP-WORKING-AGED       VALUE 'WA'.
               88  MSP-ESRD               VALUE 'ES'.
               88  MSP-DISABILITY         VALUE 'DS'.
           05  WS-COB-SUBSCRIBER-DOB-M    PIC 9(04).
           05  WS-COB-SUBSCRIBER-DOB-F    PIC 9(04).

      *================================================================*
      * PAYMENT CALCULATION FIELDS                                     *
      *================================================================*
       01  WS-PAYMENT-FIELDS.
           05  WS-PAY-GROSS-AMOUNT        PIC S9(09)V99 COMP-3.
           05  WS-PAY-WITHHOLD-AMT        PIC S9(07)V99 COMP-3.
           05  WS-PAY-INTEREST-AMT        PIC S9(07)V99 COMP-3.
           05  WS-PAY-PENALTY-AMT         PIC S9(07)V99 COMP-3.
           05  WS-PAY-NET-AMOUNT          PIC S9(09)V99 COMP-3.
           05  WS-PAY-PATIENT-RESP        PIC S9(09)V99 COMP-3.
           05  WS-PAY-MINIMUM-THRESHOLD   PIC S9(03)V99 COMP-3
                                           VALUE +0.50.
           05  WS-PAY-CLEAN-CLAIM-DATE    PIC 9(08).
           05  WS-PAY-PAYMENT-DATE        PIC 9(08).
           05  WS-PAY-DAYS-TO-PAY         PIC 9(05) COMP-3.
           05  WS-PAY-PROMPT-PAY-DAYS     PIC 9(03) COMP-3.
           05  WS-PAY-INTEREST-RATE       PIC S9(01)V9(06) COMP-3.
           05  WS-PAY-LATE-DAYS           PIC 9(05) COMP-3.
           05  WS-PAY-STATE-PROMPT-DAYS   PIC 9(03) COMP-3.
           05  WS-PAY-STATE-INT-RATE      PIC S9(01)V9(06) COMP-3.
           05  WS-PAY-OVERPAYMENT-AMT     PIC S9(09)V99 COMP-3.

      *================================================================*
      * PEND REASON FIELDS                                             *
      *================================================================*
       01  WS-PEND-FIELDS.
           05  WS-PEND-REASON-CODE        PIC X(05).
           05  WS-PEND-REASON-DESC        PIC X(50).
           05  WS-PEND-QUEUE              PIC X(03).
               88  PEND-MEDICAL-REVIEW    VALUE 'MRV'.
               88  PEND-PRICING-REVIEW    VALUE 'PRV'.
               88  PEND-COB-REVIEW        VALUE 'COB'.
               88  PEND-AUTH-REVIEW       VALUE 'AUT'.
               88  PEND-MANAGEMENT        VALUE 'MGT'.
               88  PEND-SPECIAL-INVEST    VALUE 'SIU'.
           05  WS-PEND-EXPECTED-DATE      PIC 9(08).
           05  WS-PEND-PRIORITY           PIC X(01).
               88  PEND-PRIORITY-HIGH     VALUE 'H'.
               88  PEND-PRIORITY-MEDIUM   VALUE 'M'.
               88  PEND-PRIORITY-LOW      VALUE 'L'.
           05  WS-PEND-COUNT-TOTAL        PIC 9(07) COMP-3.
           05  WS-PEND-REASON-TABLE.
               10  WS-PEND-ENTRY OCCURS 10 TIMES.
                   15  WS-PEND-RSN-CD     PIC X(05).
                   15  WS-PEND-RSN-DESC   PIC X(50).

      *================================================================*
      * MODIFIER ADJUSTMENT TABLE                                      *
      *================================================================*
       01  WS-MODIFIER-TABLE.
           05  WS-MOD-26-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.2600.
           05  WS-MOD-TC-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.7400.
           05  WS-MOD-50-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +1.5000.
           05  WS-MOD-51-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.5000.
           05  WS-MOD-52-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.5000.
           05  WS-MOD-59-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +1.0000.
           05  WS-MOD-62-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.6250.
           05  WS-MOD-80-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.1600.
           05  WS-MOD-81-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.1000.
           05  WS-MOD-82-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.1600.
           05  WS-MOD-AS-PCT              PIC S9(01)V9(04) COMP-3
                                           VALUE +0.1600.

      *================================================================*
      * MULTIPLE PROCEDURE DISCOUNT TABLE                              *
      *================================================================*
       01  WS-MULTIPLE-PROC-TABLE.
           05  WS-MULT-PROC-ENTRY OCCURS 6 TIMES.
               10  WS-MULT-PROC-RANK     PIC 9(01).
               10  WS-MULT-PROC-PCT      PIC S9(01)V9(04) COMP-3.
       01  WS-MULT-PROC-TABLE-VALUES.
           05  FILLER                     PIC X(07)
               VALUE X'01000010000F'.
           05  FILLER                     PIC X(07)
               VALUE X'02000005000F'.
           05  FILLER                     PIC X(07)
               VALUE X'03000005000F'.
           05  FILLER                     PIC X(07)
               VALUE X'04000005000F'.
           05  FILLER                     PIC X(07)
               VALUE X'05000005000F'.
           05  FILLER                     PIC X(07)
               VALUE X'06000005000F'.

      *================================================================*
      * STATE PROMPT PAY TABLE                                         *
      *================================================================*
       01  WS-STATE-PROMPT-PAY-TABLE.
           05  WS-SPP-ENTRY OCCURS 52 TIMES.
               10  WS-SPP-STATE-CODE      PIC X(02).
               10  WS-SPP-CLEAN-DAYS      PIC 9(03) COMP-3.
               10  WS-SPP-UNCLEAN-DAYS    PIC 9(03) COMP-3.
               10  WS-SPP-INT-RATE        PIC S9(01)V9(06) COMP-3.
               10  WS-SPP-PENALTY-PCT     PIC S9(01)V9(04) COMP-3.

      *================================================================*
      * PROCESSING COUNTERS AND ACCUMULATORS                           *
      *================================================================*
       01  WS-COUNTERS.
           05  WS-CTR-CLAIMS-READ         PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CLAIMS-PROCESSED    PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CLAIMS-PAID         PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CLAIMS-DENIED       PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CLAIMS-PENDED       PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CLAIMS-ZERO-PAY     PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CLAIMS-ERROR        PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-PROFESSIONAL        PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-INSTITUTIONAL       PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-LINES-PROCESSED     PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-DB-READS            PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-DB-UPDATES          PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-DB-INSERTS          PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-DEADLOCK-RETRIES    PIC 9(05) COMP-3 VALUE 0.
           05  WS-CTR-COB-CLAIMS          PIC 9(09) COMP-3 VALUE 0.
           05  WS-CTR-CAPITATED           PIC 9(09) COMP-3 VALUE 0.

       01  WS-DOLLAR-ACCUMS.
           05  WS-TOT-BILLED-AMOUNT       PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-ALLOWED-AMOUNT      PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-DEDUCTIBLE-AMOUNT   PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-COPAY-AMOUNT        PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-COINSURANCE-AMOUNT  PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-PAID-AMOUNT         PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-WITHHOLD-AMOUNT     PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-INTEREST-AMOUNT     PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-PATIENT-RESP        PIC S9(13)V99 COMP-3
                                           VALUE +0.
           05  WS-TOT-COB-SAVINGS         PIC S9(13)V99 COMP-3
                                           VALUE +0.

      *================================================================*
      * DATABASE HOST VARIABLES                                        *
      *================================================================*
       01  WS-DB-HOST-VARS.
           05  HV-CLAIM-ID                PIC X(15).
           05  HV-MEMBER-ID               PIC X(12).
           05  HV-PROVIDER-NPI            PIC X(10).
           05  HV-PLAN-CODE               PIC X(06).
           05  HV-CPT-CODE                PIC X(05).
           05  HV-HCPCS-CODE              PIC X(05).
           05  HV-DRG-CODE                PIC X(04).
           05  HV-REVENUE-CODE            PIC X(04).
           05  HV-MODIFIER-1              PIC X(02).
           05  HV-CBSA-CODE               PIC X(05).
           05  HV-STATE-CODE              PIC X(02).
           05  HV-CONTRACT-ID             PIC X(10).
           05  HV-SERVICE-DATE            PIC X(08).
           05  HV-PLAN-YEAR               PIC 9(04).
           05  HV-ALLOWED-AMOUNT          PIC S9(09)V99.
           05  HV-PAID-AMOUNT             PIC S9(09)V99.
           05  HV-DEDUCTIBLE-AMT          PIC S9(07)V99.
           05  HV-COPAY-AMT              PIC S9(07)V99.
           05  HV-COINSURANCE-AMT        PIC S9(07)V99.
           05  HV-WITHHOLD-AMT           PIC S9(07)V99.
           05  HV-INTEREST-AMT           PIC S9(07)V99.
           05  HV-NET-PAYMENT            PIC S9(09)V99.
           05  HV-WORK-RVU               PIC S9(05)V9(04).
           05  HV-PE-RVU                 PIC S9(05)V9(04).
           05  HV-MP-RVU                 PIC S9(05)V9(04).
           05  HV-WORK-GPCI              PIC S9(03)V9(04).
           05  HV-PE-GPCI               PIC S9(03)V9(04).
           05  HV-MP-GPCI               PIC S9(03)V9(04).
           05  HV-CONV-FACTOR            PIC S9(05)V9(04).
           05  HV-DRG-WEIGHT             PIC S9(03)V9(04).
           05  HV-FEDERAL-RATE           PIC S9(09)V99.
           05  HV-WAGE-INDEX             PIC S9(03)V9(04).
           05  HV-LABOR-SHARE            PIC S9(01)V9(04).
           05  HV-NON-LABOR              PIC S9(01)V9(04).
           05  HV-OUTLIER-THRESHOLD      PIC S9(09)V99.
           05  HV-PER-DIEM-RATE          PIC S9(07)V99.
           05  HV-ICU-RATE               PIC S9(07)V99.
           05  HV-CASE-RATE-AMT          PIC S9(07)V99.
           05  HV-GLOBAL-DAYS            PIC 9(03).
           05  HV-PCT-OF-CHARGE          PIC S9(03)V99.
           05  HV-UCR-AMOUNT             PIC S9(07)V99.
           05  HV-MAX-ALLOW              PIC S9(07)V99.
           05  HV-LOS-LIMIT              PIC 9(03).
           05  HV-STEP-DOWN-DAY          PIC 9(03).
           05  HV-STEP-DOWN-PCT          PIC S9(01)V9(04).
           05  HV-MAX-PER-DIEM           PIC S9(07)V99.
           05  HV-GEOM-LOS              PIC S9(03)V99.
           05  HV-ARITH-LOS             PIC S9(03)V99.
           05  HV-NEW-TECH-AMT          PIC S9(09)V99.
           05  HV-READMIT-PENALTY       PIC S9(01)V9(04).
           05  HV-COST-TO-CHARGE        PIC S9(01)V9(06).
           05  HV-DSH-PCT               PIC S9(03)V9(04).
           05  HV-IME-PCT               PIC S9(03)V9(06).
           05  HV-WITHHOLD-PCT          PIC S9(03)V99.
           05  HV-ANCILLARY-RATE        PIC S9(07)V99.
           05  HV-CASE-MIX-INDEX        PIC S9(03)V9(04).
           05  HV-IND-DEDUCT-USED       PIC S9(07)V99.
           05  HV-FAM-DEDUCT-USED       PIC S9(07)V99.
           05  HV-IND-OOP-USED          PIC S9(07)V99.
           05  HV-FAM-OOP-USED          PIC S9(07)V99.
           05  HV-LIFETIME-USED         PIC S9(09)V99.
           05  HV-ANNUAL-USED           PIC S9(09)V99.
           05  HV-RESERVED-AMT          PIC S9(07)V99.
           05  HV-ACCUM-TIMESTAMP       PIC X(26).
           05  HV-COB-OTHER-PAID        PIC S9(07)V99.
           05  HV-COB-OTHER-ALLOWED     PIC S9(07)V99.
           05  HV-AUDIT-ACTION          PIC X(03).
           05  HV-AUDIT-TIMESTAMP       PIC X(26).
           05  HV-CLAIM-STATUS          PIC X(02).
           05  HV-PEND-REASON           PIC X(05).
           05  HV-PEND-QUEUE            PIC X(03).
           05  HV-EXPECTED-DATE         PIC X(08).
           05  HV-SOS-INDICATOR         PIC X(01).
           05  HV-CAPITATION-SW         PIC X(01).
           05  HV-FAMILY-MBR-COUNT      PIC 9(02).
           05  HV-FAM-MEMBERS-MET       PIC 9(02).
           05  HV-ESRD-START-DATE       PIC X(08).
           05  HV-COURT-ORDER-SW        PIC X(01).
           05  HV-OTHER-COV-EFF-DATE    PIC X(08).
           05  HV-OTHER-COV-TERM-DATE   PIC X(08).
           05  HV-SUBSCRIBER-DOB-M      PIC X(08).
           05  HV-SUBSCRIBER-DOB-F      PIC X(08).
           05  HV-EMPLOYER-SIZE         PIC 9(05).
           05  HV-GLOBAL-START-DATE     PIC X(08).
           05  HV-GLOBAL-END-DATE       PIC X(08).
           05  HV-ICU-DAYS              PIC 9(03).

      *================================================================*
      * SQL RETURN CODE AND ERROR HANDLING                             *
      *================================================================*
       01  WS-SQL-FIELDS.
           05  WS-SQLCODE                 PIC S9(09) COMP.
           05  WS-SQLCODE-DISP            PIC -(09)9.
           05  WS-SQL-RETRY-COUNT         PIC 9(03) COMP-3 VALUE 0.
           05  WS-SQL-MAX-RETRIES         PIC 9(03) COMP-3 VALUE 3.
           05  WS-SQL-DEADLOCK-CODE       PIC S9(09) COMP
                                           VALUE -1205.
           05  WS-SQL-TIMEOUT-CODE        PIC S9(09) COMP
                                           VALUE -1224.
           05  WS-SQL-NOT-FOUND-CODE      PIC S9(09) COMP
                                           VALUE +100.

      *================================================================*
      * ERROR HANDLING FIELDS                                          *
      *================================================================*
       01  WS-ERROR-FIELDS.
           05  WS-ERR-PARAGRAPH           PIC X(30).
           05  WS-ERR-MESSAGE             PIC X(80).
           05  WS-ERR-SQLCODE             PIC S9(09) COMP.
           05  WS-ERR-FILE-STATUS         PIC X(02).
           05  WS-ERR-SEVERITY            PIC X(01).
               88  ERR-WARNING            VALUE 'W'.
               88  ERR-SEVERE             VALUE 'S'.
               88  ERR-FATAL              VALUE 'F'.
           05  WS-ERR-ACTION              PIC X(01).
               88  ERR-CONTINUE           VALUE 'C'.
               88  ERR-SKIP-CLAIM         VALUE 'S'.
               88  ERR-ABORT              VALUE 'A'.
           05  WS-ABEND-CODE              PIC X(04).
           05  WS-ERR-COUNT               PIC 9(07) COMP-3 VALUE 0.

      *================================================================*
      * WORK FIELDS                                                    *
      *================================================================*
       01  WS-WORK-FIELDS.
           05  WS-WRK-AMOUNT-1            PIC S9(09)V99 COMP-3.
           05  WS-WRK-AMOUNT-2            PIC S9(09)V99 COMP-3.
           05  WS-WRK-AMOUNT-3            PIC S9(09)V99 COMP-3.
           05  WS-WRK-AMOUNT-4            PIC S9(09)V99 COMP-3.
           05  WS-WRK-PCT-1               PIC S9(03)V9(04) COMP-3.
           05  WS-WRK-DAYS-1              PIC 9(05) COMP-3.
           05  WS-WRK-DAYS-2              PIC 9(05) COMP-3.
           05  WS-WRK-DATE-1              PIC 9(08).
           05  WS-WRK-DATE-2              PIC 9(08).
           05  WS-WRK-INDEX-1             PIC 9(03) COMP-3.
           05  WS-WRK-INDEX-2             PIC 9(03) COMP-3.
           05  WS-WRK-INDEX-3             PIC 9(03) COMP-3.
           05  WS-WRK-COMPARE             PIC S9(09)V99 COMP-3.
           05  WS-WRK-REMAINING           PIC S9(09)V99 COMP-3.
           05  WS-WRK-DEDUCT-APPLY        PIC S9(07)V99 COMP-3.
           05  WS-WRK-COPAY-APPLY         PIC S9(07)V99 COMP-3.
           05  WS-WRK-COINS-APPLY         PIC S9(07)V99 COMP-3.
           05  WS-WRK-MEMBER-RESP         PIC S9(09)V99 COMP-3.
           05  WS-WRK-PLAN-PAYS           PIC S9(09)V99 COMP-3.
           05  WS-WRK-OOP-REMAINING       PIC S9(07)V99 COMP-3.
           05  WS-WRK-FAM-OOP-REMAINING   PIC S9(07)V99 COMP-3.
           05  WS-WRK-DEDUCT-REMAINING    PIC S9(07)V99 COMP-3.
           05  WS-WRK-LINE-IDX            PIC 9(03) COMP-3.
           05  WS-WRK-MOD-IDX             PIC 9(02) COMP-3.
           05  WS-WRK-ADJ-IDX             PIC 9(02) COMP-3.
           05  WS-WRK-PROC-RANK           PIC 9(02) COMP-3.
           05  WS-WRK-SOS-FACILITY-PCT    PIC S9(01)V9(04) COMP-3
                                           VALUE +0.5000.
           05  WS-WRK-SOS-NON-FAC-PCT     PIC S9(01)V9(04) COMP-3
                                           VALUE +1.0000.
           05  WS-WRK-DRG-OUTLIER-MARG    PIC S9(01)V9(04) COMP-3
                                           VALUE +0.8000.
           05  WS-WRK-ESRD-COORD-MONTHS   PIC 9(02) COMP-3
                                           VALUE 30.
           05  WS-WRK-FED-PROMPT-DAYS     PIC 9(03) COMP-3
                                           VALUE 030.
           05  WS-WRK-FED-INT-RATE        PIC S9(01)V9(06) COMP-3
                                           VALUE +0.100000.

      *================================================================*
      * REPORT LINE DEFINITIONS                                        *
      *================================================================*
       01  WS-RPT-HEADER-1.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(50)
               VALUE 'NATIONAL HEALTH SYSTEMS - CLAIMS ADJUDICATION'.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  WS-RPT-H1-DATE             PIC X(10).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  FILLER                      PIC X(05) VALUE 'PAGE '.
           05  WS-RPT-H1-PAGE             PIC ZZ,ZZ9.
           05  FILLER                      PIC X(36) VALUE SPACES.

       01  WS-RPT-HEADER-2.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(50)
               VALUE 'BATCH ADJUDICATION PROCESSING REPORT'.
           05  FILLER                      PIC X(82) VALUE SPACES.

       01  WS-RPT-DETAIL-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-DT-LABEL            PIC X(45).
           05  FILLER                      PIC X(03) VALUE ' : '.
           05  WS-RPT-DT-VALUE            PIC X(84).

       01  WS-RPT-DOLLAR-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-DL-LABEL            PIC X(45).
           05  FILLER                      PIC X(03) VALUE ' : '.
           05  WS-RPT-DL-AMOUNT           PIC $$$,$$$,$$$,$$9.99.
           05  FILLER                      PIC X(64) VALUE SPACES.

       01  WS-RPT-COUNT-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-CL-LABEL            PIC X(45).
           05  FILLER                      PIC X(03) VALUE ' : '.
           05  WS-RPT-CL-COUNT            PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(71) VALUE SPACES.

       01  WS-RPT-SEPARATOR.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(80) VALUE ALL '-'.
           05  FILLER                      PIC X(52) VALUE SPACES.

       01  WS-RPT-PAGE-CTR                PIC 9(05) COMP-3 VALUE 0.
       01  WS-RPT-LINE-CTR                PIC 9(03) COMP-3 VALUE 99.
       01  WS-RPT-MAX-LINES               PIC 9(03) COMP-3 VALUE 55.

      *================================================================*
      * OUTPUT RECORD LAYOUTS                                          *
      *================================================================*
       01  WS-ADJ-OUTPUT-REC.
           05  WS-ADJ-CLAIM-ID            PIC X(15).
           05  WS-ADJ-MEMBER-ID           PIC X(12).
           05  WS-ADJ-PROVIDER-NPI        PIC X(10).
           05  WS-ADJ-STATUS              PIC X(02).
           05  WS-ADJ-TOTAL-BILLED        PIC S9(09)V99 COMP-3.
           05  WS-ADJ-TOTAL-ALLOWED       PIC S9(09)V99 COMP-3.
           05  WS-ADJ-TOTAL-DEDUCTIBLE    PIC S9(07)V99 COMP-3.
           05  WS-ADJ-TOTAL-COPAY         PIC S9(07)V99 COMP-3.
           05  WS-ADJ-TOTAL-COINSURANCE   PIC S9(07)V99 COMP-3.
           05  WS-ADJ-TOTAL-COB           PIC S9(07)V99 COMP-3.
           05  WS-ADJ-TOTAL-PAID          PIC S9(09)V99 COMP-3.
           05  WS-ADJ-TOTAL-WITHHOLD      PIC S9(07)V99 COMP-3.
           05  WS-ADJ-TOTAL-INTEREST      PIC S9(07)V99 COMP-3.
           05  WS-ADJ-NET-PAYMENT         PIC S9(09)V99 COMP-3.
           05  WS-ADJ-PATIENT-RESP        PIC S9(09)V99 COMP-3.
           05  WS-ADJ-PRICING-METHOD      PIC X(02).
           05  WS-ADJ-COB-ORDER           PIC X(01).
           05  WS-ADJ-PROCESS-DATE        PIC 9(08).
           05  WS-ADJ-DENY-REASON         PIC X(05).
           05  WS-ADJ-LINE-COUNT          PIC 9(03) COMP-3.
           05  WS-ADJ-FILLER              PIC X(1375).

       01  WS-PEND-OUTPUT-REC.
           05  WS-PND-CLAIM-ID            PIC X(15).
           05  WS-PND-MEMBER-ID           PIC X(12).
           05  WS-PND-PROVIDER-NPI        PIC X(10).
           05  WS-PND-PEND-REASON         PIC X(05).
           05  WS-PND-PEND-QUEUE          PIC X(03).
           05  WS-PND-PEND-DATE           PIC 9(08).
           05  WS-PND-EXPECTED-DATE       PIC 9(08).
           05  WS-PND-PRIORITY            PIC X(01).
           05  WS-PND-TOTAL-BILLED        PIC S9(09)V99 COMP-3.
           05  WS-PND-DESCRIPTION         PIC X(50).
           05  WS-PND-FILLER              PIC X(675).

       01  WS-PAY-OUTPUT-REC.
           05  WS-PYR-CLAIM-ID            PIC X(15).
           05  WS-PYR-PROVIDER-NPI        PIC X(10).
           05  WS-PYR-PROVIDER-TAX-ID     PIC X(09).
           05  WS-PYR-PAYMENT-DATE        PIC 9(08).
           05  WS-PYR-GROSS-PAYMENT       PIC S9(09)V99 COMP-3.
           05  WS-PYR-WITHHOLD            PIC S9(07)V99 COMP-3.
           05  WS-PYR-INTEREST            PIC S9(07)V99 COMP-3.
           05  WS-PYR-NET-PAYMENT         PIC S9(09)V99 COMP-3.
           05  WS-PYR-PATIENT-RESP        PIC S9(09)V99 COMP-3.
           05  WS-PYR-CHECK-NUMBER        PIC X(10).
           05  WS-PYR-FILLER              PIC X(405).

       01  WS-ERR-OUTPUT-REC.
           05  WS-EOR-CLAIM-ID            PIC X(15).
           05  WS-EOR-ERROR-CODE          PIC X(05).
           05  WS-EOR-ERROR-MSG           PIC X(80).
           05  WS-EOR-PARAGRAPH           PIC X(30).
           05  WS-EOR-SQLCODE             PIC -(09)9.
           05  WS-EOR-TIMESTAMP           PIC X(26).
           05  WS-EOR-FILLER              PIC X(224).

       01  WS-AUDIT-OUTPUT-REC.
           05  WS-AUD-CLAIM-ID            PIC X(15).
           05  WS-AUD-ACTION              PIC X(03).
           05  WS-AUD-TIMESTAMP           PIC X(26).
           05  WS-AUD-USER-ID             PIC X(08) VALUE 'BATCH'.
           05  WS-AUD-PARAGRAPH           PIC X(30).
           05  WS-AUD-OLD-VALUE           PIC X(50).
           05  WS-AUD-NEW-VALUE           PIC X(50).
           05  WS-AUD-FILLER              PIC X(318).

      *================================================================*
      * CLAIM DETAIL LINE TABLE (UP TO 999 LINES)                     *
      *================================================================*
       01  WS-CLAIM-LINES-TABLE.
           05  WS-CLM-LINE-ENTRY OCCURS 999 TIMES
               DEPENDING ON WS-CLM-LINE-COUNT.
               10  WS-CL-LINE-NUM         PIC 9(03) COMP-3.
               10  WS-CL-CPT-CODE         PIC X(05).
               10  WS-CL-REVENUE-CODE     PIC X(04).
               10  WS-CL-MODIFIERS.
                   15  WS-CL-MODIFIER     PIC X(02) OCCURS 4.
               10  WS-CL-FROM-DATE        PIC 9(08).
               10  WS-CL-THRU-DATE        PIC 9(08).
               10  WS-CL-UNITS            PIC S9(05)V99 COMP-3.
               10  WS-CL-BILLED-AMT       PIC S9(07)V99 COMP-3.
               10  WS-CL-ALLOWED-AMT      PIC S9(07)V99 COMP-3.
               10  WS-CL-DEDUCT-AMT       PIC S9(07)V99 COMP-3.
               10  WS-CL-COPAY-AMT        PIC S9(07)V99 COMP-3.
               10  WS-CL-COINS-AMT        PIC S9(07)V99 COMP-3.
               10  WS-CL-COB-AMT          PIC S9(07)V99 COMP-3.
               10  WS-CL-PAID-AMT         PIC S9(07)V99 COMP-3.
               10  WS-CL-PATIENT-RESP     PIC S9(07)V99 COMP-3.
               10  WS-CL-PRICING-METHOD   PIC X(02).
               10  WS-CL-STATUS           PIC X(02).
               10  WS-CL-DENY-REASON      PIC X(05).
       01  WS-CLM-LINE-COUNT              PIC 9(03) COMP-3.

      *================================================================*
      * PROCEDURE DIVISION                                             *
      *================================================================*
       PROCEDURE DIVISION.

       0000-MAIN-CONTROL.
      *---------------------------------------------------------------*
      * MAIN CONTROL - ORCHESTRATES THE ENTIRE ADJUDICATION PROCESS   *
      *---------------------------------------------------------------*
           PERFORM 1000-INITIALIZATION
              THRU 1000-INITIALIZATION-EXIT

           PERFORM 2000-MAIN-PROCESS-LOOP
              THRU 2000-MAIN-PROCESS-LOOP-EXIT
              UNTIL END-OF-FILE

           PERFORM 9000-TERMINATION
              THRU 9000-TERMINATION-EXIT

           STOP RUN
           .
       0000-MAIN-CONTROL-EXIT.
           EXIT.

      *================================================================*
      * 1000 - INITIALIZATION                                          *
      *================================================================*
       1000-INITIALIZATION.
      *---------------------------------------------------------------*
      * OPEN ALL FILES, CONNECT TO DATABASE, LOAD REFERENCE TABLES,   *
      * SET UP DATE FIELDS AND PROCESSING CONTROL VARIABLES.           *
      *---------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME

           MOVE WS-CURRENT-TIME TO WS-PROCESS-START-TIME

           STRING WS-CURRENT-YEAR '-'
                  WS-CURRENT-MONTH '-'
                  WS-CURRENT-DAY
                  DELIMITED BY SIZE
                  INTO WS-BATCH-RUN-DATE-X

           STRING WS-CURRENT-YEAR
                  WS-CURRENT-MONTH
                  WS-CURRENT-DAY
                  DELIMITED BY SIZE
                  INTO WS-BATCH-RUN-DATE

           DISPLAY 'HCCLMADJ - CLAIMS ADJUDICATION ENGINE'
           DISPLAY 'BATCH RUN DATE: ' WS-BATCH-RUN-DATE-X
           DISPLAY 'START TIME:     ' WS-CURRENT-HOUR ':'
                                      WS-CURRENT-MINUTE ':'
                                      WS-CURRENT-SECOND
           DISPLAY '================================================'

      *--- OPEN INPUT FILES ---
           OPEN INPUT CLAIM-INPUT-FILE
           IF NOT CLMIN-OK
               MOVE 'CLAIM-INPUT-FILE OPEN FAILED'
                 TO WS-ERR-MESSAGE
               MOVE WS-CLMIN-STATUS TO WS-ERR-FILE-STATUS
               MOVE '1000-INITIALIZATION' TO WS-ERR-PARAGRAPH
               SET ERR-FATAL TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
               MOVE 'U001' TO WS-ABEND-CODE
               PERFORM 9999-ABEND-PROGRAM
                  THRU 9999-ABEND-PROGRAM-EXIT
           END-IF

           OPEN INPUT REFERENCE-DATA-FILE
           IF NOT REFDATA-OK
               DISPLAY 'WARNING: REFERENCE DATA FILE OPEN FAILED'
               DISPLAY '  FILE STATUS: ' WS-REFDATA-STATUS
               DISPLAY '  CONTINUING WITH DATABASE ONLY'
           END-IF

      *--- OPEN OUTPUT FILES ---
           OPEN OUTPUT ADJUDICATED-OUTPUT-FILE
           IF NOT ADJOUT-OK
               MOVE 'ADJUDICATED-OUTPUT-FILE OPEN FAILED'
                 TO WS-ERR-MESSAGE
               MOVE WS-ADJOUT-STATUS TO WS-ERR-FILE-STATUS
               MOVE '1000-INITIALIZATION' TO WS-ERR-PARAGRAPH
               SET ERR-FATAL TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
               MOVE 'U002' TO WS-ABEND-CODE
               PERFORM 9999-ABEND-PROGRAM
                  THRU 9999-ABEND-PROGRAM-EXIT
           END-IF

           OPEN OUTPUT PEND-FILE
           IF NOT PEND-OK
               MOVE 'PEND-FILE OPEN FAILED' TO WS-ERR-MESSAGE
               MOVE WS-PEND-STATUS TO WS-ERR-FILE-STATUS
               MOVE '1000-INITIALIZATION' TO WS-ERR-PARAGRAPH
               SET ERR-FATAL TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
               MOVE 'U003' TO WS-ABEND-CODE
               PERFORM 9999-ABEND-PROGRAM
                  THRU 9999-ABEND-PROGRAM-EXIT
           END-IF

           OPEN OUTPUT APPEAL-FILE
           IF NOT APPEAL-OK
               MOVE 'APPEAL-FILE OPEN FAILED' TO WS-ERR-MESSAGE
               MOVE WS-APPEAL-STATUS TO WS-ERR-FILE-STATUS
               MOVE '1000-INITIALIZATION' TO WS-ERR-PARAGRAPH
               SET ERR-FATAL TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
               MOVE 'U004' TO WS-ABEND-CODE
               PERFORM 9999-ABEND-PROGRAM
                  THRU 9999-ABEND-PROGRAM-EXIT
           END-IF

           OPEN OUTPUT PAYMENT-FILE
           IF NOT PAY-OK
               MOVE 'PAYMENT-FILE OPEN FAILED' TO WS-ERR-MESSAGE
               MOVE WS-PAY-STATUS TO WS-ERR-FILE-STATUS
               MOVE '1000-INITIALIZATION' TO WS-ERR-PARAGRAPH
               SET ERR-FATAL TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
               MOVE 'U005' TO WS-ABEND-CODE
               PERFORM 9999-ABEND-PROGRAM
                  THRU 9999-ABEND-PROGRAM-EXIT
           END-IF

           OPEN OUTPUT ERROR-FILE
           IF NOT ERR-OK
               DISPLAY 'WARNING: ERROR FILE OPEN FAILED - '
                       WS-ERR-STATUS
           END-IF

           OPEN OUTPUT REPORT-FILE
           IF NOT RPT-OK
               DISPLAY 'WARNING: REPORT FILE OPEN FAILED - '
                       WS-RPT-STATUS
           END-IF

           OPEN OUTPUT AUDIT-TRAIL-FILE
           IF NOT AUDIT-OK
               DISPLAY 'WARNING: AUDIT FILE OPEN FAILED - '
                       WS-AUDIT-STATUS
           END-IF

      *--- CONNECT TO SYBASE DATABASE ---
           EXEC SQL
               CONNECT TO CLMPROCDB
               USER 'clmbatch' IDENTIFIED BY 'clmb@tch94'
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE NOT = 0
               MOVE 'DATABASE CONNECT FAILED' TO WS-ERR-MESSAGE
               MOVE WS-SQLCODE TO WS-ERR-SQLCODE
               MOVE '1000-INITIALIZATION' TO WS-ERR-PARAGRAPH
               SET ERR-FATAL TO TRUE
               PERFORM 8900-DATABASE-ERROR
                  THRU 8900-DATABASE-ERROR-EXIT
               MOVE 'U010' TO WS-ABEND-CODE
               PERFORM 9999-ABEND-PROGRAM
                  THRU 9999-ABEND-PROGRAM-EXIT
           ELSE
               SET DB-IS-CONNECTED TO TRUE
               DISPLAY 'DATABASE CONNECTION ESTABLISHED'
           END-IF

      *--- SET TRANSACTION ISOLATION LEVEL ---
           EXEC SQL
               SET TRANSACTION ISOLATION LEVEL READ COMMITTED
           END-EXEC

      *--- LOAD STATE PROMPT PAY TABLE INTO MEMORY ---
           PERFORM 1100-LOAD-STATE-PROMPT-PAY
              THRU 1100-LOAD-STATE-PROMPT-PAY-EXIT

      *--- INITIALIZE MULTIPLE PROCEDURE TABLE ---
           MOVE 1 TO WS-MULT-PROC-RANK(1)
           MOVE +1.0000 TO WS-MULT-PROC-PCT(1)
           MOVE 2 TO WS-MULT-PROC-RANK(2)
           MOVE +0.5000 TO WS-MULT-PROC-PCT(2)
           MOVE 3 TO WS-MULT-PROC-RANK(3)
           MOVE +0.5000 TO WS-MULT-PROC-PCT(3)
           MOVE 4 TO WS-MULT-PROC-RANK(4)
           MOVE +0.5000 TO WS-MULT-PROC-PCT(4)
           MOVE 5 TO WS-MULT-PROC-RANK(5)
           MOVE +0.5000 TO WS-MULT-PROC-PCT(5)
           MOVE 6 TO WS-MULT-PROC-RANK(6)
           MOVE +0.5000 TO WS-MULT-PROC-PCT(6)

      *--- WRITE REPORT HEADERS ---
           MOVE WS-BATCH-RUN-DATE-X TO WS-RPT-H1-DATE
           MOVE 1 TO WS-RPT-PAGE-CTR
           MOVE WS-RPT-PAGE-CTR TO WS-RPT-H1-PAGE
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-HEADER-1
               AFTER ADVANCING PAGE-TOP
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-HEADER-2
               AFTER ADVANCING 1 LINE
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR
               AFTER ADVANCING 1 LINE
           MOVE 4 TO WS-RPT-LINE-CTR

      *--- PRIME THE READ ---
           READ CLAIM-INPUT-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-CTR-CLAIMS-READ
           END-READ
           .
       1000-INITIALIZATION-EXIT.
           EXIT.

      *================================================================*
      * 1100 - LOAD STATE PROMPT PAY TABLE                             *
      *================================================================*
       1100-LOAD-STATE-PROMPT-PAY.
      *---------------------------------------------------------------*
      * LOAD STATE-SPECIFIC PROMPT PAY REQUIREMENTS FROM DATABASE.     *
      * EACH STATE HAS DIFFERENT CLEAN CLAIM DAYS AND INTEREST RATES. *
      *---------------------------------------------------------------*
           MOVE ZEROS TO WS-WRK-INDEX-1

           EXEC SQL
               DECLARE PROMPT_PAY_CURSOR CURSOR FOR
               SELECT STATE_CODE,
                      CLEAN_CLAIM_DAYS,
                      UNCLEAN_CLAIM_DAYS,
                      INTEREST_RATE,
                      PENALTY_PERCENT
               FROM   STATE_PROMPT_PAY_RULES
               WHERE  EFFECTIVE_DATE <= :WS-BATCH-RUN-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE > :WS-BATCH-RUN-DATE)
               ORDER BY STATE_CODE
           END-EXEC

           EXEC SQL
               OPEN PROMPT_PAY_CURSOR
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE NOT = 0
               DISPLAY 'WARNING: COULD NOT LOAD STATE PROMPT PAY'
               DISPLAY '  SQLCODE: ' WS-SQLCODE
               GO TO 1100-LOAD-STATE-PROMPT-PAY-EXIT
           END-IF

           PERFORM UNTIL WS-SQLCODE = +100
                      OR WS-WRK-INDEX-1 >= 52
               EXEC SQL
                   FETCH PROMPT_PAY_CURSOR
                   INTO :WS-SPP-STATE-CODE(WS-WRK-INDEX-1 + 1),
                        :WS-SPP-CLEAN-DAYS(WS-WRK-INDEX-1 + 1),
                        :WS-SPP-UNCLEAN-DAYS(WS-WRK-INDEX-1 + 1),
                        :WS-SPP-INT-RATE(WS-WRK-INDEX-1 + 1),
                        :WS-SPP-PENALTY-PCT(WS-WRK-INDEX-1 + 1)
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               IF WS-SQLCODE = 0
                   ADD 1 TO WS-WRK-INDEX-1
               END-IF
           END-PERFORM

           EXEC SQL
               CLOSE PROMPT_PAY_CURSOR
           END-EXEC

           DISPLAY 'LOADED ' WS-WRK-INDEX-1
                   ' STATE PROMPT PAY RULES'
           .
       1100-LOAD-STATE-PROMPT-PAY-EXIT.
           EXIT.

      *================================================================*
      * 2000 - MAIN PROCESS LOOP                                       *
      *================================================================*
       2000-MAIN-PROCESS-LOOP.
      *---------------------------------------------------------------*
      * READ EACH CLAIM, VALIDATE, PRICE, APPLY BENEFITS, AND WRITE  *
      * OUTPUT. ROUTE BY CLAIM TYPE FOR APPROPRIATE PROCESSING.        *
      *---------------------------------------------------------------*
           MOVE WS-CURRENT-TIME TO WS-CLAIM-START-TIME

      *--- INITIALIZE PER-CLAIM FIELDS ---
           SET CLAIM-IS-VALID TO TRUE
           SET CLAIM-SHOULD-NOT-PEND TO TRUE
           SET PRICING-NOT-FOUND TO TRUE
           SET COB-NOT-REQUIRED TO TRUE
           SET SERVICE-NOT-CAPITATED TO TRUE
           SET DEDUCTIBLE-NOT-MET TO TRUE
           SET OOP-MAX-NOT-MET TO TRUE
           SET FAMILY-DEDUCT-NOT-MET TO TRUE
           SET FAMILY-OOP-NOT-MET TO TRUE
           SET NOT-OUTLIER-CASE TO TRUE
           SET NOT-TRANSFER-DRG TO TRUE
           SET NOT-IN-GLOBAL-PERIOD TO TRUE
           SET IS-CLEAN-CLAIM TO TRUE
           SET WE-ARE-PRIMARY TO TRUE
           SET NOT-PREVENTIVE-CARE TO TRUE
           SET NOT-COVID-RELATED TO TRUE
           SET NOT-TELEHEALTH TO TRUE
           SET NO-SURPRISE-NA TO TRUE

           INITIALIZE WS-CLAIM-HEADER
           INITIALIZE WS-CLAIM-DETAIL
           INITIALIZE WS-PRICING-FIELDS
           INITIALIZE WS-DRG-PRICING
           INITIALIZE WS-PER-DIEM-PRICING
           INITIALIZE WS-CASE-RATE-PRICING
           INITIALIZE WS-PCT-CHARGE-PRICING
           INITIALIZE WS-COB-FIELDS
           INITIALIZE WS-PAYMENT-FIELDS
           INITIALIZE WS-PEND-FIELDS

      *--- PARSE CLAIM INPUT RECORD ---
           PERFORM 2100-PARSE-CLAIM-INPUT
              THRU 2100-PARSE-CLAIM-INPUT-EXIT

           IF CLAIM-IS-INVALID
               ADD 1 TO WS-CTR-CLAIMS-ERROR
               PERFORM 8500-WRITE-OUTPUT-FILES
                  THRU 8500-WRITE-OUTPUT-FILES-EXIT
               GO TO 2000-READ-NEXT
           END-IF

      *--- LOOK UP MEMBER ELIGIBILITY ---
           PERFORM 2200-LOOKUP-MEMBER
              THRU 2200-LOOKUP-MEMBER-EXIT

           IF CLAIM-IS-INVALID
               MOVE 'DN' TO WS-CLM-STATUS
               MOVE 'E0001' TO WS-DTL-DENY-REASON
               ADD 1 TO WS-CTR-CLAIMS-DENIED
               PERFORM 8500-WRITE-OUTPUT-FILES
                  THRU 8500-WRITE-OUTPUT-FILES-EXIT
               PERFORM 8000-UPDATE-DATABASE
                  THRU 8000-UPDATE-DATABASE-EXIT
               GO TO 2000-READ-NEXT
           END-IF

      *--- LOOK UP PROVIDER INFORMATION ---
           PERFORM 2300-LOOKUP-PROVIDER
              THRU 2300-LOOKUP-PROVIDER-EXIT

           IF CLAIM-IS-INVALID
               MOVE 'DN' TO WS-CLM-STATUS
               MOVE 'P0001' TO WS-DTL-DENY-REASON
               ADD 1 TO WS-CTR-CLAIMS-DENIED
               PERFORM 8500-WRITE-OUTPUT-FILES
                  THRU 8500-WRITE-OUTPUT-FILES-EXIT
               PERFORM 8000-UPDATE-DATABASE
                  THRU 8000-UPDATE-DATABASE-EXIT
               GO TO 2000-READ-NEXT
           END-IF

      *--- LOAD BENEFIT PLAN ---
           PERFORM 2400-LOAD-BENEFIT-PLAN
              THRU 2400-LOAD-BENEFIT-PLAN-EXIT

      *--- CHECK FOR AUTHORIZATION ---
           PERFORM 2500-CHECK-AUTHORIZATION
              THRU 2500-CHECK-AUTHORIZATION-EXIT

           IF AUTH-IS-REQUIRED AND AUTH-NOT-FOUND
               SET CLAIM-SHOULD-PEND TO TRUE
               MOVE 'A0001' TO WS-PEND-REASON-CODE
               MOVE 'AUTHORIZATION NOT FOUND OR EXPIRED'
                 TO WS-PEND-REASON-DESC
               SET PEND-AUTH-REVIEW TO TRUE
           END-IF

      *--- CHECK FOR DUPLICATE CLAIM ---
           PERFORM 2600-CHECK-DUPLICATE
              THRU 2600-CHECK-DUPLICATE-EXIT

           IF CLAIM-IS-INVALID
               MOVE 'DN' TO WS-CLM-STATUS
               MOVE 'D0001' TO WS-DTL-DENY-REASON
               ADD 1 TO WS-CTR-CLAIMS-DENIED
               PERFORM 8500-WRITE-OUTPUT-FILES
                  THRU 8500-WRITE-OUTPUT-FILES-EXIT
               PERFORM 8000-UPDATE-DATABASE
                  THRU 8000-UPDATE-DATABASE-EXIT
               GO TO 2000-READ-NEXT
           END-IF

      *--- CHECK FOR PREVENTIVE CARE (ACA) ---
           PERFORM 2700-CHECK-PREVENTIVE-CARE
              THRU 2700-CHECK-PREVENTIVE-CARE-EXIT

      *--- CHECK FOR COVID-RELATED SERVICES ---
           PERFORM 2710-CHECK-COVID-SERVICES
              THRU 2710-CHECK-COVID-SERVICES-EXIT

      *--- CHECK FOR TELEHEALTH MODIFIER ---
           IF POS-TELEHEALTH OR WS-DTL-MODIFIER(1) = '95'
              OR WS-DTL-MODIFIER(2) = '95'
              OR WS-CLM-SUB-TYPE = 'TH'
               SET IS-TELEHEALTH TO TRUE
           END-IF

      *--- ROUTE TO PRICING ---
           EVALUATE TRUE
               WHEN CLM-TYPE-PROFESSIONAL
                   ADD 1 TO WS-CTR-PROFESSIONAL
                   PERFORM 3000-DETERMINE-PRICING-METHOD
                      THRU 3000-DETERMINE-PRICING-METHOD-EXIT
               WHEN CLM-TYPE-INSTITUTIONAL
                   ADD 1 TO WS-CTR-INSTITUTIONAL
                   IF CLM-INPATIENT
                       PERFORM 3000-DETERMINE-PRICING-METHOD
                          THRU 3000-DETERMINE-PRICING-METHOD-EXIT
                   ELSE
                       PERFORM 3000-DETERMINE-PRICING-METHOD
                          THRU 3000-DETERMINE-PRICING-METHOD-EXIT
                   END-IF
               WHEN CLM-TYPE-DENTAL
                   PERFORM 3000-DETERMINE-PRICING-METHOD
                      THRU 3000-DETERMINE-PRICING-METHOD-EXIT
               WHEN OTHER
                   MOVE 'UNKNOWN CLAIM TYPE'
                     TO WS-ERR-MESSAGE
                   MOVE '2000-MAIN-PROCESS-LOOP'
                     TO WS-ERR-PARAGRAPH
                   SET ERR-WARNING TO TRUE
                   PERFORM 8800-ERROR-HANDLER
                      THRU 8800-ERROR-HANDLER-EXIT
                   SET CLAIM-SHOULD-PEND TO TRUE
                   MOVE 'X0001' TO WS-PEND-REASON-CODE
                   MOVE 'UNKNOWN CLAIM TYPE FOR PRICING'
                     TO WS-PEND-REASON-DESC
                   SET PEND-PRICING-REVIEW TO TRUE
           END-EVALUATE

      *--- IF CLAIM SHOULD PEND, ROUTE TO PEND PROCESSING ---
           IF CLAIM-SHOULD-PEND
               PERFORM 7000-PEND-CLAIM-PROCESSING
                  THRU 7000-PEND-CLAIM-PROCESSING-EXIT
               ADD 1 TO WS-CTR-CLAIMS-PENDED
               PERFORM 8500-WRITE-OUTPUT-FILES
                  THRU 8500-WRITE-OUTPUT-FILES-EXIT
               PERFORM 8000-UPDATE-DATABASE
                  THRU 8000-UPDATE-DATABASE-EXIT
               GO TO 2000-READ-NEXT
           END-IF

      *--- APPLY MEMBER COST SHARING ---
      * PREVENTIVE CARE UNDER ACA: NO COST SHARING FOR IN-NETWORK
      * COVID VACCINE/TESTING: NO COST SHARING PER FEDERAL RULES
           IF (IS-PREVENTIVE-CARE AND MBR-IN-NETWORK
               AND NOT-GRANDFATHERED)
              OR (IS-COVID-RELATED AND NOT-GRANDFATHERED)
               MOVE +0 TO WS-DTL-DEDUCTIBLE-AMT
               MOVE +0 TO WS-DTL-COPAY-AMOUNT
               MOVE +0 TO WS-DTL-COINSURANCE-AMT
               MOVE WS-DTL-ALLOWED-AMOUNT TO WS-DTL-PAID-AMOUNT
           ELSE
               PERFORM 4000-APPLY-MEMBER-COST-SHARING
                  THRU 4000-APPLY-MEMBER-COST-SHARING-EXIT
           END-IF

      *--- COORDINATION OF BENEFITS ---
           IF COB-IS-REQUIRED
               ADD 1 TO WS-CTR-COB-CLAIMS
               PERFORM 5000-COORDINATION-OF-BENEFITS
                  THRU 5000-COORDINATION-OF-BENEFITS-EXIT
           END-IF

      *--- CALCULATE PROVIDER PAYMENT ---
           PERFORM 6000-CALCULATE-PROVIDER-PAYMENT
              THRU 6000-CALCULATE-PROVIDER-PAYMENT-EXIT

      *--- DETERMINE FINAL CLAIM STATUS ---
           IF WS-PAY-NET-AMOUNT > 0
               SET CLM-STATUS-PAID TO TRUE
               ADD 1 TO WS-CTR-CLAIMS-PAID
           ELSE
               IF WS-PAY-NET-AMOUNT = 0
                   ADD 1 TO WS-CTR-CLAIMS-ZERO-PAY
                   IF SERVICE-IS-CAPITATED
                       SET CLM-STATUS-PAID TO TRUE
                   ELSE
                       SET CLM-STATUS-DENIED TO TRUE
                       ADD 1 TO WS-CTR-CLAIMS-DENIED
                   END-IF
               ELSE
                   SET CLM-STATUS-PAID TO TRUE
                   ADD 1 TO WS-CTR-CLAIMS-PAID
               END-IF
           END-IF

      *--- UPDATE BATCH ACCUMULATORS ---
           ADD WS-CLM-TOTAL-CHARGE    TO WS-TOT-BILLED-AMOUNT
           ADD WS-DTL-ALLOWED-AMOUNT  TO WS-TOT-ALLOWED-AMOUNT
           ADD WS-DTL-DEDUCTIBLE-AMT  TO WS-TOT-DEDUCTIBLE-AMOUNT
           ADD WS-DTL-COPAY-AMOUNT    TO WS-TOT-COPAY-AMOUNT
           ADD WS-DTL-COINSURANCE-AMT TO WS-TOT-COINSURANCE-AMOUNT
           ADD WS-PAY-NET-AMOUNT      TO WS-TOT-PAID-AMOUNT
           ADD WS-PAY-WITHHOLD-AMT    TO WS-TOT-WITHHOLD-AMOUNT
           ADD WS-PAY-INTEREST-AMT    TO WS-TOT-INTEREST-AMOUNT
           ADD WS-PAY-PATIENT-RESP    TO WS-TOT-PATIENT-RESP
           ADD WS-COB-SAVINGS         TO WS-TOT-COB-SAVINGS

           ADD 1 TO WS-CTR-CLAIMS-PROCESSED

      *--- WRITE OUTPUT FILES AND UPDATE DATABASE ---
           PERFORM 8500-WRITE-OUTPUT-FILES
              THRU 8500-WRITE-OUTPUT-FILES-EXIT
           PERFORM 8000-UPDATE-DATABASE
              THRU 8000-UPDATE-DATABASE-EXIT
           .
       2000-READ-NEXT.
      *--- READ NEXT CLAIM ---
           READ CLAIM-INPUT-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-CTR-CLAIMS-READ
           END-READ
           .
       2000-MAIN-PROCESS-LOOP-EXIT.
           EXIT.

      *================================================================*
      * 2100 - PARSE CLAIM INPUT                                       *
      *================================================================*
       2100-PARSE-CLAIM-INPUT.
      *---------------------------------------------------------------*
      * PARSE THE INBOUND CLAIM RECORD INTO WORKING STORAGE FIELDS.   *
      * VALIDATE REQUIRED FIELDS ARE PRESENT AND PROPERLY FORMATTED.   *
      *---------------------------------------------------------------*
           IF NOT CI-IS-HEADER
               MOVE 'EXPECTED HEADER RECORD NOT FOUND'
                 TO WS-ERR-MESSAGE
               MOVE '2100-PARSE-CLAIM-INPUT' TO WS-ERR-PARAGRAPH
               SET ERR-WARNING TO TRUE
               SET CLAIM-IS-INVALID TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
               GO TO 2100-PARSE-CLAIM-INPUT-EXIT
           END-IF

           MOVE CI-CLAIM-ID TO WS-CLM-ID

           MOVE CI-CLAIM-DATA(1:2) TO WS-CLM-TYPE
           MOVE CI-CLAIM-DATA(3:2) TO WS-CLM-SUB-TYPE
           MOVE CI-CLAIM-DATA(5:1) TO WS-CLM-FREQUENCY-CODE
           MOVE CI-CLAIM-DATA(6:8) TO WS-CLM-RECEIVED-DATE
           MOVE CI-CLAIM-DATA(14:8) TO WS-CLM-FROM-DATE
           MOVE CI-CLAIM-DATA(22:8) TO WS-CLM-THRU-DATE
           MOVE CI-CLAIM-DATA(30:8) TO WS-CLM-ADMIT-DATE
           MOVE CI-CLAIM-DATA(38:8) TO WS-CLM-DISCHARGE-DATE
           MOVE CI-CLAIM-DATA(46:2) TO WS-CLM-DISCHARGE-STATUS
           MOVE CI-CLAIM-DATA(48:12) TO WS-MBR-ID
           MOVE CI-CLAIM-DATA(60:10) TO WS-PROV-NPI
           MOVE CI-CLAIM-DATA(70:4) TO WS-CLM-TYPE-OF-BILL
           MOVE CI-CLAIM-DATA(74:2) TO WS-CLM-PLACE-OF-SERVICE
           MOVE CI-CLAIM-DATA(76:7) TO WS-CLM-PRINCIPAL-DIAG
           MOVE CI-CLAIM-DATA(83:7) TO WS-CLM-ADMIT-DIAG
           MOVE CI-CLAIM-DATA(90:4) TO WS-CLM-DRG-CODE
           MOVE CI-CLAIM-DATA(94:2) TO WS-CLM-SOURCE
           MOVE CI-CLAIM-DATA(96:3) TO WS-CLM-LINE-COUNT

      *--- VALIDATE CLAIM ID ---
           IF WS-CLM-ID = SPACES OR WS-CLM-ID = LOW-VALUES
               MOVE 'CLAIM ID IS MISSING' TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

      *--- VALIDATE MEMBER ID ---
           IF WS-MBR-ID = SPACES OR WS-MBR-ID = LOW-VALUES
               MOVE 'MEMBER ID IS MISSING' TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

      *--- VALIDATE PROVIDER NPI ---
           IF WS-PROV-NPI = SPACES OR WS-PROV-NPI = LOW-VALUES
               MOVE 'PROVIDER NPI IS MISSING' TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

      *--- VALIDATE DATES ---
           IF WS-CLM-FROM-DATE = ZEROS
               MOVE 'SERVICE FROM DATE IS MISSING'
                 TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

           IF WS-CLM-FROM-DATE > WS-BATCH-RUN-DATE
               MOVE 'SERVICE DATE IS IN THE FUTURE'
                 TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

      *--- VALIDATE CLAIM TYPE ---
           IF NOT (CLM-TYPE-PROFESSIONAL OR CLM-TYPE-INSTITUTIONAL
                   OR CLM-TYPE-DENTAL OR CLM-TYPE-PHARMACY)
               MOVE 'INVALID CLAIM TYPE' TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

      *--- PARSE TOTAL BILLED CHARGE ---
           MOVE CI-CLAIM-DATA(99:11) TO WS-CLM-TOTAL-CHARGE

      *--- VALIDATE BILLED AMOUNT ---
           IF WS-CLM-TOTAL-CHARGE <= ZEROS
               MOVE 'BILLED AMOUNT IS ZERO OR NEGATIVE'
                 TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

      *--- PARSE DIAGNOSIS CODES ---
           PERFORM VARYING WS-WRK-INDEX-1 FROM 1 BY 1
               UNTIL WS-WRK-INDEX-1 > 25
               MOVE CI-CLAIM-DATA(110 + ((WS-WRK-INDEX-1 - 1) * 8)
                    :7)
                 TO WS-CLM-DIAG-CODE(WS-WRK-INDEX-1)
               MOVE CI-CLAIM-DATA(117 + ((WS-WRK-INDEX-1 - 1) * 8)
                    :1)
                 TO WS-CLM-DIAG-POA(WS-WRK-INDEX-1)
           END-PERFORM

      *--- PARSE LINE COUNT AND VALIDATE ---
           IF WS-CLM-LINE-COUNT < 1 OR WS-CLM-LINE-COUNT > 999
               MOVE 'INVALID LINE COUNT' TO WS-ERR-MESSAGE
               SET CLAIM-IS-INVALID TO TRUE
           END-IF

           IF CLAIM-IS-INVALID
               MOVE '2100-PARSE-CLAIM-INPUT' TO WS-ERR-PARAGRAPH
               SET ERR-WARNING TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
           END-IF
           .
       2100-PARSE-CLAIM-INPUT-EXIT.
           EXIT.

      *================================================================*
      * 2200 - LOOKUP MEMBER                                           *
      *================================================================*
       2200-LOOKUP-MEMBER.
      *---------------------------------------------------------------*
      * LOOK UP MEMBER DEMOGRAPHICS AND ELIGIBILITY FROM DATABASE.     *
      * VERIFY MEMBER WAS ELIGIBLE ON DATE OF SERVICE.                 *
      *---------------------------------------------------------------*
           MOVE WS-MBR-ID TO HV-MEMBER-ID

           EXEC SQL
               SELECT M.LAST_NAME,
                      M.FIRST_NAME,
                      M.DATE_OF_BIRTH,
                      M.GENDER,
                      M.RELATIONSHIP_CODE,
                      E.GROUP_ID,
                      E.PLAN_CODE,
                      E.PLAN_TYPE,
                      E.EFFECTIVE_DATE,
                      E.TERMINATION_DATE,
                      E.COBRA_INDICATOR,
                      E.MEDICARE_INDICATOR,
                      E.MEDICARE_REASON,
                      E.ESRD_START_DATE,
                      E.EMPLOYER_SIZE,
                      M.SUBSCRIBER_DOB
               INTO   :WS-MBR-LAST-NAME,
                      :WS-MBR-FIRST-NAME,
                      :WS-MBR-DOB,
                      :WS-MBR-GENDER,
                      :WS-MBR-RELATIONSHIP,
                      :WS-MBR-GROUP-ID,
                      :WS-MBR-PLAN-CODE,
                      :WS-MBR-PLAN-TYPE,
                      :WS-MBR-EFF-DATE,
                      :WS-MBR-TERM-DATE,
                      :WS-MBR-COBRA-SW,
                      :WS-MBR-MEDICARE-SW,
                      :WS-MBR-MEDICARE-REASON,
                      :WS-MBR-ESRD-START-DATE,
                      :WS-MBR-EMPLOYER-SIZE,
                      :WS-MBR-SUBSCRIBER-DOB
               FROM   MEMBER M
               JOIN   ELIGIBILITY E
                 ON   M.MEMBER_ID = E.MEMBER_ID
               WHERE  M.MEMBER_ID = :HV-MEMBER-ID
               AND    E.EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (E.TERMINATION_DATE IS NULL
                  OR   E.TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           EVALUATE WS-SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'MEMBER NOT FOUND OR NOT ELIGIBLE'
                     TO WS-ERR-MESSAGE
                   MOVE '2200-LOOKUP-MEMBER' TO WS-ERR-PARAGRAPH
                   SET ERR-WARNING TO TRUE
                   SET CLAIM-IS-INVALID TO TRUE
                   PERFORM 8800-ERROR-HANDLER
                      THRU 8800-ERROR-HANDLER-EXIT
                   GO TO 2200-LOOKUP-MEMBER-EXIT
               WHEN OTHER
                   MOVE 'MEMBER LOOKUP SQL ERROR'
                     TO WS-ERR-MESSAGE
                   MOVE WS-SQLCODE TO WS-ERR-SQLCODE
                   MOVE '2200-LOOKUP-MEMBER' TO WS-ERR-PARAGRAPH
                   SET ERR-SEVERE TO TRUE
                   PERFORM 8900-DATABASE-ERROR
                      THRU 8900-DATABASE-ERROR-EXIT
                   SET CLAIM-IS-INVALID TO TRUE
                   GO TO 2200-LOOKUP-MEMBER-EXIT
           END-EVALUATE

      *--- CALCULATE MEMBER AGE ---
           COMPUTE WS-MBR-AGE =
               (WS-CLM-FROM-DATE / 10000) -
               (WS-MBR-DOB / 10000)

      *--- CHECK IF CLAIM IS WITHIN TIMELY FILING ---
           COMPUTE WS-WRK-DAYS-1 =
               (WS-CLM-RECEIVED-DATE - WS-CLM-FROM-DATE) / 10000
                * 365
           IF WS-WRK-DAYS-1 > 365
               SET CLAIM-IS-INVALID TO TRUE
               MOVE 'CLAIM EXCEEDS TIMELY FILING LIMIT'
                 TO WS-ERR-MESSAGE
               SET ERR-WARNING TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
           END-IF

      *--- DETERMINE NETWORK STATUS BASED ON PROVIDER ---
           IF PROV-IS-PAR
               SET MBR-IN-NETWORK TO TRUE
           ELSE
               IF PROV-IS-NON-PAR
                   SET MBR-OUT-OF-NETWORK TO TRUE
               ELSE
                   SET MBR-OUT-OF-NETWORK TO TRUE
               END-IF
           END-IF

      *--- CHECK FOR OTHER COVERAGE (COB) ---
           EXEC SQL
               SELECT OTHER_CARRIER_PLAN,
                      OTHER_CARRIER_PAID,
                      OTHER_CARRIER_ALLOWED,
                      OTHER_CARRIER_DEDUCTIBLE,
                      OTHER_CARRIER_COPAY,
                      OTHER_CARRIER_COINSURANCE
               INTO   :WS-COB-OTHER-PLAN,
                      :WS-COB-OTHER-PAID,
                      :WS-COB-OTHER-ALLOWED,
                      :WS-COB-OTHER-DEDUCT,
                      :WS-COB-OTHER-COPAY,
                      :WS-COB-OTHER-COINS
               FROM   MEMBER_OTHER_COVERAGE
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
               SET COB-IS-REQUIRED TO TRUE
           ELSE
               SET COB-NOT-REQUIRED TO TRUE
           END-IF
           .
       2200-LOOKUP-MEMBER-EXIT.
           EXIT.

      *================================================================*
      * 2300 - LOOKUP PROVIDER                                         *
      *================================================================*
       2300-LOOKUP-PROVIDER.
      *---------------------------------------------------------------*
      * LOOK UP PROVIDER INFORMATION, NETWORK STATUS, AND CONTRACT.   *
      *---------------------------------------------------------------*
           MOVE WS-PROV-NPI TO HV-PROVIDER-NPI

           EXEC SQL
               SELECT P.TAX_ID,
                      P.LAST_NAME,
                      P.FIRST_NAME,
                      P.SPECIALTY_CODE,
                      P.PROVIDER_TYPE,
                      P.PAR_STATUS,
                      P.CBSA_CODE,
                      P.STATE_CODE,
                      P.ZIP_CODE,
                      P.TEACHING_INDICATOR,
                      P.DSH_ELIGIBLE,
                      C.CONTRACT_ID,
                      C.CONTRACT_TYPE,
                      C.WITHHOLD_PERCENT,
                      C.DSH_PERCENT,
                      C.IME_PERCENT,
                      C.COST_TO_CHARGE_RATIO
               INTO   :WS-PROV-TAX-ID,
                      :WS-PROV-LAST-NAME,
                      :WS-PROV-FIRST-NAME,
                      :WS-PROV-SPECIALTY,
                      :WS-PROV-TYPE,
                      :WS-PROV-PAR-STATUS,
                      :WS-PROV-CBSA-CODE,
                      :WS-PROV-STATE,
                      :WS-PROV-ZIP,
                      :WS-PROV-TEACHING-SW,
                      :WS-PROV-DSH-ELIGIBLE-SW,
                      :WS-PROV-CONTRACT-ID,
                      :WS-PROV-CONTRACT-TYPE,
                      :WS-PROV-WITHHOLD-PCT,
                      :WS-PROV-DSH-PCT,
                      :WS-PROV-IME-PCT,
                      :WS-PROV-COST-TO-CHARGE
               FROM   PROVIDER P
               LEFT JOIN PROVIDER_CONTRACT C
                 ON   P.NPI = C.NPI
                 AND  C.EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                 AND  (C.TERMINATION_DATE IS NULL
                  OR   C.TERMINATION_DATE >= :WS-CLM-FROM-DATE)
               WHERE  P.NPI = :HV-PROVIDER-NPI
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           EVALUATE WS-SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'PROVIDER NOT FOUND IN NETWORK FILE'
                     TO WS-ERR-MESSAGE
                   MOVE '2300-LOOKUP-PROVIDER' TO WS-ERR-PARAGRAPH
                   SET ERR-WARNING TO TRUE
      *--- NON-PAR PROVIDER: MAY STILL PROCESS THE CLAIM ---
                   SET PROV-IS-OON TO TRUE
                   SET MBR-OUT-OF-NETWORK TO TRUE
                   MOVE SPACES TO WS-PROV-CONTRACT-ID
                   MOVE SPACES TO WS-PROV-CONTRACT-TYPE
                   PERFORM 8800-ERROR-HANDLER
                      THRU 8800-ERROR-HANDLER-EXIT
               WHEN OTHER
                   MOVE 'PROVIDER LOOKUP SQL ERROR'
                     TO WS-ERR-MESSAGE
                   MOVE WS-SQLCODE TO WS-ERR-SQLCODE
                   MOVE '2300-LOOKUP-PROVIDER' TO WS-ERR-PARAGRAPH
                   SET ERR-SEVERE TO TRUE
                   PERFORM 8900-DATABASE-ERROR
                      THRU 8900-DATABASE-ERROR-EXIT
                   SET CLAIM-IS-INVALID TO TRUE
           END-EVALUATE
           .
       2300-LOOKUP-PROVIDER-EXIT.
           EXIT.

      *================================================================*
      * 2400 - LOAD BENEFIT PLAN                                       *
      *================================================================*
       2400-LOAD-BENEFIT-PLAN.
      *---------------------------------------------------------------*
      * LOAD THE MEMBER'S BENEFIT PLAN DESIGN INCLUDING DEDUCTIBLES,  *
      * COPAYS, COINSURANCE, AND OUT-OF-POCKET MAXIMUMS.              *
      *---------------------------------------------------------------*
           MOVE WS-MBR-PLAN-CODE TO HV-PLAN-CODE
           MOVE WS-CURRENT-YEAR TO HV-PLAN-YEAR

           EXEC SQL
               SELECT PLAN_CODE,
                      PLAN_YEAR,
                      INN_IND_DEDUCTIBLE,
                      INN_FAM_DEDUCTIBLE,
                      OON_IND_DEDUCTIBLE,
                      OON_FAM_DEDUCTIBLE,
                      INN_COINSURANCE_PCT,
                      OON_COINSURANCE_PCT,
                      INN_IND_OOP_MAX,
                      INN_FAM_OOP_MAX,
                      OON_IND_OOP_MAX,
                      OON_FAM_OOP_MAX,
                      LIFETIME_MAXIMUM,
                      ANNUAL_MAXIMUM,
                      COPAY_PCP,
                      COPAY_SPECIALIST,
                      COPAY_ER,
                      COPAY_URGENT_CARE,
                      COPAY_INPATIENT,
                      COPAY_OUTPATIENT,
                      COPAY_LAB,
                      COPAY_RADIOLOGY,
                      COPAY_MENTAL_HEALTH,
                      COPAY_PHYS_THERAPY,
                      COPAY_RX_GENERIC,
                      COPAY_RX_BRAND,
                      COPAY_RX_SPECIALTY,
                      EMBEDDED_DEDUCTIBLE_IND,
                      COPAY_BEFORE_DEDUCTIBLE,
                      COPAY_APPLIES_TO_OOP,
                      CARRYOVER_INDICATOR,
                      CARRYOVER_MONTHS,
                      GRANDFATHERED_INDICATOR,
                      PREVENTIVE_COVERAGE_IND,
                      MH_PARITY_INDICATOR,
                      FAMILY_DEDUCTIBLE_RULE
               INTO   :WS-BEN-PLAN-CODE,
                      :WS-BEN-PLAN-YEAR,
                      :WS-BEN-INN-IND-DEDUCT,
                      :WS-BEN-INN-FAM-DEDUCT,
                      :WS-BEN-OON-IND-DEDUCT,
                      :WS-BEN-OON-FAM-DEDUCT,
                      :WS-BEN-INN-COINSURANCE,
                      :WS-BEN-OON-COINSURANCE,
                      :WS-BEN-INN-IND-OOP,
                      :WS-BEN-INN-FAM-OOP,
                      :WS-BEN-OON-IND-OOP,
                      :WS-BEN-OON-FAM-OOP,
                      :WS-BEN-LIFETIME-MAX,
                      :WS-BEN-ANNUAL-MAX,
                      :WS-BEN-COPAY-PCP,
                      :WS-BEN-COPAY-SPEC,
                      :WS-BEN-COPAY-ER,
                      :WS-BEN-COPAY-URGENT,
                      :WS-BEN-COPAY-INPT,
                      :WS-BEN-COPAY-OUTPT,
                      :WS-BEN-COPAY-LAB,
                      :WS-BEN-COPAY-RAD,
                      :WS-BEN-COPAY-MH,
                      :WS-BEN-COPAY-PT,
                      :WS-BEN-COPAY-RX-GEN,
                      :WS-BEN-COPAY-RX-BRAND,
                      :WS-BEN-COPAY-RX-SPEC,
                      :WS-BEN-EMBEDDED-IND,
                      :WS-BEN-COPAY-BEFORE-DED,
                      :WS-BEN-COPAY-TO-OOP,
                      :WS-BEN-CARRYOVER-IND,
                      :WS-BEN-CARRYOVER-MONTHS,
                      :WS-BEN-GRANDFATHERED,
                      :WS-BEN-PREVENTIVE-IND,
                      :WS-BEN-MH-PARITY-IND,
                      :WS-BEN-FAM-DEDUCT-RULE
               FROM   BENEFIT_PLAN
               WHERE  PLAN_CODE = :HV-PLAN-CODE
               AND    PLAN_YEAR  = :HV-PLAN-YEAR
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
               MOVE 'BENEFIT PLAN NOT FOUND' TO WS-ERR-MESSAGE
               MOVE WS-SQLCODE TO WS-ERR-SQLCODE
               MOVE '2400-LOAD-BENEFIT-PLAN' TO WS-ERR-PARAGRAPH
               SET ERR-SEVERE TO TRUE
               SET CLAIM-SHOULD-PEND TO TRUE
               MOVE 'B0001' TO WS-PEND-REASON-CODE
               MOVE 'BENEFIT PLAN NOT ON FILE'
                 TO WS-PEND-REASON-DESC
               SET PEND-MANAGEMENT TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
           END-IF

      *--- SET PLAN FLAGS FROM LOADED DATA ---
           IF BEN-IS-GRANDFATHERED
               SET IS-GRANDFATHERED-PLAN TO TRUE
           ELSE
               SET NOT-GRANDFATHERED TO TRUE
           END-IF

           IF BEN-COPAY-BEFORE-DED
               SET COPAY-BEFORE-DEDUCTIBLE TO TRUE
           ELSE
               SET COPAY-AFTER-DEDUCTIBLE TO TRUE
           END-IF

           IF BEN-EMBEDDED-DEDUCT
               SET IS-EMBEDDED-DEDUCT TO TRUE
           ELSE
               SET NOT-EMBEDDED-DEDUCT TO TRUE
           END-IF

           IF BEN-COPAY-TO-OOP
               SET COPAY-COUNTS-TO-OOP TO TRUE
           ELSE
               SET COPAY-NOT-TO-OOP TO TRUE
           END-IF

           IF BEN-HAS-CARRYOVER
               SET CARRYOVER-APPLIES TO TRUE
           ELSE
               SET CARRYOVER-NA TO TRUE
           END-IF
           .
       2400-LOAD-BENEFIT-PLAN-EXIT.
           EXIT.

      *================================================================*
      * 2500 - CHECK AUTHORIZATION                                     *
      *================================================================*
       2500-CHECK-AUTHORIZATION.
      *---------------------------------------------------------------*
      * CHECK IF THE SERVICE REQUIRES PRIOR AUTHORIZATION AND IF SO,  *
      * VERIFY THAT A VALID AUTHORIZATION EXISTS.                      *
      *---------------------------------------------------------------*
           MOVE WS-DTL-CPT-CODE TO HV-CPT-CODE

      *--- CHECK IF AUTH IS REQUIRED FOR THIS SERVICE ---
           EXEC SQL
               SELECT 'Y'
               INTO   :WS-AUTH-REQUIRED-SW
               FROM   AUTH_REQUIRED_SERVICES
               WHERE  (CPT_CODE = :HV-CPT-CODE
                  OR   REVENUE_CODE = :HV-REVENUE-CODE)
               AND    PLAN_TYPE = :WS-MBR-PLAN-TYPE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = +100
               SET AUTH-NOT-REQUIRED TO TRUE
               GO TO 2500-CHECK-AUTHORIZATION-EXIT
           END-IF

           IF WS-SQLCODE NOT = 0
               SET AUTH-NOT-REQUIRED TO TRUE
               GO TO 2500-CHECK-AUTHORIZATION-EXIT
           END-IF

           SET AUTH-IS-REQUIRED TO TRUE

      *--- LOOK FOR VALID AUTHORIZATION ---
           EXEC SQL
               SELECT 'Y'
               INTO   :WS-AUTH-FOUND-SW
               FROM   AUTHORIZATION
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    PROVIDER_NPI = :HV-PROVIDER-NPI
               AND    (AUTH_CPT_CODE = :HV-CPT-CODE
                  OR   AUTH_CPT_CODE = '*ALL*')
               AND    AUTH_STATUS = 'AP'
               AND    AUTH_EFF_DATE <= :WS-CLM-FROM-DATE
               AND    AUTH_TERM_DATE >= :WS-CLM-THRU-DATE
               AND    UNITS_REMAINING > 0
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
               SET AUTH-WAS-FOUND TO TRUE
           ELSE
               SET AUTH-NOT-FOUND TO TRUE
           END-IF
           .
       2500-CHECK-AUTHORIZATION-EXIT.
           EXIT.

      *================================================================*
      * 2600 - CHECK DUPLICATE                                         *
      *================================================================*
       2600-CHECK-DUPLICATE.
      *---------------------------------------------------------------*
      * CHECK FOR DUPLICATE CLAIMS USING MEMBER, PROVIDER, DATES,     *
      * AND BILLED AMOUNT. ALLOW VOIDS AND REPLACEMENTS THROUGH.      *
      *---------------------------------------------------------------*
           IF CLM-VOID OR CLM-REPLACEMENT OR CLM-ADJUSTMENT
               GO TO 2600-CHECK-DUPLICATE-EXIT
           END-IF

           EXEC SQL
               SELECT COUNT(*)
               INTO   :WS-WRK-INDEX-1
               FROM   CLAIM_HEADER
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    PROVIDER_NPI = :HV-PROVIDER-NPI
               AND    SERVICE_FROM_DATE = :WS-CLM-FROM-DATE
               AND    SERVICE_THRU_DATE = :WS-CLM-THRU-DATE
               AND    TOTAL_BILLED_AMOUNT = :WS-CLM-TOTAL-CHARGE
               AND    CLAIM_STATUS NOT IN ('VD', 'DN')
               AND    CLAIM_ID <> :HV-CLAIM-ID
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0 AND WS-WRK-INDEX-1 > 0
               MOVE 'DUPLICATE CLAIM DETECTED' TO WS-ERR-MESSAGE
               MOVE '2600-CHECK-DUPLICATE' TO WS-ERR-PARAGRAPH
               SET ERR-WARNING TO TRUE
               SET CLAIM-IS-INVALID TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
           END-IF
           .
       2600-CHECK-DUPLICATE-EXIT.
           EXIT.

      *================================================================*
      * 2700 - CHECK PREVENTIVE CARE                                   *
      *================================================================*
       2700-CHECK-PREVENTIVE-CARE.
      *---------------------------------------------------------------*
      * DETERMINE IF SERVICE IS PREVENTIVE CARE UNDER ACA.            *
      * PREVENTIVE SERVICES HAVE NO COST SHARING WHEN IN-NETWORK.     *
      *---------------------------------------------------------------*
           EXEC SQL
               SELECT 'Y'
               INTO   :WS-PREVENTIVE-SW
               FROM   PREVENTIVE_SERVICES_LIST
               WHERE  (CPT_CODE = :HV-CPT-CODE
                  OR   HCPCS_CODE = :HV-HCPCS-CODE)
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
               AND    (MIN_AGE IS NULL
                  OR   MIN_AGE <= :WS-MBR-AGE)
               AND    (MAX_AGE IS NULL
                  OR   MAX_AGE >= :WS-MBR-AGE)
               AND    (GENDER_RESTRICTION IS NULL
                  OR   GENDER_RESTRICTION = :WS-MBR-GENDER
                  OR   GENDER_RESTRICTION = 'A')
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
               SET IS-PREVENTIVE-CARE TO TRUE
           ELSE
               SET NOT-PREVENTIVE-CARE TO TRUE
           END-IF
           .
       2700-CHECK-PREVENTIVE-CARE-EXIT.
           EXIT.

      *================================================================*
      * 2710 - CHECK COVID SERVICES                                    *
      *================================================================*
       2710-CHECK-COVID-SERVICES.
      *---------------------------------------------------------------*
      * CHECK IF SERVICE IS COVID-19 RELATED (TESTING OR VACCINE).     *
      * COVID TESTING AND VACCINATION HAVE NO COST SHARING PER         *
      * FAMILIES FIRST CORONAVIRUS RESPONSE ACT AND CARES ACT.        *
      *---------------------------------------------------------------*
           IF WS-DTL-CPT-CODE = '87635' OR '86328' OR '86769'
              OR '99211' OR '87426' OR '87428' OR '0202U'
              OR '0223U' OR '0225U' OR '0226U' OR '0240U'
              OR '91300' OR '91301' OR '91302' OR '91303'
              OR '91304' OR '91305' OR '91306' OR '91307'
              OR '0001A' OR '0002A' OR '0003A' OR '0004A'
              OR '0011A' OR '0012A' OR '0013A' OR '0021A'
              OR '0022A' OR '0031A' OR '0034A' OR '0041A'
              OR '0042A' OR '0051A' OR '0052A' OR '0053A'
              OR '0054A' OR '0064A' OR '0071A' OR '0072A'
              OR '0073A' OR '0074A' OR '0081A' OR '0082A'
               SET IS-COVID-RELATED TO TRUE
           ELSE
               IF WS-CLM-PRINCIPAL-DIAG = 'U071  '
                  OR WS-CLM-PRINCIPAL-DIAG = 'U072  '
                  OR WS-CLM-PRINCIPAL-DIAG = 'Z20822'
                  OR WS-CLM-PRINCIPAL-DIAG = 'U099  '
                  OR WS-CLM-PRINCIPAL-DIAG = 'U109  '
                   SET IS-COVID-RELATED TO TRUE
               ELSE
                   SET NOT-COVID-RELATED TO TRUE
               END-IF
           END-IF
           .
       2710-CHECK-COVID-SERVICES-EXIT.
           EXIT.

      *================================================================*
      * 3000 - DETERMINE PRICING METHOD                                *
      *================================================================*
       3000-DETERMINE-PRICING-METHOD.
      *---------------------------------------------------------------*
      * DETERMINE THE APPROPRIATE PRICING METHODOLOGY BASED ON THE    *
      * PROVIDER CONTRACT, PLAN TYPE, AND CLAIM TYPE.                  *
      *---------------------------------------------------------------*

      *--- FIRST CHECK: PROVIDER CONTRACT-SPECIFIC RATES ---
           IF WS-PROV-CONTRACT-ID NOT = SPACES
               EVALUATE TRUE
                   WHEN CONTR-FEE-SCHEDULE
                       MOVE 'FS' TO WS-DTL-PRICING-METHOD
                       PERFORM 3100-PRICE-BY-FEE-SCHEDULE
                          THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
                   WHEN CONTR-DRG
                       MOVE 'DG' TO WS-DTL-PRICING-METHOD
                       PERFORM 3200-PRICE-BY-DRG
                          THRU 3200-PRICE-BY-DRG-EXIT
                   WHEN CONTR-PER-DIEM
                       MOVE 'PD' TO WS-DTL-PRICING-METHOD
                       PERFORM 3300-PRICE-BY-PER-DIEM
                          THRU 3300-PRICE-BY-PER-DIEM-EXIT
                   WHEN CONTR-CASE-RATE
                       MOVE 'CR' TO WS-DTL-PRICING-METHOD
                       PERFORM 3400-PRICE-BY-CASE-RATE
                          THRU 3400-PRICE-BY-CASE-RATE-EXIT
                   WHEN CONTR-PCT-CHARGE
                       MOVE 'PC' TO WS-DTL-PRICING-METHOD
                       PERFORM 3500-PRICE-BY-PERCENT-CHARGE
                          THRU 3500-PRICE-BY-PERCENT-CHARGE-EXIT
                   WHEN CONTR-CAPITATION
                       MOVE 'CP' TO WS-DTL-PRICING-METHOD
                       PERFORM 3600-PRICE-BY-CAPITATION
                          THRU 3600-PRICE-BY-CAPITATION-EXIT
                   WHEN OTHER
                       MOVE 'FS' TO WS-DTL-PRICING-METHOD
                       PERFORM 3100-PRICE-BY-FEE-SCHEDULE
                          THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
               END-EVALUATE
               GO TO 3000-DETERMINE-PRICING-METHOD-EXIT
           END-IF

      *--- SECOND CHECK: MEDICARE CLAIMS USE RBRVS ---
           IF PLAN-TYPE-MCARE
               IF CLM-TYPE-INSTITUTIONAL AND CLM-INPATIENT
                   MOVE 'DG' TO WS-DTL-PRICING-METHOD
                   PERFORM 3200-PRICE-BY-DRG
                      THRU 3200-PRICE-BY-DRG-EXIT
               ELSE
                   MOVE 'FS' TO WS-DTL-PRICING-METHOD
                   PERFORM 3100-PRICE-BY-FEE-SCHEDULE
                      THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
               END-IF
               GO TO 3000-DETERMINE-PRICING-METHOD-EXIT
           END-IF

      *--- THIRD CHECK: MEDICAID USES STATE FEE SCHEDULE ---
           IF PLAN-TYPE-MCAID
               MOVE 'FS' TO WS-DTL-PRICING-METHOD
               PERFORM 3100-PRICE-BY-FEE-SCHEDULE
                  THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
               GO TO 3000-DETERMINE-PRICING-METHOD-EXIT
           END-IF

      *--- FOURTH CHECK: INSTITUTIONAL INPATIENT DEFAULT ---
           IF CLM-TYPE-INSTITUTIONAL AND CLM-INPATIENT
      *--- DEFAULT INSTITUTIONAL INPATIENT TO DRG IF DRG IS PRESENT
               IF WS-CLM-DRG-CODE NOT = SPACES
                  AND WS-CLM-DRG-CODE NOT = '0000'
                   MOVE 'DG' TO WS-DTL-PRICING-METHOD
                   PERFORM 3200-PRICE-BY-DRG
                      THRU 3200-PRICE-BY-DRG-EXIT
               ELSE
      *--- NO DRG: USE PER DIEM AS DEFAULT
                   MOVE 'PD' TO WS-DTL-PRICING-METHOD
                   PERFORM 3300-PRICE-BY-PER-DIEM
                      THRU 3300-PRICE-BY-PER-DIEM-EXIT
               END-IF
               GO TO 3000-DETERMINE-PRICING-METHOD-EXIT
           END-IF

      *--- FIFTH CHECK: OUT-OF-NETWORK DEFAULT TO % OF CHARGES ---
           IF PROV-IS-OON
               MOVE 'PC' TO WS-DTL-PRICING-METHOD
               MOVE +80.00 TO WS-PCT-CONTRACT-PCT
               PERFORM 3500-PRICE-BY-PERCENT-CHARGE
                  THRU 3500-PRICE-BY-PERCENT-CHARGE-EXIT
               GO TO 3000-DETERMINE-PRICING-METHOD-EXIT
           END-IF

      *--- DEFAULT: USE FEE SCHEDULE ---
           MOVE 'FS' TO WS-DTL-PRICING-METHOD
           PERFORM 3100-PRICE-BY-FEE-SCHEDULE
              THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
           .
       3000-DETERMINE-PRICING-METHOD-EXIT.
           EXIT.

      *================================================================*
      * 3100 - PRICE BY FEE SCHEDULE (RBRVS)                          *
      *================================================================*
       3100-PRICE-BY-FEE-SCHEDULE.
      *---------------------------------------------------------------*
      * FULL RBRVS CALCULATION:                                        *
      *   ALLOWED = [(WORK_RVU * WORK_GPCI) +                         *
      *              (PE_RVU * PE_GPCI) +                              *
      *              (MP_RVU * MP_GPCI)] * CONVERSION_FACTOR           *
      * THEN APPLY MODIFIER ADJUSTMENTS AND SITE-OF-SERVICE.          *
      *---------------------------------------------------------------*
           MOVE WS-DTL-CPT-CODE TO HV-CPT-CODE
           MOVE WS-PROV-CBSA-CODE TO HV-CBSA-CODE

      *--- LOOK UP RVU VALUES ---
           EXEC SQL
               SELECT WORK_RVU,
                      PRACTICE_EXPENSE_RVU,
                      MALPRACTICE_RVU,
                      SITE_OF_SERVICE_IND
               INTO   :HV-WORK-RVU,
                      :HV-PE-RVU,
                      :HV-MP-RVU,
                      :HV-SOS-INDICATOR
               FROM   FEE_SCHEDULE_RBRVS
               WHERE  CPT_CODE = :HV-CPT-CODE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = +100
      *--- UNLISTED PROCEDURE CODE: TRY CROSSWALK TABLE ---
               EXEC SQL
                   SELECT CROSSWALK_CPT,
                          CROSSWALK_PERCENT
                   INTO   :HV-CPT-CODE,
                          :WS-WRK-PCT-1
                   FROM   UNLISTED_PROC_CROSSWALK
                   WHERE  UNLISTED_CPT = :WS-DTL-CPT-CODE
                   AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                   AND    (TERMINATION_DATE IS NULL
                      OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
               END-EXEC

               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS

               IF WS-SQLCODE = 0
      *--- FOUND CROSSWALK: LOOK UP RVU FOR CROSSWALKED CODE ---
                   EXEC SQL
                       SELECT WORK_RVU,
                              PRACTICE_EXPENSE_RVU,
                              MALPRACTICE_RVU,
                              SITE_OF_SERVICE_IND
                       INTO   :HV-WORK-RVU,
                              :HV-PE-RVU,
                              :HV-MP-RVU,
                              :HV-SOS-INDICATOR
                       FROM   FEE_SCHEDULE_RBRVS
                       WHERE  CPT_CODE = :HV-CPT-CODE
                       AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                       AND    (TERMINATION_DATE IS NULL
                          OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
                   END-EXEC
                   MOVE SQLCODE TO WS-SQLCODE
                   ADD 1 TO WS-CTR-DB-READS
               END-IF

               IF WS-SQLCODE NOT = 0
      *--- STILL NO PRICING: PEND THE CLAIM ---
                   SET CLAIM-SHOULD-PEND TO TRUE
                   MOVE 'R0001' TO WS-PEND-REASON-CODE
                   MOVE 'NO FEE SCHEDULE ENTRY FOR PROC CODE'
                     TO WS-PEND-REASON-DESC
                   SET PEND-PRICING-REVIEW TO TRUE
                   GO TO 3100-PRICE-BY-FEE-SCHEDULE-EXIT
               END-IF
           ELSE
               IF WS-SQLCODE NOT = 0
                   MOVE 'FEE SCHEDULE LOOKUP SQL ERROR'
                     TO WS-ERR-MESSAGE
                   MOVE WS-SQLCODE TO WS-ERR-SQLCODE
                   MOVE '3100-PRICE-BY-FEE-SCHEDULE'
                     TO WS-ERR-PARAGRAPH
                   SET ERR-SEVERE TO TRUE
                   PERFORM 8900-DATABASE-ERROR
                      THRU 8900-DATABASE-ERROR-EXIT
                   SET CLAIM-SHOULD-PEND TO TRUE
                   MOVE 'R0002' TO WS-PEND-REASON-CODE
                   MOVE 'FEE SCHEDULE DATABASE ERROR'
                     TO WS-PEND-REASON-DESC
                   SET PEND-PRICING-REVIEW TO TRUE
                   GO TO 3100-PRICE-BY-FEE-SCHEDULE-EXIT
               END-IF
           END-IF

           MOVE HV-WORK-RVU TO WS-PRC-WORK-RVU
           MOVE HV-PE-RVU   TO WS-PRC-PE-RVU
           MOVE HV-MP-RVU   TO WS-PRC-MP-RVU

      *--- LOOK UP GPCI VALUES BY CBSA ---
           EXEC SQL
               SELECT WORK_GPCI,
                      PE_GPCI,
                      MP_GPCI,
                      CONVERSION_FACTOR
               INTO   :HV-WORK-GPCI,
                      :HV-PE-GPCI,
                      :HV-MP-GPCI,
                      :HV-CONV-FACTOR
               FROM   GPCI_TABLE
               WHERE  CBSA_CODE = :HV-CBSA-CODE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
      *--- DEFAULT GPCI VALUES IF LOOKUP FAILS ---
               MOVE +1.0000 TO WS-PRC-WORK-GPCI
               MOVE +1.0000 TO WS-PRC-PE-GPCI
               MOVE +1.0000 TO WS-PRC-MP-GPCI
               MOVE +36.0896 TO WS-PRC-CONVERSION-FACTOR
           ELSE
               MOVE HV-WORK-GPCI TO WS-PRC-WORK-GPCI
               MOVE HV-PE-GPCI   TO WS-PRC-PE-GPCI
               MOVE HV-MP-GPCI   TO WS-PRC-MP-GPCI
               MOVE HV-CONV-FACTOR TO WS-PRC-CONVERSION-FACTOR
           END-IF

      *--- CALCULATE TOTAL RVU WITH GPCI ADJUSTMENT ---
           COMPUTE WS-PRC-TOTAL-RVU =
               (WS-PRC-WORK-RVU * WS-PRC-WORK-GPCI)
             + (WS-PRC-PE-RVU   * WS-PRC-PE-GPCI)
             + (WS-PRC-MP-RVU   * WS-PRC-MP-GPCI)

      *--- APPLY SITE-OF-SERVICE DIFFERENTIAL ---
      * IF PROCEDURE IS TYPICALLY DONE IN FACILITY BUT BILLED FROM
      * OFFICE, THE PE RVU IS REDUCED (FACILITY VS NON-FACILITY)
           IF HV-SOS-INDICATOR = 'Y'
               IF POS-OFFICE OR POS-HOME
      *--- NON-FACILITY: USE FULL PE RVU (ALREADY LOADED) ---
                   CONTINUE
               ELSE
      *--- FACILITY SETTING: REDUCE PE RVU ---
                   COMPUTE WS-PRC-TOTAL-RVU =
                       (WS-PRC-WORK-RVU * WS-PRC-WORK-GPCI)
                     + (WS-PRC-PE-RVU * WS-PRC-PE-GPCI
                        * WS-WRK-SOS-FACILITY-PCT)
                     + (WS-PRC-MP-RVU * WS-PRC-MP-GPCI)
               END-IF
           END-IF

      *--- CALCULATE BASE ALLOWED AMOUNT ---
           COMPUTE WS-PRC-ALLOWED-CALC =
               WS-PRC-TOTAL-RVU * WS-PRC-CONVERSION-FACTOR

      *--- APPLY MODIFIER ADJUSTMENTS ---
           MOVE +1.0000 TO WS-PRC-MODIFIER-ADJ-PCT

           PERFORM VARYING WS-WRK-MOD-IDX FROM 1 BY 1
               UNTIL WS-WRK-MOD-IDX > 4
               EVALUATE WS-DTL-MODIFIER(WS-WRK-MOD-IDX)
                   WHEN '26'
      *--- PROFESSIONAL COMPONENT ONLY ---
                       MULTIPLY WS-MOD-26-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN 'TC'
      *--- TECHNICAL COMPONENT ONLY ---
                       MULTIPLY WS-MOD-TC-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '50'
      *--- BILATERAL PROCEDURE: 150% ---
                       MULTIPLY WS-MOD-50-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '51'
      *--- MULTIPLE PROCEDURE: 50% REDUCTION ON 2ND+ ---
                       IF WS-WRK-PROC-RANK > 1
                           MULTIPLY WS-MOD-51-PCT
                               BY WS-PRC-MODIFIER-ADJ-PCT
                       END-IF
                   WHEN '52'
      *--- REDUCED SERVICES ---
                       MULTIPLY WS-MOD-52-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '59'
      *--- DISTINCT PROCEDURAL SERVICE: NO REDUCTION ---
                       CONTINUE
                   WHEN '62'
      *--- TWO SURGEONS: 62.5% EACH ---
                       MULTIPLY WS-MOD-62-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '80'
      *--- ASSISTANT SURGEON: 16% ---
                       MULTIPLY WS-MOD-80-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '81'
      *--- MINIMUM ASSISTANT SURGEON: 10% ---
                       MULTIPLY WS-MOD-81-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '82'
      *--- ASSISTANT SURGEON (NO QUALIFIED RESIDENT): 16% ---
                       MULTIPLY WS-MOD-82-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN 'AS'
      *--- PHYSICIAN ASSISTANT AS ASSISTANT SURGEON: 16% ---
                       MULTIPLY WS-MOD-AS-PCT
                           BY WS-PRC-MODIFIER-ADJ-PCT
                   WHEN '95'
      *--- TELEHEALTH: SAME RATE AS IN-PERSON ---
                       CONTINUE
                   WHEN SPACES
                       CONTINUE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

      *--- APPLY MODIFIER ADJUSTMENT TO ALLOWED AMOUNT ---
           COMPUTE WS-PRC-ALLOWED-CALC ROUNDED =
               WS-PRC-ALLOWED-CALC * WS-PRC-MODIFIER-ADJ-PCT

      *--- MULTIPLY BY UNITS ---
           COMPUTE WS-PRC-ALLOWED-CALC ROUNDED =
               WS-PRC-ALLOWED-CALC * WS-DTL-UNITS

      *--- APPLY CONTRACT-SPECIFIC MULTIPLIER IF EXISTS ---
           IF WS-PROV-CONTRACT-ID NOT = SPACES
               EXEC SQL
                   SELECT FEE_SCHEDULE_PCT
                   INTO   :WS-WRK-PCT-1
                   FROM   CONTRACT_FEE_SCHEDULE
                   WHERE  CONTRACT_ID = :HV-CONTRACT-ID
                   AND    (CPT_CODE = :HV-CPT-CODE
                      OR   CPT_CODE = '*ALL*')
                   AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                   AND    (TERMINATION_DATE IS NULL
                      OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
                   ORDER BY CPT_CODE DESC
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS
               IF WS-SQLCODE = 0
                   COMPUTE WS-PRC-ALLOWED-CALC ROUNDED =
                       WS-PRC-ALLOWED-CALC * WS-WRK-PCT-1
               END-IF
           END-IF

      *--- FINAL ALLOWED CANNOT EXCEED BILLED ---
           IF WS-PRC-ALLOWED-CALC > WS-DTL-BILLED-AMOUNT
               MOVE WS-DTL-BILLED-AMOUNT TO WS-PRC-ALLOWED-CALC
           END-IF

      *--- ALLOWED CANNOT BE NEGATIVE ---
           IF WS-PRC-ALLOWED-CALC < 0
               MOVE +0 TO WS-PRC-ALLOWED-CALC
           END-IF

           MOVE WS-PRC-ALLOWED-CALC TO WS-DTL-ALLOWED-AMOUNT
           SET PRICING-FOUND TO TRUE
           .
       3100-PRICE-BY-FEE-SCHEDULE-EXIT.
           EXIT.

      *================================================================*
      * 3200 - PRICE BY DRG                                            *
      *================================================================*
       3200-PRICE-BY-DRG.
      *---------------------------------------------------------------*
      * MS-DRG PRICING CALCULATION:                                    *
      *   BASE = (FEDERAL_RATE * LABOR_SHARE * WAGE_INDEX)             *
      *        + (FEDERAL_RATE * NON_LABOR_SHARE)                      *
      *   PAYMENT = BASE * DRG_WEIGHT * CASE_MIX_INDEX                 *
      *   THEN ADD: DSH, IME, OUTLIER, NEW TECH, READMIT PENALTY      *
      *---------------------------------------------------------------*
           MOVE WS-CLM-DRG-CODE TO HV-DRG-CODE
           MOVE WS-PROV-CBSA-CODE TO HV-CBSA-CODE

      *--- LOOK UP DRG WEIGHT AND AVERAGE LOS ---
           EXEC SQL
               SELECT DRG_WEIGHT,
                      GEOMETRIC_LOS,
                      ARITHMETIC_LOS,
                      NEW_TECH_INDICATOR,
                      NEW_TECH_AMOUNT
               INTO   :HV-DRG-WEIGHT,
                      :HV-GEOM-LOS,
                      :HV-ARITH-LOS,
                      :WS-DRG-NEW-TECH-SW,
                      :HV-NEW-TECH-AMT
               FROM   MS_DRG_TABLE
               WHERE  DRG_CODE = :HV-DRG-CODE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
               SET CLAIM-SHOULD-PEND TO TRUE
               MOVE 'D0010' TO WS-PEND-REASON-CODE
               MOVE 'DRG CODE NOT FOUND IN MS-DRG TABLE'
                 TO WS-PEND-REASON-DESC
               SET PEND-PRICING-REVIEW TO TRUE
               GO TO 3200-PRICE-BY-DRG-EXIT
           END-IF

           MOVE HV-DRG-WEIGHT TO WS-DRG-WEIGHT
           MOVE HV-GEOM-LOS TO WS-DRG-GEOM-LOS
           MOVE HV-ARITH-LOS TO WS-DRG-ARITH-LOS

      *--- LOOK UP FEDERAL RATE AND WAGE INDEX ---
           EXEC SQL
               SELECT FEDERAL_BASE_RATE,
                      WAGE_INDEX,
                      LABOR_SHARE,
                      NON_LABOR_SHARE,
                      OUTLIER_THRESHOLD,
                      CASE_MIX_INDEX,
                      READMISSION_PENALTY_PCT
               INTO   :HV-FEDERAL-RATE,
                      :HV-WAGE-INDEX,
                      :HV-LABOR-SHARE,
                      :HV-NON-LABOR,
                      :HV-OUTLIER-THRESHOLD,
                      :HV-CASE-MIX-INDEX,
                      :HV-READMIT-PENALTY
               FROM   DRG_PAYMENT_RATES
               WHERE  CBSA_CODE = :HV-CBSA-CODE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
               SET CLAIM-SHOULD-PEND TO TRUE
               MOVE 'D0011' TO WS-PEND-REASON-CODE
               MOVE 'DRG PAYMENT RATE NOT FOUND FOR CBSA'
                 TO WS-PEND-REASON-DESC
               SET PEND-PRICING-REVIEW TO TRUE
               GO TO 3200-PRICE-BY-DRG-EXIT
           END-IF

           MOVE HV-FEDERAL-RATE TO WS-DRG-FEDERAL-RATE
           MOVE HV-WAGE-INDEX TO WS-DRG-WAGE-INDEX
           MOVE HV-LABOR-SHARE TO WS-DRG-LABOR-SHARE
           MOVE HV-NON-LABOR TO WS-DRG-NON-LABOR-SHARE
           MOVE HV-OUTLIER-THRESHOLD TO WS-DRG-OUTLIER-THRESHOLD
           MOVE HV-CASE-MIX-INDEX TO WS-DRG-CASE-MIX-INDEX
           MOVE HV-READMIT-PENALTY TO WS-DRG-READMIT-PENALTY-PCT

      *--- CALCULATE BASE RATE ---
      * BASE = (FEDERAL * LABOR% * WAGE_INDEX) + (FEDERAL * NON_LABOR%)
           COMPUTE WS-DRG-BASE-RATE ROUNDED =
               (WS-DRG-FEDERAL-RATE * WS-DRG-LABOR-SHARE
                * WS-DRG-WAGE-INDEX)
             + (WS-DRG-FEDERAL-RATE * WS-DRG-NON-LABOR-SHARE)

      *--- APPLY DRG WEIGHT AND CASE MIX INDEX ---
           COMPUTE WS-DRG-TOTAL-PAYMENT ROUNDED =
               WS-DRG-BASE-RATE * WS-DRG-WEIGHT
               * WS-DRG-CASE-MIX-INDEX

      *--- CHECK FOR TRANSFER DRG (SHORT STAY TRANSFER) ---
           IF CLM-TRANSFERRED
               SET IS-TRANSFER-DRG TO TRUE
      *--- CALCULATE ACTUAL LOS ---
               COMPUTE WS-PDM-ACTUAL-LOS =
                   (WS-CLM-DISCHARGE-DATE - WS-CLM-ADMIT-DATE)
                   / 10000 * 365
               IF WS-PDM-ACTUAL-LOS < 1
                   MOVE 1 TO WS-PDM-ACTUAL-LOS
               END-IF
      *--- TRANSFER ADJUSTMENT: PER DIEM = FULL DRG / GEOM LOS ---
      *--- PAYMENT = PER DIEM * ACTUAL LOS (CAPPED AT FULL DRG) ---
               IF WS-PDM-ACTUAL-LOS < WS-DRG-GEOM-LOS
                   COMPUTE WS-DRG-TRANSFER-PER-DIEM ROUNDED =
                       WS-DRG-TOTAL-PAYMENT / WS-DRG-GEOM-LOS
                   MOVE WS-PDM-ACTUAL-LOS
                     TO WS-DRG-TRANSFER-DAYS
                   COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                       WS-DRG-TRANSFER-PER-DIEM
                       * WS-DRG-TRANSFER-DAYS
      *--- ADD ONE PER DIEM FOR THE DAY OF TRANSFER ---
                   ADD WS-DRG-TRANSFER-PER-DIEM
                     TO WS-WRK-AMOUNT-1
      *--- CAP AT FULL DRG PAYMENT ---
                   IF WS-WRK-AMOUNT-1 < WS-DRG-TOTAL-PAYMENT
                       MOVE WS-WRK-AMOUNT-1
                         TO WS-DRG-TOTAL-PAYMENT
                   END-IF
               END-IF
           END-IF

      *--- DISPROPORTIONATE SHARE HOSPITAL (DSH) ADJUSTMENT ---
           MOVE +0 TO WS-DRG-DSH-AMOUNT
           IF PROV-DSH-ELIGIBLE
               MOVE WS-PROV-DSH-PCT TO HV-DSH-PCT
               IF HV-DSH-PCT > 0
                   COMPUTE WS-DRG-DSH-AMOUNT ROUNDED =
                       WS-DRG-TOTAL-PAYMENT * WS-PROV-DSH-PCT
                       / 100
                   ADD WS-DRG-DSH-AMOUNT TO WS-DRG-TOTAL-PAYMENT
               END-IF
           END-IF

      *--- INDIRECT MEDICAL EDUCATION (IME) ADJUSTMENT ---
           MOVE +0 TO WS-DRG-IME-AMOUNT
           IF PROV-IS-TEACHING
               MOVE WS-PROV-IME-PCT TO HV-IME-PCT
               IF HV-IME-PCT > 0
                   COMPUTE WS-DRG-IME-AMOUNT ROUNDED =
                       WS-DRG-TOTAL-PAYMENT * WS-PROV-IME-PCT
                   ADD WS-DRG-IME-AMOUNT TO WS-DRG-TOTAL-PAYMENT
               END-IF
           END-IF

      *--- OUTLIER PAYMENT CALCULATION ---
           MOVE +0 TO WS-DRG-OUTLIER-PAYMENT
           MOVE WS-PROV-COST-TO-CHARGE TO HV-COST-TO-CHARGE

      *--- COST = BILLED CHARGES * COST-TO-CHARGE RATIO ---
           COMPUTE WS-DRG-COST-AMOUNT ROUNDED =
               WS-CLM-TOTAL-CHARGE * WS-PROV-COST-TO-CHARGE

      *--- OUTLIER IF COST > (DRG PAYMENT + FIXED THRESHOLD) ---
           COMPUTE WS-WRK-AMOUNT-1 =
               WS-DRG-TOTAL-PAYMENT + WS-DRG-OUTLIER-THRESHOLD

           IF WS-DRG-COST-AMOUNT > WS-WRK-AMOUNT-1
               SET IS-OUTLIER-CASE TO TRUE
      *--- OUTLIER = 80% OF (COST - DRG PAYMENT - THRESHOLD) ---
               COMPUTE WS-DRG-OUTLIER-PAYMENT ROUNDED =
                   (WS-DRG-COST-AMOUNT - WS-DRG-TOTAL-PAYMENT
                    - WS-DRG-OUTLIER-THRESHOLD)
                   * WS-WRK-DRG-OUTLIER-MARG
               IF WS-DRG-OUTLIER-PAYMENT < 0
                   MOVE +0 TO WS-DRG-OUTLIER-PAYMENT
               END-IF
               ADD WS-DRG-OUTLIER-PAYMENT TO WS-DRG-TOTAL-PAYMENT
           END-IF

      *--- NEW TECHNOLOGY ADD-ON PAYMENT ---
           MOVE +0 TO WS-DRG-NEW-TECH-AMOUNT
           IF HAS-NEW-TECH-ADD-ON
               MOVE HV-NEW-TECH-AMT TO WS-DRG-NEW-TECH-AMOUNT
      *--- NEW TECH ADD-ON = LESSER OF (COST - DRG) * 65% OR
      *--- MAXIMUM NEW TECH AMOUNT ---
               COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                   (WS-DRG-COST-AMOUNT - WS-DRG-TOTAL-PAYMENT)
                   * 0.65
               IF WS-WRK-AMOUNT-1 < WS-DRG-NEW-TECH-AMOUNT
                  AND WS-WRK-AMOUNT-1 > 0
                   MOVE WS-WRK-AMOUNT-1 TO WS-DRG-NEW-TECH-AMOUNT
               END-IF
               IF WS-DRG-NEW-TECH-AMOUNT > 0
                   ADD WS-DRG-NEW-TECH-AMOUNT
                     TO WS-DRG-TOTAL-PAYMENT
               END-IF
           END-IF

      *--- READMISSION PENALTY ---
      * CHECK IF THIS IS A READMISSION WITHIN 30 DAYS
           EXEC SQL
               SELECT 'Y'
               INTO   :WS-DRG-READMIT-SW
               FROM   CLAIM_HEADER
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    CLAIM_TYPE = 'IN'
               AND    CLAIM_SUB_TYPE = 'IP'
               AND    DISCHARGE_DATE >= :WS-CLM-ADMIT-DATE - 30
               AND    DISCHARGE_DATE < :WS-CLM-ADMIT-DATE
               AND    CLAIM_STATUS IN ('PA', 'AJ')
               AND    DRG_CODE = :HV-DRG-CODE
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0 AND IS-READMISSION
               IF WS-DRG-READMIT-PENALTY-PCT > 0
                   COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                       WS-DRG-TOTAL-PAYMENT
                       * WS-DRG-READMIT-PENALTY-PCT
                   SUBTRACT WS-WRK-AMOUNT-1
                     FROM WS-DRG-TOTAL-PAYMENT
               END-IF
           END-IF

      *--- FINAL DRG PAYMENT CANNOT EXCEED BILLED ---
           IF WS-DRG-TOTAL-PAYMENT > WS-CLM-TOTAL-CHARGE
               MOVE WS-CLM-TOTAL-CHARGE TO WS-DRG-TOTAL-PAYMENT
           END-IF

           IF WS-DRG-TOTAL-PAYMENT < 0
               MOVE +0 TO WS-DRG-TOTAL-PAYMENT
           END-IF

           MOVE WS-DRG-TOTAL-PAYMENT TO WS-DTL-ALLOWED-AMOUNT
           SET PRICING-FOUND TO TRUE
           .
       3200-PRICE-BY-DRG-EXIT.
           EXIT.

      *================================================================*
      * 3300 - PRICE BY PER DIEM                                       *
      *================================================================*
       3300-PRICE-BY-PER-DIEM.
      *---------------------------------------------------------------*
      * PER DIEM PRICING:                                              *
      *   LOOK UP RATES BY SERVICE TYPE.                               *
      *   CALCULATE ROOM & BOARD AND ANCILLARY SEPARATELY.             *
      *   APPLY STEP-DOWN AFTER SPECIFIED DAY.                         *
      *   HANDLE ICU VS REGULAR BED RATES.                             *
      *---------------------------------------------------------------*

      *--- DETERMINE SERVICE TYPE FOR PER DIEM LOOKUP ---
           EVALUATE TRUE
               WHEN WS-CLM-SUB-TYPE = 'IP'
                   EVALUATE WS-DTL-REVENUE-CODE
                       WHEN '0200' THRU '0219'
                           SET PDM-ICU TO TRUE
                           MOVE 'ICU' TO WS-PDM-SERVICE-TYPE
                       WHEN '0100' THRU '0169'
                           SET PDM-MED-SURG TO TRUE
                           MOVE 'MSR' TO WS-PDM-SERVICE-TYPE
                       WHEN '0170' THRU '0179'
                           SET PDM-NICU TO TRUE
                           MOVE 'NIC' TO WS-PDM-SERVICE-TYPE
                       WHEN '0114'
                           SET PDM-PSYCH TO TRUE
                           MOVE 'PSY' TO WS-PDM-SERVICE-TYPE
                       WHEN '0118' THRU '0119'
                           SET PDM-REHAB TO TRUE
                           MOVE 'REH' TO WS-PDM-SERVICE-TYPE
                       WHEN OTHER
                           SET PDM-MED-SURG TO TRUE
                           MOVE 'MSR' TO WS-PDM-SERVICE-TYPE
                   END-EVALUATE
               WHEN WS-CLM-SUB-TYPE = 'SN'
                   SET PDM-SNF TO TRUE
                   MOVE 'SNF' TO WS-PDM-SERVICE-TYPE
               WHEN OTHER
                   SET PDM-MED-SURG TO TRUE
                   MOVE 'MSR' TO WS-PDM-SERVICE-TYPE
           END-EVALUATE

      *--- CHECK FOR MATERNITY ---
           IF WS-CLM-PRINCIPAL-DIAG(1:3) = 'O80'
              OR WS-CLM-PRINCIPAL-DIAG(1:3) = 'O82'
              OR WS-CLM-PRINCIPAL-DIAG(1:3) = 'Z37'
              OR WS-CLM-PRINCIPAL-DIAG(1:3) = 'Z38'
               SET PDM-MATERNITY TO TRUE
               MOVE 'MAT' TO WS-PDM-SERVICE-TYPE
           END-IF

      *--- LOOK UP PER DIEM RATES ---
           MOVE WS-PROV-CONTRACT-ID TO HV-CONTRACT-ID

           EXEC SQL
               SELECT PER_DIEM_RATE,
                      ICU_RATE,
                      ANCILLARY_RATE,
                      LOS_LIMIT,
                      STEP_DOWN_DAY,
                      STEP_DOWN_PERCENT,
                      MAX_PER_DIEM
               INTO   :HV-PER-DIEM-RATE,
                      :HV-ICU-RATE,
                      :HV-ANCILLARY-RATE,
                      :HV-LOS-LIMIT,
                      :HV-STEP-DOWN-DAY,
                      :HV-STEP-DOWN-PCT,
                      :HV-MAX-PER-DIEM
               FROM   PER_DIEM_RATES
               WHERE  CONTRACT_ID = :HV-CONTRACT-ID
               AND    SERVICE_TYPE = :WS-PDM-SERVICE-TYPE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
      *--- TRY DEFAULT RATES IF CONTRACT-SPECIFIC NOT FOUND ---
               EXEC SQL
                   SELECT PER_DIEM_RATE,
                          ICU_RATE,
                          ANCILLARY_RATE,
                          LOS_LIMIT,
                          STEP_DOWN_DAY,
                          STEP_DOWN_PERCENT,
                          MAX_PER_DIEM
                   INTO   :HV-PER-DIEM-RATE,
                          :HV-ICU-RATE,
                          :HV-ANCILLARY-RATE,
                          :HV-LOS-LIMIT,
                          :HV-STEP-DOWN-DAY,
                          :HV-STEP-DOWN-PCT,
                          :HV-MAX-PER-DIEM
                   FROM   PER_DIEM_RATES
                   WHERE  CONTRACT_ID = '*DEFAULT*'
                   AND    SERVICE_TYPE = :WS-PDM-SERVICE-TYPE
                   AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                   AND    (TERMINATION_DATE IS NULL
                      OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS

               IF WS-SQLCODE NOT = 0
                   SET CLAIM-SHOULD-PEND TO TRUE
                   MOVE 'P0010' TO WS-PEND-REASON-CODE
                   MOVE 'PER DIEM RATE NOT FOUND'
                     TO WS-PEND-REASON-DESC
                   SET PEND-PRICING-REVIEW TO TRUE
                   GO TO 3300-PRICE-BY-PER-DIEM-EXIT
               END-IF
           END-IF

           MOVE HV-PER-DIEM-RATE TO WS-PDM-BASE-RATE
           MOVE HV-ICU-RATE TO WS-PDM-ICU-RATE
           MOVE HV-ANCILLARY-RATE TO WS-PDM-ANCILLARY-RATE
           MOVE HV-LOS-LIMIT TO WS-PDM-LOS-LIMIT
           MOVE HV-STEP-DOWN-DAY TO WS-PDM-STEP-DOWN-DAY
           MOVE HV-STEP-DOWN-PCT TO WS-PDM-STEP-DOWN-PCT
           MOVE HV-MAX-PER-DIEM TO WS-PDM-MAX-PER-DIEM

      *--- CALCULATE ACTUAL LENGTH OF STAY ---
           COMPUTE WS-PDM-ACTUAL-LOS =
               (WS-CLM-DISCHARGE-DATE - WS-CLM-ADMIT-DATE) / 10000
               * 365
           IF WS-PDM-ACTUAL-LOS < 1
               MOVE 1 TO WS-PDM-ACTUAL-LOS
           END-IF

      *--- DETERMINE COVERED DAYS (LESSER OF ACTUAL OR LIMIT) ---
           IF WS-PDM-LOS-LIMIT > 0
              AND WS-PDM-ACTUAL-LOS > WS-PDM-LOS-LIMIT
               MOVE WS-PDM-LOS-LIMIT TO WS-PDM-COVERED-DAYS
           ELSE
               MOVE WS-PDM-ACTUAL-LOS TO WS-PDM-COVERED-DAYS
           END-IF

      *--- GET ICU DAYS FROM CLAIM ---
           MOVE HV-ICU-DAYS TO WS-PDM-ICU-DAYS
           IF WS-PDM-ICU-DAYS > WS-PDM-COVERED-DAYS
               MOVE WS-PDM-COVERED-DAYS TO WS-PDM-ICU-DAYS
           END-IF

           COMPUTE WS-PDM-REGULAR-DAYS =
               WS-PDM-COVERED-DAYS - WS-PDM-ICU-DAYS

      *--- CALCULATE ROOM AND BOARD ---
           MOVE +0 TO WS-PDM-TOTAL-ROOM-BOARD

      *--- ICU DAYS AT ICU RATE ---
           IF WS-PDM-ICU-DAYS > 0
               COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                   WS-PDM-ICU-RATE * WS-PDM-ICU-DAYS
               ADD WS-WRK-AMOUNT-1 TO WS-PDM-TOTAL-ROOM-BOARD
           END-IF

      *--- REGULAR DAYS: APPLY STEP-DOWN IF APPLICABLE ---
           IF WS-PDM-STEP-DOWN-DAY > 0
              AND WS-PDM-REGULAR-DAYS > WS-PDM-STEP-DOWN-DAY
      *--- DAYS BEFORE STEP-DOWN: FULL RATE ---
               COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                   WS-PDM-BASE-RATE * WS-PDM-STEP-DOWN-DAY
               ADD WS-WRK-AMOUNT-1 TO WS-PDM-TOTAL-ROOM-BOARD
      *--- DAYS AFTER STEP-DOWN: REDUCED RATE ---
               COMPUTE WS-WRK-DAYS-1 =
                   WS-PDM-REGULAR-DAYS - WS-PDM-STEP-DOWN-DAY
               COMPUTE WS-WRK-AMOUNT-2 ROUNDED =
                   WS-PDM-BASE-RATE * WS-PDM-STEP-DOWN-PCT
                   * WS-WRK-DAYS-1
               ADD WS-WRK-AMOUNT-2 TO WS-PDM-TOTAL-ROOM-BOARD
           ELSE
      *--- ALL REGULAR DAYS AT FULL RATE ---
               IF WS-PDM-REGULAR-DAYS > 0
                   COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                       WS-PDM-BASE-RATE * WS-PDM-REGULAR-DAYS
                   ADD WS-WRK-AMOUNT-1 TO WS-PDM-TOTAL-ROOM-BOARD
               END-IF
           END-IF

      *--- CALCULATE ANCILLARY PER DIEM ---
           COMPUTE WS-PDM-TOTAL-ANCILLARY ROUNDED =
               WS-PDM-ANCILLARY-RATE * WS-PDM-COVERED-DAYS

      *--- TOTAL PER DIEM PAYMENT ---
           COMPUTE WS-PDM-TOTAL-PAYMENT =
               WS-PDM-TOTAL-ROOM-BOARD + WS-PDM-TOTAL-ANCILLARY

      *--- VALIDATE AGAINST MAXIMUM PER DIEM LIMIT ---
           IF WS-PDM-MAX-PER-DIEM > 0
               COMPUTE WS-WRK-AMOUNT-1 =
                   WS-PDM-MAX-PER-DIEM * WS-PDM-COVERED-DAYS
               IF WS-PDM-TOTAL-PAYMENT > WS-WRK-AMOUNT-1
                   MOVE WS-WRK-AMOUNT-1 TO WS-PDM-TOTAL-PAYMENT
               END-IF
           END-IF

      *--- CANNOT EXCEED BILLED CHARGES ---
           IF WS-PDM-TOTAL-PAYMENT > WS-CLM-TOTAL-CHARGE
               MOVE WS-CLM-TOTAL-CHARGE TO WS-PDM-TOTAL-PAYMENT
           END-IF

           IF WS-PDM-TOTAL-PAYMENT < 0
               MOVE +0 TO WS-PDM-TOTAL-PAYMENT
           END-IF

           MOVE WS-PDM-TOTAL-PAYMENT TO WS-DTL-ALLOWED-AMOUNT
           SET PRICING-FOUND TO TRUE
           .
       3300-PRICE-BY-PER-DIEM-EXIT.
           EXIT.

      *================================================================*
      * 3400 - PRICE BY CASE RATE                                      *
      *================================================================*
       3400-PRICE-BY-CASE-RATE.
      *---------------------------------------------------------------*
      * CASE RATE / GLOBAL RATE PRICING:                               *
      *   LOOK UP THE CASE RATE BY PROCEDURE CODE.                     *
      *   DETERMINE IF WITHIN THE GLOBAL PERIOD.                       *
      *   HANDLE CARVE-OUT SERVICES NOT INCLUDED IN THE CASE RATE.     *
      *---------------------------------------------------------------*
           MOVE WS-DTL-CPT-CODE TO HV-CPT-CODE
           MOVE WS-PROV-CONTRACT-ID TO HV-CONTRACT-ID

      *--- LOOK UP CASE RATE ---
           EXEC SQL
               SELECT CASE_RATE_AMOUNT,
                      GLOBAL_PERIOD_DAYS,
                      BUNDLE_INDICATOR
               INTO   :HV-CASE-RATE-AMT,
                      :HV-GLOBAL-DAYS,
                      :WS-CSR-BUNDLE-SW
               FROM   CASE_RATE_TABLE
               WHERE  CONTRACT_ID = :HV-CONTRACT-ID
               AND    CPT_CODE = :HV-CPT-CODE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
      *--- NO CASE RATE: FALL BACK TO FEE SCHEDULE ---
               MOVE 'FS' TO WS-DTL-PRICING-METHOD
               PERFORM 3100-PRICE-BY-FEE-SCHEDULE
                  THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
               GO TO 3400-PRICE-BY-CASE-RATE-EXIT
           END-IF

           MOVE HV-CASE-RATE-AMT TO WS-CSR-RATE-AMOUNT
           MOVE HV-GLOBAL-DAYS TO WS-CSR-GLOBAL-DAYS

      *--- CALCULATE GLOBAL PERIOD DATES ---
           MOVE WS-CLM-FROM-DATE TO WS-CSR-GLOBAL-START-DATE

      *--- CHECK IF THIS CLAIM FALLS WITHIN A PRIOR CASE RATE GLOBAL
      *--- PERIOD (I.E., A FOLLOW-UP VISIT THAT IS INCLUDED) ---
           EXEC SQL
               SELECT GLOBAL_START_DATE,
                      GLOBAL_END_DATE
               INTO   :HV-GLOBAL-START-DATE,
                      :HV-GLOBAL-END-DATE
               FROM   CASE_RATE_HISTORY
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    PROVIDER_NPI = :HV-PROVIDER-NPI
               AND    CPT_CODE = :HV-CPT-CODE
               AND    GLOBAL_START_DATE <= :WS-CLM-FROM-DATE
               AND    GLOBAL_END_DATE >= :WS-CLM-FROM-DATE
               AND    STATUS = 'ACTIVE'
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
      *--- WITHIN EXISTING GLOBAL PERIOD ---
               SET WITHIN-GLOBAL-PERIOD TO TRUE

      *--- CHECK FOR CARVE-OUT SERVICES ---
               MOVE +0 TO WS-CSR-CARVE-OUT-AMT

               EXEC SQL
                   SELECT CARVEOUT_AMOUNT
                   INTO   :WS-CSR-CARVE-OUT-AMT
                   FROM   CASE_RATE_CARVEOUTS
                   WHERE  CONTRACT_ID = :HV-CONTRACT-ID
                   AND    CPT_CODE = :WS-DTL-CPT-CODE
                   AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                   AND    (TERMINATION_DATE IS NULL
                      OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
               END-EXEC

               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS

               IF WS-SQLCODE = 0
      *--- THIS IS A CARVE-OUT: PAY SEPARATELY ---
                   MOVE WS-CSR-CARVE-OUT-AMT
                     TO WS-DTL-ALLOWED-AMOUNT
                   SET PRICING-FOUND TO TRUE
               ELSE
      *--- INCLUDED IN GLOBAL: $0 PAYMENT ---
                   MOVE +0 TO WS-DTL-ALLOWED-AMOUNT
                   SET PRICING-FOUND TO TRUE
                   MOVE 'AP' TO WS-DTL-ADJUD-STATUS
                   MOVE 'B0010' TO WS-DTL-DENY-REASON
               END-IF
           ELSE
      *--- NEW CASE RATE: PAY THE FULL CASE RATE ---
               SET NOT-IN-GLOBAL-PERIOD TO TRUE
               IF CSR-IS-BUNDLED
      *--- BUNDLED CASE RATE: SINGLE PAYMENT FOR ALL SERVICES ---
                   MOVE WS-CSR-RATE-AMOUNT TO WS-CSR-TOTAL-PAYMENT
               ELSE
                   MOVE WS-CSR-RATE-AMOUNT TO WS-CSR-TOTAL-PAYMENT
               END-IF

      *--- CANNOT EXCEED BILLED ---
               IF WS-CSR-TOTAL-PAYMENT > WS-CLM-TOTAL-CHARGE
                   MOVE WS-CLM-TOTAL-CHARGE
                     TO WS-CSR-TOTAL-PAYMENT
               END-IF

               MOVE WS-CSR-TOTAL-PAYMENT TO WS-DTL-ALLOWED-AMOUNT
               SET PRICING-FOUND TO TRUE

      *--- INSERT NEW GLOBAL PERIOD RECORD ---
               COMPUTE WS-CSR-GLOBAL-END-DATE =
                   WS-CLM-FROM-DATE + (WS-CSR-GLOBAL-DAYS * 10000
                   / 365)
               EXEC SQL
                   INSERT INTO CASE_RATE_HISTORY
                   (MEMBER_ID, PROVIDER_NPI, CPT_CODE,
                    GLOBAL_START_DATE, GLOBAL_END_DATE,
                    CASE_RATE_AMOUNT, CLAIM_ID, STATUS)
                   VALUES
                   (:HV-MEMBER-ID, :HV-PROVIDER-NPI, :HV-CPT-CODE,
                    :WS-CLM-FROM-DATE, :WS-CSR-GLOBAL-END-DATE,
                    :WS-CSR-RATE-AMOUNT, :HV-CLAIM-ID, 'ACTIVE')
               END-EXEC
               ADD 1 TO WS-CTR-DB-INSERTS
           END-IF
           .
       3400-PRICE-BY-CASE-RATE-EXIT.
           EXIT.

      *================================================================*
      * 3500 - PRICE BY PERCENT OF CHARGE                              *
      *================================================================*
       3500-PRICE-BY-PERCENT-CHARGE.
      *---------------------------------------------------------------*
      * PERCENT OF BILLED CHARGES PRICING:                             *
      *   APPLY THE CONTRACTED PERCENTAGE TO BILLED CHARGES.           *
      *   CHECK AGAINST USUAL AND CUSTOMARY (UCR) LIMITS.              *
      *   CHECK AGAINST MAXIMUM ALLOWABLE.                             *
      *---------------------------------------------------------------*
           MOVE WS-PROV-CONTRACT-ID TO HV-CONTRACT-ID
           MOVE WS-DTL-CPT-CODE TO HV-CPT-CODE

      *--- LOOK UP CONTRACT PERCENTAGE ---
           IF WS-PROV-CONTRACT-ID NOT = SPACES
               EXEC SQL
                   SELECT PCT_OF_CHARGE,
                          UCR_AMOUNT,
                          MAX_ALLOWABLE
                   INTO   :HV-PCT-OF-CHARGE,
                          :HV-UCR-AMOUNT,
                          :HV-MAX-ALLOW
                   FROM   PCT_CHARGE_RATES
                   WHERE  CONTRACT_ID = :HV-CONTRACT-ID
                   AND    (CPT_CODE = :HV-CPT-CODE
                      OR   CPT_CODE = '*ALL*')
                   AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                   AND    (TERMINATION_DATE IS NULL
                      OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
                   ORDER BY CPT_CODE DESC
               END-EXEC

               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS

               IF WS-SQLCODE = 0
                   MOVE HV-PCT-OF-CHARGE TO WS-PCT-CONTRACT-PCT
                   MOVE HV-UCR-AMOUNT TO WS-PCT-UCR-LIMIT
                   MOVE HV-MAX-ALLOW TO WS-PCT-MAX-ALLOW
               ELSE
      *--- DEFAULT: 80% OF CHARGES ---
                   MOVE +80.00 TO WS-PCT-CONTRACT-PCT
                   MOVE +0 TO WS-PCT-UCR-LIMIT
                   MOVE +0 TO WS-PCT-MAX-ALLOW
               END-IF
           ELSE
      *--- NO CONTRACT: DEFAULT PERCENTAGES ---
               MOVE +80.00 TO WS-PCT-CONTRACT-PCT
               MOVE +0 TO WS-PCT-UCR-LIMIT
               MOVE +0 TO WS-PCT-MAX-ALLOW
           END-IF

      *--- LOOK UP UCR IF NOT IN CONTRACT ---
           IF WS-PCT-UCR-LIMIT = 0
               EXEC SQL
                   SELECT UCR_AMOUNT
                   INTO   :HV-UCR-AMOUNT
                   FROM   UCR_TABLE
                   WHERE  CPT_CODE = :HV-CPT-CODE
                   AND    ZIP_CODE_3 = :WS-PROV-ZIP(1:3)
                   AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
                   AND    (TERMINATION_DATE IS NULL
                      OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS
               IF WS-SQLCODE = 0
                   MOVE HV-UCR-AMOUNT TO WS-PCT-UCR-LIMIT
               END-IF
           END-IF

      *--- CALCULATE PERCENT OF CHARGES ---
           COMPUTE WS-PCT-CALC-AMOUNT ROUNDED =
               WS-DTL-BILLED-AMOUNT * WS-PCT-CONTRACT-PCT / 100

      *--- APPLY UCR LIMIT ---
           IF WS-PCT-UCR-LIMIT > 0
              AND WS-PCT-CALC-AMOUNT > WS-PCT-UCR-LIMIT
               MOVE WS-PCT-UCR-LIMIT TO WS-PCT-CALC-AMOUNT
           END-IF

      *--- APPLY MAXIMUM ALLOWABLE ---
           IF WS-PCT-MAX-ALLOW > 0
              AND WS-PCT-CALC-AMOUNT > WS-PCT-MAX-ALLOW
               MOVE WS-PCT-MAX-ALLOW TO WS-PCT-CALC-AMOUNT
           END-IF

      *--- CANNOT EXCEED BILLED ---
           IF WS-PCT-CALC-AMOUNT > WS-DTL-BILLED-AMOUNT
               MOVE WS-DTL-BILLED-AMOUNT TO WS-PCT-CALC-AMOUNT
           END-IF

      *--- MULTIPLY BY UNITS ---
           COMPUTE WS-PCT-TOTAL-PAYMENT ROUNDED =
               WS-PCT-CALC-AMOUNT * WS-DTL-UNITS

           IF WS-PCT-TOTAL-PAYMENT < 0
               MOVE +0 TO WS-PCT-TOTAL-PAYMENT
           END-IF

           MOVE WS-PCT-TOTAL-PAYMENT TO WS-DTL-ALLOWED-AMOUNT
           SET PRICING-FOUND TO TRUE
           .
       3500-PRICE-BY-PERCENT-CHARGE-EXIT.
           EXIT.

      *================================================================*
      * 3600 - PRICE BY CAPITATION                                     *
      *================================================================*
       3600-PRICE-BY-CAPITATION.
      *---------------------------------------------------------------*
      * CAPITATION PRICING:                                            *
      *   CHECK IF THE SERVICE IS CAPITATED FOR THIS PROVIDER.         *
      *   IF CAPITATED, PAYMENT IS $0 BUT ENCOUNTER IS TRACKED.       *
      *   LOG THE ENCOUNTER FOR CAPITATION RECONCILIATION.             *
      *---------------------------------------------------------------*
           MOVE WS-PROV-NPI TO HV-PROVIDER-NPI
           MOVE WS-MBR-PLAN-CODE TO HV-PLAN-CODE
           MOVE WS-DTL-CPT-CODE TO HV-CPT-CODE

      *--- CHECK IF SERVICE IS CAPITATED ---
           EXEC SQL
               SELECT CAPITATION_INDICATOR
               INTO   :HV-CAPITATION-SW
               FROM   CAPITATION_SERVICES
               WHERE  NPI = :HV-PROVIDER-NPI
               AND    PLAN_CODE = :HV-PLAN-CODE
               AND    (CPT_CODE = :HV-CPT-CODE
                  OR   CPT_CODE = '*ALL*'
                  OR   SERVICE_CATEGORY =
                       (SELECT SERVICE_CATEGORY
                        FROM CPT_CATEGORY_MAP
                        WHERE CPT_CODE = :HV-CPT-CODE))
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0 AND HV-CAPITATION-SW = 'Y'
               SET SERVICE-IS-CAPITATED TO TRUE
               ADD 1 TO WS-CTR-CAPITATED
               MOVE +0 TO WS-DTL-ALLOWED-AMOUNT
               MOVE +0 TO WS-DTL-PAID-AMOUNT
               SET PRICING-FOUND TO TRUE

      *--- TRACK ENCOUNTER FOR RECONCILIATION ---
               EXEC SQL
                   INSERT INTO CAPITATION_ENCOUNTERS
                   (CLAIM_ID, MEMBER_ID, PROVIDER_NPI,
                    SERVICE_DATE, CPT_CODE, BILLED_AMOUNT,
                    ENCOUNTER_STATUS, CREATE_DATE)
                   VALUES
                   (:HV-CLAIM-ID, :HV-MEMBER-ID,
                    :HV-PROVIDER-NPI,
                    :WS-CLM-FROM-DATE, :HV-CPT-CODE,
                    :WS-DTL-BILLED-AMOUNT,
                    'POSTED', :WS-BATCH-RUN-DATE)
               END-EXEC
               ADD 1 TO WS-CTR-DB-INSERTS
           ELSE
      *--- NOT CAPITATED: FALL BACK TO FEE SCHEDULE ---
               SET SERVICE-NOT-CAPITATED TO TRUE
               MOVE 'FS' TO WS-DTL-PRICING-METHOD
               PERFORM 3100-PRICE-BY-FEE-SCHEDULE
                  THRU 3100-PRICE-BY-FEE-SCHEDULE-EXIT
           END-IF
           .
       3600-PRICE-BY-CAPITATION-EXIT.
           EXIT.

      *================================================================*
      * 4000 - APPLY MEMBER COST SHARING                               *
      *================================================================*
       4000-APPLY-MEMBER-COST-SHARING.
      *---------------------------------------------------------------*
      * APPLY DEDUCTIBLE, COPAY, COINSURANCE, AND OOP MAXIMUM.        *
      * ORDER DEPENDS ON PLAN DESIGN: SOME PLANS APPLY COPAY BEFORE   *
      * DEDUCTIBLE, OTHERS AFTER.                                      *
      *---------------------------------------------------------------*

      *--- SKIP COST SHARING FOR CAPITATED SERVICES ---
           IF SERVICE-IS-CAPITATED
               MOVE +0 TO WS-DTL-DEDUCTIBLE-AMT
               MOVE +0 TO WS-DTL-COPAY-AMOUNT
               MOVE +0 TO WS-DTL-COINSURANCE-AMT
               GO TO 4000-APPLY-MEMBER-COST-SHARING-EXIT
           END-IF

      *--- LOAD CURRENT ACCUMULATORS ---
           PERFORM 4050-LOAD-ACCUMULATORS
              THRU 4050-LOAD-ACCUMULATORS-EXIT

           IF COPAY-BEFORE-DEDUCTIBLE
      *--- PLAN DESIGN: COPAY FIRST, THEN DEDUCTIBLE ON REMAINDER ---
               PERFORM 4200-APPLY-COPAY
                  THRU 4200-APPLY-COPAY-EXIT
               PERFORM 4100-APPLY-DEDUCTIBLE
                  THRU 4100-APPLY-DEDUCTIBLE-EXIT
               PERFORM 4300-APPLY-COINSURANCE
                  THRU 4300-APPLY-COINSURANCE-EXIT
           ELSE
      *--- STANDARD: DEDUCTIBLE FIRST, THEN COPAY, THEN COINSURANCE
               PERFORM 4100-APPLY-DEDUCTIBLE
                  THRU 4100-APPLY-DEDUCTIBLE-EXIT
               PERFORM 4200-APPLY-COPAY
                  THRU 4200-APPLY-COPAY-EXIT
               PERFORM 4300-APPLY-COINSURANCE
                  THRU 4300-APPLY-COINSURANCE-EXIT
           END-IF

      *--- APPLY OUT-OF-POCKET MAXIMUM ---
           PERFORM 4400-APPLY-OOP-MAXIMUM
              THRU 4400-APPLY-OOP-MAXIMUM-EXIT

      *--- CHECK LIFETIME/ANNUAL MAXIMUM ---
           PERFORM 4500-CHECK-LIFETIME-MAXIMUM
              THRU 4500-CHECK-LIFETIME-MAXIMUM-EXIT

      *--- CALCULATE PLAN PAYMENT ---
           COMPUTE WS-DTL-PAID-AMOUNT =
               WS-DTL-ALLOWED-AMOUNT
             - WS-DTL-DEDUCTIBLE-AMT
             - WS-DTL-COPAY-AMOUNT
             - WS-DTL-COINSURANCE-AMT

           IF WS-DTL-PAID-AMOUNT < 0
               MOVE +0 TO WS-DTL-PAID-AMOUNT
           END-IF

      *--- CALCULATE PATIENT RESPONSIBILITY ---
           COMPUTE WS-DTL-PATIENT-RESP =
               WS-DTL-DEDUCTIBLE-AMT
             + WS-DTL-COPAY-AMOUNT
             + WS-DTL-COINSURANCE-AMT
             + (WS-DTL-BILLED-AMOUNT - WS-DTL-ALLOWED-AMOUNT)
           .
       4000-APPLY-MEMBER-COST-SHARING-EXIT.
           EXIT.

      *================================================================*
      * 4050 - LOAD ACCUMULATORS                                       *
      *================================================================*
       4050-LOAD-ACCUMULATORS.
      *---------------------------------------------------------------*
      * LOAD CURRENT BENEFIT ACCUMULATORS FOR THE MEMBER FROM THE     *
      * DATABASE. INCLUDES INDIVIDUAL AND FAMILY DEDUCTIBLES AND OOP. *
      *---------------------------------------------------------------*
           MOVE WS-MBR-ID TO HV-MEMBER-ID
           MOVE WS-CURRENT-YEAR TO HV-PLAN-YEAR

      *--- LOAD INDIVIDUAL ACCUMULATORS ---
           EXEC SQL
               SELECT INN_DEDUCTIBLE_USED,
                      FAM_DEDUCTIBLE_USED,
                      INN_OOP_USED,
                      FAM_OOP_USED,
                      LIFETIME_USED,
                      ANNUAL_USED,
                      RESERVED_AMOUNT,
                      LAST_UPDATE_TIMESTAMP
               INTO   :HV-IND-DEDUCT-USED,
                      :HV-FAM-DEDUCT-USED,
                      :HV-IND-OOP-USED,
                      :HV-FAM-OOP-USED,
                      :HV-LIFETIME-USED,
                      :HV-ANNUAL-USED,
                      :HV-RESERVED-AMT,
                      :HV-ACCUM-TIMESTAMP
               FROM   BENEFIT_ACCUMULATORS
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    PLAN_YEAR = :HV-PLAN-YEAR
               AND    PLAN_CODE = :WS-MBR-PLAN-CODE
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
               MOVE HV-IND-DEDUCT-USED
                 TO WS-ACC-INN-IND-DEDUCT-USED
               MOVE HV-FAM-DEDUCT-USED
                 TO WS-ACC-INN-FAM-DEDUCT-USED
               MOVE HV-IND-OOP-USED
                 TO WS-ACC-INN-IND-OOP-USED
               MOVE HV-FAM-OOP-USED
                 TO WS-ACC-INN-FAM-OOP-USED
               MOVE HV-LIFETIME-USED TO WS-ACC-LIFETIME-USED
               MOVE HV-ANNUAL-USED TO WS-ACC-ANNUAL-USED
               MOVE HV-RESERVED-AMT TO WS-ACC-RESERVED-AMT
           ELSE
      *--- NO ACCUMULATORS YET: INITIALIZE TO ZERO ---
               MOVE +0 TO WS-ACC-INN-IND-DEDUCT-USED
               MOVE +0 TO WS-ACC-INN-FAM-DEDUCT-USED
               MOVE +0 TO WS-ACC-INN-IND-OOP-USED
               MOVE +0 TO WS-ACC-INN-FAM-OOP-USED
               MOVE +0 TO WS-ACC-LIFETIME-USED
               MOVE +0 TO WS-ACC-ANNUAL-USED
               MOVE +0 TO WS-ACC-RESERVED-AMT
           END-IF

      *--- ADD RESERVED AMOUNTS FOR CONCURRENT CLAIMS ---
      * THIS PREVENTS DOUBLE-COUNTING WHEN MULTIPLE CLAIMS FOR
      * THE SAME MEMBER ARE BEING PROCESSED SIMULTANEOUSLY
           ADD WS-ACC-RESERVED-AMT TO WS-ACC-INN-IND-DEDUCT-USED

      *--- LOAD FAMILY DEDUCTIBLE STATUS ---
           IF NOT MBR-IS-SUBSCRIBER
      *--- FOR DEPENDENTS, ALSO LOAD FAMILY ACCUMULATOR STATUS ---
               EXEC SQL
                   SELECT FAMILY_MEMBER_COUNT,
                          FAMILY_MEMBERS_MET_DEDUCTIBLE
                   INTO   :HV-FAMILY-MBR-COUNT,
                          :HV-FAM-MEMBERS-MET
                   FROM   FAMILY_ACCUMULATORS
                   WHERE  GROUP_ID = :WS-MBR-GROUP-ID
                   AND    SUBSCRIBER_ID =
                       (SELECT SUBSCRIBER_ID
                        FROM MEMBER_SUBSCRIBER_XREF
                        WHERE MEMBER_ID = :HV-MEMBER-ID)
                   AND    PLAN_YEAR = :HV-PLAN-YEAR
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS
               IF WS-SQLCODE = 0
                   MOVE HV-FAMILY-MBR-COUNT
                     TO WS-ACC-FAMILY-MEMBER-CNT
                   MOVE HV-FAM-MEMBERS-MET
                     TO WS-ACC-FAM-MEMBERS-MET
               ELSE
                   MOVE 0 TO WS-ACC-FAMILY-MEMBER-CNT
                   MOVE 0 TO WS-ACC-FAM-MEMBERS-MET
               END-IF
           END-IF
           .
       4050-LOAD-ACCUMULATORS-EXIT.
           EXIT.

      *================================================================*
      * 4100 - APPLY DEDUCTIBLE                                        *
      *================================================================*
       4100-APPLY-DEDUCTIBLE.
      *---------------------------------------------------------------*
      * APPLY INDIVIDUAL AND FAMILY DEDUCTIBLE LOGIC.                  *
      * HANDLES EMBEDDED VS NON-EMBEDDED, IN-NET VS OUT-OF-NET,       *
      * CARRYOVER DEDUCTIBLE FOR Q4 CHARGES.                           *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-DTL-DEDUCTIBLE-AMT
           MOVE +0 TO WS-WRK-DEDUCT-APPLY

      *--- DETERMINE WHICH DEDUCTIBLE TO USE ---
           IF MBR-IN-NETWORK
               MOVE WS-BEN-INN-IND-DEDUCT TO WS-WRK-AMOUNT-1
               MOVE WS-ACC-INN-IND-DEDUCT-USED TO WS-WRK-AMOUNT-2
               MOVE WS-BEN-INN-FAM-DEDUCT TO WS-WRK-AMOUNT-3
               MOVE WS-ACC-INN-FAM-DEDUCT-USED TO WS-WRK-AMOUNT-4
           ELSE
               MOVE WS-BEN-OON-IND-DEDUCT TO WS-WRK-AMOUNT-1
               MOVE WS-ACC-OON-IND-DEDUCT-USED TO WS-WRK-AMOUNT-2
               MOVE WS-BEN-OON-FAM-DEDUCT TO WS-WRK-AMOUNT-3
               MOVE WS-ACC-OON-FAM-DEDUCT-USED TO WS-WRK-AMOUNT-4
           END-IF

      *--- CHECK CARRYOVER DEDUCTIBLE (Q4 CHARGES APPLY TO NEXT YEAR)
           IF CARRYOVER-APPLIES
               IF WS-CURRENT-MONTH >= 10
                  AND WS-CURRENT-MONTH <= 12
      *--- Q4: THESE CHARGES ALSO COUNT TOWARD NEXT YEAR DEDUCTIBLE
                   SET CARRYOVER-APPLIES TO TRUE
               ELSE
                   SET CARRYOVER-NA TO TRUE
               END-IF
           END-IF

      *--- CHECK FAMILY DEDUCTIBLE FIRST ---
      * FOR EMBEDDED DEDUCTIBLE: EACH FAMILY MEMBER HAS THEIR OWN
      * INDIVIDUAL DEDUCTIBLE, PLUS THERE IS A FAMILY AGGREGATE.
      * THE FAMILY DEDUCTIBLE IS MET WHEN THE AGGREGATE OF ALL
      * FAMILY MEMBERS' CHARGES REACHES THE FAMILY LIMIT,
      * REGARDLESS OF WHETHER INDIVIDUAL LIMITS ARE MET.

      *--- CHECK FAMILY DEDUCTIBLE STATUS ---
           IF WS-WRK-AMOUNT-4 >= WS-WRK-AMOUNT-3
              AND WS-WRK-AMOUNT-3 > 0
      *--- FAMILY DEDUCTIBLE IS ALREADY MET ---
               SET FAMILY-DEDUCT-MET TO TRUE
               IF NOT IS-EMBEDDED-DEDUCT
      *--- NON-EMBEDDED: FAMILY MET MEANS ALL MEMBERS ARE MET ---
                   SET DEDUCTIBLE-IS-MET TO TRUE
                   MOVE +0 TO WS-WRK-DEDUCT-APPLY
                   GO TO 4100-APPLY-DEDUCTIBLE-CALC
               END-IF
           END-IF

      *--- 2-OF-3 RULE: IF 2 OF 3 FAMILY MEMBERS MEET INDIVIDUAL
      *--- DEDUCTIBLE, THE FAMILY DEDUCTIBLE IS CONSIDERED MET ---
           IF FAM-DEDUCT-2-OF-3
               IF WS-ACC-FAM-MEMBERS-MET >= 2
                  AND WS-ACC-FAMILY-MEMBER-CNT >= 3
                   SET FAMILY-DEDUCT-MET TO TRUE
                   IF NOT IS-EMBEDDED-DEDUCT
                       SET DEDUCTIBLE-IS-MET TO TRUE
                       MOVE +0 TO WS-WRK-DEDUCT-APPLY
                       GO TO 4100-APPLY-DEDUCTIBLE-CALC
                   END-IF
               END-IF
           END-IF

      *--- CHECK INDIVIDUAL DEDUCTIBLE ---
           IF WS-WRK-AMOUNT-2 >= WS-WRK-AMOUNT-1
              AND WS-WRK-AMOUNT-1 > 0
      *--- INDIVIDUAL DEDUCTIBLE IS MET ---
               SET DEDUCTIBLE-IS-MET TO TRUE
               MOVE +0 TO WS-WRK-DEDUCT-APPLY
           ELSE
      *--- CALCULATE HOW MUCH DEDUCTIBLE APPLIES TO THIS CLAIM ---
               COMPUTE WS-WRK-DEDUCT-REMAINING =
                   WS-WRK-AMOUNT-1 - WS-WRK-AMOUNT-2

      *--- DEDUCTIBLE APPLIES TO ALLOWED AMOUNT ---
               IF COPAY-BEFORE-DEDUCTIBLE
      *--- IF COPAY WAS ALREADY TAKEN, DEDUCTIBLE ON REMAINDER ---
                   COMPUTE WS-WRK-REMAINING =
                       WS-DTL-ALLOWED-AMOUNT - WS-DTL-COPAY-AMOUNT
               ELSE
                   MOVE WS-DTL-ALLOWED-AMOUNT TO WS-WRK-REMAINING
               END-IF

               IF WS-WRK-REMAINING <= WS-WRK-DEDUCT-REMAINING
      *--- ENTIRE REMAINING AMOUNT GOES TO DEDUCTIBLE ---
                   MOVE WS-WRK-REMAINING TO WS-WRK-DEDUCT-APPLY
               ELSE
      *--- ONLY THE REMAINING DEDUCTIBLE AMOUNT APPLIES ---
                   MOVE WS-WRK-DEDUCT-REMAINING
                     TO WS-WRK-DEDUCT-APPLY
               END-IF
           END-IF

       4100-APPLY-DEDUCTIBLE-CALC.

           MOVE WS-WRK-DEDUCT-APPLY TO WS-DTL-DEDUCTIBLE-AMT

      *--- RESERVE THE DEDUCTIBLE AMOUNT IN ACCUMULATORS ---
      * THIS PREVENTS OTHER CONCURRENT CLAIMS FROM DOUBLE-COUNTING
           IF WS-WRK-DEDUCT-APPLY > 0
               COMPUTE WS-ACC-RESERVED-AMT =
                   WS-ACC-RESERVED-AMT + WS-WRK-DEDUCT-APPLY

               EXEC SQL
                   UPDATE BENEFIT_ACCUMULATORS
                   SET    RESERVED_AMOUNT =
                          RESERVED_AMOUNT + :WS-WRK-DEDUCT-APPLY,
                          LAST_UPDATE_TIMESTAMP = GETDATE()
                   WHERE  MEMBER_ID = :HV-MEMBER-ID
                   AND    PLAN_YEAR = :HV-PLAN-YEAR
                   AND    PLAN_CODE = :WS-MBR-PLAN-CODE
               END-EXEC
               ADD 1 TO WS-CTR-DB-UPDATES
           END-IF
           .
       4100-APPLY-DEDUCTIBLE-EXIT.
           EXIT.

      *================================================================*
      * 4200 - APPLY COPAY                                             *
      *================================================================*
       4200-APPLY-COPAY.
      *---------------------------------------------------------------*
      * DETERMINE AND APPLY THE APPROPRIATE COPAY BASED ON THE TYPE   *
      * OF SERVICE. ER COPAY IS WAIVED IF PATIENT IS ADMITTED.         *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-DTL-COPAY-AMOUNT
           MOVE +0 TO WS-WRK-COPAY-APPLY

      *--- DETERMINE COPAY AMOUNT BASED ON SERVICE TYPE ---
           EVALUATE TRUE
               WHEN CLM-OFFICE-VISIT
                   IF PROV-IS-PCP
                       MOVE WS-BEN-COPAY-PCP TO WS-WRK-COPAY-APPLY
                   ELSE
                       MOVE WS-BEN-COPAY-SPEC
                         TO WS-WRK-COPAY-APPLY
                   END-IF

               WHEN CLM-EMERGENCY
      *--- CHECK IF ER VISIT RESULTED IN ADMISSION ---
                   EXEC SQL
                       SELECT 'Y'
                       INTO   :WS-ER-ADMITTED-SW
                       FROM   CLAIM_HEADER
                       WHERE  MEMBER_ID = :HV-MEMBER-ID
                       AND    CLAIM_TYPE = 'IN'
                       AND    CLAIM_SUB_TYPE = 'IP'
                       AND    ADMIT_DATE = :WS-CLM-FROM-DATE
                       AND    CLAIM_STATUS IN ('PA', 'AJ', 'RC')
                   END-EXEC
                   MOVE SQLCODE TO WS-SQLCODE
                   ADD 1 TO WS-CTR-DB-READS

                   IF WS-SQLCODE = 0 AND ER-WAS-ADMITTED
      *--- ER COPAY WAIVED DUE TO ADMISSION ---
                       MOVE +0 TO WS-WRK-COPAY-APPLY
                   ELSE
                       MOVE WS-BEN-COPAY-ER
                         TO WS-WRK-COPAY-APPLY
                   END-IF

               WHEN CLM-INPATIENT
                   MOVE WS-BEN-COPAY-INPT TO WS-WRK-COPAY-APPLY

               WHEN CLM-OUTPATIENT
                   IF POS-ASC
                       MOVE WS-BEN-COPAY-OUTPT
                         TO WS-WRK-COPAY-APPLY
                   ELSE
                       MOVE WS-BEN-COPAY-OUTPT
                         TO WS-WRK-COPAY-APPLY
                   END-IF

               WHEN CLM-LAB
                   MOVE WS-BEN-COPAY-LAB TO WS-WRK-COPAY-APPLY

               WHEN CLM-RADIOLOGY
                   MOVE WS-BEN-COPAY-RAD TO WS-WRK-COPAY-APPLY

               WHEN CLM-MENTAL-HEALTH OR CLM-SUBSTANCE-ABUSE
      *--- MENTAL HEALTH PARITY: SAME COPAY AS MEDICAL ---
                   IF BEN-MH-PARITY
                       IF PROV-IS-PCP
                           MOVE WS-BEN-COPAY-PCP
                             TO WS-WRK-COPAY-APPLY
                       ELSE
                           MOVE WS-BEN-COPAY-SPEC
                             TO WS-WRK-COPAY-APPLY
                       END-IF
                   ELSE
                       MOVE WS-BEN-COPAY-MH
                         TO WS-WRK-COPAY-APPLY
                   END-IF

               WHEN CLM-TELEHEALTH
      *--- TELEHEALTH: USUALLY PCP OR SPECIALIST COPAY ---
                   IF PROV-IS-PCP
                       MOVE WS-BEN-COPAY-PCP
                         TO WS-WRK-COPAY-APPLY
                   ELSE
                       MOVE WS-BEN-COPAY-SPEC
                         TO WS-WRK-COPAY-APPLY
                   END-IF

               WHEN OTHER
      *--- DEFAULT: USE SPECIALIST COPAY ---
                   MOVE WS-BEN-COPAY-SPEC TO WS-WRK-COPAY-APPLY
           END-EVALUATE

      *--- COPAY CANNOT EXCEED ALLOWED AMOUNT ---
           IF WS-WRK-COPAY-APPLY > WS-DTL-ALLOWED-AMOUNT
               MOVE WS-DTL-ALLOWED-AMOUNT TO WS-WRK-COPAY-APPLY
           END-IF

      *--- IF COPAY AFTER DEDUCTIBLE, REDUCE BY DEDUCTIBLE APPLIED ---
           IF COPAY-AFTER-DEDUCTIBLE
               COMPUTE WS-WRK-REMAINING =
                   WS-DTL-ALLOWED-AMOUNT - WS-DTL-DEDUCTIBLE-AMT
               IF WS-WRK-COPAY-APPLY > WS-WRK-REMAINING
                   MOVE WS-WRK-REMAINING TO WS-WRK-COPAY-APPLY
               END-IF
               IF WS-WRK-REMAINING <= 0
                   MOVE +0 TO WS-WRK-COPAY-APPLY
               END-IF
           END-IF

           MOVE WS-WRK-COPAY-APPLY TO WS-DTL-COPAY-AMOUNT
           .
       4200-APPLY-COPAY-EXIT.
           EXIT.

      *================================================================*
      * 4300 - APPLY COINSURANCE                                       *
      *================================================================*
       4300-APPLY-COINSURANCE.
      *---------------------------------------------------------------*
      * CALCULATE COINSURANCE ON THE REMAINING BALANCE AFTER           *
      * DEDUCTIBLE AND COPAY. DIFFERENT RATES FOR IN-NET VS OUT.       *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-DTL-COINSURANCE-AMT
           MOVE +0 TO WS-WRK-COINS-APPLY

      *--- CALCULATE REMAINING AFTER DEDUCTIBLE AND COPAY ---
           COMPUTE WS-WRK-REMAINING =
               WS-DTL-ALLOWED-AMOUNT
             - WS-DTL-DEDUCTIBLE-AMT
             - WS-DTL-COPAY-AMOUNT

           IF WS-WRK-REMAINING <= 0
               MOVE +0 TO WS-WRK-COINS-APPLY
               GO TO 4300-APPLY-COINSURANCE-CALC
           END-IF

      *--- DETERMINE COINSURANCE RATE ---
           IF MBR-IN-NETWORK
               COMPUTE WS-WRK-COINS-APPLY ROUNDED =
                   WS-WRK-REMAINING
                   * (1 - (WS-BEN-INN-COINSURANCE / 100))
           ELSE
               COMPUTE WS-WRK-COINS-APPLY ROUNDED =
                   WS-WRK-REMAINING
                   * (1 - (WS-BEN-OON-COINSURANCE / 100))
           END-IF

      *--- MENTAL HEALTH PARITY CHECK ---
           IF (CLM-MENTAL-HEALTH OR CLM-SUBSTANCE-ABUSE)
              AND BEN-MH-PARITY
      *--- USE MEDICAL COINSURANCE RATE FOR MH PARITY ---
               COMPUTE WS-WRK-COINS-APPLY ROUNDED =
                   WS-WRK-REMAINING
                   * (1 - (WS-BEN-INN-COINSURANCE / 100))
           END-IF

       4300-APPLY-COINSURANCE-CALC.

           IF WS-WRK-COINS-APPLY < 0
               MOVE +0 TO WS-WRK-COINS-APPLY
           END-IF

           MOVE WS-WRK-COINS-APPLY TO WS-DTL-COINSURANCE-AMT
           .
       4300-APPLY-COINSURANCE-EXIT.
           EXIT.

      *================================================================*
      * 4400 - APPLY OOP MAXIMUM                                       *
      *================================================================*
       4400-APPLY-OOP-MAXIMUM.
      *---------------------------------------------------------------*
      * CHECK IF THE OUT-OF-POCKET MAXIMUM HAS BEEN REACHED.          *
      * OOP INCLUDES DEDUCTIBLE + COPAY + COINSURANCE.                 *
      * IF MET, PLAN PAYS 100% (MEMBER COST SHARING = $0).            *
      * SEPARATE TRACKING FOR IN-NET AND OUT-OF-NET.                   *
      *---------------------------------------------------------------*

      *--- CALCULATE TOTAL MEMBER COST SHARING FOR THIS CLAIM ---
           COMPUTE WS-WRK-MEMBER-RESP =
               WS-DTL-DEDUCTIBLE-AMT
             + WS-DTL-COINSURANCE-AMT

      *--- ADD COPAY IF PLAN COUNTS COPAY TOWARD OOP ---
           IF COPAY-COUNTS-TO-OOP
               ADD WS-DTL-COPAY-AMOUNT TO WS-WRK-MEMBER-RESP
           END-IF

      *--- DETERMINE WHICH OOP MAX TO USE ---
           IF MBR-IN-NETWORK
               MOVE WS-BEN-INN-IND-OOP TO WS-WRK-AMOUNT-1
               MOVE WS-ACC-INN-IND-OOP-USED TO WS-WRK-AMOUNT-2
               MOVE WS-BEN-INN-FAM-OOP TO WS-WRK-AMOUNT-3
               MOVE WS-ACC-INN-FAM-OOP-USED TO WS-WRK-AMOUNT-4
           ELSE
               MOVE WS-BEN-OON-IND-OOP TO WS-WRK-AMOUNT-1
               MOVE WS-ACC-OON-IND-OOP-USED TO WS-WRK-AMOUNT-2
               MOVE WS-BEN-OON-FAM-OOP TO WS-WRK-AMOUNT-3
               MOVE WS-ACC-OON-FAM-OOP-USED TO WS-WRK-AMOUNT-4
           END-IF

      *--- CHECK FAMILY OOP FIRST ---
           IF WS-WRK-AMOUNT-4 >= WS-WRK-AMOUNT-3
              AND WS-WRK-AMOUNT-3 > 0
      *--- FAMILY OOP IS MET: PLAN PAYS 100% ---
               SET FAMILY-OOP-MET TO TRUE
               SET OOP-MAX-IS-MET TO TRUE
               MOVE +0 TO WS-DTL-DEDUCTIBLE-AMT
               MOVE +0 TO WS-DTL-COPAY-AMOUNT
               MOVE +0 TO WS-DTL-COINSURANCE-AMT
               MOVE WS-DTL-ALLOWED-AMOUNT TO WS-DTL-PAID-AMOUNT
               GO TO 4400-APPLY-OOP-MAXIMUM-EXIT
           END-IF

      *--- CHECK INDIVIDUAL OOP ---
           IF WS-WRK-AMOUNT-2 >= WS-WRK-AMOUNT-1
              AND WS-WRK-AMOUNT-1 > 0
      *--- INDIVIDUAL OOP IS MET: PLAN PAYS 100% ---
               SET OOP-MAX-IS-MET TO TRUE
               MOVE +0 TO WS-DTL-DEDUCTIBLE-AMT
               MOVE +0 TO WS-DTL-COPAY-AMOUNT
               MOVE +0 TO WS-DTL-COINSURANCE-AMT
               MOVE WS-DTL-ALLOWED-AMOUNT TO WS-DTL-PAID-AMOUNT
               GO TO 4400-APPLY-OOP-MAXIMUM-EXIT
           END-IF

      *--- CHECK IF THIS CLAIM WOULD EXCEED OOP ---
           COMPUTE WS-WRK-OOP-REMAINING =
               WS-WRK-AMOUNT-1 - WS-WRK-AMOUNT-2

           IF WS-WRK-MEMBER-RESP > WS-WRK-OOP-REMAINING
              AND WS-WRK-OOP-REMAINING > 0
      *--- THIS CLAIM CAUSES OOP TO BE MET ---
      *--- MEMBER ONLY PAYS UP TO THE REMAINING OOP ---
               MOVE WS-WRK-OOP-REMAINING TO WS-WRK-MEMBER-RESP

      *--- REDISTRIBUTE: DEDUCTIBLE FIRST, THEN COINSURANCE ---
               IF WS-DTL-DEDUCTIBLE-AMT > WS-WRK-MEMBER-RESP
                   MOVE WS-WRK-MEMBER-RESP
                     TO WS-DTL-DEDUCTIBLE-AMT
                   MOVE +0 TO WS-DTL-COINSURANCE-AMT
                   IF COPAY-COUNTS-TO-OOP
                       MOVE +0 TO WS-DTL-COPAY-AMOUNT
                   END-IF
               ELSE
                   COMPUTE WS-WRK-REMAINING =
                       WS-WRK-MEMBER-RESP - WS-DTL-DEDUCTIBLE-AMT
                   IF COPAY-COUNTS-TO-OOP
                       IF WS-DTL-COPAY-AMOUNT > WS-WRK-REMAINING
                           MOVE WS-WRK-REMAINING
                             TO WS-DTL-COPAY-AMOUNT
                           MOVE +0 TO WS-DTL-COINSURANCE-AMT
                       ELSE
                           COMPUTE WS-WRK-REMAINING =
                               WS-WRK-REMAINING
                             - WS-DTL-COPAY-AMOUNT
                           MOVE WS-WRK-REMAINING
                             TO WS-DTL-COINSURANCE-AMT
                       END-IF
                   ELSE
                       MOVE WS-WRK-REMAINING
                         TO WS-DTL-COINSURANCE-AMT
                   END-IF
               END-IF

      *--- RECALCULATE PLAN PAYMENT ---
               COMPUTE WS-DTL-PAID-AMOUNT =
                   WS-DTL-ALLOWED-AMOUNT
                 - WS-DTL-DEDUCTIBLE-AMT
                 - WS-DTL-COPAY-AMOUNT
                 - WS-DTL-COINSURANCE-AMT
           END-IF

      *--- CHECK FAMILY OOP WOULD BE EXCEEDED ---
           COMPUTE WS-WRK-FAM-OOP-REMAINING =
               WS-WRK-AMOUNT-3 - WS-WRK-AMOUNT-4

           IF WS-WRK-FAM-OOP-REMAINING > 0
              AND WS-WRK-FAM-OOP-REMAINING < WS-WRK-OOP-REMAINING
      *--- FAMILY OOP IS CLOSER TO BEING MET THAN INDIVIDUAL ---
      *--- USE FAMILY OOP REMAINING INSTEAD ---
               IF WS-WRK-MEMBER-RESP > WS-WRK-FAM-OOP-REMAINING
                   MOVE WS-WRK-FAM-OOP-REMAINING
                     TO WS-WRK-MEMBER-RESP
      *--- REDISTRIBUTE COST SHARING TO FAMILY OOP LIMIT ---
                   PERFORM 4410-REDISTRIBUTE-COST-SHARING
                      THRU 4410-REDISTRIBUTE-COST-SHARING-EXIT
               END-IF
           END-IF
           .
       4400-APPLY-OOP-MAXIMUM-EXIT.
           EXIT.

      *================================================================*
      * 4410 - REDISTRIBUTE COST SHARING                               *
      *================================================================*
       4410-REDISTRIBUTE-COST-SHARING.
      *---------------------------------------------------------------*
      * REDISTRIBUTE MEMBER COST SHARING WHEN OOP IS PARTIALLY MET.   *
      *---------------------------------------------------------------*
           IF WS-DTL-DEDUCTIBLE-AMT > WS-WRK-MEMBER-RESP
               MOVE WS-WRK-MEMBER-RESP TO WS-DTL-DEDUCTIBLE-AMT
               MOVE +0 TO WS-DTL-COINSURANCE-AMT
               IF COPAY-COUNTS-TO-OOP
                   MOVE +0 TO WS-DTL-COPAY-AMOUNT
               END-IF
           ELSE
               COMPUTE WS-WRK-REMAINING =
                   WS-WRK-MEMBER-RESP - WS-DTL-DEDUCTIBLE-AMT
               IF COPAY-COUNTS-TO-OOP
                   IF WS-DTL-COPAY-AMOUNT > WS-WRK-REMAINING
                       MOVE WS-WRK-REMAINING
                         TO WS-DTL-COPAY-AMOUNT
                       MOVE +0 TO WS-DTL-COINSURANCE-AMT
                   ELSE
                       COMPUTE WS-DTL-COINSURANCE-AMT =
                           WS-WRK-REMAINING - WS-DTL-COPAY-AMOUNT
                   END-IF
               ELSE
                   IF WS-WRK-REMAINING < WS-DTL-COINSURANCE-AMT
                       MOVE WS-WRK-REMAINING
                         TO WS-DTL-COINSURANCE-AMT
                   END-IF
               END-IF
           END-IF

      *--- RECALCULATE PLAN PAYMENT AFTER REDISTRIBUTION ---
           COMPUTE WS-DTL-PAID-AMOUNT =
               WS-DTL-ALLOWED-AMOUNT
             - WS-DTL-DEDUCTIBLE-AMT
             - WS-DTL-COPAY-AMOUNT
             - WS-DTL-COINSURANCE-AMT

           IF WS-DTL-PAID-AMOUNT < 0
               MOVE +0 TO WS-DTL-PAID-AMOUNT
           END-IF
           .
       4410-REDISTRIBUTE-COST-SHARING-EXIT.
           EXIT.

      *================================================================*
      * 4500 - CHECK LIFETIME MAXIMUM                                  *
      *================================================================*
       4500-CHECK-LIFETIME-MAXIMUM.
      *---------------------------------------------------------------*
      * CHECK LIFETIME AND ANNUAL BENEFIT MAXIMUMS.                    *
      * NOTE: ACA ELIMINATED LIFETIME MAXIMUMS FOR NON-GRANDFATHERED  *
      * PLANS EFFECTIVE 2010. THIS LOGIC REMAINS FOR GRANDFATHERED     *
      * PLANS AND CERTAIN EXCEPTED BENEFITS.                           *
      *---------------------------------------------------------------*

      *--- ANNUAL BENEFIT MAXIMUM ---
           IF WS-BEN-ANNUAL-MAX > 0
               IF WS-ACC-ANNUAL-USED >= WS-BEN-ANNUAL-MAX
      *--- ANNUAL MAX ALREADY EXCEEDED ---
                   SET ANNUAL-MAX-EXCEEDED TO TRUE
                   MOVE +0 TO WS-DTL-PAID-AMOUNT
                   MOVE WS-DTL-ALLOWED-AMOUNT
                     TO WS-DTL-PATIENT-RESP
                   GO TO 4500-CHECK-LIFETIME-MAXIMUM-EXIT
               END-IF

      *--- CHECK IF THIS CLAIM WOULD EXCEED ANNUAL MAX ---
               COMPUTE WS-WRK-REMAINING =
                   WS-BEN-ANNUAL-MAX - WS-ACC-ANNUAL-USED

               IF WS-DTL-PAID-AMOUNT > WS-WRK-REMAINING
      *--- REDUCE PAYMENT TO REMAINING ANNUAL MAX ---
                   MOVE WS-WRK-REMAINING TO WS-DTL-PAID-AMOUNT
                   COMPUTE WS-DTL-PATIENT-RESP =
                       WS-DTL-ALLOWED-AMOUNT - WS-DTL-PAID-AMOUNT
                   SET ANNUAL-MAX-EXCEEDED TO TRUE
               END-IF
           END-IF

      *--- LIFETIME MAXIMUM (GRANDFATHERED PLANS ONLY) ---
           IF IS-GRANDFATHERED-PLAN
              AND WS-BEN-LIFETIME-MAX > 0
               IF WS-ACC-LIFETIME-USED >= WS-BEN-LIFETIME-MAX
      *--- LIFETIME MAX ALREADY EXCEEDED ---
                   SET LIFETIME-MAX-EXCEEDED TO TRUE
                   MOVE +0 TO WS-DTL-PAID-AMOUNT
                   MOVE WS-DTL-ALLOWED-AMOUNT
                     TO WS-DTL-PATIENT-RESP
                   GO TO 4500-CHECK-LIFETIME-MAXIMUM-EXIT
               END-IF

      *--- CHECK IF THIS CLAIM WOULD EXCEED LIFETIME MAX ---
               COMPUTE WS-WRK-REMAINING =
                   WS-BEN-LIFETIME-MAX - WS-ACC-LIFETIME-USED

               IF WS-DTL-PAID-AMOUNT > WS-WRK-REMAINING
      *--- REDUCE PAYMENT TO REMAINING LIFETIME MAX ---
                   MOVE WS-WRK-REMAINING TO WS-DTL-PAID-AMOUNT
                   COMPUTE WS-DTL-PATIENT-RESP =
                       WS-DTL-ALLOWED-AMOUNT - WS-DTL-PAID-AMOUNT
                   SET LIFETIME-MAX-EXCEEDED TO TRUE
               END-IF
           END-IF
           .
       4500-CHECK-LIFETIME-MAXIMUM-EXIT.
           EXIT.

      *================================================================*
      * 5000 - COORDINATION OF BENEFITS                                *
      *================================================================*
       5000-COORDINATION-OF-BENEFITS.
      *---------------------------------------------------------------*
      * COORDINATE BENEFITS WITH OTHER INSURANCE COVERAGE.             *
      * DETERMINE ORDER (PRIMARY/SECONDARY), THEN CALCULATE.           *
      *---------------------------------------------------------------*
           PERFORM 5100-DETERMINE-COB-ORDER
              THRU 5100-DETERMINE-COB-ORDER-EXIT

           IF WE-ARE-PRIMARY
               PERFORM 5200-CALCULATE-COB-PRIMARY
                  THRU 5200-CALCULATE-COB-PRIMARY-EXIT
           ELSE
               PERFORM 5300-CALCULATE-COB-SECONDARY
                  THRU 5300-CALCULATE-COB-SECONDARY-EXIT
           END-IF

      *--- CHECK FOR MEDICARE SECONDARY PAYER ---
           IF MBR-HAS-MEDICARE
               PERFORM 5400-MEDICARE-SECONDARY-PAYER
                  THRU 5400-MEDICARE-SECONDARY-PAYER-EXIT
           END-IF
           .
       5000-COORDINATION-OF-BENEFITS-EXIT.
           EXIT.

      *================================================================*
      * 5100 - DETERMINE COB ORDER                                     *
      *================================================================*
       5100-DETERMINE-COB-ORDER.
      *---------------------------------------------------------------*
      * DETERMINE WHETHER WE ARE PRIMARY OR SECONDARY PAYER USING      *
      * STANDARD COB DETERMINATION RULES IN ORDER OF PRECEDENCE:       *
      * 1. COURT ORDER OVERRIDE                                        *
      * 2. MEDICARE SECONDARY PAYER RULES                              *
      * 3. ACTIVE/INACTIVE EMPLOYEE RULE                               *
      * 4. COBRA SECONDARY RULE                                        *
      * 5. BIRTHDAY RULE (DEPENDENT CHILDREN)                          *
      * 6. GENDER RULE (LEGACY PLANS)                                  *
      * 7. LONGEST COVERAGE RULE                                       *
      *---------------------------------------------------------------*
           SET WE-ARE-PRIMARY TO TRUE

      *--- RULE 1: COURT ORDER OVERRIDE ---
           EXEC SQL
               SELECT COURT_ORDER_INDICATOR
               INTO   :HV-COURT-ORDER-SW
               FROM   MEMBER_COB_OVERRIDE
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC
           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
               IF HV-COURT-ORDER-SW = 'P'
                   SET WE-ARE-PRIMARY TO TRUE
                   SET COB-RULE-COURT TO TRUE
                   GO TO 5100-DETERMINE-COB-ORDER-EXIT
               ELSE IF HV-COURT-ORDER-SW = 'S'
                   SET WE-ARE-SECONDARY TO TRUE
                   SET COB-RULE-COURT TO TRUE
                   GO TO 5100-DETERMINE-COB-ORDER-EXIT
               END-IF
           END-IF

      *--- RULE 2: MEDICARE SECONDARY PAYER ---
           IF MBR-HAS-MEDICARE
               SET COB-RULE-MSP TO TRUE
               EVALUATE TRUE
                   WHEN MCR-REASON-AGED
      *--- WORKING AGED: EMPLOYER GROUP HEALTH PLAN IS PRIMARY
      *--- IF EMPLOYER HAS 20+ EMPLOYEES ---
                       IF WS-MBR-EMPLOYER-SIZE >= 20
                           SET WE-ARE-PRIMARY TO TRUE
                           SET MSP-WORKING-AGED TO TRUE
                       ELSE
                           SET WE-ARE-SECONDARY TO TRUE
                           SET MSP-WORKING-AGED TO TRUE
                       END-IF
                       GO TO 5100-DETERMINE-COB-ORDER-EXIT
                   WHEN MCR-REASON-DISABILITY
      *--- DISABILITY: LARGE GROUP HEALTH PLAN IS PRIMARY
      *--- IF EMPLOYER HAS 100+ EMPLOYEES ---
                       IF WS-MBR-EMPLOYER-SIZE >= 100
                           SET WE-ARE-PRIMARY TO TRUE
                           SET MSP-DISABILITY TO TRUE
                       ELSE
                           SET WE-ARE-SECONDARY TO TRUE
                           SET MSP-DISABILITY TO TRUE
                       END-IF
                       GO TO 5100-DETERMINE-COB-ORDER-EXIT
                   WHEN MCR-REASON-ESRD
      *--- ESRD: 30-MONTH COORDINATION PERIOD ---
      *--- DURING FIRST 30 MONTHS, GROUP PLAN IS PRIMARY ---
                       COMPUTE WS-WRK-DAYS-1 =
                           (WS-CLM-FROM-DATE
                            - WS-MBR-ESRD-START-DATE) / 10000
                            * 365
                       IF WS-WRK-DAYS-1 <=
                          (WS-WRK-ESRD-COORD-MONTHS * 30)
                           SET WE-ARE-PRIMARY TO TRUE
                           SET MSP-ESRD TO TRUE
                       ELSE
                           SET WE-ARE-SECONDARY TO TRUE
                           SET MSP-ESRD TO TRUE
                       END-IF
                       GO TO 5100-DETERMINE-COB-ORDER-EXIT
               END-EVALUATE
           END-IF

      *--- RULE 3: ACTIVE VS INACTIVE EMPLOYEE ---
      * PLAN COVERING ACTIVE EMPLOYEE IS PRIMARY OVER PLAN
      * COVERING RETIREE OR LAID-OFF EMPLOYEE
      * (SKIP IF BOTH PLANS ARE SAME STATUS)
           IF MBR-ON-COBRA
      *--- RULE 4: COBRA IS ALWAYS SECONDARY ---
               SET WE-ARE-SECONDARY TO TRUE
               SET COB-RULE-COBRA TO TRUE
               GO TO 5100-DETERMINE-COB-ORDER-EXIT
           END-IF

      *--- RULE 5: BIRTHDAY RULE (FOR DEPENDENT CHILDREN) ---
           IF MBR-IS-CHILD
      *--- PARENT WHOSE BIRTHDAY (MONTH/DAY) COMES FIRST IN
      *--- CALENDAR YEAR HAS THE PRIMARY PLAN ---
               MOVE WS-MBR-SUBSCRIBER-DOB TO HV-SUBSCRIBER-DOB-M
               EXEC SQL
                   SELECT SUBSCRIBER_DOB_MALE,
                          SUBSCRIBER_DOB_FEMALE
                   INTO   :HV-SUBSCRIBER-DOB-M,
                          :HV-SUBSCRIBER-DOB-F
                   FROM   MEMBER_COB_INFO
                   WHERE  MEMBER_ID = :HV-MEMBER-ID
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS

               IF WS-SQLCODE = 0
      *--- COMPARE MONTH/DAY OF EACH PARENT ---
                   MOVE HV-SUBSCRIBER-DOB-M(5:4)
                     TO WS-COB-SUBSCRIBER-DOB-M
                   MOVE HV-SUBSCRIBER-DOB-F(5:4)
                     TO WS-COB-SUBSCRIBER-DOB-F
                   IF WS-COB-SUBSCRIBER-DOB-M <
                      WS-COB-SUBSCRIBER-DOB-F
      *--- MALE SUBSCRIBER BIRTHDAY IS EARLIER: WE ARE PRIMARY
      *--- IF WE COVER THE MALE SUBSCRIBER ---
                       IF MBR-MALE
                           SET WE-ARE-PRIMARY TO TRUE
                       ELSE
                           SET WE-ARE-SECONDARY TO TRUE
                       END-IF
                   ELSE
                       IF WS-COB-SUBSCRIBER-DOB-F <
                          WS-COB-SUBSCRIBER-DOB-M
                           IF MBR-FEMALE
                               SET WE-ARE-PRIMARY TO TRUE
                           ELSE
                               SET WE-ARE-SECONDARY TO TRUE
                           END-IF
                       ELSE
      *--- SAME BIRTHDAY: USE LONGEST COVERAGE RULE ---
                           GO TO 5100-LONGEST-COVERAGE
                       END-IF
                   END-IF
                   SET COB-RULE-BIRTHDAY TO TRUE
                   GO TO 5100-DETERMINE-COB-ORDER-EXIT
               END-IF
           END-IF

      *--- RULE 7: LONGEST COVERAGE RULE ---
       5100-LONGEST-COVERAGE.
           EXEC SQL
               SELECT OTHER_COV_EFFECTIVE_DATE
               INTO   :HV-OTHER-COV-EFF-DATE
               FROM   MEMBER_OTHER_COVERAGE
               WHERE  MEMBER_ID = :HV-MEMBER-ID
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC
           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
               IF WS-MBR-EFF-DATE <= HV-OTHER-COV-EFF-DATE
      *--- WE HAVE LONGER COVERAGE: WE ARE PRIMARY ---
                   SET WE-ARE-PRIMARY TO TRUE
               ELSE
                   SET WE-ARE-SECONDARY TO TRUE
               END-IF
               SET COB-RULE-LONGEST TO TRUE
           ELSE
      *--- CANNOT DETERMINE: DEFAULT TO PRIMARY ---
               SET WE-ARE-PRIMARY TO TRUE
           END-IF
           .
       5100-DETERMINE-COB-ORDER-EXIT.
           EXIT.

      *================================================================*
      * 5200 - CALCULATE COB PRIMARY                                   *
      *================================================================*
       5200-CALCULATE-COB-PRIMARY.
      *---------------------------------------------------------------*
      * AS PRIMARY PAYER, PROCESS NORMALLY AS IF NO OTHER COVERAGE.    *
      * THE COB AMOUNT ON THE CLAIM WILL BE $0 FOR PRIMARY.           *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-DTL-COB-AMOUNT
           MOVE +0 TO WS-COB-SAVINGS
           MOVE WS-DTL-ALLOWED-AMOUNT TO WS-COB-OUR-ALLOWED
           MOVE WS-DTL-PAID-AMOUNT TO WS-COB-OUR-BENEFIT
           .
       5200-CALCULATE-COB-PRIMARY-EXIT.
           EXIT.

      *================================================================*
      * 5300 - CALCULATE COB SECONDARY                                 *
      *================================================================*
       5300-CALCULATE-COB-SECONDARY.
      *---------------------------------------------------------------*
      * AS SECONDARY PAYER, CALCULATE BENEFIT BASED ON COB METHOD:     *
      *   TRADITIONAL: PAY LESSER OF OWN BENEFIT OR BALANCE            *
      *   MAINTENANCE OF BENEFITS: OWN BENEFIT MINUS PRIMARY PAID      *
      *   NON-DUPLICATION: PAY DIFFERENCE, NO OVERPAYMENT              *
      *   CARVE-OUT: REDUCE OWN BENEFIT BY PRIMARY PAID                *
      *   STANDARD (NAIC): PAY LESSER OF OWN BENEFIT OR BALANCE       *
      *---------------------------------------------------------------*

      *--- DETERMINE COB METHOD FROM PLAN ---
           EXEC SQL
               SELECT COB_METHOD
               INTO   :WS-COB-METHOD
               FROM   BENEFIT_PLAN
               WHERE  PLAN_CODE = :WS-MBR-PLAN-CODE
               AND    PLAN_YEAR = :WS-CURRENT-YEAR
           END-EXEC
           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE NOT = 0
      *--- DEFAULT TO TRADITIONAL COB ---
               SET COB-TRADITIONAL TO TRUE
           END-IF

      *--- CALCULATE BALANCE AFTER PRIMARY ---
           COMPUTE WS-COB-BALANCE-AFTER-PRI =
               WS-DTL-BILLED-AMOUNT - WS-COB-OTHER-PAID

           IF WS-COB-BALANCE-AFTER-PRI < 0
               MOVE +0 TO WS-COB-BALANCE-AFTER-PRI
           END-IF

      *--- CALCULATE OUR BENEFIT (WHAT WE WOULD PAY AS PRIMARY) ---
           MOVE WS-DTL-PAID-AMOUNT TO WS-COB-OUR-BENEFIT
           MOVE WS-DTL-ALLOWED-AMOUNT TO WS-COB-OUR-ALLOWED

           EVALUATE TRUE
               WHEN COB-TRADITIONAL
      *--- TRADITIONAL: PAY LESSER OF OWN BENEFIT OR BALANCE ---
                   IF WS-COB-OUR-BENEFIT >
                      WS-COB-BALANCE-AFTER-PRI
                       MOVE WS-COB-BALANCE-AFTER-PRI
                         TO WS-COB-SECONDARY-PAY
                   ELSE
                       MOVE WS-COB-OUR-BENEFIT
                         TO WS-COB-SECONDARY-PAY
                   END-IF

               WHEN COB-MAINTENANCE
      *--- MAINTENANCE OF BENEFITS: OWN BENEFIT - PRIMARY PAID ---
                   COMPUTE WS-COB-SECONDARY-PAY =
                       WS-COB-OUR-BENEFIT - WS-COB-OTHER-PAID
                   IF WS-COB-SECONDARY-PAY < 0
                       MOVE +0 TO WS-COB-SECONDARY-PAY
                   END-IF
      *--- CANNOT EXCEED BALANCE ---
                   IF WS-COB-SECONDARY-PAY >
                      WS-COB-BALANCE-AFTER-PRI
                       MOVE WS-COB-BALANCE-AFTER-PRI
                         TO WS-COB-SECONDARY-PAY
                   END-IF

               WHEN COB-NON-DUPLICATION
      *--- NON-DUPLICATION: PAY DIFFERENCE, NO OVERPAYMENT ---
                   IF WS-COB-OUR-ALLOWED > WS-COB-OTHER-PAID
                       COMPUTE WS-COB-SECONDARY-PAY =
                           WS-COB-OUR-ALLOWED - WS-COB-OTHER-PAID
                   ELSE
                       MOVE +0 TO WS-COB-SECONDARY-PAY
                   END-IF

               WHEN COB-CARVE-OUT
      *--- CARVE-OUT: REDUCE OWN BENEFIT BY PRIMARY PAYMENT ---
                   COMPUTE WS-COB-SECONDARY-PAY =
                       WS-COB-OUR-BENEFIT - WS-COB-OTHER-PAID
                   IF WS-COB-SECONDARY-PAY < 0
                       MOVE +0 TO WS-COB-SECONDARY-PAY
                   END-IF

               WHEN COB-STANDARD
      *--- NAIC STANDARD: SAME AS TRADITIONAL ---
                   IF WS-COB-OUR-BENEFIT >
                      WS-COB-BALANCE-AFTER-PRI
                       MOVE WS-COB-BALANCE-AFTER-PRI
                         TO WS-COB-SECONDARY-PAY
                   ELSE
                       MOVE WS-COB-OUR-BENEFIT
                         TO WS-COB-SECONDARY-PAY
                   END-IF

               WHEN OTHER
      *--- DEFAULT TO TRADITIONAL ---
                   IF WS-COB-OUR-BENEFIT >
                      WS-COB-BALANCE-AFTER-PRI
                       MOVE WS-COB-BALANCE-AFTER-PRI
                         TO WS-COB-SECONDARY-PAY
                   ELSE
                       MOVE WS-COB-OUR-BENEFIT
                         TO WS-COB-SECONDARY-PAY
                   END-IF
           END-EVALUATE

      *--- TOTAL OF PRIMARY + SECONDARY CANNOT EXCEED BILLED ---
           COMPUTE WS-WRK-AMOUNT-1 =
               WS-COB-OTHER-PAID + WS-COB-SECONDARY-PAY
           IF WS-WRK-AMOUNT-1 > WS-DTL-BILLED-AMOUNT
               COMPUTE WS-COB-SECONDARY-PAY =
                   WS-DTL-BILLED-AMOUNT - WS-COB-OTHER-PAID
               IF WS-COB-SECONDARY-PAY < 0
                   MOVE +0 TO WS-COB-SECONDARY-PAY
               END-IF
           END-IF

      *--- CALCULATE SAVINGS ---
           COMPUTE WS-COB-SAVINGS =
               WS-COB-OUR-BENEFIT - WS-COB-SECONDARY-PAY

      *--- UPDATE CLAIM AMOUNTS ---
           MOVE WS-COB-SECONDARY-PAY TO WS-DTL-PAID-AMOUNT
           MOVE WS-COB-OTHER-PAID TO WS-DTL-COB-AMOUNT

      *--- RECALCULATE PATIENT RESPONSIBILITY ---
           COMPUTE WS-DTL-PATIENT-RESP =
               WS-DTL-BILLED-AMOUNT
             - WS-COB-OTHER-PAID
             - WS-COB-SECONDARY-PAY

           IF WS-DTL-PATIENT-RESP < 0
               MOVE +0 TO WS-DTL-PATIENT-RESP
           END-IF
           .
       5300-CALCULATE-COB-SECONDARY-EXIT.
           EXIT.

      *================================================================*
      * 5400 - MEDICARE SECONDARY PAYER                                *
      *================================================================*
       5400-MEDICARE-SECONDARY-PAYER.
      *---------------------------------------------------------------*
      * APPLY MEDICARE SECONDARY PAYER (MSP) RULES.                   *
      * CALCULATE MEDICARE ALLOWED AND APPLY MSP REDUCTION.           *
      *---------------------------------------------------------------*

      *--- ONLY APPLIES IF MEDICARE IS SECONDARY ---
           IF MSP-WORKING-AGED AND WS-MBR-EMPLOYER-SIZE >= 20
      *--- WE ARE PRIMARY, MEDICARE IS SECONDARY ---
      *--- NO ADJUSTMENT NEEDED ON OUR SIDE ---
               GO TO 5400-MEDICARE-SECONDARY-PAYER-EXIT
           END-IF

           IF MSP-DISABILITY AND WS-MBR-EMPLOYER-SIZE >= 100
      *--- WE ARE PRIMARY, MEDICARE IS SECONDARY ---
               GO TO 5400-MEDICARE-SECONDARY-PAYER-EXIT
           END-IF

           IF MSP-ESRD
      *--- DURING FIRST 30 MONTHS WE ARE PRIMARY ---
               COMPUTE WS-WRK-DAYS-1 =
                   (WS-CLM-FROM-DATE
                    - WS-MBR-ESRD-START-DATE) / 10000 * 365
               IF WS-WRK-DAYS-1 <=
                  (WS-WRK-ESRD-COORD-MONTHS * 30)
      *--- WITHIN COORDINATION PERIOD: WE ARE PRIMARY ---
                   GO TO 5400-MEDICARE-SECONDARY-PAYER-EXIT
               END-IF
           END-IF

      *--- WE ARE SECONDARY TO MEDICARE ---
      * LOOK UP MEDICARE ALLOWED AMOUNT FOR COMPARISON
           EXEC SQL
               SELECT MEDICARE_ALLOWED_AMOUNT
               INTO   :WS-WRK-AMOUNT-1
               FROM   MEDICARE_FEE_SCHEDULE
               WHERE  CPT_CODE = :HV-CPT-CODE
               AND    CBSA_CODE = :HV-CBSA-CODE
               AND    EFFECTIVE_DATE <= :WS-CLM-FROM-DATE
               AND    (TERMINATION_DATE IS NULL
                  OR   TERMINATION_DATE >= :WS-CLM-FROM-DATE)
           END-EXEC
           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-READS

           IF WS-SQLCODE = 0
      *--- APPLY MSP REDUCTION ---
      * AS SECONDARY, WE PAY THE LESSER OF:
      * 1. OUR CALCULATED BENEFIT
      * 2. BALANCE AFTER MEDICARE
      * 3. OUR CALCULATED BENEFIT REDUCED BY MEDICARE PAYMENT
               IF WS-COB-OTHER-PAID > 0
                   COMPUTE WS-WRK-AMOUNT-2 =
                       WS-DTL-BILLED-AMOUNT - WS-COB-OTHER-PAID
                   IF WS-DTL-PAID-AMOUNT > WS-WRK-AMOUNT-2
                       MOVE WS-WRK-AMOUNT-2
                         TO WS-DTL-PAID-AMOUNT
                   END-IF
                   IF WS-DTL-PAID-AMOUNT < 0
                       MOVE +0 TO WS-DTL-PAID-AMOUNT
                   END-IF
               END-IF
           END-IF
           .
       5400-MEDICARE-SECONDARY-PAYER-EXIT.
           EXIT.

      *================================================================*
      * 6000 - CALCULATE PROVIDER PAYMENT                              *
      *================================================================*
       6000-CALCULATE-PROVIDER-PAYMENT.
      *---------------------------------------------------------------*
      * CALCULATE FINAL NET PAYMENT TO PROVIDER INCLUDING              *
      * WITHHOLD, INTEREST, AND PENALTIES.                             *
      *---------------------------------------------------------------*
           MOVE WS-DTL-PAID-AMOUNT TO WS-PAY-GROSS-AMOUNT

      *--- CALCULATE WITHHOLD ---
           PERFORM 6100-CALCULATE-WITHHOLD
              THRU 6100-CALCULATE-WITHHOLD-EXIT

      *--- CALCULATE PROMPT PAY INTEREST ---
           PERFORM 6200-CALCULATE-INTEREST
              THRU 6200-CALCULATE-INTEREST-EXIT

      *--- CALCULATE PENALTIES ---
           PERFORM 6300-CALCULATE-PENALTIES
              THRU 6300-CALCULATE-PENALTIES-EXIT

      *--- CALCULATE NET PAYMENT ---
           PERFORM 6400-CALCULATE-NET-PAYMENT
              THRU 6400-CALCULATE-NET-PAYMENT-EXIT
           .
       6000-CALCULATE-PROVIDER-PAYMENT-EXIT.
           EXIT.

      *================================================================*
      * 6100 - CALCULATE WITHHOLD                                      *
      *================================================================*
       6100-CALCULATE-WITHHOLD.
      *---------------------------------------------------------------*
      * APPLY PROVIDER WITHHOLD PERCENTAGE PER CONTRACT.               *
      * WITHHOLDS ARE HELD FOR YEAR-END PERFORMANCE RECONCILIATION.    *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-PAY-WITHHOLD-AMT

           IF WS-PROV-WITHHOLD-PCT > 0
              AND WS-PAY-GROSS-AMOUNT > 0
               COMPUTE WS-PAY-WITHHOLD-AMT ROUNDED =
                   WS-PAY-GROSS-AMOUNT * WS-PROV-WITHHOLD-PCT / 100

      *--- TRACK WITHHOLD FOR RECONCILIATION ---
               EXEC SQL
                   UPDATE PROVIDER_WITHHOLD_FUND
                   SET    WITHHOLD_AMOUNT =
                          WITHHOLD_AMOUNT + :WS-PAY-WITHHOLD-AMT,
                          CLAIM_COUNT = CLAIM_COUNT + 1,
                          LAST_UPDATE_DATE = :WS-BATCH-RUN-DATE
                   WHERE  NPI = :HV-PROVIDER-NPI
                   AND    CONTRACT_ID = :HV-CONTRACT-ID
                   AND    FUND_YEAR = :WS-CURRENT-YEAR
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE

               IF WS-SQLCODE = +100
      *--- NO FUND RECORD: INSERT NEW ---
                   EXEC SQL
                       INSERT INTO PROVIDER_WITHHOLD_FUND
                       (NPI, CONTRACT_ID, FUND_YEAR,
                        WITHHOLD_AMOUNT, CLAIM_COUNT,
                        LAST_UPDATE_DATE, STATUS)
                       VALUES
                       (:HV-PROVIDER-NPI, :HV-CONTRACT-ID,
                        :WS-CURRENT-YEAR,
                        :WS-PAY-WITHHOLD-AMT, 1,
                        :WS-BATCH-RUN-DATE, 'ACTIVE')
                   END-EXEC
                   ADD 1 TO WS-CTR-DB-INSERTS
               ELSE
                   ADD 1 TO WS-CTR-DB-UPDATES
               END-IF
           END-IF
           .
       6100-CALCULATE-WITHHOLD-EXIT.
           EXIT.

      *================================================================*
      * 6200 - CALCULATE INTEREST                                      *
      *================================================================*
       6200-CALCULATE-INTEREST.
      *---------------------------------------------------------------*
      * CALCULATE PROMPT PAYMENT INTEREST IF CLAIM PAYMENT IS LATE.   *
      * CHECK BOTH FEDERAL AND STATE PROMPT PAY REQUIREMENTS.          *
      * USE THE MORE STRINGENT (SHORTER DAYS / HIGHER RATE).           *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-PAY-INTEREST-AMT

      *--- DETERMINE CLEAN CLAIM DATE ---
           IF IS-CLEAN-CLAIM
               MOVE WS-CLM-RECEIVED-DATE TO WS-PAY-CLEAN-CLAIM-DATE
           ELSE
      *--- UNCLEAN CLAIM: USE DATE WHEN CLEAN INFORMATION RECEIVED ---
               MOVE WS-BATCH-RUN-DATE TO WS-PAY-CLEAN-CLAIM-DATE
           END-IF

      *--- CALCULATE PAYMENT DATE (TODAY) ---
           MOVE WS-BATCH-RUN-DATE TO WS-PAY-PAYMENT-DATE

      *--- CALCULATE DAYS FROM CLEAN CLAIM TO PAYMENT ---
           COMPUTE WS-PAY-DAYS-TO-PAY =
               (WS-PAY-PAYMENT-DATE - WS-PAY-CLEAN-CLAIM-DATE)
               / 10000 * 365

      *--- FEDERAL PROMPT PAY: 30 DAYS FOR CLEAN CLAIMS ---
           MOVE WS-WRK-FED-PROMPT-DAYS TO WS-PAY-PROMPT-PAY-DAYS
           MOVE WS-WRK-FED-INT-RATE TO WS-PAY-INTEREST-RATE

      *--- CHECK STATE-SPECIFIC PROMPT PAY REQUIREMENTS ---
           PERFORM VARYING WS-WRK-INDEX-1 FROM 1 BY 1
               UNTIL WS-WRK-INDEX-1 > 52
                  OR WS-SPP-STATE-CODE(WS-WRK-INDEX-1) =
                     WS-PROV-STATE
               CONTINUE
           END-PERFORM

           IF WS-WRK-INDEX-1 <= 52
      *--- FOUND STATE-SPECIFIC RULES ---
               IF IS-CLEAN-CLAIM
                   MOVE WS-SPP-CLEAN-DAYS(WS-WRK-INDEX-1)
                     TO WS-PAY-STATE-PROMPT-DAYS
               ELSE
                   MOVE WS-SPP-UNCLEAN-DAYS(WS-WRK-INDEX-1)
                     TO WS-PAY-STATE-PROMPT-DAYS
               END-IF
               MOVE WS-SPP-INT-RATE(WS-WRK-INDEX-1)
                 TO WS-PAY-STATE-INT-RATE

      *--- USE THE MORE STRINGENT REQUIREMENT ---
               IF WS-PAY-STATE-PROMPT-DAYS <
                  WS-PAY-PROMPT-PAY-DAYS
                   MOVE WS-PAY-STATE-PROMPT-DAYS
                     TO WS-PAY-PROMPT-PAY-DAYS
               END-IF
               IF WS-PAY-STATE-INT-RATE >
                  WS-PAY-INTEREST-RATE
                   MOVE WS-PAY-STATE-INT-RATE
                     TO WS-PAY-INTEREST-RATE
               END-IF
           END-IF

      *--- CALCULATE INTEREST IF LATE ---
           IF WS-PAY-DAYS-TO-PAY > WS-PAY-PROMPT-PAY-DAYS
               COMPUTE WS-PAY-LATE-DAYS =
                   WS-PAY-DAYS-TO-PAY - WS-PAY-PROMPT-PAY-DAYS

      *--- INTEREST = GROSS PAYMENT * RATE * LATE DAYS / 365 ---
               COMPUTE WS-PAY-INTEREST-AMT ROUNDED =
                   WS-PAY-GROSS-AMOUNT
                   * WS-PAY-INTEREST-RATE
                   * WS-PAY-LATE-DAYS / 365
           END-IF
           .
       6200-CALCULATE-INTEREST-EXIT.
           EXIT.

      *================================================================*
      * 6300 - CALCULATE PENALTIES                                     *
      *================================================================*
       6300-CALCULATE-PENALTIES.
      *---------------------------------------------------------------*
      * CALCULATE LATE PAYMENT PENALTIES PER STATE REGULATIONS.        *
      * SOME STATES IMPOSE FLAT PENALTIES IN ADDITION TO INTEREST.     *
      *---------------------------------------------------------------*
           MOVE +0 TO WS-PAY-PENALTY-AMT

      *--- CHECK IF STATE HAS PENALTY PROVISIONS ---
           IF WS-PAY-DAYS-TO-PAY > WS-PAY-PROMPT-PAY-DAYS
               PERFORM VARYING WS-WRK-INDEX-1 FROM 1 BY 1
                   UNTIL WS-WRK-INDEX-1 > 52
                      OR WS-SPP-STATE-CODE(WS-WRK-INDEX-1) =
                         WS-PROV-STATE
                   CONTINUE
               END-PERFORM

               IF WS-WRK-INDEX-1 <= 52
                   IF WS-SPP-PENALTY-PCT(WS-WRK-INDEX-1) > 0
      *--- PENALTY = PERCENTAGE OF GROSS PAYMENT ---
                       COMPUTE WS-PAY-PENALTY-AMT ROUNDED =
                           WS-PAY-GROSS-AMOUNT
                           * WS-SPP-PENALTY-PCT(WS-WRK-INDEX-1)
                   END-IF
               END-IF

      *--- SPECIAL HANDLING FOR SPECIFIC STATES ---
               EVALUATE WS-PROV-STATE
                   WHEN 'CA'
      *--- CALIFORNIA: $15 PER DAY LATE PENALTY ---
                       COMPUTE WS-WRK-AMOUNT-1 =
                           WS-PAY-LATE-DAYS * 15.00
                       IF WS-WRK-AMOUNT-1 > WS-PAY-PENALTY-AMT
                           MOVE WS-WRK-AMOUNT-1
                             TO WS-PAY-PENALTY-AMT
                       END-IF
                   WHEN 'NY'
      *--- NEW YORK: 2% PER MONTH LATE ---
                       COMPUTE WS-WRK-AMOUNT-1 ROUNDED =
                           WS-PAY-GROSS-AMOUNT * 0.02
                           * (WS-PAY-LATE-DAYS / 30)
                       IF WS-WRK-AMOUNT-1 > WS-PAY-PENALTY-AMT
                           MOVE WS-WRK-AMOUNT-1
                             TO WS-PAY-PENALTY-AMT
                       END-IF
                   WHEN 'NJ'
      *--- NEW JERSEY: TIERED PENALTY BASED ON DAYS LATE ---
                       IF WS-PAY-LATE-DAYS > 60
                           COMPUTE WS-PAY-PENALTY-AMT ROUNDED =
                               WS-PAY-GROSS-AMOUNT * 0.03
                       ELSE IF WS-PAY-LATE-DAYS > 30
                           COMPUTE WS-PAY-PENALTY-AMT ROUNDED =
                               WS-PAY-GROSS-AMOUNT * 0.02
                       ELSE
                           COMPUTE WS-PAY-PENALTY-AMT ROUNDED =
                               WS-PAY-GROSS-AMOUNT * 0.01
                       END-IF
                   WHEN 'TX'
      *--- TEXAS: 18% ANNUAL PENALTY RATE ---
                       COMPUTE WS-PAY-PENALTY-AMT ROUNDED =
                           WS-PAY-GROSS-AMOUNT * 0.18
                           * WS-PAY-LATE-DAYS / 365
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           .
       6300-CALCULATE-PENALTIES-EXIT.
           EXIT.

      *================================================================*
      * 6400 - CALCULATE NET PAYMENT                                   *
      *================================================================*
       6400-CALCULATE-NET-PAYMENT.
      *---------------------------------------------------------------*
      * CALCULATE FINAL NET PAYMENT:                                   *
      *   NET = GROSS - WITHHOLD + INTEREST + PENALTY                  *
      * APPLY MINIMUM THRESHOLD AND HANDLE OVERPAYMENTS.               *
      *---------------------------------------------------------------*

      *--- CALCULATE NET ---
           COMPUTE WS-PAY-NET-AMOUNT =
               WS-PAY-GROSS-AMOUNT
             - WS-PAY-WITHHOLD-AMT
             + WS-PAY-INTEREST-AMT
             + WS-PAY-PENALTY-AMT

      *--- CHECK MINIMUM PAYMENT THRESHOLD ---
           IF WS-PAY-NET-AMOUNT > 0
              AND WS-PAY-NET-AMOUNT < WS-PAY-MINIMUM-THRESHOLD
      *--- BELOW MINIMUM: ROUND UP TO MINIMUM ---
               MOVE WS-PAY-MINIMUM-THRESHOLD
                 TO WS-PAY-NET-AMOUNT
           END-IF

      *--- HANDLE NEGATIVE BALANCE (OVERPAYMENT) ---
           IF WS-PAY-NET-AMOUNT < 0
               MOVE WS-PAY-NET-AMOUNT TO WS-PAY-OVERPAYMENT-AMT
      *--- CHECK IF PROVIDER HAS OUTSTANDING BALANCE ---
               EXEC SQL
                   SELECT SUM(OVERPAYMENT_BALANCE)
                   INTO   :WS-WRK-AMOUNT-1
                   FROM   PROVIDER_OVERPAYMENT
                   WHERE  NPI = :HV-PROVIDER-NPI
                   AND    STATUS = 'OUTSTANDING'
               END-EXEC
               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-READS

               IF WS-SQLCODE = 0
      *--- ADD TO EXISTING OVERPAYMENT TRACKING ---
                   EXEC SQL
                       INSERT INTO PROVIDER_OVERPAYMENT
                       (NPI, CLAIM_ID, OVERPAYMENT_AMOUNT,
                        OVERPAYMENT_BALANCE, OVERPAYMENT_DATE,
                        STATUS)
                       VALUES
                       (:HV-PROVIDER-NPI, :HV-CLAIM-ID,
                        :WS-PAY-OVERPAYMENT-AMT,
                        :WS-PAY-OVERPAYMENT-AMT,
                        :WS-BATCH-RUN-DATE, 'OUTSTANDING')
                   END-EXEC
                   ADD 1 TO WS-CTR-DB-INSERTS
               END-IF
      *--- SET NET PAYMENT TO ZERO FOR THIS CLAIM ---
               MOVE +0 TO WS-PAY-NET-AMOUNT
           END-IF

      *--- CALCULATE FINAL PATIENT RESPONSIBILITY ---
           COMPUTE WS-PAY-PATIENT-RESP =
               WS-DTL-BILLED-AMOUNT - WS-PAY-GROSS-AMOUNT
             - WS-DTL-COB-AMOUNT

      *--- PATIENT RESP CANNOT BE NEGATIVE ---
           IF WS-PAY-PATIENT-RESP < 0
               MOVE +0 TO WS-PAY-PATIENT-RESP
           END-IF

      *--- ADD COST SHARING TO PATIENT RESPONSIBILITY ---
           ADD WS-DTL-DEDUCTIBLE-AMT TO WS-PAY-PATIENT-RESP
           ADD WS-DTL-COPAY-AMOUNT TO WS-PAY-PATIENT-RESP
           ADD WS-DTL-COINSURANCE-AMT TO WS-PAY-PATIENT-RESP
           .
       6400-CALCULATE-NET-PAYMENT-EXIT.
           EXIT.

      *================================================================*
      * 7000 - PEND CLAIM PROCESSING                                   *
      *================================================================*
       7000-PEND-CLAIM-PROCESSING.
      *---------------------------------------------------------------*
      * ROUTE PENDED CLAIMS TO THE APPROPRIATE REVIEW QUEUE.          *
      * SET EXPECTED RESOLUTION DATE BASED ON PEND TYPE.               *
      * INSERT INTO PEND_QUEUE TABLE FOR MANUAL PROCESSING.            *
      *---------------------------------------------------------------*
           SET CLM-STATUS-PENDED TO TRUE

      *--- SET EXPECTED RESOLUTION DATE BASED ON QUEUE ---
           EVALUATE TRUE
               WHEN PEND-MEDICAL-REVIEW
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 140000
                   SET PEND-PRIORITY-MEDIUM TO TRUE
               WHEN PEND-PRICING-REVIEW
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 70000
                   SET PEND-PRIORITY-HIGH TO TRUE
               WHEN PEND-COB-REVIEW
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 210000
                   SET PEND-PRIORITY-LOW TO TRUE
               WHEN PEND-AUTH-REVIEW
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 70000
                   SET PEND-PRIORITY-HIGH TO TRUE
               WHEN PEND-MANAGEMENT
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 300000
                   SET PEND-PRIORITY-LOW TO TRUE
               WHEN PEND-SPECIAL-INVEST
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 600000
                   SET PEND-PRIORITY-HIGH TO TRUE
               WHEN OTHER
                   COMPUTE WS-PEND-EXPECTED-DATE =
                       WS-BATCH-RUN-DATE + 140000
                   SET PEND-PRIORITY-MEDIUM TO TRUE
           END-EVALUATE

      *--- INSERT INTO PEND QUEUE ---
           MOVE WS-PEND-REASON-CODE TO HV-PEND-REASON
           MOVE WS-PEND-QUEUE TO HV-PEND-QUEUE

           EXEC SQL
               INSERT INTO PEND_QUEUE
               (CLAIM_ID, MEMBER_ID, PROVIDER_NPI,
                PEND_REASON_CODE, PEND_QUEUE,
                PEND_DATE, EXPECTED_RESOLUTION_DATE,
                PRIORITY, TOTAL_BILLED,
                PEND_DESCRIPTION, STATUS,
                ASSIGNED_TO, CREATE_DATE)
               VALUES
               (:HV-CLAIM-ID, :HV-MEMBER-ID,
                :HV-PROVIDER-NPI,
                :HV-PEND-REASON, :HV-PEND-QUEUE,
                :WS-BATCH-RUN-DATE, :WS-PEND-EXPECTED-DATE,
                :WS-PEND-PRIORITY, :WS-CLM-TOTAL-CHARGE,
                :WS-PEND-REASON-DESC, 'OPEN',
                'UNASSIGNED', :WS-BATCH-RUN-DATE)
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-INSERTS

           IF WS-SQLCODE NOT = 0
               MOVE 'PEND QUEUE INSERT FAILED'
                 TO WS-ERR-MESSAGE
               MOVE WS-SQLCODE TO WS-ERR-SQLCODE
               MOVE '7000-PEND-CLAIM-PROCESSING'
                 TO WS-ERR-PARAGRAPH
               SET ERR-SEVERE TO TRUE
               PERFORM 8900-DATABASE-ERROR
                  THRU 8900-DATABASE-ERROR-EXIT
           END-IF

           ADD 1 TO WS-PEND-COUNT-TOTAL
           .
       7000-PEND-CLAIM-PROCESSING-EXIT.
           EXIT.

      *================================================================*
      * 8000 - UPDATE DATABASE                                         *
      *================================================================*
       8000-UPDATE-DATABASE.
      *---------------------------------------------------------------*
      * UPDATE ALL DATABASE TABLES WITHIN A TRANSACTION.               *
      * INCLUDES CLAIM_HEADER, CLAIM_DETAIL, BENEFIT_ACCUMULATORS,     *
      * CLAIM_AUDIT_LOG, AND PAYMENT_HISTORY.                          *
      *---------------------------------------------------------------*

      *--- BEGIN TRANSACTION ---
           EXEC SQL
               BEGIN TRANSACTION
           END-EXEC

           MOVE 0 TO WS-SQL-RETRY-COUNT
           SET NO-DEADLOCK TO TRUE

       8000-RETRY-POINT.

      *--- UPDATE CLAIM HEADER ---
           MOVE WS-CLM-STATUS TO HV-CLAIM-STATUS
           MOVE WS-CLM-ID TO HV-CLAIM-ID

           EXEC SQL
               UPDATE CLAIM_HEADER
               SET    CLAIM_STATUS = :HV-CLAIM-STATUS,
                      ALLOWED_AMOUNT = :WS-DTL-ALLOWED-AMOUNT,
                      DEDUCTIBLE_AMOUNT = :WS-DTL-DEDUCTIBLE-AMT,
                      COPAY_AMOUNT = :WS-DTL-COPAY-AMOUNT,
                      COINSURANCE_AMOUNT = :WS-DTL-COINSURANCE-AMT,
                      COB_AMOUNT = :WS-DTL-COB-AMOUNT,
                      PAID_AMOUNT = :WS-DTL-PAID-AMOUNT,
                      WITHHOLD_AMOUNT = :WS-PAY-WITHHOLD-AMT,
                      INTEREST_AMOUNT = :WS-PAY-INTEREST-AMT,
                      NET_PAYMENT = :WS-PAY-NET-AMOUNT,
                      PATIENT_RESPONSIBILITY = :WS-PAY-PATIENT-RESP,
                      PRICING_METHOD = :WS-DTL-PRICING-METHOD,
                      COB_ORDER = :WS-COB-ORDER,
                      ADJUDICATION_DATE = :WS-BATCH-RUN-DATE,
                      LAST_UPDATE_DATE = :WS-BATCH-RUN-DATE,
                      LAST_UPDATE_USER = 'HCCLMADJ'
               WHERE  CLAIM_ID = :HV-CLAIM-ID
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           ADD 1 TO WS-CTR-DB-UPDATES

           IF WS-SQLCODE = WS-SQL-DEADLOCK-CODE
              OR WS-SQLCODE = WS-SQL-TIMEOUT-CODE
               PERFORM 8010-HANDLE-DEADLOCK
                  THRU 8010-HANDLE-DEADLOCK-EXIT
               IF NO-DEADLOCK
                   GO TO 8000-RETRY-POINT
               ELSE
                   GO TO 8000-ROLLBACK
               END-IF
           END-IF

           IF WS-SQLCODE NOT = 0
               MOVE 'CLAIM HEADER UPDATE FAILED'
                 TO WS-ERR-MESSAGE
               MOVE WS-SQLCODE TO WS-ERR-SQLCODE
               MOVE '8000-UPDATE-DATABASE' TO WS-ERR-PARAGRAPH
               SET ERR-SEVERE TO TRUE
               PERFORM 8900-DATABASE-ERROR
                  THRU 8900-DATABASE-ERROR-EXIT
               GO TO 8000-ROLLBACK
           END-IF

      *--- UPDATE BENEFIT ACCUMULATORS ---
           IF WS-DTL-DEDUCTIBLE-AMT > 0
              OR WS-DTL-COPAY-AMOUNT > 0
              OR WS-DTL-COINSURANCE-AMT > 0
              OR WS-DTL-PAID-AMOUNT > 0

               EXEC SQL
                   UPDATE BENEFIT_ACCUMULATORS
                   SET    INN_DEDUCTIBLE_USED =
                          INN_DEDUCTIBLE_USED
                          + :WS-DTL-DEDUCTIBLE-AMT,
                          FAM_DEDUCTIBLE_USED =
                          FAM_DEDUCTIBLE_USED
                          + :WS-DTL-DEDUCTIBLE-AMT,
                          INN_OOP_USED =
                          INN_OOP_USED
                          + :WS-DTL-DEDUCTIBLE-AMT
                          + :WS-DTL-COPAY-AMOUNT
                          + :WS-DTL-COINSURANCE-AMT,
                          FAM_OOP_USED =
                          FAM_OOP_USED
                          + :WS-DTL-DEDUCTIBLE-AMT
                          + :WS-DTL-COPAY-AMOUNT
                          + :WS-DTL-COINSURANCE-AMT,
                          ANNUAL_USED =
                          ANNUAL_USED + :WS-DTL-PAID-AMOUNT,
                          LIFETIME_USED =
                          LIFETIME_USED + :WS-DTL-PAID-AMOUNT,
                          RESERVED_AMOUNT =
                          RESERVED_AMOUNT
                          - :WS-ACC-RESERVED-AMT,
                          LAST_UPDATE_TIMESTAMP = GETDATE()
                   WHERE  MEMBER_ID = :HV-MEMBER-ID
                   AND    PLAN_YEAR = :HV-PLAN-YEAR
                   AND    PLAN_CODE = :WS-MBR-PLAN-CODE
               END-EXEC

               MOVE SQLCODE TO WS-SQLCODE
               ADD 1 TO WS-CTR-DB-UPDATES

               IF WS-SQLCODE = +100
      *--- NO ACCUMULATOR ROW: INSERT NEW ---
                   EXEC SQL
                       INSERT INTO BENEFIT_ACCUMULATORS
                       (MEMBER_ID, PLAN_YEAR, PLAN_CODE,
                        INN_DEDUCTIBLE_USED, FAM_DEDUCTIBLE_USED,
                        INN_OOP_USED, FAM_OOP_USED,
                        ANNUAL_USED, LIFETIME_USED,
                        RESERVED_AMOUNT,
                        LAST_UPDATE_TIMESTAMP)
                       VALUES
                       (:HV-MEMBER-ID, :HV-PLAN-YEAR,
                        :WS-MBR-PLAN-CODE,
                        :WS-DTL-DEDUCTIBLE-AMT,
                        :WS-DTL-DEDUCTIBLE-AMT,
                        :WS-DTL-DEDUCTIBLE-AMT
                        + :WS-DTL-COPAY-AMOUNT
                        + :WS-DTL-COINSURANCE-AMT,
                        :WS-DTL-DEDUCTIBLE-AMT
                        + :WS-DTL-COPAY-AMOUNT
                        + :WS-DTL-COINSURANCE-AMT,
                        :WS-DTL-PAID-AMOUNT,
                        :WS-DTL-PAID-AMOUNT,
                        0,
                        GETDATE())
                   END-EXEC
                   ADD 1 TO WS-CTR-DB-INSERTS
               ELSE
                   IF WS-SQLCODE NOT = 0
                       MOVE 'ACCUMULATOR UPDATE FAILED'
                         TO WS-ERR-MESSAGE
                       MOVE WS-SQLCODE TO WS-ERR-SQLCODE
                       PERFORM 8900-DATABASE-ERROR
                          THRU 8900-DATABASE-ERROR-EXIT
                       GO TO 8000-ROLLBACK
                   END-IF
               END-IF
           END-IF

      *--- INSERT AUDIT LOG ENTRY ---
           MOVE 'ADJ' TO HV-AUDIT-ACTION

           EXEC SQL
               INSERT INTO CLAIM_AUDIT_LOG
               (CLAIM_ID, AUDIT_ACTION, AUDIT_TIMESTAMP,
                USER_ID, PROGRAM_NAME,
                OLD_STATUS, NEW_STATUS,
                OLD_PAID_AMOUNT, NEW_PAID_AMOUNT,
                BATCH_RUN_DATE)
               VALUES
               (:HV-CLAIM-ID, :HV-AUDIT-ACTION, GETDATE(),
                'HCCLMADJ', 'HCCLMADJ',
                'RC', :HV-CLAIM-STATUS,
                0, :WS-PAY-NET-AMOUNT,
                :WS-BATCH-RUN-DATE)
           END-EXEC
           ADD 1 TO WS-CTR-DB-INSERTS

      *--- INSERT PAYMENT HISTORY ---
           IF WS-PAY-NET-AMOUNT > 0
               EXEC SQL
                   INSERT INTO PAYMENT_HISTORY
                   (CLAIM_ID, PROVIDER_NPI, PROVIDER_TAX_ID,
                    PAYMENT_DATE, GROSS_PAYMENT,
                    WITHHOLD_AMOUNT, INTEREST_AMOUNT,
                    PENALTY_AMOUNT, NET_PAYMENT,
                    PATIENT_RESPONSIBILITY,
                    PAYMENT_METHOD, PAYMENT_STATUS)
                   VALUES
                   (:HV-CLAIM-ID, :HV-PROVIDER-NPI,
                    :WS-PROV-TAX-ID,
                    :WS-BATCH-RUN-DATE,
                    :WS-PAY-GROSS-AMOUNT,
                    :WS-PAY-WITHHOLD-AMT,
                    :WS-PAY-INTEREST-AMT,
                    :WS-PAY-PENALTY-AMT,
                    :WS-PAY-NET-AMOUNT,
                    :WS-PAY-PATIENT-RESP,
                    'EFT', 'PENDING')
               END-EXEC
               ADD 1 TO WS-CTR-DB-INSERTS
           END-IF

      *--- COMMIT TRANSACTION ---
           EXEC SQL
               COMMIT TRANSACTION
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE NOT = 0
               MOVE 'COMMIT FAILED' TO WS-ERR-MESSAGE
               MOVE WS-SQLCODE TO WS-ERR-SQLCODE
               MOVE '8000-UPDATE-DATABASE' TO WS-ERR-PARAGRAPH
               SET ERR-SEVERE TO TRUE
               PERFORM 8900-DATABASE-ERROR
                  THRU 8900-DATABASE-ERROR-EXIT
           END-IF

           GO TO 8000-UPDATE-DATABASE-EXIT
           .
       8000-ROLLBACK.
      *--- ROLLBACK ON ERROR ---
           EXEC SQL
               ROLLBACK TRANSACTION
           END-EXEC
           MOVE 'TRANSACTION ROLLED BACK' TO WS-ERR-MESSAGE
           MOVE '8000-UPDATE-DATABASE' TO WS-ERR-PARAGRAPH
           SET ERR-SEVERE TO TRUE
           PERFORM 8800-ERROR-HANDLER
              THRU 8800-ERROR-HANDLER-EXIT
           .
       8000-UPDATE-DATABASE-EXIT.
           EXIT.

      *================================================================*
      * 8010 - HANDLE DEADLOCK                                         *
      *================================================================*
       8010-HANDLE-DEADLOCK.
      *---------------------------------------------------------------*
      * HANDLE DATABASE DEADLOCK WITH RETRY LOGIC.                     *
      * WAIT AND RETRY UP TO MAX-RETRIES TIMES.                        *
      *---------------------------------------------------------------*
           ADD 1 TO WS-SQL-RETRY-COUNT
           ADD 1 TO WS-CTR-DEADLOCK-RETRIES

           IF WS-SQL-RETRY-COUNT > WS-SQL-MAX-RETRIES
               SET DEADLOCK-OCCURRED TO TRUE
               MOVE 'MAX DEADLOCK RETRIES EXCEEDED'
                 TO WS-ERR-MESSAGE
               MOVE '8010-HANDLE-DEADLOCK' TO WS-ERR-PARAGRAPH
               SET ERR-SEVERE TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
           ELSE
               SET NO-DEADLOCK TO TRUE
               DISPLAY 'DEADLOCK RETRY ' WS-SQL-RETRY-COUNT
                       ' FOR CLAIM ' WS-CLM-ID

      *--- ROLLBACK CURRENT TRANSACTION ---
               EXEC SQL
                   ROLLBACK TRANSACTION
               END-EXEC

      *--- WAIT BEFORE RETRY (EXPONENTIAL BACKOFF) ---
      * FIRST RETRY: ~1 SECOND, SECOND: ~2 SECONDS, THIRD: ~4 SECONDS
               EXEC SQL
                   WAITFOR DELAY '00:00:01'
               END-EXEC

      *--- BEGIN NEW TRANSACTION ---
               EXEC SQL
                   BEGIN TRANSACTION
               END-EXEC
           END-IF
           .
       8010-HANDLE-DEADLOCK-EXIT.
           EXIT.

      *================================================================*
      * 8500 - WRITE OUTPUT FILES                                      *
      *================================================================*
       8500-WRITE-OUTPUT-FILES.
      *---------------------------------------------------------------*
      * WRITE ADJUDICATED CLAIM, PEND, AND PAYMENT RECORDS TO FILES. *
      *---------------------------------------------------------------*

      *--- WRITE ADJUDICATED CLAIM OUTPUT ---
           INITIALIZE WS-ADJ-OUTPUT-REC
           MOVE WS-CLM-ID TO WS-ADJ-CLAIM-ID
           MOVE WS-MBR-ID TO WS-ADJ-MEMBER-ID
           MOVE WS-PROV-NPI TO WS-ADJ-PROVIDER-NPI
           MOVE WS-CLM-STATUS TO WS-ADJ-STATUS
           MOVE WS-CLM-TOTAL-CHARGE TO WS-ADJ-TOTAL-BILLED
           MOVE WS-DTL-ALLOWED-AMOUNT TO WS-ADJ-TOTAL-ALLOWED
           MOVE WS-DTL-DEDUCTIBLE-AMT TO WS-ADJ-TOTAL-DEDUCTIBLE
           MOVE WS-DTL-COPAY-AMOUNT TO WS-ADJ-TOTAL-COPAY
           MOVE WS-DTL-COINSURANCE-AMT TO WS-ADJ-TOTAL-COINSURANCE
           MOVE WS-DTL-COB-AMOUNT TO WS-ADJ-TOTAL-COB
           MOVE WS-DTL-PAID-AMOUNT TO WS-ADJ-TOTAL-PAID
           MOVE WS-PAY-WITHHOLD-AMT TO WS-ADJ-TOTAL-WITHHOLD
           MOVE WS-PAY-INTEREST-AMT TO WS-ADJ-TOTAL-INTEREST
           MOVE WS-PAY-NET-AMOUNT TO WS-ADJ-NET-PAYMENT
           MOVE WS-PAY-PATIENT-RESP TO WS-ADJ-PATIENT-RESP
           MOVE WS-DTL-PRICING-METHOD TO WS-ADJ-PRICING-METHOD
           MOVE WS-COB-ORDER TO WS-ADJ-COB-ORDER
           MOVE WS-BATCH-RUN-DATE TO WS-ADJ-PROCESS-DATE
           MOVE WS-DTL-DENY-REASON TO WS-ADJ-DENY-REASON
           MOVE WS-CLM-LINE-COUNT TO WS-ADJ-LINE-COUNT

           WRITE ADJUDICATED-OUTPUT-RECORD FROM WS-ADJ-OUTPUT-REC
           IF NOT ADJOUT-OK
               MOVE 'ADJ OUTPUT WRITE FAILED' TO WS-ERR-MESSAGE
               MOVE WS-ADJOUT-STATUS TO WS-ERR-FILE-STATUS
               MOVE '8500-WRITE-OUTPUT-FILES' TO WS-ERR-PARAGRAPH
               SET ERR-SEVERE TO TRUE
               PERFORM 8800-ERROR-HANDLER
                  THRU 8800-ERROR-HANDLER-EXIT
           END-IF

      *--- WRITE PEND RECORD IF PENDED ---
           IF CLAIM-SHOULD-PEND
               INITIALIZE WS-PEND-OUTPUT-REC
               MOVE WS-CLM-ID TO WS-PND-CLAIM-ID
               MOVE WS-MBR-ID TO WS-PND-MEMBER-ID
               MOVE WS-PROV-NPI TO WS-PND-PROVIDER-NPI
               MOVE WS-PEND-REASON-CODE TO WS-PND-PEND-REASON
               MOVE WS-PEND-QUEUE TO WS-PND-PEND-QUEUE
               MOVE WS-BATCH-RUN-DATE TO WS-PND-PEND-DATE
               MOVE WS-PEND-EXPECTED-DATE TO WS-PND-EXPECTED-DATE
               MOVE WS-PEND-PRIORITY TO WS-PND-PRIORITY
               MOVE WS-CLM-TOTAL-CHARGE TO WS-PND-TOTAL-BILLED
               MOVE WS-PEND-REASON-DESC TO WS-PND-DESCRIPTION

               WRITE PEND-OUTPUT-RECORD FROM WS-PEND-OUTPUT-REC
               IF NOT PEND-OK
                   MOVE 'PEND FILE WRITE FAILED'
                     TO WS-ERR-MESSAGE
                   MOVE WS-PEND-STATUS TO WS-ERR-FILE-STATUS
                   SET ERR-WARNING TO TRUE
                   PERFORM 8800-ERROR-HANDLER
                      THRU 8800-ERROR-HANDLER-EXIT
               END-IF
           END-IF

      *--- WRITE PAYMENT RECORD IF PAID ---
           IF WS-PAY-NET-AMOUNT > 0
               INITIALIZE WS-PAY-OUTPUT-REC
               MOVE WS-CLM-ID TO WS-PYR-CLAIM-ID
               MOVE WS-PROV-NPI TO WS-PYR-PROVIDER-NPI
               MOVE WS-PROV-TAX-ID TO WS-PYR-PROVIDER-TAX-ID
               MOVE WS-BATCH-RUN-DATE TO WS-PYR-PAYMENT-DATE
               MOVE WS-PAY-GROSS-AMOUNT TO WS-PYR-GROSS-PAYMENT
               MOVE WS-PAY-WITHHOLD-AMT TO WS-PYR-WITHHOLD
               MOVE WS-PAY-INTEREST-AMT TO WS-PYR-INTEREST
               MOVE WS-PAY-NET-AMOUNT TO WS-PYR-NET-PAYMENT
               MOVE WS-PAY-PATIENT-RESP TO WS-PYR-PATIENT-RESP

               WRITE PAYMENT-OUTPUT-RECORD FROM WS-PAY-OUTPUT-REC
               IF NOT PAY-OK
                   MOVE 'PAYMENT FILE WRITE FAILED'
                     TO WS-ERR-MESSAGE
                   MOVE WS-PAY-STATUS TO WS-ERR-FILE-STATUS
                   SET ERR-WARNING TO TRUE
                   PERFORM 8800-ERROR-HANDLER
                      THRU 8800-ERROR-HANDLER-EXIT
               END-IF
           END-IF

      *--- WRITE AUDIT TRAIL ---
           INITIALIZE WS-AUDIT-OUTPUT-REC
           MOVE WS-CLM-ID TO WS-AUD-CLAIM-ID
           MOVE 'ADJ' TO WS-AUD-ACTION
           MOVE FUNCTION CURRENT-DATE TO WS-AUD-TIMESTAMP
           MOVE 'BATCH   ' TO WS-AUD-USER-ID
           MOVE '8500-WRITE-OUTPUT-FILES' TO WS-AUD-PARAGRAPH
           MOVE WS-CLM-STATUS TO WS-AUD-NEW-VALUE

           WRITE AUDIT-TRAIL-RECORD FROM WS-AUDIT-OUTPUT-REC
           .
       8500-WRITE-OUTPUT-FILES-EXIT.
           EXIT.

      *================================================================*
      * 8800 - ERROR HANDLER                                           *
      *================================================================*
       8800-ERROR-HANDLER.
      *---------------------------------------------------------------*
      * CENTRAL ERROR HANDLING ROUTINE. LOGS ERROR INFORMATION TO      *
      * ERROR FILE AND DISPLAY. DETERMINES APPROPRIATE ACTION BASED    *
      * ON SEVERITY.                                                   *
      *---------------------------------------------------------------*
           ADD 1 TO WS-ERR-COUNT

           DISPLAY '*** ERROR ***'
           DISPLAY '  CLAIM ID:  ' WS-CLM-ID
           DISPLAY '  PARAGRAPH: ' WS-ERR-PARAGRAPH
           DISPLAY '  MESSAGE:   ' WS-ERR-MESSAGE
           DISPLAY '  SEVERITY:  ' WS-ERR-SEVERITY
           IF WS-ERR-SQLCODE NOT = 0
               MOVE WS-ERR-SQLCODE TO WS-SQLCODE-DISP
               DISPLAY '  SQLCODE:   ' WS-SQLCODE-DISP
           END-IF
           IF WS-ERR-FILE-STATUS NOT = SPACES
               DISPLAY '  FILE STAT: ' WS-ERR-FILE-STATUS
           END-IF

      *--- WRITE ERROR RECORD ---
           INITIALIZE WS-ERR-OUTPUT-REC
           MOVE WS-CLM-ID TO WS-EOR-CLAIM-ID
           MOVE WS-PEND-REASON-CODE TO WS-EOR-ERROR-CODE
           MOVE WS-ERR-MESSAGE TO WS-EOR-ERROR-MSG
           MOVE WS-ERR-PARAGRAPH TO WS-EOR-PARAGRAPH
           MOVE WS-ERR-SQLCODE TO WS-EOR-SQLCODE
           MOVE FUNCTION CURRENT-DATE TO WS-EOR-TIMESTAMP

           WRITE ERROR-OUTPUT-RECORD FROM WS-ERR-OUTPUT-REC

      *--- DETERMINE ACTION ---
           EVALUATE TRUE
               WHEN ERR-WARNING
                   SET ERR-CONTINUE TO TRUE
               WHEN ERR-SEVERE
                   SET ERR-SKIP-CLAIM TO TRUE
               WHEN ERR-FATAL
                   SET ERR-ABORT TO TRUE
           END-EVALUATE

      *--- RESET ERROR FIELDS FOR NEXT ERROR ---
           MOVE SPACES TO WS-ERR-MESSAGE
           MOVE ZEROS TO WS-ERR-SQLCODE
           MOVE SPACES TO WS-ERR-FILE-STATUS
           .
       8800-ERROR-HANDLER-EXIT.
           EXIT.

      *================================================================*
      * 8900 - DATABASE ERROR                                          *
      *================================================================*
       8900-DATABASE-ERROR.
      *---------------------------------------------------------------*
      * HANDLE DATABASE-SPECIFIC ERRORS INCLUDING DEADLOCK DETECTION,  *
      * TIMEOUT HANDLING, AND TRANSACTION ROLLBACK.                    *
      *---------------------------------------------------------------*
           MOVE WS-SQLCODE TO WS-ERR-SQLCODE
           MOVE WS-SQLCODE TO WS-SQLCODE-DISP

           DISPLAY '*** DATABASE ERROR ***'
           DISPLAY '  CLAIM ID:  ' WS-CLM-ID
           DISPLAY '  PARAGRAPH: ' WS-ERR-PARAGRAPH
           DISPLAY '  MESSAGE:   ' WS-ERR-MESSAGE
           DISPLAY '  SQLCODE:   ' WS-SQLCODE-DISP

      *--- CHECK FOR DEADLOCK ---
           IF WS-SQLCODE = WS-SQL-DEADLOCK-CODE
               DISPLAY '  TYPE:      DEADLOCK DETECTED'
               SET DEADLOCK-OCCURRED TO TRUE
               GO TO 8900-DATABASE-ERROR-EXIT
           END-IF

      *--- CHECK FOR TIMEOUT ---
           IF WS-SQLCODE = WS-SQL-TIMEOUT-CODE
               DISPLAY '  TYPE:      LOCK TIMEOUT'
               SET DEADLOCK-OCCURRED TO TRUE
               GO TO 8900-DATABASE-ERROR-EXIT
           END-IF

      *--- CHECK FOR CONNECTION LOST ---
           IF WS-SQLCODE = -908 OR WS-SQLCODE = -921
               DISPLAY '  TYPE:      CONNECTION LOST'
               DISPLAY '  ATTEMPTING RECONNECT...'

               EXEC SQL
                   CONNECT TO CLMPROCDB
                   USER 'clmbatch' IDENTIFIED BY 'clmb@tch94'
               END-EXEC

               IF SQLCODE = 0
                   DISPLAY '  RECONNECT SUCCESSFUL'
                   SET DB-IS-CONNECTED TO TRUE
               ELSE
                   DISPLAY '  RECONNECT FAILED - SQLCODE: ' SQLCODE
                   SET DB-NOT-CONNECTED TO TRUE
                   SET ERR-FATAL TO TRUE
               END-IF
               GO TO 8900-DATABASE-ERROR-EXIT
           END-IF

      *--- CHECK FOR NOT FOUND (NOT AN ERROR) ---
           IF WS-SQLCODE = WS-SQL-NOT-FOUND-CODE
               DISPLAY '  TYPE:      ROW NOT FOUND'
               GO TO 8900-DATABASE-ERROR-EXIT
           END-IF

      *--- ALL OTHER DATABASE ERRORS ---
           DISPLAY '  TYPE:      GENERAL SQL ERROR'

      *--- WRITE ERROR TO ERROR FILE ---
           INITIALIZE WS-ERR-OUTPUT-REC
           MOVE WS-CLM-ID TO WS-EOR-CLAIM-ID
           MOVE 'SQL  ' TO WS-EOR-ERROR-CODE
           MOVE WS-ERR-MESSAGE TO WS-EOR-ERROR-MSG
           MOVE WS-ERR-PARAGRAPH TO WS-EOR-PARAGRAPH
           MOVE WS-SQLCODE TO WS-EOR-SQLCODE
           MOVE FUNCTION CURRENT-DATE TO WS-EOR-TIMESTAMP

           WRITE ERROR-OUTPUT-RECORD FROM WS-ERR-OUTPUT-REC
           .
       8900-DATABASE-ERROR-EXIT.
           EXIT.

      *================================================================*
      * 9000 - TERMINATION                                             *
      *================================================================*
       9000-TERMINATION.
      *---------------------------------------------------------------*
      * CLOSE ALL FILES, DISCONNECT FROM DATABASE, DISPLAY BATCH       *
      * PROCESSING STATISTICS, AND WRITE FINAL REPORT.                 *
      *---------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
           MOVE WS-CURRENT-TIME TO WS-PROCESS-END-TIME

      *--- CALCULATE ELAPSED TIME ---
           COMPUTE WS-ELAPSED-SECONDS =
               (WS-PROCESS-END-TIME - WS-PROCESS-START-TIME)

      *--- DISPLAY FINAL STATISTICS ---
           DISPLAY '================================================'
           DISPLAY 'HCCLMADJ - BATCH PROCESSING COMPLETE'
           DISPLAY '================================================'
           DISPLAY 'CLAIMS READ:            ' WS-CTR-CLAIMS-READ
           DISPLAY 'CLAIMS PROCESSED:       ' WS-CTR-CLAIMS-PROCESSED
           DISPLAY 'CLAIMS PAID:            ' WS-CTR-CLAIMS-PAID
           DISPLAY 'CLAIMS DENIED:          ' WS-CTR-CLAIMS-DENIED
           DISPLAY 'CLAIMS PENDED:          ' WS-CTR-CLAIMS-PENDED
           DISPLAY 'CLAIMS ZERO PAY:        ' WS-CTR-CLAIMS-ZERO-PAY
           DISPLAY 'CLAIMS ERROR:           ' WS-CTR-CLAIMS-ERROR
           DISPLAY '------------------------------------------------'
           DISPLAY 'PROFESSIONAL CLAIMS:    ' WS-CTR-PROFESSIONAL
           DISPLAY 'INSTITUTIONAL CLAIMS:   ' WS-CTR-INSTITUTIONAL
           DISPLAY 'COB CLAIMS:             ' WS-CTR-COB-CLAIMS
           DISPLAY 'CAPITATED ENCOUNTERS:   ' WS-CTR-CAPITATED
           DISPLAY '------------------------------------------------'
           DISPLAY 'TOTAL BILLED:           ' WS-TOT-BILLED-AMOUNT
           DISPLAY 'TOTAL ALLOWED:          ' WS-TOT-ALLOWED-AMOUNT
           DISPLAY 'TOTAL DEDUCTIBLE:       ' WS-TOT-DEDUCTIBLE-AMOUNT
           DISPLAY 'TOTAL COPAY:            ' WS-TOT-COPAY-AMOUNT
           DISPLAY 'TOTAL COINSURANCE:      '
                   WS-TOT-COINSURANCE-AMOUNT
           DISPLAY 'TOTAL PAID:             ' WS-TOT-PAID-AMOUNT
           DISPLAY 'TOTAL WITHHOLD:         ' WS-TOT-WITHHOLD-AMOUNT
           DISPLAY 'TOTAL INTEREST:         ' WS-TOT-INTEREST-AMOUNT
           DISPLAY 'TOTAL PATIENT RESP:     ' WS-TOT-PATIENT-RESP
           DISPLAY 'TOTAL COB SAVINGS:      ' WS-TOT-COB-SAVINGS
           DISPLAY '------------------------------------------------'
           DISPLAY 'DB READS:               ' WS-CTR-DB-READS
           DISPLAY 'DB UPDATES:             ' WS-CTR-DB-UPDATES
           DISPLAY 'DB INSERTS:             ' WS-CTR-DB-INSERTS
           DISPLAY 'DEADLOCK RETRIES:       ' WS-CTR-DEADLOCK-RETRIES
           DISPLAY 'ERROR COUNT:            ' WS-ERR-COUNT
           DISPLAY '------------------------------------------------'
           DISPLAY 'START TIME:             ' WS-PROCESS-START-TIME
           DISPLAY 'END TIME:               ' WS-PROCESS-END-TIME
           DISPLAY 'ELAPSED SECONDS:        ' WS-ELAPSED-SECONDS
           DISPLAY '================================================'

      *--- WRITE FINAL REPORT ---
           MOVE 'CLAIMS READ' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-READ TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 2 LINES

           MOVE 'CLAIMS PROCESSED' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-PROCESSED TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'CLAIMS PAID' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-PAID TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'CLAIMS DENIED' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-DENIED TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'CLAIMS PENDED' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-PENDED TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'CLAIMS ZERO PAY' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-ZERO-PAY TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'CLAIMS IN ERROR' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CLAIMS-ERROR TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL BILLED AMOUNT' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-BILLED-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL ALLOWED AMOUNT' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-ALLOWED-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL DEDUCTIBLE' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-DEDUCTIBLE-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL COPAY' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-COPAY-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL COINSURANCE' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-COINSURANCE-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL PAID' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-PAID-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL WITHHOLD' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-WITHHOLD-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL INTEREST' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-INTEREST-AMOUNT TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL PATIENT RESPONSIBILITY' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-PATIENT-RESP TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'TOTAL COB SAVINGS' TO WS-RPT-DL-LABEL
           MOVE WS-TOT-COB-SAVINGS TO WS-RPT-DL-AMOUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-DOLLAR-LINE
               AFTER ADVANCING 1 LINE

           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR
               AFTER ADVANCING 1 LINE

           MOVE 'PROFESSIONAL CLAIMS' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-PROFESSIONAL TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'INSTITUTIONAL CLAIMS' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-INSTITUTIONAL TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'COB CLAIMS' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-COB-CLAIMS TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'CAPITATED ENCOUNTERS' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-CAPITATED TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'DB READS' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-DB-READS TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'DB UPDATES' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-DB-UPDATES TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'DB INSERTS' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-DB-INSERTS TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'DEADLOCK RETRIES' TO WS-RPT-CL-LABEL
           MOVE WS-CTR-DEADLOCK-RETRIES TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

           MOVE 'ERRORS ENCOUNTERED' TO WS-RPT-CL-LABEL
           MOVE WS-ERR-COUNT TO WS-RPT-CL-COUNT
           WRITE REPORT-OUTPUT-RECORD FROM WS-RPT-COUNT-LINE
               AFTER ADVANCING 1 LINE

      *--- CLOSE ALL FILES ---
           CLOSE CLAIM-INPUT-FILE
           CLOSE REFERENCE-DATA-FILE
           CLOSE ADJUDICATED-OUTPUT-FILE
           CLOSE PEND-FILE
           CLOSE APPEAL-FILE
           CLOSE PAYMENT-FILE
           CLOSE ERROR-FILE
           CLOSE REPORT-FILE
           CLOSE AUDIT-TRAIL-FILE

      *--- DISCONNECT FROM DATABASE ---
           IF DB-IS-CONNECTED
               EXEC SQL
                   DISCONNECT CURRENT
               END-EXEC
               SET DB-NOT-CONNECTED TO TRUE
               DISPLAY 'DATABASE DISCONNECTED'
           END-IF

           DISPLAY 'HCCLMADJ - NORMAL TERMINATION'
           .
       9000-TERMINATION-EXIT.
           EXIT.

      *================================================================*
      * 9999 - ABEND PROGRAM                                          *
      *================================================================*
       9999-ABEND-PROGRAM.
      *---------------------------------------------------------------*
      * ABNORMAL END OF PROGRAM. ATTEMPT TO CLOSE FILES AND           *
      * DISCONNECT BEFORE ABENDING.                                    *
      *---------------------------------------------------------------*
           DISPLAY '***** ABEND *****'
           DISPLAY 'ABEND CODE: ' WS-ABEND-CODE
           DISPLAY 'LAST ERROR: ' WS-ERR-MESSAGE
           DISPLAY 'PARAGRAPH:  ' WS-ERR-PARAGRAPH

      *--- ATTEMPT TO CLOSE FILES ---
           CLOSE CLAIM-INPUT-FILE
           CLOSE ADJUDICATED-OUTPUT-FILE
           CLOSE PEND-FILE
           CLOSE APPEAL-FILE
           CLOSE PAYMENT-FILE
           CLOSE ERROR-FILE
           CLOSE REPORT-FILE
           CLOSE AUDIT-TRAIL-FILE

      *--- ATTEMPT DATABASE DISCONNECT ---
           IF DB-IS-CONNECTED
               EXEC SQL
                   ROLLBACK TRANSACTION
               END-EXEC
               EXEC SQL
                   DISCONNECT CURRENT
               END-EXEC
           END-IF

           DISPLAY 'HCCLMADJ - ABNORMAL TERMINATION'
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
       9999-ABEND-PROGRAM-EXIT.
           EXIT.
