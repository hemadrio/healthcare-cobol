      ******************************************************************
      * PROGRAM:    HCRPTGEN
      * TITLE:      HEALTHCARE MANAGEMENT REPORTING AND ANALYTICS
      *             BATCH REPORT GENERATION SYSTEM
      * AUTHOR:     ENTERPRISE REPORTING TEAM
      * DATE:       1994-03-15
      * SYSTEM:     HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
      * SUBSYSTEM:  MANAGEMENT REPORTING & ANALYTICS (MRA)
      *
      * DESCRIPTION:
      *   THIS PROGRAM GENERATES 12 COMPREHENSIVE MANAGEMENT
      *   REPORTS FOR HEALTHCARE CLAIMS OPERATIONS INCLUDING:
      *     RPT01 - CLAIMS AGING ANALYSIS
      *     RPT02 - PROVIDER PAYMENT SUMMARY
      *     RPT03 - PAYER MIX ANALYSIS
      *     RPT04 - DENIAL ANALYSIS
      *     RPT05 - FINANCIAL SUMMARY DASHBOARD
      *     RPT06 - PEND QUEUE AGING
      *     RPT07 - AUTHORIZATION UTILIZATION
      *     RPT08 - HIGH-DOLLAR CLAIMS TRACKING
      *     RPT09 - DUPLICATE CLAIMS DETECTION
      *     RPT10 - QUALITY METRICS DASHBOARD
      *     RPT11 - FRAUD/WASTE/ABUSE INDICATORS
      *     RPT12 - REGULATORY COMPLIANCE
      *
      *   READS CONTROL FILE FOR REPORT SELECTION AND DATE
      *   OVERRIDES. QUERIES DB2 CLAIMS/PROVIDER/ELIGIBILITY
      *   TABLES. PRODUCES FIXED-FORMAT PRINT FILES FOR
      *   DISTRIBUTION VIA JES2 OR PDF CONVERSION.
      *
      * INPUT FILES:
      *   RPTCTRL  - REPORT CONTROL PARAMETER FILE
      *
      * OUTPUT FILES:
      *   RPT01FL  - CLAIMS AGING REPORT
      *   RPT02FL  - PROVIDER PAYMENT SUMMARY
      *   RPT03FL  - PAYER MIX ANALYSIS
      *   RPT04FL  - DENIAL ANALYSIS REPORT
      *   RPT05FL  - FINANCIAL SUMMARY DASHBOARD
      *   RPT06FL  - PEND QUEUE AGING REPORT
      *   RPT07FL  - AUTHORIZATION UTILIZATION
      *   RPT08FL  - HIGH-DOLLAR CLAIMS REPORT
      *   RPT09FL  - DUPLICATE CLAIMS REPORT
      *   RPT10FL  - QUALITY METRICS DASHBOARD
      *   RPT11FL  - FWA INDICATORS REPORT
      *   RPT12FL  - REGULATORY COMPLIANCE REPORT
      *   ERRFILE  - ERROR/EXCEPTION LOG
      *   AUDFILE  - AUDIT TRAIL FILE
      *
      * DB2 TABLES ACCESSED:
      *   HCDB.CLAIM_HEADER        - CLAIM HEADER DATA
      *   HCDB.CLAIM_LINE          - CLAIM LINE DETAIL
      *   HCDB.PROVIDER_MASTER     - PROVIDER DEMOGRAPHICS
      *   HCDB.MEMBER_ELIGIBILITY  - MEMBER ELIGIBILITY
      *   HCDB.AUTHORIZATION       - AUTH/REFERRAL DATA
      *   HCDB.PAYER_CONTRACT      - PAYER CONTRACT TERMS
      *   HCDB.DENIAL_REASON       - DENIAL REASON CODES
      *   HCDB.PEND_QUEUE          - PENDING CLAIMS QUEUE
      *   HCDB.PAYMENT_HISTORY     - PAYMENT TRANSACTIONS
      *   HCDB.QUALITY_MEASURES    - HEDIS/STAR METRICS
      *   HCDB.FWA_INDICATORS      - FRAUD/WASTE ALERTS
      *   HCDB.REGULATORY_CONFIG   - COMPLIANCE PARAMETERS
      *
      * MODIFICATION HISTORY:
      * DATE       AUTHOR       TICKET    DESCRIPTION
      * ---------- ------------ --------- -------------------------
      * 1994-03-15 R.MORRISON   INIT-001  INITIAL DEVELOPMENT
      *                                    REPORTS 01-05 ONLY
      * 1994-08-22 R.MORRISON   RPT-0042  ADDED REPORTS 06-08
      *                                    PEND/AUTH/HIGH-DOLLAR
      * 1995-01-10 D.NAKAMURA   RPT-0089  ADDED DUPLICATE CLAIMS
      *                                    DETECTION REPORT (09)
      * 1995-06-30 D.NAKAMURA   RPT-0134  ADDED QUALITY METRICS
      *                                    AND FWA REPORTS (10-11)
      * 1995-11-15 R.MORRISON   RPT-0167  ADDED REGULATORY
      *                                    COMPLIANCE REPORT (12)
      * 1996-03-20 S.PETROVA    RPT-0201  REDESIGNED AGING BUCKETS
      *                                    ADDED 121-180 AND 181+
      * 1996-09-12 S.PETROVA    RPT-0245  ADDED TREND COMPARISON
      *                                    CURRENT VS PRIOR MONTH
      * 1997-02-28 K.WASHINGTON RPT-0312  ADDED PROVIDER RANKING
      *                                    TOP 50 BY PAYMENT VOL
      * 1997-07-14 K.WASHINGTON RPT-0356  PAYER MIX 12-MONTH
      *                                    ROLLING TREND ANALYSIS
      * 1998-01-05 D.NAKAMURA   RPT-0402  Y2K REMEDIATION - ALL
      *                                    DATE FIELDS EXPANDED
      * 1998-06-19 R.MORRISON   RPT-0445  ADDED MLR CALCULATION
      *                                    TO FINANCIAL SUMMARY
      * 1999-03-08 S.PETROVA    RPT-0501  EXPANDED DENIAL CODES
      *                                    FROM 25 TO 50 REASONS
      * 1999-11-30 K.WASHINGTON RPT-0534  Y2K FINAL CERTIFICATION
      *                                    ALL DATE LOGIC VERIFIED
      * 2000-04-17 D.NAKAMURA   RPT-0578  POST-Y2K STABILIZATION
      *                                    CENTURY WINDOW FIXES
      * 2001-02-14 T.OKONKWO    RPT-0623  HIPAA TRANSACTION
      *                                    COMPLIANCE METRICS
      * 2002-08-09 T.OKONKWO    RPT-0689  ADDED PRODUCT LINE
      *                                    BREAKDOWN (HMO/PPO/POS)
      * 2003-05-22 S.PETROVA    RPT-0734  MEDICARE PART D METRICS
      *                                    ADDED TO PAYER MIX
      * 2004-11-03 K.WASHINGTON RPT-0801  REINSURANCE STOP-LOSS
      *                                    TRACKING ENHANCEMENT
      * 2006-03-17 T.OKONKWO    RPT-0867  NPI TRANSITION SUPPORT
      *                                    PROVIDER REPORT CHANGES
      * 2007-09-28 D.NAKAMURA   RPT-0923  ADDED STAR RATING
      *                                    COMPONENTS TO RPT10
      * 2008-06-11 L.MARTINEZ   RPT-0978  IBNR ESTIMATION LOGIC
      *                                    ADDED TO FINANCIAL RPT
      * 2010-01-25 L.MARTINEZ   RPT-1045  ACA COMPLIANCE METRICS
      *                                    1094-C/1095-C READINESS
      * 2011-07-19 T.OKONKWO    RPT-1102  ICD-10 TRANSITION
      *                                    DUAL CODING SUPPORT
      * 2012-12-05 L.MARTINEZ   RPT-1156  EXCHANGE/MARKETPLACE
      *                                    PLAN REPORTING
      * 2014-04-23 A.KRISHNAN   RPT-1234  PAYMENT METHOD BREAKOUT
      *                                    CHECK VS EFT TRACKING
      * 2015-10-08 A.KRISHNAN   RPT-1289  HEDIS 2015 MEASURE SET
      *                                    QUALITY METRICS UPDATE
      * 2017-03-14 L.MARTINEZ   RPT-1345  MACRA/MIPS QUALITY
      *                                    PAYMENT PROGRAM METRICS
      * 2018-08-27 A.KRISHNAN   RPT-1401  SURPRISE BILLING METRICS
      *                                    ADDED TO COMPLIANCE
      * 2019-11-19 J.TANAKA     RPT-1456  PERFORMANCE TUNING
      *                                    SQL OPTIMIZATION PASS
      * 2020-06-03 J.TANAKA     RPT-1502  COVID-19 CLAIM TYPE
      *                                    TRACKING AND REPORTING
      * 2021-02-17 A.KRISHNAN   RPT-1558  TELEHEALTH UTILIZATION
      *                                    METRICS ADDED TO RPT07
      * 2022-09-12 J.TANAKA     RPT-1612  NO SURPRISES ACT
      *                                    COMPLIANCE METRICS
      * 2023-04-28 J.TANAKA     RPT-1667  PRICE TRANSPARENCY
      *                                    REPORTING REQUIREMENTS
      * 2024-01-15 M.ODUYA      RPT-1723  ANNUAL MEASURE UPDATE
      *                                    HEDIS MY2024 ALIGNMENT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HCRPTGEN.
       AUTHOR.        ENTERPRISE REPORTING TEAM.
       INSTALLATION.  HEALTHCARE CLAIMS PROCESSING CENTER.
       DATE-WRITTEN.  1994-03-15.
       DATE-COMPILED.
       SECURITY.      CONFIDENTIAL - PHI DATA - AUTHORIZED ACCESS ONLY.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-ZOS WITH DEBUGGING MODE.
       OBJECT-COMPUTER.    IBM-ZOS.
       SPECIAL-NAMES.
           C01 IS PAGE-EJECT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RPTCTRL-FILE
               ASSIGN TO RPTCTRL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPTCTRL-STATUS.

           SELECT RPT01-FILE
               ASSIGN TO RPT01FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT01-STATUS.

           SELECT RPT02-FILE
               ASSIGN TO RPT02FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT02-STATUS.

           SELECT RPT03-FILE
               ASSIGN TO RPT03FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT03-STATUS.

           SELECT RPT04-FILE
               ASSIGN TO RPT04FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT04-STATUS.

           SELECT RPT05-FILE
               ASSIGN TO RPT05FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT05-STATUS.

           SELECT RPT06-FILE
               ASSIGN TO RPT06FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT06-STATUS.

           SELECT RPT07-FILE
               ASSIGN TO RPT07FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT07-STATUS.

           SELECT RPT08-FILE
               ASSIGN TO RPT08FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT08-STATUS.

           SELECT RPT09-FILE
               ASSIGN TO RPT09FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT09-STATUS.

           SELECT RPT10-FILE
               ASSIGN TO RPT10FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT10-STATUS.

           SELECT RPT11-FILE
               ASSIGN TO RPT11FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT11-STATUS.

           SELECT RPT12-FILE
               ASSIGN TO RPT12FL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT12-STATUS.

           SELECT ERROR-FILE
               ASSIGN TO ERRFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERROR-STATUS.

           SELECT AUDIT-FILE
               ASSIGN TO AUDFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.

       FD  RPTCTRL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPTCTRL-RECORD.
           05  RPTCTRL-RECORD-TYPE     PIC X(02).
           05  RPTCTRL-DATA            PIC X(78).

       FD  RPT01-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT01-RECORD                PIC X(132).

       FD  RPT02-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT02-RECORD                PIC X(132).

       FD  RPT03-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT03-RECORD                PIC X(132).

       FD  RPT04-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT04-RECORD                PIC X(132).

       FD  RPT05-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT05-RECORD                PIC X(132).

       FD  RPT06-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT06-RECORD                PIC X(132).

       FD  RPT07-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT07-RECORD                PIC X(132).

       FD  RPT08-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT08-RECORD                PIC X(132).

       FD  RPT09-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT09-RECORD                PIC X(132).

       FD  RPT10-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT10-RECORD                PIC X(132).

       FD  RPT11-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT11-RECORD                PIC X(132).

       FD  RPT12-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RPT12-RECORD                PIC X(132).

       FD  ERROR-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  ERROR-RECORD                PIC X(200).

       FD  AUDIT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  AUDIT-RECORD                PIC X(200).

      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************

       01  WS-PROGRAM-ID              PIC X(08) VALUE 'HCRPTGEN'.
       01  WS-VERSION                 PIC X(06) VALUE '24.001'.

      *---------------------------------------------------------------*
      *    COPYBOOK INCLUDES                                          *
      *---------------------------------------------------------------*
           COPY CPYCLMHD.
           COPY CPYCLMLN.
           COPY CPYPROVD.
           COPY CPYELIG.
           COPY CPYSQLCA.
           COPY CPYERROR.

      *---------------------------------------------------------------*
      *    FILE STATUS VARIABLES                                      *
      *---------------------------------------------------------------*
       01  WS-FILE-STATUS-AREA.
           05  WS-RPTCTRL-STATUS       PIC X(02) VALUE SPACES.
           05  WS-RPT01-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT02-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT03-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT04-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT05-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT06-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT07-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT08-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT09-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT10-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT11-STATUS         PIC X(02) VALUE SPACES.
           05  WS-RPT12-STATUS         PIC X(02) VALUE SPACES.
           05  WS-ERROR-STATUS         PIC X(02) VALUE SPACES.
           05  WS-AUDIT-STATUS         PIC X(02) VALUE SPACES.

      *---------------------------------------------------------------*
      *    PROGRAM FLAGS AND SWITCHES                                 *
      *---------------------------------------------------------------*
       01  WS-FLAGS.
           05  WS-EOF-RPTCTRL         PIC X(01) VALUE 'N'.
               88  EOF-RPTCTRL                   VALUE 'Y'.
               88  NOT-EOF-RPTCTRL               VALUE 'N'.
           05  WS-EOF-CURSOR          PIC X(01) VALUE 'N'.
               88  EOF-CURSOR                    VALUE 'Y'.
               88  NOT-EOF-CURSOR                VALUE 'N'.
           05  WS-ABEND-FLAG          PIC X(01) VALUE 'N'.
               88  PROGRAM-ABEND                 VALUE 'Y'.
               88  PROGRAM-OK                    VALUE 'N'.
           05  WS-RPT-SELECTED        PIC X(01) VALUE 'N'.
               88  REPORT-SELECTED               VALUE 'Y'.
               88  REPORT-NOT-SELECTED           VALUE 'N'.
           05  WS-FIRST-RECORD        PIC X(01) VALUE 'Y'.
               88  IS-FIRST-RECORD               VALUE 'Y'.
               88  NOT-FIRST-RECORD              VALUE 'N'.
           05  WS-SQL-ERROR-FLAG       PIC X(01) VALUE 'N'.
               88  SQL-ERROR-OCCURRED            VALUE 'Y'.
               88  SQL-OK                        VALUE 'N'.
           05  WS-NEW-PAGE-FLAG        PIC X(01) VALUE 'N'.
               88  NEED-NEW-PAGE                 VALUE 'Y'.
               88  NO-NEW-PAGE                   VALUE 'N'.

      *---------------------------------------------------------------*
      *    REPORT SELECTION FLAGS (FROM CONTROL FILE)                 *
      *---------------------------------------------------------------*
       01  WS-REPORT-FLAGS.
           05  WS-RUN-RPT01           PIC X(01) VALUE 'N'.
               88  RUN-CLAIMS-AGING              VALUE 'Y'.
           05  WS-RUN-RPT02           PIC X(01) VALUE 'N'.
               88  RUN-PROVIDER-PMT              VALUE 'Y'.
           05  WS-RUN-RPT03           PIC X(01) VALUE 'N'.
               88  RUN-PAYER-MIX                 VALUE 'Y'.
           05  WS-RUN-RPT04           PIC X(01) VALUE 'N'.
               88  RUN-DENIAL-ANALYSIS           VALUE 'Y'.
           05  WS-RUN-RPT05           PIC X(01) VALUE 'N'.
               88  RUN-FINANCIAL-SUMM            VALUE 'Y'.
           05  WS-RUN-RPT06           PIC X(01) VALUE 'N'.
               88  RUN-PEND-AGING                VALUE 'Y'.
           05  WS-RUN-RPT07           PIC X(01) VALUE 'N'.
               88  RUN-AUTH-UTIL                 VALUE 'Y'.
           05  WS-RUN-RPT08           PIC X(01) VALUE 'N'.
               88  RUN-HIGH-DOLLAR               VALUE 'Y'.
           05  WS-RUN-RPT09           PIC X(01) VALUE 'N'.
               88  RUN-DUPLICATE-DET             VALUE 'Y'.
           05  WS-RUN-RPT10           PIC X(01) VALUE 'N'.
               88  RUN-QUALITY-MTRC              VALUE 'Y'.
           05  WS-RUN-RPT11           PIC X(01) VALUE 'N'.
               88  RUN-FWA-REPORT                VALUE 'Y'.
           05  WS-RUN-RPT12           PIC X(01) VALUE 'N'.
               88  RUN-COMPLIANCE                VALUE 'Y'.

      *---------------------------------------------------------------*
      *    DATE AND PERIOD FIELDS                                     *
      *---------------------------------------------------------------*
       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YYYY       PIC 9(04).
               10  WS-CURR-MM         PIC 9(02).
               10  WS-CURR-DD         PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HH         PIC 9(02).
               10  WS-CURR-MN         PIC 9(02).
               10  WS-CURR-SS         PIC 9(02).
               10  WS-CURR-HS         PIC 9(02).
           05  WS-REPORT-DATE-DISP    PIC X(10).
           05  WS-REPORT-TIME-DISP    PIC X(08).
           05  WS-RPT-PERIOD-TYPE     PIC X(01) VALUE 'M'.
               88  PERIOD-DAILY                  VALUE 'D'.
               88  PERIOD-WEEKLY                 VALUE 'W'.
               88  PERIOD-MONTHLY                VALUE 'M'.
               88  PERIOD-QUARTERLY              VALUE 'Q'.
               88  PERIOD-ANNUAL                 VALUE 'A'.
           05  WS-RPT-START-DATE      PIC X(10).
           05  WS-RPT-END-DATE        PIC X(10).
           05  WS-PRIOR-START-DATE    PIC X(10).
           05  WS-PRIOR-END-DATE      PIC X(10).
           05  WS-YTD-START-DATE      PIC X(10).
           05  WS-FISCAL-YR-START     PIC X(10).
           05  WS-ROLLING-12-START    PIC X(10).
           05  WS-PERIOD-START-INT    PIC 9(08).
           05  WS-PERIOD-END-INT      PIC 9(08).
           05  WS-PRIOR-START-INT     PIC 9(08).
           05  WS-PRIOR-END-INT       PIC 9(08).
           05  WS-JULIAN-DATE         PIC 9(07).
           05  WS-DAYS-IN-MONTH       PIC 9(02).
           05  WS-FISCAL-YEAR-MM      PIC 9(02) VALUE 07.
           05  WS-QUARTER-NUM         PIC 9(01).
           05  WS-OVERRIDE-START      PIC X(10) VALUE SPACES.
           05  WS-OVERRIDE-END        PIC X(10) VALUE SPACES.

      *---------------------------------------------------------------*
      *    CONTROL FILE PARAMETER AREA                                *
      *---------------------------------------------------------------*
       01  WS-CONTROL-PARMS.
           05  WS-CTRL-RECORD-TYPE    PIC X(02).
           05  WS-CTRL-RPT-SELECT.
               10  WS-CTRL-RPT-ID     PIC X(05).
               10  WS-CTRL-RPT-FLAG   PIC X(01).
           05  WS-CTRL-DATE-OVERRIDE.
               10  WS-CTRL-OVR-TYPE   PIC X(01).
               10  WS-CTRL-OVR-START  PIC X(10).
               10  WS-CTRL-OVR-END    PIC X(10).
           05  WS-CTRL-THRESHOLD.
               10  WS-CTRL-THR-CODE   PIC X(04).
               10  WS-CTRL-THR-VALUE  PIC 9(11)V99.
           05  WS-CTRL-DISTRIB-LIST   PIC X(60).
           05  WS-CTRL-PERIOD-TYPE    PIC X(01).
           05  WS-HIGH-DOLLAR-THRESH  PIC S9(11)V99 COMP-3
                                      VALUE 25000.00.
           05  WS-FWA-OUTLIER-PCTILE  PIC S9(03)V99 COMP-3
                                      VALUE 99.00.
           05  WS-PROMPT-PAY-DAYS     PIC S9(03) COMP-3
                                      VALUE 30.
           05  WS-CLEAN-CLAIM-DAYS    PIC S9(03) COMP-3
                                      VALUE 30.
           05  WS-MAX-RECORDS         PIC S9(09) COMP VALUE 999999.
           05  WS-TOP-N-PROVIDERS     PIC S9(03) COMP-3 VALUE 50.

      *---------------------------------------------------------------*
      *    PAGE CONTROL VARIABLES                                     *
      *---------------------------------------------------------------*
       01  WS-PAGE-CONTROL.
           05  WS-MAX-LINES           PIC S9(03) COMP-3 VALUE 55.
           05  WS-RPT01-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT01-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT02-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT02-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT03-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT03-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT04-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT04-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT05-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT05-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT06-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT06-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT07-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT07-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT08-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT08-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT09-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT09-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT10-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT10-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT11-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT11-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT12-LINE-CTR      PIC S9(03) COMP-3 VALUE 99.
           05  WS-RPT12-PAGE-CTR      PIC S9(05) COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    REPORT COUNTERS AND STATISTICS                             *
      *---------------------------------------------------------------*
       01  WS-REPORT-STATS.
           05  WS-TOTAL-CTRL-RECS     PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-REPORTS-RUN   PIC S9(03) COMP-3 VALUE 0.
           05  WS-TOTAL-ERRORS        PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-WARNINGS      PIC S9(07) COMP-3 VALUE 0.
           05  WS-RPT01-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT02-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT03-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT04-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT05-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT06-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT07-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT08-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT09-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT10-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT11-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RPT12-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-ERROR-RECORDS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUDIT-RECORDS       PIC S9(09) COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    WORKING FIELDS FOR CALCULATIONS                            *
      *---------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-WORK-AMT1           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-WORK-AMT2           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-WORK-AMT3           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-WORK-PCT            PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-WORK-COUNT          PIC S9(09)   COMP-3  VALUE 0.
           05  WS-WORK-COUNT2         PIC S9(09)   COMP-3  VALUE 0.
           05  WS-WORK-DAYS           PIC S9(05)   COMP-3  VALUE 0.
           05  WS-WORK-AVG            PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-WORK-RATE           PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
           05  WS-WORK-INDEX          PIC S9(03)   COMP    VALUE 0.
           05  WS-WORK-INDEX2         PIC S9(03)   COMP    VALUE 0.
           05  WS-WORK-INDEX3         PIC S9(03)   COMP    VALUE 0.
           05  WS-WORK-STRING         PIC X(132)   VALUE SPACES.
           05  WS-WORK-DATE           PIC X(10)    VALUE SPACES.
           05  WS-DAYS-AGED           PIC S9(05)   COMP-3  VALUE 0.
           05  WS-BUCKET-INDEX        PIC S9(02)   COMP-3  VALUE 0.
           05  WS-PAYER-INDEX         PIC S9(02)   COMP-3  VALUE 0.
           05  WS-CLMTYP-INDEX        PIC S9(02)   COMP-3  VALUE 0.
           05  WS-MONTH-INDEX         PIC S9(02)   COMP-3  VALUE 0.
           05  WS-DENIAL-INDEX        PIC S9(02)   COMP-3  VALUE 0.
           05  WS-SPEC-INDEX          PIC S9(02)   COMP-3  VALUE 0.
           05  WS-PROD-INDEX          PIC S9(02)   COMP-3  VALUE 0.
           05  WS-RANK-INDEX          PIC S9(03)   COMP-3  VALUE 0.
           05  WS-PROVIDER-RANK       PIC S9(03)   COMP-3  VALUE 0.
           05  WS-SAVE-PAYER-ID       PIC X(10)    VALUE SPACES.
           05  WS-SAVE-PROVIDER-ID    PIC X(10)    VALUE SPACES.
           05  WS-SAVE-CLAIM-TYPE     PIC X(02)    VALUE SPACES.
           05  WS-SAVE-FACILITY-ID    PIC X(10)    VALUE SPACES.
           05  WS-VARIANCE-AMT        PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-VARIANCE-PCT        PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-GRAND-TOTAL-AMT     PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-GRAND-TOTAL-CNT     PIC S9(11)    COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    AGING BUCKET ACCUMULATOR TABLE (20 PAYERS x 6 BUCKETS)    *
      *---------------------------------------------------------------*
       01  WS-AGING-TABLE.
           05  WS-AGING-PAYER OCCURS 20 TIMES.
               10  WS-AG-PAYER-ID      PIC X(10).
               10  WS-AG-PAYER-NAME    PIC X(30).
               10  WS-AG-BUCKET OCCURS 6 TIMES.
                   15  WS-AG-BKT-COUNT PIC S9(09)   COMP-3 VALUE 0.
                   15  WS-AG-BKT-AMT   PIC S9(13)V99 COMP-3 VALUE 0.
                   15  WS-AG-BKT-AVG-DAYS
                                        PIC S9(05)V99 COMP-3 VALUE 0.
                   15  WS-AG-BKT-TOTAL-DAYS
                                        PIC S9(11)   COMP-3 VALUE 0.
               10  WS-AG-PAYER-TOTAL-CNT
                                        PIC S9(11)   COMP-3 VALUE 0.
               10  WS-AG-PAYER-TOTAL-AMT
                                        PIC S9(15)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    AGING BY CLAIM TYPE TABLE (4 TYPES x 6 BUCKETS)           *
      *---------------------------------------------------------------*
       01  WS-AGING-CLMTYPE-TABLE.
           05  WS-AG-CLMTYPE OCCURS 4 TIMES.
               10  WS-AGC-TYPE-CODE    PIC X(02).
               10  WS-AGC-TYPE-DESC    PIC X(20).
               10  WS-AGC-BUCKET OCCURS 6 TIMES.
                   15  WS-AGC-BKT-CNT  PIC S9(09)   COMP-3 VALUE 0.
                   15  WS-AGC-BKT-AMT  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-AGC-TOTAL-CNT    PIC S9(11)   COMP-3 VALUE 0.
               10  WS-AGC-TOTAL-AMT    PIC S9(15)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    AGING GRAND TOTALS (6 BUCKETS)                             *
      *---------------------------------------------------------------*
       01  WS-AGING-GRAND-TOTALS.
           05  WS-AGG-BUCKET OCCURS 6 TIMES.
               10  WS-AGG-BKT-COUNT   PIC S9(11)   COMP-3 VALUE 0.
               10  WS-AGG-BKT-AMT     PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-AGG-BKT-PCT     PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-AGG-BKT-AVG-DAYS
                                       PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-AGG-GRAND-CNT       PIC S9(11)   COMP-3 VALUE 0.
           05  WS-AGG-GRAND-AMT       PIC S9(15)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    PRIOR PERIOD AGING FOR TREND COMPARISON                    *
      *---------------------------------------------------------------*
       01  WS-PRIOR-AGING-TOTALS.
           05  WS-PAG-BUCKET OCCURS 6 TIMES.
               10  WS-PAG-BKT-COUNT   PIC S9(11)   COMP-3 VALUE 0.
               10  WS-PAG-BKT-AMT     PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-PAG-GRAND-CNT       PIC S9(11)   COMP-3 VALUE 0.
           05  WS-PAG-GRAND-AMT       PIC S9(15)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    BUCKET RANGE DEFINITIONS                                   *
      *---------------------------------------------------------------*
       01  WS-BUCKET-RANGES.
           05  WS-BUCKET-DEF OCCURS 6 TIMES.
               10  WS-BKT-LOW         PIC S9(05) COMP-3.
               10  WS-BKT-HIGH        PIC S9(05) COMP-3.
               10  WS-BKT-LABEL       PIC X(10).

      *---------------------------------------------------------------*
      *    PROVIDER PAYMENT ACCUMULATOR TABLE (TOP 50+)               *
      *---------------------------------------------------------------*
       01  WS-PROVIDER-TABLE.
           05  WS-PROV-COUNT          PIC S9(05) COMP-3 VALUE 0.
           05  WS-PROV-ENTRY OCCURS 500 TIMES.
               10  WS-PRV-NPI         PIC X(10).
               10  WS-PRV-TAX-ID      PIC X(09).
               10  WS-PRV-NAME        PIC X(35).
               10  WS-PRV-SPECIALTY   PIC X(03).
               10  WS-PRV-CLM-CNT     PIC S9(09)   COMP-3 VALUE 0.
               10  WS-PRV-BILLED-AMT  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-ALLOWED-AMT PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-PAID-AMT    PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-WITHHOLD    PIC S9(11)V99 COMP-3 VALUE 0.
               10  WS-PRV-NET-PMT     PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-AVG-PMT     PIC S9(09)V99 COMP-3 VALUE 0.
               10  WS-PRV-YTD-PAID    PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-MTD-PAID    PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-PRIOR-PAID  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-CHECK-CNT   PIC S9(07)   COMP-3 VALUE 0.
               10  WS-PRV-CHECK-AMT   PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-EFT-CNT     PIC S9(07)   COMP-3 VALUE 0.
               10  WS-PRV-EFT-AMT     PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PRV-RANK        PIC S9(03)   COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    PAYER MIX ANALYSIS TABLE (20 PAYERS x 12 MONTHS)          *
      *---------------------------------------------------------------*
       01  WS-PAYER-MIX-TABLE.
           05  WS-PMX-PAYER OCCURS 20 TIMES.
               10  WS-PMX-PAYER-ID     PIC X(10).
               10  WS-PMX-PAYER-NAME   PIC X(30).
               10  WS-PMX-PAYER-TYPE   PIC X(02).
               10  WS-PMX-MONTH OCCURS 12 TIMES.
                   15  WS-PMX-REVENUE  PIC S9(13)V99 COMP-3 VALUE 0.
                   15  WS-PMX-CLM-CNT  PIC S9(09)   COMP-3 VALUE 0.
                   15  WS-PMX-DENIED   PIC S9(09)   COMP-3 VALUE 0.
                   15  WS-PMX-COLLECTED
                                       PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PMX-TOTAL-REV   PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-PMX-TOTAL-CLM   PIC S9(11)   COMP-3 VALUE 0.
               10  WS-PMX-AVG-REIMB   PIC S9(07)V9(04) COMP-3
                                                          VALUE 0.
               10  WS-PMX-DENIAL-RATE PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-PMX-COLL-RATE   PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-PMX-REV-PCT     PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    PRODUCT LINE TABLE (HMO/PPO/POS/EPO/HDHP/IND/OTHER)       *
      *---------------------------------------------------------------*
       01  WS-PRODUCT-LINE-TABLE.
           05  WS-PROD-LINE OCCURS 7 TIMES.
               10  WS-PL-CODE         PIC X(05).
               10  WS-PL-DESC         PIC X(20).
               10  WS-PL-REVENUE      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PL-CLM-CNT      PIC S9(09)   COMP-3 VALUE 0.
               10  WS-PL-PAID-AMT     PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PL-AVG-REIMB    PIC S9(07)V9(04) COMP-3
                                                          VALUE 0.
               10  WS-PL-REV-PCT      PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    DENIAL ANALYSIS TABLE (50 REASON CODES)                    *
      *---------------------------------------------------------------*
       01  WS-DENIAL-TABLE.
           05  WS-DEN-REASON OCCURS 50 TIMES.
               10  WS-DEN-CODE        PIC X(05).
               10  WS-DEN-DESC        PIC X(40).
               10  WS-DEN-CATEGORY    PIC X(03).
               10  WS-DEN-COUNT       PIC S9(09)   COMP-3 VALUE 0.
               10  WS-DEN-AMOUNT      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-DEN-APPEAL-CNT  PIC S9(07)   COMP-3 VALUE 0.
               10  WS-DEN-OVERTURN    PIC S9(07)   COMP-3 VALUE 0.
               10  WS-DEN-RATE        PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-DEN-TREND-CURR  PIC S9(09)   COMP-3 VALUE 0.
               10  WS-DEN-TREND-PRIOR PIC S9(09)   COMP-3 VALUE 0.
           05  WS-DEN-TOTAL-COUNT     PIC S9(11)   COMP-3 VALUE 0.
           05  WS-DEN-TOTAL-AMOUNT    PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-DEN-TOTAL-CLAIMS    PIC S9(11)   COMP-3 VALUE 0.
           05  WS-DEN-OVERALL-RATE    PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    DENIAL BY PAYER TABLE                                      *
      *---------------------------------------------------------------*
       01  WS-DENIAL-PAYER-TABLE.
           05  WS-DENP-ENTRY OCCURS 20 TIMES.
               10  WS-DENP-PAYER-ID   PIC X(10).
               10  WS-DENP-PAYER-NAME PIC X(30).
               10  WS-DENP-TOTAL-CLM  PIC S9(09)   COMP-3 VALUE 0.
               10  WS-DENP-DENIED-CLM PIC S9(09)   COMP-3 VALUE 0.
               10  WS-DENP-DENIED-AMT PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-DENP-DEN-RATE   PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    DENIAL CATEGORY SUMMARY                                    *
      *---------------------------------------------------------------*
       01  WS-DENIAL-CATEGORY-TABLE.
           05  WS-DCAT-ENTRY OCCURS 10 TIMES.
               10  WS-DCAT-CODE       PIC X(03).
               10  WS-DCAT-DESC       PIC X(25).
               10  WS-DCAT-COUNT      PIC S9(09)   COMP-3 VALUE 0.
               10  WS-DCAT-AMOUNT     PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-DCAT-PCT        PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    FINANCIAL SUMMARY ACCUMULATORS                             *
      *---------------------------------------------------------------*
       01  WS-FINANCIAL-SUMMARY.
           05  WS-FIN-MTD.
               10  WS-FIN-MTD-CHARGES PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-ALLOWED PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-PAID    PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-CONTRACTUAL
                                      PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-WRITEOFF
                                      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-BADDEBT PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-DEDUCT  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-COPAY   PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-COINS   PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-PAT-RESP
                                      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-NET-REV PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-PREMIUM PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-MLR     PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
               10  WS-FIN-MTD-ADMIN-EXP
                                      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-MTD-ADMIN-RATIO
                                      PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
               10  WS-FIN-MTD-COLLECT-RATE
                                      PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
               10  WS-FIN-MTD-AVG-REIMB
                                      PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
           05  WS-FIN-YTD.
               10  WS-FIN-YTD-CHARGES PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-ALLOWED PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-PAID    PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-CONTRACTUAL
                                      PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-WRITEOFF
                                      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-BADDEBT PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-DEDUCT  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-COPAY   PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-COINS   PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-PAT-RESP
                                      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-NET-REV PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-PREMIUM PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-YTD-MLR     PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
           05  WS-FIN-BUDGET.
               10  WS-FIN-BUD-CHARGES PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-BUD-PAID    PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-FIN-BUD-NET-REV PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-FIN-IBNR-EST        PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-FIN-DISCOUNT-PCT    PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.

      *---------------------------------------------------------------*
      *    PEND QUEUE ACCUMULATORS                                    *
      *---------------------------------------------------------------*
       01  WS-PEND-TABLE.
           05  WS-PEND-QUEUE OCCURS 8 TIMES.
               10  WS-PQ-QUEUE-CODE   PIC X(03).
               10  WS-PQ-QUEUE-DESC   PIC X(25).
               10  WS-PQ-TOTAL-CNT    PIC S9(09)   COMP-3 VALUE 0.
               10  WS-PQ-TOTAL-AMT    PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PQ-BUCKET OCCURS 6 TIMES.
                   15  WS-PQ-BKT-CNT  PIC S9(09)   COMP-3 VALUE 0.
                   15  WS-PQ-BKT-AMT  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-PQ-AVG-DAYS     PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-PQ-TOTAL-DAYS   PIC S9(11)   COMP-3 VALUE 0.
               10  WS-PQ-PAST-DUE-CNT PIC S9(07)   COMP-3 VALUE 0.
               10  WS-PQ-AUTO-CAND    PIC S9(07)   COMP-3 VALUE 0.
           05  WS-PEND-GRAND-CNT      PIC S9(11)   COMP-3 VALUE 0.
           05  WS-PEND-GRAND-AMT      PIC S9(15)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    AUTHORIZATION UTILIZATION ACCUMULATORS                     *
      *---------------------------------------------------------------*
       01  WS-AUTH-TABLE.
           05  WS-AUTH-TOTAL-REQUESTS  PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-APPROVED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-DENIED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-PENDED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-APPROVAL-RATE   PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-AUTH-DENIAL-RATE     PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-AUTH-AVG-TAT        PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-AUTH-TO-CLM-RATE    PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-AUTH-NO-AUTH-SVCS   PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-EXPIRED-UNUSED PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-EXPIRED-AMT    PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-AUTH-CONCURRENT-CNT PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-CONCURRENT-APP PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-RETRO-CNT      PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-RETRO-APP      PIC S9(09) COMP-3 VALUE 0.
           05  WS-AUTH-BY-SPECIALTY OCCURS 30 TIMES.
               10  WS-ABS-SPEC-CODE   PIC X(03).
               10  WS-ABS-SPEC-DESC   PIC X(25).
               10  WS-ABS-REQUEST-CNT PIC S9(07) COMP-3 VALUE 0.
               10  WS-ABS-APPROVED    PIC S9(07) COMP-3 VALUE 0.
               10  WS-ABS-DENIED      PIC S9(07) COMP-3 VALUE 0.
               10  WS-ABS-AVG-TAT     PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    HIGH-DOLLAR CLAIMS TABLE                                   *
      *---------------------------------------------------------------*
       01  WS-HIGH-DOLLAR-TABLE.
           05  WS-HD-THRESHOLD OCCURS 6 TIMES.
               10  WS-HD-THR-AMOUNT   PIC S9(11)V99 COMP-3 VALUE 0.
               10  WS-HD-THR-LABEL    PIC X(12).
               10  WS-HD-THR-COUNT    PIC S9(07)   COMP-3 VALUE 0.
               10  WS-HD-THR-TOTAL    PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-HD-THR-PAID     PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-HD-DETAIL OCCURS 200 TIMES.
               10  WS-HDD-CLAIM-ID    PIC X(15).
               10  WS-HDD-MEMBER-ID   PIC X(12).
               10  WS-HDD-PROVIDER    PIC X(35).
               10  WS-HDD-DIAG-CODE   PIC X(07).
               10  WS-HDD-PROC-CODE   PIC X(05).
               10  WS-HDD-BILLED-AMT  PIC S9(11)V99 COMP-3 VALUE 0.
               10  WS-HDD-PAID-AMT    PIC S9(11)V99 COMP-3 VALUE 0.
               10  WS-HDD-STATUS      PIC X(02).
               10  WS-HDD-CASE-MGMT   PIC X(01).
               10  WS-HDD-REINS-FLAG  PIC X(01).
               10  WS-HDD-PROJ-COST   PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-HD-DETAIL-COUNT     PIC S9(05) COMP-3 VALUE 0.
           05  WS-HD-REINS-THRESHOLD  PIC S9(11)V99 COMP-3
                                      VALUE 100000.00.
           05  WS-HD-STOPLOSS-COUNT   PIC S9(05) COMP-3 VALUE 0.
           05  WS-HD-STOPLOSS-AMT     PIC S9(15)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    DUPLICATE CLAIMS DETECTION ACCUMULATORS                    *
      *---------------------------------------------------------------*
       01  WS-DUPLICATE-TABLE.
           05  WS-DUP-TOTAL-PAIRS     PIC S9(09) COMP-3 VALUE 0.
           05  WS-DUP-TOTAL-AMT       PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-DUP-FALSE-POS       PIC S9(07) COMP-3 VALUE 0.
           05  WS-DUP-BY-PAYER OCCURS 20 TIMES.
               10  WS-DUBP-PAYER-ID   PIC X(10).
               10  WS-DUBP-PAIR-CNT   PIC S9(07) COMP-3 VALUE 0.
               10  WS-DUBP-AMOUNT     PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-DUBP-RATE       PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-DUP-BY-PROVIDER OCCURS 50 TIMES.
               10  WS-DUBR-PROV-ID    PIC X(10).
               10  WS-DUBR-PAIR-CNT   PIC S9(07) COMP-3 VALUE 0.
               10  WS-DUBR-AMOUNT     PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-DUP-BY-CRITERIA OCCURS 5 TIMES.
               10  WS-DUBC-CRITERIA   PIC X(20).
               10  WS-DUBC-MATCH-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-DUP-DETAIL OCCURS 100 TIMES.
               10  WS-DUPD-CLM-ID-1   PIC X(15).
               10  WS-DUPD-CLM-ID-2   PIC X(15).
               10  WS-DUPD-MATCH-TYPE PIC X(03).
               10  WS-DUPD-AMOUNT     PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-DUP-DETAIL-COUNT    PIC S9(05) COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    QUALITY METRICS TABLE                                      *
      *---------------------------------------------------------------*
       01  WS-QUALITY-TABLE.
           05  WS-QL-AUTO-ADJ-RATE    PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-QL-AUTO-ADJ-CNT     PIC S9(09)   COMP-3 VALUE 0.
           05  WS-QL-TOTAL-ADJ-CNT    PIC S9(09)   COMP-3 VALUE 0.
           05  WS-QL-FIRST-PASS-RATE  PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-QL-ACCURACY-RATE    PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-QL-ACCURACY-SAMP    PIC S9(07)   COMP-3 VALUE 0.
           05  WS-QL-ACCURACY-ERR     PIC S9(07)   COMP-3 VALUE 0.
           05  WS-QL-PROV-COMPLAINT   PIC S9(07)   COMP-3 VALUE 0.
           05  WS-QL-MEMB-COMPLAINT   PIC S9(07)   COMP-3 VALUE 0.
           05  WS-QL-APPEAL-CNT       PIC S9(07)   COMP-3 VALUE 0.
           05  WS-QL-APPEAL-OVERTURN  PIC S9(07)   COMP-3 VALUE 0.
           05  WS-QL-APPEAL-RATE      PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-QL-SLA-MET-CNT      PIC S9(09)   COMP-3 VALUE 0.
           05  WS-QL-SLA-TOTAL-CNT    PIC S9(09)   COMP-3 VALUE 0.
           05  WS-QL-SLA-PCT          PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-QL-HEDIS OCCURS 15 TIMES.
               10  WS-QH-MEASURE-ID   PIC X(08).
               10  WS-QH-MEASURE-DESC PIC X(40).
               10  WS-QH-NUMERATOR    PIC S9(09) COMP-3 VALUE 0.
               10  WS-QH-DENOMINATOR  PIC S9(09) COMP-3 VALUE 0.
               10  WS-QH-RATE         PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-QH-BENCHMARK    PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-QH-STAR-RATING  PIC S9(01)   COMP-3 VALUE 0.
               10  WS-QH-VARIANCE     PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    FWA INDICATORS TABLE                                       *
      *---------------------------------------------------------------*
       01  WS-FWA-TABLE.
           05  WS-FWA-TOTAL-ALERTS    PIC S9(09) COMP-3 VALUE 0.
           05  WS-FWA-TOTAL-AMT       PIC S9(15)V99 COMP-3 VALUE 0.
           05  WS-FWA-UPCODING-CNT    PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-UNBUNDLE-CNT    PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-DUPLICATE-CNT   PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-IMPOSSIBLE-CNT  PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-POST-DEATH-CNT  PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-VOLUME-CNT      PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-GEO-OUTLIER-CNT PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-REFERRAL-CNT    PIC S9(07) COMP-3 VALUE 0.
           05  WS-FWA-BY-PROVIDER OCCURS 50 TIMES.
               10  WS-FWAP-PROV-NPI   PIC X(10).
               10  WS-FWAP-PROV-NAME  PIC X(35).
               10  WS-FWAP-ALERT-CNT  PIC S9(05) COMP-3 VALUE 0.
               10  WS-FWAP-ALERT-AMT  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-FWAP-ALERT-TYPE PIC X(03).
               10  WS-FWAP-RISK-SCORE PIC S9(03)V99 COMP-3 VALUE 0.
           05  WS-FWA-PROV-COUNT      PIC S9(05) COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    REGULATORY COMPLIANCE TABLE                                *
      *---------------------------------------------------------------*
       01  WS-COMPLIANCE-TABLE.
           05  WS-CMP-PROMPT-PAY.
               10  WS-CMP-PP-TOTAL    PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-PP-ONTIME   PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-PP-LATE     PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-PP-PCT      PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-CMP-PP-AVG-DAYS PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-CMP-PP-INTEREST PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-CMP-CLEAN-CLAIM.
               10  WS-CMP-CC-TOTAL    PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-CC-CLEAN    PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-CC-RATE     PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-CMP-TIMELY-FILING.
               10  WS-CMP-TF-TOTAL    PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-TF-DENIED   PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-TF-RATE     PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-CMP-HIPAA.
               10  WS-CMP-HIP-837-CNT PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-HIP-835-CNT PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-HIP-277-CNT PIC S9(09)   COMP-3 VALUE 0.
               10  WS-CMP-HIP-ERR-CNT PIC S9(07)   COMP-3 VALUE 0.
               10  WS-CMP-HIP-COMPLY  PIC S9(05)V99 COMP-3 VALUE 0.
           05  WS-CMP-ACA.
               10  WS-CMP-ACA-1094-RDY PIC X(01)   VALUE 'N'.
               10  WS-CMP-ACA-1095-CNT PIC S9(09)  COMP-3 VALUE 0.
               10  WS-CMP-ACA-1095-ERR PIC S9(07)  COMP-3 VALUE 0.
           05  WS-CMP-MLR.
               10  WS-CMP-MLR-CLAIMS  PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-CMP-MLR-QI-EXP  PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-CMP-MLR-PREMIUM PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-CMP-MLR-TAXES   PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-CMP-MLR-RATIO   PIC S9(03)V9(04) COMP-3
                                                          VALUE 0.
           05  WS-CMP-STATE OCCURS 10 TIMES.
               10  WS-CMPS-STATE-CD   PIC X(02).
               10  WS-CMPS-STATE-NAME PIC X(20).
               10  WS-CMPS-PP-DAYS    PIC S9(03)   COMP-3 VALUE 0.
               10  WS-CMPS-PP-COMPLY  PIC S9(05)V99 COMP-3 VALUE 0.
               10  WS-CMPS-CLM-CNT    PIC S9(09)   COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    PROVIDER SPECIALTY TABLE (30 SPECIALTIES)                  *
      *---------------------------------------------------------------*
       01  WS-SPECIALTY-TABLE.
           05  WS-SPEC-ENTRY OCCURS 30 TIMES.
               10  WS-SPC-CODE        PIC X(03).
               10  WS-SPC-DESC        PIC X(25).
               10  WS-SPC-CLM-CNT     PIC S9(09)   COMP-3 VALUE 0.
               10  WS-SPC-BILLED      PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-SPC-PAID        PIC S9(13)V99 COMP-3 VALUE 0.
               10  WS-SPC-DENIED      PIC S9(07)   COMP-3 VALUE 0.
               10  WS-SPC-DEN-RATE    PIC S9(05)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    MONTH-BY-MONTH TREND TABLE (12 MONTHS)                     *
      *---------------------------------------------------------------*
       01  WS-MONTHLY-TREND-TABLE.
           05  WS-MTH-ENTRY OCCURS 12 TIMES.
               10  WS-MTH-YYYY-MM     PIC X(07).
               10  WS-MTH-CLM-CNT     PIC S9(09)   COMP-3 VALUE 0.
               10  WS-MTH-BILLED      PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-MTH-PAID        PIC S9(15)V99 COMP-3 VALUE 0.
               10  WS-MTH-DENIED-CNT  PIC S9(07)   COMP-3 VALUE 0.
               10  WS-MTH-DENIED-AMT  PIC S9(13)V99 COMP-3 VALUE 0.

      *---------------------------------------------------------------*
      *    SQL HOST VARIABLES                                         *
      *---------------------------------------------------------------*
       01  WS-SQL-HOST-VARS.
           05  HV-CLAIM-ID            PIC X(15).
           05  HV-CLAIM-TYPE          PIC X(02).
           05  HV-CLAIM-STATUS        PIC X(02).
           05  HV-PAYER-ID            PIC X(10).
           05  HV-PAYER-NAME          PIC X(30).
           05  HV-PAYER-TYPE          PIC X(02).
           05  HV-PROVIDER-NPI        PIC X(10).
           05  HV-PROVIDER-TAX-ID     PIC X(09).
           05  HV-PROVIDER-NAME       PIC X(35).
           05  HV-PROVIDER-SPEC       PIC X(03).
           05  HV-MEMBER-ID           PIC X(12).
           05  HV-RECEIPT-DATE        PIC X(10).
           05  HV-SERVICE-DATE        PIC X(10).
           05  HV-PROCESS-DATE        PIC X(10).
           05  HV-PAID-DATE           PIC X(10).
           05  HV-BILLED-AMT          PIC S9(11)V99 COMP-3.
           05  HV-ALLOWED-AMT         PIC S9(11)V99 COMP-3.
           05  HV-PAID-AMT            PIC S9(11)V99 COMP-3.
           05  HV-WITHHOLD-AMT        PIC S9(09)V99 COMP-3.
           05  HV-DEDUCT-AMT          PIC S9(09)V99 COMP-3.
           05  HV-COPAY-AMT           PIC S9(07)V99 COMP-3.
           05  HV-COINS-AMT           PIC S9(09)V99 COMP-3.
           05  HV-DENIAL-CODE         PIC X(05).
           05  HV-DENIAL-DESC         PIC X(40).
           05  HV-DENIAL-CATEGORY     PIC X(03).
           05  HV-PAYMENT-METHOD      PIC X(01).
           05  HV-FACILITY-ID         PIC X(10).
           05  HV-FACILITY-NAME       PIC X(35).
           05  HV-DIAG-CODE           PIC X(07).
           05  HV-PROC-CODE           PIC X(05).
           05  HV-DAYS-AGED           PIC S9(05)   COMP-3.
           05  HV-COUNT               PIC S9(09)   COMP.
           05  HV-SUM-AMT             PIC S9(15)V99 COMP-3.
           05  HV-AVG-AMT             PIC S9(11)V99 COMP-3.
           05  HV-TOTAL-DAYS          PIC S9(11)   COMP.
           05  HV-BUCKET-NBR          PIC S9(02)   COMP-3.
           05  HV-QUEUE-CODE          PIC X(03).
           05  HV-QUEUE-DESC          PIC X(25).
           05  HV-EXAMINER-ID         PIC X(08).
           05  HV-EXPECTED-DATE       PIC X(10).
           05  HV-AUTH-ID             PIC X(15).
           05  HV-AUTH-STATUS         PIC X(02).
           05  HV-AUTH-TYPE           PIC X(02).
           05  HV-MATCH-TYPE          PIC X(03).
           05  HV-MATCH-CLAIM1        PIC X(15).
           05  HV-MATCH-CLAIM2        PIC X(15).
           05  HV-MEASURE-ID          PIC X(08).
           05  HV-MEASURE-DESC        PIC X(40).
           05  HV-NUMERATOR           PIC S9(09)   COMP.
           05  HV-DENOMINATOR         PIC S9(09)   COMP.
           05  HV-BENCHMARK           PIC S9(05)V99 COMP-3.
           05  HV-ALERT-TYPE          PIC X(03).
           05  HV-RISK-SCORE          PIC S9(03)V99 COMP-3.
           05  HV-STATE-CODE          PIC X(02).
           05  HV-PRODUCT-LINE        PIC X(05).
           05  HV-MONTH-NUM           PIC S9(02)   COMP-3.
           05  HV-CASE-MGMT-FLAG      PIC X(01).
           05  HV-REINS-FLAG          PIC X(01).
           05  HV-PROJECTED-COST      PIC S9(13)V99 COMP-3.
           05  HV-STAR-RATING         PIC S9(01)   COMP-3.
           05  HV-RPT-START-DATE      PIC X(10).
           05  HV-RPT-END-DATE        PIC X(10).
           05  HV-PRIOR-START-DATE    PIC X(10).
           05  HV-PRIOR-END-DATE      PIC X(10).
           05  HV-YTD-START-DATE      PIC X(10).

      *---------------------------------------------------------------*
      *    SQL NULL INDICATORS                                        *
      *---------------------------------------------------------------*
       01  WS-SQL-INDICATORS.
           05  WS-IND-PAID-AMT        PIC S9(04) COMP VALUE 0.
           05  WS-IND-ALLOWED-AMT     PIC S9(04) COMP VALUE 0.
           05  WS-IND-WITHHOLD-AMT    PIC S9(04) COMP VALUE 0.
           05  WS-IND-DENIAL-CODE     PIC S9(04) COMP VALUE 0.
           05  WS-IND-DENIAL-DESC     PIC S9(04) COMP VALUE 0.
           05  WS-IND-PAYMENT-METHOD  PIC S9(04) COMP VALUE 0.
           05  WS-IND-PROCESS-DATE    PIC S9(04) COMP VALUE 0.
           05  WS-IND-PAID-DATE       PIC S9(04) COMP VALUE 0.
           05  WS-IND-FACILITY-NAME   PIC S9(04) COMP VALUE 0.
           05  WS-IND-PROJECTED-COST  PIC S9(04) COMP VALUE 0.

      *---------------------------------------------------------------*
      *    REPORT 01 - CLAIMS AGING REPORT FORMAT LINES              *
      *---------------------------------------------------------------*
       01  WS-RPT01-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R01H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R01H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT01-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'RPT01 - CLAIMS AGING ANALYSIS REPORT'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'TIME: '.
           05  WS-R01H2-TIME          PIC X(08).
           05  FILLER                  PIC X(27) VALUE SPACES.

       01  WS-RPT01-HEADER3.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(08) VALUE 'PERIOD: '.
           05  WS-R01H3-START         PIC X(10).
           05  FILLER                  PIC X(04) VALUE ' TO '.
           05  WS-R01H3-END           PIC X(10).
           05  FILLER                  PIC X(99) VALUE SPACES.

       01  WS-RPT01-COL-HDR.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE 'PAYER/TYPE  '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE '    0-30 DAYS '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE '   31-60 DAYS '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE '   61-90 DAYS '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE '  91-120 DAYS '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE ' 121-180 DAYS '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE '    181+ DAYS '.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  FILLER                  PIC X(14) VALUE '  TOTAL       '.
           05  FILLER                  PIC X(06) VALUE SPACES.

       01  WS-RPT01-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D-PAYER          PIC X(12).
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D-BKT1-CNT       PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D-BKT1-AMT       PIC $$$,$$$,$$9.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D-BKT2-CNT       PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D-BKT2-AMT       PIC $$$,$$$,$$9.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D-BKT3-CNT       PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D-BKT3-AMT       PIC $$$,$$$,$$9.
           05  FILLER                  PIC X(01) VALUE SPACES.

       01  WS-RPT01-DETAIL2.
           05  FILLER                  PIC X(15) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D2-BKT4-CNT      PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D2-BKT4-AMT      PIC $$$,$$$,$$9.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D2-BKT5-CNT      PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D2-BKT5-AMT      PIC $$$,$$$,$$9.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D2-BKT6-CNT      PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D2-BKT6-AMT      PIC $$$,$$$,$$9.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01D2-TOTAL-CNT     PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R01D2-TOTAL-AMT     PIC $$$$,$$$,$$9.
           05  FILLER                  PIC X(05) VALUE SPACES.

       01  WS-RPT01-PCT-LINE.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(12)
               VALUE '  % OF TOTAL'.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01P-BKT1-PCT       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01P-BKT2-PCT       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01P-BKT3-PCT       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01P-BKT4-PCT       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01P-BKT5-PCT       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R01P-BKT6-PCT       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(07) VALUE SPACES.

       01  WS-RPT01-TREND-LINE.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(14)
               VALUE ' PRIOR PERIOD '.
           05  WS-R01T-PRIOR-AMT      PIC $$$$,$$$,$$$,$$9.99.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE 'VARIANCE: '.
           05  WS-R01T-VARIANCE       PIC -(9)9,$$$,$$9.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-R01T-VAR-PCT        PIC --9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(40) VALUE SPACES.

      *---------------------------------------------------------------*
      *    REPORT 02 - PROVIDER PAYMENT SUMMARY FORMAT LINES         *
      *---------------------------------------------------------------*
       01  WS-RPT02-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R02H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R02H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT02-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'RPT02 - PROVIDER PAYMENT SUMMARY'.
           05  FILLER                  PIC X(81) VALUE SPACES.

       01  WS-RPT02-COL-HDR.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE 'RANK'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE 'NPI       '.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(25) VALUE
               'PROVIDER NAME            '.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'CLAIMS'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(14) VALUE
               '  TOTAL BILLED'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(14) VALUE
               ' TOTAL ALLOWED'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(14) VALUE
               '    TOTAL PAID'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE
               '    WITHHOLD'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(14) VALUE
               '   NET PAYMENT'.
           05  FILLER                  PIC X(10) VALUE SPACES.

       01  WS-RPT02-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-RANK           PIC ZZ9.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  WS-R02D-NPI            PIC X(10).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-NAME           PIC X(25).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-CLM-CNT        PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-BILLED         PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-ALLOWED        PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-PAID           PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-WITHHOLD       PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R02D-NET            PIC $$$,$$$,$$9.99.

       01  WS-RPT02-SUBDETAIL.
           05  FILLER                  PIC X(06) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE 'AVG PMT:  '.
           05  WS-R02S-AVG            PIC $$$,$$9.99.
           05  FILLER                  PIC X(04) VALUE '   |'.
           05  FILLER                  PIC X(06) VALUE ' YTD: '.
           05  WS-R02S-YTD            PIC $$$$,$$$,$$9.99.
           05  FILLER                  PIC X(04) VALUE '   |'.
           05  FILLER                  PIC X(08) VALUE ' PRIOR: '.
           05  WS-R02S-PRIOR          PIC $$$$,$$$,$$9.99.
           05  FILLER                  PIC X(04) VALUE '   |'.
           05  FILLER                  PIC X(06) VALUE ' CHK: '.
           05  WS-R02S-CHK-CNT        PIC ZZ,ZZ9.
           05  FILLER                  PIC X(06) VALUE ' EFT: '.
           05  WS-R02S-EFT-CNT        PIC ZZ,ZZ9.
           05  FILLER                  PIC X(14) VALUE SPACES.

      *---------------------------------------------------------------*
      *    REPORT 03 - PAYER MIX FORMAT LINES                        *
      *---------------------------------------------------------------*
       01  WS-RPT03-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R03H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R03H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT03-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'RPT03 - PAYER MIX ANALYSIS REPORT'.
           05  FILLER                  PIC X(81) VALUE SPACES.

       01  WS-RPT03-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-PAYER-NAME     PIC X(30).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-TYPE           PIC X(12).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-REVENUE        PIC $$$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-REV-PCT        PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-CLM-CNT        PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-AVG-REIMB      PIC Z9.9999.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-DENY-RATE      PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R03D-COLL-RATE      PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(09) VALUE SPACES.

      *---------------------------------------------------------------*
      *    REPORT 04 - DENIAL ANALYSIS FORMAT LINES                  *
      *---------------------------------------------------------------*
       01  WS-RPT04-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R04H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R04H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT04-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'RPT04 - DENIAL ANALYSIS REPORT'.
           05  FILLER                  PIC X(81) VALUE SPACES.

       01  WS-RPT04-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-CODE           PIC X(05).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-DESC           PIC X(35).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-CAT            PIC X(10).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-COUNT          PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-AMOUNT         PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-RATE           PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-APPEAL         PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-OVERTURN       PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R04D-TREND          PIC X(05).
           05  FILLER                  PIC X(05) VALUE SPACES.

      *---------------------------------------------------------------*
      *    REPORT 05 - FINANCIAL SUMMARY FORMAT LINES                *
      *---------------------------------------------------------------*
       01  WS-RPT05-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R05H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R05H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT05-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(55)
               VALUE 'RPT05 - EXECUTIVE FINANCIAL SUMMARY DASHBOARD'.
           05  FILLER                  PIC X(76) VALUE SPACES.

       01  WS-RPT05-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R05D-LABEL          PIC X(35).
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R05D-MTD-AMT        PIC $$$,$$$,$$$,$$9.99.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R05D-YTD-AMT        PIC $$$,$$$,$$$,$$9.99.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R05D-BUDGET-AMT     PIC $$$,$$$,$$$,$$9.99.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R05D-VARIANCE       PIC -$$,$$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R05D-VAR-PCT        PIC --9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(03) VALUE SPACES.

       01  WS-RPT05-RATIO-LINE.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R05R-LABEL          PIC X(35).
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R05R-MTD-RATIO      PIC ZZ9.9999.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '| '.
           05  WS-R05R-YTD-RATIO      PIC ZZ9.9999.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(61) VALUE SPACES.

      *---------------------------------------------------------------*
      *    REPORT 06-12 HEADER/DETAIL LINES (COMPACT DEFINITIONS)    *
      *---------------------------------------------------------------*
       01  WS-RPT06-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R06H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R06H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT06-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'RPT06 - PEND QUEUE AGING REPORT'.
           05  FILLER                  PIC X(81) VALUE SPACES.

       01  WS-RPT06-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R06D-QUEUE          PIC X(25).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R06D-TOTAL-CNT      PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R06D-TOTAL-AMT      PIC $$$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R06D-AVG-DAYS       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R06D-PAST-DUE       PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R06D-AUTO-CAND      PIC ZZ,ZZ9.
           05  FILLER                  PIC X(38) VALUE SPACES.

       01  WS-RPT07-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R07H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R07H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT07-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(55)
               VALUE 'RPT07 - AUTHORIZATION UTILIZATION REPORT'.
           05  FILLER                  PIC X(76) VALUE SPACES.

       01  WS-RPT07-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07D-LABEL          PIC X(35).
           05  FILLER                  PIC X(02) VALUE ': '.
           05  WS-R07D-VALUE          PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  WS-R07D-RATE           PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(69) VALUE SPACES.

       01  WS-RPT07-SPEC-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07S-SPEC           PIC X(25).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07S-REQUESTS       PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07S-APPROVED       PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07S-DENIED         PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07S-APP-RATE       PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R07S-AVG-TAT        PIC ZZ9.99.
           05  FILLER                  PIC X(47) VALUE SPACES.

       01  WS-RPT08-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R08H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R08H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT08-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(55)
               VALUE 'RPT08 - HIGH-DOLLAR CLAIMS TRACKING REPORT'.
           05  FILLER                  PIC X(76) VALUE SPACES.

       01  WS-RPT08-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-CLAIM-ID       PIC X(15).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-MEMBER         PIC X(12).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-PROVIDER       PIC X(20).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-DIAG           PIC X(07).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-BILLED         PIC $$$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-PAID           PIC $$$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-STATUS         PIC X(02).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-CASE           PIC X(01).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R08D-REINS          PIC X(01).
           05  FILLER                  PIC X(20) VALUE SPACES.

       01  WS-RPT09-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R09H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R09H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT09-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(55)
               VALUE 'RPT09 - DUPLICATE CLAIMS DETECTION REPORT'.
           05  FILLER                  PIC X(76) VALUE SPACES.

       01  WS-RPT09-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R09D-CLM1           PIC X(15).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R09D-CLM2           PIC X(15).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R09D-MATCH          PIC X(20).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R09D-AMOUNT         PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(60) VALUE SPACES.

       01  WS-RPT10-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R10H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R10H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT10-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(55)
               VALUE 'RPT10 - QUALITY METRICS DASHBOARD'.
           05  FILLER                  PIC X(76) VALUE SPACES.

       01  WS-RPT10-HEDIS-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-MEASURE        PIC X(08).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-DESC           PIC X(35).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-NUMER          PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE '/'.
           05  WS-R10D-DENOM          PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-RATE           PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-BENCH          PIC ZZ9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-STAR           PIC 9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R10D-VARIANCE       PIC --9.99.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(05) VALUE SPACES.

       01  WS-RPT11-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R11H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R11H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT11-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(60)
               VALUE 'RPT11 - FRAUD, WASTE & ABUSE INDICATOR REPORT'.
           05  FILLER                  PIC X(71) VALUE SPACES.

       01  WS-RPT11-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R11D-NPI            PIC X(10).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R11D-NAME           PIC X(25).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R11D-ALERT-TYPE     PIC X(15).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R11D-ALERT-CNT      PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R11D-AMOUNT         PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R11D-RISK           PIC ZZ9.99.
           05  FILLER                  PIC X(26) VALUE SPACES.

       01  WS-RPT12-HEADER1.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'DATE: '.
           05  WS-R12H1-DATE          PIC X(10).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'PAGE: '.
           05  WS-R12H1-PAGE          PIC Z,ZZ9.
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  WS-RPT12-HEADER2.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(55)
               VALUE 'RPT12 - REGULATORY COMPLIANCE REPORT'.
           05  FILLER                  PIC X(76) VALUE SPACES.

       01  WS-RPT12-DETAIL.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-R12D-LABEL          PIC X(40).
           05  FILLER                  PIC X(02) VALUE ': '.
           05  WS-R12D-VALUE          PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  WS-R12D-RATE           PIC ZZ9.9999.
           05  FILLER                  PIC X(01) VALUE '%'.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  WS-R12D-STATUS         PIC X(10).
           05  FILLER                  PIC X(41) VALUE SPACES.

      *---------------------------------------------------------------*
      *    COMMON SEPARATOR/DIVIDER LINES                             *
      *---------------------------------------------------------------*
       01  WS-SEPARATOR-LINE          PIC X(132) VALUE ALL '-'.
       01  WS-DOUBLE-SEP-LINE         PIC X(132) VALUE ALL '='.
       01  WS-BLANK-LINE              PIC X(132) VALUE SPACES.

      *---------------------------------------------------------------*
      *    ERROR AND AUDIT LOG RECORD LAYOUTS                         *
      *---------------------------------------------------------------*
       01  WS-ERROR-LOG-REC.
           05  WS-ERR-TIMESTAMP       PIC X(26).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-ERR-SEVERITY        PIC X(01).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-ERR-PROGRAM         PIC X(08).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-ERR-PARAGRAPH       PIC X(30).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-ERR-SQLCODE         PIC -(5)9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-ERR-MESSAGE         PIC X(118).

       01  WS-AUDIT-LOG-REC.
           05  WS-AUD-TIMESTAMP       PIC X(26).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-AUD-EVENT-TYPE      PIC X(10).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-AUD-REPORT-ID       PIC X(05).
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-AUD-RECORD-COUNT    PIC Z(8)9.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  WS-AUD-MESSAGE         PIC X(137).

      *---------------------------------------------------------------*
      *    TIMESTAMP WORK AREA                                        *
      *---------------------------------------------------------------*
       01  WS-TIMESTAMP-AREA.
           05  WS-TS-DATE             PIC X(10).
           05  WS-TS-DASH1            PIC X(01) VALUE '-'.
           05  WS-TS-TIME             PIC X(08).
           05  WS-TS-DOT              PIC X(01) VALUE '.'.
           05  WS-TS-MICRO            PIC X(06) VALUE '000000'.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *================================================================*
      *    0000-MAIN-CONTROL                                           *
      *    MASTER CONTROL PARAGRAPH - ORCHESTRATES ALL PROCESSING      *
      *================================================================*
       0000-MAIN-CONTROL.

           PERFORM 1000-INITIALIZATION

           IF PROGRAM-OK
               PERFORM 2000-DETERMINE-REPORT-PERIOD
           END-IF

           IF PROGRAM-OK
               PERFORM 2100-LOAD-REPORT-PARAMETERS
           END-IF

           IF PROGRAM-OK
               PERFORM 3000-GENERATE-REPORTS
           END-IF

           PERFORM 9000-TERMINATION

           STOP RUN.

      *================================================================*
      *    1000-INITIALIZATION                                         *
      *    OPEN ALL FILES, INITIALIZE TABLES, GET CURRENT DATE/TIME    *
      *================================================================*
       1000-INITIALIZATION.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
                                         WS-CURRENT-TIME

           STRING WS-CURR-YYYY '-' WS-CURR-MM '-' WS-CURR-DD
               DELIMITED BY SIZE
               INTO WS-REPORT-DATE-DISP

           STRING WS-CURR-HH ':' WS-CURR-MN ':' WS-CURR-SS
               DELIMITED BY SIZE
               INTO WS-REPORT-TIME-DISP

           MOVE WS-REPORT-DATE-DISP TO WS-TS-DATE
           MOVE WS-REPORT-TIME-DISP TO WS-TS-TIME

           DISPLAY 'HCRPTGEN - INITIALIZATION STARTED AT '
               WS-REPORT-DATE-DISP ' ' WS-REPORT-TIME-DISP

           INITIALIZE WS-AGING-TABLE
           INITIALIZE WS-AGING-CLMTYPE-TABLE
           INITIALIZE WS-AGING-GRAND-TOTALS
           INITIALIZE WS-PRIOR-AGING-TOTALS
           INITIALIZE WS-PROVIDER-TABLE
           INITIALIZE WS-PAYER-MIX-TABLE
           INITIALIZE WS-PRODUCT-LINE-TABLE
           INITIALIZE WS-DENIAL-TABLE
           INITIALIZE WS-DENIAL-PAYER-TABLE
           INITIALIZE WS-DENIAL-CATEGORY-TABLE
           INITIALIZE WS-FINANCIAL-SUMMARY
           INITIALIZE WS-PEND-TABLE
           INITIALIZE WS-AUTH-TABLE
           INITIALIZE WS-HIGH-DOLLAR-TABLE
           INITIALIZE WS-DUPLICATE-TABLE
           INITIALIZE WS-QUALITY-TABLE
           INITIALIZE WS-FWA-TABLE
           INITIALIZE WS-COMPLIANCE-TABLE
           INITIALIZE WS-SPECIALTY-TABLE
           INITIALIZE WS-MONTHLY-TREND-TABLE

           PERFORM 1100-INIT-BUCKET-RANGES
           PERFORM 1200-INIT-CLAIM-TYPES
           PERFORM 1300-INIT-DENIAL-CATEGORIES
           PERFORM 1400-INIT-PEND-QUEUES
           PERFORM 1500-INIT-HIGH-DOLLAR-THRESHOLDS
           PERFORM 1600-INIT-PRODUCT-LINES
           PERFORM 1700-INIT-DUPLICATE-CRITERIA

           PERFORM 1800-OPEN-FILES

           IF PROGRAM-OK
               STRING WS-TS-DATE WS-TS-DASH1 WS-TS-TIME
                      WS-TS-DOT WS-TS-MICRO
                   DELIMITED BY SIZE INTO WS-AUD-TIMESTAMP
               MOVE 'START     ' TO WS-AUD-EVENT-TYPE
               MOVE 'INIT ' TO WS-AUD-REPORT-ID
               MOVE 0 TO WS-AUD-RECORD-COUNT
               MOVE 'REPORT GENERATION BATCH INITIATED'
                   TO WS-AUD-MESSAGE
               WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
               ADD 1 TO WS-AUDIT-RECORDS
           END-IF

           DISPLAY 'HCRPTGEN - INITIALIZATION COMPLETE'.

      *================================================================*
      *    1100-INIT-BUCKET-RANGES                                     *
      *    SET UP THE 6 AGING BUCKET RANGE DEFINITIONS                 *
      *================================================================*
       1100-INIT-BUCKET-RANGES.

           MOVE 0     TO WS-BKT-LOW(1)
           MOVE 30    TO WS-BKT-HIGH(1)
           MOVE '0-30 DAYS ' TO WS-BKT-LABEL(1)

           MOVE 31    TO WS-BKT-LOW(2)
           MOVE 60    TO WS-BKT-HIGH(2)
           MOVE '31-60 DAYS' TO WS-BKT-LABEL(2)

           MOVE 61    TO WS-BKT-LOW(3)
           MOVE 90    TO WS-BKT-HIGH(3)
           MOVE '61-90 DAYS' TO WS-BKT-LABEL(3)

           MOVE 91    TO WS-BKT-LOW(4)
           MOVE 120   TO WS-BKT-HIGH(4)
           MOVE '91-120DAYS' TO WS-BKT-LABEL(4)

           MOVE 121   TO WS-BKT-LOW(5)
           MOVE 180   TO WS-BKT-HIGH(5)
           MOVE '121-180DAY' TO WS-BKT-LABEL(5)

           MOVE 181   TO WS-BKT-LOW(6)
           MOVE 9999  TO WS-BKT-HIGH(6)
           MOVE '181+ DAYS ' TO WS-BKT-LABEL(6).

      *================================================================*
      *    1200-INIT-CLAIM-TYPES                                       *
      *    INITIALIZE THE 4 CLAIM TYPE DESCRIPTIONS                    *
      *================================================================*
       1200-INIT-CLAIM-TYPES.

           MOVE 'IN' TO WS-AGC-TYPE-CODE(1)
           MOVE 'INPATIENT           ' TO WS-AGC-TYPE-DESC(1)

           MOVE 'OP' TO WS-AGC-TYPE-CODE(2)
           MOVE 'OUTPATIENT          ' TO WS-AGC-TYPE-DESC(2)

           MOVE 'PR' TO WS-AGC-TYPE-CODE(3)
           MOVE 'PROFESSIONAL        ' TO WS-AGC-TYPE-DESC(3)

           MOVE 'RX' TO WS-AGC-TYPE-CODE(4)
           MOVE 'PHARMACY            ' TO WS-AGC-TYPE-DESC(4).

      *================================================================*
      *    1300-INIT-DENIAL-CATEGORIES                                 *
      *    INITIALIZE DENIAL ROOT CAUSE CATEGORIES                     *
      *================================================================*
       1300-INIT-DENIAL-CATEGORIES.

           MOVE 'ELG' TO WS-DCAT-CODE(1)
           MOVE 'ELIGIBILITY              ' TO WS-DCAT-DESC(1)

           MOVE 'ATH' TO WS-DCAT-CODE(2)
           MOVE 'AUTHORIZATION            ' TO WS-DCAT-DESC(2)

           MOVE 'COD' TO WS-DCAT-CODE(3)
           MOVE 'CODING ERRORS            ' TO WS-DCAT-DESC(3)

           MOVE 'TMF' TO WS-DCAT-CODE(4)
           MOVE 'TIMELY FILING            ' TO WS-DCAT-DESC(4)

           MOVE 'MNC' TO WS-DCAT-CODE(5)
           MOVE 'MEDICAL NECESSITY        ' TO WS-DCAT-DESC(5)

           MOVE 'BND' TO WS-DCAT-CODE(6)
           MOVE 'BUNDLING/UNBUNDLING      ' TO WS-DCAT-DESC(6)

           MOVE 'DUP' TO WS-DCAT-CODE(7)
           MOVE 'DUPLICATE CLAIM          ' TO WS-DCAT-DESC(7)

           MOVE 'COB' TO WS-DCAT-CODE(8)
           MOVE 'COORDINATION OF BENEFITS ' TO WS-DCAT-DESC(8)

           MOVE 'BEN' TO WS-DCAT-CODE(9)
           MOVE 'BENEFIT LIMITATION       ' TO WS-DCAT-DESC(9)

           MOVE 'OTH' TO WS-DCAT-CODE(10)
           MOVE 'OTHER/MISCELLANEOUS      ' TO WS-DCAT-DESC(10).

      *================================================================*
      *    1400-INIT-PEND-QUEUES                                       *
      *    INITIALIZE PEND QUEUE DESCRIPTIONS                          *
      *================================================================*
       1400-INIT-PEND-QUEUES.

           MOVE 'MRV' TO WS-PQ-QUEUE-CODE(1)
           MOVE 'MEDICAL REVIEW           ' TO WS-PQ-QUEUE-DESC(1)

           MOVE 'PRC' TO WS-PQ-QUEUE-CODE(2)
           MOVE 'PRICING/REPRICING        ' TO WS-PQ-QUEUE-DESC(2)

           MOVE 'ATH' TO WS-PQ-QUEUE-CODE(3)
           MOVE 'AUTHORIZATION VERIFY     ' TO WS-PQ-QUEUE-DESC(3)

           MOVE 'COB' TO WS-PQ-QUEUE-CODE(4)
           MOVE 'COB INVESTIGATION        ' TO WS-PQ-QUEUE-DESC(4)

           MOVE 'INF' TO WS-PQ-QUEUE-CODE(5)
           MOVE 'INFORMATION REQUEST      ' TO WS-PQ-QUEUE-DESC(5)

           MOVE 'CRD' TO WS-PQ-QUEUE-CODE(6)
           MOVE 'CREDENTIALING HOLD       ' TO WS-PQ-QUEUE-DESC(6)

           MOVE 'SUP' TO WS-PQ-QUEUE-CODE(7)
           MOVE 'SUPERVISOR REVIEW        ' TO WS-PQ-QUEUE-DESC(7)

           MOVE 'SIU' TO WS-PQ-QUEUE-CODE(8)
           MOVE 'SPECIAL INVESTIGATION    ' TO WS-PQ-QUEUE-DESC(8).

      *================================================================*
      *    1500-INIT-HIGH-DOLLAR-THRESHOLDS                            *
      *    SET UP DOLLAR THRESHOLD TIERS FOR HIGH-DOLLAR REPORTING     *
      *================================================================*
       1500-INIT-HIGH-DOLLAR-THRESHOLDS.

           MOVE 25000.00    TO WS-HD-THR-AMOUNT(1)
           MOVE '$25K+       ' TO WS-HD-THR-LABEL(1)

           MOVE 50000.00    TO WS-HD-THR-AMOUNT(2)
           MOVE '$50K+       ' TO WS-HD-THR-LABEL(2)

           MOVE 100000.00   TO WS-HD-THR-AMOUNT(3)
           MOVE '$100K+      ' TO WS-HD-THR-LABEL(3)

           MOVE 250000.00   TO WS-HD-THR-AMOUNT(4)
           MOVE '$250K+      ' TO WS-HD-THR-LABEL(4)

           MOVE 500000.00   TO WS-HD-THR-AMOUNT(5)
           MOVE '$500K+      ' TO WS-HD-THR-LABEL(5)

           MOVE 1000000.00  TO WS-HD-THR-AMOUNT(6)
           MOVE '$1M+        ' TO WS-HD-THR-LABEL(6).

      *================================================================*
      *    1600-INIT-PRODUCT-LINES                                     *
      *    INITIALIZE PRODUCT LINE DESCRIPTIONS                        *
      *================================================================*
       1600-INIT-PRODUCT-LINES.

           MOVE 'HMO  ' TO WS-PL-CODE(1)
           MOVE 'HMO                 ' TO WS-PL-DESC(1)

           MOVE 'PPO  ' TO WS-PL-CODE(2)
           MOVE 'PPO                 ' TO WS-PL-DESC(2)

           MOVE 'POS  ' TO WS-PL-CODE(3)
           MOVE 'POS                 ' TO WS-PL-DESC(3)

           MOVE 'EPO  ' TO WS-PL-CODE(4)
           MOVE 'EPO                 ' TO WS-PL-DESC(4)

           MOVE 'HDHP ' TO WS-PL-CODE(5)
           MOVE 'HIGH DEDUCTIBLE     ' TO WS-PL-DESC(5)

           MOVE 'IND  ' TO WS-PL-CODE(6)
           MOVE 'INDEMNITY           ' TO WS-PL-DESC(6)

           MOVE 'OTH  ' TO WS-PL-CODE(7)
           MOVE 'OTHER               ' TO WS-PL-DESC(7).

      *================================================================*
      *    1700-INIT-DUPLICATE-CRITERIA                                *
      *    INITIALIZE DUPLICATE MATCH CRITERIA DESCRIPTIONS            *
      *================================================================*
       1700-INIT-DUPLICATE-CRITERIA.

           MOVE 'EXACT MATCH (ALL)   ' TO WS-DUBC-CRITERIA(1)
           MOVE 'MEMBER+DOS+PROC     ' TO WS-DUBC-CRITERIA(2)
           MOVE 'MEMBER+DOS+AMOUNT   ' TO WS-DUBC-CRITERIA(3)
           MOVE 'PROV+MEMBER+DOS     ' TO WS-DUBC-CRITERIA(4)
           MOVE 'NEAR-DUPLICATE      ' TO WS-DUBC-CRITERIA(5).

      *================================================================*
      *    1800-OPEN-FILES                                             *
      *    OPEN ALL INPUT AND OUTPUT FILES                             *
      *================================================================*
       1800-OPEN-FILES.

           OPEN INPUT RPTCTRL-FILE
           IF WS-RPTCTRL-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPTCTRL FILE: '
                   WS-RPTCTRL-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT ERROR-FILE
           IF WS-ERROR-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING ERROR FILE: '
                   WS-ERROR-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT AUDIT-FILE
           IF WS-AUDIT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING AUDIT FILE: '
                   WS-AUDIT-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT01-FILE
           IF WS-RPT01-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT01 FILE: '
                   WS-RPT01-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT02-FILE
           IF WS-RPT02-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT02 FILE: '
                   WS-RPT02-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT03-FILE
           IF WS-RPT03-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT03 FILE: '
                   WS-RPT03-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT04-FILE
           IF WS-RPT04-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT04 FILE: '
                   WS-RPT04-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT05-FILE
           IF WS-RPT05-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT05 FILE: '
                   WS-RPT05-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT06-FILE
           IF WS-RPT06-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT06 FILE: '
                   WS-RPT06-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT07-FILE
           IF WS-RPT07-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT07 FILE: '
                   WS-RPT07-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT08-FILE
           IF WS-RPT08-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT08 FILE: '
                   WS-RPT08-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT09-FILE
           IF WS-RPT09-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT09 FILE: '
                   WS-RPT09-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT10-FILE
           IF WS-RPT10-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT10 FILE: '
                   WS-RPT10-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT11-FILE
           IF WS-RPT11-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT11 FILE: '
                   WS-RPT11-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF

           OPEN OUTPUT RPT12-FILE
           IF WS-RPT12-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RPT12 FILE: '
                   WS-RPT12-STATUS
               SET PROGRAM-ABEND TO TRUE
           END-IF.

      *================================================================*
      *    2000-DETERMINE-REPORT-PERIOD                                *
      *    CALCULATE REPORTING DATE RANGES BASED ON PERIOD TYPE        *
      *    HANDLES DAILY/WEEKLY/MONTHLY/QUARTERLY/ANNUAL               *
      *    CALCULATES PRIOR PERIOD FOR TREND COMPARISON                *
      *    DETERMINES FISCAL VS CALENDAR YEAR BOUNDARIES               *
      *================================================================*
       2000-DETERMINE-REPORT-PERIOD.

           DISPLAY 'HCRPTGEN - DETERMINING REPORT PERIOD'

      *    CHECK FOR DATE OVERRIDES FIRST
           IF WS-OVERRIDE-START NOT = SPACES
               AND WS-OVERRIDE-END NOT = SPACES
               MOVE WS-OVERRIDE-START TO WS-RPT-START-DATE
               MOVE WS-OVERRIDE-END   TO WS-RPT-END-DATE
           ELSE
      *        DEFAULT: CALCULATE BASED ON PERIOD TYPE
               EVALUATE TRUE
                   WHEN PERIOD-DAILY
      *                USE PRIOR BUSINESS DAY
                       STRING WS-CURR-YYYY '-' WS-CURR-MM '-'
                              WS-CURR-DD
                           DELIMITED BY SIZE
                           INTO WS-RPT-START-DATE
                       MOVE WS-RPT-START-DATE TO WS-RPT-END-DATE

                   WHEN PERIOD-WEEKLY
      *                CALCULATE MONDAY-SUNDAY OF PRIOR WEEK
                       COMPUTE WS-WORK-DAYS =
                           FUNCTION INTEGER-OF-DATE(
                               WS-CURR-YYYY * 10000 +
                               WS-CURR-MM * 100 +
                               WS-CURR-DD)
      *                FIND PRIOR MONDAY (DAY 1 = MON IN ISO)
                       COMPUTE WS-WORK-DAYS =
                           WS-WORK-DAYS -
                           FUNCTION MOD(WS-WORK-DAYS - 1, 7) - 7
                       COMPUTE WS-PERIOD-START-INT =
                           FUNCTION DATE-OF-INTEGER(WS-WORK-DAYS)
                       COMPUTE WS-PERIOD-END-INT =
                           FUNCTION DATE-OF-INTEGER(
                               WS-WORK-DAYS + 6)
                       STRING WS-PERIOD-START-INT(1:4) '-'
                              WS-PERIOD-START-INT(5:2) '-'
                              WS-PERIOD-START-INT(7:2)
                           DELIMITED BY SIZE
                           INTO WS-RPT-START-DATE
                       STRING WS-PERIOD-END-INT(1:4) '-'
                              WS-PERIOD-END-INT(5:2) '-'
                              WS-PERIOD-END-INT(7:2)
                           DELIMITED BY SIZE
                           INTO WS-RPT-END-DATE

                   WHEN PERIOD-MONTHLY
      *                PRIOR FULL MONTH
                       IF WS-CURR-MM = 1
                           COMPUTE WS-WORK-INDEX =
                               WS-CURR-YYYY - 1
                           MOVE 12 TO WS-MONTH-INDEX
                       ELSE
                           MOVE WS-CURR-YYYY TO WS-WORK-INDEX
                           COMPUTE WS-MONTH-INDEX =
                               WS-CURR-MM - 1
                       END-IF
                       STRING WS-WORK-INDEX '-'
                              WS-MONTH-INDEX '-01'
                           DELIMITED BY SIZE
                           INTO WS-RPT-START-DATE
      *                DETERMINE LAST DAY OF MONTH
                       EVALUATE WS-MONTH-INDEX
                           WHEN 1 WHEN 3 WHEN 5 WHEN 7
                           WHEN 8 WHEN 10 WHEN 12
                               MOVE 31 TO WS-DAYS-IN-MONTH
                           WHEN 4 WHEN 6 WHEN 9 WHEN 11
                               MOVE 30 TO WS-DAYS-IN-MONTH
                           WHEN 2
                               IF FUNCTION MOD(WS-WORK-INDEX, 4)
                                   = 0
                                   AND (FUNCTION MOD(
                                       WS-WORK-INDEX, 100)
                                       NOT = 0
                                   OR FUNCTION MOD(
                                       WS-WORK-INDEX, 400) = 0)
                                   MOVE 29 TO WS-DAYS-IN-MONTH
                               ELSE
                                   MOVE 28 TO WS-DAYS-IN-MONTH
                               END-IF
                       END-EVALUATE
                       STRING WS-WORK-INDEX '-'
                              WS-MONTH-INDEX '-'
                              WS-DAYS-IN-MONTH
                           DELIMITED BY SIZE
                           INTO WS-RPT-END-DATE

                   WHEN PERIOD-QUARTERLY
      *                DETERMINE PRIOR QUARTER
                       EVALUATE WS-CURR-MM
                           WHEN 1 THRU 3
                               MOVE 4 TO WS-QUARTER-NUM
                               COMPUTE WS-WORK-INDEX =
                                   WS-CURR-YYYY - 1
                               STRING WS-WORK-INDEX '-10-01'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-START-DATE
                               STRING WS-WORK-INDEX '-12-31'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-END-DATE
                           WHEN 4 THRU 6
                               MOVE 1 TO WS-QUARTER-NUM
                               MOVE WS-CURR-YYYY TO WS-WORK-INDEX
                               STRING WS-WORK-INDEX '-01-01'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-START-DATE
                               STRING WS-WORK-INDEX '-03-31'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-END-DATE
                           WHEN 7 THRU 9
                               MOVE 2 TO WS-QUARTER-NUM
                               MOVE WS-CURR-YYYY TO WS-WORK-INDEX
                               STRING WS-WORK-INDEX '-04-01'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-START-DATE
                               STRING WS-WORK-INDEX '-06-30'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-END-DATE
                           WHEN 10 THRU 12
                               MOVE 3 TO WS-QUARTER-NUM
                               MOVE WS-CURR-YYYY TO WS-WORK-INDEX
                               STRING WS-WORK-INDEX '-07-01'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-START-DATE
                               STRING WS-WORK-INDEX '-09-30'
                                   DELIMITED BY SIZE
                                   INTO WS-RPT-END-DATE
                       END-EVALUATE

                   WHEN PERIOD-ANNUAL
      *                PRIOR FULL CALENDAR YEAR
                       COMPUTE WS-WORK-INDEX =
                           WS-CURR-YYYY - 1
                       STRING WS-WORK-INDEX '-01-01'
                           DELIMITED BY SIZE
                           INTO WS-RPT-START-DATE
                       STRING WS-WORK-INDEX '-12-31'
                           DELIMITED BY SIZE
                           INTO WS-RPT-END-DATE
               END-EVALUATE
           END-IF

      *    CALCULATE PRIOR PERIOD DATES FOR TREND COMPARISON
           EVALUATE TRUE
               WHEN PERIOD-MONTHLY
      *            GO BACK ONE MORE MONTH
                   IF WS-MONTH-INDEX = 1
                       COMPUTE WS-WORK-INDEX2 =
                           WS-WORK-INDEX - 1
                       MOVE 12 TO WS-WORK-INDEX3
                   ELSE
                       MOVE WS-WORK-INDEX TO WS-WORK-INDEX2
                       COMPUTE WS-WORK-INDEX3 =
                           WS-MONTH-INDEX - 1
                   END-IF
                   STRING WS-WORK-INDEX2 '-'
                          WS-WORK-INDEX3 '-01'
                       DELIMITED BY SIZE
                       INTO WS-PRIOR-START-DATE
                   EVALUATE WS-WORK-INDEX3
                       WHEN 1 WHEN 3 WHEN 5 WHEN 7
                       WHEN 8 WHEN 10 WHEN 12
                           MOVE 31 TO WS-DAYS-IN-MONTH
                       WHEN 4 WHEN 6 WHEN 9 WHEN 11
                           MOVE 30 TO WS-DAYS-IN-MONTH
                       WHEN 2
                           IF FUNCTION MOD(WS-WORK-INDEX2, 4) = 0
                               AND (FUNCTION MOD(
                                   WS-WORK-INDEX2, 100) NOT = 0
                               OR FUNCTION MOD(
                                   WS-WORK-INDEX2, 400) = 0)
                               MOVE 29 TO WS-DAYS-IN-MONTH
                           ELSE
                               MOVE 28 TO WS-DAYS-IN-MONTH
                           END-IF
                   END-EVALUATE
                   STRING WS-WORK-INDEX2 '-'
                          WS-WORK-INDEX3 '-'
                          WS-DAYS-IN-MONTH
                       DELIMITED BY SIZE
                       INTO WS-PRIOR-END-DATE
               WHEN OTHER
      *            DEFAULT: SAME-LENGTH PERIOD IMMEDIATELY PRIOR
                   MOVE WS-RPT-START-DATE TO WS-PRIOR-END-DATE
                   MOVE WS-RPT-START-DATE TO WS-PRIOR-START-DATE
           END-EVALUATE

      *    CALCULATE YTD START DATE (JANUARY 1 OF CURRENT YEAR)
           STRING WS-CURR-YYYY '-01-01'
               DELIMITED BY SIZE
               INTO WS-YTD-START-DATE

      *    CALCULATE FISCAL YEAR START (JULY 1)
           IF WS-CURR-MM >= WS-FISCAL-YEAR-MM
               STRING WS-CURR-YYYY '-07-01'
                   DELIMITED BY SIZE
                   INTO WS-FISCAL-YR-START
           ELSE
               COMPUTE WS-WORK-INDEX = WS-CURR-YYYY - 1
               STRING WS-WORK-INDEX '-07-01'
                   DELIMITED BY SIZE
                   INTO WS-FISCAL-YR-START
           END-IF

      *    CALCULATE 12-MONTH ROLLING START DATE
           IF WS-CURR-MM = 1
               COMPUTE WS-WORK-INDEX = WS-CURR-YYYY - 1
               STRING WS-WORK-INDEX '-02-01'
                   DELIMITED BY SIZE
                   INTO WS-ROLLING-12-START
           ELSE
               COMPUTE WS-WORK-INDEX2 = WS-CURR-MM + 1
               IF WS-WORK-INDEX2 > 12
                   MOVE 1 TO WS-WORK-INDEX2
                   MOVE WS-CURR-YYYY TO WS-WORK-INDEX
               ELSE
                   COMPUTE WS-WORK-INDEX = WS-CURR-YYYY - 1
               END-IF
               STRING WS-WORK-INDEX '-'
                      WS-WORK-INDEX2 '-01'
                   DELIMITED BY SIZE
                   INTO WS-ROLLING-12-START
           END-IF

      *    SET SQL HOST VARIABLES FOR DATE RANGES
           MOVE WS-RPT-START-DATE  TO HV-RPT-START-DATE
           MOVE WS-RPT-END-DATE    TO HV-RPT-END-DATE
           MOVE WS-PRIOR-START-DATE TO HV-PRIOR-START-DATE
           MOVE WS-PRIOR-END-DATE  TO HV-PRIOR-END-DATE
           MOVE WS-YTD-START-DATE  TO HV-YTD-START-DATE

           DISPLAY 'HCRPTGEN - REPORT PERIOD: '
               WS-RPT-START-DATE ' TO ' WS-RPT-END-DATE
           DISPLAY 'HCRPTGEN - PRIOR PERIOD: '
               WS-PRIOR-START-DATE ' TO ' WS-PRIOR-END-DATE.

      *================================================================*
      *    2100-LOAD-REPORT-PARAMETERS                                 *
      *    READ CONTROL FILE FOR REPORT SELECTION AND OVERRIDES        *
      *================================================================*
       2100-LOAD-REPORT-PARAMETERS.

           DISPLAY 'HCRPTGEN - LOADING REPORT PARAMETERS'

           SET NOT-EOF-RPTCTRL TO TRUE

           PERFORM UNTIL EOF-RPTCTRL
               READ RPTCTRL-FILE INTO RPTCTRL-RECORD
                   AT END
                       SET EOF-RPTCTRL TO TRUE
                   NOT AT END
                       ADD 1 TO WS-TOTAL-CTRL-RECS
                       PERFORM 2110-PROCESS-CTRL-RECORD
               END-READ
           END-PERFORM

           DISPLAY 'HCRPTGEN - CONTROL RECORDS READ: '
               WS-TOTAL-CTRL-RECS

      *    LOG REPORT SELECTIONS
           IF RUN-CLAIMS-AGING
               DISPLAY 'HCRPTGEN - RPT01 CLAIMS AGING: SELECTED'
           END-IF
           IF RUN-PROVIDER-PMT
               DISPLAY 'HCRPTGEN - RPT02 PROVIDER PMT: SELECTED'
           END-IF
           IF RUN-PAYER-MIX
               DISPLAY 'HCRPTGEN - RPT03 PAYER MIX: SELECTED'
           END-IF
           IF RUN-DENIAL-ANALYSIS
               DISPLAY 'HCRPTGEN - RPT04 DENIAL ANLYS: SELECTED'
           END-IF
           IF RUN-FINANCIAL-SUMM
               DISPLAY 'HCRPTGEN - RPT05 FINANCIAL: SELECTED'
           END-IF
           IF RUN-PEND-AGING
               DISPLAY 'HCRPTGEN - RPT06 PEND AGING: SELECTED'
           END-IF
           IF RUN-AUTH-UTIL
               DISPLAY 'HCRPTGEN - RPT07 AUTH UTIL: SELECTED'
           END-IF
           IF RUN-HIGH-DOLLAR
               DISPLAY 'HCRPTGEN - RPT08 HIGH-DOLLAR: SELECTED'
           END-IF
           IF RUN-DUPLICATE-DET
               DISPLAY 'HCRPTGEN - RPT09 DUPLICATES: SELECTED'
           END-IF
           IF RUN-QUALITY-MTRC
               DISPLAY 'HCRPTGEN - RPT10 QUALITY: SELECTED'
           END-IF
           IF RUN-FWA-REPORT
               DISPLAY 'HCRPTGEN - RPT11 FWA: SELECTED'
           END-IF
           IF RUN-COMPLIANCE
               DISPLAY 'HCRPTGEN - RPT12 COMPLIANCE: SELECTED'
           END-IF.

      *================================================================*
      *    2110-PROCESS-CTRL-RECORD                                    *
      *    PARSE INDIVIDUAL CONTROL FILE RECORDS                       *
      *================================================================*
       2110-PROCESS-CTRL-RECORD.

           MOVE RPTCTRL-RECORD-TYPE TO WS-CTRL-RECORD-TYPE
           MOVE RPTCTRL-DATA        TO WS-CTRL-RPT-SELECT

           EVALUATE WS-CTRL-RECORD-TYPE
               WHEN 'RS'
      *            REPORT SELECTION RECORD
                   EVALUATE WS-CTRL-RPT-ID
                       WHEN 'RPT01'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT01
                       WHEN 'RPT02'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT02
                       WHEN 'RPT03'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT03
                       WHEN 'RPT04'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT04
                       WHEN 'RPT05'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT05
                       WHEN 'RPT06'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT06
                       WHEN 'RPT07'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT07
                       WHEN 'RPT08'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT08
                       WHEN 'RPT09'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT09
                       WHEN 'RPT10'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT10
                       WHEN 'RPT11'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT11
                       WHEN 'RPT12'
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT12
                       WHEN 'RPTXX'
      *                    SELECT ALL REPORTS
                           MOVE WS-CTRL-RPT-FLAG
                               TO WS-RUN-RPT01
                                  WS-RUN-RPT02
                                  WS-RUN-RPT03
                                  WS-RUN-RPT04
                                  WS-RUN-RPT05
                                  WS-RUN-RPT06
                                  WS-RUN-RPT07
                                  WS-RUN-RPT08
                                  WS-RUN-RPT09
                                  WS-RUN-RPT10
                                  WS-RUN-RPT11
                                  WS-RUN-RPT12
                       WHEN OTHER
                           ADD 1 TO WS-TOTAL-WARNINGS
                           DISPLAY 'WARNING: UNKNOWN REPORT ID: '
                               WS-CTRL-RPT-ID
                   END-EVALUATE

               WHEN 'DO'
      *            DATE OVERRIDE RECORD
                   MOVE RPTCTRL-DATA TO WS-CTRL-DATE-OVERRIDE
                   IF WS-CTRL-OVR-TYPE = 'R'
                       MOVE WS-CTRL-OVR-START
                           TO WS-OVERRIDE-START
                       MOVE WS-CTRL-OVR-END
                           TO WS-OVERRIDE-END
                   END-IF

               WHEN 'TH'
      *            THRESHOLD PARAMETER RECORD
                   MOVE RPTCTRL-DATA TO WS-CTRL-THRESHOLD
                   EVALUATE WS-CTRL-THR-CODE
                       WHEN 'HDOL'
                           MOVE WS-CTRL-THR-VALUE
                               TO WS-HIGH-DOLLAR-THRESH
                       WHEN 'FWAP'
                           MOVE WS-CTRL-THR-VALUE
                               TO WS-FWA-OUTLIER-PCTILE
                       WHEN 'PPDY'
                           MOVE WS-CTRL-THR-VALUE
                               TO WS-PROMPT-PAY-DAYS
                       WHEN 'CCDY'
                           MOVE WS-CTRL-THR-VALUE
                               TO WS-CLEAN-CLAIM-DAYS
                   END-EVALUATE

               WHEN 'PT'
      *            PERIOD TYPE RECORD
                   MOVE RPTCTRL-DATA(1:1) TO WS-RPT-PERIOD-TYPE

               WHEN 'DL'
      *            DISTRIBUTION LIST RECORD
                   MOVE RPTCTRL-DATA TO WS-CTRL-DISTRIB-LIST

               WHEN OTHER
                   ADD 1 TO WS-TOTAL-WARNINGS
                   DISPLAY 'WARNING: UNKNOWN CONTROL RECORD TYPE: '
                       WS-CTRL-RECORD-TYPE
           END-EVALUATE.

      *================================================================*
      *    3000-GENERATE-REPORTS                                       *
      *    DISPATCH TO EACH SELECTED REPORT GENERATOR                  *
      *================================================================*
       3000-GENERATE-REPORTS.

           DISPLAY 'HCRPTGEN - BEGINNING REPORT GENERATION'

           IF RUN-CLAIMS-AGING
               PERFORM 3100-BUILD-CLAIMS-AGING
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-PROVIDER-PMT
               PERFORM 3200-BUILD-PROVIDER-PAYMENT
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-PAYER-MIX
               PERFORM 3300-BUILD-PAYER-MIX
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-DENIAL-ANALYSIS
               PERFORM 3400-BUILD-DENIAL-ANALYSIS
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-FINANCIAL-SUMM
               PERFORM 3500-BUILD-FINANCIAL-SUMMARY
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-PEND-AGING
               PERFORM 3600-BUILD-PEND-AGING
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-AUTH-UTIL
               PERFORM 3700-BUILD-AUTH-UTILIZATION
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-HIGH-DOLLAR
               PERFORM 3800-BUILD-HIGH-DOLLAR
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-DUPLICATE-DET
               PERFORM 3900-BUILD-DUPLICATE-REPORT
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-QUALITY-MTRC
               PERFORM 4000-BUILD-QUALITY-METRICS
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-FWA-REPORT
               PERFORM 4100-BUILD-FWA-REPORT
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           IF RUN-COMPLIANCE
               PERFORM 4200-BUILD-COMPLIANCE-REPORT
               ADD 1 TO WS-TOTAL-REPORTS-RUN
           END-IF

           DISPLAY 'HCRPTGEN - REPORT GENERATION COMPLETE: '
               WS-TOTAL-REPORTS-RUN ' REPORTS PRODUCED'.

      *================================================================*
      *    3100-BUILD-CLAIMS-AGING                                     *
      *    REPORT 01: CLAIMS AGING ANALYSIS BY PAYER AND BUCKET        *
      *    QUERIES OPEN CLAIMS, CALCULATES DAYS AGED, ASSIGNS TO       *
      *    BUCKETS, ACCUMULATES BY PAYER/TYPE, COMPARES TO PRIOR       *
      *================================================================*
       3100-BUILD-CLAIMS-AGING.

           DISPLAY 'HCRPTGEN - BUILDING RPT01 CLAIMS AGING'

      *    WRITE AUDIT ENTRY FOR REPORT START
           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT01' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'CLAIMS AGING REPORT GENERATION STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY CURRENT PERIOD AGING DATA BY PAYER
           EXEC SQL
               DECLARE CSR-AGING CURSOR FOR
               SELECT
                   CH.PAYER_ID,
                   PC.PAYER_NAME,
                   CH.CLAIM_TYPE,
                   CH.FACILITY_ID,
                   DAYS(CURRENT DATE) - DAYS(CH.RECEIPT_DATE)
                       AS DAYS_AGED,
                   CH.BILLED_AMOUNT,
                   CH.CLAIM_ID
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.PAYER_CONTRACT PC
                   ON CH.PAYER_ID = PC.PAYER_ID
               WHERE CH.CLAIM_STATUS IN ('OP', 'PD', 'SU')
                 AND CH.RECEIPT_DATE <= :HV-RPT-END-DATE
               ORDER BY CH.PAYER_ID, CH.CLAIM_TYPE
           END-EXEC

           EXEC SQL OPEN CSR-AGING END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3100-EXIT
           END-IF

           SET NOT-EOF-CURSOR TO TRUE
           MOVE SPACES TO WS-SAVE-PAYER-ID

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-AGING
                   INTO :HV-PAYER-ID,
                        :HV-PAYER-NAME,
                        :HV-CLAIM-TYPE,
                        :HV-FACILITY-ID,
                        :HV-DAYS-AGED,
                        :HV-BILLED-AMT,
                        :HV-CLAIM-ID
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3110-ASSIGN-AGING-BUCKET
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-AGING END-EXEC

      *    QUERY PRIOR PERIOD FOR TREND COMPARISON
           PERFORM 3120-LOAD-PRIOR-AGING

      *    CALCULATE PERCENTAGES AND AVERAGES
           PERFORM 3130-CALC-AGING-STATS

      *    FORMAT AND WRITE THE REPORT
           PERFORM 3140-WRITE-AGING-REPORT

       3100-EXIT.
           EXIT.

      *================================================================*
      *    3110-ASSIGN-AGING-BUCKET                                    *
      *    DETERMINE WHICH AGING BUCKET A CLAIM FALLS INTO             *
      *    ACCUMULATE COUNTS AND DOLLARS BY PAYER AND TYPE             *
      *================================================================*
       3110-ASSIGN-AGING-BUCKET.

      *    DETERMINE BUCKET INDEX FROM DAYS AGED
           MOVE 0 TO WS-BUCKET-INDEX
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 6
                  OR WS-BUCKET-INDEX > 0
               IF HV-DAYS-AGED >= WS-BKT-LOW(WS-WORK-INDEX)
                   AND HV-DAYS-AGED <= WS-BKT-HIGH(WS-WORK-INDEX)
                   MOVE WS-WORK-INDEX TO WS-BUCKET-INDEX
               END-IF
           END-PERFORM

           IF WS-BUCKET-INDEX = 0
               MOVE 6 TO WS-BUCKET-INDEX
           END-IF

      *    FIND OR ASSIGN PAYER SLOT IN AGING TABLE
           MOVE 0 TO WS-PAYER-INDEX
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
                  OR WS-PAYER-INDEX > 0
               IF WS-AG-PAYER-ID(WS-WORK-INDEX) = HV-PAYER-ID
                   MOVE WS-WORK-INDEX TO WS-PAYER-INDEX
               ELSE
                   IF WS-AG-PAYER-ID(WS-WORK-INDEX) = SPACES
                       MOVE WS-WORK-INDEX TO WS-PAYER-INDEX
                       MOVE HV-PAYER-ID
                           TO WS-AG-PAYER-ID(WS-PAYER-INDEX)
                       MOVE HV-PAYER-NAME
                           TO WS-AG-PAYER-NAME(WS-PAYER-INDEX)
                   END-IF
               END-IF
           END-PERFORM

           IF WS-PAYER-INDEX > 0 AND WS-PAYER-INDEX <= 20
      *        ACCUMULATE INTO PAYER BUCKET
               ADD 1 TO WS-AG-BKT-COUNT(
                   WS-PAYER-INDEX, WS-BUCKET-INDEX)
               ADD HV-BILLED-AMT TO WS-AG-BKT-AMT(
                   WS-PAYER-INDEX, WS-BUCKET-INDEX)
               ADD HV-DAYS-AGED TO WS-AG-BKT-TOTAL-DAYS(
                   WS-PAYER-INDEX, WS-BUCKET-INDEX)
               ADD 1 TO WS-AG-PAYER-TOTAL-CNT(WS-PAYER-INDEX)
               ADD HV-BILLED-AMT
                   TO WS-AG-PAYER-TOTAL-AMT(WS-PAYER-INDEX)
           END-IF

      *    ACCUMULATE INTO CLAIM TYPE TABLE
           MOVE 0 TO WS-CLMTYP-INDEX
           EVALUATE HV-CLAIM-TYPE
               WHEN 'IN'  MOVE 1 TO WS-CLMTYP-INDEX
               WHEN 'OP'  MOVE 2 TO WS-CLMTYP-INDEX
               WHEN 'PR'  MOVE 3 TO WS-CLMTYP-INDEX
               WHEN 'RX'  MOVE 4 TO WS-CLMTYP-INDEX
           END-EVALUATE

           IF WS-CLMTYP-INDEX > 0
               ADD 1 TO WS-AGC-BKT-CNT(
                   WS-CLMTYP-INDEX, WS-BUCKET-INDEX)
               ADD HV-BILLED-AMT TO WS-AGC-BKT-AMT(
                   WS-CLMTYP-INDEX, WS-BUCKET-INDEX)
               ADD 1 TO WS-AGC-TOTAL-CNT(WS-CLMTYP-INDEX)
               ADD HV-BILLED-AMT
                   TO WS-AGC-TOTAL-AMT(WS-CLMTYP-INDEX)
           END-IF

      *    ACCUMULATE GRAND TOTALS
           ADD 1 TO WS-AGG-BKT-COUNT(WS-BUCKET-INDEX)
           ADD HV-BILLED-AMT
               TO WS-AGG-BKT-AMT(WS-BUCKET-INDEX)
           ADD 1 TO WS-AGG-GRAND-CNT
           ADD HV-BILLED-AMT TO WS-AGG-GRAND-AMT.

      *================================================================*
      *    3120-LOAD-PRIOR-AGING                                       *
      *    QUERY PRIOR PERIOD AGING FOR TREND COMPARISON               *
      *================================================================*
       3120-LOAD-PRIOR-AGING.

           EXEC SQL
               SELECT
                   CASE
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE) BETWEEN 0 AND 30
                           THEN 1
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE) BETWEEN 31 AND 60
                           THEN 2
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE) BETWEEN 61 AND 90
                           THEN 3
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE)
                                BETWEEN 91 AND 120
                           THEN 4
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE)
                                BETWEEN 121 AND 180
                           THEN 5
                       ELSE 6
                   END AS BUCKET_NBR,
                   COUNT(*),
                   SUM(CH.BILLED_AMOUNT)
               INTO :HV-BUCKET-NBR,
                    :HV-COUNT,
                    :HV-SUM-AMT
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.CLAIM_STATUS IN ('OP', 'PD', 'SU')
                 AND CH.RECEIPT_DATE <= :HV-PRIOR-END-DATE
               GROUP BY
                   CASE
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE) BETWEEN 0 AND 30
                           THEN 1
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE) BETWEEN 31 AND 60
                           THEN 2
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE) BETWEEN 61 AND 90
                           THEN 3
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE)
                                BETWEEN 91 AND 120
                           THEN 4
                       WHEN DAYS(:HV-PRIOR-END-DATE)
                            - DAYS(CH.RECEIPT_DATE)
                                BETWEEN 121 AND 180
                           THEN 5
                       ELSE 6
                   END
           END-EXEC

           IF SQLCODE = 0
               IF HV-BUCKET-NBR >= 1 AND HV-BUCKET-NBR <= 6
                   MOVE HV-COUNT
                       TO WS-PAG-BKT-COUNT(HV-BUCKET-NBR)
                   MOVE HV-SUM-AMT
                       TO WS-PAG-BKT-AMT(HV-BUCKET-NBR)
                   ADD HV-COUNT TO WS-PAG-GRAND-CNT
                   ADD HV-SUM-AMT TO WS-PAG-GRAND-AMT
               END-IF
           ELSE
               IF SQLCODE NOT = 100
                   PERFORM 8100-LOG-SQL-ERROR
               END-IF
           END-IF.

      *================================================================*
      *    3130-CALC-AGING-STATS                                       *
      *    CALCULATE PERCENTAGES, AVERAGES FOR AGING REPORT            *
      *================================================================*
       3130-CALC-AGING-STATS.

      *    CALCULATE PERCENTAGE DISTRIBUTION FOR EACH BUCKET
           IF WS-AGG-GRAND-AMT NOT = 0
               PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
                   UNTIL WS-WORK-INDEX > 6
                   COMPUTE WS-AGG-BKT-PCT(WS-WORK-INDEX) =
                       (WS-AGG-BKT-AMT(WS-WORK-INDEX) /
                        WS-AGG-GRAND-AMT) * 100
               END-PERFORM
           END-IF

      *    CALCULATE AVERAGE DAYS PER BUCKET PER PAYER
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
               IF WS-AG-PAYER-ID(WS-WORK-INDEX) NOT = SPACES
                   PERFORM VARYING WS-WORK-INDEX2 FROM 1 BY 1
                       UNTIL WS-WORK-INDEX2 > 6
                       IF WS-AG-BKT-COUNT(WS-WORK-INDEX,
                           WS-WORK-INDEX2) > 0
                           COMPUTE WS-AG-BKT-AVG-DAYS(
                               WS-WORK-INDEX, WS-WORK-INDEX2) =
                               WS-AG-BKT-TOTAL-DAYS(
                                   WS-WORK-INDEX, WS-WORK-INDEX2)
                               / WS-AG-BKT-COUNT(
                                   WS-WORK-INDEX, WS-WORK-INDEX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *    CALCULATE GRAND TOTAL AVERAGE DAYS PER BUCKET
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 6
               MOVE 0 TO WS-WORK-COUNT
               MOVE 0 TO WS-WORK-DAYS
               PERFORM VARYING WS-WORK-INDEX2 FROM 1 BY 1
                   UNTIL WS-WORK-INDEX2 > 20
                   IF WS-AG-PAYER-ID(WS-WORK-INDEX2) NOT = SPACES
                       ADD WS-AG-BKT-COUNT(WS-WORK-INDEX2,
                           WS-WORK-INDEX) TO WS-WORK-COUNT
                       ADD WS-AG-BKT-TOTAL-DAYS(WS-WORK-INDEX2,
                           WS-WORK-INDEX) TO WS-WORK-DAYS
                   END-IF
               END-PERFORM
               IF WS-WORK-COUNT > 0
                   COMPUTE WS-AGG-BKT-AVG-DAYS(WS-WORK-INDEX) =
                       WS-WORK-DAYS / WS-WORK-COUNT
               END-IF
           END-PERFORM.

      *================================================================*
      *    3140-WRITE-AGING-REPORT                                     *
      *    FORMAT AND OUTPUT THE COMPLETE CLAIMS AGING REPORT          *
      *================================================================*
       3140-WRITE-AGING-REPORT.

      *    WRITE REPORT HEADERS
           PERFORM 3141-WRITE-AGING-HEADERS

      *    WRITE DETAIL LINES BY PAYER
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
               IF WS-AG-PAYER-ID(WS-WORK-INDEX) NOT = SPACES
                   IF WS-RPT01-LINE-CTR >= WS-MAX-LINES
                       PERFORM 3141-WRITE-AGING-HEADERS
                   END-IF

                   MOVE WS-AG-PAYER-NAME(WS-WORK-INDEX)(1:12)
                       TO WS-R01D-PAYER
                   MOVE WS-AG-BKT-COUNT(WS-WORK-INDEX, 1)
                       TO WS-R01D-BKT1-CNT
                   MOVE WS-AG-BKT-AMT(WS-WORK-INDEX, 1)
                       TO WS-R01D-BKT1-AMT
                   MOVE WS-AG-BKT-COUNT(WS-WORK-INDEX, 2)
                       TO WS-R01D-BKT2-CNT
                   MOVE WS-AG-BKT-AMT(WS-WORK-INDEX, 2)
                       TO WS-R01D-BKT2-AMT
                   MOVE WS-AG-BKT-COUNT(WS-WORK-INDEX, 3)
                       TO WS-R01D-BKT3-CNT
                   MOVE WS-AG-BKT-AMT(WS-WORK-INDEX, 3)
                       TO WS-R01D-BKT3-AMT

                   WRITE RPT01-RECORD FROM WS-RPT01-DETAIL
                   ADD 1 TO WS-RPT01-LINE-CTR
                   ADD 1 TO WS-RPT01-RECORDS

      *            SECOND LINE WITH REMAINING BUCKETS AND TOTALS
                   MOVE WS-AG-BKT-COUNT(WS-WORK-INDEX, 4)
                       TO WS-R01D2-BKT4-CNT
                   MOVE WS-AG-BKT-AMT(WS-WORK-INDEX, 4)
                       TO WS-R01D2-BKT4-AMT
                   MOVE WS-AG-BKT-COUNT(WS-WORK-INDEX, 5)
                       TO WS-R01D2-BKT5-CNT
                   MOVE WS-AG-BKT-AMT(WS-WORK-INDEX, 5)
                       TO WS-R01D2-BKT5-AMT
                   MOVE WS-AG-BKT-COUNT(WS-WORK-INDEX, 6)
                       TO WS-R01D2-BKT6-CNT
                   MOVE WS-AG-BKT-AMT(WS-WORK-INDEX, 6)
                       TO WS-R01D2-BKT6-AMT
                   MOVE WS-AG-PAYER-TOTAL-CNT(WS-WORK-INDEX)
                       TO WS-R01D2-TOTAL-CNT
                   MOVE WS-AG-PAYER-TOTAL-AMT(WS-WORK-INDEX)
                       TO WS-R01D2-TOTAL-AMT

                   WRITE RPT01-RECORD FROM WS-RPT01-DETAIL2
                   ADD 1 TO WS-RPT01-LINE-CTR
                   ADD 1 TO WS-RPT01-RECORDS

                   WRITE RPT01-RECORD FROM WS-SEPARATOR-LINE
                   ADD 1 TO WS-RPT01-LINE-CTR
               END-IF
           END-PERFORM

      *    WRITE CLAIM TYPE BREAKDOWN
           WRITE RPT01-RECORD FROM WS-BLANK-LINE
           ADD 1 TO WS-RPT01-LINE-CTR
           MOVE '  BREAKDOWN BY CLAIM TYPE:' TO WS-WORK-STRING
           WRITE RPT01-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT01-LINE-CTR
           WRITE RPT01-RECORD FROM WS-DOUBLE-SEP-LINE
           ADD 1 TO WS-RPT01-LINE-CTR

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 4
               MOVE WS-AGC-TYPE-DESC(WS-WORK-INDEX)(1:12)
                   TO WS-R01D-PAYER
               MOVE WS-AGC-BKT-CNT(WS-WORK-INDEX, 1)
                   TO WS-R01D-BKT1-CNT
               MOVE WS-AGC-BKT-AMT(WS-WORK-INDEX, 1)
                   TO WS-R01D-BKT1-AMT
               MOVE WS-AGC-BKT-CNT(WS-WORK-INDEX, 2)
                   TO WS-R01D-BKT2-CNT
               MOVE WS-AGC-BKT-AMT(WS-WORK-INDEX, 2)
                   TO WS-R01D-BKT2-AMT
               MOVE WS-AGC-BKT-CNT(WS-WORK-INDEX, 3)
                   TO WS-R01D-BKT3-CNT
               MOVE WS-AGC-BKT-AMT(WS-WORK-INDEX, 3)
                   TO WS-R01D-BKT3-AMT
               WRITE RPT01-RECORD FROM WS-RPT01-DETAIL
               ADD 1 TO WS-RPT01-LINE-CTR
               ADD 1 TO WS-RPT01-RECORDS
           END-PERFORM

      *    WRITE GRAND TOTALS
           WRITE RPT01-RECORD FROM WS-DOUBLE-SEP-LINE
           ADD 1 TO WS-RPT01-LINE-CTR
           MOVE 'GRAND TOTAL ' TO WS-R01D-PAYER
           MOVE WS-AGG-BKT-COUNT(1) TO WS-R01D-BKT1-CNT
           MOVE WS-AGG-BKT-AMT(1)   TO WS-R01D-BKT1-AMT
           MOVE WS-AGG-BKT-COUNT(2) TO WS-R01D-BKT2-CNT
           MOVE WS-AGG-BKT-AMT(2)   TO WS-R01D-BKT2-AMT
           MOVE WS-AGG-BKT-COUNT(3) TO WS-R01D-BKT3-CNT
           MOVE WS-AGG-BKT-AMT(3)   TO WS-R01D-BKT3-AMT
           WRITE RPT01-RECORD FROM WS-RPT01-DETAIL
           ADD 1 TO WS-RPT01-LINE-CTR
           ADD 1 TO WS-RPT01-RECORDS

      *    WRITE PERCENTAGE DISTRIBUTION ROW
           MOVE WS-AGG-BKT-PCT(1) TO WS-R01P-BKT1-PCT
           MOVE WS-AGG-BKT-PCT(2) TO WS-R01P-BKT2-PCT
           MOVE WS-AGG-BKT-PCT(3) TO WS-R01P-BKT3-PCT
           MOVE WS-AGG-BKT-PCT(4) TO WS-R01P-BKT4-PCT
           MOVE WS-AGG-BKT-PCT(5) TO WS-R01P-BKT5-PCT
           MOVE WS-AGG-BKT-PCT(6) TO WS-R01P-BKT6-PCT
           WRITE RPT01-RECORD FROM WS-RPT01-PCT-LINE
           ADD 1 TO WS-RPT01-LINE-CTR
           ADD 1 TO WS-RPT01-RECORDS

      *    WRITE TREND COMPARISON
           WRITE RPT01-RECORD FROM WS-BLANK-LINE
           ADD 1 TO WS-RPT01-LINE-CTR
           MOVE WS-PAG-GRAND-AMT TO WS-R01T-PRIOR-AMT
           COMPUTE WS-VARIANCE-AMT =
               WS-AGG-GRAND-AMT - WS-PAG-GRAND-AMT
           MOVE WS-VARIANCE-AMT TO WS-R01T-VARIANCE
           IF WS-PAG-GRAND-AMT NOT = 0
               COMPUTE WS-VARIANCE-PCT =
                   (WS-VARIANCE-AMT / WS-PAG-GRAND-AMT) * 100
           ELSE
               MOVE 0 TO WS-VARIANCE-PCT
           END-IF
           MOVE WS-VARIANCE-PCT TO WS-R01T-VAR-PCT
           WRITE RPT01-RECORD FROM WS-RPT01-TREND-LINE
           ADD 1 TO WS-RPT01-LINE-CTR
           ADD 1 TO WS-RPT01-RECORDS

      *    AUDIT THE REPORT COMPLETION
           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT01' TO WS-AUD-REPORT-ID
           MOVE WS-RPT01-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'CLAIMS AGING REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3141-WRITE-AGING-HEADERS                                    *
      *    WRITE PAGE HEADERS FOR CLAIMS AGING REPORT                  *
      *================================================================*
       3141-WRITE-AGING-HEADERS.

           ADD 1 TO WS-RPT01-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R01H1-DATE
           MOVE WS-RPT01-PAGE-CTR   TO WS-R01H1-PAGE
           MOVE WS-REPORT-TIME-DISP TO WS-R01H2-TIME
           MOVE WS-RPT-START-DATE   TO WS-R01H3-START
           MOVE WS-RPT-END-DATE     TO WS-R01H3-END

           WRITE RPT01-RECORD FROM WS-RPT01-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT01-RECORD FROM WS-RPT01-HEADER2
           WRITE RPT01-RECORD FROM WS-RPT01-HEADER3
           WRITE RPT01-RECORD FROM WS-BLANK-LINE
           WRITE RPT01-RECORD FROM WS-DOUBLE-SEP-LINE
           WRITE RPT01-RECORD FROM WS-RPT01-COL-HDR
           WRITE RPT01-RECORD FROM WS-DOUBLE-SEP-LINE

           MOVE 7 TO WS-RPT01-LINE-CTR.

      *================================================================*
      *    3200-BUILD-PROVIDER-PAYMENT                                 *
      *    REPORT 02: PROVIDER PAYMENT SUMMARY WITH RANKING            *
      *    QUERIES PAYMENT DATA BY PROVIDER, RANKS TOP 50,             *
      *    INCLUDES YTD VS MTD AND PAYMENT METHOD BREAKDOWN            *
      *================================================================*
       3200-BUILD-PROVIDER-PAYMENT.

           DISPLAY 'HCRPTGEN - BUILDING RPT02 PROVIDER PAYMENT'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT02' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'PROVIDER PAYMENT SUMMARY STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY PROVIDER PAYMENT SUMMARY FOR CURRENT PERIOD
           EXEC SQL
               DECLARE CSR-PROVPMT CURSOR FOR
               SELECT
                   PM.PROVIDER_NPI,
                   PM.TAX_ID,
                   PM.PROVIDER_NAME,
                   PM.SPECIALTY_CODE,
                   COUNT(DISTINCT CH.CLAIM_ID) AS CLM_COUNT,
                   SUM(CH.BILLED_AMOUNT) AS TOTAL_BILLED,
                   SUM(CH.ALLOWED_AMOUNT) AS TOTAL_ALLOWED,
                   SUM(CH.PAID_AMOUNT) AS TOTAL_PAID,
                   SUM(COALESCE(CH.WITHHOLD_AMOUNT, 0))
                       AS TOTAL_WITHHOLD,
                   SUM(CH.PAID_AMOUNT)
                       - SUM(COALESCE(CH.WITHHOLD_AMOUNT, 0))
                       AS NET_PAYMENT
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.PROVIDER_MASTER PM
                   ON CH.RENDERING_PROVIDER = PM.PROVIDER_NPI
               WHERE CH.CLAIM_STATUS = 'PD'
                 AND CH.PAID_DATE BETWEEN :HV-RPT-START-DATE
                                       AND :HV-RPT-END-DATE
               GROUP BY PM.PROVIDER_NPI, PM.TAX_ID,
                        PM.PROVIDER_NAME, PM.SPECIALTY_CODE
               ORDER BY TOTAL_PAID DESC
               FETCH FIRST 500 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-PROVPMT END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3200-EXIT
           END-IF

           MOVE 0 TO WS-PROV-COUNT
           SET NOT-EOF-CURSOR TO TRUE

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-PROVPMT
                   INTO :HV-PROVIDER-NPI,
                        :HV-PROVIDER-TAX-ID,
                        :HV-PROVIDER-NAME,
                        :HV-PROVIDER-SPEC,
                        :HV-COUNT,
                        :HV-BILLED-AMT,
                        :HV-ALLOWED-AMT :WS-IND-ALLOWED-AMT,
                        :HV-PAID-AMT :WS-IND-PAID-AMT,
                        :HV-WITHHOLD-AMT :WS-IND-WITHHOLD-AMT,
                        :HV-SUM-AMT
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1 TO WS-PROV-COUNT
                       IF WS-PROV-COUNT <= 500
                           PERFORM 3210-STORE-PROVIDER-DATA
                       END-IF
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-PROVPMT END-EXEC

      *    LOAD YTD AND PRIOR PERIOD DATA FOR EACH PROVIDER
           PERFORM 3220-LOAD-PROVIDER-YTD
           PERFORM 3230-LOAD-PROVIDER-PAYMENT-METHOD

      *    CALCULATE RANKINGS AND AVERAGES
           PERFORM 3240-RANK-PROVIDERS

      *    WRITE THE REPORT
           PERFORM 3250-WRITE-PROVIDER-REPORT

       3200-EXIT.
           EXIT.

      *================================================================*
      *    3210-STORE-PROVIDER-DATA                                    *
      *    STORE FETCHED PROVIDER DATA INTO WORKING TABLE              *
      *================================================================*
       3210-STORE-PROVIDER-DATA.

           MOVE HV-PROVIDER-NPI
               TO WS-PRV-NPI(WS-PROV-COUNT)
           MOVE HV-PROVIDER-TAX-ID
               TO WS-PRV-TAX-ID(WS-PROV-COUNT)
           MOVE HV-PROVIDER-NAME
               TO WS-PRV-NAME(WS-PROV-COUNT)
           MOVE HV-PROVIDER-SPEC
               TO WS-PRV-SPECIALTY(WS-PROV-COUNT)
           MOVE HV-COUNT
               TO WS-PRV-CLM-CNT(WS-PROV-COUNT)
           MOVE HV-BILLED-AMT
               TO WS-PRV-BILLED-AMT(WS-PROV-COUNT)

           IF WS-IND-ALLOWED-AMT >= 0
               MOVE HV-ALLOWED-AMT
                   TO WS-PRV-ALLOWED-AMT(WS-PROV-COUNT)
           ELSE
               MOVE 0 TO WS-PRV-ALLOWED-AMT(WS-PROV-COUNT)
           END-IF

           IF WS-IND-PAID-AMT >= 0
               MOVE HV-PAID-AMT
                   TO WS-PRV-PAID-AMT(WS-PROV-COUNT)
           ELSE
               MOVE 0 TO WS-PRV-PAID-AMT(WS-PROV-COUNT)
           END-IF

           IF WS-IND-WITHHOLD-AMT >= 0
               MOVE HV-WITHHOLD-AMT
                   TO WS-PRV-WITHHOLD(WS-PROV-COUNT)
           ELSE
               MOVE 0 TO WS-PRV-WITHHOLD(WS-PROV-COUNT)
           END-IF

           COMPUTE WS-PRV-NET-PMT(WS-PROV-COUNT) =
               WS-PRV-PAID-AMT(WS-PROV-COUNT) -
               WS-PRV-WITHHOLD(WS-PROV-COUNT)

           IF WS-PRV-CLM-CNT(WS-PROV-COUNT) > 0
               COMPUTE WS-PRV-AVG-PMT(WS-PROV-COUNT) =
                   WS-PRV-PAID-AMT(WS-PROV-COUNT) /
                   WS-PRV-CLM-CNT(WS-PROV-COUNT)
           ELSE
               MOVE 0 TO WS-PRV-AVG-PMT(WS-PROV-COUNT)
           END-IF.

      *================================================================*
      *    3220-LOAD-PROVIDER-YTD                                      *
      *    LOAD YTD AND PRIOR PERIOD TOTALS FOR EACH PROVIDER          *
      *================================================================*
       3220-LOAD-PROVIDER-YTD.

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-PROV-COUNT
                  OR WS-WORK-INDEX > 500

               MOVE WS-PRV-NPI(WS-WORK-INDEX) TO HV-PROVIDER-NPI

      *        GET YTD TOTALS
               EXEC SQL
                   SELECT COALESCE(SUM(CH.PAID_AMOUNT), 0)
                   INTO :HV-SUM-AMT
                   FROM HCDB.CLAIM_HEADER CH
                   WHERE CH.RENDERING_PROVIDER = :HV-PROVIDER-NPI
                     AND CH.CLAIM_STATUS = 'PD'
                     AND CH.PAID_DATE >= :HV-YTD-START-DATE
                     AND CH.PAID_DATE <= :HV-RPT-END-DATE
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-SUM-AMT
                       TO WS-PRV-YTD-PAID(WS-WORK-INDEX)
               END-IF

      *        GET MTD TOTALS (SAME AS CURRENT PERIOD FOR MONTHLY)
               MOVE WS-PRV-PAID-AMT(WS-WORK-INDEX)
                   TO WS-PRV-MTD-PAID(WS-WORK-INDEX)

      *        GET PRIOR PERIOD TOTALS
               EXEC SQL
                   SELECT COALESCE(SUM(CH.PAID_AMOUNT), 0)
                   INTO :HV-SUM-AMT
                   FROM HCDB.CLAIM_HEADER CH
                   WHERE CH.RENDERING_PROVIDER = :HV-PROVIDER-NPI
                     AND CH.CLAIM_STATUS = 'PD'
                     AND CH.PAID_DATE BETWEEN :HV-PRIOR-START-DATE
                                           AND :HV-PRIOR-END-DATE
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-SUM-AMT
                       TO WS-PRV-PRIOR-PAID(WS-WORK-INDEX)
               END-IF
           END-PERFORM.

      *================================================================*
      *    3230-LOAD-PROVIDER-PAYMENT-METHOD                           *
      *    LOAD CHECK VS EFT PAYMENT BREAKDOWN PER PROVIDER            *
      *================================================================*
       3230-LOAD-PROVIDER-PAYMENT-METHOD.

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-PROV-COUNT
                  OR WS-WORK-INDEX > 500

               MOVE WS-PRV-NPI(WS-WORK-INDEX) TO HV-PROVIDER-NPI

               EXEC SQL
                   SELECT
                       SUM(CASE WHEN PH.PAYMENT_METHOD = 'C'
                           THEN 1 ELSE 0 END),
                       SUM(CASE WHEN PH.PAYMENT_METHOD = 'C'
                           THEN PH.PAYMENT_AMOUNT ELSE 0 END),
                       SUM(CASE WHEN PH.PAYMENT_METHOD = 'E'
                           THEN 1 ELSE 0 END),
                       SUM(CASE WHEN PH.PAYMENT_METHOD = 'E'
                           THEN PH.PAYMENT_AMOUNT ELSE 0 END)
                   INTO :WS-PRV-CHECK-CNT(:WS-WORK-INDEX),
                        :WS-PRV-CHECK-AMT(:WS-WORK-INDEX),
                        :WS-PRV-EFT-CNT(:WS-WORK-INDEX),
                        :WS-PRV-EFT-AMT(:WS-WORK-INDEX)
                   FROM HCDB.PAYMENT_HISTORY PH
                   WHERE PH.PROVIDER_NPI = :HV-PROVIDER-NPI
                     AND PH.PAYMENT_DATE BETWEEN :HV-RPT-START-DATE
                                               AND :HV-RPT-END-DATE
               END-EXEC

               IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                   PERFORM 8100-LOG-SQL-ERROR
               END-IF
           END-PERFORM.

      *================================================================*
      *    3240-RANK-PROVIDERS                                         *
      *    ASSIGN RANKING NUMBERS (ALREADY SORTED BY QUERY)            *
      *================================================================*
       3240-RANK-PROVIDERS.

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-PROV-COUNT
                  OR WS-WORK-INDEX > 500
               MOVE WS-WORK-INDEX
                   TO WS-PRV-RANK(WS-WORK-INDEX)
           END-PERFORM.

      *================================================================*
      *    3250-WRITE-PROVIDER-REPORT                                  *
      *    FORMAT AND OUTPUT PROVIDER PAYMENT SUMMARY REPORT           *
      *================================================================*
       3250-WRITE-PROVIDER-REPORT.

           MOVE 0 TO WS-GRAND-TOTAL-AMT
           MOVE 0 TO WS-GRAND-TOTAL-CNT

           PERFORM 3251-WRITE-PROVIDER-HEADERS

           COMPUTE WS-WORK-COUNT =
               FUNCTION MIN(WS-PROV-COUNT, WS-TOP-N-PROVIDERS)

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-WORK-COUNT

               IF WS-RPT02-LINE-CTR >= (WS-MAX-LINES - 2)
                   PERFORM 3251-WRITE-PROVIDER-HEADERS
               END-IF

               MOVE WS-PRV-RANK(WS-WORK-INDEX)
                   TO WS-R02D-RANK
               MOVE WS-PRV-NPI(WS-WORK-INDEX)
                   TO WS-R02D-NPI
               MOVE WS-PRV-NAME(WS-WORK-INDEX)(1:25)
                   TO WS-R02D-NAME
               MOVE WS-PRV-CLM-CNT(WS-WORK-INDEX)
                   TO WS-R02D-CLM-CNT
               MOVE WS-PRV-BILLED-AMT(WS-WORK-INDEX)
                   TO WS-R02D-BILLED
               MOVE WS-PRV-ALLOWED-AMT(WS-WORK-INDEX)
                   TO WS-R02D-ALLOWED
               MOVE WS-PRV-PAID-AMT(WS-WORK-INDEX)
                   TO WS-R02D-PAID
               MOVE WS-PRV-WITHHOLD(WS-WORK-INDEX)
                   TO WS-R02D-WITHHOLD
               MOVE WS-PRV-NET-PMT(WS-WORK-INDEX)
                   TO WS-R02D-NET

               WRITE RPT02-RECORD FROM WS-RPT02-DETAIL
               ADD 1 TO WS-RPT02-LINE-CTR
               ADD 1 TO WS-RPT02-RECORDS

      *        WRITE SUB-DETAIL LINE
               MOVE WS-PRV-AVG-PMT(WS-WORK-INDEX)
                   TO WS-R02S-AVG
               MOVE WS-PRV-YTD-PAID(WS-WORK-INDEX)
                   TO WS-R02S-YTD
               MOVE WS-PRV-PRIOR-PAID(WS-WORK-INDEX)
                   TO WS-R02S-PRIOR
               MOVE WS-PRV-CHECK-CNT(WS-WORK-INDEX)
                   TO WS-R02S-CHK-CNT
               MOVE WS-PRV-EFT-CNT(WS-WORK-INDEX)
                   TO WS-R02S-EFT-CNT

               WRITE RPT02-RECORD FROM WS-RPT02-SUBDETAIL
               ADD 1 TO WS-RPT02-LINE-CTR
               ADD 1 TO WS-RPT02-RECORDS

               WRITE RPT02-RECORD FROM WS-SEPARATOR-LINE
               ADD 1 TO WS-RPT02-LINE-CTR

               ADD WS-PRV-PAID-AMT(WS-WORK-INDEX)
                   TO WS-GRAND-TOTAL-AMT
               ADD WS-PRV-CLM-CNT(WS-WORK-INDEX)
                   TO WS-GRAND-TOTAL-CNT
           END-PERFORM

      *    WRITE GRAND TOTAL LINE
           WRITE RPT02-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 'TOTAL (TOP ' TO WS-WORK-STRING
           MOVE WS-GRAND-TOTAL-CNT TO WS-R02D-CLM-CNT
           MOVE WS-GRAND-TOTAL-AMT TO WS-R02D-PAID
           MOVE SPACES TO WS-R02D-NPI
           MOVE SPACES TO WS-R02D-NAME
           MOVE 0 TO WS-R02D-RANK
           WRITE RPT02-RECORD FROM WS-RPT02-DETAIL
           ADD 1 TO WS-RPT02-RECORDS

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT02' TO WS-AUD-REPORT-ID
           MOVE WS-RPT02-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'PROVIDER PAYMENT SUMMARY COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3251-WRITE-PROVIDER-HEADERS                                 *
      *================================================================*
       3251-WRITE-PROVIDER-HEADERS.

           ADD 1 TO WS-RPT02-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R02H1-DATE
           MOVE WS-RPT02-PAGE-CTR   TO WS-R02H1-PAGE
           WRITE RPT02-RECORD FROM WS-RPT02-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT02-RECORD FROM WS-RPT02-HEADER2
           WRITE RPT02-RECORD FROM WS-BLANK-LINE
           WRITE RPT02-RECORD FROM WS-DOUBLE-SEP-LINE
           WRITE RPT02-RECORD FROM WS-RPT02-COL-HDR
           WRITE RPT02-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 7 TO WS-RPT02-LINE-CTR.

      *================================================================*
      *    3300-BUILD-PAYER-MIX                                        *
      *    REPORT 03: PAYER MIX ANALYSIS WITH 12-MONTH ROLLING        *
      *    TREND, DENIAL RATES, COLLECTION RATES BY PAYER TYPE         *
      *================================================================*
       3300-BUILD-PAYER-MIX.

           DISPLAY 'HCRPTGEN - BUILDING RPT03 PAYER MIX'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT03' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'PAYER MIX ANALYSIS STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY REVENUE DATA BY PAYER WITH 12-MONTH BREAKDOWN
           EXEC SQL
               DECLARE CSR-PAYERMIX CURSOR FOR
               SELECT
                   PC.PAYER_ID,
                   PC.PAYER_NAME,
                   PC.PAYER_TYPE,
                   MONTH(CH.SERVICE_DATE) AS SVC_MONTH,
                   SUM(CH.BILLED_AMOUNT) AS REVENUE,
                   COUNT(*) AS CLM_COUNT,
                   SUM(CASE WHEN CH.CLAIM_STATUS = 'DN'
                       THEN 1 ELSE 0 END) AS DENIED_COUNT,
                   SUM(CASE WHEN CH.CLAIM_STATUS = 'PD'
                       THEN CH.PAID_AMOUNT ELSE 0 END)
                       AS COLLECTED
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.PAYER_CONTRACT PC
                   ON CH.PAYER_ID = PC.PAYER_ID
               WHERE CH.SERVICE_DATE >= :HV-RPT-START-DATE
                 AND CH.SERVICE_DATE <= :HV-RPT-END-DATE
               GROUP BY PC.PAYER_ID, PC.PAYER_NAME,
                        PC.PAYER_TYPE, MONTH(CH.SERVICE_DATE)
               ORDER BY PC.PAYER_ID, SVC_MONTH
           END-EXEC

           EXEC SQL OPEN CSR-PAYERMIX END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3300-EXIT
           END-IF

           SET NOT-EOF-CURSOR TO TRUE
           MOVE 0 TO WS-GRAND-TOTAL-AMT

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-PAYERMIX
                   INTO :HV-PAYER-ID,
                        :HV-PAYER-NAME,
                        :HV-PAYER-TYPE,
                        :HV-MONTH-NUM,
                        :HV-SUM-AMT,
                        :HV-COUNT,
                        :WS-WORK-COUNT,
                        :WS-WORK-AMT1
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3310-ACCUMULATE-PAYER-MIX
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-PAYERMIX END-EXEC

      *    CALCULATE PERCENTAGES AND RATES
           PERFORM 3320-CALC-PAYER-MIX-STATS

      *    LOAD PRODUCT LINE BREAKDOWN
           PERFORM 3330-LOAD-PRODUCT-LINE-DATA

      *    WRITE THE REPORT
           PERFORM 3340-WRITE-PAYER-MIX-REPORT

       3300-EXIT.
           EXIT.

      *================================================================*
      *    3310-ACCUMULATE-PAYER-MIX                                   *
      *================================================================*
       3310-ACCUMULATE-PAYER-MIX.

      *    FIND OR ASSIGN PAYER SLOT
           MOVE 0 TO WS-PAYER-INDEX
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
                  OR WS-PAYER-INDEX > 0
               IF WS-PMX-PAYER-ID(WS-WORK-INDEX) = HV-PAYER-ID
                   MOVE WS-WORK-INDEX TO WS-PAYER-INDEX
               ELSE IF WS-PMX-PAYER-ID(WS-WORK-INDEX) = SPACES
                   MOVE WS-WORK-INDEX TO WS-PAYER-INDEX
                   MOVE HV-PAYER-ID
                       TO WS-PMX-PAYER-ID(WS-PAYER-INDEX)
                   MOVE HV-PAYER-NAME
                       TO WS-PMX-PAYER-NAME(WS-PAYER-INDEX)
                   MOVE HV-PAYER-TYPE
                       TO WS-PMX-PAYER-TYPE(WS-PAYER-INDEX)
               END-IF
           END-PERFORM

           IF WS-PAYER-INDEX > 0 AND WS-PAYER-INDEX <= 20
               IF HV-MONTH-NUM >= 1 AND HV-MONTH-NUM <= 12
                   ADD HV-SUM-AMT TO WS-PMX-REVENUE(
                       WS-PAYER-INDEX, HV-MONTH-NUM)
                   ADD HV-COUNT TO WS-PMX-CLM-CNT(
                       WS-PAYER-INDEX, HV-MONTH-NUM)
                   ADD WS-WORK-COUNT TO WS-PMX-DENIED(
                       WS-PAYER-INDEX, HV-MONTH-NUM)
                   ADD WS-WORK-AMT1 TO WS-PMX-COLLECTED(
                       WS-PAYER-INDEX, HV-MONTH-NUM)
               END-IF
               ADD HV-SUM-AMT
                   TO WS-PMX-TOTAL-REV(WS-PAYER-INDEX)
               ADD HV-COUNT
                   TO WS-PMX-TOTAL-CLM(WS-PAYER-INDEX)
               ADD HV-SUM-AMT TO WS-GRAND-TOTAL-AMT
           END-IF.

      *================================================================*
      *    3320-CALC-PAYER-MIX-STATS                                   *
      *================================================================*
       3320-CALC-PAYER-MIX-STATS.

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
               IF WS-PMX-PAYER-ID(WS-WORK-INDEX) NOT = SPACES

      *            CALCULATE REVENUE PERCENTAGE
                   IF WS-GRAND-TOTAL-AMT > 0
                       COMPUTE WS-PMX-REV-PCT(WS-WORK-INDEX) =
                           (WS-PMX-TOTAL-REV(WS-WORK-INDEX) /
                            WS-GRAND-TOTAL-AMT) * 100
                   END-IF

      *            CALCULATE AVERAGE REIMBURSEMENT RATE
                   IF WS-PMX-TOTAL-REV(WS-WORK-INDEX) > 0
                       MOVE 0 TO WS-WORK-AMT1
                       PERFORM VARYING WS-WORK-INDEX2 FROM 1 BY 1
                           UNTIL WS-WORK-INDEX2 > 12
                           ADD WS-PMX-COLLECTED(WS-WORK-INDEX,
                               WS-WORK-INDEX2) TO WS-WORK-AMT1
                       END-PERFORM
                       COMPUTE WS-PMX-AVG-REIMB(WS-WORK-INDEX) =
                           WS-WORK-AMT1 /
                           WS-PMX-TOTAL-REV(WS-WORK-INDEX)
                   END-IF

      *            CALCULATE DENIAL RATE
                   MOVE 0 TO WS-WORK-COUNT
                   PERFORM VARYING WS-WORK-INDEX2 FROM 1 BY 1
                       UNTIL WS-WORK-INDEX2 > 12
                       ADD WS-PMX-DENIED(WS-WORK-INDEX,
                           WS-WORK-INDEX2) TO WS-WORK-COUNT
                   END-PERFORM
                   IF WS-PMX-TOTAL-CLM(WS-WORK-INDEX) > 0
                       COMPUTE WS-PMX-DENIAL-RATE(WS-WORK-INDEX) =
                           (WS-WORK-COUNT /
                            WS-PMX-TOTAL-CLM(WS-WORK-INDEX)) * 100
                   END-IF

      *            CALCULATE COLLECTION RATE
                   IF WS-PMX-TOTAL-REV(WS-WORK-INDEX) > 0
                       COMPUTE WS-PMX-COLL-RATE(WS-WORK-INDEX) =
                           (WS-WORK-AMT1 /
                            WS-PMX-TOTAL-REV(WS-WORK-INDEX)) * 100
                   END-IF
               END-IF
           END-PERFORM.

      *================================================================*
      *    3330-LOAD-PRODUCT-LINE-DATA                                 *
      *================================================================*
       3330-LOAD-PRODUCT-LINE-DATA.

           EXEC SQL
               SELECT
                   PC.PRODUCT_LINE,
                   SUM(CH.BILLED_AMOUNT) AS REVENUE,
                   COUNT(*) AS CLM_COUNT,
                   SUM(CASE WHEN CH.CLAIM_STATUS = 'PD'
                       THEN CH.PAID_AMOUNT ELSE 0 END) AS PAID
               INTO :HV-PRODUCT-LINE,
                    :HV-SUM-AMT,
                    :HV-COUNT,
                    :WS-WORK-AMT1
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.PAYER_CONTRACT PC
                   ON CH.PAYER_ID = PC.PAYER_ID
               WHERE CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
               GROUP BY PC.PRODUCT_LINE
           END-EXEC

           IF SQLCODE = 0
               PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
                   UNTIL WS-WORK-INDEX > 7
                   IF WS-PL-CODE(WS-WORK-INDEX) = HV-PRODUCT-LINE
                       MOVE HV-SUM-AMT
                           TO WS-PL-REVENUE(WS-WORK-INDEX)
                       MOVE HV-COUNT
                           TO WS-PL-CLM-CNT(WS-WORK-INDEX)
                       MOVE WS-WORK-AMT1
                           TO WS-PL-PAID-AMT(WS-WORK-INDEX)
                       IF WS-PL-REVENUE(WS-WORK-INDEX) > 0
                           COMPUTE WS-PL-AVG-REIMB(WS-WORK-INDEX) =
                               WS-PL-PAID-AMT(WS-WORK-INDEX) /
                               WS-PL-REVENUE(WS-WORK-INDEX)
                       END-IF
                       IF WS-GRAND-TOTAL-AMT > 0
                           COMPUTE WS-PL-REV-PCT(WS-WORK-INDEX) =
                               (WS-PL-REVENUE(WS-WORK-INDEX) /
                                WS-GRAND-TOTAL-AMT) * 100
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

      *================================================================*
      *    3340-WRITE-PAYER-MIX-REPORT                                 *
      *================================================================*
       3340-WRITE-PAYER-MIX-REPORT.

           PERFORM 3341-WRITE-PAYER-MIX-HEADERS

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
               IF WS-PMX-PAYER-ID(WS-WORK-INDEX) NOT = SPACES

                   IF WS-RPT03-LINE-CTR >= WS-MAX-LINES
                       PERFORM 3341-WRITE-PAYER-MIX-HEADERS
                   END-IF

                   MOVE WS-PMX-PAYER-NAME(WS-WORK-INDEX)
                       TO WS-R03D-PAYER-NAME
                   EVALUATE WS-PMX-PAYER-TYPE(WS-WORK-INDEX)
                       WHEN 'CM' MOVE 'COMMERCIAL  '
                           TO WS-R03D-TYPE
                       WHEN 'MC' MOVE 'MEDICARE    '
                           TO WS-R03D-TYPE
                       WHEN 'MD' MOVE 'MEDICAID    '
                           TO WS-R03D-TYPE
                       WHEN 'SP' MOVE 'SELF-PAY    '
                           TO WS-R03D-TYPE
                       WHEN 'WC' MOVE 'WORKERS COMP'
                           TO WS-R03D-TYPE
                       WHEN 'TR' MOVE 'TRICARE     '
                           TO WS-R03D-TYPE
                       WHEN 'VA' MOVE 'VA          '
                           TO WS-R03D-TYPE
                       WHEN OTHER MOVE 'OTHER       '
                           TO WS-R03D-TYPE
                   END-EVALUATE
                   MOVE WS-PMX-TOTAL-REV(WS-WORK-INDEX)
                       TO WS-R03D-REVENUE
                   MOVE WS-PMX-REV-PCT(WS-WORK-INDEX)
                       TO WS-R03D-REV-PCT
                   MOVE WS-PMX-TOTAL-CLM(WS-WORK-INDEX)
                       TO WS-R03D-CLM-CNT
                   MOVE WS-PMX-AVG-REIMB(WS-WORK-INDEX)
                       TO WS-R03D-AVG-REIMB
                   MOVE WS-PMX-DENIAL-RATE(WS-WORK-INDEX)
                       TO WS-R03D-DENY-RATE
                   MOVE WS-PMX-COLL-RATE(WS-WORK-INDEX)
                       TO WS-R03D-COLL-RATE

                   WRITE RPT03-RECORD FROM WS-RPT03-DETAIL
                   ADD 1 TO WS-RPT03-LINE-CTR
                   ADD 1 TO WS-RPT03-RECORDS
               END-IF
           END-PERFORM

      *    WRITE PRODUCT LINE SECTION
           WRITE RPT03-RECORD FROM WS-BLANK-LINE
           WRITE RPT03-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  PRODUCT LINE BREAKDOWN:' TO WS-WORK-STRING
           WRITE RPT03-RECORD FROM WS-WORK-STRING
           WRITE RPT03-RECORD FROM WS-SEPARATOR-LINE
           ADD 4 TO WS-RPT03-LINE-CTR

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 7
               IF WS-PL-CLM-CNT(WS-WORK-INDEX) > 0
                   MOVE WS-PL-DESC(WS-WORK-INDEX)
                       TO WS-R03D-PAYER-NAME
                   MOVE WS-PL-CODE(WS-WORK-INDEX)
                       TO WS-R03D-TYPE
                   MOVE WS-PL-REVENUE(WS-WORK-INDEX)
                       TO WS-R03D-REVENUE
                   MOVE WS-PL-REV-PCT(WS-WORK-INDEX)
                       TO WS-R03D-REV-PCT
                   MOVE WS-PL-CLM-CNT(WS-WORK-INDEX)
                       TO WS-R03D-CLM-CNT
                   MOVE WS-PL-AVG-REIMB(WS-WORK-INDEX)
                       TO WS-R03D-AVG-REIMB
                   MOVE 0 TO WS-R03D-DENY-RATE
                   MOVE 0 TO WS-R03D-COLL-RATE

                   WRITE RPT03-RECORD FROM WS-RPT03-DETAIL
                   ADD 1 TO WS-RPT03-LINE-CTR
                   ADD 1 TO WS-RPT03-RECORDS
               END-IF
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT03' TO WS-AUD-REPORT-ID
           MOVE WS-RPT03-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'PAYER MIX ANALYSIS COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3341-WRITE-PAYER-MIX-HEADERS                                *
      *================================================================*
       3341-WRITE-PAYER-MIX-HEADERS.

           ADD 1 TO WS-RPT03-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R03H1-DATE
           MOVE WS-RPT03-PAGE-CTR   TO WS-R03H1-PAGE
           WRITE RPT03-RECORD FROM WS-RPT03-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT03-RECORD FROM WS-RPT03-HEADER2
           WRITE RPT03-RECORD FROM WS-BLANK-LINE
           WRITE RPT03-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT03-LINE-CTR.

      *================================================================*
      *    3400-BUILD-DENIAL-ANALYSIS                                  *
      *    REPORT 04: COMPREHENSIVE DENIAL ANALYSIS                    *
      *    TOP 50 DENIAL REASONS, RATES BY PAYER/TYPE, TRENDS,         *
      *    ROOT CAUSE CATEGORIZATION, APPEAL OVERTURN TRACKING         *
      *================================================================*
       3400-BUILD-DENIAL-ANALYSIS.

           DISPLAY 'HCRPTGEN - BUILDING RPT04 DENIAL ANALYSIS'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT04' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'DENIAL ANALYSIS REPORT STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    GET TOTAL CLAIMS FOR DENOMINATOR
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-DEN-TOTAL-CLAIMS
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY TOP 50 DENIAL REASONS
           EXEC SQL
               DECLARE CSR-DENIALS CURSOR FOR
               SELECT
                   DR.DENIAL_CODE,
                   DR.DENIAL_DESCRIPTION,
                   DR.DENIAL_CATEGORY,
                   COUNT(*) AS DENIAL_COUNT,
                   SUM(CH.BILLED_AMOUNT) AS DENIAL_AMOUNT,
                   SUM(CASE WHEN CH.APPEAL_STATUS IS NOT NULL
                       THEN 1 ELSE 0 END) AS APPEAL_COUNT,
                   SUM(CASE WHEN CH.APPEAL_STATUS = 'OV'
                       THEN 1 ELSE 0 END) AS OVERTURN_COUNT
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.DENIAL_REASON DR
                   ON CH.DENIAL_REASON_CODE = DR.DENIAL_CODE
               WHERE CH.CLAIM_STATUS = 'DN'
                 AND CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
               GROUP BY DR.DENIAL_CODE, DR.DENIAL_DESCRIPTION,
                        DR.DENIAL_CATEGORY
               ORDER BY DENIAL_COUNT DESC
               FETCH FIRST 50 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-DENIALS END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3400-EXIT
           END-IF

           MOVE 0 TO WS-DENIAL-INDEX
           SET NOT-EOF-CURSOR TO TRUE

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-DENIALS
                   INTO :HV-DENIAL-CODE,
                        :HV-DENIAL-DESC,
                        :HV-DENIAL-CATEGORY,
                        :HV-COUNT,
                        :HV-SUM-AMT,
                        :WS-WORK-COUNT,
                        :WS-WORK-COUNT2
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1 TO WS-DENIAL-INDEX
                       IF WS-DENIAL-INDEX <= 50
                           MOVE HV-DENIAL-CODE
                               TO WS-DEN-CODE(WS-DENIAL-INDEX)
                           MOVE HV-DENIAL-DESC
                               TO WS-DEN-DESC(WS-DENIAL-INDEX)
                           MOVE HV-DENIAL-CATEGORY
                               TO WS-DEN-CATEGORY(WS-DENIAL-INDEX)
                           MOVE HV-COUNT
                               TO WS-DEN-COUNT(WS-DENIAL-INDEX)
                           MOVE HV-SUM-AMT
                               TO WS-DEN-AMOUNT(WS-DENIAL-INDEX)
                           MOVE WS-WORK-COUNT
                               TO WS-DEN-APPEAL-CNT(WS-DENIAL-INDEX)
                           MOVE WS-WORK-COUNT2
                               TO WS-DEN-OVERTURN(WS-DENIAL-INDEX)

                           ADD HV-COUNT TO WS-DEN-TOTAL-COUNT
                           ADD HV-SUM-AMT TO WS-DEN-TOTAL-AMOUNT

      *                    CALCULATE DENIAL RATE
                           IF WS-DEN-TOTAL-CLAIMS > 0
                               COMPUTE WS-DEN-RATE(
                                   WS-DENIAL-INDEX) =
                                   (HV-COUNT /
                                    WS-DEN-TOTAL-CLAIMS) * 100
                           END-IF
                       END-IF
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-DENIALS END-EXEC

      *    LOAD DENIAL BY PAYER
           PERFORM 3410-LOAD-DENIAL-BY-PAYER

      *    LOAD DENIAL CATEGORY SUMMARY
           PERFORM 3420-CALC-DENIAL-CATEGORIES

      *    LOAD TREND DATA (CURRENT VS PRIOR)
           PERFORM 3430-LOAD-DENIAL-TRENDS

      *    WRITE THE REPORT
           PERFORM 3440-WRITE-DENIAL-REPORT

       3400-EXIT.
           EXIT.

      *================================================================*
      *    3410-LOAD-DENIAL-BY-PAYER                                   *
      *================================================================*
       3410-LOAD-DENIAL-BY-PAYER.

           EXEC SQL
               DECLARE CSR-DENPAYER CURSOR FOR
               SELECT
                   PC.PAYER_ID,
                   PC.PAYER_NAME,
                   COUNT(*) AS TOTAL_CLAIMS,
                   SUM(CASE WHEN CH.CLAIM_STATUS = 'DN'
                       THEN 1 ELSE 0 END) AS DENIED_CLAIMS,
                   SUM(CASE WHEN CH.CLAIM_STATUS = 'DN'
                       THEN CH.BILLED_AMOUNT ELSE 0 END)
                       AS DENIED_AMOUNT
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.PAYER_CONTRACT PC
                   ON CH.PAYER_ID = PC.PAYER_ID
               WHERE CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
               GROUP BY PC.PAYER_ID, PC.PAYER_NAME
               ORDER BY DENIED_CLAIMS DESC
               FETCH FIRST 20 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-DENPAYER END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
           ELSE
               MOVE 0 TO WS-PAYER-INDEX
               SET NOT-EOF-CURSOR TO TRUE

               PERFORM UNTIL EOF-CURSOR
                   EXEC SQL
                       FETCH CSR-DENPAYER
                       INTO :HV-PAYER-ID,
                            :HV-PAYER-NAME,
                            :HV-COUNT,
                            :WS-WORK-COUNT,
                            :HV-SUM-AMT
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-PAYER-INDEX
                       IF WS-PAYER-INDEX <= 20
                           MOVE HV-PAYER-ID
                               TO WS-DENP-PAYER-ID(WS-PAYER-INDEX)
                           MOVE HV-PAYER-NAME
                               TO WS-DENP-PAYER-NAME(WS-PAYER-INDEX)
                           MOVE HV-COUNT
                               TO WS-DENP-TOTAL-CLM(WS-PAYER-INDEX)
                           MOVE WS-WORK-COUNT
                               TO WS-DENP-DENIED-CLM(WS-PAYER-INDEX)
                           MOVE HV-SUM-AMT
                               TO WS-DENP-DENIED-AMT(WS-PAYER-INDEX)
                           IF HV-COUNT > 0
                               COMPUTE WS-DENP-DEN-RATE(
                                   WS-PAYER-INDEX) =
                                   (WS-WORK-COUNT / HV-COUNT) * 100
                           END-IF
                       END-IF
                   ELSE IF SQLCODE = 100
                       SET EOF-CURSOR TO TRUE
                   ELSE
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
                   END-IF
               END-PERFORM

               EXEC SQL CLOSE CSR-DENPAYER END-EXEC
           END-IF.

      *================================================================*
      *    3420-CALC-DENIAL-CATEGORIES                                 *
      *================================================================*
       3420-CALC-DENIAL-CATEGORIES.

      *    ACCUMULATE DENIALS INTO ROOT CAUSE CATEGORIES
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 50
               IF WS-DEN-CODE(WS-WORK-INDEX) NOT = SPACES
                   PERFORM VARYING WS-WORK-INDEX2 FROM 1 BY 1
                       UNTIL WS-WORK-INDEX2 > 10
                       IF WS-DEN-CATEGORY(WS-WORK-INDEX) =
                           WS-DCAT-CODE(WS-WORK-INDEX2)
                           ADD WS-DEN-COUNT(WS-WORK-INDEX)
                               TO WS-DCAT-COUNT(WS-WORK-INDEX2)
                           ADD WS-DEN-AMOUNT(WS-WORK-INDEX)
                               TO WS-DCAT-AMOUNT(WS-WORK-INDEX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *    CALCULATE CATEGORY PERCENTAGES
           IF WS-DEN-TOTAL-COUNT > 0
               PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
                   UNTIL WS-WORK-INDEX > 10
                   IF WS-DCAT-COUNT(WS-WORK-INDEX) > 0
                       COMPUTE WS-DCAT-PCT(WS-WORK-INDEX) =
                           (WS-DCAT-COUNT(WS-WORK-INDEX) /
                            WS-DEN-TOTAL-COUNT) * 100
                   END-IF
               END-PERFORM
           END-IF.

      *================================================================*
      *    3430-LOAD-DENIAL-TRENDS                                     *
      *================================================================*
       3430-LOAD-DENIAL-TRENDS.

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 50
               IF WS-DEN-CODE(WS-WORK-INDEX) NOT = SPACES
                   MOVE WS-DEN-COUNT(WS-WORK-INDEX)
                       TO WS-DEN-TREND-CURR(WS-WORK-INDEX)

                   MOVE WS-DEN-CODE(WS-WORK-INDEX)
                       TO HV-DENIAL-CODE
                   EXEC SQL
                       SELECT COUNT(*)
                       INTO :WS-DEN-TREND-PRIOR(:WS-WORK-INDEX)
                       FROM HCDB.CLAIM_HEADER CH
                       WHERE CH.CLAIM_STATUS = 'DN'
                         AND CH.DENIAL_REASON_CODE = :HV-DENIAL-CODE
                         AND CH.SERVICE_DATE
                             BETWEEN :HV-PRIOR-START-DATE
                                 AND :HV-PRIOR-END-DATE
                   END-EXEC

                   IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                       MOVE 0 TO WS-DEN-TREND-PRIOR(WS-WORK-INDEX)
                   END-IF
               END-IF
           END-PERFORM.

      *================================================================*
      *    3440-WRITE-DENIAL-REPORT                                    *
      *================================================================*
       3440-WRITE-DENIAL-REPORT.

           PERFORM 3441-WRITE-DENIAL-HEADERS

      *    WRITE DENIAL REASON DETAILS
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 50
               IF WS-DEN-CODE(WS-WORK-INDEX) NOT = SPACES
                   IF WS-RPT04-LINE-CTR >= WS-MAX-LINES
                       PERFORM 3441-WRITE-DENIAL-HEADERS
                   END-IF

                   MOVE WS-DEN-CODE(WS-WORK-INDEX)
                       TO WS-R04D-CODE
                   MOVE WS-DEN-DESC(WS-WORK-INDEX)(1:35)
                       TO WS-R04D-DESC
                   EVALUATE WS-DEN-CATEGORY(WS-WORK-INDEX)
                       WHEN 'ELG' MOVE 'ELIGIBILTY'
                                      TO WS-R04D-CAT
                       WHEN 'ATH' MOVE 'AUTH      '
                                      TO WS-R04D-CAT
                       WHEN 'COD' MOVE 'CODING    '
                                      TO WS-R04D-CAT
                       WHEN 'TMF' MOVE 'TIMELY FIL'
                                      TO WS-R04D-CAT
                       WHEN 'MNC' MOVE 'MED NECESS'
                                      TO WS-R04D-CAT
                       WHEN 'BND' MOVE 'BUNDLING  '
                                      TO WS-R04D-CAT
                       WHEN 'DUP' MOVE 'DUPLICATE '
                                      TO WS-R04D-CAT
                       WHEN OTHER MOVE 'OTHER     '
                                      TO WS-R04D-CAT
                   END-EVALUATE
                   MOVE WS-DEN-COUNT(WS-WORK-INDEX)
                       TO WS-R04D-COUNT
                   MOVE WS-DEN-AMOUNT(WS-WORK-INDEX)
                       TO WS-R04D-AMOUNT
                   MOVE WS-DEN-RATE(WS-WORK-INDEX)
                       TO WS-R04D-RATE
                   MOVE WS-DEN-APPEAL-CNT(WS-WORK-INDEX)
                       TO WS-R04D-APPEAL
                   MOVE WS-DEN-OVERTURN(WS-WORK-INDEX)
                       TO WS-R04D-OVERTURN

      *            DETERMINE TREND DIRECTION
                   IF WS-DEN-TREND-CURR(WS-WORK-INDEX) >
                       WS-DEN-TREND-PRIOR(WS-WORK-INDEX)
                       MOVE ' UP  ' TO WS-R04D-TREND
                   ELSE IF WS-DEN-TREND-CURR(WS-WORK-INDEX) <
                       WS-DEN-TREND-PRIOR(WS-WORK-INDEX)
                       MOVE ' DOWN' TO WS-R04D-TREND
                   ELSE
                       MOVE ' FLAT' TO WS-R04D-TREND
                   END-IF

                   WRITE RPT04-RECORD FROM WS-RPT04-DETAIL
                   ADD 1 TO WS-RPT04-LINE-CTR
                   ADD 1 TO WS-RPT04-RECORDS
               END-IF
           END-PERFORM

      *    WRITE CATEGORY SUMMARY SECTION
           WRITE RPT04-RECORD FROM WS-BLANK-LINE
           WRITE RPT04-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  ROOT CAUSE CATEGORY SUMMARY:' TO WS-WORK-STRING
           WRITE RPT04-RECORD FROM WS-WORK-STRING
           WRITE RPT04-RECORD FROM WS-SEPARATOR-LINE
           ADD 4 TO WS-RPT04-LINE-CTR

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 10
               IF WS-DCAT-COUNT(WS-WORK-INDEX) > 0
                   MOVE WS-DCAT-CODE(WS-WORK-INDEX)
                       TO WS-R04D-CODE
                   MOVE WS-DCAT-DESC(WS-WORK-INDEX)(1:35)
                       TO WS-R04D-DESC
                   MOVE SPACES TO WS-R04D-CAT
                   MOVE WS-DCAT-COUNT(WS-WORK-INDEX)
                       TO WS-R04D-COUNT
                   MOVE WS-DCAT-AMOUNT(WS-WORK-INDEX)
                       TO WS-R04D-AMOUNT
                   MOVE WS-DCAT-PCT(WS-WORK-INDEX)
                       TO WS-R04D-RATE
                   MOVE 0 TO WS-R04D-APPEAL
                   MOVE 0 TO WS-R04D-OVERTURN
                   MOVE SPACES TO WS-R04D-TREND

                   WRITE RPT04-RECORD FROM WS-RPT04-DETAIL
                   ADD 1 TO WS-RPT04-LINE-CTR
                   ADD 1 TO WS-RPT04-RECORDS
               END-IF
           END-PERFORM

      *    WRITE DENIAL BY PAYER SECTION
           WRITE RPT04-RECORD FROM WS-BLANK-LINE
           MOVE '  DENIAL RATE BY PAYER:' TO WS-WORK-STRING
           WRITE RPT04-RECORD FROM WS-WORK-STRING
           WRITE RPT04-RECORD FROM WS-SEPARATOR-LINE
           ADD 3 TO WS-RPT04-LINE-CTR

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 20
               IF WS-DENP-PAYER-ID(WS-WORK-INDEX) NOT = SPACES
                   MOVE WS-DENP-PAYER-ID(WS-WORK-INDEX)(1:5)
                       TO WS-R04D-CODE
                   MOVE WS-DENP-PAYER-NAME(WS-WORK-INDEX)(1:35)
                       TO WS-R04D-DESC
                   MOVE SPACES TO WS-R04D-CAT
                   MOVE WS-DENP-DENIED-CLM(WS-WORK-INDEX)
                       TO WS-R04D-COUNT
                   MOVE WS-DENP-DENIED-AMT(WS-WORK-INDEX)
                       TO WS-R04D-AMOUNT
                   MOVE WS-DENP-DEN-RATE(WS-WORK-INDEX)
                       TO WS-R04D-RATE
                   MOVE 0 TO WS-R04D-APPEAL
                   MOVE 0 TO WS-R04D-OVERTURN
                   MOVE SPACES TO WS-R04D-TREND

                   WRITE RPT04-RECORD FROM WS-RPT04-DETAIL
                   ADD 1 TO WS-RPT04-LINE-CTR
                   ADD 1 TO WS-RPT04-RECORDS
               END-IF
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT04' TO WS-AUD-REPORT-ID
           MOVE WS-RPT04-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'DENIAL ANALYSIS REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3441-WRITE-DENIAL-HEADERS                                   *
      *================================================================*
       3441-WRITE-DENIAL-HEADERS.

           ADD 1 TO WS-RPT04-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R04H1-DATE
           MOVE WS-RPT04-PAGE-CTR   TO WS-R04H1-PAGE
           WRITE RPT04-RECORD FROM WS-RPT04-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT04-RECORD FROM WS-RPT04-HEADER2
           WRITE RPT04-RECORD FROM WS-BLANK-LINE
           WRITE RPT04-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT04-LINE-CTR.

      *================================================================*
      *    3500-BUILD-FINANCIAL-SUMMARY                                *
      *    REPORT 05: EXECUTIVE FINANCIAL DASHBOARD                    *
      *    CHARGES, ALLOWED, PAID, ADJUSTMENTS, PATIENT RESP,          *
      *    MLR, ADMIN RATIO, IBNR, BUDGET VARIANCE                    *
      *================================================================*
       3500-BUILD-FINANCIAL-SUMMARY.

           DISPLAY 'HCRPTGEN - BUILDING RPT05 FINANCIAL SUMMARY'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT05' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'FINANCIAL SUMMARY STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY MTD FINANCIAL SUMMARY
           EXEC SQL
               SELECT
                   COALESCE(SUM(CH.BILLED_AMOUNT), 0),
                   COALESCE(SUM(CH.ALLOWED_AMOUNT), 0),
                   COALESCE(SUM(CASE WHEN CH.CLAIM_STATUS = 'PD'
                       THEN CH.PAID_AMOUNT ELSE 0 END), 0),
                   COALESCE(SUM(CASE WHEN CH.CLAIM_STATUS = 'PD'
                       THEN CH.BILLED_AMOUNT - CH.ALLOWED_AMOUNT
                       ELSE 0 END), 0),
                   COALESCE(SUM(CH.WRITEOFF_AMOUNT), 0),
                   COALESCE(SUM(CH.BADDEBT_AMOUNT), 0),
                   COALESCE(SUM(CH.DEDUCTIBLE_AMOUNT), 0),
                   COALESCE(SUM(CH.COPAY_AMOUNT), 0),
                   COALESCE(SUM(CH.COINSURANCE_AMOUNT), 0)
               INTO :WS-FIN-MTD-CHARGES,
                    :WS-FIN-MTD-ALLOWED,
                    :WS-FIN-MTD-PAID,
                    :WS-FIN-MTD-CONTRACTUAL,
                    :WS-FIN-MTD-WRITEOFF,
                    :WS-FIN-MTD-BADDEBT,
                    :WS-FIN-MTD-DEDUCT,
                    :WS-FIN-MTD-COPAY,
                    :WS-FIN-MTD-COINS
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               PERFORM 8100-LOG-SQL-ERROR
           END-IF

      *    CALCULATE DERIVED MTD FIELDS
           COMPUTE WS-FIN-MTD-PAT-RESP =
               WS-FIN-MTD-DEDUCT + WS-FIN-MTD-COPAY +
               WS-FIN-MTD-COINS

           COMPUTE WS-FIN-MTD-NET-REV =
               WS-FIN-MTD-PAID - WS-FIN-MTD-WRITEOFF -
               WS-FIN-MTD-BADDEBT

           IF WS-FIN-MTD-CHARGES > 0
               COMPUTE WS-FIN-MTD-AVG-REIMB =
                   WS-FIN-MTD-PAID / WS-FIN-MTD-CHARGES
               COMPUTE WS-FIN-MTD-COLLECT-RATE =
                   WS-FIN-MTD-PAID / WS-FIN-MTD-CHARGES
               COMPUTE WS-FIN-DISCOUNT-PCT =
                   (WS-FIN-MTD-CHARGES - WS-FIN-MTD-ALLOWED) /
                   WS-FIN-MTD-CHARGES * 100
           END-IF

      *    QUERY PREMIUM DATA FOR MLR CALCULATION
           EXEC SQL
               SELECT COALESCE(SUM(PREMIUM_AMOUNT), 0)
               INTO :WS-FIN-MTD-PREMIUM
               FROM HCDB.PREMIUM_REVENUE
               WHERE PREMIUM_MONTH BETWEEN :HV-RPT-START-DATE
                                        AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-FIN-MTD-PREMIUM > 0
               COMPUTE WS-FIN-MTD-MLR =
                   WS-FIN-MTD-PAID / WS-FIN-MTD-PREMIUM
           END-IF

      *    QUERY ADMINISTRATIVE EXPENSES
           EXEC SQL
               SELECT COALESCE(SUM(EXPENSE_AMOUNT), 0)
               INTO :WS-FIN-MTD-ADMIN-EXP
               FROM HCDB.ADMIN_EXPENSES
               WHERE EXPENSE_MONTH BETWEEN :HV-RPT-START-DATE
                                        AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-FIN-MTD-PREMIUM > 0
               COMPUTE WS-FIN-MTD-ADMIN-RATIO =
                   WS-FIN-MTD-ADMIN-EXP / WS-FIN-MTD-PREMIUM
           END-IF

      *    QUERY YTD FINANCIAL SUMMARY (SAME STRUCTURE)
           EXEC SQL
               SELECT
                   COALESCE(SUM(CH.BILLED_AMOUNT), 0),
                   COALESCE(SUM(CH.ALLOWED_AMOUNT), 0),
                   COALESCE(SUM(CASE WHEN CH.CLAIM_STATUS = 'PD'
                       THEN CH.PAID_AMOUNT ELSE 0 END), 0),
                   COALESCE(SUM(CASE WHEN CH.CLAIM_STATUS = 'PD'
                       THEN CH.BILLED_AMOUNT - CH.ALLOWED_AMOUNT
                       ELSE 0 END), 0),
                   COALESCE(SUM(CH.WRITEOFF_AMOUNT), 0),
                   COALESCE(SUM(CH.BADDEBT_AMOUNT), 0),
                   COALESCE(SUM(CH.DEDUCTIBLE_AMOUNT), 0),
                   COALESCE(SUM(CH.COPAY_AMOUNT), 0),
                   COALESCE(SUM(CH.COINSURANCE_AMOUNT), 0)
               INTO :WS-FIN-YTD-CHARGES,
                    :WS-FIN-YTD-ALLOWED,
                    :WS-FIN-YTD-PAID,
                    :WS-FIN-YTD-CONTRACTUAL,
                    :WS-FIN-YTD-WRITEOFF,
                    :WS-FIN-YTD-BADDEBT,
                    :WS-FIN-YTD-DEDUCT,
                    :WS-FIN-YTD-COPAY,
                    :WS-FIN-YTD-COINS
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.SERVICE_DATE BETWEEN :HV-YTD-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

           COMPUTE WS-FIN-YTD-PAT-RESP =
               WS-FIN-YTD-DEDUCT + WS-FIN-YTD-COPAY +
               WS-FIN-YTD-COINS

           COMPUTE WS-FIN-YTD-NET-REV =
               WS-FIN-YTD-PAID - WS-FIN-YTD-WRITEOFF -
               WS-FIN-YTD-BADDEBT

      *    QUERY YTD PREMIUM FOR MLR
           EXEC SQL
               SELECT COALESCE(SUM(PREMIUM_AMOUNT), 0)
               INTO :WS-FIN-YTD-PREMIUM
               FROM HCDB.PREMIUM_REVENUE
               WHERE PREMIUM_MONTH BETWEEN :HV-YTD-START-DATE
                                        AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-FIN-YTD-PREMIUM > 0
               COMPUTE WS-FIN-YTD-MLR =
                   WS-FIN-YTD-PAID / WS-FIN-YTD-PREMIUM
           END-IF

      *    CALCULATE IBNR ESTIMATE (DEVELOPMENT FACTOR METHOD)
      *    USES 3-MONTH LAG FACTOR BASED ON HISTORICAL COMPLETION
           EXEC SQL
               SELECT COALESCE(SUM(CH.BILLED_AMOUNT), 0)
               INTO :WS-WORK-AMT1
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
                 AND CH.RECEIPT_DATE IS NULL
           END-EXEC
           COMPUTE WS-FIN-IBNR-EST =
               WS-FIN-MTD-PAID * 0.08

      *    LOAD BUDGET DATA
           EXEC SQL
               SELECT
                   COALESCE(SUM(BUDGET_CHARGES), 0),
                   COALESCE(SUM(BUDGET_PAID), 0),
                   COALESCE(SUM(BUDGET_NET_REVENUE), 0)
               INTO :WS-FIN-BUD-CHARGES,
                    :WS-FIN-BUD-PAID,
                    :WS-FIN-BUD-NET-REV
               FROM HCDB.BUDGET_MASTER
               WHERE BUDGET_MONTH BETWEEN :HV-RPT-START-DATE
                                       AND :HV-RPT-END-DATE
           END-EXEC

      *    WRITE THE FINANCIAL REPORT
           PERFORM 3510-WRITE-FINANCIAL-REPORT

       3500-EXIT.
           EXIT.

      *================================================================*
      *    3510-WRITE-FINANCIAL-REPORT                                 *
      *================================================================*
       3510-WRITE-FINANCIAL-REPORT.

           ADD 1 TO WS-RPT05-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R05H1-DATE
           MOVE WS-RPT05-PAGE-CTR   TO WS-R05H1-PAGE
           WRITE RPT05-RECORD FROM WS-RPT05-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT05-RECORD FROM WS-RPT05-HEADER2
           WRITE RPT05-RECORD FROM WS-BLANK-LINE
           WRITE RPT05-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT05-LINE-CTR

      *    TOTAL CHARGES
           MOVE 'TOTAL CHARGES                      '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-CHARGES TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-CHARGES TO WS-R05D-YTD-AMT
           MOVE WS-FIN-BUD-CHARGES TO WS-R05D-BUDGET-AMT
           COMPUTE WS-VARIANCE-AMT =
               WS-FIN-MTD-CHARGES - WS-FIN-BUD-CHARGES
           MOVE WS-VARIANCE-AMT TO WS-R05D-VARIANCE
           IF WS-FIN-BUD-CHARGES > 0
               COMPUTE WS-VARIANCE-PCT =
                   (WS-VARIANCE-AMT / WS-FIN-BUD-CHARGES) * 100
           ELSE
               MOVE 0 TO WS-VARIANCE-PCT
           END-IF
           MOVE WS-VARIANCE-PCT TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    TOTAL ALLOWED
           MOVE 'TOTAL ALLOWED                      '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-ALLOWED TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-ALLOWED TO WS-R05D-YTD-AMT
           MOVE 0 TO WS-R05D-BUDGET-AMT
           MOVE 0 TO WS-R05D-VARIANCE
           MOVE 0 TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    TOTAL PAID
           MOVE 'TOTAL PAID                         '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-PAID TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-PAID TO WS-R05D-YTD-AMT
           MOVE WS-FIN-BUD-PAID TO WS-R05D-BUDGET-AMT
           COMPUTE WS-VARIANCE-AMT =
               WS-FIN-MTD-PAID - WS-FIN-BUD-PAID
           MOVE WS-VARIANCE-AMT TO WS-R05D-VARIANCE
           IF WS-FIN-BUD-PAID > 0
               COMPUTE WS-VARIANCE-PCT =
                   (WS-VARIANCE-AMT / WS-FIN-BUD-PAID) * 100
           ELSE
               MOVE 0 TO WS-VARIANCE-PCT
           END-IF
           MOVE WS-VARIANCE-PCT TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    CONTRACTUAL ADJUSTMENTS
           MOVE 'CONTRACTUAL ADJUSTMENTS            '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-CONTRACTUAL TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-CONTRACTUAL TO WS-R05D-YTD-AMT
           MOVE 0 TO WS-R05D-BUDGET-AMT
           MOVE 0 TO WS-R05D-VARIANCE
           MOVE 0 TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    WRITE-OFFS
           MOVE 'WRITE-OFFS                         '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-WRITEOFF TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-WRITEOFF TO WS-R05D-YTD-AMT
           MOVE 0 TO WS-R05D-BUDGET-AMT
           MOVE 0 TO WS-R05D-VARIANCE
           MOVE 0 TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    BAD DEBT
           MOVE 'BAD DEBT                           '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-BADDEBT TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-BADDEBT TO WS-R05D-YTD-AMT
           MOVE 0 TO WS-R05D-BUDGET-AMT
           MOVE 0 TO WS-R05D-VARIANCE
           MOVE 0 TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    PATIENT RESPONSIBILITY
           MOVE 'PATIENT RESPONSIBILITY (DED+COP+COI'
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-PAT-RESP TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-PAT-RESP TO WS-R05D-YTD-AMT
           MOVE 0 TO WS-R05D-BUDGET-AMT
           MOVE 0 TO WS-R05D-VARIANCE
           MOVE 0 TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

           WRITE RPT05-RECORD FROM WS-DOUBLE-SEP-LINE

      *    NET REVENUE
           MOVE 'NET REVENUE                        '
               TO WS-R05D-LABEL
           MOVE WS-FIN-MTD-NET-REV TO WS-R05D-MTD-AMT
           MOVE WS-FIN-YTD-NET-REV TO WS-R05D-YTD-AMT
           MOVE WS-FIN-BUD-NET-REV TO WS-R05D-BUDGET-AMT
           COMPUTE WS-VARIANCE-AMT =
               WS-FIN-MTD-NET-REV - WS-FIN-BUD-NET-REV
           MOVE WS-VARIANCE-AMT TO WS-R05D-VARIANCE
           IF WS-FIN-BUD-NET-REV > 0
               COMPUTE WS-VARIANCE-PCT =
                   (WS-VARIANCE-AMT / WS-FIN-BUD-NET-REV) * 100
           ELSE
               MOVE 0 TO WS-VARIANCE-PCT
           END-IF
           MOVE WS-VARIANCE-PCT TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    IBNR ESTIMATE
           MOVE 'IBNR ESTIMATE                      '
               TO WS-R05D-LABEL
           MOVE WS-FIN-IBNR-EST TO WS-R05D-MTD-AMT
           MOVE 0 TO WS-R05D-YTD-AMT
           MOVE 0 TO WS-R05D-BUDGET-AMT
           MOVE 0 TO WS-R05D-VARIANCE
           MOVE 0 TO WS-R05D-VAR-PCT
           WRITE RPT05-RECORD FROM WS-RPT05-DETAIL
           ADD 1 TO WS-RPT05-LINE-CTR
           ADD 1 TO WS-RPT05-RECORDS

      *    KEY RATIOS SECTION
           WRITE RPT05-RECORD FROM WS-BLANK-LINE
           WRITE RPT05-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  KEY FINANCIAL RATIOS:' TO WS-WORK-STRING
           WRITE RPT05-RECORD FROM WS-WORK-STRING
           WRITE RPT05-RECORD FROM WS-SEPARATOR-LINE

      *    MEDICAL LOSS RATIO
           MOVE 'MEDICAL LOSS RATIO (MLR)           '
               TO WS-R05R-LABEL
           COMPUTE WS-R05R-MTD-RATIO = WS-FIN-MTD-MLR * 100
           COMPUTE WS-R05R-YTD-RATIO = WS-FIN-YTD-MLR * 100
           WRITE RPT05-RECORD FROM WS-RPT05-RATIO-LINE
           ADD 1 TO WS-RPT05-RECORDS

      *    ADMINISTRATIVE EXPENSE RATIO
           MOVE 'ADMINISTRATIVE EXPENSE RATIO        '
               TO WS-R05R-LABEL
           COMPUTE WS-R05R-MTD-RATIO =
               WS-FIN-MTD-ADMIN-RATIO * 100
           MOVE 0 TO WS-R05R-YTD-RATIO
           WRITE RPT05-RECORD FROM WS-RPT05-RATIO-LINE
           ADD 1 TO WS-RPT05-RECORDS

      *    COLLECTION RATE
           MOVE 'COLLECTION RATE                    '
               TO WS-R05R-LABEL
           COMPUTE WS-R05R-MTD-RATIO =
               WS-FIN-MTD-COLLECT-RATE * 100
           MOVE 0 TO WS-R05R-YTD-RATIO
           WRITE RPT05-RECORD FROM WS-RPT05-RATIO-LINE
           ADD 1 TO WS-RPT05-RECORDS

      *    DISCOUNT PERCENTAGE
           MOVE 'DISCOUNT PERCENTAGE                '
               TO WS-R05R-LABEL
           MOVE WS-FIN-DISCOUNT-PCT TO WS-R05R-MTD-RATIO
           MOVE 0 TO WS-R05R-YTD-RATIO
           WRITE RPT05-RECORD FROM WS-RPT05-RATIO-LINE
           ADD 1 TO WS-RPT05-RECORDS

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT05' TO WS-AUD-REPORT-ID
           MOVE WS-RPT05-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'FINANCIAL SUMMARY REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3600-BUILD-PEND-AGING                                       *
      *    REPORT 06: PEND QUEUE AGING ANALYSIS                        *
      *    PENDING CLAIMS BY QUEUE TYPE, AGING WITHIN QUEUE,           *
      *    EXAMINER ASSIGNMENT, PAST-DUE, AUTO-RESOLUTION CANDIDATES   *
      *================================================================*
       3600-BUILD-PEND-AGING.

           DISPLAY 'HCRPTGEN - BUILDING RPT06 PEND QUEUE AGING'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT06' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'PEND QUEUE AGING STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY PEND QUEUE SUMMARY BY QUEUE TYPE AND AGING
           EXEC SQL
               DECLARE CSR-PEND CURSOR FOR
               SELECT
                   PQ.QUEUE_CODE,
                   PQ.QUEUE_DESCRIPTION,
                   CASE
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 0 AND 30
                           THEN 1
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 31 AND 60
                           THEN 2
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 61 AND 90
                           THEN 3
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 91 AND 120
                           THEN 4
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 121 AND 180
                           THEN 5
                       ELSE 6
                   END AS BUCKET,
                   COUNT(*) AS PEND_COUNT,
                   SUM(CH.BILLED_AMOUNT) AS PEND_AMOUNT,
                   SUM(DAYS(CURRENT DATE)
                       - DAYS(PQ.PEND_DATE)) AS TOTAL_DAYS,
                   SUM(CASE WHEN PQ.EXPECTED_DATE < CURRENT DATE
                       THEN 1 ELSE 0 END) AS PAST_DUE,
                   SUM(CASE WHEN PQ.AUTO_RESOLVE_FLAG = 'Y'
                       THEN 1 ELSE 0 END) AS AUTO_CAND
               FROM HCDB.PEND_QUEUE PQ
               JOIN HCDB.CLAIM_HEADER CH
                   ON PQ.CLAIM_ID = CH.CLAIM_ID
               WHERE PQ.PEND_STATUS = 'OP'
               GROUP BY PQ.QUEUE_CODE, PQ.QUEUE_DESCRIPTION,
                   CASE
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 0 AND 30
                           THEN 1
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 31 AND 60
                           THEN 2
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 61 AND 90
                           THEN 3
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 91 AND 120
                           THEN 4
                       WHEN DAYS(CURRENT DATE)
                            - DAYS(PQ.PEND_DATE) BETWEEN 121 AND 180
                           THEN 5
                       ELSE 6
                   END
               ORDER BY PQ.QUEUE_CODE, BUCKET
           END-EXEC

           EXEC SQL OPEN CSR-PEND END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3600-EXIT
           END-IF

           SET NOT-EOF-CURSOR TO TRUE

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-PEND
                   INTO :HV-QUEUE-CODE,
                        :HV-QUEUE-DESC,
                        :HV-BUCKET-NBR,
                        :HV-COUNT,
                        :HV-SUM-AMT,
                        :HV-TOTAL-DAYS,
                        :WS-WORK-COUNT,
                        :WS-WORK-COUNT2
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3610-ACCUMULATE-PEND-DATA
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-PEND END-EXEC

      *    CALCULATE AVERAGES
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 8
               IF WS-PQ-TOTAL-CNT(WS-WORK-INDEX) > 0
                   COMPUTE WS-PQ-AVG-DAYS(WS-WORK-INDEX) =
                       WS-PQ-TOTAL-DAYS(WS-WORK-INDEX) /
                       WS-PQ-TOTAL-CNT(WS-WORK-INDEX)
               END-IF
               ADD WS-PQ-TOTAL-CNT(WS-WORK-INDEX)
                   TO WS-PEND-GRAND-CNT
               ADD WS-PQ-TOTAL-AMT(WS-WORK-INDEX)
                   TO WS-PEND-GRAND-AMT
           END-PERFORM

      *    WRITE THE PEND REPORT
           PERFORM 3620-WRITE-PEND-REPORT

       3600-EXIT.
           EXIT.

      *================================================================*
      *    3610-ACCUMULATE-PEND-DATA                                   *
      *================================================================*
       3610-ACCUMULATE-PEND-DATA.

      *    FIND MATCHING QUEUE SLOT
           MOVE 0 TO WS-WORK-INDEX3
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 8
                  OR WS-WORK-INDEX3 > 0
               IF WS-PQ-QUEUE-CODE(WS-WORK-INDEX) = HV-QUEUE-CODE
                   MOVE WS-WORK-INDEX TO WS-WORK-INDEX3
               ELSE IF WS-PQ-QUEUE-CODE(WS-WORK-INDEX) = SPACES
                   MOVE WS-WORK-INDEX TO WS-WORK-INDEX3
                   MOVE HV-QUEUE-CODE
                       TO WS-PQ-QUEUE-CODE(WS-WORK-INDEX3)
                   MOVE HV-QUEUE-DESC
                       TO WS-PQ-QUEUE-DESC(WS-WORK-INDEX3)
               END-IF
           END-PERFORM

           IF WS-WORK-INDEX3 > 0 AND WS-WORK-INDEX3 <= 8
               IF HV-BUCKET-NBR >= 1 AND HV-BUCKET-NBR <= 6
                   ADD HV-COUNT TO WS-PQ-BKT-CNT(
                       WS-WORK-INDEX3, HV-BUCKET-NBR)
                   ADD HV-SUM-AMT TO WS-PQ-BKT-AMT(
                       WS-WORK-INDEX3, HV-BUCKET-NBR)
               END-IF
               ADD HV-COUNT
                   TO WS-PQ-TOTAL-CNT(WS-WORK-INDEX3)
               ADD HV-SUM-AMT
                   TO WS-PQ-TOTAL-AMT(WS-WORK-INDEX3)
               ADD HV-TOTAL-DAYS
                   TO WS-PQ-TOTAL-DAYS(WS-WORK-INDEX3)
               ADD WS-WORK-COUNT
                   TO WS-PQ-PAST-DUE-CNT(WS-WORK-INDEX3)
               ADD WS-WORK-COUNT2
                   TO WS-PQ-AUTO-CAND(WS-WORK-INDEX3)
           END-IF.

      *================================================================*
      *    3620-WRITE-PEND-REPORT                                      *
      *================================================================*
       3620-WRITE-PEND-REPORT.

           ADD 1 TO WS-RPT06-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R06H1-DATE
           MOVE WS-RPT06-PAGE-CTR   TO WS-R06H1-PAGE
           WRITE RPT06-RECORD FROM WS-RPT06-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT06-RECORD FROM WS-RPT06-HEADER2
           WRITE RPT06-RECORD FROM WS-BLANK-LINE
           WRITE RPT06-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT06-LINE-CTR

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 8
               IF WS-PQ-QUEUE-CODE(WS-WORK-INDEX) NOT = SPACES
                   MOVE WS-PQ-QUEUE-DESC(WS-WORK-INDEX)
                       TO WS-R06D-QUEUE
                   MOVE WS-PQ-TOTAL-CNT(WS-WORK-INDEX)
                       TO WS-R06D-TOTAL-CNT
                   MOVE WS-PQ-TOTAL-AMT(WS-WORK-INDEX)
                       TO WS-R06D-TOTAL-AMT
                   MOVE WS-PQ-AVG-DAYS(WS-WORK-INDEX)
                       TO WS-R06D-AVG-DAYS
                   MOVE WS-PQ-PAST-DUE-CNT(WS-WORK-INDEX)
                       TO WS-R06D-PAST-DUE
                   MOVE WS-PQ-AUTO-CAND(WS-WORK-INDEX)
                       TO WS-R06D-AUTO-CAND

                   WRITE RPT06-RECORD FROM WS-RPT06-DETAIL
                   ADD 1 TO WS-RPT06-LINE-CTR
                   ADD 1 TO WS-RPT06-RECORDS

      *            WRITE BUCKET BREAKDOWN FOR EACH QUEUE
                   PERFORM VARYING WS-WORK-INDEX2 FROM 1 BY 1
                       UNTIL WS-WORK-INDEX2 > 6
                       IF WS-PQ-BKT-CNT(WS-WORK-INDEX,
                           WS-WORK-INDEX2) > 0
                           MOVE SPACES TO WS-WORK-STRING
                           STRING '    '
                               WS-BKT-LABEL(WS-WORK-INDEX2)
                               '              '
                               DELIMITED BY SIZE
                               INTO WS-WORK-STRING
                           MOVE WS-PQ-BKT-CNT(WS-WORK-INDEX,
                               WS-WORK-INDEX2) TO WS-R06D-TOTAL-CNT
                           MOVE WS-PQ-BKT-AMT(WS-WORK-INDEX,
                               WS-WORK-INDEX2) TO WS-R06D-TOTAL-AMT
                           MOVE WS-WORK-STRING(1:25)
                               TO WS-R06D-QUEUE
                           MOVE 0 TO WS-R06D-AVG-DAYS
                           MOVE 0 TO WS-R06D-PAST-DUE
                           MOVE 0 TO WS-R06D-AUTO-CAND
                           WRITE RPT06-RECORD FROM WS-RPT06-DETAIL
                           ADD 1 TO WS-RPT06-LINE-CTR
                           ADD 1 TO WS-RPT06-RECORDS
                       END-IF
                   END-PERFORM

                   WRITE RPT06-RECORD FROM WS-SEPARATOR-LINE
                   ADD 1 TO WS-RPT06-LINE-CTR
               END-IF
           END-PERFORM

      *    GRAND TOTAL
           WRITE RPT06-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 'GRAND TOTAL              '
               TO WS-R06D-QUEUE
           MOVE WS-PEND-GRAND-CNT TO WS-R06D-TOTAL-CNT
           MOVE WS-PEND-GRAND-AMT TO WS-R06D-TOTAL-AMT
           MOVE 0 TO WS-R06D-AVG-DAYS
           MOVE 0 TO WS-R06D-PAST-DUE
           MOVE 0 TO WS-R06D-AUTO-CAND
           WRITE RPT06-RECORD FROM WS-RPT06-DETAIL
           ADD 1 TO WS-RPT06-RECORDS

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT06' TO WS-AUD-REPORT-ID
           MOVE WS-RPT06-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'PEND QUEUE AGING REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3700-BUILD-AUTH-UTILIZATION                                  *
      *    REPORT 07: AUTHORIZATION UTILIZATION ANALYSIS               *
      *    VOLUME, APPROVAL/DENIAL RATES, TAT, CONVERSION,             *
      *    EXPIRED UNUSED, CONCURRENT/RETRO, BY SPECIALTY              *
      *================================================================*
       3700-BUILD-AUTH-UTILIZATION.

           DISPLAY 'HCRPTGEN - BUILDING RPT07 AUTH UTILIZATION'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT07' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'AUTHORIZATION UTILIZATION STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY OVERALL AUTH STATISTICS
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE WHEN AUTH_STATUS = 'AP' THEN 1
                       ELSE 0 END),
                   SUM(CASE WHEN AUTH_STATUS = 'DN' THEN 1
                       ELSE 0 END),
                   SUM(CASE WHEN AUTH_STATUS = 'PD' THEN 1
                       ELSE 0 END),
                   AVG(DAYS(DECISION_DATE) - DAYS(REQUEST_DATE)),
                   SUM(CASE WHEN AUTH_TYPE = 'CN' THEN 1
                       ELSE 0 END),
                   SUM(CASE WHEN AUTH_TYPE = 'CN'
                       AND AUTH_STATUS = 'AP' THEN 1 ELSE 0 END),
                   SUM(CASE WHEN AUTH_TYPE = 'RT' THEN 1
                       ELSE 0 END),
                   SUM(CASE WHEN AUTH_TYPE = 'RT'
                       AND AUTH_STATUS = 'AP' THEN 1 ELSE 0 END)
               INTO :WS-AUTH-TOTAL-REQUESTS,
                    :WS-AUTH-APPROVED,
                    :WS-AUTH-DENIED,
                    :WS-AUTH-PENDED,
                    :WS-AUTH-AVG-TAT,
                    :WS-AUTH-CONCURRENT-CNT,
                    :WS-AUTH-CONCURRENT-APP,
                    :WS-AUTH-RETRO-CNT,
                    :WS-AUTH-RETRO-APP
               FROM HCDB.AUTHORIZATION
               WHERE REQUEST_DATE BETWEEN :HV-RPT-START-DATE
                                       AND :HV-RPT-END-DATE
           END-EXEC

           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               PERFORM 8100-LOG-SQL-ERROR
           END-IF

      *    CALCULATE RATES
           IF WS-AUTH-TOTAL-REQUESTS > 0
               COMPUTE WS-AUTH-APPROVAL-RATE =
                   (WS-AUTH-APPROVED /
                    WS-AUTH-TOTAL-REQUESTS) * 100
               COMPUTE WS-AUTH-DENIAL-RATE =
                   (WS-AUTH-DENIED /
                    WS-AUTH-TOTAL-REQUESTS) * 100
           END-IF

      *    QUERY AUTH-TO-CLAIM CONVERSION
           EXEC SQL
               SELECT COUNT(DISTINCT A.AUTH_ID)
               INTO :WS-WORK-COUNT
               FROM HCDB.AUTHORIZATION A
               JOIN HCDB.CLAIM_HEADER CH
                   ON A.AUTH_ID = CH.AUTH_ID
               WHERE A.REQUEST_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
                 AND A.AUTH_STATUS = 'AP'
           END-EXEC

           IF WS-AUTH-APPROVED > 0
               COMPUTE WS-AUTH-TO-CLM-RATE =
                   (WS-WORK-COUNT / WS-AUTH-APPROVED) * 100
           END-IF

      *    QUERY SERVICES WITHOUT AUTH
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-AUTH-NO-AUTH-SVCS
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.AUTH_REQUIRED = 'Y'
                 AND CH.AUTH_ID IS NULL
                 AND CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY EXPIRED UNUSED AUTHORIZATIONS
           EXEC SQL
               SELECT COUNT(*),
                      COALESCE(SUM(A.APPROVED_AMOUNT), 0)
               INTO :WS-AUTH-EXPIRED-UNUSED,
                    :WS-AUTH-EXPIRED-AMT
               FROM HCDB.AUTHORIZATION A
               WHERE A.AUTH_STATUS = 'AP'
                 AND A.EXPIRATION_DATE < CURRENT DATE
                 AND NOT EXISTS (
                     SELECT 1 FROM HCDB.CLAIM_HEADER CH
                     WHERE CH.AUTH_ID = A.AUTH_ID
                 )
                 AND A.EXPIRATION_DATE BETWEEN :HV-RPT-START-DATE
                                            AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY AUTH STATS BY SPECIALTY
           EXEC SQL
               DECLARE CSR-AUTHSPEC CURSOR FOR
               SELECT
                   PM.SPECIALTY_CODE,
                   PM.SPECIALTY_DESC,
                   COUNT(*) AS REQUEST_CNT,
                   SUM(CASE WHEN A.AUTH_STATUS = 'AP'
                       THEN 1 ELSE 0 END) AS APPROVED_CNT,
                   SUM(CASE WHEN A.AUTH_STATUS = 'DN'
                       THEN 1 ELSE 0 END) AS DENIED_CNT,
                   AVG(DAYS(A.DECISION_DATE)
                       - DAYS(A.REQUEST_DATE)) AS AVG_TAT
               FROM HCDB.AUTHORIZATION A
               JOIN HCDB.PROVIDER_MASTER PM
                   ON A.PROVIDER_NPI = PM.PROVIDER_NPI
               WHERE A.REQUEST_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
               GROUP BY PM.SPECIALTY_CODE, PM.SPECIALTY_DESC
               ORDER BY REQUEST_CNT DESC
               FETCH FIRST 30 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-AUTHSPEC END-EXEC

           IF SQLCODE = 0
               MOVE 0 TO WS-SPEC-INDEX
               SET NOT-EOF-CURSOR TO TRUE

               PERFORM UNTIL EOF-CURSOR
                   EXEC SQL
                       FETCH CSR-AUTHSPEC
                       INTO :HV-PROVIDER-SPEC,
                            :HV-QUEUE-DESC,
                            :HV-COUNT,
                            :WS-WORK-COUNT,
                            :WS-WORK-COUNT2,
                            :HV-AVG-AMT
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-SPEC-INDEX
                       IF WS-SPEC-INDEX <= 30
                           MOVE HV-PROVIDER-SPEC
                               TO WS-ABS-SPEC-CODE(WS-SPEC-INDEX)
                           MOVE HV-QUEUE-DESC
                               TO WS-ABS-SPEC-DESC(WS-SPEC-INDEX)
                           MOVE HV-COUNT
                               TO WS-ABS-REQUEST-CNT(WS-SPEC-INDEX)
                           MOVE WS-WORK-COUNT
                               TO WS-ABS-APPROVED(WS-SPEC-INDEX)
                           MOVE WS-WORK-COUNT2
                               TO WS-ABS-DENIED(WS-SPEC-INDEX)
                           MOVE HV-AVG-AMT
                               TO WS-ABS-AVG-TAT(WS-SPEC-INDEX)
                       END-IF
                   ELSE IF SQLCODE = 100
                       SET EOF-CURSOR TO TRUE
                   ELSE
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
                   END-IF
               END-PERFORM

               EXEC SQL CLOSE CSR-AUTHSPEC END-EXEC
           END-IF

      *    WRITE THE AUTH REPORT
           PERFORM 3710-WRITE-AUTH-REPORT

       3700-EXIT.
           EXIT.

      *================================================================*
      *    3710-WRITE-AUTH-REPORT                                      *
      *================================================================*
       3710-WRITE-AUTH-REPORT.

           ADD 1 TO WS-RPT07-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R07H1-DATE
           MOVE WS-RPT07-PAGE-CTR   TO WS-R07H1-PAGE
           WRITE RPT07-RECORD FROM WS-RPT07-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT07-RECORD FROM WS-RPT07-HEADER2
           WRITE RPT07-RECORD FROM WS-BLANK-LINE
           WRITE RPT07-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT07-LINE-CTR

      *    OVERALL STATISTICS
           MOVE 'TOTAL AUTH REQUESTS                '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-TOTAL-REQUESTS TO WS-R07D-VALUE
           MOVE 0 TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'APPROVED                           '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-APPROVED TO WS-R07D-VALUE
           MOVE WS-AUTH-APPROVAL-RATE TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'DENIED                             '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-DENIED TO WS-R07D-VALUE
           MOVE WS-AUTH-DENIAL-RATE TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'AVERAGE TURNAROUND TIME (DAYS)     '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-AVG-TAT TO WS-R07D-VALUE
           MOVE 0 TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'AUTH-TO-CLAIM CONVERSION RATE       '
               TO WS-R07D-LABEL
           MOVE 0 TO WS-R07D-VALUE
           MOVE WS-AUTH-TO-CLM-RATE TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'SERVICES WITHOUT AUTHORIZATION     '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-NO-AUTH-SVCS TO WS-R07D-VALUE
           MOVE 0 TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'EXPIRED UNUSED AUTHORIZATIONS      '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-EXPIRED-UNUSED TO WS-R07D-VALUE
           MOVE 0 TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'CONCURRENT REVIEW TOTAL            '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-CONCURRENT-CNT TO WS-R07D-VALUE
           MOVE 0 TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

           MOVE 'RETROSPECTIVE REVIEW TOTAL         '
               TO WS-R07D-LABEL
           MOVE WS-AUTH-RETRO-CNT TO WS-R07D-VALUE
           MOVE 0 TO WS-R07D-RATE
           WRITE RPT07-RECORD FROM WS-RPT07-DETAIL
           ADD 1 TO WS-RPT07-RECORDS

      *    WRITE SPECIALTY BREAKDOWN
           WRITE RPT07-RECORD FROM WS-BLANK-LINE
           WRITE RPT07-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  AUTHORIZATION BY SPECIALTY:' TO WS-WORK-STRING
           WRITE RPT07-RECORD FROM WS-WORK-STRING
           WRITE RPT07-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 30
               IF WS-ABS-SPEC-CODE(WS-WORK-INDEX) NOT = SPACES
                   MOVE WS-ABS-SPEC-DESC(WS-WORK-INDEX)
                       TO WS-R07S-SPEC
                   MOVE WS-ABS-REQUEST-CNT(WS-WORK-INDEX)
                       TO WS-R07S-REQUESTS
                   MOVE WS-ABS-APPROVED(WS-WORK-INDEX)
                       TO WS-R07S-APPROVED
                   MOVE WS-ABS-DENIED(WS-WORK-INDEX)
                       TO WS-R07S-DENIED
                   IF WS-ABS-REQUEST-CNT(WS-WORK-INDEX) > 0
                       COMPUTE WS-WORK-RATE =
                           (WS-ABS-APPROVED(WS-WORK-INDEX) /
                            WS-ABS-REQUEST-CNT(WS-WORK-INDEX))
                           * 100
                   ELSE
                       MOVE 0 TO WS-WORK-RATE
                   END-IF
                   MOVE WS-WORK-RATE TO WS-R07S-APP-RATE
                   MOVE WS-ABS-AVG-TAT(WS-WORK-INDEX)
                       TO WS-R07S-AVG-TAT

                   WRITE RPT07-RECORD FROM WS-RPT07-SPEC-DETAIL
                   ADD 1 TO WS-RPT07-LINE-CTR
                   ADD 1 TO WS-RPT07-RECORDS
               END-IF
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT07' TO WS-AUD-REPORT-ID
           MOVE WS-RPT07-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'AUTH UTILIZATION REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3800-BUILD-HIGH-DOLLAR                                      *
      *    REPORT 08: HIGH-DOLLAR CLAIMS TRACKING                      *
      *    CLAIMS BY THRESHOLD TIER, REINSURANCE/STOP-LOSS,            *
      *    CASE MANAGEMENT, PROJECTED COSTS                            *
      *================================================================*
       3800-BUILD-HIGH-DOLLAR.

           DISPLAY 'HCRPTGEN - BUILDING RPT08 HIGH-DOLLAR CLAIMS'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT08' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'HIGH-DOLLAR CLAIMS REPORT STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY HIGH-DOLLAR CLAIM DETAILS
           EXEC SQL
               DECLARE CSR-HIGHDOL CURSOR FOR
               SELECT
                   CH.CLAIM_ID,
                   CH.MEMBER_ID,
                   PM.PROVIDER_NAME,
                   CH.PRIMARY_DIAG,
                   CH.PRIMARY_PROC,
                   CH.BILLED_AMOUNT,
                   COALESCE(CH.PAID_AMOUNT, 0),
                   CH.CLAIM_STATUS,
                   COALESCE(CH.CASE_MGMT_FLAG, 'N'),
                   COALESCE(CH.REINSURANCE_FLAG, 'N'),
                   COALESCE(CH.PROJECTED_TOTAL_COST, 0)
               FROM HCDB.CLAIM_HEADER CH
               LEFT JOIN HCDB.PROVIDER_MASTER PM
                   ON CH.RENDERING_PROVIDER = PM.PROVIDER_NPI
               WHERE CH.BILLED_AMOUNT >= :WS-HIGH-DOLLAR-THRESH
                 AND CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
               ORDER BY CH.BILLED_AMOUNT DESC
               FETCH FIRST 200 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-HIGHDOL END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3800-EXIT
           END-IF

           MOVE 0 TO WS-HD-DETAIL-COUNT
           MOVE 0 TO WS-HD-STOPLOSS-COUNT
           MOVE 0 TO WS-HD-STOPLOSS-AMT
           SET NOT-EOF-CURSOR TO TRUE

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-HIGHDOL
                   INTO :HV-CLAIM-ID,
                        :HV-MEMBER-ID,
                        :HV-PROVIDER-NAME,
                        :HV-DIAG-CODE,
                        :HV-PROC-CODE,
                        :HV-BILLED-AMT,
                        :HV-PAID-AMT :WS-IND-PAID-AMT,
                        :HV-CLAIM-STATUS,
                        :HV-CASE-MGMT-FLAG,
                        :HV-REINS-FLAG,
                        :HV-PROJECTED-COST :WS-IND-PROJECTED-COST
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1 TO WS-HD-DETAIL-COUNT
                       IF WS-HD-DETAIL-COUNT <= 200
                           PERFORM 3810-STORE-HIGH-DOLLAR
                       END-IF
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-HIGHDOL END-EXEC

      *    WRITE THE HIGH-DOLLAR REPORT
           PERFORM 3820-WRITE-HIGH-DOLLAR-REPORT

       3800-EXIT.
           EXIT.

      *================================================================*
      *    3810-STORE-HIGH-DOLLAR                                      *
      *================================================================*
       3810-STORE-HIGH-DOLLAR.

           MOVE HV-CLAIM-ID
               TO WS-HDD-CLAIM-ID(WS-HD-DETAIL-COUNT)
           MOVE HV-MEMBER-ID
               TO WS-HDD-MEMBER-ID(WS-HD-DETAIL-COUNT)
           MOVE HV-PROVIDER-NAME(1:35)
               TO WS-HDD-PROVIDER(WS-HD-DETAIL-COUNT)
           MOVE HV-DIAG-CODE
               TO WS-HDD-DIAG-CODE(WS-HD-DETAIL-COUNT)
           MOVE HV-PROC-CODE
               TO WS-HDD-PROC-CODE(WS-HD-DETAIL-COUNT)
           MOVE HV-BILLED-AMT
               TO WS-HDD-BILLED-AMT(WS-HD-DETAIL-COUNT)

           IF WS-IND-PAID-AMT >= 0
               MOVE HV-PAID-AMT
                   TO WS-HDD-PAID-AMT(WS-HD-DETAIL-COUNT)
           ELSE
               MOVE 0 TO WS-HDD-PAID-AMT(WS-HD-DETAIL-COUNT)
           END-IF

           MOVE HV-CLAIM-STATUS
               TO WS-HDD-STATUS(WS-HD-DETAIL-COUNT)
           MOVE HV-CASE-MGMT-FLAG
               TO WS-HDD-CASE-MGMT(WS-HD-DETAIL-COUNT)
           MOVE HV-REINS-FLAG
               TO WS-HDD-REINS-FLAG(WS-HD-DETAIL-COUNT)

           IF WS-IND-PROJECTED-COST >= 0
               MOVE HV-PROJECTED-COST
                   TO WS-HDD-PROJ-COST(WS-HD-DETAIL-COUNT)
           ELSE
               MOVE 0 TO WS-HDD-PROJ-COST(WS-HD-DETAIL-COUNT)
           END-IF

      *    ACCUMULATE THRESHOLD TIER COUNTS
           PERFORM VARYING WS-WORK-INDEX FROM 6 BY -1
               UNTIL WS-WORK-INDEX < 1
               IF HV-BILLED-AMT >=
                   WS-HD-THR-AMOUNT(WS-WORK-INDEX)
                   ADD 1 TO WS-HD-THR-COUNT(WS-WORK-INDEX)
                   ADD HV-BILLED-AMT
                       TO WS-HD-THR-TOTAL(WS-WORK-INDEX)
                   ADD WS-HDD-PAID-AMT(WS-HD-DETAIL-COUNT)
                       TO WS-HD-THR-PAID(WS-WORK-INDEX)
               END-IF
           END-PERFORM

      *    CHECK REINSURANCE/STOP-LOSS
           IF HV-BILLED-AMT >= WS-HD-REINS-THRESHOLD
               ADD 1 TO WS-HD-STOPLOSS-COUNT
               ADD HV-BILLED-AMT TO WS-HD-STOPLOSS-AMT
           END-IF.

      *================================================================*
      *    3820-WRITE-HIGH-DOLLAR-REPORT                               *
      *================================================================*
       3820-WRITE-HIGH-DOLLAR-REPORT.

           ADD 1 TO WS-RPT08-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R08H1-DATE
           MOVE WS-RPT08-PAGE-CTR   TO WS-R08H1-PAGE
           WRITE RPT08-RECORD FROM WS-RPT08-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT08-RECORD FROM WS-RPT08-HEADER2
           WRITE RPT08-RECORD FROM WS-BLANK-LINE

      *    WRITE THRESHOLD SUMMARY
           MOVE '  THRESHOLD TIER SUMMARY:' TO WS-WORK-STRING
           WRITE RPT08-RECORD FROM WS-WORK-STRING
           WRITE RPT08-RECORD FROM WS-SEPARATOR-LINE
           MOVE 6 TO WS-RPT08-LINE-CTR

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 6
               MOVE SPACES TO WS-WORK-STRING
               STRING '  ' WS-HD-THR-LABEL(WS-WORK-INDEX)
                   DELIMITED BY SIZE INTO WS-WORK-STRING
               WRITE RPT08-RECORD FROM WS-WORK-STRING
               ADD 1 TO WS-RPT08-LINE-CTR
               ADD 1 TO WS-RPT08-RECORDS
           END-PERFORM

      *    REINSURANCE SUMMARY
           WRITE RPT08-RECORD FROM WS-BLANK-LINE
           MOVE SPACES TO WS-WORK-STRING
           STRING '  REINSURANCE/STOP-LOSS CANDIDATES: '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT08-RECORD FROM WS-WORK-STRING

      *    WRITE CLAIM DETAILS
           WRITE RPT08-RECORD FROM WS-BLANK-LINE
           WRITE RPT08-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  HIGH-DOLLAR CLAIM DETAILS:' TO WS-WORK-STRING
           WRITE RPT08-RECORD FROM WS-WORK-STRING
           WRITE RPT08-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-HD-DETAIL-COUNT
                  OR WS-WORK-INDEX > 200

               IF WS-RPT08-LINE-CTR >= WS-MAX-LINES
                   ADD 1 TO WS-RPT08-PAGE-CTR
                   MOVE WS-REPORT-DATE-DISP TO WS-R08H1-DATE
                   MOVE WS-RPT08-PAGE-CTR TO WS-R08H1-PAGE
                   WRITE RPT08-RECORD FROM WS-RPT08-HEADER1
                       AFTER ADVANCING PAGE-EJECT
                   WRITE RPT08-RECORD FROM WS-RPT08-HEADER2
                   WRITE RPT08-RECORD FROM WS-SEPARATOR-LINE
                   MOVE 4 TO WS-RPT08-LINE-CTR
               END-IF

               MOVE WS-HDD-CLAIM-ID(WS-WORK-INDEX)
                   TO WS-R08D-CLAIM-ID
               MOVE WS-HDD-MEMBER-ID(WS-WORK-INDEX)
                   TO WS-R08D-MEMBER
               MOVE WS-HDD-PROVIDER(WS-WORK-INDEX)(1:20)
                   TO WS-R08D-PROVIDER
               MOVE WS-HDD-DIAG-CODE(WS-WORK-INDEX)
                   TO WS-R08D-DIAG
               MOVE WS-HDD-BILLED-AMT(WS-WORK-INDEX)
                   TO WS-R08D-BILLED
               MOVE WS-HDD-PAID-AMT(WS-WORK-INDEX)
                   TO WS-R08D-PAID
               MOVE WS-HDD-STATUS(WS-WORK-INDEX)
                   TO WS-R08D-STATUS
               MOVE WS-HDD-CASE-MGMT(WS-WORK-INDEX)
                   TO WS-R08D-CASE
               MOVE WS-HDD-REINS-FLAG(WS-WORK-INDEX)
                   TO WS-R08D-REINS

               WRITE RPT08-RECORD FROM WS-RPT08-DETAIL
               ADD 1 TO WS-RPT08-LINE-CTR
               ADD 1 TO WS-RPT08-RECORDS
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT08' TO WS-AUD-REPORT-ID
           MOVE WS-RPT08-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'HIGH-DOLLAR CLAIMS REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    3900-BUILD-DUPLICATE-REPORT                                 *
      *    REPORT 09: DUPLICATE CLAIMS DETECTION AND ANALYSIS          *
      *    IDENTIFIED PAIRS, MATCH CRITERIA, FINANCIAL EXPOSURE,       *
      *    FALSE POSITIVE RATE, DUPLICATE RATE BY PAYER/PROVIDER       *
      *================================================================*
       3900-BUILD-DUPLICATE-REPORT.

           DISPLAY 'HCRPTGEN - BUILDING RPT09 DUPLICATE DETECTION'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT09' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'DUPLICATE CLAIMS REPORT STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY IDENTIFIED DUPLICATE PAIRS
           EXEC SQL
               DECLARE CSR-DUPS CURSOR FOR
               SELECT
                   D.CLAIM_ID_1,
                   D.CLAIM_ID_2,
                   D.MATCH_TYPE,
                   CH.BILLED_AMOUNT
               FROM HCDB.DUPLICATE_CLAIMS D
               JOIN HCDB.CLAIM_HEADER CH
                   ON D.CLAIM_ID_1 = CH.CLAIM_ID
               WHERE D.DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                           AND :HV-RPT-END-DATE
               ORDER BY CH.BILLED_AMOUNT DESC
               FETCH FIRST 100 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-DUPS END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-LOG-SQL-ERROR
               GO TO 3900-EXIT
           END-IF

           MOVE 0 TO WS-DUP-DETAIL-COUNT
           SET NOT-EOF-CURSOR TO TRUE

           PERFORM UNTIL EOF-CURSOR
               EXEC SQL
                   FETCH CSR-DUPS
                   INTO :HV-MATCH-CLAIM1,
                        :HV-MATCH-CLAIM2,
                        :HV-MATCH-TYPE,
                        :HV-BILLED-AMT
               END-EXEC

               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1 TO WS-DUP-DETAIL-COUNT
                       ADD 1 TO WS-DUP-TOTAL-PAIRS
                       ADD HV-BILLED-AMT TO WS-DUP-TOTAL-AMT

                       IF WS-DUP-DETAIL-COUNT <= 100
                           MOVE HV-MATCH-CLAIM1
                               TO WS-DUPD-CLM-ID-1(
                                   WS-DUP-DETAIL-COUNT)
                           MOVE HV-MATCH-CLAIM2
                               TO WS-DUPD-CLM-ID-2(
                                   WS-DUP-DETAIL-COUNT)
                           MOVE HV-MATCH-TYPE
                               TO WS-DUPD-MATCH-TYPE(
                                   WS-DUP-DETAIL-COUNT)
                           MOVE HV-BILLED-AMT
                               TO WS-DUPD-AMOUNT(
                                   WS-DUP-DETAIL-COUNT)
                       END-IF

      *                ACCUMULATE BY MATCH CRITERIA
                       EVALUATE HV-MATCH-TYPE
                           WHEN 'EXA'
                               ADD 1 TO WS-DUBC-MATCH-CNT(1)
                           WHEN 'MDP'
                               ADD 1 TO WS-DUBC-MATCH-CNT(2)
                           WHEN 'MDA'
                               ADD 1 TO WS-DUBC-MATCH-CNT(3)
                           WHEN 'PMD'
                               ADD 1 TO WS-DUBC-MATCH-CNT(4)
                           WHEN OTHER
                               ADD 1 TO WS-DUBC-MATCH-CNT(5)
                       END-EVALUATE
                   WHEN 100
                       SET EOF-CURSOR TO TRUE
                   WHEN OTHER
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
               END-EVALUATE
           END-PERFORM

           EXEC SQL CLOSE CSR-DUPS END-EXEC

      *    QUERY FALSE POSITIVE COUNT
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-DUP-FALSE-POS
               FROM HCDB.DUPLICATE_CLAIMS D
               WHERE D.DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                           AND :HV-RPT-END-DATE
                 AND D.RESOLUTION = 'FP'
           END-EXEC

      *    WRITE THE DUPLICATE REPORT
           PERFORM 3910-WRITE-DUPLICATE-REPORT

       3900-EXIT.
           EXIT.

      *================================================================*
      *    3910-WRITE-DUPLICATE-REPORT                                 *
      *================================================================*
       3910-WRITE-DUPLICATE-REPORT.

           ADD 1 TO WS-RPT09-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R09H1-DATE
           MOVE WS-RPT09-PAGE-CTR   TO WS-R09H1-PAGE
           WRITE RPT09-RECORD FROM WS-RPT09-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT09-RECORD FROM WS-RPT09-HEADER2
           WRITE RPT09-RECORD FROM WS-BLANK-LINE
           WRITE RPT09-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT09-LINE-CTR

      *    SUMMARY SECTION
           MOVE SPACES TO WS-WORK-STRING
           STRING '  TOTAL DUPLICATE PAIRS DETECTED: '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT09-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT09-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  FALSE POSITIVE COUNT: '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT09-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT09-RECORDS

      *    WRITE MATCH CRITERIA BREAKDOWN
           WRITE RPT09-RECORD FROM WS-BLANK-LINE
           MOVE '  DUPLICATES BY MATCH CRITERIA:' TO WS-WORK-STRING
           WRITE RPT09-RECORD FROM WS-WORK-STRING
           WRITE RPT09-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 5
               MOVE SPACES TO WS-WORK-STRING
               STRING '    ' WS-DUBC-CRITERIA(WS-WORK-INDEX)
                   DELIMITED BY SIZE INTO WS-WORK-STRING
               WRITE RPT09-RECORD FROM WS-WORK-STRING
               ADD 1 TO WS-RPT09-RECORDS
           END-PERFORM

      *    WRITE DETAIL SECTION
           WRITE RPT09-RECORD FROM WS-BLANK-LINE
           WRITE RPT09-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  DUPLICATE PAIR DETAILS:' TO WS-WORK-STRING
           WRITE RPT09-RECORD FROM WS-WORK-STRING
           WRITE RPT09-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-DUP-DETAIL-COUNT
                  OR WS-WORK-INDEX > 100

               IF WS-RPT09-LINE-CTR >= WS-MAX-LINES
                   ADD 1 TO WS-RPT09-PAGE-CTR
                   MOVE WS-RPT09-PAGE-CTR TO WS-R09H1-PAGE
                   WRITE RPT09-RECORD FROM WS-RPT09-HEADER1
                       AFTER ADVANCING PAGE-EJECT
                   WRITE RPT09-RECORD FROM WS-RPT09-HEADER2
                   WRITE RPT09-RECORD FROM WS-SEPARATOR-LINE
                   MOVE 4 TO WS-RPT09-LINE-CTR
               END-IF

               MOVE WS-DUPD-CLM-ID-1(WS-WORK-INDEX)
                   TO WS-R09D-CLM1
               MOVE WS-DUPD-CLM-ID-2(WS-WORK-INDEX)
                   TO WS-R09D-CLM2
               EVALUATE WS-DUPD-MATCH-TYPE(WS-WORK-INDEX)
                   WHEN 'EXA' MOVE 'EXACT MATCH         '
                       TO WS-R09D-MATCH
                   WHEN 'MDP' MOVE 'MBR+DOS+PROC        '
                       TO WS-R09D-MATCH
                   WHEN 'MDA' MOVE 'MBR+DOS+AMT         '
                       TO WS-R09D-MATCH
                   WHEN 'PMD' MOVE 'PROV+MBR+DOS        '
                       TO WS-R09D-MATCH
                   WHEN OTHER MOVE 'NEAR-DUPLICATE       '
                       TO WS-R09D-MATCH
               END-EVALUATE
               MOVE WS-DUPD-AMOUNT(WS-WORK-INDEX)
                   TO WS-R09D-AMOUNT

               WRITE RPT09-RECORD FROM WS-RPT09-DETAIL
               ADD 1 TO WS-RPT09-LINE-CTR
               ADD 1 TO WS-RPT09-RECORDS
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT09' TO WS-AUD-REPORT-ID
           MOVE WS-RPT09-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'DUPLICATE CLAIMS REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    4000-BUILD-QUALITY-METRICS                                  *
      *    REPORT 10: QUALITY METRICS AND HEDIS/STAR DASHBOARD         *
      *    AUTO-ADJUDICATION RATE, FIRST-PASS, ACCURACY,               *
      *    COMPLAINT RATES, SLA ADHERENCE, HEDIS MEASURES              *
      *================================================================*
       4000-BUILD-QUALITY-METRICS.

           DISPLAY 'HCRPTGEN - BUILDING RPT10 QUALITY METRICS'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT10' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'QUALITY METRICS DASHBOARD STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY AUTO-ADJUDICATION RATE
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE WHEN CH.AUTO_ADJUDICATED = 'Y'
                       THEN 1 ELSE 0 END)
               INTO :WS-QL-TOTAL-ADJ-CNT,
                    :WS-QL-AUTO-ADJ-CNT
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.PROCESS_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
                 AND CH.CLAIM_STATUS IN ('PD', 'DN')
           END-EXEC

           IF WS-QL-TOTAL-ADJ-CNT > 0
               COMPUTE WS-QL-AUTO-ADJ-RATE =
                   (WS-QL-AUTO-ADJ-CNT /
                    WS-QL-TOTAL-ADJ-CNT) * 100
           END-IF

      *    QUERY FIRST-PASS RESOLUTION RATE
           EXEC SQL
               SELECT
                   CAST(SUM(CASE WHEN CH.TOUCH_COUNT = 1
                       THEN 1 ELSE 0 END) AS DECIMAL(11,2)) /
                   CAST(COUNT(*) AS DECIMAL(11,2)) * 100
               INTO :WS-QL-FIRST-PASS-RATE
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.PROCESS_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
                 AND CH.CLAIM_STATUS IN ('PD', 'DN')
           END-EXEC

      *    QUERY ACCURACY RATE FROM QA SAMPLING
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE WHEN QA.ACCURACY_FLAG = 'N'
                       THEN 1 ELSE 0 END)
               INTO :WS-QL-ACCURACY-SAMP,
                    :WS-QL-ACCURACY-ERR
               FROM HCDB.QA_AUDIT_SAMPLE QA
               WHERE QA.AUDIT_DATE BETWEEN :HV-RPT-START-DATE
                                        AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-QL-ACCURACY-SAMP > 0
               COMPUTE WS-QL-ACCURACY-RATE =
                   ((WS-QL-ACCURACY-SAMP - WS-QL-ACCURACY-ERR) /
                    WS-QL-ACCURACY-SAMP) * 100
           END-IF

      *    QUERY COMPLAINT AND APPEAL COUNTS
           EXEC SQL
               SELECT
                   SUM(CASE WHEN COMPLAINT_TYPE = 'P'
                       THEN 1 ELSE 0 END),
                   SUM(CASE WHEN COMPLAINT_TYPE = 'M'
                       THEN 1 ELSE 0 END)
               INTO :WS-QL-PROV-COMPLAINT,
                    :WS-QL-MEMB-COMPLAINT
               FROM HCDB.COMPLAINTS
               WHERE COMPLAINT_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE WHEN APPEAL_RESULT = 'OV'
                       THEN 1 ELSE 0 END)
               INTO :WS-QL-APPEAL-CNT,
                    :WS-QL-APPEAL-OVERTURN
               FROM HCDB.APPEALS
               WHERE APPEAL_DATE BETWEEN :HV-RPT-START-DATE
                                      AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-QL-APPEAL-CNT > 0
               COMPUTE WS-QL-APPEAL-RATE =
                   (WS-QL-APPEAL-OVERTURN /
                    WS-QL-APPEAL-CNT) * 100
           END-IF

      *    QUERY SLA ADHERENCE
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE
                       WHEN DAYS(CH.PROCESS_DATE)
                            - DAYS(CH.RECEIPT_DATE)
                            <= :WS-CLEAN-CLAIM-DAYS
                       THEN 1 ELSE 0 END)
               INTO :WS-QL-SLA-TOTAL-CNT,
                    :WS-QL-SLA-MET-CNT
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.PROCESS_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
                 AND CH.CLAIM_STATUS IN ('PD', 'DN')
           END-EXEC

           IF WS-QL-SLA-TOTAL-CNT > 0
               COMPUTE WS-QL-SLA-PCT =
                   (WS-QL-SLA-MET-CNT /
                    WS-QL-SLA-TOTAL-CNT) * 100
           END-IF

      *    QUERY HEDIS MEASURES
           EXEC SQL
               DECLARE CSR-HEDIS CURSOR FOR
               SELECT
                   QM.MEASURE_ID,
                   QM.MEASURE_DESCRIPTION,
                   QM.NUMERATOR_COUNT,
                   QM.DENOMINATOR_COUNT,
                   QM.BENCHMARK_RATE,
                   QM.STAR_RATING
               FROM HCDB.QUALITY_MEASURES QM
               WHERE QM.MEASUREMENT_YEAR =
                   YEAR(:HV-RPT-END-DATE)
               ORDER BY QM.MEASURE_ID
               FETCH FIRST 15 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-HEDIS END-EXEC

           IF SQLCODE = 0
               MOVE 0 TO WS-WORK-INDEX
               SET NOT-EOF-CURSOR TO TRUE

               PERFORM UNTIL EOF-CURSOR
                   EXEC SQL
                       FETCH CSR-HEDIS
                       INTO :HV-MEASURE-ID,
                            :HV-MEASURE-DESC,
                            :HV-NUMERATOR,
                            :HV-DENOMINATOR,
                            :HV-BENCHMARK,
                            :HV-STAR-RATING
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-WORK-INDEX
                       IF WS-WORK-INDEX <= 15
                           MOVE HV-MEASURE-ID
                               TO WS-QH-MEASURE-ID(WS-WORK-INDEX)
                           MOVE HV-MEASURE-DESC
                               TO WS-QH-MEASURE-DESC(WS-WORK-INDEX)
                           MOVE HV-NUMERATOR
                               TO WS-QH-NUMERATOR(WS-WORK-INDEX)
                           MOVE HV-DENOMINATOR
                               TO WS-QH-DENOMINATOR(WS-WORK-INDEX)
                           MOVE HV-BENCHMARK
                               TO WS-QH-BENCHMARK(WS-WORK-INDEX)
                           MOVE HV-STAR-RATING
                               TO WS-QH-STAR-RATING(WS-WORK-INDEX)

                           IF WS-QH-DENOMINATOR(WS-WORK-INDEX) > 0
                               COMPUTE WS-QH-RATE(WS-WORK-INDEX) =
                                   (WS-QH-NUMERATOR(WS-WORK-INDEX)
                                    / WS-QH-DENOMINATOR(
                                        WS-WORK-INDEX)) * 100
                           END-IF

                           COMPUTE WS-QH-VARIANCE(WS-WORK-INDEX) =
                               WS-QH-RATE(WS-WORK-INDEX) -
                               WS-QH-BENCHMARK(WS-WORK-INDEX)
                       END-IF
                   ELSE IF SQLCODE = 100
                       SET EOF-CURSOR TO TRUE
                   ELSE
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
                   END-IF
               END-PERFORM

               EXEC SQL CLOSE CSR-HEDIS END-EXEC
           END-IF

      *    WRITE THE QUALITY REPORT
           PERFORM 4010-WRITE-QUALITY-REPORT.

      *================================================================*
      *    4010-WRITE-QUALITY-REPORT                                   *
      *================================================================*
       4010-WRITE-QUALITY-REPORT.

           ADD 1 TO WS-RPT10-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R10H1-DATE
           MOVE WS-RPT10-PAGE-CTR   TO WS-R10H1-PAGE
           WRITE RPT10-RECORD FROM WS-RPT10-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT10-RECORD FROM WS-RPT10-HEADER2
           WRITE RPT10-RECORD FROM WS-BLANK-LINE
           WRITE RPT10-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT10-LINE-CTR

      *    OPERATIONAL QUALITY METRICS
           MOVE '  OPERATIONAL QUALITY METRICS:' TO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-SEPARATOR-LINE

           MOVE SPACES TO WS-WORK-STRING
           STRING '  AUTO-ADJUDICATION RATE:    '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT10-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  FIRST-PASS RESOLUTION:     '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT10-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  CLAIMS ACCURACY RATE:      '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT10-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  SLA ADHERENCE:             '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT10-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  APPEAL OVERTURN RATE:      '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT10-RECORDS

      *    HEDIS MEASURES SECTION
           WRITE RPT10-RECORD FROM WS-BLANK-LINE
           WRITE RPT10-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  HEDIS / STAR RATING MEASURES:'
               TO WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-WORK-STRING
           WRITE RPT10-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 15
               IF WS-QH-MEASURE-ID(WS-WORK-INDEX) NOT = SPACES
                   MOVE WS-QH-MEASURE-ID(WS-WORK-INDEX)
                       TO WS-R10D-MEASURE
                   MOVE WS-QH-MEASURE-DESC(WS-WORK-INDEX)(1:35)
                       TO WS-R10D-DESC
                   MOVE WS-QH-NUMERATOR(WS-WORK-INDEX)
                       TO WS-R10D-NUMER
                   MOVE WS-QH-DENOMINATOR(WS-WORK-INDEX)
                       TO WS-R10D-DENOM
                   MOVE WS-QH-RATE(WS-WORK-INDEX)
                       TO WS-R10D-RATE
                   MOVE WS-QH-BENCHMARK(WS-WORK-INDEX)
                       TO WS-R10D-BENCH
                   MOVE WS-QH-STAR-RATING(WS-WORK-INDEX)
                       TO WS-R10D-STAR
                   MOVE WS-QH-VARIANCE(WS-WORK-INDEX)
                       TO WS-R10D-VARIANCE

                   WRITE RPT10-RECORD FROM WS-RPT10-HEDIS-DETAIL
                   ADD 1 TO WS-RPT10-LINE-CTR
                   ADD 1 TO WS-RPT10-RECORDS
               END-IF
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT10' TO WS-AUD-REPORT-ID
           MOVE WS-RPT10-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'QUALITY METRICS DASHBOARD COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    4100-BUILD-FWA-REPORT                                       *
      *    REPORT 11: FRAUD, WASTE AND ABUSE INDICATOR REPORT          *
      *    UPCODING, UNBUNDLING, IMPOSSIBLE DAY, POST-DEATH,           *
      *    VOLUME ANOMALIES, GEOGRAPHIC OUTLIERS, REFERRAL PATTERNS    *
      *================================================================*
       4100-BUILD-FWA-REPORT.

           DISPLAY 'HCRPTGEN - BUILDING RPT11 FWA INDICATORS'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT11' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'FRAUD/WASTE/ABUSE REPORT STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY UPCODING INDICATORS (E/M LEVEL DISTRIBUTION)
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-UPCODING-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'UPC'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY UNBUNDLING INDICATORS (MODIFIER 59 FREQUENCY)
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-UNBUNDLE-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'UNB'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY DUPLICATE SERVICE INDICATORS
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-DUPLICATE-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'DUP'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY IMPOSSIBLE DAY BILLING (>24 HOURS)
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-IMPOSSIBLE-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'IMP'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY SERVICES AFTER DEATH
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-POST-DEATH-CNT
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.MEMBER_ELIGIBILITY ME
                   ON CH.MEMBER_ID = ME.MEMBER_ID
               WHERE ME.DATE_OF_DEATH IS NOT NULL
                 AND CH.SERVICE_DATE > ME.DATE_OF_DEATH
                 AND CH.SERVICE_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY VOLUME ANOMALIES (>99TH PERCENTILE)
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-VOLUME-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'VOL'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY GEOGRAPHIC OUTLIERS
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-GEO-OUTLIER-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'GEO'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    QUERY REFERRAL PATTERN ANOMALIES
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-FWA-REFERRAL-CNT
               FROM HCDB.FWA_INDICATORS
               WHERE ALERT_TYPE = 'REF'
                 AND DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                         AND :HV-RPT-END-DATE
           END-EXEC

      *    CALCULATE TOTAL ALERTS
           COMPUTE WS-FWA-TOTAL-ALERTS =
               WS-FWA-UPCODING-CNT + WS-FWA-UNBUNDLE-CNT +
               WS-FWA-DUPLICATE-CNT + WS-FWA-IMPOSSIBLE-CNT +
               WS-FWA-POST-DEATH-CNT + WS-FWA-VOLUME-CNT +
               WS-FWA-GEO-OUTLIER-CNT + WS-FWA-REFERRAL-CNT

      *    QUERY PROVIDER-LEVEL FWA DETAILS
           EXEC SQL
               DECLARE CSR-FWAPROV CURSOR FOR
               SELECT
                   PM.PROVIDER_NPI,
                   PM.PROVIDER_NAME,
                   COUNT(*) AS ALERT_COUNT,
                   SUM(FI.ALERT_AMOUNT) AS ALERT_AMOUNT,
                   FI.ALERT_TYPE,
                   FI.RISK_SCORE
               FROM HCDB.FWA_INDICATORS FI
               JOIN HCDB.PROVIDER_MASTER PM
                   ON FI.PROVIDER_NPI = PM.PROVIDER_NPI
               WHERE FI.DETECTION_DATE BETWEEN :HV-RPT-START-DATE
                                            AND :HV-RPT-END-DATE
               GROUP BY PM.PROVIDER_NPI, PM.PROVIDER_NAME,
                        FI.ALERT_TYPE, FI.RISK_SCORE
               ORDER BY ALERT_COUNT DESC
               FETCH FIRST 50 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-FWAPROV END-EXEC

           IF SQLCODE = 0
               MOVE 0 TO WS-FWA-PROV-COUNT
               SET NOT-EOF-CURSOR TO TRUE

               PERFORM UNTIL EOF-CURSOR
                   EXEC SQL
                       FETCH CSR-FWAPROV
                       INTO :HV-PROVIDER-NPI,
                            :HV-PROVIDER-NAME,
                            :HV-COUNT,
                            :HV-SUM-AMT,
                            :HV-ALERT-TYPE,
                            :HV-RISK-SCORE
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-FWA-PROV-COUNT
                       IF WS-FWA-PROV-COUNT <= 50
                           MOVE HV-PROVIDER-NPI
                               TO WS-FWAP-PROV-NPI(
                                   WS-FWA-PROV-COUNT)
                           MOVE HV-PROVIDER-NAME
                               TO WS-FWAP-PROV-NAME(
                                   WS-FWA-PROV-COUNT)
                           MOVE HV-COUNT
                               TO WS-FWAP-ALERT-CNT(
                                   WS-FWA-PROV-COUNT)
                           MOVE HV-SUM-AMT
                               TO WS-FWAP-ALERT-AMT(
                                   WS-FWA-PROV-COUNT)
                           MOVE HV-ALERT-TYPE
                               TO WS-FWAP-ALERT-TYPE(
                                   WS-FWA-PROV-COUNT)
                           MOVE HV-RISK-SCORE
                               TO WS-FWAP-RISK-SCORE(
                                   WS-FWA-PROV-COUNT)
                           ADD HV-SUM-AMT TO WS-FWA-TOTAL-AMT
                       END-IF
                   ELSE IF SQLCODE = 100
                       SET EOF-CURSOR TO TRUE
                   ELSE
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
                   END-IF
               END-PERFORM

               EXEC SQL CLOSE CSR-FWAPROV END-EXEC
           END-IF

      *    WRITE THE FWA REPORT
           PERFORM 4110-WRITE-FWA-REPORT.

      *================================================================*
      *    4110-WRITE-FWA-REPORT                                       *
      *================================================================*
       4110-WRITE-FWA-REPORT.

           ADD 1 TO WS-RPT11-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R11H1-DATE
           MOVE WS-RPT11-PAGE-CTR   TO WS-R11H1-PAGE
           WRITE RPT11-RECORD FROM WS-RPT11-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT11-RECORD FROM WS-RPT11-HEADER2
           WRITE RPT11-RECORD FROM WS-BLANK-LINE
           WRITE RPT11-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT11-LINE-CTR

      *    ALERT SUMMARY BY TYPE
           MOVE '  FWA ALERT SUMMARY BY TYPE:' TO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-SEPARATOR-LINE

           MOVE SPACES TO WS-WORK-STRING
           STRING '  UPCODING INDICATORS:       '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  UNBUNDLING INDICATORS:     '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  DUPLICATE SERVICES:        '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  IMPOSSIBLE DAY BILLING:    '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  SERVICES AFTER DEATH:      '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  VOLUME ANOMALIES (>99TH):  '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  GEOGRAPHIC OUTLIERS:       '
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

           MOVE SPACES TO WS-WORK-STRING
           STRING '  REFERRAL PATTERN ANOMALIES:'
               DELIMITED BY SIZE INTO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           ADD 1 TO WS-RPT11-RECORDS

      *    PROVIDER DETAIL SECTION
           WRITE RPT11-RECORD FROM WS-BLANK-LINE
           WRITE RPT11-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  PROVIDER-LEVEL FWA ALERTS:'
               TO WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-WORK-STRING
           WRITE RPT11-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > WS-FWA-PROV-COUNT
                  OR WS-WORK-INDEX > 50

               IF WS-RPT11-LINE-CTR >= WS-MAX-LINES
                   ADD 1 TO WS-RPT11-PAGE-CTR
                   MOVE WS-RPT11-PAGE-CTR TO WS-R11H1-PAGE
                   WRITE RPT11-RECORD FROM WS-RPT11-HEADER1
                       AFTER ADVANCING PAGE-EJECT
                   WRITE RPT11-RECORD FROM WS-RPT11-HEADER2
                   WRITE RPT11-RECORD FROM WS-SEPARATOR-LINE
                   MOVE 4 TO WS-RPT11-LINE-CTR
               END-IF

               MOVE WS-FWAP-PROV-NPI(WS-WORK-INDEX)
                   TO WS-R11D-NPI
               MOVE WS-FWAP-PROV-NAME(WS-WORK-INDEX)(1:25)
                   TO WS-R11D-NAME
               EVALUATE WS-FWAP-ALERT-TYPE(WS-WORK-INDEX)
                   WHEN 'UPC' MOVE 'UPCODING       '
                       TO WS-R11D-ALERT-TYPE
                   WHEN 'UNB' MOVE 'UNBUNDLING     '
                       TO WS-R11D-ALERT-TYPE
                   WHEN 'DUP' MOVE 'DUPLICATE SVC  '
                       TO WS-R11D-ALERT-TYPE
                   WHEN 'IMP' MOVE 'IMPOSSIBLE DAY '
                       TO WS-R11D-ALERT-TYPE
                   WHEN 'VOL' MOVE 'VOLUME ANOMALY '
                       TO WS-R11D-ALERT-TYPE
                   WHEN 'GEO' MOVE 'GEO OUTLIER    '
                       TO WS-R11D-ALERT-TYPE
                   WHEN 'REF' MOVE 'REFERRAL PTRN  '
                       TO WS-R11D-ALERT-TYPE
                   WHEN OTHER MOVE 'OTHER          '
                       TO WS-R11D-ALERT-TYPE
               END-EVALUATE
               MOVE WS-FWAP-ALERT-CNT(WS-WORK-INDEX)
                   TO WS-R11D-ALERT-CNT
               MOVE WS-FWAP-ALERT-AMT(WS-WORK-INDEX)
                   TO WS-R11D-AMOUNT
               MOVE WS-FWAP-RISK-SCORE(WS-WORK-INDEX)
                   TO WS-R11D-RISK

               WRITE RPT11-RECORD FROM WS-RPT11-DETAIL
               ADD 1 TO WS-RPT11-LINE-CTR
               ADD 1 TO WS-RPT11-RECORDS
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT11' TO WS-AUD-REPORT-ID
           MOVE WS-RPT11-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'FWA INDICATOR REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    4200-BUILD-COMPLIANCE-REPORT                                *
      *    REPORT 12: REGULATORY COMPLIANCE DASHBOARD                  *
      *    PROMPT PAY, CLEAN CLAIM RATE, TIMELY FILING,                *
      *    HIPAA, ACA/1094-C/1095-C, MLR, STATE-SPECIFIC               *
      *================================================================*
       4200-BUILD-COMPLIANCE-REPORT.

           DISPLAY 'HCRPTGEN - BUILDING RPT12 COMPLIANCE REPORT'

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-START ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT12' TO WS-AUD-REPORT-ID
           MOVE 0 TO WS-AUD-RECORD-COUNT
           MOVE 'REGULATORY COMPLIANCE REPORT STARTED'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    QUERY PROMPT PAY COMPLIANCE
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE
                       WHEN DAYS(CH.PAID_DATE)
                            - DAYS(CH.RECEIPT_DATE)
                            <= :WS-PROMPT-PAY-DAYS
                       THEN 1 ELSE 0 END),
                   SUM(CASE
                       WHEN DAYS(CH.PAID_DATE)
                            - DAYS(CH.RECEIPT_DATE)
                            > :WS-PROMPT-PAY-DAYS
                       THEN 1 ELSE 0 END),
                   AVG(DAYS(CH.PAID_DATE) - DAYS(CH.RECEIPT_DATE)),
                   COALESCE(SUM(CH.INTEREST_AMOUNT), 0)
               INTO :WS-CMP-PP-TOTAL,
                    :WS-CMP-PP-ONTIME,
                    :WS-CMP-PP-LATE,
                    :WS-CMP-PP-AVG-DAYS,
                    :WS-CMP-PP-INTEREST
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.CLAIM_STATUS = 'PD'
                 AND CH.PAID_DATE BETWEEN :HV-RPT-START-DATE
                                       AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-CMP-PP-TOTAL > 0
               COMPUTE WS-CMP-PP-PCT =
                   (WS-CMP-PP-ONTIME / WS-CMP-PP-TOTAL) * 100
           END-IF

      *    QUERY CLEAN CLAIM RATE
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE WHEN CH.CLEAN_CLAIM_FLAG = 'Y'
                       THEN 1 ELSE 0 END)
               INTO :WS-CMP-CC-TOTAL,
                    :WS-CMP-CC-CLEAN
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.RECEIPT_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-CMP-CC-TOTAL > 0
               COMPUTE WS-CMP-CC-RATE =
                   (WS-CMP-CC-CLEAN / WS-CMP-CC-TOTAL) * 100
           END-IF

      *    QUERY TIMELY FILING DENIALS
           EXEC SQL
               SELECT
                   COUNT(*),
                   SUM(CASE WHEN CH.DENIAL_REASON_CODE IN
                       ('TMF01', 'TMF02', 'TMF03')
                       THEN 1 ELSE 0 END)
               INTO :WS-CMP-TF-TOTAL,
                    :WS-CMP-TF-DENIED
               FROM HCDB.CLAIM_HEADER CH
               WHERE CH.RECEIPT_DATE BETWEEN :HV-RPT-START-DATE
                                          AND :HV-RPT-END-DATE
           END-EXEC

           IF WS-CMP-TF-TOTAL > 0
               COMPUTE WS-CMP-TF-RATE =
                   (WS-CMP-TF-DENIED / WS-CMP-TF-TOTAL) * 100
           END-IF

      *    QUERY HIPAA TRANSACTION VOLUMES
           EXEC SQL
               SELECT
                   SUM(CASE WHEN TRANS_TYPE = '837'
                       THEN 1 ELSE 0 END),
                   SUM(CASE WHEN TRANS_TYPE = '835'
                       THEN 1 ELSE 0 END),
                   SUM(CASE WHEN TRANS_TYPE = '277'
                       THEN 1 ELSE 0 END),
                   SUM(CASE WHEN ERROR_FLAG = 'Y'
                       THEN 1 ELSE 0 END)
               INTO :WS-CMP-HIP-837-CNT,
                    :WS-CMP-HIP-835-CNT,
                    :WS-CMP-HIP-277-CNT,
                    :WS-CMP-HIP-ERR-CNT
               FROM HCDB.HIPAA_TRANSACTIONS
               WHERE TRANS_DATE BETWEEN :HV-RPT-START-DATE
                                     AND :HV-RPT-END-DATE
           END-EXEC

           COMPUTE WS-WORK-COUNT =
               WS-CMP-HIP-837-CNT + WS-CMP-HIP-835-CNT +
               WS-CMP-HIP-277-CNT
           IF WS-WORK-COUNT > 0
               COMPUTE WS-CMP-HIP-COMPLY =
                   ((WS-WORK-COUNT - WS-CMP-HIP-ERR-CNT) /
                    WS-WORK-COUNT) * 100
           END-IF

      *    QUERY ACA 1095-C READINESS
           EXEC SQL
               SELECT COUNT(*),
                      SUM(CASE WHEN VALIDATION_STATUS = 'ER'
                          THEN 1 ELSE 0 END)
               INTO :WS-CMP-ACA-1095-CNT,
                    :WS-CMP-ACA-1095-ERR
               FROM HCDB.ACA_1095C_RECORDS
               WHERE TAX_YEAR = YEAR(:HV-RPT-END-DATE)
           END-EXEC

           IF WS-CMP-ACA-1095-ERR = 0
               AND WS-CMP-ACA-1095-CNT > 0
               MOVE 'Y' TO WS-CMP-ACA-1094-RDY
           ELSE
               MOVE 'N' TO WS-CMP-ACA-1094-RDY
           END-IF

      *    CALCULATE MLR FOR REGULATORY REPORTING
           EXEC SQL
               SELECT
                   COALESCE(SUM(CLAIMS_PAID), 0),
                   COALESCE(SUM(QI_EXPENSES), 0),
                   COALESCE(SUM(PREMIUM_COLLECTED), 0),
                   COALESCE(SUM(TAXES_FEES), 0)
               INTO :WS-CMP-MLR-CLAIMS,
                    :WS-CMP-MLR-QI-EXP,
                    :WS-CMP-MLR-PREMIUM,
                    :WS-CMP-MLR-TAXES
               FROM HCDB.MLR_REPORTING
               WHERE REPORT_YEAR = YEAR(:HV-RPT-END-DATE)
           END-EXEC

           IF (WS-CMP-MLR-PREMIUM - WS-CMP-MLR-TAXES) > 0
               COMPUTE WS-CMP-MLR-RATIO =
                   (WS-CMP-MLR-CLAIMS + WS-CMP-MLR-QI-EXP) /
                   (WS-CMP-MLR-PREMIUM - WS-CMP-MLR-TAXES)
           END-IF

      *    QUERY STATE-SPECIFIC COMPLIANCE
           EXEC SQL
               DECLARE CSR-STATE CURSOR FOR
               SELECT
                   RC.STATE_CODE,
                   RC.STATE_NAME,
                   RC.PROMPT_PAY_DAYS,
                   CAST(SUM(CASE
                       WHEN DAYS(CH.PAID_DATE)
                            - DAYS(CH.RECEIPT_DATE)
                            <= RC.PROMPT_PAY_DAYS
                       THEN 1 ELSE 0 END) AS DECIMAL(7,2)) /
                   CAST(COUNT(*) AS DECIMAL(7,2)) * 100
                       AS COMPLY_PCT,
                   COUNT(*) AS STATE_CLM_CNT
               FROM HCDB.CLAIM_HEADER CH
               JOIN HCDB.REGULATORY_CONFIG RC
                   ON CH.STATE_CODE = RC.STATE_CODE
               WHERE CH.CLAIM_STATUS = 'PD'
                 AND CH.PAID_DATE BETWEEN :HV-RPT-START-DATE
                                       AND :HV-RPT-END-DATE
               GROUP BY RC.STATE_CODE, RC.STATE_NAME,
                        RC.PROMPT_PAY_DAYS
               ORDER BY RC.STATE_CODE
               FETCH FIRST 10 ROWS ONLY
           END-EXEC

           EXEC SQL OPEN CSR-STATE END-EXEC

           IF SQLCODE = 0
               MOVE 0 TO WS-WORK-INDEX
               SET NOT-EOF-CURSOR TO TRUE

               PERFORM UNTIL EOF-CURSOR
                   EXEC SQL
                       FETCH CSR-STATE
                       INTO :HV-STATE-CODE,
                            :WS-WORK-STRING,
                            :WS-WORK-DAYS,
                            :WS-WORK-RATE,
                            :HV-COUNT
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-WORK-INDEX
                       IF WS-WORK-INDEX <= 10
                           MOVE HV-STATE-CODE
                               TO WS-CMPS-STATE-CD(WS-WORK-INDEX)
                           MOVE WS-WORK-STRING(1:20)
                               TO WS-CMPS-STATE-NAME(WS-WORK-INDEX)
                           MOVE WS-WORK-DAYS
                               TO WS-CMPS-PP-DAYS(WS-WORK-INDEX)
                           MOVE WS-WORK-RATE
                               TO WS-CMPS-PP-COMPLY(WS-WORK-INDEX)
                           MOVE HV-COUNT
                               TO WS-CMPS-CLM-CNT(WS-WORK-INDEX)
                       END-IF
                   ELSE IF SQLCODE = 100
                       SET EOF-CURSOR TO TRUE
                   ELSE
                       PERFORM 8100-LOG-SQL-ERROR
                       SET EOF-CURSOR TO TRUE
                   END-IF
               END-PERFORM

               EXEC SQL CLOSE CSR-STATE END-EXEC
           END-IF

      *    WRITE THE COMPLIANCE REPORT
           PERFORM 4210-WRITE-COMPLIANCE-REPORT.

      *================================================================*
      *    4210-WRITE-COMPLIANCE-REPORT                                *
      *================================================================*
       4210-WRITE-COMPLIANCE-REPORT.

           ADD 1 TO WS-RPT12-PAGE-CTR
           MOVE WS-REPORT-DATE-DISP TO WS-R12H1-DATE
           MOVE WS-RPT12-PAGE-CTR   TO WS-R12H1-PAGE
           WRITE RPT12-RECORD FROM WS-RPT12-HEADER1
               AFTER ADVANCING PAGE-EJECT
           WRITE RPT12-RECORD FROM WS-RPT12-HEADER2
           WRITE RPT12-RECORD FROM WS-BLANK-LINE
           WRITE RPT12-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE 5 TO WS-RPT12-LINE-CTR

      *    PROMPT PAY SECTION
           MOVE '  PROMPT PAY COMPLIANCE:' TO WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-SEPARATOR-LINE

           MOVE 'TOTAL CLAIMS PROCESSED             '
               TO WS-R12D-LABEL
           MOVE WS-CMP-PP-TOTAL TO WS-R12D-VALUE
           MOVE WS-CMP-PP-PCT TO WS-R12D-RATE
           IF WS-CMP-PP-PCT >= 95.00
               MOVE 'COMPLIANT ' TO WS-R12D-STATUS
           ELSE
               MOVE 'AT RISK   ' TO WS-R12D-STATUS
           END-IF
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

           MOVE 'CLAIMS PAID ON-TIME                '
               TO WS-R12D-LABEL
           MOVE WS-CMP-PP-ONTIME TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           MOVE SPACES TO WS-R12D-STATUS
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

           MOVE 'CLAIMS PAID LATE                   '
               TO WS-R12D-LABEL
           MOVE WS-CMP-PP-LATE TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           MOVE SPACES TO WS-R12D-STATUS
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

           MOVE 'AVERAGE DAYS TO PAY                '
               TO WS-R12D-LABEL
           MOVE WS-CMP-PP-AVG-DAYS TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           MOVE SPACES TO WS-R12D-STATUS
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

      *    CLEAN CLAIM SECTION
           WRITE RPT12-RECORD FROM WS-BLANK-LINE
           MOVE '  CLEAN CLAIM RATE:' TO WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-SEPARATOR-LINE

           MOVE 'TOTAL CLAIMS RECEIVED              '
               TO WS-R12D-LABEL
           MOVE WS-CMP-CC-TOTAL TO WS-R12D-VALUE
           MOVE WS-CMP-CC-RATE TO WS-R12D-RATE
           IF WS-CMP-CC-RATE >= 90.00
               MOVE 'COMPLIANT ' TO WS-R12D-STATUS
           ELSE
               MOVE 'AT RISK   ' TO WS-R12D-STATUS
           END-IF
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

      *    HIPAA COMPLIANCE SECTION
           WRITE RPT12-RECORD FROM WS-BLANK-LINE
           MOVE '  HIPAA TRANSACTION COMPLIANCE:' TO WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-SEPARATOR-LINE

           MOVE '837 CLAIM SUBMISSIONS              '
               TO WS-R12D-LABEL
           MOVE WS-CMP-HIP-837-CNT TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           MOVE SPACES TO WS-R12D-STATUS
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

           MOVE '835 REMITTANCE ADVICES             '
               TO WS-R12D-LABEL
           MOVE WS-CMP-HIP-835-CNT TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           MOVE SPACES TO WS-R12D-STATUS
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

           MOVE '277 CLAIM STATUS RESPONSES         '
               TO WS-R12D-LABEL
           MOVE WS-CMP-HIP-277-CNT TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           MOVE SPACES TO WS-R12D-STATUS
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

           MOVE 'OVERALL HIPAA COMPLIANCE           '
               TO WS-R12D-LABEL
           MOVE 0 TO WS-R12D-VALUE
           MOVE WS-CMP-HIP-COMPLY TO WS-R12D-RATE
           IF WS-CMP-HIP-COMPLY >= 99.00
               MOVE 'COMPLIANT ' TO WS-R12D-STATUS
           ELSE
               MOVE 'AT RISK   ' TO WS-R12D-STATUS
           END-IF
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

      *    ACA COMPLIANCE SECTION
           WRITE RPT12-RECORD FROM WS-BLANK-LINE
           MOVE '  ACA REPORTING COMPLIANCE:'
               TO WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-SEPARATOR-LINE

           MOVE '1095-C RECORDS GENERATED           '
               TO WS-R12D-LABEL
           MOVE WS-CMP-ACA-1095-CNT TO WS-R12D-VALUE
           MOVE 0 TO WS-R12D-RATE
           IF WS-CMP-ACA-1094-RDY = 'Y'
               MOVE 'READY     ' TO WS-R12D-STATUS
           ELSE
               MOVE 'NOT READY ' TO WS-R12D-STATUS
           END-IF
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

      *    MLR REGULATORY SECTION
           WRITE RPT12-RECORD FROM WS-BLANK-LINE
           MOVE '  MEDICAL LOSS RATIO (REGULATORY):'
               TO WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-SEPARATOR-LINE

           MOVE 'MLR RATIO                          '
               TO WS-R12D-LABEL
           MOVE 0 TO WS-R12D-VALUE
           COMPUTE WS-R12D-RATE = WS-CMP-MLR-RATIO * 100
           IF WS-CMP-MLR-RATIO >= 0.80
               MOVE 'COMPLIANT ' TO WS-R12D-STATUS
           ELSE
               MOVE 'BELOW 80% ' TO WS-R12D-STATUS
           END-IF
           WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
           ADD 1 TO WS-RPT12-RECORDS

      *    STATE-SPECIFIC COMPLIANCE
           WRITE RPT12-RECORD FROM WS-BLANK-LINE
           WRITE RPT12-RECORD FROM WS-DOUBLE-SEP-LINE
           MOVE '  STATE-SPECIFIC PROMPT PAY COMPLIANCE:'
               TO WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-WORK-STRING
           WRITE RPT12-RECORD FROM WS-SEPARATOR-LINE

           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 10
               IF WS-CMPS-STATE-CD(WS-WORK-INDEX) NOT = SPACES
                   MOVE SPACES TO WS-R12D-LABEL
                   STRING WS-CMPS-STATE-CD(WS-WORK-INDEX)
                       ' - '
                       WS-CMPS-STATE-NAME(WS-WORK-INDEX)
                       DELIMITED BY SIZE INTO WS-R12D-LABEL
                   MOVE WS-CMPS-CLM-CNT(WS-WORK-INDEX)
                       TO WS-R12D-VALUE
                   MOVE WS-CMPS-PP-COMPLY(WS-WORK-INDEX)
                       TO WS-R12D-RATE
                   IF WS-CMPS-PP-COMPLY(WS-WORK-INDEX) >= 95.00
                       MOVE 'COMPLIANT ' TO WS-R12D-STATUS
                   ELSE
                       MOVE 'AT RISK   ' TO WS-R12D-STATUS
                   END-IF

                   WRITE RPT12-RECORD FROM WS-RPT12-DETAIL
                   ADD 1 TO WS-RPT12-LINE-CTR
                   ADD 1 TO WS-RPT12-RECORDS
               END-IF
           END-PERFORM

           PERFORM 8000-GET-TIMESTAMP
           MOVE 'RPT-END   ' TO WS-AUD-EVENT-TYPE
           MOVE 'RPT12' TO WS-AUD-REPORT-ID
           MOVE WS-RPT12-RECORDS TO WS-AUD-RECORD-COUNT
           MOVE 'REGULATORY COMPLIANCE REPORT COMPLETE'
               TO WS-AUD-MESSAGE
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS.

      *================================================================*
      *    8000-GET-TIMESTAMP                                          *
      *    REFRESH CURRENT TIMESTAMP FOR AUDIT/ERROR LOGGING           *
      *================================================================*
       8000-GET-TIMESTAMP.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
                                         WS-CURRENT-TIME

           STRING WS-CURR-YYYY '-' WS-CURR-MM '-' WS-CURR-DD
               DELIMITED BY SIZE INTO WS-TS-DATE

           STRING WS-CURR-HH ':' WS-CURR-MN ':' WS-CURR-SS
               DELIMITED BY SIZE INTO WS-TS-TIME

           STRING WS-TS-DATE WS-TS-DASH1 WS-TS-TIME
                  WS-TS-DOT WS-TS-MICRO
               DELIMITED BY SIZE INTO WS-AUD-TIMESTAMP.

      *================================================================*
      *    8100-LOG-SQL-ERROR                                          *
      *    LOG SQL ERRORS TO ERROR FILE AND DISPLAY                    *
      *================================================================*
       8100-LOG-SQL-ERROR.

           PERFORM 8000-GET-TIMESTAMP

           MOVE WS-AUD-TIMESTAMP TO WS-ERR-TIMESTAMP
           MOVE 'E' TO WS-ERR-SEVERITY
           MOVE WS-PROGRAM-ID TO WS-ERR-PROGRAM
           MOVE SPACES TO WS-ERR-PARAGRAPH
           MOVE SQLCODE TO WS-ERR-SQLCODE

           EVALUATE TRUE
               WHEN SQLCODE = -803
                   MOVE 'DUPLICATE KEY ON INSERT'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -811
                   MOVE 'MORE THAN ONE ROW RETURNED BY SUBQUERY'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -904
                   MOVE 'RESOURCE UNAVAILABLE - TABLE/INDEX'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -911
                   MOVE 'DEADLOCK OR TIMEOUT - ROLLBACK'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -913
                   MOVE 'DEADLOCK - RESOURCE NOT AVAILABLE'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -922
                   MOVE 'AUTHORIZATION FAILURE'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -501
                   MOVE 'CURSOR NOT OPEN'
                       TO WS-ERR-MESSAGE
               WHEN SQLCODE = -204
                   MOVE 'OBJECT NOT DEFINED IN DB2'
                       TO WS-ERR-MESSAGE
               WHEN OTHER
                   STRING 'SQL ERROR SQLCODE=' SQLCODE
                       ' SQLSTATE=' SQLSTATE
                       DELIMITED BY SIZE INTO WS-ERR-MESSAGE
           END-EVALUATE

           WRITE ERROR-RECORD FROM WS-ERROR-LOG-REC
           ADD 1 TO WS-ERROR-RECORDS
           ADD 1 TO WS-TOTAL-ERRORS

           DISPLAY 'HCRPTGEN - SQL ERROR: SQLCODE=' SQLCODE
               ' ' WS-ERR-MESSAGE.

      *================================================================*
      *    8200-LOG-FILE-ERROR                                         *
      *    LOG FILE I/O ERRORS                                         *
      *================================================================*
       8200-LOG-FILE-ERROR.

           PERFORM 8000-GET-TIMESTAMP

           MOVE WS-AUD-TIMESTAMP TO WS-ERR-TIMESTAMP
           MOVE 'E' TO WS-ERR-SEVERITY
           MOVE WS-PROGRAM-ID TO WS-ERR-PROGRAM
           MOVE SPACES TO WS-ERR-PARAGRAPH
           MOVE 0 TO WS-ERR-SQLCODE
           MOVE 'FILE I/O ERROR - SEE FILE STATUS'
               TO WS-ERR-MESSAGE

           WRITE ERROR-RECORD FROM WS-ERROR-LOG-REC
           ADD 1 TO WS-ERROR-RECORDS
           ADD 1 TO WS-TOTAL-ERRORS.

      *================================================================*
      *    9000-TERMINATION                                            *
      *    CLOSE ALL FILES, WRITE SUMMARY, SET RETURN CODE             *
      *================================================================*
       9000-TERMINATION.

           DISPLAY '================================================'
           DISPLAY 'HCRPTGEN - BATCH REPORT GENERATION SUMMARY'
           DISPLAY '================================================'
           DISPLAY 'CONTROL RECORDS READ:    ' WS-TOTAL-CTRL-RECS
           DISPLAY 'TOTAL REPORTS GENERATED: ' WS-TOTAL-REPORTS-RUN
           DISPLAY 'RPT01 AGING RECORDS:     ' WS-RPT01-RECORDS
           DISPLAY 'RPT02 PROVIDER RECORDS:  ' WS-RPT02-RECORDS
           DISPLAY 'RPT03 PAYER MIX RECORDS: ' WS-RPT03-RECORDS
           DISPLAY 'RPT04 DENIAL RECORDS:    ' WS-RPT04-RECORDS
           DISPLAY 'RPT05 FINANCIAL RECORDS: ' WS-RPT05-RECORDS
           DISPLAY 'RPT06 PEND RECORDS:      ' WS-RPT06-RECORDS
           DISPLAY 'RPT07 AUTH RECORDS:      ' WS-RPT07-RECORDS
           DISPLAY 'RPT08 HIGH-DOLLAR RECS:  ' WS-RPT08-RECORDS
           DISPLAY 'RPT09 DUPLICATE RECORDS: ' WS-RPT09-RECORDS
           DISPLAY 'RPT10 QUALITY RECORDS:   ' WS-RPT10-RECORDS
           DISPLAY 'RPT11 FWA RECORDS:       ' WS-RPT11-RECORDS
           DISPLAY 'RPT12 COMPLIANCE RECS:   ' WS-RPT12-RECORDS
           DISPLAY 'TOTAL ERRORS:            ' WS-TOTAL-ERRORS
           DISPLAY 'TOTAL WARNINGS:          ' WS-TOTAL-WARNINGS
           DISPLAY 'AUDIT TRAIL RECORDS:     ' WS-AUDIT-RECORDS
           DISPLAY '================================================'

      *    WRITE FINAL AUDIT ENTRY
           PERFORM 8000-GET-TIMESTAMP
           MOVE 'END       ' TO WS-AUD-EVENT-TYPE
           MOVE 'BATCH' TO WS-AUD-REPORT-ID
           MOVE WS-TOTAL-REPORTS-RUN TO WS-AUD-RECORD-COUNT
           IF WS-TOTAL-ERRORS = 0
               MOVE 'BATCH COMPLETE - ALL REPORTS SUCCESSFUL'
                   TO WS-AUD-MESSAGE
           ELSE
               STRING 'BATCH COMPLETE WITH '
                   WS-TOTAL-ERRORS ' ERRORS'
                   DELIMITED BY SIZE INTO WS-AUD-MESSAGE
           END-IF
           WRITE AUDIT-RECORD FROM WS-AUDIT-LOG-REC
           ADD 1 TO WS-AUDIT-RECORDS

      *    CLOSE ALL FILES
           CLOSE RPTCTRL-FILE
           CLOSE RPT01-FILE
           CLOSE RPT02-FILE
           CLOSE RPT03-FILE
           CLOSE RPT04-FILE
           CLOSE RPT05-FILE
           CLOSE RPT06-FILE
           CLOSE RPT07-FILE
           CLOSE RPT08-FILE
           CLOSE RPT09-FILE
           CLOSE RPT10-FILE
           CLOSE RPT11-FILE
           CLOSE RPT12-FILE
           CLOSE ERROR-FILE
           CLOSE AUDIT-FILE

      *    SET RETURN CODE BASED ON ERRORS
           IF WS-TOTAL-ERRORS > 0
               MOVE 8 TO RETURN-CODE
               DISPLAY 'HCRPTGEN - ENDING WITH RC=8 (ERRORS)'
           ELSE IF WS-TOTAL-WARNINGS > 0
               MOVE 4 TO RETURN-CODE
               DISPLAY 'HCRPTGEN - ENDING WITH RC=4 (WARNINGS)'
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'HCRPTGEN - ENDING WITH RC=0 (SUCCESS)'
           END-IF

           DISPLAY 'HCRPTGEN - PROCESSING COMPLETE'.
