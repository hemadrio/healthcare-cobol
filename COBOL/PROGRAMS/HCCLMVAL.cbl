       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HCCLMVAL.
      *================================================================*
      * PROGRAM:     HCCLMVAL                                          *
      * DESCRIPTION: CLAIMS INTAKE VALIDATION AND EDIT CHECK ENGINE    *
      *              PERFORMS FRONT-END EDITS ON INCOMING CLAIMS        *
      *              BEFORE ADJUDICATION. VALIDATES AGAINST HIPAA      *
      *              REQUIREMENTS, CHECKS FIELD-LEVEL EDITS, AND       *
      *              PERFORMS CROSS-FIELD VALIDATION LOGIC.             *
      *              INCLUDES NCCI BUNDLING EDITS, MUE CHECKS,         *
      *              LCD/NCD COVERAGE DETERMINATIONS, GENDER/AGE       *
      *              EDITS, MODIFIER LOGIC, TELEHEALTH EDITS,          *
      *              MENTAL HEALTH PARITY, AND PREVENTIVE CARE         *
      *              MANDATE COMPLIANCE.                                *
      *                                                                *
      * SYSTEM:      HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)        *
      * AUTHOR:      SYSTEMS DEVELOPMENT GROUP                         *
      * DATE WRITTEN: 1994-06-15                                       *
      *                                                                *
      * MODIFICATION LOG:                                              *
      * DATE       AUTHOR   DESCRIPTION                                *
      * ---------- -------- ------------------------------------------ *
      * 1994-06-15 JSMITH   INITIAL DEVELOPMENT                        *
      * 1995-04-10 JSMITH   ADDED REVENUE CODE CROSS-WALK EDITS       *
      * 1996-01-20 BMURPHY  ADDED STATE-SPECIFIC MANDATE TABLES       *
      * 1996-11-01 RJONES   NCCI EDIT TABLES INITIAL LOAD             *
      * 1997-03-01 RJONES   ADDED COORDINATION OF BENEFITS EDITS      *
      * 1997-08-15 RJONES   MUE THRESHOLD TABLE IMPLEMENTATION        *
      * 1998-05-01 KPATEL   MODIFIER VALIDATION ENGINE REWRITE        *
      * 1999-02-15 KPATEL   ADDED GLOBAL SURGERY PERIOD EDITS         *
      * 2000-01-15 KPATEL   Y2K REMEDIATION                           *
      * 2000-07-01 MWILSON  GENDER/AGE SPECIFIC PROCEDURE EDITS       *
      * 2001-03-22 MWILSON  LCD/NCD COVERAGE DETERMINATION LOOKUPS    *
      * 2002-09-01 MWILSON  ADDED TELEHEALTH POS 02 EDITS             *
      * 2003-10-01 MWILSON  HIPAA 4010A TRANSACTION SET COMPLIANCE     *
      * 2004-06-15 AGARCIA  MENTAL HEALTH PARITY ACT EDITS            *
      * 2005-01-01 AGARCIA  DRG/LOS CONSISTENCY CHECKS                *
      * 2006-04-01 AGARCIA  EXPANDED CONDITION/OCCURRENCE CODE EDITS  *
      * 2007-05-23 AGARCIA  NPI VALIDATION ADDED                       *
      * 2008-01-01 TLEE     ACA PREVENTIVE CARE NO-COST-SHARING       *
      * 2009-06-15 TLEE     MHPAEA COMPLIANCE EXPANSIONS              *
      * 2010-01-01 TLEE     ICD-10 READINESS - DUAL CODING SUPPORT    *
      * 2011-03-01 BCHANG   ADDED ADD-ON CODE VALIDATION              *
      * 2012-04-15 BCHANG   5010 COMPANION GUIDE EDITS                *
      * 2013-10-01 BCHANG   EXPANDED NCCI COLUMN1/COLUMN2 EDITS      *
      * 2014-07-01 SRAO     MUTUALLY EXCLUSIVE PROCEDURE EDITS        *
      * 2015-10-01 SRAO     ICD-10 IMPLEMENTATION - REMOVED ICD-9     *
      * 2016-03-15 SRAO     COMPONENT/COMPREHENSIVE EDITING           *
      * 2017-01-01 DKIM     EXPANDED STATE MANDATE TABLES (50 STATES) *
      * 2018-03-01 DKIM     ADDED OPIOID PRESCRIPTION EDITS           *
      * 2019-05-01 DKIM     TELEHEALTH EXPANSION - MODIFIER 95        *
      * 2020-03-15 JNGUYEN  COVID-19 PHE EMERGENCY BILLING CODES      *
      * 2020-11-15 JNGUYEN  COVID-19 VACCINE NO-COST-SHARING          *
      * 2021-07-01 JNGUYEN  AUDIO-ONLY TELEHEALTH EDITS               *
      * 2022-01-01 MTHOMAS  NO SURPRISES ACT COMPLIANCE               *
      * 2022-06-15 MTHOMAS  IDR PROCESS SUPPORT FIELDS                *
      * 2023-06-01 MTHOMAS  EXPANDED CROSSOVER CLAIM VALIDATION       *
      * 2024-01-15 PWRIGHT  ADDED X-MODIFIER LOGIC (XE/XP/XS/XU)     *
      * 2024-09-01 PWRIGHT  RETROSPECTIVE AUTH EMERGENCY LOGIC        *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-INPUT-FILE
               ASSIGN TO CLMINPUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CLMIN-STATUS.

           SELECT VALID-CLAIM-FILE
               ASSIGN TO CLMVALID
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CLMVL-STATUS.

           SELECT REJECT-CLAIM-FILE
               ASSIGN TO CLMREJCT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CLMRJ-STATUS.

           SELECT ERROR-REPORT-FILE
               ASSIGN TO ERRRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERRRP-STATUS.

           SELECT EDIT-RULES-FILE
               ASSIGN TO EDITRULE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ER-RULE-KEY
               FILE STATUS IS WS-EDRUL-STATUS.

           SELECT NCCI-EDIT-FILE
               ASSIGN TO NCCIEDIT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS NE-EDIT-KEY
               FILE STATUS IS WS-NCCI-STATUS.

           SELECT MUE-THRESHOLD-FILE
               ASSIGN TO MUETHRES
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MT-MUE-KEY
               FILE STATUS IS WS-MUE-STATUS.

           SELECT LCD-NCD-FILE
               ASSIGN TO LCDNCD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LN-LCD-KEY
               FILE STATUS IS WS-LCD-STATUS.

           SELECT STATE-MANDATE-FILE
               ASSIGN TO STATEMND
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SM-STATE-KEY
               FILE STATUS IS WS-STMND-STATUS.

           SELECT CROSSOVER-OUTPUT-FILE
               ASSIGN TO XOVROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-XOVR-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CLAIM-INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 4096 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  CLAIM-INPUT-REC                 PIC X(4096).

       FD  VALID-CLAIM-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 4096 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  VALID-CLAIM-REC                 PIC X(4096).

       FD  REJECT-CLAIM-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 4200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  REJECT-CLAIM-REC                PIC X(4200).

       FD  ERROR-REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  ERROR-REPORT-REC                PIC X(133).

       FD  EDIT-RULES-FILE
           RECORD CONTAINS 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  EDIT-RULES-REC.
           05  ER-RULE-KEY.
               10  ER-RULE-TYPE            PIC X(02).
               10  ER-RULE-NUMBER          PIC X(06).
           05  ER-RULE-DESC                PIC X(80).
           05  ER-RULE-SEVERITY            PIC X(01).
               88  ER-RULE-FATAL           VALUE 'F'.
               88  ER-RULE-WARN            VALUE 'W'.
               88  ER-RULE-INFO            VALUE 'I'.
           05  ER-RULE-ACTIVE-FLAG         PIC X(01).
               88  ER-RULE-ACTIVE          VALUE 'Y'.
           05  ER-RULE-EFF-DT              PIC 9(08).
           05  ER-RULE-TERM-DT             PIC 9(08).
           05  ER-RULE-PAYER-SPEC          PIC X(08).
           05  ER-FILLER                   PIC X(86).

       FD  NCCI-EDIT-FILE
           RECORD CONTAINS 120 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  NCCI-EDIT-REC.
           05  NE-EDIT-KEY.
               10  NE-COL1-CPT             PIC X(05).
               10  NE-COL2-CPT             PIC X(05).
           05  NE-EDIT-TYPE                PIC X(01).
               88  NE-COLUMN-EDIT          VALUE '1'.
               88  NE-MUTUALLY-EXCL        VALUE '2'.
           05  NE-MODIFIER-IND             PIC X(01).
               88  NE-MOD-ALLOWED          VALUE '1'.
               88  NE-MOD-NOT-ALLOWED      VALUE '0'.
               88  NE-MOD-NA               VALUE '9'.
           05  NE-EFF-DT                   PIC 9(08).
           05  NE-TERM-DT                  PIC 9(08).
           05  NE-RATIONALE                PIC X(80).
           05  NE-FILLER                   PIC X(12).

       FD  MUE-THRESHOLD-FILE
           RECORD CONTAINS 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  MUE-THRESHOLD-REC.
           05  MT-MUE-KEY.
               10  MT-CPT-CD               PIC X(05).
               10  MT-PRACT-IND            PIC X(01).
           05  MT-MUE-VALUE                PIC 9(03).
           05  MT-MUE-ADJUD-IND            PIC X(01).
               88  MT-MUE-LINE-EDIT        VALUE '1'.
               88  MT-MUE-DAY-EDIT         VALUE '2'.
               88  MT-MUE-DATE-EDIT        VALUE '3'.
           05  MT-MUE-EFF-DT              PIC 9(08).
           05  MT-MUE-TERM-DT             PIC 9(08).
           05  MT-MUE-FILLER              PIC X(54).

       FD  LCD-NCD-FILE
           RECORD CONTAINS 300 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LCD-NCD-REC.
           05  LN-LCD-KEY.
               10  LN-COVERAGE-TYPE        PIC X(01).
                   88  LN-IS-LCD           VALUE 'L'.
                   88  LN-IS-NCD           VALUE 'N'.
               10  LN-COVERAGE-ID          PIC X(10).
           05  LN-PROC-CD                  PIC X(05).
           05  LN-DIAG-TABLE.
               10  LN-DIAG-ENTRY OCCURS 20 TIMES.
                   15  LN-COVERED-DIAG     PIC X(08).
           05  LN-CONTRACTOR-ID            PIC X(05).
           05  LN-EFF-DT                   PIC 9(08).
           05  LN-TERM-DT                  PIC 9(08).
           05  LN-LCD-FILLER               PIC X(111).

       FD  STATE-MANDATE-FILE
           RECORD CONTAINS 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  STATE-MANDATE-REC.
           05  SM-STATE-KEY.
               10  SM-STATE-CD             PIC X(02).
               10  SM-MANDATE-TYPE         PIC X(03).
           05  SM-MANDATE-DESC             PIC X(80).
           05  SM-PROC-CD                  PIC X(05).
           05  SM-DIAG-CD                  PIC X(08).
           05  SM-COVERAGE-REQ             PIC X(01).
               88  SM-MUST-COVER           VALUE 'Y'.
               88  SM-MUST-NOT-COVER       VALUE 'N'.
           05  SM-NO-COST-SHARING          PIC X(01).
               88  SM-WAIVE-COST-SHARE     VALUE 'Y'.
           05  SM-EFF-DT                   PIC 9(08).
           05  SM-TERM-DT                  PIC 9(08).
           05  SM-FILLER                   PIC X(88).

       FD  CROSSOVER-OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 4096 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  CROSSOVER-OUTPUT-REC            PIC X(4096).

       WORKING-STORAGE SECTION.

       01  WS-PROGRAM-FIELDS.
           05  WS-PROGRAM-NAME             PIC X(08) VALUE 'HCCLMVAL'.
           05  WS-PROGRAM-VERSION          PIC X(06) VALUE '14.02 '.
           05  WS-COMPILE-DT               PIC X(10) VALUE '2024-09-01'.

       01  WS-FILE-STATUSES.
           05  WS-CLMIN-STATUS             PIC X(02).
           05  WS-CLMVL-STATUS             PIC X(02).
           05  WS-CLMRJ-STATUS             PIC X(02).
           05  WS-ERRRP-STATUS             PIC X(02).
           05  WS-EDRUL-STATUS             PIC X(02).
           05  WS-NCCI-STATUS              PIC X(02).
           05  WS-MUE-STATUS               PIC X(02).
           05  WS-LCD-STATUS               PIC X(02).
           05  WS-STMND-STATUS             PIC X(02).
           05  WS-XOVR-STATUS              PIC X(02).

       01  WS-FLAGS-AND-SWITCHES.
           05  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-END-OF-FILE          VALUE 'Y'.
               88  WS-NOT-END-OF-FILE      VALUE 'N'.
           05  WS-VALID-CLAIM-FLAG         PIC X(01) VALUE 'Y'.
               88  WS-CLAIM-IS-VALID       VALUE 'Y'.
               88  WS-CLAIM-IS-INVALID     VALUE 'N'.
           05  WS-FATAL-EDIT-FLAG          PIC X(01) VALUE 'N'.
               88  WS-HAS-FATAL-EDIT       VALUE 'Y'.
               88  WS-NO-FATAL-EDITS       VALUE 'N'.
           05  WS-DB-LOOKUP-FLAG           PIC X(01) VALUE 'N'.
               88  WS-DB-FOUND             VALUE 'Y'.
               88  WS-DB-NOT-FND           VALUE 'N'.
           05  WS-TIMELY-FILING-FLAG       PIC X(01) VALUE 'Y'.
               88  WS-TIMELY-FILED         VALUE 'Y'.
               88  WS-NOT-TIMELY           VALUE 'N'.
           05  WS-DUP-CHECK-FLAG           PIC X(01) VALUE 'N'.
               88  WS-IS-DUPLICATE         VALUE 'Y'.
               88  WS-NOT-DUPLICATE        VALUE 'N'.
           05  WS-AUTH-REQUIRED-FLAG       PIC X(01) VALUE 'N'.
               88  WS-AUTH-REQUIRED        VALUE 'Y'.
               88  WS-AUTH-NOT-REQUIRED    VALUE 'N'.
           05  WS-COB-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-HAS-COB             VALUE 'Y'.
               88  WS-NO-COB              VALUE 'N'.
           05  WS-NSA-APPLICABLE           PIC X(01) VALUE 'N'.
               88  WS-NSA-APPLIES          VALUE 'Y'.
           05  WS-NCCI-FAIL-FLAG           PIC X(01) VALUE 'N'.
               88  WS-NCCI-FAILED          VALUE 'Y'.
               88  WS-NCCI-PASSED          VALUE 'N'.
           05  WS-MUE-FAIL-FLAG            PIC X(01) VALUE 'N'.
               88  WS-MUE-FAILED           VALUE 'Y'.
               88  WS-MUE-PASSED           VALUE 'N'.
           05  WS-GENDER-EDIT-FLAG         PIC X(01) VALUE 'N'.
               88  WS-GENDER-MISMATCH      VALUE 'Y'.
               88  WS-GENDER-OK            VALUE 'N'.
           05  WS-AGE-EDIT-FLAG            PIC X(01) VALUE 'N'.
               88  WS-AGE-MISMATCH         VALUE 'Y'.
               88  WS-AGE-OK               VALUE 'N'.
           05  WS-MODIFIER-VALID-FLAG      PIC X(01) VALUE 'Y'.
               88  WS-MODIFIER-VALID       VALUE 'Y'.
               88  WS-MODIFIER-INVALID     VALUE 'N'.
           05  WS-LCD-FOUND-FLAG           PIC X(01) VALUE 'N'.
               88  WS-LCD-FOUND            VALUE 'Y'.
               88  WS-LCD-NOT-FOUND        VALUE 'N'.
           05  WS-NCD-FOUND-FLAG           PIC X(01) VALUE 'N'.
               88  WS-NCD-FOUND            VALUE 'Y'.
               88  WS-NCD-NOT-FOUND        VALUE 'N'.
           05  WS-TELEHEALTH-FLAG          PIC X(01) VALUE 'N'.
               88  WS-IS-TELEHEALTH        VALUE 'Y'.
               88  WS-NOT-TELEHEALTH       VALUE 'N'.
           05  WS-PREVENTIVE-FLAG          PIC X(01) VALUE 'N'.
               88  WS-IS-PREVENTIVE        VALUE 'Y'.
               88  WS-NOT-PREVENTIVE       VALUE 'N'.
           05  WS-MH-PARITY-FLAG           PIC X(01) VALUE 'N'.
               88  WS-MH-PARITY-APPLIES    VALUE 'Y'.
               88  WS-MH-PARITY-NA         VALUE 'N'.
           05  WS-CROSSOVER-FLAG           PIC X(01) VALUE 'N'.
               88  WS-IS-CROSSOVER         VALUE 'Y'.
               88  WS-NOT-CROSSOVER        VALUE 'N'.
           05  WS-ADDON-CODE-FLAG          PIC X(01) VALUE 'N'.
               88  WS-IS-ADDON-CODE        VALUE 'Y'.
               88  WS-NOT-ADDON-CODE       VALUE 'N'.
           05  WS-GLOBAL-SURG-FLAG         PIC X(01) VALUE 'N'.
               88  WS-IN-GLOBAL-PERIOD     VALUE 'Y'.
               88  WS-NOT-IN-GLOBAL        VALUE 'N'.
           05  WS-AUTH-VALID-FLAG          PIC X(01) VALUE 'N'.
               88  WS-AUTH-IS-VALID        VALUE 'Y'.
               88  WS-AUTH-IS-INVALID      VALUE 'N'.
           05  WS-RETRO-AUTH-FLAG          PIC X(01) VALUE 'N'.
               88  WS-IS-RETRO-AUTH        VALUE 'Y'.
               88  WS-NOT-RETRO-AUTH       VALUE 'N'.

       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR            PIC 9(04).
               10  WS-CURR-MONTH           PIC 9(02).
               10  WS-CURR-DAY             PIC 9(02).
           05  WS-CURRENT-DATE-8           PIC 9(08).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR            PIC 9(02).
               10  WS-CURR-MIN             PIC 9(02).
               10  WS-CURR-SEC             PIC 9(02).
           05  WS-CURRENT-TIMESTAMP        PIC X(26).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-DATE-WORK-R REDEFINES WS-DATE-WORK.
               10  WS-DW-CC                PIC 9(02).
               10  WS-DW-YY                PIC 9(02).
               10  WS-DW-MM                PIC 9(02).
               10  WS-DW-DD                PIC 9(02).
           05  WS-DAYS-DIFF                PIC S9(05) COMP.
           05  WS-TIMELY-FILE-DAYS         PIC 9(04) VALUE 365.
           05  WS-INTEGER-DATE-1           PIC 9(07).
           05  WS-INTEGER-DATE-2           PIC 9(07).
           05  WS-DATE-WORK-2              PIC 9(08).
           05  WS-PATIENT-AGE-YEARS        PIC 9(03).
           05  WS-PATIENT-AGE-DAYS         PIC S9(05) COMP.
           05  WS-LOS-DAYS                 PIC S9(05) COMP.
           05  WS-GLOBAL-SURG-DAYS         PIC 9(03).
           05  WS-GLOBAL-SURG-END-DT       PIC 9(08).
           05  WS-AUTH-EFF-DT-WK           PIC 9(08).
           05  WS-AUTH-TERM-DT-WK          PIC 9(08).

      *-----------------------------------------------------------------
      * NCCI (NATIONAL CORRECT CODING INITIATIVE) EDIT TABLES
      * LOADED FROM NCCI-EDIT-FILE AT INITIALIZATION
      * COLUMN 1/COLUMN 2 CODE PAIR EDITS FOR BUNDLING
      *-----------------------------------------------------------------
       01  WS-NCCI-EDIT-TABLE.
           05  WS-NCCI-ENTRY-COUNT         PIC 9(05) VALUE 0.
           05  WS-NCCI-ENTRY OCCURS 5000 TIMES
               INDEXED BY WS-NCCI-IDX.
               10  WS-NCCI-COL1-CPT        PIC X(05).
               10  WS-NCCI-COL2-CPT        PIC X(05).
               10  WS-NCCI-EDIT-TYPE        PIC X(01).
                   88  WS-NCCI-COLUMN       VALUE '1'.
                   88  WS-NCCI-MUTEX        VALUE '2'.
               10  WS-NCCI-MOD-IND         PIC X(01).
                   88  WS-NCCI-MOD-OK       VALUE '1'.
                   88  WS-NCCI-MOD-NO       VALUE '0'.
                   88  WS-NCCI-MOD-NA2      VALUE '9'.
               10  WS-NCCI-EFF-DT          PIC 9(08).
               10  WS-NCCI-TERM-DT         PIC 9(08).

      *-----------------------------------------------------------------
      * MUE (MEDICALLY UNLIKELY EDITS) THRESHOLD TABLES
      * MAXIMUM UNITS OF SERVICE PER CPT CODE PER DAY
      *-----------------------------------------------------------------
       01  WS-MUE-THRESHOLD-TABLE.
           05  WS-MUE-ENTRY-COUNT          PIC 9(05) VALUE 0.
           05  WS-MUE-ENTRY OCCURS 3000 TIMES
               INDEXED BY WS-MUE-IDX.
               10  WS-MUE-CPT-CD           PIC X(05).
               10  WS-MUE-PRACT-IND        PIC X(01).
               10  WS-MUE-MAX-UNITS        PIC 9(03).
               10  WS-MUE-ADJUD-IND        PIC X(01).
                   88  WS-MUE-BY-LINE       VALUE '1'.
                   88  WS-MUE-BY-DAY        VALUE '2'.
                   88  WS-MUE-BY-DATE       VALUE '3'.
               10  WS-MUE-EFF-DT           PIC 9(08).
               10  WS-MUE-TERM-DT          PIC 9(08).

      *-----------------------------------------------------------------
      * LCD/NCD (LOCAL/NATIONAL COVERAGE DETERMINATION) TABLES
      *-----------------------------------------------------------------
       01  WS-LCD-NCD-TABLE.
           05  WS-LCD-ENTRY-COUNT          PIC 9(04) VALUE 0.
           05  WS-LCD-ENTRY OCCURS 2000 TIMES
               INDEXED BY WS-LCD-IDX.
               10  WS-LCD-COV-TYPE         PIC X(01).
               10  WS-LCD-COV-ID           PIC X(10).
               10  WS-LCD-PROC-CD          PIC X(05).
               10  WS-LCD-DIAG-TBL.
                   15  WS-LCD-DIAG-CD OCCURS 20 TIMES
                                           PIC X(08).
               10  WS-LCD-CONTRACTOR       PIC X(05).
               10  WS-LCD-EFF-DT           PIC 9(08).
               10  WS-LCD-TERM-DT          PIC 9(08).

      *-----------------------------------------------------------------
      * GENDER-SPECIFIC PROCEDURE TABLE
      * PROCEDURES THAT REQUIRE A SPECIFIC PATIENT GENDER
      *-----------------------------------------------------------------
       01  WS-GENDER-PROC-TABLE.
           05  WS-GENDER-PROC-COUNT        PIC 9(04) VALUE 68.
           05  WS-GENDER-PROC-DATA.
      *---     FORMAT: CPT-CODE + REQUIRED-GENDER (M/F)
               10  FILLER PIC X(06) VALUE '1992 F'.
               10  FILLER PIC X(06) VALUE '19303F'.
               10  FILLER PIC X(06) VALUE '19304F'.
               10  FILLER PIC X(06) VALUE '19305F'.
               10  FILLER PIC X(06) VALUE '19306F'.
               10  FILLER PIC X(06) VALUE '19307F'.
               10  FILLER PIC X(06) VALUE '19316F'.
               10  FILLER PIC X(06) VALUE '19318F'.
               10  FILLER PIC X(06) VALUE '19325F'.
               10  FILLER PIC X(06) VALUE '19328F'.
               10  FILLER PIC X(06) VALUE '19330F'.
               10  FILLER PIC X(06) VALUE '77065F'.
               10  FILLER PIC X(06) VALUE '77066F'.
               10  FILLER PIC X(06) VALUE '77067F'.
               10  FILLER PIC X(06) VALUE 'G0202F'.
               10  FILLER PIC X(06) VALUE 'G0204F'.
               10  FILLER PIC X(06) VALUE 'G0206F'.
               10  FILLER PIC X(06) VALUE '58150F'.
               10  FILLER PIC X(06) VALUE '58152F'.
               10  FILLER PIC X(06) VALUE '58180F'.
               10  FILLER PIC X(06) VALUE '58200F'.
               10  FILLER PIC X(06) VALUE '58210F'.
               10  FILLER PIC X(06) VALUE '58260F'.
               10  FILLER PIC X(06) VALUE '58262F'.
               10  FILLER PIC X(06) VALUE '58263F'.
               10  FILLER PIC X(06) VALUE '58267F'.
               10  FILLER PIC X(06) VALUE '58270F'.
               10  FILLER PIC X(06) VALUE '58275F'.
               10  FILLER PIC X(06) VALUE '58280F'.
               10  FILLER PIC X(06) VALUE '58285F'.
               10  FILLER PIC X(06) VALUE '58290F'.
               10  FILLER PIC X(06) VALUE '58291F'.
               10  FILLER PIC X(06) VALUE '58292F'.
               10  FILLER PIC X(06) VALUE '58294F'.
               10  FILLER PIC X(06) VALUE '58300F'.
               10  FILLER PIC X(06) VALUE '58301F'.
               10  FILLER PIC X(06) VALUE '58600F'.
               10  FILLER PIC X(06) VALUE '58605F'.
               10  FILLER PIC X(06) VALUE '58611F'.
               10  FILLER PIC X(06) VALUE '58615F'.
               10  FILLER PIC X(06) VALUE '58660F'.
               10  FILLER PIC X(06) VALUE '58661F'.
               10  FILLER PIC X(06) VALUE '58670F'.
               10  FILLER PIC X(06) VALUE '58671F'.
               10  FILLER PIC X(06) VALUE '59400F'.
               10  FILLER PIC X(06) VALUE '59409F'.
               10  FILLER PIC X(06) VALUE '59410F'.
               10  FILLER PIC X(06) VALUE '59412F'.
               10  FILLER PIC X(06) VALUE '59414F'.
               10  FILLER PIC X(06) VALUE '59510F'.
               10  FILLER PIC X(06) VALUE '59514F'.
               10  FILLER PIC X(06) VALUE '59515F'.
               10  FILLER PIC X(06) VALUE '59610F'.
               10  FILLER PIC X(06) VALUE '59612F'.
               10  FILLER PIC X(06) VALUE '59614F'.
               10  FILLER PIC X(06) VALUE '59618F'.
               10  FILLER PIC X(06) VALUE '59620F'.
               10  FILLER PIC X(06) VALUE '59622F'.
               10  FILLER PIC X(06) VALUE '55700M'.
               10  FILLER PIC X(06) VALUE '55705M'.
               10  FILLER PIC X(06) VALUE '55706M'.
               10  FILLER PIC X(06) VALUE '55810M'.
               10  FILLER PIC X(06) VALUE '55812M'.
               10  FILLER PIC X(06) VALUE '55815M'.
               10  FILLER PIC X(06) VALUE '55821M'.
               10  FILLER PIC X(06) VALUE '55831M'.
               10  FILLER PIC X(06) VALUE '55840M'.
               10  FILLER PIC X(06) VALUE '55842M'.
               10  FILLER PIC X(06) VALUE '55845M'.
               10  FILLER PIC X(06) VALUE '55866M'.
           05  WS-GENDER-PROC-TBL REDEFINES
               WS-GENDER-PROC-DATA.
               10  WS-GENDER-PROC-ENT OCCURS 68 TIMES
                   INDEXED BY WS-GENDER-IDX.
                   15  WS-GENDER-CPT        PIC X(05).
                   15  WS-GENDER-REQ        PIC X(01).

      *-----------------------------------------------------------------
      * AGE-SPECIFIC PROCEDURE TABLE
      * FORMAT: CPT + MIN-AGE + MAX-AGE (999 = NO MAX)
      *-----------------------------------------------------------------
       01  WS-AGE-PROC-TABLE.
           05  WS-AGE-PROC-COUNT           PIC 9(04) VALUE 30.
           05  WS-AGE-PROC-DATA.
               10  FILLER PIC X(11) VALUE '99381000001'.
               10  FILLER PIC X(11) VALUE '99382000004'.
               10  FILLER PIC X(11) VALUE '99383005011'.
               10  FILLER PIC X(11) VALUE '99384012017'.
               10  FILLER PIC X(11) VALUE '99385018039'.
               10  FILLER PIC X(11) VALUE '99386040064'.
               10  FILLER PIC X(11) VALUE '99387065999'.
               10  FILLER PIC X(11) VALUE '99391000001'.
               10  FILLER PIC X(11) VALUE '99392000004'.
               10  FILLER PIC X(11) VALUE '99393005011'.
               10  FILLER PIC X(11) VALUE '99394012017'.
               10  FILLER PIC X(11) VALUE '99395018039'.
               10  FILLER PIC X(11) VALUE '99396040064'.
               10  FILLER PIC X(11) VALUE '99397065999'.
               10  FILLER PIC X(11) VALUE '99460000000'.
               10  FILLER PIC X(11) VALUE '99461000000'.
               10  FILLER PIC X(11) VALUE '99462000000'.
               10  FILLER PIC X(11) VALUE '99463000000'.
               10  FILLER PIC X(11) VALUE '99464000000'.
               10  FILLER PIC X(11) VALUE '99465000000'.
               10  FILLER PIC X(11) VALUE '99477000000'.
               10  FILLER PIC X(11) VALUE '99478000000'.
               10  FILLER PIC X(11) VALUE '99479000000'.
               10  FILLER PIC X(11) VALUE '99480000000'.
               10  FILLER PIC X(11) VALUE '90630018045'.
               10  FILLER PIC X(11) VALUE '90651009026'.
               10  FILLER PIC X(11) VALUE '90670000005'.
               10  FILLER PIC X(11) VALUE '90680000032'.
               10  FILLER PIC X(11) VALUE '90696004006'.
               10  FILLER PIC X(11) VALUE '90714007999'.
           05  WS-AGE-PROC-TBL REDEFINES WS-AGE-PROC-DATA.
               10  WS-AGE-PROC-ENT OCCURS 30 TIMES
                   INDEXED BY WS-AGE-IDX.
                   15  WS-AGE-CPT           PIC X(05).
                   15  WS-AGE-MIN           PIC 9(03).
                   15  WS-AGE-MAX           PIC 9(03).

      *-----------------------------------------------------------------
      * PLACE OF SERVICE CODE VALIDATION TABLE
      * ALL VALID CMS PLACE OF SERVICE CODES
      *-----------------------------------------------------------------
       01  WS-POS-VALID-TABLE.
           05  WS-POS-VALID-COUNT          PIC 9(03) VALUE 55.
           05  WS-POS-VALID-DATA.
               10  FILLER PIC X(32) VALUE '01PHARMACY                      '.
               10  FILLER PIC X(32) VALUE '02TELEHEALTH-PROVIDED OTHER SITE'.
               10  FILLER PIC X(32) VALUE '03SCHOOL                        '.
               10  FILLER PIC X(32) VALUE '04HOMELESS SHELTER              '.
               10  FILLER PIC X(32) VALUE '05INDIAN HEALTH SERVICE-FREE    '.
               10  FILLER PIC X(32) VALUE '06INDIAN HEALTH SERVICE-PROVIDER'.
               10  FILLER PIC X(32) VALUE '07TRIBAL 638 FREE-STANDING      '.
               10  FILLER PIC X(32) VALUE '08TRIBAL 638 PROVIDER-BASED     '.
               10  FILLER PIC X(32) VALUE '09PRISON/CORRECTIONAL FACILITY  '.
               10  FILLER PIC X(32) VALUE '10TELEHEALTH IN PATIENT HOME    '.
               10  FILLER PIC X(32) VALUE '11OFFICE                        '.
               10  FILLER PIC X(32) VALUE '12HOME                          '.
               10  FILLER PIC X(32) VALUE '13ASSISTED LIVING FACILITY      '.
               10  FILLER PIC X(32) VALUE '14GROUP HOME                    '.
               10  FILLER PIC X(32) VALUE '15MOBILE UNIT                   '.
               10  FILLER PIC X(32) VALUE '16TEMPORARY LODGING             '.
               10  FILLER PIC X(32) VALUE '17WALK-IN RETAIL HEALTH CLINIC  '.
               10  FILLER PIC X(32) VALUE '18PLACE OF EMPLOYMENT/WORKSITE  '.
               10  FILLER PIC X(32) VALUE '19OFF CAMPUS-OUTPATIENT HOSPITAL'.
               10  FILLER PIC X(32) VALUE '20URGENT CARE FACILITY          '.
               10  FILLER PIC X(32) VALUE '21INPATIENT HOSPITAL            '.
               10  FILLER PIC X(32) VALUE '22ON CAMPUS-OUTPATIENT HOSPITAL '.
               10  FILLER PIC X(32) VALUE '23EMERGENCY ROOM - HOSPITAL     '.
               10  FILLER PIC X(32) VALUE '24AMBULATORY SURGICAL CENTER    '.
               10  FILLER PIC X(32) VALUE '25BIRTHING CENTER               '.
               10  FILLER PIC X(32) VALUE '26MILITARY TREATMENT FACILITY   '.
               10  FILLER PIC X(32) VALUE '27OUTREACH SITE / STREET        '.
               10  FILLER PIC X(32) VALUE '31SKILLED NURSING FACILITY      '.
               10  FILLER PIC X(32) VALUE '32NURSING FACILITY              '.
               10  FILLER PIC X(32) VALUE '33CUSTODIAL CARE FACILITY       '.
               10  FILLER PIC X(32) VALUE '34HOSPICE                       '.
               10  FILLER PIC X(32) VALUE '41AMBULANCE - LAND              '.
               10  FILLER PIC X(32) VALUE '42AMBULANCE - AIR OR WATER      '.
               10  FILLER PIC X(32) VALUE '49INDEPENDENT CLINIC            '.
               10  FILLER PIC X(32) VALUE '50FEDERALLY QUALIFIED HC CENTER '.
               10  FILLER PIC X(32) VALUE '51INPATIENT PSYCHIATRIC FACILITY'.
               10  FILLER PIC X(32) VALUE '52PSYCHIATRIC FACILITY-PART HOSP'.
               10  FILLER PIC X(32) VALUE '53COMMUNITY MENTAL HEALTH CENTER'.
               10  FILLER PIC X(32) VALUE '54INTERMEDIATE CARE/INTELL DISAB'.
               10  FILLER PIC X(32) VALUE '55RESIDENTIAL SUBSTANCE ABUSE TX'.
               10  FILLER PIC X(32) VALUE '56PSYCHIATRIC RESIDENTIAL TX CTR'.
               10  FILLER PIC X(32) VALUE '57NON-RESIDENTIAL SUBSTANCE ABUS'.
               10  FILLER PIC X(32) VALUE '58NON-RESIDENTIAL OPIOID TX     '.
               10  FILLER PIC X(32) VALUE '60MASS IMMUNIZATION CENTER      '.
               10  FILLER PIC X(32) VALUE '61COMPREHENSIVE INPAT REHAB FAC '.
               10  FILLER PIC X(32) VALUE '62COMPREHENSIVE OUTPAT REHAB FAC'.
               10  FILLER PIC X(32) VALUE '65END-STAGE RENAL DISEASE TX FAC'.
               10  FILLER PIC X(32) VALUE '71PUBLIC HEALTH CLINIC          '.
               10  FILLER PIC X(32) VALUE '72RURAL HEALTH CLINIC           '.
               10  FILLER PIC X(32) VALUE '81INDEPENDENT LABORATORY        '.
               10  FILLER PIC X(32) VALUE '99OTHER PLACE OF SERVICE        '.
               10  FILLER PIC X(32) VALUE '28PROSTHETIC CENTER             '.
               10  FILLER PIC X(32) VALUE '29PROSTHETIC LAB                '.
               10  FILLER PIC X(32) VALUE '35ADULT LIVING CARE FACILITY    '.
               10  FILLER PIC X(32) VALUE '36MILITARY/UNIF SVC PHARMACY    '.
           05  WS-POS-TBL REDEFINES WS-POS-VALID-DATA.
               10  WS-POS-ENTRY OCCURS 55 TIMES
                   INDEXED BY WS-POS-IDX.
                   15  WS-POS-CODE          PIC X(02).
                   15  WS-POS-DESC          PIC X(30).

      *-----------------------------------------------------------------
      * MODIFIER VALIDATION TABLES
      *-----------------------------------------------------------------
       01  WS-MODIFIER-TABLE.
           05  WS-MOD-WORK-1               PIC X(02).
           05  WS-MOD-WORK-2               PIC X(02).
           05  WS-MOD-HAS-25               PIC X(01) VALUE 'N'.
               88  WS-MOD-25-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-26               PIC X(01) VALUE 'N'.
               88  WS-MOD-26-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-TC               PIC X(01) VALUE 'N'.
               88  WS-MOD-TC-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-50               PIC X(01) VALUE 'N'.
               88  WS-MOD-50-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-51               PIC X(01) VALUE 'N'.
               88  WS-MOD-51-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-59               PIC X(01) VALUE 'N'.
               88  WS-MOD-59-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-76               PIC X(01) VALUE 'N'.
               88  WS-MOD-76-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-77               PIC X(01) VALUE 'N'.
               88  WS-MOD-77-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-78               PIC X(01) VALUE 'N'.
               88  WS-MOD-78-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-79               PIC X(01) VALUE 'N'.
               88  WS-MOD-79-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-RT               PIC X(01) VALUE 'N'.
               88  WS-MOD-RT-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-LT               PIC X(01) VALUE 'N'.
               88  WS-MOD-LT-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-XE               PIC X(01) VALUE 'N'.
               88  WS-MOD-XE-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-XP               PIC X(01) VALUE 'N'.
               88  WS-MOD-XP-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-XS               PIC X(01) VALUE 'N'.
               88  WS-MOD-XS-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-XU               PIC X(01) VALUE 'N'.
               88  WS-MOD-XU-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-95               PIC X(01) VALUE 'N'.
               88  WS-MOD-95-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-GQ               PIC X(01) VALUE 'N'.
               88  WS-MOD-GQ-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-GT               PIC X(01) VALUE 'N'.
               88  WS-MOD-GT-PRESENT       VALUE 'Y'.
           05  WS-MOD-HAS-FQ               PIC X(01) VALUE 'N'.
               88  WS-MOD-FQ-PRESENT       VALUE 'Y'.
           05  WS-MOD-COUNT                PIC 9(02) VALUE 0.

      *-----------------------------------------------------------------
      * REVENUE CODE / HCPCS CROSS-WALK TABLE
      * VALIDATES THAT REVENUE CODES AND HCPCS CODES ARE COMPATIBLE
      *-----------------------------------------------------------------
       01  WS-REVCD-HCPCS-XWALK.
           05  WS-XWALK-COUNT              PIC 9(04) VALUE 40.
           05  WS-XWALK-DATA.
      *---     FORMAT: REV-CODE + HCPCS-PREFIX (FIRST 2 CHARS)
               10  FILLER PIC X(06) VALUE '025099'.
               10  FILLER PIC X(06) VALUE '026099'.
               10  FILLER PIC X(06) VALUE '027099'.
               10  FILLER PIC X(06) VALUE '028099'.
               10  FILLER PIC X(06) VALUE '029099'.
               10  FILLER PIC X(06) VALUE '030099'.
               10  FILLER PIC X(06) VALUE '031010'.
               10  FILLER PIC X(06) VALUE '032010'.
               10  FILLER PIC X(06) VALUE '033020'.
               10  FILLER PIC X(06) VALUE '034020'.
               10  FILLER PIC X(06) VALUE '035036'.
               10  FILLER PIC X(06) VALUE '036036'.
               10  FILLER PIC X(06) VALUE '037037'.
               10  FILLER PIC X(06) VALUE '038038'.
               10  FILLER PIC X(06) VALUE '039039'.
               10  FILLER PIC X(06) VALUE '040040'.
               10  FILLER PIC X(06) VALUE '041040'.
               10  FILLER PIC X(06) VALUE '042042'.
               10  FILLER PIC X(06) VALUE '043044'.
               10  FILLER PIC X(06) VALUE '044044'.
               10  FILLER PIC X(06) VALUE '045045'.
               10  FILLER PIC X(06) VALUE '046046'.
               10  FILLER PIC X(06) VALUE '047047'.
               10  FILLER PIC X(06) VALUE '048048'.
               10  FILLER PIC X(06) VALUE '049050'.
               10  FILLER PIC X(06) VALUE '050050'.
               10  FILLER PIC X(06) VALUE '051051'.
               10  FILLER PIC X(06) VALUE '052052'.
               10  FILLER PIC X(06) VALUE '053053'.
               10  FILLER PIC X(06) VALUE '054054'.
               10  FILLER PIC X(06) VALUE '055055'.
               10  FILLER PIC X(06) VALUE '063090'.
               10  FILLER PIC X(06) VALUE '064090'.
               10  FILLER PIC X(06) VALUE '070070'.
               10  FILLER PIC X(06) VALUE '071071'.
               10  FILLER PIC X(06) VALUE '072072'.
               10  FILLER PIC X(06) VALUE '073073'.
               10  FILLER PIC X(06) VALUE '074074'.
               10  FILLER PIC X(06) VALUE '075077'.
               10  FILLER PIC X(06) VALUE '076077'.
           05  WS-XWALK-TBL REDEFINES WS-XWALK-DATA.
               10  WS-XWALK-ENTRY OCCURS 40 TIMES
                   INDEXED BY WS-XWALK-IDX.
                   15  WS-XWALK-REV-CD     PIC X(04).
                   15  WS-XWALK-HCPCS-PFX  PIC X(02).

      *-----------------------------------------------------------------
      * BUNDLING/UNBUNDLING EDIT TABLES
      * COMPONENT/COMPREHENSIVE CODE RELATIONSHIPS
      *-----------------------------------------------------------------
       01  WS-BUNDLE-TABLE.
           05  WS-BUNDLE-COUNT             PIC 9(04) VALUE 25.
           05  WS-BUNDLE-DATA.
               10  FILLER PIC X(10) VALUE '9921399214'.
               10  FILLER PIC X(10) VALUE '9921499215'.
               10  FILLER PIC X(10) VALUE '9921599215'.
               10  FILLER PIC X(10) VALUE '3600036415'.
               10  FILLER PIC X(10) VALUE '7105271048'.
               10  FILLER PIC X(10) VALUE '7105271049'.
               10  FILLER PIC X(10) VALUE '7105271050'.
               10  FILLER PIC X(10) VALUE '7104871046'.
               10  FILLER PIC X(10) VALUE '7104971047'.
               10  FILLER PIC X(10) VALUE '7625076519'.
               10  FILLER PIC X(10) VALUE '4316143162'.
               10  FILLER PIC X(10) VALUE '2718027185'.
               10  FILLER PIC X(10) VALUE '2718527186'.
               10  FILLER PIC X(10) VALUE '5024550250'.
               10  FILLER PIC X(10) VALUE '4714447143'.
               10  FILLER PIC X(10) VALUE '4714547144'.
               10  FILLER PIC X(10) VALUE '4714647145'.
               10  FILLER PIC X(10) VALUE '2710027101'.
               10  FILLER PIC X(10) VALUE '2710127102'.
               10  FILLER PIC X(10) VALUE '5835058353'.
               10  FILLER PIC X(10) VALUE '5006050065'.
               10  FILLER PIC X(10) VALUE '5838058381'.
               10  FILLER PIC X(10) VALUE '4716047160'.
               10  FILLER PIC X(10) VALUE '6221262213'.
               10  FILLER PIC X(10) VALUE '6221462215'.
           05  WS-BUNDLE-TBL REDEFINES WS-BUNDLE-DATA.
               10  WS-BUNDLE-ENTRY OCCURS 25 TIMES
                   INDEXED BY WS-BUNDLE-IDX.
                   15  WS-BUNDLE-COMP-CPT   PIC X(05).
                   15  WS-BUNDLE-COMPR-CPT  PIC X(05).

      *-----------------------------------------------------------------
      * ADD-ON CODE TABLE
      * ADD-ON CODES THAT MUST BE BILLED WITH A PRIMARY CODE
      *-----------------------------------------------------------------
       01  WS-ADDON-CODE-TABLE.
           05  WS-ADDON-COUNT              PIC 9(04) VALUE 20.
           05  WS-ADDON-DATA.
      *---     FORMAT: ADD-ON-CPT + PRIMARY-CPT
               10  FILLER PIC X(10) VALUE '2210022103'.
               10  FILLER PIC X(10) VALUE '2211622100'.
               10  FILLER PIC X(10) VALUE '6321263210'.
               10  FILLER PIC X(10) VALUE '6327063268'.
               10  FILLER PIC X(10) VALUE '6329063285'.
               10  FILLER PIC X(10) VALUE '2205522050'.
               10  FILLER PIC X(10) VALUE '2252522524'.
               10  FILLER PIC X(10) VALUE '2783527830'.
               10  FILLER PIC X(10) VALUE '2783627830'.
               10  FILLER PIC X(10) VALUE '3350033475'.
               10  FILLER PIC X(10) VALUE '3360033602'.
               10  FILLER PIC X(10) VALUE '4939149390'.
               10  FILLER PIC X(10) VALUE '5835358350'.
               10  FILLER PIC X(10) VALUE '6430064305'.
               10  FILLER PIC X(10) VALUE '9236792366'.
               10  FILLER PIC X(10) VALUE '9501995018'.
               10  FILLER PIC X(10) VALUE '9503195030'.
               10  FILLER PIC X(10) VALUE '9596195954'.
               10  FILLER PIC X(10) VALUE '9596795967'.
               10  FILLER PIC X(10) VALUE '9946799465'.
           05  WS-ADDON-TBL REDEFINES WS-ADDON-DATA.
               10  WS-ADDON-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-ADDON-IDX.
                   15  WS-ADDON-CPT         PIC X(05).
                   15  WS-ADDON-PRIMARY-CPT PIC X(05).

      *-----------------------------------------------------------------
      * GLOBAL SURGERY PERIOD TABLE
      * FORMAT: CPT + GLOBAL-DAYS (000=0-DAY, 010=10-DAY, 090=90-DAY)
      *-----------------------------------------------------------------
       01  WS-GLOBAL-SURG-TABLE.
           05  WS-GLOBAL-COUNT             PIC 9(04) VALUE 15.
           05  WS-GLOBAL-DATA.
               10  FILLER PIC X(08) VALUE '10060010'.
               10  FILLER PIC X(08) VALUE '10061010'.
               10  FILLER PIC X(08) VALUE '10120010'.
               10  FILLER PIC X(08) VALUE '10121010'.
               10  FILLER PIC X(08) VALUE '10140010'.
               10  FILLER PIC X(08) VALUE '10160010'.
               10  FILLER PIC X(08) VALUE '11042000'.
               10  FILLER PIC X(08) VALUE '19120090'.
               10  FILLER PIC X(08) VALUE '19125090'.
               10  FILLER PIC X(08) VALUE '27447090'.
               10  FILLER PIC X(08) VALUE '27130090'.
               10  FILLER PIC X(08) VALUE '33405090'.
               10  FILLER PIC X(08) VALUE '47562090'.
               10  FILLER PIC X(08) VALUE '49505090'.
               10  FILLER PIC X(08) VALUE '66984090'.
           05  WS-GLOBAL-TBL REDEFINES WS-GLOBAL-DATA.
               10  WS-GLOBAL-ENTRY OCCURS 15 TIMES
                   INDEXED BY WS-GLOBAL-IDX.
                   15  WS-GLOBAL-CPT        PIC X(05).
                   15  WS-GLOBAL-DAYS       PIC 9(03).

      *-----------------------------------------------------------------
      * STATE-SPECIFIC TIMELY FILING TABLES (MEDICAID)
      * DAYS ALLOWED FOR TIMELY FILING BY STATE
      *-----------------------------------------------------------------
       01  WS-STATE-TIMELY-TABLE.
           05  WS-STATE-TF-COUNT           PIC 9(02) VALUE 50.
           05  WS-STATE-TF-DATA.
               10  FILLER PIC X(05) VALUE 'AL365'.
               10  FILLER PIC X(05) VALUE 'AK365'.
               10  FILLER PIC X(05) VALUE 'AZ180'.
               10  FILLER PIC X(05) VALUE 'AR180'.
               10  FILLER PIC X(05) VALUE 'CA180'.
               10  FILLER PIC X(05) VALUE 'CO120'.
               10  FILLER PIC X(05) VALUE 'CT090'.
               10  FILLER PIC X(05) VALUE 'DE180'.
               10  FILLER PIC X(05) VALUE 'FL365'.
               10  FILLER PIC X(05) VALUE 'GA365'.
               10  FILLER PIC X(05) VALUE 'HI365'.
               10  FILLER PIC X(05) VALUE 'ID180'.
               10  FILLER PIC X(05) VALUE 'IL180'.
               10  FILLER PIC X(05) VALUE 'IN180'.
               10  FILLER PIC X(05) VALUE 'IA365'.
               10  FILLER PIC X(05) VALUE 'KS365'.
               10  FILLER PIC X(05) VALUE 'KY180'.
               10  FILLER PIC X(05) VALUE 'LA365'.
               10  FILLER PIC X(05) VALUE 'ME180'.
               10  FILLER PIC X(05) VALUE 'MD180'.
               10  FILLER PIC X(05) VALUE 'MA090'.
               10  FILLER PIC X(05) VALUE 'MI365'.
               10  FILLER PIC X(05) VALUE 'MN365'.
               10  FILLER PIC X(05) VALUE 'MS180'.
               10  FILLER PIC X(05) VALUE 'MO365'.
               10  FILLER PIC X(05) VALUE 'MT180'.
               10  FILLER PIC X(05) VALUE 'NE180'.
               10  FILLER PIC X(05) VALUE 'NV180'.
               10  FILLER PIC X(05) VALUE 'NH120'.
               10  FILLER PIC X(05) VALUE 'NJ180'.
               10  FILLER PIC X(05) VALUE 'NM090'.
               10  FILLER PIC X(05) VALUE 'NY090'.
               10  FILLER PIC X(05) VALUE 'NC180'.
               10  FILLER PIC X(05) VALUE 'ND365'.
               10  FILLER PIC X(05) VALUE 'OH365'.
               10  FILLER PIC X(05) VALUE 'OK365'.
               10  FILLER PIC X(05) VALUE 'OR180'.
               10  FILLER PIC X(05) VALUE 'PA180'.
               10  FILLER PIC X(05) VALUE 'RI180'.
               10  FILLER PIC X(05) VALUE 'SC365'.
               10  FILLER PIC X(05) VALUE 'SD180'.
               10  FILLER PIC X(05) VALUE 'TN365'.
               10  FILLER PIC X(05) VALUE 'TX095'.
               10  FILLER PIC X(05) VALUE 'UT365'.
               10  FILLER PIC X(05) VALUE 'VT180'.
               10  FILLER PIC X(05) VALUE 'VA180'.
               10  FILLER PIC X(05) VALUE 'WA365'.
               10  FILLER PIC X(05) VALUE 'WV365'.
               10  FILLER PIC X(05) VALUE 'WI365'.
               10  FILLER PIC X(05) VALUE 'WY180'.
           05  WS-STATE-TF-TBL REDEFINES WS-STATE-TF-DATA.
               10  WS-STATE-TF-ENTRY OCCURS 50 TIMES
                   INDEXED BY WS-STATE-TF-IDX.
                   15  WS-STATE-TF-CD       PIC X(02).
                   15  WS-STATE-TF-DAYS     PIC 9(03).

      *-----------------------------------------------------------------
      * TELEHEALTH ELIGIBLE PROCEDURE CODE TABLE
      *-----------------------------------------------------------------
       01  WS-TELEHEALTH-TABLE.
           05  WS-TELE-PROC-COUNT          PIC 9(04) VALUE 30.
           05  WS-TELE-PROC-DATA.
               10  FILLER PIC X(05) VALUE '99201'.
               10  FILLER PIC X(05) VALUE '99202'.
               10  FILLER PIC X(05) VALUE '99203'.
               10  FILLER PIC X(05) VALUE '99204'.
               10  FILLER PIC X(05) VALUE '99205'.
               10  FILLER PIC X(05) VALUE '99211'.
               10  FILLER PIC X(05) VALUE '99212'.
               10  FILLER PIC X(05) VALUE '99213'.
               10  FILLER PIC X(05) VALUE '99214'.
               10  FILLER PIC X(05) VALUE '99215'.
               10  FILLER PIC X(05) VALUE '99221'.
               10  FILLER PIC X(05) VALUE '99222'.
               10  FILLER PIC X(05) VALUE '99223'.
               10  FILLER PIC X(05) VALUE '90791'.
               10  FILLER PIC X(05) VALUE '90792'.
               10  FILLER PIC X(05) VALUE '90832'.
               10  FILLER PIC X(05) VALUE '90833'.
               10  FILLER PIC X(05) VALUE '90834'.
               10  FILLER PIC X(05) VALUE '90836'.
               10  FILLER PIC X(05) VALUE '90837'.
               10  FILLER PIC X(05) VALUE '90838'.
               10  FILLER PIC X(05) VALUE '90839'.
               10  FILLER PIC X(05) VALUE '90840'.
               10  FILLER PIC X(05) VALUE '90845'.
               10  FILLER PIC X(05) VALUE '90846'.
               10  FILLER PIC X(05) VALUE '90847'.
               10  FILLER PIC X(05) VALUE '96150'.
               10  FILLER PIC X(05) VALUE '96151'.
               10  FILLER PIC X(05) VALUE '96152'.
               10  FILLER PIC X(05) VALUE '96153'.
           05  WS-TELE-PROC-TBL REDEFINES WS-TELE-PROC-DATA.
               10  WS-TELE-PROC-ENTRY OCCURS 30 TIMES
                   INDEXED BY WS-TELE-IDX.
                   15  WS-TELE-CPT-CD       PIC X(05).

      *-----------------------------------------------------------------
      * PREVENTIVE CARE PROCEDURE TABLE (ACA MANDATE)
      *-----------------------------------------------------------------
       01  WS-PREVENTIVE-TABLE.
           05  WS-PREV-PROC-COUNT          PIC 9(03) VALUE 25.
           05  WS-PREV-PROC-DATA.
               10  FILLER PIC X(06) VALUE '99381P'.
               10  FILLER PIC X(06) VALUE '99382P'.
               10  FILLER PIC X(06) VALUE '99383P'.
               10  FILLER PIC X(06) VALUE '99384P'.
               10  FILLER PIC X(06) VALUE '99385P'.
               10  FILLER PIC X(06) VALUE '99386P'.
               10  FILLER PIC X(06) VALUE '99387P'.
               10  FILLER PIC X(06) VALUE '99391P'.
               10  FILLER PIC X(06) VALUE '99392P'.
               10  FILLER PIC X(06) VALUE '99393P'.
               10  FILLER PIC X(06) VALUE '99394P'.
               10  FILLER PIC X(06) VALUE '99395P'.
               10  FILLER PIC X(06) VALUE '99396P'.
               10  FILLER PIC X(06) VALUE '99397P'.
               10  FILLER PIC X(06) VALUE '77067S'.
               10  FILLER PIC X(06) VALUE '77063S'.
               10  FILLER PIC X(06) VALUE 'G0101W'.
               10  FILLER PIC X(06) VALUE 'G0123W'.
               10  FILLER PIC X(06) VALUE 'G0124W'.
               10  FILLER PIC X(06) VALUE 'G0104S'.
               10  FILLER PIC X(06) VALUE 'G0105S'.
               10  FILLER PIC X(06) VALUE 'G0121S'.
               10  FILLER PIC X(06) VALUE 'G0328S'.
               10  FILLER PIC X(06) VALUE '96040C'.
               10  FILLER PIC X(06) VALUE 'S0610C'.
           05  WS-PREV-PROC-TBL REDEFINES WS-PREV-PROC-DATA.
               10  WS-PREV-PROC-ENTRY OCCURS 25 TIMES
                   INDEXED BY WS-PREV-IDX.
                   15  WS-PREV-CPT-CD       PIC X(05).
                   15  WS-PREV-TYPE         PIC X(01).
      *---             P=PREVENTIVE VISIT, S=SCREENING,
      *---             W=WELL-WOMAN, C=CONTRACEPTIVE/COUNSELING

      *-----------------------------------------------------------------
      * MENTAL HEALTH / SUBSTANCE ABUSE DIAGNOSIS RANGES
      *-----------------------------------------------------------------
       01  WS-MH-DIAG-TABLE.
           05  WS-MH-RANGE-COUNT           PIC 9(02) VALUE 08.
           05  WS-MH-RANGE-DATA.
               10  FILLER PIC X(06) VALUE 'F01F09'.
               10  FILLER PIC X(06) VALUE 'F10F19'.
               10  FILLER PIC X(06) VALUE 'F20F29'.
               10  FILLER PIC X(06) VALUE 'F30F39'.
               10  FILLER PIC X(06) VALUE 'F40F48'.
               10  FILLER PIC X(06) VALUE 'F50F59'.
               10  FILLER PIC X(06) VALUE 'F60F69'.
               10  FILLER PIC X(06) VALUE 'F70F79'.
           05  WS-MH-RANGE-TBL REDEFINES WS-MH-RANGE-DATA.
               10  WS-MH-RANGE-ENTRY OCCURS 8 TIMES
                   INDEXED BY WS-MH-IDX.
                   15  WS-MH-RANGE-FROM     PIC X(03).
                   15  WS-MH-RANGE-TO       PIC X(03).

      *-----------------------------------------------------------------
      * CONDITION CODE TABLE (UB-04) - VALID VALUES
      *-----------------------------------------------------------------
       01  WS-COND-CODE-TABLE.
           05  WS-COND-CODE-COUNT          PIC 9(03) VALUE 48.
           05  WS-COND-CODE-DATA.
               10  FILLER PIC X(32) VALUE '01MILITARY SERVICE RELATED      '.
               10  FILLER PIC X(32) VALUE '02CONDITION IS EMPLOYMENT RELATE'.
               10  FILLER PIC X(32) VALUE '03PATIENT COVERED BY INSURANCE  '.
               10  FILLER PIC X(32) VALUE '04HMO ENROLLEE                  '.
               10  FILLER PIC X(32) VALUE '05LIEN HAS BEEN FILED           '.
               10  FILLER PIC X(32) VALUE '06ESRD PATIENT IN FIRST 18 MOS  '.
               10  FILLER PIC X(32) VALUE '07TX OF NONTERMINAL CONDITION   '.
               10  FILLER PIC X(32) VALUE '08BENEFICIARY WOULD NOT PROVIDE '.
               10  FILLER PIC X(32) VALUE '09NEITHER PAT NOR SPOUSE EMPLOY '.
               10  FILLER PIC X(32) VALUE '10PAT AND/OR SPOUSE IS EMPLOYED '.
               10  FILLER PIC X(32) VALUE '11DISABLED BENEFICIARY/FAMILY ME'.
               10  FILLER PIC X(32) VALUE '12PAYER CODES                   '.
               10  FILLER PIC X(32) VALUE '13WORKERS COMP                  '.
               10  FILLER PIC X(32) VALUE '14GHP 100 OR MORE EMPLOYEES     '.
               10  FILLER PIC X(32) VALUE '15GHP FEWER THAN 100 EMPLOYEES  '.
               10  FILLER PIC X(32) VALUE '16SNF TRANSITION ADMIT NOT BILLE'.
               10  FILLER PIC X(32) VALUE '17PAT IS HOMELESS               '.
               10  FILLER PIC X(32) VALUE '18MAIDEN NAME RETAINED          '.
               10  FILLER PIC X(32) VALUE '19CHILD RETAINS MOTHERS NAME    '.
               10  FILLER PIC X(32) VALUE '20BENEFICIARY REQUESTED BILLING '.
               10  FILLER PIC X(32) VALUE '21BILLING FOR DENIAL NOTICE     '.
               10  FILLER PIC X(32) VALUE '26VA ELIGIBLE PATIENT CHOOSES V '.
               10  FILLER PIC X(32) VALUE '27PAT REFERRED TO SOLE COMMUNITY'.
               10  FILLER PIC X(32) VALUE '28PAT AND/OR SPOUSE GHP 20+ EMP '.
               10  FILLER PIC X(32) VALUE '29DISABLED BENEF/FAMILY MEMBER L'.
               10  FILLER PIC X(32) VALUE '30NON-DUAL ELIG MEDICARE BENEF  '.
               10  FILLER PIC X(32) VALUE '31STUDENT FULL-TIME DAY         '.
               10  FILLER PIC X(32) VALUE '32STUDENT COOPERATIVE/WORK STUDY'.
               10  FILLER PIC X(32) VALUE '33STUDENT FULL-TIME NIGHT       '.
               10  FILLER PIC X(32) VALUE '34STUDENT PART-TIME             '.
               10  FILLER PIC X(32) VALUE '36GENERAL CARE PATIENT IN SPECIA'.
               10  FILLER PIC X(32) VALUE '37WARD ACCOMMODATION            '.
               10  FILLER PIC X(32) VALUE '38SEMI-PRIVATE ROOM NOT AVAILABL'.
               10  FILLER PIC X(32) VALUE '39PRIVATE ROOM MEDICALLY NECESS '.
               10  FILLER PIC X(32) VALUE '40SAME DAY TRANSFER             '.
               10  FILLER PIC X(32) VALUE '41PARTIAL HOSPITALIZATION        '.
               10  FILLER PIC X(32) VALUE '42CONTINUING CARE NOT RELATED   '.
               10  FILLER PIC X(32) VALUE '43CONTINUING CARE NOT PROVIDED  '.
               10  FILLER PIC X(32) VALUE '44INPATIENT ADMISSION CHANGED TO'.
               10  FILLER PIC X(32) VALUE '46NON-PAYMENT CODE              '.
               10  FILLER PIC X(32) VALUE '48PSYCHIATRIC RESIDENTIAL TX     '.
               10  FILLER PIC X(32) VALUE '55SNF BED NOT AVAILABLE         '.
               10  FILLER PIC X(32) VALUE '56MEDICAL APPROPRIATENESS       '.
               10  FILLER PIC X(32) VALUE '57SNF READMISSION               '.
               10  FILLER PIC X(32) VALUE '58TERMINATED TRANSFER            '.
               10  FILLER PIC X(32) VALUE '59NON-PRIMARY ESRD FACILITY     '.
               10  FILLER PIC X(32) VALUE '60DAY OUTLIER                   '.
               10  FILLER PIC X(32) VALUE '61COST OUTLIER                  '.
               10  FILLER PIC X(32) VALUE '67BENEFICIARY ELECTS NOT TO USE '.
           05  WS-COND-CODE-TBL REDEFINES WS-COND-CODE-DATA.
               10  WS-COND-CODE-ENTRY OCCURS 48 TIMES
                   INDEXED BY WS-COND-IDX.
                   15  WS-COND-CD-VALUE     PIC X(02).
                   15  WS-COND-CD-DESC      PIC X(30).

      *-----------------------------------------------------------------
      * OCCURRENCE CODE TABLE (UB-04)
      *-----------------------------------------------------------------
       01  WS-OCCUR-CODE-TABLE.
           05  WS-OCCUR-CODE-COUNT         PIC 9(03) VALUE 20.
           05  WS-OCCUR-CODE-DATA.
               10  FILLER PIC X(02) VALUE '01'.
               10  FILLER PIC X(02) VALUE '02'.
               10  FILLER PIC X(02) VALUE '03'.
               10  FILLER PIC X(02) VALUE '04'.
               10  FILLER PIC X(02) VALUE '05'.
               10  FILLER PIC X(02) VALUE '06'.
               10  FILLER PIC X(02) VALUE '09'.
               10  FILLER PIC X(02) VALUE '10'.
               10  FILLER PIC X(02) VALUE '11'.
               10  FILLER PIC X(02) VALUE '12'.
               10  FILLER PIC X(02) VALUE '17'.
               10  FILLER PIC X(02) VALUE '18'.
               10  FILLER PIC X(02) VALUE '19'.
               10  FILLER PIC X(02) VALUE '20'.
               10  FILLER PIC X(02) VALUE '21'.
               10  FILLER PIC X(02) VALUE '22'.
               10  FILLER PIC X(02) VALUE '24'.
               10  FILLER PIC X(02) VALUE '25'.
               10  FILLER PIC X(02) VALUE '27'.
               10  FILLER PIC X(02) VALUE '28'.
           05  WS-OCCUR-CODE-TBL REDEFINES WS-OCCUR-CODE-DATA.
               10  WS-OCCUR-CODE-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-OCCUR-IDX.
                   15  WS-OCCUR-CD-VALUE    PIC X(02).

      *-----------------------------------------------------------------
      * VALUE CODE TABLE (UB-04)
      *-----------------------------------------------------------------
       01  WS-VALUE-CODE-TABLE.
           05  WS-VALUE-CODE-COUNT         PIC 9(03) VALUE 15.
           05  WS-VALUE-CODE-DATA.
               10  FILLER PIC X(02) VALUE '01'.
               10  FILLER PIC X(02) VALUE '02'.
               10  FILLER PIC X(02) VALUE '04'.
               10  FILLER PIC X(02) VALUE '05'.
               10  FILLER PIC X(02) VALUE '06'.
               10  FILLER PIC X(02) VALUE '08'.
               10  FILLER PIC X(02) VALUE '09'.
               10  FILLER PIC X(02) VALUE '10'.
               10  FILLER PIC X(02) VALUE '11'.
               10  FILLER PIC X(02) VALUE '12'.
               10  FILLER PIC X(02) VALUE '14'.
               10  FILLER PIC X(02) VALUE '15'.
               10  FILLER PIC X(02) VALUE '16'.
               10  FILLER PIC X(02) VALUE '17'.
               10  FILLER PIC X(02) VALUE '80'.
           05  WS-VALUE-CODE-TBL REDEFINES WS-VALUE-CODE-DATA.
               10  WS-VALUE-CODE-ENTRY OCCURS 15 TIMES
                   INDEXED BY WS-VALUE-IDX.
                   15  WS-VALUE-CD-VALUE    PIC X(02).

      *-----------------------------------------------------------------
      * DISCHARGE STATUS CODE TABLE
      *-----------------------------------------------------------------
       01  WS-DISCHARGE-STATUS-TABLE.
           05  WS-DISCH-CODE-COUNT         PIC 9(03) VALUE 30.
           05  WS-DISCH-CODE-DATA.
               10  FILLER PIC X(02) VALUE '01'.
               10  FILLER PIC X(02) VALUE '02'.
               10  FILLER PIC X(02) VALUE '03'.
               10  FILLER PIC X(02) VALUE '04'.
               10  FILLER PIC X(02) VALUE '05'.
               10  FILLER PIC X(02) VALUE '06'.
               10  FILLER PIC X(02) VALUE '07'.
               10  FILLER PIC X(02) VALUE '08'.
               10  FILLER PIC X(02) VALUE '09'.
               10  FILLER PIC X(02) VALUE '20'.
               10  FILLER PIC X(02) VALUE '21'.
               10  FILLER PIC X(02) VALUE '30'.
               10  FILLER PIC X(02) VALUE '40'.
               10  FILLER PIC X(02) VALUE '41'.
               10  FILLER PIC X(02) VALUE '42'.
               10  FILLER PIC X(02) VALUE '43'.
               10  FILLER PIC X(02) VALUE '50'.
               10  FILLER PIC X(02) VALUE '51'.
               10  FILLER PIC X(02) VALUE '61'.
               10  FILLER PIC X(02) VALUE '62'.
               10  FILLER PIC X(02) VALUE '63'.
               10  FILLER PIC X(02) VALUE '64'.
               10  FILLER PIC X(02) VALUE '65'.
               10  FILLER PIC X(02) VALUE '66'.
               10  FILLER PIC X(02) VALUE '69'.
               10  FILLER PIC X(02) VALUE '70'.
               10  FILLER PIC X(02) VALUE '71'.
               10  FILLER PIC X(02) VALUE '72'.
               10  FILLER PIC X(02) VALUE '81'.
               10  FILLER PIC X(02) VALUE '82'.
           05  WS-DISCH-CODE-TBL REDEFINES WS-DISCH-CODE-DATA.
               10  WS-DISCH-CODE-ENTRY OCCURS 30 TIMES
                   INDEXED BY WS-DISCH-IDX.
                   15  WS-DISCH-CD-VALUE    PIC X(02).

       01  WS-EDIT-ERROR-TABLE.
           05  WS-EDIT-ERR-COUNT           PIC 9(03) VALUE 0.
           05  WS-EDIT-ERR-MAX             PIC 9(03) VALUE 50.
           05  WS-EDIT-ERR-ENTRY OCCURS 50 TIMES.
               10  WS-EDIT-ERR-CD          PIC X(06).
               10  WS-EDIT-ERR-SEV         PIC X(01).
               10  WS-EDIT-ERR-MSG         PIC X(80).
               10  WS-EDIT-ERR-FIELD       PIC X(30).
               10  WS-EDIT-ERR-VALUE       PIC X(30).
               10  WS-EDIT-ERR-LINE-NO     PIC 9(03).

       01  WS-REJECT-RECORD.
           05  WS-REJ-CLAIM-DATA          PIC X(4096).
           05  WS-REJ-ERROR-COUNT         PIC 9(03).
           05  WS-REJ-PRIMARY-ERR-CD      PIC X(06).
           05  WS-REJ-ERROR-CODES         PIC X(60).
           05  WS-REJ-TIMESTAMP           PIC X(26).
           05  WS-REJ-FILLER              PIC X(09).

       01  WS-RPT-HEADER-1.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(50)
               VALUE 'HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)'.
           05  FILLER                      PIC X(30) VALUE SPACES.
           05  FILLER                      PIC X(06) VALUE 'DATE: '.
           05  WS-RPT-DATE                 PIC X(10).
           05  FILLER                      PIC X(36) VALUE SPACES.

       01  WS-RPT-HEADER-2.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  FILLER                      PIC X(50)
               VALUE 'CLAIMS VALIDATION EDIT CHECK REPORT'.
           05  FILLER                      PIC X(30) VALUE SPACES.
           05  FILLER                      PIC X(06) VALUE 'PAGE: '.
           05  WS-RPT-PAGE-NO             PIC Z,ZZ9.
           05  FILLER                      PIC X(36) VALUE SPACES.

       01  WS-RPT-DETAIL-LINE.
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-CLM-NO              PIC X(15).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-EDIT-CD             PIC X(06).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-SEVERITY            PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-EDIT-MSG            PIC X(80).
           05  FILLER                      PIC X(01) VALUE SPACES.
           05  WS-RPT-FIELD-NAME          PIC X(25).
           05  FILLER                      PIC X(01) VALUE SPACES.

       01  WS-NPI-VALIDATION.
           05  WS-NPI-WORK                PIC X(10).
           05  WS-NPI-CHECK-DIGIT         PIC 9(01).
           05  WS-NPI-SUM                 PIC 9(05).
           05  WS-NPI-DOUBLED             PIC 9(02).
           05  WS-NPI-REMAINDER           PIC 9(02).
           05  WS-NPI-VALID-FLAG          PIC X(01).
               88  WS-NPI-IS-VALID        VALUE 'Y'.
               88  WS-NPI-INVALID         VALUE 'N'.

       01  WS-ICD10-VALIDATION.
           05  WS-ICD-CODE-WORK           PIC X(08).
           05  WS-ICD-ALPHA-PORTION       PIC X(01).
           05  WS-ICD-NUMERIC-PORTION     PIC X(06).
           05  WS-ICD-VALID-FLAG          PIC X(01).
               88  WS-ICD-VALID           VALUE 'Y'.
               88  WS-ICD-INVALID         VALUE 'N'.

       01  WS-OPIOID-EDIT-FIELDS.
           05  WS-OPIOID-MME-TOTAL        PIC S9(05)V99 COMP-3.
           05  WS-OPIOID-MME-THRESHOLD    PIC S9(05)V99 COMP-3
                                          VALUE +90.00.
           05  WS-OPIOID-DAYS-SUPPLY      PIC 9(03).
           05  WS-OPIOID-FLAG             PIC X(01).
               88  WS-IS-OPIOID-RX        VALUE 'Y'.
           05  WS-OPIOID-CONCURRENT       PIC X(01).
               88  WS-CONCURRENT-OPIOID   VALUE 'Y'.

       01  WS-COVID-FIELDS.
           05  WS-COVID-RELATED-FLAG      PIC X(01).
               88  WS-IS-COVID-RELATED    VALUE 'Y'.
           05  WS-COVID-DIAG-FOUND        PIC X(01).
               88  WS-HAS-COVID-DIAG      VALUE 'Y'.
           05  WS-COVID-VACCINE-CPT       PIC X(01).
               88  WS-IS-COVID-VACCINE    VALUE 'Y'.
           05  WS-COVID-TEST-CPT          PIC X(01).
               88  WS-IS-COVID-TEST       VALUE 'Y'.

      *-----------------------------------------------------------------
      * STATISTICS ACCUMULATORS BY PAYER TYPE
      *-----------------------------------------------------------------
       01  WS-PAYER-STATISTICS.
           05  WS-PAYER-STAT-ENTRY OCCURS 10 TIMES.
               10  WS-PSTAT-PAYER-TYPE     PIC X(02).
               10  WS-PSTAT-CLAIMS-READ    PIC 9(07) VALUE 0.
               10  WS-PSTAT-CLAIMS-PASS    PIC 9(07) VALUE 0.
               10  WS-PSTAT-CLAIMS-FAIL    PIC 9(07) VALUE 0.
               10  WS-PSTAT-TOT-CHARGES    PIC S9(11)V99 COMP-3
                                           VALUE +0.
               10  WS-PSTAT-TOT-DENIED     PIC S9(11)V99 COMP-3
                                           VALUE +0.

      *-----------------------------------------------------------------
      * STATISTICS ACCUMULATORS BY CLAIM TYPE
      *-----------------------------------------------------------------
       01  WS-CLMTYPE-STATISTICS.
           05  WS-CSTAT-INST-READ          PIC 9(07) VALUE 0.
           05  WS-CSTAT-INST-PASS          PIC 9(07) VALUE 0.
           05  WS-CSTAT-INST-FAIL          PIC 9(07) VALUE 0.
           05  WS-CSTAT-PROF-READ          PIC 9(07) VALUE 0.
           05  WS-CSTAT-PROF-PASS          PIC 9(07) VALUE 0.
           05  WS-CSTAT-PROF-FAIL          PIC 9(07) VALUE 0.
           05  WS-CSTAT-DENT-READ          PIC 9(07) VALUE 0.
           05  WS-CSTAT-DENT-PASS          PIC 9(07) VALUE 0.
           05  WS-CSTAT-DENT-FAIL          PIC 9(07) VALUE 0.
           05  WS-CSTAT-PHARM-READ         PIC 9(07) VALUE 0.
           05  WS-CSTAT-PHARM-PASS         PIC 9(07) VALUE 0.
           05  WS-CSTAT-PHARM-FAIL         PIC 9(07) VALUE 0.

      *-----------------------------------------------------------------
      * STATISTICS ACCUMULATORS BY ERROR CATEGORY
      *-----------------------------------------------------------------
       01  WS-ERROR-CATEGORY-STATS.
           05  WS-ESTAT-HEADER-ERRS        PIC 9(07) VALUE 0.
           05  WS-ESTAT-PATIENT-ERRS       PIC 9(07) VALUE 0.
           05  WS-ESTAT-PROVIDER-ERRS      PIC 9(07) VALUE 0.
           05  WS-ESTAT-DATE-ERRS          PIC 9(07) VALUE 0.
           05  WS-ESTAT-DIAG-ERRS          PIC 9(07) VALUE 0.
           05  WS-ESTAT-PROC-ERRS          PIC 9(07) VALUE 0.
           05  WS-ESTAT-FINANCIAL-ERRS     PIC 9(07) VALUE 0.
           05  WS-ESTAT-ELIG-ERRS          PIC 9(07) VALUE 0.
           05  WS-ESTAT-DUP-ERRS           PIC 9(07) VALUE 0.
           05  WS-ESTAT-TIMELY-ERRS        PIC 9(07) VALUE 0.
           05  WS-ESTAT-AUTH-ERRS          PIC 9(07) VALUE 0.
           05  WS-ESTAT-COB-ERRS           PIC 9(07) VALUE 0.
           05  WS-ESTAT-NCCI-ERRS          PIC 9(07) VALUE 0.
           05  WS-ESTAT-MUE-ERRS           PIC 9(07) VALUE 0.
           05  WS-ESTAT-GENDER-AGE-ERRS    PIC 9(07) VALUE 0.
           05  WS-ESTAT-MODIFIER-ERRS      PIC 9(07) VALUE 0.
           05  WS-ESTAT-POS-ERRS           PIC 9(07) VALUE 0.
           05  WS-ESTAT-TELEHLTH-ERRS      PIC 9(07) VALUE 0.
           05  WS-ESTAT-PARITY-ERRS        PIC 9(07) VALUE 0.
           05  WS-ESTAT-PREVENT-ERRS       PIC 9(07) VALUE 0.
           05  WS-ESTAT-NSA-ERRS           PIC 9(07) VALUE 0.
           05  WS-ESTAT-COVID-ERRS         PIC 9(07) VALUE 0.
           05  WS-ESTAT-OPIOID-ERRS        PIC 9(07) VALUE 0.
           05  WS-ESTAT-XOVER-ERRS         PIC 9(07) VALUE 0.

      *-----------------------------------------------------------------
      * AUTHORIZATION DETAIL WORK FIELDS
      *-----------------------------------------------------------------
       01  WS-AUTH-DETAIL-FIELDS.
           05  WS-AUTH-NUMBER-WK           PIC X(20).
           05  WS-AUTH-STATUS-WK           PIC X(02).
           05  WS-AUTH-EFF-DT-DB           PIC 9(08).
           05  WS-AUTH-TERM-DT-DB          PIC 9(08).
           05  WS-AUTH-UNITS-APPROVED      PIC 9(05).
           05  WS-AUTH-UNITS-USED          PIC 9(05).
           05  WS-AUTH-UNITS-REMAINING     PIC 9(05).
           05  WS-AUTH-PROC-CD-DB          PIC X(05).
           05  WS-AUTH-DIAG-CD-DB          PIC X(08).
           05  WS-AUTH-PROV-NPI-DB         PIC X(10).
           05  WS-AUTH-TYPE-WK             PIC X(01).
               88  WS-AUTH-PROSPECTIVE     VALUE 'P'.
               88  WS-AUTH-CONCURRENT      VALUE 'C'.
               88  WS-AUTH-RETROSPECTIVE   VALUE 'R'.
           05  WS-AUTH-REVIEW-REQ          PIC X(01).
               88  WS-CONCURRENT-REVIEW    VALUE 'Y'.
           05  WS-AUTH-EXTENSION-FLAG      PIC X(01).
               88  WS-AUTH-HAS-EXTENSION   VALUE 'Y'.

      *-----------------------------------------------------------------
      * COB / CROSSOVER WORK FIELDS
      *-----------------------------------------------------------------
       01  WS-COB-DETAIL-FIELDS.
           05  WS-COB-PRIMARY-PAYER        PIC X(08).
           05  WS-COB-PRIMARY-PAID         PIC S9(09)V99 COMP-3.
           05  WS-COB-PAT-RESP-AMT         PIC S9(09)V99 COMP-3.
           05  WS-COB-ADJUSTMENT-AMT       PIC S9(09)V99 COMP-3.
           05  WS-COB-ADJREASON-CD         PIC X(05).
           05  WS-COB-XOVER-IND            PIC X(01).
               88  WS-COB-AUTO-XOVER       VALUE 'A'.
               88  WS-COB-MANUAL-XOVER     VALUE 'M'.
           05  WS-COB-MCARE-MCAID-IND      PIC X(01).
               88  WS-COB-MCARE-TO-MCAID   VALUE 'Y'.
           05  WS-COB-MCARE-SUPP-IND       PIC X(01).
               88  WS-COB-MCARE-TO-SUPP    VALUE 'Y'.

       01  WS-WORK-FIELDS.
           05  WS-SUB-1                    PIC 9(03).
           05  WS-SUB-2                    PIC 9(03).
           05  WS-SUB-3                    PIC 9(03).
           05  WS-SUB-4                    PIC 9(03).
           05  WS-SUB-5                    PIC 9(03).
           05  WS-WORK-AMOUNT              PIC S9(09)V99 COMP-3.
           05  WS-WORK-STRING              PIC X(256).
           05  WS-PAGE-COUNT               PIC 9(05) VALUE 0.
           05  WS-LINE-COUNT               PIC 9(03) VALUE 99.
           05  WS-LINES-PER-PAGE           PIC 9(03) VALUE 55.
           05  WS-TOTAL-CHARGES-CALC       PIC S9(09)V99 COMP-3.
           05  WS-LINE-CTR                 PIC 9(03).
           05  WS-LINE-CTR-2               PIC 9(03).
           05  WS-FOUND-FLAG               PIC X(01).
               88  WS-WK-FOUND             VALUE 'Y'.
               88  WS-WK-NOT-FOUND         VALUE 'N'.
           05  WS-BILL-TYPE-DIGIT-1        PIC X(01).
           05  WS-BILL-TYPE-DIGIT-2        PIC X(01).
           05  WS-BILL-TYPE-DIGIT-3        PIC X(01).
           05  WS-BILL-TYPE-DIGIT-4        PIC X(01).
           05  WS-TOTAL-LINE-UNITS         PIC 9(05).
           05  WS-PAYER-TYPE-IDX           PIC 9(02).
           05  WS-PREV-SURG-CPT           PIC X(05).
           05  WS-PREV-SURG-DOS           PIC 9(08).
           05  WS-PREV-SURG-GLOBAL        PIC 9(03).
           05  WS-STATE-CD-WORK            PIC X(02).
           05  WS-WORK-DIAG-3             PIC X(03).

      *-----------------------------------------------------------------
      * INCLUDE COMMON COPYBOOKS
      *-----------------------------------------------------------------
           COPY CPYCLMHD.
           COPY CPYCLMLN.
           COPY CPYPATIN.
           COPY CPYPROVD.
           COPY CPYELIG.
           COPY CPYSQLCA.
           COPY CPYERROR.

       01  WS-CLAIM-LINE-TABLE.
           05  WS-CLT-LINE-COUNT           PIC 9(03) VALUE 0.
           05  WS-CLT-ENTRY OCCURS 999 TIMES.
               10  WS-CLT-LINE-DATA        PIC X(500).

       PROCEDURE DIVISION.

       0000-MAIN-CONTROL.
      *================================================================*
      * MAIN CONTROL PARAGRAPH - ORCHESTRATES CLAIM VALIDATION         *
      * THIS PROGRAM HAS BEEN THE PRIMARY FRONT-END EDIT ENGINE        *
      * SINCE 1994 - ALL CLAIMS MUST PASS THROUGH THIS PROGRAM         *
      * BEFORE ENTERING THE ADJUDICATION PIPELINE.                     *
      *================================================================*
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-CLAIMS
               UNTIL WS-END-OF-FILE
           PERFORM 9000-TERMINATION
           STOP RUN
           .

       1000-INITIALIZATION.
      *================================================================*
      * INITIALIZE PROGRAM - OPEN FILES, CONNECT TO DB, SET DATES      *
      * LOAD NCCI EDIT TABLES INTO MEMORY FOR PERFORMANCE               *
      *================================================================*
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP
           MOVE WS-CURRENT-TIMESTAMP(1:4) TO WS-CURR-YEAR
           MOVE WS-CURRENT-TIMESTAMP(5:2) TO WS-CURR-MONTH
           MOVE WS-CURRENT-TIMESTAMP(7:2) TO WS-CURR-DAY

           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE
               INTO WS-CURRENT-DATE-8
           END-STRING

           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
               DELIMITED BY SIZE
               INTO WS-RPT-DATE
           END-STRING

           MOVE WS-CURRENT-TIMESTAMP TO WS-STAT-START-TIME

           PERFORM 1100-OPEN-FILES
           PERFORM 1200-CONNECT-DATABASE
           PERFORM 1250-LOAD-NCCI-TABLES
           PERFORM 1260-LOAD-MUE-TABLES
           PERFORM 1270-LOAD-LCD-NCD-TABLES
           PERFORM 1300-WRITE-REPORT-HEADERS
           PERFORM 1400-READ-CLAIM-INPUT

           DISPLAY 'HCCLMVAL - INITIALIZATION COMPLETE'
           DISPLAY 'HCCLMVAL - PROCESSING DATE: ' WS-CURRENT-DATE-8
           DISPLAY 'HCCLMVAL - NCCI EDITS LOADED: '
               WS-NCCI-ENTRY-COUNT
           DISPLAY 'HCCLMVAL - MUE ENTRIES LOADED: '
               WS-MUE-ENTRY-COUNT
           DISPLAY 'HCCLMVAL - LCD/NCD ENTRIES LOADED: '
               WS-LCD-ENTRY-COUNT
           .

       1100-OPEN-FILES.
      *----------------------------------------------------------------*
      * OPEN ALL INPUT AND OUTPUT FILES                                *
      *----------------------------------------------------------------*
           OPEN INPUT  CLAIM-INPUT-FILE
           IF WS-CLMIN-STATUS NOT = '00'
               MOVE 'CLAIM-INPUT-FILE OPEN FAILED'
                   TO WS-ERR-MESSAGE
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           OPEN OUTPUT VALID-CLAIM-FILE
           IF WS-CLMVL-STATUS NOT = '00'
               MOVE 'VALID-CLAIM-FILE OPEN FAILED'
                   TO WS-ERR-MESSAGE
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           OPEN OUTPUT REJECT-CLAIM-FILE
           IF WS-CLMRJ-STATUS NOT = '00'
               MOVE 'REJECT-CLAIM-FILE OPEN FAILED'
                   TO WS-ERR-MESSAGE
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           OPEN OUTPUT ERROR-REPORT-FILE
           IF WS-ERRRP-STATUS NOT = '00'
               MOVE 'ERROR-REPORT-FILE OPEN FAILED'
                   TO WS-ERR-MESSAGE
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           OPEN INPUT EDIT-RULES-FILE
           IF WS-EDRUL-STATUS NOT = '00'
               MOVE 'EDIT-RULES-FILE OPEN FAILED'
                   TO WS-ERR-MESSAGE
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           OPEN INPUT NCCI-EDIT-FILE
           IF WS-NCCI-STATUS NOT = '00'
               DISPLAY 'HCCLMVAL - WARNING: NCCI FILE OPEN FAILED'
               DISPLAY 'HCCLMVAL - NCCI EDITS WILL USE DB ONLY'
           END-IF

           OPEN INPUT MUE-THRESHOLD-FILE
           IF WS-MUE-STATUS NOT = '00'
               DISPLAY 'HCCLMVAL - WARNING: MUE FILE OPEN FAILED'
               DISPLAY 'HCCLMVAL - MUE EDITS WILL USE DB ONLY'
           END-IF

           OPEN INPUT LCD-NCD-FILE
           IF WS-LCD-STATUS NOT = '00'
               DISPLAY 'HCCLMVAL - WARNING: LCD/NCD FILE OPEN FAILED'
           END-IF

           OPEN OUTPUT CROSSOVER-OUTPUT-FILE
           IF WS-XOVR-STATUS NOT = '00'
               DISPLAY 'HCCLMVAL - WARNING: XOVER FILE OPEN FAILED'
           END-IF
           .

       1200-CONNECT-DATABASE.
      *----------------------------------------------------------------*
      * ESTABLISH SYBASE DATABASE CONNECTION FOR LOOKUP TABLES         *
      *----------------------------------------------------------------*
           MOVE WS-SYB-SERVER   TO WS-DB-SERVER-NAME
           MOVE WS-SYB-DATABASE TO WS-DB-DATABASE-NAME
           MOVE WS-SYB-USER     TO WS-DB-USER-ID

           EXEC SQL
               CONNECT TO :WS-SYB-SERVER
               USER :WS-SYB-USER
               USING :WS-SYB-PASSWORD
           END-EXEC

           IF WS-SQLCODE NOT = 0
               STRING 'DATABASE CONNECT FAILED - SQLCODE: '
                   WS-SQLCODE
                   DELIMITED BY SIZE
                   INTO WS-ERR-MESSAGE
               END-STRING
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           SET WS-DB-CONNECTED TO TRUE
           MOVE 'C' TO WS-DB-CONN-STATUS

           EXEC SQL
               USE :WS-SYB-DATABASE
           END-EXEC

           DISPLAY 'HCCLMVAL - DATABASE CONNECTION ESTABLISHED'
           .

       1250-LOAD-NCCI-TABLES.
      *----------------------------------------------------------------*
      * LOAD NCCI COLUMN 1/COLUMN 2 EDIT PAIRS INTO MEMORY            *
      * FOR HIGH-PERFORMANCE LOOKUPS DURING CLAIM PROCESSING           *
      * TABLE IS REFRESHED QUARTERLY BY CMS                           *
      *----------------------------------------------------------------*
           MOVE 0 TO WS-NCCI-ENTRY-COUNT

           IF WS-NCCI-STATUS = '00'
      *---     LOAD FROM FLAT FILE
               PERFORM UNTIL WS-NCCI-STATUS NOT = '00'
                   OR WS-NCCI-ENTRY-COUNT >= 5000
                   READ NCCI-EDIT-FILE
                       AT END
                           MOVE '10' TO WS-NCCI-STATUS
                       NOT AT END
                           ADD 1 TO WS-NCCI-ENTRY-COUNT
                           MOVE NE-COL1-CPT TO
                               WS-NCCI-COL1-CPT(
                                   WS-NCCI-ENTRY-COUNT)
                           MOVE NE-COL2-CPT TO
                               WS-NCCI-COL2-CPT(
                                   WS-NCCI-ENTRY-COUNT)
                           MOVE NE-EDIT-TYPE TO
                               WS-NCCI-EDIT-TYPE(
                                   WS-NCCI-ENTRY-COUNT)
                           MOVE NE-MODIFIER-IND TO
                               WS-NCCI-MOD-IND(
                                   WS-NCCI-ENTRY-COUNT)
                           MOVE NE-EFF-DT TO
                               WS-NCCI-EFF-DT(
                                   WS-NCCI-ENTRY-COUNT)
                           MOVE NE-TERM-DT TO
                               WS-NCCI-TERM-DT(
                                   WS-NCCI-ENTRY-COUNT)
                   END-READ
               END-PERFORM
           ELSE
      *---     FALL BACK TO DATABASE LOAD
               EXEC SQL
                   DECLARE NCCI_CURSOR CURSOR FOR
                   SELECT NCCI_COL1_CPT, NCCI_COL2_CPT,
                          NCCI_EDIT_TYPE, NCCI_MOD_IND,
                          NCCI_EFF_DT, NCCI_TERM_DT
                   FROM   HCPS_CLAIMS_DB..NCCI_EDIT_PAIRS
                   WHERE  NCCI_TERM_DT >= :WS-CURRENT-DATE-8
                   OR     NCCI_TERM_DT = 0
               END-EXEC
               EXEC SQL OPEN NCCI_CURSOR END-EXEC
               PERFORM UNTIL WS-SQLCODE NOT = 0
                   OR WS-NCCI-ENTRY-COUNT >= 5000
                   EXEC SQL
                       FETCH NCCI_CURSOR INTO
                           :WS-NCCI-COL1-CPT(
                               WS-NCCI-ENTRY-COUNT + 1),
                           :WS-NCCI-COL2-CPT(
                               WS-NCCI-ENTRY-COUNT + 1),
                           :WS-NCCI-EDIT-TYPE(
                               WS-NCCI-ENTRY-COUNT + 1),
                           :WS-NCCI-MOD-IND(
                               WS-NCCI-ENTRY-COUNT + 1),
                           :WS-NCCI-EFF-DT(
                               WS-NCCI-ENTRY-COUNT + 1),
                           :WS-NCCI-TERM-DT(
                               WS-NCCI-ENTRY-COUNT + 1)
                   END-EXEC
                   IF WS-SQLCODE = 0
                       ADD 1 TO WS-NCCI-ENTRY-COUNT
                   END-IF
               END-PERFORM
               EXEC SQL CLOSE NCCI_CURSOR END-EXEC
           END-IF

           DISPLAY 'HCCLMVAL - NCCI TABLE: ' WS-NCCI-ENTRY-COUNT
               ' ENTRIES LOADED'
           .

       1260-LOAD-MUE-TABLES.
      *----------------------------------------------------------------*
      * LOAD MUE (MEDICALLY UNLIKELY EDITS) THRESHOLDS INTO MEMORY    *
      * MUE VALUES REPRESENT THE MAX UNITS REPORTABLE PER CPT PER     *
      * DATE OF SERVICE FOR A SINGLE PROVIDER                          *
      *----------------------------------------------------------------*
           MOVE 0 TO WS-MUE-ENTRY-COUNT

           IF WS-MUE-STATUS = '00'
               PERFORM UNTIL WS-MUE-STATUS NOT = '00'
                   OR WS-MUE-ENTRY-COUNT >= 3000
                   READ MUE-THRESHOLD-FILE
                       AT END
                           MOVE '10' TO WS-MUE-STATUS
                       NOT AT END
                           ADD 1 TO WS-MUE-ENTRY-COUNT
                           MOVE MT-CPT-CD TO
                               WS-MUE-CPT-CD(WS-MUE-ENTRY-COUNT)
                           MOVE MT-PRACT-IND TO
                               WS-MUE-PRACT-IND(
                                   WS-MUE-ENTRY-COUNT)
                           MOVE MT-MUE-VALUE TO
                               WS-MUE-MAX-UNITS(
                                   WS-MUE-ENTRY-COUNT)
                           MOVE MT-MUE-ADJUD-IND TO
                               WS-MUE-ADJUD-IND(
                                   WS-MUE-ENTRY-COUNT)
                           MOVE MT-MUE-EFF-DT TO
                               WS-MUE-EFF-DT(WS-MUE-ENTRY-COUNT)
                           MOVE MT-MUE-TERM-DT TO
                               WS-MUE-TERM-DT(
                                   WS-MUE-ENTRY-COUNT)
                   END-READ
               END-PERFORM
           ELSE
               EXEC SQL
                   DECLARE MUE_CURSOR CURSOR FOR
                   SELECT MUE_CPT_CD, MUE_PRACT_IND,
                          MUE_MAX_UNITS, MUE_ADJUD_IND,
                          MUE_EFF_DT, MUE_TERM_DT
                   FROM   HCPS_CLAIMS_DB..MUE_THRESHOLDS
                   WHERE  MUE_TERM_DT >= :WS-CURRENT-DATE-8
                   OR     MUE_TERM_DT = 0
               END-EXEC
               EXEC SQL OPEN MUE_CURSOR END-EXEC
               PERFORM UNTIL WS-SQLCODE NOT = 0
                   OR WS-MUE-ENTRY-COUNT >= 3000
                   EXEC SQL
                       FETCH MUE_CURSOR INTO
                           :WS-MUE-CPT-CD(
                               WS-MUE-ENTRY-COUNT + 1),
                           :WS-MUE-PRACT-IND(
                               WS-MUE-ENTRY-COUNT + 1),
                           :WS-MUE-MAX-UNITS(
                               WS-MUE-ENTRY-COUNT + 1),
                           :WS-MUE-ADJUD-IND(
                               WS-MUE-ENTRY-COUNT + 1),
                           :WS-MUE-EFF-DT(
                               WS-MUE-ENTRY-COUNT + 1),
                           :WS-MUE-TERM-DT(
                               WS-MUE-ENTRY-COUNT + 1)
                   END-EXEC
                   IF WS-SQLCODE = 0
                       ADD 1 TO WS-MUE-ENTRY-COUNT
                   END-IF
               END-PERFORM
               EXEC SQL CLOSE MUE_CURSOR END-EXEC
           END-IF

           DISPLAY 'HCCLMVAL - MUE TABLE: ' WS-MUE-ENTRY-COUNT
               ' ENTRIES LOADED'
           .

       1270-LOAD-LCD-NCD-TABLES.
      *----------------------------------------------------------------*
      * LOAD LCD/NCD COVERAGE DETERMINATION TABLES INTO MEMORY         *
      * LOCAL COVERAGE DETERMINATIONS ARE MAC-SPECIFIC                 *
      * NATIONAL COVERAGE DETERMINATIONS APPLY TO ALL MEDICARE         *
      *----------------------------------------------------------------*
           MOVE 0 TO WS-LCD-ENTRY-COUNT

           IF WS-LCD-STATUS = '00'
               PERFORM UNTIL WS-LCD-STATUS NOT = '00'
                   OR WS-LCD-ENTRY-COUNT >= 2000
                   READ LCD-NCD-FILE
                       AT END
                           MOVE '10' TO WS-LCD-STATUS
                       NOT AT END
                           ADD 1 TO WS-LCD-ENTRY-COUNT
                           MOVE LN-COVERAGE-TYPE TO
                               WS-LCD-COV-TYPE(
                                   WS-LCD-ENTRY-COUNT)
                           MOVE LN-COVERAGE-ID TO
                               WS-LCD-COV-ID(
                                   WS-LCD-ENTRY-COUNT)
                           MOVE LN-PROC-CD TO
                               WS-LCD-PROC-CD(
                                   WS-LCD-ENTRY-COUNT)
                           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                               UNTIL WS-SUB-1 > 20
                               MOVE LN-COVERED-DIAG(WS-SUB-1)
                                   TO WS-LCD-DIAG-CD(
                                       WS-LCD-ENTRY-COUNT,
                                       WS-SUB-1)
                           END-PERFORM
                           MOVE LN-CONTRACTOR-ID TO
                               WS-LCD-CONTRACTOR(
                                   WS-LCD-ENTRY-COUNT)
                           MOVE LN-EFF-DT TO
                               WS-LCD-EFF-DT(
                                   WS-LCD-ENTRY-COUNT)
                           MOVE LN-TERM-DT TO
                               WS-LCD-TERM-DT(
                                   WS-LCD-ENTRY-COUNT)
                   END-READ
               END-PERFORM
           END-IF

           DISPLAY 'HCCLMVAL - LCD/NCD TABLE: ' WS-LCD-ENTRY-COUNT
               ' ENTRIES LOADED'
           .

       1300-WRITE-REPORT-HEADERS.
      *----------------------------------------------------------------*
      * WRITE REPORT HEADERS TO ERROR REPORT FILE                     *
      *----------------------------------------------------------------*
           ADD 1 TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT TO WS-RPT-PAGE-NO
           WRITE ERROR-REPORT-REC FROM WS-RPT-HEADER-1
               AFTER ADVANCING PAGE
           WRITE ERROR-REPORT-REC FROM WS-RPT-HEADER-2
               AFTER ADVANCING 1 LINE
           MOVE SPACES TO ERROR-REPORT-REC
           WRITE ERROR-REPORT-REC
               AFTER ADVANCING 1 LINE
           MOVE 3 TO WS-LINE-COUNT
           .

       1400-READ-CLAIM-INPUT.
      *----------------------------------------------------------------*
      * READ NEXT CLAIM FROM INPUT FILE                                *
      *----------------------------------------------------------------*
           READ CLAIM-INPUT-FILE INTO WS-CLAIM-HEADER-REC
               AT END
                   SET WS-END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-STAT-RECORDS-READ
           END-READ
           .

       2000-PROCESS-CLAIMS.
      *================================================================*
      * MAIN CLAIM PROCESSING LOOP - VALIDATE EACH CLAIM              *
      * PERFORMS ALL EDITS IN SEQUENCE. LATER EDITS MAY BE SKIPPED    *
      * IF EARLIER FATAL EDITS ARE ENCOUNTERED.                        *
      *================================================================*
           INITIALIZE WS-EDIT-ERROR-TABLE
           INITIALIZE WS-MODIFIER-TABLE
           SET WS-CLAIM-IS-VALID TO TRUE
           SET WS-NO-FATAL-EDITS TO TRUE
           SET WS-NCCI-PASSED TO TRUE
           SET WS-MUE-PASSED TO TRUE
           SET WS-GENDER-OK TO TRUE
           SET WS-AGE-OK TO TRUE
           SET WS-NOT-TELEHEALTH TO TRUE
           SET WS-NOT-PREVENTIVE TO TRUE
           SET WS-MH-PARITY-NA TO TRUE
           SET WS-NOT-CROSSOVER TO TRUE
           MOVE 0 TO WS-EDIT-ERR-COUNT

      *--- UPDATE CLAIM TYPE STATISTICS
           EVALUATE TRUE
               WHEN WS-CLM-INSTITUTIONAL
                   ADD 1 TO WS-CSTAT-INST-READ
               WHEN WS-CLM-PROFESSIONAL
                   ADD 1 TO WS-CSTAT-PROF-READ
               WHEN WS-CLM-DENTAL
                   ADD 1 TO WS-CSTAT-DENT-READ
               WHEN WS-CLM-PHARMACY
                   ADD 1 TO WS-CSTAT-PHARM-READ
           END-EVALUATE

      *--- LEVEL 1: BASIC FIELD VALIDATION
           PERFORM 2100-VALIDATE-CLAIM-HEADER
           PERFORM 2200-VALIDATE-PATIENT-INFO
           PERFORM 2300-VALIDATE-PROVIDER-INFO
           PERFORM 2400-VALIDATE-DATES
           PERFORM 2500-VALIDATE-DIAGNOSIS-CODES
           PERFORM 2600-VALIDATE-PROCEDURE-CODES
           PERFORM 2700-VALIDATE-FINANCIAL-INFO

      *--- LEVEL 2: CLAIM-TYPE SPECIFIC VALIDATION
           IF WS-CLM-INSTITUTIONAL
               PERFORM 2150-VALIDATE-INSTITUTIONAL-FIELDS
           END-IF
           IF WS-CLM-PROFESSIONAL
               PERFORM 2250-VALIDATE-PROFESSIONAL-FIELDS
           END-IF

      *--- LEVEL 3: DATABASE LOOKUPS
           PERFORM 2800-CHECK-ELIGIBILITY
           PERFORM 2900-CHECK-DUPLICATE-CLAIM

      *--- LEVEL 4: NCCI / MUE / CODING EDITS
      *--- ONLY PERFORM IF NO FATAL EDITS SO FAR (PERFORMANCE)
           IF WS-NO-FATAL-EDITS
               PERFORM 2550-VALIDATE-NCCI-EDITS
               PERFORM 2560-VALIDATE-GENDER-AGE-EDITS
               PERFORM 2570-VALIDATE-DIAGNOSIS-PROC-LINK
               PERFORM 2580-VALIDATE-PLACE-OF-SERVICE
               PERFORM 2590-VALIDATE-MODIFIER-LOGIC
           END-IF

      *--- LEVEL 5: PAYER AND BUSINESS RULE EDITS
           PERFORM 3150-CHECK-TIMELY-FILING-BY-PAYER
           PERFORM 3250-VALIDATE-AUTHORIZATION-DETAIL
           PERFORM 3350-VALIDATE-COB-SECONDARY-CLAIM
           PERFORM 3450-CHECK-CLAIM-CROSSOVER
           PERFORM 3300-APPLY-PAYER-SPECIFIC-EDITS

      *--- LEVEL 6: SPECIALIZED EDITS
           PERFORM 3400-CHECK-NO-SURPRISES-ACT
           PERFORM 3550-VALIDATE-TELEHEALTH-EDITS
           PERFORM 3650-VALIDATE-MENTAL-HEALTH-PARITY
           PERFORM 3750-VALIDATE-PREVENTIVE-CARE
           PERFORM 3500-CHECK-OPIOID-EDITS
           PERFORM 3600-CHECK-COVID-EDITS

           IF WS-CLAIM-IS-VALID
               PERFORM 4000-WRITE-VALID-CLAIM
           ELSE
               PERFORM 5000-WRITE-REJECTED-CLAIM
           END-IF

           PERFORM 1400-READ-CLAIM-INPUT
           .

       2100-VALIDATE-CLAIM-HEADER.
      *================================================================*
      * VALIDATE CLAIM HEADER REQUIRED FIELDS                          *
      *================================================================*
      *--- EDIT 01: CLAIM NUMBER MUST BE PRESENT
           IF WS-CLM-NUMBER = SPACES OR LOW-VALUES
               PERFORM 6000-ADD-EDIT-ERROR-CLM-NUM
               ADD 1 TO WS-ESTAT-HEADER-ERRS
           END-IF

      *--- EDIT 02: CLAIM TYPE MUST BE VALID
           IF NOT (WS-CLM-INSTITUTIONAL OR WS-CLM-PROFESSIONAL
                   OR WS-CLM-DENTAL OR WS-CLM-PHARMACY)
               MOVE 'ED0002' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'INVALID CLAIM TYPE CODE'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               MOVE 'CLM-TYPE' TO
                   WS-EDIT-ERR-FIELD(WS-EDIT-ERR-COUNT + 1)
               MOVE WS-CLM-TYPE TO
                   WS-EDIT-ERR-VALUE(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-HEADER-ERRS
           END-IF

      *--- EDIT 03: CLAIM FORM TYPE MUST BE VALID
           IF NOT (WS-CLM-UB04 OR WS-CLM-CMS1500 OR WS-CLM-ADA)
               MOVE 'ED0003' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'INVALID CLAIM FORM TYPE'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               MOVE 'CLM-FORM-TYPE' TO
                   WS-EDIT-ERR-FIELD(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-HEADER-ERRS
           END-IF

      *--- EDIT 04: FREQUENCY CODE MUST BE VALID
           IF NOT (WS-CLM-ORIGINAL OR WS-CLM-REPLACEMENT
                   OR WS-CLM-VOID)
               MOVE 'ED0004' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'W' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'INVALID FREQUENCY CODE - DEFAULT TO ORIGINAL'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               MOVE '1' TO WS-CLM-FREQ-CD
               ADD 1 TO WS-ESTAT-HEADER-ERRS
           END-IF

      *--- EDIT 05: REPLACEMENT CLAIM MUST HAVE ORIGINAL REFERENCE
           IF WS-CLM-REPLACEMENT
               IF WS-CLM-ORIG-CLM-NO = SPACES OR LOW-VALUES
                   MOVE 'ED0005' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'REPLACEMENT CLAIM MISSING ORIGINAL REF'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   SET WS-HAS-FATAL-EDIT TO TRUE
                   ADD 1 TO WS-ESTAT-HEADER-ERRS
               END-IF
           END-IF

      *--- EDIT 06: PAYER CODE MUST BE PRESENT
           IF WS-CLM-PAYER-CD = SPACES OR LOW-VALUES
               MOVE 'ED0006' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'PAYER CODE IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-HEADER-ERRS
           ELSE
      *---     VERIFY PAYER EXISTS IN DATABASE
               PERFORM 6100-LOOKUP-PAYER
           END-IF

      *--- EDIT 07: INSTITUTIONAL CLAIMS REQUIRE BILL TYPE
           IF WS-CLM-INSTITUTIONAL
               IF WS-CLM-BILL-TYPE = SPACES OR LOW-VALUES
                   MOVE 'ED0007' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'BILL TYPE REQUIRED FOR INSTITUTIONAL CLAIM'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-HEADER-ERRS
               END-IF
           END-IF

      *--- EDIT 08: INSTITUTIONAL CLAIMS REQUIRE ADMIT/DISCHARGE
           IF WS-CLM-INSTITUTIONAL
               IF WS-CLM-ADMIT-DT = ZEROS
                   MOVE 'ED0008' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'ADMISSION DATE REQUIRED FOR INPATIENT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-HEADER-ERRS
               END-IF
           END-IF

      *--- EDIT 09: VOID CLAIM MUST HAVE ORIGINAL REFERENCE
           IF WS-CLM-VOID
               IF WS-CLM-ORIG-CLM-NO = SPACES OR LOW-VALUES
                   MOVE 'ED0009' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'VOID CLAIM MUST REFERENCE ORIGINAL CLAIM'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   SET WS-HAS-FATAL-EDIT TO TRUE
                   ADD 1 TO WS-ESTAT-HEADER-ERRS
               END-IF
           END-IF
           .

       2150-VALIDATE-INSTITUTIONAL-FIELDS.
      *================================================================*
      * VALIDATE UB-04 INSTITUTIONAL CLAIM SPECIFIC FIELDS             *
      * BILL TYPE, REVENUE CODES, CONDITION CODES, OCCURRENCE CODES,   *
      * VALUE CODES, DRG, DISCHARGE STATUS, ATTENDING PHYSICIAN        *
      * ADDED MWILSON 2005-01-01, EXPANDED AGARCIA 2006-04-01         *
      *================================================================*

      *--- BILL TYPE VALIDATION (4-DIGIT: TYPE OF FACILITY / BILL
      *    CLASSIFICATION / FREQUENCY)
      *    DIGIT 1: TYPE OF FACILITY
      *      0 = NOT APPLICABLE
      *      1 = HOSPITAL
      *      2 = SKILLED NURSING
      *      3 = HOME HEALTH
      *      4 = RELIGIOUS NONMEDICAL
      *      5 = RESERVED
      *      6 = INTERMEDIATE CARE
      *      7 = CLINIC (FREESTANDING)
      *      8 = SPECIAL FACILITY
      *      9 = RESERVED
      *    DIGIT 2: BILL CLASSIFICATION
      *    DIGIT 3: FREQUENCY
      *      1 = ADMIT THROUGH DISCHARGE
      *      2 = INTERIM - FIRST CLAIM
      *      3 = INTERIM - CONTINUING
      *      4 = INTERIM - LAST CLAIM
      *      5 = LATE CHARGE ONLY
      *      7 = REPLACEMENT OF PRIOR CLAIM
      *      8 = VOID/CANCEL OF PRIOR CLAIM

           IF WS-CLM-BILL-TYPE NOT = SPACES AND
              WS-CLM-BILL-TYPE NOT = LOW-VALUES

               MOVE WS-CLM-BILL-TYPE(1:1) TO WS-BILL-TYPE-DIGIT-1
               MOVE WS-CLM-BILL-TYPE(2:1) TO WS-BILL-TYPE-DIGIT-2
               MOVE WS-CLM-BILL-TYPE(3:1) TO WS-BILL-TYPE-DIGIT-3
               IF WS-CLM-BILL-TYPE(4:1) NOT = SPACES
                   MOVE WS-CLM-BILL-TYPE(4:1)
                       TO WS-BILL-TYPE-DIGIT-4
               ELSE
                   MOVE '0' TO WS-BILL-TYPE-DIGIT-4
               END-IF

      *---     VALIDATE FACILITY TYPE (DIGIT 1 IN 3-DIGIT BILL TYPE)
               IF WS-BILL-TYPE-DIGIT-1 NOT = '0'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '1'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '2'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '3'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '4'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '6'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '7'
                   AND WS-BILL-TYPE-DIGIT-1 NOT = '8'
                   MOVE 'ED0200' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'INVALID BILL TYPE FACILITY CODE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   MOVE 'CLM-BILL-TYPE' TO
                       WS-EDIT-ERR-FIELD(WS-EDIT-ERR-COUNT + 1)
                   MOVE WS-CLM-BILL-TYPE TO
                       WS-EDIT-ERR-VALUE(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-HEADER-ERRS
               END-IF

      *---     VALIDATE CLASSIFICATION (DIGIT 2) BY FACILITY TYPE
               EVALUATE WS-BILL-TYPE-DIGIT-1
                   WHEN '1'
      *---             HOSPITAL: VALID CLASSIFICATIONS
      *                1=INPATIENT(A), 2=INPATIENT(B),
      *                3=OUTPATIENT, 4=OTHER, 8=SWING BED
                       IF WS-BILL-TYPE-DIGIT-2 NOT = '1'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '2'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '3'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '4'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '8'
                           MOVE 'ED0201' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'INVALID CLASSIFICATION FOR HOSPITAL BILL TYPE'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                       END-IF
                   WHEN '2'
      *---             SNF: VALID CLASSIFICATIONS
      *                1=INPATIENT(A), 2=INPATIENT(B),
      *                3=OUTPATIENT, 8=SWING BED
                       IF WS-BILL-TYPE-DIGIT-2 NOT = '1'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '2'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '3'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '8'
                           MOVE 'ED0202' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'INVALID CLASSIFICATION FOR SNF BILL TYPE'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                       END-IF
                   WHEN '3'
      *---             HOME HEALTH: 1=VISIT, 2=CONTINUING,
      *                3=RECERT, 4=FINAL
                       IF WS-BILL-TYPE-DIGIT-2 NOT = '1'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '2'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '3'
                           AND WS-BILL-TYPE-DIGIT-2 NOT = '4'
                           MOVE 'ED0203' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'INVALID CLASSIFICATION FOR HH BILL TYPE'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
               END-EVALUATE

      *---     VALIDATE FREQUENCY CODE (DIGIT 3)
               IF WS-BILL-TYPE-DIGIT-3 NOT = '0'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '1'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '2'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '3'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '4'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '5'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '7'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '8'
                   AND WS-BILL-TYPE-DIGIT-3 NOT = '9'
                   MOVE 'ED0204' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'INVALID BILL TYPE FREQUENCY CODE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
               END-IF
           END-IF

      *--- REVENUE CODE / CHARGE VALIDATION PER REVENUE CODE
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

               IF WS-CLN-REV-CD NOT = SPACES AND
                  WS-CLN-REV-CD NOT = LOW-VALUES
      *---         REVENUE CODE 0001 (TOTAL) MUST BE LAST LINE
                   IF WS-CLN-REV-CD = '0001'
                       IF WS-LINE-CTR NOT = WS-CLT-LINE-COUNT
                           MOVE 'ED0210' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'REV CODE 0001 (TOTAL) MUST BE LAST LINE'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           MOVE WS-LINE-CTR TO
                               WS-EDIT-ERR-LINE-NO(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF

      *---         ROOM AND BOARD CODES (010X-021X) NEED UNITS
                   IF WS-CLN-REV-CD >= '0100' AND
                      WS-CLN-REV-CD <= '0219'
                       IF WS-CLN-UNITS <= 0
                           MOVE 'ED0211' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'ROOM & BOARD REV CODE REQUIRES UNITS (DAYS)'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF

      *---         PHARMACY REV CODES (025X) NEED HCPCS OR NDC
                   IF WS-CLN-REV-CD >= '0250' AND
                      WS-CLN-REV-CD <= '0259'
                       IF WS-CLN-HCPCS-CD = SPACES AND
                          WS-CLN-NDC-CD = SPACES
                           MOVE 'ED0212' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'PHARMACY REV CD NEEDS HCPCS/NDC'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF

      *---         EMERGENCY REV CODES (045X) CHECK
                   IF WS-CLN-REV-CD >= '0450' AND
                      WS-CLN-REV-CD <= '0459'
                       IF WS-CLN-HCPCS-CD = SPACES
                           MOVE 'ED0213' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'EMERGENCY REV CD SHOULD HAVE HCPCS'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      *--- CONDITION CODE VALIDATION
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 11
               IF WS-CLM-COND-CD(WS-SUB-1) NOT = SPACES AND
                  WS-CLM-COND-CD(WS-SUB-1) NOT = LOW-VALUES
                   SET WS-WK-NOT-FOUND TO TRUE
                   PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                       UNTIL WS-SUB-2 > WS-COND-CODE-COUNT
                       OR WS-WK-FOUND
                       IF WS-CLM-COND-CD(WS-SUB-1) =
                           WS-COND-CD-VALUE(WS-SUB-2)
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                   END-PERFORM
                   IF WS-WK-NOT-FOUND
                       MOVE 'ED0220' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'INVALID CONDITION CODE: '
                           WS-CLM-COND-CD(WS-SUB-1)
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
           END-PERFORM

      *--- OCCURRENCE CODE / DATE VALIDATION
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 8
               IF WS-CLM-OCCUR-CD(WS-SUB-1) NOT = SPACES AND
                  WS-CLM-OCCUR-CD(WS-SUB-1) NOT = LOW-VALUES
      *---         OCCURRENCE CODE MUST HAVE A DATE
                   IF WS-CLM-OCCUR-DT(WS-SUB-1) = ZEROS
                       MOVE 'ED0221' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'OCCURRENCE CODE '
                           WS-CLM-OCCUR-CD(WS-SUB-1)
                           ' MISSING DATE'
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                   ELSE
      *---             VALIDATE THE OCCURRENCE DATE
                       MOVE WS-CLM-OCCUR-DT(WS-SUB-1)
                           TO WS-DATE-WORK
                       PERFORM 7000-VALIDATE-DATE
                       IF WS-DATE-WORK = ZEROS
                           MOVE 'ED0222' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'OCCURRENCE DATE IS INVALID'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      *--- VALUE CODE / AMOUNT VALIDATION
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 12
               IF WS-CLM-VALUE-CD(WS-SUB-1) NOT = SPACES AND
                  WS-CLM-VALUE-CD(WS-SUB-1) NOT = LOW-VALUES
      *---         VALUE CODE MUST HAVE AN AMOUNT
                   IF WS-CLM-VALUE-AMT(WS-SUB-1) = ZEROS
                       MOVE 'ED0223' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'VALUE CODE '
                           WS-CLM-VALUE-CD(WS-SUB-1)
                           ' HAS ZERO AMOUNT'
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
      *---         VALIDATE VALUE CODE EXISTS
                   SET WS-WK-NOT-FOUND TO TRUE
                   PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                       UNTIL WS-SUB-2 > WS-VALUE-CODE-COUNT
                       OR WS-WK-FOUND
                       IF WS-CLM-VALUE-CD(WS-SUB-1) =
                           WS-VALUE-CD-VALUE(WS-SUB-2)
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                   END-PERFORM
                   IF WS-WK-NOT-FOUND
                       MOVE 'ED0224' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'UNRECOGNIZED VALUE CODE: '
                           WS-CLM-VALUE-CD(WS-SUB-1)
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
           END-PERFORM

      *--- DRG / LOS CONSISTENCY CHECKS
      *--- DRG IS ONLY RELEVANT FOR INPATIENT HOSPITAL (BILL TYPE 11X)
           IF WS-BILL-TYPE-DIGIT-1 = '1' AND
              WS-BILL-TYPE-DIGIT-2 = '1'
               IF WS-CLM-DRG-CD NOT = SPACES AND
                  WS-CLM-DRG-CD NOT = LOW-VALUES
      *---         VALIDATE DRG EXISTS IN DATABASE
                   EXEC SQL
                       SELECT DRG_WEIGHT, DRG_GMLOS, DRG_AMLOS
                       INTO  :WS-WORK-AMOUNT,
                             :WS-LOS-DAYS,
                             :WS-DAYS-DIFF
                       FROM  HCPS_CLAIMS_DB..DRG_MASTER
                       WHERE DRG_CD = :WS-CLM-DRG-CD
                       AND   DRG_EFF_DT <= :WS-CLM-FROM-DOS
                       AND   (DRG_TERM_DT >= :WS-CLM-FROM-DOS
                              OR DRG_TERM_DT = 0)
                   END-EXEC
                   IF WS-SQLCODE = 100
                       MOVE 'ED0230' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'DRG CODE NOT FOUND IN DRG MASTER'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
                   IF WS-SQLCODE = 0
      *---             CHECK ACTUAL LOS VS DRG EXPECTED LOS
                       IF WS-CLM-ADMIT-DT NOT = ZEROS AND
                          WS-CLM-DISCH-DT NOT = ZEROS
                           COMPUTE WS-PATIENT-AGE-DAYS =
                               FUNCTION INTEGER-OF-DATE(
                                   WS-CLM-DISCH-DT)
                             - FUNCTION INTEGER-OF-DATE(
                                   WS-CLM-ADMIT-DT)
      *---                 IF ACTUAL LOS > 2X ARITHMETIC MEAN LOS
                           IF WS-PATIENT-AGE-DAYS >
                               (WS-DAYS-DIFF * 2)
                               MOVE 'ED0231' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE
                       'LOS EXCEEDS 2X DRG ARITHMETIC MEAN LOS'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                           END-IF
                       END-IF
                   END-IF
               ELSE
      *---         INPATIENT HOSPITAL SHOULD HAVE DRG
                   MOVE 'ED0232' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'DRG CODE MISSING FOR INPATIENT CLAIM'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF

      *--- DISCHARGE STATUS CODE VALIDATION
           IF WS-CLM-DISCH-STATUS NOT = SPACES AND
              WS-CLM-DISCH-STATUS NOT = LOW-VALUES
               SET WS-WK-NOT-FOUND TO TRUE
               PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                   UNTIL WS-SUB-1 > WS-DISCH-CODE-COUNT
                   OR WS-WK-FOUND
                   IF WS-CLM-DISCH-STATUS =
                       WS-DISCH-CD-VALUE(WS-SUB-1)
                       SET WS-WK-FOUND TO TRUE
                   END-IF
               END-PERFORM
               IF WS-WK-NOT-FOUND
                   MOVE 'ED0240' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'INVALID DISCHARGE STATUS CODE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
               END-IF
      *---     STATUS 20 (EXPIRED) CHECK - MUST HAVE DISCHARGE DATE
               IF WS-CLM-DISCH-STATUS = '20'
                   IF WS-CLM-DISCH-DT = ZEROS
                       MOVE 'ED0241' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'DECEASED STATUS REQUIRES DISCHARGE DATE'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                   END-IF
               END-IF
      *---     STATUS 30 (STILL PATIENT) - NO DISCHARGE DATE
               IF WS-CLM-DISCH-STATUS = '30'
                   IF WS-CLM-DISCH-DT NOT = ZEROS
                       MOVE 'ED0242' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'STILL PATIENT STATUS SHOULD NOT HAVE DISCH DT'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
           ELSE
      *---     DISCHARGE STATUS IS REQUIRED FOR INSTITUTIONAL
               MOVE 'ED0243' TO WS-EDIT-ERR-CD(
                   WS-EDIT-ERR-COUNT + 1)
               MOVE 'W' TO WS-EDIT-ERR-SEV(
                   WS-EDIT-ERR-COUNT + 1)
               MOVE 'DISCHARGE STATUS CODE IS MISSING'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
           END-IF

      *--- OPERATING/ATTENDING PHYSICIAN REQUIREMENTS BY BILL TYPE
      *--- INPATIENT (X1X) REQUIRES ATTENDING PHYSICIAN
           IF WS-BILL-TYPE-DIGIT-2 = '1'
               IF WS-CLM-ATTEND-PROV-NPI = SPACES OR LOW-VALUES
                   MOVE 'ED0250' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE
               'ATTENDING PHYSICIAN REQUIRED FOR INPATIENT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
               ELSE
      *---         VALIDATE ATTENDING NPI
                   MOVE WS-CLM-ATTEND-PROV-NPI TO WS-NPI-WORK
                   PERFORM 7100-VALIDATE-NPI
                   IF WS-NPI-INVALID
                       MOVE 'ED0251' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'ATTENDING PHYSICIAN NPI FAILS CHECK DIGIT'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
      *---     SURGICAL CLAIMS SHOULD HAVE OPERATING PHYSICIAN
               IF WS-CLM-OPER-PROV-NPI = SPACES OR LOW-VALUES
      *---         CHECK IF ANY SURGICAL REVENUE CODES PRESENT
                   PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                       UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                       MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                           TO WS-CLAIM-LINE-REC
                       IF WS-CLN-REV-CD >= '0360' AND
                          WS-CLN-REV-CD <= '0369'
                           MOVE 'ED0252' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                   'OPERATING PHYSICIAN EXPECTED FOR SURGERY REVCD'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .

       2200-VALIDATE-PATIENT-INFO.
      *================================================================*
      * VALIDATE PATIENT DEMOGRAPHIC INFORMATION                       *
      *================================================================*
      *--- EDIT 10: PATIENT MRN REQUIRED
           IF WS-CLM-PAT-MRN = SPACES OR LOW-VALUES
               MOVE 'ED0010' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'PATIENT MRN IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           END-IF

      *--- EDIT 11: PATIENT NAME REQUIRED
           IF WS-CLM-PAT-LAST-NAME = SPACES OR LOW-VALUES
               MOVE 'ED0011' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'PATIENT LAST NAME IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           END-IF

      *--- EDIT 12: PATIENT DOB MUST BE VALID
           IF WS-CLM-PAT-DOB = ZEROS
               MOVE 'ED0012' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'PATIENT DATE OF BIRTH IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           ELSE
               MOVE WS-CLM-PAT-DOB TO WS-DATE-WORK
               PERFORM 7000-VALIDATE-DATE
               IF WS-DATE-WORK = ZEROS
                   MOVE 'ED0012' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'PATIENT DOB IS INVALID'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-PATIENT-ERRS
               END-IF
           END-IF

      *--- EDIT 13: PATIENT DOB CANNOT BE FUTURE
           IF WS-CLM-PAT-DOB > WS-CURRENT-DATE-8
               MOVE 'ED0013' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'PATIENT DOB CANNOT BE IN THE FUTURE'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           END-IF

      *--- EDIT 14: GENDER MUST BE VALID
           IF WS-CLM-PAT-GENDER NOT = 'M' AND
              WS-CLM-PAT-GENDER NOT = 'F' AND
              WS-CLM-PAT-GENDER NOT = 'U'
               MOVE 'ED0014' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'W' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'INVALID PATIENT GENDER CODE'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           END-IF

      *--- EDIT 15: SUBSCRIBER INFO REQUIRED
           IF WS-CLM-SUB-MEMBER-ID = SPACES OR LOW-VALUES
               MOVE 'ED0015' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'SUBSCRIBER MEMBER ID IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           END-IF

      *--- EDIT 16: PATIENT RELATIONSHIP REQUIRED
           IF NOT (WS-CLM-PAT-SELF OR WS-CLM-PAT-SPOUSE
                   OR WS-CLM-PAT-CHILD OR WS-CLM-PAT-OTHER-REL)
               MOVE 'ED0016' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'INVALID PATIENT RELATIONSHIP TO SUBSCRIBER'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-PATIENT-ERRS
           END-IF

      *--- COMPUTE PATIENT AGE FOR LATER EDITS
           IF WS-CLM-PAT-DOB NOT = ZEROS AND
              WS-CLM-FROM-DOS NOT = ZEROS
               COMPUTE WS-PATIENT-AGE-DAYS =
                   FUNCTION INTEGER-OF-DATE(WS-CLM-FROM-DOS)
                 - FUNCTION INTEGER-OF-DATE(WS-CLM-PAT-DOB)
               COMPUTE WS-PATIENT-AGE-YEARS =
                   WS-PATIENT-AGE-DAYS / 365
           ELSE
               MOVE 0 TO WS-PATIENT-AGE-YEARS
               MOVE 0 TO WS-PATIENT-AGE-DAYS
           END-IF
           .

       2250-VALIDATE-PROFESSIONAL-FIELDS.
      *================================================================*
      * VALIDATE CMS-1500 PROFESSIONAL CLAIM SPECIFIC FIELDS          *
      * PLACE OF SERVICE, RENDERING/BILLING PROVIDER, REFERRING       *
      * PROVIDER, SERVICE FACILITY                                     *
      *================================================================*

      *--- PLACE OF SERVICE CODE VALIDATION (ALL 50+ VALID POS CODES)
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

               IF WS-CLN-PLACE-OF-SVC NOT = SPACES AND
                  WS-CLN-PLACE-OF-SVC NOT = LOW-VALUES
                   SET WS-WK-NOT-FOUND TO TRUE
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-POS-VALID-COUNT
                       OR WS-WK-FOUND
                       IF WS-CLN-PLACE-OF-SVC =
                           WS-POS-CODE(WS-SUB-1)
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                   END-PERFORM
                   IF WS-WK-NOT-FOUND
                       MOVE 'ED0260' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'INVALID PLACE OF SERVICE CODE: '
                           WS-CLN-PLACE-OF-SVC
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       MOVE WS-LINE-CTR TO
                           WS-EDIT-ERR-LINE-NO(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-POS-ERRS
                   END-IF
               END-IF
           END-PERFORM

      *--- RENDERING VS BILLING PROVIDER LOGIC
      *--- IF BILLING AND RENDERING ARE SAME, RENDERING IS OPTIONAL
      *--- BUT IF RENDERING IS PROVIDED IT MUST BE VALID
           IF WS-CLM-REND-PROV-NPI NOT = SPACES AND
              WS-CLM-REND-PROV-NPI NOT = LOW-VALUES
               MOVE WS-CLM-REND-PROV-NPI TO WS-NPI-WORK
               PERFORM 7100-VALIDATE-NPI
               IF WS-NPI-INVALID
                   MOVE 'ED0261' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'RENDERING PROVIDER NPI FAILS CHECK DIGIT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-PROVIDER-ERRS
               END-IF
      *---     CHECK RENDERING PROVIDER NOT ON EXCLUSION LIST
               EXEC SQL
                   SELECT PRV_OIG_EXCL_FLAG
                   INTO  :WS-PRV-OIG-EXCL-FLAG
                   FROM  HCPS_CLAIMS_DB..PROVIDER_MASTER
                   WHERE PRV_NPI = :WS-CLM-REND-PROV-NPI
               END-EXEC
               IF WS-SQLCODE = 0
                   IF WS-PRV-OIG-EXCL-FLAG = 'Y'
                       MOVE 'ED0262' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'RENDERING PROVIDER ON OIG EXCLUSION LIST'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       SET WS-HAS-FATAL-EDIT TO TRUE
                   END-IF
               END-IF
           ELSE
      *---     RENDERING NPI REQUIRED IF DIFFERENT FROM BILLING
      *---     FOR GROUP PRACTICES BILLING UNDER ORG NPI
               IF WS-CLM-BILL-PROV-NPI NOT = SPACES
                   EXEC SQL
                       SELECT PRV_ENTITY_TYPE
                       INTO  :WS-WORK-STRING
                       FROM  HCPS_CLAIMS_DB..PROVIDER_MASTER
                       WHERE PRV_NPI = :WS-CLM-BILL-PROV-NPI
                   END-EXEC
                   IF WS-SQLCODE = 0
                       IF WS-WORK-STRING(1:1) = '2'
      *---                 TYPE 2 = ORGANIZATION - RENDERING REQUIRED
                           MOVE 'ED0263' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                   'RENDERING NPI REQUIRED FOR ORG BILLING NPI'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF

      *--- REFERRING PROVIDER REQUIREMENTS BY SPECIALTY
      *--- CERTAIN SPECIALTIES REQUIRE A REFERRING PROVIDER
           IF WS-CLM-REFER-PROV-NPI NOT = SPACES AND
              WS-CLM-REFER-PROV-NPI NOT = LOW-VALUES
               MOVE WS-CLM-REFER-PROV-NPI TO WS-NPI-WORK
               PERFORM 7100-VALIDATE-NPI
               IF WS-NPI-INVALID
                   MOVE 'ED0264' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'REFERRING PROVIDER NPI FAILS CHECK DIGIT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF

      *--- SERVICE FACILITY VALIDATION FOR ASC/HOSPITAL OUTPATIENT
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
      *---     POS 24 (ASC) OR POS 22 (HOSP OUTPATIENT) REQUIRE
      *        SERVICE FACILITY NPI
               IF WS-CLN-PLACE-OF-SVC = '24' OR
                  WS-CLN-PLACE-OF-SVC = '22'
                   IF WS-CLM-SVC-FAC-NPI = SPACES OR LOW-VALUES
                       MOVE 'ED0265' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'SERVICE FACILITY NPI EXPECTED FOR POS 22/24'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
           END-PERFORM
           .

       2300-VALIDATE-PROVIDER-INFO.
      *================================================================*
      * VALIDATE PROVIDER NPI AND NETWORK STATUS                       *
      *================================================================*
      *--- EDIT 20: BILLING PROVIDER NPI REQUIRED
           IF WS-CLM-BILL-PROV-NPI = SPACES OR LOW-VALUES
               MOVE 'ED0020' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'BILLING PROVIDER NPI IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-PROVIDER-ERRS
           ELSE
      *---     VALIDATE NPI CHECK DIGIT (LUHN ALGORITHM)
               MOVE WS-CLM-BILL-PROV-NPI TO WS-NPI-WORK
               PERFORM 7100-VALIDATE-NPI
               IF WS-NPI-INVALID
                   MOVE 'ED0021' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'BILLING PROVIDER NPI FAILS CHECK DIGIT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-PROVIDER-ERRS
               ELSE
      *---         VERIFY PROVIDER EXISTS AND IS NOT EXCLUDED
                   PERFORM 6200-LOOKUP-PROVIDER
               END-IF
           END-IF

      *--- EDIT 22: RENDERING PROVIDER NPI FOR PROFESSIONAL CLAIMS
           IF WS-CLM-PROFESSIONAL
               IF WS-CLM-REND-PROV-NPI = SPACES OR LOW-VALUES
                   MOVE 'ED0022' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'RENDERING NPI REQUIRED FOR PROF CLAIMS'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-PROVIDER-ERRS
               END-IF
           END-IF

      *--- EDIT 23: TAX ID MUST BE PRESENT
           IF WS-CLM-BILL-PROV-TAX-ID = SPACES OR LOW-VALUES
               MOVE 'ED0023' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'BILLING PROVIDER TAX ID IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-PROVIDER-ERRS
           END-IF

      *--- EDIT 24: CHECK OIG EXCLUSION LIST
           IF WS-DB-FOUND
               IF WS-PRV-OIG-EXCLUDED
                   MOVE 'ED0024' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'PROVIDER IS ON OIG EXCLUSION LIST'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   SET WS-HAS-FATAL-EDIT TO TRUE
                   ADD 1 TO WS-ESTAT-PROVIDER-ERRS
               END-IF
           END-IF

      *--- EDIT 25: ATTENDING PHYSICIAN FOR INPATIENT
           IF WS-CLM-INSTITUTIONAL
               IF WS-CLM-ATTEND-PROV-NPI = SPACES OR LOW-VALUES
                   MOVE 'ED0025' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'ATTENDING PHYSICIAN NPI RECOMMENDED'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF

      *--- EDIT 26: CHECK SAM (SYSTEM FOR AWARD MANAGEMENT) EXCLUSION
           IF WS-DB-FOUND
               IF WS-PRV-SAM-EXCL-FLAG = 'Y'
                   MOVE 'ED0027' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'PROVIDER IS ON SAM EXCLUSION LIST'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   SET WS-HAS-FATAL-EDIT TO TRUE
                   ADD 1 TO WS-ESTAT-PROVIDER-ERRS
               END-IF
           END-IF
           .

       2400-VALIDATE-DATES.
      *================================================================*
      * VALIDATE ALL DATE FIELDS ON THE CLAIM                          *
      *================================================================*
      *--- EDIT 30: FROM DOS REQUIRED
           IF WS-CLM-FROM-DOS = ZEROS
               MOVE 'ED0030' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'FROM DATE OF SERVICE IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-DATE-ERRS
           ELSE
               MOVE WS-CLM-FROM-DOS TO WS-DATE-WORK
               PERFORM 7000-VALIDATE-DATE
               IF WS-DATE-WORK = ZEROS
                   MOVE 'ED0030' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'FROM DOS IS NOT A VALID DATE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-DATE-ERRS
               END-IF
           END-IF

      *--- EDIT 31: FROM DOS CANNOT BE FUTURE
           IF WS-CLM-FROM-DOS > WS-CURRENT-DATE-8
               MOVE 'ED0031' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'FROM DOS CANNOT BE IN THE FUTURE'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-DATE-ERRS
           END-IF

      *--- EDIT 32: THRU DOS >= FROM DOS
           IF WS-CLM-THRU-DOS NOT = ZEROS
               IF WS-CLM-THRU-DOS < WS-CLM-FROM-DOS
                   MOVE 'ED0032' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'THRU DOS CANNOT BE BEFORE FROM DOS'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-DATE-ERRS
               END-IF
           END-IF

      *--- EDIT 33: DOS MUST BE AFTER PATIENT DOB
           IF WS-CLM-FROM-DOS < WS-CLM-PAT-DOB
               IF WS-CLM-PAT-DOB NOT = ZEROS
                   MOVE 'ED0033' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'DOS CANNOT BE BEFORE PATIENT BIRTH DATE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-DATE-ERRS
               END-IF
           END-IF

      *--- EDIT 34: ADMIT DATE <= DISCHARGE DATE
           IF WS-CLM-INSTITUTIONAL
               IF WS-CLM-DISCH-DT NOT = ZEROS
                   IF WS-CLM-DISCH-DT < WS-CLM-ADMIT-DT
                       MOVE 'ED0034' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'DISCHARGE DATE BEFORE ADMISSION DATE'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-DATE-ERRS
                   END-IF
               END-IF
           END-IF

      *--- EDIT 35: VALIDATE LENGTH OF STAY
           IF WS-CLM-INSTITUTIONAL
               IF WS-CLM-ADMIT-DT NOT = ZEROS AND
                  WS-CLM-DISCH-DT NOT = ZEROS
                   COMPUTE WS-DAYS-DIFF =
                       FUNCTION INTEGER-OF-DATE(WS-CLM-DISCH-DT)
                     - FUNCTION INTEGER-OF-DATE(WS-CLM-ADMIT-DT)
                   IF WS-DAYS-DIFF > 365
                       MOVE 'ED0035' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'LENGTH OF STAY EXCEEDS 365 DAYS'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-DATE-ERRS
                   END-IF
               END-IF
           END-IF

      *--- EDIT 36: LINE DOS MUST BE WITHIN HEADER DOS RANGE
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-LINE-FROM-DOS NOT = ZEROS
                   IF WS-CLN-LINE-FROM-DOS < WS-CLM-FROM-DOS
                       MOVE 'ED0036' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'LINE DOS BEFORE HEADER FROM DOS'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       MOVE WS-LINE-CTR TO
                           WS-EDIT-ERR-LINE-NO(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                   END-IF
                   IF WS-CLM-THRU-DOS NOT = ZEROS
                       IF WS-CLN-LINE-FROM-DOS > WS-CLM-THRU-DOS
                           MOVE 'ED0037' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                   'LINE DOS AFTER HEADER THRU DOS'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           MOVE WS-LINE-CTR TO
                               WS-EDIT-ERR-LINE-NO(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

       2500-VALIDATE-DIAGNOSIS-CODES.
      *================================================================*
      * VALIDATE ICD-10 DIAGNOSIS CODES                                *
      *================================================================*
      *--- EDIT 40: PRINCIPAL DIAGNOSIS REQUIRED
           IF WS-CLM-PRINC-DIAG = SPACES OR LOW-VALUES
               MOVE 'ED0040' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'PRINCIPAL DIAGNOSIS CODE IS REQUIRED'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-DIAG-ERRS
           ELSE
      *---     VALIDATE ICD-10 FORMAT
               MOVE WS-CLM-PRINC-DIAG TO WS-ICD-CODE-WORK
               PERFORM 7200-VALIDATE-ICD10-FORMAT
               IF WS-ICD-INVALID
                   MOVE 'ED0041' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'PRINCIPAL DIAG IS NOT VALID ICD-10 FORMAT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-DIAG-ERRS
               ELSE
      *---         VERIFY CODE EXISTS IN ICD-10 TABLE
                   PERFORM 6300-LOOKUP-DIAGNOSIS
               END-IF
           END-IF

      *--- EDIT 42: ICD VERSION MUST BE 10
           IF NOT WS-CLM-ICD10
               MOVE 'ED0042' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'ICD VERSION MUST BE 10 FOR DOS AFTER 10/1/2015'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-DIAG-ERRS
           END-IF

      *--- EDIT 43: VALIDATE ALL ADDITIONAL DIAGNOSIS CODES
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 25
               IF WS-CLM-DIAG-CD(WS-SUB-1) NOT = SPACES
                   AND WS-CLM-DIAG-CD(WS-SUB-1) NOT = LOW-VALUES
                   MOVE WS-CLM-DIAG-CD(WS-SUB-1)
                       TO WS-ICD-CODE-WORK
                   PERFORM 7200-VALIDATE-ICD10-FORMAT
                   IF WS-ICD-INVALID
                       MOVE 'ED0043' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'DIAG CODE ' WS-SUB-1
                           ' HAS INVALID ICD-10 FORMAT'
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-DIAG-ERRS
                   END-IF
               END-IF
           END-PERFORM

      *--- EDIT 44: PRESENT ON ADMISSION FOR INPATIENT
           IF WS-CLM-INSTITUTIONAL
               PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                   UNTIL WS-SUB-1 > 25
                   IF WS-CLM-DIAG-CD(WS-SUB-1) NOT = SPACES
                       AND WS-CLM-DIAG-CD(WS-SUB-1)
                           NOT = LOW-VALUES
                       IF WS-CLM-DIAG-POA(WS-SUB-1) = SPACES
                           MOVE 'ED0044' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'POA INDICATOR MISSING FOR DIAG'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       ELSE
      *---                 POA MUST BE VALID (Y/N/U/W/1)
                           IF WS-CLM-DIAG-POA(WS-SUB-1)
                               NOT = 'Y' AND
                              WS-CLM-DIAG-POA(WS-SUB-1)
                               NOT = 'N' AND
                              WS-CLM-DIAG-POA(WS-SUB-1)
                               NOT = 'U' AND
                              WS-CLM-DIAG-POA(WS-SUB-1)
                               NOT = 'W' AND
                              WS-CLM-DIAG-POA(WS-SUB-1)
                               NOT = '1'
                               MOVE 'ED0045' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'INVALID POA INDICATOR VALUE'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .

       2550-VALIDATE-NCCI-EDITS.
      *================================================================*
      * NCCI (NATIONAL CORRECT CODING INITIATIVE) EDITS                *
      * COLUMN 1 / COLUMN 2 CODE PAIR EDITS (BUNDLING)                *
      * MUTUALLY EXCLUSIVE PROCEDURE EDITS                             *
      * MUE (MEDICALLY UNLIKELY EDIT) CHECKS                          *
      * ADD-ON CODE VALIDATION                                         *
      * GLOBAL SURGERY PERIOD EDITS                                    *
      * SAME-DAY DUPLICATE PROCEDURE CHECK                             *
      * COMPONENT / COMPREHENSIVE CODE EDITING                         *
      * ORIGINALLY ADDED RJONES 1996-11-01, EXPANDED THROUGH 2024     *
      *================================================================*

      *--- COLUMN 1 / COLUMN 2 CODE PAIR EDITS
      *--- WHEN TWO PROCEDURES ARE BILLED ON THE SAME CLAIM FOR THE
      *--- SAME BENEFICIARY ON THE SAME DOS BY THE SAME PROVIDER,
      *--- AND THE CODE PAIR EXISTS IN THE NCCI TABLE, THE COLUMN 2
      *--- CODE IS BUNDLED INTO THE COLUMN 1 CODE
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-LINE-CTR-2 FROM 1 BY 1
                       UNTIL WS-LINE-CTR-2 > WS-CLT-LINE-COUNT
                       IF WS-LINE-CTR-2 NOT = WS-LINE-CTR
      *---                 GET THE OTHER LINE'S CPT CODE
                           MOVE WS-CLT-LINE-DATA(WS-LINE-CTR-2)
                               (1:5) TO WS-WORK-STRING

      *---                 SEARCH NCCI TABLE FOR THIS PAIR
                           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                               UNTIL WS-SUB-1 >
                                   WS-NCCI-ENTRY-COUNT
                               IF WS-CLN-HCPCS-CD =
                                   WS-NCCI-COL1-CPT(WS-SUB-1)
                                   AND WS-WORK-STRING(1:5) =
                                   WS-NCCI-COL2-CPT(WS-SUB-1)
      *---                         CHECK IF EDIT IS ACTIVE
                                   IF WS-NCCI-EFF-DT(WS-SUB-1)
                                       <= WS-CLM-FROM-DOS
                                       AND (
                                       WS-NCCI-TERM-DT(WS-SUB-1)
                                       >= WS-CLM-FROM-DOS
                                       OR WS-NCCI-TERM-DT(WS-SUB-1)
                                       = 0)

      *---                             CHECK EDIT TYPE
                                       IF WS-NCCI-COLUMN(WS-SUB-1)
      *---                                 COLUMN 1/2 EDIT - CHECK
      *                                    IF MODIFIER OVERRIDE
      *                                    IS ALLOWED
                                           IF WS-NCCI-MOD-OK(
                                               WS-SUB-1)
      *---                                     MODIFIER CAN OVERRIDE
      *                                        CHECK FOR 25,59,XE,XP,
      *                                        XS,XU
                                               PERFORM
                                               7300-CHECK-NCCI-MOD-OVERRIDE
                                               IF WS-NCCI-FAILED
                                                   MOVE 'ED0300' TO
                                                   WS-EDIT-ERR-CD(
                                                   WS-EDIT-ERR-COUNT
                                                       + 1)
                                                   MOVE 'F' TO
                                                   WS-EDIT-ERR-SEV(
                                                   WS-EDIT-ERR-COUNT
                                                       + 1)
                                                   STRING
                                                   'NCCI BUNDLE: '
                                                   WS-CLN-HCPCS-CD
                                                   ' INCLUDES '
                                                   WS-WORK-STRING
                                                       (1:5)
                                                   ' - NO MOD OVRD'
                                                   DELIMITED BY SIZE
                                                   INTO
                                                   WS-EDIT-ERR-MSG(
                                                   WS-EDIT-ERR-COUNT
                                                       + 1)
                                                   END-STRING
                                                   ADD 1 TO
                                                   WS-EDIT-ERR-COUNT
                                                   SET
                                                   WS-CLAIM-IS-INVALID
                                                       TO TRUE
                                                   ADD 1 TO
                                                   WS-ESTAT-NCCI-ERRS
                                               END-IF
                                           ELSE
      *---                                     NO MODIFIER OVERRIDE
      *                                        ALLOWED - ALWAYS BUNDLE
                                               MOVE 'ED0301' TO
                                               WS-EDIT-ERR-CD(
                                               WS-EDIT-ERR-COUNT + 1)
                                               MOVE 'F' TO
                                               WS-EDIT-ERR-SEV(
                                               WS-EDIT-ERR-COUNT + 1)
                                               STRING
                                               'NCCI BUNDLE: '
                                               WS-CLN-HCPCS-CD
                                               ' INCLUDES '
                                               WS-WORK-STRING(1:5)
                                               DELIMITED BY SIZE
                                               INTO WS-EDIT-ERR-MSG(
                                               WS-EDIT-ERR-COUNT + 1)
                                               END-STRING
                                               ADD 1 TO
                                               WS-EDIT-ERR-COUNT
                                               SET
                                               WS-CLAIM-IS-INVALID
                                                   TO TRUE
                                               ADD 1 TO
                                               WS-ESTAT-NCCI-ERRS
                                           END-IF
                                       END-IF

      *---                             MUTUALLY EXCLUSIVE EDIT
                                       IF WS-NCCI-MUTEX(WS-SUB-1)
                                           MOVE 'ED0302' TO
                                           WS-EDIT-ERR-CD(
                                           WS-EDIT-ERR-COUNT + 1)
                                           MOVE 'F' TO
                                           WS-EDIT-ERR-SEV(
                                           WS-EDIT-ERR-COUNT + 1)
                                           STRING
                                           'MUTUALLY EXCLUSIVE: '
                                           WS-CLN-HCPCS-CD
                                           ' AND '
                                           WS-WORK-STRING(1:5)
                                           DELIMITED BY SIZE
                                           INTO WS-EDIT-ERR-MSG(
                                           WS-EDIT-ERR-COUNT + 1)
                                           END-STRING
                                           ADD 1 TO
                                           WS-EDIT-ERR-COUNT
                                           SET WS-CLAIM-IS-INVALID
                                               TO TRUE
                                           ADD 1 TO
                                           WS-ESTAT-NCCI-ERRS
                                       END-IF
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *--- MUE (MEDICALLY UNLIKELY EDIT) CHECKS PER CPT CODE
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-MUE-ENTRY-COUNT
                       IF WS-CLN-HCPCS-CD =
                           WS-MUE-CPT-CD(WS-SUB-1)
                           IF WS-MUE-EFF-DT(WS-SUB-1)
                               <= WS-CLM-FROM-DOS
                               AND (WS-MUE-TERM-DT(WS-SUB-1)
                               >= WS-CLM-FROM-DOS
                               OR WS-MUE-TERM-DT(WS-SUB-1) = 0)
      *---                     CHECK UNITS AGAINST MUE THRESHOLD
                               IF WS-CLN-UNITS >
                                   WS-MUE-MAX-UNITS(WS-SUB-1)
                                   MOVE 'ED0310' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'F' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   STRING 'MUE EXCEEDED: '
                                   WS-CLN-HCPCS-CD
                                   ' UNITS='
                                   WS-CLN-UNITS
                                   ' MAX='
                                   WS-MUE-MAX-UNITS(WS-SUB-1)
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                                   END-STRING
                                   MOVE WS-LINE-CTR TO
                                   WS-EDIT-ERR-LINE-NO(
                                   WS-EDIT-ERR-COUNT + 1)
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                                   SET WS-MUE-FAILED TO TRUE
                                   SET WS-CLAIM-IS-INVALID
                                       TO TRUE
                                   ADD 1 TO WS-ESTAT-MUE-ERRS
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *--- ADD-ON CODE VALIDATION (MUST HAVE PRIMARY CODE)
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   SET WS-NOT-ADDON-CODE TO TRUE
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-ADDON-COUNT
                       IF WS-CLN-HCPCS-CD =
                           WS-ADDON-CPT(WS-SUB-1)
                           SET WS-IS-ADDON-CODE TO TRUE
      *---                 SEARCH FOR PRIMARY CODE ON THE CLAIM
                           SET WS-WK-NOT-FOUND TO TRUE
                           PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                               UNTIL WS-SUB-2 >
                                   WS-CLT-LINE-COUNT
                               OR WS-WK-FOUND
                               IF WS-SUB-2 NOT = WS-LINE-CTR
                                   IF WS-CLT-LINE-DATA(WS-SUB-2)
                                       (1:5) =
                                       WS-ADDON-PRIMARY-CPT(
                                           WS-SUB-1)
                                       SET WS-WK-FOUND TO TRUE
                                   END-IF
                               END-IF
                           END-PERFORM
                           IF WS-WK-NOT-FOUND
                               MOVE 'ED0320' TO
                               WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE 'F' TO
                               WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                               STRING 'ADD-ON CODE '
                               WS-CLN-HCPCS-CD
                               ' REQUIRES PRIMARY '
                               WS-ADDON-PRIMARY-CPT(WS-SUB-1)
                               DELIMITED BY SIZE
                               INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               SET WS-CLAIM-IS-INVALID TO TRUE
                               ADD 1 TO WS-ESTAT-NCCI-ERRS
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *--- GLOBAL SURGERY PERIOD EDITS
      *--- CHECK IF ANY PROCEDURE IS WITHIN GLOBAL PERIOD OF A
      *--- PRIOR SURGERY BY THE SAME PROVIDER
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   EXEC SQL
                       SELECT CLM_HCPCS_CD, CLM_FROM_DOS,
                              GLB_GLOBAL_DAYS
                       INTO  :WS-PREV-SURG-CPT,
                             :WS-PREV-SURG-DOS,
                             :WS-PREV-SURG-GLOBAL
                       FROM  HCPS_CLAIMS_DB..CLAIM_LINE_HIST H
                       JOIN  HCPS_CLAIMS_DB..GLOBAL_SURGERY_PERIODS G
                       ON    H.CLM_HCPCS_CD = G.GLB_CPT_CD
                       WHERE H.CLM_PAT_MRN = :WS-CLM-PAT-MRN
                       AND   H.CLM_BILL_PROV_NPI =
                                 :WS-CLM-BILL-PROV-NPI
                       AND   H.CLM_STATUS <> 'VD'
                       AND   H.CLM_FROM_DOS < :WS-CLM-FROM-DOS
                       AND   G.GLB_GLOBAL_DAYS > 0
                   END-EXEC
                   IF WS-SQLCODE = 0
      *---             CALCULATE GLOBAL PERIOD END DATE
                       COMPUTE WS-DAYS-DIFF =
                           FUNCTION INTEGER-OF-DATE(
                               WS-CLM-FROM-DOS)
                         - FUNCTION INTEGER-OF-DATE(
                               WS-PREV-SURG-DOS)
                       IF WS-DAYS-DIFF <= WS-PREV-SURG-GLOBAL
      *---                 WITHIN GLOBAL PERIOD - CHECK MODIFIERS
      *                    78 = RETURN TO OR FOR RELATED PROC
      *                    79 = UNRELATED PROC IN POSTOP PERIOD
                           SET WS-IN-GLOBAL-PERIOD TO TRUE
                           IF WS-CLN-MOD-1 NOT = '78' AND
                              WS-CLN-MOD-1 NOT = '79' AND
                              WS-CLN-MOD-2 NOT = '78' AND
                              WS-CLN-MOD-2 NOT = '79'
                               MOVE 'ED0330' TO
                               WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO
                               WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                               STRING
                               'WITHIN GLOBAL PERIOD OF '
                               WS-PREV-SURG-CPT
                               ' DOS ' WS-PREV-SURG-DOS
                               ' (' WS-PREV-SURG-GLOBAL
                               ' DAY)'
                               DELIMITED BY SIZE
                               INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-NCCI-ERRS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      *--- SAME-DAY DUPLICATE PROCEDURE CHECK
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-LINE-CTR-2
                       FROM WS-LINE-CTR BY 1
                       UNTIL WS-LINE-CTR-2 > WS-CLT-LINE-COUNT
                       IF WS-LINE-CTR-2 NOT = WS-LINE-CTR
                           IF WS-CLT-LINE-DATA(WS-LINE-CTR-2)
                               (1:5) = WS-CLN-HCPCS-CD
      *---                     SAME CPT - CHECK IF MOD 76/77 PRESENT
      *                        76 = REPEAT PROCEDURE SAME PHYSICIAN
      *                        77 = REPEAT PROCEDURE DIFF PHYSICIAN
                               IF WS-CLN-MOD-1 NOT = '76' AND
                                  WS-CLN-MOD-1 NOT = '77' AND
                                  WS-CLN-MOD-2 NOT = '76' AND
                                  WS-CLN-MOD-2 NOT = '77'
                                   MOVE 'ED0335' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'W' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   STRING 'DUPLICATE PROC '
                                   WS-CLN-HCPCS-CD
                                   ' ON SAME CLAIM - NEED MOD 76/77'
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                                   END-STRING
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                                   ADD 1 TO WS-ESTAT-NCCI-ERRS
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *--- COMPONENT/COMPREHENSIVE CODE EDITING
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-BUNDLE-COUNT
                       IF WS-CLN-HCPCS-CD =
                           WS-BUNDLE-COMP-CPT(WS-SUB-1)
      *---                 COMPONENT CODE FOUND - CHECK IF
      *                    COMPREHENSIVE IS ALSO ON CLAIM
                           PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                               UNTIL WS-SUB-2 > WS-CLT-LINE-COUNT
                               IF WS-CLT-LINE-DATA(WS-SUB-2)
                                   (1:5) =
                                   WS-BUNDLE-COMPR-CPT(WS-SUB-1)
                                   MOVE 'ED0340' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'F' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   STRING 'COMPONENT '
                                   WS-CLN-HCPCS-CD
                                   ' BUNDLED INTO '
                                   WS-BUNDLE-COMPR-CPT(WS-SUB-1)
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                                   END-STRING
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                                   SET WS-CLAIM-IS-INVALID TO TRUE
                                   ADD 1 TO WS-ESTAT-NCCI-ERRS
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .

       2560-VALIDATE-GENDER-AGE-EDITS.
      *================================================================*
      * GENDER-SPECIFIC AND AGE-SPECIFIC PROCEDURE EDITS               *
      * ADDED MWILSON 2000-07-01                                       *
      *================================================================*

      *--- GENDER-SPECIFIC PROCEDURES
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-GENDER-PROC-COUNT
                       IF WS-CLN-HCPCS-CD =
                           WS-GENDER-CPT(WS-SUB-1)
      *---                 FOUND GENDER-SPECIFIC PROCEDURE
                           IF WS-CLM-PAT-GENDER NOT =
                               WS-GENDER-REQ(WS-SUB-1) AND
                              WS-CLM-PAT-GENDER NOT = 'U'
                               SET WS-GENDER-MISMATCH TO TRUE
                               MOVE 'ED0350' TO
                               WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE 'F' TO
                               WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                               STRING 'GENDER MISMATCH: '
                               WS-CLN-HCPCS-CD
                               ' REQUIRES GENDER '
                               WS-GENDER-REQ(WS-SUB-1)
                               ' PAT='
                               WS-CLM-PAT-GENDER
                               DELIMITED BY SIZE
                               INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               MOVE WS-LINE-CTR TO
                               WS-EDIT-ERR-LINE-NO(
                               WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               SET WS-CLAIM-IS-INVALID TO TRUE
                               ADD 1 TO WS-ESTAT-GENDER-AGE-ERRS
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *--- AGE-SPECIFIC PROCEDURES
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-AGE-PROC-COUNT
                       IF WS-CLN-HCPCS-CD =
                           WS-AGE-CPT(WS-SUB-1)
      *---                 FOUND AGE-SPECIFIC PROCEDURE
                           IF WS-PATIENT-AGE-YEARS <
                               WS-AGE-MIN(WS-SUB-1) OR
                              WS-PATIENT-AGE-YEARS >
                               WS-AGE-MAX(WS-SUB-1)
                               SET WS-AGE-MISMATCH TO TRUE
                               MOVE 'ED0355' TO
                               WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE 'F' TO
                               WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                               STRING 'AGE MISMATCH: '
                               WS-CLN-HCPCS-CD
                               ' AGE '
                               WS-AGE-MIN(WS-SUB-1)
                               '-'
                               WS-AGE-MAX(WS-SUB-1)
                               ' PAT='
                               WS-PATIENT-AGE-YEARS
                               DELIMITED BY SIZE
                               INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               SET WS-CLAIM-IS-INVALID TO TRUE
                               ADD 1 TO WS-ESTAT-GENDER-AGE-ERRS
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

      *--- MATERNITY EDITS - AGE RANGE AND DIAGNOSIS CONSISTENCY
           IF WS-CLM-PAT-GENDER = 'F'
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC
      *---         CHECK FOR MATERNITY PROCEDURE CODES (59XXX)
                   IF WS-CLN-HCPCS-CD >= '59000' AND
                      WS-CLN-HCPCS-CD <= '59899'
      *---             MATERNITY AGE CHECK (TYPICALLY 10-55)
                       IF WS-PATIENT-AGE-YEARS < 10 OR
                          WS-PATIENT-AGE-YEARS > 55
                           MOVE 'ED0360' TO
                           WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO
                           WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'MATERNITY PROC: PATIENT AGE OUTSIDE 10-55'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           ADD 1 TO WS-ESTAT-GENDER-AGE-ERRS
                       END-IF
      *---             CHECK FOR MATCHING MATERNITY DIAGNOSIS
      *                O00-O9A = PREGNANCY/CHILDBIRTH/PUERPERIUM
                       SET WS-WK-NOT-FOUND TO TRUE
                       IF WS-CLM-PRINC-DIAG(1:1) = 'O'
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                       IF WS-WK-NOT-FOUND
                           PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                               UNTIL WS-SUB-2 > 25
                               OR WS-WK-FOUND
                               IF WS-CLM-DIAG-CD(WS-SUB-2)
                                   (1:1) = 'O'
                                   SET WS-WK-FOUND TO TRUE
                               END-IF
                           END-PERFORM
                       END-IF
                       IF WS-WK-NOT-FOUND
                           MOVE 'ED0361' TO
                           WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO
                           WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'MATERNITY PROC WITHOUT PREGNANCY DIAGNOSIS'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

      *--- NEWBORN EDITS (PATIENT AGE = 0 DAYS)
           IF WS-PATIENT-AGE-DAYS <= 28
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC
      *---         NEWBORN CODES (99460-99465, 99477-99480)
                   IF WS-CLN-HCPCS-CD >= '99460' AND
                      WS-CLN-HCPCS-CD <= '99480'
      *---             CHECK FOR Z38 DIAGNOSIS (NEWBORN BORN IN HOSP)
                       SET WS-WK-NOT-FOUND TO TRUE
                       IF WS-CLM-PRINC-DIAG(1:3) = 'Z38'
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                       IF WS-WK-NOT-FOUND
                           PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                               UNTIL WS-SUB-2 > 25
                               OR WS-WK-FOUND
                               IF WS-CLM-DIAG-CD(WS-SUB-2)
                                   (1:3) = 'Z38'
                                   SET WS-WK-FOUND TO TRUE
                               END-IF
                           END-PERFORM
                       END-IF
                       IF WS-WK-NOT-FOUND
                           MOVE 'ED0365' TO
                           WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO
                           WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'NEWBORN PROC NEEDS Z38 LIVEBORN DIAGNOSIS'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .

       2570-VALIDATE-DIAGNOSIS-PROC-LINK.
      *================================================================*
      * MEDICAL NECESSITY: DIAGNOSIS MUST SUPPORT THE PROCEDURE        *
      * LCD/NCD COVERAGE DETERMINATION LOOKUPS                         *
      * ADDED MWILSON 2001-03-22                                       *
      *================================================================*

      *--- LCD / NCD COVERAGE DETERMINATION LOOKUPS
      *--- FOR MEDICARE CLAIMS, CHECK IF PROCEDURE IS COVERED
      *--- FOR THE GIVEN DIAGNOSIS UNDER AN LCD OR NCD
           IF WS-CLM-PAYER-MCARE
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC
                   IF WS-CLN-HCPCS-CD NOT = SPACES

      *---             SEARCH LCD/NCD TABLE FOR THIS PROCEDURE
                       SET WS-LCD-NOT-FOUND TO TRUE
                       SET WS-NCD-NOT-FOUND TO TRUE

                       PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                           UNTIL WS-SUB-1 > WS-LCD-ENTRY-COUNT
                           IF WS-CLN-HCPCS-CD =
                               WS-LCD-PROC-CD(WS-SUB-1)
                               AND WS-LCD-EFF-DT(WS-SUB-1)
                                   <= WS-CLM-FROM-DOS
                               AND (WS-LCD-TERM-DT(WS-SUB-1)
                                   >= WS-CLM-FROM-DOS
                                   OR WS-LCD-TERM-DT(WS-SUB-1) = 0)

                               IF WS-LCD-COV-TYPE(WS-SUB-1) = 'L'
                                   SET WS-LCD-FOUND TO TRUE
                               END-IF
                               IF WS-LCD-COV-TYPE(WS-SUB-1) = 'N'
                                   SET WS-NCD-FOUND TO TRUE
                               END-IF

      *---                     CHECK IF CLAIM DIAGNOSIS IS IN THE
      *                        LCD/NCD COVERED DIAGNOSIS LIST
                               SET WS-WK-NOT-FOUND TO TRUE
                               PERFORM VARYING WS-SUB-2
                                   FROM 1 BY 1
                                   UNTIL WS-SUB-2 > 20
                                   OR WS-WK-FOUND
                                   IF WS-LCD-DIAG-CD(WS-SUB-1,
                                       WS-SUB-2)
                                       NOT = SPACES
      *---                             CHECK PRINCIPAL DIAG
                                       IF WS-CLM-PRINC-DIAG(1:
                                           FUNCTION LENGTH(
                                           FUNCTION TRIM(
                                           WS-LCD-DIAG-CD(
                                           WS-SUB-1, WS-SUB-2)
                                           TRAILING)))
                                           = WS-LCD-DIAG-CD(
                                           WS-SUB-1, WS-SUB-2)
                                           SET WS-WK-FOUND TO TRUE
                                       END-IF
      *---                             CHECK ALL ADDITIONAL DIAGS
                                       IF WS-WK-NOT-FOUND
                                       PERFORM VARYING WS-SUB-3
                                           FROM 1 BY 1
                                           UNTIL WS-SUB-3 > 25
                                           OR WS-WK-FOUND
                                           IF WS-CLM-DIAG-CD(
                                               WS-SUB-3)
                                               NOT = SPACES
                                               IF WS-CLM-DIAG-CD(
                                                   WS-SUB-3)
                                                   (1:3) =
                                                   WS-LCD-DIAG-CD(
                                                   WS-SUB-1,
                                                   WS-SUB-2)
                                                   (1:3)
                                                   SET WS-WK-FOUND
                                                       TO TRUE
                                               END-IF
                                           END-IF
                                       END-PERFORM
                                       END-IF
                                   END-IF
                               END-PERFORM

                               IF WS-WK-NOT-FOUND
                                   MOVE 'ED0370' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'W' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   STRING
                                   'LCD/NCD: DIAG DOES NOT SUPPORT '
                                   WS-CLN-HCPCS-CD
                                   ' PER '
                                   WS-LCD-COV-ID(WS-SUB-1)
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                                   END-STRING
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF

      *--- DIAGNOSIS-SPECIFIC PROCEDURE REQUIREMENTS
      *--- CERTAIN DIAGNOSIS CODES REQUIRE SPECIFIC PROCEDURES
           IF WS-CLM-PRINC-DIAG NOT = SPACES
               EXEC SQL
                   SELECT COUNT(*)
                   INTO  :WS-DB-ROW-COUNT
                   FROM  HCPS_CLAIMS_DB..DIAG_PROC_REQUIREMENTS
                   WHERE DPR_DIAG_CD = :WS-CLM-PRINC-DIAG
                   AND   DPR_EFF_DT <= :WS-CLM-FROM-DOS
                   AND   (DPR_TERM_DT >= :WS-CLM-FROM-DOS
                          OR DPR_TERM_DT = 0)
               END-EXEC
               IF WS-SQLCODE = 0 AND WS-DB-ROW-COUNT > 0
      *---         DIAGNOSIS HAS REQUIRED PROCEDURES - CHECK CLAIM
                   EXEC SQL
                       SELECT DPR_REQUIRED_PROC
                       INTO  :WS-WORK-STRING
                       FROM  HCPS_CLAIMS_DB..DIAG_PROC_REQUIREMENTS
                       WHERE DPR_DIAG_CD = :WS-CLM-PRINC-DIAG
                       AND   DPR_EFF_DT <= :WS-CLM-FROM-DOS
                       AND   (DPR_TERM_DT >= :WS-CLM-FROM-DOS
                              OR DPR_TERM_DT = 0)
                   END-EXEC
                   IF WS-SQLCODE = 0
                       SET WS-WK-NOT-FOUND TO TRUE
                       PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                           UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                           OR WS-WK-FOUND
                           MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                               TO WS-CLAIM-LINE-REC
                           IF WS-CLN-HCPCS-CD =
                               WS-WORK-STRING(1:5)
                               SET WS-WK-FOUND TO TRUE
                           END-IF
                       END-PERFORM
                       IF WS-WK-NOT-FOUND
                           MOVE 'ED0375' TO
                           WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                           MOVE 'I' TO
                           WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                           STRING 'DIAG '
                           WS-CLM-PRINC-DIAG
                           ' MAY REQUIRE PROC '
                           WS-WORK-STRING(1:5)
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                           WS-EDIT-ERR-COUNT + 1)
                           END-STRING
                           ADD 1 TO WS-EDIT-ERR-COUNT
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2580-VALIDATE-PLACE-OF-SERVICE.
      *================================================================*
      * PLACE OF SERVICE VS PROCEDURE CODE CONSISTENCY                 *
      * INPATIENT PROCEDURES REQUIRE INPATIENT POS                    *
      * CERTAIN PROCEDURES ONLY VALID IN ASC/HOSPITAL OUTPATIENT      *
      *================================================================*

           IF WS-CLM-PROFESSIONAL
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC
                   IF WS-CLN-HCPCS-CD NOT = SPACES AND
                      WS-CLN-PLACE-OF-SVC NOT = SPACES

      *---             INPATIENT PROCEDURES REQUIRE POS 21
      *                (SURGICAL CODES 10000-69999 IN INPATIENT
      *                 SETTING)
                       IF WS-CLN-HCPCS-CD >= '10000' AND
                          WS-CLN-HCPCS-CD <= '69999'
      *---                 MAJOR SURGICAL PROCEDURES
                           IF WS-CLN-HCPCS-CD >= '20000' AND
                              WS-CLN-HCPCS-CD <= '69999'
      *---                     CHECK IF INPATIENT-ONLY PROC
                               EXEC SQL
                                   SELECT COUNT(*)
                                   INTO  :WS-DB-ROW-COUNT
                                   FROM  HCPS_CLAIMS_DB..
                                         INPATIENT_ONLY_PROCS
                                   WHERE IOP_CPT_CD =
                                         :WS-CLN-HCPCS-CD
                                   AND   IOP_EFF_DT <=
                                         :WS-CLM-FROM-DOS
                                   AND   (IOP_TERM_DT >=
                                         :WS-CLM-FROM-DOS
                                         OR IOP_TERM_DT = 0)
                               END-EXEC
                               IF WS-SQLCODE = 0 AND
                                  WS-DB-ROW-COUNT > 0
                                   IF WS-CLN-PLACE-OF-SVC
                                       NOT = '21'
                                       MOVE 'ED0380' TO
                                       WS-EDIT-ERR-CD(
                                       WS-EDIT-ERR-COUNT + 1)
                                       MOVE 'F' TO
                                       WS-EDIT-ERR-SEV(
                                       WS-EDIT-ERR-COUNT + 1)
                                       STRING
                                       'INPATIENT-ONLY PROC '
                                       WS-CLN-HCPCS-CD
                                       ' REQUIRES POS 21'
                                       DELIMITED BY SIZE
                                       INTO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                                       END-STRING
                                       ADD 1 TO WS-EDIT-ERR-COUNT
                                       SET WS-CLAIM-IS-INVALID
                                           TO TRUE
                                       ADD 1 TO WS-ESTAT-POS-ERRS
                                   END-IF
                               END-IF
                           END-IF
                       END-IF

      *---             ASC-ELIGIBLE PROCEDURES WITH POS 24
                       IF WS-CLN-PLACE-OF-SVC = '24'
                           EXEC SQL
                               SELECT COUNT(*)
                               INTO  :WS-DB-ROW-COUNT
                               FROM  HCPS_CLAIMS_DB..ASC_ELIGIBLE_PROCS
                               WHERE ASC_CPT_CD = :WS-CLN-HCPCS-CD
                               AND   ASC_EFF_DT <= :WS-CLM-FROM-DOS
                               AND   (ASC_TERM_DT >= :WS-CLM-FROM-DOS
                                      OR ASC_TERM_DT = 0)
                           END-EXEC
                           IF WS-SQLCODE = 0 AND
                              WS-DB-ROW-COUNT = 0
                               MOVE 'ED0381' TO
                               WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO
                               WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                               STRING 'PROC '
                               WS-CLN-HCPCS-CD
                               ' NOT ON ASC ELIGIBLE LIST'
                               DELIMITED BY SIZE
                               INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-POS-ERRS
                           END-IF
                       END-IF

      *---             E/M CODES IN OFFICE (POS 11) VS FACILITY
                       IF WS-CLN-HCPCS-CD >= '99201' AND
                          WS-CLN-HCPCS-CD <= '99499'
      *---                 OFFICE E/M WITH FACILITY POS
                           IF WS-CLN-PLACE-OF-SVC = '21' OR
                              WS-CLN-PLACE-OF-SVC = '22' OR
                              WS-CLN-PLACE-OF-SVC = '23'
      *---                     INFORMATIONAL - DIFFERENT FEE SCHEDULE
                               MOVE 'ED0382' TO
                               WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE 'I' TO
                               WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                               MOVE
                       'E/M CODE WITH FACILITY POS - FACILITY RATE'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .

       2590-VALIDATE-MODIFIER-LOGIC.
      *================================================================*
      * MODIFIER VALIDATION ENGINE                                     *
      * VALIDATES ALL MODIFIERS FOR CONSISTENCY AND RULES              *
      * ORIGINAL REWRITE KPATEL 1998-05-01                             *
      * X-MODIFIER EXPANSION PWRIGHT 2024-01-15                       *
      *================================================================*

           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

      *---     RESET MODIFIER FLAGS FOR EACH LINE
               INITIALIZE WS-MODIFIER-TABLE
               MOVE 0 TO WS-MOD-COUNT

      *---     CATALOG ALL MODIFIERS ON THIS LINE
               IF WS-CLN-MOD-1 NOT = SPACES
                   ADD 1 TO WS-MOD-COUNT
                   EVALUATE WS-CLN-MOD-1
                       WHEN '25' SET WS-MOD-25-PRESENT TO TRUE
                       WHEN '26' SET WS-MOD-26-PRESENT TO TRUE
                       WHEN 'TC' SET WS-MOD-TC-PRESENT TO TRUE
                       WHEN '50' SET WS-MOD-50-PRESENT TO TRUE
                       WHEN '51' SET WS-MOD-51-PRESENT TO TRUE
                       WHEN '59' SET WS-MOD-59-PRESENT TO TRUE
                       WHEN '76' SET WS-MOD-76-PRESENT TO TRUE
                       WHEN '77' SET WS-MOD-77-PRESENT TO TRUE
                       WHEN '78' SET WS-MOD-78-PRESENT TO TRUE
                       WHEN '79' SET WS-MOD-79-PRESENT TO TRUE
                       WHEN 'RT' SET WS-MOD-RT-PRESENT TO TRUE
                       WHEN 'LT' SET WS-MOD-LT-PRESENT TO TRUE
                       WHEN 'XE' SET WS-MOD-XE-PRESENT TO TRUE
                       WHEN 'XP' SET WS-MOD-XP-PRESENT TO TRUE
                       WHEN 'XS' SET WS-MOD-XS-PRESENT TO TRUE
                       WHEN 'XU' SET WS-MOD-XU-PRESENT TO TRUE
                       WHEN '95' SET WS-MOD-95-PRESENT TO TRUE
                       WHEN 'GQ' SET WS-MOD-GQ-PRESENT TO TRUE
                       WHEN 'GT' SET WS-MOD-GT-PRESENT TO TRUE
                       WHEN 'FQ' SET WS-MOD-FQ-PRESENT TO TRUE
                   END-EVALUATE
               END-IF
               IF WS-CLN-MOD-2 NOT = SPACES
                   ADD 1 TO WS-MOD-COUNT
                   EVALUATE WS-CLN-MOD-2
                       WHEN '25' SET WS-MOD-25-PRESENT TO TRUE
                       WHEN '26' SET WS-MOD-26-PRESENT TO TRUE
                       WHEN 'TC' SET WS-MOD-TC-PRESENT TO TRUE
                       WHEN '50' SET WS-MOD-50-PRESENT TO TRUE
                       WHEN '51' SET WS-MOD-51-PRESENT TO TRUE
                       WHEN '59' SET WS-MOD-59-PRESENT TO TRUE
                       WHEN '76' SET WS-MOD-76-PRESENT TO TRUE
                       WHEN '77' SET WS-MOD-77-PRESENT TO TRUE
                       WHEN '78' SET WS-MOD-78-PRESENT TO TRUE
                       WHEN '79' SET WS-MOD-79-PRESENT TO TRUE
                       WHEN 'RT' SET WS-MOD-RT-PRESENT TO TRUE
                       WHEN 'LT' SET WS-MOD-LT-PRESENT TO TRUE
                       WHEN 'XE' SET WS-MOD-XE-PRESENT TO TRUE
                       WHEN 'XP' SET WS-MOD-XP-PRESENT TO TRUE
                       WHEN 'XS' SET WS-MOD-XS-PRESENT TO TRUE
                       WHEN 'XU' SET WS-MOD-XU-PRESENT TO TRUE
                       WHEN '95' SET WS-MOD-95-PRESENT TO TRUE
                       WHEN 'GQ' SET WS-MOD-GQ-PRESENT TO TRUE
                       WHEN 'GT' SET WS-MOD-GT-PRESENT TO TRUE
                       WHEN 'FQ' SET WS-MOD-FQ-PRESENT TO TRUE
                   END-EVALUATE
               END-IF

      *---     MODIFIER 25: SIGNIFICANT SEPARATELY IDENTIFIABLE E/M
      *        ONLY VALID ON E/M CODES (99201-99499)
               IF WS-MOD-25-PRESENT
                   IF WS-CLN-HCPCS-CD < '99201' OR
                      WS-CLN-HCPCS-CD > '99499'
                       MOVE 'ED0400' TO
                       WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO
                       WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                       MOVE 'MOD 25 ONLY VALID ON E/M CODES'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       MOVE WS-LINE-CTR TO
                           WS-EDIT-ERR-LINE-NO(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-MODIFIER-ERRS
                   END-IF
               END-IF

      *---     MODIFIER 26/TC: PROFESSIONAL/TECHNICAL COMPONENT
      *        CANNOT HAVE BOTH 26 AND TC ON SAME LINE
               IF WS-MOD-26-PRESENT AND WS-MOD-TC-PRESENT
                   MOVE 'ED0401' TO
                   WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO
                   WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                   MOVE 'MOD 26 AND TC CANNOT BE ON SAME LINE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-MODIFIER-ERRS
               END-IF

      *---     MODIFIER 50: BILATERAL - CANNOT ALSO HAVE RT/LT
               IF WS-MOD-50-PRESENT
                   IF WS-MOD-RT-PRESENT OR WS-MOD-LT-PRESENT
                       MOVE 'ED0402' TO
                       WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO
                       WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'MOD 50 (BILATERAL) CONFLICTS WITH RT/LT'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-MODIFIER-ERRS
                   END-IF
               END-IF

      *---     MODIFIER 76 AND 77 CANNOT BOTH BE ON SAME LINE
               IF WS-MOD-76-PRESENT AND WS-MOD-77-PRESENT
                   MOVE 'ED0403' TO
                   WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO
                   WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                   MOVE
               'MOD 76 (SAME MD) AND 77 (DIFF MD) CONFLICT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-MODIFIER-ERRS
               END-IF

      *---     MODIFIER 78 AND 79 CANNOT BOTH BE ON SAME LINE
               IF WS-MOD-78-PRESENT AND WS-MOD-79-PRESENT
                   MOVE 'ED0404' TO
                   WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO
                   WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                   MOVE
               'MOD 78 (RELATED) AND 79 (UNRELATED) CONFLICT'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-MODIFIER-ERRS
               END-IF

      *---     MODIFIER 59 AND X-MODIFIERS SHOULD NOT COEXIST
      *        (X-MODIFIERS ARE MORE SPECIFIC REPLACEMENTS FOR 59)
               IF WS-MOD-59-PRESENT
                   IF WS-MOD-XE-PRESENT OR WS-MOD-XP-PRESENT OR
                      WS-MOD-XS-PRESENT OR WS-MOD-XU-PRESENT
                       MOVE 'ED0405' TO
                       WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO
                       WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'MOD 59 SHOULD NOT BE USED WITH X-MODIFIERS'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-MODIFIER-ERRS
                   END-IF
               END-IF

      *---     MODIFIER 95: SYNCHRONOUS TELEHEALTH
      *        MUST BE WITH TELEHEALTH-ELIGIBLE CPT
               IF WS-MOD-95-PRESENT
                   SET WS-WK-NOT-FOUND TO TRUE
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-TELE-PROC-COUNT
                       OR WS-WK-FOUND
                       IF WS-CLN-HCPCS-CD =
                           WS-TELE-CPT-CD(WS-SUB-1)
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                   END-PERFORM
                   IF WS-WK-NOT-FOUND
                       MOVE 'ED0406' TO
                       WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO
                       WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
                       STRING 'MOD 95: '
                       WS-CLN-HCPCS-CD
                       ' NOT TELEHEALTH ELIGIBLE'
                       DELIMITED BY SIZE
                       INTO WS-EDIT-ERR-MSG(
                       WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-MODIFIER-ERRS
                   END-IF
               END-IF
           END-PERFORM
           .

       2600-VALIDATE-PROCEDURE-CODES.
      *================================================================*
      * VALIDATE HCPCS/CPT AND REVENUE CODES                          *
      *================================================================*
      *--- VALIDATE CLAIM LINES EXIST
           IF WS-CLT-LINE-COUNT = 0
               MOVE 'ED0050' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'CLAIM MUST HAVE AT LEAST ONE SERVICE LINE'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               SET WS-HAS-FATAL-EDIT TO TRUE
               ADD 1 TO WS-ESTAT-PROC-ERRS
           END-IF

      *--- VALIDATE EACH CLAIM LINE
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT

               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

      *---     EDIT 51: REVENUE CODE FOR INSTITUTIONAL
               IF WS-CLM-INSTITUTIONAL
                   IF WS-CLN-REV-CD = SPACES OR LOW-VALUES
                       MOVE 'ED0051' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'REVENUE CODE REQUIRED FOR INST CLAIM'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       MOVE WS-LINE-CTR TO
                           WS-EDIT-ERR-LINE-NO(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-PROC-ERRS
                   END-IF
               END-IF

      *---     EDIT 52: HCPCS/CPT FOR PROFESSIONAL
               IF WS-CLM-PROFESSIONAL
                   IF WS-CLN-HCPCS-CD = SPACES OR LOW-VALUES
                       MOVE 'ED0052' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'HCPCS/CPT CODE REQUIRED FOR PROF CLAIM'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       MOVE WS-LINE-CTR TO
                           WS-EDIT-ERR-LINE-NO(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-PROC-ERRS
                   END-IF
               END-IF

      *---     EDIT 53: PLACE OF SERVICE FOR PROFESSIONAL
               IF WS-CLM-PROFESSIONAL
                   IF WS-CLN-PLACE-OF-SVC = SPACES OR LOW-VALUES
                       MOVE 'ED0053' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'PLACE OF SERVICE CODE IS MISSING'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF

      *---     EDIT 54: CHARGE AMOUNT MUST BE POSITIVE
               IF WS-CLN-CHARGE-AMT <= ZEROS
                   MOVE 'ED0054' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'LINE CHARGE AMOUNT MUST BE > ZERO'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   MOVE WS-LINE-CTR TO
                       WS-EDIT-ERR-LINE-NO(
                           WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-PROC-ERRS
               END-IF

      *---     EDIT 55: UNITS MUST BE POSITIVE
               IF WS-CLN-UNITS <= ZEROS
                   MOVE 'ED0055' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'UNITS OF SERVICE MUST BE > ZERO'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-PROC-ERRS
               END-IF

      *---     EDIT 56: DIAGNOSIS POINTER VALIDATION
               PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                   UNTIL WS-SUB-2 > 4
                   IF WS-CLN-DIAG-PTR(WS-SUB-2) > 0
                       IF WS-CLN-DIAG-PTR(WS-SUB-2) > 25
                           MOVE 'ED0056' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'DIAG POINTER EXCEEDS MAX DIAGS'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       2700-VALIDATE-FINANCIAL-INFO.
      *================================================================*
      * VALIDATE FINANCIAL AMOUNTS AND TOTALS                          *
      *================================================================*
      *--- EDIT 60: TOTAL CHARGES MUST BE PRESENT
           IF WS-CLM-TOTAL-CHARGES <= ZEROS
               MOVE 'ED0060' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'TOTAL CHARGES MUST BE GREATER THAN ZERO'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-FINANCIAL-ERRS
           END-IF

      *--- EDIT 61: VERIFY LINE CHARGES SUM TO HEADER TOTAL
           MOVE ZEROS TO WS-TOTAL-CHARGES-CALC
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC
               ADD WS-CLN-CHARGE-AMT TO WS-TOTAL-CHARGES-CALC
           END-PERFORM

           IF WS-TOTAL-CHARGES-CALC NOT = WS-CLM-TOTAL-CHARGES
               MOVE 'ED0061' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'LINE CHARGES DO NOT SUM TO HEADER TOTAL'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-FINANCIAL-ERRS
           END-IF

      *--- EDIT 62: TOTAL CHARGES REASONABILITY CHECK
           IF WS-CLM-TOTAL-CHARGES > 9999999.99
               MOVE 'ED0062' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT
                   + 1)
               MOVE 'W' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
               MOVE 'TOTAL CHARGES EXCEED REASONABILITY THRESHOLD'
                   TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               ADD 1 TO WS-EDIT-ERR-COUNT
               ADD 1 TO WS-ESTAT-FINANCIAL-ERRS
           END-IF
           .

       2800-CHECK-ELIGIBILITY.
      *================================================================*
      * VERIFY MEMBER ELIGIBILITY FOR DATE OF SERVICE                  *
      *================================================================*
           EXEC SQL
               SELECT ELG_MEMBER_ID,
                      ELG_PLAN_CD,
                      ELG_EFF_DT,
                      ELG_TERM_DT,
                      ELG_COV_STATUS,
                      ELG_PLAN_TYPE,
                      ELG_NETWORK_CD
               INTO  :WS-ELG-MEMBER-ID,
                     :WS-ELG-PLAN-CD,
                     :WS-ELG-EFF-DT,
                     :WS-ELG-TERM-DT,
                     :WS-ELG-COV-STATUS,
                     :WS-ELG-PLAN-TYPE,
                     :WS-ELG-NETWORK-CD
               FROM  HCPS_CLAIMS_DB..ELIGIBILITY
               WHERE ELG_MEMBER_ID = :WS-CLM-SUB-MEMBER-ID
               AND   ELG_PLAN_CD   = :WS-CLM-PAYER-CD
               AND   ELG_EFF_DT   <= :WS-CLM-FROM-DOS
               AND   (ELG_TERM_DT >= :WS-CLM-FROM-DOS
                      OR ELG_TERM_DT = 0)
               AND   ELG_COV_STATUS = 'A'
           END-EXEC

           EVALUATE WS-SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE 'ED0070' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'MEMBER NOT ELIGIBLE ON DATE OF SERVICE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   SET WS-HAS-FATAL-EDIT TO TRUE
                   ADD 1 TO WS-ESTAT-ELIG-ERRS
               WHEN OTHER
                   MOVE 'ELIGIBILITY LOOKUP' TO WS-DB-OPERATION
                   PERFORM 8100-DATABASE-ERROR
           END-EVALUATE
           .

       2900-CHECK-DUPLICATE-CLAIM.
      *================================================================*
      * CHECK FOR DUPLICATE CLAIM SUBMISSION                           *
      *================================================================*
           SET WS-NOT-DUPLICATE TO TRUE

           EXEC SQL
               SELECT COUNT(*)
               INTO  :WS-DB-ROW-COUNT
               FROM  HCPS_CLAIMS_DB..CLAIM_HEADER
               WHERE CLM_PAT_MRN      = :WS-CLM-PAT-MRN
               AND   CLM_BILL_PROV_NPI = :WS-CLM-BILL-PROV-NPI
               AND   CLM_FROM_DOS     = :WS-CLM-FROM-DOS
               AND   CLM_THRU_DOS     = :WS-CLM-THRU-DOS
               AND   CLM_TOTAL_CHARGES = :WS-CLM-TOTAL-CHARGES
               AND   CLM_STATUS       <> 'VD'
               AND   CLM_NUMBER       <> :WS-CLM-NUMBER
           END-EXEC

           IF WS-SQLCODE = 0
               IF WS-DB-ROW-COUNT > 0
                   SET WS-IS-DUPLICATE TO TRUE
                   MOVE 'ED0080' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'POTENTIAL DUPLICATE CLAIM DETECTED'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-DUP-ERRS
               END-IF
           END-IF
           .

       3150-CHECK-TIMELY-FILING-BY-PAYER.
      *================================================================*
      * VERIFY CLAIM IS WITHIN TIMELY FILING LIMIT BY PAYER TYPE      *
      * MEDICARE: 365 DAYS FROM DOS                                    *
      * MEDICAID: VARIES BY STATE (90-365 DAYS)                       *
      * COMMERCIAL: PER CONTRACT (90, 120, 180, 365)                  *
      * WORKERS COMP: STATE-SPECIFIC                                   *
      * TRICARE: 365 DAYS                                              *
      * VA: SPECIFIC RULES                                             *
      *================================================================*
           IF WS-CLM-FROM-DOS NOT = ZEROS
               COMPUTE WS-DAYS-DIFF =
                   FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE-8)
                 - FUNCTION INTEGER-OF-DATE(WS-CLM-FROM-DOS)
           ELSE
               MOVE 0 TO WS-DAYS-DIFF
           END-IF

      *--- DETERMINE TIMELY FILING LIMIT BY PAYER TYPE
           EVALUATE TRUE
               WHEN WS-CLM-PAYER-MCARE
      *---         MEDICARE: 365 CALENDAR DAYS FROM DOS
      *            (OR 27 MONTHS FOR CERTAIN SITUATIONS)
                   MOVE 365 TO WS-TIMELY-FILE-DAYS
               WHEN WS-CLM-PAYER-MCAID
      *---         MEDICAID: VARIES BY STATE
                   MOVE 365 TO WS-TIMELY-FILE-DAYS
                   MOVE WS-CLM-PAT-STATE TO WS-STATE-CD-WORK
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-STATE-TF-COUNT
                       IF WS-STATE-CD-WORK =
                           WS-STATE-TF-CD(WS-SUB-1)
                           MOVE WS-STATE-TF-DAYS(WS-SUB-1)
                               TO WS-TIMELY-FILE-DAYS
                       END-IF
                   END-PERFORM
               WHEN WS-CLM-PAYER-WC
      *---         WORKERS COMP: STATE-SPECIFIC, LOOK UP
                   EXEC SQL
                       SELECT WC_TIMELY_FILE_DAYS
                       INTO  :WS-TIMELY-FILE-DAYS
                       FROM  HCPS_CLAIMS_DB..WC_STATE_CONFIG
                       WHERE WC_STATE_CD = :WS-CLM-PAT-STATE
                   END-EXEC
                   IF WS-SQLCODE = 100
                       MOVE 365 TO WS-TIMELY-FILE-DAYS
                   END-IF
               WHEN WS-CLM-PAYER-TRICARE
      *---         TRICARE: 365 DAYS FROM DOS
                   MOVE 365 TO WS-TIMELY-FILE-DAYS
               WHEN WS-CLM-PAYER-VA
      *---         VA: 180 DAYS STANDARD
                   MOVE 180 TO WS-TIMELY-FILE-DAYS
               WHEN OTHER
      *---         COMMERCIAL: LOOKUP PAYER-SPECIFIC LIMIT
                   EXEC SQL
                       SELECT PAYER_TIMELY_FILE_DAYS
                       INTO  :WS-TIMELY-FILE-DAYS
                       FROM  HCPS_CLAIMS_DB..PAYER_CONFIG
                       WHERE PAYER_CD = :WS-CLM-PAYER-CD
                   END-EXEC
                   IF WS-SQLCODE = 100
                       MOVE 365 TO WS-TIMELY-FILE-DAYS
                   END-IF
           END-EVALUATE

      *--- CHECK FOR TIMELY FILING EXCEPTIONS
      *--- EXCEPTION: RETROACTIVE ELIGIBILITY
           IF WS-DAYS-DIFF > WS-TIMELY-FILE-DAYS
               EXEC SQL
                   SELECT COUNT(*)
                   INTO  :WS-DB-ROW-COUNT
                   FROM  HCPS_CLAIMS_DB..RETRO_ELIGIBILITY
                   WHERE RETRO_MEMBER_ID = :WS-CLM-SUB-MEMBER-ID
                   AND   RETRO_EFF_DT <= :WS-CLM-FROM-DOS
                   AND   RETRO_PROCESSED_DT >=
                         :WS-CLM-FROM-DOS
               END-EXEC
               IF WS-SQLCODE = 0 AND WS-DB-ROW-COUNT > 0
      *---         RETROACTIVE ELIGIBILITY - EXTEND FILING LIMIT
      *            TO 365 DAYS FROM RETRO ELIGIBILITY DATE
                   MOVE 0 TO WS-DAYS-DIFF
               END-IF
           END-IF

      *--- EXCEPTION: COB/SUBROGATION DELAYS
           IF WS-DAYS-DIFF > WS-TIMELY-FILE-DAYS
               IF WS-CLM-HAS-COB
                   IF WS-CLM-SECONDARY OR WS-CLM-TERTIARY
      *---             SECONDARY/TERTIARY: ALLOW 365 DAYS FROM
      *                PRIMARY PAYER ADJUDICATION DATE
                       MOVE 0 TO WS-DAYS-DIFF
                   END-IF
               END-IF
           END-IF

      *--- APPLY TIMELY FILING CHECK
           IF WS-DAYS-DIFF > WS-TIMELY-FILE-DAYS
               SET WS-NOT-TIMELY TO TRUE
               MOVE 'ED0090' TO WS-EDIT-ERR-CD(
                   WS-EDIT-ERR-COUNT + 1)
               MOVE 'F' TO WS-EDIT-ERR-SEV(
                   WS-EDIT-ERR-COUNT + 1)
               STRING 'CLAIM EXCEEDS TIMELY FILING LIMIT ('
                   WS-TIMELY-FILE-DAYS ' DAYS)'
                   DELIMITED BY SIZE
                   INTO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
               END-STRING
               ADD 1 TO WS-EDIT-ERR-COUNT
               SET WS-CLAIM-IS-INVALID TO TRUE
               ADD 1 TO WS-ESTAT-TIMELY-ERRS
           END-IF
           .

       3250-VALIDATE-AUTHORIZATION-DETAIL.
      *================================================================*
      * DETAILED AUTHORIZATION VALIDATION                              *
      * UNITS, DIAGNOSIS, PROVIDER, DATE RANGE, PROCEDURE              *
      * CONCURRENT REVIEW, RETROSPECTIVE AUTH FOR EMERGENCY            *
      * ADDED 2024 PWRIGHT - EXPANSION OF ORIGINAL 3100               *
      *================================================================*
           SET WS-AUTH-NOT-REQUIRED TO TRUE

      *--- CHECK IF SERVICE REQUIRES AUTH
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

               EXEC SQL
                   SELECT COUNT(*)
                   INTO  :WS-DB-ROW-COUNT
                   FROM  HCPS_CLAIMS_DB..AUTH_REQUIRED_SVCS
                   WHERE ARS_PLAN_CD  = :WS-CLM-PAYER-CD
                   AND   (ARS_HCPCS_CD = :WS-CLN-HCPCS-CD
                          OR ARS_REV_CD = :WS-CLN-REV-CD)
                   AND   ARS_EFF_DT  <= :WS-CLM-FROM-DOS
                   AND   (ARS_TERM_DT >= :WS-CLM-FROM-DOS
                          OR ARS_TERM_DT = 0)
               END-EXEC

               IF WS-SQLCODE = 0 AND WS-DB-ROW-COUNT > 0
                   SET WS-AUTH-REQUIRED TO TRUE

                   IF WS-CLM-PRIOR-AUTH-NO = SPACES OR LOW-VALUES
      *---             CHECK FOR RETROSPECTIVE AUTH FOR EMERGENCY
                       IF WS-CLM-EMERGENCY
                           MOVE 'ED0102' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'I' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'EMERGENCY: RETRO AUTH MAY BE NEEDED'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-IS-RETRO-AUTH TO TRUE
                       ELSE
                           MOVE 'ED0100' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'PRIOR AUTH REQUIRED BUT NOT PROVIDED'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                           ADD 1 TO WS-ESTAT-AUTH-ERRS
                       END-IF
                   ELSE
      *---             AUTH NUMBER PROVIDED - FULL VALIDATION
                       EXEC SQL
                           SELECT AUTH_NUMBER, AUTH_STATUS,
                                  AUTH_EFF_DT, AUTH_TERM_DT,
                                  AUTH_UNITS_APPROVED,
                                  AUTH_UNITS_USED,
                                  AUTH_PROC_CD, AUTH_DIAG_CD,
                                  AUTH_PROV_NPI, AUTH_TYPE
                           INTO  :WS-AUTH-NUMBER-WK,
                                 :WS-AUTH-STATUS-WK,
                                 :WS-AUTH-EFF-DT-DB,
                                 :WS-AUTH-TERM-DT-DB,
                                 :WS-AUTH-UNITS-APPROVED,
                                 :WS-AUTH-UNITS-USED,
                                 :WS-AUTH-PROC-CD-DB,
                                 :WS-AUTH-DIAG-CD-DB,
                                 :WS-AUTH-PROV-NPI-DB,
                                 :WS-AUTH-TYPE-WK
                           FROM  HCPS_CLAIMS_DB..AUTHORIZATIONS
                           WHERE AUTH_NUMBER =
                                 :WS-CLM-PRIOR-AUTH-NO
                       END-EXEC

                       IF WS-SQLCODE = 100
                           MOVE 'ED0101' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'F' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                       'AUTH NUMBER NOT FOUND IN SYSTEM'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           SET WS-CLAIM-IS-INVALID TO TRUE
                           ADD 1 TO WS-ESTAT-AUTH-ERRS
                       END-IF

                       IF WS-SQLCODE = 0
      *---                 CHECK AUTH STATUS (A=APPROVED, D=DENIED,
      *                    P=PENDING, C=CANCELLED)
                           IF WS-AUTH-STATUS-WK NOT = 'AP'
                               MOVE 'ED0103' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'F' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               STRING 'AUTH STATUS IS '
                                   WS-AUTH-STATUS-WK
                                   ' - NOT APPROVED'
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               SET WS-CLAIM-IS-INVALID TO TRUE
                               ADD 1 TO WS-ESTAT-AUTH-ERRS
                           END-IF

      *---                 AUTH DATE RANGE VS DATES OF SERVICE
                           IF WS-CLM-FROM-DOS <
                               WS-AUTH-EFF-DT-DB OR
                              WS-CLM-FROM-DOS >
                               WS-AUTH-TERM-DT-DB
                               MOVE 'ED0104' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'F' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE
                       'DOS OUTSIDE AUTH DATE RANGE'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               SET WS-CLAIM-IS-INVALID TO TRUE
                               ADD 1 TO WS-ESTAT-AUTH-ERRS
                           END-IF

      *---                 AUTH UNITS VS BILLED UNITS
                           COMPUTE WS-AUTH-UNITS-REMAINING =
                               WS-AUTH-UNITS-APPROVED
                             - WS-AUTH-UNITS-USED
                           IF WS-CLN-UNITS >
                               WS-AUTH-UNITS-REMAINING
                               MOVE 'ED0105' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               STRING 'BILLED UNITS ('
                                   WS-CLN-UNITS
                                   ') EXCEED AUTH REMAINING ('
                                   WS-AUTH-UNITS-REMAINING ')'
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-AUTH-ERRS
                           END-IF

      *---                 AUTH PROCEDURE VS BILLED PROCEDURE
                           IF WS-AUTH-PROC-CD-DB NOT = SPACES
                               IF WS-CLN-HCPCS-CD NOT =
                                   WS-AUTH-PROC-CD-DB
                                   MOVE 'ED0106' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'W' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   STRING 'AUTH PROC '
                                   WS-AUTH-PROC-CD-DB
                                   ' <> BILLED '
                                   WS-CLN-HCPCS-CD
                                   DELIMITED BY SIZE
                                   INTO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                                   END-STRING
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                               END-IF
                           END-IF

      *---                 AUTH PROVIDER VS RENDERING PROVIDER
                           IF WS-AUTH-PROV-NPI-DB NOT = SPACES
                               IF WS-CLM-REND-PROV-NPI NOT =
                                   WS-AUTH-PROV-NPI-DB AND
                                  WS-CLM-BILL-PROV-NPI NOT =
                                   WS-AUTH-PROV-NPI-DB
                                   MOVE 'ED0107' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'W' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE
                       'AUTH PROVIDER DOES NOT MATCH CLAIM PROVIDER'
                                       TO WS-EDIT-ERR-MSG(
                                           WS-EDIT-ERR-COUNT + 1)
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                               END-IF
                           END-IF

      *---                 CONCURRENT REVIEW FOR INPATIENT
                           IF WS-CLM-INSTITUTIONAL
                               IF WS-AUTH-CONCURRENT
                                   MOVE 'ED0108' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'I' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE
                       'CONCURRENT REVIEW AUTH - CHECK LOS APPROVAL'
                                       TO WS-EDIT-ERR-MSG(
                                           WS-EDIT-ERR-COUNT + 1)
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

       3300-APPLY-PAYER-SPECIFIC-EDITS.
      *================================================================*
      * APPLY PAYER-SPECIFIC EDIT RULES FROM CONFIGURATION             *
      *================================================================*
      *--- MEDICARE-SPECIFIC EDITS
           IF WS-CLM-PAYER-MCARE
      *---     CHECK MEDICARE SECONDARY PAYER LOGIC
               IF WS-CLM-SECONDARY
                   IF WS-CLM-COB-AMT = ZEROS
                       MOVE 'ED0120' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'MSP: PRIMARY PAYER AMT REQUIRED'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                   END-IF
               END-IF

      *---     MEDICARE REQUIRES TAXONOMY CODE
               IF WS-CLM-BILL-PROV-TAXNMY = SPACES OR LOW-VALUES
                   MOVE 'ED0121' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'TAXONOMY CODE RECOMMENDED FOR MEDICARE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF

      *--- MEDICAID-SPECIFIC EDITS
           IF WS-CLM-PAYER-MCAID
      *---     REFERRAL REQUIRED FOR SOME MEDICAID PLANS
               IF WS-CLM-REFER-PROV-NPI = SPACES OR LOW-VALUES
                   MOVE 'ED0130' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'REFERRAL PROVIDER MAY BE REQUIRED'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF

      *--- WORKERS COMP SPECIFIC EDITS
           IF WS-CLM-PAYER-WC
               IF WS-CLM-E-CODE = SPACES OR LOW-VALUES
                   MOVE 'ED0140' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'EXTERNAL CAUSE CODE EXPECTED FOR WC'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF
           .

       3350-VALIDATE-COB-SECONDARY-CLAIM.
      *================================================================*
      * VALIDATE COORDINATION OF BENEFITS / SECONDARY CLAIM DATA       *
      * PRIMARY EOB INFORMATION, PAYMENT AMOUNTS, ADJUSTMENTS          *
      * ADDED RJONES 1997-03-01, EXPANDED 2023                        *
      *================================================================*
           IF WS-CLM-HAS-COB
               SET WS-HAS-COB TO TRUE

      *---     PAYER SEQUENCE MUST BE VALID
               IF NOT (WS-CLM-PRIMARY OR WS-CLM-SECONDARY
                       OR WS-CLM-TERTIARY)
                   MOVE 'ED0110' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'INVALID PAYER SEQUENCE FOR COB CLAIM'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
                   ADD 1 TO WS-ESTAT-COB-ERRS
               END-IF

      *---     SECONDARY/TERTIARY MUST HAVE COB AMOUNTS
               IF WS-CLM-SECONDARY OR WS-CLM-TERTIARY
                   IF WS-CLM-COB-AMT = ZEROS
                       MOVE 'ED0111' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'COB AMOUNT EXPECTED FOR NON-PRIMARY'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-COB-ERRS
                   END-IF

      *---         PRIMARY EOB INFORMATION REQUIRED
                   IF WS-CLM-COB-PRIMARY-PAYER = SPACES
                       MOVE 'ED0112' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'PRIMARY PAYER IDENTIFICATION EXPECTED'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-COB-ERRS
                   END-IF

      *---         PRIMARY PAID + PATIENT RESPONSIBILITY SHOULD
      *            RELATE TO TOTAL CHARGES
                   IF WS-CLM-COB-AMT > ZEROS AND
                      WS-CLM-COB-PAT-RESP > ZEROS
                       COMPUTE WS-WORK-AMOUNT =
                           WS-CLM-COB-AMT +
                           WS-CLM-COB-PAT-RESP +
                           WS-CLM-COB-ADJUSTMENT
                       IF WS-WORK-AMOUNT > WS-CLM-TOTAL-CHARGES
                           MOVE 'ED0113' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                   'COB AMOUNTS EXCEED TOTAL CHARGES'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           ADD 1 TO WS-ESTAT-COB-ERRS
                       END-IF
                   END-IF

      *---         ADJUSTMENT REASON CODES FROM PRIMARY
                   IF WS-CLM-COB-ADJ-REASON = SPACES
                       MOVE 'ED0114' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'I' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'PRIMARY ADJUSTMENT REASON CODE MISSING'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
           END-IF
           .

       3400-CHECK-NO-SURPRISES-ACT.
      *================================================================*
      * NO SURPRISES ACT (NSA) COMPLIANCE CHECKS                      *
      * EFFECTIVE JANUARY 1, 2022                                      *
      *================================================================*
           SET WS-NSA-APPLIES TO TRUE

      *--- CHECK IF EMERGENCY SERVICES
           IF WS-CLM-INSTITUTIONAL
               IF WS-CLM-EMERGENCY
      *---         EMERGENCY CLAIMS - NSA PROTECTIONS APPLY
                   PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                       UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                       MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                           TO WS-CLAIM-LINE-REC
      *---             CHECK IF OON PROVIDER
                       IF WS-CLN-PROV-OON
      *---                 MUST USE QUALIFYING PAYMENT AMOUNT
                           IF WS-CLN-ALLOWED-AMT = ZEROS
                               MOVE 'ED0150' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE
                            'NSA: QPA REQUIRED FOR OON EMERGENCY SVC'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-NSA-ERRS
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .

       3450-CHECK-CLAIM-CROSSOVER.
      *================================================================*
      * IDENTIFY AND VALIDATE CROSSOVER CLAIMS                         *
      * MEDICARE TO MEDICAID (MEDI-MEDI)                               *
      * MEDICARE TO SUPPLEMENTAL (MEDIGAP)                             *
      * AUTO-CROSSOVER VS MANUAL CROSSOVER                             *
      * ADDED MTHOMAS 2023-06-01                                       *
      *================================================================*

      *--- CHECK FOR AUTO-CROSSOVER CLAIMS
           IF WS-CLM-PAYER-MCARE AND WS-CLM-PRIMARY
      *---     LOOK FOR SECONDARY COVERAGE THAT QUALIFIES FOR XOVER
               EXEC SQL
                   SELECT ELG_PLAN_CD, ELG_PLAN_TYPE
                   INTO  :WS-COB-PRIMARY-PAYER,
                         :WS-WORK-STRING
                   FROM  HCPS_CLAIMS_DB..ELIGIBILITY
                   WHERE ELG_MEMBER_ID = :WS-CLM-SUB-MEMBER-ID
                   AND   ELG_COV_STATUS = 'A'
                   AND   ELG_EFF_DT <= :WS-CLM-FROM-DOS
                   AND   (ELG_TERM_DT >= :WS-CLM-FROM-DOS
                          OR ELG_TERM_DT = 0)
                   AND   ELG_PLAN_CD <> :WS-CLM-PAYER-CD
                   AND   (ELG_PLAN_TYPE = 'MCAID'
                          OR ELG_PLAN_TYPE = 'SUPPL')
               END-EXEC
               IF WS-SQLCODE = 0
                   SET WS-IS-CROSSOVER TO TRUE
      *---         DETERMINE CROSSOVER TYPE
                   IF WS-WORK-STRING(1:5) = 'MCAID'
                       SET WS-COB-MCARE-TO-MCAID TO TRUE
                       MOVE 'ED0450' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'I' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'MEDICARE/MEDICAID CROSSOVER IDENTIFIED'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
                   IF WS-WORK-STRING(1:5) = 'SUPPL'
                       SET WS-COB-MCARE-TO-SUPP TO TRUE
                       MOVE 'ED0451' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'I' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'MEDICARE/SUPPLEMENT CROSSOVER IDENTIFIED'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                   END-IF
               END-IF
           END-IF

      *--- VALIDATE CROSSOVER DATA INTEGRITY
           IF WS-IS-CROSSOVER
      *---     WRITE CROSSOVER RECORD FOR DOWNSTREAM PROCESSING
               IF WS-XOVR-STATUS = '00'
                   WRITE CROSSOVER-OUTPUT-REC
                       FROM WS-CLAIM-HEADER-REC
                   IF WS-XOVR-STATUS NOT = '00'
                       DISPLAY 'HCCLMVAL - XOVER WRITE FAILED'
                   END-IF
               END-IF
           END-IF
           .

       3500-CHECK-OPIOID-EDITS.
      *================================================================*
      * OPIOID PRESCRIPTION MONITORING EDITS                           *
      * ADDED 2018 PER STATE/FEDERAL REQUIREMENTS                     *
      *================================================================*
           IF WS-CLM-PHARMACY
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC

      *---         CHECK IF NDC IS ON OPIOID LIST
                   IF WS-CLN-NDC-CD NOT = SPACES
                       EXEC SQL
                           SELECT COUNT(*)
                           INTO  :WS-DB-ROW-COUNT
                           FROM  HCPS_CLAIMS_DB..OPIOID_NDC_LIST
                           WHERE NDC_CD = :WS-CLN-NDC-CD
                       END-EXEC

                       IF WS-DB-ROW-COUNT > 0
                           SET WS-IS-OPIOID-RX TO TRUE

      *---                 CHECK DAYS SUPPLY > 7 FOR INITIAL
                           IF WS-CLN-UNITS > 7
                               MOVE 'ED0160' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE
                            'OPIOID RX EXCEEDS 7-DAY INITIAL SUPPLY'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-OPIOID-ERRS
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .

       3550-VALIDATE-TELEHEALTH-EDITS.
      *================================================================*
      * TELEHEALTH EDITS                                               *
      * ORIGINATING SITE, DISTANT SITE, POS 02, MODIFIER 95           *
      * AUDIO-ONLY TELEHEALTH, STATE-SPECIFIC RULES                   *
      * ORIGINAL MWILSON 2002-09-01, EXPANDED 2019, 2021              *
      *================================================================*

           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

      *---     DETECT TELEHEALTH INDICATORS
               IF WS-CLN-PLACE-OF-SVC = '02' OR
                  WS-CLN-PLACE-OF-SVC = '10' OR
                  WS-CLN-MOD-1 = '95' OR
                  WS-CLN-MOD-2 = '95' OR
                  WS-CLN-MOD-1 = 'GT' OR
                  WS-CLN-MOD-2 = 'GT' OR
                  WS-CLN-MOD-1 = 'GQ' OR
                  WS-CLN-MOD-2 = 'GQ' OR
                  WS-CLN-MOD-1 = 'FQ' OR
                  WS-CLN-MOD-2 = 'FQ'

                   SET WS-IS-TELEHEALTH TO TRUE

      *---         CHECK IF PROCEDURE IS TELEHEALTH ELIGIBLE
                   SET WS-WK-NOT-FOUND TO TRUE
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-TELE-PROC-COUNT
                       OR WS-WK-FOUND
                       IF WS-CLN-HCPCS-CD =
                           WS-TELE-CPT-CD(WS-SUB-1)
                           SET WS-WK-FOUND TO TRUE
                       END-IF
                   END-PERFORM
                   IF WS-WK-NOT-FOUND
                       MOVE 'ED0500' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'F' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       STRING 'PROC '
                           WS-CLN-HCPCS-CD
                           ' NOT TELEHEALTH ELIGIBLE'
                           DELIMITED BY SIZE
                           INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       END-STRING
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       SET WS-CLAIM-IS-INVALID TO TRUE
                       ADD 1 TO WS-ESTAT-TELEHLTH-ERRS
                   END-IF

      *---         POS 02 AND MODIFIER 95 VALIDATION
      *            POS 02 = TELEHEALTH OTHER THAN HOME
      *            POS 10 = TELEHEALTH IN PATIENT HOME
                   IF WS-CLN-PLACE-OF-SVC = '02'
      *---             POS 02 SHOULD NOT ALSO HAVE MOD 95
                       IF WS-CLN-MOD-1 = '95' OR
                          WS-CLN-MOD-2 = '95'
                           MOVE 'ED0501' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                   'POS 02 AND MOD 95 SHOULD NOT BOTH BE USED'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           ADD 1 TO WS-ESTAT-TELEHLTH-ERRS
                       END-IF
                   END-IF

      *---         AUDIO-ONLY TELEHEALTH EDITS
      *            MODIFIER FQ = AUDIO-ONLY SERVICES
                   IF WS-CLN-MOD-1 = 'FQ' OR
                      WS-CLN-MOD-2 = 'FQ'
      *---             AUDIO-ONLY HAS RESTRICTED CODE LIST
                       IF WS-CLN-HCPCS-CD < '90791' OR
                          WS-CLN-HCPCS-CD > '90899'
      *---                 MOST AUDIO-ONLY IS BEHAVIORAL HEALTH
                           IF WS-CLN-HCPCS-CD < '99201' OR
                              WS-CLN-HCPCS-CD > '99215'
                               MOVE 'ED0502' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE
                   'AUDIO-ONLY (FQ) MAY NOT BE VALID FOR THIS CPT'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-TELEHLTH-ERRS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

       3600-CHECK-COVID-EDITS.
      *================================================================*
      * COVID-19 SPECIFIC BILLING EDITS                                *
      * ADDED 2020 PER EMERGENCY PROVISIONS                           *
      *================================================================*
           SET WS-IS-COVID-RELATED TO TRUE
           MOVE 'N' TO WS-COVID-DIAG-FOUND
           MOVE 'N' TO WS-COVID-VACCINE-CPT
           MOVE 'N' TO WS-COVID-TEST-CPT

      *--- CHECK FOR COVID DIAGNOSIS
           IF WS-CLM-PRINC-DIAG(1:3) = 'U07'
               SET WS-HAS-COVID-DIAG TO TRUE
           END-IF

           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 25
               IF WS-CLM-DIAG-CD(WS-SUB-1)(1:3) = 'U07'
                   SET WS-HAS-COVID-DIAG TO TRUE
               END-IF
           END-PERFORM

      *--- CHECK FOR COVID VACCINE AND TEST CODES
           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

               EVALUATE WS-CLN-HCPCS-CD
                   WHEN '91300'
                   WHEN '91301'
                   WHEN '91302'
                   WHEN '91303'
                   WHEN '91304'
                   WHEN '91305'
                   WHEN '91306'
                   WHEN '91307'
                       SET WS-IS-COVID-VACCINE TO TRUE
                   WHEN '87635'
                   WHEN '87636'
                   WHEN '87637'
                   WHEN 'U0001'
                   WHEN 'U0002'
                   WHEN 'U0003'
                   WHEN 'U0004'
                   WHEN 'U0005'
                       SET WS-IS-COVID-TEST TO TRUE
               END-EVALUATE
           END-PERFORM

      *--- COVID VACCINE - NO COST SHARING
           IF WS-IS-COVID-VACCINE
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC
                   IF WS-CLN-COPAY-AMT > ZEROS
                       MOVE 'ED0170' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'COVID VACCINE: NO COST SHARING ALLOWED'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-COVID-ERRS
                   END-IF
               END-PERFORM
           END-IF
           .

       3650-VALIDATE-MENTAL-HEALTH-PARITY.
      *================================================================*
      * MENTAL HEALTH PARITY AND ADDICTION EQUITY ACT (MHPAEA)         *
      * FINANCIAL REQUIREMENTS, QUANTITATIVE TREATMENT LIMITS,         *
      * NON-QUANTITATIVE TREATMENT LIMIT FLAGS                         *
      * ORIGINALLY ADDED AGARCIA 2004-06-15                           *
      * EXPANDED FOR MHPAEA COMPLIANCE TLEE 2009-06-15                *
      *================================================================*

      *--- DETERMINE IF CLAIM HAS MENTAL HEALTH / SUBSTANCE ABUSE DIAG
           SET WS-MH-PARITY-NA TO TRUE
           MOVE WS-CLM-PRINC-DIAG(1:3) TO WS-WORK-DIAG-3

           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > WS-MH-RANGE-COUNT
               IF WS-WORK-DIAG-3 >= WS-MH-RANGE-FROM(WS-SUB-1)
                   AND WS-WORK-DIAG-3 <=
                       WS-MH-RANGE-TO(WS-SUB-1)
                   SET WS-MH-PARITY-APPLIES TO TRUE
               END-IF
           END-PERFORM

      *--- IF NOT PRINCIPAL, CHECK ADDITIONAL DIAGNOSES
           IF WS-MH-PARITY-NA
               PERFORM VARYING WS-SUB-2 FROM 1 BY 1
                   UNTIL WS-SUB-2 > 25
                   OR WS-MH-PARITY-APPLIES
                   IF WS-CLM-DIAG-CD(WS-SUB-2) NOT = SPACES
                       MOVE WS-CLM-DIAG-CD(WS-SUB-2)(1:3)
                           TO WS-WORK-DIAG-3
                       PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                           UNTIL WS-SUB-1 > WS-MH-RANGE-COUNT
                           IF WS-WORK-DIAG-3 >=
                               WS-MH-RANGE-FROM(WS-SUB-1)
                               AND WS-WORK-DIAG-3 <=
                               WS-MH-RANGE-TO(WS-SUB-1)
                               SET WS-MH-PARITY-APPLIES TO TRUE
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF

           IF WS-MH-PARITY-APPLIES
      *---     FINANCIAL REQUIREMENTS CANNOT BE MORE RESTRICTIVE
      *        THAN MEDICAL/SURGICAL BENEFITS
               PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
                   UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
                   MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                       TO WS-CLAIM-LINE-REC

      *---         CHECK COPAY IS NOT HIGHER THAN MED/SURG COPAY
                   IF WS-CLN-COPAY-AMT > ZEROS
                       EXEC SQL
                           SELECT PLAN_MH_COPAY, PLAN_MS_COPAY
                           INTO  :WS-WORK-AMOUNT,
                                 :WS-COB-PRIMARY-PAID
                           FROM  HCPS_CLAIMS_DB..PLAN_BENEFITS
                           WHERE PLAN_CD = :WS-CLM-PAYER-CD
                           AND   PLAN_EFF_DT <= :WS-CLM-FROM-DOS
                           AND   (PLAN_TERM_DT >= :WS-CLM-FROM-DOS
                                  OR PLAN_TERM_DT = 0)
                       END-EXEC
                       IF WS-SQLCODE = 0
                           IF WS-WORK-AMOUNT >
                               WS-COB-PRIMARY-PAID
                               MOVE 'ED0600' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE
                   'MHPAEA: MH COPAY EXCEEDS MED/SURG COPAY'
                                   TO WS-EDIT-ERR-MSG(
                                       WS-EDIT-ERR-COUNT + 1)
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-PARITY-ERRS
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

      *---     QUANTITATIVE TREATMENT LIMITS CHECK
      *        (VISIT LIMITS, DAY LIMITS)
               EXEC SQL
                   SELECT PLAN_MH_VISIT_LIMIT,
                          PLAN_MS_VISIT_LIMIT
                   INTO  :WS-AUTH-UNITS-APPROVED,
                         :WS-AUTH-UNITS-USED
                   FROM  HCPS_CLAIMS_DB..PLAN_BENEFITS
                   WHERE PLAN_CD = :WS-CLM-PAYER-CD
                   AND   PLAN_EFF_DT <= :WS-CLM-FROM-DOS
                   AND   (PLAN_TERM_DT >= :WS-CLM-FROM-DOS
                          OR PLAN_TERM_DT = 0)
               END-EXEC
               IF WS-SQLCODE = 0
                   IF WS-AUTH-UNITS-APPROVED > 0 AND
                      WS-AUTH-UNITS-USED > 0
                       IF WS-AUTH-UNITS-APPROVED <
                           WS-AUTH-UNITS-USED
                           MOVE 'ED0601' TO WS-EDIT-ERR-CD(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE 'W' TO WS-EDIT-ERR-SEV(
                               WS-EDIT-ERR-COUNT + 1)
                           MOVE
                   'MHPAEA: MH VISIT LIMIT MORE RESTRICTIVE'
                               TO WS-EDIT-ERR-MSG(
                                   WS-EDIT-ERR-COUNT + 1)
                           ADD 1 TO WS-EDIT-ERR-COUNT
                           ADD 1 TO WS-ESTAT-PARITY-ERRS
                       END-IF
                   END-IF
               END-IF

      *---     SUBSTANCE ABUSE SPECIFIC EDITS (F10-F19 RANGE)
               IF WS-CLM-PRINC-DIAG(1:3) >= 'F10' AND
                  WS-CLM-PRINC-DIAG(1:3) <= 'F19'
                   MOVE 'ED0605' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'I' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE
               'SUBSTANCE ABUSE CLAIM - PARITY PROTECTIONS APPLY'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               END-IF
           END-IF
           .

       3750-VALIDATE-PREVENTIVE-CARE.
      *================================================================*
      * ACA PREVENTIVE CARE MANDATE                                    *
      * NO COST SHARING FOR PREVENTIVE SERVICES                        *
      * WELL-WOMAN VISIT, IMMUNIZATIONS, SCREENINGS                   *
      * CONTRACEPTIVE COVERAGE MANDATE                                 *
      * AGE/GENDER APPROPRIATE PREVENTIVE CARE                        *
      * ADDED TLEE 2008-01-01                                         *
      *================================================================*

           SET WS-NOT-PREVENTIVE TO TRUE

           PERFORM VARYING WS-LINE-CTR FROM 1 BY 1
               UNTIL WS-LINE-CTR > WS-CLT-LINE-COUNT
               MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
                   TO WS-CLAIM-LINE-REC

      *---     CHECK IF PROCEDURE IS ON PREVENTIVE CARE LIST
               IF WS-CLN-HCPCS-CD NOT = SPACES
                   PERFORM VARYING WS-SUB-1 FROM 1 BY 1
                       UNTIL WS-SUB-1 > WS-PREV-PROC-COUNT
                       IF WS-CLN-HCPCS-CD =
                           WS-PREV-CPT-CD(WS-SUB-1)
                           SET WS-IS-PREVENTIVE TO TRUE

      *---                 NO COST SHARING FOR PREVENTIVE
                           IF WS-CLN-COPAY-AMT > ZEROS OR
                              WS-CLN-COINS-AMT > ZEROS OR
                              WS-CLN-DEDUCT-AMT > ZEROS
                               MOVE 'ED0700' TO WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                               MOVE 'W' TO WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                               STRING
                               'ACA: NO COST SHARING FOR '
                               WS-CLN-HCPCS-CD
                               ' (PREVENTIVE)'
                               DELIMITED BY SIZE
                               INTO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                               END-STRING
                               ADD 1 TO WS-EDIT-ERR-COUNT
                               ADD 1 TO WS-ESTAT-PREVENT-ERRS
                           END-IF

      *---                 WELL-WOMAN VISIT (TYPE 'W')
                           IF WS-PREV-TYPE(WS-SUB-1) = 'W'
                               IF WS-CLM-PAT-GENDER NOT = 'F'
                                   MOVE 'ED0701' TO
                                   WS-EDIT-ERR-CD(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE 'F' TO
                                   WS-EDIT-ERR-SEV(
                                   WS-EDIT-ERR-COUNT + 1)
                                   MOVE
                       'WELL-WOMAN CODE REQUIRES FEMALE PATIENT'
                                       TO WS-EDIT-ERR-MSG(
                                           WS-EDIT-ERR-COUNT + 1)
                                   ADD 1 TO WS-EDIT-ERR-COUNT
                                   SET WS-CLAIM-IS-INVALID TO TRUE
                                   ADD 1 TO WS-ESTAT-PREVENT-ERRS
                               END-IF
                           END-IF

      *---                 SCREENING (TYPE 'S') - CHECK AGE
                           IF WS-PREV-TYPE(WS-SUB-1) = 'S'
      *---                     MAMMOGRAPHY - AGE 40+
                               IF WS-CLN-HCPCS-CD = '77067' OR
                                  WS-CLN-HCPCS-CD = '77063'
                                   IF WS-PATIENT-AGE-YEARS < 40
                                       MOVE 'ED0702' TO
                                       WS-EDIT-ERR-CD(
                                       WS-EDIT-ERR-COUNT + 1)
                                       MOVE 'W' TO
                                       WS-EDIT-ERR-SEV(
                                       WS-EDIT-ERR-COUNT + 1)
                                       MOVE
                   'SCREENING MAMMOGRAPHY TYPICALLY AGE 40+'
                                           TO WS-EDIT-ERR-MSG(
                                           WS-EDIT-ERR-COUNT + 1)
                                       ADD 1 TO WS-EDIT-ERR-COUNT
                                       ADD 1 TO
                                       WS-ESTAT-PREVENT-ERRS
                                   END-IF
                               END-IF
      *---                     COLONOSCOPY - AGE 45+
                               IF WS-CLN-HCPCS-CD = 'G0104' OR
                                  WS-CLN-HCPCS-CD = 'G0105' OR
                                  WS-CLN-HCPCS-CD = 'G0121'
                                   IF WS-PATIENT-AGE-YEARS < 45
                                       MOVE 'ED0703' TO
                                       WS-EDIT-ERR-CD(
                                       WS-EDIT-ERR-COUNT + 1)
                                       MOVE 'W' TO
                                       WS-EDIT-ERR-SEV(
                                       WS-EDIT-ERR-COUNT + 1)
                                       MOVE
                   'COLORECTAL SCREENING TYPICALLY AGE 45+'
                                           TO WS-EDIT-ERR-MSG(
                                           WS-EDIT-ERR-COUNT + 1)
                                       ADD 1 TO WS-EDIT-ERR-COUNT
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF

      *---     IMMUNIZATION CODES (90XXX) - NO COST SHARING
               IF WS-CLN-HCPCS-CD >= '90281' AND
                  WS-CLN-HCPCS-CD <= '90756'
                   SET WS-IS-PREVENTIVE TO TRUE
                   IF WS-CLN-COPAY-AMT > ZEROS OR
                      WS-CLN-COINS-AMT > ZEROS
                       MOVE 'ED0710' TO WS-EDIT-ERR-CD(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE 'W' TO WS-EDIT-ERR-SEV(
                           WS-EDIT-ERR-COUNT + 1)
                       MOVE
                   'ACA: NO COST SHARING FOR IMMUNIZATIONS'
                           TO WS-EDIT-ERR-MSG(
                               WS-EDIT-ERR-COUNT + 1)
                       ADD 1 TO WS-EDIT-ERR-COUNT
                       ADD 1 TO WS-ESTAT-PREVENT-ERRS
                   END-IF
               END-IF
           END-PERFORM
           .

       4000-WRITE-VALID-CLAIM.
      *================================================================*
      * WRITE VALIDATED CLAIM TO OUTPUT FILE                           *
      *================================================================*
           WRITE VALID-CLAIM-REC FROM WS-CLAIM-HEADER-REC
           IF WS-CLMVL-STATUS NOT = '00'
               MOVE 'VALID CLAIM WRITE FAILED' TO WS-ERR-MESSAGE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF
           ADD 1 TO WS-STAT-RECORDS-WRITTEN
           ADD 1 TO WS-STAT-CLAIMS-APPROVED
           ADD WS-CLM-TOTAL-CHARGES TO WS-STAT-TOTAL-CHARGED

      *--- UPDATE CLAIM TYPE PASS STATISTICS
           EVALUATE TRUE
               WHEN WS-CLM-INSTITUTIONAL
                   ADD 1 TO WS-CSTAT-INST-PASS
               WHEN WS-CLM-PROFESSIONAL
                   ADD 1 TO WS-CSTAT-PROF-PASS
               WHEN WS-CLM-DENTAL
                   ADD 1 TO WS-CSTAT-DENT-PASS
               WHEN WS-CLM-PHARMACY
                   ADD 1 TO WS-CSTAT-PHARM-PASS
           END-EVALUATE
           .

       5000-WRITE-REJECTED-CLAIM.
      *================================================================*
      * WRITE REJECTED CLAIM AND ERROR DETAILS                         *
      *================================================================*
           MOVE WS-CLAIM-HEADER-REC TO WS-REJ-CLAIM-DATA
           MOVE WS-EDIT-ERR-COUNT TO WS-REJ-ERROR-COUNT
           MOVE WS-EDIT-ERR-CD(1) TO WS-REJ-PRIMARY-ERR-CD
           MOVE FUNCTION CURRENT-DATE TO WS-REJ-TIMESTAMP

      *--- BUILD ERROR CODE STRING
           MOVE SPACES TO WS-REJ-ERROR-CODES
           MOVE 1 TO WS-SUB-2
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > WS-EDIT-ERR-COUNT
               OR WS-SUB-1 > 10
               STRING WS-EDIT-ERR-CD(WS-SUB-1) ','
                   DELIMITED BY SIZE
                   INTO WS-REJ-ERROR-CODES
                   WITH POINTER WS-SUB-2
               END-STRING
           END-PERFORM

           WRITE REJECT-CLAIM-REC FROM WS-REJECT-RECORD
           IF WS-CLMRJ-STATUS NOT = '00'
               MOVE 'REJECT CLAIM WRITE FAILED' TO WS-ERR-MESSAGE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF

           ADD 1 TO WS-STAT-RECORDS-REJECTED
           ADD 1 TO WS-STAT-CLAIMS-DENIED
           ADD WS-CLM-TOTAL-CHARGES TO WS-STAT-TOTAL-DENIED

      *--- UPDATE CLAIM TYPE FAIL STATISTICS
           EVALUATE TRUE
               WHEN WS-CLM-INSTITUTIONAL
                   ADD 1 TO WS-CSTAT-INST-FAIL
               WHEN WS-CLM-PROFESSIONAL
                   ADD 1 TO WS-CSTAT-PROF-FAIL
               WHEN WS-CLM-DENTAL
                   ADD 1 TO WS-CSTAT-DENT-FAIL
               WHEN WS-CLM-PHARMACY
                   ADD 1 TO WS-CSTAT-PHARM-FAIL
           END-EVALUATE

      *--- WRITE ERRORS TO REPORT
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > WS-EDIT-ERR-COUNT
               MOVE WS-CLM-NUMBER TO WS-RPT-CLM-NO
               MOVE WS-EDIT-ERR-CD(WS-SUB-1) TO WS-RPT-EDIT-CD
               MOVE WS-EDIT-ERR-SEV(WS-SUB-1) TO WS-RPT-SEVERITY
               MOVE WS-EDIT-ERR-MSG(WS-SUB-1) TO WS-RPT-EDIT-MSG
               MOVE WS-EDIT-ERR-FIELD(WS-SUB-1)
                   TO WS-RPT-FIELD-NAME

               IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
                   PERFORM 1300-WRITE-REPORT-HEADERS
               END-IF

               WRITE ERROR-REPORT-REC FROM WS-RPT-DETAIL-LINE
                   AFTER ADVANCING 1 LINE
               ADD 1 TO WS-LINE-COUNT
           END-PERFORM
           .

       6000-ADD-EDIT-ERROR-CLM-NUM.
      *================================================================*
      * ADD EDIT ERROR FOR MISSING CLAIM NUMBER                        *
      *================================================================*
           MOVE 'ED0001' TO WS-EDIT-ERR-CD(WS-EDIT-ERR-COUNT + 1)
           MOVE 'F' TO WS-EDIT-ERR-SEV(WS-EDIT-ERR-COUNT + 1)
           MOVE 'CLAIM NUMBER IS REQUIRED'
               TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
           MOVE 'CLM-NUMBER' TO
               WS-EDIT-ERR-FIELD(WS-EDIT-ERR-COUNT + 1)
           ADD 1 TO WS-EDIT-ERR-COUNT
           SET WS-CLAIM-IS-INVALID TO TRUE
           SET WS-HAS-FATAL-EDIT TO TRUE
           .

       6100-LOOKUP-PAYER.
      *================================================================*
      * LOOK UP PAYER IN DATABASE                                      *
      *================================================================*
           EXEC SQL
               SELECT COUNT(*)
               INTO  :WS-DB-ROW-COUNT
               FROM  HCPS_CLAIMS_DB..PAYER_MASTER
               WHERE PAYER_CD = :WS-CLM-PAYER-CD
               AND   PAYER_STATUS = 'A'
           END-EXEC

           IF WS-SQLCODE = 0
               IF WS-DB-ROW-COUNT = 0
                   MOVE 'ED0006' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'PAYER CODE NOT FOUND IN PAYER MASTER'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
               END-IF
           END-IF
           .

       6200-LOOKUP-PROVIDER.
      *================================================================*
      * LOOK UP PROVIDER IN DATABASE AND CHECK STATUS                  *
      *================================================================*
           EXEC SQL
               SELECT PRV_NPI,
                      PRV_TAX_ID,
                      PRV_LAST_NAME,
                      PRV_FIRST_NAME,
                      PRV_ORG_NAME,
                      PRV_TAXONOMY_CD,
                      PRV_PAR_STATUS,
                      PRV_NETWORK_CD,
                      PRV_OIG_EXCL_FLAG,
                      PRV_SAM_EXCL_FLAG,
                      PRV_RECORD_STATUS
               INTO  :WS-PRV-NPI,
                     :WS-PRV-TAX-ID,
                     :WS-PRV-LAST-NAME,
                     :WS-PRV-FIRST-NAME,
                     :WS-PRV-ORG-NAME,
                     :WS-PRV-TAXONOMY-CD,
                     :WS-PRV-PAR-STATUS,
                     :WS-PRV-NETWORK-CD,
                     :WS-PRV-OIG-EXCL-FLAG,
                     :WS-PRV-SAM-EXCL-FLAG,
                     :WS-PRV-RECORD-STATUS
               FROM  HCPS_CLAIMS_DB..PROVIDER_MASTER
               WHERE PRV_NPI = :WS-CLM-BILL-PROV-NPI
           END-EXEC

           EVALUATE WS-SQLCODE
               WHEN 0
                   SET WS-DB-FOUND TO TRUE
               WHEN 100
                   SET WS-DB-NOT-FND TO TRUE
                   MOVE 'ED0026' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'W' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'BILLING PROVIDER NOT IN PROVIDER MASTER'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
               WHEN OTHER
                   MOVE 'PROVIDER LOOKUP' TO WS-DB-OPERATION
                   PERFORM 8100-DATABASE-ERROR
           END-EVALUATE
           .

       6300-LOOKUP-DIAGNOSIS.
      *================================================================*
      * VERIFY DIAGNOSIS CODE EXISTS IN ICD-10 REFERENCE TABLE         *
      *================================================================*
           EXEC SQL
               SELECT COUNT(*)
               INTO  :WS-DB-ROW-COUNT
               FROM  HCPS_CLAIMS_DB..ICD10_DIAGNOSIS
               WHERE ICD_CD = :WS-CLM-PRINC-DIAG
               AND   ICD_EFF_DT <= :WS-CLM-FROM-DOS
               AND   (ICD_TERM_DT >= :WS-CLM-FROM-DOS
                      OR ICD_TERM_DT = 0)
           END-EXEC

           IF WS-SQLCODE = 0
               IF WS-DB-ROW-COUNT = 0
                   MOVE 'ED0041' TO WS-EDIT-ERR-CD(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'F' TO WS-EDIT-ERR-SEV(
                       WS-EDIT-ERR-COUNT + 1)
                   MOVE 'PRINCIPAL DIAG NOT IN ICD-10 TABLE'
                       TO WS-EDIT-ERR-MSG(WS-EDIT-ERR-COUNT + 1)
                   ADD 1 TO WS-EDIT-ERR-COUNT
                   SET WS-CLAIM-IS-INVALID TO TRUE
               END-IF
           END-IF
           .

       7000-VALIDATE-DATE.
      *================================================================*
      * VALIDATE DATE FORMAT AND LOGICAL CORRECTNESS                   *
      *================================================================*
      *--- CHECK MONTH RANGE
           IF WS-DW-MM < 01 OR WS-DW-MM > 12
               MOVE ZEROS TO WS-DATE-WORK
               GO TO 7000-EXIT
           END-IF

      *--- CHECK DAY RANGE BASED ON MONTH
           EVALUATE WS-DW-MM
               WHEN 01 WHEN 03 WHEN 05 WHEN 07
               WHEN 08 WHEN 10 WHEN 12
                   IF WS-DW-DD < 01 OR WS-DW-DD > 31
                       MOVE ZEROS TO WS-DATE-WORK
                   END-IF
               WHEN 04 WHEN 06 WHEN 09 WHEN 11
                   IF WS-DW-DD < 01 OR WS-DW-DD > 30
                       MOVE ZEROS TO WS-DATE-WORK
                   END-IF
               WHEN 02
      *---         CHECK LEAP YEAR
                   IF FUNCTION MOD(WS-DATE-WORK / 10000, 4) = 0
                       IF WS-DW-DD < 01 OR WS-DW-DD > 29
                           MOVE ZEROS TO WS-DATE-WORK
                       END-IF
                   ELSE
                       IF WS-DW-DD < 01 OR WS-DW-DD > 28
                           MOVE ZEROS TO WS-DATE-WORK
                       END-IF
                   END-IF
           END-EVALUATE
           .
       7000-EXIT.
           EXIT.

       7100-VALIDATE-NPI.
      *================================================================*
      * VALIDATE NPI CHECK DIGIT USING LUHN ALGORITHM                  *
      * NPI IS A 10-DIGIT NUMBER WITH EMBEDDED CHECK DIGIT             *
      *================================================================*
           MOVE 0 TO WS-NPI-SUM
           SET WS-NPI-IS-VALID TO TRUE

      *--- VERIFY ALL CHARACTERS ARE NUMERIC
           INSPECT WS-NPI-WORK TALLYING WS-NPI-SUM
               FOR ALL '0' '1' '2' '3' '4' '5' '6' '7' '8' '9'

           IF WS-NPI-SUM < 10
               SET WS-NPI-INVALID TO TRUE
               GO TO 7100-EXIT
           END-IF

      *--- APPLY LUHN ALGORITHM (SIMPLIFIED FOR COBOL)
           MOVE 0 TO WS-NPI-SUM
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 9
               COMPUTE WS-NPI-CHECK-DIGIT =
                   FUNCTION ORD(WS-NPI-WORK(WS-SUB-1:1)) - 48
               IF FUNCTION MOD(WS-SUB-1, 2) = 0
                   COMPUTE WS-NPI-DOUBLED =
                       WS-NPI-CHECK-DIGIT * 2
                   IF WS-NPI-DOUBLED > 9
                       SUBTRACT 9 FROM WS-NPI-DOUBLED
                   END-IF
                   ADD WS-NPI-DOUBLED TO WS-NPI-SUM
               ELSE
                   ADD WS-NPI-CHECK-DIGIT TO WS-NPI-SUM
               END-IF
           END-PERFORM

           ADD 24 TO WS-NPI-SUM
           COMPUTE WS-NPI-REMAINDER = FUNCTION MOD(WS-NPI-SUM, 10)
           IF WS-NPI-REMAINDER NOT = 0
               COMPUTE WS-NPI-CHECK-DIGIT =
                   10 - WS-NPI-REMAINDER
           ELSE
               MOVE 0 TO WS-NPI-CHECK-DIGIT
           END-IF

           IF WS-NPI-CHECK-DIGIT NOT =
               FUNCTION ORD(WS-NPI-WORK(10:1)) - 48
               SET WS-NPI-INVALID TO TRUE
           END-IF
           .
       7100-EXIT.
           EXIT.

       7200-VALIDATE-ICD10-FORMAT.
      *================================================================*
      * VALIDATE ICD-10 CODE FORMAT                                    *
      * FORMAT: ALPHA + 2 NUMERIC + OPTIONAL DOT + UP TO 4 CHARS      *
      *================================================================*
           SET WS-ICD-VALID TO TRUE

           MOVE WS-ICD-CODE-WORK(1:1) TO WS-ICD-ALPHA-PORTION
           MOVE WS-ICD-CODE-WORK(2:6) TO WS-ICD-NUMERIC-PORTION

      *--- FIRST CHARACTER MUST BE ALPHA
           IF WS-ICD-ALPHA-PORTION < 'A'
               OR WS-ICD-ALPHA-PORTION > 'Z'
               SET WS-ICD-INVALID TO TRUE
               GO TO 7200-EXIT
           END-IF

      *--- SECOND AND THIRD CHARACTERS MUST BE NUMERIC
           IF WS-ICD-CODE-WORK(2:1) < '0'
               OR WS-ICD-CODE-WORK(2:1) > '9'
               SET WS-ICD-INVALID TO TRUE
               GO TO 7200-EXIT
           END-IF

           IF WS-ICD-CODE-WORK(3:1) < '0'
               OR WS-ICD-CODE-WORK(3:1) > '9'
               SET WS-ICD-INVALID TO TRUE
               GO TO 7200-EXIT
           END-IF
           .
       7200-EXIT.
           EXIT.

       7300-CHECK-NCCI-MOD-OVERRIDE.
      *================================================================*
      * CHECK IF MODIFIER OVERRIDES NCCI EDIT                          *
      * MODIFIERS 25, 59, XE, XP, XS, XU CAN OVERRIDE NCCI EDITS    *
      * ADDED BCHANG 2013-10-01                                       *
      *================================================================*
           SET WS-NCCI-FAILED TO TRUE

      *--- CHECK ALL MODIFIERS ON THE CURRENT LINE
           MOVE WS-CLT-LINE-DATA(WS-LINE-CTR)
               TO WS-CLAIM-LINE-REC

           IF WS-CLN-MOD-1 = '25' OR WS-CLN-MOD-2 = '25'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = '59' OR WS-CLN-MOD-2 = '59'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XE' OR WS-CLN-MOD-2 = 'XE'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XP' OR WS-CLN-MOD-2 = 'XP'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XS' OR WS-CLN-MOD-2 = 'XS'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XU' OR WS-CLN-MOD-2 = 'XU'
               SET WS-NCCI-PASSED TO TRUE
           END-IF

      *--- ALSO CHECK THE PAIRED LINE
           MOVE WS-CLT-LINE-DATA(WS-LINE-CTR-2)
               TO WS-CLAIM-LINE-REC

           IF WS-CLN-MOD-1 = '25' OR WS-CLN-MOD-2 = '25'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = '59' OR WS-CLN-MOD-2 = '59'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XE' OR WS-CLN-MOD-2 = 'XE'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XP' OR WS-CLN-MOD-2 = 'XP'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XS' OR WS-CLN-MOD-2 = 'XS'
               SET WS-NCCI-PASSED TO TRUE
           END-IF
           IF WS-CLN-MOD-1 = 'XU' OR WS-CLN-MOD-2 = 'XU'
               SET WS-NCCI-PASSED TO TRUE
           END-IF

      *--- TRANSFER RESULT BACK TO NCCI FLAG
           IF WS-NCCI-PASSED
               SET WS-NCCI-PASSED TO TRUE
           ELSE
               SET WS-NCCI-FAILED TO TRUE
           END-IF
           .

       8000-ERROR-HANDLER.
      *================================================================*
      * CENTRALIZED ERROR HANDLING                                     *
      *================================================================*
           ADD 1 TO WS-ERR-COUNT
           MOVE WS-PROGRAM-NAME TO WS-ERR-PROGRAM
           MOVE FUNCTION CURRENT-DATE TO WS-ERR-TIMESTAMP

           DISPLAY 'HCCLMVAL - ERROR: ' WS-ERR-MESSAGE
           DISPLAY 'HCCLMVAL - SEVERITY: ' WS-ERR-SEVERITY
           DISPLAY 'HCCLMVAL - ERROR COUNT: ' WS-ERR-COUNT

           IF WS-ERR-FATAL
               DISPLAY 'HCCLMVAL - FATAL ERROR - ABENDING'
               MOVE WS-ERR-CODE TO WS-ABEND-CODE
               MOVE WS-PROGRAM-NAME TO WS-ABEND-PROGRAM
               MOVE WS-ERR-MESSAGE TO WS-ABEND-REASON
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF

           IF WS-ERR-COUNT >= WS-ERR-MAX-ALLOWED
               SET WS-ERR-OVER-THRESHOLD TO TRUE
               DISPLAY 'HCCLMVAL - ERROR THRESHOLD REACHED'
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF
           .

       8100-DATABASE-ERROR.
      *================================================================*
      * DATABASE ERROR HANDLING WITH RETRY LOGIC                       *
      *================================================================*
           DISPLAY 'HCCLMVAL - DB ERROR IN: ' WS-DB-OPERATION
           DISPLAY 'HCCLMVAL - SQLCODE: ' WS-SQLCODE
           DISPLAY 'HCCLMVAL - SQLERRM: ' WS-SQLERRMC

      *--- CHECK FOR DEADLOCK - RETRY
           IF WS-SQLCODE = -1205
               ADD 1 TO WS-DB-RETRY-COUNT
               IF WS-DB-RETRY-COUNT <= WS-DB-MAX-RETRIES
                   DISPLAY 'HCCLMVAL - DEADLOCK RETRY: '
                       WS-DB-RETRY-COUNT
               ELSE
                   MOVE 'DATABASE DEADLOCK - MAX RETRIES EXCEEDED'
                       TO WS-ERR-MESSAGE
                   MOVE 'F' TO WS-ERR-SEVERITY
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
               STRING 'DB ERROR - ' WS-DB-OPERATION
                   ' SQLCODE=' WS-SQLCODE
                   DELIMITED BY SIZE
                   INTO WS-ERR-MESSAGE
               END-STRING
               MOVE 'F' TO WS-ERR-SEVERITY
               PERFORM 8000-ERROR-HANDLER
           END-IF
           .

       9000-TERMINATION.
      *================================================================*
      * PROGRAM TERMINATION - CLOSE FILES, DISCONNECT DB, PRINT STATS *
      *================================================================*
           MOVE FUNCTION CURRENT-DATE TO WS-STAT-END-TIME

           DISPLAY '================================================='
           DISPLAY 'HCCLMVAL - PROCESSING STATISTICS'
           DISPLAY '================================================='
           DISPLAY 'RECORDS READ:      ' WS-STAT-RECORDS-READ
           DISPLAY 'RECORDS VALIDATED:  ' WS-STAT-RECORDS-WRITTEN
           DISPLAY 'RECORDS REJECTED:   ' WS-STAT-RECORDS-REJECTED
           DISPLAY 'CLAIMS APPROVED:    ' WS-STAT-CLAIMS-APPROVED
           DISPLAY 'CLAIMS DENIED:      ' WS-STAT-CLAIMS-DENIED
           DISPLAY 'TOTAL CHARGES:      ' WS-STAT-TOTAL-CHARGED
           DISPLAY 'TOTAL DENIED:       ' WS-STAT-TOTAL-DENIED
           DISPLAY 'ERRORS ENCOUNTERED: ' WS-ERR-COUNT
           DISPLAY 'START TIME:         ' WS-STAT-START-TIME
           DISPLAY 'END TIME:           ' WS-STAT-END-TIME
           DISPLAY '-------------------------------------------------'
           DISPLAY 'BY CLAIM TYPE:'
           DISPLAY '  INSTITUTIONAL: READ=' WS-CSTAT-INST-READ
               ' PASS=' WS-CSTAT-INST-PASS
               ' FAIL=' WS-CSTAT-INST-FAIL
           DISPLAY '  PROFESSIONAL:  READ=' WS-CSTAT-PROF-READ
               ' PASS=' WS-CSTAT-PROF-PASS
               ' FAIL=' WS-CSTAT-PROF-FAIL
           DISPLAY '  DENTAL:        READ=' WS-CSTAT-DENT-READ
               ' PASS=' WS-CSTAT-DENT-PASS
               ' FAIL=' WS-CSTAT-DENT-FAIL
           DISPLAY '  PHARMACY:      READ=' WS-CSTAT-PHARM-READ
               ' PASS=' WS-CSTAT-PHARM-PASS
               ' FAIL=' WS-CSTAT-PHARM-FAIL
           DISPLAY '-------------------------------------------------'
           DISPLAY 'BY ERROR CATEGORY:'
           DISPLAY '  HEADER:        ' WS-ESTAT-HEADER-ERRS
           DISPLAY '  PATIENT:       ' WS-ESTAT-PATIENT-ERRS
           DISPLAY '  PROVIDER:      ' WS-ESTAT-PROVIDER-ERRS
           DISPLAY '  DATE:          ' WS-ESTAT-DATE-ERRS
           DISPLAY '  DIAGNOSIS:     ' WS-ESTAT-DIAG-ERRS
           DISPLAY '  PROCEDURE:     ' WS-ESTAT-PROC-ERRS
           DISPLAY '  FINANCIAL:     ' WS-ESTAT-FINANCIAL-ERRS
           DISPLAY '  ELIGIBILITY:   ' WS-ESTAT-ELIG-ERRS
           DISPLAY '  DUPLICATE:     ' WS-ESTAT-DUP-ERRS
           DISPLAY '  TIMELY FILING: ' WS-ESTAT-TIMELY-ERRS
           DISPLAY '  AUTH:          ' WS-ESTAT-AUTH-ERRS
           DISPLAY '  COB:           ' WS-ESTAT-COB-ERRS
           DISPLAY '  NCCI:          ' WS-ESTAT-NCCI-ERRS
           DISPLAY '  MUE:           ' WS-ESTAT-MUE-ERRS
           DISPLAY '  GENDER/AGE:    ' WS-ESTAT-GENDER-AGE-ERRS
           DISPLAY '  MODIFIER:      ' WS-ESTAT-MODIFIER-ERRS
           DISPLAY '  POS:           ' WS-ESTAT-POS-ERRS
           DISPLAY '  TELEHEALTH:    ' WS-ESTAT-TELEHLTH-ERRS
           DISPLAY '  MH PARITY:     ' WS-ESTAT-PARITY-ERRS
           DISPLAY '  PREVENTIVE:    ' WS-ESTAT-PREVENT-ERRS
           DISPLAY '  NSA:           ' WS-ESTAT-NSA-ERRS
           DISPLAY '  COVID:         ' WS-ESTAT-COVID-ERRS
           DISPLAY '  OPIOID:        ' WS-ESTAT-OPIOID-ERRS
           DISPLAY '  CROSSOVER:     ' WS-ESTAT-XOVER-ERRS
           DISPLAY '================================================='

      *--- DISCONNECT FROM DATABASE
           IF WS-DB-CONNECTED
               EXEC SQL
                   DISCONNECT ALL
               END-EXEC
           END-IF

      *--- CLOSE ALL FILES
           CLOSE CLAIM-INPUT-FILE
           CLOSE VALID-CLAIM-FILE
           CLOSE REJECT-CLAIM-FILE
           CLOSE ERROR-REPORT-FILE
           CLOSE EDIT-RULES-FILE
           CLOSE NCCI-EDIT-FILE
           CLOSE MUE-THRESHOLD-FILE
           CLOSE LCD-NCD-FILE
           CLOSE CROSSOVER-OUTPUT-FILE
           .
