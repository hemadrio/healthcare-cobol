      *================================================================*
      * PROGRAM:    HCELIGVR
      * TITLE:      MEMBER ELIGIBILITY VERIFICATION AND ENROLLMENT
      *             BATCH PROCESSOR
      * SYSTEM:     HEALTHCARE CLAIMS MANAGEMENT SYSTEM (HCMS)
      * SUBSYSTEM:  ENROLLMENT AND ELIGIBILITY (E&E)
      * AUTHOR:     D. KOWALSKI / R. NAIPAUL
      * WRITTEN:    1996-03-18
      * INSTALLED:  1996-06-01
      *
      * PURPOSE:    PROCESSES 834 ENROLLMENT TRANSACTION FILES FOR
      *             NEW ENROLLMENTS, TERMINATIONS, DEPENDENT
      *             CHANGES, PLAN TRANSFERS, COBRA, OPEN ENROLLMENT,
      *             AND RETROACTIVE ELIGIBILITY ADJUSTMENTS.
      *             PERFORMS REAL-TIME 270/271 ELIGIBILITY
      *             VERIFICATION, BENEFIT ACCUMULATOR UPDATES,
      *             FAMILY ROLLUP, AND PLAN YEAR RESET LOGIC.
      *             ACA-SPECIFIC PROCESSING FOR EXCHANGE
      *             RECONCILIATION, APTC TRACKING, EHB
      *             VERIFICATION, AND METAL TIER VALIDATION.
      *
      * INPUT:      - ENRL-834-FILE   (834 TRANSACTION FLAT FILE)
      *             - ELIG-270-FILE   (270 INQUIRY FILE)
      *             - SYBASE ELIGIBILITY / ACCUMULATOR TABLES
      *
      * OUTPUT:     - ELIG-271-FILE   (271 RESPONSE FILE)
      *             - ENRL-RPT-FILE   (ENROLLMENT REPORT)
      *             - AUDIT-TRAIL-FILE (CHANGE HISTORY LOG)
      *             - SYBASE TABLE UPDATES
      *
      * TABLES:     T_MEMBER_ELIG, T_MEMBER_ACCUM, T_FAMILY_ACCUM,
      *             T_PLAN_BENEFIT, T_ENRL_HISTORY, T_AUDIT_TRAIL,
      *             T_COBRA_TRACK, T_ACA_EXCHANGE, T_APTC_DETAIL
      *
      * ABEND CODES:
      *   U0810 - UNABLE TO OPEN INPUT FILE
      *   U0811 - UNABLE TO OPEN OUTPUT FILE
      *   U0820 - DATABASE CONNECTION FAILURE
      *   U0830 - CRITICAL DATA INTEGRITY ERROR
      *   U0840 - ACCUMULATOR OVERFLOW DETECTED
      *   U0850 - PLAN CONFIGURATION MISSING
      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY:
      *----------------------------------------------------------------*
      * DATE       AUTHOR     TICKET     DESCRIPTION
      *----------------------------------------------------------------*
      * 1996-03-18 DKOWALSKI  HC-00412   INITIAL DEVELOPMENT
      * 1996-08-22 DKOWALSKI  HC-00567   ADD COBRA CONTINUATION LOGIC
      * 1997-01-15 RNAIPAUL   HC-00701   PLAN YEAR RESET FOR JAN 1
      * 1997-06-30 RNAIPAUL   HC-00788   FAMILY ACCUMULATOR ROLLUP
      * 1998-03-01 JMCCARTHY  HC-01022   RETRO ELIGIBILITY CHANGES
      * 1998-11-10 JMCCARTHY  HC-01198   FIX DUPLICATE ENROLLMENT BUG
      * 1999-09-15 DKOWALSKI  HC-01455   Y2K DATE REMEDIATION
      * 2000-01-05 DKOWALSKI  HC-01502   POST-Y2K DATE FIX PATCH
      * 2001-04-20 TLIU       HC-01780   HIPAA 834 FORMAT CHANGES
      * 2002-08-14 TLIU       HC-02015   270/271 REAL-TIME SUPPORT
      * 2003-02-28 RNAIPAUL   HC-02201   LIFETIME MAX ACCUMULATOR
      * 2004-06-15 KMENDEZ    HC-02540   OPEN ENROLLMENT PROCESSING
      * 2005-11-30 KMENDEZ    HC-02811   DEPENDENT AGE-OUT LOGIC
      * 2007-03-22 TLIU       HC-03190   MENTAL HEALTH PARITY ACT
      * 2008-09-10 SWRIGHT    HC-03455   COBRA SUBSIDY (ARRA 2009)
      * 2010-07-01 SWRIGHT    HC-03920   DEPENDENT TO AGE 26 (ACA)
      * 2011-01-15 PGUPTA     HC-04105   GRANDFATHERED PLAN FLAG
      * 2013-10-01 PGUPTA     HC-04780   ACA EXCHANGE ENROLLMENT
      * 2014-01-02 PGUPTA     HC-04912   APTC / CSR TRACKING
      * 2014-03-31 JMCCARTHY  HC-05010   METAL TIER VALIDATION
      * 2014-09-15 PGUPTA     HC-05188   EHB VERIFICATION LOGIC
      * 2015-06-20 SWRIGHT    HC-05402   GRACE PERIOD PROCESSING
      * 2016-02-14 TLIU       HC-05690   FAMILY GLITCH FIX
      * 2017-08-30 KMENDEZ    HC-06012   SILVER LOADING LOGIC
      * 2019-04-01 PGUPTA     HC-06455   SHORT-TERM PLAN EXCLUSION
      * 2020-03-15 SWRIGHT    HC-06780   COVID SPECIAL ENROLLMENT
      * 2021-01-10 PGUPTA     HC-07020   ENHANCED APTC (ARP ACT)
      * 2022-06-15 KMENDEZ    HC-07340   FIX RETRO ACCUM RECALC
      * 2023-09-01 SWRIGHT    HC-07612   ICD-10 ELIG RULE UPDATES
      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID.    HCELIGVR.
       AUTHOR.        KOWALSKI-NAIPAUL.
       INSTALLATION.  HEALTHCARE INFORMATION SYSTEMS.
       DATE-WRITTEN.  1996-03-18.
       DATE-COMPILED.
       SECURITY.      CONFIDENTIAL - CONTAINS PHI/HIPAA DATA.
                      ACCESS RESTRICTED TO AUTHORIZED PERSONNEL.

      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-ES9000 WITH DEBUGGING MODE.
       OBJECT-COMPUTER.  IBM-ES9000.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ENRL-834-FILE
               ASSIGN TO ENRL834
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ENRL-834-STATUS.

           SELECT ELIG-270-FILE
               ASSIGN TO ELIG270
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ELIG-270-STATUS.

           SELECT ELIG-271-FILE
               ASSIGN TO ELIG271
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ELIG-271-STATUS.

           SELECT ENRL-RPT-FILE
               ASSIGN TO ENRLRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ENRL-RPT-STATUS.

           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO AUDTRL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.

      *================================================================*
       DATA DIVISION.
      *================================================================*
       FILE SECTION.

       FD  ENRL-834-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 500 CHARACTERS.
       01  ENRL-834-RECORD.
           05  E834-TRANS-TYPE         PIC X(03).
               88  E834-NEW-ENROLL         VALUE '021'.
               88  E834-TERMINATION        VALUE '024'.
               88  E834-DEPEND-ADD         VALUE '001'.
               88  E834-DEPEND-REMOVE      VALUE '002'.
               88  E834-PLAN-CHANGE        VALUE '025'.
               88  E834-COBRA-ELECT        VALUE '030'.
               88  E834-OPEN-ENROLL        VALUE '028'.
               88  E834-RETRO-CHANGE       VALUE '026'.
           05  E834-MEMBER-SSN         PIC X(09).
           05  E834-MEMBER-ID          PIC X(12).
           05  E834-SUBSCRIBER-IND     PIC X(01).
               88  E834-IS-SUBSCRIBER      VALUE 'Y'.
               88  E834-IS-DEPENDENT       VALUE 'N'.
           05  E834-RELATION-CODE      PIC X(02).
           05  E834-LAST-NAME          PIC X(30).
           05  E834-FIRST-NAME         PIC X(20).
           05  E834-MIDDLE-INIT        PIC X(01).
           05  E834-DOB                PIC X(08).
           05  E834-GENDER             PIC X(01).
           05  E834-EFF-DATE           PIC X(08).
           05  E834-TERM-DATE          PIC X(08).
           05  E834-PLAN-CODE          PIC X(08).
           05  E834-PRIOR-PLAN-CODE    PIC X(08).
           05  E834-GROUP-NUMBER       PIC X(10).
           05  E834-COBRA-QUAL-EVENT   PIC X(02).
           05  E834-COBRA-EVENT-DATE   PIC X(08).
           05  E834-ACA-EXCHANGE-IND   PIC X(01).
               88  E834-IS-EXCHANGE        VALUE 'Y'.
           05  E834-APTC-AMOUNT        PIC S9(07)V99.
           05  E834-CSR-LEVEL          PIC X(02).
           05  E834-METAL-TIER         PIC X(02).
               88  E834-TIER-BRONZE        VALUE 'BR'.
               88  E834-TIER-SILVER        VALUE 'SV'.
               88  E834-TIER-GOLD          VALUE 'GD'.
               88  E834-TIER-PLATINUM      VALUE 'PT'.
               88  E834-TIER-CATASTROPHIC  VALUE 'CA'.
           05  E834-SPEC-ENRL-REASON   PIC X(03).
           05  FILLER                  PIC X(260).

       FD  ELIG-270-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  ELIG-270-RECORD.
           05  E270-TRANS-ID           PIC X(10).
           05  E270-MEMBER-ID          PIC X(12).
           05  E270-SERVICE-DATE       PIC X(08).
           05  E270-SERVICE-TYPE       PIC X(03).
           05  E270-PROVIDER-NPI       PIC X(10).
           05  E270-PAYER-ID           PIC X(10).
           05  FILLER                  PIC X(247).

       FD  ELIG-271-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  ELIG-271-RECORD             PIC X(400).

       FD  ENRL-RPT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  ENRL-RPT-RECORD             PIC X(133).

       FD  AUDIT-TRAIL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 250 CHARACTERS.
       01  AUDIT-TRAIL-RECORD          PIC X(250).

      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*
       01  WS-PROGRAM-ID              PIC X(08)  VALUE 'HCELIGVR'.
       01  WS-VERSION                 PIC X(06)  VALUE '14.7.2'.

      *----------------------------------------------------------------*
      * FILE STATUS CODES
      *----------------------------------------------------------------*
       01  WS-ENRL-834-STATUS         PIC X(02)  VALUE SPACES.
       01  WS-ELIG-270-STATUS         PIC X(02)  VALUE SPACES.
       01  WS-ELIG-271-STATUS         PIC X(02)  VALUE SPACES.
       01  WS-ENRL-RPT-STATUS         PIC X(02)  VALUE SPACES.
       01  WS-AUDIT-STATUS            PIC X(02)  VALUE SPACES.

      *----------------------------------------------------------------*
      * CONTROL FLAGS AND SWITCHES
      *----------------------------------------------------------------*
       01  WS-FLAGS.
           05  WS-834-EOF-FLAG        PIC X(01)  VALUE 'N'.
               88  WS-834-EOF                    VALUE 'Y'.
               88  WS-834-NOT-EOF                VALUE 'N'.
           05  WS-270-EOF-FLAG        PIC X(01)  VALUE 'N'.
               88  WS-270-EOF                    VALUE 'Y'.
               88  WS-270-NOT-EOF                VALUE 'N'.
           05  WS-MEMBER-FOUND-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-MEMBER-FOUND               VALUE 'Y'.
               88  WS-MEMBER-NOT-FOUND           VALUE 'N'.
           05  WS-PLAN-VALID-FLAG     PIC X(01)  VALUE 'N'.
               88  WS-PLAN-VALID                 VALUE 'Y'.
               88  WS-PLAN-INVALID               VALUE 'N'.
           05  WS-ELIG-ACTIVE-FLAG    PIC X(01)  VALUE 'N'.
               88  WS-ELIG-ACTIVE                VALUE 'Y'.
               88  WS-ELIG-INACTIVE              VALUE 'N'.
           05  WS-ACCUM-OVERFLOW-FLAG PIC X(01)  VALUE 'N'.
               88  WS-ACCUM-OVERFLOW             VALUE 'Y'.
           05  WS-COBRA-ELIGIBLE-FLAG PIC X(01)  VALUE 'N'.
               88  WS-COBRA-ELIGIBLE             VALUE 'Y'.
           05  WS-ACA-VALID-FLAG      PIC X(01)  VALUE 'N'.
               88  WS-ACA-VALID                  VALUE 'Y'.
           05  WS-RETRO-FLAG          PIC X(01)  VALUE 'N'.
               88  WS-IS-RETRO                   VALUE 'Y'.
           05  WS-DB-ERROR-FLAG       PIC X(01)  VALUE 'N'.
               88  WS-DB-ERROR                   VALUE 'Y'.
           05  WS-PLAN-YEAR-RESET-FL  PIC X(01)  VALUE 'N'.
               88  WS-PLAN-YEAR-RESET            VALUE 'Y'.
           05  WS-GRACE-PERIOD-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-IN-GRACE-PERIOD            VALUE 'Y'.

      *----------------------------------------------------------------*
      * COUNTERS AND ACCUMULATORS
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-834-READ-CNT        PIC S9(09) COMP VALUE 0.
           05  WS-834-PROCESS-CNT     PIC S9(09) COMP VALUE 0.
           05  WS-834-ERROR-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-NEW-ENROLL-CNT      PIC S9(09) COMP VALUE 0.
           05  WS-TERM-CNT            PIC S9(09) COMP VALUE 0.
           05  WS-DEPEND-ADD-CNT      PIC S9(09) COMP VALUE 0.
           05  WS-DEPEND-REM-CNT      PIC S9(09) COMP VALUE 0.
           05  WS-PLAN-CHNG-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-COBRA-CNT           PIC S9(09) COMP VALUE 0.
           05  WS-OPEN-ENRL-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-RETRO-CHG-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-270-READ-CNT        PIC S9(09) COMP VALUE 0.
           05  WS-271-WRITE-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-ACCUM-UPDATE-CNT    PIC S9(09) COMP VALUE 0.
           05  WS-AUDIT-WRITE-CNT     PIC S9(09) COMP VALUE 0.
           05  WS-ACA-EXCHANGE-CNT    PIC S9(09) COMP VALUE 0.

      *----------------------------------------------------------------*
      * DATE AND TIME WORKING FIELDS
      *----------------------------------------------------------------*
       01  WS-CURRENT-DATE-TIME.
           05  WS-CURR-DATE.
               10  WS-CURR-YYYY       PIC 9(04).
               10  WS-CURR-MM         PIC 9(02).
               10  WS-CURR-DD         PIC 9(02).
           05  WS-CURR-TIME.
               10  WS-CURR-HH         PIC 9(02).
               10  WS-CURR-MN         PIC 9(02).
               10  WS-CURR-SS         PIC 9(02).
               10  WS-CURR-HS         PIC 9(02).

       01  WS-PLAN-YEAR-START         PIC 9(08)  VALUE ZEROES.
       01  WS-PLAN-YEAR-END           PIC 9(08)  VALUE ZEROES.

       01  WS-DATE-WORK.
           05  WS-DATE-WORK-YYYY      PIC 9(04).
           05  WS-DATE-WORK-MM        PIC 9(02).
           05  WS-DATE-WORK-DD        PIC 9(02).

       01  WS-DATE-NUMERIC            PIC 9(08)  VALUE ZEROES.
       01  WS-AGE-YEARS               PIC 9(03)  VALUE ZEROES.
       01  WS-COBRA-MAX-MONTHS        PIC 9(02)  VALUE 18.
       01  WS-COBRA-END-DATE          PIC 9(08)  VALUE ZEROES.
       01  WS-DEPEND-AGE-LIMIT        PIC 9(02)  VALUE 26.
       01  WS-GRACE-PERIOD-DAYS       PIC 9(03)  VALUE 90.

      *----------------------------------------------------------------*
      * BENEFIT ACCUMULATOR WORK AREA
      *----------------------------------------------------------------*
       01  WS-ACCUM-WORK.
           05  WS-DEDUCTIBLE-IND      PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-DEDUCTIBLE-FAM      PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-OOP-IND             PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-OOP-FAM             PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-LIFETIME-MAX-USED   PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-DEDUCTIBLE     PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-OOP-MAX        PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-LIFETIME-MAX   PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-FAM-DEDUCTIBLE      PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-FAM-OOP-MAX         PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.

      *----------------------------------------------------------------*
      * ACA / EXCHANGE WORK FIELDS
      *----------------------------------------------------------------*
       01  WS-ACA-WORK.
           05  WS-APTC-MONTHLY        PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-APTC-APPLIED        PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-CSR-VARIANT         PIC X(02)  VALUE SPACES.
           05  WS-METAL-TIER-CODE     PIC X(02)  VALUE SPACES.
           05  WS-EXCHANGE-PLAN-ID    PIC X(14)  VALUE SPACES.
           05  WS-EHB-COMPLIANT-FL    PIC X(01)  VALUE SPACES.
               88  WS-EHB-COMPLIANT             VALUE 'Y'.
           05  WS-AV-PERCENTAGE       PIC 9(03)  VALUE ZEROES.
           05  WS-ENHANCED-APTC-FL    PIC X(01)  VALUE 'N'.
               88  WS-ENHANCED-APTC             VALUE 'Y'.

      *----------------------------------------------------------------*
      * 271 RESPONSE WORK AREA
      *----------------------------------------------------------------*
       01  WS-271-RESPONSE.
           05  WS-271-TRANS-ID         PIC X(10).
           05  WS-271-MEMBER-ID        PIC X(12).
           05  WS-271-ELIG-STATUS      PIC X(01).
               88  WS-271-ACTIVE               VALUE '1'.
               88  WS-271-INACTIVE             VALUE '6'.
               88  WS-271-COBRA               VALUE 'C'.
           05  WS-271-PLAN-CODE        PIC X(08).
           05  WS-271-EFF-DATE         PIC X(08).
           05  WS-271-TERM-DATE        PIC X(08).
           05  WS-271-DEDUCT-REMAIN    PIC S9(07)V99.
           05  WS-271-OOP-REMAIN       PIC S9(07)V99.
           05  WS-271-COPAY-AMT        PIC S9(05)V99.
           05  WS-271-COINSURANCE      PIC 9(03).
           05  WS-271-REJECT-REASON    PIC X(03).
           05  FILLER                  PIC X(311).

      *----------------------------------------------------------------*
      * AUDIT TRAIL WORK AREA
      *----------------------------------------------------------------*
       01  WS-AUDIT-WORK.
           05  WS-AUD-TIMESTAMP        PIC X(26).
           05  WS-AUD-PROGRAM          PIC X(08).
           05  WS-AUD-USER-ID          PIC X(08).
           05  WS-AUD-ACTION           PIC X(04).
           05  WS-AUD-MEMBER-ID        PIC X(12).
           05  WS-AUD-TABLE-NAME       PIC X(20).
           05  WS-AUD-FIELD-NAME       PIC X(30).
           05  WS-AUD-OLD-VALUE        PIC X(50).
           05  WS-AUD-NEW-VALUE        PIC X(50).
           05  WS-AUD-REASON-CODE      PIC X(05).
           05  FILLER                  PIC X(37).

      *----------------------------------------------------------------*
      * REPORT LINE WORK AREAS
      *----------------------------------------------------------------*
       01  WS-RPT-HEADER-1.
           05  FILLER        PIC X(01) VALUE SPACES.
           05  FILLER        PIC X(50)
               VALUE 'HCELIGVR - ENROLLMENT/ELIGIBILITY BATCH REPORT'.
           05  FILLER        PIC X(40) VALUE SPACES.
           05  WS-RPT-DATE   PIC X(10).
           05  FILLER        PIC X(05) VALUE SPACES.
           05  WS-RPT-PAGE   PIC X(08) VALUE 'PAGE:   '.
           05  WS-RPT-PAGE-NO PIC Z(04)9.
           05  FILLER        PIC X(14) VALUE SPACES.

       01  WS-RPT-DETAIL.
           05  FILLER        PIC X(01) VALUE SPACES.
           05  WS-RPT-TRANS  PIC X(12).
           05  FILLER        PIC X(02) VALUE SPACES.
           05  WS-RPT-MBR-ID PIC X(12).
           05  FILLER        PIC X(02) VALUE SPACES.
           05  WS-RPT-NAME   PIC X(30).
           05  FILLER        PIC X(02) VALUE SPACES.
           05  WS-RPT-PLAN   PIC X(08).
           05  FILLER        PIC X(02) VALUE SPACES.
           05  WS-RPT-EFF    PIC X(10).
           05  FILLER        PIC X(02) VALUE SPACES.
           05  WS-RPT-STAT   PIC X(12).
           05  FILLER        PIC X(02) VALUE SPACES.
           05  WS-RPT-MSG    PIC X(24).
           05  FILLER        PIC X(12) VALUE SPACES.

       01  WS-RPT-PAGE-NUM             PIC 9(05)  VALUE ZEROES.
       01  WS-RPT-LINE-CNT            PIC 9(03)  VALUE 99.
       01  WS-RPT-LINES-PER-PAGE      PIC 9(03)  VALUE 55.

      *----------------------------------------------------------------*
      * COPY MEMBERS
      *----------------------------------------------------------------*
           COPY CPYPATIN.
           COPY CPYELIG.
           COPY CPYSQLCA.
           COPY CPYERROR.

      *----------------------------------------------------------------*
      * SYBASE HOST VARIABLES
      *----------------------------------------------------------------*
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  HV-MEMBER-ELIG.
           05  HV-ME-MEMBER-ID        PIC X(12).
           05  HV-ME-SSN              PIC X(09).
           05  HV-ME-LAST-NAME        PIC X(30).
           05  HV-ME-FIRST-NAME       PIC X(20).
           05  HV-ME-DOB              PIC X(08).
           05  HV-ME-GENDER           PIC X(01).
           05  HV-ME-PLAN-CODE        PIC X(08).
           05  HV-ME-GROUP-NUM        PIC X(10).
           05  HV-ME-EFF-DATE         PIC X(08).
           05  HV-ME-TERM-DATE        PIC X(08).
           05  HV-ME-STATUS           PIC X(01).
           05  HV-ME-SUBSCRIBER-ID    PIC X(12).
           05  HV-ME-RELATION-CODE    PIC X(02).
           05  HV-ME-COBRA-IND        PIC X(01).
           05  HV-ME-ACA-EXCHANGE-IND PIC X(01).
           05  HV-ME-METAL-TIER       PIC X(02).

       01  HV-MEMBER-ACCUM.
           05  HV-MA-MEMBER-ID        PIC X(12).
           05  HV-MA-PLAN-YEAR        PIC X(04).
           05  HV-MA-DEDUCTIBLE-USED  PIC S9(07)V99.
           05  HV-MA-OOP-USED         PIC S9(07)V99.
           05  HV-MA-LIFETIME-USED    PIC S9(09)V99.
           05  HV-MA-LAST-UPDATED     PIC X(26).

       01  HV-FAMILY-ACCUM.
           05  HV-FA-SUBSCRIBER-ID    PIC X(12).
           05  HV-FA-PLAN-YEAR        PIC X(04).
           05  HV-FA-DEDUCTIBLE-USED  PIC S9(07)V99.
           05  HV-FA-OOP-USED         PIC S9(07)V99.
           05  HV-FA-LAST-UPDATED     PIC X(26).

       01  HV-PLAN-BENEFIT.
           05  HV-PB-PLAN-CODE        PIC X(08).
           05  HV-PB-IND-DEDUCTIBLE   PIC S9(07)V99.
           05  HV-PB-FAM-DEDUCTIBLE   PIC S9(07)V99.
           05  HV-PB-IND-OOP-MAX      PIC S9(07)V99.
           05  HV-PB-FAM-OOP-MAX      PIC S9(07)V99.
           05  HV-PB-LIFETIME-MAX     PIC S9(09)V99.
           05  HV-PB-COPAY-PCP        PIC S9(05)V99.
           05  HV-PB-COINSURANCE      PIC 9(03).
           05  HV-PB-PLAN-YEAR-START  PIC X(04).
           05  HV-PB-EHB-COMPLIANT    PIC X(01).
           05  HV-PB-METAL-TIER       PIC X(02).
           05  HV-PB-AV-PERCENT       PIC 9(03).

       01  HV-AUDIT-TRAIL.
           05  HV-AT-TIMESTAMP        PIC X(26).
           05  HV-AT-PROGRAM-ID       PIC X(08).
           05  HV-AT-USER-ID          PIC X(08).
           05  HV-AT-ACTION-CODE      PIC X(04).
           05  HV-AT-MEMBER-ID        PIC X(12).
           05  HV-AT-TABLE-NAME       PIC X(20).
           05  HV-AT-FIELD-NAME       PIC X(30).
           05  HV-AT-OLD-VALUE        PIC X(50).
           05  HV-AT-NEW-VALUE        PIC X(50).
           05  HV-AT-REASON-CODE      PIC X(05).

       01  HV-COBRA-TRACK.
           05  HV-CT-MEMBER-ID        PIC X(12).
           05  HV-CT-QUAL-EVENT       PIC X(02).
           05  HV-CT-EVENT-DATE       PIC X(08).
           05  HV-CT-ELECT-DATE       PIC X(08).
           05  HV-CT-END-DATE         PIC X(08).
           05  HV-CT-STATUS           PIC X(01).
           05  HV-CT-SUBSIDY-IND      PIC X(01).

       01  HV-ACA-EXCHANGE.
           05  HV-AX-MEMBER-ID        PIC X(12).
           05  HV-AX-EXCHANGE-PLAN-ID PIC X(14).
           05  HV-AX-APTC-AMOUNT      PIC S9(07)V99.
           05  HV-AX-APTC-APPLIED     PIC S9(07)V99.
           05  HV-AX-CSR-LEVEL        PIC X(02).
           05  HV-AX-METAL-TIER       PIC X(02).
           05  HV-AX-EHB-IND          PIC X(01).
           05  HV-AX-ENHANCED-APTC    PIC X(01).
           05  HV-AX-EFF-DATE         PIC X(08).
           05  HV-AX-TERM-DATE        PIC X(08).

       01  HV-SERVICE-DATE            PIC X(08).
       01  HV-PLAN-CODE-IN            PIC X(08).
       01  HV-SUBSCRIBER-ID-IN        PIC X(12).

           EXEC SQL END DECLARE SECTION END-EXEC.

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-MAIN-CONTROL.
      *----------------------------------------------------------------*
           PERFORM 1000-INITIALIZATION

           PERFORM 2000-PROCESS-834-ENROLLMENTS

           PERFORM 3000-PROCESS-270-INQUIRIES

           PERFORM 8000-WRITE-SUMMARY-REPORT

           PERFORM 9000-TERMINATION

           STOP RUN
           .

      *----------------------------------------------------------------*
       1000-INITIALIZATION.
      *----------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME

           PERFORM 1100-CONNECT-DATABASE

           PERFORM 1200-OPEN-FILES

           INITIALIZE WS-COUNTERS
           INITIALIZE WS-FLAGS

           MOVE 'N' TO WS-834-EOF-FLAG
           MOVE 'N' TO WS-270-EOF-FLAG
           .

      *----------------------------------------------------------------*
       1100-CONNECT-DATABASE.
      *----------------------------------------------------------------*
           EXEC SQL
               CONNECT TO HCMSDB
               USER 'hcbatch' IDENTIFIED BY 'hcb$ecur3'
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCELIGVR: DATABASE CONNECT FAILED, SQLCODE='
                   SQLCODE
               MOVE 820 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF
           .

      *----------------------------------------------------------------*
       1200-OPEN-FILES.
      *----------------------------------------------------------------*
           OPEN INPUT  ENRL-834-FILE
           IF WS-ENRL-834-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ENRL-834-FILE, '
                   'STATUS=' WS-ENRL-834-STATUS
               MOVE 810 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN INPUT  ELIG-270-FILE
           IF WS-ELIG-270-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ELIG-270-FILE, '
                   'STATUS=' WS-ELIG-270-STATUS
               MOVE 810 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT ELIG-271-FILE
           IF WS-ELIG-271-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ELIG-271-FILE, '
                   'STATUS=' WS-ELIG-271-STATUS
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT ENRL-RPT-FILE
           IF WS-ENRL-RPT-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ENRL-RPT-FILE, '
                   'STATUS=' WS-ENRL-RPT-STATUS
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT AUDIT-TRAIL-FILE
           IF WS-AUDIT-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN AUDIT-TRAIL-FILE, '
                   'STATUS=' WS-AUDIT-STATUS
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF
           .

      *----------------------------------------------------------------*
       2000-PROCESS-834-ENROLLMENTS.
      *----------------------------------------------------------------*
           PERFORM 2010-READ-834-RECORD

           PERFORM UNTIL WS-834-EOF
               ADD 1 TO WS-834-PROCESS-CNT

               EVALUATE TRUE
                   WHEN E834-NEW-ENROLL
                       PERFORM 2100-NEW-ENROLLMENT
                   WHEN E834-TERMINATION
                       PERFORM 2200-MEMBER-TERMINATION
                   WHEN E834-DEPEND-ADD
                       PERFORM 2300-DEPENDENT-ADD
                   WHEN E834-DEPEND-REMOVE
                       PERFORM 2400-DEPENDENT-REMOVE
                   WHEN E834-PLAN-CHANGE
                       PERFORM 2500-PLAN-CHANGE
                   WHEN E834-COBRA-ELECT
                       PERFORM 2600-COBRA-CONTINUATION
                   WHEN E834-OPEN-ENROLL
                       PERFORM 2700-OPEN-ENROLLMENT
                   WHEN E834-RETRO-CHANGE
                       PERFORM 2800-RETROACTIVE-CHANGE
                   WHEN OTHER
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'INVALID TRANS TYPE' TO WS-RPT-MSG
                       PERFORM 8100-WRITE-DETAIL-LINE
               END-EVALUATE

               PERFORM 2010-READ-834-RECORD
           END-PERFORM
           .

      *----------------------------------------------------------------*
       2010-READ-834-RECORD.
      *----------------------------------------------------------------*
           READ ENRL-834-FILE INTO ENRL-834-RECORD
               AT END
                   SET WS-834-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-834-READ-CNT
           END-READ
           .

      *----------------------------------------------------------------*
       2100-NEW-ENROLLMENT.
      *----------------------------------------------------------------*
      *    VERIFY MEMBER DOES NOT ALREADY HAVE ACTIVE ENROLLMENT
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               SELECT status
               INTO   :HV-ME-STATUS
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status = 'A'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'ALREADY ACTIVE     ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
           ELSE
      *        VALIDATE PLAN CODE EXISTS AND IS ACTIVE
               MOVE E834-PLAN-CODE TO HV-PLAN-CODE-IN
               PERFORM 4000-VALIDATE-PLAN

               IF WS-PLAN-VALID
      *            IF ACA EXCHANGE, VALIDATE METAL TIER AND EHB
                   IF E834-IS-EXCHANGE
                       PERFORM 5000-ACA-EXCHANGE-PROCESSING
                   END-IF

                   IF WS-ACA-VALID OR NOT E834-IS-EXCHANGE

                       EXEC SQL
                           INSERT INTO T_MEMBER_ELIG
                               (member_id, ssn, last_name,
                                first_name, dob, gender,
                                plan_code, group_num,
                                eff_date, term_date, status,
                                subscriber_id, relation_code,
                                cobra_ind, aca_exchange_ind,
                                metal_tier)
                           VALUES
                               (:E834-MEMBER-ID,
                                :E834-MEMBER-SSN,
                                :E834-LAST-NAME,
                                :E834-FIRST-NAME,
                                :E834-DOB,
                                :E834-GENDER,
                                :E834-PLAN-CODE,
                                :E834-GROUP-NUMBER,
                                :E834-EFF-DATE,
                                '99991231', 'A',
                                :E834-MEMBER-ID,
                                :E834-RELATION-CODE,
                                'N',
                                :E834-ACA-EXCHANGE-IND,
                                :E834-METAL-TIER)
                       END-EXEC

                       IF SQLCODE = 0
                           ADD 1 TO WS-NEW-ENROLL-CNT
                           PERFORM 4200-INITIALIZE-ACCUMULATORS
                           MOVE 'ENRL' TO WS-AUD-ACTION
                           PERFORM 7000-WRITE-AUDIT-TRAIL
                           MOVE 'ENROLLED OK        ' TO WS-RPT-MSG
                       ELSE
                           ADD 1 TO WS-834-ERROR-CNT
                           MOVE 'DB INSERT FAILED   ' TO WS-RPT-MSG
                       END-IF

                       PERFORM 8100-WRITE-DETAIL-LINE
                   END-IF
               ELSE
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'INVALID PLAN CODE  ' TO WS-RPT-MSG
                   PERFORM 8100-WRITE-DETAIL-LINE
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       2200-MEMBER-TERMINATION.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status    = 'T',
                      term_date = :E834-TERM-DATE
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'A'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-TERM-CNT
               MOVE 'TERM' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'TERMINATED OK      ' TO WS-RPT-MSG

      *        CHECK IF RETRO TERMINATION REQUIRES ACCUM RECALC
               IF E834-TERM-DATE < WS-CURR-DATE
                   SET WS-IS-RETRO TO TRUE
                   PERFORM 6000-RETRO-ACCUM-RECALC
               END-IF
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'MEMBER NOT ACTIVE  ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2300-DEPENDENT-ADD.
      *----------------------------------------------------------------*
      *    VALIDATE SUBSCRIBER EXISTS AND IS ACTIVE
           MOVE E834-MEMBER-ID TO HV-SUBSCRIBER-ID-IN

           EXEC SQL
               SELECT member_id, plan_code
               INTO   :HV-ME-MEMBER-ID, :HV-ME-PLAN-CODE
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-SUBSCRIBER-ID-IN
               AND    status    = 'A'
               AND    relation_code = '18'
           END-EXEC

           IF SQLCODE = 0
      *        CHECK DEPENDENT AGE (ACA: UNDER 26)
               PERFORM 4100-CALCULATE-AGE

               IF WS-AGE-YEARS < WS-DEPEND-AGE-LIMIT

                   EXEC SQL
                       INSERT INTO T_MEMBER_ELIG
                           (member_id, ssn, last_name,
                            first_name, dob, gender,
                            plan_code, group_num,
                            eff_date, term_date, status,
                            subscriber_id, relation_code,
                            cobra_ind, aca_exchange_ind,
                            metal_tier)
                       VALUES
                           (:E834-MEMBER-ID,
                            :E834-MEMBER-SSN,
                            :E834-LAST-NAME,
                            :E834-FIRST-NAME,
                            :E834-DOB,
                            :E834-GENDER,
                            :HV-ME-PLAN-CODE,
                            :E834-GROUP-NUMBER,
                            :E834-EFF-DATE,
                            '99991231', 'A',
                            :HV-SUBSCRIBER-ID-IN,
                            :E834-RELATION-CODE,
                            'N',
                            :E834-ACA-EXCHANGE-IND,
                            :E834-METAL-TIER)
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-DEPEND-ADD-CNT
                       PERFORM 4200-INITIALIZE-ACCUMULATORS
                       MOVE 'DADD' TO WS-AUD-ACTION
                       PERFORM 7000-WRITE-AUDIT-TRAIL
                       MOVE 'DEPENDENT ADDED    ' TO WS-RPT-MSG
                   ELSE
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'DEP INSERT FAILED  ' TO WS-RPT-MSG
                   END-IF
               ELSE
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'DEP OVER AGE LIMIT ' TO WS-RPT-MSG
               END-IF
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'SUBSCRIBER NOT FOUND' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2400-DEPENDENT-REMOVE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status    = 'T',
                      term_date = :E834-TERM-DATE
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'A'
               AND    relation_code <> '18'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-DEPEND-REM-CNT
               MOVE 'DREM' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               PERFORM 4300-FAMILY-ACCUM-ROLLUP
               MOVE 'DEPENDENT REMOVED  ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'DEP NOT FOUND/ACTIV' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2500-PLAN-CHANGE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           MOVE E834-PLAN-CODE TO HV-PLAN-CODE-IN
           PERFORM 4000-VALIDATE-PLAN

           IF WS-PLAN-VALID
      *        RETRIEVE CURRENT PLAN FOR AUDIT
               EXEC SQL
                   SELECT plan_code
                   INTO   :HV-ME-PLAN-CODE
                   FROM   T_MEMBER_ELIG
                   WHERE  member_id = :HV-ME-MEMBER-ID
                   AND    status    = 'A'
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-ME-PLAN-CODE TO WS-AUD-OLD-VALUE

                   EXEC SQL
                       UPDATE T_MEMBER_ELIG
                       SET    plan_code = :E834-PLAN-CODE,
                              metal_tier = :E834-METAL-TIER
                       WHERE  member_id = :HV-ME-MEMBER-ID
                       AND    status    = 'A'
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-PLAN-CHNG-CNT
                       MOVE E834-PLAN-CODE TO WS-AUD-NEW-VALUE
                       MOVE 'PCHG' TO WS-AUD-ACTION
                       PERFORM 7000-WRITE-AUDIT-TRAIL
      *                RESET ACCUMULATORS FOR NEW PLAN IF DIFFERENT
      *                DEDUCTIBLE/OOP STRUCTURE
                       PERFORM 4500-PLAN-YEAR-RESET-CHECK
                       MOVE 'PLAN CHANGED OK    ' TO WS-RPT-MSG
                   ELSE
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'PLAN UPDATE FAILED ' TO WS-RPT-MSG
                   END-IF
               ELSE
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'MEMBER NOT ACTIVE  ' TO WS-RPT-MSG
               END-IF
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'INVALID NEW PLAN   ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2600-COBRA-CONTINUATION.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

      *    VERIFY MEMBER HAS RECENT TERMINATION (WITHIN 60 DAYS)
           EXEC SQL
               SELECT member_id, plan_code, term_date
               INTO   :HV-ME-MEMBER-ID,
                      :HV-ME-PLAN-CODE,
                      :HV-ME-TERM-DATE
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'T'
           END-EXEC

           IF SQLCODE = 0
      *        CALCULATE COBRA END DATE (18 OR 36 MONTHS)
               IF E834-COBRA-QUAL-EVENT = '01' OR '02' OR '03'
                   MOVE 18 TO WS-COBRA-MAX-MONTHS
               ELSE
                   MOVE 36 TO WS-COBRA-MAX-MONTHS
               END-IF

      *        REACTIVATE MEMBER WITH COBRA FLAG
               EXEC SQL
                   UPDATE T_MEMBER_ELIG
                   SET    status    = 'A',
                          cobra_ind = 'Y',
                          term_date = '99991231'
                   WHERE  member_id = :HV-ME-MEMBER-ID
                   AND    status    = 'T'
               END-EXEC

               IF SQLCODE = 0
      *            INSERT COBRA TRACKING RECORD
                   MOVE E834-MEMBER-ID     TO HV-CT-MEMBER-ID
                   MOVE E834-COBRA-QUAL-EVENT TO HV-CT-QUAL-EVENT
                   MOVE E834-COBRA-EVENT-DATE  TO HV-CT-EVENT-DATE
                   MOVE E834-EFF-DATE      TO HV-CT-ELECT-DATE
                   MOVE 'A'                TO HV-CT-STATUS
                   MOVE 'N'                TO HV-CT-SUBSIDY-IND

                   EXEC SQL
                       INSERT INTO T_COBRA_TRACK
                           (member_id, qual_event, event_date,
                            elect_date, end_date, status,
                            subsidy_ind)
                       VALUES
                           (:HV-CT-MEMBER-ID,
                            :HV-CT-QUAL-EVENT,
                            :HV-CT-EVENT-DATE,
                            :HV-CT-ELECT-DATE,
                            :HV-CT-END-DATE,
                            :HV-CT-STATUS,
                            :HV-CT-SUBSIDY-IND)
                   END-EXEC

                   ADD 1 TO WS-COBRA-CNT
                   MOVE 'COBR' TO WS-AUD-ACTION
                   PERFORM 7000-WRITE-AUDIT-TRAIL
                   MOVE 'COBRA ACTIVATED    ' TO WS-RPT-MSG
               ELSE
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'COBRA REACT FAILED ' TO WS-RPT-MSG
               END-IF
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'NO RECENT TERM FOUND' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2700-OPEN-ENROLLMENT.
      *----------------------------------------------------------------*
      *    OPEN ENROLLMENT - EFFECTIVE NEXT PLAN YEAR
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           MOVE E834-PLAN-CODE TO HV-PLAN-CODE-IN
           PERFORM 4000-VALIDATE-PLAN

           IF WS-PLAN-VALID
               EXEC SQL
                   SELECT member_id, plan_code
                   INTO   :HV-ME-MEMBER-ID, :HV-ME-PLAN-CODE
                   FROM   T_MEMBER_ELIG
                   WHERE  member_id = :HV-ME-MEMBER-ID
                   AND    status    = 'A'
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-ME-PLAN-CODE TO WS-AUD-OLD-VALUE

      *            TERMINATE CURRENT PLAN AT END OF CURRENT YEAR
                   EXEC SQL
                       UPDATE T_MEMBER_ELIG
                       SET    term_date = :WS-PLAN-YEAR-END
                       WHERE  member_id = :HV-ME-MEMBER-ID
                       AND    status    = 'A'
                       AND    plan_code = :HV-ME-PLAN-CODE
                   END-EXEC

      *            INSERT NEW ENROLLMENT EFFECTIVE NEXT PLAN YEAR
                   EXEC SQL
                       INSERT INTO T_MEMBER_ELIG
                           (member_id, ssn, last_name,
                            first_name, dob, gender,
                            plan_code, group_num,
                            eff_date, term_date, status,
                            subscriber_id, relation_code,
                            cobra_ind, aca_exchange_ind,
                            metal_tier)
                       SELECT member_id, ssn, last_name,
                              first_name, dob, gender,
                              :E834-PLAN-CODE, group_num,
                              :E834-EFF-DATE, '99991231', 'P',
                              subscriber_id, relation_code,
                              cobra_ind,
                              :E834-ACA-EXCHANGE-IND,
                              :E834-METAL-TIER
                       FROM   T_MEMBER_ELIG
                       WHERE  member_id = :HV-ME-MEMBER-ID
                       AND    plan_code = :HV-ME-PLAN-CODE
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-OPEN-ENRL-CNT
                       MOVE E834-PLAN-CODE TO WS-AUD-NEW-VALUE
                       MOVE 'OPEN' TO WS-AUD-ACTION
                       PERFORM 7000-WRITE-AUDIT-TRAIL
                       MOVE 'OPEN ENRL QUEUED   ' TO WS-RPT-MSG
                   ELSE
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'OE INSERT FAILED   ' TO WS-RPT-MSG
                   END-IF
               ELSE
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'MEMBER NOT ACTIVE  ' TO WS-RPT-MSG
               END-IF
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'INVALID OE PLAN    ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2800-RETROACTIVE-CHANGE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           SET WS-IS-RETRO TO TRUE

      *    UPDATE ELIGIBILITY WITH RETROACTIVE EFFECTIVE DATE
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    eff_date  = :E834-EFF-DATE,
                      plan_code = :E834-PLAN-CODE
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status IN ('A', 'T')
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-RETRO-CHG-CNT
               MOVE 'RETR' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL

      *        RECALCULATE ALL ACCUMULATORS FROM RETRO DATE
               PERFORM 6000-RETRO-ACCUM-RECALC

               MOVE 'RETRO CHANGE OK    ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'RETRO UPDATE FAILED' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       3000-PROCESS-270-INQUIRIES.
      *----------------------------------------------------------------*
           PERFORM 3010-READ-270-RECORD

           PERFORM UNTIL WS-270-EOF
               PERFORM 3100-VERIFY-ELIGIBILITY
               PERFORM 3010-READ-270-RECORD
           END-PERFORM
           .

      *----------------------------------------------------------------*
       3010-READ-270-RECORD.
      *----------------------------------------------------------------*
           READ ELIG-270-FILE INTO ELIG-270-RECORD
               AT END
                   SET WS-270-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-270-READ-CNT
           END-READ
           .

      *----------------------------------------------------------------*
       3100-VERIFY-ELIGIBILITY.
      *----------------------------------------------------------------*
      *    REAL-TIME 270/271 ELIGIBILITY VERIFICATION
           MOVE E270-MEMBER-ID   TO HV-ME-MEMBER-ID
           MOVE E270-SERVICE-DATE TO HV-SERVICE-DATE

           INITIALIZE WS-271-RESPONSE
           MOVE E270-TRANS-ID    TO WS-271-TRANS-ID
           MOVE E270-MEMBER-ID   TO WS-271-MEMBER-ID

      *    CHECK DATE-OF-SERVICE ELIGIBILITY
           EXEC SQL
               SELECT me.plan_code, me.eff_date, me.term_date,
                      me.status, me.cobra_ind, me.subscriber_id
               INTO   :HV-ME-PLAN-CODE, :HV-ME-EFF-DATE,
                      :HV-ME-TERM-DATE, :HV-ME-STATUS,
                      :HV-ME-COBRA-IND, :HV-ME-SUBSCRIBER-ID
               FROM   T_MEMBER_ELIG me
               WHERE  me.member_id = :HV-ME-MEMBER-ID
               AND    me.eff_date <= :HV-SERVICE-DATE
               AND    (me.term_date >= :HV-SERVICE-DATE
                       OR me.term_date = '99991231')
               AND    me.status IN ('A', 'P')
           END-EXEC

           IF SQLCODE = 0
               SET WS-ELIG-ACTIVE TO TRUE
               MOVE HV-ME-PLAN-CODE TO WS-271-PLAN-CODE
               MOVE HV-ME-EFF-DATE  TO WS-271-EFF-DATE
               MOVE HV-ME-TERM-DATE TO WS-271-TERM-DATE

               IF HV-ME-COBRA-IND = 'Y'
                   SET WS-271-COBRA TO TRUE
      *            CHECK COBRA GRACE PERIOD
                   PERFORM 3200-CHECK-GRACE-PERIOD
               ELSE
                   SET WS-271-ACTIVE TO TRUE
               END-IF

      *        RETRIEVE BENEFIT ACCUMULATORS
               PERFORM 3300-GET-ACCUMULATORS
      *        CHECK FOR PLAN YEAR RESET
               PERFORM 4500-PLAN-YEAR-RESET-CHECK

               MOVE SPACES TO WS-271-REJECT-REASON
           ELSE
               SET WS-ELIG-INACTIVE TO TRUE
               SET WS-271-INACTIVE  TO TRUE
               MOVE '072' TO WS-271-REJECT-REASON
           END-IF

      *    WRITE 271 RESPONSE
           MOVE WS-271-RESPONSE TO ELIG-271-RECORD
           WRITE ELIG-271-RECORD
           ADD 1 TO WS-271-WRITE-CNT
           .

      *----------------------------------------------------------------*
       3200-CHECK-GRACE-PERIOD.
      *----------------------------------------------------------------*
      *    ACA 90-DAY GRACE PERIOD FOR EXCHANGE PLANS / APTC MEMBERS
           MOVE E270-MEMBER-ID TO HV-AX-MEMBER-ID

           EXEC SQL
               SELECT aptc_amount
               INTO   :HV-AX-APTC-AMOUNT
               FROM   T_ACA_EXCHANGE
               WHERE  member_id = :HV-AX-MEMBER-ID
               AND    eff_date <= :HV-SERVICE-DATE
               AND    (term_date >= :HV-SERVICE-DATE
                       OR term_date = '99991231')
           END-EXEC

           IF SQLCODE = 0 AND HV-AX-APTC-AMOUNT > 0
               MOVE 90 TO WS-GRACE-PERIOD-DAYS
           ELSE
               MOVE 31 TO WS-GRACE-PERIOD-DAYS
           END-IF

           SET WS-IN-GRACE-PERIOD TO TRUE
           .

      *----------------------------------------------------------------*
       3300-GET-ACCUMULATORS.
      *----------------------------------------------------------------*
      *    RETRIEVE INDIVIDUAL ACCUMULATORS
           MOVE WS-CURR-YYYY TO HV-MA-PLAN-YEAR

           EXEC SQL
               SELECT deductible_used, oop_used, lifetime_used
               INTO   :HV-MA-DEDUCTIBLE-USED,
                      :HV-MA-OOP-USED,
                      :HV-MA-LIFETIME-USED
               FROM   T_MEMBER_ACCUM
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    plan_year  = :HV-MA-PLAN-YEAR
           END-EXEC

           IF SQLCODE = 0
      *        RETRIEVE PLAN BENEFIT LIMITS
               EXEC SQL
                   SELECT ind_deductible, ind_oop_max,
                          lifetime_max, copay_pcp, coinsurance
                   INTO   :HV-PB-IND-DEDUCTIBLE,
                          :HV-PB-IND-OOP-MAX,
                          :HV-PB-LIFETIME-MAX,
                          :HV-PB-COPAY-PCP,
                          :HV-PB-COINSURANCE
                   FROM   T_PLAN_BENEFIT
                   WHERE  plan_code = :HV-ME-PLAN-CODE
               END-EXEC

               IF SQLCODE = 0
      *            CALCULATE REMAINING AMOUNTS
                   COMPUTE WS-271-DEDUCT-REMAIN =
                       HV-PB-IND-DEDUCTIBLE - HV-MA-DEDUCTIBLE-USED
                   IF WS-271-DEDUCT-REMAIN < 0
                       MOVE ZEROES TO WS-271-DEDUCT-REMAIN
                   END-IF

                   COMPUTE WS-271-OOP-REMAIN =
                       HV-PB-IND-OOP-MAX - HV-MA-OOP-USED
                   IF WS-271-OOP-REMAIN < 0
                       MOVE ZEROES TO WS-271-OOP-REMAIN
                   END-IF

                   MOVE HV-PB-COPAY-PCP TO WS-271-COPAY-AMT
                   MOVE HV-PB-COINSURANCE TO WS-271-COINSURANCE
               ELSE
                   MOVE 850 TO ERR-ABEND-CODE
                   DISPLAY 'HCELIGVR: PLAN CONFIG MISSING FOR '
                       HV-ME-PLAN-CODE
                   PERFORM 9500-ABEND-ROUTINE
               END-IF
           ELSE
      *        NO ACCUMULATORS YET - RETURN FULL BENEFIT
               PERFORM 4200-INITIALIZE-ACCUMULATORS
               PERFORM 3300-GET-ACCUMULATORS
           END-IF
           .

      *----------------------------------------------------------------*
       4000-VALIDATE-PLAN.
      *----------------------------------------------------------------*
           SET WS-PLAN-INVALID TO TRUE

           EXEC SQL
               SELECT plan_code, ind_deductible, fam_deductible,
                      ind_oop_max, fam_oop_max, lifetime_max,
                      plan_year_start, ehb_compliant,
                      metal_tier, av_percent
               INTO   :HV-PB-PLAN-CODE,
                      :HV-PB-IND-DEDUCTIBLE,
                      :HV-PB-FAM-DEDUCTIBLE,
                      :HV-PB-IND-OOP-MAX,
                      :HV-PB-FAM-OOP-MAX,
                      :HV-PB-LIFETIME-MAX,
                      :HV-PB-PLAN-YEAR-START,
                      :HV-PB-EHB-COMPLIANT,
                      :HV-PB-METAL-TIER,
                      :HV-PB-AV-PERCENT
               FROM   T_PLAN_BENEFIT
               WHERE  plan_code = :HV-PLAN-CODE-IN
           END-EXEC

           IF SQLCODE = 0
               SET WS-PLAN-VALID TO TRUE
      *        STORE PLAN LIMITS FOR ACCUMULATOR PROCESSING
               MOVE HV-PB-IND-DEDUCTIBLE TO WS-PLAN-DEDUCTIBLE
               MOVE HV-PB-IND-OOP-MAX    TO WS-PLAN-OOP-MAX
               MOVE HV-PB-LIFETIME-MAX   TO WS-PLAN-LIFETIME-MAX
               MOVE HV-PB-FAM-DEDUCTIBLE TO WS-FAM-DEDUCTIBLE
               MOVE HV-PB-FAM-OOP-MAX    TO WS-FAM-OOP-MAX
           END-IF
           .

      *----------------------------------------------------------------*
       4100-CALCULATE-AGE.
      *----------------------------------------------------------------*
           MOVE E834-DOB TO WS-DATE-WORK
           COMPUTE WS-AGE-YEARS =
               WS-CURR-YYYY - WS-DATE-WORK-YYYY
           IF WS-CURR-MM < WS-DATE-WORK-MM
               SUBTRACT 1 FROM WS-AGE-YEARS
           ELSE
               IF WS-CURR-MM = WS-DATE-WORK-MM
                   IF WS-CURR-DD < WS-DATE-WORK-DD
                       SUBTRACT 1 FROM WS-AGE-YEARS
                   END-IF
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4200-INITIALIZE-ACCUMULATORS.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-MA-MEMBER-ID
           MOVE WS-CURR-YYYY   TO HV-MA-PLAN-YEAR
           MOVE ZEROES          TO HV-MA-DEDUCTIBLE-USED
           MOVE ZEROES          TO HV-MA-OOP-USED
           MOVE ZEROES          TO HV-MA-LIFETIME-USED
           MOVE WS-CURRENT-DATE-TIME TO HV-MA-LAST-UPDATED

           EXEC SQL
               INSERT INTO T_MEMBER_ACCUM
                   (member_id, plan_year, deductible_used,
                    oop_used, lifetime_used, last_updated)
               VALUES
                   (:HV-MA-MEMBER-ID, :HV-MA-PLAN-YEAR,
                    :HV-MA-DEDUCTIBLE-USED, :HV-MA-OOP-USED,
                    :HV-MA-LIFETIME-USED, :HV-MA-LAST-UPDATED)
           END-EXEC

           IF SQLCODE NOT = 0
      *        MIGHT ALREADY EXIST, NOT AN ERROR
               CONTINUE
           END-IF

           ADD 1 TO WS-ACCUM-UPDATE-CNT
           .

      *----------------------------------------------------------------*
       4300-FAMILY-ACCUM-ROLLUP.
      *----------------------------------------------------------------*
      *    RECALCULATE FAMILY ACCUMULATORS BY SUMMING ALL MEMBERS
           MOVE HV-ME-SUBSCRIBER-ID TO HV-FA-SUBSCRIBER-ID
           MOVE WS-CURR-YYYY TO HV-FA-PLAN-YEAR

           EXEC SQL
               SELECT SUM(ma.deductible_used),
                      SUM(ma.oop_used)
               INTO   :HV-FA-DEDUCTIBLE-USED,
                      :HV-FA-OOP-USED
               FROM   T_MEMBER_ACCUM ma
               JOIN   T_MEMBER_ELIG me
                 ON   ma.member_id = me.member_id
               WHERE  me.subscriber_id = :HV-FA-SUBSCRIBER-ID
               AND    me.status        = 'A'
               AND    ma.plan_year     = :HV-FA-PLAN-YEAR
           END-EXEC

           IF SQLCODE = 0
      *        CAP FAMILY ACCUMULATORS AT FAMILY LIMITS
               IF HV-FA-DEDUCTIBLE-USED > WS-FAM-DEDUCTIBLE
                   MOVE WS-FAM-DEDUCTIBLE TO HV-FA-DEDUCTIBLE-USED
               END-IF
               IF HV-FA-OOP-USED > WS-FAM-OOP-MAX
                   MOVE WS-FAM-OOP-MAX TO HV-FA-OOP-USED
               END-IF

               MOVE WS-CURRENT-DATE-TIME TO HV-FA-LAST-UPDATED

               EXEC SQL
                   UPDATE T_FAMILY_ACCUM
                   SET    deductible_used = :HV-FA-DEDUCTIBLE-USED,
                          oop_used        = :HV-FA-OOP-USED,
                          last_updated    = :HV-FA-LAST-UPDATED
                   WHERE  subscriber_id   = :HV-FA-SUBSCRIBER-ID
                   AND    plan_year       = :HV-FA-PLAN-YEAR
               END-EXEC

               IF SQLCODE NOT = 0
      *            INSERT IF NOT EXISTS
                   EXEC SQL
                       INSERT INTO T_FAMILY_ACCUM
                           (subscriber_id, plan_year,
                            deductible_used, oop_used,
                            last_updated)
                       VALUES
                           (:HV-FA-SUBSCRIBER-ID,
                            :HV-FA-PLAN-YEAR,
                            :HV-FA-DEDUCTIBLE-USED,
                            :HV-FA-OOP-USED,
                            :HV-FA-LAST-UPDATED)
                   END-EXEC
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4500-PLAN-YEAR-RESET-CHECK.
      *----------------------------------------------------------------*
      *    CHECK IF WE HAVE CROSSED A PLAN YEAR BOUNDARY
      *    PLAN YEAR TYPICALLY STARTS JAN 1 OR GROUP ANNIVERSARY
           MOVE HV-PB-PLAN-YEAR-START TO WS-DATE-WORK-MM
           MOVE '01' TO WS-DATE-WORK-DD
           MOVE WS-CURR-YYYY TO WS-DATE-WORK-YYYY

           STRING WS-DATE-WORK-YYYY WS-DATE-WORK-MM
                  WS-DATE-WORK-DD
                  DELIMITED BY SIZE INTO WS-PLAN-YEAR-START

      *    CHECK IF CURRENT ACCUM YEAR MATCHES CURRENT PLAN YEAR
           MOVE E834-MEMBER-ID TO HV-MA-MEMBER-ID
           MOVE WS-CURR-YYYY   TO HV-MA-PLAN-YEAR

           EXEC SQL
               SELECT plan_year
               INTO   :HV-MA-PLAN-YEAR
               FROM   T_MEMBER_ACCUM
               WHERE  member_id = :HV-MA-MEMBER-ID
               AND    plan_year = :HV-MA-PLAN-YEAR
           END-EXEC

           IF SQLCODE NOT = 0
      *        NO RECORD FOR CURRENT YEAR - RESET ACCUMULATORS
               SET WS-PLAN-YEAR-RESET TO TRUE
               PERFORM 4200-INITIALIZE-ACCUMULATORS
           END-IF
           .

      *----------------------------------------------------------------*
       5000-ACA-EXCHANGE-PROCESSING.
      *----------------------------------------------------------------*
           SET WS-ACA-VALID TO TRUE

      *    VALIDATE METAL TIER
           PERFORM 5100-VALIDATE-METAL-TIER

      *    VERIFY ESSENTIAL HEALTH BENEFITS COMPLIANCE
           PERFORM 5200-VERIFY-EHB

      *    PROCESS APTC IF APPLICABLE
           IF E834-APTC-AMOUNT > ZEROES
               PERFORM 5300-PROCESS-APTC
           END-IF

           IF WS-ACA-VALID
               ADD 1 TO WS-ACA-EXCHANGE-CNT
           END-IF
           .

      *----------------------------------------------------------------*
       5100-VALIDATE-METAL-TIER.
      *----------------------------------------------------------------*
      *    VALIDATE METAL TIER MATCHES PLAN ACTUARIAL VALUE
           EVALUATE TRUE
               WHEN E834-TIER-BRONZE
                   IF HV-PB-AV-PERCENT < 56 OR
                      HV-PB-AV-PERCENT > 64
                       SET WS-PLAN-INVALID TO TRUE
                       MOVE 'BRONZE AV MISMATCH ' TO WS-RPT-MSG
                       ADD 1 TO WS-834-ERROR-CNT
                       SET WS-ACA-VALID TO TRUE
                       MOVE 'N' TO WS-ACA-VALID-FLAG
                   END-IF
               WHEN E834-TIER-SILVER
                   IF HV-PB-AV-PERCENT < 66 OR
                      HV-PB-AV-PERCENT > 74
                       SET WS-PLAN-INVALID TO TRUE
                       MOVE 'SILVER AV MISMATCH ' TO WS-RPT-MSG
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'N' TO WS-ACA-VALID-FLAG
                   END-IF
               WHEN E834-TIER-GOLD
                   IF HV-PB-AV-PERCENT < 76 OR
                      HV-PB-AV-PERCENT > 84
                       MOVE 'GOLD AV MISMATCH   ' TO WS-RPT-MSG
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'N' TO WS-ACA-VALID-FLAG
                   END-IF
               WHEN E834-TIER-PLATINUM
                   IF HV-PB-AV-PERCENT < 86 OR
                      HV-PB-AV-PERCENT > 94
                       MOVE 'PLAT AV MISMATCH   ' TO WS-RPT-MSG
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'N' TO WS-ACA-VALID-FLAG
                   END-IF
               WHEN E834-TIER-CATASTROPHIC
                   PERFORM 4100-CALCULATE-AGE
                   IF WS-AGE-YEARS >= 30
                       MOVE 'CAT AGE INELIGIBLE ' TO WS-RPT-MSG
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'N' TO WS-ACA-VALID-FLAG
                   END-IF
               WHEN OTHER
                   MOVE 'INVALID METAL TIER ' TO WS-RPT-MSG
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'N' TO WS-ACA-VALID-FLAG
           END-EVALUATE
           .

      *----------------------------------------------------------------*
       5200-VERIFY-EHB.
      *----------------------------------------------------------------*
      *    VERIFY PLAN COVERS ALL 10 ESSENTIAL HEALTH BENEFITS
           IF HV-PB-EHB-COMPLIANT NOT = 'Y'
               MOVE 'PLAN NOT EHB COMPLNT' TO WS-RPT-MSG
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'N' TO WS-ACA-VALID-FLAG
           ELSE
               MOVE 'Y' TO WS-EHB-COMPLIANT-FL
           END-IF
           .

      *----------------------------------------------------------------*
       5300-PROCESS-APTC.
      *----------------------------------------------------------------*
      *    ADVANCED PREMIUM TAX CREDIT TRACKING
           MOVE E834-MEMBER-ID     TO HV-AX-MEMBER-ID
           MOVE E834-APTC-AMOUNT   TO HV-AX-APTC-AMOUNT
           MOVE ZEROES             TO HV-AX-APTC-APPLIED
           MOVE E834-CSR-LEVEL     TO HV-AX-CSR-LEVEL
           MOVE E834-METAL-TIER    TO HV-AX-METAL-TIER
           MOVE 'Y'                TO HV-AX-EHB-IND
           MOVE E834-EFF-DATE      TO HV-AX-EFF-DATE
           MOVE '99991231'         TO HV-AX-TERM-DATE

      *    CHECK FOR ENHANCED APTC (AMERICAN RESCUE PLAN)
           IF WS-CURR-YYYY >= 2021
               MOVE 'Y' TO HV-AX-ENHANCED-APTC
               SET WS-ENHANCED-APTC TO TRUE
           ELSE
               MOVE 'N' TO HV-AX-ENHANCED-APTC
           END-IF

      *    APTC CANNOT EXCEED PLAN PREMIUM (NOT TRACKED HERE)
           EXEC SQL
               INSERT INTO T_APTC_DETAIL
                   (member_id, aptc_amount, aptc_applied,
                    csr_level, metal_tier, ehb_ind,
                    enhanced_aptc, eff_date, term_date)
               VALUES
                   (:HV-AX-MEMBER-ID,
                    :HV-AX-APTC-AMOUNT,
                    :HV-AX-APTC-APPLIED,
                    :HV-AX-CSR-LEVEL,
                    :HV-AX-METAL-TIER,
                    :HV-AX-EHB-IND,
                    :HV-AX-ENHANCED-APTC,
                    :HV-AX-EFF-DATE,
                    :HV-AX-TERM-DATE)
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCELIGVR: APTC INSERT FAILED FOR '
                   E834-MEMBER-ID ' SQLCODE=' SQLCODE
           END-IF
           .

      *----------------------------------------------------------------*
       6000-RETRO-ACCUM-RECALC.
      *----------------------------------------------------------------*
      *    RETROACTIVE ACCUMULATOR RECALCULATION
      *    MUST REPROCESS ALL CLAIMS FROM RETRO DATE FORWARD
           MOVE E834-MEMBER-ID TO HV-MA-MEMBER-ID
           MOVE WS-CURR-YYYY   TO HV-MA-PLAN-YEAR

      *    ZERO OUT CURRENT ACCUMULATORS
           EXEC SQL
               UPDATE T_MEMBER_ACCUM
               SET    deductible_used = 0,
                      oop_used        = 0,
                      last_updated    = :WS-CURRENT-DATE-TIME
               WHERE  member_id       = :HV-MA-MEMBER-ID
               AND    plan_year       = :HV-MA-PLAN-YEAR
           END-EXEC

      *    NOTE: ACTUAL CLAIM REPROCESSING IS HANDLED BY HCCLMADJ
      *    THIS MODULE ONLY RESETS THE ACCUMULATORS AND FLAGS
      *    THE CLAIMS FOR REPROCESSING VIA THE RETRO QUEUE TABLE

           EXEC SQL
               INSERT INTO T_RETRO_QUEUE
                   (member_id, retro_eff_date, plan_code,
                    queue_status, created_date, created_pgm)
               VALUES
                   (:E834-MEMBER-ID, :E834-EFF-DATE,
                    :E834-PLAN-CODE, 'P',
                    :WS-CURRENT-DATE-TIME, 'HCELIGVR')
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCELIGVR: RETRO QUEUE INSERT FAILED '
                   'SQLCODE=' SQLCODE
           END-IF

      *    RECALCULATE FAMILY ACCUMULATORS TOO
           PERFORM 4300-FAMILY-ACCUM-ROLLUP
           .

      *----------------------------------------------------------------*
       7000-WRITE-AUDIT-TRAIL.
      *----------------------------------------------------------------*
           MOVE WS-CURRENT-DATE-TIME TO WS-AUD-TIMESTAMP
           MOVE WS-PROGRAM-ID        TO WS-AUD-PROGRAM
           MOVE 'BATCH'              TO WS-AUD-USER-ID
           MOVE E834-MEMBER-ID       TO WS-AUD-MEMBER-ID
           MOVE 'T_MEMBER_ELIG'      TO WS-AUD-TABLE-NAME

      *    WRITE TO FLAT FILE
           MOVE WS-AUDIT-WORK TO AUDIT-TRAIL-RECORD
           WRITE AUDIT-TRAIL-RECORD
           ADD 1 TO WS-AUDIT-WRITE-CNT

      *    ALSO INSERT INTO DATABASE AUDIT TABLE
           MOVE WS-AUD-TIMESTAMP  TO HV-AT-TIMESTAMP
           MOVE WS-AUD-PROGRAM    TO HV-AT-PROGRAM-ID
           MOVE WS-AUD-USER-ID    TO HV-AT-USER-ID
           MOVE WS-AUD-ACTION     TO HV-AT-ACTION-CODE
           MOVE WS-AUD-MEMBER-ID  TO HV-AT-MEMBER-ID
           MOVE WS-AUD-TABLE-NAME TO HV-AT-TABLE-NAME
           MOVE WS-AUD-FIELD-NAME TO HV-AT-FIELD-NAME
           MOVE WS-AUD-OLD-VALUE  TO HV-AT-OLD-VALUE
           MOVE WS-AUD-NEW-VALUE  TO HV-AT-NEW-VALUE
           MOVE WS-AUD-REASON-CODE TO HV-AT-REASON-CODE

           EXEC SQL
               INSERT INTO T_AUDIT_TRAIL
                   (audit_timestamp, program_id, user_id,
                    action_code, member_id, table_name,
                    field_name, old_value, new_value,
                    reason_code)
               VALUES
                   (:HV-AT-TIMESTAMP, :HV-AT-PROGRAM-ID,
                    :HV-AT-USER-ID, :HV-AT-ACTION-CODE,
                    :HV-AT-MEMBER-ID, :HV-AT-TABLE-NAME,
                    :HV-AT-FIELD-NAME, :HV-AT-OLD-VALUE,
                    :HV-AT-NEW-VALUE, :HV-AT-REASON-CODE)
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCELIGVR: AUDIT INSERT WARNING SQLCODE='
                   SQLCODE
           END-IF

           INITIALIZE WS-AUDIT-WORK
           .

      *----------------------------------------------------------------*
       8000-WRITE-SUMMARY-REPORT.
      *----------------------------------------------------------------*
           PERFORM 8050-WRITE-REPORT-HEADER

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  PROCESSING SUMMARY' DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  ==================' DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  834 RECORDS READ:        '
               WS-834-READ-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  834 RECORDS PROCESSED:   '
               WS-834-PROCESS-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  NEW ENROLLMENTS:         '
               WS-NEW-ENROLL-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  TERMINATIONS:            '
               WS-TERM-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  DEPENDENT ADDS:          '
               WS-DEPEND-ADD-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  DEPENDENT REMOVES:       '
               WS-DEPEND-REM-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  PLAN CHANGES:            '
               WS-PLAN-CHNG-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  COBRA ELECTIONS:         '
               WS-COBRA-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  OPEN ENROLLMENTS:        '
               WS-OPEN-ENRL-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  RETRO CHANGES:           '
               WS-RETRO-CHG-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  ACA EXCHANGE ENRL:       '
               WS-ACA-EXCHANGE-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  270 INQUIRIES:           '
               WS-270-READ-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  271 RESPONSES:           '
               WS-271-WRITE-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  ERRORS:                  '
               WS-834-ERROR-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  AUDIT RECORDS:           '
               WS-AUDIT-WRITE-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD
           .

      *----------------------------------------------------------------*
       8050-WRITE-REPORT-HEADER.
      *----------------------------------------------------------------*
           ADD 1 TO WS-RPT-PAGE-NUM
           MOVE WS-RPT-PAGE-NUM TO WS-RPT-PAGE-NO
           STRING WS-CURR-YYYY '-' WS-CURR-MM '-' WS-CURR-DD
               DELIMITED BY SIZE INTO WS-RPT-DATE

           WRITE ENRL-RPT-RECORD FROM WS-RPT-HEADER-1
               AFTER ADVANCING PAGE
           MOVE SPACES TO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD
           MOVE 3 TO WS-RPT-LINE-CNT
           .

      *----------------------------------------------------------------*
       8100-WRITE-DETAIL-LINE.
      *----------------------------------------------------------------*
           IF WS-RPT-LINE-CNT >= WS-RPT-LINES-PER-PAGE
               PERFORM 8050-WRITE-REPORT-HEADER
           END-IF

           MOVE E834-TRANS-TYPE  TO WS-RPT-TRANS
           MOVE E834-MEMBER-ID   TO WS-RPT-MBR-ID
           STRING E834-LAST-NAME ', ' E834-FIRST-NAME
               DELIMITED BY SIZE INTO WS-RPT-NAME
           MOVE E834-PLAN-CODE   TO WS-RPT-PLAN
           STRING E834-EFF-DATE(1:4) '-' E834-EFF-DATE(5:2)
                  '-' E834-EFF-DATE(7:2)
               DELIMITED BY SIZE INTO WS-RPT-EFF

           WRITE ENRL-RPT-RECORD FROM WS-RPT-DETAIL
           ADD 1 TO WS-RPT-LINE-CNT
           INITIALIZE WS-RPT-DETAIL
           .

      *----------------------------------------------------------------*
       9000-TERMINATION.
      *----------------------------------------------------------------*
           EXEC SQL
               COMMIT WORK
           END-EXEC

           EXEC SQL
               DISCONNECT
           END-EXEC

           CLOSE ENRL-834-FILE
           CLOSE ELIG-270-FILE
           CLOSE ELIG-271-FILE
           CLOSE ENRL-RPT-FILE
           CLOSE AUDIT-TRAIL-FILE

           DISPLAY 'HCELIGVR: PROCESSING COMPLETE'
           DISPLAY 'HCELIGVR: 834 RECORDS READ:      '
               WS-834-READ-CNT
           DISPLAY 'HCELIGVR: 834 RECORDS PROCESSED:  '
               WS-834-PROCESS-CNT
           DISPLAY 'HCELIGVR: ERRORS:                 '
               WS-834-ERROR-CNT
           DISPLAY 'HCELIGVR: 270 INQUIRIES:          '
               WS-270-READ-CNT
           DISPLAY 'HCELIGVR: AUDIT RECORDS WRITTEN:  '
               WS-AUDIT-WRITE-CNT

           MOVE ZEROES TO RETURN-CODE
           .

      *----------------------------------------------------------------*
       9500-ABEND-ROUTINE.
      *----------------------------------------------------------------*
           DISPLAY 'HCELIGVR: ABEND INITIATED, CODE=' ERR-ABEND-CODE

           EXEC SQL
               ROLLBACK WORK
           END-EXEC

           EXEC SQL
               DISCONNECT
           END-EXEC

           MOVE ERR-ABEND-CODE TO RETURN-CODE

           STOP RUN
           .
